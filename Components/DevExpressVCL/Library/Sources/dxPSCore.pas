{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressPrinting System                                   }
{                                                                    }
{           Copyright (C) 1998-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSPRINTING SYSTEM AND            }
{   ALL ACCOMPANYING VCL CONTROLS AS PART OF AN                      }
{   EXECUTABLE PROGRAM ONLY.                                         }
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

unit dxPSCore;

interface

{$I cxVer.inc}

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, StdCtrls, ExtCtrls, Generics.Collections,
  ActiveX, Menus, Forms, ImgList, Contnrs, IniFiles,
  dxMessages, dxCore, cxClasses, cxControls, cxGraphics, cxButtons, cxGeometry, dxPrinting, dxGDIPlusClasses,
  dxBase, dxPSGlbl, dxPSSngltn, dxPSEngn, dxPSESys, dxPSForm, dxBkgnd, dxPrnPg,
  dxPgsDlg, dxPrnDlg, dxWrap, cxDrawTextUtils, dxPSFillPatterns,
  dxPSReportRenderCanvas, dxPSPDFExportCore;

type
  TdxPreviewEnableOption = (peoCanChangeMargins, peoHelp,
    peoPageBackground, peoPageSetup, peoPreferences, peoPrint, peoReportDesign, peoPrintDialog);
  TdxPreviewEnableOptions = set of TdxPreviewEnableOption;
  TdxPreviewVisibleOption = (pvoHelp, pvoPageBackground, pvoPageSetup, pvoPreferences,
    pvoPrint, pvoReportDesign, pvoPrintStyles, pvoReportFileOperations, pvoPageMargins, pvoPrintDialog);
  TdxPreviewVisibleOptions = set of TdxPreviewVisibleOption;
  TdxPSPreviewState = (prsNone, prsEditHeaders, prsEditFooters);
  TdxPSThumbnailsSize = (tsSmall, tsLarge);

const
  dxDefaultPreviewEnableOptions  = [peoCanChangeMargins, peoPageBackground,
    peoPageSetup, peoPreferences, peoPrint, peoReportDesign, peoPrintDialog];
  dxDefaultPreviewVisibleOptions = [pvoPageBackground, pvoPageSetup, pvoPreferences, pvoPrint, pvoPrintDialog,
    pvoReportDesign, pvoPrintStyles, pvoReportFileOperations, pvoPageMargins];

type
  EdxPrintEngine = class(EdxException);
  EdxReportLink = class(EdxPrintEngine);
  EdxPSExplorer = class(EdxPrintEngine);
  EdxComponentPrinter = class(EdxPrintEngine);
  TdxPSReportRendererClass = class of TdxPSReportRenderer;

  { EdxInvalidStorageVersion }

  EdxInvalidStorageVersion = class(EdxComponentPrinter)
  strict private
    FVersion: UINT;
  public
    constructor Create(AVersion: UINT; ADummy: Boolean = False{#AI: workaround for CBuilder});
    //
    property Version: UINT read FVersion write FVersion;
  end;

  TCustomdxPSExplorerContextCommand = class;
  TdxPSCompositionReportRenderRowInfo = class;

  IdxPSExplorerContextCommandBuilder = interface
  ['{EE36E842-FD6A-4A89-A343-A32828AEFE3D}']
    procedure AddExplorerContextCommand(ACommand: TCustomdxPSExplorerContextCommand);
    procedure UpdateExplorerContextCommands;
  end;

  IdxPSExplorerContextCommands = interface
  ['{DC3A582D-7E33-410F-A235-680A846824D3}']
    procedure BuildCommandSet(ABuilder: IdxPSExplorerContextCommandBuilder);
  end;

  IdxPSExplorerContextCommands2 = interface
  ['{E4D68CF3-AD07-4220-B892-92C8F9B3F966}']
    procedure FinalizeCommand(ACommand: TCustomdxPSExplorerContextCommand);
    procedure InitializeCommand(ACommand: TCustomdxPSExplorerContextCommand);
  end;

  IdxReportLinkController = interface
  ['{120F53E4-1B09-46EF-B42D-04AB8BBCC374}']
    function GetControlSiteBounds(AControl: TControl): TRect;
  end;

  TdxReportTitle = class;
  TdxPSReportDocument = class;
  TdxPSReportRenderer = class;
  TdxReportLinkClass = class of TBasedxReportLink;
  TBasedxReportLink = class;
  TdxCompositionReportLink = class;
  TCustomdxComponentPrinter = class;
  TCustomdxPSExplorerTreeContainer = class;
  TAbstractdxReportLinkDesigner = class;
  TAbstractdxPreviewWindowDesigner = class;
  TAbstractdxReportLinkDesignWindow = class;
  TdxReportLinkDesignWindowClass = class of TAbstractdxReportLinkDesignWindow;
  TdxPSCustomPreviewWindow = class;
  TdxBasePreviewOptions = class;
  TdxPreviewOptions = class;
  TdxReportNote = class;

  TdxReportVisualItemClass = class of TdxReportVisualItem;
  TdxReportVisualItem = class;
  TdxReportCellDataClass = class of TAbstractdxReportCellData;
  TAbstractdxReportCellData = class;
  TdxReportCellClass = class of TdxReportCell;
  TdxReportCell = class;
  TdxReportCellsClass = class of TdxReportCells;
  TdxReportCells = class;

  TdxPSCellBorderClass = class of TdxPSCustomCellBorder;
  TdxPSCustomCellBorder = class;
  TdxPSCellBorderPainterClass = class of TdxPSCellBorderPainter;
  TdxPSCellBorderPainter = class;

  TdxPSReportGroupLookAndFeelClass = class of TdxPSReportGroupLookAndFeel;
  TdxPSReportGroupLookAndFeel = class;
  TdxPSReportGroupLookAndFeelPainterClass = class of TdxPSReportGroupLookAndFeelPainter;
  TdxPSReportGroupLookAndFeelPainter = class;

  TdxPSPDFReportExportOptions = class;
  TdxPSPrintPageRangeInfo = class;

  { TdxPSDataReader }

  TdxPSDataReaderClass = class of TdxPSDataReader;

  TdxPSDataReader = class(TReader)
  strict private
    FPSVersion: TdxPSVersion;
    FStorageVersion: Integer;
    FUnitsPerInch: Integer;
  protected
    class function SupportsStorageVersion(AVersion: Integer): Boolean; virtual;
  public
    class procedure Register; virtual;
    class procedure Unregister; virtual;

    function ReadCellBorderClass: TdxPSCellBorderClass;
    function ReadClass: TClass;
    function ReadFillPatternClass: TdxPSFillPatternClass;
    function ReadFont(AFont: TFont): TFont;
    function ReadGraphicClass: TGraphicClass;
    function ReadLinkClass: TdxReportLinkClass;
    function ReadLookAndFeelClass: TdxPSReportGroupLookAndFeelClass;
    function ReadPoint: TPoint;
    function ReadPSVersion: TdxPSVersion;
    function ReadRect: TRect;
    procedure ReadImage(AImage: TGraphic);
    procedure ReadImageList(AnImageList: TCustomImageList);
    procedure SkipBytes(Count: Int64);
    //
    property StorageVersion: Integer read FStorageVersion write FStorageVersion;
    property PSVersion: TdxPSVersion read FPSVersion;
    property UnitsPerInch: Integer read FUnitsPerInch write FUnitsPerInch;
  end;

  { TdxPSDataWriter }

  TdxPSDataWriterClass = class of TdxPSDataWriter;

  TdxPSDataWriter = class(TWriter)
  protected
    FPSVersion: TdxPSVersion;
    FStorageVersion: Integer;

    class function SupportsStorageVersion(AVersion: Integer): Boolean; virtual;
  public
    class procedure Register; virtual;
    class procedure Unregister; virtual;

    procedure WriteClassName(AClass: TClass); overload;
    procedure WriteClassName(AnObject: TObject); overload;
    procedure WriteFont(AFont: TFont);
    procedure WriteImage(AnImage: TGraphic);
    procedure WriteImageList(AnImageList: TCustomImageList);
    procedure WritePoint(const Pt: TPoint);
    procedure WritePSVersion(const AVersion: TdxPSVersion);
    procedure WriteRect(const R: TRect);
    //
    property StorageVersion: Integer read FStorageVersion;
    property PSVersion: TdxPSVersion read FPSVersion;
  end;

  { Report Renderers }

  TdxWindowScalePair = record
    Numerator: Integer;
    Denominator: Integer;
  end;

  TdxContinuedIndexPair = class
  public
    StartIndex: Integer;
    EndIndex: Integer;
  end;

  { TdxPageOverlayIndexes }

  TdxPageOverlayIndexes = class(TList)
  private
    function GetItem(Index: Integer): Integer;
    procedure SetItem(Index, Value: Integer);
  public
    function Add(AValue: Integer): Integer;
    property Items[Index: Integer]: Integer read GetItem write SetItem; default;
  end;

  TdxPSReportRenderInfo = class;
  TdxPSPageRenderInfoClass = class of TdxPSPageRenderInfo;

  { TdxPSPageRenderInfo }

  TdxPSPageRenderInfo = class
  strict private
    FIndexPairs: TList;
    FIsCompositionPagePart: Boolean;
    FIsEmptyPage: Boolean;
    FIsEmptyPageCalculated: Boolean;
    FOverlays: TList;
    FPageIndex: Integer;
    FPrinterPage: TdxPrinterPage;
    FRenderInfo: TdxPSReportRenderInfo;

    function GetColIndex: Integer;
    function GetFootnotesBounds: TRect;
    function GetFootnotesHeight: Integer;
    function GetIndexPair(Index: Integer): TdxContinuedIndexPair;
    function GetIndexPairCount: Integer;
    function GetIsBottomPage: Boolean;
    function GetIsEmptyPage: Boolean;
    function GetIsTopPage: Boolean;
    function GetOverlay(Index: Integer): TdxPageOverlayIndexes;
    function GetOverlayCount: Integer;
    function GetReportCells: TdxReportCells;
    function GetReportLink: TBasedxReportLink;
    function GetRowIndex: Integer;
    function GetTitleBounds: TRect;
    function GetTitleHeight: Integer;
    procedure SetIndexPair(Index: Integer; Value: TdxContinuedIndexPair);

    procedure FreeAndNilIndexPairs;
    procedure FreeAndNilOverlays;
  protected
    PageOffset: TPoint;

    function AreRectsIntersected(const R1, R2: TRect): Boolean;
    function CalculateIndexPairCount: Integer; virtual;
    function CalculateIsEmptyPage: Boolean; virtual;
    function CreatePrinterPage(ARenderInfo: TdxPSReportRenderInfo): TdxPrinterPage; virtual;
    procedure CalculateBounds; virtual;
    procedure CalculateHeadersBounds; virtual;
    procedure CalculateIndexPairs; virtual;
    procedure CalculateOffsets; virtual;
    procedure CalculateOverlayIndexes;
    procedure CalculatePageHeaderAndFooterBounds;
    function GetPageSize: TPoint; virtual;
    function GetPaintSize: TPoint; virtual;

    property IsEmptyPageCalculated: Boolean read FIsEmptyPageCalculated write FIsEmptyPageCalculated;
  public
    ContentBounds: TRect;
    DataOffset: TPoint;
    DetailBounds: TRect;
    FooterBounds: TRect;
    FootnotesOffset: TPoint;
    HeaderBounds: TRect;
    HeaderCornerBounds: TRect;
    PageFooterBounds: TRect;
    PageHeaderBounds: TRect;
    RowHeaderBounds: TRect;
    TitleOffset: TPoint;

    constructor Create(ARenderInfo: TdxPSReportRenderInfo; APageIndex: Integer); virtual;
    destructor Destroy; override;
    procedure Calculate; virtual;
    procedure CalculateCompositionPartInfo; virtual;
    procedure CalculateFootnotesOffset; virtual;
    procedure CalculateTitleOffset; virtual;

    function HasDetails: Boolean; virtual;
    function HasFooter: Boolean; virtual;
    function HasFootnotes: Boolean; virtual;
    function HasHeader: Boolean; virtual;
    function HasHeaderCorner: Boolean; virtual;
    function HasRowHeader: Boolean; virtual;
    function HasTitle: Boolean; virtual;

    property ColIndex: Integer read GetColIndex;
    property FootnotesBounds: TRect read GetFootnotesBounds;
    property FootnotesHeight: Integer read GetFootnotesHeight;
    property IndexPairCount: Integer read GetIndexPairCount;
    property IndexPairs[Index: Integer]: TdxContinuedIndexPair read GetIndexPair write SetIndexPair;
    property IsBottomPage: Boolean read GetIsBottomPage;
    property IsCompositionPagePart: Boolean read FIsCompositionPagePart;
    property IsEmptyPage: Boolean read GetIsEmptyPage;
    property IsTopPage: Boolean read GetIsTopPage;
    property OverlayCount: Integer read GetOverlayCount;
    property Overlays[Index: Integer]: TdxPageOverlayIndexes read GetOverlay;
    property PageIndex: Integer read FPageIndex;
    property PageSize: TPoint read GetPageSize;
    property PaintSize: TPoint read GetPaintSize;
    property PrinterPage: TdxPrinterPage read FPrinterPage;
    property RenderInfo: TdxPSReportRenderInfo read FRenderInfo;
    property ReportCells: TdxReportCells read GetReportCells;
    property ReportLink: TBasedxReportLink read GetReportLink;
    property RowIndex: Integer read GetRowIndex;
    property TitleBounds: TRect read GetTitleBounds;
    property TitleHeight: Integer read GetTitleHeight;
  end;

  TdxPSReportRenderInfoClass = class of TdxPSReportRenderInfo;
  TdxPSCalculatePagesStage = (cpsFirstPass, cpsSecondPass);

  { TdxPSReportRenderInfo }

  TdxPSReportRenderInfo = class
  private
    FActualScaleFactor: Integer;
    FBaseContentFont: TFont;
    FCompositionPagePartCount: Integer;
    FDelimitersX: TList;
    FDelimitersY: TList;
    FGridLinesColor: TColor;
    FHardDelimitersX: TList;
    FHardDelimitersY: TList;
    FLockCounter: Integer;
    FPageDelimitersX: TList;
    FPageDelimitersY: TList;
    FPageRenderInfos: TList;
    FReportLink: TBasedxReportLink;

    function GetBreakPagesByHardDelimiters: Boolean;
    function GetDelimiterX(Index: Integer): Integer;
    function GetDelimiterXCount: Integer;
    function GetDelimiterY(Index: Integer): Integer;
    function GetDelimiterYCount: Integer;
    function GetFooterHeight: Integer;
    function GetFootnotesAdjustOnReportScale: Boolean;
    function GetFootnotesFont: TFont;
    function GetFootnotesHeight: Integer;
    function GetFootnotesText: string;
    function GetHeaderHeight: Integer;
    function GetLocked: Boolean;
    function GetPageDelimiterX(Index: Integer): Integer;
    function GetPageDelimiterXCount: Integer;
    function GetPageDelimiterY(Index: Integer): Integer;
    function GetPageDelimiterYCount: Integer;
    function GetPageRenderInfo(Index: Integer): TdxPSPageRenderInfo;
    function GetPageRenderInfoCount: Integer;
    function GetPrinterPage: TdxPrinterPage;
    function GetReportCells: TdxReportCells;
    function GetReportHeight: Integer;
    function GetReportWidth: Integer;
    function GetRowHeaderWidth: Integer;
    function GetScaleFactor: Integer;
    function GetTitleAdjustOnReportScale: Boolean;
    function GetTitleFont: TFont;
    function GetTitleText: string;
    function GetUseHardHorzDelimiters: Boolean;
    function GetUseHardVertDelimiters: Boolean;
    function GetUseHorzDelimiters: Boolean;
    function GetUseVertDelimiters: Boolean;
    function GetTitleHeight: Integer;
    procedure SetBaseContentFont(Value: TFont);

    function IsNonEmptyPage(const ABounds: TRect): Boolean;
  protected
    function CalculateCompositionPagePartCount: Integer; virtual;
    function CalculatePageContentHeight(APageIndex: Integer): Integer; virtual;
    function CalculatePageContentWidth(APageIndex: Integer): Integer; virtual;
    function CalculatePageDetailBounds(APageCol, APageRow: Integer): TRect; virtual;
    procedure CalculateFootnotesBounds; virtual;
    procedure CalculateHeaderAndFooterBounds; virtual;
    procedure CalculatePages(AStage: TdxPSCalculatePagesStage); virtual;
    procedure CalculatePageRenderInfos; virtual;
    procedure CalculateTitleBounds; virtual;
    procedure DoCalculate; virtual;

    function GetCompositionPagePartCount: Integer; virtual;
    function GetNonEmptyPageCount: Integer; virtual;
    function GetPageColCount: Integer; virtual;
    function GetPageRowCount: Integer; virtual;
    function GetPageSize: TPoint; virtual;
    function GetPaintSize: TPoint; virtual;
    function GetPointsPerInch: Integer; virtual;
    function GetUnitsPerInch: Integer; virtual;
    function GetWindowScalePair: TdxWindowScalePair; virtual;
    procedure SetUnitsPerInch(Value: Integer); virtual;

    procedure ClearPageRenderInfos;
    function CreatePageRenderInfo(APageIndex: Integer): TdxPSPageRenderInfo; virtual;
    procedure FreeAndNilPageRenderInfos;
    function GetPageRenderInfoClass: TdxPSPageRenderInfoClass; virtual;
    procedure PreparePixelsNumeratorAndDenominator;
    procedure Refresh; virtual;

    function HasPageFootnotes(APageIndex: Integer): Boolean;
    function HasPageTitle(APageIndex: Integer): Boolean;
    function IsHardHorizontalDelimiter(AValue: Integer): Boolean;
    function IsHardVerticalDelimiter(AValue: Integer): Boolean;
    function IsReportFitsToPages(APrinterPage: TdxPrinterPage): Boolean; virtual;

    procedure CheckStorageCompatibility(AReader: TdxPSDataReader); virtual;
    procedure ReadData(AReader: TdxPSDataReader); virtual;
    procedure ReadDelimiters(AReader: TdxPSDataReader);
    procedure WriteData(AWriter: TdxPSDataWriter); virtual;
    procedure WriteDelimiters(AWriter: TdxPSDataWriter);

    procedure AddPageDelimiterX(AValue: Integer);
    procedure AddPageDelimiterY(AValue: Integer);
    procedure PageDelimiterXClear;
    procedure PageDelimiterYClear;

    procedure AddStandardDelimiters;
    procedure EliminateDuplicatesAndSortDelimiters(AList: TList);
    procedure GetDelimiters;
    procedure MakeDelimiters;
    procedure MakeHardDelimiters;
    procedure TruncateDelimiters(AList: TList; AValue: Integer);

    function LoMetricRectToInternalUnits(const R: TRect): TRect;
    function LoMetricValueToInternalUnits(Value: Integer): Integer; virtual;

    property DelimiterXList: TList read FDelimitersX;
    property DelimiterYList: TList read FDelimitersY;
    property HardDelimiterXList: TList read FHardDelimitersX;
    property HardDelimiterYList: TList read FHardDelimitersY;
  public
    CanUseHFOnEveryPageMode: Boolean;
    FooterBounds: TRect;
    FootnotesBounds: TRect;
    HeaderBounds: TRect;
    HeaderCornerBounds: TRect;
    RowHeaderBounds: TRect;
    TitleBounds: TRect;
    VirtualPageCount: Integer;

    constructor Create(AReportLink: TBasedxReportLink); virtual;
    destructor Destroy; override;

    procedure Calculate;
    function CalculateActualScaleFactor: Integer; overload; virtual;
    function CalculateActualScaleFactor(AFitToPagesHorizontally, AFitToPagesVertically, AReportDPI: Integer): Integer; overload; virtual;
    function CalculateFootnotesHeight(AAdjustOnReportScale: Boolean): Integer; overload; virtual;
    function CalculateFootnotesHeight: Integer; overload;
    function CalculateTitleHeight(AAdjustOnReportScale: Boolean): Integer; overload; virtual;
    function CalculateTitleHeight: Integer; overload;
    function CalculateReportNotesTextHeight(const AText: string;
      AFont: TFont; AAdjustOnReportScale: Boolean): Integer; virtual;
    function CanRenderPage(APageIndex: Integer): Boolean; virtual;
    function GetActualPageRenderInfo(APageIndex: Integer): TdxPSPageRenderInfo;
    function GetCompositionInfo(APageIndex: Integer; out ARowInfo: TdxPSCompositionReportRenderRowInfo): Boolean; virtual;
    function IsDrawPageFootnotesOnPage(APageIndex: Integer): Boolean; virtual;
    function IsDrawPageTitleOnPage(APageIndex: Integer): Boolean; virtual;
    function IsFooterOnPage(APageIndex: Integer): Boolean; virtual;
    function IsHeaderOnPage(APageIndex: Integer): Boolean; virtual;
    function IsRowHeaderOnPage(APageIndex: Integer): Boolean; virtual;

    procedure Lock;
    procedure Unlock;

    property BaseContentFont: TFont read FBaseContentFont write SetBaseContentFont;
    property BreakPagesByHardDelimiters: Boolean read GetBreakPagesByHardDelimiters;
    property CompositionPagePartCount: Integer read GetCompositionPagePartCount;
    property DelimitersX[Index: Integer]: Integer read GetDelimiterX;
    property DelimitersY[Index: Integer]: Integer read GetDelimiterY;
    property DelimiterXCount: Integer read GetDelimiterXCount;
    property DelimiterYCount: Integer read GetDelimiterYCount;
    property FooterHeight: Integer read GetFooterHeight;
    property FootnotesAdjustOnReportScale: Boolean read GetFootnotesAdjustOnReportScale;
    property FootnotesFont: TFont read GetFootnotesFont;
    property FootnotesHeight: Integer read GetFootnotesHeight;
    property FootnotesText: string read GetFootnotesText;
    property GridLinesColor: TColor read FGridLinesColor write FGridLinesColor;
    property HeaderHeight: Integer read GetHeaderHeight;
    property Locked: Boolean read GetLocked;
    property NonEmptyPageCount: Integer read GetNonEmptyPageCount;
    property PageColCount: Integer read GetPageColCount;
    property PageDelimitersX[Index: Integer]: Integer read GetPageDelimiterX;
    property PageDelimiterXCount: Integer read GetPageDelimiterXCount;
    property PageDelimitersY[Index: Integer]: Integer read GetPageDelimiterY;
    property PageDelimiterYCount: Integer read GetPageDelimiterYCount;
    property PageRenderInfoCount: Integer read GetPageRenderInfoCount;
    property PageRenderInfos[Index: Integer]: TdxPSPageRenderInfo read GetPageRenderInfo;
    property PageRowCount: Integer read GetPageRowCount;
    property PageSize: TPoint read GetPageSize;
    property PaintSize: TPoint read GetPaintSize;
    property PrinterPage: TdxPrinterPage read GetPrinterPage;
    property ReportCells: TdxReportCells read GetReportCells;
    property ReportHeight: Integer read GetReportHeight;
    property ReportLink: TBasedxReportLink read FReportLink;
    property ReportWidth: Integer read GetReportWidth;
    property RowHeaderWidth: Integer read GetRowHeaderWidth;
    property ScaleFactor: Integer read GetScaleFactor;
    property TitleAdjustOnReportScale: Boolean read GetTitleAdjustOnReportScale;
    property TitleFont: TFont read GetTitleFont;
    property TitleHeight: Integer read GetTitleHeight;
    property TitleText: string read GetTitleText;
    property PointsPerInch: Integer read GetPointsPerInch;
    property UnitsPerInch: Integer read GetUnitsPerInch write SetUnitsPerInch;
    property UseHardHorzDelimiters: Boolean read GetUseHardHorzDelimiters;
    property UseHardVertDelimiters: Boolean read GetUseHardVertDelimiters;
    property UseHorzDelimiters: Boolean read GetUseHorzDelimiters;
    property UseVertDelimiters: Boolean read GetUseVertDelimiters;
    property WindowScalePair: TdxWindowScalePair read GetWindowScalePair;
  end;

  TdxCellCheckPos = (ccpLeft, ccpCenter, ccpRight);
  TdxCellEdgeKind = (cekInner, cekOuter);
  TdxCellEdgeMode = (cemPattern, cem3DEffects);
  TdxCellEdgeStyle = (cesRaised, cesSunken);
  TdxCellSide = (csLeft, csTop, csRight, csBottom);
  TdxCellSides = set of TdxCellSide;
  TdxCellSortOrder = (csoNone, csoUp, csoDown);
  TdxCellUpDown = csoUp..csoDown;
  TdxCheckButtonEdgeStyle = dxPSReportRenderCanvas.TdxCheckButtonEdgeStyle;
  TdxGraphicDrawMode = (gdmNone, gdmCenter, gdmStretch, gdmStretchProportional,
    gdmCenterAndStretchProportional, gdmProportional, gdmCenterProportional);

  TdxImageLayout = (ilImageTopLeft, ilImageTopCenter, ilImageTopRight, ilImageCenterLeft,
    ilImageCenterCenter, ilImageCenterRight, ilImageBottomLeft, ilImageBottomCenter, ilImageBottomRight);

  TdxCellImageBuffering = (cibDefault, cibNone, cibAlways);
{$IFDEF BCBCOMPATIBLE}
  TdxCellImageActualBuffering = TdxCellImageBuffering;
{$ELSE}
  TdxCellImageActualBuffering = cibNone..cibAlways;
{$ENDIF}

  TdxPSCellBorderSalientType = (bstOuter, bstInner);

  TdxPSTreeLineMode = (tlmNone, tlmVertical, tlmCross, tlmTopRightCorner, tlmBottomRightCorner);
  TdxPSTreeLinePart = (tlpTop, tlpRight, tlpBottom);
  TdxPSTreeLineParts = set of TdxPSTreeLinePart;
  TdxPSTreeLineStyle = (tlsSolid, tlsDot);

  { TdxPSCellBorderPainter }

  TdxPSCellBorderPainter = class(TObject)
  private
    FItem: TdxReportVisualItem;
    FRenderer: TdxPSReportRenderer;
    function GetLineThickness: Integer;
  protected
    class function GetBorderBounds(const R: TRect; ASide: TdxCellSide; ALineThickness: Integer): TRect; virtual;
    class function GetBottomShadowBounds(const R: TRect; AShadowDepth: Integer): TRect;
    class function GetBottomShadowRestSpaceBounds(const R: TRect; AShadowDepth: Integer): TRect;
    class function GetRightShadowBounds(const R: TRect; AShadowDepth: Integer): TRect;
    class function GetRightShadowRestSpaceBounds(const R: TRect; AShadowDepth: Integer): TRect;
    class procedure InflateRect(var R: TRect; ASides: TdxCellSides; ALineThickness: Integer);
  public
    constructor Create(ARenderer: TdxPSReportRenderer); virtual;

    procedure DrawBorders(ACanvas: TdxPSReportRenderCustomCanvas; const R: TRect); virtual;
    class procedure DrawFrame(ACanvas: TdxPSReportRenderCustomCanvas; R: TRect;
      ASides: TdxCellSides; ATopLeftColor, ARightBottomColor: TColor;
      ALineThickness: Integer; AThickness: Integer = 1);
    class procedure DrawShadow(ACanvas: TdxPSReportRenderCustomCanvas;
      const R: TRect; AShadowDepth: Integer; AShadowColor, ARestSpaceColor: TColor);

    function BorderClass: TdxPSCellBorderClass; overload; virtual;
    function Item: TdxReportVisualItem; overload; virtual;
    function Renderer: TdxPSReportRenderer; overload; virtual;

    property LineThickness: Integer read GetLineThickness;
  end;

  { TdxPSCustomCellBorder }

  TdxPSCustomCellBorder = class(TPersistent)
  protected
    class function Edge3DSoft: Boolean; virtual;
    class function Edge3DStyle: TdxCellEdgeStyle; virtual;
    class function EdgeMode: TdxCellEdgeMode; virtual;

    class function GetBorderEdgeSalient(ASide: TdxCellSide; ASalient: TdxPSCellBorderSalientType): Integer; virtual;
    class function GetPainterClass: TdxPSCellBorderPainterClass; virtual;
  public
    class procedure Register; virtual;
    class procedure Unregister; virtual;

    class function Solid: Boolean; virtual;
    class function Thickness: Integer; virtual;
  end;

  { TdxPSCellNullBorder }

  TdxPSCellNullBorder = class(TdxPSCustomCellBorder)
  public
    class function Thickness: Integer; override;
  end;

  { TdxPSCellFlatBorder }

  TdxPSCellFlatBorder = class(TdxPSCustomCellBorder);

  { TdxPSCellBoldFlatBorder }

  TdxPSCellBoldFlatBorder = class(TdxPSCellFlatBorder)
  public
    class function Thickness: Integer; override;
  end;

  { TdxPSCellUltraFlatBorder }

  TdxPSCellUltraFlatBorder = class(TdxPSCellFlatBorder)
  public
    class function Thickness: Integer; override;
  end;

  TdxPSCell3DBorderClass = class of TdxPSCustomCell3DBorder;

  { TdxPSCell3DBorderPainter }

  TdxPSCell3DBorderPainter = class(TdxPSCellBorderPainter)
  public
    procedure DrawBorders(ACanvas: TdxPSReportRenderCustomCanvas; const R: TRect); override;
    class procedure Draw3DFrame(ACanvas: TdxPSReportRenderCustomCanvas; R: TRect;
      ASides: TdxCellSides; AOuterTLColor, AOuterBRColor: TColor;
      AInnerTLColor, AInnerBRColor: TColor; ALineThickness: Integer); overload; virtual;
    class procedure Draw3DFrame(ACanvas: TdxPSReportRenderCustomCanvas; R: TRect;
      ASides: TdxCellSides; ACellBorders: TdxPSCell3DBorderClass; ALineThickness: Integer); overload; virtual;

    function BorderClass: TdxPSCell3DBorderClass; reintroduce; overload;
  end;

  { TdxPSCustomCell3DBorder }

  TdxPSCustomCell3DBorder = class(TdxPSCustomCellBorder)
  protected
    class function EdgeMode: TdxCellEdgeMode; override;

    class function GetBorderEdgeSalient(ASide: TdxCellSide; ASalient: TdxPSCellBorderSalientType): Integer; override;
    class function GetPainterClass: TdxPSCellBorderPainterClass; override;
  public
    class function Solid: Boolean; override;
    class function Thickness: Integer; override;

    class function BottomRightInnerBrush: HBRUSH; virtual;
    class function BottomRightInnerColor: TColor; virtual;
    class function BottomRightOuterBrush: HBRUSH; virtual;
    class function BottomRightOuterColor: TColor; virtual;
    class function TopLeftInnerBrush: HBRUSH; virtual;
    class function TopLeftInnerColor: TColor; virtual;
    class function TopLeftOuterBrush: HBRUSH; virtual;
    class function TopLeftOuterColor: TColor; virtual;
  end;

  { TdxPSCellRaisedBorder }

  TdxPSCellRaisedBorder = class(TdxPSCustomCell3DBorder)
  protected
    class function Edge3DSoft: Boolean; override;
    class function Edge3DStyle: TdxCellEdgeStyle; override;
  public
    class function BottomRightInnerBrush: HBRUSH; override;
    class function BottomRightInnerColor: TColor; override;
    class function BottomRightOuterBrush: HBRUSH; override;
    class function BottomRightOuterColor: TColor; override;
    class function TopLeftInnerBrush: HBRUSH; override;
    class function TopLeftInnerColor: TColor; override;
    class function TopLeftOuterBrush: HBRUSH; override;
    class function TopLeftOuterColor: TColor; override;
  end;

  { TdxPSCellRaisedSoftBorder }

  TdxPSCellRaisedSoftBorder = class(TdxPSCellRaisedBorder)
  protected
    class function Edge3DSoft: Boolean; override;
  public
    class function BottomRightInnerBrush: HBRUSH; override;
    class function BottomRightInnerColor: TColor; override;
  end;

  { TdxPSCellSunkenBorder }

  TdxPSCellSunkenBorder = class(TdxPSCustomCell3DBorder)
  protected
    class function Edge3DSoft: Boolean; override;
    class function Edge3DStyle: TdxCellEdgeStyle; override;
  public
    class function BottomRightInnerBrush: HBRUSH; override;
    class function BottomRightInnerColor: TColor; override;
    class function BottomRightOuterBrush: HBRUSH; override;
    class function BottomRightOuterColor: TColor; override;
    class function TopLeftInnerBrush: HBRUSH; override;
    class function TopLeftInnerColor: TColor; override;
    class function TopLeftOuterBrush: HBRUSH; override;
    class function TopLeftOuterColor: TColor; override;
  end;

  { TdxPSCellSunkenSoftBorder }

  TdxPSCellSunkenSoftBorder = class(TdxPSCellSunkenBorder)
  protected
    class function Edge3DSoft: Boolean; override;
  public
    class function BottomRightInnerBrush: HBRUSH; override;
    class function BottomRightInnerColor: TColor; override;
    class function TopLeftInnerBrush: HBRUSH; override;
    class function TopLeftInnerColor: TColor; override;
  end;

  { TdxPSCellTwistedBorderPainter }

  TdxPSCellTwistedBorderPainter = class(TdxPSCell3DBorderPainter)
  public
    class procedure Draw3DFrame(ACanvas: TdxPSReportRenderCustomCanvas; R: TRect;
      ASides: TdxCellSides; ACellBorders: TdxPSCell3DBorderClass; ALineThickness: Integer); override;
    class procedure Draw3DFrame(ACanvas: TdxPSReportRenderCustomCanvas; R: TRect;
      ASides: TdxCellSides; AOuterTLColor, AOuterBRColor: TColor;
      AInnerTLColor, AInnerBRColor: TColor; ALineThickness: Integer); override;
  end;

  { TdxPSCellTwistedBorder }

  TdxPSCellTwistedBorder = class(TdxPSCustomCell3DBorder)
  protected
    class function GetPainterClass: TdxPSCellBorderPainterClass; override;
  end;

  { TdxPSCellEtchedBorder }

  TdxPSCellEtchedBorder = class(TdxPSCellTwistedBorder)
  public
    class function BottomRightInnerBrush: HBRUSH; override;
    class function BottomRightInnerColor: TColor; override;
    class function BottomRightOuterBrush: HBRUSH; override;
    class function BottomRightOuterColor: TColor; override;
    class function TopLeftInnerBrush: HBRUSH; override;
    class function TopLeftInnerColor: TColor; override;
    class function TopLeftOuterBrush: HBRUSH; override;
    class function TopLeftOuterColor: TColor; override;
  end;

  { TdxPSCellBumpedBorder }

  TdxPSCellBumpedBorder = class(TdxPSCellTwistedBorder)
  public
    class function BottomRightInnerBrush: HBRUSH; override;
    class function BottomRightInnerColor: TColor; override;
    class function BottomRightOuterBrush: HBRUSH; override;
    class function BottomRightOuterColor: TColor; override;
    class function TopLeftInnerBrush: HBRUSH; override;
    class function TopLeftInnerColor: TColor; override;
    class function TopLeftOuterBrush: HBRUSH; override;
    class function TopLeftOuterColor: TColor; override;
  end;

  { TdxPSColorBorderPainter }

  TdxPSColorBorderPainter = class(TdxPSCellBorderPainter)
  protected
    function GetSideColor(ASide: TdxCellSide): TColor; virtual;
  public
    procedure DrawBorders(ACanvas: TdxPSReportRenderCustomCanvas; const R: TRect); override;
    //
    property SideColor[ASide: TdxCellSide]: TColor read GetSideColor;
  end;

  { TdxPSColorBorder }

  TdxPSColorBorder = class(TdxPSCellUltraFlatBorder)
  protected
    class function GetPainterClass: TdxPSCellBorderPainterClass; override;
  end;

  TdxPSBackgroundBitmapPool = class
  private
    FItems: TList;
    function GetCount: Integer;
    function GetItem(Index: Integer): TBitmap;
  protected
    procedure ReadData(AReader: TdxPSDataReader);
    procedure WriteData(AWriter: TdxPSDataWriter);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TdxPSBackgroundBitmapPool);

    function Add(ABitmap: TGraphic): Integer;
    procedure Clear;
    procedure Delete(AnIndex: Integer);
    function Find(ABitmap: TGraphic; out AnIndex: Integer): Boolean;

    property Count: Integer read GetCount;
    property Items[Index: Integer]: TBitmap read GetItem; default;
  end;

  { TdxPSBrushPoolItem }

  TdxPSBrushPoolItem = class(TObject)
  strict private
    FBrush: TBrush;
    FColor: TColor;

    function GetBrush: TBrush;
  public
    constructor Create(AColor: TColor);
    destructor Destroy; override;
    //
    property Brush: TBrush read GetBrush;
    property Color: TColor read FColor;
  end;

  { TdxPSReportBrushPool }

  TdxPSReportBrushPool = class(TObject)
  strict private
    FItems: TList;

    function GetBrush(AColor: TColor): TBrush;
    function GetCount: Integer;
    function GetItem(Index: Integer): TdxPSBrushPoolItem;
  protected
    function Add(AColor: TColor): Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function IndexOf(AColor: TColor): Integer;
    //
    property Brushes[AColor: TColor]: TBrush read GetBrush; default;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TdxPSBrushPoolItem read GetItem;
  end;

  { TdxPSFontPoolItem }

  TdxPSFontPoolItem = class
  strict private
    FFont: TFont;
    FOriginalHeight: Integer;
    FOriginalPixelsPerInch: Integer;
  protected
    procedure ReadData(AReader: TdxPSDataReader);
    procedure WriteData(AWriter: TdxPSDataWriter);
  public
    constructor Create(AFont: TFont; AFontPPI: Integer); overload;
    constructor Create(const AName: string; AColor: TColor;
      APitch: TFontPitch; AStyle: TFontStyles; AHeight, AFontPPI: Integer); overload;
    destructor Destroy; override;
    procedure PrepareFont(AUnitsPerInch: Integer);

    property Font: TFont read FFont;
  end;

  { TdxPSReportFontPool }

  TdxPSReportFontPool = class
  strict private
    FItems: TList;
    FOwner: TBasedxReportLink;

    function GetCount: Integer;
    function GetFont(Index: Integer): TFont;
    function GetItem(Index: Integer): TdxPSFontPoolItem;
  protected
    FLocked: Boolean;

    function CreateFont(AFont: TFont): Integer; overload;
    function CreateFont(const AName: string; AColor: TColor;
      APitch: TFontPitch; AStyle: TFontStyles; AHeight: Integer): Integer; overload;
    procedure FontChanged(Sender: TObject);
    procedure PrepareFonts(UPI: Integer);

    procedure ReadData(AReader: TdxPSDataReader);
    procedure WriteData(AWriter: TdxPSDataWriter);
  public
    constructor Create(AOwner: TBasedxReportLink);
    destructor Destroy; override;

    function Add(AFont: TFont): Integer; overload;
    function Add(const AName: string; AColor: TColor;
      APitch: TFontPitch; AStyle: TFontStyles; AHeight: Integer): Integer; overload;
    procedure Clear;
    function IndexOf(AFont: TFont): Integer; overload;
    function IndexOf(const AName: string; AColor: TColor;
      APitch: TFontPitch; AStyle: TFontStyles; AHeight: Integer): Integer; overload;

    property Count: Integer read GetCount;
    property Fonts[Index: Integer]: TFont read GetFont; default;
    property Items[Index: Integer]: TdxPSFontPoolItem read GetItem;
  end;

  { TdxPSCachedGraphicInfo }

  TdxPSCachedGraphicInfo = class(TObject)
  private
    FRenderer: TdxPSReportRenderer;
    FGraphicHelper: TdxDrawGraphicHelper;
  protected
    UnitsPerInch: Integer;
    UnitsPerPixel: Integer;
    ViewPortRect: TRect;
    ZoomFactor: Integer;
    function CheckRendererModeInfo: Boolean;
    procedure SaveModeInfo;

    property Renderer: TdxPSReportRenderer read FRenderer;
  public
    constructor Create(ARenderer: TdxPSReportRenderer); virtual;
    destructor Destroy; override;
    procedure Clear;
    procedure ValidateGraphicHelper(AGraphic: TGraphic; AImageList: TCustomImageList;
      AImageIndex: Integer; ABackgroundColor, ATransparentColor: TColor; AIsTransparent: Boolean);
    property GraphicHelper: TdxDrawGraphicHelper read FGraphicHelper;
  end;

  TdxPSRenderStage = (rsFirstPass, rsSecondPass);
  TdxPSRenderStages = set of TdxPSRenderStage;

  { TdxPSReportRenderer }

  TdxPSReportRenderer = class(TObject)
  private
    FBorderColor: TColor;
    FBorderPainters: TList;
    FBrushPool: TdxPSReportBrushPool;
    FCachedGraphicInfo: TdxPSCachedGraphicInfo;
    FCanvas: TdxPSReportRenderCustomCanvas;
    FCheckBitmap: TcxBitmap;
    FGroupLookAndFeelPainters: TList;
    FHFStrings: TStrings;
    FIsRendering: Boolean;
    FLineThickness: Integer;
    FMarlettFont10: TFont;
    FMarlettFont8: TFont;
    FPixelsPerInch: Integer;
    FRenderingPageIndex: Integer; // virtual index
    FRenderStage: TdxPSRenderStages;
    FReportLink: TBasedxReportLink;
    FSymbolFont: TFont;
    FUnitsPerPixel: Integer;
    FViewPortRect: TRect;
    FZoomFactor: Integer;

    function GetBorderPainterItem(Index: Integer): TdxPSCellBorderPainter;
    function GetBorderPainterCount: Integer;
    function GetGroupLookAndFeelPainter(Index: Integer): TdxPSReportGroupLookAndFeelPainter;
    function GetGroupLookAndFeelPainterCount: Integer;
    function GetHalfLineThickness: Integer;
    function GetIsCompositionPagePart: Boolean;
    function GetIsPrinting: Boolean;
    function GetPageRenderInfo: TdxPSPageRenderInfo;
    function GetRenderInfo: TdxPSReportRenderInfo;
    function GetReportCells: TdxReportCells;
  protected
    function CustomDrawReportItem(AItem: TAbstractdxReportCellData): Boolean;
    procedure Get3DBorderBrushes(AnItem: TdxReportVisualItem; var AOuterTLBrush, AOuterBRBrush, AInnerTLBrush, AInnerBRBrush: HBRUSH);
    procedure Get3DBorderColors(AnItem: TdxReportVisualItem; var AOuterTLColor, AOuterBRColor, AInnerTLColor, AInnerBRColor: TColor);

    function GetPageContentClipRect: TRect;
    function GetUnitsPerInch: Integer; virtual;
    function GetUnitsPerInchEx(AAdjustOnReportScale: Boolean): Integer; virtual;

    function CreateBorderPainter(AClass: TdxPSCellBorderPainterClass): TdxPSCellBorderPainter;
    function FindBorderPainter(AClass: TdxPSCellBorderPainterClass): TdxPSCellBorderPainter;
    procedure FreeAndNilBorderPainters;

    function CreateReportGroupLookAndFeelPainter(AClass: TdxPSReportGroupLookAndFeelPainterClass): TdxPSReportGroupLookAndFeelPainter;
    function FindReportGroupLookAndFeelPainter(AClass: TdxPSReportGroupLookAndFeelPainterClass): TdxPSReportGroupLookAndFeelPainter;
    procedure FreeAndNilReportGroupLookAndFeelPainters;

    procedure PrepareFont(AFont: TFont; AAdjustOnScale: Boolean); virtual;
    procedure PrepareFonts;
    procedure PrepareGDIObjects; virtual;
    procedure PrepareLogicalCoordinates; virtual;
    procedure PrepareLogicalUnits; virtual;
    function PreparedPageIndex(APageIndex: Integer): Integer;
    procedure PrepareRenderPage; virtual;
    procedure RenderCell(ACell: TdxReportCell; const OriginRect: TRect);
    procedure RenderDelimiters;
    procedure RenderEntirePage(ARealPageIndex: Integer); virtual;
    procedure RenderPageContent; virtual;
    procedure RenderPageContentDetails(const ADetailsPosition: TPoint; out AOffset: Integer); virtual;
    procedure RenderPageContentFooter(ADetailsOffset: Integer); virtual;
    procedure RenderPageContentHeader(ACell: TdxReportCell; AAnchors: TAnchors; const R: TRect; const AOffset: TPoint); virtual;
    procedure RenderPageContentHeaders(const ADetailsPosition: TPoint); virtual;
    procedure RenderPageContentPart(ACell: TdxReportCell; StartIndex, EndIndex: Integer; const OriginRect: TRect);
    procedure RenderPageOverlay(AOverlayIndex: Integer; AOverlay: TdxPageOverlayIndexes; const OriginRect: TRect); virtual;
    procedure RenderPageBackground(ARealPageIndex: Integer); virtual;
    procedure RenderPageFooter(ARealPageIndex: Integer); virtual;
    procedure RenderPageHeader(ARealPageIndex: Integer); virtual;
    procedure RenderPageHeaderOrFooter(AHeaderOrFooter: TCustomdxPageObject; APageIndex: Integer; ARect: TRect); virtual;
    procedure RenderPageHeaderOrFooterContent(HF: TCustomdxPageObject; APageIndex: Integer;
      ARect: TRect; ATitleParts: TdxPageTitleParts; ADrawBackground: Boolean); virtual;
    procedure RenderPageHeaderOrFooterContentPart(ATitlePart: TdxPageTitlePart; AStrings: TStrings;
      ATextAlignY: TcxTextAlignY; ALineHeight, ADestWidth, ADestHeight: Integer; const ARect: TRect);
    procedure RenderPageReportArea(ARealPageIndex: Integer); virtual;
    procedure RestoreMapMode; virtual;
    procedure SaveMapMode; virtual;
    procedure UnprepareGDIObjects; virtual;
    procedure UnprepareLogicalUnits; virtual;
    procedure UnprepareRenderPage; virtual;

    property BorderPainterCount: Integer read GetBorderPainterCount;
    property BorderPainters[Index: Integer]: TdxPSCellBorderPainter read GetBorderPainterItem;
    property BrushPool: TdxPSReportBrushPool read FBrushPool;
    property CachedGraphicInfo: TdxPSCachedGraphicInfo read FCachedGraphicInfo;
    property GroupLookAndFeelPainterCount: Integer read GetGroupLookAndFeelPainterCount;
    property GroupLookAndFeelPainters[Index: Integer]: TdxPSReportGroupLookAndFeelPainter read GetGroupLookAndFeelPainter;
  public
    constructor Create(AReportLink: TBasedxReportLink); virtual;
    destructor Destroy; override;

    function CalcTextHeight(ACanvas: TdxPSReportRenderCustomCanvas;
      const AText: string; AWordBreak: Boolean; AFont: TFont = nil;
      ABaseWidth: Integer = -1): Integer;
    function CalcTextLineCount(ACanvas: TdxPSReportRenderCustomCanvas;
      const AText: string; AFont: TFont = nil; ABaseWidth: Integer = -1): Integer;
    function CalcTextPatternHeight(ACanvas: TdxPSReportRenderCustomCanvas; AFont: TFont = nil): Integer;
    function CalcTextRect(ACanvas: TdxPSReportRenderCustomCanvas;
      const AText: string; var ARect: TRect; AWordBreak: Boolean; AFont: TFont = nil): Integer; overload;
    function CalcTextRect(ACanvas: TdxPSReportRenderCustomCanvas; const AText: string;
      var ARect: TRect; AFormat: DWORD; AFont: TFont = nil): Integer; overload;
    function CalcTextWidth(ACanvas: TdxPSReportRenderCustomCanvas;
      const AText: string; AFont: TFont = nil): Integer;

    procedure DrawCheckBox(ACanvas: TdxPSReportRenderCustomCanvas; var R: TRect;
      AChecked, AEnabled, AIsRadio: Boolean; AEdgeStyle: TdxCheckButtonEdgeStyle;
      ABorderColor: TColor = clWindowText); virtual;
    procedure DrawEdge(ACanvas: TdxPSReportRenderCustomCanvas; var R: TRect;
      AEdgeMode: TdxCellEdgeMode; AEdge3DEdge: TdxCellEdgeStyle; ASides: TdxCellSides;
      ASoft: Boolean; ABorderColor: TColor = -1); virtual;
    procedure DrawEllipse(ACanvas: TdxPSReportRenderCustomCanvas; R: TRect;
      AForeColor, ABackColor: TColor; APattern: TdxPSFillPatternClass;
      ABorderColor: TColor; ABorderThickness: Integer = 1); virtual;
    procedure DrawExpandButton(ACanvas: TdxPSReportRenderCustomCanvas; var R: TRect;
      AExpanded, ADrawBorder, AEdge3D, AEdge3DSoft, AShadow, AFillInterior: Boolean;
      ABorderColor, AInteriorColor: TColor); virtual;
    procedure DrawGlyph(ACanvas: TdxPSReportRenderCustomCanvas; const R: TRect; AGlyph: Byte);
    // Draw indvidual glyph from Font (used to render SortMark, ExpandButton, CheckBox Glyphs and so on)
    procedure DrawGraphic(ACanvas: TdxPSReportRenderCustomCanvas; var R: TRect;
      const AClipRect: TRect; AImageList: TCustomImageList; AImageIndex: Integer;
      AGraphic: TGraphic; AGraphicTransparent, ATransparent: Boolean; AColor: TColor); virtual;
    procedure DrawGraphicEx(ACanvas: TdxPSReportRenderCustomCanvas; R: TRect;
      const AClipRect: TRect; AImageList: TCustomImageList; AImageIndex: Integer;
      AGraphic: TGraphic; AGraphicTransparent, ATransparent: Boolean;
      AColor, ABkColor: TColor; APattern: TdxPSFillPatternClass;
      AActualImageBuffering: TdxCellImageActualBuffering = cibAlways; ATransparentColor: TColor = clNone); virtual;
    procedure DrawRectangle(ACanvas: TdxPSReportRenderCustomCanvas; R: TRect;
      AForeColor, ABackColor: TColor; AContentPattern: TdxPSFillPatternClass;
      ABorderColor: TColor; ABorderThickness: Integer = 1); virtual;
    procedure DrawRoundRect(ACanvas: TdxPSReportRenderCustomCanvas; R: TRect;
      AEllipseWidth, AEllipseHeight: Integer; AForeColor, ABackColor: TColor;
      AContentPattern: TdxPSFillPatternClass; ABorderColor: TColor;
      ABorderThickness: Integer = 1); virtual;
    procedure DrawSortMark(ACanvas: TdxPSReportRenderCustomCanvas; var R: TRect;
      ASortOrder: TdxCellSortOrder; AMono: Boolean); virtual;
    procedure DrawText(ACanvas: TdxPSReportRenderCustomCanvas; var R: TRect;
      AMaxLineCount, ALeftIndent, ARightIndent: Integer; const AText: string;
      AFont: TFont; ABkColor: TColor; ATextAlignX: TcxTextAlignX;
      ATextAlignY: TcxTextAlignY; AFillBackground, AMultiline, AEndEllipsis: Boolean;
      APreventLeftTextExceed: Boolean = True; APreventTopTextExceed: Boolean = True;
      AHidePrefix: Boolean = True);
    procedure DrawTextEx(ACanvas: TdxPSReportRenderCustomCanvas; var R: TRect;
      AMaxLineCount, ALeftIndent, ARightIndent: Integer; const AText: string;
      AFont: TFont; AFormat: Cardinal); overload;
    procedure DrawTextEx(ACanvas: TdxPSReportRenderCustomCanvas; var R: TRect; AMaxLineCount: Integer;
      const AText: string; AFont: TFont; AFormat: Cardinal; const AIndents: TRect; ALineSpacing: Single = 1.0); overload;
    function MakeTextFormat(ATextAlignX: TcxTextAlignX; ATextAlignY: TcxTextAlignY;
      AMultiline, AEndEllipsis, APreventLeftTextExceed, APreventTopTextExceed, AHidePrefix: Boolean): DWORD;
    procedure FillEllipse(ACanvas: TdxPSReportRenderCustomCanvas;
      const R: TRect; AColor: TColor); virtual;
    procedure FillEllipseEx(ACanvas: TdxPSReportRenderCustomCanvas;
      const R: TRect; AForeColor, ABackColor: TColor; APattern: TdxPSFillPatternClass); virtual;
    procedure FillRect(ACanvas: TdxPSReportRenderCustomCanvas; const R: TRect; AColor: TColor); virtual;
    procedure FillRectByBrush(ACanvas: TdxPSReportRenderCustomCanvas; const R: TRect; ABrush: TBrush); virtual;
    procedure FillRectEx(ACanvas: TdxPSReportRenderCustomCanvas; const R: TRect;
      AForeColor, ABackColor: TColor; APattern: TdxPSFillPatternClass); virtual;
    procedure FillRoundRect(ACanvas: TdxPSReportRenderCustomCanvas; const R: TRect;
      AEllipseWidth, AEllipseHeight: Integer; AColor: TColor); virtual;
    procedure FillRoundRectEx(ACanvas: TdxPSReportRenderCustomCanvas;
      const R: TRect; AEllipseWidth, AEllipseHeight: Integer;
      AForeColor, ABackColor: TColor; APattern: TdxPSFillPatternClass); virtual;
    procedure FillRgn(ACanvas: TdxPSReportRenderCustomCanvas; Rgn: TcxRegionHandle; AColor: TColor); virtual;
    procedure FillRgnEx(ACanvas: TdxPSReportRenderCustomCanvas; Rgn: TcxRegionHandle;
      AForeColor, ABackColor: TColor; APattern: TdxPSFillPatternClass); virtual;
    procedure FrameEllipse(ACanvas: TdxPSReportRenderCustomCanvas; R: TRect;
      AColor: TColor; AThickness: Integer = 1);
    procedure FrameRect(ACanvas: TdxPSReportRenderCustomCanvas; R: TRect;
      AColor: TColor; ASides: TdxCellSides = [csLeft..csBottom]; AThickness: Integer = 1); virtual;
    procedure FrameRoundRect(ACanvas: TdxPSReportRenderCustomCanvas; R: TRect;
      AEllipseWidth, AEllipseHeight: Integer; AColor: TColor; AThickness: Integer = 1); virtual;
    procedure Polyline(ACanvas: TdxPSReportRenderCustomCanvas;
      const APoints: array of TPoint; AColor: TColor; AThickness: Integer = 1); virtual;
    procedure Polygone(ACanvas: TdxPSReportRenderCustomCanvas;
      const APoints: array of TPoint; ABkColor, AFrameColor: TColor; AThickness: Integer = 1); virtual;

    function GetBorderPainter(AClass: TdxPSCellBorderPainterClass): TdxPSCellBorderPainter;
    function GetBrushByColor(AColor: TColor): HBRUSH;
    function GetPatternBrush(APattern: TdxPSFillPatternClass; AColor: TColor): TBrush;
    function GetReportGroupLookAndFeelPainter(AClass: TdxPSReportGroupLookAndFeelPainterClass): TdxPSReportGroupLookAndFeelPainter;

    procedure RenderPage(ACanvas: TCanvas; const APageBounds: TRect;
      APageIndex, AContinuousPageIndex, AZoomFactor: Integer); virtual;
    procedure RenderPageEx(ACanvas: TdxPSReportRenderCustomCanvas;
      const APageBounds: TRect; APageIndex, AContinuousPageIndex, AZoomFactor: Integer); virtual;

    property BorderColor: TColor read FBorderColor;
    property Canvas: TdxPSReportRenderCustomCanvas read FCanvas;
    property HalfLineThickness: Integer read GetHalfLineThickness;
    property IsCompositionPagePart: Boolean read GetIsCompositionPagePart;
    property IsPrinting: Boolean read GetIsPrinting;
    property IsRendering: Boolean read FIsRendering;
    property LineThickness: Integer read FLineThickness write FLineThickness;
    property MarlettFont10: TFont read FMarlettFont10;
    property MarlettFont8: TFont read FMarlettFont8;
    property PageRenderInfo: TdxPSPageRenderInfo read GetPageRenderInfo;
    property PixelsPerInch: Integer read FPixelsPerInch write FPixelsPerInch;
    property RenderInfo: TdxPSReportRenderInfo read GetRenderInfo;
    property RenderingPageIndex: Integer read FRenderingPageIndex; // virtual
    property RenderStage: TdxPSRenderStages read FRenderStage;
    property ReportCells: TdxReportCells read GetReportCells;
    property ReportLink: TBasedxReportLink read FReportLink;
    property SymbolFont: TFont read FSymbolFont;
    property UnitsPerInch: Integer read GetUnitsPerInch;
    property UnitsPerPixel: Integer read FUnitsPerPixel write FUnitsPerPixel;
    property ViewPortRect: TRect read FViewPortRect;
    property ZoomFactor: Integer read FZoomFactor;
  end;

  { TdxPSCustomReportExportProvider }

  TdxPSCustomReportExportProvider = class(TObject)
  private
    FReportLink: TBasedxReportLink;
    function GetRenderer: TdxPSReportRenderer;
    function GetRenderInfo: TdxPSReportRenderInfo;
  public
    constructor Create(AReportLink: TBasedxReportLink); virtual;
    procedure Build; virtual; abstract;
    procedure SaveToFile(const AFileName: string);
    procedure SaveToStream(AStream: TStream); virtual; abstract;
    //
    property Renderer: TdxPSReportRenderer read GetRenderer;
    property RenderInfo: TdxPSReportRenderInfo read GetRenderInfo;
    property ReportLink: TBasedxReportLink read FReportLink;
  end;

  { TdxPSPDFReportExportOptions }

  TdxPSPDFReportExportOptions = class(TPersistent)
  private
    FAuthor: string;
    FCompressStreams: Boolean;
    FCreator: string;
    FDefaultFileName: string;
    FDefaultFileNameAssigned: Boolean;
    FEmbedFonts: Boolean;
    FIsCreatorAssigned: Boolean;
    FIsTitleAssigned: Boolean;
    FJPEGQuality: Integer;
    FKeywords: string;
    FOpenDocumentAfterExport: Boolean;
    FPageRangeInfo: TdxPSPrintPageRangeInfo;
    FSecurityOptions: TdxPSPDFSecurityOptions;
    FSubject: string;
    FTitle: string;
    FUseCIDFonts: Boolean;
    FUseJPEGCompression: Boolean;
    function GenerateDefaultFileName: string;
    function GetCreator: string;
    function GetDefaultCreator: string;
    function GetDefaultFileName: string;
    function GetDefaultFileNameIsStored: Boolean;
    function GetDefaultTitle: string;
    function GetIsCreatorStored: Boolean;
    function GetIsTitleStored: Boolean;
    function GetTitle: string;
    procedure SetCreator(const AValue: string);
    procedure SetDefaultFileName(const AValue: string);
    procedure SetSecurityOptions(AValue: TdxPSPDFSecurityOptions);
    procedure SetTitle(const AValue: string);
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure LoadDefaultSettings;
    procedure SaveDefaultSettings;
    procedure ReadIsCreatorAssigned(Reader: TReader);
    procedure ReadIsTitleAssigned(Reader: TReader);
    procedure WriteIsCreatorAssigned(Writer: TWriter);
    procedure WriteIsTitleAssigned(Writer: TWriter);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure Assign(Source: TPersistent); override;
    procedure LoadFromIniFile(AIniFile: TCustomIniFile; const ASectionName: string); overload;
    procedure LoadFromIniFile(const AFileName: string); overload;
    procedure LoadFromRegistry(const APath: string);
    procedure SaveToIniFile(AIniFile: TCustomIniFile; const ASectionName: string); overload;
    procedure SaveToIniFile(const AFileName: string); overload;
    procedure SaveToRegistry(const APath: string);
    //
    property PageRangeInfo: TdxPSPrintPageRangeInfo read FPageRangeInfo;
  published
    property Author: string read FAuthor write FAuthor;
    property Creator: string read GetCreator write SetCreator stored GetIsCreatorStored;
    property Keywords: string read FKeywords write FKeywords;
    property Subject: string read FSubject write FSubject;
    property Title: string read GetTitle write SetTitle stored GetIsTitleStored;
    //
    property CompressStreams: Boolean read FCompressStreams write FCompressStreams default False;
    property DefaultFileName: string read GetDefaultFileName write SetDefaultFileName stored GetDefaultFileNameIsStored;
    property DefaultFileNameAssigned: Boolean read FDefaultFileNameAssigned write FDefaultFileNameAssigned default False;
    property EmbedFonts: Boolean read FEmbedFonts write FEmbedFonts default True;
    property JPEGQuality: Integer read FJPEGQuality write FJPEGQuality default 90;
    property OpenDocumentAfterExport: Boolean read FOpenDocumentAfterExport write FOpenDocumentAfterExport default False;
    property SecurityOptions: TdxPSPDFSecurityOptions read FSecurityOptions write SetSecurityOptions;
    property UseCIDFonts: Boolean read FUseCIDFonts write FUseCIDFonts default True;
    property UseJPEGCompression: Boolean read FUseJPEGCompression write FUseJPEGCompression default True;
  end;

  { TdxReportItem }

  TdxReportItemClass = class of TdxReportItem;

  TdxReportItem = class(TPersistent)
  private
    FCreatorLink: TcxObjectLink;
    FData: Int64;
    FParent: TdxReportCell;
    procedure FreeCreatorLink;
    function GetCreator: TBasedxReportLink;
    function GetIndex: Integer;
    function GetReportCells: TdxReportCells; virtual;
    function GetReportLink: TBasedxReportLink;
    procedure SetCreator(AValue: TBasedxReportLink);
    procedure SetIndex(Value: Integer);
    procedure SetParent(Value: TdxReportCell);
  protected
    function AsCell: TdxReportCell;
    function GetTopLevelParent: TdxReportCell;
    class function IsCell: Boolean; virtual;

    procedure ReadData(AReader: TdxPSDataReader); virtual;
    procedure WriteData(AWriter: TdxPSDataWriter); virtual;
    class function Serializable: Boolean; virtual;
  public
    constructor Create(AParent: TdxReportCell); virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    class function ReportItemClass: TdxReportItemClass;
    class procedure Register; virtual;
    class procedure Unregister; virtual;

    function Clone(AParent: TdxReportCell): TdxReportItem;
    function getNextSibling: TdxReportItem; // GetNextSibling conflicts with C++ macro
    function getPrevSibling: TdxReportItem; // GetPrevSibling conflicts with C++ macro
    function HasParent: Boolean;
    function IsFirstItem: Boolean;
    function IsLastItem: Boolean;

    property Creator: TBasedxReportLink read GetCreator;
    property Data: Int64 read FData write FData;
    property Index: Integer read GetIndex write SetIndex;
    property Parent: TdxReportCell read FParent write SetParent;
    property ReportCells: TdxReportCells read GetReportCells;
    property ReportLink: TBasedxReportLink read GetReportLink;
    property TopLevelParent: TdxReportCell read GetTopLevelParent;
  end;

  { TdxReportVisualItem }

  TdxReportVisualItem = class(TdxReportItem)
  private
    FBackgroundBitmapIndex: Integer;
    FBorderClass: TdxPSCellBorderClass;
    FBorderColor: TColor;
    FBoundsRect: TRect;
    FCellSideColors: array[TdxCellSide] of TColor;
    FColor: TColor;
    FFontIndex: Integer;
    FFormat: DWORD;

    function GetAbsoluteOrigin: TPoint;
    function GetAbsoluteRect: TRect;
    function GetBackgroundBitmap: TBitmap;
    function GetBackgroundBitmapHeight: Integer;
    function GetBackgroundBitmapPool: TdxPSBackgroundBitmapPool;
    function GetBackgroundBitmapTileOrigin: TPoint;
    function GetBackgroundBitmapTileStartIndexX: Integer;
    function GetBackgroundBitmapTileStartIndexY: Integer;
    function GetBackgroundBitmapTileStopIndexX: Integer;
    function GetBackgroundBitmapTileStopIndexY: Integer;
    function GetBackgroundBitmapWidth: Integer;
    function GetBorderBrush: HBRUSH;
    function GetBorderColor: TColor;
    function GetBottom: Integer;
    function GetCellSideColors(ASide: TdxCellSide): TColor;
    function GetCellSides: TdxCellSides;
    function GetContentBrush: HBRUSH;
    function GetEdge3DSoft: Boolean;
    function GetEdge3DStyle: TdxCellEdgeStyle;
    function GetEdgeMode: TdxCellEdgeMode;
    function GetExcludeFromClipRgn: Boolean;
    function GetFont: TFont;
    function GetHeight: Integer;
    function GetIsPrinting: Boolean;
    function GetLeft: Integer;
    function GetLineThickness: Integer;
    function GetOrigin: TPoint;
    function GetParentBrush: HBRUSH;
    function GetParentColor: TColor;
    function GetRenderer: TdxPSReportRenderer;
    function GetRight: Integer;
    function GetShadowBrush: HBRUSH;
    function GetShowShadow: Boolean;
    function GetTop: Integer;
    function GetTransparent: Boolean;
    function GetVisible: Boolean;
    function GetWidth: Integer;
    procedure SetBackgroundBitmapIndex(Value: Integer);
    procedure SetBorderClass(Value: TdxPSCellBorderClass);
    procedure SetBottom(const Value: Integer);
    procedure SetBoundsRect(const Value: TRect);
    procedure SetCellSideColors(ASide: TdxCellSide; AValue: TColor);
    procedure SetCellSides(Value: TdxCellSides);
    procedure SetColor(Value: TColor);
    procedure SetEdge3DSoft(Value: Boolean);
    procedure SetEdge3DStyle(Value: TdxCellEdgeStyle);
    procedure SetEdgeMode(Value: TdxCellEdgeMode);
    procedure SetExcludeFromClipRgn(Value: Boolean);
    procedure SetFont(Value: TFont);
    procedure SetFormat(Value: DWORD);
    procedure SetHeight(Value: Integer);
    procedure SetLeft(Value: Integer);
    procedure SetOrigin(const Value: TPoint);
    procedure SetRight(const Value: Integer);
    procedure SetShowShadow(Value: Boolean);
    procedure SetTop(Value: Integer);
    procedure SetTransparent(Value: Boolean);
    procedure SetVisible(Value: Boolean);
    procedure SetWidth(Value: Integer);
  protected
    procedure BoundsChanged; virtual;
    procedure ConvertCoords(APixelsNumerator, APixelsDenominator: Integer); virtual;
    procedure DoExcludeFromClipRgn(ACanvas: TdxPSReportRenderCustomCanvas; const R: TRect; var AResult: Integer);

    function GetBackgroundBitmapTileBounds(Col, Row: Integer): TRect; virtual;
    function GetBackgroundBounds(ACanvas: TdxPSReportRenderCustomCanvas): TRect; virtual;
    function GetBorderClass: TdxPSCellBorderClass; virtual;
    function GetBorderEdgeBounds(ASide: TdxCellSide; const AOuterRect: TRect): TRect; virtual;
    function GetBorderEdgeClass(ASide: TdxCellSide): TdxPSCellBorderClass; virtual;
    function GetBorderEdgeSalient(ASide: TdxCellSide; ASalient: TdxPSCellBorderSalientType): Integer; virtual;
    function GetBorderEdgeThickness(ASide: TdxCellSide): Integer; virtual;
    function GetBorderBounds(ACanvas: TdxPSReportRenderCustomCanvas): TRect; virtual;

    function GetBorderOuterBoundsRelativeTo(ACanvas: TdxPSReportRenderCustomCanvas; const R: TRect): TRect;
    function GetInnerBoundsRelativeTo(ACanvas: TdxPSReportRenderCustomCanvas; const R: TRect): TRect;
    function GetOuterBoundsRelativeTo(ACanvas: TdxPSReportRenderCustomCanvas; const R: TRect): TRect;

    function GetBorderPainter: TdxPSCellBorderPainter;
    function GetBorderPainterClass: TdxPSCellBorderPainterClass; virtual;
    procedure InitBorderPainter(ABorderPainter: TdxPSCellBorderPainter); virtual;
    function HasBorderColoration: Boolean; virtual;

    function GetFormatBit(ABit: DWORD): Boolean;
    procedure SetFormatBit(ABit: DWORD; Value: Boolean);

    function GetContentBkColor: TColor; virtual;
    function GetContentPattern: TdxPSFillPatternClass; virtual;
    function GetShadowColor: TColor; virtual;
    function GetShadowDepth: Integer; virtual;
    procedure SetContentBkColor(Value: TColor); virtual;
    procedure SetContentPattern(Value: TdxPSFillPatternClass); virtual;
    procedure SetFontIndex(Value: Integer); virtual;
    procedure SetShadowColor(Value: TColor); virtual;
    procedure SetShadowDepth(Value: Integer); virtual;

    function HasBackground: Boolean; virtual;
    function IsBackgroundBitmapDrawn: Boolean; virtual;
    function IsBackgroundDrawn: Boolean; virtual;
    function IsBordersDrawn: Boolean; virtual;
    function IsShadowDrawn: Boolean; virtual;

    procedure ReadData(AReader: TdxPSDataReader); override;
    procedure WriteData(AWriter: TdxPSDataWriter); override;

    property BackgroundBitmapHeight: Integer read GetBackgroundBitmapHeight;
    property BackgroundBitmapTileBounds[Col, Row: Integer]: TRect read GetBackgroundBitmapTileBounds;
    property BackgroundBitmapTileOrigin: TPoint read GetBackgroundBitmapTileOrigin;
    property BackgroundBitmapTileStartIndexX: Integer read GetBackgroundBitmapTileStartIndexX;
    property BackgroundBitmapTileStartIndexY: Integer read GetBackgroundBitmapTileStartIndexY;
    property BackgroundBitmapTileStopIndexX: Integer read GetBackgroundBitmapTileStopIndexX;
    property BackgroundBitmapTileStopIndexY: Integer read GetBackgroundBitmapTileStopIndexY;
    property BackgroundBitmapWidth: Integer read GetBackgroundBitmapWidth;

    property Format: DWORD read FFormat write SetFormat;
    property IsPrinting: Boolean read GetIsPrinting;
    property LineThickness: Integer read GetLineThickness;
    property Renderer: TdxPSReportRenderer read GetRenderer;
  public
    constructor Create(AParent: TdxReportCell); override;
    procedure Assign(Source: TPersistent); override;

    procedure AdjustContent(ACanvas: TdxPSReportRenderCustomCanvas); virtual;

    procedure DrawBackground(ACanvas: TdxPSReportRenderCustomCanvas); overload; virtual;
    procedure DrawBackground(ACanvas: TdxPSReportRenderCustomCanvas; const R: TRect); overload; virtual;
    procedure DrawBackgroundBitmap(ACanvas: TdxPSReportRenderCustomCanvas; const R: TRect); virtual;
    procedure DrawBackgroundBitmapTile(ACanvas: TdxPSReportRenderCustomCanvas; const Rect: TRect); virtual;
    procedure DrawBackgroundRect(ACanvas: TdxPSReportRenderCustomCanvas; const R: TRect);
    procedure DrawBorders(ACanvas: TdxPSReportRenderCustomCanvas); virtual;

    function GetBorderOuterBounds(ACanvas: TdxPSReportRenderCustomCanvas): TRect; virtual;
    function GetInnerBounds(ACanvas: TdxPSReportRenderCustomCanvas): TRect; virtual;
    function GetOuterBounds(ACanvas: TdxPSReportRenderCustomCanvas): TRect; virtual;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); virtual;

    function CalculateLineCount(ACanvas: TdxPSReportRenderCustomCanvas): Integer; virtual;

    function MeasureBordersHeight(ACanvas: TdxPSReportRenderCustomCanvas): Integer; virtual;
    function MeasureBordersWidth(ACanvas: TdxPSReportRenderCustomCanvas): Integer; virtual;
    function MeasureContentHeight(ACanvas: TdxPSReportRenderCustomCanvas): Integer; virtual;
    function MeasureContentWidth(ACanvas: TdxPSReportRenderCustomCanvas): Integer; virtual;
    function MeasureFontHeight(ACanvas: TdxPSReportRenderCustomCanvas): Integer; virtual;
    function MeasureHeight(ACanvas: TdxPSReportRenderCustomCanvas): Integer; virtual;
    function MeasureWidth(ACanvas: TdxPSReportRenderCustomCanvas): Integer; virtual;
    procedure Offset(AOffsetX, AOffsetY: Integer); virtual;

    // for backward compatibility
    class function MapBorderClass(AEdgeMode: TdxCellEdgeMode; AEdgeStyle: TdxCellEdgeStyle; ASoft: Boolean): TdxPSCellBorderClass;

    property AbsoluteOrigin: TPoint read GetAbsoluteOrigin;
    property AbsoluteRect: TRect read GetAbsoluteRect;
    property BackgroundBitmap: TBitmap read GetBackgroundBitmap;
    property BackgroundBitmapIndex: Integer read FBackgroundBitmapIndex write SetBackgroundBitmapIndex;
    property BackgroundBitmapPool: TdxPSBackgroundBitmapPool read GetBackgroundBitmapPool;
    property BorderBrush: HBRUSH read GetBorderBrush;
    property BorderClass: TdxPSCellBorderClass read GetBorderClass write SetBorderClass;
    property BorderColor: TColor read GetBorderColor write FBorderColor; // clDefault;
    property BorderEdgeClasses[Side: TdxCellSide]: TdxPSCellBorderClass read GetBorderEdgeClass;
    property BorderEdgeSalients[Side: TdxCellSide; Salient: TdxPSCellBorderSalientType]: Integer read GetBorderEdgeSalient;
    property BorderEdgeThicknesses[Side: TdxCellSide]: Integer read GetBorderEdgeThickness;
    property BorderPainter: TdxPSCellBorderPainter read GetBorderPainter;
    property BorderPainterClass: TdxPSCellBorderPainterClass read GetBorderPainterClass;
    property Bottom: Integer read GetBottom write SetBottom;
    property BoundsRect: TRect read FBoundsRect write SetBoundsRect;
    property CellSideColors[ASide: TdxCellSide]: TColor read GetCellSideColors write SetCellSideColors;
    property CellSides: TdxCellSides read GetCellSides write SetCellSides;     {csAll}
    property Color: TColor read FColor write SetColor;
    property ContentBkColor: TColor read GetContentBkColor write SetContentBkColor;
    property ContentBrush: HBRUSH read GetContentBrush;
    property ContentPattern: TdxPSFillPatternClass read GetContentPattern write SetContentPattern;
    property Edge3DSoft: Boolean read GetEdge3DSoft write SetEdge3DSoft;             // obsolete - use BorderClass instead
    property Edge3DStyle: TdxCellEdgeStyle read GetEdge3DStyle write SetEdge3DStyle; // obsolete - use BorderClass instead
    property EdgeMode: TdxCellEdgeMode read GetEdgeMode write SetEdgeMode;           // obsolete - use BorderClass instead
    property ExcludeFromClipRgn: Boolean read GetExcludeFromClipRgn write SetExcludeFromClipRgn;
    property Font: TFont read GetFont write SetFont;
    property FontIndex: Integer read FFontIndex write SetFontIndex;
    property Height: Integer read GetHeight write SetHeight;
    property Left: Integer read GetLeft write SetLeft;
    property Origin: TPoint read GetOrigin write SetOrigin;
    property ParentBrush: HBRUSH read GetParentBrush;
    property ParentColor: TColor read GetParentColor;
    property Right: Integer read GetRight write SetRight;
    property ShadowBrush: HBRUSH read GetShadowBrush;
    property ShadowColor: TColor read GetShadowColor write SetShadowColor;
    property ShadowDepth: Integer read GetShadowDepth write SetShadowDepth;
    property ShowShadow: Boolean read GetShowShadow write SetShowShadow;
    property Top: Integer read GetTop write SetTop;
    property Transparent: Boolean read GetTransparent write SetTransparent;
    property Visible: Boolean read GetVisible write SetVisible;
    property Width: Integer read GetWidth write SetWidth;
  end;

  TdxReportCell = class(TdxReportVisualItem)
  private
    FCellList: TcxObjectList;
    FDataList: TcxObjectList;
    FReportCells: TdxReportCells;
    function GetAbsoluteIndex: Integer;
    function GetCellCount: Integer;
    function GetCell(Index: Integer): TdxReportCell;
    function GetClipChildren: Boolean;
    function GetDataItemCount: Integer;
    function GetDataItem(Index: Integer): TAbstractdxReportCellData;
    function GetIsTopLevel: Boolean;
    function GetLevel: Integer;
    function GetReportCells: TdxReportCells; override;
    procedure CellListNeeded;
    procedure CellListRelease;
    procedure DataListNeeded;
    procedure DataListRelease;
    procedure InsertCell(AItem: TdxReportCell);
    procedure InsertDataItem(AnItem: TdxReportItem);
    procedure InsertItem(AnItem: TdxReportItem);
    procedure MoveCell(ACurIndex, ANewIndex: Integer);
    procedure MoveDataItem(ACurIndex, ANewIndex: Integer);
    procedure MoveItem(AnItem: TdxReportItem; ACurIndex, ANewIndex: Integer);
    procedure RemoveCell(AnItem: TdxReportCell);
    procedure RemoveDataItem(AnItem: TdxReportItem);
    procedure RemoveItem(AnItem: TdxReportItem);
    procedure SetClipChildren(Value: Boolean);
  protected
    procedure ConvertCoords(APixelsNumerator, APixelsDenominator: Integer); override;
    function GetBackgroundBitmapTileBounds(Col, Row: Integer): TRect; override;
    function GetBackgroundBounds(ACanvas: TdxPSReportRenderCustomCanvas): TRect; override;
    function GetBorderBounds(ACanvas: TdxPSReportRenderCustomCanvas): TRect; override;
    class function IsCell: Boolean; override;

    procedure ReadData(AReader: TdxPSDataReader); override;
    procedure WriteData(AWriter: TdxPSDataWriter); override;

    procedure ReadCells(AReader: TdxPSDataReader); virtual;
    procedure ReadDataItems(AReader: TdxPSDataReader); virtual;
    procedure ReadProperties(AReader: TdxPSDataReader); virtual;
    procedure WriteCells(AWriter: TdxPSDataWriter); virtual;
    procedure WriteDataItems(AWriter: TdxPSDataWriter); virtual;
    procedure WriteProperties(AWriter: TdxPSDataWriter); virtual;
  public
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    procedure DrawContent(ACanvas: TdxPSReportRenderCustomCanvas;
      DrawRect: TRect; const OriginRect: TRect; AStage: TdxPSRenderStages); virtual;
    procedure DrawItself(ACanvas: TdxPSReportRenderCustomCanvas;
      AStage: TdxPSRenderStages); virtual;
    procedure DrawNestedCells(ACanvas: TdxPSReportRenderCustomCanvas;
      ADrawRect: TRect; const AOriginRect: TRect; AStage: TdxPSRenderStages); virtual;
    procedure DrawNestedDataItems(ACanvas: TdxPSReportRenderCustomCanvas;
      const OriginRect: TRect; AStage: TdxPSRenderStages); virtual;
    function ExcludeNestedItems(ACanvas: TdxPSReportRenderCustomCanvas;
      const OriginRect: TRect): Integer; virtual;

    function GetBorderOuterBounds(ACanvas: TdxPSReportRenderCustomCanvas): TRect; override;
    function GetInnerBounds(ACanvas: TdxPSReportRenderCustomCanvas): TRect; override;
    function GetOuterBounds(ACanvas: TdxPSReportRenderCustomCanvas): TRect; override;

    function MeasureHeight(ACanvas: TdxPSReportRenderCustomCanvas): Integer; override;
    function MeasureWidth(ACanvas: TdxPSReportRenderCustomCanvas): Integer; override;
    procedure Offset(AOffsetX, AOffsetY: Integer); override;


    function AddCell: TdxReportCell;
    function AddDataItem(AClass: TdxReportCellDataClass): TAbstractdxReportCellData;
    procedure AllocateSpaceForCells(ACapacity: Integer);
    procedure AllocateSpaceForDatas(ACapacity: Integer);
    procedure ClearAll;
    procedure ClearCells;
    procedure ClearDataItems;
    procedure DeleteCell(Index: Integer);
    procedure DeleteDataItem(Index: Integer);
    function FirstCell: TdxReportCell;
    function FindDataItemByData(const AData: Int64; var ADataItem: TAbstractdxReportCellData): Boolean;
    function HasChildren: Boolean;
    function IndexOf(AnItem: TdxReportItem): Integer;
    function LastCell: TdxReportCell;

    property AbsoluteIndex: Integer read GetAbsoluteIndex;
    property CellCount: Integer read GetCellCount;
    property Cells[Index: Integer]: TdxReportCell read GetCell; default;
    property ClipChildren: Boolean read GetClipChildren write SetClipChildren;
    property DataItemCount: Integer read GetDataItemCount;
    property DataItems[Index: Integer]: TAbstractdxReportCellData read GetDataItem;
    property IsTopLevel: Boolean read GetIsTopLevel;
    property Level: Integer read GetLevel;
  end;

  TdxReportGroup = class;

  TdxPSReportGroupLookAndFeelPainter = class
  private
    FGroup: TdxReportGroup;
    FLookAndFeel: TdxPSReportGroupLookAndFeel;
    FRenderer: TdxPSReportRenderer;
  protected
    procedure DrawBorders(ACanvas: TdxPSReportRenderCustomCanvas); virtual;
    procedure DrawCaption(ACanvas: TdxPSReportRenderCustomCanvas); virtual;
    procedure DrawCaptionText(ACanvas: TdxPSReportRenderCustomCanvas); virtual;
    procedure DrawCheckBox(ACanvas: TdxPSReportRenderCustomCanvas); virtual;
    procedure Initialize(ALookAndFeel: TdxPSReportGroupLookAndFeel; AGroup: TdxReportGroup); virtual;

    function Group: TdxReportGroup; overload; virtual;
    function LookAndFeel: TdxPSReportGroupLookAndFeel; overload; virtual;
  public
    constructor Create(ARenderer: TdxPSReportRenderer); virtual;
    procedure Paint(ACanvas: TdxPSReportRenderCustomCanvas); virtual;
    property Renderer: TdxPSReportRenderer read FRenderer;
  end;

  TdxPSReportGroupLookAndFeel = class(TPersistent)
  private
    FCaptionHeight: Integer;
    FCaptionFontIndex: Integer;
    FCaptionIndent: Integer;
    FColor: TColor;
    FData: Pointer;
    FFontIndex: Integer;
    FReportCells: TdxReportCells;
    function GetCaptionFont: TFont;
    function GetFont: TFont;
    function GetRenderer: TdxPSReportRenderer;
    procedure SetCaptionFont(Value: TFont);
    procedure SetCaptionFontIndex(Value: Integer);
    procedure SetFont(Value: TFont);
    procedure SetFontIndex(Value: Integer);
  protected
    procedure AdjustBorderOuterBounds(var R: TRect; AGroup: TdxReportGroup); virtual;
    procedure ConvertCoords(APixelsNumerator, APixelsDenominator: Integer); virtual;

    procedure ReadData(AReader: TdxPSDataReader); virtual;
    procedure WriteData(AWriter: TdxPSDataWriter); virtual;

    function GetBorderEdgeThickness(AGroup: TdxReportGroup; ASide: TdxCellSide): Integer; virtual;
    function GetBorderSides(AGroup: TdxReportGroup): TdxCellSides; virtual;
    function GetBorderThickness: Integer; virtual;
    function GetColor: TColor; virtual;
    procedure SetBorderThickness(Value: Integer); virtual;

    function GetCaptionAreaHeight(AGroup: TdxReportGroup): Integer; virtual;
    function GetCaptionBounds(AGroup: TdxReportGroup): TRect; virtual;
    function GetCaptionColor: TColor; virtual;
    function GetCaptionHeight(AGroup: TdxReportGroup): Integer; virtual;
    function GetCaptionIndent: Integer; virtual;
    function GetCaptionLeftRestSpaceBounds(AGroup: TdxReportGroup): TRect; virtual;
    function GetCaptionRightRestSpaceBounds(AGroup: TdxReportGroup): TRect; virtual;
    function GetCaptionTextBounds(AGroup: TdxReportGroup): TRect; virtual;

    function GetCheckBounds(AGroup: TdxReportGroup): TRect; virtual;
    function GetCheckWidth: Integer;

    function GetPainter: TdxPSReportGroupLookAndFeelPainter;
    class function GetPainterClass: TdxPSReportGroupLookAndFeelPainterClass; virtual;

    property BorderThickness: Integer read GetBorderThickness write SetBorderThickness;
    property CaptionColor: TColor read GetCaptionColor;
    property CaptionIndent: Integer read GetCaptionIndent;
    property Renderer: TdxPSReportRenderer read GetRenderer;
    property ReportCells: TdxReportCells read FReportCells;
  public
    constructor Create(AReportCells: TdxReportCells); virtual;
    procedure Assign(Source: TPersistent); override;

    class function BorderClass: TdxPSCellBorderClass; virtual;
    class function DefaultBorderSides: TdxCellSides; virtual;
    class function Name: string; virtual;

    class procedure Register; virtual;
    class procedure Unregister; virtual;

    procedure Paint(ACanvas: TdxPSReportRenderCustomCanvas; AGroup: TdxReportGroup);
    procedure Prepare(ACanvas: TdxPSReportRenderCustomCanvas); virtual;

    property CaptionFont: TFont read GetCaptionFont write SetCaptionFont;
    property CaptionFontIndex: Integer read FCaptionFontIndex write SetCaptionFontIndex;
    property Color: TColor read GetColor write FColor;
    property Data: Pointer read FData write FData;
    property Font: TFont read GetFont write SetFont;
    property FontIndex: Integer read FFontIndex write SetFontIndex;
  end;

  { TdxPSReportGroupNullLookAndFeelPainter }

  TdxPSReportGroupNullLookAndFeelPainter = class(TdxPSReportGroupLookAndFeelPainter)
  public
    procedure Paint(ACanvas: TdxPSReportRenderCustomCanvas); override;
  end;

  { TdxPSReportGroupNullLookAndFeel }

  TdxPSReportGroupNullLookAndFeel = class(TdxPSReportGroupLookAndFeel)
  protected
    function GetBorderThickness: Integer; override;
    function GetCaptionHeight(AGroup: TdxReportGroup): Integer; override;
    class function GetPainterClass: TdxPSReportGroupLookAndFeelPainterClass; override;
  public
    class function BorderClass: TdxPSCellBorderClass; override;
    class function DefaultBorderSides: TdxCellSides; override;
    class function Name: string; override;
  end;

  { TdxPSReportGroupStandardLookAndFeel }

  TdxPSReportGroupStandardLookAndFeel = class(TdxPSReportGroupLookAndFeel)
  protected
    class function GetPainterClass: TdxPSReportGroupLookAndFeelPainterClass; override;
  public
    class function BorderClass: TdxPSCellBorderClass; override;
    class function Name: string; override;

    property CaptionFont;
    property CaptionFontIndex;
  end;

  { TdxPSReportGroupStandardLookAndFeelPainter }

  TdxPSReportGroupStandardLookAndFeelPainter = class(TdxPSReportGroupLookAndFeelPainter)
  protected
    procedure DrawBorders(ACanvas: TdxPSReportRenderCustomCanvas); override;
    procedure DrawCaptionRestSpace(ACanvas: TdxPSReportRenderCustomCanvas); virtual;
    function LookAndFeel: TdxPSReportGroupStandardLookAndFeel; reintroduce; overload;
  public
    procedure Paint(ACanvas: TdxPSReportRenderCustomCanvas); override;
  end;

  { TdxPSReportGroupPanelStyleLookAndFeel }

  TdxPSReportGroupPanelStyleLookAndFeel = class(TdxPSReportGroupStandardLookAndFeel)
  protected
    procedure AdjustBorderOuterBounds(var R: TRect; AGroup: TdxReportGroup); override;
    class function GetPainterClass: TdxPSReportGroupLookAndFeelPainterClass; override;
    function GetBorderEdgeThickness(AGroup: TdxReportGroup; ASide: TdxCellSide): Integer; override;
  end;

  { TdxPSReportGroupPanelStyleLookAndFeelPainter }

  TdxPSReportGroupPanelStyleLookAndFeelPainter = class(TdxPSReportGroupStandardLookAndFeelPainter)
  public
    procedure Paint(ACanvas: TdxPSReportRenderCustomCanvas); override;
  end;

  { TdxPSReportGroupOfficeLookAndFeel }

  TdxPSReportGroupOfficeLookAndFeel = class(TdxPSReportGroupStandardLookAndFeel)
  protected
    function GetCaptionIndent: Integer; override;
  public
    class function DefaultBorderSides: TdxCellSides; override;
    class function Name: string; override;
  end;

  { TdxPSReportGroupWebLookAndFeel }

  TdxPSReportGroupWebLookAndFeel = class(TdxPSReportGroupLookAndFeel)
  private
    FBorderColor: TColor;
    FBorderThickness: Integer;
    FCaptionColor: TColor;
    FCaptionSeparatorColor: TColor;
    FCaptionSeparatorThickness: Integer;
    procedure SetCaptionSeparatorThickness(Value: Integer);
  protected
    procedure ConvertCoords(APixelsNumerator, APixelsDenominator: Integer); override;

    procedure ReadData(AReader: TdxPSDataReader); override;
    procedure WriteData(AWriter: TdxPSDataWriter); override;

    function GetBorderEdgeThickness(AGroup: TdxReportGroup; ASide: TdxCellSide): Integer; override;
    function GetBorderThickness: Integer; override;
    procedure SetBorderThickness(Value: Integer); override;

    function GetBorderColor: TColor; virtual;
    function GetCaptionColor: TColor; override;
    function GetCaptionSeparatorColor: TColor; virtual;

    function GetCaptionAreaHeight(AGroup: TdxReportGroup): Integer; override;
    function GetCaptionBounds(AGroup: TdxReportGroup): TRect; override;
    function GetCaptionLeftRestSpaceBounds(AGroup: TdxReportGroup): TRect; override;
    function GetCaptionRightRestSpaceBounds(AGroup: TdxReportGroup): TRect; override;
    function GetCaptionSeparatorBounds(AGroup: TdxReportGroup): TRect; virtual;
    function GetCaptionTextBounds(AGroup: TdxReportGroup): TRect; override;

    class function GetPainterClass: TdxPSReportGroupLookAndFeelPainterClass; override;
  public
    constructor Create(AReportCells: TdxReportCells); override;
    procedure Assign(Source: TPersistent); override;

    class function BorderClass: TdxPSCellBorderClass; override;
    class function Name: string; override;

    property BorderColor: TColor read GetBorderColor write FBorderColor;
    property BorderThickness;
    property CaptionColor write FCaptionColor;
    property CaptionFont;
    property CaptionSeparatorColor: TColor read GetCaptionSeparatorColor write FCaptionSeparatorColor;
    property CaptionSeparatorThickness: Integer read FCaptionSeparatorThickness write SetCaptionSeparatorThickness;
  end;

  { TdxPSReportGroupWebLookAndFeelPainter }

  TdxPSReportGroupWebLookAndFeelPainter = class(TdxPSReportGroupLookAndFeelPainter)
  protected
    procedure DrawBorders(ACanvas: TdxPSReportRenderCustomCanvas); override;
    procedure DrawCaptionSeparator(ACanvas: TdxPSReportRenderCustomCanvas); virtual;
    function LookAndFeel: TdxPSReportGroupWebLookAndFeel; reintroduce; overload;
  public
    procedure Paint(ACanvas: TdxPSReportRenderCustomCanvas); override;
  end;

  { TdxReportGroup }

  TdxReportGroup = class(TdxReportCell)
  private
    FCaptionText: string;
    FCaptionTextWidth: Integer;
    FCheckBoxChecked: Boolean;
    FCheckBoxVisible: Boolean;
    FLookAndFeel: TdxPSReportGroupLookAndFeel;

    function GetCaptionAlignment: TcxTextAlignX;
    function GetCaptionTextWidth: Integer;
    function GetCaptionTransparent: Boolean;
    function GetLookAndFeel: TdxPSReportGroupLookAndFeel;
    function GetLookAndFeelIndex: Integer;
    function GetShowCaption: Boolean;
    function GetUseOwnBorderClass: Boolean;
    procedure SetCaptionAlignment(Value: TcxTextAlignX);
    procedure SetCaptionTransparent(Value: Boolean);
    procedure SetLookAndFeel(Value: TdxPSReportGroupLookAndFeel);
    procedure SetShowCaption(Value: Boolean);
    procedure SetUseOwnBorderClass(Value: Boolean);
  protected
    procedure AdjustBorderOuterBounds(var R: TRect); virtual;
    procedure ConvertCoords(APixelsNumerator, APixelsDenominator: Integer); override;
    procedure InternalDrawBorders(ACanvas: TdxPSReportRenderCustomCanvas); virtual;

    function GetBorderClass: TdxPSCellBorderClass; override;
    function GetBorderEdgeSalient(ASide: TdxCellSide; ASalient: TdxPSCellBorderSalientType): Integer; override;
    function GetBorderEdgeThickness(ASide: TdxCellSide): Integer; override;
    function InternalGetBorderEdgeThickness(ASide: TdxCellSide): Integer;

    function IsBordersDrawn: Boolean; override;

    procedure ReadData(AReader: TdxPSDataReader); override;
    procedure WriteData(AWriter: TdxPSDataWriter); override;

    property CaptionTextWidth: Integer read GetCaptionTextWidth;
    property LookAndFeelIndex: Integer read GetLookAndFeelIndex;
  public
    constructor Create(AParent: TdxReportCell); override;
    procedure Assign(Source: TPersistent); override;

    procedure DrawBorders(ACanvas: TdxPSReportRenderCustomCanvas); override;

    procedure CalculateCaptionTextWidth(ACanvas: TdxPSReportRenderCustomCanvas);
    function GetBorderOuterBounds(ACanvas: TdxPSReportRenderCustomCanvas): TRect; override;

    property CaptionAlignment: TcxTextAlignX read GetCaptionAlignment write SetCaptionAlignment;
    property CaptionText: string read FCaptionText write FCaptionText;
    property CaptionTransparent: Boolean read GetCaptionTransparent write SetCaptionTransparent;
    property CheckBoxVisible: Boolean read FCheckBoxVisible write FCheckBoxVisible;
    property CheckBoxChecked: Boolean read FCheckBoxChecked write FCheckBoxChecked;

    property LookAndFeel: TdxPSReportGroupLookAndFeel read GetLookAndFeel write SetLookAndFeel;
    property ShowCaption: Boolean read GetShowCaption write SetShowCaption;
    property UseOwnBorderClass: Boolean read GetUseOwnBorderClass write SetUseOwnBorderClass;
  end;

  { TdxReportCells }

  TdxReportCells = class(TPersistent)
  strict private
    FBorderColor: TColor;
    FBorderWidth: Integer;
    FCells: TdxReportCell;
    FExpandButtonBorderColor: TColor;
    FFooterCells: TdxReportCell;
    FGroupBorderColor: TColor;
    FGroupCaptionColor: TColor;
    FGroupCaptionSeparatorColor: TColor;
    FGroupColor: TColor;
    FHeaderCells: TdxReportCell;
    FHeaderCornerCells: TdxReportCell;
    FImageLists: TList;
    FLookAndFeel: TdxPSReportGroupLookAndFeel;
    FLookAndFeels: TList;
    FOverlays: TList;
    FOwnImageLists: Boolean;
    FReportLink: TBasedxReportLink;
    FRowHeaderCells: TdxReportCell;
    FShadowColor: TColor;
    FShadowDepth: Integer;
    FTreeLineColor: TColor;
    FTreeLineStyle: TdxPSTreeLineStyle;

    function CreateFixedGroupCell: TdxReportCell;
    function GetAreFooterCellsAllocated: Boolean;
    function GetAreHeaderCellsAllocated: Boolean;
    function GetAreHeaderCornerCellsAllocated: Boolean;
    function GetAreRowHeaderCellsAllocated: Boolean;
    function GetBoundsRect: TRect;
    function GetCount: Integer;
    function GetFont: TFont;
    function GetFooterBoundsRect: TRect;
    function GetFooterCellCount: Integer;
    function GetFooterCells: TdxReportCell;
    function GetHeaderBoundsRect: TRect;
    function GetHeaderCellCount: Integer;
    function GetHeaderCells: TdxReportCell;
    function GetHeaderCornerBoundsRect: TRect;
    function GetHeaderCornerCells: TdxReportCell;
    function GetImageList(Index: Integer): TCustomImageList;
    function GetImageListCount: Integer;
    function GetLookAndFeel(Index: Integer): TdxPSReportGroupLookAndFeel;
    function GetLookAndFeelCount: Integer;
    function GetOverlay(Index: Integer): TdxReportCell;
    function GetOverlayCount: Integer;
    function GetRenderer: TdxPSReportRenderer;
    function GetRowHeaderBoundsRect: TRect;
    function GetRowHeaderCellCount: Integer;
    function GetRowHeaderCells: TdxReportCell;
    procedure SetBorderColor(Value: TColor);
    procedure SetShadowColor(Value: TColor);
    procedure SetShadowDepth(Value: Integer);
    procedure SetTreeLineColor(Value: TColor);
  protected
    procedure AfterReadData(AReader: TdxPSDataReader); virtual;
    procedure AfterWriteData(AWriter: TdxPSDataWriter); virtual;
    procedure BeforeReadData(AReader: TdxPSDataReader); virtual;
    procedure BeforeWriteData(AWriter: TdxPSDataWriter); virtual;
    procedure CheckStorageCompatibility(AReader: TdxPSDataReader); virtual;

    procedure ReadCells(AReader: TdxPSDataReader);
    procedure ReadHeaderCells(AReader: TdxPSDataReader);
    procedure ReadFooterCells(AReader: TdxPSDataReader);
    procedure ReadImageLists(AReader: TdxPSDataReader);
    procedure ReadLookAndFeels(AReader: TdxPSDataReader);
    procedure ReadOverlayCells(AReader: TdxPSDataReader);
    procedure ReadProperties(AReader: TdxPSDataReader);

    procedure WriteCells(AWriter: TdxPSDataWriter);
    procedure WriteFooterCells(AWriter: TdxPSDataWriter);
    procedure WriteHeaderCells(AWriter: TdxPSDataWriter);
    procedure WriteImageLists(AWriter: TdxPSDataWriter);
    procedure WriteLookAndFeels(AWriter: TdxPSDataWriter);
    procedure WriteOverlayCells(AWriter: TdxPSDataWriter);
    procedure WriteProperties(AWriter: TdxPSDataWriter);

    function CalculateOverlaysHeight: Integer;
    function CalculateOverlaysWidth: Integer;
    function CalculateTotalHeight: Integer;
    function CalculateTotalWidth: Integer;
    procedure ConvertCoords(APixelsNumerator, APixelsDenominator: Integer); virtual;
    function GetCellTopLevelParent(AnItem: TdxReportItem): TdxReportCell; virtual;

    procedure FreeAndNilReportGroupLookAndFeels;
    procedure PrepareReportGroupsLookAndFeels(ACanvas: TdxPSReportRenderCustomCanvas);

    procedure AddImageList(AImageList: TCustomImageList);
    procedure ClearImageLists;
    procedure FreeAndNilImageLists;
    procedure GetImageLists;

    property ImageListCount: Integer read GetImageListCount;
    property OwnImageLists: Boolean read FOwnImageLists write FOwnImageLists;
    property Renderer: TdxPSReportRenderer read GetRenderer;
  public
    constructor Create(AReportLink: TBasedxReportLink); virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    procedure ClearItems;
    procedure ClearLookAndFeels;
    function CreateGroupLookAndFeel(AClass: TdxPSReportGroupLookAndFeelClass; ACheckExisting: Boolean = True): TdxPSReportGroupLookAndFeel;
    function FindGroupLookAndFeelByClass(AClass: TdxPSReportGroupLookAndFeelClass): TdxPSReportGroupLookAndFeel;
    function FindGroupLookAndFeelByData(AData: Pointer): TdxPSReportGroupLookAndFeel;
    function IndexOfImageList(AnImageList: TCustomImageList): Integer;
    function IndexOfReportGroupLookAndFeel(ALookAndFeel: TdxPSReportGroupLookAndFeel): Integer;

    procedure DoProgress(const APercentDone: Double);

    function AddOverlay: TdxReportCell;
    procedure AppendOverlays(Source: TdxReportCells; AnOffsetX: Integer = 0; AnOffsetY: Integer = 0);
    procedure AssignOverlays(Source: TdxReportCells; AnOffsetX: Integer = 0; AnOffsetY: Integer = 0);
    procedure ClearOverlays;
    procedure DeleteOverlay(AnOverlay: TdxReportCell);
    procedure FreeAndNilOverlays;
    function HasOverlays: Boolean;
    function IndexOfOverlay(AnOverlay: TdxReportCell): Integer;

    procedure AssignLookAndFeels(Source: TdxReportCells); virtual;

    function GetFontByIndex(AnIndex: Integer): TFont;
    function GetIndexByFont(AFont: TFont): Integer;

    procedure ReadData(AReader: TdxPSDataReader); virtual;
    procedure WriteData(AWriter: TdxPSDataWriter); virtual;

    property AreFooterCellsAllocated: Boolean read GetAreFooterCellsAllocated;
    property AreHeaderCellsAllocated: Boolean read GetAreHeaderCellsAllocated;
    property AreHeaderCornerCellsAllocated: Boolean read GetAreHeaderCornerCellsAllocated;
    property AreRowHeaderCellsAllocated: Boolean read GetAreRowHeaderCellsAllocated;
    property BorderColor: TColor read FBorderColor write SetBorderColor;
    property BorderWidth: Integer read FBorderWidth write FBorderWidth;
    property BoundsRect: TRect read GetBoundsRect;
    property Cells: TdxReportCell read FCells;
    property Count: Integer read GetCount;
    property ExpandButtonBorderColor: TColor read FExpandButtonBorderColor write FExpandButtonBorderColor default clBlack;
    property Font: TFont read GetFont;
    property FooterBoundsRect: TRect read GetFooterBoundsRect;
    property FooterCellCount: Integer read GetFooterCellCount;
    property FooterCells: TdxReportCell read GetFooterCells;
    property GroupBorderColor: TColor read FGroupBorderColor write FGroupBorderColor;
    property GroupCaptionColor: TColor read FGroupCaptionColor write FGroupCaptionColor;
    property GroupCaptionSeparatorColor: TColor read FGroupCaptionSeparatorColor write FGroupCaptionSeparatorColor;
    property GroupColor: TColor read FGroupColor write FGroupColor;
    property HeaderBoundsRect: TRect read GetHeaderBoundsRect;
    property HeaderCellCount: Integer read GetHeaderCellCount;
    property HeaderCells: TdxReportCell read GetHeaderCells;
    property HeaderCornerBoundsRect: TRect read GetHeaderCornerBoundsRect;
    property HeaderCornerCells: TdxReportCell read GetHeaderCornerCells;
    property ImageLists[Index: Integer]: TCustomImageList read GetImageList;
    property LookAndFeel: TdxPSReportGroupLookAndFeel read FLookAndFeel write FLookAndFeel;
    property LookAndFeelCount: Integer read GetLookAndFeelCount;
    property LookAndFeels[Index: Integer]: TdxPSReportGroupLookAndFeel read GetLookAndFeel;
    property OverlayCount: Integer read GetOverlayCount;
    property Overlays[Index: Integer]: TdxReportCell read GetOverlay;
    property ReportLink: TBasedxReportLink read FReportLink;
    property RowHeaderBoundsRect: TRect read GetRowHeaderBoundsRect;
    property RowHeaderCellCount: Integer read GetRowHeaderCellCount;
    property RowHeaderCells: TdxReportCell read GetRowHeaderCells;
    property ShadowColor: TColor read FShadowColor write SetShadowColor;
    property ShadowDepth: Integer read FShadowDepth write SetShadowDepth;
    property TreeLineColor: TColor read FTreeLineColor write SetTreeLineColor;
    property TreeLineStyle: TdxPSTreeLineStyle read FTreeLineStyle write FTreeLineStyle default tlsDot;
  end;

  { TAbstractdxReportCellData }

  TAbstractdxReportCellData = class(TdxReportVisualItem)
  strict private
    FNoClip: Boolean;
    FPreventAutoIndents: Boolean;

    function GetBreakByChars: Boolean;
    function GetEndEllipsis: Boolean;
    function GetHidePrefix: Boolean;
    function GetMultiline: Boolean;
    function GetPreventLeftTextExceed: Boolean;
    function GetPreventTopTextExceed: Boolean;
    function GetSortOrder: TdxCellSortOrder;
    function GetTextAlignX: TcxTextAlignX;
    function GetTextAlignY: TcxTextAlignY;
    procedure SetBreakByChars(Value: Boolean);
    procedure SetEndEllipsis(Value: Boolean);
    procedure SetHidePrefix(Value: Boolean);
    procedure SetMultiline(Value: Boolean);
    procedure SetPreventLeftTextExceed(Value: Boolean);
    procedure SetPreventTopTextExceed(Value: Boolean);
    procedure SetSortOrder(Value: TdxCellSortOrder);
    procedure SetTextAlignX(Value: TcxTextAlignX);
    procedure SetTextAlignY(Value: TcxTextAlignY);
    //
    function HasPreventAutoIndentsProperty(const AVersion: TdxPSVersion): Boolean;
  protected
    function CustomDraw(ACanvas: TdxPSReportRenderCustomCanvas): Boolean; virtual;
    function GetAbsoluteEffectiveBounds(ACanvas: TdxPSReportRenderCustomCanvas; AStage: TdxPSRenderStages): TRect; virtual;
    function GetDefaultDTFormat: DWORD; virtual;
    function GetEffectiveBounds(ACanvas: TdxPSReportRenderCustomCanvas; AStage: TdxPSRenderStages): TRect; virtual;
    function GetDTFormat: DWORD; virtual;
    function IsCustomDrawn: Boolean; virtual;
    function IsDrawingNeeded(ACanvas: TdxPSReportRenderCustomCanvas; AStage: TdxPSRenderStages; const ARect: TRect): Boolean; virtual;
    function IsDrawn(ACanvas: TdxPSReportRenderCustomCanvas; AStage: TdxPSRenderStages; const ARect: TRect): Boolean; virtual;

    procedure ReadData(AReader: TdxPSDataReader); override;
    procedure WriteData(AWriter: TdxPSDataWriter); override;

    property BreakByChars: Boolean read GetBreakByChars write SetBreakByChars default True;
    property EndEllipsis: Boolean read GetEndEllipsis write SetEndEllipsis default False;
    property HidePrefix: Boolean read GetHidePrefix write SetHidePrefix default False;
    property Multiline: Boolean read GetMultiline write SetMultiline default False;
    property NoClip: Boolean read FNoClip write FNoClip default False;
    property PreventAutoIndents: Boolean read FPreventAutoIndents write FPreventAutoIndents default False;
    property PreventLeftTextExceed: Boolean read GetPreventLeftTextExceed write SetPreventLeftTextExceed;
    property PreventTopTextExceed: Boolean read GetPreventTopTextExceed write SetPreventTopTextExceed;
    property SortOrder: TdxCellSortOrder read GetSortOrder write SetSortOrder default csoNone;
    property TextAlignX: TcxTextAlignX read GetTextAlignX write SetTextAlignX default taLeft;
    property TextAlignY: TcxTextAlignY read GetTextAlignY write SetTextAlignY default taCenterY;
  public
    constructor Create(AParent: TdxReportCell); override;

    procedure DrawContent(ACanvas: TdxPSReportRenderCustomCanvas; AStage: TdxPSRenderStages); virtual;
    function GetCustomDrawID: Integer; virtual;

    function MeasureContentHeight(ACanvas: TdxPSReportRenderCustomCanvas): Integer; override;
    function MeasureContentWidth(ACanvas: TdxPSReportRenderCustomCanvas): Integer; override;

    property DefaultDTFormat: DWORD read GetDefaultDTFormat;
    property DTFormat: DWORD read GetDTFormat;
  end;

  { TdxReportCellBox }

  TdxReportCellBoxClass = class of TdxReportCellBox;
  TdxReportCellBox = class(TAbstractdxReportCellData);

  { TdxReportCellText }

  TdxReportCellTextClass = class of TdxReportCellText;
  TdxReportCellText = class(TAbstractdxReportCellData)
  strict private
    FAdjustFont: Boolean;
    FIndents: TcxRect;
    FSortMarkRegionSize: Integer;

    function GetSortMarkRegionSize: Integer;
    procedure SetIndents(AValue: TcxRect);
  protected
    function GetText: string; virtual; abstract;
    procedure SetText(const Value: string); virtual; abstract;

    procedure ConvertCoords(APixelsNumerator, APixelsDenominator: Integer); override;
    function GetSortMarkBounds(ACanvas: TdxPSReportRenderCustomCanvas): TRect; virtual;
    function GetTextBounds(ACanvas: TdxPSReportRenderCustomCanvas): TRect; virtual;
    function IsSortMarkDrawn: Boolean; virtual;
    function IsTextDrawn: Boolean; virtual;

    procedure ReadData(AReader: TdxPSDataReader); override;
    procedure WriteData(AWriter: TdxPSDataWriter); override;

    property AdjustFont: Boolean read FAdjustFont write FAdjustFont;
    property SortMarkRegionSize: Integer read GetSortMarkRegionSize;
  public
    constructor Create(AParent: TdxReportCell); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    procedure DrawContent(ACanvas: TdxPSReportRenderCustomCanvas; AStage: TdxPSRenderStages); override;
    procedure DrawSortMark(ACanvas: TdxPSReportRenderCustomCanvas); virtual;
    procedure DrawText(ACanvas: TdxPSReportRenderCustomCanvas); virtual;

    function CalculateLineCount(ACanvas: TdxPSReportRenderCustomCanvas): Integer; override;
    function MeasureContentHeight(ACanvas: TdxPSReportRenderCustomCanvas): Integer; override;
    function MeasureContentWidth(ACanvas: TdxPSReportRenderCustomCanvas): Integer; override;
    function MeasureFontHeight(ACanvas: TdxPSReportRenderCustomCanvas): Integer; override;
    function MeasureTextHeight(ACanvas: TdxPSReportRenderCustomCanvas): Integer; virtual;
    function MeasureTextWidth(ACanvas: TdxPSReportRenderCustomCanvas): Integer; virtual;

    property NoClip;
    property EndEllipsis;
    property HidePrefix;
    property Indents: TcxRect read FIndents write SetIndents;
    property Multiline;
    property SortOrder;
    property Text: string read GetText write SetText;
  end;

  { TdxReportCellString }

  TdxReportCellStringClass = class of TdxReportCellString;
  TdxReportCellString = class(TdxReportCellText)
  strict private
    FText: string;
  protected
    function GetText: string; override;
    procedure SetText(const Value: string); override;
  public
    property AdjustFont;
    property PreventAutoIndents;
    property PreventLeftTextExceed;
    property PreventTopTextExceed;
    property TextAlignX;
    property TextAlignY;
  end;

  { TdxReportCellImageContainer }

  TdxReportCellImageContainer = class(TdxReportCellString)
  strict private
    function GetImageTransparent: Boolean;
    procedure SetImageTransparent(Value: Boolean);
  protected
    function GetImageAreaBounds(ACanvas: TdxPSReportRenderCustomCanvas): TRect; virtual;
    function GetImageBounds(ACanvas: TdxPSReportRenderCustomCanvas): TRect; virtual;
    procedure GetImageSizes(var AImageWidth, AImageHeight: Integer); virtual;
    function HasImage: Boolean; virtual;
    function IsImageBackgroundDrawn: Boolean; virtual;
    function IsImageDrawn: Boolean; virtual;
    function IsTextBackgroundDrawn: Boolean; virtual;
    property ImageTransparent: Boolean read GetImageTransparent write SetImageTransparent default True;
  public
    constructor Create(AParent: TdxReportCell); override;
    procedure Assign(Source: TPersistent); override;

    procedure DrawContent(ACanvas: TdxPSReportRenderCustomCanvas; AStage: TdxPSRenderStages); override;
    procedure DrawImage(ACanvas: TdxPSReportRenderCustomCanvas); virtual;
    procedure DrawImageBackground(ACanvas: TdxPSReportRenderCustomCanvas); virtual;
    procedure DrawTextBackground(ACanvas: TdxPSReportRenderCustomCanvas); virtual;
  end;

  TdxCustomReportCellCheckClass = class of TdxCustomReportCellCheck;

  TdxCustomReportCellCheck = class(TdxReportCellImageContainer)
  private
    FCheckSize: TSize;

    function GetBoldBorder: Boolean;
    function GetButtonEdgeStyle: TdxCheckButtonEdgeStyle;
    function GetChecked: Boolean;
    function GetCheckPos: TdxCellCheckPos;
    function GetEnabled: Boolean;
    function GetFlatBorder: Boolean;
    function GetState: TCheckBoxState;
    procedure SetBoldBorder(Value: Boolean);
    procedure SetButtonEdgeStyle(Value: TdxCheckButtonEdgeStyle);
    procedure SetCheckPos(Value: TdxCellCheckPos);
    procedure SetEnabled(Value: Boolean);
    procedure SetFlatBorder(Value: Boolean);
  protected
    procedure ConvertCoords(APixelsNumerator: Integer; APixelsDenominator: Integer); override;
    function GetCheckBounds(ACanvas: TdxPSReportRenderCustomCanvas): TRect; virtual;
    function GetImageAreaBounds(ACanvas: TdxPSReportRenderCustomCanvas): TRect; override;
    function GetImageBounds(ACanvas: TdxPSReportRenderCustomCanvas): TRect; override;
    procedure GetImageSizes(var AImageWidth, AImageHeight: Integer); override;
    function GetTextBounds(ACanvas: TdxPSReportRenderCustomCanvas): TRect; override;
    function HasImage: Boolean; override;
    function IsImageBackgroundDrawn: Boolean; override;
    class function IsRadio: Boolean; virtual;
    procedure SetChecked(Value: Boolean); virtual;

    property BoldBorder: Boolean read GetBoldBorder write SetBoldBorder;  // obsolete
    property FlatBorder: Boolean read GetFlatBorder write SetFlatBorder; // obsolete
    property State: TCheckBoxState read GetState;
  public
    constructor Create(AParent: TdxReportCell); override;

    procedure DrawCheck(ACanvas: TdxPSReportRenderCustomCanvas); virtual;
    procedure DrawImage(ACanvas: TdxPSReportRenderCustomCanvas); override;

    function MeasureContentHeight(ACanvas: TdxPSReportRenderCustomCanvas): Integer; override;
    function MeasureContentWidth(ACanvas: TdxPSReportRenderCustomCanvas): Integer; override;

    property ButtonEdgeStyle: TdxCheckButtonEdgeStyle read GetButtonEdgeStyle write SetButtonEdgeStyle default cbesUltraFlat;
    property Checked: Boolean read GetChecked write SetChecked default False;
    property CheckPos: TdxCellCheckPos read GetCheckPos write SetCheckPos default ccpCenter;
    property Enabled: Boolean read GetEnabled write SetEnabled default True;
  end;

  TdxReportCellCheck = class(TdxCustomReportCellCheck)
  public
    property BoldBorder;
    property FlatBorder;
    property State;
  end;

  TdxCustomReportCellRadio = class(TdxReportCellCheck)
  protected
    class function IsRadio: Boolean; override;
  end;

  TdxReportCellRadio = class(TdxCustomReportCellRadio)
  end;

  TdxCustomReportCellCheckImage = class(TdxReportCellCheck)
  private
    function GetGlyphPartialBounds: TRect;
  protected
    procedure GetImageSizes(var AImageWidth, AImageHeight: Integer); override;

    function GetGlyph: TdxSmartGlyph; virtual;
    function GetGlyphCount: Integer; virtual;
    function GetGlyphIndex: Integer; virtual;
    procedure SetGlyph(Value: TdxSmartGlyph); virtual;
    procedure SetGlyphCount(Value: Integer); virtual;

    function HasGlyph: Boolean; virtual;
    procedure ReleaseGlyph; virtual;

    procedure ReadData(AReader: TdxPSDataReader); override;
    procedure WriteData(AWriter: TdxPSDataWriter); override;

    property Glyph: TdxSmartGlyph read GetGlyph write SetGlyph;
    property GlyphCount: Integer read GetGlyphCount write SetGlyphCount;
    property GlyphIndex: Integer read GetGlyphIndex;
    property GlyphPartialBounds: TRect read GetGlyphPartialBounds;
  public
    constructor Create(AParent: TdxReportCell); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    procedure DrawCheck(ACanvas: TdxPSReportRenderCustomCanvas); override;
    procedure DrawCheckGlyph(ACanvas: TdxPSReportRenderCustomCanvas); virtual;
  end;

  TdxReportCellCheckImage = class(TdxCustomReportCellCheckImage)
  private
    FGlyph: TdxSmartGlyph;
  protected
    function GetGlyphCount: Integer; override;
    function GetGlyph: TdxSmartGlyph; override;
    procedure SetGlyph(Value: TdxSmartGlyph); override;
    procedure SetGlyphCount(Value: Integer); override;

    function HasGlyph: Boolean; override;
    procedure ReleaseGlyph; override;
  public
    property Glyph;
    property GlyphCount;
    property GlyphIndex;
    property GlyphPartialBounds;
  end;

  TdxCustomReportButtonGroupClass = class of TdxCustomReportButtonGroup;

  TdxCustomReportButtonGroup = class(TdxReportGroup)
  private
    FColumnCount: Integer;
    FInterColumnsMinSpace: Integer;
    FInterRowsMinSpace: Integer;
    FIndents: TRect;
    FItemSize: TSize;
    function GetButtonEdgeStyle: TdxCheckButtonEdgeStyle;
    function GetCheckPos: TdxCellCheckPos;
    function GetItem(Index: Integer): TdxCustomReportCellCheck;
    function GetItemColumn(Index: Integer): Integer;
    function GetItemCount: Integer;
    function GetItemRow(Index: Integer): Integer;
    function GetRowCount: Integer;
    procedure SetButtonEdgeStyle(Value: TdxCheckButtonEdgeStyle);
    procedure SetCheckPos(Value: TdxCellCheckPos);
    procedure SetColumnCount(Value: Integer);
    procedure SetInterColumnsMinSpace(Value: Integer);
    procedure SetInterRowsMinSpace(Value: Integer);
    procedure SetIndents(Value: TRect);
  protected
    FLocked: Boolean;
    procedure ConvertCoords(APixelsNumerator, APixelsDenominator: Integer); override;
    procedure SetFontIndex(Value: Integer); override;

    procedure ReadDataItems(AReader: TdxPSDataReader); override;
    procedure ReadData(AReader: TdxPSDataReader); override;
    procedure WriteData(AWriter: TdxPSDataWriter); override;

    class function GetItemClass: TdxCustomReportCellCheckClass; virtual;
    procedure InitializeItem(AnItem: TdxCustomReportCellCheck); virtual;

    property ItemSize: TSize read FItemSize;
    property Locked: Boolean read FLocked write FLocked;
  public
    constructor Create(AParent: TdxReportCell); override;
    procedure Assign(Source: TPersistent); override;

    procedure AdjustContent(ACanvas: TdxPSReportRenderCustomCanvas); override;

    function MeasureContentHeight(ACanvas: TdxPSReportRenderCustomCanvas): Integer; override;
    function MeasureContentWidth(ACanvas: TdxPSReportRenderCustomCanvas): Integer; override;

    function Add(const AText: string = ''): TdxCustomReportCellCheck;
    procedure Clear;
    procedure Delete(Index: Integer);
    function FindItem(const ACaption: string): Integer;

    property ButtonEdgeStyle: TdxCheckButtonEdgeStyle read GetButtonEdgeStyle write SetButtonEdgeStyle default cbesUltraFlat;
    property CheckPos: TdxCellCheckPos read GetCheckPos write SetCheckPos default ccpCenter;
    property ColumnCount: Integer read FColumnCount write SetColumnCount;
    property Indents: TRect read FIndents write SetIndents;
    property InterColumnsMinSpace: Integer read FInterColumnsMinSpace write SetInterColumnsMinSpace;
    property InterRowsMinSpace: Integer read FInterRowsMinSpace write SetInterRowsMinSpace;
    property ItemColumns[Index: Integer]: Integer read GetItemColumn;
    property ItemCount: Integer read GetItemCount;
    property ItemRows[Index: Integer]: Integer read GetItemRow;
    property Items[Index: Integer]: TdxCustomReportCellCheck read GetItem;
    property RowCount: Integer read GetRowCount;
  end;

  TdxReportRadioGroup = class(TdxCustomReportButtonGroup)
  private
    function GetItem(Index: Integer): TdxCustomReportCellRadio;
    function GetItemIndex: Integer;
    procedure SetItemIndex(Value: Integer);
  protected
    class function GetItemClass: TdxCustomReportCellCheckClass; override;
  public
    function Add(const AText: string = ''): TdxCustomReportCellRadio;
    property ItemIndex: Integer read GetItemIndex write SetItemIndex;
    property Items[Index: Integer]: TdxCustomReportCellRadio read GetItem;
  end;

  TdxReportCheckGroup = class(TdxCustomReportButtonGroup)
  private
    FGlyph: TdxSmartGlyph;
    FGlyphCount: Integer;
    function GetGlyph: TdxSmartGlyph;
    function GetItem(Index: Integer): TdxCustomReportCellCheckImage;
    function GetItemChecked(Index: Integer): Boolean;
    function GetItemEnabled(Index: Integer): Boolean;
    function GetItemState(Index: Integer): TCheckBoxState;
    procedure SetGlyph(Value: TdxSmartGlyph);
    procedure SetGlyphCount(Value: Integer);
    procedure SetItemChecked(Index: Integer; Value: Boolean);
    procedure SetItemEnabled(Index: Integer; Value: Boolean);
  protected
    procedure ReadData(AReader: TdxPSDataReader); override;
    procedure WriteData(AWriter: TdxPSDataWriter); override;

    class function GetItemClass: TdxCustomReportCellCheckClass; override;
    procedure InitializeItem(AnItem: TdxCustomReportCellCheck); override;
  public
    constructor Create(AParent: TdxReportCell); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    function Add(const AText: string = ''): TdxCustomReportCellCheckImage;
    function HasGlyph: Boolean;
    procedure ReleaseGlyph;

    property Glyph: TdxSmartGlyph read GetGlyph write SetGlyph;
    property GlyphCount: Integer read FGlyphCount write SetGlyphCount;
    property Items[Index: Integer]: TdxCustomReportCellCheckImage read GetItem;
    property ItemsChecked[Index: Integer]: Boolean read GetItemChecked write SetItemChecked;
    property ItemsEnabled[Index: Integer]: Boolean read GetItemEnabled write SetItemEnabled;
    property ItemsState[Index: Integer]: TCheckBoxState read GetItemState;
  end;

  { TCustomdxReportCellImageContainer }

  TCustomdxReportCellImageContainer = class(TdxReportCellImageContainer)
  strict private
    FImage: TGraphic;
    FImageIndex: Integer;
    FImageList: TCustomImageList;
    FImagePrepared: Boolean;
    FImageTransparentColor: TColor;
    FOverlayImageIndex: Integer;

    function GetImageListUsed: Boolean;
    procedure SetImage(Value: TGraphic);
  protected
    function CanPrepareImage: Boolean; virtual;
    function GetActualImageBuffering: TdxCellImageActualBuffering; virtual;
    function GetImageBuffering: TdxCellImageBuffering; virtual;
    procedure GetImageSizes(var AImageWidth, AImageHeight: Integer); override;
    function GetImageSourceDPI: Integer; virtual;
    function GetOriginalImageSize: TSize; virtual;
    function HasImage: Boolean; override;
    function PrepareImage: TcxBitmap;
    procedure PrepareImageBackground(ACanvas: TCanvas; const R: TRect);
    procedure SetImageBuffering(Value: TdxCellImageBuffering); virtual;

    function GetImageListIndex: Integer;

    procedure DoReadData(AReader: TdxPSDataReader); virtual;
    procedure DoWriteData(AWriter: TdxPSDataWriter); virtual;
    procedure ReadData(AReader: TdxPSDataReader); override;
    procedure WriteData(AWriter: TdxPSDataWriter); override;

    property ActualImageBuffering: TdxCellImageActualBuffering read GetActualImageBuffering;
    property ImageBuffering: TdxCellImageBuffering read GetImageBuffering write SetImageBuffering;
    property ImageListUsed: Boolean read GetImageListUsed;
    property ImagePrepared: Boolean read FImagePrepared write FImagePrepared;
  public
    constructor Create(AParent: TdxReportCell); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    function CreateImage(AGraphicClass: TGraphicClass): TGraphic; virtual;
    procedure DrawImage(ACanvas: TdxPSReportRenderCustomCanvas); override;

    property Image: TGraphic read FImage write SetImage;
    property ImageIndex: Integer read FImageIndex write FImageIndex;
    property ImageList: TCustomImageList read FImageList write FImageList;
    property ImageTransparent;
    property ImageTransparentColor: TColor read FImageTransparentColor write FImageTransparentColor;
    property OverlayImageIndex: Integer read FOverlayImageIndex write FOverlayImageIndex;
  end;

  { TdxReportCellImage }

  TdxReportCellImageClass = class of TdxReportCellImage;
  TdxReportCellImage = class(TCustomdxReportCellImageContainer)
  strict private
    function GetImageLayout: TdxImageLayout;
    function GetImageSize: TSize;
    function GetIsTextDrawnForCenteredImage: Boolean;
    function GetIsTextShiftedForHorizontallyCenteredImage: Boolean;
    function GetMakeSpaceForEmptyImage: Boolean;
    procedure SetImageLayout(Value: TdxImageLayout);
    procedure SetIsTextDrawnForCenteredImage(Value: Boolean);
    procedure SetIsTextShiftedForHorizontallyCenteredImage(Value: Boolean);
    procedure SetMakeSpaceForEmptyImage(Value: Boolean);
  protected
    function GetImageAreaBounds(ACanvas: TdxPSReportRenderCustomCanvas): TRect; override;
    function GetImageBounds(ACanvas: TdxPSReportRenderCustomCanvas): TRect; override;
    function GetTextBounds(ACanvas: TdxPSReportRenderCustomCanvas): TRect; override;
    function IsImageBackgroundDrawn: Boolean; override;
    function IsTextDrawn: Boolean; override;
    function IsTextBackgroundDrawn: Boolean; override;
  public
    constructor Create(AParent: TdxReportCell); override;
    procedure Assign(Source: TPersistent); override;

    function MeasureContentHeight(ACanvas: TdxPSReportRenderCustomCanvas): Integer; override;
    function MeasureContentWidth(ACanvas: TdxPSReportRenderCustomCanvas): Integer; override;

    property ImageLayout: TdxImageLayout read GetImageLayout write SetImageLayout default ilImageCenterLeft;
    property ImageTransparent;
    property IsTextDrawnForCenteredImage: Boolean read GetIsTextDrawnForCenteredImage write SetIsTextDrawnForCenteredImage default False;
    property IsTextShiftedForHorizontallyCenteredImage: Boolean read GetIsTextShiftedForHorizontallyCenteredImage write SetIsTextShiftedForHorizontallyCenteredImage default True;
    property MakeSpaceForEmptyImage: Boolean read GetMakeSpaceForEmptyImage write SetMakeSpaceForEmptyImage default True;
  end;

  { TdxReportCellGraphic }

  TdxReportCellGraphicClass = class of TdxReportCellGraphic;
  TdxReportCellGraphic = class(TCustomdxReportCellImageContainer)
  strict private
    FCenter: Boolean;
    FProportional: Boolean;
    FStretch: Boolean;

    function GetImage: TGraphic;
    function GetDrawMode: TdxGraphicDrawMode;
    function GetRealStretch: Boolean;
    procedure SetImage(Value: TGraphic);
    procedure SetCenter(Value: Boolean);
    procedure SetDrawMode(Value: TdxGraphicDrawMode);
    procedure SetProportional(Value: Boolean);
    procedure SetStretch(Value: Boolean);
  protected
    procedure CalculateDrawMode;
    function GetImageBounds(ACanvas: TdxPSReportRenderCustomCanvas): TRect; override;
    function GetImageBuffering: TdxCellImageBuffering; override;
    function GetTextBounds(ACanvas: TdxPSReportRenderCustomCanvas): TRect; override;
    procedure SetImageBuffering(Value: TdxCellImageBuffering); override;
  public
    constructor Create(AParent: TdxReportCell); override;
    function MeasureFontHeight(ACanvas: TdxPSReportRenderCustomCanvas): Integer; override;
    function MeasureContentHeight(ACanvas: TdxPSReportRenderCustomCanvas): Integer; override;
    function MeasureContentWidth(ACanvas: TdxPSReportRenderCustomCanvas): Integer; override;

    property Center: Boolean read FCenter write SetCenter;
    property DrawMode: TdxGraphicDrawMode read GetDrawMode write SetDrawMode default gdmNone;
    property Image: TGraphic read GetImage write SetImage;
    property ImageBuffering;
    property ImageTransparent;
    property Proportional: Boolean read FProportional write SetProportional;
    property RealStretch: Boolean read GetRealStretch;
    property Stretch: Boolean read FStretch write SetStretch;
  end;

  { TdxReportCellDpiAwareGraphic }

  TdxReportCellDpiAwareGraphic = class(TdxReportCellGraphic)
  strict private
    FImageSourceDPI: Integer;

    procedure SetImageSourceDPI(AValue: Integer);
  protected
    function GetImageSourceDPI: Integer; override;
    procedure ReadData(AReader: TdxPSDataReader); override;
    procedure WriteData(AWriter: TdxPSDataWriter); override;
  public
    constructor Create(AParent: TdxReportCell); override;
    //
    property ImageSourceDPI: Integer read FImageSourceDPI write SetImageSourceDPI;
  end;

  TdxReportCellExpandButtonAlignHorz = (bahLeft, bahCenter, bahRight);
  TdxReportCellExpandButtonAlignVert = (bavTop, bavCenter, bavBottom);

  TdxReportCellExpandButtonClass = class of TdxReportCellExpandButton;
  TdxReportCellExpandButton = class(TAbstractdxReportCellData)
  private
    FButtonInteriorColor: TColor;
    FButtonSize: Integer;
    function GetActualButtonSize: Integer;
    function GetButtonBorder3D: Boolean;
    function GetButtonBorder3DSoft: Boolean;
    function GetButtonBorderShadow: Boolean;
    function GetButtonExpanded: Boolean;
    function GetButtonTransparent: Boolean;
    function GetKeepOddSize: Boolean;
    function GetShowButton: Boolean;
    function GetShowButtonBorder: Boolean;
    function GetTreeLineMode: TdxPSTreeLineMode;
    procedure SetButtonBorder3D(Value: Boolean);
    procedure SetButtonBorder3DSoft(Value: Boolean);
    procedure SetButtonBorderShadow(Value: Boolean);
    procedure SetButtonExpanded(Value: Boolean);
    procedure SetButtonTransparent(Value: Boolean);
    procedure SetKeepOddSize(Value: Boolean);
    procedure SetShowButton(Value: Boolean);
    procedure SetShowButtonBorder(Value: Boolean);
    procedure SetTreeLineMode(Value: TdxPSTreeLineMode);
  protected
    procedure ConvertCoords(APixelsNumerator, APixelsDenominator: Integer); override;

    function AreTreeLinesDrawn: Boolean;
    function CalculateButtonBounds: TRect; virtual;

    function GetButtonAlignHorz: TdxReportCellExpandButtonAlignHorz; virtual;
    function GetButtonAlignVert: TdxReportCellExpandButtonAlignVert; virtual;
    function GetButtonIndents: TRect; virtual;
    procedure SetButtonAlignHorz(Value: TdxReportCellExpandButtonAlignHorz); virtual;
    procedure SetButtonAlignVert(Value: TdxReportCellExpandButtonAlignVert); virtual;

    procedure ReadData(AReader: TdxPSDataReader); override;
    procedure WriteData(AWriter: TdxPSDataWriter); override;
  public
    constructor Create(AParent: TdxReportCell); override;
    procedure Assign(Source: TPersistent); override;

    procedure DrawContent(ACanvas: TdxPSReportRenderCustomCanvas; AStage: TdxPSRenderStages); override;
    procedure DrawExpandButton(ACanvas: TdxPSReportRenderCustomCanvas); virtual;
    procedure DrawTreeLines(ACanvas: TdxPSReportRenderCustomCanvas); virtual;

    function GetButtonBounds(ACanvas: TdxPSReportRenderCustomCanvas): TRect; virtual;

    property ActualButtonSize: Integer read GetActualButtonSize;
    property ButtonAlignHorz: TdxReportCellExpandButtonAlignHorz read GetButtonAlignHorz write SetButtonAlignHorz default bahCenter;
    property ButtonAlignVert: TdxReportCellExpandButtonAlignVert read GetButtonAlignVert write SetButtonAlignVert default bavCenter;
    property ButtonBorder3D: Boolean read GetButtonBorder3D write SetButtonBorder3D default False;
    property ButtonBorder3DSoft: Boolean read GetButtonBorder3DSoft write SetButtonBorder3DSoft default False;
    property ButtonBorderShadow: Boolean read GetButtonBorderShadow write SetButtonBorderShadow default False;
    property ButtonExpanded: Boolean read GetButtonExpanded write SetButtonExpanded default False;
    property ButtonIndents: TRect read GetButtonIndents;
    property ButtonInteriorColor: TColor read FButtonInteriorColor write FButtonInteriorColor default clNone;
    property ButtonSize: Integer read FButtonSize write FButtonSize default 9;
    property ButtonTransparent: Boolean read GetButtonTransparent write SetButtonTransparent default True;
    property KeepOddSize: Boolean read GetKeepOddSize write SetKeepOddSize default True;
    property ShowButton: Boolean read GetShowButton write SetShowButton default False;
    property ShowButtonBorder: Boolean read GetShowButtonBorder write SetShowButtonBorder default True;
    property TreeLineMode: TdxPSTreeLineMode read GetTreeLineMode write SetTreeLineMode default tlmNone;
  end;

  TdxReportCellExpandButtonEx = class(TdxReportCellExpandButton)
  private
    FFormatEx: DWORD;
  protected
    function GetButtonAlignHorz: TdxReportCellExpandButtonAlignHorz; override;
    function GetButtonAlignVert: TdxReportCellExpandButtonAlignVert; override;
    function GetButtonIndents: TRect; override;
    procedure SetButtonAlignHorz(Value: TdxReportCellExpandButtonAlignHorz); override;
    procedure SetButtonAlignVert(Value: TdxReportCellExpandButtonAlignVert); override;

    procedure ReadData(AReader: TdxPSDataReader); override;
    procedure WriteData(AWriter: TdxPSDataWriter); override;

    property FormatEx: DWORD read FFormatEx write FFormatEx;
  end;

  { Explorer }

  TdxPSExplorerRefreshStage = (ersBefore, ersAfter);

  TCustomdxPSExplorerItemStateInfo = record
    Count: Integer;
    UniqueIDSize: Integer;
    // UniqueID: TBytes follows by UniqueIDSize with Length UniqueIDSize
  end;

  TCustomdxPSExplorer = class;
  TCustomdxPSExplorerItem = class;
  TdxPSExplorerFolder = class;
  TdxPSExplorerItem = class;


  IdxPSExplorerTreeContainerHost = interface
  ['{4E52E062-EDCF-4A58-8212-45EAE673F506}']
    function GetFlat: Boolean;
    function GetReportLink: TBasedxReportLink;
    function GetTreeContainerParent: TcxControl;

    procedure UpdateState;

    property Flat: Boolean read GetFlat;
    property ReportLink: TBasedxReportLink read GetReportLink;
    property TreeContainerParent: TcxControl read GetTreeContainerParent;
  end;


  TdxPSExplorerChangeNotifier = class
  private
    FExplorer: TCustomdxPSExplorer;
    procedure SetExplorer(Value: TCustomdxPSExplorer);
  protected
    procedure ExplorerRefresh(AStage: TdxPSExplorerRefreshStage); virtual; abstract;
    procedure FolderPopulated(AFolder: TdxPSExplorerFolder); virtual; abstract;
    procedure ItemAdded(AnItem: TCustomdxPSExplorerItem); virtual; abstract;
    procedure ItemDataLoaded(AnItem: TdxPSExplorerItem); virtual; abstract;
    procedure ItemDataUnloaded(AnItem: TdxPSExplorerItem); virtual; abstract;
    procedure ItemDeleted(AnItem: TCustomdxPSExplorerItem); virtual; abstract;
    procedure ItemParentChanged(AnItem: TCustomdxPSExplorerItem); virtual; abstract;
    procedure ItemPropertiesChanged(AnItem: TCustomdxPSExplorerItem); virtual; abstract;
    procedure ItemRenamed(AnItem: TCustomdxPSExplorerItem); virtual; abstract;
  public
    constructor Create(AnExplorer: TCustomdxPSExplorer);
    destructor Destroy; override;

    property Explorer: TCustomdxPSExplorer read FExplorer write SetExplorer;
  end;

  TdxPSExplorerChangeNotifierAdapter = class(TdxPSExplorerChangeNotifier)
  protected
    procedure ExplorerRefresh(AStage: TdxPSExplorerRefreshStage); override;
    procedure FolderPopulated(AFolder: TdxPSExplorerFolder); override;
    procedure ItemAdded(AnItem: TCustomdxPSExplorerItem); override;
    procedure ItemDataLoaded(AnItem: TdxPSExplorerItem); override;
    procedure ItemDataUnloaded(AnItem: TdxPSExplorerItem); override;
    procedure ItemDeleted(AnItem: TCustomdxPSExplorerItem); override;
    procedure ItemParentChanged(AnItem: TCustomdxPSExplorerItem); override;
    procedure ItemPropertiesChanged(AnItem: TCustomdxPSExplorerItem); override;
    procedure ItemRenamed(AnItem: TCustomdxPSExplorerItem); override;
  end;

  TdxPSExplorerTreeChangeNotifier = class(TdxPSExplorerChangeNotifierAdapter)
  private
    FTreeContainer: TCustomdxPSExplorerTreeContainer;
  protected
    procedure ExplorerRefresh(AStage: TdxPSExplorerRefreshStage); override;
    procedure FolderPopulated(AFolder: TdxPSExplorerFolder); override;
    procedure ItemAdded(AnItem: TCustomdxPSExplorerItem); override;
    procedure ItemDataLoaded(AnItem: TdxPSExplorerItem); override;
    procedure ItemDataUnloaded(AnItem: TdxPSExplorerItem); override;
    procedure ItemDeleted(AnItem: TCustomdxPSExplorerItem); override;
    procedure ItemParentChanged(AnItem: TCustomdxPSExplorerItem); override;
    procedure ItemPropertiesChanged(AnItem: TCustomdxPSExplorerItem); override;
    procedure ItemRenamed(AnItem: TCustomdxPSExplorerItem); override;
  public
    constructor Create(ATreeContainer: TCustomdxPSExplorerTreeContainer; ARegister: Boolean = True);
    property TreeContainer: TCustomdxPSExplorerTreeContainer read FTreeContainer;
  end;

  TCustomdxPSExplorerTreeContainerClass = class of TCustomdxPSExplorerTreeContainer;

  TCustomdxPSExplorerTreeContainer = class
  private
    FControl: TWinControl;
    FChangeNotifier: TdxPSExplorerTreeChangeNotifier;
    FExplorer: TCustomdxPSExplorer;
    FHost: IdxPSExplorerTreeContainerHost;
  protected
    { Next virtual (abstract) methods must be overriden in descendants }
    procedure AddItem(AParent: TdxPSExplorerFolder; AnItem: TCustomdxPSExplorerItem); virtual; abstract;
    procedure Clear; virtual; abstract;
    procedure DeleteItem(AnItem: TCustomdxPSExplorerItem); virtual;
    procedure InvalidateItem(AnItem: TCustomdxPSExplorerItem); virtual;
    procedure MoveItem(AnItem: TCustomdxPSExplorerItem); virtual; abstract;
    procedure RenameItem(AnItem: TCustomdxPSExplorerItem); virtual; abstract;

    function GetCreationParent: TdxPSExplorerFolder; virtual; abstract;
    function GetFocusedItem: TCustomdxPSExplorerItem; virtual; abstract;
    function GetIsEditing: Boolean; virtual; abstract;
    function GetIsFolderSelected: Boolean; virtual; abstract;
    function GetIsItemSelected: Boolean; virtual; abstract;
    function GetIsRootSelected: Boolean; virtual; abstract;
    function GetSelectedFolder: TdxPSExplorerFolder; virtual; abstract;
    function GetSelectedItem: TCustomdxPSExplorerItem; virtual; abstract;
    function GetSelectedItemText: string; virtual; abstract;
    procedure SetFocusedItem(Value: TCustomdxPSExplorerItem); virtual; abstract;
    procedure SetSelectedItem(Value: TCustomdxPSExplorerItem); virtual; abstract;
    procedure SetSelectedItemText(const Value: string); virtual; abstract;

    procedure RestoreState; virtual;
    procedure SaveState; virtual;

    procedure CreateTreeContainer;
    procedure InitializeTreeContainer; virtual;
    procedure ProcessKeyDown(var Key: Word; Shift: TShiftState); virtual;
    procedure ProcessKeyPress(var Key: Char); virtual;

    property ChangeNotifier: TdxPSExplorerTreeChangeNotifier read FChangeNotifier;
    property Host: IdxPSExplorerTreeContainerHost read FHost;
  public
    constructor Create(AnExplorer: TCustomdxPSExplorer; AHost: IdxPSExplorerTreeContainerHost); virtual;
    destructor Destroy; override;

    class function ControlClass: TWinControlClass; virtual;
    class procedure Register;
    class procedure Unregister;

    procedure BeginUpdate; virtual;
    procedure EndUpdate; virtual;

    { Follow virtual (abstract) methods must be overriden in descendants }
    function BeginEdit(AnImmediate: Boolean = True): Boolean; virtual; abstract;
    procedure EndEdit(ACancel: Boolean); virtual; abstract;

    procedure CollapseItem(AnItem: TCustomdxPSExplorerItem; ARecursive: Boolean = False); virtual; abstract;
    procedure ExpandItem(AnItem: TCustomdxPSExplorerItem; ARecursive: Boolean = False); virtual; abstract;
    procedure ItemDataLoaded(AnItem: TCustomdxPSExplorerItem); virtual; abstract;
    procedure ItemDataUnloaded(AnItem: TCustomdxPSExplorerItem); virtual; abstract;
    procedure MakeItemVisible(AnItem: TCustomdxPSExplorerItem); virtual; abstract;

    function CanCreateFolder: Boolean; virtual;
    function CanCreateItem: Boolean; virtual;
    function CanDeleteSelection: Boolean; virtual;
    function CanLoadSelectedItemData: Boolean; virtual;
    function CanRefresh: Boolean; virtual;
    function CanRenameSelectedItem: Boolean; virtual;
    function CanShowPropertySheetsForSelectedItem: Boolean; virtual;
    function CanUnloadItemData: Boolean; virtual;
    function CreateItem: TdxPSExplorerItem; virtual;
    procedure DeleteSelection(AShowMessage: Boolean = True); virtual;
    function IsSelectedItemCurrentlyLoaded: Boolean;
    procedure LoadSelectedItemData; virtual;
    procedure RenameSelectedItem; virtual;
    function ShowSelectedItemPropertySheets: Boolean;
    procedure UnloadItemData; virtual;

    { Follow abstract methods must be overriden in descendants }
    function GetDropTarget(X, Y: Integer): TdxPSExplorerFolder; virtual; abstract;
    function GetItemAt(X, Y: Integer): TCustomdxPSExplorerItem; virtual; abstract;

    function CanFocus: Boolean; virtual;
    procedure SetFocus; virtual;

    procedure RefreshSorting(ANode: TObject); overload; virtual;
    procedure RefreshSorting(AFolder: TdxPSExplorerFolder); overload; virtual;

    property Control: TWinControl read FControl;
    property CreationParent: TdxPSExplorerFolder read GetCreationParent;
    property Explorer: TCustomdxPSExplorer read FExplorer;
    property FocusedItem: TCustomdxPSExplorerItem read GetFocusedItem write SetFocusedItem;
    property IsEditing: Boolean read GetIsEditing;
    property IsFolderSelected: Boolean read GetIsFolderSelected;
    property IsItemSelected: Boolean read GetIsItemSelected;
    property IsRootSelected: Boolean read GetIsRootSelected;
    property SelectedFolder: TdxPSExplorerFolder read GetSelectedFolder;
    property SelectedItem: TCustomdxPSExplorerItem read GetSelectedItem write SetSelectedItem;
    property SelectedItemText: string read GetSelectedItemText write SetSelectedItemText;
  end;

  TdxPSExplorerTreeBuilderClass = class of TdxPSExplorerTreeBuilder;

  TdxPSExplorerTreeBuilder = class
  protected
    class procedure BuildTree(AnExplorer: TCustomdxPSExplorer;
      ATreeContainer: TCustomdxPSExplorerTreeContainer); virtual;
    class procedure CreateFolderNode(ATreeContainer: TCustomdxPSExplorerTreeContainer;
      AParent, AFolder: TdxPSExplorerFolder); virtual;
    class procedure CreateItemNode(ATreeContainer: TCustomdxPSExplorerTreeContainer;
      AParent: TdxPSExplorerFolder; AnItem: TCustomdxPSExplorerItem); virtual;
    class procedure PopulateTreeFolder(ATreeContainer: TCustomdxPSExplorerTreeContainer;
      AFolder: TdxPSExplorerFolder); virtual;
  public
    class procedure Register;
    class procedure Unregister;
  end;

  TdxPSStreamMode = (smRead, smWrite, smReadWrite);

  TCustomdxPSExplorerItemPropertySheetsClass = class of TCustomdxPSExplorerItemPropertySheets;

  TCustomdxPSExplorerItemPropertySheets = class(TCustomdxPSForm)
  private
    FExplorerItem: TCustomdxPSExplorerItem;
    class function FormClass: TCustomdxPSExplorerItemPropertySheetsClass;
  protected
    procedure Done; virtual;
    procedure Initialize; virtual;
  public
    constructor CreateEx(AnExplorerItem: TCustomdxPSExplorerItem); virtual;

    class function Execute(AnExplorerItem: TCustomdxPSExplorerItem): Boolean;
    function ExplorerItem: TCustomdxPSExplorerItem; overload; virtual;
  end;

  TCustomdxPSExplorerItemComparator = class
  public
    class function CompareItems(AnItem1, AnItem2: Pointer): Integer;
  end;

  TCustomdxPSExplorerItemHelper = class
  public
    class function GetHasChildren(AFolder: TdxPSExplorerFolder): Boolean; virtual;
    class function GetImageIndex(AnItem: TCustomdxPSExplorerItem): Integer; virtual;
    class function GetSelectedIndex(AnItem: TCustomdxPSExplorerItem): Integer; virtual;
    class procedure SetHasChildren(AFolder: TdxPSExplorerFolder; Value: Boolean); virtual;
  end;

  TCustomdxPSExplorerItemClass = class of TCustomdxPSExplorerItem;

  TCustomdxPSExplorerItem = class
  private
    FExplorer: TCustomdxPSExplorer;
    FName: string;
    FParent: TdxPSExplorerFolder;
    FWindowHandle: HWND;
    procedure SetParent(Value: TdxPSExplorerFolder);
  protected
    function CompareTo(AnItem: TCustomdxPSExplorerItem): Integer; virtual;

    function DoDelete: Boolean; virtual;
    function DoMove(AParent: TdxPSExplorerFolder): Boolean; virtual;
    function DoRename(var ANewName: string): Boolean; virtual;

    function GetDisplayName: string; virtual;
    function GetImageIndex: Integer; virtual;
    function GetInfoTip: string; virtual;
    function GetNewName(AReportLink: TBasedxReportLink): string; virtual;
    function GetSelectedIndex: Integer; virtual;

    procedure InternalDelete; virtual;
    procedure InternalMove(AParent: TdxPSExplorerFolder); virtual;
    procedure InternalRename(const AName: string); virtual;
    procedure SetName(const Value: string); virtual;

    function GetItemStateInfo: TCustomdxPSExplorerItemStateInfo; virtual;
    procedure WriteState(AStream: TStream); virtual;

    procedure WndProc(var Message: TMessage); virtual;
  public
    constructor Create(AnExplorer: TCustomdxPSExplorer; AParent: TdxPSExplorerFolder); virtual;
    destructor Destroy; override;

    function Explorer: TCustomdxPSExplorer; overload; virtual;

    function CanAccept(AnItem: TCustomdxPSExplorerItem): Boolean; virtual;
    function CanDelete: Boolean; virtual;
    function CanMove: Boolean; virtual;
    function CanMoveTo(AParent: TCustomdxPSExplorerItem): Boolean; overload; virtual;
    function CanRename: Boolean; virtual;
    function CanRenameTo(const AName: string): Boolean; virtual;
    procedure Delete; virtual;
    function GetUniqueID(out AnUniqueID: TBytes): Integer; virtual; // returns Length of needed memory
    function HasAsParent(AnItem: TCustomdxPSExplorerItem): Boolean;
    function IsNameChanged(const ANewName: string): Boolean; virtual;

    class function HasPropertySheets: Boolean;
    class function PropertySheetsClass: TCustomdxPSExplorerItemPropertySheetsClass; virtual;
    function ShowPropertySheets: Boolean;

    function CannotRenameMessageText(const AOldName, ANewName: string): string; virtual;
    function DeleteMessageText: string; virtual;
    function OverwriteMessageText(Dest: TCustomdxPSExplorerItem): string; virtual;

    property DisplayName: string read GetDisplayName;
    property InfoTip: string read GetInfoTip;
    property Name: string read FName write SetName;
    property Parent: TdxPSExplorerFolder read FParent write SetParent;
  end;

  TdxPSExplorerFolderHelper = class(TCustomdxPSExplorerItemHelper)
  public
    class function GetHasChildren(AFolder: TdxPSExplorerFolder): Boolean; override;
    class procedure SetHasChildren(AFolder: TdxPSExplorerFolder; Value: Boolean); override;
  end;

  TdxPSExplorerFolderClass = class of TdxPSExplorerFolder;

  TdxPSExplorerFolder = class(TCustomdxPSExplorerItem)
  private
    FFolders: TList;
    FHasChildren: Boolean;
    FItems: TList;
    function GetFolder(Index: Integer): TdxPSExplorerFolder;
    function GetFolderCount: Integer;
    function GetHasChildren: Boolean;
    function GetIsRoot: Boolean;
    function GetItem(Index: Integer): TdxPSExplorerItem;
    function GetItemCount: Integer;
    function GetItemList(AnItem: TCustomdxPSExplorerItem): TList; overload;
    function GetItemList(AnItemClass: TCustomdxPSExplorerItemClass): TList; overload;
    procedure SetHasChildren(Value: Boolean);
  protected
    function CompareTo(AnItem: TCustomdxPSExplorerItem): Integer; override;
    function GetImageIndex: Integer; override;
    function GetNewName(AReportLink: TBasedxReportLink): string; override;
    function GetSelectedIndex: Integer; override;

    function GetItemStateInfo: TCustomdxPSExplorerItemStateInfo; override;
    procedure WriteState(AStream: TStream); override;

    procedure LoadData; virtual;

    procedure Add(AnItem: TCustomdxPSExplorerItem);
    procedure Remove(AnItem: TCustomdxPSExplorerItem);

    procedure FreeAndNilFolders;
    procedure FreeAndNilItems;

    property HasChildren: Boolean read GetHasChildren write SetHasChildren;
  public
    constructor Create(AnExplorer: TCustomdxPSExplorer; AParent: TdxPSExplorerFolder); override;
    destructor Destroy; override;

    function CanAccept(AnItem: TCustomdxPSExplorerItem): Boolean; override;
    function CanRenameTo(const AName: string): Boolean; override;
    function CreateFolder: TdxPSExplorerFolder; virtual;
    function CreateItem(AReportLink: TBasedxReportLink): TdxPSExplorerItem; virtual;
    procedure Populate;

    procedure Delete; override;
    procedure DeleteFolders; virtual;
    procedure DeleteItems; virtual;

    function HasFolders: Boolean; virtual;
    function HasItems: Boolean; virtual;
    function HasLoadedItem: Boolean; virtual;

    function FolderByName(const AName: string): TdxPSExplorerFolder; virtual;
    function ItemByName(const AName: string): TdxPSExplorerItem; virtual;

    function CannotRenameMessageText(const AOldName, ANewName: string): string; override;
    function DeleteMessageText: string; override;
    function OverwriteMessageText(Dest: TCustomdxPSExplorerItem): string; override;

    property FolderCount: Integer read GetFolderCount;
    property Folders[Index: Integer]: TdxPSExplorerFolder read GetFolder; default;
    property IsRoot: Boolean read GetIsRoot;
    property ItemCount: Integer read GetItemCount;
    property Items[Index: Integer]: TdxPSExplorerItem read GetItem;
  end;

  TdxPSExplorerItemClass = class of TdxPSExplorerItem;

  TdxPSExplorerItem = class(TCustomdxPSExplorerItem)
  private
    FHasInvalidData: Boolean;
    FReportDocument: TdxPSReportDocument;
    function GetIsCurrentlyLoaded: Boolean;
    procedure SetHasInvalidData(Value: Boolean);
  protected
    function CompareTo(AnItem: TCustomdxPSExplorerItem): Integer; override;
    function DoDelete: Boolean; override;
    function GetFormCaption: string; virtual;
    function GetImageIndex: Integer; override;
    function GetInfoTip: string; override;
    function GetNewName(AReportLink: TBasedxReportLink): string; override;
    function GetSelectedIndex: Integer; override;
    procedure InternalDelete; override;

    procedure DocumentChanged(Sender: TObject); virtual;
    procedure SaveDocument; virtual;

    procedure SetReportData(AReportLink: TBasedxReportLink); virtual;

    property FormCaption: string read GetFormCaption;
  public
    constructor Create(AnExplorer: TCustomdxPSExplorer; AParent: TdxPSExplorerFolder); override;
    destructor Destroy; override;

    function CanLoadData: Boolean; virtual;
    function CanRenameTo(const AName: string): Boolean; override;

    function CannotRenameMessageText(const AOldName, ANewName: string): string; override;
    function DataLoadErrorText: string; virtual;
    function DeleteMessageText: string; override;
    function OverwriteMessageText(Dest: TCustomdxPSExplorerItem): string; override;

    function CreateDataStream(AMode: TdxPSStreamMode): TStream; virtual;
    procedure RetrieveReportData(AReportLink: TBasedxReportLink); virtual;

    function IsLoading: Boolean; virtual;
    procedure Load(AReportLink: TBasedxReportLink);
    procedure Unload;

    class function PropertySheetsClass: TCustomdxPSExplorerItemPropertySheetsClass; override;

    property HasInvalidData: Boolean read FHasInvalidData write SetHasInvalidData;
    property IsCurrentlyLoaded: Boolean read GetIsCurrentlyLoaded;
    property ReportDocument: TdxPSReportDocument read FReportDocument;
  end;

  TCustomdxPSExplorerContextCommandClass = class of TCustomdxPSExplorerContextCommand;

  { TCustomdxPSExplorerContextCommand }

  TCustomdxPSExplorerContextCommand = class(TPersistent)
  private
    FBitmap: TcxBitmap;
    FCaption: string;
    FData: Pointer;
    FExplorer: TCustomdxPSExplorer;
    FHint: string;
    FShortCut: TShortCut;
    procedure SetBitmap(Value: TcxBitmap);
  public
    constructor Create(AnExplorer: TCustomdxPSExplorer); virtual;
    destructor Destroy; override;

    function Enabled: Boolean; virtual;
    function Explorer: TCustomdxPSExplorer; overload; virtual;
    procedure Execute; virtual;

    property Bitmap: TcxBitmap read FBitmap write SetBitmap;
    property Caption: string read FCaption write FCaption;
    property Data: Pointer read FData Write FData;
    property Hint: string read FHint write FHint;
    property ShortCut: TShortCut read FShortCut write FShortCut;
  end;

  { TdxPSExplorerContextCommandSeparator }

  TdxPSExplorerContextCommandSeparator = class(TCustomdxPSExplorerContextCommand)
  public
    constructor Create(AnExplorer: TCustomdxPSExplorer); override;
    function Enabled: Boolean; override;
  end;

  { TdxPSExplorerRefreshContextCommand }

  TdxPSExplorerRefreshContextCommand = class(TCustomdxPSExplorerContextCommand)
  public
    constructor Create(AnExplorer: TCustomdxPSExplorer); override;
    procedure Execute; override;
  end;

  TdxPSExplorerState = (esItemCreating, esFolderCreating, esLoading, esRefreshing);
  TdxPSExplorerStates = set of TdxPSExplorerState;

  TdxPSExplorerItemDataLoadErrorEvent = procedure(Sender: TCustomdxPSExplorer;
    AnItem: TdxPSExplorerItem; var AShowErrorMessage: Boolean; var AText: string) of object;

  TdxPSExplorerClass = class of TCustomdxPSExplorer;

  { TCustomdxPSExplorer }

  TCustomdxPSExplorer = class(TComponent,
    IdxPSExplorerContextCommands, IdxPSExplorerContextCommands2)
  private
    FActiveFolder: TdxPSExplorerFolder;
    FCommands: TList;
    FFilterLinkClass: TComponentClass;
    FFilterLinkClassName: string;
    FLoadedItem: TdxPSExplorerItem;
    FLoadingCounter: Integer;
    FLockCounter: Integer;
    FNotifiers: TList;
    FRefreshCounter: Integer;
    FRoot: TdxPSExplorerFolder;
    FState: TdxPSExplorerStates;
    FStateStream: TStream;
    FOnItemDataLoadError: TdxPSExplorerItemDataLoadErrorEvent;
    function GetCommand(Index: Integer): TCustomdxPSExplorerContextCommand;
    function GetCommandCount: Integer;
    function GetNotifier(Index: Integer): TdxPSExplorerChangeNotifier;
    function GetNotifierCount: Integer;
    function GetRoot: TdxPSExplorerFolder;
    procedure SetFilterLink(const Value: string);
  protected
    { IdxPSExplorerContextCommands }
    procedure BuildCommandSet(ABuilder: IdxPSExplorerContextCommandBuilder); virtual;
    { IdxPSExplorerContextCommands2 }
    procedure FinalizeCommand(ACommand: TCustomdxPSExplorerContextCommand); virtual;
    procedure InitializeCommand(ACommand: TCustomdxPSExplorerContextCommand); virtual;

    function AddCommand(ACommandClass: TCustomdxPSExplorerContextCommandClass): TCustomdxPSExplorerContextCommand;
    function AddCommandSeparator: TdxPSExplorerContextCommandSeparator;
    procedure ClearCommands;
    function CreateCommand(ACommandClass: TCustomdxPSExplorerContextCommandClass): TCustomdxPSExplorerContextCommand;
    function CreateCommandSeparator: TdxPSExplorerContextCommandSeparator;
    function FindCommand(ACommandClass: TCustomdxPSExplorerContextCommandClass): TCustomdxPSExplorerContextCommand;
    procedure FreeAndNilCommands;

    class function AcceptItemNameChar(AnItem: TCustomdxPSExplorerItem; Ch: Char): Boolean; virtual;
    function CreateItemDataStream(AnItem: TdxPSExplorerItem; AMode: TdxPSStreamMode): TStream; virtual; abstract;
    class function GetFolderClass: TdxPSExplorerFolderClass; virtual;
    class function GetItemClass: TdxPSExplorerItemClass; virtual;
    class function GetRootFolderClass: TdxPSExplorerFolderClass; virtual;
    function GetRootDisplayName: string; virtual;
    procedure LoadData(AFolder: TdxPSExplorerFolder);

    function CanDelete(AnItem: TCustomdxPSExplorerItem): Boolean; virtual;
    function CanMove(AnItem: TCustomdxPSExplorerItem): Boolean; virtual;
    function CanMoveTo(AnItem, AParent: TCustomdxPSExplorerItem): Boolean; virtual;
    function CanRename(AnItem: TCustomdxPSExplorerItem): Boolean; virtual;
    function CanRenameTo(AnItem: TCustomdxPSExplorerItem; const AName: string): Boolean; virtual;
    procedure Delete(AnItem: TCustomdxPSExplorerItem); virtual;
    procedure MoveTo(AnItem: TCustomdxPSExplorerItem; AParent: TdxPSExplorerFolder); virtual;
    procedure PopulateFolder(AFolder: TdxPSExplorerFolder); virtual;
    procedure RenameTo(AnItem: TCustomdxPSExplorerItem; AName: string); virtual;

    procedure AfterRefresh;
    procedure BeforeRefresh;
    procedure DoRefresh; virtual;
    procedure RootNeeded;

    procedure DoItemDataLoadError(AnItem: TdxPSExplorerItem); dynamic;
    procedure DoLoadData(AFolder: TdxPSExplorerFolder); virtual; abstract;
    procedure InternalSetLoadedItem(Value: TdxPSExplorerItem);

    procedure PopulateTreeFolder(ATreeContainer: TCustomdxPSExplorerTreeContainer; AFolder: TdxPSExplorerFolder);

    procedure LoadState;
    procedure SaveState;

    procedure NotifyFolderPopulated(AFolder: TdxPSExplorerFolder);
    procedure NotifyItemAdded(AnItem: TCustomdxPSExplorerItem);
    procedure NotifyItemDataLoaded(AnItem: TdxPSExplorerItem);
    procedure NotifyItemDataUnloaded(AnItem: TdxPSExplorerItem);
    procedure NotifyItemDeleted(AnItem: TCustomdxPSExplorerItem);
    procedure NotifyItemParentChanged(AnItem: TCustomdxPSExplorerItem);
    procedure NotifyItemPropertiesChanged(AnItem: TCustomdxPSExplorerItem);
    procedure NotifyItemRenamed(AnItem: TCustomdxPSExplorerItem);
    procedure NotifyRefresh(AStage: TdxPSExplorerRefreshStage);

    function AreNotificationsLocked: Boolean;
    function IndexOfNotifier(ANotifier: TdxPSExplorerChangeNotifier): Integer;
    procedure LockNotifications;
    procedure ReleaseAndNilNotifiers;
    procedure UnlockNotifications;

    procedure BeginLoading;
    procedure EndLoading;
    function IsLoading: Boolean;

    property CommandCount: Integer read GetCommandCount;
    property Commands[Index: Integer]: TCustomdxPSExplorerContextCommand read GetCommand;
    property NotifierCount: Integer read GetNotifierCount;
    property Notifiers[Index: Integer]: TdxPSExplorerChangeNotifier read GetNotifier;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function LoadedItem: TdxPSExplorerItem; overload; virtual;

    function CanCreateFolder: Boolean; virtual;
    function CanCreateItem: Boolean; virtual;
    function CreateNewFolder(AParent: TdxPSExplorerFolder): TdxPSExplorerFolder; virtual;
    function CreateNewItem(AParent: TdxPSExplorerFolder; AReportLink: TBasedxReportLink): TdxPSExplorerItem; virtual;

    procedure BuildTree(ATreeContainer: TCustomdxPSExplorerTreeContainer);
    function CreateTree(const AHost: IdxPSExplorerTreeContainerHost): TCustomdxPSExplorerTreeContainer;

    function FindCustomItemByUniqueID(const AnUniqueID: TBytes): TCustomdxPSExplorerItem; virtual;
    procedure LoadItemData(AnItem: TdxPSExplorerItem; AReportLink: TBasedxReportLink); overload;
    procedure UnloadItemData(AnItem: TdxPSExplorerItem); overload;
    procedure Refresh; virtual;

    procedure RegisterNotifier(ANotifier: TdxPSExplorerChangeNotifier);
    procedure UnregisterNotifier(ANotifier: TdxPSExplorerChangeNotifier);

    property ActiveFolder: TdxPSExplorerFolder read FActiveFolder write FActiveFolder;
    property FilterLinkClass: TComponentClass read FFilterLinkClass write FFilterLinkClass;
    property FilterLinkClassName: string read FFilterLinkClassName write FFilterLinkClassName;
    property Root: TdxPSExplorerFolder read GetRoot;
    property State: TdxPSExplorerStates read FState;
  published
    property FilterLink: string read FFilterLinkClassName write SetFilterLink;
    property OnItemDataLoadError: TdxPSExplorerItemDataLoadErrorEvent read FOnItemDataLoadError write FOnItemDataLoadError;
  end;

  { TdxReportNote }

  TdxReportNote = class(TPersistent)
  private
    FAdjustOnReportScale: Boolean;
    FColor: TColor;
    FDefaultFont: TFont;
    FFont: TFont;
    FHasChanged: Boolean;
    FReportLink: TBasedxReportLink;
    FText: string;
    FTextAlignX: TcxTextAlignX;
    FTextAlignY: TcxTextAlignY;
    FTransparent: Boolean;
    FUpdateCount: Integer;
    function IsFontStored: Boolean;
    procedure SetAdjustOnReportScale(Value: Boolean);
    procedure SetFont(Value: TFont);
    procedure SetText(const Value: string);
    procedure SetTextAlignX(Value: TcxTextAlignX);
    procedure SetTextAlignY(Value: TcxTextAlignY);
  protected
    procedure CalculateRenderInfos;
    procedure Changed; virtual;
    procedure DoRestoreDefaults; virtual;
    procedure FontChanged(Sender: TObject);
    procedure InitializeDefaultFont(AFont: TFont); virtual;
    //
    procedure DoCustomDraw(ACanvas: TCanvas; R: TRect;
      var ATextAlignX: TcxTextAlignX; var ATextAlignY: TcxTextAlignY;
      var AColor: TColor; AFont: TFont; var ADone: Boolean); virtual; abstract;
    function IsCustomDrawn: Boolean; virtual; abstract;
    procedure Draw(ARenderer: TdxPSReportRenderer; const R: TRect); virtual;
    //
    procedure ReadData(AReader: TdxPSDataReader); virtual;
    procedure ReadMode(AReader: TdxPSDataReader); virtual;
    procedure WriteData(AWriter: TdxPSDataWriter); virtual;
    procedure WriteMode(AWriter: TdxPSDataWriter); virtual;
  public
    constructor Create(AReportLink: TBasedxReportLink); virtual;
    destructor Destroy; override;
    function DefaultFont: TFont; virtual;
    procedure Assign(Source: TPersistent); override;
    procedure BeginUpdate;
    procedure CancelUpdate;
    procedure EndUpdate;
    procedure RestoreDefaults(ARefresh: Boolean = True);
    //
    property AdjustOnReportScale: Boolean read FAdjustOnReportScale write SetAdjustOnReportScale default False;
    property Color: TColor read FColor write FColor default clWhite;
    property Font: TFont read FFont write SetFont stored IsFontStored;
    property ReportLink: TBasedxReportLink read FReportLink;
    property Text: string read FText write SetText;
    property TextAlignX: TcxTextAlignX read FTextAlignX write SetTextAlignX default taCenterX;
    property TextAlignY: TcxTextAlignY read FTextAlignY write SetTextAlignY default taCenterY;
    property Transparent: Boolean read FTransparent write FTransparent default True;
  end;

  { TdxReportTitle }

  TdxReportTitleMode = (tmNone, tmOnFirstPage, tmOnEveryTopPage);

  TdxReportTitle = class(TdxReportNote)
  private
    FMode: TdxReportTitleMode;
    procedure SetMode(Value: TdxReportTitleMode);
  protected
    procedure DoRestoreDefaults; override;
    procedure ReadMode(AReader: TdxPSDataReader); override;
    procedure WriteMode(AWriter: TdxPSDataWriter); override;
    //
    procedure DoCustomDraw(ACanvas: TCanvas; R: TRect;
      var ATextAlignX: TcxTextAlignX; var ATextAlignY: TcxTextAlignY;
      var AColor: TColor; AFont: TFont; var ADone: Boolean); override;
    function IsCustomDrawn: Boolean; override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property AdjustOnReportScale;
    property Color;
    property Font;
    property Mode: TdxReportTitleMode read FMode write SetMode default tmOnEveryTopPage;
    property Text;
    property TextAlignX;
    property TextAlignY;
    property Transparent;
  end;

  { TdxReportFootnotes }

  TdxReportFootnoteMode = (fnmNone, fnmOnLastPage, fnmOnEveryBottomPage);

  TdxReportFootnotes = class(TdxReportNote)
  private
    FMode: TdxReportFootnoteMode;
    procedure SetMode(Value: TdxReportFootnoteMode);
  protected
    procedure DoRestoreDefaults; override;
    procedure ReadMode(AReader: TdxPSDataReader); override;
    procedure WriteMode(AWriter: TdxPSDataWriter); override;
    //
    procedure DoCustomDraw(ACanvas: TCanvas; R: TRect;
      var ATextAlignX: TcxTextAlignX; var ATextAlignY: TcxTextAlignY;
      var AColor: TColor; AFont: TFont; var ADone: Boolean); override;
    function IsCustomDrawn: Boolean; override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property AdjustOnReportScale;
    property Color;
    property Font;
    property Mode: TdxReportFootnoteMode read FMode write SetMode default fnmOnEveryBottomPage;
    property Text;
    property TextAlignX;
    property TextAlignY;
    property Transparent;
  end;

  { TdxPSReportDocument }

  TdxPSReportDocumentClass = class of TdxPSReportDocument;

  TdxPSReportDocument = class(TPersistent)
  private
    FCaption: string;
    FCreationDate: TDateTime;
    FCreator: string;
    FDescription: string;
    FIsCaptionAssigned: Boolean;
    FIsCreatorAssigned: Boolean;
    FIsDescriptionAssigned: Boolean;
    FPreview: TMetafile;
    FReportLink: TBasedxReportLink;
    FUpdateCount: Integer;
    FOnChanged: TNotifyEvent;
    function GetCaption: string;
    function GetCreator: string;
    function GetDescription: string;
    function IsCaptionStored: Boolean;
    function IsCreatorStored: Boolean;
    function IsDesciptionStored: Boolean;
    procedure SetCaption(const Value: string);
    procedure SetCreationDate(const Value: TDateTime);
    procedure SetCreator(const Value: string);
    procedure SetDescription(const Value: string);

    procedure ReadIsCaptionAssigned(Reader: TReader);
    procedure ReadIsCreatorAssigned(Reader: TReader);
    procedure ReadIsDescriptionAssigned(Reader: TReader);
    procedure WriteIsCaptionAssigned(Writer: TWriter);
    procedure WriteIsCreatorAssigned(Writer: TWriter);
    procedure WriteIsDescriptionAssigned(Writer: TWriter);
  protected
    procedure DefineProperties(Filer: TFiler); override;

    procedure Changed; virtual;
    procedure DoAssign(Source: TdxPSReportDocument); virtual;
    procedure DoRestoreDefaults; virtual;
    function GetInfoTip: string; virtual;

    procedure ReadData(AReader: TdxPSDataReader); virtual;
    procedure WriteData(AWriter: TdxPSDataWriter); virtual;

    property UpdateCount: Integer read FUpdateCount write FUpdateCount;
  public
    constructor Create(AReportLink: TBasedxReportLink); virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    procedure BeginUpdate;
    procedure CancelUpdate;
    procedure EndUpdate;
    function IsUpdateLocked: Boolean;

    function DefaultCaption: string; virtual;
    function DefaultCreator: string; virtual;
    function DefaultDescription: string; virtual;

    procedure RestoreDefaults;
    procedure RetrievePreview; virtual;

    property InfoTip: string read GetInfoTip;
    property Preview: TMetafile read FPreview;
    property ReportLink: TBasedxReportLink read FReportLink;

    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  published
    property Caption: string read GetCaption write SetCaption stored IsCaptionStored;
    property CreationDate: TDateTime read FCreationDate write SetCreationDate;
    property Creator: string read GetCreator write SetCreator stored IsCreatorStored;
    property Description: string read GetDescription write SetDescription stored IsDesciptionStored;
  end;

  TdxPSDataStorageOffsetTableInfo = packed record
    Information: Longint;
    Document: Longint;
    Title: Longint;
    Data: Longint;
    Footnotes: Longint;
    Reserved1: Longint;
    Reserved2: Longint;
    Reserved3: Longint;
  end;

  { TdxPSDataStorageOffsetTable }

  TdxPSDataStorageOffsetTable = class(TObject)
  protected
    FTableInfo: TdxPSDataStorageOffsetTableInfo;
    procedure DoAssign(Source: TdxPSDataStorageOffsetTable); virtual;
  public
    constructor Create(ATemplate: TdxPSDataStorageOffsetTable = nil); virtual;
    procedure Assign(Source: TdxPSDataStorageOffsetTable); virtual;
    procedure Clear; virtual;
    procedure ReadData(AReader: TdxPSDataReader); virtual;
    procedure WriteData(AWriter: TdxPSDataWriter); virtual;
    //
    property Information: Longint read FTableInfo.Information write FTableInfo.Information;
    property Document: Longint read FTableInfo.Document write FTableInfo.Document;
    property Title: Longint read FTableInfo.Title write FTableInfo.Title;
    property Data: Longint read FTableInfo.Data write FTableInfo.Data;
    property Footnotes: Longint read FTableInfo.Footnotes write FTableInfo.Footnotes;
    property Reserved1: Longint read FTableInfo.Reserved1 write FTableInfo.Reserved1;
    property Reserved2: Longint read FTableInfo.Reserved2 write FTableInfo.Reserved2;
    property Reserved3: Longint read FTableInfo.Reserved3 write FTableInfo.Reserved3;
  end;
  TdxPSDataStorageOffsetTableClass = class of TdxPSDataStorageOffsetTable;
  PdxPSDataStorageOffsetTable = TdxPSDataStorageOffsetTable; // for backward compatibility

  { TdxPSDataStorageInfo }

  TdxPSDataStorageInfoClass = class of TdxPSDataStorageInfo;
  TdxPSDataStorageInfo = class(TObject)
  public
    StorageVersion: Integer;
    PrintingSystemVersion: TdxPSVersion;
    LinkClassName: string[255];
    ComponentClassName: string[255];
    LinkClass: TdxReportLinkClass;
    ComponentClass: TComponentClass;
    UnitsPerInch: Integer;
    constructor Create(AReportLink: TBasedxReportLink); virtual;
    procedure ReadData(AReader: TdxPSDataReader); virtual;
    procedure WriteData(AWriter: TdxPSDataWriter); virtual;
  end;
  PdxPSDataStorageInfo = TdxPSDataStorageInfo; // for backward compatibility

  TdxAssignedFormatValue = (fvDate, fvTime, fvPageNumber);
  TdxAssignedFormatValues = set of TdxAssignedFormatValue;

  TdxReportLinkState = (rlsDataLoading, rlsDataSaving, rlsPagination);
  TdxReportLinkStates = set of TdxReportLinkState;

  TdxReportLinkCapability = (rlcPageSetup, rlcHeaderFooter, rlcTitle, rlcFootnotes);
  TdxReportLinkCapabilities = set of TdxReportLinkCapability;

  TdxReportLinkDataSource = (rldsComponent, rldsExternalStorage);

  TdxCustomDrawReportLinkTitleEvent = procedure(Sender: TBasedxReportLink; ACanvas: TCanvas; ARect: TRect;
    ANom, ADenom: Integer; var ATextAlignX: TcxTextAlignX; var ATextAlignY: TcxTextAlignY; var AColor: TColor;
    AFont: TFont; var ADone: Boolean) of object;

  TdxCustomDrawReportLinkHFEvent = procedure(Sender: TObject; ACanvas: TCanvas; APageIndex: Integer;
    var ARect: TRect; ANom, ADenom: Integer; var ADefaultDrawText, ADefaultDrawBackground: Boolean) of object;

  TdxFilterStyleEvent = procedure(Sender: TBasedxReportLink;
    AStyle: TBasedxPrintStyle; var ASupported: Boolean) of object;

  TdxMeasureReportLinkTitleEvent = procedure (Sender: TBasedxReportLink; var AHeight: Integer) of object;
  TdxPSGetNewReportStorageNameEvent = procedure (Sender: TBasedxReportLink; var AName: string) of object;

  TdxPSGetImageListProc = procedure(AnImageList: TCustomImageList) of object;

  { IdxPSNativeWin32ControlHandleSupport }

  IdxPSNativeWin32ControlHandleSupport = interface
  ['{4B649281-A283-4CAC-98D4-08E779A7F9C8}']
    function GetNativeHandle: THandle;
    procedure SetNativeHandle(Value: THandle);
    property NativeHandle: THandle read GetNativeHandle write SetNativeHandle;
  end;

  { TBasedxReportLink }

  TBasedxReportLink = class(TcxScalableComponent, IdxPrinting)
  private
    FActive: Boolean;
    FAssignedFormatValues: TdxAssignedFormatValues;
    FBuiltIn: Boolean;
    FComponent: TComponent;
    FComponentPrinter: TCustomdxComponentPrinter;
    FController: TBasedxReportLink;
    FCurrentPage: Integer;
    FData: Pointer;
    FDataSource: TdxReportLinkDataSource;
    FDataStream: TStream;
    FDateFormat: Integer;
    FDefaultFont: TFont;
    FDesignerCaption: string;
    FDesignerHelpContext: THelpContext;
    FDesignWindow: TAbstractdxReportLinkDesignWindow;
    FFont: TFont;
    FFootersOnEveryPage: Boolean;
    FHeadersOnEveryPage: Boolean;
    FInternalStreaming: Boolean;
    FIsDesignerCaptionAssigned: Boolean;
    FIsInvalidReport: Boolean;
    FPageNumberFormat: TdxPageNumberFormat;
    FPDFExportOptions: TdxPSPDFReportExportOptions;
    FPrinterPage: TdxPrinterPage;
    FRebuildNeeded: Boolean;
    FRenderer: TdxPSReportRenderer;
    FRenderInfo: TdxPSReportRenderInfo;
    FReportCells: TdxReportCells;
    FReportDocument: TdxPSReportDocument;
    FReportFootnotes: TdxReportFootnotes;
    FReportTitle: TdxReportTitle;
    FSavedReportDocument: TdxPSReportDocument;
    FSavedReportFootnotes: TdxReportFootnotes;
    FSavedReportTitle: TdxReportTitle;
    FScaleFonts: Boolean;
    FShowDesigner: Boolean;
    FShowPageFooter: Boolean;
    FShowPageHeader: Boolean;
    FShowPageRowHeader: Boolean;
    FStartPageIndex: Integer;
    FStorageName: string;
    FStyleManager: TdxPrintStyleManager;
    FSubscriber: TdxEventSubscriber;
    FTimeFormat: Integer;
    FTransparent: Boolean;
    FUseHorzDelimiters: Boolean;
    FUseVertDelimiters: Boolean;

    FBackgroundBitmapPool: TdxPSBackgroundBitmapPool;
    FFontPool: TdxPSReportFontPool;
    FPainting: Boolean;
    FPrepared: Boolean;
    FStreamedActive: Boolean;

    FOnChangeComponent: TNotifyEvent;
    FOnCustomDrawPageFooter: TdxCustomDrawReportLinkHFEvent;
    FOnCustomDrawPageHeader: TdxCustomDrawReportLinkHFEvent;
    FOnCustomDrawReportLinkFootnotes: TdxCustomDrawReportLinkTitleEvent;
    FOnCustomDrawReportLinkTitle: TdxCustomDrawReportLinkTitleEvent;
    FOnDataSourceChanged: TNotifyEvent;
    FOnDestroy: TNotifyEvent;
    FOnFilterStyle: TdxFilterStyleEvent;
    FOnGetNewReportStorageName: TdxPSGetNewReportStorageNameEvent;
    FOnMeasureReportLinkFootnotes: TdxMeasureReportLinkTitleEvent;
    FOnMeasureReportLinkTitle: TdxMeasureReportLinkTitleEvent;

    function GetAbortBuilding: Boolean;
    function GetCaption: string;
    function GetCurrentPrintStyle: TBasedxPrintStyle;
    function getDateFormat: Integer;
    function GetDateTime: TDateTime;
    function GetDescription: string;
    function GetDesignerCaption: string;
    function GetFontPool: TdxPSReportFontPool;
    function GetHasDesignWindow: Boolean;
    function GetHasPreviewWindow: Boolean;
    function GetIndex: Integer;
    function GetIsAggregated: Boolean;
    function GetIsBuilding: Boolean;
    function GetIsCurrentLink: Boolean;
    function GetIsReading: Boolean;
    function GetPageHeight: Integer;
    function GetPageNumberFormat: TdxPageNumberFormat;
    function GetPageWidth: Integer;
    function GetPreviewWindow: TdxPSCustomPreviewWindow;
    function GetRealPrinterPage: TdxPrinterPage;
    function GetRenderer: TdxPSReportRenderer;
    function GetRenderInfo: TdxPSReportRenderInfo;
    function GetRenderStage: TdxPSRenderStages;
    function GetReportTitleMode: TdxReportTitleMode;
    function GetReportTitleText: string;
    function GetShowPageFooter: Boolean;
    function GetShowPageHeader: Boolean;
    function GetShowPageRowHeader: Boolean;
    function GetShrinkToPageWidth: Boolean;
    function getTimeFormat: Integer;
    function GetVirtualPageCount: Integer;
    function IsDateFormatStored: Boolean;
    function IsDesignerCaptionStored: Boolean;
    function IsPageNumberFormatStored: Boolean;
    function IsTimeFormatStored: Boolean;
    procedure SetAbortBuilding(Value: Boolean);
    procedure SetAssignedFormatValues(Value: TdxAssignedFormatValues);
    procedure SetCaption(const Value: string);
    procedure SetComponentPrinter(Value: TCustomdxComponentPrinter);
    procedure SetCurrentPage(Value: Integer);
    procedure SetDateFormat(Value: Integer);
    procedure SetDataSource(Value: TdxReportLinkDataSource);
    procedure SetDateTime(const Value: TDateTime);
    procedure SetDesignerCaption(const Value: string);
    procedure SetDescription(const Value: string);
    procedure SetIndex(Value: Integer);
    procedure SetIsCurrentLink(Value: Boolean);
    procedure SetPageNumberFormat(Value: TdxPageNumberFormat);
    procedure SetPDFExportOptions(AValue: TdxPSPDFReportExportOptions);

    procedure SetRealPrinterPage(Value: TdxPrinterPage);
    procedure SetReportDocument(Value: TdxPSReportDocument);
    procedure SetReportFootnotes(Value: TdxReportFootnotes);
    procedure SetReportTitle(Value: TdxReportTitle);
    procedure SetReportTitleMode(Value: TdxReportTitleMode);
    procedure SetReportTitleText(const Value: string);
    procedure SetShowPageFooter(Value: Boolean);
    procedure SetShowPageHeader(Value: Boolean);
    procedure SetShowPageRowHeader(Value: Boolean);
    procedure SetShrinkToPageWidth(Value: Boolean);
    procedure SetStorageName(const Value: string);
    procedure SetStyleManager(Value: TdxPrintStyleManager);
    procedure SetTimeFormat(Value: Integer);
    procedure SetUseHorzDelimiters(Value: Boolean);
    procedure SetUseVertDelimiters(Value: Boolean);

    function CurrentComposition: TdxCompositionReportLink;

    function ValidateMargins: Boolean;

    procedure DefineStylesClick(Sender: TObject);
    procedure StyleClick(Sender: TObject);

    procedure ReadBuiltIn(Reader: TReader);
    procedure ReadComponentName(AReader: TReader);
    procedure ReadIsDesignerCaptionAssigned(Reader: TReader);
    procedure ReadStyleManagerName(AReader: TReader);
    procedure WriteBuiltIn(Writer: TWriter);
    procedure WriteComponentName(AWriter: TWriter);
    procedure WriteIsDesignerCaptionAssigned(Writer: TWriter);
    procedure WriteStyleManagerName(AWriter: TWriter);
  protected
    FColor: TColor;
    FFontIndex: Integer;
    FState: TdxReportLinkStates;

    procedure AssignTo(Dest: TPersistent); override;
    procedure ChangeScale(M, D: Integer); override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure ReadState(Reader: TReader); override;
    procedure SetName(const NewName: TComponentName); override;
    procedure SetParentComponent(AParent: TComponent); override;

    function GetStartPageIndex: Integer; virtual;
    procedure SetStartPageIndex(Value: Integer); virtual;

    { PrinterPage }
    function CreatePrinterPage: TdxPrinterPage; virtual;
    function GetPrinterPage: TdxPrinterPage; virtual;
    procedure SetPrinterPage(Value: TdxPrinterPage); virtual;

    { Render Info}
    function CreateRenderInfo: TdxPSReportRenderInfo; virtual;
    function GetRenderInfoClass: TdxPSReportRenderInfoClass; virtual;
    procedure CalculateRenderInfos;
    procedure ClearGDIPools;
    procedure FreeRenderInfos;

    function CreateRenderer: TdxPSReportRenderer; virtual;
    function GetRendererClass: TdxPSReportRendererClass; virtual;
    procedure FreeRenderer;

    procedure InitializeDefaultFont(AFont: TFont); virtual;
    procedure InternalGetDelimiters(ADelimitersHorz, ADelimitersVert: TList);

    function IsEntirePageCustomDrawn: Boolean;
    function IsFootnotesCustomDrawn: Boolean;
    function IsHeaderOrFooterCustomDrawn(AHFObject: TCustomdxPageObject): Boolean;
    function IsTitleCustomDrawn: Boolean;

    function PageReady(APageIndex: Integer): Boolean; {obsolete: always returns True}
    procedure PrepareBuildReport; virtual;
    procedure PrepareFonts(UPI: Integer); virtual;

    procedure PrepareLongOperation;
    procedure UnprepareLongOperation;

    function CreateReportDocument: TdxPSReportDocument;
    procedure DocumentChanged(Sender: TObject); virtual;
    class function GetReportDocumentClass: TdxPSReportDocumentClass; virtual;

    function CreateReportCells: TdxReportCells; virtual;
    function GetReportCellsClass: TdxReportCellsClass; virtual;

    function IsRealPrinterPage(APage: TdxPrinterPage): Boolean;
    function CreatePreviewWindowForm: TdxPSCustomPreviewWindow; virtual;

    { Read/Write Data }
    class function CreateDataReader(AStream: TStream): TdxPSDataReader;
    class function CreateDataWriter(AStream: TStream): TdxPSDataWriter;
    class function GetDataReaderClass: TdxPSDataReaderClass; virtual;
    class function GetDataWriterClass: TdxPSDataWriterClass; virtual;
    procedure InternalLoadDataFromStream(AStream: TStream);
    procedure InternalReadData(AReader: TdxPSDataReader); virtual;
    procedure InternalWriteData(AWriter: TdxPSDataWriter); virtual;
    function IsRebuildNeededAndAllowed: Boolean;

    function RetrieveStorageInfo: TdxPSDataStorageInfo; virtual;
    procedure ReadData(AReader: TdxPSDataReader); virtual;
    class function ReadOffsetTable(AReader: TdxPSDataReader): TdxPSDataStorageOffsetTable; virtual;
    class procedure ReadReportDocument(AReader: TdxPSDataReader; AReportDocument: TdxPSReportDocument); virtual;
    class function ReadStorageInfo(AReader: TdxPSDataReader): TdxPSDataStorageInfo; virtual;
    class procedure SkipStorageInfo(AReader: TdxPSDataReader);
    procedure WriteData(AWriter: TdxPSDataWriter); virtual;
    class procedure WriteOffsetTable(AWriter: TdxPSDataWriter; AnOffsetTable: TdxPSDataStorageOffsetTable); virtual;
    class procedure WriteReportDocument(AWriter: TdxPSDataWriter; AReportDocument: TdxPSReportDocument); virtual;
    class procedure WriteStorageInfo(AWriter: TdxPSDataWriter; AStorageInfo: TdxPSDataStorageInfo); virtual;

    { Title / Footnotes }
    procedure ReadFootnotes(AReader: TdxPSDataReader); virtual;
    procedure ReadTitle(AReader: TdxPSDataReader); virtual;
    procedure WriteFootnotes(AWriter: TdxPSDataWriter); virtual;
    procedure WriteTitle(AWriter: TdxPSDataWriter); virtual;

    { Read/Write Link Data}
    procedure ReadBackgroundBitmapPool(AReader: TdxPSDataReader);
    procedure ReadFontPool(AReader: TdxPSDataReader);
    procedure ReadRenderInfo(AReader: TdxPSDataReader);
    procedure ReadReportData(AReader: TdxPSDataReader);
    procedure WriteBackgroundBitmapPool(AWriter: TdxPSDataWriter);
    procedure WriteFontPool(AWriter: TdxPSDataWriter);
    procedure WriteRenderInfo(AWriter: TdxPSDataWriter);
    procedure WriteReportData(AWriter: TdxPSDataWriter);

    { properties read/write virtual methods }
    function GetAllowContinuousPageIndexes: Boolean; virtual;
    function GetAlwaysBufferedGraphics: Boolean; virtual;
    function GetContinuousPageIndexes: Boolean; virtual;
    function GetRealScaleFactor: Integer; virtual;
    function GetReportHeight: Integer; virtual;
    function GetReportWidth: Integer; virtual;
    function IsFontStored: Boolean; virtual;
    procedure SetActive(Value: Boolean); virtual;
    procedure SetColor(Value: TColor); virtual;
    procedure SetComponent(Value: TComponent); virtual;
    procedure SetContinuousPageIndexes(Value: Boolean); virtual;
    procedure SetFont(Value: TFont); virtual;
    procedure SetFootersOnEveryPage(Value: Boolean); virtual;
    procedure SetHeadersOnEveryPage(Value: Boolean); virtual;
    procedure SetTransparent(Value: Boolean); virtual;

    procedure FontChanged(Sender: TObject); virtual;
    procedure LinkModified(Value: Boolean); virtual;

    procedure AfterDesignReport(ADone: Boolean); virtual;
    procedure BeforeDesignReport; virtual;

    function PrintDialog(var APrintDlgData: TdxPrintDlgData): Boolean; virtual;
    procedure AfterPrinting; virtual;
    procedure BeforePrinting; virtual;

    function CalculateActualScaleFactor: Integer; virtual;
    function CannotActivateReportErrorString: string; virtual;
    procedure ConstructReport(AReportCells: TdxReportCells); virtual;
    procedure ConvertCoords; virtual;

    procedure CustomDraw(AnItem: TAbstractdxReportCellData;
      ACanvas: TCanvas; ABoundsRect, AClientRect: TRect; var ADone: Boolean); virtual;
    function GetDesignerClass: TdxReportLinkDesignWindowClass; virtual;
    procedure DoApplyInDesigner; virtual;
    function DoBeforeExportToPDF(const AFileName: string; AOptions: TdxPSPDFReportExportOptions): Boolean; dynamic;
    procedure DoCreateReport; virtual;
    procedure DoCreateReportData;
    procedure DoDataProviderDontPresent; dynamic;
    procedure DoDestroyReport; virtual;
    procedure DoPageParamsChanged; virtual;

    procedure CopyDataStreamFrom(AStream: TStream);
    procedure FinalizeDataStream; virtual;
    function GetBreakPagesByHardDelimiters: Boolean; virtual;
    function GetCapabilities: TdxReportLinkCapabilities; virtual;
    function GetCriticalSize(AReportCells: TdxReportCells): Integer; virtual;
    procedure GetImageLists(AProc: TdxPSGetImageListProc); virtual;
    function GetPageCount: Integer; virtual;
    function GetRebuildOnPageParamsChange(AUpdateCodes: TdxPrinterPageUpdateCodes): Boolean; virtual;
    function GetUseHardHorzDelimiters: Boolean; virtual;
    function GetUseHardVertDelimiters: Boolean; virtual;
    procedure InternalActivate; virtual;
    procedure InternalRestoreDefaults; virtual;
    procedure InternalRestoreFromOriginal; virtual;
    function IsApplyBackgroundToEntirePage: Boolean; virtual;
    function IsDrawFootersOnEveryPage: Boolean; virtual;
    function IsDrawHeadersOnEveryPage: Boolean; virtual;
    function IsDrawHeaderCornersOnEveryPage: Boolean; virtual;
    function IsDrawRowHeadersOnEveryPage: Boolean; virtual;
    function IsScaleGridLines: Boolean; virtual;
    function IsSupportedCustomDraw(Item: TAbstractdxReportCellData): Boolean; virtual;
    procedure MakeDelimiters(AReportCells: TdxReportCells; AHorzDelimiters, AVertDelimiters: TList); virtual;
    procedure MakeHardDelimiters(AReportCells: TdxReportCells; AHorzDelimiters, AVertDelimiters: TList); virtual;
    procedure PageParamsChanged(Sender: TdxPrinterPage; AStyle: TBasedxPrintStyle; AUpdateCodes: TdxPrinterPageUpdateCodes); virtual;
    function PossibleCustomDraw(AnItem: TAbstractdxReportCellData): Boolean; virtual;
    procedure PrepareReportGroupsLookAndFeels;
    procedure RetrievePageAsImage(APageIndex: Integer; AGraphicClass: TGraphicClass; AGraphic: TGraphic);
    procedure RetrievePageAsImageCallBack(AComponentPrinter: TCustomdxComponentPrinter; AReportLink: TBasedxReportLink;
      AIndex, APageIndex: Integer; const AGraphic: TGraphic; AData: Pointer; var AContinue: Boolean);
    procedure ShowPageFooterChanged; virtual;
    procedure ShowPageHeaderChanged; virtual;
    procedure StdProcessDataSourceDontPresent; virtual;
    procedure TunePixelsNumerator(AReportCells: TdxReportCells); virtual;
    procedure PaintPage(ACanvas: TCanvas; const APageBounds: TRect; APageIndex, AContinuousPageIndex, AZoomFactor: Integer); virtual;
    procedure PaintPageEx(ACanvas: TdxPSReportRenderCustomCanvas;
      const APageBounds: TRect; APageIndex, AContinuousPageIndex, AZoomFactor: Integer); virtual;

    procedure ComponentUnsupportedError(AComponent: TComponent);

    procedure DoChangeComponent; dynamic;
    procedure DoCustomDrawEntirePage(ACanvas: TCanvas; R: TRect; ARealPageIndex: Integer); virtual;
    procedure DoCustomDrawPageHeaderOrFooter(AHFObject: TCustomdxPageObject; ACanvas: TCanvas;
      APageIndex: Integer; R: TRect; var ADefaultDrawText, ADefaultDrawBackground: Boolean); virtual;
    procedure DoCustomDrawPageFootnotes(ACanvas: TCanvas; R: TRect; var ATextAlignX: TcxTextAlignX;
      var ATextAlignY: TcxTextAlignY; var AColor: TColor; AFont: TFont; var ADone: Boolean); virtual;
    procedure DoCustomDrawPageTitle(ACanvas: TCanvas; R: TRect; var ATextAlignX: TcxTextAlignX;
      var ATextAlignY: TcxTextAlignY; var AColor: TColor; AFont: TFont; var ADone: Boolean); virtual;
    procedure DoParentCustomDrawPageHeaderOrFooter(AHFObject: TCustomdxPageObject; ACanvas: TCanvas;
      APageIndex: Integer; R: TRect; var ADefaultDrawText, ADefaultDrawBackground: Boolean; APixelsNumerator: Integer); virtual;
    procedure DoParentCustomDrawReportFootnotes(ACanvas: TCanvas; R: TRect; var ATextAlignX: TcxTextAlignX;
      var ATextAlignY: TcxTextAlignY; var AColor: TColor; AFont: TFont; var ADone: Boolean; APixelsNumerator: Integer); virtual;
    procedure DoParentCustomDrawReportTitle(ACanvas: TCanvas; R: TRect; var ATextAlignX: TcxTextAlignX;
      var ATextAlignY: TcxTextAlignY; var AColor: TColor; AFont: TFont; var ADone: Boolean; APixelsNumerator: Integer); virtual;
    procedure DoGetNewReportStorageName(var AName: string); virtual;

    procedure DoDataSourceChanged; dynamic;
    procedure DoDestroy; dynamic;
    procedure DoMeasureReportLinkFootnotes(var AHeight: Integer); virtual;
    procedure DoMeasureReportLinkTitle(var AHeight: Integer); virtual;
    procedure DoProgress(const APercentDone: Double); dynamic;
    function HasExternalListeners: Boolean;
    function IsComposable(AComposition: TdxCompositionReportLink): Boolean; virtual;
    function IsSupportedStyle(APrintStyle: TBasedxPrintStyle): Boolean; virtual;
    function NeedTwoPassRendering: Boolean; virtual;

    procedure DesignerModified;
    procedure DesignerUpdate(TheAll: Boolean);
    function IsComponentPrinterAvailable: Boolean;
    function IsDesigning: Boolean;
    function IsDestroying: Boolean;
    function IsLoading: Boolean;
    // IdxPrinting
    function CanBuildReport(AComponent: TComponent): Boolean;
    procedure Print(const APageIndexes: array of Integer); overload; virtual;

    property AllowContinuousPageIndexes: Boolean read GetAllowContinuousPageIndexes;
    property AlwaysBufferedGraphics: Boolean read GetAlwaysBufferedGraphics;
    property BackgroundBitmapPool: TdxPSBackgroundBitmapPool read FBackgroundBitmapPool;
    property BreakPagesByHardDelimiters: Boolean read GetBreakPagesByHardDelimiters;
    property Color: TColor read FColor write SetColor default clWhite;
    property ContinuousPageIndexes: Boolean read GetContinuousPageIndexes write SetContinuousPageIndexes default False;
    property DataStream: TStream read FDataStream;
    property Font: TFont read FFont write SetFont stored IsFontStored;
    property FontPool: TdxPSReportFontPool read GetFontPool;
    property FootersOnEveryPage: Boolean read FFootersOnEveryPage write SetFootersOnEveryPage default False;
    property HeadersOnEveryPage: Boolean read FHeadersOnEveryPage write SetHeadersOnEveryPage default False;
    property InternalStreaming: Boolean read FInternalStreaming write FInternalStreaming;
    property IsBuilding: Boolean read GetIsBuilding;
    property IsInvalidReport: Boolean read FIsInvalidReport;  //stored in external storage
    property IsReading: Boolean read GetIsReading;
    property PageHeight: Integer read GetPageHeight;
    property PageWidth: Integer read GetPageWidth;
    property RenderInfo: TdxPSReportRenderInfo read GetRenderInfo;
    property ScaleFonts: Boolean read FScaleFonts write FScaleFonts default True;
    property State: TdxReportLinkStates read FState;
    property Transparent: Boolean read FTransparent write SetTransparent default True;
    property UseHardHorzDelimiters: Boolean read GetUseHardHorzDelimiters;
    property UseHardVertDelimiters: Boolean read GetUseHardVertDelimiters;
    property UseHorzDelimiters: Boolean read FUseHorzDelimiters write SetUseHorzDelimiters default True;
    property UseVertDelimiters: Boolean read FUseVertDelimiters write SetUseVertDelimiters default True;
    property VirtualPageCount: Integer read GetVirtualPageCount;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeforeDestruction; override;
    procedure Assign(Source: TPersistent); override;
    function GetParentComponent: TComponent; override;
    function HasParent: Boolean; override;

    procedure LoadFromIniFile(AIniFile: TCustomIniFile; const ASection: string); overload; virtual;
    procedure LoadFromIniFile(const AFileName: string); overload;
    procedure LoadFromRegistry(const APath: string);
    procedure SaveToIniFile(AIniFile: TCustomIniFile; const ASection: string); overload; virtual;
    procedure SaveToIniFile(const AFileName: string); overload;
    procedure SaveToRegistry(const APath: string);

    class function Aggregable: Boolean; virtual;
    class function CanBeUsedAsStub: Boolean; virtual; // when data loaded from external storage
    class function Serializable: Boolean; virtual;

    function DefaultDateFormat: Integer; virtual;
    function DefaultDesignerCaption: string; virtual;
    function DefaultFont: TFont; virtual;
    function DefaultPageNumberFormat: TdxPageNumberFormat; virtual;
    function DefaultTimeFormat: Integer; virtual;

    procedure RestoreDefaults; virtual;
    procedure RestoreFromOriginal; virtual;

    function CheckToDesign: Boolean;
    function DataProviderPresent: Boolean; virtual;
    function DesignerExists(AComponentClass: TComponentClass): Boolean; virtual;
    function DesignReport: Boolean;
    procedure DestroyReport; virtual;
    procedure GetPageColRowCount(out APageColCount, APageRowCount: Integer); virtual;
    procedure Initialize; virtual;
    function IsEmptyPage(APageIndex: Integer): Boolean; virtual;
    function IsEmptyReport: Boolean; virtual;
    procedure RebuildReport; virtual;
    { Use this routine when you attempt to assign ReportLink to Component that actually is in a DLL }
    { Don't use this routine in regular cases, i.e. ReportLink is in the same executable file }
    procedure SetComponentUnconditionally(Value: TComponent);

    class procedure GetSupportedComponentList(AList: TdxClassList);
    class function IsSupportedCompClass(AComponentClass: TClass): Boolean; virtual;
    class function LinkClass: TdxReportLinkClass;
    class function Supports(AnObject: TObject{TComponent}): Boolean; overload;
    class function Supports(AClass: TClass): Boolean; overload;

    function Print(AShowDialog: Boolean): Boolean; overload;
    function Print(AShowDialog: Boolean; APrintDialogData: PdxPrintDlgData): Boolean; overload;
    procedure PrintEx(APageNums: TdxPageNumbers; ACopies: Integer; ACollate: Boolean);
    procedure PrintPages(const APageIndexes: TdxPSPageIndexes);
    procedure PrintPagesEx(const APageIndexes: TdxPSPageIndexes;
      APageNums: TdxPageNumbers; ACopyCount: Integer; ACollate: Boolean);

    procedure ExportToPDF; overload;
    procedure ExportToPDF(const AFileName: string; ACanShowDialog: Boolean = True; ASettings: TdxPSPDFReportExportOptions = nil); overload;

    procedure Preview(Modal: Boolean = True); virtual;
    function PreviewExists: Boolean;

    procedure BuildPageSetupMenu(ARootItem: TComponent; AData: Pointer; AIncludeDefineItem: Boolean = True);
    procedure DefinePrintStylesDlg;
    procedure GetFilteredStyles(AStrings: TStrings);
    function PageSetup: Boolean;
    function PageSetupEx(AActivePageIndex: Integer; AShowPreviewBtn, AShowPrintBtn: Boolean;
      out APreviewBtnClicked, APrintBtnClicked: Boolean): Boolean; overload;

    function ShowDateTimeFormatsDlg: Boolean;
    function ShowFootnotesPropertiesDlg: Boolean;
    function ShowPageNumberFormatsDlg: Boolean;
    function ShowTitlePropertiesDlg: Boolean;

    function SupportsScaling: Boolean; virtual;

    function CanChangeFootnotes: Boolean;
    function CanChangeTitle: Boolean;

    function CanLoadData: Boolean; virtual;
    function CanSaveData: Boolean; virtual;
    function CanUnloadData: Boolean; virtual;
    function GetNewReportStorageName: string; virtual;
    procedure LoadDataFromFile(const AName: string); virtual;
    procedure LoadDataFromStream(AStream: TStream); virtual;
    procedure SaveDataToFile(const AName: string); virtual;
    procedure SaveDataToStream(AStream: TStream); virtual;

    class function ExtractComponentClass(AStream: TStream; ARaiseException: Boolean = False): TComponentClass;
    class function ExtractComponentClassName(AStream: TStream; ARaiseException: Boolean = False): string;
    class function ExtractLinkClass(AStream: TStream; ARaiseException: Boolean = False): TdxReportLinkClass;
    class function ExtractOffsetTable(AStream: TStream; ARaiseException: Boolean = False): TdxPSDataStorageOffsetTable;
    class function ExtractReportDocument(AStream: TStream; ARaiseException: Boolean = False): TdxPSReportDocument;
    class function ExtractStorageInfo(AStream: TStream; ARaiseException: Boolean = False): TdxPSDataStorageInfo;
    class function ExtractStorageVersion(AStream: TStream; ARaiseException: Boolean = False): Integer;
    class procedure FinalizeStorageInfo(var AStorageInfo: TdxPSDataStorageInfo);
    class function PossibleDataStorage(AStream: TStream; ARaiseException: Boolean = False): Boolean; overload;
    class function PossibleDataStorage(const AFileName: string; ARaiseException: Boolean = False): Boolean; overload;

    function AddBackgroundBitmapToPool(ABitmap: TGraphic): Integer;
    function AddFontToPool(AFont: TFont): Integer; overload;
    function AddFontToPool(const AName: string; AColor: TColor; APitch: TFontPitch; AStyle: TFontStyles; ASize: Integer): Integer; overload;

    function CreateGroupLookAndFeel(AClass: TdxPSReportGroupLookAndFeelClass; ACheckExisting: Boolean = True): TdxPSReportGroupLookAndFeel;
    function FindGroupLookAndFeelByClass(AClass: TdxPSReportGroupLookAndFeelClass): TdxPSReportGroupLookAndFeel;
    function IndexOfReportGroupLookAndFeel(ALookAndFeel: TdxPSReportGroupLookAndFeel): Integer;

    procedure DrawPageHeader(APageIndex: Integer; ARect: TRect; ATitleParts: TdxPageTitleParts; ADrawBackground: Boolean);
    procedure DrawPageFooter(APageIndex: Integer; ARect: TRect; ATitleParts: TdxPageTitleParts; ADrawBackground: Boolean);

    procedure DrawCheckBox(Canvas: TCanvas; var R: TRect; Checked, Enabled, FlatBorder: Boolean;
      BoldBorder: Boolean = False); overload; virtual;
    procedure DrawCheckBox(Canvas: TCanvas; var R: TRect; Checked, Enabled, IsRadio: Boolean;
      EdgeStyle: TdxCheckButtonEdgeStyle; BorderColor: TColor = clWindowText); overload; virtual;
    procedure drawEdge(Canvas: TCanvas; var R: TRect; EdgeMode: TdxCellEdgeMode;
      InnerEdge, OuterEdge: TdxCellEdgeStyle; Sides: TdxCellSides = [csLeft..csBottom]; Soft: Boolean = True); virtual;
    procedure DrawEllipse(Canvas: TCanvas; R: TRect; ForeColor, BkColor: TColor;
      Pattern: TdxPSFillPatternClass; BorderColor: TColor; BorderThickness: Integer = 1); virtual;
    procedure DrawExpandButton(Canvas: TCanvas; var R: TRect;
      Expanded, DrawBorder, Edge3D, Edge3DSoft, Shadow, FillInterior: Boolean;
      BorderColor, InteriorColor: TColor); virtual;
    procedure DrawGlyph(DC: HDC; const R: TRect; AGlyph: Byte);
    procedure DrawGraphic(Canvas: TCanvas; var R: TRect; const ClipRect: TRect;
      ImageList: TCustomImageList; ImageIndex: Integer; Graphic: TGraphic;
      GraphicTransparent, Transparent: Boolean; BkColor: TColor); virtual;
    procedure DrawGraphicEx(Canvas: TCanvas; var R: TRect; const ClipRect: TRect;
      ImageList: TCustomImageList; ImageIndex: Integer; Graphic: TGraphic;
      GraphicTransparent, Transparent: Boolean; BkColor, ForeColor: TColor;
      Pattern: TdxPSFillPatternClass; AActualImageBuffering: TdxCellImageActualBuffering = cibAlways); virtual;
    procedure DrawRectangle(Canvas: TCanvas; R: TRect; ForeColor, BkColor: TColor;
      ContentPattern: TdxPSFillPatternClass; BorderColor: TColor;
      BorderThickness: Integer = 1); virtual;
    procedure DrawRoundRect(Canvas: TCanvas; R: TRect; CornerWidth, CornerHeight: Integer;
      ForeColor, BkColor: TColor; ContentPattern: TdxPSFillPatternClass;
      BorderColor: TColor; BorderThickness: Integer = 1); virtual;
    procedure DrawSortMark(Canvas: TCanvas; var R: TRect; SortOrder: TdxCellSortOrder; Mono: Boolean); virtual;
    procedure drawText(Canvas: TCanvas; var R: TRect; AIndent: Integer;
      const Text: string; Font: TFont; BkColor: TColor; TextAlignX: TcxTextAlignX;
      TextAlignY: TcxTextAlignY; FillBackground, Multiline, EndEllipsis: Boolean); virtual;
    procedure drawTextEx(Canvas: TCanvas; var R: TRect; MaxLineCount: Integer;
      LeftIndent, RightIndent: Integer; const Text: string; Font: TFont;
      BkColor: TColor; TextAlignX: TcxTextAlignX; TextAlignY: TcxTextAlignY;
      FillBackground, Multiline, EndEllipsis, PreventLeftTextExceed, PreventTopTextExceed: Boolean); virtual;
    procedure FillEllipse(Canvas: TCanvas; const R: TRect; Color: TColor); virtual;
    procedure FillEllipseEx(Canvas: TCanvas; const R: TRect; ForeColor, BkColor: TColor; Pattern: TdxPSFillPatternClass); virtual;
    procedure FillRectEx(Canvas: TCanvas; const R: TRect; ForeColor, BkColor: TColor; Pattern: TdxPSFillPatternClass); virtual;
    procedure FillRoundRect(Canvas: TCanvas; const R: TRect; CornerWidth, CornerHeight: Integer; Color: TColor); virtual;
    procedure FillRoundRectEx(Canvas: TCanvas; const R: TRect; CornerWidth, CornerHeight: Integer;
      ForeColor, BkColor: TColor; Pattern: TdxPSFillPatternClass); virtual;
    procedure FillRgnEx(Canvas: TCanvas; Rgn: TcxRegionHandle; ForeColor, BkColor: TColor; Pattern: TdxPSFillPatternClass); virtual;
    procedure FrameEllipse(Canvas: TCanvas; R: TRect; Color: TColor; Thickness: Integer = 1);
    procedure frameRect(Canvas: TCanvas; R: TRect; Color: TColor; Sides: TdxCellSides = [csLeft..csBottom];
      Thickness: Integer = 1);
    procedure FrameRoundRect(Canvas: TCanvas; R: TRect; CornerWidth, CornerHeight: Integer;
      Color: TColor; Thickness: Integer = 1); virtual;

    property AbortBuilding: Boolean read GetAbortBuilding write SetAbortBuilding;
    property BuiltIn: Boolean read FBuiltIn write FBuiltIn;
    property Capabilities: TdxReportLinkCapabilities read GetCapabilities;
    property ComponentPrinter: TCustomdxComponentPrinter read FComponentPrinter write SetComponentPrinter;
    property Controller: TBasedxReportLink read FController write FController;
    property CurrentPage: Integer read FCurrentPage write SetCurrentPage;
    property CurrentPrintStyle: TBasedxPrintStyle read GetCurrentPrintStyle;
    property Data: Pointer read FData write FData;
    property DataSource: TdxReportLinkDataSource read FDataSource write SetDataSource;
    property DesignWindow: TAbstractdxReportLinkDesignWindow read FDesignWindow;
    property HasDesignWindow: Boolean read GetHasDesignWindow;
    property HasPreviewWindow: Boolean read GetHasPreviewWindow;
    property IsAggregated: Boolean read GetIsAggregated;
    property PageCount: Integer read GetPageCount;
    property PreviewWindow: TdxPSCustomPreviewWindow read GetPreviewWindow;
    property RealPrinterPage: TdxPrinterPage read GetRealPrinterPage write SetRealPrinterPage;
    property RealScaleFactor: Integer read GetRealScaleFactor;
    property RebuildNeeded: Boolean read FRebuildNeeded;
    property Renderer: TdxPSReportRenderer read GetRenderer;
    property RenderStage: TdxPSRenderStages read GetRenderStage;
    property ReportCells: TdxReportCells read FReportCells;
    property ReportHeight: Integer read GetReportHeight;
    property ReportWidth: Integer read GetReportWidth;
  published
    property Active: Boolean read FActive write SetActive default False;
    property Caption: string read GetCaption write SetCaption stored False;
    property Component: TComponent read FComponent write SetComponent;
    property Description: string read GetDescription write SetDescription stored False;
    property DateFormat: Integer read getDateFormat write SetDateFormat stored IsDateFormatStored;
    property DateTime: TDateTime read GetDateTime write SetDateTime stored False;
    property DesignerCaption: string read GetDesignerCaption write SetDesignerCaption stored IsDesignerCaptionStored;
    property DesignerHelpContext: THelpContext read FDesignerHelpContext write FDesignerHelpContext default 0;
    property Index: Integer read GetIndex write SetIndex stored False;
    property IsCurrentLink: Boolean read GetIsCurrentLink write SetIsCurrentLink stored False;
    property PageNumberFormat: TdxPageNumberFormat read GetPageNumberFormat write SetPageNumberFormat stored IsPageNumberFormatStored;
    property PDFExportOptions: TdxPSPDFReportExportOptions read FPDFExportOptions write SetPDFExportOptions;
    property PrinterPage: TdxPrinterPage read GetPrinterPage write SetPrinterPage;
    property ReportDocument: TdxPSReportDocument read FReportDocument write SetReportDocument;
    property ReportFootnotes: TdxReportFootnotes read FReportFootnotes write SetReportFootnotes;
    property ReportTitle: TdxReportTitle read FReportTitle write SetReportTitle;
    property ReportTitleMode: TdxReportTitleMode read GetReportTitleMode write SetReportTitleMode stored False default tmOnEveryTopPage;
    property ReportTitleText: string read GetReportTitleText write SetReportTitleText stored False;
    property ShowDesigner: Boolean read FShowDesigner write FShowDesigner stored False;
    property ShowPageFooter: Boolean read GetShowPageFooter write SetShowPageFooter default True;
    property ShowPageHeader: Boolean read GetShowPageHeader write SetShowPageHeader default True;
    property ShowPageRowHeader: Boolean read GetShowPageRowHeader write SetShowPageRowHeader default True;
    property ShrinkToPageWidth: Boolean read GetShrinkToPageWidth write SetShrinkToPageWidth default False;
    property StartPageIndex: Integer read GetStartPageIndex write SetStartPageIndex default 1;
    property StorageName: string read FStorageName write SetStorageName;
    property StyleManager: TdxPrintStyleManager read FStyleManager write SetStyleManager;
    property TimeFormat: Integer read getTimeFormat write SetTimeFormat stored IsTimeFormatStored;
    property AssignedFormatValues: TdxAssignedFormatValues read FAssignedFormatValues write SetAssignedFormatValues stored False;  //must be last

    property OnChangeComponent: TNotifyEvent read FOnChangeComponent write FOnChangeComponent;
    property OnCustomDrawPageFooter: TdxCustomDrawReportLinkHFEvent read FOnCustomDrawPageFooter write FOnCustomDrawPageFooter;
    property OnCustomDrawPageHeader: TdxCustomDrawReportLinkHFEvent read FOnCustomDrawPageHeader write FOnCustomDrawPageHeader;
    property OnCustomDrawReportLinkFootnotes: TdxCustomDrawReportLinkTitleEvent read FOnCustomDrawReportLinkFootnotes write FOnCustomDrawReportLinkFootnotes;
    property OnCustomDrawReportLinkTitle: TdxCustomDrawReportLinkTitleEvent read FOnCustomDrawReportLinkTitle write FOnCustomDrawReportLinkTitle;
    property OnDataSourceChanged: TNotifyEvent read FOnDataSourceChanged write FOnDataSourceChanged;
    property OnDestroy: TNotifyEvent read FOnDestroy write FOnDestroy;
    property OnFilterStyle: TdxFilterStyleEvent read FOnFilterStyle write FOnFilterStyle;
    property OnGetNewReportStorageName: TdxPSGetNewReportStorageNameEvent read FOnGetNewReportStorageName write FOnGetNewReportStorageName;
    property OnMeasureReportLinkFootnotes: TdxMeasureReportLinkTitleEvent read FOnMeasureReportLinkFootnotes write FOnMeasureReportLinkFootnotes;
    property OnMeasureReportLinkTitle: TdxMeasureReportLinkTitleEvent read FOnMeasureReportLinkTitle write FOnMeasureReportLinkTitle;
  end;

  { TdxCompositionLinkItem }

  TdxCompositionLinkItem = class(TCollectionItem)
  private
    FBuiltIn: Boolean;
    FReportLink: TBasedxReportLink;
    FLoadingReportLinkName: string;
    procedure SetReportLink(Value: TBasedxReportLink);
    procedure ReadData(Reader: TReader);
    procedure ReadLinkName(Reader: TReader);
    procedure WriteData(Writer: TWriter);
    procedure WriteLinkName(Writer: TWriter);
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure SetIndex(Value: Integer); override;
    function Composition: TdxCompositionReportLink;
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
    property BuiltIn: Boolean read FBuiltIn write FBuiltIn;
  published
    property ReportLink: TBasedxReportLink read FReportLink write SetReportLink;
  end;

  { TdxCompositionLinkItemList }

  TdxCompositionLinkItemList = class(TStringList)
  private
    function GetObjectEx(Index: Integer): TdxCompositionLinkItem;
    function GetReportLink(Index: Integer): TBasedxReportLink;
  public
    property Objects[Index: Integer]: TdxCompositionLinkItem read GetObjectEx;
    property ReportLinks[Index: Integer]: TBasedxReportLink read GetReportLink;
  end;

  { TdxCompositionLinkItems }

  TdxCompositionLinkItems = class(TCollection)
  private
    FComposition: TdxCompositionReportLink;
    FDontNeedRebuild: Boolean;
    function GetItem(Index: Integer): TdxCompositionLinkItem;
    procedure SetItem(Index: Integer; Value: TdxCompositionLinkItem);
  protected
    procedure CorrectLinksAfterLoadings;
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AComposition: TdxCompositionReportLink);

    function Add: TdxCompositionLinkItem;
    function AddLink(AReportLink: TBasedxReportLink): TdxCompositionLinkItem;
    procedure DeleteItemsByLink(AReportLink: TBasedxReportLink);
    procedure DeleteNonBuiltIns;
    procedure GetLinkEntries(AReportLink: TBasedxReportLink; AList: TList); // returns list of Items
    function IndexOfLink(AReportLink: TBasedxReportLink): Integer;          // returns first entry for AReportLink in Items
    function IsLinkComposable(AReportLink: TBasedxReportLink): Boolean;
    function LinkExists(AReportLink: TBasedxReportLink): Boolean;
    function NextAssignedItem(AnItem: TdxCompositionLinkItem): TdxCompositionLinkItem;
    function NonBuiltInsExists: Boolean;
    function PrevAssignedItem(AnItem: TdxCompositionLinkItem): TdxCompositionLinkItem;

    property Composition: TdxCompositionReportLink read FComposition;
    property Items[Index: Integer]: TdxCompositionLinkItem read GetItem write SetItem; default;
  end;

  { TdxPSCompositionReportRenderRowInfo }

  TdxPSCompositionReportRenderRowInfo = class(TObject)
  private
    FComposedLinks: TList;
    FItem: TdxCompositionLinkItem;
    FPageAbsoluteIndex: Integer;
    FPageIndex: Integer;

    function GetComposedLink(Index: Integer): TBasedxReportLink;
    function GetComposedLinksCount: Integer;
    function GetPageCount: Integer;
    function GetReportLink: TBasedxReportLink;
    function GetRenderInfo: TdxPSReportRenderInfo;
  protected
    procedure CalculateContentInfo(AReportLink: TBasedxReportLink; out AOffset, AHeight: Integer); virtual;
  public
    constructor Create(AItem: TdxCompositionLinkItem; APageIndex, APageAbsoluteIndex: Integer); virtual;
    destructor Destroy; override;
    function CalculateContentAreaHeight(AReportLink: TBasedxReportLink): Integer;
    function CalculateContentOffset(AReportLink: TBasedxReportLink): Integer;
    function CanCompose(AReportLink: TBasedxReportLink): Boolean;
    function Compose(AReportLink: TBasedxReportLink): Boolean;
    function ContainsPage(APageIndex: Integer): Boolean;
    function GetPageIndexRelativeToReportLink(APageIndex: Integer): Integer;
    function IsPartOfComposition(AReportLink: TBasedxReportLink): Boolean;
    //
    property ComposedLinks[Index: Integer]: TBasedxReportLink read GetComposedLink;
    property ComposedLinksCount: Integer read GetComposedLinksCount;
    property Item: TdxCompositionLinkItem read FItem;
    property PageAbsoluteIndex: Integer read FPageAbsoluteIndex;
    property PageCount: Integer read GetPageCount;
    property PageIndex: Integer read FPageIndex;
    property RenderInfo: TdxPSReportRenderInfo read GetRenderInfo;
    property ReportLink: TBasedxReportLink read GetReportLink;
  end;

  { TdxPSCompositionReportRenderInfo }

  TdxPSCompositionReportRenderInfo = class(TdxPSReportRenderInfo)
  private
    FNonEmptyPageCount: Integer;
    FPageColCount: Integer;
    FPageRowCount: Integer;
    FRowInfoList: TcxObjectList;

    function GetRowInfo(Index: Integer): TdxPSCompositionReportRenderRowInfo;
    function GetRowInfoCount: Integer;
    function GetReportLink: TdxCompositionReportLink;
   protected
    function AddRowInfo(AItem: TdxCompositionLinkItem; AStartPageIndex: Integer;
      AStartPageAbsoluteIndex: Integer): TdxPSCompositionReportRenderRowInfo;
    function CalculateNonEmptyPageCount: Integer; virtual;
    function CalculatePageColCount: Integer; virtual;
    function CalculatePageRowCount: Integer; virtual;
    function GetItems: TdxCompositionLinkItemList;

    procedure DoCalculate; override;
    procedure Refresh; override;

    function GetNonEmptyPageCount: Integer; override;
    function GetPageColCount: Integer; override;
    function GetPageRowCount: Integer; override;
  public
    constructor Create(AReportLink: TBasedxReportLink); override;
    destructor Destroy; override;
    function FindRowInfo(AAbsolutePageIndex: Integer; out ARowInfo: TdxPSCompositionReportRenderRowInfo): Boolean; overload;
    function FindRowInfo(AReportLink: TBasedxReportLink; APageIndex: Integer; out ARowInfo: TdxPSCompositionReportRenderRowInfo): Boolean; overload;
    function GetCompositionInfo(APageIndex: Integer; out ARowInfo: TdxPSCompositionReportRenderRowInfo): Boolean; override;

    property ReportLink: TdxCompositionReportLink read GetReportLink;
    property RowInfo[Index: Integer]: TdxPSCompositionReportRenderRowInfo read GetRowInfo;
    property RowInfoCount: Integer read GetRowInfoCount;
  end;

  { TdxPSCompositionReportRenderer }

  TdxPSCompositionReportRenderer = class(TdxPSReportRenderer)
  private
    function GetRenderInfo: TdxPSCompositionReportRenderInfo;
    function GetReportLink: TdxCompositionReportLink;
  public
    procedure RenderPageEx(ACanvas: TdxPSReportRenderCustomCanvas;
      const APageBounds: TRect; APageIndex, AContinuousPageIndex: Integer;
      AZoomFactor: Integer); override;
    //
    property RenderInfo: TdxPSCompositionReportRenderInfo read GetRenderInfo;
    property ReportLink: TdxCompositionReportLink read GetReportLink;
  end;

  { TdxPSReportCompositionDocument }

  TdxPSReportCompositionDocument = class(TdxPSReportDocument)
  private
    function GetReportLink: TdxCompositionReportLink;
  public
    function DefaultDescription: string; override;
    property ReportLink: TdxCompositionReportLink read GetReportLink;
  end;

  TdxCompositionReportLinkEvent = procedure(Sender: TdxCompositionReportLink;
    AItem: TdxCompositionLinkItem) of object;

  TdxCompositionState = (csRebuildReportLink);
  TdxCompositionStates = set of TdxCompositionState;

  TdxCompositionOption = (coCanEdit, coShowDescription);
  TdxCompositionOptions = set of TdxCompositionOption;

  { TdxCompositionReportLink }

  TdxCompositionReportLink = class(TBasedxReportLink)
  private
    FCompositionState: TdxCompositionStates;
    FContinuousPageIndexes: Boolean;
    FDesignerOptions: TdxCompositionOptions;
    FInvalidatedLinks: TList;
    FItems: TdxCompositionLinkItems;
    FStartEachItemFromNewPage: Boolean;

    FOnAfterBuildReport: TdxCompositionReportLinkEvent;
    FOnBeforeBuildReport: TdxCompositionReportLinkEvent;

    function GetRenderer: TdxPSCompositionReportRenderer;
    function GetRenderInfo: TdxPSCompositionReportRenderInfo;
    function GetReportDocument: TdxPSReportCompositionDocument;
    procedure SetItems(Value: TdxCompositionLinkItems);
    procedure SetReportDocument(Value: TdxPSReportCompositionDocument);
    procedure SetStartEachItemFromNewPage(AValue: Boolean);

    procedure ActivateLink(AReportLink: TBasedxReportLink);
    procedure UpdateComposition(AUpdateCodes: TdxPrinterPageUpdateCodes);
  protected
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;

    procedure ConstructReport(AReportCells: TdxReportCells); override;
    procedure DoCreateReport; override;
    function GetRendererClass: TdxPSReportRendererClass; override;
    function GetRenderInfoClass: TdxPSReportRenderInfoClass; override;
    procedure InternalRestoreDefaults; override;
    procedure InternalRestoreFromOriginal; override;
    procedure ShowPageFooterChanged; override;
    procedure ShowPageHeaderChanged; override;
    procedure StdProcessDataSourceDontPresent; override;

    function GetAllowContinuousPageIndexes: Boolean; override;
    function GetCapabilities: TdxReportLinkCapabilities; override;
    function GetContinuousPageIndexes: Boolean; override;
    function GetRebuildOnPageParamsChange(AUpdateCodes: TdxPrinterPageUpdateCodes): Boolean; override;
    function GetReportHeight: Integer; override;
    function GetReportWidth: Integer; override;
    procedure SetContinuousPageIndexes(Value: Boolean); override;

    procedure DoAfterBuildReport(AItem: TdxCompositionLinkItem); dynamic;
    procedure DoBeforeBuildReport(AItem: TdxCompositionLinkItem); dynamic;

    class function GetReportDocumentClass: TdxPSReportDocumentClass; override;

    property Renderer: TdxPSCompositionReportRenderer read GetRenderer;
    property RenderInfo: TdxPSCompositionReportRenderInfo read GetRenderInfo;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    function DefaultDesignerCaption: string; override;

    class function CanBeUsedAsStub: Boolean; override;
    class function Serializable: Boolean; override;
    function DataProviderPresent: Boolean; override;
    function IsEmptyPage(APageIndex: Integer): Boolean; override;

    procedure GetItems(AStrings: TStrings; AExcludeUnassigned: Boolean);

    property CompositionState: TdxCompositionStates read FCompositionState;
    property ReportDocument: TdxPSReportCompositionDocument read GetReportDocument write SetReportDocument;
  published
    property ContinuousPageIndexes default True;
    property DesignerOptions: TdxCompositionOptions read FDesignerOptions write FDesignerOptions default [coCanEdit, coShowDescription];
    property Items: TdxCompositionLinkItems read FItems write SetItems;
    property StartEachItemFromNewPage: Boolean read FStartEachItemFromNewPage write SetStartEachItemFromNewPage default True;

    property OnAfterBuildReport: TdxCompositionReportLinkEvent read FOnAfterBuildReport write FOnAfterBuildReport;
    property OnBeforeBuildReport: TdxCompositionReportLinkEvent read FOnBeforeBuildReport write FOnBeforeBuildReport;
  end;

  { Report PropertySheets }

  TdxReportLinkDesignWindowState = (dwsInitialize);
  TdxReportLinkDesignWindowStates = set of TdxReportLinkDesignWindowState;

  TAbstractdxReportLinkDesignWindow = class(TCustomdxPSForm)
  strict private
    FApplyed: Boolean;
    FModified: Boolean;
    FPrevKeyPreview: Boolean;
    FReportLink: TBasedxReportLink;

    function GetComponent: TComponent;
    function GetIsDesigning: Boolean;
    function IsCaptionStored: Boolean;
    procedure SetModified(Value: Boolean);
    procedure WMHelp(var message: TWMHelp); message WM_HELP;
  protected
    FState: TdxReportLinkDesignWindowStates;

    function CanApply: Boolean; virtual;
    procedure CreateWnd; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;

    procedure AfterRebuildReport; virtual;
    procedure BeforeRebuildReport; virtual;

    procedure Initialize; virtual;
    procedure LoadStrings; virtual;
    procedure UpdateControlsState; virtual;

    property Applyed: Boolean read FApplyed write FApplyed;
    property Component: TComponent read GetComponent;
  public
    constructor Create(AOwner: TComponent); override;
    function Execute: Boolean; virtual;

    property IsDesigning: Boolean read GetIsDesigning;
    property Modified: Boolean read FModified write SetModified;
    property ReportLink: TBasedxReportLink read FReportLink write FReportLink;
    property State: TdxReportLinkDesignWindowStates read FState;
  published
    property Caption stored IsCaptionStored;
  end;

  { Print Styles }

  TdxPSPrintStyle = class(TBasedxPrintStyle)
  private
    FOnAfterGenerating: TNotifyEvent;
    FOnAfterPrinting: TNotifyEvent;
    FOnBeforeGenerating: TNotifyEvent;
    FOnBeforePrinting: TNotifyEvent;

    procedure AfterGenerating;
    procedure BeforeGenerating;
  protected
    procedure AddStdHFFunctions; virtual;

    procedure DoAfterGenerating; virtual;
    procedure DoAfterPrinting; override;
    procedure DoBeforeGenerating; virtual;
    procedure DoBeforePrinting; override;

    procedure InitializeDefaultStyleGlyph(ABitmap: TBitmap); override;
  public
    constructor Create(AOwner: TComponent); override;

    function DefaultPageFooterText(APart: TdxPageTitlePart): string; override;
    function DefaultStyleCaption: string; override;
  published
    property OnAfterGenerating: TNotifyEvent read FOnAfterGenerating write FOnAfterGenerating;
    property OnAfterPrinting: TNotifyEvent read FOnAfterPrinting write FOnAfterPrinting;
    property OnBeforeGenerating: TNotifyEvent read FOnBeforeGenerating write FOnBeforeGenerating;
    property OnBeforePrinting: TNotifyEvent read FOnBeforePrinting write FOnBeforePrinting;
  end;

  { TdxComponentPrinterThumbnailsOptions }

  TdxComponentPrinterThumbnailsOptions = class(TPersistent)
  strict private
    FDefaultFont: TFont;
    FFont: TFont;
    FPreviewOptions: TdxBasePreviewOptions;
    FShowPageNumbers: Boolean;

    procedure FontChangeHandler(Sender: TObject);
    procedure SetFont(Value: TFont);
    procedure SetShowPageNumbers(Value: Boolean);
  protected
    procedure DoAssign(Source: TdxComponentPrinterThumbnailsOptions); virtual;
    procedure InitializeDefaultFont(AFont: TFont); virtual;
    function IsFontStored: Boolean; virtual;
  public
    constructor Create(APreviewOptions: TdxBasePreviewOptions); virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    function DefaultFont: TFont; virtual;
    procedure RestoreDefaults; virtual;

    property PreviewOptions: TdxBasePreviewOptions read FPreviewOptions;
  published
    property Font: TFont read FFont write SetFont stored IsFontStored;
    property ShowPageNumbers: Boolean read FShowPageNumbers write SetShowPageNumbers default True;
  end;

  { TdxBasePreviewOptions }

  TdxBasePreviewOptions = class(TdxBaseObject)
  strict private
    FEnableOptions: TdxPreviewEnableOptions;
    FSaveZoomPosition: Boolean;
    FShowExplorer: Boolean;
    FThumbnailsOptions: TdxComponentPrinterThumbnailsOptions;
    FVisibleOptions: TdxPreviewVisibleOptions;

    FOnChanged: TNotifyEvent;

    procedure SetEnableOptions(Value: TdxPreviewEnableOptions);
    procedure SetShowExplorer(Value: Boolean);
    procedure SetThumbnailsOptions(Value: TdxComponentPrinterThumbnailsOptions);
    procedure SetVisibleOptions(Value: TdxPreviewVisibleOptions);
  protected
    function CreateThumbnailsOptions: TdxComponentPrinterThumbnailsOptions; virtual;

    procedure Changed;
    procedure DoAssign(Source: TdxBaseObject); override;
    procedure DoRestoreDefaults; override;
    procedure LockUpdate(ALockState: TdxLockState); override;

    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  public
    constructor Create; override;
    destructor Destroy; override;
  published
    property EnableOptions: TdxPreviewEnableOptions read FEnableOptions write SetEnableOptions default dxDefaultPreviewEnableOptions;
    property SaveZoomPosition: Boolean read FSaveZoomPosition write FSaveZoomPosition default True;
    property ShowExplorer: Boolean read FShowExplorer write SetShowExplorer default False;
    property ThumbnailsOptions: TdxComponentPrinterThumbnailsOptions read FThumbnailsOptions write SetThumbnailsOptions;
    property VisibleOptions: TdxPreviewVisibleOptions read FVisibleOptions write SetVisibleOptions default dxDefaultPreviewVisibleOptions;
  end;

  { TdxPreviewWindowThumbnailsOptions }

  TdxPreviewWindowThumbnailsOptions = class(TdxComponentPrinterThumbnailsOptions)
  strict private
    FSize: TdxPSThumbnailsSize;
    FVisible: Boolean;

    procedure SetSize(const Value: TdxPSThumbnailsSize);
    procedure SetVisible(const Value: Boolean);
  protected
    procedure DoAssign(Source: TdxComponentPrinterThumbnailsOptions); override;
  public
    constructor Create(APreviewOptions: TdxBasePreviewOptions); override;
    procedure RestoreDefaults; override;
  published
    property Size: TdxPSThumbnailsSize read FSize write SetSize default tsLarge;
    property Visible: Boolean read FVisible write SetVisible default False;
  end;

  { TdxPreviewWindowOptions }

  TdxPreviewWindowOptions = class(TdxBasePreviewOptions)
  strict private
    function GetThumbnailsOptions: TdxPreviewWindowThumbnailsOptions;
    procedure SetThumbnailsOptions(AValue: TdxPreviewWindowThumbnailsOptions);
  protected
    function CreateThumbnailsOptions: TdxComponentPrinterThumbnailsOptions; override;
  published
    property ThumbnailsOptions: TdxPreviewWindowThumbnailsOptions read GetThumbnailsOptions write SetThumbnailsOptions;
  end;

  { IdxPSPreviewWindowDialog }

  IdxPSPreviewWindowDialog = interface
  ['{C74F707D-13E4-46EB-877C-AF1ADE822944}']
    procedure Initialize;
  end;

  { TdxPSCustomPreviewWindow }

  TdxPreviewWindowClass = class of TdxPSCustomPreviewWindow;
  TdxPSCustomPreviewWindow = class(TcxControl,
  {$IFDEF OLEDRAGANDDROP}
    IDropTarget,
  {$ENDIF}
    IdxPSExplorerContextCommandBuilder)
  strict private
    FComponentPrinter: TCustomdxComponentPrinter;
    FIsDestroying: Boolean;
    FOptions: TdxPreviewWindowOptions;
  {$IFDEF OLEDRAGANDDROP}
    FDraggedFileName: string;
  {$ENDIF}

    procedure OptionsChangeHandler(Sender: TObject);
    function GetReportLink: TBasedxReportLink;
    procedure SetComponentPrinter(AValue: TCustomdxComponentPrinter);
    procedure SetOptions(AValue: TdxPreviewWindowOptions);
  protected
  {$IFDEF OLEDRAGANDDROP}
    { IDropTarget }
    function IDropTarget.DragEnter = IDropTarget_DragEnter;
    function IDropTarget.DragOver = IDropTarget_DragOver;
    function IDropTarget.DragLeave = IDropTarget_DragLeave;
    function IDropTarget.Drop = IDropTarget_Drop;

    function IDropTarget_DragEnter(const DataObj: IDataObject; grfKeyState: Longint; Pt: TPoint; var dwEffect: Longint): HRESULT;  stdcall;
    function IDropTarget_DragLeave: HRESULT; stdcall;
    function IDropTarget_DragOver(grfKeyState: Longint; Pt: TPoint; var dwEffect: Longint): HRESULT;  stdcall;
    function IDropTarget_Drop(const DataObj: IDataObject; grfKeyState: Longint; Pt: TPoint; var dwEffect: Longint): HRESULT;  stdcall;
  {$ENDIF}

    procedure AddExplorerContextCommand(ACommand: TCustomdxPSExplorerContextCommand); virtual; abstract;
    procedure UpdateExplorerContextCommands; virtual; abstract;

    function CreateOptions: TdxPreviewWindowOptions; virtual;
    function GetActivePageIndex: Integer; virtual; abstract;
    function GetBackground: TdxBackground; virtual; abstract;
    function GetExplorerTree: TCustomdxPSExplorerTreeContainer; virtual;
    function GetHFEditPart: TdxPageTitlePart; virtual;
    function GetPageCount: Integer; virtual; abstract;
    function GetState: TdxPSPreviewState; virtual;
    function GetZoomFactor: Integer; virtual; abstract;
    procedure SetActivePageIndex(Value: Integer); virtual; abstract;
    procedure SetBackground(const Value: TdxBackground); virtual; abstract;
    procedure SetHFEditPart(const Value: TdxPageTitlePart); virtual; abstract;
    procedure SetState(const Value: TdxPSPreviewState); virtual; abstract;
    procedure SetZoomFactor(Value: Integer); virtual; abstract;

    procedure AfterComponentPrinterChanged; virtual;
    procedure BeforeComponentPrinterChanged; virtual;
    procedure CheckRebuildReport;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure OptionsChanged; virtual; abstract;
    procedure PaintPage(Sender: TObject; ACanvas: TCanvas; ARect: TRect; APageIndex: Integer); virtual;
    procedure PaintThumbnailPage(Sender: TObject; ACanvas: TCanvas; ARect: TRect; APageIndex: Integer); virtual;
    procedure UpdateCaption;

  {$IFDEF OLEDRAGANDDROP}
    function CanDrop: Boolean; virtual;
    function DoCanAccept: Boolean; virtual;
    procedure DoDrop; virtual;
    //
    property DraggedFileName: string read FDraggedFileName;
  {$ENDIF}
    property IsDestroying: Boolean read FIsDestroying;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeforeDestruction; override;

    procedure GoToFirstPage; virtual; abstract;
    procedure GoToLastPage; virtual; abstract;
    procedure GoToNextPage; virtual; abstract;
    procedure GoToPrevPage; virtual; abstract;

    procedure InitContent; virtual;
    procedure InvalidateContent; virtual;
    procedure InvalidatePage(APageIndex: Integer); virtual;
    procedure InvalidateAllPages; virtual;
    procedure InvalidatePagesContent; virtual;
    procedure InvalidatePagesHeaderContent; virtual;
    procedure InvalidatePagesFooterContent; virtual;

    procedure LoadFromIniFile(AIniFile: TCustomIniFile; const ASectionName: string); overload; virtual;
    procedure LoadFromIniFile(const AFileName: string); overload;
    procedure LoadFromRegistry(const APath: string);
    procedure SaveToIniFile(AIniFile: TCustomIniFile; const ASectionName: string); overload; virtual;
    procedure SaveToIniFile(const AFileName: string); overload;
    procedure SaveToRegistry(const APath: string);

    procedure FullRefresh; virtual; abstract;
    procedure RebuildReport; virtual;
    procedure UpdateControls; virtual;

    function Locked: Boolean; virtual;
    procedure BeginUpdate; virtual;
    procedure CancelUpdate; virtual;
    procedure EndUpdate; virtual;

    property ActivePageIndex: Integer read GetActivePageIndex write SetActivePageIndex;
    property Background: TdxBackground read GetBackground write SetBackground;
    property ComponentPrinter: TCustomdxComponentPrinter read FComponentPrinter write SetComponentPrinter;
    property ExplorerTree: TCustomdxPSExplorerTreeContainer read GetExplorerTree;
    property HFEditPart: TdxPageTitlePart read GetHFEditPart write SetHFEditPart;
    property Options: TdxPreviewWindowOptions read FOptions write SetOptions;
    property PageCount: Integer read GetPageCount;
    property ReportLink: TBasedxReportLink read GetReportLink;
    property State: TdxPSPreviewState read GetState write SetState;
    property ZoomFactor: Integer read GetZoomFactor write SetZoomFactor;
  end;

  { TdxPreviewOptions }

  TdxPreviewOptions = class(TdxBasePreviewOptions)
  strict private
    FCaption: string;
    FDefaultIcon: TIcon;
    FHelpContext: THelpContext;
    FIcon: TIcon;
    FIsBoundsAssigned: Boolean;
    FIsCaptionAssigned: Boolean;
    FIsIconAssigned: Boolean;
    FRect: TRect;
    FSavePosition: Boolean;
    FWindowState: TWindowState;

    function GetCaption: string;
    function GetHelpFile: string;
    function GetIcon: TIcon;
    function GetPosition(index: Integer): Integer;
    function GetRect: TRect;
    function IsBoundsStored: Boolean;
    function IsCaptionStored: Boolean;
    function IsIconStored: Boolean;
    procedure SetCaption(const Value: string);
    procedure SetHelpContext(Value: THelpContext);
    procedure SetHelpFile(const Value: string);
    procedure SetIcon(Value: TIcon);
    procedure SetPosition(Index: Integer; Value: Integer);
    procedure SetRect(Value: TRect);
    procedure SetWindowState(Value: TWindowState);

    procedure ReadBoundsRect(Stream: TStream);
    procedure ReadIsCaptionAssigned(Reader: TReader);
    procedure ReadIsIconAssigned(Reader: TReader);
    procedure WriteBoundsRect(Stream: TStream);
    procedure WriteIsCaptionAssigned(Writer: TWriter);
    procedure WriteIsIconAssigned(Writer: TWriter);
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure DoAssign(Source: TdxBaseObject); override;
    procedure DoRestoreDefaults; override;

    function GetIsIconAssigned: Boolean;
    procedure IconChanged(Sender: TObject); virtual;
    procedure InitializeDefaultIcon(AnIcon: TIcon); virtual;
  public
    constructor Create; override;
    destructor Destroy; override;

    function DefaultCaption: string; virtual;
    function DefaultIcon: TIcon; virtual;
    function DefaultRect: TRect; virtual;

    procedure RestoreOriginalIcon;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer);

    property IsIconAssigned: Boolean read FIsIconAssigned;
    property Rect: TRect read GetRect write SetRect;
  published
    property Caption: string read GetCaption write SetCaption stored IsCaptionStored;
    property HelpContext: THelpContext read FHelpContext write SetHelpContext default 0;
    property HelpFile: string read GetHelpFile write SetHelpFile stored False;
    property Icon: TIcon read GetIcon write SetIcon stored IsIconStored;

    property SavePosition: Boolean read FSavePosition write FSavePosition stored False default True;

    property Left: Integer index 1 read GetPosition write SetPosition stored False;
    property Top: Integer index 2 read GetPosition write SetPosition stored False;
    property Width: Integer index 3 read GetPosition write SetPosition stored False;
    property Height: Integer index 0 read GetPosition write SetPosition stored False;

    property WindowState: TWindowState read FWindowState write SetWindowState default wsNormal;
  end;

  { ComponentPrinter }

  TdxBeforeDesignReportEvent = procedure(Sender: TObject; AReportLink: TBasedxReportLink;
    ADesignWindow: TAbstractdxReportLinkDesignWindow) of object;

  TdxCustomDrawPageEvent = procedure(Sender: TObject; AReportLink: TBasedxReportLink;
    ACanvas: TCanvas; APageIndex: Integer; ARect: TRect; ANom, ADenom: Integer) of object;

  TdxCustomDrawReportTitleEvent = procedure(Sender: TObject; AReportLink: TBasedxReportLink;
    ACanvas: TCanvas; ARect: TRect; ANom, ADenom: Integer;
    var TextAlignX: TcxTextAlignX; var TextAlignY: TcxTextAlignY;
    var AColor: TColor; AFont: TFont; var ADone: Boolean) of object;

  TdxCustomDrawPageHFEvent = procedure(Sender: TObject; AReportLink: TBasedxReportLink;
    ACanvas: TCanvas; APageIndex: Integer; var ARect: TRect; ANom, ADenom: Integer;
    var ADefaultDrawText, ADefaultDrawBackground: Boolean) of object;

  TdxDesignReportEvent = procedure(Sender: TObject; AReportLink: TBasedxReportLink;
    var ADone: Boolean) of object;

  TdxGenerateReportProgressEvent = procedure(Sender: TObject;
    AReportLink: TBasedxReportLink; APercentDone: Double {'##0.00'}) of object;

  TdxGetPrintTitleEvent = procedure(Sender: TObject; AReportLink: TBasedxReportLink;
    var ATitle: string) of object;

  TdxPrintDlgDataEvent = procedure(Sender: TObject; AReportLink: TBasedxReportLink;
    var APrintDlgData: TdxPrintDlgData) of object;

  TdxMeasureReportTitleEvent = procedure(Sender: TObject; AReportLink: TBasedxReportLink;
    var AHeight: Integer) of object;

  TdxNewPageEvent = procedure(Sender: TObject; AReportLink: TBasedxReportLink;
    APageIndex: Integer) of object;

  TdxPageSetupEvent = procedure(Sender: TObject; AReportLink: TBasedxReportLink;
    ADone: Boolean) of object;

  TdxPreviewEvent = procedure(Sender: TObject; AReportLink: TBasedxReportLink) of object;

  TdxPrintDeviceProblemEvent = procedure(Sender: TObject; var ADone: Boolean) of object;

  TdxReportLinkBeforeExportToPDFEvent = procedure (Sender: TObject; AReportLink: TBasedxReportLink;
    const AFileName: string; AOptions: TdxPSPDFReportExportOptions; var AAllow: Boolean) of object;
  TdxReportLinkNotifyEvent = procedure(Sender: TObject; AReportLink: TBasedxReportLink) of object;

  TdxStartPrintEvent = procedure(Sender: TObject; AReportLink: TBasedxReportLink;
    APageCount: Integer) of object;

  TdxCPOption = (cpoAutoRebuildBeforePreview, cpoAutoRebuildBeforePrint,
    cpoGenerateReportProgressEvent, cpoShowHourGlass, cpoDropStorageModeAfterPreview);
  TdxCPOptions = set of TdxCPOption;

  TdxCPState = (cpsBuilding, cpsDesigning, cpsPreviewing, cpsPrinting, cpsPrintDialog,
    cpsPageSetupDialog, cpsDefineStylesDialog, cpsCustomizing, cpsLoading, cpsSaving,
    cpsExplore);
  TdxCPStates = set of TdxCPState;

  TdxPSBuildStage = (bsStart, bsProgress, bsEnd);
  TdxPSPrintStage = (psStart, psProgress, psEnd);

  TdxPSComponentPrinterExplorerChangeNotifier = class(TdxPSExplorerChangeNotifierAdapter)
  private
    FComponentPrinter: TCustomdxComponentPrinter;
  protected
    procedure ItemDataUnloaded(AnItem: TdxPSExplorerItem); override;
  public
    constructor Create(AComponentPrinter: TCustomdxComponentPrinter);
    property ComponentPrinter: TCustomdxComponentPrinter read FComponentPrinter;
  end;

  { TdxPSPrintPageRangeInfo }

  TdxPSPrintPageRangeInfo = class(TObject)
  strict private
    FPageIndexes: TdxPSPageIndexes;
    FPageRanges: TdxPageRanges;

    function GetPageIndex(Index: Integer): Integer;
    function GetPageIndexCount: Integer;
    function GetPageIndexesAsString: string;
    procedure SetPageIndexesAsString(const AValue: string);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Assign(AInfo: TdxPSPrintPageRangeInfo);
    //
    property PageIndex[Index: Integer]: Integer read GetPageIndex;
    property PageIndexCount: Integer read GetPageIndexCount;
    property PageIndexesAsString: string read GetPageIndexesAsString write SetPageIndexesAsString;
    property PageRanges: TdxPageRanges read FPageRanges write FPageRanges;
  end;

  { IdxComponentPrinterListener }

  IdxComponentPrinterListener = interface
  ['{9767046A-9D27-41EE-B72B-89062D09D2CB}']
    procedure BeforeDestroyReport(AReportLink: TBasedxReportLink);
    procedure CurrentLinkChanged(AReportLink: TBasedxReportLink);
    procedure ExplorerChanged(AReportLink: TBasedxReportLink);
    procedure LayoutChanged(AReportLink: TBasedxReportLink);
    procedure PageParamsChanged(AReportLink: TBasedxReportLink);
    procedure PrepareBuildReport(AReportLink: TBasedxReportLink);
    procedure UnprepareBuildReport(AReportLink: TBasedxReportLink);
  end;

  { TdxComponentPrinterListeners }

  TdxComponentPrinterListeners = class(TList<IUnknown>)
  public
    procedure BeforeDestroyReport(AReportLink: TBasedxReportLink);
    procedure CurrentLinkChanged(AReportLink: TBasedxReportLink);
    procedure ExplorerChanged(AReportLink: TBasedxReportLink);
    procedure LayoutChanged(AReportLink: TBasedxReportLink);
    procedure PageParamsChanged(AReportLink: TBasedxReportLink);
    procedure PrepareBuildReport(AReportLink: TBasedxReportLink);
    procedure UnprepareBuildReport(AReportLink: TBasedxReportLink);
  end;

  { TCustomdxComponentPrinter }

  TCustomdxComponentPrinter = class(TcxScalableComponent)
  private
    FAbortBuilding: Boolean;
    FAbortPrinting: Boolean;
    FAutoUpdateDateTime: Boolean;
    FBeepAfterLongOperations: Boolean;
    FCurrentLink: TBasedxReportLink;
    FDateFormat: Integer;
    FExplorer: TCustomdxPSExplorer;
    FExplorerChangeNotifier: TdxPSComponentPrinterExplorerChangeNotifier;
    FExplorerStubLink: TBasedxReportLink;
    FInternalStreaming: Boolean;
    FListeners: TdxComponentPrinterListeners;
    FLongOperationTime: Integer;
    FOptions: TdxCPOptions;
    FPageNumberFormat: TdxPageNumberFormat;
    FPreviewOptions: TdxPreviewOptions;
    FPreviewWindow: TdxPSCustomPreviewWindow;
    FPreviewWindowDesigner: TAbstractdxPreviewWindowDesigner;
    FPreviewWindowForm: TForm;
    FPrintFileList: TStrings;
    FPrintTitle: string;
    FReportLinkDesigner: TAbstractdxReportLinkDesigner;
    FReportLinks: TList;
    FState: TdxCPStates;
    FTimeFormat: Integer;
    FVersion: Integer;

    FOnAddReportLink: TdxReportLinkNotifyEvent;
    FOnAfterPreview: TdxPreviewEvent;
    FOnBeforeDesignReport: TdxBeforeDesignReportEvent;
    FOnBeforeExportToPDF: TdxReportLinkBeforeExportToPDFEvent;
    FOnBeforePreview: TdxPreviewEvent;
    FOnChangeComponent: TdxReportLinkNotifyEvent;
    FOnChangeCurrentLink: TNotifyEvent;
    FOnCustomDrawPage: TdxCustomDrawPageEvent;
    FOnCustomDrawPageFooter: TdxCustomDrawPageHFEvent;
    FOnCustomDrawPageHeader: TdxCustomDrawPageHFEvent;
    FOnCustomDrawReportFootnotes: TdxCustomDrawReportTitleEvent;
    FOnCustomDrawReportTitle: TdxCustomDrawReportTitleEvent;
    FOnDeleteReportLink: TdxReportLinkNotifyEvent;
    FOnDesignReport: TdxDesignReportEvent;
    FOnEndGenerateReport: TdxReportLinkNotifyEvent;
    FOnEndPrint: TdxReportLinkNotifyEvent;
    FOnFinalizePrintDlgData: TdxPrintDlgDataEvent;
    FOnGenerateReportProgress: TdxGenerateReportProgressEvent;
    FOnGetPrintTitle: TdxGetPrintTitleEvent;
    FOnInitializePrintDlgData: TdxPrintDlgDataEvent;
    FOnMeasureReportFootnotes: TdxMeasureReportTitleEvent;
    FOnMeasureReportTitle: TdxMeasureReportTitleEvent;
    FOnNewPage: TdxNewPageEvent;
    FOnPageSetup: TdxPageSetupEvent;
    FOnPrintDeviceBusy: TdxPrintDeviceProblemEvent;
    FOnPrintDeviceError: TdxPrintDeviceProblemEvent;
    FOnStartGenerateReport: TdxReportLinkNotifyEvent;
    FOnStartPrint: TdxStartPrintEvent;

    FEndTime: DWORD;
    FHFTextEntryChooseSubscriber: TdxEventSubscriber;
    FLongOperationCounter: Integer;
    FMemoryStream: TStream;
    FPrintAll: Boolean;
    FSavePrintToFile: Boolean;
    FStartTime: DWORD;
    FWindowHandle: HWND;

    function GetCurrentLinkIndex: Integer;
    function GetExplorerRealStubLink: TBasedxReportLink;
    function GetIsExplorerMode: Boolean;
    function GetLinkCount: Integer;
    function GetPreviewCaption: string;
    function GetReportLink(index: Integer): TBasedxReportLink;
    procedure SetAbortBuilding(Value: Boolean);
    procedure SetAbortPrinting(Value: Boolean);
    procedure SetAutoUpdateDateTime(Value: Boolean);
    procedure SetCurrentLink(Value: TBasedxReportLink);
    procedure SetCurrentLinkIndex(Value: Integer);
    procedure SetDateFormat(Value: Integer);
    procedure SetExplorer(Value: TCustomdxPSExplorer);
    procedure SetExplorerStubLink(Value: TBasedxReportLink);
    procedure SetLongOperationTime(Value: Integer);
    procedure SetPageNumberFormat(Value: TdxPageNumberFormat);
    procedure SetPreviewOptions(Value: TdxPreviewOptions);
    procedure SetPrintFileList(Value: TStrings);
    procedure SetReportLink(index: Integer; Value: TBasedxReportLink);
    procedure SetTimeFormat(Value: Integer);

    function BeginPrintPages(const Source: string; out APageIndexes: TdxPSPageIndexes): Boolean;
    procedure EndPrintPages(var APageIndexes: TdxPSPageIndexes);

    // Built-in Preview Window
    procedure DestroyPreviewWindowForm;
    procedure ReleasePreviewWindowForm;
    procedure SetupPreviewWindowForm;
    procedure ShowPreviewWindowForm(AModal: Boolean);

    procedure FinalizeDefaultPrintDlgData(AReportLink: TBasedxReportLink; var APrintDlgData: TdxPrintDlgData); virtual;
    procedure InitDevModeFromPrinterPageSettings(APrinterPage: TdxPrinterPage);

    function PrintDialog(AReportLink: TBasedxReportLink; var APrintDlgData: TdxPrintDlgData): Boolean;
    procedure PrnDlgPageSetup(Sender: TObject; var ADone: Boolean; APreviewBtnClicked, APrintBtnClicked: PBoolean);

    procedure RaiseBuildingEvent(AReportLink: TBasedxReportLink; const APercentCompleted: Double; AStage: TdxPSBuildStage);
    procedure RaisePrintingEvent(AReportLink: TBasedxReportLink; APageIndex, APageCount: Integer; AStage: TdxPSPrintStage);

    procedure ActivateLink(AReportLink: TBasedxReportLink);
    function CheckLink(Value: TBasedxReportLink): TBasedxReportLink;
    function CreateLink(ALinkClass: TdxReportLinkClass; AComponent: TComponent; AOwner: TComponent): TBasedxReportLink;
    procedure DeactivateLink(AReportLink: TBasedxReportLink);
    procedure InsertLink(Value: TBasedxReportLink);
    procedure MoveLink(ACurIndex, ANewIndex: Integer);
    procedure RemoveLink(Value: TBasedxReportLink);
    procedure ResyncCurrentLink(AIndex: Integer);

    procedure OnHFTextEntryChosen(Sender: TObject; const AEntry: string);
    procedure PreviewOptionsChangeHandler(Sender: TObject);

    { design-time support }
    procedure DesignerModified;
    procedure DesignerUpdate(AnItem: TBasedxReportLink);
    function IsDesigning: Boolean;
    function IsDestroying: Boolean;
    function IsLoading: Boolean;

    procedure WndProc(var Message: TMessage);
  protected
    procedure ChangeScale(M, D: Integer); override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetChildOrder(Child: TComponent; Order: Integer); override;
    procedure SetName(const NewName: TComponentName); override;

    procedure CreatePreviewWindowForm(AReportLink: TBasedxReportLink); virtual;
    procedure DoAddReportLink(AReportLink: TBasedxReportLink); dynamic;
    procedure DoAfterPreview(AReportLink: TBasedxReportLink); dynamic;
    procedure DoBeforeDesignReport(AReportLink: TBasedxReportLink); dynamic;
    procedure DoBeforeDestroyReport(AReportLink: TBasedxReportLink); dynamic;
    function DoBeforeExportToPDF(AReportLink: TBasedxReportLink;
      const AFileName: string; AOptions: TdxPSPDFReportExportOptions): Boolean; dynamic;
    procedure DoBeforePreview(AReportLink: TBasedxReportLink); dynamic;
    procedure DoChangeComponent(AReportLink: TBasedxReportLink); dynamic;
    procedure DoChangeCurrentLink; dynamic;
    procedure DoCustomDrawEntirePage(AReportLink: TBasedxReportLink; ACanvas: TCanvas;
      APageIndex: Integer; ARect: TRect; ANom, ADenom: Integer); virtual;
    procedure DoCustomDrawPageHeaderOrFooter(AReportLink: TBasedxReportLink; AHFObject: TCustomdxPageObject;
      ACanvas: TCanvas; APageIndex: Integer;  R: TRect; var ADefaultDrawText, ADefaultDrawBackground: Boolean;
      APixelsNumerator: Integer = 0); virtual;
    procedure DoCustomDrawReportFootnotes(AReportLink: TBasedxReportLink; ACanvas: TCanvas; ARect: TRect;
      var ATextAlignX: TcxTextAlignX; var ATextAlignY: TcxTextAlignY; var AColor: TColor; AFont: TFont;
      var ADone: Boolean; APixelsNumerator: Integer = 0); virtual;
    procedure DoCustomDrawReportTitle(AReportLink: TBasedxReportLink; ACanvas: TCanvas; ARect: TRect;
      var ATextAlignX: TcxTextAlignX; var ATextAlignY: TcxTextAlignY; var AColor: TColor; AFont: TFont;
      var ADone: Boolean; APixelsNumerator: Integer = 0); virtual;
    procedure DoDeleteReportLink(AReportLink: TBasedxReportLink); dynamic;
    procedure DoDesignReport(AReportLink: TBasedxReportLink; ADone: Boolean); dynamic;
    procedure DoEndPrint(AReportLink: TBasedxReportLink); dynamic;
    procedure DoFinalizePrintDlgData(AReportLink: TBasedxReportLink; var APrintDlgData: TdxPrintDlgData); dynamic;
    procedure DoInitializePrintDlgData(AReportLink: TBasedxReportLink; var APrintDlgData: TdxPrintDlgData); dynamic;
    procedure DoMeasureReportFootnotes(AReportLink: TBasedxReportLink; var AHeight: Integer); virtual;
    procedure DoMeasureReportTitle(AReportLink: TBasedxReportLink; var AHeight: Integer); virtual;
    procedure DoNewPage(AReportLink: TBasedxReportLink; APageIndex: Integer); dynamic;
    procedure DoLayoutChanged(AReportLink: TBasedxReportLink);
    procedure DoPageParamsChanged(AReportLink: TBasedxReportLink);
    procedure DoPageSetup(AReportLink: TBasedxReportLink; ADone: Boolean); dynamic;
    procedure DoPrintDeviceBusy; dynamic;
    procedure DoPrintDeviceError; dynamic;
    procedure DoProgress(AReportLink: TBasedxReportLink; const PercentDone: Double); dynamic;
    procedure DoStartPrint(AReportLink: TBasedxReportLink; FullPageCount: Integer); dynamic;
    procedure DoStartUpdateReport(AReportLink: TBasedxReportLink); dynamic;
    procedure DoEndUpdateReport(AReportLink: TBasedxReportLink); dynamic;
    procedure InitializeDefaultPrintDlgData(AReportLink: TBasedxReportLink; out APrintDlgData: TdxPrintDlgData);
    function GetPrintTitle(AReportLink: TBasedxReportLink): string; dynamic;
    procedure StdProcessPrintDeviceBusy; virtual;
    procedure StdProcessPrintDeviceError; virtual;

    function IsCustomPrintDlgData: Boolean;
    function IsForegroundPreviewWindow: Boolean;
    function IsGenerateReportProgressEvent: Boolean;
    function IsRebuildBeforeOutput(AForceRebuild: Boolean): Boolean;
    function IsRebuildBeforePreview: Boolean;
    function IsRebuildBeforePrint: Boolean;
    function IsShowHourGlass: Boolean;

    procedure PaintPageCore(ACanvas: TCanvas; APageIndex: Integer; AZoomFactor: Integer;
      const APageBounds, AContentBounds: TRect; AReportLink: TBasedxReportLink = nil);
    procedure PaintThumbnailPage(ACanvas: TCanvas; APageIndex: Integer;
      AShowPageNumbers: Boolean; AThumbnailSize: TdxPSThumbnailsSize;
      const APageBounds, AContentBounds: TRect; AReportLink: TBasedxReportLink = nil);
    function PrintPagesAsStringEx(const APages: string; APageNums: TdxPageNumbers;
      ACopyCount: Integer; ACollate: Boolean; AReportLink: TBasedxReportLink = nil): Boolean;

    procedure FormatChanged(AReportLink: TBasedxReportLink);
    procedure PreparePageSetup;
    procedure PrepareBuildReport(AReportLink: TBasedxReportLink);
    procedure PrepareLongOperation;
    procedure PrepareReport(AReportLink: TBasedxReportLink);
    procedure PrintPage(AReportLink: TBasedxReportLink; APageIndex: Integer); virtual;
    procedure UnprepareBuildReport(AReportLink: TBasedxReportLink);
    procedure UnprepareLongOperation;
    procedure UnpreparePageSetup;
    procedure UnprepareReport(AReportLink: TBasedxReportLink);

    { Stream loading }
    procedure AfterLoadFromStream(AStream: TStream);
    procedure BeforeLoadFromStream(AStream: TStream);
    procedure ErrorLoadFromStream(AStream: TStream);
    procedure LoadItselfFromStream(AStream: TStream);
    procedure LoadLinksFromStream(AStream: TStream);
    procedure LoadVersionFromStream(AStream: TStream; var AVersion: Integer);
    procedure PrepareLoadFromStream(AStream: TStream);
    procedure UnprepareLoadFromStream(AStream: TStream);
    { Stream saving}
    procedure PrepareSaveToStream(AStream: TStream);
    procedure SaveItselfToStream(AStream: TStream);
    procedure SaveLinksToStream(AStream: TStream);
    procedure SaveVersionToStream(AStream: TStream);
    procedure UnprepareSaveToStream(AStream: TStream);

    property ExplorerChangeNotifier: TdxPSComponentPrinterExplorerChangeNotifier read FExplorerChangeNotifier;
    property Listeners: TdxComponentPrinterListeners read FListeners;
    property PreviewCaption: string read GetPreviewCaption;

    property OnAfterPreview: TdxPreviewEvent read FOnAfterPreview write FOnAfterPreview;
    property OnBeforeDesignReport: TdxBeforeDesignReportEvent read FOnBeforeDesignReport write FOnBeforeDesignReport;
    property OnBeforeExportToPDF: TdxReportLinkBeforeExportToPDFEvent read FOnBeforeExportToPDF write FOnBeforeExportToPDF;
    property OnBeforePreview: TdxPreviewEvent read FOnBeforePreview write FOnBeforePreview;
    property OnChangeComponent: TdxReportLinkNotifyEvent read FOnChangeComponent write FOnChangeComponent;
    property OnChangeCurrentLink: TNotifyEvent read FOnChangeCurrentLink  write FOnChangeCurrentLink;
    property OnCustomDrawPageFooter: TdxCustomDrawPageHFEvent read FOnCustomDrawPageFooter write FOnCustomDrawPageFooter;
    property OnCustomDrawPageHeader: TdxCustomDrawPageHFEvent read FOnCustomDrawPageHeader write FOnCustomDrawPageHeader;
    property OnCustomDrawReportFootnotes: TdxCustomDrawReportTitleEvent read FOnCustomDrawReportFootnotes write FOnCustomDrawReportFootnotes;
    property OnCustomDrawReportTitle: TdxCustomDrawReportTitleEvent read FOnCustomDrawReportTitle write FOnCustomDrawReportTitle;
    property OnDesignReport: TdxDesignReportEvent read FOnDesignReport write FOnDesignReport;
    property OnMeasureReportFootnotes: TdxMeasureReportTitleEvent read FOnMeasureReportFootnotes write FOnMeasureReportFootnotes;
    property OnMeasureReportTitle: TdxMeasureReportTitleEvent read FOnMeasureReportTitle write FOnMeasureReportTitle;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    function AddComposition: TdxCompositionReportLink;
    function AddEmptyLink(ALinkClass: TdxReportLinkClass): TBasedxReportLink;
    function AddEmptyLinkEx(ALinkClass: TdxReportLinkClass; AOwner: TComponent): TBasedxReportLink;
    function AddLink(AComponent: TComponent): TBasedxReportLink;
    function AddLinkEx(AComponent: TComponent; AOwner: TComponent): TBasedxReportLink;
    procedure AssignReportLinks(Source: TCustomdxComponentPrinter);
    function CreateLinkFromFile(const AFileName: string): TBasedxReportLink;
    function CreateLinkFromStream(AStream: TStream): TBasedxReportLink;
    procedure DeleteAllLinks;
    procedure DeleteLink(AIndex: Integer);
    procedure DestroyReport(AReportLink: TBasedxReportLink = nil);
    function FindLinkByComponent(Value: TComponent; ACanCreate: Boolean = False): TBasedxReportLink;
    procedure GetLinks(AList: TList);
    function IndexOfLink(AReportLink: TBasedxReportLink): Integer; overload;
    function IndexOfLink(const AName: string): Integer; overload;
    function IndexOfLinkByName(const AName: string): Integer;
    function LinkByName(const AName: string): TBasedxReportLink;
    procedure RebuildReport(AReportLink: TBasedxReportLink = nil);

    class function GetNewLinkName(AReportLink: TBasedxReportLink): string;
    class function IsSupportedCompClass(AComponentClass: TClass): Boolean; overload;
    class function IsSupportedCompClass(AComponent: TObject{TComponent}): Boolean; overload;

    { composition support }
    function CurrentCompositionByLink(AReportLink: TBasedxReportLink): TdxCompositionReportLink;
    procedure GetCompositionsByLink(AReportLink: TBasedxReportLink; ACompositions: TList);
    procedure GetItems(AComposition: TdxCompositionReportLink; AStrings: TStrings; AExcludeAssigned: Boolean);
    function IsLinkInComposition(AReportLink: TBasedxReportLink; AComposition: TdxCompositionReportLink): Boolean;
    function IsLinkInCurrentComposition(AReportLink: TBasedxReportLink): Boolean;

    procedure Explore;

    procedure ExportToPDF(AReportLink: TBasedxReportLink = nil); overload;
    procedure ExportToPDF(const AFileName: string; ACanShowDialog: Boolean = True;
      ASettings: TdxPSPDFReportExportOptions = nil; AReportLink: TBasedxReportLink = nil); overload;

    function DesignerExists(AReportLink: TBasedxReportLink = nil): Boolean;
    function DesignerExistsByComponent(AComponent: TComponent): Boolean;
    function DesignReport(AReportLink: TBasedxReportLink = nil): Boolean;

    procedure DrawPageFooter(AReportLink: TBasedxReportLink; APageIndex: Integer;
      ARect: TRect; ATitleParts: TdxPageTitleParts; ADrawBackground: Boolean);
    procedure DrawPageHeader(AReportLink: TBasedxReportLink; APageIndex: Integer;
      ARect: TRect; ATitleParts: TdxPageTitleParts; ADrawBackground: Boolean);

    procedure GetPageColRowCount(out ACol, ARow: Integer; AReportLink: TBasedxReportLink = nil);
    function GetPageCount(AReportLink: TBasedxReportLink = nil): Integer;

    function PageSetup(AReportLink: TBasedxReportLink = nil): Boolean;
    function PageSetupEx(AActivePageIndex: Integer; AShowPreviewBtn, AShowPrintBtn: Boolean;
      out APreviewBtnClicked, APrintBtnClicked: Boolean; AReportLink: TBasedxReportLink = nil): Boolean; overload;

    procedure PaintPage(ACanvas: TCanvas; APageIndex: Integer;
      const APageBounds, AContentBounds: TRect; AReportLink: TBasedxReportLink = nil);

    procedure Preview(AModal: Boolean = True; AReportLink: TBasedxReportLink = nil);
    function PreviewExists: Boolean;

    function Print(AShowDialog: Boolean; APrintDialogData: PdxPrintDlgData; AReportLink: TBasedxReportLink = nil): Boolean;
    procedure PrintEx(APageNums: TdxPageNumbers; ACopies: Integer; ACollate: Boolean; AReportLink: TBasedxReportLink = nil);
    procedure PrintPages(const APageIndexes: TdxPSPageIndexes; AReportLink: TBasedxReportLink = nil);
    procedure PrintPagesEx(const APageIndexes: TdxPSPageIndexes; APageNums: TdxPageNumbers;
      ACopyCount: Integer; ACollate: Boolean; AReportLink: TBasedxReportLink = nil);

    procedure LoadFromFile(const AName: string);
    procedure LoadFromStream(AStream: TStream);
    procedure SaveToFile(const AName: string);
    procedure SaveToStream(AStream: TStream);

    property AbortBuilding: Boolean read FAbortBuilding write SetAbortBuilding;
    property AbortPrinting: Boolean read FAbortPrinting write SetAbortPrinting;
    property AutoUpdateDateTime: Boolean read FAutoUpdateDateTime write SetAutoUpdateDateTime default True;
    property BeepAfterLongOperations: Boolean read FBeepAfterLongOperations write FBeepAfterLongOperations default True;
    property CurrentLink: TBasedxReportLink read FCurrentLink write SetCurrentLink;
    property CurrentLinkIndex: Integer read GetCurrentLinkIndex write SetCurrentLinkIndex;
    property DateFormat: Integer read FDateFormat write SetDateFormat default 0;
    property Explorer: TCustomdxPSExplorer read FExplorer write SetExplorer;
    property ExplorerRealStubLink: TBasedxReportLink read GetExplorerRealStubLink;
    property ExplorerStubLink: TBasedxReportLink read FExplorerStubLink write SetExplorerStubLink;
    property IsExplorerMode: Boolean read GetIsExplorerMode;
    property LinkCount: Integer read GetLinkCount;
    property LongOperationTime: Integer read FLongOperationTime write SetLongOperationTime default 5000; {ms}
    property Options: TdxCPOptions read FOptions write FOptions default [Low(TdxCPOption)..High(TdxCPOption)]; {dxDefaultCPOptions}
    property PageNumberFormat: TdxPageNumberFormat read FPageNumberFormat write SetPageNumberFormat default pnfNumeral;
    property PreviewOptions: TdxPreviewOptions read FPreviewOptions write SetPreviewOptions;
    property PreviewWindow: TdxPSCustomPreviewWindow read FPreviewWindow;
    property PreviewWindowForm: TForm read FPreviewWindowForm;
    property PrintFileList: TStrings read FPrintFileList write SetPrintFileList;
    property PrintTitle: string read FPrintTitle write FPrintTitle;
    property ReportLink[Index: Integer]: TBasedxReportLink read GetReportLink write SetReportLink; default;
    property State: TdxCPStates read FState;
    property TimeFormat: Integer read FTimeFormat write SetTimeFormat default 0;
    property Version: Integer read FVersion write FVersion;

    property PreviewWindowDesigner: TAbstractdxPreviewWindowDesigner read FPreviewWindowDesigner;
    property ReportLinkDesigner: TAbstractdxReportLinkDesigner read FReportLinkDesigner;

    property OnAddReportLink: TdxReportLinkNotifyEvent read FOnAddReportLink write FOnAddReportLink;
    property OnCustomDrawPage: TdxCustomDrawPageEvent read FOnCustomDrawPage write FOnCustomDrawPage;
    property OnDeleteReportLink: TdxReportLinkNotifyEvent read FOnDeleteReportLink write FOnDeleteReportLink;
    property OnEndGenerateReport: TdxReportLinkNotifyEvent read FOnEndGenerateReport write FOnEndGenerateReport;
    property OnEndPrint: TdxReportLinkNotifyEvent read FOnEndPrint write FOnEndPrint;
    property OnFinalizePrintDlgData: TdxPrintDlgDataEvent read FOnFinalizePrintDlgData write FOnFinalizePrintDlgData;
    property OnGenerateReportProgress: TdxGenerateReportProgressEvent read FOnGenerateReportProgress write FOnGenerateReportProgress;
    property OnGetPrintTitle: TdxGetPrintTitleEvent read FOnGetPrintTitle write FOnGetPrintTitle;
    property OnInitializePrintDlgData: TdxPrintDlgDataEvent read FOnInitializePrintDlgData write FOnInitializePrintDlgData;
    property OnNewPage: TdxNewPageEvent read FOnNewPage write FOnNewPage;
    property OnPageSetup: TdxPageSetupEvent read FOnPageSetup write FOnPageSetup;
    property OnPrintDeviceBusy: TdxPrintDeviceProblemEvent read FOnPrintDeviceBusy write FOnPrintDeviceBusy;
    property OnPrintDeviceError: TdxPrintDeviceProblemEvent read FOnPrintDeviceError write FOnPrintDeviceError;
    property OnStartGenerateReport: TdxReportLinkNotifyEvent read FOnStartGenerateReport write FOnStartGenerateReport;
    property OnStartPrint: TdxStartPrintEvent read FOnStartPrint write FOnStartPrint;
  end;

  TdxEnumPagesAsImagesProc = procedure(AComponentPrinter: TCustomdxComponentPrinter;
    AReportLink: TBasedxReportLink; AIndex, APageIndex: Integer;
    const AGraphic: TGraphic; AData: Pointer;
    var AContinue: Boolean) of object;

  TdxExportProgressEvent = procedure(Sender: TCustomdxComponentPrinter;
    AReportLink: TBasedxReportLink; APageCount, AIndex, APageIndex: Integer;
    AData: Pointer) of object;

  TdxExportPrepareGraphicEvent = procedure(Sender: TCustomdxComponentPrinter;
    AReportLink: TBasedxReportLink; const AGraphic: TGraphic;
    AData: Pointer) of object;

  TdxExportGetPageFileNameEvent = procedure(Sender: TCustomdxComponentPrinter;
    AIndex, APageIndex: Integer; var AFileName: string) of object;

  TdxOnFilterComponentEvent = procedure(Sender: TObject; var AComponent: TComponent;
    var ACaption, ADescription: string; var Accept: Boolean) of object;

  TdxOnGetSupportedComponentsEvent = procedure(Sender: TObject; AComponents: TStrings) of object;

  TdxCPCustomizeDlgOption = (cdoShowDescription);
  TdxCPCustomizeDlgOptions = set of TdxCPCustomizeDlgOption;

  { TdxComponentPrinter }

  TdxComponentPrinter = class(TCustomdxComponentPrinter)
  private
    FCustomizeDlgOptions: TdxCPCustomizeDlgOptions;
    FOverWriteAll: Boolean;
    FOverWriteExistingFiles: Boolean;
    FOverWriteFile: Boolean;
    FOnExportGetPageFileName: TdxExportGetPageFileNameEvent;
    FOnExportPrepareGraphic: TdxExportPrepareGraphicEvent;
    FOnExportProgress: TdxExportProgressEvent;
    FOnFilterComponent: TdxOnFilterComponentEvent;
    FOnGetSupportedComponents: TdxOnGetSupportedComponentsEvent;
    procedure WritePageAsImageToDisk(AComponentPrinter: TCustomdxComponentPrinter;
      AReportLink: TBasedxReportLink; AIndex, APageIndex: Integer;
      const AGraphic: TGraphic; AData: Pointer;
      var AContinue: Boolean);
  protected
    function DoFilterComponent(AComponent: TComponent; var ACaption, ADescription: string): Boolean; dynamic;
    function GetSupportedComponents(AComponents: TStrings): Boolean; dynamic;
    procedure GetDefaultExportPageFileName(AIndex, APageIndex: Integer; var AFileName: string); dynamic;
    procedure GetExportPageFileName(AIndex, APageIndex: Integer; var AFileName: string); dynamic;
    procedure InternalLoadDefaultSettigns;
    procedure InternalSaveDefaultSettings;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ShowCustomizeDlg;

    procedure LoadFromIniFile(AIniFile: TCustomIniFile; const ASection: string); overload; virtual;
    procedure LoadFromIniFile(const AFileName: string); overload;
    procedure LoadFromRegistry(const APath: string);
    procedure SaveToIniFile(AIniFile: TCustomIniFile; const ASection: string); overload; virtual;
    procedure SaveToIniFile(const AFileName: string); overload;
    procedure SaveToRegistry(const APath: string);

    procedure EnumPagesAsImages(const APageIndexes: array of Integer;
      AGraphicClass: TGraphicClass; ADrawBackground: Boolean;
      ACallBackProc: TdxEnumPagesAsImagesProc; ACallBackData, AProgressData,
      APrepareData: Pointer;
      AReportLink: TBasedxReportLink = nil);
    procedure SavePagesAsImagesToDisk(const APageIndexes: array of Integer;
      AGraphicClass: TGraphicClass; ADrawBackground: Boolean; const AFileMask: string;
      AProgressData, APrepareData: Pointer;
      AReportLink: TBasedxReportLink = nil);
  published
    property AutoUpdateDateTime;
    property BeepAfterLongOperations;
    property CurrentLink;
    property CustomizeDlgOptions: TdxCPCustomizeDlgOptions read FCustomizeDlgOptions write FCustomizeDlgOptions default [cdoShowDescription];
    property DateFormat;
    property Explorer;
    property ExplorerStubLink;
    property LongOperationTime;
    property Options;
    property OverWriteExistingFiles: Boolean read FOverWriteExistingFiles write FOverWriteExistingFiles default False;
    property PageNumberFormat;
    property PreviewOptions;
    property PrintTitle;
    property TimeFormat;
    property Version;

    property OnAddReportLink;
    property OnAfterPreview;
    property OnBeforeDesignReport;
    property OnBeforeExportToPDF;
    property OnBeforePreview;
    property OnChangeComponent;
    property OnChangeCurrentLink;
    property OnCustomDrawPage;
    property OnCustomDrawPageFooter;
    property OnCustomDrawPageHeader;
    property OnCustomDrawReportTitle;
    property OnDeleteReportLink;
    property OnDesignReport;
    property OnEndGenerateReport;
    property OnEndPrint;
    property OnExportGetPageFileName: TdxExportGetPageFileNameEvent read FOnExportGetPageFileName write FOnExportGetPageFileName;
    property OnExportPrepareGraphic: TdxExportPrepareGraphicEvent read FOnExportPrepareGraphic write FOnExportPrepareGraphic;
    property OnExportProgress: TdxExportProgressEvent read FOnExportProgress write FOnExportProgress;
    property OnFilterComponent: TdxOnFilterComponentEvent read FOnFilterComponent write FOnFilterComponent;
    property OnFinalizePrintDlgData;
    property OnGetPrintTitle;
    property OnGenerateReportProgress;
    property OnGetSupportedComponents: TdxOnGetSupportedComponentsEvent read FOnGetSupportedComponents write FOnGetSupportedComponents;
    property OnInitializePrintDlgData;
    property OnMeasureReportTitle;
    property OnNewPage;
    property OnPageSetup;
    property OnPrintDeviceBusy;
    property OnPrintDeviceError;
    property OnStartGenerateReport;
    property OnStartPrint;
  end;

  TAbstractdxPreviewWindowDesigner = class
  private
    FComponentPrinter: TCustomdxComponentPrinter;
  protected
    procedure Activate; virtual; abstract;
    procedure Modified; virtual; abstract;
  public
    constructor Create(AComponentPrinter: TCustomdxComponentPrinter);
    destructor Destroy; override;

    property ComponentPrinter: TCustomdxComponentPrinter read FComponentPrinter;
  end;

  TAbstractdxReportLinkDesigner = class
  private
    FComponentPrinter: TCustomdxComponentPrinter;
  protected
    procedure Modified; virtual; abstract;
    procedure Update(Item: TBasedxReportLink); virtual; abstract;
  public
    constructor Create(AComponentPrinter: TCustomdxComponentPrinter);
    destructor Destroy; override;

    procedure BeginUpdate; virtual; abstract;
    procedure CancelUpdate; virtual; abstract;
    procedure EndUpdate; virtual; abstract;

    property ComponentPrinter: TCustomdxComponentPrinter read FComponentPrinter;
  end;

  { TdxPSPreviewDialogStyleInfo }

  TdxPSPreviewDialogStyleInfoClass = class of TdxPSPreviewDialogStyleInfo;
  TdxPSPreviewDialogStyleInfo = class(TPersistent)
  public
    class function CreatePreviewWindow: TdxPSCustomPreviewWindow; virtual;
    class function GetName: string; virtual;
    class function GetUnitName: string; virtual;
  end;

  { TdxPSPreviewDialogManager }

  TdxPSPreviewDialogManager = class(TObject)
  private
    FCurrentPreviewDialogStyle: TdxPSPreviewDialogStyle;
    FList: TList;
    function GetCount: Integer;
    function GetCurrentPreviewDialogStyle: TdxPSPreviewDialogStyle;
    function GetCurrentPreviewDialogStyleInfo: TdxPSPreviewDialogStyleInfoClass;
    function GetDefaultPreviewDialogStyleInfo: TdxPSPreviewDialogStyleInfoClass;
    function GetItem(Index: Integer): TdxPSPreviewDialogStyleInfoClass;
    function GetName(Index: Integer): string;
    procedure SetCurrentPreviewDialogStyle(const AValue: TdxPSPreviewDialogStyle);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function CreatePreviewWindow: TdxPSCustomPreviewWindow;
    function FindPreviewDialogStyleInfoByName(const AName: string; out AInfoClass: TdxPSPreviewDialogStyleInfoClass): Boolean;
    procedure PopulatePreviewDialogList(AStrings: TStrings);
    procedure RegisterPreviewDialog(APreviewDialogStyleInfo: TdxPSPreviewDialogStyleInfoClass);
    procedure UnregisterPreviewDialog(APreviewDialogStyleInfo: TdxPSPreviewDialogStyleInfoClass);
    //
    property Count: Integer read GetCount;
    property CurrentPreviewDialogStyle: TdxPSPreviewDialogStyle read GetCurrentPreviewDialogStyle write SetCurrentPreviewDialogStyle;
    property CurrentPreviewDialogStyleInfo: TdxPSPreviewDialogStyleInfoClass read GetCurrentPreviewDialogStyleInfo;
    property DefaultPreviewDialogStyleInfo: TdxPSPreviewDialogStyleInfoClass read GetDefaultPreviewDialogStyleInfo;
    property Items[Index: Integer]: TdxPSPreviewDialogStyleInfoClass read GetItem;
    property Names[Index: Integer]: string read GetName;
  end;

  { TdxReportLinkPrinterPage }

  TdxReportLinkPrinterPage = class(TdxPrinterPage)
  strict private
    FReportLink: TBasedxReportLink;
  protected
    procedure ChangeScale(M: Integer; D: Integer); override;
    function GetOwner: TPersistent; override;
    function GetCurrentDPI: Integer; override;
    function GetSupportsScaling: Boolean; override;
    procedure PageParamsChanged(AUpdateCodes: TdxPrinterPageUpdateCodes); override;
  public
    constructor Create(AReportLink: TBasedxReportLink); reintroduce; virtual;
    property ReportLink: TBasedxReportLink read FReportLink;
  end;

{ This si a routine for fast PrintPreview and(or) Printing an individual component.
  You MUST add "unit" contained "Link" that supports the "AComponent" into the uses
  clause of the your unit }

function dxPrintComponent(AComponent: TComponent; APrintPreview: Boolean = True;
  APrintDialog: Boolean = False; const AReportTitle: string = '';
  const APrintTitle: string = ''): Boolean;

{ Enum Pages as Images routines }

procedure dxPSEnumReportPages(AComponentPrinter: TdxComponentPrinter;
  AReportLink: TBasedxReportLink; const APageIndexes: array of Integer;
  AGraphicClass: TGraphicClass; AnExportBackground: Boolean;
  ACallBackProc: TdxEnumPagesAsImagesProc;
  ACallBackData: Pointer;
  AProgressProc: TdxExportProgressEvent;
  AProgressData: Pointer;
  APrepareGraphicProc: TdxExportPrepareGraphicEvent;
  APrepareData: Pointer);

{ ReportLinks registration routines }

procedure dxPSRegisterReportLink(ALinkClass: TdxReportLinkClass;
  AComponentClass: TComponentClass; ADesignerClass: TdxReportLinkDesignWindowClass);
procedure dxPSUnregisterReportLink(ALinkClass: TdxReportLinkClass;
  AComponentClass: TComponentClass; ADesignerClass: TdxReportLinkDesignWindowClass);
procedure dxPSUnregisterReportLinks(const ALinkClasses: array of TdxReportLinkClass);

function dxPSDesignerClassByCompClass(AComponentClass: TClass): TdxReportLinkDesignWindowClass; overload;
function dxPSDesignerClassByCompClass(AComponent: TObject{TComponent}): TdxReportLinkDesignWindowClass; overload;

function dxPSDesignerClassByLinkClass(ALinkClass: TClass): TdxReportLinkDesignWindowClass; overload;
function dxPSDesignerClassByLinkClass(ALink: TObject{TBasedxReportLink}): TdxReportLinkDesignWindowClass; overload;

function dxPSLinkClassByCompClass(AComponentClass: TClass): TdxReportLinkClass; overload;
function dxPSLinkClassByCompClass(AComponent: TObject{TComponent}): TdxReportLinkClass; overload;
function dxPSLinkClassByCompClassEx(AComponentClass: TClass): TList;

procedure dxPSGetActiveReportLinksList(AClassList: TdxClassList);
procedure dxPSGetReportLinksList(AClassList: TdxClassList);
procedure dxPSGetLinkSupportedComponentsList(AClassList: TdxClassList; ALinkClass: TClass); overload;
procedure dxPSGetLinkSupportedComponentsList(AClassList: TdxClassList; ALink: TObject{TBasedxReportLink}); overload;
procedure dxPSGetSupportedComponentsList(AClassList: TdxClassList);

function dxPSIsSupportedCompClass(AComponentClass: TClass): Boolean; overload;
function dxPSIsSupportedCompClass(AComponent: TObject{TComponent}): Boolean; overload;

function dxPSPreviewDialogManager: TdxPSPreviewDialogManager;

{ Units convertation routines }

function OnePixel: Integer;
function PixelsNumerator: Integer;
function PixelsDenominator: Integer;
function ConvertValue(AValue: Integer): Integer;

{ Helpers }

procedure FixupRect(ACanvas: TdxPSReportRenderCustomCanvas; var R: TRect);
procedure Get3DBorderBrushes(AEdgeStyle: TdxCellEdgeStyle; ASoft: Boolean;
  var AOuterTLBrush, AOuterBRBrush, AInnerTLBrush, AInnerBRBrush: HBRUSH);
procedure Get3DBorderColors(AEdgeStyle: TdxCellEdgeStyle; ASoft: Boolean;
  var AOuterTLColor, AOuterBRColor, AInnerTLColor, AInnerBRColor: TColor);

function dxPSExplorerImages: TCustomImageList;

procedure dxPSStartWait;
procedure dxPSStopWait;

const
  PSTO_DEFAULT_FORMAT = CXTO_AUTOINDENTS or CXTO_PATTERNEDTEXT;

  cemSingle = cemPattern; // obsolete: declared only for backward compatibility
  cesNone = cesRaised;    // obsolete: declared only for backward compatibility

  csAll = [csLeft..csBottom];
  csTopLeft = [csLeft, csTop];
  csBottomRight = [csRight, csBottom];
  csLeftRight = [csLeft, csRight];
  csTopBottom = [csTop, csBottom];

  tlpAll: TdxPSTreeLineParts = [tlpTop, tlpRight, tlpBottom];

  dxAlignment: array[TcxTextAlignX] of TAlignment = (taLeftJustify, taCenter, taRightJustify, taCenter, taCenter);
  dxCalcFormat: array[Boolean] of UINT =
    (CXTO_CALCRECT or CXTO_AUTOINDENTS {or CXTO_EXPANDTABS} or CXTO_CHARBREAK,
     CXTO_CALCRECT or CXTO_AUTOINDENTS or CXTO_EXPANDTABS or CXTO_CHARBREAK or CXTO_WORDBREAK);

  dxDrawTextTextAlignX: array[TcxTextAlignX] of UINT = (DT_LEFT, DT_CENTER, DT_RIGHT, DT_CENTER, DT_CENTER);
  dxDrawTextTextAlignY: array[TcxTextAlignY] of UINT = (DT_TOP, DT_VCENTER, DT_BOTTOM, DT_TOP);

  dxImageLayout: array[TAlignment] of TdxImageLayout = (ilImageCenterLeft, ilImageCenterRight, ilImageCenterCenter);
  dxMultilineTextAlignY: array[Boolean] of TcxTextAlignY = (taCenterY, taTop);
  dxTextAlignX: array[TAlignment] of TcxTextAlignX = (taLeft, taRight, taCenterX);
  dxTextAlignY: array[TTextLayout] of TcxTextAlignY = (taTop, taCenterY, taBottom);

  //TdxCellEdgeStyle = (cesRaised, cesSunken)
  dxCellBorderClassMap: array[TdxCellEdgeStyle, Boolean] of TdxPSCellBorderClass =
   ((TdxPSCellRaisedBorder, TdxPSCellRaisedSoftBorder),
    (TdxPSCellSunkenBorder, TdxPSCellSunkenSoftBorder));
  dxDefaultCPOptions =
    [cpoAutoRebuildBeforePreview, cpoAutoRebuildBeforePrint,
     cpoGenerateReportProgressEvent, cpoShowHourGlass, cpoDropStorageModeAfterPreview];

  // Stretch, Proportional, Center
  dxGraphicDrawModeMap: array[Boolean, Boolean, Boolean] of TdxGraphicDrawMode = (
    ((gdmNone, gdmCenter), (gdmProportional, gdmCenterProportional)),
    ((gdmStretch, gdmStretch), (gdmStretchProportional, gdmCenterAndStretchProportional)));

  dxThumbnailsZoomFactors: array[TdxPSThumbnailsSize] of Integer = (5, 10);

  dxDefaultBkColor = clWhite;
  dxDefaultBreakByChars = True;
  dxDefaultCellSides = csAll;
  dxDefaultCheckFlatBorder = True;
  dxDefaultCheckPos = ccpCenter;
  dxDefaultColor = clWhite;
  dxDefaultContentColor = dxDefaultColor;
  dxDefaultCrossSignCrossSize = 9; // pixels
  dxDefaultEdgeMode = cemPattern;
  dxDefaultEdge3DSoft = True;
  dxDefaultEdge3DStyle = cesRaised;
  dxDefaultEndEllipsis = False;
  dxDefaultExpandButtonBorderColor = clBlack;
  dxDefaultFixedColor = clBtnFace; //clSilver;
  dxDefaultFixedTransparent = False;
  dxDefaultFont: array[0..LF_FACESIZE - 1] of Char = 'Times New Roman';
  dxDefaultGridLineColor = clBlack;
  dxDefaultGroupBorderColor = clBtnFace;
  dxDefaultGroupCaptionColor = clBtnFace;
  dxDefaultGroupCaptionSeparatorColor = clBtnFace;
  dxDefaultGroupColor = clBtnFace;
  dxDefaultHidePrefix = False;
  dxDefaultMultiline = False;
  dxDefaultShadowColor = clBlack;
  dxDefaultShadowDepth = 3;
  dxDefaultSortOrder = csoNone;
  dxDefaultPreventLeftTextExceed = True;
  dxDefaultPreventTopTextExceed = True;
  dxDefaultReportGroupCaptionIndent = 5;
  dxDefaultReportGroupLookAndFeel: TdxPSReportGroupLookAndFeelClass = TdxPSReportGroupStandardLookAndFeel;
  dxDefaultTextAlignX = taLeft;
  dxDefaultTextAlignY = taCenterY;
  dxDefaultTransparent = True;
  dxDefaultTreeLineColor = clGray;

  dxPSDefaultFontCharSet = DEFAULT_CHARSET;
  dxPSDefaultFontColor = clBlack;
  dxPSDefaultFontName = 'Times New Roman';
  dxPSDefaultFontSize = 8;
  dxPSDefaultFontStyle = [];

  dxPSDefaultPreviewThumbnailsFontColor = clBlue;
  dxPSDefaultPreviewThumbnailsFontName = 'Tahoma';
  dxPSDefaultPreviewThumbnailsFontSize = 48;
  dxPSDefaultPreviewThumbnailsFontStyle = [];

  dxPSDefaultReportTitleFontColor = clBlack;
  dxPSDefaultReportTitleFontName = dxPSDefaultFontName;
  dxPSDefaultReportTitleFontSize = 14;
  dxPSDefaultReportTitleFontStyle = [fsBold];

  dxRadioGroupInterColumnsMinSpace = 1;
  dxRadioGroupInterRowsMinSpace = 1;
  dxRadioGroupBoundsIndent = 2;

  dxPSReportFileLongExtension = 'ExpressPrinting System-Report'; //Don't Localize
  dxPSReportFileShortExtension = 'rps';                          //Don't Localize

  dxSortMarkRgnSize = 16;
  dxSortMarkWidth = 8;
  dxSortMarkHeight = 7;

  sdxDocumentCaptionSeparator = '-';

var
  FDontPrintTransparentImages: Boolean = False; // affects only on Printout
  FEmulatePrintCopies: Boolean = False;
  FSmoothlyStretchImages: Boolean = True;
  FUnitFinalized: Boolean;
  FUnitsPerInch: Integer = 600;

  iiExplorerFolderCollapsed: Integer = -1;
  iiExplorerFolderExpanded: Integer = -1;
  iiExplorerItem: Integer = -1;
  iiExplorerItemHasInvalidData: Integer = -1;
  iiDriveTypes: array[TdxDriveType] of Integer = (-1, -1, -1, -1, -1, -1, -1);

  dxTextSpace: Integer = 2;
  dxComponentPrintersList: TList;

  dxPSIndentBetweenComposedPages: Integer = 20;

  // you need to disable the option below only if printing a file
  //(such as Metafile, PDF, etc.) produces artifacts
  FUseIsotropicMode: Boolean = True;

function dxPSCalculateImageBounds(const R: TRect;
  AWidth, AHeight: Integer; ADrawMode: TdxGraphicDrawMode): TRect;
function dxCellSidesToBorders(ASides: TdxCellSides): TcxBorders;
procedure dxPSScaleFont(AFont: TFont; AFontSize, ATargetDPI: Integer); overload;
procedure dxPSScaleFont(AFont: TFont; AFontSize, ATargetDPI, ASourceDPI: Integer); overload;
implementation

uses
  Themes, UxTheme, Variants, dxCoreGraphics, dxPSCPDsg,
 {$IFDEF USEJPEGIMAGE}
  Jpeg,
 {$ENDIF}
  TypInfo, Registry, Consts, CommCtrl, Dialogs, ShlObj, ShellAPI, dxPSRes,
  dxPSImgs, dxPSUtl, dxPSEvnt, dxPSEdgePatterns, dxfmDTFmt, dxfmPNFmt,
  dxPSfmTtl, dxPSfmFootnotes, dxfmChFN, dxPrnDev, dxPSPgsMnuBld,
  dxPSfmReportProperties, dxPSXplorerTreeView, dxPSCompsProvider,
  dxPSfmCompositionDsg, dxPSPrVwStd, Types, ComObj, dxPSPDFExport,
  dxPSHFLibrary, Math, dxTypeHelpers, cxLookAndFeels, dxDPIAwareUtils;

const
  FExplorerImages: TcxImageList = nil;

  // Image Indexes of Drive Types in ExplorerImages
  DriveTypeImageIndexMap: array[TdxDriveType] of Integer = (-1, -1, 7, 8, 9, 11, 12);

  // Page "Update Codes" that force us to Rebuild Report or Recalculate ViewInfos
  SignificantPrinterPageUpdateCodes: TdxPrinterPageUpdateCodes = [ucMarginLeft, ucMarginTop, ucMarginRight, ucMarginBottom, ucScale];

  MaxGlyphCount = 6;  // Max Glyph count used in TdxCustomReportCellCheckImage
  PtPerInch = 72;     // Typographic Point per Inch

  // Consts used in TCustomdxReportCellImageContainer to recognize Image kind saved in Stream
  imstImageList = 0;
  imstShellLargeImageList = 1;
  imstShellSmallImageList = 2;
  imstImage = 3;

  // New ItemLink Name Template
  sdxNewLinkNameTemplate = 'Link%d';                        // Don't Localize

  // Const used to store/load various Report Data to/from Windows Registry or Stream(File)
  sdxNil = 'nil';                                           // Don't Localize
  sdxFilePort = 'FILE:';                                    // Don't Localize

  sdxAssignedDateFormat = 'OwnDateFormat';                  // Don't Localize
  sdxAssignedTimeFormat = 'OwnTimeFormat';                  // Don't Localize
  sdxAssignedPageNumberFormat = 'OwnPageNumberFormat';      // Don't Localize
  sdxAutoUpdateDateTime = 'AutoUpdateDateTime';             // Don't Localize
  sdxDateFormat = 'DateFormat';                             // Don't Localize
  sdxPageNumberFormat = 'PageNumberFormat';                 // Don't Localize
  sdxStartPageIndex = 'StartPageIndex';                     // Don't Localize
  sdxTimeFormat = 'TimeFormat';                             // Don't Localize
  dxFormatTextAdjustFont = Integer($80000000);

  // Don't Localize: PDF Settings
  sdxCompressStreams = 'CompressStreams';
  sdxEmbedFonts = 'EmbedFonts';
  sdxJPEGQuality = 'JPEGQuality';
  sdxOpenDocumentAfterExport = 'OpenDocumentAfterExport';
  sdxUseCIDFonts = 'UseCIDFonts';
  sdxUseJPEGCompression = 'UseJPEGCompression';
  sdxPDFSecurityAllowComments = 'AllowComments';
  sdxPDFSecurityAllowContentCopy = 'AllowContentCopy';
  sdxPDFSecurityAllowContentEdit = 'AllowContentEdit';
  sdxPDFSecurityAllowDocumentAssemble = 'AllowDocumentAssemble';
  sdxPDFSecurityAllowPrint = 'AllowPrint';
  sdxPDFSecurityAllowPrintHiResolution = 'AllowPrintHiResolution';
  sdxPDFSecurityKeyLength = 'KeyLength';

  sdxPrintDlgFilesPath = 'PrintDialogFiles';       // Don't Localize
  sdxPDFSettingSectionName = 'PDFSettings';               // Don't Localize

type
  TdxPDFSecurityActionMapEntry = packed record
    Action: TdxPSPDFDocumentAction;
    Name: string;
  end;

  TCustomPanelAccess = class(TCustomPanel);
  TdxPrinterPageAccess = class(TdxPrinterPage);
  TCustomdxPageObjectAccess = class(TCustomdxPageObject);

  TdxLinkList = class;

  { TdxReportLinkRegItem }

  TdxReportLinkRegItem = class(TObject)
  private
    FLinkList: TdxLinkList;
  protected
    property LinkList: TdxLinkList read FLinkList;
  public
    ComponentClass: TComponentClass;
    DesignerClass: TdxReportLinkDesignWindowClass;
    LinkClass: TdxReportLinkClass;
    constructor Create(ALinkClass: TdxReportLinkClass; AComponentClass: TComponentClass;
      ADesignerClass: TdxReportLinkDesignWindowClass);
    destructor Destroy; override;

    function IsEqual(ALinkClass: TdxReportLinkClass; AComponentClass: TComponentClass = nil;
      ADesignerClass: TdxReportLinkDesignWindowClass = nil): Boolean;
    function IsLastLinkClass: Boolean;
  end;

  { TdxLinkList }

  TdxLinkList = class(TObjectList)
  private
    function GetItem(Index: Integer): TdxReportLinkRegItem;
  protected
    function GetLinkClassCount(ALinkClass: TdxReportLinkClass): Integer;
  public
    procedure Add(ALinkClass: TdxReportLinkClass; AComponentClass: TComponentClass;
      ADesignerClass: TdxReportLinkDesignWindowClass);

    function Find(out AIndex: Integer; ALinkClass: TdxReportLinkClass; AComponentClass: TComponentClass = nil;
      ADesignerClass: TdxReportLinkDesignWindowClass = nil): Boolean; overload;
    function Find(ALinkClass: TdxReportLinkClass; AComponentClass: TComponentClass = nil;
      ADesignerClass: TdxReportLinkDesignWindowClass = nil): Boolean; overload;

    function FindDesignerByLink(ALinkClass: TClass): TdxReportLinkDesignWindowClass; overload;
    function FindDesignerByLink(ALink: TObject{TBasedxReportLink}): TdxReportLinkDesignWindowClass; overload;

    function FindLinkByComponent(AComponentClass: TClass): TdxReportLinkClass; overload;
    function FindLinkByComponent(AComponent: TObject{TComponent}): TdxReportLinkClass; overload;
    function FindLinksByComponent(AComponentClass: TClass): TList; overload;

    procedure GetLinks(AClassList: TdxClassList; AExcludeInactive: Boolean = True);
    procedure GetSupportedComponents(AClassList: TdxClassList; ALinkClass: TClass = nil); overload;
    procedure GetSupportedComponents(AClassList: TdxClassList; ALink: TObject{TBasedxReportLink}); overload;

    procedure UnregisterLink(ALinkClass: TdxReportLinkClass; AComponentClass: TComponentClass = nil;
      ADesignerClass: TdxReportLinkDesignWindowClass = nil);
    procedure UnregisterLinks(const ALinkClasses: array of TdxReportLinkClass);

    property Items[Index: Integer]: TdxReportLinkRegItem read GetItem;
  end;

  { TdxPSReportCellTreeLineViewInfo }

  TdxPSReportCellTreeLineViewInfo = class(TObject)
  private
    FButtonRect: TRect;
    FCellButton: TdxReportCellExpandButton;
    FMode: TdxPSTreeLineMode;
    FParts: TdxPSTreeLineParts;
    FRightRect: TRect;
    FVerticalRect: TRect;
    function GetCellSides: TdxCellSides;
    function GetLineThickness: Integer;
    function GetRenderer: TdxPSReportRenderer;
    function GetTreeLineStyle: TdxPSTreeLineStyle;
    function GetUnitsPerPixel: Integer;
  protected
    function CalculateParts(AMode: TdxPSTreeLineMode): TdxPSTreeLineParts;
    function CreateTreeLineRegion(const R: TRect; AIsVertical: Boolean): TcxRegion;
    procedure CalculateRects(const ABounds, AButtonRect: TRect);
  public
    constructor Create(AMode: TdxPSTreeLineMode; ACellButton: TdxReportCellExpandButton); virtual;
    procedure Draw(ACanvas: TdxPSReportRenderCustomCanvas); virtual;
    //
    property ButtonRect: TRect read FButtonRect;
    property CellButton: TdxReportCellExpandButton read FCellButton;
    property CellSides: TdxCellSides read GetCellSides;
    property LineThickness: Integer read GetLineThickness;
    property Mode: TdxPSTreeLineMode read FMode;
    property Parts: TdxPSTreeLineParts read FParts;
    property Renderer: TdxPSReportRenderer read GetRenderer;
    property RightRect: TRect read FRightRect;
    property TreeLineStyle: TdxPSTreeLineStyle read GetTreeLineStyle;
    property UnitsPerPixel: Integer read GetUnitsPerPixel;
    property VerticalRect: TRect read FVerticalRect;
  end;

  { TdxPSExplorerTreeContainerFactory }

  TdxPSExplorerTreeContainerFactory = class(TdxCustomClassFactory)
  private
    function GetActiveTreeContainerClass: TCustomdxPSExplorerTreeContainerClass;
  public
    class function Instance: TdxPSExplorerTreeContainerFactory; reintroduce; overload;
    property ActiveTreeContainerClass: TCustomdxPSExplorerTreeContainerClass read GetActiveTreeContainerClass;
  end;

  { TdxPSExplorerTreeBuilderFactory }

  TdxPSExplorerTreeBuilderFactory = class(TdxCustomClassFactory)
  private
    // because of CLR :-(
    function GetActiveBuilderClass: TdxPSExplorerTreeBuilderClass;
  public
    class function Instance: TdxPSExplorerTreeBuilderFactory; reintroduce; overload;
    property ActiveBuilderClass: TdxPSExplorerTreeBuilderClass read GetActiveBuilderClass;
  end;

  { TdxPSReaderFactory }

  TdxPSReaderFactory = class(TdxCustomClassFactory)
  private
    function GetActualReaderClass: TdxPSDataReaderClass;
    function GetReaderClass(Version: Integer): TdxPSDataReaderClass;
  public
    class function Instance: TdxPSReaderFactory; reintroduce; overload;

    property ActualReaderClass: TdxPSDataReaderClass read GetActualReaderClass;
    property ReaderClasses[Version: Integer]: TdxPSDataReaderClass read GetReaderClass; default;
  end;

  { TdxPSWriterFactory }

  TdxPSWriterFactory = class(TdxCustomClassFactory)
  private
    function GetActualWriterClass: TdxPSDataWriterClass;
    function GetWriterClass(Version: Integer): TdxPSDataWriterClass;
  public
    class function Instance: TdxPSWriterFactory; reintroduce; overload;

    property ActualWriterClass: TdxPSDataWriterClass read GetActualWriterClass;
    property WriterClasses[Version: Integer]: TdxPSDataWriterClass read GetWriterClass; default;
  end;

  { TdxPSReportLinkList }

  TdxPSReportLinkList = class(TList)
  private
    function GetItem(Index: Integer): TBasedxReportLink;
  public
    property Items[Index: Integer]: TBasedxReportLink read GetItem; default;
  end;

  { TdxPSPageDelimitersCalculator }

  TdxPSPageDelimitersCalculator = class(TObject)
  private
    FPageDelimiters: TList;
    FReportContentSize: Integer;
    FReportDelimiters: TList;
    FReportRenderInfo: TdxPSReportRenderInfo;
    function GetPageDelimiters(Index: Integer): Integer;
    function GetReportDelimiters(Index: Integer): Integer;
  protected
    function CalculatePageContentSize(APageIndex: Integer): Integer; virtual;
    function IsHardDelimiter(AValue: Integer): Boolean; virtual;
    function ScaleValue(AValue: Integer; AScaleIn: Boolean = True): Integer;
    procedure AddPageDelimiter(AValue: Integer);
    procedure BeginCalculate; virtual;
    procedure CalculateDelimiters;
    procedure EndCalculate(ACurDelimiter: Integer);
    //
    property PageDelimiters[Index: Integer]: Integer read GetPageDelimiters;
    property PageDelimitersList: TList read FPageDelimiters;
    property ReportContentSize: Integer read FReportContentSize;
    property ReportDelimiters[Index: Integer]: Integer read GetReportDelimiters;
    property ReportDelimitersList: TList read FReportDelimiters;
  public
    constructor Create(AReportRenderInfo: TdxPSReportRenderInfo); virtual;
    class procedure Calculate(AReportRenderInfo: TdxPSReportRenderInfo);
    //
    property ReportRenderInfo: TdxPSReportRenderInfo read FReportRenderInfo;
  end;

  { TdxPSPageVerticalDelimitersCalculator }

  TdxPSPageVerticalDelimitersCalculator = class(TdxPSPageDelimitersCalculator)
  protected
    function CalculatePageContentSize(APageIndex: Integer): Integer; override;
    function IsHardDelimiter(AValue: Integer): Boolean; override;
    procedure BeginCalculate; override;
  end;

  { TdxPSStorageVersionCompability }

  TdxPSStorageVersionCompability = class
  public
    class procedure UpdateReportCells(AReportCells: TdxReportCells; AReader: TdxPSDataReader); static;
    class procedure UpdateReportRenderInfo(AReportRenderInfo: TdxPSReportRenderInfo; AReader: TdxPSDataReader); static;
  end;

var
  FActiveReportLinks: TdxPSReportLinkList;
  FLinkList: TdxLinkList = nil;
  FPixelsDenominator: Integer;
  FPixelsNumerator: Integer;
  FPreviewDialogManager: TdxPSPreviewDialogManager;

  FSaveCursor: TCursor;
  FWaitCounter: Integer = 0;

  FPDFSecurityMapEntries: array [0..5] of TdxPDFSecurityActionMapEntry = (
    (Action: pdaComment; Name: sdxPDFSecurityAllowComments),
    (Action: pdaContentCopy; Name: sdxPDFSecurityAllowContentCopy),
    (Action: pdaContentEdit; Name: sdxPDFSecurityAllowContentEdit),
    (Action: pdaDocumentAssemble; Name: sdxPDFSecurityAllowDocumentAssemble),
    (Action: pdaPrint; Name: sdxPDFSecurityAllowPrint),
    (Action: pdaPrintHighResolution; Name: sdxPDFSecurityAllowPrintHiResolution)
  );

function dxPSActiveReportLinks: TdxPSReportLinkList;
begin
  if FActiveReportLinks = nil then
    FActiveReportLinks := TdxPSReportLinkList.Create;
  Result := FActiveReportLinks;
end;

function dxPSCalculateImageBounds(const R: TRect;
  AWidth, AHeight: Integer; ADrawMode: TdxGraphicDrawMode): TRect;

  function CalculateProportionalBounds(
    AWidth, AHeight: Integer; AStretch, ACenter: Boolean): TRect;
  begin
    if AStretch or (AWidth > cxRectWidth(R)) or (AHeight > cxRectHeight(R)) then
    begin
      Result := cxRectProportionalStretch(R, AWidth, AHeight);
      AHeight := cxRectHeight(Result);
      AWidth := cxRectWidth(Result);
    end
    else
      Result := cxRectBounds(R.Left, R.Top, AWidth, AHeight);

    if ACenter then
      Result := cxRectCenter(R, AWidth, AHeight);
  end;

begin
  case ADrawMode of
    gdmNone:
      Result := cxRectSetSize(R, AWidth, AHeight);
    gdmCenter:
      Result := cxRectCenter(R, AWidth, AHeight);
    gdmProportional, gdmCenterProportional,
    gdmStretchProportional, gdmCenterAndStretchProportional:
      Result := CalculateProportionalBounds(AWidth, AHeight,
        ADrawMode in [gdmStretchProportional, gdmCenterAndStretchProportional],
        ADrawMode in [gdmCenterProportional, gdmCenterAndStretchProportional]);
    else
      Result := R;
  end;
end;

function dxCellSidesToBorders(ASides: TdxCellSides): TcxBorders;
begin
  Result := [];
  if csLeft in ASides then
    Include(Result, bLeft);
  if csTop in ASides then
    Include(Result, bTop);
  if csRight in ASides then
    Include(Result, bRight);
  if csBottom in ASides then
    Include(Result, bBottom);
end;

procedure dxPSScaleFont(AFont: TFont; AFontSize, ATargetDPI: Integer);
begin
  dxPSScaleFont(AFont, AFontSize, ATargetDPI, dxDefaultDPI);
end;

procedure dxPSScaleFont(AFont: TFont; AFontSize, ATargetDPI, ASourceDPI: Integer);
begin
  AFont.Height := -MulDiv(MulDiv(AFontSize, ASourceDPI, PtPerInch), ATargetDPI, ASourceDPI);
end;

procedure dxPSScaleFontEx(AFont: TFont; AFontHeight, ATargetPPI, ASourcePPI: Integer);
var
  ASize: Integer;
begin
  ASize := MulDiv(AFontHeight, PtPerInch, dxDefaultDPI);
  ASize := MulDiv(ASize, ATargetPPI, ASourcePPI);
  AFont.Height := MulDiv(ASize, dxDefaultDPI, PtPerInch);
end;

function dxCanPrintTransparentImages(ACanvas: TdxPSReportRenderCustomCanvas): Boolean;
begin
  Result := ACanvas.SupportsTransparentImagesDrawing and not FDontPrintTransparentImages;
end;

function dxPSPreviewDialogManager: TdxPSPreviewDialogManager;
begin
  if (FPreviewDialogManager = nil) and not FUnitFinalized then
    FPreviewDialogManager := TdxPSPreviewDialogManager.Create;
  Result := FPreviewDialogManager;
end;

{ TdxPSReportLinkList }

function TdxPSReportLinkList.GetItem(Index: Integer): TBasedxReportLink;
begin
  Result := TBasedxReportLink(inherited Items[Index]);
end;

{ TdxReportLinkRegItem }

constructor TdxReportLinkRegItem.Create(ALinkClass: TdxReportLinkClass;
  AComponentClass: TComponentClass; ADesignerClass: TdxReportLinkDesignWindowClass);
begin
  inherited Create;
  LinkClass := ALinkClass;
  ComponentClass := AComponentClass;
  DesignerClass := ADesignerClass;
  if GetClass(LinkClass.ClassName) = nil then
    RegisterClass(LinkClass);
end;

destructor TdxReportLinkRegItem.Destroy;
begin
  if IsLastLinkClass and (GetClass(LinkClass.ClassName) <> nil) then
    UnregisterClass(LinkClass);
  inherited;
end;

function TdxReportLinkRegItem.IsEqual(ALinkClass: TdxReportLinkClass;
  AComponentClass: TComponentClass = nil; ADesignerClass: TdxReportLinkDesignWindowClass = nil): Boolean;
begin
  Result := (LinkClass = ALinkClass) and
    ((ComponentClass = nil) or (ComponentClass = AComponentClass)) and
    ((DesignerClass = nil) or (DesignerClass = ADesignerClass));
end;

function TdxReportLinkRegItem.IsLastLinkClass: Boolean;
begin
  Result := LinkList.GetLinkClassCount(LinkClass) = 1;
end;

{ TdxLinkList }

procedure TdxLinkList.Add(ALinkClass: TdxReportLinkClass;
  AComponentClass: TComponentClass; ADesignerClass: TdxReportLinkDesignWindowClass);
var
  AItem: TdxReportLinkRegItem;
begin
  if not Find(ALinkClass, AComponentClass, ADesignerClass) then
  begin
    if Assigned(AComponentClass) then
      RegisterClass(AComponentClass);
    AItem := TdxReportLinkRegItem.Create(ALinkClass, AComponentClass, ADesignerClass);
    AItem.FLinkList := Self;
    Insert(0, AItem);
  end;
end;

function TdxLinkList.Find(out AIndex: Integer;
  ALinkClass: TdxReportLinkClass; AComponentClass: TComponentClass = nil;
  ADesignerClass: TdxReportLinkDesignWindowClass = nil): Boolean;
var
  I: Integer;
begin
  AIndex := -1;
  for I := 0 to FLinkList.Count - 1 do
    if Items[I].IsEqual(ALinkClass, AComponentClass, ADesignerClass) then
    begin
      AIndex := I;
      Break;
    end;
  Result := AIndex <> -1;
end;

function TdxLinkList.Find(
  ALinkClass: TdxReportLinkClass; AComponentClass: TComponentClass = nil;
  ADesignerClass: TdxReportLinkDesignWindowClass = nil): Boolean;
var
  AIndex: Integer;
begin
  Result := Find(AIndex, ALinkClass, AComponentClass, ADesignerClass);
end;

function TdxLinkList.FindDesignerByLink(ALinkClass: TClass): TdxReportLinkDesignWindowClass;
var
  I: Integer;
begin
  Result := nil;
  if ALinkClass <> nil then
  begin
    for I := 0 to Count - 1 do
      if Items[I].LinkClass = ALinkClass then
      begin
        Result := Items[I].DesignerClass;
        Break;
      end;
  end;
end;

function TdxLinkList.FindDesignerByLink(ALink: TObject{TBasedxReportLink}): TdxReportLinkDesignWindowClass;
begin
  if ALink <> nil then
    Result := FindDesignerByLink(ALink.ClassType)
  else
    Result := nil;
end;

function TdxLinkList.FindLinkByComponent(AComponentClass: TClass): TdxReportLinkClass;
var
  AItem: TdxReportLinkRegItem;
  I: Integer;
begin
  Result := nil;
  if AComponentClass <> nil then
    for I := 0 to Count - 1 do
    begin
      AItem := Items[I];
      if AComponentClass.InheritsFrom(AItem.ComponentClass) then
      begin
        Result := AItem.LinkClass;
        if AComponentClass = AItem.ComponentClass then
          Break;
      end;
    end;
end;

function TdxLinkList.FindLinkByComponent(AComponent: TObject{TComponent}): TdxReportLinkClass;
begin
  if AComponent <> nil then
    Result := FindLinkByComponent(AComponent.ClassType)
  else
    Result := nil;
end;

function TdxLinkList.FindLinksByComponent(AComponentClass: TClass): TList;
var
  AClass: TdxReportLinkClass;
  AItem: TdxReportLinkRegItem;
  I: Integer;
begin
  Result := TList.Create;
  while (AComponentClass <> nil) and (Result.Count = 0) do
  begin
    for I := 0 to Count - 1 do
    begin
      AItem := Items[I];
      if AComponentClass = AItem.ComponentClass then
        Result.Add(AItem.LinkClass);
    end;
    AComponentClass := AComponentClass.ClassParent;
  end;
  if Result.Count = 0 then
  begin
    AClass := FindLinkByComponent(AComponentClass);
    if AClass <> nil then
      Result.Add(AClass);
  end;
end;

procedure TdxLinkList.GetLinks(AClassList: TdxClassList; AExcludeInactive: Boolean = True);
var
  ABuffer: TdxClassList;
  AComponentClass: TComponentClass;
  AItem: TdxReportLinkRegItem;
  ALinkClass: TdxReportLinkClass;
  I: Integer;
begin
  ABuffer := TdxClassList.Create;
  try
    for I := 0 to Count - 1 do
    begin
      AItem := Items[I];
      AComponentClass := AItem.ComponentClass;
      if not AExcludeInactive or (AComponentClass = nil) or (ABuffer.IndexOf(AComponentClass) = -1) then
      begin
        ALinkClass := AItem.LinkClass;
        if AClassList.IndexOf(ALinkClass) = -1 then
          AClassList.Add(ALinkClass);
        if AExcludeInactive and (AComponentClass <> nil) then
          ABuffer.Add(AComponentClass);
      end;
    end;
  finally
    ABuffer.Free;
  end;
end;

procedure TdxLinkList.GetSupportedComponents(AClassList: TdxClassList; ALinkClass: TClass = nil);
var
  I: Integer;
  Item: TdxReportLinkRegItem;
  ComponentClass: TComponentClass;
begin
  for I := 0 to Count - 1 do
  begin
    Item := Items[I];
    if (ALinkClass = nil) or (Item.LinkClass = ALinkClass) then
    begin
      ComponentClass := Item.ComponentClass;
      if AClassList.IndexOf(ComponentClass) = -1 then
        AClassList.Add(ComponentClass);
    end;
  end;
end;

procedure TdxLinkList.GetSupportedComponents(AClassList: TdxClassList; ALink: TObject{TBasedxReportLink});
var
  LinkClass: TClass;
begin
  if ALink <> nil then
    LinkClass := ALink.ClassType
  else
    LinkClass := nil;

  GetSupportedComponents(AClassList, LinkClass);
end;

procedure TdxLinkList.UnregisterLink(ALinkClass: TdxReportLinkClass;
  AComponentClass: TComponentClass = nil; ADesignerClass: TdxReportLinkDesignWindowClass = nil);
var
  Index: Integer;
begin
  if Find(Index, ALinkClass, AComponentClass, ADesignerClass) then
    Delete(Index);
end;

procedure TdxLinkList.UnregisterLinks(const ALinkClasses: array of TdxReportLinkClass);
var
  I: Integer;
begin
  for I := Low(ALinkClasses) to High(ALinkClasses) do
    UnregisterLink(ALinkClasses[I]);
end;

function TdxLinkList.GetLinkClassCount(ALinkClass: TdxReportLinkClass): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do
    if Items[I].LinkClass = ALinkClass then
      Inc(Result);
end;

function TdxLinkList.GetItem(Index: Integer): TdxReportLinkRegItem;
begin
  Result := inherited Items[Index] as TdxReportLinkRegItem;
end;

{ TdxPSExplorerTreeContainerFactory }

function dxPSExplorerTreeContainerFactory: TdxPSExplorerTreeContainerFactory;
begin
  Result := TdxPSExplorerTreeContainerFactory.Instance;
end;

class function TdxPSExplorerTreeContainerFactory.Instance: TdxPSExplorerTreeContainerFactory;
begin
  Result := inherited Instance as TdxPSExplorerTreeContainerFactory;
end;

function TdxPSExplorerTreeContainerFactory.GetActiveTreeContainerClass: TCustomdxPSExplorerTreeContainerClass;
begin
  Result := TCustomdxPSExplorerTreeContainerClass(Items[0]);
end;

{ TdxPSExplorerTreeBuilderFactory }

function dxPSExplorerTreeBuilderFactory: TdxPSExplorerTreeBuilderFactory;
begin
  Result := TdxPSExplorerTreeBuilderFactory.Instance;
end;

function dxPSExplorerTreeBuilderFactory_ActiveBuilderClass: TdxPSExplorerTreeBuilderClass;
begin
  Result := dxPSExplorerTreeBuilderFactory.ActiveBuilderClass;
end;

class function TdxPSExplorerTreeBuilderFactory.Instance: TdxPSExplorerTreeBuilderFactory;
begin
  Result := inherited Instance as TdxPSExplorerTreeBuilderFactory;
end;

 // because of CLR :-(
function TdxPSExplorerTreeBuilderFactory.GetActiveBuilderClass: TdxPSExplorerTreeBuilderClass;
begin
  Result := TdxPSExplorerTreeBuilderClass(Items[Count - 1]);
end;

{ TdxPSReaderFactory }

function dxPSReaderFactory: TdxPSReaderFactory;
begin
  Result := TdxPSReaderFactory.Instance;
end;

class function TdxPSReaderFactory.Instance: TdxPSReaderFactory;
begin
  Result := inherited Instance as TdxPSReaderFactory
end;

function TdxPSReaderFactory.GetActualReaderClass: TdxPSDataReaderClass;
begin
  Result := ReaderClasses[dxPSGlbl.dxPSStorageVersion];
end;

function TdxPSReaderFactory.GetReaderClass(Version: Integer): TdxPSDataReaderClass;
var
  I: Integer;
begin
  for I := Count - 1 to 0 do
  begin
    Result := TdxPSDataReaderClass(Items[I]);
    if Result.SupportsStorageVersion(Version) then Exit;
  end;
  Result := nil;
end;

{ TdxPSWriterFactory }

function dxPSWriterFactory: TdxPSWriterFactory;
begin
  Result := TdxPSWriterFactory.Instance;
end;

class function TdxPSWriterFactory.Instance: TdxPSWriterFactory;
begin
  Result := inherited Instance as TdxPSWriterFactory;
end;

function TdxPSWriterFactory.GetActualWriterClass: TdxPSDataWriterClass;
begin
  Result := WriterClasses[dxPSGlbl.dxPSStorageVersion];
end;

function TdxPSWriterFactory.GetWriterClass(Version: Integer): TdxPSDataWriterClass;
var
  I: Integer;
begin
  for I := Count - 1 to 0 do
  begin
    Result := TdxPSDataWriterClass(Items[I]);
    if Result.SupportsStorageVersion(Version) then Exit;
  end;
  Result := nil;
end;

{ units convertation routines }

function OnePixel: Integer;
begin
  Result := Max(FPixelsNumerator div FPixelsDenominator, 1);
end;

function PixelsNumerator: Integer;
begin
  Result := FPixelsNumerator;
end;

function PixelsDenominator: Integer;
begin
  Result := FPixelsDenominator;
end;

function ConvertValue(AValue: Integer): Integer;
begin
  Result := MulDiv(AValue, PixelsNumerator, PixelsDenominator);
end;

function ConvertRect(const AValue: TRect): TRect;
begin
  Result := cxRectScale(AValue, PixelsNumerator, PixelsDenominator);
end;

function ConvertPoint(const AValue: TPoint): TPoint;
begin
  Result := ScalePoint(AValue, PixelsNumerator, PixelsDenominator);
end;

{ Helpers }

procedure FixupRect(ACanvas: TdxPSReportRenderCustomCanvas; var R: TRect);
begin
  if Assigned(ACanvas) then
    ACanvas.FixupRect(R);
end;

const
  InnerBorderBRColors: array[TdxCellEdgeStyle, Boolean] of Integer =
    ((COLOR_BTNSHADOW, COLOR_BTNFACE), (COLOR_BTNSHADOW, COLOR_BTNFACE));
  InnerBorderTLColors: array[TdxCellEdgeStyle, Boolean] of Integer =
    ((COLOR_BTNHIGHLIGHT, COLOR_BTNHIGHLIGHT), (COLOR_BTNSHADOW, COLOR_BTNFACE));

  OuterBorderBRColors: array[TdxCellEdgeStyle, Boolean] of Integer =
    ((COLOR_WINDOWTEXT, COLOR_WINDOWTEXT), (COLOR_BTNHIGHLIGHT, COLOR_BTNHIGHLIGHT));
  OuterBorderTLColors: array[TdxCellEdgeStyle, Boolean] of Integer =
    ((COLOR_WINDOWTEXT, COLOR_WINDOWTEXT), (COLOR_WINDOWTEXT, COLOR_WINDOWTEXT));

procedure Get3DBorderBrushes(AEdgeStyle: TdxCellEdgeStyle; ASoft: Boolean;
  var AOuterTLBrush, AOuterBRBrush, AInnerTLBrush, AInnerBRBrush: HBRUSH);
begin
  AOuterTLBrush := GetSysColorBrush(OuterBorderTLColors[AEdgeStyle, ASoft]);
  AOuterBRBrush := GetSysColorBrush(OuterBorderBRColors[AEdgeStyle, ASoft]);
  AInnerTLBrush := GetSysColorBrush(InnerBorderTLColors[AEdgeStyle, ASoft]);
  AInnerBRBrush := GetSysColorBrush(InnerBorderBRColors[AEdgeStyle, ASoft]);
end;

procedure Get3DBorderColors(AEdgeStyle: TdxCellEdgeStyle; ASoft: Boolean;
  var AOuterTLColor, AOuterBRColor, AInnerTLColor, AInnerBRColor: TColor);
begin
  AOuterTLColor := GetSysColor(OuterBorderTLColors[AEdgeStyle, ASoft]);
  AOuterBRColor := GetSysColor(OuterBorderBRColors[AEdgeStyle, ASoft]);
  AInnerTLColor := GetSysColor(InnerBorderTLColors[AEdgeStyle, ASoft]);
  AInnerBRColor := GetSysColor(InnerBorderBRColors[AEdgeStyle, ASoft]);
end;

function dxPSExplorerImages: TCustomImageList;

  function AddShellIconByIndex(AnIndex: Integer): Integer;
  var
    Icon: TIcon;
  begin
    if AnIndex <> -1 then
    begin
      Icon := TIcon.Create;
      try
        dxPSUtl.ShellSmallImages.GetIcon(AnIndex, Icon);
        Result := FExplorerImages.AddIcon(Icon);
      finally
        Icon.Free;
      end
    end
    else
      Result := -1;
  end;

  function LoadFolderIcon(AExtraFlags: Integer = 0): Integer;
  const
    DefaultFlags = SHGFI_USEFILEATTRIBUTES or SHGFI_ICON or SHGFI_SYSICONINDEX;
  var
    AFileInfo: TSHFileInfo;
  begin
    FillChar(AFileInfo, SizeOf(AFileInfo), 0);
    AFileInfo.dwAttributes := SFGAO_FOLDER;
    try
      SHGetFileInfo('C:\Folder\', FILE_ATTRIBUTE_DIRECTORY,
        AFileInfo, SizeOf(AFileInfo), DefaultFlags or AExtraFlags);
    finally
      DestroyIcon(AFileInfo.hIcon);
    end;
    Result := AddShellIconByIndex(AFileInfo.iIcon);
  end;

  function LoadBitmap(const AResName: string): Integer;
  var
    Bitmap: TBitmap;
  begin
    Bitmap := TBitmap.Create;
    try
      dxLoadBitmapFromResource(Bitmap, AResName);
      Result := FExplorerImages.AddMasked(Bitmap, clFuchsia);
    finally
      Bitmap.Free;
    end;
  end;

begin
  if FExplorerImages = nil then
  begin
    FExplorerImages := TcxImageList.Create(nil);
    FExplorerImages.Width := ShellSmallImages.Width;
    FExplorerImages.Height := ShellSmallImages.Height;
    FExplorerImages.AllocBy := ShellSmallImages.AllocBy;
    FExplorerImages.BkColor := ShellSmallImages.BkColor;
    iiExplorerFolderCollapsed := LoadFolderIcon;
    iiExplorerFolderExpanded := LoadFolderIcon(SHGFI_OPENICON);
    iiExplorerItem := LoadBitmap(IDB_DXPSEXPLORERITEM_SMALL);
    iiExplorerItemHasInvalidData := LoadBitmap(IDB_DXPSEXPLORERITEM_INVALID);
  end;
  Result := FExplorerImages;
end;

procedure dxPSStartWait;
begin
  if FWaitCounter = 0 then
  begin
    FSaveCursor := Screen.Cursor;
    Screen.Cursor := crHourGlass;
  end;
  Inc(FWaitCounter);
end;

procedure dxPSStopWait;
begin
  if FWaitCounter <> 0 then
  begin
    Dec(FWaitCounter);
    if FWaitCounter = 0 then
      Screen.Cursor := FSaveCursor;
  end;
end;

{ ReportLinks Registration and Utilities}

function dxPSIndexOfRegItem(ALinkClass: TdxReportLinkClass;
  AComponentClass: TComponentClass; ADesignerClass: TdxReportLinkDesignWindowClass): Integer;
begin
  if FLinkList <> nil then
    FLinkList.Find(Result, ALinkClass, AComponentClass, ADesignerClass)
  else
    Result := -1;
end;

procedure dxPSRegisterReportLink(ALinkClass: TdxReportLinkClass;
  AComponentClass: TComponentClass; ADesignerClass: TdxReportLinkDesignWindowClass);
begin
  if (ALinkClass = nil) or ((AComponentClass <> nil) and
     (dxPSIndexOfRegItem(ALinkClass, AComponentClass, ADesignerClass) <> -1))
  then
    Exit;

  if FLinkList = nil then
    FLinkList := TdxLinkList.Create;
  FLinkList.Add(ALinkClass, AComponentClass, ADesignerClass);
end;

procedure dxPSUnregisterReportLink(ALinkClass: TdxReportLinkClass;
  AComponentClass: TComponentClass; ADesignerClass: TdxReportLinkDesignWindowClass);
begin
  if FLinkList <> nil then
    FLinkList.UnregisterLink(ALinkClass, AComponentClass, ADesignerClass);
end;

procedure dxPSUnregisterReportLinks(const ALinkClasses: array of TdxReportLinkClass);
begin
  if FLinkList <> nil then
    FLinkList.UnregisterLinks(ALinkClasses);
end;

procedure dxPSUnregisterAllReportLinks;
begin
  if FLinkList <> nil then
  begin
    FLinkList.Clear;
    FreeAndNil(FLinkList);
  end;
end;

function dxPSDesignerClassByCompClass(AComponentClass: TClass): TdxReportLinkDesignWindowClass;
var
  LinkClass: TdxReportLinkClass;
begin
  LinkClass := dxPSLinkClassByCompClass(AComponentClass);
  Result := dxPSDesignerClassByLinkClass(LinkClass);
end;

function dxPSDesignerClassByCompClass(AComponent: TObject{TComponent}): TdxReportLinkDesignWindowClass;
begin
  if AComponent <> nil then
    Result := dxPSDesignerClassByCompClass(AComponent.ClassType)
  else
    Result := nil;
end;

function dxPSLinkClassByCompClass(AComponentClass: TClass): TdxReportLinkClass;
begin
  if FLinkList <> nil then
    Result := FLinkList.FindLinkByComponent(AComponentClass)
  else
    Result := nil;
end;

function dxPSLinkClassByCompClass(AComponent: TObject{TComponent}): TdxReportLinkClass;
begin
  if FLinkList <> nil then
    Result := FLinkList.FindLinkByComponent(AComponent)
  else
    Result := nil;
end;

function dxPSLinkClassByCompClassEx(AComponentClass: TClass): TList;
begin
  if FLinkList <> nil then
    Result := FLinkList.FindLinksByComponent(AComponentClass)
  else
    Result := nil;
end;

function dxPSDesignerClassByLinkClass(ALinkClass: TClass): TdxReportLinkDesignWindowClass;
begin
  if FLinkList <> nil then
    Result := FLinkList.FindDesignerByLink(ALinkClass)
  else
    Result := nil;
end;

function dxPSDesignerClassByLinkClass(ALink: TObject{TBasedxReportLink}): TdxReportLinkDesignWindowClass;
begin
  if ALink <> nil then
    Result := FLinkList.FindDesignerByLink(ALink)
  else
    Result := nil;
end;

procedure dxPSGetActiveReportLinksList(AClassList: TdxClassList);
begin
  if FLinkList <> nil then
    FLinkList.GetLinks(AClassList, True);
end;

procedure dxPSGetReportLinksList(AClassList: TdxClassList);
begin
  if FLinkList <> nil then
    FLinkList.GetLinks(AClassList, False);
end;

procedure dxPSGetLinkSupportedComponentsList(AClassList: TdxClassList; ALinkClass: TClass);
begin
  if FLinkList <> nil then
    FLinkList.GetSupportedComponents(AClassList, ALinkClass);
end;

procedure dxPSGetLinkSupportedComponentsList(AClassList: TdxClassList; ALink: TObject{TBasedxReportLink});
begin
  if FLinkList <> nil then
    FLinkList.GetSupportedComponents(AClassList, ALink);
end;

procedure dxPSGetSupportedComponentsList(AClassList: TdxClassList);
begin
  if FLinkList <> nil then
    FLinkList.GetSupportedComponents(AClassList);
end;

function dxPSIsSupportedCompClass(AComponentClass: TClass): Boolean;
begin
  Result := dxPSLinkClassByCompClass(AComponentClass) <> nil;
end;

function dxPSIsSupportedCompClass(AComponent: TObject{TComponent}): Boolean;
begin
  Result := dxPSLinkClassByCompClass(AComponent) <> nil;
end;

function dxPrintComponent(AComponent: TComponent;
  APrintPreview: Boolean = True; APrintDialog: Boolean = False;
  const AReportTitle: string = ''; const APrintTitle: string = ''): Boolean;
var
  ComponentPrinter: TdxComponentPrinter;
  ReportLink: TBasedxReportLink;
begin
  try
    ComponentPrinter := TdxComponentPrinter.Create(nil);
    try
      ComponentPrinter.PrintTitle := APrintTitle;
      ReportLink := ComponentPrinter.AddLink(AComponent);
      Result := ReportLink <> nil;
      if Result then
      try
        ReportLink.ReportTitleText := AReportTitle;
        if APrintPreview then
          ReportLink.Preview(True)
        else
        begin
          ReportLink.PrinterPage.InitFromPrintDevice;
          ReportLink.Print(APrintDialog, nil);
        end;
      finally
        ReportLink.Free;
      end
    finally
      ComponentPrinter.Free;
    end;
  except
    Result := False;
  end;
end;

type
  TdxPSRunTimeComponentsProvider = class(TAbstractdxPSComponentsProvider)
  public
    procedure GetComponents(AComponentPrinter: TdxComponentPrinter; AReportLink: TBasedxReportLink;
      AComponents: TStrings; AnOptions: TdxPSGetComponentOptions); override;
  end;

procedure TdxPSRunTimeComponentsProvider.GetComponents(AComponentPrinter: TdxComponentPrinter;
  AReportLink: TBasedxReportLink; AComponents: TStrings; AnOptions: TdxPSGetComponentOptions);

  procedure ProcessComponent(AComponent: TComponent);
  var
    Caption: string;
    Description: string;
    Item: TdxComponentItem;
  begin
    Caption := AComponent.Name;
    Description := '';
    if (AComponentPrinter.FindLinkByComponent(AComponent) = nil) and
      (((AReportLink = nil) and dxPSIsSupportedCompClass(AComponent)) or
       ((AReportLink <> nil) and AReportLink.Supports(AComponent))) and
      AComponentPrinter.DoFilterComponent(AComponent, Caption, Description) then
    begin
      Item := dxPSCreateComponentItem(AComponent, Caption, Description);
      AComponents.AddObject(Item.Caption, Item);
    end;
  end;

var
  Owner: TComponent;
  I: Integer;
begin
  if not AComponentPrinter.GetSupportedComponents(AComponents) then
  begin
    //Item := dxPSCreateComponentItem(nil, 'Composition', '');
    //AComponents.AddObject(Item.Caption, Item);

    Owner := AComponentPrinter.Owner;
    if Owner <> nil then
    begin
      ProcessComponent(Owner);
      for I := 0 to Owner.ComponentCount - 1 do
        ProcessComponent(Owner.Components[I]);
    end;
  end;
end;

{ EdxInvalidStorageVersion }

constructor EdxInvalidStorageVersion.Create(AVersion: UINT; ADummy: Boolean = False);
begin
  FVersion := AVersion;
  inherited CreateFmt(cxGetResourceString(@sdxInvalidStorageVersion), [Version]);
end;

procedure ReportCellClassNotRegistered(const AClassName: string);
begin
  raise EdxReportLink.CreateFmt(
    cxGetResourceString(@sdxReportCellClassNotRegistered), [AClassName]);
end;

{ TdxPSDataReader }

class procedure TdxPSDataReader.Register;
begin
  dxPSReaderFactory.Register(Self);
end;

class procedure TdxPSDataReader.Unregister;
begin
  dxPSReaderFactory.Unregister(Self);
end;

function TdxPSDataReader.ReadClass: TClass;
begin
  Result := GetClass(ReadString);
end;

function TdxPSDataReader.ReadCellBorderClass: TdxPSCellBorderClass;
begin
  Result := TdxPSCellBorderClass(ReadClass);
  if Result = nil then
    Result := TdxPSCellUltraFlatBorder;
end;

function TdxPSDataReader.ReadFillPatternClass: TdxPSFillPatternClass;
begin
  Result := TdxPSFillPatternClass(ReadClass);
  if Result = nil then
    Result := TdxPSSolidFillPattern;
end;

function TdxPSDataReader.ReadFont(AFont: TFont): TFont;
begin
  if AFont <> nil then
    Result := AFont
  else
    Result := TFont.Create;

  Result.Charset := TFontCharset(ReadInteger);
  Result.Color := ReadInteger;
  Result.Name := ReadString;
  Result.Pitch := TFontPitch(ReadInteger);
  Result.Size := ReadInteger;
  Result.Style := TFontStyles(Byte(ReadInteger));
  if dxPSCheckVersion(PSVersion, 4, 20160000) then
    Result.Orientation := ReadInteger;
end;

function TdxPSDataReader.ReadGraphicClass: TGraphicClass;
begin
  Result := TGraphicClass(ReadClass);
end;

procedure TdxPSDataReader.ReadImage(AImage: TGraphic);
var
  AMemoryStream: TMemoryStream;
  AHasMask: Boolean;
  AMask: TBitmap;
begin
  AMemoryStream := TMemoryStream.Create;
  try
    AMemoryStream.Size := ReadInt64;
    if AMemoryStream.Size <> 0 then
    begin
      Read(AMemoryStream.Memory^, AMemoryStream.Size);
      AMemoryStream.Position := 0;
      AImage.LoadFromStream(AMemoryStream);

      if (AMemoryStream.Read(AHasMask, SizeOf(AHasMask)) = SizeOf(AHasMask)) and AHasMask then
      begin
        AMask := TBitmap.Create;
        try
          AMask.LoadFromStream(AMemoryStream);
          (AImage as TBitmap).MaskHandle := AMask.ReleaseHandle;
        finally
          AMask.Free;
        end;
      end;
    end;
  finally
    AMemoryStream.Free;
  end;
end;

procedure TdxPSDataReader.ReadImageList(AnImageList: TCustomImageList);
var
  MemoryStream: TMemoryStream;
  Adapter: IStream;
begin
  MemoryStream := TMemoryStream.Create;
  try
    MemoryStream.Size := Self.ReadInt64;
    if MemoryStream.Size <> 0 then
    begin
      Self.Read(MemoryStream.Memory^, MemoryStream.Size);
      MemoryStream.Position := 0;
      Adapter := TStreamAdapter.Create(MemoryStream);
      AnImageList.Handle := CommCtrl.ImageList_Read(Adapter);
    end;
  finally
    MemoryStream.Free;
  end;
end;

function TdxPSDataReader.ReadLinkClass: TdxReportLinkClass;
begin
  Result := TdxReportLinkClass(ReadClass);
end;

function TdxPSDataReader.ReadLookAndFeelClass: TdxPSReportGroupLookAndFeelClass;
begin
  Result := TdxPSReportGroupLookAndFeelClass(ReadClass);
  if Result = nil then
    Result := TdxPSReportGroupNullLookAndFeel;
end;

function TdxPSDataReader.ReadPoint: TPoint;
begin
  Read(Result, SizeOf(Result));
end;

function TdxPSDataReader.ReadPSVersion: TdxPSVersion;
begin
  Read(FPSVersion, SizeOf(FPSVersion));
  Result := FPSVersion;
end;

function TdxPSDataReader.ReadRect: TRect;
begin
  Read(Result, SizeOf(Result));
end;

procedure TdxPSDataReader.SkipBytes(Count: Int64);
var
  ABytes: PByte;
begin
  ABytes := AllocMem(Count);
  try
    Read(ABytes^, Count);
  finally
    FreeMem(ABytes);
  end;
end;

class function TdxPSDataReader.SupportsStorageVersion(AVersion: Integer): Boolean;
begin
  Result := True;
end;

{ TdxPSDataWriter }

class procedure TdxPSDataWriter.Register;
begin
  dxPSWriterFactory.Register(Self);
end;

class procedure TdxPSDataWriter.Unregister;
begin
  dxPSWriterFactory.Unregister(Self);
end;

procedure TdxPSDataWriter.WriteClassName(AClass: TClass);
begin
  if AClass <> nil then
    WriteString(AClass.ClassName)
  else
    WriteString(sdxNil);
end;

procedure TdxPSDataWriter.WriteClassName(AnObject: TObject);
begin
  if AnObject <> nil then
    WriteClassName(AnObject.ClassType)
  else
    WriteString(sdxNil);
end;

procedure TdxPSDataWriter.WriteFont(AFont: TFont);
begin
  WriteInteger(AFont.Charset);
  WriteInteger(AFont.Color);
  WriteString(AFont.Name);
  WriteInteger(Integer(AFont.Pitch));
  WriteInteger(AFont.Size);
  WriteInteger(Integer(Byte(AFont.Style)));
  if dxPSCheckVersion(PSVersion, 4, 20160000) then
    WriteInteger(AFont.Orientation);
end;

procedure TdxPSDataWriter.WriteImage(AnImage: TGraphic);
var
  MemoryStream: TMemoryStream;
  HasMask: Boolean;
  Mask: TBitmap;
begin
  MemoryStream:= TMemoryStream.Create;
  try
    AnImage.SaveToStream(MemoryStream);

    HasMask := (AnImage is TBitmap) and (TBitmap(AnImage).MaskHandle <> 0);
    MemoryStream.Write(HasMask, SizeOf(HasMask));
    if HasMask then
    begin
      Mask := TBitmap.Create;
      try
        Mask.Monochrome := True;
        Mask.Handle := TBitmap(AnImage).MaskHandle;
        Mask.SaveToStream(MemoryStream);
      finally
        Mask.Free;
      end;
    end;

    Self.WriteInteger(MemoryStream.Size);
    Self.Write(MemoryStream.Memory^, MemoryStream.Size);
  finally
    MemoryStream.Free;
  end;
end;

procedure TdxPSDataWriter.WriteImageList(AnImageList: TCustomImageList);
var
  MemoryStream: TMemoryStream;
  Adapter: IStream;
begin
  MemoryStream := TMemoryStream.Create;
  try
    Adapter := TStreamAdapter.Create(MemoryStream);
    CommCtrl.ImageList_Write(AnImageList.Handle, Adapter);

    Self.WriteInteger(MemoryStream.Size);
    Self.Write(MemoryStream.Memory^, MemoryStream.Size);
  finally
    MemoryStream.Free;
  end;
end;

procedure TdxPSDataWriter.WritePoint(const Pt: TPoint);
begin
  Write(Pt, SizeOf(Pt));
end;

procedure TdxPSDataWriter.WritePSVersion(const AVersion: TdxPSVersion);
begin
  Write(AVersion, SizeOf(AVersion));
  FPSVersion := AVersion;
end;

procedure TdxPSDataWriter.WriteRect(const R: TRect);
begin
  Write(R, SizeOf(R));
end;

class function TdxPSDataWriter.SupportsStorageVersion(AVersion: Integer): Boolean;
begin
  Result := True;
end;

{ TdxPageOverlayIndexes }

function TdxPageOverlayIndexes.Add(AValue: Integer): Integer;
begin
  Result := inherited Add(TObject(AValue));
end;

function TdxPageOverlayIndexes.GetItem(Index: Integer): Integer;
begin
  Result := Integer(inherited Items[Index]);
end;

procedure TdxPageOverlayIndexes.SetItem(Index, Value: Integer);
begin
  inherited Items[Index] := TObject(Value);
end;

{ TdxPSPageRenderInfo }

constructor TdxPSPageRenderInfo.Create(ARenderInfo: TdxPSReportRenderInfo; APageIndex: Integer);
begin
  inherited Create;
  FPageIndex := APageIndex;
  FRenderInfo := ARenderInfo;
  FIndexPairs := TList.Create;
  FOverlays := TList.Create;
  FPrinterPage := CreatePrinterPage(ARenderInfo);
  FPrinterPage.Assign(ARenderInfo.PrinterPage);
end;

destructor TdxPSPageRenderInfo.Destroy;
begin
  FreeAndNilOverlays;
  FreeAndNilIndexPairs;
  FreeAndNil(FPrinterPage);
  inherited Destroy;
end;

procedure TdxPSPageRenderInfo.Calculate;
begin
  CalculateCompositionPartInfo;
  CalculateBounds;
  CalculateOffsets;
  FIndexPairs.Count := CalculateIndexPairCount;
  CalculateIndexPairs;
  CalculateOverlayIndexes;
  CalculatePageHeaderAndFooterBounds;
end;

function TdxPSPageRenderInfo.HasDetails: Boolean;
begin
  Result := not IsRectEmpty(DetailBounds);
end;

function TdxPSPageRenderInfo.HasFooter: Boolean;
begin
  Result := not IsRectEmpty(FooterBounds) and (RenderInfo.CanUseHFOnEveryPageMode or IsBottomPage);
end;

function TdxPSPageRenderInfo.HasFootnotes: Boolean;
begin
  Result := RenderInfo.IsDrawPageFootnotesOnPage(PageIndex);
end;

function TdxPSPageRenderInfo.HasHeader: Boolean;
begin
  Result := not IsRectEmpty(HeaderBounds) and (RenderInfo.CanUseHFOnEveryPageMode or IsTopPage);
end;

function TdxPSPageRenderInfo.HasHeaderCorner: Boolean;
begin
  Result := not IsRectEmpty(HeaderCornerBounds) and (RenderInfo.CanUseHFOnEveryPageMode or IsTopPage);
end;

function TdxPSPageRenderInfo.HasRowHeader: Boolean;
begin
  Result := not IsRectEmpty(RowHeaderBounds) and (RenderInfo.CanUseHFOnEveryPageMode or IsTopPage);
end;

function TdxPSPageRenderInfo.HasTitle: Boolean;
begin
  Result := RenderInfo.IsDrawPageTitleOnPage(PageIndex);
end;

function TdxPSPageRenderInfo.AreRectsIntersected(const R1, R2: TRect): Boolean;
var
  R: TRect;
begin
  if R1.Top = R1.Bottom then
    Result := (R1.Top >= R2.Top) and (R1.Top <= R2.Bottom) // bear in mind that R.Top = R.Bottom
  else
    Result := Windows.IntersectRect(R, R1, R2);
end;

procedure TdxPSPageRenderInfo.CalculateBounds;
var
  AContentHeight: Integer;
begin
  DetailBounds := RenderInfo.CalculatePageDetailBounds(ColIndex, RowIndex);
  CalculateHeadersBounds;

  ContentBounds := DetailBounds;
  if HasHeader then
    Inc(ContentBounds.Bottom, cxRectHeight(HeaderBounds));
  if HasFooter then
    Inc(ContentBounds.Bottom, cxRectHeight(FooterBounds));
  if HasRowHeader then
    Inc(ContentBounds.Right, cxRectWidth(RowHeaderBounds));

  AContentHeight := MulDiv(PaintSize.Y, 100, RenderInfo.ScaleFactor);
  if HasTitle then
    Dec(AContentHeight, TitleHeight);
  if HasFootnotes then
    Dec(AContentHeight, FootnotesHeight);

  ContentBounds.Bottom := ContentBounds.Top + Min(AContentHeight, cxRectHeight(ContentBounds));
end;

procedure TdxPSPageRenderInfo.CalculateHeadersBounds;
begin
  FooterBounds := RenderInfo.FooterBounds;
  HeaderBounds := RenderInfo.HeaderBounds;
  HeaderCornerBounds := RenderInfo.HeaderCornerBounds;
  RowHeaderBounds := RenderInfo.RowHeaderBounds;
end;

procedure TdxPSPageRenderInfo.CalculateCompositionPartInfo;
var
  ARowInfo: TdxPSCompositionReportRenderRowInfo;
begin
  FIsCompositionPagePart := RenderInfo.GetCompositionInfo(PageIndex, ARowInfo);
  if IsCompositionPagePart then
    PageOffset := Point(0, MulDiv(ARowInfo.CalculateContentOffset(ReportLink), 100, RenderInfo.ScaleFactor))
  else
    PageOffset := cxNullPoint;
end;

function TdxPSPageRenderInfo.CalculateIndexPairCount: Integer;
begin
  Result := 1;
end;

procedure TdxPSPageRenderInfo.CalculateIndexPairs;
var
  EndIndex, I, StartIndex: Integer;
  R: TRect;
  Intersected: Boolean;
begin
  if (IndexPairCount > 0) and (ReportCells <> nil) then
  begin
    EndIndex := {AStartIndex } -1;
    for I := 0 to IndexPairCount - 1 do
    begin
      StartIndex := EndIndex + 1;
      repeat
        R := ReportCells.Cells[StartIndex].AbsoluteRect;
        Inc(StartIndex);
      until (StartIndex = ReportCells.Count) or AreRectsIntersected(R, DetailBounds);
      Dec(StartIndex);
      EndIndex := StartIndex;
      // fix 2.1
      Intersected := True;
      while (EndIndex < ReportCells.Count) and Intersected do
      begin
        R := ReportCells.Cells[EndIndex].AbsoluteRect;
        Intersected := AreRectsIntersected(R, DetailBounds);
        Inc(EndIndex);
      end;
      Dec(EndIndex);
      if EndIndex <> ReportCells.Count - 1 then
        Dec(EndIndex);
      EndIndex := Max(StartIndex, EndIndex);
      IndexPairs[I].StartIndex := StartIndex;
      IndexPairs[I].EndIndex := EndIndex;
    end;
  end
  else
    DetailBounds := cxNullRect;
end;

function TdxPSPageRenderInfo.CalculateIsEmptyPage: Boolean;
begin
  Result := not RenderInfo.IsNonEmptyPage(DetailBounds);
end;

function TdxPSPageRenderInfo.CreatePrinterPage(ARenderInfo: TdxPSReportRenderInfo): TdxPrinterPage;
begin
  Result := TdxPrinterPage.Create;
end;

procedure TdxPSPageRenderInfo.CalculateFootnotesOffset;
var
  R: TRect;
begin
  R := RenderInfo.LoMetricRectToInternalUnits(PrinterPage.FooterRectLoMetric);
  FootnotesOffset := Point(R.Left, R.Top - FootnotesHeight);
end;

procedure TdxPSPageRenderInfo.CalculateTitleOffset;
var
  R: TRect;
begin
  R := RenderInfo.LoMetricRectToInternalUnits(PrinterPage.HeaderRectLoMetric);
  TitleOffset := Point(R.Left + PageOffset.X, R.Bottom + PageOffset.Y);
end;

procedure TdxPSPageRenderInfo.CalculateOffsets;

  procedure CalculateHorizontalOffsets(const AMarginsOffset: TPoint; const R: TRect);
  var
    AOffset: Integer;
  begin
    DataOffset.X := RenderInfo.LoMetricValueToInternalUnits(AMarginsOffset.X);
    if RenderInfo.PrinterPage.CenterOnPageH then
    begin
      AOffset := Max(PaintSize.X - cxRectWidth(R), 0) div 2;
      Inc(DataOffset.X, MulDiv(AOffset, 100, RenderInfo.ScaleFactor));
    end;
  end;

  procedure CalculateVerticalOffsets(const AMarginsOffset: TPoint; const R: TRect);
  var
    APaintSize: Integer;
  begin
    DataOffset.Y := RenderInfo.LoMetricValueToInternalUnits(AMarginsOffset.Y);
    if HasTitle then
      Inc(DataOffset.Y, RenderInfo.TitleHeight);
    if RenderInfo.PrinterPage.CenterOnPageV then
    begin
      APaintSize := PaintSize.Y;
      if HasTitle then
        Dec(APaintSize, RenderInfo.TitleHeight);
      if HasFootnotes then
        Dec(APaintSize, RenderInfo.FootnotesHeight);
      Inc(DataOffset.Y, MulDiv(Max(0, APaintSize - cxRectHeight(R)) div 2, 100, RenderInfo.ScaleFactor));
    end;
  end;

var
  AFullRect: TRect;
begin
  AFullRect := cxRectOffset(ContentBounds, ContentBounds.TopLeft, False);
  AFullRect := cxRectScale(AFullRect, RenderInfo.ScaleFactor, 100);
  CalculateHorizontalOffsets(PrinterPage.MarginsLoMetric.TopLeft, AFullRect);
  CalculateVerticalOffsets(PrinterPage.MarginsLoMetric.TopLeft, AFullRect);
  if HasTitle then
    CalculateTitleOffset;
  if HasFootnotes then
    CalculateFootnotesOffset;
  DataOffset := cxPointOffset(DataOffset, PageOffset);
end;

procedure TdxPSPageRenderInfo.CalculateOverlayIndexes;

  function CreateOverlayIndexes(AnOverlay: TdxReportCell): TdxPageOverlayIndexes;
  var
    I: Integer;
    R: TRect;
  begin
    Result := TdxPageOverlayIndexes.Create;
    for I := 0 to AnOverlay.CellCount - 1 do
    begin
      if IntersectRect(R, AnOverlay[I].AbsoluteRect, DetailBounds) then
        Result.Add(I);
    end;
  end;

var
  I: Integer;
begin
  if (ReportCells = nil) or not ReportCells.HasOverlays then
    Exit;

  FOverlays.Count := ReportCells.OverlayCount;
  for I := 0 to ReportCells.OverlayCount - 1 do
    FOverlays[I] := CreateOverlayIndexes(ReportCells.Overlays[I]);
end;

procedure TdxPSPageRenderInfo.CalculatePageHeaderAndFooterBounds;
begin
  PageHeaderBounds := RenderInfo.LoMetricRectToInternalUnits(PrinterPage.HeaderRectLoMetric);
  PageFooterBounds := RenderInfo.LoMetricRectToInternalUnits(PrinterPage.FooterRectLoMetric);
end;

function TdxPSPageRenderInfo.GetPageSize: TPoint;
begin
  Result := PrinterPage.RealPageSizeLoMetric;
  Result.X := MulDiv(Result.X, RenderInfo.UnitsPerInch, 254);
  Result.Y := MulDiv(Result.Y, RenderInfo.UnitsPerInch, 254);
end;

function TdxPSPageRenderInfo.GetPaintSize: TPoint;
begin
  Result.X := MulDiv(cxRectWidth(PrinterPage.PaintRectLoMetric), RenderInfo.UnitsPerInch, 254);
  Result.Y := MulDiv(cxRectHeight(PrinterPage.PaintRectLoMetric), RenderInfo.UnitsPerInch, 254);
end;

function TdxPSPageRenderInfo.GetColIndex: Integer;
begin
  Result := PageIndex mod RenderInfo.PageColCount;
end;

function TdxPSPageRenderInfo.GetFootnotesBounds: TRect;
begin
  Result := RenderInfo.FootnotesBounds;
end;

function TdxPSPageRenderInfo.GetFootnotesHeight: Integer;
begin
  Result := cxRectHeight(FootnotesBounds);
end;

function TdxPSPageRenderInfo.GetIndexPair(Index: Integer): TdxContinuedIndexPair;
begin
  Result := TdxContinuedIndexPair(FIndexPairs[Index]);
  if Result = nil then
  begin
    Result := TdxContinuedIndexPair.Create;
    FIndexPairs[Index] := Result;
  end;
end;

function TdxPSPageRenderInfo.GetIndexPairCount: Integer;
begin
  Result := FIndexPairs.Count;
end;

function TdxPSPageRenderInfo.GetIsBottomPage: Boolean;
begin
  Result := PageIndex >= RenderInfo.PageRenderInfoCount - RenderInfo.PageColCount;
end;

function TdxPSPageRenderInfo.GetIsEmptyPage: Boolean;
begin
  if not FIsEmptyPageCalculated then
  begin
    FIsEmptyPageCalculated := True;
    FIsEmptyPage := CalculateIsEmptyPage;
  end;
  Result := FIsEmptyPage;
end;

function TdxPSPageRenderInfo.GetIsTopPage: Boolean;
begin
  Result := PageIndex < RenderInfo.PageColCount;
end;

function TdxPSPageRenderInfo.GetOverlay(Index: Integer): TdxPageOverlayIndexes;
begin
  Result := TdxPageOverlayIndexes(FOverlays[Index]);
end;

function TdxPSPageRenderInfo.GetOverlayCount: Integer;
begin
  Result := FOverlays.Count;
end;

function TdxPSPageRenderInfo.GetReportCells: TdxReportCells;
begin
  Result := ReportLink.FReportCells;
end;

function TdxPSPageRenderInfo.GetReportLink: TBasedxReportLink;
begin
  Result := RenderInfo.ReportLink;
end;

function TdxPSPageRenderInfo.GetRowIndex: Integer;
begin
  Result := PageIndex div RenderInfo.PageColCount;
end;

function TdxPSPageRenderInfo.GetTitleBounds: TRect;
begin
  Result := RenderInfo.TitleBounds;
end;

function TdxPSPageRenderInfo.GetTitleHeight: Integer;
begin
  Result := cxRectHeight(TitleBounds);
end;

procedure TdxPSPageRenderInfo.SetIndexPair(Index: Integer; Value: TdxContinuedIndexPair);
begin
  FIndexPairs[Index] := Value;
end;

procedure TdxPSPageRenderInfo.FreeAndNilIndexPairs;
var
  I: Integer;
begin
  for I := 0 to IndexPairCount - 1 do
    IndexPairs[I].Free;
  FreeAndNil(FIndexPairs);
end;

procedure TdxPSPageRenderInfo.FreeAndNilOverlays;
var
  I: Integer;
begin
  for I := 0 to OverlayCount - 1 do
    Overlays[I].Free;
  FreeAndNil(FOverlays);
end;

{ TdxPSPageDelimitersCalculator }

constructor TdxPSPageDelimitersCalculator.Create(AReportRenderInfo: TdxPSReportRenderInfo);
begin
  inherited Create;
  FReportRenderInfo := AReportRenderInfo;
end;

procedure TdxPSPageDelimitersCalculator.AddPageDelimiter(AValue: Integer);
begin
  FPageDelimiters.Add(Pointer(AValue));
end;

procedure TdxPSPageDelimitersCalculator.BeginCalculate;
begin
  FPageDelimiters := ReportRenderInfo.FPageDelimitersX;
  FReportDelimiters := ReportRenderInfo.DelimiterXList;
  FReportContentSize := ReportRenderInfo.ReportWidth;
end;

class procedure TdxPSPageDelimitersCalculator.Calculate(AReportRenderInfo: TdxPSReportRenderInfo);
begin
  with Create(AReportRenderInfo) do
  try
    CalculateDelimiters;
  finally
    Free;
  end;
end;

procedure TdxPSPageDelimitersCalculator.CalculateDelimiters;
var
  ACurDelimiter: Integer;
  ADelimiter: Integer;
  AHardDelimiterIndex: Integer;
  APageIndex, AContentSize: Integer;
  APixelSize: Integer;
  I, AOffset: Integer;
begin
  I := 0;
  APageIndex := 0;
  ACurDelimiter := 0;
  APixelSize := OnePixel;
  BeginCalculate;
  try
    PageDelimitersList.Clear;
    AddPageDelimiter(0);
    while I < ReportDelimitersList.Count do
    begin
      AContentSize := CalculatePageContentSize(APageIndex);
      if AContentSize <= 0 then
      begin
        AddPageDelimiter(0);
        Inc(APageIndex);
        Continue;
      end;

      AHardDelimiterIndex := -1;
      AOffset := PageDelimiters[APageIndex];
      while I < ReportDelimitersList.Count do
      begin
        ADelimiter := ReportDelimiters[I];
        if ScaleValue(ADelimiter - AOffset) >= AContentSize + APixelSize then
          Break;
        if IsHardDelimiter(ADelimiter) then
        begin
          AHardDelimiterIndex := I;
          if ReportRenderInfo.BreakPagesByHardDelimiters then
            Break;
        end;
        Inc(I);
      end;

      if I < ReportDelimitersList.Count then
      begin
        if AHardDelimiterIndex <> -1 then
          I := AHardDelimiterIndex
        else
          Dec(I);

        ACurDelimiter := ReportDelimiters[I];
        if AHardDelimiterIndex <> -1 then
          Inc(I);
        if AOffset >= ACurDelimiter then
          ACurDelimiter := AOffset + ScaleValue(AContentSize, False);
        AddPageDelimiter(ACurDelimiter);
        Inc(APageIndex);
      end;
    end;
  finally
    EndCalculate(ACurDelimiter);
  end;
end;

procedure TdxPSPageDelimitersCalculator.EndCalculate(ACurDelimiter: Integer);
var
  ADelimiter: Integer;
begin
  ADelimiter := PageDelimiters[PageDelimitersList.Count - 1];
  if ADelimiter <> ReportContentSize then
  begin
    Inc(ADelimiter, ReportContentSize);
    if PageDelimitersList.Count > 1 then
      Dec(ADelimiter, ACurDelimiter);
    AddPageDelimiter(ADelimiter);
  end;
  if PageDelimitersList.Count = 1 then
    PageDelimitersList.Clear;
end;

function TdxPSPageDelimitersCalculator.GetPageDelimiters(Index: Integer): Integer;
begin
  Result := Integer(FPageDelimiters.Items[Index]);
end;

function TdxPSPageDelimitersCalculator.GetReportDelimiters(Index: Integer): Integer;
begin
  Result := Integer(FReportDelimiters.Items[Index]);
end;

function TdxPSPageDelimitersCalculator.CalculatePageContentSize(APageIndex: Integer): Integer;
begin
  Result := ReportRenderInfo.CalculatePageContentWidth(APageIndex);
end;

function TdxPSPageDelimitersCalculator.IsHardDelimiter(AValue: Integer): Boolean;
begin
  Result := ReportRenderInfo.IsHardHorizontalDelimiter(AValue);
end;

function TdxPSPageDelimitersCalculator.ScaleValue(
  AValue: Integer; AScaleIn: Boolean = True): Integer;
begin
  if AScaleIn then
    Result := MulDiv(AValue, ReportRenderInfo.ScaleFactor, 100)
  else
    Result := MulDiv(AValue, 100, ReportRenderInfo.ScaleFactor);
end;

{ TdxPSPageVerticalDelimitersCalculator }

procedure TdxPSPageVerticalDelimitersCalculator.BeginCalculate;
begin
  FReportDelimiters := ReportRenderInfo.DelimiterYList;
  FPageDelimiters := ReportRenderInfo.FPageDelimitersY;
  FReportContentSize := ReportRenderInfo.ReportHeight;
end;

function TdxPSPageVerticalDelimitersCalculator.CalculatePageContentSize(APageIndex: Integer): Integer;
begin
  Result := ReportRenderInfo.CalculatePageContentHeight(ReportRenderInfo.PageColCount * APageIndex);
end;

function TdxPSPageVerticalDelimitersCalculator.IsHardDelimiter(AValue: Integer): Boolean;
begin
  Result := ReportRenderInfo.IsHardVerticalDelimiter(AValue)
end;

{ TdxPSStorageVersionCompability }

class procedure TdxPSStorageVersionCompability.UpdateReportCells(AReportCells: TdxReportCells; AReader: TdxPSDataReader);
var
  AUnitsPerInch: Integer;
begin
  AUnitsPerInch := AReader.UnitsPerInch;
  if AUnitsPerInch <> dxPSCore.FUnitsPerInch then
    AReportCells.ConvertCoords(dxPSCore.FUnitsPerInch, AUnitsPerInch);
end;

class procedure TdxPSStorageVersionCompability.UpdateReportRenderInfo(AReportRenderInfo: TdxPSReportRenderInfo; AReader: TdxPSDataReader);

  procedure ConvertList(AList: TList);
  var
    I: Integer;
    AValue: TdxNativeInt;
  begin
    for I := 0 to AList.Count - 1 do
    begin
      AValue := TdxNativeInt(AList[I]);
      AValue := MulDiv(AValue, dxPSCore.FUnitsPerInch, 4800);
      AList[I] := Pointer(AValue);
    end;
  end;

var
  AUnitsPerInch: Integer;
begin
  AUnitsPerInch := AReader.UnitsPerInch;
  if AUnitsPerInch <> dxPSCore.FUnitsPerInch then
  begin
    ConvertList(AReportRenderInfo.FDelimitersX);
    ConvertList(AReportRenderInfo.FDelimitersY);
  end;
end;

{ TdxPSReportRenderInfo }

constructor TdxPSReportRenderInfo.Create(AReportLink: TBasedxReportLink);
begin
  inherited Create;
  FReportLink := AReportLink;
  FBaseContentFont := TFont.Create;
  FDelimitersX := TList.Create;
  FDelimitersY := TList.Create;
  FHardDelimitersX := TList.Create;
  FHardDelimitersY := TList.Create;
  FPageDelimitersX := TList.Create;
  FPageDelimitersY := TList.Create;
  FPageRenderInfos := TList.Create;
end;

destructor TdxPSReportRenderInfo.Destroy;
begin
  FreeAndNilPageRenderInfos;
  FreeAndNil(FBaseContentFont);
  FreeAndNil(FPageDelimitersY);
  FreeAndNil(FPageDelimitersX);
  FreeAndNil(FHardDelimitersX);
  FreeAndNil(FHardDelimitersY);
  FreeAndNil(FDelimitersY);
  FreeAndNil(FDelimitersX);
  inherited Destroy;
end;

procedure TdxPSReportRenderInfo.Calculate;
var
  APrinterPage: TdxPrinterPage;
begin
  if not Locked then
  begin
    Lock;
    try
      APrinterPage := PrinterPage;
      FActualScaleFactor := -1;
      repeat
        Refresh;
        Dec(FActualScaleFactor, 5);
        DoCalculate;
      until (APrinterPage.ScaleMode <> smFit) or IsReportFitsToPages(APrinterPage) or (FActualScaleFactor < TdxPrinterPage.MinScaleFactor);
    finally
      Unlock;
    end;
  end;
end;

function TdxPSReportRenderInfo.CalculateActualScaleFactor: Integer;
begin
  if FActualScaleFactor < 0 then
  begin
    FActualScaleFactor := CalculateActualScaleFactor(
      PrinterPage.FitToPagesHorizontally, PrinterPage.FitToPagesVertically, UnitsPerInch);
  end;
  Result := FActualScaleFactor;
end;

function TdxPSReportRenderInfo.CalculateActualScaleFactor(
  AFitToPagesHorizontally, AFitToPagesVertically, AReportDPI: Integer): Integer;

  function GetTargetPageCountX: Integer;
  begin
    Result := Max(1, AFitToPagesHorizontally);
  end;

  function GetTargetPageCountY: Integer;
  begin
    Result := Max(1, AFitToPagesVertically);
  end;

  function CalculateActualContentPaintAreaSize: TPoint;
  begin
    Result := PaintSize;
    if not TitleAdjustOnReportScale then
      Dec(Result.Y, ConvertValue(CalculateTitleHeight(False)));
    if not FootnotesAdjustOnReportScale then
      Dec(Result.Y, ConvertValue(CalculateFootnotesHeight(False)));
    Result.X := Result.X * GetTargetPageCountX;
    Result.Y := Result.Y * GetTargetPageCountY;
  end;

  function CalculateActualReportWidth: Integer;
  begin
    Result := MulDiv(ReportWidth + RowHeaderWidth * GetTargetPageCountX, UnitsPerInch, AReportDPI);
  end;

  function CalculateActualReportHeight: Integer;
  begin
    CalculateHeaderAndFooterBounds;
    Result := MulDiv(ReportHeight + (HeaderHeight + FooterHeight) * GetTargetPageCountY, UnitsPerInch, AReportDPI);
    if TitleAdjustOnReportScale then
      Inc(Result, ConvertValue(CalculateTitleHeight(False)));
    if FootnotesAdjustOnReportScale then
      Inc(Result, ConvertValue(CalculateFootnotesHeight(False)));
  end;

  procedure CalculateScaleFactor(var AScaleFactor: Integer; APaintAreaSize, AReportSize: Integer);
  var
    AValue: Integer;
  begin
    if AReportSize > 0 then
    begin
      AValue := Trunc(100 * APaintAreaSize / AReportSize);
      if AScaleFactor >= 0 then
        AScaleFactor := Min(AScaleFactor, AValue)
      else
        AScaleFactor := AValue;
    end;
  end;

var
  APaintAreaSize: TPoint;
begin
  Result := -1;
  APaintAreaSize := CalculateActualContentPaintAreaSize;
  if AFitToPagesHorizontally > 0 then
    CalculateScaleFactor(Result, APaintAreaSize.X, CalculateActualReportWidth);
  if AFitToPagesVertically > 0 then
    CalculateScaleFactor(Result, APaintAreaSize.Y, CalculateActualReportHeight);
  if Result < 0 then
    Result := 100;
end;

function TdxPSReportRenderInfo.CalculateCompositionPagePartCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to PageRenderInfoCount - 1 do
  begin
    if PageRenderInfos[I].IsCompositionPagePart then
      Inc(Result);
  end;
end;

function TdxPSReportRenderInfo.CanRenderPage(APageIndex: Integer): Boolean;
begin
  Result := True;
end;

function TdxPSReportRenderInfo.GetActualPageRenderInfo(APageIndex: Integer): TdxPSPageRenderInfo;
var
  ARowInfo: TdxPSCompositionReportRenderRowInfo;
begin
  if GetCompositionInfo(APageIndex, ARowInfo) then
    Result := ARowInfo.RenderInfo.PageRenderInfos[ARowInfo.GetPageIndexRelativeToReportLink(APageIndex)]
  else
    Result := PageRenderInfos[APageIndex];
end;

function TdxPSReportRenderInfo.GetCompositionInfo(
  APageIndex: Integer; out ARowInfo: TdxPSCompositionReportRenderRowInfo): Boolean;
begin
  Result := (ReportLink.CurrentComposition <> nil) and
    ReportLink.CurrentComposition.RenderInfo.FindRowInfo(ReportLink, APageIndex, ARowInfo);
end;

function TdxPSReportRenderInfo.IsDrawPageFootnotesOnPage(APageIndex: Integer): Boolean;
begin
  case ReportLink.ReportFootnotes.Mode of
    fnmNone:
      Result := False;
    fnmOnLastPage:
      Result := (VirtualPageCount = 0) or (APageIndex = VirtualPageCount - 1);
    else
      Result := APageIndex > VirtualPageCount - PageColCount - 1;
  end;
end;

function TdxPSReportRenderInfo.IsDrawPageTitleOnPage(APageIndex: Integer): Boolean;
begin
  case ReportLink.ReportTitleMode of
    tmNone:
      Result := False;
    tmOnFirstPage:
      Result := APageIndex = 0;
    else
      Result := APageIndex < PageColCount;
  end;
end;

function TdxPSReportRenderInfo.IsFooterOnPage(APageIndex: Integer): Boolean;
begin
  Result := True;
end;

function TdxPSReportRenderInfo.IsHeaderOnPage(APageIndex: Integer): Boolean;
begin
  Result := True;
end;

function TdxPSReportRenderInfo.IsRowHeaderOnPage(APageIndex: Integer): Boolean;
begin
  Result := True;
end;

procedure TdxPSReportRenderInfo.Lock;
begin
  Inc(FLockCounter);
end;

procedure TdxPSReportRenderInfo.Unlock;
begin
  if FLockCounter <> 0 then
    Dec(FLockCounter);
end;

procedure TdxPSReportRenderInfo.CalculateHeaderAndFooterBounds;
begin
  if ReportCells <> nil then
  begin
    if ReportLink.IsDrawFootersOnEveryPage or (ReportLink.DataSource = rldsExternalStorage) then
      FooterBounds := ReportCells.FooterBoundsRect;
    if ReportLink.IsDrawHeadersOnEveryPage or (ReportLink.DataSource = rldsExternalStorage) then
      HeaderBounds := ReportCells.HeaderBoundsRect;
    if ReportLink.IsDrawRowHeadersOnEveryPage or (ReportLink.DataSource = rldsExternalStorage) then
      RowHeaderBounds := ReportCells.RowHeaderBoundsRect;
    if ReportLink.IsDrawHeaderCornersOnEveryPage or (ReportLink.DataSource = rldsExternalStorage) then
      HeaderCornerBounds := ReportCells.HeaderCornerBoundsRect;
  end;
end;

function TdxPSReportRenderInfo.CalculatePageContentHeight(APageIndex: Integer): Integer;
var
  AAddContentHeight: Integer;
  AFooterHeight: Integer;
  AHeaderHeight: Integer;
  ARowInfo: TdxPSCompositionReportRenderRowInfo;
begin
  if GetCompositionInfo(APageIndex, ARowInfo) then
    Result := ARowInfo.CalculateContentAreaHeight(ReportLink)
  else
    Result := PaintSize.Y;

  if IsHeaderOnPage(APageIndex) then
    AHeaderHeight := MulDiv(HeaderHeight, ScaleFactor, 100)
  else
    AHeaderHeight := 0;

  if IsFooterOnPage(APageIndex) then
    AFooterHeight := MulDiv(FooterHeight, ScaleFactor, 100)
  else
    AFooterHeight := 0;

  AAddContentHeight := 0;
  if IsDrawPageTitleOnPage(APageIndex) then
    Inc(AAddContentHeight, MulDiv(TitleHeight, ScaleFactor, 100));
  if IsDrawPageFootnotesOnPage(APageIndex) then
    Inc(AAddContentHeight, MulDiv(FootnotesHeight, ScaleFactor, 100));

  if APageIndex = 0 then
    CanUseHFOnEveryPageMode := Result > AHeaderHeight + AFooterHeight + AAddContentHeight;

  if Result < AHeaderHeight + AFooterHeight + AAddContentHeight then
    AFooterHeight := 0;
  if (Result < AHeaderHeight + AAddContentHeight) and (APageIndex > FPageDelimitersX.Count - 1) then
    AHeaderHeight := 0;

  Dec(Result, AHeaderHeight + AFooterHeight + AAddContentHeight);
end;

function TdxPSReportRenderInfo.CalculatePageContentWidth(APageIndex: Integer): Integer;
var
  ARowHeaderWidth: Integer;
begin
  if IsRowHeaderOnPage(APageIndex) then
    ARowHeaderWidth := MulDiv(RowHeaderWidth, ScaleFactor, 100)
  else
    ARowHeaderWidth := 0;

  Result := PaintSize.X;
  if APageIndex = 0 then
    CanUseHFOnEveryPageMode := Result > ARowHeaderWidth;
  if Result < ARowHeaderWidth then
    ARowHeaderWidth := 0;
  Dec(Result, ARowHeaderWidth);
end;

procedure TdxPSReportRenderInfo.CalculatePages(AStage: TdxPSCalculatePagesStage);
begin
  TdxPSPageDelimitersCalculator.Calculate(Self);
  TdxPSPageVerticalDelimitersCalculator.Calculate(Self);
  if AStage = cpsSecondPass then
  begin
    if VirtualPageCount <> PageColCount * PageRowCount then
      AddPageDelimiterY(0);
  end;
  VirtualPageCount := PageColCount * PageRowCount;
  if (PageDelimiterXCount = 0) or (PageDelimiterYCount = 0) then
    VirtualPageCount := 0;
  FPageRenderInfos.Capacity := VirtualPageCount;
end;

function TdxPSReportRenderInfo.CalculatePageDetailBounds(APageCol, APageRow: Integer): TRect;
begin
  Result := Rect(PageDelimitersX[APageCol], PageDelimitersY[APageRow],
    PageDelimitersX[APageCol + 1], PageDelimitersY[APageRow + 1]);
end;

procedure TdxPSReportRenderInfo.CalculatePageRenderInfos;
var
  I: Integer;
begin
  for I := 0 to VirtualPageCount - 1 do
    CreatePageRenderInfo(I).Calculate;
end;

function TdxPSReportRenderInfo.CalculateReportNotesTextHeight(
  const AText: string; AFont: TFont; AAdjustOnReportScale: Boolean): Integer;
var
  APrevFont: HFONT;
  ASavedFontHeight: Integer;
  ATextLength: Integer;
  DC: HDC;
  R: TRect;
begin
  Result := 0;
  ATextLength := Length(AText);
  if ATextLength > 0 then
  begin
    ASavedFontHeight := AFont.Height;
    if AAdjustOnReportScale then
      AFont.Height := MulDiv(ASavedFontHeight, ScaleFactor, 100);
    try
      DC := GetDC(0);
      try
        APrevFont := SelectObject(DC, AFont.Handle);
        R := PrinterPage.PaintRectPixels;
        OffsetRect(R, -R.Left, -R.Top);
        Result := 4 + Windows.DrawText(DC, PChar(AText), ATextLength, R, DT_CALCRECT or DT_EDITCONTROL or DT_WORDBREAK);
        SelectObject(DC, APrevFont);
      finally
        ReleaseDC(0, DC);
      end;
    finally
      if AAdjustOnReportScale then
        AFont.Height := ASavedFontHeight
    end;
  end;
end;

function TdxPSReportRenderInfo.CalculateFootnotesHeight: Integer;
begin
  Result := CalculateFootnotesHeight(FootnotesAdjustOnReportScale);
end;

function TdxPSReportRenderInfo.CalculateFootnotesHeight(AAdjustOnReportScale: Boolean): Integer;
begin
  Result := CalculateReportNotesTextHeight(FootnotesText, FootnotesFont, AAdjustOnReportScale);
  ReportLink.DoMeasureReportLinkFootnotes(Result);
  Result := Max(0, Min(cxRectHeight(PrinterPage.PaintRectPixels) div 2, Result));
end;

function TdxPSReportRenderInfo.CalculateTitleHeight: Integer;
begin
  Result := CalculateTitleHeight(TitleAdjustOnReportScale);
end;

function TdxPSReportRenderInfo.CalculateTitleHeight(AAdjustOnReportScale: Boolean): Integer;
begin
  Result := CalculateReportNotesTextHeight(TitleText, TitleFont, AAdjustOnReportScale);
  ReportLink.DoMeasureReportLinkTitle(Result);
  Result := Max(0, Min(cxRectHeight(PrinterPage.PaintRectPixels) div 2, Result));
end;

procedure TdxPSReportRenderInfo.CalculateFootnotesBounds;
begin
  FootnotesBounds := cxRectSetSize(cxNullRect, PaintSize.X, ConvertValue(CalculateFootnotesHeight));
  FootnotesBounds := cxRectScale(FootnotesBounds, 100, ScaleFactor);
end;

procedure TdxPSReportRenderInfo.CalculateTitleBounds;
begin
  TitleBounds := cxRectSetSize(cxNullRect, PaintSize.X, ConvertValue(CalculateTitleHeight));
  TitleBounds := cxRectScale(TitleBounds, 100, ScaleFactor);
end;

procedure TdxPSReportRenderInfo.DoCalculate;
begin
  if ReportCells <> nil then
  begin
    BaseContentFont := ReportCells.Font;
    GridLinesColor := ReportCells.BorderColor;
  end;

  CalculateTitleBounds;
  CalculateFootnotesBounds;
  CalculateHeaderAndFooterBounds;
  CalculatePages(cpsFirstPass);
  CalculatePages(cpsSecondPass);
  CalculatePageRenderInfos;
end;

function TdxPSReportRenderInfo.GetNonEmptyPageCount: Integer;
begin
  Result := VirtualPageCount;
end;

function TdxPSReportRenderInfo.GetPageColCount: Integer;
begin
  Result := Max(0, PageDelimiterXCount - 1);
  if (Result = 0) and (ReportCells <> nil) and (ReportCells.HeaderCellCount <> 0) and (ReportCells.FooterCellCount <> 0) then
    Result := 1;
end;

function TdxPSReportRenderInfo.GetPageRowCount: Integer;
begin
  Result := Max(0, PageDelimiterYCount - 1);
  if (Result = 0) and (ReportCells <> nil) and (ReportCells.HeaderCellCount <> 0) and (ReportCells.FooterCellCount <> 0) then
    Result := 1;
end;

function TdxPSReportRenderInfo.GetPageSize: TPoint;
begin
  Result := cxPointScale(PrinterPage.RealPageSizeLoMetric, UnitsPerInch, 254);
end;

function TdxPSReportRenderInfo.GetPaintSize: TPoint;
begin
  Result := cxPointScale(cxPoint(cxRectSize(PrinterPage.PaintRectLoMetric)), UnitsPerInch, 254);
end;

function TdxPSReportRenderInfo.GetPointsPerInch: Integer;
begin
  Result := PtPerInch;
end;

function TdxPSReportRenderInfo.GetUnitsPerInch: Integer;
begin
  Result := FUnitsPerInch;
end;

function TdxPSReportRenderInfo.GetWindowScalePair: TdxWindowScalePair;
begin
  Result.Numerator := 100;
  Result.Denominator := ScaleFactor;
end;

procedure TdxPSReportRenderInfo.SetUnitsPerInch(Value: Integer);
begin
  FUnitsPerInch := Value;
end;

procedure TdxPSReportRenderInfo.ClearPageRenderInfos;
var
  I: Integer;
begin
  for I := 0 to PageRenderInfoCount - 1 do
    PageRenderInfos[I].Free;
  FPageRenderInfos.Clear;
end;

function TdxPSReportRenderInfo.CreatePageRenderInfo(APageIndex: Integer): TdxPSPageRenderInfo;
begin
  Result := GetPageRenderInfoClass.Create(Self, APageIndex);
  FPageRenderInfos.Add(Result);
end;

procedure TdxPSReportRenderInfo.FreeAndNilPageRenderInfos;
begin
  ClearPageRenderInfos;
  FreeAndNil(FPageRenderInfos);
end;

function TdxPSReportRenderInfo.GetPageRenderInfoClass: TdxPSPageRenderInfoClass;
begin
  Result := TdxPSPageRenderInfo;
end;

procedure TdxPSReportRenderInfo.Refresh;
begin
  ClearPageRenderInfos;
  FPageDelimitersX.Clear;
  FPageDelimitersY.Clear;

  CanUseHFOnEveryPageMode := True;

  FCompositionPagePartCount := -1;
  VirtualPageCount := 0;

  FooterBounds := cxNullRect;
  FootnotesBounds := cxNullRect;
  HeaderBounds := cxNullRect;
  RowHeaderBounds := cxNullRect;
  HeaderCornerBounds := cxNullRect;
  TitleBounds := cxNullRect;

  PreparePixelsNumeratorAndDenominator;
end;

procedure TdxPSReportRenderInfo.PreparePixelsNumeratorAndDenominator;
begin
  FPixelsNumerator := UnitsPerInch;
  FPixelsDenominator := ReportLink.PixelsPerInch;
end;

function TdxPSReportRenderInfo.HasPageFootnotes(APageIndex: Integer): Boolean;
begin
  Result := not IsRectEmpty(FootnotesBounds) and IsDrawPageFootnotesOnPage(APageIndex);
end;

function TdxPSReportRenderInfo.HasPageTitle(APageIndex: Integer): Boolean;
begin
  Result := not IsRectEmpty(TitleBounds) and IsDrawPageTitleOnPage(APageIndex);
end;

function TdxPSReportRenderInfo.IsHardHorizontalDelimiter(AValue: Integer): Boolean;
begin
  Result := HardDelimiterXList.IndexOf(Pointer(AValue)) <> -1;
end;

function TdxPSReportRenderInfo.IsHardVerticalDelimiter(AValue: Integer): Boolean;
begin
  Result := HardDelimiterYList.IndexOf(Pointer(AValue)) <> -1;
end;

procedure TdxPSReportRenderInfo.CheckStorageCompatibility(AReader: TdxPSDataReader);
begin
  TdxPSStorageVersionCompability.UpdateReportRenderInfo(Self, AReader);
end;

procedure TdxPSReportRenderInfo.ReadData(AReader: TdxPSDataReader);
begin
  ReadDelimiters(AReader);
  CheckStorageCompatibility(AReader);
end;

procedure TdxPSReportRenderInfo.ReadDelimiters(AReader: TdxPSDataReader);

  procedure ReadList(AList: TList);
  var
    I, AValue, ACount: Integer;
  begin
    AList.Clear;
    ACount := AReader.ReadInteger;
    AList.Capacity := ACount;
    for I := 0 to ACount - 1 do
    begin
      AReader.Read(AValue, SizeOf(AValue));
      AList.Add(Pointer(AValue));
    end;
  end;

begin
  ReadList(FDelimitersX);
  ReadList(FDelimitersY);
end;

procedure TdxPSReportRenderInfo.WriteData(AWriter: TdxPSDataWriter);
begin
  WriteDelimiters(AWriter);
end;

procedure TdxPSReportRenderInfo.WriteDelimiters(AWriter: TdxPSDataWriter);

  procedure WriteList(AList: TList);
  var
    I, AValue: Integer;
  begin
    AWriter.WriteInteger(AList.Count);
    for I := 0 to AList.Count - 1 do
    begin
      AValue := Integer(AList.List[I]);
      AWriter.Write(AValue, SizeOf(AValue));
    end;
  end;

begin
  WriteList(FDelimitersX);
  WriteList(FDelimitersY);
end;

procedure TdxPSReportRenderInfo.AddPageDelimiterX(AValue: Integer);
begin
  FPageDelimitersX.Add(TObject(AValue));
end;

procedure TdxPSReportRenderInfo.AddPageDelimiterY(AValue: Integer);
begin
  FPageDelimitersY.Add(TObject(AValue));
end;

procedure TdxPSReportRenderInfo.PageDelimiterXClear;
begin
  FPageDelimitersX.Clear;
end;

procedure TdxPSReportRenderInfo.PageDelimiterYClear;
begin
  FPageDelimitersY.Clear;
end;

procedure TdxPSReportRenderInfo.AddStandardDelimiters;
begin
  DelimiterXList.Add(TObject(Integer(0)));
  DelimiterXList.Add(TObject(Integer(ReportWidth)));
  DelimiterYList.Add(TObject(Integer(0)));
  DelimiterYList.Add(TObject(Integer(ReportHeight)));
end;

procedure TdxPSReportRenderInfo.EliminateDuplicatesAndSortDelimiters(AList: TList);
var
  ADuplicates: TList;
  I: Integer;
  AValue: Pointer;
begin
  ADuplicates := TList.Create;
  try
    for I := 0 to AList.Count - 1 do
    begin
      AValue := AList[I];
      if ADuplicates.IndexOf(AValue) = -1 then
        ADuplicates.Add(AValue);
    end;
    ADuplicates.Sort(dxCompareValues);
    AList.Clear;
    dxCopyList(ADuplicates, AList);
  finally
    ADuplicates.Free;
  end;
end;

procedure TdxPSReportRenderInfo.GetDelimiters;

  procedure CheckDelimiterList(List: TList; AValue: Integer);
  begin
    EliminateDuplicatesAndSortDelimiters(List);
    TruncateDelimiters(List, AValue);
  end;

begin
  DelimiterXList.Clear;
  DelimiterYList.Clear;
  HardDelimiterXList.Clear;
  HardDelimiterYList.Clear;
  if UseHorzDelimiters or UseVertDelimiters then
  begin
    MakeDelimiters;
    if UseHardVertDelimiters or UseHardHorzDelimiters then
    begin
      MakeHardDelimiters;
      CheckDelimiterList(HardDelimiterXList, ReportLink.ReportWidth);
      CheckDelimiterList(HardDelimiterYList, ReportLink.ReportHeight);
    end;
  end;
  AddStandardDelimiters;
  CheckDelimiterList(DelimiterXList, ReportLink.ReportWidth);
  CheckDelimiterList(DelimiterYList, ReportLink.ReportHeight);
end;

procedure TdxPSReportRenderInfo.MakeDelimiters;
begin
  ReportLink.MakeDelimiters(ReportCells, DelimiterXList, DelimiterYList);
end;

procedure TdxPSReportRenderInfo.MakeHardDelimiters;
begin
  ReportLink.MakeHardDelimiters(ReportCells, HardDelimiterXList, HardDelimiterYList);
end;

procedure TBasedxReportLink.PageParamsChanged(Sender: TdxPrinterPage;
  AStyle: TBasedxPrintStyle; AUpdateCodes: TdxPrinterPageUpdateCodes);
begin
  if IsRealPrinterPage(Sender) and IsCurrentLink and
    (SignificantPrinterPageUpdateCodes * AUpdateCodes <> []) then
  begin
    if Active then
    begin
      if GetRebuildOnPageParamsChange(AUpdateCodes) then
        RebuildReport
      else
        CalculateRenderInfos;
    end;
    DoPageParamsChanged;
  end;
end;

function TBasedxReportLink.PossibleCustomDraw(AnItem: TAbstractdxReportCellData): Boolean;
begin
  Result := (DataSource = rldsComponent) and IsSupportedCustomDraw(AnItem);
end;

procedure TBasedxReportLink.PrepareReportGroupsLookAndFeels;
var
  ACanvas: TdxPSReportRenderCustomCanvas;
begin
  if ReportCells <> nil then
  begin
    ACanvas := TdxPSReportRenderScreenCanvas.Create;
    try
      ReportCells.PrepareReportGroupsLookAndFeels(ACanvas);
    finally
      ACanvas.Free;
    end;
  end;
end;

procedure TdxPSReportRenderInfo.TruncateDelimiters(AList: TList; AValue: Integer);
var
  I: Integer;
begin
  I := AList.Count - 1;
  while (I > -1) and (Integer(AList[I]) > AValue) do
  begin
    AList.Delete(I);
    Dec(I);
  end;
end;

function TdxPSReportRenderInfo.LoMetricRectToInternalUnits(const R: TRect): TRect;
begin
  Result.Bottom := LoMetricValueToInternalUnits(R.Bottom);
  Result.Left := LoMetricValueToInternalUnits(R.Left);
  Result.Right := LoMetricValueToInternalUnits(R.Right);
  Result.Top := LoMetricValueToInternalUnits(R.Top);
end;

function TdxPSReportRenderInfo.LoMetricValueToInternalUnits(Value: Integer): Integer;
begin
  Result := MulDiv(Value, 100 * UnitsPerInch, 254 * ScaleFactor);
end;

function TdxPSReportRenderInfo.GetPrinterPage: TdxPrinterPage;
begin
  Result := ReportLink.RealPrinterPage;
end;

function TdxPSReportRenderInfo.GetReportCells: TdxReportCells;
begin
  Result := ReportLink.FReportCells;
end;

function TdxPSReportRenderInfo.GetReportHeight: Integer;
begin
  Result := ReportLink.ReportHeight;
end;

function TdxPSReportRenderInfo.GetReportWidth: Integer;
begin
  Result := ReportLink.ReportWidth;
end;

function TdxPSReportRenderInfo.GetRowHeaderWidth: Integer;
begin
  Result := cxRectWidth(RowHeaderBounds);
end;

function TdxPSReportRenderInfo.GetScaleFactor: Integer;
begin
  Result := ReportLink.RealScaleFactor;
end;

function TdxPSReportRenderInfo.GetTitleAdjustOnReportScale: Boolean;
begin
  Result := ReportLink.ReportTitle.AdjustOnReportScale;
end;

function TdxPSReportRenderInfo.GetTitleFont: TFont;
begin
  Result := ReportLink.ReportTitle.Font;
end;

function TdxPSReportRenderInfo.GetTitleText: string;
begin
  Result := ReportLink.ReportTitleText;
end;

function TdxPSReportRenderInfo.GetBreakPagesByHardDelimiters: Boolean;
begin
  Result := ReportLink.BreakPagesByHardDelimiters and
    (UseHardVertDelimiters or UseHardHorzDelimiters);
end;

function TdxPSReportRenderInfo.GetCompositionPagePartCount: Integer;
begin
  if FCompositionPagePartCount = -1 then
    FCompositionPagePartCount := CalculateCompositionPagePartCount;
  Result := FCompositionPagePartCount;
end;

function TdxPSReportRenderInfo.GetDelimiterX(Index: Integer): Integer;
begin
  Result := Integer(FDelimitersX.List[Index]);
end;

function TdxPSReportRenderInfo.GetDelimiterXCount: Integer;
begin
  Result := FDelimitersX.Count;
end;

function TdxPSReportRenderInfo.GetDelimiterY(Index: Integer): Integer;
begin
  Result := Integer(FDelimitersY.List[Index]);
end;

function TdxPSReportRenderInfo.GetDelimiterYCount: Integer;
begin
  Result := FDelimitersY.Count;
end;

function TdxPSReportRenderInfo.GetFootnotesAdjustOnReportScale: Boolean;
begin
  Result := ReportLink.ReportFootnotes.AdjustOnReportScale;
end;

function TdxPSReportRenderInfo.GetFootnotesFont: TFont;
begin
  Result := ReportLink.ReportFootnotes.Font;
end;

function TdxPSReportRenderInfo.GetFootnotesHeight: Integer;
begin
  Result := cxRectHeight(FootnotesBounds);
end;

function TdxPSReportRenderInfo.GetFootnotesText: string;
begin
  Result := ReportLink.ReportFootnotes.Text;
end;

function TdxPSReportRenderInfo.GetFooterHeight: Integer;
begin
  Result := cxRectHeight(FooterBounds);
end;

function TdxPSReportRenderInfo.GetHeaderHeight: Integer;
begin
  Result := cxRectHeight(HeaderBounds);
end;

function TdxPSReportRenderInfo.GetLocked: Boolean;
begin
  Result := FLockCounter <> 0;
end;

function TdxPSReportRenderInfo.GetPageDelimiterX(Index: Integer): Integer;
begin
  Result := Integer(FPageDelimitersX.List[Index]);
end;

function TdxPSReportRenderInfo.GetPageDelimiterXCount: Integer;
begin
  Result := FPageDelimitersX.Count;
end;

function TdxPSReportRenderInfo.GetPageDelimiterY(Index: Integer): Integer;
begin
  Result := Integer(FPageDelimitersY.List[Index]);
end;

function TdxPSReportRenderInfo.GetPageDelimiterYCount: Integer;
begin
  Result := FPageDelimitersY.Count;
end;

function TdxPSReportRenderInfo.GetPageRenderInfo(Index: Integer): TdxPSPageRenderInfo;
begin
  Result := TdxPSPageRenderInfo(FPageRenderInfos[Index]);
end;

function TdxPSReportRenderInfo.GetPageRenderInfoCount: Integer;
begin
  Result := FPageRenderInfos.Count;
end;

function TdxPSReportRenderInfo.GetUseHardHorzDelimiters: Boolean;
begin
  Result := ReportLink.UseHardHorzDelimiters;
end;

function TdxPSReportRenderInfo.GetUseHardVertDelimiters: Boolean;
begin
  Result := ReportLink.UseHardVertDelimiters;
end;

function TdxPSReportRenderInfo.GetUseHorzDelimiters: Boolean;
begin
  Result := ReportLink.UseHorzDelimiters;
end;

function TdxPSReportRenderInfo.GetUseVertDelimiters: Boolean;
begin
  Result := ReportLink.UseVertDelimiters;
end;

function TdxPSReportRenderInfo.GetTitleHeight: Integer;
begin
  Result := cxRectHeight(TitleBounds);
end;

procedure TdxPSReportRenderInfo.SetBaseContentFont(Value: TFont);
begin
  BaseContentFont.Assign(Value);
end;

function TdxPSReportRenderInfo.IsNonEmptyPage(const ABounds: TRect): Boolean;
var
  I: Integer;
  R: TRect;
begin
  Result := False;
  if ReportCells <> nil then
    for I := 0 to ReportCells.Count - 1 do
    begin
      R := ReportCells.Cells[I].AbsoluteRect;
      Result := IntersectRect(R, R, ABounds);
      if Result then Exit;
    end;
end;

function TdxPSReportRenderInfo.IsReportFitsToPages(APrinterPage: TdxPrinterPage): Boolean;
begin
  Result :=
    ((APrinterPage.FitToPagesVertically = 0) or (PageRowCount <= APrinterPage.FitToPagesVertically)) and
    ((APrinterPage.FitToPagesHorizontally = 0) or (PageColCount <= APrinterPage.FitToPagesHorizontally));
end;

{ TdxPSCellBorderPainter }

constructor TdxPSCellBorderPainter.Create(ARenderer: TdxPSReportRenderer);
begin
  inherited Create;
  FRenderer := ARenderer;
end;

procedure TdxPSCellBorderPainter.DrawBorders(
  ACanvas: TdxPSReportRenderCustomCanvas; const R: TRect);
begin
  with Item do
    DrawFrame(ACanvas, R, CellSides, BorderColor, BorderColor, LineThickness,
      BorderClass.Thickness);
end;

class procedure TdxPSCellBorderPainter.DrawFrame(ACanvas: TdxPSReportRenderCustomCanvas; R: TRect;
  ASides: TdxCellSides; ATopLeftColor, ARightBottomColor: TColor; ALineThickness: Integer; AThickness: Integer = 1);
begin
  ACanvas.DrawFrame(R, ATopLeftColor, ARightBottomColor, AThickness * ALineThickness, dxCellSidesToBorders(ASides));
end;

class procedure TdxPSCellBorderPainter.DrawShadow(
  ACanvas: TdxPSReportRenderCustomCanvas; const R: TRect;
  AShadowDepth: Integer; AShadowColor, ARestSpaceColor: TColor);
begin
  ACanvas.FillRect(GetBottomShadowBounds(R, AShadowDepth), AShadowColor);
  ACanvas.FillRect(GetBottomShadowRestSpaceBounds(R, AShadowDepth), ARestSpaceColor);
  ACanvas.FillRect(GetRightShadowBounds(R, AShadowDepth), AShadowColor);
  ACanvas.FillRect(GetRightShadowRestSpaceBounds(R, AShadowDepth), ARestSpaceColor);
end;

function TdxPSCellBorderPainter.BorderClass: TdxPSCellBorderClass;
begin
  Result := Item.BorderClass;
end;

function TdxPSCellBorderPainter.Item: TdxReportVisualItem;
begin
  Result := FItem;
end;

function TdxPSCellBorderPainter.Renderer: TdxPSReportRenderer;
begin
  Result := FRenderer;
end;

class function TdxPSCellBorderPainter.GetBorderBounds(
  const R: TRect; ASide: TdxCellSide; ALineThickness: Integer): TRect;
begin
  Result := R;
  case ASide of
    csLeft:
      Result.Right := Result.Left + ALineThickness;
    csTop:
      Result.Bottom := Result.Top + ALineThickness;
    csRight:
      Result.Left := Result.Right - ALineThickness;
    csBottom:
      Result.Top := Result.Bottom - ALineThickness;
  end;
end;

class function TdxPSCellBorderPainter.GetBottomShadowBounds(
  const R: TRect; AShadowDepth: Integer): TRect;
begin
  with Result do
  begin
    Left := R.Left + AShadowDepth;
    Top := R.Bottom;
    Right := R.Right + AShadowDepth;
    Bottom := Top + AShadowDepth;
  end;
end;

class function TdxPSCellBorderPainter.GetBottomShadowRestSpaceBounds(
  const R: TRect; AShadowDepth: Integer): TRect;
begin
  with Result do
  begin
    Left := R.Left;
    Top := R.Bottom;
    Right := Left + AShadowDepth;
    Bottom := Top + AShadowDepth;
  end;
end;

class function TdxPSCellBorderPainter.GetRightShadowBounds(
  const R: TRect; AShadowDepth: Integer): TRect;
begin
  with Result do
  begin
    Left := R.Right;
    Top := R.Top + AShadowDepth;
    Right := Left + AShadowDepth;
    Bottom := Bottom;
  end;
end;

class function TdxPSCellBorderPainter.GetRightShadowRestSpaceBounds(
  const R: TRect; AShadowDepth: Integer): TRect;
begin
  with Result do
  begin
    Left := R.Right;
    Top := R.Top;
    Right := Left + AShadowDepth;
    Bottom := Top + AShadowDepth;
  end;
end;

class procedure TdxPSCellBorderPainter.InflateRect(
  var R: TRect; ASides: TdxCellSides; ALineThickness: Integer);
begin
  if csLeft in ASides then
    Inc(R.Left, ALineThickness);
  if csTop in ASides then
    Inc(R.Top, ALineThickness);
  if csRight in ASides then
    Dec(R.Right, ALineThickness);
  if csBottom in ASides then
    Dec(R.Bottom, ALineThickness);
end;

function TdxPSCellBorderPainter.GetLineThickness: Integer;
begin
  Result := Renderer.LineThickness;
end;

{ TdxPSCustomCellBorder }

class procedure TdxPSCustomCellBorder.Register;
begin
  if GetClass(ClassName) = nil then
    RegisterClass(Self);
end;

class procedure TdxPSCustomCellBorder.Unregister;
begin
end;

class function TdxPSCustomCellBorder.Solid: Boolean;
begin
  Result := True;
end;

class function TdxPSCustomCellBorder.Thickness: Integer;
begin
  Result := 0;
end;

class function TdxPSCustomCellBorder.Edge3DSoft: Boolean;
begin
  Result := False;
end;

class function TdxPSCustomCellBorder.Edge3DStyle: TdxCellEdgeStyle;
begin
  Result := cesRaised;
end;

class function TdxPSCustomCellBorder.EdgeMode: TdxCellEdgeMode;
begin
  Result := cemPattern;
end;

class function TdxPSCustomCellBorder.GetBorderEdgeSalient(
  ASide: TdxCellSide; ASalient: TdxPSCellBorderSalientType): Integer;
begin
  if ASalient = bstOuter then
    if ASide in csTopLeft then
      Result := 1 + Thickness div 2
    else
      Result := (Thickness - 1) div 2
  else
    if ASide in csTopLeft then
      Result := (Thickness - 1) div 2
    else
      Result := 1 + Thickness div 2
end;

class function TdxPSCustomCellBorder.GetPainterClass: TdxPSCellBorderPainterClass;
begin
  Result := TdxPSCellBorderPainter;
end;

{ TdxPSNullCellEdge }

class function TdxPSCellNullBorder.Thickness: Integer;
begin
  Result := 0;
end;

{ TdxPSCellBoldFlatBorder }

class function TdxPSCellBoldFlatBorder.Thickness: Integer;
begin
  Result := 2;
end;

{ TdxPSCellUltraFlatBorder }

class function TdxPSCellUltraFlatBorder.Thickness: Integer;
begin
  Result := 1;
end;

{ TdxPSCell3DBorderPainter }

procedure TdxPSCell3DBorderPainter.DrawBorders(
  ACanvas: TdxPSReportRenderCustomCanvas; const R: TRect);
begin
  Draw3DFrame(ACanvas, R, Item.CellSides, Self.BorderClass, Item.LineThickness);
end;

class procedure TdxPSCell3DBorderPainter.Draw3DFrame(
  ACanvas: TdxPSReportRenderCustomCanvas; R: TRect; ASides: TdxCellSides;
  AOuterTLColor, AOuterBRColor: TColor; AInnerTLColor, AInnerBRColor: TColor;
  ALineThickness: Integer);
begin
  DrawFrame(ACanvas, R, ASides, AOuterTLColor, AOuterBRColor, ALineThickness);
  InflateRect(R, ASides, ALineThickness);
  DrawFrame(ACanvas, R, ASides, AInnerTLColor, AInnerBRColor, ALineThickness);
end;

class procedure TdxPSCell3DBorderPainter.Draw3DFrame(
  ACanvas: TdxPSReportRenderCustomCanvas; R: TRect; ASides: TdxCellSides;
  ACellBorders: TdxPSCell3DBorderClass; ALineThickness: Integer);
begin
  with ACellBorders do
    Draw3DFrame(ACanvas, R, ASides, TopLeftOuterColor, BottomRightOuterColor,
      TopLeftInnerColor, BottomRightInnerColor, ALineThickness);
end;

function TdxPSCell3DBorderPainter.BorderClass: TdxPSCell3DBorderClass;
begin
  Result := TdxPSCell3DBorderClass(inherited BorderClass);
end;

{ TdxPSCustomCell3DBorder }

class function TdxPSCustomCell3DBorder.Solid: Boolean;
begin
  Result := False;
end;

class function TdxPSCustomCell3DBorder.Thickness: Integer;
begin
  Result := 2;
end;

class function TdxPSCustomCell3DBorder.BottomRightInnerBrush: HBRUSH;
begin
  Result := NULL_BRUSH;
end;

class function TdxPSCustomCell3DBorder.BottomRightInnerColor: TColor;
begin
  Result := clNone;
end;

class function TdxPSCustomCell3DBorder.BottomRightOuterBrush: HBRUSH;
begin
  Result := NULL_BRUSH;
end;

class function TdxPSCustomCell3DBorder.BottomRightOuterColor: TColor;
begin
  Result := clNone;
end;

class function TdxPSCustomCell3DBorder.TopLeftInnerBrush: HBRUSH;
begin
  Result := NULL_BRUSH;
end;

class function TdxPSCustomCell3DBorder.TopLeftInnerColor: TColor;
begin
  Result := clNone;
end;

class function TdxPSCustomCell3DBorder.TopLeftOuterBrush: HBRUSH;
begin
  Result := NULL_BRUSH;
end;

class function TdxPSCustomCell3DBorder.TopLeftOuterColor: TColor;
begin
  Result := clNone;
end;

class function TdxPSCustomCell3DBorder.EdgeMode: TdxCellEdgeMode;
begin
  Result := cem3DEffects;
end;

class function TdxPSCustomCell3DBorder.GetBorderEdgeSalient(ASide: TdxCellSide;
  ASalient: TdxPSCellBorderSalientType): Integer;
begin
  if ASalient = bstOuter then
    if ASide in csTopLeft then
      Result := Thickness div 2
    else
      Result := (Thickness - 1) div 2
  else
    if ASide in csTopLeft then
      Result := (Thickness - 1) div 2
    else
      Result := Thickness div 2
end;

class function TdxPSCustomCell3DBorder.GetPainterClass: TdxPSCellBorderPainterClass;
begin
  Result := TdxPSCell3DBorderPainter;
end;

{ TdxPSCellRaisedBorder }

class function TdxPSCellRaisedBorder.BottomRightInnerBrush: HBRUSH;
begin
  Result := GetSysColorBrush(COLOR_BTNSHADOW);
end;

class function TdxPSCellRaisedBorder.BottomRightInnerColor: TColor;
begin
  Result := clBtnShadow;
end;

class function TdxPSCellRaisedBorder.BottomRightOuterBrush: HBRUSH;
begin
  Result := GetSysColorBrush(COLOR_WINDOWTEXT);
end;

class function TdxPSCellRaisedBorder.BottomRightOuterColor: TColor;
begin
  Result := clWindowText;
end;

class function TdxPSCellRaisedBorder.TopLeftInnerBrush: HBRUSH;
begin
  Result := GetSysColorBrush(COLOR_BTNHIGHLIGHT);
end;

class function TdxPSCellRaisedBorder.TopLeftInnerColor: TColor;
begin
  Result := clBtnHighlight;
end;

class function TdxPSCellRaisedBorder.TopLeftOuterBrush: HBRUSH;
begin
  Result := GetSysColorBrush(COLOR_WINDOWTEXT);
end;

class function TdxPSCellRaisedBorder.TopLeftOuterColor: TColor;
begin
  Result := clWindowText;
end;

class function TdxPSCellRaisedBorder.Edge3DSoft: Boolean;
begin
  Result := False;
end;

class function TdxPSCellRaisedBorder.Edge3DStyle: TdxCellEdgeStyle;
begin
  Result := cesRaised;
end;

{ TdxPSCellRaisedSoftBorder }

class function TdxPSCellRaisedSoftBorder.BottomRightInnerBrush: HBRUSH;
begin
  Result := GetSysColorBrush(COLOR_BTNFACE);
end;

class function TdxPSCellRaisedSoftBorder.BottomRightInnerColor: TColor;
begin
  Result := clBtnFace;
end;

class function TdxPSCellRaisedSoftBorder.Edge3DSoft: Boolean;
begin
  Result := True;
end;

{ TdxPSCellSunkenBorder }

class function TdxPSCellSunkenBorder.BottomRightInnerBrush: HBRUSH;
begin
  Result := GetSysColorBrush(COLOR_BTNFACE);
end;

class function TdxPSCellSunkenBorder.BottomRightInnerColor: TColor;
begin
  Result := clBtnFace;
end;

class function TdxPSCellSunkenBorder.BottomRightOuterBrush: HBRUSH;
begin
  Result := GetSysColorBrush(COLOR_BTNHIGHLIGHT);
end;

class function TdxPSCellSunkenBorder.BottomRightOuterColor: TColor;
begin
  Result := clBtnHighlight;
end;

class function TdxPSCellSunkenBorder.TopLeftInnerBrush: HBRUSH;
begin
  Result := GetSysColorBrush(COLOR_BTNSHADOW);
end;

class function TdxPSCellSunkenBorder.TopLeftInnerColor: TColor;
begin
  Result := clBtnShadow;
end;

class function TdxPSCellSunkenBorder.TopLeftOuterBrush: HBRUSH;
begin
  Result := GetSysColorBrush(COLOR_WINDOWTEXT);
end;

class function TdxPSCellSunkenBorder.TopLeftOuterColor: TColor;
begin
  Result := clWindowText;
end;

class function TdxPSCellSunkenBorder.Edge3DSoft: Boolean;
begin
  Result := False;
end;

class function TdxPSCellSunkenBorder.Edge3DStyle: TdxCellEdgeStyle;
begin
  Result := cesSunken;
end;

{ TdxPSCellSunkenSoftBorder }

class function TdxPSCellSunkenSoftBorder.BottomRightInnerBrush: HBRUSH;
begin
  Result := GetSysColorBrush(COLOR_BTNFACE);
end;

class function TdxPSCellSunkenSoftBorder.BottomRightInnerColor: TColor;
begin
  Result := clBtnFace;
end;

class function TdxPSCellSunkenSoftBorder.TopLeftInnerBrush: HBRUSH;
begin
  Result := GetSysColorBrush(COLOR_BTNFACE);
end;

class function TdxPSCellSunkenSoftBorder.TopLeftInnerColor: TColor;
begin
  Result := clBtnFace;
end;

class function TdxPSCellSunkenSoftBorder.Edge3DSoft: Boolean;
begin
  Result := True;
end;

{ TdxPSCellEtchedBorderPainter }

class procedure TdxPSCellTwistedBorderPainter.Draw3DFrame(
  ACanvas: TdxPSReportRenderCustomCanvas; R: TRect; ASides: TdxCellSides;
  ACellBorders: TdxPSCell3DBorderClass; ALineThickness: Integer);
begin
  inherited;
end;

class procedure TdxPSCellTwistedBorderPainter.Draw3DFrame(
  ACanvas: TdxPSReportRenderCustomCanvas; R: TRect; ASides: TdxCellSides;
  AOuterTLColor, AOuterBRColor, AInnerTLColor, AInnerBRColor: TColor;
  ALineThickness: Integer);
begin
  Inc(R.Left, ALineThickness);
  Inc(R.Top, ALineThickness);
  DrawFrame(ACanvas, R, ASides, AInnerTLColor, AInnerBRColor, ALineThickness);
  OffsetRect(R, -ALineThickness, -ALineThickness);
  DrawFrame(ACanvas, R, ASides, AOuterTLColor, AOuterBRColor, ALineThickness);
end;

{ TdxPSCellTwistedBorder }

class function TdxPSCellTwistedBorder.GetPainterClass: TdxPSCellBorderPainterClass;
begin
  Result := TdxPSCellTwistedBorderPainter;
end;

{ TdxPSCellEtchedBorder }

class function TdxPSCellEtchedBorder.BottomRightInnerBrush: HBRUSH;
begin
  Result := GetSysColorBrush(COLOR_BTNHIGHLIGHT);
end;

class function TdxPSCellEtchedBorder.BottomRightInnerColor: TColor;
begin
  Result := clBtnHighLight;
end;

class function TdxPSCellEtchedBorder.BottomRightOuterBrush: HBRUSH;
begin
  Result := GetSysColorBrush(COLOR_WINDOWTEXT);
end;

class function TdxPSCellEtchedBorder.BottomRightOuterColor: TColor;
begin
  Result := clWindowText;
end;

class function TdxPSCellEtchedBorder.TopLeftInnerBrush: HBRUSH;
begin
  Result := GetSysColorBrush(COLOR_BTNHIGHLIGHT);
end;

class function TdxPSCellEtchedBorder.TopLeftInnerColor: TColor;
begin
  Result := clBtnHighLight;
end;

class function TdxPSCellEtchedBorder.TopLeftOuterBrush: HBRUSH;
begin
  Result := GetSysColorBrush(COLOR_WINDOWTEXT);
end;

class function TdxPSCellEtchedBorder.TopLeftOuterColor: TColor;
begin
  Result := clWindowText;
end;

{ TdxPSCellBumpedBorder }

class function TdxPSCellBumpedBorder.BottomRightInnerBrush: HBRUSH;
begin
  Result := GetSysColorBrush(COLOR_WINDOWTEXT);
end;

class function TdxPSCellBumpedBorder.BottomRightInnerColor: TColor;
begin
  Result := clWindowText;
end;

class function TdxPSCellBumpedBorder.BottomRightOuterBrush: HBRUSH;
begin
  Result := GetSysColorBrush(COLOR_BTNHIGHLIGHT);
end;

class function TdxPSCellBumpedBorder.BottomRightOuterColor: TColor;
begin
  Result := clBtnHighLight;
end;

class function TdxPSCellBumpedBorder.TopLeftInnerBrush: HBRUSH;
begin
  Result := GetSysColorBrush(COLOR_WINDOWTEXT);
end;

class function TdxPSCellBumpedBorder.TopLeftInnerColor: TColor;
begin
  Result := clWindowText;
end;

class function TdxPSCellBumpedBorder.TopLeftOuterBrush: HBRUSH;
begin
  Result := GetSysColorBrush(COLOR_BTNHIGHLIGHT);
end;

class function TdxPSCellBumpedBorder.TopLeftOuterColor: TColor;
begin
  Result := clBtnHighLight;
end;

{ TdxPSCellBorderPainter }

function TdxPSColorBorderPainter.GetSideColor(ASide: TdxCellSide): TColor;
begin
  Result := FItem.FCellSideColors[ASide];
end;

procedure TdxPSColorBorderPainter.DrawBorders(
  ACanvas: TdxPSReportRenderCustomCanvas; const R: TRect);
var
  ASide: TdxCellSide;
begin
  for ASide := High(TdxCellSide) downto Low(TdxCellSide) do
  begin
    if ASide in Item.CellSides then
      ACanvas.FillRect(GetBorderBounds(R, ASide, Renderer.LineThickness), SideColor[ASide]);
  end;
end;

{ TdxPSColorBorder }

class function TdxPSColorBorder.GetPainterClass: TdxPSCellBorderPainterClass;
begin
  Result := TdxPSColorBorderPainter;
end;

{ TdxPSBackgroundBitmapPool }

constructor TdxPSBackgroundBitmapPool.Create;
begin
  inherited Create;
  FItems := TList.Create;
end;

destructor TdxPSBackgroundBitmapPool.Destroy;
begin
  Clear;
  FreeAndNil(FItems);
  inherited Destroy;
end;

procedure TdxPSBackgroundBitmapPool.Assign(Source: TdxPSBackgroundBitmapPool);
var
  I: Integer;
begin
  Clear;
  for I := 0 to Source.Count - 1 do
    Add(Source.Items[I]);
end;

function TdxPSBackgroundBitmapPool.Add(ABitmap: TGraphic): Integer;
begin
  if not Find(ABitmap, Result) then Result := FItems.Add(ABitmap);
end;

procedure TdxPSBackgroundBitmapPool.Clear;
begin
  FItems.Clear;
end;

procedure TdxPSBackgroundBitmapPool.Delete(AnIndex: Integer);
begin
  FItems.Delete(AnIndex);
end;

function TdxPSBackgroundBitmapPool.Find(ABitmap: TGraphic; out AnIndex: Integer): Boolean;
var
  I: Integer;
begin
  AnIndex := -1;
  for I := 0 to Count - 1 do
    if dxPSUtl.dxAreGraphicsEqual(Items[I], ABitmap) then
    begin
      AnIndex := I;
      Break;
    end;
  Result := AnIndex <> -1;
end;

procedure TdxPSBackgroundBitmapPool.ReadData(AReader: TdxPSDataReader);
var
  Bitmap: TBitmap;
begin
  AReader.ReadListBegin;
  try
    while not AReader.EndOfList do
    begin
      Bitmap := TBitmap.Create;
      try
        AReader.ReadImage(Bitmap);
        Add(Bitmap);
      except
        Bitmap.Free;
        raise;
      end;
    end;
  finally
    AReader.ReadListEnd;
  end;
end;

procedure TdxPSBackgroundBitmapPool.WriteData(AWriter: TdxPSDataWriter);
var
  I: Integer;
begin
  AWriter.WriteListBegin;
  try
    for I := 0 to Count - 1 do
      AWriter.WriteImage(Items[I]);
  finally
    AWriter.WriteListEnd;
  end;
end;

function TdxPSBackgroundBitmapPool.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TdxPSBackgroundBitmapPool.GetItem(Index: Integer): TBitmap;
begin
  if Index <> -1 then
    Result := TBitmap(FItems[Index])
  else
    Result := nil;
end;

{ TdxPSBrushPoolItem }

constructor TdxPSBrushPoolItem.Create(AColor: TColor);
begin
  inherited Create;
  FColor := ColorToRGB(AColor);
end;

destructor TdxPSBrushPoolItem.Destroy;
begin
  FreeAndNil(FBrush);
  inherited Destroy;
end;

function TdxPSBrushPoolItem.GetBrush: TBrush;
begin
  if FBrush = nil then
  begin
    FBrush := TBrush.Create;
    FBrush.Color := Color;
  end;
  Result := FBrush;
end;

{ TdxPSReportBrushPool }

constructor TdxPSReportBrushPool.Create;
begin
  inherited Create;
  FItems := TList.Create;
end;

destructor TdxPSReportBrushPool.Destroy;
begin
  Clear;
  FreeAndNil(FItems);
  inherited;
end;

procedure TdxPSReportBrushPool.Clear;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].Free;
  FItems.Clear;
end;

function TdxPSReportBrushPool.IndexOf(AColor: TColor): Integer;
begin
  AColor := ColorToRGB(AColor);
  for Result := 0 to Count - 1 do
    if Items[Result].Color = AColor then Exit;
  Result := -1;
end;

function TdxPSReportBrushPool.Add(AColor: TColor): Integer;
begin
  Result := Count;
  FItems.Add(TdxPSBrushPoolItem.Create(AColor));
end;

function TdxPSReportBrushPool.GetBrush(AColor: TColor): TBrush;
var
  Index: Integer;
begin
  Index := IndexOf(AColor);
  if Index = -1 then
    Index := Add(AColor);
  Result := Items[Index].Brush;
end;

function TdxPSReportBrushPool.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TdxPSReportBrushPool.GetItem(Index: Integer): TdxPSBrushPoolItem;
begin
  Result := TdxPSBrushPoolItem(FItems[Index]);
end;

{ TdxPSFontPoolItem }

constructor TdxPSFontPoolItem.Create(AFont: TFont; AFontPPI: Integer);
begin
  inherited Create;
  FFont := TFont.Create;
  FFont.Assign(AFont);
  FOriginalHeight := AFont.Height;
  FOriginalPixelsPerInch := AFontPPI;
end;

constructor TdxPSFontPoolItem.Create(const AName: string; AColor: TColor;
  APitch: TFontPitch; AStyle: TFontStyles; AHeight, AFontPPI: Integer);
var
  ALogFont: TLogFont;
begin
  inherited Create;
  FFont := TFont.Create;
  FFont.Color := AColor;
  ZeroMemory(@ALogFont, SizeOf(ALogFont));
  Move(AName[1], ALogFont.lfFaceName[0], Min(SizeOf(ALogFont.lfFaceName), Length(AName) * SizeOf(Char)));
  ALogFont.lfCharset := DEFAULT_CHARSET;
  ALogFont.lfHeight := -AHeight;//-MulDiv(ASize, FFont.PixelsPerInch, 72);
  ALogFont.lfWeight := IfThen(fsBold in AStyle, FW_BOLD, FW_NORMAL);
  ALogFont.lfItalic := Byte(fsItalic in AStyle);
  ALogFont.lfUnderline := Byte(fsUnderline in AStyle);
  ALogFont.lfStrikeOut := Byte(fsStrikeOut in AStyle);
  FFont.Handle := CreateFontIndirectW(ALogFont);
  FOriginalHeight := -AHeight;
  FOriginalPixelsPerInch := AFontPPI;
end;

destructor TdxPSFontPoolItem.Destroy;
begin
  FreeAndNil(FFont);
  inherited Destroy;
end;

procedure TdxPSFontPoolItem.PrepareFont(AUnitsPerInch: Integer);
begin
  dxPSScaleFontEx(Font, FOriginalHeight, AUnitsPerInch, FOriginalPixelsPerInch);
end;

procedure TdxPSFontPoolItem.ReadData(AReader: TdxPSDataReader);
begin
  AReader.ReadFont(Font);
  if AReader.StorageVersion > 1 then
  begin
    FOriginalHeight := AReader.ReadInteger;
    FOriginalPixelsPerInch := AReader.ReadInteger;
  end
  else
    Font.Size := AReader.ReadInteger;
end;

procedure TdxPSFontPoolItem.WriteData(AWriter: TdxPSDataWriter);
begin
  AWriter.WriteFont(Font);
  if AWriter.StorageVersion > 1 then
  begin
    AWriter.WriteInteger(FOriginalHeight);
    AWriter.WriteInteger(FOriginalPixelsPerInch);
  end
  else
    AWriter.WriteInteger(-MulDiv(FOriginalHeight, PtPerInch, FOriginalPixelsPerInch));
end;

{ TdxPSReportFontPool }

constructor TdxPSReportFontPool.Create(AOwner: TBasedxReportLink);
begin
  inherited Create;
  FOwner := AOwner;
  FItems := TList.Create;
end;

destructor TdxPSReportFontPool.Destroy;
begin
  Clear;
  FreeAndNil(FItems);
  inherited;
end;

function TdxPSReportFontPool.Add(AFont: TFont): Integer;
begin
  Result := IndexOf(AFont);
  if Result = -1 then
    Result := CreateFont(AFont);
end;

function TdxPSReportFontPool.Add(const AName: string; AColor: TColor;
  APitch: TFontPitch; AStyle: TFontStyles; AHeight: Integer): Integer;
begin
  Result := IndexOf(AName, AColor, APitch, AStyle, AHeight);
  if Result = -1 then
    Result := CreateFont(AName, AColor, APitch, AStyle, AHeight);
end;

procedure TdxPSReportFontPool.Clear;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].Free;
  FItems.Clear;
end;

function TdxPSReportFontPool.IndexOf(AFont: TFont): Integer;
begin
  for Result := 0 to Count - 1 do
  begin
    if dxAreFontsEqual(Fonts[Result], AFont) then
      Exit;
  end;
  Result := -1;
end;

function TdxPSReportFontPool.IndexOf(const AName: string; AColor: TColor;
  APitch: TFontPitch; AStyle: TFontStyles; AHeight: Integer): Integer;
begin
  for Result := Count - 1 downto 0 do
  begin
    if dxAreFontsEqual(AName, AColor, APitch, AStyle, AHeight, Fonts[Result]) then
       Exit;
  end;
  Result := -1;
end;

function TdxPSReportFontPool.CreateFont(AFont: TFont): Integer;
var
  Item: TdxPSFontPoolItem;
begin
  Result := Count;
  Item := TdxPSFontPoolItem.Create(AFont, FOwner.PixelsPerInch);
  FItems.Add(Item);
  Item.Font.OnChange := FontChanged;
end;

function TdxPSReportFontPool.CreateFont(const AName: string;
  AColor: TColor; APitch: TFontPitch; AStyle: TFontStyles; AHeight: Integer): Integer;
var
  Item: TdxPSFontPoolItem;
begin
  Result := Count;
  Item := TdxPSFontPoolItem.Create(AName, AColor, APitch, AStyle, AHeight, FOwner.PixelsPerInch);
  FItems.Add(Item);
  Item.Font.OnChange := FontChanged;
end;

procedure TdxPSReportFontPool.FontChanged(Sender: TObject);
begin
  if not FLocked then
    Add(TFont(Sender));
end;

procedure TdxPSReportFontPool.PrepareFonts(UPI: Integer);
var
  I: Integer;
begin
  FLocked := True;
  try
    for I := 0 to Count - 1 do
      Items[I].PrepareFont(UPI);
  finally
    FLocked := False;
  end;
end;

procedure TdxPSReportFontPool.ReadData(AReader: TdxPSDataReader);
var
  AFont: TFont;
begin
  Clear;
  AFont := TFont.Create;
  try
    AReader.ReadListBegin;
    try
      while not AReader.EndOfList do
        Items[CreateFont(AFont)].ReadData(AReader);
    finally
      AReader.ReadListEnd;
    end;
  finally
    AFont.Free;
  end;
end;

procedure TdxPSReportFontPool.WriteData(AWriter: TdxPSDataWriter);
var
  I: Integer;
begin
  AWriter.WriteListBegin;
  try
    for I := 0 to Count - 1 do
      Items[I].WriteData(AWriter);
  finally
    AWriter.WriteListEnd;
  end;
end;

function TdxPSReportFontPool.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TdxPSReportFontPool.GetFont(Index: Integer): TFont;
begin
  Result := Items[Index].Font;
end;

function TdxPSReportFontPool.GetItem(Index: Integer): TdxPSFontPoolItem;
begin
  Result := TdxPSFontPoolItem(FItems[Index]);
end;

{  TdxPSCachedGraphicInfo }

constructor TdxPSCachedGraphicInfo.Create(ARenderer: TdxPSReportRenderer);
begin
  inherited Create;
  FRenderer := ARenderer;
end;

destructor TdxPSCachedGraphicInfo.Destroy;
begin
  FreeAndNil(FGraphicHelper);
  inherited Destroy;
end;

procedure TdxPSCachedGraphicInfo.Clear;
begin
  ZoomFactor := 0;
  UnitsPerInch := 0;
  UnitsPerPixel := 0;
end;

function TdxPSCachedGraphicInfo.CheckRendererModeInfo: Boolean;
begin
  Result := (ZoomFactor = Renderer.ZoomFactor) and
    (UnitsPerInch = Renderer.UnitsPerInch) and
    (UnitsPerPixel = Renderer.UnitsPerPixel);
end;

procedure TdxPSCachedGraphicInfo.ValidateGraphicHelper(AGraphic: TGraphic; AImageList: TCustomImageList;
  AImageIndex: Integer; ABackgroundColor, ATransparentColor: TColor; AIsTransparent: Boolean);
begin
  if FGraphicHelper = nil then
    FGraphicHelper := TdxDrawGraphicHelper.Create;

  if Assigned(AImageList) then
  begin
    if (FGraphicHelper.ImageList <> AImageList) or (FGraphicHelper.ImageIndex <> AImageIndex) then
      FGraphicHelper.Initialize(AImageList, AImageIndex, ABackgroundColor, ATransparentColor, AIsTransparent, Renderer.IsPrinting)
  end
  else
    if (FGraphicHelper.Graphic <> AGraphic) or not CheckRendererModeInfo then
    begin
      FGraphicHelper.Initialize(AGraphic, ABackgroundColor, ATransparentColor, AIsTransparent, Renderer.IsPrinting);
      SaveModeInfo;
    end;
end;

procedure TdxPSCachedGraphicInfo.SaveModeInfo;
begin
  UnitsPerInch := Renderer.UnitsPerInch;
  UnitsPerPixel := Renderer.UnitsPerPixel;
  ZoomFactor := Renderer.ZoomFactor;
end;

{ TdxPSCustomReportExportProvider }

constructor TdxPSCustomReportExportProvider.Create(AReportLink: TBasedxReportLink);
begin
  inherited Create;
  FReportLink := AReportLink;
end;

function TdxPSCustomReportExportProvider.GetRenderer: TdxPSReportRenderer;
begin
  Result := ReportLink.Renderer;
end;

function TdxPSCustomReportExportProvider.GetRenderInfo: TdxPSReportRenderInfo;
begin
  Result := ReportLink.RenderInfo;
end;

procedure TdxPSCustomReportExportProvider.SaveToFile(const AFileName: string);
var
  AStream: TStream;
begin
  AStream := TFileStream.Create(AFileName, fmCreate);
  try
    SaveToStream(AStream);
  finally
    AStream.Free;
  end;
end;

{ TdxPSPDFReportExportOptions }

constructor TdxPSPDFReportExportOptions.Create;
begin
  inherited Create;
  FPageRangeInfo := TdxPSPrintPageRangeInfo.Create;
  FSecurityOptions := TdxPSPDFSecurityOptions.Create;
  FJPEGQuality := 90;
  FEmbedFonts := True;
  FUseCIDFonts := True;
  FUseJPEGCompression := True;
end;

destructor TdxPSPDFReportExportOptions.Destroy;
begin
  SaveDefaultSettings;
  FreeAndNil(FSecurityOptions);
  FreeAndNil(FPageRangeInfo);
  inherited Destroy;
end;

procedure TdxPSPDFReportExportOptions.AfterConstruction;
begin
  inherited AfterConstruction;
  LoadDefaultSettings;
end;

procedure TdxPSPDFReportExportOptions.Assign(Source: TPersistent);
var
  AOptions: TdxPSPDFReportExportOptions;
begin
  if Source is TdxPSPDFReportExportOptions then
  begin
    AOptions := TdxPSPDFReportExportOptions(Source);
    Author := AOptions.Author;
    CompressStreams := AOptions.CompressStreams;
    Creator := AOptions.Creator;
    EmbedFonts := AOptions.EmbedFonts;
    JPEGQuality := AOptions.JPEGQuality;
    Keywords := AOptions.Keywords;
    OpenDocumentAfterExport := AOptions.OpenDocumentAfterExport;
    SecurityOptions := AOptions.SecurityOptions;
    PageRangeInfo.Assign(AOptions.PageRangeInfo);
    Subject := AOptions.Subject;
    Title := AOptions.Title;
    UseCIDFonts := AOptions.UseCIDFonts;
    UseJPEGCompression := AOptions.UseJPEGCompression;
    DefaultFileName := AOptions.DefaultFileName;
    DefaultFileNameAssigned := AOptions.DefaultFileNameAssigned;
    FIsCreatorAssigned := AOptions.FIsCreatorAssigned;
    FIsTitleAssigned := AOptions.FIsTitleAssigned;
  end;
end;

procedure TdxPSPDFReportExportOptions.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('IsCreatorAssigned',
    ReadIsCreatorAssigned, WriteIsCreatorAssigned, FIsCreatorAssigned);
  Filer.DefineProperty('IsTitleAssigned',
    ReadIsTitleAssigned, WriteIsTitleAssigned, FIsTitleAssigned);
end;

procedure TdxPSPDFReportExportOptions.LoadDefaultSettings;
var
  AIniFile: TCustomIniFile;
begin
  if dxPSStoringManager.BeginStoring(AIniFile) then
  try
    LoadFromIniFile(AIniFile, sdxPDFSettingSectionName);
  finally
    dxPSStoringManager.EndStoring(AIniFile);
  end;
end;

procedure TdxPSPDFReportExportOptions.SaveDefaultSettings;
var
  AIniFile: TCustomIniFile;
begin
  if dxPSStoringManager.BeginStoring(AIniFile) then
  try
    SaveToIniFile(AIniFile, sdxPDFSettingSectionName);
  finally
    dxPSStoringManager.EndStoring(AIniFile);
  end;
end;

procedure TdxPSPDFReportExportOptions.LoadFromIniFile(
  AIniFile: TCustomIniFile; const ASectionName: string);
var
  AAllowActions: TdxPSPDFDocumentActions;
  I: Integer;
begin
  if AIniFile.SectionExists(ASectionName) then
  begin
    EmbedFonts := AIniFile.ReadBool(ASectionName, sdxEmbedFonts, True);
    UseCIDFonts := AIniFile.ReadBool(ASectionName, sdxUseCIDFonts, True);
    JPEGQuality := AIniFile.ReadInteger(ASectionName, sdxJPEGQuality, 90);
    CompressStreams := AIniFile.ReadBool(ASectionName, sdxCompressStreams, False);
    UseJPEGCompression := AIniFile.ReadBool(ASectionName, sdxUseJPEGCompression, True);
    OpenDocumentAfterExport := AIniFile.ReadBool(ASectionName, sdxOpenDocumentAfterExport, False);

    AAllowActions := [];
    for I := Low(FPDFSecurityMapEntries) to High(FPDFSecurityMapEntries) do
    begin
      if AIniFile.ReadBool(ASectionName, FPDFSecurityMapEntries[I].Name, True) then
        Include(AAllowActions, FPDFSecurityMapEntries[I].Action);
    end;
    SecurityOptions.KeyLength := TdxPSPDFEncryptKeyLength(
      AIniFile.ReadInteger(ASectionName, sdxPDFSecurityKeyLength, Ord(pekl128)));
  end;
end;

procedure TdxPSPDFReportExportOptions.LoadFromIniFile(const AFileName: string);
var
  AIniFile: TMemIniFile;
begin
  AIniFile := TMemIniFile.Create(AFileName);
  try
    LoadFromIniFile(AIniFile, sdxPDFSettingSectionName);
  finally
    AIniFile.Free;
  end;
end;

procedure TdxPSPDFReportExportOptions.SaveToIniFile(
  AIniFile: TCustomIniFile; const ASectionName: string);
var
  I: Integer;
begin
  AIniFile.WriteBool(ASectionName, sdxCompressStreams, CompressStreams);
  AIniFile.WriteBool(ASectionName, sdxEmbedFonts, EmbedFonts);
  AIniFile.WriteBool(ASectionName, sdxUseCIDFonts, UseCIDFonts);
  AIniFile.WriteBool(ASectionName, sdxOpenDocumentAfterExport, OpenDocumentAfterExport);
  AIniFile.WriteBool(ASectionName, sdxUseJPEGCompression, UseJPEGCompression);
  AIniFile.WriteInteger(ASectionName, sdxJPEGQuality, JPEGQuality);
  AIniFile.WriteInteger(ASectionName, sdxPDFSecurityKeyLength, Ord(SecurityOptions.KeyLength));
  for I := Low(FPDFSecurityMapEntries) to High(FPDFSecurityMapEntries) do
  begin
    with FPDFSecurityMapEntries[I] do
      AIniFile.WriteBool(ASectionName, Name, Action in SecurityOptions.AllowActions);
  end;
end;

procedure TdxPSPDFReportExportOptions.SaveToIniFile(const AFileName: string);
var
  AIniFile: TMemIniFile;
begin
  AIniFile := TMemIniFile.Create(AFileName);
  try
    SaveToIniFile(AIniFile, sdxPDFSettingSectionName);
    AIniFile.UpdateFile;
  finally
    AIniFile.Free;
  end;
end;

procedure TdxPSPDFReportExportOptions.LoadFromRegistry(const APath: string);
var
  ARegIniFile: TRegistryIniFile;
begin
  ARegIniFile := TRegistryIniFile.Create('');
  try
    LoadFromIniFile(ARegIniFile, APath);
  finally
    ARegIniFile.Free;
  end;
end;

procedure TdxPSPDFReportExportOptions.SaveToRegistry(const APath: string);
var
  ARegIniFile: TRegistryIniFile;
begin
  if APath <> '' then
  begin
    ARegIniFile := TRegistryIniFile.Create('');
    try
      SaveToIniFile(ARegIniFile, APath);
    finally
      ARegIniFile.Free;
    end;
  end;
end;

function TdxPSPDFReportExportOptions.GenerateDefaultFileName: string;
begin
  Result := Title;
end;

procedure TdxPSPDFReportExportOptions.ReadIsCreatorAssigned(Reader: TReader);
begin
  FIsCreatorAssigned := Reader.ReadBoolean;
end;

procedure TdxPSPDFReportExportOptions.ReadIsTitleAssigned(Reader: TReader);
begin
  FIsTitleAssigned := Reader.ReadBoolean;
end;

procedure TdxPSPDFReportExportOptions.WriteIsCreatorAssigned(Writer: TWriter);
begin
  Writer.WriteBoolean(FIsCreatorAssigned);
end;

procedure TdxPSPDFReportExportOptions.WriteIsTitleAssigned(Writer: TWriter);
begin
  Writer.WriteBoolean(FIsTitleAssigned);
end;

function TdxPSPDFReportExportOptions.GetDefaultFileName: string;
begin
  if not DefaultFileNameAssigned then
    FDefaultFileName := GenerateDefaultFileName;
  Result := FDefaultFileName;
end;

function TdxPSPDFReportExportOptions.GetCreator: string;
begin
  if FIsCreatorAssigned then
    Result := FCreator
  else
    Result := GetDefaultCreator;
end;

function TdxPSPDFReportExportOptions.GetDefaultCreator: string;
begin
  Result := dxGetUserName;
end;

function TdxPSPDFReportExportOptions.GetDefaultTitle: string;
begin
  Result := cxGetResourceString(@sdxNewReport);
end;

function TdxPSPDFReportExportOptions.GetDefaultFileNameIsStored: Boolean;
begin
  Result := DefaultFileNameAssigned;
end;

function TdxPSPDFReportExportOptions.GetIsCreatorStored: Boolean;
begin
  Result := FIsCreatorAssigned and (Creator <> GetDefaultCreator);
end;

function TdxPSPDFReportExportOptions.GetIsTitleStored: Boolean;
begin
  Result := FIsTitleAssigned and (Title <> GetDefaultTitle);
end;

function TdxPSPDFReportExportOptions.GetTitle: string;
begin
  if FIsTitleAssigned then
    Result := FTitle
  else
    Result := GetDefaultTitle;
end;

procedure TdxPSPDFReportExportOptions.SetCreator(const AValue: string);
begin
  if AValue <> Creator then
  begin
    FCreator := AValue;
    FIsCreatorAssigned := True;
  end;
end;

procedure TdxPSPDFReportExportOptions.SetTitle(const AValue: string);
begin
  if AValue <> Title then
  begin
    FTitle := AValue;
    FIsTitleAssigned := True;
  end;
end;

procedure TdxPSPDFReportExportOptions.SetDefaultFileName(const AValue: string);
begin
  if AValue <> FDefaultFileName then
  begin
    FDefaultFileName := AValue;
    DefaultFileNameAssigned := True;
  end;
end;

procedure TdxPSPDFReportExportOptions.SetSecurityOptions(AValue: TdxPSPDFSecurityOptions);
begin
  FSecurityOptions.Assign(AValue);
end;

{ TdxPSReportRenderer }

constructor TdxPSReportRenderer.Create(AReportLink: TBasedxReportLink);

  function CreateFont(const AName: string; ASize: Integer): TFont;
  begin
    Result := TFont.Create;
    Result.Name := AName;
    Result.Size := ASize;
    Result.Charset := SYMBOL_CHARSET;//DEFAULT_CHARSET;
    Result.PixelsPerInch := dxDefaultDPI;
    dxSetFontAsNonAntialiased(Result);
  end;

begin
  inherited Create;
  FCachedGraphicInfo := TdxPSCachedGraphicInfo.Create(Self);
  FBorderPainters := TList.Create;
  FBrushPool := TdxPSReportBrushPool.Create;
  FReportLink := AReportLink;
  FCheckBitmap := TcxBitmap.Create;
  FGroupLookAndFeelPainters := TList.Create;
  FHFStrings := TStringList.Create;
  FMarlettFont10 := CreateFont('Marlett', 10);
  FMarlettFont8 := CreateFont('Marlett', 8);
  FSymbolFont := CreateFont('Symbol', 7);
  FLineThickness := 1;
end;

destructor TdxPSReportRenderer.Destroy;
begin
  FreeAndNil(FSymbolFont);
  FreeAndNil(FMarlettFont10);
  FreeAndNil(FMarlettFont8);
  FreeAndNil(FHFStrings);
  FreeAndNilReportGroupLookAndFeelPainters;
  FreeAndNil(FCheckBitmap);
  FreeAndNil(FBrushPool);
  FreeAndNil(FCachedGraphicInfo);
  FreeAndNilBorderPainters;
  inherited Destroy;
end;

function TdxPSReportRenderer.CustomDrawReportItem(AItem: TAbstractdxReportCellData): Boolean;
var
  ACanvas: TCanvas;
  R: TRect;
begin
  Result := False;
  if Assigned(AItem.Creator) then
  begin
    R := AItem.GetOuterBounds(Canvas);
    ACanvas := Canvas.BeginCustomDraw(R, AItem.Font, AItem.Color);
    try
      AItem.Creator.CustomDraw(AItem, ACanvas, R, AItem.BoundsRect, Result);
    finally
      Canvas.EndCustomDraw(ACanvas);
    end;
  end;
end;

procedure TdxPSReportRenderer.Get3DBorderBrushes(AnItem: TdxReportVisualItem;
  var AOuterTLBrush, AOuterBRBrush, AInnerTLBrush, AInnerBRBrush: HBRUSH);
begin
  dxPSCore.Get3DBorderBrushes(AnItem.Edge3DStyle, AnItem.Edge3DSoft,
    AOuterTLBrush, AOuterBRBrush, AInnerTLBrush, AInnerBRBrush);
end;

procedure TdxPSReportRenderer.Get3DBorderColors(AnItem: TdxReportVisualItem;
  var AOuterTLColor, AOuterBRColor, AInnerTLColor, AInnerBRColor: TColor);
begin
  dxPSCore.Get3DBorderColors(AnItem.Edge3DStyle, AnItem.Edge3DSoft,
    AOuterTLColor, AOuterBRColor, AInnerTLColor, AInnerBRColor);
end;

function TdxPSReportRenderer.CalcTextHeight(
  ACanvas: TdxPSReportRenderCustomCanvas; const AText: string;
  AWordBreak: Boolean; AFont: TFont = nil; ABaseWidth: Integer = -1): Integer;
var
  R: TRect;
begin
  if ABaseWidth = -1 then
    ABaseWidth := 5;
  R := Rect(0, 0, ABaseWidth, 5);
  Result := CalcTextRect(ACanvas, AText, R, AWordBreak, AFont);
end;

function TdxPSReportRenderer.CalcTextLineCount(
  ACanvas: TdxPSReportRenderCustomCanvas; const AText: string;
  AFont: TFont = nil; ABaseWidth: Integer = -1): Integer;
var
  R: TRect;
begin
  Result := 0;
  if AText <> '' then
  begin
    if ABaseWidth = -1 then
      ABaseWidth := 2 * dxTextSpace + 1;
    R := Rect(0, 0, ABaseWidth, 5);
    ACanvas.CalculateTextRect(AText, R, dxCalcFormat[True] or CXTO_CALCROWCOUNT, AFont);
  end;
end;

function TdxPSReportRenderer.CalcTextPatternHeight(
  ACanvas: TdxPSReportRenderCustomCanvas; AFont: TFont = nil): Integer;
begin
  Result := CalcTextHeight(ACanvas, dxMeasurePattern, False, AFont, -1);
end;

function TdxPSReportRenderer.CalcTextRect(ACanvas: TdxPSReportRenderCustomCanvas;
  const AText: string; var ARect: TRect; AWordBreak: Boolean; AFont: TFont = nil): Integer;
const
  CalcFormats: array[Boolean] of UINT = (0, CXTO_EXPANDTABS or CXTO_WORDBREAK);
begin
  Result := CalcTextRect(ACanvas, AText, ARect, CXTO_CALCROWCOUNT or
    CXTO_AUTOINDENTS or CXTO_CHARBREAK or CalcFormats[AWordBreak], AFont);
end;

function TdxPSReportRenderer.CalcTextRect(ACanvas: TdxPSReportRenderCustomCanvas;
  const AText: string; var ARect: TRect; AFormat: DWORD; AFont: TFont = nil): Integer;
var
  ATextParams: TcxTextParams;
  ATextRows: TcxTextRows;
  ALineCount: Integer;
begin
  Result := 0;
  if (AText <> '') and (AText[1] <> #0) then
  begin
    ACanvas.SaveState;
    try
      ACanvas.Font := AFont;
      ARect.Bottom := MaxInt;
      InflateRect(ARect, -dxTextSpace * LineThickness, 0);
      ATextParams := ACanvas.CalculateTextParams(AFormat);
      ACanvas.MakeTextRows(PChar(AText), Length(AText), ARect, ATextParams, ATextRows, ALineCount);
      ARect.Bottom := ARect.Top + 3 * LineThickness * dxTextSpace + ALineCount * ATextParams.FullRowHeight;
      cxResetTextRows(ATextRows);
      Result := cxRectHeight(ARect);
    finally
      ACanvas.RestoreState;
    end;
  end;
end;

function TdxPSReportRenderer.CalcTextWidth(
  ACanvas: TdxPSReportRenderCustomCanvas; const AText: string; AFont: TFont = nil): Integer;
var
  AOnePixel: Integer;
  ARect: TRect;
begin
  AOnePixel := ACanvas.CalculateTextParams(dxCalcFormat[False]).OnePixel;
  ARect := Rect(0, 0, 10 * AOnePixel, 10 * AOnePixel);
  ACanvas.CalculateTextRect(AText, ARect, dxCalcFormat[False], AFont);
  Result := cxRectWidth(ARect);
end;

procedure TdxPSReportRenderer.DrawCheckBox(ACanvas: TdxPSReportRenderCustomCanvas;
  var R: TRect; AChecked, AEnabled, AIsRadio: Boolean; AEdgeStyle: TdxCheckButtonEdgeStyle;
  ABorderColor: TColor = clWindowText);
begin
  ACanvas.DrawCheckBox(R, AChecked, AEnabled, AIsRadio, AEdgeStyle, MarlettFont10, LineThickness, ABorderColor);
end;

procedure TdxPSReportRenderer.DrawEdge(ACanvas: TdxPSReportRenderCustomCanvas;
  var R: TRect; AEdgeMode: TdxCellEdgeMode; AEdge3DEdge: TdxCellEdgeStyle;
  ASides: TdxCellSides; ASoft: Boolean; ABorderColor: TColor = -1);
var
  ACellBorders: TdxPSCell3DBorderClass;
begin
  case AEdgeMode of
    cem3DEffects:
      begin
        ACellBorders := TdxPSCell3DBorderClass(
          TdxReportVisualItem.MapBorderClass(AEdgeMode, AEdge3DEdge, ASoft));
        TdxPSCell3DBorderPainter.Draw3DFrame(ACanvas, R,
          ASides, ACellBorders, LineThickness);
      end;

    cemPattern:
      begin
        if ABorderColor = -1 then
          ABorderColor := RenderInfo.GridLinesColor;
        TdxPSCellBorderPainter.DrawFrame(ACanvas, R,
          ASides, ABorderColor, ABorderColor, LineThickness);
      end;
  end;
end;

procedure TdxPSReportRenderer.DrawEllipse(ACanvas: TdxPSReportRenderCustomCanvas;
  R: TRect; AForeColor, ABackColor: TColor; APattern: TdxPSFillPatternClass;
  ABorderColor: TColor; ABorderThickness: Integer = 1);
begin
  FrameEllipse(ACanvas, R, ABorderColor, ABorderThickness);
  ABorderThickness := ABorderThickness * LineThickness;
  InflateRect(R, -ABorderThickness, -ABorderThickness);
  FillEllipseEx(ACanvas, R, AForeColor, ABackColor, APattern);
end;

procedure TdxPSReportRenderer.DrawExpandButton(ACanvas: TdxPSReportRenderCustomCanvas;
  var R: TRect; AExpanded, ADrawBorder, AEdge3D, AEdge3DSoft: Boolean;
  AShadow, AFillInterior: Boolean; ABorderColor, AInteriorColor: TColor);

  function GetEdgeStyle: TdxCheckButtonEdgeStyle;
  const
    Edge3DStyleMap: array[Boolean] of TdxCheckButtonEdgeStyle = (cbes3D, cbesSoft3D);
  begin
    if not ADrawBorder then
      Result := cbesNone
    else
      if AEdge3D then
        Result := Edge3DStyleMap[AEdge3DSoft]
      else
        Result := cbesUltraFlat;
  end;

begin
  ACanvas.DrawExpandButton(R, GetEdgeStyle, MarlettFont8, SymbolFont, AExpanded,
    AShadow, AFillInterior, ABorderColor, AInteriorColor, LineThickness);
end;

procedure TdxPSReportRenderer.DrawGlyph(
  ACanvas: TdxPSReportRenderCustomCanvas; const R: TRect; AGlyph: Byte);
begin
  ACanvas.DrawGlyph(R, nil, AGlyph, True);
end;

procedure TdxPSReportRenderer.DrawGraphic(ACanvas: TdxPSReportRenderCustomCanvas;
  var R: TRect; const AClipRect: TRect; AImageList: TCustomImageList;
  AImageIndex: Integer; AGraphic: TGraphic; AGraphicTransparent: Boolean;
  ATransparent: Boolean; AColor: TColor);
begin
  DrawGraphicEx(ACanvas, R, AClipRect, AImageList, AImageIndex, AGraphic,
    AGraphicTransparent, ATransparent, AColor, clWindow, TdxPSSolidFillPattern,
    cibAlways);
end;

procedure TdxPSReportRenderer.DrawGraphicEx(ACanvas: TdxPSReportRenderCustomCanvas; R: TRect;
  const AClipRect: TRect; AImageList: TCustomImageList; AImageIndex: Integer;
  AGraphic: TGraphic; AGraphicTransparent, ATransparent: Boolean;
  AColor, ABkColor: TColor; APattern: TdxPSFillPatternClass;
  AActualImageBuffering: TdxCellImageActualBuffering = cibAlways; ATransparentColor: TColor = clNone);

  function IsImageTransparent(AGraphic: TGraphic): Boolean;
  begin
    Result := AGraphicTransparent and not ((AGraphic is TIcon) or
      (IsPrinting and not dxCanPrintTransparentImages(ACanvas)));
  end;

begin
  ACanvas.SaveClipRgn;
  try
    if (cxRectWidth(R) > cxRectWidth(AClipRect)) or (cxRectHeight(R) > cxRectHeight(AClipRect)) then
      ACanvas.IntersectClipRgn(AClipRect);

    if not ATransparent then
      FillRectEx(ACanvas, AClipRect, AColor, ABkColor, APattern);

    if ACanvas.IsRectVisible(R) then
    begin
      CachedGraphicInfo.ValidateGraphicHelper(AGraphic, AImageList, AImageIndex, ABkColor, ATransparentColor, IsImageTransparent(AGraphic));
      ACanvas.DrawGraphic(CachedGraphicInfo.GraphicHelper, R);
    end;
  finally
    ACanvas.RestoreClipRgn;
    if AActualImageBuffering <> cibAlways then
      CachedGraphicInfo.Clear;
  end;
end;

procedure TdxPSReportRenderer.DrawRectangle(ACanvas: TdxPSReportRenderCustomCanvas;
  R: TRect; AForeColor, ABackColor: TColor; AContentPattern: TdxPSFillPatternClass;
  ABorderColor: TColor; ABorderThickness: Integer = 1);
begin
  FrameRect(ACanvas, R, ABorderColor, csAll, ABorderThickness);
  InflateRect(R, -ABorderThickness * LineThickness, -ABorderThickness * LineThickness);
  FillRectEx(ACanvas, R, AForeColor, ABackColor, AContentPattern);
end;

procedure TdxPSReportRenderer.DrawRoundRect(ACanvas: TdxPSReportRenderCustomCanvas;
  R: TRect; AEllipseWidth, AEllipseHeight: Integer; AForeColor, ABackColor: TColor;
  AContentPattern: TdxPSFillPatternClass; ABorderColor: TColor;
  ABorderThickness: Integer = 1);
begin
  FrameRoundRect(ACanvas, R, AEllipseWidth, AEllipseHeight, ABorderColor, ABorderThickness);

  AEllipseHeight := MulDiv(AEllipseHeight, R.Bottom - R.Top - 2 * ABorderThickness * LineThickness, R.Bottom - R.Top);
  AEllipseWidth :=  MulDiv(AEllipseWidth, R.Right - R.Left - 2 * ABorderThickness * LineThickness, R.Right - R.Left);
  InflateRect(R, -ABorderThickness * LineThickness, -ABorderThickness * LineThickness);

  FillRoundRectEx(ACanvas, R, AEllipseWidth, AEllipseHeight, AForeColor, ABackColor, AContentPattern);
end;

procedure TdxPSReportRenderer.DrawSortMark(ACanvas: TdxPSReportRenderCustomCanvas;
  var R: TRect; ASortOrder: TdxCellSortOrder; AMono: Boolean);
const
  GlyphIndexes: array[TdxCellSortOrder] of Byte = (0, SortUpMarkIndex, SortDownMarkIndex);
begin
  if ASortOrder <> csoNone then
    ACanvas.DrawGlyph(R, MarlettFont10, GlyphIndexes[ASortOrder], True);
end;

procedure TdxPSReportRenderer.DrawText(ACanvas: TdxPSReportRenderCustomCanvas;
  var R: TRect; AMaxLineCount: Integer; ALeftIndent, ARightIndent: Integer;
  const AText: string; AFont: TFont; ABkColor: TColor; ATextAlignX: TcxTextAlignX;
  ATextAlignY: TcxTextAlignY; AFillBackground, AMultiline, AEndEllipsis: Boolean;
  APreventLeftTextExceed: Boolean = True; APreventTopTextExceed: Boolean = True;
  AHidePrefix: Boolean = True);
begin
  if AFillBackground then
    ACanvas.FillRect(R, ABkColor);
  DrawTextEx(ACanvas, R, AMaxLineCount, ALeftIndent, ARightIndent, AText, AFont,
    CXTO_AUTOINDENTS or CXTO_EXPANDTABS or CXTO_PATTERNEDTEXT or CXTO_CHARBREAK or
    MakeTextFormat(ATextAlignX, ATextAlignY, AMultiline, AEndEllipsis,
      APreventLeftTextExceed, APreventTopTextExceed, AHidePrefix));
end;

procedure TdxPSReportRenderer.DrawTextEx(ACanvas: TdxPSReportRenderCustomCanvas; var R: TRect;
  AMaxLineCount, ALeftIndent, ARightIndent: Integer; const AText: string; AFont: TFont; AFormat: DWORD);
begin
  DrawTextEx(ACanvas, R, AMaxLineCount, AText, AFont, AFormat, Rect(ALeftIndent, 0, ARightIndent, 0));
end;

procedure TdxPSReportRenderer.DrawTextEx(ACanvas: TdxPSReportRenderCustomCanvas; var R: TRect; AMaxLineCount: Integer;
  const AText: string; AFont: TFont; AFormat: Cardinal; const AIndents: TRect; ALineSpacing: Single = 1.0);

  function GetTextColor: TColor;
  begin
    if AFont <> nil then
      Result := AFont.Color
    else
      Result := clDefault;
  end;

var
  ARect: TRect;
begin
  ARect := cxRectContent(R, AIndents);
  ACanvas.DrawText(ARect, AText, AFont, AFormat, GetTextColor, AMaxLineCount, ALineSpacing);
end;

function TdxPSReportRenderer.MakeTextFormat(ATextAlignX: TcxTextAlignX;
  ATextAlignY: TcxTextAlignY; AMultiline, AEndEllipsis, APreventLeftTextExceed,
  APreventTopTextExceed, AHidePrefix: Boolean): DWORD;
const
  dxEndEllipsis: array[Boolean] of UINT = (0, CXTO_END_ELLIPSIS);
  dxHidePrefix: array[Boolean] of UINT = (0, CXTO_HIDEPREFIX);
  dxPreventLeftExceed: array[Boolean] of UINT = (0, CXTO_PREVENT_LEFT_EXCEED);
  dxPreventTopExceed: array[Boolean] of UINT = (0, CXTO_PREVENT_TOP_EXCEED);
  dxWordBreak: array[Boolean] of UINT = (0, CXTO_WORDBREAK);
begin
  Result :=
    cxMakeFormat(ATextAlignX, ATextAlignY) or
    dxEndEllipsis[AEndEllipsis] or
    dxHidePrefix[AHidePrefix] or
    dxPreventLeftExceed[APreventLeftTextExceed] or
    dxPreventTopExceed[APreventTopTextExceed] or
    dxWordBreak[AMultiline];
end;

procedure TdxPSReportRenderer.FillEllipse(
  ACanvas: TdxPSReportRenderCustomCanvas; const R: TRect; AColor: TColor);
begin
  FillEllipseEx(ACanvas, R, AColor, clWhite, TdxPSSolidFillPattern);
end;

procedure TdxPSReportRenderer.FillEllipseEx(ACanvas: TdxPSReportRenderCustomCanvas;
  const R: TRect; AForeColor, ABackColor: TColor; APattern: TdxPSFillPatternClass);
begin
  ACanvas.FillEllipse(R, ABackColor, AForeColor,
    APattern, GetPatternBrush(APattern, AForeColor));
end;

procedure TdxPSReportRenderer.FillRect(
  ACanvas: TdxPSReportRenderCustomCanvas; const R: TRect; AColor: TColor);
begin
  FillRectEx(ACanvas, R, AColor, clWhite, TdxPSSolidFillPattern);
end;

procedure TdxPSReportRenderer.FillRectByBrush(
  ACanvas: TdxPSReportRenderCustomCanvas; const R: TRect; ABrush: TBrush);

  function IsSolidBrush(ABrush: TBrush): Boolean;
  begin
    Result := (ABrush.Style = bsSolid) and
      ((ABrush.Bitmap = nil) or ABrush.Bitmap.Empty);
  end;

var
  ARegion: TcxRegionHandle;
begin
  if ABrush.Style <> bsClear then
  begin
    if IsSolidBrush(ABrush) then
      FillRect(ACanvas, R, ABrush.Color)
    else
    begin
      ARegion := CreateRectRgnIndirect(R);
      ACanvas.FillRegion(ARegion, clDefault, clNone, nil, ABrush);
      DeleteObject(ARegion);
    end;
  end;
end;

procedure TdxPSReportRenderer.FillRectEx(ACanvas: TdxPSReportRenderCustomCanvas;
  const R: TRect; AForeColor, ABackColor: TColor; APattern: TdxPSFillPatternClass);
var
  ARgn: TcxRegionHandle;
begin
  if Assigned(APattern) and ACanvas.IsRectVisible(R) then
  begin
    ARgn := CreateRectRgnIndirect(R);
    FillRgnEx(ACanvas, ARgn, AForeColor, ABackColor, APattern);
    DeleteObject(ARgn);
  end;
end;

procedure TdxPSReportRenderer.FillRoundRect(
  ACanvas: TdxPSReportRenderCustomCanvas; const R: TRect;
  AEllipseWidth, AEllipseHeight: Integer; AColor: TColor);
begin
  FillRoundRectEx(ACanvas, R, AEllipseWidth, AEllipseHeight,
    AColor, clWhite, TdxPSSolidFillPattern);
end;

procedure TdxPSReportRenderer.FillRoundRectEx(ACanvas: TdxPSReportRenderCustomCanvas;
  const R: TRect; AEllipseWidth, AEllipseHeight: Integer; AForeColor, ABackColor: TColor; APattern: TdxPSFillPatternClass);
begin
  ACanvas.FillRoundRect(R, AEllipseWidth, AEllipseHeight,
    ABackColor, AForeColor, APattern, GetPatternBrush(APattern, AForeColor));
end;

procedure TdxPSReportRenderer.FillRgn(ACanvas: TdxPSReportRenderCustomCanvas; Rgn: TcxRegionHandle; AColor: TColor);
begin
  FillRgnEx(ACanvas, Rgn, AColor, clWhite, TdxPSSolidFillPattern);
end;

procedure TdxPSReportRenderer.FillRgnEx(ACanvas: TdxPSReportRenderCustomCanvas;
  Rgn: TcxRegionHandle; AForeColor, ABackColor: TColor; APattern: TdxPSFillPatternClass);
begin
  ACanvas.FillRegion(Rgn, ABackColor, AForeColor, APattern, GetPatternBrush(APattern, AForeColor));
end;

procedure TdxPSReportRenderer.FrameEllipse(ACanvas: TdxPSReportRenderCustomCanvas;
  R: TRect; AColor: TColor; AThickness: Integer = 1);
begin
  ACanvas.DrawEllipseFrame(R, AColor, AThickness * LineThickness);
end;

procedure TdxPSReportRenderer.FrameRect(ACanvas: TdxPSReportRenderCustomCanvas;
  R: TRect; AColor: TColor; ASides: TdxCellSides = [csLeft..csBottom];
  AThickness: Integer = 1);
begin
  ACanvas.DrawFrame(R, AColor, AColor, AThickness * LineThickness,
    dxCellSidesToBorders(ASides));
end;

procedure TdxPSReportRenderer.FrameRoundRect(
  ACanvas: TdxPSReportRenderCustomCanvas; R: TRect;
  AEllipseWidth, AEllipseHeight: Integer; AColor: TColor;
  AThickness: Integer = 1);
begin
  ACanvas.DrawRoundFrame(R, AEllipseWidth, AEllipseHeight, AColor, AThickness * LineThickness);
end;

procedure TdxPSReportRenderer.Polyline(ACanvas: TdxPSReportRenderCustomCanvas;
  const APoints: array of TPoint; AColor: TColor; AThickness: Integer = 1);
begin
  ACanvas.Polyline(APoints, AColor, AThickness * LineThickness);
end;

procedure TdxPSReportRenderer.Polygone(ACanvas: TdxPSReportRenderCustomCanvas;
  const APoints: array of TPoint; ABkColor, AFrameColor: TColor; AThickness: Integer = 1);
begin
  if (AFrameColor = clNone) or (AFrameColor = clDefault) then
    AFrameColor := ABkColor;
  ACanvas.Polygon(APoints, AFrameColor, ABkColor, AThickness * LineThickness);
end;

function TdxPSReportRenderer.GetBorderPainter(AClass: TdxPSCellBorderPainterClass): TdxPSCellBorderPainter;
begin
  Result := FindBorderPainter(AClass);
  if Result = nil then
    Result := CreateBorderPainter(AClass);
end;

function TdxPSReportRenderer.GetBrushByColor(AColor: TColor): HBRUSH;
begin
  if (AColor = clNone) or (AColor = clDefault) then
    Result := GetStockObject(NULL_BRUSH)
  else
    Result := BrushPool.Brushes[AColor].Handle;
end;

function TdxPSReportRenderer.GetPatternBrush(APattern: TdxPSFillPatternClass; AColor: TColor): TBrush;
begin
  if APattern.Solid then
    Result := BrushPool.Brushes[AColor]
  else
    Result := dxPSFillPatternFactory.Items[APattern, IsPrinting].Brush;
end;

function TdxPSReportRenderer.GetReportGroupLookAndFeelPainter(AClass: TdxPSReportGroupLookAndFeelPainterClass): TdxPSReportGroupLookAndFeelPainter;
begin
  Result := FindReportGroupLookAndFeelPainter(AClass);
  if Result = nil then
    Result := CreateReportGroupLookAndFeelPainter(AClass);
end;

procedure TdxPSReportRenderer.RenderPage(ACanvas: TCanvas;
  const APageBounds: TRect; APageIndex, AContinuousPageIndex, AZoomFactor: Integer);
var
  ARenderCanvas: TdxPSReportRenderCanvas;
begin
  ARenderCanvas := TdxPSReportRenderCanvas.Create(ACanvas);
  try
    RenderPageEx(ARenderCanvas, APageBounds, APageIndex, AContinuousPageIndex, AZoomFactor);
  finally
    ARenderCanvas.Free;
  end;
end;

procedure TdxPSReportRenderer.RenderPageEx(ACanvas: TdxPSReportRenderCustomCanvas;
  const APageBounds: TRect; APageIndex, AContinuousPageIndex, AZoomFactor: Integer);
var
  AComposition: TdxCompositionReportLink;
begin
  if RenderInfo.CanRenderPage(APageIndex) then
  begin
    AComposition := ReportLink.CurrentComposition;
    if (AComposition = nil) or not AComposition.ContinuousPageIndexes then
      AContinuousPageIndex := APageIndex;

    dxHFFormatObject.CurrentPage := PreparedPageIndex(AContinuousPageIndex) + 1;

    FCanvas := ACanvas;
    FPixelsPerInch := ACanvas.PixelsPerInch;
    FViewPortRect := APageBounds;
    FZoomFactor := AZoomFactor;
    FRenderingPageIndex := APageIndex;
    PrepareRenderPage;
    try
      if not IsCompositionPagePart then
      begin
        RenderPageBackground(APageIndex);
        RenderPageHeader(APageIndex);
        RenderPageFooter(APageIndex);
      end;
      RenderPageReportArea(APageIndex);
    finally
      FRenderStage := [];
      UnprepareRenderPage;
    end;
    if not IsCompositionPagePart then
      RenderEntirePage(APageIndex);
  end;
end;

procedure TdxPSReportRenderer.RenderPageReportArea(ARealPageIndex: Integer);
begin
  if RenderInfo.HasPageTitle(ARealPageIndex) then
    ReportLink.ReportTitle.Draw(Self, cxRectOffset(RenderInfo.TitleBounds, PageRenderInfo.TitleOffset));

  if ReportLink.NeedTwoPassRendering then
  begin
    FRenderStage := [rsFirstPass];
    RenderPageContent;
    FRenderStage := [rsSecondPass];
    RenderPageContent;
  end
  else
  begin
    FRenderStage := [rsFirstPass, rsSecondPass];
    RenderPageContent;
  end;

  if RenderInfo.HasPageFootnotes(ARealPageIndex) then
    ReportLink.ReportFootnotes.Draw(Self, cxRectOffset(RenderInfo.FootnotesBounds, PageRenderInfo.FootnotesOffset));
end;

procedure TdxPSReportRenderer.RenderDelimiters;
var
  I: Integer;
begin
  for I := 0 to RenderInfo.DelimiterXCount - 1 do
    Canvas.FillRect(Bounds(RenderInfo.DelimitersX[I], 0, 2 * LineThickness, 20 * LineThickness), clHighlight);
end;

procedure TdxPSReportRenderer.RenderPageBackground(ARealPageIndex: Integer);
var
  ABackground: TdxBackground;
  APageRenderInfo: TdxPSPageRenderInfo;
  ARect: TRect;
begin
  if IsPrinting then
  begin
    APageRenderInfo := RenderInfo.PageRenderInfos[ARealPageIndex];
    if ReportLink.IsApplyBackgroundToEntirePage then
      ARect := cxRect(cxNullPoint, APageRenderInfo.PageSize)
    else
    begin
      ARect := APageRenderInfo.PageHeaderBounds;
      ARect.Bottom := APageRenderInfo.PageFooterBounds.Top;
    end;

    ABackground := ReportLink.RealPrinterPage.Background;
    if ABackground.IsNeedDrawBackground then
      Canvas.FillRect(ARect, clWhite);
    ABackground.PaintEx(Canvas, ARect, PixelsNumerator * 100, PixelsDenominator * RenderInfo.ScaleFactor);
  end;
end;

procedure TdxPSReportRenderer.RenderPageFooter(ARealPageIndex: Integer);
var
  R: TRect;
begin
  if ReportLink.ShowPageFooter and (rlcHeaderFooter in ReportLink.Capabilities) then
  begin
    R := RenderInfo.PageRenderInfos[ARealPageIndex].PageFooterBounds;
    if not IsRectEmpty(R) and (IsPrinting or Canvas.IsRectVisible(R)) then
      RenderPageHeaderOrFooter(ReportLink.RealPrinterPage.PageFooter, ARealPageIndex, R);
  end;
end;

procedure TdxPSReportRenderer.RenderPageHeader(ARealPageIndex: Integer);
var
  R: TRect;
begin
  if ReportLink.ShowPageHeader and (rlcHeaderFooter in ReportLink.Capabilities) then
  begin
    R := RenderInfo.PageRenderInfos[ARealPageIndex].PageHeaderBounds;
    if not IsRectEmpty(R) and (IsPrinting or Canvas.IsRectVisible(R)) then
      RenderPageHeaderOrFooter(ReportLink.RealPrinterPage.PageHeader, ARealPageIndex, R);
  end;
end;

procedure TdxPSReportRenderer.RenderPageHeaderOrFooter(
  AHeaderOrFooter: TCustomdxPageObject; APageIndex: Integer; ARect: TRect);
const
  TitleParts: array[Boolean] of TdxPageTitleParts = ([], [tpLeft..tpRight]);
var
  ACustomDrawCanvas: TCanvas;
  ADefaultDrawBackground: Boolean;
  ADefaultDrawText: Boolean;
  ASavedFontHeight: Integer;
  ASavedFontPixelsPerInch: Integer;
begin
  ASavedFontHeight := AHeaderOrFooter.Font.Height;
  ASavedFontPixelsPerInch := AHeaderOrFooter.Font.PixelsPerInch;
  try
    AHeaderOrFooter.Font.PixelsPerInch := dxSystemScaleFactor.TargetDPI;
    PrepareFont(AHeaderOrFooter.Font, TCustomdxPageObjectAccess(AHeaderOrFooter).AdjustOnScale);
    Canvas.Font := AHeaderOrFooter.Font;
    if ReportLink.IsHeaderOrFooterCustomDrawn(AHeaderOrFooter) then
    begin
      ACustomDrawCanvas := Canvas.BeginCustomDraw(ARect, AHeaderOrFooter.Font, clDefault);
      try
        ADefaultDrawText := True;
        ADefaultDrawBackground := True;
        ReportLink.DoCustomDrawPageHeaderOrFooter(AHeaderOrFooter,
          ACustomDrawCanvas, APageIndex, ARect, ADefaultDrawText, ADefaultDrawBackground);
        if ADefaultDrawText or ADefaultDrawBackground then
          RenderPageHeaderOrFooterContent(AHeaderOrFooter, APageIndex,
            ARect, TitleParts[ADefaultDrawText], ADefaultDrawBackground);
      finally
        Canvas.EndCustomDraw(ACustomDrawCanvas);
      end;
    end
    else
      RenderPageHeaderOrFooterContent(AHeaderOrFooter, APageIndex, ARect, [tpLeft..tpRight], True);
  finally
    AHeaderOrFooter.Font.PixelsPerInch := ASavedFontPixelsPerInch;
    AHeaderOrFooter.Font.Height := ASavedFontHeight;
  end;
end;

procedure TdxPSReportRenderer.RenderPageHeaderOrFooterContentPart(
  ATitlePart: TdxPageTitlePart; AStrings: TStrings; ATextAlignY: TcxTextAlignY;
  ALineHeight, ADestWidth, ADestHeight: Integer; const ARect: TRect);
const
  TextFormatFlags = CXTO_SINGLELINE or CXTO_NOCLIP or CXTO_EDITCONTROL or
    CXTO_LEFT or CXTO_CENTER_VERTICALLY;
var
  FullHeight: Integer;
  R: TRect;
  I: Integer;
  S: string;
  TextSize: TSize;
begin
  if ATextAlignY = taTop then
    FullHeight := ARect.Top
  else
  begin
    FullHeight := AStrings.Count * ALineHeight;
    if ATextAlignY = taCenterY then
      FullHeight := ARect.Top + (ADestHeight - FullHeight) div 2
    else {taBottom}
      FullHeight := ARect.Top + ADestHeight - FullHeight;
  end;
  R := Rect(ARect.Left, 0, ARect.Right, ALineHeight);
  OffsetRect(R, 0, Max(FullHeight, ARect.Top));

  for I := 0 to AStrings.Count - 1 do
  begin
    S := AStrings[I];
    if S <> '' then
    begin
      TextSize := Canvas.TextSize(S);
      if ADestWidth > TextSize.cX then
      begin
        case ATitlePart of
          tpLeft:
            R.Right := R.Left + TextSize.cX;
          tpRight:
            R.Left := R.Right - TextSize.cX;
          tpCenter:
            begin
              R.Left := ARect.Left + (ADestWidth - TextSize.cX) div 2;
              R.Right := R.Left + TextSize.cX;
            end;
        end;
      end;
      if Canvas.IsRectVisible(R) then
      begin
        DrawTextEx(Canvas, R, 0, 0, 0, S, nil, TextFormatFlags);
        Canvas.ExcludeClipRect(R);
      end;
      R.Right := ARect.Right;
    end;
    OffsetRect(R, 0, ALineHeight);
  end;
end;

procedure TdxPSReportRenderer.RenderPageHeaderOrFooterContent(
  HF: TCustomdxPageObject; APageIndex: Integer; ARect: TRect; ATitleParts: TdxPageTitleParts; ADrawBackground: Boolean);
var
  ABuilder: TdxHFReportCellBuilder;
  APageObjectCells: TdxReportCells;
begin
  if ADrawBackground then
    HF.Background.PaintEx(Canvas, ARect, PixelsNumerator * 100, PixelsDenominator * RenderInfo.ScaleFactor);
  if ATitleParts <> [] then
  begin
    ABuilder := TdxHFReportCellBuilder.Create;
    APageObjectCells := TdxReportCells.Create(ReportLink);
    try
      Canvas.SaveClipRgn;
      try
        Canvas.IntersectClipRgn(ARect);
        ABuilder.Build(Canvas, APageObjectCells, ARect, ATitleParts, HF,
          ReportLink.RealPrinterPage.ReverseTitlesOnEvenPages and Odd(APageIndex));
        APageObjectCells.Cells.DrawContent(Canvas, ARect, ARect, RenderStage);
      finally
        Canvas.RestoreClipRgn;
      end;
    finally
      APageObjectCells.Free;
      ABuilder.Free;
    end;
  end;
end;

procedure TdxPSReportRenderer.RenderCell(ACell: TdxReportCell; const OriginRect: TRect);
var
  AOldWindowOrg: TPoint;
  R: TRect;
begin
  if ACell.Visible then
  begin
    R := ACell.BoundsRect;
    { Transform R into the coordinate space of page }
    OffsetRect(R, -OriginRect.Left, -OriginRect.Top);
    AOldWindowOrg := Canvas.OffsetWindowOrg(cxPointInvert(R.TopLeft));
    try
      OffsetRect(R, -R.Left, -R.Top);
      ACell.DrawContent(Canvas, R, OriginRect, RenderStage);
    finally
      Canvas.WindowOrg := AOldWindowOrg;
    end;
  end;
end;

procedure TdxPSReportRenderer.RenderEntirePage(ARealPageIndex: Integer);
var
  ACustomDrawCanvas: TCanvas;
begin
  if ReportLink.IsEntirePageCustomDrawn then
  begin
    ACustomDrawCanvas := Canvas.BeginCustomDraw(ViewPortRect, nil, clDefault);
    try
      ReportLink.DoCustomDrawEntirePage(ACustomDrawCanvas, ViewPortRect, ARealPageIndex);
    finally
      Canvas.EndCustomDraw(ACustomDrawCanvas);
    end;
  end;
end;

procedure TdxPSReportRenderer.RenderPageContent;
var
  ADetailsOffset: Integer;
  APrevWindowOrg: TPoint;
begin
  APrevWindowOrg := Canvas.OffsetWindowOrg(cxPointInvert(cxPointOffset(PageRenderInfo.DataOffset, LineThickness, LineThickness)));
  try
    Canvas.SaveState;
    try
      Canvas.IntersectClipRgn(GetPageContentClipRect);
      RenderPageContentDetails(PageRenderInfo.DetailBounds.TopLeft, ADetailsOffset);
      RenderPageContentFooter(ADetailsOffset);
    finally
      Canvas.RestoreState;
    end;
  finally
    Canvas.WindowOrg := APrevWindowOrg;
  end;
end;

procedure TdxPSReportRenderer.RenderPageContentDetails(const ADetailsPosition: TPoint; out AOffset: Integer);

  function GetPageDetailsClipRect: TRect;
  begin
    Result := GetPageContentClipRect;
    if PageRenderInfo.HasRowHeader then
      Result.Left := PageRenderInfo.RowHeaderBounds.Right - LineThickness;
    if PageRenderInfo.HasHeader then
      Result.Top := PageRenderInfo.HeaderBounds.Bottom - LineThickness;
  end;

  function GetPageDetailsOffset: TPoint;
  begin
    Result := cxNullPoint;
    if PageRenderInfo.HasRowHeader then
      Result.X := -cxRectWidth(PageRenderInfo.RowHeaderBounds);
    if PageRenderInfo.HasHeader then
      Result.Y := -cxRectHeight(PageRenderInfo.HeaderBounds);
  end;

var
  AEndIndex: Integer;
  ASaveOrg: TPoint;
  I: Integer;
begin
  Canvas.SaveClipRgn;
  try
    AOffset := 0;
    ASaveOrg := Canvas.WindowOrg;
    try
      RenderPageContentHeaders(ADetailsPosition);

      Canvas.IntersectClipRgn(GetPageDetailsClipRect);
      Canvas.OffsetWindowOrg(GetPageDetailsOffset);

      if PageRenderInfo.HasDetails then
      begin
        AEndIndex := 0;
        for I := 0 to PageRenderInfo.IndexPairCount - 1 do
        begin
          AEndIndex := PageRenderInfo.IndexPairs[I].EndIndex;
          RenderPageContentPart(PageRenderInfo.ReportCells.Cells,
            PageRenderInfo.IndexPairs[I].StartIndex, AEndIndex, PageRenderInfo.DetailBounds);
        end;

        for I := 0 to PageRenderInfo.OverlayCount - 1 do
          RenderPageOverlay(I, PageRenderInfo.Overlays[I], PageRenderInfo.DetailBounds);

        if not PageRenderInfo.IsBottomPage then
          AOffset := PageRenderInfo.DetailBounds.Bottom
        else
          if PageRenderInfo.ReportCells.Cells.CellCount > AEndIndex then
            AOffset := ReportCells.Cells[AEndIndex].BoundsRect.Bottom;

        Dec(AOffset, PageRenderInfo.DetailBounds.Top);
        Canvas.OffsetWindowOrg(Point(0, -AOffset));
      end;
    finally
      Canvas.WindowOrg := ASaveOrg;
    end;
  finally
    Canvas.RestoreClipRgn;
  end;
end;

procedure TdxPSReportRenderer.RenderPageContentFooter(ADetailsOffset: Integer);
begin
  Canvas.OffsetWindowOrg(Point(0, -cxRectHeight(PageRenderInfo.HeaderBounds) - ADetailsOffset));
  if PageRenderInfo.HasFooter then
  begin
    RenderPageContentPart(ReportCells.FooterCells, 0, -1,
      cxRectOffset(PageRenderInfo.FooterBounds, PageRenderInfo.DetailBounds.Left, 0));
  end;
end;

procedure TdxPSReportRenderer.RenderPageContentHeader(
  ACell: TdxReportCell; AAnchors: TAnchors; const R: TRect; const AOffset: TPoint);

  function AdjustFixedPageHeaderClipRect(const R: TRect; AAnchors: TAnchors): TRect;
  begin
    Result := R;
    // #AI: B206802 - Adjust ClipRect for correct merging headers grid lines
    if akLeft in AAnchors then
      Dec(Result.Left, LineThickness)
    else
      Inc(Result.Left, LineThickness);

    if akRight in AAnchors then
      Inc(Result.Right, LineThickness)
    else
      Dec(Result.Right, LineThickness);

    if akBottom in AAnchors then
      Inc(Result.Bottom, LineThickness)
    else
      Dec(Result.Bottom, LineThickness);

    if akTop in AAnchors then
      Dec(Result.Top, LineThickness)
    else
      Inc(Result.Top, LineThickness);
  end;

begin
  RenderPageContentPart(ACell, 0, -1, cxRectOffset(R, AOffset));
  Canvas.ExcludeClipRect(AdjustFixedPageHeaderClipRect(R, AAnchors));
end;

procedure TdxPSReportRenderer.RenderPageContentHeaders(const ADetailsPosition: TPoint);
begin
  if PageRenderInfo.HasHeaderCorner then
    RenderPageContentHeader(ReportCells.HeaderCornerCells, [akLeft, akTop], PageRenderInfo.HeaderCornerBounds, cxNullPoint);
  if PageRenderInfo.HasHeader then
    RenderPageContentHeader(ReportCells.HeaderCells, [akLeft, akTop, akRight], PageRenderInfo.HeaderBounds, cxPoint(ADetailsPosition.X, 0));
  if PageRenderInfo.HasRowHeader then
    RenderPageContentHeader(ReportCells.RowHeaderCells, [akLeft, akTop, akBottom], PageRenderInfo.RowHeaderBounds, cxPoint(0, ADetailsPosition.Y));
end;

procedure TdxPSReportRenderer.RenderPageContentPart(
  ACell: TdxReportCell; StartIndex, EndIndex: Integer; const OriginRect: TRect);
var
  I: Integer;
begin
  if EndIndex = -1 then
    EndIndex := ACell.CellCount - 1;
  for I := StartIndex to EndIndex do
    RenderCell(ACell[I], OriginRect);
end;

procedure TdxPSReportRenderer.RenderPageOverlay(
  AOverlayIndex: Integer; AOverlay: TdxPageOverlayIndexes; const OriginRect: TRect);
var
  AOverlayCell: TdxReportCell;
  I: Integer;
begin
  AOverlayCell := ReportCells.Overlays[AOverlayIndex];
  for I := 0 to AOverlay.Count - 1 do
    RenderCell(AOverlayCell.Cells[AOverlay[I]], OriginRect);
end;

function TdxPSReportRenderer.GetPageContentClipRect: TRect;
begin
  Result := cxRectSetNullOrigin(PageRenderInfo.ContentBounds);
  Result := cxRectInflate(Result, LineThickness);
end;

function TdxPSReportRenderer.GetUnitsPerInch: Integer;
begin
  Result := RenderInfo.UnitsPerInch;
end;

function TdxPSReportRenderer.GetUnitsPerInchEx(AAdjustOnReportScale: Boolean): Integer;
begin
  if AAdjustOnReportScale then
    Result := RenderInfo.UnitsPerInch
  else
    Result := MulDiv(RenderInfo.UnitsPerInch, 100, ReportLink.RealScaleFactor);
end;

function TdxPSReportRenderer.CreateBorderPainter(AClass: TdxPSCellBorderPainterClass): TdxPSCellBorderPainter;
begin
  Result := AClass.Create(Self);
  FBorderPainters.Add(Result);
end;

function TdxPSReportRenderer.FindBorderPainter(AClass: TdxPSCellBorderPainterClass): TdxPSCellBorderPainter;
var
  I: Integer;
begin
  for I := 0 to BorderPainterCount - 1 do
  begin
    Result := BorderPainters[I];
    if Result.ClassType = AClass then Exit;
  end;
  Result := nil;
end;

procedure TdxPSReportRenderer.FreeAndNilBorderPainters;
var
  I: Integer;
begin
  for I := 0 to BorderPainterCount - 1 do
    BorderPainters[I].Free;
  FreeAndNil(FBorderPainters);
end;

function TdxPSReportRenderer.CreateReportGroupLookAndFeelPainter(AClass: TdxPSReportGroupLookAndFeelPainterClass): TdxPSReportGroupLookAndFeelPainter;
begin
  Result := AClass.Create(Self);
  FGroupLookAndFeelPainters.Add(Result);
end;

function TdxPSReportRenderer.FindReportGroupLookAndFeelPainter(AClass: TdxPSReportGroupLookAndFeelPainterClass): TdxPSReportGroupLookAndFeelPainter;
var
  I: Integer;
begin
  for I := 0 to GroupLookAndFeelPainterCount - 1 do
  begin
    Result := GroupLookAndFeelPainters[I];
    if Result.ClassType = AClass then Exit;
  end;
  Result := nil;
end;

procedure TdxPSReportRenderer.FreeAndNilReportGroupLookAndFeelPainters;
var
  I: Integer;
begin
  for I := 0 to GroupLookAndFeelPainterCount - 1 do
    GroupLookAndFeelPainters[I].Free;
  FreeAndNil(FGroupLookAndFeelPainters);
end;

function TdxPSReportRenderer.GetBorderPainterItem(Index: Integer): TdxPSCellBorderPainter;
begin
  Result := TdxPSCellBorderPainter(FBorderPainters[Index]);
end;

function TdxPSReportRenderer.GetBorderPainterCount: Integer;
begin
  Result := FBorderPainters.Count;
end;

function TdxPSReportRenderer.GetGroupLookAndFeelPainter(Index: Integer): TdxPSReportGroupLookAndFeelPainter;
begin
  Result := TdxPSReportGroupLookAndFeelPainter(FGroupLookAndFeelPainters[Index]);
end;

function TdxPSReportRenderer.GetGroupLookAndFeelPainterCount: Integer;
begin
  Result := FGroupLookAndFeelPainters.Count;
end;

function TdxPSReportRenderer.GetHalfLineThickness: Integer;
begin
  Result := LineThickness div 2;
end;

function TdxPSReportRenderer.GetIsCompositionPagePart: Boolean;
begin
  Result := PageRenderInfo.IsCompositionPagePart;
end;

function TdxPSReportRenderer.GetIsPrinting: Boolean;
begin
  Result := Canvas.IsPrinterCanvas;
end;

function TdxPSReportRenderer.GetPageRenderInfo: TdxPSPageRenderInfo;
begin
  Result := RenderInfo.PageRenderInfos[RenderingPageIndex];
end;

function TdxPSReportRenderer.GetRenderInfo: TdxPSReportRenderInfo;
begin
  Result := FReportLink.RenderInfo;
end;

function TdxPSReportRenderer.GetReportCells: TdxReportCells;
begin
  Result := ReportLink.FReportCells;
end;

procedure TdxPSReportRenderer.PrepareFont(AFont: TFont; AAdjustOnScale: Boolean);
begin
  dxPSScaleFontEx(AFont, AFont.Height, GetUnitsPerInchEx(AAdjustOnScale), ReportLink.PixelsPerInch);
end;

procedure TdxPSReportRenderer.PrepareFonts;
begin
  ReportLink.PrepareFonts(GetUnitsPerInchEx(ReportLink.ScaleFonts));
  dxPSScaleFont(MarlettFont10, 10, UnitsPerInch);
  dxSetFontAsNonAntialiased(MarlettFont10);
  dxPSScaleFont(MarlettFont8, 8, UnitsPerInch);
  dxSetFontAsNonAntialiased(MarlettFont8);
  dxPSScaleFont(SymbolFont, 7, UnitsPerInch);
  dxSetFontAsNonAntialiased(SymbolFont);
end;

procedure TdxPSReportRenderer.PrepareGDIObjects;
begin
  FBorderColor := ColorToRGB(RenderInfo.GridLinesColor);
end;

procedure TdxPSReportRenderer.PrepareLogicalCoordinates;
const
  MappingMode: array[Boolean] of TdxPSReportRenderCanvasMappingMode = (rrmmDefault, rrmmIsotropic);
begin
  Canvas.SetCanvasExts(PageRenderInfo.PageSize, MappingMode[FUseIsotropicMode],
    RenderInfo.WindowScalePair.Numerator, RenderInfo.WindowScalePair.Denominator, ViewPortRect);
end;

procedure TdxPSReportRenderer.PrepareLogicalUnits;
var
  ARect: TRect;
  AUnitsPerPt: Integer;
begin
  RenderInfo.PreparePixelsNumeratorAndDenominator;
  Canvas.PrepareLogicalUnits;
  ARect := Rect(0, 0, 1, 1);
  Canvas.DeviceToLogicalCoordinates(ARect);
  UnitsPerPixel := Max(0, ARect.Right - ARect.Left);
  AUnitsPerPt := MulDiv(UnitsPerPixel, PixelsPerInch, PtPerInch);
  LineThickness := Canvas.CalculateLineThickness(UnitsPerPixel, AUnitsPerPt);
  if not IsPrinting and (ZoomFactor > 100) and ReportLink.IsScaleGridLines then
    LineThickness := LineThickness * (ZoomFactor div 100);
end;

function TdxPSReportRenderer.PreparedPageIndex(APageIndex: Integer): Integer;
var
  RowIndex, ColIndex: Integer;
begin
  if RenderInfo.PrinterPage.PageOrder = poDownThenOver then
  begin
    RowIndex := APageIndex div RenderInfo.PageColCount;
    ColIndex := APageIndex mod RenderInfo.PageColCount;
    Result := RowIndex + ColIndex * RenderInfo.PageRowCount;
  end
  else
    Result := APageIndex;
end;

procedure TdxPSReportRenderer.PrepareRenderPage;
begin
  RenderInfo.Lock;
  SaveMapMode;
  PrepareLogicalCoordinates;
  PrepareLogicalUnits;
  PrepareFonts;
  PrepareGDIObjects;
  FIsRendering := True;
end;

procedure TdxPSReportRenderer.RestoreMapMode;
begin
  Canvas.RestoreState;
end;

procedure TdxPSReportRenderer.SaveMapMode;
begin
  Canvas.SaveState;
end;

procedure TdxPSReportRenderer.UnprepareLogicalUnits;
begin
  LineThickness := 1;
end;

procedure TdxPSReportRenderer.UnprepareGDIObjects;
begin
end;

procedure TdxPSReportRenderer.UnprepareRenderPage;
begin
  FIsRendering := False;
  Canvas.UnprepareLogicalUnits;
  UnprepareLogicalUnits;
  UnprepareGDIObjects;
  RestoreMapMode;
  RenderInfo.Unlock;
end;

{ TdxReportItem }

constructor TdxReportItem.Create(AParent: TdxReportCell);
begin
  inherited Create;
  Parent := AParent;
end;

destructor TdxReportItem.Destroy;
begin
  SetParent(nil);
  SetCreator(nil);
  FreeCreatorLink;
  inherited Destroy;
end;

procedure TdxReportItem.Assign(Source: TPersistent);
begin
  if Source is TdxReportItem then
  begin
    Data := TdxReportItem(Source).Data;
    SetCreator(TdxReportItem(Source).Creator);
  end
  else
    inherited Assign(Source);
end;

class function TdxReportItem.ReportItemClass: TdxReportItemClass;
begin
  Result := TdxReportItemClass(GetTypeData(ClassInfo)^.ClassType);
end;

class procedure TdxReportItem.Register;
begin
  if GetClass(ClassName) = nil then
    RegisterClass(Self);
end;

class procedure TdxReportItem.Unregister;
begin
  UnregisterClass(Self);
end;

function TdxReportItem.Clone(AParent: TdxReportCell): TdxReportItem;
begin
  Result := ReportItemClass.Create(AParent);
  try
    Result.Assign(Self);
  except
    Result.Free;
    raise;
  end;
end;

function TdxReportItem.getNextSibling: TdxReportItem;
var
  Index: Integer;
begin
  Result := nil;
  if not HasParent then Exit;
  Index := Parent.IndexOf(Self);

  if IsCell then
  begin
    if Index < Parent.CellCount - 1 then
      Result := TdxReportItem(Parent.FCellList[Index + 1]);
  end
  else
    if Index < Parent.DataItemCount - 1 then
      Result := TdxReportItem(Parent.FDataList[Index + 1]);
end;

function TdxReportItem.getPrevSibling: TdxReportItem;
var
  Index: Integer;
begin
  Result := nil;
  if not HasParent then Exit;
  Index := Parent.IndexOf(Self);
  if Index < 1 then Exit;

  if IsCell then
    Result := TdxReportItem(Parent.FCellList[Index - 1])
  else
    Result := TdxReportItem(Parent.FDataList[Index - 1]);
end;

function TdxReportItem.HasParent: Boolean;
begin
  Result := Parent <> nil;
end;

function TdxReportItem.IsFirstItem: Boolean;
begin
  Result := getPrevSibling = nil;
end;

function TdxReportItem.IsLastItem: Boolean;
begin
  Result := getNextSibling = nil;
end;

function TdxReportItem.AsCell: TdxReportCell;
begin
  if IsCell then
    Result := TdxReportCell(Self)
  else
    Result := nil;
end;

function TdxReportItem.GetTopLevelParent: TdxReportCell;
begin
  if ReportCells <> nil then
    Result := ReportCells.GetCellTopLevelParent(Self)
  else
    Result := nil;
end;

class function TdxReportItem.IsCell: Boolean;
begin
  Result := False;
end;

procedure TdxReportItem.ReadData(AReader: TdxPSDataReader);
begin
  Data := AReader.ReadInt64;
end;

procedure TdxReportItem.WriteData(AWriter: TdxPSDataWriter);
begin
  AWriter.WriteInteger(Data);
end;

class function TdxReportItem.Serializable: Boolean;
begin
  Result := True;
end;

procedure TdxReportItem.FreeCreatorLink;
begin
  cxRemoveObjectLink(FCreatorLink);
  FCreatorLink := nil;
end;

function TdxReportItem.GetCreator: TBasedxReportLink;
begin
  if (FCreatorLink <> nil) and (FCreatorLink.Ref <> nil) then
    Result := TBasedxReportLink(FCreatorLink.Ref)
  else
    Result := ReportLink;
end;

function TdxReportItem.GetIndex: Integer;
begin
  if Parent <> nil then
    Result := Parent.IndexOf(Self)
  else
    Result := -1;
end;

function TdxReportItem.GetReportCells: TdxReportCells;
begin
  if Parent <> nil then
    Result := Parent.ReportCells
  else
    Result := nil;
end;

function TdxReportItem.GetReportLink: TBasedxReportLink;
begin
  if ReportCells <> nil then
    Result := ReportCells.ReportLink
  else
    Result := nil;
end;

procedure TdxReportItem.SetCreator(AValue: TBasedxReportLink);
begin
  if AValue <> Creator then
  begin
    if AValue = nil then
      FreeCreatorLink
    else
      if FCreatorLink <> nil then
        FCreatorLink.Ref := AValue
      else
        FCreatorLink := cxAddObjectLink(AValue);
  end;
end;

procedure TdxReportItem.SetIndex(Value: Integer);
var
  CurIndex: Integer;
begin
  if Parent = nil then Exit;

  Value := Max(Value, 0);
  if IsCell then
    Value := Min(Value, Parent.CellCount - 1)
  else
    Value := Min(Value, Parent.DataItemCount - 1);

  CurIndex := GetIndex;
  if CurIndex <> Value then
    Parent.MoveItem(Self, CurIndex, Value);
end;

procedure TdxReportItem.SetParent(Value: TdxReportCell);
begin
  if Parent <> Value then
  begin
    if Parent <> nil then
      Parent.RemoveItem(Self);
    if Value <> nil then
      Value.InsertItem(Self);
  end;
end;

{ TdxReportVisualItem }

constructor TdxReportVisualItem.Create(AParent: TdxReportCell);
begin
  inherited;
  FBackgroundBitmapIndex := -1;
  if HasBorderColoration then
    FBorderClass := TdxPSColorBorder
  else
    FBorderClass := TdxPSCellUltraFlatBorder;
  FBorderColor := clDefault;
  CellSides := dxDefaultCellSides;     // csAll
  FColor := dxDefaultContentColor;     // clWhite
  ShowShadow := False;
  Transparent := dxDefaultTransparent; // True
  Visible := True;
end;

procedure TdxReportVisualItem.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TdxReportVisualItem then
    with TdxReportVisualItem(Source) do
    begin
      Self.BackgroundBitmapIndex := BackgroundBitmapIndex;
      Self.BorderClass := BorderClass;
      Self.BorderColor := BorderColor;
      Self.BoundsRect := BoundsRect;
      Self.Color := Color;
      Self.ContentBkColor := ContentBkColor;
      Self.ContentPattern := ContentPattern;
      Self.FontIndex := FontIndex;
      Self.Format := Format;
      Self.ShadowColor := ShadowColor;
      Self.ShadowDepth := ShadowDepth;
      Self.FCellSideColors := FCellSideColors;
    end;
end;

procedure TdxReportVisualItem.AdjustContent(ACanvas: TdxPSReportRenderCustomCanvas);
begin
end;

procedure TdxReportVisualItem.DrawBackground(ACanvas: TdxPSReportRenderCustomCanvas);
begin
  DrawBackground(ACanvas, GetBackgroundBounds(ACanvas));
end;

procedure TdxReportVisualItem.DrawBackground(
  ACanvas: TdxPSReportRenderCustomCanvas; const R: TRect);
begin
  if not IsRectEmpty(R) then
  begin
    if IsBackgroundBitmapDrawn then
      DrawBackgroundBitmap(ACanvas, R)
    else
      if IsBackgroundDrawn then
        DrawBackgroundRect(ACanvas, R);
  end;
end;

procedure TdxReportVisualItem.DrawBackgroundBitmap(
  ACanvas: TdxPSReportRenderCustomCanvas; const R: TRect);
var
  R1: TRect;
  I, J: Integer;
begin
  ACanvas.SaveClipRgn;
  try
    ACanvas.IntersectClipRgn(R);
    for I := BackgroundBitmapTileStartIndexX to BackgroundBitmapTileStopIndexX do
      for J := BackgroundBitmapTileStartIndexY to BackgroundBitmapTileStopIndexY do
      begin
        R1 := BackgroundBitmapTileBounds[I, J];
        if ACanvas.IsRectVisible(R1) then
          DrawBackgroundBitmapTile(ACanvas, R1);
      end;
  finally
    ACanvas.RestoreClipRgn;
  end;
end;

procedure TdxReportVisualItem.DrawBackgroundBitmapTile(
  ACanvas: TdxPSReportRenderCustomCanvas; const Rect: TRect);
begin
  Renderer.DrawGraphicEx(ACanvas, Rect, GetBackgroundBounds(ACanvas), nil, -1,
    BackgroundBitmap, False, False, clNone, clNone, nil, cibAlways);
end;

procedure TdxReportVisualItem.DrawBackgroundRect(
  ACanvas: TdxPSReportRenderCustomCanvas; const R: TRect);
begin
  Renderer.FillRectEx(ACanvas, R, Color, ContentBkColor, ContentPattern);
end;

procedure TdxReportVisualItem.DrawBorders(ACanvas: TdxPSReportRenderCustomCanvas);
var
  ABorderBounds: TRect;
begin
  ABorderBounds := GetBorderOuterBounds(ACanvas);
  BorderPainter.DrawBorders(ACanvas, ABorderBounds);
  if IsShadowDrawn then
    BorderPainter.DrawShadow(ACanvas, ABorderBounds, ShadowDepth, ShadowColor, ParentColor);
end;

function TdxReportVisualItem.GetBorderOuterBounds(
  ACanvas: TdxPSReportRenderCustomCanvas): TRect;
begin
  Result := GetBorderOuterBoundsRelativeTo(ACanvas, GetOuterBounds(ACanvas));
end;

function TdxReportVisualItem.GetInnerBounds(ACanvas: TdxPSReportRenderCustomCanvas): TRect;
begin
  Result := GetInnerBoundsRelativeTo(ACanvas, BoundsRect);
end;

function TdxReportVisualItem.GetOuterBounds(ACanvas: TdxPSReportRenderCustomCanvas): TRect;
begin
  Result := GetOuterBoundsRelativeTo(ACanvas, BoundsRect);
end;

procedure TdxReportVisualItem.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  if (ALeft <> Left) or (ATop <> Top) or (AWidth <> Width) or (AHeight <> Height) then
  begin
    FBoundsRect.Left := ALeft;
    FBoundsRect.Top := ATop;
    FBoundsRect.Right := ALeft + AWidth;
    FBoundsRect.Bottom := ATop + AHeight;
    BoundsChanged;
  end;
end;

function TdxReportVisualItem.CalculateLineCount(
  ACanvas: TdxPSReportRenderCustomCanvas): Integer;
begin
  Result := 1;
end;

function TdxReportVisualItem.MeasureBordersHeight(
  ACanvas: TdxPSReportRenderCustomCanvas): Integer;
begin
  Result := 0;
  if csTop in CellSides then
    Inc(Result, GetBorderEdgeThickness(csTop) - 1);
  if csBottom in CellSides then
    Inc(Result, GetBorderEdgeThickness(csBottom) - 1);
  if ShowShadow then
    Inc(Result, ShadowDepth);
end;

function TdxReportVisualItem.MeasureBordersWidth(
  ACanvas: TdxPSReportRenderCustomCanvas): Integer;
begin
  Result := 0;
  if csLeft in CellSides then
    Inc(Result, GetBorderEdgeThickness(csLeft) - 1);
  if csRight in CellSides then
    Inc(Result, GetBorderEdgeThickness(csRight) - 1);
  if ShowShadow then
    Inc(Result, ShadowDepth);
end;

function TdxReportVisualItem.MeasureContentHeight(ACanvas: TdxPSReportRenderCustomCanvas): Integer;
begin
  Result := 0;
end;

function TdxReportVisualItem.MeasureContentWidth(ACanvas: TdxPSReportRenderCustomCanvas): Integer;
begin
  Result := 0;
end;

function TdxReportVisualItem.MeasureFontHeight(ACanvas: TdxPSReportRenderCustomCanvas): Integer;
begin
  Result := Height;
end;

function TdxReportVisualItem.MeasureHeight(ACanvas: TdxPSReportRenderCustomCanvas): Integer;
begin
  Result := MeasureBordersHeight(ACanvas) + MeasureContentHeight(ACanvas);
end;

function TdxReportVisualItem.MeasureWidth(ACanvas: TdxPSReportRenderCustomCanvas): Integer;
begin
  Result := MeasureBordersWidth(ACanvas) + MeasureContentWidth(ACanvas);
end;

procedure TdxReportVisualItem.Offset(AOffsetX, AOffsetY: Integer);
begin
  Left := Left + AOffsetX;
  Top := Top + AOffsetY;
end;

class function TdxReportVisualItem.MapBorderClass(AEdgeMode: TdxCellEdgeMode;
  AEdgeStyle: TdxCellEdgeStyle; ASoft: Boolean): TdxPSCellBorderClass;
begin
  if AEdgeMode = cem3DEffects then
    Result := dxCellBorderClassMap[AEdgeStyle, ASoft]
  else
    Result := TdxPSCellUltraFlatBorder;
end;

procedure TdxReportVisualItem.BoundsChanged;
begin
end;

procedure TdxReportVisualItem.ConvertCoords(APixelsNumerator, APixelsDenominator: Integer);
begin
  FBoundsRect := cxRectScale(FBoundsRect, APixelsNumerator, APixelsDenominator);
end;

procedure TdxReportVisualItem.DoExcludeFromClipRgn(
  ACanvas: TdxPSReportRenderCustomCanvas; const R: TRect; var AResult: Integer);
var
  R2: TRect;
begin
  if Visible and IntersectRect(R2, GetAbsoluteRect, R) then
  begin
    if ExcludeFromClipRgn and not Transparent then
      AResult := ACanvas.ExcludeClipRect(GetOuterBounds(ACanvas));
  end;
end;

function TdxReportVisualItem.GetBackgroundBitmapTileBounds(Col, Row: Integer): TRect;
begin
  Result := Bounds(Left + BackgroundBitmapWidth * Col,
    Top + BackgroundBitmapHeight * Row, BackgroundBitmapWidth, BackgroundBitmapHeight);
  with BackgroundBitmapTileOrigin do
    OffsetRect(Result, X, Y);
end;

function TdxReportVisualItem.GetBackgroundBounds(ACanvas: TdxPSReportRenderCustomCanvas): TRect;
begin
  Result := GetInnerBounds(ACanvas);
end;

function TdxReportVisualItem.GetBorderClass: TdxPSCellBorderClass;
begin
  Result := FBorderClass;
end;

function TdxReportVisualItem.GetBorderEdgeClass(ASide: TdxCellSide): TdxPSCellBorderClass;
begin
  Result := BorderClass;
end;

function TdxReportVisualItem.GetBorderEdgeBounds(ASide: TdxCellSide; const AOuterRect: TRect): TRect;
begin
  Result := AOuterRect;
  with Result do
    case ASide of
      csLeft:
        Right := Left + LineThickness * BorderEdgeThicknesses[csLeft];
      csTop:
        Bottom := Top + LineThickness * BorderEdgeThicknesses[csTop];
      csRight:
        Left := Right - LineThickness * BorderEdgeThicknesses[csRight];
      csBottom:
        Top := Bottom - LineThickness * BorderEdgeThicknesses[csBottom];
    end;
end;

function TdxReportVisualItem.GetBorderEdgeSalient(ASide: TdxCellSide; ASalient: TdxPSCellBorderSalientType): Integer;
begin
  Result := BorderEdgeClasses[ASide].GetBorderEdgeSalient(ASide, ASalient);
end;

function TdxReportVisualItem.GetBorderEdgeThickness(ASide: TdxCellSide): Integer;
begin
  Result := BorderEdgeClasses[ASide].Thickness;
end;

function TdxReportVisualItem.GetBorderBounds(ACanvas: TdxPSReportRenderCustomCanvas): TRect;
begin
  Result := FBoundsRect;
end;

function TdxReportVisualItem.GetBorderOuterBoundsRelativeTo(
  ACanvas: TdxPSReportRenderCustomCanvas; const R: TRect): TRect;
begin
  Result := R;
  if ShowShadow then
  begin
    Dec(Result.Right, ShadowDepth);
    Dec(Result.Bottom, ShadowDepth);
  end;
end;

function TdxReportVisualItem.GetInnerBoundsRelativeTo(
  ACanvas: TdxPSReportRenderCustomCanvas; const R: TRect): TRect;
var
  LineThickness: Integer;
begin
  Result := GetOuterBoundsRelativeTo(ACanvas, R);
  LineThickness := Self.LineThickness;

  with Result do
  begin
    if csLeft in CellSides then
      Inc(Left, LineThickness * BorderEdgeThicknesses[csLeft]);
    if csTop in CellSides then
      Inc(Top, LineThickness * BorderEdgeThicknesses[csTop]);
    if csRight in CellSides then
      Dec(Right, LineThickness * BorderEdgeThicknesses[csRight]);
    if csBottom in CellSides then
      Dec(Bottom, LineThickness * BorderEdgeThicknesses[csBottom]);

    if ShowShadow then
    begin
      Dec(Right, ShadowDepth);
      Dec(Bottom, ShadowDepth);
    end;
  end;
  FixupRect(ACanvas, Result);
end;

function TdxReportVisualItem.GetOuterBoundsRelativeTo(
  ACanvas: TdxPSReportRenderCustomCanvas; const R: TRect): TRect;
begin
  Result := R;
  if csLeft in CellSides then
    Dec(Result.Left, LineThickness * BorderEdgeSalients[csLeft, bstOuter]);
  if csTop in CellSides then
    Dec(Result.Top, LineThickness * BorderEdgeSalients[csTop, bstOuter]);
  if csRight in CellSides then
    Inc(Result.Right, LineThickness * BorderEdgeSalients[csRight, bstOuter]);
  if csBottom in CellSides then
    Inc(Result.Bottom, LineThickness * BorderEdgeSalients[csBottom, bstOuter]);
  FixupRect(ACanvas, Result);
end;

function TdxReportVisualItem.GetBorderPainter: TdxPSCellBorderPainter;
begin
  Result := Renderer.GetBorderPainter(BorderPainterClass);
  InitBorderPainter(Result);
end;

function TdxReportVisualItem.GetBorderPainterClass: TdxPSCellBorderPainterClass;
begin
  Result := BorderClass.GetPainterClass;
end;

function TdxReportVisualItem.GetBottom: Integer;
begin
  Result := BoundsRect.Bottom;
end;

procedure TdxReportVisualItem.InitBorderPainter(ABorderPainter: TdxPSCellBorderPainter);
begin
  ABorderPainter.FItem := Self;
end;

function TdxReportVisualItem.HasBorderColoration: Boolean;
begin
  Result := False; // todo MSN;
end;

function TdxReportVisualItem.GetFormatBit(ABit: DWORD): Boolean;
begin
  Result := Format and ABit = ABit;
end;

procedure TdxReportVisualItem.SetFormatBit(ABit: DWORD; Value: Boolean);
begin
  Format := Format and not ABit;
  if Value then
    Format := Format or ABit;
end;

function TdxReportVisualItem.GetContentBkColor: TColor;
begin
  Result := Color;
end;

function TdxReportVisualItem.GetContentPattern: TdxPSFillPatternClass;
begin
  Result := TdxPSSolidFillPattern;
end;

function TdxReportVisualItem.GetShadowColor: TColor;
begin
  if ReportCells <> nil then
    Result := ReportCells.ShadowColor
  else
    Result := dxDefaultShadowColor;
end;

function TdxReportVisualItem.GetShadowDepth: Integer;
begin
  if ReportCells <> nil then
    Result := ReportCells.ShadowDepth
  else
    Result := dxDefaultShadowDepth;
end;

procedure TdxReportVisualItem.SetContentBkColor(Value: TColor);
begin
end;

procedure TdxReportVisualItem.SetContentPattern(Value: TdxPSFillPatternClass);
begin
end;

procedure TdxReportVisualItem.SetFontIndex(Value: Integer);
begin
  FFontIndex := Max(Value, -1);
end;

procedure TdxReportVisualItem.SetShadowColor(Value: TColor);
begin
end;

procedure TdxReportVisualItem.SetShadowDepth(Value: Integer);
begin
end;

function TdxReportVisualItem.HasBackground: Boolean;
begin
  Result := IsBackgroundBitmapDrawn or IsBackgroundDrawn;
end;

function TdxReportVisualItem.IsShadowDrawn: Boolean;
begin
  Result := ShowShadow and (ShadowDepth <> 0);
end;

function TdxReportVisualItem.IsBackgroundBitmapDrawn: Boolean;
begin
  Result := (BackgroundBitmapIndex <> -1) and not BackgroundBitmap.Empty;
end;

function TdxReportVisualItem.IsBackgroundDrawn: Boolean;
begin
  Result := not Transparent;
end;

function TdxReportVisualItem.IsBordersDrawn: Boolean;
begin
  Result := (CellSides <> []) or ShowShadow;
end;

procedure TdxReportVisualItem.ReadData(AReader: TdxPSDataReader);
begin
  inherited ReadData(AReader);
  BackgroundBitmapIndex := AReader.ReadInteger;
  BoundsRect := AReader.ReadRect;
  BorderClass := AReader.ReadCellBorderClass;
  if HasBorderColoration then
    TRect(FCellSideColors) := AReader.ReadRect;
  BorderColor := AReader.ReadInteger;
  Color := AReader.ReadInteger;
  ContentBkColor := AReader.ReadInteger;
  ContentPattern := AReader.ReadFillPatternClass;
  FontIndex := AReader.ReadInteger;
  AReader.Read(FFormat, SizeOf(Format));
end;

procedure TdxReportVisualItem.WriteData(AWriter: TdxPSDataWriter);
begin
  inherited WriteData(AWriter);
  AWriter.WriteInteger(BackgroundBitmapIndex);
  AWriter.WriteRect(BoundsRect);
  AWriter.WriteClassName(BorderClass);
  if HasBorderColoration then
    AWriter.WriteRect(TRect(FCellSideColors));
  AWriter.WriteInteger(BorderColor);
  AWriter.WriteInteger(Color);
  AWriter.WriteInteger(ContentBkColor);
  AWriter.WriteClassName(ContentPattern);
  AWriter.WriteInteger(FontIndex);
  AWriter.Write(Format, SizeOf(Format));
end;

function TdxReportVisualItem.GetAbsoluteOrigin: TPoint;
var
  Item: TdxReportVisualItem;
  Origin: TPoint;
begin
  Result := cxNullPoint;
  Item := Self;
  while Item <> nil do
  begin
    Origin := Item.GetOrigin;
    Inc(Result.X, Origin.X);
    Inc(Result.Y, Origin.Y);
    Item := Item.Parent;
  end;
end;

function TdxReportVisualItem.GetAbsoluteRect: TRect;
begin
  Result.TopLeft := AbsoluteOrigin;
  Result.Right := Result.Left + Width;
  Result.Bottom := Result.Top + Height;
end;

function TdxReportVisualItem.GetBackgroundBitmap: TBitmap;
begin
  Result := BackgroundBitmapPool[BackgroundBitmapIndex];
end;

function TdxReportVisualItem.GetBackgroundBitmapHeight: Integer;
begin
  Result := BackgroundBitmap.Height * Renderer.UnitsPerPixel;
end;

function TdxReportVisualItem.GetBackgroundBitmapPool: TdxPSBackgroundBitmapPool;
begin
  if (ReportCells <> nil) and (ReportCells.ReportLink <> nil) then
    Result := ReportCells.ReportLink.BackgroundBitmapPool
  else
    Result := nil;
end;

function TdxReportVisualItem.GetBackgroundBitmapTileOrigin: TPoint;
begin
  Result := AbsoluteOrigin;
  Result.X := -Result.X;
  Result.Y := -Result.Y;
end;

function TdxReportVisualItem.GetBackgroundBitmapTileStartIndexX: Integer;
begin
  Result := Abs(BackgroundBitmapTileOrigin.X) div BackgroundBitmapWidth;
end;

function TdxReportVisualItem.GetBackgroundBitmapTileStartIndexY: Integer;
begin
  Result := Abs(BackgroundBitmapTileOrigin.Y) div BackgroundBitmapHeight;
end;

function TdxReportVisualItem.GetBackgroundBitmapTileStopIndexX: Integer;
begin
  Result := (Width + Abs(BackgroundBitmapTileOrigin.X)) div BackgroundBitmapWidth;
end;

function TdxReportVisualItem.GetBackgroundBitmapTileStopIndexY: Integer;
begin
  Result := (Height + Abs(BackgroundBitmapTileOrigin.Y)) div BackgroundBitmapHeight;
end;

function TdxReportVisualItem.GetBackgroundBitmapWidth: Integer;
begin
  Result := BackgroundBitmap.Width * Renderer.UnitsPerPixel;
end;

function TdxReportVisualItem.GetBorderBrush: HBRUSH;
begin
  Result := Renderer.GetBrushByColor(BorderColor);
end;

function TdxReportVisualItem.GetBorderColor: TColor;
begin
  Result := FBorderColor;
  if Result = clDefault then
    Result := ReportCells.BorderColor;
end;

function TdxReportVisualItem.GetCellSides: TdxCellSides;
begin
  Result := TdxCellSides(Byte(Format and dxPSGlbl.dxFormatRect));
end;

function TdxReportVisualItem.GetCellSideColors(ASide: TdxCellSide): TColor;
begin
  Result := FCellSideColors[ASide];
end;

function TdxReportVisualItem.GetContentBrush: HBRUSH;
begin
  Result := dxPSFillPatternFactory.Items[ContentPattern, IsPrinting].Brush.Handle;
end;

function TdxReportVisualItem.GetEdge3DSoft: Boolean;
begin
  Result := BorderClass.Edge3DSoft;
end;

function TdxReportVisualItem.GetEdge3DStyle: TdxCellEdgeStyle;
begin
  Result := BorderClass.Edge3DStyle;
end;

function TdxReportVisualItem.GetEdgeMode: TdxCellEdgeMode;
begin
  Result := BorderClass.EdgeMode;
end;

function TdxReportVisualItem.GetExcludeFromClipRgn: Boolean;
begin
  Result := GetFormatBit(dxPSGlbl.dxFormatExcludeFromClipRgn);
end;

function TdxReportVisualItem.GetFont: TFont;
begin
  if (FontIndex <> -1) and (ReportCells <> nil) then
    Result := ReportCells.GetFontByIndex(FontIndex)
  else
    Result := nil;
end;

function TdxReportVisualItem.GetHeight: Integer;
begin
  with BoundsRect do
    Result := Bottom - Top;
end;

function TdxReportVisualItem.GetIsPrinting: Boolean;
begin
  Result := Renderer.IsPrinting;
end;

function TdxReportVisualItem.GetLeft: Integer;
begin
  Result := BoundsRect.Left;
end;

function TdxReportVisualItem.GetLineThickness: Integer;
begin
  Result := Renderer.LineThickness;
end;

function TdxReportVisualItem.GetOrigin: TPoint;
begin
  Result := BoundsRect.TopLeft;
end;

function TdxReportVisualItem.GetParentBrush: HBRUSH;
begin
  Result := Renderer.GetBrushByColor(ParentColor);
end;

function TdxReportVisualItem.GetParentColor: TColor;
var
  ItemParent: TdxReportCell;
begin
  ItemParent := Parent;
  while (ItemParent <> nil) and ItemParent.Transparent do
    ItemParent := ItemParent.Parent;

  if ItemParent <> nil then
    Result := ItemParent.Color
  else
    Result := clNone;
end;

function TdxReportVisualItem.GetRenderer: TdxPSReportRenderer;
begin
  Result := ReportCells.Renderer;
end;

function TdxReportVisualItem.GetRight: Integer;
begin
  Result := BoundsRect.Right;
end;

function TdxReportVisualItem.GetShadowBrush: HBRUSH;
begin
  Result := Renderer.GetBrushByColor(ShadowColor);
end;

function TdxReportVisualItem.GetShowShadow: Boolean;
begin
  Result := GetFormatBit(dxPSGlbl.dxFormatShowShadow);
end;

function TdxReportVisualItem.GetTop: Integer;
begin
  Result := BoundsRect.Top;
end;

function TdxReportVisualItem.GetTransparent: Boolean;
begin
  Result := GetFormatBit(dxPSGlbl.dxFormatTransparent);
end;

function TdxReportVisualItem.GetWidth: Integer;
begin
  Result := cxRectWidth(BoundsRect);
end;

function TdxReportVisualItem.GetVisible: Boolean;
begin
  Result := GetFormatBit(dxPSGlbl.dxFormatVisible);
end;

procedure TdxReportVisualItem.SetBackgroundBitmapIndex(Value: Integer);
begin
  FBackgroundBitmapIndex := Max(Value, -1);
end;

procedure TdxReportVisualItem.SetBorderClass(Value: TdxPSCellBorderClass);
begin
  if Value = nil then
    Value := TdxPSCellUltraFlatBorder;
  FBorderClass := Value;
end;

procedure TdxReportVisualItem.SetBottom(const Value: Integer);
begin
  if Value <> Bottom then
    BoundsRect := cxRect(Left, Top, Right, Value);
end;

procedure TdxReportVisualItem.SetBoundsRect(const Value: TRect);
begin
  SetBounds(Value.Left, Value.Top, Value.Right - Value.Left, Value.Bottom - Value.Top);
end;

procedure TdxReportVisualItem.SetCellSides(Value: TdxCellSides);
begin
  if CellSides <> Value then
    Format := Format and not dxPSGlbl.dxFormatRect or Byte(Value);
end;

procedure TdxReportVisualItem.SetCellSideColors(ASide: TdxCellSide; AValue: TColor);
begin
  FCellSideColors[ASide] := AValue;
end;

procedure TdxReportVisualItem.SetColor(Value: TColor);
begin
  FColor := ColorToRGB(Value);
end;

procedure TdxReportVisualItem.SetEdge3DSoft(Value: Boolean);
begin
  BorderClass := MapBorderClass(EdgeMode, Edge3DStyle, Value);
end;

procedure TdxReportVisualItem.SetEdge3DStyle(Value: TdxCellEdgeStyle);
begin
  BorderClass := MapBorderClass(EdgeMode, Value, Edge3DSoft);
end;

procedure TdxReportVisualItem.SetEdgeMode(Value: TdxCellEdgeMode);
begin
  BorderClass := MapBorderClass(Value, Edge3DStyle, Edge3DSoft);
end;

procedure TdxReportVisualItem.SetExcludeFromClipRgn(Value: Boolean);
begin
  SetFormatBit(dxPSGlbl.dxFormatExcludeFromClipRgn, Value);
end;

procedure TdxReportVisualItem.SetFont(Value: TFont);
begin
  if ReportCells <> nil then
    FontIndex := ReportCells.GetIndexByFont(Value);
end;

procedure TdxReportVisualItem.SetFormat(Value: DWORD);
begin
  FFormat := Value;
end;

procedure TdxReportVisualItem.SetHeight(Value: Integer);
begin
  SetBounds(Left, Top, Width, Value);
end;

procedure TdxReportVisualItem.SetLeft(Value: Integer);
begin
  SetBounds(Value, Top, Width, Height);
end;

procedure TdxReportVisualItem.SetOrigin(const Value: TPoint);
begin
  SetBounds(Value.X, Value.Y, Width, Height);
end;

procedure TdxReportVisualItem.SetRight(const Value: Integer);
begin
  if Right <> Value then
    BoundsRect := cxRect(Left, Top, Value, Bottom);
end;

procedure TdxReportVisualItem.SetShowShadow(Value: Boolean);
begin
  SetFormatBit(dxPSGlbl.dxFormatShowShadow, Value);
end;

procedure TdxReportVisualItem.SetTop(Value: Integer);
begin
  SetBounds(Left, Value, Width, Height);
end;

procedure TdxReportVisualItem.SetTransparent(Value: Boolean);
begin
  SetFormatBit(dxPSGlbl.dxFormatTransparent, Value);
end;

procedure TdxReportVisualItem.SetWidth(Value: Integer);
begin
  SetBounds(Left, Top, Value, Height);
end;

procedure TdxReportVisualItem.SetVisible(Value: Boolean);
begin
  SetFormatBit(dxPSGlbl.dxFormatVisible, Value);
end;

{  TdxReportCell }

destructor TdxReportCell.Destroy;
begin
  SetParent(nil);
  ClearDataItems;
  ClearCells;
  inherited Destroy;
end;

procedure TdxReportCell.Assign(Source: TPersistent);

  procedure DoAssign(ASource, ADest: TdxReportCell);
  var
    I: Integer;
  begin
    for I := 0 to ASource.CellCount - 1 do
      ASource.Cells[I].Clone(ADest);

    for I := 0 to ASource.DataItemCount - 1 do
      ASource.DataItems[I].Clone(ADest);
  end;

begin
  if (Source is TdxReportCell) and (Source <> Self) then
  begin
    ClearAll;
    inherited Assign(Source);
    DoAssign(TdxReportCell(Source), Self);
  end
  else
    inherited Assign(Source);
end;

procedure TdxReportCell.DrawContent(ACanvas: TdxPSReportRenderCustomCanvas;
  DrawRect: TRect; const OriginRect: TRect; AStage: TdxPSRenderStages);
var
  R: TRect;
begin
  ACanvas.SaveClipRgn;
  try
    if ClipChildren then
    begin
      R := DrawRect;
      Dec(R.Left, BorderClass.Thickness * LineThickness);  // BorderClass.Thickness - v3.2
      Dec(R.Top, BorderClass.Thickness * LineThickness);   // BorderClass.Thickness - v3.2
      ACanvas.IntersectClipRgn(R);
    end;
    ACanvas.SaveClipRgn;
    try
      if ExcludeNestedItems(ACanvas, OriginRect) <> NULLREGION then
      begin
        DrawItself(ACanvas, AStage);
        DrawNestedDataItems(ACanvas, OriginRect, AStage);
      end;
    finally
      ACanvas.RestoreClipRgn;
    end;
    DrawNestedCells(ACanvas, DrawRect, OriginRect, AStage);
  finally
    ACanvas.RestoreClipRgn;
  end;
end;

procedure TdxReportCell.DrawItself(ACanvas: TdxPSReportRenderCustomCanvas; AStage: TdxPSRenderStages);
begin
  if rsFirstPass in AStage then
  begin
    DrawBackground(ACanvas);
    if IsBordersDrawn then
      DrawBorders(ACanvas);
  end;
end;

procedure TdxReportCell.DrawNestedCells(ACanvas: TdxPSReportRenderCustomCanvas;
  ADrawRect: TRect; const AOriginRect: TRect; AStage: TdxPSRenderStages);
var
  ACell: TdxReportCell;
  AWindowOrg: TPoint;
  I: Integer;
  R: TRect;
begin
  for I := 0 to CellCount - 1 do
  begin
    ACell := Cells[I];
    if ACell.Visible and IntersectRect(R, ACell.AbsoluteRect, AOriginRect) then
    begin
      ADrawRect := ACell.BoundsRect;
      AWindowOrg := ACanvas.OffsetWindowOrg(cxPointInvert(ADrawRect.TopLeft));
      try
        OffsetRect(ADrawRect, -ADrawRect.Left, -ADrawRect.Top);
        ACell.DrawContent(ACanvas, ADrawRect, AOriginRect, AStage);
      finally
        ACanvas.WindowOrg := AWindowOrg;
      end;
    end;
  end;
end;

procedure TdxReportCell.DrawNestedDataItems(ACanvas: TdxPSReportRenderCustomCanvas;
  const OriginRect: TRect; AStage: TdxPSRenderStages);

  procedure DoCustomDraw(AItem: TAbstractdxReportCellData);
  var
    ASavedItem: TAbstractdxReportCellData;
  begin
    ASavedItem := TAbstractdxReportCellData(AItem.Clone(nil));
    try
      if not AItem.CustomDraw(ACanvas) then
        AItem.DrawContent(ACanvas, AStage);
      if AItem.FontIndex = -1 then
        ACanvas.Font := ReportCells.GetFontByIndex(ASavedItem.FontIndex);
    finally
      AItem.Assign(ASavedItem);
      FreeAndNil(ASavedItem);
    end;
  end;

var
  AItem: TAbstractdxReportCellData;
  I: Integer;
begin
  for I := DataItemCount - 1 downto 0 do
  begin
    AItem := DataItems[I];
    if AItem.IsDrawingNeeded(ACanvas, AStage, OriginRect) then
    begin
      if AItem.IsCustomDrawn then
        DoCustomDraw(AItem)
      else
        AItem.DrawContent(ACanvas, AStage);
    end;
  end;
end;

function TdxReportCell.ExcludeNestedItems(
  ACanvas: TdxPSReportRenderCustomCanvas; const OriginRect: TRect): Integer;

  function ExcludeNestedCells(ACanvas: TdxPSReportRenderCustomCanvas; const R: TRect): Integer;
  var
    I: Integer;
  begin
    Result := SIMPLEREGION;
    for I := 0 to CellCount - 1 do
    begin
      Cells[I].DoExcludeFromClipRgn(ACanvas, R, Result);
      if Result = NULLREGION then
        Break;
    end;
  end;

  function ExcludeNestedDataItems(ACanvas: TdxPSReportRenderCustomCanvas; const R: TRect): Integer;
  var
    I: Integer;
  begin
    Result := SIMPLEREGION;
    for I := 0 to DataItemCount - 1 do
    begin
      DataItems[I].DoExcludeFromClipRgn(ACanvas, R, Result);
      if Result = NULLREGION then
        Break;
    end;
  end;

begin
  Result := ExcludeNestedCells(ACanvas, OriginRect);
  if Result <> NULLREGION then
    Result := ExcludeNestedDataItems(ACanvas, OriginRect);
end;

function TdxReportCell.GetBorderOuterBounds(ACanvas: TdxPSReportRenderCustomCanvas): TRect;
begin
  Result := BoundsRect;
  OffsetRect(Result, -Result.Left, -Result.Top);
  Result := GetBorderOuterBoundsRelativeTo(
    ACanvas, GetOuterBoundsRelativeTo(ACanvas, Result));
end;

function TdxReportCell.GetInnerBounds(ACanvas: TdxPSReportRenderCustomCanvas): TRect;
begin
  Result := BoundsRect;
  OffsetRect(Result, -Result.Left, -Result.Top);
  Result := GetInnerBoundsRelativeTo(ACanvas, Result);
end;

function TdxReportCell.GetOuterBounds(ACanvas: TdxPSReportRenderCustomCanvas): TRect;
begin
  Result := BoundsRect;
  OffsetRect(Result, -Result.Left, -Result.Top);
  Result := GetOuterBoundsRelativeTo(ACanvas, Result);
end;

function TdxReportCell.MeasureHeight(ACanvas: TdxPSReportRenderCustomCanvas): Integer;
begin
  Result := Height;
end;

function TdxReportCell.MeasureWidth(ACanvas: TdxPSReportRenderCustomCanvas): Integer;
begin
  Result := Width;
end;

procedure TdxReportCell.Offset(AOffsetX, AOffsetY: Integer);
var
  I: Integer;
begin
  for I := 0 to CellCount - 1 do
    Cells[I].Offset(AOffsetX, AOffsetY);
  for I := 0 to DataItemCount - 1 do
    DataItems[I].Offset(AOffsetX, AOffsetY);
end;

procedure TdxReportCell.InsertCell(AItem: TdxReportCell);
begin
  CellListNeeded;
  FCellList.Add(AItem);
  AItem.FReportCells := FReportCells;
end;

procedure TdxReportCell.InsertDataItem(AnItem: TdxReportItem);
begin
  DataListNeeded;
  FDataList.Add(AnItem);
end;

procedure TdxReportCell.InsertItem(AnItem: TdxReportItem);
begin
  if AnItem.IsCell then
    InsertCell(AnItem.AsCell)
  else
    InsertDataItem(AnItem);
  AnItem.FParent := Self;
end;

procedure TdxReportCell.MoveCell(ACurIndex, ANewIndex: Integer);
begin
  FCellList.Move(ACurIndex, ANewIndex);
end;

procedure TdxReportCell.MoveDataItem(ACurIndex, ANewIndex: Integer);
begin
  FDataList.Move(ACurIndex, ANewIndex);
end;

procedure TdxReportCell.MoveItem(AnItem: TdxReportItem; ACurIndex, ANewIndex: Integer);
begin
  if AnItem.IsCell then
    MoveCell(ACurIndex, ANewIndex)
  else
    MoveDataItem(ACurIndex, ANewIndex);
end;

procedure TdxReportCell.RemoveCell(AnItem: TdxReportCell);
begin
  if FCellList <> nil then
  begin
    FCellList.Remove(AnItem);
    CellListRelease;
  end
end;

procedure TdxReportCell.RemoveDataItem(AnItem: TdxReportItem);
begin
  if FDataList <> nil then
  begin
    FDataList.Remove(AnItem);
    DataListRelease;
  end;
end;

procedure TdxReportCell.RemoveItem(AnItem: TdxReportItem);
begin
  if AnItem.IsCell then
    RemoveCell(AnItem.AsCell)
  else
    RemoveDataItem(AnItem);
  AnItem.FParent := nil;
end;

procedure TdxReportCell.SetClipChildren(Value: Boolean);
begin
  SetFormatBit(dxPSGlbl.dxFormatClipChildren, Value);
end;

function TdxReportCell.GetAbsoluteIndex: Integer;
var
  Cell: TdxReportCell;
begin
  Cell := Self;
  Result := 0;
  while Cell <> nil do
  begin
    Inc(Result, Cell.Index);
    Cell := Cell.Parent;
  end;
end;

function TdxReportCell.GetLevel: Integer;
var
  Cell: TdxReportCell;
begin
  Result := 0;
  Cell := Parent;
  while Cell <> nil do
  begin
    Inc(Result);
    Cell := Cell.Parent;
  end;
end;

procedure TdxReportCell.AllocateSpaceForCells(ACapacity: Integer);
begin
  CellListNeeded;
  if ACapacity > FCellList.Capacity then
    FCellList.Capacity := ACapacity;
end;

procedure TdxReportCell.AllocateSpaceForDatas(ACapacity: Integer);
begin
  DataListNeeded;
  if ACapacity > FDataList.Capacity then
    FDataList.Capacity := ACapacity;
end;

function TdxReportCell.GetReportCells: TdxReportCells;
begin
  Result := FReportCells;
end;

procedure TdxReportCell.CellListNeeded;
begin
  if FCellList = nil then
    FCellList := TcxObjectList.Create;
end;

procedure TdxReportCell.CellListRelease;
begin
  if CellCount = 0 then
    FreeAndNil(FCellList);
end;

procedure TdxReportCell.DataListNeeded;
begin
  if FDataList = nil then
    FDataList := TcxObjectList.Create;
end;

procedure TdxReportCell.DataListRelease;
begin
  if DataItemCount = 0 then
    FreeAndNil(FDataList);
end;

procedure TdxReportCell.ClearAll;
begin
  ClearCells;
  ClearDataItems;
end;

procedure TdxReportCell.ClearCells;
begin
  FreeAndNil(FCellList);
end;

procedure TdxReportCell.ClearDataItems;
begin
  FreeAndNil(FDataList);
end;

function TdxReportCell.GetCell(Index: Integer): TdxReportCell;
begin
  Result := TdxReportCell(FCellList[Index]);
end;

function TdxReportCell.GetClipChildren: Boolean;
begin
  Result := GetFormatBit(dxPSGlbl.dxFormatClipChildren);
end;

function TdxReportCell.GetCellCount: Integer;
begin
  if FCellList <> nil then
    Result := FCellList.Count
  else
    Result := 0;
end;

function TdxReportCell.GetDataItem(Index: Integer): TAbstractdxReportCellData;
begin
  Result := TAbstractdxReportCellData(FDataList[Index]);
end;

function TdxReportCell.GetIsTopLevel: Boolean;
begin
  Result := Level = 1;
end;

function TdxReportCell.GetDataItemCount: Integer;
begin
  if FDataList <> nil then
    Result := FDataList.Count
  else
    Result := 0;
end;

function TdxReportCell.IndexOf(AnItem: TdxReportItem): Integer;
begin
  Result := -1;
  if AnItem.IsCell then
  begin
    if FCellList <> nil then
      Result := FCellList.IndexOf(AnItem)
  end
  else
    if FDataList <> nil then
      Result := FDataList.IndexOf(AnItem);
end;

class function TdxReportCell.IsCell: Boolean;
begin
  Result := True;
end;

procedure TdxReportCell.ReadData(AReader: TdxPSDataReader);
begin
  inherited ReadData(AReader);
  ReadProperties(AReader);
  ReadCells(AReader);
  ReadDataItems(AReader);
end;

procedure TdxReportCell.WriteData(AWriter: TdxPSDataWriter);
begin
  inherited WriteData(AWriter);
  WriteProperties(AWriter);
  WriteCells(AWriter);
  WriteDataItems(AWriter);
end;

procedure TdxReportCell.ReadCells(AReader: TdxPSDataReader);

  procedure LoadCell;
  var
    Cell: TdxReportCell;
    CellClass: TdxReportCellClass;
    CellClassName: string;
  begin
    CellClassName := AReader.ReadString;
    CellClass := TdxReportCellClass(GetClass(CellClassName));
    if CellClass = nil then
      ReportCellClassNotRegistered(CellClassName);
    if CellClass <> nil then
    begin
      Cell := CellClass.Create(Self);
      Cell.ReadData(AReader);
    end;
  end;

begin
  ClearCells;
  AReader.ReadListBegin;
  try
    while not AReader.EndOfList do
      LoadCell;
  finally
    AReader.ReadListEnd;
  end;
end;

procedure TdxReportCell.ReadDataItems(AReader: TdxPSDataReader);

  procedure LoadDataItem;
  var
    DataItemClassName: string;
    DataItemClass: TdxReportCellDataClass;
    DataItem: TAbstractdxReportCellData;
  begin
    DataItemClassName := AReader.ReadString;
    DataItemClass := TdxReportCellDataClass(GetClass(DataItemClassName));
    if DataItemClass = nil then
      ReportCellClassNotRegistered(DataItemClassName);
    if DataItemClass <> nil then
    begin
      DataItem := DataItemClass.Create(Self);
      DataItem.ReadData(AReader);
    end;
  end;

begin
  ClearDataItems;
  AReader.ReadListBegin;
  try
    while not AReader.EndOfList do
      LoadDataItem;
  finally
    AReader.ReadListEnd;
  end;
end;

procedure TdxReportCell.ReadProperties(AReader: TdxPSDataReader);
begin
end;

procedure TdxReportCell.WriteCells(AWriter: TdxPSDataWriter);
var
  I: Integer;
begin
  AWriter.WriteListBegin;
  try
    for I := 0 to CellCount - 1 do
      with Cells[I] do
        if Serializable then
        begin
          AWriter.WriteString(ClassName);
          WriteData(AWriter);
        end;
  finally
    AWriter.WriteListEnd;
  end;
end;

procedure TdxReportCell.WriteDataItems(AWriter: TdxPSDataWriter);
var
  I: Integer;
begin
  AWriter.WriteListBegin;
  try
    for I := 0 to DataItemCount - 1 do
      with DataItems[I] do
        if Serializable then
        begin
          AWriter.WriteString(ClassName);
          WriteData(AWriter);
        end;
  finally
    AWriter.WriteListEnd;
  end;
end;

procedure TdxReportCell.WriteProperties(AWriter: TdxPSDataWriter);
begin
end;

function TdxReportCell.AddCell: TdxReportCell;
begin
  Result := TdxReportCell.Create(Self);
end;

function TdxReportCell.AddDataItem(AClass: TdxReportCellDataClass): TAbstractdxReportCellData;
begin
  Result := AClass.Create(Self);
end;

procedure TdxReportCell.DeleteCell(Index: Integer);
var
  Cell: TdxReportCell;
begin
  Cell := Cells[Index];
  Cell.Parent := nil;
end;

procedure TdxReportCell.DeleteDataItem(Index: Integer);
var
  DataItem: TAbstractdxReportCellData;
begin
  DataItem := DataItems[Index];
  DataItem.Parent := nil;
end;

function TdxReportCell.LastCell: TdxReportCell;
begin
  if CellCount > 0 then
    Result := TdxReportCell(FCellList.Last)
  else
    Result := nil;
end;

function TdxReportCell.FirstCell: TdxReportCell;
begin
  if CellCount > 0 then
    Result := TdxReportCell(FCellList.First)
  else
    Result := nil;
end;

function TdxReportCell.FindDataItemByData(
  const AData: Int64; var ADataItem: TAbstractdxReportCellData): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to DataItemCount - 1 do
  begin
    Result := DataItems[I].Data = AData;
    if Result then
    begin
      ADataItem := DataItems[I];
      Break;
    end;
  end;
end;

function TdxReportCell.HasChildren: Boolean;
begin
  Result := CellCount > 0;
end;

procedure TdxReportCell.ConvertCoords(APixelsNumerator, APixelsDenominator: Integer);
var
  I: Integer;
begin
  for I := 0 to DataItemCount - 1 do
    DataItems[I].ConvertCoords(APixelsNumerator, APixelsDenominator);
  for I := 0 to CellCount - 1 do
    Cells[I].ConvertCoords(APixelsNumerator, APixelsDenominator);
  inherited ConvertCoords(APixelsNumerator, APixelsDenominator);
end;

function TdxReportCell.GetBackgroundBitmapTileBounds(Col, Row: Integer): TRect;
begin
  Result := inherited GetBackgroundBitmapTileBounds(Col, Row);
  OffsetRect(Result, -Left, -Top);
end;

function TdxReportCell.GetBackgroundBounds(ACanvas: TdxPSReportRenderCustomCanvas): TRect;
var
  R: TRect;
begin
  R := BoundsRect;
  OffsetRect(R, -R.Left, -R.Top);
  Result := GetInnerBoundsRelativeTo(ACanvas, R);
end;

function TdxReportCell.GetBorderBounds(ACanvas: TdxPSReportRenderCustomCanvas): TRect;
begin
  Result := inherited GetBorderBounds(ACanvas);
  OffsetRect(Result, -Result.Left, -Result.Top);
end;

{ TdxPSReportGroupLookAndFeelPainter }

constructor TdxPSReportGroupLookAndFeelPainter.Create(ARenderer: TdxPSReportRenderer);
begin
  inherited Create;
  FRenderer := ARenderer;
end;

procedure TdxPSReportGroupLookAndFeelPainter.Paint(ACanvas: TdxPSReportRenderCustomCanvas);
begin
end;

procedure TdxPSReportGroupLookAndFeelPainter.DrawBorders(ACanvas: TdxPSReportRenderCustomCanvas);
begin
end;

procedure TdxPSReportGroupLookAndFeelPainter.DrawCaption(ACanvas: TdxPSReportRenderCustomCanvas);
begin
  if Group.CheckBoxVisible then
    DrawCheckBox(ACanvas);
  DrawCaptionText(ACanvas);
end;

procedure TdxPSReportGroupLookAndFeelPainter.DrawCaptionText(ACanvas: TdxPSReportRenderCustomCanvas);

  procedure CalculateTextBounds(var R: TRect);
  var
    ACheckWidth: Integer;
  begin
    ACheckWidth := LookAndFeel.GetCheckWidth;
    if Group.CheckBoxVisible then
      case Group.CaptionAlignment of
        taLeft:
          OffsetRect(R, ACheckWidth, 0);
        taCenterX:
          OffsetRect(R, (R.Right - R.Left - Group.CaptionTextWidth + ACheckWidth) div 2, 0);
        taRight:
      end;
  end;

var
  R: TRect;
begin
  R := LookAndFeel.GetCaptionTextBounds(Group);
  CalculateTextBounds(R);
  if ACanvas.IsRectVisible(R) then
    Renderer.DrawText(ACanvas, R, 1, 0, 0, Group.CaptionText, LookAndFeel.CaptionFont,
      LookAndFeel.GetCaptionColor, Group.CaptionAlignment, taCenterY,
      not Group.CaptionTransparent, False, False, False, False, True);
end;

procedure TdxPSReportGroupLookAndFeelPainter.DrawCheckBox(ACanvas: TdxPSReportRenderCustomCanvas);
var
  R: TRect;
begin
  R := LookAndFeel.GetCheckBounds(Group);
  Renderer.DrawCheckBox(ACanvas, R, Group.CheckBoxChecked, True, False, cbesUltraFlat);
end;

procedure TdxPSReportGroupLookAndFeelPainter.Initialize(ALookAndFeel: TdxPSReportGroupLookAndFeel;
  AGroup: TdxReportGroup);
begin
  FGroup := AGroup;
  FLookAndFeel := ALookAndFeel;
end;

function TdxPSReportGroupLookAndFeelPainter.Group: TdxReportGroup;
begin
  Result := FGroup;
end;

function TdxPSReportGroupLookAndFeelPainter.LookAndFeel: TdxPSReportGroupLookAndFeel;
begin
  Result := FLookAndFeel;
end;

{ TdxPSReportGroupLookAndFeel }

constructor TdxPSReportGroupLookAndFeel.Create(AReportCells: TdxReportCells);
begin
  inherited Create;
  FReportCells := AReportCells;
  FCaptionFontIndex := -1;
  FCaptionHeight := 0;
  FCaptionIndent := dxDefaultReportGroupCaptionIndent;
  FColor := dxDefaultGroupColor;
  FFontIndex := -1;
end;

procedure TdxPSReportGroupLookAndFeel.AdjustBorderOuterBounds(var R: TRect; AGroup: TdxReportGroup);
begin
  Inc(R.Top, GetCaptionHeight(AGroup) div 2);
end;

procedure TdxPSReportGroupLookAndFeel.Assign(Source: TPersistent);
begin
  if Source is TdxPSReportGroupLookAndFeel then
    with TdxPSReportGroupLookAndFeel(Source) do
    begin
      Self.CaptionFontIndex := CaptionFontIndex;
      Self.FCaptionIndent := CaptionIndent;
      Self.Color := Color;
      Self.FontIndex := FontIndex;
    end
  else
    inherited;
end;

class function TdxPSReportGroupLookAndFeel.BorderClass: TdxPSCellBorderClass;
begin
  Result := TdxPSCellEtchedBorder;
end;

class function TdxPSReportGroupLookAndFeel.DefaultBorderSides: TdxCellSides;
begin
  Result := csAll;
end;

class function TdxPSReportGroupLookAndFeel.Name: string;
begin
  Result := '';
end;

class procedure TdxPSReportGroupLookAndFeel.Register;
begin
  if GetClass(ClassName) = nil then RegisterClass(Self);
end;

class procedure TdxPSReportGroupLookAndFeel.Unregister;
begin
end;

procedure TdxPSReportGroupLookAndFeel.Paint(
  ACanvas: TdxPSReportRenderCustomCanvas; AGroup: TdxReportGroup);
begin
  with GetPainter do
  begin
    Initialize(Self, AGroup);
    Paint(ACanvas);
  end;
end;

procedure TdxPSReportGroupLookAndFeel.Prepare(ACanvas: TdxPSReportRenderCustomCanvas);
begin
  FCaptionHeight := 2 * Renderer.CalcTextPatternHeight(ACanvas, CaptionFont) div 2;
end;

procedure TdxPSReportGroupLookAndFeel.ConvertCoords(APixelsNumerator, APixelsDenominator: Integer);
begin
  FCaptionHeight := MulDiv(FCaptionHeight, APixelsNumerator, APixelsDenominator);
  FCaptionIndent := MulDiv(FCaptionIndent, APixelsNumerator, APixelsDenominator);
end;

procedure TdxPSReportGroupLookAndFeel.ReadData(AReader: TdxPSDataReader);
begin
  FCaptionHeight := AReader.ReadInteger;
  CaptionFontIndex := AReader.ReadInteger;
  FCaptionIndent := AReader.ReadInteger;
  Color := AReader.ReadInteger;
  Data := Pointer(AReader.ReadInteger);
  FontIndex := AReader.ReadInteger;
end;

procedure TdxPSReportGroupLookAndFeel.WriteData(AWriter: TdxPSDataWriter);
begin
  AWriter.WriteInteger(FCaptionHeight);
  AWriter.WriteInteger(CaptionFontIndex);
  AWriter.WriteInteger(CaptionIndent);
  AWriter.WriteInteger(Color);
  AWriter.WriteInteger(Integer(Data));
  AWriter.WriteInteger(FontIndex);
end;

function TdxPSReportGroupLookAndFeel.GetBorderEdgeThickness(AGroup: TdxReportGroup;
  ASide: TdxCellSide): Integer;
begin
  if ASide = csTop then
  begin
    Result := BorderThickness;
    if Renderer.IsRendering and (Renderer.LineThickness <> 0) then
      Result := Result * Renderer.LineThickness;
    if AGroup.ShowCaption then
      Inc(Result, GetCaptionAreaHeight(AGroup));
    if Renderer.IsRendering and (Renderer.LineThickness <> 0) then
      Result := Result div Renderer.LineThickness;
  end
  else
    Result := AGroup.InternalGetBorderEdgeThickness(ASide);
end;

function TdxPSReportGroupLookAndFeel.GetBorderThickness: Integer;
begin
  Result := 0;
end;

function TdxPSReportGroupLookAndFeel.GetColor: TColor;
begin
  Result := FColor;
  if Result = clDefault then
    Result := ReportCells.GroupColor;
end;

procedure TdxPSReportGroupLookAndFeel.SetBorderThickness(Value: Integer);
begin
end;

function TdxPSReportGroupLookAndFeel.GetCaptionAreaHeight(AGroup: TdxReportGroup): Integer;
begin
  Result := GetCaptionHeight(AGroup);
end;

function TdxPSReportGroupLookAndFeel.GetCaptionBounds(AGroup: TdxReportGroup): TRect;
begin
  Result := AGroup.BoundsRect;
  Result.Bottom := Result.Top + GetCaptionHeight(AGroup);
end;

function TdxPSReportGroupLookAndFeel.GetCaptionColor: TColor;
begin
  Result := Color;
end;

function TdxPSReportGroupLookAndFeel.GetCaptionHeight(AGroup: TdxReportGroup): Integer;
begin
  Result := FCaptionHeight;
end;

function TdxPSReportGroupLookAndFeel.GetCaptionIndent: Integer;
begin
  Result := FCaptionIndent;
end;

function TdxPSReportGroupLookAndFeel.GetCaptionLeftRestSpaceBounds(AGroup: TdxReportGroup): TRect;
begin
  if AGroup.Transparent then
    Result := cxNullRect
  else
  begin
    Result := GetCaptionBounds(AGroup);
    Result.Right := GetCaptionTextBounds(AGroup).Left;
  end;
end;

function TdxPSReportGroupLookAndFeel.GetCaptionRightRestSpaceBounds(AGroup: TdxReportGroup): TRect;
begin
  if AGroup.Transparent then
    Result := cxNullRect
  else
  begin
    Result := GetCaptionBounds(AGroup);
    Result.Left := GetCaptionTextBounds(AGroup).Right;
  end;
end;

function TdxPSReportGroupLookAndFeel.GetCaptionTextBounds(AGroup: TdxReportGroup): TRect;
var
  TextWidth: Integer;
begin
  Result := cxNullRect;
  if AGroup.ShowCaption then
  begin
    TextWidth := AGroup.CaptionTextWidth;
    if TextWidth <> 0 then
    begin
      Result := AGroup.BoundsRect;
      OffsetRect(Result, -Result.Left, -Result.Top);
      InflateRect(Result, -CaptionIndent, 0);

      if not IsRectEmpty(Result) then
        with Result do
        begin
          case AGroup.CaptionAlignment of
            taLeft:
              Right := Left + TextWidth;
            taCenterX:
              begin
                Inc(Left, (Right - Left - TextWidth) div 2);
                Right := Left + TextWidth;
              end;
            taRight:
              Left := Right - TextWidth;
          end;
          Bottom := Top + GetCaptionHeight(AGroup);
        end;
    end;
  end;
end;

function TdxPSReportGroupLookAndFeel.GetCheckBounds(AGroup: TdxReportGroup): TRect;
begin
  Result := GetCaptionTextBounds(AGroup);
  Result.Right := Result.Left + GetCheckWidth;
end;

function TdxPSReportGroupLookAndFeel.GetCheckWidth: Integer;
begin
  Result := MulDiv(dxPSGlbl.CheckWidth, PixelsNumerator, Screen.PixelsPerInch);
end;

function TdxPSReportGroupLookAndFeel.GetBorderSides(AGroup: TdxReportGroup): TdxCellSides;
begin
  Result := DefaultBorderSides;
end;

function TdxPSReportGroupLookAndFeel.GetPainter: TdxPSReportGroupLookAndFeelPainter;
begin
  Result := Renderer.GetReportGroupLookAndFeelPainter(GetPainterClass);
end;

class function TdxPSReportGroupLookAndFeel.GetPainterClass: TdxPSReportGroupLookAndFeelPainterClass;
begin
  Result := nil; // Actually method must be declared as "abstract" but "C++" syntax does not allow us to use "static virtual abstract" methods
end;

function TdxPSReportGroupLookAndFeel.GetCaptionFont: TFont;
begin
  if CaptionFontIndex <> -1 then
    Result := ReportCells.GetFontByIndex(CaptionFontIndex)
  else
    Result := nil;
end;

function TdxPSReportGroupLookAndFeel.GetFont: TFont;
begin
  if FontIndex <> -1 then
    Result := ReportCells.GetFontByIndex(FontIndex)
  else
    Result := nil;
end;

function TdxPSReportGroupLookAndFeel.GetRenderer: TdxPSReportRenderer;
begin
  Result := ReportCells.Renderer;
end;

procedure TdxPSReportGroupLookAndFeel.SetCaptionFont(Value: TFont);
begin
  CaptionFontIndex := ReportCells.GetIndexByFont(Value);
end;

procedure TdxPSReportGroupLookAndFeel.SetCaptionFontIndex(Value: Integer);
begin
  FCaptionFontIndex := Max(Value, -1);
end;

procedure TdxPSReportGroupLookAndFeel.SetFont(Value: TFont);
begin
  FontIndex := ReportCells.GetIndexByFont(Value);
end;

procedure TdxPSReportGroupLookAndFeel.SetFontIndex(Value: Integer);
begin
  FFontIndex := Max(Value, -1);
end;

{ TdxPSReportGroupNullLookAndFeelPainter }

procedure TdxPSReportGroupNullLookAndFeelPainter.Paint(
  ACanvas: TdxPSReportRenderCustomCanvas);
begin
end;

{ TdxPSReportGroupNullLookAndFeel }

class function TdxPSReportGroupNullLookAndFeel.BorderClass: TdxPSCellBorderClass;
begin
  Result := TdxPSCellUltraFlatBorder;
end;

class function TdxPSReportGroupNullLookAndFeel.DefaultBorderSides: TdxCellSides;
begin
  Result := [];
end;

class function TdxPSReportGroupNullLookAndFeel.Name: string;
begin
  Result := cxGetResourceString(@sdxReportGroupNullLookAndFeel);
end;

function TdxPSReportGroupNullLookAndFeel.GetBorderThickness: Integer;
begin
  Result := 1;
end;

function TdxPSReportGroupNullLookAndFeel.GetCaptionHeight(AGroup: TdxReportGroup): Integer;
begin
  Result := 0;
end;

class function TdxPSReportGroupNullLookAndFeel.GetPainterClass: TdxPSReportGroupLookAndFeelPainterClass;
begin
  Result := TdxPSReportGroupNullLookAndFeelPainter;
end;

{ TdxPSReportGroupStandardLookAndFeelPainter }

procedure TdxPSReportGroupStandardLookAndFeelPainter.Paint(ACanvas: TdxPSReportRenderCustomCanvas);
begin
  DrawCaptionRestSpace(ACanvas);
  DrawCaption(ACanvas);
  if Group.IsBordersDrawn then
  begin
    ACanvas.SaveClipRgn;
    try
      ACanvas.ExcludeClipRect(LookAndFeel.GetCaptionTextBounds(Group));
      DrawBorders(ACanvas);
    finally
      ACanvas.RestoreClipRgn;
    end;
  end;
end;

procedure TdxPSReportGroupStandardLookAndFeelPainter.DrawBorders(ACanvas: TdxPSReportRenderCustomCanvas);
begin
  Group.InternalDrawBorders(ACanvas);
end;

procedure TdxPSReportGroupStandardLookAndFeelPainter.DrawCaptionRestSpace(ACanvas: TdxPSReportRenderCustomCanvas);

  procedure DrawCaptionPart(const R: TRect);
  begin
    if not cxRectIsEmpty(R) and ACanvas.IsRectVisible(R) then
      Renderer.FillRect(ACanvas, R, LookAndFeel.Color);
  end;

begin
  DrawCaptionPart(LookAndFeel.GetCaptionLeftRestSpaceBounds(Group));
  DrawCaptionPart(LookAndFeel.GetCaptionRightRestSpaceBounds(Group));
end;

function TdxPSReportGroupStandardLookAndFeelPainter.LookAndFeel: TdxPSReportGroupStandardLookAndFeel;
begin
  Result := inherited LookAndFeel as TdxPSReportGroupStandardLookAndFeel;
end;

{ TdxPSReportGroupStandardLookAndFeel }

class function TdxPSReportGroupStandardLookAndFeel.BorderClass: TdxPSCellBorderClass;
begin
  Result := TdxPSCellEtchedBorder;
end;

class function TdxPSReportGroupStandardLookAndFeel.Name: string;
begin
  Result := cxGetResourceString(@sdxReportGroupStandardLookAndFeel);
end;

class function TdxPSReportGroupStandardLookAndFeel.GetPainterClass: TdxPSReportGroupLookAndFeelPainterClass;
begin
  Result := TdxPSReportGroupStandardLookAndFeelPainter;
end;

{ TdxPSReportGroupPanelStyleLookAndFeel }

procedure TdxPSReportGroupPanelStyleLookAndFeel.AdjustBorderOuterBounds(var R: TRect; AGroup: TdxReportGroup);
begin
  // do nothing
end;

function TdxPSReportGroupPanelStyleLookAndFeel.GetBorderEdgeThickness(
  AGroup: TdxReportGroup; ASide: TdxCellSide): Integer;
begin
  Result := AGroup.InternalGetBorderEdgeThickness(ASide);
end;

class function TdxPSReportGroupPanelStyleLookAndFeel.GetPainterClass: TdxPSReportGroupLookAndFeelPainterClass;
begin
  Result := TdxPSReportGroupPanelStyleLookAndFeelPainter;
end;

{ TdxPSReportGroupPanelStyleLookAndFeelPainter }

procedure TdxPSReportGroupPanelStyleLookAndFeelPainter.Paint(ACanvas: TdxPSReportRenderCustomCanvas);
begin
  DrawCaptionText(ACanvas);
  if Group.IsBordersDrawn then
    DrawBorders(ACanvas);
end;

{ TdxPSReportGroupOfficeLookAndFeel }

class function TdxPSReportGroupOfficeLookAndFeel.DefaultBorderSides: TdxCellSides;
begin
  Result := [csTop];
end;

class function TdxPSReportGroupOfficeLookAndFeel.Name: string;
begin
  Result := cxGetResourceString(@sdxReportGroupOfficeLookAndFeel);
end;

function TdxPSReportGroupOfficeLookAndFeel.GetCaptionIndent: Integer;
begin
  Result := 0;
end;

{ TdxPSReportGroupWebLookAndFeelPainter }

procedure TdxPSReportGroupWebLookAndFeelPainter.Paint(ACanvas: TdxPSReportRenderCustomCanvas);
begin
  DrawBorders(ACanvas);
  DrawCaptionText(ACanvas);
  DrawCaptionSeparator(ACanvas);
end;

procedure TdxPSReportGroupWebLookAndFeelPainter.DrawBorders(
  ACanvas: TdxPSReportRenderCustomCanvas);
var
  R: TRect;
begin
  R := Group.BoundsRect;
  OffsetRect(R, -R.Left, -R.Top);
  Renderer.FrameRect(ACanvas, R, LookAndFeel.BorderColor,
    Group.CellSides, LookAndFeel.BorderThickness);
end;

procedure TdxPSReportGroupWebLookAndFeelPainter.DrawCaptionSeparator(
  ACanvas: TdxPSReportRenderCustomCanvas);
var
  R: TRect;
begin
  R := LookAndFeel.GetCaptionSeparatorBounds(Group);
  if ACanvas.IsRectVisible(R) then
    Renderer.FillRect(ACanvas, R, LookAndFeel.CaptionSeparatorColor);
end;

function TdxPSReportGroupWebLookAndFeelPainter.LookAndFeel: TdxPSReportGroupWebLookAndFeel;
begin
  Result := inherited LookAndFeel as TdxPSReportGroupWebLookAndFeel;
end;

{ TdxPSReportGroupWebLookAndFeel }

constructor TdxPSReportGroupWebLookAndFeel.Create(AReportCells: TdxReportCells);
begin
  inherited Create(AReportCells);
  BorderColor := clDefault;
  BorderThickness := 1;
  CaptionColor := clDefault;
  CaptionSeparatorColor := clDefault;
  CaptionSeparatorThickness := 0;
end;

procedure TdxPSReportGroupWebLookAndFeel.Assign(Source: TPersistent);
begin
  if Source is TdxPSReportGroupWebLookAndFeel then
    with TdxPSReportGroupWebLookAndFeel(Source) do
    begin
      Self.BorderColor := BorderColor;
      Self.BorderThickness := BorderThickness;
      Self.CaptionColor := CaptionColor;
      Self.CaptionSeparatorColor := CaptionSeparatorColor;
      Self.CaptionSeparatorThickness := CaptionSeparatorThickness;
    end;
  inherited;
end;

class function TdxPSReportGroupWebLookAndFeel.BorderClass: TdxPSCellBorderClass;
begin
  Result := TdxPSCellUltraFlatBorder;
end;

class function TdxPSReportGroupWebLookAndFeel.Name: string;
begin
  Result := cxGetResourceString(@sdxReportGroupWebLookAndFeel);
end;

procedure TdxPSReportGroupWebLookAndFeel.ConvertCoords(APixelsNumerator, APixelsDenominator: Integer);
begin
  inherited;
  CaptionSeparatorThickness := MulDiv(CaptionSeparatorThickness, APixelsNumerator, APixelsDenominator);
end;

procedure TdxPSReportGroupWebLookAndFeel.ReadData(AReader: TdxPSDataReader);
begin
  inherited ReadData(AReader);
  BorderColor := AReader.ReadInteger;
  BorderThickness := AReader.ReadInteger;
  CaptionColor := AReader.ReadInteger;
  CaptionSeparatorColor := AReader.ReadInteger;
  CaptionSeparatorThickness := AReader.ReadInteger;
end;

procedure TdxPSReportGroupWebLookAndFeel.WriteData(AWriter: TdxPSDataWriter);
begin
  inherited WriteData(AWriter);
  AWriter.WriteInteger(BorderColor);
  AWriter.WriteInteger(BorderThickness);
  AWriter.WriteInteger(CaptionColor);
  AWriter.WriteInteger(CaptionSeparatorColor);
  AWriter.WriteInteger(CaptionSeparatorThickness);
end;

function TdxPSReportGroupWebLookAndFeel.GetBorderEdgeThickness(AGroup: TdxReportGroup;
  ASide: TdxCellSide): Integer;
begin
  if ASide = csTop then
    Result := inherited GetBorderEdgeThickness(AGroup, ASide)
  else
    Result := BorderThickness;
end;

function TdxPSReportGroupWebLookAndFeel.GetBorderThickness: Integer;
begin
  Result := FBorderThickness;
end;

procedure TdxPSReportGroupWebLookAndFeel.SetBorderThickness(Value: Integer);
begin
  FBorderThickness := Max(Value, 0);
end;

function TdxPSReportGroupWebLookAndFeel.GetCaptionAreaHeight(AGroup: TdxReportGroup): Integer;
begin
  Result := inherited GetCaptionAreaHeight(AGroup) + CaptionSeparatorThickness;
end;

function TdxPSReportGroupWebLookAndFeel.GetCaptionBounds(AGroup: TdxReportGroup): TRect;
begin
  if AGroup.ShowCaption then
  begin
    Result := AGroup.BoundsRect;
    OffsetRect(Result, -Result.Left, -Result.Top);
    InflateRect(Result, -BorderThickness * Renderer.LineThickness, -BorderThickness * Renderer.LineThickness);
    Result.Bottom := Result.Top + GetCaptionHeight(AGroup);
  end
  else
    Result := cxNullRect;
end;

function TdxPSReportGroupWebLookAndFeel.GetCaptionLeftRestSpaceBounds(AGroup: TdxReportGroup): TRect;
begin
  Result := cxNullRect;
end;

function TdxPSReportGroupWebLookAndFeel.GetCaptionRightRestSpaceBounds(AGroup: TdxReportGroup): TRect;
begin
  Result := cxNullRect;
end;

function TdxPSReportGroupWebLookAndFeel.GetCaptionSeparatorBounds(AGroup: TdxReportGroup): TRect;
begin
  Result := GetCaptionBounds(AGroup);
  if AGroup.ShowCaption then
  begin
    Result.Top := Result.Bottom;
    Result.Bottom := Result.Top + CaptionSeparatorThickness;
  end;
end;

function TdxPSReportGroupWebLookAndFeel.GetCaptionTextBounds(AGroup: TdxReportGroup): TRect;
begin
  Result := GetCaptionBounds(AGroup);
end;

class function TdxPSReportGroupWebLookAndFeel.GetPainterClass: TdxPSReportGroupLookAndFeelPainterClass;
begin
  Result := TdxPSReportGroupWebLookAndFeelPainter;
end;

function TdxPSReportGroupWebLookAndFeel.GetBorderColor: TColor;
begin
  Result := FBorderColor;
  if Result = clDefault then Result := ReportCells.GroupBorderColor;
end;

function TdxPSReportGroupWebLookAndFeel.GetCaptionColor: TColor;
begin
  Result := FCaptionColor;
  if Result = clDefault then Result := ReportCells.GroupCaptionColor;
end;

function TdxPSReportGroupWebLookAndFeel.GetCaptionSeparatorColor: TColor;
begin
  Result := FCaptionSeparatorColor;
  if Result = clDefault then Result := ReportCells.GroupCaptionSeparatorColor;
end;

procedure TdxPSReportGroupWebLookAndFeel.SetCaptionSeparatorThickness(Value: Integer);
begin
  FCaptionSeparatorThickness := Max(Value, 0);
end;

{ TdxReportGroup }

constructor TdxReportGroup.Create(AParent: TdxReportCell);
begin
  inherited Create(AParent);
  CaptionAlignment := taLeft;
  ClipChildren := True;
  ShowCaption := True;
end;

procedure TdxReportGroup.Assign(Source: TPersistent);
begin
  if Source is TdxReportGroup then
  begin
    inherited;
    with TdxReportGroup(Source) do
    begin
      Self.CaptionText := CaptionText;
      if FLookAndFeel <> nil then
        Self.FLookAndFeel := Self.ReportCells.FindGroupLookAndFeelByData(FLookAndFeel.Data)
      else
        Self.FLookAndFeel := nil;
    end;
  end
  else
    inherited;
end;

procedure TdxReportGroup.DrawBorders(ACanvas: TdxPSReportRenderCustomCanvas);
begin
  if LookAndFeel <> nil then
  begin
    if (ReportLink <> nil) and not ReportLink.ScaleFonts then
      CalculateCaptionTextWidth(ACanvas);
    LookAndFeel.Paint(ACanvas, Self);
  end
  else
    inherited DrawBorders(ACanvas);
end;

procedure TdxReportGroup.CalculateCaptionTextWidth(ACanvas: TdxPSReportRenderCustomCanvas);
var
  AFont: TFont;
begin
  FCaptionTextWidth := 0;
  if CaptionText <> '' then
  begin
    if LookAndFeel <> nil then
      AFont := LookAndFeel.CaptionFont
    else
      AFont := Font;

    FCaptionTextWidth := Renderer.CalcTextWidth(ACanvas, CaptionText, AFont);
    if FCaptionTextWidth > 0 then
      Inc(FCaptionTextWidth, 4);
  end;
end;

function TdxReportGroup.GetBorderOuterBounds(ACanvas: TdxPSReportRenderCustomCanvas): TRect;
begin
  Result := inherited GetBorderOuterBounds(ACanvas);
  AdjustBorderOuterBounds(Result);
end;

procedure TdxReportGroup.AdjustBorderOuterBounds(var R: TRect);
begin
  if ShowCaption and (LookAndFeel <> nil) then
    LookAndFeel.AdjustBorderOuterBounds(R, Self);
end;

procedure TdxReportGroup.ConvertCoords(APixelsNumerator, APixelsDenominator: Integer);
begin
  inherited ConvertCoords(APixelsNumerator, APixelsDenominator);
  FCaptionTextWidth := MulDiv(FCaptionTextWidth, APixelsNumerator, APixelsDenominator);
end;

procedure TdxReportGroup.InternalDrawBorders(ACanvas: TdxPSReportRenderCustomCanvas);
begin
  inherited DrawBorders(ACanvas);
end;

function TdxReportGroup.GetBorderClass: TdxPSCellBorderClass;
begin
  if UseOwnBorderClass or (LookAndFeel = nil) then
    Result := inherited GetBorderClass
  else
    Result := LookAndFeel.BorderClass;
end;

function TdxReportGroup.GetBorderEdgeSalient(ASide: TdxCellSide; ASalient: TdxPSCellBorderSalientType): Integer;
begin
  if LookAndFeel <> nil then
    if ASalient = bstOuter then
      Result := 0
    else
      Result := GetBorderEdgeThickness(ASide)
  else
    Result := inherited GetBorderEdgeSalient(ASide, ASalient);
end;

function TdxReportGroup.GetBorderEdgeThickness(ASide: TdxCellSide): Integer;
begin
  if LookAndFeel <> nil then
    Result := LookAndFeel.GetBorderEdgeThickness(Self, ASide)
  else
    Result := inherited GetBorderEdgeThickness(ASide);
end;

function TdxReportGroup.InternalGetBorderEdgeThickness(ASide: TdxCellSide): Integer;
begin
  Result := inherited GetBorderEdgeThickness(ASide);
end;

function TdxReportGroup.IsBordersDrawn: Boolean;
begin
  Result := inherited IsBordersDrawn or (CaptionText <> '');
end;

procedure TdxReportGroup.ReadData(AReader: TdxPSDataReader);
var
  Index: Integer;
begin
  inherited ReadData(AReader);
  CaptionText := AReader.ReadString;
  FCaptionTextWidth := AReader.ReadInteger;

  Index := AReader.ReadInteger;
  if Index <> -1 then
    LookAndFeel := ReportCells.LookAndFeels[Index];
end;

procedure TdxReportGroup.WriteData(AWriter: TdxPSDataWriter);
begin
  inherited WriteData(AWriter);
  AWriter.WriteString(CaptionText);
  AWriter.WriteInteger(CaptionTextWidth);
  AWriter.WriteInteger(LookAndFeelIndex);
end;

function TdxReportGroup.GetCaptionAlignment: TcxTextAlignX;
begin
  Result := TcxTextAlignX((Format and dxPSGlbl.dxFormatTextAlignXMask) shr dxPSGlbl.dxFormatTextAlignXOffset);
end;

function TdxReportGroup.GetCaptionTextWidth: Integer;
begin
  Result := FCaptionTextWidth;
  if FCheckBoxVisible then
    Result := Result + LookAndFeel.GetCheckWidth;
end;

function TdxReportGroup.GetCaptionTransparent: Boolean;
begin
  Result := GetFormatBit(dxPSGlbl.dxFormatGroupCaptionTransparent);
end;

function TdxReportGroup.GetLookAndFeel: TdxPSReportGroupLookAndFeel;
begin
  Result := FLookAndFeel;
  if Result = nil then
    if Parent is TdxReportGroup then
      Result := TdxReportGroup(Parent).LookAndFeel
    else
      Result := ReportCells.LookAndFeel;
end;

function TdxReportGroup.GetLookAndFeelIndex: Integer;
begin
  if FLookAndFeel <> nil then
    Result := ReportCells.IndexOfReportGroupLookAndFeel(LookAndFeel)
  else
    Result := -1;
end;

function TdxReportGroup.GetShowCaption: Boolean;
begin
  Result := GetFormatBit(dxPSGlbl.dxFormatGroupShowCaption);
end;

function TdxReportGroup.GetUseOwnBorderClass: Boolean;
begin
  Result := GetFormatBit(dxPSGlbl.dxFormatOwnBorderClass);
end;

procedure TdxReportGroup.SetCaptionAlignment(Value: TcxTextAlignX);
begin
  Format := Format and not dxPSGlbl.dxFormatTextAlignXMask or (Byte(Value) shl dxPSGlbl.dxFormatTextAlignXOffset);
end;

procedure TdxReportGroup.SetCaptionTransparent(Value: Boolean);
begin
  SetFormatBit(dxPSGlbl.dxFormatGroupCaptionTransparent, Value);
end;

procedure TdxReportGroup.SetLookAndFeel(Value: TdxPSReportGroupLookAndFeel);
begin
  FLookAndFeel := Value;
end;

procedure TdxReportGroup.SetShowCaption(Value: Boolean);
begin
  SetFormatBit(dxPSGlbl.dxFormatGroupShowCaption, Value);
end;

procedure TdxReportGroup.SetUseOwnBorderClass(Value: Boolean);
begin
  SetFormatBit(dxPSGlbl.dxFormatOwnBorderClass, Value);
end;

{ TdxReportCells }

constructor TdxReportCells.Create(AReportLink: TBasedxReportLink);
begin
  inherited Create;
  FReportLink := AReportLink;

  BorderColor := dxDefaultGridLineColor;
  BorderWidth := 1;
  ExpandButtonBorderColor := dxDefaultExpandButtonBorderColor;
  GroupBorderColor := dxDefaultGroupBorderColor;
  GroupCaptionColor := dxDefaultGroupCaptionColor;
  GroupCaptionSeparatorColor := dxDefaultGroupCaptionSeparatorColor;
  GroupColor := dxDefaultGroupColor;
  ShadowColor := dxDefaultShadowColor;
  ShadowDepth := dxDefaultShadowDepth;
  TreeLineColor := dxDefaultTreeLineColor;
  TreeLineStyle := tlsDot;

  FLookAndFeels := TList.Create;
  FLookAndFeel := CreateGroupLookAndFeel(dxDefaultReportGroupLookAndFeel);
  CreateGroupLookAndFeel(TdxPSReportGroupNullLookAndFeel);

  FImageLists := TList.Create;

  FCells := TdxReportCell.Create(nil);
  FCells.FReportCells := Self;
  FCells.Color := dxDefaultContentColor;
end;

destructor TdxReportCells.Destroy;
begin
  FreeAndNil(FCells);
  FreeAndNilReportGroupLookAndFeels;
  FreeAndNil(FFooterCells);
  FreeAndNil(FHeaderCells);
  FreeAndNil(FHeaderCornerCells);
  FreeAndNil(FRowHeaderCells);
  FreeAndNilOverlays;
  FreeAndNilImageLists;
  inherited;
end;

procedure TdxReportCells.Assign(Source: TPersistent);
begin
  if Source is TdxReportCells then
    with TdxReportCells(Source) do
    begin
      Self.BorderColor := BorderColor;
      Self.BorderWidth := BorderWidth;
      Self.ExpandButtonBorderColor := ExpandButtonBorderColor;
      Self.GroupBorderColor := GroupBorderColor;
      Self.GroupCaptionColor := GroupCaptionColor;
      Self.GroupCaptionSeparatorColor := GroupCaptionSeparatorColor;
      Self.GroupColor := GroupColor;
      Self.ShadowColor := ShadowColor;
      Self.ShadowDepth := ShadowDepth;
      Self.TreeLineColor := TreeLineColor;
      Self.TreeLineStyle := TreeLineStyle;
      Self.Cells.Assign(Cells);
      if AreHeaderCellsAllocated then
        Self.HeaderCells.Assign(HeaderCells);
      if AreHeaderCornerCellsAllocated then
        Self.HeaderCornerCells.Assign(HeaderCornerCells);
      if AreFooterCellsAllocated then
        Self.FooterCells.Assign(FooterCells);
      if AreRowHeaderCellsAllocated then
        Self.RowHeaderCells.Assign(RowHeaderCells);
      if HasOverlays then
        Self.AssignOverlays(TdxReportCells(Source));
    end
  else
    inherited;
end;

procedure TdxReportCells.ClearItems;
begin
  FCells.ClearCells;
  FCells.ClearDataItems;
end;

procedure TdxReportCells.ClearLookAndFeels;
var
  I: Integer;
begin
  for I := 0 to LookAndFeelCount - 1 do
    LookAndFeels[I].Free;
  FLookAndFeels.Clear;
end;

function TdxReportCells.CreateGroupLookAndFeel(AClass: TdxPSReportGroupLookAndFeelClass;
  ACheckExisting: Boolean = True): TdxPSReportGroupLookAndFeel;
begin
  if ACheckExisting then
    Result := FindGroupLookAndFeelByClass(AClass)
  else
    Result := nil;

  if Result = nil then
  begin
    Result := AClass.Create(Self);
    FLookAndFeels.Add(Result);
  end;
end;

function TdxReportCells.FindGroupLookAndFeelByClass(AClass: TdxPSReportGroupLookAndFeelClass): TdxPSReportGroupLookAndFeel;
var
  I: Integer;
begin
  for I := 0 to LookAndFeelCount - 1 do
  begin
    Result := LookAndFeels[I];
    if Result.ClassType = AClass then Exit;
  end;
  Result := nil;
end;

function TdxReportCells.FindGroupLookAndFeelByData(AData: Pointer): TdxPSReportGroupLookAndFeel;
var
  I: Integer;
begin
  for I := 0 to LookAndFeelCount - 1 do
  begin
    Result := LookAndFeels[I];
    if Result.Data = AData then Exit;
  end;
  Result := nil;
end;

function TdxReportCells.IndexOfReportGroupLookAndFeel(ALookAndFeel: TdxPSReportGroupLookAndFeel): Integer;
begin
  Result := FLookAndFeels.IndexOf(ALookAndFeel);
end;

procedure TdxReportCells.DoProgress(const APercentDone: Double);
begin
  if ReportLink <> nil then ReportLink.DoProgress(APercentDone);
end;

function TdxReportCells.AddOverlay: TdxReportCell;
begin
  Result := TdxReportCell.Create(nil);
  Result.FReportCells := Self;
  if FOverlays = nil then
    FOverlays := TList.Create;
  FOverlays.Add(Result);
end;

procedure TdxReportCells.AppendOverlays(
  Source: TdxReportCells; AnOffsetX: Integer = 0; AnOffsetY: Integer = 0);
var
  AOverlay: TdxReportCell;
  I: Integer;
begin
  for I := 0 to Source.OverlayCount - 1 do
  begin
    AOverlay := AddOverlay;
    AOverlay.Assign(Source.Overlays[I]);
    AOverlay.Offset(AnOffsetX, AnOffsetY);
  end;
end;

procedure TdxReportCells.AssignOverlays(Source: TdxReportCells;
  AnOffsetX: Integer = 0; AnOffsetY: Integer = 0);
begin
  ClearOverlays;
  AppendOverlays(Source, AnOffsetX, AnOffsetY);
end;

procedure TdxReportCells.ClearOverlays;
var
  I: Integer;
begin
  for I := 0 to OverlayCount - 1 do
    Overlays[I].Free;
  if FOverlays <> nil then
    FOverlays.Clear;
end;

procedure TdxReportCells.DeleteOverlay(AnOverlay: TdxReportCell);
var
  Index: Integer;
begin
  Index := IndexOfOverlay(AnOverlay);
  if Index <> -1 then
  begin
    Overlays[Index].Free;
    if OverlayCount = 0 then FreeAndNil(FOverlays);
  end;
end;

procedure TdxReportCells.FreeAndNilOverlays;
begin
  ClearOverlays;
  FreeAndNil(FOverlays);
end;

function TdxReportCells.HasOverlays: Boolean;
begin
  Result := FOverlays <> nil;
end;

function TdxReportCells.IndexOfOverlay(AnOverlay: TdxReportCell): Integer;
begin
  if HasOverlays then
    Result := FOverlays.IndexOf(AnOverlay)
  else
    Result := -1;
end;

procedure TdxReportCells.AssignLookAndFeels(Source: TdxReportCells);

  procedure DoAssignLookAndFeel(Source: TdxPSReportGroupLookAndFeel);
  var
    ADest: TdxPSReportGroupLookAndFeel;
  begin
    if FindGroupLookAndFeelByData(Source.Data) <> nil then
      Exit;
    ADest := CreateGroupLookAndFeel(TdxPSReportGroupLookAndFeelClass(Source.ClassType));
    ADest.Assign(Source);
    ADest.Data := Source.Data;
  end;

var
  I: Integer;
begin
  if Source = Self then
    Exit;
  for I := 0 to Source.LookAndFeelCount - 1 do
    DoAssignLookAndFeel(Source.LookAndFeels[I]);
end;

function TdxReportCells.GetFontByIndex(AnIndex: Integer): TFont;
begin
  Result := ReportLink.FontPool[AnIndex];
end;

function TdxReportCells.GetIndexByFont(AFont: TFont): Integer;
begin
  Result := ReportLink.FontPool.Add(AFont);
end;

procedure TdxReportCells.ReadData(AReader: TdxPSDataReader);
begin
  BeforeReadData(AReader);
  try
    ClearItems;
    try
      ReadLookAndFeels(AReader);
      ReadCells(AReader);
      ReadFooterCells(AReader);
      ReadHeaderCells(AReader);
      ReadOverlayCells(AReader);
      ReadProperties(AReader);
    except
      ClearItems;
      raise;
    end;
  finally
    AfterReadData(AReader);
  end;
end;

procedure TdxReportCells.WriteData(AWriter: TdxPSDataWriter);
begin
  BeforeWriteData(AWriter);
  try
    WriteLookAndFeels(AWriter);
    WriteCells(AWriter);
    WriteFooterCells(AWriter);
    WriteHeaderCells(AWriter);
    WriteOverlayCells(AWriter);
    WriteProperties(AWriter);
  finally
    AfterWriteData(AWriter);
  end;
end;

procedure TdxReportCells.AfterReadData(AReader: TdxPSDataReader);
begin
  CheckStorageCompatibility(AReader);
end;

procedure TdxReportCells.AfterWriteData(AWriter: TdxPSDataWriter);
begin
  ClearImageLists;
end;

procedure TdxReportCells.BeforeReadData(AReader: TdxPSDataReader);
begin
  ClearImageLists;
  OwnImageLists := True;
  ReadImageLists(AReader);
end;

procedure TdxReportCells.BeforeWriteData(AWriter: TdxPSDataWriter);
begin
  ClearImageLists;
  OwnImageLists := False;
  GetImageLists;
  WriteImageLists(AWriter);
end;

procedure TdxReportCells.CheckStorageCompatibility(AReader: TdxPSDataReader);
begin
  TdxPSStorageVersionCompability.UpdateReportCells(Self, AReader);
end;

procedure TdxReportCells.ReadCells(AReader: TdxPSDataReader);
begin
  Cells.ReadData(AReader);
end;

procedure TdxReportCells.ReadFooterCells(AReader: TdxPSDataReader);
begin
  if AReader.ReadBoolean then
    FooterCells.ReadData(AReader);
end;

procedure TdxReportCells.ReadHeaderCells(AReader: TdxPSDataReader);
begin
  if AReader.ReadBoolean then
    HeaderCells.ReadData(AReader);
  if dxPSCheckVersion(AReader.PSVersion, 4, 48) then
  begin
    if AReader.ReadBoolean then
      HeaderCornerCells.ReadData(AReader);
    if AReader.ReadBoolean then
      RowHeaderCells.ReadData(AReader);
  end;
end;

procedure TdxReportCells.ReadImageLists(AReader: TdxPSDataReader);
var
  ImageList: TImageList;
begin
  AReader.ReadListBegin;
  try
    while not AReader.EndOfList do
    begin
      ImageList := TImageList.Create(nil);
      AReader.ReadImageList(ImageList);
      FImageLists.Add(Imagelist);
    end;
  finally
    AReader.ReadListEnd;
  end;
end;

procedure TdxReportCells.ReadLookAndFeels(AReader: TdxPSDataReader);
var
  LookAndFeelClass: TdxPSReportGroupLookAndFeelClass;
  Index: Integer;
begin
  ClearLookAndFeels;

  AReader.ReadListBegin;
  try
    while not AReader.EndOfList do
    begin
      LookAndFeelClass := AReader.ReadLookAndFeelClass;
      CreateGroupLookAndFeel(LookAndFeelClass, False).ReadData(AReader);
    end;
  finally
    AReader.ReadListEnd;
  end;
  Index := AReader.ReadInteger;
  if Index <> -1 then
    LookAndFeel := LookAndFeels[Index]
  else
    LookAndFeel := nil;

  {if LookAndFeelCount = 0 then
  begin
    FLookAndFeel := CreateGroupLookAndFeel(dxDefaultReportGroupLookAndFeel);
    CreateGroupLookAndFeel(TdxPSReportGroupNullLookAndFeel);
  end
  else
    FLookAndFeel := LookAndFeels[0];}
end;

procedure TdxReportCells.ReadOverlayCells(AReader: TdxPSDataReader);
begin
  ClearOverlays;
  AReader.ReadListBegin;
  try
    while not AReader.EndOfList do
      AddOverlay.ReadData(AReader);
  finally
    AReader.ReadListEnd;
  end;
end;

procedure TdxReportCells.ReadProperties(AReader: TdxPSDataReader);
begin
  with AReader do
  begin
    BorderColor := ReadInteger;
    BorderWidth := ReadInteger;
    ExpandButtonBorderColor := ReadInteger;
    GroupBorderColor := ReadInteger;
    GroupCaptionColor := ReadInteger;
    GroupCaptionSeparatorColor := ReadInteger;
    GroupColor := ReadInteger;
    ShadowColor := ReadInteger;
    ShadowDepth := ReadInteger;
    TreeLineColor := ReadInteger;
    TreeLineStyle := TdxPSTreeLineStyle(ReadInteger);
  end;
end;

procedure TdxReportCells.WriteCells(AWriter: TdxPSDataWriter);
begin
  Cells.WriteData(AWriter);
end;

procedure TdxReportCells.WriteFooterCells(AWriter: TdxPSDataWriter);
begin
  AWriter.WriteBoolean(AreFooterCellsAllocated);
  if AreFooterCellsAllocated then
    FooterCells.WriteData(AWriter);
end;

procedure TdxReportCells.WriteHeaderCells(AWriter: TdxPSDataWriter);
begin
  AWriter.WriteBoolean(AreHeaderCellsAllocated);
  if AreHeaderCellsAllocated then
    HeaderCells.WriteData(AWriter);
  if dxPSCheckVersion(AWriter.PSVersion, 4, 48) then
  begin
    AWriter.WriteBoolean(AreHeaderCornerCellsAllocated);
    if AreHeaderCornerCellsAllocated then
      HeaderCornerCells.WriteData(AWriter);
    AWriter.WriteBoolean(AreRowHeaderCellsAllocated);
    if AreRowHeaderCellsAllocated then
      RowHeaderCells.WriteData(AWriter);
  end;
end;

procedure TdxReportCells.WriteImageLists(AWriter: TdxPSDataWriter);
var
  I: Integer;
begin
  AWriter.WriteListBegin;
  try
    for I := 0 to FImageLists.Count - 1 do
      AWriter.WriteImageList(TCustomImageList(FImageLists[I]));
  finally
    AWriter.WriteListEnd;
  end;
end;

procedure TdxReportCells.WriteLookAndFeels(AWriter: TdxPSDataWriter);
var
  I: Integer;
  LookAndFeel: TdxPSReportGroupLookAndFeel;
begin
  AWriter.WriteListBegin;
  try
    for I := 0 to LookAndFeelCount - 1 do
    begin
      LookAndFeel := LookAndFeels[I];
      AWriter.WriteClassName(LookAndFeel);
      LookAndFeel.WriteData(AWriter);
    end;
  finally
    AWriter.WriteListEnd;
  end;

  AWriter.WriteInteger(IndexOfReportGroupLookAndFeel(Self.LookAndFeel));
end;

procedure TdxReportCells.WriteOverlayCells(AWriter: TdxPSDataWriter);
var
  I: Integer;
begin
  AWriter.WriteListBegin;
  try
    for I := 0 to OverlayCount - 1 do
      Overlays[I].WriteData(AWriter);
  finally
    AWriter.WriteListEnd;
  end;
end;

procedure TdxReportCells.WriteProperties(AWriter: TdxPSDataWriter);
begin
  with AWriter do
  begin
    WriteInteger(BorderColor);
    WriteInteger(BorderWidth);
    WriteInteger(ExpandButtonBorderColor);
    WriteInteger(GroupBorderColor);
    WriteInteger(GroupCaptionColor);
    WriteInteger(GroupCaptionSeparatorColor);
    WriteInteger(GroupColor);
    WriteInteger(ShadowColor);
    WriteInteger(ShadowDepth);
    WriteInteger(TreeLineColor);
    WriteInteger(Integer(TreeLineStyle));
  end;
end;

function TdxReportCells.CalculateOverlaysHeight: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to OverlayCount - 1 do
    Result := Max(Result, Overlays[I].GetAbsoluteRect.Bottom);
end;

function TdxReportCells.CalculateOverlaysWidth: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to OverlayCount - 1 do
    Result := Max(Result, Overlays[I].GetAbsoluteRect.Right);
end;

function TdxReportCells.CalculateTotalHeight: Integer;
begin
  Result := Max(Cells.Height, CalculateOverlaysHeight);
end;

function TdxReportCells.CalculateTotalWidth: Integer;
begin
  Result := Max(Cells.Width, CalculateOverlaysWidth);
end;

procedure TdxReportCells.ConvertCoords(APixelsNumerator, APixelsDenominator: Integer);
var
  I: Integer;
begin
  Cells.ConvertCoords(APixelsNumerator, APixelsDenominator);
  ShadowDepth := MulDiv(ShadowDepth, APixelsNumerator, APixelsDenominator);
  for I := 0 to LookAndFeelCount - 1 do
    LookAndFeels[I].ConvertCoords(APixelsNumerator, APixelsDenominator);
  if AreFooterCellsAllocated then
    FFooterCells.ConvertCoords(APixelsNumerator, APixelsDenominator);
  if AreHeaderCornerCellsAllocated then
    HeaderCornerCells.ConvertCoords(APixelsNumerator, APixelsDenominator);
  if AreHeaderCellsAllocated then
    HeaderCells.ConvertCoords(APixelsNumerator, APixelsDenominator);
  if AreRowHeaderCellsAllocated then
    RowHeaderCells.ConvertCoords(APixelsNumerator, APixelsDenominator);
  if HasOverlays then
    for I := 0 to OverlayCount - 1 do
      Overlays[I].ConvertCoords(APixelsNumerator, APixelsDenominator);
end;

function TdxReportCells.GetCellTopLevelParent(AnItem: TdxReportItem): TdxReportCell;

  function IsParentInTopLevels(AnItem: TdxReportItem): Boolean;
  var
    I: Integer;
  begin
    Result := (AnItem = Cells) or
      (AreHeaderCellsAllocated and (AnItem = HeaderCells)) or
      (AreFooterCellsAllocated and (AnItem = FooterCells)) or
      (AreRowHeaderCellsAllocated and (AnItem = RowHeaderCells));

    if not Result then
      for I := 0 to OverlayCount - 1 do
      begin
        Result := AnItem = Overlays[I];
        if Result then Exit;
      end;
  end;

begin
  Result := AnItem.Parent;
  if Result <> nil then
    while (Result.Parent <> nil) and not IsParentInTopLevels(Result.Parent) do
      Result := Result.Parent;
end;

procedure TdxReportCells.FreeAndNilReportGroupLookAndFeels;
begin
  ClearLookAndFeels;
  FreeAndNil(FLookAndFeels);
end;

procedure TdxReportCells.PrepareReportGroupsLookAndFeels(
  ACanvas: TdxPSReportRenderCustomCanvas);
var
  I: Integer;
begin
  for I := 0 to LookAndFeelCount - 1 do
    LookAndFeels[I].Prepare(ACanvas);
end;

procedure TdxReportCells.AddImageList(AImageList: TCustomImageList);
begin
  if (AImageList <> nil) and (FImageLists.IndexOf(AImageList) = -1) then
    FImageLists.Add(AImageList);
end;

procedure TdxReportCells.ClearImageLists;
var
  I: Integer;
begin
  if OwnImageLists then
    for I := 0 to ImageListCount - 1 do
      ImageLists[I].Free;
  FImageLists.Clear;
end;

procedure TdxReportCells.FreeAndNilImageLists;
begin
  ClearImageLists;
  FreeAndNil(FImageLists);
end;

procedure TdxReportCells.GetImageLists;
begin
  ReportLink.GetImageLists(AddImageList);
end;

function TdxReportCells.IndexOfImageList(AnImageList: TCustomImageList): Integer;
begin
  Result := FImageLists.IndexOf(AnImageList);
end;

function TdxReportCells.CreateFixedGroupCell: TdxReportCell;
begin
  Result := TdxReportCell.Create(nil);
  Result.FReportCells := Self;
  Result.Color := dxDefaultFixedColor;
end;

function TdxReportCells.GetAreFooterCellsAllocated: Boolean;
begin
  Result := FFooterCells <> nil;
end;

function TdxReportCells.GetAreHeaderCellsAllocated: Boolean;
begin
  Result := FHeaderCells <> nil;
end;

function TdxReportCells.GetAreHeaderCornerCellsAllocated: Boolean;
begin
  Result := FHeaderCornerCells <> nil;
end;

function TdxReportCells.GetAreRowHeaderCellsAllocated: Boolean;
begin
  Result := FRowHeaderCells <> nil;
end;

function TdxReportCells.GetBoundsRect: TRect;
begin
  Result := Cells.BoundsRect;
end;

function TdxReportCells.GetCount: Integer;
begin
  Result := Cells.CellCount;
end;

function TdxReportCells.GetFooterCells: TdxReportCell;
begin
  if not AreFooterCellsAllocated then
    FFooterCells := CreateFixedGroupCell;
  Result := FFooterCells;
end;

function TdxReportCells.GetHeaderBoundsRect: TRect;
begin
  if AreHeaderCellsAllocated then
    Result := FHeaderCells.BoundsRect
  else
    Result := cxNullRect;
end;

function TdxReportCells.GetHeaderCellCount: Integer;
begin
  if AreHeaderCellsAllocated then
    Result := FHeaderCells.CellCount
  else
    Result := 0;
end;

function TdxReportCells.GetHeaderCells: TdxReportCell;
begin
  if not AreHeaderCellsAllocated then
    FHeaderCells := CreateFixedGroupCell;
  Result := FHeaderCells;
end;

function TdxReportCells.GetHeaderCornerBoundsRect: TRect;
begin
  if AreHeaderCornerCellsAllocated then
    Result := FHeaderCornerCells.BoundsRect
  else
    Result := cxNullRect;
end;

function TdxReportCells.GetHeaderCornerCells: TdxReportCell;
begin
  if not AreHeaderCornerCellsAllocated then
    FHeaderCornerCells := CreateFixedGroupCell;
  Result := FHeaderCornerCells;
end;

function TdxReportCells.GetImageList(Index: Integer): TCustomImageList;
begin
  if Index = -1 then
    Result := nil
  else
    Result := TCustomImageList(FImageLists[Index]);
end;

function TdxReportCells.GetImageListCount: Integer;
begin
  if FImageLists <> nil then
    Result := FImageLists.Count
  else
    Result := 0;
end;

function TdxReportCells.GetLookAndFeel(Index: Integer): TdxPSReportGroupLookAndFeel;
begin
  Result := TdxPSReportGroupLookAndFeel(FLookAndFeels[Index]);
end;

function TdxReportCells.GetLookAndFeelCount: Integer;
begin
  Result := FLookAndFeels.Count;
end;

function TdxReportCells.GetOverlay(Index: Integer): TdxReportCell;
begin
  if HasOverlays then
    Result := TdxReportCell(FOverlays[Index])
  else
    Result := nil;
end;

function TdxReportCells.GetOverlayCount: Integer;
begin
  if HasOverlays then
    Result := FOverlays.Count
  else
    Result := 0;
end;

function TdxReportCells.GetRenderer: TdxPSReportRenderer;
begin
  Result := ReportLink.Renderer;
end;

function TdxReportCells.GetRowHeaderBoundsRect: TRect;
begin
  if AreRowHeaderCellsAllocated then
    Result := FRowHeaderCells.BoundsRect
  else
    Result := cxNullRect;
end;

function TdxReportCells.GetRowHeaderCellCount: Integer;
begin
  if AreRowHeaderCellsAllocated then
    Result := FRowHeaderCells.CellCount
  else
    Result := 0;
end;

function TdxReportCells.GetRowHeaderCells: TdxReportCell;
begin
  if not AreRowHeaderCellsAllocated then
    FRowHeaderCells := CreateFixedGroupCell;
  Result := FRowHeaderCells;
end;

procedure TdxReportCells.SetBorderColor(Value: TColor);
begin
  if Value = clDefault then
    FBorderColor := dxDefaultGridLineColor
  else
    FBorderColor := ColorToRGB(Value);
end;

procedure TdxReportCells.SetShadowColor(Value: TColor);
begin
  if Value = clDefault then
    FShadowColor := dxDefaultShadowColor
  else
    FShadowColor := ColorToRGB(Value);
end;

procedure TdxReportCells.SetShadowDepth(Value: Integer);
begin
  FShadowDepth := Max(0, Value);
end;

procedure TdxReportCells.SetTreeLineColor(Value: TColor);
begin
  if Value = clDefault then
    FTreeLineColor := dxDefaultTreeLineColor
  else
    FTreeLineColor := ColorToRGB(Value);
end;

function TdxReportCells.GetFont: TFont;
begin
  Result := Cells.Font;
end;

function TdxReportCells.GetFooterBoundsRect: TRect;
begin
  if AreFooterCellsAllocated then
    Result := FFooterCells.BoundsRect
  else
    Result := cxNullRect;
end;

function TdxReportCells.GetFooterCellCount: Integer;
begin
  if AreFooterCellsAllocated then
    Result := FFooterCells.CellCount
  else
    Result := 0;
end;

{ TAbstractdxReportCellData }

constructor TAbstractdxReportCellData.Create(AParent: TdxReportCell);
begin
  inherited Create(AParent);
  BreakByChars := dxDefaultBreakByChars;                   // True
  EndEllipsis := dxDefaultEndEllipsis;                     // False
  HidePrefix := dxDefaultHidePrefix;                       // False
  Multiline := dxDefaultMultiline;                         // False
  PreventLeftTextExceed := dxDefaultPreventLeftTextExceed; // True
  PreventTopTextExceed := dxDefaultPreventTopTextExceed;   // True
  SortOrder := dxDefaultSortOrder;                         // csoNone
  TextAlignX := dxDefaultTextAlignX;                       // taLeft
  TextAlignY := dxDefaultTextAlignY;                       // taCenterY
end;

procedure TAbstractdxReportCellData.DrawContent(
  ACanvas: TdxPSReportRenderCustomCanvas; AStage: TdxPSRenderStages);
begin
  if rsFirstPass in AStage then
  begin
    DrawBackground(ACanvas);
    if IsBordersDrawn then
      DrawBorders(ACanvas);
  end;
end;

function TAbstractdxReportCellData.GetCustomDrawID: Integer;
begin
  Result := Data;
end;

function TAbstractdxReportCellData.MeasureContentHeight(ACanvas: TdxPSReportRenderCustomCanvas): Integer;
begin
  Result := Height;
end;

function TAbstractdxReportCellData.MeasureContentWidth(ACanvas: TdxPSReportRenderCustomCanvas): Integer;
begin
  Result := Width;
end;

function TAbstractdxReportCellData.CustomDraw(ACanvas: TdxPSReportRenderCustomCanvas): Boolean;
begin
  Result := IsCustomDrawn and Renderer.CustomDrawReportItem(Self);
end;

function TAbstractdxReportCellData.GetAbsoluteEffectiveBounds(
  ACanvas: TdxPSReportRenderCustomCanvas; AStage: TdxPSRenderStages): TRect;
begin
  Result := GetEffectiveBounds(ACanvas, AStage);
  if Parent <> nil then
  begin
    with Parent.AbsoluteOrigin do
      OffsetRect(Result, X, Y);
  end;
end;

function TAbstractdxReportCellData.GetDefaultDTFormat: DWORD;
begin
  Result := CXTO_EXPANDTABS or CXTO_PATTERNEDTEXT or CXTO_CHARBREAK or CXTO_EDITCONTROL;
end;

function TAbstractdxReportCellData.GetEffectiveBounds(
  ACanvas: TdxPSReportRenderCustomCanvas; AStage: TdxPSRenderStages): TRect;
begin
  Result := GetOuterBounds(ACanvas);
end;

function TAbstractdxReportCellData.GetDTFormat: DWORD;
const
  dxEndEllipsis: array[Boolean] of UINT = (0, CXTO_END_ELLIPSIS);
  dxHidePrefix: array[Boolean] of UINT = (0, CXTO_HIDEPREFIX);
  dxNoClip: array[Boolean] of UINT = (0, CXTO_NOCLIP);
  dxPreventAutoIndents: array[Boolean] of UINT = (CXTO_AUTOINDENTS, 0);
  dxPreventLeftExceed: array[Boolean] of UINT = (0, CXTO_PREVENT_LEFT_EXCEED);
  dxPreventTopExceed: array[Boolean] of UINT = (0, CXTO_PREVENT_TOP_EXCEED);
  dxWordBreak: array[Boolean] of UINT = (0, CXTO_WORDBREAK);
begin
  Result := DefaultDTFormat or
    dxPreventAutoIndents[PreventAutoIndents] or
    cxMakeFormat(TextAlignX, TextAlignY) or
    dxEndEllipsis[EndEllipsis] or
    dxHidePrefix[HidePrefix] or
    dxPreventLeftExceed[PreventLeftTextExceed] or
    dxPreventTopExceed[PreventTopTextExceed] or
    dxWordBreak[Multiline] or
    dxNoClip[NoClip];
end;

function TAbstractdxReportCellData.HasPreventAutoIndentsProperty(const AVersion: TdxPSVersion): Boolean;
begin
  Result := dxPSCheckVersion(AVersion, 4, 20110104);
end;

procedure TAbstractdxReportCellData.ReadData(AReader: TdxPSDataReader);
begin
  inherited ReadData(AReader);
  if HasPreventAutoIndentsProperty(AReader.PSVersion) then
    PreventAutoIndents := AReader.ReadBoolean;
end;

procedure TAbstractdxReportCellData.WriteData(AWriter: TdxPSDataWriter);
begin
  inherited WriteData(AWriter);
  if HasPreventAutoIndentsProperty(AWriter.PSVersion) then
    AWriter.WriteBoolean(PreventAutoIndents);
end;

function TAbstractdxReportCellData.IsCustomDrawn: Boolean;
begin
  Result := Assigned(Creator) and Creator.PossibleCustomDraw(Self);
end;

function TAbstractdxReportCellData.IsDrawingNeeded(
  ACanvas: TdxPSReportRenderCustomCanvas; AStage: TdxPSRenderStages;
  const ARect: TRect): Boolean;
begin
  Result := Visible and IsDrawn(ACanvas, AStage, ARect) and
    ACanvas.IsRectVisible(GetEffectiveBounds(ACanvas, AStage));
end;

function TAbstractdxReportCellData.IsDrawn(ACanvas: TdxPSReportRenderCustomCanvas;
  AStage: TdxPSRenderStages; const ARect: TRect): Boolean;
var
  R: TRect;
begin
  Result := IntersectRect(R, GetAbsoluteEffectiveBounds(ACanvas, AStage), ARect);
end;

function TAbstractdxReportCellData.GetSortOrder: TdxCellSortOrder;
begin
  Result := TdxCellSortOrder((Format and dxPSGlbl.dxFormatSortOrderMask) shr
    dxPSGlbl.dxFormatSortOrderOffset);
end;

function TAbstractdxReportCellData.GetBreakByChars: Boolean;
begin
  Result := GetFormatBit(dxPSGlbl.dxFormatBreakByChars);
end;

function TAbstractdxReportCellData.GetEndEllipsis: Boolean;
begin
  Result := GetFormatBit(dxPSGlbl.dxFormatEndEllipsis);
end;

function TAbstractdxReportCellData.GetHidePrefix: Boolean;
begin
  Result := GetFormatBit(dxPSGlbl.dxFormatHidePrefix);
end;

function TAbstractdxReportCellData.GetMultiline: Boolean;
begin
  Result := GetFormatBit(dxPSGlbl.dxFormatMultiline);
end;

function TAbstractdxReportCellData.GetPreventLeftTextExceed: Boolean;
begin
  Result := GetFormatBit(dxPSGlbl.dxFormatPreventLeftTextExceed);
end;

function TAbstractdxReportCellData.GetPreventTopTextExceed: Boolean;
begin
  Result := GetFormatBit(dxPSGlbl.dxFormatPreventTopTextExceed);
end;

function TAbstractdxReportCellData.GetTextAlignX: TcxTextAlignX;
begin
  Result := TcxTextAlignX((Format and dxPSGlbl.dxFormatTextAlignXMask) shr dxPSGlbl.dxFormatTextAlignXOffset);
end;

function TAbstractdxReportCellData.GetTextAlignY: TcxTextAlignY;
begin
  Result := TcxTextAlignY((Format and dxPSGlbl.dxFormatTextAlignYMask) shr dxPSGlbl.dxFormatTextAlignYOffset);
end;

procedure TAbstractdxReportCellData.SetBreakByChars(Value: Boolean);
begin
  SetFormatBit(dxPSGlbl.dxFormatBreakByChars, Value);
end;

procedure TAbstractdxReportCellData.SetEndEllipsis(Value: Boolean);
begin
  SetFormatBit(dxPSGlbl.dxFormatEndEllipsis, Value);
end;

procedure TAbstractdxReportCellData.SetHidePrefix(Value: Boolean);
begin
  SetFormatBit(dxPSGlbl.dxFormatHidePrefix, Value);
end;

procedure TAbstractdxReportCellData.SetMultiline(Value: Boolean);
begin
  SetFormatBit(dxPSGlbl.dxFormatMultiline, Value);
end;

procedure TAbstractdxReportCellData.SetPreventLeftTextExceed(Value: Boolean);
begin
  SetFormatBit(dxPSGlbl.dxFormatPreventLeftTextExceed, Value);
end;

procedure TAbstractdxReportCellData.SetPreventTopTextExceed(Value: Boolean);
begin
  SetFormatBit(dxPSGlbl.dxFormatPreventTopTextExceed, Value);
end;

procedure TAbstractdxReportCellData.SetSortOrder(Value: TdxCellSortOrder);
begin
  Format := Format and not dxPSGlbl.dxFormatSortOrderMask or (Byte(Value) shl dxPSGlbl.dxFormatSortOrderOffset);
end;

procedure TAbstractdxReportCellData.SetTextAlignX(Value: TcxTextAlignX);
begin
  Format := Format and not dxPSGlbl.dxFormatTextAlignXMask or (Byte(Value) shl dxPSGlbl.dxFormatTextAlignXOffset);
end;

procedure TAbstractdxReportCellData.SetTextAlignY(Value: TcxTextAlignY);
begin
  Format := Format and not dxPSGlbl.dxFormatTextAlignYMask or (Byte(Value) shl dxPSGlbl.dxFormatTextAlignYOffset);
end;

{ TdxReportCellText }

constructor TdxReportCellText.Create(AParent: TdxReportCell);
begin
  inherited Create(AParent);
  FIndents := TcxRect.Create(Self);
  FSortMarkRegionSize := dxSortMarkRgnSize;
end;

destructor TdxReportCellText.Destroy;
begin
  FreeAndNil(FIndents);
  inherited Destroy;
end;

procedure TdxReportCellText.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TdxReportCellText then
  begin
    Text := TdxReportCellText(Source).Text;
    Indents.Assign(TdxReportCellText(Source).Indents);
    AdjustFont := TdxReportCellText(Source).AdjustFont;
  end;
end;

procedure TdxReportCellText.DrawContent(ACanvas: TdxPSReportRenderCustomCanvas; AStage: TdxPSRenderStages);
begin
  DrawBackground(ACanvas);
  if IsTextDrawn then
    DrawText(ACanvas);
  if IsSortMarkDrawn then
    DrawSortMark(ACanvas);
  if IsBordersDrawn then
    DrawBorders(ACanvas);
end;

procedure TdxReportCellText.DrawSortMark(ACanvas: TdxPSReportRenderCustomCanvas);
var
  R: TRect;
begin
  R := GetSortMarkBounds(ACanvas);
  Renderer.DrawSortMark(ACanvas, R, SortOrder, EdgeMode = cemPattern);
end;

procedure TdxReportCellText.DrawText(ACanvas: TdxPSReportRenderCustomCanvas);
var
  AHelper: TdxPSAdjustFontSizeHelper;
  ARect: TRect;
  AText: string;
  ATextParams: TcxTextParams;
begin
  ARect := GetTextBounds(ACanvas);
  if not IsRectEmpty(ARect) then
  begin
    AText := GetText;
    if AdjustFont then
    begin
      ACanvas.SaveState;
      try
        ACanvas.Font := Font;
        ATextParams := ACanvas.CalculateTextParams(DTFormat);
        if ATextParams.PatternedText then
          Renderer.DrawTextEx(ACanvas, ARect, 0, AText, Font, DTFormat, Indents.Rect)
        else
        begin
          AHelper := TdxPSAdjustFontSizeHelper.Create(ACanvas);
          try
            AHelper.Font.Assign(Font);
            AHelper.Calculate(cxSize(cxPrepareRect(ARect, ATextParams, 0, 0)), AText, Multiline, DTFormat);
            Renderer.DrawTextEx(ACanvas, ARect, 0, AText, AHelper.Font, DTFormat, Indents.Rect, AHelper.LineSpacing);
          finally
            AHelper.Free;
          end;
        end;
      finally
        ACanvas.RestoreState;
      end;
    end
    else
      Renderer.DrawTextEx(ACanvas, ARect, 0, AText, Font, DTFormat, Indents.Rect);
  end;
end;

function TdxReportCellText.CalculateLineCount(ACanvas: TdxPSReportRenderCustomCanvas): Integer;
begin
  Result := Renderer.CalcTextLineCount(ACanvas, Text, Font, Width - Indents.Left - Indents.Right);
end;

function TdxReportCellText.MeasureContentHeight(ACanvas: TdxPSReportRenderCustomCanvas): Integer;
begin
  Result := Max(SortMarkRegionSize, cxMarginsHeight(Indents.Rect) + MeasureTextHeight(ACanvas));
  if Result > 0 then
  begin
    if csTop in CellSides then
      Inc(Result, LineThickness * BorderEdgeThicknesses[csTop]);
    if csBottom in CellSides then
      Inc(Result, LineThickness * BorderEdgeThicknesses[csBottom]);
  end;
end;

function TdxReportCellText.MeasureContentWidth(ACanvas: TdxPSReportRenderCustomCanvas): Integer;
begin
  Result := MeasureTextWidth(ACanvas);
  Inc(Result, SortMarkRegionSize + cxMarginsWidth(Indents.Rect));
  if csLeft in CellSides then
    Inc(Result, LineThickness * BorderEdgeThicknesses[csLeft]);
  if csRight in CellSides then
    Inc(Result, LineThickness * BorderEdgeThicknesses[csRight]);
end;

function TdxReportCellText.MeasureFontHeight(ACanvas: TdxPSReportRenderCustomCanvas): Integer;
begin
  Result := Max(Renderer.CalcTextPatternHeight(ACanvas, Font), SortMarkRegionSize);
  if Result <> 0 then
    Inc(Result, MeasureBordersHeight(ACanvas));
  Inc(Result, cxMarginsHeight(Indents.Rect));
end;

function TdxReportCellText.MeasureTextHeight(ACanvas: TdxPSReportRenderCustomCanvas): Integer;
var
  R: TRect;
begin
  R := Rect(0, 0, cxRectWidth(GetTextBounds(ACanvas)) - cxMarginsWidth(Indents.Rect), 5);
  Result := Renderer.CalcTextRect(ACanvas, Text, R, GetDTFormat or CXTO_CALCROWCOUNT or CXTO_AUTOINDENTS, Font)
end;

function TdxReportCellText.MeasureTextWidth(ACanvas: TdxPSReportRenderCustomCanvas): Integer;
begin
  if Multiline then
    Result := Width
  else
    Result := Renderer.CalcTextWidth(ACanvas, Text, Font);
end;

procedure TdxReportCellText.ConvertCoords(APixelsNumerator, APixelsDenominator: Integer);
begin
  inherited ConvertCoords(APixelsNumerator, APixelsDenominator);
  FSortMarkRegionSize := MulDiv(FSortMarkRegionSize, APixelsNumerator, APixelsDenominator);
  FIndents.Rect := cxRectScale(FIndents.Rect, APixelsNumerator, APixelsDenominator);
end;

function TdxReportCellText.GetSortMarkBounds(ACanvas: TdxPSReportRenderCustomCanvas): TRect;
begin
  Result := GetInnerBounds(ACanvas);
  Result.Left := Result.Right - SortMarkRegionSize;
end;

function TdxReportCellText.GetTextBounds(ACanvas: TdxPSReportRenderCustomCanvas): TRect;
begin
  Result := GetInnerBounds(ACanvas);
  if IsSortMarkDrawn then
    Result.Right := GetSortMarkBounds(ACanvas).Left;
end;

function TdxReportCellText.IsSortMarkDrawn: Boolean;
begin
  Result := SortOrder <> csoNone;
end;

function TdxReportCellText.IsTextDrawn: Boolean;
begin
  Result := GetText <> '';
end;

procedure TdxReportCellText.ReadData(AReader: TdxPSDataReader);
begin
  inherited ReadData(AReader);
  Indents.Rect := cxNullRect;
  Indents.Left := AReader.ReadInteger;
  FAdjustFont := Indents.Left and dxFormatTextAdjustFont <> 0;
  Indents.Left := Indents.Left and not dxFormatTextAdjustFont;
  Text := AReader.ReadString;
  if dxPSCheckVersion(AReader.PSVersion, 4, 48) then
    Indents.Rect := AReader.ReadRect;
end;

procedure TdxReportCellText.WriteData(AWriter: TdxPSDataWriter);
begin
  inherited WriteData(AWriter);
  AWriter.WriteInteger(Ord(AdjustFont) * dxFormatTextAdjustFont);
  AWriter.WriteString(Text);
  if dxPSCheckVersion(AWriter.PSVersion, 4, 48) then
    AWriter.WriteRect(Indents.Rect);
end;

function TdxReportCellText.GetSortMarkRegionSize: Integer;
begin
  if IsSortMarkDrawn then
    Result := FSortMarkRegionSize
  else
    Result := 0;
end;

procedure TdxReportCellText.SetIndents(AValue: TcxRect);
begin
  FIndents.Assign(AValue);
end;

{ TdxReportCellString }

function TdxReportCellString.GetText: string;
begin
  Result := FText;
end;

procedure TdxReportCellString.SetText(const Value: string);
begin
  FText := Value;
end;

{ TdxReportCellImageContainer }

constructor TdxReportCellImageContainer.Create(AParent: TdxReportCell);
begin
  inherited Create(AParent);
  ImageTransparent := True;
end;

procedure TdxReportCellImageContainer.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TdxReportCellImageContainer then
    ImageTransparent := TdxReportCellImageContainer(Source).ImageTransparent;
end;

procedure TdxReportCellImageContainer.DrawContent(
  ACanvas: TdxPSReportRenderCustomCanvas; AStage: TdxPSRenderStages);
begin
  if IsBackgroundBitmapDrawn then
    DrawBackgroundBitmap(ACanvas, GetBackgroundBounds(ACanvas));
  if IsTextBackgroundDrawn then
    DrawTextBackground(ACanvas);
  if IsTextDrawn then
    DrawText(ACanvas);
  if IsImageBackgroundDrawn then
    DrawImageBackground(ACanvas);
  if IsImageDrawn then
    DrawImage(ACanvas);
  if IsSortMarkDrawn then
    DrawSortMark(ACanvas);
  if IsBordersDrawn then
    DrawBorders(ACanvas);
end;

procedure TdxReportCellImageContainer.DrawImage(ACanvas: TdxPSReportRenderCustomCanvas);
begin
end;

procedure TdxReportCellImageContainer.DrawImageBackground(
  ACanvas: TdxPSReportRenderCustomCanvas);
begin
  DrawBackgroundRect(ACanvas, GetImageAreaBounds(ACanvas));
end;

procedure TdxReportCellImageContainer.DrawTextBackground(
  ACanvas: TdxPSReportRenderCustomCanvas);
var
  R: TRect;
begin
  R := GetTextBounds(ACanvas);
  if IsSortMarkDrawn then
    R.Right := GetSortMarkBounds(ACanvas).Right;
  DrawBackgroundRect(ACanvas, R);
end;

function TdxReportCellImageContainer.GetImageAreaBounds(
  ACanvas: TdxPSReportRenderCustomCanvas): TRect;
begin
  Result := GetInnerBounds(ACanvas);
  if IsSortMarkDrawn then
    Result.Right := GetSortMarkBounds(ACanvas).Left;
end;

function TdxReportCellImageContainer.GetImageBounds(
  ACanvas: TdxPSReportRenderCustomCanvas): TRect;
begin
  Result := cxNullRect;
end;

procedure TdxReportCellImageContainer.GetImageSizes(var AImageWidth, AImageHeight: Integer);
begin
  AImageWidth := 0;
  AImageHeight := 0;
end;

function TdxReportCellImageContainer.HasImage: Boolean;
begin
  Result := False;
end;

function TdxReportCellImageContainer.IsImageBackgroundDrawn: Boolean;
begin
  Result := inherited IsBackgroundDrawn and not IsImageDrawn and not IsBackgroundBitmapDrawn;
end;

function TdxReportCellImageContainer.IsImageDrawn: Boolean;
begin
  Result := HasImage;
end;

function TdxReportCellImageContainer.IsTextBackgroundDrawn: Boolean;
begin
  Result := not Transparent and not IsBackgroundBitmapDrawn;
end;

function TdxReportCellImageContainer.GetImageTransparent: Boolean;
begin
  Result := (Format and dxPSGlbl.dxFormatImageTransparent) = dxPSGlbl.dxFormatImageTransparent;
end;

procedure TdxReportCellImageContainer.SetImageTransparent(Value: Boolean);
const
  dxImageTransparent: array[Boolean] of DWORD = (0, dxPSGlbl.dxFormatImageTransparent);
begin
  Format := Format and not dxPSGlbl.dxFormatImageTransparent or dxImageTransparent[Value];
end;

{ TdxReportCellCheck }

constructor TdxCustomReportCellCheck.Create(AParent: TdxReportCell);
begin
  inherited Create(AParent);
  FCheckSize := cxSize(dxPSGlbl.CheckWidth, dxPSGlbl.CheckHeight);
  Checked := False;
  //BorderColor := clWindowText;
  ButtonEdgeStyle := cbesUltraFlat;
  CheckPos := dxDefaultCheckPos;
  Enabled := True;
end;

procedure TdxCustomReportCellCheck.DrawCheck(ACanvas: TdxPSReportRenderCustomCanvas);
var
  R: TRect;
begin
  R := GetCheckBounds(ACanvas);
  Renderer.DrawCheckBox(ACanvas, R, Checked, Enabled, IsRadio, ButtonEdgeStyle, BorderColor);
end;

procedure TdxCustomReportCellCheck.DrawImage(ACanvas: TdxPSReportRenderCustomCanvas);
begin
  DrawCheck(ACanvas);
end;

function TdxCustomReportCellCheck.MeasureContentHeight(ACanvas: TdxPSReportRenderCustomCanvas): Integer;
begin
  Result := Max(1 + CheckHeight + 1, inherited MeasureContentHeight(ACanvas));
end;

function TdxCustomReportCellCheck.MeasureContentWidth(ACanvas: TdxPSReportRenderCustomCanvas): Integer;
begin
  Result := 1 + CheckWidth + 1;
  if CheckPos <> ccpCenter then
    Inc(Result, inherited MeasureContentWidth(ACanvas));
end;

procedure TdxCustomReportCellCheck.ConvertCoords(APixelsNumerator, APixelsDenominator: Integer);
begin
  inherited;
  FCheckSize := cxSizeScale(FCheckSize, APixelsNumerator, APixelsDenominator);
end;

function TdxCustomReportCellCheck.GetCheckBounds(ACanvas: TdxPSReportRenderCustomCanvas): TRect;
var
  W, H: Integer;
begin
  Result := GetImageAreaBounds(ACanvas);
  if not IsRectEmpty(Result) then
  begin
    GetImageSizes(W, H);
    with Result do
    begin
      Inc(Left, (Right - Left - W) div 2);
      Inc(Top, (Bottom - Top - H) div 2);
      Right := Left + W;
      Bottom := Top + H;
    end;
    FixupRect(ACanvas, Result);
  end;
end;

function TdxCustomReportCellCheck.GetImageAreaBounds(ACanvas: TdxPSReportRenderCustomCanvas): TRect;
var
  W, H: Integer;
begin
  GetImageSizes(W, H);
  if (W <> 0) and (H <> 0) then
  begin
    Result := inherited GetImageAreaBounds(ACanvas);
    case CheckPos of
      ccpLeft:
        Result.Right := Result.Left + (W + 2 * Renderer.LineThickness);
      ccpRight:
        Result.Left := Result.Right - (W + 2 * Renderer.LineThickness);
    end;
  end
  else
    Result := cxNullRect;
end;

function TdxCustomReportCellCheck.GetImageBounds(ACanvas: TdxPSReportRenderCustomCanvas): TRect;
begin
  Result := GetCheckBounds(ACanvas);
end;

procedure TdxCustomReportCellCheck.GetImageSizes(var AImageWidth, AImageHeight: Integer);
begin
  AImageWidth := FCheckSize.cx;
  AImageHeight := FCheckSize.cy;
end;

function TdxCustomReportCellCheck.GetTextBounds(ACanvas: TdxPSReportRenderCustomCanvas): TRect;
begin
  if CheckPos <> ccpCenter then
  begin
    Result := inherited GetTextBounds(ACanvas);
    case CheckPos of
      ccpLeft:
        Result.Left := GetImageAreaBounds(ACanvas).Right;
      ccpRight:
        Result.Right := GetImageAreaBounds(ACanvas).Left;
    end;
  end
  else
    Result := cxNullRect;
end;

function TdxCustomReportCellCheck.HasImage: Boolean;
begin
  Result := True;
end;

function TdxCustomReportCellCheck.IsImageBackgroundDrawn: Boolean;
begin
  Result := not Transparent and not IsBackgroundBitmapDrawn;
end;

class function TdxCustomReportCellCheck.IsRadio: Boolean;
begin
  Result := False;
end;

procedure TdxCustomReportCellCheck.SetChecked(Value: Boolean);
begin
  SetFormatBit(dxPSGlbl.dxFormatCheckChecked, Value);
end;

function TdxCustomReportCellCheck.GetState: TCheckBoxState;
begin
  if Enabled then
    if Checked then
      Result := cbChecked
    else
      Result := cbUnchecked
  else
    Result := cbGrayed;
end;

function TdxCustomReportCellCheck.GetBoldBorder: Boolean;
begin
  Result := ButtonEdgeStyle = cbesBoldFlat;
end;

function TdxCustomReportCellCheck.GetButtonEdgeStyle: TdxCheckButtonEdgeStyle;
begin
  Result := TdxCheckButtonEdgeStyle((Format and dxPSGlbl.dxFormatCheckButtonEdgeStyleMask) shr dxPSGlbl.dxFormatCheckButtonEdgeStyleOffset);
end;

function TdxCustomReportCellCheck.GetChecked: Boolean;
begin
  Result := GetFormatBit(dxPSGlbl.dxFormatCheckChecked);
end;

function TdxCustomReportCellCheck.GetCheckPos: TdxCellCheckPos;
begin
  Result := TdxCellCheckPos((Format and dxPSGlbl.dxFormatCheckPosMask) shr dxPSGlbl.dxFormatCheckPosOffset);
end;

function TdxCustomReportCellCheck.GetEnabled: Boolean;
begin
  Result := GetFormatBit(dxPSGlbl.dxFormatCheckEnabled);
end;

function TdxCustomReportCellCheck.GetFlatBorder: Boolean;
begin
  Result := ButtonEdgeStyle = cbesUltraFlat;
end;

procedure TdxCustomReportCellCheck.SetBoldBorder(Value: Boolean);
begin
  if Value then
    ButtonEdgeStyle := cbesBoldFlat;
end;

procedure TdxCustomReportCellCheck.SetButtonEdgeStyle(Value: TdxCheckButtonEdgeStyle);
begin
  Format := Format and not dxPSGlbl.dxFormatCheckButtonEdgeStyleMask or (Byte(Value) shl dxPSGlbl.dxFormatCheckButtonEdgeStyleOffset);
end;

procedure TdxCustomReportCellCheck.SetCheckPos(Value: TdxCellCheckPos);
begin
  if Value = ccpCenter then
    SetText('');
  Format := Format and not dxPSGlbl.dxFormatCheckPosMask or (Byte(Value) shl dxPSGlbl.dxFormatCheckPosOffset);
end;

procedure TdxCustomReportCellCheck.SetEnabled(Value: Boolean);
begin
  SetFormatBit(dxPSGlbl.dxFormatCheckEnabled, Value);
end;

procedure TdxCustomReportCellCheck.SetFlatBorder(Value: Boolean);
begin
  if Value then
    ButtonEdgeStyle := cbesUltraFlat
  else
    ButtonEdgeStyle := cbes3D;
end;

{ TdxCustomReportCellRadio }

class function TdxCustomReportCellRadio.IsRadio: Boolean;
begin
  Result := True;
end;

{ TdxCustomReportCellCheckImage }

constructor TdxCustomReportCellCheckImage.Create(AParent: TdxReportCell);
begin
  inherited;
  GlyphCount := 1;
end;

destructor TdxCustomReportCellCheckImage.Destroy;
begin
  ReleaseGlyph;
  inherited;
end;

procedure TdxCustomReportCellCheckImage.Assign(Source: TPersistent);
begin
  if Source is TdxCustomReportCellCheckImage then
    with TdxCustomReportCellCheckImage(Source) do
    begin
      Self.GlyphCount := GlyphCount;
      if HasGlyph then
        Self.SetGlyph(Glyph)
      else
        Self.ReleaseGlyph;
    end;
  inherited;
end;

procedure TdxCustomReportCellCheckImage.DrawCheck(ACanvas: TdxPSReportRenderCustomCanvas);
begin
  if HasGlyph and not Glyph.Empty and (GlyphCount <> 0) then
    DrawCheckGlyph(ACanvas)
  else
    inherited DrawCheck(ACanvas);
end;

procedure TdxCustomReportCellCheckImage.DrawCheckGlyph(ACanvas: TdxPSReportRenderCustomCanvas);
var
  R: TRect;
begin
  if HasGlyph and (GlyphCount <> 0) then
  begin
    R := GlyphPartialBounds;
    OffsetRect(R, -R.Left, -R.Top);
    with Renderer.FCheckBitmap do
    begin
      Height := R.Bottom - R.Top;
      Width := R.Right - R.Left;
      Canvas.Brush.Color := clWhite;
      Canvas.FillRect(R);
      Glyph.StretchDraw(Canvas.Handle, R, GlyphPartialBounds);
    end;
    Renderer.CachedGraphicInfo.Clear;
    Renderer.DrawGraphicEx(ACanvas, GetCheckBounds(ACanvas),
      GetImageAreaBounds(ACanvas), nil, 0, Renderer.FCheckBitmap, True, True,
      Color, ContentBkColor, ContentPattern, cibAlways);
  end;
end;

procedure TdxCustomReportCellCheckImage.GetImageSizes(var AImageWidth, AImageHeight: Integer);
begin
  if HasGlyph and not Glyph.Empty and (GlyphCount <> 0) then
  begin
    AImageWidth := MulDiv(Glyph.Width div GlyphCount, PixelsNumerator, PixelsDenominator);
    AImageHeight := MulDiv(Glyph.Height, PixelsNumerator, PixelsDenominator);
  end
  else
    inherited;
end;

function TdxCustomReportCellCheckImage.GetGlyph: TdxSmartGlyph;
begin
  Result := nil;
end;

function TdxCustomReportCellCheckImage.GetGlyphCount: Integer;
begin
  Result := 1;
end;

function TdxCustomReportCellCheckImage.GetGlyphIndex: Integer;
//cbUnchecked, cbChecked, cbGrayed
const
  GlyphIndexes: array[TCheckBoxState] of Integer = (0, 1, 2);
begin
  Result := GlyphIndexes[State];
  if (State = cbGrayed) and (GlyphCount < 3) then
    Result := 0;
  Result := Min(Result, GlyphCount - 1);
end;

procedure TdxCustomReportCellCheckImage.SetGlyph(Value: TdxSmartGlyph);
begin
end;

procedure TdxCustomReportCellCheckImage.SetGlyphCount(Value: Integer);
begin
end;

function TdxCustomReportCellCheckImage.HasGlyph: Boolean;
begin
  Result := False;
end;

procedure TdxCustomReportCellCheckImage.ReleaseGlyph;
begin
end;

procedure TdxCustomReportCellCheckImage.ReadData(AReader: TdxPSDataReader);
begin
  inherited;
  if AReader.ReadBoolean then AReader.ReadImage(Glyph);
end;

procedure TdxCustomReportCellCheckImage.WriteData(AWriter: TdxPSDataWriter);
begin
  inherited;
  AWriter.WriteBoolean(HasGlyph);
  if HasGlyph then AWriter.WriteImage(Glyph);
end;

function TdxCustomReportCellCheckImage.GetGlyphPartialBounds: TRect;
var
  AWidth: Integer;
begin
  if HasGlyph and (GlyphCount <> 0) then
  begin
    if GlyphCount = 1 then
      Result := Bounds(0, 0, Glyph.Width, Glyph.Height)
    else
    begin
      AWidth := (Glyph.Width div GlyphCount);
      Result := Bounds(AWidth * GlyphIndex, 0, AWidth, Glyph.Height);
    end;
  end
  else
    Result := cxNullRect;
end;

{ TdxReportCellCheckImage }

function TdxReportCellCheckImage.GetGlyph: TdxSmartGlyph;
begin
  if FGlyph = nil then
    FGlyph := TdxSmartGlyph.Create;
  Result := FGlyph;
end;

function TdxReportCellCheckImage.GetGlyphCount: Integer;
begin
  Result := (Format and dxPSGlbl.dxFormatCheckGlyphCountMask) shr dxPSGlbl.dxFormatCheckGlyphCountOffset;
end;

procedure TdxReportCellCheckImage.SetGlyph(Value: TdxSmartGlyph);
begin
  if Value = nil then
    ReleaseGlyph
  else
    Glyph.Assign(Value);
end;

procedure TdxReportCellCheckImage.SetGlyphCount(Value: Integer);
begin
  Value := Min(Max(0, Value), MaxGlyphCount);
  Format := Format and not dxPSGlbl.dxFormatCheckGlyphCountMask or
    (DWORD(Value) shl dxPSGlbl.dxFormatCheckGlyphCountOffset);
end;

function TdxReportCellCheckImage.HasGlyph: Boolean;
begin
  Result := FGlyph <> nil;
end;

procedure TdxReportCellCheckImage.ReleaseGlyph;
begin
  FreeAndNil(FGlyph);
end;

{ TdxCustomReportButtonGroup }

constructor TdxCustomReportButtonGroup.Create(AParent: TdxReportCell);
begin
  inherited Create(AParent);
  FColumnCount := 1;
  FInterColumnsMinSpace := dxRadioGroupInterColumnsMinSpace;
  FInterRowsMinSpace := dxRadioGroupInterRowsMinSpace;
  FIndents := Rect(dxRadioGroupBoundsIndent, dxRadioGroupBoundsIndent, dxRadioGroupBoundsIndent, dxRadioGroupBoundsIndent);
  ShowCaption := False;
end;

procedure TdxCustomReportButtonGroup.Assign(Source: TPersistent);
begin
  if Source is TdxCustomReportButtonGroup then
    with TdxCustomReportButtonGroup(Source) do
    begin
      Self.ColumnCount := ColumnCount;
      Self.InterColumnsMinSpace := InterColumnsMinSpace;
      Self.InterRowsMinSpace := InterRowsMinSpace;
      Self.Indents := Indents;
    end;
  inherited;
end;

procedure TdxCustomReportButtonGroup.AdjustContent(ACanvas: TdxPSReportRenderCustomCanvas);
type
  TViewInfo = record
    ColumnWidth: Integer;
    ItemsArea: TRect;
    RowHeight: Integer;
  end;

var
  ViewInfo: TViewInfo;

  procedure CalcItemSizes;
  var
    I: Integer;
  begin
    FItemSize.cX := 0;
    for I := 0 to ItemCount - 1 do
      FItemSize.cx := Max(Items[I].MeasureContentWidth(ACanvas), FItemSize.cx);
    // Note: 4 - difference between PS and cxRadioGroup AB9777
    FItemSize.cY := Items[0].MeasureContentHeight(ACanvas) - 4;
  end;

  procedure CalcViewInfo;
  begin
    FillChar(ViewInfo, SizeOf(TViewInfo), 0);
    with ViewInfo do
    begin
      ItemsArea := BoundsRect;
      OffsetRect(ItemsArea, -ItemsArea.Left, -ItemsArea.Top);
      if ShowCaption then
        ItemsArea := GetInnerBoundsRelativeTo(ACanvas, ItemsArea);

      Inc(ItemsArea.Left, Indents.Left);
      if ShowCaption then
        Dec(ItemsArea.Top, Indents.Top)
      else
        Inc(ItemsArea.Top, Indents.Top);
      Dec(ItemsArea.Right, Indents.Right);
      Dec(ItemsArea.Bottom, Indents.Bottom);

      if not IsRectEmpty(ItemsArea) then
      begin
        ColumnWidth := Max(ItemSize.cX + InterColumnsMinSpace,
          (ItemsArea.Right - ItemsArea.Left) div ColumnCount);
        RowHeight := Max(ItemSize.cY, cxRectHeight(ItemsArea) div RowCount);
      end;
    end;
  end;

  function GetColumnRect(AnIndex: Integer): TRect;
  begin
    with ViewInfo do
    begin
      Result := ItemsArea;
      Result.Right := Result.Left + ColumnWidth;
      OffsetRect(Result, AnIndex * ColumnWidth, 0);
    end;
  end;

  function GetRowRect(AnIndex: Integer): TRect;
  begin
    with ViewInfo do
    begin
      Result := ItemsArea;
      Result.Bottom := Result.Top + RowHeight;
      OffsetRect(Result, 0, AnIndex * RowHeight);
    end;
  end;

  function GetItemBounds(AnIndex: Integer): TRect;
  var
    Delta: Integer;
  begin
    with GetColumnRect(ItemColumns[AnIndex]) do
    begin
      Result.Left := Left;
      Result.Right := Right;
    end;
    with GetRowRect(ItemRows[AnIndex]) do
    begin
      Result.Top := Top;
      Result.Bottom := Bottom;
    end;

    Delta := (Result.Bottom - Result.Top - ItemSize.cY) div 2;
    InflateRect(Result, 0, -Delta);
  end;

  procedure CheckItemVisibility(AnItem: TdxReportVisualItem);
  var
    R: TRect;
  begin
    AnItem.Visible := IntersectRect(R, AnItem.BoundsRect, ViewInfo.ItemsArea);
  end;

var
  I, J, Index: Integer;
  Item: TdxCustomReportCellCheck;
begin
  if ItemCount <> 0 then
  begin
    CalcItemSizes;
    CalcViewInfo;
    if not IsRectEmpty(ViewInfo.ItemsArea) then
      for I := 0 to ColumnCount - 1 do
        for J := 0 to RowCount - 1 do
        begin
          Index := I * RowCount + J;
          if Index > ItemCount - 1 then Break;

          Item := Items[Index];
          Item.BoundsRect := GetItemBounds(Index);
          CheckItemVisibility(Item);
        end;
  end;
end;

function TdxCustomReportButtonGroup.MeasureContentHeight(
  ACanvas: TdxPSReportRenderCustomCanvas): Integer;
begin
  Result := RowCount * (ItemSize.cY + InterRowsMinSpace) - InterRowsMinSpace;
  Inc(Result, Indents.Top + Indents.Bottom);
  if RowCount <> 0 then
    Inc(Result, 2 * cxDrawTextUtils.cxTextSpace);
end;

function TdxCustomReportButtonGroup.MeasureContentWidth(
  ACanvas: TdxPSReportRenderCustomCanvas): Integer;
begin
  Result := ColumnCount * (ItemSize.cX + InterColumnsMinSpace) - InterColumnsMinSpace;
  Inc(Result, Indents.Left + Indents.Right);
end;

function TdxCustomReportButtonGroup.Add(const AText: string = ''): TdxCustomReportCellCheck;
begin
  Result := GetItemClass.Create(Self);
  Result.Text := AText;
  Result.BoundsRect := Rect(0, 0, -1, 0);
  InitializeItem(Result);
end;

procedure TdxCustomReportButtonGroup.Clear;
begin
  ClearDataItems;
end;

procedure TdxCustomReportButtonGroup.Delete(Index: Integer);
begin
  DeleteDataItem(Index);
end;

function TdxCustomReportButtonGroup.FindItem(const ACaption: string): Integer;
begin
  for Result := 0 to ItemCount - 1 do
    if Items[Result].Text = ACaption then Exit;
  Result := -1;
end;

procedure TdxCustomReportButtonGroup.ConvertCoords(APixelsNumerator, APixelsDenominator: Integer);
var
  R: TRect;
begin
  inherited;
  R := Indents;
  with R do
  begin
    Left := MulDiv(Left, APixelsNumerator, APixelsDenominator);
    Right := MulDiv(Right, APixelsNumerator, APixelsDenominator);
    Top := MulDiv(Top, APixelsNumerator, APixelsDenominator);
    Bottom := MulDiv(Bottom, APixelsNumerator, APixelsDenominator);
  end;
  Indents := R;

  InterColumnsMinSpace := MulDiv(InterColumnsMinSpace, APixelsNumerator, APixelsDenominator);
  InterRowsMinSpace := MulDiv(InterRowsMinSpace, APixelsNumerator, APixelsDenominator);
end;

procedure TdxCustomReportButtonGroup.SetFontIndex(Value: Integer);
var
  I: Integer;
begin
  inherited;
  for I := 0 to ItemCount - 1 do
    Items[I].FontIndex := Value;
end;

procedure TdxCustomReportButtonGroup.ReadDataItems(AReader: TdxPSDataReader);
begin
  FLocked := True;
  try
    inherited;
  finally
    FLocked := False;
  end;
end;

procedure TdxCustomReportButtonGroup.ReadData(AReader: TdxPSDataReader);
begin
  inherited ReadData(AReader);
  ColumnCount := AReader.ReadInteger;
  InterColumnsMinSpace := AReader.ReadInteger;
  InterRowsMinSpace := AReader.ReadInteger;
  Indents := AReader.ReadRect;
end;

procedure TdxCustomReportButtonGroup.WriteData(AWriter: TdxPSDataWriter);
begin
  inherited WriteData(AWriter);
  AWriter.WriteInteger(ColumnCount);
  AWriter.WriteInteger(InterColumnsMinSpace);
  AWriter.WriteInteger(InterRowsMinSpace);
  AWriter.WriteRect(Indents);
end;

class function TdxCustomReportButtonGroup.GetItemClass: TdxCustomReportCellCheckClass;
begin
  Result := TdxCustomReportCellCheck;
end;

procedure TdxCustomReportButtonGroup.InitializeItem(AnItem: TdxCustomReportCellCheck);
begin
  AnItem.ButtonEdgeStyle := ButtonEdgeStyle;
  AnItem.CellSides := [];
  AnItem.CheckPos := CheckPos;
  AnItem.Color := Color;
  AnItem.FontIndex := FontIndex;
  AnItem.TextAlignY := taCenterY;
  AnItem.Transparent := True;
end;

function TdxCustomReportButtonGroup.GetButtonEdgeStyle: TdxCheckButtonEdgeStyle;
begin
  Result := TdxCheckButtonEdgeStyle((Format and dxPSGlbl.dxFormatCheckButtonEdgeStyleMask) shr dxPSGlbl.dxFormatCheckButtonEdgeStyleOffset);
end;

function TdxCustomReportButtonGroup.GetCheckPos: TdxCellCheckPos;
begin
  Result := TdxCellCheckPos((Format and dxPSGlbl.dxFormatCheckPosMask) shr dxPSGlbl.dxFormatCheckPosOffset);
end;

function TdxCustomReportButtonGroup.GetItem(Index: Integer): TdxCustomReportCellCheck;
begin
  Result := DataItems[Index] as TdxCustomReportCellCheck;
end;

function TdxCustomReportButtonGroup.GetItemColumn(Index: Integer): Integer;
begin
  Result := Index div RowCount;
end;

function TdxCustomReportButtonGroup.GetItemCount: Integer;
begin
  Result := DataItemCount;
end;

function TdxCustomReportButtonGroup.GetItemRow(Index: Integer): Integer;
begin
  Result := Index mod RowCount;
end;

function TdxCustomReportButtonGroup.GetRowCount: Integer;
begin
  Result := ItemCount div ColumnCount;
  if (ItemCount <> 0) and (ItemCount mod ColumnCount <> 0) then
    Inc(Result);
end;

procedure TdxCustomReportButtonGroup.SetButtonEdgeStyle(Value: TdxCheckButtonEdgeStyle);
var
  I: Integer;
begin
  Format := Format and not dxPSGlbl.dxFormatCheckButtonEdgeStyleMask or (Byte(Value) shl dxPSGlbl.dxFormatCheckButtonEdgeStyleOffset);

  for I := 0 to ItemCount - 1 do
    Items[I].ButtonEdgeStyle := Value;
end;

procedure TdxCustomReportButtonGroup.SetCheckPos(Value: TdxCellCheckPos);
var
  I: Integer;
begin
  Format := Format and not dxPSGlbl.dxFormatCheckPosMask or (Byte(Value) shl dxPSGlbl.dxFormatCheckPosOffset);

  if Value = ccpCenter then
    for I := 0 to ItemCount - 1 do
      Items[I].Text := ''
end;

procedure TdxCustomReportButtonGroup.SetColumnCount(Value: Integer);
begin
  FColumnCount := Max(Value, 1);
end;

procedure TdxCustomReportButtonGroup.SetInterColumnsMinSpace(Value: Integer);
begin
  FInterColumnsMinSpace := Max(Value, 0);
end;

procedure TdxCustomReportButtonGroup.SetInterRowsMinSpace(Value: Integer);
begin
  FInterRowsMinSpace := Max(Value, 0);
end;

procedure TdxCustomReportButtonGroup.SetIndents(Value: TRect);
begin
  with Value do
  begin
    if Left < 0 then Left := 0;
    if Top < 0 then Top := 0;
    if Right < 0 then Right := 0;
    if Bottom < 0 then Bottom := 0;
  end;
  FIndents := Value;
end;

{ TdxReportCellRadioGroupButton }

type
  TdxReportCellRadioGroupButton = class(TdxCustomReportCellRadio)
  private
    function GetParent: TdxReportRadioGroup;
  protected
    procedure SetChecked(Value: Boolean); override;
  public
    property Checked write SetChecked;
    property Enabled;
    property Parent: TdxReportRadioGroup read GetParent;
  end;

  // for backward compatibility with saved reports
  TdxReportCellGroupButton = class(TdxReportCellRadioGroupButton);

function TdxReportCellRadioGroupButton.GetParent: TdxReportRadioGroup;
begin
  Result := inherited Parent as TdxReportRadioGroup;
end;

procedure TdxReportCellRadioGroupButton.SetChecked(Value: Boolean);
begin
  inherited SetChecked(Value);
  if Value then
    Parent.ItemIndex := Index
  else
    Parent.ItemIndex := -1;
end;

{ TdxReportRadioGroup }

function TdxReportRadioGroup.Add(const AText: string = ''): TdxCustomReportCellRadio;
begin
  Result := inherited Add(AText) as TdxCustomReportCellRadio;
end;

class function TdxReportRadioGroup.GetItemClass: TdxCustomReportCellCheckClass;
begin
  Result := TdxReportCellRadioGroupButton;
end;

function TdxReportRadioGroup.GetItem(Index: Integer): TdxCustomReportCellRadio;
begin
  Result := inherited Items[Index] as TdxCustomReportCellRadio;
end;

function TdxReportRadioGroup.GetItemIndex: Integer;
begin
  for Result := 0 to ItemCount - 1 do
    if Items[Result].Checked then Exit;
  Result := -1;
end;

procedure TdxReportRadioGroup.SetItemIndex(Value: Integer);
var
  AnItemIndex: Integer;
begin
  if Locked then Exit;

  Value := Max(Value, -1);
  Value := Min(Value, ItemCount - 1);

  AnItemIndex := ItemIndex;
  if AnItemIndex <> Value then
  begin
    Locked := True;
    try
      if AnItemIndex <> -1 then
        Items[AnItemIndex].Checked := False;
      if Value <> -1 then
        Items[Value].Checked := True;
    finally
      Locked := False;
    end;
  end;
end;

{ TdxReportCellCheckGroupButton }

type
  TdxReportCellCheckGroupButton = class(TdxCustomReportCellCheckImage)
  private
    function GetParent: TdxReportCheckGroup;
  protected
    function GetGlyph: TdxSmartGlyph; override;
    function GetGlyphCount: Integer; override;
    function HasGlyph: Boolean; override;
  public
    property Parent: TdxReportCheckGroup read GetParent;
  end;

{ TdxReportCellCheckGroupButton }

function TdxReportCellCheckGroupButton.GetGlyph: TdxSmartGlyph;
begin
  if Parent <> nil then
    Result := Parent.Glyph
  else
    Result := inherited GetGlyph;
end;

function TdxReportCellCheckGroupButton.GetGlyphCount: Integer;
begin
  if Parent <> nil then
    Result := Parent.GlyphCount
  else
    Result := inherited GetGlyphCount;
end;

function TdxReportCellCheckGroupButton.HasGlyph: Boolean;
begin
  Result := (Parent <> nil) and Parent.HasGlyph;
end;

function TdxReportCellCheckGroupButton.GetParent: TdxReportCheckGroup;
begin
  Result := inherited Parent as TdxReportCheckGroup;
end;

{ TdxReportCheckGroup }

constructor TdxReportCheckGroup.Create(AParent: TdxReportCell);
begin
  inherited;
  GlyphCount := 1;
end;

destructor TdxReportCheckGroup.Destroy;
begin
  ReleaseGlyph;
  inherited;
end;

procedure TdxReportCheckGroup.Assign(Source: TPersistent);
begin
  if Source is TdxReportCheckGroup then
    with TdxReportCheckGroup(Source) do
    begin
      if HasGlyph then
        Self.Glyph := Glyph
      else
        Self.ReleaseGlyph;
      Self.GlyphCount := GlyphCount;
    end;
end;

function TdxReportCheckGroup.Add(const AText: string = ''): TdxCustomReportCellCheckImage;
begin
  Result := inherited Add(AText) as TdxCustomReportCellCheckImage;
end;

function TdxReportCheckGroup.HasGlyph: Boolean;
begin
  Result := FGlyph <> nil;//) and not FGlyph.Empty;
end;

procedure TdxReportCheckGroup.ReleaseGlyph;
begin
  FreeAndNil(FGlyph);
end;

procedure TdxReportCheckGroup.ReadData(AReader: TdxPSDataReader);
begin
  inherited;
  GlyphCount := AReader.ReadInteger;
  if AReader.ReadBoolean then AReader.ReadImage(Glyph);
end;

procedure TdxReportCheckGroup.WriteData(AWriter: TdxPSDataWriter);
begin
  inherited;
  AWriter.WriteInteger(GlyphCount);
  AWriter.WriteBoolean(HasGlyph);
  if HasGlyph then AWriter.WriteImage(Glyph);
end;

class function TdxReportCheckGroup.GetItemClass: TdxCustomReportCellCheckClass;
begin
  Result := TdxReportCellCheckGroupButton;
end;

procedure TdxReportCheckGroup.InitializeItem(AnItem: TdxCustomReportCellCheck);
begin
  inherited;
end;

function TdxReportCheckGroup.GetGlyph: TdxSmartGlyph;
begin
  if FGlyph = nil then
    FGlyph := TdxSmartGlyph.Create;
  Result := FGlyph;
end;

function TdxReportCheckGroup.GetItem(Index: Integer): TdxCustomReportCellCheckImage;
begin
  Result := inherited Items[Index] as TdxCustomReportCellCheckImage;
end;

function TdxReportCheckGroup.GetItemChecked(Index: Integer): Boolean;
begin
  Result := Items[Index].Checked;
end;

function TdxReportCheckGroup.GetItemEnabled(Index: Integer): Boolean;
begin
  Result := Items[Index].Enabled;
end;

function TdxReportCheckGroup.GetItemState(Index: Integer): TCheckBoxState;
begin
  Result := Items[Index].State;
end;

procedure TdxReportCheckGroup.SetGlyph(Value: TdxSmartGlyph);
begin
  Glyph.Assign(Value);
end;

procedure TdxReportCheckGroup.SetGlyphCount(Value: Integer);
begin
  Value := Max(Value, 0);
  Value := Min(Value, MaxGlyphCount);
  FGlyphCount := Value;
end;

procedure TdxReportCheckGroup.SetItemChecked(Index: Integer; Value: Boolean);
begin
  Items[Index].Checked := Value;
end;

procedure TdxReportCheckGroup.SetItemEnabled(Index: Integer; Value: Boolean);
begin
  Items[Index].Enabled := Value;
end;

{ TCustomdxReportCellImageContainer }

constructor TCustomdxReportCellImageContainer.Create(AParent: TdxReportCell);
begin
  inherited Create(AParent);
  ImageIndex := -1;
  ImageTransparent := True;
  ImageTransparentColor := clNone;
  OverlayImageIndex := -1;
end;

destructor TCustomdxReportCellImageContainer.Destroy;
begin
  FreeAndNil(FImage);
  inherited Destroy;
end;

procedure TCustomdxReportCellImageContainer.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TCustomdxReportCellImageContainer then
    with TCustomdxReportCellImageContainer(Source) do
    begin
      Self.SetImage(FImage);
      Self.ImageTransparent := ImageTransparent;
      Self.ImageIndex := ImageIndex;
      Self.ImageList := ImageList;
    end;
end;

function TCustomdxReportCellImageContainer.CreateImage(AGraphicClass: TGraphicClass): TGraphic;
begin
  if (FImage = nil) or (FImage.ClassType <> AGraphicClass) then
  begin
    FreeAndNil(FImage);
    FImage := dxPSUtl.CreateGraphic(AGraphicClass);
  end;
  Result := FImage;
end;

procedure TCustomdxReportCellImageContainer.DrawImage(
  ACanvas: TdxPSReportRenderCustomCanvas);

  procedure DoDrawImage(AImage: TGraphic; AImageList: TCustomImageList; AImageIndex: Integer);
  begin
    Renderer.DrawGraphicEx(ACanvas, GetImageBounds(ACanvas),
      GetImageAreaBounds(ACanvas), AImageList, AImageIndex, AImage,
      ImageTransparent, IsBackgroundBitmapDrawn or Transparent, Color,
      ContentBkColor, ContentPattern, ActualImageBuffering, ImageTransparentColor);
  end;

  procedure DoDrawOverlay(AImageList: TCustomImageList; AImageIndex: Integer);
  begin
    if (AImageList <> nil) and (AImageIndex <> -1) then
      Renderer.DrawGraphicEx(ACanvas, GetImageBounds(ACanvas),
        GetImageAreaBounds(ACanvas), AImageList, AImageIndex, nil,
        True, True, clNone, clNone, ContentPattern, ActualImageBuffering, ImageTransparentColor);
  end;

  procedure DoPrepareImage;
  var
    ATempImage: TcxBitmap;
  begin
    if ((Image = nil) or not ImagePrepared) and CanPrepareImage then
    begin
      ATempImage := PrepareImage;
      try
        Image := ATempImage;
        ImagePrepared := True;
        ImageIndex := -1;
        ImageList := nil;
      finally
        ATempImage.Free;
      end;
    end;
  end;

var
  ABitmap: TcxBitmap;
begin
  Renderer.CachedGraphicInfo.Clear;
  if ImageListUsed and CanPrepareImage then
  begin
    ABitmap := PrepareImage;
    try
      DoDrawImage(ABitmap, nil, -1);
    finally
      ABitmap.Free;
    end;
  end
  else
  begin
    DoPrepareImage;
    DoDrawImage(Image, ImageList, ImageIndex);
    DoDrawOverlay(ImageList, OverlayImageIndex);
  end;
end;

function TCustomdxReportCellImageContainer.GetActualImageBuffering: TdxCellImageActualBuffering;
const
  BufferingMap: array[Boolean] of TdxCellImageActualBuffering = (cibNone, cibAlways);
begin
  Result := ImageBuffering;
  if Result = cibDefault then
  begin
    if Assigned(ReportCells) and Assigned(ReportCells.ReportLink) then
      Result := BufferingMap[ReportCells.ReportLink.AlwaysBufferedGraphics]
    else
      Result := cibAlways;
  end;
end;

function TCustomdxReportCellImageContainer.GetImageBuffering: TdxCellImageBuffering;
begin
  Result := cibDefault;
end;

procedure TCustomdxReportCellImageContainer.GetImageSizes(var AImageWidth, AImageHeight: Integer);
var
  ASize: TSize;
begin
  if (ImageList <> nil) or (Image <> nil) then
  begin
    ASize := cxSizeScale(GetOriginalImageSize, PixelsNumerator, PixelsDenominator);
    AImageHeight := ASize.cy;
    AImageWidth := ASize.cx;
  end
  else
    inherited;
end;

function TCustomdxReportCellImageContainer.GetImageSourceDPI: Integer;
begin
  Result := dxGetImageSourceDPI(Image);
end;

function TCustomdxReportCellImageContainer.GetOriginalImageSize: TSize;
begin
  if ImageList <> nil then
  begin
    Result := cxSize(ImageList.Width, ImageList.Height);
    if ReportLink <> nil then
      Result := cxSizeScale(Result, ReportLink.PixelsPerInch, dxGetImageSourceDPI(ImageList));
  end
  else
    if Image <> nil then
    begin
      Result := cxSize(Image.Width, Image.Height);
      if ReportLink <> nil then
        Result := cxSizeScale(Result, ReportLink.PixelsPerInch, GetImageSourceDPI);
    end
    else
      Result := cxNullSize;
end;

function TCustomdxReportCellImageContainer.HasImage: Boolean;
begin
  Result := Assigned(Image) and not Image.Empty or ImageListUsed;
end;

function TCustomdxReportCellImageContainer.CanPrepareImage: Boolean;
begin
  if ImageListUsed then
    Result := TcxImageList.GetPixelFormat(ImageList.Handle) = 32
  else
    Result := Assigned(Image) and not Image.InheritsFrom(TMetafile);

  Result := Result and (IsTextBackgroundDrawn or IsImageBackgroundDrawn or not dxCanPrintTransparentImages(Renderer.Canvas));
end;

function TCustomdxReportCellImageContainer.PrepareImage: TcxBitmap;

  function CreateTempBitmap: TcxBitmap;
  begin
    if ImageListUsed then
      Result := TcxBitmap.CreateSize(ImageList.Width, ImageList.Height, pf24Bit)
    else
      Result := TcxBitmap.CreateSize(Image.Width, Image.Height);
  end;

begin
  Result := CreateTempBitmap;
  PrepareImageBackground(Result.Canvas, Result.ClientRect);
  if ImageListUsed then
  begin
    ImageList.Draw(Result.Canvas, 0, 0, ImageIndex);
    ImageList.Draw(Result.Canvas, 0, 0, OverlayImageIndex);
  end
  else
  begin
    Image.Transparent := ImageTransparent;
    Result.Canvas.Draw(0, 0, Image);
  end;
end;

procedure TCustomdxReportCellImageContainer.PrepareImageBackground(ACanvas: TCanvas; const R: TRect);
var
  ARenderCanvas: TdxPSReportRenderCanvas;
begin
  ARenderCanvas := TdxPSReportRenderCanvas.Create(ACanvas);
  try
    Renderer.FillRectEx(ARenderCanvas, R, Color, ContentBkColor, ContentPattern);
  finally
    ARenderCanvas.Free;
  end;
end;

function TCustomdxReportCellImageContainer.GetImageListUsed: Boolean;
begin
  Result := IsImageAssigned(ImageList, ImageIndex);
end;

procedure TCustomdxReportCellImageContainer.SetImageBuffering(Value: TdxCellImageBuffering);
begin
end;

function TCustomdxReportCellImageContainer.GetImageListIndex: Integer;
begin
  Result := ReportCells.IndexOfImageList(ImageList);
end;

procedure TCustomdxReportCellImageContainer.DoReadData(AReader: TdxPSDataReader);
var
  ABytesToSkip: Integer;
  AGraphicClass: TGraphicClass;
begin
  if AReader.ReadBoolean then // HasImage
    case AReader.ReadInteger of
      imstImageList:
        begin
          ImageList := ReportCells.ImageLists[AReader.ReadInteger];
          ImageIndex := AReader.ReadInteger;
        end;

      imstShellLargeImageList, imstShellSmallImageList, imstImage:
        begin
          AGraphicClass := AReader.ReadGraphicClass;
          if AGraphicClass = nil then
          begin
            Image := nil;
            ABytesToSkip := AReader.ReadInt64;
            AReader.SkipBytes(ABytesToSkip);
          end
          else
            AReader.ReadImage(CreateImage(AGraphicClass));
        end;
    end;
end;

procedure TCustomdxReportCellImageContainer.DoWriteData(AWriter: TdxPSDataWriter);

  function RetrieveSourceType: Integer;
  begin
    if ImageList <> nil then
      if (ImageList = dxPSUtl.ShellLargeImages) or
        (ImageList.HandleAllocated and (ImageList.Handle = dxPSUtl.ShellLargeImages.Handle)) then
        Result := imstShellLargeImageList
      else
        if (ImageList = dxPSUtl.ShellSmallImages) or
          (ImageList.HandleAllocated and (ImageList.Handle = dxPSUtl.ShellSmallImages.Handle)) then
          Result := imstShellSmallImageList
        else
          Result := imstImageList
    else
      Result := imstImage;
  end;

  function RetrieveImage(ASourceType: Integer; AnImageIndex: Integer): TGraphic;
  begin
    case ASourceType of
      imstShellLargeImageList:
        begin
          Result := TBitmap.Create;
          dxPSUtl.ShellLargeImages.GetBitmap(AnImageIndex, TBitmap(Result));
        end;

      imstShellSmallImageList:
        begin
          Result := TBitmap.Create;
          dxPSUtl.ShellSmallImages.GetBitmap(AnImageIndex, TBitmap(Result));
        end;
    else
      Result := Self.Image;
    end;
  end;

var
  SourceType: Integer;
  Buffer: TGraphic;
begin
  AWriter.WriteBoolean(HasImage);
  if HasImage then
  begin
    SourceType := RetrieveSourceType;
    AWriter.WriteInteger(SourceType);
    case SourceType of
      imstImageList:
        begin
          AWriter.WriteInteger(GetImageListIndex);
          AWriter.WriteInteger(ImageIndex);
        end;

    else
      begin
        Buffer := RetrieveImage(SourceType, ImageIndex);
        try
          AWriter.WriteString(Buffer.ClassName);
          AWriter.WriteImage(Buffer);
        finally
          if Buffer <> Self.Image then Buffer.Free;
        end;
      end;
    end;
  end;
end;

procedure TCustomdxReportCellImageContainer.ReadData(AReader: TdxPSDataReader);
begin
  inherited ReadData(AReader);
  DoReadData(AReader);
end;

procedure TCustomdxReportCellImageContainer.WriteData(AWriter: TdxPSDataWriter);
begin
  inherited WriteData(AWriter);
  DoWriteData(AWriter);
end;

procedure TCustomdxReportCellImageContainer.SetImage(Value: TGraphic);
begin
  ImagePrepared := False;
  if Value <> nil then
  begin
    CreateImage(TGraphicClass(Value.ClassType));
    Image.Assign(Value);
    if Width = 0 then
      Width := Image.Width;
    if Height = 0 then
      Height := Image.Height;
  end
  else
    FreeAndNil(FImage);
end;

{ TdxReportCellImage }

constructor TdxReportCellImage.Create(AParent: TdxReportCell);
begin
  inherited Create(AParent);
  ImageLayout := ilImageCenterLeft;
  IsTextDrawnForCenteredImage := False;
  IsTextShiftedForHorizontallyCenteredImage := True;
  MakeSpaceForEmptyImage := True;
end;

procedure TdxReportCellImage.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TdxReportCellImage then
  begin
    ImageLayout := TdxReportCellImage(Source).ImageLayout;
    MakeSpaceForEmptyImage := TdxReportCellImage(Source).MakeSpaceForEmptyImage;
  end;
end;

function TdxReportCellImage.MeasureContentHeight(ACanvas: TdxPSReportRenderCustomCanvas): Integer;
begin
  Result := GetImageSize.cy;
  if Result <> 0 then
    Inc(Result, 2);

  if ImageLayout in [ilImageTopLeft, ilImageCenterLeft, ilImageBottomLeft,  ilImageTopRight, ilImageCenterRight, ilImageBottomRight] then
    Result := Max(inherited MeasureContentHeight(ACanvas), Result)
  else
    if ImageLayout in [ilImageTopCenter, ilImageBottomCenter] then
    begin
      if IsTextShiftedForHorizontallyCenteredImage then
        Inc(Result, inherited MeasureContentHeight(ACanvas))
      else
        Result := Max(Result, inherited MeasureContentHeight(ACanvas));
    end
    else
      if IsTextDrawnForCenteredImage then
        Result := Max(Result, inherited MeasureContentHeight(ACanvas));
end;

function TdxReportCellImage.MeasureContentWidth(ACanvas: TdxPSReportRenderCustomCanvas): Integer;
begin
  Result := GetImageSize.cx;
  if Result <> 0 then
    Inc(Result, 2);

  if ImageLayout in [ilImageTopLeft, ilImageCenterLeft, ilImageBottomLeft, ilImageTopRight, ilImageCenterRight, ilImageBottomRight] then
    Inc(Result, inherited MeasureContentHeight(ACanvas))
  else
    if (ImageLayout in [ilImageTopCenter, ilImageBottomCenter]) or
      ((ImageLayout = ilImageCenterCenter) and IsTextDrawnForCenteredImage)
    then
      Result := Max(inherited MeasureContentWidth(ACanvas), Result);
end;

function TdxReportCellImage.GetImageAreaBounds(ACanvas: TdxPSReportRenderCustomCanvas): TRect;
const
  MapTextAlign: array[TcxTextAlignY] of TdxImageLayout = (
    ilImageTopCenter, ilImageCenterCenter, ilImageBottomCenter, ilImageTopCenter);
var
  R: TRect;
  W, H: Integer;
begin
  GetImageSizes(W, H);
  if (W <> 0) and (H <> 0) then
  begin
    Result := inherited GetImageAreaBounds(ACanvas);
    case ImageLayout of
      ilImageTopRight, ilImageCenterRight, ilImageBottomRight:
        Result.Left := Result.Right - W - 2 * Renderer.UnitsPerPixel;
      ilImageCenterLeft, ilImageBottomLeft, ilImageTopLeft:
        Result.Right := Result.Left + W + 2 * Renderer.UnitsPerPixel;
      else
      begin
        if not IsTextShiftedForHorizontallyCenteredImage then
        begin
          Inc(Result.Left, (cxRectWidth(Result) - W) div 2 + Renderer.UnitsPerPixel);
          Result.Right := Result.Left + W + Renderer.UnitsPerPixel;
        end;
        case ImageLayout of
          ilImageTopCenter:
            Result.Bottom := Result.Top + H + 2 * Renderer.UnitsPerPixel;
          ilImageBottomCenter:
            Result.Top := Result.Bottom - H - 2 * Renderer.UnitsPerPixel;
          ilImageCenterCenter:
            if not IsTextShiftedForHorizontallyCenteredImage then
            begin
              Inc(Result.Top, (cxRectHeight(Result) - H) div 2 + Renderer.UnitsPerPixel);
              Result.Bottom := Result.Top + H + Renderer.UnitsPerPixel;
            end;
        end;
      end;

      if IsTextDrawnForCenteredImage and not IsTextShiftedForHorizontallyCenteredImage and
         (TextAlignX = taCenterX) and ((MapTextAlign[TextAlignY] = ImageLayout) or
         (ImageLayout = ilImageCenterCenter))
      then
        OffsetRect(Result, -(Renderer.CalcTextWidth(ACanvas, Text, Font) + W) div 2, 0);

      R := GetInnerBounds(ACanvas);
      Result.Left := Max(Result.Left, R.Left);
      Result.Bottom := Min(Result.Bottom, R.Bottom);
      Result.Right := Min(Result.Right, R.Right);
      Result.Top := Max(Result.Top, R.Top);
    end;
  end
  else
    Result := cxNullRect;
end;

function TdxReportCellImage.GetImageBounds(ACanvas: TdxPSReportRenderCustomCanvas): TRect;
var
  W, H: Integer;
begin
  Result := GetImageAreaBounds(ACanvas);
  if not IsRectEmpty(Result) then
  begin
    GetImageSizes(W, H);
    with Result do
    begin
      case ImageLayout of
        ilImageTopLeft, ilImageTopCenter, ilImageTopRight:
          Inc(Top, LineThickness);
        ilImageCenterLeft, ilImageCenterCenter, ilImageCenterRight:
          Inc(Top, (Bottom - Top - H) div 2);
        ilImageBottomLeft, ilImageBottomCenter, ilImageBottomRight:
          Top := Bottom - H - LineThickness;
      end;
      Inc(Left, (Right - Left - W) div 2);
      Right := Left + W;
      Bottom := Top + H;
    end;
  end;
end;

function TdxReportCellImage.GetTextBounds(ACanvas: TdxPSReportRenderCustomCanvas): TRect;
var
  R: TRect;
begin
  Result := inherited GetTextBounds(ACanvas);
  if IsImageDrawn or MakeSpaceForEmptyImage then
  begin
    if not ((ImageLayout in [ilImageTopCenter, ilImageCenterCenter, ilImageBottomCenter]) and
      not IsTextShiftedForHorizontallyCenteredImage) then
    begin
      R := GetImageBounds(ACanvas);
      if not IsRectEmpty(R) then
        with Result do
          case ImageLayout of
            ilImageTopLeft, ilImageCenterLeft, ilImageBottomLeft:
              Left := R.Right;
            ilImageTopRight, ilImageCenterRight, ilImageBottomRight:
              Right := R.Left;
            ilImageTopCenter:
              if IsTextShiftedForHorizontallyCenteredImage then
                Top := R.Bottom;
            ilImageCenterCenter:
              if not IsTextDrawn then
                Result := cxNullRect;
            ilImageBottomCenter:
              if IsTextShiftedForHorizontallyCenteredImage then
                Bottom := R.Top;
          end;
    end;
  end;
end;

function TdxReportCellImage.IsImageBackgroundDrawn: Boolean;
begin
  Result := inherited IsImageBackgroundDrawn and MakeSpaceForEmptyImage and
    not ((ImageLayout in [ilImageTopCenter, ilImageCenterCenter, ilImageBottomCenter]) and
          not IsTextShiftedForHorizontallyCenteredImage);
end;

function TdxReportCellImage.IsTextDrawn: Boolean;
begin
  Result := inherited IsTextDrawn and ((ImageLayout <> ilImageCenterCenter) or IsTextDrawnForCenteredImage);
end;

function TdxReportCellImage.IsTextBackgroundDrawn: Boolean;
begin
  Result := inherited IsTextBackgroundDrawn and
    (not (ImageLayout in [ilImageTopCenter, ilImageCenterCenter, ilImageBottomCenter]) or
    IsTextDrawnForCenteredImage or not HasImage);
end;

function TdxReportCellImage.GetImageLayout: TdxImageLayout;
begin
  Result := TdxImageLayout((Format and dxPSGlbl.dxFormatImageLayoutMask) shr dxPSGlbl.dxFormatImageLayoutOffset);
end;

function TdxReportCellImage.GetImageSize: TSize;
begin
  if Assigned(ImageList) then
    Result := cxSize(ImageList.Width, ImageList.Height)
  else
    if Assigned(Image) then
      Result := cxSize(Image.Width, Image.Height)
    else
      Result := cxNullSize;
end;

function TdxReportCellImage.GetIsTextDrawnForCenteredImage: Boolean;
begin
  Result := GetFormatBit(dxPSGlbl.dxFormatIsTextDrawnForCenteredImage);
end;

function TdxReportCellImage.GetIsTextShiftedForHorizontallyCenteredImage: Boolean;
begin
  Result := GetFormatBit(dxPSGlbl.dxFormatIsTextShiftedForHorizontallyCenteredImage);
end;

function TdxReportCellImage.GetMakeSpaceForEmptyImage: Boolean;
begin
  Result := GetFormatBit(dxPSGlbl.dxFormatMakeSpaceForEmptyImage);
end;

procedure TdxReportCellImage.SetImageLayout(Value: TdxImageLayout);
begin
  Format := Format and not dxPSGlbl.dxFormatImageLayoutMask or (Byte(Value) shl dxPSGlbl.dxFormatImageLayoutOffset);
end;

procedure TdxReportCellImage.SetIsTextDrawnForCenteredImage(Value: Boolean);
begin
  SetFormatBit(dxPSGlbl.dxFormatIsTextDrawnForCenteredImage, Value);
end;

procedure TdxReportCellImage.SetIsTextShiftedForHorizontallyCenteredImage(Value: Boolean);
begin
  SetFormatBit(dxPSGlbl.dxFormatIsTextShiftedForHorizontallyCenteredImage, Value);
end;

procedure TdxReportCellImage.SetMakeSpaceForEmptyImage(Value: Boolean);
begin
  SetFormatBit(dxPSGlbl.dxFormatMakeSpaceForEmptyImage, Value);
end;

{ TdxReportCellGraphic }

constructor TdxReportCellGraphic.Create(AParent: TdxReportCell);
begin
  inherited;
  ImageBuffering := cibAlways;
  CalculateDrawMode;
end;

function TdxReportCellGraphic.MeasureFontHeight(ACanvas: TdxPSReportRenderCustomCanvas): Integer;
begin
  Result := Renderer.CalcTextPatternHeight(ACanvas, Renderer.RenderInfo.BaseContentFont);
end;

function TdxReportCellGraphic.MeasureContentHeight(ACanvas: TdxPSReportRenderCustomCanvas): Integer;
var
  ASize: TSize;
begin
  ASize := GetOriginalImageSize;
  if ImageList <> nil then
    Result := ASize.cy
  else
    if Image <> nil then
    begin
      Result := ASize.cy;
      if RealStretch and (Result > 0) and (ASize.cx > 0) then
        Result := Round(Result * Min(1, Width / ASize.cx));
    end
    else
      Result := 0;
end;

function TdxReportCellGraphic.MeasureContentWidth(ACanvas: TdxPSReportRenderCustomCanvas): Integer;
begin
  Result := GetOriginalImageSize.cx;
end;

function TdxReportCellGraphic.GetImageBounds(ACanvas: TdxPSReportRenderCustomCanvas): TRect;
var
  W, H: Integer;
begin
  GetImageSizes(W, H);
  if (W <> 0) and (H <> 0) then
    Result := dxPSCalculateImageBounds(GetImageAreaBounds(ACanvas), W, H, DrawMode)
  else
    Result := cxNullRect;
end;

function TdxReportCellGraphic.GetImageBuffering: TdxCellImageBuffering;
begin
  Result := TdxCellImageBuffering((Format and dxPSGlbl.dxFormatGraphicBufferingMask) shr
    dxPSGlbl.dxFormatGraphicBufferingOffset);
end;

function TdxReportCellGraphic.GetTextBounds(ACanvas: TdxPSReportRenderCustomCanvas): TRect;
begin
  Result := cxNullRect;
end;

procedure TdxReportCellGraphic.SetImageBuffering(Value: TdxCellImageBuffering);
begin
  Format := Format and not dxPSGlbl.dxFormatGraphicBufferingMask or
    (Byte(Value) shl dxPSGlbl.dxFormatGraphicBufferingOffset);
end;

procedure TdxReportCellGraphic.CalculateDrawMode;
begin
  DrawMode := dxGraphicDrawModeMap[RealStretch, Proportional, Center];
end;

function TdxReportCellGraphic.GetImage: TGraphic;
begin
  Result := inherited Image;
end;

function TdxReportCellGraphic.GetDrawMode: TdxGraphicDrawMode;
begin
  Result := TdxGraphicDrawMode((Format and dxPSGlbl.dxFormatGraphicDrawModeMask) shr
    dxPSGlbl.dxFormatGraphicDrawModeOffset);
end;

function TdxReportCellGraphic.GetRealStretch: Boolean;
begin
  Result := Stretch or Proportional;
end;

procedure TdxReportCellGraphic.SetImage(Value: TGraphic);
begin
  inherited Image := Value;
  CalculateDrawMode;
end;

procedure TdxReportCellGraphic.SetCenter(Value: Boolean);
begin
  if Value <> FCenter then
  begin
    FCenter := Value;
    CalculateDrawMode;
  end;
end;

procedure TdxReportCellGraphic.SetDrawMode(Value: TdxGraphicDrawMode);
begin
  Format := Format and not dxPSGlbl.dxFormatGraphicDrawModeMask or (Byte(Value) shl dxPSGlbl.dxFormatGraphicDrawModeOffset);
end;

procedure TdxReportCellGraphic.SetProportional(Value: Boolean);
begin
  if Value <> FProportional then
  begin
    FProportional := Value;
    CalculateDrawMode;
  end;
end;

procedure TdxReportCellGraphic.SetStretch(Value: Boolean);
begin
  if Value <> FStretch then
  begin
    FStretch := Value;
    CalculateDrawMode;
  end;
end;

{ TdxReportCellDpiAwareGraphic }

constructor TdxReportCellDpiAwareGraphic.Create(AParent: TdxReportCell);
begin
  inherited;

  if ReportLink <> nil then
    FImageSourceDPI := ReportLink.PixelsPerInch
  else
    FImageSourceDPI := dxDefaultDPI;
end;

function TdxReportCellDpiAwareGraphic.GetImageSourceDPI: Integer;
begin
  Result := ImageSourceDPI;
end;

procedure TdxReportCellDpiAwareGraphic.ReadData(AReader: TdxPSDataReader);
begin
  inherited;
  ImageSourceDPI := AReader.ReadInteger;
end;

procedure TdxReportCellDpiAwareGraphic.WriteData(AWriter: TdxPSDataWriter);
begin
  inherited;
  AWriter.WriteInteger(ImageSourceDPI);
end;

procedure TdxReportCellDpiAwareGraphic.SetImageSourceDPI(AValue: Integer);
begin
  AValue := dxCheckDPIValue(AValue);
  if AValue <> FImageSourceDPI then
  begin
    FImageSourceDPI := AValue;
    CalculateDrawMode;
  end;
end;

{ TdxPSReportCellTreeLineViewInfo }

constructor TdxPSReportCellTreeLineViewInfo.Create(
  AMode: TdxPSTreeLineMode; ACellButton: TdxReportCellExpandButton);
begin
  FMode := AMode;
  FCellButton := ACellButton;
  FParts := CalculateParts(Mode);
end;

function TdxPSReportCellTreeLineViewInfo.CalculateParts(
  AMode: TdxPSTreeLineMode): TdxPSTreeLineParts;
const
  PartsMap: array[TdxPSTreeLineMode] of TdxPSTreeLineParts = (
    [], [tlpTop, tlpBottom], [tlpTop, tlpRight, tlpBottom],
    [tlpBottom, tlpRight], [tlpTop, tlpRight]
  );
begin
  Result := PartsMap[AMode];
end;

procedure TdxPSReportCellTreeLineViewInfo.CalculateRects(const ABounds, AButtonRect: TRect);
var
  AMiddleHeight: Integer;
begin
  FButtonRect := AButtonRect;
  AMiddleHeight := (ABounds.Top + ABounds.Bottom) div 2;

  FVerticalRect := ABounds;
  FVerticalRect.Left := (VerticalRect.Left + VerticalRect.Right - LineThickness) div 2;
  FVerticalRect.Right := VerticalRect.Left + LineThickness;

  FRightRect := ABounds;
  FRightRect.Left := FVerticalRect.Left;
  FRightRect.Top := AMiddleHeight;
  FRightRect.Bottom := FRightRect.Top + LineThickness;

  if not (tlpTop in Parts) then
    FVerticalRect.Top := AMiddleHeight;
  if not (tlpBottom In Parts) then
    FVerticalRect.Bottom := AMiddleHeight;
  if not (tlpRight In Parts) then
    FRightRect.Right := FRightRect.Left;
end;

function TdxPSReportCellTreeLineViewInfo.CreateTreeLineRegion(
  const R: TRect; AIsVertical: Boolean): TcxRegion;

  function CalculateCount(const R: TRect; AIsVertical: Boolean): Integer;
  begin
    if AIsVertical then
      Result := cxRectHeight(R) div (2 * LineThickness)
    else
      Result := cxRectWidth(R) div (2 * LineThickness);
  end;

var
  I, ACount: Integer;
  R1: TRect;
begin
  if TreeLineStyle = tlsSolid then
    Result := TcxRegion.Create(R)
  else
  begin
    Result := TcxRegion.Create(cxNullRect);
    R1 := Rect(R.Left, R.Top, R.Left + LineThickness, R.Top + LineThickness);
    ACount := CalculateCount(R, AIsVertical);
    for I := 0 to ACount - 1 do
    begin
      Result.Combine(R1, roAdd);
      if AIsVertical then
        OffsetRect(R1, 0, 2 * LineThickness)
      else
        OffsetRect(R1, 2 * LineThickness, 0);
    end;
  end;
end;

procedure TdxPSReportCellTreeLineViewInfo.Draw(ACanvas: TdxPSReportRenderCustomCanvas);
var
  ARegion: TcxRegion;
begin
  ARegion := TcxRegion.Create(cxNullRect);
  try
    if not IsRectEmpty(RightRect) then
      ARegion.Combine(CreateTreeLineRegion(RightRect, False), roAdd);
    if not IsRectEmpty(VerticalRect) then
      ARegion.Combine(CreateTreeLineRegion(VerticalRect, True), roAdd);
    if not IsRectEmpty(ButtonRect) then
    begin
      ARegion.Combine(cxRectInflate(ButtonRect, LineThickness, LineThickness),roSubtract);
    end;
    ARegion.Combine(CellButton.BoundsRect, roIntersect);
    Renderer.FillRgn(ACanvas, ARegion.Handle, CellButton.ReportCells.TreeLineColor);
  finally
    ARegion.Free;
  end;
end;

function TdxPSReportCellTreeLineViewInfo.GetCellSides: TdxCellSides;
begin
  Result := CellButton.CellSides;
end;

function TdxPSReportCellTreeLineViewInfo.GetLineThickness: Integer;
begin
  Result := CellButton.Renderer.LineThickness;
end;

function TdxPSReportCellTreeLineViewInfo.GetTreeLineStyle: TdxPSTreeLineStyle;
begin
  Result := CellButton.ReportCells.TreeLineStyle;
end;

function TdxPSReportCellTreeLineViewInfo.GetRenderer: TdxPSReportRenderer;
begin
  Result := CellButton.Renderer;
end;

function TdxPSReportCellTreeLineViewInfo.GetUnitsPerPixel: Integer;
begin
  Result := CellButton.Renderer.UnitsPerPixel;
end;

{ TdxReportCellExpandButton }

constructor TdxReportCellExpandButton.Create(AParent: TdxReportCell);
begin
  inherited Create(AParent);
  ButtonAlignHorz := bahCenter;
  ButtonAlignVert := bavCenter;
  ButtonBorder3D := False;
  ButtonBorder3DSoft := False;
  ButtonBorderShadow := False;
  ButtonExpanded := False;
  ButtonInteriorColor := clNone;
  ButtonSize := dxDefaultCrossSignCrossSize;       // 9
  ButtonTransparent := True;
  KeepOddSize := True;
  ShowButton := False;
  ShowButtonBorder := True;
  TreeLineMode := tlmNone;
end;

procedure TdxReportCellExpandButton.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TdxReportCellExpandButton then
  begin
    ButtonBorder3D := TdxReportCellExpandButton(Source).ButtonBorder3D;
    ButtonBorder3DSoft := TdxReportCellExpandButton(Source).ButtonBorder3DSoft;
    ButtonBorderShadow := TdxReportCellExpandButton(Source).ButtonBorderShadow;
    ButtonExpanded := TdxReportCellExpandButton(Source).ButtonExpanded;
    ButtonInteriorColor := TdxReportCellExpandButton(Source).ButtonInteriorColor;
    ButtonSize := TdxReportCellExpandButton(Source).ButtonSize;
    ButtonTransparent := TdxReportCellExpandButton(Source).ButtonTransparent;
    KeepOddSize := TdxReportCellExpandButton(Source).KeepOddSize;
    ShowButton := TdxReportCellExpandButton(Source).ShowButton;
    ShowButtonBorder := TdxReportCellExpandButton(Source).ShowButtonBorder;
    TreeLineMode := TdxReportCellExpandButton(Source).TreeLineMode;
  end;
end;

procedure TdxReportCellExpandButton.DrawContent(
  ACanvas: TdxPSReportRenderCustomCanvas; AStage: TdxPSRenderStages);
begin
  DrawBackground(ACanvas);
  if ShowButton then
    DrawExpandButton(ACanvas);
  if AreTreeLinesDrawn then
    DrawTreeLines(ACanvas);
  if IsBordersDrawn then
    DrawBorders(ACanvas);
end;

procedure TdxReportCellExpandButton.DrawExpandButton(
  ACanvas: TdxPSReportRenderCustomCanvas);
var
  R: TRect;
begin
  R := GetButtonBounds(ACanvas);
  Renderer.DrawExpandButton(ACanvas, R, ButtonExpanded, ShowButtonBorder,
    ButtonBorder3D, ButtonBorder3DSoft, ButtonBorderShadow, not ButtonTransparent,
    ReportCells.ExpandButtonBorderColor, ButtonInteriorColor);
end;

procedure TdxReportCellExpandButton.DrawTreeLines(ACanvas: TdxPSReportRenderCustomCanvas);
var
  ATreeLinesInfo: TdxPSReportCellTreeLineViewInfo;
begin
  ATreeLinesInfo := TdxPSReportCellTreeLineViewInfo.Create(TreeLineMode, Self);
  try
    ATreeLinesInfo.CalculateRects(BoundsRect, GetButtonBounds(ACanvas));
    ATreeLinesInfo.Draw(ACanvas);
  finally
    ATreeLinesInfo.Free;
  end;
end;

function TdxReportCellExpandButton.GetButtonBounds(
  ACanvas: TdxPSReportRenderCustomCanvas): TRect;
begin
  Result := cxNullRect;
  if ShowButton then
  begin
    Result := CalculateButtonBounds;
    FixupRect(ACanvas, Result);
  end;
end;

procedure TdxReportCellExpandButton.ConvertCoords(
  APixelsNumerator, APixelsDenominator: Integer);
begin
  inherited ConvertCoords(APixelsNumerator, APixelsDenominator);
  ButtonSize := MulDiv(ButtonSize, APixelsNumerator, APixelsDenominator);
end;

function TdxReportCellExpandButton.AreTreeLinesDrawn: Boolean;
begin
  Result := TreeLineMode <> tlmNone;
end;

function TdxReportCellExpandButton.CalculateButtonBounds: TRect;
var
  ButtonSize: Integer;
begin
  Result := BoundsRect;
  with Result do
  begin
    ButtonSize := ActualButtonSize;
    case ButtonAlignHorz of
      bahLeft:
        begin
          Inc(Left, ButtonIndents.Left);
          Right := Left + ButtonSize;
        end;
      bahCenter:
        begin
          Inc(Left, (Right - Left - ButtonSize) div 2);
          Right := Left + ButtonSize;
        end;
      bahRight:
        begin
          Dec(Right, ButtonIndents.Right);
          Left := Right - ButtonSize;
        end;
    end;

    case ButtonAlignVert of
      bavTop:
        begin
          Inc(Top, ButtonIndents.Top);
          Bottom := Top + ButtonSize;
        end;
      bavCenter:
        begin
          Inc(Top, (Bottom - Top - ButtonSize) div 2);
          Bottom := Top + ButtonSize;
        end;
      bavBottom:
        begin
          Dec(Bottom, ButtonIndents.Bottom);
          Top := Bottom - ButtonSize;
        end;
    end;

    if Right < Left then Right := Left;
    if Bottom < Top then Bottom := Top;
  end;
end;

function TdxReportCellExpandButton.GetButtonAlignHorz: TdxReportCellExpandButtonAlignHorz;
begin
  Result := bahCenter;
end;

function TdxReportCellExpandButton.GetButtonAlignVert: TdxReportCellExpandButtonAlignVert;
begin
  Result := bavCenter;
end;

function TdxReportCellExpandButton.GetButtonIndents: TRect;
begin
  if Renderer <> nil then
    Result := Rect(LineThickness, LineThickness, LineThickness, LineThickness)
  else
    Result := cxNullRect;
end;

procedure TdxReportCellExpandButton.SetButtonAlignHorz(Value: TdxReportCellExpandButtonAlignHorz);
begin
end;

procedure TdxReportCellExpandButton.SetButtonAlignVert(Value: TdxReportCellExpandButtonAlignVert);
begin
end;

procedure TdxReportCellExpandButton.ReadData(AReader: TdxPSDataReader);
begin
  inherited ReadData(AReader);
  ButtonInteriorColor := AReader.ReadInteger;
  ButtonSize := AReader.ReadInteger;
end;

procedure TdxReportCellExpandButton.WriteData(AWriter: TdxPSDataWriter);
begin
  inherited WriteData(AWriter);
  AWriter.WriteInteger(ButtonInteriorColor);
  AWriter.WriteInteger(ButtonSize);
end;

function TdxReportCellExpandButton.GetActualButtonSize: Integer;
var
  LineThickness: Integer;
begin
  Result := ButtonSize;

  LineThickness := Renderer.LineThickness;
  if (Result mod LineThickness) > (LineThickness div 2) then
    Inc(Result, LineThickness - (Result mod LineThickness))
  else
    Dec(Result, Result mod LineThickness);
end;

function TdxReportCellExpandButton.GetButtonBorder3D: Boolean;
begin
  Result := GetFormatBit(dxPSGlbl.dxFormatExpandButtonBorder3D);
end;

function TdxReportCellExpandButton.GetButtonBorder3DSoft: Boolean;
begin
  Result := GetFormatBit(dxPSGlbl.dxFormatExpandButtonBorder3DSoft);
end;

function TdxReportCellExpandButton.GetButtonBorderShadow: Boolean;
begin
  Result := GetFormatBit(dxPSGlbl.dxFormatExpandButtonBorderShadow);
end;

function TdxReportCellExpandButton.GetButtonExpanded: Boolean;
begin
  Result := GetFormatBit(dxPSGlbl.dxFormatExpandButtonExpanded);
end;

function TdxReportCellExpandButton.GetButtonTransparent: Boolean;
begin
  Result := GetFormatBit(dxPSGlbl.dxFormatExpandButtonTransparent);
end;

function TdxReportCellExpandButton.GetShowButtonBorder: Boolean;
begin
  Result := GetFormatBit(dxPSGlbl.dxFormatExpandButtonShowBorder);
end;

function TdxReportCellExpandButton.GetKeepOddSize: Boolean;
begin
  Result := GetFormatBit(dxPSGlbl.dxFormatExpandButtonKeepOddSize);
end;

function TdxReportCellExpandButton.GetShowButton: Boolean;
begin
  Result := GetFormatBit(dxPSGlbl.dxFormatExpandButtonVisible);
end;

function TdxReportCellExpandButton.GetTreeLineMode: TdxPSTreeLineMode;
begin
  Result := TdxPSTreeLineMode((Format and dxPSGlbl.dxFormatTreeLineModeMask) shr dxPSGlbl.dxFormatTreeLineModeOffset);
end;

procedure TdxReportCellExpandButton.SetButtonBorder3D(Value: Boolean);
begin
  SetFormatBit(dxPSGlbl.dxFormatExpandButtonBorder3D, Value);
end;

procedure TdxReportCellExpandButton.SetButtonBorder3DSoft(Value: Boolean);
begin
  SetFormatBit(dxPSGlbl.dxFormatExpandButtonBorder3DSoft, Value);
end;

procedure TdxReportCellExpandButton.SetButtonBorderShadow(Value: Boolean);
begin
  SetFormatBit(dxPSGlbl.dxFormatExpandButtonBorderShadow, Value);
end;

procedure TdxReportCellExpandButton.SetButtonExpanded(Value: Boolean);
begin
  SetFormatBit(dxPSGlbl.dxFormatExpandButtonExpanded, Value);
end;

procedure TdxReportCellExpandButton.SetButtonTransparent(Value: Boolean);
begin
  SetFormatBit(dxPSGlbl.dxFormatExpandButtonTransparent, Value);
end;

procedure TdxReportCellExpandButton.SetKeepOddSize(Value: Boolean);
begin
  SetFormatBit(dxPSGlbl.dxFormatExpandButtonKeepOddSize, Value);
end;

procedure TdxReportCellExpandButton.SetShowButton(Value: Boolean);
begin
  SetFormatBit(dxPSGlbl.dxFormatExpandButtonVisible, Value);
end;

procedure TdxReportCellExpandButton.SetShowButtonBorder(Value: Boolean);
begin
  SetFormatBit(dxPSGlbl.dxFormatExpandButtonShowBorder, Value);
end;

procedure TdxReportCellExpandButton.SetTreeLineMode(Value: TdxPSTreeLineMode);
begin
  Format := Format and not dxPSGlbl.dxFormatTreeLineModeMask or (Byte(Value) shl dxPSGlbl.dxFormatTreeLineModeOffset);
end;

{ TdxReportCellExpandButtonEx }

function TdxReportCellExpandButtonEx.GetButtonAlignHorz: TdxReportCellExpandButtonAlignHorz;
begin
  Result := TdxReportCellExpandButtonAlignHorz((FormatEx and dxPSGlbl.dxFormatExButtonAlignHorzMask) shr dxPSGlbl.dxFormatExButtonAlignHorzOffset);
end;

function TdxReportCellExpandButtonEx.GetButtonAlignVert: TdxReportCellExpandButtonAlignVert;
begin
  Result := TdxReportCellExpandButtonAlignVert((FormatEx and dxPSGlbl.dxFormatExButtonAlignVertMask) shr dxPSGlbl.dxFormatExButtonAlignVertOffset);
end;

function TdxReportCellExpandButtonEx.GetButtonIndents: TRect;
begin
  Result := inherited GetButtonIndents;
  if Renderer <> nil then
    with Result do
    begin
      Inc(Top, 5 * LineThickness);
      Inc(Bottom, 5 * LineThickness);
    end;
end;

procedure TdxReportCellExpandButtonEx.SetButtonAlignHorz(Value: TdxReportCellExpandButtonAlignHorz);
begin
  FormatEx := FormatEx and not dxPSGlbl.dxFormatExButtonAlignHorzMask or (Byte(Value) shl dxPSGlbl.dxFormatExButtonAlignHorzOffset);
end;

procedure TdxReportCellExpandButtonEx.SetButtonAlignVert(Value: TdxReportCellExpandButtonAlignVert);
begin
  FormatEx := FormatEx and not dxPSGlbl.dxFormatExButtonAlignVertMask or (Byte(Value) shl dxPSGlbl.dxFormatExButtonAlignVertOffset);
end;

procedure TdxReportCellExpandButtonEx.ReadData(AReader: TdxPSDataReader);
begin
  inherited;
  FormatEx := AReader.ReadInteger;
end;

procedure TdxReportCellExpandButtonEx.WriteData(AWriter: TdxPSDataWriter);
begin
  inherited;
  AWriter.WriteInteger(FormatEx);
end;

{ TdxPSExplorerChangeNotifier }

constructor TdxPSExplorerChangeNotifier.Create(AnExplorer: TCustomdxPSExplorer);
begin
  inherited Create;
  Explorer := AnExplorer;
end;

destructor TdxPSExplorerChangeNotifier.Destroy;
begin
  Explorer := nil;
  inherited Destroy;
end;

procedure TdxPSExplorerChangeNotifier.SetExplorer(Value: TCustomdxPSExplorer);
begin
  if FExplorer <> Value then
  begin
    if FExplorer <> nil then
      FExplorer.UnregisterNotifier(Self);
    if Value <> nil then
      Value.RegisterNotifier(Self);
  end;
end;

{ TdxPSExplorerChangeNotifierAdapter }

procedure TdxPSExplorerChangeNotifierAdapter.ExplorerRefresh(AStage: TdxPSExplorerRefreshStage);
begin
end;

procedure TdxPSExplorerChangeNotifierAdapter.FolderPopulated(AFolder: TdxPSExplorerFolder);
begin
end;

procedure TdxPSExplorerChangeNotifierAdapter.ItemAdded(AnItem: TCustomdxPSExplorerItem);
begin
end;

procedure TdxPSExplorerChangeNotifierAdapter.ItemDataLoaded(AnItem: TdxPSExplorerItem);
begin
end;

procedure TdxPSExplorerChangeNotifierAdapter.ItemDataUnloaded(AnItem: TdxPSExplorerItem);
begin
end;

procedure TdxPSExplorerChangeNotifierAdapter.ItemDeleted(AnItem: TCustomdxPSExplorerItem);
begin
end;

procedure TdxPSExplorerChangeNotifierAdapter.ItemParentChanged(AnItem: TCustomdxPSExplorerItem);
begin
end;

procedure TdxPSExplorerChangeNotifierAdapter.ItemPropertiesChanged(AnItem: TCustomdxPSExplorerItem);
begin
end;

procedure TdxPSExplorerChangeNotifierAdapter.ItemRenamed(AnItem: TCustomdxPSExplorerItem);
begin
end;

{ TdxPSExplorerTreeChangeNotifier }

constructor TdxPSExplorerTreeChangeNotifier.Create(ATreeContainer: TCustomdxPSExplorerTreeContainer;
  ARegister: Boolean = True);
begin
  Assert(ATreeContainer <> nil);
  FTreeContainer := ATreeContainer;
  inherited Create(TreeContainer.Explorer);
end;

procedure TdxPSExplorerTreeChangeNotifier.ExplorerRefresh(AStage: TdxPSExplorerRefreshStage);
begin
  case AStage of
    ersBefore:
      begin
        TreeContainer.EndEdit(True);
        TreeContainer.BeginUpdate;
        TreeContainer.SaveState;
      end;
    ersAfter:
      begin
        if Explorer <> nil then Explorer.BuildTree(TreeContainer);
        TreeContainer.RestoreState;
        TreeContainer.RefreshSorting(nil);
        TreeContainer.EndUpdate;
      end;
  end;
end;

procedure TdxPSExplorerTreeChangeNotifier.FolderPopulated(AFolder: TdxPSExplorerFolder);
begin
  if Explorer <> nil then Explorer.PopulateTreeFolder(TreeContainer, AFolder);
end;

procedure TdxPSExplorerTreeChangeNotifier.ItemAdded(AnItem: TCustomdxPSExplorerItem);
begin
  with TreeContainer do
  begin
    AddItem(CreationParent, AnItem);
    SelectedItem := AnItem;
    if Control.Focused then BeginEdit(True);
  end;
end;

procedure TdxPSExplorerTreeChangeNotifier.ItemDataLoaded(AnItem: TdxPSExplorerItem);
begin
  TreeContainer.MakeItemVisible(AnItem);
  TreeContainer.ItemDataLoaded(AnItem);
end;

procedure TdxPSExplorerTreeChangeNotifier.ItemDataUnloaded(AnItem: TdxPSExplorerItem);
begin
  TreeContainer.ItemDataUnloaded(AnItem);
end;

procedure TdxPSExplorerTreeChangeNotifier.ItemDeleted(AnItem: TCustomdxPSExplorerItem);
begin
  TreeContainer.DeleteItem(AnItem);
end;

procedure TdxPSExplorerTreeChangeNotifier.ItemParentChanged(AnItem: TCustomdxPSExplorerItem);
begin
  TreeContainer.MoveItem(AnItem);
end;

procedure TdxPSExplorerTreeChangeNotifier.ItemPropertiesChanged(AnItem: TCustomdxPSExplorerItem);
begin
  if TreeContainer.SelectedItem = AnItem then
    TreeContainer.SelectedItemText := AnItem.DisplayName;
end;

procedure TdxPSExplorerTreeChangeNotifier.ItemRenamed(AnItem: TCustomdxPSExplorerItem);
begin
  TreeContainer.RenameItem(AnItem);
end;

{ TCustomdxPSExplorerTreeContainer }

constructor TCustomdxPSExplorerTreeContainer.Create(AnExplorer: TCustomdxPSExplorer;
  AHost: IdxPSExplorerTreeContainerHost);
begin
  Assert(AnExplorer <> nil);
  Assert(AHost <> nil);

  inherited Create;
  FExplorer := AnExplorer;
  FHost := AHost;

  CreateTreeContainer;
  FChangeNotifier := TdxPSExplorerTreeChangeNotifier.Create(Self, True);
end;

destructor TCustomdxPSExplorerTreeContainer.Destroy;
begin
  FreeAndNil(FControl);
  FreeAndNil(FChangeNotifier);
  inherited Destroy;
end;

class function TCustomdxPSExplorerTreeContainer.ControlClass: TWinControlClass;
begin
  Result := nil;
end;

class procedure TCustomdxPSExplorerTreeContainer.Register;
begin
  dxPSExplorerTreeContainerFactory.Register(Self);
end;

class procedure TCustomdxPSExplorerTreeContainer.Unregister;
begin
  dxPSExplorerTreeContainerFactory.Unregister(Self);
end;

procedure TCustomdxPSExplorerTreeContainer.BeginUpdate;
begin
end;

procedure TCustomdxPSExplorerTreeContainer.EndUpdate;
begin
end;

function TCustomdxPSExplorerTreeContainer.CanCreateFolder: Boolean;
begin
  Result := Explorer.CanCreateFolder and not IsEditing and IsFolderSelected;
end;

function TCustomdxPSExplorerTreeContainer.CanCreateItem: Boolean;
begin
  Result := Explorer.CanCreateItem and not IsEditing;
end;

function TCustomdxPSExplorerTreeContainer.CanDeleteSelection: Boolean;
begin
  Result := not IsEditing and Explorer.CanDelete(SelectedItem);
end;

function TCustomdxPSExplorerTreeContainer.CanLoadSelectedItemData: Boolean;
begin
  Result := (Control <> nil) and Control.Focused and not IsEditing and
    IsItemSelected and TdxPSExplorerItem(SelectedItem).CanLoadData;
end;

function TCustomdxPSExplorerTreeContainer.CanRefresh: Boolean;
begin
  Result := not IsEditing;
end;

function TCustomdxPSExplorerTreeContainer.CanRenameSelectedItem: Boolean;
begin
  Result := not IsEditing and Explorer.CanRename(SelectedItem);
end;

function TCustomdxPSExplorerTreeContainer.CanShowPropertySheetsForSelectedItem: Boolean;
begin
  Result := not IsEditing and (SelectedItem <> nil) and SelectedItem.HasPropertySheets;
end;

function TCustomdxPSExplorerTreeContainer.CanUnloadItemData: Boolean;
begin
  Result := not IsEditing and (Host.ReportLink <> nil) and
    Host.ReportLink.CanUnloadData and (Explorer.LoadedItem <> nil);
end;

function TCustomdxPSExplorerTreeContainer.CreateItem: TdxPSExplorerItem;
begin
  if CanCreateItem then
    Result := Explorer.CreateNewItem(CreationParent, Host.ReportLink)
  else
    Result := nil;
end;

procedure TCustomdxPSExplorerTreeContainer.DeleteSelection(AShowMessage: Boolean = True);
begin
  if CanDeleteSelection and (not AShowMessage or dxPSUtl.MessageQuestion(SelectedItem.DeleteMessageText)) then
    SelectedItem.Delete;
end;

function TCustomdxPSExplorerTreeContainer.IsSelectedItemCurrentlyLoaded: Boolean;
var
  Item: TCustomdxPSExplorerItem;
begin
  Item := SelectedItem;
  Result := (Item is TdxPSExplorerItem) and TdxPSExplorerItem(Item).IsCurrentlyLoaded;
end;

procedure TCustomdxPSExplorerTreeContainer.LoadSelectedItemData;
begin
  if CanLoadSelectedItemData then
  begin
    Explorer.LoadItemData(TdxPSExplorerItem(SelectedItem), Host.ReportLink);
    Control.Invalidate;
  end;
end;

procedure TCustomdxPSExplorerTreeContainer.RenameSelectedItem;
begin
  if CanRenameSelectedItem then BeginEdit(True);
end;

function TCustomdxPSExplorerTreeContainer.ShowSelectedItemPropertySheets: Boolean;
begin
  Result := CanShowPropertySheetsForSelectedItem and TdxPSExplorerItem(SelectedItem).ShowPropertySheets;
end;

procedure TCustomdxPSExplorerTreeContainer.UnloadItemData;
begin
  if CanUnloadItemData then
  begin
    Explorer.UnloadItemData(Explorer.LoadedItem);
    Control.Invalidate;
  end;
end;

function TCustomdxPSExplorerTreeContainer.CanFocus: Boolean;
begin
  Result := (Control <> nil) and Control.CanFocus;
end;

procedure TCustomdxPSExplorerTreeContainer.SetFocus;
begin
  if (Control <> nil) and Control.CanFocus and not IsRectEmpty(Control.ClientRect) then
    Control.SetFocus;
end;

procedure TCustomdxPSExplorerTreeContainer.RefreshSorting(ANode: TObject);
begin
end;

procedure TCustomdxPSExplorerTreeContainer.RefreshSorting(AFolder: TdxPSExplorerFolder);
begin

end;

procedure TCustomdxPSExplorerTreeContainer.DeleteItem(AnItem: TCustomdxPSExplorerItem);
begin
  EndEdit(False);
end;

procedure TCustomdxPSExplorerTreeContainer.InvalidateItem(AnItem: TCustomdxPSExplorerItem);
begin
end;

procedure TCustomdxPSExplorerTreeContainer.RestoreState;
begin
end;

procedure TCustomdxPSExplorerTreeContainer.SaveState;
begin
end;

procedure TCustomdxPSExplorerTreeContainer.CreateTreeContainer;
begin
  if FControl = nil then
  begin
    FControl := ControlClass.Create(nil);
    FControl.Parent := Host.TreeContainerParent;
    InitializeTreeContainer;
  end;
end;

procedure TCustomdxPSExplorerTreeContainer.InitializeTreeContainer;
begin
end;

procedure TCustomdxPSExplorerTreeContainer.ProcessKeyDown(var Key: Word; Shift: TShiftState);
var
  PopupMenu: TPopupMenu;
  MenuItem: TMenuItem;
begin
  PopupMenu := dxPSUtl.Control_GetPopupMenu(Control);
  if PopupMenu <> nil then
  begin
    MenuItem := PopupMenu.FindItem(Key, fkShortCut);
    if (MenuItem <> nil) and MenuItem.Enabled and MenuItem.Visible then
    begin
      MenuItem.Click;
      Key := 0;
      Exit;
    end
  end;

  if not IsEditing then
    case Key of
      VK_F2:
        if cxShiftStateMoveOnly(Shift) then BeginEdit(True);
      VK_F5:
        if cxShiftStateMoveOnly(Shift) then Explorer.Refresh;
      VK_DELETE:
        if cxShiftStateMoveOnly(Shift) then DeleteSelection(True);
      VK_ESCAPE:
        Host.UpdateState;
      VK_RETURN:
        if ssCtrl in Shift then LoadSelectedItemData;
    end;
end;

procedure TCustomdxPSExplorerTreeContainer.ProcessKeyPress(var Key: Char);
begin
  if IsEditing and not Explorer.AcceptItemNameChar(SelectedItem, Key) then
    Key := #0;
end;

{ TdxPSExplorerTreeBuilder }

class procedure TdxPSExplorerTreeBuilder.Register;
begin
  dxPSExplorerTreeBuilderFactory.Register(Self);
end;

class procedure TdxPSExplorerTreeBuilder.Unregister;
begin
  dxPSExplorerTreeBuilderFactory.Unregister(Self);
end;

class procedure TdxPSExplorerTreeBuilder.BuildTree(AnExplorer: TCustomdxPSExplorer;
  ATreeContainer: TCustomdxPSExplorerTreeContainer);
begin
  with ATreeContainer do
  begin
    BeginUpdate;
    try
      Clear;
      CreateFolderNode(ATreeContainer, nil, AnExplorer.Root);
      ExpandItem(AnExplorer.Root);
      SelectedItem := AnExplorer.Root;
    finally
      EndUpdate;
    end;
  end;
end;

class procedure TdxPSExplorerTreeBuilder.CreateFolderNode(ATreeContainer: TCustomdxPSExplorerTreeContainer;
  AParent, AFolder: TdxPSExplorerFolder);
begin
  CreateItemNode(ATreeContainer, AParent, AFolder);
  PopulateTreeFolder(ATreeContainer, AFolder);
end;

class procedure TdxPSExplorerTreeBuilder.CreateItemNode(ATreeContainer: TCustomdxPSExplorerTreeContainer;
  AParent: TdxPSExplorerFolder; AnItem: TCustomdxPSExplorerItem);
begin
  ATreeContainer.AddItem(AParent, AnItem);
end;

class procedure TdxPSExplorerTreeBuilder.PopulateTreeFolder(ATreeContainer: TCustomdxPSExplorerTreeContainer;
  AFolder: TdxPSExplorerFolder);
var
  I: Integer;
begin
  with ATreeContainer do
  begin
    BeginUpdate;
    try
      for I := 0 to AFolder.FolderCount - 1 do
        CreateFolderNode(ATreeContainer, AFolder, AFolder.Folders[I]);
      for I := 0 to AFolder.ItemCount - 1 do
        CreateItemNode(ATreeContainer, AFolder, AFolder.Items[I]);
    finally
      EndUpdate;
    end;
    //RefreshSorting(AFolder); {3.1}
  end;
end;

{ TdxPSExplorerItemPropertySheets }

constructor TCustomdxPSExplorerItemPropertySheets.CreateEx(AnExplorerItem: TCustomdxPSExplorerItem);
begin
  Create(nil);
  FExplorerItem := AnExplorerItem;
end;

class function TCustomdxPSExplorerItemPropertySheets.Execute(AnExplorerItem: TCustomdxPSExplorerItem): Boolean;
begin
  with FormClass.CreateEx(AnExplorerItem) do
  try
    Initialize;
    try
      Result := ShowModal = mrOK;
    finally
      Done;
    end;
  finally
    Free;
  end;
end;

function TCustomdxPSExplorerItemPropertySheets.ExplorerItem: TCustomdxPSExplorerItem;
begin
  Result := FExplorerItem;
end;

procedure TCustomdxPSExplorerItemPropertySheets.Done;
begin
end;

procedure TCustomdxPSExplorerItemPropertySheets.Initialize;
begin
end;

class function TCustomdxPSExplorerItemPropertySheets.FormClass: TCustomdxPSExplorerItemPropertySheetsClass;
begin
  Result := TCustomdxPSExplorerItemPropertySheetsClass(Self);
end;

{ TCustomdxPSExplorerItemComparator }

class function TCustomdxPSExplorerItemComparator.CompareItems(
  AnItem1, AnItem2: Pointer): Integer;
begin
  Result := TCustomdxPSExplorerItem(AnItem1).CompareTo(TCustomdxPSExplorerItem(AnItem2));
end;

{ TCustomdxPSExplorerItemHelper }

class function TCustomdxPSExplorerItemHelper.GetHasChildren(AFolder: TdxPSExplorerFolder): Boolean;
begin
  Result := False;
end;

class function TCustomdxPSExplorerItemHelper.GetImageIndex(AnItem: TCustomdxPSExplorerItem): Integer;
begin
  Result := AnItem.GetImageIndex;
end;

class function TCustomdxPSExplorerItemHelper.GetSelectedIndex(AnItem: TCustomdxPSExplorerItem): Integer;
begin
  Result := AnItem.GetSelectedIndex;
end;

class procedure TCustomdxPSExplorerItemHelper.SetHasChildren(AFolder: TdxPSExplorerFolder; Value: Boolean);
begin
end;

{ TCustomdxPSExplorerItem }

constructor TCustomdxPSExplorerItem.Create(AnExplorer: TCustomdxPSExplorer;
  AParent: TdxPSExplorerFolder);
begin
  inherited Create;
  Assert(AnExplorer <> nil);
  FExplorer := AnExplorer;
  Parent := AParent;
  FName := GetNewName(nil);
  FWindowHandle := dxPSUtl.dxAllocatehWnd(WndProc);
end;

destructor TCustomdxPSExplorerItem.Destroy;
begin
  dxPSUtl.dxDeallocatehWnd(FWindowHandle);
  if Parent <> nil then Parent.Remove(Self);
  inherited;
end;

function TCustomdxPSExplorerItem.Explorer: TCustomdxPSExplorer;
begin
  Result := FExplorer;
end;

function TCustomdxPSExplorerItem.CanAccept(AnItem: TCustomdxPSExplorerItem): Boolean;
begin
  Result := False;
end;

function TCustomdxPSExplorerItem.CanDelete: Boolean;
begin
  Result := True;
end;

function TCustomdxPSExplorerItem.CanMove: Boolean;
begin
  Result := (Explorer = nil) or Explorer.CanMove(Self);
end;

function TCustomdxPSExplorerItem.CanMoveTo(AParent: TCustomdxPSExplorerItem): Boolean;
begin
  Result := ((Explorer = nil) or Explorer.CanMoveTo(Self, AParent)) and
    (AParent <> nil) and AParent.CanAccept(Self);
end;

function TCustomdxPSExplorerItem.CanRename: Boolean;
begin
  Result := (Explorer = nil) or Explorer.CanRename(Self);
end;

function TCustomdxPSExplorerItem.CanRenameTo(const AName: string): Boolean;
begin
  Result := (Explorer = nil) or Explorer.CanRenameTo(Self, AName);
end;

procedure TCustomdxPSExplorerItem.Delete;
begin
  if CanDelete and (Explorer <> nil) then
    Explorer.Delete(Self);
end;

function TCustomdxPSExplorerItem.GetUniqueID(out AnUniqueID: TBytes): Integer;
begin
  SetLength(AnUniqueID, 0);
  Result := 0;
end;

function TCustomdxPSExplorerItem.HasAsParent(AnItem: TCustomdxPSExplorerItem): Boolean;
var
  ItemParent: TCustomdxPSExplorerItem;
begin
  Result := not ((AnItem = nil) or (Parent = Self));
  if Result then
  begin
    ItemParent := Parent;
    while (ItemParent <> nil) and (ItemParent <> AnItem) do
      ItemParent := ItemParent.Parent;
    Result := ItemParent <> nil;
  end;
end;

function TCustomdxPSExplorerItem.IsNameChanged(const ANewName: string): Boolean;
begin
  Result := not dxSameStr(ANewName, Name);
end;

class function TCustomdxPSExplorerItem.HasPropertySheets: Boolean;
begin
  Result := PropertySheetsClass <> nil;
end;

class function TCustomdxPSExplorerItem.PropertySheetsClass: TCustomdxPSExplorerItemPropertySheetsClass;
begin
  Result := nil;
end;

function TCustomdxPSExplorerItem.ShowPropertySheets: Boolean;
begin
  Result := HasPropertySheets and PropertySheetsClass.Execute(Self);
end;

function TCustomdxPSExplorerItem.CannotRenameMessageText(const AOldName, ANewName: string): string;
begin
  Result := '';
end;

function TCustomdxPSExplorerItem.DeleteMessageText: string;
begin
  Result := '';
end;

function TCustomdxPSExplorerItem.OverwriteMessageText(Dest: TCustomdxPSExplorerItem): string;
begin
  Result := '';
end;

function TCustomdxPSExplorerItem.GetDisplayName: string;
begin
  Result := Name;
end;

function TCustomdxPSExplorerItem.CompareTo(AnItem: TCustomdxPSExplorerItem): Integer;
begin
  Result := 0;
end;

function TCustomdxPSExplorerItem.DoDelete: Boolean;
begin
  Result := True;
end;

function TCustomdxPSExplorerItem.DoMove(AParent: TdxPSExplorerFolder): Boolean;
begin
  Result := True;
end;

function TCustomdxPSExplorerItem.DoRename(var ANewName: string): Boolean;
begin
  Result := True;
end;

function TCustomdxPSExplorerItem.GetImageIndex: Integer;
begin
  Result := -1;
end;

function TCustomdxPSExplorerItem.GetInfoTip: string;
begin
  Result := '';
end;

function TCustomdxPSExplorerItem.GetNewName(AReportLink: TBasedxReportLink): string;
begin
  Result := '';
end;

function TCustomdxPSExplorerItem.GetSelectedIndex: Integer;
begin
  Result := -1;
end;

procedure TCustomdxPSExplorerItem.InternalDelete;
begin
  if IsWindow(FWindowHandle) then
    PostMessage(FWindowHandle, DXM_PS_FREEEXPLORERITEM, 0, 0);
end;

procedure TCustomdxPSExplorerItem.InternalMove(AParent: TdxPSExplorerFolder);
begin
  if Parent <> nil then Parent.Remove(Self);
  if AParent <> nil then AParent.Add(Self);
end;

procedure TCustomdxPSExplorerItem.InternalRename(const AName: string);
begin
  FName := AName;
end;

procedure TCustomdxPSExplorerItem.SetName(const Value: string);
begin
  if (FName <> Value) and CanRenameTo(Value) then
    Explorer.RenameTo(Self, Value);
end;

function GetItemStateInfoSize: Integer;
begin
  Result := SizeOf(TCustomdxPSExplorerItemStateInfo);
end;

function TCustomdxPSExplorerItem.GetItemStateInfo: TCustomdxPSExplorerItemStateInfo;
var
  Bytes: TBytes;
begin
  FillChar(Result, GetItemStateInfoSize, 0);
  Result.Count := 0;
  Result.UniqueIDSize := GetUniqueID(Bytes);
end;

procedure TCustomdxPSExplorerItem.WriteState(AStream: TStream);
var
  ItemInfo: TCustomdxPSExplorerItemStateInfo;
  Bytes: TBytes;
begin
  ItemInfo := GetItemStateInfo;
  AStream.WriteBuffer(ItemInfo, GetItemStateInfoSize);
  if ItemInfo.UniqueIDSize <> 0 then
  begin
    GetUniqueID(Bytes);
    AStream.WriteBuffer(Pointer(Bytes)^, ItemInfo.UniqueIDSize);
  end;
end;

procedure TCustomdxPSExplorerItem.WndProc(var Message: TMessage);
begin
  with Message do
    if Msg = DXM_PS_FREEEXPLORERITEM then
      Free
    else
      Result := DefWindowProc(FWindowHandle, Msg, wParam, lParam);
end;

procedure TCustomdxPSExplorerItem.SetParent(Value: TdxPSExplorerFolder);
begin
  if (FParent <> Value) and CanMoveTo(Value) then
    Explorer.MoveTo(Self, Value);
end;

{ TdxPSExplorerFolderHelper }

class function TdxPSExplorerFolderHelper.GetHasChildren(AFolder: TdxPSExplorerFolder): Boolean;
begin
  Result := AFolder.HasChildren;
end;

class procedure TdxPSExplorerFolderHelper.SetHasChildren(AFolder: TdxPSExplorerFolder;
  Value: Boolean);
begin
  AFolder.HasChildren := Value;
end;

{ TdxPSExplorerFolder }

constructor TdxPSExplorerFolder.Create(AnExplorer: TCustomdxPSExplorer;
  AParent: TdxPSExplorerFolder);
begin
  inherited Create(AnExplorer, AParent);
  FFolders := TList.Create;
  FItems := TList.Create;
end;

destructor TdxPSExplorerFolder.Destroy;
begin
  FreeAndNilFolders;
  FreeAndNilItems;
  inherited Destroy;
end;

function TdxPSExplorerFolder.CanAccept(AnItem: TCustomdxPSExplorerItem): Boolean;
begin
  Result := (AnItem <> Self) and (AnItem.Parent <> Self) and not HasAsParent(AnItem);
end;

function TdxPSExplorerFolder.CanRenameTo(const AName: string): Boolean;
begin
  Result := inherited CanRenameTo(AName) and ((Parent = nil) or (Parent.FolderByName(AName) = nil));
end;

function TdxPSExplorerFolder.CreateFolder: TdxPSExplorerFolder;
begin
  Result := Explorer.CreateNewFolder(Self);
end;

function TdxPSExplorerFolder.CreateItem(AReportLink: TBasedxReportLink): TdxPSExplorerItem;
begin
  Result := Explorer.CreateNewItem(Self, AReportLink);
end;

procedure TdxPSExplorerFolder.Populate;
begin
  Explorer.PopulateFolder(Self);
end;

procedure TdxPSExplorerFolder.Delete;
begin
  if CanDelete then
  begin
    DeleteFolders;
    DeleteItems;
  end;
  inherited Delete;
end;

procedure TdxPSExplorerFolder.DeleteFolders;
var
  I: Integer;
begin
  for I := FolderCount - 1 downto 0 do
    Folders[I].Delete;
end;

procedure TdxPSExplorerFolder.DeleteItems;
var
  I: Integer;
begin
  for I := ItemCount - 1 downto 0 do
    Items[I].Delete;
end;

function TdxPSExplorerFolder.HasFolders: Boolean;
begin
  Result := FolderCount <> 0;
end;

function TdxPSExplorerFolder.HasItems: Boolean;
begin
  Result := ItemCount <> 0;
end;

function TdxPSExplorerFolder.HasLoadedItem: Boolean;
var
  I: Integer;
begin
  if Explorer.LoadedItem <> nil then
  begin
    Result := True;
    for I := 0 to ItemCount - 1 do
      if Items[I] = Explorer.LoadedItem then Exit;
    for I := 0 to FolderCount - 1 do
      if Folders[I].HasLoadedItem then Exit;
    Result := False;
  end
  else
    Result := False;
end;

function TdxPSExplorerFolder.FolderByName(const AName: string): TdxPSExplorerFolder;
var
  I: Integer;
begin
  Populate;
  for I := 0 to FolderCount - 1 do
  begin
    Result := Folders[I];
    if Result.Name = AName then Exit;
  end;
  Result := nil;
end;

function TdxPSExplorerFolder.ItemByName(const AName: string): TdxPSExplorerItem;
var
  I: Integer;
begin
  for I := 0 to ItemCount - 1 do
  begin
    Result := Items[I];
    if Result.Name = AName then Exit;
  end;
  Result := nil;
end;

function TdxPSExplorerFolder.CannotRenameMessageText(const AOldName, ANewName: string): string;
begin
  Result := Format(cxGetResourceString(@sdxCannotRenameFolderText), [AOldName, ANewName]);
end;

function TdxPSExplorerFolder.DeleteMessageText: string;
begin
  if HasFolders or HasItems then
    Result := Format(cxGetResourceString(@sdxDeleteNonEmptyFolderMessageText), [DisplayName])
  else
    Result := Format(cxGetResourceString(@sdxDeleteFolderMessageText), [DisplayName]);
end;

function TdxPSExplorerFolder.OverwriteMessageText(Dest: TCustomdxPSExplorerItem): string;
begin
  Result := Format(cxGetResourceString(@sdxOverwriteFolderMessageText), [Dest.DisplayName, DisplayName]);
end;

function TdxPSExplorerFolder.CompareTo(AnItem: TCustomdxPSExplorerItem): Integer;
begin
  if AnItem is TdxPSExplorerFolder then
    Result := CompareText(DisplayName, AnItem.DisplayName)
  else
    Result := -1;
end;

function TdxPSExplorerFolder.GetItemStateInfo: TCustomdxPSExplorerItemStateInfo;
begin
  Result := inherited GetItemStateInfo;
  Result.Count := FolderCount + ItemCount;
end;

procedure TdxPSExplorerFolder.WriteState(AStream: TStream);
var
  I: Integer;
begin
  inherited;
  for I := 0 to FolderCount - 1 do
    Folders[I].WriteState(AStream);
  for I := 0 to ItemCount - 1 do
    Items[I].WriteState(AStream);
end;

procedure TdxPSExplorerFolder.LoadData;
begin
  Explorer.LoadData(Self);
end;

function TdxPSExplorerFolder.GetImageIndex: Integer;
begin
  Result := iiExplorerFolderCollapsed;
end;

function TdxPSExplorerFolder.GetNewName(AReportLink: TBasedxReportLink): string;
var
  Index: Integer;
  Template: string;
begin
  Result := cxGetResourceString(@sdxNewExplorerFolderItem);
  if (Parent <> nil) and (Parent.FolderByName(Result) <> nil) then
  begin
    Index := 1;
    Template := Result;
    repeat
      Inc(Index);
      Result := Template + ' (' + IntToStr(Index) + ')';
    until Parent.FolderByName(Result) = nil;
  end;
end;

function TdxPSExplorerFolder.GetSelectedIndex: Integer;
begin
  Result := iiExplorerFolderExpanded;
end;

procedure TdxPSExplorerFolder.Add(AnItem: TCustomdxPSExplorerItem);
begin
  GetItemList(AnItem).Add(AnItem);
  AnItem.FParent := Self;
  HasChildren := True;
end;

procedure TdxPSExplorerFolder.Remove(AnItem: TCustomdxPSExplorerItem);
begin
  AnItem.FParent := nil;
  GetItemList(AnItem).Remove(AnItem);
  if FolderCount + ItemCount = 0 then
    HasChildren := False;
end;

procedure TdxPSExplorerFolder.FreeAndNilFolders;
var
  I: Integer;
begin
  for I := FolderCount - 1 downto 0 do
    Folders[I].Free;
  FreeAndNil(FFolders);
end;

procedure TdxPSExplorerFolder.FreeAndNilItems;
var
  I: Integer;
begin
  for I := ItemCount - 1 downto 0 do
    Items[I].Free;
  FreeAndNil(FItems);
end;

function TdxPSExplorerFolder.GetFolder(Index: Integer): TdxPSExplorerFolder;
begin
  Result := TdxPSExplorerFolder(FFolders[Index]);
end;

function TdxPSExplorerFolder.GetFolderCount: Integer;
begin
  if FFolders <> nil then
    Result := FFolders.Count
  else
    Result := 0;
end;

function TdxPSExplorerFolder.GetHasChildren: Boolean;
begin
  Result := FHasChildren or (FolderCount + ItemCount <> 0);
end;

function TdxPSExplorerFolder.GetIsRoot: Boolean;
begin
  Result := Self = Explorer.Root;
end;

function TdxPSExplorerFolder.GetItem(Index: Integer): TdxPSExplorerItem;
begin
  Result := TdxPSExplorerItem(FItems[Index]);
end;

function TdxPSExplorerFolder.GetItemCount: Integer;
begin
  if FItems <> nil then
    Result := FItems.Count
  else
    Result := 0;
end;

function TdxPSExplorerFolder.GetItemList(AnItem: TCustomdxPSExplorerItem): TList;
begin
  Result := GetItemList(TCustomdxPSExplorerItemClass(AnItem.ClassType));
end;

function TdxPSExplorerFolder.GetItemList(AnItemClass: TCustomdxPSExplorerItemClass): TList;
begin
  if AnItemClass.InheritsFrom(TdxPSExplorerItem) then
    Result := FItems
  else
    if AnItemClass.InheritsFrom(TdxPSExplorerFolder) then
      Result := FFolders
    else
      Result := nil;
end;

procedure TdxPSExplorerFolder.SetHasChildren(Value: Boolean);
begin
  FHasChildren := Value or (FolderCount + ItemCount <> 0);
end;

{ TdxPSExplorerItem }

constructor TdxPSExplorerItem.Create(AnExplorer: TCustomdxPSExplorer; AParent: TdxPSExplorerFolder);
begin
  inherited;
  FReportDocument := TdxPSReportDocument.Create(nil);
  FReportDocument.OnChanged := DocumentChanged;
end;

destructor TdxPSExplorerItem.Destroy;
begin
  FreeAndNil(FReportDocument);
  inherited;
end;

function TdxPSExplorerItem.CanLoadData: Boolean;
begin
  Result := not HasInvalidData and not IsCurrentlyLoaded;
end;

function TdxPSExplorerItem.CanRenameTo(const AName: string): Boolean;
begin
  Result := inherited CanRenameTo(AName) and ((Parent = nil) or (Parent.ItemByName(AName) = nil));
end;

function TdxPSExplorerItem.CannotRenameMessageText(const AOldName, ANewName: string): string;
begin
  Result := Format(cxGetResourceString(@sdxCannotRenameItemText), [AOldName, ANewName]);
end;

function TdxPSExplorerItem.DataLoadErrorText: string;
begin
  Result := cxGetResourceString(@sdxDataLoadErrorText);
end;

function TdxPSExplorerItem.DeleteMessageText: string;
begin
  Result := Format(cxGetResourceString(@sdxDeleteItemMessageText), [DisplayName]);
end;

function TdxPSExplorerItem.OverwriteMessageText(Dest: TCustomdxPSExplorerItem): string;
begin
  Result := Format(cxGetResourceString(@sdxOverwriteItemMessageText), [Dest.DisplayName, DisplayName]);
end;

function TdxPSExplorerItem.CreateDataStream(AMode: TdxPSStreamMode): TStream;
begin
  Result := Explorer.CreateItemDataStream(Self, AMode);
end;

procedure TdxPSExplorerItem.RetrieveReportData(AReportLink: TBasedxReportLink);
var
  Stream: TStream;
begin
  if AReportLink <> nil then
  begin
    Stream := CreateDataStream(smWrite);
    if Stream <> nil then
    try
      AReportLink.SaveDataToStream(Stream);
    finally
      Stream.Free;
    end;
  end;
end;

function TdxPSExplorerItem.IsLoading: Boolean;
begin
  Result := (Explorer <> nil) and Explorer.IsLoading;
end;

procedure TdxPSExplorerItem.Load(AReportLink: TBasedxReportLink);
begin
  if Explorer <> nil then
    Explorer.LoadItemData(Self, AReportLink);
end;

procedure TdxPSExplorerItem.Unload;
begin
  Explorer.UnloadItemData(Self);
end;

class function TdxPSExplorerItem.PropertySheetsClass: TCustomdxPSExplorerItemPropertySheetsClass;
begin
  Result := TdxfmPSReportProperties;
end;

function TdxPSExplorerItem.CompareTo(AnItem: TCustomdxPSExplorerItem): Integer;
begin
  if AnItem is TdxPSExplorerItem then
    Result := CompareText(DisplayName, AnItem.DisplayName)
  else
    Result := 1;
end;

function TdxPSExplorerItem.DoDelete: Boolean;
begin
  if IsCurrentlyLoaded then Unload;
  Result := inherited DoDelete;
end;

function TdxPSExplorerItem.GetFormCaption: string;
begin
  Result := Name;
end;

function TdxPSExplorerItem.GetImageIndex: Integer;
begin
  if HasInvalidData then
    Result := iiExplorerItemHasInvalidData
  else
    Result := iiExplorerItem;
end;

function TdxPSExplorerItem.GetInfoTip: string;
begin
  Result := ReportDocument.InfoTip;
end;

function TdxPSExplorerItem.GetNewName(AReportLink: TBasedxReportLink): string;
var
  AIndex: Integer;
  ATemplate: string;
begin
  Result := cxGetResourceString(@sdxNewReport);
  if AReportLink <> nil then
    Result := AReportLink.GetNewReportStorageName;
  if (Parent <> nil) and (Parent.ItemByName(Result) <> nil) then
  begin
    AIndex := 1;
    ATemplate := ChangeFileExt(Result, '');
    repeat
      Inc(AIndex);
      Result := ATemplate + ' (' + IntToStr(AIndex) + ')';
    until Parent.ItemByName(Result) = nil;
  end;
end;

function TdxPSExplorerItem.GetSelectedIndex: Integer;
begin
  if HasInvalidData then
    Result := iiExplorerItemHasInvalidData
  else
    Result := iiExplorerItem;
end;

procedure TdxPSExplorerItem.InternalDelete;
begin
  SaveDocument;
  inherited;
end;

procedure TdxPSExplorerItem.DocumentChanged(Sender: TObject);
begin
  if not IsLoading then
  begin
    Name := ReportDocument.Caption;
    Explorer.NotifyItemPropertiesChanged(Self);
  end;
end;

procedure TdxPSExplorerItem.SaveDocument;
var
  Stream: TStream;
  OffsetTable: TdxPSDataStorageOffsetTable;
  P: Integer;
  Writer: TdxPSDataWriter;
begin
  Stream := CreateDataStream(smWrite);
  if Stream <> nil then
  try
    OffsetTable := TBasedxReportLink.ExtractOffsetTable(Stream, True);
    try
      P := Stream.Position;
      try
        Stream.Position := OffsetTable.Document;
        Writer := TBasedxReportLink.CreateDataWriter(Stream);
        try
          ReportDocument.WriteData(Writer);
        finally
          Writer.Free;
        end;
      finally
        Stream.Position := P;
      end;
    finally
      OffsetTable.Free;
    end;
  finally
    Stream.Free;
  end;
end;

procedure TdxPSExplorerItem.SetReportData(AReportLink: TBasedxReportLink);
var
  Stream: TStream;
begin
  if AReportLink <> nil then
  begin
    Stream := CreateDataStream(smRead);
    if Stream <> nil then
    try
      try
        AReportLink.LoadDataFromStream(Stream);
      except
        AReportLink.FinalizeDataStream;
        HasInvalidData := True;
        raise;
      end;
    finally
      Stream.Free;
    end;
  end;
end;

function TdxPSExplorerItem.GetIsCurrentlyLoaded: Boolean;
begin
  Result := Explorer.LoadedItem = Self;
end;

procedure TdxPSExplorerItem.SetHasInvalidData(Value: Boolean);
begin
  if FHasInvalidData <> Value then
  begin
    FHasInvalidData := Value;
  end;
end;

{ TCustomdxPSExplorerContextCommand }

constructor TCustomdxPSExplorerContextCommand.Create(AnExplorer: TCustomdxPSExplorer);
begin
  inherited Create;
  FBitmap := TcxBitmap.Create;
  FExplorer := AnExplorer;
end;

destructor TCustomdxPSExplorerContextCommand.Destroy;
begin
  FreeAndNil(FBitmap);
  inherited Destroy;
end;

function TCustomdxPSExplorerContextCommand.Enabled: Boolean;
begin
  Result := True;
end;

procedure TCustomdxPSExplorerContextCommand.Execute;
begin
end;

function TCustomdxPSExplorerContextCommand.Explorer: TCustomdxPSExplorer;
begin
  Result := FExplorer;
end;

procedure TCustomdxPSExplorerContextCommand.SetBitmap(Value: TcxBitmap);
begin
  Bitmap.Assign(Value);
end;

{ TdxPSExplorerContextCommandSeparator }

constructor TdxPSExplorerContextCommandSeparator.Create(AnExplorer: TCustomdxPSExplorer);
begin
  inherited;
  Caption := dxPSGlbl.cMenuSeparator;
end;

function TdxPSExplorerContextCommandSeparator.Enabled: Boolean;
begin
  Result := False;
end;

{ TdxPSExplorerRefreshContextCommand }

constructor TdxPSExplorerRefreshContextCommand.Create(AnExplorer: TCustomdxPSExplorer);
begin
  inherited Create(AnExplorer);
  Caption := cxGetResourceString(@sdxMenuExplorerRefresh);
  Hint := cxGetResourceString(@sdxHintExplorerRefresh);
  ShortCut := Menus.TextToShortCut('F5');
  dxLoadBitmapFromResource(Bitmap, IDB_DXPSREFRESH);
  Bitmap.Transparent := True;
end;

procedure TdxPSExplorerRefreshContextCommand.Execute;
begin
  Explorer.Refresh;
end;

{ TCustomdxPSExplorer }

constructor TCustomdxPSExplorer.Create(AOwner: TComponent);
begin
  inherited;
  FNotifiers := TList.Create;
  FCommands := TList.Create;
end;

destructor TCustomdxPSExplorer.Destroy;
begin
  FreeAndNilCommands;
  ReleaseAndNilNotifiers;
  FreeAndNil(FRoot);
  inherited Destroy;
end;

function TCustomdxPSExplorer.LoadedItem: TdxPSExplorerItem;
begin
  Result := FLoadedItem;
end;

function TCustomdxPSExplorer.CanCreateFolder: Boolean;
begin
  Result := True;
end;

function TCustomdxPSExplorer.CanCreateItem: Boolean;
begin
  Result := True;
end;

function TCustomdxPSExplorer.CreateNewFolder(AParent: TdxPSExplorerFolder): TdxPSExplorerFolder;
begin
  Include(FState, esFolderCreating);
  try
    Result := GetFolderClass.Create(Self, AParent);
    //DoCreateNewFolder(Result);
    NotifyItemAdded(Result);
  finally
    Exclude(FState, esFolderCreating);
  end;
end;

function TCustomdxPSExplorer.CreateNewItem(AParent: TdxPSExplorerFolder;
  AReportLink: TBasedxReportLink): TdxPSExplorerItem;
begin
  Include(FState, esItemCreating);
  try
    Result := GetItemClass.Create(Self, AParent);
    Result.FName := Result.GetNewName(AReportLink);
    AReportLink.ReportDocument.RetrievePreview;
    with Result.ReportDocument do
    begin
      BeginUpdate;
      try
        Assign(AReportLink.ReportDocument);
      finally
        CancelUpdate;
      end;
    end;

    //DoCreateNewItem(Result);
    NotifyItemAdded(Result);
  finally
    Exclude(FState, esItemCreating);
  end;
end;

procedure TCustomdxPSExplorer.BuildTree(ATreeContainer: TCustomdxPSExplorerTreeContainer);
begin
  dxPSExplorerTreeBuilderFactory_ActiveBuilderClass.BuildTree(Self, ATreeContainer);
  //dxPSExplorerTreeBuilderFactory.ActiveBuilderClass.BuildTree(Self, ATreeContainer);
  if LoadedItem <> nil then
    ATreeContainer.MakeItemVisible(LoadedItem);
end;

function TCustomdxPSExplorer.CreateTree(const AHost: IdxPSExplorerTreeContainerHost): TCustomdxPSExplorerTreeContainer;
begin
  Result := dxPSExplorerTreeContainerFactory.ActiveTreeContainerClass.Create(Self, AHost);
end;

function TCustomdxPSExplorer.FindCustomItemByUniqueID(const AnUniqueID: TBytes): TCustomdxPSExplorerItem;
begin
  Result := nil;
end;

procedure TCustomdxPSExplorer.LoadItemData(AnItem: TdxPSExplorerItem;
  AReportLink: TBasedxReportLink);
begin
  if LoadedItem <> nil then
    NotifyItemDataUnloaded(LoadedItem);

  try
    AnItem.SetReportData(AReportLink);
    FLoadedItem := AnItem;
    NotifyItemDataLoaded(AnItem);
  except
    FLoadedItem := nil;
    DoItemDataLoadError(AnItem);
  end;
end;

procedure TCustomdxPSExplorer.UnloadItemData(AnItem: TdxPSExplorerItem);
begin
  FLoadedItem := nil;
  NotifyItemDataUnloaded(AnItem);
end;

procedure TCustomdxPSExplorer.Refresh;
begin
  BeforeRefresh;
  try
    DoRefresh;
  finally
    AfterRefresh;
  end;
end;

procedure TCustomdxPSExplorer.RegisterNotifier(ANotifier: TdxPSExplorerChangeNotifier);
begin
  if IndexOfNotifier(ANotifier) = -1 then
  begin
    FNotifiers.Add(ANotifier);
    ANotifier.FExplorer := Self;
  end;
end;

procedure TCustomdxPSExplorer.UnregisterNotifier(ANotifier: TdxPSExplorerChangeNotifier);
begin
  FNotifiers.Remove(ANotifier);
  ANotifier.FExplorer := nil;
end;

{ IdxPSExplorerContextCommands }

procedure TCustomdxPSExplorer.BuildCommandSet(ABuilder: IdxPSExplorerContextCommandBuilder);
begin
  Assert(ABuilder <> nil);
  ABuilder.AddExplorerContextCommand(AddCommandSeparator);
  ABuilder.AddExplorerContextCommand(AddCommand(TdxPSExplorerRefreshContextCommand));
end;

procedure TCustomdxPSExplorer.FinalizeCommand(ACommand: TCustomdxPSExplorerContextCommand);
begin
end;

procedure TCustomdxPSExplorer.InitializeCommand(ACommand: TCustomdxPSExplorerContextCommand);
begin
end;

function TCustomdxPSExplorer.AddCommand(ACommandClass: TCustomdxPSExplorerContextCommandClass): TCustomdxPSExplorerContextCommand;
begin
  Result := FindCommand(ACommandClass);
  if Result = nil then
    Result := CreateCommand(ACommandClass);
end;

function TCustomdxPSExplorer.AddCommandSeparator: TdxPSExplorerContextCommandSeparator;
begin
  Result := AddCommand(TdxPSExplorerContextCommandSeparator) as TdxPSExplorerContextCommandSeparator;
end;

procedure TCustomdxPSExplorer.ClearCommands;
var
  I: Integer;
begin
  for I := 0 to CommandCount - 1 do
    Commands[I].Free;
  FCommands.Clear;
end;

function TCustomdxPSExplorer.CreateCommand(ACommandClass: TCustomdxPSExplorerContextCommandClass): TCustomdxPSExplorerContextCommand;
begin
  Result := ACommandClass.Create(Self);
  FCommands.Add(Result);
end;

function TCustomdxPSExplorer.CreateCommandSeparator: TdxPSExplorerContextCommandSeparator;
begin
  Result := TdxPSExplorerContextCommandSeparator.Create(Self);
end;

function TCustomdxPSExplorer.FindCommand(ACommandClass: TCustomdxPSExplorerContextCommandClass): TCustomdxPSExplorerContextCommand;
var
  I: Integer;
begin
  for I := 0 to CommandCount - 1 do
  begin
    Result := Commands[I];
    if Result.ClassType = ACommandClass then Exit;
  end;
  Result := nil;
end;

procedure TCustomdxPSExplorer.FreeAndNilCommands;
begin
  ClearCommands;
  FreeAndNil(FCommands);
end;

class function TCustomdxPSExplorer.AcceptItemNameChar(AnItem: TCustomdxPSExplorerItem;
  Ch: Char): Boolean;
begin
  Result := True;
end;

class function TCustomdxPSExplorer.GetFolderClass: TdxPSExplorerFolderClass;
begin
  Result := TdxPSExplorerFolder;
end;

class function TCustomdxPSExplorer.GetItemClass: TdxPSExplorerItemClass;
begin
  Result := TdxPSExplorerItem;
end;

class function TCustomdxPSExplorer.GetRootFolderClass: TdxPSExplorerFolderClass;
begin
  Result := TdxPSExplorerFolder;
end;

function TCustomdxPSExplorer.GetRootDisplayName: string;
begin
  Result := cxGetResourceString(@sdxExplorerRootFolderCaption);
end;

procedure TCustomdxPSExplorer.LoadData(AFolder: TdxPSExplorerFolder);
begin
  BeginLoading;
  try
    LockNotifications;
    try
      dxPSStartWait;
      try
        DoLoadData(AFolder);
      finally
        dxPSStopWait;
      end;
    finally
      UnlockNotifications
    end;
  finally
    EndLoading;
  end;
end;

function TCustomdxPSExplorer.CanDelete(AnItem: TCustomdxPSExplorerItem): Boolean;
begin
  Result := (AnItem <> nil) and (AnItem <> Root);
end;

function TCustomdxPSExplorer.CanMove(AnItem: TCustomdxPSExplorerItem): Boolean;
begin
  Result := (AnItem <> nil) and (AnItem <> Root) and (AnItem <> LoadedItem);
end;

function TCustomdxPSExplorer.CanMoveTo(AnItem: TCustomdxPSExplorerItem;
  AParent: TCustomdxPSExplorerItem): Boolean;
begin
  Result := CanMove(AnItem);
end;

function TCustomdxPSExplorer.CanRename(AnItem: TCustomdxPSExplorerItem): Boolean;
begin
  Result := (AnItem <> nil) and (AnItem <> Root) and (AnItem <> LoadedItem);
end;

function TCustomdxPSExplorer.CanRenameTo(AnItem: TCustomdxPSExplorerItem;
  const AName: string): Boolean;
begin
  Result := CanRename(AnItem);
end;

procedure TCustomdxPSExplorer.Delete(AnItem: TCustomdxPSExplorerItem);
begin
  if not (esLoading in State) then
    if AnItem.DoDelete then
    begin
      NotifyItemDeleted(AnItem);
      AnItem.InternalDelete;
    end;
end;

procedure TCustomdxPSExplorer.MoveTo(AnItem: TCustomdxPSExplorerItem;
  AParent: TdxPSExplorerFolder);
begin
  if AnItem.DoMove(AParent) then
  begin
    AnItem.InternalMove(AParent);
    NotifyItemParentChanged(AnItem);
  end;
end;

procedure TCustomdxPSExplorer.PopulateFolder(AFolder: TdxPSExplorerFolder);
begin
  if AFolder.HasChildren and (AFolder.FolderCount + AFolder.ItemCount = 0) then
  begin
    LoadData(AFolder);
    NotifyFolderPopulated(AFolder);
  end;
end;

procedure TCustomdxPSExplorer.RenameTo(AnItem: TCustomdxPSExplorerItem; AName: string);
begin
  if AnItem.DoRename(AName) then
  begin
    AnItem.InternalRename(AName);
    NotifyItemRenamed(AnItem);
  end;
end;

procedure TCustomdxPSExplorer.AfterRefresh;
begin
  Dec(FRefreshCounter);
  if FRefreshCounter = 0 then
  begin
    UnlockNotifications;
    LoadState;
    NotifyRefresh(ersAfter);
    Exclude(FState, esRefreshing);
  end;
end;

procedure TCustomdxPSExplorer.BeforeRefresh;
begin
  if FRefreshCounter = 0 then
  begin
    Include(FState, esRefreshing);
    SaveState;
    NotifyRefresh(ersBefore);
    LockNotifications;
  end;
  Inc(FRefreshCounter);
end;

procedure TCustomdxPSExplorer.DoRefresh;
begin
  if LoadedItem <> nil then LoadedItem.Unload;
  FreeAndNil(FRoot);
end;

procedure TCustomdxPSExplorer.RootNeeded;
begin
  Root;
end;

procedure TCustomdxPSExplorer.DoItemDataLoadError(AnItem: TdxPSExplorerItem);
var
  S: string;
  ShowError: Boolean;
begin
  S := AnItem.DataLoadErrorText;
  ShowError := True;
  if Assigned(FOnItemDataLoadError) then
    FOnItemDataLoadError(Self, AnItem, ShowError, S);
  if ShowError then
    MessageError(S);
end;

procedure TCustomdxPSExplorer.InternalSetLoadedItem(Value: TdxPSExplorerItem);
begin
  if Value <> nil then  //?
    FLoadedItem := Value;
end;

procedure TCustomdxPSExplorer.PopulateTreeFolder(ATreeContainer: TCustomdxPSExplorerTreeContainer;
  AFolder: TdxPSExplorerFolder);
begin
  dxPSExplorerTreeBuilderFactory_ActiveBuilderClass.PopulateTreeFolder(ATreeContainer, AFolder);
  //dxPSExplorerTreeBuilderFactory.ActiveBuilderClass.PopulateTreeFolder(ATreeContainer, AFolder);
end;

procedure TCustomdxPSExplorer.LoadState;

  function ReadItem: TCustomdxPSExplorerItem;
  var
    ItemInfo: TCustomdxPSExplorerItemStateInfo;
    Bytes: TBytes;
    I: Integer;
  begin
    FStateStream.ReadBuffer(ItemInfo, GetItemStateInfoSize);
    if ItemInfo.UniqueIDSize <> 0 then
    begin
      SetLength(Bytes, ItemInfo.UniqueIDSize);
      FStateStream.ReadBuffer(Pointer(Bytes)^, ItemInfo.UniqueIDSize);
      Result := FindCustomItemByUniqueID(Bytes);
    end
    else
      Result := nil;
    for I := 0 to ItemInfo.Count - 1 do
      ReadItem;
  end;

  procedure ReadLoadedItem;
  var
    Buffer: Integer;
  begin
    FStateStream.ReadBuffer(Buffer , SizeOf(Buffer));
    if Buffer <> 0 then
      InternalSetLoadedItem(TdxPSExplorerItem(ReadItem));
  end;

begin
  if FStateStream <> nil then
  try
    FStateStream.Position := 0;
    ReadItem;
    ReadLoadedItem;
  finally
    FreeAndNil(FStateStream);
  end;
end;

procedure TCustomdxPSExplorer.SaveState;

  procedure WriteLoadedItem;
  var
    Flag: Integer;
  begin
    Flag := Ord(LoadedItem <> nil);
    FStateStream.WriteBuffer(Flag, SizeOf(Flag));
    if LoadedItem <> nil then
      LoadedItem.WriteState(FStateStream);
  end;

begin
  if FRoot <> nil then
  begin
    FStateStream := TMemoryStream.Create;
    try
      Root.WriteState(FStateStream);
      WriteLoadedItem;
    except
      FreeAndNil(FStateStream);
      raise;
    end;
  end;
end;

procedure TCustomdxPSExplorer.NotifyFolderPopulated(AFolder: TdxPSExplorerFolder);
var
  I: Integer;
begin
  if not AreNotificationsLocked then
    for I := 0 to NotifierCount - 1 do
      Notifiers[I].FolderPopulated(AFolder);
end;

procedure TCustomdxPSExplorer.NotifyItemAdded(AnItem: TCustomdxPSExplorerItem);
var
  I: Integer;
begin
  if not AreNotificationsLocked then
    for I := 0 to NotifierCount - 1 do
      Notifiers[I].ItemAdded(AnItem);
end;

procedure TCustomdxPSExplorer.NotifyItemDataLoaded(AnItem: TdxPSExplorerItem);
var
  I: Integer;
begin
  if not AreNotificationsLocked then
    for I := 0 to NotifierCount - 1 do
      Notifiers[I].ItemDataLoaded(AnItem);
end;

procedure TCustomdxPSExplorer.NotifyItemDataUnloaded(AnItem: TdxPSExplorerItem);
var
  I: Integer;
begin
  if not AreNotificationsLocked then
    for I := 0 to NotifierCount - 1 do
      Notifiers[I].ItemDataUnloaded(AnItem);
end;

procedure TCustomdxPSExplorer.NotifyItemDeleted(AnItem: TCustomdxPSExplorerItem);
var
  I: Integer;
begin
  if not AreNotificationsLocked then
    for I := 0 to NotifierCount - 1 do
      Notifiers[I].ItemDeleted(AnItem);
end;

procedure TCustomdxPSExplorer.NotifyItemParentChanged(AnItem: TCustomdxPSExplorerItem);
var
  I: Integer;
begin
  if not AreNotificationsLocked then
    for I := 0 to NotifierCount - 1 do
      Notifiers[I].ItemParentChanged(AnItem);
end;

procedure TCustomdxPSExplorer.NotifyItemPropertiesChanged(AnItem: TCustomdxPSExplorerItem);
var
  I: Integer;
begin
  if not AreNotificationsLocked then
    for I := 0 to NotifierCount - 1 do
      Notifiers[I].ItemPropertiesChanged(AnItem);
end;

procedure TCustomdxPSExplorer.NotifyItemRenamed(AnItem: TCustomdxPSExplorerItem);
var
  I: Integer;
begin
  if not AreNotificationsLocked then
    for I := 0 to NotifierCount - 1 do
      Notifiers[I].ItemRenamed(AnItem);
end;

procedure TCustomdxPSExplorer.NotifyRefresh(AStage: TdxPSExplorerRefreshStage);
var
  I: Integer;
begin
  if not AreNotificationsLocked then
    for I := 0 to NotifierCount - 1 do
      Notifiers[I].ExplorerRefresh(AStage);
end;

function TCustomdxPSExplorer.AreNotificationsLocked: Boolean;
begin
  Result := FLockCounter <> 0;
end;

function TCustomdxPSExplorer.IndexOfNotifier(ANotifier: TdxPSExplorerChangeNotifier): Integer;
begin
  Result := FNotifiers.IndexOf(ANotifier);
end;

procedure TCustomdxPSExplorer.LockNotifications;
begin
  Inc(FLockCounter);
end;

procedure TCustomdxPSExplorer.ReleaseAndNilNotifiers;
var
  I: Integer;
begin
  for I := NotifierCount - 1 downto 0 do
    Notifiers[I].Explorer := nil;
  FreeAndNil(FNotifiers);
end;

procedure TCustomdxPSExplorer.UnlockNotifications;
begin
  Dec(FLockCounter);
end;

procedure TCustomdxPSExplorer.BeginLoading;
begin
  if FLoadingCounter = 0 then Include(FState, esLoading);
  Inc(FLoadingCounter);
end;

procedure TCustomdxPSExplorer.EndLoading;
begin
  Dec(FLoadingCounter);
  if FLoadingCounter = 0 then Exclude(FState, esLoading);
end;

function TCustomdxPSExplorer.IsLoading: Boolean;
begin
  Result := FLoadingCounter <> 0;
end;

function TCustomdxPSExplorer.GetCommand(Index: Integer): TCustomdxPSExplorerContextCommand;
begin
  Result := TCustomdxPSExplorerContextCommand(FCommands[Index]);
end;

function TCustomdxPSExplorer.GetCommandCount: Integer;
begin
  Result := FCommands.Count;
end;

function TCustomdxPSExplorer.GetNotifier(Index: Integer): TdxPSExplorerChangeNotifier;
begin
  Result := TdxPSExplorerChangeNotifier(FNotifiers[Index]);
end;

function TCustomdxPSExplorer.GetNotifierCount: Integer;
begin
  Result := FNotifiers.Count;
end;

function TCustomdxPSExplorer.GetRoot: TdxPSExplorerFolder;
begin
  if FRoot = nil then
  begin
    BeginLoading;
    try
      LockNotifications;
      try
        FRoot := GetRootFolderClass.Create(Self, nil);
        FRoot.FName := GetRootDisplayName;
        LoadData(FRoot);
      finally
        UnlockNotifications;
      end;
    finally
      EndLoading;
    end;
  end;
  Result := FRoot;
end;

procedure TCustomdxPSExplorer.SetFilterLink(const Value: string);
var
  AComponentClass: TComponentClass;
begin
  AComponentClass := TComponentClass(GetClass(Value));
  if dxPSIsSupportedCompClass(AComponentClass) then
  begin
    FilterLinkClass := AComponentClass;
    FilterLinkClassName := FilterLinkClass.ClassName;
  end
  else
  begin
    FilterLinkClassName := '';
    FilterLinkClass := nil;
  end;
end;

{ TdxReportTitle }

procedure TdxReportTitle.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TdxReportTitle then
    Mode := TdxReportTitle(Source).Mode;
end;

procedure TdxReportTitle.DoRestoreDefaults;
begin
  inherited DoRestoreDefaults;
  Mode := tmOnEveryTopPage;
end;

procedure TdxReportTitle.DoCustomDraw(ACanvas: TCanvas; R: TRect;
  var ATextAlignX: TcxTextAlignX; var ATextAlignY: TcxTextAlignY;
  var AColor: TColor; AFont: TFont; var ADone: Boolean);
begin
  if Assigned(ReportLink) then
  begin
    ReportLink.DoCustomDrawPageTitle(ACanvas, R,
      ATextAlignX, ATextAlignY, AColor, AFont, ADone);
  end;
end;

function TdxReportTitle.IsCustomDrawn: Boolean;
begin
  Result := Assigned(ReportLink) and ReportLink.IsTitleCustomDrawn;
end;

procedure TdxReportTitle.ReadMode(AReader: TdxPSDataReader);
begin
  Mode := TdxReportTitleMode(AReader.ReadInteger);
end;

procedure TdxReportTitle.WriteMode(AWriter: TdxPSDataWriter);
begin
  AWriter.WriteInteger(Integer(Mode));
end;

procedure TdxReportTitle.SetMode(Value: TdxReportTitleMode);
begin
  if FMode <> Value then
  begin
    FMode := Value;
    CalculateRenderInfos;
  end;
end;

{ TdxReportNote }

constructor TdxReportNote.Create(AReportLink: TBasedxReportLink);
begin
  inherited Create;
  FReportLink := AReportLink;
  FFont := TFont.Create;
  RestoreDefaults(False);
  FFont.OnChange := FontChanged;
end;

destructor TdxReportNote.Destroy;
begin
  FreeAndNil(FDefaultFont);
  FreeAndNil(FFont);
  inherited Destroy;
end;

procedure TdxReportNote.Assign(Source: TPersistent);
begin
  if Source is TdxReportNote then
  begin
    BeginUpdate;
    try
      Color := TdxReportNote(Source).Color;
      Font := TdxReportNote(Source).Font;
      Text := TdxReportNote(Source).Text;
      TextAlignX := TdxReportNote(Source).TextAlignX;
      TextAlignY := TdxReportNote(Source).TextAlignY;
      Transparent := TdxReportNote(Source).Transparent;
      AdjustOnReportScale := TdxReportNote(Source).AdjustOnReportScale;
    finally
      EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;

procedure TdxReportNote.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TdxReportNote.CancelUpdate;
begin
  Dec(FUpdateCount);
end;

procedure TdxReportNote.EndUpdate;
begin
  Dec(FUpdateCount);
  if FHasChanged and (FUpdateCount = 0) then
    Changed;
end;

function TdxReportNote.DefaultFont: TFont;
begin
  if FDefaultFont = nil then
  begin
    FDefaultFont := TFont.Create;
    InitializeDefaultFont(FDefaultFont);
  end;
  Result := FDefaultFont;
end;

procedure TdxReportNote.Changed;
begin
  FHasChanged := True;
  if FUpdateCount = 0 then
  begin
    CalculateRenderInfos;
    FHasChanged := False;
  end;
end;

procedure TdxReportNote.CalculateRenderInfos;
begin
  if (FUpdateCount = 0) and Assigned(ReportLink) then
    ReportLink.CalculateRenderInfos;
end;

procedure TdxReportNote.FontChanged(Sender: TObject);
begin
  Changed;
end;

procedure TdxReportNote.Draw(ARenderer: TdxPSReportRenderer; const R: TRect);

  procedure DoDrawNoteText(ACanvas: TdxPSReportRenderCustomCanvas; R: TRect;
    AColor: TColor; AFont: TFont; ATextAlignX: TcxTextAlignX; ATextAlignY: TcxTextAlignY);
  begin
    if not Transparent then
      ACanvas.FillRect(R, AColor);
    ARenderer.DrawTextEx(ACanvas, R, 0, 0, 0, Text, AFont,
      CXTO_AUTOINDENTS or CXTO_EXPANDTABS or CXTO_PATTERNEDTEXT or CXTO_CHARBREAK or
      ARenderer.MakeTextFormat(ATextAlignX, ATextAlignY, True, False, True, True, False));
  end;

  procedure InternalCustomDraw(ACanvas: TdxPSReportRenderCustomCanvas; R: TRect);
  var
    AColor: TColor;
    ACustomDrawCanvas: TCanvas;
    AHandled: Boolean;
    ARenderCanvas: TdxPSReportRenderCanvas;
    ATextAlignX: TcxTextAlignX;
    ATextAlignY: TcxTextAlignY;
  begin
    AHandled := False;
    ACustomDrawCanvas := ACanvas.BeginCustomDraw(R, ACanvas.Font, Color);
    try
      AColor := Color;
      ATextAlignX := TextAlignX;
      ATextAlignY := TextAlignY;
      DoCustomDraw(ACustomDrawCanvas, R, ATextAlignX, ATextAlignY,
        AColor, ACustomDrawCanvas.Font, AHandled);
      if not AHandled then
      begin
        ARenderCanvas := TdxPSReportRenderCanvas.Create(ACustomDrawCanvas);
        try
          DoDrawNoteText(ARenderCanvas, R, AColor,
            ACustomDrawCanvas.Font, ATextAlignX, ATextAlignY);
        finally
          ARenderCanvas.Free;
        end;
      end;
    finally
      ACanvas.EndCustomDraw(ACustomDrawCanvas);
    end;
  end;

var
  ATempFont: TFont;
begin
  ARenderer.Canvas.SaveState;
  try
    ATempFont := TFont.Create;
    try
      ATempFont.Assign(Font);
      ARenderer.PrepareFont(ATempFont, AdjustOnReportScale);
      ARenderer.Canvas.Font := ATempFont;
      if IsCustomDrawn then
        InternalCustomDraw(ARenderer.Canvas, R)
      else
        DoDrawNoteText(ARenderer.Canvas, R, Color, ATempFont, TextAlignX, TextAlignY);
    finally
      ATempFont.Free;
    end;
  finally
    ARenderer.Canvas.RestoreState;
  end;
end;

procedure TdxReportNote.DoRestoreDefaults;
begin
  AdjustOnReportScale := False;
  Color := clWhite;
  Font.Assign(DefaultFont);
  TextAlignX := taCenterX;
  TextAlignY := taCenterY;
  Transparent := True;
end;

procedure TdxReportNote.InitializeDefaultFont(AFont: TFont);
begin
  AFont.Color := dxPSDefaultReportTitleFontColor;
  AFont.Name := dxPSDefaultReportTitleFontName;
  AFont.Size := dxPSDefaultReportTitleFontSize;
  AFont.Style := dxPSDefaultReportTitleFontStyle;
end;

procedure TdxReportNote.ReadData(AReader: TdxPSDataReader);
begin
  BeginUpdate;
  try
    AdjustOnReportScale := AReader.ReadBoolean;
    Color := AReader.ReadInteger;
    AReader.ReadFont(Font);
    ReadMode(AReader); // backward compatibility
    Text := AReader.ReadString;
    TextAlignX := TcxTextAlignX(AReader.ReadInteger);
    TextAlignY := TcxTextAlignY(AReader.ReadInteger);
    Transparent := AReader.ReadBoolean;
  finally
    CancelUpdate;
  end;
end;

procedure TdxReportNote.RestoreDefaults(ARefresh: Boolean = True);
begin
  BeginUpdate;
  try
    DoRestoreDefaults;
    FHasChanged := ARefresh;
  finally
    EndUpdate;
  end;
end;

procedure TdxReportNote.ReadMode(AReader: TdxPSDataReader);
begin
end;

procedure TdxReportNote.WriteData(AWriter: TdxPSDataWriter);
begin
  AWriter.WriteBoolean(AdjustOnReportScale);
  AWriter.WriteInteger(Color);
  AWriter.WriteFont(Font);
  WriteMode(AWriter);
  AWriter.WriteString(Text);
  AWriter.WriteInteger(Integer(TextAlignX));
  AWriter.WriteInteger(Integer(TextAlignY));
  AWriter.WriteBoolean(Transparent);
end;

procedure TdxReportNote.WriteMode(AWriter: TdxPSDataWriter);
begin
end;

function TdxReportNote.IsFontStored: Boolean;
begin
  Result := not dxPSUtl.dxAreFontsEqual(Font, DefaultFont);
end;

procedure TdxReportNote.SetAdjustOnReportScale(Value: Boolean);
begin
  if FAdjustOnReportScale <> Value then
  begin
    FAdjustOnReportScale := Value;
    Changed;
  end;
end;

procedure TdxReportNote.SetFont(Value: TFont);
begin
  Font.Assign(Value);
end;

procedure TdxReportNote.SetText(const Value: string);
begin
  if FText <> Value then
  begin
    FText := Value;
    Changed;
  end;
end;

procedure TdxReportNote.SetTextAlignX(Value: TcxTextAlignX);
begin
  if Value in [taLeft, taCenterX, taRight] then
  begin
    FTextAlignX := Value; // backward compatibility
    Changed;
  end;
end;

procedure TdxReportNote.SetTextAlignY(Value: TcxTextAlignY);
begin
  if Value in [taTop, taCenterY, taBottom] then
  begin
    FTextAlignY := Value; // backward compatibility
    Changed;
  end;
end;

{ TdxReportFootnotes }

procedure TdxReportFootnotes.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TdxReportFootnotes then
    Mode := TdxReportFootnotes(Source).Mode;
end;

procedure TdxReportFootnotes.DoRestoreDefaults;
begin
  inherited DoRestoreDefaults;
  FMode := fnmOnEveryBottomPage;
end;

procedure TdxReportFootnotes.DoCustomDraw(ACanvas: TCanvas; R: TRect;
  var ATextAlignX: TcxTextAlignX; var ATextAlignY: TcxTextAlignY;
  var AColor: TColor; AFont: TFont; var ADone: Boolean);
begin
  if Assigned(ReportLink) then
  begin
    ReportLink.DoCustomDrawPageFootnotes(ACanvas, R,
      ATextAlignX, ATextAlignY, AColor, AFont, ADone);
  end;
end;

function TdxReportFootnotes.IsCustomDrawn: Boolean;
begin
  Result := Assigned(ReportLink) and ReportLink.IsFootnotesCustomDrawn;
end;

procedure TdxReportFootnotes.ReadMode(AReader: TdxPSDataReader);
begin
  Mode := TdxReportFootnoteMode(AReader.ReadInteger);
end;

procedure TdxReportFootnotes.WriteMode(AWriter: TdxPSDataWriter);
begin
  AWriter.WriteInteger(Integer(Mode));
end;

procedure TdxReportFootnotes.SetMode(Value: TdxReportFootnoteMode);
begin
  if Value <> Mode then
  begin
    FMode := Value;
    Changed;
  end;
end;

{ TdxPSReportDocument }

constructor TdxPSReportDocument.Create(AReportLink: TBasedxReportLink);
begin
  inherited Create;
  FReportLink := AReportLink;
  FPreview := TMetafile.Create;
end;

destructor TdxPSReportDocument.Destroy;
begin
  FreeAndNil(FPreview);
  inherited;
end;

procedure TdxPSReportDocument.Assign(Source: TPersistent);
begin
  if Source is TdxPSReportDocument then
  begin
    BeginUpdate;
    try
      DoAssign(TdxPSReportDocument(Source));
    finally
      EndUpdate;
    end;
  end
  else
    inherited;
end;

procedure TdxPSReportDocument.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TdxPSReportDocument.CancelUpdate;
begin
  Dec(FUpdateCount);
end;

procedure TdxPSReportDocument.EndUpdate;
begin
  Dec(FUpdateCount);
  Changed;
end;

function TdxPSReportDocument.IsUpdateLocked: Boolean;
begin
  Result := UpdateCount <> 0;
end;

function TdxPSReportDocument.DefaultCaption: string;
begin
  Result := cxGetResourceString(@sdxNewReport);
end;

function TdxPSReportDocument.DefaultCreator: string;
begin
  Result := dxGetUserName;
end;

function TdxPSReportDocument.DefaultDescription: string;
begin
  Result := '';
end;

procedure TdxPSReportDocument.RestoreDefaults;
begin
  BeginUpdate;
  try
    DoRestoreDefaults;
  finally
    EndUpdate;
  end;
end;

procedure TdxPSReportDocument.RetrievePreview;
begin
  Preview.Clear;
  if ReportLink <> nil then
    ReportLink.RetrievePageAsImage(0, TMetafile, Preview);
end;

procedure TdxPSReportDocument.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('IsCaptionAssigned', ReadIsCaptionAssigned, WriteIsCaptionAssigned,
    FIsCaptionAssigned and (Caption = ''));
  Filer.DefineProperty('IsCreatorAssigned', ReadIsCreatorAssigned, WriteIsCreatorAssigned,
    FIsCreatorAssigned and (Creator = ''));
  Filer.DefineProperty('IsDescriptionAssigned', ReadIsDescriptionAssigned, WriteIsDescriptionAssigned,
    FIsDescriptionAssigned and (Description = ''));
end;

procedure TdxPSReportDocument.Changed;
begin
  if not IsUpdateLocked then
    dxCallNotify(OnChanged, Self);
end;

procedure TdxPSReportDocument.DoAssign(Source: TdxPSReportDocument);
begin
  Caption := Source.Caption;
  CreationDate := Source.CreationDate;
  Creator := Source.Creator;
  Description := Source.Description;
  Preview.Assign(Source.Preview);
  FIsCaptionAssigned := Source.FIsCaptionAssigned;
  FIsCreatorAssigned := Source.FIsCreatorAssigned;
  FIsDescriptionAssigned := Source.FIsDescriptionAssigned;
end;

procedure TdxPSReportDocument.DoRestoreDefaults;
begin
  FIsCaptionAssigned := False;
  FisCreatorAssigned := False;
  FIsDescriptionAssigned := False;
end;

function TdxPSReportDocument.GetInfoTip: string;
begin
  Result := '';

  if Caption <> '' then
  begin
    if Result <> '' then Result := Result + dxCRLF;
    Result := Result + DropAmpersand(cxGetResourceString(@sdxCaption)) + ' ' + Caption;
  end;

  if Creator <> '' then
  begin
    if Result <> '' then Result := Result + dxCRLF;
    Result := Result + DropAmpersand(cxGetResourceString(@sdxCreator)) + ' ' + Creator;
  end;

  if Result <> '' then Result := Result + dxCRLF;
  Result := Result + DropAmpersand(cxGetResourceString(@sdxCreationDate)) + ' ' + DateToStr(CreationDate);

  if Description <> '' then
  begin
    if Result <> '' then Result := Result + dxCRLF;
    Result := Result + DropAmpersand(cxGetResourceString(@sdxDescription)) + ' ' + Description;
  end;
end;

procedure TdxPSReportDocument.ReadData(AReader: TdxPSDataReader);
begin
  BeginUpdate;
  try
    Caption := AReader.ReadString;
    CreationDate := AReader.ReadDate;
    Creator := AReader.ReadString;
    Description := AReader.ReadString;
    AReader.ReadImage(Preview);
  finally
    EndUpdate;
  end;
end;

procedure TdxPSReportDocument.WriteData(AWriter: TdxPSDataWriter);
begin
  RetrievePreview;
  AWriter.WriteString(Caption);
  AWriter.WriteDate(CreationDate);
  AWriter.WriteString(Creator);
  AWriter.WriteString(Description);
  AWriter.WriteImage(Preview);
end;

function TdxPSReportDocument.GetCaption: string;
begin
  if FIsCaptionAssigned then
    Result := FCaption
  else
    Result := DefaultCaption;
end;

function TdxPSReportDocument.GetCreator: string;
begin
  if FIsCreatorAssigned then
    Result := FCreator
  else
    Result := DefaultCreator;
end;

function TdxPSReportDocument.GetDescription: string;
begin
  if FIsDescriptionAssigned then
    Result := FDescription
  else
    Result := DefaultDescription;
end;

function TdxPSReportDocument.IsCaptionStored: Boolean;
begin
  Result := FIsCaptionAssigned and (FCaption <> DefaultCaption);
end;

function TdxPSReportDocument.IsCreatorStored: Boolean;
begin
  Result := FIsCreatorAssigned and (FCreator <> DefaultCreator);
end;

function TdxPSReportDocument.IsDesciptionStored: Boolean;
begin
  Result := FIsDescriptionAssigned and (FDescription <> DefaultDescription);
end;

procedure TdxPSReportDocument.SetCaption(const Value: string);
begin
  if Caption <> Value then
  begin
    FCaption := Value;
    FIsCaptionAssigned := True;
    Changed;
  end;
end;

procedure TdxPSReportDocument.SetCreationDate(const Value: TDateTime);
begin
  if FCreationDate <> Value then
  begin
    FCreationDate := Value;
    Changed;
  end;
end;

procedure TdxPSReportDocument.SetCreator(const Value: string);
begin
  if FCreator <> Value then
  begin
    FIsCreatorAssigned := True;
    FCreator := Value;
    Changed;
  end;
end;

procedure TdxPSReportDocument.SetDescription(const Value: string);
begin
  if FDescription <> Value then
  begin
    FIsDescriptionAssigned := True;
    FDescription := Value;
    Changed;
  end;
end;

procedure TdxPSReportDocument.ReadIsCaptionAssigned(Reader: TReader);
begin
  FIsCaptionAssigned := Reader.ReadBoolean;
end;

procedure TdxPSReportDocument.ReadIsCreatorAssigned(Reader: TReader);
begin
  FIsCreatorAssigned := Reader.ReadBoolean;
end;

procedure TdxPSReportDocument.ReadIsDescriptionAssigned(Reader: TReader);
begin
  FIsDescriptionAssigned := Reader.ReadBoolean;
end;

procedure TdxPSReportDocument.WriteIsCaptionAssigned(Writer: TWriter);
begin
  Writer.WriteBoolean(FIsCaptionAssigned);
end;

procedure TdxPSReportDocument.WriteIsCreatorAssigned(Writer: TWriter);
begin
  Writer.WriteBoolean(FIsCreatorAssigned);
end;

procedure TdxPSReportDocument.WriteIsDescriptionAssigned(Writer: TWriter);
begin
  Writer.WriteBoolean(FIsDescriptionAssigned);
end;

{ TdxPSDataStorageOffsetTable }

constructor TdxPSDataStorageOffsetTable.Create(ATemplate: TdxPSDataStorageOffsetTable = nil);
begin
  inherited Create;
  Assign(ATemplate);
end;

procedure TdxPSDataStorageOffsetTable.Assign(Source: TdxPSDataStorageOffsetTable);
begin
  Clear;
  if Source <> nil then
    DoAssign(Source);
end;

procedure TdxPSDataStorageOffsetTable.Clear;
begin
  ZeroMemory(@FTableInfo, SizeOf(FTableInfo));
end;

procedure TdxPSDataStorageOffsetTable.DoAssign(Source: TdxPSDataStorageOffsetTable);
begin
  FTableInfo := Source.FTableInfo;
end;

procedure TdxPSDataStorageOffsetTable.ReadData(AReader: TdxPSDataReader);
var
  AMemSize: Integer;
begin
  Clear;
  AReader.Read(AMemSize, SizeOf(AMemSize));
  AReader.Read(FTableInfo, AMemSize);
end;

procedure TdxPSDataStorageOffsetTable.WriteData(AWriter: TdxPSDataWriter);
var
  AMemSize: Integer;
begin
  AMemSize := SizeOf(FTableInfo);
  AWriter.Write(AMemSize, SizeOf(AMemSize));
  AWriter.Write(FTableInfo, AMemSize);
end;

{ TdxPSDataStorageInfo }

constructor TdxPSDataStorageInfo.Create(AReportLink: TBasedxReportLink);
begin
  inherited Create;
  StorageVersion := dxPSGlbl.dxPSStorageVersion;
  PrintingSystemVersion := dxPSGlbl.dxPSVersion;
  if Assigned(AReportLink) then
  begin
    LinkClassName := dxStringToShortString(AReportLink.ClassName);
    LinkClass := TdxReportLinkClass(Classes.GetClass(dxShortStringToString(LinkClassName)));
    if Assigned(AReportLink.Component) then
    begin
      ComponentClassName := dxStringToShortString(AReportLink.Component.ClassName);
      ComponentClass := TComponentClass(Classes.GetClass(dxShortStringToString(ComponentClassName)));
    end;
  end;
end;

procedure TdxPSDataStorageInfo.ReadData(AReader: TdxPSDataReader);
const
  StorageVersion_1_DefaultUnitsPerInch = 4800;
begin
  StorageVersion := AReader.ReadInteger;
  AReader.StorageVersion := StorageVersion;
  PrintingSystemVersion := AReader.ReadPSVersion;
  LinkClassName := dxStringToShortString(AReader.ReadString);
  ComponentClassName := dxStringToShortString(AReader.ReadString);
  LinkClass := TdxReportLinkClass(Classes.GetClass(dxShortStringToString(LinkClassName)));
  ComponentClass := TComponentClass(Classes.GetClass(dxShortStringToString(ComponentClassName)));
  if StorageVersion > 1 then
    UnitsPerInch := AReader.ReadInteger
  else
    UnitsPerInch := StorageVersion_1_DefaultUnitsPerInch;
  AReader.UnitsPerInch := UnitsPerInch;
end;

procedure TdxPSDataStorageInfo.WriteData(AWriter: TdxPSDataWriter);
begin
  AWriter.WriteInteger(StorageVersion);
  AWriter.WritePSVersion(PrintingSystemVersion);
  AWriter.WriteString(dxShortStringToString(LinkClassName));
  AWriter.WriteString(dxShortStringToString(ComponentClassName));
  AWriter.WriteInteger(dxPSCore.FUnitsPerInch);
end;

{ TBasedxReportLink }

constructor TBasedxReportLink.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  dxPSActiveReportLinks.Add(Self);
  FBackgroundBitmapPool := TdxPSBackgroundBitmapPool.Create;
  FBuiltIn := IsDesigning;
  FColor := dxDefaultContentColor; {clWhite}
  FCurrentPage := 1;
  FFont := TFont.Create;
  FFont.Assign(DefaultFont);
  FFont.OnChange := FontChanged;
  FDataSource := rldsComponent;
  FPageNumberFormat := pnfNumeral;
  FPrinterPage := CreatePrinterPage;
  FReportDocument := CreateReportDocument;
  FReportDocument.OnChanged := DocumentChanged;
  FReportTitle := TdxReportTitle.Create(Self);
  FReportFootnotes := TdxReportFootnotes.Create(Self);
  FSavedReportFootnotes := TdxReportFootnotes.Create(nil);
  FSavedReportTitle := TdxReportTitle.Create(nil);
  FSavedReportDocument := CreateReportDocument;
  FScaleFonts := True;
  FShowPageFooter := True;
  FShowPageHeader := True;
  FShowPageRowHeader := True;
  FSubscriber := TdxPageParamsChangedSubscriber.Create([TdxSMPageParamsChangedEvent]);
  TdxPageParamsChangedSubscriber(FSubscriber).OnPageParamsChanged := PageParamsChanged;
  FPDFExportOptions := TdxPSPDFReportExportOptions.Create;
  FStartPageIndex := 1;
  FTransparent := True;
  FUseHorzDelimiters := True;
  FUseVertDelimiters := True;
  dxPrintingRepository.Register(Self);
end;

destructor TBasedxReportLink.Destroy;
begin
  dxPrintingRepository.Unregister(Self);
  Destroying;
  cxClearObjectLinks(Self);
  dxPSActiveReportLinks.Remove(Self);
  ComponentPrinter := nil;
  Component := nil;
  DoDestroyReport; {v2.0}
  FreeAndNil(FPDFExportOptions);
  FreeAndNil(FRenderer);
  FreeAndNil(FRenderInfo);
  FreeAndNil(FSubscriber);
  FreeAndNil(FReportFootnotes);
  FreeAndNil(FSavedReportFootnotes);
  FreeAndNil(FSavedReportDocument);
  FreeAndNil(FSavedReportTitle);
  FreeAndNil(FReportTitle);
  FreeAndNil(FReportDocument);
  FreeAndNil(FPrinterPage);
  FreeAndNil(FFontPool);
  FreeAndNil(FFont);
  FreeAndNil(FDefaultFont);
  FinalizeDataStream;
  FreeAndNil(FBackgroundBitmapPool);
  inherited Destroy;
end;

procedure TBasedxReportLink.BeforeDestruction;
begin
  inherited BeforeDestruction;
  DoDestroy;
end;

procedure TBasedxReportLink.Assign(Source: TPersistent);
begin
  if Source is TBasedxReportLink then
    with TBasedxReportLink(Source) do
    begin
      Self.Color := Color;
      Self.DesignerCaption := DesignerCaption;
      Self.Font := Font;
      Self.FootersOnEveryPage := FootersOnEveryPage;
      Self.HeadersOnEveryPage := HeadersOnEveryPage;
      Self.PageNumberFormat := PageNumberFormat;
      Self.PDFExportOptions.Assign(PDFExportOptions);
      Self.RealPrinterPage.BeginUpdate;
      try
        Self.RealPrinterPage := RealPrinterPage;
      finally
        Self.RealPrinterPage.CancelUpdate;
      end;
      Self.ReportDocument := ReportDocument;
      Self.ReportTitle := ReportTitle;
      Self.ScaleFonts := ScaleFonts;
      Self.ShowPageFooter := ShowPageFooter;
      Self.ShowPageHeader := ShowPageHeader;
      Self.ShowPageRowHeader := ShowPageRowHeader;
      Self.TimeFormat := TimeFormat;
      Self.Transparent := Transparent;

      Self.FIsDesignerCaptionAssigned := FIsDesignerCaptionAssigned;
    end
  else
    if Source is TBasedxPrintStyle then
      PrinterPage := TBasedxPrintStyle(Source).PrinterPage
    else
      inherited;
end;

function TBasedxReportLink.GetParentComponent: TComponent;
begin
  Result := ComponentPrinter;
end;

function TBasedxReportLink.HasParent: Boolean;
begin
  Result := IsComponentPrinterAvailable;
end;

procedure TBasedxReportLink.LoadFromIniFile(AIniFile: TCustomIniFile; const ASection: string);
begin
  if AIniFile.ReadBool(ASection, sdxAssignedDateFormat, False) then
    DateFormat := AIniFile.ReadInteger(ASection, sdxDateFormat, DateFormat);
  if AIniFile.ReadBool(ASection, sdxAssignedTimeFormat, False) then
    TimeFormat := AIniFile.ReadInteger(ASection, sdxTimeFormat, TimeFormat);
  if AIniFile.ReadBool(ASection, sdxAssignedPageNumberFormat, False) then
    PageNumberFormat := TdxPageNumberFormat(AIniFile.ReadInteger(ASection, sdxPageNumberFormat, Ord(PageNumberFormat)));
  StartPageIndex := AIniFile.ReadInteger(ASection, sdxStartPageIndex, StartPageIndex);
end;

procedure TBasedxReportLink.LoadFromIniFile(const AFileName: string);
var
  AIniFile: TCustomIniFile;
begin
  AIniFile := TMemIniFile.Create(AFileName);
  try
    LoadFromIniFile(AIniFile, dxGetStoringSectionName(Self));
  finally
    AIniFile.Free;
  end;
end;

procedure TBasedxReportLink.LoadFromRegistry(const APath: string);
var
  ARegIniFile: TRegistryIniFile;
begin
  if APath <> '' then
  begin
    ARegIniFile := TRegistryIniFile.Create('');
    try
      LoadFromIniFile(ARegIniFile, APath);
    finally
      ARegIniFile.Free;
    end;
  end;
end;

procedure TBasedxReportLink.SaveToIniFile(AIniFile: TCustomIniFile; const ASection: string);
begin
  AIniFile.WriteBool(ASection, sdxAssignedDateFormat, fvDate in AssignedFormatValues);
  AIniFile.WriteInteger(ASection, sdxDateFormat, DateFormat);
  AIniFile.WriteBool(ASection, sdxAssignedTimeFormat, fvTime in AssignedFormatValues);
  AIniFile.WriteInteger(ASection, sdxTimeFormat, TimeFormat);
  AIniFile.WriteBool(ASection, sdxAssignedPageNumberFormat, fvPageNumber in AssignedFormatValues);
  AIniFile.WriteInteger(ASection, sdxPageNumberFormat, Integer(PageNumberFormat));
  AIniFile.WriteInteger(ASection, sdxStartPageIndex, StartPageIndex);
end;

procedure TBasedxReportLink.SaveToIniFile(const AFileName: string);
var
  AIniFile: TMemIniFile;
begin
  AIniFile := TMemIniFile.Create(AFileName);
  try
    SaveToIniFile(AIniFile, dxGetStoringSectionName(Self));
    AIniFile.UpdateFile;
  finally
    AIniFile.Free;
  end;
end;

procedure TBasedxReportLink.SaveToRegistry(const APath: string);
var
  ARegIniFile: TRegistryIniFile;
begin
  if APath <> '' then
  begin
    ARegIniFile := TRegistryIniFile.Create('');
    try
      SaveToIniFile(ARegIniFile, APath);
    finally
      ARegIniFile.Free;
    end;
  end;
end;

class function TBasedxReportLink.Aggregable: Boolean;
begin
  Result := True;
end;

class function TBasedxReportLink.CanBeUsedAsStub: Boolean;
begin
  Result := True;
end;

class function TBasedxReportLink.Serializable: Boolean;
begin
  Result := True;
end;

function TBasedxReportLink.DefaultDateFormat: Integer;
begin
  if not (fvDate in AssignedFormatValues) and IsComponentPrinterAvailable then
    Result := ComponentPrinter.DateFormat
  else
    Result := 0;
end;

function TBasedxReportLink.DefaultDesignerCaption: string;
begin
  Result := cxGetResourceString(@sdxReportDesignerCaption);
end;

function TBasedxReportLink.DefaultFont: TFont;
begin
  if FDefaultFont = nil then
  begin
    FDefaultFont := TFont.Create;
    InitializeDefaultFont(FDefaultFont);
  end;
  Result := FDefaultFont;
end;

function TBasedxReportLink.DefaultPageNumberFormat: TdxPageNumberFormat;
begin
  if not (fvPageNumber in AssignedFormatValues) and IsComponentPrinterAvailable then
    Result := ComponentPrinter.PageNumberFormat
  else
    Result := pnfNumeral;
end;

function TBasedxReportLink.DefaultTimeFormat: Integer;
begin
  if not (fvTime in AssignedFormatValues) and IsComponentPrinterAvailable then
    Result := ComponentPrinter.TimeFormat
  else
    Result := 0;
end;

procedure TBasedxReportLink.RestoreDefaults;
begin
  InternalRestoreDefaults;
end;

procedure TBasedxReportLink.RestoreFromOriginal;
begin
  if Component <> nil then InternalRestoreFromOriginal;
end;

function TBasedxReportLink.CheckToDesign: Boolean;
begin
  Result := (DataSource = rldsComponent) and (GetDesignerClass <> nil);
end;

function TBasedxReportLink.DataProviderPresent: Boolean;
begin
  if DataSource = rldsComponent then
    Result := Component <> nil
  else
    Result := PossibleDataStorage(DataStream, False);
end;

function TBasedxReportLink.DesignerExists(AComponentClass: TComponentClass): Boolean;
begin
  Result := GetDesignerClass <> nil;
end;

function TBasedxReportLink.DesignReport: Boolean;
var
  DesignWindowClass: TdxReportLinkDesignWindowClass;
  SaveLink: TBasedxReportLink;
begin
  Result := False;
  DesignWindowClass := GetDesignerClass;
  if DesignWindowClass <> nil then
  begin
    FDesignWindow := DesignWindowClass.Create(nil);
    try
      DesignWindow.ReportLink := Self;
      SaveLink := LinkClass.Create(nil);
      try
        SaveLink.Assign(Self);
        BeforeDesignReport;
        try
          Result := DesignWindow.Execute;
        finally
          AfterDesignReport(Result);
        end;
        if not Result and not DesignWindow.Applyed then
          Assign(SaveLink);
      finally
        SaveLink.Free;
      end;
    finally
      FreeAndNil(FDesignWindow);
    end;
  end;
end;

procedure TBasedxReportLink.DestroyReport;
begin
  Active := False;
end;

procedure TBasedxReportLink.GetPageColRowCount(out APageColCount, APageRowCount: Integer);
begin
  APageColCount := RenderInfo.PageColCount;
  APageRowCount := RenderInfo.PageRowCount;
end;

procedure TBasedxReportLink.Initialize;
begin
end;

function TBasedxReportLink.IsEmptyPage(APageIndex: Integer): Boolean;
begin
  if (APageIndex > -1) and (APageIndex < RenderInfo.VirtualPageCount) then
    Result := RenderInfo.PageRenderInfos[APageIndex].IsEmptyPage
  else
    Result := True;
end;

function TBasedxReportLink.IsEmptyReport: Boolean;
begin
  Result := (FRenderInfo = nil) or (RenderInfo.NonEmptyPageCount = 0);
end;

procedure TBasedxReportLink.RebuildReport;
begin
  if HasDesignWindow then
    DesignWindow.BeforeRebuildReport;
  try
    Active := False;
    Active := True;
  finally
    if HasDesignWindow then
      DesignWindow.AfterRebuildReport;
  end;
end;

procedure TBasedxReportLink.SetComponentUnconditionally(Value: TComponent);
begin
  FComponent := Value;
  if Active or FStreamedActive then
  try
    if FStreamedActive then FStreamedActive := False;
    RebuildReport;
    if not IsLoading and IsComponentPrinterAvailable and not ComponentPrinter.AutoUpdateDateTime then
    begin
      DateTime := Now;
      DesignerModified;
    end;
    LinkModified(False);
  except
    on E: Exception do
    begin
      FComponent := nil;
      if IsDesigning then
        ShowException(E, ExceptAddr)
      else
        raise;
    end;
  end;
  if Value <> nil then
    Value.FreeNotification(Self);
end;

class procedure TBasedxReportLink.GetSupportedComponentList(AList: TdxClassList);
begin
  dxPSGetLinkSupportedComponentsList(AList, LinkClass);
end;

class function TBasedxReportLink.IsSupportedCompClass(AComponentClass: TClass): Boolean;
var
  AList: TList;
begin
  AList := dxPSLinkClassByCompClassEx(AComponentClass);
  try
    Result := (AList <> nil) and (AList.IndexOf(LinkClass) >= 0);
  finally
    AList.Free;
  end;
end;

class function TBasedxReportLink.LinkClass: TdxReportLinkClass;
begin
  Result := TdxReportLinkClass(GetTypeData(ClassInfo)^.ClassType);
end;

class function TBasedxReportLink.Supports(AnObject: TObject): Boolean;
begin
  Result := (AnObject <> nil) and Supports(AnObject.ClassType);
end;

class function TBasedxReportLink.Supports(AClass: TClass): Boolean;
begin
  Result := (AClass <> nil) and AClass.InheritsFrom(TComponent) and IsSupportedCompClass(AClass);
end;

procedure TBasedxReportLink.ExportToPDF;
begin
  dxPSExportToPDF(Self);
end;

procedure TBasedxReportLink.ExportToPDF(
  const AFileName: string; ACanShowDialog: Boolean = True;
  ASettings: TdxPSPDFReportExportOptions = nil);
begin
  dxPSExportToPDFFile(AFileName, Self, ACanShowDialog, ASettings);
end;

function TBasedxReportLink.Print(AShowDialog: Boolean): Boolean;
begin
  Result := Print(AShowDialog, nil);
end;

function TBasedxReportLink.Print(
  AShowDialog: Boolean; APrintDialogData: PdxPrintDlgData): Boolean;
begin
  Result := False;
  if Assigned(ComponentPrinter) then
  begin
    IsCurrentLink := True;
    Result := ComponentPrinter.Print(AShowDialog, APrintDialogData, Self);
  end;
end;

procedure TBasedxReportLink.PrintEx(
  APageNums: TdxPageNumbers; ACopies: Integer; ACollate: Boolean);
begin
  if Assigned(ComponentPrinter) then
  begin
    IsCurrentLink := True;
    ComponentPrinter.PrintEx(APageNums, ACopies, ACollate, Self);
  end;
end;

procedure TBasedxReportLink.PrintPages(const APageIndexes: TdxPSPageIndexes);
begin
  if IsComponentPrinterAvailable then
  begin
    IsCurrentLink := True;
    ComponentPrinter.PrintPages(APageIndexes, Self);
  end;
end;

procedure TBasedxReportLink.PrintPagesEx(const APageIndexes: TdxPSPageIndexes;
  APageNums: TdxPageNumbers; ACopyCount: Integer; ACollate: Boolean);
begin
  if IsComponentPrinterAvailable then
  begin
    IsCurrentLink := True;
    ComponentPrinter.PrintPagesEx(APageIndexes, APageNums, ACopyCount, ACollate, Self);
  end;
end;

procedure TBasedxReportLink.BuildPageSetupMenu(ARootItem: TComponent;
  AData: Pointer; AIncludeDefineItem: Boolean = True);
var
  MenuBuilder: TAbstractdxPSPageSetupMenuBuilder;
  Styles: TStringList;
begin
  if StyleManager = nil then Exit;
  MenuBuilder := dxPSPageSetupMenuBuilderFactory.ActiveBuilder.Create;
  try
    try
      Styles := TStringList.Create;
      try
        GetFilteredStyles(Styles);
        MenuBuilder.BuildPageSetupMenu(ARootItem, AData, AIncludeDefineItem,
          Styles, CurrentPrintStyle, StyleClick, DefineStylesClick);
      finally
        Styles.Free;
      end;
    except
      Application.HandleException(Self);
    end;
  finally
    MenuBuilder.Free;
  end;
end;

procedure TBasedxReportLink.DefinePrintStylesDlg;
var
  PreviewBtnClicked, PrintBtnClicked: Boolean;
begin
  if StyleManager <> nil then
  begin
    if IsComponentPrinterAvailable then
      Include(ComponentPrinter.FState, cpsDefineStylesDialog);

    try
      StyleManager.DefinePrintStylesDlg(PreviewBtnClicked, PrintBtnClicked);
    finally
      if IsComponentPrinterAvailable then
        Exclude(ComponentPrinter.FState, cpsDefineStylesDialog);
    end;

    if PrintBtnClicked then
      Print(True, nil)
    else
      if PreviewBtnClicked then Preview(True);
  end;
end;

procedure TBasedxReportLink.GetFilteredStyles(AStrings: TStrings);
var
  I: Integer;
  Style: TBasedxPrintStyle;
begin
  if StyleManager <> nil then
    for I := 0 to StyleManager.Count - 1 do
    begin
      Style := StyleManager.Styles[I];
      if IsSupportedStyle(Style) then
        AStrings.AddObject(Style.StyleCaption, Style);
    end;
end;

function TBasedxReportLink.PageSetup: Boolean;
var
  PreviewBtnClicked, PrintBtnClicked: Boolean;
begin
  Result := PageSetupEx(0, True, True, PreviewBtnClicked, PrintBtnClicked);
  if PreviewBtnClicked then
    Preview(True)
  else
    if PrintBtnClicked then
      Result := Print(True, nil);
end;

function TBasedxReportLink.PageSetupEx(
  AActivePageIndex: Integer; AShowPreviewBtn, AShowPrintBtn: Boolean;
  out APreviewBtnClicked, APrintBtnClicked: Boolean): Boolean;

  function GetPrintStyle(out APrintStyleExists: Boolean): TBasedxPrintStyle;
  begin
    APrintStyleExists := Assigned(CurrentPrintStyle);
    if APrintStyleExists then
      Result := CurrentPrintStyle
    else
    begin
      Result := TdxPSPrintStyle.Create(nil);
      Result.Assign(Self);
    end;
  end;

  procedure PreparePageSetup;
  begin
    if Assigned(ComponentPrinter) then
    begin
      if (dxPrintDevice <> nil) and Assigned(dxPrintDevice.DeviceMode) then
        PrinterPage.InitFromPrintDevice;
      ComponentPrinter.PreparePageSetup;
    end;
  end;

  procedure UnpreparePageSetup;
  begin
    if Assigned(ComponentPrinter) then
      ComponentPrinter.UnpreparePageSetup;
  end;

var
  APrintStyle: TBasedxPrintStyle;
  APrintStyleExists: Boolean;
begin
  APrintStyle := GetPrintStyle(APrintStyleExists);
  try
    PreparePageSetup;
    try
      if not DataProviderPresent then
      begin
        AShowPreviewBtn := False;
        AShowPrintBtn := False;
      end;
      Result := APrintStyle.PageSetup(AActivePageIndex, AShowPreviewBtn,
        AShowPrintBtn, APreviewBtnClicked, APrintBtnClicked);
    finally
      UnpreparePageSetup;
    end;
    if Result and not APrintStyleExists then
    begin
      Assign(APrintStyle);
      if IsComponentPrinterAvailable then
        ComponentPrinter.InitDevModeFromPrinterPageSettings(RealPrinterPage);
    end;
    if IsComponentPrinterAvailable then
      ComponentPrinter.DoPageSetup(Self, Result);
  finally
    if not APrintStyleExists then
      FreeAndNil(APrintStyle);
  end;
end;

procedure TBasedxReportLink.Preview(Modal: Boolean = True);
begin
  if IsComponentPrinterAvailable then
  begin
    IsCurrentLink := True;
    ComponentPrinter.Preview(Modal, Self);
  end;
end;

function TBasedxReportLink.PreviewExists: Boolean;
begin
  Result := IsCurrentLink and ComponentPrinter.PreviewExists;
end;

function TBasedxReportLink.ShowDateTimeFormatsDlg: Boolean;
var
  AData: TdxDateTimeFormatDlgData;
begin
  FillChar(AData, SizeOf(TdxDateTimeFormatDlgData), 0);
  AData.DateFormats := dxPgsDlg.DateFormats;
  AData.TimeFormats := dxPgsDlg.TimeFormats;
  AData.DateFormatIndex := DateFormat;
  AData.TimeFormatIndex := TimeFormat;
  if IsComponentPrinterAvailable then
    AData.AutoUpdateDateTime := ComponentPrinter.AutoUpdateDateTime;
  AData.ShowAsDefaultButton := True;

  Result := dxShowDateTimeFormatDlg(AData);
  if Result then
  begin
    if IsComponentPrinterAvailable then
    begin
      ComponentPrinter.AutoUpdateDateTime := AData.AutoUpdateDateTime;
      if AData.SetDateTimeFormatAsDefault then
      begin
        ComponentPrinter.DateFormat := AData.DateFormatIndex;
        ComponentPrinter.TimeFormat := AData.TimeFormatIndex;
      end;
    end;
    DateFormat := AData.DateFormatIndex;
    TimeFormat := AData.TimeFormatIndex;
  end;
end;

function TBasedxReportLink.ShowPageNumberFormatsDlg: Boolean;
var
  AData: TdxPageNumberFormatDlgData;
begin
  FillChar(AData, SizeOf(TdxPageNumberFormatDlgData), 0);
  AData.PageNumberFormats := dxPgsDlg.PageNumberFormats;
  AData.PageNumberFormat := PageNumberFormat;
  AData.AllowContinueFromPrevSection := AllowContinuousPageIndexes;
  if AData.AllowContinueFromPrevSection then
    AData.ContinueFromPrevSection := ContinuousPageIndexes;
  AData.StartPageIndex := StartPageIndex;
  AData.ShowAsDefaultButton := True;

  Result := dxShowPageNumberFormatDlg(AData);
  if Result then
  begin
    if IsComponentPrinterAvailable and AData.SetPageNumberFormatAsDefault then
      ComponentPrinter.PageNumberFormat := AData.PageNumberFormat;
    PageNumberFormat := AData.PageNumberFormat;
    StartPageIndex := AData.StartPageIndex;
    if AllowContinuousPageIndexes then
      ContinuousPageIndexes := AData.ContinueFromPrevSection;
  end;
end;

function TBasedxReportLink.ShowFootnotesPropertiesDlg: Boolean;
begin
  Result := False;
  if CanChangeFootnotes then
  begin
    Result := dxShowReportFootnotesPropertiesDlg(ReportFootnotes);
    if Result and HasExternalListeners and DataProviderPresent then
      RebuildReport;
  end;
end;

function TBasedxReportLink.ShowTitlePropertiesDlg: Boolean;
begin
  Result := False;
  if CanChangeTitle then
  begin
    Result := dxShowReportTitlePropertiesDlg(ReportTitle);
    if Result and HasExternalListeners and DataProviderPresent then
      RebuildReport;
  end;
end;

function TBasedxReportLink.SupportsScaling: Boolean;
begin
  Result := True;
end;

function TBasedxReportLink.CanChangeFootnotes: Boolean;
begin
  Result := (rlcFootnotes in Capabilities) and not IsAggregated and (DataSource = rldsComponent) and
    ((ComponentPrinter = nil) or not ComponentPrinter.IsExplorerMode);
end;

function TBasedxReportLink.CanChangeTitle: Boolean;
begin
  Result := (rlcTitle in Capabilities) and not IsAggregated and (DataSource = rldsComponent) and
    ((ComponentPrinter = nil) or not ComponentPrinter.IsExplorerMode);
end;

function TBasedxReportLink.CanLoadData: Boolean;
begin
  Result := True;
end;

function TBasedxReportLink.CanSaveData: Boolean;
begin
  Result := Serializable and not IsEmptyReport and (DataSource = rldsComponent);
end;

function TBasedxReportLink.CanUnloadData: Boolean;
begin
  Result := DataSource = rldsExternalStorage;
end;

function TBasedxReportLink.GetNewReportStorageName: string;
var
  P: Integer;
begin
  Result := ReportTitle.Text;
  if Result = '' then
    Result := ReportDocument.Caption;
  if Result <> '' then
  begin
    P := Pos(dxCRLF, Result);
    if P <> 0 then
      Result := Copy(Result, 1, P - 1);
  end;
  if Result = '' then
    Result := cxGetResourceString(@sdxNewReport);
  DoGetNewReportStorageName(Result);
  Result := dxPSFixInvalidFileNameChars(Result);
end;

procedure TBasedxReportLink.LoadDataFromFile(const AName: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(AName, fmOpenRead or fmShareDenyWrite);
  try
    FStorageName := AName;
    LoadDataFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TBasedxReportLink.LoadDataFromStream(AStream: TStream);
begin
  if PossibleDataStorage(AStream, True) then
  begin
    CopyDataStreamFrom(AStream);
    if DataSource <> rldsExternalStorage then
      DataSource := rldsExternalStorage
    else
      if IsRebuildNeededAndAllowed then
        RebuildReport;
  end;
end;

procedure TBasedxReportLink.SaveDataToFile(const AName: string);
var
  AStream: TStream;
begin
  try
    AStream := TFileStream.Create(AName, fmCreate);// or fmOpenWrite);
    try
      SaveDataToStream(AStream);
    finally
      AStream.Free;
    end;
  except
    // Note: File is locked
  end;
end;

procedure TBasedxReportLink.SaveDataToStream(AStream: TStream);
var
  AWriter: TdxPSDataWriter;
begin
  if DataProviderPresent and (FReportCells = nil) then
    RebuildReport;

  if ReportCells <> nil then //HasData
  begin
    PrepareLongOperation;
    try
      Include(FState, rlsDataSaving);
      try
        AWriter := CreateDataWriter(AStream);
        try
          InternalWriteData(AWriter);
        finally
          AWriter.Free;
        end;
      finally
        Exclude(FState, rlsDataSaving);
      end;
    finally
      UnprepareLongOperation;
    end;
  end;
end;

class function TBasedxReportLink.ExtractComponentClass(
  AStream: TStream; ARaiseException: Boolean = False): TComponentClass;
var
  AStorageInfo: TdxPSDataStorageInfo;
begin
  try
    AStorageInfo := ExtractStorageInfo(AStream, ARaiseException);
    try
      Result := AStorageInfo.ComponentClass;
    finally
      FinalizeStorageInfo(AStorageInfo);
    end;
  except
    Result := nil;
    if ARaiseException then
      raise;
  end;
end;

class function TBasedxReportLink.ExtractComponentClassName(
  AStream: TStream; ARaiseException: Boolean = False): string;
var
  AStorageInfo: TdxPSDataStorageInfo;
begin
  try
    AStorageInfo := ExtractStorageInfo(AStream, ARaiseException);
    try
      Result := dxShortStringToString(AStorageInfo.ComponentClassName);
    finally
      FinalizeStorageInfo(AStorageInfo);
    end;
  except
    Result := '';
    if ARaiseException then
      raise;
  end;
end;

class function TBasedxReportLink.ExtractLinkClass(
  AStream: TStream; ARaiseException: Boolean = False): TdxReportLinkClass;
var
  AStorageInfo: TdxPSDataStorageInfo;
begin
  try
    AStorageInfo := ExtractStorageInfo(AStream, ARaiseException);
    try
      Result := AStorageInfo.LinkClass;
    finally
      FinalizeStorageInfo(AStorageInfo);
    end;
  except
    Result := nil;
    if ARaiseException then
      raise;
  end;
end;

class function TBasedxReportLink.ExtractOffsetTable(AStream: TStream;
  ARaiseException: Boolean = False): TdxPSDataStorageOffsetTable;
var
  P: Int64;
  AReader: TdxPSDataReader;
begin
  Result := nil;
  try
    P := AStream.Position;
    try
      AStream.Position := 0;
      AReader := CreateDataReader(AStream);
      try
        Result := ReadOffsetTable(AReader);
      finally
        AReader.Free;
      end;
    finally
      AStream.Position := P;
    end;
  except
    FreeAndNil(Result);
    if ARaiseException then
      raise;
  end;
end;

class function TBasedxReportLink.ExtractReportDocument(AStream: TStream;
  ARaiseException: Boolean = False): TdxPSReportDocument;
var
  AOffsetTable: TdxPSDataStorageOffsetTable;
  P: Int64;
  AReader: TdxPSDataReader;
begin
  Result := nil;
  try
    AOffsetTable := ExtractOffsetTable(AStream, ARaiseException);
    if AOffsetTable <> nil then
    try
      P := AStream.Position;
      AStream.Position := AOffsetTable.Document;
      try
        AReader := CreateDataReader(AStream);
        try
          Result := GetReportDocumentClass.Create(nil);
          ReadReportDocument(AReader, Result);
        finally
          AReader.Free;
        end;
      finally
        AStream.Position := P;
      end;
    finally
      AOffsetTable.Free;
    end;
  except
    FreeAndNil(Result);
    if ARaiseException then
      raise;
  end;
end;

class function TBasedxReportLink.ExtractStorageInfo(
  AStream: TStream; ARaiseException: Boolean = False): TdxPSDataStorageInfo;
var
  AOffsetTable: TdxPSDataStorageOffsetTable;
  P: Int64;
  AReader: TdxPSDataReader;
begin
  try
    AOffsetTable := ExtractOffsetTable(AStream, ARaiseException);
    if AOffsetTable <> nil then
    try
      P := AStream.Position;
      AStream.Position := AOffsetTable.Information;
      try
        AReader := CreateDataReader(AStream);
        try
          Result := ReadStorageInfo(AReader);
        finally
          AReader.Free;
        end;
      finally
        AStream.Position := P;
      end;
    finally
      AOffsetTable.Free;
    end;
  except
    FreeAndNil(Result);
    if ARaiseException then
      raise;
  end;
end;

class function TBasedxReportLink.ExtractStorageVersion(AStream: TStream;
  ARaiseException: Boolean = False): Integer;
var
  AStorageInfo: TdxPSDataStorageInfo;
begin
  try
    AStorageInfo := ExtractStorageInfo(AStream, ARaiseException);
    try
      Result := AStorageInfo.StorageVersion;
    finally
      FinalizeStorageInfo(AStorageInfo);
    end;
  except
    Result := dxPSGlbl.dxPSInvalidStorageVersion;
    if ARaiseException then
      raise;
  end;
end;

class procedure TBasedxReportLink.FinalizeStorageInfo(var AStorageInfo: TdxPSDataStorageInfo);
begin
  FreeAndNil(AStorageInfo);
end;

class function TBasedxReportLink.PossibleDataStorage(
  AStream: TStream; ARaiseException: Boolean = False): Boolean;
var
  AStorageVersion: Integer;
begin
  Result := AStream <> nil;
  if Result then
  try
    AStorageVersion := ExtractStorageVersion(AStream, ARaiseException);
    Result := (AStorageVersion >= 1) and (AStorageVersion <= dxPSGlbl.dxPSStorageVersion);
  except
    Result := False;
    if ARaiseException then
      raise;
  end;
end;

class function TBasedxReportLink.PossibleDataStorage(const AFileName: string;
  ARaiseException: Boolean = False): Boolean;
var
  AStream: TFileStream;
begin
  Result := FileExists(AFileName);
  if Result then
  try
    AStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
    try
      Result := PossibleDataStorage(AStream, ARaiseException);
    finally
      AStream.Free;
    end;
  except
    Result := False;
    if ARaiseException then
      raise;
  end;
end;

function TBasedxReportLink.AddBackgroundBitmapToPool(ABitmap: TGraphic): Integer;
begin
  Result := BackgroundBitmapPool.Add(ABitmap);
end;

function TBasedxReportLink.AddFontToPool(AFont: TFont): Integer;
begin
  Result := FontPool.Add(AFont);
end;

function TBasedxReportLink.AddFontToPool(const AName: string; AColor: TColor; APitch: TFontPitch;
  AStyle: TFontStyles; ASize: Integer): Integer;
begin
  Result := FontPool.Add(AName, AColor, APitch, AStyle, ASize);
end;

function TBasedxReportLink.CreateGroupLookAndFeel(AClass: TdxPSReportGroupLookAndFeelClass;
  ACheckExisting: Boolean = True): TdxPSReportGroupLookAndFeel;
begin
  if ReportCells <> nil then
    Result := ReportCells.CreateGroupLookAndFeel(AClass, ACheckExisting)
  else
    Result := nil;
end;

function TBasedxReportLink.FindGroupLookAndFeelByClass(AClass: TdxPSReportGroupLookAndFeelClass): TdxPSReportGroupLookAndFeel;
begin
  if ReportCells <> nil then
    Result := ReportCells.FindGroupLookAndFeelByClass(AClass)
  else
    Result := nil;
end;

function TBasedxReportLink.IndexOfReportGroupLookAndFeel(ALookAndFeel: TdxPSReportGroupLookAndFeel): Integer;
begin
  if ReportCells <> nil then
    Result := ReportCells.IndexOfReportGroupLookAndFeel(ALookAndFeel)
  else
    Result := -1;
end;

procedure TBasedxReportLink.DrawPageHeader(APageIndex: Integer; ARect: TRect;
  ATitleParts: TdxPageTitleParts; ADrawBackground: Boolean);
begin
  if FPainting then
    Renderer.RenderPageHeaderOrFooterContent(RealPrinterPage.PageHeader,
      APageIndex, ARect, ATitleParts, ADrawBackground);
end;

procedure TBasedxReportLink.DrawPageFooter(APageIndex: Integer; ARect: TRect;
  ATitleParts: TdxPageTitleParts; ADrawBackground: Boolean);
begin
  if FPainting then
    Renderer.RenderPageHeaderOrFooterContent(RealPrinterPage.PageFooter,
      APageIndex, ARect, ATitleParts, ADrawBackground);
end;

procedure TBasedxReportLink.DrawCheckBox(Canvas: TCanvas; var R: TRect;
  Checked, Enabled, FlatBorder: Boolean; BoldBorder: Boolean = False);
const
  EdgeStyleMap: array[Boolean, Boolean] of TdxCheckButtonEdgeStyle = (
    (cbes3D, cbesBoldFlat), (cbesUltraFlat, cbesUltraFlat)
  );
var
  ARenderCanvas: TdxPSReportRenderCustomCanvas;
begin
  if FPainting then
  begin
    ARenderCanvas := TdxPSReportRenderCanvas.Create(Canvas);
    try
      Renderer.DrawCheckBox(ARenderCanvas, R, Checked,
        Enabled, False, EdgeStyleMap[FlatBorder, BoldBorder]);
    finally
      ARenderCanvas.Free;
    end;
  end;
end;

procedure TBasedxReportLink.DrawCheckBox(Canvas: TCanvas; var R: TRect;
  Checked, Enabled, IsRadio: Boolean; EdgeStyle: TdxCheckButtonEdgeStyle;
  BorderColor: TColor = clWindowText);
var
  ARenderCanvas: TdxPSReportRenderCanvas;
begin
  if FPainting then
  begin
    ARenderCanvas := TdxPSReportRenderCanvas.Create(Canvas);
    try
      Renderer.DrawCheckBox(ARenderCanvas, R,
        Checked, Enabled, IsRadio, EdgeStyle, BorderColor);
    finally
      ARenderCanvas.Free;
    end;
  end;
end;

procedure TBasedxReportLink.DrawEdge(Canvas: TCanvas; var R: TRect;
  EdgeMode: TdxCellEdgeMode; InnerEdge, OuterEdge: TdxCellEdgeStyle;
  Sides: TdxCellSides = [csLeft..csBottom]; Soft: Boolean = True);
var
  ARenderCanvas: TdxPSReportRenderCanvas;
begin
  if FPainting then
  begin
    ARenderCanvas := TdxPSReportRenderCanvas.Create(Canvas);
    try
      Renderer.DrawEdge(ARenderCanvas, R, EdgeMode, OuterEdge, Sides, Soft, -1);
    finally
      ARenderCanvas.Free;
    end;
  end;
end;

procedure TBasedxReportLink.DrawEllipse(Canvas: TCanvas; R: TRect; ForeColor, BkColor: TColor;
  Pattern: TdxPSFillPatternClass; BorderColor: TColor; BorderThickness: Integer = 1);
var
  ARenderCanvas: TdxPSReportRenderCustomCanvas;
begin
  if FPainting then
  begin
    ARenderCanvas := TdxPSReportRenderCanvas.Create(Canvas);
    try
      Renderer.DrawEllipse(ARenderCanvas, R, ForeColor,
        BkColor, Pattern, BorderColor, BorderThickness);
    finally
      ARenderCanvas.Free;
    end;
  end;
end;

procedure TBasedxReportLink.DrawExpandButton(Canvas: TCanvas; var R: TRect;
  Expanded, DrawBorder, Edge3D, Edge3DSoft, Shadow, FillInterior: Boolean;
  BorderColor, InteriorColor: TColor);
var
  ARenderCanvas: TdxPSReportRenderCanvas;
begin
  if FPainting then
  begin
    ARenderCanvas := TdxPSReportRenderCanvas.Create(Canvas);
    try
      Renderer.DrawExpandButton(ARenderCanvas, R, Expanded, DrawBorder,
        Edge3D, Edge3DSoft, Shadow, FillInterior, BorderColor, InteriorColor);
    finally
      ARenderCanvas.Free;
    end;
  end;
end;

procedure TBasedxReportLink.DrawGlyph(DC: HDC; const R: TRect; AGlyph: Byte);
var
  ACanvas: TCanvas;
  AReportRenderCanvas: TdxPSReportRenderCanvas;
begin
  if FPainting then
  begin
    ACanvas := TCanvas.Create;
    try
      ACanvas.Handle := DC;
      AReportRenderCanvas := TdxPSReportRenderCanvas.Create(ACanvas);
      try
        Renderer.DrawGlyph(AReportRenderCanvas, R, AGlyph);
      finally
        AReportRenderCanvas.Free;
        ACanvas.Handle := 0;
      end;
    finally
      ACanvas.Free;
    end;
  end;
end;

procedure TBasedxReportLink.DrawGraphic(Canvas: TCanvas; var R: TRect;
  const ClipRect: TRect; ImageList: TCustomImageList; ImageIndex: Integer;
  Graphic: TGraphic; GraphicTransparent, Transparent: Boolean; BkColor: TColor);
var
  ARenderCanvas: TdxPSReportRenderCanvas;
begin
  if FPainting then
  begin
    ARenderCanvas := TdxPSReportRenderCanvas.Create(Canvas);
    try
      Renderer.DrawGraphic(ARenderCanvas, R, ClipRect, ImageList, ImageIndex,
        Graphic, GraphicTransparent, Transparent, BkColor);
    finally
      ARenderCanvas.Free;
    end;
  end;
end;

procedure TBasedxReportLink.DrawGraphicEx(Canvas: TCanvas; var R: TRect;
  const ClipRect: TRect; ImageList: TCustomImageList; ImageIndex: Integer;
  Graphic: TGraphic; GraphicTransparent, Transparent: Boolean;
  BkColor, ForeColor: TColor; Pattern: TdxPSFillPatternClass;
  AActualImageBuffering: TdxCellImageActualBuffering = cibAlways);
var
  ARenderCanvas: TdxPSReportRenderCanvas;
begin
  if FPainting then
  begin
    ARenderCanvas := TdxPSReportRenderCanvas.Create(Canvas);
    try
      Renderer.DrawGraphicEx(ARenderCanvas, R, ClipRect, ImageList, ImageIndex,
        Graphic, GraphicTransparent, Transparent, BkColor, ForeColor, Pattern,
        AActualImageBuffering);
    finally
      ARenderCanvas.Free;
    end;
  end;
end;

procedure TBasedxReportLink.DrawRectangle(Canvas: TCanvas; R: TRect;
  ForeColor, BkColor: TColor; ContentPattern: TdxPSFillPatternClass;
  BorderColor: TColor; BorderThickness: Integer = 1);
var
  ARenderCanvas: TdxPSReportRenderCanvas;
begin
  if FPainting then
  begin
    ARenderCanvas := TdxPSReportRenderCanvas.Create(Canvas);
    try
      Renderer.DrawRectangle(ARenderCanvas, R, ForeColor,
        BkColor, ContentPattern, BorderColor, BorderThickness);
    finally
      ARenderCanvas.Free;
    end;
  end;
end;

procedure TBasedxReportLink.DrawRoundRect(Canvas: TCanvas; R: TRect;
  CornerWidth, CornerHeight: Integer; ForeColor, BkColor: TColor;
  ContentPattern: TdxPSFillPatternClass; BorderColor: TColor;
  BorderThickness: Integer = 1);
var
  ARenderCanvas: TdxPSReportRenderCustomCanvas;
begin
  if FPainting then
  begin
    ARenderCanvas := TdxPSReportRenderCanvas.Create(Canvas);
    try
      Renderer.DrawRoundRect(ARenderCanvas, R, CornerWidth, CornerHeight,
        ForeColor, BkColor, ContentPattern, BorderColor, BorderThickness);
    finally
      ARenderCanvas.Free;
    end;
  end;
end;

procedure TBasedxReportLink.DrawSortMark(
  Canvas: TCanvas; var R: TRect; SortOrder: TdxCellSortOrder; Mono: Boolean);
var
  ARenderCanvas: TdxPSReportRenderCanvas;
begin
  if FPainting and (SortOrder <> csoNone) then
  begin
    ARenderCanvas := TdxPSReportRenderCanvas.Create(Canvas);
    try
      Renderer.DrawSortMark(ARenderCanvas, R, SortOrder, Mono);
    finally
      ARenderCanvas.Free;
    end;
  end;
end;

procedure TBasedxReportLink.DrawText(Canvas: TCanvas; var R: TRect; AIndent: Integer;
  const Text: string; Font: TFont; BkColor: TColor; TextAlignX: TcxTextAlignX;
  TextAlignY: TcxTextAlignY; FillBackground, Multiline, EndEllipsis: Boolean);
var
  ARenderCanvas: TdxPSReportRenderCanvas;
begin
  if FPainting then
  begin
    ARenderCanvas := TdxPSReportRenderCanvas.Create(Canvas);
    try
      Renderer.DrawText(ARenderCanvas, R, 0, AIndent, 0, Text, Font, BkColor,
        TextAlignX, TextAlignY, FillBackground, Multiline, EndEllipsis, True, True);
    finally
      ARenderCanvas.Free;
    end;
  end;
end;

procedure TBasedxReportLink.DrawTextEx(Canvas: TCanvas; var R: TRect;
  MaxLineCount: Integer; LeftIndent, RightIndent: Integer; const Text: string;
  Font: TFont; BkColor: TColor; TextAlignX: TcxTextAlignX; TextAlignY: TcxTextAlignY;
  FillBackground, Multiline, EndEllipsis, PreventLeftTextExceed,
  PreventTopTextExceed: Boolean);
var
  ARenderCanvas: TdxPSReportRenderCanvas;
begin
  if FPainting then
  begin
    ARenderCanvas := TdxPSReportRenderCanvas.Create(Canvas);
    try
      Renderer.DrawText(ARenderCanvas, R, MaxLineCount, LeftIndent, RightIndent,
        Text, Font, BkColor, TextAlignX, TextAlignY, FillBackground, Multiline,
        EndEllipsis, PreventLeftTextExceed, PreventTopTextExceed);
    finally
      ARenderCanvas.Free;
    end;
  end;
end;

procedure TBasedxReportLink.FillEllipse(Canvas: TCanvas; const R: TRect; Color: TColor);
var
  ARenderCanvas: TdxPSReportRenderCanvas;
begin
  if FPainting then
  begin
    ARenderCanvas := TdxPSReportRenderCanvas.Create(Canvas);
    try
      Renderer.FillEllipse(ARenderCanvas, R, Color);
    finally
      ARenderCanvas.Free;
    end;
  end;
end;

procedure TBasedxReportLink.FillEllipseEx(Canvas: TCanvas; const R: TRect;
  ForeColor, BkColor: TColor; Pattern: TdxPSFillPatternClass);
var
  ARenderCanvas: TdxPSReportRenderCanvas;
begin
  if FPainting then
  begin
    ARenderCanvas := TdxPSReportRenderCanvas.Create(Canvas);
    try
      Renderer.FillEllipseEx(ARenderCanvas, R, ForeColor, BkColor, Pattern);
    finally
      ARenderCanvas.Free;
    end;
  end;
end;

procedure TBasedxReportLink.FillRectEx(Canvas: TCanvas; const R: TRect;
  ForeColor, BkColor: TColor; Pattern: TdxPSFillPatternClass);
var
  ARenderCanvas: TdxPSReportRenderCustomCanvas;
begin
  if FPainting then
  begin
    ARenderCanvas := TdxPSReportRenderCanvas.Create(Canvas);
    try
      Renderer.FillRectEx(ARenderCanvas, R, ForeColor, BkColor, Pattern);
    finally
      ARenderCanvas.Free;
    end;
  end;
end;

procedure TBasedxReportLink.FillRoundRect(Canvas: TCanvas; const R: TRect;
  CornerWidth, CornerHeight: Integer; Color: TColor);
var
  ARenderCanvas: TdxPSReportRenderCanvas;
begin
  if FPainting then
  begin
    ARenderCanvas := TdxPSReportRenderCanvas.Create(Canvas);
    try
      Renderer.FillRoundRect(ARenderCanvas, R, CornerWidth, CornerHeight, Color);
    finally
      ARenderCanvas.Free;
    end;
  end;
end;

procedure TBasedxReportLink.FillRoundRectEx(Canvas: TCanvas; const R: TRect;
  CornerWidth, CornerHeight: Integer; ForeColor, BkColor: TColor;
  Pattern: TdxPSFillPatternClass);
var
  ARenderCanvas: TdxPSReportRenderCanvas;
begin
  if FPainting then
  begin
    ARenderCanvas := TdxPSReportRenderCanvas.Create(Canvas);
    try
      Renderer.FillRoundRectEx(ARenderCanvas, R,
        CornerWidth, CornerHeight, ForeColor, BkColor, Pattern);
    finally
      ARenderCanvas.Free;
    end;
  end;
end;

procedure TBasedxReportLink.FillRgnEx(Canvas: TCanvas; Rgn: TcxRegionHandle;
  ForeColor, BkColor: TColor; Pattern: TdxPSFillPatternClass);
var
  ARenderCanvas: TdxPSReportRenderCanvas;
begin
  if FPainting then
  begin
    ARenderCanvas := TdxPSReportRenderCanvas.Create(Canvas);
    try
      Renderer.FillRgnEx(ARenderCanvas, Rgn, ForeColor, BkColor, Pattern);
    finally
      ARenderCanvas.Free;
    end;
  end;
end;

procedure TBasedxReportLink.FrameEllipse(
  Canvas: TCanvas; R: TRect; Color: TColor; Thickness: Integer = 1);
var
  ARenderCanvas: TdxPSReportRenderCanvas;
begin
  if FPainting then
  begin
    ARenderCanvas := TdxPSReportRenderCanvas.Create(Canvas);
    try
      Renderer.FrameEllipse(ARenderCanvas, R, Color, Thickness);
    finally
      ARenderCanvas.Free;
    end;
  end;
end;

procedure TBasedxReportLink.FrameRect(Canvas: TCanvas; R: TRect; Color: TColor;
  Sides: TdxCellSides = [csLeft..csBottom]; Thickness: Integer = 1);
var
  ARenderCanvas: TdxPSReportRenderCanvas;
begin
  if FPainting then
  begin
    ARenderCanvas := TdxPSReportRenderCanvas.Create(Canvas);
    try
      Renderer.FrameRect(ARenderCanvas, R, Color, Sides, Thickness);
    finally
      ARenderCanvas.Free;
    end;
  end;
end;

procedure TBasedxReportLink.FrameRoundRect(Canvas: TCanvas; R: TRect;
  CornerWidth, CornerHeight: Integer; Color: TColor; Thickness: Integer = 1);
var
  ARenderCanvas: TdxPSReportRenderCanvas;
begin
  if FPainting then
  begin
    ARenderCanvas := TdxPSReportRenderCanvas.Create(Canvas);
    try
      Renderer.FrameRoundRect(ARenderCanvas, R, CornerWidth, CornerHeight, Color, Thickness);
    finally
      ARenderCanvas.Free;
    end;
  end;
end;

procedure TBasedxReportLink.AssignTo(Dest: TPersistent);
begin
  if Dest is TBasedxPrintStyle then
    TBasedxPrintStyle(Dest).PrinterPage := PrinterPage
  else
    inherited;
end;

procedure TBasedxReportLink.ChangeScale(M, D: Integer);
begin
  inherited;
  TdxPrinterPageAccess(PrinterPage).ChangeScale(M, D);
  Font.Height := MulDiv(Font.Height, M, D);
end;

procedure TBasedxReportLink.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('BuiltInReportLink', ReadBuiltIn, WriteBuiltIn, True);
  Filer.DefineProperty('IsDesignerCaptionAssigned',
    ReadIsDesignerCaptionAssigned, WriteIsDesignerCaptionAssigned,
    FIsDesignerCaptionAssigned and (DesignerCaption = ''));
  Filer.DefineProperty('LinkedComponentName', ReadComponentName, WriteComponentName, InternalStreaming and (Component <> nil));
  Filer.DefineProperty('StyleManagerName', ReadStyleManagerName, WriteStyleManagerName, InternalStreaming and (StyleManager <> nil));
end;

procedure TBasedxReportLink.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = Component then
      Component := nil;
    if AComponent = StyleManager then
      StyleManager := nil;
  end;
end;

procedure TBasedxReportLink.ReadState(Reader: TReader);
begin
  inherited ReadState(Reader);
  if Reader.Parent is TCustomdxComponentPrinter then
    ComponentPrinter := Reader.Parent as TCustomdxComponentPrinter;
end;

procedure TBasedxReportLink.SetName(const NewName: TComponentName);
begin
  inherited SetName(NewName);
  if Caption = '' then Caption := NewName;
  DesignerUpdate(False);
end;

procedure TBasedxReportLink.SetParentComponent(AParent: TComponent);
begin
  inherited;
  if not IsLoading then
    ComponentPrinter := AParent as TCustomdxComponentPrinter;
end;

function TBasedxReportLink.CreatePrinterPage: TdxPrinterPage;
begin
  Result := TdxReportLinkPrinterPage.Create(Self);
end;

function TBasedxReportLink.GetPrinterPage: TdxPrinterPage;
begin
  Result := FPrinterPage;
end;

procedure TBasedxReportLink.SetPrinterPage(Value: TdxPrinterPage);
begin
  PrinterPage.Assign(Value);
end;

procedure TBasedxReportLink.FontChanged(Sender: TObject);
begin
  LinkModified(True);
end;

procedure TBasedxReportLink.LinkModified(Value: Boolean);
var
  ACompositions: TList;
  I: Integer;
begin
  FRebuildNeeded := Value;
  if IsComponentPrinterAvailable then
  begin
    ACompositions := TList.Create;
    try
      ComponentPrinter.GetCompositionsByLink(Self, ACompositions);
      for I := 0 to ACompositions.Count - 1 do
        TdxCompositionReportLink(ACompositions[I]).LinkModified(Value);
    finally
      ACompositions.Free;
    end;
  end;
end;

function TBasedxReportLink.IsFontStored: Boolean;
begin
  Result := not dxPSUtl.dxAreFontsEqual(Font, DefaultFont);
end;

procedure TBasedxReportLink.SetColor(Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    LinkModified(True);
  end;
end;

procedure TBasedxReportLink.SetContinuousPageIndexes(Value: Boolean);
begin
end;

procedure TBasedxReportLink.SetFont(Value: TFont);
begin
  Font.Assign(Value);
end;

procedure TBasedxReportLink.SetTransparent(Value: Boolean);
begin
  if FTransparent <> Value then
  begin
    FTransparent := Value;
    LinkModified(True);
  end;
end;

procedure TBasedxReportLink.AfterDesignReport(ADone: Boolean);
begin
  if IsComponentPrinterAvailable then
    ComponentPrinter.DoDesignReport(Self, ADone);
end;

procedure TBasedxReportLink.BeforeDesignReport;
begin
  if IsComponentPrinterAvailable then
    ComponentPrinter.DoBeforeDesignReport(Self);
end;

function TBasedxReportLink.PrintDialog(var APrintDlgData: TdxPrintDlgData): Boolean;
begin
  Result := dxPrnDlg.dxPrintDialog(APrintDlgData);
end;

procedure TBasedxReportLink.AfterPrinting;
begin
  if CurrentPrintStyle <> nil then CurrentPrintStyle.AfterPrinting;
end;

procedure TBasedxReportLink.BeforePrinting;
begin
  if CurrentPrintStyle <> nil then CurrentPrintStyle.BeforePrinting;
end;

function TBasedxReportLink.GetBreakPagesByHardDelimiters: Boolean;
begin
  Result := False;
end;

function TBasedxReportLink.GetCapabilities: TdxReportLinkCapabilities;
begin
  Result := [Low(TdxReportLinkCapability)..High(TdxReportLinkCapability)];
end;

function TBasedxReportLink.CalculateActualScaleFactor: Integer;
begin
  Result := RenderInfo.CalculateActualScaleFactor;
end;

function TBasedxReportLink.CannotActivateReportErrorString: string;
begin
  if DataSource = rldsComponent then
    Result := cxGetResourceString(@sdxMissingComponent)
  else
    Result := cxGetResourceString(@sdxInvalidExternalStorage);
end;

function TBasedxReportLink.CurrentComposition: TdxCompositionReportLink;
begin
  if IsComponentPrinterAvailable then
    Result := ComponentPrinter.CurrentCompositionByLink(Self)
  else
    Result := nil;
end;

procedure TBasedxReportLink.SetRealPrinterPage(Value: TdxPrinterPage);
begin
  RealPrinterPage.Assign(Value);
end;

function TBasedxReportLink.GetAllowContinuousPageIndexes: Boolean;
begin
  Result := False;
end;

function TBasedxReportLink.GetAlwaysBufferedGraphics: Boolean;
begin
  Result := True;
end;

function TBasedxReportLink.GetContinuousPageIndexes: Boolean;
begin
  Result := False;
end;

procedure TBasedxReportLink.SetFootersOnEveryPage(Value: Boolean);
begin
  if not IsAggregated then FFootersOnEveryPage := Value
end;

procedure TBasedxReportLink.SetHeadersOnEveryPage(Value: Boolean);
begin
  if not IsAggregated then FHeadersOnEveryPage := Value
end;

function TBasedxReportLink.GetRealScaleFactor: Integer;
begin
  if RealPrinterPage.ScaleMode = smAdjust then
    Result := RealPrinterPage.ScaleFactor
  else
    Result := CalculateActualScaleFactor;
end;

function TBasedxReportLink.GetReportHeight: Integer;
begin
  if ReportCells <> nil then
    Result := ReportCells.CalculateTotalHeight
  else
    Result := 0;
end;

function TBasedxReportLink.GetReportWidth: Integer;
begin
  if ReportCells <> nil then
    Result := ReportCells.CalculateTotalWidth
  else
    Result := 0;
end;

procedure TBasedxReportLink.DefineStylesClick(Sender: TObject);
begin
  DefinePrintStylesDlg;
end;

procedure TBasedxReportLink.StyleClick(Sender: TObject);
var
  APreviewBtnClicked, APrintBtnClicked: Boolean;
  APrevCurrentStyle, AStyle: TBasedxPrintStyle;
begin
  if StyleManager = nil then
    Exit;

  try
    AStyle := dxPSPageSetupMenuBuilderFactory.ActiveBuilder.ExtractPrintStyleFromObj(Sender);
  except
    AStyle := nil;
    Application.HandleException(Self);
  end;
  if AStyle = nil then
    Exit;

  APrevCurrentStyle := StyleManager.CurrentStyle;
  StyleManager.CurrentStyle := AStyle;

  if IsComponentPrinterAvailable and not ComponentPrinter.IsForegroundPreviewWindow then
  begin
    if not PageSetupEx(0, True, True, APreviewBtnClicked, APrintBtnClicked) then
      StyleManager.CurrentStyle := APrevCurrentStyle;

    if APrintBtnClicked then
      Print(True, nil)
    else
      if APreviewBtnClicked then
        Preview(True);
  end;
end;

procedure TBasedxReportLink.SetCurrentPage(Value: Integer);
begin
  Value := Max(Value, 1);
  Value := Min(Value, PageCount);
  FCurrentPage := Value;
end;

procedure TBasedxReportLink.DoCustomDrawEntirePage(ACanvas: TCanvas; R: TRect; ARealPageIndex: Integer);
const
  Denominator = 100;
begin
  ComponentPrinter.DoCustomDrawEntirePage(Self, ACanvas, ARealPageIndex, R,
    MulDiv(Denominator, cxRectWidth(R), MulDiv(RenderInfo.PageSize.X, PixelsDenominator, PixelsNumerator)), Denominator);
end;

procedure TBasedxReportLink.DoCustomDrawPageHeaderOrFooter(
  AHFObject: TCustomdxPageObject; ACanvas: TCanvas; APageIndex: Integer;
  R: TRect; var ADefaultDrawText, ADefaultDrawBackground: Boolean);
begin
  DoParentCustomDrawPageHeaderOrFooter(AHFObject, ACanvas, APageIndex, R,
    ADefaultDrawText, ADefaultDrawBackground, PixelsNumerator);
  if ADefaultDrawText or ADefaultDrawBackground then
  begin
    if AHFObject is TdxPageHeader then
    begin
      if Assigned(FOnCustomDrawPageHeader) then
        FOnCustomDrawPageHeader(Self, ACanvas, APageIndex, R,
          PixelsNumerator, PixelsDenominator, ADefaultDrawText, ADefaultDrawBackground)
    end
    else
      if Assigned(FOnCustomDrawPageFooter) then
        FOnCustomDrawPageFooter(Self, ACanvas, APageIndex, R,
          PixelsNumerator, PixelsDenominator, ADefaultDrawText, ADefaultDrawBackground);
  end;
end;

procedure TBasedxReportLink.DoCustomDrawPageFootnotes(ACanvas: TCanvas;
  R: TRect; var ATextAlignX: TcxTextAlignX; var ATextAlignY: TcxTextAlignY;
  var AColor: TColor; AFont: TFont; var ADone: Boolean);
begin
  DoParentCustomDrawReportFootnotes(ACanvas, R,
    ATextAlignX, ATextAlignY, AColor, AFont, ADone, PixelsNumerator);
  if not ADone and Assigned(OnCustomDrawReportLinkFootnotes) then
    OnCustomDrawReportLinkFootnotes(Self, ACanvas, R, PixelsNumerator,
      PixelsDenominator, ATextAlignX, ATextAlignY, AColor, AFont, ADone);
end;

procedure TBasedxReportLink.DoCustomDrawPageTitle(ACanvas: TCanvas; R: TRect;
  var ATextAlignX: TcxTextAlignX; var ATextAlignY: TcxTextAlignY;
  var AColor: TColor; AFont: TFont; var ADone: Boolean);
begin
  DoParentCustomDrawReportTitle(ACanvas, R, ATextAlignX, ATextAlignY,
    AColor, AFont, ADone, PixelsNumerator);
  if not ADone and Assigned(FOnCustomDrawReportLinkTitle) then
    FOnCustomDrawReportLinkTitle(Self, ACanvas, R, PixelsNumerator,
      PixelsDenominator, ATextAlignX, ATextAlignY, AColor, AFont, ADone);
end;

procedure TBasedxReportLink.DoParentCustomDrawPageHeaderOrFooter(
  AHFObject: TCustomdxPageObject; ACanvas: TCanvas; APageIndex: Integer;
  R: TRect; var ADefaultDrawText, ADefaultDrawBackground: Boolean;
  APixelsNumerator: Integer);
begin
  ComponentPrinter.DoCustomDrawPageHeaderOrFooter(Self, AHFObject, ACanvas,
    APageIndex, R, ADefaultDrawText, ADefaultDrawBackground, APixelsNumerator);
end;

procedure TBasedxReportLink.DoParentCustomDrawReportFootnotes(ACanvas: TCanvas;
  R: TRect; var ATextAlignX: TcxTextAlignX; var ATextAlignY: TcxTextAlignY;
  var AColor: TColor; AFont: TFont; var ADone: Boolean; APixelsNumerator: Integer);
begin
  ComponentPrinter.DoCustomDrawReportFootnotes(Self, ACanvas,
    R, ATextAlignX, ATextAlignY, AColor, AFont, ADone, APixelsNumerator);
end;

procedure TBasedxReportLink.DoParentCustomDrawReportTitle(ACanvas: TCanvas;
  R: TRect; var ATextAlignX: TcxTextAlignX; var ATextAlignY: TcxTextAlignY;
  var AColor: TColor; AFont: TFont; var ADone: Boolean; APixelsNumerator: Integer);
begin
  ComponentPrinter.DoCustomDrawReportTitle(Self, ACanvas,
    R, ATextAlignX, ATextAlignY, AColor, AFont, ADone, APixelsNumerator);
end;

procedure TBasedxReportLink.DoGetNewReportStorageName(var AName: string);
begin
  if Assigned(FOnGetNewReportStorageName) then
    FOnGetNewReportStorageName(Self, AName);
end;

procedure TBasedxReportLink.PaintPage(ACanvas: TCanvas;
  const APageBounds: TRect; APageIndex, AContinuousPageIndex, AZoomFactor: Integer);
var
  ARenderCanvas: TdxPSReportRenderCanvas;
begin
  ARenderCanvas := TdxPSReportRenderCanvas.Create(ACanvas);
  try
    PaintPageEx(ARenderCanvas, APageBounds, APageIndex, AContinuousPageIndex, AZoomFactor);
  finally
    ARenderCanvas.Free;
  end;
end;

procedure TBasedxReportLink.PaintPageEx(ACanvas: TdxPSReportRenderCustomCanvas;
  const APageBounds: TRect; APageIndex, AContinuousPageIndex, AZoomFactor: Integer);
begin
  FPainting := True;
  try
    Renderer.RenderPageEx(ACanvas, APageBounds, APageIndex, AContinuousPageIndex, AZoomFactor);
  finally
    FPainting := False;
  end;
end;

function TBasedxReportLink.IsEntirePageCustomDrawn: Boolean;
begin
  Result := Assigned(ComponentPrinter.FOnCustomDrawPage);
end;

function TBasedxReportLink.IsHeaderOrFooterCustomDrawn(AHFObject: TCustomdxPageObject): Boolean;
begin
  if AHFObject is TdxPageHeader then
    Result := Assigned(ComponentPrinter.FOnCustomDrawPageHeader) or Assigned(FOnCustomDrawPageHeader)
  else
    Result := Assigned(ComponentPrinter.FOnCustomDrawPageFooter) or Assigned(FOnCustomDrawPageFooter);
end;

function TBasedxReportLink.IsTitleCustomDrawn: Boolean;
begin
  Result := Assigned(ComponentPrinter.FOnCustomDrawReportTitle) or Assigned(FOnCustomDrawReportLinkTitle);
end;

function TBasedxReportLink.IsFootnotesCustomDrawn: Boolean;
begin
  Result := Assigned(ComponentPrinter.FOnCustomDrawReportFootnotes) or Assigned(FOnCustomDrawReportLinkFootnotes);
end;

function TBasedxReportLink.HasExternalListeners: Boolean;
begin
  Result := IsCurrentLink and (ComponentPrinter.Listeners.Count > 0);
end;

function TBasedxReportLink.IsComposable(AComposition: TdxCompositionReportLink): Boolean;
begin
  Result := True;
end;

function TBasedxReportLink.IsSupportedStyle(APrintStyle: TBasedxPrintStyle): Boolean;
begin
  Result := True;
  if Assigned(FOnFilterStyle) then
  begin
    FOnFilterStyle(Self, APrintStyle, Result);
    if not Result and (APrintStyle = CurrentPrintStyle) then
      Result := True;
  end;
end;

function TBasedxReportLink.NeedTwoPassRendering: Boolean;
begin
  Result := False;
end;

procedure TBasedxReportLink.DesignerModified;
begin
  if IsComponentPrinterAvailable then
    ComponentPrinter.DesignerModified;
end;

procedure TBasedxReportLink.DesignerUpdate(TheAll: Boolean);
begin
  if IsComponentPrinterAvailable then
    if TheAll then
      ComponentPrinter.DesignerUpdate(nil)
    else
      ComponentPrinter.DesignerUpdate(Self);
end;

function TBasedxReportLink.IsComponentPrinterAvailable: Boolean;
begin
  Result := ComponentPrinter <> nil;
end;

function TBasedxReportLink.IsDesigning: Boolean;
begin
  Result := csDesigning in ComponentState;
end;

function TBasedxReportLink.IsDestroying: Boolean;
begin
  Result := csDestroying in ComponentState;
end;

function TBasedxReportLink.IsLoading: Boolean;
begin
  Result := csLoading in ComponentState;
end;

function TBasedxReportLink.CanBuildReport(AComponent: TComponent): Boolean;
begin
  Result := IsComponentPrinterAvailable and (Component = AComponent);
end;

procedure TBasedxReportLink.Print(const APageIndexes: array of Integer);
begin
  PrintPages(TIntegers(@APageIndexes[0]));
end;

procedure TBasedxReportLink.DoMeasureReportLinkFootnotes(var AHeight: Integer);
begin
  if Assigned(ComponentPrinter) then
    ComponentPrinter.DoMeasureReportFootnotes(Self, AHeight);
  if Assigned(OnMeasureReportLinkFootnotes) then
    OnMeasureReportLinkFootnotes(Self, AHeight);
end;

procedure TBasedxReportLink.DoMeasureReportLinkTitle(var AHeight: Integer);
begin
  if Assigned(ComponentPrinter) then
    ComponentPrinter.DoMeasureReportTitle(Self, AHeight);
  if Assigned(OnMeasureReportLinkTitle) then
    OnMeasureReportLinkTitle(Self, AHeight);
end;

function TBasedxReportLink.GetCriticalSize(AReportCells: TdxReportCells): Integer;
begin
  with AReportCells.BoundsRect do
    Result := Right - Left;
end;

procedure TBasedxReportLink.GetImageLists(AProc: TdxPSGetImageListProc);
begin
end;

function TBasedxReportLink.PageReady(APageIndex: Integer): Boolean;
begin
  Result := True;
end;

procedure TBasedxReportLink.PrepareBuildReport;
begin
  // do nothing
end;

procedure TBasedxReportLink.PrepareFonts(UPI: Integer);
begin
  if FFontPool <> nil then
    FontPool.PrepareFonts(UPI);
end;

procedure TBasedxReportLink.PrepareLongOperation;
begin
  if IsComponentPrinterAvailable then
    ComponentPrinter.PrepareLongOperation;
end;

procedure TBasedxReportLink.UnprepareLongOperation;
begin
  if IsComponentPrinterAvailable then
    ComponentPrinter.UnprepareLongOperation;
end;

function TBasedxReportLink.CreateReportDocument: TdxPSReportDocument;
begin
  Result := GetReportDocumentClass.Create(Self);
end;

procedure TBasedxReportLink.DocumentChanged(Sender: TObject);
begin
  if IsCurrentLink then
    dxHFFormatObject.DateTime := DateTime;
end;

class function TBasedxReportLink.GetReportDocumentClass: TdxPSReportDocumentClass;
begin
  Result := TdxPSReportDocument;
end;

function TBasedxReportLink.CreateReportCells: TdxReportCells;
begin
  Result := GetReportCellsClass.Create(Self);
end;

function TBasedxReportLink.GetReportCellsClass: TdxReportCellsClass;
begin
  Result := TdxReportCells;
end;

function TBasedxReportLink.IsRealPrinterPage(APage: TdxPrinterPage): Boolean;
begin
  Result := RealPrinterPage = APage;
end;

function TBasedxReportLink.CreatePreviewWindowForm: TdxPSCustomPreviewWindow;
begin
  Result := dxPSPreviewDialogManager.CreatePreviewWindow;
end;

class function TBasedxReportLink.CreateDataReader(AStream: TStream): TdxPSDataReader;
begin
  Result := GetDataReaderClass.Create(AStream, 4096);
end;

class function TBasedxReportLink.CreateDataWriter(AStream: TStream): TdxPSDataWriter;
begin
  Result := GetDataWriterClass.Create(AStream, 4096);
  Result.FStorageVersion := dxPSGlbl.dxPSStorageVersion;
end;

class function TBasedxReportLink.GetDataReaderClass: TdxPSDataReaderClass;
begin
  Result := dxPSReaderFactory.ActualReaderClass;
end;

class function TBasedxReportLink.GetDataWriterClass: TdxPSDataWriterClass;
begin
  Result := dxPSWriterFactory.ActualWriterClass;
end;

procedure TBasedxReportLink.InternalLoadDataFromStream(AStream: TStream);
var
  Reader: TdxPSDataReader;
begin
  Include(FState, rlsDataLoading);
  try
    Reader := CreateDataReader(AStream);
    try
      try
        InternalReadData(Reader);
      except
        ClearGDIPools;
        AddFontToPool(Font);
        ReportTitle.Assign(FSavedReportTitle);
        ReportFootnotes.Assign(FSavedReportTitle);
        ReportDocument.Assign(FSavedReportDocument);
        raise;
      end;
    finally
      Reader.Free;
    end;
  finally
    Exclude(FState, rlsDataLoading);
  end;
end;

procedure TBasedxReportLink.InternalReadData(AReader: TdxPSDataReader);
var
  AOffsetTable: TdxPSDataStorageOffsetTable;
  AStorageInfo: TdxPSDataStorageInfo;
begin
  AStorageInfo := TdxPSDataStorageInfo.Create(Self);
  AOffsetTable := ReadOffsetTable(AReader);
  try
    AReader.Position := AOffsetTable.Information;
    AStorageInfo.ReadData(AReader);

    AReader.Position := AOffsetTable.Document;
    ReadReportDocument(AReader, ReportDocument);

    AReader.Position := AOffsetTable.Title;
    ReadTitle(AReader);

    AReader.Position := AOffsetTable.Data;
    ReadData(AReader);

    if (AOffsetTable.Footnotes <> 0) and (AStorageInfo.PrintingSystemVersion.Major > 3) then
    begin
      AReader.Position := AOffsetTable.Footnotes;
      ReadFootnotes(AReader);
    end;
  finally
    AStorageInfo.Free;
    AOffsetTable.Free;
  end;
end;

procedure TBasedxReportLink.InternalWriteData(AWriter: TdxPSDataWriter);
var
  AOffsetTable: TdxPSDataStorageOffsetTable;
  ASavedPosition: Longint;
  AStorageInfo: TdxPSDataStorageInfo;
begin
  WriteOffsetTable(AWriter, nil);

  AStorageInfo := RetrieveStorageInfo;
  AOffsetTable := TdxPSDataStorageOffsetTable.Create(nil);
  try
    AOffsetTable.Information := AWriter.Position;
    WriteStorageInfo(AWriter, AStorageInfo);

    AOffsetTable.Document := AWriter.Position;
    WriteReportDocument(AWriter, ReportDocument);

    AOffsetTable.Title := AWriter.Position;
    WriteTitle(AWriter);

    AOffsetTable.Footnotes := AWriter.Position;
    WriteFootnotes(AWriter);

    AOffsetTable.Data := AWriter.Position;
    WriteData(AWriter);

    ASavedPosition := AWriter.Position;
    try
      AWriter.Position := 0;
      WriteOffsetTable(AWriter, AOffsetTable);
    finally
      AWriter.Position := ASavedPosition;
    end;
  finally
    AOffsetTable.Free;
    AStorageInfo.Free;
  end;
end;

function TBasedxReportLink.IsRebuildNeededAndAllowed: Boolean;
begin
  Result := IsCurrentLink and DataProviderPresent and not IsAggregated and HasExternalListeners;
end;

function TBasedxReportLink.RetrieveStorageInfo: TdxPSDataStorageInfo;
begin
  Result := TdxPSDataStorageInfo.Create(Self);
end;

procedure TBasedxReportLink.ReadData(AReader: TdxPSDataReader);
begin
  ReadReportData(AReader);
  ReadRenderInfo(AReader);
  ReadBackgroundBitmapPool(AReader);
  ReadFontPool(AReader);
end;

class function TBasedxReportLink.ReadOffsetTable(AReader: TdxPSDataReader): TdxPSDataStorageOffsetTable;
begin
  Result := TdxPSDataStorageOffsetTable.Create(nil);
  Result.ReadData(AReader);
end;

class procedure TBasedxReportLink.ReadReportDocument(AReader: TdxPSDataReader; AReportDocument: TdxPSReportDocument);
begin
  AReportDocument.ReadData(AReader);
end;

class function TBasedxReportLink.ReadStorageInfo(AReader: TdxPSDataReader): TdxPSDataStorageInfo;
begin
  Result := TdxPSDataStorageInfo.Create(nil);
  Result.ReadData(AReader);
end;

class procedure TBasedxReportLink.SkipStorageInfo(AReader: TdxPSDataReader);
var
  StorageInfo: TdxPSDataStorageInfo;
begin
  StorageInfo := nil;
  try
    StorageInfo := ReadStorageInfo(AReader);
  except
    FreeAndNil(StorageInfo);
    raise;
  end;
end;

procedure TBasedxReportLink.ReadFootnotes(AReader: TdxPSDataReader);
begin
  ReportFootnotes.ReadData(AReader);
end;

procedure TBasedxReportLink.ReadTitle(AReader: TdxPSDataReader);
begin
  ReportTitle.ReadData(AReader);
end;

procedure TBasedxReportLink.WriteData(AWriter: TdxPSDataWriter);
begin
  WriteReportData(AWriter);
  WriteRenderInfo(AWriter);
  WriteBackgroundBitmapPool(AWriter);
  WriteFontPool(AWriter);
end;

class procedure TBasedxReportLink.WriteOffsetTable(AWriter: TdxPSDataWriter;
  AnOffsetTable: TdxPSDataStorageOffsetTable);
var
  OffsetTable: TdxPSDataStorageOffsetTable;
begin
  OffsetTable := TdxPSDataStorageOffsetTable.Create(AnOffsetTable);
  try
    OffsetTable.WriteData(AWriter);
  finally
    OffsetTable.Free;
  end;
end;

class procedure TBasedxReportLink.WriteStorageInfo(AWriter: TdxPSDataWriter;
  AStorageInfo: TdxPSDataStorageInfo);
begin
  AStorageInfo.WriteData(AWriter);
end;

class procedure TBasedxReportLink.WriteReportDocument(AWriter: TdxPSDataWriter;
  AReportDocument: TdxPSReportDocument);
begin
  AReportDocument.WriteData(AWriter);
end;

procedure TBasedxReportLink.WriteTitle(AWriter: TdxPSDataWriter);
begin
  ReportTitle.WriteData(AWriter);
end;

procedure TBasedxReportLink.WriteFootnotes(AWriter: TdxPSDataWriter);
begin
  ReportFootnotes.WriteData(AWriter);
end;

procedure TBasedxReportLink.ReadBackgroundBitmapPool(AReader: TdxPSDataReader);
begin
  BackgroundBitmapPool.ReadData(AReader);
end;

procedure TBasedxReportLink.ReadReportData(AReader: TdxPSDataReader);
begin
  if FReportCells = nil then
    FReportCells := TdxReportCells.Create(Self);
  ReportCells.ReadData(AReader);
end;

procedure TBasedxReportLink.ReadFontPool(AReader: TdxPSDataReader);
begin
  FontPool.ReadData(AReader);
end;

procedure TBasedxReportLink.ReadRenderInfo(AReader: TdxPSDataReader);
begin
  RenderInfo.ReadData(AReader);
end;

procedure TBasedxReportLink.WriteBackgroundBitmapPool(AWriter: TdxPSDataWriter);
begin
  BackgroundBitmapPool.WriteData(AWriter);
end;

procedure TBasedxReportLink.WriteFontPool(AWriter: TdxPSDataWriter);
begin
  FontPool.WriteData(AWriter);
end;

procedure TBasedxReportLink.WriteReportData(AWriter: TdxPSDataWriter);
begin
  ReportCells.WriteData(AWriter);
end;

procedure TBasedxReportLink.WriteRenderInfo(AWriter: TdxPSDataWriter);
begin
  RenderInfo.WriteData(AWriter);
end;

procedure TBasedxReportLink.SetActive(Value: Boolean);
begin
  if IsReading and Value then
    FStreamedActive := Value
  else
    if (FActive <> Value) and Assigned(ComponentPrinter) then
    begin
      if Value then
        ComponentPrinter.ActivateLink(Self)
      else
        ComponentPrinter.DeactivateLink(Self);
    end;
end;

procedure TBasedxReportLink.SetComponent(Value: TComponent);
begin
  if IsReading then
    FComponent := Value
  else
    if FComponent <> Value then
    begin
      if PreviewExists then
        ComponentPrinter.DestroyPreviewWindowForm;

      if Value = nil then
      begin
        Active := False;
        FComponent := nil;
      end
      else
        if IsSupportedCompClass(TComponentClass(Value.ClassType)) then
          SetComponentUnconditionally(Value)
        else
          ComponentUnsupportedError(Value);

      if not IsDestroying and not IsLoading then
      begin
        DoChangeComponent;
        if IsComponentPrinterAvailable then
          ComponentPrinter.DoChangeComponent(Self);
        DesignerUpdate(False);
      end;
    end;
end;

procedure TBasedxReportLink.SetShowPageFooter(Value: Boolean);
begin
  if FShowPageFooter <> Value then
  begin
    FShowPageFooter := Value;
    ShowPageFooterChanged;
  end;
end;

procedure TBasedxReportLink.SetShowPageHeader(Value: Boolean);
begin
  if FShowPageHeader <> Value then
  begin
    FShowPageHeader := Value;
    ShowPageHeaderChanged;
  end;
end;

procedure TBasedxReportLink.SetShowPageRowHeader(Value: Boolean);
begin
  if FShowPageRowHeader <> Value then
  begin
    FShowPageRowHeader := Value;
    ShowPageHeaderChanged;
  end;
end;

procedure TBasedxReportLink.SetShrinkToPageWidth(Value: Boolean);
var
  APage: TdxPrinterPage;
begin
  if ShrinkToPageWidth <> Value then
  begin
    PrepareLongOperation;
    try
      APage := RealPrinterPage;
      APage.BeginUpdate;
      try
        if Value then
        begin
          APage.FitToPagesHorizontally := 1;
          APage.FitToPagesVertically := 0;
          APage.ScaleMode := smFit;
        end
        else
          APage.ScaleMode := smAdjust;
      finally
        APage.EndUpdate;
      end;
    finally
      UnprepareLongOperation;
    end;
  end;
end;

procedure TBasedxReportLink.SetDateTime(const Value: TDateTime);
begin
  ReportDocument.CreationDate := Value;
end;

procedure TBasedxReportLink.SetDesignerCaption(const Value: string);
begin
  if DesignerCaption <> Value then
  begin
    FDesignerCaption := Value;
    FIsDesignerCaptionAssigned := True;
  end;
end;

procedure TBasedxReportLink.SetDescription(const Value: string);
begin
  ReportDocument.Description := Value;
end;

function TBasedxReportLink.getDateFormat: Integer;
begin
  if not (fvDate in AssignedFormatValues) and IsComponentPrinterAvailable then
    Result := ComponentPrinter.DateFormat
  else
    Result := FDateFormat;
end;

function TBasedxReportLink.GetDateTime: TDateTime;
begin
  Result := ReportDocument.CreationDate;
end;

function TBasedxReportLink.GetDescription: string;
begin
  Result := ReportDocument.Description;
end;

function TBasedxReportLink.GetDesignerCaption: string;
begin
  if FIsDesignerCaptionAssigned then
    Result := FDesignerCaption
  else
    Result := DefaultDesignerCaption;
end;

function TBasedxReportLink.GetIndex: Integer;
begin
  if IsComponentPrinterAvailable then
    Result := ComponentPrinter.IndexOfLink(Self)
  else
    Result := -1;
end;

function TBasedxReportLink.GetIsAggregated: Boolean;
begin
  Result := Controller <> nil;
end;

function TBasedxReportLink.GetIsBuilding: Boolean;
begin
  Result := IsComponentPrinterAvailable and (cpsBuilding in ComponentPrinter.State);
end;

function TBasedxReportLink.GetIsCurrentLink: Boolean;
begin
  Result := IsComponentPrinterAvailable and (ComponentPrinter.CurrentLink = Self);
end;

function TBasedxReportLink.GetIsReading: Boolean;
begin
  Result := csReading in ComponentState;
end;

function TBasedxReportLink.GetPageHeight: Integer;
begin
  Result := cxRectHeight(RealPrinterPage.PaintRectPixels) - 1;
end;

function TBasedxReportLink.GetPageNumberFormat: TdxPageNumberFormat;
begin
  if not (fvPageNumber in AssignedFormatValues) and IsComponentPrinterAvailable then
    Result := ComponentPrinter.PageNumberFormat
  else
    Result := FPageNumberFormat;
end;

function TBasedxReportLink.GetPageWidth: Integer;
begin
  Result := cxRectWidth(RealPrinterPage.PaintRectPixels) - 1;
end;

function TBasedxReportLink.GetPreviewWindow: TdxPSCustomPreviewWindow;
begin
  if IsComponentPrinterAvailable then
    Result := ComponentPrinter.PreviewWindow
  else
    Result := nil;
end;

function TBasedxReportLink.GetRealPrinterPage: TdxPrinterPage;
var
  AComposition: TdxCompositionReportLink;
  AStyle: TBasedxPrintStyle;
begin
  AComposition := CurrentComposition;
  if AComposition <> nil then
    Result := AComposition.RealPrinterPage
  else
    Result := nil;

  if Result = nil then
  begin
    AStyle := CurrentPrintStyle;
    if AStyle <> nil then
      Result := AStyle.PrinterPage
    else
      Result := FPrinterPage;
  end;
end;

function TBasedxReportLink.GetReportTitleMode: TdxReportTitleMode;
begin
  Result := ReportTitle.Mode;
end;

function TBasedxReportLink.GetReportTitleText: string;
begin
  Result := ReportTitle.Text;
end;

function TBasedxReportLink.GetRenderer: TdxPSReportRenderer;
begin
  if FRenderer = nil then
    FRenderer := CreateRenderer;
  Result := FRenderer;
end;

function TBasedxReportLink.GetRenderInfo: TdxPSReportRenderInfo;
begin
  if FRenderInfo = nil then
    FRenderInfo := CreateRenderInfo;
  Result := FRenderInfo;
end;

function TBasedxReportLink.GetRenderStage: TdxPSRenderStages;
begin
  if FPainting and (FRenderer <> nil) then
    Result := Renderer.RenderStage
  else
    Result := [];
end;

function TBasedxReportLink.GetShowPageFooter: Boolean;
var
  AComposition: TdxCompositionReportLink;
begin
  Result := FShowPageFooter;
  AComposition := CurrentComposition;
  if AComposition <> nil then
    Result := AComposition.ShowPageFooter;
end;

function TBasedxReportLink.GetShowPageHeader: Boolean;
var
  AComposition: TdxCompositionReportLink;
begin
  Result := FShowPageHeader;
  AComposition := CurrentComposition;
  if AComposition <> nil then
    Result := AComposition.ShowPageHeader;
end;

function TBasedxReportLink.GetShowPageRowHeader: Boolean;
var
  AComposition: TdxCompositionReportLink;
begin
  Result := FShowPageRowHeader;
  AComposition := CurrentComposition;
  if AComposition <> nil then
    Result := AComposition.ShowPageRowHeader;
end;

function TBasedxReportLink.GetShrinkToPageWidth: Boolean;
var
  APage: TdxPrinterPage;
begin
  APage := RealPrinterPage;
  Result := (APage.ScaleMode = smFit) and (APage.FitToPagesHorizontally = 1) and (APage.FitToPagesVertically = 0);
end;

function TBasedxReportLink.GetStartPageIndex: Integer;
begin
  Result := FStartPageIndex;
end;

function TBasedxReportLink.getTimeFormat: Integer;
begin
  if not (fvTime in AssignedFormatValues) and Assigned(ComponentPrinter) then
    Result := ComponentPrinter.TimeFormat
  else
    Result := FTimeFormat;
end;

function TBasedxReportLink.GetVirtualPageCount: Integer;
begin
  Result := RenderInfo.VirtualPageCount;
end;

function TBasedxReportLink.IsDateFormatStored: Boolean;
begin
  Result := fvDate in AssignedFormatValues;
end;

function TBasedxReportLink.IsDesignerCaptionStored: Boolean;
begin
  Result := FIsDesignerCaptionAssigned and (DesignerCaption <> DefaultDesignerCaption);
end;

function TBasedxReportLink.IsTimeFormatStored: Boolean;
begin
  Result := fvTime in AssignedFormatValues;
end;

function TBasedxReportLink.IsPageNumberFormatStored: Boolean;
begin
  Result := fvPageNumber in AssignedFormatValues;
end;

procedure TBasedxReportLink.SetDateFormat(Value: Integer);
begin
  Value := Max(Value, 0);
  Value := Min(Value, dxPgsDlg.DateFormats.Count - 1);

  FDateFormat := Value;
  Include(FAssignedFormatValues, fvDate);
  if IsCurrentLink then
    dxHFFormatObject.DateFormat := dxPgsDlg.DateFormats[FDateFormat];
end;

procedure TBasedxReportLink.SetTimeFormat(Value: Integer);
begin
  Value := Max(Value, 0);
  Value := Min(Value, dxPgsDlg.TimeFormats.Count - 1);

  FTimeFormat := Value;
  Include(FAssignedFormatValues, fvTime);
  if IsCurrentLink then
    dxHFFormatObject.TimeFormat := dxPgsDlg.TimeFormats[FTimeFormat];
end;

procedure TBasedxReportLink.SetPageNumberFormat(Value: TdxPageNumberFormat);
begin
  FPageNumberFormat := Value;
  Include(FAssignedFormatValues, fvPageNumber);
  if IsCurrentLink then
    dxHFFormatObject.PageNumberFormat := FPageNumberFormat;
end;

procedure TBasedxReportLink.SetAbortBuilding(Value: Boolean);
begin
  if IsComponentPrinterAvailable then
    ComponentPrinter.AbortBuilding := Value;
end;

procedure TBasedxReportLink.SetAssignedFormatValues(Value: TdxAssignedFormatValues);
begin
  if FAssignedFormatValues <> Value then
  begin
    FAssignedFormatValues := Value;
    DesignerModified;
  end;
end;

procedure TBasedxReportLink.SetCaption(const Value: string);
begin
  ReportDocument.Caption := Value;
end;

function TBasedxReportLink.GetAbortBuilding: Boolean;
begin
  Result := IsComponentPrinterAvailable and ComponentPrinter.AbortBuilding;
end;

function TBasedxReportLink.GetCaption: string;
begin
  Result := ReportDocument.Caption;
end;

function TBasedxReportLink.GetCurrentPrintStyle: TBasedxPrintStyle;
begin
  if StyleManager <> nil then
    Result := StyleManager.CurrentStyle
  else
    Result := nil;
end;

function TBasedxReportLink.GetFontPool: TdxPSReportFontPool;
begin
  if IsAggregated then
    Result := Controller.FontPool
  else
  begin
    if FFontPool = nil then
      FFontPool := TdxPSReportFontPool.Create(Self);
    Result := FFontPool;
  end;
end;

function TBasedxReportLink.GetHasDesignWindow: Boolean;
begin
  Result := DesignWindow <> nil;
end;

function TBasedxReportLink.GetHasPreviewWindow: Boolean;
begin
  Result := IsComponentPrinterAvailable and (ComponentPrinter.PreviewWindow <> nil);
end;

procedure TBasedxReportLink.RetrievePageAsImage(APageIndex: Integer; AGraphicClass: TGraphicClass;
  AGraphic: TGraphic);
begin
  if IsComponentPrinterAvailable then
    TdxComponentPrinter(ComponentPrinter).EnumPagesAsImages([APageIndex],
      AGraphicClass, False, RetrievePageAsImageCallBack, AGraphic, nil, nil, Self);
end;

procedure TBasedxReportLink.RetrievePageAsImageCallBack(AComponentPrinter: TCustomdxComponentPrinter;
  AReportLink: TBasedxReportLink; AIndex, APageIndex: Integer; const AGraphic: TGraphic;
  AData: Pointer; var AContinue: Boolean);
begin
  TGraphic(AData).Assign(AGraphic);
end;

procedure TBasedxReportLink.ShowPageFooterChanged;
begin
  if CurrentComposition = nil then
    CalculateRenderInfos;
end;

procedure TBasedxReportLink.ShowPageHeaderChanged;
begin
  if CurrentComposition = nil then
    CalculateRenderInfos;
end;

procedure TBasedxReportLink.StdProcessDataSourceDontPresent;
begin
  raise EdxReportLink.Create(CannotActivateReportErrorString);
end;

procedure TBasedxReportLink.TunePixelsNumerator(AReportCells: TdxReportCells);
const
  MaxReportWidth = $7FFF;
  MinFactor = 2;
var
  Wo, W: Integer;
begin
  Wo := GetCriticalSize(AReportCells);
  W := MulDiv(Wo, RenderInfo.UnitsPerInch, PixelsDenominator);
  if W > MaxReportWidth then
  begin
    RenderInfo.UnitsPerInch := MulDiv(PixelsDenominator, MaxReportWidth, Wo);
    RenderInfo.UnitsPerInch := RenderInfo.UnitsPerInch - RenderInfo.UnitsPerInch mod PixelsPerInch;
    RenderInfo.UnitsPerInch := Max(RenderInfo.UnitsPerInch, MinFactor * PixelsPerInch);
  end;
end;

procedure TBasedxReportLink.CalculateRenderInfos;
begin
  if DataProviderPresent then
  begin
    PrepareLongOperation;
    try
      Include(FState, rlsPagination);
      try
        RenderInfo.Calculate;
        if PreviewExists then
          PreviewWindow.FullRefresh;
      finally
        Exclude(FState, rlsPagination);
      end;
    finally
      UnprepareLongOperation;
    end;
  end;
end;

procedure TBasedxReportLink.ClearGDIPools;
begin
  BackgroundBitmapPool.Clear;
  Renderer.BrushPool.Clear;
  if (FFontPool <> nil) and not IsAggregated then
    FontPool.Clear;
end;

function TBasedxReportLink.CreateRenderInfo: TdxPSReportRenderInfo;
begin
  Result := GetRenderInfoClass.Create(Self);
end;

procedure TBasedxReportLink.FreeRenderInfos;
begin
  FreeAndNil(FRenderInfo);
end;

function TBasedxReportLink.GetRenderInfoClass: TdxPSReportRenderInfoClass;
begin
  Result := TdxPSReportRenderInfo;
end;

function TBasedxReportLink.CreateRenderer: TdxPSReportRenderer;
begin
  Result := GetRendererClass.Create(Self);
end;

procedure TBasedxReportLink.FreeRenderer;
begin
  FreeAndNil(FRenderer);
end;

function TBasedxReportLink.GetRendererClass: TdxPSReportRendererClass;
begin
  Result := TdxPSReportRenderer;
end;

procedure TBasedxReportLink.InitializeDefaultFont(AFont: TFont);
begin
  AFont.CharSet := dxPSDefaultFontCharSet;
  AFont.Color := dxPSDefaultFontColor;
  AFont.Name := dxPSDefaultFontName;
  AFont.Size := dxPSDefaultFontSize;
  AFont.Style := dxPSDefaultFontStyle;
end;

procedure TBasedxReportLink.InternalGetDelimiters(ADelimitersHorz, ADelimitersVert: TList);
begin
  dxAppendList(RenderInfo.DelimiterXList, ADelimitersHorz);
  dxAppendList(RenderInfo.DelimiterYList, ADelimitersVert);
end;

function TBasedxReportLink.GetPageCount: Integer;
begin
  Result := RenderInfo.NonEmptyPageCount;
  if IsCurrentLink then
    dxHFFormatObject.TotalPages := Result;
end;

function TBasedxReportLink.GetUseHardHorzDelimiters: Boolean;
begin
  Result := False;
end;

function TBasedxReportLink.GetUseHardVertDelimiters: Boolean;
begin
  Result := False;
end;

function TBasedxReportLink.GetRebuildOnPageParamsChange(AUpdateCodes: TdxPrinterPageUpdateCodes): Boolean;
begin
  Result := False;
end;

procedure TBasedxReportLink.SetStorageName(const Value: string);
begin
  if FStorageName <> Value then
  begin
    FStorageName := Value;
    if FileExists(FStorageName) and (DataSource = rldsExternalStorage) then
      LoadDataFromFile(FStorageName);
  end;
end;

procedure TBasedxReportLink.SetStyleManager(Value: TdxPrintStyleManager);
begin
  if FStyleManager <> Value then
  begin
    FStyleManager := Value;
    if Value <> nil then
      Value.FreeNotification(Self);
    if not IsDestroying then
      PageParamsChanged(RealPrinterPage, CurrentPrintStyle, ucAll);
  end;
end;

procedure TBasedxReportLink.SetPDFExportOptions(AValue: TdxPSPDFReportExportOptions);
begin
  FPDFExportOptions.Assign(AValue);
end;

procedure TBasedxReportLink.SetReportDocument(Value: TdxPSReportDocument);
begin
  ReportDocument.Assign(Value);
end;

procedure TBasedxReportLink.SetReportTitleMode(Value: TdxReportTitleMode);
begin
  ReportTitle.Mode := Value;
end;

procedure TBasedxReportLink.SetReportTitleText(const Value: string);
begin
  ReportTitle.Text := Value;
end;

procedure TBasedxReportLink.SetReportFootnotes(Value: TdxReportFootnotes);
begin
  ReportFootnotes.Assign(Value);
end;

procedure TBasedxReportLink.SetReportTitle(Value: TdxReportTitle);
begin
  ReportTitle.Assign(Value);
end;

procedure TBasedxReportLink.SetComponentPrinter(Value: TCustomdxComponentPrinter);
begin
  if FComponentPrinter <> Value then
  begin
    if IsComponentPrinterAvailable then
      ComponentPrinter.RemoveLink(Self);
    if Value <> nil then
      Value.InsertLink(Self);
  end;
end;

procedure TBasedxReportLink.SetIsCurrentLink(Value: Boolean);
begin
  if Value and not IsReading and Assigned(ComponentPrinter) then
    ComponentPrinter.CurrentLink := Self;
end;

procedure TBasedxReportLink.SetDataSource(Value: TdxReportLinkDataSource);
begin
  if FDataSource <> Value then
  begin
    FDataSource := Value;
    DoDataSourceChanged;
  end;
end;

procedure TBasedxReportLink.SetIndex(Value: Integer);
var
  CurIndex: Integer;
begin
  if ComponentPrinter = nil then Exit;
  Value := Min(Max(Value, 0), ComponentPrinter.LinkCount - 1);
  CurIndex := GetIndex;
  if CurIndex <> Value then
    ComponentPrinter.MoveLink(CurIndex, Value);
end;

procedure TBasedxReportLink.ReadBuiltIn(Reader: TReader);
begin
  FBuiltIn := Reader.ReadBoolean;
end;

function GetComponentByName(const AName: string): TComponent;

   procedure CheckOwner(AOwner: TComponent);
   var
     I: Integer;
     AComponent: TComponent;
   begin
     if Result <> nil then Exit;
     for I := 0 to AOwner.ComponentCount - 1 do
     begin
       AComponent := AOwner.Components[I];
       if SameText(cxGetFullComponentName(AComponent), AName) then
       begin
         Result := AComponent;
         break;
       end
       else
         CheckOwner(AComponent);
     end;
   end;

var
  AOwner: TComponent;
begin
  Result := nil;
  AOwner := Application;
  while AOwner.Owner <> nil do
    AOwner := AOwner.Owner;
  CheckOwner(AOwner);
end;

procedure TBasedxReportLink.ReadComponentName(AReader: TReader);
begin
  Component := GetComponentByName(AReader.ReadString);
end;

procedure TBasedxReportLink.ReadIsDesignerCaptionAssigned(Reader: TReader);
begin
  FIsDesignerCaptionAssigned := Reader.ReadBoolean;
end;

procedure TBasedxReportLink.ReadStyleManagerName(AReader: TReader);
begin
  StyleManager := GetComponentByName(AReader.ReadString) as TdxPrintStyleManager;
end;

procedure TBasedxReportLink.WriteBuiltIn(Writer: TWriter);
begin
  Writer.WriteBoolean(FBuiltIn);
end;

procedure TBasedxReportLink.WriteComponentName(AWriter: TWriter);
begin
  AWriter.WriteString(cxGetFullComponentName(Component));
end;

procedure TBasedxReportLink.WriteIsDesignerCaptionAssigned(Writer: TWriter);
begin
  Writer.WriteBoolean(FIsDesignerCaptionAssigned);
end;

procedure TBasedxReportLink.WriteStyleManagerName(AWriter: TWriter);
begin
  AWriter.WriteString(cxGetFullComponentName(StyleManager));
end;

procedure TBasedxReportLink.InternalActivate;
begin
  if DataProviderPresent then
  try
    AbortBuilding := False;
    try
      DoCreateReport;
    finally
      FRebuildNeeded := False;
      FActive := True;
      if AbortBuilding then
      begin
        FRebuildNeeded := True;
        DoDestroyReport;
      end;
    end;
  except
    on E: Exception do
    begin
      DoDestroyReport;
      if IsDesigning then
        ShowException(E, ExceptAddr)
      else
        raise;
    end;
  end
  else
    DoDataProviderDontPresent;
end;

procedure TBasedxReportLink.InternalRestoreDefaults;
begin
  Transparent := True;
  Color := dxDefaultContentColor; {clWhite}
  Font := DefaultFont;
  FootersOnEveryPage := False;
  HeadersOnEveryPage := False;

  PageNumberFormat := DefaultPageNumberFormat;
  DateFormat := DefaultDateFormat;
  TimeFormat := DefaultTimeFormat;
  AssignedFormatValues := [];

  FIsDesignerCaptionAssigned := False;
end;

procedure TBasedxReportLink.InternalRestoreFromOriginal;
begin
  if Component is TControl then
  begin
    Color := dxPSUtl.Control_GetColor(TControl(Component));
    Font := dxPSUtl.Control_GetFont(TControl(Component));
  end;
end;

procedure TBasedxReportLink.SetUseHorzDelimiters(Value: Boolean);
begin
  if FUseHorzDelimiters <> Value then
  begin
    FUseHorzDelimiters := Value;
    LinkModified(True);
  end;
end;

procedure TBasedxReportLink.SetUseVertDelimiters(Value: Boolean);
begin
  if FUseVertDelimiters <> Value then
  begin
    FUseVertDelimiters := Value;
    LinkModified(True);
  end;
end;

function TBasedxReportLink.ValidateMargins: Boolean;
begin
  Result := RealPrinterPage.ValidateMargins;
  if not Result then
  begin
    Result := MessageQuestion(cxGetResourceString(@sdxOutsideMarginsMessage));
    if Result then
    begin
      Include(FState, rlsPagination);
      try
        RealPrinterPage.FixMarginsOutSide;
      finally
        Exclude(FState, rlsPagination);
      end;
    end;
  end;
end;

procedure TBasedxReportLink.SetStartPageIndex(Value: Integer);
begin
  Value := Max(Value, 1);
  if FStartPageIndex <> Value then
  begin
    FStartPageIndex := Value;
    dxHFFormatObject.StartPageIndex := Value;
  end;
end;

procedure TBasedxReportLink.DoDestroy;
begin
  dxCallNotify(OnDestroy, Self);
end;

procedure TBasedxReportLink.ComponentUnsupportedError(AComponent: TComponent);
begin
  if not IsDesigning then
    raise EdxReportLink.CreateFmt(cxGetResourceString(@sdxComponentNotSupportedByLink), [AComponent.ClassName]);
end;

procedure TBasedxReportLink.DoChangeComponent;
begin
  dxCallNotify(OnChangeComponent, Self);
end;

procedure TBasedxReportLink.DoProgress(const APercentDone: Double);
begin
  if IsComponentPrinterAvailable and not IsAggregated then
    ComponentPrinter.DoProgress(Self, APercentDone);
end;

function TBasedxReportLink.GetDesignerClass: TdxReportLinkDesignWindowClass;
begin
  Result := dxPSDesignerClassByLinkClass(LinkClass);
end;

procedure TBasedxReportLink.DoApplyInDesigner;
begin
  RebuildReport;
end;

function TBasedxReportLink.DoBeforeExportToPDF(
  const AFileName: string; AOptions: TdxPSPDFReportExportOptions): Boolean;
begin
  if IsComponentPrinterAvailable then
    Result := ComponentPrinter.DoBeforeExportToPDF(Self, AFileName, AOptions)
  else
    Result := True;
end;

procedure TBasedxReportLink.DoCreateReport;
begin
  if FReportCells = nil then
    FReportCells := CreateReportCells;
  DoCreateReportData;

  if not (AbortBuilding or IsInvalidReport) then
  begin
    if IsAggregated then
    begin
      if DataSource = rldsComponent then
        RenderInfo.GetDelimiters;
    end
    else
    begin
      if IsWin9X then
        TunePixelsNumerator(FReportCells);

      RenderInfo.PreparePixelsNumeratorAndDenominator;
      if DataSource = rldsComponent then
      begin
        PrepareReportGroupsLookAndFeels;
        ConvertCoords;
        RenderInfo.GetDelimiters;
      end;
      CalculateRenderInfos;
    end;
  end;
end;

procedure TBasedxReportLink.DoCreateReportData;
var
  ReraiseException: Boolean;
begin
  case DataSource of
    rldsComponent:
      ConstructReport(FReportCells);
    rldsExternalStorage:
      try
        InternalLoadDataFromStream(DataStream);
        DataStream.Position := 0;
      except
        ReraiseException := IsComponentPrinterAvailable and (ComponentPrinter.Explorer <> nil);
        if not ReraiseException then
          MessageError(Format(cxGetResourceString(@sdxReportFileLoadError), [StorageName]));

        FIsInvalidReport := True;
        try
          DataSource := rldsComponent;
        finally
          FIsInvalidReport := False;
        end;
        if ReraiseException then raise;
      end;
  end;
end;

procedure TBasedxReportLink.DoDataProviderDontPresent;
begin
  if (ComponentPrinter = nil) or (ComponentPrinter.CurrentCompositionByLink(Self) = nil) then
    StdProcessDataSourceDontPresent;
end;

procedure TBasedxReportLink.DoDestroyReport;
begin
  FActive := False;
  PrepareLongOperation;
  try
    FreeAndNil(FReportCells);
    if Assigned(RenderInfo) then
      RenderInfo.Refresh;
    if Assigned(ComponentPrinter) then
      ComponentPrinter.DoBeforeDestroyReport(Self);
  finally
    UnprepareLongOperation;
  end;
end;

procedure TBasedxReportLink.DoPageParamsChanged;
begin
  if IsComponentPrinterAvailable then
    ComponentPrinter.DoPageParamsChanged(Self);
end;

procedure TBasedxReportLink.CopyDataStreamFrom(AStream: TStream);
begin
  AStream.Position := 0;
  FinalizeDataStream;
  FDataStream := TMemoryStream.Create;
  FDataStream.CopyFrom(AStream, AStream.Size);
  FDataStream.Position := 0;
end;

procedure TBasedxReportLink.FinalizeDataStream;
begin
  FreeAndNil(FDataStream);
end;

function TBasedxReportLink.IsApplyBackgroundToEntirePage: Boolean;
begin
  Result := False;
end;

function TBasedxReportLink.IsDrawFootersOnEveryPage: Boolean;
begin
  Result := FootersOnEveryPage;
end;

function TBasedxReportLink.IsDrawHeadersOnEveryPage: Boolean;
begin
  Result := HeadersOnEveryPage;
end;

function TBasedxReportLink.IsDrawHeaderCornersOnEveryPage: Boolean;
begin
  Result := HeadersOnEveryPage;
end;

function TBasedxReportLink.IsDrawRowHeadersOnEveryPage: Boolean;
begin
  Result := HeadersOnEveryPage;
end;

procedure TBasedxReportLink.ConstructReport(AReportCells: TdxReportCells);
begin
  ClearGDIPools;
  AddFontToPool(Font);
end;

procedure TBasedxReportLink.ConvertCoords;
begin
  FReportCells.ConvertCoords(PixelsNumerator, PixelsDenominator);
end;

procedure TBasedxReportLink.MakeDelimiters(
  AReportCells: TdxReportCells; AHorzDelimiters, AVertDelimiters: TList);
begin
end;

procedure TBasedxReportLink.MakeHardDelimiters(
  AReportCells: TdxReportCells; AHorzDelimiters, AVertDelimiters: TList);
begin
end;

procedure TBasedxReportLink.DoDataSourceChanged;

  procedure AssignReportNote(ADest, ASource: TdxReportNote);
  begin
    ADest.BeginUpdate;
    try
      ADest.Assign(ASource);
    finally
      ADest.CancelUpdate;
    end;
  end;

begin
  dxCallNotify(OnDataSourceChanged, Self);
  if IsRebuildNeededAndAllowed then
  begin
    if DataSource = rldsComponent then
    begin
      ReportDocument.Assign(FSavedReportDocument);
      AssignReportNote(ReportTitle, FSavedReportTitle);
      AssignReportNote(ReportFootnotes, FSavedReportFootnotes);
    end
    else
    begin
      FSavedReportDocument.Assign(ReportDocument);
      FSavedReportFootnotes.Assign(ReportFootnotes);
      FSavedReportTitle.Assign(ReportTitle);
    end;
    RebuildReport;
  end
  else
    if DataSource = rldsComponent then
      DestroyReport;
end;

procedure TBasedxReportLink.CustomDraw(AnItem: TAbstractdxReportCellData; ACanvas: TCanvas;
  ABoundsRect, AClientRect: TRect; var ADone: Boolean);
begin
end;

function TBasedxReportLink.IsScaleGridLines: Boolean;
begin
  Result := True;
end;

function TBasedxReportLink.IsSupportedCustomDraw(Item: TAbstractdxReportCellData): Boolean;
begin
  Result := False;
end;

{ TdxCompositionLinkItem }

constructor TdxCompositionLinkItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FBuiltIn := (Composition <> nil) and Composition.IsDesigning;
end;

procedure TdxCompositionLinkItem.Assign(Source: TPersistent);

  function IsInheritedForm: Boolean;
  begin
    with Composition do
      Result := (csUpdating in ComponentState) or (csUpdating in ComponentPrinter.ComponentState) or
        ((ComponentPrinter.Owner <> nil) and (csUpdating in ComponentPrinter.Owner.ComponentState));
  end;

begin
  if Source is TdxCompositionLinkItem then
    with TdxCompositionLinkItem(Source) do
      if (ReportLink <> nil) and (Self.Composition <> nil) and Self.Composition.IsComponentPrinterAvailable and IsInheritedForm then
        Self.ReportLink := Self.Composition.ComponentPrinter.LinkByName(ReportLink.Name)
      else
        Self.ReportLink := ReportLink
  else
    inherited;
end;

procedure TdxCompositionLinkItem.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('BuiltInCompositionItem', ReadData, WriteData, True);
  Filer.DefineProperty('LinkName', ReadLinkName, WriteLinkName,
    Composition.InternalStreaming and (ReportLink <> nil));
end;

procedure TdxCompositionLinkItem.SetIndex(Value: Integer);
begin
  if Collection <> nil then
    TdxCompositionLinkItems(Collection).FDontNeedRebuild := True;
  inherited;
end;

function TdxCompositionLinkItem.Composition: TdxCompositionReportLink;
begin
  if Collection <> nil then
    Result := TdxCompositionLinkItems(Collection).Composition
  else
    Result := nil;
end;

procedure TdxCompositionLinkItem.SetReportLink(Value: TBasedxReportLink);
begin
  if (FReportLink <> Value) and (Collection <> nil) and
   ((Value = nil) or TdxCompositionLinkItems(Collection).IsLinkComposable(Value)) then
  begin
    if (FReportLink <> nil) and (Composition <> nil) and Composition.IsCurrentLink and
      Composition.PreviewExists and not TdxCompositionLinkItems(Collection).LinkExists(Value)
    then
      Composition.ComponentPrinter.DestroyPreviewWindowForm;

    FReportLink := Value;
    if Collection <> nil then
      TdxCompositionLinkItems(Collection).FDontNeedRebuild := False;
    Changed(True);
  end;
end;

procedure TdxCompositionLinkItem.ReadData(Reader: TReader);
begin
  FBuiltIn := Reader.ReadBoolean;
end;

procedure TdxCompositionLinkItem.ReadLinkName(Reader: TReader);
begin
  FLoadingReportLinkName := Reader.ReadString;
end;

procedure TdxCompositionLinkItem.WriteData(Writer: TWriter);
begin
  Writer.WriteBoolean(FBuiltIn);
end;

procedure TdxCompositionLinkItem.WriteLinkName(Writer: TWriter);
begin
  Writer.WriteString(cxGetFullComponentName(ReportLink));
end;

{ TdxCompositionLinkItems }

constructor TdxCompositionLinkItems.Create(AComposition: TdxCompositionReportLink);
begin
  inherited Create(TdxCompositionLinkItem);
  FComposition := AComposition;
end;

function TdxCompositionLinkItems.Add: TdxCompositionLinkItem;
begin
  Result := TdxCompositionLinkItem(inherited Add);
end;

function TdxCompositionLinkItems.AddLink(AReportLink: TBasedxReportLink): TdxCompositionLinkItem;
begin
  if IsLinkComposable(AReportLink) then
  begin
    Result := Add;
    Result.ReportLink := AReportLink;
  end
  else
    Result := nil;
end;

procedure TdxCompositionLinkItems.DeleteItemsByLink(AReportLink: TBasedxReportLink);
var
  List: TList;
  I: Integer;
begin
  List := TList.Create;
  try
    GetLinkEntries(AReportLink, List);
    if List.Count <> 0 then
    begin
      BeginUpdate;
      try
        for I := 0 to List.Count - 1 do
          TObject(List[I]).Free;
      finally
        EndUpdate;
      end;
    end;
  finally
    List.Free;
  end;
end;

procedure TdxCompositionLinkItems.DeleteNonBuiltIns;
var
  I: Integer;
  Item: TdxCompositionLinkItem;
begin
  BeginUpdate;
  try
    for I := Count - 1 downto 0 do
    begin
      Item := Items[I];
      if not Item.BuiltIn then Item.Free;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TdxCompositionLinkItems.GetLinkEntries(AReportLink: TBasedxReportLink; AList: TList);
var
  I: Integer;
  Item: TdxCompositionLinkItem;
begin
  for I := 0 to Count - 1 do
  begin
    Item := Items[I];
    if Item.ReportLink = AReportLink then
      AList.Add(Item);
  end;
end;

function TdxCompositionLinkItems.IndexOfLink(AReportLink: TBasedxReportLink): Integer;
begin
  for Result := 0 to Count - 1 do
    if AReportLink = Items[Result].ReportLink then
      Exit;
  Result := -1;
end;

function TdxCompositionLinkItems.IsLinkComposable(AReportLink: TBasedxReportLink): Boolean;
begin
  Result := (AReportLink <> nil) and (Composition <> nil) and AReportLink.IsComposable(Composition) and
    (AReportLink.ComponentPrinter = Composition.ComponentPrinter) and
    not (AReportLink is TdxCompositionReportLink);
end;

function TdxCompositionLinkItems.LinkExists(AReportLink: TBasedxReportLink): Boolean;
var
  List: TList;
begin
  List := TList.Create;
  try
    GetLinkEntries(AReportLink, List);
    Result := List.Count <> 0;
  finally
    List.Free;
  end;
end;

function TdxCompositionLinkItems.NextAssignedItem(AnItem: TdxCompositionLinkItem): TdxCompositionLinkItem;
var
  Index: Integer;
begin
  if AnItem <> nil then
    Index := AnItem.Index + 1
  else
    Index := Count;
  while (Index < Count) and (Items[Index].ReportLink = nil) do
    Inc(Index);
  if Index < Count then
    Result := Items[Index]
  else
    Result := nil;
end;

function TdxCompositionLinkItems.NonBuiltInsExists: Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 0 to Count - 1 do
    if not Items[I].BuiltIn then Exit;
  Result := False;
end;

function TdxCompositionLinkItems.PrevAssignedItem(AnItem: TdxCompositionLinkItem): TdxCompositionLinkItem;
var
  Index: Integer;
begin
  if AnItem <> nil then
    Index := AnItem.Index - 1
  else
    Index := -1;
  while (Index > -1) and (Items[Index].ReportLink = nil) do
    Dec(Index);
  if Index > -1 then
    Result := Items[Index]
  else
    Result := nil;
end;

procedure TdxCompositionLinkItems.CorrectLinksAfterLoadings;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].FReportLink := GetComponentByName(Items[I].FLoadingReportLinkName) as TBasedxReportLink;
end;

function TdxCompositionLinkItems.GetOwner: TPersistent;
begin
  Result := Composition;
end;

procedure TdxCompositionLinkItems.Update(Item: TCollectionItem);
begin
  if (Composition <> nil) and not (Composition.IsDestroying or Composition.IsLoading) then
  begin
    if FDontNeedRebuild or not Composition.DataProviderPresent then
    begin
      Composition.UpdateComposition(ucAll);
      FDontNeedRebuild := False;
    end
    else
      if Composition.DataProviderPresent then // TODO: Check non PreviewState
        Composition.RebuildReport;
  end;
end;

function TdxCompositionLinkItems.GetItem(Index: Integer): TdxCompositionLinkItem;
begin
  Result := TdxCompositionLinkItem(inherited Items[Index]);
end;

procedure TdxCompositionLinkItems.SetItem(Index: Integer; Value: TdxCompositionLinkItem);
begin
  inherited SetItem(Index, Value);
end;

{ TdxCompositionLinkItemList }

function TdxCompositionLinkItemList.GetObjectEx(Index: Integer): TdxCompositionLinkItem;
begin
  Result := TdxCompositionLinkItem(inherited Objects[Index]);
end;

function TdxCompositionLinkItemList.GetReportLink(Index: Integer): TBasedxReportLink;
begin
  Result := Objects[Index].ReportLink;
end;

{ TdxPSCompositionReportRenderRowInfo }

constructor TdxPSCompositionReportRenderRowInfo.Create(
  AItem: TdxCompositionLinkItem; APageIndex, APageAbsoluteIndex: Integer);
begin
  inherited Create;
  FItem := AItem;
  FPageIndex := APageIndex;
  FPageAbsoluteIndex := APageAbsoluteIndex;
end;

destructor TdxPSCompositionReportRenderRowInfo.Destroy;
begin
  FreeAndNil(FComposedLinks);
  inherited Destroy;
end;

function TdxPSCompositionReportRenderRowInfo.CalculateContentAreaHeight(AReportLink: TBasedxReportLink): Integer;
var
  X: Integer;
begin
  CalculateContentInfo(AReportLink, X, Result);
end;

procedure TdxPSCompositionReportRenderRowInfo.CalculateContentInfo(
  AReportLink: TBasedxReportLink; out AOffset, AHeight: Integer);

  function GetPageContentHeight(APageInfo: TdxPSPageRenderInfo): Integer;
  begin
    Result := APageInfo.TitleHeight + cxRectHeight(APageInfo.ContentBounds);
    Result := MulDiv(Result, APageInfo.RenderInfo.ScaleFactor, 100);
    Result := MulDiv(Result, FUnitsPerInch, APageInfo.RenderInfo.UnitsPerInch);
    Result := Result + MulDiv(dxPSIndentBetweenComposedPages, FUnitsPerInch, PixelsDenominator);
  end;

var
  AComposedLink: TBasedxReportLink;
  APageInfo: TdxPSPageRenderInfo;
  I: Integer;
begin
  APageInfo := ReportLink.RenderInfo.PageRenderInfos[PageIndex];
  AOffset := GetPageContentHeight(APageInfo);
  for I := 0 to ComposedLinksCount - 1 do
  begin
    AComposedLink := ComposedLinks[I];
    if AComposedLink <> AReportLink then
    begin
      if AComposedLink.RenderInfo.PageRenderInfoCount > 0 then
        Inc(AOffset, GetPageContentHeight(AComposedLink.RenderInfo.PageRenderInfos[0]))
    end
    else
      Break;
  end;
  AHeight := MulDiv(APageInfo.PaintSize.Y, FUnitsPerInch, APageInfo.RenderInfo.UnitsPerInch) - AOffset;

  if AReportLink <> nil then
  begin
    AHeight := MulDiv(AHeight, AReportLink.RenderInfo.UnitsPerInch, FUnitsPerInch);
    AOffset := MulDiv(AOffset, AReportLink.RenderInfo.UnitsPerInch, FUnitsPerInch);
  end;
end;

function TdxPSCompositionReportRenderRowInfo.CalculateContentOffset(AReportLink: TBasedxReportLink): Integer;
var
  X: Integer;
begin
  CalculateContentInfo(AReportLink, Result, X);
end;

function TdxPSCompositionReportRenderRowInfo.CanCompose(AReportLink: TBasedxReportLink): Boolean;

  function GetMinimalContentHeight(AReportLink: TBasedxReportLink): Integer;
  begin
    Result := AReportLink.RenderInfo.TitleHeight + Max(
      cxRectHeight(AReportLink.ReportCells.HeaderBoundsRect),
      cxRectHeight(AReportLink.ReportCells.HeaderCornerBoundsRect)) +
      cxRectHeight(AReportLink.ReportCells.FooterBoundsRect);
    if AReportLink.RenderInfo.DelimiterYCount >= 2 then
      Inc(Result, AReportLink.RenderInfo.DelimitersY[1]);
  end;

begin
  Result := (ReportLink <> AReportLink) and (AReportLink.ReportCells <> nil) and
    (ReportLink.RenderInfo.PageRenderInfos[PageIndex].IsBottomPage) and
    (ReportLink.RenderInfo.FootnotesHeight = 0) and
    (ReportLink.RenderInfo.PageColCount >= AReportLink.RenderInfo.PageColCount) and
    (CalculateContentAreaHeight(AReportLink) > GetMinimalContentHeight(AReportLink));
end;

function TdxPSCompositionReportRenderRowInfo.Compose(AReportLink: TBasedxReportLink): Boolean;
begin
  Result := CanCompose(AReportLink);
  if Result then
  begin
    if FComposedLinks = nil then
      FComposedLinks := TList.Create;
    FComposedLinks.Add(AReportLink);
  end;
end;

function TdxPSCompositionReportRenderRowInfo.ContainsPage(APageIndex: Integer): Boolean;
begin
  Result := (PageAbsoluteIndex <= APageIndex) and (APageIndex < PageAbsoluteIndex + PageCount);
end;

function TdxPSCompositionReportRenderRowInfo.IsPartOfComposition(AReportLink: TBasedxReportLink): Boolean;
begin
  Result := (FComposedLinks <> nil) and (FComposedLinks.IndexOf(AReportLink) >= 0);
end;

function TdxPSCompositionReportRenderRowInfo.GetComposedLink(Index: Integer): TBasedxReportLink;
begin
  Result := TBasedxReportLink(FComposedLinks[Index]);
end;

function TdxPSCompositionReportRenderRowInfo.GetComposedLinksCount: Integer;
begin
  if FComposedLinks <> nil then
    Result := FComposedLinks.Count
  else
    Result := 0;
end;

function TdxPSCompositionReportRenderRowInfo.GetPageCount: Integer;
begin
  Result := ReportLink.RenderInfo.PageColCount;
end;

function TdxPSCompositionReportRenderRowInfo.GetPageIndexRelativeToReportLink(APageIndex: Integer): Integer;
begin
  Result := APageIndex - PageAbsoluteIndex + PageIndex;
end;

function TdxPSCompositionReportRenderRowInfo.GetRenderInfo: TdxPSReportRenderInfo;
begin
  Result := ReportLink.RenderInfo;
end;

function TdxPSCompositionReportRenderRowInfo.GetReportLink: TBasedxReportLink;
begin
  Result := Item.ReportLink;
end;

{ TdxPSCompositionReportRenderInfo }

constructor TdxPSCompositionReportRenderInfo.Create(AReportLink: TBasedxReportLink);
begin
  inherited Create(AReportLink);
  FRowInfoList := TcxObjectList.Create;
end;

destructor TdxPSCompositionReportRenderInfo.Destroy;
begin
  FreeAndNil(FRowInfoList);
  inherited Destroy;
end;

function TdxPSCompositionReportRenderInfo.FindRowInfo(
  AAbsolutePageIndex: Integer; out ARowInfo: TdxPSCompositionReportRenderRowInfo): Boolean;
var
  I: Integer;
begin
  ARowInfo := nil;
  for I := 0 to RowInfoCount - 1 do
    if RowInfo[I].ContainsPage(AAbsolutePageIndex) then
    begin
      ARowInfo := RowInfo[I];
      Break;
    end;

  Result := ARowInfo <> nil;
end;

function TdxPSCompositionReportRenderInfo.FindRowInfo(AReportLink: TBasedxReportLink;
  APageIndex: Integer; out ARowInfo: TdxPSCompositionReportRenderRowInfo): Boolean;
var
  I: Integer;
begin
  Result := False;
  if APageIndex < AReportLink.RenderInfo.PageColCount then
  begin
    for I := 0 to RowInfoCount - 1 do
      if RowInfo[I].IsPartOfComposition(AReportLink) then
      begin
        ARowInfo := RowInfo[I];
        Result := True;
        Break;
      end;
  end;
end;

function TdxPSCompositionReportRenderInfo.GetCompositionInfo(
  APageIndex: Integer; out ARowInfo: TdxPSCompositionReportRenderRowInfo): Boolean;
begin
  Result := FindRowInfo(APageIndex, ARowInfo);
end;

function TdxPSCompositionReportRenderInfo.AddRowInfo(AItem: TdxCompositionLinkItem;
  AStartPageIndex, AStartPageAbsoluteIndex: Integer): TdxPSCompositionReportRenderRowInfo;
begin
  Result := TdxPSCompositionReportRenderRowInfo.Create(AItem, AStartPageIndex, AStartPageAbsoluteIndex);
  FRowInfoList.Add(Result);
end;

function TdxPSCompositionReportRenderInfo.CalculateNonEmptyPageCount: Integer;
var
  AItems: TdxCompositionLinkItemList;
  I: Integer;
begin
  Result := 0;
  AItems := GetItems;
  try
    for I := 0 to AItems.Count - 1 do
    begin
      Inc(Result, AItems.ReportLinks[I].RenderInfo.NonEmptyPageCount);
      Dec(Result, AItems.ReportLinks[I].RenderInfo.CompositionPagePartCount);
    end;
  finally
    AItems.Free;
  end;
end;

function TdxPSCompositionReportRenderInfo.CalculatePageColCount: Integer;
var
  AItems: TdxCompositionLinkItemList;
  I: Integer;
begin
  Result := 0;
  AItems := GetItems;
  try
    for I := 0 to AItems.Count - 1 do
      Result := Max(Result, AItems.ReportLinks[I].RenderInfo.PageColCount);
  finally
    AItems.Free;
  end;
end;

function TdxPSCompositionReportRenderInfo.CalculatePageRowCount: Integer;
var
  AItems: TdxCompositionLinkItemList;
  I: Integer;
begin
  Result := 0;
  AItems := GetItems;
  try
    for I := 0 to AItems.Count - 1 do
    begin
      Result := Max(Result, AItems.ReportLinks[I].RenderInfo.PageRowCount -
        Ord(AItems.ReportLinks[I].RenderInfo.CompositionPagePartCount > 0));
    end;
  finally
    AItems.Free;
  end;
end;

procedure TdxPSCompositionReportRenderInfo.DoCalculate;

  procedure CalculateRowsInfo(AStartFromIndex: Integer; AItem: TdxCompositionLinkItem;
    var APageIndex: Integer; var APrevRowInfo: TdxPSCompositionReportRenderRowInfo);
  var
    APageInfo: TdxPSPageRenderInfo;
    APrevRowIndex, I: Integer;
  begin
    APrevRowIndex := -1;
    for I := AStartFromIndex to AItem.ReportLink.RenderInfo.PageRenderInfoCount - 1 do
    begin
      APageInfo := AItem.ReportLink.RenderInfo.PageRenderInfos[I];
      if APrevRowIndex <> APageInfo.RowIndex then
      begin
        APrevRowInfo := AddRowInfo(AItem, I, APageIndex);
        APrevRowIndex := APageInfo.RowIndex;
      end;
      Inc(APageIndex);
    end;
  end;

  function SkipTopRowPages(AReportLink: TBasedxReportLink): Integer;
  begin
    Result := 0;
    while Result < AReportLink.RenderInfo.PageRenderInfoCount do
    begin
      if AReportLink.RenderInfo.PageRenderInfos[Result].IsTopPage then
        Inc(Result)
      else
        Break;
    end;
  end;

var
  AItem: TdxCompositionLinkItem;
  AItems: TdxCompositionLinkItemList;
  APageIndex: Integer;
  APrevRowInfo: TdxPSCompositionReportRenderRowInfo;
  AStartIndex, I: Integer;
begin
  AItems := GetItems;
  try
    APageIndex := 0;
    APrevRowInfo := nil;
    for I := 0 to AItems.Count - 1 do
    begin
      AItem := AItems.Objects[I];
      AItem.ReportLink.CalculateRenderInfos;

      AStartIndex := 0;
      if not ReportLink.StartEachItemFromNewPage then
      begin
        if (APrevRowInfo <> nil) and APrevRowInfo.CanCompose(AItem.ReportLink) then
        begin
          APrevRowInfo.Compose(AItem.ReportLink);
          AItem.ReportLink.CalculateRenderInfos;
          AStartIndex := SkipTopRowPages(AItem.ReportLink);
        end
      end;
      CalculateRowsInfo(AStartIndex, AItem, APageIndex, APrevRowInfo);
    end;
  finally
    AItems.Free;
  end;
end;

procedure TdxPSCompositionReportRenderInfo.Refresh;
begin
  FNonEmptyPageCount := -1;
  FPageColCount := -1;
  FPageRowCount := -1;
  FRowInfoList.Clear;
  inherited Refresh;
end;

function TdxPSCompositionReportRenderInfo.GetNonEmptyPageCount: Integer;
begin
  if FNonEmptyPageCount = -1 then
    FNonEmptyPageCount := CalculateNonEmptyPageCount;
  Result := FNonEmptyPageCount;
end;

function TdxPSCompositionReportRenderInfo.GetPageColCount: Integer;
begin
  if FPageColCount = -1 then
    FPageColCount := CalculatePageColCount;
  Result := FPageColCount;
end;

function TdxPSCompositionReportRenderInfo.GetRowInfo(
  Index: Integer): TdxPSCompositionReportRenderRowInfo;
begin
  Result := TdxPSCompositionReportRenderRowInfo(FRowInfoList[Index]);
end;

function TdxPSCompositionReportRenderInfo.GetRowInfoCount: Integer;
begin
  Result := FRowInfoList.Count;
end;

function TdxPSCompositionReportRenderInfo.GetPageRowCount: Integer;
begin
  if FPageRowCount = -1 then
    FPageRowCount := CalculatePageRowCount;
  Result := FPageRowCount;
end;

function TdxPSCompositionReportRenderInfo.GetItems: TdxCompositionLinkItemList;
begin
  Result := TdxCompositionLinkItemList.Create;
  ReportLink.GetItems(Result, True);
end;

function TdxPSCompositionReportRenderInfo.GetReportLink: TdxCompositionReportLink;
begin
  Result := inherited ReportLink as TdxCompositionReportLink;
end;

{ TdxPSCompositionReportRenderer }

procedure TdxPSCompositionReportRenderer.RenderPageEx(
  ACanvas: TdxPSReportRenderCustomCanvas; const APageBounds: TRect;
  APageIndex, AContinuousPageIndex, AZoomFactor: Integer);
var
  AComposedLink: TBasedxReportLink;
  ARelativePageIndex: Integer;
  ARowInfo: TdxPSCompositionReportRenderRowInfo;
  I: Integer;
begin
  if RenderInfo.FindRowInfo(APageIndex, ARowInfo) then
  begin
    if not ReportLink.ContinuousPageIndexes then
      dxHFFormatObject.TotalPages := ARowInfo.ReportLink.PageCount;

    ARowInfo.ReportLink.PaintPageEx(ACanvas, APageBounds,
      ARowInfo.GetPageIndexRelativeToReportLink(APageIndex),
      AContinuousPageIndex, AZoomFactor);

    ARelativePageIndex := APageIndex - ARowInfo.PageAbsoluteIndex;
    for I := 0 to ARowInfo.ComposedLinksCount - 1 do
    begin
      AComposedLink := ARowInfo.ComposedLinks[I];
      if ARelativePageIndex < AComposedLink.RenderInfo.PageColCount then
        AComposedLink.PaintPageEx(ACanvas, APageBounds, ARelativePageIndex, AContinuousPageIndex, AZoomFactor);
    end;
  end;
end;

function TdxPSCompositionReportRenderer.GetRenderInfo: TdxPSCompositionReportRenderInfo;
begin
  Result := inherited RenderInfo as TdxPSCompositionReportRenderInfo;
end;

function TdxPSCompositionReportRenderer.GetReportLink: TdxCompositionReportLink;
begin
  Result := inherited ReportLink as TdxCompositionReportLink;
end;

{ TdxPSReportCompositionDocument }

function TdxPSReportCompositionDocument.DefaultDescription: string;
begin
  Result := cxGetResourceString(@sdxComposition);
end;

function TdxPSReportCompositionDocument.GetReportLink: TdxCompositionReportLink;
begin
  Result := inherited ReportLink as TdxCompositionReportLink;
end;

{ TdxCompositionReportLink }

constructor TdxCompositionReportLink.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FContinuousPageIndexes := True;
  FStartEachItemFromNewPage := True;
  ReportDocument.Description := cxGetResourceString(@sdxComposition);
  FDesignerOptions := [coCanEdit, coShowDescription];
  FItems := TdxCompositionLinkItems.Create(Self);
end;

destructor TdxCompositionReportLink.Destroy;
begin
  FreeAndNil(FItems);
  inherited Destroy;
end;

procedure TdxCompositionReportLink.Assign(Source: TPersistent);
begin
  if Source is TdxCompositionReportLink then
  begin
    ContinuousPageIndexes := TdxCompositionReportLink(Source).ContinuousPageIndexes;
    DesignerOptions := TdxCompositionReportLink(Source).DesignerOptions;
    StartEachItemFromNewPage := TdxCompositionReportLink(Source).StartEachItemFromNewPage;
  end;
  inherited Assign(Source);
end;

function TdxCompositionReportLink.DefaultDesignerCaption: string;
begin
  Result := cxGetResourceString(@sdxCompositionDesignerCaption);
end;

class function TdxCompositionReportLink.CanBeUsedAsStub: Boolean;
begin
  Result := False;
end;

function TdxCompositionReportLink.DataProviderPresent: Boolean;
var
  AReportLink: TBasedxReportLink;
  I: Integer;
begin
  Result := True;
  for I := 0 to Items.Count - 1 do
  begin
    AReportLink := Items[I].ReportLink;
    if (AReportLink <> nil) and AReportLink.DataProviderPresent then
      Exit;
  end;
  Result := False;
end;

function TdxCompositionReportLink.IsEmptyPage(APageIndex: Integer): Boolean;
var
  ARowInfo: TdxPSCompositionReportRenderRowInfo;
begin
  if RenderInfo.FindRowInfo(APageIndex, ARowInfo) then
    Result := ARowInfo.ReportLink.IsEmptyPage(ARowInfo.GetPageIndexRelativeToReportLink(APageIndex))
  else
    Result := False;
end;

procedure TdxCompositionReportLink.GetItems(AStrings: TStrings; AExcludeUnassigned: Boolean);
var
  I: Integer;
  Item: TdxCompositionLinkItem;
  S: string;
begin
  AStrings.BeginUpdate;
  try
    for I := 0 to Items.Count - 1 do
    begin
      Item := Items[I];
      if (Item.ReportLink = nil) and AExcludeUnassigned then
        Continue;
      if Item.ReportLink = nil then
        S := ''
      else
        S := Item.ReportLink.Caption;
      AStrings.AddObject(S, Item);
    end;
  finally
    AStrings.EndUpdate;
  end;
end;

procedure TdxCompositionReportLink.Notification(AComponent: TComponent; AOperation: TOperation);
var
  List: TList;
  I: Integer;
begin
  inherited;
  if not (csDestroying in ComponentState) and (AOperation = opRemove) and (AComponent is TBasedxReportLink) then
  begin
    List := TList.Create;
    try
      Items.GetLinkEntries(TBasedxReportLink(AComponent), List);
      for I := 0 to List.Count - 1 do
        TdxCompositionLinkItem(List[I]).ReportLink := nil;
    finally
      List.Free;
    end;
  end;
end;

function TdxCompositionReportLink.GetCapabilities: TdxReportLinkCapabilities;
var
  ALink: TBasedxReportLink;
  I: Integer;
begin
  Result := inherited GetCapabilities - [rlcTitle, rlcFootnotes];
  for I := 0 to Items.Count - 1 do
  begin
    ALink := Items[I].ReportLink;
    if ALink <> nil then
      Result := Result * ALink.Capabilities;
  end;
end;

function TdxCompositionReportLink.GetContinuousPageIndexes: Boolean;
begin
  Result := FContinuousPageIndexes;
end;

procedure TdxCompositionReportLink.SetContinuousPageIndexes(Value: Boolean);
begin
  if Value <> FContinuousPageIndexes then
  begin
    FContinuousPageIndexes := Value;
    if not ContinuousPageIndexes then
      StartEachItemFromNewPage := True;
    ComponentPrinter.DoLayoutChanged(Self);
  end;
end;

function TdxCompositionReportLink.GetAllowContinuousPageIndexes: Boolean;
begin
  Result := True;
end;

function TdxCompositionReportLink.GetRebuildOnPageParamsChange(AUpdateCodes: TdxPrinterPageUpdateCodes): Boolean;
var
  I: Integer;
  ReportLink: TBasedxReportLink;
begin
  Result := True;
  for I := 0 to Items.Count - 1 do
  begin
    ReportLink := Items[I].ReportLink;
    if (ReportLink <> nil) and ReportLink.GetRebuildOnPageParamsChange(AUpdateCodes) then
      Exit;
  end;
  Result := False;
end;

procedure TdxCompositionReportLink.ConstructReport(AReportCells: TdxReportCells);
begin
end;

procedure TdxCompositionReportLink.DoCreateReport;
var
  I: Integer;
  Item: TdxCompositionLinkItem;
  ReportLink: TBasedxReportLink;
begin
  Include(FCompositionState, csRebuildReportLink);
  try
    for I := 0 to Items.Count - 1 do
    begin
      Item := Items[I];
      ReportLink := Item.ReportLink;
      if (ReportLink <> nil) and ReportLink.DataProviderPresent and
        ((FInvalidatedLinks = nil) or (FInvalidatedLinks.IndexOf(ReportLink) <> -1)) then
      begin
        DoBeforeBuildReport(Item);
        try
          ReportLink.RebuildReport;
        finally
          DoAfterBuildReport(Item);
        end;
      end;
      DoProgress(100 * (I + 1) / Items.Count);
      if AbortBuilding then Break;
    end;
  finally
    Exclude(FCompositionState, csRebuildReportLink);
  end;

  if not AbortBuilding then
    CalculateRenderInfos;
end;

function TdxCompositionReportLink.GetRendererClass: TdxPSReportRendererClass;
begin
  Result := TdxPSCompositionReportRenderer;
end;

function TdxCompositionReportLink.GetRenderInfoClass: TdxPSReportRenderInfoClass;
begin
  Result := TdxPSCompositionReportRenderInfo;
end;

function TdxCompositionReportLink.GetReportHeight: Integer;
var
  I: Integer;
  ReportLink: TBasedxReportLink;
begin
  Result := 0;
  for I := 0 to Items.Count - 1 do
  begin
    ReportLink := Items[I].ReportLink;
    if ReportLink <> nil then
      Inc(Result, ReportLink.ReportHeight);
  end;
end;

function TdxCompositionReportLink.GetReportWidth: Integer;
var
  I: Integer;
  ReportLink: TBasedxReportLink;
begin
  Result := 0;
  for I := 0 to Items.Count - 1 do
  begin
    ReportLink := Items[I].ReportLink;
    if ReportLink <> nil then
      Result := Max(Result, ReportLink.ReportWidth);
  end;
end;

procedure TdxCompositionReportLink.InternalRestoreDefaults;
var
  I: Integer;
  ReportLink: TBasedxReportLink;
begin
  FContinuousPageIndexes := True;
  FStartEachItemFromNewPage := True;
  ReportDocument.Description := cxGetResourceString(@sdxComposition);
  FDesignerCaption := cxGetResourceString(@sdxCompositionDesignerCaption);
  FDesignerOptions := [coCanEdit, coShowDescription];
  for I := 0 to Items.Count - 1 do
  begin
    ReportLink := Items[I].ReportLink;
    if ReportLink <> nil then
      ReportLink.RestoreDefaults;
  end;
end;

procedure TdxCompositionReportLink.InternalRestoreFromOriginal;
var
  I: Integer;
  ReportLink: TBasedxReportLink;
begin
  for I := 0 to Items.Count - 1 do
  begin
    ReportLink := Items[I].ReportLink;
    if ReportLink <> nil then
      ReportLink.RestoreFromOriginal;
  end;
end;

procedure TdxCompositionReportLink.ShowPageFooterChanged;
begin
  CalculateRenderInfos;
end;

procedure TdxCompositionReportLink.ShowPageHeaderChanged;
begin
  CalculateRenderInfos;
end;

procedure TdxCompositionReportLink.StdProcessDataSourceDontPresent;
begin
//  raise EdxReportLink.Create(cxGetResourceString(@sdxDataSourceDontPresent));
end;

procedure TdxCompositionReportLink.DoAfterBuildReport(AItem: TdxCompositionLinkItem);
begin
  if Assigned(FOnAfterBuildReport) then FOnAfterBuildReport(Self, AItem);
end;

procedure TdxCompositionReportLink.DoBeforeBuildReport(AItem: TdxCompositionLinkItem);
begin
  if Assigned(FOnBeforeBuildReport) then FOnBeforeBuildReport(Self, AItem);
end;

class function TdxCompositionReportLink.GetReportDocumentClass: TdxPSReportDocumentClass;
begin
  Result := TdxPSReportCompositionDocument;
end;

class function TdxCompositionReportLink.Serializable: Boolean;
begin
  Result := False;
end;

function TdxCompositionReportLink.GetRenderer: TdxPSCompositionReportRenderer;
begin
  Result := inherited Renderer as TdxPSCompositionReportRenderer;
end;

function TdxCompositionReportLink.GetRenderInfo: TdxPSCompositionReportRenderInfo;
begin
  Result := inherited RenderInfo as TdxPSCompositionReportRenderInfo;
end;

function TdxCompositionReportLink.GetReportDocument: TdxPSReportCompositionDocument;
begin
  Result := inherited ReportDocument as TdxPSReportCompositionDocument;
end;

procedure TdxCompositionReportLink.SetItems(Value: TdxCompositionLinkItems);
begin
  FItems.Assign(Value);
end;

procedure TdxCompositionReportLink.SetReportDocument(Value: TdxPSReportCompositionDocument);
begin
  inherited ReportDocument := Value;
end;

procedure TdxCompositionReportLink.SetStartEachItemFromNewPage(AValue: Boolean);
begin
  if AValue <> FStartEachItemFromNewPage then
  begin
    FStartEachItemFromNewPage := AValue;
    if not StartEachItemFromNewPage then
      ContinuousPageIndexes := True;
    RebuildReport;
  end;
end;

procedure TdxCompositionReportLink.ActivateLink(AReportLink: TBasedxReportLink);
begin
  Include(FCompositionState, csRebuildReportLink);
  try
    FInvalidatedLinks := TList.Create;
    try
      FInvalidatedLinks.Add(AReportLink);
      RebuildReport;
    finally
      FreeAndNil(FInvalidatedLinks);
    end;
  finally
    Exclude(FCompositionState, csRebuildReportLink);
  end;
end;

procedure TdxCompositionReportLink.UpdateComposition(AUpdateCodes: TdxPrinterPageUpdateCodes);
begin
  PageParamsChanged(RealPrinterPage, CurrentPrintStyle, AUpdateCodes);
end;

{ TAbstractdxReportLinkDesignWindow }

constructor TAbstractdxReportLinkDesignWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Caption := cxGetResourceString(@sdxReportDesignerCaption);
end;

function TAbstractdxReportLinkDesignWindow.Execute: Boolean;
begin
  Include(FState, dwsInitialize);
  try
    Initialize;
  finally
    Exclude(FState, dwsInitialize);
  end;
  Result := (ReportLink <> nil) and (ShowModal = mrOK);// and Modified and not Applyed;
end;

function TAbstractdxReportLinkDesignWindow.CanApply: Boolean;
begin
  Result := (ReportLink <> nil) and ReportLink.DataProviderPresent and Modified and not (Applyed or ReportLink.IsAggregated);
end;

procedure TAbstractdxReportLinkDesignWindow.CreateWnd;
begin
  inherited CreateWnd;
  if Icon.Handle = 0 then
    dxLoadIconFromResource(Icon, IDB_DXPSREPORTDESIGNER);
  SendMessage(Handle, WM_SETICON, 1, Icon.Handle);
end;

procedure TAbstractdxReportLinkDesignWindow.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (ReportLink <> nil) and ReportLink.PreviewExists and ReportLink.IsBuilding and (Key = VK_ESCAPE) then
    ReportLink.AbortBuilding := True
  else
    inherited;
end;

procedure TAbstractdxReportLinkDesignWindow.AfterRebuildReport;
begin
  KeyPreview := FPrevKeyPreview;
end;

procedure TAbstractdxReportLinkDesignWindow.BeforeRebuildReport;
begin
  FPrevKeyPreview := KeyPreview;
  KeyPreview := True;
end;

procedure TAbstractdxReportLinkDesignWindow.Initialize;
begin
  LoadStrings;
  Caption := ReportLink.DesignerCaption;
  if ReportLink.IsDesigning and (Component <> nil) then
    Caption := Caption + ' : ' + Component.Name;
  if ReportLink.DesignerHelpContext <> 0 then
    HelpContext := ReportLink.DesignerHelpContext;
end;

procedure TAbstractdxReportLinkDesignWindow.LoadStrings;
begin
end;

procedure TAbstractdxReportLinkDesignWindow.UpdateControlsState;
begin
end;

function TAbstractdxReportLinkDesignWindow.GetComponent: TComponent;
begin
  if ReportLink <> nil then
    Result := ReportLink.Component
  else
    Result := nil;
end;

function TAbstractdxReportLinkDesignWindow.GetIsDesigning: Boolean;
begin
  Result := (ReportLink <> nil) and Reportlink.IsDesigning;
end;

function TAbstractdxReportLinkDesignWindow.IsCaptionStored: Boolean;
begin
  Result := Caption <> cxGetResourceString(@sdxReportDesignerCaption);
end;

procedure TAbstractdxReportLinkDesignWindow.SetModified(Value: Boolean);
begin
  FModified := Value;
  if Modified and Applyed then
    Applyed := False;
  UpdateControlsState;
end;

procedure TAbstractdxReportLinkDesignWindow.WMHelp(var Message: TWMHelp);
var
  Control: TWinControl;
  ContextID: Integer;
begin
  if csDesigning in ComponentState then
    inherited
  else
  begin
    ContextID := 0;
    with Message.HelpInfo^ do
      if iContextType = HELPINFO_WINDOW then
      begin
        Control := FindControl(hItemHandle);
        if Control = nil then Exit;
        Control := GetParentForm(Control);
        if Control = nil then Exit;
        ContextID := Control.HelpContext;
      end;
    if ContextID <> 0 then Application.HelpContext(ContextID);
  end;
end;

{ TdxPSPrintStyle }

constructor TdxPSPrintStyle.Create(AOwner: TComponent);
begin
  inherited;
  AddStdHFFunctions;
end;

function TdxPSPrintStyle.DefaultPageFooterText(APart: TdxPageTitlePart): string;
var
  Index: Integer;
begin
  Index := -1;
  if dxHFFunctionLibrary <> nil then
    case APart of
      tpCenter:
        Index := dxHFFunctionLibrary.IndexOfByClass(TdxHFPageNumberFunction);
      tpRight:
        Index := dxHFFunctionLibrary.IndexOfByClass(TdxHFDateFunction);
    end;

  if Index <> -1 then
    Result := dxHFFunctionLibrary[Index].TemplateString + dxCRLF
  else
    Result := inherited DefaultPageFooterText(APart);
end;

function TdxPSPrintStyle.DefaultStyleCaption: string;
begin
  Result := cxGetResourceString(@sdxStandardStyle);
end;

procedure TdxPSPrintStyle.AddStdHFFunctions;
var
  Index: Integer;
begin
  if dxHFFunctionLibrary = nil then Exit;
  with PrinterPage.PageFooter do
  begin
    Index := dxHFFunctionLibrary.IndexOfByClass(TdxHFPageNumberFunction);
    if Index <> -1 then
      CenterTitle.Text := dxHFFunctionLibrary[Index].TemplateString;
    Index := dxHFFunctionLibrary.IndexOfByClass(TdxHFDateFunction);
    if Index <> -1 then
      RightTitle.Text := dxHFFunctionLibrary[Index].TemplateString;
  end;
end;

procedure TdxPSPrintStyle.AfterGenerating;
begin
  DoAfterGenerating;
end;

procedure TdxPSPrintStyle.BeforeGenerating;
begin
  DoBeforeGenerating;
end;

procedure TdxPSPrintStyle.DoAfterGenerating;
begin
  dxCallNotify(OnAfterGenerating, Self);
end;

procedure TdxPSPrintStyle.DoAfterPrinting;
begin
  dxCallNotify(OnAfterPrinting, Self);
end;

procedure TdxPSPrintStyle.DoBeforeGenerating;
begin
  dxCallNotify(OnBeforeGenerating, Self);
end;

procedure TdxPSPrintStyle.DoBeforePrinting;
begin
  dxCallNotify(OnBeforePrinting, Self);
end;

procedure TdxPSPrintStyle.InitializeDefaultStyleGlyph(ABitmap: TBitmap);
begin
  inherited InitializeDefaultStyleGlyph(ABitmap);
  dxLoadBitmapFromResource(ABitmap, IDB_DXPSPRINTSTYLE_STANDARD);
end;

{ TdxComponentPrinterThumbnailsOptions }

constructor TdxComponentPrinterThumbnailsOptions.Create(APreviewOptions: TdxBasePreviewOptions);
begin
  inherited Create;
  FPreviewOptions := APreviewOptions;
  FFont := TFont.Create;
  FFont.OnChange := FontChangeHandler;
  RestoreDefaults;
end;

destructor TdxComponentPrinterThumbnailsOptions.Destroy;
begin
  FreeAndNil(FDefaultFont);
  FreeAndNil(FFont);
  inherited;
end;

procedure TdxComponentPrinterThumbnailsOptions.Assign(Source: TPersistent);
begin
  if Source is TdxComponentPrinterThumbnailsOptions then
  begin
    PreviewOptions.BeginUpdate;
    try
      DoAssign(TdxComponentPrinterThumbnailsOptions(Source));
    finally
      PreviewOptions.EndUpdate;
    end;
  end
  else
    inherited;
end;

function TdxComponentPrinterThumbnailsOptions.DefaultFont: TFont;
begin
  if FDefaultFont = nil then
  begin
    FDefaultFont := TFont.Create;
    InitializeDefaultFont(FDefaultFont);
  end;
  Result := FDefaultFont;
end;

procedure TdxComponentPrinterThumbnailsOptions.RestoreDefaults;
begin
  inherited;
  Font := DefaultFont;
  ShowPageNumbers := True;
end;

procedure TdxComponentPrinterThumbnailsOptions.DoAssign(Source: TdxComponentPrinterThumbnailsOptions);
begin
  Font := Source.Font;
  ShowPageNumbers := Source.ShowPageNumbers;
end;

procedure TdxComponentPrinterThumbnailsOptions.InitializeDefaultFont(AFont: TFont);
begin
  AFont.Color := dxPSDefaultPreviewThumbnailsFontColor;
  AFont.Name := dxPSDefaultPreviewThumbnailsFontName;
  AFont.Size := dxPSDefaultPreviewThumbnailsFontSize;
  AFont.Style := dxPSDefaultPreviewThumbnailsFontStyle
end;

function TdxComponentPrinterThumbnailsOptions.IsFontStored: Boolean;
begin
  Result := not dxPSUtl.dxAreFontsEqual(Font, DefaultFont);
end;

procedure TdxComponentPrinterThumbnailsOptions.FontChangeHandler(Sender: TObject);
begin
  PreviewOptions.Changed;
end;

procedure TdxComponentPrinterThumbnailsOptions.SetFont(Value: TFont);
begin
  Font.Assign(Value);
end;

procedure TdxComponentPrinterThumbnailsOptions.SetShowPageNumbers(Value: Boolean);
begin
  if FShowPageNumbers <> Value then
  begin
    FShowPageNumbers := Value;
    PreviewOptions.Changed;
  end;
end;

{ TdxBasePreviewOptions }

constructor TdxBasePreviewOptions.Create;
begin
  inherited Create;
  FSaveZoomPosition := True;
  FEnableOptions := dxDefaultPreviewEnableOptions;
  FThumbnailsOptions := CreateThumbnailsOptions;
  FVisibleOptions := dxDefaultPreviewVisibleOptions;
end;

destructor TdxBasePreviewOptions.Destroy;
begin
  FreeAndNil(FThumbnailsOptions);
  inherited Destroy;
end;

function TdxBasePreviewOptions.CreateThumbnailsOptions: TdxComponentPrinterThumbnailsOptions;
begin
  Result := TdxComponentPrinterThumbnailsOptions.Create(Self);
end;

procedure TdxBasePreviewOptions.Changed;
begin
  if not IsLocked then
    dxCallNotify(OnChanged, Self);
end;

procedure TdxBasePreviewOptions.DoAssign(Source: TdxBaseObject);
begin
  inherited DoAssign(Source);
  EnableOptions := TdxBasePreviewOptions(Source).EnableOptions;
  SaveZoomPosition := TdxBasePreviewOptions(Source).SaveZoomPosition;
  ThumbnailsOptions := TdxBasePreviewOptions(Source).ThumbnailsOptions;
  VisibleOptions := TdxBasePreviewOptions(Source).VisibleOptions;
end;

procedure TdxBasePreviewOptions.DoRestoreDefaults;
begin
  inherited DoRestoreDefaults;

  SaveZoomPosition := True;
  EnableOptions := dxDefaultPreviewEnableOptions;
  ThumbnailsOptions.RestoreDefaults;
  VisibleOptions := dxDefaultPreviewVisibleOptions;
end;

procedure TdxBasePreviewOptions.LockUpdate(ALockState: TdxLockState);
begin
  inherited LockUpdate(ALockState);
  if ALockState = lsUnlock then
    Changed;
end;

procedure TdxBasePreviewOptions.SetEnableOptions(Value: TdxPreviewEnableOptions);
begin
  if FEnableOptions <> Value then
  begin
    FEnableOptions := Value;
    Changed;
  end;
end;

procedure TdxBasePreviewOptions.SetShowExplorer(Value: Boolean);
begin
  if FShowExplorer <> Value then
  begin
    FShowExplorer := Value;
    Changed;
  end;
end;

procedure TdxBasePreviewOptions.SetThumbnailsOptions(Value: TdxComponentPrinterThumbnailsOptions);
begin
  ThumbnailsOptions.Assign(Value);
  Changed;
end;

procedure TdxBasePreviewOptions.SetVisibleOptions(Value: TdxPreviewVisibleOptions);
begin
  if FVisibleOptions <> Value then
  begin
    FVisibleOptions := Value;
    Changed;
  end;
end;

{ TdxPreviewWindowThumbnailsOptions }

constructor TdxPreviewWindowThumbnailsOptions.Create(APreviewOptions: TdxBasePreviewOptions);
begin
  inherited Create(APreviewOptions);
  FSize := tsLarge;
end;

procedure TdxPreviewWindowThumbnailsOptions.DoAssign(Source: TdxComponentPrinterThumbnailsOptions);
begin
  inherited DoAssign(Source);

  if Source is TdxPreviewWindowThumbnailsOptions then
  begin
    Visible := TdxPreviewWindowThumbnailsOptions(Source).Visible;
    Size := TdxPreviewWindowThumbnailsOptions(Source).Size;
  end;
end;

procedure TdxPreviewWindowThumbnailsOptions.RestoreDefaults;
begin
  inherited RestoreDefaults;
  Size := tsLarge;
  Visible := False;
end;

procedure TdxPreviewWindowThumbnailsOptions.SetSize(const Value: TdxPSThumbnailsSize);
begin
  if FSize <> Value then
  begin
    FSize := Value;
    PreviewOptions.Changed;
  end;
end;

procedure TdxPreviewWindowThumbnailsOptions.SetVisible(const Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    PreviewOptions.Changed;
  end;
end;

{ TdxPreviewWindowOptions }

function TdxPreviewWindowOptions.CreateThumbnailsOptions: TdxComponentPrinterThumbnailsOptions;
begin
  Result := TdxPreviewWindowThumbnailsOptions.Create(Self);
end;

function TdxPreviewWindowOptions.GetThumbnailsOptions: TdxPreviewWindowThumbnailsOptions;
begin
  Result := inherited ThumbnailsOptions as TdxPreviewWindowThumbnailsOptions;
end;

procedure TdxPreviewWindowOptions.SetThumbnailsOptions(AValue: TdxPreviewWindowThumbnailsOptions);
begin
  inherited ThumbnailsOptions := AValue;
end;

{ TdxPSCustomPreviewWindow }

constructor TdxPSCustomPreviewWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOptions := CreateOptions;
  FOptions.OnChanged := OptionsChangeHandler;
end;

destructor TdxPSCustomPreviewWindow.Destroy;
begin
  FreeAndNil(FOptions);
  inherited Destroy;
end;

procedure TdxPSCustomPreviewWindow.BeforeDestruction;
begin
  inherited BeforeDestruction;
  FIsDestroying := True;
  ComponentPrinter := nil;
end;

procedure TdxPSCustomPreviewWindow.InitContent;
begin
  // do nothing
end;

procedure TdxPSCustomPreviewWindow.InvalidateContent;
begin
  // do nothing
end;

procedure TdxPSCustomPreviewWindow.InvalidatePage(APageIndex: Integer);
begin
  // do nothing
end;

procedure TdxPSCustomPreviewWindow.InvalidateAllPages;
begin
  // do nothing
end;

procedure TdxPSCustomPreviewWindow.InvalidatePagesContent;
begin
  // do nothing
end;

procedure TdxPSCustomPreviewWindow.InvalidatePagesHeaderContent;
begin
  // do nothing
end;

procedure TdxPSCustomPreviewWindow.InvalidatePagesFooterContent;
begin
  // do nothing
end;

procedure TdxPSCustomPreviewWindow.LoadFromIniFile(AIniFile: TCustomIniFile; const ASectionName: string);
begin
  // do nothing
end;

procedure TdxPSCustomPreviewWindow.LoadFromIniFile(const AFileName: string);
var
  AIniFile: TCustomIniFile;
begin
  AIniFile := TMemIniFile.Create(AFileName);
  try
    LoadFromIniFile(AIniFile, dxGetStoringSectionName(Self));
  finally
    AIniFile.Free;
  end;
end;

procedure TdxPSCustomPreviewWindow.LoadFromRegistry(const APath: string);
var
  ARegIniFile: TRegistryIniFile;
begin
  ARegIniFile := TRegistryIniFile.Create('');
  try
    LoadFromIniFile(ARegIniFile, APath);
  finally
    ARegIniFile.Free;
  end;
end;

procedure TdxPSCustomPreviewWindow.SaveToIniFile(AIniFile: TCustomIniFile; const ASectionName: string);
begin
  // do nothing
end;

procedure TdxPSCustomPreviewWindow.SaveToIniFile(const AFileName: string);
var
  AIniFile: TCustomIniFile;
begin
  AIniFile := TMemIniFile.Create(AFileName);
  try
    SaveToIniFile(AIniFile, dxGetStoringSectionName(Self));
    AIniFile.UpdateFile;
  finally
    AIniFile.Free;
  end;
end;

procedure TdxPSCustomPreviewWindow.SaveToRegistry(const APath: string);
var
  ARegIniFile: TRegistryIniFile;
begin
  if APath <> '' then
  begin
    ARegIniFile := TRegistryIniFile.Create('');
    try
      SaveToIniFile(ARegIniFile, APath);
    finally
      ARegIniFile.Free;
    end;
  end;
end;

function TdxPSCustomPreviewWindow.CreateOptions: TdxPreviewWindowOptions;
begin
  Result := TdxPreviewWindowOptions.Create;
end;

procedure TdxPSCustomPreviewWindow.AfterComponentPrinterChanged;
begin
  if ComponentPrinter <> nil then
  begin
    ComponentPrinter.FreeNotification(Self);
    ComponentPrinter.Listeners.Add(Self);
  end;
end;

procedure TdxPSCustomPreviewWindow.BeforeComponentPrinterChanged;
begin
  if ComponentPrinter <> nil then
  begin
    ComponentPrinter.RemoveFreeNotification(Self);
    ComponentPrinter.Listeners.Remove(Self);
  end;
end;

procedure TdxPSCustomPreviewWindow.CheckRebuildReport;
begin
  if (ReportLink <> nil) and ReportLink.IsEmptyReport then
    RebuildReport;
end;

procedure TdxPSCustomPreviewWindow.RebuildReport;
begin
  if (ReportLink <> nil) and ReportLink.DataProviderPresent then
    ReportLink.RebuildReport;
end;

procedure TdxPSCustomPreviewWindow.UpdateCaption;
begin
  if ComponentPrinter <> nil then
    Caption := ComponentPrinter.PreviewCaption;
end;

procedure TdxPSCustomPreviewWindow.UpdateControls;
begin
  UpdateCaption;
  UpdateExplorerContextCommands;
end;

{$IFDEF OLEDRAGANDDROP}

const
  DropEffects: array[Boolean] of Longint = (DROPEFFECT_NONE, DROPEFFECT_COPY);

function TdxPSCustomPreviewWindow.IDropTarget_DragEnter(const DataObj: IDataObject;
  grfKeyState: Longint; Pt: TPoint; var dwEffect: Longint): HRESULT;
var
  Format: TFormatETC;
  Medium: TSTGMedium;
  BufferSize: DWORD;
  Buffer:  PChar ;
begin
  FillChar(Format, SizeOf(Format), 0);
  with Format do
  begin
    cfFormat := CF_HDROP;
    dwAspect := DVASPECT_CONTENT;
    lindex := -1;
    tymed := TYMED_HGLOBAL;
  end;
  DataObj._AddRef;
  try
    FDraggedFileName := '';
    if (DataObj.GetData(Format, Medium) = S_OK) and (DragQueryFile(Medium.hGlobal, $FFFFFFFF, nil, 0) <> 0) then
    try
      BufferSize := DragQueryFile(Medium.hGlobal, 0, nil, 0);
      Buffer := StrAlloc(BufferSize + 1);
      try
        if BufferSize = DragQueryFile(Medium.hGlobal, 0, Buffer, BufferSize) then
          FDraggedFileName := StrPas(Buffer);
      finally
       StrDispose(Buffer);
      end;
    finally
      if Medium.unkForRelease = nil then ReleaseSTGMedium(Medium);
    end;
  finally
    DataObj._Release;
  end;

  dwEffect := DropEffects[CanDrop and DoCanAccept];
  if dwEffect = DROPEFFECT_NONE then FDraggedFileName := '';

  Result := S_OK;
end;

function TdxPSCustomPreviewWindow.IDropTarget_DragOver(
  grfKeyState: Longint; Pt: TPoint; var dwEffect: Longint): HRESULT;
begin
  dwEffect := DropEffects[CanDrop];
  Result := S_OK;
end;

function TdxPSCustomPreviewWindow.IDropTarget_DragLeave: HRESULT;
begin
  Result := S_OK;
end;

function TdxPSCustomPreviewWindow.IDropTarget_Drop(const DataObj: IDataObject;
  grfKeyState: Longint; Pt: TPoint; var dwEffect: Longint): HRESULT;
begin
  dwEffect := DropEffects[CanDrop];
  if dwEffect = DROPEFFECT_COPY then DoDrop;
  Result := S_OK;
end;

{$ENDIF}

function TdxPSCustomPreviewWindow.GetExplorerTree: TCustomdxPSExplorerTreeContainer;
begin
  Result := nil;
end;

function TdxPSCustomPreviewWindow.GetHFEditPart: TdxPageTitlePart;
begin
  Result := tpLeft;
end;

function TdxPSCustomPreviewWindow.GetState: TdxPSPreviewState;
begin
  Result := prsNone;
end;

procedure TdxPSCustomPreviewWindow.BeginUpdate;
begin
  // do nothing
end;

procedure TdxPSCustomPreviewWindow.CancelUpdate;
begin
  // do nothing
end;

procedure TdxPSCustomPreviewWindow.EndUpdate;
begin
  // do nothing
end;

function TdxPSCustomPreviewWindow.Locked: Boolean;
begin
  Result := False;
end;

procedure TdxPSCustomPreviewWindow.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = ComponentPrinter) then
    ComponentPrinter := nil;
end;

procedure TdxPSCustomPreviewWindow.PaintPage(Sender: TObject; ACanvas: TCanvas; ARect: TRect; APageIndex: Integer);
begin
  ComponentPrinter.PaintPageCore(ACanvas, APageIndex, ZoomFactor, ARect, ARect);
end;

procedure TdxPSCustomPreviewWindow.PaintThumbnailPage(
  Sender: TObject; ACanvas: TCanvas; ARect: TRect; APageIndex: Integer);
begin
  ACanvas.Font := Options.ThumbnailsOptions.Font;
  ComponentPrinter.PaintThumbnailPage(ACanvas, APageIndex,
    Options.ThumbnailsOptions.ShowPageNumbers, Options.ThumbnailsOptions.Size, ARect, ARect);
end;

{$IFDEF OLEDRAGANDDROP}

function TdxPSCustomPreviewWindow.CanDrop: Boolean;
begin
  Result := DraggedFileName <> '';
end;

function TdxPSCustomPreviewWindow.DoCanAccept: Boolean;
begin
  Result := (ReportLink <> nil) and ReportLink.CanLoadData and ReportLink.PossibleDataStorage(DraggedFileName, False);
end;

procedure TdxPSCustomPreviewWindow.DoDrop;
begin
  if ReportLink <> nil then
  begin
    ReportLink.DataSource := rldsExternalStorage;
    ReportLink.StorageName := DraggedFileName;
    RebuildReport;
    UpdateControls;
    Application.BringToFront;
  end;
end;

{$ENDIF}

procedure TdxPSCustomPreviewWindow.OptionsChangeHandler(Sender: TObject);
begin
  OptionsChanged;
end;

function TdxPSCustomPreviewWindow.GetReportLink: TBasedxReportLink;
begin
  if ComponentPrinter <> nil then
    Result := ComponentPrinter.CurrentLink
  else
    Result := nil;
end;

procedure TdxPSCustomPreviewWindow.SetComponentPrinter(AValue: TCustomdxComponentPrinter);
begin
  if AValue <> FComponentPrinter then
  begin
    BeforeComponentPrinterChanged;
    try
      FComponentPrinter := AValue;
      CheckRebuildReport;
    finally
      AfterComponentPrinterChanged;
    end;
  end;
end;

procedure TdxPSCustomPreviewWindow.SetOptions(AValue: TdxPreviewWindowOptions);
begin
  FOptions.Assign(AValue);
end;

{ TdxPreviewOptions }

constructor TdxPreviewOptions.Create;
begin
  inherited Create;
  FIcon := TIcon.Create;
  FIcon.OnChange := IconChanged;
  FSavePosition := True;
  FWindowState := wsNormal;
end;

destructor TdxPreviewOptions.Destroy;
begin
  FreeAndNil(FDefaultIcon);
  FreeAndNil(FIcon);
  inherited Destroy;
end;

function TdxPreviewOptions.DefaultCaption: string;
begin
  Result := cxGetResourceString(@sdxPrintPreview);
end;

function TdxPreviewOptions.DefaultIcon: TIcon;
begin
  if FDefaultIcon = nil then
  begin
    FDefaultIcon := TIcon.Create;
    InitializeDefaultIcon(FDefaultIcon);
  end;
  Result := FDefaultIcon;
end;

function TdxPreviewOptions.DefaultRect: TRect;
begin
  Result := dxPSUtl.GetDesktopWorkArea;
end;

procedure TdxPreviewOptions.RestoreOriginalIcon;
begin
  FIsIconAssigned := False;
  Changed;
end;

procedure TdxPreviewOptions.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  Rect := Bounds(ALeft, ATop, AWidth, AHeight);
end;

procedure TdxPreviewOptions.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineBinaryProperty('PreviewBoundsRect', ReadBoundsRect, WriteBoundsRect, IsBoundsStored);
  Filer.DefineProperty('IsCaptionAssigned', ReadIsCaptionAssigned, WriteIsCaptionAssigned, FIsCaptionAssigned and (Caption = ''));
  Filer.DefineProperty('IsIconAssigned', ReadIsIconAssigned, WriteIsIconAssigned, FIsIconAssigned and Icon.Empty);
end;

procedure TdxPreviewOptions.DoAssign(Source: TdxBaseObject);
begin
  inherited DoAssign(Source);

  Caption := TdxPreviewOptions(Source).Caption;
  HelpContext := TdxPreviewOptions(Source).HelpContext;
  HelpFile := TdxPreviewOptions(Source).HelpFile;
  Icon := TdxPreviewOptions(Source).Icon;
  Rect := TdxPreviewOptions(Source).Rect;
  SavePosition := TdxPreviewOptions(Source).SavePosition;
  WindowState := TdxPreviewOptions(Source).WindowState;

  FIsBoundsAssigned := TdxPreviewOptions(Source).FIsBoundsAssigned;
  FIsCaptionAssigned := TdxPreviewOptions(Source).FIsCaptionAssigned;
  FIsIconAssigned := TdxPreviewOptions(Source).FIsIconAssigned;

  Changed;
end;

procedure TdxPreviewOptions.DoRestoreDefaults;
begin
  inherited DoRestoreDefaults;

  SavePosition := True;
  WindowState := wsNormal;

  FIsBoundsAssigned := False;
  FIsCaptionAssigned := False;
  FIsIconAssigned := False;

  Changed;
end;

function TdxPreviewOptions.GetIsIconAssigned: Boolean;
begin
  Result := not dxPSUtl.dxAreGraphicsEqual(FIcon, DefaultIcon);
end;

procedure TdxPreviewOptions.IconChanged(Sender: TObject);
begin
  FIsIconAssigned := True;
  Changed;
end;

procedure TdxPreviewOptions.InitializeDefaultIcon(AnIcon: TIcon);
begin
  dxLoadIconFromResource(AnIcon, IDB_DXPSPREVIEW);
end;

function TdxPreviewOptions.GetCaption: string;
begin
  if FIsCaptionAssigned then
    Result := FCaption
  else
    Result := DefaultCaption;
end;

function TdxPreviewOptions.GetHelpFile: string;
begin
  Result := dxPSEngine.HelpFile;
end;

function TdxPreviewOptions.GetIcon: TIcon;
begin
  if FIsIconAssigned then
    Result := FIcon
  else
    Result := DefaultIcon;
end;

function TdxPreviewOptions.GetPosition(Index: Integer): Integer;
begin
  case Index of
    0: Result := Rect.Height;
    1: Result := Rect.Left;
    2: Result := Rect.Top;
  else
    Result := Rect.Width;
  end;
end;

function TdxPreviewOptions.GetRect: TRect;
begin
  if FIsBoundsAssigned then
    Result := FRect
  else
    Result := DefaultRect;
end;

function TdxPreviewOptions.IsBoundsStored: Boolean;
begin
  Result := FIsBoundsAssigned and not EqualRect(FRect, DefaultRect);
end;

function TdxPreviewOptions.IsCaptionStored: Boolean;
begin
  Result := FIsCaptionAssigned and (FCaption <> DefaultCaption);
end;

function TdxPreviewOptions.IsIconStored: Boolean;
begin
  Result := FIsIconAssigned and not dxPSUtl.dxAreGraphicsEqual(FIcon, DefaultIcon);
end;

procedure TdxPreviewOptions.SetCaption(const Value: string);
begin
  if Caption <> Value then
  begin
    FCaption := Value;
    FIsCaptionAssigned := True;
    Changed;
  end;
end;

procedure TdxPreviewOptions.SetHelpContext(Value: THelpContext);
begin
  if HelpContext <> Value then
  begin
    FHelpContext := Value;
    Changed;
  end;
end;

procedure TdxPreviewOptions.SetHelpFile(const Value: string);
begin
  dxPSEngine.HelpFile := Value;
  Changed;
end;

procedure TdxPreviewOptions.SetIcon(Value: TIcon);
begin
  FIcon.Assign(Value);
end;

procedure TdxPreviewOptions.SetPosition(Index: Integer; Value: Integer);
var
  R: TRect;
begin
  R := Rect;
  case Index of
    0: R.Bottom := R.Top + Value;
    1: R.Left := Value;
    2: R.Top := Value;
    3: R.Right := R.Left + Value;
  end;
  Rect := R;
end;

procedure TdxPreviewOptions.SetRect(Value: TRect);
begin
  Value := cxRectAdjust(Value);
  if not EqualRect(FRect, Value) then
  begin
    FRect := Value;
    FIsBoundsAssigned := True;
    Changed;
  end;
end;

procedure TdxPreviewOptions.SetWindowState(Value: TWindowState);
begin
  if FWindowState <> Value then
  begin
    FWindowState := Value;
    Changed;
  end;
end;

procedure TdxPreviewOptions.ReadBoundsRect(Stream: TStream);
var
  R: TRect;
begin
  Stream.ReadBuffer(R, SizeOf(R));
  Rect := R;
end;

procedure TdxPreviewOptions.ReadIsCaptionAssigned(Reader: TReader);
begin
  FIsCaptionAssigned := Reader.ReadBoolean;
end;

procedure TdxPreviewOptions.ReadIsIconAssigned(Reader: TReader);
begin
  FIsIconAssigned := Reader.ReadBoolean;
end;

procedure TdxPreviewOptions.WriteBoundsRect(Stream: TStream);
begin
  Stream.WriteBuffer(FRect, SizeOf(TRect));
end;

procedure TdxPreviewOptions.WriteIsCaptionAssigned(Writer: TWriter);
begin
  Writer.WriteBoolean(FIsCaptionAssigned);
end;

procedure TdxPreviewOptions.WriteIsIconAssigned(Writer: TWriter);
begin
  Writer.WriteBoolean(FIsIconAssigned);
end;

{ TdxPSComponentPrinterExplorerChangeNotifier }

constructor TdxPSComponentPrinterExplorerChangeNotifier.Create(AComponentPrinter: TCustomdxComponentPrinter);
begin
  Assert(AComponentPrinter <> nil);
  FComponentPrinter := AComponentPrinter;
  inherited Create(nil);
end;

procedure TdxPSComponentPrinterExplorerChangeNotifier.ItemDataUnloaded(AnItem: TdxPSExplorerItem);
begin
  if (AnItem.Explorer = Explorer) and (ComponentPrinter.CurrentLink <> nil) and not ComponentPrinter.IsDestroying then
  begin
    ComponentPrinter.CurrentLink.DataSource := rldsComponent;
    ComponentPrinter.CurrentLink.FinalizeDataStream;
  end;
end;

{ TdxPSPrintPageRangeInfo }

constructor TdxPSPrintPageRangeInfo.Create;
begin
  inherited Create;
  FPageRanges := prAll;
end;

destructor TdxPSPrintPageRangeInfo.Destroy;
begin
  SetLength(FPageIndexes, 0);
  inherited Destroy;
end;

procedure TdxPSPrintPageRangeInfo.Assign(AInfo: TdxPSPrintPageRangeInfo);
begin
  PageRanges := AInfo.PageRanges;
  PageIndexesAsString := AInfo.PageIndexesAsString;
end;

function TdxPSPrintPageRangeInfo.GetPageIndex(Index: Integer): Integer;
begin
  if (Index >= 0) and (Index < PageIndexCount) then
    Result := FPageIndexes[Index]
  else
    Result := -1;
end;

function TdxPSPrintPageRangeInfo.GetPageIndexCount: Integer;
begin
  Result := Length(FPageIndexes);
end;

function TdxPSPrintPageRangeInfo.GetPageIndexesAsString: string;
begin
  Result := EncodePageIndexes(FPageIndexes)
end;

procedure TdxPSPrintPageRangeInfo.SetPageIndexesAsString(const AValue: string);
begin
  if PageIndexesAsString <> AValue then
    DecodePageIndexes(AValue, FPageIndexes);
end;

{ TdxComponentPrinterListeners }

procedure TdxComponentPrinterListeners.BeforeDestroyReport(AReportLink: TBasedxReportLink);
var
  AIntf: IdxComponentPrinterListener;
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    if Supports(Items[I], IdxComponentPrinterListener, AIntf) then
      AIntf.BeforeDestroyReport(AReportLink);
  end;
end;

procedure TdxComponentPrinterListeners.PageParamsChanged(AReportLink: TBasedxReportLink);
var
  AIntf: IdxComponentPrinterListener;
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    if Supports(Items[I], IdxComponentPrinterListener, AIntf) then
      AIntf.PageParamsChanged(AReportLink);
  end;
end;

procedure TdxComponentPrinterListeners.CurrentLinkChanged(AReportLink: TBasedxReportLink);
var
  AIntf: IdxComponentPrinterListener;
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    if Supports(Items[I], IdxComponentPrinterListener, AIntf) then
      AIntf.CurrentLinkChanged(AReportLink);
  end;
end;

procedure TdxComponentPrinterListeners.ExplorerChanged(AReportLink: TBasedxReportLink);
var
  AIntf: IdxComponentPrinterListener;
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    if Supports(Items[I], IdxComponentPrinterListener, AIntf) then
      AIntf.ExplorerChanged(AReportLink);
  end;
end;

procedure TdxComponentPrinterListeners.LayoutChanged(AReportLink: TBasedxReportLink);
var
  AIntf: IdxComponentPrinterListener;
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    if Supports(Items[I], IdxComponentPrinterListener, AIntf) then
      AIntf.LayoutChanged(AReportLink);
  end;
end;

procedure TdxComponentPrinterListeners.PrepareBuildReport(AReportLink: TBasedxReportLink);
var
  AIntf: IdxComponentPrinterListener;
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    if Supports(Items[I], IdxComponentPrinterListener, AIntf) then
      AIntf.PrepareBuildReport(AReportLink);
  end;
end;

procedure TdxComponentPrinterListeners.UnprepareBuildReport(AReportLink: TBasedxReportLink);
var
  AIntf: IdxComponentPrinterListener;
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    if Supports(Items[I], IdxComponentPrinterListener, AIntf) then
      AIntf.UnprepareBuildReport(AReportLink);
  end;
end;

{ TCustomdxComponentPrinter }

constructor TCustomdxComponentPrinter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if AOwner <> nil then
    AOwner.FreeNotification(Self);
  FAutoUpdateDateTime := True;
  FBeepAfterLongOperations := True;
  FCurrentLink := nil;
  FDateFormat := 0;
  FExplorerChangeNotifier := TdxPSComponentPrinterExplorerChangeNotifier.Create(Self);
  FInternalStreaming := False;
  FLongOperationTime := 5000;
  FPageNumberFormat := pnfNumeral;
  FReportLinkDesigner := nil;
  FPreviewWindowDesigner := nil;
  FPrintTitle := '';
  FState := [];
  FPreviewOptions := TdxPreviewOptions.Create;
  FPreviewOptions.OnChanged := PreviewOptionsChangeHandler;
  FListeners := TdxComponentPrinterListeners.Create;

  FPrintFileList := TStringList.Create;
  FOptions := dxDefaultCPOptions;
  FReportLinks := TList.Create;
  FTimeFormat := 0;
  FHFTextEntryChooseSubscriber := TdxHFTextEntryChooseSubscriber.Create([TdxHFTextEntryChooseEvent]);
  TdxHFTextEntryChooseSubscriber(FHFTextEntryChooseSubscriber).OnHFTextEntryChoose := OnHFTextEntryChosen;
  FWindowHandle := dxPSUtl.dxAllocatehWnd(WndProc);
  FVersion := 0;
end;

destructor TCustomdxComponentPrinter.Destroy;
begin
  dxPSUtl.dxDeallocatehWnd(FWindowHandle);
  FreeAndNil(FHFTextEntryChooseSubscriber);
  FreeAndNil(FPrintFileList);
  FreeAndNil(FPreviewWindowDesigner);
  FreeAndNil(FReportLinkDesigner);
  FreeAndNil(FPreviewOptions);
  FreeAndNil(FExplorerChangeNotifier);
  DeleteAllLinks;
  FreeAndNil(FReportLinks);
  FreeAndNil(FListeners);
  inherited Destroy;
end;

procedure TCustomdxComponentPrinter.AfterConstruction;
begin
  inherited AfterConstruction;
  if Assigned(dxComponentPrintersList) then
    dxComponentPrintersList.Add(Self);
end;

procedure TCustomdxComponentPrinter.BeforeDestruction;
begin
  if not IsDesigning then
    DestroyPreviewWindowForm;
  inherited BeforeDestruction;
  if Assigned(dxComponentPrintersList) then
    dxComponentPrintersList.Remove(Self);
end;

procedure TCustomdxComponentPrinter.ChangeScale(M, D: Integer);
var
  I: Integer;
begin
  inherited;
  for I := 0 to LinkCount - 1 do
    ReportLink[I].ScaleForPPI(ScaleFactor.TargetDPI);
end;

procedure TCustomdxComponentPrinter.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  I: Integer;
  ReportLink: TBasedxReportLink;
begin
  if not FInternalStreaming then
    for I := 0 to LinkCount - 1 do
    begin
      ReportLink := Self.ReportLink[I];
      if Root = ReportLink.Owner then
        Proc(ReportLink);
    end;
end;

procedure TCustomdxComponentPrinter.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = Explorer then
      Explorer := nil;
    if AComponent = ExplorerStubLink then
      ExplorerStubLink := nil;
    if AComponent = FPreviewWindowForm then
    begin
      Exclude(FState, cpsPreviewing);
      DoAfterPreview(CurrentLink);
      FPreviewWindowForm := nil;
      FPreviewWindow := nil;
    end;
  end;
end;

procedure TCustomdxComponentPrinter.SetChildOrder(Child: TComponent; Order: Integer);
begin
  inherited;
  if FReportLinks.IndexOf(Child) > -1 then
    (Child as TBasedxReportLink).Index := Order;
end;

procedure TCustomdxComponentPrinter.SetName(const NewName: TComponentName);
var
  AName: string;
  OldName: string;
  P, I: Integer;
  Link: TBasedxReportLink;
begin
  OldName := Name;
  inherited SetName(NewName);
  if IsDesigning and (LinkCount > 0) then
  try
    if ReportLinkDesigner <> nil then
      ReportLinkDesigner.BeginUpdate;
    try
      for I := 0 to LinkCount - 1 do
      begin
        Link := ReportLink[I];
        P := Pos(OldName, Link.Name);
        if P = 0 then
          AName := Name + Link.Name
        else
          AName := Copy(Link.Name, 1, P - 1) + Name + Copy(Link.Name, P + Length(OldName), Length(Link.Name) - P - Length(OldName) + 1);
        Link.Name := AName;
      end;
    finally
      if ReportLinkDesigner <> nil then
        ReportLinkDesigner.EndUpdate;
    end;
  except
    on EComponentError do ; {Ignore rename errors }
  end;
end;

procedure TCustomdxComponentPrinter.SetExplorer(Value: TCustomdxPSExplorer);
begin
  if FExplorer <> Value then
  begin
    if FExplorer <> nil then
    begin
      FExplorer.RemoveFreeNotification(Self);
      FExplorer := nil;
    end;
    if Value <> nil then
    begin
      FExplorer := Value;
      Explorer.FreeNotification(Self);
    end;
    ExplorerChangeNotifier.Explorer := Explorer;
    Listeners.ExplorerChanged(CurrentLink);
  end;
end;

procedure TCustomdxComponentPrinter.SetExplorerStubLink(Value: TBasedxReportLink);
begin
  if (FExplorerStubLink <> Value) and ((Value = nil) or Value.CanBeUsedAsStub) then
  begin
    if FExplorerStubLink <> nil then
    begin
      FExplorerStubLink.RemoveFreeNotification(Self);
      FExplorerStubLink := nil;
    end;
    if Value <> nil then
    begin
      FExplorerStubLink := Value;
      FExplorerStubLink.FreeNotification(Self);
    end;
    Listeners.ExplorerChanged(CurrentLink);
  end;
end;

procedure TCustomdxComponentPrinter.SetLongOperationTime(Value: Integer);
begin
  FLongOperationTime := Max(0, Value);
end;

procedure TCustomdxComponentPrinter.SetPreviewOptions(Value: TdxPreviewOptions);
begin
  PreviewOptions.Assign(Value);
end;

function TCustomdxComponentPrinter.IsCustomPrintDlgData: Boolean;
begin
  Result := Assigned(FOnInitializePrintDlgData);
end;

function TCustomdxComponentPrinter.IsForegroundPreviewWindow: Boolean;
begin
  Result := (cpsPreviewing in State) and (PreviewWindow <> nil) and (GetForegroundWindow = PreviewWindow.Handle);
end;

function TCustomdxComponentPrinter.IsGenerateReportProgressEvent: Boolean;
begin
  Result := not IsDesigning and not IsLoading and (cpoGenerateReportProgressEvent in Options);
end;

function TCustomdxComponentPrinter.IsRebuildBeforeOutput(AForceRebuild: Boolean): Boolean;
begin
  Result := AForceRebuild or IsDesigning;
  if CurrentLink <> nil then
    Result := Result or CurrentLink.RebuildNeeded or (CurrentLink.DataSource = rldsExternalStorage);
end;

function TCustomdxComponentPrinter.IsRebuildBeforePreview: Boolean;
begin
  Result := IsRebuildBeforeOutput(cpoAutoRebuildBeforePreview in Options);
end;

function TCustomdxComponentPrinter.IsRebuildBeforePrint: Boolean;
begin
  Result := IsRebuildBeforeOutput(cpoAutoRebuildBeforePrint in Options);
end;

function TCustomdxComponentPrinter.IsShowHourGlass: Boolean;
begin
  Result := cpoShowHourGlass in Options;
end;

procedure TCustomdxComponentPrinter.PaintPageCore(ACanvas: TCanvas; APageIndex: Integer;
  AZoomFactor: Integer; const APageBounds, AContentBounds: TRect; AReportLink: TBasedxReportLink = nil);
begin
  AReportLink := CheckLink(AReportLink);
  if AReportLink <> nil then
    AReportLink.PaintPage(ACanvas, APageBounds, APageIndex, APageIndex, AZoomFactor);
end;

procedure TCustomdxComponentPrinter.PaintThumbnailPage(ACanvas: TCanvas;
  APageIndex: Integer; AShowPageNumbers: Boolean; AThumbnailSize: TdxPSThumbnailsSize;
  const APageBounds, AContentBounds: TRect; AReportLink: TBasedxReportLink = nil);
var
  AFontSize: Integer;
begin
  AReportLink := CheckLink(AReportLink);
  if AReportLink <> nil then
  begin
    AReportLink.PaintPage(ACanvas, APageBounds, APageIndex, APageIndex, dxThumbnailsZoomFactors[AThumbnailSize]);
    if AShowPageNumbers then
    begin
      AFontSize := ACanvas.Font.Size;
      try
        if AThumbnailSize = tsSmall then
          ACanvas.Font.Size := ACanvas.Font.Size div 2;
        dxPSUtl.DrawBlendedText(ACanvas, APageBounds, IntToStr(AReportLink.Renderer.PreparedPageIndex(APageIndex) + 1), ACanvas.Font);
      finally
        ACanvas.Font.Size := AFontSize;
      end;
    end;
  end;
end;

function TCustomdxComponentPrinter.GetCurrentLinkIndex: Integer;
begin
  if CurrentLink <> nil then
    Result := CurrentLink.Index
  else
    Result := -1;
end;

function TCustomdxComponentPrinter.GetExplorerRealStubLink: TBasedxReportLink;
begin
  Result := FExplorerStubLink;
  if Result = nil then
    Result := AddEmptyLink(TBasedxReportLink);
end;

function TCustomdxComponentPrinter.GetIsExplorerMode: Boolean;
begin
  Result := cpsExplore in State;
end;

function TCustomdxComponentPrinter.GetLinkCount: Integer;
begin
  Result := FReportLinks.Count;
end;

function TCustomdxComponentPrinter.GetPreviewCaption: string;
begin
  if cpsExplore in State then
    Result := cxGetResourceString(@sdxReportExplorer)
  else
    Result := PreviewOptions.Caption;

  if (Explorer <> nil) and (Explorer.LoadedItem <> nil) then
    Result := Result + ' ' + sdxDocumentCaptionSeparator + ' ' + Explorer.LoadedItem.FormCaption
  else
    if (CurrentLink <> nil) and (CurrentLink.DataSource = rldsExternalStorage) and (CurrentLink.StorageName <> '') then
      Result := Result + ' ' + sdxDocumentCaptionSeparator + ' ' + CurrentLink.StorageName;
end;

function TCustomdxComponentPrinter.GetReportLink(Index: Integer): TBasedxReportLink;
begin
  Result := TBasedxReportLink(FReportLinks[Index]);
end;

procedure TCustomdxComponentPrinter.SetAbortBuilding(Value: Boolean);
begin
  FAbortBuilding := Value;
end;

procedure TCustomdxComponentPrinter.SetAbortPrinting(Value: Boolean);
begin
  FAbortPrinting := Value;
end;

procedure TCustomdxComponentPrinter.SetAutoUpdateDateTime(Value: Boolean);
begin
  if FAutoUpdateDateTime <> Value then
    FAutoUpdateDateTime := Value;
end;

procedure TCustomdxComponentPrinter.SetDateFormat(Value: Integer);
begin
  Value := Max(Value, 0);
  Value := Min(Value, dxPgsDlg.DateFormats.Count - 1);
  FDateFormat := Value;
end;

procedure TCustomdxComponentPrinter.SetCurrentLink(Value: TBasedxReportLink);
begin
  if (CurrentLink <> Value) and (IndexOfLink(Value) > -1) then
  begin
    if PreviewExists then
      DestroyPreviewWindowForm;
    FCurrentLink := Value;
    DoChangeCurrentLink;
    FormatChanged(FCurrentLink);
    if FCurrentLink <> nil then
      FCurrentLink.PrinterPage.ApplyToPrintDevice;
    DesignerUpdate(Value);
  end;
end;

procedure TCustomdxComponentPrinter.SetCurrentLinkIndex(Value: Integer);
begin
  Value := Max(Value, 0);
  Value := Min(Value, LinkCount - 1);
  if Value > -1 then
    CurrentLink := ReportLink[Value];
end;

procedure TCustomdxComponentPrinter.SetPageNumberFormat(Value: TdxPageNumberFormat);
begin
  FPageNumberFormat := Value;
end;

procedure TCustomdxComponentPrinter.SetPrintFileList(Value: TStrings);
begin
  FPrintFileList.Assign(Value);
end;

procedure TCustomdxComponentPrinter.SetReportLink(Index: Integer; Value: TBasedxReportLink);
begin
  ReportLink[Index].Assign(Value);
end;

procedure TCustomdxComponentPrinter.SetTimeFormat(Value: Integer);
begin
  Value := Max(Value, 0);
  Value := Min(Value, dxPgsDlg.TimeFormats.Count - 1);
  FTimeFormat := Value;
end;

function TCustomdxComponentPrinter.BeginPrintPages(const Source: string; out APageIndexes: TdxPSPageIndexes): Boolean;
begin
  Result := DecodePageIndexes(Source, APageIndexes);
end;

procedure TCustomdxComponentPrinter.EndPrintPages(var APageIndexes: TIntegers);
begin
  SetLength(APageIndexes, 0);
end;

procedure TCustomdxComponentPrinter.CreatePreviewWindowForm(AReportLink: TBasedxReportLink);
var
  AIntf: IdxPSPreviewWindowDialog;
begin
  FPreviewWindow := AReportLink.CreatePreviewWindowForm;
  FPreviewWindowForm := GetParentForm(FPreviewWindow) as TForm;
  try
    FPreviewWindowForm.FreeNotification(Self);
    FPreviewWindow.BeginUpdate;
    try
      FPreviewWindow.ComponentPrinter := Self;
      SetupPreviewWindowForm;
    finally
      FPreviewWindow.EndUpdate;
    end;
    if Supports(FPreviewWindowForm, IdxPSPreviewWindowDialog, AIntf) then
      AIntf.Initialize;
  except
    FreeAndNil(FPreviewWindowForm);
    FPreviewWindow := nil;
    raise;
  end;
end;

procedure TCustomdxComponentPrinter.DestroyPreviewWindowForm;
begin
  if cpsPrinting in State then
    AbortPrinting := True;
  ReleasePreviewWindowForm;
end;

procedure TCustomdxComponentPrinter.ReleasePreviewWindowForm;
var
  AIniFile: TCustomIniFile;
begin
  if PreviewWindowForm <> nil then
  try
    if dxPSStoringManager.BeginStoring(AIniFile) then
    try
      if dxPSEngine.SaveFormsPosition then
        dxPSFormSavePositionToIniFile(PreviewWindowForm, AIniFile, dxPSFormGetActualSectionName(PreviewWindowForm));
    finally
      dxPSStoringManager.EndStoring(AIniFile);
    end;
  finally
    FPreviewWindowForm.Free;
  end;
end;

procedure TCustomdxComponentPrinter.SetupPreviewWindowForm;

  function GetCaption: string;
  begin
    if IsExplorerMode then
      Result := cxGetResourceString(@sdxReportExplorer)
    else
      Result := PreviewOptions.Caption;
  end;

var
  AIniFile: TCustomIniFile;
  AIsPositionAssigned: Boolean;
begin
  if not IsDesigning then
  begin
    PreviewWindowForm.PopupMode := pmAuto;
    PreviewWindowForm.Caption := GetCaption;
    PreviewWindowForm.Icon := PreviewOptions.Icon;
    PreviewWindowForm.HelpContext := PreviewOptions.HelpContext;
    PreviewWindowForm.HelpFile := PreviewOptions.HelpFile;

    AIsPositionAssigned := False;
    if dxPSEngine.SaveFormsPosition then
    begin
      if dxPSStoringManager.BeginStoring(AIniFile) then
      try
        AIsPositionAssigned := dxPSFormLoadPositionFromIniFile(
          PreviewWindowForm, AIniFile, dxPSFormGetActualSectionName(PreviewWindowForm));
      finally
        dxPSStoringManager.EndStoring(AIniFile);
      end;
    end;

    if not AIsPositionAssigned then
    begin
      PreviewWindowForm.Position := poDesigned;
      PreviewWindowForm.BoundsRect := PreviewOptions.Rect;
      PreviewWindowForm.WindowState := PreviewOptions.WindowState;
    end;
  end;
  PreviewWindow.Options.Assign(PreviewOptions);
end;

procedure TCustomdxComponentPrinter.ShowPreviewWindowForm(AModal: Boolean);
const
  Flags: array[Boolean] of UINT = (SW_SHOW, SW_RESTORE or SW_SHOWNORMAL);
var
  AWindowPlacement: TWindowPlacement;
begin
  if PreviewWindowForm.HandleAllocated and PreviewWindowForm.Visible then
  begin
    AWindowPlacement.Length := SizeOf(AWindowPlacement);
    GetWindowPlacement(PreviewWindowForm.Handle, @AWindowPlacement);
    ShowWindow(PreviewWindowForm.Handle, Flags[AWindowPlacement.ShowCmd = SW_SHOWMINIMIZED]);
    SetForegroundWindow(PreviewWindowForm.Handle);
  end
  else
    if AModal then
    begin
      PreviewWindowForm.BorderIcons := PreviewWindowForm.BorderIcons - [biMinimize];
      PreviewWindowForm.ShowModal;
    end
    else
      PreviewWindowForm.Show;
end;

procedure TCustomdxComponentPrinter.FinalizeDefaultPrintDlgData(
  AReportLink: TBasedxReportLink; var APrintDlgData: TdxPrintDlgData);
begin
  FreeAndNil(APrintDlgData.DialogData.FileList);
  DoFinalizePrintDlgData(AReportLink, APrintDlgData);
end;

procedure TCustomdxComponentPrinter.InitDevModeFromPrinterPageSettings(APrinterPage: TdxPrinterPage);
begin
  if (dxPrintDevice = nil) or (dxPrintDevice.DeviceMode = nil) then Exit;
  dxPrintDevice.Orientation := APrinterPage.Orientation;
  dxPrintDevice.DeviceMode^.dmColor := Byte(not APrinterPage.GrayShading) + 1;
end;

function TCustomdxComponentPrinter.PrintDialog(AReportLink: TBasedxReportLink; var APrintDlgData: TdxPrintDlgData): Boolean;
var
  PreviewBtnClicked: Boolean;
begin
  Include(FState, cpsPrintDialog);
  try
    if not (rlcPageSetup in AReportLink.Capabilities) then
      APrintDlgData.ButtonsVisible := APrintDlgData.ButtonsVisible - [pdbPageSetup];

    Result := AReportLink.PrintDialog(APrintDlgData);
    PreviewBtnClicked := APrintDlgData.PreviewBtnClicked;
    if Result then
    begin
      FSavePrintToFile := APrintDlgData.DialogData.PrintToFile;
      PrintFileList := APrintDlgData.DialogData.FileList;
      AReportLink.PrinterPage.InitFromPrintDevice;
      if not PreviewBtnClicked then
        with APrintDlgData.DialogData do
        begin
          if PrintToFile then
            dxPrintDevice.FileName := FileName
          else
            dxPrintDevice.FileName := '';

          FPrintAll := PageRanges = prAll;
          if PageRanges in [prCurrent, prRange] then
          begin
            if PageRanges = prCurrent then
              Pages := IntToStr(AReportLink.CurrentPage);
            Result := PrintPagesAsStringEx(Pages, PageNums, Copies, Collate, AReportLink);
          end
          else // prAll
            PrintEx(PageNums, Copies, Collate, AReportLink);
        end;
    end;
  finally
    FPrintAll := False;
    Exclude(FState, cpsPrintDialog);
  end;

  if PreviewBtnClicked then
    AReportLink.Preview(True);
end;

function TCustomdxComponentPrinter.PrintPagesAsStringEx(const APages: string;
  APageNums: TdxPageNumbers; ACopyCount: Integer; ACollate: Boolean;
  AReportLink: TBasedxReportLink = nil): Boolean;
var
  PageIndexes: TdxPSPageIndexes;
begin
  Result := BeginPrintPages(APages, PageIndexes);
  if Result then
  try
    PrintPagesEx(PageIndexes, APageNums, ACopyCount, ACollate, AReportLink);
  finally
    EndPrintPages(PageIndexes);
  end
end;

procedure TCustomdxComponentPrinter.PrnDlgPageSetup(
  Sender: TObject; var ADone: Boolean; APreviewBtnClicked, APrintBtnClicked: PBoolean);
var
  ShowPreviewBtn, ShowPrintBtn, PreviewBtnClicked, PrintBtnClicked: Boolean;
begin
  ShowPreviewBtn := APreviewBtnClicked <> nil;
  ShowPrintBtn := APrintBtnClicked <> nil;
  ADone := (CurrentLink <> nil) and PageSetupEx(0, ShowPreviewBtn, ShowPrintBtn, PreviewBtnClicked, PrintBtnClicked, CurrentLink);
  if (ADone or PreviewBtnClicked) and CurrentLink.HasExternalListeners then
  begin
    CurrentLink.CalculateRenderInfos;
    Listeners.PageParamsChanged(CurrentLink);
  end;
  if ShowPreviewBtn then
    APreviewBtnClicked^ := PreviewBtnClicked;
  if ShowPrintBtn then
    APrintBtnClicked^ := PrintBtnClicked;
end;

procedure TCustomdxComponentPrinter.RaiseBuildingEvent(
  AReportLink: TBasedxReportLink; const APercentCompleted: Double; AStage: TdxPSBuildStage);
var
  AEvent: TdxEvent;
begin
  if CurrentCompositionByLink(AReportLink) = nil then
  begin
    AEvent := TdxPSBuildEvent.Create(Self, AReportLink, APercentCompleted, AStage);
    dxPSProcessEvent(AEvent)
  end;
end;

procedure TCustomdxComponentPrinter.RaisePrintingEvent(AReportLink: TBasedxReportLink;
  APageIndex, APageCount: Integer; AStage: TdxPSPrintStage);
var
  Event: TdxEvent;
begin
  Event := TdxPSPrintEvent.Create(Self, AReportLink, APageIndex, APageCount, AStage);
  dxPSProcessEvent(Event);
end;

procedure TCustomdxComponentPrinter.ActivateLink(AReportLink: TBasedxReportLink);

  function IsCompositionActivatation(AComposition: TdxCompositionReportLink): Boolean;
  begin
    Result := (AComposition <> nil) and not (csRebuildReportLink in AComposition.CompositionState);
  end;

var
  Composition: TdxCompositionReportLink;
begin
  Composition := CurrentCompositionByLink(AReportLink);
  if (AReportLink is TdxCompositionReportLink) or (Composition = nil) or IsCompositionActivatation(Composition) then
  begin
    PrepareBuildReport(AReportLink);
    PrepareLongOperation;
    if AReportLink.CurrentPrintStyle is TdxPSPrintStyle then
      TdxPSPrintStyle(AReportLink.CurrentPrintStyle).BeforeGenerating;
  end;
  try
    if IsCompositionActivatation(Composition) then
      Composition.ActivateLink(AReportLink)
    else
      AReportLink.InternalActivate;

    if AutoUpdateDateTime then
    begin
      if IsCompositionActivatation(Composition) and (Composition.DataSource = rldsComponent) then
        Composition.DateTime := Now
      else
        if AReportLink.DataSource = rldsComponent then
          AReportLink.DateTime := Now;
    end;
  finally
    if (AReportLink is TdxCompositionReportLink) or (Composition = nil) or IsCompositionActivatation(Composition) then
    begin
      if AReportLink.CurrentPrintStyle is TdxPSPrintStyle then
        TdxPSPrintStyle(AReportLink.CurrentPrintStyle).AfterGenerating;
      UnprepareLongOperation;
      UnprepareBuildReport(AReportLink);
    end;
    FormatChanged(AReportLink);
  end;
end;

function TCustomdxComponentPrinter.CheckLink(Value: TBasedxReportLink): TBasedxReportLink;
begin
  if Value <> nil then
    CurrentLink := Value;
  Result := CurrentLink;
end;

function TCustomdxComponentPrinter.CreateLink(ALinkClass: TdxReportLinkClass;
  AComponent: TComponent; AOwner: TComponent): TBasedxReportLink;
var
  LinkClass: TdxReportLinkClass;
begin
  Result := nil;
  LinkClass := ALinkClass;
  if AComponent <> nil then
    if IsSupportedCompClass(AComponent) then
      LinkClass := dxPSLinkClassByCompClass(TComponentClass(AComponent.ClassType))
    else
      if IsDesigning then
        raise EdxComponentPrinter.Create(cxGetResourceString(@sdxComponentNotSupported));
  if LinkClass = nil then Exit;
  Result := LinkClass.Create(AOwner);
  if AComponent <> nil then
    Result.Component := AComponent;
  Result.SetComponentPrinter(Self);
  DoAddReportLink(Result);
  DesignerModified;
end;

procedure TCustomdxComponentPrinter.DeactivateLink(AReportLink: TBasedxReportLink);
begin
  AReportLink.DoDestroyReport;
end;

procedure TCustomdxComponentPrinter.InsertLink(Value: TBasedxReportLink);
begin
  FReportLinks.Add(Value);
  Value.FComponentPrinter := Self;
  Value.ScaleForPPI(ScaleFactor.TargetDPI);
  if LinkCount = 1 then
    Value.IsCurrentLink := True;
end;

procedure TCustomdxComponentPrinter.MoveLink(ACurIndex, ANewIndex: Integer);
begin
  FReportLinks.Move(ACurIndex, ANewIndex);
  DesignerUpdate(nil);
end;

procedure TCustomdxComponentPrinter.RemoveLink(Value: TBasedxReportLink);
var
  AIndex: Integer;
begin
  if not IsDestroying and (FCurrentLink = Value) then
  begin
    if PreviewExists then
      DestroyPreviewWindowForm;
    AIndex := Value.Index;
  end
  else
    AIndex := -1;

  FReportLinks.Remove(Value);
  Value.FComponentPrinter := nil;
  if AIndex <> -1 then
    ResyncCurrentLink(AIndex);
  DoDeleteReportLink(Value);
end;

procedure TCustomdxComponentPrinter.ResyncCurrentLink(AIndex: Integer);
begin
  AIndex := Min(AIndex, LinkCount - 1);
  if AIndex < 0 then
  begin
    FCurrentLink := nil;
    DoChangeCurrentLink;
  end
  else
    CurrentLink := ReportLink[AIndex];
end;

procedure TCustomdxComponentPrinter.OnHFTextEntryChosen(Sender: TObject; const AEntry: string);
var
  PagePart: TCustomdxPageObject;
  Part1, Part2, Part3: string;
begin
  if (CurrentLink <> nil) and (TdxPrintStyleManager(Sender) = CurrentLink.StyleManager) then
  begin
    if PreviewExists then
      case PreviewWindow.State of
        prsEditHeaders:
          PagePart := CurrentLink.RealPrinterPage.PageHeader;
        prsEditFooters:
          PagePart := CurrentLink.RealPrinterPage.PageFooter;
      else //prsNone
        Exit;
      end
    else
      PagePart := CurrentLink.RealPrinterPage.PageHeader;

    if PagePart <> nil then
    begin
      dxPSSplitAutoHFTextEntry(AEntry, Part1, Part2, Part3);
      if (Part2 = '') and (Part3 = '') then
        PagePart.Titles[PreviewWindow.HFEditPart].Add(Part1)
      else
        with PagePart do
        begin
          if Part1 <> '' then LeftTitle.Add(Part1);
          if Part2 <> '' then CenterTitle.Add(Part2);
          if Part3 <> '' then RightTitle.Add(Part3);
        end;

      if PreviewExists then
        case PreviewWindow.State of
          prsEditHeaders:
            PreviewWindow.InvalidatePagesHeaderContent;
          prsEditFooters:
            PreviewWindow.InvalidatePagesFooterContent;
        end;
    end;
  end;
end;

procedure TCustomdxComponentPrinter.PreviewOptionsChangeHandler(Sender: TObject);
begin
  if PreviewExists then
  begin
    PreviewWindow.Options.Assign(PreviewOptions);
    PreviewWindow.Caption := PreviewOptions.Caption;
    PreviewWindow.HelpContext := PreviewOptions.HelpContext;
    PreviewWindowForm.HelpFile := PreviewOptions.HelpFile;
    PreviewWindowForm.WindowState := PreviewOptions.WindowState;
  end;
  DesignerModified;
end;

procedure TCustomdxComponentPrinter.DesignerModified;
begin
  if ReportLinkDesigner <> nil then
    ReportLinkDesigner.Modified;
end;

procedure TCustomdxComponentPrinter.DesignerUpdate(AnItem: TBasedxReportLink);
begin
  if ReportLinkDesigner <> nil then
    ReportLinkDesigner.Update(AnItem);
end;

function TCustomdxComponentPrinter.IsDesigning: Boolean;
begin
  Result := csDesigning in ComponentState;
end;

function TCustomdxComponentPrinter.IsDestroying: Boolean;
begin
  Result := csDestroying in ComponentState;
end;

function TCustomdxComponentPrinter.IsLoading: Boolean;
begin
  Result := csLoading in ComponentState;
end;

procedure TCustomdxComponentPrinter.WndProc(var Message: TMessage);
var
  I: Integer;
begin
  with Message do
  begin
    case Msg of
      WM_SETTINGCHANGE:
        begin
          RereadDefaultPrinterPage;
          for I := 0 to LinkCount - 1 do
            TdxReportLinkPrinterPage(ReportLink[I].PrinterPage).SynchronizeMeasurementUnits;
          DesignerModified;
        end;
    end;
    Result := DefWindowProc(FWindowHandle, Msg, wParam, lParam);
  end;
end;

procedure TCustomdxComponentPrinter.FormatChanged(AReportLink: TBasedxReportLink);
begin
  dxHFFormatObject.DateFormat := dxPgsDlg.DateFormats[AReportLink.DateFormat];
  dxHFFormatObject.DateTime := AReportLink.DateTime;
  dxHFFormatObject.PageNumberFormat := AReportLink.PageNumberFormat;
  dxHFFormatObject.StartPageIndex := AReportLink.StartPageIndex;
  dxHFFormatObject.TimeFormat := dxPgsDlg.TimeFormats[AReportLink.TimeFormat];
end;

procedure TCustomdxComponentPrinter.AssignReportLinks(Source: TCustomdxComponentPrinter);
var
  SaveOwner: TComponent;
  I: Integer;
  Link: TBasedxReportLink;
begin
  if LinkCount > 0 then
    SaveOwner := ReportLink[0].Owner
  else
    SaveOwner := Owner;
  DeleteAllLinks;
  if Source <> nil then
    for I := 0 to Source.LinkCount - 1 do
    begin
      Link := Source.ReportLink[I];
      with AddEmptyLinkEx(Link.LinkClass, SaveOwner) do
      begin
        Component := Link.Component;
        Assign(Link);
      end;
    end;
end;

function TCustomdxComponentPrinter.CreateLinkFromFile(const AFileName: string): TBasedxReportLink;
var
  AStream: TFileStream;
begin
  try
    AStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
    try
      Result := CreateLinkFromStream(AStream);
    finally
      AStream.Free;
    end;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TCustomdxComponentPrinter.CreateLinkFromStream(AStream: TStream): TBasedxReportLink;
var
  AStorageInfo: TdxPSDataStorageInfo;
begin
  try
    AStorageInfo := TBasedxReportLink.ExtractStorageInfo(AStream, False);
    if AStorageInfo.StorageVersion = dxPSStorageVersion then
      if AStorageInfo.LinkClass <> nil then
      begin
        Result := AStorageInfo.LinkClass.Create(Self.Owner);
        Result.InternalLoadDataFromStream(AStream);
      end
      else
        raise EdxReportLink.CreateFmt(cxGetResourceString(@sdxLinkIsNotIncludedInUsesClause), [AStorageInfo.LinkClassName])
    else
      raise EdxInvalidStorageVersion.Create(AStorageInfo.StorageVersion);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TCustomdxComponentPrinter.AddComposition: TdxCompositionReportLink;
begin
  Result := AddEmptyLinkEx(TdxCompositionReportLink, Owner) as TdxCompositionReportLink;
end;

function TCustomdxComponentPrinter.AddEmptyLink(ALinkClass: TdxReportLinkClass): TBasedxReportLink;
begin
  Result := AddEmptyLinkEx(ALinkClass, Owner);
end;

function TCustomdxComponentPrinter.AddEmptyLinkEx(ALinkClass: TdxReportLinkClass;
  AOwner: TComponent): TBasedxReportLink;
begin
  Result := CreateLink(ALinkClass, nil, AOwner);
end;

function TCustomdxComponentPrinter.AddLink(AComponent: TComponent): TBasedxReportLink;
begin
  Result := AddLinkEx(AComponent, Self.Owner);
end;

function TCustomdxComponentPrinter.AddLinkEx(AComponent: TComponent; AOwner: TComponent): TBasedxReportLink;
begin
  Result := CreateLink(nil, AComponent, AOwner);
end;

procedure TCustomdxComponentPrinter.DeleteAllLinks;
begin
  while LinkCount > 0 do
    DeleteLink(LinkCount - 1);
end;

procedure TCustomdxComponentPrinter.DeleteLink(AIndex: Integer);
var
  ALink: TBasedxReportLink;
begin
  if (AIndex > -1) and (AIndex < LinkCount) then
  begin
    ALink := ReportLink[AIndex];
    ALink.Free;
  end;
end;

procedure TCustomdxComponentPrinter.DestroyReport(AReportLink: TBasedxReportLink = nil);
begin
  AReportLink := CheckLink(AReportLink);
  if AReportLink <> nil then
    AReportLink.DestroyReport;
end;

function TCustomdxComponentPrinter.FindLinkByComponent(Value: TComponent;
  ACanCreate: Boolean = False): TBasedxReportLink;
var
  I: Integer;
begin
  if Value <> nil then
    for I := 0 to LinkCount - 1 do
    begin
      Result := ReportLink[I];
      if Result.Component = Value then Exit;
    end;

  if ACanCreate then
    Result := AddLink(Value)
  else
    Result := nil;
end;

procedure TCustomdxComponentPrinter.GetLinks(AList: TList);
var
  I: Integer;
begin
  for I := 0 to LinkCount - 1 do
    AList.Add(ReportLink[I]);
end;

function TCustomdxComponentPrinter.IndexOfLink(AReportLink: TBasedxReportLink): Integer;
begin
  Result := FReportLinks.IndexOf(AReportLink);
end;

function TCustomdxComponentPrinter.IndexOfLink(const AName: string): Integer;
begin
  Result := IndexOfLinkByName(AName);
end;

function TCustomdxComponentPrinter.IndexOfLinkByName(const AName: string): Integer;
begin
  Result := IndexOfLink(LinkByName(AName));
end;

function TCustomdxComponentPrinter.LinkByName(const AName: string): TBasedxReportLink;
var
  I: Integer;
begin
  for I := 0 to LinkCount - 1 do
  begin
    Result := ReportLink[I];
    if Result.Name = AName then Exit;
  end;
  Result := nil;
end;

procedure TCustomdxComponentPrinter.RebuildReport(AReportLink: TBasedxReportLink = nil);
begin
  if AReportLink = nil then
    AReportLink := CurrentLink;
  if AReportLink <> nil then
    AReportLink.RebuildReport;
end;

function TCustomdxComponentPrinter.CurrentCompositionByLink(AReportLink: TBasedxReportLink): TdxCompositionReportLink;
var
  I: Integer;
  ALink: TBasedxReportLink;
begin
  for I := 0 to LinkCount - 1 do
  begin
    ALink := ReportLink[I];
    if (ALink is TdxCompositionReportLink) and
      TdxCompositionReportLink(ALink).IsCurrentLink and
      TdxCompositionReportLink(ALink).Items.LinkExists(AReportLink) then
    begin
      Result := TdxCompositionReportLink(ALink);
      Exit;
    end;
  end;
  Result := nil;
end;

procedure TCustomdxComponentPrinter.GetCompositionsByLink(AReportLink: TBasedxReportLink;
  ACompositions: TList);
var
  I: Integer;
  ALink: TBasedxReportLink;
begin
  for I := 0 to LinkCount - 1 do
  begin
    ALink := ReportLink[I];
    if (ALink is TdxCompositionReportLink) and
      TdxCompositionReportLink(ALink).Items.LinkExists(AReportLink) then
      ACompositions.Add(ALink);
  end;
end;

procedure TCustomdxComponentPrinter.GetItems(AComposition: TdxCompositionReportLink;
  AStrings: TStrings; AExcludeAssigned: Boolean);
var
  I: Integer;
  AItem: TBasedxReportLink;
begin
  if AComposition.ComponentPrinter = Self then
  begin
    AStrings.BeginUpdate;
    try
      for I := 0 to LinkCount - 1 do
      begin
        AItem := ReportLink[I];
        if not (AItem is TdxCompositionReportLink) and
          (not AExcludeAssigned or not IsLinkInComposition(AItem, AComposition)) then
          AStrings.AddObject(AItem.Caption, AItem);
      end;
    finally
      AStrings.EndUpdate;
    end;
  end;
end;

function TCustomdxComponentPrinter.IsLinkInComposition(AReportLink: TBasedxReportLink;
  AComposition: TdxCompositionReportLink): Boolean;
var
  AList: TList;
begin
  AList := TList.Create;
  try
    GetCompositionsByLink(AReportLink, AList);
    Result := AList.IndexOf(AComposition) <> -1;
  finally
    AList.Free;
  end;
end;

function TCustomdxComponentPrinter.IsLinkInCurrentComposition(AReportLink: TBasedxReportLink): Boolean;
begin
  Result := (CurrentLink is TdxCompositionReportLink) and
    IsLinkInComposition(AReportLink, TdxCompositionReportLink(CurrentLink));
end;

class function TCustomdxComponentPrinter.GetNewLinkName(AReportLink: TBasedxReportLink): string;
begin
  Result := sdxNewLinkNameTemplate;
end;

class function TCustomdxComponentPrinter.IsSupportedCompClass(AComponentClass: TClass): Boolean;
begin
  Result := dxPSIsSupportedCompClass(AComponentClass);
end;

class function TCustomdxComponentPrinter.IsSupportedCompClass(AComponent: TObject{TComponent}): Boolean;
begin
  Result := dxPSIsSupportedCompClass(AComponent);
end;

procedure TCustomdxComponentPrinter.PreparePageSetup;
begin
  Include(FState, cpsPageSetupDialog);
end;

procedure TCustomdxComponentPrinter.UnpreparePageSetup;
begin
  Exclude(FState, cpsPageSetupDialog);
end;

procedure TCustomdxComponentPrinter.PrepareBuildReport(AReportLink: TBasedxReportLink);
begin
  Include(FState, cpsBuilding);
  AReportLink.PrepareBuildReport;
  Listeners.PrepareBuildReport(AReportLink);
  DoStartUpdateReport(AReportLink);
end;

procedure TCustomdxComponentPrinter.UnprepareBuildReport(AReportLink: TBasedxReportLink);
begin
  Listeners.UnprepareBuildReport(AReportLink);
  DoEndUpdateReport(AReportLink);
  Exclude(FState, cpsBuilding);
end;

procedure TCustomdxComponentPrinter.PrepareLongOperation;
begin
  if IsDestroying then Exit;
  if FLongOperationCounter = 0 then
  begin
    FStartTime := GetTickCount;
    if IsShowHourGlass then dxPSStartWait;
  end;
  Inc(FLongOperationCounter);
end;

procedure TCustomdxComponentPrinter.UnprepareLongOperation;
begin
  if IsDestroying then Exit;
  if FLongOperationCounter <> 0 then
  begin
    Dec(FLongOperationCounter);
    if FLongOperationCounter = 0 then
    begin
      if IsShowHourGlass then
        dxPSStopWait;
      if BeepAfterLongOperations then
      begin
        FEndTime := GetTickCount;
        if FEndTime - FStartTime > DWORD(LongOperationTime) then Beep;
      end;
    end;
  end;
end;

procedure TCustomdxComponentPrinter.LoadFromFile(const AName: string);
var
  Stream: TFileStream;
begin
  if (AName <> '') and FileExists(AName) then
  begin
    Stream := TFileStream.Create(AName, fmOpenRead or fmShareDenyWrite);
    try
      LoadFromStream(Stream);
    finally
      Stream.Free;
    end;
  end;
end;

procedure TCustomdxComponentPrinter.LoadFromStream(AStream: TStream);
var
  Version: Integer;
begin
  LoadVersionFromStream(AStream, Version);
  if Version <> Self.Version then Exit;

  PrepareLoadFromStream(AStream);
  try
    BeforeLoadFromStream(AStream);
    try
      try
        LoadLinksFromStream(AStream);
        LoadItselfFromStream(AStream);
      except
        ErrorLoadFromStream(AStream);
      end;
    finally
      AfterLoadFromStream(AStream);
    end;
  finally
    UnprepareLoadFromStream(AStream);
  end;
end;

procedure TCustomdxComponentPrinter.AfterLoadFromStream(AStream: TStream);
begin
  FMemoryStream.Free;
end;

procedure TCustomdxComponentPrinter.SaveToFile(const AName: string);
var
  Stream: TFileStream;
begin
  if ValidateFileName(AName) then
  begin
    Stream := TFileStream.Create(AName, fmCreate);
    try
      SaveToStream(Stream);
    finally
      Stream.Free;
    end;
  end;
end;

procedure TCustomdxComponentPrinter.SaveToStream(AStream: TStream);
begin
  PrepareSaveToStream(AStream);
  try
    SaveVersionToStream(AStream);
    SaveLinksToStream(AStream);
    SaveItselfToStream(AStream);
  finally
    UnprepareSaveToStream(AStream);
  end;
end;

procedure TCustomdxComponentPrinter.DoCustomDrawEntirePage(AReportLink: TBasedxReportLink;
  ACanvas: TCanvas; APageIndex: Integer; ARect: TRect; ANom, ADenom: Integer);
begin
  if Assigned(FOnCustomDrawPage) then
    FOnCustomDrawPage(Self, AReportLink, ACanvas, APageIndex, ARect, ANom, ADenom);
end;

procedure TCustomdxComponentPrinter.DoNewPage(AReportLink: TBasedxReportLink;
  APageIndex: Integer);
begin
  AReportLink := CheckLink(AReportLink);
  if AReportLink <> nil then
  begin
    if Assigned(FOnNewPage) then
      FOnNewPage(Self, AReportLink, APageIndex);
    RaisePrintingEvent(AReportLink, APageIndex, 0, psProgress);
  end;
end;

procedure TCustomdxComponentPrinter.DoEndPrint(AReportLink: TBasedxReportLink);
begin
  AReportLink := CheckLink(AReportLink);
  if AReportLink <> nil then
  begin
    if Assigned(FOnEndPrint) then
      FOnEndPrint(Self, AReportLink);
    RaisePrintingEvent(AReportLink, 0, 0, psEnd);
  end;
  AbortPrinting := False;
end;

procedure TCustomdxComponentPrinter.DoStartPrint(AReportLink: TBasedxReportLink;
  FullPageCount: Integer);
begin
  AReportLink := CheckLink(AReportLink);
  if AReportLink <> nil then
  begin
    if Assigned(FOnStartPrint) then
      FOnStartPrint(Self, AReportLink, FullPageCount);
    RaisePrintingEvent(AReportLink, 0, FullPageCount, psStart);
  end;
end;

procedure TCustomdxComponentPrinter.DoAddReportLink(AReportLink: TBasedxReportLink);
begin
  if not (IsLoading or FInternalStreaming) then
  begin
    if Assigned(FOnAddReportLink) then
      FOnAddReportLink(Self, AReportLink);
  end;
end;

procedure TCustomdxComponentPrinter.DoAfterPreview(AReportLink: TBasedxReportLink);
begin
  if cpoDropStorageModeAfterPreview in Options then
    AReportLink.DataSource := rldsComponent;
  if Assigned(FOnAfterPreview) then
    FOnAfterPreview(Self, AReportLink);
end;

procedure TCustomdxComponentPrinter.DoBeforeDesignReport(AReportLink: TBasedxReportLink);
begin
  if Assigned(FOnBeforeDesignReport) then
    FOnBeforeDesignReport(Self, AReportLink, AReportLink.DesignWindow);
end;

procedure TCustomdxComponentPrinter.DoBeforeDestroyReport(AReportLink: TBasedxReportLink);
begin
  Listeners.BeforeDestroyReport(AReportLink);
end;

function TCustomdxComponentPrinter.DoBeforeExportToPDF(AReportLink: TBasedxReportLink;
  const AFileName: string; AOptions: TdxPSPDFReportExportOptions): Boolean;
begin
  Result := True;
  if Assigned(OnBeforeExportToPDF) then
    OnBeforeExportToPDF(Self, AReportLink, AFileName, AOptions, Result);
end;

procedure TCustomdxComponentPrinter.DoBeforePreview(AReportLink: TBasedxReportLink);
begin
  if (PreviewWindow <> nil) and Assigned(OnBeforePreview) then
    OnBeforePreview(Self, AReportLink);
end;

procedure TCustomdxComponentPrinter.DoChangeComponent(AReportLink: TBasedxReportLink);
begin
  if not IsLoading then
  begin
    Listeners.CurrentLinkChanged(AReportLink);
    if Assigned(OnChangeComponent) then
      OnChangeComponent(Self, AReportLink);
  end;
end;

procedure TCustomdxComponentPrinter.DoChangeCurrentLink;
begin
  if not (IsLoading or IsDestroying) then
  begin
    Listeners.CurrentLinkChanged(CurrentLink);
    dxCallNotify(OnChangeCurrentLink, Self);
  end;
end;

procedure TCustomdxComponentPrinter.DoDeleteReportLink(AReportLink: TBasedxReportLink);
begin
  if not IsLoading then
  begin
    if Assigned(OnDeleteReportLink) then
      OnDeleteReportLink(Self, AReportLink);
  end;
end;

procedure TCustomdxComponentPrinter.DoFinalizePrintDlgData(
  AReportLink: TBasedxReportLink; var APrintDlgData: TdxPrintDlgData);
begin
  if Assigned(OnFinalizePrintDlgData) then
    OnFinalizePrintDlgData(Self, AReportLink, APrintDlgData);
end;

procedure TCustomdxComponentPrinter.DoInitializePrintDlgData(
  AReportLink: TBasedxReportLink; var APrintDlgData: TdxPrintDlgData);
begin
  if Assigned(OnInitializePrintDlgData) then
    OnInitializePrintDlgData(Self, AReportLink, APrintDlgData);
end;

procedure TCustomdxComponentPrinter.DoMeasureReportFootnotes(AReportLink: TBasedxReportLink; var AHeight: Integer);
begin
  if Assigned(OnMeasureReportFootnotes) then
    OnMeasureReportFootnotes(Self, AReportLink, AHeight);
end;

procedure TCustomdxComponentPrinter.DoMeasureReportTitle(AReportLink: TBasedxReportLink; var AHeight: Integer);
begin
  if Assigned(OnMeasureReportTitle) then
    OnMeasureReportTitle(Self, AReportLink, AHeight);
end;

procedure TCustomdxComponentPrinter.DoDesignReport(AReportLink: TBasedxReportLink; ADone: Boolean);
begin
  if Assigned(OnDesignReport) then
    OnDesignReport(Self, AReportLink, ADone);

  if ADone and AReportLink.IsCurrentLink then
  begin
    if AReportLink.DesignWindow.CanApply then
      AReportLink.RebuildReport;
  end;
end;

procedure TCustomdxComponentPrinter.DoPrintDeviceBusy;
var
  ADone: Boolean;
begin
  ADone := False;
  if Assigned(OnPrintDeviceBusy) then
    OnPrintDeviceBusy(Self, ADone);
  if not ADone then
    StdProcessPrintDeviceBusy;
end;

procedure TCustomdxComponentPrinter.StdProcessPrintDeviceBusy;
begin
  MessageError(cxGetResourceString(@sdxPrintDeviceIsBusy));
end;

procedure TCustomdxComponentPrinter.DoPrintDeviceError;
var
  ADone: Boolean;
begin
  ADone := False;
  if Assigned(OnPrintDeviceError) then
    OnPrintDeviceError(Self, ADone);
  if not ADone then
    StdProcessPrintDeviceError;
end;

procedure TCustomdxComponentPrinter.StdProcessPrintDeviceError;
begin
  MessageError(cxGetResourceString(@sdxPrintDeviceError));
end;

procedure TCustomdxComponentPrinter.DoLayoutChanged(AReportLink: TBasedxReportLink);
begin
  Listeners.LayoutChanged(AReportLink);
end;

procedure TCustomdxComponentPrinter.DoPageParamsChanged(AReportLink: TBasedxReportLink);
begin
  Listeners.PageParamsChanged(AReportLink);
end;

procedure TCustomdxComponentPrinter.DoPageSetup(AReportLink: TBasedxReportLink; ADone: Boolean);
begin
  if Assigned(OnPageSetup) then
    OnPageSetup(Self, AReportLink, ADone);
end;

procedure TCustomdxComponentPrinter.DoProgress(AReportLink: TBasedxReportLink; const PercentDone: Double);

  procedure ProcessKeyboardMessages;
  var
    AMsg: TMsg;
  begin
    while PeekMessage(AMsg, 0, WM_KEYFIRST, WM_KEYLAST, PM_NOREMOVE) do
    begin
      case Integer(GetMessage(AMsg, 0, WM_KEYFIRST, WM_KEYLAST)) of
        -1: Break;
         0: begin
             PostQuitMessage(AMsg.wParam);
             Break;
           end;
      end;
      DispatchMessage(AMsg);
    end;
  end;

begin
  if IsGenerateReportProgressEvent and Assigned(FOnGenerateReportProgress) then
    FOnGenerateReportProgress(Self, AReportLink, PercentDone);
  if GetAsyncKeyState(VK_ESCAPE) < 0 then
    ProcessKeyboardMessages;
  RaiseBuildingEvent(AReportLink, PercentDone, bsProgress);
end;

procedure TCustomdxComponentPrinter.DoStartUpdateReport(AReportLink: TBasedxReportLink);
begin
  if IsGenerateReportProgressEvent and Assigned(FOnStartGenerateReport) then
    FOnStartGenerateReport(Self, AReportLink);
  RaiseBuildingEvent(AReportLink, 0, bsStart);
end;

procedure TCustomdxComponentPrinter.DoEndUpdateReport(AReportLink: TBasedxReportLink);
begin
  DoProgress(AReportLink, 100);
  if IsGenerateReportProgressEvent and Assigned(FOnEndGenerateReport) then
    FOnEndGenerateReport(Self, AReportLink);
  RaiseBuildingEvent(AReportLink, 0, bsEnd);
end;

procedure TCustomdxComponentPrinter.InitializeDefaultPrintDlgData(
  AReportLink: TBasedxReportLink; out APrintDlgData: TdxPrintDlgData);
const
  BtnEnabledOn: TdxPrintDlgButtons = [pdbPreview, pdbPageSetup, pdbHelp];
  BtnVisibleOn: TdxPrintDlgButtons = [pdbPreview, pdbPageSetup, pdbHelp];
  OptEnabledOn: TdxPrintDlgOptions = [pdoCurrentPage];
  OptVisibleOn: TdxPrintDlgOptions = [pdoCurrentPage];
begin
  FillChar(APrintDlgData, SizeOf(TdxPrintDlgData), 0);
  with APrintDlgData do
  begin
    DialogData.Collate := False;
    DialogData.Copies := 1;
    DialogData.FileList := TStringList.Create;
    DialogData.FileList.Assign(PrintFileList);
    DialogData.MaxRange := CurrentLink.PageCount; // (v2.2)
    if DialogData.MaxRange = 0 then DialogData.MaxRange := -1;
    DialogData.MinRange := 1;
    if CurrentLink.PageCount = 0 then DialogData.MinRange := 0;
    DialogData.PageCount := CurrentLink.PageCount;
    DialogData.PageNums := pnAll;
    DialogData.PageRanges := prAll;
    if DialogData.PageCount > 0 then {v3.2}
      DialogData.Pages := '1-' + IntToStr(DialogData.PageCount);
    DialogData.PrintToFile := FSavePrintToFile;
    if not IsDesigning and (not (cpsPreviewing in State) or (pvoPrintStyles in PreviewOptions.VisibleOptions)) then
      DialogData.StyleManager := CurrentLink.StyleManager;
    DialogData.PageCount := CurrentLink.PageCount;

    Title := cxGetResourceString(@sdxPrintDialogCaption);

    OptionsEnabled := pdoDefaultOptionsEnabled + OptEnabledOn;
    OptionsVisible := pdoDefaultOptionsVisible + OptVisibleOn;
    ButtonsEnabled := pdbDefault + BtnEnabledOn;
    ButtonsVisible := pdbDefault + BtnVisibleOn;
    if cpsPreviewing in State then
      ButtonsVisible := ButtonsVisible - [pdbPreview];

    Events.OnPageSetup := PrnDlgPageSetup;
    IsCheckUserInput := True;
  end;
  DoInitializePrintDlgData(AReportLink, APrintDlgData);
end;

function TCustomdxComponentPrinter.GetPrintTitle(AReportLink: TBasedxReportLink): string;
begin
  Result := FPrintTitle;
  if Assigned(FOnGetPrintTitle) then
  begin
    AReportLink := CheckLink(AReportLink);
    if AReportLink <> nil then
      FOnGetPrintTitle(Self, AReportLink, Result);
  end;
end;

function TCustomdxComponentPrinter.PreviewExists: Boolean;
begin
  Result := (PreviewWindow <> nil) and (cpsPreviewing in State);
end;

procedure TCustomdxComponentPrinter.Preview(AModal: Boolean = True; AReportLink: TBasedxReportLink = nil);

  procedure UnloadExplorerItem;
  var
    ALoadedItem: TdxPSExplorerItem;
  begin
    ALoadedItem := Explorer.LoadedItem;
    if Assigned(ALoadedItem) then
    begin
      Explorer.DoItemDataLoadError(ALoadedItem);
      Explorer.FLoadedItem := nil; //!!!
    end;
  end;

  function PrepareReportLink: Boolean;
  begin
    Result := True;
    if AReportLink.DataProviderPresent and (IsDesigning or IsRebuildBeforePreview) then
    begin
      AReportLink.PrinterPage.InitFromPrintDevice;
      try
        RebuildReport(AReportLink);
        if AbortBuilding then
          Exit(False);
      except
        if Explorer <> nil then
        begin
          UnloadExplorerItem;
          if not (cpsExplore in State) then
            Exit(False);
        end;
      end;
      DesignerModified;
    end
    else
      if AutoUpdateDateTime then
        AReportLink.DateTime := Now;
  end;

begin
  AReportLink := CheckLink(AReportLink);
  if Assigned(AReportLink) then
  begin
    if PreviewExists then
    begin
      if IsRebuildBeforePreview then
        PrepareReportLink;
      ShowPreviewWindowForm(False);
    end
    else
    begin
      InitDevModeFromPrinterPageSettings(AReportLink.RealPrinterPage);
      if PrepareReportLink then
      begin
        Include(FState, cpsPreviewing);
        if Explorer <> nil then
          Explorer.Refresh;
        CreatePreviewWindowForm(AReportLink);
        if IsExplorerMode then
          PreviewWindow.ExplorerTree.MakeItemVisible(Explorer.LoadedItem);

        DoBeforePreview(AReportLink);
        ShowPreviewWindowForm(AModal);
        if AModal then
          ReleasePreviewWindowForm;
      end;
    end;
  end;
end;

procedure TCustomdxComponentPrinter.PaintPage(ACanvas: TCanvas; APageIndex: Integer;
  const APageBounds, AContentBounds: TRect; AReportLink: TBasedxReportLink = nil);
begin
  PaintPageCore(ACanvas, APageIndex, 100, APageBounds, AContentBounds, AReportLink);
end;

procedure TCustomdxComponentPrinter.DoCustomDrawReportFootnotes(
  AReportLink: TBasedxReportLink; ACanvas: TCanvas; ARect: TRect;
  var ATextAlignX: TcxTextAlignX; var ATextAlignY: TcxTextAlignY;
  var AColor: TColor; AFont: TFont; var ADone: Boolean; APixelsNumerator: Integer = 0);
begin
  if Assigned(OnCustomDrawReportFootnotes) then
  begin
    if APixelsNumerator = 0 then
      APixelsNumerator := PixelsNumerator;
    OnCustomDrawReportFootnotes(Self, AReportLink, ACanvas, ARect, APixelsNumerator,
      PixelsDenominator, ATextAlignX, ATextAlignY, AColor, AFont, ADone);
  end;
end;

procedure TCustomdxComponentPrinter.DoCustomDrawReportTitle(AReportLink: TBasedxReportLink;
  ACanvas: TCanvas; ARect: TRect; var ATextAlignX: TcxTextAlignX;
  var ATextAlignY: TcxTextAlignY; var AColor: TColor; AFont: TFont;
  var ADone: Boolean; APixelsNumerator: Integer = 0);
begin
  if Assigned(OnCustomDrawReportTitle) then
  begin
    if APixelsNumerator = 0 then
      APixelsNumerator := PixelsNumerator;
    OnCustomDrawReportTitle(Self, AReportLink, ACanvas, ARect, APixelsNumerator,
      PixelsDenominator, ATextAlignX, ATextAlignY, AColor, AFont, ADone);
  end;
end;

procedure TCustomdxComponentPrinter.DoCustomDrawPageHeaderOrFooter(
  AReportLink: TBasedxReportLink; AHFObject: TCustomdxPageObject;
  ACanvas: TCanvas; APageIndex: Integer; R: TRect;
  var ADefaultDrawText, ADefaultDrawBackground: Boolean;
  APixelsNumerator: Integer = 0);
begin
  if APixelsNumerator = 0 then
    APixelsNumerator := PixelsNumerator;
  if AHFObject is TdxPageHeader then
  begin
    if Assigned(FOnCustomDrawPageHeader) then
      FOnCustomDrawPageHeader(Self, AReportLink, ACanvas, APageIndex, R,
        APixelsNumerator, PixelsDenominator, ADefaultDrawText, ADefaultDrawBackground)
  end
  else
    if Assigned(FOnCustomDrawPageFooter) then
      FOnCustomDrawPageFooter(Self, AReportLink, ACanvas, APageIndex, R,
        APixelsNumerator, PixelsDenominator, ADefaultDrawText, ADefaultDrawBackground);
end;

procedure TCustomdxComponentPrinter.PrintPage(AReportLink: TBasedxReportLink; APageIndex: Integer);
var
  R: TRect;
begin
  if AbortPrinting then Exit;
  AReportLink := CheckLink(AReportLink);
  if AReportLink = nil then Exit;

  with dxPrintDevice do
  begin
    R := Bounds(0, 0, PageWidth, PageHeight);
    InflateRect(R, PhysOffsetX, PhysOffsetY);
  end;
  PaintPage(dxPrintDevice.Canvas, APageIndex, R, R, AReportLink);
end;

procedure TCustomdxComponentPrinter.PrintPages(
  const APageIndexes: TdxPSPageIndexes; AReportLink: TBasedxReportLink = nil);
begin
  PrintPagesEx(APageIndexes, pnAll, 1, False, AReportLink);
end;

procedure TCustomdxComponentPrinter.PrintPagesEx(const APageIndexes: TdxPSPageIndexes;
  APageNums: TdxPageNumbers; ACopyCount: Integer; ACollate: Boolean; AReportLink: TBasedxReportLink = nil);

  function GetTotalPageCount: Integer;
  begin
    Result := ACopyCount * Length(APageIndexes);
    if APageNums <> pnAll then
    begin
      if (APageNums = pnOdd) and Odd(Result) then
        Inc(Result);
      Result := Result div 2;
    end;
  end;

  procedure GetActualPageIndexes(out AnActualPageIndexes: TIntegers);

    function AreAllPagesInRange: Boolean;
    var
      I: Integer;
    begin
      if FPrintAll then
        Result := True
      else
      begin
        Result := Length(APageIndexes) = AReportLink.PageCount;
        if Result then
          for I := 1 to AReportLink.PageCount do
          begin
            Result := APageIndexes[I - 1] = I;
            if not Result then Exit;
          end;
      end;
    end;

  var
    IsRecombineNeeded: Boolean;
    ColCount, RowCount, I, J, K: Integer;
  begin
    SetLength(AnActualPageIndexes, Length(APageIndexes));
    IsRecombineNeeded := AReportLink.RealPrinterPage.PageOrder = poDownThenOver;
    if IsRecombineNeeded then
    begin
      AReportLink.GetPageColRowCount(ColCount, RowCount);
      IsRecombineNeeded := (ColCount <> 1) and (RowCount <> 1) and AreAllPagesInRange;
    end;

    if IsRecombineNeeded then
    begin
      K := 0;
      for I := 0 to ColCount - 1 do
        for J := 0 to RowCount - 1 do
        begin
          AnActualPageIndexes[K] := APageIndexes[I + J * ColCount];
          Inc(K);
        end;
    end
    else
      for I := 0 to Length(APageIndexes) - 1 do
        AnActualPageIndexes[I] := APageIndexes[I];
  end;

  function CanPrintPage(AIndex, APageIndex: Integer): Boolean;
  begin
    Result := not ((APageIndex < 0) or
      (APageIndex > AReportLink.PageCount - 1) or
      ((APageNums = pnEven) and not Odd(AIndex)) or
      ((APageNums = pnOdd) and Odd(AIndex)));
  end;

  function CanShowErrorMessage(AErrorCode: LongWord): Boolean;
  begin
    Result := (Integer(AErrorCode) <= 0) and (AErrorCode <> SP_APPABORT) and
      (AErrorCode <> SP_USERABORT) and ((dxPrintDevice.CurrentPort = nil) or
      (StrIComp(dxPrintDevice.CurrentPort, sdxFilePort) <> 0));
  end;

  function BeginPrint: Boolean;
  var
    AErrorCode: Integer;
  begin
    AErrorCode := dxPrintDevice.BeginDoc;
    if CanShowErrorMessage(AErrorCode) then
      DoPrintDeviceError;
    Result := AErrorCode > 0;
  end;

  procedure EndPrint;
  begin
    if dxPrintDevice.Printing then
    begin
      if AbortPrinting then
        dxPrintDevice.Abort
      else
        dxPrintDevice.EndDoc;
    end;
    DoEndPrint(AReportLink);
  end;

  procedure PreparePrintDevice(const AActualPageIndexes: TIntegers; out ACurrentPrinterPage: TdxPrinterPage);
  begin
    dxPSEdgePatternFactory.ResetPrintItems;
    dxPSFillPatternFactory.ResetPrintItems;
    ACurrentPrinterPage := AReportLink.RenderInfo.GetActualPageRenderInfo(AActualPageIndexes[0] - 1).PrinterPage;
    ACurrentPrinterPage.ApplyToPrintDevice;
    dxPrintDevice.Title := GetPrintTitle(AReportLink);
  end;

  procedure DoPrintPage(var ACurrentPage: Integer; APageIndex: Integer; var ACurrentPrinterPage: TdxPrinterPage);
  var
    APrinterPage: TdxPrinterPage;
  begin
    APrinterPage := AReportLink.RenderInfo.GetActualPageRenderInfo(APageIndex).PrinterPage;
    if not APrinterPage.IsEqual(ACurrentPrinterPage) then
    begin
      ACurrentPrinterPage := APrinterPage;
      ACurrentPrinterPage.ApplyToPrintDevice;
    end;

    if ACurrentPage > 0 then
    begin
      DoNewPage(AReportLink, ACurrentPage);
      dxPrintDevice.NewPage;
    end;

    if Application.Terminated then
      AbortPrinting := True;
    if not AbortPrinting then
      PrintPage(AReportLink, APageIndex);

    Inc(ACurrentPage);
  end;

  procedure DoPrintPages(const AActualPageIndexes: TIntegers; ACollate: Boolean; ACopyCount: Integer);
  var
    ACopyIndex: Integer;
    ACurrentPage: Integer;
    ACurrentPrinterPage: TdxPrinterPage;
    APageIndex: Integer;
    I: Integer;
  begin
    PreparePrintDevice(AActualPageIndexes, ACurrentPrinterPage);
    if BeginPrint then
    try
      ACopyCount := Max(ACopyCount, 1);
      DoStartPrint(AReportLink, GetTotalPageCount);

      ACurrentPage := 0;
      if ACollate or (ACopyCount = 1) then
      begin
        for ACopyIndex := 1 to ACopyCount do
        begin
          for I := 0 to Length(AActualPageIndexes) - 1 do
          begin
            APageIndex := AActualPageIndexes[I] - 1;
            if CanPrintPage(I, APageIndex) then
              DoPrintPage(ACurrentPage, APageIndex, ACurrentPrinterPage);
            if AbortPrinting then Break;
          end;
          if AbortPrinting then Break;
        end;
      end
      else
        for I := 0 to Length(AActualPageIndexes) - 1 do
        begin
          APageIndex := AActualPageIndexes[I] - 1;
          if CanPrintPage(I, APageIndex) then
            for ACopyIndex := 1 to ACopyCount do
            begin
              DoPrintPage(ACurrentPage, APageIndex, ACurrentPrinterPage);
              if AbortPrinting then Break;
            end;
          if AbortPrinting then Break;
        end;
    finally
      EndPrint;
    end;
  end;

  procedure DoPrintReport(const AActualPageIndexes: TIntegers);
  var
    ASavedCollate: Boolean;
    ASavedCopies: Integer;
  begin
    Include(FState, cpsPrinting);
    try
      ASavedCopies := dxPrintDevice.Copies;
      ASavedCollate := dxPrintDevice.Collate;
      try
        if FEmulatePrintCopies then
        begin
          dxPrintDevice.Collate := False;
          dxPrintDevice.Copies := 1;
          DoPrintPages(AActualPageIndexes, ACollate, ACopyCount);
        end
        else
        begin
          dxPrintDevice.Collate := ACollate;
          dxPrintDevice.Copies := ACopyCount;
          DoPrintPages(AActualPageIndexes, False, 1);
        end;
      finally
        dxPrintDevice.Collate := ASavedCollate;
        dxPrintDevice.Copies := ASavedCopies;
      end;
    finally
      Exclude(FState, cpsPrinting);
    end;
  end;

var
  AActualPageIndexes: TIntegers;
begin
  AReportLink := CheckLink(AReportLink);
  if (AReportLink = nil) or not AReportLink.DataProviderPresent then
    Exit;

  PrepareReport(AReportLink);
  try
    if AbortBuilding or (AReportLink.PageCount = 0) then
      Exit;

    dxInitPrintDevice(True);
    if dxPrintDevice.Printing then
    begin
      DoPrintDeviceBusy;
      Exit;
    end;

    if AReportLink.ValidateMargins then
    begin
      GetActualPageIndexes(AActualPageIndexes);
      AReportLink.BeforePrinting;
      try
        try
          DoPrintReport(AActualPageIndexes);
        except
          if dxPrintDevice.Printing then
          try
            dxPrintDevice.Abort;
          except
            Application.HandleException(Self);
          end;
          DoPrintDeviceError;
        end;
      finally
        AReportLink.AfterPrinting;
      end;
    end;
  finally
    UnprepareReport(AReportLink);
  end;
end;

function TCustomdxComponentPrinter.Print(AShowDialog: Boolean;
  APrintDialogData: PdxPrintDlgData; AReportLink: TBasedxReportLink = nil): Boolean;

  function GetPrintDlgData(AReportLink: TBasedxReportLink): TdxPrintDlgData;
  begin
    if APrintDialogData <> nil then
      Result := APrintDialogData^
    else
      InitializeDefaultPrintDlgData(AReportLink, Result);
  end;

  procedure DonePrintDlgData(AReportLink: TBasedxReportLink; var APrintDlgData: TdxPrintDlgData);
  begin
    if APrintDialogData = nil then
      FinalizeDefaultPrintDlgData(AReportLink, APrintDlgData);
  end;

var
  APrintDlgData: TdxPrintDlgData;
begin
  Result := False;
  AReportLink := CheckLink(AReportLink);
  if AReportLink = nil then Exit;
  InitDevModeFromPrinterPageSettings(AReportLink.RealPrinterPage);
  try
    if not AShowDialog then
    begin
      PrepareReport(AReportLink);
      APrintDlgData := GetPrintDlgData(AReportLink);
      try
        if not AbortBuilding and (AReportLink.PageCount > 0) then
          with APrintDlgData.DialogData do
            Result := PrintPagesAsStringEx(Pages, PageNums, Copies, Collate, AReportLink)
        else
          Result := False;
      finally
        UnprepareReport(AReportLink);
      end;
    end
    else
    begin
      APrintDlgData := GetPrintDlgData(AReportLink);
      Result := PrintDialog(AReportLink, APrintDlgData);
    end;
  finally
    DonePrintDlgData(AReportLink, APrintDlgData);
  end;
end;

procedure TCustomdxComponentPrinter.PrepareReport(AReportLink: TBasedxReportLink);
begin
  if not AReportLink.FPrepared then
    if AReportLink.DataProviderPresent then
    begin
      if not PreviewExists and (IsDesigning or IsRebuildBeforePrint) then
      begin
        RebuildReport(AReportLink);
        DesignerModified;
      end
      else
        if AutoUpdateDateTime then
          AReportLink.DateTime := Now;

      AReportLink.FPrepared := True;
    end
    else
      AReportLink.FPrepared := False;
end;

procedure TCustomdxComponentPrinter.UnprepareReport(AReportLink: TBasedxReportLink);
begin
  AReportLink.FPrepared := False;
end;

procedure TCustomdxComponentPrinter.BeforeLoadFromStream(AStream: TStream);
begin
  FMemoryStream := TMemoryStream.Create;
  SaveToStream(FMemoryStream);
  DeleteAllLinks;
end;

procedure TCustomdxComponentPrinter.ErrorLoadFromStream(AStream: TStream);
begin
  DeleteAllLinks;
  FMemoryStream.Position := 0;
  LoadFromStream(FMemoryStream);
  Application.HandleException(Self);
end;

procedure TCustomdxComponentPrinter.PrepareLoadFromStream(AStream: TStream);
begin
  FInternalStreaming := True;
  Include(FState, cpsLoading);
end;

procedure TCustomdxComponentPrinter.UnprepareLoadFromStream(AStream: TStream);
begin
  Exclude(FState, cpsLoading);
  FInternalStreaming := False;
end;

procedure TCustomdxComponentPrinter.LoadItselfFromStream(AStream: TStream);
var
  I: Integer;
begin
  AStream.ReadComponent(Self);
  Loaded;
  for I := 0 to LinkCount - 1 do
    ReportLink[I].Loaded;
end;

procedure TCustomdxComponentPrinter.LoadLinksFromStream(AStream: TStream);
var
  Count, I: Integer;
  Links: TList;
begin
  AStream.ReadBuffer(Count, SizeOf(Count));

  Links := TList.Create;
  try
    Links.Count := Count;

    for I := 0 to Count - 1 do
    begin
      Links[I] := AStream.ReadComponent(nil);
      Owner.InsertComponent(TComponent(Links[I]));
    end;

    for I := 0 to Count - 1 do
    begin
      TBasedxReportLink(Links[I]).ComponentPrinter := Self;
      if TObject(Links[I]) is TdxCompositionReportLink then
        TdxCompositionReportLink(Links[I]).Items.CorrectLinksAfterLoadings;
    end;
  finally
    Links.Free;
  end;
end;

procedure TCustomdxComponentPrinter.LoadVersionFromStream(AStream: TStream; var AVersion: Integer);
begin
  AStream.ReadBuffer(AVersion , SizeOf(AVersion));
end;

procedure TCustomdxComponentPrinter.PrepareSaveToStream(AStream: TStream);
begin
  FInternalStreaming := True;
  Include(FState, cpsSaving);
end;

procedure TCustomdxComponentPrinter.UnprepareSaveToStream(AStream: TStream);
begin
  Exclude(FState, cpsSaving);
  FInternalStreaming := False;
end;

procedure TCustomdxComponentPrinter.SaveItselfToStream(AStream: TStream);
begin
  AStream.WriteComponent(Self);
end;

procedure TCustomdxComponentPrinter.SaveLinksToStream(AStream: TStream);
var
  I: Integer;
begin
  I := LinkCount;
  AStream.WriteBuffer(I , SizeOf(I));
  for I := 0 to LinkCount - 1 do
  begin
    ReportLink[I].InternalStreaming := True;
    AStream.WriteComponent(ReportLink[I]);
  end;
end;

procedure TCustomdxComponentPrinter.SaveVersionToStream(AStream: TStream);
begin
  AStream.WriteBuffer(Version , SizeOf(Version));
end;

procedure TCustomdxComponentPrinter.PrintEx(APageNums: TdxPageNumbers;
  ACopies: Integer; ACollate: Boolean; AReportLink: TBasedxReportLink = nil);
begin
  AReportLink := CheckLink(AReportLink);
  if AReportLink <> nil then
  begin
    PrepareReport(AReportLink);
    try
      if not AbortBuilding and (AReportLink.PageCount > 0) then
      begin
        FPrintAll := True;
        try
          PrintPagesAsStringEx('1-' + Trim(IntToStr(AReportLink.PageCount)), APageNums, ACopies, ACollate, AReportLink);
        finally
          FPrintAll := False;
        end;
      end;
    finally
      UnprepareReport(AReportLink);
    end;
  end;
end;

procedure TCustomdxComponentPrinter.GetPageColRowCount(out ACol, ARow: Integer;
  AReportLink: TBasedxReportLink = nil);
begin
  AReportLink := CheckLink(AReportLink);
  if AReportLink = nil then
  begin
    ACol := 0;
    ARow := 0;
  end
  else
    AReportLink.GetPageColRowCount(ACol, ARow);
end;

function TCustomdxComponentPrinter.GetPageCount(AReportLink: TBasedxReportLink = nil): Integer;
begin
  AReportLink := CheckLink(AReportLink);
  if AReportLink <> nil then
    Result := AReportLink.PageCount
  else
    Result := 0;
end;

function TCustomdxComponentPrinter.PageSetup(AReportLink: TBasedxReportLink): Boolean;
begin
  AReportLink := CheckLink(AReportLink);
  Result := (AReportLink <> nil) and AReportLink.PageSetup;
end;

function TCustomdxComponentPrinter.PageSetupEx(AActivePageIndex: Integer;
  AShowPreviewBtn, AShowPrintBtn: Boolean; out APreviewBtnClicked, APrintBtnClicked: Boolean;
  AReportLink: TBasedxReportLink = nil): Boolean;
begin
  AReportLink := CheckLink(AReportLink);
  Result := (AReportLink <> nil) and AReportLink.PageSetupEx(AActivePageIndex,
    AShowPreviewBtn, AShowPrintBtn, APreviewBtnClicked, APrintBtnClicked);
end;

procedure TCustomdxComponentPrinter.Explore;
var
  PrevCurrentLink: TBasedxReportLink;
begin
  if Explorer <> nil then
  begin
    Include(FState, cpsExplore);
    try
      PrevCurrentLink := CurrentLink;
      CurrentLink := ExplorerRealStubLink;
      try
        CurrentLink.DataSource := rldsExternalStorage;
        Preview(True);
      finally
        if ExplorerStubLink = nil then { Link had been created manually before }
          CurrentLink.Free;
        CurrentLink := PrevCurrentLink;
      end;
    finally
      Exclude(FState, cpsExplore);
    end;
  end;
end;

procedure TCustomdxComponentPrinter.ExportToPDF(AReportLink: TBasedxReportLink);
begin
  AReportLink := CheckLink(AReportLink);
  if AReportLink <> nil then
    AReportLink.ExportToPDF;
end;

procedure TCustomdxComponentPrinter.ExportToPDF(const AFileName: string;
  ACanShowDialog: Boolean; ASettings: TdxPSPDFReportExportOptions;
  AReportLink: TBasedxReportLink);
begin
  AReportLink := CheckLink(AReportLink);
  if AReportLink <> nil then
    AReportLink.ExportToPDF(AFileName, ACanShowDialog, ASettings);
end;

function TCustomdxComponentPrinter.DesignerExists(AReportLink: TBasedxReportLink = nil): Boolean;
begin
  AReportLink := CheckLink(AReportLink);
  Result := (AReportLink <> nil) and (AReportLink.GetDesignerClass <> nil);
end;

function TCustomdxComponentPrinter.DesignerExistsByComponent(AComponent: TComponent): Boolean;
begin
  Result := (AComponent <> nil) and (dxPSDesignerClassByCompClass(TComponentClass(AComponent.ClassType)) <> nil);
end;

function TCustomdxComponentPrinter.DesignReport(AReportLink: TBasedxReportLink): Boolean;
begin
  AReportLink := CheckLink(AReportLink);
  if AReportLink <> nil then
  begin
    Include(FState, cpsDesigning);
    try
      Result := AReportLink.DesignReport;
    finally
      Exclude(FState, cpsDesigning);
    end;
  end
  else
    Result := False;
end;

procedure TCustomdxComponentPrinter.DrawPageHeader(AReportLink: TBasedxReportLink;
  APageIndex: Integer; ARect: TRect;
  ATitleParts: TdxPageTitleParts; ADrawBackground: Boolean);
begin
  AReportLink.DrawPageHeader(APageIndex, ARect, ATitleParts, ADrawBackground);
end;

procedure TCustomdxComponentPrinter.DrawPageFooter(AReportLink: TBasedxReportLink;
  APageIndex: Integer; ARect: TRect;
  ATitleParts: TdxPageTitleParts; ADrawBackground: Boolean);
begin
  AReportLink.DrawPageFooter(APageIndex, ARect, ATitleParts, ADrawBackground);
end;

{ TdxComponentPrinter }

constructor TdxComponentPrinter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCustomizeDlgOptions := [cdoShowDescription];
  FOverWriteExistingFiles := False;
end;

destructor TdxComponentPrinter.Destroy;
begin
  InternalSaveDefaultSettings;
  inherited Destroy;
end;

procedure TdxComponentPrinter.ShowCustomizeDlg;
{$IFNDEF CBUILDER6}
var
  Data: TdxCPDesignerDlgData;
{$ENDIF}
begin
{$IFNDEF CBUILDER6}
  FillChar(Data, SizeOf(TdxCPDesignerDlgData), 0);
  Data.ComponentPrinter := Self;
  if cdoShowDescription in CustomizeDlgOptions then
    Include(Data.Options, doShowDescription);
  Include(FState, cpsCustomizing);
  try
    dxShowCPDesignerDlg(Data);
  finally
    Exclude(FState, cpsCustomizing);
  end;
{$ENDIF}
end;

procedure TdxComponentPrinter.InternalLoadDefaultSettigns;
var
  AIniFile: TCustomIniFile;
begin
  if not IsDesigning then
  begin
    if dxPSStoringManager.BeginStoring(AIniFile) then
    try
      LoadFromIniFile(AIniFile, dxGetStoringSectionName(Self));
    finally
      dxPSStoringManager.EndStoring(AIniFile);
    end;
  end;
end;

procedure TdxComponentPrinter.InternalSaveDefaultSettings;
var
  AIniFile: TCustomIniFile;
begin
  if not IsDesigning then
  begin
    if dxPSStoringManager.BeginStoring(AIniFile) then
    try
      SaveToIniFile(AIniFile, dxGetStoringSectionName(Self));
    finally
      dxPSStoringManager.EndStoring(AIniFile);
    end;
  end;
end;

procedure TdxComponentPrinter.Loaded;
begin
  inherited Loaded;
  if (LinkCount > 0) and (CurrentLink = nil) then
    CurrentLink := ReportLink[0];
  InternalLoadDefaultSettigns;
end;

procedure TdxComponentPrinter.LoadFromIniFile(AIniFile: TCustomIniFile; const ASection: string);
begin
  AutoUpdateDateTime := AIniFile.ReadBool(ASection, sdxAutoUpdateDateTime, AutoUpdateDateTime);
  DateFormat := AIniFile.ReadInteger(ASection, sdxDateFormat, DateFormat);
  TimeFormat := AIniFile.ReadInteger(ASection, sdxTimeFormat, TimeFormat);
  PageNumberFormat := TdxPageNumberFormat(AIniFile.ReadInteger(ASection, sdxPageNumberFormat, Ord(TimeFormat)));
  dxLoadStrings(AIniFile, dxValidatePath(ASection) + sdxPrintDlgFilesPath + '\' + Name, FPrintFileList);
end;

procedure TdxComponentPrinter.LoadFromIniFile(const AFileName: string);
var
  AIniFile: TCustomIniFile;
begin
  AIniFile := TMemIniFile.Create(AFileName);
  try
    LoadFromIniFile(AIniFile, dxGetStoringSectionName(Self));
  finally
    AIniFile.Free;
  end;
end;

procedure TdxComponentPrinter.SaveToIniFile(AIniFile: TCustomIniFile; const ASection: string);
begin
  AIniFile.WriteBool(ASection, sdxAutoUpdateDateTime, AutoUpdateDateTime);
  AIniFile.WriteInteger(ASection, sdxDateFormat, DateFormat);
  AIniFile.WriteInteger(ASection, sdxTimeFormat, TimeFormat);
  AIniFile.WriteInteger(ASection, sdxPageNumberFormat, Integer(PageNumberFormat));
  dxSaveStrings(AIniFile, dxValidatePath(ASection) +
    sdxPrintDlgFilesPath + '\' + Name, FPrintFileList);
end;

procedure TdxComponentPrinter.SaveToIniFile(const AFileName: string);
var
  AIniFile: TCustomIniFile;
begin
  AIniFile := TMemIniFile.Create(AFileName);
  try
    SaveToIniFile(AIniFile, dxGetStoringSectionName(Self));
    AIniFile.UpdateFile;
  finally
    AIniFile.Free;
  end;
end;

procedure TdxComponentPrinter.SaveToRegistry(const APath: string);
var
  ARegIniFile: TCustomIniFile;
begin
  if APath <> '' then
  begin
    ARegIniFile := TRegistryIniFile.Create('');
    try
      SaveToIniFile(ARegIniFile, APath);
    finally
      ARegIniFile.Free;
    end;
  end;
end;

procedure TdxComponentPrinter.LoadFromRegistry(const APath: string);
var
  ARegIniFile: TCustomIniFile;
begin
  if APath <> '' then
  begin
    ARegIniFile := TRegistryIniFile.Create('');
    try
      LoadFromIniFile(ARegIniFile, APath);
    finally
      ARegIniFile.Free;
    end;
  end;
end;

function TdxComponentPrinter.DoFilterComponent(
  AComponent: TComponent; var ACaption, ADescription: string): Boolean;
begin
  Result := True;
  if Assigned(FOnFilterComponent) then
    FOnFilterComponent(Self, AComponent, ACaption, ADescription, Result);
end;

procedure TdxComponentPrinter.GetDefaultExportPageFileName(
  AIndex, APageIndex: Integer; var AFileName: string);
begin
  AFileName := Format(AFileName, [APageIndex]);
end;

procedure TdxComponentPrinter.GetExportPageFileName(
  AIndex, APageIndex: Integer; var AFileName: string);
begin
  if Assigned(FOnExportGetPageFileName) then
    FOnExportGetPageFileName(Self, AIndex, APageIndex, AFileName)
  else
    GetDefaultExportPageFileName(AIndex, APageIndex, AFileName);
end;

function TdxComponentPrinter.GetSupportedComponents(AComponents: TStrings): Boolean;
begin
  Result := Assigned(FOnGetSupportedComponents);
  if Result then
    FOnGetSupportedComponents(Self, AComponents);
end;

procedure TdxComponentPrinter.EnumPagesAsImages(const APageIndexes: array of Integer;
  AGraphicClass: TGraphicClass; ADrawBackground: Boolean; ACallBackProc: TdxEnumPagesAsImagesProc;
  ACallBackData, AProgressData, APrepareData: Pointer;
  AReportLink: TBasedxReportLink = nil);
begin
  AReportLink := CheckLink(AReportLink);
  if AReportLink = nil then Exit;
  dxPSEnumReportPages(Self, AReportLink, APageIndexes, AGraphicClass, ADrawBackground,
    ACallBackProc, ACallBackData, OnExportProgress, AProgressData,
    OnExportPrepareGraphic, APrepareData);
end;

procedure TdxComponentPrinter.WritePageAsImageToDisk(
  AComponentPrinter: TCustomdxComponentPrinter; AReportLink: TBasedxReportLink;
  AIndex, APageIndex: Integer; const AGraphic: TGraphic; AData: Pointer;
  var AContinue: Boolean);
const
  Buttons: TMsgDlgButtons = [mbYes, mbYesToAll, mbNo, mbCancel {, mbHelp}];
var
  FileName: string;
  MessageResult: Word;
  MessageText: string;
begin
  if AData = nil then Exit;
  FileName := string(AData);
  GetExportPageFileName(AIndex, APageIndex, FileName);
  if not ValidateFileName(FileName) then Exit;

  FOverWriteFile := True;
  if FileExists(FileName) and not OverWriteExistingFiles and not FOverWriteAll then
  begin
    Beep;
    FOverWriteFile := False;
    MessageText := Format(cxGetResourceString(@sdxConfirmOverWrite), [FileName]);
    MessageResult := MessageDlg(MessageText, mtWarning, Buttons, 0);
    case MessageResult of
      mrYes:
        FOverWriteFile := True;
      mrYesToAll:
        begin
          FOverWriteFile := True;
          FOverWriteAll := True;
        end;
      mrNo:
        begin
          AContinue := dxShowChooseFileNameDlg(FileName);
          if AContinue then
            FOverWriteFile := True;
        end;
      mrCancel:
        AContinue := False;
    end;
  end;
  if FOverWriteFile then
  begin
    if AGraphic is TBitmap then
      TBitmap(AGraphic).HandleType := bmDIB;
    AGraphic.SaveToFile(FileName);
  end;
end;

procedure TdxComponentPrinter.SavePagesAsImagesToDisk(const APageIndexes: array of Integer;
  AGraphicClass: TGraphicClass; ADrawBackground: Boolean; const AFileMask: string;
  AProgressData, APrepareData: Pointer;
  AReportLink: TBasedxReportLink = nil);
var
  PFileMask: PChar;
begin
  FOverWriteAll := False;
  PrepareLongOperation;
  try
    PFileMask := PChar(AFileMask);
    EnumPagesAsImages(APageIndexes, AGraphicClass, ADrawBackground,
      WritePageAsImageToDisk, PFileMask, AProgressData, APrepareData,
      AReportLink);
  finally
    FOverWriteAll := False;
    UnprepareLongOperation;
  end;
end;

{ TAbstractdxPreviewWindowDesigner }

constructor TAbstractdxPreviewWindowDesigner.Create(AComponentPrinter: TCustomdxComponentPrinter);
begin
  inherited Create;
  FComponentPrinter := AComponentPrinter;
  if FComponentPrinter <> nil then
    FComponentPrinter.FPreviewWindowDesigner := Self;
end;

destructor TAbstractdxPreviewWindowDesigner.Destroy;
begin
  if FComponentPrinter <> nil then
    FComponentPrinter.FPreviewWindowDesigner := nil;
  inherited Destroy;
end;

{ TAbstractdxReportLinkDesigner }

constructor TAbstractdxReportLinkDesigner.Create(AComponentPrinter: TCustomdxComponentPrinter);
begin
  inherited Create;
  FComponentPrinter := AComponentPrinter;
  if FComponentPrinter <> nil then
    FComponentPrinter.FReportLinkDesigner := Self;
end;

destructor TAbstractdxReportLinkDesigner.Destroy;
begin
  if FComponentPrinter <> nil then
    FComponentPrinter.FReportLinkDesigner := nil;
  inherited Destroy;
end;

{ TdxPSPreviewDialogManager }

constructor TdxPSPreviewDialogManager.Create;
begin
  inherited Create;
  FList := TList.Create;
end;

destructor TdxPSPreviewDialogManager.Destroy;
begin
  FreeAndNil(FList);
  inherited Destroy;
end;

function TdxPSPreviewDialogManager.CreatePreviewWindow: TdxPSCustomPreviewWindow;
begin
  Result := CurrentPreviewDialogStyleInfo.CreatePreviewWindow;
end;

function TdxPSPreviewDialogManager.FindPreviewDialogStyleInfoByName(
  const AName: string; out AInfoClass: TdxPSPreviewDialogStyleInfoClass): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Count - 1 do
  begin
    Result := SameText(Items[I].GetName, AName);
    if Result then
    begin
      AInfoClass := Items[I];
      Break;
    end;
  end;
end;

procedure TdxPSPreviewDialogManager.PopulatePreviewDialogList(AStrings: TStrings);
var
  I: Integer;
begin
  AStrings.BeginUpdate;
  try
    for I := 0 to Count - 1 do
      AStrings.AddObject(Items[I].GetName, TObject(Items[I]));
  finally
    AStrings.EndUpdate;
  end;
end;

procedure TdxPSPreviewDialogManager.RegisterPreviewDialog(
  APreviewDialogStyleInfo: TdxPSPreviewDialogStyleInfoClass);
begin
  if Self <> nil then
  begin
    if FList.IndexOf(APreviewDialogStyleInfo) < 0 then
      FList.Add(APreviewDialogStyleInfo);
  end;
end;

procedure TdxPSPreviewDialogManager.UnregisterPreviewDialog(
  APreviewDialogStyleInfo: TdxPSPreviewDialogStyleInfoClass);
begin
  if Self <> nil then
    FList.Remove(APreviewDialogStyleInfo);
end;

function TdxPSPreviewDialogManager.GetCurrentPreviewDialogStyleInfo: TdxPSPreviewDialogStyleInfoClass;
var
  AInfoClass: TdxPSPreviewDialogStyleInfoClass;
begin
  if FindPreviewDialogStyleInfoByName(CurrentPreviewDialogStyle, AInfoClass) then
    Result := AInfoClass
  else
    raise EdxComponentPrinter.Create(cxGetResourceString(@sdxPreviewNotRegistered));
end;

function TdxPSPreviewDialogManager.GetCurrentPreviewDialogStyle: TdxPSPreviewDialogStyle;
var
  AInfoClass: TdxPSPreviewDialogStyleInfoClass;
begin
  if not FindPreviewDialogStyleInfoByName(FCurrentPreviewDialogStyle, AInfoClass) then
    FCurrentPreviewDialogStyle := DefaultPreviewDialogStyleInfo.GetName;
  Result := FCurrentPreviewDialogStyle;
end;

function TdxPSPreviewDialogManager.GetDefaultPreviewDialogStyleInfo: TdxPSPreviewDialogStyleInfoClass;
begin
  if Count > 0 then
    Result := TdxPSPreviewDialogStyleInfoClass(Items[Count - 1])
  else
    raise EdxComponentPrinter.Create(cxGetResourceString(@sdxPreviewNotRegistered));
end;

function TdxPSPreviewDialogManager.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TdxPSPreviewDialogManager.GetItem(Index: Integer): TdxPSPreviewDialogStyleInfoClass;
begin
  Result := TdxPSPreviewDialogStyleInfoClass(FList.Items[Index]);
end;

function TdxPSPreviewDialogManager.GetName(Index: Integer): string;
begin
  Result := Items[Index].GetName;
end;

procedure TdxPSPreviewDialogManager.SetCurrentPreviewDialogStyle(
  const AValue: TdxPSPreviewDialogStyle);
var
  APreviewDialogStyleInfo: TdxPSPreviewDialogStyleInfoClass;
begin
  if not FindPreviewDialogStyleInfoByName(AValue, APreviewDialogStyleInfo) then
    APreviewDialogStyleInfo := DefaultPreviewDialogStyleInfo;
  FCurrentPreviewDialogStyle := APreviewDialogStyleInfo.GetName;
end;

{ TdxReportLinkPrinterPage }

constructor TdxReportLinkPrinterPage.Create(AReportLink: TBasedxReportLink);
begin
  inherited Create;
  FReportLink := AReportLink;
  PixelsPerInch := AReportLink.PixelsPerInch;
end;

procedure TdxReportLinkPrinterPage.ChangeScale(M: Integer; D: Integer);
begin
  inherited;
  PixelsPerInch := ReportLink.PixelsPerInch;
end;

function TdxReportLinkPrinterPage.GetOwner: TPersistent;
begin
  Result := ReportLink;
end;

function TdxReportLinkPrinterPage.GetCurrentDPI: Integer;
begin
  Result := ReportLink.ScaleFactor.TargetDPI;
end;

function TdxReportLinkPrinterPage.GetSupportsScaling: Boolean;
begin
  Result := ReportLink.SupportsScaling;
end;

procedure TdxReportLinkPrinterPage.PageParamsChanged(AUpdateCodes: TdxPrinterPageUpdateCodes);
begin
  inherited PageParamsChanged(AUpdateCodes);
  if UpdateCount = 0 then
    ReportLink.PageParamsChanged(Self, nil, AUpdateCodes);
end;

{ TdxPSPreviewDialogStyleInfo }

// note: TdxPSPreviewDialogStyleInfo - must be a abstract class
// CBuilder12: Unsupported language feature: 'abstract class method'
class function TdxPSPreviewDialogStyleInfo.CreatePreviewWindow: TdxPSCustomPreviewWindow;
begin
  Result := nil;
end;

class function TdxPSPreviewDialogStyleInfo.GetName: string;
begin
  Result := '';
end;

class function TdxPSPreviewDialogStyleInfo.GetUnitName: string;
begin
  Result := cxGetUnitName(Self);
end;

{ export }

procedure dxPSEnumReportPages(AComponentPrinter: TdxComponentPrinter;
  AReportLink: TBasedxReportLink; const APageIndexes: array of Integer;
  AGraphicClass: TGraphicClass; AnExportBackground: Boolean;
  ACallBackProc: TdxEnumPagesAsImagesProc;
  ACallBackData: Pointer;
  AProgressProc: TdxExportProgressEvent;
  AProgressData: Pointer;
  APrepareGraphicProc: TdxExportPrepareGraphicEvent;
  APrepareData: Pointer);

  procedure GetPPI(out APPI: TPoint);
  var
    DC: HDC;
  begin
    DC := GetDC(0);
    try
      APPI.X := GetDeviceCaps(DC, LOGPIXELSX);
      APPI.Y := GetDeviceCaps(DC, LOGPIXELSY);
    finally
      ReleaseDC(0, DC);
    end;
  end;

  function AreAllPagesEnumerated: Boolean;
  begin
    Result := (Low(APageIndexes) = High(APageIndexes)) and (APageIndexes[0] = -1);
  end;

  procedure GetPageBounds(out R: TRect);
  begin
    with AReportLink.RealPrinterPage.PageSizePixels do
      R := Rect(0, 0, X, Y);
  end;

  procedure PreparePageIndexes(out AnActualPageIndexes: TIntegers);
  var
    I: Integer;
  begin
    if AreAllPagesEnumerated then
    begin
      SetLength(AnActualPageIndexes, AReportLink.PageCount);
      for I := 0 to AReportLink.PageCount - 1 do
        AnActualPageIndexes[I] := I;
    end
    else
    begin
      SetLength(AnActualPageIndexes, Length(APageIndexes));
      for I := 0 to Length(APageIndexes) - 1 do
        AnActualPageIndexes[I] := APageIndexes[I];
    end;
  end;

  function CreatePageAsMetafile(const ABounds: TRect; const APPI: TPoint; APageIndex: Integer): TMetafile;
  var
    AMetaCanvas: TCanvas;
    APrevUnitsPerInch: Integer;
  begin
    Result := TMetafile.Create;
    try
      APrevUnitsPerInch := FUnitsPerInch;
      Result.Width := cxRectWidth(ABounds);
      Result.Height := cxRectHeight(ABounds);

      AMetaCanvas := TMetafileCanvas.Create(Result, 0);
      try
        if not FUseIsotropicMode then
        begin
          FUnitsPerInch := dxDefaultDPI;
          AReportLink.RebuildReport;
        end;
        if AGraphicClass.InheritsFrom(TMetafile) and Assigned(APrepareGraphicProc) then
          APrepareGraphicProc(AComponentPrinter, AReportLink, Result, APrepareData);

        if AnExportBackground and AReportLink.RealPrinterPage.Background.IsEmpty then
          AReportLink.RealPrinterPage.Background.Paint(AMetaCanvas, ABounds)
        else
        begin
          AMetaCanvas.Brush.Color := clWhite;
          AMetaCanvas.FillRect(ABounds);
        end;

        if (-1 < APageIndex) and (APageIndex < AReportLink.PageCount) then
          AComponentPrinter.PaintPage(AMetaCanvas, APageIndex, ABounds, ABounds, AReportLink);
      finally
        if not FUseIsotropicMode then
        begin
          FUnitsPerInch := APrevUnitsPerInch;
          AReportLink.RebuildReport;
        end;
        AMetaCanvas.Free;
      end;
    except
      Result.Free;
      raise;
    end;
  end;

  function CreatePageAsGraphic(const ABounds: TRect; const APPI: TPoint; APageIndex: Integer): TGraphic;
  var
    Metafile: TMetafile;
    Bitmap: TBitmap;
  begin
    Metafile := CreatePageAsMetafile(ABounds, APPI, APageIndex);
    try
      if not AGraphicClass.InheritsFrom(TMetafile) then
      begin
        Bitmap := TBitmap.Create;
        try
          Bitmap.Height := Metafile.Height;
          Bitmap.Width := Metafile.Width;
          if AGraphicClass.InheritsFrom(TBitmap) and Assigned(APrepareGraphicProc) then
            APrepareGraphicProc(AComponentPrinter, AReportLink, Bitmap, APrepareData);
          Bitmap.Canvas.Draw(0, 0, Metafile);

          if not AGraphicClass.InheritsFrom(TBitmap) then
          begin
            Result := dxPSUtl.CreateGraphic(AGraphicClass);
            try
              if Assigned(APrepareGraphicProc) then
                APrepareGraphicProc(AComponentPrinter, AReportLink, Result, APrepareData);
              Result.Assign(Bitmap);
            except
              Result.Free;
              raise;
            end;
          end
          else
            Result := Bitmap;
        finally
          if not AGraphicClass.InheritsFrom(TBitmap) then Bitmap.Free;
        end;
      end
      else
        Result := Metafile;
    finally
      if not AGraphicClass.InheritsFrom(TMetafile) then Metafile.Free;
    end;
  end;

var
  PPI: TPoint;
  PageCount, I, PageIndex: Integer;
  ActualPageIndexes: TIntegers;
  ContinueExport: Boolean;
  R: TRect;
  Graphic: TGraphic;
begin
  if (AComponentPrinter = nil) or (AReportLink = nil) or not Assigned(ACallBackProc) then
    Exit;

  if AGraphicClass = nil then AGraphicClass := TMetafile;

  GetPPI(PPI);
  PreparePageIndexes(ActualPageIndexes);
  try
    GetPageBounds(R);
    ContinueExport := True;
    PageCount := Length(ActualPageIndexes);
    for I := 0 to PageCount - 1 do
    begin
      PageIndex := ActualPageIndexes[I];
      Graphic := CreatePageAsGraphic(R, PPI, PageIndex);
      try
        ACallBackProc(AComponentPrinter, AReportLink, I, PageIndex, Graphic, ACallBackData, ContinueExport);
      finally
        Graphic.Free;
      end;
      if not ContinueExport then Break;
      if Assigned(AProgressProc) then
        AProgressProc(AComponentPrinter, AReportLink, PageCount, I, PageIndex, AProgressData);
    end;
  finally
     SetLength(ActualPageIndexes, 0);
  end;
end;

procedure InitializeUnits;
begin
  if IsWin9X then
    FUnitsPerInch := Min(FUnitsPerInch, 960);
  FPixelsNumerator := FUnitsPerInch;
  FPixelsDenominator := Screen.PixelsPerInch;
end;

procedure RegisterAssistants;
begin
  TdxPSExplorerTreeBuilder.Register;
  TdxPSRunTimeComponentsProvider.Register;

  TdxPSDataReader.Register;
  TdxPSDataWriter.Register;

  TdxPSCustomCellBorder.Register;
  TdxPSCellNullBorder.Register;
  TdxPSCellFlatBorder.Register;
  TdxPSCellBoldFlatBorder.Register;
  TdxPSCellUltraFlatBorder.Register;
  TdxPSCustomCell3DBorder.Register;
  TdxPSCellRaisedBorder.Register;
  TdxPSCellRaisedSoftBorder.Register;
  TdxPSCellSunkenBorder.Register;
  TdxPSCellSunkenSoftBorder.Register;
  TdxPSCellTwistedBorder.Register;
  TdxPSCellEtchedBorder.Register;
  TdxPSCellBumpedBorder.Register;
  TdxPSColorBorder.Register;

  TdxPSReportGroupNullLookAndFeel.Register;
  TdxPSReportGroupStandardLookAndFeel.Register;
  TdxPSReportGroupOfficeLookAndFeel.Register;
  TdxPSReportGroupWebLookAndFeel.Register;
  TdxPSReportGroupPanelStyleLookAndFeel.Register;
end;

procedure RegisterGraphicClasses;
const
  GraphicClassCount = 3 {$IFDEF USEJPEGIMAGE} + 1 {$ENDIF};
  GraphicClasses: array[0..GraphicClassCount - 1] of TGraphicClass =
    (TBitmap, TMetafile, TIcon{$IFDEF USEJPEGIMAGE}, TJPEGImage{$ENDIF});
var
  I: Integer;
  GraphicClass: TGraphicClass;
begin
  for I := Low(GraphicClasses) to High(GraphicClasses) do
  begin
    GraphicClass := GraphicClasses[I];
    if GetClass(GraphicClass.ClassName) = nil then
      RegisterClass(GraphicClass);
  end;
end;

procedure RegisterItems;
begin
  TdxReportCell.Register;
  TdxReportGroup.Register;
  TdxReportRadioGroup.Register;
  TdxReportCheckGroup.Register;

  TdxReportCellBox.Register;
  TdxReportCellString.Register;
  TdxReportCellCheck.Register;
  TdxReportCellRadio.Register;
  TdxReportCellCheckImage.Register;
  TdxReportCellRadioGroupButton.Register;
  TdxReportCellGroupButton.Register;
  TdxReportCellCheckGroupButton.Register;
  TdxReportCellImage.Register;
  TdxReportCellGraphic.Register;
  TdxReportCellDpiAwareGraphic.Register;
  TdxReportCellExpandButton.Register;
  TdxReportCellExpandButtonEx.Register;
end;

procedure UnregisterAssistants;
begin
  TdxPSDataWriter.Unregister;
  TdxPSDataReader.Unregister;

  TdxPSRunTimeComponentsProvider.Unregister;
  TdxPSExplorerTreeBuilder.Unregister;
end;

procedure UnregisterItems;
begin
  TdxReportCellExpandButtonEx.Unregister;
  TdxReportCellExpandButton.Unregister;
  TdxReportCellDpiAwareGraphic.Unregister;
  TdxReportCellGraphic.Unregister;
  TdxReportCellImage.Unregister;
  TdxReportCellCheckGroupButton.Unregister;
  TdxReportCellGroupButton.Unregister;
  TdxReportCellRadioGroupButton.Unregister;
  TdxReportCellCheckImage.Unregister;
  TdxReportCellRadio.Unregister;
  TdxReportCellCheck.Unregister;
  TdxReportCellString.Unregister;
  TdxReportCellBox.Unregister;

  TdxReportCheckGroup.Unregister;
  TdxReportRadioGroup.Unregister;
  TdxReportGroup.Unregister;
  TdxReportCell.Unregister;
end;

initialization
  FUnitFinalized := False;
  OleInitialize(nil);

  dxComponentPrintersList := TList.Create;
  dxPgsDlg.dxPSRegisterPrintStyle(TdxPSPrintStyle, True);
  dxPSRegisterReportLink(TdxCompositionReportLink, nil, TdxfmCompositionDesignWindow);

  InitializeUnits;
  RegisterAssistants;
  RegisterGraphicClasses;
  RegisterItems;

  GroupDescendentsWith(TCustomdxPSExplorer, TControl);

finalization
  FUnitFinalized := True;
  UnregisterItems;
  UnregisterAssistants;

  dxPSUnregisterAllReportLinks;
  dxPgsDlg.dxPSUnregisterPrintStyle(TdxPSPrintStyle);
  FreeAndNil(dxComponentPrintersList);
  FreeAndNil(FPreviewDialogManager);
  FreeAndNil(FActiveReportLinks);
  FreeAndNil(FExplorerImages);

  OleUninitialize;
end.
