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

unit dxPSdxPDFViewerLnk;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, Classes, Windows, Graphics, cxGeometry, dxCore, cxGraphics, dxPrnPg, dxPSReportRenderCanvas, dxPSCore, dxPrnDev,
  dxBase, dxPSGlbl, dxPgsDlg, dxPrnDlg, dxPDFCore, dxPDFCommandInterpreter, dxPDFDocument, dxPDFViewer, dxPDFFontUtils,
  dxPSdxPDFViewerPrintDialog;

type
  TdxPDFViewerReportLink = class;

  { TdxPDFRenderParameters }

  TdxPDFPrintParameters  = class(TdxPDFRenderParameters)
  public
    CenterOnPageH: Boolean;
    CenterOnPageV: Boolean;
    ContentBounds: TRect;
    DPI: Integer;
    FitToPageWidth: Boolean;
    UnitsPerInch: Integer;
  end;

  { TcxPDFViewerReportLinkPageRenderInfo }

  TcxPDFViewerReportLinkPageRenderInfo = class(TdxPSPageRenderInfo)
  strict private
    function GetUnitsPerInch: Integer;
    procedure CalculateScaleFactor;
  protected
    FPDFPage: TdxPDFPage;
    FRealPageSize: TPoint;
    FRotationAngle: TcxRotationAngle;
    FScaleFactor: Single;

    function CreatePrinterPage(ARenderInfo: TdxPSReportRenderInfo): TdxPrinterPage; override;
    function GetPageSize: TPoint; override;
  public
    procedure Calculate; override;
    procedure CalculateBounds; override;

    property UnitsPerInch: Integer read GetUnitsPerInch;
  end;

  { TdxPDFViewerReportLinkRenderInfo }

  TdxPDFViewerReportLinkRenderInfo = class(TdxPSReportRenderInfo)
  strict private
    FPageIndices: TIntegerDynArray;
    function GetPageOrientation(AInfo: TcxPDFViewerReportLinkPageRenderInfo): TdxPrinterOrientation;
    function GetReportLink: TdxPDFViewerReportLink;
    procedure CalculatePageIndices;
    procedure CreatePageRenderInfos;
  protected
    PageIndexesToPrint: TIntegerDynArray;
    IsPageLayoutChanged: Boolean;
    function GetPageColCount: Integer; override;
    function GetPageRenderInfoClass: TdxPSPageRenderInfoClass; override;
    function GetPageRowCount: Integer; override;
    function GetPageSize: TPoint; override;
    function GetUnitsPerInch: Integer; override;
    function GetWindowScalePair: TdxWindowScalePair; override;
    function LoMetricValueToInternalUnits(Value: Integer): Integer; override;
    procedure CalculatePageRenderInfos; override;
    procedure DoCalculate; override;
  public
    property ReportLink: TdxPDFViewerReportLink read GetReportLink;
  end;

  { TdxPDFViewerPrinterPage }

  TdxPDFViewerPrinterPage = class(TdxReportLinkPrinterPage)
  strict private
    FIsPrintAsImageAssigned: Boolean;
    FPrintAsImage: Boolean;
    procedure SetPrintAsImage(const AValue: Boolean);
  protected
    procedure DoAssign(Source: TdxBaseObject); override;
  public
    constructor Create(AReportLink: TBasedxReportLink); override;
  published
    property PrintAsImage: Boolean read FPrintAsImage write SetPrintAsImage default False;
  end;

  { TdxPDFViewerReportRenderer }

  TdxPDFViewerReportRenderer = class(TdxPSReportRenderer)
  private
    function GetDocument: TdxPDFDocument;
    function GetFontDataStorage: TdxPDFFontDataStorage;
    function GetImageDataStorage: TdxPDFDocumentImageDataStorage;
    function GetPageRenderInfo: TcxPDFViewerReportLinkPageRenderInfo;
    function GetPrinterPage: TdxPDFViewerPrinterPage;
    function GetRenderInfo: TdxPDFViewerReportLinkRenderInfo;
  protected
    procedure PrepareFont(AFont: TFont; AAdjustOnScale: Boolean); override;
    procedure PrepareRenderPage; override;
    procedure RenderPageContent; override;
    procedure UnprepareRenderPage; override;

    property Document: TdxPDFDocument read GetDocument;
    property FontDataStorage: TdxPDFFontDataStorage read GetFontDataStorage;
    property ImageDataStorage: TdxPDFDocumentImageDataStorage read GetImageDataStorage;
  public
    property PageRenderInfo: TcxPDFViewerReportLinkPageRenderInfo read GetPageRenderInfo;
    property PrinterPage: TdxPDFViewerPrinterPage read GetPrinterPage;
    property RenderInfo: TdxPDFViewerReportLinkRenderInfo read GetRenderInfo;
  end;

  { TdxPDFViewerReportLink }

  TdxPDFViewerReportLink = class(TBasedxReportLink)
  strict private
    FIsPreviewLocked: Boolean;
    FPrintDlgData: PdxPrintDlgData;
    FPrinted: Boolean;
    function GetControl: TdxPDFViewer;
    function GetDocument: TdxPDFDocument;
    procedure ClearPagesToPrint;
    procedure OnGetPrintingResultHandler(APrinted: Boolean);
  protected
    PagesToPrint: TIntegerDynArray;
    function CreatePreviewWindowForm: TdxPSCustomPreviewWindow; override;
    function CreatePrinterPage: TdxPrinterPage; override;
    function DoBeforeExportToPDF(const AFileName: string; AOptions: TdxPSPDFReportExportOptions): Boolean; override;
    function GetAlwaysBufferedGraphics: Boolean; override;
    function GetPageCount: Integer; override;
    function GetRendererClass: TdxPSReportRendererClass; override;
    function GetRenderInfoClass: TdxPSReportRenderInfoClass; override;
    function PrintDialog(var APrintDlgData: TdxPrintDlgData): Boolean; override;
    procedure Print(const APageIndexes: array of Integer); override;
    procedure AfterPrinting; override;
    procedure PageParamsChanged(Sender: TdxPrinterPage; AStyle: TBasedxPrintStyle;
      AUpdateCodes: TdxPrinterPageUpdateCodes); override;

    function GetPrinterPage: TdxPDFViewerPrinterPage; reintroduce;
    procedure SetPrinterPage(Value: TdxPDFViewerPrinterPage); reintroduce;

    property Control: TdxPDFViewer read GetControl;
    property Document: TdxPDFDocument read GetDocument;
    property PrintDlgData: PdxPrintDlgData read FPrintDlgData;
  public
    constructor Create(AOwner: TComponent); override;
    class function Aggregable: Boolean; override;
    class function CanBeUsedAsStub: Boolean; override;
    class function Serializable: Boolean; override;
    procedure Preview(Modal: Boolean = True); override;
  published
    property PrinterPage: TdxPDFViewerPrinterPage read GetPrinterPage write SetPrinterPage;
  end;

implementation

uses
  Math, Forms, SysUtils, dxTypeHelpers, cxClasses, dxPrinting, dxCoreGraphics, dxGDIPlusAPI, dxGDIPlusClasses, dxPSUtl,
  dxPDFTypes, dxPDFUtils;

type
  TdxComponentPrinterAccess = class(TdxComponentPrinter);
  TdxPDFDocumentAccess = class(TdxPDFDocument);
  TdxPDFPageNodeAccess = class(TdxPDFPage);
  TdxPDFPrintDialogAccess = class(TdxPDFPrintDialog);
  TdxPDFViewerAccess = class(TdxPDFCustomViewer);
  TdxfmPrintDialogAccess = class(TdxfmPrintDialog);

  { TdxPDFPrinting }

  TdxPDFPrinting = class(TdxPDFGraphicsDevice)
  strict private
    function CreateRenderParameters(APage: TdxPDFPage; AParameters: TdxPDFPrintParameters; var ARealBounds: TRect): TdxPDFPrintParameters;
  protected
    function CreateGraphics: TdxGPCanvas; override;
    procedure DestroyGraphics; override;
    procedure InitializeGraphics; override;
  public
    procedure Export(APage: TdxPDFPage; AParameters: TdxPDFExportParameters); override;
    procedure ExportAsImage(APage: TdxPDFPage; AParameters: TdxPDFExportParameters);
  end;

{ TdxPDFPrinting }

procedure TdxPDFPrinting.Export(APage: TdxPDFPage; AParameters: TdxPDFExportParameters);
var
  ARealBounds: TRect;
  ARenderParameters: TdxPDFRenderParameters;
begin
  ARenderParameters := CreateRenderParameters(APage, AParameters as TdxPDFPrintParameters, ARealBounds);
  try
    inherited Export(APage, ARenderParameters);
  finally
    ARenderParameters.Free;
  end;
end;

procedure TdxPDFPrinting.ExportAsImage(APage: TdxPDFPage; AParameters: TdxPDFExportParameters);
var
  ABitmap: TBitmap;
  ARealBounds, ADestRect: TRect;
  APrinterCanvas: TCanvas;
  ARenderParameters, APrintParameters: TdxPDFPrintParameters;
begin
  APrintParameters := AParameters as TdxPDFPrintParameters;
  ARenderParameters := CreateRenderParameters(APage, APrintParameters, ARealBounds) as TdxPDFPrintParameters;
  ABitmap := TBitmap.Create;
  ABitmap.Width := cxRectWidth(ARealBounds);
  ABitmap.Height := cxRectHeight(ARealBounds);
  APrinterCanvas := ARenderParameters.Canvas;
  ARenderParameters.Canvas := ABitmap.Canvas;
  ARenderParameters.Position := dxPointF(cxNullPoint);
  if APrintParameters.FitToPageWidth then
    ARenderParameters.ContentBounds := cxRectSetOrigin(ARenderParameters.ContentBounds, cxNullPoint);
  APrinterCanvas.Lock;
  try
    inherited Export(APage, ARenderParameters);
    ADestRect := cxRect(ARealBounds.TopLeft, ARealBounds.TopLeft);
    ADestRect := cxRectSetSize(ADestRect, ABitmap.Width, ABitmap.Height);
    cxBitBlt(APrinterCanvas.Handle, ABitmap.Canvas.Handle, ADestRect, cxNullPoint, SRCCOPY);
  finally
    APrinterCanvas.Unlock;
    ABitmap.Free;
    ARenderParameters.Free;
  end;
end;

function TdxPDFPrinting.CreateGraphics: TdxGPCanvas;
begin
  Result := TdxGPCanvas.Create(GetRenderParameters.Canvas.Handle);
end;

procedure TdxPDFPrinting.DestroyGraphics;
begin
  Graphics.Free;
end;

procedure TdxPDFPrinting.InitializeGraphics;
var
  AParams: TdxPDFPrintParameters;
begin
  inherited InitializeGraphics;
  if Graphics <> nil then
  begin
    AParams := GetRenderParameters as TdxPDFPrintParameters;
    Graphics.SetClipRect(cxRectScale(AParams.ContentBounds, AParams.DPI, AParams.UnitsPerInch), gmIntersect);
  end;
end;

function TdxPDFPrinting.CreateRenderParameters(APage: TdxPDFPage; AParameters: TdxPDFPrintParameters; var ARealBounds: TRect): TdxPDFPrintParameters;
var
  AZoomFactor: Single;
  W, H: Integer;
  APageSize: TdxPointF;
begin
  Result := TdxPDFPrintParameters.Create(AParameters.DocumentState);
  Result.Canvas := AParameters.Canvas;
  Result.Rect := AParameters.Rect;
  Result.ScaleFactor := AParameters.ScaleFactor;
  Result.Bounds := AParameters.Bounds;
  Result.Position := AParameters.Position;
  Result.ContentBounds := AParameters.ContentBounds;
  Result.UnitsPerInch := AParameters.UnitsPerInch;
  Result.DPI := AParameters.DPI;

  AZoomFactor := AParameters.DPI / cxGetCurrentDPI;

  Result.ScaleFactor := AParameters.ScaleFactor * AZoomFactor;
  Result.Position := cxPointF(AParameters.ContentBounds.Left * AZoomFactor, AParameters.ContentBounds.Top * AZoomFactor);

  APageSize := TdxPDFViewerViewState.CalculatePageSize(APage.Size, AParameters.DocumentState.RotationAngle);
  ARealBounds := cxRect(cxRectCenter(AParameters.ContentBounds), cxRectCenter(AParameters.ContentBounds));
  W := Trunc(APageSize.X * AParameters.ScaleFactor / 2);
  H := Trunc(APageSize.Y * AParameters.ScaleFactor / 2);
  ARealBounds := cxRectScale(cxRectInflate(ARealBounds, W, H), AParameters.DPI, cxGetCurrentDPI);

  if AParameters.CenterOnPageH then
    Result.Position.X := ARealBounds.Left;
  if AParameters.CenterOnPageV then
    Result.Position.Y := ARealBounds.Top;
end;

{ TcxPDFViewerReportLinkPageRenderInfo }

procedure TcxPDFViewerReportLinkPageRenderInfo.CalculateScaleFactor;
var
  ASize: TdxPOintF;
begin
  ASize := TdxPDFViewerViewState.CalculatePageSize(FPDFPage.Size, FRotationAngle);
  case PrinterPage.ScaleMode of
    smFit:
      FScaleFactor := Min(cxRectWidth(ContentBounds) /  ASize.X, cxRectHeight(ContentBounds) /  ASize.Y);
    smAdjust:
      FScaleFactor := Min(FRealPageSize.X /  ASize.X * PrinterPage.ScaleFactor / 100,
        FRealPageSize.Y /  ASize.Y * PrinterPage.ScaleFactor / 100)
  end;
end;

procedure TcxPDFViewerReportLinkPageRenderInfo.Calculate;
begin
  CalculateBounds;
  CalculateOffsets;
  CalculateScaleFactor;
  CalculatePageHeaderAndFooterBounds;
end;

procedure TcxPDFViewerReportLinkPageRenderInfo.CalculateBounds;

  function GetRealPageSize: TPoint;
  begin
    Result := PrinterPage.RealPageSizeLoMetric;
    Result.X := MulDiv(Result.X, UnitsPerInch, 254);
    Result.Y := MulDiv(Result.Y, UnitsPerInch, 254);
  end;

var
  ADetailsTwipsRect: TRect;
begin
  FRealPageSize := GetRealPageSize;
  ADetailsTwipsRect := cxRectScale(PrinterPage.PaintRectLoMetric, 1440, 254, 1440, 254);

  CalculateCompositionPartInfo;
  if IsCompositionPagePart then
  begin
    Inc(ADetailsTwipsRect.Top, MulDiv(PageOffset.Y, 1440, UnitsPerInch));
    Inc(ADetailsTwipsRect.Left, MulDiv(PageOffset.X, 1440, UnitsPerInch));
  end;
  if HasTitle then
    Inc(ADetailsTwipsRect.Top, MulDiv(TitleHeight, 1440, UnitsPerInch));
  if HasFootnotes then
    Dec(ADetailsTwipsRect.Bottom, MulDiv(FootnotesHeight, 1440, UnitsPerInch));

  DetailBounds := PrinterPage.PaintRectPixels;
  ContentBounds := cxRectScale(ADetailsTwipsRect, PixelsDenominator, 1440);
end;

function TcxPDFViewerReportLinkPageRenderInfo.GetPageSize: TPoint;
begin
  Result := FRealPageSize;
end;

function TcxPDFViewerReportLinkPageRenderInfo.CreatePrinterPage(ARenderInfo: TdxPSReportRenderInfo): TdxPrinterPage;
begin
  Result := TdxPDFViewerPrinterPage.Create(ARenderInfo.ReportLink);
end;

function TcxPDFViewerReportLinkPageRenderInfo.GetUnitsPerInch: Integer;
begin
  Result := RenderInfo.UnitsPerInch;
end;

{ TdxPDFViewerReportLinkRenderInfo }

procedure TdxPDFViewerReportLinkRenderInfo.CalculatePageIndices;
var
  I: Integer;
  ADocument: TdxPDFDocumentAccess;
begin
  if ReportLink.Control.IsDocumentLoaded then
  begin
    SetLength(FPageIndices, Length(ReportLink.PagesToPrint));
    TdxPDFUtils.CopyData(ReportLink.PagesToPrint, 0, FPageIndices, 0, Length(ReportLink.PagesToPrint));
    if Length(FPageIndices) = 0 then
    begin
      ADocument := TdxPDFDocumentAccess(ReportLink.Document);
      begin
        SetLength(FPageIndices, ADocument.PageCount);
        for I := 1 to ADocument.PageCount do
          FPageIndices[I - 1] := I;
      end;
    end;
  end;
end;

procedure TdxPDFViewerReportLinkRenderInfo.CreatePageRenderInfos;
var
  I: Integer;
  ADocument: TdxPDFDocumentAccess;
  ARenderInfo: TcxPDFViewerReportLinkPageRenderInfo;
begin
  ADocument := TdxPDFDocumentAccess(ReportLink.Document);
  SetLength(PageIndexesToPrint, Length(FPageIndices));
  for I := 0 to Length(FPageIndices) - 1 do
  begin
    ARenderInfo := CreatePageRenderInfo(VirtualPageCount) as TcxPDFViewerReportLinkPageRenderInfo;
    ARenderInfo.FRotationAngle := ReportLink.Control.RotationAngle;
    ARenderInfo.FPDFPage := ADocument.Pages[FPageIndices[I] - 1];
    ARenderInfo.PrinterPage.Orientation := GetPageOrientation(ARenderInfo);
    PageIndexesToPrint[I] := I + 1;
    Inc(VirtualPageCount);
  end;
end;

procedure TdxPDFViewerReportLinkRenderInfo.CalculatePageRenderInfos;
var
  I: Integer;
begin
  for I := 0 to VirtualPageCount - 1 do
    PageRenderInfos[I].Calculate;
end;

procedure TdxPDFViewerReportLinkRenderInfo.DoCalculate;
begin
  IsPageLayoutChanged := False;
  CalculatePageIndices;
  CreatePageRenderInfos;
  CalculateTitleBounds;
  CalculateFootnotesBounds;
  CalculateHeaderAndFooterBounds;
  CalculatePageRenderInfos;
end;

function TdxPDFViewerReportLinkRenderInfo.GetPageColCount: Integer;
begin
  Result := 1;
end;

function TdxPDFViewerReportLinkRenderInfo.GetPageRenderInfoClass: TdxPSPageRenderInfoClass;
begin
  Result := TcxPDFViewerReportLinkPageRenderInfo;
end;

function TdxPDFViewerReportLinkRenderInfo.GetPageRowCount: Integer;
begin
  Result := VirtualPageCount;
end;

function TdxPDFViewerReportLinkRenderInfo.GetPageSize: TPoint;
begin
  Result := (PageRenderInfos[ReportLink.Renderer.RenderingPageIndex] as TcxPDFViewerReportLinkPageRenderInfo).PageSize;
end;

function TdxPDFViewerReportLinkRenderInfo.GetUnitsPerInch: Integer;
begin
  Result := Screen.PixelsPerInch;
end;

function TdxPDFViewerReportLinkRenderInfo.GetWindowScalePair: TdxWindowScalePair;
begin
  Result.Numerator := 100;
  Result.Denominator := 100;
end;

function TdxPDFViewerReportLinkRenderInfo.LoMetricValueToInternalUnits(Value: Integer): Integer;
begin
  Result := MulDiv(Value, UnitsPerInch, 254);
end;

function TdxPDFViewerReportLinkRenderInfo.GetPageOrientation(AInfo: TcxPDFViewerReportLinkPageRenderInfo): TdxPrinterOrientation;
var
  ASize: TdxPointF;
begin
  if ReportLink.PrinterPage.Orientation = poAuto then
  begin
    ASize := TdxPDFViewerViewState.CalculatePageSize(AInfo.FPDFPage.Size, AInfo.FRotationAngle);
    if ASize.X > ASize.Y then
      Result := poLandscape
    else
      Result := poPortrait;
  end
  else
    Result := PrinterPage.Orientation;
end;

function TdxPDFViewerReportLinkRenderInfo.GetReportLink: TdxPDFViewerReportLink;
begin
  Result := TdxPDFViewerReportLink(inherited ReportLink);
end;

{ TdxPDFViewerReportLink }

constructor TdxPDFViewerReportLink.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  PrinterPage.BeginUpdate;
  try
    PrinterPage.CenterOnPageH := True;
    PrinterPage.CenterOnPageV := True;
    PrinterPage.Orientation := poAuto;
    PrinterPage.ScaleFactor := 100;
    PrinterPage.ScaleMode := smFit;
    PrinterPage.GrayShading := False;
    PrinterPage.DMPaper := dxPrnDev.dxPrintDevice.DefaultDMPaper;
    PrinterPage.Header := 0;
    PrinterPage.Footer := 0;
    PrinterPage.MinMargins.Rect := cxNullRect;
    PrinterPage.Margins.Rect := cxNullRect;
    PrinterPage.FixMarginsOutside;
  finally
    PrinterPage.EndUpdate;
  end;
end;

class function TdxPDFViewerReportLink.Aggregable: Boolean;
begin
  Result := False;
end;

class function TdxPDFViewerReportLink.CanBeUsedAsStub: Boolean;
begin
  Result := False;
end;

class function TdxPDFViewerReportLink.Serializable: Boolean;
begin
  Result := False;
end;

procedure TdxPDFViewerReportLink.Preview(Modal: Boolean = True);
begin
  if not FIsPreviewLocked then
    inherited Preview(Modal);
end;

function TdxPDFViewerReportLink.GetAlwaysBufferedGraphics: Boolean;
begin
  Result := False;
end;

function TdxPDFViewerReportLink.GetRendererClass: TdxPSReportRendererClass;
begin
  Result := TdxPDFViewerReportRenderer;
end;

function TdxPDFViewerReportLink.GetRenderInfoClass: TdxPSReportRenderInfoClass;
begin
  Result := TdxPDFViewerReportLinkRenderInfo;
end;

procedure TdxPDFViewerReportLink.PageParamsChanged(Sender: TdxPrinterPage; AStyle: TBasedxPrintStyle;
  AUpdateCodes: TdxPrinterPageUpdateCodes);
var
  APrevOrientation: TdxPrinterOrientation;
begin
  if not (rlsPagination in State) then
  begin
    if IsRealPrinterPage(Sender) then
      inherited
    else
    begin
      APrevOrientation := RealPrinterPage.Orientation;
      RealPrinterPage.BeginUpdate;
      try
        RealPrinterPage.Orientation := Sender.Orientation;
        RealPrinterPage.Margins.Assign(Sender.Margins);
        RealPrinterPage.Orientation := APrevOrientation;
      finally
        RealPrinterPage.EndUpdate;
      end;
    end;
  end;
end;

procedure TdxPDFViewerReportLink.AfterPrinting;
begin
  inherited AfterPrinting;
  if TdxPDFViewerReportLinkRenderInfo(RenderInfo).IsPageLayoutChanged then
  begin
    CalculateRenderInfos;
    if ComponentPrinter.PreviewExists then
      ComponentPrinter.PreviewWindow.InvalidateAllPages;
  end;
  ClearPagesToPrint;
end;

function TdxPDFViewerReportLink.PrintDialog(var APrintDlgData: TdxPrintDlgData): Boolean;
begin
  try
    FPrinted := False;
    FIsPreviewLocked := False;
    FPrintDlgData := @APrintDlgData;
    ComponentPrinter.Preview(True, Self);
    FIsPreviewLocked := True;
    Result := FPrinted;
    APrintDlgData.PreviewBtnClicked := Result;
    FPrintDlgData := nil;
  finally
    ClearPagesToPrint;
  end;
end;

procedure TdxPDFViewerReportLink.Print(const APageIndexes: array of Integer);
var
  APages: string;
  APrintDlgData: TdxPrintDlgData;
begin
  if IsComponentPrinterAvailable then
  begin
    TdxComponentPrinterAccess(ComponentPrinter).InitializeDefaultPrintDlgData(Self, APrintDlgData);
    try
      if Length(APageIndexes) > 0 then
      begin
        APages := TdxPDFUtils.ConvertToPageRanges(TIntegerDynArray(@APageIndexes[0]));
        if APages <> '' then
        begin
          APrintDlgData.DialogData.PageRanges := prRange;
          APrintDlgData.DialogData.Pages := APages;
        end;
      end;
      Print(True, @APrintDlgData);
    finally
      FreeAndNil(APrintDlgData.DialogData.FileList);
      ClearPagesToPrint;
    end;
  end;
end;

function TdxPDFViewerReportLink.CreatePreviewWindowForm: TdxPSCustomPreviewWindow;
var
  ADialog: TdxPDFPrintDialog;
begin
  ADialog := TdxPDFPrintDialog.Create(nil);
  TdxPDFPrintDialogAccess(ADialog).PrintDlgData := FPrintDlgData;
  TdxPDFPrintDialogAccess(ADialog).OnGetPrintingResult := OnGetPrintingResultHandler;
  Result := ADialog.Preview;
end;

function TdxPDFViewerReportLink.CreatePrinterPage: TdxPrinterPage;
begin
  Result := TdxPDFViewerPrinterPage.Create(Self);
end;

function TdxPDFViewerReportLink.DoBeforeExportToPDF(const AFileName: string;
  AOptions: TdxPSPDFReportExportOptions): Boolean;
begin
  Result := False;
end;

function TdxPDFViewerReportLink.GetPageCount: Integer;
begin
  if Control <> nil then
  begin
    if Length(PagesToPrint) = 0 then
      Result := Control.PageCount
    else
      Result := Length(PagesToPrint);
  end
  else
    Result := 0;
end;

function TdxPDFViewerReportLink.GetControl: TdxPDFViewer;
begin
  Result := TdxPDFViewer(Component);
end;

function TdxPDFViewerReportLink.GetDocument: TdxPDFDocument;
begin
  Result := Control.Document;
end;

procedure TdxPDFViewerReportLink.ClearPagesToPrint;
begin
  SetLength(PagesToPrint, 0);
end;

function TdxPDFViewerReportLink.GetPrinterPage: TdxPDFViewerPrinterPage;
begin
  Result := inherited GetPrinterPage as TdxPDFViewerPrinterPage;
end;

procedure TdxPDFViewerReportLink.SetPrinterPage(Value: TdxPDFViewerPrinterPage);
begin
  inherited SetPrinterPage(Value);
end;

procedure TdxPDFViewerReportLink.OnGetPrintingResultHandler(APrinted: Boolean);
begin
  FPrinted := APrinted;
end;

{ TdxPDFViewerPrinterPage }

constructor TdxPDFViewerPrinterPage.Create(AReportLink: TBasedxReportLink);
begin
  inherited Create(AReportLink);
  FPrintAsImage := False;
end;

procedure TdxPDFViewerPrinterPage.DoAssign(Source: TdxBaseObject);
begin
  inherited DoAssign(Source);
  if not FIsPrintAsImageAssigned then
    FPrintAsImage := TdxPDFViewerPrinterPage(Source).PrintAsImage;
end;

procedure TdxPDFViewerPrinterPage.SetPrintAsImage(const AValue: Boolean);
begin
  if FPrintAsImage <> AValue then
  begin
    FPrintAsImage := AValue;
    FIsPrintAsImageAssigned := True;
  end;
end;

{ TdxPDFViewerReportRenderer }

procedure TdxPDFViewerReportRenderer.UnprepareRenderPage;
begin
  inherited UnprepareRenderPage;
  RenderInfo.FootnotesBounds := cxRectScale(RenderInfo.FootnotesBounds, UnitsPerInch, PixelsPerInch);
  RenderInfo.TitleBounds := cxRectScale(RenderInfo.TitleBounds, UnitsPerInch, PixelsPerInch);
  PageRenderInfo.FRealPageSize.X := MulDiv(PageRenderInfo.FRealPageSize.X, UnitsPerInch, PixelsPerInch);
  PageRenderInfo.FRealPageSize.Y := MulDiv(PageRenderInfo.FRealPageSize.Y, UnitsPerInch, PixelsPerInch);
  PageRenderInfo.TitleOffset := ScalePoint(PageRenderInfo.TitleOffset, UnitsPerInch, PixelsPerInch);
  PageRenderInfo.FootnotesOffset := ScalePoint(PageRenderInfo.FootnotesOffset, UnitsPerInch, PixelsPerInch);
  PageRenderInfo.PageHeaderBounds := cxRectScale(PageRenderInfo.PageHeaderBounds, UnitsPerInch, PixelsPerInch);
  PageRenderInfo.PageFooterBounds := cxRectScale(PageRenderInfo.PageFooterBounds, UnitsPerInch, PixelsPerInch);
end;

procedure TdxPDFViewerReportRenderer.PrepareFont(AFont: TFont; AAdjustOnScale: Boolean);
begin
  if Canvas.IsPrinterCanvas then
    dxPSScaleFont(AFont, AFont.Size, PixelsPerInch)
  else
    inherited PrepareFont(AFont, AAdjustOnScale);
end;

procedure TdxPDFViewerReportRenderer.PrepareRenderPage;
begin
  RenderInfo.TitleBounds := cxRectScale(RenderInfo.TitleBounds, PixelsPerInch, UnitsPerInch);
  RenderInfo.FootnotesBounds := cxRectScale(RenderInfo.FootnotesBounds, PixelsPerInch, UnitsPerInch);
  PageRenderInfo.FRealPageSize.X := MulDiv(PageRenderInfo.FRealPageSize.X, PixelsPerInch, UnitsPerInch);
  PageRenderInfo.FRealPageSize.Y := MulDiv(PageRenderInfo.FRealPageSize.Y, PixelsPerInch, UnitsPerInch);
  PageRenderInfo.TitleOffset := ScalePoint(PageRenderInfo.TitleOffset, PixelsPerInch, UnitsPerInch);
  PageRenderInfo.FootnotesOffset := ScalePoint(PageRenderInfo.FootnotesOffset, PixelsPerInch, UnitsPerInch);
  PageRenderInfo.PageHeaderBounds := cxRectScale(PageRenderInfo.PageHeaderBounds, PixelsPerInch, UnitsPerInch);
  PageRenderInfo.PageFooterBounds := cxRectScale(PageRenderInfo.PageFooterBounds, PixelsPerInch, UnitsPerInch);
  inherited PrepareRenderPage;
end;

procedure TdxPDFViewerReportRenderer.RenderPageContent;

  function NeedPrintAsImage: Boolean;
  begin
    Result := IsPrinting and (PrinterPage.PrintAsImage or ReportLink.PrinterPage.GrayShading);
  end;

var
  ADevice: TdxPDFPrinting;
  AParameters: TdxPDFPrintParameters;
  ARenderCanvas: TdxPSReportRenderCanvas;
begin
  ARenderCanvas := Canvas as TdxPSReportRenderCanvas;
  if ARenderCanvas <> nil then
  begin
    AParameters := TdxPDFPrintParameters.Create(TdxPDFDocumentAccess(Document).State);
    AParameters.DPI := Canvas.PixelsPerInch;
    AParameters.ScaleFactor := PageRenderInfo.FScaleFactor;
    AParameters.Canvas := ARenderCanvas.Canvas.Canvas;
    AParameters.ContentBounds := PageRenderInfo.ContentBounds;
    AParameters.UnitsPerInch := UnitsPerInch;
    AParameters.CenterOnPageH := PageRenderInfo.PrinterPage.CenterOnPageH;
    AParameters.CenterOnPageV := PageRenderInfo.PrinterPage.CenterOnPageV;
    AParameters.FitToPageWidth := ReportLink.ShrinkToPageWidth;
    ADevice := TdxPDFPrinting.Create;
    try
      if NeedPrintAsImage then
      begin
        if ReportLink.ShrinkToPageWidth then
          AParameters.ContentBounds := cxRectSetOrigin(AParameters.ContentBounds, cxNullPoint);
        ADevice.ExportAsImage(PageRenderInfo.FPDFPage, AParameters)
      end
      else
        ADevice.Export(PageRenderInfo.FPDFPage, AParameters);
    finally
      AParameters.Free;
      ADevice.Free;
      RenderInfo.IsPageLayoutChanged := True;
    end;
  end;
end;

function TdxPDFViewerReportRenderer.GetDocument: TdxPDFDocument;
begin
  Result := (ReportLink as TdxPDFViewerReportLink).Document;
end;

function TdxPDFViewerReportRenderer.GetFontDataStorage: TdxPDFFontDataStorage;
begin
  Result := TdxPDFDocumentAccess(Document).State.FontDataStorage;
end;

function TdxPDFViewerReportRenderer.GetImageDataStorage: TdxPDFDocumentImageDataStorage;
begin
  Result := TdxPDFDocumentAccess(Document).State.ImageDataStorage;
end;

function TdxPDFViewerReportRenderer.GetPageRenderInfo: TcxPDFViewerReportLinkPageRenderInfo;
begin
  Result := inherited PageRenderInfo as TcxPDFViewerReportLinkPageRenderInfo;
end;

function TdxPDFViewerReportRenderer.GetPrinterPage: TdxPDFViewerPrinterPage;
begin
  Result := ReportLink.PrinterPage as TdxPDFViewerPrinterPage;
end;

function TdxPDFViewerReportRenderer.GetRenderInfo: TdxPDFViewerReportLinkRenderInfo;
begin
  Result := inherited RenderInfo as TdxPDFViewerReportLinkRenderInfo;
end;

initialization
  dxPSRegisterReportLink(TdxPDFViewerReportLink, TdxPDFViewer, nil);

finalization
  dxPSUnregisterReportLink(TdxPDFViewerReportLink, TdxPDFViewer, nil);

end.

