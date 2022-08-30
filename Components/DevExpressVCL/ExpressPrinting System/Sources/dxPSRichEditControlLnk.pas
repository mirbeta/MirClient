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

unit dxPSRichEditControlLnk;

interface

{$I cxVer.inc}

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, Classes, Windows, Graphics, Controls, StdCtrls, Generics.Defaults, Generics.Collections,
  cxGeometry,
  dxCore, dxPSCore, dxPSGlbl, dxPrnPg, dxPSRes, dxBase, dxPSFillPatterns, dxPSReportRenderCanvas, dxCustomPreview, dxPreVw, dxPSPrVw,

  dxRichEdit.Control,
  dxRichEdit.Printing,
  dxPSRichEditControlLnk.Printing,
  dxPSRichEditControlLnk.Bricks,
  dxRichEdit.DocumentModel.Boxes,
  dxRichEdit.DocumentModel.Section,
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.View.Core,
  dxRichEdit.Types;

type
  TdxRichEditControlReportLink = class;

  { TdxRichEditControlReportLinkPageInfo }

  TdxRichEditControlReportLinkPageInfo = record
    Footer: Integer;
    Header: Integer;
    Height: Integer;
    Landscape: Boolean;
    Margins: TRect;
    DPI: Integer;
    Width: Integer;

    constructor Create(APage: TdxPage; ADPI: Integer);
    procedure ApplyTo(APrinterPage: TdxPrinterPage);
  end;

  { TcxRichEditControlReportLinkPageRenderInfo }

  TcxRichEditControlReportLinkPageRenderInfo = class(TdxPSPageRenderInfo)
  protected
    procedure CalculateBounds; override;
    procedure CalculateOffsets; override;
    function GetPaintSize: TPoint; override;
  public
    constructor Create(ARenderInfo: TdxPSReportRenderInfo; APageIndex: Integer); override;
  end;

  { TcxRichEditControlReportLinkRenderInfo }

  TcxRichEditControlReportLinkRenderInfo = class(TdxPSReportRenderInfo)
  strict private
    function GetReportLink: TdxRichEditControlReportLink;
  protected
    FPageColCount: Integer;
    FPageRowCount: Integer;

    function CalculatePageContentHeight(APageIndex: Integer): Integer; override;
    function CalculatePageContentWidth(APageIndex: Integer): Integer; override;
    procedure DoCalculate; override;
    function GetPageColCount: Integer; override;
    function GetPageRenderInfoClass: TdxPSPageRenderInfoClass; override;
    function GetPageRowCount: Integer; override;
    function GetPaintSize: TPoint; override;
    function IsLoading: Boolean;
  public
    function CalculateActualScaleFactor: Integer; override;
    function IsDrawPageFootnotesOnPage(APageIndex: Integer): Boolean; override;
    function IsDrawPageTitleOnPage(APageIndex: Integer): Boolean; override;
    //
    property ReportLink: TdxRichEditControlReportLink read GetReportLink;
  end;

  { TdxRichEditControlReportLink }

  TdxRichEditControlReportLink = class(TBasedxReportLink,
    IdxPSPreviewMeasurementUnitsProvider)
  strict private
    FDocumentDPI: Integer;
    FDocumentPageCount: Integer;
    FPages: TList<TdxRichEditControlReportLinkPageInfo>;
    FPrintable: TdxRichEditControlPrinter;
    FVerticalDelimiters: TList;

    function GetControl: TdxCustomRichEditControl;
  protected
    procedure ConvertCoords; override;
    procedure PrepareFonts(UPI: Integer); override;

    procedure ConvertDelimiters(ADelimiters: TList; ANumerator, ADenominator: Integer);
    function GetRenderInfoClass: TdxPSReportRenderInfoClass; override;

    function GetAlwaysBufferedGraphics: Boolean; override;
    function GetCapabilities: TdxReportLinkCapabilities; override;
    function GetCriticalSize(AReportCells: TdxReportCells): Integer; override;
    procedure ConstructReport(AReportCells: TdxReportCells); override;
    procedure MakeDelimiters(AReportCells: TdxReportCells; AHorzDelimiters, AVertDelimiters: TList); override;

    procedure AfterCreateDocument(AReportCells: TdxReportCells); virtual;
    procedure BeforeCreateDocument(AReportCells: TdxReportCells); virtual;
    procedure CreateDocument; virtual;
    // IdxPSPreviewMeasurementUnitsProvider
    function GetMeasurementUnits: TdxPreviewMeasurementUnits;

    property Control: TdxCustomRichEditControl read GetControl;
    property DocumentDPI: Integer read FDocumentDPI;
    property DocumentPageCount: Integer read FDocumentPageCount;
    property Pages: TList<TdxRichEditControlReportLinkPageInfo> read FPages;
    property Printable: TdxRichEditControlPrinter read FPrintable;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function AddPage(APage: TdxPage): TdxOfficeBrick;
    procedure AddVerticalDelimiter(Y: Integer);
    function CanLoadData: Boolean; override;
    class function Aggregable: Boolean; override;
    class function GetCellsBounds(AParent: TdxReportCell): TRect;
    function IsApplyBackgroundToEntirePage: Boolean; override;
    class function Serializable: Boolean; override;

    property PageHeight;
    property PageWidth;
  end;

implementation

uses
  Forms, SysUtils, Math,
  cxClasses, dxPSUtl, dxPSImgs, cxGraphics, dxTypeHelpers, cxDrawTextUtils, dxPrnDev, dxRichEdit.Commands.Strs,
  dxRichEdit.Commands.Images;

type
  TdxPSReportRenderInfoAccess = class(TdxPSReportRenderInfo);
  TdxReportCellsAccess = class(TdxReportCells);

{ TdxRichEditControlReportLinkPageInfo }

constructor TdxRichEditControlReportLinkPageInfo.Create(APage: TdxPage; ADPI: Integer);
begin
  Landscape := APage.Areas.First.Section.Page.Landscape;
  DPI := ADPI;

  Width := APage.Bounds.Width;
  Height := APage.Bounds.Height;
  if Landscape then
    ExchangeLongWords(Height, Width);

  Margins := Rect(
    APage.ClientBounds.Left - APage.Bounds.Left,
    APage.ClientBounds.Top - APage.Bounds.Top,
    APage.Bounds.Right - APage.ClientBounds.Right,
    APage.Bounds.Bottom - APage.ClientBounds.Bottom);

  if APage.Footer <> nil then
    Footer := APage.Footer.Bounds.Height
  else
    Footer := 0;

  if APage.Header <> nil then
    Header := APage.Header.Bounds.Height
  else
    Header := 0;
end;

procedure TdxRichEditControlReportLinkPageInfo.ApplyTo(APrinterPage: TdxPrinterPage);

  function ConvertValue(AValue: Integer): Integer; overload;
  begin
    if APrinterPage.RealMeasurementUnits = muInches then
      Result := MulDiv(AValue, 1000, DPI)
    else
      Result := MulDiv(AValue, 25400, DPI);
  end;

  function ConvertValue(const R: TRect): TRect; overload;
  begin
    Result := Rect(ConvertValue(R.Left), ConvertValue(R.Top), ConvertValue(R.Right), ConvertValue(R.Bottom));
  end;

const
  OrientationMap: array[Boolean] of TdxPrinterOrientation = (poPortrait, poLandscape);
begin
  APrinterPage.BeginUpdate;
  try
    APrinterPage.PageSize.Point := Point(ConvertValue(Width), ConvertValue(Height));
    APrinterPage.Orientation := OrientationMap[Landscape];
    APrinterPage.Margins.Rect := ConvertValue(Margins);
    APrinterPage.Header := ConvertValue(Header);
    APrinterPage.Footer := ConvertValue(Footer);
    APrinterPage.FixMarginsOutside;
  finally
    APrinterPage.EndUpdate;
  end;
end;

{ TcxRichEditControlReportLinkPageRenderInfo }

constructor TcxRichEditControlReportLinkPageRenderInfo.Create(ARenderInfo: TdxPSReportRenderInfo; APageIndex: Integer);
begin
  inherited Create(ARenderInfo, APageIndex);
  TdxRichEditControlReportLink(ReportLink).Pages[APageIndex].ApplyTo(PrinterPage);
end;

procedure TcxRichEditControlReportLinkPageRenderInfo.CalculateBounds;
var
  AContentHeight: Integer;
begin
  DetailBounds := cxRectSetSize(TdxPSReportRenderInfoAccess(RenderInfo).
    CalculatePageDetailBounds(ColIndex, RowIndex), PageSize.X, PageSize.Y);

  ContentBounds := DetailBounds;
  Inc(ContentBounds.Bottom, cxRectHeight(HeaderBounds));
  Inc(ContentBounds.Bottom, cxRectHeight(FooterBounds));
  Inc(ContentBounds.Right, cxRectWidth(RowHeaderBounds));

  AContentHeight := MulDiv(PaintSize.Y, 100, RenderInfo.ScaleFactor);
  if HasTitle then
    Dec(AContentHeight, TitleHeight);
  if HasFootnotes then
    Dec(AContentHeight, FootnotesHeight);

  ContentBounds.Bottom := ContentBounds.Top + Min(AContentHeight, cxRectHeight(ContentBounds));
end;

procedure TcxRichEditControlReportLinkPageRenderInfo.CalculateOffsets;
begin
  DataOffset := PageOffset;
end;

function TcxRichEditControlReportLinkPageRenderInfo.GetPaintSize: TPoint;
begin
  Result := PageSize;
end;

{ TcxRichEditControlReportLinkRenderInfo }

function TcxRichEditControlReportLinkRenderInfo.CalculatePageContentHeight(APageIndex: Integer): Integer;
begin
  Result := ReportCells.Cells[APageIndex].Height;
end;

function TcxRichEditControlReportLinkRenderInfo.CalculatePageContentWidth(APageIndex: Integer): Integer;
begin
  Result := ReportCells.Cells[APageIndex].Width;
end;

procedure TcxRichEditControlReportLinkRenderInfo.DoCalculate;
var
  I: Integer;
begin
  if not IsLoading then
  begin
    if ReportLink <> nil then
    begin
      FPageColCount := 1;
      FPageRowCount := ReportLink.DocumentPageCount;
    end
    else
    begin
      FPageColCount := 1;
      FPageRowCount := 1;
    end;
  end;

  if (ReportLink = nil) or (ReportLink.ReportCells <> nil) and (ReportLink.ReportCells.Count > 0) then
  begin
    inherited DoCalculate;
    for I := 0 to PageRenderInfoCount - 1 do
    begin
      PageRenderInfos[I].CalculateTitleOffset;
      PageRenderInfos[I].CalculateFootnotesOffset;
    end;
  end;
end;

function TcxRichEditControlReportLinkRenderInfo.GetPageColCount: Integer;
begin
  if IsLoading then
    Result := inherited GetPageColCount
  else
    Result := 1;
end;

function TcxRichEditControlReportLinkRenderInfo.GetPageRenderInfoClass: TdxPSPageRenderInfoClass;
begin
  Result := TcxRichEditControlReportLinkPageRenderInfo;
end;

function TcxRichEditControlReportLinkRenderInfo.GetPageRowCount: Integer;
begin
  if IsLoading then
    Result := inherited GetPageRowCount
  else
    Result := FPageRowCount;
end;

function TcxRichEditControlReportLinkRenderInfo.GetPaintSize: TPoint;
begin
  Result := GetPageSize;
end;

function TcxRichEditControlReportLinkRenderInfo.GetReportLink: TdxRichEditControlReportLink;
begin
  Result := TdxRichEditControlReportLink(inherited ReportLink);
end;

function TcxRichEditControlReportLinkRenderInfo.CalculateActualScaleFactor: Integer;
begin
  Result := 100;
end;

function TcxRichEditControlReportLinkRenderInfo.IsDrawPageFootnotesOnPage(APageIndex: Integer): Boolean;
begin
  Result := False;
end;

function TcxRichEditControlReportLinkRenderInfo.IsDrawPageTitleOnPage(APageIndex: Integer): Boolean;
begin
  Result := False;
end;

function TcxRichEditControlReportLinkRenderInfo.IsLoading: Boolean;
begin
  Result := ReportLink.DataSource = rldsExternalStorage;
end;

{ TdxRichEditControlReportLink }

constructor TdxRichEditControlReportLink.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FVerticalDelimiters := TList.Create;
  FPages := TList<TdxRichEditControlReportLinkPageInfo>.Create;
end;

destructor TdxRichEditControlReportLink.Destroy;
begin
  FreeAndNil(FVerticalDelimiters);
  FreeAndNil(FPages);
  inherited Destroy;
end;

function TdxRichEditControlReportLink.AddPage(APage: TdxPage): TdxOfficeBrick;
var
  ABounds: TRect;
begin
  FPages.Add(TdxRichEditControlReportLinkPageInfo.Create(APage, Printable.DocumentDPI));

  ABounds := APage.Bounds;
  if ReportCells.Cells.CellCount > 0 then
    ABounds := cxRectOffset(ABounds, 0, ReportCells.Cells.LastCell.Bottom);

  Result := TdxOfficeBrick.Create(ReportCells.Cells);
  Result.BoundsRect := ABounds;
  Result.Transparent := True;
  Result.ClipChildren := False;
  AddVerticalDelimiter(ABounds.Top);
  AddVerticalDelimiter(ABounds.Bottom);
end;

procedure TdxRichEditControlReportLink.AddVerticalDelimiter(Y: Integer);
begin
  FVerticalDelimiters.Add(Pointer(Y));
end;

function TdxRichEditControlReportLink.CanLoadData: Boolean;
begin
  Result := False;
end;

class function TdxRichEditControlReportLink.Aggregable: Boolean;
begin
  Result := False;
end;

function TdxRichEditControlReportLink.GetCapabilities: TdxReportLinkCapabilities;
begin
  Result := inherited GetCapabilities - [rlcPageSetup, rlcHeaderFooter, rlcTitle, rlcFootnotes];
end;

class function TdxRichEditControlReportLink.GetCellsBounds(AParent: TdxReportCell): TRect;

  procedure Extend(var R: TRect; const R1: TRect);
  begin
    R.Left := Min(R.Left, R1.Left);
    R.Top := Min(R.Top, R1.Top);
    R.Right := Max(R.Right, R1.Right);
    R.Bottom := Max(R.Bottom, R1.Bottom);
  end;

var
  I: Integer;
begin
  if (AParent.CellCount = 0) and (AParent.DataItemCount = 0) then
    Exit(TRect.Null);

  Result.Init(MaxInt, MaxInt, MinInt, MinInt);
  for I := 0 to AParent.CellCount - 1 do
    Extend(Result, AParent.Cells[I].BoundsRect);
  for I := 0 to AParent.DataItemCount - 1 do
    Extend(Result, AParent.DataItems[I].BoundsRect);
end;

class function TdxRichEditControlReportLink.Serializable: Boolean;
begin
  Result := False;
end;

procedure TdxRichEditControlReportLink.ConvertCoords;
begin
  TdxReportCellsAccess(ReportCells).ConvertCoords(PixelsNumerator, DocumentDPI);
  ConvertDelimiters(FVerticalDelimiters, PixelsNumerator, DocumentDPI);
end;

procedure TdxRichEditControlReportLink.PrepareFonts(UPI: Integer);
begin
  inherited PrepareFonts(MulDiv(UPI, PixelsPerInch, DocumentDPI));
end;

procedure TdxRichEditControlReportLink.ConvertDelimiters(ADelimiters: TList; ANumerator, ADenominator: Integer);
var
  I: Integer;
begin
  for I := 0 to ADelimiters.Count - 1 do
    ADelimiters[I] := Pointer(MulDiv(Integer(ADelimiters[I]), ANumerator, ADenominator));
end;

function TdxRichEditControlReportLink.GetRenderInfoClass: TdxPSReportRenderInfoClass;
begin
  Result := TcxRichEditControlReportLinkRenderInfo;
end;

function TdxRichEditControlReportLink.GetAlwaysBufferedGraphics: Boolean;
begin
  Result := False;
end;

function TdxRichEditControlReportLink.GetCriticalSize(AReportCells: TdxReportCells): Integer;
begin
  Result := Max(cxRectWidth(AReportCells.BoundsRect), cxRectHeight(AReportCells.BoundsRect));
end;

procedure TdxRichEditControlReportLink.ConstructReport(AReportCells: TdxReportCells);
begin
  inherited ConstructReport(AReportCells);

  BeforeCreateDocument(AReportCells);
  try
    CreateDocument;
  finally
    AfterCreateDocument(AReportCells);
  end;
end;

function TdxRichEditControlReportLink.IsApplyBackgroundToEntirePage: Boolean;
begin
  Result := (Control <> nil) and Control.Options.Printing.EnablePageBackgroundOnPrint
end;

procedure TdxRichEditControlReportLink.MakeDelimiters(AReportCells: TdxReportCells; AHorzDelimiters, AVertDelimiters: TList);
begin
  inherited MakeDelimiters(AReportCells, AHorzDelimiters, AVertDelimiters);
  AVertDelimiters.Assign(FVerticalDelimiters, laOr);
end;

procedure TdxRichEditControlReportLink.AfterCreateDocument(AReportCells: TdxReportCells);
begin
  AReportCells.Cells.BoundsRect := GetCellsBounds(AReportCells.Cells);
  AReportCells.Cells.CellSides := [];
  AReportCells.Cells.ClipChildren := False;
  FDocumentPageCount := Printable.PageCount;
  FDocumentDPI := Printable.DocumentDPI;
  FPrintable.Finalize(Self);
  FreeAndNil(FPrintable);
end;

procedure TdxRichEditControlReportLink.BeforeCreateDocument(AReportCells: TdxReportCells);
begin
  if Control <> nil then
    Control.EnsureImagesLoadComplete;

  AReportCells.Cells.CellSides := [];
  FPrintable := TdxRichEditControlPrinter.Create(Control.InnerControl);
  FPrintable.Initialize(Self);
  FVerticalDelimiters.Clear;
  FPages.Clear;
end;

procedure TdxRichEditControlReportLink.CreateDocument;
begin
  FPrintable.CreateArea(Self);
  ReportCells.DoProgress(100);
end;

function TdxRichEditControlReportLink.GetMeasurementUnits: TdxPreviewMeasurementUnits;
begin
  case Control.MeasurementUnit of
    TdxMeasurementUnit.Document, TdxMeasurementUnit.Inch:
      Result := pmuInches;
    TdxMeasurementUnit.Millimeter:
      Result := pmuMillimeters;
    TdxMeasurementUnit.Centimeter:
      Result := pmuCentimeters;
    TdxMeasurementUnit.Point:
      Result := pmuPoints;
  else
    Result := pmuDefault;
  end;
end;

function TdxRichEditControlReportLink.GetControl: TdxCustomRichEditControl;
begin
  Result := TdxCustomRichEditControl(Component);
end;

initialization
  dxPSRegisterReportLink(TdxRichEditControlReportLink, TdxRichEditControl, nil);

finalization
  dxPSUnregisterReportLink(TdxRichEditControlReportLink, TdxRichEditControl, nil);
end.

