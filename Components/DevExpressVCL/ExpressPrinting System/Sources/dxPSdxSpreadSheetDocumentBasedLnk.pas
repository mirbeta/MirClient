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

unit dxPSdxSpreadSheetDocumentBasedLnk;

interface

{$I cxVer.inc}

uses
  Windows, Types, Classes, Controls, Graphics, Generics.Collections, Generics.Defaults,
  dxCore, dxPSBaseGridLnk, dxPSCore, dxPSReportRenderCanvas, dxPSdxSpreadSheetLnkCore, dxPSEdgePatterns,
  dxSpreadSheetCore, dxSpreadSheetPrinting, dxSpreadSheetCoreStyles, dxSpreadSheetTypes, dxSpreadSheetGraphics,
  dxSpreadSheetContainers, dxPSGlbl, dxPrnPg, dxPgsDlg, dxPSHFLibrary, dxSpreadSheet;

type
  TdxSpreadSheetDocumentBasedReportLink = class;

  { TdxSpreadSheetMacroConverter }

  TdxSpreadSheetMacroConverter = class(TdxSpreadSheetAbstractHeaderFooterMacroExpander)
  strict private
    class var FPSToExcel: TDictionary<string, string>;
  protected
    class var FReportLink: TdxSpreadSheetDocumentBasedReportLink;

    class procedure CheckIsEncoderReady;
    class function EvalFuncDate(const S: string; var AIndex: Integer; ALength: Integer): string; override;
    class function EvalFuncPageNumber(const S: string; var AIndex: Integer; ALength: Integer): string; override;
    class function EvalFuncPages(const S: string; var AIndex: Integer; ALength: Integer): string; override;
    class function EvalFuncSheetName(const S: string; var AIndex: Integer; ALength: Integer): string; override;
    class function EvalFuncTime(const S: string; var AIndex: Integer; ALength: Integer): string; override;
    class function EncodeCore(const S: string): string;
    class function EncodeFont: string;
    class function GetDefinition(const FunctionClass: TdxHFCustomFunctionClass): string;
  public
    class procedure Decode(AReportLink: TdxSpreadSheetDocumentBasedReportLink;
      ATarget: TCustomdxPageObject; ASource: TdxSpreadSheetTableViewOptionsPrintHeaderFooterText);
    class procedure Encode(AReportLink: TdxSpreadSheetDocumentBasedReportLink;
      ATarget: TdxSpreadSheetTableViewOptionsPrintHeaderFooterText; ASource: TCustomdxPageObject);
    class procedure Finalize;
  end;

  { TdxSpreadSheetDocumentBasedReportLinkCommentProducer }

  TdxSpreadSheetDocumentBasedReportLinkCommentProducer = class
  strict private
    FHost: TdxReportCell;
    FInitialized: Boolean;
    FReportCells: TdxReportCells;
    FReportLink: TdxSpreadSheetDocumentBasedReportLink;

    function GetScreenCanvas: TdxPSReportRenderCustomCanvas;
  protected
    FFontBoldIndex: Integer;
    FFontIndex: Integer;

    procedure Finalize; virtual;
    procedure Initialize; virtual;
    procedure SetupBoundsRect(ACell: TdxReportCell);
  public
    constructor Create(AReportLink: TdxSpreadSheetDocumentBasedReportLink; AReportCells: TdxReportCells);
    destructor Destroy; override;
    procedure CheckInitialized;
    procedure Produce(AComment: TdxSpreadSheetCommentContainer);
    //
    property Host: TdxReportCell read FHost;
    property ReportCells: TdxReportCells read FReportCells;
    property ReportLink: TdxSpreadSheetDocumentBasedReportLink read FReportLink;
    property ScreenCanvas: TdxPSReportRenderCustomCanvas read GetScreenCanvas;
  end;

  { TdxSpreadSheetDocumentBasedReportLink }

  TdxSpreadSheetDocumentBasedReportLink = class(TdxSpreadSheetAbstractReportLink,
    IdxSpreadSheetListener,
    IdxSpreadSheetOptionsPrintListener)
  strict private
    FIsPrintOptionsLocked: Boolean;

    function GetOptions: TdxSpreadSheetTableViewOptionsPrint;
    procedure OffsetChildren(AHost: TdxReportCell; const AOffset: TPoint);
  protected
    FColumnsToRepeatArea: TRect;
    FPrintArea: TRect;
    FRowsToRepeatArea: TRect;
    FSelectionRect: TRect;

    procedure CalculatePrintAreas; virtual;
    function GetBorderStylePatternClass(AStyle: TdxSpreadSheetCellBorderStyle): TdxPSEdgePatternClass; override;
    function GetBreakPagesByHardDelimiters: Boolean; override;
    function GetCellTextCore(ACell: TdxSpreadSheetCell): string; override;
    function GetDefaultCellBorderColor: TColor; override;
    function GetSelectionRect: TRect; override;
    function GetSourceCellColor(ACol: Integer; ARow: Integer): TColor; override;
    function GetSourceCellContentBkColor(ACol: Integer; ARow: Integer): TColor; override;
    function GetSourceColWidth(ACol: Integer): Integer; override;
    function GetSourceRowHeight(ARow: Integer): Integer; override;
    function GetUseHardHorzDelimiters: Boolean; override;
    function GetUseHardVertDelimiters: Boolean; override;
    function HasSelection: Boolean; override;
    function IsDraftQuality: Boolean;
    function IsDrawBorder: Boolean; override;
    function IsDrawHeaderCornersOnEveryPage: Boolean; override;
    function IsDrawHeadersOnEveryPage: Boolean; override;
    function IsDrawRowHeadersOnEveryPage: Boolean; override;
    function IsShowGridLines: Boolean; override;
    function IsShowRowAndColumnHeadings: Boolean; override;
    function IsSuppressFontColors: Boolean; override;
    function IsSuppressSourceFormats: Boolean; override;
    function UseGrayScaleForGraphics: Boolean; override;

    function GetRebuildOnPageParamsChange(AUpdateCodes: TdxPrinterPageUpdateCodes): Boolean; override;
    procedure MakeDelimiters(AReportCells: TdxReportCells; AHorzDelimiters, AVertDelimiters: TList); override;
    procedure PrepareBuildReport; override;
    procedure PrepareConstruct(AReportCells: TdxReportCells); override;
    procedure SetComponent(Value: TComponent); override;
    procedure UnprepareConstruct(AReportCells: TdxReportCells); override;

    // HeadersOnEveryPage
    procedure ConstructHeadersOnEveryPage; overload; virtual;
    function ConstructHeadersOnEveryPage(AHost: TdxReportCell; AArea: TRect): Boolean; overload; virtual;
    // Containers
    procedure AddContainers(AReportCells: TdxReportCells); override;
    function CanPrintContainer(AContainer: TdxSpreadSheetContainer): Boolean; override;
    procedure InitializeContainerCell(ACell: TdxReportCell; AViewInfo: TdxSpreadSheetContainerViewInfo); override;
    // Comments
    procedure AddComments(AReportCells: TdxReportCells); virtual;
    function CreateCommentsProducer(AReportCells: TdxReportCells): TdxSpreadSheetDocumentBasedReportLinkCommentProducer; virtual;
    // Render
    function GetRenderInfoClass: TdxPSReportRenderInfoClass; override;

    // PrinterPage
    procedure AssignOptionsToPrinterPage(APage: TdxPrinterPage);
    procedure AssignOptionsToPrinterPageCore(APage: TdxPrinterPage); virtual;
    procedure AssignPrinterPageToOptions(APage: TdxPrinterPage);
    procedure AssignPrinterPageToOptionsCore(APage: TdxPrinterPage); virtual;
    function GetStartPageIndex: Integer; override;
    procedure SetStartPageIndex(Value: Integer); override;
    procedure PageParamsChanged(Sender: TdxPrinterPage; AStyle: TBasedxPrintStyle; AUpdateCodes: TdxPrinterPageUpdateCodes); override;

    // IdxSpreadSheetListener
    procedure DataChanged(Sender: TdxCustomSpreadSheet);
    // IdxSpreadSheetOptionsPrintListener
    procedure OptionsChanged(Sender: TdxSpreadSheetCustomView);
    //
    property Options: TdxSpreadSheetTableViewOptionsPrint read GetOptions;
  public
    procedure AddHorizontalPageBreak(AColumn: Integer); override;
    procedure AddPageBreak(ARow: Integer); override;
  end;

  { TdxSpreadSheetDocumentBasedReportRenderInfo }

  TdxSpreadSheetDocumentBasedReportRenderInfo = class(TdxPSReportRenderInfo)
  strict private
    function AdjustFitToPagesVerticallyValue(AValue: Integer): Integer;
    function GetReportLink: TdxSpreadSheetDocumentBasedReportLink;
  protected
    function GetPageRenderInfoClass: TdxPSPageRenderInfoClass; override;
    function IsReportFitsToPages(APrinterPage: TdxPrinterPage): Boolean; override;
  public
    function CalculateActualScaleFactor(AFitToPageHorizontally, AFitToPageVertically, AReportDPI: Integer): Integer; override;
    function HasCommentsPage: Boolean; virtual;
    function IsCommentsPage(APageIndex: Integer): Boolean; virtual;
    function IsHeaderOnPage(APageIndex: Integer): Boolean; override;
    function IsRowHeaderOnPage(APageIndex: Integer): Boolean; override;
    //
    property ReportLink: TdxSpreadSheetDocumentBasedReportLink read GetReportLink;
  end;

  { TdxSpreadSheetDocumentBasedReportPageRenderInfo }

  TdxSpreadSheetDocumentBasedReportPageRenderInfo = class(TdxPSPageRenderInfo)
  strict private
    function GetRenderInfo: TdxSpreadSheetDocumentBasedReportRenderInfo;
    function GetReportLink: TdxSpreadSheetDocumentBasedReportLink;
  protected
    procedure CalculateHeadersBounds; override;
  public
    function HasHeader: Boolean; override;
    function HasHeaderCorner: Boolean; override;
    function HasRowHeader: Boolean; override;
    function IsCommentsPage: Boolean;
    function IsLeftPage: Boolean; virtual;
    //
    property RenderInfo: TdxSpreadSheetDocumentBasedReportRenderInfo read GetRenderInfo;
    property ReportLink: TdxSpreadSheetDocumentBasedReportLink read GetReportLink;
  end;

implementation

uses
  dxPSExcelEdgePatterns, dxPSExcelFillPatterns, cxGeometry, dxSpreadSheetUtils, dxSpreadSheetDialogStrs,
  dxCoreGraphics, dxSpreadSheetStrs, cxDrawTextUtils, Math, SysUtils, dxPrnDev, dxPrintUtils, dxMeasurementUnits,
  dxStringHelper, dxPSUtl, StrUtils, cxGraphics, dxPSdxSpreadSheetLnk;

type
  TControlAccess = class(TControl);
  TdxPrinterPageAccess = class(TdxPrinterPage);
  TdxSpreadSheetCellAccess = class(TdxSpreadSheetCell);
  TdxSpreadSheetTableViewAccess = class(TdxSpreadSheetTableView);
  TCustomdxPageObjectAccess = class(TCustomdxPageObject);

{ TdxSpreadSheetMacroConverter }

class procedure TdxSpreadSheetMacroConverter.Finalize;
begin
  FreeAndNil(FPSToExcel);
end;

class procedure TdxSpreadSheetMacroConverter.Decode(AReportLink: TdxSpreadSheetDocumentBasedReportLink;
  ATarget: TCustomdxPageObject; ASource: TdxSpreadSheetTableViewOptionsPrintHeaderFooterText);
begin
  ATarget.BeginUpdate;
  try
    FReportLink := AReportLink;
    try
      ATarget.LeftTitle.Text := EvaluateCore(ASource.LeftSection);
      ATarget.CenterTitle.Text := EvaluateCore(ASource.CenterSection);
      ATarget.RightTitle.Text := EvaluateCore(ASource.RightSection);
    finally
      FReportLink := nil;
    end;
  finally
    ATarget.EndUpdate;
  end;
end;

class procedure TdxSpreadSheetMacroConverter.CheckIsEncoderReady;
begin
  if FPSToExcel = nil then
  begin
    FPSToExcel := TDictionary<string, string>.Create;
    FPSToExcel.Add('&', '&&');
    FPSToExcel.Add(GetDefinition(TdxHFDateFunction), '&D');
    FPSToExcel.Add(GetDefinition(TdxHFDateTimeFunction), '&D &T');
    FPSToExcel.Add(GetDefinition(TdxHFMachineNameFunction), GetMachineName);
    FPSToExcel.Add(GetDefinition(TdxHFPageNumberFunction), '&P');
    FPSToExcel.Add(GetDefinition(TdxHFPageOfPagesFunction), '&P of &N');
    FPSToExcel.Add(GetDefinition(TdxHFTimeFunction), '&T');
    FPSToExcel.Add(GetDefinition(TdxHFTotalPagesFunction), '&N');
    FPSToExcel.Add(GetDefinition(TdxHFUserNameFunction), dxGetUserName);
  end;
end;

class procedure TdxSpreadSheetMacroConverter.Encode(AReportLink: TdxSpreadSheetDocumentBasedReportLink;
  ATarget: TdxSpreadSheetTableViewOptionsPrintHeaderFooterText; ASource: TCustomdxPageObject);

  function CheckEncode(const Old, New: string): string;
  begin
    if TrimRight(EvaluateCore(Old)) <> TrimRight(New) then
      Result := EncodeCore(New)
    else
      Result := Old;
  end;

begin
  FReportLink := AReportLink;
  try
    ATarget.LeftSection := CheckEncode(ATarget.LeftSection, ASource.LeftTitle.Text);
    ATarget.CenterSection := CheckEncode(ATarget.CenterSection, ASource.CenterTitle.Text);
    ATarget.RightSection := CheckEncode(ATarget.RightSection, ASource.RightTitle.Text);
    ATarget.Assigned := (ATarget.LeftSection <> '') and (ATarget.CenterSection <> '') and (ATarget.RightSection <> '');
  finally
    FReportLink := nil;
  end;
end;

class function TdxSpreadSheetMacroConverter.EncodeCore(const S: string): string;
var
  AExpander: TdxHFTemplateExpander;
  AFuncInfo: TdxHFFunctionPlaceInfo;
  AValue: string;
begin
  CheckIsEncoderReady;

  AExpander := TdxHFTemplateExpander.Create(StringReplace(S, '&', '&&', [rfReplaceAll]));
  try
    while AExpander.GetFuncInfo(AFuncInfo) do
    begin
      if not FPSToExcel.TryGetValue(AFuncInfo.Name, AValue) then
        AValue := '';
      AExpander.Replace(AFuncInfo, AValue, False);
    end;
    Result := EncodeFont + AExpander.Str;
  finally
    AExpander.Free;
  end;
end;

class function TdxSpreadSheetMacroConverter.EncodeFont: string;
//var
//  ABuffer: TStringBuilder;
begin
  Result := '';
end;

class function TdxSpreadSheetMacroConverter.EvalFuncPageNumber(const S: string; var AIndex: Integer; ALength: Integer): string;
begin
  Result := GetDefinition(TdxHFPageNumberFunction);
end;

class function TdxSpreadSheetMacroConverter.EvalFuncPages(const S: string; var AIndex: Integer; ALength: Integer): string;
begin
  Result := GetDefinition(TdxHFTotalPagesFunction);
end;

class function TdxSpreadSheetMacroConverter.EvalFuncDate(const S: string; var AIndex: Integer; ALength: Integer): string;
begin
  Result := GetDefinition(TdxHFDateFunction);
end;

class function TdxSpreadSheetMacroConverter.EvalFuncTime(const S: string; var AIndex: Integer; ALength: Integer): string;
begin
  Result := GetDefinition(TdxHFTimeFunction);
end;

class function TdxSpreadSheetMacroConverter.EvalFuncSheetName(const S: string; var AIndex: Integer; ALength: Integer): string;
begin
  Result := FReportLink.Sheet.Caption;
end;

class function TdxSpreadSheetMacroConverter.GetDefinition(const FunctionClass: TdxHFCustomFunctionClass): string;
begin
  Result := dxFunctionDelimiters[False] + FunctionClass.DefaultTemplateStringValue + dxFunctionDelimiters[True];
end;

{ TdxSpreadSheetDocumentBasedReportLinkCommentProducer }

constructor TdxSpreadSheetDocumentBasedReportLinkCommentProducer.Create(
  AReportLink: TdxSpreadSheetDocumentBasedReportLink; AReportCells: TdxReportCells);
begin
  inherited Create;
  FReportLink := AReportLink;
  FReportCells := AReportCells;
end;

destructor TdxSpreadSheetDocumentBasedReportLinkCommentProducer.Destroy;
begin
  if FInitialized then
    Finalize;
  inherited;
end;

procedure TdxSpreadSheetDocumentBasedReportLinkCommentProducer.Finalize;
begin
  SetupBoundsRect(Host.Parent);
  FReportCells.Cells.Bottom := Host.Parent.Bottom;
end;

procedure TdxSpreadSheetDocumentBasedReportLinkCommentProducer.CheckInitialized;
begin
  if not FInitialized then
  begin
    FInitialized := True;
    Initialize;
  end;
end;

procedure TdxSpreadSheetDocumentBasedReportLinkCommentProducer.Initialize;
var
  AFont: TFont;
begin
  AFont := TFont.Create;
  try
    AFont.PixelsPerInch := ReportLink.PixelsPerInch;
    ReportLink.SpreadSheet.DefaultCellStyle.Font.AssignToFont(AFont);
    AFont.Style := [];
    FFontIndex := ReportLink.AddFontToPool(AFont);
    AFont.Style := [fsBold];
    FFontBoldIndex := ReportLink.AddFontToPool(AFont);
  finally
    AFont.Free;
  end;

  SetupBoundsRect(ReportCells.Cells);

  FHost := TdxReportCell.Create(ReportCells.AddOverlay);
  FHost.CellSides := [];
  FHost.Transparent := True;
  FHost.BoundsRect := cxRectSetTop(ReportCells.BoundsRect, ReportCells.BoundsRect.Bottom, 0);

  ReportLink.AddVerticalHardDelimiter(FHost);
end;

procedure TdxSpreadSheetDocumentBasedReportLinkCommentProducer.Produce(AComment: TdxSpreadSheetCommentContainer);

  function CreateString(AHost: TdxReportCell; const AText: string; AFontIndex: Integer): TdxReportCellString;
  begin
    Result := TdxReportCellString(AHost.AddDataItem(TdxReportCellString));
    Result.CellSides := [];
    Result.Transparent := True;
    Result.FontIndex := AFontIndex;
    Result.TextAlignY := taTop;
    Result.Text := AText;
  end;

  function CreateHeader(AHost: TdxReportCell; const AText: string): TdxReportCellString;
  begin
    Result := CreateString(AHost, AText, FFontBoldIndex);
    Result.TextAlignX := taRight;
  end;

  function CreateContent(AHost: TdxReportCell; const AText: string): TdxReportCellString;
  begin
    Result := CreateString(AHost, AText, FFontIndex);
    Result.Multiline := True;
  end;

var
  AHeaderWidth: Integer;
  ALine1Content: TdxReportCellString;
  ALine1Header: TdxReportCellString;
  ALine2Content: TdxReportCellString;
  ALine2Header: TdxReportCellString;
  AOffset: Integer;
begin
  CheckInitialized;

  ALine1Header := CreateHeader(Host, cxGetResourceString(@sdxCell));
  ALine2Header := CreateHeader(Host, cxGetResourceString(@sdxComment));
  ALine1Content := CreateContent(Host, AComment.Cell.GetReference);
  ALine2Content := CreateContent(Host, AComment.TextBox.TextAsString);

  AOffset := Host.Height;
  AHeaderWidth := Max(ALine1Header.MeasureWidth(ScreenCanvas), ALine2Header.MeasureWidth(ScreenCanvas));
  ALine1Header.BoundsRect := cxRectBounds(0, AOffset, AHeaderWidth, ALine1Header.MeasureHeight(ScreenCanvas));
  ALine1Content.BoundsRect := cxRectBounds(AHeaderWidth, AOffset, Host.Width - AHeaderWidth, 0);
  ALine1Content.Height := ALine1Content.MeasureHeight(ScreenCanvas);
  Inc(AOffset, ALine1Content.Height);

  ALine2Header.BoundsRect := cxRectBounds(0, AOffset, AHeaderWidth, ALine2Header.MeasureHeight(ScreenCanvas));
  ALine2Content.BoundsRect := cxRectBounds(AHeaderWidth, AOffset, Host.Width - AHeaderWidth, 0);
  ALine2Content.Height := ALine2Content.MeasureHeight(ScreenCanvas);
  Inc(AOffset, ALine2Content.Height);

  Inc(AOffset, ALine2Header.Height);

  Host.Height := AOffset;
end;

procedure TdxSpreadSheetDocumentBasedReportLinkCommentProducer.SetupBoundsRect(ACell: TdxReportCell);
begin
  ReportLink.SetupBoundsRect(ACell);
end;

function TdxSpreadSheetDocumentBasedReportLinkCommentProducer.GetScreenCanvas: TdxPSReportRenderCustomCanvas;
begin
  Result := ReportLink.ScreenCanvas;
end;

{ TdxSpreadSheetDocumentBasedReportLink }

procedure TdxSpreadSheetDocumentBasedReportLink.AddHorizontalPageBreak(AColumn: Integer);
var
  ACell: TdxReportCellSSString;
  I: Integer;
begin
  if FGridAdapter.RowCount > 0 then
    for I := 0 to FGridAdapter.ColCount - 1 do
    begin
      ACell := FGridAdapter.Cells[I, 0];
      if ACell.RealCol = AColumn then
      begin
        AddHorizontalHardDelimiter(ACell.AbsoluteOrigin.X);
        Break;
      end;
    end;
end;

procedure TdxSpreadSheetDocumentBasedReportLink.AddPageBreak(ARow: Integer);
var
  ACell: TdxReportCellSSString;
  I: Integer;
begin
  for I := 0 to FGridAdapter.RowCount - 1 do
  begin
    ACell := FGridAdapter.Cells[0, I];
    if ACell.RealRow = ARow then
    begin
      AddVerticalHardDelimiter(ACell.AbsoluteOrigin.Y);
      Break;
    end;
  end;
end;

procedure TdxSpreadSheetDocumentBasedReportLink.CalculatePrintAreas;
begin
  FPrintArea := Sheet.PrintArea;
  if dxSpreadSheetIsEntireRow(FPrintArea) then
    FPrintArea.Right := Sheet.Dimensions.Right;
  if dxSpreadSheetIsEntireColumn(FPrintArea) then
    FPrintArea.Bottom := Sheet.Dimensions.Bottom;
  FPrintArea := SpreadSheetAreaToReportArea(FPrintArea);
  FSelectionRect := FPrintArea;

  if IsDrawHeadersOnEveryPage then
  begin
    FRowsToRepeatArea := SpreadSheetAreaToReportArea(Options.Source.RowsToRepeat.Rect);
    FSelectionRect.Top := Min(FSelectionRect.Top, FRowsToRepeatArea.Top);
    FSelectionRect.Bottom := Max(FSelectionRect.Bottom, FRowsToRepeatArea.Bottom);
  end
  else
    FRowsToRepeatArea := cxInvalidRect;

  if IsDrawRowHeadersOnEveryPage then
  begin
    FColumnsToRepeatArea := SpreadSheetAreaToReportArea(Options.Source.ColumnsToRepeat.Rect);
    FSelectionRect.Left := Min(FSelectionRect.Left, FColumnsToRepeatArea.Left);
    FSelectionRect.Right := Max(FSelectionRect.Right, FColumnsToRepeatArea.Right);
  end
  else
    FColumnsToRepeatArea := cxInvalidRect;
end;

function TdxSpreadSheetDocumentBasedReportLink.GetBorderStylePatternClass(
  AStyle: TdxSpreadSheetCellBorderStyle): TdxPSEdgePatternClass;
begin
//  if AStyle = sscbsDefault then
//    Result := TdxPSDottedEdgePattern
//  else
    Result := inherited;
end;

function TdxSpreadSheetDocumentBasedReportLink.GetBreakPagesByHardDelimiters: Boolean;
begin
  Result := True;
end;

function TdxSpreadSheetDocumentBasedReportLink.GetCellTextCore(ACell: TdxSpreadSheetCell): string;
begin
  if TdxSpreadSheetCellAccess(ACell).ActualDataType = cdtError then
    case Options.Source.ActualErrorIndication of
      pseiBlank:
        Exit(sdxErrorIndicationBlank);
      pseiDash:
        Exit(sdxErrorIndicationDash);
      pseiNA:
        Exit(sdxErrorIndicationNA);
    end;

  Result := inherited;
end;

function TdxSpreadSheetDocumentBasedReportLink.GetSelectionRect: TRect;
begin
  Result := FSelectionRect;
end;

function TdxSpreadSheetDocumentBasedReportLink.GetSourceCellColor(ACol, ARow: Integer): TColor;
begin
  if IsSuppressSourceFormats then
    Result := clWindow
  else
    Result := inherited;
end;

function TdxSpreadSheetDocumentBasedReportLink.GetSourceCellContentBkColor(ACol, ARow: Integer): TColor;
begin
  if IsSuppressSourceFormats then
    Result := clWindow
  else
    Result := inherited;
end;

function TdxSpreadSheetDocumentBasedReportLink.GetSourceColWidth(ACol: Integer): Integer;

  function IsInPrintArea(C: Integer): Boolean;
  begin
    Result := dxSpreadSheetContainsColumn(FPrintArea, C) or dxSpreadSheetContainsColumn(FColumnsToRepeatArea, C);
  end;

begin
  if HasSelection and not IsInPrintArea(ACol) and not IsFixedCol(ACol) then
    Result := 0
  else
    Result := inherited;
end;

function TdxSpreadSheetDocumentBasedReportLink.GetSourceRowHeight(ARow: Integer): Integer;

  function IsInPrintArea(R: Integer): Boolean;
  begin
    Result := dxSpreadSheetContainsRow(FPrintArea, R) or dxSpreadSheetContainsRow(FRowsToRepeatArea, R);
  end;

begin
  if HasSelection and not IsInPrintArea(ARow) and not IsFixedRow(ARow) then
    Result := 0
  else
    Result := inherited;
end;

function TdxSpreadSheetDocumentBasedReportLink.GetUseHardHorzDelimiters: Boolean;
begin
  Result := True;
end;

function TdxSpreadSheetDocumentBasedReportLink.GetUseHardVertDelimiters: Boolean;
begin
  Result := True;
end;

function TdxSpreadSheetDocumentBasedReportLink.HasSelection: Boolean;
begin
  Result := Sheet.OptionsPrint.Source.Area.Assigned;
end;

function TdxSpreadSheetDocumentBasedReportLink.GetDefaultCellBorderColor: TColor;
begin
  Result := TdxSpreadSheetTableViewAccess(Sheet).ViewInfo.GridLineColor;
end;

function TdxSpreadSheetDocumentBasedReportLink.IsDraftQuality: Boolean;
begin
  Result := Options.Printing.ActualDraft;
end;

function TdxSpreadSheetDocumentBasedReportLink.IsDrawBorder: Boolean;
begin
  Result := (IsShowGridLines or IsShowRowAndColumnHeadings) and not IsDraftQuality;
end;

function TdxSpreadSheetDocumentBasedReportLink.IsDrawHeaderCornersOnEveryPage: Boolean;
begin
  Result := IsDrawRowHeadersOnEveryPage and IsDrawHeadersOnEveryPage;
end;

function TdxSpreadSheetDocumentBasedReportLink.IsDrawHeadersOnEveryPage: Boolean;
begin
  Result := Options.Source.RowsToRepeat.Assigned;
end;

function TdxSpreadSheetDocumentBasedReportLink.IsDrawRowHeadersOnEveryPage: Boolean;
begin
  Result := Options.Source.ColumnsToRepeat.Assigned;
end;

function TdxSpreadSheetDocumentBasedReportLink.IsShowGridLines: Boolean;
begin
  Result := Options.Source.ActualGridLines and not IsDraftQuality;
end;

function TdxSpreadSheetDocumentBasedReportLink.IsShowRowAndColumnHeadings: Boolean;
begin
  Result := Options.Source.ActualHeaders;
end;

function TdxSpreadSheetDocumentBasedReportLink.IsSuppressFontColors: Boolean;
begin
  Result := Options.Printing.ActualBlackAndWhite
end;

function TdxSpreadSheetDocumentBasedReportLink.IsSuppressSourceFormats: Boolean;
begin
  Result := Options.Printing.ActualBlackAndWhite or IsDraftQuality;
end;

function TdxSpreadSheetDocumentBasedReportLink.UseGrayScaleForGraphics: Boolean;
begin
  Result := IsSuppressFontColors;
end;

function TdxSpreadSheetDocumentBasedReportLink.GetRebuildOnPageParamsChange(AUpdateCodes: TdxPrinterPageUpdateCodes): Boolean;
begin
  Result := inherited or (ucScale in AUpdateCodes);
end;

procedure TdxSpreadSheetDocumentBasedReportLink.MakeDelimiters(
  AReportCells: TdxReportCells; AHorzDelimiters, AVertDelimiters: TList);
var
  I: Integer;
begin
  inherited;
  for I := 0 to DelimitersHardHorz.Count - 1 do
    AHorzDelimiters.Add(DelimitersHardHorz[I]);
  for I := 0 to DelimitersHardVert.Count - 1 do
    AVertDelimiters.Add(DelimitersHardVert[I]);
end;

procedure TdxSpreadSheetDocumentBasedReportLink.PrepareBuildReport;
begin
  inherited;
  AssignOptionsToPrinterPage(PrinterPage);
end;

procedure TdxSpreadSheetDocumentBasedReportLink.PrepareConstruct(AReportCells: TdxReportCells);
begin
  CalculatePrintAreas;
  FixedTransparent := True;
  FixedFont := TControlAccess(SpreadSheet).Font;
  DrawMode := gdmBorrowSource;
  inherited;
end;

procedure TdxSpreadSheetDocumentBasedReportLink.SetComponent(Value: TComponent);
begin
  if Component <> Value then
  begin
    if Component is TdxCustomSpreadSheet then
      TdxCustomSpreadSheet(Component).RemoveListener(Self);
    inherited;
    if Component is TdxCustomSpreadSheet then
      TdxCustomSpreadSheet(Component).AddListener(Self);
  end;
end;

procedure TdxSpreadSheetDocumentBasedReportLink.UnprepareConstruct(AReportCells: TdxReportCells);
var
  AIndex: Integer;
  I: Integer;
begin
  ConstructHeadersOnEveryPage;
  inherited;

  if Options.Page.ScaleMode <> oppsmFitToPage then
  begin
    if not ShrinkToPageWidth then
      for I := 0 to Options.Pagination.ColumnPageBreaks.Count - 1 do
      begin
        AIndex := Options.Pagination.ColumnPageBreaks[I];
        if dxSpreadSheetContainsColumn(FPrintArea, AIndex) then
          AddHorizontalPageBreak(AIndex + 1);
      end;

    for I := 0 to Options.Pagination.RowPageBreaks.Count - 1 do
    begin
      AIndex := Options.Pagination.RowPageBreaks[I];
      if dxSpreadSheetContainsRow(FPrintArea, AIndex) then
        AddPageBreak(AIndex + 1);
    end;
  end;
end;

procedure TdxSpreadSheetDocumentBasedReportLink.AssignOptionsToPrinterPage(APage: TdxPrinterPage);
begin
  if not FIsPrintOptionsLocked and (Sheet <> nil) then
  begin
    FIsPrintOptionsLocked := True;
    APage.BeginUpdate;
    try
      AssignOptionsToPrinterPageCore(APage);
    finally
      APage.EndUpdate;
      FIsPrintOptionsLocked := False;
    end;
  end;
end;

procedure TdxSpreadSheetDocumentBasedReportLink.AssignOptionsToPrinterPageCore(APage: TdxPrinterPage);

  function ConvertValue(AValue: Double): Integer;
  begin
    if APage.RealMeasurementUnits = muMillimeters then
      AValue := TdxInchesUnits.ToMM(AValue);
    Result := Round(AValue * 1000);
  end;

const
  OrientationMap: array[Boolean] of TdxPrinterOrientation = (poPortrait, poLandscape);
  PageOrderMap: array[Boolean] of TdxPageOrder = (poDownThenOver, poOverThenDown);
begin
  APage.GrayShading := Options.Printing.ActualBlackAndWhite;
  APage.PageOrder := PageOrderMap[Options.Printing.ActualPageOrder = opppOverThenDown];
  APage.Orientation := OrientationMap[Options.Page.Orientation = oppoLandscape];

  APage.CenterOnPageH := Options.Printing.ActualHorizontalCentered;
  APage.CenterOnPageV := Options.Printing.ActualVerticalCentered;

  if Options.Page.ScaleMode <> oppsmDefault then
  begin
    if Options.Page.ScaleMode = oppsmAdjustToScale then
    begin
      APage.ScaleMode := smAdjust;
      APage.ScaleFactor := Options.Page.Scale;
    end
    else
    begin
      APage.FitToPagesHorizontally := Options.Page.FitToWidth;
      APage.FitToPagesVertically := Options.Page.FitToHeight;
      APage.ScaleMode := smFit;
    end;
  end;

  if Options.Page.Margins.Assigned then
  begin
    APage.Margins.Bottom := ConvertValue(Options.Page.Margins.Bottom);
    APage.Margins.Left := ConvertValue(Options.Page.Margins.Left);
    APage.Margins.Right := ConvertValue(Options.Page.Margins.Right);
    APage.Margins.Top := ConvertValue(Options.Page.Margins.Top);
    APage.Footer := ConvertValue(Options.Page.Margins.Footer);
    APage.Header := ConvertValue(Options.Page.Margins.Header);
  end;

  if Options.Page.Paper.Assigned then
  begin
    if Options.Page.Paper.SizeID = 0 then
    begin
      APage.PageSize.Point := Point(
        ConvertValue(Options.Page.Paper.CustomSize.X),
        ConvertValue(Options.Page.Paper.CustomSize.Y));
    end
    else
      APage.DMPaper := Options.Page.Paper.SizeID;
  end
  else
    APage.DMPaper := TdxPrintingDefaults.DMPaper;

  TdxSpreadSheetMacroConverter.Decode(Self, APage.PageFooter, Options.HeaderFooter.CommonFooter);
  TdxSpreadSheetMacroConverter.Decode(Self, APage.PageHeader, Options.HeaderFooter.CommonHeader);
  TCustomdxPageObjectAccess(APage.PageFooter).AdjustOnScale := Options.HeaderFooter.ActualScaleWithDocument;
  TCustomdxPageObjectAccess(APage.PageFooter).AlignWithMargins := Options.HeaderFooter.ActualAlignWithMargins;
  TCustomdxPageObjectAccess(APage.PageHeader).AdjustOnScale := Options.HeaderFooter.ActualScaleWithDocument;
  TCustomdxPageObjectAccess(APage.PageHeader).AlignWithMargins := Options.HeaderFooter.ActualAlignWithMargins;
end;

procedure TdxSpreadSheetDocumentBasedReportLink.AssignPrinterPageToOptions(APage: TdxPrinterPage);
begin
  if not FIsPrintOptionsLocked and (Sheet <> nil) then
  begin
    FIsPrintOptionsLocked := True;
    Options.BeginUpdate;
    try
      AssignPrinterPageToOptionsCore(APage);
    finally
      Options.EndUpdate;
      FIsPrintOptionsLocked := False;
    end;
  end;
end;

procedure TdxSpreadSheetDocumentBasedReportLink.AssignPrinterPageToOptionsCore(APage: TdxPrinterPage);

  function ConvertValue(AValue: Integer): Double;
  begin
    Result := AValue / 1000;
    if APage.RealMeasurementUnits = muMillimeters then
      Result := TdxInchesUnits.FromMM(Result);
  end;

const
  OrientationMap: array[Boolean] of TdxSpreadSheetTableViewOptionsPrintPageOrientation = (
    oppoPortrait, oppoLandscape
  );
  ScaleModeMap: array[TdxScaleMode] of TdxSpreadSheetTableViewOptionsPrintPageScaleMode = (
    oppsmAdjustToScale, oppsmFitToPage
  );
  PageOrderMap: array[TdxPageOrder] of TdxSpreadSheetTableViewOptionsPrintPrintingPageOrder = (
    opppOverThenDown, opppDownThenOver
  );
begin
  Options.Page.Orientation := OrientationMap[APage.Orientation = poLandscape];
  Options.Page.FitToHeight := APage.FitToPagesVertically;
  Options.Page.FitToWidth := APage.FitToPagesHorizontally;
  Options.Page.Scale := APage.ScaleFactor;
  Options.Page.ScaleMode := ScaleModeMap[APage.ScaleMode];

  Options.Printing.PageOrder := PageOrderMap[APage.PageOrder];
  Options.Printing.BlackAndWhite := dxBooleanToDefaultBoolean(APage.GrayShading);
  Options.Printing.HorizontalCentered := dxBooleanToDefaultBoolean(APage.CenterOnPageH);
  Options.Printing.VerticalCentered := dxBooleanToDefaultBoolean(APage.CenterOnPageV);

  if APage.DMPaper = DMPAPER_USER then
  begin
    Options.Page.Paper.CustomSize.X := ConvertValue(APage.PageSize.X);
    Options.Page.Paper.CustomSize.Y := ConvertValue(APage.PageSize.Y);
  end
  else
    Options.Page.Paper.SizeID := APage.DMPaper;

  Options.Page.Margins.Bottom := ConvertValue(APage.Margins.Bottom);
  Options.Page.Margins.Left := ConvertValue(APage.Margins.Left);
  Options.Page.Margins.Right := ConvertValue(APage.Margins.Right);
  Options.Page.Margins.Top := ConvertValue(APage.Margins.Top);
  Options.Page.Margins.Footer := ConvertValue(APage.Footer);
  Options.Page.Margins.Header := ConvertValue(APage.Header);

  TdxSpreadSheetMacroConverter.Encode(Self, Options.HeaderFooter.CommonFooter, APage.PageFooter);
  TdxSpreadSheetMacroConverter.Encode(Self, Options.HeaderFooter.CommonHeader, APage.PageHeader);
  Options.HeaderFooter.AlignWithMargins := dxBooleanToDefaultBoolean(TCustomdxPageObjectAccess(APage.PageHeader).AlignWithMargins);
  Options.HeaderFooter.ScaleWithDocument := dxBooleanToDefaultBoolean(TCustomdxPageObjectAccess(APage.PageHeader).AdjustOnScale);
  Options.HeaderFooter.EvenPagesFooter.Assigned := False;
  Options.HeaderFooter.EvenPagesHeader.Assigned := False;
  Options.HeaderFooter.FirstPageFooter.Assigned := False;
  Options.HeaderFooter.FirstPageHeader.Assigned := False;
end;

function TdxSpreadSheetDocumentBasedReportLink.GetStartPageIndex: Integer;
begin
  Result := 1;
  if Sheet <> nil then
    Result := Max(Result, Options.Page.FirstPageNumber);
end;

procedure TdxSpreadSheetDocumentBasedReportLink.SetStartPageIndex(Value: Integer);
begin
  Options.Page.FirstPageNumber := Max(Value, 0);
  inherited SetStartPageIndex(StartPageIndex);
end;

procedure TdxSpreadSheetDocumentBasedReportLink.PageParamsChanged(
  Sender: TdxPrinterPage; AStyle: TBasedxPrintStyle; AUpdateCodes: TdxPrinterPageUpdateCodes);
begin
  if Sender = PrinterPage then
    AssignPrinterPageToOptions(PrinterPage);
  inherited;
end;

procedure TdxSpreadSheetDocumentBasedReportLink.DataChanged(Sender: TdxCustomSpreadSheet);
begin
  // do nothing
end;

procedure TdxSpreadSheetDocumentBasedReportLink.OptionsChanged(Sender: TdxSpreadSheetCustomView);
begin
  if Sender = Sheet then
    AssignOptionsToPrinterPage(PrinterPage);
end;

procedure TdxSpreadSheetDocumentBasedReportLink.ConstructHeadersOnEveryPage;

  function ConvertArea(const AArea: TRect; AOrientation: TdxOrientation): TRect;
  begin
    Result := AArea;
    if HasSelection then
      Result := cxRectOffset(Result, ReportAreaToSpreadSheetArea(SelectionRect).TopLeft, False);
    if IsShowRowAndColumnHeadings then
      Result := cxRectOffset(Result, Ord(AOrientation = orVertical), Ord(AOrientation = orHorizontal));
    Result.Left := Max(Result.Left, 0);
    Result.Top := Max(Result.Top, 0);
  end;

var
  AHeadersCorner: TRect;
  AHeaders: TRect;
  ARowHeaders: TRect;
begin
  if IsDrawRowHeadersOnEveryPage then
    ARowHeaders := ConvertArea(Options.Source.ColumnsToRepeat.Rect, orVertical)
  else
    ARowHeaders := cxInvalidRect;

  if IsDrawHeadersOnEveryPage then
    AHeaders := ConvertArea(Options.Source.RowsToRepeat.Rect, orHorizontal)
  else
    AHeaders := cxInvalidRect;

  if IsDrawHeadersOnEveryPage and IsDrawRowHeadersOnEveryPage then
    AHeadersCorner := cxRect(ARowHeaders.Left, AHeaders.Top, ARowHeaders.Right, AHeaders.Bottom)
  else
    AHeadersCorner := cxInvalidRect;

  if dxSpreadSheetIsValidArea(AHeaders) then
  begin
    if ConstructHeadersOnEveryPage(ReportCells.HeaderCells, AHeaders) then
      OffsetChildren(ReportCells.HeaderCells, cxPoint(0, -ReportCells.HeaderCells.FirstCell.Top));
  end;

  if dxSpreadSheetIsValidArea(ARowHeaders) then
  begin
    if ConstructHeadersOnEveryPage(ReportCells.RowHeaderCells, ARowHeaders) then
      OffsetChildren(ReportCells.RowHeaderCells,
        cxPoint(-ReportCells.RowHeaderCells.FirstCell.Left, ReportCells.HeaderCells.Height));
  end;

  if dxSpreadSheetIsValidArea(AHeadersCorner) then
  begin
    if ConstructHeadersOnEveryPage(ReportCells.HeaderCornerCells, AHeadersCorner) then
      OffsetChildren(ReportCells.HeaderCornerCells, cxPoint(0, -ReportCells.HeaderCornerCells.Cells[0].Top));
  end;
end;

function TdxSpreadSheetDocumentBasedReportLink.ConstructHeadersOnEveryPage(AHost: TdxReportCell; AArea: TRect): Boolean;

  procedure CloneAffectedCells(var ADataOffset: Integer);
  var
    ACell: TdxReportCell;
    AColumn: Integer;
    ARow: Integer;
    AStartIndex: Integer;
  begin
    AStartIndex := Ord(IsShowRowAndColumnHeadings);
    for ARow := AArea.Top to AArea.Bottom do
    begin
      ACell := TdxReportCell(FGridAdapter.Rows[ARow].Clone(AHost));
      if AArea.Right <> MaxInt then
      begin
        for AColumn := ACell.DataItemCount - 1 downto AArea.Right + 1 do
          ACell.DataItems[AColumn].Free;
      end;
      for AColumn := AArea.Left - 1 downto AStartIndex do
        ACell.DataItems[AColumn].Free;
      if ACell.DataItemCount > AStartIndex then
      begin
        ADataOffset := ACell.DataItems[AStartIndex].Left;
        if AStartIndex > 0 then
          Dec(ADataOffset, ACell.DataItems[AStartIndex - 1].Right);
        for AColumn := AStartIndex to ACell.DataItemCount - 1 do
          ACell.DataItems[AColumn].Offset(-ADataOffset, 0);
        ACell.Width := ACell.DataItems[ACell.DataItemCount - 1].Right;
      end;
    end;
  end;

  procedure CloneAffectedContainers(AContainers: TdxReportCell; const AOffset: TPoint);
  var
    ACell: TdxReportCell;
    ACellBounds: TRect;
    AHostBounds: TRect;
    I: Integer;
  begin
    AHostBounds := AHost.AbsoluteRect;
    for I := 0 to AContainers.CellCount - 1 do
    begin
      ACell := AContainers.Cells[I];
      ACellBounds := cxRectOffset(ACell.AbsoluteRect, AOffset);
      if cxRectIntersect(ACellBounds, AHostBounds) then
      begin
        ACell := ACell.Clone(AHost) as TdxReportCell;
        ACell.BoundsRect := ACellBounds;
      end;
    end;
  end;

var
  ADataOffset: Integer;
begin
  if dxSpreadSheetIntersects(AArea, cxRect(0, 0, MaxInt, FGridAdapter.RowCount - 1), AArea) then
  begin
    ADataOffset := 0;
    CloneAffectedCells(ADataOffset);
    SetupBoundsRect(AHost);
    if HostMergedCells <> nil then
      CloneAffectedContainers(HostMergedCells, cxPoint(-ADataOffset, 0));
    if HostContainers <> nil then
      CloneAffectedContainers(HostContainers, cxPoint(-ADataOffset, 0));
  end;
  Result := AHost.CellCount > 0;
end;

procedure TdxSpreadSheetDocumentBasedReportLink.AddContainers(AReportCells: TdxReportCells);
begin
  inherited;
  if Options.Source.CellComments = psccAtEnd then
    AddComments(AReportCells);
end;

function TdxSpreadSheetDocumentBasedReportLink.CanPrintContainer(AContainer: TdxSpreadSheetContainer): Boolean;
begin
  Result := inherited CanPrintContainer(AContainer) and not IsDraftQuality;
  if Result then
  begin
    if AContainer is TdxSpreadSheetCommentContainer then
      Result := Options.Source.CellComments = psccAsDisplayed;
  end;
end;

procedure TdxSpreadSheetDocumentBasedReportLink.InitializeContainerCell(
  ACell: TdxReportCell; AViewInfo: TdxSpreadSheetContainerViewInfo);
var
  ACellBounds: TRect;
  AConnectionCell: TdxReportCellConnectionLine;
  AContainer: TdxSpreadSheetCommentContainer;
  AContainerBounds: TRect;
  ALinePoint1: TPoint;
  ALinePoint2: TPoint;
begin
  inherited;

  if (Options.Source.CellComments = psccAsDisplayed) and (AViewInfo.Owner is TdxSpreadSheetCommentContainer) then
  begin
    AContainer := TdxSpreadSheetCommentContainer(AViewInfo.Owner);
    if GetCellBoundsInReport(AContainer.Cell.RowIndex, AContainer.Cell.ColumnIndex, True, ACellBounds) then
    begin
      AContainerBounds := ACell.BoundsRect;
      ALinePoint1 := Point(ACellBounds.Right, ACellBounds.Top);
      if AContainerBounds.Left >= ACellBounds.Right then
        ALinePoint2 := AContainerBounds.TopLeft
      else if AContainerBounds.Bottom < ACellBounds.Top  then
        ALinePoint2 := Point(AContainerBounds.Right, AContainerBounds.Bottom)
      else
        ALinePoint2 := Point(AContainerBounds.Right, AContainerBounds.Top);

      if not (cxRectPtIn(AContainerBounds, ALinePoint1) and cxRectPtIn(AContainerBounds, ALinePoint2)) then
      begin
        AConnectionCell := TdxReportCellConnectionLine.Create(ACell.Parent);
        AConnectionCell.LinePoint1 := ALinePoint1;
        AConnectionCell.LinePoint2 := ALinePoint2;
      end;
    end;
  end;
end;

procedure TdxSpreadSheetDocumentBasedReportLink.AddComments(AReportCells: TdxReportCells);
var
  AProducer: TdxSpreadSheetDocumentBasedReportLinkCommentProducer;
  I: Integer;
begin
  AProducer := CreateCommentsProducer(AReportCells);
  try
    for I := 0 to Sheet.Containers.Count - 1 do
    begin
      if Sheet.Containers[I] is TdxSpreadSheetCommentContainer then
        AProducer.Produce(TdxSpreadSheetCommentContainer(Sheet.Containers[I]));
    end;
  finally
    AProducer.Free;
  end;
end;

function TdxSpreadSheetDocumentBasedReportLink.CreateCommentsProducer(
  AReportCells: TdxReportCells): TdxSpreadSheetDocumentBasedReportLinkCommentProducer;
begin
  Result := TdxSpreadSheetDocumentBasedReportLinkCommentProducer.Create(Self, AReportCells);
end;

function TdxSpreadSheetDocumentBasedReportLink.GetRenderInfoClass: TdxPSReportRenderInfoClass;
begin
  Result := TdxSpreadSheetDocumentBasedReportRenderInfo;
end;

function TdxSpreadSheetDocumentBasedReportLink.GetOptions: TdxSpreadSheetTableViewOptionsPrint;
begin
  Result := Sheet.OptionsPrint;
end;

procedure TdxSpreadSheetDocumentBasedReportLink.OffsetChildren(AHost: TdxReportCell; const AOffset: TPoint);
var
  ACell: TdxReportCell;
  I: Integer;
begin
  for I := 0 to AHost.CellCount  - 1 do
  begin
    ACell := AHost.Cells[I];
    ACell.SetBounds(ACell.Left + AOffset.X, ACell.Top + AOffset.Y, ACell.Width, ACell.Height);
  end;
  AHost.SetBounds(0, 0, AHost.Width + AOffset.X, AHost.Height + AOffset.Y);
end;

{ TdxSpreadSheetDocumentBasedReportRenderInfo }

function TdxSpreadSheetDocumentBasedReportRenderInfo.CalculateActualScaleFactor(
  AFitToPageHorizontally, AFitToPageVertically, AReportDPI: Integer): Integer;
begin
  AFitToPageVertically := AdjustFitToPagesVerticallyValue(AFitToPageVertically);
  Result := Min(inherited, 100);
end;

function TdxSpreadSheetDocumentBasedReportRenderInfo.GetPageRenderInfoClass: TdxPSPageRenderInfoClass;
begin
  Result := TdxSpreadSheetDocumentBasedReportPageRenderInfo;
end;

function TdxSpreadSheetDocumentBasedReportRenderInfo.HasCommentsPage: Boolean;

  function HasComments: Boolean;
  var
    I: Integer;
  begin
    for I := 0 to ReportLink.Sheet.Containers.Count - 1 do
    begin
      if ReportLink.Sheet.Containers[I] is TdxSpreadSheetCommentContainer then
        Exit(True);
    end;
    Result := False;
  end;

begin
  Result := (ReportLink.Options.Source.CellComments = psccAtEnd) and HasComments;
end;

function TdxSpreadSheetDocumentBasedReportRenderInfo.IsCommentsPage(APageIndex: Integer): Boolean;
var
  ACommentsPageOffset: Integer;
begin
  Result := False;
  if HasCommentsPage then
  begin
    ACommentsPageOffset := ReportCells.Overlays[ReportCells.OverlayCount - 1][0].Top;
    if ACommentsPageOffset = 0 then
      Exit(True);
    APageIndex := APageIndex div PageColCount;
    if PageDelimiterYCount > APageIndex then
      Result := PageDelimitersY[APageIndex] >= ACommentsPageOffset;
  end;
end;

function TdxSpreadSheetDocumentBasedReportRenderInfo.IsHeaderOnPage(APageIndex: Integer): Boolean;
begin
  Result := (APageIndex >= PageColCount) and not IsCommentsPage(APageIndex);
end;

function TdxSpreadSheetDocumentBasedReportRenderInfo.IsReportFitsToPages(APrinterPage: TdxPrinterPage): Boolean;
begin
  Result :=
    ((APrinterPage.FitToPagesVertically = 0) or (PageRowCount <= AdjustFitToPagesVerticallyValue(APrinterPage.FitToPagesVertically))) and
    ((APrinterPage.FitToPagesHorizontally = 0) or (PageColCount <= APrinterPage.FitToPagesHorizontally));
end;

function TdxSpreadSheetDocumentBasedReportRenderInfo.IsRowHeaderOnPage(APageIndex: Integer): Boolean;
begin
  Result := (APageIndex > 0) and not IsCommentsPage(APageIndex);
end;

function TdxSpreadSheetDocumentBasedReportRenderInfo.AdjustFitToPagesVerticallyValue(AValue: Integer): Integer;
begin
  Result := AValue;
  if HasCommentsPage then
    Result := Max(Result, 2);
end;

function TdxSpreadSheetDocumentBasedReportRenderInfo.GetReportLink: TdxSpreadSheetDocumentBasedReportLink;
begin
  Result := TdxSpreadSheetDocumentBasedReportLink(inherited ReportLink);
end;

{ TdxSpreadSheetDocumentBasedReportPageRenderInfo }

function TdxSpreadSheetDocumentBasedReportPageRenderInfo.HasHeader: Boolean;
begin
  Result := inherited and not IsTopPage and not IsCommentsPage;
end;

function TdxSpreadSheetDocumentBasedReportPageRenderInfo.HasHeaderCorner: Boolean;
begin
  Result := HasHeader and HasRowHeader;
end;

function TdxSpreadSheetDocumentBasedReportPageRenderInfo.HasRowHeader: Boolean;
begin
  Result := inherited and not IsLeftPage and not IsCommentsPage;
end;

function TdxSpreadSheetDocumentBasedReportPageRenderInfo.IsCommentsPage: Boolean;
begin
  Result := RenderInfo.IsCommentsPage(PageIndex);
end;

function TdxSpreadSheetDocumentBasedReportPageRenderInfo.IsLeftPage: Boolean;
begin
  Result := ColIndex = 0;
end;

procedure TdxSpreadSheetDocumentBasedReportPageRenderInfo.CalculateHeadersBounds;
begin
  inherited CalculateHeadersBounds;
  if not HasHeader then
    OffsetRect(RowHeaderBounds, 0, cxRectHeight(HeaderBounds));
  if HasHeaderCorner then
    OffsetRect(HeaderBounds, -cxRectWidth(HeaderCornerBounds), 0);
end;

function TdxSpreadSheetDocumentBasedReportPageRenderInfo.GetRenderInfo: TdxSpreadSheetDocumentBasedReportRenderInfo;
begin
  Result := TdxSpreadSheetDocumentBasedReportRenderInfo(inherited RenderInfo);
end;

function TdxSpreadSheetDocumentBasedReportPageRenderInfo.GetReportLink: TdxSpreadSheetDocumentBasedReportLink;
begin
  Result := TdxSpreadSheetDocumentBasedReportLink(inherited ReportLink);
end;

initialization

finalization
  TdxSpreadSheetMacroConverter.Finalize;
end.
