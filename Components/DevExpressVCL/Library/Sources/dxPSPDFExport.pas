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

unit dxPSPDFExport;

interface

{$I cxVer.inc}

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Windows, Classes, SysUtils, Graphics, Generics.Collections, Generics.Defaults,
  cxGraphics, dxPSCore, dxBkGnd, cxClasses, cxDrawTextUtils, cxGeometry,
  dxPrnPg, dxPSPDFExportCore, dxPSReportRenderCanvas, dxPSFillPatterns, dxPSGlbl, dxPrnDlg;

type
  TdxPSPDFReportRenderCanvas = class;

  { TdxPSPDFReportExportProvider }

  TdxPSPDFReportExportProvider = class(TdxPSCustomReportExportProvider)
  strict private
    FOptions: TdxPSPDFReportExportOptions;
    FPage: TdxPSPDFPage;
    FPageContent: TdxPSPDFPageContent;
    FPDFFile: TdxPSPDFFile;

    procedure SetOptions(AOptions: TdxPSPDFReportExportOptions);
    procedure SetPage(AValue: TdxPSPDFPage);
  protected
    procedure AssignSettings(APDFFile: TdxPSPDFFile);
    procedure ExportPage(APageIndex: Integer); virtual;
  public
    constructor Create(AReportLink: TBasedxReportLink); override;
    destructor Destroy; override;
    procedure Build; override;
    procedure SaveToStream(AStream: TStream); override;
    //
    property Options: TdxPSPDFReportExportOptions read FOptions write SetOptions;
    property Page: TdxPSPDFPage read FPage write SetPage;
    property PageContent: TdxPSPDFPageContent read FPageContent;
    property PDFFile: TdxPSPDFFile read FPDFFile;
  end;

  { TdxPSPDFCanvasRegion }

  TdxPSPDFCanvasRegion = class(TcxRegion)
  strict private
    FXForm: TXForm;
  public
    function Clone: TdxPSPDFCanvasRegion;

    property XForm: TXForm read FXForm write FXForm;
  end;

  { TdxPSPDFCanvasClipRegion }

  TdxPSPDFCanvasClipRegion = class(TObjectList<TdxPSPDFCanvasRegion>)
  strict private
    FCanvasSize: TSize;

    function GetActualRegion(const XForm: TXForm): TdxPSPDFCanvasRegion;
    procedure SetCanvasSize(const Value: TSize);
  public
    procedure AfterConstruction; override;
    function Clone: TdxPSPDFCanvasClipRegion;
    function Contains(const R: TRect): Boolean;
    procedure Exclude(const R: TRect; const XForm: TXForm);
    procedure Intersect(const R: TRect; const XForm: TXForm);
    procedure IntersectEx(const R: TcxRegion; const XForm: TXForm);
    //
    property CanvasSize: TSize read FCanvasSize write SetCanvasSize;
  end;

  { TdxPSPDFReportRenderCanvas }

  TdxPSPDFReportRenderCanvas = class(TdxPSReportRenderScreenCanvas)
  strict private const
    DirectionAngle: array[TcxRotationAngle] of Double = (0, 90, -90, 180);
  strict private
    FClipRegion: TdxPSPDFCanvasClipRegion;
    FCustomDrawBounds: TRect;
    FCustomDrawMetaFile: TMetafile;
    FCustomDrawMetaFileCanvas: TMetafileCanvas;
    FPageSize: TPoint;
    FPDFMatrix: TXForm;
    FProvider: TdxPSPDFReportExportProvider;
    FSavedRegions: TcxObjectList;
    FSavedWorldTransforms: TStack<TXForm>;
    FWorldTransform: TXForm;
    FWorldTransformAssigned: Boolean;

    function CalculateTextOffset(const ARect: TdxRectF; AAngle: TcxRotationAngle): TdxPointF;
    function GetPageContent: TdxPSPDFPageContent;
    procedure InternalDrawText(const ABounds: TRect; const AText: string; AFont: TFont; ATextColor: TColor;
      AFormat: TcxTextOutFormat; AMaxLineCount: Integer; AVertical: Boolean; ADirection: TcxVerticalTextOutDirection;
      ALineSpacing: Single = 1);
    procedure InternalFillRect(const R: TRect; ABackColor, AForeColor: TColor;
      APattern: TdxPSFillPatternClass; APatternBrush: TBrush);
    procedure SelectFonT(AFont: TFont; ATextColor: TColor); // SelectFont conflicts with C++ macro
    procedure SetClipRegion(const Value: TdxPSPDFCanvasClipRegion);
  protected
    procedure DoFillEdge(ARegion: TcxRegionHandle; ABackColor, AEdgeColor: TColor;
      AIsVerticalOrientation: Boolean; AEdgePattern: TClass); override;
    procedure DrawPictureStretch(APicture: TGraphic; const R: TRect; ANumerator, ADenominator: Integer); override;
    procedure DrawPictureTile(APicture: TGraphic; const R: TRect;
      APictureWidth, APictureHeight, ANumerator, ADenominator: Integer); override;
    function GetSupportsTransparentImagesDrawing: Boolean; override;
    function GetWorldTransform: TXForm; override;
    procedure SetWorldTransform(const Value: TXForm); override;
    procedure UpdatePDFMatrix;
  public
    constructor Create(AProvider: TdxPSPDFReportExportProvider);
    destructor Destroy; override;
    function AbsoluteRect(const R: TRect): TRect;
    function CalculateLineThickness(AUnitsPerPixel, AUnitsPerPoint: Integer): Integer; override;
    function IsPrinterCanvas: Boolean; override;

    procedure DeviceToLogicalCoordinates(var R: TRect); override;
    procedure LogicalToDeviceCoordinates(var R: TRect); override;
    procedure SetCanvasExts(const APageSize: TPoint; AMappingMode: TdxPSReportRenderCanvasMappingMode;
      AScaleNumerator, AScaleDenominator: Integer; const AViewPort: TRect); override;
    procedure UnprepareLogicalUnits; override;

    // Custom Draw
    function BeginCustomDraw(const AClipBounds: TRect; AFont: TFont; AColor: TColor): TCanvas; override;
    procedure EndCustomDraw(var ACanvas: TCanvas); override;

    // Clipping
    function ExcludeClipRect(const R: TRect): Integer; override;
    function IntersectClipRgn(const R: TRect): Integer; override;
    function IntersectClipRgnEx(ARegion: TcxRegionHandle): Boolean;

    // PageContent
    procedure PreparePageContent;
    procedure UnpreparePageContent;

    // Save/Restore clipping
    procedure RestoreClipRgn; override;
    procedure SaveClipRgn; override;

    // Save/Restore state
    procedure RestoreState; override;
    procedure SaveState; override;

    // Text
    function CalculateTextRect(const AText: string; var R: TRect; AFormat: TcxTextOutFormat = CXTO_DEFAULT_FORMAT;
      AFont: TFont = nil; AMaxLineCount: Integer = 0; ALeftIndent: Integer = 0; ARightIndent: Integer = 0;
      ATextColor: TColor = clDefault; ALineSpacing: Single = 1.0): Integer; override;
    procedure ExtTextOut(const ABounds: TRect; AFont: TFont; const AText: string;
      AGlyphs: PWord; AGlyphCount: Integer; AGlyphWidths: PInteger); override;
    procedure DrawText(var R: TRect; const AText: string; AFont: TFont; AFormat: Cardinal;
      ATextColor: TColor = clDefault; AMaxLineCount: Integer = 0; ALineSpacing: Single = 1.0); override;
    procedure DrawTextLine(const AText: string; const ATextRect, AClipRect: TRect; ATextWidth: Integer;
      ATextColor: TColor = clDefault; ADirection: TcxRotationAngle = ra0; ABreakExtraSpacing: Integer = 0;
      ANoClip: Boolean = False); overload;
    procedure DrawTextLine(const AText: string; const ATextOffset: TdxPointF; const AClipRect: TRect;
      ATextWidth: Integer; ATextColor: TColor; AAngle: Single; ABreakExtraSpacing: Integer; ANoClip: Boolean); overload;

    procedure RotatedTextOut(const ABounds: TRect; const AText: string; AFont: TFont;
      AAlignHorz: TcxTextAlignX = taCenterX; AAlignVert: TcxTextAlignY = taCenterY; AWordBreak: Boolean = True;
      ADirection: TcxVerticalTextOutDirection = vtdBottomToTop); override;

    procedure DrawEllipseFrame(const R: TRect; AColor: TColor; AThickness: Integer); override;
    procedure DrawExpandButton(R: TRect; AEdgeStyle: TdxCheckButtonEdgeStyle;
      AMarlettFont, ASymbolFont: TFont; AExpanded, AShadow, AFillInterior: Boolean;
      ABorderColor, ABackgroundColor: TColor; ALineThickness: Integer); override;
    procedure DrawFrame(const R: TRect; ATopLeftColor, ARightBottomColor: TColor;
      ABorderWidth: Integer = 1; ABorders: TcxBorders = cxBordersAll); override;
    procedure DrawGraphic(AHelper: TdxDrawGraphicHelper; const R: TRect); override;
    procedure DrawRoundFrame(const R: TRect; AEllipseWidth, AEllipseHeight: Integer;
      AColor: TColor; AThickness: Integer);  override;
    procedure FillEllipse(const R: TRect; ABackColor, AForeColor: TColor;
      APattern: TdxPSFillPatternClass; APatternBrush: TBrush); override;
    procedure FillRect(const R: TRect; AColor: TColor); override;
    procedure FillRectEx(const R: TRect; AColor: TColor;
      APattern: TdxPSFillPatternClass; APatternBrush: TBrush); virtual;
    procedure FillRegion(ARegion: TcxRegionHandle; ABackColor, AForeColor: TColor;
      APattern: TdxPSFillPatternClass; APatternBrush: TBrush); override;
    procedure FillRoundRect(R: TRect; AEllipseWidth, AEllipseHeight: Integer;
      ABackColor, AForeColor: TColor; APattern: TdxPSFillPatternClass; APatternBrush: TBrush); override;
    function IsRectVisible(const R: TRect): Boolean; override;
    procedure Pie(const R: TRect; const APoint1, APoint2: TPoint; AColor: TColor);
    procedure Polyline(const APoints: array of TPoint; AColor: TColor; ALineWidth: Integer); override;
    procedure Polygon(const APoints: array of TPoint; AColor: TColor;
      ABackgroundColor: TColor; ALineWidth: Integer; AFillMode: Integer = ALTERNATE); override;
    //
    property ClipRegion: TdxPSPDFCanvasClipRegion read FClipRegion write SetClipRegion;
    property PageContent: TdxPSPDFPageContent read GetPageContent;
    property PageSize: TPoint read FPageSize;
    property Provider: TdxPSPDFReportExportProvider read FProvider;
    property WorldTransformAssigned: Boolean read FWorldTransformAssigned;
  end;

function dxPSExportToPDF(AReportLink: TBasedxReportLink): Boolean;
function dxPSExportToPDFStream(AStream: TStream; AReportLink: TBasedxReportLink;
  AShowSettingsDialog: Boolean = True; ASettings: TdxPSPDFReportExportOptions = nil): Boolean;
function dxPSExportToPDFFile(const AFileName: string; AReportLink: TBasedxReportLink;
  AShowSettingsDialog: Boolean = True; ASettings: TdxPSPDFReportExportOptions = nil): Boolean;
implementation

uses
  Types, Math, Dialogs, ShellApi, dxCore, dxPSPDFExportDialog, dxPSPDFMetaFileParser,
  dxPSEdgePatterns, dxPSPDFStrings, cxControls, dxGDIPlusClasses;

const
  sdxPdfSaveDialogFilter = 'PDF Document (*.pdf)|*.pdf;';

type
  TBasedxReportLinkAccess = class(TBasedxReportLink);

  { TdxPSPDFExportCustomTextHelper }

  TdxPSPDFExportCustomTextHelper = class
  private
    FHasEndEllipsis: Boolean;
    FPDFCanvas: TdxPSPDFReportRenderCanvas;
    FRows: TcxTextRows;
    FTextParams: TcxTextParams;

    procedure CorrectTextRowsExtends(const S: TSize; ALineSpacing: Single);
    function GetLastTextRow: PcxTextRow;
    function GetTextRow(Index: Integer): PcxTextRow;
    function GetTextRowCount: Integer;
    function GetTextRowText(Index: Integer): string;
  public
    constructor Create(APDFCanvas: TdxPSPDFReportRenderCanvas; ATextFormat: Cardinal); virtual;
    function CalculateBreakExtractSpacing(const AWidth: Integer; ARow: PcxTextRow): Integer; virtual;
    function CalculateTextRect(const ABounds: TRect): TRect; virtual; abstract;
    function CalculateTextRowOutRect(ATextRow: PcxTextRow; var R: TRect; out ABreakExtraSpacing: Integer): TRect; virtual; abstract;
    function CalculateTextOutParams(const ABounds: TRect; AMaxLineCount: Integer; ALineSpacing: Single; const AText: string): TRect; virtual;
    function CalculateTotalTextHeight: Integer;
    procedure CalculateEndEllipsis(DC: HDC; const R: TRect; ARow: PcxTextRow);
    //
    property PDFCanvas: TdxPSPDFReportRenderCanvas read FPDFCanvas;
    property LastTextRow: PcxTextRow read GetLastTextRow;
    property TextParams: TcxTextParams read FTextParams;
    property TextRow[Index: Integer]: PcxTextRow read GetTextRow;
    property TextRowCount: Integer read GetTextRowCount;
    property TextRowText[Index: Integer]: string read GetTextRowText;
  end;

  { TdxPSPDFExportHorizontalTextHelper }

  TdxPSPDFExportHorizontalTextHelper = class(TdxPSPDFExportCustomTextHelper)
  public
    function CalculateTextRect(const ABounds: TRect): TRect; override;
    function CalculateTextRowOutRect(ATextRow: PcxTextRow; var R: TRect; out ABreakExtraSpacing: Integer): TRect; override;
  end;

  { TdxPSPDFExportVerticalTextHelper }

  TdxPSPDFExportVerticalTextHelper = class(TdxPSPDFExportCustomTextHelper)
  public
    function CalculateTextRect(const ABounds: TRect): TRect; override;
    function CalculateTextRowOutRect(ATextRow: PcxTextRow; var R: TRect; out ABreakExtraSpacing: Integer): TRect; override;
    function CalculateTextOutParams(const ABounds: TRect; AMaxLineCount: Integer; ALineSpacing: Single; const AText: string): TRect; override;
  end;

  { TdxPSPDFEdgePattern }

  TdxPSPDFEdgePattern = class(TdxPSPDFCustomPattern)
  private
    FColor: TColor;
    FLines: TRects;
  protected
    function GetContentData: string; override;
    procedure InitializePixels(APattern: TdxPSEdgePatternClass; AIsVertical: Boolean);
    //
    property Color: TColor read FColor;
  public
    constructor Create(AOwner: TdxPSPDFPatternList; AColor: TColor; AEdgePattern: TdxPSEdgePatternClass; AIsVertical: Boolean);
    destructor Destroy; override;
  end;

function InvertXForm(const X: TXForm): TXForm;
begin
  with TdxGPMatrix.CreateEx(X.eM11, X.eM12, X.eM21, X.eM22, X.eDx, X.eDy) do
  try
    Invert;
    GetElements(Result.eM11, Result.eM12, Result.eM21, Result.eM22, Result.eDx, Result.eDy);
  finally
    Free;
  end;
end;

function InternalExportToPDF(AStream: TStream; AReportLink: TBasedxReportLink;
  AShowSettingsDialog: Boolean; ASettings: TdxPSPDFReportExportOptions;
  const AStreamFileName: string = ''): Boolean;

  function DoBeforeExportToPDF(AOptions: TdxPSPDFReportExportOptions): Boolean;
  begin
    Result := TBasedxReportLinkAccess(AReportLink).DoBeforeExportToPDF(AStreamFileName, AOptions);
  end;

  function SetupOptions(AOptions: TdxPSPDFReportExportOptions): Boolean;
  begin
    if ASettings <> nil then
      AOptions.Assign(ASettings)
    else
      AOptions.Assign(AReportLink.PDFExportOptions);

    Result := not AShowSettingsDialog or dxPSShowPDFSettingsDialog(AOptions);
  end;

var
  AProvider: TdxPSPDFReportExportProvider;
begin
  AProvider := TdxPSPDFReportExportProvider.Create(AReportLink);
  try
    Result := SetupOptions(AProvider.Options) and DoBeforeExportToPDF(AProvider.Options);
    if Result then
    begin
      AProvider.Build;
      AProvider.SaveToStream(AStream);
    end;
  finally
    AProvider.Free;
  end;
end;

function dxPDFCalcTextRowExtents(AHandle: TCanvasHandle; AText: PWideChar;
  ATextLength: Integer; AExpandTabs: Boolean; PDFCanvas: TdxPSPDFReportRenderCanvas): TSize;
var
  AWideStr: string;
begin
  SetString(AWideStr, AText, ATextLength);
  Result := cxCalcTextExtents(AHandle, AText, ATextLength, AExpandTabs);
  Result.cx := Round(PDFCanvas.PageContent.TextWidth(AWideStr));
end;

function dxPSExportToPDF(AReportLink: TBasedxReportLink): Boolean;

  function CheckFileNameLength(const AFileName: string): string;
  begin
    Result := AFileName;
    if Length(AFileName) > MAX_PATH then
      Delete(Result, MAX_PATH, MaxInt);
  end;

var
  ADestFileName: string;
  ASaveDialog: TSaveDialog;
begin
  Result := dxPSShowPDFSettingsDialog(AReportLink.PDFExportOptions);
  if Result then
  begin
    ASaveDialog := TSaveDialog.Create(nil);
    try
      ASaveDialog.Filter := sdxPDFSaveDialogFilter;
      ASaveDialog.InitialDir := ExtractFilePath(AReportLink.PDFExportOptions.DefaultFileName);
      ASaveDialog.FileName := CheckFileNameLength(ExtractFileName(AReportLink.PDFExportOptions.DefaultFileName));
      ASaveDialog.Options := ASaveDialog.Options + [ofOverwritePrompt];
      ASaveDialog.DefaultExt := sdxPDFExt;
      Result := ASaveDialog.Execute;
      if Result then
      begin
        ADestFileName := ChangeFileExt(ASaveDialog.FileName, sdxPDFExt);
        Result := dxPSExportToPDFFile(ADestFileName, AReportLink, False, AReportLink.PDFExportOptions);
        if Result and AReportLink.PDFExportOptions.OpenDocumentAfterExport then
          dxShellExecute(ADestFileName);
      end;
    finally
      ASaveDialog.Free;
    end;
  end;
end;

function dxPSExportToPDFFile(const AFileName: string; AReportLink: TBasedxReportLink;
  AShowSettingsDialog: Boolean = True; ASettings: TdxPSPDFReportExportOptions = nil): Boolean;
var
  AStream: TFileStream;
begin
  Result := False;
  AStream := TFileStream.Create(AFileName, fmCreate);
  try
    Result := InternalExportToPDF(AStream, AReportLink, AShowSettingsDialog, ASettings, AFileName);
  finally
    FreeAndNil(AStream);
    if not Result then
      DeleteFile(AFileName);
  end;
end;

function dxPSExportToPDFStream(AStream: TStream; AReportLink: TBasedxReportLink;
  AShowSettingsDialog: Boolean = True; ASettings: TdxPSPDFReportExportOptions = nil): Boolean;
begin
  Result := InternalExportToPDF(AStream, AReportLink, AShowSettingsDialog, ASettings);
end;

function CreateTextExportHelper(ACanvas: TdxPSPDFReportRenderCanvas;
  AFormat: TcxTextOutFormat; AVertical: Boolean): TdxPSPDFExportCustomTextHelper;
begin
  if AVertical then
    Result := TdxPSPDFExportVerticalTextHelper.Create(ACanvas, AFormat)
  else
    Result := TdxPSPDFExportHorizontalTextHelper.Create(ACanvas, AFormat);
end;

{ TdxPSPDFEdgePattern }

constructor TdxPSPDFEdgePattern.Create(AOwner: TdxPSPDFPatternList;
  AColor: TColor; AEdgePattern: TdxPSEdgePatternClass; AIsVertical: Boolean);
begin
  if AIsVertical then
    inherited Create(AOwner, AEdgePattern.Size.cy, AEdgePattern.Size.cx)
  else
    inherited Create(AOwner, AEdgePattern.Size.cx, AEdgePattern.Size.cy);

  InitializePixels(AEdgePattern, AIsVertical);
  FColor := AColor;
end;

destructor TdxPSPDFEdgePattern.Destroy;
begin
  SetLength(FLines, 0);
  inherited Destroy;
end;

function TdxPSPDFEdgePattern.GetContentData: string;

  function GetLinesData: string;
  var
    I: Integer;
  begin
    Result := '';
    for I := 0 to Length(FLines) - 1 do
      Result := Result + dxPDFEncodeBounds(FLines[I], PatternHeight) + sdxPDFSpace;
  end;

begin
  Result := Format('%s rg %s f', [dxPDFEncodeColor(Color), GetLinesData]);
end;

procedure TdxPSPDFEdgePattern.InitializePixels(APattern: TdxPSEdgePatternClass; AIsVertical: Boolean);

  function CompactLine(var APrev, ACurrent: TRect): Boolean;
  begin
    Result := True;
    if (APrev.Top = ACurrent.Top) and (APrev.Bottom = ACurrent.Bottom) and (APrev.Right = ACurrent.Left) then
      APrev.Right := ACurrent.Right
    else
      if (APrev.Left = ACurrent.Left) and (APrev.Right = ACurrent.Right) and (APrev.Bottom = ACurrent.Top) then
        APrev.Bottom := ACurrent.Bottom
      else
        Result := False;
  end;

var
  AIndex: Integer;
  ASize: TSize;
  I, J: Integer;
begin
  AIndex := 0;
  ASize := APattern.Size;
  SetLength(FLines, ASize.cx * ASize.cy);

  for I := 0 to ASize.cx - 1 do
  begin
    for J := 0 to ASize.cy - 1 do
      if APattern.Bits(J) and (1 shl I) <> 0 then
      begin
        if AIsVertical then
          FLines[AIndex] := Bounds(J, I, 1, 1)
        else
          FLines[AIndex] := Bounds(I, J, 1, 1);

        if (AIndex = 0) or not CompactLine(FLines[AIndex - 1], FLines[AIndex]) then
          Inc(AIndex);
      end;
  end;

  I := AIndex - 1;
  while I > 1 do
  begin
    if CompactLine(FLines[I - 1], FLines[I]) then
      Dec(AIndex);
    Dec(I);
  end;
  SetLength(FLines, AIndex);
end;

{ TdxPSPDFExportCustomTextHelper }

constructor TdxPSPDFExportCustomTextHelper.Create(APDFCanvas: TdxPSPDFReportRenderCanvas; ATextFormat: Cardinal);
begin
  inherited Create;
  FPDFCanvas := APDFCanvas;
  FTextParams := PDFCanvas.CalculateTextParams(ATextFormat or CXTO_CALCROWCOUNT);
end;

function TdxPSPDFExportCustomTextHelper.CalculateBreakExtractSpacing(const AWidth: Integer; ARow: PcxTextRow): Integer;
begin
  if ARow.BreakCount > 0 then
    Result := (AWidth - ARow.TextExtents.cx) div ARow.BreakCount
  else
    Result := 0;
end;

function TdxPSPDFExportCustomTextHelper.CalculateTextOutParams(
  const ABounds: TRect; AMaxLineCount: Integer; ALineSpacing: Single; const AText: string): TRect;
var
  ATextRowCount: Integer;
begin
  Result := cxPrepareRect(ABounds, TextParams, 0, 0);
  if not cxRectIsEmpty(Result) then
  begin
    cxMakeTextRows(PDFCanvas.Canvas.Handle, PChar(AText), Length(AText), Result, TextParams, FRows, ATextRowCount, AMaxLineCount);
    CorrectTextRowsExtends(cxRectSize(Result), ALineSpacing);
    if TextParams.EndEllipsis and (TextRowCount > 0) then
      CalculateEndEllipsis(PDFCanvas.Canvas.Handle, ABounds, LastTextRow);
    Result := CalculateTextRect(Result);
  end;
end;

procedure TdxPSPDFExportCustomTextHelper.CalculateEndEllipsis(DC: HDC; const R: TRect; ARow: PcxTextRow);
var
  AWidth: Integer;
begin
  AWidth := cxRectWidth(R);
  FHasEndEllipsis := ARow^.TextExtents.cx > AWidth;
  if FHasEndEllipsis then
  begin
    cxPrepareEndEllipsis(DC, ARow, TextParams, @dxPDFCalcTextRowExtents, AWidth, PDFCanvas);
    Inc(ARow^.TextExtents.cx, TextParams.EndEllipsisWidth);
  end;
end;

function TdxPSPDFExportCustomTextHelper.CalculateTotalTextHeight: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to TextRowCount - 1 do
    Inc(Result, TextRow[I].TextExtents.cy);
end;

procedure TdxPSPDFExportCustomTextHelper.CorrectTextRowsExtends(const S: TSize; ALineSpacing: Single);
var
  AExtraSpace: Integer;
  AReminder: Single;
  ARow: PcxTextRow;
  I: Integer;
begin
  if ALineSpacing <> 1 then
    for I := 0 to TextRowCount - 1 do
    begin
      ARow := TextRow[I];
      ARow.TextExtents.cy := Round(ARow.TextExtents.cy * ALineSpacing);
    end;

  for I := 0 to TextRowCount - 1 do
  begin
    ARow := TextRow[I];
    ARow^.TextExtents.cx := Min(ARow^.TextExtents.cx, Round(PDFCanvas.PageContent.TextWidth(TextRowText[I])));
  end;

  AExtraSpace := Max(CalculateTotalTextHeight - S.cy, 0);
  if AExtraSpace > 0 then
  begin
    AReminder := 0;
    ALineSpacing := AExtraSpace / TextRowCount;
    for I := 0 to TextRowCount - 1 do
    begin
      ARow := TextRow[I];
      AReminder := AReminder + ALineSpacing;
      while (AReminder > 1) and (AExtraSpace > 0) do
      begin
        AReminder := AReminder - 1;
        Dec(ARow.TextExtents.cy);
        Dec(AExtraSpace);
      end;
      if AExtraSpace = 0 then
        Break;
    end;
  end;
end;

function TdxPSPDFExportCustomTextHelper.GetLastTextRow: PcxTextRow;
begin
  Result := TextRow[TextRowCount - 1];
end;

function TdxPSPDFExportCustomTextHelper.GetTextRow(Index: Integer): PcxTextRow;
begin
  Result := cxGetTextRow(FRows, Index);
end;

function TdxPSPDFExportCustomTextHelper.GetTextRowCount: Integer;
begin
  Result := FRows.Count;
end;

function TdxPSPDFExportCustomTextHelper.GetTextRowText(Index: Integer): string;
var
  ARow: PcxTextRow;
begin
  ARow := TextRow[Index];
  if Assigned(ARow) then
  begin
    SetString(Result, ARow^.Text, ARow^.TextLength);
    if FHasEndEllipsis and (Index = TextRowCount - 1) then
      Result := Result + '...';
    if FTextParams.HidePrefix then
      Result := RemoveAccelChars(Result, False);
  end
  else
    Result := '';
end;

{ TdxPSPDFExportHorizontalTextHelper }

function TdxPSPDFExportHorizontalTextHelper.CalculateTextRect(const ABounds: TRect): TRect;
var
  AHeight: Integer;
begin
  Result := ABounds;
  AHeight := CalculateTotalTextHeight;
  case TextParams.TextAlignY of
    taCenterY:
      Result := cxRectCenter(ABounds, cxRectWidth(ABounds), AHeight);
    taBottom:
      Result.Top := Result.Bottom - AHeight;
  end;
  if TextParams.PreventTopExceed then
    Result := cxRectSetTop(Result, Max(Result.Top, ABounds.Top));
end;

function TdxPSPDFExportHorizontalTextHelper.CalculateTextRowOutRect(
  ATextRow: PcxTextRow; var R: TRect; out ABreakExtraSpacing: Integer): TRect;
var
  APoint: TPoint;
  ASize: TSize;
begin
  APoint := R.TopLeft;
  ABreakExtraSpacing := 0;
  ASize := ATextRow.TextExtents;
  case TextParams.TextAlignX of
    taCenterX:
      APoint.X := Max(R.Left, (R.Right + R.Left - ASize.cx) div 2);
    taRight:
      APoint.X := R.Right - ASize.cx;
    taJustifyX, taDistributeX:
      begin
        ASize.cx := cxRectWidth(R);
        ABreakExtraSpacing := CalculateBreakExtractSpacing(ASize.cx, ATextRow);
      end;
  end;
  Inc(R.Top, ASize.cy);
  if TextParams.PreventLeftExceed then
    APoint.X := Max(APoint.X, R.Left);
  Result := Bounds(APoint.X, APoint.Y, ASize.cx, ASize.cy);
end;

{ TdxPSPDFExportVerticalTextHelper }

function TdxPSPDFExportVerticalTextHelper.CalculateTextRect(const ABounds: TRect): TRect;
var
  AHeight: Integer;
begin
  Result := cxRectRotate(ABounds);
  AHeight := CalculateTotalTextHeight;
  case TextParams.TextAlignY of
    taCenterY:
      Result := cxRectCenterHorizontally(Result, AHeight);
    taBottom:
      Result.Left := Result.Right - AHeight
  end;
end;

function TdxPSPDFExportVerticalTextHelper.CalculateTextRowOutRect(
  ATextRow: PcxTextRow; var R: TRect; out ABreakExtraSpacing: Integer): TRect;
var
  APoint: TPoint;
  ASize: TSize;
begin
  ABreakExtraSpacing := 0;
  APoint := Point(R.Left, R.Bottom);
  ASize := ATextRow.TextExtents;
  case TextParams.TextAlignX of
    taCenterX:
      APoint.Y := Max(R.Top, (R.Bottom + R.Top - ASize.cx) div 2);
    taRight:
      APoint.Y := R.Top + ATextRow.TextExtents.cx;
    taJustifyX, taDistributeX:
      begin
        ASize.cx := cxRectHeight(R);
        ABreakExtraSpacing := CalculateBreakExtractSpacing(ASize.cx, ATextRow);
      end;
  end;
  Inc(R.Left, ASize.cy);
  if TextParams.PreventLeftExceed then
    APoint.Y := Max(APoint.Y, R.Top);
  Result := Bounds(APoint.X, APoint.Y, ASize.cy, ASize.cx);
end;

function TdxPSPDFExportVerticalTextHelper.CalculateTextOutParams(
  const ABounds: TRect; AMaxLineCount: Integer; ALineSpacing: Single; const AText: string): TRect;
begin
  Result := inherited CalculateTextOutParams(cxRectRotate(ABounds), AMaxLineCount, ALineSpacing, AText);
end;

{ TdxPSPDFReportExportProvider }

constructor TdxPSPDFReportExportProvider.Create(AReportLink: TBasedxReportLink);
begin
  inherited Create(AReportLink);
  FOptions := TdxPSPDFReportExportOptions.Create;
  FPDFFile := TdxPSPDFFile.Create;
end;

destructor TdxPSPDFReportExportProvider.Destroy;
begin
  FreeAndNil(FOptions);
  FreeAndNil(FPDFFile);
  inherited Destroy;
end;

procedure TdxPSPDFReportExportProvider.AssignSettings(APDFFile: TdxPSPDFFile);
begin
  APDFFile.EmbedFonts := Options.EmbedFonts;
  APDFFile.UseCIDFonts := Options.UseCIDFonts;
  APDFFile.JPEGQuality := Options.JPEGQuality;
  APDFFile.CompressStreams := Options.CompressStreams;
  APDFFile.UseJPEGCompression := Options.UseJPEGCompression;
  APDFFile.DocumentInfo.Author := Options.Author;
  APDFFile.DocumentInfo.Creator := Options.Creator;
  APDFFile.DocumentInfo.Keywords := Options.Keywords;
  APDFFile.DocumentInfo.Subject := Options.Subject;
  APDFFile.DocumentInfo.Title := Options.Title;
  APDFFile.DocumentInfo.Producer := 'ExpressPrinting System';
  APDFFile.SecurityOptions.Assign(Options.SecurityOptions);
end;

procedure TdxPSPDFReportExportProvider.Build;
var
  APageIndex: Integer;
begin
  AssignSettings(PDFFile);
  ReportLink.IsCurrentLink := True;
  ReportLink.RebuildReport;

  case Options.PageRangeInfo.PageRanges of
    prCurrent:
      ExportPage(ReportLink.CurrentPage - 1);
    prAll:
      for APageIndex := 0 to ReportLink.PageCount - 1 do
        ExportPage(APageIndex);
    prRange:
      for APageIndex := 0 to Options.PageRangeInfo.PageIndexCount - 1 do
        ExportPage(Options.PageRangeInfo.PageIndex[APageIndex] - 1);
  end;
end;

procedure TdxPSPDFReportExportProvider.ExportPage(APageIndex: Integer);

  function GetPageSize(APageInfo: TdxPSPageRenderInfo): TPoint;
  begin
    Result := APageInfo.PageSize;
    Result.X := MulDiv(Result.X, APageInfo.RenderInfo.PointsPerInch, APageInfo.RenderInfo.UnitsPerInch);
    Result.Y := MulDiv(Result.Y, APageInfo.RenderInfo.PointsPerInch, APageInfo.RenderInfo.UnitsPerInch);
  end;

var
  ACanvas: TdxPSPDFReportRenderCanvas;
  APageSize: TPoint;
begin
  if (APageIndex >= 0) and (APageIndex < ReportLink.PageCount) then
  begin
    ACanvas := TdxPSPDFReportRenderCanvas.Create(Self);
    try
      APageSize := GetPageSize(RenderInfo.GetActualPageRenderInfo(APageIndex));
      Page := PDFFile.AddPage;
      Page.PageWidth := APageSize.X;
      Page.PageHeight := APageSize.Y;
      TBasedxReportLinkAccess(ReportLink).PaintPageEx(ACanvas, Page.PageBounds, APageIndex, APageIndex, 100);
    finally
      ACanvas.Free;
    end;
  end;
end;

procedure TdxPSPDFReportExportProvider.SaveToStream(AStream: TStream);
begin
  PDFFile.SaveToStream(AStream);
end;

procedure TdxPSPDFReportExportProvider.SetOptions(AOptions: TdxPSPDFReportExportOptions);
begin
  FOptions.Assign(AOptions);
end;

procedure TdxPSPDFReportExportProvider.SetPage(AValue: TdxPSPDFPage);
begin
  if FPage <> AValue then
  begin
    FPage := AValue;
    if FPage = nil then
      FPageContent := nil
    else
      FPageContent := FPage.Content;
  end;
end;

{ TdxPSPDFCanvasRegion }

function TdxPSPDFCanvasRegion.Clone: TdxPSPDFCanvasRegion;
begin
  Result := TdxPSPDFCanvasRegion.Create;
  Result.Combine(Handle, roSet);
  Result.XForm := XForm;
end;

{ TdxPSPDFCanvasClipRegion }

procedure TdxPSPDFCanvasClipRegion.AfterConstruction;
begin
  inherited AfterConstruction;
  FCanvasSize := cxSize(cxMaxRegionSize, cxMaxRegionSize);
end;

function TdxPSPDFCanvasClipRegion.Clone: TdxPSPDFCanvasClipRegion;
var
  I: Integer;
begin
  Result := TdxPSPDFCanvasClipRegion.Create;
  Result.CanvasSize := CanvasSize;
  Result.Capacity := Count;
  for I := 0 to Count - 1 do
    Result.Add(Items[I].Clone);
end;

function TdxPSPDFCanvasClipRegion.Contains(const R: TRect): Boolean;
begin
  Result := (Count = 0) or Last.RectInRegion(R);
end;

procedure TdxPSPDFCanvasClipRegion.Exclude(const R: TRect; const XForm: TXForm);
begin
  GetActualRegion(XForm).Combine(R, roSubtract);
end;

procedure TdxPSPDFCanvasClipRegion.Intersect(const R: TRect; const XForm: TXForm);
begin
  GetActualRegion(XForm).Combine(R, roIntersect);
end;

procedure TdxPSPDFCanvasClipRegion.IntersectEx(const R: TcxRegion; const XForm: TXForm);
begin
  GetActualRegion(XForm).Combine(R, roIntersect, False);
end;

function TdxPSPDFCanvasClipRegion.GetActualRegion(const XForm: TXForm): TdxPSPDFCanvasRegion;
begin
  if (Count > 0) and TXForm.IsEqual(XForm, Last.XForm) then
    Result := Last
  else
  begin
    Result := TdxPSPDFCanvasRegion.Create(cxRect(FCanvasSize));
    Result.XForm := XForm;
    Add(Result);
  end;
end;

procedure TdxPSPDFCanvasClipRegion.SetCanvasSize(const Value: TSize);
begin
  Clear;
  FCanvasSize := Value;
end;

{ TdxPSPDFReportRenderCanvas }

constructor TdxPSPDFReportRenderCanvas.Create(AProvider: TdxPSPDFReportExportProvider);
begin
  inherited Create;
  FProvider := AProvider;
  FSavedRegions := TcxObjectList.Create;
  FClipRegion := TdxPSPDFCanvasClipRegion.Create;
  FSavedWorldTransforms := TStack<TXForm>.Create;
  FWorldTransform := TXForm.CreateIdentityMatrix;
end;

destructor TdxPSPDFReportRenderCanvas.Destroy;
begin
  FreeAndNil(FSavedWorldTransforms);
  FreeAndNil(FSavedRegions);
  FreeAndNil(FClipRegion);
  inherited Destroy;
end;

function TdxPSPDFReportRenderCanvas.AbsoluteRect(const R: TRect): TRect;
begin
  Result := cxRectOffset(R, WindowOrg, False);
end;

function TdxPSPDFReportRenderCanvas.CalculateLineThickness(AUnitsPerPixel, AUnitsPerPoint: Integer): Integer;
begin
  Result := AUnitsPerPixel;
end;

function TdxPSPDFReportRenderCanvas.IsPrinterCanvas: Boolean;
begin
  Result := True;
end;

procedure TdxPSPDFReportRenderCanvas.DeviceToLogicalCoordinates(var R: TRect);
begin
  R := cxRectScale(R, PixelsNumerator, PixelsDenominator);
end;

procedure TdxPSPDFReportRenderCanvas.LogicalToDeviceCoordinates(var R: TRect);
begin
  R := cxRectScale(R, PixelsDenominator, PixelsNumerator);
end;

procedure TdxPSPDFReportRenderCanvas.SetCanvasExts(
  const APageSize: TPoint; AMappingMode: TdxPSReportRenderCanvasMappingMode;
  AScaleNumerator, AScaleDenominator: Integer; const AViewPort: TRect);
var
  AZoom: Double;
begin
  inherited SetCanvasExts(APageSize, AMappingMode, AScaleNumerator, AScaleDenominator, AViewPort);
  AZoom := AScaleDenominator / AScaleNumerator;
  FPageSize := Point(Round(APageSize.X / AZoom), Round(APageSize.Y / AZoom));
  PageContent.ScaleFactor := AZoom * cxRectWidth(AViewPort) / APageSize.X;
  ClipRegion.CanvasSize := cxSize(PageSize);
  UpdatePDFMatrix;
end;

procedure TdxPSPDFReportRenderCanvas.DrawPictureStretch(
  APicture: TGraphic; const R: TRect; ANumerator, ADenominator: Integer);
begin
  if IsRectVisible(R) then
  begin
    if APicture.InheritsFrom(TMetafile) then
      dxPDFExportMetaFile(Self, TMetafile(APicture), R)
    else
    begin
      PreparePageContent;
      try
        PageContent.DrawGraphic(R, APicture);
      finally
        UnpreparePageContent;
      end;
    end;
  end;
end;

procedure TdxPSPDFReportRenderCanvas.InternalDrawText(const ABounds: TRect; const AText: string;
  AFont: TFont; ATextColor: TColor; AFormat: TcxTextOutFormat; AMaxLineCount: Integer;
  AVertical: Boolean; ADirection: TcxVerticalTextOutDirection; ALineSpacing: Single = 1);

  function GetRotationAngle: TcxRotationAngle;
  begin
    if not AVertical then
      Result := ra0
    else
      if ADirection = vtdTopToBottom then
        Result := raMinus90
      else
        Result := raPlus90;
  end;

var
  ABreakExtraSpacing: Integer;
  ATextExportHelper: TdxPSPDFExportCustomTextHelper;
  ATextRow: PcxTextRow;
  ATextRowRect: TRect;
  I: Integer;
  R: TRect;
begin
  SaveState;
  try
    SelectFonT(AFont, ATextColor);
    ATextExportHelper := CreateTextExportHelper(Self, AFormat, AVertical);
    try
      R := ATextExportHelper.CalculateTextOutParams(ABounds, AMaxLineCount, ALineSpacing, AText);
      for I := 0 to ATextExportHelper.TextRowCount - 1 do
      begin
        ATextRow := ATextExportHelper.TextRow[I];
        ATextRowRect := ATextExportHelper.CalculateTextRowOutRect(ATextRow, R, ABreakExtraSpacing);
        if not IsRectEmpty(ATextRowRect) then
        begin
          DrawTextLine(ATextExportHelper.TextRowText[I], ATextRowRect, ABounds, ATextRow.TextExtents.cx,
            ATextColor, GetRotationAngle, ABreakExtraSpacing, ATextExportHelper.TextParams.NoClip);
        end;
      end;
    finally
      ATextExportHelper.Free;
    end;
  finally
    RestoreState;
  end;
end;

procedure TdxPSPDFReportRenderCanvas.DrawTextLine(const AText: string;
  const ATextRect, AClipRect: TRect; ATextWidth: Integer; ATextColor: TColor = clDefault;
  ADirection: TcxRotationAngle = ra0; ABreakExtraSpacing: Integer = 0; ANoClip: Boolean = False);
begin
  if IsRectVisible(ATextRect) then
  begin
    DrawTextLine(AText, CalculateTextOffset(dxRectF(ATextRect), ADirection),
      AClipRect, ATextWidth, ATextColor, DirectionAngle[ADirection], ABreakExtraSpacing, ANoClip);
  end;
end;

procedure TdxPSPDFReportRenderCanvas.DrawTextLine(
  const AText: string; const ATextOffset: TdxPointF; const AClipRect: TRect;
  ATextWidth: Integer; ATextColor: TColor; AAngle: Single; ABreakExtraSpacing: Integer; ANoClip: Boolean);

  function CalculateCharsSpacing: Double;
  var
    ALineThickness: Double;
  begin
    ALineThickness := 1 / PageContent.ScaleFactor;
    Result := Min(0, (ATextWidth - PageContent.TextWidth(AText)) / Length(AText));
    if Result < -ALineThickness then
    begin
      PageContent.AdjustFontSize(AText, ATextWidth);
      Result := 0;
    end;
  end;

  procedure DrawTextLineCore;
  begin
    PreparePageContent;
    try
      SelectFonT(Font, ATextColor);
      PageContent.DrawText(AText, ATextOffset, AAngle + Font.Orientation / 10, CalculateCharsSpacing, ABreakExtraSpacing);
    finally
      UnpreparePageContent;
    end;
  end;

begin
  if AText <> '' then
  begin
    if ANoClip then
      DrawTextLineCore
    else
    begin
      SaveClipRgn;
      try
        IntersectClipRgn(AClipRect);
        DrawTextLineCore;
      finally
        RestoreClipRgn;
      end;
    end;
  end;
end;

procedure TdxPSPDFReportRenderCanvas.DrawPictureTile(APicture: TGraphic;
  const R: TRect; APictureWidth, APictureHeight, ANumerator, ADenominator: Integer);
begin
  if IsRectVisible(R) then
  begin
    PreparePageContent;
    try
      PageContent.FillRectByGraphic(R,
        MulDiv(APictureWidth, ADenominator, ANumerator),
        MulDiv(APictureHeight, ADenominator, ANumerator), APicture);
    finally
      UnpreparePageContent
    end;
  end;
end;

procedure TdxPSPDFReportRenderCanvas.InternalFillRect(const R: TRect;
  ABackColor, AForeColor: TColor; APattern: TdxPSFillPatternClass; APatternBrush: TBrush);
var
  ABitmap: TcxBitmap;
begin
  if IsSolidFillPattern(APattern, APatternBrush) then
  begin
    if APatternBrush <> nil then
      ABackColor := APatternBrush.Color;
    PageContent.FillRect(R, ABackColor);
  end
  else
    if APatternBrush.Style <> bsSolid then
      PageContent.FillRectByBrush(R, APatternBrush.Color, APatternBrush.Style)
    else
    begin
      ABitmap := CreatePatternBitmap(ABackColor, AForeColor, R,
        Assigned(APattern) and APattern.RequiredBrushOrigin, APatternBrush);
      try
        PageContent.FillRectByGraphic(R, ABitmap.Width, ABitmap.Height, ABitmap);
      finally
        ABitmap.Free;
      end;
    end;
end;

function TdxPSPDFReportRenderCanvas.GetSupportsTransparentImagesDrawing: Boolean;
begin
  Result := False;
end;

function TdxPSPDFReportRenderCanvas.GetWorldTransform: TXForm;
begin
  Result := FWorldTransform;
end;

procedure TdxPSPDFReportRenderCanvas.SetWorldTransform(const Value: TXForm);
begin
  FWorldTransform := Value;
  FWorldTransformAssigned := not TXForm.IsIdentity(Value);
end;

procedure TdxPSPDFReportRenderCanvas.UpdatePDFMatrix;
begin
  FPDFMatrix := TXForm.CreateMatrix(1, 0, 0, -1, 0, PageContent.PageHeight);
end;

procedure TdxPSPDFReportRenderCanvas.DrawEllipseFrame(const R: TRect; AColor: TColor; AThickness: Integer);
begin
  if IsRectVisible(R) then
  begin
    PreparePageContent;
    try
      PageContent.DrawEllipseFrame(R, AColor, AThickness);
    finally
      UnpreparePageContent;
    end;
  end;
end;

procedure TdxPSPDFReportRenderCanvas.DrawExpandButton(R: TRect;
  AEdgeStyle: TdxCheckButtonEdgeStyle; AMarlettFont, ASymbolFont: TFont;
  AExpanded, AShadow, AFillInterior: Boolean; ABorderColor, ABackgroundColor: TColor;
  ALineThickness: Integer);

  procedure DrawExpandButtonBorders(const R: TRect; AEdgeStyle: TdxCheckButtonEdgeStyle);
  var
    ATopLeftColor, ARightBottomColor: TColor;
  begin
    if AEdgeStyle <> cbesNone then
    begin
      GetBorderColors(AEdgeStyle, ABorderColor, True, ARightBottomColor, ATopLeftColor);
      PageContent.DrawFrame(cxRectInflate(R, -ALineThickness, -ALineThickness),
        ALineThickness, ATopLeftColor, ARightBottomColor, cxBordersAll);
      if AEdgeStyle in [cbes3D, cbesSoft3D, cbesBoldFlat] then
      begin
        GetBorderColors(AEdgeStyle, ABorderColor, False, ARightBottomColor, ATopLeftColor);
        PageContent.DrawFrame(R, ALineThickness, ATopLeftColor, ARightBottomColor, cxBordersAll);
      end;
    end;
  end;

  procedure DrawExpandButtonGlyph(R: TRect; AExpanded: Boolean);
  begin
    InflateRect(R, -6 * ALineThickness, -6 * ALineThickness);
    PageContent.FillRect(cxRectCenter(R, cxRectWidth(R), ALineThickness), ABorderColor);
    if not AExpanded then
      PageContent.FillRect(cxRectCenter(R, ALineThickness, cxRectHeight(R)), ABorderColor);
  end;

begin
  if IsRectVisible(R) then
  begin
    PreparePageContent;
    try
      ALineThickness := ALineThickness div 2;
      if AFillInterior then
        PageContent.FillRect(cxRectInflate(R, -ALineThickness, -ALineThickness), ABackgroundColor);
      InflateRect(R, ALineThickness, ALineThickness);
      DrawExpandButtonBorders(R, AEdgeStyle);
      DrawExpandButtonGlyph(R, AExpanded);
      PageContent.Fill;
    finally
      UnpreparePageContent;
    end;
  end;
end;

procedure TdxPSPDFReportRenderCanvas.DrawFrame(const R: TRect; ATopLeftColor, ARightBottomColor: TColor;
  ABorderWidth: Integer = 1; ABorders: TcxBorders = cxBordersAll);
begin
  if IsRectVisible(R) and (ABorders <> []) and ((ATopLeftColor <> clNone) or (ARightBottomColor <> clNone)) then
  begin
    PreparePageContent;
    try
      PageContent.DrawFrame(R, ABorderWidth, ATopLeftColor, ARightBottomColor, ABorders);
    finally
      UnpreparePageContent;
    end;
  end;
end;

procedure TdxPSPDFReportRenderCanvas.DrawGraphic(AHelper: TdxDrawGraphicHelper; const R: TRect);
var
  ARect: TRect;
begin
  ARect := R;
  if IsRectVisible(ARect) then
  begin
    if AHelper.Graphic is TMetafile then
      dxPDFExportMetaFile(Self, TMetafile(AHelper.Graphic), ARect)
    else
      AHelper.Export(
        procedure (AGraphic: TGraphic; AMask: TcxRegion)
        begin
          PreparePageContent;
          try
            PageContent.DrawGraphic(ARect, AGraphic);
          finally
            UnpreparePageContent;
          end;
        end);
  end;
end;

procedure TdxPSPDFReportRenderCanvas.DrawRoundFrame(const R: TRect;
  AEllipseWidth, AEllipseHeight: Integer; AColor: TColor; AThickness: Integer);
begin
  if IsRectVisible(R) then
  begin
    PreparePageContent;
    try
      PageContent.DrawRoundFrame(R, AEllipseWidth, AEllipseHeight, AColor, AThickness);
    finally
      UnpreparePageContent;
    end;
  end;
end;

procedure TdxPSPDFReportRenderCanvas.ExtTextOut(const ABounds: TRect; AFont: TFont;
  const AText: string; AGlyphs: PWord; AGlyphCount: Integer; AGlyphWidths: PInteger);
begin
  SelectFonT(AFont, clDefault);
  DrawTextLine(AText, ABounds, ABounds, cxRectWidth(ABounds), clDefault, ra0, 0, True);
end;

procedure TdxPSPDFReportRenderCanvas.DrawText(var R: TRect; const AText: string; AFont: TFont;
  AFormat: DWORD; ATextColor: TColor = clDefault; AMaxLineCount: Integer = 0; ALineSpacing: Single = 1.0);
begin
  InternalDrawText(R, AText, AFont, ATextColor, AFormat, AMaxLineCount, False, vtdTopToBottom, ALineSpacing);
end;

procedure TdxPSPDFReportRenderCanvas.DoFillEdge(ARegion: TcxRegionHandle;
  ABackColor, AEdgeColor: TColor; AIsVerticalOrientation: Boolean; AEdgePattern: TClass);

  function CreatePattern: TdxPSPDFCustomPattern;
  begin
    Result := TdxPSPDFEdgePattern.Create(PageContent.PatternList,
      AEdgeColor, TdxPSEdgePatternClass(AEdgePattern), AIsVerticalOrientation);
    Result := PageContent.PatternList.AddPattern(Result);
  end;

var
  ARect: TRect;
begin
  if AEdgePattern.InheritsFrom(TdxPSEdgePattern) and (GetRgnBox(ARegion, ARect) <> NULLREGION) then
  begin
    SaveClipRgn;
    try
      if IntersectClipRgnEx(ARegion) then
      begin
        PreparePageContent;
        try
          PageContent.FillRectByPattern(ARect, CreatePattern);
        finally
          UnpreparePageContent;
        end;
      end;
    finally
      RestoreClipRgn;
    end;
  end;
end;

procedure TdxPSPDFReportRenderCanvas.FillEllipse(const R: TRect;
  ABackColor, AForeColor: TColor; APattern: TdxPSFillPatternClass; APatternBrush: TBrush);
begin
  if IsRectVisible(R) then
  begin
    PreparePageContent;
    try
      PageContent.WriteEllipse(R);
      PageContent.Clip;
      InternalFillRect(R, ABackColor, AForeColor, APattern, APatternBrush);
    finally
      UnpreparePageContent;
    end;
  end;
end;

procedure TdxPSPDFReportRenderCanvas.FillRect(const R: TRect; AColor: TColor);
begin
  FillRectEx(R, AColor, TdxPSSolidFillPattern, nil);
end;

procedure TdxPSPDFReportRenderCanvas.FillRectEx(const R: TRect;
  AColor: TColor; APattern: TdxPSFillPatternClass; APatternBrush: TBrush);
begin
  if IsRectVisible(R) and ((AColor <> clNone) or Assigned(APatternBrush)) then
  begin
    PreparePageContent;
    try
      InternalFillRect(R, AColor, clNone, APattern, APatternBrush);
    finally
      UnpreparePageContent;
    end;
  end;
end;

procedure TdxPSPDFReportRenderCanvas.FillRegion(ARegion: TcxRegionHandle;
  ABackColor, AForeColor: TColor; APattern: TdxPSFillPatternClass; APatternBrush: TBrush);
var
  ARect: TRect;
begin
  if GetRgnBox(ARegion, ARect) <> NULLREGION then
  begin
    SaveClipRgn;
    try
      if IntersectClipRgnEx(ARegion) then
      begin
        PreparePageContent;
        try
          InternalFillRect(ARect, ABackColor, AForeColor, APattern, APatternBrush);
        finally
          UnpreparePageContent;
        end;
      end;
    finally
      RestoreClipRgn;
    end;
  end;
end;

procedure TdxPSPDFReportRenderCanvas.FillRoundRect(R: TRect; AEllipseWidth, AEllipseHeight: Integer;
  ABackColor, AForeColor: TColor; APattern: TdxPSFillPatternClass; APatternBrush: TBrush);
begin
  if IsRectVisible(R) then
  begin
    PreparePageContent;
    try
      PageContent.WriteRoundRect(R, AEllipseWidth, AEllipseHeight);
      PageContent.Clip;
      InternalFillRect(R, ABackColor, AForeColor, APattern, APatternBrush);
    finally
      UnpreparePageContent;
    end;
  end;
end;

function TdxPSPDFReportRenderCanvas.IsRectVisible(const R: TRect): Boolean;
begin
  Result := not IsRectEmpty(R) and ClipRegion.Contains(AbsoluteRect(R));
end;

procedure TdxPSPDFReportRenderCanvas.Pie(const R: TRect; const APoint1, APoint2: TPoint; AColor: TColor);
begin
  if IsRectVisible(R) then
  begin
    PreparePageContent;
    try
      PageContent.Pie(R, APoint1, APoint2, AColor);
    finally
      UnpreparePageContent;
    end;
  end;
end;

procedure TdxPSPDFReportRenderCanvas.Polyline(const APoints: array of TPoint; AColor: TColor; ALineWidth: Integer);
begin
  if Length(APoints) > 1 then
  begin
    PreparePageContent;
    try
      PageContent.Polyline(APoints, ALineWidth, AColor);
    finally
      UnpreparePageContent;
    end;
  end;
end;

procedure TdxPSPDFReportRenderCanvas.Polygon(const APoints: array of TPoint;
  AColor, ABackgroundColor: TColor; ALineWidth: Integer; AFillMode: Integer = ALTERNATE);
begin
  if Length(APoints) > 1 then
  begin
    PreparePageContent;
    try
      PageContent.Polygon(APoints, ALineWidth, AColor, ABackgroundColor, AFillMode);
    finally
      UnpreparePageContent;
    end;
  end;
end;

procedure TdxPSPDFReportRenderCanvas.RotatedTextOut(const ABounds: TRect; const AText: string;
  AFont: TFont; AAlignHorz: TcxTextAlignX = taCenterX; AAlignVert: TcxTextAlignY = taCenterY;
  AWordBreak: Boolean = True; ADirection: TcxVerticalTextOutDirection = vtdBottomToTop);
var
  AFormat: Cardinal;
begin
  AFormat := cxMakeFormat(AAlignHorz, AAlignVert);
  if AWordBreak then
    AFormat := AFormat or CXTO_WORDBREAK;
  InternalDrawText(ABounds, AText, AFont, clDefault, AFormat, 0, True, ADirection);
end;

function TdxPSPDFReportRenderCanvas.CalculateTextRect(const AText: string;
  var R: TRect; AFormat: TcxTextOutFormat = CXTO_DEFAULT_FORMAT; AFont: TFont = nil;
  AMaxLineCount: Integer = 0; ALeftIndent: Integer = 0; ARightIndent: Integer = 0;
  ATextColor: TColor = clDefault; ALineSpacing: Single = 1.0): Integer;
var
  ABreakExtraSpacing: Integer;
  ATextExportHelper: TdxPSPDFExportCustomTextHelper;
  ATextRowRect: TRect;
  I: Integer;
  R1: TRect;
begin
  SaveState;
  try
    SelectFonT(AFont, ATextColor);
    ATextExportHelper := CreateTextExportHelper(Self, AFormat, False);
    try
      R1 := ATextExportHelper.CalculateTextOutParams(R, AMaxLineCount, ALineSpacing, AText);
      for I := 0 to ATextExportHelper.TextRowCount - 1 do
      begin
        ATextRowRect := ATextExportHelper.CalculateTextRowOutRect(ATextExportHelper.TextRow[I], R1, ABreakExtraSpacing);
        R.Top := Min(R.Top, ATextRowRect.Top);
        R.Right := Max(R.Right, ATextRowRect.Right);
        R.Bottom := Max(R.Bottom, ATextRowRect.Bottom);
        R.Left := Min(R.Left, ATextRowRect.Left);
      end;
      if AFormat and CXTO_AUTOINDENTS <> 0 then
      begin
        Inc(R.Bottom, cxTextSpace * ATextExportHelper.TextParams.OnePixel);
        Inc(R.Right, cxTextSpace * ATextExportHelper.TextParams.OnePixel);
      end;
      if AFormat and CXTO_CALCROWCOUNT = 0 then
        Result := cxRectHeight(R)
      else
        Result := ATextExportHelper.TextRowCount;
    finally
      ATextExportHelper.Free;
    end;
  finally
    RestoreState;
  end;
end;

function TdxPSPDFReportRenderCanvas.CalculateTextOffset(const ARect: TdxRectF; AAngle: TcxRotationAngle): TdxPointF;

  function GetPivotPoint(const ARect: TdxRectF; AAngle: TcxRotationAngle = ra0): TdxPointF;
  begin
    case AAngle of
      ra180:
        Result := ARect.BottomRight;
      raPlus90:
        Result := ARect.BottomLeft;
      raMinus90:
        Result := ARect.TopRight;
    else
      Result := ARect.TopLeft;
    end;
  end;

var
  AMatrix: TdxMatrix;
begin
  if AAngle = ra0 then
    Exit(ARect.TopLeft);

  AMatrix := TdxMatrix.Create;
  try
    AMatrix.Rotate(DirectionAngle[AAngle], GetPivotPoint(ARect, AAngle));
    Result := cxRectAdjustF(AMatrix.Transform(ARect)).TopLeft;
  finally
    AMatrix.Free;
  end;
end;

function TdxPSPDFReportRenderCanvas.GetPageContent: TdxPSPDFPageContent;
begin
  Result := Provider.PageContent;
end;

function TdxPSPDFReportRenderCanvas.ExcludeClipRect(const R: TRect): Integer;
begin
  ClipRegion.Exclude(AbsoluteRect(R), WorldTransform);
  Result := COMPLEXREGION;
end;

function TdxPSPDFReportRenderCanvas.IntersectClipRgn(const R: TRect): Integer;
begin
  ClipRegion.Intersect(AbsoluteRect(R), WorldTransform);
  Result := COMPLEXREGION;
end;

function TdxPSPDFReportRenderCanvas.IntersectClipRgnEx(ARegion: TcxRegionHandle): Boolean;
var
  ATempRegion: TcxRegion;
begin
  ATempRegion := TcxRegion.Create;
  try
    ATempRegion.Combine(ARegion, roSet);
    ATempRegion.Offset(cxPointInvert(WindowOrg));
    ClipRegion.IntersectEx(ATempRegion, WorldTransform);
    Result := True;
  finally
    ATempRegion.Free;
  end;
end;

function TdxPSPDFReportRenderCanvas.BeginCustomDraw(const AClipBounds: TRect; AFont: TFont; AColor: TColor): TCanvas;
begin
  SaveState;
  SaveClipRgn;
  FCustomDrawBounds := AClipBounds;
  PrepareCanvasForCustomDraw(AFont, AColor);
  FCustomDrawMetaFile := TMetafile.Create;
  FCustomDrawMetaFile.Width := cxRectWidth(FCustomDrawBounds);
  FCustomDrawMetaFile.Height := cxRectHeight(FCustomDrawBounds);
  FCustomDrawMetaFileCanvas := TMetafileCanvas.Create(FCustomDrawMetaFile, 0);
  FCustomDrawMetaFileCanvas.Brush.Assign(Brush);
  FCustomDrawMetaFileCanvas.Font.Assign(Font);
  SetWindowOrgEx(FCustomDrawMetaFileCanvas.Handle, FCustomDrawBounds.Left, FCustomDrawBounds.Top, nil);
  Result := FCustomDrawMetaFileCanvas;
  IntersectClipRgn(AClipBounds);
end;

procedure TdxPSPDFReportRenderCanvas.EndCustomDraw(var ACanvas: TCanvas);
begin
  FreeAndNil(FCustomDrawMetaFileCanvas);
  DrawPicture(FCustomDrawMetaFile, FCustomDrawBounds, ppmStretch, 1, 1);
  FreeAndNil(FCustomDrawMetaFile);
  RestoreClipRgn;
  RestoreState;
end;

procedure TdxPSPDFReportRenderCanvas.UnprepareLogicalUnits;
begin
  inherited UnprepareLogicalUnits;
  PageContent.ScaleFactor := 1;
  UpdatePDFMatrix;
end;

procedure TdxPSPDFReportRenderCanvas.PreparePageContent;

  function ConvertMatrix(const X: TXForm): TXForm;
  begin
    Result := TXForm.Combine(TXForm.Combine(FPDFMatrix, X), FPDFMatrix);
  end;

  function IsComplexClipRegion: Boolean;
  begin
    Result := (ClipRegion.Count > 1) or not TXForm.IsIdentity(ClipRegion.Last.XForm);
  end;

  procedure SelectComplexClipRegion;
  var
    AIndex: Integer;
    ATransform: TXForm;
  begin
    for AIndex := 0 to ClipRegion.Count - 1 do
    begin
      PageContent.ModifyWorldTransform(ConvertMatrix(ClipRegion[AIndex].XForm));
      PageContent.SelectClipRegion(ClipRegion[AIndex].Handle);
    end;
    ATransform := TXForm.CreateIdentityMatrix;
    for AIndex := ClipRegion.Count - 1 downto 0 do
      ATransform := TXForm.Combine(ATransform, InvertXForm(ConvertMatrix(ClipRegion[AIndex].XForm)));
    PageContent.ModifyWorldTransform(ATransform);
  end;

begin
  PageContent.SaveState;
  if ClipRegion.Count > 0 then
  begin
    if IsComplexClipRegion then
      SelectComplexClipRegion
    else
      PageContent.SelectClipRegion(ClipRegion.First.Handle);
  end;
  PageContent.ModifyWorldTransform(TXForm.CreateTranslateMatrix(-WindowOrg.X, WindowOrg.Y));
  if WorldTransformAssigned then
    PageContent.ModifyWorldTransform(ConvertMatrix(WorldTransform));
end;

procedure TdxPSPDFReportRenderCanvas.UnpreparePageContent;
begin
  PageContent.RestoreState;
end;

procedure TdxPSPDFReportRenderCanvas.RestoreClipRgn;
begin
  ClipRegion := TdxPSPDFCanvasClipRegion(FSavedRegions.Last);
  FSavedRegions.Delete(FSavedRegions.Count - 1);
end;

procedure TdxPSPDFReportRenderCanvas.RestoreState;
begin
  inherited RestoreState;
  FWorldTransform := FSavedWorldTransforms.Pop;
  RestoreClipRgn;
end;

procedure TdxPSPDFReportRenderCanvas.SaveClipRgn;
begin
  FSavedRegions.Add(ClipRegion.Clone);
end;

procedure TdxPSPDFReportRenderCanvas.SaveState;
begin
  SaveClipRgn;
  FSavedWorldTransforms.Push(FWorldTransform);
  inherited SaveState;
end;

procedure TdxPSPDFReportRenderCanvas.SelectFonT(AFont: TFont; ATextColor: TColor);

  function GetActualFontSize(AFont: TFont): Integer;
  var
    ATempFont: TFont;
  begin
    if AFont.Height < 0 then
      Result := -AFont.Height
    else
    begin
      ATempFont := TFont.Create;
      try
        ATempFont.Assign(AFont);
        ATempFont.Size := -AFont.Size;
        Result := -MulDiv(AFont.Size, cxTextHeight(ATempFont), cxTextHeight(AFont));
      finally
        ATempFont.Free;
      end;
    end;
  end;

  function GetActualTextColor(ATextColor: TColor): TColor;
  begin
    Result := ATextColor;
    if (Result = clDefault) then
      Result := Font.Color;
    if (Result = clDefault) or (Result = clNone) then
      Result := clWindowText;
  end;

begin
  Font := AFont;
  PageContent.Font := Provider.PDFFile.AddFont(Font);
  PageContent.FontColor := GetActualTextColor(ATextColor);
  PageContent.FontSize := GetActualFontSize(Font);
end;

procedure TdxPSPDFReportRenderCanvas.SetClipRegion(const Value: TdxPSPDFCanvasClipRegion);
begin
  if FClipRegion <> Value then
  begin
    FreeAndNil(FClipRegion);
    FClipRegion := Value;
  end;
end;

end.
