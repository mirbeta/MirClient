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

unit dxPSRichEditControlLnk.Bricks;

{$I cxVer.inc}

{.$DEFINE DXLOGGING}

interface

uses
{$IFDEF DELPHIXE2}
  System.UITypes,
{$ENDIF}
  Types, Windows, Messages, Classes, SysUtils, Graphics, Generics.Defaults, Generics.Collections, Controls,
  StdCtrls, Forms, ActiveX, dxCoreClasses, cxGeometry, cxLookAndFeels, cxGraphics, cxControls, cxClasses, dxCore,
  dxMessages, dxGDIPlusAPI, dxGDIPlusClasses, cxLookAndFeelPainters, dxCoreGraphics, dxPSCore, dxPSReportRenderCanvas,
  dxPSEdgePatterns,

  dxRichEdit.Types,
  dxRichEdit.Utils.Types,
  dxRichEdit.Utils.Graphics,
  dxRichEdit.Utils.OfficeImage,
  dxRichEdit.Utils.Properties,
  dxRichEdit.Utils.TextColors,
  dxRichEdit.InnerControl,
  dxRichEdit.View.Core,
  dxRichEdit.View.ViewInfo,
  dxRichEdit.View.PrintLayout,
  dxRichEdit.Platform.Win.Painter,
  dxRichEdit.Platform.Win.Control,
  dxRichEdit.Platform.Font,
  dxRichEdit.Platform.PatternLinePainter,
  dxRichEdit.LayoutEngine.DocumentFormatter,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.Section,
  dxRichEdit.DocumentModel.Styles,
  dxRichEdit.DocumentModel.PatternLine,
  dxRichEdit.DocumentLayout,
  dxRichEdit.DocumentLayout.UnitConverter,
  dxRichEdit.DocumentModel.CharacterFormatting,
  dxRichEdit.DocumentModel.ParagraphFormatting,
  dxRichEdit.DocumentLayout.CommentPadding,
  dxRichEdit.DocumentLayout.Painters;

type
  { TdxOfficeBrick }

  TdxOfficeBrick = class(TdxReportCell)
  public
    constructor Create(AParent: TdxReportCell); override;
    procedure DrawNestedDataItems(ACanvas: TdxPSReportRenderCustomCanvas;
      const OriginRect: TRect; AStage: TdxPSRenderStages); override;
  end;

  { TdxOfficePanelBrick }

  TdxOfficePanelBrick = class(TdxOfficeBrick)
  strict private
    FAbsoluteLocation: TdxPointF;
  public
    property AbsoluteLocation: TdxPointF read FAbsoluteLocation write FAbsoluteLocation;
  end;

  { TdxOfficeRectBrick }

  TdxOfficeRectBrick = class(TAbstractdxReportCellData)
  public
    constructor Create(AParent: TdxReportCell); override;
    procedure DrawContent(ACanvas: TdxPSReportRenderCustomCanvas; AStage: TdxPSRenderStages); override;
  end;

  { TdxOfficeImageBrick }

  TdxOfficeImageBrick = class(TdxReportCellGraphic)
  public
    constructor Create(AParent: TdxReportCell); override;
    procedure DrawContent(ACanvas: TdxPSReportRenderCustomCanvas; AStage: TdxPSRenderStages); override;
  end;

  { TdxOfficeTextBrick }

  TdxOfficeTextBrick = class(TAbstractdxReportCellData)
  strict private
    FGlyphs: array of Word;
    FGlyphWidths: array of Integer;
    FText: string;
  protected
    procedure ConvertCoords(APixelsNumerator, APixelsDenominator: Integer); override;
    function GetDefaultDTFormat: Cardinal; override;
    procedure ReadData(AReader: TdxPSDataReader); override;
    procedure WriteData(AWriter: TdxPSDataWriter); override;
  public
    constructor Create(AParent: TdxReportCell); override;
    procedure DrawContent(ACanvas: TdxPSReportRenderCustomCanvas; AStage: TdxPSRenderStages); override;
    procedure DrawText(ACanvas: TdxPSReportRenderCustomCanvas); virtual;
    function HasText: Boolean;
    procedure SetText(const AText: string; AGlyphs: PWord; AGlyphWidths: PInteger; AGlyphCount: Integer); overload;
    procedure SetText(const AText: string); overload;
    //
    property Text: string read FText;
  end;

  { TdxLineBrick }

  TdxLineBrick = class(TAbstractdxReportCellData)
  protected
    function GetPatternSize: Integer; virtual;
    function IsDrawn(ACanvas: TdxPSReportRenderCustomCanvas; AStage: TdxPSRenderStages; const ARect: TRect): Boolean; override;
  public
    constructor Create(AParent: TdxReportCell); override;
    //
    class function CreateStrikeoutBrick(AParent: TdxReportCell; ALineType: TdxStrikeoutType): TdxLineBrick;
    class function CreateUnderlineBrick(AParent: TdxReportCell; ALineType: TdxUnderlineType; AHorizontal: Boolean): TdxLineBrick;
    //
    property PatternSize: Integer read GetPatternSize;
  end;

  { TdxPatternLineBrick }

  TdxPatternLineBrick = class(TdxLineBrick)
  strict private const
    PatternDPI = 96;
  strict private
    FHorizontal: Boolean;
    FPattern: array of Word;
    FThickness: Integer;
  protected
    function GetPatternSize: Integer; override;
  public
    constructor Create(AParent: TdxReportCell; const APattern: array of Word; AThickness: Integer; AHorizontal: Boolean); reintroduce;
    procedure DrawContent(ACanvas: TdxPSReportRenderCustomCanvas; AStage: TdxPSRenderStages); override;
  end;

  { TdxSolidLineBrick }

  TdxSolidLineBrick = class(TdxLineBrick)
  strict private
    FLineCount: Integer;
  protected
    function GetPatternSize: Integer; override;
  public
    constructor Create(AParent: TdxReportCell; ALineCount: Integer); reintroduce;
    procedure DrawContent(ACanvas: TdxPSReportRenderCustomCanvas; AStage: TdxPSRenderStages); override;
    //
    property LineCount: Integer read FLineCount;
  end;

  { TdxWavyLineBrick }

  TdxWavyLineBrick = class(TdxLineBrick)
  strict private
    FHorizontal: Boolean;
    FLineWidth: Integer;
  protected
    procedure CalculatePoints(const R: TRect; out APoints: TPoints);
    function GetPatternSize: Integer; override;

    property Horizontal: Boolean read FHorizontal;
  public
    constructor Create(AParent: TdxReportCell; ALineWidth: Integer; AHorizontal: Boolean); reintroduce;
    procedure DrawContent(ACanvas: TdxPSReportRenderCustomCanvas; AStage: TdxPSRenderStages); override;
    //
    property LineWidth: Integer read FLineWidth;
  end;

  { TdxDoubleWavyLineBrick }

  TdxDoubleWavyLineBrick = class(TdxWavyLineBrick)
  protected
    function GetPatternSize: Integer; override;
  public
    procedure DrawContent(ACanvas: TdxPSReportRenderCustomCanvas; AStage: TdxPSRenderStages); override;
  end;

implementation

uses
  Math, Dialogs, ShellAPI, ComObj, ShlObj, UxTheme, DwmApi, dxTypeHelpers, cxDrawTextUtils,
  dxPSExcelEdgePatterns,

  dxRichEdit.LayoutEngine.BoxMeasurer,
  dxRichEdit.Utils.Mouse,
  dxRichEdit.Platform.Win.Font,
  dxRichEdit.Platform.Win.FontCache,
  dxRichEdit.DocumentModel.Borders,
  dxRichEdit.DocumentModel.DocumentProperties,
  dxRichEdit.DocumentModel.Fields,
  dxRichEdit.DocumentModel.UnitConverter,
  dxRichEdit.DocumentModel.UnitToLayoutUnitConverter;

{ TdxOfficeBrick }

constructor TdxOfficeBrick.Create(AParent: TdxReportCell);
begin
  inherited Create(AParent);
  CellSides := [];
  Transparent := True;
end;

procedure TdxOfficeBrick.DrawNestedDataItems(
  ACanvas: TdxPSReportRenderCustomCanvas; const OriginRect: TRect; AStage: TdxPSRenderStages);
var
  AItem: TAbstractdxReportCellData;
  I: Integer;
begin
  for I := 0 to DataItemCount - 1 do
  begin
    AItem := DataItems[I];
    AItem.DrawContent(ACanvas, AStage);
  end;
end;

{ TdxOfficeRectBrick }

constructor TdxOfficeRectBrick.Create(AParent: TdxReportCell);
begin
  inherited Create(AParent);
  CellSides := [];
  TextAlignY := taTop;
end;

procedure TdxOfficeRectBrick.DrawContent(ACanvas: TdxPSReportRenderCustomCanvas; AStage: TdxPSRenderStages);
begin
  if rsSecondPass in AStage then
    ACanvas.FillRect(BoundsRect, Color);
end;

{ TdxOfficeImageBrick }

constructor TdxOfficeImageBrick.Create(AParent: TdxReportCell);
begin
  inherited Create(AParent);
  ImageBuffering := cibNone;
  ImageTransparent := False;
  Stretch := True;
  CellSides := [];
end;

procedure TdxOfficeImageBrick.DrawContent(ACanvas: TdxPSReportRenderCustomCanvas; AStage: TdxPSRenderStages);
var
  ASaveSmoothlyStretchImages: Boolean;
begin
  ASaveSmoothlyStretchImages := FSmoothlyStretchImages;
  FSmoothlyStretchImages := True;
  try
    inherited DrawContent(ACanvas, AStage);
  finally
    FSmoothlyStretchImages := ASaveSmoothlyStretchImages;
  end;
end;

{ TdxOfficeTextBrick }

constructor TdxOfficeTextBrick.Create(AParent: TdxReportCell);
begin
  inherited Create(AParent);
  CellSides := [];
  TextAlignY := taTop;
end;

procedure TdxOfficeTextBrick.DrawContent(ACanvas: TdxPSReportRenderCustomCanvas; AStage: TdxPSRenderStages);
begin
  inherited DrawContent(ACanvas, AStage);

  if rsSecondPass in AStage then
  begin
    if HasText then
      DrawText(ACanvas);
  end;
end;

procedure TdxOfficeTextBrick.DrawText(ACanvas: TdxPSReportRenderCustomCanvas);
begin
  ACanvas.ExtTextOut(BoundsRect, Font, Text, @FGlyphs[0], Length(FGlyphs), @FGlyphWidths[0]);
end;

procedure TdxOfficeTextBrick.SetText(const AText: string; AGlyphs: PWord; AGlyphWidths: PInteger; AGlyphCount: Integer);
begin
  FText := AText;
  SetLength(FGlyphs, AGlyphCount);
  SetLength(FGlyphWidths, AGlyphCount);
  Move(AGlyphs^, FGlyphs[0], AGlyphCount * SizeOf(Word));
  Move(AGlyphWidths^, FGlyphWidths[0], AGlyphCount * SizeOf(Integer));
end;

procedure TdxOfficeTextBrick.SetText(const AText: string);
begin
  FText := AText;
  FGlyphs := nil;
  FGlyphWidths := nil;
end;

procedure TdxOfficeTextBrick.ConvertCoords(APixelsNumerator, APixelsDenominator: Integer);
var
  I: Integer;
begin
  inherited ConvertCoords(APixelsNumerator, APixelsDenominator);
  for I := 0 to Length(FGlyphWidths) - 1 do
    FGlyphWidths[I] := MulDiv(FGlyphWidths[I], APixelsNumerator, APixelsDenominator);
end;

function TdxOfficeTextBrick.GetDefaultDTFormat: Cardinal;
begin
  Result := CXTO_PATTERNEDTEXT or CXTO_NOCLIP;
end;

function TdxOfficeTextBrick.HasText: Boolean;
begin
  Result := Length(FGlyphs) > 0;
end;

procedure TdxOfficeTextBrick.ReadData(AReader: TdxPSDataReader);
var
  ACount: Integer;
  I: Integer;
begin
  inherited ReadData(AReader);
  ACount := AReader.ReadInteger;
  SetLength(FGlyphWidths, ACount);
  SetLength(FGlyphs, ACount);
  for I := 0 to ACount - 1 do
    FGlyphs[I] := AReader.ReadInteger;
  for I := 0 to ACount - 1 do
    FGlyphWidths[I] := AReader.ReadInteger;
  FText := AReader.ReadString;
end;

procedure TdxOfficeTextBrick.WriteData(AWriter: TdxPSDataWriter);
var
  I: Integer;
begin
  inherited WriteData(AWriter);
  AWriter.WriteInteger(Length(FGlyphWidths));
  for I := 0 to Length(FGlyphs) - 1 do
    AWriter.WriteInteger(FGlyphs[I]);
  for I := 0 to Length(FGlyphWidths) - 1 do
    AWriter.WriteInteger(FGlyphWidths[I]);
  AWriter.WriteString(Text);
end;

{ TdxLineBrick }

constructor TdxLineBrick.Create(AParent: TdxReportCell);
begin
  inherited Create(AParent);
  CellSides := [];
  PreventAutoIndents := True;
  PreventLeftTextExceed := False;
  PreventTopTextExceed := False;
end;

class function TdxLineBrick.CreateStrikeoutBrick(AParent: TdxReportCell; ALineType: TdxStrikeoutType): TdxLineBrick;
begin
  case ALineType of
    TdxStrikeoutType.Single:
      Result := TdxSolidLineBrick.Create(AParent, 1);
    TdxStrikeoutType.Double:
      Result := TdxSolidLineBrick.Create(AParent, 2);
  else // TdxStrikeoutType.None
    Result := TdxLineBrick.Create(AParent);
  end;
end;

class function TdxLineBrick.CreateUnderlineBrick(AParent: TdxReportCell; ALineType: TdxUnderlineType; AHorizontal: Boolean): TdxLineBrick;
begin
  case ALineType of
    TdxUnderlineType.Single:
      Result := TdxSolidLineBrick.Create(AParent, 1);
    TdxUnderlineType.Double:
      Result := TdxSolidLineBrick.Create(AParent, 2);
    TdxUnderlineType.Dotted:
      Result := TdxPatternLineBrick.Create(AParent, [2, 2], 1, AHorizontal);
    TdxUnderlineType.ThickDotted:
      Result := TdxPatternLineBrick.Create(AParent, [2, 2], 2, AHorizontal);
    TdxUnderlineType.DashSmallGap:
      Result := TdxPatternLineBrick.Create(AParent, [8, 2], 1, AHorizontal);
    TdxUnderlineType.Dashed:
      Result := TdxPatternLineBrick.Create(AParent, [8, 4], 1, AHorizontal);
    TdxUnderlineType.ThickDashed:
      Result := TdxPatternLineBrick.Create(AParent, [8, 4], 2, AHorizontal);
    TdxUnderlineType.ThickSingle:
      Result := TdxSolidLineBrick.Create(AParent, 2);
    TdxUnderlineType.DashDotted:
      Result := TdxPatternLineBrick.Create(AParent, [8, 2, 3, 2], 1, AHorizontal);
    TdxUnderlineType.ThickDashDotted:
      Result := TdxPatternLineBrick.Create(AParent, [8, 2, 3, 2], 2, AHorizontal);
    TdxUnderlineType.DashDotDotted:
      Result := TdxPatternLineBrick.Create(AParent, [6, 2, 2, 2, 2, 2], 1, AHorizontal);
    TdxUnderlineType.ThickDashDotDotted:
      Result := TdxPatternLineBrick.Create(AParent, [6, 2, 2, 2, 2, 2], 2, AHorizontal);
    TdxUnderlineType.LongDashed:
      Result := TdxPatternLineBrick.Create(AParent, [16, 8], 1, AHorizontal);
    TdxUnderlineType.ThickLongDashed:
      Result := TdxPatternLineBrick.Create(AParent, [16, 8], 2, AHorizontal);
    TdxUnderlineType.Wave:
      Result := TdxWavyLineBrick.Create(AParent, 1, AHorizontal);
    TdxUnderlineType.HeavyWave:
      Result := TdxWavyLineBrick.Create(AParent, 2, AHorizontal);
    TdxUnderlineType.DoubleWave:
      Result := TdxDoubleWavyLineBrick.Create(AParent, 1, AHorizontal);
  else // TdxUnderlineType.None
    Result := TdxLineBrick.Create(AParent);
  end;
end;

function TdxLineBrick.GetPatternSize: Integer;
begin
  Result := 0;
end;

function TdxLineBrick.IsDrawn(ACanvas: TdxPSReportRenderCustomCanvas;
  AStage: TdxPSRenderStages; const ARect: TRect): Boolean;
begin
  Result := rsFirstPass in AStage;
end;

{ TdxSolidLineBrick }

constructor TdxSolidLineBrick.Create(AParent: TdxReportCell; ALineCount: Integer);
begin
  inherited Create(AParent);
  FLineCount := ALineCount;
end;

procedure TdxSolidLineBrick.DrawContent(ACanvas: TdxPSReportRenderCustomCanvas; AStage: TdxPSRenderStages);
var
  ALineRect: TRect;
  ALineThickness: Integer;
  I: Integer;
begin
  ALineThickness := Height div PatternSize;
  ALineRect := cxRectSetHeight(BoundsRect, ALineThickness);
  for I := 0 to LineCount - 1 do
  begin
    ACanvas.FillRect(ALineRect, Color);
    OffsetRect(ALineRect, 0, 2 * ALineThickness);
  end;
end;

function TdxSolidLineBrick.GetPatternSize: Integer;
begin
  Result := 2 * LineCount - 1;
end;

{ TdxPatternLineBrick }

constructor TdxPatternLineBrick.Create(AParent: TdxReportCell; const APattern: array of Word; AThickness: Integer;
  AHorizontal: Boolean);
var
  I: Integer;
begin
  inherited Create(AParent);
  FThickness := AThickness;
  FHorizontal := AHorizontal;
  dxTestCheck(Length(APattern) mod 2 = 0, ClassName);

  SetLength(FPattern, Length(APattern));
  for I := 0 to Length(APattern) - 1 do
    FPattern[I] := APattern[I];
end;

procedure TdxPatternLineBrick.DrawContent(ACanvas: TdxPSReportRenderCustomCanvas; AStage: TdxPSRenderStages);
var
  AIndex: Integer;
  ARect, ADrawRect: TRect;
  ASize: Integer;
begin
  AIndex := 0;
  ARect := BoundsRect;
  if FHorizontal then
  begin
    while ARect.Left < ARect.Right do
    begin
      ASize := MulDiv(FPattern[AIndex], PixelsNumerator, PatternDPI);
      ADrawRect.Init(ARect.Left, ARect.Top, Min(ARect.Left + ASize, ARect.Right), ARect.Bottom);
      ACanvas.FillRect(ADrawRect, Color);
      Inc(ASize, MulDiv(FPattern[AIndex + 1], PixelsNumerator, PatternDPI));
      Inc(ARect.Left, ASize);
      AIndex := (AIndex + 2) mod Length(FPattern);
    end;
  end
  else
  begin
    while ARect.Top < ARect.Bottom do
    begin
      ASize := MulDiv(FPattern[AIndex], PixelsNumerator, PatternDPI);
      ADrawRect.Init(ARect.Left, ARect.Top, ARect.Right, Min(ARect.Top + ASize, ARect.Bottom));
      ACanvas.FillRect(ADrawRect, Color);
      Inc(ASize, MulDiv(FPattern[AIndex + 1], PixelsNumerator, PatternDPI));
      Inc(ARect.Top, ASize);
      AIndex := (AIndex + 2) mod Length(FPattern);
    end;
  end;
end;

function TdxPatternLineBrick.GetPatternSize: Integer;
begin
  Result := FThickness;
end;

{ TdxWavyLineBrick }

constructor TdxWavyLineBrick.Create(AParent: TdxReportCell; ALineWidth: Integer; AHorizontal: Boolean);
begin
  inherited Create(AParent);
  FLineWidth := ALineWidth;
  FHorizontal := AHorizontal;
end;

procedure TdxWavyLineBrick.DrawContent(ACanvas: TdxPSReportRenderCustomCanvas; AStage: TdxPSRenderStages);
var
  APoints: TPoints;
begin
  CalculatePoints(BoundsRect, APoints);
  ACanvas.Polyline(APoints, Color, LineWidth * LineThickness);
end;

procedure TdxWavyLineBrick.CalculatePoints(const R: TRect; out APoints: TPoints);
var
  ACount: Integer;
  ASum: Integer;
  I, X, Y: Integer;
begin
  X := R.Left;
  Y := R.Top;
  if Horizontal then
  begin
    ACount := Math.Ceil(R.Width / R.Height);
    ASum := Y + R.Bottom;
    SetLength(APoints, ACount);
    for I := 0 to ACount - 1 do
    begin
      APoints[I] := cxPoint(X, Y);
      Y := ASum - Y;
      Inc(X, R.Height);
    end;
  end
  else
  begin
    ACount := Math.Ceil(R.Height / R.Width);
    ASum := X + R.Right;
    SetLength(APoints, ACount);
    for I := 0 to ACount - 1 do
    begin
      APoints[I] := cxPoint(X, Y);
      X := ASum - X;
      Inc(Y, R.Width);
    end;
  end;
end;

function TdxWavyLineBrick.GetPatternSize: Integer;
begin
  Result := 3;
end;

{ TdxDoubleWavyLineBrick }

procedure TdxDoubleWavyLineBrick.DrawContent(ACanvas: TdxPSReportRenderCustomCanvas; AStage: TdxPSRenderStages);
var
  APoints: TPoints;
  ARect: TRect;
begin
  ARect := cxRectSetHeight(BoundsRect, Height div 2);
  CalculatePoints(ARect, APoints);
  ACanvas.Polyline(APoints, Color, LineThickness);
  if Horizontal then
    ARect.Offset(0, ARect.Height - 1)
  else
    ARect.Offset(ARect.Width - 1, 0);
  CalculatePoints(ARect, APoints);
  ACanvas.Polyline(APoints, Color, LineThickness);
end;

function TdxDoubleWavyLineBrick.GetPatternSize: Integer;
begin
  Result := 5;
end;

initialization
end.
