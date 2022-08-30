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

unit dxRichEdit.Platform.PatternLinePainter;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Windows, Forms, SysUtils, Graphics, Types, Generics.Defaults, Generics.Collections, dxCore,
  dxCoreClasses, dxCoreGraphics, cxGeometry, cxGraphics, dxGDIPlusAPI, dxGDIPlusClasses,

  dxRichEdit.Utils.Graphics,
  dxRichEdit.Utils.Types,
  dxRichEdit.DocumentModel.CharacterFormatting,
  dxRichEdit.DocumentModel.UnitConverter,
  dxRichEdit.DocumentLayout.UnitConverter,
  dxRichEdit.DocumentModel.PatternLine;

type

  { IdxPatternLinePaintingSupport }

  IdxPatternLinePaintingSupport = interface
  ['{CA9A0CED-1580-49FB-AF27-1912870E7895}']
    function GetPen(AColor: TdxAlphaColor): TdxGPColorPen; overload;
    function GetPen(AColor: TdxAlphaColor; AThickness: Single): TdxGPColorPen; overload;
    procedure ReleasePen(APen: TdxGPPen);
    function GetBrush(AColor: TdxAlphaColor): TdxGPBrush;
    procedure ReleaseBrush(ABrush: TdxGPBrush);
    procedure DrawLine(APen: TdxGPPen; X1, Y1, X2, Y2: Single);
    procedure DrawLines(APen: TdxGPPen; const APoints: TArray<TdxPointF>);
  end;

  { TdxPatternLinePainterParameters }

  TdxPatternLinePainterParameters = class abstract
  private
    FDotPattern: TArray<Single>;
    FDashPattern: TArray<Single>;
    FDashSmallGapPattern: TArray<Single>;
    FDashDotPattern: TArray<Single>;
    FDashDotDotPattern: TArray<Single>;
    FLongDashPattern: TArray<Single>;
    FPixelPenWidth: Single;
    FPixelStep: Single;
  protected
    function CreatePattern(const APattern: array of Single; ADpiX: Single): TArray<Single>;
    function PixelsToUnits(AValue, ADpi: Single): Single; virtual; abstract;
  public
    destructor Destroy; override;
    procedure Initialize(ADpiX: Single); virtual;

    property DotPattern: TArray<Single> read FDotPattern;
    property DashPattern: TArray<Single> read FDashPattern;
    property DashSmallGapPattern: TArray<Single> read FDashSmallGapPattern;
    property DashDotPattern: TArray<Single> read FDashDotPattern;
    property DashDotDotPattern: TArray<Single> read FDashDotDotPattern;
    property LongDashPattern: TArray<Single> read FLongDashPattern;
    property PixelPenWidth: Single read FPixelPenWidth;
    property PixelStep: Single read FPixelStep;
  end;

  { TdxRichEditPatternLinePainterParameters }

  TdxRichEditPatternLinePainterParameters = class(TdxPatternLinePainterParameters)
  private
    FUnitConverter: TdxDocumentLayoutUnitConverter;
    FSectionStartPattern: TArray<Single>;
    FPageBreakPattern: TArray<Single>;
    FColumnBreakPattern: TArray<Single>;
  protected
    function PixelsToUnits(AValue, ADpi: Single): Single; override;
  public
    constructor Create(AUnitConverter: TdxDocumentLayoutUnitConverter);
    destructor Destroy; override;
    procedure Initialize(ADpiX: Single); override;

    property SectionStartPattern: TArray<Single> read FSectionStartPattern;
    property PageBreakPattern: TArray<Single> read FPageBreakPattern;
    property ColumnBreakPattern: TArray<Single> read FColumnBreakPattern;
  end;


  { TdxPatternLinePainterParameters }

  TdxPatternLinePainter = class abstract (TcxIUnknownObject)
  private
    FPainter: IdxPatternLinePaintingSupport;
    FUnitConverter: TdxDocumentLayoutUnitConverter;
    function GetDashDotPattern: TArray<Single>;
    function GetDotPattern: TArray<Single>;
    function GetDashPattern: TArray<Single>;
    function GetDashSmallGapPattern: TArray<Single>;
    function GetDashDotDotPattern: TArray<Single>;
    function GetLongDashPattern: TArray<Single>;
    function GetPixelPenWidth: Single;
    function GetPixelStep: Single;
  protected
    function GetParameters: TdxPatternLinePainterParameters; virtual; abstract;
    function GetPixelGraphics: TdxGraphics; virtual; abstract;
    function UnitsToPixels(AVal, ADpi: Single): Single; virtual;
    function PixelsToUnits(AVal, ADpi: Single): Single; virtual;
    function RoundToPixels(AVal, ADpi: Single): Single; virtual;

    property DotPattern: TArray<Single> read GetDotPattern;
    property DashPattern: TArray<Single> read GetDashPattern;
    property DashSmallGapPattern: TArray<Single> read GetDashSmallGapPattern;
    property DashDotPattern: TArray<Single> read GetDashDotPattern;
    property DashDotDotPattern: TArray<Single> read GetDashDotDotPattern;
    property LongDashPattern: TArray<Single> read GetLongDashPattern;
    property PixelPenWidth: Single read GetPixelPenWidth;
    property PixelStep: Single read GetPixelStep;
    property Parameters: TdxPatternLinePainterParameters read GetParameters;
    property PixelGraphics: TdxGraphics read GetPixelGraphics;
  public
    constructor Create(const APainter: IdxPatternLinePaintingSupport; AUnitConverter: TdxDocumentLayoutUnitConverter);
    function RotateBounds(const ABounds: TdxRectF): TdxRectF; virtual;
    function RotatePoint(const APointF: TdxPointF): TdxPointF; virtual;
    procedure DrawLine(APen: TdxGPPen; const ABounds: TdxRectF); virtual;
    procedure DrawSolidLine(const ABounds: TdxRectF; AColor: TdxAlphaColor);
    procedure DrawDoubleSolidLine(const ABounds: TdxRectF; AColor: TdxAlphaColor);

    procedure DrawPatternLine(const ABounds: TdxRectF; AColor: TdxAlphaColor; const APattern: TArray<Single>);
    procedure DrawWaveUnderline(const ABounds: TdxRectF; AColor: TdxAlphaColor; APenWidth: Single); overload;
    procedure DrawWaveUnderline(const ABounds: TdxRectF; APen: TdxGPPen; AStep: Single); overload;
    procedure DrawWaveUnderline(const ABounds: TdxRectF; APen: TdxGPPen); overload;
    function MakeFixedWidthPattern(const APattern: TArray<Single>; AThickness: Single): TArray<Single>;
    function MakeBoundsAtLeast2PixelsHigh(const ABounds: TdxRectF): TdxRectF; virtual;

    property Painter: IdxPatternLinePaintingSupport read FPainter;
    property UnitConverter: TdxDocumentLayoutUnitConverter read FUnitConverter;
  end;

  TdxPatternLinePainterParametersTable = TObjectDictionary<TClass, TdxPatternLinePainterParameters>;

  { TdxRichEditPatternLinePainter }

  TdxRichEditPatternLinePainter = class(TdxPatternLinePainter,
    IdxCharacterLinePainter,
    IdxUnderlinePainter,
    IdxStrikeoutPainter)
  strict private
    class var FPixelGraphics: TdxGraphics;
    class function CreatePixelGraphics: TdxGraphics;
  protected
    class procedure FinalizePixelGraphics;
  strict private
    FParameters: TdxPatternLinePainterParameters;
    function GetColumnBreakPattern: TArray<Single>;
    function GetPageBreakPattern: TArray<Single>;
    function GetSectionStartPattern: TArray<Single>;
  protected
    function CreateParameters: TdxPatternLinePainterParameters; virtual;
    function GetParameters: TdxPatternLinePainterParameters; override;
    procedure PrepareParameters;
    procedure InitializeParameters(AParameters: TdxPatternLinePainterParameters); virtual; abstract;
    function GetPixelGraphics: TdxGraphics; override;
    function GetParametersTable: TdxPatternLinePainterParametersTable; virtual; abstract;
  public
    constructor Create(const APainter: IdxPatternLinePaintingSupport; AUnitConverter: TdxDocumentLayoutUnitConverter);
    //IUnderlinePainter
    procedure DrawUnderline(AUnderline: TdxUnderlineSingle; const ABounds: TdxRectF; AColor: TdxAlphaColor); overload;
    procedure DrawUnderline(AUnderline: TdxUnderlineDotted; const ABounds: TdxRectF; AColor: TdxAlphaColor); overload;
    procedure DrawUnderline(AUnderline: TdxUnderlineDashed; const ABounds: TdxRectF; AColor: TdxAlphaColor); overload;
    procedure DrawUnderline(AUnderline: TdxUnderlineDashSmallGap; const ABounds: TdxRectF; AColor: TdxAlphaColor); overload;
    procedure DrawUnderline(AUnderline: TdxUnderlineDashDotted; const ABounds: TdxRectF; AColor: TdxAlphaColor); overload;
    procedure DrawUnderline(AUnderline: TdxUnderlineDashDotDotted; const ABounds: TdxRectF; AColor: TdxAlphaColor); overload;
    procedure DrawUnderline(AUnderline: TdxUnderlineDouble; const ABounds: TdxRectF; AColor: TdxAlphaColor); overload;
    procedure DrawUnderline(AUnderline: TdxUnderlineHeavyWave; const ABounds: TdxRectF; AColor: TdxAlphaColor); overload;
    procedure DrawUnderline(AUnderline: TdxUnderlineLongDashed; const ABounds: TdxRectF; AColor: TdxAlphaColor); overload;
    procedure DrawUnderline(AUnderline: TdxUnderlineThickSingle; const ABounds: TdxRectF; AColor: TdxAlphaColor); overload;
    procedure DrawUnderline(AUnderline: TdxUnderlineThickDotted; const ABounds: TdxRectF; AColor: TdxAlphaColor); overload;
    procedure DrawUnderline(AUnderline: TdxUnderlineThickDashed; const ABounds: TdxRectF; AColor: TdxAlphaColor); overload;
    procedure DrawUnderline(AUnderline: TdxUnderlineThickDashDotted; const ABounds: TdxRectF; AColor: TdxAlphaColor); overload;
    procedure DrawUnderline(AUnderline: TdxUnderlineThickDashDotDotted; const ABounds: TdxRectF; AColor: TdxAlphaColor); overload;
    procedure DrawUnderline(AUnderline: TdxUnderlineThickLongDashed; const ABounds: TdxRectF; AColor: TdxAlphaColor); overload;
    procedure DrawUnderline(AUnderline: TdxUnderlineDoubleWave; const ABounds: TdxRectF; AColor: TdxAlphaColor); overload;
    procedure DrawUnderline(AUnderline: TdxUnderlineWave; const ABounds: TdxRectF; AColor: TdxAlphaColor); overload;
    //IStrikeoutPainter
    procedure DrawStrikeout(AStrikeout: TdxStrikeoutSingle; const ABounds: TdxRectF; AColor: TdxAlphaColor); overload;
    procedure DrawStrikeout(AStrikeout: TdxStrikeoutDouble; const ABounds: TdxRectF; AColor: TdxAlphaColor); overload;

    property ColumnBreakPattern: TArray<Single> read GetColumnBreakPattern;
    property PageBreakPattern: TArray<Single> read GetPageBreakPattern;
    property SectionStartPattern: TArray<Single> read GetSectionStartPattern;
    property ParametersTable: TdxPatternLinePainterParametersTable read GetParametersTable;
  end;

  { TdxRichEditHorizontalPatternLinePainter }

  TdxRichEditHorizontalPatternLinePainter = class(TdxRichEditPatternLinePainter)
  strict private
    class var
      FParametersTable: TdxPatternLinePainterParametersTable;
    class constructor Initialize;
    class destructor Finalize;
  protected
    function GetParametersTable: TdxPatternLinePainterParametersTable; override;
    procedure InitializeParameters(AParameters: TdxPatternLinePainterParameters); override;
  end;

  { TdxRichEditVerticalPatternLinePainter }

  TdxRichEditVerticalPatternLinePainter = class(TdxRichEditPatternLinePainter)
  strict private
    class var
      FParametersTable: TdxPatternLinePainterParametersTable;
    class constructor Initialize;
    class destructor Finalize;
  protected
    function GetParametersTable: TdxPatternLinePainterParametersTable; override;
    procedure InitializeParameters(AParameters: TdxPatternLinePainterParameters); override;
  public
    function RotateBounds(const ABounds: TdxRectF): TdxRectF; override;
    function RotatePoint(const APointF: TdxPointF): TdxPointF; override;
    procedure DrawLine(APen: TdxGPPen; const ABounds: TdxRectF); override;
  end;

implementation

uses
  Math, dxTypeHelpers;

{ TdxPatternLinePainterParameters }

function TdxPatternLinePainterParameters.CreatePattern(const APattern: array of Single; ADpiX: Single): TArray<Single>;
var
  I: Integer;
begin
  SetLength(Result, Length(APattern));
  for I := 0 to Length(APattern) - 1 do
    Result[I] := PixelsToUnits(APattern[I], ADpiX) / 5;
end;

destructor TdxPatternLinePainterParameters.Destroy;
begin
  SetLength(FDotPattern, 0);
  SetLength(FDashPattern, 0);
  SetLength(FDashSmallGapPattern, 0);
  SetLength(FDashDotPattern, 0);
  SetLength(FDashDotDotPattern, 0);
  SetLength(FLongDashPattern, 0);
  inherited Destroy;
end;

procedure TdxPatternLinePainterParameters.Initialize(ADpiX: Single);
const
  ADotPattern: array [0..1] of Single = (10, 10);
  ADashPattern: array [0..1] of Single = (40, 20);
  ADashSmallGapPattern: array [0..1] of Single = (40, 10);
  ADashDotPattern: array [0..3] of Single = (40, 13, 13, 13);
  ADashDotDotPattern: array [0..5] of Single = (30, 10, 10, 10, 10, 10);
  ALongDashPattern: array [0..1] of Single = (80, 40);
begin
  FDotPattern := CreatePattern(ADotPattern, ADpiX);
  FDashPattern := CreatePattern(ADashPattern, ADpiX);
  FDashSmallGapPattern := CreatePattern(ADashSmallGapPattern, ADpiX);
  FDashDotPattern := CreatePattern(ADashDotPattern, ADpiX);
  FDashDotDotPattern := CreatePattern(ADashDotDotPattern, ADpiX);
  FLongDashPattern := CreatePattern(ALongDashPattern, ADpiX);
  FPixelPenWidth := PixelsToUnits(1, ADpiX);
  FPixelStep := PixelsToUnits(2, ADpiX);
end;

{ TdxRichEditPatternLinePainterParameters }

constructor TdxRichEditPatternLinePainterParameters.Create(AUnitConverter: TdxDocumentLayoutUnitConverter);
begin
  inherited Create;
  Assert(AUnitConverter <> nil, 'AUnitConverter');
  FUnitConverter := AUnitConverter;
end;

destructor TdxRichEditPatternLinePainterParameters.Destroy;
begin
  SetLength(FSectionStartPattern, 0);
  SetLength(FPageBreakPattern, 0);
  SetLength(FColumnBreakPattern, 0);
  inherited Destroy;
end;

procedure TdxRichEditPatternLinePainterParameters.Initialize(ADpiX: Single);
const
  ASectionStartPattern: array [0..1] of Single = (10, 10);
  APageBreakPattern: array [0..1] of Single = (10, 10);
  AColumnBreakPattern: array [0..1] of Single = (10, 20);
begin
  FSectionStartPattern := CreatePattern(ASectionStartPattern, ADpiX);
  FPageBreakPattern := CreatePattern(APageBreakPattern, ADpiX);
  FColumnBreakPattern := CreatePattern(AColumnBreakPattern, ADpiX);
  inherited Initialize(ADpiX);
end;

function TdxRichEditPatternLinePainterParameters.PixelsToUnits(AValue, ADpi: Single): Single;
begin
  Result := FUnitConverter.PixelsToLayoutUnitsF(AValue, ADpi);
end;

{ TdxRichEditPatternLinePainter }

constructor TdxRichEditPatternLinePainter.Create(const APainter: IdxPatternLinePaintingSupport;
  AUnitConverter: TdxDocumentLayoutUnitConverter);
begin
  inherited Create(APainter, AUnitConverter);
  PrepareParameters;
end;

class procedure TdxRichEditPatternLinePainter.FinalizePixelGraphics;
begin
  FreeAndNil(FPixelGraphics);
end;

function TdxRichEditPatternLinePainter.CreateParameters: TdxPatternLinePainterParameters;
begin
  Result := TdxRichEditPatternLinePainterParameters.Create(UnitConverter);
end;

class function TdxRichEditPatternLinePainter.CreatePixelGraphics: TdxGraphics;
begin
  Result := TdxGraphics.Create;
end;

procedure TdxRichEditPatternLinePainter.DrawStrikeout(AStrikeout: TdxStrikeoutSingle;
  const ABounds: TdxRectF; AColor: TdxAlphaColor);
begin
  DrawSolidLine(ABounds, AColor);
end;

procedure TdxRichEditPatternLinePainter.DrawStrikeout(AStrikeout: TdxStrikeoutDouble;
  const ABounds: TdxRectF; AColor: TdxAlphaColor);
begin
  DrawDoubleSolidLine(ABounds, AColor);
end;

procedure TdxRichEditPatternLinePainter.DrawUnderline(AUnderline: TdxUnderlineDotted;
  const ABounds: TdxRectF; AColor: TdxAlphaColor);
begin
  DrawPatternLine(ABounds, AColor, DotPattern);
end;

procedure TdxRichEditPatternLinePainter.DrawUnderline(AUnderline: TdxUnderlineSingle;
  const ABounds: TdxRectF; AColor: TdxAlphaColor);
begin
  DrawSolidLine(ABounds, AColor);
end;

procedure TdxRichEditPatternLinePainter.DrawUnderline(AUnderline: TdxUnderlineDouble;
  const ABounds: TdxRectF; AColor: TdxAlphaColor);
begin
  DrawDoubleSolidLine(ABounds, AColor);
end;

procedure TdxRichEditPatternLinePainter.DrawUnderline(AUnderline: TdxUnderlineThickDashDotted;
  const ABounds: TdxRectF; AColor: TdxAlphaColor);
begin
  DrawPatternLine(ABounds, AColor, DashDotPattern);
end;

procedure TdxRichEditPatternLinePainter.DrawUnderline(AUnderline: TdxUnderlineThickDashed;
  const ABounds: TdxRectF; AColor: TdxAlphaColor);
begin
  DrawPatternLine(ABounds, AColor, DashPattern);
end;

procedure TdxRichEditPatternLinePainter.DrawUnderline(AUnderline: TdxUnderlineThickDotted;
  const ABounds: TdxRectF; AColor: TdxAlphaColor);
begin
  DrawPatternLine(ABounds, AColor, DotPattern);
end;

procedure TdxRichEditPatternLinePainter.DrawUnderline(AUnderline: TdxUnderlineThickSingle;
  const ABounds: TdxRectF; AColor: TdxAlphaColor);
begin
  DrawSolidLine(ABounds, AColor);
end;

procedure TdxRichEditPatternLinePainter.DrawUnderline(AUnderline: TdxUnderlineLongDashed;
  const ABounds: TdxRectF; AColor: TdxAlphaColor);
begin
  DrawPatternLine(ABounds, AColor, LongDashPattern);
end;

procedure TdxRichEditPatternLinePainter.DrawUnderline(AUnderline: TdxUnderlineHeavyWave;
  const ABounds: TdxRectF; AColor: TdxAlphaColor);
var
  ADrawBounds: TdxRectF;
begin
  ADrawBounds := MakeBoundsAtLeast2PixelsHigh(ABounds);
  DrawWaveUnderline(ADrawBounds, AColor, 2 * PixelPenWidth);
end;

procedure TdxRichEditPatternLinePainter.DrawUnderline(AUnderline: TdxUnderlineDashDotDotted;
  const ABounds: TdxRectF; AColor: TdxAlphaColor);
begin
  DrawPatternLine(ABounds, AColor, DashDotDotPattern);
end;

procedure TdxRichEditPatternLinePainter.DrawUnderline(AUnderline: TdxUnderlineDashDotted;
  const ABounds: TdxRectF; AColor: TdxAlphaColor);
begin
  DrawPatternLine(ABounds, AColor, DashDotPattern);
end;

procedure TdxRichEditPatternLinePainter.DrawUnderline(AUnderline: TdxUnderlineDashSmallGap;
  const ABounds: TdxRectF; AColor: TdxAlphaColor);
begin
  DrawPatternLine(ABounds, AColor, DashSmallGapPattern);
end;

procedure TdxRichEditPatternLinePainter.DrawUnderline(AUnderline: TdxUnderlineDashed;
  const ABounds: TdxRectF; AColor: TdxAlphaColor);
begin
  DrawPatternLine(ABounds, AColor, DashPattern);
end;

function TdxRichEditPatternLinePainter.GetParameters: TdxPatternLinePainterParameters;
begin
  Result := FParameters;
end;

function TdxRichEditPatternLinePainter.GetColumnBreakPattern: TArray<Single>;
begin
  if FParameters is TdxRichEditPatternLinePainterParameters then
    Result := TdxRichEditPatternLinePainterParameters(FParameters).ColumnBreakPattern
  else
    Result := FParameters.DotPattern;
end;

function TdxRichEditPatternLinePainter.GetPageBreakPattern: TArray<Single>;
begin
  if FParameters is TdxRichEditPatternLinePainterParameters then
    Result := TdxRichEditPatternLinePainterParameters(FParameters).PageBreakPattern
  else
    Result := FParameters.DotPattern;
end;

function TdxRichEditPatternLinePainter.GetSectionStartPattern: TArray<Single>;
begin
  if FParameters is TdxRichEditPatternLinePainterParameters then
    Result := TdxRichEditPatternLinePainterParameters(FParameters).SectionStartPattern
  else
    Result := FParameters.DotPattern;
end;

function TdxRichEditPatternLinePainter.GetPixelGraphics: TdxGraphics;
begin
  if FPixelGraphics = nil then
    FPixelGraphics := CreatePixelGraphics;
  Result := FPixelGraphics;
end;

procedure TdxRichEditPatternLinePainter.PrepareParameters;
var
  ARichEditParameters: TdxPatternLinePainterParameters;
begin
  System.TMonitor.Enter(ParametersTable);
  try
    if ParametersTable.TryGetValue(UnitConverter.ClassType, FParameters) then
      Exit;
    ARichEditParameters := CreateParameters;
    FParameters := ARichEditParameters;
    InitializeParameters(ARichEditParameters);
    ParametersTable.Add(UnitConverter.ClassType, FParameters);
  finally
    System.TMonitor.Exit(ParametersTable);
  end;
end;

procedure TdxRichEditPatternLinePainter.DrawUnderline(AUnderline: TdxUnderlineThickDashDotDotted;
  const ABounds: TdxRectF; AColor: TdxAlphaColor);
begin
  DrawPatternLine(ABounds, AColor, DashDotDotPattern);
end;

procedure TdxRichEditPatternLinePainter.DrawUnderline(AUnderline: TdxUnderlineThickLongDashed;
  const ABounds: TdxRectF; AColor: TdxAlphaColor);
begin
  DrawPatternLine(ABounds, AColor, LongDashPattern);
end;

procedure TdxRichEditPatternLinePainter.DrawUnderline(AUnderline: TdxUnderlineDoubleWave;
  const ABounds: TdxRectF; AColor: TdxAlphaColor);
var
  AThickness: Single;
  ADrawBounds, ATopBounds, ABottomBounds: TdxRectF;
begin
  ADrawBounds := RotateBounds(ABounds);

  AThickness := RoundToPixels(ADrawBounds.Height / 2, Screen.PixelsPerInch);
  if AThickness <= PixelsToUnits(1, Screen.PixelsPerInch) then
    AThickness := PixelsToUnits(1, Screen.PixelsPerInch);

  ATopBounds.InitSize(ADrawBounds.Left, ADrawBounds.Top, ADrawBounds.Width, AThickness);
  ATopBounds := MakeBoundsAtLeast2PixelsHigh(ATopBounds);
  ABottomBounds.InitSize(ADrawBounds.Left, ATopBounds.Bottom, ADrawBounds.Width, AThickness);
  ABottomBounds := MakeBoundsAtLeast2PixelsHigh(ABottomBounds);

  DrawWaveUnderline(RotateBounds(ATopBounds), AColor, 0);
  DrawWaveUnderline(RotateBounds(ABottomBounds), AColor, 0);
end;

procedure TdxRichEditPatternLinePainter.DrawUnderline(AUnderline: TdxUnderlineWave;
  const ABounds: TdxRectF; AColor: TdxAlphaColor);
var
  ADrawBounds: TdxRectF;
begin
  ADrawBounds := MakeBoundsAtLeast2PixelsHigh(RotateBounds(ABounds));
  DrawWaveUnderline(RotateBounds(ADrawBounds), AColor, 0);
end;

{ TdxPatternLinePainter }

constructor TdxPatternLinePainter.Create(const APainter: IdxPatternLinePaintingSupport;
  AUnitConverter: TdxDocumentLayoutUnitConverter);
begin
  inherited Create;
  Assert(APainter <> nil);
  Assert(AUnitConverter <> nil);
  FPainter := APainter;
  FUnitConverter := AUnitConverter;
end;

procedure TdxPatternLinePainter.DrawDoubleSolidLine(const ABounds: TdxRectF; AColor: TdxAlphaColor);
var
  AThickness, AHalfThickness, ADistance: Single;
  ADrawBounds, ATopBounds, ABottomBounds: TdxRectF;
begin
  ADrawBounds := RotateBounds(ABounds);
  AThickness := RoundToPixels(ADrawBounds.Height / 4, UnitConverter.Dpi);
  if AThickness <= PixelsToUnits(1, UnitConverter.Dpi) then
    AThickness := PixelsToUnits(1, UnitConverter.Dpi);

  AHalfThickness := AThickness / 2;

  ATopBounds.InitSize(ADrawBounds.Left, ADrawBounds.Top + AHalfThickness, ADrawBounds.Width, AThickness);
  ABottomBounds.InitSize(ADrawBounds.Left, ADrawBounds.Bottom - AHalfThickness, ADrawBounds.Width, AThickness);

  ADistance := RoundToPixels(ABottomBounds.Top - ATopBounds.Bottom, UnitConverter.Dpi);
  if ADistance <= PixelsToUnits(1, UnitConverter.Dpi) then
    ADistance := PixelsToUnits(1, UnitConverter.Dpi);
  ABottomBounds := cxRectSetTop(ABottomBounds, ATopBounds.Bottom + Trunc(ADistance));

  DrawSolidLine(RotateBounds(ATopBounds), AColor);
  DrawSolidLine(RotateBounds(ABottomBounds), AColor);
end;

procedure TdxPatternLinePainter.DrawPatternLine(const ABounds: TdxRectF; AColor: TdxAlphaColor; const APattern: TArray<Single>);
var
  APen: TdxGPColorPen;
  AFixedPattern: TArray<Single>;
  ADrawBounds: TdxRectF;
begin
  ADrawBounds := RotateBounds(ABounds);
  AFixedPattern := MakeFixedWidthPattern(APattern, ADrawBounds.Height);
  try
    APen := TdxGPColorPen.Create(AColor, ADrawBounds.Height);
    try
      APen.DashPattern := AFixedPattern;
      DrawLine(APen, RotateBounds(ADrawBounds));
    finally
      APen.Free;
    end;
  finally
    SetLength(AFixedPattern, 0);
  end;
end;

function TdxPatternLinePainter.RotateBounds(const ABounds: TdxRectF): TdxRectF;
begin
  Result := ABounds;
end;

function TdxPatternLinePainter.RotatePoint(const APointF: TdxPointF): TdxPointF;
begin
  Result := APointF;
end;

procedure TdxPatternLinePainter.DrawLine(APen: TdxGPPen; const ABounds: TdxRectF);
begin
  Painter.DrawLine(APen, ABounds.Left, ABounds.Top, ABounds.Right, ABounds.Top);
end;

procedure TdxPatternLinePainter.DrawSolidLine(const ABounds: TdxRectF; AColor: TdxAlphaColor);
var
  APen: TdxGPPen;
begin
  APen := FPainter.GetPen(AColor, Min(cxRectWidth(ABounds), cxRectHeight(ABounds)));
  try
    DrawLine(APen, ABounds);
  finally
    FPainter.ReleasePen(APen);
  end;
end;

procedure TdxPatternLinePainter.DrawWaveUnderline(const ABounds: TdxRectF; APen: TdxGPPen; AStep: Single);
var
  X, Y, ASum: Single;
  APoints: TArray<TdxPointF>;
  I, ACount: Integer;
  ADrawBounds: TdxRectF;
begin
  ADrawBounds := RotateBounds(ABounds);
  ACount := Math.Ceil(ADrawBounds.Width / AStep);
  if ACount <= 1 then
    Exit;

  X := ADrawBounds.Left;
  Y := ADrawBounds.Top;
  ASum := Y + ADrawBounds.Bottom;
  begin
    SetLength(APoints, ACount);
    try
      for I := 0 to ACount - 1 do
      begin
        APoints[I] := RotatePoint(TdxPointF.Create(X, Y));
        Y := ASum - Y;
        X := X + AStep;
      end;
      Painter.DrawLines(APen, APoints);
    finally
      SetLength(APoints, 0);
    end;
  end;
end;

procedure TdxPatternLinePainter.DrawWaveUnderline(const ABounds: TdxRectF; APen: TdxGPPen);
var
  AStep: Single;
  ADrawBounds: TdxRectF;
begin
  ADrawBounds := RotateBounds(ABounds);
  AStep := RoundToPixels(ADrawBounds.Height, Screen.PixelsPerInch);
  AStep := Math.Max(PixelStep, AStep);

  DrawWaveUnderline(RotateBounds(ADrawBounds), APen, AStep);
end;

procedure TdxPatternLinePainter.DrawWaveUnderline(const ABounds: TdxRectF; AColor: TdxAlphaColor; APenWidth: Single);
var
  APen: TdxGPPen;
begin
  APen := FPainter.GetPen(AColor, APenWidth);
  try
    DrawWaveUnderline(ABounds, APen);
  finally
    FPainter.ReleasePen(APen);
  end;
end;

function TdxPatternLinePainter.GetDashDotDotPattern: TArray<Single>;
begin
  Result := Parameters.DashDotDotPattern;
end;

function TdxPatternLinePainter.GetDashDotPattern: TArray<Single>;
begin
  Result := Parameters.DashDotPattern;
end;

function TdxPatternLinePainter.GetDashPattern: TArray<Single>;
begin
  Result := Parameters.DashPattern;
end;

function TdxPatternLinePainter.GetDashSmallGapPattern: TArray<Single>;
begin
  Result := Parameters.DashSmallGapPattern;
end;

function TdxPatternLinePainter.GetDotPattern: TArray<Single>;
begin
  Result := Parameters.DotPattern;
end;

function TdxPatternLinePainter.GetLongDashPattern: TArray<Single>;
begin
  Result := Parameters.LongDashPattern;
end;

function TdxPatternLinePainter.GetPixelPenWidth: Single;
begin
  Result := Parameters.PixelPenWidth;
end;

function TdxPatternLinePainter.GetPixelStep: Single;
begin
  Result := Parameters.PixelStep;
end;

function TdxPatternLinePainter.MakeBoundsAtLeast2PixelsHigh(const ABounds: TdxRectF): TdxRectF;
var
  AHeight: Single;
begin
  Result := ABounds;
  AHeight := RoundToPixels(ABounds.Height, Screen.PixelsPerInch);
  if AHeight <= PixelsToUnits(2, Screen.PixelsPerInch) then
  begin
    AHeight := PixelsToUnits(2, Screen.PixelsPerInch);
    Result.InitSize(ABounds.Left, ABounds.Top, ABounds.Width, AHeight);
  end;
end;

function TdxPatternLinePainter.MakeFixedWidthPattern(const APattern: TArray<Single>; AThickness: Single): TArray<Single>;
var
  ACount: Integer;
  I: Integer;
begin
  ACount := Length(APattern);
  SetLength(Result, ACount);
  for I := 0 to ACount - 1 do
    Result[I] := APattern[I] / AThickness;
end;

function TdxPatternLinePainter.PixelsToUnits(AVal, ADpi: Single): Single;
begin
  Result := UnitConverter.PixelsToLayoutUnitsF(AVal, ADpi);
end;

function TdxPatternLinePainter.RoundToPixels(AVal, ADpi: Single): Single;
var
  APixelVal: Double;
begin
  APixelVal := UnitsToPixels(AVal, ADpi);
  APixelVal := Round(APixelVal);
  Result := PixelsToUnits(APixelVal, ADpi);
end;

function TdxPatternLinePainter.UnitsToPixels(AVal, ADpi: Single): Single;
begin
  Result := UnitConverter.LayoutUnitsToPixelsF(AVal, ADpi);
end;

{ TdxRichEditHorizontalPatternLinePainter }

class constructor TdxRichEditHorizontalPatternLinePainter.Initialize;
begin
  FParametersTable := TdxPatternLinePainterParametersTable.Create([doOwnsValues]);
end;

class destructor TdxRichEditHorizontalPatternLinePainter.Finalize;
begin
  FreeAndNil(FParametersTable);
end;

function TdxRichEditHorizontalPatternLinePainter.GetParametersTable: TdxPatternLinePainterParametersTable;
begin
  Result := FParametersTable;
end;

procedure TdxRichEditHorizontalPatternLinePainter.InitializeParameters(AParameters: TdxPatternLinePainterParameters);
begin
  AParameters.Initialize(Screen.PixelsPerInch);
end;

{ TdxRichEditVerticalPatternLinePainter }

class constructor TdxRichEditVerticalPatternLinePainter.Initialize;
begin
  FParametersTable := TdxPatternLinePainterParametersTable.Create([doOwnsValues]);
end;

class destructor TdxRichEditVerticalPatternLinePainter.Finalize;
begin
  FreeAndNil(FParametersTable);
end;

procedure TdxRichEditVerticalPatternLinePainter.DrawLine(APen: TdxGPPen; const ABounds: TdxRectF);
begin
  Painter.DrawLine(APen, ABounds.Left, ABounds.Top, ABounds.Left, ABounds.Bottom);
end;

function TdxRichEditVerticalPatternLinePainter.GetParametersTable: TdxPatternLinePainterParametersTable;
begin
  Result := FParametersTable;
end;

procedure TdxRichEditVerticalPatternLinePainter.InitializeParameters(AParameters: TdxPatternLinePainterParameters);
begin
  AParameters.Initialize(Screen.PixelsPerInch);
end;

function TdxRichEditVerticalPatternLinePainter.RotateBounds(const ABounds: TdxRectF): TdxRectF;
begin
  Result := cxRectFBounds(ABounds.Top, ABounds.Left, ABounds.Height, ABounds.Width);
end;

function TdxRichEditVerticalPatternLinePainter.RotatePoint(const APointF: TdxPointF): TdxPointF;
begin
  Result.x := APointF.y;
  Result.y := APointF.x;
end;

procedure Finalize;
begin
  TdxRichEditPatternLinePainter.FinalizePixelGraphics;
end;

initialization
  dxUnitsLoader.AddUnit(nil, @Finalize);

finalization
  dxUnitsLoader.RemoveUnit(@Finalize);

end.
