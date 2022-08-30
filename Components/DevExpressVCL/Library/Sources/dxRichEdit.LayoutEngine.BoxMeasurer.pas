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

unit dxRichEdit.LayoutEngine.BoxMeasurer;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Windows, Classes, Types, SysUtils, Graphics,
  dxCore, cxGeometry, dxCoreClasses, dxGDIPlusAPI, dxGDIPlusClasses,

  dxRichEdit.Utils.Types,
  dxRichEdit.Utils.Graphics,
  dxRichEdit.Platform.Font,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Boxes.Core,
  dxRichEdit.DocumentModel.Boxes.Simple,
  dxRichEdit.DocumentModel.Simple;


type
  PdxIntegerArray = ^TdxIntegerArray;
  TdxIntegerArray = array [0..0] of Integer;

  PdxWordArray = ^TdxWordArray;
  TdxWordArray = array [0..0] of Word;

  IntPtr = PINT;

  { TdxGdiTextViewInfo }

  TdxGdiTextViewInfo = class(TdxTextViewInfo)
  private
    FGlyphCount: Integer;
    FGlyphs: PdxWordArray;
    FCharacterWidths: PdxIntegerArray;
  public
    destructor Destroy; override;

    property Glyphs: PdxWordArray read FGlyphs write FGlyphs;
    property GlyphCount: Integer read FGlyphCount write FGlyphCount;
    property CharacterWidths: PdxIntegerArray read FCharacterWidths write FCharacterWidths;
  end;

  { TdxGdiBoxMeasurer }

  TdxGdiBoxMeasurer = class(TdxBoxMeasurer)
  private
    FCaretPosBufferSize: Integer;
    FCaretPosBuffer: Pointer;
    FGraphics: TdxGraphics;
  protected
    procedure AdjustCharacterBoundsForLigature(const ACharacterBounds: TArray<TRect>; AFrom, ATo: Integer);
    procedure EstimateCaretPositionsForLigatures(const ACharacterBounds: TArray<TRect>);
    function CalculateCharactersBounds(ACaret: PINT; ALength: Integer; const ABounds: TRect): TArray<TRect>;
    function CreateTextViewInfoCore(AHdc: HDC; const AText: string; AFontInfo: TdxFontInfo): TdxGdiTextViewInfo; virtual;
    function MeasureCharactersWithGetCharacterPlacementSlow(AHdc: THandle; const AText: string; var AGcpResults: TGCPResultsW): Integer;
  public
    constructor Create(ADocumentModel: TdxCustomDocumentModel; AGraphics: TdxGraphics); reintroduce;
    destructor Destroy; override;

    function GetHdc: HDC; virtual;
    procedure ReleaseHdc(ADC: HDC); virtual;
    function MeasureCharactersBounds(const AText: string; AFontInfo: TdxFontInfo; const ABounds: TRect): TArray<TRect>; override;
    function MeasureWithGetCharacterPlacementSlow(AHdc: THandle; const AText: string; var AGcpResults: TGCPResultsW): Integer;
    function CreateTextViewInfo(ABoxInfo: TdxBoxInfo; const AText: string; AFontInfo: TdxFontInfo): TdxTextViewInfo; override;
    function GetCaretPosBuffer(AItemsCount: Integer): Pointer;
    function SnapToPixels(AValue: Integer; ADpi: Single): Integer;
    function TryAdjustEndPositionToFit(ABoxInfo: TdxBoxInfo; AMaxWidth: Integer): Boolean; overload; override;
    function TryAdjustEndPositionToFit(ABoxInfo: TdxBoxInfo; const AText: string; AFontInfo: TdxFontInfo; AMaxWidth: Integer): Boolean; overload; override;

    property Graphics: TdxGraphics read FGraphics;
  end;

  { TdxGdiBoxMeasurerLockHdc }

  TdxGdiBoxMeasurerLockHdc = class(TdxGdiBoxMeasurer)
  strict private
    FHdcGraphics: TdxGraphics;
    FHdc: THandle;
  public
    constructor Create(ADocumentModel: TdxCustomDocumentModel; AGraphics: TdxGraphics; AHdcGraphics: TdxGraphics); reintroduce;
    destructor Destroy; override;
    function GetHdc: HDC; override;
    procedure ReleaseHdc(ADC: HDC); override;
    procedure ObtainCachedHdc;
    procedure ReleaseCachedHdc;
  end;

  { TdxRectangleUtils }

  TdxRectangleUtils = class
  public
    class function SplitHorizontally(const ABounds: TRect; ACellCount: Integer): TArray<TRect>; static;
    class function BoundingRectangle(const APoints: array of TPoint): TRect; overload; static;
    class function BoundingRectangle(const ABounds: TRect;
      ATransform: TdxTransformMatrix): TRect; overload; static;
    class function CenterPoint(const ARectangle: TRect): TPoint; static;
  end;

implementation

uses
  dxRichEdit.Platform.Win.Font, Math, dxTypeHelpers;

{ TdxGdiTextViewInfo }

destructor TdxGdiTextViewInfo.Destroy;
begin
  FreeMem(FGlyphs);
  FreeMem(FCharacterWidths);
  inherited Destroy;
end;

{ TdxGdiBoxMeasurer }

constructor TdxGdiBoxMeasurer.Create(ADocumentModel: TdxCustomDocumentModel; AGraphics: TdxGraphics);
const
  dxInitialBufferItemCount = 64;
begin
  inherited Create(ADocumentModel);
  FGraphics := AGraphics;
  GetCaretPosBuffer(dxInitialBufferItemCount);
end;

destructor TdxGdiBoxMeasurer.Destroy;
begin
  FreeMem(FCaretPosBuffer);
  inherited Destroy;
end;

function TdxGdiBoxMeasurer.CreateTextViewInfo(ABoxInfo: TdxBoxInfo; const AText: string;
  AFontInfo: TdxFontInfo): TdxTextViewInfo;
var
  ADC: HDC;
  AGdiFontInfo: TdxGdiFontInfo absolute AFontInfo;
begin
  ADC := GetHdc;
  try
    SelectObject(ADC, AGdiFontInfo.GdiFontHandle);
    Result := CreateTextViewInfoCore(ADC, AText, AFontInfo);
  finally
    ReleaseHdc(ADC);
  end;
end;

function TdxGdiBoxMeasurer.CreateTextViewInfoCore(AHdc: HDC; const AText: string; AFontInfo: TdxFontInfo): TdxGdiTextViewInfo;
var
  AGcpResults: TGCPResultsW;
  ASize: Cardinal;
  ALength, AWidth, AHeight, ACaretBasedWidth: Integer;
  AGdiFontInfo: TdxGdiFontInfo absolute AFontInfo;
begin
  ALength := Length(AText);
  ZeroMemory(@AGcpResults, SizeOf(TGCPResults));
  AGcpResults.lStructSize := SizeOf(TGCPResults);

  AGcpResults.lpCaretPos := GetCaretPosBuffer(ALength);
  GetMem(AGcpResults.lpDx, SizeOf(Integer) * ALength);

  if not AGdiFontInfo.UseGetGlyphIndices then
  begin
    GetMem(AGcpResults.lpGlyphs, SizeOf(Word) * ALength);
    if ALength > 0 then
      PWord(AGcpResults.lpGlyphs)^ := 0;
    AGcpResults.nGlyphs := ALength;
  end;

  ASize := GetCharacterPlacement(AHdc, PChar(AText), ALength, 0, AGcpResults, GCP_USEKERNING or GCP_LIGATE);
  if (ASize = 0) and (ALength > 0) then
    ASize := MeasureWithGetCharacterPlacementSlow(AHdc, AText, AGcpResults);
  AWidth := LongRec(ASize).Lo;
  AHeight := LongRec(ASize).Hi;

  if Length(AText) > 0 then
  begin
    ACaretBasedWidth := PdxIntegerArray(AGcpResults.lpCaretPos)[ALength - 1];
    if ACaretBasedWidth > $FFFF then
      AWidth := ACaretBasedWidth + PdxIntegerArray(AGcpResults.lpDx)[ALength - 1];
  end;
  if AGdiFontInfo.UseGetGlyphIndices then
  begin
    GetMem(AGcpResults.lpGlyphs, SizeOf(Word) * ALength);
    GetGlyphIndices(AHdc, PChar(AText), ALength, Pointer(AGcpResults.lpGlyphs), 0);
  end;

  Result := TdxGdiTextViewInfo.Create;
  Result.Size := TSize.Create(AWidth, AHeight);
  Result.Glyphs := Pointer(AGcpResults.lpGlyphs);
  Result.GlyphCount := AGcpResults.nGlyphs;
  Result.CharacterWidths := Pointer(AGcpResults.lpDx);
end;

function TdxGdiBoxMeasurer.GetCaretPosBuffer(AItemsCount: Integer): Pointer;
var
  ASize: Integer;
begin
  ASize := SizeOf(Integer) * AItemsCount;
  if ASize > FCaretPosBufferSize then
  begin
    ReallocMem(FCaretPosBuffer, ASize);
    FCaretPosBufferSize := ASize;
  end;
  Result := FCaretPosBuffer;
end;

function TdxGdiBoxMeasurer.GetHdc: HDC;
begin
  Result := FGraphics.GetHDC;
end;

procedure TdxGdiBoxMeasurer.ReleaseHdc(ADC: HDC);
begin
  FGraphics.ReleaseHDC(ADC);
end;

function TdxGdiBoxMeasurer.MeasureCharactersBounds(const AText: string; AFontInfo: TdxFontInfo;
  const ABounds: TRect): TArray<TRect>;
var
  AGcpResults: TGCPResultsW;
  ASize: Cardinal;
  ACaretPos: Integer;
  ALength: Integer;
  ADC: HDC;
  AOldFont: THandle;
  AGdiFontInfo: TdxGdiFontInfo absolute AFontInfo;
begin
  TMonitor.Enter(FGraphics);
  try
    ADC := FGraphics.GetHDC;
    ALength := Length(AText);
    AOldFont := SelectObject(ADC, AGdiFontInfo.GdiFontHandle);
    try
      ACaretPos := SizeOf(Integer) * ALength;
      AGcpResults.lStructSize := SizeOf(GCP_RESULTS);
      AGcpResults.lpOutString := nil;
      AGcpResults.lpOrder := nil;
      AGcpResults.lpDx := nil;
      GetMem(AGcpResults.lpCaretPos, ACaretPos);
      try
        AGcpResults.lpClass := nil;
        AGcpResults.lpGlyphs := nil;
        AGcpResults.nGlyphs := ALength;
        AGcpResults.nMaxFit := 0;
        ASize := GetCharacterPlacement(ADC, PChar(AText), ALength, 0, AGcpResults, GCP_USEKERNING or GCP_LIGATE);
        if (ASize = 0) and (ALength > 0) then
          MeasureCharactersWithGetCharacterPlacementSlow(ADC, AText, AGcpResults);
        Result := CalculateCharactersBounds(AGcpResults.lpCaretPos, ALength, ABounds);
      finally
        FreeMem(AGcpResults.lpCaretPos);
      end;
    finally
      SelectObject(ADC, AOldFont);
      FGraphics.ReleaseHDC(ADC);
    end;
  finally
    TMonitor.Exit(FGraphics);
  end;
end;

function TdxGdiBoxMeasurer.MeasureWithGetCharacterPlacementSlow(AHdc: THandle; const AText: string; var AGcpResults: TGCPResultsW): Integer;
var
  I, AAdd, AStep, ALength: Integer;
begin
  ALength := Length(AText);
  AStep := Max(1, ALength div 2);
  AAdd := AStep;
  for I := 0 to 2 do
  begin
    ReallocMem(AGcpResults.lpDx, SizeOf(Integer) * (ALength + AAdd));
    ReallocMem(AGcpResults.lpGlyphs, SizeOf(Word) * (ALength + AAdd));
    AGcpResults.nGlyphs := ALength + AAdd;
    if ALength > 0 then
      PWord(AGcpResults.lpGlyphs)^ := 0;
    Result := GetCharacterPlacement(AHdc, PChar(AText), ALength, 0, AGcpResults, GCP_USEKERNING or GCP_LIGATE);
    if Result <> 0 then
      Exit;
  end;
end;

function TdxGdiBoxMeasurer.SnapToPixels(AValue: Integer; ADpi: Single): Integer;
begin
  Result := DocumentModel.LayoutUnitConverter.SnapToPixels(AValue, ADpi);
end;

function TdxGdiBoxMeasurer.CalculateCharactersBounds(ACaret: PINT; ALength: Integer; const ABounds: TRect): TArray<TRect>;
var
  I, APrevPos, ANextPos: Integer;
  ANeedProcessLigatures: Boolean;
begin
  ANeedProcessLigatures := False;
  SetLength(Result, ALength);
  APrevPos := ACaret^;
  for I := 0 to ALength - 2 do
  begin
    Inc(ACaret);
    ANextPos := ACaret^;
    Result[I].InitSize(ABounds.Left + APrevPos, ABounds.Top, ANextPos - APrevPos, ABounds.Height);
    ANeedProcessLigatures := (APrevPos = ANextPos) or ANeedProcessLigatures;
    APrevPos := ANextPos;
  end;
  if ALength > 0 then
    Result[ALength - 1].InitSize(ABounds.Left + APrevPos, ABounds.Top, ABounds.Width - APrevPos, ABounds.Height);
  if ANeedProcessLigatures then
    EstimateCaretPositionsForLigatures(Result);
end;

function TdxGdiBoxMeasurer.MeasureCharactersWithGetCharacterPlacementSlow(AHdc: THandle; const AText: string; var AGcpResults: TGCPResultsW): Integer;
var
  I, AAdd, AStep, ALength: Integer;
begin
  ALength := Length(AText);
  AStep := Max(1, ALength div 2);
  AAdd := AStep;
  for I := 0 to 2 do
  begin
    ReallocMem(AGcpResults.lpCaretPos, SizeOf(Integer) * (ALength + AAdd));
    AGcpResults.nGlyphs := ALength + AAdd;
    Result := GetCharacterPlacement(AHdc, PChar(AText), ALength, 0, AGcpResults, GCP_USEKERNING or GCP_LIGATE);
    if Result <> 0 then
      Exit;
  end;
end;

function TdxGdiBoxMeasurer.TryAdjustEndPositionToFit(ABoxInfo: TdxBoxInfo; AMaxWidth: Integer): Boolean;
var
  ARun: TdxTextRunBase;
begin
  ARun := TdxTextRunBase(PieceTable.Runs[ABoxInfo.StartPos.RunIndex]);
  Result := ARun.TryAdjustEndPositionToFit(ABoxInfo, AMaxWidth, Self);
end;

function TdxGdiBoxMeasurer.TryAdjustEndPositionToFit(ABoxInfo: TdxBoxInfo; const AText: string; AFontInfo: TdxFontInfo; AMaxWidth: Integer): Boolean;
var
  AHdc: THandle;
  AGdiFontInfo: TdxGdiFontInfo;
  AGcpResults: TGCPResults;
  ANewEndOffset: Integer;
begin
  AHdc := GetHdc;
  try
    AGdiFontInfo := TdxGdiFontInfo(AFontInfo);
    SelectObject(AHdc, AGdiFontInfo.GdiFontHandle);

    ZeroMemory(@AGcpResults, SizeOf(AGcpResults));
    AGcpResults.lStructSize := SizeOf(AGcpResults);
    AGcpResults.nGlyphs := Length(AText) * 4;

    GetCharacterPlacement(AHdc, PChar(AText), Length(AText), AMaxWidth, AGcpResults, GCP_USEKERNING or GCP_LIGATE or GCP_MAXEXTENT);
    if AGcpResults.nMaxFit > 0 then
    begin
      ANewEndOffset := ABoxInfo.StartPos.Offset + AGcpResults.nMaxFit - 1;
      if ANewEndOffset > ABoxInfo.EndPos.Offset then
        Exit(False);
      ABoxInfo.EndPos := TdxFormatterPosition.Create(ABoxInfo.EndPos.RunIndex, ANewEndOffset,
        ABoxInfo.EndPos.BoxIndex);
      Result := True;
    end
    else
      Result := False;
  finally
    ReleaseHdc(AHdc);
  end;
end;

procedure TdxGdiBoxMeasurer.EstimateCaretPositionsForLigatures(const ACharacterBounds: TArray<TRect>);
var
  ABounds: TRect;
  I, AFrom, ACount: Integer;
begin
  ACount := Length(ACharacterBounds);
  AFrom := MaxInt;
  for I := 0 to ACount - 1 do
  begin
    ABounds := ACharacterBounds[I];
    if (ABounds.Right - ABounds.Left) = 0 then
    begin
      if AFrom = MaxInt then
        AFrom := I;
    end
    else
      if AFrom < I then
      begin
        AdjustCharacterBoundsForLigature(ACharacterBounds, AFrom, I);
        AFrom := MaxInt;
      end;
  end;
end;

procedure TdxGdiBoxMeasurer.AdjustCharacterBoundsForLigature(const ACharacterBounds: TArray<TRect>;
  AFrom, ATo: Integer);
var
  I, ACount: Integer;
  ABounds: TArray<TRect>;
begin
  ACount := ATo - AFrom + 1;
  ABounds := TdxRectangleUtils.SplitHorizontally(ACharacterBounds[ATo], ACount);
  for I := 0 to ACount - 1 do
    ACharacterBounds[I + AFrom] := ABounds[I];
end;

{ TdxGdiBoxMeasurerLockHdc }

constructor TdxGdiBoxMeasurerLockHdc.Create(ADocumentModel: TdxCustomDocumentModel; AGraphics: TdxGraphics; AHdcGraphics: TdxGraphics);
begin
  inherited Create(ADocumentModel, AGraphics);
  Assert(AHdcGraphics <> nil);
  FHdcGraphics := AHdcGraphics;
  ObtainCachedHdc;
end;

destructor TdxGdiBoxMeasurerLockHdc.Destroy;
begin
  ReleaseCachedHdc;
  inherited Destroy;
end;

procedure TdxGdiBoxMeasurerLockHdc.ObtainCachedHdc;
begin
  FHdc := FHdcGraphics.GetHdc;
end;

procedure TdxGdiBoxMeasurerLockHdc.ReleaseCachedHdc;
begin
  if FHdc <> 0 then
  begin
    FHdcGraphics.ReleaseHdc(FHdc);
    FHdc := 0;
  end;
end;

function TdxGdiBoxMeasurerLockHdc.GetHdc: HDC;
begin
  if FHdc <> 0 then
    Result := FHdc
  else
    Result := inherited GetHdc;
end;

procedure TdxGdiBoxMeasurerLockHdc.ReleaseHdc(ADC: HDC);
begin
  if FHdc = 0 then
    inherited ReleaseHdc(ADc);
end;

{ TdxRectangleUtils }

class function TdxRectangleUtils.BoundingRectangle(const APoints: array of TPoint): TRect;
var
  I, ACount, AMaxX, AMaxY, AMinX, AMinY: Integer;
begin
  ACount := Length(APoints);
  if ACount = 0 then
    Exit(TRect.Null);
  AMinX := APoints[0].X;
  AMaxX := AMinX;
  AMinY := APoints[0].Y;
  AMaxY := AMinY;
  for I := 1 to ACount - 1 do
  begin
    AMinX := Min(APoints[I].X, AMinX);
    AMinY := Min(APoints[I].Y, AMinY);
    AMaxX := Max(APoints[I].X, AMaxX);
    AMaxY := Max(APoints[I].Y, AMaxY);
  end;
  Result.Init(AMinX, AMinY, AMaxX, AMaxY);
end;

class function TdxRectangleUtils.BoundingRectangle(const ABounds: TRect; ATransform: TdxTransformMatrix): TRect;
var
  APt0, APt1, APt2, APt3: TPoint;
begin
  APt0 := ATransform.TransformPoint(Point(ABounds.Right, ABounds.Bottom));
  APt1 := ATransform.TransformPoint(Point(ABounds.Right, ABounds.Top));
  APt2 := ATransform.TransformPoint(Point(ABounds.Left, ABounds.Bottom));
  APt3 := ATransform.TransformPoint(ABounds.Location);

  Result := TdxRectangleUtils.BoundingRectangle([APt0, APt1, APt2, APt3]);
end;

class function TdxRectangleUtils.CenterPoint(const ARectangle: TRect): TPoint;
begin
  Result := Point(Trunc((ARectangle.Right + ARectangle.Left) / 2), Trunc((ARectangle.Bottom + ARectangle.Top) / 2));
end;

class function TdxRectangleUtils.SplitHorizontally(const ABounds: TRect; ACellCount: Integer): TArray<TRect>;
var
  I, AWidth, ARemainder, AColumnWidth, AColumnsAreaWidth, AHeight, AOffset: Integer;
begin
  if ACellCount <= 0 then
  begin
    SetLength(Result, 1);
    Result[0] := ABounds;
    Exit;
  end;
  SetLength(Result, ACellCount);
  AOffset := ABounds.Left;
  AHeight := ABounds.Height;
  AColumnsAreaWidth := ABounds.Width;
  AColumnWidth := AColumnsAreaWidth div ACellCount;
  ARemainder := AColumnsAreaWidth - (AColumnWidth * ACellCount);
  for I := 0 to ACellCount - 1 do
  begin
    AWidth := AColumnWidth;
    if ARemainder > 0 then
      Inc(AWidth);
    Result[I].InitSize(AOffset, ABounds.Top, AWidth, AHeight);
    Inc(AOffset, AWidth);
    Dec(ARemainder)
  end;
end;

end.
