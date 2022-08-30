{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressPDFViewer                                         }
{                                                                    }
{           Copyright (c) 2015-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSPDFVIEWER AND ALL              }
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

unit dxPDFSelection;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  SysUtils, Windows, Classes, Messages, Forms, Controls, StdCtrls, Graphics, Generics.Defaults, Generics.Collections,
  dxCore, dxCoreClasses, cxClasses, cxGeometry, cxGraphics, dxCoreGraphics, dxPDFCore, dxPDFTypes, dxPDFText, dxPDFDocument,
  dxPDFRecognizedObject;

const
  // HitTests bits
  hcTextHighlight = 8192;
  hcTextSelection = 16384;

type
  TdxPDFMovementDirection = (mdLeft, mdDown, mdRight, mdUp, mdNextWord, mdPreviousWord, mdLineStart,
    mdLineEnd, mdDocumentStart, mdDocumentEnd);

  { TdxPDFCustomSelection }

  TdxPDFCustomSelection = class
  strict private
    FBackColor: TdxAlphaColor;
    FFrameColor: TdxAlphaColor;
  protected
    function Contains(const APosition: TdxPDFPosition): Boolean; virtual;
    function GetHitCode: Integer; virtual; abstract;
    function Same(AValue: TdxPDFCustomSelection): Boolean; virtual;
  public
    constructor Create(ABackColor, AFrameColor: TdxAlphaColor); virtual;

    property BackColor: TdxAlphaColor read FBackColor;
    property FrameColor: TdxAlphaColor read FFrameColor;

    property HitCode: Integer read GetHitCode;
  end;

  { TdxPDFImageSelection }

  TdxPDFImageSelection = class(TdxPDFCustomSelection)
  strict private
    FClipRect: TdxRectF;
    FImage: TdxPDFImage;
    FPageIndex: Integer;

    function GetBitmap: TBitmap;
    function GetBounds: TdxRectF;
    function GetClipBounds: TdxRectF;
    procedure SetImage(const AValue: TdxPDFImage);

    function ConvertToRotationAngle(AAngle: Integer): TcxRotationAngle;
    procedure CalculateRendererParameters(APage: TdxPDFPage; AScale: Single; out APageSize: TPoint; out AClipRect: TdxRectF);
  protected
    function Contains(const APosition: TdxPDFPosition): Boolean; override;
    function GetHitCode: Integer; override;
    function Same(AValue: TdxPDFCustomSelection): Boolean; override;

    function GetImageBitmap(AViewer: TObject): TcxBitmap;

    property Image: TdxPDFImage read FImage write SetImage;
    property PageIndex: Integer read FPageIndex;
  public
    constructor Create(ABackColor, AFrameColor: TdxAlphaColor; APageIndex: Integer; AImage: TdxPDFImage;
      const AClipRect: TdxRectF); reintroduce;
    destructor Destroy; override;

    property Bitmap: TBitmap read GetBitmap;
    property Bounds: TdxRectF read GetBounds;
  end;

  { TdxPDFCustomHighlight }

  TdxPDFTextHighlight = class(TdxPDFCustomSelection)
  strict private
    FRects: TObjectDictionary<Integer, TList<TdxRectF>>;
    FPages: TdxPDFPages;
    FRanges: TdxPDFPageTextRanges;

    function GetPageRects(APageIndex: Integer): TList<TdxRectF>; overload;
    procedure AddLineRects(APageRects: TList<TdxRectF>; APageLine: TdxPDFTextLine;
      const AStartPosition, AEndPosition: TdxPDFPageTextPosition; var AContainsEndOfLine: Boolean);
    procedure AddPageRects(ARects: TList<TdxRectF>; ALines: TdxPDFTextLineList);
    procedure AddRects(APageIndex: Integer; ARects: TList<TdxRectF>);
  strict protected
    function Contains(const APosition: TdxPDFPosition): Boolean; override;
    function GetHitCode: Integer; override;
    function Same(AValue: TdxPDFCustomSelection): Boolean; override;

    property Pages: TdxPDFPages read FPages;
  protected
    function Exclude(const ARange: TdxPDFPageTextRange): Boolean;

    property Ranges: TdxPDFPageTextRanges read FRanges;
    property PageRects[APageIndex: Integer]: TList<TdxRectF> read GetPageRects;
  public
    constructor Create(ABackColor, AFrameColor: TdxAlphaColor; APages: TdxPDFPages;
      const ARanges: TdxPDFPageTextRanges); reintroduce;
    destructor Destroy; override;
  end;

  { TdxPDFTextSelection }

  TdxPDFTextSelection = class(TdxPDFTextHighlight)
  strict private
    FConverted: Boolean;
    FText: string;
    function GetText: string;
  protected
    function GetHitCode: Integer; override;
  public
    property Text: string read GetText;
  end;

  TdxPDFDocumentCaretViewData = record
    Angle: Single;
    Height: Single;
    TopLeft: TdxPointF;
  end;

  TdxPDFDocumentCaret = record
    Position: TdxPDFTextPosition;
    StartCoordinates: TdxPointF;
    ViewData: TdxPDFDocumentCaretViewData;
  end;

  { TdxPDFDocumentCaretHelper }

  TdxPDFDocumentCaretHelper = record helper for TdxPDFDocumentCaret
  public
    class function Create(const APosition: TdxPDFTextPosition; const AViewData: TdxPDFDocumentCaretViewData;
      const AStartCoordinates: TdxPointF): TdxPDFDocumentCaret; static;
    class function Invalid: TdxPDFDocumentCaret; static;
    function IsValid: Boolean;
    function Same(const AValue: TdxPDFDocumentCaret): Boolean;
  end;

  { TdxPDFDocumentCaretViewDataHelper }

  TdxPDFDocumentCaretViewDataHelper = record helper for TdxPDFDocumentCaretViewData
  public
    class function Create(const ATopLeft: TdxPointF; AHeight, AAngle: Single): TdxPDFDocumentCaretViewData; static;
    class function Invalid: TdxPDFDocumentCaretViewData; static;
    function Same(const AValue: TdxPDFDocumentCaretViewData): Boolean;
  end;

  { TdxPDFViewerClickController }

  TdxPDFViewerClickController = class
  strict private
    FClickCount: Integer;
    FLastClickPoint: TPoint;
    FLastButton: TMouseButton;
    FTimer: TcxTimer;
    procedure OnTimerHandler(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Click(AButton: TMouseButton; X, Y: Integer);

    property ClickCount: Integer read FClickCount;
  end;

implementation

uses
  Types, Math, dxTypeHelpers, cxFormats, cxControls, dxGDIPlusClasses, dxPDFBase, dxPDFUtils, dxPDFDocumentViewer,
  dxPDFCommandInterpreter;

type
  TdxPDFDocumentCustomViewerAccess = class(TdxPDFDocumentCustomViewer);
  TdxPDFDocumentAccess = class(TdxPDFDocument);
  TdxPDFImageAccess = class(TdxPDFImage);
  TdxPDFTextLineAccess = class(TdxPDFTextLine);
  TdxPDFTextWordPartAccess = class(TdxPDFTextWordPart);

  { TdxPDFImageRenderer }

  TdxPDFImageRenderer = class(TdxPDFGraphicsDevice)
  protected
    procedure BeginText; override;
    procedure ClipPaths(AUseNonzeroWindingRule: Boolean); override;
    procedure DrawShading(AShading: TdxPDFCustomShading); override;
    procedure DrawString(const ALocation: TdxPointF; const AData: TdxPDFStringData;
      const AOffsets: TDoubleDynArray); override;
    procedure EndText; override;
    procedure InitializeClipBounds(const ATopLeft, ABottomRight: TdxPointF); override;
    procedure FillPaths(AUseNonzeroWindingRule: Boolean); override;
    procedure SetTextMatrix(AMatrix: TdxPDFTransformationMatrix); overload; override;
    procedure StrokePaths; override;
  end;

{ TdxPDFCustomSelection }

constructor TdxPDFCustomSelection.Create(ABackColor, AFrameColor: TdxAlphaColor);
begin
  inherited Create;
  FBackColor := ABackColor;
  FFrameColor := AFrameColor;
end;

function TdxPDFCustomSelection.Contains(const APosition: TdxPDFPosition): Boolean;
begin
  Result := False;
end;

function TdxPDFCustomSelection.Same(AValue: TdxPDFCustomSelection): Boolean;
begin
  Result := (AValue <> nil) and (HitCode = AValue.HitCode);
end;

{ TdxPDFImageSelection }

constructor TdxPDFImageSelection.Create(ABackColor, AFrameColor: TdxAlphaColor; APageIndex: Integer;
  AImage: TdxPDFImage; const AClipRect: TdxRectF);
begin
  inherited Create(ABackColor, AFrameColor);
  Image := AImage;
  FPageIndex := APageIndex;
  FClipRect := AClipRect;
end;

destructor TdxPDFImageSelection.Destroy;
begin
  Image := nil;
  inherited Destroy;
end;

function TdxPDFImageSelection.Contains(const APosition: TdxPDFPosition): Boolean;
var
  R: TdxRectF;
begin
  R := cxRectAdjustF(Bounds);
  Result := (PageIndex = APosition.PageIndex) and TdxPDFUtils.PtInRect(R, APosition.Point);
end;

function TdxPDFImageSelection.GetHitCode: Integer;
begin
  Result := hcImage;
end;

function TdxPDFImageSelection.Same(AValue: TdxPDFCustomSelection): Boolean;
var
  ASelection: TdxPDFImageSelection;
begin
  Result := inherited Same(AValue);
  if Result  then
  begin
    ASelection := AValue as TdxPDFImageSelection;
    Result := (AValue <> nil) and (Image = ASelection.Image) and (PageIndex = ASelection.PageIndex) and
      TdxPDFUtils.RectIsEqual(Bounds, ASelection.Bounds, 0.0001);
  end;
end;

function TdxPDFImageSelection.GetImageBitmap(AViewer: TObject): TcxBitmap;
const
  Precision = 0.1;
var
  AOriginalPageBitmap: TcxBitmap;
  ARenderer: TdxPDFImageRenderer;
  AParameters: TdxPDFRenderParameters;
  ADocument: TdxPDFDocument;
  APage: TdxPDFPage;
  APageSize: TPoint;
  AImageBounds: TdxRectF;
  AClipRect: TdxRectF;
  AScale: Single;
begin
  AImageBounds := TdxPDFImageAccess(FImage).Bounds;
  AScale := Bitmap.Width / AImageBounds.Width;
  ADocument := TdxPDFDocumentCustomViewerAccess(AViewer).Document;
  APage := TdxPDFDocumentAccess(ADocument).Pages[FPageIndex];
  CalculateRendererParameters(APage, AScale, APageSize, AClipRect);
  AOriginalPageBitmap := TcxBitmap.CreateSize(APageSize.X, APageSize.Y);
  AParameters := TdxPDFRenderParameters.Create(TdxPDFDocumentAccess(ADocument).State);
  try
    AParameters.Angle := ConvertToRotationAngle(APage.RotationAngle);
    AParameters.Canvas := AOriginalPageBitmap.Canvas;
    AParameters.Rect := AOriginalPageBitmap.ClientRect;
    AParameters.ScaleFactor := AScale;
    AParameters.CancelCallback := nil;
    ARenderer := TdxPDFImageRenderer.Create;
    try
      ARenderer.ExportAndPack(APage, AParameters);
    finally
      ARenderer.Free;
    end;
    Result := TcxBitmap.CreateSize(cxRect(AClipRect));
    Result.Canvas.CopyRect(Result.ClientRect, AOriginalPageBitmap.Canvas, cxRect(AClipRect));
  finally
    AParameters.Free;
    AOriginalPageBitmap.Free;
  end;
end;

function TdxPDFImageSelection.GetBitmap: TBitmap;
begin
  Result := FImage.Bitmap;
end;

function TdxPDFImageSelection.GetBounds: TdxRectF;
begin
  if FClipRect <> dxNullRectF then
    Result := GetClipBounds
  else
    Result := TdxPDFImageAccess(FImage).Bounds;
end;

function TdxPDFImageSelection.GetClipBounds: TdxRectF;
begin
  if not TdxPDFUtils.Intersects(Result, TdxPDFImageAccess(FImage).Bounds, FClipRect) and (Result.Width > 0) and
    (Result.Height > 0) then
    Result := dxRectF(Result.Left, Result.Top, Result.Left + 1, Result.Bottom + 1);
end;

procedure TdxPDFImageSelection.SetImage(const AValue: TdxPDFImage);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FImage));
end;

function TdxPDFImageSelection.ConvertToRotationAngle(AAngle: Integer): TcxRotationAngle;
begin
  case AAngle of
    270:
      Result := raMinus90;
    90:
      Result := raPlus90;
    180:
      Result := ra180;
  else
    Result := ra0;
  end;
end;

procedure TdxPDFImageSelection.CalculateRendererParameters(APage: TdxPDFPage; AScale: Single;
  out APageSize: TPoint; out AClipRect: TdxRectF);
var
  R: TdxRectF;
  ATempPageSize: TPoint;
begin
  APageSize.X := Round(APage.Size.X * AScale);
  APageSize.Y := Round(APage.Size.Y * AScale);

  ATempPageSize := APageSize;
  if (APage.RotationAngle = 90) or (APage.RotationAngle = 270) then
    ATempPageSize := dxPointF(ATempPageSize.Y, ATempPageSize.X);

  AClipRect.Left := Bounds.Left * AScale;
  AClipRect.Right := Bounds.Right * AScale;
  AClipRect.Top := ATempPageSize.Y - Bounds.Bottom * AScale;
  AClipRect.Height := Bounds.Height * AScale;

  R := cxRectUnion(AClipRect, TdxRectF.CreateSize(0, 0, APageSize.X, APageSize.Y));

  if (APage.RotationAngle = 90) or (APage.RotationAngle = 270) then
    APageSize := dxPointF(R.Height, R.Width)
  else
    APageSize := dxPointF(R.Width, R.Height);
end;

{ TdxPDFTextHighlight }

constructor TdxPDFTextHighlight.Create(ABackColor, AFrameColor: TdxAlphaColor; APages: TdxPDFPages;
  const ARanges: TdxPDFPageTextRanges);
begin
  inherited Create(ABackColor, AFrameColor);
  FRects := TObjectDictionary<Integer, TList<TdxRectF>>.Create([doOwnsValues]);
  FPages := APages;
  FRanges := ARanges;
end;

destructor TdxPDFTextHighlight.Destroy;
begin
  FreeAndNil(FRects);
  SetLength(FRanges, 0);
  inherited Destroy;
end;

function TdxPDFTextHighlight.Exclude(const ARange: TdxPDFPageTextRange): Boolean;
var
  I: Integer;
  AUpdatedRanges: TdxPDFPageTextRanges;
begin
  SetLength(AUpdatedRanges, 0);
  for I := Low(Ranges) to High(Ranges) do
    if not TdxPDFPageTextRange.Same(Ranges[I], ARange) then
      TdxPDFTextUtils.AddRange(Ranges[I], AUpdatedRanges);
  Result := Length(AUpdatedRanges) < Length(Ranges);
  if Result then
  begin
    FRanges := AUpdatedRanges;
    FReeAndNil(FRects);
  end;
end;

function TdxPDFTextHighlight.Contains(const APosition: TdxPDFPosition): Boolean;
var
  I: Integer;
  ARects: TList<TdxRectF>;
begin
  ARects := PageRects[APosition.PageIndex];
  Result := (ARects <> nil) and (ARects.Count > 0);
  if Result then
    for I := 0 to ARects.Count - 1 do
    begin
      Result := TdxPDFUtils.PtInRect(ARects[I], APosition.Point);
      if Result then
        Break;
    end;
end;

function TdxPDFTextHighlight.GetHitCode: Integer;
begin
  Result := hcTextHighlight;
end;

function TdxPDFTextHighlight.Same(AValue: TdxPDFCustomSelection): Boolean;
var
  I: Integer;
  ATextSelection: TdxPDFTextSelection;
begin
  Result := AValue is TdxPDFTextSelection;
  if Result then
  begin
    ATextSelection := TdxPDFTextSelection(AValue);
    Result := inherited Same(ATextSelection) and (Length(Ranges) = Length(ATextSelection.Ranges));
    if Result then
      for I := 0 to Length(Ranges) - 1 do
      begin
        Result := TdxPDFPageTextRange.Same(Ranges[I], ATextSelection.Ranges[I]);
        if not Result then
          Break;
      end;
  end;
end;

function TdxPDFTextHighlight.GetPageRects(APageIndex: Integer): TList<TdxRectF>;
var
  I: Integer;
  AContentsEndOfLine: Boolean;
  ACurrentLine: TdxPDFTextLine;
  APage: TdxPDFPage;
  ARange: TdxPDFPageTextRange;
  ARects: TList<TdxRectF>;
begin
  if not FRects.TryGetValue(APageIndex, Result) then
  begin
    for I := Low(FRanges) to High(FRanges) do
    begin
      ARange := FRanges[I];
      if ARange.PageIndex = APageIndex then
      begin
        APage := FPages[ARange.PageIndex];
        if APage.RecognizedContent <> nil then
        begin
          ARects := TList<TdxRectF>.Create;
          if (ARange.StartPosition.WordIndex = 0) and (ARange.EndPosition.WordIndex = 0) or ARange.WholePage then
            AddPageRects(ARects, APage.RecognizedContent.TextLines)
          else
            for ACurrentLine in APage.RecognizedContent.TextLines do
            begin
              AddLineRects(ARects, ACurrentLine, ARange.StartPosition, ARange.EndPosition, AContentsEndOfLine);
              if AContentsEndOfLine then
                Break;
            end;
          AddRects(ARange.PageIndex, ARects);
        end;
      end;
    end;
    if not FRects.TryGetValue(APageIndex, Result) then
      Result := nil;
  end;
end;

procedure TdxPDFTextHighlight.AddLineRects(APageRects: TList<TdxRectF>; APageLine: TdxPDFTextLine;
  const AStartPosition, AEndPosition: TdxPDFPageTextPosition; var AContainsEndOfLine: Boolean);
var
  AFirstPartIndex: Integer;
  AHighlights: TList<TdxPDFOrientedRect>;
  ALastPart: TdxPDFTextWordPart;
  ALine: TdxPDFTextLineAccess;
  R: TdxPDFOrientedRect;
  AStart, AEnd: TdxPDFPageTextPosition;
begin
  AContainsEndOfLine := False;
  ALine := TdxPDFTextLineAccess(APageLine);
  if ALine.WordPartList.Count > 0 then
  begin
    AHighlights := nil;
    AContainsEndOfLine := ALine.PositionInLine(AEndPosition.WordIndex, AEndPosition.Offset);
    AFirstPartIndex := ALine.WordPartList[0].WordIndex;
    AEnd.WordIndex := IfThen(AContainsEndOfLine, AEndPosition.WordIndex - AFirstPartIndex, 0);
    AEnd.Offset := IfThen(AContainsEndOfLine, AEndPosition.Offset -
      TdxPDFTextWordPartAccess(ALine.WordPartList[AEnd.WordIndex]).WrapOffset, 0);
    if ALine.PositionInLine(AStartPosition.WordIndex, AStartPosition.Offset) then
    begin
      AStart.WordIndex := AStartPosition.WordIndex - AFirstPartIndex;
      AStart.Offset := AStartPosition.Offset - TdxPDFTextWordPartAccess(ALine.WordPartList[AStart.WordIndex]).WrapOffset;
      if AContainsEndOfLine then
        AHighlights := ALine.GetHighlights(AStart.WordIndex, AStart.Offset, AEnd.WordIndex, AEnd.Offset, True)
      else
        AHighlights := ALine.GetHighlights(AStart.WordIndex, AStart.Offset, True);
    end
    else
      if AContainsEndOfLine then
        AHighlights := ALine.GetHighlights(0, 0, AEnd.WordIndex, AEnd.Offset, True)
      else
      begin
        ALastPart := ALine.WordPartList.Last;
        if ((AStartPosition.WordIndex = 0) or (AStartPosition.WordIndex <= AFirstPartIndex)) and
          ((AEndPosition.WordIndex = 0) or (ALastPart.WordIndex - 1 <= AEndPosition.WordIndex)) then
          AHighlights := ALine.GetHighlights(0, 0, ALastPart.WordIndex - AFirstPartIndex,
            Length(TdxPDFTextWordPartAccess(ALastPart).Characters), True);
      end;
    if AHighlights <> nil then
    begin
      for R in AHighlights do
        if R.Width > 0 then
          APageRects.Add(R.RotatedRect);
      AHighlights.Free;
    end;
  end;
end;

procedure TdxPDFTextHighlight.AddPageRects(ARects: TList<TdxRectF>; ALines: TdxPDFTextLineList);
var
  ALine: TdxPDFTextLine;
begin
  for ALine in ALines do
    ARects.Add(TdxPDFTextLineAccess(ALine).Bounds.Rect);
end;

procedure TdxPDFTextHighlight.AddRects(APageIndex: Integer; ARects: TList<TdxRectF>);
var
  APageRects: TList<TdxRectF>;
begin
  if FRects.TryGetValue(APageIndex, APageRects) then
  begin
    APageRects.AddRange(ARects);
    ARects.Free;
  end
  else
    FRects.Add(APageIndex, ARects);
end;

{ TdxPDFTextSelection }

function TdxPDFTextSelection.GetHitCode: Integer;
begin
  Result := hcTextSelection;
end;

function TdxPDFTextSelection.GetText: string;
begin
  if not FConverted then
  begin
    FText := TdxPDFTextUtils.ConvertToString(Ranges, Pages);
    FConverted := True;
  end;
  Result := FText;
end;

{ TdxPDFDocumentCaretHelper }

class function TdxPDFDocumentCaretHelper.Create(const APosition: TdxPDFTextPosition;
  const AViewData: TdxPDFDocumentCaretViewData; const AStartCoordinates: TdxPointF): TdxPDFDocumentCaret;
begin
  Result.Position := APosition;
  Result.StartCoordinates := AStartCoordinates;
  Result.ViewData := AViewData;
end;

class function TdxPDFDocumentCaretHelper.Invalid: TdxPDFDocumentCaret;
begin
  Result.Position := TdxPDFTextPosition.Invalid;
  Result.StartCoordinates := dxNullPointF;
  Result.ViewData := TdxPDFDocumentCaretViewData.Invalid;
end;

function TdxPDFDocumentCaretHelper.IsValid: Boolean;
begin
  Result := (Position.IsValid) and (ViewData.Height > 0);
end;

function TdxPDFDocumentCaretHelper.Same(const AValue: TdxPDFDocumentCaret): Boolean;
begin
  Result := Position.Same(AValue.Position) and cxPointIsEqual(StartCoordinates, AValue.StartCoordinates) and
    ViewData.Same(AValue.ViewData);
end;

{ TdxPDFDocumentCaretViewDataHelper }

class function TdxPDFDocumentCaretViewDataHelper.Create(const ATopLeft: TdxPointF;
  AHeight, AAngle: Single): TdxPDFDocumentCaretViewData;
begin
  Result.Angle := AAngle;
  Result.Height := AHeight;
  Result.TopLeft := ATopLeft;
end;

class function TdxPDFDocumentCaretViewDataHelper.Invalid: TdxPDFDocumentCaretViewData;
begin
  Result.Angle := -1;
  Result.Height := -1;
  Result.TopLeft := dxNullPointF;
end;

function TdxPDFDocumentCaretViewDataHelper.Same(const AValue: TdxPDFDocumentCaretViewData): Boolean;
begin
  Result := (Angle = AValue.Angle) and cxPointIsEqual(TopLeft, AValue.TopLeft) and (Height = AValue.Height);
end;

{ TdxPDFViewerClickController }

constructor TdxPDFViewerClickController.Create;
begin
  inherited Create;
  FTimer := TcxTimer.Create(nil);
  FTimer.Interval := GetDblClickInterval;
  FTimer.OnTimer := OnTimerHandler;
end;

destructor TdxPDFViewerClickController.Destroy;
begin
  FreeAndNil(FTimer);
  inherited Destroy;
end;

procedure TdxPDFViewerClickController.OnTimerHandler(Sender: TObject);
begin
  FClickCount := 0;
  FTimer.Enabled := False;
end;

procedure TdxPDFViewerClickController.Click(AButton: TMouseButton; X, Y: Integer);
begin
  FTimer.Enabled := True;
  FClickCount := IfThen(AButton <> FLastButton, 1, FClickCount + 1);
  if (Abs(FLastClickPoint.X - X) > 0) or (Abs(FLastClickPoint.Y - Y) > 0) or (FClickCount = 5) then
    FClickCount := 1;
  FLastButton := AButton;
  FLastClickPoint := cxPoint(X, Y);
end;

{ TdxPDFImageRenderer }

procedure TdxPDFImageRenderer.BeginText;
begin
// do nothing
end;

procedure TdxPDFImageRenderer.ClipPaths(AUseNonzeroWindingRule: Boolean);
begin
// do nothing
end;

procedure TdxPDFImageRenderer.DrawShading(AShading: TdxPDFCustomShading);
begin
// do nothing
end;

procedure TdxPDFImageRenderer.DrawString(const ALocation: TdxPointF; const AData: TdxPDFStringData;
  const AOffsets: TDoubleDynArray);
begin
// do nothing
end;

procedure TdxPDFImageRenderer.EndText;
begin
// do nothing
end;

procedure TdxPDFImageRenderer.InitializeClipBounds(const ATopLeft, ABottomRight: TdxPointF);
begin
  FClipBounds := GetRenderParameters.Rect;
end;

procedure TdxPDFImageRenderer.FillPaths(AUseNonzeroWindingRule: Boolean);
begin
// do nothing
end;

procedure TdxPDFImageRenderer.SetTextMatrix(AMatrix: TdxPDFTransformationMatrix);
begin
// do nothing
end;

procedure TdxPDFImageRenderer.StrokePaths;
begin
// do nothing
end;

end.
