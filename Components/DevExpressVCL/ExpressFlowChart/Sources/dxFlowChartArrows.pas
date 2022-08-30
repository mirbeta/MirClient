{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressFlowChart                                         }
{                                                                    }
{           Copyright (c) 1998-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSFLOWCHART AND ALL ACCOMPANYING }
{   VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY.              }
{                                                                    }
{   THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED       }
{   FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE         }
{   COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE        }
{   AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT   }
{   AND PERMISSION FROM DEVELOPER EXPRESS INC.                       }
{                                                                    }
{   CONSULT THE end USER LICENSE AGREEMENT FOR INFORMATION ON        }
{   ADDITIONAL RESTRICTIONS.                                         }
{                                                                    }
{********************************************************************}

unit dxFlowChartArrows;

interface

{$I cxVer.inc}
{$SCOPEDENUMS ON}

uses
  Types, SysUtils, Classes, Generics.Defaults, Generics.Collections,
  dxCore, dxCoreClasses, dxCoreGraphics, dxGDIPlusAPI, dxGDIPlusClasses, cxGeometry;

type

  { TdxFlowChartArrowShape }

  TdxFlowChartArrowShape = class
  {$REGION 'internal types'}
  public type
    TExcludeConnectorTipKind = (None, Half, Full);
    TConnectorKind = (Left, Right, Center);

    TSegment = class(TdxGPPath)
    strict private
      FClosed: Boolean;
      FFilled: Boolean;
      FHasBackground: Boolean;
      FTransparent: Boolean;
      FRoundedJoin: Boolean;
      function CreatePen(AColor: TdxAlphaColor; AWidth: Integer; AScale: Single): TdxGPPen;
      function CreateBrush(AColor: TdxAlphaColor): TdxGPBrush;
    public
      constructor Create(AClosed, AFilled, ATransparent: Boolean);
      procedure AddCircle(X, ACenterY, ADiameter: Single);
      procedure Draw(AGraphics: TdxGPGraphics; AScale: Single; APenWidth: Byte; APenColor, ABrushColor: TdxAlphaColor);
      procedure ExcludeRegion(AGraphics: TdxGPGraphics);

      property Closed: Boolean read FClosed;
      property Filled: Boolean read FFilled;
      property HasBackground: Boolean read FHasBackground;
      property Transparent: Boolean read FTransparent;
      property RoundedJoin: Boolean read FRoundedJoin write FRoundedJoin;
    end;

    TSegments = class(TObjectList<TSegment>)
    public
      function NewPath(AClosed, AFilled, ATransparent: Boolean): TSegment;
    end;

    TPopulateSegmentsProc = reference to procedure (ASegments: TSegments; ASize: TSize);
  {$ENDREGION}
  strict private
    FConnectorKind: TConnectorKind;
    FDefaultSize: TSize;
    FExcludeConnectionTipKind: TExcludeConnectorTipKind;
    FHasBackground: Boolean;
    FPopulateSegmentsProc: TPopulateSegmentsProc;
    FRotatable: Boolean;
    FSizedArrows: TObjectDictionary<TSize, TSegments>;
    FTipOffset: Single;
    function GetConnectionPoint(const APoint: TPoint; const ASize: TSize): TdxPointF;
    function GetRotationPoint(const ASize: TSize): TdxPointF;
    procedure CorrectDrawingSize(var ASize: TSize);
  strict protected
    function CreateArrow(ASize: TSize): TSegments;
    procedure CreateDefaultSizeArrow;
    procedure ExcludeConnectionTipFromDrawing(AGraphics: TdxGPGraphics; const APoint: TPoint; AScale: Single; APenWidth: Byte);
    function GetArrowBySize(const ASize: TSize): TSegments;
    procedure PrepareGraphics(AGraphics: TdxGPGraphics; const ATargetPoint, APrevPoint: TPoint;
      ASize: TSize; APenWidth, APathCount: Integer; AScale: Single);
    procedure UnprepareGraphics(AGraphics: TdxGPGraphics);

    property ConnectorKind: TConnectorKind read FConnectorKind;
    property ExcludeConnectionTipKind: TExcludeConnectorTipKind read FExcludeConnectionTipKind;
    property Rotatable: Boolean read FRotatable;
  public
    constructor Create(const ADefaultSize: TSize; AConnectorKind: TConnectorKind;
      const APopulateSegmentsProc: TPopulateSegmentsProc; ARotatable: Boolean = True;
      AExcludeConnectionTipKind: TExcludeConnectorTipKind = TExcludeConnectorTipKind.None); overload;
    constructor CreateSharpTipArrow(const ADefaultSize: TSize;
      const APopulateSegmentsProc: TPopulateSegmentsProc); overload;
    destructor Destroy; override;
    procedure Draw(AGraphics: TdxGPGraphics; const ATargetPoint, APrevPoint: TPoint; ASize: TSize; AScale: Single;
      APenWidth: Byte; APenColor, ABrushColor: TdxAlphaColor); virtual;
    procedure DrawPreview(AGraphics: TdxGPGraphics; const ABounds: TRect; AScale: Single; ABeginArrow: Boolean); virtual;
    function GetActualSize(const ASize: TSize; APenWidth: Integer; AScale: Single; AKeepProportion: Boolean): TSize;

    property DefaultSize: TSize read FDefaultSize;
    property HasBackground: Boolean read FHasBackground;
  end;

  { TdxFlowChartArrowShapeRepository }

  TdxFlowChartArrowShapeRepository = class
  strict private
    FIDs: TDictionary<Integer, TdxFlowChartArrowShape>;
    FItems: TObjectList<TdxFlowChartArrowShape>;
    procedure AddArrow(AID: Integer; AShape: TdxFlowChartArrowShape);
    function GetCount: Integer;
    function GetItem(Index: Integer): TdxFlowChartArrowShape;
  strict protected
    procedure CreateLegacyArrows;
    procedure CreateASMEArrow(AID: Integer; AFilled: Boolean);
    procedure CreateDiamond(AID: Integer; AFilled: Boolean);
    procedure CreateClosedDiamond(AID: Integer; AFilled: Boolean);
    procedure CreateDiamonds;
    procedure CreateOneDash(AID: Integer);
    procedure CreateTwoDash(AID: Integer);
    procedure CreateThreeDash(AID: Integer);
    procedure CreateClosedOneDash(AID: Integer; AFilled: Boolean);
    procedure CreateClosedTwoDash(AID: Integer; AFilled: Boolean);
    procedure CreateClosedThreeDash(AID: Integer; AFilled: Boolean);
    procedure CreateDashArrows;
    procedure CreateDoubleArrows;
    procedure CreateSharpArrow(AID: Integer; AFilled: Boolean);
    procedure CreateIndentedArrow(AID: Integer; AFilled: Boolean);
    procedure CreateOutdentedArrow(AID: Integer; AFilled: Boolean);
    procedure Populate; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    function GetShapeByID(AID: Integer): TdxFlowChartArrowShape;
    procedure DrawPreview(AID: Integer; AGraphics: TdxGPGraphics; const ABounds: TRect; AScale: Single; ABeginArrow: Boolean);

    property Count: Integer read GetCount;
    property Items[Index: Integer]: TdxFlowChartArrowShape read GetItem; default;
  end;

implementation

uses
  Windows, Math, dxTypeHelpers, dxflchrt;

{ TdxFlowChartArrow.TSegment }

constructor TdxFlowChartArrowShape.TSegment.Create(AClosed, AFilled, ATransparent: Boolean);
begin
  inherited Create;
  FClosed := AClosed;
  FFilled := AFilled;
  FTransparent := ATransparent;
  FHasBackground := not Transparent and Closed and not Filled;
end;

procedure TdxFlowChartArrowShape.TSegment.AddCircle(X, ACenterY, ADiameter: Single);
var
  R: TdxRectF;
  ARadius: Single;
begin
  ARadius := ADiameter / 2;
  if IsZero(ARadius) then
    Exit;
  R.Init(X, ACenterY - ARadius, X + ADiameter, ACenterY + ARadius);
  AddEllipse(R);
end;

function TdxFlowChartArrowShape.TSegment.CreateBrush(AColor: TdxAlphaColor): TdxGPBrush;
begin
  if Transparent or not Closed or not (Filled or not TdxAlphaColors.IsTransparentOrEmpty(AColor)) then
    Exit(nil);
  Result := TdxGPBrush.Create;
  Result.Color := AColor;
end;

function TdxFlowChartArrowShape.TSegment.CreatePen(AColor: TdxAlphaColor; AWidth: Integer; AScale: Single): TdxGPPen;
begin
  if TdxAlphaColors.IsTransparentOrEmpty(AColor) then
    Exit(nil);
  Result := TdxGPPen.Create(AColor, Max(1, AWidth * AScale));
  if RoundedJoin then
    Result.LineJoin := LineJoinRound;
end;

procedure TdxFlowChartArrowShape.TSegment.Draw(AGraphics: TdxGPGraphics; AScale: Single; APenWidth: Byte; APenColor, ABrushColor: TdxAlphaColor);
var
  APen: TdxGPPen;
  ABrush: TdxGPBrush;
begin
  if Filled then
    ABrushColor := APenColor;
  APen := CreatePen(APenColor, APenWidth, AScale);
  ABrush := CreateBrush(ABrushColor);
  try
    AGraphics.Path(Self, APen, ABrush);
  finally
    APen.Free;
    ABrush.Free;
  end;
end;

procedure TdxFlowChartArrowShape.TSegment.ExcludeRegion(AGraphics: TdxGPGraphics);
begin
  if Closed then
    AGraphics.SetClipPath(Self, TdxGPCombineMode.gmExclude);
end;

{ TdxFlowChartArrow.TSegments }

function TdxFlowChartArrowShape.TSegments.NewPath(AClosed, AFilled, ATransparent: Boolean): TSegment;
begin
  Result := TSegment.Create(AClosed, AFilled, ATransparent);
  Add(Result);
end;

{ TdxFlowChartArrow }

constructor TdxFlowChartArrowShape.Create(const ADefaultSize: TSize; AConnectorKind: TConnectorKind;
  const APopulateSegmentsProc: TPopulateSegmentsProc; ARotatable: Boolean = True;
  AExcludeConnectionTipKind: TExcludeConnectorTipKind = TExcludeConnectorTipKind.None);
begin
  FConnectorKind := AConnectorKind;
  FDefaultSize := ADefaultSize;
  FPopulateSegmentsProc := APopulateSegmentsProc;
  FRotatable := ARotatable;
  if AConnectorKind = TConnectorKind.Left then
    FExcludeConnectionTipKind := AExcludeConnectionTipKind
  else
    FExcludeConnectionTipKind := TExcludeConnectorTipKind.None;
  FSizedArrows := TObjectDictionary<TSize, TSegments>.Create([doOwnsValues]);
  CreateDefaultSizeArrow;
end;

constructor TdxFlowChartArrowShape.CreateSharpTipArrow(const ADefaultSize: TSize;
  const APopulateSegmentsProc: TPopulateSegmentsProc);
begin
  Create(ADefaultSize, TConnectorKind.Left, APopulateSegmentsProc, True, TExcludeConnectorTipKind.Full);
end;

destructor TdxFlowChartArrowShape.Destroy;
begin
  FSizedArrows.Free;
  inherited Destroy;
end;

function TdxFlowChartArrowShape.CreateArrow(ASize: TSize): TSegments;
begin
  Result := TSegments.Create;
  CorrectDrawingSize(ASize);
  FPopulateSegmentsProc(Result, ASize);
end;

procedure TdxFlowChartArrowShape.CreateDefaultSizeArrow;
var
  ASegments: TSegments;
  AActualSize: TSize;
  I: Integer;
begin
  AActualSize := GetActualSize(FDefaultSize, 1, 1, False);
  ASegments := CreateArrow(AActualSize);
  FSizedArrows.Add(AActualSize, ASegments);
  for I := 0 to ASegments.Count - 1 do
    if ASegments[I].HasBackground then
    begin
      FHasBackground := True;
      break;
    end;
end;

function TdxFlowChartArrowShape.GetActualSize(const ASize: TSize; APenWidth: Integer; AScale: Single; AKeepProportion: Boolean): TSize;
var
  ARatio: Single;
  AAdditionalGeometrySize: Integer;
begin
  AAdditionalGeometrySize := Max(0, (APenWidth - 1)) * 4;
  if AKeepProportion then
  begin
    if DefaultSize.cx > DefaultSize.cy then
    begin
      ARatio := DefaultSize.cy / DefaultSize.cx;
      Result.cx := Round((ASize.cx + AAdditionalGeometrySize) * AScale);
      Result.cy := Round(Result.cx * ARatio);
    end
    else
    begin
      ARatio := DefaultSize.cx / DefaultSize.cy;
      Result.cy := Round((ASize.cy + AAdditionalGeometrySize) * AScale);
      Result.cx := Round(Result.cy * ARatio);
    end
  end
  else
  begin
    Result.cy := Round((ASize.cy + AAdditionalGeometrySize) * AScale);
    Result.cx := Round((ASize.cx + AAdditionalGeometrySize) * AScale);
  end;
end;

procedure TdxFlowChartArrowShape.CorrectDrawingSize(var ASize: TSize);
begin
  Dec(ASize.cx);
  Dec(ASize.cy);
end;

function TdxFlowChartArrowShape.GetArrowBySize(const ASize: TSize): TSegments;
begin
  if not FSizedArrows.TryGetValue(ASize, Result) then
  begin
    Result := CreateArrow(ASize);
    FSizedArrows.Add(ASize, Result);
  end;
end;

function TdxFlowChartArrowShape.GetConnectionPoint(const APoint: TPoint; const ASize: TSize): TdxPointF;
begin
  Result.Y := APoint.Y - ASize.cy div 2;
  case ConnectorKind of
    TConnectorKind.Left:
      Result.X := APoint.X;
    TConnectorKind.Right:
      Result.X := APoint.X - ASize.cx;
  else
    Result.X := APoint.X - ASize.cx div 2;
  end;
end;

function TdxFlowChartArrowShape.GetRotationPoint(const ASize: TSize): TdxPointF;
begin
  Result.Y := ASize.cy div 2;
  case ConnectorKind of
    TConnectorKind.Left:
      Result.X := 0;
    TConnectorKind.Right:
      Result.X := ASize.cx;
  else
    Result.X := ASize.cx div 2;
  end;
end;

function GetAngleABC(const A, B, C: TPoint): Single;
var
  AB, CB: TPoint;
  ADotProduct, ACrossProduct, Alpha: Single;
begin
  AB.Init(B.X - A.X, B.Y - A.Y);
  CB.Init(B.X - C.X, B.Y - C.Y);
  ADotProduct := (AB.X * CB.X + AB.Y * CB.Y);
  ACrossProduct := (AB.X * CB.Y - AB.Y * CB.X);
  Alpha := ArcTan2(ACrossProduct, ADotProduct);
  Result := Alpha * 180 / PI;
end;

procedure TdxFlowChartArrowShape.Draw(AGraphics: TdxGPGraphics; const ATargetPoint, APrevPoint: TPoint;
  ASize: TSize; AScale: Single; APenWidth: Byte; APenColor, ABrushColor: TdxAlphaColor);
var
  AArrow: TSegments;
  AActualSize: TSize;
  I: Integer;
begin
  AScale := EnsureRange(AScale, 0.1, 10);
  ASize.Init(Max(1, ASize.cx), Max(1, ASize.cy));
  AActualSize := GetActualSize(ASize, APenWidth, AScale, False);
  AArrow := GetArrowBySize(AActualSize);
  PrepareGraphics(AGraphics, ATargetPoint, APrevPoint, AActualSize, APenWidth, AArrow.Count, AScale);
  try
    for I := 0 to AArrow.Count - 1 do
      AArrow[I].Draw(AGraphics, AScale, APenWidth, APenColor, ABrushColor);
    for I := 0 to AArrow.Count - 1 do
      AArrow[I].ExcludeRegion(AGraphics);
    if ExcludeConnectionTipKind <> TExcludeConnectorTipKind.None then
      ExcludeConnectionTipFromDrawing(AGraphics, TPoint.Create(0, AActualSize.cy div 2), AScale, APenWidth);
  finally
    UnprepareGraphics(AGraphics);
  end;
end;

procedure TdxFlowChartArrowShape.DrawPreview(AGraphics: TdxGPGraphics; const ABounds: TRect; AScale: Single; ABeginArrow: Boolean);
const
  Indent = 8;
var
  ATarget, APrev: TPoint;
begin
  AScale := EnsureRange(AScale, 0.1, 10);
  if ABeginArrow then
  begin
    ATarget.Init(Round(ABounds.Left + Indent * AScale), (ABounds.Bottom + ABounds.Top) div 2);
    APrev.Init(Round(ABounds.Right - Indent * AScale), ATarget.Y);
    case ConnectorKind of
      TConnectorKind.Right:
        Inc(ATarget.X, Round(DefaultSize.cx * AScale));
      TConnectorKind.Center:
        Inc(ATarget.X, Round(DefaultSize.cx div 2 * AScale));
    end;
  end
  else
  begin
    ATarget.Init(Round(ABounds.Right - Indent * AScale), (ABounds.Bottom + ABounds.Top) div 2);
    APrev.Init(Round(ABounds.Left + Indent * AScale), ATarget.Y);
    case ConnectorKind of
      TConnectorKind.Right:
        Dec(ATarget.X, Round(DefaultSize.cx * AScale));
      TConnectorKind.Center:
        Dec(ATarget.X, Round(DefaultSize.cx div 2 * AScale));
    end;
  end;
  AGraphics.SaveClipRegion;
  try
    AGraphics.FillRectangle(ABounds, TdxAlphaColors.White);
    Draw(AGraphics, ATarget, APrev, DefaultSize, AScale, 1, TdxAlphaColors.Black, TdxAlphaColors.White);
    AGraphics.Line(ATarget.X, ATarget.Y, APrev.X, APrev.Y, TdxAlphaColors.Black, Max(1, AScale));
  finally
    AGraphics.RestoreClipRegion;
  end;
end;

procedure TdxFlowChartArrowShape.ExcludeConnectionTipFromDrawing(AGraphics: TdxGPGraphics; const APoint: TPoint;
  AScale: Single; APenWidth: Byte);
var
  R: TRect;
  W, H: Integer;
begin
  W := Round((APenWidth - 1) * AScale);
  if W < 1 then
    Exit;
  H := W;
  if ExcludeConnectionTipKind = TExcludeConnectorTipKind.Half then
    W := Ceil(W / 4);
  R.Init(APoint.X - W, APoint.Y - H, APoint.X + W, APoint.Y + H);
  R.Offset(-Round(FTipOffset), 0);
  AGraphics.SetClipRect(R, gmExclude);
end;

procedure TdxFlowChartArrowShape.PrepareGraphics(AGraphics: TdxGPGraphics; const ATargetPoint, APrevPoint: TPoint;
  ASize: TSize; APenWidth, APathCount: Integer; AScale: Single);
var
  AAngle: Single;
  AConnectionPoint: TdxPointF;
begin
  AGraphics.SaveWorldTransform;
  AConnectionPoint := GetConnectionPoint(ATargetPoint, ASize);
  AAngle := GetAngleABC(TPoint.Create(ATargetPoint.X + 100, ATargetPoint.Y), ATargetPoint, APrevPoint);
  if ConnectorKind = TConnectorKind.Left then
    FTipOffset := (APenWidth - 1) * AScale / 2
  else
    FTipOffset := 0;
  AGraphics.TranslateWorldTransform(AConnectionPoint.X, AConnectionPoint.Y);
  if Rotatable  and not IsZero(AAngle) then
  begin
    Inc(ASize.cx, Round(FTipOffset));
    AGraphics.RotateWorldTransform(AAngle, GetRotationPoint(ASize));
  end;
  AGraphics.TranslateWorldTransform(FTipOffset, 0);
end;

procedure TdxFlowChartArrowShape.UnprepareGraphics(AGraphics: TdxGPGraphics);
begin
  AGraphics.RestoreWorldTransform;
end;

{ TdxFlowChartArrowShapeRepository }

constructor TdxFlowChartArrowShapeRepository.Create;
begin
  FItems := TObjectList<TdxFlowChartArrowShape>.Create;
  FIDs := TDictionary<Integer, TdxFlowChartArrowShape>.Create;
  Populate;
end;

destructor TdxFlowChartArrowShapeRepository.Destroy;
begin
  FIDs.Free;
  FItems.Free;
  inherited Destroy;
end;

procedure TdxFlowChartArrowShapeRepository.DrawPreview(AID: Integer; AGraphics: TdxGPGraphics;
  const ABounds: TRect; AScale: Single; ABeginArrow: Boolean);
var
  AShape: TdxFlowChartArrowShape;
begin
  AShape := GetShapeByID(AID);
  AShape.DrawPreview(AGraphics, ABounds, AScale, ABeginArrow);
end;

procedure TdxFlowChartArrowShapeRepository.AddArrow(AID: Integer; AShape: TdxFlowChartArrowShape);
begin
  try
    FIDs.Add(AID, AShape);
    FItems.Add(AShape);
  except
    AShape.Free;
    raise;
  end;
end;

function TdxFlowChartArrowShapeRepository.GetShapeByID(AID: Integer): TdxFlowChartArrowShape;
begin
  Result := FIDs[AID];
end;

function TdxFlowChartArrowShapeRepository.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TdxFlowChartArrowShapeRepository.GetItem(Index: Integer): TdxFlowChartArrowShape;
begin
  Result := FItems[Index];
end;

procedure TdxFlowChartArrowShapeRepository.CreateLegacyArrows;
begin
  AddArrow(Ord(fcaArrow),
    TdxFlowChartArrowShape.CreateSharpTipArrow(
      TSize.Create(10, 7),
      procedure (AShape: TdxFlowChartArrowShape.TSegments; ASize: TSize)
      var
        APath: TdxFlowChartArrowShape.TSegment;
        APoints: TArray<TdxPointF>;
      begin
        APath := AShape.NewPath(False, False, False);
        APoints := TArray<TdxPointF>.Create(TdxPointF.Create(ASize.cx, 0), TdxPointF.Create(0.0, Trunc(ASize.cy / 2)),
          TdxPointF.Create(ASize.cx, ASize.cy));
        if Odd(ASize.cy) then
          APoints[1].Y := APoints[1].Y +1;
        APath.AddPolyline(APoints);
      end)
    );
  AddArrow(Ord(fcaEllipse),
    TdxFlowChartArrowShape.Create(
      TSize.Create(7, 7), TdxFlowChartArrowShape.TConnectorKind.Center,
      procedure (AShape: TdxFlowChartArrowShape.TSegments; ASize: TSize)
      var
        APath: TdxFlowChartArrowShape.TSegment;
      begin
        APath := AShape.NewPath(True, False, False);
        APath.AddEllipse(TRect.CreateSize(ASize));
        APath.FigureFinish;
      end,
      False)
    );
  AddArrow(Ord(fcaRectangle),
    TdxFlowChartArrowShape.Create(
      TSize.Create(7, 7), TdxFlowChartArrowShape.TConnectorKind.Center,
      procedure (AShape: TdxFlowChartArrowShape.TSegments; ASize: TSize)
      var
        APath: TdxFlowChartArrowShape.TSegment;
      begin
        APath := AShape.NewPath(True, False, False);
        APath.AddRect(TRect.CreateSize(ASize));
        APath.FigureFinish;
      end,
      False)
    );
end;

procedure TdxFlowChartArrowShapeRepository.CreateASMEArrow(AID: Integer; AFilled: Boolean);
begin
  AddArrow(AID,
    TdxFlowChartArrowShape.CreateSharpTipArrow(
      TSize.Create(11, 5),
      procedure (AShape: TdxFlowChartArrowShape.TSegments; ASize: TSize)
      var
        APath: TdxFlowChartArrowShape.TSegment;
        APoints: TArray<TdxPointF>;
      begin
        APath := AShape.NewPath(True, AFilled, False);
        APoints := TArray<TdxPointF>.Create(TdxPointF.Create(0.0, ASize.cy div 2), TdxPointF.Create(ASize.cx, 0), TdxPointF.Create(ASize.cx, ASize.cy));
        APath.AddPolygon(APoints);
        APath.FigureFinish;
      end)
    );
end;

procedure TdxFlowChartArrowShapeRepository.CreateClosedOneDash(AID: Integer; AFilled: Boolean);
begin
  AddArrow(AID,
    TdxFlowChartArrowShape.Create(
      TSize.Create(10, 7), TdxFlowChartArrowShape.TConnectorKind.Left,
      procedure (AShape: TdxFlowChartArrowShape.TSegments; ASize: TSize)
      var
        APath: TdxFlowChartArrowShape.TSegment;
      begin
        APath := AShape.NewPath(True, AFilled, False);
        APath.AddCircle(0, Ceil(ASize.cy / 2), MulDiv(ASize.cx, 2, 3));
        APath.FigureFinish;
        APath := AShape.NewPath(True, False, False);
        APath.AddLine(ASize.cx, 0, ASize.cx, ASize.cy);
        APath.FigureFinish;
      end)
    );
end;

procedure TdxFlowChartArrowShapeRepository.CreateClosedTwoDash(AID: Integer; AFilled: Boolean);
begin
  AddArrow(AID,
    TdxFlowChartArrowShape.Create(
      TSize.Create(13, 7), TdxFlowChartArrowShape.TConnectorKind.Left,
      procedure (AShape: TdxFlowChartArrowShape.TSegments; ASize: TSize)
      var
        APath: TdxFlowChartArrowShape.TSegment;
      begin
        APath := AShape.NewPath(True, AFilled, False);
        APath.AddCircle(0, Ceil(ASize.cy / 2), ASize.cx div 2);
        APath.FigureFinish;
        APath := AShape.NewPath(True, False, False);
        APath.AddLine(ASize.cx, 0, ASize.cx, ASize.cy);
        APath.FigureFinish;
        APath := AShape.NewPath(True, False, False);
        APath.AddLine(MulDiv(ASize.cx, 3, 4), 0, MulDiv(ASize.cx, 3, 4), ASize.cy);
        APath.FigureFinish;
      end)
    );
end;

procedure TdxFlowChartArrowShapeRepository.CreateClosedThreeDash(AID: Integer; AFilled: Boolean);
begin
  AddArrow(AID,
    TdxFlowChartArrowShape.Create(
      TSize.Create(16, 7), TdxFlowChartArrowShape.TConnectorKind.Left,
      procedure (AShape: TdxFlowChartArrowShape.TSegments; ASize: TSize)
      var
        APath: TdxFlowChartArrowShape.TSegment;
      begin
        APath := AShape.NewPath(True, AFilled, False);
        APath.AddCircle(0, Ceil(ASize.cy / 2), MulDiv(ASize.cx, 2, 5));
        APath.FigureFinish;
        APath := AShape.NewPath(True, False, False);
        APath.AddLine(ASize.cx, 0, ASize.cx, ASize.cy);
        APath := AShape.NewPath(True, False, False);
        APath.AddLine(MulDiv(ASize.cx, 3, 5), 0, MulDiv(ASize.cx, 3, 5), ASize.cy);
        APath.FigureFinish;
        APath := AShape.NewPath(True, False, False);
        APath.AddLine(MulDiv(ASize.cx, 4, 5), 0, MulDiv(ASize.cx, 4, 5), ASize.cy);
        APath.FigureFinish;
      end)
    );
end;

procedure TdxFlowChartArrowShapeRepository.CreateOneDash(AID: Integer);
begin
  AddArrow(AID,
    TdxFlowChartArrowShape.Create(
      TSize.Create(7, 7), TdxFlowChartArrowShape.TConnectorKind.Left,
      procedure (AShape: TdxFlowChartArrowShape.TSegments; ASize: TSize)
      var
        APath: TdxFlowChartArrowShape.TSegment;
      begin
        APath := AShape.NewPath(True, False, False);
        APath.AddLine(ASize.cx, 0, ASize.cx, ASize.cy);
        APath.FigureFinish;
      end)
    );
end;

procedure TdxFlowChartArrowShapeRepository.CreateSharpArrow(AID: Integer; AFilled: Boolean);
begin
  AddArrow(AID,
    TdxFlowChartArrowShape.CreateSharpTipArrow(
      TSize.Create(10, 7),
      procedure (AShape: TdxFlowChartArrowShape.TSegments; ASize: TSize)
      var
        APath: TdxFlowChartArrowShape.TSegment;
        APoints: TArray<TdxPointF>;
      begin
        APath := AShape.NewPath(True, AFilled, False);
        APoints := TArray<TdxPointF>.Create(TdxPointF.Create(ASize.cx, 0), TdxPointF.Create(0.0, ASize.cy div 2),
          TdxPointF.Create(ASize.cx, ASize.cy));
        APath.AddPolyline(APoints);
        APath.FigureFinish;
      end)
    );
end;

procedure TdxFlowChartArrowShapeRepository.CreateTwoDash(AID: Integer);
begin
  AddArrow(AID,
    TdxFlowChartArrowShape.Create(
      TSize.Create(10, 7), TdxFlowChartArrowShape.TConnectorKind.Left,
      procedure (AShape: TdxFlowChartArrowShape.TSegments; ASize: TSize)
      var
        APath: TdxFlowChartArrowShape.TSegment;
      begin
        APath := AShape.NewPath(True, False, False);
        APath.AddLine(ASize.cx, 0, ASize.cx, ASize.cy);
        APath.FigureFinish;
        APath.AddLine(MulDiv(ASize.cx, 2, 3), 0, MulDiv(ASize.cx, 2, 3), ASize.cy);
        APath.FigureFinish;
      end)
    );
end;

procedure TdxFlowChartArrowShapeRepository.CreateThreeDash(AID: Integer);
begin
  AddArrow(AID,
    TdxFlowChartArrowShape.Create(
      TSize.Create(13, 7), TdxFlowChartArrowShape.TConnectorKind.Left,
      procedure (AShape: TdxFlowChartArrowShape.TSegments; ASize: TSize)
      var
        APath: TdxFlowChartArrowShape.TSegment;
      begin
        APath := AShape.NewPath(True, False, False);
        APath.AddLine(ASize.cx, 0, ASize.cx, ASize.cy);
        APath.FigureFinish;
        APath.AddLine(MulDiv(ASize.cx, 2, 4), 0, MulDiv(ASize.cx, 2, 4), ASize.cy);
        APath.FigureFinish;
        APath.AddLine(MulDiv(ASize.cx, 3, 4), 0, MulDiv(ASize.cx, 3, 4), ASize.cy);
        APath.FigureFinish;
      end)
    );
end;

procedure TdxFlowChartArrowShapeRepository.CreateDashArrows;
begin
  AddArrow(Ord(fcaDimensionLine),
    TdxFlowChartArrowShape.Create(
      TSize.Create(7, 7), TdxFlowChartArrowShape.TConnectorKind.Left,
      procedure (AShape: TdxFlowChartArrowShape.TSegments; ASize: TSize)
      var
        APath: TdxFlowChartArrowShape.TSegment;
      begin
        APath := AShape.NewPath(False, False, False);
        APath.RoundedJoin := True;
        APath.AddLine(-ASize.cx div 2, ASize.cy, ASize.cx div 2, -1);
        APath.FigureFinish;
      end,
      True,
      TdxFlowChartArrowShape.TExcludeConnectorTipKind.Half)
    );
  AddArrow(Ord(fcaBackslash),
    TdxFlowChartArrowShape.Create(
      TSize.Create(13, 7), TdxFlowChartArrowShape.TConnectorKind.Left,
      procedure (AShape: TdxFlowChartArrowShape.TSegments; ASize: TSize)
      var
        APath: TdxFlowChartArrowShape.TSegment;
      begin
        APath := AShape.NewPath(False, False, False);
        APath.RoundedJoin := True;
        APath.AddLine(ASize.cx div 2, -1, ASize.cx, ASize.cy);
        APath.FigureFinish;
      end)
    );
  CreateOneDash(Ord(fcaOpenOneDash));
  CreateTwoDash(Ord(fcaOpenTwoDash));
  CreateThreeDash(Ord(fcaOpenThreeDash));
  CreateClosedOneDash(Ord(fcaClosedOneDash), False);
  CreateClosedTwoDash(Ord(fcaClosedTwoDash), False);
  CreateClosedThreeDash(Ord(fcaClosedThreeDash), False);
  CreateClosedOneDash(Ord(fcaFilledOneDash), True);
  CreateClosedTwoDash(Ord(fcaFilledTwoDash), True);
  CreateClosedThreeDash(Ord(fcaFilledThreeDash), True);
end;

procedure TdxFlowChartArrowShapeRepository.CreateDiamond(AID: Integer; AFilled: Boolean);
begin
  AddArrow(AID,
    TdxFlowChartArrowShape.CreateSharpTipArrow(
      TSize.Create(15, 9),
      procedure (AShape: TdxFlowChartArrowShape.TSegments; ASize: TSize)
      var
        APath: TdxFlowChartArrowShape.TSegment;
        APoints: TArray<TdxPointF>;
      begin
        APath := AShape.NewPath(True, AFilled, False);
        APoints := TArray<TdxPointF>.Create(TdxPointF.Create(0.0, ASize.cy div 2), TdxPointF.Create(ASize.cx div 2, 0),
          TdxPointF.Create(ASize.cx, ASize.cy div 2), TdxPointF.Create(ASize.cx div 2, ASize.cy));
        APath.AddPolygon(APoints);
        APath.FigureFinish;
      end)
    );
end;

procedure TdxFlowChartArrowShapeRepository.CreateClosedDiamond(AID: Integer; AFilled: Boolean);
begin
  AddArrow(AID,
    TdxFlowChartArrowShape.CreateSharpTipArrow(
      TSize.Create(23, 9),
      procedure (AShape: TdxFlowChartArrowShape.TSegments; ASize: TSize)
      var
        APath: TdxFlowChartArrowShape.TSegment;
        APoints: TArray<TdxPointF>;
        AEllipseHeight: Integer;
        R: TRect;
      begin
        APath := AShape.NewPath(True, AFilled, False);
        AEllipseHeight := MulDiv(ASize.cy, 3, 4);
        R := TRect.CreateSize(TSize.Create(MulDiv(ASize.cx, 3, 11), AEllipseHeight));
        R.Offset(0, (ASize.cy - AEllipseHeight) div 2);
        APath.AddEllipse(R);
        APath.FigureFinish;
        APath := AShape.NewPath(True, False, True);
        APoints := TArray<TdxPointF>.Create(
          TdxPointF.Create(ASize.cx / 10 * 3 + 1, ASize.cy div 2),
          TdxPointF.Create(ASize.cx / 10 * 6 + 1, 0),
          TdxPointF.Create(ASize.cx, ASize.cy div 2),
          TdxPointF.Create(ASize.cx / 10 * 6 + 1, ASize.cy));
        APath.AddPolygon(APoints);
        APath.FigureFinish;
      end)
    );
end;

procedure TdxFlowChartArrowShapeRepository.CreateDiamonds;
begin
  CreateDiamond(Ord(fcaDiamond), False);
  CreateDiamond(Ord(fcaFilledDiamond), True);
  CreateClosedDiamond(Ord(fcaClosedDiamond), False);
  CreateClosedDiamond(Ord(fcaFilledClosedDiamond), True);
end;

procedure TdxFlowChartArrowShapeRepository.CreateDoubleArrows;
begin
  AddArrow(Ord(fcaClosedDoubleArrow),
    TdxFlowChartArrowShape.CreateSharpTipArrow(
      TSize.Create(15, 6),
      procedure (AShape: TdxFlowChartArrowShape.TSegments; ASize: TSize)
      var
        APath: TdxFlowChartArrowShape.TSegment;
        APoints: TArray<TdxPointF>;
      begin
        APath := AShape.NewPath(True, False, False);
        APoints := TArray<TdxPointF>.Create(TdxPointF.Create(0.0, ASize.cy div 2), TdxPointF.Create(ASize.cx div 2, -1), TdxPointF.Create(ASize.cx div 2, ASize.cy));
        APath.AddPolygon(APoints);
        APath.FigureFinish;
        APath := AShape.NewPath(True, False, False);
        APoints := TArray<TdxPointF>.Create(TdxPointF.Create(ASize.cx div 2, ASize.cy div 2), TdxPointF.Create(ASize.cx, -1), TdxPointF.Create(ASize.cx, ASize.cy));
        APath.AddPolygon(APoints);
        APath.FigureFinish;
      end)
    );
  AddArrow(Ord(fcaFilledDoubleArrow),
    TdxFlowChartArrowShape.CreateSharpTipArrow(
      TSize.Create(15, 6),
      procedure (AShape: TdxFlowChartArrowShape.TSegments; ASize: TSize)
      var
        APath: TdxFlowChartArrowShape.TSegment;
        APoints: TArray<TdxPointF>;
      begin
        APath := AShape.NewPath(True, True, False);
        APoints := TArray<TdxPointF>.Create(TdxPointF.Create(0.0, ASize.cy div 2), TdxPointF.Create(ASize.cx div 2, -1), TdxPointF.Create(ASize.cx div 2, ASize.cy));
        APath.AddPolygon(APoints);
        APath.FigureFinish;
        APath := AShape.NewPath(True, True, False);
        APoints := TArray<TdxPointF>.Create(TdxPointF.Create(ASize.cx div 2, ASize.cy div 2), TdxPointF.Create(ASize.cx, -1), TdxPointF.Create(ASize.cx, ASize.cy));
        APath.AddPolygon(APoints);
        APath.FigureFinish;
      end)
    );
end;

procedure TdxFlowChartArrowShapeRepository.CreateIndentedArrow(AID: Integer; AFilled: Boolean);
begin
  AddArrow(AID,
    TdxFlowChartArrowShape.CreateSharpTipArrow(
      TSize.Create(10, 7),
      procedure (AShape: TdxFlowChartArrowShape.TSegments; ASize: TSize)
      var
        APath: TdxFlowChartArrowShape.TSegment;
        APoints: TArray<TdxPointF>;
        W, I: Single;
      begin
        APath := AShape.NewPath(True, AFilled, False);
        APath.RoundedJoin := True;
        APoints := TArray<TdxPointF>.Create(TdxPointF.Create(ASize.cx, 0), TdxPointF.Create(0.0, ASize.cy div 2),
          TdxPointF.Create(ASize.cx, ASize.cy));
        APath.AddPolyline(APoints);
        W := ASize.cy * Sqrt(2);
        I := (W - ASize.cy) / 2;
        APath.AddArc(ASize.cx - I, -I, W, W, 145, 85);
        APath.FigureFinish;
      end)
    );
end;

procedure TdxFlowChartArrowShapeRepository.CreateOutdentedArrow(AID: Integer; AFilled: Boolean);
begin
  AddArrow(AID,
    TdxFlowChartArrowShape.CreateSharpTipArrow(
      TSize.Create(10, 7),
      procedure (AShape: TdxFlowChartArrowShape.TSegments; ASize: TSize)
      var
        APath: TdxFlowChartArrowShape.TSegment;
        APoints: TArray<TdxPointF>;
        W, I: Single;
      begin
        APath := AShape.NewPath(True, AFilled, False);
        APath.RoundedJoin := True;
        APoints := TArray<TdxPointF>.Create(TdxPointF.Create(ASize.cx, 0), TdxPointF.Create(0.0, ASize.cy div 2),
          TdxPointF.Create(ASize.cx, ASize.cy));
        APath.AddPolyline(APoints);
        W := ASize.cy * 2;
        I := (W - ASize.cy) / 2;
        APath.AddArc(ASize.cx - ASize.cy * 2 + I / 3 + 1, -I, W, W, 30, -60);
        APath.FigureFinish;
      end)
    );
end;

procedure TdxFlowChartArrowShapeRepository.Populate;
begin
  CreateLegacyArrows;
  CreateASMEArrow(Ord(fcaClosedASMEarrow), False);
  CreateASMEArrow(Ord(fcaFilledASMEarrow), True);
  CreateSharpArrow(Ord(fcaClosedArrow), False);
  CreateSharpArrow(Ord(fcaFilledArrow), True);
  CreateIndentedArrow(Ord(fcaIndentedClosedArrow), False);
  CreateIndentedArrow(Ord(fcaIndentedFilledArrow), True);
  CreateOutdentedArrow(Ord(fcaOutdentedClosedArrow), False);
  CreateOutdentedArrow(Ord(fcaOutdentedFilledArrow), True);
  CreateDoubleArrows;
  CreateDiamonds;
  CreateDashArrows;
end;

end.

