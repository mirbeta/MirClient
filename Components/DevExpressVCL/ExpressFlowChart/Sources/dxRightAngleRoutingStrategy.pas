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

unit dxRightAngleRoutingStrategy;

interface

{$I cxVer.inc}
{$SCOPEDENUMS ON}

uses
  Types, Math, SysUtils, Classes, Generics.Defaults, Generics.Collections,
  dxTypeHelpers, cxGeometry, dxCore, dxCoreClasses, dxGenerics, dxflchrt;

type

  { TdxHeuristicFunctions }

  TdxHeuristicFunctions = class
  public
    class function Manhattan(const APoint1, APoint2: TPoint): Single; static; inline;
    class function Euclidean(const APoint1, APoint2: TPoint): Single; static; inline;
  end;

  { TdxRightAngleRoutingStrategy }

  TdxRightAngleRoutingStrategy = class(TdxDiagramRoutingStrategy)
  protected type
  {$REGION 'internal classes'}
    TAStarNode = class;

    THasIntersection = function (const APoint: TPoint): Boolean of object;

    TDirection = (Left, Up, Right, Down);

    TDirectionValue = record
      Value: Single;
      Direction: TDirection;
      class function Create(const AValue: Single; ADirection: TDirection): TDirectionValue; static; inline;
    end;

    TPointData = record
      Point: TPoint;
      RouteOverlap: Boolean;
      procedure Init(AX, AY: Integer; ARouteOverlap: Boolean); overload; inline;
      procedure Init(const APoint: TPoint; ARouteOverlap: Boolean); overload; inline;
    end;

    TNeighbors = record
      Points: array[0..3] of TPointData;
      Count: Integer;
    end;

    TPathIntervals = class
    protected type
      TPointIntervals = TObjectDictionary<Integer, TList<Integer>>;
    strict private
      FHIntervals: TPointIntervals;
      FVIntervals: TPointIntervals;
    strict protected
      procedure AddInterval(const AStart, AEnd: TPoint); overload;
      procedure AddInterval(ALine, AValue1, AValue2: Integer; const AIntervals: TPointIntervals); overload;
      function HasIntersection(ALine, AValue1, AValue2: Integer; const AIntervals: TPointIntervals): Boolean; overload;
    public
      constructor Create;
      destructor Destroy; override;
      procedure AddPath(const APath: TArray<TPoint>);
      function HasIntersection(const APoint, ANext: TPoint): Boolean; overload;
    end;

    TDiagramGrid = class
    strict protected type
      TParentDirection = (Unknown, Left, Up, Right, Down);
    strict private
      FHorizontalGridLines: TArray<Integer>;
      FHorizontalGridLinesPattern: TArray<Integer>;
      FVerticalGridLines: TArray<Integer>;
      FVerticalGridLinesPattern: TArray<Integer>;
      class procedure AddAdditionalLine(ALine: Integer; var ALines: TArray<Integer>); static; inline;
      class procedure AddAdditionalLines(ALine, AOffset: Integer; var ALines: TArray<Integer>); static; inline;
      class function BinarySearch(const AValues: TArray<Integer>; AItem: Integer; out AFoundIndex: Integer): Boolean; static;
      class procedure InsertArrayItem(AItem: Integer; var AValues: TArray<Integer>; AIndex: Integer); static;
      function GetParentDirection(AParent: TAStarNode; const AEnd: TPoint): TParentDirection;
      procedure PrepareLines(AValue1, AValue2: Integer; const APattern: TArray<Integer>; var ALines: TArray<Integer>);
      procedure ProcessHorizontalDirection(AForward: Boolean; const APoint: TPoint;
        var APointData: TPointData; AIntervals: TPathIntervals; AMargin: Integer);
      procedure ProcessVerticalDirection(AForward: Boolean; const APoint: TPoint;
        var APointData: TPointData; AIntervals: TPathIntervals; AMargin: Integer);
    public
      constructor Create(const ABounds: TArray<TRect>);
      procedure Prepare(const AStart, AEnd: TPoint);
      function GetNeighbors(ANode: TAStarNode; AMargin: Integer; AIntervals: TPathIntervals): TNeighbors;

      property HorizontalGridLines: TArray<Integer> read FHorizontalGridLines;
      property VerticalGridLines: TArray<Integer> read FVerticalGridLines;
    end;

    TAStarNode = class
    strict private
      FG: Single;
      FH: Single;
      FParent: TAStarNode;
      FPosition: TPoint;
      function GetF: Single; inline;
    public
      constructor Create(const APosition: TPoint; AH: Single);
      function GetPath(const AStart, AFinish: TPoint): TArray<TPoint>;

      property F: Single read GetF;
      property G: Single read FG write FG;
      property Parent: TAStarNode read FParent write FParent;
      property Position: TPoint read FPosition;
    end;

    TOpenSet = class
    strict protected type
      TTuple = packed record
        Key: Single;
        Queue: TQueue<TAStarNode>;
      end;
    strict private class var
      FComparer: IComparer<TTuple>;
      class constructor Initialize;
    strict private
      FSortedItems: TList<TTuple>;
    public
      constructor Create;
      destructor Destroy; override;

      procedure Enqueue(AItem: TAStarNode);
      procedure Remove(AItem: TAStarNode);
      function DequeueMin: TAStarNode;
    end;

    TAStarRoutingRunner = class
    public const
      MaxStepsCount = 10000;
    strict private
      FItemMargin: Integer;
      FObjectsToDelete: TdxFastObjectList;
      FPenalty: Single;
      function CalculateG(ANode: TAStarNode; const APoint, ATarget: TPoint; const AHasIntersection: THasIntersection;
        AIntervals: TPathIntervals; ARouteOverlap: Boolean = False): Single;
      function CalculatePoint(const APoint: TPoint; const ASource, ATarget: TdxNullableRect): TPoint;
      function CalculateStartNode(AHasIntersection: THasIntersection; const AStart, ANext, ATarget: TPoint): TAStarNode;
      function CanProcess(const ABegin, AEnd: TPoint;
  const ABeginRect, AEndRect: TdxNullableRect): Boolean;
      function CreateNode(const APosition: TPoint; AH: Single): TAStarNode;
      function GetRoutingBounds(const AObjectBounds: TRect): TRect;
    public
      constructor Create(AItemMargin: Integer);
      destructor Destroy; override;
      function FindPath(const AHasIntersection: THasIntersection; AGrid: TDiagramGrid;
        AIntervals: TPathIntervals; const AStart, ATarget: TPoint;
        const ABeginRect, AEndRect: TdxNullableRect): TArray<TPoint>;

      property Penalty: Single read FPenalty write FPenalty;
    end;
  {$ENDREGION}
  strict private
    FEnd: TPoint;
    FFreePoints: TdxHashSet<TPoint>;
    FGrid: TDiagramGrid;
    FPathIntervals: TPathIntervals;
    FRoutingBounds: TArray<TRect>;
    FStart: TPoint;
  protected
    function GetRoutingBounds(AObject: TdxFcObject): TRect;
    function HasIntersection(const APoint: TPoint): Boolean;
    procedure Initialize; virtual;
  public
    constructor Create(AChart: TdxCustomFlowChart; AItemMargin: Integer); override;
    destructor Destroy; override;
    function RouteConnection(AConnection: TdxFcConnection; const AStart, AEnd: TPoint): TArray<TPoint>; override;
  end;

implementation

type

  { TRoutingHelper }

  TRoutingHelper = class
  public
    class function GetPointPosition(const AStartPoint: TPoint; const ADistanceCoef: Single; const AEndLinePoint: TPoint): TPoint; inline;
    class function IsCorner(const APrevPoint, ACurrentPoint, ANextPoint: TPoint): Boolean; static; inline;
  end;

{ TRoutingHelper }

class function TRoutingHelper.GetPointPosition(const AStartPoint: TPoint; const ADistanceCoef: Single; const AEndLinePoint: TPoint): TPoint;
begin
  Result.X := Round(AStartPoint.X + (AEndLinePoint.X - AStartPoint.X) * ADistanceCoef);
  Result.Y := Round(AStartPoint.Y + (AEndLinePoint.Y - AStartPoint.Y) * ADistanceCoef);
end;

class function TRoutingHelper.IsCorner(const APrevPoint, ACurrentPoint, ANextPoint: TPoint): Boolean;
begin
  Result := not
    (((APrevPoint.X = ACurrentPoint.X) and (ACurrentPoint.X = ANextPoint.X)) or
     ((APrevPoint.Y = ACurrentPoint.Y) and (ACurrentPoint.Y = ANextPoint.Y)));
end;

{ TdxHeuristicFunctions }

class function TdxHeuristicFunctions.Manhattan(const APoint1, APoint2: TPoint): Single;
begin
  Result := Abs(APoint1.X - APoint2.X) + Abs(APoint1.Y - APoint2.Y);
end;

class function TdxHeuristicFunctions.Euclidean(const APoint1, APoint2: TPoint): Single;
begin
  Result := APoint1.Distance(APoint2);
end;

{ TdxRightAngleRoutingStrategy.TDirectionValue }

class function TdxRightAngleRoutingStrategy.TDirectionValue.Create(const AValue: Single; ADirection: TDirection): TDirectionValue;
begin
  Result.Value := AValue;
  Result.Direction := ADirection;
end;

{ TdxRightAngleRoutingStrategy.TPointData }

procedure TdxRightAngleRoutingStrategy.TPointData.Init(AX, AY: Integer; ARouteOverlap: Boolean);
begin
  Point.X := AX;
  Point.Y := AY;
  RouteOverlap := ARouteOverlap;
end;

procedure TdxRightAngleRoutingStrategy.TPointData.Init(const APoint: TPoint; ARouteOverlap: Boolean);
begin
  Point := APoint;
  RouteOverlap := ARouteOverlap;
end;

{ TdxRightAngleRoutingStrategy.TDiagramGrid }

constructor TdxRightAngleRoutingStrategy.TDiagramGrid.Create(const ABounds: TArray<TRect>);
var
  AHorizontalLines, AVerticalLines: TdxHashSet<Integer>;
  I, ACount: Integer;
  R: TRect;
begin
  ACount := Length(ABounds);
  AHorizontalLines := TdxHashSet<Integer>.Create(ACount);
  AVerticalLines := TdxHashSet<Integer>.Create(ACount);
  try
    for I := 0 to ACount - 1 do
    begin
      R := ABounds[I];
      AHorizontalLines.Include(R.Top);
      AHorizontalLines.Include(R.Bottom);
      AVerticalLines.Include(R.Left);
      AVerticalLines.Include(R.Right);
    end;
    FHorizontalGridLinesPattern := AHorizontalLines.ToArray;
    FVerticalGridLinesPattern := AVerticalLines.ToArray;
    TArray.Sort<Integer>(FHorizontalGridLinesPattern);
    TArray.Sort<Integer>(FVerticalGridLinesPattern);
  finally
    AHorizontalLines.Free;
    AVerticalLines.Free;
  end;
end;

class procedure TdxRightAngleRoutingStrategy.TDiagramGrid.AddAdditionalLine(ALine: Integer; var ALines: TArray<Integer>);
var
  AIndex: Integer;
begin
  if not BinarySearch(ALines, ALine, AIndex) then
    InsertArrayItem(ALine, ALines, AIndex);
end;

class procedure TdxRightAngleRoutingStrategy.TDiagramGrid.AddAdditionalLines(ALine, AOffset: Integer; var ALines: TArray<Integer>);
begin
  AddAdditionalLine(ALine + AOffset, ALines);
  AddAdditionalLine(ALine - AOffset, ALines);
end;

class function TdxRightAngleRoutingStrategy.TDiagramGrid.BinarySearch(const AValues: TArray<Integer>; AItem: Integer;
  out AFoundIndex: Integer): Boolean;
var
  L, H, ATest, ACompare, ACount: Integer;
begin
  ACount := Length(AValues);
  if ACount = 0 then
  begin
    AFoundIndex := 0;
    Exit(False);
  end;

  Result := False;
  L := 0;
  H := ACount - 1;
  while L <= H do
  begin
    ATest := L + (H - L) shr 1;
    ACompare := AValues[ATest] - AItem;
    if ACompare < 0 then
      L := ATest + 1
    else
    begin
      H := ATest - 1;
      if ACompare = 0 then
        Result := True;
    end;
  end;
  AFoundIndex := L;
end;

class procedure TdxRightAngleRoutingStrategy.TDiagramGrid.InsertArrayItem(AItem: Integer; var AValues: TArray<Integer>; AIndex: Integer);
var
  L: Integer;
begin
  L := Length(AValues);
  SetLength(AValues, L + 1);
  if AIndex < L then
    System.Move(AValues[AIndex], AValues[AIndex + 1], (L - AIndex) * SizeOf(Integer));
  AValues[AIndex] := AItem;
end;

function TdxRightAngleRoutingStrategy.TDiagramGrid.GetParentDirection(AParent: TAStarNode; const AEnd: TPoint): TParentDirection;
var
  AStart: TPoint;
begin
  if AParent = nil then
    Result := TParentDirection.Unknown
  else
  begin
    AStart := AParent.Position;
    if AStart.X = AEnd.X then
      if AEnd.Y < AStart.Y  then
        Result := TParentDirection.Up
      else
        Result := TParentDirection.Down
    else
      if AEnd.X < AStart.X then
        Result := TParentDirection.Left
      else
        Result := TParentDirection.Right;
  end;
end;

function TdxRightAngleRoutingStrategy.TDiagramGrid.GetNeighbors(ANode: TAStarNode; AMargin: Integer; AIntervals: TPathIntervals): TNeighbors;
var
  APoint: TPoint;
  AParentDirection: TParentDirection;
begin
  Result.Count := 0;
  APoint := ANode.Position;
  AParentDirection := GetParentDirection(ANode.Parent, APoint);
  if AParentDirection <> TParentDirection.Right then
  begin
    ProcessHorizontalDirection(False, APoint, Result.Points[Result.Count], AIntervals, AMargin);
    Inc(Result.Count);
  end;
  if AParentDirection <> TParentDirection.Left then
  begin
    ProcessHorizontalDirection(True, APoint, Result.Points[Result.Count], AIntervals, AMargin);
    Inc(Result.Count);
  end;
  if AParentDirection <> TParentDirection.Down then
  begin
    ProcessVerticalDirection(False, APoint, Result.Points[Result.Count], AIntervals, AMargin);
    Inc(Result.Count);
  end;
  if AParentDirection <> TParentDirection.Up then
  begin
    ProcessVerticalDirection(True, APoint, Result.Points[Result.Count], AIntervals, AMargin);
    Inc(Result.Count);
  end;
end;

procedure TdxRightAngleRoutingStrategy.TDiagramGrid.Prepare(const AStart, AEnd: TPoint);
begin
  PrepareLines(AStart.X, AEnd.X, FVerticalGridLinesPattern, FVerticalGridLines);
  PrepareLines(AStart.Y, AEnd.Y, FHorizontalGridLinesPattern, FHorizontalGridLines);
end;

procedure TdxRightAngleRoutingStrategy.TDiagramGrid.PrepareLines(AValue1, AValue2: Integer; const APattern: TArray<Integer>; var ALines: TArray<Integer>);
var
  AIndex: Integer;
begin
  SetLength(ALines, Length(APattern));
  Move(Pointer(@APattern[0])^, Pointer(@ALines[0])^, SizeOf(Integer) * Length(APattern));
  if not BinarySearch(ALines, AValue1, AIndex) then
    InsertArrayItem(AValue1, ALines, AIndex);
  if not BinarySearch(ALines, AValue2, AIndex) then
    InsertArrayItem(AValue2, ALines, AIndex);
end;

procedure TdxRightAngleRoutingStrategy.TDiagramGrid.ProcessHorizontalDirection(AForward: Boolean; const APoint: TPoint;
  var APointData: TPointData; AIntervals: TPathIntervals; AMargin: Integer);
var
  AIndex, ALine: Integer;
  ANextPoint: TPoint;
  AHasIntersection: Boolean;
begin
  BinarySearch(VerticalGridLines, APoint.X, AIndex);
  if AForward then
  begin
    Inc(AIndex);
    if AIndex > High(VerticalGridLines) then
    begin
      ALine := VerticalGridLines[High(VerticalGridLines)] + AMargin;
      InsertArrayItem(ALine, FVerticalGridLines, AIndex);
      APointData.Init(ALine, APoint.Y, False);
    end
    else
    begin
      ALine := VerticalGridLines[AIndex];
      ANextPoint.Init(ALine, APoint.Y);
      AHasIntersection := AIntervals.HasIntersection(APoint, ANextPoint);
      if AHasIntersection and (ANextPoint.X - AMargin > APoint.X) then
      begin
        Dec(ANextPoint.X, AMargin);
        AHasIntersection := AIntervals.HasIntersection(APoint, ANextPoint);
        if not AHasIntersection then
          InsertArrayItem(ANextPoint.X, FVerticalGridLines, AIndex);
      end;
      APointData.Init(ANextPoint, AHasIntersection);
      if AHasIntersection then
        AddAdditionalLines(APoint.Y, AMargin, FHorizontalGridLines);
    end;
  end
  else
  begin
    Dec(AIndex);
    if AIndex < Low(VerticalGridLines) then
    begin
      ALine := VerticalGridLines[0] - AMargin;
      InsertArrayItem(ALine, FVerticalGridLines, 0);
      APointData.Init(ALine, APoint.Y, False);
    end
    else
    begin
      ALine := VerticalGridLines[AIndex];
      ANextPoint.Init(ALine, APoint.Y);
      AHasIntersection := AIntervals.HasIntersection(APoint, ANextPoint);
      if AHasIntersection and (ANextPoint.X + AMargin < APoint.X) then
      begin
        Inc(ANextPoint.X, AMargin);
        AHasIntersection := AIntervals.HasIntersection(APoint, ANextPoint);
        if not AHasIntersection then
          InsertArrayItem(ANextPoint.X, FVerticalGridLines, AIndex + 1);
      end;
      APointData.Init(ANextPoint, AHasIntersection);
      if AHasIntersection then
        AddAdditionalLines(APoint.Y, AMargin, FHorizontalGridLines);
    end;
  end;
end;

procedure TdxRightAngleRoutingStrategy.TDiagramGrid.ProcessVerticalDirection(AForward: Boolean; const APoint: TPoint;
  var APointData: TPointData; AIntervals: TPathIntervals; AMargin: Integer);
var
  AIndex, ALine: Integer;
  ANextPoint: TPoint;
  AHasIntersection: Boolean;
begin
  BinarySearch(HorizontalGridLines, APoint.Y, AIndex);
  if AForward then
  begin
    Inc(AIndex);
    if AIndex > High(HorizontalGridLines) then
    begin
      ALine := HorizontalGridLines[High(HorizontalGridLines)] + AMargin;
      InsertArrayItem(ALine, FHorizontalGridLines, AIndex);
      APointData.Init(APoint.X, ALine, False);
    end
    else
    begin
      ALine := FHorizontalGridLines[AIndex];
      ANextPoint.Init(APoint.X, ALine);
      AHasIntersection := AIntervals.HasIntersection(APoint, ANextPoint);
      if AHasIntersection and (ANextPoint.Y - AMargin > APoint.Y) then
      begin
        Dec(ANextPoint.Y, AMargin);
        AHasIntersection := AIntervals.HasIntersection(APoint, ANextPoint);
        if not AHasIntersection then
          InsertArrayItem(ANextPoint.Y, FHorizontalGridLines, AIndex);
      end;
      APointData.Init(ANextPoint, AHasIntersection);
      if AHasIntersection then
        AddAdditionalLines(APoint.X, AMargin, FVerticalGridLines);
    end;
  end
  else
  begin
    Dec(AIndex);
    if AIndex < Low(FHorizontalGridLines) then
    begin
      ALine := FHorizontalGridLines[0] - AMargin;
      InsertArrayItem(ALine, FHorizontalGridLines, 0);
      APointData.Init(APoint.X, ALine, False);
    end
    else
    begin
      ALine := FHorizontalGridLines[AIndex];
      ANextPoint.Init(APoint.X, ALine);
      AHasIntersection := AIntervals.HasIntersection(APoint, ANextPoint);
      if AHasIntersection and (ANextPoint.Y + AMargin < APoint.Y) then
      begin
        Inc(ANextPoint.Y, AMargin);
        AHasIntersection := AIntervals.HasIntersection(APoint, ANextPoint);
        if not AHasIntersection then
          InsertArrayItem(ANextPoint.Y, FHorizontalGridLines, AIndex + 1);
      end;
      APointData.Init(ANextPoint, AHasIntersection);
      if AHasIntersection then
        AddAdditionalLines(APoint.X, AMargin, FVerticalGridLines);
    end;
  end;
end;

{ TdxRightAngleRoutingStrategy.TAStarNode }

constructor TdxRightAngleRoutingStrategy.TAStarNode.Create(const APosition: TPoint; AH: Single);
begin
  FPosition := APosition;
  FH := AH;
end;

function TdxRightAngleRoutingStrategy.TAStarNode.GetF: Single;
begin
  Result := G + FH;
end;

function TdxRightAngleRoutingStrategy.TAStarNode.GetPath(const AStart, AFinish: TPoint): TArray<TPoint>;
var
  ACurrentNode: TAStarNode;
  L: TList<TPoint>;
  APrev, ACurrent, ANext: TPoint;
{$IFNDEF DELPHIXE}
  I: Integer;
{$ENDIF}
begin
  L := TList<TPoint>.Create;
  try
    L.Capacity := $400;
    ACurrentNode := Self;
    APrev := AFinish;
    repeat
      ACurrent := ACurrentNode.Position;
      ACurrentNode := ACurrentNode.Parent;
      if ACurrentNode = nil then
        ANext := AStart
      else
        ANext := ACurrentNode.Position;
      if TRoutingHelper.IsCorner(APrev, ACurrent, ANext) then
        L.Add(ACurrent);
      APrev := ACurrent;
    until ACurrentNode = nil;
    L.Reverse;
  {$IFDEF DELPHIXE}
    Result := L.ToArray;
  {$ELSE}
    SetLength(Result, L.Count);
    for I := 0 to L.Count - 1 do
      Result[I] := L[I];
  {$ENDIF}
  finally
    L.Free;
  end;
end;

{ TdxRightAngleRoutingStrategy.TAStarRoutingRunner }

constructor TdxRightAngleRoutingStrategy.TAStarRoutingRunner.Create(AItemMargin: Integer);
begin
  FItemMargin := AItemMargin;
  FObjectsToDelete := TdxFastObjectList.Create(True, $4000);
end;

destructor TdxRightAngleRoutingStrategy.TAStarRoutingRunner.Destroy;
begin
  FObjectsToDelete.Free;
  inherited Destroy;
end;

function TdxRightAngleRoutingStrategy.TAStarRoutingRunner.CreateNode(const APosition: TPoint; AH: Single): TAStarNode;
begin
  Result := TAStarNode.Create(APosition, AH);
  FObjectsToDelete.Add(Result);
end;

function TdxRightAngleRoutingStrategy.TAStarRoutingRunner.CalculateG(ANode: TAStarNode; const APoint, ATarget: TPoint;
  const AHasIntersection: THasIntersection; AIntervals: TPathIntervals; ARouteOverlap: Boolean = False): Single;
var
  G: Single;
begin
  G := TdxHeuristicFunctions.Manhattan(ANode.Position, APoint);
  if (ANode.Parent <> nil) and TRoutingHelper.IsCorner(APoint, ANode.Position, ANode.Parent.Position) then
    G := G + TdxHeuristicFunctions.Manhattan(APoint, ATarget) * 0.001;
  if AHasIntersection(TRoutingHelper.GetPointPosition(ANode.Position, 0.5, APoint)) then
    G := G + 2 * Penalty;
  if ARouteOverlap then
    G := G + Penalty * 0.01;
  Result := ANode.G + G;
end;

function TdxRightAngleRoutingStrategy.TAStarRoutingRunner.CalculatePoint(const APoint: TPoint;
  const ASource, ATarget: TdxNullableRect): TPoint;
var
  ARect: TRect;
  ADirection: TDirection;
  ADirectionValues: TArray<TDirectionValue>;
  AMinValue: Single;
  I: Integer;
begin
  if not ASource.HasValue or ASource.Value.IsEmpty then
    Exit(APoint);

  ARect := ASource;
  ARect.Inflate(FItemMargin, FItemMargin);
{$IFDEF DELPHIXE}
  ADirectionValues := TArray<TDirectionValue>.Create(
    TDirectionValue.Create(APoint.X - ARect.Left, TDirection.Left),
    TDirectionValue.Create(APoint.Y - ARect.Top, TDirection.Up),
    TDirectionValue.Create(ARect.Right - APoint.X, TDirection.Right),
    TDirectionValue.Create(ARect.Bottom - APoint.Y, TDirection.Down));
{$ELSE}
  SetLength(ADirectionValues, 4);
  ADirectionValues[0] := TDirectionValue.Create(APoint.X - ARect.Left, TDirection.Left);
  ADirectionValues[1] := TDirectionValue.Create(APoint.Y - ARect.Top, TDirection.Up);
  ADirectionValues[2] := TDirectionValue.Create(ARect.Right - APoint.X, TDirection.Right);
  ADirectionValues[3] := TDirectionValue.Create(ARect.Bottom - APoint.Y, TDirection.Down);
{$ENDIF}
  AMinValue := ADirectionValues[0].Value;
  ADirection := ADirectionValues[0].Direction;
  for I := 1 to High(ADirectionValues) do
    if ADirectionValues[I].Value < AMinValue then
    begin
      AMinValue := ADirectionValues[I].Value;
      ADirection := ADirectionValues[I].Direction;
    end;

  case ADirection of
    TDirection.Up:
      Result.Init(APoint.X, ARect.Top);
    TDirection.Down:
      Result.Init(APoint.X, ARect.Bottom);
    TDirection.Right:
      Result.Init(ARect.Right, APoint.Y);
    else
      Result.Init(ARect.Left, APoint.Y);
  end;
  if (ATarget.HasValue and ATarget.Value.Contains(Result)) and {$IFDEF DELPHIXE}(ASource <> ATarget){$ELSE}not ASource.IsEqual(ATarget){$ENDIF} then
    Exit(APoint);
end;

function TdxRightAngleRoutingStrategy.TAStarRoutingRunner.CalculateStartNode(AHasIntersection: THasIntersection;
  const AStart, ANext, ATarget: TPoint): TAStarNode;
begin
  Result := CreateNode(ANext, TdxHeuristicFunctions.Manhattan(ANext, ATarget));
  if not AStart.IsEqual(ANext) then
  begin
    Result.Parent := CreateNode(AStart, TdxHeuristicFunctions.Manhattan(AStart, ATarget));
    Result.G := CalculateG(Result.Parent, ANext, ATarget, AHasIntersection, nil);
  end;
end;

function TdxRightAngleRoutingStrategy.TAStarRoutingRunner.CanProcess(const ABegin, AEnd: TPoint;
  const ABeginRect, AEndRect: TdxNullableRect): Boolean;
var
  AStartRoutingBounds, ATargetRoutingBounds: TRect;
begin
  if ABeginRect.HasValue then
  begin
    AStartRoutingBounds := GetRoutingBounds(ABeginRect);
    if AStartRoutingBounds.Contains(AEnd) then
      Exit(False);
    if AEndRect.HasValue then
    begin
      ATargetRoutingBounds := GetRoutingBounds(AEndRect);
      if ATargetRoutingBounds.IntersectsWith(AStartRoutingBounds) then
        Exit(False);
    end;
  end
  else
    if AEndRect.HasValue then
    begin
      ATargetRoutingBounds := GetRoutingBounds(AEndRect);
      if ATargetRoutingBounds.Contains(ABegin) then
        Exit(False);
    end;
  Result := True;
end;

function TdxRightAngleRoutingStrategy.TAStarRoutingRunner.FindPath(const AHasIntersection: THasIntersection;
  AGrid: TDiagramGrid; AIntervals: TPathIntervals; const AStart, ATarget: TPoint;
  const ABeginRect, AEndRect: TdxNullableRect): TArray<TPoint>;
var
  AOpenSet1: TOpenSet;
  AOpenSet2: TDictionary<TPoint, TAStarNode>;
  AClosedSet: TDictionary<TPoint, Single>;
  ABegin, AEnd, ANeighborPoint: TPoint;
  ANeighbors: TNeighbors;
  ACurrent, AOpenPoint: TAStarNode;
  I, AStepsCount: Integer;
  G, H, AClosed: Single;
begin
  ABegin := CalculatePoint(AStart, ABeginRect, AEndRect);
  AEnd := CalculatePoint(ATarget, AEndRect, ABeginRect);
  if not CanProcess(ABegin, AEnd, ABeginRect, AEndRect) then
    Exit(nil);

  AOpenSet1 := TOpenSet.Create;
  AOpenSet2 := TDictionary<TPoint, TAStarNode>.Create($400);
  AClosedSet := TDictionary<TPoint, Single>.Create($400);
  try
    Penalty := TdxHeuristicFunctions.Manhattan(AStart, ATarget);

    AGrid.Prepare(ABegin, AEnd);

    ACurrent := CalculateStartNode(AHasIntersection, AStart, ABegin, ATarget);
    AOpenSet1.Enqueue(ACurrent);
    AOpenSet2.AddOrSetValue(ACurrent.Position, ACurrent);

    AStepsCount := 0;
    AOpenPoint := nil;
    while (AOpenSet2.Count > 0) and (AStepsCount < MaxStepsCount) do
    begin
      Inc(AStepsCount);
      ACurrent := AOpenSet1.DequeueMin;
      if AEnd.Distance(ACurrent.Position) < 0.1 then
        Exit(ACurrent.GetPath(AStart, ATarget));

      AOpenSet2.Remove(ACurrent.Position);
      AClosedSet.AddOrSetValue(ACurrent.Position, ACurrent.G);

      ANeighbors := AGrid.GetNeighbors(ACurrent, FItemMargin, AIntervals);

      for I := 0 to ANeighbors.Count - 1 do
      begin
        ANeighborPoint := ANeighbors.Points[I].Point;
        if (ACurrent.Parent <> nil) and ACurrent.Parent.Position.IsEqual(ANeighborPoint) then
          Continue;

        G := CalculateG(ACurrent, ANeighborPoint, ATarget, AHasIntersection, AIntervals, ANeighbors.Points[I].RouteOverlap);
        if AClosedSet.TryGetValue(ANeighborPoint, AClosed) then
        begin
          if AClosed <= G then
            Continue;
          AClosedSet.Remove(ANeighborPoint);
        end;

        if AOpenSet2.TryGetValue(ANeighborPoint, AOpenPoint) then
        begin
          if AOpenPoint.G > G then
          begin
            AOpenPoint.Parent := ACurrent;
            AOpenPoint.G := G;
            AOpenSet1.Enqueue(AOpenPoint);
          end;
        end
        else
        begin
          H := TdxHeuristicFunctions.Manhattan(ANeighborPoint, ATarget);
          AOpenPoint := CreateNode(ANeighborPoint, H);
          AOpenPoint.Parent := ACurrent;
          AOpenPoint.G := G;
          AOpenSet1.Enqueue(AOpenPoint);
          AOpenSet2.AddOrSetValue(AOpenPoint.Position, AOpenPoint);
        end;
      end;
    end;
    Result := nil;
  finally
    AOpenSet1.Free;
    AOpenSet2.Free;
    AClosedSet.Free;
  end;
end;

function TdxRightAngleRoutingStrategy.TAStarRoutingRunner.GetRoutingBounds(const AObjectBounds: TRect): TRect;
begin
  Result := AObjectBounds;
  Result.Inflate(FItemMargin, FItemMargin);
end;

{ TdxRightAngleRoutingStrategy.TOpenSet }

constructor TdxRightAngleRoutingStrategy.TOpenSet.Create;
begin
  FSortedItems := TList<TTuple>.Create;
  FSortedItems.Capacity := $400;
end;

destructor TdxRightAngleRoutingStrategy.TOpenSet.Destroy;
var
  I: Integer;
begin
  for I := 0 to FSortedItems.Count - 1 do
    FSortedItems[I].Queue.Free;
  FSortedItems.Free;
  inherited Destroy;
end;

function TdxRightAngleRoutingStrategy.TOpenSet.DequeueMin: TAStarNode;
var
  AMinKeyQueue: TQueue<TAStarNode>;
begin
  if FSortedItems.Count = 0 then
    Exit(nil);

  AMinKeyQueue := FSortedItems.Last.Queue;
  Result := AMinKeyQueue.Dequeue;
  if AMinKeyQueue.Count = 0 then
  begin
    AMinKeyQueue.Free;
    FSortedItems.Delete(FSortedItems.Count - 1);
  end;
end;


procedure TdxRightAngleRoutingStrategy.TOpenSet.Enqueue(AItem: TAStarNode);
var
  AKey: Single;
  AIndex: Integer;
  AQueue: TQueue<TAStarNode>;
  APair: TTuple;
begin
  AKey := AItem.F;
  APair.Key := AKey;
  if FSortedItems.BinarySearch(APair, AIndex, FComparer) then
    AQueue := FSortedItems[AIndex].Queue
  else
  begin
    AQueue := TQueue<TAStarNode>.Create;
  {$IFDEF DELPHIXE}
    AQueue.Capacity := 16;
  {$ENDIF}
    APair.Queue := AQueue;
    FSortedItems.Insert(AIndex, APair);
  end;
  AQueue.Enqueue(AItem);
end;

class constructor TdxRightAngleRoutingStrategy.TOpenSet.Initialize;
begin
  FComparer := TComparer<TTuple>.Construct(
    function (const Left, Right: TTuple): Integer
    begin
      if Right.Key < Left.Key then
        Result := -1
      else
        if Right.Key > Left.Key then
          Result := 1
        else
          Result := 0;
    end);
end;

procedure TdxRightAngleRoutingStrategy.TOpenSet.Remove(AItem: TAStarNode);
var
  AKey: Single;
  AIndex: Integer;
  AQueue, ANewQueue: TQueue<TAStarNode>;
  AQueueItem: TAStarNode;
  APair: TTuple;
begin
  AKey := AItem.F;
  APair.Key := AKey;
  if FSortedItems.BinarySearch(APair, AIndex, FComparer) then
  begin
    AQueue := FSortedItems[AIndex].Queue;
    if AQueue.Count > 1 then
    begin
      ANewQueue := TQueue<TAStarNode>.Create;
    {$IFDEF DELPHIXE}
      ANewQueue.Capacity := AQueue.Capacity;
    {$ENDIF}
      while AQueue.Count > 0 do
      begin
        AQueueItem := AQueue.Dequeue;
        if AQueueItem <> AItem then
          ANewQueue.Enqueue(AQueueItem);
      end;
      APair.Queue := ANewQueue;
      FSortedItems[AIndex] := APair;
      AQueue.Free;
    end
    else
    begin
      FSortedItems[AIndex].Queue.Free;
      FSortedItems.Delete(AIndex);
    end;
    Exit;
  end;
  Assert(False, 'Item not found');
end;

{ TdxRightAngleRoutingStrategy.TPathIntervals }

constructor TdxRightAngleRoutingStrategy.TPathIntervals.Create;
begin
  FHIntervals := TPointIntervals.Create([doOwnsValues]);
  FVIntervals := TPointIntervals.Create([doOwnsValues]);
end;

destructor TdxRightAngleRoutingStrategy.TPathIntervals.Destroy;
begin
  FHIntervals.Free;
  FVIntervals.Free;
  inherited Destroy;
end;

procedure TdxRightAngleRoutingStrategy.TPathIntervals.AddInterval(const AStart, AEnd: TPoint);
begin
  if AStart.X = AEnd.X then
    AddInterval(AStart.X, AStart.Y, AEnd.Y, FVIntervals)
  else
    AddInterval(AStart.Y, AStart.X, AEnd.X, FHIntervals);
end;

procedure TdxRightAngleRoutingStrategy.TPathIntervals.AddInterval(ALine, AValue1, AValue2: Integer; const AIntervals: TPointIntervals);
var
  L: TList<Integer>;
  ATemp: Integer;
begin
  if AValue1 > AValue2 then
  begin
    ATemp := AValue1;
    AValue1 := AValue2;
    AValue2 := ATemp;
  end;
  if not AIntervals.TryGetValue(ALine, L) then
  begin
    L := TList<Integer>.Create;
    AIntervals.Add(ALine, L);
  end;
  L.Add(AValue1);
  L.Add(AValue2);
end;

procedure TdxRightAngleRoutingStrategy.TPathIntervals.AddPath(const APath: TArray<TPoint>);
var
  AStart, AEnd: TPoint;
  I: Integer;
begin
  for I := Low(APath) to High(APath) - 1 do
  begin
    AStart := APath[I];
    AEnd := APath[I + 1];
    AddInterval(AStart, AEnd);
  end;
end;

function TdxRightAngleRoutingStrategy.TPathIntervals.HasIntersection(const APoint, ANext: TPoint): Boolean;
begin
  if APoint.Y = ANext.Y then
    Result := HasIntersection(APoint.Y, APoint.X, ANext.X, FHIntervals)
  else
    Result := HasIntersection(APoint.X, APoint.Y, ANext.Y, FVIntervals);
end;

function TdxRightAngleRoutingStrategy.TPathIntervals.HasIntersection(ALine, AValue1, AValue2: Integer;
  const AIntervals: TPointIntervals): Boolean;
var
  AList: TList<Integer>;
  I, ATemp: Integer;
begin
  if not AIntervals.TryGetValue(ALine, AList) then
    Exit(False);

  if AList.Count = 0 then
    Exit(False);

  if AValue1 > AValue2 then
  begin
    ATemp := AValue1;
    AValue1 := AValue2;
    AValue2 := ATemp;
  end;

  I := 0;
  while I < AList.Count - 1 do
  begin
    if not ((AValue1 > AList[I + 1]) or (AValue2 < AList[I])) then
      Exit(True);
    Inc(I, 2);
  end;
  Result := False;
end;

{ TdxRightAngleRoutingStrategy }

constructor TdxRightAngleRoutingStrategy.Create(AChart: TdxCustomFlowChart; AItemMargin: Integer);
begin
  inherited Create(AChart, AItemMargin);
  FFreePoints := TdxHashSet<TPoint>.Create($1000);
  Initialize;
end;

destructor TdxRightAngleRoutingStrategy.Destroy;
begin
  FGrid.Free;
  FPathIntervals.Free;
  FFreePoints.Free;
  inherited Destroy;
end;

function TdxRightAngleRoutingStrategy.HasIntersection(const APoint: TPoint): Boolean;
var
  I: Integer;
  AAddToFreePoints: Boolean;
begin
  if FFreePoints.Contains(APoint) then
    Exit(False);

  AAddToFreePoints := True;
  for I := 0 to Length(FRoutingBounds) - 1 do
  begin
    if FRoutingBounds[I].Contains(APoint) then
    begin
      AAddToFreePoints := False;
      if FRoutingBounds[I].Contains(FStart) and FRoutingBounds[I].Contains(FEnd) then
        Continue;
      Exit(True);
    end;
  end;
  if AAddToFreePoints then
    FFreePoints.Include(APoint);
  Result := False;
end;

function TdxRightAngleRoutingStrategy.GetRoutingBounds(AObject: TdxFcObject): TRect;
begin
  Result.InitSize(AObject.RealLeft, AObject.RealTop, AObject.RealWidth, AObject.RealHeight);
  Result := AObject.GetBoundsForRotatedRect(Result);
end;

procedure TdxRightAngleRoutingStrategy.Initialize;
var
  I: Integer;
  AObject: TdxFcObject;
  ABounds: TRect;
  ADiagramRects: TArray<TRect>;
begin
  SetLength(FRoutingBounds, Chart.ObjectCount);
  SetLength(ADiagramRects, Chart.ObjectCount);
  for I := 0 to Chart.ObjectCount - 1 do
  begin
    AObject := Chart.Objects[I];
    ABounds := GetRoutingBounds(AObject);
    ABounds.Inflate(ItemMargin, ItemMargin);
    ADiagramRects[I] := ABounds;
    ABounds.Inflate(-1, -1);
    FRoutingBounds[I] := ABounds;
  end;
  FGrid := TDiagramGrid.Create(ADiagramRects);
  FPathIntervals := TPathIntervals.Create;
end;

function TdxRightAngleRoutingStrategy.RouteConnection(AConnection: TdxFcConnection; const AStart, AEnd: TPoint): TArray<TPoint>;
var
  ARunner: TAStarRoutingRunner;
  AStartRect, AEndRect: TdxNullableValue<TRect>;
  AObjectSource, AObjectDest: TdxFcObject;
begin
  FStart := AStart;
  FEnd := AEnd;
  ARunner := TAStarRoutingRunner.Create(ItemMargin);
  try
    AObjectSource := AConnection.ObjectSource;
    if AObjectSource <> nil then
      AStartRect := GetRoutingBounds(AObjectSource)
    else
      AStartRect.Reset;
    AObjectDest := AConnection.ObjectDest;
    if AObjectDest <> nil then
      AEndRect := GetRoutingBounds(AObjectDest)
    else
      AEndRect.Reset;
    Result := ARunner.FindPath(HasIntersection, FGrid, FPathIntervals, AStart, AEnd, AStartRect, AEndRect);
    FPathIntervals.AddPath(Result);
  finally
    ARunner.Free;
  end;
end;

end.

