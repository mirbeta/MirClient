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

unit dxGuides;

{$I cxVer.inc}
{$SCOPEDENUMS ON}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, SysUtils, Classes, Controls, Generics.Defaults, Generics.Collections;

type
  TdxResizeMarker = (Left, TopLeft, Top, TopRight, Right, BottomRight, Bottom, BottomLeft);

  { TdxGuideController }

  TdxGuideController = class
  public type
    TAddItemsBoundsProc = reference to procedure (AController: TdxGuideController);
  {$REGION 'internal types'}
  strict private type
    TGuideKind = (Left, Right, Top, Bottom, HorizontalCenter, VerticalCenter);
    TGuideKinds = set of TGuideKind;
    TGuideValues = array[TGuideKind] of Integer;

    TGuide = class
    strict private
      FMin: Integer;
      FMax: Integer;
      FValue: Integer;
    public
      constructor Create(AValue: Integer);
      procedure Update(AMin, AMax: Integer);

      property Min: Integer read FMin;
      property Max: Integer read FMax;
      property Value: Integer read FValue;
    end;

    TGuideList = class(TObjectList<TGuide>)
    strict private class var
      FComparer: IComparer<TGuide>;
      class constructor Initialize;
    {$HINTS OFF}
      class destructor Finalize;
    {$HINTS ON}
    strict private
      FDictionary: TDictionary<Integer, TGuide>;
    public
      procedure BeginPopulate;
      procedure EndPopulate;
      function Find(AValue, ATolerance: Integer): TGuide;
      procedure Update(AValue, AMin, AMax: Integer);
    end;
  {$ENDREGION}
  strict private
    FAddItemsBoundsProc: TAddItemsBoundsProc;
    FGuides: array[TGuideKind] of TGuideList;
    FBounds: TList<TRect>;
    FOverlap: Integer;
    FTolerance: Integer;
  strict protected
    procedure CheckBoundsWidths(AOriginalX, ADelta: Integer; var X: Integer; AMarker: TdxResizeMarker; const ABounds: TRect);
    procedure CheckBoundsHeights(AOriginalY, ADelta: Integer; var Y: Integer; AMarker: TdxResizeMarker; const ABounds: TRect);
    procedure CheckGuide(const ABounds: TRect; AKind: TGuideKind; var ADelta: Integer);
    function GetBoundsValue(const ABounds: TRect; AKind: TGuideKind): Integer;
    function GetDirectionDelta(const ABounds: TRect; const AKinds: array of TGuideKind): Integer;
    function GetGuideBounds(const ABounds: TRect; ATolerance: Integer; AKind: TGuideKind;
      out AGuideBounds: TRect; var AKinds: TGuideKinds; var AValues: TGuideValues; AResize: Boolean): Boolean;
    function GetResizeTolerance(AMarker: TdxResizeMarker; AKind: TGuideKind; AExactMatch: Boolean): Integer;
    function FindNearestGuideValue(const ABounds: TRect; AKind: TGuideKind; out AValue: Integer): Boolean;
    function FindNearestSnappedX(var X: Integer; AMarker: TdxResizeMarker; const ABounds: TRect): Boolean;
    function FindNearestSnappedY(var Y: Integer; AMarker: TdxResizeMarker; const ABounds: TRect): Boolean;
    function IsHorizontal(AKind: TGuideKind): Boolean; inline;
    function IsVertical(AKind: TGuideKind): Boolean; inline;
    procedure PopulateGuides(AAddItemsBoundsProc: TAddItemsBoundsProc);
  protected
    function GetGuides(const ABounds: TRect): TArray<TRect>; overload;
    function GetGuides(const ABounds: TRect; AMarker: TdxResizeMarker): TArray<TRect>; overload;

    property Tolerance: Integer read FTolerance;
  public
    constructor Create(AAddItemsBoundsProc: TAddItemsBoundsProc; ACapacity, ATolerance, AOverlap: Integer);
    destructor Destroy; override;
    procedure AddItemBounds(const ABounds: TRect);
    procedure GetResizePoints(const ABounds: TRect; AMarker: TdxResizeMarker; var ALeftTop, ABottomRight: TPoint);
    procedure GetSnappedResizePoints(const ABounds: TRect; AMarker: TdxResizeMarker; var ASnappedLeftTop, ASnappedBottomRight: TPoint);
    procedure GetMoveDeltas(const ABounds: TRect; out ADeltaX, ADeltaY: Integer);
    procedure UpdateGuides;

    property Bounds: TList<TRect> read FBounds;
  end;

  { TdxGuidesViewData }

  TdxGuidesViewData = class
  strict private
    FCheckSizes: Boolean;
    FControl: TControl;
    FController: TdxGuideController;
    FEmpty: Boolean;
    FEqualityBounds: TRect;
    FEqualitySize: TSize;
    FGuideBounds: TArray<TRect>;
    function InitializeGuides(const ABounds: TRect; const AGuideBounds: TArray<TRect>): Boolean;
  public
    constructor Create(AControl: TControl);
    procedure Reset;
    procedure UpdateByResizing(AController: TdxGuideController; const ABounds: TRect; AMarker: TdxResizeMarker);
    procedure UpdateByMoving(AController: TdxGuideController; const ABounds: TRect);

    property CheckSizes: Boolean read FCheckSizes;
    property Controller: TdxGuideController read FController;
    property Empty: Boolean read FEmpty;
    property EqualityBounds: TRect read FEqualityBounds;
    property EqualitySize: TSize read FEqualitySize;
    property GuideBounds: TArray<TRect> read FGuideBounds;
  end;

implementation

uses
  Math, dxTypeHelpers, dxCore;

{ TdxGuideController }

{ TdxGuideController.TGuide }

constructor TdxGuideController.TGuide.Create(AValue: Integer);
begin
  FValue := AValue;
  FMin := MaxInt;
  FMax := MinInt;
end;

procedure TdxGuideController.TGuide.Update(AMin, AMax: Integer);
begin
  if AMin < FMin then
    FMin := AMin;
  if AMax > FMax then
    FMax := AMax;
end;

{ TdxGuideController.TGuideList }

procedure TdxGuideController.TGuideList.BeginPopulate;
begin
  Clear;
  FDictionary := TDictionary<Integer, TGuide>.Create(Capacity);
end;

procedure TdxGuideController.TGuideList.EndPopulate;
begin
  AddRange(FDictionary.Values);
  FreeAndNil(FDictionary);
  Sort(FComparer);
end;

class constructor TdxGuideController.TGuideList.Initialize;
begin
  FComparer := TComparer<TGuide>.Construct(
    function(const Left, Right: TGuide): Integer
    begin
      Result := Left.Value - Right.Value;
    end);
end;

class destructor TdxGuideController.TGuideList.Finalize;
begin
  FComparer := nil;
end;

function TdxGuideController.TGuideList.Find(AValue, ATolerance: Integer): TGuide;
var
  ACompare, L, H, T, ADistance, ADistance2: Integer;
begin
  Result := nil;
  if Count = 0 then
    Exit
  else
    if Count = 1 then
    begin
      if Abs(First.Value - AValue) <= ATolerance then
        Result := First;
      Exit;
    end;
  L := 0;
  H := Count - 1;
  while L <= H do
  begin
    T := L + (H - L) shr 1;
    ACompare := Items[T].Value - AValue;
    if ACompare < 0 then
      L := T + 1
    else
    begin
      H := T - 1;
      if ACompare = 0 then
        Exit(Items[T]);
    end;
  end;
  if L = Count then
  begin
    ADistance := Abs(Last.Value - AValue);
    if ADistance <= ATolerance  then
      Result := Last;
    Exit;
  end;
  if L = 0 then
  begin
    ADistance := Abs(First.Value - AValue);
    if ADistance <= ATolerance  then
      Result := First;
    Exit;
  end;
  ADistance := Abs(Items[L].Value - AValue);
  ADistance2 := Abs(Items[L - 1].Value - AValue);
  if ADistance2 < ADistance then
  begin
    if ADistance2 <= ATolerance then
      Exit(Items[L - 1])
  end
  else
    if ADistance <= ATolerance then
      Exit(Items[L]);
end;

procedure TdxGuideController.TGuideList.Update(AValue, AMin, AMax: Integer);
var
  AGuide: TGuide;
begin
  if not FDictionary.TryGetValue(AValue, AGuide) then
  begin
    AGuide := TGuide.Create(AValue);
    FDictionary.Add(AValue, AGuide);
  end;
  AGuide.Update(AMin, AMax);
end;

{ TdxGuideController }

constructor TdxGuideController.Create(AAddItemsBoundsProc: TAddItemsBoundsProc; ACapacity, ATolerance, AOverlap: Integer);
var
  I: TGuideKind;
begin
  FOverlap := AOverlap;
  FTolerance := ATolerance;
  FAddItemsBoundsProc := AAddItemsBoundsProc;
  for I := Low(TGuideKind) to High(TGuideKind) do
  begin
    FGuides[I] := TGuideList.Create;
    FGuides[I].Capacity := ACapacity;
  end;
  FBounds := TList<TRect>.Create;
  FBounds.Capacity := ACapacity;
  PopulateGuides(AAddItemsBoundsProc);
end;

destructor TdxGuideController.Destroy;
var
  I: TGuideKind;
begin
  for I := Low(TGuideKind) to High(TGuideKind) do
    FGuides[I].Free;
  FBounds.Free;
  inherited Destroy;
end;

procedure TdxGuideController.AddItemBounds(const ABounds: TRect);
begin
  FGuides[TGuideKind.Top].Update(ABounds.Top, ABounds.Left, ABounds.Right);
  FGuides[TGuideKind.VerticalCenter].Update(ABounds.CenterPoint.Y, ABounds.Left, ABounds.Right);
  FGuides[TGuideKind.Bottom].Update(ABounds.Bottom, ABounds.Left, ABounds.Right);

  FGuides[TGuideKind.Left].Update(ABounds.Left, ABounds.Top, ABounds.Bottom);
  FGuides[TGuideKind.HorizontalCenter].Update(ABounds.CenterPoint.X, ABounds.Top, ABounds.Bottom);
  FGuides[TGuideKind.Right].Update(ABounds.Right, ABounds.Top, ABounds.Bottom);
  FBounds.Add(ABounds);
end;

function TdxGuideController.GetBoundsValue(const ABounds: TRect; AKind: TGuideKind): Integer;
begin
  case AKind of
    TGuideKind.Left: Result := ABounds.Left;
    TGuideKind.HorizontalCenter: Result := ABounds.CenterPoint.X;
    TGuideKind.Right: Result := ABounds.Right;
    TGuideKind.Top: Result := ABounds.Top;
    TGuideKind.VerticalCenter: Result := ABounds.CenterPoint.Y;
  else
    Result := ABounds.Bottom;
  end;
end;

function TdxGuideController.GetDirectionDelta(const ABounds: TRect; const AKinds: array of TGuideKind): Integer;
var
  I: TGuideKind;
begin
  Result := MaxInt;
  for I in AKinds do
    CheckGuide(ABounds, I, Result);
end;

function TdxGuideController.GetGuideBounds(const ABounds: TRect; ATolerance: Integer; AKind: TGuideKind;
  out AGuideBounds: TRect; var AKinds: TGuideKinds; var AValues: TGuideValues; AResize: Boolean): Boolean;
var
  AGuide: TGuide;
begin
  AGuide := FGuides[AKind].Find(GetBoundsValue(ABounds, AKind), ATolerance);
  if AGuide <> nil then
  begin
    if AResize then
    begin
      if (AKind = TGuideKind.VerticalCenter) and
         not ([TGuideKind.Top, TGuideKind.Bottom] * AKinds = [TGuideKind.Top, TGuideKind.Bottom]) then
        Exit(False);
      if (AKind = TGuideKind.HorizontalCenter) and
         not ([TGuideKind.Left, TGuideKind.Right] * AKinds = [TGuideKind.Left, TGuideKind.Right]) then
        Exit(False);
    end;

    if AKind in [TGuideKind.Top, TGuideKind.VerticalCenter, TGuideKind.Bottom] then
      AGuideBounds.Init(
        Min(ABounds.Left, AGuide.Min) - FOverlap,
        AGuide.Value,
        Max(ABounds.Right, AGuide.Max) + FOverlap,
        AGuide.Value)
    else
      AGuideBounds.Init(
        AGuide.Value,
        Min(ABounds.Top, AGuide.Min) - FOverlap,
        AGuide.Value,
        Max(ABounds.Bottom, AGuide.Max) + FOverlap);
    AValues[AKind] := AGuide.Value;
    Include(AKinds, AKind);
    Exit(True);
  end;
  Result := False;
end;

function TdxGuideController.GetGuides(const ABounds: TRect; AMarker: TdxResizeMarker): TArray<TRect>;
var
  I: TGuideKind;
  ACount: Integer;
  AGuideBounds: TRect;
  AKinds: TGuideKinds;
  AValues: TGuideValues;
begin
  FillChar(AValues, SizeOf(AValues), 0);
  AKinds := [];
  ACount := 0;
  SetLength(Result, 6);
  for I := Low(TGuideKind) to High(TGuideKind) do
    if GetGuideBounds(ABounds, GetResizeTolerance(AMarker, I, True), I, AGuideBounds, AKinds, AValues, False) then
    begin
      Result[ACount] := AGuideBounds;
      Inc(ACount);
    end;
  SetLength(Result, ACount);
end;

function TdxGuideController.GetGuides(const ABounds: TRect): TArray<TRect>;
var
  I: TGuideKind;
  ACount: Integer;
  AGuideBounds: TRect;
  AKinds: TGuideKinds;
  AValues: TGuideValues;
begin
  FillChar(AValues, SizeOf(AValues), 0);
  AKinds := [];
  ACount := 0;
  SetLength(Result, 6);
  for I := Low(TGuideKind) to High(TGuideKind) do
    if GetGuideBounds(ABounds, 0, I, AGuideBounds, AKinds, AValues, False) then
    begin
      Result[ACount] := AGuideBounds;
      Inc(ACount);
    end;
  SetLength(Result, ACount);
end;

procedure TdxGuideController.CheckGuide(const ABounds: TRect; AKind: TGuideKind; var ADelta: Integer);
var
  AValue, ABoundValue: Integer;
begin
  ABoundValue := GetBoundsValue(ABounds, AKind);
  if FindNearestGuideValue(ABounds, AKind, AValue) and (Abs(ABoundValue - AValue) < Abs(ADelta)) then
    ADelta := ABoundValue - AValue;
end;

procedure TdxGuideController.GetMoveDeltas(const ABounds: TRect; out ADeltaX, ADeltaY: Integer);
begin
  ADeltaX := GetDirectionDelta(ABounds, [TGuideKind.Left, TGuideKind.Right, TGuideKind.HorizontalCenter]);
  ADeltaY := GetDirectionDelta(ABounds, [TGuideKind.Top, TGuideKind.Bottom, TGuideKind.VerticalCenter]);
end;

procedure TdxGuideController.UpdateGuides;
begin
  PopulateGuides(FAddItemsBoundsProc);
end;

procedure TdxGuideController.GetResizePoints(const ABounds: TRect; AMarker: TdxResizeMarker; var ALeftTop, ABottomRight: TPoint);
var
  X, Y: Integer;
begin
  X := ABounds.Left;
  if FindNearestSnappedX(X, AMarker, ABounds) then
  begin
    if Abs(ABounds.Left - X) < Tolerance then
      ALeftTop.X := X;
  end;
  Y := ABounds.Top;
  if FindNearestSnappedY(Y, AMarker, ABounds) then
  begin
    if Abs(ABounds.Top - Y) < Tolerance then
      ALeftTop.Y := Y;
  end;
  X := ABounds.Right;
  if FindNearestSnappedX(X, AMarker, ABounds) then
  begin
    if Abs(ABounds.Right - X) < Tolerance then
      ABottomRight.X := X;
  end;
  Y := ABounds.Bottom;
  if FindNearestSnappedY(Y, AMarker, ABounds) then
  begin
    if Abs(ABounds.Bottom - Y) < Tolerance then
      ABottomRight.Y := Y;
  end;
end;

procedure TdxGuideController.GetSnappedResizePoints(const ABounds: TRect; AMarker: TdxResizeMarker; var ASnappedLeftTop, ASnappedBottomRight: TPoint);
var
  X, Y: Integer;
begin
  X := ABounds.Left;
  if FindNearestSnappedX(X, AMarker, ABounds) then
  begin
    if Abs(ABounds.Left - X) < Abs(ABounds.Left - ASnappedLeftTop.X) then
      ASnappedLeftTop.X := X;
  end;
  Y := ABounds.Top;
  if FindNearestSnappedY(Y, AMarker, ABounds) then
  begin
    if Abs(ABounds.Top - Y) < Abs(ABounds.Top - ASnappedLeftTop.Y) then
      ASnappedLeftTop.Y := Y;
  end;
  X := ABounds.Right;
  if FindNearestSnappedX(X, AMarker, ABounds) then
  begin
    if Abs(ABounds.Right - X) < Abs(ABounds.Right - ASnappedBottomRight.X) then
      ASnappedBottomRight.X := X;
  end;
  Y := ABounds.Bottom;
  if FindNearestSnappedY(Y, AMarker, ABounds) then
  begin
    if Abs(ABounds.Bottom - Y) < Abs(ABounds.Bottom - ASnappedBottomRight.Y) then
      ASnappedBottomRight.Y := Y;
  end;
end;

procedure TdxGuideController.CheckBoundsHeights(AOriginalY, ADelta: Integer; var Y: Integer;
  AMarker: TdxResizeMarker; const ABounds: TRect);
var
  I, ASign, ABoundsDelta, AHeight: Integer;
begin
  case AMarker of
    TdxResizeMarker.TopLeft, TdxResizeMarker.Top, TdxResizeMarker.TopRight:
      begin
        if AOriginalY = ABounds.Bottom then
          Exit;
        ASign := -1;
      end;
    TdxResizeMarker.BottomLeft, TdxResizeMarker.Bottom, TdxResizeMarker.BottomRight:
      begin
        if AOriginalY = ABounds.Top then
          Exit;
        ASign := 1;
      end;
    else
      Exit;
  end;
  AHeight := ABounds.Height;
  for I := 0 to Bounds.Count - 1 do
  begin
    ABoundsDelta := Bounds[I].Height - AHeight;
    if Abs(ABoundsDelta) < ADelta then
    begin
      Y := AOriginalY + ABoundsDelta * ASign;
      ADelta := Abs(ABoundsDelta);
    end;
  end;
end;

procedure TdxGuideController.CheckBoundsWidths(AOriginalX, ADelta: Integer; var X: Integer; AMarker: TdxResizeMarker;
  const ABounds: TRect);
var
  I, ASign, ABoundsDelta, AWidth: Integer;
begin
  case AMarker of
    TdxResizeMarker.TopLeft, TdxResizeMarker.Left, TdxResizeMarker.BottomLeft:
      begin
        if AOriginalX = ABounds.Right then
          Exit;
        ASign := -1;
      end;
    TdxResizeMarker.TopRight, TdxResizeMarker.Right, TdxResizeMarker.BottomRight:
      begin
        if AOriginalX = ABounds.Left then
          Exit;
        ASign := 1;
      end;
    else
      Exit;
  end;
  AWidth := ABounds.Width;
  for I := 0 to Bounds.Count - 1 do
  begin
    ABoundsDelta := Bounds[I].Width - AWidth;
    if Abs(ABoundsDelta) < ADelta then
    begin
      X := AOriginalX + ABoundsDelta * ASign;
      ADelta := Abs(ABoundsDelta);
    end;
  end;
end;

function TdxGuideController.FindNearestGuideValue(const ABounds: TRect; AKind: TGuideKind; out AValue: Integer): Boolean;
var
  AGuide: TGuide;
begin
  AValue := GetBoundsValue(ABounds, AKind);
  AGuide := FGuides[AKind].Find(AValue, Tolerance);
  Result := AGuide <> nil;
  if Result then
    AValue := AGuide.Value;
end;

function TdxGuideController.FindNearestSnappedX(var X: Integer; AMarker: TdxResizeMarker; const ABounds: TRect): Boolean;
var
  AGuide: TGuide;
  ADelta, AOriginalX: Integer;
begin
  AOriginalX := X;
  case AMarker of
    TdxResizeMarker.TopLeft, TdxResizeMarker.Left, TdxResizeMarker.BottomLeft:
      AGuide := FGuides[TGuideKind.Left].Find(X, MaxInt);
    TdxResizeMarker.TopRight, TdxResizeMarker.Right, TdxResizeMarker.BottomRight:
      AGuide := FGuides[TGuideKind.Right].Find(X, MaxInt);
  else
    AGuide := nil;
  end;
  Result := AGuide <> nil;
  if Result then
  begin
    X := AGuide.Value;
    ADelta := Abs(AOriginalX - AGuide.Value);
    if ADelta <> 0 then
      CheckBoundsWidths(AOriginalX, ADelta, X, AMarker, ABounds);
  end;
end;

function TdxGuideController.FindNearestSnappedY(var Y: Integer; AMarker: TdxResizeMarker; const ABounds: TRect): Boolean;
var
  AGuide: TGuide;
  ADelta, AOriginalY: Integer;
begin
  AOriginalY := Y;
  case AMarker of
    TdxResizeMarker.TopLeft, TdxResizeMarker.Top, TdxResizeMarker.TopRight:
      AGuide := FGuides[TGuideKind.Top].Find(Y, MaxInt);
    TdxResizeMarker.BottomLeft, TdxResizeMarker.Bottom, TdxResizeMarker.BottomRight:
      AGuide := FGuides[TGuideKind.Bottom].Find(Y, MaxInt);
  else
    AGuide := nil;
  end;
  Result := AGuide <> nil;
  if Result then
  begin
    Y := AGuide.Value;
    ADelta := Abs(AOriginalY - AGuide.Value);
    if ADelta <> 0 then
      CheckBoundsHeights(AOriginalY, ADelta, Y, AMarker, ABounds);
  end;
end;

function TdxGuideController.GetResizeTolerance(AMarker: TdxResizeMarker; AKind: TGuideKind; AExactMatch: Boolean): Integer;
var
  ATolerance: Integer;
begin
  if AExactMatch then
    ATolerance := 0
  else
    ATolerance := FTolerance;
  if AKind in [TGuideKind.HorizontalCenter, TGuideKind.VerticalCenter] then
    Exit(ATolerance);
  case AMarker of
    TdxResizeMarker.Left:
      if AKind = TGuideKind.Left then
        Exit(ATolerance)
      else if IsHorizontal(AKind) then
        Exit(MinInt);
    TdxResizeMarker.Top:
      if AKind = TGuideKind.Top then
        Exit(ATolerance)
      else if IsVertical(AKind) then
        Exit(MinInt);
    TdxResizeMarker.Right:
      if AKind = TGuideKind.Right then
        Exit(ATolerance)
      else if IsHorizontal(AKind) then
        Exit(MinInt);
    TdxResizeMarker.Bottom:
      if AKind = TGuideKind.Bottom then
        Exit(ATolerance)
      else if IsVertical(AKind) then
        Exit(MinInt);
    TdxResizeMarker.TopLeft:
      if AKind in [TGuideKind.Top, TGuideKind.Left] then
        Exit(ATolerance);
    TdxResizeMarker.TopRight:
      if AKind in [TGuideKind.Top, TGuideKind.Right] then
        Exit(ATolerance);
    TdxResizeMarker.BottomRight:
      if AKind in [TGuideKind.Bottom, TGuideKind.Right] then
        Exit(ATolerance);
    TdxResizeMarker.BottomLeft:
      if AKind in [TGuideKind.Bottom, TGuideKind.Left] then
        Exit(ATolerance);
  end;
  Result := 0;
end;

function TdxGuideController.IsHorizontal(AKind: TGuideKind): Boolean;
begin
  Result := AKind in [TGuideKind.Top, TGuideKind.Bottom, TGuideKind.VerticalCenter];
end;

function TdxGuideController.IsVertical(AKind: TGuideKind): Boolean;
begin
  Result := AKind in [TGuideKind.Left, TGuideKind.Right, TGuideKind.HorizontalCenter];
end;

procedure TdxGuideController.PopulateGuides(AAddItemsBoundsProc: TAddItemsBoundsProc);
var
  I: TGuideKind;
begin
  for I := Low(TGuideKind) to High(TGuideKind) do
    FGuides[I].BeginPopulate;
  try
    FBounds.Count := 0;
    FAddItemsBoundsProc(Self);
  finally
    for I := Low(TGuideKind) to High(TGuideKind) do
      FGuides[I].EndPopulate;
  end;
end;

{ TdxGuidesViewData }

constructor TdxGuidesViewData.Create(AControl: TControl);
begin
  FControl := AControl;
  FEmpty := True;
end;

procedure TdxGuidesViewData.Reset;
begin
  if not Empty then
  begin
    FControl.Invalidate;
    FEmpty := True;
    FGuideBounds := nil;
  end;
end;

function TdxGuidesViewData.InitializeGuides(const ABounds: TRect; const AGuideBounds: TArray<TRect>): Boolean;
var
  I: Integer;
begin
  Result := Length(AGuideBounds) <> Length(FGuideBounds);
  if not Result then
    for I := Low(AGuideBounds) to High(AGuideBounds) do
      if not AGuideBounds[I].IsEqual(FGuideBounds[I]) then
      begin
        Result := True;
        break;
      end;
  FGuideBounds := AGuideBounds;
end;

procedure TdxGuidesViewData.UpdateByMoving(AController: TdxGuideController; const ABounds: TRect);
begin
  FController := AController;
  FEqualityBounds := ABounds;
  FEqualitySize.Init(MinInt, MinInt);
  FCheckSizes := False;
  FEmpty := False;
  if InitializeGuides(ABounds, AController.GetGuides(ABounds)) then
    FControl.Invalidate;
end;

procedure TdxGuidesViewData.UpdateByResizing(AController: TdxGuideController; const ABounds: TRect; AMarker: TdxResizeMarker);
begin
  FController := AController;
  FEqualitySize := ABounds.Size;
  if AMarker in [TdxResizeMarker.Left, TdxResizeMarker.Right] then
    FEqualitySize.cy := MinInt;
  if AMarker in [TdxResizeMarker.Top, TdxResizeMarker.Bottom] then
    FEqualitySize.cx := MinInt;
  FCheckSizes := True;
  FEqualityBounds := ABounds;
  FEmpty := False;
  if InitializeGuides(ABounds, AController.GetGuides(ABounds, AMarker)) then
    FControl.Invalidate;
end;

end.

