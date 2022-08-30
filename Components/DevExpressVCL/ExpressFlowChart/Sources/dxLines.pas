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

unit dxLines;

{$I cxVer.inc}

interface

uses
  Windows, Classes, Types, cxGraphics, dxCore;

type
  TLineType = (liStraight, liQSpline, liRectH, liRectV);

  TdxFCPointArray = array[0..$FFFFFF] of TPoint;
  PdxFCPointArray = ^TdxFCPointArray;

  { TdxFcLinesHelper }

  TdxFcLinesHelper = class(TObject)
  private
    class function CheckPointOutside(const P: TPoint; const R: TRect): TcxBorders;
    class function PartLengthQSpline(P: PdxFCPointArray; I, ACount: Integer; ACenter: PPoint): Integer;
    class function PartLengthRect(P: PdxFCPointArray; I, ACount: Integer; ACenter: PPoint; AHorizontal: Boolean): Integer;
    class function PartLengthStraight(P: PdxFCPointArray; I, ACount: Integer; ACenter: PPoint): Integer;
    class procedure QSplineCalcPart(var P0, P1, P2: TPoint; AIsLast: Boolean;
      var ASplinePoints: TPoints; var ASplinePointsCount: Integer);
  public
    class function IsLineInRect(P1, P2: TPoint; const R: TRect): Boolean;
    class function IsPointOnLine(ALineType: TLineType;
      APoints: PdxFCPointArray; ACount, ASigma: Integer; const P: TPoint): Boolean;
    class function IsRectOnLine(ALineType: TLineType;
      APoints: PdxFCPointArray; ACount: Integer; const R: TRect): Boolean;
    class function LineCenter(ALineType: TLineType; APoints: PdxFCPointArray; ACount: Integer): TPoint;
    class function PartDistance(const PS, PE, Src: TPoint): Integer;
    class function PartLength(ALineType: TLineType;
      APoints: PdxFCPointArray; I, ACount: Integer; ACenter: PPoint): Integer;
    class procedure QSpline(APoints: PdxFCPointArray; ACount: Integer; out ASplinePoints: TPoints);
  end;

  { TdxFcPoints }

  PdxFcPointList = ^TdxFcPointList;
  TdxFcPointList = array[0..0] of TPoint;

  TdxFcPoints = class(TObject)
  private
    FCapacity: Integer;
    FCount: Integer;
    FList: PdxFcPointList;
    function GetPoint(Index: Integer): TPoint;
    procedure SetPoint(Index: Integer; const P: TPoint);
  protected
    procedure Grow;
    //
    property Capacity: Integer read FCapacity;
  public
    destructor Destroy; override;
    procedure Add(const P: TPoint);
    procedure Assign(ASource: TdxFcPoints);
    procedure Clear;
    procedure Delete(AIndex: Integer);
    procedure Insert(AIndex: Integer; const P: TPoint);
    //
    procedure CalculatePolyline(ALineType: TLineType; out P: TPoints);
    function GetLineCenter(ALineType: TLineType): TPoint;
    function GetNearestPointIndex(const P: TPoint): Integer;
    function IsPointOnLine(ALineType: TLineType; const P: TPoint): Boolean;
    function IsRectOnLine(ALineType: TLineType; const R: TRect): Boolean;
    function IsValidIndex(Index: Integer): Boolean;
    procedure OffsetPoints(DX, DY: Integer);
    //
    property Count: Integer read FCount;
    property List: PdxFcPointList read FList;
    property Points[Index: Integer]: TPoint read GetPoint write SetPoint; default;
  end;

function Distance(const A, B: TPoint): Integer;
function LineCenter(AType: TLineType; var APoints; ACount: Integer): TPoint;
function PointIndex: Integer;
function PtOnLine(LType: TLineType; var Points; Count, Delta,X,Y: Integer): Boolean;
function QDistance(X, Y: Integer; const P: TPoint): Integer;
function RectOnLine(LType: TLineType; var Points; Count: Integer; const R: TRect): Boolean;
procedure ExtendRect(var R: TRect; const P: TPoint);
implementation

uses
  cxGeometry, Forms, Math;

var
  LastIndex: Integer;

procedure CheckLineType(var ALineType: TLineType; var ACount: Integer);
begin
  if (ALineType = liQSpline) and (ACount < 3) then
    ALineType := liStraight;
  if ALineType = liQSpline then
    Dec(ACount);
end;

{ Service public routines }

procedure ExtendRect(var R: TRect; const P: TPoint);
begin
  if P.X < R.Left then R.Left := P.X;
  if P.X > R.Right then R.Right := P.X;
  if P.Y < R.Top then R.Top := P.Y;
  if P.Y > R.Bottom then R.Bottom := P.Y;
end;

function Distance(const A, B: TPoint): Integer;
var
  DX, DY: Extended;
begin
  try
    DX := A.X - B.X;
    DY := A.Y - B.Y;
    if (DX = 0) or (DY = 0) then
      Result := Round(Abs(DX + DY))
    else
      Result := Round(Sqrt(DX * DX + DY * DY));
  except
    Result := 0;
  end;
end;

function QDistance(X, Y: Integer; const P: TPoint): Integer;
var
  DX, DY: Integer;
begin
  DX := X - P.X;
  DY := Y - P.Y;
  Result := DX * DX + DY * DY;
end;

procedure MiddlePoint(var Dst: TPoint; const Src: TPoint);
begin
  Dst.X := Src.X + (Dst.X - Src.X) div 2;
  Dst.Y := Src.Y + (Dst.Y - Src.Y) div 2;
end;

procedure CalculateRectLine(var APoints; ACount: Integer; AVertical: Boolean; out P: TPoints);
var
  ASrc: PPoint;
  J: Integer;
begin
  if ACount <= 1 then
  begin
    SetLength(P, 0);
    Exit;
  end;

  J := 0;
  ASrc := @APoints;
  SetLength(P, 2 * ACount - 1);
  while ACount > 0 do
  begin
    P[J] := ASrc^;
    Dec(ACount);
    Inc(ASrc);
    Inc(J);

    if ACount > 0 then
    begin
      P[J] := P[J - 1];
      if AVertical then
        P[J].Y := ASrc^.Y
      else
        P[J].X := ASrc^.X;
      Inc(J);
    end;
  end;
end;

function LineInRect(P1,P2: TPoint; const R: TRect): Boolean;
begin
  Result := TdxFcLinesHelper.IsLineInRect(P1, P2, R);
end;

function LineCenter(AType: TLineType; var APoints; ACount: Integer): TPoint;
begin
  Result := TdxFcLinesHelper.LineCenter(AType, @APoints, ACount);
end;

function PointIndex: Integer;
begin
  Result := LastIndex;
end;

function PtOnLine(LType: TLineType; var Points; Count, Delta,X,Y: Integer): Boolean;
begin
  Result := TdxFcLinesHelper.IsPointOnLine(LType, @Points, Count, Delta, Point(X, Y));
end;

function RectOnLine(LType: TLineType; var Points; Count: Integer; const R: TRect): Boolean;
begin
  Result := TdxFcLinesHelper.IsRectOnLine(LType, @Points, Count, R);
end;

{ TdxFcLinesHelper }

class function TdxFcLinesHelper.CheckPointOutside(const P: TPoint; const R: TRect): TcxBorders;
begin
  Result := [];
  if P.X < R.Left then
    Include(Result, bLeft);
  if P.Y < R.Top then
    Include(Result, bTop);
  if P.X > R.Right then
    Include(Result, bRight);
  if P.Y > R.Bottom then
    Include(Result, bBottom);
end;

class function TdxFcLinesHelper.LineCenter(
  ALineType: TLineType; APoints: PdxFCPointArray; ACount: Integer): TPoint;

  function PartialLength(AOldLength, AIndex: Integer; var ACenter: TPoint): Integer;
  var
    ANewLenght: Integer;
  begin
    Result := PartLength(ALineType, APoints, AIndex, ACount, nil);
    if AIndex = ACount - 2 then
      ANewLenght := 0
    else
      ANewLenght := PartialLength(AOldLength + Result, AIndex + 1, ACenter);

    if (Result + AOldLength >= ANewLenght) and (ANewLenght + Result >= AOldLength) then
    begin
      ACenter.X := (ANewLenght + Result - AOldLength) shr 1;
      ACenter.Y := Result;
      LastIndex := AIndex;
      PartLength(ALineType, APoints, AIndex, ACount, @ACenter);
    end;
    Inc(Result, ANewLenght);
  end;

begin
  Result := APoints^[0];
  CheckLineType(ALineType, ACount);
  if ACount > 1 then
    PartialLength(0, 0, Result);
end;

class function TdxFcLinesHelper.IsLineInRect(P1, P2: TPoint; const R: TRect): Boolean;

  procedure RecalculateX(var P: TPoint; AValue: Integer);
  begin
    P.X := P.X + MulDiv(P2.X - P1.X, AValue - P1.Y, P2.Y - P1.Y);
    P.Y := AValue;
  end;

  procedure RecalculateY(var P: TPoint; AValue: Integer);
  begin
    P.Y := P.Y + MulDiv(P2.Y - P1.Y, AValue - P1.X, P2.X - P1.X);
    P.X := AValue;
  end;

var
  B1, B2: TcxBorders;
begin
  B2 := CheckPointOutside(P2, R);
  repeat
    B1 := CheckPointOutside(P1, R);
    Result := B1 * B2 = [];
    if not Result then Exit;

    Result := (B1 = []) or (B2 = []) or (B1 + B2 = [bLeft, bRight]) or (B1 + B2 = [bTop, bBottom]);
    if Result then Exit;

    if bLeft   in B1 then RecalculateY(P1, R.Left)  else
    if bTop    in B1 then RecalculateX(P1, R.Top)   else
    if bRight  in B1 then RecalculateY(P1, R.Right) else
    if bBottom in B1 then RecalculateX(P1, R.Bottom)else
    Break;
  until False;
end;

class function TdxFcLinesHelper.IsPointOnLine(ALineType: TLineType;
  APoints: PdxFCPointArray; ACount, ASigma: Integer; const P: TPoint): Boolean;
var
  ASplinePointsCount: Integer;
  ASplinePoints: TPoints;
  I, ASaveLastIndex: Integer;
  P0, P1, P2: TPoint;
  R: TRect;
begin
  Result := False;
  CheckLineType(ALineType, ACount);
  for I := 0 to ACount - 2 do
  begin
    LastIndex := I;
    P0 := APoints^[I];
    P1 := APoints^[I + 1];
    R := cxRect(P1, P1);

    case ALineType of
      liRectH:
        P2 := cxPoint(P1.X, P0.Y);
      liRectV:
        P2 := cxPoint(P0.X, P1.Y);
      liQSpline:
        begin
          P2 := APoints^[I + 2];
          if I > 0 then
            MiddlePoint(P0, P1);
          if I < ACount - 2 then
            MiddlePoint(P2, P1);
          ExtendRect(R, P2);
        end;
    end;

    ExtendRect(R, P0);
    InflateRect(R, ASigma, ASigma);
    if PtInRect(R, P) then
    begin
      case ALineType of
        liRectH, liRectV:
          Result := Min(Abs(P2.X - P.X), Abs(P2.Y - P.Y)) <= ASigma;
        liStraight:
          Result := PartDistance(P0, P1, P) <= ASigma;
        liQSpline:
          begin
            ASplinePointsCount := 0;
            ASaveLastIndex := LastIndex;
            QSplineCalcPart(P0, P1, P2, False, ASplinePoints, ASplinePointsCount);
            Result := IsPointOnLine(liStraight, @ASplinePoints[0], ASplinePointsCount, ASigma, P);
            if LastIndex >= ASplinePointsCount then
              Inc(ASaveLastIndex);
            LastIndex := ASaveLastIndex;
          end;
      end;
      if Result then Exit;
    end;
  end;
  LastIndex := -1;
end;

class function TdxFcLinesHelper.IsRectOnLine(ALineType: TLineType;
  APoints: PdxFCPointArray; ACount: Integer; const R: TRect): Boolean;
var
  ASaveLastIndex: Integer;
  ASplinePoints: TPoints;
  ASplinePointsCount: Integer;
  B0, B1, B2: TcxBorders;
  I: Integer;
  P0, P1, P2: TPoint;
begin
  Result := False;
  CheckLineType(ALineType, ACount);
  for I := 0 to ACount - 2 do
  begin
    LastIndex := I;
    P0 := APoints^[I];
    P1 := APoints^[I + 1];
    case ALineType of
      liStraight:
        Result := LineInRect(P0, P1, R);

      liRectH, liRectV:
        begin
          if ALineType = liRectV then
            P2 := cxPoint(P0.X, P1.Y)
          else
            P2 := cxPoint(P1.X, P0.Y);

          Result := LineInRect(P0, P2, R) or LineInRect(P2, P1, R);
        end;

      liQSpline:
        begin
          P2 := APoints^[I + 2];
          if I > 0 then
            MiddlePoint(P0, P1);
          if I < ACount - 2 then
            MiddlePoint(P2, P1);

          B0 := CheckPointOutside(P0, R);
          B1 := CheckPointOutside(P0, R);
          B2 := CheckPointOutside(P0, R);

          Result := B0 * B1 * B2 = [];
          if not Result then Exit;

          Result := (B0 = []) or (B1 = []) or (B2 = []);
          if Result then Exit;

          ASplinePointsCount := 0;
          ASaveLastIndex := LastIndex;
          QSplineCalcPart(P0, P1, P2, False, ASplinePoints, ASplinePointsCount);
          Result := IsRectOnLine(liStraight, @ASplinePoints[0], ASplinePointsCount, R);
          if LastIndex >= ASplinePointsCount then
            Inc(ASaveLastIndex);
          LastIndex := ASaveLastIndex;
        end;
    end;
    if Result then Exit;
  end;
  LastIndex := -1;
end;

class function TdxFcLinesHelper.PartDistance(const PS,PE,Src: TPoint): Integer;
var
  DX,DY,Tmp: Integer;
begin
  DX := PE.X - PS.X;
  DY := PE.Y - PS.Y;
  if DX + DY = 0 then
  begin
    DX := PS.Y - Src.Y;
    DY := Src.X - PS.X;
    if DX + DY = 0 then
      DX := 1;
  end;

  if Abs(DX) <= Abs(DY) then
    Result := Src.X - PS.X - MulDiv(Src.Y - PS.Y, DX, DY)
  else
    Result := Src.Y - PS.Y - MulDiv(Src.X - PS.X, DY, DX);

  if Result = 0 then Exit;

  Result := Abs(Result);
  DX := Abs(DX);
  DY := Abs(DY);
  if DX < DY then
  begin
    Tmp := DX;
    DX := DY;
    DY := Tmp;
  end;
  DX := DX * 181 shr 7;
  Result := MulDiv(Result, DX, DX + DY);
end;

class function TdxFcLinesHelper.PartLength(ALineType: TLineType;
  APoints: PdxFCPointArray; I, ACount: Integer; ACenter: PPoint): Integer;
begin
  case ALineType of
    liQSpline:
      Result := PartLengthQSpline(APoints, I, ACount, ACenter);
    liStraight:
      Result := PartLengthStraight(APoints, I, ACount, ACenter);
    liRectH, liRectV:
      Result := PartLengthRect(APoints, I, ACount, ACenter, ALineType = liRectH);
    else
      Result := 0;
  end;
end;

class function TdxFcLinesHelper.PartLengthQSpline(
  P: PdxFCPointArray; I, ACount: Integer; ACenter: PPoint): Integer;

  function Limits(X, W: Extended): Extended;
  var
    S: Extended;
  begin
    S := Sqrt(X * X + W);
    if X + S <= 0 then
      Result := X * Abs(X)
    else
      Result := X * S + W * Ln(X + S);
  end;

var
  A, C, W, AX, BX, AY, BY, B2A, SqA, DT, T, L0: Extended;
  P0, P1, P2: TPoint;
begin
  P0 := P^[I];
  P1 := P^[I + 1];
  P2 := P^[I + 2];
  if I > 0 then
    MiddlePoint(P0, P1);
  if I < ACount - 2 then
    MiddlePoint(P2, P1);

  AX := P0.X + P2.X - P1.X * 2;
  BX := P1.X - P0.X;
  AY := P0.Y + P2.Y - P1.Y * 2;
  BY := P1.Y - P0.Y;

  A := Sqr(AX) + Sqr(AY);
  C := Sqr(BX) + Sqr(BY);

  if IsZero(A) then
  begin
    Result := Round(Sqrt(C * 4));
    B2A := 0;
    SqA := 0;
    L0 := 0;
    W := 0;
  end
  else
  begin
    SqA := Sqrt(A);
    B2A := (AX * BX + AY * BY) / A;
    W := C / A - Sqr(B2A);
    L0 := Limits(B2A, W);
    Result := Round(SqA * (Limits(B2A + 1, W) - L0));
  end;

  if ACenter <> nil then
  begin
    T := 0.5;
    DT := 0.25;
    if W <> 0 then
    begin
      while True do
      begin
        C := Round(SqA * (Limits(B2A + T, W) - L0)) - ACenter^.X;
        if C = 0 then
          Break;
        if C > 0 then
          T := T - DT
        else
          T := T + DT;
        DT := DT / 2;
      end;
    end;
    if T >= 0.5 then
      Inc(LastIndex);
    ACenter^.X := Round(T * (AX * T + BX * 2)) + P0.X;
    ACenter^.Y := Round(T * (AY * T + BY * 2)) + P0.Y;
  end;
end;

class function TdxFcLinesHelper.PartLengthStraight(
  P: PdxFCPointArray; I, ACount: Integer; ACenter: PPoint): Integer;
var
  X, Y: Integer;
begin
  Result := Distance(P^[I], P^[I + 1]);
  if ACenter <> nil then
  begin
    X := ACenter^.X;
    Y := ACenter^.Y;
    ACenter^.X := P^[I].X + MulDiv(P^[I + 1].X - P^[I].X, X, Y);
    ACenter^.Y := P^[I].Y + MulDiv(P^[I + 1].Y - P^[I].Y, X, Y);
  end;
end;

class function TdxFcLinesHelper.PartLengthRect(P: PdxFCPointArray;
  I, ACount: Integer; ACenter: PPoint; AHorizontal: Boolean): Integer;
var
  X, Y: Integer;
  P0, P1: TPoint;
begin
  Result := Abs(P^[I].X - P^[I + 1].X) + Abs(P^[I].Y - P^[I + 1].Y);
  if ACenter <> nil then
  begin
    X := Abs(P^[I].X - P^[I + 1].X) - ACenter^.X;
    if X >= 0 then
      Y := 0
    else
    begin
      Y := -X;
      X := 0;
    end;

    P0 := cxPoint(P^[I + 1].X, P^[I].Y);
    P1 := cxPoint(P^[I].X, P^[I + 1].Y);

    if P0.X < P1.X then X := -X;
    if P1.Y < P0.Y then Y := -Y;

    Dec(P0.X, X);
    Inc(P0.Y, Y);
    Inc(P1.X, X);
    Dec(P1.Y, Y);

    if AHorizontal then
      ACenter^ := P0
    else
      ACenter^ := P1;
  end;
end;

class procedure TdxFcLinesHelper.QSpline(
  APoints: PdxFCPointArray; ACount: Integer; out ASplinePoints: TPoints);
var
  I, AIndex: Integer;
  P0, P1, P2: TPoint;
begin
  if ACount < 3 then
  begin
    ASplinePoints := nil;
    Exit;
  end;

  AIndex := 0;
  P0 := APoints^[0];
  for I := 1 to ACount - 2 do
  begin
    P1 := APoints^[I];
    P2 := APoints^[I + 1];
    if I <> ACount - 2 then
      MiddlePoint(P2, P1);
    QSplineCalcPart(P0, P1, P2, I = ACount - 2, ASplinePoints, AIndex);
  end;
  SetLength(ASplinePoints, AIndex);
end;

class procedure TdxFcLinesHelper.QSplineCalcPart(var P0, P1, P2: TPoint;
  AIsLast: Boolean; var ASplinePoints: TPoints; var ASplinePointsCount: Integer);

  function AbsMax(const P1, P2: TPoint): Integer;
  begin
    Result := Max(Abs(P1.X - P2.X), Abs(P1.Y - P2.Y));
  end;

  function Initialize(var P0, P1, P2, DPx2, Px2: TPoint; out AStep: Integer): Boolean;
  var
    ALength, D: Integer;
  begin
    ALength := AbsMax(P0, P1) + AbsMax(P1, P2);
    Result := ALength > 0;
    if Result then
    begin
      D := 8;
      while D >= ALength do
        ALength := ALength * 2;

      AStep := MakeLong(0, D) div ALength;

      DPx2.X := MulDiv(P0.X + P2.X - 2 * P1.X, AStep * AStep, 32768);
      DPx2.Y := MulDiv(P0.Y + P2.Y - 2 * P1.Y, AStep * AStep, 32768);

      Px2.X := DPx2.X div 2 + (P1.X - P0.X) * 2 * AStep;
      Px2.Y := DPx2.Y div 2 + (P1.Y - P0.Y) * 2 * AStep;

      P1 := P0;
    end;
  end;

  procedure OutputPoint(const P: TPoint; var ASplinePointsCount: Integer);
  var
    ALength: Integer;
  begin
    ALength := Length(ASplinePoints);
    if ALength <= ASplinePointsCount then
      SetLength(ASplinePoints, ALength + Max(4, ALength div 4));
    ASplinePoints[ASplinePointsCount] := P;
    Inc(ASplinePointsCount);
  end;

var
  DPx2, Px2, Qx: TPoint;
  AStep, ALimit: Integer;
begin
  if Initialize(P0, P1, P2, DPx2, Px2, AStep) then
  begin
    Qx.X := 0;
    Qx.Y := 0;
    ALimit := MAXWORD;
    while ALimit > 0 do
    begin
      OutputPoint(P0, ASplinePointsCount);

      Qx.X := Qx.X + Px2.X;
      Qx.Y := Qx.Y + Px2.Y;

      P0.X := Qx.X div 65536 + P1.X;
      P0.Y := Qx.Y div 65536 + P1.Y;

      Px2.X := Px2.X + DPx2.X;
      Px2.Y := Px2.Y + DPx2.Y;

      Dec(ALimit, AStep);
    end;
    P0 := P2;
    if AIsLast then
      OutputPoint(P0, ASplinePointsCount);
  end;
end;

{ TdxFcPoints }

destructor TdxFcPoints.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TdxFcPoints.Add(const P: TPoint);
begin
  Insert(Count, P);
end;

procedure TdxFcPoints.Assign(ASource: TdxFcPoints);
var
  I: Integer;
begin
  Clear;
  if ASource = nil then
    Exit;
  for I := 0 to ASource.Count - 1 do
    Add(ASource.List[I]);
end;

procedure TdxFcPoints.CalculatePolyline(ALineType: TLineType; out P: TPoints);
begin
  case ALineType of
    liRectV, liRectH:
      CalculateRectLine(FList^, Count, ALineType = liRectV, P);
    liQSpline:
      TdxFcLinesHelper.QSpline(@FList^[0], Count, P);
    else
      begin
        SetLength(P, Count);
        Move(FList^, P[0], Count * SizeOf(TPoint));
      end;
  end;
end;

procedure TdxFcPoints.Clear;
begin
  if FList <> nil then
  begin
    FreeMem(FList);
    FList := nil;
  end;
  FCapacity := 0;
  FCount := 0;
end;

procedure TdxFcPoints.Delete(AIndex: Integer);
begin
  if IsValidIndex(AIndex) then
  begin
    Dec(FCount);
    if AIndex < Count then
      System.Move(FList^[AIndex + 1], FList^[AIndex], (Count - AIndex) * SizeOf(TPoint));
  end;
end;

procedure TdxFcPoints.Grow;
var
  ADelta: Integer;
begin
  if Capacity > 64 then
    ADelta := Capacity div 4
  else
    if Capacity > 8 then
      ADelta := 16
    else
      ADelta := 4;

  Inc(FCapacity, ADelta);
  ReallocMem(FList, Capacity * SizeOf(TPoint));
end;

procedure TdxFcPoints.Insert(AIndex: Integer; const P: TPoint);
begin
  if (AIndex >= 0) and (AIndex <= Count) then
  begin
    if Count = Capacity then
      Grow;
    if AIndex < Count then
      Move(FList^[AIndex], FList^[AIndex + 1], (Count - AIndex) * SizeOf(TPoint));
    FList^[AIndex] := P;
    Inc(FCount);
  end;
end;

function TdxFcPoints.IsPointOnLine(ALineType: TLineType; const P: TPoint): Boolean;
begin
  Result := PtOnLine(ALineType, FList^, Count, Screen.Width shr 8, P.X, P.Y);
end;

function TdxFcPoints.IsRectOnLine(ALineType: TLineType; const R: TRect): Boolean;
begin
  Result := RectOnLine(ALineType, FList^, Count, R);
end;

function TdxFcPoints.IsValidIndex(Index: Integer): Boolean;
begin
  Result := (Index >= 0) and (Index < Count);
end;

procedure TdxFcPoints.OffsetPoints(DX, DY: Integer);
var
  I: Integer;
begin
  for I := 0 to FCount - 1 do
  begin
    Inc(FList^[I].X, DX);
    Inc(FList^[I].Y, DY);
  end;
end;

function TdxFcPoints.GetLineCenter(ALineType: TLineType): TPoint;
begin
  Result := LineCenter(ALineType, FList^, Count);
end;

function TdxFcPoints.GetNearestPointIndex(const P: TPoint): Integer;
var
  ACurrentDistance: Integer;
  AMinimalDistance: Integer;
  I: Integer;
begin
  Result := 0;
  AMinimalDistance := MaxInt;
  for I := 0 to Count - 1 do
  begin
    ACurrentDistance := QDistance(P.X, P.Y, Points[I]);
    if ACurrentDistance < AMinimalDistance then
    begin
      AMinimalDistance := ACurrentDistance;
      Result := I;
    end;
  end;
end;

function TdxFcPoints.GetPoint(Index: Integer): TPoint;
begin
  if IsValidIndex(Index) then
    Result := FList^[Index]
  else
    Result := cxNullPoint;
end;

procedure TdxFcPoints.SetPoint(Index: Integer; const P: TPoint);
begin
  if IsValidIndex(Index) then
    FList^[Index] := P;
end;

end.
