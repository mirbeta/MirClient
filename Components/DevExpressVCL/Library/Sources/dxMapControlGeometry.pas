{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressMapControl                                        }
{                                                                    }
{           Copyright (c) 2013-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSMAPCONTROL AND ALL             }
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

unit dxMapControlGeometry;

interface

{$I cxVer.inc}

uses
  dxCore, dxCoreClasses, cxGeometry, Math, Windows, Types;

type
  TdxDoublePoints = array of TdxPointDouble;
  TdxDoublePointsArray = array of TdxDoublePoints;
  TdxPointsArray = array of TPoints;

  TdxEdge = record
    StartPoint, EndPoint: TdxPointDouble;
  end;

  TdxVertex = class(TcxDoublyLinkedObject)
  private
    FAlpha: Double;
    FNeighbour: TdxVertex;
    FNextOriginal: TdxVertex;
    FPoint: TdxPointDouble;
    FIntersect: Boolean;
    FIsProcessed: Boolean;
    FIsEnter: Boolean;
    function GetNext: TdxVertex;
    function GetPrev: TdxVertex;
  protected
    property NextOriginal: TdxVertex read FNextOriginal write FNextOriginal;
  public
    constructor Create;
    property Point: TdxPointDouble read FPoint write FPoint;
    property Next: TdxVertex read GetNext;
    property Prev: TdxVertex read GetPrev;
    property Intersect: Boolean read FIntersect write FIntersect;
    property IsEnter: Boolean read FIsEnter write FIsEnter;
    property IsProcessed: Boolean read FIsProcessed write FIsProcessed;
    property Neighbour: TdxVertex read FNeighbour write FNeighbour;
    property Alpha: Double read FAlpha write FAlpha;
  end;

  TdxVertices = class(TcxDoublyLinkedObjectList)
  private
    function GetFirst: TdxVertex;
    function GetLast: TdxVertex;
  protected
    function CreateLinkedObject: TcxDoublyLinkedObject; override;
  public
    function AddVertex: TdxVertex;
    property First: TdxVertex read GetFirst;
    property Last: TdxVertex read GetLast;
  end;

function dxEdge(const AStartPoint, AEndPoint: TdxPointDouble): TdxEdge; inline;
function dxEdgeIsIntersected(const AEdge1, AEdge2: TdxEdge; out AAlpha1, AAlpha2: Double; out APoint: TdxPointDouble): Boolean; inline;
function dxIsPtInPolygon(const APoint: TdxPointDouble; const APolygon: TdxDoublePoints): Boolean; inline;
procedure dxClipPolygon(const ASubjectPolygon, AClipPolygon: TdxDoublePoints; out APolygons: TdxDoublePointsArray);
procedure dxClipPolylineByRect(const ASubjectPolyline: TPoints; const AClipRect: TRect; var APolylines: TdxPointsArray);

implementation

function CreateVertices(const APolygon: TdxDoublePoints): TdxVertices;
var
  I: Integer;
begin
  Result := TdxVertices.Create;
  for I := 0 to High(APolygon) do
    Result.AddVertex.Point := APolygon[I];
end;

function dxEdge(const AStartPoint, AEndPoint: TdxPointDouble): TdxEdge; inline;
begin
  Result.StartPoint := AStartPoint;
  Result.EndPoint := AEndPoint;
end;

function dxEdgeIsIntersected(const AEdge1, AEdge2: TdxEdge; out AAlpha1, AAlpha2: Double; out APoint: TdxPointDouble): Boolean; inline;
var
  A: Double;
begin
  A := (AEdge2.EndPoint.Y - AEdge2.StartPoint.Y) * (AEdge1.EndPoint.X - AEdge1.StartPoint.X) -
    (AEdge2.EndPoint.X - AEdge2.StartPoint.X) * (AEdge1.EndPoint.Y - AEdge1.StartPoint.Y);
  Result := A <> 0;
  if Result then
  begin
    AAlpha1 := ((AEdge2.EndPoint.X - AEdge2.StartPoint.X) * (AEdge1.StartPoint.y - AEdge2.StartPoint.y) -
      (AEdge2.EndPoint.y - AEdge2.StartPoint.y) * (AEdge1.StartPoint.x - AEdge2.StartPoint.x)) / A;
    AAlpha2 := ((AEdge1.EndPoint.x - AEdge1.StartPoint.x) * (AEdge1.StartPoint.y - AEdge2.StartPoint.y) -
      (AEdge1.EndPoint.y - AEdge1.StartPoint.y) * (AEdge1.StartPoint.x - AEdge2.StartPoint.x)) / A;
//    Result := not (((AAlpha1 = 0) or (AAlpha1 = 1)) and (0 <= AAlpha2) and (AAlpha2 <= 1) or
//       ((AAlpha2 = 0) or (AAlpha2 = 1)) and (0 <= AAlpha1) and (AAlpha1 <= 1)); //# degenerate case
    Result := (0 <= AAlpha1) and (AAlpha1 <= 1) and (0 <= AAlpha2) and (AAlpha2 <= 1);
    if Result then
    begin
      APoint.X := AEdge1.StartPoint.X + AAlpha1 * (AEdge1.EndPoint.X - AEdge1.StartPoint.X);
      APoint.Y := AEdge1.StartPoint.Y + AAlpha1 * (AEdge1.EndPoint.Y - AEdge1.StartPoint.Y);
    end;
  end;
end;

procedure GetBoundingBox(const APolygon: TdxDoublePoints; out ALeft, ATop, ARight, ABottom: Double); inline;
var
  I: Integer;
begin
  ALeft := APolygon[0].X;
  ARight := ALeft;
  ATop := APolygon[0].Y;
  ABottom := ATop;
  for I := 1 to High(APolygon) do
  begin
    ALeft := Min(ALeft, APolygon[I].X);
    ARight := Max(ARight, APolygon[I].X);
    ATop := Min(ATop, APolygon[I].Y);
    ABottom := Max(ABottom, APolygon[I].Y);
  end;
end;

function dxIsPtInPolygon(const APoint: TdxPointDouble; const APolygon: TdxDoublePoints): Boolean; inline;
var
  AIntersectCount: Integer;
  AEdge1, AEdge2: TdxEdge;
  I: Integer;
  AAlpha1, AAlpha2: Double;
  APoint1: TdxPointDouble;
  ALeft, ATop, ARight, ABottom: Double;
begin
  Result := False;
  if Length(APolygon) > 2 then
  begin
    GetBoundingBox(APolygon, ALeft, ATop, ARight, ABottom);
    if (APoint.X >= ALeft) and (APoint.X <= ARight) and
      (APoint.Y >= ATop) and (APoint.Y <= ABottom) then
    begin
      AIntersectCount := 0;
      AEdge1.StartPoint := APoint;
      AEdge1.EndPoint.X := Max(APoint.X, ARight) + 1;
      AEdge1.EndPoint.Y := APoint.Y;
      for I := 0 to High(APolygon) - 1 do
      begin
        AEdge2.StartPoint := APolygon[I];
        AEdge2.EndPoint := APolygon[I + 1];
        if (AEdge2.StartPoint.Y = AEdge2.EndPoint.Y) and
          (AEdge1.StartPoint.Y = AEdge2.StartPoint.Y) and
          (AEdge1.StartPoint.X <= Max(AEdge2.StartPoint.X, AEdge2.EndPoint.X)) and
          (AEdge1.StartPoint.X >= Min(AEdge2.StartPoint.X, AEdge2.EndPoint.X)) then
        begin
          Result := True;
          Exit;
        end;
        if dxEdgeIsIntersected(AEdge1, AEdge2, AAlpha1, AAlpha2, APoint1) then
        begin
         if SameValue(AAlpha1, 0) then
         begin
           Result := True;
           Exit;
         end
         else
           if not SameValue(AAlpha2, 0) and not SameValue(AAlpha2, 1) then
             Inc(AIntersectCount)
           else
           begin
             if SameValue(AAlpha2, 0) then
             begin
               APolygon[I].Y := APolygon[I].Y + 0.1;
               if I = 0 then
                 APolygon[High(APolygon)].Y := APolygon[I].Y;
             end
             else
               APolygon[I + 1].Y := APolygon[I + 1].Y + 0.1;
             AEdge2.StartPoint := APolygon[I];
             AEdge2.EndPoint := APolygon[I + 1];
             if dxEdgeIsIntersected(AEdge1, AEdge2, AAlpha1, AAlpha2, APoint1) then
               Inc(AIntersectCount);
           end;
        end;
      end;
      Result := Odd(AIntersectCount);
    end;
  end;
end;

function GetUnprocessedIntersectVertex(AVertices: TdxVertices; out AVertex: TdxVertex): Boolean; inline;
begin
  AVertex := TdxVertex(AVertices.First);
  repeat
    Result := AVertex.Intersect and not AVertex.IsProcessed;
    if Result then
      Break;
    AVertex := AVertex.Next;
  until AVertex = nil;
end;

function OffsetDegeneratePoint(const APoint: TdxPointDouble; AEdge: TdxEdge; ADelta: Double = 0.1): TdxPointDouble; inline;
var
  AStartPoint: TdxPointDouble;
  ADeltaX, ADeltaY: Double;
  X1, Y1: Double;
  A: Double;
begin
  if not dxPointDoubleIsEqual(AEdge.StartPoint, APoint) then
    AStartPoint := AEdge.StartPoint
  else
    AStartPoint := AEdge.EndPoint;

  ADeltaX := APoint.X - AStartPoint.X;
  ADeltaY := APoint.Y - AStartPoint.Y;
  if ADeltaX = 0 then
  begin
    Result.X := APoint.X - ADelta;
    Result.Y := APoint.Y;
  end
  else
    if ADeltaY = 0 then
    begin
      Result.X := APoint.X;
      Result.Y := APoint.Y - ADelta;
    end
    else
    begin
      A := ADeltaY / ADeltaX;
      Y1 := ADelta / Sqrt(1 + Sqr(A));
      X1 := A * Y1;
      Result.X := APoint.X + X1;
      Result.Y := APoint.Y - Y1;
    end;
end;

function AddIntersectVertex(AVertices: TdxVertices; AVertex: TdxVertex;
  AAlpha: Double; AIntersectPoint: TdxPointDouble): TdxVertex; inline;
begin
  Result := TdxVertex.Create;
  Result.Alpha := AAlpha;
  Result.Point := AIntersectPoint;
  Result.Intersect := True;
  AVertex := AVertex.Prev;
  while Result.Alpha < AVertex.Alpha do
    AVertex := AVertex.Prev;
  AVertices.InsertAfter(AVertex, Result);
end;

procedure AddPoint(var APolygon: TdxDoublePoints; APoint: TdxPointDouble); inline;
begin
  SetLength(APolygon, Length(APolygon) + 1);
  APolygon[High(APolygon)] := APoint;
end;

function NextOriginalVertex(var AVertex: TdxVertex): Boolean; inline;
begin
  AVertex := AVertex.NextOriginal;
//  while (AVertex <> nil) and AVertex.Intersect do
//    AVertex := AVertex.Next;
  Result := AVertex <> nil;
end;

function NextIntersectVertex(var AVertex: TdxVertex): Boolean; inline;
begin
  AVertex := AVertex.Next;
  while (AVertex <> nil) and not AVertex.Intersect do
    AVertex := AVertex.Next;
  Result := AVertex <> nil;
end;

procedure ClearIntersectVertices(AVertices: TdxVertices); inline;
var
  AVertex, AIntersectVertex: TdxVertex;
begin
  AVertex := AVertices.First;
  while NextIntersectVertex(AVertex) do
  begin
    AIntersectVertex := AVertex;
    AVertex := AIntersectVertex.Prev;
    AVertices.Delete(AIntersectVertex);
  end;
end;

procedure dxClipPolygon(const ASubjectPolygon, AClipPolygon: TdxDoublePoints; out APolygons: TdxDoublePointsArray);

  function CalculateIntersections(ASVertices, ACVertices: TdxVertices): Boolean;
  var
    ASIntersectVertex, ACIntersectVertex: TdxVertex;
    AAlphaS, AAlphaC: Double;
    AIntersectPoint: TdxPointDouble;
    ASEdge, ACEdge: TdxEdge;
    ASVertex, ACVertex: TdxVertex;
    ASStartVertex, ACStartVertex: TdxVertex;
    ASEndVertex, ACEndVertex: TdxVertex;
    APoint: TdxPointDouble;
  begin
    Result := True;
    ASVertex := ASVertices.First;
    ASStartVertex := ASVertex;
    while NextOriginalVertex(ASVertex) do
    begin
      ASEndVertex := ASVertex;
      ACVertex := ACVertices.First;
      ACStartVertex := ACVertex;
      while NextOriginalVertex(ACVertex) do
      begin
        ACEndVertex := ACVertex;
        ASEdge := dxEdge(ASStartVertex.Point, ASEndVertex.Point);
        ACEdge := dxEdge(ACStartVertex.Point, ACEndVertex.Point);
        if dxEdgeIsIntersected(ASEdge, ACEdge, AAlphaS, AAlphaC, AIntersectPoint) then
          if not SameValue(AAlphaS, 0) and not SameValue(AAlphaC, 0) and
            not SameValue(AAlphaS, 1) and not SameValue(AAlphaC, 1) then
          begin
            ASIntersectVertex := AddIntersectVertex(ASVertices, ASEndVertex, AAlphaS, AIntersectPoint);
            ACIntersectVertex := AddIntersectVertex(ACVertices, ACEndVertex, AAlphaC, AIntersectPoint);
            ASIntersectVertex.Neighbour := ACIntersectVertex;
            ACIntersectVertex.Neighbour := ASIntersectVertex;
          end
          else
          begin
            Result := False;
            if SameValue(AAlphaS, 0) then
            begin
              APoint := OffsetDegeneratePoint(ASStartVertex.Point, ACEdge);
              ASStartVertex.Point := APoint;
              if ASStartVertex.Prev = nil then
                ASVertices.Last.Point := APoint;
            end
            else
            if SameValue(AAlphaS, 1) then
            begin
              APoint := OffsetDegeneratePoint(ASEndVertex.Point, ACEdge);
              ASEndVertex.Point := APoint;
              if ASEndVertex.Next = nil then
                ASVertices.First.Point := APoint;
            end
            else
            if SameValue(AAlphaC, 0) then
            begin
              APoint := OffsetDegeneratePoint(ACStartVertex.Point, ASEdge);
              ACStartVertex.Point := APoint;
              if ACStartVertex.Prev = nil then
                ACVertices.Last.Point := APoint;
            end
            else
            if SameValue(AAlphaC, 1) then
            begin
              APoint := OffsetDegeneratePoint(ACEndVertex.Point, ASEdge);
              ACEndVertex.Point := APoint;
              if ACEndVertex.Next = nil then
                ACVertices.First.Point := APoint;
            end;

            ClearIntersectVertices(ACVertices);
            ClearIntersectVertices(ASVertices);
            Exit;
          end;
        ACStartVertex := ACVertex;
      end;
      ASStartVertex := ASVertex;
    end;
  end;

  function InternalIsPtInPolygon(const APoint: TdxPointDouble; AVertices: TdxVertices): Boolean;
  var
    APolygon: TdxDoublePoints;
    AVertex: TdxVertex;
  begin
    AVertex := AVertices.First;
    repeat
      SetLength(APolygon, Length(APolygon) + 1);
      APolygon[High(APolygon)] := AVertex.Point;
    until not NextOriginalVertex(AVertex);
    Result := dxIsPtInPolygon(APoint, APolygon);
  end;

  procedure MarkEntryAndExitPoints(ASVertices, ACVertices: TdxVertices; out AIsSubjectInClip, AIsClipInSubject: Boolean);
  var
    ASVertex, ACVertex: TdxVertex;
    AIsEnter: Boolean;
  begin
    ASVertex := ASVertices.First;
    AIsEnter := not InternalIsPtInPolygon(ASVertex.Point, ACVertices);
    AIsSubjectInClip := not AIsEnter;
    while NextIntersectVertex(ASVertex) do
    begin
      ASVertex.IsEnter := AIsEnter;
      AIsEnter := not AIsEnter;
    end;

    ACVertex := ACVertices.First;
    AIsEnter := not InternalIsPtInPolygon(ACVertex.Point, ASVertices);
    AIsClipInSubject := not AIsEnter;
    while NextIntersectVertex(ACVertex) do
    begin
      ACVertex.IsEnter := AIsEnter;
      AIsEnter := not AIsEnter;
    end;
  end;

  procedure BuildPolygons(ASVertices, ACVertices: TdxVertices;
    AIsSubjectInClip, AIsClipInSubject: Boolean; out APolygons: TdxDoublePointsArray);
  var
    APolygonCount: Integer;
    AVertex: TdxVertex;
  begin
    SetLength(APolygons, 0);
    APolygonCount := 0;
    while GetUnprocessedIntersectVertex(ASVertices, AVertex) do
    begin
      Inc(APolygonCount);
      SetLength(APolygons, APolygonCount);
      AddPoint(APolygons[APolygonCount - 1], AVertex.Point);
      repeat
        AVertex.IsProcessed := True;
        if AVertex.Neighbour <> nil then
          AVertex.Neighbour.IsProcessed := True;
        if AVertex.IsEnter then
          repeat
            if AVertex.Next <> nil then
              AVertex := AVertex.Next
            else
            begin
              if AVertex = ACVertices.Last then
                AVertex := ACVertices.First
              else
                AVertex := ASVertices.First;
            end;
            AddPoint(APolygons[APolygonCount - 1], AVertex.Point);
          until AVertex.Intersect
        else
          repeat
            if AVertex.Prev <> nil then
              AVertex := AVertex.Prev
            else
            begin
              if AVertex = ACVertices.First then
                AVertex := ACVertices.Last
              else
                AVertex := ASVertices.Last;
            end;
            AddPoint(APolygons[APolygonCount - 1], AVertex.Point);
          until AVertex.Intersect;
        AVertex := AVertex.Neighbour;
      until AVertex.IsProcessed;
    end;

    if (APolygonCount = 0) and
      (AIsClipInSubject or AIsSubjectInClip) then
    begin
      SetLength(APolygons, 1);
      if AIsSubjectInClip then
        APolygons[0] := ASubjectPolygon
      else
        APolygons[0] := AClipPolygon;
    end;
  end;

var
  ASVertices, ACVertices: TdxVertices;
  AIsSubjectInClip, AIsClipInSubject: Boolean;
begin
  if (Length(ASubjectPolygon) = 0) or (Length(AClipPolygon) = 0) then
    SetLength(APolygons, 0)
  else
  begin
    ASVertices := CreateVertices(ASubjectPolygon);
    ACVertices := CreateVertices(AClipPolygon);
    try
      repeat
      until CalculateIntersections(ASVertices, ACVertices);
      MarkEntryAndExitPoints(ASVertices, ACVertices, AIsSubjectInClip, AIsClipInSubject);
      BuildPolygons(ASVertices, ACVertices, AIsSubjectInClip, AIsClipInSubject, APolygons);
    finally
      ASVertices.Free;
      ACVertices.Free;
    end;
  end;
end;

procedure dxClipPolyline(const ASubjectPolyline, AClipPolygon: TdxDoublePoints; out APolylines: TdxDoublePointsArray);

  function CalculateIntersections(ASVertices, ACVertices: TdxVertices): Boolean;
  var
    AAlphaS, AAlphaC: Double;
    AIntersectPoint: TdxPointDouble;
    ASEdge, ACEdge: TdxEdge;
    ASVertex, ACVertex: TdxVertex;
    ASStartVertex, ACStartVertex: TdxVertex;
    ASEndVertex, ACEndVertex: TdxVertex;
    APoint: TdxPointDouble;
  begin
    Result := True;
    ASVertex := ASVertices.First;
    ASStartVertex := ASVertex;
    while NextOriginalVertex(ASVertex) do
    begin
      ASEndVertex := ASVertex;
      ACVertex := ACVertices.First;
      ACStartVertex := ACVertex;
      while NextOriginalVertex(ACVertex) do
      begin
        ACEndVertex := ACVertex;
        ASEdge := dxEdge(ASStartVertex.Point, ASEndVertex.Point);
        ACEdge := dxEdge(ACStartVertex.Point, ACEndVertex.Point);
        if dxEdgeIsIntersected(ASEdge, ACEdge, AAlphaS, AAlphaC, AIntersectPoint) then
          if not SameValue(AAlphaS, 0) and not SameValue(AAlphaS, 1) then
            AddIntersectVertex(ASVertices, ASEndVertex, AAlphaS, AIntersectPoint)
          else
          begin
            Result := False;
            if SameValue(AAlphaS, 0) then
            begin
              APoint := OffsetDegeneratePoint(ASStartVertex.Point, ACEdge);
              ASStartVertex.Point := APoint;
            end
            else
            if SameValue(AAlphaS, 1) then
            begin
              APoint := OffsetDegeneratePoint(ASEndVertex.Point, ACEdge);
              ASEndVertex.Point := APoint;
            end;

            ClearIntersectVertices(ASVertices);
            Exit;
          end;
        ACStartVertex := ACVertex;
      end;
      ASStartVertex := ASVertex;
    end;
  end;

  function InternalIsPtInPolygon(const APoint: TdxPointDouble; AVertices: TdxVertices): Boolean;
  var
    APolygon: TdxDoublePoints;
    AVertex: TdxVertex;
  begin
    AVertex := AVertices.First;
    repeat
      SetLength(APolygon, Length(APolygon) + 1);
      APolygon[High(APolygon)] := AVertex.Point;
    until not NextOriginalVertex(AVertex);
    Result := dxIsPtInPolygon(APoint, APolygon);
  end;

  procedure MarkEntryAndExitPoints(ASVertices, ACVertices: TdxVertices; out AIsSubjectInClip: Boolean);
  var
    ASVertex: TdxVertex;
    AIsEnter: Boolean;
  begin
    ASVertex := ASVertices.First;
    AIsEnter := not InternalIsPtInPolygon(ASVertex.Point, ACVertices);
    AIsSubjectInClip := not AIsEnter;
    while NextIntersectVertex(ASVertex) do
    begin
      ASVertex.IsEnter := AIsEnter;
      AIsEnter := not AIsEnter;
    end;
  end;

  procedure BuildPolylines(ASVertices, ACVertices: TdxVertices;
    AIsSubjectInClip: Boolean; out APolylines: TdxDoublePointsArray);
  var
    APolylineCount: Integer;
    AVertex: TdxVertex;
  begin
    SetLength(APolylines, 0);
    APolylineCount := 0;
    while GetUnprocessedIntersectVertex(ASVertices, AVertex) do
    begin
      Inc(APolylineCount);
      SetLength(APolylines, APolylineCount);
      AddPoint(APolylines[APolylineCount - 1], AVertex.Point);
      AVertex.IsProcessed := True;
      if AVertex.IsEnter then
        repeat
          AVertex := AVertex.Next;
          AddPoint(APolylines[APolylineCount - 1], AVertex.Point);
        until AVertex.Intersect or (AVertex.Next = nil)
      else
        repeat
            AVertex := AVertex.Prev;
          AddPoint(APolylines[APolylineCount - 1], AVertex.Point);
        until AVertex.Intersect or (AVertex.Prev = nil);
      if AVertex.Intersect then
        AVertex.IsProcessed := True;
    end;

    if (APolylineCount = 0) and AIsSubjectInClip then
    begin
      SetLength(APolylines, 1);
      APolylines[0] := ASubjectPolyline;
    end;
  end;

var
  ASVertices, ACVertices: TdxVertices;
  AIsSubjectInClip: Boolean;
begin
  ASVertices := CreateVertices(ASubjectPolyline);
  ACVertices := CreateVertices(AClipPolygon);
  try
    repeat
    until CalculateIntersections(ASVertices, ACVertices);
    MarkEntryAndExitPoints(ASVertices, ACVertices, AIsSubjectInClip);
    BuildPolylines(ASVertices, ACVertices, AIsSubjectInClip, APolylines);
  finally
    ASVertices.Free;
    ACVertices.Free;
  end;
end;

procedure FindTopIntersectPoint(const AStartPoint: TPoint; const AClipRect: TRect;
  K: Double; var AIntersectPoint: TPoint); inline;
begin
  AIntersectPoint.Y := AClipRect.Top;
  AIntersectPoint.X := Round(AStartPoint.X + (AIntersectPoint.Y - AStartPoint.Y) * K);
end;

procedure FindBottomIntersectPoint(const AStartPoint: TPoint; const AClipRect: TRect;
  K: Double; var AIntersectPoint: TPoint); inline;
begin
  AIntersectPoint.Y := AClipRect.Bottom;
  AIntersectPoint.X := Round(AStartPoint.X + (AIntersectPoint.Y - AStartPoint.Y) * K);
end;

procedure FindLeftIntersectPoint(const AStartPoint: TPoint; const AClipRect: TRect;
  K: Double; var AIntersectPoint: TPoint); inline;
begin
  AIntersectPoint.X := AClipRect.Left;
  AIntersectPoint.Y := Round(AStartPoint.Y + (AIntersectPoint.X - AStartPoint.X) * K);
end;

procedure FindRightIntersectPoint(const AStartPoint: TPoint; const AClipRect: TRect;
  K: Double; var AIntersectPoint: TPoint); inline;
begin
  AIntersectPoint.X := AClipRect.Right;
  AIntersectPoint.Y := Round(AStartPoint.Y + (AIntersectPoint.X - AStartPoint.X) * K);
end;

//type
//  TdxIntersectionType = (itPointOutOfRect, itPointStrictInRect, itPointOnEdge);

function GetIntersectPointWhenStartPointStrictInRect(const AInnerPoint, AExtraPoint: TPoint;
  const AClipRect: TRect): TPoint; inline;
var
  A, B: Integer;
begin
  A := AExtraPoint.X - AInnerPoint.X;
  B := AExtraPoint.Y - AInnerPoint.Y;
  if A = 0 then
  begin
    Result.X := AExtraPoint.X;
    if AExtraPoint.Y < AClipRect.Top then
      Result.Y := AClipRect.Top
    else
      Result.Y := AClipRect.Bottom;
  end
  else
    if B = 0 then
    begin
      Result.Y := AExtraPoint.Y;
      if AExtraPoint.X < AClipRect.Left then
        Result.X := AClipRect.Left
      else
        Result.X := AClipRect.Right;
    end
    else
      if AExtraPoint.X > AClipRect.Right then
      begin
        FindRightIntersectPoint(AInnerPoint, AClipRect, B / A, Result);
        if Result.Y < AClipRect.Top then
          FindTopIntersectPoint(AInnerPoint, AClipRect, A / B, Result)
        else
          if Result.Y > AClipRect.Bottom then
            FindBottomIntersectPoint(AInnerPoint, AClipRect, A / B, Result)
      end
      else
        if AExtraPoint.X < AClipRect.Left then
        begin
          FindLeftIntersectPoint(AInnerPoint, AClipRect, B / A, Result);
          if Result.Y < AClipRect.Top then
            FindTopIntersectPoint(AInnerPoint, AClipRect, A / B, Result)
          else
            if Result.Y > AClipRect.Bottom then
              FindBottomIntersectPoint(AInnerPoint, AClipRect, A / B, Result);
        end
        else
          if AExtraPoint.Y < AClipRect.Top then
            FindTopIntersectPoint(AInnerPoint, AClipRect, A / B, Result)
          else
            FindBottomIntersectPoint(AInnerPoint, AClipRect, A / B, Result);
end;

function IsIntersectClipRect(const APoint1, APoint2: TPoint; const AClipRect: TRect;
  out AIntersectPoint1, AIntersectPoint2: TPoint): Boolean; inline;
var
  A, B: Integer;
  PL, PR: TPoint;
  BA, AB: Double;
begin
  Result := False;
  if not ((APoint1.X <= AClipRect.Left) and (APoint2.X <= AClipRect.Left) or
    (APoint1.X >= AClipRect.Right) and (APoint2.X >= AClipRect.Right) or
    (APoint1.Y <= AClipRect.Top) and (APoint2.Y <= AClipRect.Top) or
    (APoint1.Y >= AClipRect.Bottom) and (APoint2.Y >= AClipRect.Bottom)) then
  begin
    A := APoint2.X - APoint1.X;
    B := APoint2.Y - APoint1.Y;
    if A = 0 then
    begin
      AIntersectPoint1.X := APoint1.X;
      AIntersectPoint2.X := AIntersectPoint1.X;
      AIntersectPoint1.Y := AClipRect.Top;
      AIntersectPoint2.Y := AClipRect.Bottom;
      Result := True;
    end
    else
      if B = 0 then
      begin
        AIntersectPoint1.Y := APoint1.Y;
        AIntersectPoint2.Y := AIntersectPoint1.Y;
        AIntersectPoint1.X := AClipRect.Left;
        AIntersectPoint2.X := AClipRect.Right;
        Result := True;
      end
      else
      begin
        if A > 0 then
        begin
          PL := APoint1;
          PR := APoint2;
        end
        else
        begin
          PL := APoint2;
          PR := APoint1;
        end;
        BA := B / A;
        AB := A / B;
        if PL.X < AClipRect.Left then
        begin
          FindLeftIntersectPoint(PL, AClipRect, BA, AIntersectPoint1);
          if AIntersectPoint1.Y <= AClipRect.Top then
          begin
            if PL.Y < AIntersectPoint1.Y then
            begin
              FindTopIntersectPoint(PL, AClipRect, AB, AIntersectPoint1);
              Result := AIntersectPoint1.X < AClipRect.Right;
              if Result then
              begin
                FindBottomIntersectPoint(PL, AClipRect, AB, AIntersectPoint2);
                if AIntersectPoint2.X > AClipRect.Right then
                  FindRightIntersectPoint(PL, AClipRect, BA, AIntersectPoint2);
              end;
            end
            else
              Result := False;
          end
          else
            if AIntersectPoint1.Y >= AClipRect.Bottom then
            begin
              if PL.Y > AIntersectPoint1.Y then
              begin
                FindBottomIntersectPoint(PL, AClipRect, AB, AIntersectPoint1);
                Result := AIntersectPoint1.X < AClipRect.Right;
                if Result then
                begin
                  FindTopIntersectPoint(PL, AClipRect, AB, AIntersectPoint2);
                  if AIntersectPoint2.X > AClipRect.Right then
                    FindRightIntersectPoint(PL, AClipRect, BA, AIntersectPoint2);
                end;
              end
              else
                Result := False;
            end
            else
            begin
              if (PL.Y > PR.Y) then
                FindTopIntersectPoint(PL, AClipRect, AB, AIntersectPoint2)
              else
                FindBottomIntersectPoint(PL, AClipRect, AB, AIntersectPoint2);
              if AIntersectPoint2.X > AClipRect.Right then
                FindRightIntersectPoint(PL, AClipRect, BA, AIntersectPoint2);
              Result := True;
            end;
        end
        else
          if PL.Y < AClipRect.Top then
          begin
            FindTopIntersectPoint(PL, AClipRect, AB, AIntersectPoint1);
            Result := AIntersectPoint1.X < AClipRect.Right;
            if Result then
            begin
              FindBottomIntersectPoint(PL, AClipRect, AB, AIntersectPoint2);
              if AIntersectPoint2.X > AClipRect.Right then
                FindRightIntersectPoint(PL, AClipRect, BA, AIntersectPoint2);
            end;
          end
          else
          begin
            FindBottomIntersectPoint(PL, AClipRect, AB, AIntersectPoint1);
            Result := AIntersectPoint1.X < AClipRect.Right;
            if Result then
            begin
              FindTopIntersectPoint(PL, AClipRect, AB, AIntersectPoint2);
              if AIntersectPoint2.X > AClipRect.Right then
                FindRightIntersectPoint(PL, AClipRect, BA, AIntersectPoint2);
            end;
          end;
      end;
  end;
end;

function IsIntersectClipRectWhenStartPointOnEdge(const APoint1, APoint2: TPoint; const AClipRect: TRect;
  out AIntersectPoint: TPoint): Boolean; inline;
var
  A, B: Integer;
begin
  Result := False;
  if not ((APoint1.X <= AClipRect.Left) and (APoint2.X <= AClipRect.Left) or
    (APoint1.X >= AClipRect.Right) and (APoint2.X >= AClipRect.Right) or
    (APoint1.Y <= AClipRect.Top) and (APoint2.Y <= AClipRect.Top) or
    (APoint1.Y >= AClipRect.Bottom) and (APoint2.Y >= AClipRect.Bottom)) then
  begin
    Result := True;
    A := APoint2.X - APoint1.X;
    B := APoint2.Y - APoint1.Y;
    if A = 0 then
    begin
      AIntersectPoint.X := APoint1.X;
      if APoint1.Y = AClipRect.Top then
        AIntersectPoint.Y := AClipRect.Bottom
      else
        AIntersectPoint.Y := AClipRect.Top;
    end
    else
      if B = 0 then
      begin
        AIntersectPoint.Y := APoint1.Y;
        if APoint1.X = AClipRect.Right then
          AIntersectPoint.X := AClipRect.Left
        else
          AIntersectPoint.X := AClipRect.Right;
      end
      else
        if APoint1.Y = AClipRect.Top then
        begin
          FindBottomIntersectPoint(APoint1, AClipRect, A / B, AIntersectPoint);
          if AIntersectPoint.X > AClipRect.Right then
            FindRightIntersectPoint(APoint1, AClipRect, B / A, AIntersectPoint)
          else
            if AIntersectPoint.X < AClipRect.Left then
               FindLeftIntersectPoint(APoint1, AClipRect, B / A, AIntersectPoint);
        end
        else
          if APoint1.Y = AClipRect.Bottom then
          begin
            FindTopIntersectPoint(APoint1, AClipRect, A / B, AIntersectPoint);
            if AIntersectPoint.X > AClipRect.Right then
              FindRightIntersectPoint(APoint1, AClipRect, B / A, AIntersectPoint)
            else
              if AIntersectPoint.X < AClipRect.Left then
                 FindLeftIntersectPoint(APoint1, AClipRect, B / A, AIntersectPoint);
          end
          else
            if APoint1.X = AClipRect.Left then
            begin
              FindRightIntersectPoint(APoint1, AClipRect, B / A, AIntersectPoint);
              if AIntersectPoint.Y < AClipRect.Top then
                FindTopIntersectPoint(APoint1, AClipRect, A / B, AIntersectPoint)
              else
                if AIntersectPoint.Y > AClipRect.Bottom then
                   FindBottomIntersectPoint(APoint1, AClipRect, A / B, AIntersectPoint);
            end
            else
            begin
              FindLeftIntersectPoint(APoint1, AClipRect, B / A, AIntersectPoint);
              if AIntersectPoint.Y < AClipRect.Top then
                FindTopIntersectPoint(APoint1, AClipRect, A / B, AIntersectPoint)
              else
                if AIntersectPoint.Y > AClipRect.Bottom then
                   FindBottomIntersectPoint(APoint1, AClipRect, A / B, AIntersectPoint);
            end;
  end;
end;

procedure dxClipPolylineByRect(const ASubjectPolyline: TPoints;
  const AClipRect: TRect; var APolylines: TdxPointsArray);
var
  APoint, AIntersectPoint: TPoint;
  AIntersectPoint1, AIntersectPoint2: TPoint;
  I, APolylineCount, APointCount: Integer;
  AIsPointInRect, AIsPointOnEdge, AIsPrevPointInRect, AIsPrevPointOnEdge: Boolean;
begin
  APolylineCount := 0;
  APointCount := 0;
  AIsPrevPointInRect := False;
  AIsPrevPointOnEdge := False;

  for I := 0 to High(ASubjectPolyline) do
  begin
    APoint := ASubjectPolyline[I];
    AIsPointInRect := (APoint.X >= AClipRect.Left) and (APoint.X <= AClipRect.Right) and
      (APoint.Y >= AClipRect.Top) and (APoint.Y <= AClipRect.Bottom);
    AIsPointOnEdge := AIsPointInRect and ((APoint.X = AClipRect.Left) or (APoint.X = AClipRect.Right) or
      (APoint.Y = AClipRect.Top) or (APoint.Y = AClipRect.Bottom));
    if AIsPointInRect then
    begin
      if AIsPrevPointInRect then
      begin
        Inc(APointCount);
        SetLength(APolylines[APolylineCount - 1], APointCount);
        APolylines[APolylineCount - 1, APointCount - 1] := APoint;
      end
      else
      begin
        if (APointCount > 1) or (APolylineCount = 0) then
        begin
          Inc(APolylineCount);
          SetLength(APolylines, APolylineCount);
        end;
        APointCount := 0;
        if I <> 0 then
          if AIsPointOnEdge then
          begin
            if IsIntersectClipRectWhenStartPointOnEdge(APoint, ASubjectPolyline[I - 1], AClipRect, AIntersectPoint) then
            begin
              Inc(APointCount);
              SetLength(APolylines[APolylineCount - 1], APointCount);
              APolylines[APolylineCount - 1, APointCount - 1] := AIntersectPoint;
            end;
          end
          else
          begin
            AIntersectPoint := GetIntersectPointWhenStartPointStrictInRect(APoint, ASubjectPolyline[I - 1], AClipRect);
            Inc(APointCount);
            SetLength(APolylines[APolylineCount - 1], APointCount);
            APolylines[APolylineCount - 1, APointCount - 1] := AIntersectPoint;
          end;
        Inc(APointCount);
        SetLength(APolylines[APolylineCount - 1], APointCount);
        APolylines[APolylineCount - 1, APointCount - 1] := APoint;
      end;
    end
    else
      if AIsPrevPointInRect then
        if not AIsPrevPointOnEdge then
        begin
          AIntersectPoint := GetIntersectPointWhenStartPointStrictInRect(ASubjectPolyline[I - 1], APoint, AClipRect);
          Inc(APointCount);
          SetLength(APolylines[APolylineCount - 1], APointCount);
          APolylines[APolylineCount - 1, APointCount - 1] := AIntersectPoint;
        end
        else
        begin
          if IsIntersectClipRectWhenStartPointOnEdge(ASubjectPolyline[I - 1], APoint, AClipRect, AIntersectPoint) then
          begin
            Inc(APointCount);
            SetLength(APolylines[APolylineCount - 1], APointCount);
            APolylines[APolylineCount - 1, APointCount - 1] := AIntersectPoint;
          end;
        end
      else
      begin
        if (I <> 0) and IsIntersectClipRect(ASubjectPolyline[I - 1], APoint, AClipRect, AIntersectPoint1, AIntersectPoint2) then
        begin
          if (APointCount > 1) or (APolylineCount = 0) then
          begin
            Inc(APolylineCount);
            SetLength(APolylines, APolylineCount);
          end;
          APointCount := 0;
          Inc(APointCount, 2);
          SetLength(APolylines[APolylineCount - 1], APointCount);
          APolylines[APolylineCount - 1, 0] := AIntersectPoint1;
          APolylines[APolylineCount - 1, 1] := AIntersectPoint2;
        end;
      end;

    AIsPrevPointInRect := AIsPointInRect;
    AIsPrevPointOnEdge := AIsPointOnEdge;
  end;
  if (APolylineCount > 0) and (APointCount <= 1) then
  begin
    Dec(APolylineCount);
    SetLength(APolylines, APolylineCount);
  end;
end;

{ TdxVertices }

function TdxVertices.AddVertex: TdxVertex;
begin
  Result := Add as TdxVertex;
  if Result.Prev <> nil then
    Result.Prev.NextOriginal := Result;
end;

function TdxVertices.CreateLinkedObject: TcxDoublyLinkedObject;
begin
  Result := TdxVertex.Create;
end;

function TdxVertices.GetFirst: TdxVertex;
begin
  Result := inherited First as TdxVertex;
end;

function TdxVertices.GetLast: TdxVertex;
begin
  Result := inherited Last as TdxVertex;
end;

{ TdxVertex }

constructor TdxVertex.Create;
begin
  inherited Create;
end;

function TdxVertex.GetNext: TdxVertex;
begin
  Result := inherited Next as TdxVertex;
end;

function TdxVertex.GetPrev: TdxVertex;
begin
  Result := inherited Prev as TdxVertex;
end;

end.
