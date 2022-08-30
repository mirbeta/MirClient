{*************************************************************************}
{ THotSpotImage component                                                 }
{ for Delphi 4.0,5.0,6.0 & C++Builder 4.0,5.0,6.0                         }
{ version 1.0 - rel. March, 2002                                          }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright © 2002                                              }
{           Email : info@tmssoftware.com                                  }
{           Web : http://www.tmssoftware.com                              }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage done due to the use of this code.               }
{ The component can be freely used in any application. The complete       }
{ source code remains property of the author and may not be distributed,  }
{ published, given or sold in any form as such. No parts of the source    }
{ code can be included in any other component or application without      }
{ written authorization of the author.                                    }
{*************************************************************************}

unit LineLibrary;


interface

uses
  Windows,              // TPoint, ClipCursor
  HotSpotImage,    // TRealPoint
  ExtCtrls;             // TImage

type
  TLineSelected     = (lsNotSelected, lsPoint1, lsPoint2, lsLine);

  // TLineOrientation is used as part of the heuristic algorithm that decides
  // if a line is selected and if lines intersect}
  TLineOrientation  = (loPoint, loHorizontal, loVertical);

  function AddPoints(const PointA, PointB:  TPoint):  TPoint; overload;
  function AddPoints(const PointA, PointB:  TRealPoint):  TRealPoint; overload;
  function AddPoints(const PointA:  TRealPoint;PointB:TPoint):  TRealPoint; overload;
  function SubtractPoints(const PointA, PointB:  TPoint):  TPoint; overload;
  function SubtractPoints(const PointA, PointB:  TRealPoint):  TRealPoint; overload;

  procedure CalcLineParameters (const PointA, PointB  :  TPoint;
                                 var   Slope, Intercept:  DOUBLE;
                                 var   LineOrientation :  TLineOrientation);
  function NearLine(const Target, Point1, Point2:  TPoint):  Boolean;


  procedure RestrictCursorToDrawingArea (const Image:  TImage);
  procedure RemoveCursorRestrictions;


  function SquareContainsPoint (const SquareCenter   :  TPoint;
                                const SquareHalfSize :  Integer; {pixels}
                                const TestPoint      :  TPoint):  Boolean;

implementation

uses
  Math,     // MinIntValue, MaxIntValue
  Classes;  // Point

function AddPoints(const PointA, PointB:  TPoint):  TPoint;
begin
  with Result DO
  begin
    X := PointA.X + PointB.X;
    Y := PointA.Y + PointB.Y
  end
end {AddPoints};


function AddPoints(const PointA, PointB:  TRealPoint):  TRealPoint;
begin
  with Result do
  begin
    X := PointA.X + PointB.X;
    Y := PointA.Y + PointB.Y
  end
end {AddPoints};


function AddPoints(const PointA:  TRealPoint;PointB:TPoint):  TRealPoint;
begin
  with Result DO
  begin
    X := PointA.X + PointB.X;
    Y := PointA.Y + PointB.Y
  end
end {AddPoints};


function SubtractPoints(const PointA, PointB:  TPoint):  TPoint;
begin
  with Result DO
  begin
    X := PointA.X - PointB.X;
    Y := PointA.Y - PointB.Y
  end
end {SubtractPoints};


function SubtractPoints(const PointA, PointB:  TRealPoint):  TRealPoint;
begin
  with Result DO
  begin
    X := PointA.X - PointB.X;
    Y := PointA.Y - PointB.Y
  end
end {SubtractPoints};


// Determine whether a line is ltHorizonal or ltVertical,  along with the
// appropriate slope and intercept FOR point-slope line  equations.  These
// parameters are used to determine if a line is selected.
procedure CalcLineParameters (const PointA, PointB  :  TPoint;
                              var   Slope, Intercept:  Double;
                              var   LineOrientation :  TLineOrientation);
var
  Delta:  TPoint;
begin
  Delta := SubtractPoints(PointB, PointA);

  if  (Delta.X = 0) AND (Delta.Y = 0) then
  begin
    // This special case should never happen if iMinPixels > 0
    LineOrientation := loPoint;
    Slope     := 0.0;
    Intercept := 0.0
  end
  else
  begin
    if   ABS(Delta.X) >= ABS(Delta.Y) then
    begin
      // The line is more horizontal than vertical.  Determine values FOR
      // equation:  Y = slope*X + intercept
      LineOrientation := loHorizontal;
      try
        Slope := Delta.Y / Delta.X   {conventional slope in geometry}
      except
        Slope := 0.0
      end;
      Intercept := PointA.Y - PointA.X*Slope
    end
    else
    begin
      // The line is more vertical than horizontal.  Determine values FOR
      // equation:  X = slope*Y + intercept
      LineOrientation := loVertical;
      try
        Slope := Delta.X / Delta.Y  {reciprocal of conventional slope}
      except
        Slope := 0.0
      end;
      Intercept := PointA.X - PointA.Y*Slope;
    end
  end
end {CalcLineParameters};


// Determine if Target1 is "near" line segment between Point1 and Point2
function NearLine(const Target, Point1, Point2:  TPoint):  Boolean;
const
  LineSelectFuzz =  4;  // Pixel "fuzz" used in line selection
var
  Intercept      :  DOUBLE;
  LineOrientation:  TLineOrientation;
  maxX           :  Integer;
  maxY           :  Integer;
  minX           :  Integer;
  minY           :  Integer;
  Slope          :  DOUBLE;
  xCalculated    :  Integer;
  yCalculated    :  Integer;
begin
  Result := False;

  // if an endpoint is not selected, was part of line selected?
  CalcLineParameters (Point1, Point2, Slope, Intercept, LineOrientation);

  case LineOrientation OF
  loHorizontal:
    begin
      minX := MinIntValue([Point1.X, Point2.X]);
      maxX := MaxIntValue([Point1.X, Point2.X]);
      // first check if selection within horizontal range of line
      if (Target.X >= minX) and (Target.X <= maxX) then
      begin
        // Since X is within range of line, now see if Y value is close
        // enough to the calculated Y value FOR the line to be selected.
        yCalculated := ROUND( Slope*Target.X + Intercept );
        if ABS(yCalculated - Target.Y) <= LineSelectFuzz then
          Result := TRUE
      end
    end;
  loVertical:
    begin
      minY := MinIntValue([Point1.Y, Point2.Y]);
      maxY := MaxIntValue([Point1.Y, Point2.Y]);
      // first check if selection within vertical range of line
      if   (Target.Y >= minY) AND (Target.Y <= maxY) then
      begin
        // Since Y is within range of line, now see if X value is close
        // enough to the calculated X value FOR the line to be selected.
        xCalculated := ROUND( Slope*Target.Y + Intercept );
        if ABS(xCalculated - Target.X) <= LineSelectFuzz then
          Result := TRUE
      end
    end;
  loPoint:
      // do nothing -- should not occur
  end
end {NearLine};


procedure RestrictCursorToDrawingArea (const Image:  TImage);
var
  CursorClipArea:  TRect;
begin
  CursorClipArea := Bounds(Image.ClientOrigin.X,
                           Image.ClientOrigin.Y,
                           Image.Width, Image.Height);
  Windows.ClipCursor(@CursorClipArea)
end {RestrictCursorToDrawingArea};


procedure RemoveCursorRestrictions;
begin
  Windows.ClipCursor(NIL)
end {RemoveCursorRestrictions};



// Could use Windows PtInRect API call instead (but it excludes the
// bottom and right edges of the rectangle while including the left
// and top edges.
function SquareContainsPoint (const SquareCenter   :  TPoint;
                              const SquareHalfSize :  Integer; {pixels}
                              const TestPoint      :  TPoint):  Boolean;
begin
  Result := (TestPoint.X >= SquareCenter.X - SquareHalfSize) AND
            (TestPoint.X <= SquareCenter.X + SquareHalfSize) AND
            (TestPoint.Y >= SquareCenter.Y - SquareHalfSize) AND
            (TestPoint.Y <= SquareCenter.Y + SquareHalfSize)
end {SquareContainsPoint};


end.
