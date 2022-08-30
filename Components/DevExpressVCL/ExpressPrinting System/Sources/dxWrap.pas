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

unit dxWrap;

interface

{$I cxVer.inc}

uses
  Windows, Classes, dxPSGlbl, cxGeometry;

type
  TdxPointCoord = (pcX, pcY);
  TdxPointCoords = set of TdxPointCoord;

  TdxPointChangingEvent = procedure(Sender: TObject; Coords: TdxPointCoords;
    var Values: array of Integer) of object;

  TdxPointChangeEvent = procedure(Sender: TObject; Coords: TdxPointCoords) of object;

  TdxPointWrapper = class(TPersistent)
  private
    FPoint: TPoint;
    FOnChanged: TdxPointChangeEvent;
    FOnChanging: TdxPointChangingEvent;
    function GetPartPoint(Index: Integer): Integer;
    procedure SetPartPoint(Index: Integer; Value: Integer);
    procedure SetPoint(const Value: TPoint);
  protected
    procedure DoChanged(ACoords: TdxPointCoords); dynamic;
    procedure DoChanging(ACoords: TdxPointCoords; var AValues: array of Integer); dynamic;
  public
    constructor Create(AX, AY: Integer);
    constructor CreateEmpty;
    procedure Assign(Source: TPersistent); override;

    function Clone: TPersistent;
    procedure Empty;
    function IsEqual(const APoint: TPoint): Boolean;
    class function ArePointsEqual(const Pt1, Pt2: TPoint): Boolean;

    property Point: TPoint read FPoint write SetPoint;
    property OnChanging: TdxPointChangingEvent read FOnChanging write FOnChanging;
    property OnChanged: TdxPointChangeEvent read FOnChanged write FOnChanged;
  published
    property X: Integer index 0 read GetPartPoint write SetPartPoint default 0;
    property Y: Integer index 1 read GetPartPoint write SetPartPoint default 0;
  end;

  TdxRectSide = (rsLeft, rsTop, rsRight, rsBottom);
  TdxRectSides = set of TdxRectSide;

  TdxRectChangingEvent = procedure(Sender: TObject; Sides: TdxRectSides;
    var Values: array of Integer) of object;

  TdxRectChangeEvent = procedure(Sender: TObject; Sides: TdxRectSides) of object;

  TdxRectWrapper = class(TPersistent)
  private
    FRect: TRect;
    FOnChanged: TdxRectChangeEvent;
    FOnChanging: TdxRectChangingEvent;
    function GetHeight: Integer;
    function GetPartRect(Index: Integer): Integer;
    function GetRectPoint(Index: Integer): TPoint;
    function GetSide(ASide: TdxRectSide): Integer;
    function GetWidth: Integer;
    procedure SetHeight(Value: Integer);
    procedure SetPartRect(Index: Integer; Value: Integer);
    procedure SetRect(const Value: TRect);
    procedure SetRectPoint(Index: Integer; const Value: TPoint);
    procedure SetSide(ASide: TdxRectSide; Value: Integer);
    procedure SetWidth(Value: Integer);
  protected
    procedure DoChanged(ASides: TdxRectSides); dynamic;
    procedure DoChanging(ASides: TdxRectSides; var AValues: array of Integer); dynamic;
  public
    constructor Create(ALeft, ATop, ARight, ABottom: Integer);
    constructor CreateEmpty;
    procedure Assign(Source: TPersistent); override;

    function Clone: TPersistent;
    procedure Empty;
    function IsEqual(const ARect: TRect): Boolean;
    function IsEmpty(AnExact: Boolean = False): Boolean;

    property BottomRight: TPoint Index 1 read GetRectPoint write SetRectPoint;
    property Height: Integer read GetHeight write SetHeight;
    property Rect: TRect read FRect write SetRect;
    property Side[ASide: TdxRectSide]: Integer read GetSide write SetSide; default;
    property TopLeft: TPoint Index 0 read GetRectPoint write SetRectPoint;
    property Width: Integer read GetWidth write SetWidth;

    property OnChanging: TdxRectChangingEvent read FOnChanging write FOnChanging;
    property OnChanged: TdxRectChangeEvent read FOnChanged write FOnChanged;
  published
    property Bottom: Integer index 3 read GetPartRect write SetPartRect default 0;
    property Left: Integer index 0 read GetPartRect write SetPartRect default 0;
    property Right: Integer index 2 read GetPartRect write SetPartRect default 0;
    property Top: Integer index 1 read GetPartRect write SetPartRect default 0;
  end;

implementation

uses
  dxPSUtl;

type
  TPoints = array[0..1] of Integer;
  TRects = array[0..3] of Integer;

function MakePoints(X, Y: Integer): TPoints; overload;
begin
  Result[0] := X;
  Result[1] := Y;
end;

function MakePoints(const Pt: TPoint): TPoints; overload;
begin
  Result := MakePoints(Pt.X, Pt.Y);
end;

function MakeRects(ALeft, ATop, ARight, ABottom: Integer): TRects; overload;
begin
  Result[0] := ALeft;
  Result[1] := ATop;
  Result[2] := ARight;
  Result[3] := ABottom;
end;

function MakeRects(const R: TRect): TRects; overload;
begin
  with R do
    Result := MakeRects(Left, Top, Right, Bottom);
end;

{ TdxPointWrapper }

constructor TdxPointWrapper.Create(AX, AY: Integer);
begin
  inherited Create;
  FPoint.X := AX;
  FPoint.Y := AY;
end;

constructor TdxPointWrapper.CreateEmpty;
begin
  Create(0, 0);
end;

procedure TdxPointWrapper.Assign(Source: TPersistent);
begin
  if Source = nil then
    Empty
  else
    if Source is TdxPointWrapper then
      Point := TdxPointWrapper(Source).Point
    else
      inherited Assign(Source)
end;

function TdxPointWrapper.Clone: TPersistent;
begin
  Result := TdxPointWrapper.Create(0, 0);
  try
    Result.Assign(Self);
  except
    Result.Free;
    raise;
  end;
end;

procedure TdxPointWrapper.DoChanged(ACoords: TdxPointCoords);
begin
  if Assigned(FOnChanged) then FOnChanged(Self, ACoords);
end;

procedure TdxPointWrapper.DoChanging(ACoords: TdxPointCoords;
  var AValues: array of Integer);
begin
  if Assigned(FOnChanging) then FOnChanging(Self, ACoords, AValues);
end;

procedure TdxPointWrapper.SetPoint(const Value: TPoint);
var
  Points: TPoints;
begin
  if not IsEqual(Value) then
  begin
    Points := MakePoints(Value);
    DoChanging([pcX, pcY], Points);
    FPoint.X := Points[0];
    FPoint.Y := Points[1];
    DoChanged([pcX, pcY]);
  end;
end;

function TdxPointWrapper.GetPartPoint(Index: Integer): Integer;
begin
  if Index = 0 then
    Result := FPoint.X
  else
    Result := FPoint.Y;
end;

procedure TdxPointWrapper.SetPartPoint(Index: Integer; Value: Integer);
var
  Points: TPoints;
begin
  if Index = 0 then
  begin
    if FPoint.X <> Value then
    begin
      Points := MakePoints(Value, 0);
      DoChanging([pcX], Points);
      FPoint.X := Points[0];
      DoChanged([pcX]);
    end
  end
  else
    if FPoint.Y <> Value then
    begin
      Points := MakePoints(0, Value);
      DoChanging([pcY], Points);
      FPoint.Y := Points[1];
      DoChanged([pcY]);
    end;
end;

function TdxPointWrapper.IsEqual(const APoint: TPoint): Boolean;
begin
  Result := (FPoint.X = APoint.X) and (FPoint.Y = APoint.Y);
end;

class function TdxPointWrapper.ArePointsEqual(const Pt1, Pt2: TPoint): Boolean;
begin
  Result := (Pt1.X = Pt2.X) and (Pt1.Y = Pt2.Y);
end;

procedure TdxPointWrapper.Empty;
var
  Points: TPoints;
begin
  if not IsEqual(cxNullPoint) then
  begin
    Points := MakePoints(0, 0);
    DoChanging([pcX, pcY], Points);
    FPoint.X := Points[0];
    FPoint.Y := Points[1];
    DoChanged([pcX, pcY]);
  end;
end;

{ TdxRectWrapper }

constructor TdxRectWrapper.Create(ALeft, ATop, ARight, ABottom: Integer);
begin
  inherited Create;
  FRect.Left := ALeft;
  FRect.Top := ATop;
  FRect.Right := ARight;
  FRect.Bottom := ABottom;
end;

constructor TdxRectWrapper.CreateEmpty;
begin
  Create(0, 0, 0, 0);
end;

procedure TdxRectWrapper.Assign(Source: TPersistent);
begin
  if Source = nil then
    Empty
  else
    if Source is TdxRectWrapper then
      Rect := TdxRectWrapper(Source).Rect
    else
      inherited Assign(Source)
end;

function TdxRectWrapper.Clone: TPersistent;
begin
  Result := TdxRectWrapper.Create(0, 0, 0, 0);
  try
    Result.Assign(Self);
  except
    Result.Free;
    raise;
  end;
end;

procedure TdxRectWrapper.Empty;
var
  Rects: TRects;
begin
  if not IsEqual(cxNullRect) then
  begin
    Rects := MakeRects(0, 0, 0, 0);
    DoChanging([rsLeft, rsTop, rsRight, rsBottom], Rects);
    FRect.Left := Rects[0];
    FRect.Top := Rects[1];
    FRect.Right := Rects[2];
    FRect.Bottom := Rects[3];
    DoChanged([rsLeft, rsTop, rsRight, rsBottom]);
  end;
end;

function TdxRectWrapper.IsEqual(const ARect: TRect): Boolean;
begin
  Result := EqualRect(FRect, ARect);
end;

function TdxRectWrapper.IsEmpty(AnExact: Boolean = False): Boolean;
begin
  if AnExact then
    Result := IsEqual(cxNullRect)
  else
    Result := IsRectEmpty(Rect);
end;

procedure TdxRectWrapper.DoChanged(ASides: TdxRectSides);
begin
  if Assigned(FOnChanged) then FOnChanged(Self, ASides);
end;

procedure TdxRectWrapper.DoChanging(ASides: TdxRectSides; var AValues: array of Integer);
begin
  if Assigned(FOnChanging) then FOnChanging(Self, ASides, AValues);
end;

procedure TdxRectWrapper.SetRect(const Value: TRect);
var
  Rects: TRects;
begin
  if not EqualRect(FRect, Value) then
  begin
    Rects := MakeRects(Value);
    DoChanging([rsLeft, rsTop, rsRight, rsBottom], Rects);
    FRect.Left := Rects[0];
    FRect.Top := Rects[1];
    FRect.Right := Rects[2];
    FRect.Bottom := Rects[3];
    DoChanged([rsLeft, rsTop, rsRight, rsBottom]);
  end;
end;

function TdxRectWrapper.GetWidth: Integer;
begin
  Result := FRect.Right - FRect.Left;
end;

procedure TdxRectWrapper.SetWidth(Value: Integer);
var
  Rects: TRects;
begin
  if Width <> Value then
  begin
    Rects := MakeRects(0, 0, Value, 0);
    DoChanging([rsRight], Rects);
    FRect.Right := Rects[2];
    DoChanged([rsRight]);
  end;
end;

function TdxRectWrapper.GetHeight: Integer;
begin
  Result := FRect.Bottom - FRect.Top
end;

procedure TdxRectWrapper.SetHeight(Value: Integer);
var
  Rects: TRects;
begin
  if Height <> Value then
  begin
    Rects := MakeRects(0, 0, 0, Value);
    DoChanging([rsBottom], Rects);
    FRect.Bottom := Rects[3];
    DoChanged([rsBottom]);
  end;
end;

function TdxRectWrapper.GetPartRect(Index: Integer): Integer;
begin
  case Index of
    0: Result := FRect.Left;
    1: Result := FRect.Top;
    2: Result := FRect.Right;
  else
    Result := FRect.Bottom;
  end;
end;

procedure TdxRectWrapper.SetPartRect(Index: Integer; Value: Integer);
var
  Rects: TRects;
begin
  case Index of
    0:
      if FRect.Left <> Value then
      begin
        Rects := MakeRects(Value, 0, 0, 0);
        DoChanging([rsLeft], Rects);
        FRect.Left := Rects[0];
        DoChanged([rsLeft]);
      end;
    1:
      if FRect.Top <> Value then
      begin
        Rects := MakeRects(0, Value, 0, 0);
        DoChanging([rsTop], Rects);
        FRect.Top := Rects[1];
        DoChanged([rsTop]);
      end;
    2:
      if FRect.Right <> Value then
      begin
        Rects := MakeRects(0, 0, Value, 0);
        DoChanging([rsRight], Rects);
        FRect.Right := Rects[2];
        DoChanged([rsRight]);
      end;
    3:
      if FRect.Bottom <> Value then
      begin
        Rects := MakeRects(0, 0, 0, Value);
        DoChanging([rsBottom], Rects);
        FRect.Bottom := Rects[3];
        DoChanged([rsBottom]);
      end;
  end;
end;

function TdxRectWrapper.GetRectPoint(Index: Integer): TPoint;
begin
  if Index = 0 then
    Result := FRect.TopLeft
  else
    Result := FRect.BottomRight;
end;

procedure TdxRectWrapper.SetRectPoint(Index: Integer; const Value: TPoint);
var
  Rects: TRects;
begin
  if Index = 0 then
    if not TdxPointWrapper.ArePointsEqual(FRect.TopLeft, Value) then
    begin
      Rects := MakeRects(Value.X, Value.Y, 0, 0);
      DoChanging([rsLeft, rsTop], Rects);
      FRect.Left := Rects[0];
      FRect.Top := Rects[1];
      DoChanged([rsLeft, rsTop]);
    end
    else
  else
    if not TdxPointWrapper.ArePointsEqual(FRect.BottomRight, Value) then
    begin
      Rects := MakeRects(0, 0, Value.X, Value.Y);
      DoChanging([rsRight, rsBottom], Rects);
      FRect.Right := Rects[2];
      FRect.Bottom := Rects[3];
      DoChanged([rsRight, rsBottom]);
    end;
end;

function TdxRectWrapper.GetSide(ASide: TdxRectSide): Integer;
begin
  case ASide of
    rsLeft:
      Result := FRect.Left;
    rsTop:
      Result := FRect.Top;
    rsRight:
      Result := FRect.Right;
  else //rsBottom
    Result := FRect.Bottom;
  end;
end;

procedure TdxRectWrapper.SetSide(ASide: TdxRectSide; Value: Integer);
begin
  SetPartRect(Integer(ASide), Value);
end;

initialization
  RegisterClasses([TdxPointWrapper, TdxRectWrapper]);

finalization
  UnregisterClasses([TdxPointWrapper, TdxRectWrapper]);

end.

