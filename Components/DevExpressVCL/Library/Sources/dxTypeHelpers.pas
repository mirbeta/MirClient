{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressCore Library                                      }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSCORE LIBRARY AND ALL           }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY. }
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

unit dxTypeHelpers;

{$I cxVer.inc}

interface

uses
  Types, Generics.Defaults, Generics.Collections, cxGeometry;

type
  { TdxSizeHelper }


  TdxSizeHelper = record helper for TSize
  private
    function GetHeight: Integer; inline;
    function GetWidth: Integer; inline;
    procedure SetHeight(const Value: Integer); inline;
    procedure SetWidth(const Value: Integer); inline;
  public
    class function Create(const P : TPoint): TSize; overload; static;
    class function Create(const X, Y : Integer): TSize; overload; static;
    class function Create(const R : TRect): TSize; overload; static;

    class function Empty: TSize; static;
    procedure Init(const P : TPoint); overload;
    procedure Init(const X, Y : Integer); overload;
    procedure Init(const R : TRect); overload;

    function Add(const S: TSize): TSize; inline;
    function IsEqual(const S : TSize) : Boolean; inline;
    function IsZero : Boolean; inline;
    class function Round(const S: TdxSizeF): TSize; static;
    function Subtract(const S: TSize): TSize; inline;
    function ToPoint: TPoint;
    function ToSizeF: TdxSizeF;

    property Height: Integer read GetHeight write SetHeight;
    property Width: Integer read GetWidth write SetWidth;
  end;

  { TdxPointHelper }

  TdxPointHelper = record helper for TPoint
  public
    class function Create(const S : TSize): TPoint; overload; static;
    class function Create(const X, Y : Integer): TPoint; overload; static;

    procedure Init(const S : TSize); overload; inline;
    procedure Init(const X, Y : Integer); overload; inline;

    function Add(const P: TPoint): TPoint; inline;
    function Distance(const P: TPoint): Single; inline;
    procedure Invert;
    function IsEqual(const P: TPoint) : Boolean; inline;
    function IsZero : Boolean; inline;
    class function Null: TPoint; static;
    procedure Offset(const DX, DY : Integer); overload; inline;
    procedure Offset(const P: TPoint); overload; inline;
    procedure Offset(const S: TSize); overload; inline;
    function SquareOfDistance(const P: TPoint): Int64; inline;
    function Subtract(const P: TPoint): TPoint; inline;
    function ToPointF: TdxPointF;
    function ToSize: TSize;
  end;

  { TdxRectHelper }

  TdxRectHelper = record helper for TRect
  private
    function GetHeight: Integer; inline;
    function GetLocation: TPoint; inline;
    function GetSize: TSize; inline;
    function GetWidth: Integer; inline;
    function GetX: Integer; inline;
    function GetY: Integer; inline;
    procedure SetHeight(const Value: Integer); inline;
    procedure SetSize(const Value: TSize); inline;
    procedure SetWidth(const Value: Integer); inline;
    procedure SetX(const Value: Integer); inline;
    procedure SetY(const Value: Integer); inline;
  public
    class function Create(const ALeft, ATop, ARight, ABottom: Integer; ANormalize: Boolean = False): TRect; overload; static;
    class function Create(const AOrigin: TPoint): TRect; overload; static;
    class function Create(const ATopLeft, ABottomRight: TPoint; ANormalize: Boolean = False): TRect; overload; static;
    class function CreateSize(const ASize: TSize): TRect; overload; static;
    class function CreateSize(const AOrigin: TPoint; AWidth, AHeight: Integer): TRect; overload; static;
    class function CreateSize(const ALeft, ATop, AWidth, AHeight: Integer): TRect; overload; static;
    class function CreateSize(const ALeft, ATop: Integer; const ASize: TSize): TRect; overload; static;
    class function CreateSize(const AOrigin: TPoint; const ASize: TSize): TRect; overload; static;

    class function Round(const ARect: TdxRectF): TRect; static;
    class function Trunc(const ARect: TdxRectF): TRect; static;

    procedure Init(const ALeft, ATop, ARight, ABottom: Integer; ANormalize: Boolean = False); overload; inline;
    procedure Init(const AOrigin: TPoint); overload; inline;
    procedure Init(const ATopLeft, ABottomRight: TPoint; ANormalize: Boolean = False); overload; inline;
    procedure InitSize(const ASize: TSize); overload; inline;
    procedure InitSize(const AOrigin: TPoint; const ASize: TSize); overload; inline;
    procedure InitSize(const AOrigin: TPoint; AWidth, AHeight: Integer); overload; inline;
    procedure InitSize(const ALeft, ATop, AWidth, AHeight: Integer); overload; inline;
    procedure InitSize(const ALeft, ATop: Integer; const ASize: TSize); overload; inline;

    procedure Empty;
    procedure NormalizeRect; inline;
    function IsEmpty: Boolean; inline;
    function IsEqual(const R: TRect): Boolean; inline;
    function IsZero : Boolean; inline;
    function Contains(const P: TPoint): Boolean; overload; inline;
    function Contains(X, Y: Integer): Boolean; overload; inline;
    function Contains(const R: TRect): Boolean; overload; inline;
    function IntersectsWith(const R: TRect): Boolean; inline;
    procedure Intersect(const R: TRect); overload; inline;

    procedure MoveToLeft(X: Integer); inline;
    procedure MoveToTop(Y: Integer); inline;
    procedure MoveToRight(X: Integer); inline;
    procedure MoveToBottom(Y: Integer); inline;

    class function Null: TRect; static;

    procedure Offset(const DX, DY: Integer); overload; inline;
    procedure Offset(const P: TPoint); overload; inline;
    procedure Offset(const S: TSize); overload; inline;
    function GetOffsetRect(const DX, DY: Integer): TRect; overload; inline;
    function GetOffsetRect(const P: TPoint): TRect; overload; inline;
    function GetOffsetRect(const S: TSize): TRect; overload; inline;

    procedure SetLocation(const P: TPoint); overload; inline;
    procedure SetLocation(const X, Y: Integer); overload; inline;
    function SplitHorizontally(ACellCount: Integer): TArray<TRect>;

    function CenterPoint: TPoint; inline;
    function ToRectF: TdxRectF;

    procedure Inflate(const ADeltaLeft, ADeltaTop, ADeltaRight, ADeltaBottom: Integer); overload; inline;
    procedure Inflate(const DX, DY: Integer); overload; inline;
    procedure Union(const R: TRect); overload; inline;

    property X: Integer read GetX write SetX;
    property Y: Integer read GetY write SetY;
    property Height: Integer read GetHeight write SetHeight;
    property Size: TSize read GetSize write SetSize;
    property Width: Integer read GetWidth write SetWidth;

    property Location: TPoint read GetLocation write SetLocation;
  end;

  { TdxSizeFHelper }

  TdxSizeFHelper = record helper for TdxSizeF
  private
    function GetHeight: Single; inline;
    function GetWidth: Single; inline;
    procedure SetHeight(const Value: Single); inline;
    procedure SetWidth(const Value: Single); inline;
  public
    property Height: Single read GetHeight write SetHeight;
    property Width: Single read GetWidth write SetWidth;
  end;

  { TdxArrayHelper }

  TdxArrayHelper = class helper for TArray
{$IFNDEF DELPHIXE8}
  private
    class procedure CheckArrays(ASource, ADestination: Pointer; ASourceIndex, ASourceLength, ADestIndex, ADestLength, ACount: NativeInt); static;
  public
    class procedure Copy<T>(const ASource: array of T; var ADestination: array of T; ASourceIndex, ADestIndex, ACount: NativeInt); overload; static;
    class procedure Copy<T>(const ASource: array of T; var ADestination: array of T; ACount: NativeInt); overload; static;
{$ENDIF}
    class procedure Reverse<T>(var ASource: array of T); overload; static;
    class procedure Concat<T>(const ASource: array of T; var ADestination: TArray<T>; ASourceIndex, ACount: NativeInt); overload; static;
    class procedure Concat<T>(const ASource: array of T; var ADestination: TArray<T>); overload; static;
  end;

implementation

uses
  RTLConsts, SysUtils, Math, TypInfo;

{ TdxSizeHelper }

class function TdxSizeHelper.Create(const P : TPoint): TSize;
begin
  Result.cx := P.X;
  Result.cy := P.Y;
end;

class function TdxSizeHelper.Create(const X, Y: Integer): TSize;
begin
  Result.cx := X;
  Result.cy := Y;
end;

class function TdxSizeHelper.Create(const R : TRect): TSize;
begin
  Result := R.Size;
end;

class function TdxSizeHelper.Empty: TSize;
begin
  Result := TSize.Create(0, 0);
end;

procedure TdxSizeHelper.Init(const P : TPoint);
begin
  cx := P.X;
  cy := P.Y;
end;

procedure TdxSizeHelper.Init(const X, Y: Integer);
begin
  cx := X;
  cy := Y;
end;

procedure TdxSizeHelper.Init(const R : TRect);
begin
  Self := R.Size;
end;

function TdxSizeHelper.GetHeight: Integer;
begin
  Result := cy;
end;

function TdxSizeHelper.GetWidth: Integer;
begin
  Result := cx;
end;

function TdxSizeHelper.Add(const S: TSize): TSize;
begin
  Result.cx := cx + S.cx;
  Result.cy := cy + S.cy;
end;

function TdxSizeHelper.IsZero: Boolean;
begin
  Result := (cx = 0) and (cy = 0);
end;

function TdxSizeHelper.IsEqual(const S : TSize): Boolean;
begin
  Result := (cx = S.cx) and (cy = S.cy);
end;

class function TdxSizeHelper.Round(const S: TdxSizeF): TSize;
begin
  Result.cx := System.Round(S.cx);
  Result.cy := System.Round(S.cy);
end;

procedure TdxSizeHelper.SetHeight(const Value: Integer);
begin
  cy := Value;
end;

procedure TdxSizeHelper.SetWidth(const Value: Integer);
begin
  cx := Value;
end;

function TdxSizeHelper.Subtract(const S: TSize): TSize;
begin
  Result.cx := cx - S.cx;
  Result.cy := cy - S.cy;
end;

function TdxSizeHelper.ToPoint: TPoint;
begin
  Result.X := cx;
  Result.Y := cy;
end;

function TdxSizeHelper.ToSizeF: TdxSizeF;
begin
  Result.cx := cx;
  Result.cy := cy;
end;

{ TdxPointHelper }

class function TdxPointHelper.Create(const S: TSize): TPoint;
begin
  Result.X := S.cx;
  Result.Y := S.cy;
end;

class function TdxPointHelper.Create(const X, Y: Integer): TPoint;
begin
  Result.X := X;
  Result.Y := Y;
end;

procedure TdxPointHelper.Init(const S: TSize);
begin
  X := S.cx;
  Y := S.cy;
end;

procedure TdxPointHelper.Init(const X, Y: Integer);
begin
  Self.X := X;
  Self.Y := Y;
end;

function TdxPointHelper.Add(const P: TPoint): TPoint;
begin
  Result.X := X + P.X;
  Result.Y := Y + P.Y;
end;

function TdxPointHelper.Distance(const P: TPoint): Single;
begin
  Result := Sqrt(Sqr(Int(X - P.X)) + Sqr(Int(Y - P.Y)));
end;

procedure TdxPointHelper.Invert;
begin
  X := -X;
  Y := -Y;
end;

function TdxPointHelper.IsEqual(const P: TPoint): Boolean;
begin
  Result := (X = P.X) and (Y = P.Y);
end;

function TdxPointHelper.IsZero: Boolean;
begin
  Result := (X = 0) and (Y = 0);
end;

class function TdxPointHelper.Null: TPoint;
begin
  Result.X := 0;
  Result.Y := 0;
end;

procedure TdxPointHelper.Offset(const S: TSize);
begin
  Inc(X, S.cx);
  Inc(Y, S.cy);
end;

procedure TdxPointHelper.Offset(const DX, DY: Integer);
begin
  Inc(X, DX);
  Inc(Y, DY);
end;

procedure TdxPointHelper.Offset(const P: TPoint);
begin
  Inc(X, P.X);
  Inc(Y, P.Y);
end;

function TdxPointHelper.SquareOfDistance(const P: TPoint): Int64;
begin
  Result := Sqr(X - P.X) + Sqr(Y - P.Y);
end;

function TdxPointHelper.Subtract(const P: TPoint): TPoint;
begin
  Result.X := X - P.X;
  Result.Y := Y - P.Y;
end;

function TdxPointHelper.ToPointF: TdxPointF;
begin
  Result.X := X;
  Result.Y := Y;
end;

function TdxPointHelper.ToSize: TSize;
begin
  Result.cx := X;
  Result.cy := Y;
end;

{ TdxRectHelper }

class function TdxRectHelper.Create(const AOrigin: TPoint): TRect;
begin
  Result.TopLeft := AOrigin;
  Result.BottomRight := AOrigin;
end;

class function TdxRectHelper.Create(const ALeft, ATop, ARight, ABottom: Integer; ANormalize: Boolean = False): TRect;
begin
  Result.Left := ALeft;
  Result.Top := ATop;
  Result.Right := ARight;
  Result.Bottom := ABottom;
  if ANormalize then
    Result.NormalizeRect;
end;

class function TdxRectHelper.Create(const ATopLeft, ABottomRight: TPoint; ANormalize: Boolean = False): TRect;
begin
  Result.TopLeft := ATopLeft;
  Result.BottomRight := ABottomRight;
  if ANormalize then
    Result.NormalizeRect;
end;

class function TdxRectHelper.CreateSize(const ASize: TSize): TRect;
begin
  Result.Left := 0;
  Result.Top := 0;
  Result.Right := ASize.cx;
  Result.Bottom := ASize.cy;
end;

class function TdxRectHelper.CreateSize(const AOrigin: TPoint; AWidth, AHeight: Integer): TRect;
begin
  Result.TopLeft := AOrigin;
  Result.Right := AOrigin.X + AWidth;
  Result.Bottom := AOrigin.Y + AHeight;
end;

class function TdxRectHelper.CreateSize(const ALeft, ATop, AWidth, AHeight: Integer): TRect;
begin
  Result.Left := ALeft;
  Result.Top := ATop;
  Result.Right := ALeft + AWidth;
  Result.Bottom := ATop + AHeight;
end;

class function TdxRectHelper.CreateSize(const ALeft, ATop: Integer; const ASize: TSize): TRect;
begin
  Result.Left := ALeft;
  Result.Top := ATop;
  Result.Right := ALeft + ASize.cx;
  Result.Bottom := ATop + ASize.cy;
end;

class function TdxRectHelper.CreateSize(const AOrigin: TPoint; const ASize: TSize): TRect;
begin
  Result.TopLeft := AOrigin;
  Result.Right := AOrigin.X + ASize.cx;
  Result.Bottom := AOrigin.Y + ASize.cy;
end;

class function TdxRectHelper.Round(const ARect: TdxRectF): TRect;
begin
  Result.Left := System.Round(ARect.Left);
  Result.Top := System.Round(ARect.Top);
  Result.Right := System.Round(ARect.Right);
  Result.Bottom := System.Round(ARect.Bottom);
end;

class function TdxRectHelper.Trunc(const ARect: TdxRectF): TRect;
begin
  Result.Left := System.Trunc(ARect.Left);
  Result.Top := System.Trunc(ARect.Top);
  Result.Right := System.Trunc(ARect.Right);
  Result.Bottom := System.Trunc(ARect.Bottom);
end;

procedure TdxRectHelper.Init(const AOrigin: TPoint);
begin
  TopLeft := AOrigin;
  BottomRight := AOrigin;
end;

procedure TdxRectHelper.Init(const ALeft, ATop, ARight, ABottom: Integer; ANormalize: Boolean = False);
begin
  Left := ALeft;
  Top := ATop;
  Right := ARight;
  Bottom := ABottom;
  if ANormalize then
    Self.NormalizeRect;
end;

procedure TdxRectHelper.Init(const ATopLeft, ABottomRight: TPoint; ANormalize: Boolean);
begin
  TopLeft := ATopLeft;
  BottomRight := ABottomRight;
  if ANormalize then
    NormalizeRect;
end;

procedure TdxRectHelper.InitSize(const ASize: TSize);
begin
  Left := 0;
  Top := 0;
  Right := ASize.cx;
  Bottom := ASize.cy;
end;

procedure TdxRectHelper.InitSize(const AOrigin: TPoint; const ASize: TSize);
begin
  TopLeft := AOrigin;
  Right := Left + ASize.cx;
  Bottom := Top + ASize.cy;
end;

procedure TdxRectHelper.InitSize(const AOrigin: TPoint; AWidth, AHeight: Integer);
begin
  TopLeft := AOrigin;
  Right := Left + AWidth;
  Bottom := Top + AHeight;
end;

procedure TdxRectHelper.InitSize(const ALeft, ATop, AWidth, AHeight: Integer);
begin
  Left := ALeft;
  Top := ATop;
  Right := Left + AWidth;
  Bottom := Top + AHeight;
end;

procedure TdxRectHelper.InitSize(const ALeft, ATop: Integer; const ASize: TSize);
begin
  Left := ALeft;
  Top := ATop;
  Right := Left + ASize.cx;
  Bottom := Top + ASize.cy;
end;

class function TdxRectHelper.Null: TRect;
begin
  Result.Left := 0;
  Result.Top := 0;
  Result.Right := 0;
  Result.Bottom := 0;
end;

function TdxRectHelper.CenterPoint: TPoint;
begin
  Result.X := (Right - Left) div 2 + Left;
  Result.Y := (Bottom - Top) div 2 + Top;
end;

function TdxRectHelper.ToRectF: TdxRectF;
begin
  Result.Left := Left;
  Result.Top := Top;
  Result.Right := Right;
  Result.Bottom := Bottom;
end;

function TdxRectHelper.Contains(const P: TPoint): Boolean;
begin
  Result := (P.X >= Left) and (P.X < Right) and (P.Y >= Top) and (P.Y < Bottom);
end;

function TdxRectHelper.Contains(X, Y: Integer): Boolean;
begin
  Result := (X >= Left) and (X < Right) and (Y >= Top) and (Y < Bottom);
end;

function TdxRectHelper.Contains(const R: TRect): Boolean;
begin
  Result := (Left <= R.Left) and (Right >= R.Right) and (Top <= R.Top) and (Bottom >= R.Bottom);
end;

function TdxRectHelper.GetHeight: Integer;
begin
  Result := Bottom - Top;
end;

function TdxRectHelper.GetLocation: TPoint;
begin
  Result := TopLeft;
end;

function TdxRectHelper.GetSize: TSize;
begin
  Result.cx := Width;
  Result.cy := Height;
end;

procedure TdxRectHelper.Empty;
begin
  Left := 0;
  Top := 0;
  Right := 0;
  Bottom := 0;
end;

function TdxRectHelper.GetWidth: Integer;
begin
  Result := Right - Left;
end;

function TdxRectHelper.GetX: Integer;
begin
  Result := Left;
end;

function TdxRectHelper.GetY: Integer;
begin
  Result := Top;
end;

procedure TdxRectHelper.Inflate(const ADeltaLeft, ADeltaTop, ADeltaRight, ADeltaBottom: Integer);
begin
  Dec(Left, ADeltaLeft);
  Inc(Right, ADeltaRight);
  Dec(Top, ADeltaTop);
  Inc(Bottom, ADeltaBottom);
end;

procedure TdxRectHelper.Inflate(const DX, DY: Integer);
begin
  Dec(Left, DX);
  Inc(Right, DX);
  Dec(Top, DY);
  Inc(Bottom, DY);
end;

procedure TdxRectHelper.Intersect(const R: TRect);
begin
  if R.Left > Left then Left := R.Left;
  if R.Top > Top then Top := R.Top;
  if R.Right < Right then Right := R.Right;
  if R.Bottom < Bottom then Bottom := R.Bottom;
  if (Right < Left) or (Bottom < Top) then
    Empty;
end;

function TdxRectHelper.IntersectsWith(const R: TRect): Boolean;
begin
  Result := not ((Right < R.Left) or (Bottom < R.Top) or (R.Right < Left) or (R.Bottom < Top));
end;

function TdxRectHelper.IsEmpty: Boolean;
begin
  Result := (Left >= Right) or (Top >= Bottom);
end;

function TdxRectHelper.IsEqual(const R: TRect): Boolean;
begin
  Result := (Left = R.Left) and (Top = R.Top) and (Right = R.Right) and (Bottom = R.Bottom);
end;

function TdxRectHelper.IsZero: Boolean;
begin
  Result := (Right = 0) and (Bottom = 0) and (Left = 0) and (Top = 0);
end;

procedure TdxRectHelper.MoveToLeft(X: Integer);
begin
  Inc(Right, X - Left);
  Left := X;
end;

procedure TdxRectHelper.MoveToTop(Y: Integer);
begin
  Inc(Bottom, Y - Top);
  Top := Y;
end;

procedure TdxRectHelper.MoveToRight(X: Integer);
begin
  Inc(Left, X - Right);
  Right := X;
end;

procedure TdxRectHelper.MoveToBottom(Y: Integer);
begin
  Inc(Top, Y - Bottom);
  Bottom := Y;
end;

procedure TdxRectHelper.NormalizeRect;
var
  ATemp: Integer;
begin
  if Left > Right then
  begin
    ATemp := Right;
    Right := Left;
    Left := ATemp;
  end;
  if Top > Bottom then
  begin
    ATemp := Bottom;
    Bottom := Top;
    Top := ATemp;
  end;
end;

procedure TdxRectHelper.Offset(const P: TPoint);
begin
  Inc(Left, P.X);
  Inc(Right, P.X);
  Inc(Top, P.Y);
  Inc(Bottom, P.Y);
end;

procedure TdxRectHelper.Offset(const S: TSize);
begin
  Inc(Left, S.cx);
  Inc(Right, S.cx);
  Inc(Top, S.cy);
  Inc(Bottom, S.cy);
end;

procedure TdxRectHelper.Offset(const DX, DY: Integer);
begin
  Inc(Left, DX);
  Inc(Right, DX);
  Inc(Top, DY);
  Inc(Bottom, DY);
end;

function TdxRectHelper.GetOffsetRect(const DX, DY: Integer): TRect;
begin
  Result := Self;
  Result.Offset(DX, DY);
end;

function TdxRectHelper.GetOffsetRect(const P: TPoint): TRect;
begin
  Result := Self;
  Result.Offset(P);
end;

function TdxRectHelper.GetOffsetRect(const S: TSize): TRect;
begin
  Result := Self;
  Result.Offset(S);
end;

procedure TdxRectHelper.SetHeight(const Value: Integer);
begin
  Bottom := Top + Value;
end;

procedure TdxRectHelper.SetLocation(const X, Y: Integer);
begin
  Offset(X - Left, Y - Top);
end;

procedure TdxRectHelper.SetLocation(const P: TPoint);
begin
  Offset(P.X - Left, P.Y - Top);
end;

function TdxRectHelper.SplitHorizontally(ACellCount: Integer): TArray<TRect>;
var
  I, AWidth, ARemainder, AColumnWidth, AColumnsAreaWidth, AHeight, AOffset: Integer;
begin
  if ACellCount <= 0 then
  begin
    SetLength(Result, 1);
    Result[0] := Self;
    Exit;
  end;
  SetLength(Result, ACellCount);
  AOffset := Left;
  AHeight := Height;
  AColumnsAreaWidth := Width;
  AColumnWidth := AColumnsAreaWidth div ACellCount;
  ARemainder := AColumnsAreaWidth - (AColumnWidth * ACellCount);
  for I := 0 to ACellCount - 1 do
  begin
    AWidth := AColumnWidth;
    if ARemainder > 0 then
      Inc(AWidth);
    Result[I].InitSize(AOffset, Top, AWidth, AHeight);
    Inc(AOffset, AWidth);
    Dec(ARemainder)
  end;
end;

procedure TdxRectHelper.SetSize(const Value: TSize);
begin
  Width := Value.cx;
  Height := Value.cy;
end;

procedure TdxRectHelper.SetWidth(const Value: Integer);
begin
  Right := Left + Value;
end;

procedure TdxRectHelper.SetX(const Value: Integer);
begin
  Inc(Right, Value - Left);
  Left := Value;
end;

procedure TdxRectHelper.SetY(const Value: Integer);
begin
  Inc(Bottom, Value - Top);
  Top := Value;
end;

procedure TdxRectHelper.Union(const R: TRect);
begin
  if R.Left < Left then Left := R.Left;
  if R.Top < Top then Top := R.Top;
  if R.Right > Right then Right := R.Right;
  if R.Bottom > Bottom then Bottom := R.Bottom;
end;

{ TdxSizeFHelper }

function TdxSizeFHelper.GetHeight: Single;
begin
  Result := cy;
end;

function TdxSizeFHelper.GetWidth: Single;
begin
  Result := cx;
end;

procedure TdxSizeFHelper.SetHeight(const Value: Single);
begin
  cy := Value;
end;

procedure TdxSizeFHelper.SetWidth(const Value: Single);
begin
  cx := Value;
end;

{ TdxArrayHelper }

class procedure TdxArrayHelper.Reverse<T>(var ASource: array of T);
var
  ALeft, ARight: Integer;
  ATemp: T;
begin
  if Length(ASource) < 2 then
    Exit;
  ALeft  := Low(ASource);
  ARight := High(ASource);
  while ARight > ALeft do
  begin
    ATemp := ASource[ALeft];
    ASource[ALeft] := ASource[ARight];
    ASource[ARight] := ATemp;
    Inc(ALeft);
    Dec(ARight);
  end;
end;

{$IFNDEF DELPHIXE8}

class procedure TdxArrayHelper.Copy<T>(const ASource: array of T;
  var ADestination: array of T; ASourceIndex, ADestIndex, ACount: NativeInt);
var
  ATypeInfo: PTypeInfo;
begin
  CheckArrays(Pointer(@ASource[0]), Pointer(@ADestination[0]), ASourceIndex, Length(ASource), ADestIndex, Length(ADestination), ACount);
  ATypeInfo := TypeInfo(T);
  if ATypeInfo.Kind in [tkLString, tkWString, tkUString, tkVariant, tkArray, tkRecord, tkInterface, tkDynArray] then
    System.CopyArray(Pointer(@ADestination[ADestIndex]), Pointer(@ASource[ASourceIndex]), ATypeInfo, ACount)
  else
    System.Move(Pointer(@ASource[ASourceIndex])^, Pointer(@ADestination[ADestIndex])^, ACount * SizeOf(T));
end;

class procedure TdxArrayHelper.Copy<T>(const ASource: array of T; var ADestination: array of T; ACount: NativeInt);
begin
  Copy<T>(ASource, ADestination, 0, 0, ACount);
end;

class procedure TdxArrayHelper.CheckArrays(ASource, ADestination: Pointer; ASourceIndex, ASourceLength, ADestIndex, ADestLength, ACount: NativeInt);
begin
  if (ASourceIndex < 0) or (ADestIndex < 0) or (ASourceIndex >= ASourceLength) or (ADestIndex >= ADestLength) or
     (ASourceIndex + ACount > ASourceLength) or (ADestIndex + ACount > ADestLength) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  if ASource = ADestination then
    raise EArgumentException.Create('Source and Destination arrays must not be the same');
end;
{$ENDIF}

class procedure TdxArrayHelper.Concat<T>(const ASource: array of T; var ADestination: TArray<T>; ASourceIndex, ACount: NativeInt);
var
  ADestLength: NativeInt;
begin
  if ACount = 0 then
    Exit;
  ADestLength := Length(ADestination);
  System.SetLength(ADestination, ADestLength + ACount);
  Copy<T>(ASource, ADestination, ASourceIndex, ADestLength, ACount);
end;

class procedure TdxArrayHelper.Concat<T>(const ASource: array of T; var ADestination: TArray<T>);
begin
  Concat<T>(ASource, ADestination, 0, Length(ASource));
end;

end.
