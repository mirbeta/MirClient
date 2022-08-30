{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressVerticalGrid                                      }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSVERTICALGRID AND ALL           }
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
unit cxVGridUtils;

{$I cxVer.inc}

interface

uses
  Types, Windows, Classes, Graphics, dxCore, cxGraphics, cxStyles, cxControls;

type
  { TcxDataList }

  TcxDataList = class
  private
    FAllocated: Integer;
    FData: Pointer;
    FDelta: Integer;
    FRecordSize: Integer;
  protected
    FCount: Integer;
    procedure CheckCapacity;
    function Get(Index: Integer): Pointer;
    property Data: Pointer read FData;
    property RecordSize: Integer read FRecordSize;
  public
    constructor Create(ARecordSize: Integer);
    destructor Destroy; override;
    procedure Clear; virtual;
    property Count: Integer read FCount;
    property Delta: Integer read FDelta write FDelta;
  end;

  { TcxRectList }

  TcxRectList = class(TcxDataList)
  private
    FIsRightToLeftConverted: Boolean;
    function GetRect(Index: Integer): TRect;
    procedure SetRect(Index: Integer; const Value: TRect);
  public
    constructor Create;
    procedure Assign(Source: TcxRectList);
    function Add(const R: TRect): Integer;
    procedure Clear; override;
    procedure DoRightToLeftConversion(const AClientBounds: TRect); virtual;
    procedure RightToLeftConversion(const AClientBounds: TRect);

    property IsRightToLeftConverted: Boolean read FIsRightToLeftConverted;
    property Rects[Index: Integer]: TRect read GetRect write SetRect; default;
  end;

  { TRectScaler }

  PScaleParams = ^TScaleParams;
  TScaleParams = record
    Width: Integer;
    MinWidth: Integer;
    FixedWidth: Integer;
  end;

  TRectScaler = class(TList)
  private
    FScaledRects: TcxRectList;
    function GetSummaryParam: TScaleParams;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(AWidth: Integer); overload;
    procedure Add(AWidth, AMinWidth: Integer); overload;
    procedure Add(AWidth, AMinWidth, AFixedWidth: Integer); overload;
    procedure Clear; override;
    procedure CalcRect(const Rect: TRect);
    procedure ScaleRect(const Rect: TRect);
    property ScaledRects: TcxRectList read FScaledRects;
    property SummaryParam: TScaleParams read GetSummaryParam;
  end;

  { TLineInfo }

  PLineInfo = ^TLineInfo;
  TLineInfo = record
    Rect: TRect;
    IsBrush: Boolean;
    case Boolean of
      False: (Color: TColor);
      True: (Brush: TBrush);
  end;

  { TLineInfoList }

  TLineInfoList = class(TcxDataList)
  private
    FLocked: Boolean;
    function GetItem(Index: Integer): PLineInfo;
  public
    constructor Create;
    function Add(const ARect: TRect; ABrush: TBrush): Integer; overload;
    function Add(const ARect: TRect; AColor: TColor): Integer; overload;
    function Add(X, Y, AWidth, AHeight: Integer; ABrush: TBrush): Integer; overload;
    function Add(X, Y, AWidth, AHeight: Integer; AColor: TColor): Integer; overload;
    procedure RightToLeftConversion(const AClientBounds: TRect);

    property Items[Index: Integer]: PLineInfo read GetItem; default;
    property Locked: Boolean read FLocked write FLocked;
  end;

  { TIndentInfo }

  PIndentInfo = ^TIndentInfo;
  TIndentInfo = record
    Bounds: TRect;
    ViewParams: TcxViewParams;
  end;

  { TIndentInfoList }

  TIndentInfoList = class(TcxDataList)
  private
    function GetItem(Index: Integer): PIndentInfo;
  public
    constructor Create;
    function Add(const ABounds: TRect; const AViewParams: TcxViewParams): Integer;
    procedure RightToLeftConversion(const AClientBounds: TRect);

    property Items[Index: Integer]: PIndentInfo read GetItem; default;
  end;

  { TIndentRectInfo }

  PIndentRectInfo = ^TIndentRectInfo;
  TIndentRectInfo = record
    IsCategory: Boolean;
		Size: TSize;
    ViewParams: TcxViewParams;
		Underline: Boolean;
  end;

  { TIndentRectInfoList }

  TIndentRectInfoList = class(TcxDataList)
  private
    function GetItem(Index: Integer): PIndentRectInfo;
  public
    constructor Create;
    function Add(const ASize: TSize; AIsCategory, AUnderline: Boolean; const AViewParams: TcxViewParams): Integer;
    property Items[Index: Integer]: PIndentRectInfo read GetItem; default;
  end;

  { TViewRects }

  TViewRects = class
  public
    BandRects: TcxRectList;
    EmptyRects: TcxRectList;
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure RightToLeftConversion(const AClientBounds: TRect);
  end;

function Max(A, B: Integer): Integer;
function Min(A, B: Integer): Integer;

function cxCreateHalftoneBrush(AColor1, AColor2: TColor): TBrush;

implementation

uses
  SysUtils, cxGeometry;

function Max(A, B: Integer): Integer;
begin
  if A > B then Result := A else Result := B;
end;

function Min(A, B: Integer): Integer;
begin
  if A < B then Result := A else Result := B;
end;

{ TcxDataList }

constructor TcxDataList.Create(ARecordSize: Integer);
begin
  FDelta := 1024;
  FRecordSize := ARecordSize;
end;

destructor TcxDataList.Destroy;
begin
  FreeMem(FData, FAllocated * FRecordSize);
  inherited Destroy;
end;

procedure TcxDataList.Clear;
begin
  FCount := 0;
end;

procedure TcxDataList.CheckCapacity;
begin
  if FCount = FAllocated then
  begin
    Inc(FAllocated, FDelta);
    ReallocMem(FData, FAllocated * FRecordSize);
  end;
end;

function TcxDataList.Get(Index: Integer): Pointer;
begin
  if (Index < 0) or (Index >= FCount) then
    raise EdxException.CreateFmt('Error %s: Invalid index %d', [ClassName, Index]);
  Result := ShiftPointer(FData, Index * FRecordSize);
end;

{ TcxRectList }

constructor TcxRectList.Create;
begin
  inherited Create(SizeOf(TRect));
end;

procedure TcxRectList.Assign(Source: TcxRectList);
begin
  if Source.FAllocated > FAllocated then
  begin
    FAllocated := Source.FAllocated;
    ReallocMem(FData, FAllocated * SizeOf(TRect));
  end;
  FCount := Source.Count;
  Move(Source.FData^, FData^, FCount * SizeOf(TRect));
end;

function TcxRectList.Add(const R: TRect): Integer;
begin
  CheckCapacity;
  Result := FCount;
  Inc(FCount);
  PRect(Get(Result))^ := R;
end;

procedure TcxRectList.Clear;
begin
  inherited Clear;
  FIsRightToLeftConverted := False;
end;

function TcxRectList.GetRect(Index: Integer): TRect;
begin
  Result := PRect(Get(Index))^;
end;

procedure TcxRectList.SetRect(Index: Integer; const Value: TRect);
begin
  PRect(Get(Index))^ := Value;
end;

procedure TcxRectList.DoRightToLeftConversion(const AClientBounds: TRect);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Rects[I] := TdxRightToLeftLayoutConverter.ConvertRect(Rects[I], AClientBounds);
end;

procedure TcxRectList.RightToLeftConversion(const AClientBounds: TRect);
begin
  if not IsRightToLeftConverted then
  begin
    DoRightToLeftConversion(AClientBounds);
    FIsRightToLeftConverted := True;
  end;
end;

{ TRectScaler }

constructor TRectScaler.Create;
begin
  inherited Create;
  FScaledRects := TcxRectList.Create;
end;

destructor TRectScaler.Destroy;
begin
  FreeAndNil(FScaledRects); // not Free!!!
  inherited Destroy;
end;

function TRectScaler.GetSummaryParam: TScaleParams;
var
  I: Integer;
  P: TScaleParams;
begin
  FillChar(Result, SizeOf(TScaleParams), 0);
  for I := 0 to Count - 1 do
  begin
    P := PScaleParams(List[I])^;
    Inc(Result.Width, P.Width);
    Inc(Result.MinWidth, P.MinWidth);
    Inc(Result.FixedWidth, P.FixedWidth);
  end;
end;

procedure TRectScaler.Add(AWidth: Integer);
begin
  Add(AWidth, 0, 0);
end;

procedure TRectScaler.Add(AWidth, AMinWidth: Integer);
begin
  Add(AWidth, AMinWidth, 0);
end;

procedure TRectScaler.Add(AWidth, AMinWidth, AFixedWidth: Integer);
var
  P: PScaleParams;
begin
  New(P);
  P.Width := AWidth;
  P.MinWidth := AMinWidth;
  P.FixedWidth := AFixedWidth;
  inherited Add(P);
end;

procedure TRectScaler.Clear;
var
  I: Integer;
begin
  FreeAndNil(FScaledRects);
  for I := 0 to Count - 1 do
    FreeMem(List[I], SizeOf(TScaleParams));
  inherited Clear;
end;

procedure TRectScaler.CalcRect(const Rect: TRect);
var
  AScaleParams: TScaleParams;
  I, ALeft, H: Integer;
  R: TRect;
begin
  FScaledRects.Clear;
  if Count = 0 then Exit;
  ALeft := Rect.Left;
  H := Rect.Bottom - Rect.Top;
  for I := 0 to Count -1 do
  begin
    AScaleParams := PScaleParams(Items[I])^;
    R := cxRectBounds(ALeft, Rect.Top, AScaleParams.Width, H);
    if R.Right - R.Left < AScaleParams.MinWidth then
      R.Right := R.Left + AScaleParams.MinWidth;
    if I = Count - 1 then R.Right := Rect.Right;
    if R.Right >= Rect.Right then
    begin
      R.Right := Rect.Right;
      FScaledRects.Add(R);
      break;
    end
    else
      FScaledRects.Add(R);
    Inc(ALeft, R.Right - R.Left + AScaleParams.FixedWidth);
  end;
end;

procedure TRectScaler.ScaleRect(const Rect: TRect);
var
  ASummary, AScaleParams: TScaleParams;
  I, W, ALeft, H: Integer;
  ACoeff: Double;
  R: TRect;
begin
  FScaledRects.Clear;
  if Count = 0 then Exit;
  ASummary := GetSummaryParam;
  if ASummary.Width > 0 then
  begin
    W := (Rect.Right - Rect.Left) - ASummary.FixedWidth;
    ACoeff := W / ASummary.Width;
    ALeft := Rect.Left;
    H := Rect.Bottom - Rect.Top;
    for I := 0 to Count -1 do
    begin
      AScaleParams := PScaleParams(Items[I])^;
      R := cxRectBounds(ALeft, Rect.Top, Round(ACoeff * AScaleParams.Width), H);
      if R.Right - R.Left < AScaleParams.MinWidth then
        R.Right := R.Left + AScaleParams.MinWidth;
      if I = Count - 1 then R.Right := Rect.Right;
      if R.Right >= Rect.Right then
      begin
        R.Right := Rect.Right;
        FScaledRects.Add(R);
        break;
      end
      else
        FScaledRects.Add(R);
      Inc(ALeft, R.Right - R.Left + AScaleParams.FixedWidth);
    end;
  end;
end;

{ TLineInfoList }

constructor TLineInfoList.Create;
begin
  inherited Create(SizeOf(TLineInfo));
end;

function TLineInfoList.Add(const ARect: TRect; ABrush: TBrush): Integer;
begin
  if not FLocked then
  begin
    CheckCapacity;
    Result := FCount;
    Inc(FCount);
    with PLineInfo(Get(Result))^ do
    begin
      Rect := ARect;
      IsBrush := True;
      Brush := ABrush;
    end;
  end
  else Result := -1;
end;

function TLineInfoList.Add(const ARect: TRect; AColor: TColor): Integer;
begin
  if not FLocked then
  begin
    CheckCapacity;
    Result := FCount;
    Inc(FCount);
    with PLineInfo(Get(Result))^ do
    begin
      Rect := ARect;
      IsBrush := False;
      Color := AColor;
    end;
  end
  else Result := -1;
end;

function TLineInfoList.Add(X, Y, AWidth, AHeight: Integer; ABrush: TBrush): Integer;
begin
  Result := Add(cxRectBounds(X, Y, AWidth, AHeight), ABrush);
end;

function TLineInfoList.Add(X, Y, AWidth, AHeight: Integer; AColor: TColor): Integer;
begin
  Result := Add(cxRectBounds(X, Y, AWidth, AHeight), AColor);
end;

function TLineInfoList.GetItem(Index: Integer): PLineInfo;
begin
  Result := PLineInfo(Get(Index));
end;

procedure TLineInfoList.RightToLeftConversion(const AClientBounds: TRect);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I]^.Rect := TdxRightToLeftLayoutConverter.ConvertRect(Items[I]^.Rect, AClientBounds);
end;

{ TIndentInfoList }

constructor TIndentInfoList.Create;
begin
  inherited Create(SizeOf(TIndentInfo));
end;

function TIndentInfoList.Add(const ABounds: TRect;
  const AViewParams: TcxViewParams): Integer;
begin
  CheckCapacity;
  Result := FCount;
  Inc(FCount);
  with PIndentInfo(Get(Result))^ do
  begin
    Bounds := ABounds;
    ViewParams := AViewParams;
  end;
end;

function TIndentInfoList.GetItem(Index: Integer): PIndentInfo;
begin
  Result := PIndentInfo(Get(Index));
end;

procedure TIndentInfoList.RightToLeftConversion(const AClientBounds: TRect);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I]^.Bounds := TdxRightToLeftLayoutConverter.ConvertRect(Items[I]^.Bounds, AClientBounds)
end;

{ TIndentRectInfoList }

constructor TIndentRectInfoList.Create;
begin
  inherited Create(SizeOf(TIndentRectInfo));
end;

function TIndentRectInfoList.Add(const ASize: TSize; AIsCategory,
  AUnderline: Boolean; const AViewParams: TcxViewParams): Integer;
begin
  CheckCapacity;
  Result := FCount;
  Inc(FCount);
  with PIndentRectInfo(Get(Result))^ do
  begin
    IsCategory := AIsCategory;
    Size := ASize;
    ViewParams := AViewParams;
    Underline := AUnderline;
  end;
end;

function TIndentRectInfoList.GetItem(Index: Integer): PIndentRectInfo;
begin
  Result := PIndentRectInfo(Get(Index));
end;

{ TViewRects }

constructor TViewRects.Create;
begin
  BandRects := TcxRectList.Create;
  EmptyRects := TcxRectList.Create;
end;

destructor TViewRects.Destroy;
begin
  BandRects.Free;
  EmptyRects.Free;
  inherited Destroy;
end;

procedure TViewRects.Clear;
begin
  BandRects.Clear;
  EmptyRects.Clear;
end;

procedure TViewRects.RightToLeftConversion(const AClientBounds: TRect);
begin
  BandRects.RightToLeftConversion(AClientBounds);
  EmptyRects.RightToLeftConversion(AClientBounds);
end;

function cxCreateHalftoneBrush(AColor1, AColor2: TColor): TBrush;
var
  ABitmap: TBitmap;
  I, J: Integer;
const
  APattern: array[0..7] of Word =
    ($00AA, $0055, $00AA, $0055, $00AA, $0055, $00AA, $0055);
begin
  Result := TBrush.Create;
  ABitmap := cxCreateBitmap(TSize(cxPoint(8, 8)), pfDevice);
  for I := 0 to 7 do
    for J := 0 to 7 do
    begin
      if ((APattern[I] and (1 shl J)) <> 0) then
        ABitmap.Canvas.Pixels[I, J] := AColor1
      else
        ABitmap.Canvas.Pixels[I, J] := AColor2;
    end;
  Result.Bitmap := ABitmap;
end;

end.
