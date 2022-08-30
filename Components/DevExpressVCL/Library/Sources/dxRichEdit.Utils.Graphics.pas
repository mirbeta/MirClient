{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressRichEditControl                                   }
{                                                                    }
{           Copyright (c) 2000-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSRICHEDITCONTROL AND ALL        }
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

unit dxRichEdit.Utils.Graphics;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, Classes, SysUtils, Windows, Graphics, Generics.Defaults, Generics.Collections,
  Controls,

  dxCoreClasses, cxGeometry, cxGraphics, dxCoreGraphics, dxGDIPlusAPI, dxGDIPlusClasses,

  dxRichEdit.DocumentLayout.UnitConverter;

type
  TdxTransformMatrix = TdxGPMatrix;
  TdxGraphics = class;
  TdxGraphicsUnit = TdxGraphicUnit;

  { TdxGraphicsDpi }

  TdxGraphicsDpi = class
  public const
    Display = 75.0;
    Inch = 1.0;
    Document = 300.0;
    Millimeter = 25.4;
    Point = 72.0;
    HundredthsOfAnInch = 100.0;
    TenthsOfAMillimeter = 254.0;
    Twips = 1440.0;
    EMU = 914400.0;
    DeviceIndependentPixel = 96.0;
  strict private
    class var
      FPixel: Single;
    class constructor Initialize;
  public
    class function GetGraphicsDpi(AGraphics: TdxGraphics): Single; static;
    class function UnitToDpi(AUnit: TdxGraphicsUnit): Single; static;
    class function DpiToUnit(ADpi: Single): TdxGraphicsUnit; static;

    class property Pixel: Single read FPixel;
  end;



  { TdxGraphics }

  TdxGraphics = class(TdxGPCanvas)
  public
    type
      TClipState = record
        Region: GpRegion;
        Rgn: HRGN;
      end;
  strict private
    function GetPageUnit: TdxGraphicsUnit;
    procedure SetPageUnit(const AValue: TdxGraphicsUnit);
    function GetPageScale: Single;
    procedure SetPageScale(const AValue: Single);
    function GetClipBounds: TdxRectF;
    function GetPixelOffsetMode: TdxGpPixelOffsetMode;
    procedure SetPixelOffsetMode(const Value: TdxGpPixelOffsetMode);
    procedure SetClipHdcCore(AHdc: HDC; const ABounds: TRect);
  public
    constructor Create; reintroduce; overload;
    constructor CreateFromHdc(AHdc: HDC);
    constructor CreateFromHWnd(AWnd: HWND);
    constructor CreateFromImage(AImage: TdxSmartImage);
    procedure ResetTransform;

    class function CalcTextSizeInt(G: TdxGraphics; const S: string; AFont: TdxGPFont; AStrFormat: TdxGPStringFormat;
      AMaxWidth: Integer): TSize; overload;
    class function CalcTextSizeInt(G: TdxGraphics; const S: string; AFont: TdxGPFont; AStrFormat: TdxGPStringFormat;
      AMaxWidth: Integer; AMaxHeight: Integer): TSize; overload;
    class function CalcTextSizeInt(G: TdxGraphics; const S: string; AFont: TdxGPFont; AStrFormat: TdxGPStringFormat;
      AMaxWidth: Integer; AMaxHeight: Integer; out AIsCropped: Boolean): TSize; overload;
    class function CalcTextSize(G: TdxGraphics; const S: string; AFont: TdxGPFont; AStrFormat: TdxGPStringFormat;
      AMaxWidth: Integer): TdxSizeF; overload; virtual;
    class function CalcTextSize(G: TdxGraphics; const S: string; AFont: TdxGPFont; AStrFormat: TdxGPStringFormat;
      AMaxWidth: Integer; AMaxHeight: Integer): TdxSizeF; overload; virtual;
    class function CalcTextSize(G: TdxGraphics; const S: string; AFont: TdxGPFont; AStrFormat: TdxGPStringFormat;
      AMaxWidth: Integer; AMaxHeight: Integer; out AIsCropped: Boolean): TdxSizeF; overload; virtual;
    procedure DrawLines(APen: TdxGPPen; const APoints: TArray<TdxPointF>);
    procedure DrawVString(const AText: string; AFont: TdxGPFont; AForeBrush: TdxGPCustomBrush;
      const ARect: TRect; AStrFormat: TdxGPStringFormat; AAngle: Integer);
    procedure FillPolygon(ABrush: TdxGPBrush; const APoints: TArray<TdxPointF>); overload;
    procedure FillPolygon(ABrush: TdxGPBrush; const APoints: TArray<TdxPointF>; AFillMode: FillMode); overload;

    procedure ExcludeClip(const R: TRect);
    procedure SetClip(const ARegion: TdxGPRegion; ADestroyRegion: Boolean = True); overload;
    procedure SetClip(const R: TdxRectF; AAdjustHdc: Boolean = False); overload;
    procedure SetClip(const R: TRect; AAdjustHdc: Boolean = False); overload;
    procedure SetClipHdc(const ABounds: TRect);
    procedure MultiplyTransform(AMatrix: TdxGPMatrix);

    procedure Save;
    procedure Restore;
    function GetClipState: TClipState;
    function SaveStateAndSetClip(const ABounds: TRect): TClipState;
    procedure RestoreClipState(var AClipState: TClipState);

    property ClipBounds: TdxRectF read GetClipBounds;
    property PageUnit: TdxGraphicsUnit read GetPageUnit write SetPageUnit;
    property PageScale: Single read GetPageScale write SetPageScale;
    property PixelOffsetMode: TdxGpPixelOffsetMode read GetPixelOffsetMode write SetPixelOffsetMode;
    property Transform: TdxGPMatrix read GetWorldTransform write SetWorldTransform;
  end;

  { TdxLinearGradientBrush }

  TdxLinearGradientBrush = class(TdxGPCustomBrush)
  strict private
    FColor1: TdxAlphaColor;
    FColor2: TdxAlphaColor;
    FLinearGradientMode: TdxGpLinearGradientMode;
  protected
    procedure DoCreateHandle(out AHandle: GpHandle); override;
  public
    constructor Create(const ABounds: TRect; AColor1, AColor2: TdxAlphaColor; ALinearGradientMode: TdxGpLinearGradientMode); reintroduce;
    procedure SetSigmaBellShape(AFocus, AScale: Single);
  end;

  { TdxGPColorPen }

  TdxGPColorPen = class sealed (TdxGPPen)
  private
    FColor: TdxAlphaColor;
    FWidth: Single;
    FHasPattern: Boolean;
    function GetAlignment: TdxGpPenAlignment;
    function GetDashPattern: TArray<Single>;
    function SamePatterns(AHandle1, AHandle2: GpHandle): Boolean;
    procedure SetAlignment(const Value: TdxGpPenAlignment);
    procedure SetDashPattern(const Value: TArray<Single>);
  protected
    procedure DoCreateHandle(out AHandle: GpHandle); override;
    procedure DoFreeHandle(AHandle: GpHandle); override;
    procedure DoSetDashStyle(AHandle: GpHandle);
  public
    constructor Create(AColor: TdxAlphaColor); reintroduce; overload;
    constructor Create(AColor: TdxAlphaColor; AWidth: Single); reintroduce; overload;
    procedure Assign(ASource: TdxGPCustomGraphicObject); override;
    function Equals(Obj: TObject): Boolean; override;
    function GetHashCode: Integer; override;
    property Alignment: TdxGpPenAlignment read GetAlignment write SetAlignment;
    property DashPattern: TArray<Single> read GetDashPattern write SetDashPattern;
  end;


  { TdxHdcDpiModifier }

  TdxHdcDpiModifier = class
  strict private
    FGraphics: TdxGraphics;
    FViewPort: TSize;
    FDpi: Integer;
    FOldWindowExt: TSize;
    FOldViewportExt: TSize;
    FOldMapMode: Integer;
  protected
    function GetDpi: Integer; virtual;
    procedure ApplyHDCDpi; virtual;
    procedure RestoreHDCDpi; virtual;

    property Dpi: Integer read GetDpi;
  public
    constructor Create(AGraphics: TdxGraphics; const AViewPort: TSize; ADpi: Integer);
    destructor Destroy; override;
  end;

  { TdxGraphicsToLayoutUnitsModifier }

  TdxGraphicsToLayoutUnitsModifier = class
  strict private
    FGraphics: TdxGraphics;
    FUnitConverter: TdxDocumentLayoutUnitConverter;
    FOldUnit: TdxGraphicsUnit;
    FOldScale: Single;
    FHdcDpiModifier: TdxHdcDpiModifier;
    FOldMatrix: TdxGPMatrix;
  private
    procedure Apply;
    procedure Restore;
  public
    constructor Create(AGraphics: TdxGraphics; AUnitConverter: TdxDocumentLayoutUnitConverter);
    destructor Destroy; override;
  end;

  { TdxHdcOriginModifier }

  TdxHdcOriginModifier = class
  public type
    TMode = (Replace, Combine);
  private
    FGraphics: TdxGraphics;
    FOldOrigin: TPoint;
  protected
    procedure RestoreHDC; virtual;
  public
    constructor Create(AGraphics: TdxGraphics; const ANewOrigin: TPoint; AZoomFactor: Single; AMode: TMode = TMode.Replace);
    destructor Destroy; override;

    procedure SetHDCOrigin(const ANewOrigin: TPoint; AZoomFactor: Single; AMode: TMode); virtual;
  end;

  { TdxHdcZoomModifier }

  TdxHdcZoomModifier = class
  private
    FGraphics: TdxGraphics;
    FOldWindowExtent: TSize;
  protected
    procedure RestoreHDC; virtual;
  public
    constructor Create(AGraphics: TdxGraphics; AZoomFactor: Single);
    destructor Destroy; override;

    procedure ZoomHDC(AZoomFactor: Single); virtual;
  end;

function CreateImageFromResource(AInstance: HINST; const AResourceName: string): TdxSmartImage;

implementation

uses
  Math, dxCore, dxTypeHelpers,
  dxRichEdit.Utils.Exceptions;

function CreateImageFromResource(AInstance: HINST; const AResourceName: string): TdxSmartImage;
var
  AStream: TResourceStream;
begin
  AStream := TResourceStream.Create(AInstance, AResourceName, RT_RCDATA);
  try
    Result := TdxSmartImage.CreateFromStream(AStream);
  finally
    AStream.Free;
  end;
end;

{ TdxGraphicsDpi }

class constructor TdxGraphicsDpi.Initialize;
var
  DC: HDC;
begin
  DC := GetDC(0);
  FPixel := GetDeviceCaps(DC, LOGPIXELSY);
  ReleaseDC(0, DC);
end;

class function TdxGraphicsDpi.GetGraphicsDpi(AGraphics: TdxGraphics): Single;
begin
  if AGraphics.PageUnit = TdxGraphicsUnit.guDisplay then
    Result := AGraphics.DpiX
  else
    Result := UnitToDpi(AGraphics.PageUnit);
end;

class function TdxGraphicsDpi.UnitToDpi(AUnit: TdxGraphicsUnit): Single;
begin
  case AUnit of
    TdxGraphicsUnit.guDocument:
      Exit(Document);
    TdxGraphicsUnit.guInch:
      Exit(Inch);
    TdxGraphicsUnit.guMillimeter:
      Exit(Millimeter);
    TdxGraphicsUnit.guPixel,
    TdxGraphicsUnit.guWorld:
      Exit(Pixel);
    TdxGraphicsUnit.guPoint:
      Exit(Point);
    else
      Exit(Display);
  end;
end;

class function TdxGraphicsDpi.DpiToUnit(ADpi: Single): TdxGraphicsUnit;
begin
  if SameValue(ADpi, Display) then
    Exit(TdxGraphicsUnit.guDisplay);
  if SameValue(ADpi, Inch) then
    Exit(TdxGraphicsUnit.guInch);
  if SameValue(ADpi, Document) then
    Exit(TdxGraphicsUnit.guDocument);
  if SameValue(ADpi, Millimeter) then
    Exit(TdxGraphicsUnit.guMillimeter);
  if SameValue(ADpi, Pixel) then
    Exit(TdxGraphicsUnit.guPixel);
  if SameValue(ADpi, Point) then
    Exit(TdxGraphicsUnit.guPoint);
  Result := TdxGraphicsUnit.guDisplay;
  TdxRichEditExceptions.ThrowInternalException;
end;

{ TdxGraphics }

constructor TdxGraphics.Create;
begin
  CreateFromHWnd(0);
end;

constructor TdxGraphics.CreateFromHWnd(AWnd: HWND);
var
  AHandle: GpGraphics;
begin
  GdipCheck(GdipCreateFromHWND(AWnd, AHandle));
  inherited Create(AHandle);
end;

constructor TdxGraphics.CreateFromHdc(AHdc: HDC);
begin
  inherited Create(AHdc);
end;

constructor TdxGraphics.CreateFromImage(AImage: TdxSmartImage);
var
  AGraphics: GpGraphics;
begin
  GdipCheck(GdipGetImageGraphicsContext(AImage.Handle, AGraphics));
  inherited Create(AGraphics);
end;

function TdxGraphics.GetPageUnit: TdxGraphicsUnit;
var
  AUnit: Integer;
begin
  AUnit := 0;
  GdipCheck(GdipGetPageUnit(Handle, TdxGraphicUnit(AUnit)));
  Result := TdxGraphicsUnit(AUnit);
end;

function TdxGraphics.GetPixelOffsetMode: TdxGpPixelOffsetMode;
begin
  GdipCheck(GdipGetPixelOffsetMode(Handle, Result));
end;

procedure TdxGraphics.MultiplyTransform(AMatrix: TdxGPMatrix);
begin
  ModifyWorldTransform(AMatrix);
end;

procedure TdxGraphics.ResetTransform;
begin
  ResetWorldTransform;
end;

class function TdxGraphics.CalcTextSizeInt(G: TdxGraphics; const S: string; AFont: TdxGPFont; AStrFormat: TdxGPStringFormat; AMaxWidth: Integer): TSize;
var
  AIsCropped: Boolean;
begin
  Result := CalcTextSizeInt(G, S, AFont, AStrFormat, AMaxWidth, MaxInt, AIsCropped);
end;

class function TdxGraphics.CalcTextSizeInt(G: TdxGraphics; const S: string; AFont: TdxGPFont;
  AStrFormat: TdxGPStringFormat; AMaxWidth: Integer; AMaxHeight: Integer): TSize;
var
  AIsCropped: Boolean;
begin
  Result := CalcTextSizeInt(G, S, AFont, AStrFormat, AMaxWidth, AMaxHeight, AIsCropped);
end;

class function TdxGraphics.CalcTextSizeInt(G: TdxGraphics; const S: string; AFont: TdxGPFont; AStrFormat: TdxGPStringFormat;
  AMaxWidth: Integer; AMaxHeight: Integer; out AIsCropped: Boolean): TSize;
var
  ASize: TdxSizeF;
begin
  ASize := CalcTextSize(G, S, AFont, AStrFormat, AMaxWidth, AMaxHeight, AIsCropped);
  Result := TSize.Round(ASize);
end;

class function TdxGraphics.CalcTextSize(G: TdxGraphics; const S: string; AFont: TdxGPFont; AStrFormat: TdxGPStringFormat;
  AMaxWidth: Integer): TdxSizeF;
var
  AMeasureString: string;
begin
  if Length(S) >= 65536 then
    AMeasureString := Copy(S, 1, 65535)
  else
    AMeasureString := S;
  Result := G.MeasureString(AMeasureString, AFont, AMaxWidth, AStrFormat);
  if Trunc(Result.Width) <> Result.Width then
    Result.Width := Result.Width + 1;
end;

class function TdxGraphics.CalcTextSize(G: TdxGraphics; const S: string; AFont: TdxGPFont; AStrFormat: TdxGPStringFormat;
  AMaxWidth: Integer; AMaxHeight: Integer): TdxSizeF;
begin
  Result := CalcTextSize(G, S, AFont, AStrFormat, AMaxWidth);
end;

class function TdxGraphics.CalcTextSize(G: TdxGraphics; const S: string; AFont: TdxGPFont; AStrFormat: TdxGPStringFormat;
  AMaxWidth: Integer; AMaxHeight: Integer; out AIsCropped: Boolean): TdxSizeF;
begin
  AIsCropped := False;
  Result := CalcTextSize(G, S, AFont, AStrFormat, AMaxWidth);
end;

procedure TdxGraphics.DrawLines(APen: TdxGPPen; const APoints: TArray<TdxPointF>);
begin
  if (Length(APoints) = 0) or (APen = nil) then
    raise EdxGdipException.Create(InvalidParameter);
  GdipDrawLines(Handle, APen.Handle, @APoints[0], Length(APoints));
end;

procedure TdxGraphics.DrawVString(const AText: string; AFont: TdxGPFont; AForeBrush: TdxGPCustomBrush;
  const ARect: TRect; AStrFormat: TdxGPStringFormat; AAngle: Integer);
var
  AMatrix: TdxGPMatrix;
  R: TRect;
begin
  if AAngle = 0 then
  begin
    DrawString(AText, AFont, AForeBrush, ARect.ToRectF, AStrFormat);
    Exit;
  end;

  R := ARect;
  SaveWorldTransform;
  AMatrix := Transform;
  try
    AMatrix.Translate(AMatrix.OffsetX + R.Left, AMatrix.OffsetX + R.Top);
    AMatrix.Rotate(AAngle);
    Transform := AMatrix;

    if AAngle = 90 then
      R.Offset(-R.Left, -R.Top - R.Width)
    else
      R.Offset(-R.Left - R.Height + 0, -R.Top);

    if (AAngle = 90) or (AAngle = 270) then
    begin
      R.Offset(0, -1);
      Inc(R.Right);
    end;

    R.InitSize(R.X, R.Y, R.Height, R.Width);
    DrawString(AText, AFont, AForeBrush, R.ToRectF, AStrFormat);
  finally
    AMatrix.Free;
    RestoreWorldTransform;
  end;
end;

procedure TdxGraphics.FillPolygon(ABrush: TdxGPBrush; const APoints: TArray<TdxPointF>);
begin
  FillPolygon(ABrush, APoints, FillMode.FillModeAlternate);
end;

procedure TdxGraphics.FillPolygon(ABrush: TdxGPBrush; const APoints: TArray<TdxPointF>; AFillMode: FillMode);
begin
  if (Length(APoints) = 0) or (ABrush = nil) then
    raise EdxGdipException.Create(InvalidParameter);
  GdipCheck(GdipFillPolygon(Handle, ABrush.Handle, @APoints[0], Length(APoints), AFillMode));
end;

procedure TdxGraphics.ExcludeClip(const R: TRect);
begin
  GdipCheck(GdipSetClipRectI(Handle, R.Left, R.Top, R.Width, R.Height, CombineModeExclude));
end;

procedure TdxGraphics.SetClip(const ARegion: TdxGPRegion; ADestroyRegion: Boolean = True);
begin
  if ARegion = nil then
    Exit;
  GdipCheck(GdipSetClipRegion(Handle, ARegion, CombineModeReplace));
  if ADestroyRegion then
    ARegion.Free;
end;

procedure TdxGraphics.SetClip(const R: TdxRectF; AAdjustHdc: Boolean = False);
begin
  GdipCheck(GdipSetClipRect(Handle, R.Left, R.Top, R.Width, R.Height, CombineModeReplace));
  if AAdjustHdc then
    SetClipHdc(TRect.Round(R));
end;

procedure TdxGraphics.SetClip(const R: TRect; AAdjustHdc: Boolean = False);
begin
  GdipCheck(GdipSetClipRectI(Handle, R.Left, R.Top, R.Width, R.Height, CombineModeReplace));
  if AAdjustHdc then
    SetClipHdc(R);
end;

procedure TdxGraphics.SetClipHdc(const ABounds: TRect);
var
  AHdc: THandle;
begin
  if (ABounds.Width < 1) or (ABounds.Height < 1) then
    Exit;
  AHdc := GetHdc;
  try
    SetClipHdcCore(AHdc, ABounds);
  finally
    ReleaseHDC(AHdc);
  end;
end;

procedure TdxGraphics.Save;
begin
  SaveClipRegion;
  SaveWorldTransform;
end;

procedure TdxGraphics.Restore;
begin
  RestoreWorldTransform;
  RestoreClipRegion;
end;

function TdxGraphics.GetClipState: TClipState;
var
  AHdc: HDC;
  ARes: Integer;
begin
  GdipCheck(GdipCreateRegion(Result.Region));
  GdipCheck(GdipGetClip(Handle, Result.Region));
  AHdc := GetHDC;
  try
    Result.Rgn := CreateRectRgn(0, 0, 0, 0);
    ARes := GetClipRgn(AHdc, Result.Rgn);
    if ARes <> 1 then
    begin
      DeleteObject(Result.Rgn);
      Result.Rgn := 0;
    end;
  finally
    ReleaseHDC(AHdc);
  end;
end;

function TdxGraphics.SaveStateAndSetClip(const ABounds: TRect): TClipState;
begin
  if (ABounds.Width < 1) or (ABounds.Height < 1) then
  begin
    Result.Region := nil;
    Result.Rgn := 0;
    Exit;
  end;
  Result := GetClipState;
  SetClip(ABounds, True);
end;

procedure TdxGraphics.RestoreClipState(var AClipState: TClipState);
var
  AHdc: HDC;
begin
  if AClipState.Region <> nil then
  begin
    GdipCheck(GdipSetClipRegion(Handle, AClipState.Region, CombineModeReplace));
    GdipCheck(GdipDeleteRegion(AClipState.Region));
    AClipState.Region := nil;
  end;
  if AClipState.Rgn <> 0 then
  begin
    AHdc := GetHDC;
    SelectClipRgn(AHdc, AClipState.Rgn);
    ReleaseHDC(AHdc);
    DeleteObject(AClipState.Rgn);
    AClipState.Rgn := 0;
  end;
end;

procedure TdxGraphics.SetPageUnit(const AValue: TdxGraphicsUnit);
begin
  GdipCheck(GdipSetPageUnit(Handle, TdxGraphicUnit(AValue)));
end;

procedure TdxGraphics.SetPixelOffsetMode(const Value: TdxGpPixelOffsetMode);
begin
  GdipCheck(GdipSetPixelOffsetMode(Handle, Value));
end;

procedure TdxGraphics.SetClipHdcCore(AHdc: HDC; const ABounds: TRect);
var
  AClipRgn: HRGN;
  AViewPortOrigin: TPoint;
  APointPair: array[0..1] of TPoint;
begin
  if (ABounds.Width < 1) or (ABounds.Height < 1) then
    Exit;
  APointPair[0] := ABounds.TopLeft;
  APointPair[1] := ABounds.BottomRight;
  LPtoDP(AHdc, APointPair, 2);
  AViewPortOrigin.Init(0, 0);
  GetViewportOrgEx(AHdc, AViewPortOrigin);
  AClipRgn := CreateRectRgn(APointPair[0].X - AViewPortOrigin.X,
    APointPair[0].Y - AViewPortOrigin.Y, APointPair[1].X - AViewPortOrigin.X, APointPair[1].Y - AViewPortOrigin.Y);
  ExtSelectClipRgn(AHdc, AClipRgn, RGN_COPY);
  DeleteObject(AClipRgn);
end;

function TdxGraphics.GetPageScale: Single;
begin
  GdipCheck(GdipGetPageScale(Handle, Result));
end;

procedure TdxGraphics.SetPageScale(const AValue: Single);
begin
  GdipCheck(GdipSetPageScale(Handle, AValue));
end;

function TdxGraphics.GetClipBounds: TdxRectF;
begin
  GdipCheck(GdipGetClipBounds(Handle, PdxGpRectF(@Result)));
  Result.Right := Result.Right + Result.Left;
  Result.Bottom := Result.Bottom + Result.Top;
end;

{ TdxHdcDpiModifier }

constructor TdxHdcDpiModifier.Create(AGraphics: TdxGraphics; const AViewPort: TSize; ADpi: Integer);
begin
  inherited Create;
  FGraphics := AGraphics;
  FViewPort := AViewPort;
  FDpi := ADpi;
  ApplyHDCDpi;
end;

function TdxHdcDpiModifier.GetDpi: Integer;
begin
  Result := FDpi;
end;

destructor TdxHdcDpiModifier.Destroy;
begin
  RestoreHDCDpi;
  inherited Destroy;
end;

procedure TdxHdcDpiModifier.ApplyHDCDpi;
var
  AGraphicsDpiX, AGraphicsDpiY: Integer;
  ADC: HDC;
begin
  AGraphicsDpiX := Round(FGraphics.DpiX);
  AGraphicsDpiY := Round(FGraphics.DpiY);
  ADC := FGraphics.GetHDC;
  try
    FOldMapMode := GetMapMode(ADC);
    SetMapMode(ADC, MM_ANISOTROPIC);
    SetWindowExtEx(ADC, Trunc(FViewPort.cx * Dpi / AGraphicsDpiX), Trunc(FViewPort.cy * Dpi / AGraphicsDpiY), @FOldWindowExt);
    SetViewportExtEx(ADC, FViewPort.cx, FViewPort.cy, @FOldViewportExt);
  finally
    FGraphics.ReleaseHDC(ADC);
  end;
end;

procedure TdxHdcDpiModifier.RestoreHDCDpi;
var
  ADC: HDC;
begin
  ADC := FGraphics.GetHDC;
  try
    SetViewportExtEx(ADC, FOldViewportExt.cx, FOldViewportExt.cy, @FOldViewportExt);
    SetWindowExtEx(ADC, FOldWindowExt.cx, FOldWindowExt.cy, @FOldWindowExt);
    SetMapMode(ADC, FOldMapMode);
  finally
    FGraphics.ReleaseHDC(ADC);
  end;
end;

{ TdxGraphicsToLayoutUnitsModifier }

constructor TdxGraphicsToLayoutUnitsModifier.Create(AGraphics: TdxGraphics; AUnitConverter: TdxDocumentLayoutUnitConverter);
begin
  Assert(AGraphics <> nil);
  Assert(AUnitConverter <> nil);
  FGraphics := AGraphics;
  FUnitConverter := AUnitConverter;
  Apply;
end;

destructor TdxGraphicsToLayoutUnitsModifier.Destroy;
begin
  Restore;
  FOldMatrix.Free;
  inherited Destroy;
end;

procedure TdxGraphicsToLayoutUnitsModifier.Apply;
begin
  FOldUnit := FGraphics.PageUnit;
  FOldScale := FGraphics.PageScale;
  FOldMatrix := FGraphics.Transform;
  FGraphics.PageUnit := TdxGraphicsUnit(FUnitConverter.GraphicsPageUnit);
  FGraphics.PageScale := FUnitConverter.GraphicsPageScale;
  FGraphics.ResetTransform;
  FHdcDpiModifier := TdxHdcDpiModifier.Create(FGraphics, TSize.Create(4096, 4096), Round(FUnitConverter.Dpi));
end;

procedure TdxGraphicsToLayoutUnitsModifier.Restore;
begin
  try
    FHdcDpiModifier.Free;
    FGraphics.PageUnit := FOldUnit;
    FGraphics.PageScale := FOldScale;
    FGraphics.ResetTransform;
    FGraphics.Transform := FOldMatrix;
  except
  end;
end;

{ TdxHdcOriginModifier }

constructor TdxHdcOriginModifier.Create(AGraphics: TdxGraphics; const ANewOrigin: TPoint;
  AZoomFactor: Single; AMode: TMode = TMode.Replace);
begin
  FGraphics := AGraphics;
  SetHDCOrigin(ANewOrigin, AZoomFactor, AMode);
end;

destructor TdxHdcOriginModifier.Destroy;
begin
  RestoreHDC;
  inherited Destroy;
end;

procedure TdxHdcOriginModifier.SetHDCOrigin(const ANewOrigin: TPoint; AZoomFactor: Single; AMode: TMode);
var
  P: TPoint;
  ANewX, ANewY: Integer;
  ADC: HDC;
begin
  ADC := FGraphics.GetHDC;
  try
    GetWindowOrgEx(ADC, FOldOrigin);
    ANewX := -Round(ANewOrigin.X / AZoomFactor);
    ANewY := -Round(ANewOrigin.Y / AZoomFactor);
    if AMode = TMode.Combine then
    begin
      Inc(ANewX, Round(FOldOrigin.X / AZoomFactor));
      Inc(ANewY, Round(FOldOrigin.Y / AZoomFactor));
    end;
    SetWindowOrgEx(ADC, ANewX, ANewY, @P);
  finally
    FGraphics.ReleaseHDC(ADC);
  end;
end;

procedure TdxHdcOriginModifier.RestoreHDC;
var
  P: TPoint;
  ADC: HDC;
begin
  ADC := FGraphics.GetHDC;
  try
    SetWindowOrgEx(ADC, FOldOrigin.X, FOldOrigin.Y, @P);
  finally
    FGraphics.ReleaseHDC(ADC);
  end;
end;

{ TdxHdcZoomModifier }

constructor TdxHdcZoomModifier.Create(AGraphics: TdxGraphics; AZoomFactor: Single);
begin
  FGraphics := AGraphics;
  ZoomHDC(AZoomFactor);
end;

destructor TdxHdcZoomModifier.Destroy;
begin
  RestoreHDC;
  inherited Destroy;
end;

procedure TdxHdcZoomModifier.ZoomHDC(AZoomFactor: Single);
var
  S: TSize;
  ADC: HDC;
begin
  ADC := FGraphics.GetHDC;
  try
    GetWindowExtEx(ADC, FOldWindowExtent);
    SetWindowExtEx(ADC, Round(FOldWindowExtent.cx / AZoomFactor), Round(FOldWindowExtent.cy / AZoomFactor), @S);
  finally
    FGraphics.ReleaseHDC(ADC);
  end;
end;

procedure TdxHdcZoomModifier.RestoreHDC;
var
  S: TSize;
  ADC: HDC;
begin
  ADC := FGraphics.GetHDC;
  try
    SetWindowExtEx(ADC, FOldWindowExtent.Width, FOldWindowExtent.Height, @S);
  finally
    FGraphics.ReleaseHDC(ADC);
  end;
end;

{ TdxLinearGradientBrush }

constructor TdxLinearGradientBrush.Create(const ABounds: TRect; AColor1,
  AColor2: TdxAlphaColor; ALinearGradientMode: TdxGpLinearGradientMode);
begin
  inherited Create;
  Assert(not ABounds.IsEmpty);
  SetTargetRect(ABounds);
  FColor1 := AColor1;
  FColor2 := AColor2;
  FLinearGradientMode := ALinearGradientMode;
end;

procedure TdxLinearGradientBrush.DoCreateHandle(out AHandle: GpHandle);
var
  R: TdxGpRectF;
begin
  R := TargetRect;
  GdipCheck(GdipCreateLineBrushFromRect(@R, FColor1, FColor2, FLinearGradientMode, WrapModeTile, AHandle));
end;

procedure TdxLinearGradientBrush.SetSigmaBellShape(AFocus, AScale: Single);
begin
  GdipCheck(GdipSetPathGradientSigmaBlend(Handle, AFocus, AScale));
end;

{ TdxGPColorPen }

constructor TdxGPColorPen.Create(AColor: TdxAlphaColor);
begin
  Create(AColor, 1.0);
end;

constructor TdxGPColorPen.Create(AColor: TdxAlphaColor; AWidth: Single);
begin
  inherited Create;
  FColor := AColor;
  FWidth := AWidth;
end;

procedure TdxGPColorPen.Assign(ASource: TdxGPCustomGraphicObject);
begin
  inherited Assign(ASource);
  if ASource is TdxGPColorPen then
  begin
    FColor := TdxGPColorPen(ASource).FColor;
    FWidth := TdxGPColorPen(ASource).FWidth;
    DashPattern := TdxGPColorPen(ASource).DashPattern;
  end;
end;

function TdxGPColorPen.GetHashCode: Integer;
var
  AWidth: Single;
  AWidthI: Integer absolute AWidth;
begin
  AWidth := FWidth;
  Result := Integer(FColor) xor AWidthI xor (Ord(FHasPattern) shl 24);
end;

procedure TdxGPColorPen.DoCreateHandle(out AHandle: GpHandle);
begin
  GdipCheck(GdipCreatePen1(FColor, FWidth, guWorld, AHandle));
end;

procedure TdxGPColorPen.DoFreeHandle(AHandle: GpHandle);
begin
  GdipCheck(GdipDeletePen(AHandle));
end;

procedure TdxGPColorPen.DoSetDashStyle(AHandle: GpHandle);
const
  Map: array[TdxGPPenStyle] of TdxGpDashStyle = (
    DashStyleSolid, DashStyleDash, DashStyleDot, DashStyleDashDot, DashStyleDashDotDot
  );
begin
  GdipCheck(GdipSetPenDashStyle(AHandle, Map[Style]));
end;

function TdxGPColorPen.Equals(Obj: TObject): Boolean;
var
  AOther: TdxGPColorPen absolute Obj;
begin
  Result := (FColor = AOther.FColor) and SameValue(FWidth, AOther.FWidth) and
    (Style = AOther.Style) and (FHasPattern = AOther.FHasPattern);
  if Result and FHasPattern then
    Result := SamePatterns(Handle, AOther.Handle);
end;

function TdxGPColorPen.GetAlignment: TdxGpPenAlignment;
begin
  GdipCheck(GdipGetPenMode(Handle, Result));
end;

function TdxGPColorPen.GetDashPattern: TArray<Single>;
var
  ACount: Integer;
begin
  GdipCheck(GdipGetPenDashCount(Handle, ACount));
  SetLength(Result, ACount);
  if ACount > 0 then
    GdipCheck(GdipGetPenDashArray(Handle, @Result[0], ACount));
  FHasPattern := ACount > 0;
end;

function TdxGPColorPen.SamePatterns(AHandle1, AHandle2: GpHandle): Boolean;
var
  APattern1, APattern2: TArray<Single>;
  I, ACount1, ACount2: Integer;
begin
  GdipCheck(GdipGetPenDashCount(AHandle1, ACount1));
  GdipCheck(GdipGetPenDashCount(AHandle2, ACount2));
  if ACount1 <> ACount2 then
    Exit(False);
  if ACount1 = 0 then
    Exit(True);
  SetLength(APattern1, ACount1);
  GdipCheck(GdipGetPenDashArray(AHandle1, @APattern1[0], ACount1));
  SetLength(APattern2, ACount1);
  GdipCheck(GdipGetPenDashArray(AHandle2, @APattern2[0], ACount1));
  for I := 0 to ACount1 - 1 do
    if APattern1[I] <> APattern2[I] then
      Exit(False);
  Result := True;
end;

procedure TdxGPColorPen.SetAlignment(const Value: TdxGpPenAlignment);
begin
  GdipCheck(GdipSetPenMode(Handle, Value));
end;

procedure TdxGPColorPen.SetDashPattern(const Value: TArray<Single>);
begin
  if Length(Value) = 0 then
    Exit;

  GdipCheck(GdipSetPenDashArray(Handle, @Value[0], Length(Value)));
  FHasPattern := True;
end;

end.
