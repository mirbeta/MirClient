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

unit dxBkGnd;

interface

{$I cxVer.inc}

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Windows, Classes, SysUtils, Controls, Graphics, dxBase, dxPSGlbl, dxPSUtl,
  cxGraphics, dxPSReportRenderCanvas;

type
  TdxBackgroundMode = (bmNone, bmBrush, bmBrushBitmap, bmPicture);
  TdxBackgroundParam = (bpBkColor, bpBrush, bpMode, bpPictureMode, bpPicture);
  TdxBackgroundParams = set of TdxBackgroundParam;
  TdxBackgroundClass = class of TdxBackground;
  TdxPaintSequence = (psBefore, psAfter);

  TdxBackgroundChangeEvent = procedure(Sender: TObject; AChangeWhat: TdxBackgroundParams) of object;

  TCustomdxBackgroundPaintEvent = procedure(Sender: TObject;
    ACanvas: TdxPSReportRenderCustomCanvas; ARect: TRect;
    ASequence: TdxPaintSequence; var ADone: Boolean) of object;

  TCustomdxBackgroundPaintExEvent = procedure(Sender: TObject;
    ACanvas: TdxPSReportRenderCustomCanvas; ARect: TRect;
    ASequence: TdxPaintSequence; PixelsNumerator, PixelsDenominator: Integer;
    var ADone: Boolean) of object;

  { TdxBackground }

  TdxBackground = class(TdxBaseObject)
  private
    FBkColor: TColor;
    FBrush: TBrush;
    FIsRepaintNeeded: Boolean;
    FMode: TdxBackgroundMode;
    FPicture: TGraphic;
    FPictureMode: TdxPicturePaintMode;
    FOnApply: TNotifyEvent;
    FOnChange: TdxBackgroundChangeEvent;
    FOnPaint: TCustomdxBackgroundPaintEvent;
    FOnPaintEx: TCustomdxBackgroundPaintExEvent;
    function GetIsNeedDrawBackground: Boolean;
    function GetPicture: TGraphic;
    function GetStretchDrawMode: TdxPicturePaintMode;
    procedure SetBkColor(Value: tColor);
    procedure SetBrush(Value: TBrush);
    procedure SetMode(Value: TdxBackgroundMode);
    procedure SetPicture(Value: TGraphic);
    procedure SetPictureMode(Value: TdxPicturePaintMode);
  protected
    procedure DoApply; dynamic;
    procedure DoAssign(Source: TdxBaseObject); override;
    procedure DoChange(AChangeWhats: TdxBackgroundParams); dynamic;
    procedure DoPaint(ACanvas: TdxPSReportRenderCustomCanvas; Rect: TRect;
      Sequence: TdxPaintSequence;  var ADone: Boolean); virtual;
    function DoPaintEx(ACanvas: TdxPSReportRenderCustomCanvas; Rect: TRect;
      Sequence: TdxPaintSequence; PixelsNumerator, PixelsDenominator: Integer): Boolean; virtual;
    procedure DoRestoreDefaults; override;
    //
    function RepaintNeeded(AChangeWhats: TdxBackgroundParams): Boolean; virtual;
    procedure BrushChanged(Sender: TObject);
    procedure LockUpdate(ALockState : TdxLockState); override;
    procedure PictureChanged(Sender: TObject);
    //
    property OnApply: TNotifyEvent read FOnApply write FOnApply;
    property StretchDrawMode: TdxPicturePaintMode read GetStretchDrawMode;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Clear; virtual;
    function IsEmpty: Boolean; override;
    function IsEqual(ABaseObject: TdxBaseObject): Boolean; override;
    procedure Paint(ACanvas: TCanvas; const R: TRect); virtual;
    procedure PaintEx(ACanvas: TdxPSReportRenderCustomCanvas;
      const R: TRect; APixelsNumerator, APixelsDenominator: Integer); virtual;
    function SetupEffects: Boolean;

    property IsNeedDrawBackground: Boolean read GetIsNeedDrawBackground;
    property IsRepaintNeeded: Boolean read FIsRepaintNeeded;
    property OnChange: TdxBackgroundChangeEvent read FOnChange write FOnChange;
    property OnPaint: TCustomdxBackgroundPaintEvent read FOnPaint write FOnPaint;
    property OnPaintEx: TCustomdxBackgroundPaintExEvent read FOnPaintEx write FOnPaintEx;
  published
    property BkColor: TColor read FBkColor write SetBkColor default clWhite;
    property Brush: TBrush read FBrush write SetBrush;
    property Mode: TdxBackgroundMode read FMode write SetMode default bmNone;
    property Picture: TGraphic read GetPicture write SetPicture;
    property PictureMode: TdxPicturePaintMode read FPictureMode write SetPictureMode default ppmCenter;
  end;

const
  cwAll: TdxBackgroundParams = [bpBkColor..bpPicture];
implementation

uses
  dxFEFDlg;

{ TdxBackground }

constructor TdxBackground.Create;
begin
  inherited Create;
  FBkColor := clWhite;
  FBrush := TBrush.Create;
  FBrush.OnChange := BrushChanged;
  FMode := bmNone;
  FPictureMode := ppmCenter;
  FPicture := TBitmap.Create;
  FPicture.OnChange := PictureChanged;
end;

destructor TdxBackground.Destroy;
begin
  FreeAndNil(FPicture);
  FreeAndNil(FBrush);
  inherited;
end;

procedure TdxBackground.Clear;
begin
  Brush.Color := clWhite;
  Mode := bmNone;
  Picture := nil;
end;

function TdxBackground.IsEmpty: Boolean;
begin
  case Mode of
     bmBrush:
       Result := Brush.Style = bsClear;
     bmBrushBitmap, bmPicture:
       Result := (Picture = nil) or Picture.Empty;
    else //bmNone
      Result := True;
  end;
end;

function TdxBackground.IsEqual(ABaseObject: TdxBaseObject): Boolean;
begin
  Result := inherited IsEqual(ABaseObject);
  if Result then
    with TdxBackground(ABaseObject) do
     Result :=
       (Self.BkColor = BkColor) and
       (Self.Mode = Mode) and
       (Self.PictureMode = PictureMode) and
       dxAreBrushesEqual(Self.Brush, Brush) and
       dxAreGraphicsEqual(Self.Picture, Picture);
end;

procedure TdxBackground.Paint(ACanvas: TCanvas; const R: TRect);
var
  ARenderCanvas: TdxPSReportRenderCanvas;
begin
  ARenderCanvas := TdxPSReportRenderCanvas.Create(ACanvas);
  try
    PaintEx(ARenderCanvas, R, 1, 1);
  finally
    ARenderCanvas.Free;
  end;
end;

procedure TdxBackground.PaintEx(ACanvas: TdxPSReportRenderCustomCanvas;
  const R: TRect; APixelsNumerator, APixelsDenominator: Integer);
var
  APattern: TcxBitmap;
begin
  if DoPaintEx(ACanvas, R, psBefore, APixelsNumerator, APixelsDenominator) then
    Exit;

  case Mode of
    bmNone:
      ;
    bmBrush:
      case Brush.Style of
        bsSolid:
          ACanvas.FillRect(R, Brush.Color);
        bsClear:
          ;
        else
        begin
          APattern := TcxBitmap.CreateSize(8, 8);
          try
            APattern.Canvas.Brush.Assign(Brush);
            APattern.Canvas.FillRect(APattern.ClientRect);
            ACanvas.DrawPicture(APattern, R, ppmTile, APixelsNumerator, APixelsDenominator);
          finally
            APattern.Free;
          end;
        end;
      end;

    bmPicture, bmBrushBitmap:
      ACanvas.DrawPicture(Picture, R, StretchDrawMode, APixelsNumerator, APixelsDenominator);
  end;
  DoPaintEx(ACanvas, R, psAfter, APixelsNumerator, APixelsDenominator);
end;

function TdxBackground.SetupEffects: Boolean;
begin
  Result := dxFEFDialog(Self);
end;

procedure TdxBackground.DoAssign(Source: TdxBaseObject);
begin
  inherited DoAssign(Source);
  with Source as TdxBackground do
  begin
    Self.BkColor := BkColor;
    Self.Brush := Brush;
    Self.Mode := Mode;
    Self.PictureMode := PictureMode;
    Self.Picture := Picture;
  end;
end;

procedure TdxBackground.DoRestoreDefaults;
begin
  inherited DoRestoreDefaults;
  FBkColor := clWhite;
  FMode := bmNone;
  FPictureMode := ppmCenter;
end;

procedure TdxBackground.DoApply;
begin
  if Assigned(FOnApply) then FOnApply(Self);
end;

procedure TdxBackground.DoChange(AChangeWhats: TdxBackgroundParams);
begin
  if not IsLocked then
  begin
    FIsRepaintNeeded := RepaintNeeded(AChangeWhats);
    if Assigned(FOnChange) then
      FOnChange(Self, AChangeWhats);
  end;
end;

procedure TdxBackground.DoPaint(ACanvas: TdxPSReportRenderCustomCanvas;
  Rect: TRect; Sequence: TdxPaintSequence; var ADone: Boolean);
begin
  if Assigned(OnPaint) then
    OnPaint(Self, ACanvas, Rect, Sequence, ADone);
end;

function TdxBackground.DoPaintEx(ACanvas: TdxPSReportRenderCustomCanvas;
  Rect: TRect; Sequence: TdxPaintSequence; PixelsNumerator, PixelsDenominator: Integer): Boolean;
begin
  Result := False;
  if Assigned(OnPaintEx) then
    OnPaintEx(Self, ACanvas, Rect, Sequence, PixelsNumerator, PixelsDenominator, Result);
  if not Result then
    DoPaint(ACanvas, Rect, Sequence, Result);
end;

procedure TdxBackground.LockUpdate(ALockState : TdxLockState);
begin
  if ALockState = lsUnLock then
    DoChange(cwAll);
  inherited LockUpdate(ALockState);
end;

function TdxBackground.RepaintNeeded(AChangeWhats: TdxBackgroundParams): Boolean;
begin
  if bpMode in AChangeWhats then
    Result := True
  else
    case Mode of
      bmBrush:
        Result := (bpBrush in AChangeWhats) or ((bpBkColor in AChangeWhats) and (Brush.Style > bsClear));
      bmBrushBitmap:
        Result := bpPicture in AChangeWhats;
      bmPicture:
        Result := [bpPicture, bpPictureMode] * AChangeWhats <> [];
    else
      Result := False;
    end;
end;

procedure TdxBackground.BrushChanged(Sender: TObject);
begin
  DoChange([bpBrush]);
end;

procedure TdxBackground.PictureChanged(Sender: TObject);
begin
  DoChange([bpPicture]);
end;

procedure TdxBackground.SetBkColor(Value: tColor);
begin
  if FBkColor <> Value then
  begin
    FBkColor := Value;
    DoChange([bpBkColor]);
  end;
end;

procedure TdxBackground.SetBrush(Value: TBrush);
begin
  FBrush.Assign(Value);
end;

procedure TdxBackground.SetMode(Value: TdxBackgroundMode);
begin
  if FMode <> Value then
  begin
    FMode := Value;
    DoChange([bpMode]);
  end;
end;

function TdxBackground.GetIsNeedDrawBackground: Boolean;
begin
  case Mode of
    bmNone:
      Result := True;
    bmPicture:
      Result := PictureMode in [ppmCenter, ppmProportional];
    else
      Result := False;
  end;
end;

function TdxBackground.GetStretchDrawMode: TdxPicturePaintMode;
begin
  if Mode = bmBrushBitmap then
    Result := ppmTile
  else
    Result := PictureMode;
end;

function TdxBackground.GetPicture: TGraphic;
begin
  if FPicture = nil then
    FPicture := TBitmap.Create;
  Result := FPicture;
end;

procedure TdxBackground.SetPicture(Value: TGraphic);
begin
  if Value <> nil then
  begin
    Picture.Assign(Value);
    TBitmap(Picture).HandleType := bmDIB;
  end
  else
    if FPicture <> nil then
    begin
      TBitmap(FPicture).FreeImage;
      TBitmap(FPicture).ReleaseHandle;
    end;
end;

procedure TdxBackground.SetPictureMode(Value: TdxPicturePaintMode);
begin
  if FPictureMode <> Value then
  begin
    FPictureMode := Value;
    DoChange([bpPictureMode]);
  end;
end;

initialization
  RegisterClass(TBitmap);

end.

