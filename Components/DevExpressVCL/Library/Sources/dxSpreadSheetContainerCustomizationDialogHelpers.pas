{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressSpreadSheet                                       }
{                                                                    }
{           Copyright (c) 2001-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSSPREADSHEET CONTROL AND ALL    }
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

unit dxSpreadSheetContainerCustomizationDialogHelpers;

{$I cxVer.Inc}

interface

uses
  Windows, Types, Controls, Classes, Graphics, Generics.Defaults, Generics.Collections,
  dxGDIPlusClasses, cxGraphics, dxCoreGraphics, cxControls, dxCoreClasses;

type

  { TdxSpreadSheetGradientStops }

  TdxSpreadSheetGradientStops = class(TcxControl)
  strict private const
    StopSize = 12;
  strict private
    FBrush: TdxGPBrush;
    FCaptureOffset: TPoint;
    FDragging: Boolean;
    FSelectedStopIndex: Integer;
    FStops: TdxRectList;

    FOnSelectionChanged: TNotifyEvent;

    procedure BrushChangeHandler(Sender: TObject);
    function GetSelectedStopColor: TdxAlphaColor;
    function GetTrackArea: TRect;
    function GetTrackAreaContent: TRect;
    procedure SetSelectedStopColor(const Value: TdxAlphaColor);
    procedure SetSelectedStopIndex(AValue: Integer);
  protected
    procedure BoundsChanged; override;

    // Keyboard
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;

    // Mouse
    procedure DblClick; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;

    procedure Calculate;
    procedure DoPaint; override;
    procedure DrawFrame(ACanvas: TcxCanvas; R: TRect; ASelected: Boolean = False);
    procedure DrawGradient(ACanvas: TcxCanvas; const R: TRect);
    procedure DrawStop(ACanvas: TcxCanvas; const R: TRect; AColor: TdxAlphaColor; ASelected: Boolean);
    procedure DrawStops(ACanvas: TcxCanvas);
    procedure DrawTrack(ACanvas: TcxCanvas);
    procedure MoveStop(const DeltaX: Integer);
    procedure MoveStopTo(const P: TPoint);
    function StopIndexAtPoint(const P: TPoint): Integer;
    function StopIndexByColorAndOffset(AOffset: Single; AColor: TdxAlphaColor): Integer;
    //
    property Stops: TdxRectList read FStops;
    property TrackArea: TRect read GetTrackArea;
    property TrackAreaContent: TRect read GetTrackAreaContent;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Add;
    procedure AddAtPoint(const P: TPoint);
    procedure DeleteSelected;
    //
    property Brush: TdxGPBrush read FBrush;
    property SelectedStopColor: TdxAlphaColor read GetSelectedStopColor write SetSelectedStopColor;
    property SelectedStopIndex: Integer read FSelectedStopIndex write SetSelectedStopIndex;
  published
    property OnDblClick;
    property OnSelectionChanged: TNotifyEvent read FOnSelectionChanged write FOnSelectionChanged;
  end;

implementation

uses
  cxGeometry, SysUtils, cxLookAndFeelPainters, cxLookAndFeels, dxTypeHelpers, dxCore, Math;

{ TdxSpreadSheetGradientStops }

constructor TdxSpreadSheetGradientStops.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBrush := TdxGPBrush.Create;
  FBrush.Style := gpbsGradient;
  FBrush.OnChange := BrushChangeHandler;
  FStops := TdxRectList.Create;
  FSelectedStopIndex := -1;
  ControlStyle := ControlStyle + [csCaptureMouse];
  TabStop := True;
  Keys := [kArrows];
end;

destructor TdxSpreadSheetGradientStops.Destroy;
begin
  FreeAndNil(FBrush);
  FreeAndNil(FStops);
  inherited Destroy;
end;

procedure TdxSpreadSheetGradientStops.Add;
begin
  Brush.GradientPoints.Add(0.5, 0);
  SelectedStopIndex := StopIndexByColorAndOffset(0.5, 0);
end;

procedure TdxSpreadSheetGradientStops.AddAtPoint(const P: TPoint);
begin
  Add;
  MoveStopTo(P);
end;

procedure TdxSpreadSheetGradientStops.DeleteSelected;
begin
  if SelectedStopIndex >= 0 then
  begin
    Brush.GradientPoints.Delete(SelectedStopIndex);
    SelectedStopIndex := SelectedStopIndex; // validate
  end;
end;

procedure TdxSpreadSheetGradientStops.BoundsChanged;
begin
  inherited BoundsChanged;
  Calculate;
end;

procedure TdxSpreadSheetGradientStops.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  case Key of
    VK_INSERT:
      Add;
    VK_DELETE:
      DeleteSelected;
    VK_LEFT:
      SelectedStopIndex := Max(SelectedStopIndex - 1, 0);
    VK_RIGHT:
      SelectedStopIndex := SelectedStopIndex + 1;
    VK_UP, VK_DOWN:
      MoveStop(IfThen(Key = VK_DOWN, -1, 1) * Max(1, Round((TrackArea.Width - StopSize) / 100)));
  end;
end;

procedure TdxSpreadSheetGradientStops.DblClick;
begin
  if SelectedStopIndex < 0 then
    AddAtPoint(CalcCursorPos);
  inherited DblClick;
end;

procedure TdxSpreadSheetGradientStops.MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer);
begin
  SetFocus;
  if (Button = mbLeft) and not (ssDouble in Shift) then
  begin
    SelectedStopIndex := StopIndexAtPoint(Point(X, Y));
    FDragging := SelectedStopIndex >= 0;
    if FDragging then
      FCaptureOffset := cxPointOffset(Point(X, Y), cxRectCenter(Stops[SelectedStopIndex]), False);
  end;
end;

procedure TdxSpreadSheetGradientStops.MouseMove(Shift: TShiftState; X: Integer; Y: Integer);
begin
  if FDragging then
    MoveStopTo(cxPointOffset(Point(X, Y), FCaptureOffset, False));
end;

procedure TdxSpreadSheetGradientStops.MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer);
begin
  FDragging := False;
end;

procedure TdxSpreadSheetGradientStops.Calculate;
var
  I: Integer;
  R: TRect;
begin
  Stops.Clear;
  R := cxRectCenterHorizontally(TrackArea, TrackArea.Width - StopSize);
  R.Bottom := Bounds.Bottom;
  R.Top := Bounds.Top;
  for I := 0 to Brush.GradientPoints.Count - 1 do
    Stops.Add(cxRectBounds(R.Left + Round(R.Width * Brush.GradientPoints.Offsets[I]) - StopSize shr 1, R.Top, StopSize, R.Height));
  Invalidate;
end;

procedure TdxSpreadSheetGradientStops.DoPaint;
begin
  inherited DoPaint;
  Canvas.FillRect(Bounds, LookAndFeelPainter.DefaultContentColor);
  DrawTrack(Canvas);
  DrawStops(Canvas);
end;

procedure TdxSpreadSheetGradientStops.DrawFrame(ACanvas: TcxCanvas; R: TRect; ASelected: Boolean = False);
const
  Color1Map: array[Boolean] of TColor = ($222222, $0022EF);
  Color2Map: array[Boolean] of TColor = (clWhite, $94E2FF);
begin
  ACanvas.FrameRect(R, Color1Map[ASelected]);
  R := cxRectInflate(R, -1);
  ACanvas.FrameRect(R, Color2Map[ASelected]);
  R := cxRectInflate(R, -1);
  cxDrawTransparencyCheckerboard(ACanvas, R, 4);
end;

procedure TdxSpreadSheetGradientStops.DrawGradient(ACanvas: TcxCanvas; const R: TRect);
begin
  dxGPPaintCanvas.BeginPaint(ACanvas.Handle, R);
  try
    dxGPPaintCanvas.Rectangle(R, nil, Brush);
  finally
    dxGPPaintCanvas.EndPaint;
  end;
end;

procedure TdxSpreadSheetGradientStops.DrawStop(ACanvas: TcxCanvas; const R: TRect; AColor: TdxAlphaColor; ASelected: Boolean);
begin
  DrawFrame(ACanvas, R, ASelected);
  ACanvas.SaveClipRegion;
  try
    ACanvas.IntersectClipRect(cxRectInflate(R, -2));
    dxGPPaintCanvas.BeginPaint(ACanvas.Handle, R);
    try
      dxGPPaintCanvas.Rectangle(R, 0, AColor);
    finally
      dxGPPaintCanvas.EndPaint;
    end;
  finally
    ACanvas.RestoreClipRegion;
  end;
end;

procedure TdxSpreadSheetGradientStops.DrawStops(ACanvas: TcxCanvas);
var
  I: Integer;
begin
  for I := 0 to Stops.Count - 1 do
    DrawStop(ACanvas, Stops[I], Brush.GradientPoints.Colors[I], I = SelectedStopIndex);
end;

procedure TdxSpreadSheetGradientStops.DrawTrack(ACanvas: TcxCanvas);
begin
  ACanvas.SaveClipRegion;
  try
    DrawFrame(ACanvas, TrackArea);
    ACanvas.IntersectClipRect(TrackAreaContent);
    DrawGradient(ACanvas, TrackArea);
  finally
    ACanvas.RestoreClipRegion;
  end;
end;

procedure TdxSpreadSheetGradientStops.MoveStop(const DeltaX: Integer);
begin
  if SelectedStopIndex >= 0 then
    MoveStopTo(cxRectCenter(cxRectOffset(Stops[SelectedStopIndex], DeltaX, 0)));
end;

procedure TdxSpreadSheetGradientStops.MoveStopTo(const P: TPoint);
var
  AColor: TdxAlphaColor;
  AOffset: Single;
  ARect: TRect;
begin
  if SelectedStopIndex >= 0 then
  begin
    ARect := cxRectCenterHorizontally(TrackArea, TrackArea.Width - StopSize);
    AColor := Brush.GradientPoints.Colors[SelectedStopIndex];
    AOffset := Min(Max((P.X - ARect.Left) / ARect.Width, 0), 1);
    Brush.GradientPoints.Offsets[SelectedStopIndex] := AOffset;
    FSelectedStopIndex := StopIndexByColorAndOffset(AOffset, AColor);
  end;
end;

function TdxSpreadSheetGradientStops.StopIndexAtPoint(const P: TPoint): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := Stops.Count - 1 downto 0 do
  begin
    if cxRectPtIn(Stops[I], P) then
      Exit(I);
  end;
end;

function TdxSpreadSheetGradientStops.StopIndexByColorAndOffset(AOffset: Single; AColor: TdxAlphaColor): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := Brush.GradientPoints.Count - 1 downto 0 do
  begin
    if SameValue(Brush.GradientPoints.Offsets[I], AOffset) and (Brush.GradientPoints.Colors[I] = AColor) then
      Exit(I);
  end;
end;

procedure TdxSpreadSheetGradientStops.BrushChangeHandler(Sender: TObject);
begin
  Calculate;
end;

function TdxSpreadSheetGradientStops.GetSelectedStopColor: TdxAlphaColor;
begin
  if SelectedStopIndex >= 0 then
    Result := Brush.GradientPoints.Colors[SelectedStopIndex]
  else
    Result := 0;
end;

function TdxSpreadSheetGradientStops.GetTrackArea: TRect;
begin
  Result := cxRectInflate(Bounds, 0, -2);
end;

function TdxSpreadSheetGradientStops.GetTrackAreaContent: TRect;
begin
  Result := cxRectInflate(TrackArea, -2);
end;

procedure TdxSpreadSheetGradientStops.SetSelectedStopColor(const Value: TdxAlphaColor);
begin
  if SelectedStopIndex >= 0 then
    Brush.GradientPoints.Colors[SelectedStopIndex] := Value;
end;

procedure TdxSpreadSheetGradientStops.SetSelectedStopIndex(AValue: Integer);
begin
  AValue := Min(Max(AValue, -1), Brush.GradientPoints.Count - 1);
  if SelectedStopIndex <> AValue then
  begin
    FSelectedStopIndex := AValue;
    dxCallNotify(OnSelectionChanged, Self);
    Invalidate;
  end;
end;

end.
