{***************************************************************************}
{ TMS TAdvShaper                                                            }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ Copyright 2002 - 2010 by TMS Software                                     }
{ Email : info@tmssoftware.com                                              }
{ Web : http://www.tmssoftware.com                                          }
{                                                                           }
{ The source code is given as is. The author is not responsible             }
{ for any possible damage done due to the use of this code.                 }
{ The component can be freely used in any application. The complete         }
{ source code remains property of the author and may not be distributed,    }
{ published, given or sold in any form as such. No parts of the source      }
{ code can be included in any other component or application without        }
{ written authorization of the author.                                      }
{***************************************************************************}


unit AdvShaper;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

type
  TVrRgnData = class(TPersistent)
  private
    FSize: Integer;
    FBuffer: PRgnData;
    procedure SetSize(Value: Integer);
  public
    destructor Destroy; override;
    property Size: Integer read FSize write SetSize;
    property Buffer: PRgnData read FBuffer write FBuffer;
  end;

  TCustGraphicControl = class(TGraphicControl)
  private
    FUpdateCount: Integer;
  protected
    function Designing: Boolean;
    function Loading: Boolean;
    procedure ClearClientCanvas;
    procedure UpdateControlCanvas; virtual;
    procedure UpdateControlBounds; virtual;
    procedure AdjustControlSize; virtual;
    procedure ShowDesignFrame(Dest: TCanvas);
  public
    constructor Create(AOwner: TComponent); override;
    procedure BeginUpdate;
    procedure EndUpdate;
  published
  end;

  TGraphicImageControl = class(TCustGraphicControl)
  private
    FOverlay: TBitmap;
    FBitmapImage: TBitmap;
    FRefreshOverlay: Boolean;
    FTransparent: Boolean;
    function GetBitmapCanvas: TCanvas;
    procedure SetTransparent(Value: Boolean);
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
  protected
    DestCanvas: TCanvas;
    procedure ClearBitmapCanvas; virtual;
    procedure Paint; override;
    procedure CopyParentImage;
    procedure CopyOverlayImage;
    procedure UpdateControlCanvas; override;
    property BitmapImage: TBitmap read FBitmapImage;
    property BitmapCanvas: TCanvas read GetBitmapCanvas;
    property Transparent: Boolean read FTransparent write SetTransparent;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TIsInCaptionAreaEvent = procedure(Sender: TObject; X,Y:integer; var IsInCaption: boolean) of object;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvShaper = class(TGraphicImageControl)
  private
    FMask: TBitmap;
    FRgnData: TVrRgnData;
    FRgn: HRgn;
    FOnIsInCaptionArea: TIsInCaptionAreaEvent;
    FAutoFormMove: boolean;
    function GetMaskColor: TColor;
    procedure SetMask(Value: TBitmap);
    procedure SetMaskColor(Value: TColor);
    procedure UpdateMask;
    procedure ReadMask(Reader: TStream);
    procedure WriteMask(Writer: TStream);
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
  protected
    procedure Paint; override;
    procedure Loaded; override;
    procedure SetParent(Value: TWinControl); override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateRegion;
  published
    property AutoFormMove: boolean read FAutoFormMove write FAutoFormMove default true;
    property Mask: TBitmap read FMask write SetMask;
    property MaskColor: TColor read GetMaskColor write SetMaskColor;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Hint;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property OnClick;
    property OnIsInCaptionArea: TIsInCaptionAreaEvent read FOnIsInCaptionArea write FOnIsInCaptionArea;
{$IFDEF VER130}
    property OnContextPopup;
{$ENDIF}
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

implementation

function CompareRect(R1, R2: TRect): Boolean;
begin
  Result := (R1.Left = R2.Left) and
            (R1.Top = R2.Top) and
            (R1.Right = R2.Right) and
            (R1.Bottom = R2.Bottom);
end;

procedure ExtGenerateMask(Bitmap: TBitmap; TransparentColor: TColor;
  RgnData: TVrRgnData);
var
  X, Y: integer;
  Rgn1: HRgn;
  Rgn2: HRgn;
  StartX, EndX: Integer;
  OldCursor: TCursor;
begin
  Rgn1 := 0;
  OldCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    for Y := 0 to Bitmap.Height - 1 do
    begin
      X := 0;
      repeat
        while (Bitmap.Canvas.Pixels[X, Y] = TransparentColor) and
          (X < Bitmap.Width - 1) do Inc(X);
        StartX := X;

        Inc(X);
        while (Bitmap.Canvas.Pixels[X, Y] <> TransparentColor) and
         (X < Bitmap.Width - 1) do Inc(X);
        EndX := X;

        if StartX < Bitmap.Width - 1 then
        begin
          if Rgn1 = 0 then
            Rgn1 := CreateRectRgn(StartX, Y, EndX, Y + 1)
          else
          begin
            Rgn2 := CreateRectRgn(StartX, Y, EndX, Y + 1);
            if Rgn2 <> 0 then CombineRgn(Rgn1, Rgn1, Rgn2, RGN_OR);
            DeleteObject(Rgn2);
          end;
        end;
      until X >= Bitmap.Width - 1;
    end;

    if (Rgn1 <> 0) then
    begin
      RgnData.Size := GetRegionData(Rgn1, 0, nil);
      GetRegionData(Rgn1, RgnData.Size, RgnData.Buffer);
      DeleteObject(Rgn1);
    end;

  finally
    Screen.Cursor := OldCursor;
  end;
end;

{ TVrRgnData }

destructor TVrRgnData.Destroy;
begin
  SetSize(0);
  inherited Destroy;
end;

procedure TVrRgnData.SetSize(Value: Integer);
begin
  if FSize <> Value then
  begin
    FSize := Value;
    ReallocMem(FBuffer, Value);
  end;
end;

{ TVrFormShape }

constructor TAdvShaper.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque];
  Align := alClient;
  Color := clOlive;
  ParentColor := false;
  Transparent := True;
  FMask := TBitmap.Create;
  FRgnData := TVrRgnData.Create;
  FAutoFormMove := true;
end;

destructor TAdvShaper.Destroy;
begin
  FMask.Free;
  FRgnData.Free;
  if FRgn <> 0 then DeleteObject(FRgn);
  inherited Destroy;
end;

procedure TAdvShaper.SetParent(Value: TWinControl);
begin
  if Value <> nil then
  begin
    if not (Value is TForm) then
      raise Exception.Create('TAdvShaper requires a FORM as parent!');
    with TForm(Value) do Borderstyle := bsNone;
  end;
  inherited;
end;

procedure TAdvShaper.Loaded;
begin
  inherited Loaded;
  if not (csDesigning in ComponentState) then
    UpdateRegion;
end;

procedure TAdvShaper.UpdateMask;
begin
  ExtGenerateMask(FMask, Self.Color, FRgnData);
  if not (csDesigning in ComponentState) then UpdateRegion;
end;

procedure TAdvShaper.UpdateRegion;
begin
  if FRgn <> 0 then
  begin
    DeleteObject(FRgn);
    FRgn := 0;
  end;
  if FRgnData.Size > 0 then
  begin
    FRgn := ExtCreateRegion (nil, FRgnData.Size, FRgnData.Buffer^);
    SetWindowRgn(Parent.Handle, FRgn, True);
  end;
end;

procedure TAdvShaper.SetMask(Value: TBitmap);
begin
  FMask.Assign(Value);
  if not (csLoading in ComponentState) then UpdateMask;
  UpdateControlCanvas;
end;

function TAdvShaper.GetMaskColor: TColor;
begin
  Result := Self.Color;
end;

procedure TAdvShaper.SetMaskColor(Value: TColor);
begin
  if Self.Color <> Value then
  begin
    Self.Color := Value;
    if not (csLoading in ComponentState) then
    begin
      UpdateMask;
      UpdateControlCanvas;
    end;
  end;
end;

procedure TAdvShaper.Paint;
begin
  if (FMask.Empty) or (csDesigning in ComponentState) then
    ClearBitmapCanvas;

  if not FMask.Empty then
    BitmapCanvas.Draw(0, 0, FMask);

  if csDesigning in ComponentState then
    with BitmapCanvas do
    begin
      Pen.Style := psDot;
      Brush.Style := bsClear;
      Rectangle(0, 0, Width, Height);
    end;

  inherited Paint;
end;

procedure TAdvShaper.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  IsInCaption: boolean;

begin
  if (Button = mbleft) then
  begin
    inherited;

    IsInCaption := true;

    if Assigned(OnIsInCaptionArea) then
      OnIsInCaptionArea(Self, X,Y, IsInCaption);

    if IsInCaption and FAutoFormMove then
    begin
      ReleaseCapture;
      TWinControl(Parent).Perform(WM_SYSCOMMAND, $F012, 0);
    end;
  end;
end;

procedure TAdvShaper.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
end;

procedure TAdvShaper.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.Result := 1;
end;

procedure TAdvShaper.ReadMask(Reader: TStream);
var
  Size: Integer;
begin
  Reader.Read(Size, Sizeof(Integer));
  if Size <> 0 then
  begin
    FRgnData.Size := Size;
    Reader.Read(FRgnData.Buffer^, Size);
  end;
end;

procedure TAdvShaper.WriteMask(Writer: TStream);
begin
  Writer.Write(FRgnData.Size, Sizeof(Integer));
  if FRgnData.Size <> 0 then
    Writer.Write(FRgnData.Buffer^, FRgnData.Size);
end;

procedure TAdvShaper.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineBinaryProperty('RgnData', ReadMask, WriteMask, True);
end;


{ TCustGraphicControl }

constructor TCustGraphicControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

function TCustGraphicControl.Designing: Boolean;
begin
  Result := (csDesigning in ComponentState);
end;

function TCustGraphicControl.Loading: Boolean;
begin
  Result := (csLoading in ComponentState);
end;

procedure TCustGraphicControl.AdjustControlSize;
var
  R: TRect;
begin
  R := BoundsRect;
  if not Loading then
  begin
    SetBounds(Left, Top, Width, Height);
    if CompareRect(BoundsRect, R) then
      UpdateControlCanvas; //force update
  end;
end;

{ Used in combination with AutoSize property }
procedure TCustGraphicControl.UpdateControlBounds;
var
  R: TRect;
begin
  if (AutoSize) and (Align in [alNone, alTop, alBottom, alLeft, alRight]) then
  begin
    R := BoundsRect;
    if not Loading then
      SetBounds(Left, Top, Width, Height);
    if CompareRect(BoundsRect, R) then
      UpdateControlCanvas; //force update
  end else UpdateControlCanvas;
end;

procedure TCustGraphicControl.ClearClientCanvas;
begin
  with inherited Canvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := Self.Color;
    FillRect(ClientRect);
  end;
end;

procedure TCustGraphicControl.ShowDesignFrame(Dest: TCanvas);
begin
  if Designing then
    with Dest do
    begin
      Pen.Style := psDot;
      Pen.Color := clBlack;
      Brush.Style := bsClear;
      Rectangle(0, 0, Width, Height);
    end;
end;

procedure TCustGraphicControl.UpdateControlCanvas;
begin
  if not Loading then
    if FUpdateCount = 0 then Repaint;
end;

procedure TCustGraphicControl.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TCustGraphicControl.EndUpdate;
begin
  Dec(FUpdateCount);
  UpdateControlCanvas;
end;

{ TGraphicImageControl }

constructor TGraphicImageControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOverlay := TBitmap.Create;
  FRefreshOverlay := True;
  FBitmapImage := TBitmap.Create;
  FTransparent := false;
  DestCanvas := Self.Canvas;
end;

destructor TGraphicImageControl.Destroy;
begin
  FOverlay.Free;
  FBitmapImage.Free;
  inherited Destroy;
end;

procedure TGraphicImageControl.WMPaint(var Message: TWMPaint);
{$IFDEF VIDEO256}
var
  CurPalette, OldPalette: HPalette;
  RestorePalette: Boolean;
{$ENDIF}
begin
  if Message.DC <> 0 then
  begin
    Canvas.Lock;
    BitmapImage.Width := Self.Width;
    BitmapImage.Height := Self.Height;
    BitmapCanvas.Lock;
{$IFDEF VIDEO256}
    try
      OldPalette := 0;
      RestorePalette := false;
      CurPalette := GetPalette;
      if CurPalette <> 0 then
      begin
        OldPalette := SelectPalette(BitmapCanvas.Handle, CurPalette, True);
        RealizePalette(BitmapCanvas.Handle);
        RestorePalette := True;
      end;
{$ENDIF}
      try
        Canvas.Handle := Message.DC;
        try
          Paint;
        finally
          Canvas.Handle := 0;
        end;
{$IFDEF VIDEO256}
      finally
        if RestorePalette then
          SelectPalette(BitmapCanvas.Handle, OldPalette, True);
      end;
{$ENDIF}
    finally
      BitmapCanvas.Unlock;
      Canvas.Unlock;
    end;
  end;
end;

procedure TGraphicImageControl.UpdateControlCanvas;
begin
  if (not Designing) then
    if (Transparent) and (not FOverlay.Empty) then
    begin
      FRefreshOverlay := false;
      ControlStyle := ControlStyle + [csOpaque];
    end;
  inherited UpdateControlCanvas;
end;

procedure TGraphicImageControl.CopyParentImage;
var
  R: TRect;
begin
  FOverlay.Width := Self.Width;
  FOverlay.Height := Self.Height;
  R := Canvas.ClipRect;
  FOverlay.Canvas.CopyRect(R, Canvas, R);
end;

procedure TGraphicImageControl.CopyOverlayImage;
begin
  BitBlt(BitmapCanvas.Handle, 0, 0, BitmapImage.Width, BitmapImage.Height,
    FOverlay.Canvas.Handle, 0, 0, SRCCOPY);
end;

procedure TGraphicImageControl.SetTransparent(Value: Boolean);
begin
  if FTransparent <> Value then
  begin
    FTransparent := Value;
    if Value then ControlStyle := ControlStyle - [csOpaque]
    else ControlStyle := ControlStyle + [csOpaque];
    if Designing then Invalidate
    else UpdateControlCanvas;
  end;
end;

procedure TGraphicImageControl.ClearBitmapCanvas;
begin
  with FBitmapImage do
  begin
    Canvas.Brush.Style := bsSolid;
    if FTransparent then
    begin
      if FRefreshOverlay then CopyParentImage;
      CopyOverlayImage;
    end
    else
    begin
      Canvas.Brush.Color := Self.Color;
      Canvas.FillRect(Bounds(0, 0, Width, Height));
    end;
  end;
end;

procedure TGraphicImageControl.Paint;
begin
  BitBlt(Canvas.Handle, 0, 0, Width, Height,
    BitmapCanvas.Handle, 0, 0, SRCCOPY);
  if Transparent then
  begin
    FRefreshOverlay := True;
    ControlStyle := ControlStyle - [csOpaque];
  end;
end;

function TGraphicImageControl.GetBitmapCanvas: TCanvas;
begin
  Result := FBitmapImage.Canvas;
end;



end.
