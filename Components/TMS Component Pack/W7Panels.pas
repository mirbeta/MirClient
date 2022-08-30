{***************************************************************************}
{ TMS W7 Controls Pack                                                      }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2011                                               } 
{            Email : info@tmssoftware.com                                   }
{            Web : http://www.tmssoftware.com                               }
{                                                                           }
{ The source code is given as is. The author is not responsible             }
{ for any possible damage done due to the use of this code.                 }
{ The component can be freely used in any application. The complete         }
{ source code remains property of the author and may not be distributed,    }
{ published, given or sold in any form as such. No parts of the source      }
{ code can be included in any other component or application without        }
{ written authorization of the author.                                      }
{***************************************************************************}

unit W7Panels;

interface

{$I TMSDEFS.INC}

uses
  Windows, Classes, StdCtrls, Controls, Graphics, W7Classes, W7Common, W7Graphics,
  Jpeg;

type
  TW7PanelBorders = set of (wpbLeft, wpbTop, wpbRight, wpbBottom);
  TW7PanelStyle = (wpsNavy, wpsDarkGrey, wpsBlue, wpsBlack, wpsLightBlack, wpsSilver, wpsWinCalc, wpsGreen, wpsCustom);
  TW7PanelFillStyle = (pfsSolid, pfsGradient, pfsDoubleGradient);

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TW7LeftPanel = class (TW7Control)
  private
  protected
    Texture: TBitmap;
    procedure Paint; override;
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Align;
    property Anchors;
    property Constraints;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    {$IFDEF DELPHI_UNICODE}
    property ParentDoubleBuffered;
    {$ENDIF}
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    {$IFDEF DELPHI2006_LVL}
    property OnMouseActivate;
    {$ENDIF}
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TW7CaptionPanel = class (TW7Control)
  private
  protected
    Texture: TBitmap;
    procedure Paint; override;
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Align;
    property Anchors;
    property Constraints;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    {$IFDEF DELPHI_UNICODE}
    property ParentDoubleBuffered;
    {$ENDIF}
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    {$IFDEF DELPHI2006_LVL}
    property OnMouseActivate;
    {$ENDIF}
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TW7Panel = class (TW7TransparentControl)
  private
    FSolidColor: TColor;
    FOuterBorderColor: TColor;
    FInnerBorderColor: TColor;
    FInnerBorders: TW7PanelBorders;
    FOuterBorders: TW7PanelBorders;
    FRounded: boolean;
    FOpacity: byte;
    FStyle: TW7PanelStyle;
    FFillStyle: TW7PanelFillStyle;
    FColorA: TColor;
    FColorB: TColor;
    FColorC: TColor;
    FColorD: TColor;
    FVerticalGradient: boolean;
    procedure SetSolidColor(Value: TColor);
    procedure SetOuterBorderColor(Value: TColor);
    procedure SetInnerBorderColor(Value: TColor);
    procedure SetInnerBorders(Value: TW7PanelBorders);
    procedure SetOuterBorders(Value: TW7PanelBorders);
    procedure SetRounded(Value: boolean);
    procedure SetOpacity(Value: byte);
    procedure SetStyle(Value: TW7PanelStyle);
    procedure SetFillStyle(Value: TW7PanelFillStyle);
    procedure SetColorA(Value: TColor);
    procedure SetColorB(Value: TColor);
    procedure SetColorC(Value: TColor);
    procedure SetColorD(Value: TColor);
    procedure SetVerticalGradient(Value: boolean);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Caption;
    property Font;
    property SolidColor: TColor read FSolidColor write SetSolidColor default clWhite;
    property OuterBorderColor: TColor read FOuterBorderColor write SetOuterBorderColor default $00AD9C8E;
    property InnerBorderColor: TColor read FInnerBorderColor write SetInnerBorderColor default $00FFFFFF;
    property InnerBorders: TW7PanelBorders read FInnerBorders write SetInnerBorders;
    property OuterBorders: TW7PanelBorders read FOuterBorders write SetOuterBorders;
    property Rounded: boolean read FRounded write SetRounded default True;
    property Opacity: byte read FOpacity write SetOpacity default 255;
    property Style: TW7PanelStyle read FStyle write SetStyle default wpsWinCalc;
    property FillStyle: TW7PanelFillStyle read FFillStyle write SetFillStyle default pfsDoubleGradient;
    property VerticalGradient: boolean read FVerticalGradient write SetVerticalGradient default True;
    property ColorA: TColor read FColorA write SetColorA default $00FCF3ED;
    property ColorB: TColor read FColorB write SetColorB default $00FFFFFF;
    property ColorC: TColor read FColorC write SetColorC default $00FFFFFF;
    property ColorD: TColor read FColorD write SetColorD default $00FCF3ED;
    property Align;
    property Anchors;
    property Constraints;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    {$IFDEF DELPHI_UNICODE}
    property ParentDoubleBuffered;
    {$ENDIF}
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    {$IFDEF DELPHI2006_LVL}
    property OnMouseActivate;
    {$ENDIF}
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

implementation

{$R W7_CPanel.res}
{$R W7_LPanel.res}

constructor TW7LeftPanel.Create(AOwner: TComponent);
var
  Res: TResourceStream;
  JS: TJPEGImage;
begin
  inherited;
  ControlStyle := ControlStyle + [csAcceptsControls];
  Texture := TBitmap.Create;
  Width := 200;
  Height := 480;
  Align := alLeft;
  with Texture do
  begin
    Width := Self.Width;
    Height := Self.Height;
    PixelFormat := pf24bit;
  end;
  JS := TJPEGImage.Create;
  Res := TResourceStream.Create(Hinstance, 'am_lpanel', RT_RCDATA);
  JS.LoadFromStream(Res);
  Res.Free;
  Texture.Assign(JS);
  JS.Free;
  InvalidateOnMouseEvents := False;
end;

destructor TW7LeftPanel.Destroy;
begin
  Texture.Free;
  inherited;
end;

procedure TW7LeftPanel.Paint;
begin
  inherited;
  BitBlt(Canvas.Handle, 0, 0, 200, 480, Texture.Canvas.Handle, 0, 0, SRCCOPY);
  if Height > 480 then
    DrawGradient(Canvas, Texture.Canvas.Pixels[1, 439], $00fef2e8, Rect(0, 480, Width, Height), True);
end;

procedure TW7LeftPanel.Resize;
begin
  inherited;
  Width := 200;
end;

/////////////////////////////////////////////////////////////////////////////////////
///
///
///

constructor TW7CaptionPanel.Create(AOwner: TComponent);
var
  Res: TResourceStream;
  JS: TJPEGImage;
begin
  inherited;
  ControlStyle := ControlStyle + [csAcceptsControls];
  Width := 500;
  Height := 44;
  Align := alTop;
  Texture := TBitmap.Create;
  Texture.PixelFormat := pf24bit;
  JS := TJPEGImage.Create;
  Res := TResourceStream.Create(Hinstance, 'am_cpanel', RT_RCDATA);
  JS.LoadFromStream(Res);
  Res.Free;
  Texture.Assign(JS);
  JS.Free;
  InvalidateOnMouseEvents := False;
end;

destructor TW7CaptionPanel.Destroy;
begin
  Texture.Free;
  inherited;
end;

procedure TW7CaptionPanel.Paint;
var
  Indx: integer;
begin
  inherited;
  BitBlt(Canvas.Handle, 0, 0, Texture.Width, Texture.Height, Texture.Canvas.Handle, 0, 0, SRCCOPY);
  for Indx := 693 to Width - 1 do
    BitBlt(Canvas.Handle, Indx, 0, 1, 44, Texture.Canvas.Handle, 692, 0, SRCCOPY);
end;

procedure TW7CaptionPanel.Resize;
begin
  inherited;
  Height := 44;
end;



/////////////////////////////////////////////////////////////////////////////////////
///
///
///

constructor TW7Panel.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csAcceptsControls];
  InvalidateOnMouseEvents := False;
  Width := 350;
  Height := 75;
  FRounded := True;
  FInnerBorders := [wpbLeft, wpbTop, wpbRight, wpbBottom];
  FOuterBorders := [wpbLeft, wpbTop, wpbRight, wpbBottom];
  FOpacity := 255;
  FFillStyle := pfsGradient;
  FVerticalGradient := True;
  FStyle := wpsWinCalc;
  FOuterBorderColor := $00AD9C8E;
  FInnerBorderColor := $00FFFFFF;
  FColorA := $00FCF3ED;
  FColorB := $00FFFFFF;
  FRounded := True;
end;

destructor TW7Panel.Destroy;
begin
  inherited;
end;

procedure TW7Panel.Paint;
var
  Buffer: TBitmap;
  RectA, RectB: TRect;
begin
  inherited;
  Buffer := TBitmap.Create;
  with Buffer do
  begin
    Width := Self.Width;
    Height := Self.Height;
    PixelFormat := pf24Bit;
  end;
  if FFillStyle = pfsSolid then
  begin
    Buffer.Canvas.Brush.Color := FColorA;
    Buffer.Canvas.Pen.Color := FColorA;
    Buffer.Canvas.Rectangle(0, 0, Width, Height);
  end
  else if FFillStyle = pfsGradient then
  begin     
    DrawGradient(Buffer.Canvas, FColorA, FColorB, Rect(0, 0, Width, Height), FVerticalGradient);
  end
  else if FFillStyle = pfsDoubleGradient then
  begin     
    if FVerticalGradient then
    begin
      RectA := Rect(0, 0, Width, Height div 2);
      RectB := Rect(0, Height div 2, Width, Height);
    end
    else
    begin
      RectA := Rect(0, 0, Width div 2, Height);
      RectB := Rect(Width div 2, 0, Width, Height);
    end;
    DrawGradient(Buffer.Canvas, FColorA, FColorB, RectA, FVerticalGradient);
    DrawGradient(Buffer.Canvas, FColorC, FColorD, RectB, FVerticalGradient);    
  end;

  Buffer.Canvas.Pen.Color := FInnerBorderColor;
  if wpbLeft in InnerBorders then
  begin
    Buffer.Canvas.MoveTo(1, 1);
    Buffer.Canvas.LineTo(1, Height - 1);
  end;
  if wpbTop in InnerBorders then
  begin
    Buffer.Canvas.MoveTo(1, 1);
    Buffer.Canvas.LineTo(Width - 1, 1);
  end;
  if wpbRight in InnerBorders then
  begin
    Buffer.Canvas.MoveTo(Width - 2, 1);
    Buffer.Canvas.LineTo(Width - 2, Height - 1);
  end;
  if wpbBottom in InnerBorders then
  begin
    Buffer.Canvas.MoveTo(1, Height - 2);
    Buffer.Canvas.LineTo(Width - 1, Height - 2);
  end;

  Buffer.Canvas.Pen.Color := FOuterBorderColor;
  if wpbLeft in OuterBorders then
  begin
    Buffer.Canvas.MoveTo(0, 0);
    Buffer.Canvas.LineTo(0, Height);
  end;
  if wpbTop in OuterBorders then
  begin
    Buffer.Canvas.MoveTo(0, 0);
    Buffer.Canvas.LineTo(Width, 0);
  end;
  if wpbRight in OuterBorders then
  begin
    Buffer.Canvas.MoveTo(Width - 1, 0);
    Buffer.Canvas.LineTo(Width - 1, Height);
  end;
  if wpbBottom in OuterBorders then
  begin
    Buffer.Canvas.MoveTo(0, Height - 1);
    Buffer.Canvas.LineTo(Width, Height - 1);
  end;

  if FRounded then
  begin
    Buffer.Canvas.Pixels[0, 0] := AlphaBlendPixel(Buffer.Canvas.Pixels[0, 0], Canvas.Pixels[0, 0], 230);
    Buffer.Canvas.Pixels[0, 1] := AlphaBlendPixel(Buffer.Canvas.Pixels[0, 1], Canvas.Pixels[0, 1], 60);
    Buffer.Canvas.Pixels[1, 0] := AlphaBlendPixel(Buffer.Canvas.Pixels[1, 0], Canvas.Pixels[1, 0], 60);
    Buffer.Canvas.Pixels[1, 1] := AlphaBlendPixel(Buffer.Canvas.Pixels[0, 1], Buffer.Canvas.Pixels[1, 1], 130);

    Buffer.Canvas.Pixels[Width - 1, 0] := AlphaBlendPixel(Buffer.Canvas.Pixels[Width - 1, 0], Canvas.Pixels[Width - 1, 0], 230);
    Buffer.Canvas.Pixels[Width - 1, 1] := AlphaBlendPixel(Buffer.Canvas.Pixels[Width - 1, 1], Canvas.Pixels[Width - 1, 1], 60);
    Buffer.Canvas.Pixels[Width - 2, 0] := AlphaBlendPixel(Buffer.Canvas.Pixels[Width - 2, 0], Canvas.Pixels[Width - 2, 0], 60);
    Buffer.Canvas.Pixels[Width - 2, 1] := AlphaBlendPixel(Buffer.Canvas.Pixels[Width - 1, 1], Buffer.Canvas.Pixels[Width - 2, 1], 130);

    Buffer.Canvas.Pixels[0, Height - 1] := AlphaBlendPixel(Buffer.Canvas.Pixels[0, Height - 1], Canvas.Pixels[0, Height - 1], 230);
    Buffer.Canvas.Pixels[0, Height - 2] := AlphaBlendPixel(Buffer.Canvas.Pixels[0, Height - 2], Canvas.Pixels[0, Height - 2], 60);
    Buffer.Canvas.Pixels[1, Height - 1] := AlphaBlendPixel(Buffer.Canvas.Pixels[1, Height - 1], Canvas.Pixels[1, Height - 1], 60);
    Buffer.Canvas.Pixels[1, Height - 2] := AlphaBlendPixel(Buffer.Canvas.Pixels[0, Height - 2], Buffer.Canvas.Pixels[1, Height - 2], 130);

    Buffer.Canvas.Pixels[Width - 1, Height - 1] := AlphaBlendPixel(Buffer.Canvas.Pixels[Width - 1, Height - 1], Canvas.Pixels[Width - 1, Height - 1], 230);
    Buffer.Canvas.Pixels[Width - 1, Height - 2] := AlphaBlendPixel(Buffer.Canvas.Pixels[Width - 1, Height - 2], Canvas.Pixels[Width - 1, Height - 2], 60);
    Buffer.Canvas.Pixels[Width - 2, Height - 1] := AlphaBlendPixel(Buffer.Canvas.Pixels[Width - 2, Height - 1], Canvas.Pixels[Width - 2, Height - 1], 60);
    Buffer.Canvas.Pixels[Width - 2, Height - 2] := AlphaBlendPixel(Buffer.Canvas.Pixels[Width - 1, Height - 2], Buffer.Canvas.Pixels[Width - 2, Height - 2], 130);
  end;

  Buffer.Canvas.Brush.Style := bsClear;
  RectA := ClientRect;
  DrawTextEx(Buffer.Canvas.Handle, PChar(Caption), Length(Caption), RectA, DT_CENTER or DT_VCENTER or DT_SINGLELINE or DT_END_ELLIPSIS, nil);


  AlphaBlendBitmap(Buffer, Canvas, Rect(0, 0, Width, Height), FOpacity);
  Buffer.Destroy;
end;

procedure TW7Panel.SetSolidColor(Value: TColor);
begin
  FSolidColor := Value;
  Invalidate;
end;

procedure TW7Panel.SetOuterBorderColor(Value: TColor);
begin
  FOuterBorderColor := Value;
  Invalidate;
end;

procedure TW7Panel.SetInnerBorderColor(Value: TColor);
begin
  FInnerBorderColor := Value;
  Invalidate;
end;

procedure TW7Panel.SetInnerBorders(Value: TW7PanelBorders);
begin
  FInnerBorders := Value;
  Invalidate;
end;

procedure TW7Panel.SetOuterBorders(Value: TW7PanelBorders);
begin
  FOuterBorders := Value;
  Invalidate;
end;

procedure TW7Panel.SetRounded(Value: boolean);
begin
  FRounded:= Value;
  Invalidate;
end;

procedure TW7Panel.SetOpacity(Value: byte);
begin
  FOpacity := Value;
  Invalidate;
end;

procedure TW7Panel.SetStyle(Value: TW7PanelStyle);
begin
  FStyle := Value;
  case FStyle of
    wpsNavy:
    begin
      FColorA := $00977D6A;
      FColorB := $00684226;
      FColorC := $00582E10;
      FColorD := $00552A0B;
      FFillStyle := pfsDoubleGradient;
      FVerticalGradient := True;
      FInnerBorders := [];
      FOuterBorders := [];
    end;
    wpsDarkGrey:
    begin
      FOuterBorderColor := $00120C08;
      FInnerBorderColor := $00574F49;
      FColorA := $002D231C;
      FColorB := $0020150E;
      FFillStyle := pfsGradient;
      FVerticalGradient := True;
    end;
    wpsBlack:
    begin
      FOuterBorderColor := $00434343;
      FInnerBorderColor := $00262626;
      FColorA := $00636363;
      FColorB := $00202020;
      FColorC := $00000000;
      FColorD := $00000000;
      FFillStyle := pfsDoubleGradient;
      FVerticalGradient := True;
      FInnerBorders := [];
      FOuterBorders := [];
    end;
    wpsLightBlack:
    begin
      FOuterBorderColor := $00000000;
      FInnerBorderColor := $004B4B4B;
      FColorA := $00565554;
      FColorB := $002D2D2C;
      FColorC := $00181817;
      FColorD := $00030302;
      FFillStyle := pfsDoubleGradient;
      FVerticalGradient := True;
    end;
    wpsGreen:
    begin
      FOuterBorderColor := $000E5311;
      FInnerBorderColor := $00CCE3CD;
      FColorA := $009BD09F;
      FColorB := $003BB240;
      FColorC := $001AA121;
      FColorD := $001EAA27;
      FFillStyle := pfsDoubleGradient;
      FVerticalGradient := True;
    end;
    wpsSilver:
    begin
      FOuterBorderColor := $0068635C;
      FInnerBorderColor := $0068635C;
      FColorA := $00F9F6EF;
      FColorB := $00C9C5BD;
      FFillStyle := pfsGradient;
      FVerticalGradient := True;
      FInnerBorders := [];
    end;
    wpsBlue:
    begin
      FOuterBorderColor := $00E0E0E0;
      FInnerBorderColor := $00E0E0E0;
      FColorA := $00F58C05;
      FColorB := $00E65D01;
      FFillStyle := pfsGradient;
      FVerticalGradient := True;
      FInnerBorders := [];
    end;
    wpsWinCalc:
    begin
      FOuterBorderColor := $00AD9C8E;
      FInnerBorderColor := $00FFFFFF;
      FColorA := $00FCF3ED;
      FColorB := $00FFFFFF;
      FFillStyle := pfsGradient;
      FVerticalGradient := True;
    end;
    wpsCustom:
    begin

    end;
  end;
  Invalidate;
end;

procedure TW7Panel.SetFillStyle(Value: TW7PanelFillStyle);
begin
  FFillStyle := Value;
  FStyle := wpsCustom;
  Invalidate;
end;

procedure TW7Panel.SetColorA(Value: TColor);
begin
  FColorA := Value;
  FStyle := wpsCustom;
  Invalidate;
end;

procedure TW7Panel.SetColorB(Value: TColor);
begin
  FColorB := Value;
  FStyle := wpsCustom;
  Invalidate;
end;

procedure TW7Panel.SetColorC(Value: TColor);
begin
  FColorC := Value;
  FStyle := wpsCustom;
  Invalidate;
end;

procedure TW7Panel.SetColorD(Value: TColor);
begin
  FColorD := Value;
  FStyle := wpsCustom;
  Invalidate;
end;

procedure TW7Panel.SetVerticalGradient(Value: boolean);
begin
  FVerticalGradient := Value;
  Invalidate;
end;

end.