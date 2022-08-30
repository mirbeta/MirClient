unit EditorsStylesDemoFrameControl;

interface

uses
  Controls, Classes, Messages;

type
  TcxFrameControl = class(TWinControl)
  private
    FFramedControl: TControl;
  protected
    procedure AdjustFrameRgn;
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure FrameControl(AControl: TControl);
    procedure UpdateFrameControlPos;
  end;

implementation

uses
  Windows, Graphics;

{ TcxFrameControl }

procedure TcxFrameControl.AdjustFrameRgn;
const AElipsWidth = 4;
var
  ARgn1,ARgn2: HRGN;
  ARect: TRect;
begin
  if Parent <> nil then
  begin
    ARect := Rect(0, 0, Width, Height);
    ARgn1 := CreateRoundRectRgn(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom, AElipsWidth, AElipsWidth);
    InflateRect(ARect, -2, -2);
    ARgn2 := CreateRoundRectRgn(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom, AElipsWidth, AElipsWidth);
    CombineRgn(ARgn1, ARgn2, ARgn1, RGN_XOR);
    SetWindowRgn(Handle, ARgn1, True);
    DeleteObject(ARgn1);
    DeleteObject(ARgn2);
  end;
end;

constructor TcxFrameControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Color := clRed;
end;

procedure TcxFrameControl.FrameControl(AControl: TControl);
begin
  FFramedControl := AControl;
  UpdateFrameControlPos;
end;

procedure TcxFrameControl.Resize;
begin
  AdjustFrameRgn;
  inherited;
end;

procedure TcxFrameControl.UpdateFrameControlPos;
var
  ARect, ADestRect: TRect;
begin
  if not Assigned(FFramedControl) then Exit;
  ARect := FFramedControl.Parent.ClientRect;
  if FFramedControl.Left < 0 then
    ADestRect.Left := 0
  else
    ADestRect.Left := FFramedControl.Left;
  if FFramedControl.Top < 0 then
    ADestRect.Top := 0
  else
    ADestRect.Top := FFramedControl.Top;
  if (FFramedControl.Left +  FFramedControl.Width) >= ARect.Right then
    ADestRect.Right := ARect.Right - ADestRect.Left
  else
    ADestRect.Right := FFramedControl.Width;
  if (FFramedControl.Top + FFramedControl.Height) >= ARect.Bottom then
    ADestRect.Bottom := ARect.Bottom - ADestRect.Top
  else
    ADestRect.Bottom := FFramedControl.Height;
  ADestRect.TopLeft := FFramedControl.Parent.ClientToScreen(ADestRect.TopLeft);
  ADestRect.TopLeft := Parent.ScreenToClient(ADestRect.TopLeft);
  ADestRect.Right := ADestRect.Right + ADestRect.Left;
  ADestRect.Bottom := ADestRect.Bottom + ADestRect.Top;
  BoundsRect := ADestRect;
  BringToFront;
end;

end.
