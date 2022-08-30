unit uDraggedControl;

interface
  uses Windows, Messages, Classes, Sysutils, Controls, Graphics;
type
  TDraggedControl = class(TCustomControl)
  private
    FInitPos: TPoint;
    FControl: TControl;
    FImage: TBitmap;
    FWasVisible: Boolean;
    FWasDsnVis: Boolean;
    procedure WMEraseBkgnd(var Message: TWMErasebkgnd); message WM_ERASEBKGND;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Paint; override;
  public
    constructor Create(Control: TControl);
    destructor Destroy; override;
    procedure MoveTo(OffsetX, OffsetY: integer);
  end;

implementation

{ TDraggedControl }

type
  TWinControlCrack = class(TWinControl);

constructor TDraggedControl.Create(Control: TControl);
begin
  inherited Create(nil);
  FControl  := Control;
  FInitPos  := FControl.Parent.ClientToScreen(Point(FControl.Left, FControl.Top));
  FImage    := TBitmap.Create;
  FImage.Width  := FControl.Width;
  FImage.Height := FControl.Height;
  if FControl is TWinControl then
    begin
      FImage.Canvas.Lock;
      try
        TWinControlCrack(FControl).PaintTo(FImage.Canvas.Handle, 0, 0);
      finally
        FImage.Canvas.Unlock;
      end;
    end
  else
    FControl.Perform(WM_PAINT, FImage.Canvas.Handle, 0);
  MoveTo(0, 0);

  if FControl is TWinControl then
    ShowWindow(TWinControl(FControl).Handle, SW_HIDE)
  else
    begin
      FWasVisible := FControl.Visible;
      FWasDsnVis := not (csNoDesignVisible in FControl.ControlStyle);
      if FWasDsnVis then
        FControl.ControlStyle := FControl.ControlStyle + [csNoDesignVisible];
      if FWasVisible then
        FControl.Visible := False;
    end;
end;

destructor TDraggedControl.Destroy;
begin
  if FControl is TWinControl then
    ShowWindow(TWinControl(FControl).Handle, SW_SHOWNOACTIVATE)
  else
    begin
      if FWasDsnVis then
        FControl.ControlStyle := FControl.ControlStyle - [csNoDesignVisible];
      if FWasVisible then
        FControl.Visible := True;
      FControl.Invalidate;
    end;
  FreeAndNil(FImage);
  inherited;
end;

procedure TDraggedControl.CreateParams(var Params: TCreateParams);
begin
  inherited;
  with Params do
  begin
    Style := WS_POPUP;
//    WindowClass.Style := WindowClass.Style or CS_SAVEBITS;
    if NewStyleControls then ExStyle := WS_EX_TOOLWINDOW;
    AddBiDiModeExStyle(ExStyle);
  end;
end;

procedure TDraggedControl.MoveTo(OffsetX, OffsetY: integer);
begin
  with BoundsRect do
    SetWindowPos(Handle, HWND_TOPMOST, FInitPos.X + OffsetX, FInitPos.Y + OffsetY, FControl.Width, FControl.Height,
                 SWP_NOACTIVATE or SWP_SHOWWINDOW);
end;

procedure TDraggedControl.Paint;
begin
  Canvas.Draw(0, 0, FImage);
end;

procedure TDraggedControl.WMEraseBkgnd(var Message: TWMErasebkgnd);
begin
end;

end.
