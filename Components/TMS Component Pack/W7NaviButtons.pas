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

unit W7NaviButtons;

interface
{$I TMSDEFS.INC}
uses
  Windows, Classes, Controls, Graphics, W7Classes, W7Common, W7Graphics, ImgList,
  ExtCtrls;

type
  TW7NavigationDirection = (ndBackward, ndForward);
  TW7NavigationFrame = class (TW7GraphicControl)
  private
    FFrame: TIcon;
    FOpacity: byte;
    procedure SetOpacity(Value: byte);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Opacity: byte read FOpacity write SetOpacity;
    property Visible;
  end;

  TW7CustomNavigationButton = class (TW7GraphicControl)
  private
    FEnabled: boolean;
    FDirection: TW7NavigationDirection;
    FUp: TIcon;
    FDown: TIcon;
    FHot: TIcon;
    FDisabled: TIcon;
    FInternalTimer: TTimer;
    FMouseInOpacity: integer;
    FMouseUpped: boolean;
    FFadeInInterval: integer;
    FFadeOutInterval: integer;
    procedure SetDirection(Value: TW7NavigationDirection);
  protected
    procedure TimerProc(Sender: TObject);
    procedure SetEnabled(Value: Boolean); override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Direction: TW7NavigationDirection read FDirection write SetDirection default ndForward;
    property FadeInInterval: integer read FFadeInInterval write FFadeInInterval;
    property FadeOutInterval: integer read FFadeOutInterval write FFadeOutInterval;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TW7NavigationButton = class (TW7CustomNavigationButton)
  published
    property Caption;
    property Direction;
    property FadeInInterval;
    property FadeOutInterval;
    property Action;
    property Align;
    property Anchors;
    property Constraints;
    property Enabled;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property OnClick;
    property OnDblClick;
    {$IFDEF DELPHI2006_LVL}
    property OnMouseActivate;
    {$ENDIF}
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
  end;

implementation

{$R W7_Controls.res}

constructor TW7CustomNavigationButton.Create(AOwner: TComponent);
begin
  inherited;
  FUp := TIcon.Create;
  FHot := TIcon.Create;
  FDown := TIcon.Create;
  FDisabled := TIcon.Create;
  FEnabled := True;
  FMouseInOpacity := 0;
  FFadeOutInterval := 17;
  FFadeInInterval := 17;
  FInternalTimer := TTimer.Create(Self);
  FInternalTimer.Enabled := False;
  FInternalTimer.OnTimer := TimerProc;
  FMouseUpped := True;
  Width := 24;
  Height := 24;
  Direction := ndForward;
end;

destructor TW7CustomNavigationButton.Destroy;
begin
  FInternalTimer.Destroy;
  FUp.Destroy;
  FDown.Destroy;
  FHot.Destroy;
  FDisabled.Destroy;
  inherited;
end;

procedure TW7CustomNavigationButton.Paint;
var
  X, Y: integer;
  Bmp: TBitmap;
begin
  inherited;
  Canvas.Lock;
  X := Width div 2 - 12;
  Y := Height div 2 - 12;
  Bmp := TBitmap.Create;
  with Bmp do
  begin
    Width := Self.Width;
    Height := Self.Height;
    PixelFormat := pf32bit;
    Canvas.Brush.Color := clBlack;
    Canvas.FillRect(Rect(0, 0, Width, Height));
    Canvas.Draw(X, Y, FHot);
  end;
  if not FEnabled then
    Canvas.Draw(X, Y, FDisabled)
  else
  begin
    if MouseInControl and LeftButtonPressed then
      Canvas.Draw(X, Y, FDown)
    else if FInternalTimer.Enabled then
    begin
      Canvas.Draw(X, Y, FUp);
      Bmp.Canvas.Draw(X, Y, FHot);
      AlphaBlendBitmap(Bmp, Canvas, Rect(0, 0, Width, Height), FMouseInOpacity);
    end
    else if MouseInControl and not LeftButtonPressed then
      Canvas.Draw(X, Y, FHot)
    else if (not MouseInControl) and (not LeftButtonPressed) then
      Canvas.Draw(X, Y, FUp)
  end;
  Bmp.Destroy;
  Canvas.Unlock;
end;

procedure TW7CustomNavigationButton.TimerProc(Sender: TObject);
begin
  if MouseInControl then
    Inc(FMouseInOpacity, 40)
  else
    Dec(FMouseInOpacity, 40);
  if (FMouseInOpacity <= 0) or (FMouseInOpacity >= 255) then
  begin
    FInternalTimer.Enabled := False;
    if FMouseInOpacity > 255 then
      FMouseInOpacity := 255;
    FMouseUpped := True;
  end;
  Invalidate;
end;

procedure TW7CustomNavigationButton.MouseEnter;
begin
  if FEnabled then
  begin
    if FMouseInOpacity <= 0 then
      FMouseInOpacity := 1;
    FMouseUpped := True;
    FInternalTimer.Interval := FFadeInInterval;
    FInternalTimer.Enabled := True;
  end;
  inherited;
end;

procedure TW7CustomNavigationButton.MouseLeave;
begin
  if FEnabled then
  begin
    if (FMouseInOpacity >= 255) or (not FMouseUpped) then
      FMouseInOpacity := 254;
    FInternalTimer.Interval := FFadeOutInterval;
    FInternalTimer.Enabled := True;
    FMouseUpped := True;
  end;
  inherited;
end;

procedure TW7CustomNavigationButton.SetEnabled(Value: Boolean);
begin
  inherited;
  FEnabled := Value;
end;

procedure TW7CustomNavigationButton.SetDirection(Value: TW7NavigationDirection);
var
  Res: TResourceStream;
  Str: string;
begin
  FDirection := Value;
  if FDirection = ndForward then
    Str := 'forward'
  else
    Str := 'back';
  Res := TResourceStream.Create(Hinstance, 'am_' + Str, RT_RCDATA);
  FUp.LoadFromStream(Res);
  Res.Free;
  Res := TResourceStream.Create(Hinstance, 'am_' + Str + '_hot', RT_RCDATA);
  FHot.LoadFromStream(Res);
  Res.Free;
  Res := TResourceStream.Create(Hinstance, 'am_' + Str + '_down', RT_RCDATA);
  FDown.LoadFromStream(Res);
  Res.Free;
  Res := TResourceStream.Create(Hinstance, 'am_' + Str + '_disabled', RT_RCDATA);
  FDisabled.LoadFromStream(Res);
  Res.Free;

  Invalidate;
end;

///////////////////////////

constructor TW7NavigationFrame.Create(AOwner: TComponent);
var
  Res: TResourceStream;
begin
  inherited;
  FFrame := TIcon.Create;
  Res := TResourceStream.Create(Hinstance, 'am_navi_frame', RT_RCDATA);
  FFrame.LoadFromStream(Res);
  Res.Free;
  Width := 56;
  Height := 26;
  FOpacity := 200;
end;

destructor TW7NavigationFrame.Destroy;
begin
  FFrame.Destroy;
  inherited;
end;

procedure TW7NavigationFrame.Paint;
var
  X, Y: integer;
  Bmp: TBitmap;
begin
  inherited;
  Canvas.Lock;
  X := Width div 2 - 28;
  Y := Height div 2 - 13;
  Bmp := TBitmap.Create;
  with Bmp do
  begin
    Width := Self.Width;
    Height := Self.Height;
    PixelFormat := pf32bit;
    Canvas.Brush.Color := clBlack;
    Canvas.FillRect(Rect(0, 0, Width, Height));
    Canvas.Draw(X, Y, FFrame);
  end;
  AlphaBlendBitmap(Bmp, Canvas, Rect(0, 0, Width, Height), FOpacity);
  Bmp.Destroy;
  Canvas.Unlock;
end;

procedure TW7NavigationFrame.SetOpacity(Value: byte);
begin
  FOpacity := Value;
  Invalidate;
end;

end.