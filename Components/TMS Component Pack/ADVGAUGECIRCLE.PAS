{***************************************************************************}
{ TAdvGaugeCircle component                                                 }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2014                                               }
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
unit AdvGaugeCircle;

{$I TMSDEFS.INC}

interface

uses
  Forms, Math, Classes, Graphics, Controls, SysUtils, ExtCtrls, AdvGaugeControl,
  Windows, AdvStyleIF, Messages, GDIPFill, AdvGDIP
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 0; // Build nr.

type
  TAdvGaugeCircle = class(TCustomControl, IGaugeControl)
  private
    { Private declarations }
    FBackground: TGDIPFill;
    FEndPosition: Double;
    FFormat: string;
    FInnerMargin: Integer;
    FMaximum: Double;
    FMaxText: string;
    FMinimum: Double;
    FMinText: string;
    FPosition: Double;
    FPenOpacity: Byte;
    FArcWidth: Double;
    FPenColor: TColor;
    FRectUp: TGPRectF;
    FRectDown: TGPRectF;
    FCircleBackgroundColor: TColor;
    FPictureDown: TAdvGdipPicture;
    FPictureUp: TAdvGdipPicture;
    FIncrement: Double;
    FShowUpDown: Boolean;
    FOnClickDown: TNotifyEvent;
    FOnClickUp: TNotifyEvent;
    FText: string;
    function GetEndPosition: Double;
    function GetFormat: string;
    function GetIsRange: Boolean;
    function GetMaximum: Double;
    function GetMinimum: Double;
    function GetPosition: Double;
    function GetVersion: string;
    function GetVersionNr: Integer;
    procedure SetBackground(const Value: TGDIPFill);
    procedure SetEndPosition(const Value: Double);
    procedure SetFormat(const Value: string);
    procedure SetMaximum(const Value: Double);
    procedure SetMaxText(const Value: string);
    procedure SetMinimum(const Value: Double);
    procedure SetMinText(const Value: string);
    procedure SetPosition(const Value: Double);
    procedure SetVersion(const Value: string);
    procedure SetInnerMargin(const Value: Integer);
    procedure SetPenColor(const Value: TColor);
    procedure SetPenOpacity(const Value: Byte);
    procedure SetArcWidth(const Value: Double);
    procedure SetCircleBackgroundColor(const Value: TColor);
    procedure SetPictureDown(const Value: TAdvGdipPicture);
    procedure SetPictureUp(const Value: TAdvGdipPicture);
    procedure SetIncrement(const Value: Double);
    procedure SetShowUpDown(const Value: Boolean);
    procedure SetText(const Value: string);
  protected
    { Protected declarations }
    procedure Paint; override;
    procedure Changed(Sender: TObject);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer;
      Y: Integer); override;
    procedure UpClicked(Sender: TObject);
    procedure DownClicked(Sender: TObject);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property EndPosition: Double read GetEndPosition write SetEndPosition;
    property IsRange: Boolean read GetIsRange;
    property MaxText: string read FMaxText write SetMaxText;
    property MinText: string read FMinText write SetMinText;
  published
    { Published declarations }
    property Align;
    property Anchors;
    property Background: TGDIPFill read FBackground write SetBackground;
    property CircleBackgroundColor: TColor read FCircleBackgroundColor write SetCircleBackgroundColor default clSilver;
    property Constraints;
    property DoubleBuffered default True;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Format: string read GetFormat write SetFormat;
    property Font;
    property Height default 100;
    property Increment: Double read FIncrement write SetIncrement;
    property InnerMargin: Integer read FInnerMargin write SetInnerMargin default 10;
    property Maximum: Double read GetMaximum write SetMaximum;
    property Minimum: Double read GetMinimum write SetMinimum;
    property PictureUp: TAdvGdipPicture read FPictureUp write SetPictureUp;
    property PictureDown: TAdvGdipPicture read FPictureDown write SetPictureDown;
    property PopupMenu;
    property Position: Double read GetPosition write SetPosition;
    property ShowHint;
    property ShowUpDown: Boolean read FShowUpDown write SetShowUpDown default False;
    Property Text: string read FText write SetText;
    property Tag;
    property Top;
    property Touch;
    property Visible;
    property Version: string read GetVersion write SetVersion;
    property Width default 100;

    property ArcWidth: Double read FArcWidth write SetArcWidth;
    property PenOpacity: Byte read FPenOpacity write SetPenOpacity default 255;
    property PenColor: TColor read FPenColor write SetPenColor default clBlue;

    property OnClick;
    property OnClickUp: TNotifyEvent read FOnClickUp write FOnClickUp;
    property OnClickDown: TNotifyEvent read FOnClickDown write FOnClickDown;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnGesture;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

implementation

{$R AdvGaugeCircle.res}

{ TAdvGaugeCircle }

procedure TAdvGaugeCircle.Changed(Sender: TObject);
begin
  Invalidate;
end;

constructor TAdvGaugeCircle.Create(AOwner: TComponent);
begin
  inherited;
  Width := 100;
  Height := 100;
  FCircleBackgroundColor := clSilver;
  FPenColor := clBlue;
  FArcWidth := 2;
  FPenOpacity := 255;
  FMinimum := 0;
  FMaximum := 100;
  FPosition := 50;
  DoubleBuffered := True;
  FInnerMargin := 10;
  FText := 'Text';
  FIncrement := 5;

  FBackground := TGDIPFill.Create;
  FBackground.Color := clWhite;
  FBackground.ColorTo := clWhite;
  FBackground.OnChange := Changed;

  Font.Size := 14;
  Font.Name := 'Trebuchet Sans MS';
  Font.Color := clBlack;

  FPictureDown := TAdvGdipPicture.Create;
  FPictureDown.LoadFromResourceName(HInstance, 'TMSGDOWN');
  FPictureDown.OnChange := Changed;

  FPictureUp := TAdvGdipPicture.Create;
  FPictureUp.LoadFromResourceName(HInstance, 'TMSGUP');
  FPictureUp.OnChange := Changed;
end;

destructor TAdvGaugeCircle.Destroy;
begin
  FPictureDown.Free;
  FPictureUp.Free;
  FBackground.Free;
  inherited;
end;

procedure TAdvGaugeCircle.DownClicked(Sender: TObject);
begin
  Position := Max(FPosition - FIncrement, FMinimum);

  if Assigned(FOnClickDown) then
    FOnClickDown(Sender);
end;

function TAdvGaugeCircle.GetEndPosition: Double;
begin
  Result := FEndPosition;
end;

function TAdvGaugeCircle.GetFormat: string;
begin
  Result := FFormat;
end;

function TAdvGaugeCircle.GetIsRange: Boolean;
begin
  Result := False;
end;

function TAdvGaugeCircle.GetMaximum: Double;
begin
  Result := FMaximum;
end;

function TAdvGaugeCircle.GetMinimum: Double;
begin
  Result := FMinimum;
end;

function TAdvGaugeCircle.GetPosition: Double;
begin
  Result := FPosition;
end;

function TAdvGaugeCircle.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER, REL_VER), MakeWord(MIN_VER, MAJ_VER));
end;

procedure TAdvGaugeCircle.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  if Assigned(FPictureDown) and not FPictureDown.Empty then
  begin
    if (X <= FRectDown.X + FRectDown.Width) and (X >= FRectDown.X) then
    begin
      if (Y <= FRectDown.Y + FRectDown.Height) and (Y >= FRectDown.Y) then
      DownClicked(Self);
    end;
  end;
  if Assigned(FPictureUp) and not FPictureUp.Empty then
  begin
    if (X <= FRectUp.X + FRectUp.Width) and (X >= FRectUp.X) then
    begin
      if (Y <= FRectUp.Y + FRectUp.Height) and (Y >= FRectUp.Y) then
      UpClicked(Self);
    end;
  end;
  inherited;
end;

function TAdvGaugeCircle.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn))) + '.' + IntToStr(Lo(Hiword(vn))) + '.' + IntToStr(Hi(Loword(vn))) + '.' + IntToStr(Lo(Loword(vn)));
end;

procedure TAdvGaugeCircle.Paint;
var
  g: TGPGraphics;
  r, cr, tr: TGPRectF;
  pen, bgPen: TGPPen;
  gpbrush: TGPBrush;
  val: Double;
  gpfont: TGPFont;
  PosText: string;
begin
  inherited;

  //Create the TGPGraphics
  g := TGPGraphics.Create(Canvas.Handle);
  try
    //Config TGPGraphics
    g.SetSmoothingMode(SmoothingModeAntiAlias);
    g.SetTextRenderingHint(TextRenderingHintClearTypeGridFit);

    //Fill the background
    r := MakeRect(0, 0, ClientRect.Right - ClientRect.Left, ClientRect.Bottom - ClientRect.Top);
    FBackground.Fill(g, r);

    //Draw the circle
    cr.X := r.X + Max((r.Width - r.Height) /2, 0) + InnerMargin + Trunc(FArcWidth / 2);
    cr.Y := r.Y + Max((r.Height - r.Width) /2, 0) + InnerMargin + Trunc(FArcWidth / 2);
    cr.Width := Min(r.Width, r.Height) - FInnerMargin * 2 - FArcWidth;
    cr.Height := cr.Width;

    pen := TGPPen.Create(MakeColor(FPenOpacity, FPenColor), FArcWidth);
    bgPen := TGPPen.Create(MakeColor(FPenOpacity, FCircleBackgroundColor), FArcWidth);
    //Convert Font into something TGPGraphics can handle
    gpfont := g.MakeFont(Font);
    gpbrush := TGPSolidBrush.Create(MakeColor(255, Font.Color));

    try
      val := FMaximum - FMinimum;
      if val <> 0 then
        val := (FPosition - FMinimum) / val;
      val := 360 * val;

      g.DrawArc(bgPen, cr, 270 + val, 360 - val);
      g.DrawArc(pen, cr, 270, val);

      //Draw the position centered in the circle
      PosText := FloatToStr(Position);

      if FFormat <> '' then
        PosText := SysUtils.Format(FFormat, [Position]);

      g.MeasureString(PosText,Length(PosText), gpfont ,r, tr);
      tr.X := cr.X + cr.Width / 2 - tr.Width / 2;
      tr.Y := cr.Y + cr.Height / 2 - tr.Height / 2;
      g.DrawString(PosText, Length(PosText), gpfont, MakePoint(tr.X, tr.Y), gpbrush);

      if FShowUpDown then
      begin
        if Assigned(FPictureUp) and not FPictureUp.Empty then
        begin
          FRectUp.X := tr.X + (tr.Width - FPictureUp.Width) / 2;
          FRectUp.Y := tr.Y - FPictureUp.Height;
          FRectUp.Width := FPictureUp.Width;
          FRectUp.Height := FPictureUp.Height;

          if (FRectUp.Height > tr.Y - cr.Y) and (FRectUp.Height <> 0) then
          begin
            FRectUp.Width := FRectUp.Width * ((tr.Y - cr.Y) / FRectUp.Height);
            FRectUp.Height := tr.Y - cr.Y;
          end;
          if (FRectUp.Width > cr.Width) and (FRectUp.Width <> 0) then
          begin
            FRectUp.Height := FRectUp.Height * (cr.Width / FRectUp.Width);
            FRectUp.Width :=  cr.Width;
          end;
          FRectUp.Y := tr.Y - FRectUp.Height;
          FRectUp.X := cr.X + cr.Width / 2 - FRectUp.Width / 2;
          FPictureUp.GDIPDraw(g, FRectUp);
        end;
        if Assigned(FPictureDown) and not FPictureDown.Empty then
        begin
          FRectDown.X := tr.X + (tr.Width - FPictureDown.Width) / 2;
          FRectDown.Y := tr.Y + FPictureDown.Height;
          FRectDown.Width := FPictureDown.Width;
          FRectDown.Height := FPictureDown.Height;

          if (FRectDown.Height > tr.Y - cr.Y) and (FRectDown.Height <> 0) then
          begin
            FRectDown.Width := FRectDown.Width * ((tr.Y - cr.Y) / FRectDown.Height);
            FRectDown.Height := tr.Y - cr.Y;
          end;
          if (FRectDown.Width > cr.Width) and (FRectDown.Width <> 0) then
          begin
            FRectDown.Height := FRectDown.Height * (cr.Width / FRectDown.Width);
            FRectDown.Width :=  cr.Width;
          end;
          FRectDown.Y := tr.Y + tr.Height;
          FRectDown.X := cr.X + cr.Width / 2 - FRectDown.Width / 2;
          FPictureDown.GDIPDraw(g, FRectDown);
        end
        else
        begin
          FRectDown.Y := tr.Y + tr.Height;
          FRectDown.Height := 0;
        end;
      end
      else
      begin
        FRectDown.Y := tr.Y + tr.Height;
        FRectDown.Height := 0;
      end;

      if FText <> '' then
      begin
        g.MeasureString(FText, Length(FText), gpfont, r, tr);
        tr.X := cr.X + cr.Width / 2 - tr.Width / 2;
        tr.Y := FRectDown.Y + FRectDown.Height;
        if tr.Y + tr.Height < cr.Y + cr.Height then
          g.DrawString(FText, Length(FText), gpfont, MakePoint(tr.X, tr.Y), gpBrush);
      end;
    finally
      pen.Free;
      bgpen.Free;
      gpbrush.Free;
      gpfont.Free;
    end;

  finally
    g.Free;
  end;
end;

procedure TAdvGaugeCircle.SetBackground(const Value: TGDIPFill);
begin
  FBackground.Assign(Value);
  Invalidate;
end;

procedure TAdvGaugeCircle.SetCircleBackgroundColor(const Value: TColor);
begin
  if (FCircleBackgroundColor <> Value) then
  begin
    FCircleBackgroundColor := Value;
    Invalidate;
  end;
end;

procedure TAdvGaugeCircle.SetEndPosition(const Value: Double);
begin
  if (Value <> FEndPosition) then
  begin
    FEndPosition := Value;
    Invalidate;
  end;
end;

procedure TAdvGaugeCircle.SetFormat(const Value: string);
begin
  if (Value <> FFormat) then
  begin
    FFormat := Value;
    Invalidate;
  end;
end;

procedure TAdvGaugeCircle.SetIncrement(const Value: Double);
begin
  if (FIncrement <> Value) then
  begin
    FIncrement := Value;
    Invalidate;
  end;
end;

procedure TAdvGaugeCircle.SetInnerMargin(const Value: Integer);
begin
  if (Value <> FInnerMargin) then
  begin
    FInnerMargin := Value;
    Invalidate;
  end;
end;

procedure TAdvGaugeCircle.SetMaximum(const Value: Double);
begin
  if (Value <> FMaximum) and (Value > FMinimum) then
  begin
    FMaximum := Value;
    Invalidate;
  end;
end;

procedure TAdvGaugeCircle.SetMaxText(const Value: string);
begin
  if (Value <> FMaxText) then
  begin
    FMaxText := Value;
    Invalidate;
  end;
end;

procedure TAdvGaugeCircle.SetMinimum(const Value: Double);
begin
  if (Value <> FMinimum) and (Value < FMaximum) then
  begin
    FMinimum := Value;
    Invalidate;
  end;
end;

procedure TAdvGaugeCircle.SetMinText(const Value: string);
begin
  if (Value <> FMinText) then
  begin
    FMinText := Value;
    Invalidate;
  end;
end;

procedure TAdvGaugeCircle.SetPenColor(const Value: TColor);
begin
  if (Value <> FPenColor) then
  begin
    FPenColor := Value;
    Invalidate;
  end;
end;

procedure TAdvGaugeCircle.SetPenOpacity(const Value: Byte);
begin
  if (FPenOpacity <> Value) then
  begin
    FPenOpacity := Value;
    Invalidate;
  end;
end;

procedure TAdvGaugeCircle.SetArcWidth(const Value: Double);
begin
  if (FArcWidth <> Value) then
  begin
    FArcWidth := Value;
    Invalidate;
  end;
end;

procedure TAdvGaugeCircle.SetPictureDown(const Value: TAdvGdipPicture);
begin
  FPictureDown.Assign(Value);
  Invalidate;
end;

procedure TAdvGaugeCircle.SetPictureUp(const Value: TAdvGdipPicture);
begin
  FPictureUp.Assign(Value);
  Invalidate;
end;

procedure TAdvGaugeCircle.SetPosition(const Value: Double);
begin
  if (Value <> FPosition) and (Value <= FMaximum) and (Value >= FMinimum) then
  begin
    FPosition := Value;
    Invalidate;
  end;
end;

procedure TAdvGaugeCircle.SetShowUpDown(const Value: Boolean);
begin
  if FShowUpDown <> Value then
  begin
    FShowUpDown := Value;
    Invalidate;
  end;
end;

procedure TAdvGaugeCircle.SetText(const Value: string);
begin
  if FText <> Value then
  begin
    FText := Value;
    Invalidate;
  end;
end;

procedure TAdvGaugeCircle.SetVersion(const Value: string);
begin

end;

procedure TAdvGaugeCircle.UpClicked(Sender: TObject);
begin
  Position := Min(FPosition + FIncrement, FMaximum);

  if Assigned(FOnClickUp) then
    FOnClickUp(Sender);
end;

end.
