{***************************************************************************}
{ TAdvRangeCircles component                                                }
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

{$I TMSDEFS.INC}

unit AdvRangeCircles;

interface

uses
  Windows, Classes, Forms, Math, Graphics, Controls, SysUtils, ExtCtrls, AdvGaugeControl,
  AdvStyleIF, Messages, GDIPFill, AdvGDIP
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
  TAdvRangeCircles = class;
  TRangeCircles = class;

  TRangeCircleItem = class(TCollectionItem, IInterface, IGaugeControl)
  private
    FColor: TColor;
    FDegEnd: Double;
    FDegStart: Double;
    FFormat: string;
    FMaximum: Double;
    FMinimum: Double;
    FEndPosition: Double;
    FOwner: TAdvRangeCircles;
    FPosition: Double;
    FRefCount: Integer;
    FWidth: Integer;
    function GetEndPosition: Double;
    function GetFormat: string;
    function GetIsRange: Boolean;
    function GetMinimum: Double;
    function GetMaximum: Double;
    function GetPosition: Double;
    function GetValueDegrees(MinVal, MaxVal, Value: Double): Double;
    function InRangeOrLoading(Value: Double): Boolean;
    procedure SetColor(const Value: TColor);
    procedure SetEndPosition(const Value: Double);
    procedure SetFormat(const Value: string);
    procedure SetMaximum(const Value: Double);
    procedure SetMinimum(const Value: Double);
    procedure SetPosition(const Value: Double);
    procedure SetWidth(const Value: Integer);
  protected
    { Protected declarations }
    function GetDisplayName: string; override;
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure Draw(g: TGPGraphics; r: TGPRectF; maxRad, posRad, endPosRad, StartAngle: Double;
          b: TGPBrush; Center: TGPPointF; Font: TFont; CircleBgColor: TColor);
    procedure Changed; virtual;
  public
    { Public declarations }
    constructor Create(Collection: TCollection); override;

    property IsRange: Boolean read GetIsRange;
    property RefCount: Integer read FRefCount;
  published
    { Published declarations }
    property Color: TColor read FColor write SetColor default clGreen;
    property EndPosition: Double read FEndPosition write SetEndPosition;
    property Format: string read FFormat write SetFormat;
    property Maximum: Double read FMaximum write SetMaximum;
    property Minimum: Double read FMinimum write SetMinimum;
    property Position: Double read GetPosition write SetPosition;
    property Width: Integer read FWidth write SetWidth default 2;
  end;

  TRangeCircles = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TRangeCircleItem;
    procedure SetItem(Index: Integer; const Value: TRangeCircleItem);
    { Private declarations }
  protected
    { Protected declarations }
    procedure Update(Item: TCollectionItem); override;
  public
    { Public declarations }
    constructor Create(AOwner: TAdvRangeCircles);
    destructor Destroy; override;

    function Add: TRangeCircleItem;
    function Insert(Index: Integer): TRangeCircleItem;
    property Items[Index: Integer]: TRangeCircleItem read GetItem write SetItem; default;
  published
    { Published declarations}
  end;

  TAdvRangeCircles = class(TCustomControl)
  private
    FBackgroundColor: TColor;
    FCircles: TRangeCircles;
    FEndPositionCircleRadius: Double;
    FInnerMargin: Integer;
    FPositionCircleRadius: Double;
    FStartAngle: Double;
    FCircleBackgroundColor: TColor;
    function Loading: Boolean;
    procedure SetBackgroundColor(const Value: TColor);
    procedure SetCircles(const Value: TRangeCircles);
    procedure SetEndPositionCircleRadius(const Value: Double);
    procedure SetInnerMargin(const Value: Integer);
    procedure SetPositionCircleRadius(const Value: Double);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    function GetVersionNr: Integer;
    procedure SetStartAngle(const Value: Double);
    procedure SetCircleBackgroundColor(const Value: TColor);
    { Private declarations }
  protected
    { Protected declarations }
    procedure Paint; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
    property Align;
    property Anchors;
    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor default clBlack;
    property CircleBackgroundColor: TColor read FCircleBackgroundColor write SetCircleBackgroundColor default clDkGray;
    property Circles: TRangeCircles read FCircles write SetCircles;
    property Constraints;
    property DoubleBuffered default True;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property EndPositionCircleRadius: Double read FEndPositionCircleRadius write SetEndPositionCircleRadius;
    property Font;
    property Height default 250;
    property InnerMargin:Integer read FInnerMargin write SetInnerMargin default 10;
    property PopupMenu;
    property PositionCircleRadius: Double read FPositionCircleRadius write SetPositionCircleRadius;
    property ShowHint;
    property StartAngle: Double read FStartAngle write SetStartAngle;
    property Tag;
    property Top;
    property Touch;
    property Visible;
    property Version: string read GetVersion write SetVersion;
    property Width default 200;

    property OnClick;
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

function GetRadian(theta: Double): Double;

implementation

function GetRadian(theta: Double): Double;
begin
  Result := theta * (PI / 180);
end;

{ TAdvRangeCircles }

constructor TAdvRangeCircles.Create(AOwner: TComponent);
var
  FDesignTime: Boolean;
begin
  inherited;
  FCircles := TRangeCircles.Create(Self);
  FDesignTime := (csDesigning in ComponentState) and not
                 ((csReading in Owner.ComponentState) or (csLoading in Owner.ComponentState));
  if FDesignTime then
  begin
    FCircles.Add;
    FCircles.Add.Color := clRed;
  end;
  FBackgroundColor := clBlack;
  FCircleBackgroundColor := clDkGray;
  FInnerMargin := 10;
  FPositionCircleRadius := 10;
  FEndPositionCircleRadius := 10;
  FStartAngle := 270;
  Width := 200;
  Height := 250;
  DoubleBuffered :=  True;
end;

destructor TAdvRangeCircles.Destroy;
begin
  FCircles.Free;
  inherited;
end;

function TAdvRangeCircles.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn))) + '.' + IntToStr(Lo(Hiword(vn))) + '.' + IntToStr(Hi(Loword(vn))) + '.' + IntToStr(Lo(Loword(vn)));
end;

function TAdvRangeCircles.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER, REL_VER), MakeWord(MIN_VER, MAJ_VER));
end;

function TAdvRangeCircles.Loading: Boolean;
begin
  Result := ((csReading in Owner.ComponentState) or (csLoading in Owner.ComponentState))
end;

procedure TAdvRangeCircles.Paint;
var
  I: Integer;
  g: TGPGraphics;
  R, dr: TGPRectF;
  b: TGPSolidBrush;
  maxRad: Double;
  Center: TGPPointF;
begin
  //Create the TGPGraphics and the drawing rectangle
  g := TGPGraphics.Create(Canvas.Handle);
  g.SetSmoothingMode(SmoothingModeAntiAlias);
  g.SetTextRenderingHint(TextRenderingHintClearTypeGridFit);
  b := TGPSolidBrush.Create(MakeColor(255, FBackgroundColor));
  R := MakeRect(0, 0, ClientRect.Right - ClientRect.Left, ClientRect.Bottom - ClientRect.Top);
  g.FillRectangle(b, R);

  //Calculate Center Point
  dr.X := r.X + Max((r.Width - r.Height) /2, 0) + FInnerMargin;
  dr.Y := r.Y + Max((r.Height - r.Width) /2, 0) + FInnerMargin;
  dr.Width := Min(R.Width, R.Height) - InnerMargin * 2;
  dr.Height := dr.Width;
  Center := MakePoint(dr.X + dr.Width / 2, dr.Y + dr.Height / 2);
  maxRad := dr.Width / 2;
  for I := 0 to FCircles.Count - 1 do
  begin
    FCircles[I].Draw(g, r, maxRad, FPositionCircleRadius, FEndPositionCircleRadius, StartAngle, b, Center, Font, FCircleBackgroundColor);
  end;
  b.Free;
  g.Free;
end;

procedure TAdvRangeCircles.SetBackgroundColor(const Value: TColor);
begin
  if FBackgroundColor <> Value then
  begin
    FBackgroundColor :=  Value;
    Invalidate;
  end;
end;

procedure TAdvRangeCircles.SetCircleBackgroundColor(const Value: TColor);
begin
  if FCircleBackgroundColor <> Value then
  begin
    FCircleBackgroundColor := Value;
    Invalidate;
  end;
end;

procedure TAdvRangeCircles.SetCircles(const Value: TRangeCircles);
begin
  FCircles := Value;
end;

procedure TAdvRangeCircles.SetEndPositionCircleRadius(const Value: Double);
begin
  if Value <> FEndPositionCircleRadius then
  begin
    FEndPositionCircleRadius := Value;
    Invalidate;
  end;
end;

procedure TAdvRangeCircles.SetInnerMargin(const Value: Integer);
begin
  if Value <> FInnerMargin then
  begin
    FInnerMargin := Value;
    Invalidate;
  end;
end;

procedure TAdvRangeCircles.SetPositionCircleRadius(const Value: Double);
begin
  if Value <> FPositionCircleRadius then
  begin
    FPositionCircleRadius := Value;
    Invalidate;
  end;
end;

procedure TAdvRangeCircles.SetStartAngle(const Value: Double);
begin
  if FStartAngle <> Value then
  begin
    FStartAngle := Value;
    Invalidate;
  end;
end;

procedure TAdvRangeCircles.SetVersion(const Value: string);
begin

end;

{ TRangeCircleItem }

procedure TRangeCircleItem.Changed;
begin
  (Collection as TRangeCircles).Update(Self);
end;

constructor TRangeCircleItem.Create(Collection: TCollection);
begin
  inherited Create(Collection as TCollection);
  FOwner := Collection.Owner as TAdvRangeCircles;
  FColor := clGreen;
  FMinimum := 0;
  FMaximum := 10;
  FPosition := 0;
  FEndPosition := 5;
  FRefCount := 0;
  FWidth := 2;
  FDegStart := GetValueDegrees(FMinimum, FMaximum, FPosition);
  FDegEnd := GetValueDegrees(FMinimum, FMaximum, FEndPosition);
end;

procedure TRangeCircleItem.Draw(g: TGPGraphics; r: TGPRectF; maxRad, posRad, endPosRad, StartAngle: Double;
      b: TGPBrush; Center: TGPPointF; Font: TFont; CircleBgColor: TColor);
var
  p, bgPen: TGPPen;
  bp: TGPBrush;
  circleRad: Double;
  dr, tr: TGPRectF;
  PosCenter: TGPPointF;
  PosText: string;
  gpfont: TGPFont;
begin
  //Draw the range circle
  p := TGPPen.Create(MakeColor(255, Color), FWidth);
  bgPen := TGPPen.Create(MakeColor(255, CircleBgColor), FWidth);
  bp := p.GetBrush;
  gpfont := g.MakeFont(Font);

  try
    if Collection.Count <> 0 then
      circleRad := (maxRad / Collection.Count) * (1 + Index) - Max(endPosRad, posRad)
    else
      circleRad := maxRad - Max(endPosRad, posRad);

    dr.X := Center.X - circleRad;
    dr.Y := Center.Y - circleRad;
    dr.Width := circleRad * 2;
    dr.Height := dr.Width;
    g.DrawEllipse(bgPen, dr);
    g.DrawArc(p, dr, FDegStart + StartAngle, FDegEnd - FDegStart);

    //Draw the Position Circle
    PosCenter.X := Center.X + Cos(GetRadian(FDegStart + StartAngle)) * circleRad;
    PosCenter.Y := Center.Y + Sin(GetRadian(FDegStart + StartAngle)) * circleRad;
    dr.X := Poscenter.X - posRad;
    dr.Y := PosCenter.Y - posRad;
    dr.Width := posRad * 2;
    dr.Height := dr.Width;
    g.FillEllipse(b, dr);
    g.DrawEllipse(p, dr);

    //Draw the Position Text
    PosText := FloatToStr(Position);

    if FFormat <> '' then
    begin
      PosText := SysUtils.Format(FFormat, [Position]);
    end;

    g.MeasureString(PosText, Length(PosText), gpfont, r, tr);
    tr.X := PosCenter.X - tr.Width / 2;
    tr.Y := PosCenter.Y - tr.Height / 2;
    g.DrawString(PosText, Length(PosText), gpfont, MakePoint(tr.X, tr.Y), bp);

    //Draw the EndPositionCircle
    PosCenter.X := Center.X + Cos(GetRadian(FDegEnd + StartAngle)) * circleRad;
    PosCenter.Y := Center.Y + Sin(GetRadian(FDegEnd + StartAngle)) * circleRad;
    dr.X := Poscenter.X - endPosRad;
    dr.Y := PosCenter.Y - endPosRad;
    dr.Width := endPosRad * 2;
    dr.Height := dr.Width;
    g.FillEllipse(b, dr);
    g.DrawEllipse(p, dr);

    //Draw the Position Text
    PosText := FloatToStr(EndPosition);
    g.MeasureString(PosText, Length(PosText), gpfont, r, tr);
    tr.X := PosCenter.X - tr.Width / 2;
    tr.Y := PosCenter.Y - tr.Height / 2;
    g.DrawString(PosText, Length(PosText), gpfont, MakePoint(tr.X, tr.Y), bp);
  finally
    bp.free;
    gpfont.Free;
    bgPen.Free;
    p.Free;
  end;
end;

function TRangeCircleItem.GetDisplayName: string;
begin
  Result := 'Circle ' + inttostr(Index);
end;

function TRangeCircleItem.GetEndPosition: Double;
begin
  Result := FEndPosition;
end;

function TRangeCircleItem.GetFormat: string;
begin
  Result := FFormat;
end;

function TRangeCircleItem.GetIsRange: Boolean;
begin
  Result := true;
end;

function TRangeCircleItem.GetMaximum: Double;
begin
  Result := FMaximum;
end;

function TRangeCircleItem.GetMinimum: Double;
begin
  Result := FMinimum;
end;

function TRangeCircleItem.GetPosition: Double;
begin
  Result := FPosition;
end;


function TRangeCircleItem.GetValueDegrees(MinVal, MaxVal, Value: Double): Double;
begin
  Result := MaxVal - MinVal;
  Result := (Value - MinVal) / Result;
  Result := 360 * Result;
end;

function TRangeCircleItem.InRangeOrLoading(Value: Double): Boolean;
begin
  Result := FOwner.Loading or ((Value >= FMinimum) and (Value <= FMaximum));
end;

function TRangeCircleItem.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

procedure TRangeCircleItem.SetColor(const Value: TColor);
begin
  if Value <> FColor then
  begin
    FColor := Value;
    Changed;
  end;
end;

procedure TRangeCircleItem.SetEndPosition(const Value: Double);
begin
  if (FEndPosition <> Value) and InRangeOrLoading(Value) then
  begin
    FEndPosition := Value;
    FDegEnd := GetValueDegrees(FMinimum, FMaximum, FEndPosition);
    Changed;
  end;
end;

procedure TRangeCircleItem.SetFormat(const Value: string);
begin
  FFormat := Value;
end;

procedure TRangeCircleItem.SetMaximum(const Value: Double);
begin
  if (FMaximum <> Value) and (FOwner.Loading or (Value > FMinimum)) then
  begin
    FMaximum := Value;
    FDegStart := GetValueDegrees(FMinimum, FMaximum, FPosition);
    FDegEnd := GetValueDegrees(FMinimum, FMaximum, FEndPosition);
    Changed;
  end;
end;

procedure TRangeCircleItem.SetMinimum(const Value: Double);
begin
  if (FMinimum <> Value) and (FOwner.Loading or (Value < FMaximum))then
  begin
    FMinimum := Value;
    FDegStart := GetValueDegrees(FMinimum, FMaximum, FPosition);
    FDegEnd := GetValueDegrees(FMinimum, FMaximum, FEndPosition);
    Changed;
  end;
end;

procedure TRangeCircleItem.SetPosition(const Value: Double);
begin
  if (FPosition <> Value) and InRangeOrLoading(Value) then
  begin
    FPosition := Value;
    FDegStart := GetValueDegrees(FMinimum, FMaximum, FPosition);
    Changed;
  end;
end;

procedure TRangeCircleItem.SetWidth(const Value: Integer);
begin
  if FWidth <> Value then
  begin
    FWidth := Value;
    Changed;
  end;
end;

function TRangeCircleItem._AddRef: Integer;
begin
{$IFDEF DELPHIXE4_LVL}
{$IFNDEF AUTOREFCOUNT}
  Result := AtomicIncrement(FRefCount);
{$ELSE}
  Result := __ObjAddRef;
{$ENDIF}
{$ELSE}
  Result := InterlockedIncrement(FRefCount)
{$ENDIF}
end;

function TRangeCircleItem._Release: Integer;
begin
{$IFDEF DELPHIXE4_LVL}
{$IFNDEF AUTOREFCOUNT}
  Result := AtomicDecrement(FRefCount);
  if Result = 0 then
    Destroy;
{$ELSE}
  Result := __ObjRelease;
{$ENDIF}
{$ELSE}
  Result := InterlockedDecrement(FRefCount)
{$ENDIF}
end;

{ TRangeCircles }

function TRangeCircles.Add: TRangeCircleItem;
begin
  Result := TRangeCircleItem(inherited Add);
end;

constructor TRangeCircles.Create(AOwner: TAdvRangeCircles);
begin
  inherited Create(AOwner, TRangeCircleItem);
end;

destructor TRangeCircles.Destroy;
begin
  inherited;
end;

function TRangeCircles.GetItem(Index: Integer): TRangeCircleItem;
begin
  Result := TRangeCircleItem(inherited GetItem(Index));
end;

function TRangeCircles.Insert(Index: Integer): TRangeCircleItem;
begin
  Result := TRangeCircleItem(inherited Insert(Index));
end;

procedure TRangeCircles.SetItem(Index: Integer; const Value: TRangeCircleItem);
begin
  inherited SetItem(Index, Value);
end;

procedure TRangeCircles.Update(Item: TCollectionItem);
begin
  inherited;
  (Owner as TAdvRangeCircles).Invalidate;
end;

end.
