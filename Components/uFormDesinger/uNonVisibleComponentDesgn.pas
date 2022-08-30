unit uNonVisibleComponentDesgn;
//本单元实现对非可视化组件的操控
interface
uses
  Windows, messages, SysUtils, Variants, Classes, Graphics, Controls, ExtCtrls;

type
  ///用来显示非可视组件的名称
  TNonVisibleComponentName  = class(TCustomControl)
  private
    FName: String;
    FWidth: Integer;
    FHeight: Integer;
  protected
    procedure Paint; override;
    procedure SetComponentName(const Value: String);
  end;

  TNonVisibleComponentDesgn = class(TCustomControl)
  private
    FGUID: string;
    FGlyph: TBitmap;
    FContainComponent: TComponent;
    FComponentName: TNonVisibleComponentName;
    procedure SetGlyph(const Value: TBitmap);
    procedure UpdateComponentBound;
  protected
    procedure Paint; override;
    procedure SetZOrder(TopMost: Boolean); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent; AContainComponent: TComponent; AParent: TWinControl);
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure UpdateComponentName;
    property GUID: string read FGUID;  //用来做对象区分
    property ContainComponent: TComponent read FContainComponent write FContainComponent;
  published
    property ICon: TBitmap read FGlyph write SetGlyph;
  end;

implementation

constructor TNonVisibleComponentDesgn.Create(AOwner: TComponent;
  AContainComponent: TComponent; AParent: TWinControl);
begin
  inherited Create(AOwner);
  Color                 :=  clBtnface;
  Parent                :=  AParent;
  FContainComponent     :=  AContainComponent;
  FComponentName        :=  TNonVisibleComponentName.Create(Self);
  FComponentName.FreeNotification(Self);
  FComponentName.Parent :=  AParent;
  FComponentName.SetComponentName(AContainComponent.Name);
  FGlyph                :=  nil;
  ControlStyle          :=  ControlStyle + [csReplicatable];
  Width                 :=  28;
  Height                :=  28;
  FGUID                 :=  '{2FBE9C33-E6DD-4B18-AB30-4D2AA4C0B543}';
end;

destructor TNonVisibleComponentDesgn.destroy;
begin
  FGlyph  :=  nil;
  FreeAndNil(FComponentName);
  inherited;
end;

procedure TNonVisibleComponentDesgn.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation=opRemove) and (AComponent=FComponentName) then
    FComponentName  :=  nil;
end;

procedure TNonVisibleComponentDesgn.Paint;
var
  Color1, Color2: TColor;

  procedure DrawBtnIcon(Canvas: TCanvas; ARect: TRect);
  var Pos: TPoint;
      col: TColor;
  begin
   if not Assigned(FGlyph) or FGlyph.Empty then Exit;
   Pos.x := ARect.Left + (ARect.Right -ARect.Left - FGlyph.Width) div 2;
   Pos.y := ARect.Top + (ARect.Bottom -ARect.Top - FGlyph.Height) div 2;
   Canvas.BrushCopy(Bounds(Pos.x, Pos.y, FGlyph.Width, FGlyph.Height), FGlyph,
        Rect(0,0, FGlyph.Width, FGlyph.Height), FGlyph.Canvas.Pixels[0, FGlyph.Height - 1]);
  end;

  procedure BevelRect(const R: TRect);
  begin
    with Canvas do
    begin
      Pen.Color := Color1;
      PolyLine([Point(R.Left, R.Bottom), Point(R.Left, R.Top),
        Point(R.Right, R.Top)]);
      Pen.Color := Color2;
      PolyLine([Point(R.Right, R.Top), Point(R.Right, R.Bottom),
        Point(R.Left, R.Bottom)]);
    end;
  end;
begin
  inherited;
  with Canvas do
  begin
    Brush.Color  :=  clBtnface;
    FillRect(ClientRect);
    Pen.Style   := psSolid;
    Pen.Mode    := pmCopy;
    Pen.Color   := clBlack;
    Brush.Style := bsSolid;
    Pen.Width   := 1;
    Color1      := clBtnHighlight;
    Color2      := clBtnShadow;
    BevelRect(Rect(0, 0, Width - 1, Height - 1));
    DrawBtnIcon(Canvas, ClientRect);
  end;
end;

procedure TNonVisibleComponentDesgn.SetBounds(ALeft, ATop, AWidth,
  AHeight: Integer);
var
  LongR: ^Longint;
begin
  inherited SetBounds(ALeft, ATop, 28, 28);
  if  ContainComponent <> nil then
  begin
    LongR :=  @(ContainComponent.DesignInfo);
    LongRec(LongR^).Lo  :=  Left;
    LongRec(LongR^).Hi  :=  Top;

    FComponentName.Left :=  Self.Left + Round((Self.Width-FComponentName.Width)/2);
    FComponentName.Top  :=  Top  + Height;
  end;
  BringToFront;
  FComponentName.BringToFront;
end;

procedure TNonVisibleComponentDesgn.SetGlyph(const Value: TBitmap);
begin
  FGlyph := Value;
  Invalidate;
end;

procedure TNonVisibleComponentDesgn.SetZOrder(TopMost: Boolean);
begin
  FComponentName.SetZOrder(TopMost);
  inherited;
end;

procedure TNonVisibleComponentDesgn.UpdateComponentBound;
begin
  FComponentName.Left :=  Left + Round((Self.Width-FComponentName.Width)/2);
  FComponentName.Top  :=  Top  + Height;
end;

procedure TNonVisibleComponentDesgn.UpdateComponentName;
begin
  FComponentName.SetComponentName(FContainComponent.Name);
  UpdateComponentBound;
  FComponentName.Invalidate;
end;

{ TNonVisibleComponentName }
procedure TNonVisibleComponentName.Paint;
var
  Rect: TRect;
//  DrawStyle: Longint;
begin
  inherited;
  with Canvas do
  begin
    Brush.Color  :=  clBtnface;
    FillRect(ClientRect);

    Rect := ClientRect;
    Brush.Style := bsClear;
//    DrawStyle := DT_EXPANDTABS;
    TextOut(Rect.Left, Rect.Top, FName);
  end;
end;

procedure TNonVisibleComponentName.SetComponentName(const Value: String);
begin
  if FName<>Value then
  begin
    FName   :=  Value;
    FWidth  :=  Canvas.TextWidth(FName);
    FHeight :=  Canvas.TextHeight(Value);
    Width   :=  FWidth;
    Height  :=  FHeight;
  end;
end;

end.
