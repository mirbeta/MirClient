{***************************************************************************}
{ TAdvShape component                                                       }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2007 - 2012                                        }
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

unit AdvShape;

{$I TMSDEFS.INC}

interface

uses
  Classes, Windows, Forms, Dialogs, Controls, Graphics, Messages, SysUtils, Math,
  PictureContainer, CommCtrl, ShellAPI, JPEG, ImgList, Types
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;

const

  MAJ_VER = 1; // Major version nr.
  MIN_VER = 2; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 0; // Build nr.

  // version history
  // 1.0.0.0 : first release
  // 1.0.0.1 : fixed memory leak issue
  // 1.1.0.0 : New : AutoSize support added
  //         : New : stLine shape type added
  //         : Improved : drawing of border with Pen.Width > 1
  // 1.1.1.0 : New : OnMouseEnter, OnMouseLeave events added
  //         : Fixed : issue with setting Cursor to crHandPoint
  // 1.1.2.0 : New : support for customizing bullets in HTML UL lists
  // 1.2.0.0 : New : Property MaxSize added 
  //         : Improved : Gradient background drawing with shape stSquare, stRoundSquare 

type

  TShapeDrawer = class;

  TAdvShapeType = (stRectangle, stSquare, stRoundRect, stRoundSquare, stEllipse, stCircle, stTriangle, stStar, stLine);

  TGradientDirection = (gdHorizontal, gdVertical);

  TAdvBevel = (bsNone, bsLowered, bsRaised);

  TShowTextType = (stClipped, stNonClipped);

  TBackGroundPosition = (bpTopLeft,bpTopRight,bpBottomLeft,bpBottomRight,bpTiled,bpStretched,bpCenter);
  
  TPointArray = array of TPoint;

  TAnchorClick = procedure(Sender: TObject; Anchor: string) of object;

  TShapeAppearance = class(TPersistent)
  private
    FPen: TPen;
    FBrush: TBrush;
    FSteps: Integer;
    FColor: TColor;
    FColorTo: TColor;
    FDirection: TGradientDirection;
    FOnChange: TNotifyEvent;
    FURLColor: TColor;
    procedure SetColor(const Value: TColor);
    procedure SetColorTo(const Value: TColor);
    procedure SetDirection(const Value: TGradientDirection);
    procedure SetSteps(const Value: Integer);
    procedure Changed;
    procedure StyleChanged(Sender: TObject);
    procedure SetURLColor(const Value: TColor);
    procedure SetBrush(Value: TBrush);
    procedure SetPen(Value: TPen);
  protected
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Brush: TBrush read FBrush write SetBrush;
    property Color: TColor read FColor write SetColor;
    property ColorTo: TColor read FColorTo write SetColorTo;
    property Direction: TGradientDirection read FDirection write SetDirection;
    property Pen: TPen read FPen write SetPen;
    property Steps: Integer read FSteps write SetSteps default 64;
    property URLColor: TColor read FURLColor write SetURLColor;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TShapeBackGround = class(TPersistent)
  private
    FOnChange: TNotifyEvent;
    FPosition: TBackGroundPosition;
    FPicture: TPicture;
    procedure Changed;
    procedure OnPictureChanged(Sender: TObject);
    procedure SetPicture(const Value: TPicture);
    procedure SetPosition(const Value: TBackGroundPosition);
  protected
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Position: TBackGroundPosition read FPosition write SetPosition;
    property Picture: TPicture read FPicture write SetPicture;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvShape = class(TGraphicControl)
  private
    FShape: TAdvShapeType;
    FRotationAngle: Double;
    FDrawer: TShapeDrawer;
    FShapeWidth: Integer;
    FShapeHeight: Integer;
    FText: string;
    FContainer: TPictureContainer;
    FImages: TImageList;
    FAnchor: string;
    fAnchorClick: TAnchorClick;
    FAppearance: TShapeAppearance;
    FTextOffsetY: Integer;
    FTextOffsetX: Integer;
    FBevel: TAdvBevel;
    FBackGround: TShapeBackGround;
    FAnchorExit: TAnchorClick;
    FAnchorEnter: TAnchorClick;
    FShowText: TShowTextType;
    FRounding: integer;
    FAutoSize: boolean;
    FOldCursor: TCursor;
    FOnMouseLeave: TNotifyEvent;
    FOnMouseEnter: TNotifyEvent;
    FMaxSize: Boolean;
    FClipping: Boolean;
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure OnBackGroundChanged(Sender: TObject);
    procedure OnAppearanceChanged(Sender: TObject);
    function IsAnchor(x, y: integer): string;
    procedure SetShape(Value: TAdvShapeType);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    procedure SetRotationAngle(const Value: Double);
    procedure SetShapeHeight(const Value: Integer);
    procedure SetShapeWidth(const Value: Integer);
    procedure SetText(const Value: string);
    procedure SetImages(const Value: TImageList);
    procedure SetAppearance(const Value: TShapeAppearance);
    function GetBottom: Integer;
    function GetRight: Integer;
    procedure SetTextOffsetX(const Value: Integer);
    procedure SetTextOffsetY(const Value: Integer);
    procedure SetBevel(const Value: TAdvBevel);
    procedure SetBackGround(const Value: TShapeBackGround);
    procedure SetShowText(const Value: TShowTextType);
    procedure SetRounding(const Value: integer);
    function GetShapeHeight: Integer;
    function GetShapeWidth: Integer;
    procedure SetAutoSizeEx(const Value: boolean);
    procedure SetMaxSize(const Value: Boolean);
  protected
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;

    function ShapeRect: TRect;
    function GetBoundsPts: TPointArray;
    function GetSurroundRgn: TPointArray;
    function GetShapeClipRegion: HRGN;
    function SurroundRgn: TPointArray;
    procedure ClipShape(Canvas: TCanvas);
    procedure UnclipShape(Canvas: TCanvas);
    procedure ClipBoundry(Canvas: TCanvas);
    property Drawer: TShapeDrawer read FDrawer;
    property Right: Integer read GetRight;
    property Bottom: Integer read GetBottom;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetVersionNr: Integer; virtual;
    property Clipping: boolean read FClipping write FClipping;
  published
    property Appearance: TShapeAppearance read FAppearance write SetAppearance;
    property Align;
    property Anchors;
    property AutoSize: boolean read FAutoSize write SetAutoSizeEx default False;
    property MaxSize: Boolean read FMaxSize write SetMaxSize default True; // Works with AutoSize when angle = 0
    property Bevel: TAdvBevel read FBevel write SetBevel default bsNone;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property Constraints;
    property Images: TImageList read FImages write SetImages;
    property ParentShowHint;
    property BackGround: TShapeBackGround read FBackGround write SetBackGround;
    property PictureContainer: TPictureContainer read FContainer write FContainer;
    property RotationAngle: Double read FRotationAngle write SetRotationAngle;
    property Rounding: integer read FRounding write SetRounding default 10;
    property Shape: TAdvShapeType read FShape write SetShape default stRectangle;
    property ShapeHeight: Integer read GetShapeHeight write SetShapeHeight;
    property ShapeWidth: Integer read GetShapeWidth write SetShapeWidth;
    property ShowText: TShowTextType read FShowText write SetShowText default stNonClipped;
    property ShowHint;
    property Text: string read FText write SetText;
    property TextOffsetX: Integer read FTextOffsetX write SetTextOffsetX;
    property TextOffsetY: Integer read FTextOffsetY write SetTextOffsetY;
    property Version: string read GetVersion write SetVersion;
    property Visible;
    property OnClick;
    property OnAnchorClick: TAnchorClick read fAnchorClick write fAnchorClick;
    property OnAnchorEnter: TAnchorClick read FAnchorEnter write FAnchorEnter;
    property OnAnchorExit: TAnchorClick read FAnchorExit write FAnchorExit;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnContextPopup;
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


  TPointX = record
    X: double;
    Y: double;
  end;

  TPointXArray = array of TPointX;

  TRotationType = (rtCurRectCenter, rtRotationCenter);

  TLineOrientation  = (loPoint, loHorizontal, loVertical);

  TShapeDrawer = class
  private
    FRect: TRect;
    FOrRect: TRect;
    FAngle: double;
    FRadAngle: double;
    FCanvas: TCanvas;
    FSinAngle: double;
    FCosAngle: double;
    FRotationType: TRotationType;
    FRotationCenter: TPoint;
    procedure SetAngle(const Value: double);
  public
    constructor Create;
    {Draw functions}
    procedure DrawRotRoundRect(X1, Y1, X2, Y2, X3, Y3: integer);
    procedure DrawRotRect(X1, Y1, X2, Y2: integer);
    procedure DrawRotPoly(P: TPointArray); overload;
    procedure DrawRotPoly(P: array of TPoint); overload;
    procedure AutoLineTo(X, Y: double);
    procedure AutoMoveTo(X, Y: double);
    {Rotate point in relation to center of rect}
    function RotPoly(Pts: TPointArray): TPointArray; overload;
    function RotPoly(P: array of TPoint): TPointArray; overload;
    function UnrotX(AX, AY: double): TPointX; overload;
    function UnrotX(AP: TPointX): TPointX; overload;
    function Unrot(AP: TPoint): TPoint; overload;
    function Unrot(AX, AY: double): TPoint; overload;
    function RotX(AX, AY: double): TPointX; overload;
    function RotX(AP: TPointX): TPointX; overload;
    function Rot(AP: TPoint): TPoint; overload;
    function Rot(AX, AY: double): TPoint; overload;
    function RotByCenter(R: TRect): TRect;
    {P Points keep ratio between original and current rects}
    function PPPoints(Pts: TPointXArray): TPointArray;
    function PP(AP: TPointX): TPoint; overload;
    function PP(AX, AY: double): TPoint; overload;
    function PX(AX: double): integer; virtual;
    function PY(AY: double): integer; virtual;
    {Inverse of P_ functions}
    function UnPP(AP: TPointX): TPoint; overload;
    function UnPP(Ax, AY: double): TPoint; overload;
    function UnPx(AX: double): integer;
    function UnPY(AY: double): integer;
    {R Points keep same bottom/right distance between original and current rects}
    function RP(AX, AY: double): TPoint;
    function RX(AX: double): integer;
    function RY(AY: double): integer;
    {L Points keep same top/left distance between original and current rects}
    function LP(AX, AY: double): TPoint;
    function LX(AX: double): integer;
    function LY(AY: double): integer;
    {Rotation angle}
    property Angle: double read FAngle write SetAngle;
    property RotationType: TRotationType read FRotationType write FRotationType;
    property RotationCenter: TPoint read FRotationCenter write FRotationCenter;
    property OriginalRect: TRect read FOrRect write FOrRect;
    property CurRect: TRect read FRect write FRect;
    property Canvas: TCanvas read FCanvas write FCanvas;
  end;

function PointX(P: TPoint): TPointX; overload;
function PointX(X, Y: double): TPointX; overload;
function RoundPoint(APoint: TPointX): TPoint;

implementation

{$I HtmlEngo.pas}

const PixelMax = 32768;

type
   pPixelArray = ^TPixelArray;
   TPixelArray = array[0..PixelMax-1] of TRGBTriple;

//------------------------------------------------------------------------------

function RoundPoint(APoint: TPointX): TPoint;
begin
  result := Point(round(APoint.X), round(APoint.Y));
end;

//------------------------------------------------------------------------------

function PointX(X, Y: double): TPointX;
begin
  result.X := X;
  result.Y := Y;
end;

//------------------------------------------------------------------------------

function PointX(P: TPoint): TPointX;
begin
  result := PointX(P.X, P.Y);
end;

//------------------------------------------------------------------------------

function GetPolyEllipse(Left, Top, Right, Bottom: Integer;
  StartAngle: integer = 0; EndAngle: integer = 360; ClockWise: boolean = true): TPointArray;
var
  A, B, Max: Double;
  dTheta: Double;
  N: Double;
  I: Integer;
  U, V: Integer;

  procedure FindUV(Theta: double);
  var
    X, Y: Double;
  begin
    X := A * Cos(Theta);
    Y := B * Sin(Theta);
    U := Round(X + (Right + Left) / 2.0);
    V := Round(Y + (Top + Bottom) / 2.0)
  end;

var
  I1, I2: integer;
begin
  {The "+ 1" below in right and bottom points is for Windows compatible -
   summing 1 to right and bottom edges will draw an ellipse in the exact
   way Windows does}
  A := Abs((Right - Left) + 1) / 2.0;
  B := Abs((Bottom - Top) + 1) / 2.0;
  if (A <> 0) and (B <> 0) then
  begin
    Max := A;
    if Abs(B) > Abs(A) then
      Max := B;
    dTheta := 1 / Max;
    N := 2 * Pi / dTheta;
    dTheta := 2 * Pi / Round(N);
    I1 := round(StartAngle * N / 360);
    I2 := round(EndAngle * N / 360);
    SetLength(result, I2 - I1 + 2);
    if ClockWise then
    begin
      FindUV(I1 * dTheta);
      result[0] := Point(U, V);
      for I := I1 to I2 do
      begin
        FindUV(I * dTheta);
        result[I - I1 + 1] := Point(U, V);
      end;
    end
    else
    begin
      FindUV(I2 * dTheta);
      result[0] := Point(U, V);
      for I := I2 downto I1 do
      begin
        FindUV(I * dTheta);
        result[I2 - I + 1] := Point(U, V);
      end;
    end;
  end else
  begin
    SetLength(result, 2);
    result[0] := Point(Left, Top);
    result[1] := Point(Right, Bottom);
  end;
end;

//------------------------------------------------------------------------------

function GetPolyRoundRect(X1, Y1, X2, Y2, X3, Y3: Integer): TPointArray;
var
  P1, P2, P3, P4: TPointArray;
  c: integer;
begin
  {Windows compatibility}
  P1 := GetPolyEllipse(X1, Y1, X1 + X3 - 1, Y1 + Y3 - 1, 181, 269);
  P2 := GetPolyEllipse(X2, Y1, X2 - X3 - 1, Y1 + Y3 - 1, 269, 359);
  P3 := GetPolyEllipse(X2, Y2, X2 - X3 - 1, Y2 - Y3 - 1, 1, 89);
  P4 := GetPolyEllipse(X1, Y2, X1 + X3 - 1, Y2 - Y3 - 1, 91, 179);
  SetLength(result, Length(P1) + Length(P2) + Length(P3) + Length(P4) + 1);
  for c := 0 to Length(P1) - 1 do
    result[c] := P1[c];
  for c := 0 to Length(P2) - 1 do
    result[c + Length(P1)] := P2[c];
  for c := 0 to Length(P3) - 1 do
    result[c + Length(P1) + Length(P2)] := P3[c];
  for c := 0 to Length(P4) - 1 do
    result[c + Length(P1) + Length(P2) + Length(P3)] := P4[c];
  result[Length(result) - 1] := result[0];
end;

//------------------------------------------------------------------------------

function GetTriangle(Left, Top, Right, Bottom: Integer): TPointArray;
begin
  SetLength(Result, 3);
  Result[0] := Point(Left, Bottom);
  Result[1] := Point(Left + (Right - Left) div 2, Top);
  Result[2] := Point(Right, Bottom);
end;

//------------------------------------------------------------------------------

function GetRectangle(Left, Top, Right, Bottom: Integer): TPointArray;
begin
  SetLength(Result, 4);
  Result[0] := Point(Left, Top);
  Result[1] := Point(Right, Top);
  Result[2] := Point(Right, Bottom);
  Result[3] := Point(Left, Bottom);
end;

//------------------------------------------------------------------------------

function GetStar(Left, Top, Right, Bottom: Integer): TPointArray;
var
  h, w, h2, w2, h4, w3: integer;
begin
  h := Bottom - Top;
  w := Right - Left;
  h2 := h div 2;
  w2 := w div 2;
  h4 := h div 4;
  w3 := w div 3;
  SetLength(Result, 12);
  Result[0] := Point(Left, Top + h4);
  Result[1] := Point(Left + w3, Top + h4);
  Result[2] := Point(Left + w2, Top);
  Result[3] := Point(Left + w3 * 2, Top + h4);
  Result[4] := Point(Left + w3 * 3, Top + h4);
  Result[5] := Point(Left + w3 * 2 + (w3 div 2), Top + h2);
  Result[6] := Point(Left + w3 * 3, Top + h4 * 3);
  Result[7] := Point(Left + w3 * 2, Top + h4 * 3);
  Result[8] := Point(Left + w2, Top + h4 * 4);
  Result[9] := Point(Left + w3, Top + h4 * 3);
  Result[10] := Point(Left, Top + h4 * 3);
  Result[11] := Point(Left + (w3 div 2), Top + h4 * 2);
end;

//------------------------------------------------------------------------------

{ TShapeDrawer }

function TShapeDrawer.LP(AX, AY: double): TPoint;
begin
  result := Point(LX(AX), LY(AY));
end;

//------------------------------------------------------------------------------

function TShapeDrawer.LX(AX: double): integer;
begin
  result := round(FRect.Left + (AX - FOrRect.Left));
end;

//------------------------------------------------------------------------------

function TShapeDrawer.LY(AY: double): integer;
begin
  result := round(FRect.Top + (AY - FOrRect.Top));
end;

//------------------------------------------------------------------------------

function TShapeDrawer.PPPoints(Pts: TPointXArray): TPointArray;
var
  c: integer;
begin
  SetLength(result, Length(Pts));
  for c := 0 to Length(Pts) - 1 do
    result[c] := PP(Pts[c].X, Pts[c].Y);
end;

//------------------------------------------------------------------------------

function TShapeDrawer.PP(AP: TPointX): TPoint;
begin
  result := PP(AP.X, AP.Y);
end;

//------------------------------------------------------------------------------

function TShapeDrawer.PP(AX, AY: double): TPoint;
begin
  result := Point(PX(AX), PY(AY));
end;

//------------------------------------------------------------------------------

function TShapeDrawer.PX(AX: double): integer;
begin
  if FOrRect.Right = FOrRect.Left then
    result := FRect.Left
  else
    result := round((AX - FOrRect.Left) / (FOrRect.Right - FOrRect.Left) * (FRect.Right - FRect.Left)) + FRect.Left;
end;

//------------------------------------------------------------------------------

function TShapeDrawer.PY(AY: double): integer;
begin
  if FOrRect.Bottom = FOrRect.Top then
    result := FRect.Top
  else
    result := round((AY - FOrRect.Top) / (FOrRect.Bottom - FOrRect.Top) * (FRect.Bottom - FRect.Top)) + FRect.Top;
end;

//------------------------------------------------------------------------------

function TShapeDrawer.UnPP(AP: TPointX): TPoint;
begin
  result := UnPP(AP.X, AP.Y);
end;

//------------------------------------------------------------------------------

function TShapeDrawer.UnPP(AX, AY: double): TPoint;
begin
  result := Point(UnPX(AX), UnPY(AY));
end;

//------------------------------------------------------------------------------

function TShapeDrawer.UnPX(AX: double): integer;
begin
  if FRect.Right = FRect.Left then
    result := FOrRect.Left
  else
    result := round((AX - FRect.Left) / (FRect.Right - FRect.Left) * (FOrRect.Right - FOrRect.Left)) + FOrRect.Left;
end;

//------------------------------------------------------------------------------

function TShapeDrawer.UnPY(AY: double): integer;
begin
  if FRect.Bottom = FRect.Top then
    result := FOrRect.Top
  else
    result := round((AY - FRect.Top) / (FRect.Bottom - FRect.Top) * (FOrRect.Bottom - FOrRect.Top)) + FOrRect.Top;
end;

//------------------------------------------------------------------------------

function TShapeDrawer.RP(AX, AY: double): TPoint;
begin
  result := Point(RX(AX), RY(AY));
end;

//------------------------------------------------------------------------------

function TShapeDrawer.RX(AX: double): integer;
begin
  result := round(FRect.Right - (FOrRect.Right - AX));
end;

//------------------------------------------------------------------------------

function TShapeDrawer.RY(AY: double): integer;
begin
  result := round(FRect.Bottom - (FOrRect.Bottom - AY));
end;

//------------------------------------------------------------------------------

function TShapeDrawer.Rot(AP: TPoint): TPoint;
begin
  result := Rot(AP.X, AP.Y);
end;

//------------------------------------------------------------------------------

function TShapeDrawer.Rot(AX, AY: double): TPoint;
begin
  result := RoundPoint(RotX(AX, AY));
end;

//------------------------------------------------------------------------------

function TShapeDrawer.RotX(AP: TPointX): TPointX;
begin
  result := RotX(AP.X, AP.Y);
end;

//------------------------------------------------------------------------------

function TShapeDrawer.RotX(AX, AY: double): TPointX;
var CX, CY: double;
begin
  {Optimization}
  if Angle = 0 then
    Result := PointX(AX, AY)
  else
  begin
    Case RotationType of
      rtCurRectCenter:
        begin
          CX := (FRect.Left + FRect.Right) / 2;
          CY := (FRect.Top + FRect.Bottom) / 2;
        end;
    else {rtRotationCenter}
      CX := RotationCenter.X;
      CY := RotationCenter.Y;
    end;
    Result.X := CX + FCosAngle*(AX - CX) - FSinAngle*(AY - CY);
    Result.Y := CY + FSinAngle*(AX - CX) + FCosAngle*(AY - CY);
  end;
end;

//------------------------------------------------------------------------------

function TShapeDrawer.Unrot(AP: TPoint): TPoint;
begin
  result := Unrot(AP.X, AP.Y);
end;

//------------------------------------------------------------------------------

function TShapeDrawer.Unrot(AX, AY: double): TPoint;
begin
  result := RoundPoint(UnrotX(AX, AY));
end;

//------------------------------------------------------------------------------

function TShapeDrawer.UnrotX(AP: TPointX): TPointX;
begin
  result := UnrotX(AP.X, AP.Y);
end;

//------------------------------------------------------------------------------

function TShapeDrawer.UnrotX(AX, AY: double): TPointX;
begin
  Angle := -Angle;
  result := RotX(AX, AY);
  Angle := -Angle;
end;

//------------------------------------------------------------------------------

procedure TShapeDrawer.SetAngle(const Value: double);
begin
  FAngle := Value;
  FRadAngle := FAngle * pi / 180;
  FSinAngle := Sin(FRadAngle);
  FCosAngle := Cos(FRadAngle);
end;

//------------------------------------------------------------------------------

function SubPoint(P1, P2: TPoint): TPoint;
begin
  result := Point(P1.X - P2.X, P1.Y - P2.Y);
end;

//------------------------------------------------------------------------------

function AddPoint(P1, P2: TPoint): TPoint;
begin
  result := Point(P1.X + P2.X, P1.Y + P2.Y);
end;

//------------------------------------------------------------------------------

function AddPointX(P1, P2: TPointX): TPointX;
begin
  result := PointX(P1.X + P2.X, P1.Y + P2.Y);
end;

//------------------------------------------------------------------------------

procedure TShapeDrawer.DrawRotRect(X1, Y1, X2, Y2: integer);
begin
  if Canvas <> nil then
  begin
    if Angle = 0 then
      Canvas.Rectangle(X1, Y1, X2 + 1, Y2 + 1)
    else
      Canvas.Polygon([Rot(X1, Y1), Rot(X2, Y1),
        Rot(X2, Y2), Rot(X1, Y2), Rot(X1, Y1)]);
  end;
end;

//------------------------------------------------------------------------------

procedure TShapeDrawer.DrawRotPoly(P: TPointArray);
begin
  if Canvas <> nil then
    if Angle = 0 then
      Canvas.Polygon(P)
    else
      Canvas.Polygon(RotPoly(P));
end;

//------------------------------------------------------------------------------

procedure TShapeDrawer.DrawRotPoly(P: array of TPoint);
var
  Pts: TPointArray;
  c: integer;
begin
  SetLength(Pts, Length(P));
  for c := 0 to Length(P) - 1 do
    Pts[c] := P[c];
  DrawRotPoly(Pts);
end;

//------------------------------------------------------------------------------

function TShapeDrawer.RotPoly(Pts: TPointArray): TPointArray;
var
  c: integer;
begin
  SetLength(result, Length(Pts));
  for c := 0 to Length(Pts) - 1 do
    result[c] := Rot(Pts[c]);
end;

//------------------------------------------------------------------------------

function TShapeDrawer.RotPoly(P: array of TPoint): TPointArray;
var
  c: integer;
begin
  SetLength(result, Length(P));
  for c := 0 to Length(P) - 1 do
    result[c] := Rot(P[c]);
end;

//------------------------------------------------------------------------------

procedure TShapeDrawer.DrawRotRoundRect(X1, Y1, X2, Y2, X3, Y3: integer);
begin
  if Canvas <> nil then
  begin
    if Angle = 0 then
      Canvas.RoundRect(X1, Y1, X2 + 1, Y2 + 1, X3, Y3)
    else
      Canvas.Polygon(RotPoly(GetPolyRoundRect(X1, Y1, X2, Y2, X3, Y3)));
  end;
end;

//------------------------------------------------------------------------------

procedure TShapeDrawer.AutoLineTo(X, Y: double);
var
  P: TPoint;
begin
  if Canvas <> nil then
  begin
    P := Rot(PP(X, Y));
    Canvas.LineTo(P.X, P.Y);
  end;
end;

//------------------------------------------------------------------------------

procedure TShapeDrawer.AutoMoveTo(X, Y: double);
var
  P: TPoint;
begin
  if Canvas <> nil then
  begin
    P := Rot(PP(X, Y));
    Canvas.MoveTo(P.X, P.Y);
  end;
end;

//------------------------------------------------------------------------------

constructor TShapeDrawer.Create;
begin
  Angle := 0;
  RotationType := rtCurRectCenter;
  RotationCenter := Point(0, 0);
  OriginalRect := Rect(0, 0, 100, 100);
  CurRect := OriginalRect;
end;

//------------------------------------------------------------------------------

function TShapeDrawer.RotByCenter(R: TRect): TRect;
var
  C: TPoint;
  Delta: TPoint;
  P1, P2: TPoint;
begin
  C := Point((R.Left + R.Right) div 2, (R.Top + R.Bottom) div 2);
  Delta := SubPoint(Rot(C), C);
  P1 := AddPoint(R.TopLeft, Delta);
  P2 := AddPoint(R.BottomRight, Delta);
  result := Rect(P1.X, P1.Y, P2.X, P2.Y);
end;

//------------------------------------------------------------------------------

function RectCenter(R: TRect): TPoint;
begin
  result := Point((R.Left + R.Right) div 2, (R.Top + R.Bottom) div 2);
end;

//------------------------------------------------------------------------------

procedure DrawGradient(Canvas: TCanvas; FromColor, ToColor: TColor; Steps: Integer; R: TRect; Direction: Boolean);
var
  diffr, startr, endr: Integer;
  diffg, startg, endg: Integer;
  diffb, startb, endb: Integer;
  rstepr, rstepg, rstepb, rstepw: Real;
  i, stepw: Word;

begin
  if Steps = 0 then
    Steps := 1;

  FromColor := ColorToRGB(FromColor);
  ToColor := ColorToRGB(ToColor);

  startr := (FromColor and $0000FF);
  startg := (FromColor and $00FF00) shr 8;
  startb := (FromColor and $FF0000) shr 16;
  endr := (ToColor and $0000FF);
  endg := (ToColor and $00FF00) shr 8;
  endb := (ToColor and $FF0000) shr 16;

  diffr := endr - startr;
  diffg := endg - startg;
  diffb := endb - startb;

  rstepr := diffr / steps;
  rstepg := diffg / steps;
  rstepb := diffb / steps;

  if Direction then
    rstepw := (R.Right - R.Left) / Steps
  else
    rstepw := (R.Bottom - R.Top) / Steps;

  with Canvas do
  begin
    for i := 0 to steps - 1 do
    begin
      endr := startr + Round(rstepr * i);
      endg := startg + Round(rstepg * i);
      endb := startb + Round(rstepb * i);
      stepw := Round(i * rstepw);
      Pen.Color := endr + (endg shl 8) + (endb shl 16);
      Brush.Color := Pen.Color;

      if Direction then
      begin
        Rectangle(R.Left + stepw, R.Top, R.Left + stepw + Round(rstepw) + 1, R.Bottom)
      end
      else
      begin
        if R.Right <= R.Left + 1 then
        begin
          MoveTo(R.Left, R.Top + stepw);
          LineTo(R.Right, R.Top + stepw + Round(rstepw) + 1);
        end
        else
          Rectangle(R.Left, R.Top + stepw, R.Right, R.Top + stepw + Round(rstepw) + 1);
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

{ TShapeAppearance }

procedure TShapeAppearance.Assign(Source: TPersistent);
begin
  if (Source is TShapeAppearance) then
  begin
    FColor := (Source as TShapeAppearance).Color;
    FColorTo := (Source as TShapeAppearance).ColorTo;
    FDirection := (Source as TShapeAppearance).Direction;
    FSteps := (Source as TShapeAppearance).Steps;
    URLColor := (Source as TShapeAppearance).URLColor;
    FBrush.Assign((Source as TShapeAppearance).Brush);
    FPen.Assign((Source as TShapeAppearance).Pen);
  end;
end;

//------------------------------------------------------------------------------

procedure TShapeAppearance.Changed;
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

//------------------------------------------------------------------------------

constructor TShapeAppearance.Create;
begin
  inherited;
  FPen := TPen.Create;
  //FPen.Color := clBlack;
  FPen.OnChange := StyleChanged;
  FBrush := TBrush.Create;
  FBrush.Style := bsClear;
  FBrush.OnChange := StyleChanged;
  FColor := clWhite;
  FColorTo := clBtnFace;
  Steps := 64;
  Direction := gdHorizontal;
  FURLColor := clBlue;
end;

//------------------------------------------------------------------------------

destructor TShapeAppearance.Destroy;
begin
  FPen.Free;
  FBrush.Free;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TShapeAppearance.SetBrush(Value: TBrush);
begin
  FBrush.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TShapeAppearance.SetColor(const Value: TColor);
begin
  if (FColor <> Value) then
  begin
    FColor := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TShapeAppearance.SetColorTo(const Value: TColor);
begin
  FColorTo := Value;
  Changed;
end;

//------------------------------------------------------------------------------

procedure TShapeAppearance.SetDirection(
  const Value: TGradientDirection);
begin
  FDirection := Value;
  Changed;
end;

//------------------------------------------------------------------------------

procedure TShapeAppearance.SetPen(Value: TPen);
begin
  FPen.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TShapeAppearance.SetSteps(const Value: Integer);
begin
  FSteps := Value;
  Changed;
end;

//------------------------------------------------------------------------------

procedure TShapeAppearance.SetURLColor(const Value: TColor);
begin
  if (FURLColor <> Value) then
  begin
    FURLColor := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TShapeAppearance.StyleChanged(Sender: TObject);
begin
  Changed;
end;

//------------------------------------------------------------------------------

{ TAdvShape }

constructor TAdvShape.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable];
  Width := 120;
  Height := 120;
  FShapeHeight := 100;
  FShapeWidth := 100;
  FRotationAngle := 0.0;
  FTextOffsetX := 0;
  FTextOffsetY := 0;
  FBevel := bsNone;
  FShowText := stNonClipped;
  FRounding := 10;
  FOldCursor := crNone;
  FMaxSize := True;
  FClipping := True;

  FBackGround := TShapeBackGround.Create;
  FBackGround.OnChange := OnBackGroundChanged;
  FDrawer := TShapeDrawer.Create;
  FDrawer.OriginalRect := Rect(0, 0, 100, 100);
  FAppearance := TShapeAppearance.Create;
  FAppearance.OnChange := OnAppearanceChanged;
end;

//------------------------------------------------------------------------------

destructor TAdvShape.Destroy;
begin
  FDrawer.Free;
  FAppearance.Free;
  FBackGround.Free;
  inherited Destroy;
end;

//------------------------------------------------------------------------------

function TAdvShape.ShapeRect: TRect;
var
  X, Y, W, H{, S}: Integer;
begin
  Result := ClientRect;

  X := Appearance.Pen.Width div 2;
  Y := X;
  W := ShapeWidth - Appearance.Pen.Width + 1;
  H := ShapeHeight - Appearance.Pen.Width + 1;
  
  if Appearance.Pen.Width = 0 then
  begin
    Dec(W);
    Dec(H);
  end;
  {
  S := Min(W,H);

  if FShape in [stSquare, stRoundSquare, stCircle] then
  begin
    Inc(X, (W - S) div 2);
    Inc(Y, (H - S) div 2);
    W := S;
    H := S;
    Result := Rect(X, Y, X + W, Y + H);
  end
  else}
  begin
    Inc(X, (Self.Width - W) div 2);
    Inc(Y, (Self.Height - H) div 2);
    Result := Rect(X, Y, X + W, Y + H);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvShape.Paint;
var
  R, R2, re, TextR: TRect;
  Pts: TPointArray;
  anchor, stripped: string;
  xsize, ysize: Integer;
  HyperLinks, MouseLink, i, k, l: Integer;
  Focusanchor: string;
  Clr1, Clr2: TColor;
  xo, yo:integer;

  procedure BevelLine(C: TColor; P1, P2: TPoint);
  begin
    with Canvas do
    begin
      Pen.Color := C;
      MoveTo(P1.X, P1.Y);
      LineTo(P2.X, P2.Y);
    end;
  end;

  procedure BevelEllipse(Pts: TPointArray);
  begin
    l := Length(Pts);
    k := l div 2;
    i := k;
    Canvas.Pen.Color := Clr1;
    while (i < l) do
    begin
      if (i = k) then
        Canvas.MoveTo(Pts[i].X, Pts[i].Y)
      else
        Canvas.LineTo(Pts[i].X, Pts[i].Y);
      Inc(i);
    end;
  end;

begin
  if FClipping then
    ClipShape(Canvas);

  R := Rect(0, 0, ClientRect.Right - 1, ClientRect.Bottom);

  if Shape <> stLine then
  begin
    if (Appearance.Color <> clNone) and (Appearance.ColorTo <> clNone) then
      DrawGradient(Canvas, Appearance.Color, Appearance.ColorTo, Appearance.Steps, R, Appearance.Direction = gdHorizontal)
    else
      if (Appearance.Color <> clNone) and (Appearance.ColorTo = clNone) then
      begin
        Canvas.Brush.Color := Appearance.Color;
        Canvas.Brush.Style := bsSolid;
        Canvas.Pen.Style := psClear;
        Canvas.FillRect(R);
      end;
  end;

  R := ShapeRect;
  R2 := ClientRect;

  if Assigned(FBackGround.Picture.Graphic) and not FBackGround.Picture.Graphic.Empty then
  begin
    case FBackGround.Position of
      bpTopLeft: Canvas.Draw(R.Left + 1, R.Top + 1, FBackGround.Picture.Graphic);
      bpTopRight: Canvas.Draw(Max(R.Left, R.Right - R.Left - FBackGround.Picture.Graphic.Width - 1), R.top + 1, FBackGround.Picture.Graphic);
      bpBottomLeft: Canvas.Draw(R.left + 1, Max(R.top, Height - FBackGround.Picture.Graphic.Height - 3), FBackGround.Picture.Graphic);
      bpBottomRight: Canvas.Draw(Max(R.Left, R.Right - R.Left - FBackGround.Picture.Graphic.Width - 1), Max(R.Top, Height - FBackGround.Picture.Graphic.Height - 3), FBackGround.Picture.Graphic);
      bpCenter: Canvas.Draw(Max(R.Left, R.Right - R.Left - FBackGround.Picture.Graphic.Width - 1) shr 1, Max(r.Top, Height - FBackGround.Picture.Graphic.Height - 2) shr 1, FBackGround.Picture.Graphic);
      bpTiled:
      begin
        yo := R2.Top;
        while (yo < Height) do
        begin
          xo := R2.Left;
          while (xo < Width) do
          begin
            Canvas.Draw(xo, yo, FBackGround.Picture.Graphic);
            xo := xo + FBackGround.Picture.Graphic.Width;
          end;
          yo := yo + FBackGround.Picture.Graphic.Height;
        end;
      end;
      bpStretched: Canvas.StretchDraw(R2, FBackGround.Picture.Graphic);
      else
    end;
  end;

  if FClipping then
    UnclipShape(Canvas);

  if FBevel = bsLowered then
  begin
    Clr1 := clBtnShadow;
    Clr2 := clBtnHighlight;
  end
  else
  begin
    Clr1 := clBtnHighlight;
    Clr2 := clBtnShadow;
  end;

  FDrawer.Canvas := Self.Canvas;
  FDrawer.CurRect := R;
  FDrawer.Angle := RotationAngle;
  FDrawer.RotationType := rtCurRectCenter;

  with Canvas do
  begin
    Canvas.Pen.Assign(Appearance.FPen);
    Canvas.Brush.Assign(Appearance.FBrush);
    //if (Brush.Color = clNone) then
      //Brush.Style := bsClear;
      
    SetLength(Pts, 0);

    if FClipping then
      ClipBoundry(Canvas);
    
    case FShape of
      stRectangle, stSquare:
        begin
          //Drawer.DrawRotRect(R.Left, R.Top, R.Left + ShapeWidth{ R.Right + 1}, R.Top + ShapeHeight{R.Bottom + 1});
          Drawer.DrawRotRect(R.Left, R.Top, R.Right, R.Bottom);
          if (Bevel <> bsNone) then
          begin
           BevelLine(Clr1, Drawer.Rot(R.Left, R.Top), Drawer.Rot(R.Left + ShapeWidth, R.Top));
           BevelLine(Clr2, Drawer.Rot(R.Left + ShapeWidth, R.Top), Drawer.Rot(R.Left + ShapeWidth, R.Top + ShapeHeight));
           BevelLine(Clr2, Drawer.Rot(R.Left + ShapeWidth, R.Top + ShapeHeight), Drawer.Rot(R.Left, R.Top + ShapeHeight));
           BevelLine(Clr1, Drawer.Rot(R.Left, R.Top + ShapeHeight), Drawer.Rot(R.Left, R.Top));
          end;
        end;
      stRoundRect, stRoundSquare:
        begin
          //Pts := GetPolyRoundRect(R.Left, R.Top, R.Right + 1, R.Bottom + 1, (R.Right - R.Left) div 4, (R.Bottom - R.Top) div 4);
          Pts := GetPolyRoundRect(R.Left, R.Top, R.Right, R.Bottom, Rounding, Rounding);
          Pts := Drawer.RotPoly(Pts);
          if (Bevel <> bsNone) then
          begin
            Canvas.Pen.Color := Clr2;
            Canvas.Polygon(Pts);
            l := Length(Pts);
            k := l - (l div 4) + (R.Right - R.Left) div 10;
            i := k;
            Canvas.Pen.Color := Clr1;
            while (i < l) do
            begin
              if (i = k) then
                Canvas.MoveTo(Pts[i].X, Pts[i].Y)
              else
                Canvas.LineTo(Pts[i].X, Pts[i].Y);
              Inc(i);  
            end;
            k := (l div 2) - (R.Right - R.Left) div 10;
            i := 0;
            Canvas.Pen.Color := Clr1;
            while (i < k) do
            begin
              if (i = 0) then
                Canvas.MoveTo(Pts[i].X, Pts[i].Y)
              else
                Canvas.LineTo(Pts[i].X, Pts[i].Y);
              Inc(i);
            end;
          end
          else
            Canvas.Polygon(Pts);
        end;
      stCircle, stEllipse:
        begin
          if (Bevel <> bsNone) then
          begin
            Canvas.Pen.Color := Clr2;
            Canvas.Polygon(Drawer.RotPoly(GetPolyEllipse(R.Left, R.Top, R.Right, R.Bottom)));
            BevelEllipse(Drawer.RotPoly(GetPolyEllipse(R.Left, R.Top, R.Right, R.Bottom)));
          end
          else
            Canvas.Polygon(Drawer.RotPoly(GetPolyEllipse(R.Left, R.Top, R.Right, R.Bottom)));
        end;
      stTriangle:
      begin
        if (Bevel <> bsNone) then
          Canvas.Pen.Color := Clr2;
        Pts := Drawer.RotPoly(GetTriangle(R.Left, R.Top, R.Right, R.Bottom));
        Canvas.Polygon(Pts);
        if (Bevel <> bsNone) then
        begin
          Canvas.Pen.Color := Clr1;
          Canvas.MoveTo(Pts[0].X, Pts[0].Y);
          Canvas.LineTo(Pts[1].X, Pts[1].Y);
        end;
      end;
      stLine:
      begin
        SetLength(Pts,2);
        pts[0].X := R.Right;
        pts[0].Y := R.Top + (R.Bottom - R.Top) div 2;

        pts[1].X := R.Left;
        pts[1].Y := R.Top + (R.Bottom - R.Top) div 2;

        Pts := Drawer.RotPoly(Pts);
        //Canvas.Polygon(Pts);
        Canvas.MoveTo(Pts[0].X, Pts[0].Y);
        Canvas.LineTo(Pts[1].X, Pts[1].Y);
      end;
      stStar:
      begin
        if (Bevel <> bsNone) then
          Canvas.Pen.Color := Clr2;
        Pts := Drawer.RotPoly(GetStar(R.Left, R.Top, R.Right + 1, R.Bottom + 1));
        Canvas.Polygon(Pts);
        if (Bevel <> bsNone) then
        begin
          Canvas.Pen.Color := Clr1;
          Canvas.MoveTo(Pts[9].X, Pts[9].Y);
          Canvas.LineTo(Pts[10].X, Pts[10].Y);
          Canvas.MoveTo(Pts[10].X, Pts[10].Y);
          Canvas.LineTo(Pts[11].X, Pts[11].Y);
          Canvas.MoveTo(Pts[11].X, Pts[11].Y);
          Canvas.LineTo(Pts[0].X, Pts[0].Y);
          Canvas.MoveTo(Pts[0].X, Pts[0].Y);
          Canvas.LineTo(Pts[1].X, Pts[1].Y);
          Canvas.MoveTo(Pts[1].X, Pts[1].Y);
          Canvas.LineTo(Pts[2].X, Pts[2].Y);
          Canvas.MoveTo(Pts[2].X, Pts[2].Y);
          Canvas.LineTo(Pts[3].X, Pts[3].Y);
          {
          Canvas.MoveTo(Pts[11].X, Pts[11].Y);
          Canvas.LineTo(Pts[0].X, Pts[0].Y);
          Canvas.MoveTo(Pts[0].X, Pts[0].Y);
          Canvas.LineTo(Pts[1].X, Pts[1].Y);
          Canvas.MoveTo(Pts[1].X, Pts[1].Y);
          Canvas.LineTo(Pts[2].X, Pts[2].Y);
          Canvas.MoveTo(Pts[2].X, Pts[2].Y);
          Canvas.LineTo(Pts[3].X, Pts[3].Y);
          Canvas.MoveTo(Pts[3].X, Pts[3].Y);
          Canvas.LineTo(Pts[4].X, Pts[4].Y);
          Canvas.MoveTo(Pts[4].X, Pts[4].Y);
          Canvas.LineTo(Pts[5].X, Pts[5].Y); }
        end;
      end;
    end;

    if FClipping then
      UnclipShape(Canvas);
  end;

  //----- Draw text
  if (Text <> '') then
  begin
    Canvas.Brush.Style := bsClear;
    Canvas.Font.Assign(Self.Font);

    TextR := Rect(R.Left + TextOffsetX, R.Top + TextOffsetY, R.Right, R.Bottom);

    InflateRect(TextR,-2,-2);

    if (ShowText = stClipped) then
      ClipShape(Canvas);

    HTMLDrawEx(Canvas, Text, TextR, FImages, 0, 0, -1, -1, 1, false, false, false, false, false, false,
      true, 1.0, Appearance.URLColor, clNone, clNone, clGray, anchor, stripped, focusanchor, xsize, ysize,
      hyperlinks, mouselink, re, nil , FContainer, 0);

    if (ShowText = stClipped) then
      UnClipShape(Canvas);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvShape.SetShape(Value: TAdvShapeType);
begin
  if FShape <> Value then
  begin
    FShape := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

function TAdvShape.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

//------------------------------------------------------------------------------

procedure TAdvShape.SetVersion(const Value: string);
begin

end;

//------------------------------------------------------------------------------

function TAdvShape.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

//------------------------------------------------------------------------------

procedure TAdvShape.SetRotationAngle(const Value: Double);
begin
  if (FRotationAngle <> Value) and (Value >= -360) and (Value <= 360) then
  begin
    FRotationAngle := Value;
    Invalidate;
  end;
end;

procedure TAdvShape.SetRounding(const Value: integer);
begin
  if (FRounding <> Value) then
  begin
    FRounding := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvShape.SetShapeHeight(const Value: Integer);
begin
  if (FShapeHeight <> Value) then
  begin
    FShapeHeight := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvShape.SetShapeWidth(const Value: Integer);
begin
  if (FShapeWidth <> Value) then
  begin
    FShapeWidth := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvShape.SetText(const Value: string);
begin
  if (FText <> Value) then
  begin
    FText := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvShape.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  Anchor: string;
begin
  inherited;

  Anchor := IsAnchor(X, Y);
  if Anchor <> '' then
  begin
    if (Pos('://', anchor) > 0) or (pos('mailto:', anchor) > 0) then
      ShellExecute(0, 'open', pchar(anchor), nil, nil, SW_NORMAL)
    else
    begin
      if Assigned(fAnchorClick) then
        FAnchorClick(self, anchor);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvShape.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  anchor: string;
begin
  inherited;

  anchor := IsAnchor(x, y);

  if (Anchor <> '') then
  begin
    if (FOldCursor = crNone) and (FAnchor <> Anchor) then
    begin
      FAnchor := Anchor;
      FOldCursor := Cursor;
      Cursor := crHandPoint;
      if Assigned(FAnchorEnter) then
        FAnchorEnter(self, anchor);
    end;
  end
  else
  begin
    if (Cursor = crHandPoint) and (FOldCursor <> crNone) then
    begin
      Cursor := FOldCursor;
      FOldCursor := crNone;
      FAnchor := '';
      if Assigned(FAnchorExit) then
        FAnchorExit(self, Anchor);
    end;
  end;

end;

//------------------------------------------------------------------------------

procedure TAdvShape.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;

end;

//------------------------------------------------------------------------------

procedure TAdvShape.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;

  if (AOperation = opRemove) and not (csDestroying in ComponentState) then
  begin
    if (AComponent = FImages) then
      FImages := nil;
  end;

  if (AOperation = opRemove) and (AComponent = FContainer) then
    FContainer := nil;
end;

//------------------------------------------------------------------------------

procedure TAdvShape.SetImages(const Value: TImageList);
begin
  FImages := Value;
  Invalidate;
end;

//------------------------------------------------------------------------------

function TAdvShape.IsAnchor(x, y: integer): string;
var
  r: trect;
  xsize, ysize: integer;
  anchor, stripped: string;

  HyperLinks,MouseLink: Integer;
  Focusanchor: string;
  re: TRect;
  AText: String;
begin
  R := ShapeRect;
  AText := Text;
  
  Anchor := '';
  Canvas.Font.Assign(Self.Font);
  R := Rect(R.Left + TextOffsetX, R.Top + TextOffsetY, R.Right, R.Bottom);

  InflateRect(R, -2, -2);

  if HTMLDrawEx(Canvas, AText, R, FImages, x, y, -1, -1, 1, true, false, false, true, true, false, true,
     1.0, Appearance.URLColor, clNone, clNone, clGray, anchor, stripped, focusanchor, xsize, ysize, hyperlinks,
     mouselink, re, nil, FContainer, 0) then
    Result := anchor;
end;

//------------------------------------------------------------------------------

procedure TAdvShape.SetAppearance(const Value: TShapeAppearance);
begin
  FAppearance.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TAdvShape.SetAutoSizeEx(const Value: boolean);
begin
  if (FAutoSize <> Value) then
  begin
    FAutoSize := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvShape.SetMaxSize(const Value: Boolean);
begin
  if (FMaxSize <> Value) then
  begin
    FMaxSize := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvShape.OnAppearanceChanged(Sender: TObject);
begin
  Invalidate;
end;

//------------------------------------------------------------------------------

function TAdvShape.GetShapeClipRegion: HRGN;
var
  Pts: TPointArray;
  R: TRect;
begin
  if (RotationAngle = 0) and (FShape = stEllipse) then
  begin
    Pts := GetBoundsPts;
    with BoundsRect do
      Result := CreateEllipticRgn(Pts[0].X, Pts[0].Y, Pts[1].X, Pts[1].Y)
  end
  else
  if (RotationAngle = 0) and (FShape = stRoundRect) then
  begin
    Pts := GetBoundsPts;
    R := ShapeRect;
    Result := CreateRoundRectRgn(R.Left, R.Top, R.Right, R.Bottom, Rounding, Rounding);
  end
  else
  begin
    Pts := GetSurroundRgn;
    Result := CreatePolygonRgn(Pts[0], Length(Pts), ALTERNATE);
  end;
end;

function TAdvShape.GetShapeWidth: Integer;
var
  cliw,clih: integer;
  R: Double;
  RO: Double;
begin
  if FAutoSize then
  begin
    cliw := ClientRect.Right - ClientRect.Left - Appearance.Pen.Width;
    clih := ClientRect.Bottom  - ClientRect.Top - Appearance.Pen.Width;

    if (RotationAngle > 0) or not MaxSize then
    begin
      R := sqrt( sqr( FDrawer.FCosAngle * cliw / 2) + sqr(FDrawer.FSinAngle * clih / 2));
      RO := sqrt( sqr( cliw / 2) + sqr(clih / 2));

      if (clih > 0) and (RO > 0) then
        cliw := Trunc(cliw * R/RO/(cliw/clih));
    end;

    Result := cliw;
  end
  else
    Result := FShapeWidth;
end;

function TAdvShape.GetShapeHeight: Integer;
var
  cliw,clih: integer;
  R: Double;
  RO: Double;
begin
  if FAutoSize then
  begin
    cliw := ClientRect.Right - ClientRect.Left - Appearance.Pen.Width;
    clih := ClientRect.Bottom  - ClientRect.Top - Appearance.Pen.Width;
    if (RotationAngle > 0) or not MaxSize then
    begin
      R := sqrt( sqr( FDrawer.FCosAngle * cliw / 2) + sqr(FDrawer.FSinAngle * clih / 2));
      RO := sqrt( sqr( cliw / 2) + sqr(clih / 2));

      if (clih > 0) and (RO > 0) then
        clih := Trunc(cliw * R/RO/(cliw/clih));
    end;
    Result := clih;
  end
  else
    Result := FShapeHeight;
end;

//------------------------------------------------------------------------------

function TAdvShape.GetBoundsPts: TPointArray;
var
  R: TRect;
begin
  SetLength(result, 2);
  R := ShapeRect;
  Result[0] := Point(Left, Top);
  Result[1] := Point(Left + Width, Top + Height);
end;

//------------------------------------------------------------------------------

procedure TAdvShape.ClipBoundry(Canvas: TCanvas);
var
  R1: HRGN;
  //Pts: array[0..3] of TPoint;
begin
  R1 := CreateRectRgn(Left, Top, Left + ClientRect.Right, Top + ClientRect.Bottom);
  {Pts[0] := Point(0, 0);
  Pts[1] := Point(Width, 0);
  Pts[2] := Point(Width, Height);
  Pts[3] := Point(0, Height);
  R1 := CreatePolygonRgn(Pts[0], Length(Pts), ALTERNATE); }

  try
    SelectClipRgn(Canvas.Handle, R1);
  finally
    DeleteObject(R1);
  end;
end;  

//------------------------------------------------------------------------------

procedure TAdvShape.ClipShape(Canvas: TCanvas);
var
  R1: HRGN;
begin
  R1 := GetShapeClipRegion;
  try
    SelectClipRgn(Canvas.Handle, R1);
  finally
    DeleteObject(R1);
  end;
end;

procedure TAdvShape.CMMouseEnter(var Msg: TMessage);
begin
  if Assigned(OnMouseEnter) then
    FOnMouseEnter(self);
end;

procedure TAdvShape.CMMouseLeave(var Message: TMessage);
begin
  if Assigned(OnMouseLeave) then
    FOnMouseLeave(self);
end;

//------------------------------------------------------------------------------

procedure TAdvShape.UnclipShape(Canvas: TCanvas);
begin
  SelectClipRgn(Canvas.Handle, 0);
end;

//------------------------------------------------------------------------------

function TAdvShape.GetSurroundRgn: TPointArray;
begin
  SetLength(result, 0);
  Drawer.CurRect := ShapeRect;
  Drawer.Angle := RotationAngle;
  Drawer.RotationType := rtCurRectCenter;
  result := SurroundRgn;
end;

//------------------------------------------------------------------------------

function TAdvShape.SurroundRgn: TPointArray;
var
  R: TRect;
begin
  R := ShapeRect;
  Drawer.CurRect := BoundsRect;
  Drawer.Angle := RotationAngle;
  Drawer.RotationType := rtCurRectCenter;
  case FShape of
    stEllipse, stCircle:
      result := Drawer.RotPoly(GetPolyEllipse(Left + R.Left, Top + R.Top, Right - (Width - R.Right - Appearance.Pen.Width), Bottom - (Height - R.Bottom - Appearance.Pen.Width)));
    stRoundRect, stRoundSquare:
//      result := Drawer.RotPoly(GetPolyRoundRect(Left + R.Left, Top + R.Top, Right - (Width - R.Right - Appearance.Pen.Width), Bottom - (Height - R.Bottom - Appearance.Pen.Width),
//        (R.Right - R.Left) div 4, (R.Bottom - R.Top) div 4));
      result := Drawer.RotPoly(GetPolyRoundRect(Left + R.Left, Top + R.Top, Right - (Width - R.Right - Appearance.Pen.Width), Bottom - (Height - R.Bottom - Appearance.Pen.Width),
        Rounding, Rounding));
    stRectangle, stSquare:
    begin
      //result := Drawer.RotPoly(GetPolyRoundRect(Left + R.Left, Top + R.Top, Right - (Width - R.Right - Appearance.Pen.Width), Bottom - (Height - R.Bottom - Appearance.Pen.Width), 0, 0));
      Result := GetRectangle(Left + R.Left, Top + R.Top, Right - (Width - R.Right - Appearance.Pen.Width), Bottom - (Height - R.Bottom - Appearance.Pen.Width));
      //Result := GetRectangle(Left + R.Left + 1, Top + R.Top + 1, Left + R.Left + ShapeWidth, Top + R.Top + ShapeHeight);
      result := Drawer.RotPoly(Result);
    end;
    stTriangle:
    begin
      Result := GetTriangle(Left + R.Left, Top + R.Top, Right - (Width - R.Right - Appearance.Pen.Width), Bottom - (Height - R.Bottom - Appearance.Pen.Width));
      //Rotate2Darray(Result, DegToRad(RotationAngle));
      result := Drawer.RotPoly(Result);
    end;
    stStar:
    begin
      Result := GetStar(Left + R.Left, Top + R.Top, Right - (Width - R.Right - Appearance.Pen.Width), Bottom - (Height - R.Bottom - Appearance.Pen.Width));
      result := Drawer.RotPoly(Result);
    end;
  else 
    begin
      SetLength(result, 5);
      With Drawer.OriginalRect do
      begin
        Result[0] := Drawer.Rot(Drawer.PP(Left + R.Left, Top + R.Top));
        Result[1] := Drawer.Rot(Drawer.PP(Right - (self.Width - R.Right - Appearance.Pen.Width), Top + R.Top));
        Result[2] := Drawer.Rot(Drawer.PP(Right - (self.Width - R.Right - Appearance.Pen.Width), Bottom - (self.Height - R.Bottom - Appearance.Pen.Width)));
        Result[3] := Drawer.Rot(Drawer.PP(Left + R.Left, Bottom - (self.Height - R.Bottom - Appearance.Pen.Width)));
        Result[4] := Result[0];
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TAdvShape.GetBottom: Integer;
begin
  Result := Top + Height;
end;

//------------------------------------------------------------------------------

function TAdvShape.GetRight: Integer;
begin
  Result := Left + Width;
end;

//------------------------------------------------------------------------------

procedure TAdvShape.SetTextOffsetX(const Value: Integer);
begin
  if (FTextOffsetX <> Value) then
  begin
    FTextOffsetX := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvShape.SetTextOffsetY(const Value: Integer);
begin
  if (FTextOffsetY <> Value) then
  begin
    FTextOffsetY := Value;
    Invalidate;
  end;
end;
 
//------------------------------------------------------------------------------

procedure TAdvShape.SetBevel(const Value: TAdvBevel);
begin
  if (FBevel <> Value) then
  begin
    FBevel := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvShape.SetBackGround(const Value: TShapeBackGround);
begin
  FBackGround.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TAdvShape.OnBackGroundChanged(Sender: TObject);
begin
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvShape.SetShowText(const Value: TShowTextType);
begin
  if (FShowText <> Value) then
  begin
    FShowText := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

{ TShapeBackGround }

constructor TShapeBackGround.Create;
begin
  inherited;
  FPicture := TPicture.Create;
  FPicture.OnChange := OnPictureChanged;
end;

//------------------------------------------------------------------------------

destructor TShapeBackGround.Destroy;
begin
  FPicture.Free;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TShapeBackGround.Assign(Source: TPersistent);
begin
  if (Source is TShapeBackGround) then
  begin
    FPicture.Assign(TShapeBackGround(Source).Picture);
    Position := TShapeBackGround(Source).Position;
  end
  else
    inherited;
end;

//------------------------------------------------------------------------------

procedure TShapeBackGround.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

//------------------------------------------------------------------------------

procedure TShapeBackGround.OnPictureChanged(Sender: TObject);
begin
  Changed;
end;

//------------------------------------------------------------------------------

procedure TShapeBackGround.SetPicture(const Value: TPicture);
begin
  FPicture.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TShapeBackGround.SetPosition(const Value: TBackGroundPosition);
begin
  if (FPosition <> Value) then
  begin
    FPosition := Value;
    Changed;
  end;
end;


end.
