{*************************************************************************}
{ THotSpotImage component                                                 }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright © 2002 - 2012                                       }
{           Email : info@tmssoftware.com                                  }
{           Web : http://www.tmssoftware.com                              }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage done due to the use of this code.               }
{ The component can be freely used in any application. The complete       }
{ source code remains property of the author and may not be distributed,  }
{ published, given or sold in any form as such. No parts of the source    }
{ code can be included in any other component or application without      }
{ written authorization of the author.                                    }
{*************************************************************************}

unit HotSpotImage;

interface

{$I TMSDEFS.INC}

uses
  Classes, SysUtils, ExtCtrls, Windows, Math, Graphics, Messages, Controls,
  Forms, Dialogs, ImgList, Types;

const
  CornerSize = 2;
  EllipseTolerance = 0.15;

  MAJ_VER = 2; // Major version nr.
  MIN_VER = 5; // Minor version nr.
  REL_VER = 1; // Release nr.
  BLD_VER = 1; // Build nr.

  // version history
  // 1.2.1.1 : fixed issue with Transparency
  // 1.2.2.0 : fixed issue with very large image support
  // 1.2.3.0 : added OnPaint event
  // 1.2.3.1 : Fixed issue with clearing hotspots during mouse events

  // 2.0.0.0 : New : support for color & imagelist images for hotspot hover, normal, down state
  //         : New : support for hotspot selected state / selected color & image
  //         : New : support for hotspot blink state / blink color & image
  //         : New : function HotSpotByID() added
  //         : New : function HotSpotByName() added
  //         : New : component for runtime hotspot editing
  //         : New : SelectionMode to allow single or multiselect
  //         : Improved : hotspot accuracy
  //         : Improved : magic wand selection
  // 2.0.1.0 : Improved : hotspots with hover,down,selected color set to clNone will not show in these states
  //         : New : capability to select no color (clNone) for colors in the hotspot editor
  // 2.0.2.0 : New : added the capability to work as control container
  // 2.0.2.1 : Fixed : issue with calling OnPaint event
  // 2.0.2.2 : Fixed : issue with save & load to stream
  // 2.0.2.3 : Fixed : issue with LoadFromFile
  // 2.0.2.4 : Fixed : hotspot detection issue with large images
  //         : Fixed : design time repaint issue when deleting hotspots
  // 2.5.0.0 : New : Support for rotation of image
  // 2.5.0.1 : Fixed : Issue with persistence when HotSpot.Clipped = false
  // 2.5.1.0 : Improved : Handling of scaling
  //         : Improved : Performance
  // 2.5.1.1 : Fixed : Issue with setting hover color at runtime

type
  {$IFDEF DELPHI_UNICODE}
  THintInfo = Controls.THintInfo;
  PHintInfo = Controls.PHintInfo;
  {$ENDIF}

  THotSpotImage = class;

  THoverPosition = (hpNone, hpInside, hpBorder);

  TSpotShapeType = (stRectangle, stEllipse, stPolygon);

  TBorderPoly = (bNone,bInside,bPoint,bLine);

  TSelectionMode = (smSingleSelect, smMultiSelectCtrl, smMultiSelect);

  TRealPoint = record
    x,y: Real;
  end;

  TPoints = array of TRealPoint;
  TIntPoints = array of TPoint;
  THotSpots = class;

  //class used internally; not meant to be accessed by the user

  THotSpotShape = class(TPersistent)
  private
    FTop: Integer;
    FLeft: Integer;
    FHeight: Integer;
    FWidth: Integer;
    FAngle: Integer;
    FShapeType: TSpotShapeType;
    FPoints: TPoints;
    FEx1,FEx2,FEy1,FEy2: Real;
    function EllipsePos(X, Y: Integer): THoverPosition;
    function PolyPos(X, Y: Integer): THoverPosition;
    procedure SetPoints(const Value: TPoints);
    procedure SetAngle(const Value: Integer);
  public
    procedure calcMargins;
    procedure fillEllipse(Canvas:TCanvas;Pts:TIntPoints);
    function BorderPolypos(X,Y:Integer;var p1,p2:integer):TBorderPoly;
    procedure EllipseToBezier;
    constructor Create(Shape: TSpotShapeType);
    destructor Destroy;override;
    function RectPos(X, Y: Integer): THoverPosition;
    function GetHoverPos(X, Y: Integer): THoverPosition;
    procedure Draw(Canvas :TCanvas);
    procedure DrawAt(Canvas :TCanvas; X,Y: Integer);
    property Top: Integer read FTop write FTop;
    property Left: Integer read FLeft write FLeft;
    property Height: Integer read FHeight write FHeight;
    property Width: Integer read FWidth write FWidth;
    property ShapeType: TSpotShapeType read FShapeType write FShapeType;
    property Points: TPoints read FPoints write SetPoints;
    property Angle: Integer read FAngle write SetAngle;
  end;

  THotSpot = class(TCollectionItem)
  private
    FHoverPicture: TPicture;
    FClickPicture: TPicture;
    FPicture: TPicture;
    FCClick : TBitmap;
    FCHover : TBitmap;
    FCSelect: TBitmap;
    FCBlink: TBitmap;
    FShape: THotSpotShape;
    FHint: String;
    FName: String;
    FID: Integer;
    FClipped: Boolean;
    FOwner: THotSPots;
    FDown: Boolean;
    FShowClick: Boolean;
    FBlink: Boolean;
    FSelected: Boolean;
    FBlinkImageIndex: Integer;
    FSelectedImageIndex: integer;
    FClickImageIndex: integer;
    FHoverImageIndex: integer;
    FBlinkColor: TColor;
    FHoverColor: TColor;
    FSelectedColor: TColor;
    FClickColor: TColor;
    FSelectedImage: TPicture;
    FBlinkImage: TPicture;
    FOffsetY: Integer;
    FOffsetX: Integer;
    FSelectable: Boolean;
    FX1, FY1, FX2, FY2: Extended;
    //calculate the hover&click images based on the clipping property
    procedure CalcClip(const Pict: TPicture;var Bitm: TBitmap; ImageIndex: Integer; Clr: TColor = clNone);
    procedure SetShapeType(const Value: TSpotShapeType);
    procedure SetClickPicture(const Value: TPicture);
    procedure SetHoverPicture(const Value: TPicture);
    function GetWidth: Integer;
    function GetHeight: Integer;
    function GetX: Integer;
    function GetY: Integer;
    function GetX2: Integer;
    function GetY2: Integer;
    function GetShapeType: TSpotShapeType;
    procedure SetWidth(const Value: Integer);
    procedure SetHeight(const Value: Integer);
    procedure SetX(const Value: Integer);
    procedure SetY(const Value: Integer);
    procedure SetX2(const Value: Integer);
    procedure SetY2(const Value: Integer);
    procedure setPolyPoints(const Value:TPoints);
    function getPolyPoints:TPoints;
    procedure setPolyPoint(i:Integer;const Value:TRealPoint);
    function getPolyPoint(i:Integer):TRealPoint;
    function GetAngle: Integer;
    procedure SetAngle(const Value: Integer);
    procedure SetClipped(const Value: Boolean);
    procedure SetDown(const Value: Boolean);
    procedure SetShowClick(const Value: Boolean);
    procedure SetBlink(const Value: Boolean);
    procedure SetBlinkImage(const Value: TPicture);
    procedure SetSelected(const Value: Boolean);
    procedure SetSelectedColor(const Value: TColor);
    procedure SetClickColor(const Value: TColor);
    procedure SetSelectedImage(const Value: TPicture);
    procedure SetSelectedImageIndex(const Value: integer);
    procedure SetOffsetX(const Value: Integer);
    procedure SetOffsetY(const Value: Integer);
    procedure UpdateExclusive;
    procedure SetSelectable(const Value: Boolean);
    procedure SetHoverColor(const Value: TColor);
  protected
    function GetDisplayName: String; override;
    procedure DefineProperties(Filer: TFiler);override;
    procedure StorePoints(Writer: TWriter);
    procedure LoadPoints(Reader: TReader);
    procedure HoverPictureChange(Sender: TObject);
    procedure ClickPictureChange(Sender: TObject);
    procedure SelectedImageChange(Sender: TObject);
    procedure BlinkImageChange(Sender: TObject);
    procedure InternalClick;
    procedure ReCalcClips;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    //scale ellipse with the factor derived from the difference p2-p1; the points
    //are rotated with the angle of the ellipse before substraction
    //used in the editor; not meant to be used by the user
    procedure ScaleEllipse(p1,p2,ps:TPoint);
    //draw the shape of the hotspot
    procedure DrawShape(Canvas: TCanvas);
    procedure DrawShapeAt(Canvas: TCanvas; X,Y: Integer);
    function AsRect: TRect;
    //is (x,y) inside ?
    function GetHoverPos(X, Y: Integer): THoverPosition;
    //get position relative to the bounding rectangle
    function GetRectHoverPos(X, Y: Integer): THoverPosition;
    //Get point position relative to the polygon, considering the following situations:
    // inside,over a point, over a segment
    function GetBorderPolypos(X,Y:Integer;var p1,p2:integer):TBorderPoly;
    procedure Assign(Source: TPersistent); override;
    //Set Bounding Rectangle
    procedure SetRect(AX1, AY1, AX2, AY2: Integer); overload;
    procedure SetRect(R: TRect); overload;
    procedure SaveToStream(S: TStream);
    procedure LoadFromStream(S: TStream);

    property X1: Integer read GetX write SetX;
    property Y1: Integer read GetY write SetY;
    property X2: Integer read GetX2 write SetX2;
    property Y2: Integer read GetY2 write SetY2;
    //large scale manipulation of the points; to be processed only as a whole i.e.
    //DO NOT do polypoints[i]:= something
    property PolyPoints: TPoints read getPolyPoints write setPolyPoints;
    //Access one point of the polygon; it assures coherence of the width&height
    property PolyPoint[i:Integer]: TRealPoint read getPolyPoint write setPolyPoint;
  published
    //It can be one of stPolygon stEllipse; changing shapetype is NOT recommended as it merely
    //changes the way shape points are interpreted
    property ShapeType: TSpotShapeType read GetShapeType write SetShapeType;
    property Hint: String read FHint write FHint;
    property Name: String read FName write FName;
    property ID: integer read FID write FID;
    property X: Integer read GetX write SetX;
    property Y: Integer read GetY write SetY;
    //set width & height not recommended for scaling angled ellipses - the outcome is somewhat strange
    property Width: Integer read GetWidth write SetWidth;
    property Height: Integer read GetHeight write SetHeight;
    //Hover & Click Pictures
    property HoverImage: TPicture read FHoverPicture write SetHoverPicture;
    property ClickImage: TPicture read FClickPicture write SetClickPicture;
    //Hover & Click Clipped Bitmaps
    property HoverClippedBitmap: TBitmap read FCHover;
    property ClickClippedBitmap: TBitmap read FCClick;
    property SelectClippedBitmap: TBitmap read FCSelect;
    property BlinkClippedBitmap: TBitmap read FCBlink;
    // The angle of the hotspot relative to Ox
    property Angle: Integer read GetAngle write SetAngle stored False;
    //True: shape clips hover&click picture
    property Clipped : Boolean read FClipped write SetClipped;
    property Down: Boolean read FDown write SetDown;
    property ShowClick: Boolean read FShowClick write SetShowClick default false;

    property HoverColor: TColor read FHoverColor write SetHoverColor;
    property ClickColor: TColor read FClickColor write SetClickColor;
    property SelectedColor: TColor read FSelectedColor write SetSelectedColor;
    property BlinkColor: TColor read FBlinkColor write FBlinkColor;

    property HoverImageIndex: integer read FHoverImageIndex write FHoverImageIndex;
    property ClickImageIndex: integer read FClickImageIndex write FClickImageIndex;
    property SelectedImageIndex: integer read FSelectedImageIndex write SetSelectedImageIndex;
    property BlinkImageIndex: Integer read FBlinkImageIndex write FBlinkImageIndex;

    property Selectable: Boolean read FSelectable write SetSelectable default True;
    property SelectedImage: TPicture read FSelectedImage write SetSelectedImage;
    property BlinkImage: TPicture read FBlinkImage write SetBlinkImage;
    property Blink: Boolean read FBlink write SetBlink default false;
    property Selected: Boolean read FSelected write SetSelected default false;
    property OffsetX: Integer read FOffsetX write SetOffsetX default 4;
    property OffsetY: Integer read FOffsetY write SetOffsetY default 4;
  end;

  THotSpots = class(TCollection)
  private
    FOwner: THotSpotImage;
    oldHeight,oldWidth:integer;
    function GetItem(Index: Integer): THotSpot;
    procedure SetItem(Index: Integer; const Value: THotSpot);
  protected
    function GetHotSpotImage: THotSpotImage;
  public
    procedure EndUpdate; override;
    procedure Assign(Source: TPersistent); override;
    constructor Create(AOwner: THotSpotImage);
    procedure SetDimensions(W,H: integer);
    function Add: THotSpot;
    procedure SaveToStream(S: TStream);
    procedure LoadFromStream(S: TStream);
    procedure SaveToFile(FName: TFileName);
    procedure LoadFromFile(FName: TFileName);
    procedure ReScale(newWidth,newHeight: Integer); overload;
    procedure ReScale(delta: Integer); overload;
    function GetOwner: TPersistent; override;
    property Items[Index: Integer]: THotSpot read GetItem write SetItem; default;
  end;

  THotSpotEvent = procedure(Sender: TObject; HotSpot: THotSpot) of object;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  THotSpotImage = class(TCustomControl)
  private
    FOwner: TControl;
    FHotSpots: THotSpots;
    FPicture: TPicture;
    FHoveredItem: Integer;
    FClickedItem: Integer;
    FBitmap: TBitmap;
    FColor: TColor;
    isMouseUp: Boolean;
    isDblClick: Boolean;
    FOnHotSpotExit: THotSpotEvent;
    FOnHotSpotEnter: THotSpotEvent;
    FOnHotSpotClick: THotSpotEvent;
    FOnHotSpotRightClick: THotSpotEvent;
    FOnHotSpotDblClick: THotSpotEvent;
    FTransparent: Boolean;
    FAutoSize: Boolean;
    FStretch: Boolean;
    FHotSpotCursor: TCursor;
    FOrigCursor: TCursor;
    FOnPaint: TNotifyEvent;
    FImages: TCustomImageList;
    FSelectionMode: TSelectionMode;
    FTimer: TTimer;
    FBlinkInterval: Integer;
    FBlinkCount: Integer; // 0 = draw ; 1 = restore
    //AppEv:TApplicationEvents;
    FPainting: Boolean;
    FAngle: Integer;
    FRotating: Boolean;
    procedure OnTimerTime(Sender: TObject);
    procedure SetHotSpots(const Value: THotSpots);
    procedure SetPicture(const Value: TPicture);
    procedure SetHoveredItem(const Value: Integer);
    procedure SetColor(const Value: TColor);
    procedure SetTransparent(const Value: Boolean);
    procedure SetAutoSizeEx(const Value: Boolean);
    function  GetHotSpotXY(x, y: Integer): THotSpot;
    procedure SetStretch(const Value: Boolean);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    procedure SetImages(const Value: TCustomImageList);
    procedure SetBlinkInterval(const Value: Integer);
    procedure UpdateHotSpotsBlinking;
    function GetSelectedIndex: Integer;
    procedure InternalRotate90Degrees;
  protected
    procedure Paint; override;
    procedure DrawHoverImage(HotSpotIndex: Integer;canv:TCanvas = nil);
    procedure DrawClickImage(HotSpotIndex: Integer;canv:TCanvas = nil);
    procedure DrawSelectImage(HotSpotIndex: Integer; canv:TCanvas = nil);
    procedure DrawBlinkImage(HotSpotIndex: Integer; canv:TCanvas = nil);
    procedure RestoreHotSpot(HotSpotIndex: Integer);
    procedure CMHintShow(var M: TMessage); message CM_HINTSHOW;
    procedure CMMouseLeave(Var Msg: TMessage); message CM_MOUSELEAVE;
    procedure CMMouseEnter(Var Msg: TMessage); message CM_MOUSEENTER;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure DblClick; override;
    procedure PictureChanged(Sender: TObject);
    procedure Resize; override;
    function GetVersionNr: Integer; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    property Canvas;
    //the number of the hovered item
    property HoveredItem: Integer read FHoveredItem write SetHoveredItem;
    property HotSpotXY[x,y: Integer]: THotSpot read GetHotSpotXY;
    property SelectedIndex: Integer read GetSelectedIndex;
    function HotSpotByID(ID: integer): THotSpot;
    function HotSpotByName(Name: string): THotSpot;
    procedure Rotate(Clockwise: Boolean);
    procedure RotateReset;
  published
    property Align;
    property Anchors;
    property AutoSize: Boolean read FAutoSize write SetAutoSizeEx;
    property BlinkInterval: Integer read FBlinkInterval write SetBlinkInterval default 100;
    property Color: TColor read FColor write SetColor;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property HotSpotCursor: TCursor read FHotSpotCursor write FHotSpotCursor;
    //the hotspot list
    property HotSpots: THotSpots read FHotSpots write SetHotSpots;
    property Images: TCustomImageList read FImages write SetImages;
    //background picture
    property Picture: TPicture read FPicture write SetPicture;
    property PopupMenu;
    property SelectionMode: TSelectionMode read FSelectionMode write FSelectionMode default smSingleSelect;
    property ShowHint;
    property Transparent: Boolean read FTransparent write SetTransparent;
    property Stretch: Boolean read FStretch write SetStretch;
    property Version: string read GetVersion write SetVersion;
    property Visible;
    //background color
    property OnClick;
    property OnDblClick;
    property OnHotSpotEnter: THotSpotEvent read FOnHotSpotEnter write FOnHotSpotEnter;
    property OnHotSpotExit: THotSpotEvent read FOnHotSpotExit write FOnHotSpotExit;
    property OnHotSpotClick: THotSpotEvent read FOnHotSpotClick write FOnHotSpotClick;
    property OnHotSpotRightClick: THotSpotEvent read FOnHotSpotRightClick write FOnHotSpotRightClick;
    property OnHotSpotDblClick: THotSpotEvent read FOnHotSpotDblClick write FOnHotSpotDblClick;
    property OnMouseMove;
    property OnMouseDown;
    property OnMouseUp;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnStartDrag;
  end;


function ReadInteger(S: TStream): Integer;
function ReadString(S: TStream): string;
procedure WriteString(S: TStream; Buffer: string);
procedure WriteInteger(S: TStream; Buffer: Integer);

function Between(Value, Min, Max: Integer): Boolean;overload;
function Between(Value, Min, Max: Real): Boolean;overload;

function Intersect(p1, p2, p3, p4:TPoint): Boolean;
function CCW(p0, p1, p2:TPoint): Integer;
function PRound(Value:TRealPoint): TPoint;
function RPoint(x,y:Real): TRealPoint;
function EmptyImage(Image: TPicture): Boolean;

var
  TMSHotSpot_V1Compatibility: boolean = false;


//============================================================ IMPLEMENTATION ==
implementation

uses
  LineLibrary
{$IFDEF DELPHIXE4_LVL}
  , AnsiStrings
{$ENDIF}
  ;

const
  EToBConst:Real = 0.2761423749154;


{/*************************************************************************

 * FUNCTION:   Intersect
 *
 * PURPOSE
 * Given two line segments, determine if they intersect.
 *
 * RETURN VALUE
 * TRUE if they intersect, FALSE if not.

 *************************************************************************/}
function Intersect(p1, p2, p3, p4:TPoint):boolean;
begin
   Result:=((( CCW(p1, p2, p3) * CCW(p1, p2, p4)) <= 0)
        and (( CCW(p3, p4, p1) * CCW(p3, p4, p2)  <= 0) )) ;
end;

{/*************************************************************************

 * FUNCTION:   CCW (CounterClockWise)
 *
 * PURPOSE
 * Determines, given three points, if when travelling from the first to
 * the second to the third, we travel in a counterclockwise direction.

 *
 * RETURN VALUE
 * (int) 1 if the movement is in a counterclockwise direction, -1 if
 * not.
 *************************************************************************/
}

function CCW(p0, p1, p2:TPoint):Integer;
var
  dx1, dx2:integer;
  dy1, dy2:integer;
begin
  dx1 := p1.x - p0.x ; dx2 := p2.x - p0.x ;
  dy1 := p1.y - p0.y ; dy2 := p2.y - p0.y ;

{   /* This is basically a slope comparison: we don't do divisions because
    * of divide by zero possibilities with pure horizontal and pure
    * vertical lines.
    */}

  if (dx1*dy2) > (dy1*dx2) then
    Result := 1
  else
    Result := -1;
end;

//------------------------------------------------------------------------------
function EmptyImage(Image: TPicture): Boolean;
begin
  Result := (Image.Width = 0) and (Image.Height = 0);
end;

//------------------------------------------------------------------------------

function EmptyBitmap(bmp: TBitmap): Boolean;
begin
  Result := (bmp.Width = 0) and (bmp.Height = 0);
end;

//------------------------------------------------------------------------------
function PRound(Value:TRealPoint):TPoint;
begin
  Result.x := Round(Value.x);
  Result.y := Round(Value.y);
end;

//------------------------------------------------------------------------------
function RPoint(x,y:Real):TRealPoint;
begin
  Result.x := x;
  Result.y := y;
end;

//------------------------------------------------------------------------------
function Between(Value, Min, Max: Integer): Boolean;
begin
  Result := (Value >= Min) and (Value <=  Max);
end;

//------------------------------------------------------------------------------
function Between(Value, Min, Max: Real): Boolean;
begin
  Result := (Value >= Min) and (Value <=  Max);
end;

//------------------------------------------------------------------------------
procedure WriteInteger(S: TStream; Buffer: Integer);
begin
  S.Write(Buffer, SizeOf(Integer));
end;

//------------------------------------------------------------------------------
procedure WriteString(S: TStream; Buffer: string);
var
  b: Byte;
  ABuffer: ansistring;
begin
  ABuffer := ansistring(Buffer);
  b := Length(ABuffer);
  s.WriteBuffer(b,1); // store length of string
  S.Write(ABuffer[1], b);
end;

//------------------------------------------------------------------------------
function ReadString(S: TStream): string;
var
  Buffer: array[0..255] of AnsiChar;
  b: byte;
begin
  S.ReadBuffer(b,1); // read length of string
  S.ReadBuffer(Buffer,b);
  Buffer[b] := #0;
  {$IFDEF DELPHIXE4_LVL}
  Result := string(AnsiStrings.StrPas(Buffer));
  {$ENDIF}
  {$IFNDEF DELPHIXE4_LVL}
  Result := string(StrPas(Buffer));
  {$ENDIF}
end;

//------------------------------------------------------------------------------
function ReadInteger(S: TStream): Integer;
begin
  S.Read(Result, SizeOf(Integer));
end;

//------------------------------------------------------------------------------

function CreateRotatedBitmap(Bitmap: TBitmap; const Angle: Extended; bgColor: TColor): TBitmap;
type
  PRGBQuadArray = ^TRGBQuadArray;
  TRGBQuadArray = array[0..0] of TRGBQuad;
var
  bgRGB: TRGBQuad;
  NormalAngle: Extended;
  CosTheta, SinTheta: Extended;
  iCosTheta, iSinTheta: Integer;
  xSrc, ySrc: Integer;
  xDst, yDst: Integer;
  xODst, yODst: Integer;
  xOSrc, yOSrc: Integer;
  xPrime, yPrime: Integer;
  srcWidth, srcHeight: Integer;
  dstWidth, dstHeight: Integer;
  yPrimeSinTheta, yPrimeCosTheta: Integer;
  srcRGBs: PRGBQuadArray;
  dstRGBs: PRGBQuadArray;
  dstRGB: PRGBQuad;
  BitmapInfo: TBitmapInfo;
  srcBMP, dstBMP: HBITMAP;
  DC: HDC;
begin
  { Converts bgColor to true RGB Color }
  bgColor := ColorToRGB(bgColor);
  with bgRGB do
  begin
    rgbRed := Byte(bgColor);
    rgbGreen := Byte(bgColor shr 8);
    rgbBlue := Byte(bgColor shr 16);
    rgbReserved := Byte(bgColor shr 24);
  end;

  { Calculates Sine and Cosine of the rotation angle }
  NormalAngle := Frac(Angle / 360.0) * 360.0;
  SinCos(Pi * -NormalAngle / 180, SinTheta, CosTheta);
  iSinTheta := Trunc(SinTheta * (1 shl 16));
  iCosTheta := Trunc(CosTheta * (1 shl 16));

  { Prepares the required data for the source bitmap }
  srcBMP := Bitmap.Handle;
  srcWidth := Bitmap.Width;
  srcHeight := Bitmap.Height;
  xOSrc := srcWidth shr 1;
  yOSrc := srcHeight shr 1;

  { Prepares the required data for the target bitmap }
  dstWidth := SmallInt((srcWidth * Abs(iCosTheta) + srcHeight * Abs(iSinTheta)) shr 16);
  dstHeight := SmallInt((srcWidth * Abs(iSinTheta) + srcHeight * Abs(iCosTheta)) shr 16);
  xODst := dstWidth shr 1;
  if not Odd(dstWidth) and ((NormalAngle = 0.0) or (NormalAngle = -90.0)) then
    Dec(xODst);
  yODst := dstHeight shr 1;
  if not Odd(dstHeight) and ((NormalAngle = 0.0) or (NormalAngle = +90.0)) then
    Dec(yODst);

  // Initializes bitmap header
  FillChar(BitmapInfo, SizeOf(BitmapInfo), 0);
  with BitmapInfo.bmiHeader do
  begin
    biSize := SizeOf(BitmapInfo.bmiHeader);
    biCompression := BI_RGB;
    biBitCount := 32;
    biPlanes := 1;
  end;

  // Get source and target RGB bits
  DC := CreateCompatibleDC(0);
  try
    BitmapInfo.bmiHeader.biWidth := srcWidth;
    BitmapInfo.bmiHeader.biHeight := srcHeight;
    GetMem(srcRGBs, srcWidth * srcHeight * SizeOf(TRGBQuad));
    GdiFlush;
    GetDIBits(DC, srcBMP, 0, srcHeight, srcRGBS, BitmapInfo, DIB_RGB_COLORS);
    BitmapInfo.bmiHeader.biWidth := dstWidth;
    BitmapInfo.bmiHeader.biHeight := dstHeight;
    dstBMP := CreateDIBSection(DC, BitmapInfo, DIB_RGB_COLORS, Pointer(dstRGBs), 0, 0);
  finally
    DeleteDC(DC);
  end;

  { Pefroms rotation on RGB bits }
  dstRGB := @dstRGBs[(dstWidth * dstHeight) - 1];
  yPrime := yODst;
  for yDst := dstHeight - 1 downto 0 do
  begin
    yPrimeSinTheta := yPrime * iSinTheta;
    yPrimeCosTheta := yPrime * iCosTheta;
    xPrime := xODst;
    for xDst := dstWidth - 1 downto 0 do
    begin
      xSrc := SmallInt((xPrime * iCosTheta - yPrimeSinTheta) shr 16) + xOSrc;
      ySrc := SmallInt((xPrime * iSinTheta + yPrimeCosTheta) shr 16) + yOSrc;
      {$IFDEF COMPILER4_UP}
      if (DWORD(ySrc) < DWORD(srcHeight)) and (DWORD(xSrc) < DWORD(srcWidth)) then
      {$ELSE} // Delphi 3 compiler ignores unsigned type cast and generates signed comparison code!
      if (ySrc >= 0) and (ySrc < srcHeight) and (xSrc >= 0) and (xSrc < srcWidth) then
      {$ENDIF}
        dstRGB^ := srcRGBs[ySrc * srcWidth + xSrc]
      else
        dstRGB^ := bgRGB;
      Dec(dstRGB);
      Dec(xPrime);
    end;
    Dec(yPrime);
  end;

  { Releases memory for source bitmap RGB bits }
  FreeMem(srcRGBs);

  { Create result bitmap }
  Result := TBitmap.Create;
  Result.Handle := dstBMP;
end;

//------------------------------------------------------------------------------

procedure RotateBitmap(var hBitmapDC : Longint; var lWidth : Longint;
         var lHeight : Longint; lRadians : real);
var
  I : Longint;               // loop counter
  J : Longint;               // loop counter
  hNewBitmapDC : Longint;    // DC of the new bitmap
  hNewBitmap : Longint;      // handle to the new bitmap
  lSine : extended;          // sine used in rotation
  lCosine : extended;        // cosine used in rotation
  X1 : Longint;              // used in calculating new
                            //   bitmap dimensions
  X2 : Longint;              // used in calculating new
                            //     bitmap dimensions
  X3 : Longint;              // used in calculating new
                            //     bitmap dimensions
  Y1 : Longint;              // used in calculating new
                            // bitmap dimensions
  Y2 : Longint;              // used in calculating new
                            // bitmap dimensions
  Y3 : Longint;              // used in calculating new
                            // bitmap dimensions
  lMinX : Longint;           // used in calculating new
                            // bitmap dimensions
  lMaxX : Longint;           // used in calculating new
                            // bitmap dimensions
  lMinY : Longint;           // used in calculating new
                            // bitmap dimensions
  lMaxY : Longint;           // used in calculating new
                            // bitmap dimensions
  lNewWidth : Longint;       // width of new bitmap
  lNewHeight : Longint;      // height of new bitmap
  lSourceX : Longint;        // x pixel coord we are blitting
                            // from the source  image
  lSourceY : Longint;        // y pixel coord we are blitting
                            // from the source image

begin
  // create a compatible DC from the one just brought
  // into this function
  hNewBitmapDC := CreateCompatibleDC(hBitmapDC);

  // compute the sine/cosinse of the radians used to
  // rotate this image
  lSine := Sin(lRadians);
  lCosine := Cos(lRadians);

  // compute the size of the new bitmap being created
  X1 := Round(-lHeight * lSine);
  Y1 := Round(lHeight * lCosine);
  X2 := Round(lWidth * lCosine - lHeight * lSine);
  Y2 := Round(lHeight * lCosine + lWidth * lSine);
  X3 := Round(lWidth * lCosine);
  Y3 := Round(lWidth * lSine);

  // figure out the max/min size of the new bitmap
  lMinX := Min(0, Min(X1, Min(X2, X3)));
  lMinY := Min(0, Min(Y1, Min(Y2, Y3)));
  lMaxX := Max(X1, Max(X2, X3));
  lMaxY := Max(Y1, Max(Y2, Y3));

  // set the new bitmap width/height
  lNewWidth := lMaxX - lMinX;
  lNewHeight := lMaxY - lMinY;

  // create a new bitmap based upon the new width/height of the
  // rotated bitmap
  hNewBitmap := CreateCompatibleBitmap(hBitmapDC, lNewWidth, lNewHeight);

  //attach the new bitmap to the new device context created
  //above before constructing the rotated bitmap
  SelectObject(hNewBitmapDC, hNewBitmap);

  // loop through and translate each pixel to its new location.
  // this is using a standard rotation algorithm
  For I := 0 To lNewHeight do begin
    For J := 0 To lNewWidth do begin
       lSourceX := Round((J + lMinX) * lCosine + (I + lMinY) * lSine);
       lSourceY := Round((I + lMinY) * lCosine - (J + lMinX) * lSine);
       If (lSourceX >= 0) And (lSourceX <= lWidth) And
       (lSourceY >= 0) And (lSourceY <= lHeight) Then
           BitBlt(hNewBitmapDC, J, I, 1, 1, hBitmapDC,
                      lSourceX, lSourceY, SRCCOPY);
    end;
  end;

  // reset the new bitmap width and height
  lWidth := lNewWidth;
  lHeight := lNewHeight;

  // return the DC to the new bitmap
  hBitmapDC := hNewBitmapDC;

  // destroy the bitmap created
  DeleteObject(hNewBitmap);

end;

{ THotSpots }

//------------------------------------------------------------------------------
function THotSpots.Add: THotSpot;
begin
  Result := THotSpot(inherited Add);
end;

//------------------------------------------------------------------------------
procedure THotSpots.Assign(Source: TPersistent);
var
  i: Integer;
  S: THotSpots;
begin
  inherited;
  S := THotSpots(Source);
  if S = nil then
    Exit;

  oldHeight := S.oldHeight;
  oldWidth := S.oldWidth;
  
  Clear;
  for i := 0 to S.Count-1 do
    Add.Assign(S[i]);
end;

//------------------------------------------------------------------------------
procedure THotSpots.EndUpdate;
begin
  inherited;
  if Assigned(FOwner) then
  begin
    FOwner.FHoveredItem := -1;
    FOwner.FClickedItem := -1;
  end;
end;

constructor THotSpots.Create(AOwner: THotSpotImage);
begin
  inherited Create(THotSpot);
  FOwner := AOwner;
end;

//------------------------------------------------------------------------------
function THotSpots.GetHotSpotImage: THotSpotImage;
begin
  Result := THotSpotImage(FOwner);
end;

//------------------------------------------------------------------------------
function THotSpots.GetItem(Index: Integer): THotSpot;
begin
  Result := THotSpot(inherited GetItem(Index));
end;

//------------------------------------------------------------------------------
function THotSpots.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

//------------------------------------------------------------------------------
procedure THotSpots.LoadFromFile(FName: TFileName);
var
  F: TFileStream;
begin
  Clear;
  F := TFileStream.Create(FName, fmOpenRead or fmShareExclusive);
  try
    LoadFromStream(F);
  finally
    F.Free;
  end;
end;

//------------------------------------------------------------------------------
procedure THotSpots.LoadFromStream(S: TStream);
var
  i, n: Integer;
begin
  if ReadString(S) <> 'HSF1' then
    raise Exception.Create('Incorrect data found while attempting to load hot spots.');
  n := ReadInteger(S); // first integer on the stream is nr. of hotspots
  for i := 1 to n do
    Add.LoadFromStream(S);

  if Assigned(FOwner) then
  begin
    for i := 0 to Count - 1 do
    begin
      Items[i].FPicture := FOwner.FPicture;
      //if Items[i].Clipped then
      with Items[i] do
      begin
        CalcClip(HoverImage,FCHover, HoverImageIndex, HoverColor);
        CalcClip(ClickImage,FCClick, ClickImageIndex, ClickColor);
        CalcClip(FSelectedImage, FCSelect, SelectedImageIndex, SelectedColor);
        if Selected then
          CalcClip(FSelectedImage, FCBlink, SelectedImageIndex, SelectedColor)
        else
          CalcClip(FBlinkImage, FCBlink, BlinkImageIndex, BlinkColor);

        if Selected then
          Self.FOwner.DrawSelectImage(i);
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure THotSpots.ReScale(newWidth, newHeight: Integer);
var
  i:integer;
  deltaX,deltaY,a,b,c,d: Extended;

  //nx1,ny1,nx2,ny2: integer;
begin
  if (oldWidth = 0) or (newWidth = 0)or
     (oldHeight = 0) or (newHeight = 0) then
     Exit;

  if (FOwner <> nil) then
    if (csLoading in FOwner.ComponentState)or (csReading in FOwner.ComponentState) then
      Exit;

  deltaX := newWidth/ oldWidth;
  deltaY := newHeight/ oldHeight;
  oldWidth := newWidth;
  oldHeight := newHeight;

  for i := 0 to Count - 1 do
    with Items[i] do
      if FShape.ShapeType <> stEllipse then
      begin
        a := X1;
        b := X2;
        c := Y1;
        d := Y2;
        //X1 := Round(a*deltaX);
        //Y1 := Round(c*deltaY);
        //X2 := Round(b*deltaX);
        //Y2 := Round(d*deltaY);

        if Round(FX1) = X1 then
          FX1 := FX1*deltaX
        else
          FX1 := a*deltaX;

        if Round(FY1) = Y1 then
          FY1 := FY1*deltaY
        else
          FY1 := c*deltaY;

        if Round(FX2) = X2 then
          FX2 := FX2*deltaX
        else
          FX2 := b*deltaX;

        if Round(FY2) = Y2 then
          FY2 := FY2*deltaY
        else
          FY2 := d*deltaY;

        X1 := Round(FX1);
        Y1 := Round(FY1);
        X2 := Round(FX2);
        Y2 := Round(FY2);
      end
      else
      begin
        (*
        nx1 := round(x1*deltaX);
        ny1 := round(y1*deltaY);
        nx2 := round(x2*deltaX);
        ny2 := round(y2*deltaY);
        *)

        SetRect(Round(X1*deltaX),Round(Y1*deltaY),Round(X2*deltaX),Round(Y2*deltaY));
        CalcClip(FHoverPicture,FCHover, HoverImageIndex, HoverColor);
        CalcClip(FClickPicture,FCClick, ClickImageIndex, ClickColor);
        CalcClip(FSelectedImage, FCSelect, SelectedImageIndex, SelectedColor);

        if Selected then
          CalcClip(FSelectedImage, FCBlink, SelectedImageIndex, SelectedColor)
        else
          CalcClip(FBlinkImage, FCBlink, BlinkImageIndex, BlinkColor);
      end;

  if (FOwner <> nil) then
    FOwner.Invalidate();
end;

//------------------------------------------------------------------------------
procedure THotSpots.ReScale(delta: Integer);
begin
  ReScale(oldWidth + delta,oldHeight + delta);
end;

procedure THotSpots.SaveToFile(FName: TFileName);
var
  F: TFileStream;
begin
  F := TFileStream.Create(FName, fmCreate or fmShareExclusive);
  try
    SaveToStream(F);
  finally
    F.Free;
  end;
end;

//------------------------------------------------------------------------------
procedure THotSpots.SaveToStream(S: TStream);
var i: Integer;
begin
  WriteString(S, 'HSF1');
  WriteInteger(S, Count);
  for i := 0 to Count - 1 do
    Items[i].SavetoStream(S);
end;

//------------------------------------------------------------------------------
procedure THotSpots.SetDimensions(W, H: integer);
begin
  oldWidth := W;
  oldHeight := H;
end;

procedure THotSpots.SetItem(Index: Integer; const Value: THotSpot);
begin
  inherited SetItem(Index, Value);
end;

{ THotSpot }

//------------------------------------------------------------------------------
function THotSpot.GetHeight: Integer;
begin
  Result := FShape.Height;
end;

//------------------------------------------------------------------------------
function THotSpot.GetWidth: Integer;
begin
  Result := FShape.Width;
end;

//------------------------------------------------------------------------------
procedure THotSpot.SetWidth(const Value: Integer);
var
  sang,i: Integer;
  f,xf,a: Real;
begin
  if FShape.Width = Value then
    Exit;

  if (FShape.Points = nil) or (FShape.Width = 0) then
  begin
    SetRect(x1,y1,FShape.Left + Value,y2);
    Exit;
  end;

  sang := Angle;

  if ShapeType = stEllipse then
  with FShape do
  begin
    Angle := 0;
    xf := FEx1 + (FEx2 - FEx1) / 2;
  end
  else
    xf := FShape.Left;

  f := Value / FShape.Width;
  a := xf - xf * f;
  for i := 0 to High(FShape.Points) do
    FShape.Points[i].x := FShape.Points[i].x * f + a;

  FShape.Width := Value;
  if ShapeType = stEllipse then
  begin
    Angle := sang;
    FShape.calcMargins;
  end;
  CalcClip(FHoverPicture,FCHover, HoverImageIndex, HoverColor);
  CalcClip(FClickPicture,FCClick, ClickImageIndex, ClickColor);
  CalcClip(FSelectedImage, FCSelect, SelectedImageIndex, SelectedColor);
  if Selected then
    CalcClip(FSelectedImage, FCBlink, SelectedImageIndex, SelectedColor)
  else
    CalcClip(FBlinkImage, FCBlink, BlinkImageIndex, BlinkColor);
end;

//------------------------------------------------------------------------------
procedure THotSpot.SetHeight(const Value: Integer);
var
  sang,i: Integer;
  yf,a,f: Real;
begin
  if (FShape.Points = nil) or (FShape.Height = 0) then
  begin
    SetRect(x1,y1,x2,FShape.Top+Value);
    Exit;
  end;

  sang := Angle;

  if ShapeType = stEllipse then
  with FShape do
  begin
    Angle := 0;
    yf := FEy1 + (FEy2 - FEy1) / 2;
  end
  else
    yf := FShape.Top;

  f := Value/FShape.Height;

  a := yf - yf * f;

  for i := 0 to High(FShape.Points) do
    FShape.Points[i].y := FShape.Points[i].y * f + a;
  FShape.Height := Value;

  if ShapeType = stEllipse then
  begin
    Angle := sang;
    FShape.calcMargins;
  end;
  
  CalcClip(FHoverPicture,FCHover, HoverImageIndex, HoverColor);
  CalcClip(FClickPicture,FCClick, ClickImageIndex, ClickColor);
  CalcClip(FSelectedImage, FCSelect, SelectedImageIndex, SelectedColor);
  if Selected then
    CalcClip(FSelectedImage, FCBlink, SelectedImageIndex, SelectedColor)
  else
    CalcClip(FBlinkImage, FCBlink, BlinkImageIndex, BlinkColor);
end;

//------------------------------------------------------------------------------
function THotSpot.GetX: Integer;
begin
  Result := FShape.Left;
end;

//------------------------------------------------------------------------------
function THotSpot.GetY: Integer;
begin
  Result := FShape.Top;
end;

//------------------------------------------------------------------------------
procedure THotSpot.SetX(const Value: Integer);
var
  i,delta: Integer;
begin
  if FShape.Left = Value then
    Exit;

  delta := Value - FShape.Left;
  for i := 0 to High(FShape.Points) do
    FShape.Points[i].x := FShape.Points[i].x + delta;

  if ShapeType = stEllipse then
    FShape.CalcMargins;
    
  FShape.Left := Value;
end;

//------------------------------------------------------------------------------
procedure THotSpot.SetY(const Value: Integer);
var
  i,delta: Integer;
begin
  if FShape.Top = Value then
    Exit;

  delta := Value - FShape.Top;

  for i := 0 to High(FShape.Points) do
    FShape.Points[i].y := FShape.Points[i].y + delta;

  if ShapeType = stEllipse then
    FShape.CalcMargins;

  FShape.Top := Value;
end;

//------------------------------------------------------------------------------
function THotSpot.GetX2: Integer;
begin
  Result := X1 + Width;
end;

//------------------------------------------------------------------------------
function THotSpot.GetY2: Integer;
begin
  Result := Y1 + Height;
end;

//------------------------------------------------------------------------------
procedure THotSpot.SetX2(const Value: Integer);
begin
  Width := Value - X1;
end;

//------------------------------------------------------------------------------
procedure THotSpot.SetY2(const Value: Integer);
begin
  Height := Value - Y1;
end;

//------------------------------------------------------------------------------
procedure THotSpot.SetPolyPoints;
begin
  FShape.Points:= Value;
end;

//------------------------------------------------------------------------------
procedure THotSpot.SetPolyPoint;
begin
  if (i < 0) or (i > High(FShape.Points)) then
    Exit;

  FShape.Points[i] := Value;
  if Value.X < X then
  begin
    FShape.Width := Round(FShape.Width+x-Value.x);
    FShape.Left := Round(Value.X);
  end;

  if Value.Y<Y then
  begin
    FShape.Height := Round(FShape.Height+y-Value.y);
    FShape.Top := Round(Value.Y);
  end;

  if (Value.X-FShape.Left) > FShape.Width then
    FShape.Width := Round(Value.X-FShape.Left);

  if (Value.y-FShape.Top) > FShape.Height then
    FShape.Height := Round(Value.Y-FShape.Top);
end;

//------------------------------------------------------------------------------
procedure THotSpot.LoadPoints(Reader: TReader);
var
  i,l: Integer;
begin
  Reader.ReadListBegin;
  l := Reader.ReadInteger;
  FShape.FAngle := Reader.ReadInteger;
  if l > 0 then
  begin
    SetLength(FShape.FPoints,l);
    for i := 0 to l - 1 do
    begin
      FShape.FPoints[i].x := Reader.ReadFloat;
      FShape.FPoints[i].y := Reader.ReadFloat;
    end;
    FShape.CalcMargins;
  end;
  Reader.ReadListEnd;
end;

//------------------------------------------------------------------------------
procedure THotSpot.StorePoints(Writer: TWriter);
var
  i,l: Integer;
begin
  Writer.WriteListBegin;
  l := Length(FShape.FPoints);
  Writer.WriteInteger(l);
  Writer.WriteInteger(FShape.FAngle);
  if l > 0 then
    for i := 0 to l - 1 do
    begin
      Writer.WriteFloat(FShape.FPoints[i].x);
      Writer.writeFloat(FShape.FPoints[i].y);
    end;
  Writer.WriteListEnd;
end;

//------------------------------------------------------------------------------
procedure THotSpot.DefineProperties(Filer: TFiler);
begin
  inherited; { allow base classes to define properties }
  Filer.DefineProperty('PolyPoints', LoadPoints, StorePoints, True);
end;

//------------------------------------------------------------------------------
function THotSpot.getPolyPoints;
begin
  Result := FShape.Points;
end;

//------------------------------------------------------------------------------
function THotSpot.getPolyPoint;
begin
  if (i < 0) or (i > High(FShape.Points)) then
  begin
    Result.x := -1;
    Result.y := -1;
    Exit;
  end;
  Result := FShape.Points[i];
end;

//------------------------------------------------------------------------------
function THotSpot.GetAngle: Integer;
begin
  Result := FShape.Angle;
end;

//------------------------------------------------------------------------------
procedure THotSpot.SetAngle(const Value: Integer);
begin
  FShape.Angle := Value;
end;

//------------------------------------------------------------------------------
procedure THotSpot.ScaleEllipse;
var
  i: Integer;
  signx,signy,ew,eh,tt,tx,ty,sx,sy,cosu,sinu,a,b,c,d,e,f: Extended;
  pp1,pp2,pps: TRealPoint;
begin
  if (Shapetype <> stEllipse) or (Length(FShape.FPoints) < 12) then
    Exit;

  //calculating sinu,cosu and translation factors
  tx := -(FShape.FEx1+(FShape.FEx2-FShape.FEx1)/2);
  ty := -(FShape.FEy1+(FShape.FEy2-FShape.FEy1)/2);
  SinCos((FShape.FAngle*Pi)/180,sinu,cosu);
  //rotating the points so as to be at 0 degrees compared to the ellipse
  pp1 := RPoint(p1.x,p1.y);
  pp2 := RPoint(p2.x,p2.y);
  pps := RPoint(ps.x,ps.y);

  pp1.x := pp1.x + tx;
  pp1.y := pp1.y + ty;

  pp2.x := pp2.x + tx;
  pp2.y := pp2.y + ty;

  pps.x := pps.x + tx;
  pps.y := pps.y + ty;

  tt := pp1.x*cosu-pp1.y*sinu;
  pp1.y := pp1.y*cosu+pp1.x*sinu;
  pp1.x := tt;

  tt := pp2.x*cosu-pp2.y*sinu;
  pp2.y := pp2.y*cosu+pp2.x*sinu;
  pp2.x := tt;

  tt := pps.x*cosu-pps.y*sinu;
  pps.y := pps.y*cosu+pps.x*sinu;
  pps.x := tt;
  //adjusting the sign of scaling relative to the position of the center
  if pps.x < 0 then signx := -1
               else signx := 1;
  if pps.y < 0 then signy := -1
               else signy := 1;

  //current width&height
  with FShape do
  begin
    ew := sqrt((FPoints[6].x-FPoints[0].x)*(FPoints[6].x-FPoints[0].x)+
              (FPoints[6].y-FPoints[0].y)*(FPoints[6].y-FPoints[0].y));
    eh := sqrt((FPoints[9].x-FPoints[3].x)*(FPoints[9].x-FPoints[3].x)+
              (FPoints[9].y-FPoints[3].y)*(FPoints[9].y-FPoints[3].y));
  end;
  //scaling factors
  if (ew = 0) or (eh = 0) then
    Exit;
    
  sx := (ew-signx*(pp2.x-pp1.x))/ew;
  sy := (eh-signy*(pp2.y-pp1.y))/eh;
  
  if ((ew*sx) < 10) and (sx < 1) then
    sx := 1;
  if ((eh*sy) < 10) and (sy < 1) then
    sy := 1;

  //calculating transformation parameters
  //t:=translate(tx,ty)*rotate(u)*scale(sx,sy)*rotate(-u)*translate(-tx,-ty)
  a := cosu*cosu*sx+sy-cosu*cosu*sy;
  b := -cosu*sinu*(sx-sy);
  c := b;
  d := sx+cosu*cosu*(sy-sx);
  e := sx*cosu*cosu*tx-sx*cosu*ty*sinu+sy*tx-sy*tx*cosu*cosu+sy*sinu*ty*cosu-tx;
  f := -sx*sinu*tx*cosu+sx*ty-sx*ty*cosu*cosu+sy*cosu*tx*sinu+sy*cosu*cosu*ty-ty;
  for i := 0 to High(FShape.FPoints) do with FShape.FPoints[i] do
  begin
   tt := x*a+y*c+e;
   y := x*b+y*d+f;
   x := tt;
  end;
  //and calculating new margins
  FShape.CalcMargins;
end;

//------------------------------------------------------------------------------
procedure THotSpot.SetClipped(const Value: Boolean);
begin
  FClipped := Value;
  CalcClip(FHoverPicture,FCHover, HoverImageIndex, HoverColor);
  CalcClip(FClickPicture,FCClick, ClickImageIndex, ClickColor);
  CalcClip(FSelectedImage, FCSelect, SelectedImageIndex, SelectedColor);
  if Selected then
    CalcClip(FSelectedImage, FCBlink, SelectedImageIndex, SelectedColor)
  else
    CalcClip(FBlinkImage, FCBlink, BlinkImageIndex, BlinkColor);
end;

//------------------------------------------------------------------------------
procedure THotSpot.SetDown(const Value: Boolean);
begin
  FDown := Value;
  (Collection as THotSpots).GetHotSpotImage.Invalidate;
end;


//------------------------------------------------------------------------------
procedure THotSpot.CalcClip(const Pict: TPicture; var Bitm: TBitmap; ImageIndex: Integer; Clr: TColor = clNone);
var
  source: TBitmap;
  images: THotSpotImage;
  i,j: Integer;
  a,b,c,d,okW,okH: Integer;
  clipRGN: HRGN;
  r: TRect;
  tempPoints: TIntPoints;
begin
  clipRGN := 0;

  images := (Collection as THotSpots).GetHotSpotImage;
  if {EmptyImage(Pict) or} (images = nil) or (EmptyImage(Pict) and (Clr = clNone) and (ImageIndex < 0)) then
  begin
    Bitm.Height := 0;
    Bitm.Width := 0;
    Exit;
  end;

  if images.FStretch then
  begin
    okW := Width;
    okH := Height;
    a := Round(X1 * images.FPicture.Width / images.Width);
    b := Round(Y1 * images.FPicture.Height / images.Height);
    c := Round(X2 * images.FPicture.Width / images.Width);
    d := Round(Y2 * images.FPicture.Height / images.Height);
  end
  else
  begin
    if not EmptyImage(Pict) then
    begin
      okW := min(Width,Pict.Width);
      okH := min(Height,Pict.Height);
    end
    else
    begin
      okW := Width;
      okH := Height;
    end;
    a := X1;
    b := Y1;
    c := X2;
    d := Y2;
  end;
  bitm.Height := okH; //background;
  bitm.Width := okW;
  if Assigned(FPicture) and (not EmptyImage(FPicture)) then
  begin
    r := Rect(0,0,okW,okH);
    if (not EmptyImage(Pict) and not images.Stretch) or (EmptyImage(Pict) and (Clr = clNone)) then
    begin
      source := TBitmap.Create;
      try
        source.PixelFormat := pf24bit;
        Source.Width := FPicture.Width;
        Source.Height := FPicture.Height;
        Source.Canvas.Draw(0,0,FPicture.Graphic);
        //r := Rect(0,0,okW,okH);
        bitm.Canvas.CopyRect(r,source.Canvas,Rect(a,b,c,d));
      finally
        source.free;
      end;
    end;

    {
        j := High(FShape.Points);
        SetLength(tempPoints,(j + 1));
        for i := 0 to j do
          begin
            tempPoints[i].x:=Round(FShape.Points[i].X) - X1;
            tempPoints[i].y:=Round(FShape.Points[i].Y) - Y1;
          end;
    }
    if FClipped then
    begin
      if FShape.FShapeType <> stEllipse then
      begin
        j := High(FShape.Points);
        SetLength(tempPoints,(j + 1));
        for i := 0 to j do
          begin
            tempPoints[i].x:=Round(FShape.Points[i].X) - X1;
            tempPoints[i].y:=Round(FShape.Points[i].Y) - Y1;
          end;
        clipRGN := CreatePolygonRgn(tempPoints[0],j + 1,  ALTERNATE);
      end
      else
        clipRGN := CreateEllipticRgn(0,0,okW,okH);

      if clipRGN <> 0 then
        SelectClipRgn(bitm.Canvas.Handle,clipRGN);
    end;

    if not EmptyImage(Pict) then
    begin
      if images.Stretch then
        bitm.Canvas.StretchDraw(r,Pict.Graphic)
      else
        bitm.Canvas.Draw(0,0,Pict.Graphic);
    end
    else if (Clr <> clNone) then
    begin
      bitm.Canvas.Brush.Color := Clr;
      bitm.Canvas.FillRect(Rect(0, 0, bitm.Width, bitm.Height));
    end;

    if Assigned(Images.Images) and (ImageIndex >= 0) then
      Images.Images.Draw(bitm.Canvas, OffSetX, OffSetY, ImageIndex);

    if FClipped then
    begin
      SelectClipRgn(bitm.Canvas.Handle,0);
      if clipRGN <> 0 then
        DeleteObject(clipRGN);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure THotSpot.ReCalcClips;
begin
  if (FOwner <> nil) and (FOwner.FOwner <> nil) then
  begin
    FPicture := (FOwner.GetOwner as THotSpotImage).FPicture;
    CalcClip(FHoverPicture,FCHover, HoverImageIndex, HoverColor);
    CalcClip(FClickPicture,FCClick, ClickImageIndex, ClickColor);
    CalcClip(FSelectedImage, FCSelect, SelectedImageIndex, SelectedColor);
    if Selected then
      CalcClip(FSelectedImage, FCBlink, SelectedImageIndex, SelectedColor)
    else
      CalcClip(FBlinkImage, FCBlink, BlinkImageIndex, BlinkColor);
  end;
end;

//------------------------------------------------------------------------------

procedure THotSpot.SetOffsetX(const Value: Integer);
begin
  FOffsetX := Value;
end;

//------------------------------------------------------------------------------

procedure THotSpot.SetOffsetY(const Value: Integer);
begin
  FOffsetY := Value;
end;

//------------------------------------------------------------------------------
function THotSpot.GetBorderPolypos(X,Y:Integer;var p1,p2:integer):TBorderPoly;
begin
  Result := FShape.BorderPolypos(x,y,p1,p2);
end;

//------------------------------------------------------------------------------
function THotSpot.GetDisplayName: String;
begin
  Result := 'HotSpot' + IntToStr(Index);
end;

//------------------------------------------------------------------------------
procedure THotSpot.Assign(Source: TPersistent);
var
  S: THotSpot;
begin

  S := THotSpot(Source);
  FX1 := S.FX1;
  FY1 := S.FY1;
  FX2 := S.FX2;
  FY2 := S.FY2;

  Hint := S.Hint;
  SetRect(S.X1, S.Y1, S.X2, S.Y2);
  FClipped := S.FClipped;
  HoverImage.Assign(S.HoverImage);
  ClickImage.Assign(S.ClickImage);

  FBlink := S.Blink;
  FSelected := S.FSelected;
  Selectable := S.Selectable;
  FBlinkImageIndex := S.FBlinkImageIndex;
  FSelectedImageIndex := S.FSelectedImageIndex;
  FClickImageIndex := S.FClickImageIndex;
  FHoverImageIndex := S.FHoverImageIndex;
  FBlinkColor := S.FBlinkColor;
  FHoverColor := S.FHoverColor;
  FSelectedColor := S.FSelectedColor;
  FClickColor := S.FClickColor;
  FSelectedImage.Assign(S.FSelectedImage);
  FBlinkImage.Assign(S.FBlinkImage);
  FOffsetY := S.OffsetY;
  FOffsetX := S.OffsetX;

  // Added the name,id&shapetype
  Name := S.Name;
  Id := S.Id;
  ShapeType := S.ShapeType;
  PolyPoints := S.PolyPoints;
  FShape.FAngle := S.FShape.FAngle;
end;

//------------------------------------------------------------------------------
function THotSpot.GetHoverPos(X, Y: Integer): THoverPosition;
begin
  if FPicture = nil then
  begin
    if (FOwner <> nil) and (FOwner.FOwner <> nil) then
    begin
      FPicture := (FOwner.getOwner as THotSpotImage).FPicture;
      CalcClip(FHoverPicture,FCHover, HoverImageIndex, HoverColor);
      CalcClip(FClickPicture,FCClick, ClickImageIndex, ClickColor);
      CalcClip(FSelectedImage, FCSelect, SelectedImageIndex, SelectedColor);
      if Selected then
        CalcClip(FSelectedImage, FCBlink, SelectedImageIndex, SelectedColor)
      else
        CalcClip(FBlinkImage, FCBlink, BlinkImageIndex, BlinkColor);
    end;
  end
  else
    if (EmptyImage(FPicture)) then
    begin
      if (FOwner <> nil) and (FOwner.FOwner <> nil) then
      begin
        FPicture := (FOwner.GetOwner as THotSpotImage).FPicture;
        CalcClip(FHoverPicture,FCHover, HoverImageIndex, HoverColor);
        CalcClip(FClickPicture,FCClick, ClickImageIndex, ClickColor);
        CalcClip(FSelectedImage, FCSelect, SelectedImageIndex, SelectedColor);
        if Selected then
          CalcClip(FSelectedImage, FCBlink, SelectedImageIndex, SelectedColor)
        else
          CalcClip(FBlinkImage, FCBlink, BlinkImageIndex, BlinkColor);
      end;
    end;
  Result := FShape.GetHoverPos(x,y);
end;

//------------------------------------------------------------------------------
function THotSpot.GetRectHoverPos(X, Y: Integer): THoverPosition;
begin
  Result := FShape.RectPos(x,y);
end;

//------------------------------------------------------------------------------
procedure THotSpot.DrawShape(Canvas: TCanvas);
begin
  FShape.Draw(Canvas);
end;

//------------------------------------------------------------------------------
procedure THotSpot.DrawShapeAt(Canvas: TCanvas;X,Y: Integer);
begin
  FShape.DrawAt(Canvas,X,Y);
end;

//------------------------------------------------------------------------------
function THotSpot.AsRect: TRect;
begin
  Result := Rect(X1, Y1, X2, Y2);
end;


// helper methods for TPicture streaming

procedure WritePicture(Stream: TStream; Picture: TPicture);
var
  s: string;
  Str: TmemoryStream;
begin
  s := GraphicExtension(TGraphicClass(Picture.Graphic.ClassType));
  SetLength(s,3);
  WriteString(Stream, S);
  Str := TMemoryStream.Create;
  try
    Picture.Graphic.SaveToStream(Str);
    WriteInteger(Stream, Str.Size);
    Str.Position := 0;
    Stream.CopyFrom(Str, Str.Size)
  finally
    Str.free
  end;
end;

function AddBackslash(const s: string): string;
begin
  if (Length(s) >= 1) and (s[Length(s)]<>'\') then
    Result := s + '\'
  else
    Result := s;
end;

{$IFNDEF DELPHI_UNICODE}
function TempDirectory: string;
var
  buf: array[0..MAX_PATH] of Char;
begin
  GetTempPath(sizeof(buf),buf);
  Result := AddBackslash(StrPas(buf));
end;
{$ENDIF}

{$IFDEF DELPHI_UNICODE}
function TempDirectory: string;
var
  buf:string;
  i: integer;
begin
  SetLength(buf, MAX_PATH);
  i := GetTempPath(Length(buf), PChar(buf));
  SetLength(buf, i);
  Result := AddBackslash(buf);
end;
{$ENDIF}


procedure ReadPicture(Stream: TStream; Picture: TPicture);
var
  s: string;
  tmp: TFileStream;
  i: integer;
begin
  s := ReadString(Stream);
  i := ReadInteger(Stream);
  s := TempDirectory+'tmp~.'+s;
  tmp := TFileStream.Create(s, fmCreate);
  try
    tmp.CopyFrom(Stream, i);
  finally
    tmp.Free;
  end;
  Picture.LoadFromFile(s);
  DeleteFile(pchar(s));
end;

//------------------------------------------------------------------------------
procedure THotSpot.LoadFromStream(S: TStream);
var
  tt: TPoints;
  i,j: Integer;
begin

  if TMSHotSpot_V1Compatibility then
  begin
    FHoverColor := clNone;
    FClickColor := clNone;
    FSelectedColor := clNone;
    FBlinkColor := clNone;
    FSelected := false;
    FBlink := false;
    BlinkImage := nil;
    SelectedImage := nil;
  end;

  FName := ReadString(S);
  X1 := ReadInteger(S);
  Y1 := ReadInteger(S);
  X2 := ReadInteger(S);
  Y2 := ReadInteger(S);
  FID := ReadInteger(S);
  Shapetype := TSpotShapeType(ReadInteger(S));

  if ShapeType = stPolygon then
  begin
    j := ReadInteger(S);

    SetLength(tt,j);

    for i := 0 to High(tt) do
    begin
      tt[i].x := ReadInteger(S);
      tt[i].y := ReadInteger(S);
    end;
    FShape.Points := tt;
  end;

  FShape.FAngle := ReadInteger(S);
  if ReadInteger(S) = 0 then
    Clipped := False
  else
    Clipped := True;

  FHint := ReadString(S);

  i := ReadInteger(S);
  if i = 1 then
    ReadPicture(S,FHoverPicture);

  i := ReadInteger(S);
  if i = 1 then
    ReadPicture(S,FClickPicture);

  // added support to load Down status of a HotSpot
  FDown := Boolean(ReadInteger(S));

  if TMSHotSpot_V1Compatibility then
    Exit;

  i := ReadInteger(S);
  if i = 1 then
    ReadPicture(S, FSelectedImage)
  else
    SelectedImage := nil;

  i := ReadInteger(S);
  if i = 1 then
    ReadPicture(S, FBlinkImage);

  FSelected := Boolean(ReadInteger(S));
  FBlink := Boolean(ReadInteger(S));

  FHoverColor := ReadInteger(S);
  FClickColor := ReadInteger(S);
  FSelectedColor := ReadInteger(S);
  FBlinkColor := ReadInteger(S);
end;

//------------------------------------------------------------------------------
procedure THotSpot.SaveToStream(S: TStream);
var
  i: Integer;
begin
  WriteString(S, FName);
  WriteInteger(S, X1);
  WriteInteger(S, Y1);
  WriteInteger(S, X2);
  WriteInteger(S, Y2);
  WriteInteger(S, FID);
  WriteInteger(S, ord(ShapeType));
  if ShapeType = stPolygon then
  begin
    WriteInteger(S, Length(FShape.Points));
    for i := 0 to High(FShape.Points) do
    begin
      WriteInteger(S, Round(FShape.Points[i].x));
      WriteInteger(S, Round(FShape.Points[i].y));
    end;
  end;
  WriteInteger(S, FShape.FAngle);
  if Clipped then
    WriteInteger(S,1)
  else
    WriteInteger(S,0);

  WriteString(S, FHint);

  if Assigned(FHoverPicture) and Assigned(FHoverPicture.Graphic) and not FHoverPicture.Graphic.Empty then
  begin
    WriteInteger(S, 1);
    WritePicture(S, FHoverPicture);
  end
  else
    WriteInteger(S,0);

  if Assigned(FClickPicture) and Assigned(FClickPicture.Graphic) and not FClickPicture.Graphic.Empty then
  begin
    WriteInteger(S, 1);
    WritePicture(S, FClickPicture);
  end
  else
    WriteInteger(S,0);

  // added support to save Down status of a HotSpot
  WriteInteger(S,Integer(FDown));

  
  if Assigned(FSelectedImage) and Assigned(FSelectedImage.Graphic) and not FSelectedImage.Graphic.Empty then
  begin
    WriteInteger(S, 1);
    WritePicture(S, FSelectedImage);
  end
  else
    WriteInteger(S,0);

  if Assigned(FBlinkImage) and Assigned(FBlinkImage.Graphic) and not FBlinkImage.Graphic.Empty then
  begin
    WriteInteger(S, 1);
    WritePicture(S, FBlinkImage);
  end
  else
    WriteInteger(S,0);

  WriteInteger(S,Integer(FSelected));
  WriteInteger(S,Integer(FBlink));

  WriteInteger(S,Integer(FHoverColor));
  WriteInteger(S,Integer(FClickColor));
  WriteInteger(S,Integer(FSelectedColor));
  WriteInteger(S,Integer(FBlinkColor));
end;

//------------------------------------------------------------------------------
procedure THotSpot.SetRect(AX1, AY1, AX2, AY2: Integer);
var
  tp: TPoints;
begin
  case FShape.ShapeType of
  stRectangle:
    begin
      SetLength(tp,4);
      tp[0].x := Min(AX1, AX2);
      tp[0].y := Min(AY1, AY2);
      tp[2].x := Max(AX1, AX2);
      tp[2].y := Max(AY1, AY2);

      tp[1].x := tp[2].x;
      tp[1].y := tp[0].y;
      tp[3].x := tp[0].x;
      tp[3].y := tp[2].y;
      FShape.CalcMargins;
      FShape.Points := tp;
    end;
  stEllipse:
    begin
      FShape.Left := Min(AX1, AX2);
      FShape.Top := Min(AY1, AY2);
      FShape.Width := Max(AX1, AX2)-FShape.Left;
      FShape.Height := Max(AY1, AY2)-FShape.Top;
      FShape.EllipseToBezier;
    end;
 end;
end;

//------------------------------------------------------------------------------
procedure THotSpot.SetRect(R: TRect);
var
  tp: TPoints;
begin
  case FShape.ShapeType of
  stRectangle:
    begin
      SetLength(tp,4);
      tp[0].x := Min(R.Left, R.Right);
      tp[0].y := Min(R.Top, R.Bottom);
      tp[1].x := Max(R.Left, R.Right);
      tp[1].y := Max(R.Top, R.Bottom);

      tp[1].x := tp[2].x;
      tp[1].y := tp[0].y;
      tp[3].x := tp[0].x;
      tp[3].y := tp[2].y;

      FShape.Points := tp;
      FShape.CalcMargins;
    end;
  stEllipse:
    begin
      FShape.Left := Min(R.Left, R.Right);
      FShape.Top := Min(R.Top, R.Bottom);
      FShape.Width := Max(R.Left, R.Right)-FShape.Left;
      FShape.Height := Max(R.Top, R.Bottom)-FShape.Top;
      FShape.EllipseToBezier;
    end;
 end;
end;

//------------------------------------------------------------------------------
procedure THotSpot.SetShapetype(const Value: TSpotShapeType );
begin
  FShape.ShapeType := Value;
  if Value = stEllipse then
    FShape.EllipseToBezier;

  CalcClip(FHoverPicture,FCHover, HoverImageIndex, HoverColor);
  CalcClip(FClickPicture,FCClick, ClickImageIndex, ClickColor);
  CalcClip(FSelectedImage, FCSelect, SelectedImageIndex, SelectedColor);
  if Selected then
    CalcClip(FSelectedImage, FCBlink, SelectedImageIndex, SelectedColor)
  else
    CalcClip(FBlinkImage, FCBlink, BlinkImageIndex, BlinkColor);
end;

//------------------------------------------------------------------------------
function THotSpot.GetShapeType: TSpotShapeType;
begin
  Result := FShape.ShapeType;
end;

//------------------------------------------------------------------------------
procedure THotSpot.SetClickPicture(const Value: TPicture);
begin
  FClickPicture.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure THotSpot.SetHoverPicture(const Value: TPicture);
begin
  FHoverPicture.Assign(Value);
end;

//------------------------------------------------------------------------------
constructor THotSpot.Create(Collection: TCollection);
begin
  inherited;
  FHoverPicture := TPicture.Create;
  FClickPicture := TPicture.Create;
  FHoverPicture.OnChange := HoverPictureChange;
  FClickPicture.OnChange := ClickPictureChange;
  FCClick := TBitmap.Create;
  FCHover := TBitmap.Create;
  FCSelect := TBitmap.Create;
  FCBlink := TBitmap.Create;
  FShape := THotSpotShape.Create(stRectangle);
  FShowClick:=false;
  if Collection is THotSpots then
  begin
    FOwner := Collection as THotSpots;
  end;

  FSelectedImage := TPicture.Create;
  FSelectedImage.OnChange := SelectedImageChange;
  FBlinkImage := TPicture.Create;
  FBlinkImage.OnChange := BlinkImageChange;
  FBlink := False;
  FSelectable := True;
  FSelected := False;
  FBlinkImageIndex := -1;
  FSelectedImageIndex := -1;
  FClickImageIndex := -1;
  FHoverImageIndex := -1;
  FBlinkColor := clWhite;
  FHoverColor := clYellow;
  FSelectedColor := clRed;
  FClickColor := clGray;
  FOffsetY := 4;
  FOffsetX := 4;
end;

//------------------------------------------------------------------------------
destructor THotSpot.Destroy;
begin
  FHoverPicture.Free;
  FClickPicture.Free;
  FCClick.Free;
  FShape.Free;
  FCHover.Free;
  FCSelect.Free;
  FCBlink.Free;
  FSelectedImage.Free;
  FBlinkImage.Free;
  inherited;
end;

//------------------------------------------------------------------------------

procedure THotSpot.SetShowClick(const Value: Boolean);
begin
  FShowClick := Value;
  (Collection as THotSpots).GetHotSpotImage.Invalidate;
end;

//------------------------------------------------------------------------------

procedure THotSpot.SetBlink(const Value: Boolean);
var
  HSImage: THotSpotImage;
begin
  if (FBlink <> Value) then
  begin
    FBlink := Value;
    if Blink then
    begin
      if Selected then
        CalcClip(FSelectedImage, FCBlink, SelectedImageIndex, SelectedColor)
      else
        CalcClip(FBlinkImage, FCBlink, BlinkImageIndex, BlinkColor);
    end;
    
    HSImage := (Collection as THotSpots).GetHotSpotImage;
    if FBlink and Assigned(HSImage) and not (csDesigning in HSImage.ComponentState) and not (csLoading in HSImage.ComponentState) then
      HSImage.FTimer.Enabled := True;
  end;
end;

//------------------------------------------------------------------------------

procedure THotSpot.SetBlinkImage(const Value: TPicture);
begin
  FBlinkImage.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure THotSpot.InternalClick;
begin
  Selected := not Selected;
end;

//------------------------------------------------------------------------------

procedure THotSpot.UpdateExclusive;
var
  i: Integer;
begin
  for i:= 0 to (Collection as THotSpots).Count - 1 do
  begin
    if ((Collection as THotSpots).Items[i] <> Self) and ((Collection as THotSpots).Items[i].Selected) then
      (Collection as THotSpots).Items[i].Selected := False;
  end;
end;

//------------------------------------------------------------------------------

procedure THotSpot.SetSelectable(const Value: Boolean);
begin
  if (FSelectable <> Value) then
  begin
    if not Value and Selected then
      Selected := False;
    FSelectable := Value;
  end;
end;

//------------------------------------------------------------------------------

procedure THotSpot.SetSelected(const Value: Boolean);
var
  images: THotSpotImage;
  Ctrl: Boolean;
begin
  if (FSelected <> Value) then
  begin
    images := (Collection as THotSpots).GetHotSpotImage;
    if (images = nil) or (not Selectable and Value) then
      Exit;

    FSelected := Value;

    BlinkImageChange(BlinkImage);
    if FSelected then
    begin
      case images.SelectionMode of
        smSingleSelect:
        begin
          UpdateExclusive;
        end;
        smMultiSelectCtrl:
        begin
          Ctrl := (GetKeyState(VK_CONTROL) and $8000 = $8000);
          if not Ctrl then
            UpdateExclusive;
        end;
        smMultiSelect:
        begin
          // do nothing
        end;
      end;
      images.DrawSelectImage(Index);
    end
    else
      images.RestoreHotSpot(Index);
  end;
end;

//------------------------------------------------------------------------------

procedure THotSpot.SetClickColor(const Value: TColor);
begin
  if (FClickColor <> Value) then
  begin
    FClickColor := Value;
    CalcClip(FClickPicture, FCClick, ClickImageIndex, ClickColor);
  end;
end;

//------------------------------------------------------------------------------

procedure THotSpot.SetHoverColor(const Value: TColor);
begin
  if (FHoverColor <> Value) then
  begin
    FHoverColor := Value;
    CalcClip(FHoverPicture,FCHover, HoverImageIndex, HoverColor);
  end;
end;

//------------------------------------------------------------------------------

procedure THotSpot.SetSelectedColor(const Value: TColor);
var
  images: THotSpotImage;
begin
  if (FSelectedColor <> Value) then
  begin
    FSelectedColor := Value;

    CalcClip(FSelectedImage, FCSelect, SelectedImageIndex, SelectedColor);

    if Blink then
    begin
      if Selected then
        CalcClip(FSelectedImage, FCBlink, SelectedImageIndex, SelectedColor)
      else
        CalcClip(FBlinkImage, FCBlink, BlinkImageIndex, BlinkColor);
    end;

    if Selected and not Blink then
    begin
      images := (Collection as THotSpots).GetHotSpotImage;
      if (images <> nil) then
        images.DrawSelectImage(Index);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure THotSpot.SetSelectedImage(const Value: TPicture);
begin
  FSelectedImage.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure THotSpot.SetSelectedImageIndex(const Value: integer);
begin
  if (FSelectedImageIndex <> Value) then
  begin
    FSelectedImageIndex := Value;
  end;
end;

//------------------------------------------------------------------------------

procedure THotSpot.BlinkImageChange(Sender: TObject);
begin
  if Selected then
    CalcClip(FSelectedImage, FCBlink, SelectedImageIndex, SelectedColor)
  else
    CalcClip(FBlinkImage, FCBlink, BlinkImageIndex, BlinkColor);
end;

//------------------------------------------------------------------------------

procedure THotSpot.SelectedImageChange(Sender: TObject);
begin
  CalcClip(FSelectedImage, FCSelect, SelectedImageIndex, SelectedColor);
end;

{ THotSpotImage }

//------------------------------------------------------------------------------
procedure THotSpotImage.CMHintShow(var M: TMessage);
var
  HInfo: PHintInfo;
begin
  HInfo := PHintInfo(M.LParam);
  if FHoveredItem <> -1 then
    HInfo.HintStr := FHotSpots[FHoveredItem].Hint
  else
    HInfo.HintStr := Hint;
end;

procedure THotSpotImage.CMMouseEnter(var Msg: TMessage);
begin

end;

procedure THotSpotImage.CMMouseLeave(var Msg: TMessage);
begin
  if HoveredItem <> -1 then
  begin
    if Assigned(FOnHotSpotExit) then
      FOnHotSpotExit(Self, FHotSpots[HoveredItem]);
    HoveredItem := -1;
  end;
end;


//------------------------------------------------------------------------------
constructor THotSpotImage.Create(AOwner: TComponent);
begin
  inherited;

  ControlStyle := ControlStyle + [csAcceptsControls];


  if AOwner is TControl then
    FOwner := TControl(AOwner)
  else
    FOwner := nil;

  FRotating := False;
  FPicture := TPicture.Create;
  FPicture.OnChange := PictureChanged;

  FHotSpotCursor := crDefault;
  FHotSpots := THotSpots.Create(Self);
  FHoveredItem := -1;
  FClickedItem := -1;
  FBitmap := TBitmap.Create;
  FSelectionMode := smSingleSelect;

  FBlinkInterval := 100;
  FBlinkCount := 0;
  FTimer := TTimer.Create(Self);
  FTimer.Enabled := False;
  FTimer.Interval := BlinkInterval;
  FTimer.OnTimer := OnTimerTime;
  Width := 200;
  Height := 200;
  FAngle := 0;
end;

//------------------------------------------------------------------------------
destructor THotSpotImage.Destroy;
begin
  FHotSpots.Free;
  FPicture.Free;
  FBitmap.Free;
  FTimer.Free;
  inherited;
end;

//------------------------------------------------------------------------------
procedure THotSpotImage.DrawClickImage(HotSpotIndex: Integer;canv:TCanvas = nil);
var
  c:^TCanvas;
  clipRGN: HRGN;
  tempPoints: TIntPoints;
  i, j: Integer;
  okW, okH: Integer;
begin
  if (HotSpotIndex < 0) or (HotSpotIndex >= FHotSpots.Count) then
    Exit;

  if canv = nil then
    c := @canvas
  else
    c := @canv;

  clipRGN := 0;
  if FHotSpots[HotSpotIndex].Clipped then
  begin
    if FStretch then
    begin
      okW := FHotSpots[HotSpotIndex].Width;
      okH := FHotSpots[HotSpotIndex].Height;
    end
    else
    begin
      if not EmptyImage(FHotSpots[HotSpotIndex].ClickImage) then
      begin
        okW := min(Width, FHotSpots[HotSpotIndex].ClickImage.Width);
        okH := min(Height, FHotSpots[HotSpotIndex].ClickImage.Height);
      end
      else
      begin
        okW := FHotSpots[HotSpotIndex].Width;
        okH := FHotSpots[HotSpotIndex].Height;
      end;
    end;

    if FHotSpots[HotSpotIndex].FShape.FShapeType <> stEllipse then
    begin
      j := High(FHotSpots[HotSpotIndex].FShape.Points);
      SetLength(tempPoints,(j+1));
      for i:=0 to j do
        begin
          tempPoints[i].x:=Round(FHotSpots[HotSpotIndex].FShape.Points[i].X){- FHotSpots[HotSpotIndex].X1};
          tempPoints[i].y:=Round(FHotSpots[HotSpotIndex].FShape.Points[i].Y){- FHotSpots[HotSpotIndex].Y1};
        end;
      clipRGN := CreatePolygonRgn(tempPoints[0], j + 1, ALTERNATE);
    end
    else
      clipRGN := CreateEllipticRgn(FHotSpots[HotSpotIndex].X,FHotSpots[HotSpotIndex].Y, FHotSpots[HotSpotIndex].X + okW, FHotSpots[HotSpotIndex].Y + okH);

    if clipRGN <> 0 then
      SelectClipRgn(c.Handle,clipRGN);
  end;

  with FHotSpots[HotSpotIndex] do
    if not EmptyImage(ClickImage) then
      c.Draw(X, Y, FCClick)
    else
    begin
      if not EmptyBitmap(FCClick) then
        c.Draw(X, Y, FCClick)
    end;

  if FHotSpots[HotSpotIndex].Clipped then
  begin
    SelectClipRgn(c.Handle,0);
    if clipRGN <> 0 then
      DeleteObject(clipRGN);
  end;

  if not FPainting and Assigned(OnPaint) then
    OnPaint(Self);
end;

//------------------------------------------------------------------------------
procedure THotSpotImage.DrawHoverImage(HotSpotIndex: Integer;canv:TCanvas = nil);
var
  c:^TCanvas;
  clipRGN: HRGN;
  tempPoints: TIntPoints;
  i, j: Integer;
  okW, okH: Integer;
begin
  if (HotSpotIndex < 0) or (HotSpotIndex>= FHotSpots.Count) then
    Exit;

  if FHotSpots[HotSpotIndex].Selected or FHotSpots[HotSpotIndex].Blink then
    Exit;

  if canv = nil then
  begin
    c := @canvas;
  end
  else
    c := @canv;

  clipRGN := 0;
  if FHotSpots[HotSpotIndex].Clipped then
  begin
    if FStretch then
    begin
      okW := FHotSpots[HotSpotIndex].Width;
      okH := FHotSpots[HotSpotIndex].Height;
    end
    else
    begin
      if not EmptyImage(FHotSpots[HotSpotIndex].HoverImage) then
      begin
        okW := min(Width, FHotSpots[HotSpotIndex].HoverImage.Width);
        okH := min(Height, FHotSpots[HotSpotIndex].HoverImage.Height);
      end
      else
      begin
        okW := FHotSpots[HotSpotIndex].Width;
        okH := FHotSpots[HotSpotIndex].Height;
      end;
    end;

    if FHotSpots[HotSpotIndex].FShape.FShapeType <> stEllipse then
    begin
      j := High(FHotSpots[HotSpotIndex].FShape.Points);
      SetLength(tempPoints,(j+1));
      for i:=0 to j do
        begin
          tempPoints[i].x:=Round(FHotSpots[HotSpotIndex].FShape.Points[i].X){- FHotSpots[HotSpotIndex].X1};
          tempPoints[i].y:=Round(FHotSpots[HotSpotIndex].FShape.Points[i].Y){- FHotSpots[HotSpotIndex].Y1};
        end;
      clipRGN := CreatePolygonRgn(tempPoints[0], j + 1, ALTERNATE);
    end
    else
      clipRGN := CreateEllipticRgn(FHotSpots[HotSpotIndex].X,FHotSpots[HotSpotIndex].Y, FHotSpots[HotSpotIndex].X + okW, FHotSpots[HotSpotIndex].Y + okH);

    if clipRGN <> 0 then
      SelectClipRgn(c.Handle,clipRGN);
  end;

  with FHotSpots[HotSpotIndex] do
    if not EmptyImage(HoverImage) then
      c.Draw(X,Y,FCHover)
    else
    begin
      if not EmptyBitmap(FCHover) then
        c.Draw(X,Y,FCHover)
    end;

  if FHotSpots[HotSpotIndex].Clipped then
  begin
    SelectClipRgn(c.Handle,0);
    if clipRGN <> 0 then
      DeleteObject(clipRGN);
  end;

  if not FPainting and Assigned(OnPaint) then
    OnPaint(Self);
end;

//------------------------------------------------------------------------------

procedure THotSpotImage.DrawSelectImage(HotSpotIndex: Integer;canv:TCanvas = nil);
var
  c:^TCanvas;
  clipRGN: HRGN;
  tempPoints: TIntPoints;
  i, j: Integer;
  okW, okH: Integer;  
begin
  if (HotSpotIndex < 0) or (HotSpotIndex>= FHotSpots.Count) then
    Exit;

  if canv = nil then
  begin
    c := @canvas;
  end
  else
    c := @canv;

  clipRGN := 0;
  if FHotSpots[HotSpotIndex].Clipped then
  begin
    if FStretch then
    begin
      okW := FHotSpots[HotSpotIndex].Width;
      okH := FHotSpots[HotSpotIndex].Height;
    end
    else
    begin
      if not EmptyImage(FHotSpots[HotSpotIndex].SelectedImage) then
      begin
        okW := min(Width, FHotSpots[HotSpotIndex].SelectedImage.Width);
        okH := min(Height, FHotSpots[HotSpotIndex].SelectedImage.Height);
      end
      else
      begin
        okW := FHotSpots[HotSpotIndex].Width;
        okH := FHotSpots[HotSpotIndex].Height;
      end;
    end;

    if FHotSpots[HotSpotIndex].FShape.FShapeType <> stEllipse then
    begin
      j := High(FHotSpots[HotSpotIndex].FShape.Points);
      SetLength(tempPoints,(j+1));
      for i:=0 to j do
        begin
          tempPoints[i].x:=Round(FHotSpots[HotSpotIndex].FShape.Points[i].X){- FHotSpots[HotSpotIndex].X1};
          tempPoints[i].y:=Round(FHotSpots[HotSpotIndex].FShape.Points[i].Y){- FHotSpots[HotSpotIndex].Y1};
        end;
      clipRGN := CreatePolygonRgn(tempPoints[0], j + 1, ALTERNATE);
    end
    else
      clipRGN := CreateEllipticRgn(FHotSpots[HotSpotIndex].X,FHotSpots[HotSpotIndex].Y, FHotSpots[HotSpotIndex].X + okW, FHotSpots[HotSpotIndex].Y + okH);

    if clipRGN <> 0 then
      SelectClipRgn(c.Handle,clipRGN);
  end;

  with FHotSpots[HotSpotIndex] do
    if not EmptyImage(SelectedImage) then
      c.Draw(X,Y,FCSelect)
    else
    begin
      if not EmptyBitmap(FCSelect) then
        c.Draw(X,Y,FCSelect)
    end;

  if FHotSpots[HotSpotIndex].Clipped then
  begin
    SelectClipRgn(c.Handle,0);
    if clipRGN <> 0 then
      DeleteObject(clipRGN);
  end;

  if not FPainting and Assigned(OnPaint) then
    OnPaint(Self);
end;

//------------------------------------------------------------------------------

procedure THotSpotImage.DrawBlinkImage(HotSpotIndex: Integer;
  canv: TCanvas);
var
  c:^TCanvas;
  clipRGN: HRGN;
  tempPoints: TIntPoints;
  i, j: Integer;
  okW, okH: Integer;
begin
  if (HotSpotIndex < 0) or (HotSpotIndex>= FHotSpots.Count) then
    Exit;

  if canv = nil then
  begin
    c := @canvas;
  end
  else
    c := @canv;

  clipRGN := 0;
  if FHotSpots[HotSpotIndex].Clipped then
  begin
    if FStretch then
    begin
      okW := FHotSpots[HotSpotIndex].Width;
      okH := FHotSpots[HotSpotIndex].Height;
    end
    else
    begin
      if not EmptyImage(FHotSpots[HotSpotIndex].BlinkImage) then
      begin
        okW := min(Width, FHotSpots[HotSpotIndex].BlinkImage.Width);
        okH := min(Height, FHotSpots[HotSpotIndex].BlinkImage.Height);
      end
      else
      begin
        okW := FHotSpots[HotSpotIndex].Width;
        okH := FHotSpots[HotSpotIndex].Height;
      end;
    end;

    if FHotSpots[HotSpotIndex].FShape.FShapeType <> stEllipse then
    begin
      j := High(FHotSpots[HotSpotIndex].FShape.Points);
      SetLength(tempPoints,(j+1));
      for i:=0 to j do
        begin
          tempPoints[i].x:=Round(FHotSpots[HotSpotIndex].FShape.Points[i].X){- FHotSpots[HotSpotIndex].X1};
          tempPoints[i].y:=Round(FHotSpots[HotSpotIndex].FShape.Points[i].Y){- FHotSpots[HotSpotIndex].Y1};
        end;
      clipRGN := CreatePolygonRgn(TempPoints[0],j + 1, ALTERNATE);
    end
    else
      clipRGN := CreateEllipticRgn(FHotSpots[HotSpotIndex].X,FHotSpots[HotSpotIndex].Y, FHotSpots[HotSpotIndex].X + okW, FHotSpots[HotSpotIndex].Y + okH);

    if clipRGN <> 0 then
      SelectClipRgn(c.Handle,clipRGN);
  end;

  with FHotSpots[HotSpotIndex] do
    {if not EmptyImage(BlinkImage) then
      c.Draw(X,Y,FCBlink)
    else
    begin}
      if not EmptyBitmap(FCBlink) then
        c.Draw(X,Y,FCBlink);
    //end;

  if FHotSpots[HotSpotIndex].Clipped then
  begin
    SelectClipRgn(c.Handle,0);
    if clipRGN <> 0 then
      DeleteObject(clipRGN);
  end;

  if not FPainting and Assigned(OnPaint) then
    OnPaint(Self);
end;

//------------------------------------------------------------------------------

procedure THotSpotImage.Paint;
var
  i: Integer;
  B: TBitmap;
  TC: TBitmap;
  r:TRect;
begin

  if not Assigned(FPicture.Graphic) then
  begin
    if (csDesigning in ComponentState) then
    begin
      Canvas.Pen.Style := psDash;
      Canvas.Pen.Color := FColor;
      Canvas.Brush.Style := bsClear;
      Canvas.Rectangle(0,0,Width,Height);
      Canvas.Pen.Style := psSolid;
    end;
    
    if Assigned(OnPaint) then
      OnPaint(Self);
    Exit;
  end;

  FPainting := True;

  r.Left := 0;
  r.Top := 0;
  r.Right := FPicture.Graphic.Width;
  r.Bottom := FPicture.Graphic.Height;
  // draw the surrounding rect in design mode

  B := TBitmap.Create;

  B.PixelFormat := pf24bit;

  if not FPicture.Graphic.Empty then
  begin
    B.Width := FPicture.Graphic.Width;
    B.Height := FPicture.Graphic.Height;
  end
  else
  begin
    B.Width := Width;
    B.Height := Height;
  end;

{

  with B.Canvas do
    if not FPicture.Graphic.Empty then
      Draw(0, 0, FPicture.Graphic);

  Canvas.Draw(0,0,B);

  B.Free;
  Exit;
}

  with B.Canvas do
  try
    if not FTransparent then
    begin
      Brush.Style := bsSolid;
      Brush.Color := FColor;
      FillRect(r);
    end
    else
    begin
//      Brush.Style := bsSolid;
//      Brush.Color := FColor;
//      FillRect(r);
    end;

    if not FPicture.Graphic.Empty then
      Draw(0, 0, FPicture.Graphic);

    // when in design mode, draw the hot spots
    if not( csDesigning in ComponentState) then
    begin
      // every time the control is painted, I save it's image into a bitmap
      // that I use to repaint the control after the mouse exits a hotspot
      FBitmap.Width := Width;
      FBitmap.Height := Height;
      if FStretch then
      Begin
        r.Right := Width;
        r.Bottom := Height;
      End;
      FBitmap.Canvas.StretchDraw(R, B);
      r.Right:=B.Width;
      r.Bottom:=B.Height;
    end;

  finally
    B.TransparentColor := B.Canvas.Pixels[0,0];
    B.TransparentMode := tmAuto;
    B.Transparent := FTransparent;

    r.Right := Width;
    r.Bottom := Height;
    TC := TBitmap.Create;
    TC.PixelFormat := pf24bit;
    try
      TC.Width := Width;
      TC.Height := Height;
      TC.Canvas.Brush.Color := FColor;
      TC.Canvas.FillRect(r);

      if FStretch then
        TC.Canvas.StretchDraw(r,B)
      else
        TC.Canvas.Draw(0,0,B);

      if csDesigning in ComponentState then
      begin
        TC.Canvas.Pen.Style := psDot;
        TC.Canvas.Pen.Mode := pmNOTXOR;
        TC.Canvas.Pen.Width := 1;
        TC.Canvas.Pen.Color := clBlack;
        TC.Canvas.Brush.Style := bsClear;// Modified to draw the shape
        for i := 0 to FHotSpots.Count-1 do
        begin
          FHotSpots[i].DrawShape(TC.Canvas);
        end;
      end
      else
      begin
        //Draw the hoverimage
        if (FHoveredItem >= 0) and (FClickedItem = -1) and (not FHotSpots[FHoveredItem].FShowClick) and not FHotSpots[FHoveredItem].Selected then
          DrawHoverImage(FHoveredItem,TC.Canvas);

        //Draw the clickedimage
        if (FClickedItem <> -1) then
          DrawClickImage(FClickedItem,TC.Canvas);

        for i := 1 to FHotSpots.Count do
        begin
          if FHotSpots[i - 1].Down or FHotSpots[i - 1].ShowClick then
          begin
            DrawClickImage(i - 1,TC.Canvas);
          end
          else if FHotSpots[i - 1].Selected then
          begin
            DrawSelectImage(i - 1, TC.Canvas);
          end;
        end;
      end;
    finally
      if FTransparent then
      begin
        TC.TransparentColor := TC.Canvas.Pixels[0,0];
        TC.Transparent := true;
        TC.TransparentMode := tmAuto;
      end;
      Canvas.Draw(0,0,Tc);
      FPainting := False;
      TC.Free;
    end;

    B.Free;
  end;

  if Assigned(OnPaint) then
    OnPaint(Self);
end;

//------------------------------------------------------------------------------
procedure THotSpotImage.RestoreHotSpot(HotSpotIndex: Integer);
var
  clipRGN: HRGN;
  tempPoints: TIntPoints;
  i, j: Integer;
  okW, okH: Integer;
begin
  with FHotSpots[HotSpotIndex] do
  begin
    if not FHotSpots[HotSpotIndex].Down and (not FHotSpots[HotSpotIndex].Selected or FHotSpots[HotSpotIndex].Blink) then
    begin
      clipRGN := 0;
      if FHotSpots[HotSpotIndex].Clipped then
      begin
        if FStretch then
        begin
          okW := FHotSpots[HotSpotIndex].Width;
          okH := FHotSpots[HotSpotIndex].Height;
        end
        else
        begin
          if not EmptyImage(FHotSpots[HotSpotIndex].HoverImage) then
          begin
            okW := min(Width, FHotSpots[HotSpotIndex].HoverImage.Width);
            okH := min(Height, FHotSpots[HotSpotIndex].HoverImage.Height);
          end
          else if not EmptyImage(FHotSpots[HotSpotIndex].SelectedImage) then
          begin
            okW := min(Width, FHotSpots[HotSpotIndex].SelectedImage.Width);
            okH := min(Height, FHotSpots[HotSpotIndex].SelectedImage.Height);
          end
          else
          begin
            okW := FHotSpots[HotSpotIndex].Width;
            okH := FHotSpots[HotSpotIndex].Height;
          end;
        end;

        if FHotSpots[HotSpotIndex].FShape.FShapeType <> stEllipse then
        begin
          j := High(FHotSpots[HotSpotIndex].FShape.Points);
          SetLength(tempPoints,(j+1));
          for i:=0 to j do
            begin
              tempPoints[i].x:=Round(FHotSpots[HotSpotIndex].FShape.Points[i].X){- FHotSpots[HotSpotIndex].X1};
              tempPoints[i].y:=Round(FHotSpots[HotSpotIndex].FShape.Points[i].Y){- FHotSpots[HotSpotIndex].Y1};
            end;
          clipRGN := CreatePolygonRgn(tempPoints[0], j + 1, ALTERNATE);
        end
        else
          clipRGN := CreateEllipticRgn(FHotSpots[HotSpotIndex].X,FHotSpots[HotSpotIndex].Y, FHotSpots[HotSpotIndex].X + okW, FHotSpots[HotSpotIndex].Y + okH);

        if clipRGN <> 0 then
          SelectClipRgn(canvas.Handle,clipRGN);
      end;

      Canvas.CopyRect(AsRect, FBitmap.Canvas, AsRect);

      if FHotSpots[HotSpotIndex].Clipped then
      begin
        SelectClipRgn(canvas.Handle,0);
        if clipRGN <> 0 then
          DeleteObject(clipRGN);
      end;

      if not FPainting and Assigned(OnPaint) then
        OnPaint(Self);
    end;
  end;
end;

//------------------------------------------------------------------------------

function Vektor(FromP, Top: TPoint): TPoint;
begin
  Result.x := Top.x - FromP.x;
  Result.y := Top.y - FromP.y;
end;

function xComp(Vektor: TPoint; Angle: Extended): Integer;
begin
  Result := Round(Vektor.x * cos(Angle) - (Vektor.y) * sin(Angle));
end;

function yComp(Vektor: TPoint; Angle: Extended): Integer;
begin
  Result := Round((Vektor.x) * (sin(Angle)) + (vektor.y) * cos(Angle));
end;

//------------------------------------------------------------------------------

procedure THotSpotImage.Rotate(Clockwise: Boolean);
var
  OldStretch: Boolean;
begin
  if not Picture.Graphic.Empty then
  begin
    FRotating := True;
    OldStretch := Stretch;
    Stretch := False;
    if Clockwise then
    begin
      FAngle := FAngle + (-90);
      InternalRotate90Degrees;
    end
    else
    begin
      FAngle := FAngle + 90;
      InternalRotate90Degrees;
      InternalRotate90Degrees;
      InternalRotate90Degrees;
    end;
    FRotating := False;
    Invalidate;

    if (Stretch <> OldStretch) then
      Stretch := OldStretch;
  end;
end;

//------------------------------------------------------------------------------

procedure THotSpotImage.RotateReset;
var
  i, j: Integer;
  OldStretch: Boolean;
begin
  if Picture.Graphic.Empty or (FAngle = 0) then
    Exit;

  FRotating := True;
  OldStretch := Stretch;
  Stretch := False;
  if (FAngle < 0) then
  begin
           // its -ve
    j := (360 + FAngle) div 90;
    for I := 1 to j do
      InternalRotate90Degrees;
    FAngle := 0;
  end
  else
  begin
    j := FAngle div 90;
    for I := 1 to j do
      InternalRotate90Degrees;
    FAngle := 0;
  end;

  FRotating := False;
  Invalidate;
  if (Stretch <> OldStretch) then
    Stretch := OldStretch;

end;

//------------------------------------------------------------------------------

procedure THotSpotImage.InternalRotate90Degrees;
var
  BackColor: TColor;
  RotBitmap, bmp: TBitmap;
  i, j: Integer;
  newx, newy: Extended;
  H, Angl: integer;
begin
  if not Picture.Graphic.Empty then
  begin
    h := FPicture.Graphic.Height;

    Angl := -90;
    BackColor := FColor;
    bmp := TBitmap.Create;
    try
      bmp.Width := FPicture.Graphic.Width;
      bmp.Height := FPicture.Graphic.Height;
      bmp.PixelFormat := pf24bit;
      bmp.Canvas.Draw(0, 0, FPicture.Graphic);
      RotBitmap := CreateRotatedBitmap(bmp, Angl, BackColor);
      RotBitmap.TransparentColor := BackColor;
      RotBitmap.Transparent := Picture.Bitmap.Transparent;
      FPicture.Assign(RotBitmap);
      Picture := FPicture;
      RotBitmap.Free;
    finally
      bmp.Free;
    end;

    for i := 0 to FHotSpots.Count-1 do
    begin
      for j := 0 to High(FHotSpots[i].FShape.FPoints) do
      begin
        newY := FHotSpots[i].FShape.FPoints[j].x;
        newX := h - FHotSpots[i].FShape.FPoints[j].y;
        FHotSpots[i].FShape.FPoints[j].x := newX;
        FHotSpots[i].FShape.FPoints[j].y := newY;
      end;
      FHotSpots[i].FShape.CalcMargins;
      FHotSpots[i].ReCalcClips;
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure THotSpotImage.SetColor(const Value: TColor);
begin
  FColor := Value;
  Repaint;
end;

//------------------------------------------------------------------------------
procedure THotSpotImage.SetHotSpots(const Value: THotSpots);
begin
  FHotSpots.Assign(Value);
  FHoveredItem := -1;
  FClickedItem := -1;
  Repaint;
end;

//------------------------------------------------------------------------------
procedure THotSpotImage.SetHoveredItem(const Value: Integer);
var
  i: integer;
  oldhovereditem: integer;
begin
  if FHoveredItem <> Value then
  begin
    Application.CancelHint;
    
    // restore the image beneath the hovered item
    if (FHoveredItem <> -1) and (FClickedItem = -1) then
    begin
      if not HotSpots[FHoveredItem].FShowClick then
        RestoreHotSpot(FHoveredItem);

      if Assigned(FOnHotSpotExit)
        then FOnHotSpotExit(Self, FHotSpots[FHoveredItem]);
    end;

    // if there is a hovered item, draw it's hover image (if assigned)
    if (Value <> -1) and (FClickedItem = -1) then
    begin
      if not HotSpots[Value].FShowClick then
        DrawHoverImage(Value);

      if Assigned(FOnHotSpotEnter) then
        FOnHotSpotEnter(Self, FHotSpots[Value]);
    end;

    oldhovereditem := FHoveredItem;

    FHoveredItem := Value;

    for i := 0 to HotSpots.Count - 1 do
    begin
      if HotSpots[i].Down and (i = OldHoveredItem) then
      begin
        //RestoreHotSpot(i);
        // at least one needs to be invalidated
        Invalidate;
        break;
      end;
    end;
    
  end;
end;

//------------------------------------------------------------------------------
procedure THotSpotImage.SetPicture(const Value: TPicture);
var
  i: Integer;
begin
  FPicture.Assign(Value);
  for i := 0 to FHotSpots.Count - 1 do
  begin
    FHotSpots[i].FPicture := FPicture;
    if FHotSpots[i].Clipped then
    with FHotSpots[i] do
    begin
      CalcClip(HoverImage,FCHover, HoverImageIndex, HoverColor);
      CalcClip(ClickImage,FCClick, ClickImageIndex, ClickColor);
      CalcClip(FSelectedImage, FCSelect, SelectedImageIndex, SelectedColor);
      if Selected then
        CalcClip(FSelectedImage, FCBlink, SelectedImageIndex, SelectedColor)
      else
        CalcClip(FBlinkImage, FCBlink, BlinkImageIndex, BlinkColor);
    end;
  end;
  if not FRotating then
    Repaint;
end;

//------------------------------------------------------------------------------

procedure THotSpotImage.DblClick;
var
  pt: TPoint;
  i: Integer;
  PM,PC1,PC2: TPoint;
begin
  inherited;

  isDblClick := true;
  
  GetCursorPos(pt);
  pt := ScreenToClient(pt);

  if FHoveredItem = -1 then
  begin
      PM.x := pt.X;
      PM.y := pt.Y;
      PC1.x := Left;
      PC1.y := Top;
      PC2.x := Width;
      PC2.y := Height;
      if (PM.x < PC1.x) or (PM.y < PC1.y) or (PM.x > PC2.x) or (PM.y > PC2.y)then
      begin
        HoveredItem := -1;
        Exit;
      end;

    for i := HotSpots.Count - 1 downto 0 do
      if HotSpots[i].GetHoverPos(pt.X,pt.Y) <> hpNone then
      begin
        HoveredItem := i;

        isMouseUp := False;
        FClickedItem := i;
        DrawClickImage(i);

        if Assigned(OnHotSpotDblClick) then
          FOnHotSpotDblClick(Self,HotSpots[i]);

        Exit;
      end;
    HoveredItem := -1;
  end
  else
  begin
    isMouseUp := False;
    FClickedItem := FHoveredItem;
    DrawClickImage(FClickedItem);
    if Assigned(OnHotSpotDblClick) then
      FOnHotSpotDblClick(Self,HotSpots[FClickedItem]);
  end;
end;


//------------------------------------------------------------------------------
procedure THotSpotImage.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  i: Integer;
  PM,PC1,PC2: TPoint;
begin
  inherited;
  if isDblClick then
  begin
    isDblClick := False;
    Exit;
  end;
  //this happens when the app gets focus through click on hotspot
  if FHoveredItem = -1 then
  begin
      PM.x := X;
      PM.y := Y;
      PC1.x := Left;
      PC1.y := Top;
      PC2.x := Width;
      PC2.y := Height;
      if (PM.x < PC1.x) or (PM.y < PC1.y) or (PM.x > PC2.x) or (PM.y > PC2.y)then
      begin
        HoveredItem := -1;
        Exit;
      end;

    for i := HotSpots.Count - 1 downto 0 do
      if HotSpots[i].GetHoverPos(X,Y) <> hpNone then
      begin
        HoveredItem := i;

        if Button = mbRight then
          if Assigned(FOnHotSpotRightClick) then
            FOnHotSpotRightClick(Self,HotSpots[i]);

        if Button = mbLeft then
        begin
          isMouseUp := False;
          FClickedItem := i;
          DrawClickImage(i);
          HotSpots[i].InternalClick;
        end;

        Exit;
      end;
    HoveredItem := -1;
  end
  else
  begin
    if Button = mbRight then
      if Assigned(FOnHotSpotRightClick) then
        FOnHotSpotRightClick(Self,HotSpots[HoveredItem]);

     if Button = mbLeft then
     begin
       isMouseUp := False;
       FClickedItem := FHoveredItem;
       DrawClickImage(FClickedItem);
       HotSpots[FClickedItem].InternalClick;
     end;
  end;
end;

//------------------------------------------------------------------------------
procedure THotSpotImage.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;

  //to cope with loosing focus
  if isMouseUp then
    Exit
  else
    isMouseUp := True;

  if FClickedItem <> -1 then
  begin
    with FHotSpots[FClickedItem] do
      if not HotSpots[FClickedItem].FShowClick and not HotSpots[FClickedItem].Selected then
      begin
        //Canvas.CopyRect(AsRect, FBitmap.Canvas, AsRect);
        RestoreHotSpot(FClickedItem);
      end;

    // the hotspot is clicked only when the mouse button is released within it's bounds
    if FHotSpots[FClickedItem].GetHoverPos(X, Y) <> hpNone then
    begin
      if FHoveredItem <> -1 then
      begin
        if not HotSpots[FHoveredItem].FShowClick then
          DrawHoverImage(FHoveredItem);
      end;
      if Assigned(FOnHotSpotClick) then
        FOnHotSpotClick(Self, FHotSpots[FClickedItem]);
      FClickedItem := -1;
    end
    else
    begin
      if Assigned(FOnHotSpotExit) then
        FOnHotSpotExit(Self, FHotSpots[FClickedItem]);
      FClickedItem := -1;
      FHoveredItem := -1;
    end;
  end;

end;


//------------------------------------------------------------------------------
procedure THotSpotImage.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  i: Integer;
  PM,PC1,PC2: TPoint;
begin
  inherited;

  // the hover image only appears at runtime
  // if the left mouse button is down, the hover image does NOT appear

  if (csDesigning in ComponentState) then
    Exit;

  PM.x := X;
  PM.y := Y;
  PC1.x := 0;
  PC1.y := 0;
  PC2.x := Width;
  PC2.y := Height;
  if (PM.x < PC1.x) or (PM.y < PC1.y) or (PM.x > PC2.x) or (PM.y > PC2.y) then
  begin
    HoveredItem := -1;
    Cursor := FOrigCursor;
    Exit;
  end;

  for i := HotSpots.Count - 1 downto 0 do
    if HotSpots[i].GetHoverPos(X, Y) <> hpNone then
    begin
      HoveredItem := i;
      Cursor := FHotSpotCursor;
      Exit;
    end;

  Cursor := FOrigCursor;
  HoveredItem := -1;
end;

//------------------------------------------------------------------------------
procedure THotSpotImage.SetTransparent(const Value: Boolean);
begin
  FTransparent := Value;
  if (not FTransparent)  then
    ControlStyle := ControlStyle + [csOpaque]
  else  // picture might not cover entire clientrect
    ControlStyle := ControlStyle - [csOpaque];
  Invalidate;
end;

function THotSpotImage.GetHotSpotXY(x, y: Integer): THotSpot;
var
  i: Integer;
begin
  Result := nil;
  for i := HotSpots.Count - 1 downto 0 do
    if HotSpots[i].GetHoverPos(X, Y) <> hpNone then
    begin
      Result := HotSpots[i];
      Exit;
    end;
end;

//------------------------------------------------------------------------------
procedure THotSpotImage.SetAutoSizeEx(const Value: Boolean);
begin
  FAutoSize := Value;
  if FAutoSize then Stretch:=false;
  if FAutoSize and Assigned(FPicture.Graphic) then
  begin
    if not FPicture.Graphic.Empty then
    begin
      Width := FPicture.Graphic.Width;
      Height := FPicture.Graphic.Height;
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure THotSpotImage.PictureChanged(Sender: TObject);
begin
  if FAutoSize then
    SetAutoSizeEx(FAutoSize);
  if not FStretch then
  begin                              
    HotSpots.oldHeight := FPicture.Height;
    HotSpots.oldWidth := FPicture.Width;
  end;
  Invalidate;
end;


//------------------------------------------------------------------------------
procedure THotSpotImage.SetStretch(const Value: Boolean);
begin
  if FStretch = value then
    Exit;
    
  FStretch := Value;
  if FStretch then
  Begin
    AutoSize := false;
    if (csLoading in ComponentState)or (csReading in ComponentState) then
    Begin
      HotSpots.oldHeight := Height;
      HotSpots.oldWidth := Width;
    End
    else
    Begin
      HotSpots.oldHeight := FPicture.Height;
      HotSpots.oldWidth := FPicture.Width;
      HotSpots.ReScale(Width,Height);
    End;
  End
  else
      HotSpots.ReScale(FPicture.Width,FPicture.Height);
end;

//------------------------------------------------------------------------------
procedure THotSpotImage.Resize;
begin
  if FStretch then
    HotSpots.ReScale(Width,Height);
  inherited;
end;

{ THotSpotShape }

//------------------------------------------------------------------------------
constructor THotSpotShape.Create(Shape: TSpotShapeType);
begin
  self.FShapeType := Shape;
end;

//------------------------------------------------------------------------------
Destructor THotSpotShape.Destroy;
begin
  inherited;
end;

//------------------------------------------------------------------------------
procedure THotspotShape.CalcMargins;
var
  i,minx,miny,maxx,maxy: Integer;
begin
  if Length(FPoints) < 3 then
    Exit;

  minx := Round(FPoints[0].x);
  maxx := Round(FPoints[0].x);
  miny := Round(FPoints[0].y);
  maxy := Round(FPoints[0].y);
  FEx1 := (FPoints[0].x);
  FEx2 := (FPoints[0].x);
  FEy1 := (FPoints[0].y);
  FEy2 := (FPoints[0].y);
  for i := 1 to High(FPoints) do
  begin
    if ((i mod 3) = 0) then
    begin
      if FPoints[i].x < FEx1 then FEx1 := FPoints[i].x;
      if FPoints[i].x > FEx2 then FEx2 := FPoints[i].x;
      if FPoints[i].y > FEy2 then FEy2 := FPoints[i].y;
      if FPoints[i].y < FEy1 then FEy1 := FPoints[i].y;
    end;
    if FPoints[i].x < minx then minx := Round(FPoints[i].x);
    if FPoints[i].x > maxx then maxx := Round(FPoints[i].x);
    if FPoints[i].y > maxy then maxy := Round(FPoints[i].y);
    if FPoints[i].y < miny then miny := Round(FPoints[i].y);
  end;

  FLeft := minx;
  FTop := miny;
  FWidth := maxx - minx;
  FHeight := maxy - miny;
end;

//------------------------------------------------------------------------------
procedure THotspotShape.SetPoints;
var
  i: Integer;
begin
  if Length(value) < 3 then
    Exit;

  SetLength(FPoints,Length(value));
  for i := 0 to High(value) do
  begin
    FPoints[i].x := Value[i].x;
    FPoints[i].y := Value[i].y;
  end;
  CalcMargins;
end;

//------------------------------------------------------------------------------
function THotSpotShape.GetHoverPos(X, Y: Integer): THoverPosition;
begin
  case FShapeType of
  stRectangle:Result := RectPos(x,y);
  stEllipse:Result := EllipsePos(x,y);
  stPolygon:Result := PolyPos(x,y);
  else
    Result := hpNone; //this shouldn't happen
  end;
end;

//------------------------------------------------------------------------------
function THotSpotShape.RectPos(X, Y: Integer): THoverPosition;
var
  x1,x2,y1,y2: Integer;
begin
  x1 := Left;
  y1 := Top;
  x2 := Left + Width;
  y2 := Top + Height;

  if Between(Y, Y1, Y2) and Between(X, X1, X2) then
    Result := hpInside
  else
    Result := hpNone;
end;

//------------------------------------------------------------------------------
function THotSpotShape.EllipsePos(X, Y: Integer): THoverPosition;
var
  xc,yc,sinu,cosu,tt,a,b,xe,ye: Extended;
begin
  if (Width = 0) or (Height = 0) then
  begin
    Result := hpNone;
    Exit;
  end;
  //if it's outside the bounding rectangle then out
  if (x < (FLeft - CornerSize)) or (x > (FLeft + Width + CornerSize)) or
     (y < (FTop - CornerSize)) or (y > (FTop + Height + CornerSize)) then
  begin
    Result := hpNone;
    Exit;
  end;

  //calculates the a and b ellipse parameters
  a := sqrt((FPoints[0].x-FPoints[6].x)*(FPoints[0].x-FPoints[6].x)+
          (FPoints[0].y-FPoints[6].y)*(FPoints[0].y-FPoints[6].y))/2;
  b := sqrt((FPoints[9].x-FPoints[3].x)*(FPoints[9].x-FPoints[3].x)+
          (FPoints[9].y-FPoints[3].y)*(FPoints[9].y-FPoints[3].y))/2;

  //fex,fey are width and height only for the four points of the ellipse
  xc := FEx1+(FEx2-FEx1)/2;
  yc := FEy1+(FEy2-FEy1)/2;

  //getting the center of the ellipse in the origin and rotating back to 0 deg
  xe := x - xc;
  ye := y - yc;
  SinCos((FAngle/180)*Pi,sinu,cosu);
  xe := xe*cosu-ye*sinu;
  ye := (x - xc)*sinu+ye*cosu;

  tt := (xe*xe)/(a*a)+(ye*ye)/(b*b);
  if Abs(tt-1) < EllipseTolerance then
    Result := hpBorder
  else
    if tt < 1 then
      Result := hpInside
    else
      Result := hpNone;
end;

//------------------------------------------------------------------------------
function THotSpotShape.BorderPolypos(X,Y: Integer;var p1,p2: Integer): TBorderPoly;
var
  i: Integer;
begin
  if Fpoints = nil then
  begin
    Result := bNone;
    Exit;
  end;

  (*
  if RectPos(x,y) = hpNone then
  begin
    Result := bNone;
    Exit;
  end;
  *)

  //The first point
  if Between(x,FPoints[0].X-CornerSize,FPoints[0].X+CornerSize) and
     Between(y,FPoints[0].y-CornerSize,FPoints[0].y+CornerSize) then
  begin
    Result := bPoint;
    p1 := 0;
    Exit;
  end;
  for i := 1 to high(FPoints) do
  begin
    //if it's in the vecinity of the poly points
    if Between(x,FPoints[i].X-CornerSize,FPoints[i].X+CornerSize) and
       Between(y,FPoints[i].y-CornerSize,FPoints[i].y+CornerSize) then
    begin
      Result:=bPoint;
      p1 := i;
      Exit;
    end;
    //or near one of the segments
    if NearLine(Point(x,y),PRound(FPoints[i-1]),PRound(FPoints[i])) then
    begin
      Result := bLine;
      p1 := i - 1;
      p2 := i;
      Exit;
    end;
  end;
  //The line between the first and last point
  if NearLine(Point(x,y),PRound(FPoints[0]),PRound(FPoints[High(FPoints)])) then
  begin
    Result := bLine;
    p1 := 0;
    p2 := High(FPoints);
    Exit;
  end;

  if PolyPos(x,y) = hpInside then
    Result := bInside
  else
    Result := bNone;
end;

//------------------------------------------------------------------------------
procedure THotSpotShape.Draw(Canvas :TCanvas);
var
  tp: array of TPoint;
  i: Integer;
begin
  SetLength(tp,Length(FPoints));
  for i := 0 to High(FPoints) do
    tp[i] := PRound(FPoints[i]);

  case FShapeType of
  stEllipse:Canvas.PolyBezier(tp);
  stRectangle,stPolygon:if FPoints <> nil then
    Canvas.Polygon(tp);
  end;
end;

//------------------------------------------------------------------------------
procedure THotSpotShape.drawAt(Canvas :TCanvas;X,Y :Integer);
var
  i: Integer;
  tp: TIntPoints;
begin
  if Length(FPoints) = 0 then
    Exit;

  SetLength(tp,length(FPoints));
  for i := 0 to High(FPoints) do
    tp[i] := Point(Round(FPoints[i].x-FLeft),Round(FPoints[i].y-FTop));

  case FShapeType of
  stEllipse:FillEllipse(Canvas,tp);
  stPolygon,stRectangle:Canvas.Polygon(tp);
  end;
end;

//------------------------------------------------------------------------------
procedure THotSpotShape.SetAngle(const Value: Integer);
var
  i: Integer;
  a,b,tt,cosu,sinu,yc,xc: Extended;
begin
  if Value = FAngle then
    Exit;

  a := ((FAngle-Value)*Pi)/180.0;
  SinCos(a,sinu,cosu);
  xc := FLeft + FWidth/2;
  yc := FTop + FHeight/2;
  //rotation constants
  a := xc - xc * cosu + yc * sinu;
  b := yc - xc * sinu - yc * cosu;
  //calculate the new points
  for i := 0 to High(FPoints) do
  begin
    tt := (FPoints[i].x*cosu-FPoints[i].y*sinu+a);
    FPoints[i].y := (FPoints[i].y*cosu+FPoints[i].x*sinu+b);
    FPoints[i].x := tt;
  end;

  FAngle := Value;
  CalcMargins;
end;

{
/*************************************************************************

 * FUNCTION:   Polypos
 *
 * PURPOSE
 * This routine determines if the point passed is in the polygon. It uses

 * the classical polygon hit-testing algorithm: a horizontal ray starting

 * at the point is extended infinitely rightwards and the number of
 * polygon edges that intersect the ray are counted. If the number is odd,

 * the point is inside the polygon.
 *
 *************************************************************************/
}

function THotSpotShape.PolyPos(X,Y:Integer):THoverPosition;
var
  ppt: TPoint;
  i,wnumintsct: Word;
  pt1,pt2: TPoint;
begin
  if Length(FPoints)<3 then
  begin
    Result := hpNone;
    Exit;
  end;

  if (not Between(X, FLeft, FLeft+FWidth)) or (not Between(Y, FTop, FTop+FHeight)) then
  begin
    Result := hpNone;
    Exit;
  end;

  wnumintsct := 0;
  pt1.x := x;
  pt1.y := y;
  pt2.y := y;
  pt2.x := 2560;

  // Now go through each of the lines in the polygon and see if it
  // intersects
  for i := 0 to Length(FPoints) - 2 do
  begin
    ppt := PRound(FPoints[i]);
    if (Intersect(pt1, pt2, PRound(FPoints[i]), PRound(FPoints[i+1]))) then
      Inc(wnumintsct);
  end;
  // And the last line
  if (Intersect(pt1, pt2, PRound(FPoints[High(FPoints)]), PRound(FPoints[0]))) then
    Inc(wnumintsct);

  if(Odd(wnumintsct)) then
    Result := hpInside
  else
    Result := hpNone;
end;

//------------------------------------------------------------------------------
procedure THotSpotShape.EllipseToBezier;
var
  ofx,ofy,xc,yc: Real;
  a: Integer;
begin
  SetLength(FPoints,13);

  ofx := FWidth*EToBConst;
  ofy := FHeight*EToBConst;
  xc := FLeft+FWidth/2;
  yc := FTop+FHeight/2;

  FPoints[0].x  := FLeft;                     //------------------------/
  FPoints[1].x  := FLeft;                     //                        /
  FPoints[11].x := FLeft;                     //        2___3___4       /
  FPoints[12].x := FLeft;                     //     1             5    /
  FPoints[5].x  := FLeft + FWidth;            //     |             |    /
  FPoints[6].x  := FLeft + FWidth;            //     |             |    /
  FPoints[7].x  := FLeft + FWidth;            //     0,12          6    /
  FPoints[2].x  := xc - ofx;                  //     |             |    /
  FPoints[10].x := xc - ofx;                  //     |             |    /
  FPoints[4].x  := xc + ofx;                  //    11             7    /
  FPoints[8].x  := xc + ofx;                  //       10___9___8       /
  FPoints[3].x  := xc;                        //                        /
  FPoints[9].x  := xc;                        //------------------------*

  FPoints[2].y  := FTop;
  FPoints[3].y  := FTop;
  FPoints[4].y  := FTop;
  FPoints[8].y  := FTop + FHeight;
  FPoints[9].y  := FTop + FHeight;
  FPoints[10].y := FTop + FHeight;
  FPoints[7].y  := yc + ofy;
  FPoints[11].y := yc + ofy;
  FPoints[1].y  := yc - ofy;
  FPoints[5].y  := yc - ofy;
  FPoints[0].y  := yc;
  FPoints[12].y := yc;
  FPoints[6].y  := yc;

  if FAngle <> 0 then
  begin
    a := FAngle;
    FAngle := 0;
    SetAngle(a);
  end
  else
    CalcMargins;
end;

//------------------------------------------------------------------------------
procedure THotSpotShape.FillEllipse;
begin
  if Canvas.Handle = 0 then
    Exit;
  //open path bracket
  BeginPath(Canvas.Handle);
  //draw shape
  Canvas.PolyBezier(Pts);
  //close path bracket
  EndPath(Canvas.Handle);
  //fill path
  FillPath(Canvas.Handle);
end;

procedure THotSpot.ClickPictureChange(Sender: TObject);
begin
  CalcClip(FClickPicture,FCClick, ClickImageIndex, ClickColor);
end;

procedure THotSpot.HoverPictureChange(Sender: TObject);
begin
  CalcClip(FHoverPicture,FCHover, HoverImageIndex, HoverColor);
end;

procedure THotSpotImage.Loaded;
begin
  inherited;
  FTimer.Interval := BlinkInterval;
  FTimer.Enabled := (not (csDesigning in ComponentState)) and (FTimer.Interval > 0);

  FOrigCursor := Cursor;
  SetPicture(Picture);
end;

function THotSpotImage.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

procedure THotSpotImage.SetVersion(const Value: string);
begin

end;

function THotSpotImage.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure THotSpotImage.SetImages(const Value: TCustomImageList);
begin
  if (FImages <> Value) then
  begin
    FImages := Value;
  end;
end;

procedure THotSpotImage.SetBlinkInterval(const Value: Integer);
begin
  FBlinkInterval := Value;
  if not (csDesigning in ComponentState) then
  begin
    FTimer.Interval := FBlinkInterval;
    FTimer.Enabled := (not (csDesigning in ComponentState)) and (FTimer.Interval > 0);
  end;  
end;

procedure THotSpotImage.OnTimerTime(Sender: TObject);
begin
  UpdateHotSpotsBlinking;
end;

procedure THotSpotImage.UpdateHotSpotsBlinking;
var
  i: Integer;
  found: Boolean;
begin
  if (csDesigning in ComponentState) then
    Exit;

  found := False;  
  for i := 0 to FHotSpots.Count - 1 do
  begin
    if HotSpots[i].Blink then
    begin
      found := True;
      if (FBlinkCount = 0) then
        DrawBlinkImage(i)
      else
        RestoreHotSpot(i);
    end;
  end;

  if FBlinkCount > 0 then
    FBlinkCount := 0
  else
    FBlinkCount := 1;
    
  if not found then
    FTimer.Enabled := False;
end;

function THotSpotImage.HotSpotByID(ID: integer): THotSpot;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to FHotSpots.Count - 1 do
  begin
    if FHotSpots[i].ID = ID then
    begin
      Result := FHotSpots[i];
      break;
    end;
  end;

end;

function THotSpotImage.HotSpotByName(Name: string): THotSpot;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to FHotSpots.Count - 1 do
  begin
    if FHotSpots[i].Name = Name then
    begin
      Result := FHotSpots[i];
      break;
    end;
  end;

end;


function THotSpotImage.GetSelectedIndex: Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to FHotSpots.Count - 1 do
  begin
    if FHotSpots[i].Selected then
    begin
      Result := i;
      Break;
    end;
  end;
end;


{$IFDEF FREEWARE}
{$I TRIAL.INC}
{$ENDIF}



end.
