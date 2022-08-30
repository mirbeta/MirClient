{**************************************************************************}
{ TAdvCurve                                                                }
{ for Delphi & C++Builder                                                  }
{                                                                          }
{ written                                                                  }
{   TMS Software                                                           }
{   copyright © 2013                                                       }
{   Email : info@tmssoftware.com                                           }
{   Web : http://www.tmssoftware.com                                       }
{                                                                          }
{ The source code is given as is. The author is not responsible            }
{ for any possible damage done due to the use of this code.                }
{ The component can be freely used in any application. The complete        }
{ source code remains property of the author and may not be distributed,   }
{ published, given or sold in any form as such. No parts of the source     }
{ code can be included in any other component or application without       }
{ written authorization of the author.                                     }
{**************************************************************************}

unit AdvCurve;

{$I TMSDEFS.INC}

interface

uses
  Windows, Controls, Classes, Contnrs, Math, Graphics, AdvGDIP, SysUtils;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 0; // Build nr.

  // version history
  // v1.0.0.0 : first release

type
  TAdvCustomCurve = class;

  TCurvePoint = class;

  TCurvePointRec = record
    p: TGPPointF;
    ref, refto: TCurvePoint;
  end;

  TCurvePointArray = array of TCurvePointRec;
  TPointArray = array of TGPPointF;
  TTypeArray = array of Byte;
  TArrayOfPointArray = array of TPointArray;
  TArrayOfTypeArray = array of TTypeArray;

  TCurvePoint = class(TCollectionItem)
  private
    FOwner: TAdvCustomCurve;
    FX: Single;
    FXTo: Single;
    FY: Single;
    FYTo: Single;
    procedure SetX(const Value: Single);
    procedure SetXTo(const Value: Single);
    procedure SetY(const Value: Single);
    procedure SetYTo(const Value: Single);
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  published
    property X: Single read FX write SetX;
    property Y: Single read FY write SetY;
    property XTo: Single read FXTo write SetXTo;
    property YTo: Single read FYTo write SetYTo;
  end;

  TCurvePoints = class(TCollection)
  private
    FOwner: TAdvCustomCurve;
    FOnChange: TNotifyEvent;
    function GetItem(Index: Integer): TCurvePoint;
    procedure SetItem(Index: Integer; const Value: TCurvePoint);
  protected
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TAdvCustomCurve);
    property Items[Index: Integer]: TCurvePoint read GetItem write SetItem; default;
    function Add: TCurvePoint;
    function Insert(Index: Integer): TCurvePoint;
    procedure Delete(Index: Integer);
    procedure Clear;
  end;

  TCurvePointSelectedEvent = procedure(Sender: TObject; ACurvePointIndex: Integer; ACurvePoint: TCurvePointRec; APoint: TCurvePoint) of object;
  TCurveGetPathsEvent = procedure(Sender: TObject; APaths: TObjectList) of object;
  TCurveDrawEvent = procedure(Sender: TObject; AGraphics: TGPGraphics) of object;
  TSingleArray = array of Single;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvCustomCurve = class(TGraphicControl)
  private
    FMouseDown: Boolean;
    FBezierCount: Integer;
    FSingleList: TSingleArray;
    FUpdateCount: Integer;
    FX, FY: Single;
    FSelectedPoint: Integer;
    FBezierData: TPointArray;
    FBezierPathPoints: TArrayOfPointArray;
    FBezierPathTypes: TArrayOfTypeArray;
    FBezierLengths: TSingleArray;
    FCurvePoints: TCurvePointArray;
    FDrawCurvePoints: TPointArray;
    FShowLines: Boolean;
    FInteraction: Boolean;
    FShowHandles: Boolean;
    FShowCurve: Boolean;
    FPoints: TCurvePoints;
    FOpacity: Byte;
    FBorderColor: TColor;
    FColor: TColor;
    FShadowOffset: Integer;
    FBorderStyle: TDashStyle;
    FShadow: Boolean;
    FBorderOpacity: Byte;
    FShadowColor: TColor;
    FBorderWidth: Integer;
    FShadowOpacity: Byte;
    FLineColor: TColor;
    FHandleBorderColor: TColor;
    FCurveColor: TColor;
    FHandleColor: TColor;
    FCurveStyle: TDashStyle;
    FCurveOpacity: Byte;
    FHandleOpacity: Byte;
    FCurveWidth: Integer;
    FLineStyle: TDashStyle;
    FLineOpacity: Byte;
    FHandleBorderStyle: TDashStyle;
    FHandleBorderOpacity: Byte;
    FLineWidth: Integer;
    FHandleBorderWidth: Integer;
    FHandleSize: Integer;
    FSelectedHandleBorderWidth: Integer;
    FSelectedHandleOpacity: Byte;
    FSelectedHandleBorderColor: TColor;
    FSelectedHandleColor: TColor;
    FSelectedHandleBorderStyle: TDashStyle;
    FSelectedHandleBorderOpacity: Byte;
    FSelectedHandleSize: Integer;
    FOnCurvePointSelected: TCurvePointSelectedEvent;
    FOnCurveGetPaths: TCurveGetPathsEvent;
    FOnCurveDrawEvent: TCurveDrawEvent;
    FOnCurveBeforeDrawEvent: TCurveDrawEvent;
    FTransparent: Boolean;
    FBackGroundBorderOpacity: Byte;
    FBackGroundBorderWidth: Integer;
    FBackGroundOpacity: Byte;
    FBackGroundBorderColor: TColor;
    FBackGroundColor: TColor;
    FBackGroundBorderStyle: TDashStyle;
    procedure SetInteraction(const Value: Boolean);
    procedure SetShowCurve(const Value: Boolean);
    procedure SetShowHandles(const Value: Boolean);
    procedure SetShowLines(const Value: Boolean);
    procedure SetPoints(const Value: TCurvePoints);
    procedure SetBorderColor(const Value: TColor);
    procedure SetBorderOpacity(const Value: Byte);
    procedure SetBorderStyle(const Value: TDashStyle);
    procedure SetBorderWidth(const Value: Integer);
    procedure SetColor(const Value: TColor);
    procedure SetOpacity(const Value: Byte);
    procedure SetShadow(const Value: Boolean);
    procedure SetShadowColor(const Value: TColor);
    procedure SetShadowOffset(const Value: Integer);
    procedure SetShadowOpacity(const Value: Byte);
    procedure SetCurveColor(const Value: TColor);
    procedure SetHandleBorderColor(const Value: TColor);
    procedure SetHandleColor(const Value: TColor);
    procedure SetLineColor(const Value: TColor);
    procedure SetCurveOpacity(const Value: Byte);
    procedure SetCurveStyle(const Value: TDashStyle);
    procedure SetCurveWidth(const Value: Integer);
    procedure SetHandleBorderStyle(const Value: TDashStyle);
    procedure SetHandleOpacity(const Value: Byte);
    procedure SetLineOpacity(const Value: Byte);
    procedure SetLineStyle(const Value: TDashStyle);
    procedure SetHandleBorderOpacity(const Value: Byte);
    procedure SetHandleBorderWidth(const Value: Integer);
    procedure SetLineWidth(const Value: Integer);
    procedure SetHandleSize(const Value: Integer);
    procedure SetSelectedHandleBorderColor(const Value: TColor);
    procedure SetSelectedHandleBorderOpacity(const Value: Byte);
    procedure SetSelectedHandleBorderStyle(const Value: TDashStyle);
    procedure SetSelectedHandleBorderWidth(const Value: Integer);
    procedure SetSelectedHandleColor(const Value: TColor);
    procedure SetSelectedHandleOpacity(const Value: Byte);
    procedure SetSelectedHandleSize(const Value: Integer);
    procedure SetSelectedPoint(const Value: Integer);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    procedure SetTransparent(const Value: Boolean);
    procedure SetBackGroundBorderColor(const Value: TColor);
    procedure SetBackGroundBorderOpacity(const Value: Byte);
    procedure SetBackGroundBorderStyle(const Value: TDashStyle);
    procedure SetBackGroundBorderWidth(const Value: Integer);
    procedure SetBackGroundColor(const Value: TColor);
    procedure SetBackGroundOpacity(const Value: Byte);
  protected
    procedure Changed;
    procedure FontChanged(Sender: TObject);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
  public
    function GetVersionNr: Integer; virtual;

    procedure AddCurvePoint(APoint: TCurvePoint); virtual;
    procedure UpdatePoint(X, Y, PointIndex: Integer); virtual;
    procedure UpdateReference(APoint: TCurvePointRec); virtual;
    function FindStartPoint(X, Y: Single): Integer; virtual;

    procedure DrawCurve(g: TGPGraphics); virtual;
    procedure DrawControlLines(g: TGPGraphics); virtual;
    procedure DrawControlBezier(g: TGPGraphics); virtual;
    procedure CalculateBezierPoints; virtual;
    procedure BuildPoints; virtual;
    function GetPaths: TObjectList; virtual;
    function CalculateT(t: Single): Single; virtual;
    procedure CalculateSingleList(st: Integer); virtual;
    procedure CalculateBezierLengths; virtual;
    function GetBezierIndex(t: Single): Integer; virtual;
    function GetBezierLength: Single; virtual;
    function GetBezierPoint(st: Integer; t: Single): TGPPointF; virtual;
    function SearchVal(t: Single): Integer; virtual;
    procedure ClearCurvePoints; virtual;
    property CurvePoints: TCurvePointArray read FCurvePoints write FCurvePoints;
    function XYToPoint(X, Y: Integer): Integer; virtual;
    function AddPoint(X, Y, XTo, YTo: Single): TCurvePoint; virtual;
    property BezierLengths: TSingleArray read FBezierLengths;
    property BezierData: TPointArray read FBezierData;
    property BezierPathPoints: TArrayOfPointArray read FBezierPathPoints;
    property BezierPathTypes: TArrayOfTypeArray read FBezierPathTypes;
    function SplitText(AFont: TFont; AText: String): TStringList; virtual;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Paint; override;
    procedure BeginUpdate; virtual;
    procedure EndUpdate; virtual;
    property SelectedPoint: Integer read FSelectedPoint write SetSelectedPoint;

  published
    property BackGroundBorderWidth: Integer read FBackGroundBorderWidth write SetBackGroundBorderWidth default 1;
    property BackGroundColor: TColor read FBackGroundColor write SetBackGroundColor default clWhite;
    property BackGroundBorderColor: TColor read FBackGroundBorderColor write SetBackGroundBorderColor default clSilver;
    property BackGroundBorderStyle: TDashStyle read FBackGroundBorderStyle write SetBackGroundBorderStyle default DashStyleSolid;
    property BackGroundBorderOpacity: Byte read FBackGroundBorderOpacity write SetBackGroundBorderOpacity default 255;
    property BackGroundOpacity: Byte read FBackGroundOpacity write SetBackGroundOpacity default 255;

    property CurveColor: TColor read FCurveColor write SetCurveColor default clBlue;
    property CurveWidth: Integer read FCurveWidth write SetCurveWidth default 1;
    property CurveStyle: TDashStyle read FCurveStyle write SetCurveStyle default DashStyleSolid;
    property CurveOpacity: Byte read FCurveOpacity write SetCurveOpacity default 255;
    property HandleSize: Integer read FHandleSize write SetHandleSize default 10;
    property HandleBorderWidth: Integer read FHandleBorderWidth write SetHandleBorderWidth default 1;
    property HandleColor: TColor read FHandleColor write SetHandleColor default clRed;
    property HandleBorderColor: TColor read FHandleBorderColor write SetHandleBorderColor default clBlack;
    property HandleBorderStyle: TDashStyle read FHandleBorderStyle write SetHandleBorderStyle default DashStyleSolid;
    property HandleBorderOpacity: Byte read FHandleBorderOpacity write SetHandleBorderOpacity default 255;
    property HandleOpacity: Byte read FHandleOpacity write SetHandleOpacity default 50;

    property SelectedHandleSize: Integer read FSelectedHandleSize write SetSelectedHandleSize default 12;
    property SelectedHandleBorderWidth: Integer read FSelectedHandleBorderWidth write SetSelectedHandleBorderWidth default 1;
    property SelectedHandleColor: TColor read FSelectedHandleColor write SetSelectedHandleColor default clBlue;
    property SelectedHandleBorderColor: TColor read FSelectedHandleBorderColor write SetSelectedHandleBorderColor default clBlack;
    property SelectedHandleBorderStyle: TDashStyle read FSelectedHandleBorderStyle write SetSelectedHandleBorderStyle default DashStyleSolid;
    property SelectedHandleBorderOpacity: Byte read FSelectedHandleBorderOpacity write SetSelectedHandleBorderOpacity default 255;
    property SelectedHandleOpacity: Byte read FSelectedHandleOpacity write SetSelectedHandleOpacity default 150;

    property LineColor: TColor read FLineColor write SetLineColor default clRed;
    property LineStyle: TDashStyle read FLineStyle write SetLineStyle default DashStyleSolid;
    property LineOpacity: Byte read FLineOpacity write SetLineOpacity default 255;
    property LineWidth: Integer read FLineWidth write SetLineWidth default 1;

    property Color: TColor read FColor write SetColor default $30CF9F;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clBlack;
    property BorderStyle: TDashStyle read FBorderStyle write SetBorderStyle default DashStyleSolid;
    property BorderWidth: Integer read FBorderWidth write SetBorderWidth default 1;
    property Shadow: Boolean read FShadow write SetShadow default True;
    property ShadowOffset: Integer read FShadowOffset write SetShadowOffset default 3;
    property ShadowColor: TColor read FShadowColor write SetShadowColor default clBlack;
    property ShadowOpacity: Byte read FShadowOpacity write SetShadowOpacity default 255;
    property Opacity: Byte read FOpacity write SetOpacity default 255;
    property BorderOpacity: Byte read FBorderOpacity write SetBorderOpacity default 255;

    property ShowCurve: Boolean read FShowCurve write SetShowCurve default False;
    property ShowHandles: Boolean read FShowHandles write SetShowHandles default False;
    property ShowLines: Boolean read FShowLines write SetShowLines default False;
    property Interaction: Boolean read FInteraction write SetInteraction default True;
    property Points: TCurvePoints read FPoints write SetPoints;
    property Transparent: Boolean read FTransparent write SetTransparent default True;

    property Align;
    property Anchors;
    property Constraints;
    property DragKind;
    property DragCursor;
    property DragMode;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property Height;
    property Width;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property OnEndDock;
    property OnStartDock;

    property OnCurvePointSelected: TCurvePointSelectedEvent read FOnCurvePointSelected write FOnCurvePointSelected;
    property OnCurveGetPaths: TCurveGetPathsEvent read FOnCurveGetPaths write FOnCurveGetPaths;
    property OnCurveBeforeDraw: TCurveDrawEvent read FOnCurveBeforeDrawEvent write FOnCurveBeforeDrawEvent;
    property OnCurveDraw: TCurveDrawEvent read FOnCurveDrawEvent write FOnCurveDrawEvent;
    property Version: string read GetVersion write SetVersion;
  end;

  TAdvCurve = class(TAdvCustomCurve)
  public
    constructor Create(AOwner: TComponent); override;
  end;

function GetFontStyle(AFontStyles: TFontStyles): Integer;

implementation

function GetFontStyle(AFontStyles: TFontStyles): Integer;
begin
  Result := 0;
  if (fsBold in AFontStyles) then
    Result := Result + 1;
  if (fsItalic in AFontStyles) then
    Result := Result + 2;
  if (fsUnderline in AFontStyles) then
    Result := Result + 4;
  if (fsStrikeOut in AFontStyles) then
    Result := Result + 8;
end;

function PtInGPRect(r: TGPRectF; pt: TPoint): Boolean;
begin
  Result := ((pt.X >= r.X) and (pt.X <= r.X + r.Width)) and
     ((pt.Y >= r.Y) and (pt.Y <= r.Y + r.Height));
end;

{ TAdvCustomCurve }

procedure TAdvCustomCurve.AddCurvePoint(APoint: TCurvePoint);
var
  pt, ptc: TGPPointF;
  dx, dy: Single;
  cnt: Integer;
begin
  cnt := Length(FCurvePoints);
  if cnt >= 4 then
  begin
    pt := FCurvePoints[cnt - 2].p;
    ptc := FCurvePoints[cnt - 1].p;

    dx := ptc.X - pt.X;
    dy := ptc.Y - pt.Y;

    SetLength(FCurvePoints, cnt + 1);
    FCurvePoints[cnt].p := MakePointF(ptc.X + dx, ptc.Y + dy);
    FCurvePoints[cnt].ref := nil;

    SetLength(FCurvePoints, cnt + 2);
    FCurvePoints[cnt + 1].p := MakePointF(APoint.X, APoint.Y);
    FCurvePoints[cnt + 1].ref := APoint;
    SetLength(FCurvePoints, cnt + 3);
    FCurvePoints[cnt + 2].p := MakePointF(APoint.XTo, APoint.YTo);
    FCurvePoints[cnt + 2].refto := APoint;
  end
  else
  begin
    SetLength(FCurvePoints, cnt + 1);
    FCurvePoints[cnt].p := MakePointF(APoint.X, APoint.Y);
    FCurvePoints[cnt].ref := APoint;
    SetLength(FCurvePoints, cnt + 2);
    FCurvePoints[cnt + 1].p := MakePointF(APoint.XTo, APoint.YTo);
    FCurvePoints[cnt + 1].refto := APoint;
  end;
end;

function TAdvCustomCurve.AddPoint(X, Y, XTo, YTo: Single): TCurvePoint;
begin
  Result := Points.Add;
  Result.X := X;
  Result.Y := Y;
  Result.XTo := XTo;
  Result.YTo := YTo;
end;

procedure TAdvCustomCurve.Assign(Source: TPersistent);
begin
  if (Source is TAdvCustomCurve) then
  begin
    FBackGroundBorderOpacity := (Source as TAdvCustomCurve).BackGroundBorderOpacity;
    FBackGroundBorderWidth := (Source as TAdvCustomCurve).BackGroundBorderWidth;
    FBackGroundOpacity := (Source as TAdvCustomCurve).BackGroundOpacity;
    FBackGroundBorderColor := (Source as TAdvCustomCurve).BackGroundBorderColor;
    FBackGroundColor := (Source as TAdvCustomCurve).BackGroundColor;
    FBackGroundBorderStyle := (Source as TAdvCustomCurve).BackGroundBorderStyle;
    FTransparent := (Source as TAdvCustomCurve).Transparent;

    FCurveColor := (Source as TAdvCustomCurve).CurveColor;
    FCurveWidth := (Source as TAdvCustomCurve).CurveWidth;
    FCurveStyle := (Source as TAdvCustomCurve).CurveStyle;
    FCurveOpacity := (Source as TAdvCustomCurve).CurveOpacity;
    FHandleBorderWidth := (Source as TAdvCustomCurve).HandleBorderWidth;
    FHandleSize := (Source as TAdvCustomCurve).HandleSize;
    FHandleColor := (Source as TAdvCustomCurve).HandleColor;
    FHandleBorderColor := (Source as TAdvCustomCurve).HandleBorderColor;
    FHandleBorderStyle := (Source as TAdvCustomCurve).HandleBorderStyle;
    FHandleBorderOpacity := (Source as TAdvCustomCurve).HandleBorderOpacity;
    FHandleOpacity := (Source as TAdvCustomCurve).HandleOpacity;
    FSelectedHandleBorderWidth := (Source as TAdvCustomCurve).SelectedHandleBorderWidth;
    FSelectedHandleSize := (Source as TAdvCustomCurve).SelectedHandleSize;
    FSelectedHandleColor := (Source as TAdvCustomCurve).SelectedHandleColor;
    FSelectedHandleBorderColor := (Source as TAdvCustomCurve).SelectedHandleBorderColor;
    FSelectedHandleBorderStyle := (Source as TAdvCustomCurve).SelectedHandleBorderStyle;
    FSelectedHandleBorderOpacity := (Source as TAdvCustomCurve).SelectedHandleBorderOpacity;
    FSelectedHandleOpacity := (Source as TAdvCustomCurve).SelectedHandleOpacity;
    FLineColor := (Source as TAdvCustomCurve).LineColor;
    FLineStyle := (Source as TAdvCustomCurve).LineStyle;
    FLineOpacity := (Source as TAdvCustomCurve).LineOpacity;
    FLineWidth := (Source as TAdvCustomCurve).LineWidth;
    FColor := (Source as TAdvCustomCurve).Color;
    FBorderColor := (Source as TAdvCustomCurve).BorderColor;
    FBorderStyle := (Source as TAdvCustomCurve).BorderStyle;
    FBorderWidth := (Source as TAdvCustomCurve).BorderWidth;
    FShadow := (Source as TAdvCustomCurve).Shadow;
    FShadowOffset := (Source as TAdvCustomCurve).ShadowOffset;
    FShadowColor := (Source as TAdvCustomCurve).ShadowColor;
    FOpacity := (Source as TAdvCustomCurve).Opacity;
    FBorderOpacity := (Source as TAdvCustomCurve).BorderOpacity;
    FShowCurve := (Source as TAdvCustomCurve).ShowCurve;
    FShowHandles := (Source as TAdvCustomCurve).ShowHandles;
    FShowLines := (Source as TAdvCustomCurve).ShowLines;
    FInteraction := (Source as TAdvCustomCurve).Interaction;
    FPoints.Assign((Source as TAdvCustomCurve).Points);
  end;
end;

procedure TAdvCustomCurve.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TAdvCustomCurve.BuildPoints;
var
  I: Integer;
begin
  ClearCurvePoints;
  for I := 0 to Points.Count - 1 do
    AddCurvePoint(Points[I]);

  for I := 0 to Length(FCurvePoints) - 1 do
  begin
    SetLength(FDrawCurvePoints, I + 1);
    FDrawCurvePoints[I] := FCurvePoints[I].p;
  end;
end;

procedure TAdvCustomCurve.CalculateBezierLengths;
var
  I: Integer;
begin
  SetLength(FBezierLengths, 0);
  for I := 0 to FBezierCount - 1 do
  begin
    CalculateSingleList(I * 4);
    SetLength(FBezierLengths, Length(FBezierLengths) + 1);
    FBezierLengths[Length(FBezierLengths) - 1] := GetBezierLength;
  end;
end;

procedure TAdvCustomCurve.CalculateBezierPoints;
var
  p0, p1, p2, p3: TGPPointF;
  cnt: Integer;
  I, J, K, L: Integer;
  t: Single;
  pth: TGPGraphicsPath;
  sx, sy: Single;
  st: Integer;
  pt: TGPPointF;
  pthx, pthy, tx, ty,
  px, py, mag, fx, fy, bzl: Single;
  bzp: TPointArray;
  bzt: TTypeArray;
  pthb: TGPRectF;
  pthlst: TList;
begin
  SetLength(FBezierData, 0);
  SetLength(FBezierPathPoints, 0);
  SetLength(FBezierPathTypes, 0);
  SetLength(FSingleList, 0);
  if (csDestroying in ComponentState) or (Length(FCurvePoints) = 0) then
    Exit;

  cnt := Length(FCurvePoints);
  cnt := cnt div 3;

  SetLength(FBezierData, cnt * 4);
  K := 0;
  J := 0;
  while K < cnt * 3 do
  begin
    L := 0;
    for I := J to J + 3 do
    begin
      if I < Length(FBezierData) then
      begin
        p0 := FCurvePoints[K].p;
        p1 := FCurvePoints[K + 1].p;
        p2 := FCurvePoints[K + 2].p;
        p3 := FCurvePoints[K + 3].p;
        if L = 0 then
        begin
          FBezierData[I].x := p3.x - 3 * p2.x + 3 * p1.x - p0.x;
          FBezierData[I].y := p3.Y - 3 * p2.Y + 3 * p1.Y - p0.Y;
        end
        else if L = 1 then
        begin
          FBezierData[I].x := 3 * p2.X - 6 * p1.X + 3 * p0.X;
          FBezierData[I].y := 3 * p2.Y - 6 * p1.Y + 3 * p0.Y;
        end
        else if L = 2 then
        begin
          FBezierData[I].x := 3 * p1.X - 3 * p0.X;
          FBezierData[I].y := 3 * p1.Y - 3 * p0.Y;
        end
        else if L = 3 then
        begin
          FBezierData[I].x := p0.X;
          FBezierData[I].y := p0.Y;
        end;
      end;
      Inc(L);
    end;
    K := K + 3;
    J := J + 4;
  end;

  FBezierCount := cnt;

  if (Length(FBezierData) > 0) then
  begin
    CalculateBezierLengths;
    pthlst := GetPaths;
    if Assigned(pthlst) then
    begin
      SetLength(FBezierPathPoints, pthlst.Count);
      SetLength(FBezierPathTypes, pthlst.Count);

      for I := 0 to pthlst.Count - 1 do
      begin
        SetLength(bzp, 0);
        SetLength(bzt, 0);
        pth := pthlst[I];
        pth.GetBounds(pthb);
        cnt := pth.GetPointCount;
        SetLength(bzp, cnt);
        SetLength(bzt, cnt);
        pth.GetPathPoints(@bzp[0], cnt);
        pth.GetPathTypes(@bzt[0], cnt);

        st := I * 4;

        CalculateSingleList(st);
        if (I = pthlst.Count - 1) and (I <= Length(FBezierLengths) - 1) then
          bzl := FBezierLengths[I]
        else
          bzl := pthb.Width + pthb.X;

        for K := 0 to Length(bzp) - 1 do
        begin
          pt := bzp[K];
          pthx := pt.X;
          pthy := pt.Y;

          t := 0;
          if bzl > 0 then
            t := pthx / bzl;

          t := CalculateT(t);
          t := Min(1, Max(0, t));

          sx := FBezierData[st].x * Power(t, 3) + FBezierData[st + 1].x * Power(t, 2) + FBezierData[st + 2].x * t + FBezierData[st + 3].x;
          sy := FBezierData[st].y * Power(t, 3) + FBezierData[st + 1].y * Power(t, 2) + FBezierData[st + 2].y * t + FBezierData[st + 3].y;

          // calculate the tangent vector for the point
          tx := 3 * FBezierData[st].x * Power(t, 2) + 2 * FBezierData[st + 1].x * t + FBezierData[st + 2].x;
          ty := 3 * FBezierData[st].y * Power(t, 2) + 2 * FBezierData[st + 1].y * t + FBezierData[st + 2].y;
          px := -ty;
          py := tx;

          mag := sqrt(Power(px, 2) + Power(py, 2));
          if mag <> 0 then
          begin
            px := px / mag;
            py := py / mag;
          end
          else
          begin
            px := 0;
            py := 0;
          end;

          px := px * pthy;
          py := py * pthy;

          fx := px + sx;
          fy := py + sy;

          bzp[K] := MakePoint(fx, fy);
        end;

        FBezierPathPoints[I] := bzp;
        FBezierPathTypes[I] := bzt;
      end;

      pthlst.Free;
    end;
  end;
end;

procedure TAdvCustomCurve.CalculateSingleList(st: Integer);
var
  pp, p: TGPPointF;
  sum: Single;
  I: Integer;
const
  numDiv = 100;
  maxPoint = numdiv + 1;
begin
  SetLength(FSingleList, 0);
  pp := GetBezierPoint(st, 0);
  sum := 0;
  for I := 1 to maxPoint do
  begin
    p := GetBezierPoint(st, (I / maxPoint));
    sum := sum + sqrt(Power(p.X - pp.X, 2) + Power(p.Y - pp.Y, 2));
    SetLength(FSingleList, Length(FSingleList) + 1);
    FSingleList[Length(FSingleList) - 1] := sum;
    pp := p;
  end;
end;

function TAdvCustomCurve.CalculateT(t: Single): Single;
var
  u: Single;
  al: Integer;
  tal: Single;
  lbf, laf, seg, segfr: Single;
  idx: Integer;
begin
  Result := t;
  if Length(FSingleList) > 0 then
  begin
    u := t;
    al := Length(FSingleList) - 1;
    tal := u * FSingleList[al];
    idx := SearchVal(tal);
    if FSingleList[idx] = tal then
      Result := idx / al
    else if (idx >= 0) and (idx < al) then
    begin
      lbf := FSingleList[idx];
      laf := FSingleList[idx+1];
      seg := laf - lbf;
      segfr := (tal - lbf) / seg;
      Result := (idx + segfr) / al;
    end;
  end;
end;

procedure TAdvCustomCurve.Changed;
begin
  if csDestroying in ComponentState then
    Exit;

  if FUpdateCount = 0 then
  begin
    BuildPoints;
    CalculateBezierPoints;
    Invalidate;
  end;
end;

procedure TAdvCustomCurve.ClearCurvePoints;
var
  I: Integer;
begin
  if csDestroying in ComponentState then
    Exit;

  for I := 0 to Length(FCurvePoints) - 1 do
  begin
    FCurvePoints[I].ref := nil;
    FCurvePoints[I].refto := nil;
  end;

  SetLength(FCurvePoints, 0);
  SetLength(FDrawCurvePoints, 0);
end;

constructor TAdvCustomCurve.Create(AOwner: TComponent);
begin
  inherited;

  FPoints := TCurvePoints.Create(Self);

  FBackGroundBorderOpacity := 255;
  FBackGroundBorderWidth := 1;
  FBackGroundOpacity := 255;
  FBackGroundBorderColor := clSilver;
  FBackGroundColor := clWhite;
  FBackGroundBorderStyle := DashStyleSolid;

  FTransparent := True;
  FCurveColor := clBlue;
  FCurveWidth := 1;
  FCurveStyle := DashStyleSolid;
  FCurveOpacity := 255;
  FHandleSize := 10;
  FHandleColor := clRed;
  FHandleBorderStyle := DashStyleSolid;
  FHandleBorderColor := clBlack;
  FHandleBorderOpacity := 255;
  FHandleOpacity := 50;
  FHandleBorderWidth := 1;

  FSelectedHandleSize := 12;
  FSelectedHandleColor := clBlue;
  FSelectedHandleBorderStyle := DashStyleSolid;
  FSelectedHandleBorderColor := clBlack;
  FSelectedHandleBorderOpacity := 255;
  FSelectedHandleOpacity := 150;
  FSelectedHandleBorderWidth := 1;

  FLineColor := clRed;
  FLineStyle := DashStyleSolid;
  FLineOpacity := 255;
  FLineWidth := 1;

  FShowLines := False;
  FShowHandles := False;
  FShowCurve := False;
  FInteraction := True;
  FColor := $30CF9F;
  FBorderColor := clBlack;
  FBorderStyle := DashStyleSolid;
  FBorderWidth := 1;
  FShadow := True;
  FShadowOffset := 3;
  FShadowColor := clBlack;
  FShadowOpacity := 255;
  FOpacity := 255;
  FBorderOpacity := 255;

  Width := 250;
  Height := 250;

  FSelectedPoint := -1;
end;

destructor TAdvCustomCurve.Destroy;
begin
  SetLength(FBezierData, 0);
  SetLength(FBezierPathPoints, 0);
  SetLength(FBezierPathTypes, 0);
  SetLength(FBezierData, 0);
  SetLength(FBezierLengths, 0);
  SetLength(FSingleList, 0);
  FPoints.Free;
  inherited;
end;

procedure TAdvCustomCurve.DrawControlBezier(g: TGPGraphics);
var
  p: TGPPen;
begin
  if not ShowCurve then
    Exit;

  if (CurveColor <> clNone) and (CurveOpacity > 0) and (CurveWidth > 0) then
  begin
    p := TGPPen.Create(MakeColor(CurveOpacity, CurveColor), CurveWidth);
    p.SetDashStyle(CurveStyle);
    g.DrawBeziers(p, @FDrawCurvePoints[0], Length(FDrawCurvePoints));
    p.Free;
  end;
end;

procedure TAdvCustomCurve.DrawControlLines(g: TGPGraphics);
var
  p, pl, psl: TGPPen;
  b, bs: TGPSolidBrush;
  I: Integer;
  r: TGPRectF;
  sz: Integer;
  bt, bl, fb, bsl, fsb: Boolean;
begin
  if not ShowLines and not ShowHandles then
    Exit;

  bt := (LineColor <> clNone) and (LineOpacity > 0) and (LineWidth > 0);
  bl := (HandleBorderColor <> clNone) and (HandleBorderOpacity > 0) and (HandleBorderWidth > 0);
  bsl := (SelectedHandleBorderColor <> clNone) and (SelectedHandleBorderOpacity > 0) and (SelectedHandleBorderWidth > 0);
  fb := (HandleColor <> clNone) and (HandleOpacity > 0);
  fsb := (SelectedHandleColor <> clNone) and (SelectedHandleOpacity > 0);


  p := nil;
  if bt then
  begin
    p := TGPPen.Create(MakeColor(LineOpacity, LineColor), LineWidth);
    p.SetDashStyle(LineStyle);
  end;

  pl := nil;
  if bl then
  begin
    pl := TGPPen.Create(MakeColor(HandleBorderOpacity, HandleBorderColor), HandleBorderWidth);
    pl.SetDashStyle(HandleBorderStyle);
  end;

  psl := nil;
  if bsl then
  begin
    psl := TGPPen.Create(MakeColor(SelectedHandleBorderOpacity, SelectedHandleBorderColor), SelectedHandleBorderWidth);
    psl.SetDashStyle(SelectedHandleBorderStyle);
  end;

  b := nil;
  if fb then
    b := TGPSolidBrush.Create(MakeColor(HandleOpacity, HandleColor));

  bs := nil;
  if fsb then
    bs := TGPSolidBrush.Create(MakeColor(SelectedHandleOpacity, SelectedHandleColor));

  for I := 0 to Length(FCurvePoints) - 1 do
  begin
    if (I > 0) and (((I mod 3) = 0) or (I mod 3 = 1)) and ShowLines and bt then
      g.DrawLine(p, FCurvePoints[I - 1].p.X, FCurvePoints[I - 1].p.Y, FCurvePoints[I].p.X, FCurvePoints[I].p.Y);

    if ShowHandles then
    begin
      if SelectedPoint = I then
        sz := SelectedHandleSize
      else
        sz := HandleSize;

      r := MakeRect(FCurvePoints[I].p.X - sz / 2, FCurvePoints[I].p.Y - sz / 2, sz, sz);
      if SelectedPoint = I then
      begin
        if fsb then
          g.FillRectangle(bs, r);

        if bsl then
          g.DrawRectangle(psl, r);
      end
      else
      begin
        if fb then
          g.FillRectangle(b, r);

        if bl then
          g.DrawRectangle(pl, r);
      end;
    end;
  end;

  if Assigned(b) then
    b.Free;

  if Assigned(p) then
    p.Free;

  if Assigned(pl) then
    pl.Free;

  if Assigned(psl) then
    psl.Free;

  if Assigned(bs) then
    bs.Free;
end;

procedure TAdvCustomCurve.DrawCurve(g: TGPGraphics);
var
  p: TGPPen;
  pth: TGPGraphicsPath;
  I: Integer;
  bzp: TPointArray;
  bzt: TTypeArray;
  pthb, shb: TGPSolidBrush;
  m: TGPMatrix;
begin

  if not Transparent then
  begin
    if (BackGroundColor <> clNone) and (BackGroundOpacity > 0) then
    begin
      pthb := TGPSolidBrush.Create(MakeColor(BackGroundOpacity, BackGroundColor));
      g.FillRectangle(pthb, MakeRect(0, 0, Width - 1, Height - 1));
      pthb.Free;
    end;

    if (BackGroundBorderColor <> clNone) and (BackGroundBorderOpacity > 0) and (BackGroundBorderWidth > 0) then
    begin
      p := TGPPen.Create(MakeColor(BackGroundBorderOpacity, BackGroundBorderColor), BackGroundBorderWidth);
      p.SetDashStyle(BackGroundBorderStyle);
      g.DrawRectangle(p, MakeRect(0, 0, Width - 1, Height - 1));
      p.Free;
    end;
  end;

  for I := 0 to Length(FBezierPathPoints) - 1 do
  begin
    bzp := FBezierPathPoints[I];
    bzt := FBezierPathTypes[I];
    pth := TGPGraphicsPath.Create(@bzp[0], @bzt[0], Length(bzp));

    if Shadow and (ShadowColor <> clNone) and (ShadowOpacity > 0) then
    begin
      m := TGPMatrix.Create;
      m.Translate(ShadowOffset, ShadowOffset);
      g.SetTransform(m);
      shb := TGPSolidBrush.Create(MakeColor(ShadowOpacity, ShadowColor));
      g.FillPath(shb, pth);
      shb.Free;
      g.ResetTransform;
      m.Free;
    end;

    if (Color <> clNone) and (Opacity > 0) then
    begin
      pthb := TGPSolidBrush.Create(MakeColor(Opacity, Color));
      g.FillPath(pthb, pth);
      pthb.Free;
    end;

    if (BorderColor <> clNone) and (BorderOpacity > 0) and (BorderWidth > 0) then
    begin
      p := TGPPen.Create(MakeColor(BorderOpacity, BorderColor), BorderWidth);
      p.SetDashStyle(BorderStyle);
      g.DrawPath(p, pth);
      p.Free;
    end;

    pth.Free;
  end;

  DrawControlBezier(g);
  DrawControlLines(g);
end;

procedure TAdvCustomCurve.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then
    Changed;
end;

function TAdvCustomCurve.FindStartPoint(X, Y: Single): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Length(FCurvePoints) - 1 do
  begin
    if (x > FCurvePoints[I].p.X) and (y > FCurvePoints[I].p.Y) and (I mod 3 = 0) then
    begin
      Result := I;
    end;
  end;
end;

procedure TAdvCustomCurve.FontChanged(Sender: TObject);
begin
  Changed;
end;

function TAdvCustomCurve.GetBezierIndex(t: Single): Integer;
begin
  if FBezierCount > 0 then
    Result := Round(t) - 1
  else
    Result := 0;
end;

function TAdvCustomCurve.GetBezierLength: Single;
begin
  Result := 0;
  if Length(FSingleList) > 0 then
    Result := FSingleList[Length(FSingleList) - 1];
end;

function TAdvCustomCurve.GetBezierPoint(st: Integer; t: Single): TGPPointF;
begin
  Result.X := FBezierData[st].x * Power(t, 3) + FBezierData[st + 1].x * Power(t, 2) + FBezierData[st + 2].x * t + FBezierData[st + 3].x;
  Result.Y := FBezierData[st].y * Power(t, 3) + FBezierData[st + 1].y * Power(t, 2) + FBezierData[st + 2].y * t + FBezierData[st + 3].y;
end;

function TAdvCustomCurve.GetPaths: TObjectList;
begin
  Result := TObjectList.Create;
  if Assigned(OnCurveGetPaths) then
    OnCurveGetPaths(Self, Result);
end;

function TAdvCustomCurve.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function TAdvCustomCurve.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TAdvCustomCurve.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  if not Interaction then
    Exit;

  FMouseDown := True;
  FSelectedPoint := XYToPoint(X, Y);
  Invalidate;
  FX := X;
  FY := Y;
end;

procedure TAdvCustomCurve.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if not Interaction then
    Exit;

  if FMouseDown and (FSelectedPoint >= 0) and (FSelectedPoint <= Length(FCurvePoints)-1) then
  begin
    UpdatePoint(X, Y, FSelectedPoint);
    FX := X;
    FY := Y;
  end;
end;

procedure TAdvCustomCurve.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  it: TCurvePoint;
  pt: TCurvePointRec;
begin
  inherited;
  if not Interaction then
    Exit;

  if FMouseDown and (FSelectedPoint >= 0) and (FSelectedPoint <= Length(FCurvePoints)-1) then
  begin
    if Assigned(OnCurvePointSelected) then
    begin
      pt := FCurvePoints[FSelectedPoint];

      it := nil;
      if Assigned(pt.ref) then
        it := pt.ref
      else if Assigned(pt.refto) then
        it := pt.refto;

      OnCurvePointSelected(Self, FSelectedPoint, pt, it);
    end;
  end;


  FMouseDown := False;
end;

procedure TAdvCustomCurve.Paint;
var
  g: TGPGraphics;
begin
  g := TGPGraphics.Create(Canvas.Handle);
  g.SetSmoothingMode(SmoothingModeAntiAlias);
  if Assigned(OnCurveBeforeDraw) then
    OnCurveBeforeDraw(Self, g);
  DrawCurve(g);
  if Assigned(OnCurveDraw) then
    OnCurveDraw(Self, g);
  g.Free;
end;

function TAdvCustomCurve.SearchVal(t: Single): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Length(FSingleList) - 1 do
  begin
    if FSingleList[I] < t then
      Result := I
    else
      Break;
  end;
end;

procedure TAdvCustomCurve.SetBackGroundBorderColor(const Value: TColor);
begin
  if FBackGroundBorderColor <> Value then
  begin
    FBackGroundBorderColor := Value;
    Changed;
  end;
end;

procedure TAdvCustomCurve.SetBackGroundBorderOpacity(const Value: Byte);
begin
  if FBackGroundBorderOpacity <> Value then
  begin
    FBackGroundBorderOpacity := Value;
    Changed;
  end;
end;

procedure TAdvCustomCurve.SetBackGroundBorderStyle(const Value: TDashStyle);
begin
  if FBackGroundBorderStyle <> Value then
  begin
    FBackGroundBorderStyle := Value;
    Changed;
  end;
end;

procedure TAdvCustomCurve.SetBackGroundBorderWidth(const Value: Integer);
begin
  if FBackGroundBorderWidth <> Value then
  begin
    FBackGroundBorderWidth := Value;
    Changed;
  end;
end;

procedure TAdvCustomCurve.SetBackGroundColor(const Value: TColor);
begin
  if FBackGroundColor <> Value then
  begin
    FBackGroundColor := Value;
    Changed;
  end;
end;

procedure TAdvCustomCurve.SetBackGroundOpacity(const Value: Byte);
begin
  if FBackGroundOpacity <> Value then
  begin
    FBackGroundOpacity := Value;
    Changed;
  end;
end;

procedure TAdvCustomCurve.SetBorderColor(const Value: TColor);
begin
  if FBorderColor <> Value then
  begin
    FBorderColor := Value;
    Changed;
  end;
end;

procedure TAdvCustomCurve.SetBorderOpacity(const Value: Byte);
begin
  if FBorderOpacity <> Value then
  begin
    FBorderOpacity := Value;
    Changed;
  end;
end;

procedure TAdvCustomCurve.SetBorderStyle(const Value: TDashStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    Changed;
  end;
end;

procedure TAdvCustomCurve.SetBorderWidth(const Value: Integer);
begin
  if FBorderWidth <> Value then
  begin
    FBorderWidth := Value;
    Changed;
  end;
end;

procedure TAdvCustomCurve.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Changed;
  end;
end;

procedure TAdvCustomCurve.SetCurveColor(const Value: TColor);
begin
  if FCurveColor <> Value then
  begin
    FCurveColor := Value;
    Changed;
  end;
end;

procedure TAdvCustomCurve.SetCurveOpacity(const Value: Byte);
begin
  if FCurveOpacity <> Value then
  begin
    FCurveOpacity := Value;
    Changed;
  end;
end;

procedure TAdvCustomCurve.SetCurveStyle(const Value: TDashStyle);
begin
  if FCurveStyle <> Value then
  begin
    FCurveStyle := Value;
    Changed;
  end;
end;

procedure TAdvCustomCurve.SetCurveWidth(const Value: Integer);
begin
  if FCurveWidth <> Value then
  begin
    FCurveWidth := Value;
    Changed;
  end;
end;

procedure TAdvCustomCurve.SetHandleBorderColor(const Value: TColor);
begin
  if FHandleBorderColor <> Value then
  begin
    FHandleBorderColor := Value;
    Changed;
  end;
end;

procedure TAdvCustomCurve.SetHandleBorderOpacity(const Value: Byte);
begin
  if FHandleBorderOpacity <> Value then
  begin
    FHandleBorderOpacity := Value;
    Changed;
  end;
end;

procedure TAdvCustomCurve.SetHandleBorderStyle(const Value: TDashStyle);
begin
  if FHandleBorderStyle <> Value then
  begin
    FHandleBorderStyle := Value;
    Changed;
  end;
end;

procedure TAdvCustomCurve.SetHandleBorderWidth(const Value: Integer);
begin
  if FHandleBorderWidth <> Value then
  begin
    FHandleBorderWidth := Value;
    Changed;
  end;
end;

procedure TAdvCustomCurve.SetHandleColor(const Value: TColor);
begin
  if FHandleColor <> Value then
  begin
    FHandleColor := Value;
    Changed;
  end;
end;

procedure TAdvCustomCurve.SetHandleOpacity(const Value: Byte);
begin
  if FHandleOpacity <> Value then
  begin
    FHandleOpacity := Value;
    Changed;
  end;
end;

procedure TAdvCustomCurve.SetHandleSize(const Value: Integer);
begin
  if FHandleSize <> Value then
  begin
    FHandleSize := Value;
    Changed;
  end;
end;

procedure TAdvCustomCurve.SetInteraction(const Value: Boolean);
begin
  if FInteraction <> Value then
  begin
    FInteraction := Value;
    Changed;
  end;
end;

procedure TAdvCustomCurve.SetLineColor(const Value: TColor);
begin
  if FLineColor <> Value then
  begin
    FLineColor := Value;
    Changed;
  end;
end;

procedure TAdvCustomCurve.SetLineOpacity(const Value: Byte);
begin
  if FLineOpacity <> Value then
  begin
    FLineOpacity := Value;
    Changed;
  end;
end;

procedure TAdvCustomCurve.SetLineStyle(const Value: TDashStyle);
begin
  if FLineStyle <> Value then
  begin
    FLineStyle := Value;
    Changed;
  end;
end;

procedure TAdvCustomCurve.SetLineWidth(const Value: Integer);
begin
  if FLineWidth <> Value then
  begin
    FLineWidth := Value;
    Changed;
  end;
end;

procedure TAdvCustomCurve.SetOpacity(const Value: Byte);
begin
  if FOpacity <> Value then
  begin
    FOpacity := Value;
    Changed;
  end;
end;

procedure TAdvCustomCurve.SetPoints(const Value: TCurvePoints);
begin
  if FPoints <> Value then
  begin
    FPoints.Assign(Value);
    Changed;
  end;
end;

procedure TAdvCustomCurve.SetSelectedHandleBorderColor(const Value: TColor);
begin
  if FSelectedHandleBorderColor <> Value then
  begin
    FSelectedHandleBorderColor := Value;
    Changed;
  end;
end;

procedure TAdvCustomCurve.SetSelectedHandleBorderOpacity(const Value: Byte);
begin
  if FSelectedHandleBorderOpacity <> Value then
  begin
    FSelectedHandleBorderOpacity := Value;
    Changed;
  end;
end;

procedure TAdvCustomCurve.SetSelectedHandleBorderStyle(const Value: TDashStyle);
begin
  if FSelectedHandleBorderStyle <> Value then
  begin
    FSelectedHandleBorderStyle := Value;
    Changed;
  end;
end;

procedure TAdvCustomCurve.SetSelectedHandleBorderWidth(const Value: Integer);
begin
  if FSelectedHandleBorderWidth <> Value then
  begin
    FSelectedHandleBorderWidth := Value;
    Changed;
  end;
end;

procedure TAdvCustomCurve.SetSelectedHandleColor(const Value: TColor);
begin
  if FSelectedHandleColor <> Value then
  begin
    FSelectedHandleColor := Value;
    Changed;
  end;
end;

procedure TAdvCustomCurve.SetSelectedHandleOpacity(const Value: Byte);
begin
  if FSelectedHandleOpacity <> Value then
  begin
    FSelectedHandleOpacity := Value;
    Changed;
  end;
end;

procedure TAdvCustomCurve.SetSelectedHandleSize(const Value: Integer);
begin
  if FSelectedHandleSize <> Value then
  begin
    FSelectedHandleSize := Value;
    Changed;
  end;
end;

procedure TAdvCustomCurve.SetSelectedPoint(const Value: Integer);
begin
  if FSelectedPoint <> Value then
  begin
    FSelectedPoint := Value;
    Changed;
  end;
end;

procedure TAdvCustomCurve.SetShadow(const Value: Boolean);
begin
  if FShadow <> Value then
  begin
    FShadow := Value;
    Changed;
  end;
end;

procedure TAdvCustomCurve.SetShadowColor(const Value: TColor);
begin
  if FShadowColor <> Value then
  begin
    FShadowColor := Value;
    Changed;
  end;
end;

procedure TAdvCustomCurve.SetShadowOffset(const Value: Integer);
begin
  if FShadowOffset <> Value then
  begin
    FShadowOffset := Value;
    Changed;
  end;
end;

procedure TAdvCustomCurve.SetShadowOpacity(const Value: Byte);
begin
  if FShadowOpacity <> Value then
  begin
    FShadowOpacity := Value;
    Changed;
  end;
end;

procedure TAdvCustomCurve.SetShowCurve(const Value: Boolean);
begin
  if FShowCurve <> Value then
  begin
    FShowCurve := Value;
    Changed;
  end;
end;

procedure TAdvCustomCurve.SetShowHandles(const Value: Boolean);
begin
  if FShowHandles <> Value then
  begin
    FShowHandles := Value;
    Changed;
  end;
end;

procedure TAdvCustomCurve.SetShowLines(const Value: Boolean);
begin
  if FShowLines <> Value then
  begin
    FShowLines := Value;
    Changed;
  end;
end;

procedure TAdvCustomCurve.SetTransparent(const Value: Boolean);
begin
  if FTransparent <> Value then
  begin
    FTransparent := Value;
    Changed;
  end;
end;

procedure TAdvCustomCurve.SetVersion(const Value: string);
begin

end;

function TAdvCustomCurve.SplitText(AFont: TFont; AText: String): TStringList;
var
  bmp: TBitmap;
  sf: TGPStringFormat;
  sizer: TGPRectF;
  g: TGPGraphics;
  ft: TGPFont;
  sz: Single;
  I, K, J: Integer;
  bzl: Single;
  c: String;
begin
  Result := TStringList.Create;

  if Length(FBezierLengths) = 0 then
    Exit;

  bmp := TBitmap.Create;
  g := TGPGraphics.Create(bmp.Canvas.Handle);
  ft := g.MakeFont(AFont);
  sf := TGPStringFormat.Create;

  K := 0;
  J := 0;
  bzl := FBezierLengths[K];
  for I := 1 to Length(AText) do
  begin
    c := c + AText[I];
    g.MeasureString(c, Length(c), ft, MakePointF(0, 0), sf, sizer);
    sz := sizer.Width;
    if sz > bzl then
    begin
      J := 0;
      Result.Add(c);
      c := '';
      Inc(K);
      if K < Length(FBezierLengths) - 1 then
        bzl := FBezierLengths[K]
      else
      begin
        J := Length(AText) - I;
        Break;
      end;
    end
    else
      Inc(J);
  end;

  Result.Add(Copy(AText, Length(AText) - J + 1, J));

  sf.Free;
  ft.Free;
  g.Free;
  bmp.Free;
end;


procedure TAdvCustomCurve.UpdatePoint(X, Y, PointIndex: Integer);
var
  pt: TGPPointF;
  pi: Integer;
  ptc: TGPPointF;
  dx, dy: Single;
begin
  if (PointIndex >= 0) and (PointIndex <= Length(FCurvePoints) - 1) then
  begin
    pt := FCurvePoints[PointIndex].p;
    pt.X := pt.X + (X - FX);
    pt.Y := pt.Y + (Y - FY);
    FCurvePoints[PointIndex].p := pt;
    UpdateReference(FCurvePoints[PointIndex]);


    pi := PointIndex mod 3;
    if (PointIndex > 1) and ((pi >= 0) or (pi <= 2)) and ((PointIndex + 1) <= Length(FCurvePoints) - 1) then
    begin
      if pi = 0 then
      begin
        if (PointIndex - 1 >= 0) and (PointIndex - 1 <= Length(FCurvePoints) - 1) then
        begin
          pt := FCurvePoints[PointIndex - 1].p;
          pt.X := pt.X + (X - FX);
          pt.Y := pt.Y + (Y - FY);
          FCurvePoints[PointIndex - 1].p := pt;
          UpdateReference(FCurvePoints[PointIndex - 1]);
        end;

        if (PointIndex + 1 >= 0) and (PointIndex + 1 <= Length(FCurvePoints) - 1) then
        begin
          pt := FCurvePoints[PointIndex + 1].p;
          pt.X := pt.X + (X - FX);
          pt.Y := pt.Y + (Y - FY);
          FCurvePoints[PointIndex + 1].p := pt;
          UpdateReference(FCurvePoints[PointIndex + 1]);
        end;
      end
      else if pi = 1 then
      begin
        dx := 0;
        dy := 0;
        if (PointIndex - 1 >= 0) and (PointIndex - 1 <= Length(FCurvePoints) - 1) then
        begin
          ptc := FCurvePoints[PointIndex - 1].p;
          dx := ptc.X - pt.X;
          dy := ptc.Y - pt.Y;
        end;
        if (PointIndex - 2 >= 0) and (PointIndex - 2 <= Length(FCurvePoints) - 1) then
        begin
          FCurvePoints[PointIndex - 2].p := MakePointF(ptc.X + dx, ptc.Y + dy);
          UpdateReference(FCurvePoints[PointIndex - 2]);
        end;
      end
      else if pi = 2 then
      begin
        dx := 0;
        dy := 0;
        if (PointIndex + 1 >= 0) and (PointIndex + 1 <= Length(FCurvePoints) - 1) then
        begin
          ptc := FCurvePoints[PointIndex + 1].p;
          dx := ptc.X - pt.X;
          dy := ptc.Y - pt.Y;
        end;

        if (PointIndex + 2 >= 0) and (PointIndex + 2 <= Length(FCurvePoints) - 1) then
        begin
          FCurvePoints[PointIndex + 2].p := MakePointF(ptc.X + dx, ptc.Y + dy);
          UpdateReference(FCurvePoints[PointIndex + 2]);
        end;
      end;
    end;

    Changed;
  end;
end;

procedure TAdvCustomCurve.UpdateReference(APoint: TCurvePointRec);
begin
  if Assigned(APoint.ref) then
  begin
    APoint.ref.FX := APoint.p.X;
    APoint.ref.FY := APoint.p.Y;
  end
  else if Assigned(APoint.refto) then
  begin
    APoint.refto.FXTo := APoint.p.X;
    APoint.refto.FYTo := APoint.p.Y;
  end;
end;

function TAdvCustomCurve.XYToPoint(X, Y: Integer): Integer;
var
  I: Integer;
  r: TGPRectF;
  sz: Integer;
begin
  Result := -1;
  for I := 0 to Length(FCurvePoints) - 1 do
  begin
    sz := HandleSize;
    r := MakeRect(FCurvePoints[I].p.X - sz / 2, FCurvePoints[I].p.Y - sz / 2, sz, sz);
    if PtInGPRect(r, Point(X, Y)) and ShowHandles then
    begin
      Result := I;
      Break;
    end;
  end;
end;

{ TCurvePoint }

procedure TCurvePoint.Assign(Source: TPersistent);
begin
  if Source is TCurvePoint then
  begin
    FX := (Source as TCurvePoint).X;
    FY := (Source as TCurvePoint).Y;
    FXTo := (Source as TCurvePoint).XTo;
    FYTo := (Source as TCurvePoint).YTo;
  end;
end;

constructor TCurvePoint.Create(Collection: TCollection);
begin
  inherited;
  FOwner := (Collection as TCurvePoints).FOwner;
  FOwner.Changed;
end;

destructor TCurvePoint.Destroy;
begin
  inherited;
  if not (csDestroying in FOwner.ComponentState) then
    FOwner.Changed;
end;

procedure TCurvePoint.SetX(const Value: Single);
begin
  if FX <> Value then
  begin
    FX := Value;
    FOwner.Changed;
  end;
end;

procedure TCurvePoint.SetXTo(const Value: Single);
begin
  if FXTo <> Value then
  begin
    FXTo := Value;
    FOwner.Changed;
  end;
end;

procedure TCurvePoint.SetY(const Value: Single);
begin
  if FY <> Value then
  begin
    FY := Value;
    FOwner.Changed;
  end;
end;

procedure TCurvePoint.SetYTo(const Value: Single);
begin
  if FYTo <> Value then
  begin
    FYTo := Value;
    FOwner.Changed;
  end;
end;

{ TCurvePoints }

function TCurvePoints.Add: TCurvePoint;
begin
  result := TCurvePoint( inherited Add);
end;

procedure TCurvePoints.Clear;
begin
  if Count > 0 then
  begin
    while Count > 0 do
      TCollectionItem(Items[Count - 1]).Free;
  end;
end;

constructor TCurvePoints.Create(AOwner: TAdvCustomCurve);
begin
  inherited Create(TCurvePoint);
  FOwner := AOwner;
end;

procedure TCurvePoints.Delete(Index: Integer);
begin
  Items[Index].Free;
end;

function TCurvePoints.GetItem(Index: Integer): TCurvePoint;
begin
  result := TCurvePoint( inherited Items[Index]);
end;

function TCurvePoints.GetOwner: TPersistent;
begin
  result := FOwner;
end;

function TCurvePoints.Insert(Index: Integer): TCurvePoint;
begin
  result := TCurvePoint( inherited Insert(Index));
end;

procedure TCurvePoints.SetItem(Index: Integer; const Value: TCurvePoint);
begin
  inherited Items[Index] := Value;
end;


{ TAdvCurve }

constructor TAdvCurve.Create(AOwner: TComponent);
begin
  inherited;
  ShowLines := True;
  ShowCurve := True;
  ShowHandles := True;
end;

end.
