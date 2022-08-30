{********************************************************************}
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 2015                                        }
{            Email : info@tmssoftware.com                            }
{            Web : http://www.tmssoftware.com                        }
{                                                                    }
{ The source code is given as is. The author is not responsible      }
{ for any possible damage done due to the use of this code.          }
{ The complete source code remains property of the author and may    }
{ not be distributed, published, given or sold in any form as such.  }
{ No parts of the source code can be included in any other component }
{ or application without written authorization of the author.        }
{********************************************************************}

unit AdvTreeViewBase;

{$I TMSDEFS.INC}

{$IFDEF LCLLIB}
{$mode objfpc}{$H+}{$modeswitch advancedrecords}
{$ENDIF}

interface

{$IFDEF VCLLIB}
{$IFDEF DELPHIXE2_LVL}
{$IFNDEF LCLLIB}
{$DEFINE USEUITYPES}
{$ENDIF}
{$ELSE}
{$DEFINE USEOLDERVCL}
{$ENDIF}
{$ENDIF}

{$IFDEF FMXLIB}
{$DEFINE USEUITYPES}
{$ENDIF}

uses
  {$IFDEF VCLLIB}
  Windows,
  {$ENDIF}
  Classes, Types{$IFNDEF LCLLIB}, Generics.Defaults{$ENDIF}
  {$IFDEF USEUITYPES}
  , UITypes
  {$ENDIF}
  {$IFDEF FMXLIB}
  ,AdvBaseControl, FMX.StdCtrls, FMX.Graphics, UIConsts
  {$ENDIF}
  {$IFDEF VCLLIB}
  ,Controls, StdCtrls, Graphics
  {$ENDIF}
  ;

const
  {$IFDEF MSWINDOWS}
  SWIPECOUNT = 300;
  DOWNCOUNT = 15;
  {$ENDIF}
  {$IFDEF MACOS}
  {$IFDEF IOS}
  SWIPECOUNT = 300;
  DOWNCOUNT = 200;
  {$ELSE}
  SWIPECOUNT = 300;
  DOWNCOUNT = 200;
  {$ENDIF}
  {$ENDIF}
  {$IFDEF ANDROID}
  SWIPECOUNT = 300;
  DOWNCOUNT = 100;
  {$ENDIF}

  {$IFDEF FMXLIB}
  TMSTreeViewPlatformsDesktop = pidWin32 or pidWin64 or pidOSX32;
  TMSTreeViewPlatformsMobile = pidiOSSimulator or {$IFDEF DELPHIXE8_LVL}pidiOSDevice32 or pidiOSDevice64{$ELSE}pidiOSDevice{$ENDIF} or pidAndroid;
  TAdvTreeViewColorGray = claGray;
  TAdvTreeViewColorDarkGray = claDarkGray;
  TAdvTreeViewColorSilver = claSilver;
  TAdvTreeViewColorExtended = $FFF4F4F4;
  TAdvTreeViewColorNull = claNull;
  TAdvTreeViewColorSelection = $FF1BADF8;
  TAdvTreeViewColorWhite = claWhite;
  {$ELSE}
  TAdvTreeViewColorGray = clGray;
  TAdvTreeViewColorDarkGray = $A9A9A9;
  TAdvTreeViewColorExtended = $F4F4F4;
  TAdvTreeViewColorSilver = clSilver;
  TAdvTreeViewColorNull = clNone;
  TAdvTreeViewColorSelection = $F8AD1B;
  TAdvTreeViewColorWhite = clWhite;
  {$ENDIF}

type
  {$IFDEF FMXLIB}
  TAdvTreeViewScrollBar = class(TScrollBar);
  TAdvTreeViewColor = TAlphaColor;
  TAdvTreeViewBitmap = TBitmap;
  TAdvTreeViewIconBitmap = TBitmap;
  TAdvTreeViewBrushKind = (tvbkNone, tvbkSolid, tvbkGradient, tvbkBitmap, tvbkResource);
  {$ENDIF}
  {$IFDEF VCLLIB}
  TAdvTreeViewColor = TColor;
  TAdvTreeViewBitmap = TPicture;
  TAdvTreeViewIconBitmap = TGraphic;
  TAdvTreeViewBrushKind = (tvbkNone, tvbkSolid, tvbkGradient);
  {$ENDIF}

  TAdvTreeViewStrokeBrush = class;
  TAdvTreeViewBrush = class;

  {$IFDEF FMXLIB}
  TAdvTreeViewCanvasSaveState = TCanvasSaveState;
  {$ENDIF}
  {$IFDEF VCLLIB}
  TAdvTreeViewCanvasSaveState = class(TPersistent)
  private
    FStroke: TAdvTreeViewStrokeBrush;
    FFill: TAdvTreeViewBrush;
    FSaveDC: Integer;
    procedure SetStroke(const Value: TAdvTreeViewStrokeBrush);
    procedure SetFill(const Value: TAdvTreeViewBrush);
  public
    constructor Create;
    destructor Destroy; override;
    property Stroke: TAdvTreeViewStrokeBrush read FStroke write SetStroke;
    property Fill: TAdvTreeViewBrush read FFill write SetFill;
    property SaveDC: Integer read FSaveDC write FSaveDC;
  end;

  TAdvTreeViewGradient = class(TPersistent)
  private
    FColor: TAdvTreeViewColor;
    FColorTo: TAdvTreeViewColor;
    FOnChanged: TNotifyEvent;
    procedure SetColor(const Value: TColor);
    procedure SetColorTo(const Value: TColor);
  public
    constructor Create;
  published
    property Color: TAdvTreeViewColor read FColor write SetColor default TAdvTreeViewColorWhite;
    property ColorTo: TAdvTreeViewColor read FColorTo write SetColorTo default TAdvTreeViewColorSilver;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  end;
  {$ENDIF}

  TAdvTreeViewBrush = class(TBrush)
  {$IFDEF VCLLIB}
  private
    FOnChanged: TNotifyEvent;
    FGradient: TAdvTreeViewGradient;
    FKind: TAdvTreeViewBrushKind;
    procedure SetGradient(const Value: TAdvTreeViewGradient);
    procedure SetKind(const Value: TAdvTreeViewBrushKind);
  protected
    procedure GradientChanged(Sender: TObject);
    procedure Change(Sender: TObject);
  {$ENDIF}
  public
    constructor CreateBrush(AKind: TAdvTreeViewBrushKind; AColor: TAdvTreeViewColor);
    destructor Destroy; override;
  published
    {$IFDEF VCLLIB}
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    property Gradient: TAdvTreeViewGradient read FGradient write SetGradient;
    property Kind: TAdvTreeViewBrushKind read FKind write SetKind default tvbkSolid;
    {$ENDIF}
  end;

  {$IFDEF FMXLIB}
  TAdvTreeViewStrokeBrush = class(TStrokeBrush)
  {$ENDIF}
  {$IFDEF VCLLIB}
  TAdvTreeViewStrokeBrush = class(TPen)
  private
    FOnChanged: TNotifyEvent;
  protected
    procedure Change(Sender: TObject);
  {$ENDIF}
  public
    constructor CreateBrush(AKind: TAdvTreeViewBrushKind; AColor: TAdvTreeViewColor);
    {$IFDEF VCLLIB}
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    {$ENDIF}
  end;

  {$IFDEF FMXLIB}
  TAdvTreeViewBaseControl = class(TAdvBaseControl);
  {$ENDIF}
  {$IFDEF VCLLIB}
  {$IFNDEF DELPHIXE2_LVL}
  TPointFType = array [0..1] of Single;

  PPointF = ^TPointF;
  TPointF = record
    case Integer of
      0: (V: TPointFType;);
      1: (X: Single;
          Y: Single;);
  end;

  PRectF = ^TRectF;
  TRectF = record
  private
    function GetWidth: Single;
    procedure SetWidth(const Value: Single);
    function GetHeight: Single;
    procedure SetHeight(const Value: Single);
  public
    {$IFNDEF LCLLIB}
    constructor Create(const Left, Top, Right, Bottom: Single); overload;
    {$ENDIF}
    function IsEmpty: Boolean;
    function IntersectsWith(const R: TRectF): Boolean;
    property Width: Single read GetWidth write SetWidth;
    property Height: Single read GetHeight write SetHeight;
  case Integer of
    0: (Left, Top, Right, Bottom: Single);
    1: (TopLeft, BottomRight: TPointF);
  end;
  {$ENDIF}

  TAdvTreeViewBaseControl = class(TCustomControl)
  private
    FBorderColor: TAdvTreeViewColor;
    procedure SetBorderColor(const Value: TAdvTreeViewColor);
  public
    procedure BeginUpdate; virtual;
    procedure EndUpdate; virtual;
    function LocalRect: TRectF; virtual;
    function GetDrawRect: TRectF; virtual;
    procedure Paint; override;
    constructor Create(AOwner: TComponent); override;
  published
    property BorderColor: TAdvTreeViewColor read FBorderColor write SetBorderColor default TAdvTreeViewColorDarkGray;
  end;
  {$ENDIF}

  TAdvTreeViewBase = class;

  TAdvTreeViewDoubleListItem = class(TCollectionItem)
  private
    FOwner: TAdvTreeViewBase;
    FValue: Double;
    FCellVal: Integer;
    procedure SetCellData(const Value: Double);
  public
    constructor Create(Collection: TCollection); override;
  published
    property Value: Double read FValue write SeTCellData;
    property CellVal: Integer read FCellVal write FCellVal;
  end;

  TAdvTreeViewDoubleList = class(TCollection)
  private
    FOwner: TAdvTreeViewBase;
    FOnChange: TNotifyEvent;
    function GetItem(Index: Integer): TAdvTreeViewDoubleListItem;
    procedure SetItem(Index: Integer; const Value: TAdvTreeViewDoubleListItem);
  protected
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    function GetOwner: TPersistent; override;
  public
    function IndexOf(ACellVal: Integer): Integer;
    constructor Create(AOwner: TAdvTreeViewBase);
    property Items[Index: Integer]: TAdvTreeViewDoubleListItem read GetItem write SetItem; default;
    function Add: TAdvTreeViewDoubleListItem;
    function Insert(Index: Integer): TAdvTreeViewDoubleListItem;
  end;

  TAdvTreeViewScrollMode = (smPixelScrolling, smNodeScrolling);

  TAdvTreeViewBase = class(TAdvTreeViewBaseControl)
  private
    FDesignTime: Boolean;
    FSaveHScrollPos, FSaveVScrollPos: Single;
    FTotalRowHeight: Double;
    FNodeListBuild: Boolean;
    FTotalColumnWidth: Double;
    FStartOffset: Double;
    FStartCol, FStopCol, FStartRow, FStopRow: Integer;
    FStartX, FStopX, FStartY, FStopY: Double;
    FBlockScrollingUpdate: Boolean;
    FUpdateCount: Integer;
    FVerticalScrollBar, FHorizontalScrollBar: TScrollBar;
    FRowCount: Integer;
    FColumnCount: Integer;
    FColumnW, FColumnP: TAdvTreeViewDoubleList;
    FDefaultRowHeight: Double;
    FDefaultColumnWidth: Double;
    FVerticalScrollBarVisible: Boolean;
    FHorizontalScrollBarVisible: Boolean;
    FScrollMode: TAdvTreeViewScrollMode;
    FIsMouseDown: Boolean;
    FDelayedLoading: Boolean;
    FStretchScrollBars: Boolean;
    FBlockUpdateNodeList: Boolean;
    FBlockUpdateNode: Boolean;
    FBlockRemoveNode: Integer;
    procedure SetColumnCount(const Value: Integer);
    procedure SetRowCount(const Value: Integer);
    procedure SetDefaultColumnWidth(const Value: Double);
    procedure SetDefaultRowHeight(const Value: Double);
    procedure SetHorizontalScrollBarVisible(const Value: Boolean);
    procedure SetVerticalScrollBarVisible(const Value: Boolean);
    procedure SetScrollMode(const Value: TAdvTreeViewScrollMode);
    procedure SetColWidths(Col: Integer; const Value: Double);
    procedure SetColumnP(const Value: TAdvTreeViewDoubleList);
    procedure SetColumnW(const Value: TAdvTreeViewDoubleList);
    function GetColWidths(Col: integer): Double;
    function GetColPos(Col: integer): Double;
    procedure SetColPos(Col: Integer; const Value: Double);
    procedure SetStretchScrollBars(const Value: Boolean);
  protected
    function IsDesignTime: Boolean; virtual;
    function GetVersionNumber(AMaj, AMin, ARel, ABld: ShortInt): string; virtual;
    procedure UpdateTreeView; virtual;
    procedure UpdateColumns; virtual; abstract;
    procedure AutoSizeColumnInternal(ACol: Integer; AUpdate: Boolean = False; ACallEventHandlers: Boolean = False); virtual; abstract;
    procedure UpdateVisualRange; virtual;
    procedure StretchColumn(AStretchAll: Boolean = True; ACol: Integer = -1; ANewWidth: Double = -1); virtual;
    procedure StopAnimationTimer; virtual; abstract;
    procedure Scroll(AHorizontalPos, AVerticalPos: Double);
    procedure UpdateDisplay; virtual;
    procedure VerticalScrollPositionChanged; virtual; abstract;
    procedure HorizontalScrollPositionChanged; virtual; abstract;
    procedure VScrollChanged(Sender: TObject);
    procedure HScrollChanged(Sender: TObject);
    procedure UpdateAutoSizing; virtual; abstract;
    procedure UpdateColumnRowCalculations(AUpdateTotalRowHeight: Boolean = True); virtual; abstract;
    procedure UpdateScrollBars(AUpdate: Boolean = True; ACalculate: Boolean = True);
    procedure UpdateCalculations; virtual; abstract;
    procedure ResetNodes(AUpdateAll: Boolean = True); virtual; abstract;
    procedure UpdateColumnsCache; virtual; abstract;
    procedure UpdateGroupsCache; virtual; abstract;
    procedure UpdateNodesCache(AUpdateNodes: Boolean = True; AResetNodes: Boolean = False); virtual; abstract;
    procedure UpdateTreeViewCache; virtual;
    procedure UpdateTreeViewDisplay; virtual;
    procedure Loaded; override;
    procedure SetHScrollValue(AValue: Single); virtual;
    procedure SetVScrollValue(AValue: Single); virtual;
    function IsColumnVisible(ACol: Integer): Boolean; virtual;
    function GetRowHeight(ARow: Integer): Double; virtual;
    function GetCalculationRect: TRectF; virtual;
    function GetContentRect: TRectF; virtual;
    function GetContentClipRect: TRectF; virtual;
    function GetHScrollValue: Single; virtual;
    function GetVScrollValue: Single; virtual;
    function GetVViewPortSize: Single; virtual;
    function GetHViewPortSize: Single; virtual;
    function GetColumnViewPortSize: Double; virtual;
    function GetRowViewPortSize: Double; virtual;
    property TotalColumnWidth: Double read FTotalColumnWidth write FTotalColumnWidth;
    property TotalRowHeight: Double read FTotalRowHeight write FTotalRowHeight;
    property IsMouseDown: Boolean read FIsMouseDown write FIsMouseDown;
    property DefaultRowHeight: Double read FDefaultRowHeight write SetDefaultRowHeight;
    property DefaultColumnWidth: Double read FDefaultColumnWidth write SetDefaultColumnWidth;
    property HorizontalScrollBarVisible: Boolean read FHorizontalScrollBarVisible write SetHorizontalScrollBarVisible default True;
    property VerticalScrollBarVisible: Boolean read FVerticalScrollBarVisible write SetVerticalScrollBarVisible default True;
    property RowCount: Integer read FRowCount write SetRowCount;
    property ColumnCount: Integer read FColumnCount write SetColumnCount;
    property ScrollMode: TAdvTreeViewScrollMode read FScrollMode write SetScrollMode default smPixelScrolling;
    property ColumnPositions[Col: Integer]: Double read GetColPos write SetColPos;
    property ColumnWidths[Col: Integer]: Double read GetColWidths write SetColWidths;
    property ColumnW: TAdvTreeViewDoubleList read FColumnW write SetColumnW;
    property ColumnP: TAdvTreeViewDoubleList read FColumnP write SetColumnP;
    property UpdateCount: Integer read FUpdateCount write FUpdateCount;
    property BlockScrollingUpdate: Boolean read FBlockScrollingUpdate write FBlockScrollingUpdate;
    property StretchScrollBars: Boolean read FStretchScrollBars write SetStretchScrollBars default True;
    property BlockUpdateNodeList: Boolean read FBlockUpdateNodeList write FBlockUpdateNodeList;
    property BlockUpdateNode: Boolean read FBlockUpdateNode write FBlockUpdateNode;
    property NodeListBuild: Boolean read FNodeListBuild write FNodeListBuild;
    property BlockRemoveNode: Integer read FBlockRemoveNode write FBlockRemoveNode;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Resize; override;
    procedure BeginUpdate(AClearNodeList: Boolean = False); reintroduce; virtual;
    procedure EndUpdate; override;
    procedure SaveScrollPosition; virtual;
    procedure RestoreScrollPosition; virtual;
    procedure AutoSizeColumn(ACol: Integer); virtual;
    function GetVerticalScrollPosition: Double; virtual;
    function GetHorizontalScrollPosition: Double; virtual;
    function HorizontalScrollBar: TScrollBar;
    function VerticalScrollBar: TScrollBar;
    function StartCol: Integer;
    function StopCol: Integer;
    function StartX: Double;
    function StopX: Double;
    function StartRow: Integer;
    function StartOffset: Double;
    function StopRow: Integer;
    function StartY: Double;
    function StopY: Double;
    function GetTotalColumnWidth: Double;
    function GetTotalRowHeight: Double;
  end;

function OffsetRectEx(var R: TRectF; DX, DY: Single): Boolean;
procedure InflateRectEx(var R: TRectF; DX, DY: Single);
function PtInRectEx(const Rect: TRectF; const P: TPointF): Boolean;
{$IFDEF USEOLDERVCL}
function RectF(Left, Top, Right, Bottom: Single): TRectF; inline; overload;
function PointF(X, Y: Single): TPointF; inline; overload;
{$ENDIF}

implementation

uses
  {$IFDEF FMXLIB}
  FMX.Types, FMX.Controls,
  {$ENDIF}
  {$IFDEF VCLLIB}
  Forms,
  {$ENDIF}
  Math, SysUtils, {$IFNDEF LCLLIB}Generics.Collections, {$ENDIF}RTLConsts;

{$IFDEF USEOLDERVCL}
function PtInRectEx(const Rect: TRectF; const P: TPointF): Boolean;
begin
  Result := (P.X >= Rect.Left) and (P.X < Rect.Right) and (P.Y >= Rect.Top)
    and (P.Y < Rect.Bottom);
end;

function PointF(X, Y: Single): TPointF;
begin
  Result.X := X;
  Result.Y := Y;
end;

function OffsetRectEx(var R: TRectF; DX, DY: Single): Boolean;
begin
  if @R <> nil then
  begin
    R.Left := R.Left + DX;
    R.Right := R.Right + DX;
    R.Top := R.Top + DY;
    R.Bottom := R.Bottom + DY;
    Result := True;
  end
  else
    Result := False;
end;

function RectF(Left, Top, Right, Bottom: Single): TRectF;
begin
  Result.Left := Left;
  Result.Top := Top;
  Result.Bottom := Bottom;
  Result.Right := Right;
end;

procedure InflateRectEx(var R: TRectF; DX, DY: Single);
begin
  R.Left := R.Left - DX;
  R.Right := R.Right + DX;
  R.Top := R.Top - DY;
  R.Bottom := R.Bottom + DY;
end;
{$ELSE}

function OffsetRectEx(var R: TRectF; DX, DY: Single): Boolean;
begin
  Result := OffsetRect(R, DX, DY);
end;

procedure InflateRectEx(var R: TRectF; DX, DY: Single);
begin
  InflateRect(R, DX, DY);
end;

function PtInRectEx(const Rect: TRectF; const P: TPointF): Boolean;
begin
  Result := PtInRect(Rect, P);
end;

{$ENDIF}

{ TAdvTreeViewBase }

procedure TAdvTreeViewBase.Assign(Source: TPersistent);
begin
  if Source is TAdvTreeViewBase then
  begin
    FScrollMode := (Source as TAdvTreeViewBase).ScrollMode;
    FStretchScrollBars := (Source as TAdvTreeViewBase).StretchScrollBars;
  end;
end;

procedure TAdvTreeViewBase.RestoreScrollPosition;
begin
  Scroll(FSaveHScrollPos, FSaveVScrollPos);
end;

procedure TAdvTreeViewBase.SaveScrollPosition;
begin
  FSaveHScrollPos := GetHScrollValue;
  FSaveVScrollPos := GetVScrollValue;
end;

procedure TAdvTreeViewBase.BeginUpdate(AClearNodeList: Boolean);
begin
  inherited BeginUpdate;
  Inc(FUpdateCount);
  if AClearNodeList then
    NodeListBuild := False;
end;

function TAdvTreeViewBase.IsDesignTime: Boolean;
begin
  Result := FDesignTime;
end;

constructor TAdvTreeViewBase.Create(AOwner: TComponent);
begin
  inherited;
  FDesignTime := (csDesigning in ComponentState) and not ((csReading in Owner.ComponentState) or (csLoading in Owner.ComponentState));

  {$IFDEF FMXLIB}
  CanFocus := True;
  ClipChildren := True;
  {$ENDIF}
  FDelayedLoading := False;

  FStretchScrollBars := True;

  FVerticalScrollBar := TScrollBar.Create(Self);
  FVerticalScrollBar.Parent := Self;
  FHorizontalScrollBar := TScrollBar.Create(Self);
  FHorizontalScrollBar.Parent := Self;

  {$IFDEF FMXLIB}
  FVerticalScrollBar.Stored := False;
  FHorizontalScrollBar.Stored := False;
  FVerticalScrollBar.Orientation := TOrientation.Vertical;
  FVerticalScrollBar.Align := TAlignLayout.MostRight;
  FHorizontalScrollBar.Align := TAlignLayout.MostBottom;
  {$ENDIF}

  {$IFDEF VCLLIB}
  {$IFNDEF LCLLIB}
  FVerticalScrollBar.AlignWithMargins := True;
  FHorizontalScrollBar.AlignWithMargins := True;
  FVerticalScrollBar.Ctl3D := False;
  FHorizontalScrollBar.Ctl3D := False;
  {$ENDIF}
  FVerticalScrollBar.DoubleBuffered := False;
  FHorizontalScrollBar.DoubleBuffered := False;
  FVerticalScrollBar.Kind := sbVertical;
  FVerticalScrollBar.Align := alRight;
  FHorizontalScrollBar.Align := alBottom;
  {$ENDIF}

  FVerticalScrollBar.Visible := True;
  FHorizontalScrollBar.Visible := True;
  FVerticalScrollBar.OnChange := VScrollChanged;
  FHorizontalScrollBar.OnChange := HScrollChanged;

  FScrollMode := smPixelScrolling;

  FColumnW := TAdvTreeViewDoubleList.Create(Self);
  FColumnP := TAdvTreeViewDoubleList.Create(Self);

  FDefaultRowHeight := 25;
  FDefaultColumnWidth := 75;
  FColumnCount := 0;
  FRowCount := 0;
  FVerticalScrollBarVisible := True;
  FHorizontalScrollBarVisible := True;
end;

destructor TAdvTreeViewBase.Destroy;
begin
  FColumnP.Free;
  FColumnW.Free;
  FVerticalScrollBar.Free;
  FHorizontalScrollBar.Free;
  inherited;
end;

function TAdvTreeViewBase.GetContentRect: TRectF;
begin
  Result := LocalRect;
  if HorizontalScrollBar.Visible then
    Result.Bottom := Result.Bottom - HorizontalScrollBar.Height - 1;

  if VerticalScrollBar.Visible then
    Result.Right := Result.Right - VerticalScrollBar.Width - 1;
end;

function TAdvTreeViewBase.GetContentClipRect: TRectF;
begin
  Result := GetContentRect;
end;

function TAdvTreeViewBase.GetCalculationRect: TRectF;
begin
  Result := LocalRect;
end;

function TAdvTreeViewBase.GetHorizontalScrollPosition: Double;
var
  hVal, scrollh: Double;
begin
  hVal := GetHScrollValue;
  if ScrollMode = smNodeScrolling then
  begin
    scrollh := ColumnPositions[Round(hval)];
    hVal := scrollh;
  end;

  Result := hVal;
end;

function TAdvTreeViewBase.GetHScrollValue: Single;
begin
  {$IFDEF FMXLIB}
  Result := HorizontalScrollBar.Value;
  {$ENDIF}
  {$IFDEF VCLLIB}
  Result := HorizontalScrollBar.Position;
  {$ENDIF}
end;

function TAdvTreeViewBase.GetHViewPortSize: Single;
begin
  {$IFDEF FMXLIB}
  Result := HorizontalScrollBar.ViewportSize;
  {$ENDIF}
  {$IFDEF VCLLIB}
  Result := HorizontalScrollBar.PageSize;
  {$ENDIF}
end;

function TAdvTreeViewBase.GetRowHeight(ARow: Integer): Double;
begin
  Result := 0;
end;

function TAdvTreeViewBase.GetRowViewPortSize: Double;
var
  I,cnt: Integer;
  h: Double;
  ch: Double;
begin
  ch := GetContentRect.Height;
  Result := 0;
  h := 0;
  cnt := 0;
  for I := RowCount - 1 downto 0 do
  begin
    h := h + GetRowHeight(I);
    if h > ch then
    begin
      Result := Max(1, cnt);
      Break;
    end;
    Inc(cnt);
  end;
end;

function TAdvTreeViewBase.GetTotalColumnWidth: Double;
begin
  Result := TotalColumnWidth;
end;

function TAdvTreeViewBase.GetTotalRowHeight: Double;
begin
  Result := TotalRowHeight;
end;

function TAdvTreeViewBase.GetVersionNumber(AMaj, AMin, ARel,
  ABld: ShortInt): string;
var
  vn: Integer;
begin
  vn := MakeLong(MakeWord(ABld, ARel),MakeWord(AMin, AMaj));
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function TAdvTreeViewBase.GetVerticalScrollPosition: Double;
var
  vVal: Double;
begin
  vVal := GetVScrollValue;
  if ScrollMode = smNodeScrolling then
  begin
//    scrollv := RowPositions[Round(vval)];
//    vVal := scrollv;
  end;

  Result := vVal;
end;

function TAdvTreeViewBase.GetVViewPortSize: Single;
begin
  {$IFDEF FMXLIB}
  Result := VerticalScrollBar.ViewportSize;
  {$ENDIF}
  {$IFDEF VCLLIB}
  Result := VerticalScrollBar.PageSize;
  {$ENDIF}
end;

function TAdvTreeViewBase.GetVScrollValue: Single;
begin
  {$IFDEF FMXLIB}
  Result := VerticalScrollBar.Value;
  {$ENDIF}
  {$IFDEF VCLLIB}
  Result := VerticalScrollBar.Position;
  {$ENDIF}
end;

function TAdvTreeViewBase.HorizontalScrollBar: TScrollBar;
begin
  Result := FHorizontalScrollBar;
end;

procedure TAdvTreeViewBase.HScrollChanged(Sender: TObject);
begin
  if FBlockScrollingUpdate then
    Exit;

  if not IsMouseDown then
    StopAnimationTimer;

  HorizontalScrollPositionChanged;
end;

function TAdvTreeViewBase.IsColumnVisible(ACol: Integer): Boolean;
begin
  Result := True;
end;

procedure TAdvTreeViewBase.Loaded;
begin
  inherited;
  UpdateTreeViewCache;
end;

function TAdvTreeViewBase.VerticalScrollBar: TScrollBar;
begin
  Result := FVerticalScrollBar;
end;

procedure TAdvTreeViewBase.VScrollChanged(Sender: TObject);
begin
  if FBlockScrollingUpdate then
    Exit;

  if not IsMouseDown then
    StopAnimationTimer;

  VerticalScrollPositionChanged;
end;

procedure TAdvTreeViewBase.EndUpdate;
begin
  inherited;
  Dec(FUpdateCount);
  if FUpdateCount = 0 then
    UpdateTreeViewCache;
end;

function TAdvTreeViewBase.GetColumnViewPortSize: Double;
var
  I,cnt: Integer;
  w: Double;
  cw: Double;
begin
  cw := GetContentRect.Width;
  Result := 0;
  w := 0;
  cnt := 0;
  for I := ColumnCount - 1 downto 0 do
  begin
    w := w + ColumnWidths[I];
    if w > cw then
    begin
      Result := Max(1, cnt);
      Break;
    end;
    Inc(cnt);
  end;
end;

function TAdvTreeViewBase.GetColWidths(Col: Integer): Double;
var
  idx: Integer;
begin
  idx := ColumnW.IndexOf(Col);
  if idx <> -1 then
    Result := ColumnW[idx].Value
  else
    Result := DefaultColumnWidth;
end;

function TAdvTreeViewBase.GetColPos(Col: Integer): Double;
var
  idx: Integer;
begin
  idx := ColumnP.IndexOf(Col);
  if idx <> -1 then
    Result := ColumnP[idx].Value
  else
    Result := -1;
end;

procedure TAdvTreeViewBase.Resize;
begin
  inherited;
  if (UpdateCount > 0) or (csDestroying in ComponentState) or (csLoading in ComponentState) then
    Exit;

  ResetNodes;
  SaveScrollPosition;
  UpdateNodesCache(True, True);
  UpdateGroupsCache;
  UpdateColumnsCache;
  RestoreScrollPosition;
end;

procedure TAdvTreeViewBase.Scroll(AHorizontalPos, AVerticalPos: Double);
begin
  FBlockScrollingUpdate := True;
  SetHScrollValue(AHorizontalPos);
  SetVScrollValue(AVerticalPos);
  FBlockScrollingUpdate := False;
  UpdateDisplay;
end;

procedure TAdvTreeViewBase.SetColumnCount(const Value: Integer);
begin
  if FColumnCount <> Value then
    FColumnCount := Value;
end;

procedure TAdvTreeViewBase.SetColumnW(const Value: TAdvTreeViewDoubleList);
begin
  FColumnW.Assign(Value);
end;

procedure TAdvTreeViewBase.SetColumnP(const Value: TAdvTreeViewDoubleList);
begin
  FColumnP.Assign(Value);
end;

procedure TAdvTreeViewBase.SetColWidths(Col: Integer; const Value: Double);
var
  idx: Integer;
  c: TAdvTreeViewDoubleListItem;
begin
  idx := ColumnW.IndexOf(Col);
  if (idx >= 0) and (idx <= ColumnW.Count - 1) then
  begin
    if Value = DefaultColumnWidth then
      ColumnW.Delete(idx)
    else
    begin
      ColumnW[idx].Value := Max(0, Value);
      ColumnW[idx].CellVal := Col;
    end;
  end
  else if Value <> DefaultColumnWidth then
  begin
    c := ColumnW.Add;
    c.Value := Max(0, Value);
    c.CellVal := Col;
  end;
end;

procedure TAdvTreeViewBase.SetColPos(Col: Integer; const Value: Double);
var
  idx: Integer;
  c: TAdvTreeViewDoubleListItem;
begin
  idx := ColumnP.IndexOf(Col);
  if (idx >= 0) and (idx <= ColumnP.Count - 1) then
  begin
    if Value = -1 then
      ColumnP.Delete(idx)
    else
    begin
      ColumnP[idx].Value := Max(0, Value);
      ColumnP[idx].CellVal := Col;
    end;
  end
  else if Value <> -1 then
  begin
    c := ColumnP.Add;
    c.Value := Max(0, Value);
    c.CellVal := Col;
  end;
end;

procedure TAdvTreeViewBase.SetDefaultColumnWidth(const Value: Double);
begin
  if FDefaultColumnWidth <> Value then
    FDefaultColumnWidth := Value;
end;

procedure TAdvTreeViewBase.SetDefaultRowHeight(const Value: Double);
begin
  if FDefaultRowHeight <> Value then
    FDefaultRowHeight := Value;
end;

procedure TAdvTreeViewBase.SetHorizontalScrollBarVisible(
  const Value: Boolean);
begin
  if FHorizontalScrollBarVisible <> Value then
  begin
    FHorizontalScrollBarVisible := Value;
    UpdateTreeViewCache;
  end;
end;

procedure TAdvTreeViewBase.SetHScrollValue(AValue: Single);
begin
  {$IFDEF FMXLIB}
  HorizontalScrollBar.Value := Min(HorizontalScrollBar.Max - HorizontalScrollBar.ViewportSize, Max(0, AValue));
  {$ENDIF}
  {$IFDEF VCLLIB}
  HorizontalScrollBar.Position :=  Min(HorizontalScrollBar.Max - HorizontalScrollBar.PageSize, Max(0, Round(AValue)));
  {$ENDIF}
end;

procedure TAdvTreeViewBase.SetRowCount(const Value: Integer);
begin
  if FRowCount <> Value then
    FRowCount := Value;
end;

procedure TAdvTreeViewBase.SetScrollMode(
  const Value: TAdvTreeViewScrollMode);
begin
  if FScrollMode <> Value then
  begin
    FScrollMode := Value;
    UpdateTreeViewDisplay;
  end;
end;

procedure TAdvTreeViewBase.SetStretchScrollBars(const Value: Boolean);
begin
  if FStretchScrollBars <> Value then
  begin
    FStretchScrollBars := Value;
    UpdateTreeViewDisplay;
  end;
end;

procedure TAdvTreeViewBase.SetVerticalScrollBarVisible(
  const Value: Boolean);
begin
  if FVerticalScrollBarVisible <> Value then
  begin
    FVerticalScrollBarVisible := Value;
    UpdateTreeViewCache;
  end;
end;

procedure TAdvTreeViewBase.SetVScrollValue(AValue: Single);
begin
  {$IFDEF FMXLIB}
  VerticalScrollBar.Value := Min(VerticalScrollBar.Max - VerticalScrollBar.ViewportSize, Max(0, AValue));
  {$ENDIF}
  {$IFDEF VCLLIB}
  VerticalScrollBar.Position := Min(VerticalScrollBar.Max - VerticalScrollBar.PageSize, Max(0, Round(AValue)));
  {$ENDIF}
end;

function TAdvTreeViewBase.StartCol: Integer;
begin
  Result := FStartCol;
end;

function TAdvTreeViewBase.StartOffset: Double;
begin
  Result := FStartOffset;
end;

function TAdvTreeViewBase.StartRow: Integer;
begin
  Result := FStartRow;
end;

function TAdvTreeViewBase.StartX: Double;
begin
  Result := FStartX;
end;

function TAdvTreeViewBase.StartY: Double;
begin
  Result := FStartY;
end;

function TAdvTreeViewBase.StopCol: Integer;
begin
  Result := FStopCol;
end;

function TAdvTreeViewBase.StopRow: Integer;
begin
  Result := FStopRow;
end;

function TAdvTreeViewBase.StopX: Double;
begin
  Result := FStopX;
end;

function TAdvTreeViewBase.StopY: Double;
begin
  Result := FStopY;
end;

procedure TAdvTreeViewBase.StretchColumn(AStretchAll: Boolean = True; ACol: Integer = -1; ANewWidth: Double = -1);
var
  i: Integer;
  w, nw, d: Double;
  cnt: Integer;
  horz, vert: TScrollBar;
  r: TRectF;
begin
  horz := HorizontalScrollBar;
  vert := VerticalScrollBar;
  if not Assigned(vert) or not Assigned(horz) then
    Exit;

  if ANewWidth = -1 then
  begin
    r := GetContentRect;
    nw := r.Width;
  end
  else
    nw := ANewWidth;

  cnt := 0;
  for I := 0 to ColumnCount - 1 do
  begin
    if IsColumnVisible(I) then
      cnt := cnt + 1;
  end;

  if ACol = - 1 then
    ACol := cnt - 1;

  if (ACol >= cnt) and not AStretchAll then
    raise Exception.Create('Stretch column index out of range');

  if (cnt = 0) then
    Exit;

  if cnt = 1 then
  begin
    ColumnWidths[0] := nw;
    Exit;
  end;

  w := 0;

  if AStretchAll then
  begin
    if (cnt > 0) then
    begin
      d := nw;

      w := d / cnt;

      for i := 0 to ColumnCount - 1 do
      begin
        if IsColumnVisible(i) then
          ColumnWidths[i] := w;
      end;
    end;
  end
  else
  begin
    for i := 0 to cnt - 1 do
    begin
      if (i <> ACol) and IsColumnVisible(i) then
        w := w + ColumnWidths[i];
    end;

    ColumnWidths[ACol] := nw - w {- 1};
  end;
end;

procedure TAdvTreeViewBase.AutoSizeColumn(ACol: Integer);
begin
  AutoSizeColumnInternal(ACol, True, True);
end;

procedure TAdvTreeViewBase.UpdateVisualRange;
var
  c, r: Integer;
  xval, yval: Double;
  cw, ch: Double;
  vpos, hpos: Double;
  cr: TRectF;
  reverse: Boolean;
  rh: Double;
begin
  hpos := GetHorizontalScrollPosition;
  vpos := GetVerticalScrollPosition;
  cr := GetContentRect;
  xval := -hpos;
  yval := -vpos;
  cw := cr.Width;
  ch := cr.Height;

  reverse := GetVScrollValue > VerticalScrollBar.Max / 2;

  if reverse then
    yval := yval + GetTotalRowHeight;

  FStartCol := -1;
  FStartRow := -1;
  FStopCol := -1;
  FStopRow := -1;
  FStartX := cr.Left;
  FStopX := cr.Left;
  FStartY := cr.Top;
  FStopY := cr.Top;
  FStartOffset := 0;

  for c := 0 to ColumnCount - 1 do
  begin
    xval := xval + ColumnWidths[c];
    if (xval > 0) and (FStartCol = -1) then
    begin
      FStartCol := c;
      FStartX := FStartX + int(xval - ColumnWidths[c]);
    end;

    if (xval >= cw) and (FStopCol = -1) then
    begin
      FStopCol := c;
      FStopX := FStopX + int(xval);
    end;

    if (FStartCol > -1) and (FStopCol > -1) then
      Break;
  end;

  if reverse then
  begin
    for r := RowCount - 1 downto 0 do
    begin
      rh := GetRowHeight(r);
      yval := yval - rh;
      if (yval <= 0) and (FStartRow = -1) then
      begin
        FStartRow := r;
        FStartOffset := yval;
        FStartY := FStartY + int(FStartOffset - rh);
      end;

      if (yval < ch) and (FStopRow = -1) then
      begin
        FStopRow := r;
        FStopY := FStopY + int(yval);
      end;

      if (FStartRow > -1) and (FStopRow > -1) then
        Break;
    end;
  end
  else
  begin
    for r := 0 to RowCount - 1 do
    begin
      rh := GetRowHeight(r);
      yval := yval + rh;
      if (yval > 0) and (FStartRow = -1) then
      begin
        FStartRow := r;
        FStartOffset := yval - rh;
        FStartY := FStartY + int(FStartOffset);
      end;

      if (yval >= ch) and (FStopRow = -1) then
      begin
        FStopRow := r;
        FStopY := FStopY + int(yval);
      end;

      if (FStartRow > -1) and (FStopRow > -1) then
        Break;
    end;
  end;

  if (FStartRow > -1) and (FStopRow = -1) then
    FStopRow := RowCount - 1;

  if (FStartCol > -1) and (FStopCol = -1) then
    FStopCol := ColumnCount - 1;
end;

procedure TAdvTreeViewBase.UpdateDisplay;
begin
  //ResetVisibleNodes;
end;

procedure TAdvTreeViewBase.UpdateTreeView;
begin
  SaveScrollPosition;
  UpdateNodesCache(True, True);
  RestoreScrollPosition;
  UpdateGroupsCache;
  UpdateColumnsCache;
end;

procedure TAdvTreeViewBase.UpdateTreeViewCache;
begin
  if (UpdateCount > 0) or BlockUpdateNode or (csDestroying in ComponentState) or (csLoading in ComponentState) then
    Exit;

  ResetNodes;
  UpdateCalculations;
  UpdateColumns;
  UpdateScrollBars;
  UpdateNodesCache(True, True);
  UpdateGroupsCache;
  UpdateColumnsCache;
end;

procedure TAdvTreeViewBase.UpdateTreeViewDisplay;
begin
  UpdateScrollBars;
  UpdateDisplay;
end;

procedure TAdvTreeViewBase.UpdateScrollBars(AUpdate: Boolean = True; ACalculate: Boolean = True);
var
  vs, hs: TScrollBar;
  w, h: Double;
  cw, ch: Double;
  cr: TRectF;
begin
  if {$IFDEF FMXLIB} FBlockScrollingUpdate or {$ENDIF} (UpdateCount > 0) or (csDestroying in ComponentState) then
    Exit;

  FBlockScrollingUpdate := True;

  if ACalculate then
  begin
    UpdateAutoSizing;
    UpdateColumnRowCalculations;
  end;

  vs := VerticalScrollBar;
  hs := HorizontalScrollBar;
  if Assigned(vs) and Assigned(hs) then
  begin
    cr := GetContentRect;
    cw := cr.Width;
    ch := cr.Height;
    w := GetTotalColumnWidth;
    h := GetTotalRowHeight;

    vs.Visible  := (h > 0) and (CompareValue(h, ch) = 1) and VerticalScrollBarVisible;
    hs.Visible := (w > 0) and (CompareValue(w, cw) = 1) and HorizontalScrollBarVisible;

    {$IFDEF FMXLIB}
    {$IFNDEF DELPHIXE7_LVL}
    vs.DesignVisible := hs.Visible;
    hs.DesignVisible := hs.Visible;
    {$ENDIF}
    {$ENDIF}
    {$IFDEF VCLLIB}
    if vs.Visible then
      vs.Parent := Self
    else
      vs.Parent := nil;

    if hs.Visible then
      hs.Parent := Self
    else
      hs.Parent := nil;
    {$ENDIF}

    {$IFNDEF LCLLIB}
    if vs.Visible and StretchScrollBars then
      hs.Margins.Right := vs.Width + 1
    else
      hs.Margins.Right := 1;

    vs.Margins.Top := 1;
    vs.Margins.Right := 1;
    hs.Margins.Top := 1;
    hs.Margins.Bottom := 1;
    hs.Margins.Left := 1;
    if hs.Visible then
    begin
      {$IFDEF FMXLIB}
      vs.Margins.Bottom := -1
      {$ENDIF}
      {$IFDEF VCLLIB}
      vs.Margins.Bottom := 0
      {$ENDIF}
    end
    else
      vs.Margins.Bottom := 1;

    if not StretchScrollBars then
    begin
      cr := GetCalculationRect;
      {$IFDEF FMXLIB}
      hs.Margins.Left := hs.Margins.Left + cr.Left;
      hs.Margins.Right := hs.Margins.Right + (Width - cr.Right);
      vs.Margins.Top := vs.Margins.Top + cr.Top;
      vs.Margins.Bottom := vs.Margins.Bottom + (Height - cr.Bottom);
      {$ENDIF}
      {$IFDEF VCLLIB}
      hs.Margins.Left := hs.Margins.Left + Round(cr.Left);
      hs.Margins.Right := hs.Margins.Right + Round(Width - cr.Right);
      vs.Margins.Top := vs.Margins.Top + Round(cr.Top);
      vs.Margins.Bottom := vs.Margins.Bottom + Round(Height - cr.Bottom);
      {$ENDIF}
    end;
    {$ENDIF}

    cr := GetContentRect;
    cw := cr.Width;
    ch := cr.Height;

    {$IFDEF FMXLIB}
    if ScrollMode = smNodeScrolling then
    begin
      vs.ViewportSize := GetRowViewPortSize;
      vs.Max := RowCount;
    end
    else
    begin
      vs.ViewPortSize := Min(h, ch);
      vs.Max := h;
      vs.SmallChange := Round(DefaultRowHeight);
      vs.Value := Min(vs.Value, vs.Max - vs.ViewportSize);
    end;

    if ScrollMode = smNodeScrolling then
    begin
      hs.ViewportSize := GetColumnViewPortSize;
      hs.Max := ColumnCount;
    end
    else
    begin
      hs.ViewPortSize := Min(w, cw);
      hs.Max := w;
      hs.Value := Min(hs.Value, hs.Max - hs.ViewportSize);
    end;
    {$ENDIF}
    {$IFDEF VCLLIB}
    if ScrollMode = smNodeScrolling then
    begin
      vs.PageSize := Round(GetRowViewPortSize);
      vs.Max := RowCount;
    end
    else
    begin
      vs.PageSize := Min(Round(h), Round(ch));
      vs.Max := Max(vs.PageSize, Round(h));
      vs.SmallChange := Round(DefaultRowHeight);
      vs.LargeChange := vs.PageSize;
      vs.Position := Min(vs.Position, vs.Max - vs.PageSize);
    end;

    if ScrollMode = smNodeScrolling then
    begin
      hs.PageSize := Round(GetColumnViewPortSize);
      hs.Max := ColumnCount;
    end
    else
    begin
      hs.PageSize := Min(Round(w), Round(cw));
      hs.Max := Max(hs.PageSize, Round(w));
      hs.Position := Min(hs.Position, hs.Max - hs.PageSize);
    end;
    {$ENDIF}
  end;

  FBlockScrollingUpdate := False;

  if AUpdate then
    UpdateScrollBars(False, False);
end;

{ TAdvTreeViewDoubleListItem }

constructor TAdvTreeViewDoubleListItem.Create(Collection: TCollection);
begin
  inherited;
  FOwner := (Collection as TAdvTreeViewDoubleList).FOwner;
end;

procedure TAdvTreeViewDoubleListItem.SetCellData(const Value: Double);
begin
  FValue := Value;
end;

{ TAdvTreeViewDoubleList }

function TAdvTreeViewDoubleList.Add: TAdvTreeViewDoubleListItem;
begin
  Result := TAdvTreeViewDoubleListItem(inherited Add);
end;

constructor TAdvTreeViewDoubleList.Create(AOwner: TAdvTreeViewBase);
begin
  inherited Create(TAdvTreeViewDoubleListItem);
  FOwner := AOwner;
end;

function TAdvTreeViewDoubleList.GetItem(
  Index: Integer): TAdvTreeViewDoubleListItem;
begin
  Result := TAdvTreeViewDoubleListItem(inherited Items[Index]);
end;

function TAdvTreeViewDoubleList.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TAdvTreeViewDoubleList.IndexOf(ACellVal: Integer): Integer;
begin
  for Result := 0 to Count - 1 do
    if Items[Result].CellVal = ACellVal then Exit;
  Result := -1;
end;

function TAdvTreeViewDoubleList.Insert(
  Index: Integer): TAdvTreeViewDoubleListItem;
begin
  Result := TAdvTreeViewDoubleListItem(inherited Insert(Index));
end;

procedure TAdvTreeViewDoubleList.SetItem(Index: Integer;
  const Value: TAdvTreeViewDoubleListItem);
begin
  inherited Items[Index] := Value;
end;

{$IFDEF VCLLIB}
procedure TAdvTreeViewBaseControl.BeginUpdate; 
begin

end;

procedure TAdvTreeViewBaseControl.EndUpdate;
begin

end;

function TAdvTreeViewBaseControl.LocalRect: TRectF;
begin
  Result := RectF(0, 0, Width, Height);
end;

function TAdvTreeViewBaseControl.GetDrawRect: TRectF;
begin
  Result := LocalRect;
end;

procedure TAdvTreeViewBaseControl.Paint;
begin
  inherited;
  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := Color;
  Canvas.Pen.Style := psSolid;
  Canvas.Pen.Color := BorderColor;
  Canvas.Rectangle(0, 0, Width, Height);
end;

constructor TAdvTreeViewBaseControl.Create(AOwner: TComponent);
begin
  inherited;
  DoubleBuffered := True;
  ControlStyle := ControlStyle + [csAcceptsControls];
  Color := TAdvTreeViewColorWhite;
  FBorderColor := TAdvTreeViewColorDarkGray;
end;

procedure TAdvTreeViewBaseControl.SetBorderColor(const Value: TAdvTreeViewColor);
begin
  FBorderColor := Value;
  Invalidate;
end;

{ TRectF }

{$IFNDEF DELPHIXE2_LVL}

{$IFNDEF LCLLIB}
constructor TRectF.Create(const Left, Top, Right, Bottom: Single);
begin
  Self.Left := Left; Self.Top := Top;
  Self.Right := Right; Self.Bottom := Bottom;
end;
{$ENDIF}

function TRectF.GetHeight: Single;
begin
  Result := Self.Bottom - Self.Top;
end;

procedure TRectF.SetHeight(const Value: Single);
begin
  Self.Bottom := Self.Top + Value;
end;

function TRectF.GetWidth: Single;
begin
  Result := Self.Right - Self.Left;
end;

procedure TRectF.SetWidth(const Value: Single);
begin
  Self.Right := Self.Left + Value;
end;

function TRectF.IntersectsWith(const R: TRectF): Boolean;
begin
  Result := not ( (Self.BottomRight.X < R.TopLeft.X) or
                  (Self.BottomRight.Y < R.TopLeft.Y) or
                  (R.BottomRight.X < Self.TopLeft.X) or
                  (R.BottomRight.Y < Self.TopLeft.Y) );
end;

function TRectF.IsEmpty: Boolean;
begin
  Result := (Right < Left) or SameValue(Right, Left)
         or (Bottom < Top) or SameValue(Bottom, Top);
end;
{$ENDIF}

{$ENDIF}

{ TAdvTreeViewBrush }

{$IFDEF VCLLIB}
procedure TAdvTreeViewBrush.SetGradient(const Value: TAdvTreeViewGradient);
begin
  FGradient.Assign(Value);
end;

procedure TAdvTreeViewBrush.SetKind(const Value: TAdvTreeViewBrushKind);
begin
  FKind := Value;
  if Assigned(OnChanged) then
    OnChanged(Self);
end;

procedure TAdvTreeViewBrush.Change(Sender: TObject);
begin
  if Assigned(OnChanged) then
    OnChanged(Self);
end;

procedure TAdvTreeViewBrush.GradientChanged(Sender: TObject);
begin
  if Assigned(OnChanged) then
    OnChanged(Self);
end;
{$ENDIF}

constructor TAdvTreeViewBrush.CreateBrush(AKind: TAdvTreeViewBrushKind;
  AColor: TAdvTreeViewColor);
{$IFDEF VCLLIB}
var
  b: TBrush;
{$ENDIF}
begin
  {$IFDEF FMXLIB}
  case AKind of
    tvbkNone: inherited Create(TBrushKind.None, AColor);
    tvbkSolid: inherited Create(TBrushKind.Solid, AColor);
    tvbkGradient: inherited Create(TBrushKind.Gradient, AColor);
    tvbkBitmap: inherited Create(TBrushKind.Bitmap, AColor);
    tvbkResource: inherited Create(TBrushKind.Resource, AColor);
  end;
  {$ENDIF}
  {$IFDEF VCLLIB}
  b := inherited Create;
  FKind := AKind;
  FGradient := TAdvTreeViewGradient.Create;
  FGradient.OnChanged := GradientChanged;
  case AKind of
    tvbkNone: b.Style := bsClear;
    tvbkSolid: b.Style := bsSolid;
  end;
  b.Color := AColor;
  b.OnChange := Change;
  {$ENDIF}
end;

destructor TAdvTreeViewBrush.Destroy;
begin
  {$IFDEF VCLLIB}
  FGradient.Free;
  {$ENDIF}
  inherited;
end;

{ TAdvTreeViewStrokeBrush }

{$IFDEF VCLLIB}
procedure TAdvTreeViewStrokeBrush.Change(Sender: TObject);
begin
  if Assigned(OnChanged) then
    OnChanged(Self);
end;
{$ENDIF}

constructor TAdvTreeViewStrokeBrush.CreateBrush(AKind: TAdvTreeViewBrushKind;
  AColor: TAdvTreeViewColor);
{$IFDEF VCLLIB}
var
  p: TPen;
{$ENDIF}
begin
  {$IFDEF FMXLIB}
  case AKind of
    tvbkNone: inherited Create(TBrushKind.None, AColor);
    tvbkSolid: inherited Create(TBrushKind.Solid, AColor);
    tvbkGradient: inherited Create(TBrushKind.Gradient, AColor);
    tvbkBitmap: inherited Create(TBrushKind.Bitmap, AColor);
    tvbkResource: inherited Create(TBrushKind.Resource, AColor);
  end;
  {$ENDIF}
  {$IFDEF VCLLIB}
  p := inherited Create;
  case AKind of
    tvbkNone: p.Style := psClear;
    tvbkSolid: p.Style := psSolid;
  end;
  p.Color := AColor;
  p.OnChange := Change;
  {$ENDIF}
end;

{$IFDEF VCLLIB}

{ TAdvTreeViewCanvasSaveState }

constructor TAdvTreeViewCanvasSaveState.Create;
begin
  inherited;
  FFill := TAdvTreeViewBrush.Create;
  FStroke := TAdvTreeViewStrokeBrush.Create;
end;

destructor TAdvTreeViewCanvasSaveState.Destroy;
begin
  FFill.Free;
  FStroke.Free;
  inherited;
end;

procedure TAdvTreeViewCanvasSaveState.SetFill(const Value: TAdvTreeViewBrush);
begin
  FFill.Assign(Value);
end;

procedure TAdvTreeViewCanvasSaveState.SetStroke(const Value: TAdvTreeViewStrokeBrush);
begin
  FStroke.Assign(Value);
end;

{ TAdvTreeViewGradient }

constructor TAdvTreeViewGradient.Create;
begin
  inherited;
  FColor := TAdvTreeViewColorWhite;
  FColorTo := TAdvTreeViewColorSilver;
end;

procedure TAdvTreeViewGradient.SetColor(const Value: TColor);
begin
  FColor := Value;
  if Assigned(OnChanged) then
    OnChanged(Self);
end;

procedure TAdvTreeViewGradient.SetColorTo(const Value: TColor);
begin
  FColorTo := Value;
  if Assigned(OnChanged) then
    OnChanged(Self);
end;

{$ENDIF}

{$IFDEF VCLLIB}
{$IFDEF FREEWARE}
function Scramble(s:string): string;
var
  r:string;
  i: integer;
  c: char;
  b: byte;
begin
  r := '';
  for i := 1 to length(s) do
  begin
    b := ord(s[i]);
    b := (b and $E0) + ((b and $1F) xor 5);
    c := chr(b);
    r := r + c;
  end;
  Result := r;
end;
{$ENDIF}
initialization
begin
{$IFDEF FREEWARE}
   if  (FindWindow(PChar(Scramble('QDuuilfdqljk')), nil) = 0) OR
       (FindWindow(PChar(Scramble('QDuuGplia`w')), nil) = 0) then
   begin
     MessageBox(0,PChar(Scramble('Duuilfdqljk%pv`v%qwldi%s`wvljk%jc%QHV%vjcqrdw`%fjhujk`kqv+')+#13#10+Scramble('Fjkqdfq%QHV%vjcqrdw`%mqqu?**rrr+qhvvjcqrdw`+fjh%cjw%sdila%ilf`kvlkb+')),PChar(Scramble('Rdwklkb')),MB_OK);
   end;
{$ENDIF}
end;
{$ENDIF}

end.
