{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressEditors                                           }
{                                                                    }
{           Copyright (c) 1998-2019 Developer Express Inc.           }
{           ALL RIGHTS RESERVED                                      }
{                                                                    }
{   The entire contents of this file is protected by U.S. and        }
{   International Copyright Laws. Unauthorized reproduction,         }
{   reverse-engineering, and distribution of all or any portion of   }
{   the code contained in this file is strictly prohibited and may   }
{   result in severe civil and criminal penalties and will be        }
{   prosecuted to the maximum extent possible under the law.         }
{                                                                    }
{   RESTRICTIONS                                                     }
{                                                                    }
{   THIS SOURCE CODE AND ALL RESULTING INTERMEDIATE FILES            }
{   (DCU, OBJ, DLL, ETC.) ARE CONFIDENTIAL AND PROPRIETARY TRADE     }
{   SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER IS    }
{   LICENSED TO DISTRIBUTE THE EXPRESSEDITORS AND ALL                }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY. }
{                                                                    }
{   THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED       }
{   FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE         }
{   COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE        }
{   AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT   }
{   AND PERMISSION FROM DEVELOPER EXPRESS INC.                       }
{                                                                    }
{   CONSULT THE END USER LICENSE AGREEMENT FOR INFORMATION ON        }
{   ADDITIONAL RESTRICTIONS.                                         }
{                                                                    }
{********************************************************************}

unit cxSplitter;

{$I cxVer.inc}

interface

uses
  Windows, Classes, Controls, Forms, Graphics, Messages, SysUtils, Types, ExtCtrls,
  cxGeometry, dxMessages, cxClasses, cxControls, cxExtEditConsts, cxExtEditUtils, cxGraphics,
  cxLookAndFeels, cxLookAndFeelPainters;

type
  TcxPositionAfterOpen = 2..High(Integer);
  TcxSplitterAlign = (salBottom, salLeft, salRight, salTop);
  TcxSplitterDragState = (sstNormal, sstResizing, sstHotZoneClick);
  TcxSplitterMouseState = (smsClicked, smsInHotZone);
  TcxSplitterMouseStates = set of TcxSplitterMouseState;
  TcxSplitterState = (ssOpened, ssClosed);
  TcxSplitterDirection = (cxsdLeftToRight, cxsdRightToLeft, cxsdTopToBottom, cxsdBottomToTop);

  TBeforeCloseHotZoneEvent = procedure(Sender: TObject; var AllowClose: Boolean) of object;
  TBeforeOpenHotZoneEvent = procedure(Sender: TObject; var NewSize: Integer; var AllowOpen: Boolean) of object;

type
  TcxCustomSplitter = class;

  { TcxHotZoneStyle }

  TcxHotZoneStyleClass = class of TcxHotZoneStyle;
  TcxHotZoneStyle = class(TPersistent)
  strict private
    FHotZoneRect: TRect;
    FOwner: TcxCustomSplitter;
    FSizePercent: TcxNaturalNumber;
    FVisible: Boolean;

    function GetScaleFactor: TdxScaleFactor;
    procedure SetSizePercent(Value: TcxNaturalNumber);
    procedure SetVisible(Value: Boolean);
  protected
    procedure Changed; virtual;
    function SplitterDirection: TcxSplitterDirection; virtual;
    function CalculateHotZoneRect(const ABounds: TRect): TRect; virtual;
    function GetMinSize: TcxNaturalNumber; virtual;
    function GetMaxSize: TcxNaturalNumber; virtual;
    function DrawHotZone(ACanvas: TcxCanvas; const ARect: TRect; const AHighlighted, AClicked: Boolean): TRect; virtual;
    procedure DrawBackground(ACanvas: TcxCanvas; const ARect: TRect; const AHighlighted, AClicked: Boolean); virtual;
    //
    property ScaleFactor: TdxScaleFactor read GetScaleFactor;
  public
    constructor Create(AOwner: TcxCustomSplitter); virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    //
    property SizePercent: TcxNaturalNumber read FSizePercent write SetSizePercent default 30;
    property Visible: Boolean read FVisible write SetVisible default True;
    property HotZoneRect: TRect read FHotZoneRect write FHotZoneRect;
    property Owner: TcxCustomSplitter read FOwner;
  end;

  { TcxMediaPlayer9Style }

  TcxMediaPlayer9Style = class(TcxHotZoneStyle)
  strict private
    FArrowRect: TRect;
    FArrowColor: TColor;
    FArrowHighlightColor: TColor;
    FLightColor: TColor;
    FBorderColor: TColor;
    FShadowStartColor: TColor;
    FShadowHighlightStartColor: TColor;

    procedure SetArrowColor(Value: TColor);
    procedure SetArrowHighlightColor(Value: TColor);
    procedure SetLightColor(Value: TColor);
    procedure SetBorderColor(Value: TColor);
    procedure SetShadowStartColor(Value: TColor);
    procedure SetShadowHighlightStartColor(Value: TColor);
  protected
    function CalculateHotZoneRect(const ABounds: TRect): TRect; override;
    procedure DrawBackground(ACanvas: TcxCanvas; const ARect: TRect; const AHighlighted, AClicked: Boolean); override;
    function DrawHotZone(ACanvas: TcxCanvas; const ARect: TRect; const AHighlighted, AClicked: Boolean): TRect; override;
  public
    constructor Create(AOwner: TcxCustomSplitter); override;
    procedure Assign(Source: TPersistent); override;
  published
    property SizePercent;
    property Visible;
    property ArrowColor: TColor read FArrowColor write SetArrowColor default clWindowText;
    property ArrowHighlightColor: TColor read FArrowHighlightColor write SetArrowHighlightColor default clBlue;
    property LightColor: TColor read FLightColor write SetLightColor default clWindow;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clBtnShadow;
    property ShadowStartColor: TColor read FShadowStartColor write SetShadowStartColor default $00F5E6CD;
    property ShadowHighlightStartColor: TColor read FShadowHighlightStartColor write SetShadowHighlightStartColor default $00AFF5C3;
  end;

  { TcxMediaPlayer8Style }

  TcxMediaPlayer8Style = class(TcxHotZoneStyle)
  strict private
    FArrowColor: TColor;
    FArrowHighlightColor: TColor;
    FArrowRect: TRect;
    FLightColor: TColor;
    FLTPointsRect: TRect;
    FRBPointsRect: TRect;
    FShadowColor: TColor;

    procedure SetArrowColor(Value: TColor);
    procedure SetArrowHighlightColor(Value: TColor);
    procedure SetLightColor(Value: TColor);
    procedure SetShadowColor(Value: TColor);
    procedure DrawArrowRect(ACanvas: TcxCanvas; const ARect: TRect; const AHighlighted, AClicked: Boolean);
  protected
    function CalculateHotZoneRect(const ABounds: TRect): TRect; override;
    procedure DrawBackground(ACanvas: TcxCanvas; const ARect: TRect; const AHighlighted, AClicked: Boolean); override;
    function DrawHotZone(ACanvas: TcxCanvas; const ARect: TRect; const AHighlighted, AClicked: Boolean): TRect; override;
  public
    constructor Create(AOwner: TcxCustomSplitter); override;
    procedure Assign(Source : TPersistent); override;
  published
    property SizePercent;
    property Visible;
    property ArrowColor: TColor read FArrowColor write SetArrowColor default clWindowText;
    property ArrowHighlightColor: TColor read FArrowHighlightColor write SetArrowHighlightColor default clWindow;
    property LightColor: TColor read FLightColor write SetLightColor default clWindow;
    property ShadowColor: TColor read FShadowColor write SetShadowColor default clBtnShadow;
  end;

  { TcxXPTaskBarStyle }

  TcxXPTaskBarStyle = class(TcxHotZoneStyle)
  private
    FLightColor: TColor;
    FLTPointsRect: TRect;
    FRBPointsRect: TRect;
    FShadowColor: TColor;
    procedure SetLightColor(Value: TColor);
    procedure SetShadowColor(Value: TColor);
  protected
    function CalculateHotZoneRect(const ABounds: TRect): TRect; override;
    procedure DrawBackground(ACanvas: TcxCanvas; const ARect: TRect; const AHighlighted, AClicked: Boolean); override;
    function DrawHotZone(ACanvas: TcxCanvas; const ARect: TRect; const AHighlighted, AClicked: Boolean): TRect; override;
  public
    constructor Create(AOwner: TcxCustomSplitter); override;
    procedure Assign(Source : TPersistent); override;
  published
    property SizePercent;
    property Visible;
    property LightColor: TColor read FLightColor write SetLightColor default clWindow;
    property ShadowColor: TColor read FShadowColor write SetShadowColor default clBtnShadow;
  end;

  { TcxSimpleStyle }

  TcxSimpleStyle = class(TcxHotZoneStyle)
  private
    FArrowColor: TColor;
    FArrowHighlightColor: TColor;
    FDotsColor: TColor;
    FDotsShadowColor: TColor;
    FLightColor: TColor;
    FLTArrowRect: TRect;
    FRBArrowRect: TRect;
    FShadowColor: TColor;

    procedure SetArrowColor(Value: TColor);
    procedure SetArrowHighlightColor(Value: TColor);
    procedure SetDotsColor(Value: TColor);
    procedure SetDotsShadowColor(Value: TColor);
    procedure SetLightColor(Value: TColor);
    procedure SetShadowColor(Value: TColor);
  protected
    function CalculateHotZoneRect(const ABounds: TRect): TRect; override;
    procedure DrawBackground(ACanvas: TcxCanvas; const ARect: TRect; const AHighlighted, AClicked: Boolean); override;
    function DrawHotZone(ACanvas: TcxCanvas; const ARect: TRect; const AHighlighted, AClicked: Boolean): TRect; override;
  public
    constructor Create(AOwner: TcxCustomSplitter); override;
    procedure Assign(Source : TPersistent); override;
  published
    property SizePercent;
    property Visible;
    property ArrowColor: TColor read FArrowColor write SetArrowColor default clWindowText;
    property ArrowHighlightColor: TColor read FArrowHighlightColor write SetArrowHighlightColor default clWindow;
    property LightColor: TColor read FLightColor write SetLightColor default clWindow;
    property ShadowColor: TColor read FShadowColor write SetShadowColor default clBtnShadow;
    property DotsColor: TColor read FDotsColor write SetDotsColor default clHighlight;
    property DotsShadowColor: TColor read FDotsShadowColor write SetDotsShadowColor default clWindow;
  end;

  {TdxSplitterDragImage}

  TdxSplitterDragImage = class(TcxCustomDragImage)
  protected
    procedure Paint; override;
  end;

  { TcxCustomSplitter }

  TcxCustomSplitter = class(TcxControl)
  private
    FActiveControl: TWinControl;
    FAlignSplitter: TcxSplitterAlign;
    FAllowHotZoneDrag: Boolean;
    FAutoPosition: Boolean;
    FAutoSnap: Boolean;
    FControl: TControl;
    FCurrentControl: TControl;
    FDragImage: TdxSplitterDragImage;
    FDragThreshold: TcxNaturalNumber;
    FHotZone: TcxHotZoneStyle;
    FHotZoneClickPoint: TPoint;
    FHotZoneEvents: TNotifyEvent;
    FHotZoneStyleClass: TcxHotZoneStyleClass;
    FInvertDirection: Boolean;
    FIsAdjustingPosition: Boolean;
    FIsAdjustingAlign: Boolean;
    FLastPatternDrawPosition: Integer;
    FMaxSize: Word;
    FMinSize: TcxNaturalNumber;
    FMouseStates: TcxSplitterMouseStates;
    FNativeBackground: Boolean;
    FNewSize: Integer;
    FOldSize: Integer;
    FPositionAfterOpen: TcxPositionAfterOpen;
    FResizeIgnoreSnap: Boolean; //deprecated
    FResizeUpdate: Boolean;
    FSavedParentShowHint: Boolean;
    FSavedShowHint: Boolean;
    FSplit: Integer;
    FSplitterClickPoint: TPoint;
    FSplitterState: TcxSplitterDragState;
    FState: TcxSplitterState;

    FOnAfterClose: TNotifyEvent;
    FOnAfterOpen: TNotifyEvent;
    FOnBeforeClose: TBeforeCloseHotZoneEvent;
    FOnBeforeOpen: TBeforeOpenHotZoneEvent;
    FOnCanResize: TCanResizeEvent;
    FOnMoved: TNotifyEvent;
    FPrevKeyDown: TKeyEvent;

    procedure AdjustAlignSettings(APostponedCorrection: Boolean = False);
    procedure CorrectSelfPosition;

    procedure CalcSplitSize(X, Y: Integer; var ANewSize, ASplit: Integer; ACorrectWithMaxMin: Boolean = True);
    procedure ClearConstraints;
    procedure ControlResizing(X, Y: Integer);
    function FindControl(AAlign: TAlign): TControl;
    procedure FocusKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    function GetMaxControlSize: Integer;
    function IsControlActualForSplitter(AControl: TControl): Boolean;
    function IsAllControlHotZoneStyle: Boolean;
    procedure UpdateControlSize;
    procedure UpdateSize(X, Y: Integer);

    function GetDragImageTopLeft: TPoint;
    procedure InitDragImage;
    procedure MoveDragImage;
    procedure ReleaseDragImage;

    procedure SetAlignSplitter(Value: TcxSplitterAlign);
    procedure SetAllowHotZoneDrag(Value: Boolean);
    procedure SetControl(Value: TControl);
    procedure SetDefaultStates;
    procedure SetDragThreshold(const Value: TcxNaturalNumber);
    procedure SetHotZone(Value: TcxHotZoneStyle);
    procedure SetInvertDirection(Value: Boolean);
    procedure SetMinSize(const Value: TcxNaturalNumber);
    procedure SetNativeBackground(Value: Boolean);
    procedure SetPositionAfterOpen(const Value: TcxPositionAfterOpen);
    procedure SetSplitterState(Value: TcxSplitterState);
    procedure RecalcLastPosition;
    procedure NormalizeSplitterSize;
    procedure SetHotZoneStyleClass(Value: TcxHotZoneStyleClass);
    function GetHotZoneClassName: string;
    procedure SetHotZoneClassName(Value: string);
    procedure InitResize(X, Y: Integer);

    procedure InternalOpenSplitter(ANewSize: Integer = 0);
    procedure InternalCloseSplitter;

    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure DXMAdjustPosition(var Message: TMessage); message DXM_ADJUSTPOSITION;
    procedure WMCancelMode(var Message: TWMCancelMode); message WM_CANCELMODE;
    procedure WMSetCursor(var Message: TWMSetCursor); message WM_SETCURSOR;
  protected
    { Protected declarations }
    FDrawBitmap: TcxBitmap;
    FPositionBeforeClose: Integer;

    procedure ChangeScaleEx(M, D: Integer; isDpiChange: Boolean); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function CanFocusOnClick: Boolean; override;
    procedure LookAndFeelChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;

    function CanResize(var NewSize: Integer): Boolean; reintroduce; virtual;
    function DoCanResize(var NewSize: Integer): Boolean; virtual;
    procedure HotZoneStyleChanged; virtual;
    procedure DrawHotZone; virtual;
    procedure StopSizing; virtual;
    function GetSplitterMinSize: TcxNaturalNumber; virtual;
    function GetSplitterMaxSize: TcxNaturalNumber; virtual;
    procedure CreateHotZone; virtual;
    procedure DestroyHotZone; virtual;
    procedure DoEventBeforeOpen(var ANewSize: Integer; var AllowOpenHotZone: Boolean); virtual;
    procedure DoEventAfterOpen; virtual;
    procedure DoEventBeforeClose(var AllowCloseHotZone: Boolean); virtual;
    procedure DoEventAfterClose; virtual;
    procedure DoEventMoved; virtual;
    function InternalGetMinSize: Integer;
    function CalculateSplitterDirection: TcxSplitterDirection; virtual;
    procedure UpdateMouseStates(X, Y: Integer); virtual;

    property AlignSplitter: TcxSplitterAlign read FAlignSplitter write SetAlignSplitter default salLeft;
    property AutoPosition: Boolean read FAutoPosition write FAutoPosition default True;
    property AutoSnap: Boolean read FAutoSnap write FAutoSnap default False;
    property AllowHotZoneDrag: Boolean read FAllowHotZoneDrag write SetAllowHotZoneDrag default True;
    property Control: TControl read FControl write SetControl;
    property DragThreshold: TcxNaturalNumber read FDragThreshold write SetDragThreshold default 3;
    property InvertDirection: Boolean read FInvertDirection write SetInvertDirection default False;
    property MinSize: TcxNaturalNumber read FMinSize write SetMinSize default 30;
    property NativeBackground: Boolean read FNativeBackground write SetNativeBackground default True;
    property PositionAfterOpen: TcxPositionAfterOpen read FPositionAfterOpen write SetPositionAfterOpen default 30;
    property ResizeUpdate: Boolean read FResizeUpdate write FResizeUpdate default False;
    property ResizeIgnoreSnap: Boolean read FResizeIgnoreSnap write FResizeIgnoreSnap stored False; //deprecated

    property OnCanResize: TCanResizeEvent read FOnCanResize write FOnCanResize;
    property OnMoved: TNotifyEvent read FOnMoved write FOnMoved;
    property OnBeforeOpen: TBeforeOpenHotZoneEvent read FOnBeforeOpen write FOnBeforeOpen;
    property OnAfterOpen: TNotifyEvent read FOnAfterOpen write FOnAfterOpen;
    property OnBeforeClose: TBeforeCloseHotZoneEvent read FOnBeforeClose write FOnBeforeClose;
    property OnAfterClose: TNotifyEvent read FOnAfterClose write FOnAfterClose;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure OpenSplitter;
    procedure CloseSplitter;
    function IsPointInHotZone(const X, Y: Integer): Boolean;
    function IsPointInSplitter(const X, Y: Integer): Boolean;
    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer; AHeight: Integer); override;

    property State: TcxSplitterState read FState write SetSplitterState;
    property HotZoneStyleClass: TcxHotZoneStyleClass read FHotZoneStyleClass write SetHotZoneStyleClass;
    property Direction: TcxSplitterDirection read CalculateSplitterDirection;
  published
    property HotZoneClassName: string read GetHotZoneClassName write SetHotZoneClassName;
    property HotZone: TcxHotZoneStyle read FHotZone write SetHotZone;
    property HotZoneEvents: TNotifyEvent read FHotZoneEvents write FHotZoneEvents;
  end;

  { TcxSplitter }

  TcxSplitter = class(TcxCustomSplitter)
  published
    property AlignSplitter;
    property AllowHotZoneDrag;
    property AutoPosition;
    property DragThreshold;
    property NativeBackground;
    property PositionAfterOpen;
    property AutoSnap;
    property InvertDirection;
    property MinSize;
    property ResizeUpdate;
    property ResizeIgnoreSnap; //deprecated
    property Control;
    property OnCanResize;
    property OnMoved;
    property OnBeforeOpen;
    property OnAfterOpen;
    property OnBeforeClose;
    property OnAfterClose;
    property Color;
    property ShowHint;
    property ParentColor;
    property ParentShowHint;
    property Visible;
  end;

function GetRegisteredHotZoneStyles: TcxRegisteredClasses;

implementation

uses
  Math,
  dxCore, cxContainer, dxThemeConsts, dxThemeManager, dxUxTheme;

type
  TWinControlAccess = class(TWinControl);

const
  SplitterDefaultSize = 8;

var
  FRegisteredHotZoneStyles: TcxRegisteredClasses;

function GetRegisteredHotZoneStyles: TcxRegisteredClasses;
begin
  if FRegisteredHotZoneStyles = nil then
    FRegisteredHotZoneStyles := TcxRegisteredClasses.Create;
  Result := FRegisteredHotZoneStyles;
end;

procedure DrawSplitterDots(ACanvas: TcxCanvas; const ARect: TRect;
  AClicked, AFromLeftTop: Boolean; ALightColor, AShadowColor: TColor; ASplitterDirection: TcxSplitterDirection;
  ABetweenPoints, AIndent: Integer; AScaleFactor: TdxScaleFactor);
var
  I, ANextDotPoint: Integer;

  procedure PaintOuterDot(X, Y, ASize: Integer);
  begin
    ACanvas.Brush.Color := ALightColor;
    ACanvas.FillRect(Rect(X, Y, X + ASize, Y + ASize));
    ACanvas.Brush.Color := AShadowColor;
    ACanvas.FillRect(Rect(X + 1, Y + 1, X + 1 + ASize, Y + 1 + ASize));
  end;

  procedure PaintInnerDot(X, Y, ASize: Integer);
  begin
    ACanvas.Brush.Color := ALightColor;
    ACanvas.FillRect(Rect(X + 1, Y + 1, X + 1 + ASize, Y + 1 + ASize));
    ACanvas.Brush.Color := AShadowColor;
    ACanvas.FillRect(Rect(X, Y, X + ASize, Y + ASize));
  end;

var
  ASize: Integer;
begin
  ASize := AScaleFactor.Apply(2);
  AIndent := AScaleFactor.Apply(AIndent);
  ABetweenPoints := AScaleFactor.Apply(ABetweenPoints);
  if AFromLeftTop then
  begin
    if (ASplitterDirection = cxsdLeftToRight) or (ASplitterDirection = cxsdRightToLeft) then
    begin
      ANextDotPoint := ARect.Top + ABetweenPoints;
      for I := ARect.Top + ABetweenPoints to ARect.Bottom - ABetweenPoints do
        if (I = ANextDotPoint) and ((I + ABetweenPoints) <= ARect.Bottom) then
        begin
          if not AClicked then
            PaintOuterDot(ARect.Left + AIndent, I, ASize)
          else
            PaintInnerDot(ARect.Left + AIndent, I, ASize);

          Inc(ANextDotPoint, ABetweenPoints + ASize);
        end;
    end
    else
    begin
      ANextDotPoint := ARect.Left + ABetweenPoints;
      for I := ARect.Left + ABetweenPoints to ARect.Right - ABetweenPoints do
        if (I = ANextDotPoint) and ((I + ABetweenPoints) <= ARect.Right) then
        begin
          if not AClicked then
            PaintOuterDot(I, ARect.Top + AIndent, ASize)
          else
            PaintInnerDot(I, ARect.Top + AIndent, ASize);

          Inc(ANextDotPoint, ABetweenPoints + ASize);
        end;
    end;
  end
  else
  begin
    if (ASplitterDirection = cxsdLeftToRight) or (ASplitterDirection = cxsdRightToLeft) then
    begin
      ANextDotPoint := ARect.Bottom - (ABetweenPoints * 2);
      for I := ARect.Bottom - (ABetweenPoints * 2) downto ARect.Top do
        if (I = ANextDotPoint) and (I >= ARect.Top) then
        begin
          if not AClicked then
            PaintOuterDot(ARect.Left + AIndent, I, ASize)
          else
            PaintInnerDot(ARect.Left + AIndent, I, ASize);

          Dec(ANextDotPoint, ABetweenPoints + ASize);
        end;
    end
    else
    begin
      ANextDotPoint := ARect.Right - (ABetweenPoints * 2);
      for I := ARect.Right - (ABetweenPoints * 2) downto ARect.Left do
        if (I = ANextDotPoint) and (I >= ARect.Left) then
        begin
          if not AClicked then
            PaintOuterDot(I, ARect.Top + AIndent, ASize)
          else
            PaintInnerDot(I, ARect.Top + AIndent, ASize);

          Dec(ANextDotPoint, ABetweenPoints + ASize);
        end;
    end;
  end;
end;

procedure DrawHotZoneArrow(ACanvas: TcxCanvas; const ARect: TRect;
  const AHighlighted, AClicked: Boolean; const ArrowColor, ArrowHighlightColor: TColor;
  const ASplitterDirection: TcxSplitterDirection; AScaleFactor: TdxScaleFactor);
var
  I, ADelta, ACenter, ARectSize: Integer;
  ALocalArrowColor: TColor;
begin
  if not AHighlighted or AClicked then
    ALocalArrowColor := ArrowColor
  else
    ALocalArrowColor := ArrowHighlightColor;

  if (ASplitterDirection = cxsdLeftToRight) or (ASplitterDirection = cxsdRightToLeft) then
  begin
    ARectSize := ARect.Bottom - ARect.Top;
    if (ARectSize mod 2) <> 0 then Dec(ARectSize, 1);
    ACenter := (ARectSize div 2) + 1;
  end
  else
  begin
    ARectSize := ARect.Right - ARect.Left;
    if (ARectSize mod 2) <> 0 then Dec(ARectSize, 1);
    ACenter := (ARectSize div 2) + 1;
  end;

  case ASplitterDirection of
    cxsdLeftToRight:
      for I := 0 to AScaleFactor.Apply(3) do
      begin
        if I = AScaleFactor.Apply(3) then
          ADelta := AScaleFactor.Apply(1)
        else
          ADelta := 0;

        DrawCanvasLine(ACanvas.Canvas, ALocalArrowColor, Point(ARect.Left + AScaleFactor.Apply(4) - I + ADelta,
          ARect.Top + ACenter - I), Point(ARect.Left + AScaleFactor.Apply(6) - I, ARect.Top + ACenter - I));
        DrawCanvasLine(ACanvas.Canvas, ALocalArrowColor, Point(ARect.Left + AScaleFactor.Apply(4) - I + ADelta,
          ARect.Top + ACenter + I), Point(ARect.Left + AScaleFactor.Apply(6) - I, ARect.Top + ACenter + I));
      end;

    cxsdRightToLeft:
      for I := 0 to AScaleFactor.Apply(3) do
      begin
        if I = AScaleFactor.Apply(3) then
          ADelta := -AScaleFactor.Apply(1)
        else
          ADelta := 0;

        DrawCanvasLine(ACanvas.Canvas, ALocalArrowColor,
          Point(ARect.Left + AScaleFactor.Apply(2) + I, ARect.Top + ACenter - I),
          Point(ARect.Left + AScaleFactor.Apply(4) + I + ADelta, ARect.Top + ACenter - I));
        DrawCanvasLine(ACanvas.Canvas, ALocalArrowColor,
          Point(ARect.Left + AScaleFactor.Apply(2) + I, ARect.Top + ACenter + I),
          Point(ARect.Left + AScaleFactor.Apply(4) + I + ADelta, ARect.Top + ACenter + I));
      end;

    cxsdTopToBottom:
      for I := 0 to AScaleFactor.Apply(3) do
      begin
        if I = AScaleFactor.Apply(3) then
          ADelta := AScaleFactor.Apply(1)
        else
          ADelta := 0;

        DrawCanvasLine(ACanvas.Canvas, ALocalArrowColor,
          Point(ARect.Left + ACenter - I, ARect.Top + AScaleFactor.Apply(4) - I + ADelta),
          Point(ARect.Left + ACenter - I, ARect.Top + AScaleFactor.Apply(6) - I));
        DrawCanvasLine(ACanvas.Canvas, ALocalArrowColor,
          Point(ARect.Left + ACenter + I, ARect.Top + AScaleFactor.Apply(4) - I + ADelta),
          Point(ARect.Left + ACenter + I, ARect.Top + AScaleFactor.Apply(6) - I));
      end;

    cxsdBottomToTop:
      for I := 0 to AScaleFactor.Apply(3) do
      begin
        if I = AScaleFactor.Apply(3) then
          ADelta := -AScaleFactor.Apply(1)
        else
          ADelta := 0;

        DrawCanvasLine(ACanvas.Canvas, ALocalArrowColor,
          Point(ARect.Left + ACenter - I, ARect.Top + AScaleFactor.Apply(2) + I),
          Point(ARect.Left + ACenter - I, ARect.Top + AScaleFactor.Apply(4) + I + ADelta));
        DrawCanvasLine(ACanvas.Canvas, ALocalArrowColor,
          Point(ARect.Left + ACenter + I, ARect.Top + AScaleFactor.Apply(2) + I),
          Point(ARect.Left + ACenter + I, ARect.Top + AScaleFactor.Apply(4) + I + ADelta));
      end;

  end;
end;

{ TcxHotZoneStyle }

constructor TcxHotZoneStyle.Create(AOwner: TcxCustomSplitter);
begin
  inherited Create;
  FOwner := AOwner;
  FSizePercent := 30;
  FVisible := True;
end;

destructor TcxHotZoneStyle.Destroy;
begin
  FOwner := nil;
  inherited;
end;

procedure TcxHotZoneStyle.Assign(Source: TPersistent);
begin
  if (Source is TcxHotZoneStyle) then
  begin
    with (Source as TcxHotZoneStyle) do
    begin
      Self.SizePercent := SizePercent;
      Self.Visible := Visible;
    end;
  end
  else
    inherited Assign(Source);
end;

procedure TcxHotZoneStyle.Changed;
begin
  if Assigned(FOwner) then
    FOwner.HotZoneStyleChanged;
end;

function TcxHotZoneStyle.SplitterDirection: TcxSplitterDirection;
begin
  if Assigned(FOwner) then
    Result := FOwner.CalculateSplitterDirection
  else
    Result := Low(TcxSplitterDirection);
end;

function TcxHotZoneStyle.DrawHotZone(ACanvas: TcxCanvas; const ARect: TRect; const AHighlighted, AClicked: Boolean): TRect;
begin
  Result := cxNullRect;
end;

procedure TcxHotZoneStyle.DrawBackground(ACanvas: TcxCanvas; const ARect: TRect; const AHighlighted, AClicked: Boolean);
begin
  // do nothing
end;

function TcxHotZoneStyle.GetMinSize: TcxNaturalNumber;
begin
  Result := ScaleFactor.Apply(SplitterDefaultSize);
end;

function TcxHotZoneStyle.GetMaxSize: TcxNaturalNumber;
begin
  Result := ScaleFactor.Apply(SplitterDefaultSize);
end;

function TcxHotZoneStyle.CalculateHotZoneRect(const ABounds: TRect): TRect;
var
  ARect : TRect;
  AHotZoneRectSize, APos: Integer;
begin
  ARect := ABounds;
  if (SplitterDirection = cxsdLeftToRight) or (SplitterDirection = cxsdRightToLeft) then
  begin
    ARect.Right := ARect.Left + ScaleFactor.Apply(SplitterDefaultSize) - 1;
    AHotZoneRectSize := ((ARect.Bottom - ARect.Top) * SizePercent) div 100;
    APos := ((ARect.Bottom - ARect.Top) div 2) - (AHotZoneRectSize div 2);
    Result := Rect(ARect.Left, APos, ARect.Right, APos + AHotZoneRectSize);
  end
  else
  begin
    ARect.Bottom := ARect.Top + ScaleFactor.Apply(SplitterDefaultSize) - 1;
    AHotZoneRectSize := ((ARect.Right - ARect.Left) * SizePercent) div 100;
    APos := ((ARect.Right - ARect.Left) div 2) - (AHotZoneRectSize div 2);
    Result := Rect(APos, ARect.Top, APos + AHotZoneRectSize, ARect.Bottom);
  end;
  HotZoneRect := Result;
end;

function TcxHotZoneStyle.GetScaleFactor: TdxScaleFactor;
begin
  Result := Owner.ScaleFactor;
end;

procedure TcxHotZoneStyle.SetSizePercent(Value: TcxNaturalNumber);
begin
  if FSizePercent <> Value then
  begin
    FSizePercent := Value;
    Changed;
  end;
end;

procedure TcxHotZoneStyle.SetVisible(Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    Changed;
  end;
end;

{ TcxMediaPlayer9Style }

constructor TcxMediaPlayer9Style.Create(AOwner: TcxCustomSplitter);
begin
  inherited Create(AOwner);
  FArrowColor := clWindowText;
  FArrowHighlightColor := clBlue;
  FLightColor := clWindow;
  FBorderColor := clBtnShadow;
  FShadowStartColor := $00F5E6CD;
  FShadowHighlightStartColor := $00AFF5C3;
end;

procedure TcxMediaPlayer9Style.Assign(Source: TPersistent);
begin
  if (Source is TcxMediaPlayer9Style) then
  begin
    inherited Assign(Source);
    with (Source as TcxMediaPlayer9Style) do
    begin
      Self.ArrowColor := ArrowColor;
      Self.ArrowHighlightColor := ArrowHighlightColor;
      Self.LightColor := LightColor;
      Self.BorderColor := BorderColor;
      Self.ShadowStartColor := ShadowStartColor;
      Self.ShadowHighlightStartColor := ShadowHighlightStartColor;
    end;
  end
  else
    inherited Assign(Source);
end;

function TcxMediaPlayer9Style.DrawHotZone(ACanvas: TcxCanvas; const ARect: TRect; const AHighlighted, AClicked: Boolean): TRect;
begin
  Result := CalculateHotZoneRect(ARect);
  ACanvas.Lock;
  try
    ACanvas.Canvas.Brush.Color := Owner.Color;
    DrawBackground(ACanvas, HotZoneRect, AHighlighted, AClicked);
    DrawHotZoneArrow(ACanvas, FArrowRect, AHighlighted, AClicked, FArrowColor, FArrowHighlightColor, SplitterDirection, ScaleFactor);
  finally
    ACanvas.Unlock;
  end;
end;

function TcxMediaPlayer9Style.CalculateHotZoneRect(const ABounds: TRect): TRect;
begin
  Result := inherited CalculateHotZoneRect(ABounds);
  if (SplitterDirection = cxsdLeftToRight) or (SplitterDirection = cxsdRightToLeft) then
  begin
    FArrowRect := Rect(Result.Left,
      Result.Top + (cxRectHeight(Result) div 2) - ScaleFactor.Apply(7),
      Result.Left + ScaleFactor.Apply(SplitterDefaultSize) - 1,
      Result.Top + (cxRectHeight(Result) div 2) + ScaleFactor.Apply(7))
  end
  else
  begin
    FArrowRect := Rect(
      Result.Left + (cxRectWidth(Result) div 2) - ScaleFactor.Apply(7),
      Result.Top,
      Result.Left + (cxRectWidth(Result) div 2) + ScaleFactor.Apply(7),
      Result.Top + ScaleFactor.Apply(SplitterDefaultSize) - 1);
  end;
  HotZoneRect := Result;
end;

procedure TcxMediaPlayer9Style.DrawBackground(ACanvas: TcxCanvas; const ARect: TRect; const AHighlighted, AClicked: Boolean);
var
  ALocalShadowColor: TColor;
  ALocalShadowColor2:  TColor;
  R: TRect;
begin
  case SplitterDirection of
    cxsdLeftToRight, cxsdRightToLeft:
      R := cxRectTransform(ARect, 0, 1, 0, -1);
    cxsdTopToBottom, cxsdBottomToTop:
      R := cxRectTransform(ARect, 1, 0, -1, 0);
  end;
  ACanvas.Pen.Color := BorderColor;
  ACanvas.Brush.Color := LightColor;
  ACanvas.FillRect(R);
  DrawBounds(ACanvas, R, BorderColor, BorderColor);

  if not AHighlighted then
    ALocalShadowColor := ShadowStartColor
  else
    ALocalShadowColor := ShadowHighlightStartColor;

  ALocalShadowColor2 := IncColor(ALocalShadowColor, -60);
  case SplitterDirection of
    cxsdLeftToRight, cxsdRightToLeft:
      begin
        DrawCanvasLine(ACanvas.Canvas, LightColor, Point(ARect.Left + 2, ARect.Top + 1), Point(ARect.Right - 1, ARect.Top + 1));
        DrawCanvasLine(ACanvas.Canvas, LightColor, Point(ARect.Left + 2, ARect.Bottom - 1), Point(ARect.Right - 1, ARect.Bottom - 1));

        if AClicked then
          FillGradientRect(ACanvas.Handle, cxRect(ARect.Left + 1, ARect.Top, cxRectCenter(ARect).X, ARect.Bottom),
            ALocalShadowColor2, ALocalShadowColor, True)
        else
          FillGradientRect(ACanvas.Handle, cxRect(cxRectCenter(ARect).X + 1, ARect.Top + 2, ARect.Right, ARect.Bottom),
            ALocalShadowColor, ALocalShadowColor2, True);

        DrawCanvasLine(ACanvas.Canvas, BorderColor, Point(ARect.Left + 1, ARect.Top), Point(ARect.Right, ARect.Top));
        DrawCanvasLine(ACanvas.Canvas, BorderColor, Point(ARect.Left + 1, ARect.Bottom), Point(ARect.Right, ARect.Bottom));
      end;

    cxsdTopToBottom, cxsdBottomToTop:
      begin
        DrawCanvasLine(ACanvas.Canvas, LightColor, Point(ARect.Left + 1, ARect.Top + 2), Point(ARect.Left + 1, ARect.Bottom - 1));
        DrawCanvasLine(ACanvas.Canvas, LightColor, Point(ARect.Right - 1, ARect.Top + 2), Point(ARect.Right - 1, ARect.Bottom - 1));

        if AClicked then
          FillGradientRect(ACanvas.Handle, cxRect(ARect.Left + 1, cxRectCenter(ARect).Y, ARect.Right, ARect.Bottom),
            ALocalShadowColor2, ALocalShadowColor, True)
        else
          FillGradientRect(ACanvas.Handle, cxRect(ARect.Left + 2, cxRectCenter(ARect).Y + 1, ARect.Right, ARect.Bottom),
            ALocalShadowColor, ALocalShadowColor2, True);

        DrawCanvasLine(ACanvas.Canvas, BorderColor, Point(ARect.Left, ARect.Top + 1), Point(ARect.Left, ARect.Bottom));
        DrawCanvasLine(ACanvas.Canvas, BorderColor, Point(ARect.Right, ARect.Top + 1), Point(ARect.Right, ARect.Bottom));
      end;
  end;
  ACanvas.Pen.Color := Owner.Color;
  ACanvas.Brush.Color := Owner.Color;
end;

procedure TcxMediaPlayer9Style.SetArrowColor(Value: TColor);
begin
  if FArrowColor <> Value then
  begin
    FArrowColor := Value;
    Changed;
  end;
end;

procedure TcxMediaPlayer9Style.SetArrowHighlightColor(Value: TColor);
begin
  if FArrowHighlightColor <> Value then
  begin
    FArrowHighlightColor := Value;
    Changed;
  end;
end;

procedure TcxMediaPlayer9Style.SetLightColor(Value: TColor);
begin
  if FLightColor <> Value then
  begin
    FLightColor := Value;
    Changed;
  end;
end;

procedure TcxMediaPlayer9Style.SetBorderColor(Value: TColor);
begin
  if FBorderColor <> Value then
  begin
    FBorderColor := Value;
    Changed;
  end;
end;

procedure TcxMediaPlayer9Style.SetShadowStartColor(Value: TColor);
begin
  if FShadowStartColor <> Value then
  begin
    FShadowStartColor := Value;
    Changed;
  end;
end;

procedure TcxMediaPlayer9Style.SetShadowHighlightStartColor(Value: TColor);
begin
  if FShadowHighlightStartColor <> Value then
  begin
    FShadowHighlightStartColor := Value;
    Changed;
  end;
end;

{ TcxMediaPlayer8Style }

constructor TcxMediaPlayer8Style.Create(AOwner: TcxCustomSplitter);
begin
  inherited Create(AOwner);
  FArrowColor := clWindowText;
  FArrowHighlightColor := clWindow;
  FLightColor := clWindow;
  FShadowColor := clBtnShadow;
end;

procedure TcxMediaPlayer8Style.Assign(Source: TPersistent);
begin
  if (Source is TcxMediaPlayer8Style) then
  begin
    inherited Assign(Source);
    with (Source as TcxMediaPlayer8Style) do
    begin
      Self.ShadowColor := ShadowColor;
      Self.LightColor := LightColor;
      Self.ArrowColor := ArrowColor;
      Self.ArrowHighlightColor := ArrowHighlightColor;
    end;
  end
  else
    inherited Assign(Source);
end;

procedure TcxMediaPlayer8Style.SetShadowColor(Value: TColor);
begin
  if FShadowColor <> Value then
  begin
    FShadowColor := Value;
    Changed;
  end;
end;

procedure TcxMediaPlayer8Style.SetLightColor(Value: TColor);
begin
  if FLightColor <> Value then
  begin
    FLightColor := Value;
    Changed;
  end;
end;

procedure TcxMediaPlayer8Style.SetArrowColor(Value: TColor);
begin
  if FArrowColor <> Value then
  begin
    FArrowColor := Value;
    Changed;
  end;
end;

procedure TcxMediaPlayer8Style.SetArrowHighlightColor(Value: TColor);
begin
  if FArrowHighlightColor <> Value then
  begin
    FArrowHighlightColor := Value;
    Changed;
  end;
end;

function TcxMediaPlayer8Style.DrawHotZone(ACanvas: TcxCanvas; const ARect: TRect; const AHighlighted, AClicked: Boolean): TRect;
begin
  Result := CalculateHotZoneRect(ARect);
  ACanvas.Lock;
  try
    ACanvas.Canvas.Brush.Color := Owner.Color;
    DrawBackground(ACanvas, HotZoneRect, AHighlighted, AClicked);
    DrawSplitterDots(ACanvas, FLTPointsRect, AClicked, False, FLightColor, FShadowColor, SplitterDirection, 3, 3, ScaleFactor);
    DrawSplitterDots(ACanvas, FRBPointsRect, AClicked, True, FLightColor, FShadowColor, SplitterDirection, 3, 3, ScaleFactor);
    DrawArrowRect(ACanvas, FArrowRect, AHighlighted, AClicked);
    DrawHotZoneArrow(ACanvas, FArrowRect, AHighlighted, AClicked, FArrowColor, FArrowHighlightColor, SplitterDirection, ScaleFactor);
  finally
    ACanvas.Unlock;
  end;
end;

function TcxMediaPlayer8Style.CalculateHotZoneRect(const ABounds: TRect): TRect;
var
  FRect : TRect;
  FHotZoneRectSize, FPos: Integer;
  FHotZonePointsRectHeight, FHotZoneRoundRectHeight: Integer;
begin
  FRect := ABounds;
  if (SplitterDirection = cxsdLeftToRight) or (SplitterDirection = cxsdRightToLeft) then
  begin
    FRect.Right := FRect.Left + ScaleFactor.Apply(SplitterDefaultSize) - 1;
    FHotZoneRectSize := ((FRect.Bottom - FRect.Top) * SizePercent) div 100;
    FPos := ((FRect.Bottom - FRect.Top) div 2) - (FHotZoneRectSize div 2);
    HotZoneRect := Rect(FRect.Left, FPos, FRect.Right, FPos + FHotZoneRectSize);
  end
  else
  begin
    FRect.Bottom := FRect.Top + ScaleFactor.Apply(SplitterDefaultSize) - 1;
    FHotZoneRectSize := ((FRect.Right - FRect.Left) * SizePercent) div 100;
    FPos := ((FRect.Right - FRect.Left) div 2) - (FHotZoneRectSize div 2);
    HotZoneRect := Rect(FPos, FRect.Top, FPos + FHotZoneRectSize, FRect.Bottom);
  end;

  FHotZoneRoundRectHeight := 4;
  FHotZonePointsRectHeight := (FHotZoneRectSize - (FHotZoneRoundRectHeight * 2) - ScaleFactor.Apply(19)) div 2;
  if (SplitterDirection = cxsdLeftToRight) or (SplitterDirection = cxsdRightToLeft) then
  begin
    FLTPointsRect := Rect(HotZoneRect.Left, HotZoneRect.Top + FHotZoneRoundRectHeight,
      HotZoneRect.Left + HotZoneRect.Right - HotZoneRect.Left,
      HotZoneRect.Top + FHotZoneRoundRectHeight + FHotZonePointsRectHeight);
    FRBPointsRect := Rect(HotZoneRect.Left, HotZoneRect.Bottom - FHotZoneRoundRectHeight - FHotZonePointsRectHeight,
      HotZoneRect.Left + HotZoneRect.Right - HotZoneRect.Left,
      HotZoneRect.Bottom - FHotZoneRoundRectHeight - FHotZonePointsRectHeight + FHotZonePointsRectHeight);
    FArrowRect := Rect(HotZoneRect.Left, HotZoneRect.Top + FHotZonePointsRectHeight + FHotZoneRoundRectHeight,
      HotZoneRect.Left + ScaleFactor.Apply(SplitterDefaultSize) - 1,
      HotZoneRect.Top + FHotZonePointsRectHeight + FHotZoneRoundRectHeight + ScaleFactor.Apply(19));
  end
  else
  begin
    FLTPointsRect := Rect(HotZoneRect.Left + FHotZoneRoundRectHeight,
      HotZoneRect.Top, HotZoneRect.Left + FHotZoneRoundRectHeight + FHotZonePointsRectHeight,
      HotZoneRect.Bottom);
    FRBPointsRect := Rect(HotZoneRect.Right - FHotZoneRoundRectHeight - FHotZonePointsRectHeight,
      HotZoneRect.Top, HotZoneRect.Right - FHotZoneRoundRectHeight - FHotZonePointsRectHeight + FHotZonePointsRectHeight,
      HotZoneRect.Bottom);
    FArrowRect := Rect(HotZoneRect.Left + FHotZonePointsRectHeight + FHotZoneRoundRectHeight,
      HotZoneRect.Top, HotZoneRect.Left + FHotZonePointsRectHeight + FHotZoneRoundRectHeight + ScaleFactor.Apply(19),
      HotZoneRect.Top + ScaleFactor.Apply(SplitterDefaultSize) - 1);
  end;
  Result := HotZoneRect;
end;

procedure TcxMediaPlayer8Style.DrawBackground(ACanvas: TcxCanvas; const ARect: TRect; const AHighlighted, AClicked: Boolean);
begin
  ACanvas.Pen.Color := Owner.Color;
  ACanvas.Brush.Color := Owner.Color;
  ACanvas.FillRect(ARect);
  case SplitterDirection of
    cxsdLeftToRight:
    begin
      {Shadow border}
      DrawCanvasLine(ACanvas.Canvas, ShadowColor, Point(ARect.Left, ARect.Bottom - 4),
        Point(ARect.Left, ARect.Top + 3));
      DrawCanvasLine(ACanvas.Canvas, ShadowColor, Point(ARect.Right, ARect.Bottom),
        Point(ARect.Right, ARect.Top));
      DrawCanvasLine(ACanvas.Canvas, ShadowColor, Point(ARect.Left + 1, ARect.Bottom - 2),
        Point(ARect.Left + 1, ARect.Bottom - 4));
      DrawCanvasLine(ACanvas.Canvas, ShadowColor, Point(ARect.Left + 1, ARect.Top + 2),
        Point(ARect.Left + 1, ARect.Top + 4));
      DrawCanvasLine(ACanvas.Canvas, ShadowColor, Point(ARect.Left + 2, ARect.Bottom - 1),
        Point(ARect.Left + 4, ARect.Bottom - 1));
      DrawCanvasLine(ACanvas.Canvas, ShadowColor, Point(ARect.Left + 2, ARect.Top + 1),
        Point(ARect.Left + 4, ARect.Top + 1));
      DrawCanvasLine(ACanvas.Canvas, ShadowColor, Point(ARect.Left + 4, ARect.Bottom),
        Point(ARect.Right, ARect.Bottom));
      DrawCanvasLine(ACanvas.Canvas, ShadowColor, Point(ARect.Left + 4, ARect.Top),
        Point(ARect.Right + 1, ARect.Top));
      {Light border}

      if not AClicked then
      begin
        DrawCanvasLine(ACanvas.Canvas, LightColor, Point(ARect.Left + 1, ARect.Bottom - 4),
          Point(ARect.Left + 1, ARect.Top + 3));
        DrawCanvasLine(ACanvas.Canvas, LightColor, Point(ARect.Left + 2, ARect.Top + 2),
          Point(ARect.Left + 2, ARect.Top + 4));
        DrawCanvasLine(ACanvas.Canvas, LightColor, Point(ARect.Left + 2, ARect.Top + 2),
          Point(ARect.Left + 4, ARect.Top + 2));
        DrawCanvasLine(ACanvas.Canvas, LightColor, Point(ARect.Left + 4, ARect.Top + 1),
          Point(ARect.Right, ARect.Top + 1));
        DrawCanvasLine(ACanvas.Canvas, LightColor, Point(ARect.Left + 2, ARect.Bottom - 2),
          Point(ARect.Left + 2, ARect.Bottom - 4));
        DrawCanvasLine(ACanvas.Canvas, LightColor, Point(ARect.Left + 2, ARect.Bottom - 2),
          Point(ARect.Left + 4, ARect.Bottom - 2));
        DrawCanvasLine(ACanvas.Canvas, LightColor, Point(ARect.Left + 4, ARect.Bottom - 1),
          Point(ARect.Left + 5, ARect.Bottom - 1));
      end
      else
      begin
        DrawCanvasLine(ACanvas.Canvas, LightColor, Point(ARect.Right - 1, ARect.Bottom - 1),
          Point(ARect.Right - 1, ARect.Top + 1));
        DrawCanvasLine(ACanvas.Canvas, LightColor, Point(ARect.Left + 2, ARect.Bottom - 2),
          Point(ARect.Left + 4, ARect.Bottom - 2));
        DrawCanvasLine(ACanvas.Canvas, LightColor, Point(ARect.Left + 4, ARect.Bottom - 1),
          Point(ARect.Left + 7, ARect.Bottom - 1));
      end;
    end;
    cxsdRightToLeft:
    begin
      {Shadow border}
      DrawCanvasLine(ACanvas.Canvas, ShadowColor, Point(ARect.Right, ARect.Bottom - 4),
        Point(ARect.Right, ARect.Top + 3));
      DrawCanvasLine(ACanvas.Canvas, ShadowColor, Point(ARect.Left, ARect.Bottom),
        Point(ARect.Left, ARect.Top));
      DrawCanvasLine(ACanvas.Canvas, ShadowColor, Point(ARect.Right - 1, ARect.Bottom - 2),
        Point(ARect.Right - 1, ARect.Bottom - 4));
      DrawCanvasLine(ACanvas.Canvas, ShadowColor, Point(ARect.Right - 1, ARect.Top + 2),
        Point(ARect.Right - 1, ARect.Top + 4));
      DrawCanvasLine(ACanvas.Canvas, ShadowColor, Point(ARect.Right - 2, ARect.Bottom - 1),
        Point(ARect.Right - 4, ARect.Bottom - 1));
      DrawCanvasLine(ACanvas.Canvas, ShadowColor, Point(ARect.Right - 2, ARect.Top + 1),
        Point(ARect.Right - 4, ARect.Top + 1));
      DrawCanvasLine(ACanvas.Canvas, ShadowColor, Point(ARect.Right - 4, ARect.Bottom),
        Point(ARect.Left, ARect.Bottom));
      DrawCanvasLine(ACanvas.Canvas, ShadowColor, Point(ARect.Right - 3, ARect.Top),
        Point(ARect.Left - 1, ARect.Top));
      {Light border}
      if not AClicked then
      begin
        DrawCanvasLine(ACanvas.Canvas, LightColor, Point(ARect.Left + 1, ARect.Bottom - 1),
          Point(ARect.Left + 1, ARect.Top + 1));
        DrawCanvasLine(ACanvas.Canvas, LightColor, Point(ARect.Left + 1, ARect.Top + 1),
          Point(ARect.Left + 4, ARect.Top + 1));
        DrawCanvasLine(ACanvas.Canvas, LightColor, Point(ARect.Left + 4, ARect.Top + 2),
          Point(ARect.Left + 6, ARect.Top + 2));
      end
      else
      begin
        DrawCanvasLine(ACanvas.Canvas, LightColor, Point(ARect.Left + 3, ARect.Top + 1),
          Point(ARect.Left + 4, ARect.Top + 1));
        DrawCanvasLine(ACanvas.Canvas, LightColor, Point(ARect.Left + 4, ARect.Top + 2),
          Point(ARect.Left + 6, ARect.Top + 2));
        DrawCanvasLine(ACanvas.Canvas, LightColor, Point(ARect.Left + 5, ARect.Top + 2),
          Point(ARect.Left + 5, ARect.Top + 4));
        DrawCanvasLine(ACanvas.Canvas, LightColor, Point(ARect.Left + 6, ARect.Top + 4),
          Point(ARect.Left + 6, ARect.Bottom - 3));
        DrawCanvasLine(ACanvas.Canvas, LightColor, Point(ARect.Left + 1, ARect.Bottom - 1),
          Point(ARect.Left + 4, ARect.Bottom - 1));
        DrawCanvasLine(ACanvas.Canvas, LightColor, Point(ARect.Left + 4, ARect.Bottom - 2),
          Point(ARect.Left + 6, ARect.Bottom - 2));
        DrawCanvasLine(ACanvas.Canvas, LightColor, Point(ARect.Left + 5, ARect.Bottom - 2),
          Point(ARect.Left + 5, ARect.Bottom - 4));
      end;
    end;
    cxsdTopToBottom:
    begin
      {Shadow border}
      DrawCanvasLine(ACanvas.Canvas, ShadowColor, Point(ARect.Left + 4, ARect.Top),
        Point(ARect.Right - 3, ARect.Top));
      DrawCanvasLine(ACanvas.Canvas, ShadowColor, Point(ARect.Left, ARect.Bottom),
        Point(ARect.Right, ARect.Bottom));
      DrawCanvasLine(ACanvas.Canvas, ShadowColor, Point(ARect.Left + 2, ARect.Top + 1),
        Point(ARect.Left + 4, ARect.Top + 1));
      DrawCanvasLine(ACanvas.Canvas, ShadowColor, Point(ARect.Right - 2, ARect.Top + 1),
        Point(ARect.Right - 4, ARect.Top + 1 ));
      DrawCanvasLine(ACanvas.Canvas, ShadowColor, Point(ARect.Left + 1, ARect.Top + 2),
        Point(ARect.Left + 1, ARect.Top + 4));
      DrawCanvasLine(ACanvas.Canvas, ShadowColor, Point(ARect.Right - 1, ARect.Top + 2),
        Point(ARect.Right - 1,ARect.Top + 4));
      DrawCanvasLine(ACanvas.Canvas, ShadowColor, Point(ARect.Left, ARect.Top + 4),
        Point(ARect.Left, ARect.Bottom));
      DrawCanvasLine(ACanvas.Canvas, ShadowColor, Point(ARect.Right, ARect.Top + 4),
        Point(ARect.Right, ARect.Bottom + 1));
      {Light border}
      if not AClicked then
      begin
        DrawCanvasLine(ACanvas.Canvas, LightColor, Point(ARect.Left + 4, ARect.Top + 1),
          Point(ARect.Right - 3, ARect.Top + 1));
        DrawCanvasLine(ACanvas.Canvas, LightColor, Point(ARect.Right - 2, ARect.Top + 2),
          Point(ARect.Right - 4, ARect.Top + 2));
        DrawCanvasLine(ACanvas.Canvas, LightColor, Point(ARect.Right - 2, ARect.Top + 2),
          Point(ARect.Right - 2, ARect.Top + 4));
        DrawCanvasLine(ACanvas.Canvas, LightColor, Point(ARect.Right - 1, ARect.Top + 4),
          Point(ARect.Right - 1, ARect.Bottom));
        DrawCanvasLine(ACanvas.Canvas, LightColor, Point(ARect.Left + 2, ARect.Top + 2),
          Point(ARect.Left + 4, ARect.Top + 2));
        DrawCanvasLine(ACanvas.Canvas, LightColor, Point(ARect.Left + 2, ARect.Top + 2),
          Point(ARect.Left + 2, ARect.Top + 4));
        DrawCanvasLine(ACanvas.Canvas, LightColor, Point(ARect.Left + 1, ARect.Top + 4),
          Point(ARect.Left + 1, ARect.Top + 5));
      end
      else
      begin
        DrawCanvasLine(ACanvas.Canvas, LightColor, Point(ARect.Left + 1, ARect.Bottom - 1),
          Point(ARect.Right - 1, ARect.Bottom - 1));
        DrawCanvasLine(ACanvas.Canvas, LightColor, Point(ARect.Left + 2, ARect.Top + 2),
          Point(ARect.Left + 2, ARect.Top + 4));
        DrawCanvasLine(ACanvas.Canvas, LightColor, Point(ARect.Left + 1, ARect.Top + 4),
          Point(ARect.Left + 1, ARect.Top + 7));
      end;
    end;
    cxsdBottomToTop:
    begin
      {Shadow border}
      DrawCanvasLine(ACanvas.Canvas, ShadowColor, Point(ARect.Left + 4, ARect.Bottom),
        Point(ARect.Right - 3, ARect.Bottom));
      DrawCanvasLine(ACanvas.Canvas, ShadowColor, Point(ARect.Left, ARect.Top),
        Point(ARect.Right, ARect.Top));
      DrawCanvasLine(ACanvas.Canvas, ShadowColor, Point(ARect.Left + 2, ARect.Bottom - 1),
        Point(ARect.Left + 4, ARect.Bottom - 1));
      DrawCanvasLine(ACanvas.Canvas, ShadowColor, Point(ARect.Right - 2, ARect.Bottom - 1),
        Point(ARect.Right - 4, ARect.Bottom - 1 ));
      DrawCanvasLine(ACanvas.Canvas, ShadowColor, Point(ARect.Left + 1, ARect.Bottom - 2),
        Point(ARect.Left + 1, ARect.Bottom - 4));
      DrawCanvasLine(ACanvas.Canvas, ShadowColor, Point(ARect.Right - 1, ARect.Bottom - 2),
        Point(ARect.Right - 1,ARect.Bottom - 4));
      DrawCanvasLine(ACanvas.Canvas, ShadowColor, Point(ARect.Left, ARect.Bottom - 4),
        Point(ARect.Left, ARect.Top));
      DrawCanvasLine(ACanvas.Canvas, ShadowColor, Point(ARect.Right, ARect.Bottom - 3),
        Point(ARect.Right, ARect.Top - 1));
      {Light border}
      if not AClicked then
      begin
        DrawCanvasLine(ACanvas.Canvas, LightColor, Point(ARect.Left + 1, ARect.Top + 1),
          Point(ARect.Right - 1, ARect.Top + 1));
        DrawCanvasLine(ACanvas.Canvas, LightColor, Point(ARect.Left + 2, ARect.Bottom - 2),
          Point(ARect.Left + 2, ARect.Bottom - 4));
        DrawCanvasLine(ACanvas.Canvas, LightColor, Point(ARect.Left + 1, ARect.Bottom - 4),
          Point(ARect.Left + 1, ARect.Bottom - 7));
      end
      else
      begin
        DrawCanvasLine(ACanvas.Canvas, LightColor, Point(ARect.Left + 4, ARect.Bottom - 1),
          Point(ARect.Right - 3, ARect.Bottom - 1));
        DrawCanvasLine(ACanvas.Canvas, LightColor, Point(ARect.Right - 2, ARect.Bottom - 2),
          Point(ARect.Right - 4, ARect.Bottom - 2));
        DrawCanvasLine(ACanvas.Canvas, LightColor, Point(ARect.Right - 2, ARect.Bottom - 2),
          Point(ARect.Right - 2, ARect.Bottom - 4));
        DrawCanvasLine(ACanvas.Canvas, LightColor, Point(ARect.Right - 1, ARect.Bottom - 4),
          Point(ARect.Right - 1, ARect.Top));
        DrawCanvasLine(ACanvas.Canvas, LightColor, Point(ARect.Left + 2, ARect.Bottom - 2),
          Point(ARect.Left + 4, ARect.Bottom - 2));
        DrawCanvasLine(ACanvas.Canvas, LightColor, Point(ARect.Left + 2, ARect.Bottom - 2),
          Point(ARect.Left + 2, ARect.Bottom - 4));
        DrawCanvasLine(ACanvas.Canvas, LightColor, Point(ARect.Left + 1, ARect.Bottom - 4),
          Point(ARect.Left + 1, ARect.Bottom - 5));
      end;
    end;
  end;
end;

procedure TcxMediaPlayer8Style.DrawArrowRect(ACanvas: TcxCanvas; const ARect: TRect; const AHighlighted, AClicked: Boolean);
begin
  ACanvas.Brush.Color := Owner.Color;
  ACanvas.FillRect(ARect);
  if not AClicked then
    DrawBounds(ACanvas, ARect, FShadowColor, FLightColor)
  else
    DrawBounds(ACanvas, ARect, FLightColor, FShadowColor);
end;

{ TcxXPTaskBarStyle }

constructor TcxXPTaskBarStyle.Create(AOwner: TcxCustomSplitter);
begin
  inherited Create(AOwner);
  FLightColor := clWindow;
  FShadowColor := clBtnShadow;
end;

procedure TcxXPTaskBarStyle.Assign(Source: TPersistent);
begin
  if (Source is TcxXPTaskBarStyle) then
  begin
    inherited Assign(Source);
    with (Source as TcxXPTaskBarStyle) do
    begin
      Self.LightColor := LightColor;
      Self.ShadowColor := ShadowColor;
    end;
  end
  else
    inherited Assign(Source);
end;

procedure TcxXPTaskBarStyle.SetLightColor(Value: TColor);
begin
  if FLightColor <> Value then
  begin
    FLightColor := Value;
    Changed;
  end;
end;

procedure TcxXPTaskBarStyle.SetShadowColor(Value: TColor);
begin
  if FShadowColor <> Value then
  begin
    FShadowColor := Value;
    Changed;
  end;
end;

function TcxXPTaskBarStyle.DrawHotZone(ACanvas: TcxCanvas; const ARect: TRect;
  const AHighlighted, AClicked: Boolean): TRect;
begin
  Result := CalculateHotZoneRect(ARect);
  ACanvas.Lock;
  try
    ACanvas.Canvas.Brush.Color := Owner.Color;
    DrawBackground(ACanvas, HotZoneRect, AHighlighted, AClicked);
    DrawSplitterDots(ACanvas, FLTPointsRect, not AClicked, True, FLightColor, FShadowColor, SplitterDirection, 4, 0, ScaleFactor);
    DrawSplitterDots(ACanvas, FRBPointsRect, not AClicked, True, FLightColor, FShadowColor, SplitterDirection, 4, 0, ScaleFactor);
  finally
    ACanvas.Unlock;
  end;
end;

function TcxXPTaskBarStyle.CalculateHotZoneRect(const ABounds: TRect): TRect;
var
  FRect : TRect;
  FHotZoneRectSize, FPos: Integer;
begin
  Result := inherited CalculateHotZoneRect(ABounds);

  FRect := ABounds;
  case SplitterDirection of
    cxsdLeftToRight, cxsdRightToLeft:
      begin
        FRect.Right := FRect.Left + ScaleFactor.Apply(SplitterDefaultSize) - 1;
        FHotZoneRectSize := ((FRect.Bottom - FRect.Top) * SizePercent) div 100;
        FPos := ((FRect.Bottom - FRect.Top) div 2) - (FHotZoneRectSize div 2);
        Result := Rect(FRect.Left, FPos, FRect.Right, FPos + FHotZoneRectSize);
        FLTPointsRect := Rect(Result.Left + 1, Result.Top, (Result.Right div 2), Result.Bottom);
        FRBPointsRect := Rect((Result.Right div 2) + 1, Result.Top + 3, Result.Right, Result.Bottom);
      end;
  else
    begin
      FRect.Bottom := FRect.Top + ScaleFactor.Apply(SplitterDefaultSize) - 1;
      FHotZoneRectSize := ((FRect.Right - FRect.Left) * SizePercent) div 100;
      FPos := ((FRect.Right - FRect.Left) div 2) - (FHotZoneRectSize div 2);
      Result := Rect(FPos, FRect.Top, FPos + FHotZoneRectSize, FRect.Bottom);
      FLTPointsRect := Rect(Result.Left, Result.Top + 1, Result.Right, Result.Bottom div 2);
      FRBPointsRect := Rect(Result.Left + 3, (Result.Bottom div 2) + 1, Result.Right, Result.Bottom);
    end;
  end;
  HotZoneRect := Result;
end;

procedure TcxXPTaskBarStyle.DrawBackground(ACanvas: TcxCanvas; const ARect: TRect;
  const AHighlighted, AClicked: Boolean);
begin
  if AreVisualStylesAvailable and Owner.NativeBackground then
    cxDrawThemeParentBackground(Owner, ACanvas, ARect)
  else
  begin
    ACanvas.Brush.Color := Owner.Color;
    ACanvas.FillRect(ARect);
  end;
end;
{ TcxXPTaskBarStyle }

{ TcxSimpleStyle }
constructor TcxSimpleStyle.Create(AOwner: TcxCustomSplitter);
begin
  inherited Create(AOwner);
  FLightColor := clWindow;
  FShadowColor := clBtnShadow;
  FArrowColor := clWindowText;
  FArrowHighlightColor := clWindow;
  FDotsColor := clHighlight;
  FDotsShadowColor := clWindow;
end;

procedure TcxSimpleStyle.Assign(Source: TPersistent);
begin
  if (Source is TcxSimpleStyle) then
  begin
    inherited Assign(Source);
    with (Source as TcxSimpleStyle) do
    begin
      Self.LightColor := LightColor;
      Self.ShadowColor := ShadowColor;
      Self.ArrowColor := ArrowColor;
      Self.ArrowHighlightColor := ArrowHighlightColor;
      Self.DotsColor := DotsColor;
      Self.DotsShadowColor := DotsShadowColor;
    end;
  end
  else
    inherited Assign(Source);
end;

procedure TcxSimpleStyle.SetLightColor(Value: TColor);
begin
  if FLightColor <> Value then
  begin
    FLightColor := Value;
    Changed;
  end;
end;

procedure TcxSimpleStyle.SetShadowColor(Value: TColor);
begin
  if FShadowColor <> Value then
  begin
    FShadowColor := Value;
    Changed;
  end;
end;

procedure TcxSimpleStyle.SetArrowColor(Value: TColor);
begin
  if FArrowColor <> Value then
  begin
    FArrowColor := Value;
    Changed;
  end;
end;

procedure TcxSimpleStyle.SetArrowHighlightColor(Value: TColor);
begin
  if FArrowHighlightColor <> Value then
  begin
    FArrowHighlightColor := Value;
    Changed;
  end;
end;

procedure TcxSimpleStyle.SetDotsColor(Value: TColor);
begin
  if FDotsColor <> Value then
  begin
    FDotsColor := Value;
    Changed;
  end;
end;

procedure TcxSimpleStyle.SetDotsShadowColor(Value: TColor);
begin
  if FDotsShadowColor <> Value then
  begin
    FDotsShadowColor := Value;
    Changed;
  end;
end;

function TcxSimpleStyle.DrawHotZone(ACanvas: TcxCanvas; const ARect: TRect;
  const AHighlighted, AClicked: Boolean): TRect;
begin
  Result := CalculateHotZoneRect(ARect);
  ACanvas.Lock;
  try
    ACanvas.Canvas.Brush.Color := Owner.Color;
    DrawBackground(ACanvas, HotZoneRect, AHighlighted, AClicked);
    DrawHotZoneArrow(ACanvas, FLTArrowRect, AHighlighted, AClicked, FArrowColor,
      FArrowHighlightColor, SplitterDirection, ScaleFactor);
    DrawHotZoneArrow(ACanvas, FRBArrowRect, AHighlighted, AClicked, FArrowColor,
      FArrowHighlightColor, SplitterDirection, ScaleFactor);
  finally
    ACanvas.Unlock;
  end;
end;

function TcxSimpleStyle.CalculateHotZoneRect(const ABounds: TRect): TRect;
begin
  Result := inherited CalculateHotZoneRect(ABounds);
  case SplitterDirection of
    cxsdLeftToRight, cxsdRightToLeft:
      begin
        FLTArrowRect := Rect(Result.Left, Result.Top + ScaleFactor.Apply(5),
          Result.Right, Result.Top + ScaleFactor.Apply(12));
        FRBArrowRect := Rect(Result.Left, Result.Bottom - ScaleFactor.Apply(12),
          Result.Right, Result.Bottom - ScaleFactor.Apply(5));
      end;
  else
    begin
      FLTArrowRect := Rect(Result.Left + ScaleFactor.Apply(5), Result.Top,
        Result.Left + ScaleFactor.Apply(12), Result.Bottom);
      FRBArrowRect := Rect(Result.Right - ScaleFactor.Apply(12), Result.Top,
        Result.Right - ScaleFactor.Apply(5), Result.Bottom);
    end;
  end;
  HotZoneRect := Result;
end;

procedure TcxSimpleStyle.DrawBackground(ACanvas: TcxCanvas; const ARect: TRect; const AHighlighted, AClicked: Boolean);
var
  AColor1, AColor2: TColor;
  ADotRect: TRect;
  ADotSize: Integer;
  FRect: TRect;
  MiddlePos, I_count : Integer;
begin
  with ACanvas, ARect do
  begin
    FRect := DrawBounds(ACanvas, HotZoneRect, ShadowColor, ShadowColor);
    Brush.Color := Owner.Color;
    FillRect(FRect);

    {Draw Border}
    if not AClicked then
      DrawBounds(ACanvas, FRect, LightColor, Owner.Color)
    else
      DrawBounds(ACanvas, FRect, Owner.Color, LightColor);

    if AClicked then
    begin
      AColor1 := DotsColor;
      AColor2 := DotsShadowColor;
    end
    else
    begin
      AColor1 := DotsShadowColor;
      AColor2 := DotsColor;
    end;

    ADotSize := 2 * ScaleFactor.Apply(1);
    if SplitterDirection in [cxsdTopToBottom, cxsdBottomToTop] then
    begin
      MiddlePos := Top + (Bottom - Top - (ADotSize - 1)) div 2;
      for I_count := 0 to (Right - Left - ScaleFactor.Apply(32)) div (ADotSize + 1) do
      begin
        ADotRect := cxRectBounds(Left + ScaleFactor.Apply(16) + I_count * (ADotSize + 1) - 1, MiddlePos, ADotSize - 1, ADotSize - 1);
        FillRect(ADotRect, AColor1);
        ADotRect := cxRectOffset(ADotRect, 1, 1);
        FillRect(ADotRect, AColor2);
      end;
    end
    else
    begin
      MiddlePos := Left + (Right - Left - (ADotSize - 1)) div 2;
      for I_count := 0 to (Bottom - Top - ScaleFactor.Apply(32)) div (ADotSize + 1) do
      begin
        ADotRect := cxRectBounds(MiddlePos, Top + ScaleFactor.Apply(16) + I_count * (ADotSize + 1) - 1, ADotSize - 1, ADotSize - 1);
        FillRect(ADotRect, AColor1);
        ADotRect := cxRectOffset(ADotRect, 1, 1);
        FillRect(ADotRect, AColor2);
      end;
    end;
  end;
end;

{ TdxSplitterDragImage }

procedure TdxSplitterDragImage.Paint;
begin
  Canvas.Canvas.FillRect(ClientRect);
end;

{ TcxCustomSplitter }

constructor TcxCustomSplitter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csSetCaption];
  FResizeUpdate := False;
  FSplitterState := sstNormal;
  FMouseStates := [];
  FAllowHotZoneDrag := True;
  FDragThreshold := 3;
  FResizeIgnoreSnap := False; //deprecated
  FAutoPosition := True;
  FMinSize := 30;
  FPositionAfterOpen := 30;
  FNativeBackground := True;
  FInvertDirection := False;
  FNewSize := 30;
  FOldSize := -1;
  FPositionBeforeClose := FMinSize;
  FHotZone := nil;
  FHotZoneClickPoint := Point(-1, -1);
  FLastPatternDrawPosition := -1;
  FDrawBitmap := TcxBitmap.Create;
  BorderStyle := cxcbsNone;
  FAlignSplitter := salLeft;
  SetBounds(0, 0, SplitterDefaultSize, 100 {//#Must be >=10, otherwise the IDE sets the default size (100x48)});
  TabStop := False;
end;

destructor TcxCustomSplitter.Destroy;
begin
  FControl := nil;
  DestroyHotZone;
  FreeAndNil(FDrawBitmap);
  inherited Destroy;
end;

procedure TcxCustomSplitter.ChangeScaleEx(M, D: Integer; isDpiChange: Boolean);
begin
  inherited;
  MinSize := MulDiv(MinSize, M, D);
  DragThreshold := MulDiv(DragThreshold, M, D);
  PositionAfterOpen := MulDiv(PositionAfterOpen, M, D);
end;

procedure TcxCustomSplitter.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (FControl <> nil) and (AComponent = FControl) then
    FControl := nil;
end;

procedure TcxCustomSplitter.SetHotZoneStyleClass(Value: TcxHotZoneStyleClass);
var
  ASavedHotZone: TcxHotZoneStyle;
begin
  if FHotZoneStyleClass <> Value then
  begin
    ASavedHotZone := nil;
    try
      if Assigned(FHotZone) then
      begin
        ASavedHotZone := TcxHotZoneStyle.Create(Self);
        ASavedHotZone.Assign(FHotZone);
      end;
      DestroyHotZone;
      FHotZoneStyleClass := Value;
      CreateHotZone;
      if Assigned(FHotZone) and Assigned(ASavedHotZone) then
        FHotZone.Assign(ASavedHotZone);
    finally
      if Assigned(ASavedHotZone) then
        FreeAndNil(ASavedHotZone);
    end;
    NormalizeSplitterSize;
  end;
end;

function TcxCustomSplitter.GetHotZoneClassName: string;
begin
  if FHotZone = nil then
    Result := ''
  else
    Result := FHotZone.ClassName;
end;

procedure TcxCustomSplitter.SetHotZoneClassName(Value: string);
begin
  HotZoneStyleClass := TcxHotZoneStyleClass(GetRegisteredHotZoneStyles.FindByClassName(Value));
end;

procedure TcxCustomSplitter.CreateHotZone;
begin
  if FHotZoneStyleClass <> nil then
    FHotZone := FHotZoneStyleClass.Create(Self);
  Invalidate;
end;

procedure TcxCustomSplitter.DestroyHotZone;
begin
  FreeAndNil(FHotZone);
end;

procedure TcxCustomSplitter.SetHotZone(Value: TcxHotZoneStyle);
begin
  FHotZone := Value;
  NormalizeSplitterSize;
  Invalidate;
end;

procedure TcxCustomSplitter.SetNativeBackground(Value: Boolean);
begin
  if FNativeBackground <> Value then
  begin
    FNativeBackground := Value;
    Invalidate;
  end;
end;

procedure TcxCustomSplitter.SetDefaultStates;
begin
  FMouseStates := [];
  FSplitterState := sstNormal;
end;

procedure TcxCustomSplitter.SetDragThreshold(const Value: TcxNaturalNumber);
begin
  FDragThreshold := Max(Min(Value, High(TcxNaturalNumber)), Low(TcxNaturalNumber));
end;

procedure TcxCustomSplitter.SetMinSize(const Value: TcxNaturalNumber);
begin
  FMinSize := Max(Min(Value, High(TcxNaturalNumber)), Low(TcxNaturalNumber));
end;

procedure TcxCustomSplitter.SetPositionAfterOpen(const Value: TcxPositionAfterOpen);
begin
  FPositionAfterOpen := Max(Min(Value, High(TcxPositionAfterOpen)), Low(TcxPositionAfterOpen));
end;

function TcxCustomSplitter.GetMaxControlSize: Integer;
begin
  Result := 0;
  if FCurrentControl = nil then
    Exit;
  case Align of
    alBottom, alTop:
      Result := FCurrentControl.Constraints.MaxHeight;
    alLeft, alRight:
      Result := FCurrentControl.Constraints.MaxWidth;
  end;
end;

function TcxCustomSplitter.IsControlActualForSplitter(AControl: TControl): Boolean;
begin
  Result := (AControl <> nil) and (IsDesigning or AControl.Visible) and (AControl.Align in [alTop, alBottom, alLeft, alRight]);
end;

function TcxCustomSplitter.IsAllControlHotZoneStyle: Boolean;
begin
  Result := LookAndFeel.SkinPainter <> nil;
end;

function TcxCustomSplitter.GetDragImageTopLeft: TPoint;
begin
  Result := cxGetWindowRect(Handle).TopLeft;
  if Align in [alLeft, alRight] then
    Result.X := Result.X + FSplit + 1
  else
    Result.Y := Result.Y + FSplit + 1;
end;

procedure TcxCustomSplitter.InitDragImage;
begin
  if not ResizeUpdate then
  begin
    FDragImage := TdxSplitterDragImage.Create;
    FDragImage.Canvas.Brush.Bitmap := AllocPatternBitmap(clBlack, clWhite);
    FDragImage.SetBounds(GetDragImageTopLeft.X, GetDragImageTopLeft.Y, Width - 1, Height - 1);
    FDragImage.Show;
  end;
end;

procedure TcxCustomSplitter.MoveDragImage;
begin
  FDragImage.MoveTo(GetDragImageTopLeft);
end;

procedure TcxCustomSplitter.ReleaseDragImage;
begin
  FreeAndNil(FDragImage);
end;

procedure TcxCustomSplitter.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if smsInHotZone in FMouseStates then
    Invalidate;
  Exclude(FMouseStates, smsInHotZone);
end;

procedure TcxCustomSplitter.DXMAdjustPosition(var Message: TMessage);
begin
  CorrectSelfPosition;
end;

procedure TcxCustomSplitter.SetAlignSplitter(Value: TcxSplitterAlign);
begin
  FAlignSplitter := Value;
  ClearConstraints;
  AdjustAlignSettings;
end;

function TcxCustomSplitter.GetSplitterMinSize: TcxNaturalNumber;
var
  AHorizontal: Boolean;
begin
  if LookAndFeel.SkinPainter <> nil then
  begin
    AHorizontal := Align in [alBottom, alTop];
    if AHorizontal then
      Result := LookAndFeel.SkinPainter.GetScaledSplitterSize(AHorizontal, ScaleFactor).cy
    else
      Result := LookAndFeel.SkinPainter.GetScaledSplitterSize(AHorizontal, ScaleFactor).cx;
  end
  else
    if Assigned(FHotZone) then
      Result := FHotZone.GetMinSize
    else
      Result := ScaleFactor.Apply(SplitterDefaultSize);
  dxAdjustToTouchableSize(Integer(Result), ScaleFactor);
end;

function TcxCustomSplitter.GetSplitterMaxSize: TcxNaturalNumber;
var
  AHorizontal: Boolean;
begin
  if LookAndFeel.SkinPainter <> nil then
  begin
    AHorizontal := Align in [alBottom, alTop];
    if AHorizontal then
      Result := LookAndFeel.SkinPainter.GetScaledSplitterSize(AHorizontal, ScaleFactor).cy
    else
      Result := LookAndFeel.SkinPainter.GetScaledSplitterSize(AHorizontal, ScaleFactor).cx;
  end
  else
    if Assigned(FHotZone) then
      Result := FHotZone.GetMaxSize
    else
      Result := ScaleFactor.Apply(SplitterDefaultSize);
  dxAdjustToTouchableSize(Integer(Result), ScaleFactor);
end;

procedure TcxCustomSplitter.NormalizeSplitterSize;

  procedure AdjustSplitterSize;
  begin
    case Align of
      alBottom, alTop:
        Height := GetSplitterMinSize;
      alLeft, alRight:
        Width := GetSplitterMinSize;
    end;
  end;

  procedure SetupConstraints;
  begin
    case Align of
      alBottom, alTop:
        begin
          Constraints.MinWidth := 0;
          Constraints.MaxWidth := 0;
          Constraints.MinHeight := GetSplitterMinSize;
          Constraints.MaxHeight := GetSplitterMaxSize;
          if (Height < Constraints.MinHeight) or (Height > Constraints.MaxHeight) then
            Height := Constraints.MinHeight;
        end;

      alLeft, alRight:
        begin
          Constraints.MinWidth := GetSplitterMinSize;
          Constraints.MaxWidth := GetSplitterMaxSize;
          Constraints.MinHeight := 0;
          Constraints.MaxHeight := 0;
          if (Width < Constraints.MinWidth) or (Width > Constraints.MaxWidth) then
            Width := Constraints.MinWidth;
        end;
    end;
  end;

begin
  ClearConstraints;
  if (FHotZone <> nil) or (LookAndFeel.SkinPainter <> nil) then
  begin
    AdjustSplitterSize;
    SetupConstraints;
  end;
end;

function TcxCustomSplitter.CalculateSplitterDirection: TcxSplitterDirection;
var
  AInvertDirection: Boolean;
begin
  Result := Low(TcxSplitterDirection);
  AInvertDirection := InvertDirection xor dxWindowHasRightToLeftLayout(Parent.Handle);
  case Align of
    alTop:
      if ((State = ssOpened) and (not InvertDirection)) or ((State = ssClosed) and (InvertDirection)) then
        Result := cxsdBottomToTop
      else
        Result := cxsdTopToBottom;

    alBottom:
      if ((State = ssOpened) and (not InvertDirection)) or ((State = ssClosed) and (InvertDirection)) then
        Result := cxsdTopToBottom
      else
        Result := cxsdBottomToTop;

    alLeft:
      if ((State = ssOpened) and (not AInvertDirection)) or ((State = ssClosed) and (AInvertDirection)) then
        Result := cxsdRightToLeft
      else
        Result := cxsdLeftToRight;

    alRight:
      if ((State = ssOpened) and (not AInvertDirection)) or ((State = ssClosed) and (AInvertDirection)) then
        Result := cxsdLeftToRight
      else
        Result := cxsdRightToLeft;
  end;
end;

procedure TcxCustomSplitter.UpdateMouseStates(X, Y: Integer);
begin
  if IsPointInHotZone(X, Y) then
    Include(FMouseStates, smsInHotZone)
  else
    Exclude(FMouseStates, smsInHotZone);
end;

procedure TcxCustomSplitter.SetSplitterState(Value: TcxSplitterState);
begin
  if FState <> Value then
  begin
    if FCurrentControl <> nil then
    begin
      case Value of
        ssOpened:
          InternalOpenSplitter;
        ssClosed:
          InternalCloseSplitter;
      end;
    end
    else
      FState := Value;
  end;
end;

procedure TcxCustomSplitter.SetAllowHotZoneDrag(Value: Boolean);
begin
  if FAllowHotZoneDrag <> Value then
  begin
    StopSizing;
    FAllowHotZoneDrag := Value;
  end;
end;

procedure TcxCustomSplitter.SetControl(Value: TControl);

  function CanAssign: Boolean;
  var
    I: Integer;
  begin
    Result := not ((Value is TcxCustomSplitter) or (Value is TSplitter));
    if Result then
    begin
      if Value.Parent <> nil then
        for I := 0 to Value.Parent.ControlCount - 1 do
        begin
          Result := not ((Value.Parent.Controls[I] is TcxCustomSplitter) and
            (TcxCustomSplitter(Value.Parent.Controls[I]).Control = Value));
          if not Result then
            raise EdxException.Create('The specified control is already bound to another TcxSplitter control.');
        end
    end
    else
      raise EdxException.Create('The splitter cannot be bound to another splitter.');
  end;

begin
  if (Value = nil) or (FControl <> Value) and CanAssign then
  begin
    FControl := Value;
    AdjustAlignSettings;
  end;
end;

procedure TcxCustomSplitter.SetInvertDirection(Value: Boolean);
begin
  if FInvertDirection <> Value then
  begin
    FInvertDirection := Value;
    StopSizing;
    Invalidate;
  end;
end;

function TcxCustomSplitter.FindControl(AAlign: TAlign): TControl;

  function GetSplitterOffsets: TRect;
  begin
    Result := cxRect(1, 1, Width, Height);
    if AlignWithMargins then
      Result := cxRectTransform(Result, Margins.Left, Margins.Top, Margins.Right + 1, Margins.Bottom + 1);
  end;

  function GetControlOffsets(AControl: TControl): TRect;
  begin
    Result := cxNullRect;
    if AControl.AlignWithMargins then
      Result := cxRectTransform(Result, AControl.Margins.Left, AControl.Margins.Top, AControl.Margins.Right, AControl.Margins.Bottom);
  end;

  function GetNearestControl(AControl1, AControl2: TControl): TControl;
  begin
    Result := AControl2;
    if AControl1 <> nil then
    begin
      case AAlign of
        alLeft:
          if AControl1.Left > Result.Left then
            Result := AControl1;
        alRight:
          if AControl1.Left + AControl1.Width < Result.Left + Result.Width then
            Result := AControl1;
        alTop:
          if AControl1.Top > Result.Top then
            Result := AControl1;
        alBottom:
          if AControl1.Top + AControl1.Height < Result.Top + Result.Height then
            Result := AControl1;
      end;
    end;
  end;

var
  P: TPoint;
  I: Integer;
  R, AOffsets: TRect;
  ANextControl: TControl;
begin
  Result := nil;
  if Parent = nil then
    Exit;

  P := Point(Left, Top);
  AOffsets := GetSplitterOffsets;
  case AAlign of
    alLeft:
      Dec(P.X, AOffsets.Left);
    alRight:
      Inc(P.X, AOffsets.Right);
    alTop:
      Dec(P.Y, AOffsets.Top);
    alBottom:
      Inc(P.Y, AOffsets.Bottom);
  else
    Exit;
  end;
  for I := 0 to Parent.ControlCount - 1 do
  begin
    ANextControl := Parent.Controls[I];
    if (ANextControl <> Self) and ANextControl.Visible and ANextControl.Enabled then
    begin
      R := cxRectInflate(ANextControl.BoundsRect, GetControlOffsets(ANextControl));
      if cxRectWidth(R) = 0 then
        if AAlign in [alTop, alLeft] then
          Dec(R.Left)
        else
          Inc(R.Right);
      if cxRectHeight(R) = 0 then
        if AAlign in [alTop, alLeft] then
          Dec(R.Top)
        else
          Inc(R.Bottom);

      if (ANextControl.Align = AAlign) then
      begin
        if cxRectPtInEx(R, P.X, P.Y, 0, 0, 1, 1) <> ptrtNone then
          Result := GetNearestControl(Result, ANextControl)
        else
        begin
          R := ANextControl.BoundsRect;
          case AAlign of
            alLeft:
              if (R.Right = R.Left) and (R.Left = Left + Width) then Result := GetNearestControl(Result, ANextControl);
            alRight:
              if (R.Right = R.Left) and (R.Right = Left) then Result := GetNearestControl(Result, ANextControl);
            alTop:
              if (R.Bottom = R.Top) and (R.Top = Top + Height) then Result := GetNearestControl(Result, ANextControl);
            alBottom:
              if (R.Bottom = R.Top) and (R.Bottom = Top) then Result := GetNearestControl(Result, ANextControl);
          end;
        end;
      end;
    end;
  end;
end;

procedure TcxCustomSplitter.HotZoneStyleChanged;
begin
  Invalidate;
end;

procedure TcxCustomSplitter.Paint;
var
  R: TRect;
begin
  R := ClientRect;
  FDrawBitmap.SetSize(R);

  if LookAndFeel.SkinPainter = nil then
  begin
    if AreVisualStylesAvailable and NativeBackground then
    begin
      cxDrawThemeParentBackground(Self, Canvas, R);
      cxBitBlt(FDrawBitmap.Canvas.Handle, Canvas.Handle, FDrawBitmap.ClientRect, cxNullPoint, SRCCOPY);
    end
    else
      FDrawBitmap.cxCanvas.FillRect(R, Color);
    if (HotZone <> nil) and HotZone.Visible then
      DrawHotZone;
  end
  else
  begin
    cxDrawTransparentControlBackground(Self, FDrawBitmap.cxCanvas, R);
    LookAndFeel.SkinPainter.DrawScaledSplitter(FDrawBitmap.cxCanvas, R, smsInHotZone in FMouseStates,
      (smsClicked in FMouseStates) and (smsInHotZone in FMouseStates), Align in [alBottom, alTop], ScaleFactor);
  end;
  cxDrawBitmap(Canvas.Handle, FDrawBitmap, R, cxNullPoint);
end;

procedure TcxCustomSplitter.DrawHotZone;
begin
  if HotZone <> nil then
    HotZone.DrawHotZone(FDrawBitmap.cxCanvas, FDrawBitmap.cxCanvas.Canvas.ClipRect,
      (smsInHotZone in FMouseStates), (smsClicked in FMouseStates) and (smsInHotZone in FMouseStates));
end;

function TcxCustomSplitter.DoCanResize(var NewSize: Integer): Boolean;
begin
  Result := CanResize(NewSize);
  if Result and AutoSnap and (NewSize < InternalGetMinSize) then
    NewSize := 0;
end;

procedure TcxCustomSplitter.LookAndFeelChanged(Sender: TcxLookAndFeel;
  AChangedValues: TcxLookAndFeelValues);
begin
  inherited LookAndFeelChanged(Sender, AChangedValues);
  NormalizeSplitterSize;
  if [lfvSkinName, lfvNativeStyle] * AChangedValues <> [] then
    Invalidate;
end;

function TcxCustomSplitter.CanFocusOnClick: Boolean;
begin
  Result := False;
end;

function TcxCustomSplitter.CanResize(var NewSize: Integer): Boolean;
begin
  Result := True;
  if Assigned(FOnCanResize) then
    FOnCanResize(Self, NewSize, Result);
end;

procedure TcxCustomSplitter.UpdateControlSize;
begin
  if (FCurrentControl = nil) or (FNewSize = FOldSize) then
    Exit;

  Parent.DisableAlign;
  try
    case Align of
      alLeft:
        FCurrentControl.SetBounds(FCurrentControl.Left, FCurrentControl.Top, FNewSize, FCurrentControl.Height);
      alTop:
        FCurrentControl.SetBounds(FCurrentControl.Left, FCurrentControl.Top, FCurrentControl.Width, FNewSize);
      alRight:
        FCurrentControl.SetBounds(FCurrentControl.Left + FCurrentControl.Width - FNewSize, FCurrentControl.Top, FNewSize, FCurrentControl.Height);
      alBottom:
        FCurrentControl.SetBounds(FCurrentControl.Left, FCurrentControl.Top + FCurrentControl.Height - FNewSize, FCurrentControl.Width, FNewSize);
    end;
  finally
    Parent.EnableAlign;
  end;
  FOldSize := FNewSize;
  DoEventMoved;
end;

procedure TcxCustomSplitter.AdjustAlignSettings(APostponedCorrection: Boolean);

  function LeavePrevControl(ANewCurrentControl: TControl; ANewAlign: TAlign): Boolean;
  begin
    Result := (FSplitterState <> sstNormal) or (ANewCurrentControl = nil)
      and IsControlActualForSplitter(FCurrentControl) and
      (((ANewAlign in [alLeft, alRight]) and (FCurrentControl.Width = 0))
      or ((ANewAlign in [alTop, alBottom]) and (FCurrentControl.Height = 0)));
  end;

var
  ANewAlign: TAlign;
  ANewCurrentControl: TControl;
begin
  if FIsAdjustingAlign or FIsAdjustingPosition then
    Exit;

  FIsAdjustingAlign := True;
  try
    if IsControlActualForSplitter(Control) then
    begin
      ANewAlign := Control.Align;
      ANewCurrentControl := Control;
    end
    else
    begin
      if (Control <> nil) and (Control.Align in [alTop, alBottom, alLeft, alRight]) then
        ANewAlign := Control.Align
      else
      begin
        case AlignSplitter of
          salBottom: ANewAlign := alBottom;
          salLeft: ANewAlign := alLeft;
          salRight: ANewAlign := alRight;
        else {alTop}
          ANewAlign := alTop;
        end;
      end;
      ANewCurrentControl := FindControl(ANewAlign);
      if LeavePrevControl(ANewCurrentControl, ANewAlign) then
        ANewCurrentControl := FCurrentControl
      else
        ANewCurrentControl := FindControl(ANewAlign);
    end;
    if (ANewAlign <> Align) or (ANewCurrentControl <> FCurrentControl) then
    begin
      ClearConstraints;
      FCurrentControl := ANewCurrentControl;
      Align := ANewAlign;
      if Assigned(FHotZone) then
        NormalizeSplitterSize;
    end;
    if APostponedCorrection and HandleAllocated then
      PostMessage(Handle, DXM_ADJUSTPOSITION, 0, 0)
    else
      CorrectSelfPosition;
  finally
    FIsAdjustingAlign := False;
  end;
end;

procedure TcxCustomSplitter.CalcSplitSize(X, Y: Integer; var ANewSize, ASplit: Integer; ACorrectWithMaxMin: Boolean = True);
var
  S: Integer;
  ADelta: Integer;
  AIsParentRightToLeft: Boolean;
begin
  if FCurrentControl = nil then
    Exit;

  if Align in [alLeft, alRight] then
    ASplit := X - FSplitterClickPoint.X
  else
    ASplit := Y - FSplitterClickPoint.Y;

  AIsParentRightToLeft := dxWindowHasRightToLeftLayout(Parent.Handle);

  if AIsParentRightToLeft then
    ADelta := -ASplit
  else
    ADelta := ASplit;

  case Align of
    alLeft:
      S := FCurrentControl.Width + ADelta;
    alRight:
      S := FCurrentControl.Width - ADelta;
    alTop:
      S := FCurrentControl.Height + ASplit;
    alBottom:
      S := FCurrentControl.Height - ASplit;
  else
    S := 0;
  end;

  ANewSize := S;
  if ACorrectWithMaxMin then
  begin
    ANewSize := Max(ANewSize, 0);

    if S < InternalGetMinSize then
    begin
      if AutoSnap then
        ANewSize := 0
      else
        ANewSize := InternalGetMinSize;
    end
    else
      if (S > FMaxSize) and (FMaxSize > 0) then
        ANewSize := FMaxSize;

    if S <> ANewSize then
    begin
      if Align in [alRight, alBottom] then
        S := S - ANewSize
      else
        S := ANewSize - S;
      if (Align in [alLeft, alRight]) and AIsParentRightToLeft then
        S := -S;
      Inc(ASplit, S);
    end;
  end;
end;

procedure TcxCustomSplitter.ClearConstraints;
begin
  Constraints.MinWidth := 0;
  Constraints.MaxWidth := 0;
  Constraints.MinHeight := 0;
  Constraints.MaxHeight := 0;
end;

procedure TcxCustomSplitter.ControlResizing(X, Y: Integer);

  procedure UpdateStateAfterMoving(ANewSize: Integer);
  begin
    if (ANewSize < MinSize) and (ANewSize <> FOldSize) then
    begin
      if (State = ssOpened) and not AutoSnap then
        FNewSize := MinSize
      else
        case State of
          ssClosed:
            if ANewSize >= 0 then
              InternalOpenSplitter(ANewSize)
            else
              FNewSize := 0;
          ssOpened:
            InternalCloseSplitter;
        end;
    end
    else
      if ANewSize > 0 then
        InternalOpenSplitter(ANewSize);
  end;

begin
  FLastPatternDrawPosition := -1;
  ParentShowHint := FSavedParentShowHint;
  ShowHint := FSavedShowHint;
  FHotZoneClickPoint := cxInvalidPoint;

  case FSplitterState of
    sstHotZoneClick:
      if smsInHotZone in FMouseStates then
      begin
        UpdateMouseStates(X, Y);
        case FState of
          ssClosed:
            InternalOpenSplitter;
          ssOpened:
            InternalCloseSplitter;
        end;
      end;
    sstResizing:
      begin
        StopSizing;
        UpdateStateAfterMoving(FNewSize);
        UpdateControlSize;
        if FNewSize >= 0 then
          RecalcLastPosition;
      end;
  end;
  SetDefaultStates;
  Invalidate;
end;

procedure TcxCustomSplitter.CorrectSelfPosition;

  function GetMargin: Integer;
  begin
    Result := 0;
    if AlignWithMargins then
      case Align of
        alLeft:
          Result := Margins.Left;
        alTop:
          Result := Margins.Top;
        alRight:
          Result := Margins.Right;
        alBottom:
          Result := Margins.Bottom;
      end;

    if FCurrentControl.AlignWithMargins then
      case FCurrentControl.Align of
        alLeft:
          Result := Result + FCurrentControl.Margins.Right;
        alTop:
          Result := Result + FCurrentControl.Margins.Bottom;
        alRight:
          Result := Result + FCurrentControl.Margins.Left;
        alBottom:
          Result := Result + FCurrentControl.Margins.Top;
      end;
  end;

  function CanAdjustPosition: Boolean;
  begin
    Result := IsControlActualForSplitter(FCurrentControl) and not (csLoading in ComponentState) and
      not FIsAdjustingPosition and (FCurrentControl.Parent = Parent);
  end;

var
  ANewPosition: Integer;
begin
  if not CanAdjustPosition then
    Exit;

  FIsAdjustingPosition := True;
  try
    case Align of
      alLeft:
        begin
          ANewPosition := FCurrentControl.Left + FCurrentControl.Width + GetMargin;
          if (ANewPosition <> Left) and (FCurrentControl.Width > 1) then
          begin
            Left := ANewPosition - 1;
            RequestAlign;
          end;
          FOldSize := FCurrentControl.Width;
        end;
      alTop:
        begin
          ANewPosition := FCurrentControl.Top + FCurrentControl.Height + GetMargin;
          if (ANewPosition <> Top) and (FCurrentControl.Height > 1) then
          begin
            Top := ANewPosition - 1;
            RequestAlign;
          end;
          FOldSize := FCurrentControl.Height;
        end;
      alRight:
        begin
          ANewPosition := FCurrentControl.Left - Width - GetMargin;
          if (ANewPosition <> Left) and (FCurrentControl.Width > 1) then
          begin
            Left := ANewPosition + 1;
            RequestAlign;
          end;
          FOldSize := FCurrentControl.Width;
        end;
      alBottom:
        begin
          ANewPosition := FCurrentControl.Top - Height - GetMargin;
          if (ANewPosition <> Top) and (FCurrentControl.Height > 1) then
          begin
            Top := ANewPosition + 1;
            RequestAlign;
          end;
          FOldSize := FCurrentControl.Height;
        end;
    end;
  finally
    FIsAdjustingPosition := False;
  end;
end;

procedure TcxCustomSplitter.UpdateSize(X, Y: Integer);
begin
  CalcSplitSize(X, Y, FNewSize, FSplit);
end;

function TcxCustomSplitter.IsPointInHotZone(const X, Y: Integer): Boolean;
begin
  if not IsAllControlHotZoneStyle then
  begin
    if HotZone <> nil then
      Result := cxRectPtIn(HotZone.CalculateHotZoneRect(ClientRect), X, Y)
    else
      Result := False;
  end
  else
    Result := IsPointInSplitter(X ,Y);
end;

function TcxCustomSplitter.IsPointInSplitter(const X, Y: Integer): Boolean;
begin
  Result := cxRectPtIn(ClientRect, X, Y);
end;

procedure TcxCustomSplitter.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
  AdjustAlignSettings(True);
end;

procedure TcxCustomSplitter.InitResize(X, Y: Integer);

  function GetMaxSize: Integer;
  var
    AMaxControlSize: Integer;
    I: Integer;
  begin
    Result := 0;
    case Align of
     alLeft, alRight: Result := Parent.ClientWidth;
     alTop, alBottom: Result := Parent.ClientHeight;
    end;
    for I := 0 to Parent.ControlCount - 1 do
      if (Parent.Controls[I] <> FCurrentControl) and Parent.Controls[I].Visible then
      begin
        if (Align in [alLeft, alRight]) and (Parent.Controls[I].Align in [alLeft, alRight]) then
          Dec(Result, Parent.Controls[I].Width)
        else
          if (Align in [alTop, alBottom]) and (Parent.Controls[I].Align in [alTop, alBottom]) then
            Dec(Result, Parent.Controls[I].Height);
      end;

    Dec(Result, FMinSize);
    AMaxControlSize := GetMaxControlSize;
    if AMaxControlSize <> 0 then
      Result := Min(Result, AMaxControlSize);
  end;

begin
  FMaxSize := GetMaxSize;
  UpdateSize(X, Y);
  InitDragImage;
  with ValidParentForm(Self) do
    if ActiveControl <> nil then
    begin
      FActiveControl := ActiveControl;
      FPrevKeyDown := TWinControlAccess(FActiveControl).OnKeyDown;
      TWinControlAccess(FActiveControl).OnKeyDown := FocusKeyDown;
    end;
  if not ResizeUpdate then
    MoveDragImage;
end;

procedure TcxCustomSplitter.InternalOpenSplitter(ANewSize: Integer = 0);
var
  AAllowOpenHotZone: Boolean;
begin
  if State = ssOpened then Exit;
  if ANewSize = 0 then
  begin
    if FAutoPosition then
      ANewSize := FPositionBeforeClose
    else
      ANewSize := PositionAfterOpen;
  end;
  ANewSize := Max(ANewSize, InternalGetMinSize);
  AAllowOpenHotZone := True;
  DoEventBeforeOpen(ANewSize, AAllowOpenHotZone);
  if AAllowOpenHotZone then
  begin
    FState := ssOpened;
    FNewSize := ANewSize;
    RecalcLastPosition;
    UpdateControlSize;
    DoEventAfterOpen;
    Invalidate;
  end;
end;

procedure TcxCustomSplitter.InternalCloseSplitter;
var
  FAllowCloseHotZone: Boolean;
begin
  if State = ssClosed then Exit;
  FAllowCloseHotZone := True;
  DoEventBeforeClose(FAllowCloseHotZone);
  if FAllowCloseHotZone then
  begin
    FState := ssClosed;
    FNewSize := 0;
    RecalcLastPosition;
    UpdateControlSize;
    DoEventAfterClose;
    Invalidate;
  end;
end;

procedure TcxCustomSplitter.WMCancelMode(var Message: TWMCancelMode);
begin
  inherited;
  if FSplitterState = sstResizing then
    ControlResizing(Left, Top);
end;

procedure TcxCustomSplitter.WMSetCursor(var Message: TWMSetCursor);
begin
end;

procedure TcxCustomSplitter.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  if (Button = mbLeft) and (FCurrentControl <> nil) then
  begin
    FSplitterClickPoint := Point(X, Y);
    FSavedShowHint := ShowHint;
    FSavedParentShowHint := ParentShowHint;
    ShowHint := False;
    Include(FMouseStates, smsClicked);
    UpdateMouseStates(X, Y);
    if (smsInHotZone in FMouseStates) then
    begin
      FSplitterState := sstHotZoneClick;
      FHotZoneClickPoint := Point(X, Y);
      Invalidate;
    end
    else
    begin
      FSplitterState := sstResizing;
      InitResize(X, Y);
    end;
  end;
end;

procedure TcxCustomSplitter.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  ACursor: TCursor;
  ANewSize: Integer;
  ASavedMouseStates: TcxSplitterMouseStates;
begin
  inherited MouseMove(Shift, X, Y);
  ASavedMouseStates := FMouseStates;
  UpdateMouseStates(X, Y);
  if (ssLeft in Shift) and (FCurrentControl <> nil) then
  begin
    CalcSplitSize(X, Y, ANewSize, FSplit);
    case FSplitterState of
      sstResizing:
        if DoCanResize(ANewSize) then
        begin
          FNewSize := ANewSize;
          if ResizeUpdate then
          begin
            RecalcLastPosition;
            UpdateControlSize;
          end
          else
            MoveDragImage;
        end;

      sstHotZoneClick:
        if AllowHotZoneDrag then
        begin
          if (Max(Abs(FHotZoneClickPoint.Y - Y), Abs(FHotZoneClickPoint.X - X)) >= DragThreshold) and DoCanResize(ANewSize) then
          begin
            FSplitterState := sstResizing;
            InitResize(X, Y);
          end;
        end
        else
          if FMouseStates <> ASavedMouseStates then
            Invalidate;
    end;
  end;

  if Shift * [ssLeft, ssRight] = [] then
  begin
    if FMouseStates <> ASavedMouseStates then
      Invalidate;
    if (smsInHotZone in FMouseStates) and not IsAllControlHotZoneStyle then
      ACursor := crDefault
    else
    begin
      ACursor := Cursor;
      if ACursor = crDefault then
        if Align in [alBottom, alTop] then
          ACursor := crVSplit
        else
          ACursor := crHSplit;
    end;
    SetCursor(Screen.Cursors[ACursor]);
  end;
end;

procedure TcxCustomSplitter.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if (Button = mbLeft) and (FCurrentControl <> nil) and (smsClicked in FMouseStates) then
    ControlResizing(X, Y);
end;

procedure TcxCustomSplitter.FocusKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_ESCAPE) then
  begin
    StopSizing;
    SetDefaultStates;
  end
  else
    if Assigned(FPrevKeyDown) then FPrevKeyDown(Sender, Key, Shift);
end;

procedure TcxCustomSplitter.StopSizing;
var
  AMousePoint: TPoint;
begin
  if FCurrentControl <> nil then
  begin
    ReleaseDragImage;
    AMousePoint := GetMouseCursorClientPos;
    UpdateMouseStates(AMousePoint.X, AMousePoint.Y);
    if Assigned(FActiveControl) then
    begin
      TWinControlAccess(FActiveControl).OnKeyDown := FPrevKeyDown;
      FActiveControl := nil;
    end;
    FSplitterState := sstNormal;
  end;
end;

procedure TcxCustomSplitter.OpenSplitter;
begin
  InternalOpenSplitter;
end;

procedure TcxCustomSplitter.CloseSplitter;
begin
  InternalCloseSplitter;
end;

procedure TcxCustomSplitter.RecalcLastPosition;
begin
  if IsControlActualForSplitter(FCurrentControl) then
    case Align of
      alBottom, alTop:
        FPositionBeforeClose := FCurrentControl.Height;
      alLeft, alRight:
        FPositionBeforeClose := FCurrentControl.Width;
    end;
end;

procedure TcxCustomSplitter.DoEventBeforeOpen(var ANewSize: Integer; var AllowOpenHotZone: Boolean);
begin
  if Assigned(FOnBeforeOpen) then
    FOnBeforeOpen(Self, ANewSize, AllowOpenHotZone);
end;

procedure TcxCustomSplitter.DoEventAfterOpen;
begin
  CallNotify(FOnAfterOpen, Self);
end;

procedure TcxCustomSplitter.DoEventBeforeClose(var AllowCloseHotZone: Boolean);
begin
  if Assigned(FOnBeforeClose) then
    FOnBeforeClose(Self, AllowCloseHotZone);
end;

procedure TcxCustomSplitter.DoEventAfterClose;
begin
  CallNotify(FOnAfterClose, Self);
end;

procedure TcxCustomSplitter.DoEventMoved;
begin
  CallNotify(FOnMoved, Self);
end;

function TcxCustomSplitter.InternalGetMinSize: Integer;
var
  AMinSizeConstraints: Integer;
begin
  Result := FMinSize;
  if FCurrentControl = nil then
    Exit;
  case Align of
    alBottom, alTop:
      AMinSizeConstraints := FCurrentControl.Constraints.MinHeight;
    alLeft, alRight:
      AMinSizeConstraints := FCurrentControl.Constraints.MinWidth;
  else
    AMinSizeConstraints := 0;
  end;
  if AMinSizeConstraints > FMinSize then
    Result := AMinSizeConstraints;
end;

initialization
  GetRegisteredHotZoneStyles.Register(TcxMediaPlayer8Style, scxHotZoneStyleMediaPlayer8);
  GetRegisteredHotZoneStyles.Register(TcxMediaPlayer9Style, scxHotZoneStyleMediaPlayer9);
  GetRegisteredHotZoneStyles.Register(TcxXPTaskBarStyle, scxHotZoneStyleXPTaskBar);
  GetRegisteredHotZoneStyles.Register(TcxSimpleStyle, scxHotZoneStyleSimple);

finalization
  GetRegisteredHotZoneStyles.Unregister(TcxMediaPlayer8Style);
  GetRegisteredHotZoneStyles.Unregister(TcxMediaPlayer9Style);
  GetRegisteredHotZoneStyles.Unregister(TcxXPTaskBarStyle);
  GetRegisteredHotZoneStyles.Unregister(TcxSimpleStyle);
  FreeAndNil(FRegisteredHotZoneStyles);

end.
