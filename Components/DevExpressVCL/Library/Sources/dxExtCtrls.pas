{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressPrinting System                                   }
{                                                                    }
{           Copyright (C) 1998-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSPRINTING SYSTEM AND            }
{   ALL ACCOMPANYING VCL CONTROLS AS PART OF AN                      }
{   EXECUTABLE PROGRAM ONLY.                                         }
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

unit dxExtCtrls;

interface

{$I cxVer.inc}

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Windows, Messages, Classes, Graphics, Controls, StdCtrls, ExtCtrls, ComCtrls,
  ImgList, Menus, Forms, Dialogs, dxCore, cxDropDownEdit, cxGraphics, cxGeometry,
  cxControls, cxSpinEdit;

type
  { TdxPSBrushStyleCombo }

  TdxGetBrushStyleNameEvent = procedure(Sender: TObject; Index: Integer;
    AStyle: TBrushStyle; var AName: string) of object;

  TdxPSBrushStyleCombo = class(TcxCustomComboBox)
  private
    FBrushColor: TColor;
    FEndEllipsis: Boolean;
    FShowStyleName: Boolean;
    FOnGetBrushStyleName: TdxGetBrushStyleNameEvent;
    function GetStyle(Index: Integer): TBrushStyle;
    function GetStyleIndex(Style: TBrushStyle): Integer;
    function GetStyleValue: TBrushStyle;
    procedure SetBrushColor(Value: TColor);
    procedure SetEndEllipsis(Value: Boolean);
    procedure SetShowStyleName(Value: Boolean);
    procedure SetStyleValue(Value: TBrushStyle);
    procedure RefreshItems;
    procedure ResetItemHeight;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMRecreateWnd(var Message: TMessage); message CM_RECREATEWND;
  protected
    procedure BiDiModeChanged; override;
    procedure CalculateRects(const R: TRect; out ABrushRect, ATextRect: TRect);
    procedure CreateWnd; override;
    procedure DoDrawItem(AControl: TcxCustomComboBox; ACanvas: TcxCanvas;
      AIndex: Integer; const ARect: TRect; AState: TOwnerDrawState);
    procedure DoGetStyleName(Index: Integer; var AName: string); virtual;
    procedure EnabledChanged; override;
    function GetStyleName_(Index: Integer): string; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    //
    property StyleIndexes[Style: TBrushStyle]: Integer read GetStyleIndex;
    property StyleNames[Index: Integer]: string read GetStyleName_;
    property Styles[Index: Integer]: TBrushStyle read GetStyle;
  published
    property Anchors;
    property BiDiMode;
    property BrushColor: TColor read FBrushColor write SetBrushColor default clWindowText;
    property BrushStyle: TBrushStyle read GetStyleValue write SetStyleValue default bsSolid;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragKind;
    property DragMode;
    property DragCursor;
    property Enabled;
    property EndEllipsis: Boolean read FEndEllipsis write SetEndEllipsis default False;
    property Font;
    property ImeMode;
    property ImeName;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property ShowStyleName: Boolean read FShowStyleName write SetShowStyleName default False;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetBrushStyleName: TdxGetBrushStyleNameEvent read FOnGetBrushStyleName write FOnGetBrushStyleName;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnStartDock;
    property OnStartDrag;
  end;

{ TdxPSPaintPanel }

{$DEFINE CANDEFINE_DOUBLEBUFFERED}

  TdxEdgeBorder = (ebLeft, ebTop, ebRight, ebBottom);
  TdxEdgeBorders = set of TdxEdgeBorder;

  TdxEdgeStyle = (esNone, esRaised, esSunken);

  TdxPSPaintPanel = class(TCustomPanel)
  private
    FEdgeBorders: TdxEdgeBorders;
    FEdgeInner: TdxEdgeStyle;
    FEdgeOuter: TdxEdgeStyle;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FOnPaint: TNotifyEvent;
    function GetEdgeBorders: TdxEdgeBorders;
    procedure SetEdgeBorders(Value: TdxEdgeBorders);
    procedure SetEdgeInner(Value: TdxEdgeStyle);
    procedure SetEdgeOuter(Value: TdxEdgeStyle);
    procedure WMNCCalcSize(var Message: TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure WMNCPaint(var Message: TWMNCPaint); message WM_NCPAINT;
    procedure CMCtl3DChanged(var Message: TMessage); message CM_CTL3DCHANGED;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
  protected
    procedure Paint; override;
    procedure DoPaint; dynamic;
    procedure DoMouseEnter; dynamic;
    procedure DoMouseLeave; dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    property Canvas;
  published
    property Align;
    property Anchors;
    property Constraints;
    property DragKind;
   {$IFDEF CANDEFINE_DOUBLEBUFFERED}
    property DoubleBuffered default True;
   {$ENDIF}
    property EdgeBorders: TdxEdgeBorders read GetEdgeBorders write SetEdgeBorders default [ebLeft, ebTop, ebRight, ebBottom];
    property EdgeInner: TdxEdgeStyle read FEdgeInner write SetEdgeInner default esRaised;
    property EdgeOuter: TdxEdgeStyle read FEdgeOuter write SetEdgeOuter default esSunken;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Ctl3D;
    property ParentColor;
    property ParentCtl3D;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
    property OnCanResize;
    property OnConstrainedResize;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
  end;

  { TdxPSBitmapAnimator }

  TdxPSBitmapAnimator = class(TGraphicControl)
  private
    FAnimationSpeed: Integer;
    FAnimationStepCount: Integer;
    FBitmap: TBitmap;
    FState: Boolean;
    procedure SetBitmap(Value: TBitmap);
    procedure SetState(Value: Boolean);
  protected
    procedure Paint; override;
    procedure Resize; override;

    procedure Animate; dynamic;
    procedure StateChanged; dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property AnimationSpeed: Integer read FAnimationSpeed write FAnimationSpeed {ms} default 10;
    property AnimationStepCount: Integer read FAnimationStepCount write FAnimationStepCount default 10;
    property Bitmap: TBitmap read FBitmap write SetBitmap;
    property State: Boolean read FState write SetState default False;
  end;

  { TdxPSWarningPane }

  TdxPSWarningPane = class(TdxPSBitmapAnimator)
  private
    FHint: string;
    procedure SetHint(const Value: string);
  protected
    procedure InitializeBitmap; virtual;
    procedure InitializeBitmapHint(var R: TRect); virtual;
    procedure StateChanged; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetStateAndHint(NewState: Boolean; const AHint: string);
  published
    property Font;
    property Hint: string read FHint write SetHint;
  end;

  { TdxPSImageScrollBox }

  TdxPSImageScrollBoxBuiltInMenuItem = (biiPreview, biiCopy, biiSave);
  TdxPSImageScrollBoxBuiltInMenuItems = set of TdxPSImageScrollBoxBuiltInMenuItem;

  TdxPSImageScrollBox = class(TcxControl)
  private
    FBuiltInImages: TcxImageList;
    FBuiltInMenu: TPopupMenu;
    FBuiltInMenuItemsVisibility: TdxPSImageScrollBoxBuiltInMenuItems;
    FCenter: Boolean;
    FHintText: string;
    FIsGraphicInvalid: Boolean;
    FPicture: TPicture;
    FPictureOriginX: Integer;
    FPictureOriginY: Integer;
    function GetContentColor: TColor;
    function GetContentTextColor: TColor;
    function GetHasGraphic: Boolean;
    function GetIsPictureHeightExceedControlBounds: Boolean;
    function GetIsPictureWidthExceedControlBounds: Boolean;
    function GetPictureHeight: Integer;
    function GetPictureRect: TRect;
    function GetPictureWidth: Integer;
    procedure SetCenter(Value: Boolean);
    procedure SetHintText(const Value: string);
    procedure SetPicture(Value: TPicture);
    procedure SetPictureOriginX(AValue: Integer);
    procedure SetPictureOriginY(AValue: Integer);

    procedure BuiltInMenuPopup(Sender: TObject);
    procedure CopyClick(Sender: TObject);
    procedure SaveClick(Sender: TObject);
    procedure PreviewClick(Sender: TObject);
    procedure PictureChanged(Sender: TObject);
  protected
    miCopy: TMenuItem;
    miLine1: TMenuItem;
    miLine2: TMenuItem;
    miPreview: TMenuItem;
    miSave: TMenuItem;
    function CheckPictureOriginX(AValue: Integer): Integer;
    function CheckPictureOriginY(AValue: Integer): Integer;
    procedure CreateBuiltInImages; virtual;
    procedure CreateBuiltInMenu; virtual;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DrawBackground(ACanvas: TcxCanvas; const R: TRect); virtual;
    procedure DrawHintText(ACanvas: TcxCanvas; const ATextRect: TRect); virtual;
    procedure DrawPicture(ACanvas: TcxCanvas; const R: TRect); virtual;
    procedure InitScrollBarsParameters; override;
    procedure Paint; override;
    procedure Resize; override;
    procedure Scroll(AScrollBarKind: TScrollBarKind; AScrollCode: TScrollCode;
      var AScrollPos: Integer); override;
    procedure ScrollHorizontal(AScrollCode: TScrollCode; var AScrollPos: Integer); virtual;
    procedure ScrollVertical(AScrollCode: TScrollCode; var AScrollPos: Integer); virtual;
    procedure UpdatePicturePosition; virtual;

    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMMouseActivate(var Message: TWMMouseActivate); message WM_MOUSEACTIVATE;
    procedure WMNCCalcSize(var Message: TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure WMNCPaint(var Message: TWMNCPaint); message WM_NCPAINT;

    property ContentColor: TColor read GetContentColor;
    property ContentTextColor: TColor read GetContentTextColor;
    property IsPictureHeightExceedControlBounds: Boolean read GetIsPictureHeightExceedControlBounds;
    property IsPictureWidthExceedControlBounds: Boolean read GetIsPictureWidthExceedControlBounds;
    property PictureHeight: Integer read GetPictureHeight;
    property PictureOriginX: Integer read FPictureOriginX write SetPictureOriginX;
    property PictureOriginY: Integer read FPictureOriginY write SetPictureOriginY;
    property PictureRect: TRect read GetPictureRect;
    property PictureWidth: Integer read GetPictureWidth;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property BuiltInImages: TcxImageList read FBuiltInImages;
    property BuiltInMenu: TPopupMenu read FBuiltInMenu;
    property BuiltInMenuItemsVisibility: TdxPSImageScrollBoxBuiltInMenuItems read FBuiltInMenuItemsVisibility
      write FBuiltInMenuItemsVisibility default [biiPreview..biiSave];
    property HasGraphic: Boolean read GetHasGraphic;
  published
    property Center: Boolean read FCenter write SetCenter default True;
    property Color;
    property HintText: string read FHintText write SetHintText;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property Picture: TPicture read FPicture write SetPicture;
    property PopupMenu;
  end;

  { TdxPSValueEdit }

  TdxPSValueType = (ivtDecimal, ivtLiteral, ivtCapitalLiteral, ivtRoman, ivtCapitalRoman);

  TdxPSValueEdit = class(TcxSpinEdit)
  private
    FValueType: TdxPSValueType;
    function DisplayValueToEditValue(const AValue: Variant): Variant;
    function EditValueToDisplayValue(const AValue: Variant): Variant;
    procedure SetValueType(AValueType: TdxPSValueType);
  protected
    procedure DoValidateDisplayValue(var ADisplayValue: Variant;
      var AErrorText: TCaption; var AError: Boolean); override;
    function IncrementValueToStr(const AValue: Variant): string; override;
    function IsValidChar(Key: Char): Boolean; override;
    procedure PrepareDisplayValue(const AEditValue: Variant;
      var DisplayValue: Variant; AEditFocused: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure PrepareEditValue(const ADisplayValue: Variant;
      out EditValue: Variant; AEditFocused: Boolean); override;
    //
    property ValueType: TdxPSValueType read FValueType write SetValueType default ivtDecimal;
  end;

var
  UseAllColorValuesInDropDownList: Boolean = True;

function WarningSignBitmap: TcxBitmap32;

implementation

uses
  Themes, UxTheme, CommCtrl, SysUtils, Registry, ExtDlgs, ClipBrd, Variants, Types, Math,
  cxClasses, cxLookAndFeelPainters, cxScrollBar, cxVariants, cxEdit,
  dxExtCtrlsStrs, dxPSUtl, dxPSImgs, dxPSGlbl, dxPSRes, dxPCPrVw, dxPSPopupMan;

const

  PureColors: array[0..19] of TColor =
    (clBlack, clOlive, clTeal, clGreen, clMoneyGreen, clLime, clNavy, clBlue,
     clAqua, clSkyBlue, clGray, clMedGray, clSilver, clMaroon, clPurple, clFuchsia, clRed,
     clCream, clYellow, clWhite);
  SysColors: array[0..24] of TColor =
    (clScrollBar, clBackground, clActiveCaption, clInactiveCaption, clMenu,
     clWindow, clWindowFrame, clMenuText, clWindowText, clCaptionText, clActiveBorder,
     clInactiveBorder, clAppWorkSpace, clHighlight, clHighlightText, clBtnFace,
     clBtnShadow, clGrayText, clBtnText, clInactiveCaptionText, clBtnHighlight,
     cl3DDkShadow, cl3DLight, clInfoText, clInfoBk);

var
  FWarningSignBitmap: TcxBitmap32;

function WarningSignBitmap: TcxBitmap32;
begin
  if FWarningSignBitmap = nil then
  begin
    FWarningSignBitmap := TcxBitmap32.Create;
    dxLoadBitmapFromResource(FWarningSignBitmap, IDB_DXPSWARNINGSIGN);
  end;
  Result := FWarningSignBitmap;
end;

{ TdxPSBrushStyleCombo }

constructor TdxPSBrushStyleCombo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  AutoSize := False;
  FEndEllipsis := False;
  FShowStyleName := False;
  FBrushColor := clWindowText;
  Properties.ItemHeight := ScaleFactor.Apply(22);
  Properties.DropDownListStyle := lsFixedList;
  Properties.OnDrawItem := DoDrawItem;
  Height := ScaleFactor.Apply(22);
end;

procedure TdxPSBrushStyleCombo.BiDiModeChanged;
begin
  inherited;
  Invalidate;
end;

procedure TdxPSBrushStyleCombo.CreateWnd;
begin
  inherited CreateWnd;
  RefreshItems;
  ItemIndex := 0;
end;

procedure TdxPSBrushStyleCombo.CalculateRects(const R: TRect; out ABrushRect, ATextRect: TRect);
begin
  ABrushRect := R;
  ATextRect := R;
  if ShowStyleName then
  begin
    if BiDiMode = bdRightToLeft then
      ABrushRect.Left := ABrushRect.Right - cxRectWidth(R) div 2
    else
      ABrushRect.Right := ABrushRect.Left + cxRectWidth(R) div 2;

    SubtractRect(ATextRect, R, ABrushRect);
    if BiDiMode = bdRightToLeft then
      Dec(ATextRect.Right, ScaleFactor.Apply(6));
    Inc(ATextRect.Left, ScaleFactor.Apply(6));
  end;
end;

procedure TdxPSBrushStyleCombo.DoDrawItem(AControl: TcxCustomComboBox;
  ACanvas: TcxCanvas; AIndex: Integer; const ARect: TRect; AState: TOwnerDrawState);

  function GetBrushColor(AStyle: TBrushStyle): TColor;
  begin
    if AStyle = bsClear then
      Result := clWindow
    else
      if (ColorToRGB(BrushColor) <> ColorToRGB(clWindow)) or
         (StyleIndexes[AStyle] <= StyleIndexes[bsClear])
      then
        Result := BrushColor
      else
        Result := clWindowText
  end;

const
  TextFormat = DT_SINGLELINE or DT_LEFT or DT_VCENTER;
  EndEllipsisMap: array[Boolean] of Integer = (0, DT_END_ELLIPSIS);
var
  ABrushRect, ATextRect: TRect;
begin
  ACanvas.SaveState;
  try
    ACanvas.Brush.Style := bsSolid;
    ACanvas.FillRect(ARect, clDefault);
    CalculateRects(cxRectInflate(ARect, -ScaleFactor.Apply(2), -ScaleFactor.Apply(2)), ABrushRect, ATextRect);
    ACanvas.FrameRect(ABrushRect, clBtnShadow);

    InflateRect(ABrushRect, -ScaleFactor.Apply(1), -ScaleFactor.Apply(1));
    ACanvas.Brush.Style := Styles[AIndex];
    ACanvas.Brush.Color := GetBrushColor(ACanvas.Brush.Style);
    SetBkColor(ACanvas.Handle, ColorToRGB(clWindow));
    ACanvas.FillRect(ABrushRect, clDefault);

    if ShowStyleName then
    begin
      ACanvas.Brush.Style := bsClear;
      ACanvas.DrawTexT(StyleNames[AIndex], ATextRect,
        TextFormat or EndEllipsisMap[EndEllipsis], Enabled);
    end;
  finally
    ACanvas.RestoreState;
  end;
end;

function TdxPSBrushStyleCombo.GetStyleName_(Index: Integer): string;
begin
  Result := Properties.Items[Index];
  DoGetStyleName(Index, Result);
end;

procedure TdxPSBrushStyleCombo.DoGetStyleName(Index: Integer; var AName: string);
begin
  if Assigned(OnGetBrushStyleName) then
    OnGetBrushStyleName(Self, Index, Styles[Index], AName);
end;

procedure TdxPSBrushStyleCombo.EnabledChanged;
begin
  if csDesigning in ComponentState then
    inherited;
end;

function TdxPSBrushStyleCombo.GetStyle(Index: Integer): TBrushStyle;
begin
  if Index < 0 then
    Result := bsSolid
  else
    Result := TBrushStyle(Properties.Items.Objects[Index]);
end;

function TdxPSBrushStyleCombo.GetStyleIndex(Style: TBrushStyle): Integer;
begin
  Result := Properties.Items.IndexOfObject(TObject(Style));
end;

function TdxPSBrushStyleCombo.GetStyleValue: TBrushStyle;
begin
  Result := Styles[ItemIndex];
end;

procedure TdxPSBrushStyleCombo.SetBrushColor(Value: TColor);
begin
  if FBrushColor <> Value then
  begin
    FBrushColor := Value;
    Invalidate;
  end;
end;

procedure TdxPSBrushStyleCombo.SetEndEllipsis(Value: Boolean);
begin
  if FEndEllipsis <> Value then
  begin
    FEndEllipsis := Value;
    Invalidate;
  end;
end;

procedure TdxPSBrushStyleCombo.SetShowStyleName(Value: Boolean);
begin
  if FShowStyleName <> Value then
  begin
    FShowStyleName := Value;
    Invalidate;
  end;
end;

procedure TdxPSBrushStyleCombo.SetStyleValue(Value: TBrushStyle);
begin
  ItemIndex := StyleIndexes[Value];
end;

procedure TdxPSBrushStyleCombo.RefreshItems;
begin
  Properties.Items.BeginUpdate;
  try
    Properties.Items.Clear;
    Properties.Items.AddObject(cxGetResourceString(@sdxBrushStyleSolid), TObject(bsSolid));
    Properties.Items.AddObject(cxGetResourceString(@sdxBrushStyleClear), TObject(bsClear));
    Properties.Items.AddObject(cxGetResourceString(@sdxBrushStyleHorizontal), TObject(bsHorizontal));
    Properties.Items.AddObject(cxGetResourceString(@sdxBrushStyleVertical), TObject(bsVertical));
    Properties.Items.AddObject(cxGetResourceString(@sdxBrushStyleFDiagonal), TObject(bsFDiagonal));
    Properties.Items.AddObject(cxGetResourceString(@sdxBrushStyleBDiagonal), TObject(bsBDiagonal));
    Properties.Items.AddObject(cxGetResourceString(@sdxBrushStyleCross), TObject(bsCross));
    Properties.Items.AddObject(cxGetResourceString(@sdxBrushStyleDiagCross), TObject(bsDiagCross));
  finally
    Properties.Items.EndUpdate;
  end;
end;

procedure TdxPSBrushStyleCombo.ResetItemHeight;
begin
  Properties.ItemHeight := Max(ScaleFactor.Apply(22), -MulDiv(Font.Height, 12, 10));
end;

procedure TdxPSBrushStyleCombo.CMFontChanged(var Message: TMessage);
begin
  inherited;
  ResetItemHeight;
end;

procedure TdxPSBrushStyleCombo.CMRecreateWnd(var Message: TMessage);
var
  SaveStyle: TBrushStyle;
begin
  SaveStyle := BrushStyle;
  inherited;
  BrushStyle := SaveStyle;
end;

{ TdxPSPaintPanel }

constructor TdxPSPaintPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEdgeBorders := [ebLeft, ebTop, ebRight, ebBottom];
  FEdgeInner := esRaised;
  FEdgeOuter := esSunken;
  FDoubleBuffered := True;
  SetBounds(0, 0, 10, 10);
end;

procedure TdxPSPaintPanel.Paint;
begin
  DoPaint;
end;

procedure TdxPSPaintPanel.DoMouseEnter;
begin
  if Assigned(FOnMouseEnter) then FOnMouseEnter(Self)
end;

procedure TdxPSPaintPanel.DoMouseLeave;
begin
  if Assigned(FOnMouseLeave) then FOnMouseLeave(Self)
end;

procedure TdxPSPaintPanel.DoPaint;
begin
  if Assigned(FOnPaint) then FOnPaint(Self)
end;

function TdxPSPaintPanel.GetEdgeBorders: TdxEdgeBorders;
begin
  Result := FEdgeBorders;
end;

procedure TdxPSPaintPanel.SetEdgeBorders(Value: TdxEdgeBorders);
begin
  if FEdgeBorders <> Value then
  begin
    FEdgeBorders := Value;
    if (FEdgeOuter <> esNone) and (FEdgeInner <> esNone) then
      RecreateWnd;
  end;
end;

procedure TdxPSPaintPanel.SetEdgeInner(Value: TdxEdgeStyle);
begin
  if FEdgeInner <> Value then
  begin
    FEdgeInner := Value;
    RecreateWnd;
  end;
end;

procedure TdxPSPaintPanel.SetEdgeOuter(Value: TdxEdgeStyle);
begin
  if FEdgeOuter <> Value then
  begin
    FEdgeOuter := Value;
    RecreateWnd;
  end;
end;

procedure TdxPSPaintPanel.WMNCCalcSize(var Message: TWMNCCalcSize);

  function GetEdgeSize: Integer;
  begin
    if Ctl3D then
      Result := Integer(EdgeInner > esNone) + Integer(EdgeOuter > esNone)
    else
      Result := 1;
  end;

  procedure AdjustEdges(var R: TRect; AEdgeSize: Integer);
  begin
    if ebLeft in FEdgeBorders then Inc(R.Left, AEdgeSize);
    if ebTop in FEdgeBorders then Inc(R.Top, AEdgeSize);
    if ebRight in FEdgeBorders then Dec(R.Right, AEdgeSize);
    if ebBottom in FEdgeBorders then Dec(R.Bottom, AEdgeSize);
  end;

begin
  AdjustEdges(Message.CalcSize_Params^.rgrc[0], GetEdgeSize);
  inherited;
end;

procedure TdxPSPaintPanel.WMNCPaint(var Message: TWMNCPaint);
const
  InnerStyles: array[TdxEdgeStyle] of Integer = (0, BDR_RAISEDINNER, BDR_SUNKENINNER);
  OuterStyles: array[TdxEdgeStyle] of Integer = (0, BDR_RAISEDOUTER, BDR_SUNKENOUTER);
  Ctl3DStyles: array[Boolean] of Integer = (BF_MONO, 0);
var
  R: TRect;
  DC: HDC;
  //Details: TThemedElementDetails;
begin
  GetWindowRect(Handle, R);
  OffsetRect(R, -R.Left, -R.Top);
  DC := GetWindowDC(Handle);
  try
    //Details := ThemeServices.GetElementDetails(trBandNormal);
    //ThemeServices.DrawEdge(DC, Details, R, BDR_RAISEDINNER or BDR_RAISEDOUTER, BF_RECT or BF_FLAT);
    DrawEdge(DC, R, InnerStyles[FEdgeInner] or OuterStyles[FEdgeOuter],
      Byte(FEdgeBorders) or Ctl3DStyles[Ctl3D]);
  finally
    ReleaseDC(Handle, DC);
  end;
end;

procedure TdxPSPaintPanel.CMCtl3DChanged(var Message: TMessage);
begin
  inherited;
  if FEdgeBorders <> [] then RecreateWnd;
end;

procedure TdxPSPaintPanel.CMTextChanged(var Message: TMessage);
begin
end;

procedure TdxPSPaintPanel.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  DoMouseEnter;
end;

procedure TdxPSPaintPanel.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  DoMouseLeave;
end;

{ TdxPSBitmapAnimator }

constructor TdxPSBitmapAnimator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAnimationSpeed := 10;
  FAnimationStepCount := 10;

  FBitmap := TBitmap.Create;
  FBitmap.Height := Height;
  FBitmap.Width := Width;

  FState := False;
end;

destructor TdxPSBitmapAnimator.Destroy;
begin
  FBitmap.Free;
  inherited Destroy;
end;

procedure TdxPSBitmapAnimator.Paint;
begin
  if State and not Bitmap.Empty then
    Canvas.Draw(0, 0, Bitmap)
end;

procedure TdxPSBitmapAnimator.Resize;
begin
  inherited;
  Bitmap.Height := Height;
  Bitmap.Width := Width;
end;

procedure TdxPSBitmapAnimator.Animate;
var
  dY, V, I: Integer;
  T: DWORD;
  R: TRect;
begin
  dY := Height div AnimationStepCount + Ord((Height mod AnimationStepCount) <> 0);

  T := GetTickCount;
  for I := 1 to AnimationStepCount do
  begin
    while GetTickCount - T < DWORD(FAnimationSpeed) do ;
    T := GetTickCount;

    if State then
    begin
      V := Height - I * dY;
      if V < 0 then V := 0;
      Canvas.Draw(0, V, Bitmap);
    end
    else
    begin
      V := I * dY;
      if V >= Height then V := Height;
      R := Bounds(Left, Top + V - dY, Width, dY);
      InvalidateRect(Parent.Handle, @R, True);
      UpdateWindow(Parent.Handle);
      Canvas.Draw(0, V, Bitmap);
    end;
  end;

  if Bitmap.Width < Width then
  begin
    Canvas.Brush.Color := clBtnFace;
    Canvas.FillRect(Rect(Bitmap.Width, 0, Width, Height));
  end;
end;

procedure TdxPSBitmapAnimator.StateChanged;
begin
  if not Bitmap.Empty then Animate;
end;

procedure TdxPSBitmapAnimator.SetBitmap(Value: TBitmap);
begin
  Bitmap.Assign(Value);
  Bitmap.Height := Height;
  Bitmap.Width := Width;
end;

procedure TdxPSBitmapAnimator.SetState(Value: Boolean);
begin
  if FState <> Value then
  begin
    FState := Value;
    StateChanged;
  end;
end;

{ TdxPSWarningPane }

constructor TdxPSWarningPane.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Font.Name := 'Tahoma';
  Font.Color := clInfoText;
  SetBounds(0, 0, 10, 10);
end;

procedure TdxPSWarningPane.SetStateAndHint(NewState: Boolean; const AHint: string);
begin
  if State = NewState then
    Hint := AHint
  else
    if not State then
    begin
      Hint := AHint;
      State := True;
    end
    else
      State := False;
end;

procedure TdxPSWarningPane.InitializeBitmap;

  procedure PrepareBackground(ACanvas: TCanvas; var R: TRect);
  begin
    ACanvas.Pen.Color := clBtnShadow;
    ACanvas.Brush.Style := bsSolid;
    ACanvas.Brush.Color := clInfoBk;
    ACanvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
    InflateRect(R, -1, -1);
  end;

  procedure DrawWarningSign(ACanvas: TCanvas; var R: TRect);
  var
    AImageRect: TRect;
  begin
    Inc(R.Left, 2);
    AImageRect := cxRectSetWidth(R, WarningSignBitmap.Width);
    AImageRect := cxRectCenterVertically(AImageRect, WarningSignBitmap.Height);
    cxAlphaBlend(Bitmap, WarningSignBitmap, AImageRect, WarningSignBitmap.ClientRect);
    R.Left := AImageRect.Right + 4;
    InflateRect(R, -1, -1);
  end;

var
  R: TRect;
begin
  R := Rect(0, 0, Bitmap.Width, Bitmap.Height);
  PrepareBackground(Bitmap.Canvas, R);
  DrawWarningSign(Bitmap.Canvas, R);
  Bitmap.Canvas.Font := Font;
  Bitmap.Canvas.Brush.Style := bsClear;
  InitializeBitmapHint(R);
end;

procedure TdxPSWarningPane.InitializeBitmapHint(var R: TRect);
const
  TextFormats: array[Boolean] of UINT = (DT_SINGLELINE or DT_VCENTER, DT_WORDBREAK);
begin
  DrawText(Bitmap.Canvas.Handle, PChar(Hint), Length(Hint), R,
    TextFormats[Bitmap.Canvas.TextWidth(Hint) > (R.Right - R.Left)]);
end;

procedure TdxPSWarningPane.StateChanged;
begin
  inherited StateChanged;
  Beep;
end;

procedure TdxPSWarningPane.SetHint(const Value: string);
begin
  if FHint <> Value then
  begin
    FHint := Value;
    InitializeBitmap;
    if State then
      Invalidate;
  end;
end;

{ TdxPSImageScrollBox }

constructor TdxPSImageScrollBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBuiltInMenuItemsVisibility := [biiPreview..biiSave];
  Font.Style := Font.Style + [fsBold];

  FBuiltInImages := TcxImageList.Create(Self);
  FBuiltInMenu := TPopupMenu.Create(Self);
  FPicture := TPicture.Create;
  FPicture.OnChange := PictureChanged;
  FCenter := True;
  ParentFont := False;

  CreateBuiltInImages;
  CreateBuiltInMenu;

  SetBounds(0, 0, 300, 300);
end;

destructor TdxPSImageScrollBox.Destroy;
begin
  FreeAndNil(FPicture);
  inherited Destroy;
end;

function TdxPSImageScrollBox.CheckPictureOriginX(AValue: Integer): Integer;
begin
  if IsPictureWidthExceedControlBounds then
    Result := Max(Min(AValue, 0), cxRectWidth(ClientBounds) - PictureWidth)
  else
    if Center then
      Result := (ClientWidth - PictureWidth) div 2
    else
      Result := 0;
end;

function TdxPSImageScrollBox.CheckPictureOriginY(AValue: Integer): Integer;
begin
  if IsPictureHeightExceedControlBounds then
    Result := Max(Min(AValue, 0), cxRectHeight(ClientBounds) - PictureHeight)
  else
    if Center then
      Result := (ClientHeight - PictureHeight) div 2
    else
      Result := 0;
end;

procedure TdxPSImageScrollBox.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.WindowClass.Style := Params.WindowClass.Style or (CS_HREDRAW or CS_VREDRAW);
end;

procedure TdxPSImageScrollBox.CreateBuiltInImages;

  procedure LoadImage(B: TcxBitmap; const AResName: string);
  begin
    dxLoadBitmapFromResource(B, AResName);
    BuiltInImages.Add(B, nil);
  end;

var
  B: TcxBitmap;
begin
  B := TcxBitmap.Create;
  try
    BuiltInImages.AllocBy := 3;
    LoadImage(B, IDB_DXPSPREVIEW);
    LoadImage(B, IDB_DXPSCOPY);
    LoadImage(B, IDB_DXPSSAVE);
  finally
    B.Free;
  end;
end;

procedure TdxPSImageScrollBox.CreateBuiltInMenu;

  function CreateMenuItem(const ACaption: string; AImageIndex: Integer;
    AShortCut: TShortCut; AOnClick: TNotifyEvent): TMenuItem;
  begin
    Result := TMenuItem.Create(Self);
    Result.Caption := ACaption;
    Result.ImageIndex := AImageIndex;
    Result.ShortCut := AShortCut;
    Result.OnClick := AOnClick;
    BuiltInMenu.Items.Add(Result);
  end;

begin
  miPreview := CreateMenuItem(AddEndEllipsis(cxGetResourceString(@sdxPreview)), 0, 0, PreviewClick);
  miLine1 := CreateMenuItem('-', -1, 0, nil);
  miCopy := CreateMenuItem(cxGetResourceString(@sdxCopy), 1, Menus.TextToShortCut('Ctrl+C'), CopyClick);
  miLine2 := CreateMenuItem('-', -1, 0, nil);
  miSave := CreateMenuItem(cxGetResourceString(@sdxSave), 2, Menus.TextToShortCut('Ctrl+S'), SaveClick);

  BuiltInMenu.Images := BuiltInImages;
  BuiltInMenu.OnPopup := BuiltInMenuPopup;

  PopupMenu := BuiltInMenu;
end;

procedure TdxPSImageScrollBox.DrawBackground(ACanvas: TcxCanvas; const R: TRect);
begin
  ACanvas.FillRect(R, ContentColor);
end;

procedure TdxPSImageScrollBox.DrawHintText(ACanvas: TcxCanvas; const ATextRect: TRect);
begin
  ACanvas.Font.Assign(Font);
  ACanvas.Font.Color := ContentTextColor;
  ACanvas.Brush.Style := bsClear;
  ACanvas.DrawTexT(HintText, ATextRect, cxAlignCenter or cxWordBreak);
end;

procedure TdxPSImageScrollBox.DrawPicture(ACanvas: TcxCanvas; const R: TRect);
begin
  cxDrawImage(ACanvas, R, Picture.Graphic, ifmStretch, nil, ScaleFactor);
end;

procedure TdxPSImageScrollBox.Paint;
begin
  FIsGraphicInvalid := False;
  try
    DrawBackground(Canvas, ClientRect);
    if HasGraphic then
      DrawPicture(Canvas, PictureRect)
    else
      DrawHintText(Canvas, ClientRect);
  except
    FIsGraphicInvalid := True;
  end;
end;

function TdxPSImageScrollBox.GetContentColor: TColor;
begin
  if (Color = clDefault) or (Color = clBtnFace) then
    Result := LookAndFeelPainter.DefaultGroupColor
  else
    Result := Color;
end;

function TdxPSImageScrollBox.GetContentTextColor: TColor;
begin
  Result := Font.Color;
  if Result = clWindowText then
    Result := LookAndFeelPainter.DefaultGroupTextColor;
end;

function TdxPSImageScrollBox.GetHasGraphic: Boolean;
begin
  Result := Assigned(Picture.Graphic) and not Picture.Graphic.Empty;
end;

function TdxPSImageScrollBox.GetIsPictureHeightExceedControlBounds: Boolean;
begin
  Result := PictureHeight > ClientHeight;
end;

function TdxPSImageScrollBox.GetIsPictureWidthExceedControlBounds: Boolean;
begin
  Result := PictureWidth > ClientWidth;
end;

function TdxPSImageScrollBox.GetPictureHeight: Integer;
begin
  if HasGraphic then
    Result := ScaleFactor.Apply(Picture.Graphic.Height)
  else
    Result := 0
end;

function TdxPSImageScrollBox.GetPictureRect: TRect;
begin
  if HasGraphic then
    Result := Classes.Bounds(PictureOriginX, PictureOriginY, PictureWidth, PictureHeight)
  else
    Result := cxNullRect;
end;

function TdxPSImageScrollBox.GetPictureWidth: Integer;
begin
  if HasGraphic then
    Result := ScaleFactor.Apply(Picture.Graphic.Width)
  else
    Result := 0
end;

procedure TdxPSImageScrollBox.SetCenter(Value: Boolean);
begin
  if FCenter <> Value then
  begin
    FCenter := Value;
    UpdatePicturePosition;
    Invalidate;
  end;
end;

procedure TdxPSImageScrollBox.SetHintText(const Value: string);
begin
  if FHintText <> Value then
  begin
    FHintText := Value;
    Invalidate;
  end;
end;

procedure TdxPSImageScrollBox.SetPicture(Value: TPicture);
begin
  Picture.Assign(Value);
end;

procedure TdxPSImageScrollBox.SetPictureOriginX(AValue: Integer);
begin
  AValue := CheckPictureOriginX(AValue);
  if AValue <> FPictureOriginX then
  begin
    FPictureOriginX := AValue;
    Invalidate;
  end;
end;

procedure TdxPSImageScrollBox.SetPictureOriginY(AValue: Integer);
begin
  AValue := CheckPictureOriginY(AValue);
  if AValue <> FPictureOriginY then
  begin
    FPictureOriginY := AValue;
    Invalidate;
  end;
end;

procedure TdxPSImageScrollBox.BuiltInMenuPopup(Sender: TObject);
begin
  miPreview.Visible := biiPreview in FBuiltInMenuItemsVisibility;
  miPreview.Enabled := HasGraphic;
  miLine1.Visible := miPreview.Visible;
  miCopy.Visible := biiCopy in FBuiltInMenuItemsVisibility;
  miCopy.Enabled := HasGraphic;
  miLine2.Visible := miCopy.Visible;
  miSave.Visible := biiSave in FBuiltInMenuItemsVisibility;
  miSave.Enabled := HasGraphic;
end;

procedure TdxPSImageScrollBox.CopyClick(Sender: TObject);
begin
  if HasGraphic then
    Clipboard.Assign(Picture.Graphic);
end;

procedure TdxPSImageScrollBox.SaveClick(Sender: TObject);
begin
  if HasGraphic then
  begin
    with TSavePictureDialog.Create(nil) do
    try
      DefaultExt := cxGraphicExtension(Picture.Graphic);
      Filter := cxGraphicFilter(Picture.Graphic, True);
      Options := Options + [ofOverwritePrompt];
      if Execute then
        Picture.SaveToFile(FileName);
    finally
      Free;
    end;
  end;
end;

procedure TdxPSImageScrollBox.InitScrollBarsParameters;
begin
  SetScrollBarInfo(sbHorizontal, 0, PictureWidth - 1, 10, cxRectWidth(ClientBounds), -PictureOriginX, True, True);
  SetScrollBarInfo(sbVertical, 0, PictureHeight - 1, 10, cxRectHeight(ClientBounds), -PictureOriginY, True, True);
end;

procedure TdxPSImageScrollBox.PictureChanged(Sender: TObject);
begin
  UpdatePicturePosition;
  UpdateScrollBars;
  Invalidate;
end;

procedure TdxPSImageScrollBox.PreviewClick(Sender: TObject);
begin
  if HasGraphic then
    dxPCPrVw.dxShowPicturePreview(Picture.Graphic);
end;

procedure TdxPSImageScrollBox.Resize;
begin
  inherited Resize;
  UpdatePicturePosition;
end;

procedure TdxPSImageScrollBox.Scroll(AScrollBarKind: TScrollBarKind;
  AScrollCode: TScrollCode; var AScrollPos: Integer);
begin
  case AScrollBarKind of
    sbHorizontal:
      ScrollHorizontal(AScrollCode, AScrollPos);
    sbVertical:
      ScrollVertical(AScrollCode, AScrollPos);
  end;
  UpdateScrollBars;
end;

procedure TdxPSImageScrollBox.ScrollHorizontal(AScrollCode: TScrollCode; var AScrollPos: Integer);
const
  SignsMap: array[Boolean] of Integer = (-1, 1);
begin
  case AScrollCode of
    scTop:
      PictureOriginX := 0;
    scBottom:
      PictureOriginX := -PictureWidth;
    scEndScroll, scTrack:
      PictureOriginX := -AScrollPos;
    scLineUp, scLineDown:
      PictureOriginX := PictureOriginX +
        SignsMap[AScrollCode = scLineUp] * HScrollBar.SmallChange;
    scPageUp, scPageDown:
      PictureOriginX := PictureOriginX +
        SignsMap[AScrollCode = scPageUp] * HScrollBar.PageSize;
  end;
end;

procedure TdxPSImageScrollBox.ScrollVertical(
  AScrollCode: TScrollCode; var AScrollPos: Integer);
const
  SignsMap: array[Boolean] of Integer = (-1, 1);
begin
  case AScrollCode of
    scTop:
      PictureOriginY := 0;
    scBottom:
      PictureOriginY := -PictureHeight;
    scEndScroll, scTrack:
      PictureOriginY := -AScrollPos;
    scLineUp, scLineDown:
      PictureOriginY := PictureOriginY +
        SignsMap[AScrollCode = scLineUp] * VScrollBar.SmallChange;
    scPageUp, scPageDown:
      PictureOriginY := PictureOriginY +
        SignsMap[AScrollCode = scPageUp] * VScrollBar.PageSize;
  end;
end;

procedure TdxPSImageScrollBox.UpdatePicturePosition;
begin
  PictureOriginX := CheckPictureOriginX(PictureOriginX);
  PictureOriginY := CheckPictureOriginY(PictureOriginY);
end;

procedure TdxPSImageScrollBox.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.Result := 1;
end;

procedure TdxPSImageScrollBox.WMMouseActivate(var Message: TWMMouseActivate);
begin
  inherited;
  if not (csDesigning in ComponentState) and CanFocus then SetFocus;
end;

procedure TdxPSImageScrollBox.WMNCCalcSize(var Message: TWMNCCalcSize);
begin
  inherited;
  with Message.CalcSize_Params^ do
    InflateRect(rgrc[0], -2, -2);
end;

procedure TdxPSImageScrollBox.WMNCPaint(var Message: TWMNCPaint);
var
  AWindowRect: TRect;
  DC: HDC;
begin
  DC := GetWindowDC(Handle);
  try
    Canvas.Canvas.Handle := DC;
    try
      Canvas.Brush.Style := bsSolid;
      AWindowRect := cxGetWindowRect(Self);
      OffsetRect(AWindowRect, -AWindowRect.Left, -AWindowRect.Top);
      Canvas.ExcludeClipRect(cxRectInflate(AWindowRect, -2, -2));
      DrawBackground(Canvas, AWindowRect);
      LookAndFeelPainter.DrawBorder(Canvas, AWindowRect);
    finally
      Canvas.Canvas.Handle := 0;
    end;
  finally
    ReleaseDC(Handle, DC);
  end;
end;

{ TdxPSValueEdit }

constructor TdxPSValueEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FValueType := ivtDecimal;
  Properties.MinValue := 1;
end;

function TdxPSValueEdit.DisplayValueToEditValue(const AValue: Variant): Variant;
begin
  case ValueType of
    ivtLiteral, ivtCapitalLiteral:
      Result := Chars2Int(AValue, ValueType = ivtCapitalLiteral);
    ivtRoman, ivtCapitalRoman:
      Result := Roman2Int(AValue, ValueType = ivtCapitalRoman);
    else
      Result := AValue;
  end;
end;

procedure TdxPSValueEdit.DoValidateDisplayValue(
  var ADisplayValue: Variant; var AErrorText: TCaption; var AError: Boolean);
begin
  ADisplayValue := EditValueToDisplayValue(DisplayValueToEditValue(ADisplayValue));
end;

function TdxPSValueEdit.EditValueToDisplayValue(const AValue: Variant): Variant;
begin
  case ValueType of
    ivtLiteral, ivtCapitalLiteral:
      Result := Int2Chars(Max(AValue, 0), ValueType = ivtCapitalLiteral);
    ivtRoman, ivtCapitalRoman:
      Result := Int2Roman(Max(AValue, 0), ValueType = ivtCapitalRoman);
    else
      Result := Max(AValue, 0);
  end;
end;

function TdxPSValueEdit.IncrementValueToStr(const AValue: Variant): string;
begin
  Result := EditValueToDisplayValue(AValue);
end;

function TdxPSValueEdit.IsValidChar(Key: Char): Boolean;
const
  RomanChars: string = 'cdilmxv';
  RomanCharsCapital: string = 'CDILMXV';
begin
  case ValueType of
    ivtLiteral:
      Result := dxCharInSet(Key, ['a'..'z']);
    ivtCapitalLiteral:
      Result := dxCharInSet(Key, ['A'..'Z']);
    ivtRoman:
      Result := Pos(Key, RomanChars) <> 0;
    ivtCapitalRoman:
      Result := Pos(Key, RomanCharsCapital) <> 0;
    else // ivtDecimal:
      Result := (Key <> '-') and inherited IsValidChar(Key);
  end;
end;

procedure TdxPSValueEdit.PrepareDisplayValue(const AEditValue: Variant;
  var DisplayValue: Variant; AEditFocused: Boolean);
begin
  DisplayValue := EditValueToDisplayValue(AEditValue);
end;

procedure TdxPSValueEdit.PrepareEditValue(const ADisplayValue: Variant;
  out EditValue: Variant; AEditFocused: Boolean);
begin
  EditValue := DisplayValueToEditValue(ADisplayValue);
end;

procedure TdxPSValueEdit.SetValueType(AValueType: TdxPSValueType);
var
  ATempValue: Integer;
begin
  if FValueType <> AValueType then
  begin
    ATempValue := Value;
    FValueType := AValueType;
    Value := ATempValue;
  end;
end;

initialization

finalization
  FreeAndNil(FWarningSignBitmap);

end.

