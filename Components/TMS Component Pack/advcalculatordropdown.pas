{***************************************************************************}
{ TAdvCalculatorDropdown components                                         }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2009 - 2014                                        }
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

unit AdvCalculatorDropdown;

interface

uses
  Classes, Windows, Graphics, Controls, Messages, StdCtrls, ImgList, Math, SysUtils,
  AdvDropDown, AdvStyleIF
  {$IFDEF DELPHI6_LVL}
  , Types
  {$ENDIF}
  , ATXPVS
  {$IFDEF TMSGDIPLUS}
  , AdvGDIP
  {$ENDIF}
  {$IFDEF DELPHI_UNICODE}
  , Character
  {$ENDIF}
  ;

const
  BP_BUTTONGLOWSIZE = 8;

type
  TButtonPanelType = (plCalculator, plCustom{, plScientificCal, plEvaluator etc});
  TCalculatorButtonClickEvent = procedure (Sender: TObject; Index: Integer) of object;

  TCalcButtonItem = class(TCollectionItem)
  private
    FCaption: TCaption;
    FImageIndex: Integer;
    FEnabled: Boolean;
    FImage: TPicture;
    FTag: Integer;
    FHeight: Integer;
    FLeft: Integer;
    FWidth: Integer;
    FTop: Integer;
    FName: string;
    FCaptionColor: TColor;
    FSpecial: Boolean;
    procedure SetCaption(const Value: TCaption);
    procedure SetImage(const Value: TPicture);
    procedure SetTag(const Value: Integer);
    function GetRect: TRect;
    procedure SetCaptionColor(const Value: TColor);
  protected
    property Rect: TRect read GetRect;
    property Enabled: Boolean read FEnabled write FEnabled default True;
    property Name: string read FName write FName;

    property Left: Integer read FLeft write FLeft;
    property Top: Integer read FTop write FTop;
    property Width: Integer read FWidth write FWidth;
    property Height: Integer read FHeight write FHeight;
    property Special: Boolean read FSpecial write FSpecial default False; // use SpecialTextColor
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Caption: TCaption read FCaption write SetCaption;
    property CaptionColor: TColor read FCaptionColor write SetCaptionColor default clWhite;
    property ImageIndex: Integer read FImageIndex write FImageIndex default -1;
    property Image: TPicture read FImage write SetImage;
    property Tag: Integer read FTag write SetTag default 0;
  end;

  TCalcButtonItems = class(TCollection)
  private
    FMyOwner: TPersistent;
    FOnChange: TNotifyEvent;
    function GetItem(Index: Integer): TCalcButtonItem;
    procedure SetItem(Index: Integer; const Value: TCalcButtonItem);
  public
    constructor Create(AOwner: TPersistent);
    property Items[Index: Integer]: TCalcButtonItem read GetItem write SetItem; default;
    function Add: TCalcButtonItem;
    function Insert(Index: Integer): TCalcButtonItem;
    function GetOwner: TPersistent; override;
    {$IFNDEF DELPHI6_LVL}
    property Owner: TPersistent read FMyOwner;
    {$ENDIF}
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TButtonAppearance = class(TPersistent)
  private
    FOnChange: TNotifyEvent;
    FBorderColor: TColor;
    FBorderColorHot: TColor;
    FBorderColorDown: TColor;
    FBorderColorChecked: TColor;
    FColor: TColor;
    FColorTo: TColor;
    FColorDown: TColor;
    FColorDownTo: TColor;
    FColorHot: TColor;
    FColorHotTo: TColor;
    FColorCheckedTo: TColor;
    FColorDisabled: TColor;
    FColorDisabledTo: TColor;
    FColorChecked: TColor;
    FFont: TFont;
    FShineColor: TColor;
    FGlowColorHot: TColor;
    FGlowColorDown: TColor;
    FTextColor: TColor;
    FSpecialTextColor: TColor;
    procedure SetBorderColor(const Value: TColor);
    procedure SetBorderColorChecked(const Value: TColor);
    procedure SetBorderColorDown(const Value: TColor);
    procedure SetBorderColorHot(const Value: TColor);
    procedure SetColor(const Value: TColor);
    procedure SetColorChecked(const Value: TColor);
    procedure SetColorCheckedTo(const Value: TColor);
    procedure SetColorDisabled(const Value: TColor);
    procedure SetColorDisabledTo(const Value: TColor);
    procedure SetColorDown(const Value: TColor);
    procedure SetColorDownTo(const Value: TColor);
    procedure SetColorHot(const Value: TColor);
    procedure SetColorHotTo(const Value: TColor);
    procedure SetColorTo(const Value: TColor);
    procedure SetFont(const Value: TFont);
    procedure SetShineColor(const Value: TColor);
    procedure SetSpecialTextColor(const Value: TColor);
    procedure SetTextColor(const Value: TColor);
  protected
    procedure Changed;
    property BorderColorChecked: TColor read FBorderColorChecked write SetBorderColorChecked default clBlue;
    property ColorChecked: TColor read FColorChecked write SetColorChecked;
    property ColorCheckedTo: TColor read FColorCheckedTo write SetColorCheckedTo;
    property ColorDisabled: TColor read FColorDisabled write SetColorDisabled;
    property ColorDisabledTo: TColor read FColorDisabledTo write SetColorDisabledTo;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property BorderColor: TColor read FBorderColor write SetBorderColor default clBlack;
    property BorderColorHot: TColor read FBorderColorHot write SetBorderColorHot default clBlack;
    property BorderColorDown: TColor read FBorderColorDown write SetBorderColorDown default clBlack;
    property Color: TColor read FColor write SetColor;
    property ColorTo: TColor read FColorTo write SetColorTo;
    property ColorDown: TColor read FColorDown write SetColorDown;
    property ColorDownTo: TColor read FColorDownTo write SetColorDownTo;
    property ColorHot: TColor read FColorHot write SetColorHot;
    property ColorHotTo: TColor read FColorHotTo write SetColorHotTo;
    property GlowColorHot: TColor read FGlowColorHot write FGlowColorHot;
    property GlowColorDown: TColor read FGlowColorDown write FGlowColorDown;
    property ShineColor: TColor read FShineColor write SetShineColor default clWhite;
    property Font: TFont read FFont write SetFont;
    property TextColor: TColor read FTextColor write SetTextColor default clBlack;
    property SpecialTextColor: TColor read FSpecialTextColor write SetSpecialTextColor default clMaroon;
  end;

  // suggest: Keyboard support
  // suggest: More panel types to make it more generic control
  // suggest: designer 
  TCustomButtonPanel = class(TCustomControl)
  private
    FButtons: TCalcButtonItems;
    FButtonLayout: TItemLayout;
    FItemColorStyle: TSelectionColorStyle;
    FButtonHot: Integer;
    FOffSetX: Integer;  // Control's top/bottom offset
    FOffSetY: Integer;  // Control's Left/Right offset
    FButtonOffSetX: Integer;  // Button's top/bottom offset
    FButtonOffSetY: Integer;  // Button's Left/Right offset
    FImages: TCustomImageList;
    FCaptionGap: Integer;
    FEdit: TCustomEdit;
    FButtonPanelType: TButtonPanelType;
    FButtonAppearance: TButtonAppearance;
    FButtonDown: Integer;
    FAdvDropDown: TAdvCustomDropDown;
    FOnButtonClick: TCalculatorButtonClickEvent;
    FIsWinXP: Boolean;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure OnButtonsChanged(Sender: TObject);
    procedure OnButtonAppearanceChanged(Sender: TObject);
    procedure SetButtons(const Value: TCalcButtonItems);
    procedure SetButtonLayout(const Value: TItemLayout);
    procedure SetItemColorStyle(const Value: TSelectionColorStyle);
    procedure SetImages(const Value: TCustomImageList);
    procedure SetButtonOffSetX(const Value: Integer);
    procedure SetButtonOffSetY(const Value: Integer);
    procedure SetCaptionGap(const Value: Integer);
    procedure SetButtonHot(const Value: Integer);
    procedure SetEdit(const Value: TCustomEdit);
    procedure SetButtonPanelType(const Value: TButtonPanelType);
    procedure SetButtonAppearance(const Value: TButtonAppearance);
    procedure SetButtonDown(const Value: Integer);
    procedure SetAdvDropDown(const Value: TAdvCustomDropDown);
  protected
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Paint; override;

    procedure DrawButtons(aCanvas: TCanvas; DrawHot: Boolean = true);
    procedure DrawButton(Index: Integer; aCanvas: TCanvas); virtual;
    function GetInnerRect: TRect; virtual;
    function GetMaxButtonSize: TSize; virtual;
    procedure ClearButtons; virtual;
    procedure ReInitialize; virtual; // ReAdd buttons according to PanelType
    procedure Initialize; virtual;  // add buttons according to PanelType
    procedure ArrangeButtons; virtual;  // Arrange and set buttons' size
    procedure InvalidateButton(Index: Integer); virtual;
    procedure ButtonClick(Index: Integer); virtual;
    function ButtonAtPos(X, Y: Integer): Integer;
    function GetButtonRect(Index: Integer): TRect;
    function DoVisualStyles: Boolean;

    property ButtonOffSetX: Integer read FButtonOffSetX write SetButtonOffSetX default 4;
    property ButtonOffSetY: Integer read FButtonOffSetY write SetButtonOffSetY default 4;
    property CaptionGap: Integer read FCaptionGap write SetCaptionGap default 4;
    property ButtonHot: Integer read FButtonHot write SetButtonHot default -1;
    property ButtonDown: Integer read FButtonDown write SetButtonDown default -1;

    property AdvDropDown: TAdvCustomDropDown read FAdvDropDown write SetAdvDropDown;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Buttons: TCalcButtonItems read FButtons write SetButtons;
    property ButtonAppearance: TButtonAppearance read FButtonAppearance write SetButtonAppearance;
    property ButtonLayout: TItemLayout read FButtonLayout write SetButtonLayout default ilCaptionRight;
    property ButtonPanelType: TButtonPanelType read FButtonPanelType write SetButtonPanelType default plCalculator;
    property Images: TCustomImageList read FImages write SetImages;

    property ItemColorStyle: TSelectionColorStyle read FItemColorStyle write SetItemColorStyle;
    property Edit: TCustomEdit read FEdit write SetEdit;

    property OnButtonClick: TCalculatorButtonClickEvent read FOnButtonClick write FOnButtonClick;
  end;

  TAdvButtonPanel = class(TCustomButtonPanel)
  private
  published
    property ButtonAppearance;
    property Buttons;
    property ButtonLayout;
    property ButtonPanelType;
    property Images;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvCalculatorDropdown = class(TAdvCustomDropDown)
  private
    FAdvButtonPanel: TCustomButtonPanel;
    FDecim: Integer;
    FNewval: Boolean;
    FPrevval: Extended;
    FPrevop: Integer;
    FCalcClosed: Boolean;
    FCalcButtonAppearance: TButtonAppearance;
    FMVal: Extended;
    FShowThousandSeparator: boolean;
    procedure OnAdvButtonClick(Sender: TObject; Index: Integer);
    procedure WMKeyDown(var Msg:TWMKeydown); message WM_KEYDOWN;
    procedure CMEnter(var Message: TCMGotFocus); message CM_ENTER;
    procedure HandleKeyPress(var Key: Char);
    procedure DoCalc;
    procedure DoPlus;
    procedure DoMin;
    procedure DoMul;
    procedure DoDiv;
    procedure DoEq;
    procedure DoPerc;
    function GetValue: Extended;
    procedure SetValue(const Value: Extended);
    procedure SetCalcButtonAppearance(const Value: TButtonAppearance);
    procedure SetShowThousandSeparator(const Value: boolean);
  protected
    procedure KeyPress(var key:char); override;
    procedure CalcChange; virtual;

    procedure CreateDropDownForm; override;
    procedure BeforeDropDown; override;
    procedure OnHideDropDown; override;
    procedure OnDropDownFormKeyPress(var Key: Char); override;
    procedure OnDropDownFormKeyDown(var Key: Word; Shift: TShiftState); override;
    procedure DoHideDropDown(Canceled: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetComponentStyle(AStyle: TTMSStyle); override;
    procedure CalcResult;
    property Value: extended read GetValue write SetValue;
  published
    property Align;
    property Anchors;
    property BevelEdges;
    property BevelInner;
    property BevelKind;
    property BevelOuter;
    property BevelWidth;
    property BiDiMode;
    property BorderStyle;
    property Color;
    property Constraints;
    property Ctl3D;
    property ImeMode;
    property ImeName;
    property MaxLength;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property Precision;
    property PopupMenu;
    property ShowHint;
    property Visible;

    property DropDownColor default clWhite;
    property DropDownBorderColor;
    property DropDownBorderWidth;
    property DropDownShadow;
    property DropDownWidth;
    property DropDownHeight;
    property DropPosition;
    property DropDownButtonWidth;
    property DropDownButtonHint;
    property DropDownSizeable;
    property Enabled;
    property EditorEnabled;
    property Font;
    property DropDownButtonGlyph;
    property Images;
    property Version;
    property ReadOnly;
    property Text;
    property ButtonAppearance;
    property CalcButtonAppearance: TButtonAppearance read FCalcButtonAppearance write SetCalcButtonAppearance;
    property DropDownHeader;
    property DropDownFooter;
    property DragCursor;
    property DragKind;
    property DragMode;
    property ShowThousandSeparator: boolean read FShowThousandSeparator write SetShowThousandSeparator default false;
    property TabStop;
    property TabOrder;

    property OnEnter;
    property OnExit;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    {$IFDEF DELPHI2006_LVL}
    property OnMouseEnter;
    property OnMouseLeave;
    {$ENDIF}
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnBeforeDropDown;
    property OnDropDown;
    property OnDropUp;
    property OnDropDownHeaderButtonClick;
    property OnDropDownFooterButtonClick;
    property OnDrawHeader;
    property OnDrawFooter;
    property OnGetHeaderText;
    property OnGetFooterText;
    property OnGetDropDownPos;
  end;

implementation

{$I DELPHIXE.INC}

function IsNumChar(ch: char): boolean;
begin
  {$IFNDEF DELPHIXE4_LVL}

  {$IFNDEF DELPHI_UNICODE}
  Result := (ch in ['0'..'9']);
  {$ENDIF}
  {$IFDEF DELPHI_UNICODE}
  Result := Character.IsNumber(ch);
  {$ENDIF}

  {$ENDIF}

  {$IFDEF DELPHIXE4_LVL}
  Result := ch.IsNumber;
  {$ENDIF}
end;

//------------------------------------------------------------------------------

function StripThousandSep(s:string):string;
begin
  while (Pos(ThousandSeparator,s)>0) do
    Delete(s,Pos(ThousandSeparator,s),1);
  Result := s;
end;

//------------------------------------------------------------------------------

procedure DrawButtonBackground(Canvas: TCanvas; R: TRect; BrClr, BrInClr, Clr, ClrTo, GlowClr, ShineClr: TColor);
{$IFDEF TMSGDIPLUS}
var
  R1, R2, RG: TRect;
  w: Integer;
  g: TGPGraphics;
  linGrBrush: TGPLinearGradientBrush;
  gppen: TGPPen;
  path: TGPGraphicsPath;
  //pathBrush: TGPPathGradientBrush;
  //colors : array[0..0] of TGPColor;
  //count: Integer;
  rc, gc, bc, AStart, AEnd: Byte;
  {$ENDIF}
begin
  {$IFDEF TMSGDIPLUS}
  R1 := R;
  R1.Bottom := R.Top + (R.Bottom - R.Top) div 2;
  R2 := R;
  R2.Top := R1.Bottom;
  w := R.Right - R.Left;

  AStart := 0;
  AEnd := 130;
  g := TGPGraphics.Create(Canvas.Handle);
  g.SetSmoothingMode(SmoothingModeAntiAlias);

  //--- Second part
  linGrBrush := TGPLinearGradientBrush.Create(MakeRect(R2.Left, R2.Top, w - 1, R2.Bottom - R2.Top), ColorToARGB(Clr), ColorToARGB(ClrTo), LinearGradientModeVertical);
  g.FillRectangle(linGrBrush, R2.Left, R2.Top, w - 1, R2.Bottom - R2.Top - 1);
  linGrBrush.Free;

  //--- First part
  linGrBrush := TGPLinearGradientBrush.Create(MakeRect(R1.Left, R1.Top, w - 1, R1.Bottom - R1.Top), ColorToARGB(Clr), ColorToARGB(Clr), LinearGradientModeVertical);
  g.FillRectangle(linGrBrush, R1.Left, R1.Top, w - 1, R1.Bottom - R1.Top);
  linGrBrush.Free;

  //--- shine
  rc := GetRValue(ShineClr);
  gc := GetGValue(ShineClr);
  bc := GetBValue(ShineClr);
  linGrBrush := TGPLinearGradientBrush.Create(MakeRect(R1.Left, R1.Top, w - 1, R1.Bottom - R1.Top), MakeColor(160, rc, gc, bc), MakeColor(90, rc, gc, bc), LinearGradientModeVertical);
  g.FillRectangle(linGrBrush, R1.Left, R1.Top, w - 1, R1.Bottom - R1.Top);
  linGrBrush.Free;
  //---

  //--- inner top border
  gppen := TGPPen.Create(ColorToARGB(clWhite),1);
  path := TGPGraphicsPath.Create;
  path.AddLine(R.Left, R.Top + 1, R.Left + (R.Right - R.Left), R.Top + 1);
  g.DrawPath(gpPen, path);
  path.Free;
  gppen.Free;

  //--- inner border
  gppen := TGPPen.Create(ColorToARGB(BrInClr{ $00E7E0C7}),1);
  path := TGPGraphicsPath.Create;
  path.AddLine(R.Left + 1, R.Top + 2, R.Left + 1, R.Bottom - 2);
  path.AddLine(R.Left + 1, R.Bottom - 2, R.Right - 1, R.Bottom - 2);
  path.AddLine(R.Right - 1, R.Bottom - 2, R.Right - 1, R.Top + 1);
  g.DrawPath(gpPen, path);
  path.Free;
  gppen.Free;

  //--- outer border
  gppen := TGPPen.Create(ColorToARGB(BrClr),1);
  g.DrawRectangle(gpPen, R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top - 1);
  gppen.Free;

  if (GlowClr <> clNone) then
  begin
    rc := GetRValue(GlowClr);
    gc := GetGValue(GlowClr);
    bc := GetBValue(GlowClr);
    gppen := TGPPen.Create(MakeColor(100, rc, gc, bc),1);
    g.DrawRectangle(gpPen, R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top - 1);
    gppen.Free;
  end;

  if (GlowClr <> clNone) then
  begin
    RG := R;
    //InflateRect(RG, BP_BUTTONGLOWSIZE, BP_BUTTONGLOWSIZE);

    //-- Left side
    path := TGPGraphicsPath.Create;
    path.AddLine(RG.Left - BP_BUTTONGLOWSIZE, RG.Top, RG.Left, RG.Top);
    path.AddLine(RG.Left, RG.Top, RG.Left, RG.Bottom);
    path.AddLine(RG.Left, RG.Bottom, RG.Left - BP_BUTTONGLOWSIZE, RG.Bottom);
    path.AddLine(RG.Left - BP_BUTTONGLOWSIZE, RG.Bottom, RG.Left - BP_BUTTONGLOWSIZE, RG.Top);
    path.CloseFigure;
    rc := GetRValue(GlowClr);
    gc := GetGValue(GlowClr);
    bc := GetBValue(GlowClr);
    linGrBrush := TGPLinearGradientBrush.Create(MakeRect(RG.Left - BP_BUTTONGLOWSIZE, RG.Top, BP_BUTTONGLOWSIZE, RG.Bottom - RG.Top), MakeColor(AStart, rc, gc, bc), MakeColor(AEnd, rc, gc, bc), LinearGradientModeHorizontal);
    g.FillPath(linGrBrush, path);
    linGrBrush.Free;
    path.Free;

    //-- Right side
    RG := Rect(R.Right-1, R.Top, R.Right + BP_BUTTONGLOWSIZE, R.Bottom);
    path := TGPGraphicsPath.Create;
    path.AddLine(RG.Left, RG.Top, RG.Right, RG.Top);
    path.AddLine(RG.Right, RG.Top, RG.Right, RG.Bottom);
    path.AddLine(RG.Right, RG.Bottom, RG.Left, RG.Bottom);
    path.AddLine(RG.Left, RG.Bottom, RG.Left, RG.Top);
    path.CloseFigure;
    linGrBrush := TGPLinearGradientBrush.Create(MakeRect(RG.Left, RG.Top, RG.Right - RG.Left, RG.Bottom - RG.Top), MakeColor(AEnd, rc, gc, bc), MakeColor(AStart, rc, gc, bc), LinearGradientModeHorizontal);
    g.FillPath(linGrBrush, path);
    linGrBrush.Free;
    path.Free;

    //-- Top side
    RG := Rect(R.Left, R.Top - BP_BUTTONGLOWSIZE, R.Right, R.Top);
    path := TGPGraphicsPath.Create;
    path.AddLine(RG.Left, RG.Top, RG.Right, RG.Top);
    path.AddLine(RG.Right, RG.Top, RG.Right, RG.Bottom);
    path.AddLine(RG.Right, RG.Bottom, RG.Left, RG.Bottom);
    path.AddLine(RG.Left, RG.Bottom, RG.Left, RG.Top);
    path.CloseFigure;
    linGrBrush := TGPLinearGradientBrush.Create(MakeRect(RG.Left, RG.Top, RG.Right - RG.Left, RG.Bottom - RG.Top), MakeColor(AStart, rc, gc, bc), MakeColor(AEnd, rc, gc, bc), LinearGradientModeVertical);
    g.FillPath(linGrBrush, path);
    linGrBrush.Free;
    path.Free;

    //-- Bottom side
    RG := Rect(R.Left, R.Bottom - 1, R.Right, R.Bottom + BP_BUTTONGLOWSIZE - 1);
    path := TGPGraphicsPath.Create;
    path.AddLine(RG.Left, RG.Top, RG.Right, RG.Top);
    path.AddLine(RG.Right, RG.Top, RG.Right, RG.Bottom);
    path.AddLine(RG.Right, RG.Bottom, RG.Left, RG.Bottom);
    path.AddLine(RG.Left, RG.Bottom, RG.Left, RG.Top);
    path.CloseFigure;
    linGrBrush := TGPLinearGradientBrush.Create(MakeRect(RG.Left, RG.Top, RG.Right - RG.Left, RG.Bottom - RG.Top), MakeColor(AEnd, rc, gc, bc), MakeColor(AStart, rc, gc, bc), LinearGradientModeVertical);
    g.FillPath(linGrBrush, path);
    linGrBrush.Free;
    path.Free;


    //-- LeftBottom corner
    RG := Rect(R.Left - BP_BUTTONGLOWSIZE + 2, R.Bottom - 1, R.Left, R.Bottom - 1 + BP_BUTTONGLOWSIZE - 2);
    path := TGPGraphicsPath.Create;
    path.AddLine(RG.Left, RG.Top, RG.Right, RG.Top);
    path.AddLine(RG.Right, RG.Top, RG.Right, RG.Bottom);
    path.AddLine(RG.Right, RG.Bottom, RG.Left, RG.Bottom);
    path.AddLine(RG.Left, RG.Bottom, RG.Left, RG.Top);
    path.CloseFigure;
    linGrBrush := TGPLinearGradientBrush.Create(MakeRect(RG.Left, RG.Top, RG.Right - RG.Left, RG.Bottom - RG.Top), MakeColor(AEnd, rc, gc, bc), MakeColor(AStart, rc, gc, bc), LinearGradientModeBackwardDiagonal);
    g.FillPath(linGrBrush, path);
    linGrBrush.Free;
    path.Free;

    //-- RightBottom corner
    RG := Rect(R.Right, R.Bottom, R.Right + BP_BUTTONGLOWSIZE - 3, R.Bottom + BP_BUTTONGLOWSIZE - 3);
    path := TGPGraphicsPath.Create;
    path.AddLine(RG.Left, RG.Top, RG.Right, RG.Top);
    path.AddLine(RG.Right, RG.Top, RG.Right, RG.Bottom);
    path.AddLine(RG.Right, RG.Bottom, RG.Left, RG.Bottom);
    path.AddLine(RG.Left, RG.Bottom, RG.Left, RG.Top);
    path.CloseFigure;
    linGrBrush := TGPLinearGradientBrush.Create(MakeRect(RG.Left, RG.Top, RG.Right - RG.Left, RG.Bottom - RG.Top), MakeColor(AEnd, rc, gc, bc), MakeColor(AStart, rc, gc, bc), LinearGradientModeForwardDiagonal);
    g.FillPath(linGrBrush, path);
    linGrBrush.Free;
    path.Free;

    //-- LeftTop corner
    RG := Rect(R.Left - BP_BUTTONGLOWSIZE + 2, R.Top - BP_BUTTONGLOWSIZE + 2, R.Left, R.Top);
    path := TGPGraphicsPath.Create;
    path.AddLine(RG.Left, RG.Top, RG.Right, RG.Top);
    path.AddLine(RG.Right, RG.Top, RG.Right, RG.Bottom);
    path.AddLine(RG.Right, RG.Bottom, RG.Left, RG.Bottom);
    path.AddLine(RG.Left, RG.Bottom, RG.Left, RG.Top);
    path.CloseFigure;
    linGrBrush := TGPLinearGradientBrush.Create(MakeRect(RG.Left, RG.Top, RG.Right - RG.Left, RG.Bottom - RG.Top), MakeColor(AStart, rc, gc, bc), MakeColor(AEnd, rc, gc, bc), LinearGradientModeForwardDiagonal);
    g.FillPath(linGrBrush, path);
    linGrBrush.Free;
    path.Free;

    //-- RightTop corner
    RG := Rect(R.Right, R.Top - BP_BUTTONGLOWSIZE + 2, R.Right + BP_BUTTONGLOWSIZE - 3, R.Top);
    path := TGPGraphicsPath.Create;
    path.AddLine(RG.Left, RG.Top, RG.Right, RG.Top);
    path.AddLine(RG.Right, RG.Top, RG.Right, RG.Bottom);
    path.AddLine(RG.Right, RG.Bottom, RG.Left, RG.Bottom);
    path.AddLine(RG.Left, RG.Bottom, RG.Left, RG.Top);
    path.CloseFigure;
    linGrBrush := TGPLinearGradientBrush.Create(MakeRect(RG.Left, RG.Top, RG.Right - RG.Left, RG.Bottom - RG.Top), MakeColor(AStart, rc, gc, bc), MakeColor(AEnd, rc, gc, bc), LinearGradientModeBackwardDiagonal);
    g.FillPath(linGrBrush, path);
    linGrBrush.Free;
    path.Free;

    {
    path := TGPGraphicsPath.Create;
    path.AddLine(RG.Left, RG.Top, RG.Right, RG.Top);
    path.AddLine(RG.Right, RG.Top, RG.Right, RG.Bottom);
    path.AddLine(RG.Right, RG.Bottom, RG.Left, RG.Bottom);
    path.AddLine(RG.Left, RG.Bottom, RG.Left, RG.Top);
    path.CloseFigure;
    pathBrush := TGPPathGradientBrush.Create(path);
    pathBrush.SetCenterColor(clRed);

    rc := GetRValue(HotClr);
    gc := GetGValue(HotClr);
    bc := GetBValue(HotClr);
    colors[0] := MakeColor(100, rc, gc, bc); //ColorToARGB(HotClr);
    count := 1;
    pathBrush.SetSurroundColors(@colors, count);
    pathBrush.SetFocusScales(0.7, 0.7);
    g.FillPath(pathBrush, path);
    path.Free;
    }
  end;
  g.Free;
  {$ENDIF}
end;

//------------------------------------------------------------------------------

{ TCalcButtonItem }

procedure TCalcButtonItem.Assign(Source: TPersistent);
begin
  if (Source is TCalcButtonItem) then
  begin
    Caption := (Source as TCalcButtonItem).Caption;
    CaptionColor := (Source as TCalcButtonItem).CaptionColor;
    FTag := (Source as TCalcButtonItem).Tag;
    ImageIndex := (Source as TCalcButtonItem).ImageIndex;
    Image.Assign((Source as TCalcButtonItem).Image);
    Enabled := (Source as TCalcButtonItem).Enabled;
    Name := (Source as TCalcButtonItem).Name;
  end
  else
    inherited Assign(Source);
end;

//------------------------------------------------------------------------------

constructor TCalcButtonItem.Create(Collection: TCollection);
begin
  inherited;
  FCaption := '';
  FCaptionColor := clWhite;
  FImageIndex := -1;
  FImage := TPicture.Create;
  FEnabled := True;
  FTag := 0;
  FName := '';
  FSpecial := False;
end;

//------------------------------------------------------------------------------

destructor TCalcButtonItem.Destroy;
begin
  FImage.Free;
  inherited;
end;

//------------------------------------------------------------------------------

function TCalcButtonItem.GetRect: TRect;
begin
  Result := Classes.Rect(Left, Top, Left + Width, Top + Height);
end;

//------------------------------------------------------------------------------

procedure TCalcButtonItem.SetCaption(const Value: TCaption);
begin
  if (FCaption <> Value) then
    FCaption := Value;
end;

//------------------------------------------------------------------------------

procedure TCalcButtonItem.SetCaptionColor(const Value: TColor);
begin
  if (FCaptionColor <> Value) then
  begin
    FCaptionColor := Value;
  end;
end;

//------------------------------------------------------------------------------

procedure TCalcButtonItem.SetImage(const Value: TPicture);
begin
  FImage.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TCalcButtonItem.SetTag(const Value: Integer);
begin
  FTag := Value;
end;

//------------------------------------------------------------------------------

{ TCalcButtonItems }

function TCalcButtonItems.Add: TCalcButtonItem;
begin
  Result := TCalcButtonItem(inherited Add);
end;

//------------------------------------------------------------------------------

constructor TCalcButtonItems.Create(AOwner: TPersistent);
begin
  inherited Create(TCalcButtonItem);
  FMyOwner := AOwner;
end;

//------------------------------------------------------------------------------

function TCalcButtonItems.GetItem(Index: Integer): TCalcButtonItem;
begin
  Result := TCalcButtonItem(inherited Items[Index]);
end;

//------------------------------------------------------------------------------

function TCalcButtonItems.GetOwner: TPersistent;
begin
  Result := FMyOwner;
end;

//------------------------------------------------------------------------------

function TCalcButtonItems.Insert(Index: Integer): TCalcButtonItem;
begin
  Result := TCalcButtonItem(inherited Insert(Index));
end;

//------------------------------------------------------------------------------

procedure TCalcButtonItems.SetItem(Index: Integer;
  const Value: TCalcButtonItem);
begin
  inherited Items[Index] := Value;
end;

//------------------------------------------------------------------------------

{ TButtonAppearance }

constructor TButtonAppearance.Create;
begin
  inherited;
  FColor := RGB(95, 145, 154);
  FColorTo := RGB(112, 161, 176);
  FShineColor := clWhite;

  FColorHot := RGB(28, 111, 159);
  FColorHotTo := RGB(68, 194, 213);
  FGlowColorHot := RGB(68, 194, 213);

  FColorDown := RGB(8, 47, 75);
  FColorDownTo := RGB(20, 181, 173);
  FGlowColorDown := RGB(68, 194, 213);

  FColorChecked := BrightnessColor($F5F0E1,-10,-10,0);
  FColorCheckedTo := BrightnessColor($F9D2B2, -10,-10,0);

  FColorDisabled := BrightnessColor(clWhite,-5,-5,-5);
  FColorDisabledTo := BrightnessColor(clWhite, -5,-5,-5);

  FBorderColor := clBlack;
  FBorderColorHot := clBlack; //RGB(111, 174, 205);
  FBorderColorDown := clBlack; // RGB(102, 173, 201);
  FBorderColorChecked := clBlue;

  FFont := TFont.Create;
  FFont.Size := 10;
  FFont.Style := [fsBold];

  FTextColor := clBlack;
  FSpecialTextColor := clMaroon;
end;

//------------------------------------------------------------------------------

destructor TButtonAppearance.Destroy;
begin
  FFont.Free;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TButtonAppearance.Assign(Source: TPersistent);
begin
  if (Source is TButtonAppearance) then
  begin
    Color := (Source as TButtonAppearance).Color;
    ColorTo := (Source as TButtonAppearance).ColorTo;
    ShineColor := (Source as TButtonAppearance).ShineColor;

    ColorHot := (Source as TButtonAppearance).ColorHot;
    ColorHotTo := (Source as TButtonAppearance).ColorHotTo;
    GlowColorHot := (Source as TButtonAppearance).GlowColorHot;

    ColorDown := (Source as TButtonAppearance).ColorDown;
    ColorDownTo := (Source as TButtonAppearance).ColorDownTo;
    GlowColorDown := (Source as TButtonAppearance).GlowColorDown;

    ColorChecked := (Source as TButtonAppearance).ColorChecked;
    ColorCheckedTo := (Source as TButtonAppearance).ColorCheckedTo;

    ColorDisabled := (Source as TButtonAppearance).ColorDisabled;
    ColorDisabledTo := (Source as TButtonAppearance).ColorDisabledTo;

    BorderColor := (Source as TButtonAppearance).BorderColor;
    BorderColorHot := (Source as TButtonAppearance).BorderColorHot;
    BorderColorDown := (Source as TButtonAppearance).BorderColorDown;
    BorderColorChecked := (Source as TButtonAppearance).BorderColorChecked;

    Font.Assign((Source as TButtonAppearance).Font);
    TextColor := (Source as TButtonAppearance).TextColor;
    SpecialTextColor := (Source as TButtonAppearance).SpecialTextColor;
  end
  else
    inherited Assign(Source);
end;

//------------------------------------------------------------------------------

procedure TButtonAppearance.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

//------------------------------------------------------------------------------

procedure TButtonAppearance.SetBorderColor(const Value: TColor);
begin
  if (FBorderColor <> Value) then
  begin
    FBorderColor := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TButtonAppearance.SetBorderColorChecked(const Value: TColor);
begin
  if (FBorderColorChecked <> Value) then
  begin
    FBorderColorChecked := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TButtonAppearance.SetBorderColorDown(const Value: TColor);
begin
  if (FBorderColorDown <> Value) then
  begin
    FBorderColorDown := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TButtonAppearance.SetBorderColorHot(const Value: TColor);
begin
  if (FBorderColorHot <> Value) then
  begin
    FBorderColorHot := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TButtonAppearance.SetColor(const Value: TColor);
begin
  if (FColor <> Value) then
  begin
    FColor := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TButtonAppearance.SetColorChecked(const Value: TColor);
begin
  if (FColorChecked <> Value) then
  begin
    FColorChecked := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TButtonAppearance.SetColorCheckedTo(const Value: TColor);
begin
  if (FColorCheckedTo <> Value) then
  begin
    FColorCheckedTo := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TButtonAppearance.SetColorDisabled(const Value: TColor);
begin
  if (FColorDisabled <> Value) then
  begin
    FColorDisabled := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TButtonAppearance.SetColorDisabledTo(const Value: TColor);
begin
  if (FColorDisabledTo <> Value) then
  begin
    FColorDisabledTo := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TButtonAppearance.SetColorDown(const Value: TColor);
begin
  if (FColorDown <> Value) then
  begin
    FColorDown := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TButtonAppearance.SetColorDownTo(const Value: TColor);
begin
  if (FColorDownTo <> Value) then
  begin
    FColorDownTo := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TButtonAppearance.SetColorHot(const Value: TColor);
begin
  if (FColorHot <> Value) then
  begin
    FColorHot := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TButtonAppearance.SetColorHotTo(const Value: TColor);
begin
  if (FColorHotTo <> Value) then
  begin
    FColorHotTo := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TButtonAppearance.SetColorTo(const Value: TColor);
begin
  if (FColorTo <> Value) then
  begin
    FColorTo := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TButtonAppearance.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TButtonAppearance.SetShineColor(const Value: TColor);
begin
  FShineColor := Value;
end;

//------------------------------------------------------------------------------

procedure TButtonAppearance.SetSpecialTextColor(const Value: TColor);
begin
  if (FSpecialTextColor <> Value) then
  begin
    FSpecialTextColor := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TButtonAppearance.SetTextColor(const Value: TColor);
begin
  if (FTextColor <> Value) then
  begin
    FTextColor := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

{ TCustomButtonPanel }

constructor TCustomButtonPanel.Create(AOwner: TComponent);
var
  VerInfo: TOSVersioninfo;
begin
  inherited;
  FButtons := TCalcButtonItems.Create(Self);
  FButtons.OnChange := OnButtonsChanged;
  FButtonLayout := ilCaptionRight;
  FButtonHot := -1;
  FButtonDown := -1;
  FOffSetX := BP_BUTTONGLOWSIZE;
  FOffSetY := BP_BUTTONGLOWSIZE;
  FButtonOffSetX := 4;
  FButtonOffSetY := 4;
  FButtonPanelType := plCalculator;
  FButtonAppearance := TButtonAppearance.Create;
  FButtonAppearance.OnChange := OnButtonAppearanceChanged;
  DoubleBuffered := True;
  
  VerInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  Windows.GetVersionEx(verinfo);
  FIsWinXP := (verinfo.dwMajorVersion > 5) or
    ((verinfo.dwMajorVersion = 5) and (verinfo.dwMinorVersion >= 1));
end;

//------------------------------------------------------------------------------

destructor TCustomButtonPanel.Destroy;
begin
  FButtons.Free;
  FButtonAppearance.Free;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TCustomButtonPanel.Loaded;
begin
  inherited;

  if not Assigned(AdvDropDown) then
    Initialize;
end;

//------------------------------------------------------------------------------

procedure TCustomButtonPanel.SetButtonHot(const Value: Integer);
var
  i: Integer;
begin
  if (FButtonHot <> Value) then
  begin
    if (Value >= 0) and (Value < FButtons.Count) then
      if not FButtons[Value].Enabled then
        Exit;

    if (FButtonHot >= 0) then  // refresh old hot Button
    begin
      i := FButtonHot;
      FButtonHot := -1;
      InvalidateButton(i);
    end;

    FButtonHot := Value;
    if (FButtonHot >= 0) then
      DrawButton(FButtonHot, Canvas);
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomButtonPanel.SetButtonDown(const Value: Integer);
var
  i: Integer;
begin
  if (FButtonDown <> Value) then
  begin
    if (Value >= 0) and (Value < FButtons.Count) then
      if not FButtons[Value].Enabled then
        Exit;

    if (FButtonDown >= 0) then  // refresh old down Button if any
    begin
      i := FButtonDown;
      FButtonDown := -1;
      InvalidateButton(i);
    end;

    FButtonDown := Value;
    if (FButtonDown >= 0) then
      DrawButton(FButtonDown, Canvas);
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomButtonPanel.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  i: Integer;
begin
  inherited;

  i := ButtonAtPos(X, Y);
  if (i >= 0) then
  begin
    if FButtons[i].Enabled then
    begin
      ButtonDown := i;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomButtonPanel.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  i: Integer;
begin
  inherited;

  i := ButtonAtPos(X, Y);
  if (i >= 0) then
  begin
    if (i <> FButtonHot) then
    begin
      if FButtons[i].Enabled then
        ButtonHot := i
      else
        ButtonHot := -1;
    end;
  end
  else
  begin
    if (FButtonHot >= 0) then
    begin
      ButtonHot := -1;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomButtonPanel.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  i: Integer;
begin
  inherited;

  if (ButtonDown >= 0) then
  begin
    i := ButtonAtPos(X, Y);
    if (i <> ButtonDown) then
      i := -1;
    ButtonDown := -1;
    if (i >= 0) then
      ButtonClick(i);
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomButtonPanel.ButtonClick(Index: Integer);
begin
  if Assigned(FOnButtonClick) then
    FOnButtonClick(Self, Index);
end;

//------------------------------------------------------------------------------

procedure TCustomButtonPanel.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  
  if not (csDestroying in ComponentState) and (Operation = opRemove) then
  begin
    if (AComponent = Images) then
      Images := nil;
    if (AComponent = FEdit) then
      Edit := nil;  
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomButtonPanel.Paint;
begin
  inherited;
  if not Assigned(AdvDropDown) then
  begin
    Canvas.Pen.Color := clBlack;
    Canvas.Brush.Style := bsClear;
    Canvas.Rectangle(ClientRect);
  end;
  DrawButtons(Canvas, False);
  if (FButtonHot >= 0) then
    DrawButton(FButtonHot, Canvas);
end;

//------------------------------------------------------------------------------

procedure TCustomButtonPanel.DrawButton(Index: Integer; aCanvas: TCanvas);
  procedure DrawButtonThemeBackGround(R: TRect);
  var
    HTheme: THandle;
  begin
    FillRect(aCanvas.Handle,r, aCanvas.Brush.Handle);

    HTheme := OpenThemeData(Handle,'button');

    if not Enabled then
      DrawThemeBackground(HTheme, aCanvas.Handle, BP_PUSHBUTTON,PBS_DISABLED,@r,nil)
    else
    begin
      if (Index = FButtonDown) then
        DrawThemeBackground(HTheme, aCanvas.Handle, BP_PUSHBUTTON,PBS_PRESSED,@r,nil)
      else
        if (Index = FButtonHot) then
          DrawThemeBackground(HTheme, aCanvas.Handle, BP_PUSHBUTTON,PBS_HOT,@r,nil)
        else
          DrawThemeBackground(HTheme, aCanvas.Handle, BP_PUSHBUTTON,PBS_NORMAL,@r,nil);
    end;
    CloseThemeData(HTheme);
  end;
var
  BrClr, Clr, ClrTo, GClr: TColor;
  R, TR, IR: TRect;
  iw, ih: Integer;
  ts: TSize;
  DTFLAG: DWORD;
  GDIPlusDraw: Boolean;
begin
  if (Index < 0) or (Index >= FButtons.Count) or not Assigned(aCanvas) then
    Exit;

  R := GetButtonRect(Index);
  if (R.Left < 0) and (R.Right < 0) then
    Exit;

  BrClr := ButtonAppearance.BorderColor;  
  Clr := ButtonAppearance.Color;
  ClrTo := ButtonAppearance.ColorTo;
  GClr := clNone;

  if (Index = FButtonHot) then
  begin
    BrClr := ButtonAppearance.BorderColorHot;
    Clr := ButtonAppearance.FColorHot;
    ClrTo := ButtonAppearance.ColorHotTo;
    GClr := ButtonAppearance.GlowColorHot;
  end;
  if (Index = FButtonDown) then
  begin
    BrClr := ButtonAppearance.BorderColorDown;
    Clr := ButtonAppearance.ColorDown;
    ClrTo := ButtonAppearance.ColorDownTo;
    GClr := ButtonAppearance.GlowColorDown;
  end;

  {$IFDEF TMSGDIPLUS}
  GDIPlusDraw := True;
  {$ELSE}
  GDIPlusDraw := False;
  {$ENDIF}
  
  //--- Draw Background
  if GDIPlusDraw then
    DrawButtonBackground(aCanvas, R, BrClr, $00E7E0C7, Clr, ClrTo, GClr, ButtonAppearance.ShineColor)
  else
    DrawButtonThemeBackGround(R);
  //---

  iw := 0;
  ih := 0;
  DTFLAG := DT_CENTER;
  if Assigned(FButtons[Index].Image.Graphic) and not FButtons[Index].Image.Graphic.Empty then
  begin
    iw := FButtons[Index].Image.Graphic.Width;
    ih := FButtons[Index].Image.Graphic.Height;
    DTFLAG := DT_LEFT;
  end
  else if Assigned(Images) and (FButtons[Index].ImageIndex >= 0) then
  begin
    iw := Images.Width;
    ih := Images.Height;
    DTFLAG := DT_LEFT;
  end;

  TR := R;
  InflateRect(TR, -ButtonOffSetX, -ButtonOffSetY);
  IR := TR;

  aCanvas.Font.Assign(ButtonAppearance.Font);
  {if GDIPlusDraw or (FButtons[Index].CaptionColor <> clWhite) then
    aCanvas.Font.Color := FButtons[Index].CaptionColor
  else
    aCanvas.Font.Color := clBlack;}
  if FButtons[Index].Special then
    aCanvas.Font.Color := ButtonAppearance.SpecialTextColor
  else
    aCanvas.Font.Color := ButtonAppearance.TextColor;
  aCanvas.Brush.Style := bsClear;
  case ButtonLayout of
    ilCaptionLeft:
    begin
      if (FButtons[Index].Caption <> '') then
      begin
        TR.Right := TR.Right - iw;
        ts := GetTextSize(aCanvas, FButtons[Index].Caption);
        DrawText(aCanvas.Handle, PChar(FButtons[Index].Caption), Length(FButtons[Index].Caption), TR, DTFLAG or DT_VCENTER or DT_SINGLELINE);
        IR.Left := TR.Left + ts.cx + CaptionGap;
      end;

      if Assigned(FButtons[Index].Image.Graphic) and not FButtons[Index].Image.Graphic.Empty then
      begin
        aCanvas.Draw(IR.Left, IR.Top + ((IR.Bottom - IR.Top - ih) div 2), buttons[Index].Image.Graphic);
      end
      else if Assigned(Images) and (buttons[Index].ImageIndex >= 0) then
      begin
        Images.Draw(aCanvas, IR.Left, IR.Top + ((IR.Bottom - IR.Top - ih) div 2), buttons[Index].ImageIndex, Buttons[Index].Enabled);
      end;
    end;
    ilCaptionRight:
    begin
      if Assigned(FButtons[Index].Image.Graphic) and not Buttons[Index].Image.Graphic.Empty then
      begin
        aCanvas.Draw(IR.Left, IR.Top + ((IR.Bottom - IR.Top - ih) div 2), Buttons[Index].Image.Graphic);
        TR.Left := IR.Left + Buttons[Index].Image.Graphic.Width + CaptionGap;
      end
      else if Assigned(Images) and (Buttons[Index].ImageIndex >= 0) then
      begin
        Images.Draw(aCanvas, IR.Left, IR.Top + ((IR.Bottom - IR.Top - ih) div 2), Buttons[Index].ImageIndex, Buttons[Index].Enabled);
        TR.Left := IR.Left + Images.Width + CaptionGap;
      end;

      if (Buttons[Index].Caption <> '') then
      begin
        DrawText(aCanvas.Handle, PChar(FButtons[Index].Caption), Length(FButtons[Index].Caption), TR, DTFLAG or DT_VCENTER or DT_SINGLELINE);
      end;
    end;
    ilCaptionTop:
    begin
      if (Buttons[Index].Caption <> '') then
      begin
        TR.Bottom := TR.Bottom - ih;
        ts := GetTextSize(aCanvas, Buttons[Index].Caption);
        DrawText(aCanvas.Handle, PChar(FButtons[Index].Caption), Length(FButtons[Index].Caption), TR, DT_CENTER or DT_VCENTER or DT_SINGLELINE);
        IR.Top := TR.Top + ts.cy + CaptionGap;
      end
      else
        IR.Top := IR.Top + ((IR.Bottom - IR.Top - ih) div 2);

      if Assigned(FButtons[Index].Image.Graphic) and not Buttons[Index].Image.Graphic.Empty then
      begin
        aCanvas.Draw(IR.Left, IR.Top, Buttons[Index].Image.Graphic);
      end
      else if Assigned(Images) and (Buttons[Index].ImageIndex >= 0) then
      begin
        Images.Draw(aCanvas, IR.Left, IR.Top, Buttons[Index].ImageIndex, Buttons[Index].Enabled);
      end;
    end;
    ilCaptionBottom:
    begin
      if (Buttons[Index].Caption = '') then
        IR.Top := IR.Top + ((IR.Bottom - IR.Top - ih) div 2);

      if Assigned(FButtons[Index].Image.Graphic) and not Buttons[Index].Image.Graphic.Empty then
      begin
        aCanvas.Draw(IR.Left, IR.Top, Buttons[Index].Image.Graphic);
        TR.Top := IR.Top + Buttons[Index].Image.Graphic.Width + CaptionGap;
      end
      else if Assigned(Images) and (Buttons[Index].ImageIndex >= 0) then
      begin
        Images.Draw(aCanvas, IR.Left, IR.Top, Buttons[Index].ImageIndex, Buttons[Index].Enabled);
        TR.Top := IR.Top + Images.Width + CaptionGap;
      end;

      if (Buttons[Index].Caption <> '') then
      begin
        DrawText(aCanvas.Handle, PChar(FButtons[Index].Caption), Length(FButtons[Index].Caption), TR, DT_CENTER or DT_VCENTER or DT_SINGLELINE);
      end;
    end;
    
  end;  // end case

end;

//------------------------------------------------------------------------------

procedure TCustomButtonPanel.ClearButtons;
begin
  Buttons.Clear;
end;

//------------------------------------------------------------------------------

procedure TCustomButtonPanel.ReInitialize;
begin
  ClearButtons;
  Initialize;
end;

//------------------------------------------------------------------------------

procedure TCustomButtonPanel.Initialize;
begin
  if (Buttons.Count > 0) then
    Exit;

  case ButtonPanelType of
    plCalculator:
    begin
      with Buttons.Add do
      begin
        Caption := '';
        Name := 'BTN_ON';
        Tag := 30;
      end;
      with Buttons.Add do
      begin
        Caption := 'Back';
        Name := 'BTN_BACK';
        CaptionColor := clMaroon;
        Tag := 31;
        Special := True;
      end;
      with Buttons.Add do
      begin
        Caption := 'CE';
        Name := 'BTN_CE';
        CaptionColor := clMaroon;
        Tag := 32;
        Special := True;
      end;
      with Buttons.Add do
      begin
        Caption := 'C';
        Name := 'BTN_C';
        CaptionColor := clMaroon;
        Tag := 16;
        Special := True;
      end;

      with Buttons.Add do
      begin
        Caption := 'MC';
        Name := 'BTN_MC';
        CaptionColor := clMaroon;
        Tag := 33;
        Special := True;
      end;
      with Buttons.Add do
      begin
        Caption := 'MR';
        Name := 'BTN_MR';
        CaptionColor := clMaroon;
        Tag := 34;
        Special := True;
      end;
      with Buttons.Add do
      begin
        Caption := 'MS';
        Name := 'BTN_MS';
        CaptionColor := clMaroon;
        Tag := 35;
        Special := True;
      end;
      with Buttons.Add do
      begin
        Caption := 'M+';
        Name := 'BTN_M+';
        CaptionColor := clMaroon;
        Tag := 36;
        Special := True;
      end;

      with Buttons.Add do
      begin
        Caption := '0';
        Name := 'BTN_0';
        Tag := 0;
      end;
      with Buttons.Add do
      begin
        Caption := '1';
        Name := 'BTN_1';
        Tag := 1;
      end;
      with Buttons.Add do
      begin
        Caption := '4';
        Name := 'BTN_4';
        Tag := 4;
      end;
      with Buttons.Add do
      begin
        Caption := '7';
        Name := 'BTN_7';
        Tag := 7;
      end;

      with Buttons.Add do
      begin
        Caption := '8';
        Name := 'BTN_8';
        Tag := 8;
      end;
      with Buttons.Add do
      begin
        Caption := '5';
        Name := 'BTN_5';
        Tag := 5;
      end;
      with Buttons.Add do
      begin
        Caption := '2';
        Name := 'BTN_2';
        Tag := 2;
      end;
      with Buttons.Add do
      begin
        Caption := '+/-';
        Name := 'BTN_+/-';
        Tag := 14;
      end;

      with Buttons.Add do
      begin
        Caption := '.';
        Name := 'BTN_.';
        Tag := 15;
      end;
      with Buttons.Add do
      begin
        Caption := '3';
        Name := 'BTN_3';
        Tag := 3;
      end;
      with Buttons.Add do
      begin
        Caption := '6';
        Name := 'BTN_6';
        Tag := 6;
      end;
      with Buttons.Add do
      begin
        Caption := '9';
        Name := 'BTN_9';
        Tag := 9;
      end;

      with Buttons.Add do
      begin
        Caption := '/';
        Name := 'BTN_/';
        CaptionColor := clMaroon;
        Tag := 13;
        Special := True;
      end;
      with Buttons.Add do
      begin
        Caption := '*';
        Name := 'BTN_*';
        CaptionColor := clMaroon;
        Tag := 12;
        Special := True;
      end;
      with Buttons.Add do
      begin
        Caption := '-';
        Name := 'BTN_-';
        CaptionColor := clMaroon;
        Tag := 11;
        Special := True;
      end;
      with Buttons.Add do
      begin
        Caption := '+';
        Name := 'BTN_+';
        CaptionColor := clMaroon;
        Tag := 10;
        Special := True;
      end;

      with Buttons.Add do
      begin
        Caption := '=';
        Name := 'BTN_=';
        CaptionColor := clMaroon;
        Tag := 17;
        Special := True;
      end;
      with Buttons.Add do
      begin
        Caption := '1/x';
        Name := 'BTN_1/x';
        Tag := 38;
      end;
      with Buttons.Add do
      begin
        Caption := '%';
        Name := 'BTN_%';
        Tag := 18;
      end;
      with Buttons.Add do
      begin
        Caption := 'sqrt';
        Name := 'BTN_sqrt';
        CaptionColor := clMaroon;
        Tag := 37;
        Special := True;
      end;

    end;
    plCustom:
    begin
    end;
  end;
  
  ArrangeButtons;
end;

//------------------------------------------------------------------------------
{  Buttons order/sequence
 0   1    2    3

 4  11 12 19  20 27
 5  10 13 18  21 26
 6  9  14 17  22 25
 7  8  15 16  23 24
}

procedure TCustomButtonPanel.ArrangeButtons;
var
  R: TRect;
  bw, bh, i, sp, X, Y: Integer;
begin
  if (Buttons.Count <= 0) then
    Exit;

  R := GetInnerRect;
  case ButtonPanelType of
    plCalculator:
    begin
      sp := 4;
      X := R.Left;
      Y := R.Top;

      bw := ((R.Right - R.Left) - sp * 2) div 6;
      bh := ((R.Bottom - R.Top) - sp) div 5;

      with Buttons[0] do
      begin
        Width := bw;
        Height := bh;
        Left := X;
        Top := Y;
      end;

      Y := Y + bh + sp;
      with Buttons[4] do
      begin
        Width := bw;
        Height := bh;
        Left := X;
        Top := Y;
      end;
      Y := Y + bh - 1;
      with Buttons[5] do
      begin
        Width := bw;
        Height := bh;
        Left := X;
        Top := Y;
      end;
      Y := Y + bh - 1;
      with Buttons[6] do
      begin
        Width := bw;
        Height := bh;
        Left := X;
        Top := Y;
      end;
      Y := Y + bh - 1;
      with Buttons[7] do
      begin
        Width := bw;
        Height := bh;
        Left := X;
        Top := Y;
      end;

      X := X + bw + sp;
      with Buttons[8] do
      begin
        Width := bw;
        Height := bh;
        Left := X;
        Top := Y;
      end;
      Y := Y - bh + 1;
      with Buttons[9] do
      begin
        Width := bw;
        Height := bh;
        Left := X;
        Top := Y;
      end;
      Y := Y - bh + 1;
      with Buttons[10] do
      begin
        Width := bw;
        Height := bh;
        Left := X;
        Top := Y;
      end;
      Y := Y - bh + 1;
      with Buttons[11] do
      begin
        Width := bw;
        Height := bh;
        Left := X;
        Top := Y;
      end;

      X := X + bw - 1;
      with Buttons[12] do
      begin
        Width := bw;
        Height := bh;
        Left := X;
        Top := Y;
      end;
      Y := Y + bh - 1;
      with Buttons[13] do
      begin
        Width := bw;
        Height := bh;
        Left := X;
        Top := Y;
      end;
      Y := Y + bh - 1;
      with Buttons[14] do
      begin
        Width := bw;
        Height := bh;
        Left := X;
        Top := Y;
      end;
      Y := Y + bh - 1;
      with Buttons[15] do
      begin
        Width := bw;
        Height := bh;
        Left := X;
        Top := Y;
      end;

      X := X + bw - 1;
      with Buttons[16] do
      begin
        Width := bw;
        Height := bh;
        Left := X;
        Top := Y;
      end;
      Y := Y - bh + 1;
      with Buttons[17] do
      begin
        Width := bw;
        Height := bh;
        Left := X;
        Top := Y;
      end;
      Y := Y - bh + 1;
      with Buttons[18] do
      begin
        Width := bw;
        Height := bh;
        Left := X;
        Top := Y;
      end;
      Y := Y - bh + 1;
      with Buttons[19] do
      begin
        Width := bw;
        Height := bh;
        Left := X;
        Top := Y;
      end;

      X := X + bw + sp;
      with Buttons[20] do
      begin
        Width := bw;
        Height := bh;
        Left := X;
        Top := Y;
      end;
      Y := Y + bh - 1;
      with Buttons[21] do
      begin
        Width := bw;
        Height := bh;
        Left := X;
        Top := Y;
      end;
      Y := Y + bh - 1;
      with Buttons[22] do
      begin
        Width := bw;
        Height := bh;
        Left := X;
        Top := Y;
      end;
      Y := Y + bh - 1;
      with Buttons[23] do
      begin
        Width := bw;
        Height := bh;
        Left := X;
        Top := Y;
      end;

      X := X + bw - 1;
      with Buttons[24] do
      begin
        Width := bw;
        Height := bh;
        Left := X;
        Top := Y;
      end;
      Y := Y - bh + 1;
      with Buttons[25] do
      begin
        Width := bw;
        Height := bh;
        Left := X;
        Top := Y;
      end;
      Y := Y - bh + 1;
      with Buttons[26] do
      begin
        Width := bw;
        Height := bh;
        Left := X;
        Top := Y;
      end;
      Y := Y - bh + 1;
      with Buttons[27] do
      begin
        Width := bw;
        Height := bh;
        Left := X;
        Top := Y;
      end;

      i := X + bw;
      X := Buttons[0].Left + Buttons[0].Width + sp;
      Y := Buttons[0].Top;
      bw := ((i - X) - (sp * 2)) div 3;

      with Buttons[1] do
      begin
        Width := bw;
        Height := bh;
        Left := X;
        Top := Y;
      end;
      X := X + bw + sp;
      with Buttons[2] do
      begin
        Width := bw;
        Height := bh;
        Left := X;
        Top := Y;
      end;
      X := X + bw + sp;
      with Buttons[3] do
      begin
        Left := X;
        Top := Y;
        Height := bh;
        Width := i - X;
      end;

    end;
    plCustom:
    begin
    end;
  end;
end;

//------------------------------------------------------------------------------

function TCustomButtonPanel.GetMaxButtonSize: TSize;
var
  i, ih, iw, th, tw: Integer;
  ts: TSize;
  R: TRect;
begin
  Result.cx := 0;
  Result.cy := 0;
  ih := 0;  // Image height
  iw := 0;  // Image width

  th := 0;
  tw := 0;
  Canvas.Font.Assign(Self.Font);
  R := Rect(0, 0, 1000, 500);
  for i := 0 to Buttons.Count -1 do
  begin
    if Assigned(Buttons[i].Image.Graphic) and not Buttons[i].Image.Graphic.Empty then
    begin
      ih := max(ih, Buttons[i].Image.Graphic.Height);
      iw := max(iw, Buttons[i].Image.Graphic.Width);
    end
    else if Assigned(Images) and (Buttons[i].ImageIndex >= 0) then
    begin
      ih := max(ih, Images.Height);
      iw := max(iw, Images.Width);
    end;

    ts := GetTextSize(Canvas, Buttons[i].Caption);
    tw := max(tw, ts.cx);
    th := max(th, ts.cy);
  end; // end for

  case ButtonLayout of
    ilCaptionLeft, ilCaptionRight:
    begin
      Result.cx := FButtonOffSetX * 2;
      if (iw > 0) then
        Result.cx := Result.cx + iw;
      if (tw > 0) then
      begin
        if (iw > 0) then
          Result.cx := Result.cx + CaptionGap;
        Result.cx := Result.cx + tw;
      end;

      Result.cy := ButtonOffSetY * 2 + Max(th, ih);
    end;
    ilCaptionTop, ilCaptionBottom:
    begin
      Result.cx := FButtonOffSetX * 2 + Max(tw, iw);

      Result.cy := ButtonOffSetY * 2;
      if (ih > 0) then
        Result.cy := Result.cy + ih;
      if (th > 0) then
      begin
        if (ih > 0) then
          Result.cy := Result.cy + CaptionGap;
        Result.cy := Result.cy + th;  
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomButtonPanel.SetButtons(const Value: TCalcButtonItems);
begin
  FButtons.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TCustomButtonPanel.WMSize(var Message: TWMSize);
begin
  inherited;
  ArrangeButtons;
end;

//------------------------------------------------------------------------------

procedure TCustomButtonPanel.SetButtonLayout(const Value: TItemLayout);
begin
  if (FButtonLayout <> Value) then
  begin
    FButtonLayout := Value;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomButtonPanel.OnButtonsChanged(Sender: TObject);
begin
  if not Assigned(AdvDropDown) then
    Invalidate;
end;

//------------------------------------------------------------------------------

function TCustomButtonPanel.GetButtonRect(Index: Integer): TRect;
begin
  Result := Rect(-1, -1, -1, -1);
  if (Index >= 0) and (Index < FButtons.Count) then
    Result := Buttons[Index].Rect;
end;

//------------------------------------------------------------------------------

procedure TCustomButtonPanel.InvalidateButton(Index: Integer);
var
  R: TRect;
begin
  if (Index >= 0) and (Index < Buttons.Count) then
  begin
    R := GetButtonRect(Index);
    InflateRect(R, BP_BUTTONGLOWSIZE, BP_BUTTONGLOWSIZE);
    
    InvalidateRect(Handle, @R, True);

  end;
end;

//------------------------------------------------------------------------------

function TCustomButtonPanel.ButtonAtPos(X, Y: Integer): Integer;
var
  i: Integer;
  P: TPoint;
begin
  Result := -1;
  for i := 0 to FButtons.Count - 1 do
  begin
    P := Point(X, Y);
    if PtInRect(FButtons[i].Rect, P) then
    begin
      Result := i;
      Break;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomButtonPanel.DrawButtons(aCanvas: TCanvas; DrawHot: Boolean = true);
var
  i: Integer;
begin
  for i := 0 to FButtons.Count - 1 do
  begin
    if DrawHot or (i <> FButtonHot) then
      DrawButton(i, aCanvas);
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomButtonPanel.SetItemColorStyle(
  const Value: TSelectionColorStyle);
begin
  FItemColorStyle := Value;
end;

//------------------------------------------------------------------------------

function TCustomButtonPanel.GetInnerRect: TRect;
begin
  Result := ClientRect;
  InflateRect(Result, -FOffSetX, -FOffSetY);
end;

//------------------------------------------------------------------------------

procedure TCustomButtonPanel.SetImages(const Value: TCustomImageList);
begin
  if (FImages <> Value) then
  begin
    FImages := Value;
    ArrangeButtons;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomButtonPanel.SetButtonOffSetX(const Value: Integer);
begin
  if (FButtonOffSetX <> Value) then
  begin
    FButtonOffSetX := Value;
    ArrangeButtons;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomButtonPanel.SetButtonOffSetY(const Value: Integer);
begin
  if (FButtonOffSetY <> Value) then
  begin
    FButtonOffSetY := Value;
    ArrangeButtons;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomButtonPanel.SetCaptionGap(const Value: Integer);
begin
  if (FCaptionGap <> Value) then
  begin
    FCaptionGap := Value;
    ArrangeButtons;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomButtonPanel.SetEdit(const Value: TCustomEdit);
begin
  if (FEdit <> Value) then
  begin
    FEdit := Value;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomButtonPanel.SetButtonPanelType(
  const Value: TButtonPanelType);
begin
  if (FButtonPanelType <> Value) then
  begin
    FButtonPanelType := Value;
    ArrangeButtons;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomButtonPanel.SetButtonAppearance(
  const Value: TButtonAppearance);
begin
  FButtonAppearance.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TCustomButtonPanel.OnButtonAppearanceChanged(Sender: TObject);
begin
  if not Assigned(AdvDropDown) then
    Invalidate;
end;

//------------------------------------------------------------------------------

procedure TCustomButtonPanel.SetAdvDropDown(
  const Value: TAdvCustomDropDown);
begin
  if (FAdvDropDown <> Value) then
    FAdvDropDown := Value;
end;

//------------------------------------------------------------------------------

procedure TCustomButtonPanel.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if (ButtonHot >= 0) then
    ButtonHot := -1;
end;

//------------------------------------------------------------------------------

function TCustomButtonPanel.DoVisualStyles: Boolean;
begin
  if FIsWinXP then
    Result := IsThemeActive
  else
    Result := False;
end;

//------------------------------------------------------------------------------

{ TAdvCalculatorDropdown }

constructor TAdvCalculatorDropdown.Create(AOwner: TComponent);
begin
  FCalcButtonAppearance := TButtonAppearance.Create;

  inherited;
  DropDownColor := clWhite;
  DropDownWidth := 230;
  DropDownHeader.Visible := False;
  DropDownFooter.Visible := False;
  EditType := etFloat;
  FPrevop := -1;
  Precision := 2;
  FDecim := 0;
  FCalcClosed := True;
  Signed := true;
  FMVal := 0;
end;

//------------------------------------------------------------------------------

destructor TAdvCalculatorDropdown.Destroy;
begin
  FCalcButtonAppearance.Free;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvCalculatorDropdown.CreateDropDownForm;
begin
  inherited;
  if not Assigned(FAdvButtonPanel) then
  begin
    FAdvButtonPanel := TCustomButtonPanel.Create(Self);
    FAdvButtonPanel.Parent := FDropDownForm;
    FAdvButtonPanel.AdvDropDown := Self;
    FAdvButtonPanel.Height := 150;
    FAdvButtonPanel.Initialize;
  end;
  Control := FAdvButtonPanel;
  FAdvButtonPanel.OnButtonClick := OnAdvButtonClick;
end;

//------------------------------------------------------------------------------

procedure TAdvCalculatorDropdown.BeforeDropDown;
begin
  inherited;
  if Assigned(FDropDownForm) then
    FDropDownForm.CancelOnDeActivate := False;

  if Assigned(FAdvButtonPanel) then
  begin
    FAdvButtonPanel.ButtonAppearance.Assign(CalcButtonAppearance);
    FAdvButtonPanel.Color := DropDownColor;
    FAdvButtonPanel.ArrangeButtons;
  end;
  Signed := false;
  FNewval := true;
  FDecim := 0;
  FCalcClosed := False;
end;

//------------------------------------------------------------------------------

procedure TAdvCalculatorDropdown.DoHideDropDown(Canceled: Boolean);
begin
  inherited;
  Signed := true;
end;

//------------------------------------------------------------------------------

procedure TAdvCalculatorDropdown.OnHideDropDown;
begin
  inherited;

end;

//------------------------------------------------------------------------------

procedure TAdvCalculatorDropdown.OnAdvButtonClick(Sender: TObject; Index: Integer);
var
  s:string;
  f: double;
  btn: TCalcButtonItem;
begin
  if not Assigned(FAdvButtonPanel) or (Index < 0) or (Index >= FAdvButtonPanel.Buttons.Count) then
    Exit;

  CalcChange;

  btn := FAdvButtonPanel.Buttons[Index];
  if (btn.Tag in [10..14,16,17,18]) then
    FDecim := 0;

  if (btn.Tag in [0..9,14,15]) then
  begin
    if (MaxLength > 0) and (Length(Text) = MaxLength) then
      Exit;
  end;

  if (btn.Tag < 10) then
  begin
    f := FloatValue;
    if ((FloatValue = 0 ) and (FDecim = 0)) or (FNewval) then
    begin
      Text := IntToStr(btn.Tag);
      FNewval := false;
    end
   else
   begin
     if FDecim > 0 then
     begin
       f := f + btn.Tag / FDecim;
       FDecim := FDecim * 10;
     end
     else
       f := f * 10 + btn.Tag;

     FloatValue := f;
   end;
 end
 else
  begin
   case (btn.tag) of
   10:begin doplus; FPrevop:=0; end;
   11:begin domin; FPrevop:=2; end;
   12:begin domul; FPrevop:=1; end;
   13:begin dodiv; FPrevop:=3; end;
   14:if pos('-',Text)=0 then Text:='-'+Text
      else
        begin
         s := Text;
         delete(s,1,1);
         Text := s;
        end;
   15:begin
       if pos(DecimalSeparator,Text) = 0 then
       begin
         Text := Text + DecimalSeparator;
       end;
       FDecim := 10;
       if FNewval then
         Text := '0';
       FNewval := false;
     end;
   16, 30, 32:begin
        Text := '0';
        FPrevval := 0;
        FPrevop := -1;
      end;
   17:doeq;
   18:begin doperc; end;
   31:  { Back space }
     begin
       if (FPrevop < 0) then
       begin
         s := FloatToStr(FloatValue);
         s := Copy(s, 1, Length(s) - 1);
         Text := s;
       end;
     end;
   33:  { MC }
     begin
       FMVal := 0;
     end;
   34:  { MR }
     begin
       FloatValue := {FloatToStr}(FMVal);
     end;
   35:  { MS }
     begin
       FMVal := StrToFloat(Text);
     end;
   36:  { M+ }
     begin
       FMVal := FMVal + StrToFloat(Text);
     end;
   37:  { Sqrt }
     begin
       if FloatValue < 0 then
         Text := ErrText
       else
         FloatValue := Sqrt(FloatValue);
     end;
   38: { 1/x }
     begin
       if FloatValue <> 0 then
         FloatValue := 1 / FloatValue
       else
         Text := ErrText;
     end;      
   end;
  end;
  Modified := True;
end;

//------------------------------------------------------------------------------

procedure TAdvCalculatorDropdown.WMKeyDown(var Msg: TWMKeydown);
begin
  inherited;
  if (Msg.CharCode = vk_delete) then
    if (Text = '') or (Text = ErrText) then
      Text := '0';
end;

//------------------------------------------------------------------------------

procedure TAdvCalculatorDropdown.CMEnter(var Message: TCMGotFocus);
begin
  if AutoSelect and not (csLButtonDown in ControlState) then
    SelectAll;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvCalculatorDropdown.OnDropDownFormKeyPress(var Key: Char);
begin
  inherited;
  HandleKeyPress(Key);
end;

//------------------------------------------------------------------------------

procedure TAdvCalculatorDropdown.HandleKeyPress(var Key: Char);
var
  IsAlt: Boolean;
begin
  IsAlt := GetKeyState(VK_MENU) and $8000 = $8000;
  CalcChange;
  if Key = DecimalSeparator then
    if (pos(DecimalSeparator,Text)=0) then Text:=Text+DecimalSeparator;

  if IsNumChar(key) then
  begin
    if (MaxLength > 0) and (Length(Text) = MaxLength) then
      Exit;
  end;

  case key of
  '0'..'9':
    if (Text = '0') or (FNewval) then
    begin
      //SetTextDirect(key);
      FNewval := false;
    end;
    //else
      //SetTextDirect(Text + key);
  'c','C':
    begin
      Text := '0';
      FPrevval := 0;
      FNewval := true;
      FPrevop := -1;
    end;
  '+':doplus;
  '/':dodiv;
  '-':domin;
  '*':domul;
  '=':doeq;
  '%':doperc;
  #13:
    begin
      doeq;
      //postmessage((Sender as TForm).Handle,WM_CLOSE,0,0);
      DoHideDropDown(False);
    end;
  #27:
    //postmessage((Sender as TForm).Handle,WM_CLOSE,0,0);
    DoHideDropDown(True);
  Char(VK_UP): if IsAlt then DoHideDropDown(True);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCalculatorDropdown.DoCalc;
var
  e: extended;
begin
  if (Text = '') or (Text = ErrText) then
    Text := '0';

  e := strtofloat(StripThousandSep(Text));

  try
    case FPrevop of
    0: FPrevval := FPrevval + e;
    1: FPrevval := FPrevval * e;
    2: FPrevval := FPrevval - e;
    3:if (e <> 0) then
         FPrevval := FPrevval / e
       else
       begin
         FPrevval := 0;
         Text := ErrText;
         Exit;
       end;
    else
      FPrevval := strtofloat(StripThousandSep(Text));
    end;
  except
    FPrevval:=0;
  end;

  FloatValue := FPrevval;
  FNewval := true;
end;

//------------------------------------------------------------------------------

procedure TAdvCalculatorDropdown.DoDiv;
begin
 docalc;
 FPrevop := 3;
end;

//------------------------------------------------------------------------------

procedure TAdvCalculatorDropdown.DoEq;
begin
  DoCalc;
  if (Text = '') then
    Text := '0';

  FPrevop := -1;

  if (Text <> ErrText) then
    FPrevval := strtofloat(StripThousandSep(Text));
end;

//------------------------------------------------------------------------------

procedure TAdvCalculatorDropdown.DoPerc;
var
  e: extended;
begin
  if (Text = '') or (Text = ErrText) then
    Text := '0';

  e := strtofloat(StripThousandSep(Text));

  e := FPrevval * e / 100;
  Text := format('%g',[e]);
end;

//------------------------------------------------------------------------------

procedure TAdvCalculatorDropdown.DoMin;
begin
  DoCalc;
  FPrevop := 2;
end;

//------------------------------------------------------------------------------

procedure TAdvCalculatorDropdown.DoMul;
begin
  DoCalc;
  FPrevop := 1;
end;

//------------------------------------------------------------------------------

procedure TAdvCalculatorDropdown.DoPlus;
begin
  DoCalc;
  FPrevop := 0;
end;

//------------------------------------------------------------------------------

procedure TAdvCalculatorDropdown.OnDropDownFormKeyDown(var Key: Word; Shift: TShiftState);
var
  s:string;
begin
  inherited;
  if key in [vk_F4, vk_tab] then
    //Postmessage((Sender as TForm).Handle,WM_CLOSE,0,0);
    DoHideDropDown(False);

  if (key = vk_back) then
  begin
    s := Text;
    Delete(s,Length(Text),1);
    if (s = '') or (s = ErrText) then
      s := '0';
    Text := s;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCalculatorDropdown.KeyPress(var key:char);
begin
  if FCalcClosed or (Assigned(FDropDownForm) and (not FDropDownForm.Visible)) then
  case key of
  'c','C':
    begin
      Text := '0';
      FNewval := true;
      FPrevop := -1;
    end;
  '+':doplus;
  '/':dodiv;
  '-':domin;
  '*':domul;
  '=',#13:doeq;
  '%':doperc;
  end;

  if not (IsNumChar(Key) or (Key = DecimalSeparator) or (Key = #8) or (Key = '-')) then
    key := #0;

  if ((Text='0') or (FNewval)) and (IsNumChar(key)) then
  begin
    Text := Key;
    Key := #0;
    SelStart := 1 + Length(Prefix);
    SelLength := 0;
    FNewval := False;
    Exit;
  end;

  if (Length(Text) = 1) and (Key = #8) then
  begin
    Text := '0';
    Key := #0;
    SelStart := 1 + Length(Prefix);
    SelLength := 0;
    Exit;
  end;

  if (Key = ThousandSeparator) then
  begin
    Key := #0;
  end;

  if (Key = DecimalSeparator) and (Pos(Key,Text)>0) then
  begin
    Key := #0;
  end;

  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvCalculatorDropdown.CalcChange;
begin
end;

procedure TAdvCalculatorDropdown.CalcResult;
begin
  DoEq;
end;

//------------------------------------------------------------------------------

function TAdvCalculatorDropdown.GetValue: extended;
begin
  if (Text = '') then
    Text := '0';

  try
    Result := StrToFloat(StripThousandSep(Text));
  except
    Result := 0;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCalculatorDropdown.SetValue(const Value: extended);
begin
  Text := Format('%g',[value]);
end;

//------------------------------------------------------------------------------

procedure TAdvCalculatorDropdown.SetCalcButtonAppearance(
  const Value: TButtonAppearance);
begin
  FCalcButtonAppearance.Assign(Value);
end;

procedure TAdvCalculatorDropdown.SetComponentStyle(AStyle: TTMSStyle);
begin
  inherited;


  CalcButtonAppearance.BorderColorChecked := TButtonAppearance(ButtonAppearance).BorderColorChecked;
  CalcButtonAppearance.BorderColor := ButtonAppearance.BorderColor;
  CalcButtonAppearance.BorderColorHot := ButtonAppearance.BorderColorHot;
  CalcButtonAppearance.BorderColorDown := ButtonAppearance.BorderColorDown;
  CalcButtonAppearance.Color := ButtonAppearance.Color;
  CalcButtonAppearance.ColorTo := ButtonAppearance.ColorTo;
  CalcButtonAppearance.ColorDown := ButtonAppearance.ColorDown;
  CalcButtonAppearance.ColorDownTo := ButtonAppearance.ColorDownTo;
  CalcButtonAppearance.ColorChecked := TButtonAppearance(ButtonAppearance).ColorChecked;
  CalcButtonAppearance.ColorCheckedTo := TButtonAppearance(ButtonAppearance).ColorCheckedTo;
  CalcButtonAppearance.ColorDisabled := ButtonAppearance.ColorDisabled;
  CalcButtonAppearance.ColorDisabledTo := ButtonAppearance.ColorDisabledTo;
  CalcButtonAppearance.ColorHot := ButtonAppearance.ColorHot;
  CalcButtonAppearance.ColorHotTo := ButtonAppearance.ColorHotTo;

  case AStyle of
    tsOffice2010Black, tsOffice2010Silver, tsOffice2010Blue:
    begin
      CalcButtonAppearance.GlowColorDown := ButtonAppearance.ColorHot;
      CalcButtonAppearance.GlowColorHot := ButtonAppearance.ColorHot;
    end
    else
    begin
      CalcButtonAppearance.GlowColorDown := RGB(68, 194, 213);
      CalcButtonAppearance.GlowColorHot := ButtonAppearance.ColorDownTo;
    end;
  end;
end;

procedure TAdvCalculatorDropdown.SetShowThousandSeparator(const Value: boolean);
begin
  if (Value <> FShowThousandSeparator) then
  begin
    FShowThousandSeparator := Value;
    if FShowThousandSeparator then
      EditType := etMoney
    else
      EditType := etFloat;
    FloatValue := FloatValue;
  end;
end;

//------------------------------------------------------------------------------

end.
