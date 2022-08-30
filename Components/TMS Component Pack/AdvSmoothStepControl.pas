{*************************************************************************}
{ TAdvSmoothStepControl component                                         }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright ©  2015                                             }
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

unit AdvSmoothStepControl;

{$I TMSDEFS.INC}

interface

uses
  Windows, Controls, Classes, Graphics, AdvStyleIF, GDIPFill, SysUtils,
  GDIPPictureContainer, ImgList, Math, Messages, Forms, AdvGDIP, ExtCtrls, Types
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;


const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 4; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 0; // Build nr.
  s_Edit = 'Layout';

  //version history
  //v1.0.0.0 : First release
  //v1.0.5.0 : New : Animation
  //         : Improved : Keyboard handling
  //v1.0.5.1 : New : Public Processed property on TStepAction level to verify if step is processed
  //v1.0.5.2 : Fixed : Issue with list index out of bounds and animation glitch
  //v1.1.0.0 : New : Metro Style Support
  //v1.1.1.0 : New : OnStepChanged event
  //v1.1.1.1 : Fixed : Issue wtih HTML Anchor detection
  //v1.2.0.0 : New : Disabled step actions (Enabled property for each step)
  //v1.3.0.0 : New : Windows 8, Office 2013 styles added
  //v1.3.0.1 : Fixed : Issue with doubleclick
  //v1.4.0.0 : New : Windows 10, Office 2016 styles added

type
  TAdvSmoothStepControl = class;

  TStepActionAppearance = class(TPersistent)
  private
    FOwner: TAdvSmoothStepControl;
    FShapeColor: TColor;
    FCaptionColor: TColor;
    FDescriptionColor: TColor;
    FBackGroundFill: TGDIPFill;
    procedure SetCaptionColor(const Value: TColor);
    procedure SetDescriptionColor(const Value: TColor);
    procedure SetBackGroundFill(const Value: TGDIPFill);
    procedure SetShapeColor(const Value: TColor);
  protected
    procedure Changed;
    procedure FillChanged(Sender: TObject);
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(AOwner: TAdvSmoothStepControl);
    destructor Destroy; override;
  published
    property ShapeColor: TColor read FShapeColor write SetShapeColor default clGreen;
    property CaptionColor: TColor read FCaptionColor write SetCaptionColor default clBlack;
    property DescriptionColor: TColor read FDescriptionColor write SetDescriptionColor default clGray;
    property BackGroundFill: TGDIPFill read FBackGroundFill write SetBackGroundFill;
  end;

  TStepActionsLayout = (salPointer, salSquare, salProgress, salNone);

  TStepActionsShape = (sasCircle, sasSquare, sasTriangle, sasCustom, sasNone);

  TStepActionsDescriptionLayout = (dlNormal, dlBottom);

  TStepActionsAppearance = class(TPersistent)
  private
    FOwner: TAdvSmoothStepControl;
    FProcessedCaptionFont: TFont;
    FActiveCaptionFont: TFont;
    FInActiveCaptionFont: TFont;
    FProcessedAppearance: TStepActionAppearance;
    FActiveAppearance: TStepActionAppearance;
    FInActiveAppearance: TStepActionAppearance;
    FLayout: TStepActionsLayout;
    FInActiveDescriptionFont: TFont;
    FProcessedDescriptionFont: TFont;
    FActiveDescriptionFont: TFont;
    FLayoutRounding: Integer;
    FLayoutSpacingVertical: Integer;
    FLayoutSpacingHorizontal: Integer;
    FProgressSize: Integer;
    FProgressBackGround: TGDIPFill;
    FProgress: TGDIPFill;
    FSeparatorWidth: Integer;
    FSeparatorFill: TGDIPFill;
    FShapeSize: Integer;
    FShapeStyle: TStepActionsShape;
    FShapeBorderWidth: Integer;
    FDescriptionLayout: TStepActionsDescriptionLayout;
    FDescriptionSize: Integer;
    FDescriptionFill: TGDIPFill;
    FDisabledAppearance: TStepActionAppearance;
    FDisabledCaptionFont: TFont;
    FDisabledDescriptionFont: TFont;
    procedure SetActiveCaptionFont(const Value: TFont);
    procedure SetInActiveCaptionFont(const Value: TFont);
    procedure SetProcessedCaptionFont(const Value: TFont);
    procedure SetActiveAppearance(const Value: TStepActionAppearance);
    procedure SetInActiveAppearance(const Value: TStepActionAppearance);
    procedure SetProcessedAppearance(const Value: TStepActionAppearance);
    procedure SetLayout(const Value: TStepActionsLayout);
    procedure SetActiveDescriptionFont(const Value: TFont);
    procedure SetInActiveDescriptionFont(const Value: TFont);
    procedure SetProcessedDescriptionFont(const Value: TFont);
    procedure SetLayoutRounding(const Value: Integer);
    procedure SetLayoutSpacingHorizontal(const Value: Integer);
    procedure SetLayoutSpacingVertical(const Value: Integer);
    procedure SetProgressSize(const Value: Integer);
    procedure SetProgress(const Value: TGDIPFill);
    procedure SetProgressBackGround(const Value: TGDIPFill);
    procedure SetSeparatorFill(const Value: TGDIPFill);
    procedure SetSeparatorWidth(const Value: Integer);
    procedure SetShapeBorderWidth(const Value: Integer);
    procedure SetShapeSize(const Value: Integer);
    procedure SetShapeStyle(const Value: TStepActionsShape);
    procedure SetDescriptionLayout(const Value: TStepActionsDescriptionLayout);
    procedure SetDescriptionFill(const Value: TGDIPFill);
    procedure SetDescriptionSize(const Value: Integer);
    procedure SetDisabledAppearance(const Value: TStepActionAppearance);
    procedure SetDisabledCaptionFont(const Value: TFont);
    procedure SetDisabledDescriptionFont(const Value: TFont);
  protected
    procedure Changed;
    procedure FontChanged(Sender: TObject);
    procedure FillChanged(Sender: TObject);
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(AOwner: TAdvSmoothStepControl);
    destructor Destroy; override;
  published
    property InActiveCaptionFont: TFont read FInActiveCaptionFont write SetInActiveCaptionFont;
    property DisabledCaptionFont: TFont read FDisabledCaptionFont write SetDisabledCaptionFont;
    property ActiveCaptionFont: TFont read FActiveCaptionFont write SetActiveCaptionFont;
    property ProcessedCaptionFont: TFont read FProcessedCaptionFont write SetProcessedCaptionFont;
    property InActiveDescriptionFont: TFont read FInActiveDescriptionFont write SetInActiveDescriptionFont;
    property DisabledDescriptionFont: TFont read FDisabledDescriptionFont write SetDisabledDescriptionFont;
    property ActiveDescriptionFont: TFont read FActiveDescriptionFont write SetActiveDescriptionFont;
    property ProcessedDescriptionFont: TFont read FProcessedDescriptionFont write SetProcessedDescriptionFont;
    property ActiveAppearance: TStepActionAppearance read FActiveAppearance write SetActiveAppearance;
    property InActiveAppearance: TStepActionAppearance read FInActiveAppearance write SetInActiveAppearance;
    property DisabledAppearance: TStepActionAppearance read FDisabledAppearance write SetDisabledAppearance;
    property ProcessedAppearance: TStepActionAppearance read FProcessedAppearance write SetProcessedAppearance;
    property Layout: TStepActionsLayout read FLayout write SetLayout default salPointer;
    property ShapeSize: Integer read FShapeSize write SetShapeSize default 40;
    property ShapeStyle: TStepActionsShape read FShapeStyle write SetShapeStyle default sasCircle;
    property ShapeBorderWidth: Integer read FShapeBorderWidth write SetShapeBorderWidth default 5;
    property LayoutSpacingVertical: Integer read FLayoutSpacingVertical write SetLayoutSpacingVertical default 0;
    property LayoutSpacingHorizontal: Integer read FLayoutSpacingHorizontal write SetLayoutSpacingHorizontal default 0;
    property LayoutRounding: Integer read FLayoutRounding write SetLayoutRounding default 0;
    property ProgressSize: Integer read FProgressSize write SetProgressSize default 25;
    property ProgressBackGround: TGDIPFill read FProgressBackGround write SetProgressBackGround;
    property Progress: TGDIPFill read FProgress write SetProgress;
    property SeparatorFill: TGDIPFill read FSeparatorFill write SetSeparatorFill;
    property SeparatorWidth: Integer read FSeparatorWidth write SetSeparatorWidth default 5;
    property DescriptionLayout: TStepActionsDescriptionLayout read FDescriptionLayout write SetDescriptionLayout default dlNormal;
    property DescriptionFill: TGDIPFill read FDescriptionFill write SetDescriptionFill;
    property DescriptionSize: Integer read FDescriptionSize write SetDescriptionSize default 50;
  end;

  TStepAction = class;

  TStepActionContent = class;

  TStepActionTextLocation = (tlTopLeft, tlTopCenter, tlTopRight, tlCenterLeft, tlCenterCenter, tlCenterRight, tlBottomLeft, tlBottomCenter, tlBottomRight, tlCustom);

  TStepActionContentHTMLText = class(TPersistent)
  private
    FOwner: TStepActionContent;
    FURLColor: TColor;
    FShadowOffset: integer;
    FFont: TFont;
    FText: String;
    FShadowColor: TColor;
    FOnChange: TNotifyEvent;
    FLocation: TStepActionTextLocation;
    FTop: integer;
    FLeft: integer;
    procedure SetFont(const Value: TFont);
    procedure SetLeft(const Value: integer);
    procedure SetLocation(const Value: TStepActionTextLocation);
    procedure SetShadowColor(const Value: TColor);
    procedure SetShadowOffset(const Value: integer);
    procedure SetText(const Value: string);
    procedure SetTop(const Value: integer);
    procedure SetURLColor(const Value: TColor);
  protected
    procedure Changed;
  public
    constructor Create(AOwner: TStepActionContent);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Text: string read FText write SetText;
    property Location: TStepActionTextLocation read FLocation write SetLocation default tlTopLeft;
    property Top: integer read FTop write SetTop default 0;
    property Left: integer read FLeft write SetLeft default 0;
    property URLColor: TColor read FURLColor write SetURLColor default clBlue;
    property ShadowColor: TColor read FShadowColor write SetShadowColor default clGray;
    property ShadowOffset: integer read FShadowOffset write SetShadowOffset default 5;
    property Font: TFont read FFont write SetFont;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;


  TStepActionContent = class(TPersistent)
  private
    FOwner: TStepAction;
    FImageName: string;
    FCaption: String;
    FDescription: TStepActionContentHTMLText;
    FImageIndex: Integer;
    FHint: String;
    FCaptionLocation: TStepActionTextLocation;
    FCaptionTop: Integer;
    FCaptionLeft: Integer;
    procedure SetCaption(const Value: String);
    procedure SetDescription(const Value: TStepActionContentHTMLText);
    procedure SetImageIndex(const Value: Integer);
    procedure SetImageName(const Value: string);
    procedure SetHint(const Value: String);
    procedure SetCaptionLocation(const Value: TStepActionTextLocation);
    procedure SetCaptionLeft(const Value: Integer);
    procedure SetCaptionTop(const Value: Integer);
  protected
    procedure Changed;
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(AOwner: TStepAction);
    destructor Destroy; override;
    function GetAnchorAt(g: TGPGraphics; X, Y: integer; R: TGPRectF; ft: TFont): String;
    function DrawHTMLText(g: TGPGraphics; HTML: TStepActionContentHTMLText; r: TGPRectF; ft: TFont;  str: String;
      DoAnchor: Boolean = false; fX: integer = -1; fY: integer = -1): String;
  published
    property Caption: String read FCaption write SetCaption;
    property CaptionLeft: Integer read FCaptionLeft write SetCaptionLeft default 0;
    property CaptionTop: Integer read FCaptionTop write SetCaptionTop default 0;
    property CaptionLocation: TStepActionTextLocation read FCaptionLocation write SetCaptionLocation default tlBottomLeft;
    property Description: TStepActionContentHTMLText read FDescription write SetDescription;
    property ImageIndex: Integer read FImageIndex write SetImageIndex default -1;
    property ImageName: string read FImageName write SetImageName;
    property Hint: String read FHint write SetHint;
  end;

  TStepAction = class(TCollectionItem)
  private
    FItemRect, FItemRectOrig, FDescRect: TGPRectF;
    FOwner: TAdvSmoothStepControl;
    FItemObject: TObject;
    FTag: Integer;
    FProcessedContent: TStepActionContent;
    FActiveContent: TStepActionContent;
    FInActiveContent: TStepActionContent;
    FEnabled: Boolean;
    FDisabledContent: TStepActionContent;
    procedure SetActiveContent(const Value: TStepActionContent);
    procedure SetInActiveContent(const Value: TStepActionContent);
    procedure SetProcessedContent(const Value: TStepActionContent);
    procedure SetEnabled(const Value: Boolean);
    procedure SetDisabledContent(const Value: TStepActionContent);
  protected
    procedure Changed;
    procedure Draw(g: TGPGraphics; r: TGPRectF);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function GetHTMLRect(R: TGPRectF): TGPRectF;
    function GetAnchorAt(X, Y: Integer): String;
    function Processed: Boolean;
  published
    property ActiveContent: TStepActionContent read FActiveContent write SetActiveContent;
    property InActiveContent: TStepActionContent read FInActiveContent write SetInActiveContent;
    property DisabledContent: TStepActionContent read FDisabledContent write SetDisabledContent;
    property ProcessedContent: TStepActionContent read FProcessedContent write SetProcessedContent;
    property ItemObject: TObject read FItemObject write FItemObject;
    property Tag: Integer read FTag write FTag;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
  end;

  TStepActions = class(TCollection)
  private
    FOwner: TAdvSmoothStepControl;
    FOnChange: TNotifyEvent;
    function GetItem(Index: Integer): TStepAction;
    procedure SetItem(Index: Integer;
      const Value: TStepAction);
  protected
    function GetOwner: TPersistent; override;
  public
    procedure Clear;
    constructor Create(AOwner: TAdvSmoothStepControl);
    function Add: TStepAction;
    function Insert(Index: Integer): TStepAction;
    property Items[Index: Integer]: TStepAction read GetItem write SetItem; default;
    procedure Delete(Index: Integer);
  published
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TStepMode = (mActive, mProcessed, mInactive, mDisabled);

  TStepClickEvent = procedure(Sender: TObject; StepIndex: Integer; StepMode: TStepMode) of object;

  TStepMouseDownEvent = procedure(Sender:TObject;StepIndex: Integer ; StepMode: TStepMode; Button: TMouseButton; Shift: TShiftState; X, Y: Integer) of object;

  TStepMouseMoveEvent = procedure(Sender:TObject;StepIndex: Integer ; StepMode: TStepMode; Shift: TShiftState; X, Y: Integer) of object;

  TStepMouseUpEvent = procedure(Sender:TObject; StepIndex: Integer; StepMode: TStepMode; Button: TMouseButton; Shift: TShiftState; X, Y: Integer) of object;

  TStepHintEvent = procedure(Sender: TObject; StepIndex: Integer; StepMode: TStepMode; var Hint: String) of object;

  TStepShapeDrawEvent = procedure(Sender: TObject; StepIndex: Integer; g: TGPGraphics; InnerRect, Rect: TGPRectF) of object;

  TStepDrawEvent = procedure(Sender: TObject; StepIndex: Integer; g: TGPGraphics; Rect: TGPRectF) of object;

  TStepAnchorEvent = procedure(Sender: TObject; StepIndex: Integer; Anchor: String) of object;

  TStepChangedEvent = procedure(Sender: TObject; StepIndex: Integer) of object;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvSmoothStepControl = class(TCustomControl, ITMSStyle, ITMSTones)
  private
    FTMSStyle: TTMSStyle;
    FMouseDown: Boolean;
    FMetroStyle: Boolean;
    FUpdateCount: Integer;
    FCurPos: Single;
    FAnimate: TTimer;
    FConstructed: Boolean;
    FDesignTime: Boolean;
{$IFDEF DELPHI2006_LVL}
    FLastDesignChoice: Boolean;
{$ENDIF}    
    FMouseMoveIdx: Integer;
    FFocused: Boolean;
    FAntiAlias: TAntiAlias;
    FTransparent: Boolean;
    FFill: TGDIPFill;
    FStepActions: TStepActions;
    FStepActionsAppearance: TStepActionsAppearance;
    FPictureContainer: TGDIPPictureContainer;
    FImageList: TCustomImageList;
    FActiveStep: Integer;
    FReadOnly: Boolean;
    FVisibleSteps: Integer;
    FShowFocus: Boolean;
    FOnStepMouseMove: TStepMouseMoveEvent;
    FOnStepHint: TStepHintEvent;
    FOnStepMouseDown: TStepMouseDownEvent;
    FOnStepMouseUp: TStepMouseUpEvent;
    FOnStepClick: TStepClickEvent;
    FOnStepShapeDraw: TStepShapeDrawEvent;
    FOnStepDraw: TStepDrawEvent;
    FOnStepAnchor: TStepAnchorEvent;
    FAnimation: Boolean;
    FOnStepChanged: TStepChangedEvent;
    procedure SetAntiAlias(const Value: TAntiAlias);
    procedure SetFill(const Value: TGDIPFill);
    procedure SetTransparent(const Value: Boolean);
    procedure SetStepActions(const Value: TStepActions);
    procedure SetStepActionsAppearance(const Value: TStepActionsAppearance);
    procedure SetImageList(const Value: TCustomImageList);
    procedure SetPictureContainer(const Value: TGDIPPictureContainer);
    procedure SetActiveStep(const Value: Integer);
    procedure SetReadOnly(const Value: Boolean);
    procedure SetVisibleSteps(const Value: Integer);
    function GetVersion: String;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure CMHintShow(var Message: TMessage); message CM_HINTSHOW;
    procedure SetShowFocus(const Value: Boolean);
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure SetAnimation(const Value: Boolean);
    {$IFDEF DELPHI2006_LVL}
    procedure CMDesignHitTest(var Msg: TCMDesignHitTest); message CM_DESIGNHITTEST;
    {$ENDIF}
  protected
    procedure ReadTMSStyle(Reader: TReader);
    procedure WriteTMSStyle(Writer: TWriter);
    procedure DefineProperties(Filer: TFiler); override;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure Draw(g: TGPGraphics; r: TGPRectF);
    procedure DrawBackGround(g: TGPGraphics; r: TGPRectF);
    procedure DrawSteps(g: TGPGraphics; r: TGPRectF);
    procedure CalculateSteps(r: TGPRectF);
    procedure DrawProgress(g: TGPGraphics; r: TGPRectF);
    procedure Changed;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    function GetStepCount: Integer;
    function GetStartStep: Integer;
    function GetStopStep: Integer;
    function GetVersionNr: Integer; virtual;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    {$IFDEF DELPHI2006_LVL}
    function MouseOverDesignChoice(X, Y: Integer): Boolean;
    procedure HandleDesignChoice(X,Y: integer);
    procedure PaintDesigner;
    {$ENDIF}
  public
    procedure BeginUpdate;
    procedure EndUpdate;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CreateWnd; override;

    procedure DoEnter; override;
    procedure DoExit; override;

    procedure Assign(Source: TPersistent); override;
    procedure FillChanged(Sender: TObject);
    procedure StepActionsChanged(Sender: TObject);
    procedure Animate(Sender: TObject);

    procedure Paint; override;
    procedure SetComponentStyle(AStyle: TTMSStyle);
    function GetComponentStyle: TTMSStyle;
    procedure SetColorTones(ATones: TColorTones);

    procedure NextStep;
    procedure PreviousStep;
    procedure GoToStep(Index: Integer);
    procedure Finish;
    function XYToStep(X, Y: Integer): Integer;
    function XYToAnchorStep(X, Y: Integer): Integer;
    procedure UseDefaultStyle;
    procedure InitializeSteps;
    function GetStepMode(Item: Integer): TStepMode;
    procedure SaveToImage(Filename: String; ImageType: TImageType = itBMP; ImageQualityPercentage: integer = 100);
    procedure Resize; override;
  published
    property Animation: Boolean read FAnimation write SetAnimation default true;
    property AntiAlias: TAntiAlias read FAntiAlias write SetAntiAlias default aaClearType;
    property Fill: TGDIPFill read FFill write SetFill;
    property Transparent: Boolean read FTransparent write SetTransparent default True;
    property StepActions: TStepActions read FStepActions write SetStepActions;
    property Appearance: TStepActionsAppearance read FStepActionsAppearance write SetStepActionsAppearance;
    property ImageList: TCustomImageList read FImageList write SetImageList;
    property PictureContainer: TGDIPPictureContainer read FPictureContainer write SetPictureContainer;
    property ActiveStep: Integer read FActiveStep write SetActiveStep default 0;
    property VisibleSteps: Integer read FVisibleSteps write SetVisibleSteps default 0;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly default False;
    property Version: String read GetVersion;
    property ShowFocus: Boolean read FShowFocus write SetShowFocus default true;

    property OnStepClick: TStepClickEvent read FOnStepClick write FOnStepClick;
    property OnStepMouseMove: TStepMouseMoveEvent read FOnStepMouseMove write FOnStepMouseMove;
    property OnStepMouseUp: TStepMouseUpEvent read FOnStepMouseUp write FOnStepMouseUp;
    property OnStepMouseDown: TStepMouseDownEvent read FOnStepMouseDown write FOnStepMouseDown;
    property OnStepHint: TStepHintEvent read FOnStepHint write FOnStepHint;
    property OnStepShapeDraw: TStepShapeDrawEvent read FOnStepShapeDraw write FOnStepShapeDraw;
    property OnStepDraw: TStepDrawEvent read FOnStepDraw write FOnStepDraw;
    property OnStepAnchor: TStepAnchorEvent read FOnStepAnchor write FOnStepAnchor;
    property OnStepChanged: TStepChangedEvent read FOnStepChanged write FOnStepChanged;

    property Align;
    property Anchors;
    property Constraints;
    property PopupMenu;
    property TabOrder;
    property ParentShowHint;
    property ShowHint;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseUp;
    property OnMouseMove;
    property OnMouseDown;
    {$IFDEF DELPHI2006_LVL}
    property OnMouseActivate;
    property OnMouseEnter;
    property OnMouseLeave;
    {$ENDIF}
    property DragKind;
    property DragMode;
    property OnResize;
    property OnDblClick;
    property OnClick;
    property OnEnter;
    property OnExit;
    property OnStartDrag;
    property OnEndDrag;
    property Visible;
    property TabStop default true;
    {$IFDEF DELPHI_TOUCH}
    property OnGesture;
    property Touch;
    {$ENDIF}
   end;

implementation

uses
  CommCtrl, ShellApi, Consts;

{$I GDIPHTMLEngine.pas}

function RectanglesInterSect(r1, r2: TGPRectF): Boolean;
var
  X, Y, w, h: Double;
begin
  X := max(r1.X, r2.X);
  Y := max(r1.Y, r2.Y);
  w := min(r1.X + r1.Width, r2.X + r2.Width);
  h := min(r1.Y + r1.Height, r2.Y + r2.Height);

  result := ((w > X) and (h > Y));
end;

function SaveRound(Val: Double): Integer;
begin
  if Val < 0 then
    Result := Round(Max(Val, -MaxInt))
  else
    Result := Round(Min(Val, MaxInt))
end;

function AnimateDouble(var Start: Single; Stop, Delta, Margin: Single): Boolean;
begin
  Result := true;
  if (Start > Stop - Margin) and (Start < Stop + Margin) then
  begin
    Start := Stop;
    Result := false;
  end
  else
  begin
    Delta := Max(1, Delta);
    if Start < Stop then
      Start := SaveRound(Start + Delta)
    else
      Start := SaveRound(Start - Delta);
  end;
end;

procedure GetObjectLocation(var x, y: Double; rectangle: TGPRectF; objectwidth, objectheight: Double; location: TStepActionTextLocation);
var
  w, h, tw, th: Double;
begin
  tw := objectwidth;
  th := objectheight;
  w := rectangle.Width;
  h := rectangle.Height;
  case location of
    tlTopLeft:
    begin
      x := 0;
      y := 0;
    end;
    tlTopRight:
    begin
      x := w - tw;
      y := 0;
    end;
    tlBottomLeft:
    begin
      x := 0;
      y := h - th;
    end;
    tlBottomRight:
    begin
      x := w - tw;
      y := h - th;
    end;
    tlTopCenter:
    begin
      x := (w - tw) / 2;
      y := 0;
    end;
    tlBottomCenter:
    begin
      x := (w - tw) / 2;
      y := h - th;
    end;
    tlCenterCenter:
    begin
      x := (w - tw) / 2;
      y := (h - th) / 2;
    end;
    tlCenterLeft:
    begin
      x := 0;
      y := (h - th) / 2;
    end;
    tlCenterRight:
    begin
      x := w - tw;
      y := (h - th) / 2;
    end;
  end;

  x := x + rectangle.X;
  y := y + rectangle.Y;
end;

function PtInGPRect(R: TGPRectF; pt: TPoint): Boolean;
begin
  result := ((pt.X >= R.X) and (pt.X <= R.X + R.Width)) and
    ((pt.Y >= R.Y) and (pt.Y <= R.Y + R.Height));
end;

function Lighter(Color:TColor; Percent:Byte):TColor;
var
  r, g, b:Byte;
begin
  Color := ColorToRGB(Color);
  r := GetRValue(Color);
  g := GetGValue(Color);
  b := GetBValue(Color);
  r := r + muldiv(255 - r, Percent, 100); //Percent% closer to white
  g := g + muldiv(255 - g, Percent, 100);
  b := b + muldiv(255 - b, Percent, 100);
  result := RGB(r, g, b);
end;

function Darker(Color:TColor; Percent:Byte):TColor;
var
  r, g, b:Byte;
begin
  Color := ColorToRGB(Color);
  r := GetRValue(Color);
  g := GetGValue(Color);
  b := GetBValue(Color);
  r := r - muldiv(r, Percent, 100);  //Percent% closer to black
  g := g - muldiv(g, Percent, 100);
  b := b - muldiv(b, Percent, 100);
  result := RGB(r, g, b);
end;

{ TAdvSmoothStepControl }

procedure TAdvSmoothStepControl.Animate(Sender: TObject);
var
  d, w: Double;
begin
  if Animation and (StepActions.Count > 0) then
  begin
    if (FActiveStep >= 0) and (FActiveStep <= StepActions.Count - 1) then
      w := Max(0, Min(StepActions[StepActions.Count - 1].FItemRectOrig.X + StepActions[StepActions.Count - 1].FItemRectOrig.Width - Width + 1, StepActions[FActiveStep].FItemRectOrig.X - (Width / 2) + (StepActions[FActiveStep].FItemRectOrig.Width / 2)))
    else
    begin
      if FActiveStep < 0 then
        w := 0
      else
        w := StepActions[StepActions.Count - 1].FItemRectOrig.X + StepActions[StepActions.Count - 1].FItemRectOrig.Width - Width + 1;
    end;

    d := Abs(FCurPos - w) / 10;

    if Animatedouble(FCurPos, w, d, 1) then
      Changed
    else
    begin
      FAnimate.Enabled := False;
      Changed;
    end;
  end
  else
    FAnimate.Enabled := False;
end;

procedure TAdvSmoothStepControl.Assign(Source: TPersistent);
begin
  if (Source is TAdvSmoothStepControl) then
  begin
    FFill.Assign((Source as TAdvSmoothStepControl).Fill);
    FTransparent := (Source as TAdvSmoothStepControl).Transparent;
    FActiveStep := (Source as TAdvSmoothStepControl).ActiveStep;
    FStepActions.Assign((Source as TAdvSmoothStepControl).StepActions);
    FReadOnly := (Source as TAdvSmoothStepControl).ReadOnly;
    FAntiAlias := (Source as TAdvSmoothStepControl).AntiAlias;
    FVisibleSteps := (Source as TAdvSmoothStepControl).VisibleSteps;
    FShowFocus := (Source as TAdvSmoothStepControl).ShowFocus;
    FAnimation := (Source as TAdvSmoothStepControl).Animation;
  end;
end;

procedure TAdvSmoothStepControl.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TAdvSmoothStepControl.CalculateSteps(r: TGPRectF);
var
  I, K: Integer;
  ir: TGPRectF;
  iw: Double;
begin
  K := GetStopStep - GetStartStep;
  for I := GetStopStep downto GetStartStep do
  begin
    iw := r.Width / GetStepCount;
    case Appearance.Layout of
      salProgress:
      begin
        if Appearance.DescriptionLayout <> dlBottom then
          ir := MakeRect(K * iw, r.Y, iw, r.Height - Appearance.ProgressSize)
        else
          ir := MakeRect(K * iw, r.Y, iw, r.Height - Appearance.ProgressSize - Appearance.DescriptionSize);
      end
    else
      if Appearance.DescriptionLayout <> dlBottom then
        ir := MakeRect(K * iw, r.Y, iw, r.Height)
      else
        ir := MakeRect(K * iw, r.Y, iw, r.Height - Appearance.DescriptionSize);
    end;

    StepActions[I].FItemRect := ir;
    StepActions[I].FDescRect := MakeRect(0, ir.Y + ir.Height, Width, Height - ir.Y - ir.Height);
    StepActions[I].FItemRectOrig := ir;
    StepActions[i].FItemRect.X := StepActions[i].FItemRect.X - FCurPos;
    Dec(K);
  end;
end;

procedure TAdvSmoothStepControl.Changed;
var
  r: TGPRectF;
begin
  if (FUpdateCount > 0) or (csDestroying in ComponentState) then
    Exit;

  r := MakeRect(0, 0, Width - 1, Height - 1);
  CalculateSteps(r);
  invalidate;
end;

{$IFDEF DELPHI2006_LVL}
procedure TAdvSmoothStepControl.CMDesignHitTest(var Msg: TCMDesignHitTest);
var
  p: TPoint;
  nc: Boolean;
  r: TRect;
begin
  inherited;
  if (csDesigning in ComponentState) then
  begin
    GetCursorPos(P);
    P := ScreenToClient(P);
    nc := MouseOverDesignChoice(P.X, P.Y);
    if nc <> FLastDesignChoice then
    begin
      r := ClientRect;
      r := Rect(r.Right - 60, r.Bottom - 70, r.Right, r.Bottom);
      InvalidateRect(Handle, @r, true);
    end;
    FLastDesignChoice := nc;
    if nc then
      Msg.Result := 1;
  end;
end;
{$ENDIF}

procedure TAdvSmoothStepControl.CMHintShow(var Message: TMessage);
var
  pt: TPoint;
  item: integer;
  mode: TStepMode;
begin
  with TCMHintShow(Message).HintInfo^ do
  begin
    HintStr := self.Hint;
    pt := CursorPos;
    item := XYToStep(pt.X, pt.Y);
    if (item >= 0) and (item <= StepActions.Count - 1) then
    begin
      mode := GetStepMode(Item);
      case mode of
        mActive: HintStr := StepActions[item].ActiveContent.Hint;
        mInActive: HintStr := StepActions[item].InActiveContent.Hint;
        mProcessed: HintStr := StepActions[item].ProcessedContent.Hint;
        mDisabled: HintStr := StepActions[item].DisabledContent.Hint;
      end;

      if Assigned(OnStepHint) then
        OnStepHint(Self, Item, mode, HintStr);
    end;

    ReshowTimeout := 0;
  end;
end;

procedure TAdvSmoothStepControl.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  Screen.Cursor := crDefault;
  FMouseMoveIdx := -1;
  invalidate;
end;

constructor TAdvSmoothStepControl.Create(AOwner: TComponent);
begin
  FConstructed := false;
  inherited;
  Width := 580;
  Height := 65;

  FAnimation := True;

  TabStop := True;
  DoubleBuffered := True;
  FAntiAlias := aaClearType;
  FFill := TGDIPFill.Create;
  FFill.OnChange := FillChanged;
  FTransparent := True;
  FStepActions := TStepActions.Create(Self);
  FStepActions.OnChange := StepActionsChanged;
  FStepActionsAppearance := TStepActionsAppearance.Create(Self);

  FActiveStep := 0;
  FReadOnly := False;

  FVisibleSteps := 0;
  FShowFocus := True;

  FMouseMoveIdx := -1;

  FAnimate := TTimer.Create(Self);
  FAnimate.OnTimer := Animate;
  FAnimate.Interval := 10;
  FAnimate.Enabled := False;

  FDesignTime := (csDesigning in ComponentState) and not
    ((csReading in Owner.ComponentState) or (csLoading in Owner.ComponentState));

  if FDesignTime then
    UseDefaultStyle;
end;

procedure TAdvSmoothStepControl.CreateWnd;
begin
  inherited;
  if FDesignTime and not FConstructed then
    InitializeSteps;

  FConstructed := True;
end;

destructor TAdvSmoothStepControl.Destroy;
begin
  FAnimate.Free;
  FStepActionsAppearance.Free;
  FFill.Free;
  FStepActions.Free;
  inherited;
end;

procedure TAdvSmoothStepControl.DoEnter;
begin
  inherited;
  FFocused := true;
  Changed;
end;

procedure TAdvSmoothStepControl.DoExit;
begin
  inherited;
  Application.CancelHint;
  FFocused := false;
  Changed;
end;

procedure TAdvSmoothStepControl.Draw(g: TGPGraphics; r: TGPRectF);
begin
  if not Transparent then
    DrawBackGround(g, r);

  DrawSteps(g, r);

  DrawProgress(g, r);
end;

procedure TAdvSmoothStepControl.DrawBackGround(g: TGPGraphics; r: TGPRectF);
begin
  Fill.Fill(g, R);
end;

procedure TAdvSmoothStepControl.DrawProgress(g: TGPGraphics; r: TGPRectF);
var
  rpr: TGPRectF;
  pth: TGPGraphicsPath;
  p: TGPPen;
  pr, rwidth: Double;
begin
  if (StepActions.Count > 0) and (Appearance.Layout = salProgress) then
  begin
    rpr := MakeRect(r.X, r.Y + r.Height - Appearance.ProgressSize, r.Width, Appearance.ProgressSize);
    rpr := MakeRect(rpr.X + 3, rpr.Y + 8, rpr.Width - 6, rpr.Height - 11);

    pth := GDIPFill.CreateRoundRectangle(rpr, 4, rtBoth, False);
    g.SetClip(pth);
    Appearance.ProgressBackGround.Fill(g, rpr);
    g.ResetClip;

    p := TGPPen.Create(MakeColor(Appearance.ProgressBackGround.BorderOpacity, Appearance.ProgressBackGround.BorderColor), Appearance.ProgressBackGround.BorderWidth);
    g.DrawPath(p, pth);
    p.Free;

    pth.Free;

    rwidth := (rpr.Width - 8) / StepActions.Count;
    if ActiveStep = StepActions.Count then
      pr := rpr.Width - 8
    else
    begin
      pr := ((rpr.Width - 8) / StepActions.Count) * ActiveStep;
      pr := pr + rwidth / 2;
    end;

    if pr > 0 then
    begin
      rpr := MakeRect(rpr.X + 4, rpr.Y + 4, pr, rpr.Height - 8);
      pth := GDIPFill.CreateRoundRectangle(rpr, 4, rtBoth, False);
      g.SetClip(pth);
      Appearance.Progress.Fill(g, rpr);
      g.ResetClip;

      p := TGPPen.Create(MakeColor(Appearance.Progress.BorderOpacity, Appearance.Progress.BorderColor), Appearance.Progress.BorderWidth);
      g.DrawPath(p, pth);
      p.Free;
      pth.Free;
    end;
  end;
end;

procedure TAdvSmoothStepControl.DrawSteps(g: TGPGraphics; r: TGPRectF);
var
  I: Integer;
begin
  for I := GetStopStep downto GetStartStep do
    if RectanglesInterSect(MakeRect(r.X - 20, r.Y, r.Width + 40, r.Height), StepActions[i].fitemrect) then
      StepActions[I].Draw(g, StepActions[I].FItemRect);
end;

procedure TAdvSmoothStepControl.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then
    Changed;
end;

procedure TAdvSmoothStepControl.FillChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothStepControl.Finish;
begin
  ActiveStep := StepActions.Count;
end;

function TAdvSmoothStepControl.GetComponentStyle: TTMSStyle;
begin
  Result := FTMSStyle;
end;

function TAdvSmoothStepControl.GetStartStep: Integer;
begin
  if not Animation and (VisibleSteps > 0) and (VisibleSteps <= StepActions.Count) then
  begin
    if GetStopStep = StepActions.Count - 1 then
      Result := StepActions.Count - VisibleSteps
    else
      Result := Max(0, ActiveStep - 1);
  end
  else
  begin
    Result := 0;
  end;
end;

function TAdvSmoothStepControl.GetStepCount: Integer;
begin
  if (VisibleSteps > 0) and (VisibleSteps <= StepActions.Count) then
  begin
    Result := VisibleSteps;
  end
  else
  begin
    Result := StepActions.Count;
  end;
end;

function TAdvSmoothStepControl.GetStepMode(Item: Integer): TStepMode;
var
  st: TStepAction;
begin
  if (item >= 0) and (item <= StepActions.Count - 1) then
  begin
    st := StepActions[item];
    if Assigned(st) then
    begin
      if not st.Enabled then
      begin
        Result := mDisabled;
        Exit;
      end;
    end;
  end;

  if item < ActiveStep then
    Result := mProcessed
  else if item = ActiveStep then
    Result := mActive
  else
    Result := mInactive;
end;

function TAdvSmoothStepControl.GetStopStep: Integer;
begin
  if not Animation and (VisibleSteps > 0) and (VisibleSteps <= StepActions.Count) then
  begin
    if ActiveStep > -1 then
    begin
      if ActiveStep + VisibleSteps - 1 > StepActions.Count - 1 then
      begin
        Result := StepActions.Count - 1;
      end
      else
      begin
        if ActiveStep > 0 then
          Result := ActiveStep + VisibleSteps - 2
        else
          Result := ActiveStep + VisibleSteps - 1;
      end;
    end
    else
      Result := VisibleSteps - 1;

    Result := Min(StepActions.Count - 1, Result);
  end
  else
  begin
    Result := StepActions.Count - 1;
  end;
end;

function TAdvSmoothStepControl.GetVersion: String;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function TAdvSmoothStepControl.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TAdvSmoothStepControl.GoToStep(Index: Integer);
begin
  ActiveStep := Index;
end;

{$IFDEF DELPHI2006_LVL}
procedure TAdvSmoothStepControl.HandleDesignChoice(X,Y: integer);
var
  popmenu: THandle;
  pt: TPoint;
  j: integer;
  flg: longint;
begin
  if MouseOverDesignChoice(X, Y) then
  begin
    popmenu := CreatePopupMenu;

    //salPointer, salSquare, salProgress, salNone

    if Appearance.Layout = salPointer then flg := MF_CHECKED else flg := 0;

    InsertMenu(popmenu, $FFFFFFFF, MF_BYPOSITION or flg, 1 , pchar('Pointer'));

    if Appearance.Layout = salSquare then flg := MF_CHECKED else flg := 0;

    InsertMenu(popmenu, $FFFFFFFF, MF_BYPOSITION or flg, 2 , pchar('Square'));

    if Appearance.Layout = salProgress then flg := MF_CHECKED else flg := 0;

    InsertMenu(popmenu, $FFFFFFFF, MF_BYPOSITION or flg, 3 , pchar('Progress'));

    if Appearance.Layout = salNone then flg := MF_CHECKED else flg := 0;

    InsertMenu(popmenu, $FFFFFFFF, MF_BYPOSITION or flg, 4 , pchar('None'));

    pt := ClientToScreen(Point(X,Y));
    j := integer(TrackPopupMenu(popmenu, TPM_LEFTALIGN or TPM_LEFTBUTTON or TPM_RETURNCMD, pt.X, pt.Y, 0, Handle, nil));
    DestroyMenu(popmenu);

    if j > 0 then
    begin
      Appearance.Layout := TStepActionsLayout(j - 1);
      //todo : modified
    end;
  end;
  Changed;
end;
{$ENDIF}

procedure TAdvSmoothStepControl.InitializeSteps;
begin
  StepActions.Add;
  StepActions.Add;
  StepActions.Add;
end;

procedure TAdvSmoothStepControl.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if ReadOnly then
    Exit;

  case Key of
    VK_UP, VK_LEFT: PreviousStep;
    VK_DOWN, VK_RIGHT: NextStep;
    VK_PRIOR: ActiveStep := ActiveStep - 5;
    VK_NEXT: ActiveStep := ActiveStep + 5;
    VK_HOME:
    begin
      if ActiveStep > 0 then
        ActiveStep := 0
      else
        ActiveStep := -1;
    end;
    VK_END:
    begin
      if ActiveStep < StepActions.Count - 1 then
        ActiveStep := StepActions.Count - 1
      else
        ActiveStep := StepActions.Count;
    end;
  end;
end;


procedure TAdvSmoothStepControl.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if ReadOnly then
    Exit;
end;

procedure TAdvSmoothStepControl.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  idx: Integer;
begin
  inherited;
  if ReadOnly then
    Exit;

  SetFocus;
  FMouseDown := True;

  idx := XYToStep(X, Y);
  if (idx >= 0) and (idx <= StepActions.Count - 1) then
  begin
    if Assigned(OnStepMouseDown) then
      OnStepMouseDown(Self, idx, GetStepMode(idx),Button, Shift, X, Y);
  end;
end;

procedure TAdvSmoothStepControl.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  idx, idxa: Integer;
begin
  inherited;
  if ReadOnly then
    Exit;

  idx := XYToStep(X, Y);
  if (idx >= 0) and (idx <= StepActions.Count - 1) then
  begin
    if Assigned(OnStepMouseMove) then
      OnStepMouseMove(Self, idx, GetStepMode(idx), Shift, X, Y);
  end;

  idxa := XYToAnchorStep(X, Y);
  if (idxa >= 0) and (idxa <= StepActions.Count - 1) then
  begin
    if StepActions[idxa].GetAnchorAt(X, Y) <> '' then
      Screen.Cursor := crHandPoint
    else
      Screen.Cursor := crDefault;
  end;


  if FMouseMoveIdx <> idx then
  begin
    FMouseMoveIdx := idx;
    Invalidate;
    Application.CancelHint;
  end;
end;

{$IFDEF DELPHI2006_LVL}
function TAdvSmoothStepControl.MouseOverDesignChoice(X, Y: Integer): Boolean;
var
  r: TRect;
  fh: Integer;
  h: Integer;
begin
  Result := False;

  r := ClientRect;
  Canvas.Font.Name := 'Tahoma';
  Canvas.Font.Size := 8;

  fh := Canvas.TextHeight('gh') + 2;

  h := 2 * fh + 5;

  if (x > r.Right - 60) and (x < r.Right - 60 + Canvas.TextWidth(s_Edit) + 18) and
    (y > r.Bottom - h + fh + 4) and
    (y < r.Bottom - h + 2 * fh + 4) then
    Result := True;
end;
{$ENDIF}

procedure TAdvSmoothStepControl.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  idx, idxa: Integer;
  str: String;
begin
  inherited;

  if not FMouseDown then
    Exit;

  FMouseDown := False;

  {$IFDEF DELPHI2006_LVL}
  if (csDesigning in ComponentState) then
  begin
    HandleDesignChoice(X,Y);
    Exit;
  end;
  {$ENDIF}

  if ReadOnly then
    Exit;

  str := '';
  idxa := XYToAnchorStep(X, Y);
  if (idxa >= 0) and (idxa <= StepActions.Count - 1) then
  begin
    str := StepActions[idxa].GetAnchorAt(X, Y);
    if str <> '' then
    begin
      if Assigned(OnStepAnchor) then
        OnStepAnchor(Self, idxa, str);
    end;
  end;


  idx := XYToStep(X, Y);
  if (idx >= 0) and (idx <= StepActions.Count - 1) then
  begin
    if str = '' then
    begin
     ActiveStep := idx;
     if Assigned(OnStepMouseUp) then
       OnStepMouseUp(Self, ActiveStep, GetStepMode(ActiveStep),Button, Shift, X, Y);

     if Assigned(OnStepClick) then
       OnStepClick(Self, ActiveStep, GetStepMode(ActiveStep));
    end;
  end;
end;

procedure TAdvSmoothStepControl.NextStep;
begin
  ActiveStep := ActiveStep + 1;
end;

procedure TAdvSmoothStepControl.Notification(AComponent: TComponent;
  AOperation: TOperation);
var
  i: integer;
begin
 if (AOperation = opRemove) and (AComponent = FPictureContainer) then
    FPictureContainer := nil;

  if (AOperation = opRemove) and (AComponent = FImageList) then
    FImageList := nil;

  inherited;

  if (csDestroying in ComponentState) then
    Exit;

  if (AOperation = opRemove) then
  begin
    for I := 0 to StepActions.Count - 1 do
    begin
      if (AComponent = StepActions[I].FItemObject) then
        StepActions[I].FItemObject := nil;
    end;
  end;
end;

procedure TAdvSmoothStepControl.Paint;
var
  g: TGPGraphics;
  r: TGPRectF;
begin
  g := TGPGraphics.Create(Canvas.Handle);
  g.SetSmoothingMode(SmoothingModeAntiAlias);

  r := MakeRect(0, 0, Width - 1, Height - 1);

  case AntiAlias of
    aaClearType: g.SetTextRenderingHint(TextRenderingHintClearTypeGridFit);
    aaAntiAlias: g.SetTextRenderingHint(TextRenderingHintAntiAliasGridFit);
  end;

  Draw(g, r);

  g.Free;

  {$IFDEF DELPHI2006_LVL}
  PaintDesigner;
  {$ENDIF}
end;

{$IFDEF DELPHI2006_LVL}
procedure TAdvSmoothStepControl.PaintDesigner;
var
  R: TRect;
  fh: Integer;
  bc: Boolean;
  P: TPoint;
  cr: TRect;
begin
  if (csDesigning in ComponentState) then
  begin
    r := ClientRect;

    Canvas.Font.Name := 'Tahoma';
    Canvas.Font.Size := 8;
    Canvas.Brush.Style := bsClear;
    Canvas.Pen.Color := clGray; //$B99D7F;

    Canvas.Font.Color := clNavy;
    fh := Canvas.TextHeight('gh') + 2;

    r.Left := r.Right - 60;
    r.Top := r.Bottom - (2 * fh) - 5;


    GetCursorPos(P);
    P := ScreenToClient(P);

    bc := MouseOverDesignChoice(P.X, P.Y);

    if bc then
      Canvas.Font.Style := [fsUnderline]
    else
      Canvas.Font.Style := [];


    cr := Rect(r.Left + 4, r.Top + 3 + fh, r.Left + 4 + 16, r.Top + 2 + fh + 16);

    Canvas.TextOut(r.Left + 4 + 18, r.Top + 4 + fh, s_Edit);
  end;
end;
{$ENDIF}

procedure TAdvSmoothStepControl.PreviousStep;
begin
  ActiveStep := ActiveStep - 1;
end;

procedure TAdvSmoothStepControl.Resize;
begin
  inherited;
  Changed;
end;

procedure TAdvSmoothStepControl.SaveToImage(Filename: String; ImageType: TImageType; ImageQualityPercentage: integer);
var
  gpimg: TGPBitmap;
  enc: TEncoderParameters;
  g: TGPGraphics;
begin
  gpimg := nil;
  try
    gpimg := TGPBitmap.Create(Width, Height);
    g := TGPGraphics.Create(gpimg);
    Draw(g, MakeRect(0, 0, Width - 1, Height - 1));
    g.Free;
    enc := GetEncoderQualityParameters(ImageQualityPercentage);
    gpimg.Save(filename, GetCLSID(ImageType), @enc);
  finally
    gpimg.Free;
  end;
end;

procedure TAdvSmoothStepControl.SetActiveStep(const Value: Integer);
begin
  if FActiveStep <> Value then
  begin
    if Assigned(StepActions) then
    begin
      FActiveStep := Max(-1, Min(Value, StepActions.Count));
    end
    else
      FActiveStep := -1;

    if Assigned(OnStepChanged) then
      onstepChanged(Self, ActiveStep);


    if Animation then
    begin
      FAnimate.Enabled := True;
    end;
    Changed;
  end;
end;

procedure TAdvSmoothStepControl.SetAnimation(const Value: Boolean);
begin
  if FAnimation <> Value then
  begin
    FAnimation := Value;
    Changed;
  end;
end;

procedure TAdvSmoothStepControl.SetAntiAlias(const Value: TAntiAlias);
begin
  if FAntiAlias <> Value then
  begin
    FAntiAlias := Value;
    Changed;
  end;
end;

procedure TAdvSmoothStepControl.SetColorTones(ATones: TColorTones);
begin
  FMetroStyle := True;
  Appearance.InActiveAppearance.BackGroundFill.Color := ATones.Background.BrushColor;
  Appearance.InActiveAppearance.BackGroundFill.ColorTo := ATones.Background.BrushColor;
  Appearance.InActiveAppearance.BackGroundFill.ColorMirror := ATones.Background.BrushColor;
  Appearance.InActiveAppearance.BackGroundFill.ColorMirrorTo := ATones.Background.BrushColor;
  Appearance.InActiveAppearance.BackGroundFill.BorderColor := ATones.Background.BorderColor;
  Appearance.InActiveCaptionFont.Color :=  ATones.Background.TextColor;
  Appearance.InActiveDescriptionFont.Color := ATones.Background.TextColor;

  Appearance.DisabledAppearance.BackGroundFill.Color := ATones.Hover.BrushColor;
  Appearance.DisabledAppearance.BackGroundFill.ColorTo := ATones.Hover.BrushColor;
  Appearance.DisabledAppearance.BackGroundFill.ColorMirror := ATones.Hover.BrushColor;
  Appearance.DisabledAppearance.BackGroundFill.ColorMirrorTo := ATones.Hover.BrushColor;
  Appearance.DisabledAppearance.BackGroundFill.BorderColor := ATones.Hover.BorderColor;
   Appearance.DisabledCaptionFont.Color := ATones.Hover.TextColor;
   Appearance.DisabledDescriptionFont.Color := ATones.Hover.TextColor;

  Appearance.ProcessedAppearance.BackGroundFill.Color := ATones.Hover.BrushColor;
  Appearance.ProcessedAppearance.BackGroundFill.ColorTo := ATones.Hover.BrushColor;
  Appearance.ProcessedAppearance.BackGroundFill.ColorMirror := ATones.Hover.BrushColor;
  Appearance.ProcessedAppearance.BackGroundFill.ColorMirrorTo := ATones.Hover.BrushColor;
  Appearance.ProcessedAppearance.BackGroundFill.BorderColor := ATones.Hover.BorderColor;
   Appearance.ProcessedCaptionFont.Color := ATones.Hover.TextColor;
   Appearance.ProcessedDescriptionFont.Color := ATones.Hover.TextColor;

  Appearance.ActiveAppearance.BackGroundFill.Color := ATones.Selected.BrushColor;
  Appearance.ActiveAppearance.BackGroundFill.ColorTo :=  ATones.Selected.BrushColor;
  Appearance.ActiveAppearance.BackGroundFill.ColorMirror := ATones.Selected.BrushColor;
  Appearance.ActiveAppearance.BackGroundFill.ColorMirrorTo := ATones.Selected.BrushColor;
  Appearance.ActiveAppearance.BackGroundFill.BorderColor := ATones.Selected.BorderColor;
  Appearance.ActiveCaptionFont.Color := ATones.Selected.TextColor;
  Appearance.ActiveDescriptionFont.Color := ATones.Selected.TextColor;

  Fill.Color := ATones.Hover.BrushColor;
  Fill.ColorTo := ATones.Hover.BrushColor;

  Appearance.InActiveAppearance.ShapeColor := ATones.Selected.TextColor;
  Appearance.ActiveAppearance.ShapeColor := ATones.Selected.TextColor;
  Appearance.ProcessedAppearance.ShapeColor := ATones.Selected.TextColor;
  Appearance.DisabledAppearance.ShapeColor := ATones.Selected.TextColor;
end;

procedure TAdvSmoothStepControl.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('TMSStyle',ReadTMSStyle, WriteTMSStyle, True);
end;

procedure TAdvSmoothStepControl.ReadTMSStyle(Reader: TReader);
begin
  FTMSStyle := TTMSStyle(Reader.ReadInteger);
end;

procedure TAdvSmoothStepControl.WriteTMSStyle(Writer: TWriter);
begin
  Writer.WriteInteger(integer(FTMSStyle));
end;

procedure TAdvSmoothStepControl.SetComponentStyle(AStyle: TTMSStyle);
begin
  FTMSStyle := AStyle;
  FMetroStyle := False;
  with Appearance do
  begin

    InActiveCaptionFont.Color := clBlack;

    case AStyle of
      tsOffice2003Blue:
      begin
        with Appearance do
        begin
          InActiveAppearance.BackGroundFill.Color := $FCE1CB;
          InActiveAppearance.BackGroundFill.ColorTo := $E0A57D;
          InActiveAppearance.BackGroundFill.ColorMirror := clNone;
          InActiveAppearance.BackGroundFill.ColorMirrorTo := clNone;
          InActiveAppearance.BackGroundFill.GradientType := gtSolid;
          InActiveAppearance.BackGroundFill.GradientMirrorType := gtVertical;
          InActiveAppearance.BackGroundFill.BorderColor := $962D00;

          ProcessedAppearance.BackGroundFill.Color := $EBFDFF;
          ProcessedAppearance.BackGroundFill.ColorTo := $ACECFF;
          ProcessedAppearance.BackGroundFill.ColorMirror := clNone;
          ProcessedAppearance.BackGroundFill.ColorMirrorTo := clNone;
          ProcessedAppearance.BackGroundFill.GradientType := gtSolid;
          ProcessedAppearance.BackGroundFill.GradientMirrorType := gtVertical;
          ProcessedAppearance.BackGroundFill.BorderColor := $962D00;

          DisabledAppearance.BackGroundFill.Color := $EBFDFF;
          DisabledAppearance.BackGroundFill.ColorTo := $ACECFF;
          DisabledAppearance.BackGroundFill.ColorMirror := clNone;
          DisabledAppearance.BackGroundFill.ColorMirrorTo := clNone;
          DisabledAppearance.BackGroundFill.GradientType := gtSolid;
          DisabledAppearance.BackGroundFill.GradientMirrorType := gtVertical;
          DisabledAppearance.BackGroundFill.BorderColor := $962D00;

          ActiveAppearance.BackGroundFill.Color := $94E6FB;
          ActiveAppearance.BackGroundFill.ColorTo := $1595EE;
          ActiveAppearance.BackGroundFill.ColorMirror := clNone;
          ActiveAppearance.BackGroundFill.ColorMirrorTo := clNone;
          ActiveAppearance.BackGroundFill.GradientType := gtSolid;
          ActiveAppearance.BackGroundFill.GradientMirrorType := gtVertical;
          ActiveAppearance.BackGroundFill.BorderColor := $962D00;

          Fill.Color := $00FFD2AF;
          Fill.ColorTo := $00FFD2AF;
        end;
      end;
      tsOffice2003Silver:
      begin
        with Appearance do
        begin
          InActiveAppearance.BackGroundFill.Color := $ECE2E1;
          InActiveAppearance.BackGroundFill.ColorTo := $B39698;
          InActiveAppearance.BackGroundFill.ColorMirror := clNone;
          InActiveAppearance.BackGroundFill.ColorMirrorTo := clNone;
          InActiveAppearance.BackGroundFill.GradientType := gtSolid;
          InActiveAppearance.BackGroundFill.GradientMirrorType := gtVertical;
          InActiveAppearance.BackGroundFill.BorderColor := $947C7C;

          ProcessedAppearance.BackGroundFill.Color := $EBFDFF;
          ProcessedAppearance.BackGroundFill.ColorTo := $ACECFF;
          ProcessedAppearance.BackGroundFill.ColorMirror := clNone;
          ProcessedAppearance.BackGroundFill.ColorMirrorTo := clNone;
          ProcessedAppearance.BackGroundFill.GradientType := gtSolid;
          ProcessedAppearance.BackGroundFill.GradientMirrorType := gtVertical;
          ProcessedAppearance.BackGroundFill.BorderColor := $947C7C;

          DisabledAppearance.BackGroundFill.Color := $EBFDFF;
          DisabledAppearance.BackGroundFill.ColorTo := $ACECFF;
          DisabledAppearance.BackGroundFill.ColorMirror := clNone;
          DisabledAppearance.BackGroundFill.ColorMirrorTo := clNone;
          DisabledAppearance.BackGroundFill.GradientType := gtSolid;
          DisabledAppearance.BackGroundFill.GradientMirrorType := gtVertical;
          DisabledAppearance.BackGroundFill.BorderColor := $947C7C;

          ActiveAppearance.BackGroundFill.Color := $94E6FB;
          ActiveAppearance.BackGroundFill.ColorTo := $1595EE;
          ActiveAppearance.BackGroundFill.ColorMirror := clNone;
          ActiveAppearance.BackGroundFill.ColorMirrorTo := clNone;
          ActiveAppearance.BackGroundFill.GradientType := gtSolid;
          ActiveAppearance.BackGroundFill.GradientMirrorType := gtVertical;
          ActiveAppearance.BackGroundFill.BorderColor := $947C7C;

          Fill.Color := $00E6D8D8;
          Fill.ColorTo := $00E6D8D8;
        end;
      end;
      tsOffice2003Olive:
       begin
        with Appearance do
        begin
          InActiveAppearance.BackGroundFill.Color := $CFF0EA;
          InActiveAppearance.BackGroundFill.ColorTo := $8CC0B1;
          InActiveAppearance.BackGroundFill.ColorMirror := clNone;
          InActiveAppearance.BackGroundFill.ColorMirrorTo := clNone;
          InActiveAppearance.BackGroundFill.GradientType := gtSolid;
          InActiveAppearance.BackGroundFill.GradientMirrorType := gtVertical;
          InActiveAppearance.BackGroundFill.BorderColor := $588060;

          ProcessedAppearance.BackGroundFill.Color := $EBFDFF;
          ProcessedAppearance.BackGroundFill.ColorTo := $ACECFF;
          ProcessedAppearance.BackGroundFill.ColorMirror := clNone;
          ProcessedAppearance.BackGroundFill.ColorMirrorTo := clNone;
          ProcessedAppearance.BackGroundFill.GradientType := gtSolid;
          ProcessedAppearance.BackGroundFill.GradientMirrorType := gtVertical;
          ProcessedAppearance.BackGroundFill.BorderColor := $947C7C;

          DisabledAppearance.BackGroundFill.Color := $EBFDFF;
          DisabledAppearance.BackGroundFill.ColorTo := $ACECFF;
          DisabledAppearance.BackGroundFill.ColorMirror := clNone;
          DisabledAppearance.BackGroundFill.ColorMirrorTo := clNone;
          DisabledAppearance.BackGroundFill.GradientType := gtSolid;
          DisabledAppearance.BackGroundFill.GradientMirrorType := gtVertical;
          DisabledAppearance.BackGroundFill.BorderColor := $947C7C;

          ActiveAppearance.BackGroundFill.Color := $94E6FB;
          ActiveAppearance.BackGroundFill.ColorTo := $1595EE;
          ActiveAppearance.BackGroundFill.ColorMirror := clNone;
          ActiveAppearance.BackGroundFill.ColorMirrorTo := clNone;
          ActiveAppearance.BackGroundFill.GradientType := gtSolid;
          ActiveAppearance.BackGroundFill.GradientMirrorType := gtVertical;
          ActiveAppearance.BackGroundFill.BorderColor := $588060;

          Fill.Color := $CFF0EA;
          Fill.ColorTo := $CFF0EA;
        end;
      end;
      tsOffice2003Classic:
      begin
        with Appearance do
        begin
          InActiveAppearance.BackGroundFill.Color := clWhite;
          InActiveAppearance.BackGroundFill.ColorTo := $C9D1D5;
          InActiveAppearance.BackGroundFill.ColorMirror := clNone;
          InActiveAppearance.BackGroundFill.ColorMirrorTo := clNone;
          InActiveAppearance.BackGroundFill.GradientType := gtSolid;
          InActiveAppearance.BackGroundFill.GradientMirrorType := gtVertical;
          InActiveAppearance.BackGroundFill.BorderColor := $808080;

          ProcessedAppearance.BackGroundFill.Color := $D2BDB6;
          ProcessedAppearance.BackGroundFill.ColorTo := $D2BDB6;
          ProcessedAppearance.BackGroundFill.ColorMirror := clNone;
          ProcessedAppearance.BackGroundFill.ColorMirrorTo := clNone;
          ProcessedAppearance.BackGroundFill.GradientType := gtSolid;
          ProcessedAppearance.BackGroundFill.GradientMirrorType := gtVertical;
          ProcessedAppearance.BackGroundFill.BorderColor := $808080;

          DisabledAppearance.BackGroundFill.Color := $D2BDB6;
          DisabledAppearance.BackGroundFill.ColorTo := $D2BDB6;
          DisabledAppearance.BackGroundFill.ColorMirror := clNone;
          DisabledAppearance.BackGroundFill.ColorMirrorTo := clNone;
          DisabledAppearance.BackGroundFill.GradientType := gtSolid;
          DisabledAppearance.BackGroundFill.GradientMirrorType := gtVertical;
          DisabledAppearance.BackGroundFill.BorderColor := $808080;


          ActiveAppearance.BackGroundFill.Color := $B59285;
          ActiveAppearance.BackGroundFill.ColorTo := $B59285;
          ActiveAppearance.BackGroundFill.ColorMirror := clNone;
          ActiveAppearance.BackGroundFill.ColorMirrorTo := clNone;
          ActiveAppearance.BackGroundFill.GradientType := gtSolid;
          ActiveAppearance.BackGroundFill.GradientMirrorType := gtVertical;
          ActiveAppearance.BackGroundFill.BorderColor := $962D00;

           Fill.Color := $00F2F2F2;
          Fill.ColorTo := $00F2F2F2;
        end;
      end;
      tsOffice2007Luna:
      begin
        with Appearance do
        begin
          InActiveAppearance.BackGroundFill.Color := $FFEFE3;
          InActiveAppearance.BackGroundFill.ColorTo := $FFDDC4;
          InActiveAppearance.BackGroundFill.ColorMirror := $FFD1AD;
          InActiveAppearance.BackGroundFill.ColorMirrorTo := $FFDBC0;
          InActiveAppearance.BackGroundFill.GradientType := gtSolid;
          InActiveAppearance.BackGroundFill.GradientMirrorType := gtVertical;
          InActiveAppearance.BackGroundFill.BorderColor := $FFD1AD;

          ProcessedAppearance.BackGroundFill.Color := $EBFDFF;
          ProcessedAppearance.BackGroundFill.ColorTo := $ACECFF;
          ProcessedAppearance.BackGroundFill.ColorMirror := $59DAFF;
          ProcessedAppearance.BackGroundFill.ColorMirrorTo := $A4E9FF;
          ProcessedAppearance.BackGroundFill.GradientType := gtSolid;
          ProcessedAppearance.BackGroundFill.GradientMirrorType := gtVertical;
          ProcessedAppearance.BackGroundFill.BorderColor := $99CEDB;

          DisabledAppearance.BackGroundFill.Color := $EBFDFF;
          DisabledAppearance.BackGroundFill.ColorTo := $ACECFF;
          DisabledAppearance.BackGroundFill.ColorMirror := $59DAFF;
          DisabledAppearance.BackGroundFill.ColorMirrorTo := $A4E9FF;
          DisabledAppearance.BackGroundFill.GradientType := gtSolid;
          DisabledAppearance.BackGroundFill.GradientMirrorType := gtVertical;
          DisabledAppearance.BackGroundFill.BorderColor := $99CEDB;

          ActiveAppearance.BackGroundFill.Color := $AAD9FF;
          ActiveAppearance.BackGroundFill.ColorTo := $6EBBFF;
          ActiveAppearance.BackGroundFill.ColorMirror := $42AEFE;
          ActiveAppearance.BackGroundFill.ColorMirrorTo := $7AE1FE;
          ActiveAppearance.BackGroundFill.GradientType := gtSolid;
          ActiveAppearance.BackGroundFill.GradientMirrorType := gtVertical;
          ActiveAppearance.BackGroundFill.BorderColor := $FFD1AD;

          Fill.Color := $DCB698;
          Fill.ColorTo := $DCB698;
        end;
      end;
      tsOffice2007Obsidian:
      begin
        with Appearance do
        begin
          InActiveAppearance.BackGroundFill.Color := $F9F8F8;
          InActiveAppearance.BackGroundFill.ColorTo := $E4E2DF;
          InActiveAppearance.BackGroundFill.ColorMirror := $D1CBC7;
          InActiveAppearance.BackGroundFill.ColorMirrorTo := $E2DEDB;
          InActiveAppearance.BackGroundFill.GradientType := gtSolid;
          InActiveAppearance.BackGroundFill.GradientMirrorType := gtVertical;
          InActiveAppearance.BackGroundFill.BorderColor := clBlack;

          ProcessedAppearance.BackGroundFill.Color := $EBFDFF;
          ProcessedAppearance.BackGroundFill.ColorTo := $ACECFF;
          ProcessedAppearance.BackGroundFill.ColorMirror := $59DAFF;
          ProcessedAppearance.BackGroundFill.ColorMirrorTo := $A4E9FF;
          ProcessedAppearance.BackGroundFill.GradientType := gtSolid;
          ProcessedAppearance.BackGroundFill.GradientMirrorType := gtVertical;
          ProcessedAppearance.BackGroundFill.BorderColor := $99CEDB;

          DisabledAppearance.BackGroundFill.Color := $EBFDFF;
          DisabledAppearance.BackGroundFill.ColorTo := $ACECFF;
          DisabledAppearance.BackGroundFill.ColorMirror := $59DAFF;
          DisabledAppearance.BackGroundFill.ColorMirrorTo := $A4E9FF;
          DisabledAppearance.BackGroundFill.GradientType := gtSolid;
          DisabledAppearance.BackGroundFill.GradientMirrorType := gtVertical;
          DisabledAppearance.BackGroundFill.BorderColor := $99CEDB;


          ActiveAppearance.BackGroundFill.Color := $AAD9FF;
          ActiveAppearance.BackGroundFill.ColorTo := $6EBBFF;
          ActiveAppearance.BackGroundFill.ColorMirror := $42AEFE;
          ActiveAppearance.BackGroundFill.ColorMirrorTo := $7AE1FE;
          ActiveAppearance.BackGroundFill.GradientType := gtSolid;
          ActiveAppearance.BackGroundFill.GradientMirrorType := gtVertical;
          ActiveAppearance.BackGroundFill.BorderColor := clBlack;

          Fill.Color := $5C534C;
          Fill.ColorTo := $5C534C;
        end;
      end;
      tsWindowsXP:
      begin
        with Appearance do
        begin
          InActiveAppearance.BackGroundFill.Color := clWhite;
          InActiveAppearance.BackGroundFill.ColorTo := clBtnFace;
          InActiveAppearance.BackGroundFill.ColorMirror := clNone;
          InActiveAppearance.BackGroundFill.ColorMirrorTo := clNone;
          InActiveAppearance.BackGroundFill.GradientType := gtSolid;
          InActiveAppearance.BackGroundFill.GradientMirrorType := gtVertical;
          InActiveAppearance.BackGroundFill.BorderColor := clBlack;

          ProcessedAppearance.BackGroundFill.Color := clInactiveCaptionText;
          ProcessedAppearance.BackGroundFill.ColorTo := clInactiveCaptionText;
          ProcessedAppearance.BackGroundFill.ColorMirror := clNone;
          ProcessedAppearance.BackGroundFill.ColorMirrorTo := clNone;
          ProcessedAppearance.BackGroundFill.GradientType := gtSolid;
          ProcessedAppearance.BackGroundFill.GradientMirrorType := gtVertical;
          ProcessedAppearance.BackGroundFill.BorderColor := clBlack;

          DisabledAppearance.BackGroundFill.Color := clInactiveCaptionText;
          DisabledAppearance.BackGroundFill.ColorTo := clInactiveCaptionText;
          DisabledAppearance.BackGroundFill.ColorMirror := clNone;
          DisabledAppearance.BackGroundFill.ColorMirrorTo := clNone;
          DisabledAppearance.BackGroundFill.GradientType := gtSolid;
          DisabledAppearance.BackGroundFill.GradientMirrorType := gtVertical;
          DisabledAppearance.BackGroundFill.BorderColor := clBlack;

          ActiveAppearance.BackGroundFill.Color := clInactiveCaption;
          ActiveAppearance.BackGroundFill.ColorTo := clInactiveCaption;
          ActiveAppearance.BackGroundFill.ColorMirror := clNone;
          ActiveAppearance.BackGroundFill.ColorMirrorTo := clNone;
          ActiveAppearance.BackGroundFill.GradientType := gtSolid;
          ActiveAppearance.BackGroundFill.GradientMirrorType := gtVertical;
          ActiveAppearance.BackGroundFill.BorderColor := clBlack;

          Fill.Color := $00B6B6B6;
          Fill.ColorTo := $00B6B6B6;
        end;
      end;
      tsWhidbey:
       begin
        with Appearance do
        begin
          InActiveAppearance.BackGroundFill.Color := $F5F9FA;
          InActiveAppearance.BackGroundFill.ColorTo := $A8C0C0;
          InActiveAppearance.BackGroundFill.ColorMirror := clNone;
          InActiveAppearance.BackGroundFill.ColorMirrorTo := clNone;
          InActiveAppearance.BackGroundFill.GradientType := gtSolid;
          InActiveAppearance.BackGroundFill.GradientMirrorType := gtVertical;
          InActiveAppearance.BackGroundFill.BorderColor := $962D00;

          ProcessedAppearance.BackGroundFill.Color := $94E6FB;
          ProcessedAppearance.BackGroundFill.ColorTo := $1595EE;
          ProcessedAppearance.BackGroundFill.ColorMirror := clNone;
          ProcessedAppearance.BackGroundFill.ColorMirrorTo := clNone;
          ProcessedAppearance.BackGroundFill.GradientType := gtSolid;
          ProcessedAppearance.BackGroundFill.GradientMirrorType := gtVertical;
          ProcessedAppearance.BackGroundFill.BorderColor := clBlack;

          DisabledAppearance.BackGroundFill.Color := $94E6FB;
          DisabledAppearance.BackGroundFill.ColorTo := $1595EE;
          DisabledAppearance.BackGroundFill.ColorMirror := clNone;
          DisabledAppearance.BackGroundFill.ColorMirrorTo := clNone;
          DisabledAppearance.BackGroundFill.GradientType := gtSolid;
          DisabledAppearance.BackGroundFill.GradientMirrorType := gtVertical;
          DisabledAppearance.BackGroundFill.BorderColor := clBlack;

          ActiveAppearance.BackGroundFill.Color := $94E6FB;
          ActiveAppearance.BackGroundFill.ColorTo := $1595EE;
          ActiveAppearance.BackGroundFill.ColorMirror := clNone;
          ActiveAppearance.BackGroundFill.ColorMirrorTo := clNone;
          ActiveAppearance.BackGroundFill.GradientType := gtSolid;
          ActiveAppearance.BackGroundFill.GradientMirrorType := gtVertical;
          ActiveAppearance.BackGroundFill.BorderColor := $962D00;

          Fill.Color := $F5F9FA;
          Fill.ColorTo := $F5F9FA;
        end;
      end;

      tsOffice2007Silver:
      begin
        with Appearance do
        begin
          InActiveAppearance.BackGroundFill.Color := $FAEEEB;
          InActiveAppearance.BackGroundFill.ColorTo := $E5DBD7;
          InActiveAppearance.BackGroundFill.ColorMirror := $E2D8D4;
          InActiveAppearance.BackGroundFill.ColorMirrorTo := $D1C7C5;
          InActiveAppearance.BackGroundFill.GradientType := gtSolid;
          InActiveAppearance.BackGroundFill.GradientMirrorType := gtVertical;
          InActiveAppearance.BackGroundFill.BorderColor := clBlack;

          ProcessedAppearance.BackGroundFill.Color := $EBFDFF;
          ProcessedAppearance.BackGroundFill.ColorTo := $ACECFF;
          ProcessedAppearance.BackGroundFill.ColorMirror := $59DAFF;
          ProcessedAppearance.BackGroundFill.ColorMirrorTo := $A4E9FF;
          ProcessedAppearance.BackGroundFill.GradientType := gtSolid;
          ProcessedAppearance.BackGroundFill.GradientMirrorType := gtVertical;
          ProcessedAppearance.BackGroundFill.BorderColor := clBlack;

          DisabledAppearance.BackGroundFill.Color := $EBFDFF;
          DisabledAppearance.BackGroundFill.ColorTo := $ACECFF;
          DisabledAppearance.BackGroundFill.ColorMirror := $59DAFF;
          DisabledAppearance.BackGroundFill.ColorMirrorTo := $A4E9FF;
          DisabledAppearance.BackGroundFill.GradientType := gtSolid;
          DisabledAppearance.BackGroundFill.GradientMirrorType := gtVertical;
          DisabledAppearance.BackGroundFill.BorderColor := clBlack;

          ActiveAppearance.BackGroundFill.Color := $AAD9FF;
          ActiveAppearance.BackGroundFill.ColorTo := $6EBBFF;
          ActiveAppearance.BackGroundFill.ColorMirror := $42AEFE;
          ActiveAppearance.BackGroundFill.ColorMirrorTo := $7AE1FE;
          ActiveAppearance.BackGroundFill.GradientType := gtSolid;
          ActiveAppearance.BackGroundFill.GradientMirrorType := gtVertical;
          ActiveAppearance.BackGroundFill.BorderColor := clBlack;

          Fill.Color := $00CAC1BA;
          Fill.ColorTo := $00CAC1BA;
        end;
      end;
      tsWindowsVista:
      begin
        with Appearance do
        begin
          InActiveAppearance.BackGroundFill.Color := $FFFDF9;
          InActiveAppearance.BackGroundFill.ColorTo := $FFFAF0;
          InActiveAppearance.BackGroundFill.ColorMirror := clNone;
          InActiveAppearance.BackGroundFill.ColorMirrorTo := clNone;
          InActiveAppearance.BackGroundFill.GradientType := gtSolid;
          InActiveAppearance.BackGroundFill.GradientMirrorType := gtVertical;
          InActiveAppearance.BackGroundFill.BorderColor := $FCF2DA;

          ProcessedAppearance.BackGroundFill.Color := $FEF9F0;
          ProcessedAppearance.BackGroundFill.ColorTo := $FDF0D7;
          ProcessedAppearance.BackGroundFill.ColorMirror := clNone;
          ProcessedAppearance.BackGroundFill.ColorMirrorTo := clNone;
          ProcessedAppearance.BackGroundFill.GradientType := gtSolid;
          ProcessedAppearance.BackGroundFill.GradientMirrorType := gtVertical;
          ProcessedAppearance.BackGroundFill.BorderColor := $FEDF9A;

          DisabledAppearance.BackGroundFill.Color := $FEF9F0;
          DisabledAppearance.BackGroundFill.ColorTo := $FDF0D7;
          DisabledAppearance.BackGroundFill.ColorMirror := clNone;
          DisabledAppearance.BackGroundFill.ColorMirrorTo := clNone;
          DisabledAppearance.BackGroundFill.GradientType := gtSolid;
          DisabledAppearance.BackGroundFill.GradientMirrorType := gtVertical;
          DisabledAppearance.BackGroundFill.BorderColor := $FEDF9A;


          ActiveAppearance.BackGroundFill.Color := $FEF9F0;
          ActiveAppearance.BackGroundFill.ColorTo := $FDF0D7;
          ActiveAppearance.BackGroundFill.ColorMirror := clNone;
          ActiveAppearance.BackGroundFill.ColorMirrorTo := clNone;
          ActiveAppearance.BackGroundFill.GradientType := gtSolid;
          ActiveAppearance.BackGroundFill.GradientMirrorType := gtVertical;
          ActiveAppearance.BackGroundFill.BorderColor := $FEDF9A;

          Fill.Color := $F7EED9;
          Fill.ColorTo := clNone;
        end;
      end;
      tsWindows7:
      begin
        with Appearance do
        begin
          InActiveAppearance.BackGroundFill.Color := $FFFDF9;
          InActiveAppearance.BackGroundFill.ColorTo := $FFFAF0;
          InActiveAppearance.BackGroundFill.ColorMirror := clNone;
          InActiveAppearance.BackGroundFill.ColorMirrorTo := clNone;
          InActiveAppearance.BackGroundFill.GradientType := gtSolid;
          InActiveAppearance.BackGroundFill.GradientMirrorType := gtVertical;
          InActiveAppearance.BackGroundFill.BorderColor := $FCF2DA;

          ProcessedAppearance.BackGroundFill.Color := $FDFBFA;
          ProcessedAppearance.BackGroundFill.ColorTo := $FDF3EB;
          ProcessedAppearance.BackGroundFill.ColorMirror := clNone;
          ProcessedAppearance.BackGroundFill.ColorMirrorTo := clNone;
          ProcessedAppearance.BackGroundFill.GradientType := gtSolid;
          ProcessedAppearance.BackGroundFill.GradientMirrorType := gtVertical;
          ProcessedAppearance.BackGroundFill.BorderColor := $FBD6B8;

          DisabledAppearance.BackGroundFill.Color := $FDFBFA;
          DisabledAppearance.BackGroundFill.ColorTo := $FDF3EB;
          DisabledAppearance.BackGroundFill.ColorMirror := clNone;
          DisabledAppearance.BackGroundFill.ColorMirrorTo := clNone;
          DisabledAppearance.BackGroundFill.GradientType := gtSolid;
          DisabledAppearance.BackGroundFill.GradientMirrorType := gtVertical;
          DisabledAppearance.BackGroundFill.BorderColor := $FBD6B8;


          ActiveAppearance.BackGroundFill.Color := $FCEBDC;
          ActiveAppearance.BackGroundFill.ColorTo :=  $FCDBC1;
          ActiveAppearance.BackGroundFill.ColorMirror := clNone;
          ActiveAppearance.BackGroundFill.ColorMirrorTo := clNone;
          ActiveAppearance.BackGroundFill.GradientType := gtSolid;
          ActiveAppearance.BackGroundFill.GradientMirrorType := gtVertical;
          ActiveAppearance.BackGroundFill.BorderColor := $CEA27D;

          Fill.Color := $F7EED9;
          Fill.ColorTo := clNone;
        end;
      end;
      tsTerminal:
      begin
        with Appearance do
        begin
          InActiveAppearance.BackGroundFill.Color := clBtnFace;
          InActiveAppearance.BackGroundFill.ColorTo := clBtnFace;
          InActiveAppearance.BackGroundFill.ColorMirror := clNone;
          InActiveAppearance.BackGroundFill.ColorMirrorTo := clNone;
          InActiveAppearance.BackGroundFill.GradientType := gtSolid;
          InActiveAppearance.BackGroundFill.GradientMirrorType := gtVertical;
          InActiveAppearance.BackGroundFill.BorderColor := clGray;

          ProcessedAppearance.BackGroundFill.Color := clSilver;
          ProcessedAppearance.BackGroundFill.ColorTo := clSilver;
          ProcessedAppearance.BackGroundFill.ColorMirror := clNone;
          ProcessedAppearance.BackGroundFill.ColorMirrorTo := clNone;
          ProcessedAppearance.BackGroundFill.GradientType := gtSolid;
          ProcessedAppearance.BackGroundFill.GradientMirrorType := gtVertical;
          ProcessedAppearance.BackGroundFill.BorderColor := clGray;

          DisabledAppearance.BackGroundFill.Color := clSilver;
          DisabledAppearance.BackGroundFill.ColorTo := clSilver;
          DisabledAppearance.BackGroundFill.ColorMirror := clNone;
          DisabledAppearance.BackGroundFill.ColorMirrorTo := clNone;
          DisabledAppearance.BackGroundFill.GradientType := gtSolid;
          DisabledAppearance.BackGroundFill.GradientMirrorType := gtVertical;
          DisabledAppearance.BackGroundFill.BorderColor := clGray;

          ActiveAppearance.BackGroundFill.Color := clHighLight;
          ActiveAppearance.BackGroundFill.ColorTo :=  clHighLight;
          ActiveAppearance.BackGroundFill.ColorMirror := clNone;
          ActiveAppearance.BackGroundFill.ColorMirrorTo := clNone;
          ActiveAppearance.BackGroundFill.GradientType := gtSolid;
          ActiveAppearance.BackGroundFill.GradientMirrorType := gtVertical;
          ActiveAppearance.BackGroundFill.BorderColor := clGray;

         Fill.Color := clWhite;
         Fill.ColorTo := clWhite;
        end;
      end;
      tsOffice2010Blue:
      begin
        with Appearance do
        begin
          InActiveAppearance.BackGroundFill.Color := clWhite;
          InActiveAppearance.BackGroundFill.ColorTo := RGB(237, 239, 241);
          InActiveAppearance.BackGroundFill.ColorMirror := clNone;
          InActiveAppearance.BackGroundFill.ColorMirrorTo := clNone;
          InActiveAppearance.BackGroundFill.GradientType := gtSolid;
          InActiveAppearance.BackGroundFill.GradientMirrorType := gtVertical;
          InActiveAppearance.BackGroundFill.BorderColor := RGB(236, 237, 237);

          ProcessedAppearance.BackGroundFill.Color := $8AE3FD;
          ProcessedAppearance.BackGroundFill.ColorTo := clNone;
          ProcessedAppearance.BackGroundFill.ColorMirror := clNone;
          ProcessedAppearance.BackGroundFill.ColorMirrorTo := clNone;
          ProcessedAppearance.BackGroundFill.GradientType := gtSolid;
          ProcessedAppearance.BackGroundFill.GradientMirrorType := gtVertical;
          ProcessedAppearance.BackGroundFill.BorderColor := $58CAF1;

          DisabledAppearance.BackGroundFill.Color := $8AE3FD;
          DisabledAppearance.BackGroundFill.ColorTo := clNone;
          DisabledAppearance.BackGroundFill.ColorMirror := clNone;
          DisabledAppearance.BackGroundFill.ColorMirrorTo := clNone;
          DisabledAppearance.BackGroundFill.GradientType := gtSolid;
          DisabledAppearance.BackGroundFill.GradientMirrorType := gtVertical;
          DisabledAppearance.BackGroundFill.BorderColor := $58CAF1;


          ActiveAppearance.BackGroundFill.Color := $6CD0FF;
          ActiveAppearance.BackGroundFill.ColorTo :=  clNone;
          ActiveAppearance.BackGroundFill.ColorMirror := clNone;
          ActiveAppearance.BackGroundFill.ColorMirrorTo := clNone;
          ActiveAppearance.BackGroundFill.GradientType := gtSolid;
          ActiveAppearance.BackGroundFill.GradientMirrorType := gtVertical;
          ActiveAppearance.BackGroundFill.BorderColor := $308AC2;

         Fill.Color := $EAD3BF;
         Fill.ColorTo := clNone;
        end;
      end;
      tsOffice2010Silver:
      begin
        with Appearance do
        begin
          InActiveAppearance.BackGroundFill.Color := clWhite;
          InActiveAppearance.BackGroundFill.ColorTo := RGB(237, 239, 241);
          InActiveAppearance.BackGroundFill.ColorMirror := clNone;
          InActiveAppearance.BackGroundFill.ColorMirrorTo := clNone;
          InActiveAppearance.BackGroundFill.GradientType := gtSolid;
          InActiveAppearance.BackGroundFill.GradientMirrorType := gtVertical;
          InActiveAppearance.BackGroundFill.BorderColor := RGB(236, 237, 237);

          ProcessedAppearance.BackGroundFill.Color := $8AE3FD;
          ProcessedAppearance.BackGroundFill.ColorTo := clNone;
          ProcessedAppearance.BackGroundFill.ColorMirror := clNone;
          ProcessedAppearance.BackGroundFill.ColorMirrorTo := clNone;
          ProcessedAppearance.BackGroundFill.GradientType := gtSolid;
          ProcessedAppearance.BackGroundFill.GradientMirrorType := gtVertical;
          ProcessedAppearance.BackGroundFill.BorderColor := $58CAF1;

          DisabledAppearance.BackGroundFill.Color := $8AE3FD;
          DisabledAppearance.BackGroundFill.ColorTo := clNone;
          DisabledAppearance.BackGroundFill.ColorMirror := clNone;
          DisabledAppearance.BackGroundFill.ColorMirrorTo := clNone;
          DisabledAppearance.BackGroundFill.GradientType := gtSolid;
          DisabledAppearance.BackGroundFill.GradientMirrorType := gtVertical;
          DisabledAppearance.BackGroundFill.BorderColor := $58CAF1;


          ActiveAppearance.BackGroundFill.Color := $6CD0FF;
          ActiveAppearance.BackGroundFill.ColorTo :=  clNone;
          ActiveAppearance.BackGroundFill.ColorMirror := clNone;
          ActiveAppearance.BackGroundFill.ColorMirrorTo := clNone;
          ActiveAppearance.BackGroundFill.GradientType := gtSolid;
          ActiveAppearance.BackGroundFill.GradientMirrorType := gtVertical;
          ActiveAppearance.BackGroundFill.BorderColor := $308AC2;

          Fill.Color := $D4CFCB;
          Fill.ColorTo := clNone;
        end;
      end;
      tsOffice2010Black:
      begin
        with Appearance do
        begin
          InActiveAppearance.BackGroundFill.Color := clWhite;
          InActiveAppearance.BackGroundFill.ColorTo := RGB(237, 239, 241);
          InActiveAppearance.BackGroundFill.ColorMirror := clNone;
          InActiveAppearance.BackGroundFill.ColorMirrorTo := clNone;
          InActiveAppearance.BackGroundFill.GradientType := gtSolid;
          InActiveAppearance.BackGroundFill.GradientMirrorType := gtVertical;
          InActiveAppearance.BackGroundFill.BorderColor := RGB(236, 237, 237);

          ProcessedAppearance.BackGroundFill.Color := $8AE3FD;
          ProcessedAppearance.BackGroundFill.ColorTo := clNone;
          ProcessedAppearance.BackGroundFill.ColorMirror := clNone;
          ProcessedAppearance.BackGroundFill.ColorMirrorTo := clNone;
          ProcessedAppearance.BackGroundFill.GradientType := gtSolid;
          ProcessedAppearance.BackGroundFill.GradientMirrorType := gtVertical;
          ProcessedAppearance.BackGroundFill.BorderColor := $58CAF1;

          DisabledAppearance.BackGroundFill.Color := $8AE3FD;
          DisabledAppearance.BackGroundFill.ColorTo := clNone;
          DisabledAppearance.BackGroundFill.ColorMirror := clNone;
          DisabledAppearance.BackGroundFill.ColorMirrorTo := clNone;
          DisabledAppearance.BackGroundFill.GradientType := gtSolid;
          DisabledAppearance.BackGroundFill.GradientMirrorType := gtVertical;
          DisabledAppearance.BackGroundFill.BorderColor := $58CAF1;

          ActiveAppearance.BackGroundFill.Color := $6CD0FF;
          ActiveAppearance.BackGroundFill.ColorTo :=  clNone;
          ActiveAppearance.BackGroundFill.ColorMirror := clNone;
          ActiveAppearance.BackGroundFill.ColorMirrorTo := clNone;
          ActiveAppearance.BackGroundFill.GradientType := gtSolid;
          ActiveAppearance.BackGroundFill.GradientMirrorType := gtVertical;
          ActiveAppearance.BackGroundFill.BorderColor := $308AC2;

          Fill.Color := $656565;
          Fill.ColorTo := clNone;
        end;
      end;
        tsWindows8, tsWindows10:
      begin
        with Appearance do
        begin
          InActiveAppearance.BackGroundFill.Color := clWhite;
          InActiveAppearance.BackGroundFill.ColorTo := clWhite;
          InActiveAppearance.BackGroundFill.ColorMirror := clNone;
          InActiveAppearance.BackGroundFill.ColorMirrorTo := clNone;
          InActiveAppearance.BackGroundFill.GradientType := gtSolid;
          InActiveAppearance.BackGroundFill.GradientMirrorType := gtVertical;
          InActiveAppearance.BackGroundFill.BorderColor := $E4E3E2;

          ProcessedAppearance.BackGroundFill.Color := $F7EFE8;
          ProcessedAppearance.BackGroundFill.ColorTo := clNone;
          ProcessedAppearance.BackGroundFill.ColorMirror := clNone;
          ProcessedAppearance.BackGroundFill.ColorMirrorTo := clNone;
          ProcessedAppearance.BackGroundFill.GradientType := gtSolid;
          ProcessedAppearance.BackGroundFill.GradientMirrorType := gtVertical;
          ProcessedAppearance.BackGroundFill.BorderColor := $F9CEA4;

          DisabledAppearance.BackGroundFill.Color := $F7F7F7;
          DisabledAppearance.BackGroundFill.ColorTo := clNone;
          DisabledAppearance.BackGroundFill.ColorMirror := clNone;
          DisabledAppearance.BackGroundFill.ColorMirrorTo := clNone;
          DisabledAppearance.BackGroundFill.GradientType := gtSolid;
          DisabledAppearance.BackGroundFill.GradientMirrorType := gtVertical;
          DisabledAppearance.BackGroundFill.BorderColor := $DEDEDE;

          ActiveAppearance.BackGroundFill.Color := $F7E0C9;
          ActiveAppearance.BackGroundFill.ColorTo :=  clNone;
          ActiveAppearance.BackGroundFill.ColorMirror := clNone;
          ActiveAppearance.BackGroundFill.ColorMirrorTo := clNone;
          ActiveAppearance.BackGroundFill.GradientType := gtSolid;
          ActiveAppearance.BackGroundFill.GradientMirrorType := gtVertical;
          ActiveAppearance.BackGroundFill.BorderColor := $E4A262;

          Fill.Color := clWhite;
          Fill.ColorTo := clNone;
        end;
      end;
        tsOffice2013White:
      begin
        with Appearance do
        begin
          InActiveAppearance.BackGroundFill.Color := clWhite;
          InActiveAppearance.BackGroundFill.ColorTo := clWhite;
          InActiveAppearance.BackGroundFill.ColorMirror := clNone;
          InActiveAppearance.BackGroundFill.ColorMirrorTo := clNone;
          InActiveAppearance.BackGroundFill.GradientType := gtSolid;
          InActiveAppearance.BackGroundFill.GradientMirrorType := gtVertical;
          InActiveAppearance.BackGroundFill.BorderColor := $D4D4D4;

          ProcessedAppearance.BackGroundFill.Color := $FCF0E4;
          ProcessedAppearance.BackGroundFill.ColorTo := clNone;
          ProcessedAppearance.BackGroundFill.ColorMirror := clNone;
          ProcessedAppearance.BackGroundFill.ColorMirrorTo := clNone;
          ProcessedAppearance.BackGroundFill.GradientType := gtSolid;
          ProcessedAppearance.BackGroundFill.GradientMirrorType := gtVertical;
          ProcessedAppearance.BackGroundFill.BorderColor := $EAB47E;

          DisabledAppearance.BackGroundFill.Color := $EEEEEE;
          DisabledAppearance.BackGroundFill.ColorTo := clNone;
          DisabledAppearance.BackGroundFill.ColorMirror := clNone;
          DisabledAppearance.BackGroundFill.ColorMirrorTo := clNone;
          DisabledAppearance.BackGroundFill.GradientType := gtSolid;
          DisabledAppearance.BackGroundFill.GradientMirrorType := gtVertical;
          DisabledAppearance.BackGroundFill.BorderColor := $ACACAC;

          ActiveAppearance.BackGroundFill.Color := $FCE2C8;
          ActiveAppearance.BackGroundFill.ColorTo :=  clNone;
          ActiveAppearance.BackGroundFill.ColorMirror := clNone;
          ActiveAppearance.BackGroundFill.ColorMirrorTo := clNone;
          ActiveAppearance.BackGroundFill.GradientType := gtSolid;
          ActiveAppearance.BackGroundFill.GradientMirrorType := gtVertical;
          ActiveAppearance.BackGroundFill.BorderColor := $E59D56;

          Fill.Color := clWhite;
          Fill.ColorTo := clNone;
        end;
      end;
        tsOffice2013LightGray:
      begin
        with Appearance do
        begin
          InActiveAppearance.BackGroundFill.Color := clWhite;
          InActiveAppearance.BackGroundFill.ColorTo := clWhite;
          InActiveAppearance.BackGroundFill.ColorMirror := clNone;
          InActiveAppearance.BackGroundFill.ColorMirrorTo := clNone;
          InActiveAppearance.BackGroundFill.GradientType := gtSolid;
          InActiveAppearance.BackGroundFill.GradientMirrorType := gtVertical;
          InActiveAppearance.BackGroundFill.BorderColor := $D4D4D4;

          ProcessedAppearance.BackGroundFill.Color := $FCF0E4;
          ProcessedAppearance.BackGroundFill.ColorTo := clNone;
          ProcessedAppearance.BackGroundFill.ColorMirror := clNone;
          ProcessedAppearance.BackGroundFill.ColorMirrorTo := clNone;
          ProcessedAppearance.BackGroundFill.GradientType := gtSolid;
          ProcessedAppearance.BackGroundFill.GradientMirrorType := gtVertical;
          ProcessedAppearance.BackGroundFill.BorderColor := $EAB47E;

          DisabledAppearance.BackGroundFill.Color := $EEEEEE;
          DisabledAppearance.BackGroundFill.ColorTo := clNone;
          DisabledAppearance.BackGroundFill.ColorMirror := clNone;
          DisabledAppearance.BackGroundFill.ColorMirrorTo := clNone;
          DisabledAppearance.BackGroundFill.GradientType := gtSolid;
          DisabledAppearance.BackGroundFill.GradientMirrorType := gtVertical;
          DisabledAppearance.BackGroundFill.BorderColor := $ACACAC;

          ActiveAppearance.BackGroundFill.Color := $FCE2C8;
          ActiveAppearance.BackGroundFill.ColorTo :=  clNone;
          ActiveAppearance.BackGroundFill.ColorMirror := clNone;
          ActiveAppearance.BackGroundFill.ColorMirrorTo := clNone;
          ActiveAppearance.BackGroundFill.GradientType := gtSolid;
          ActiveAppearance.BackGroundFill.GradientMirrorType := gtVertical;
          ActiveAppearance.BackGroundFill.BorderColor := $E59D56;

          Fill.Color := clWhite;
          Fill.ColorTo := clNone;
       end;
      end;
        tsOffice2013Gray:
      begin
        with Appearance do
        begin
          InActiveAppearance.BackGroundFill.Color := clWhite;
          InActiveAppearance.BackGroundFill.ColorTo := clWhite;
          InActiveAppearance.BackGroundFill.ColorMirror := clNone;
          InActiveAppearance.BackGroundFill.ColorMirrorTo := clNone;
          InActiveAppearance.BackGroundFill.GradientType := gtSolid;
          InActiveAppearance.BackGroundFill.GradientMirrorType := gtVertical;
          InActiveAppearance.BackGroundFill.BorderColor := $D4D4D4;

           ProcessedAppearance.BackGroundFill.Color := $FCF0E4;
          ProcessedAppearance.BackGroundFill.ColorTo := clNone;
          ProcessedAppearance.BackGroundFill.ColorMirror := clNone;
          ProcessedAppearance.BackGroundFill.ColorMirrorTo := clNone;
          ProcessedAppearance.BackGroundFill.GradientType := gtSolid;
          ProcessedAppearance.BackGroundFill.GradientMirrorType := gtVertical;
          ProcessedAppearance.BackGroundFill.BorderColor := $EAB47E;

          DisabledAppearance.BackGroundFill.Color := $EEEEEE;
          DisabledAppearance.BackGroundFill.ColorTo := clNone;
          DisabledAppearance.BackGroundFill.ColorMirror := clNone;
          DisabledAppearance.BackGroundFill.ColorMirrorTo := clNone;
          DisabledAppearance.BackGroundFill.GradientType := gtSolid;
          DisabledAppearance.BackGroundFill.GradientMirrorType := gtVertical;
          DisabledAppearance.BackGroundFill.BorderColor := $ACACAC;

          ActiveAppearance.BackGroundFill.Color := $FCE2C8;
          ActiveAppearance.BackGroundFill.ColorTo :=  clNone;
          ActiveAppearance.BackGroundFill.ColorMirror := clNone;
          ActiveAppearance.BackGroundFill.ColorMirrorTo := clNone;
          ActiveAppearance.BackGroundFill.GradientType := gtSolid;
          ActiveAppearance.BackGroundFill.GradientMirrorType := gtVertical;
          ActiveAppearance.BackGroundFill.BorderColor := $E59D56;

          Fill.Color := clWhite;
          Fill.ColorTo := clNone;
       end;
      end;
       tsOffice2016White:
      begin
        with Appearance do
        begin
          InActiveAppearance.BackGroundFill.Color := clWhite;
          InActiveAppearance.BackGroundFill.ColorTo := clWhite;
          InActiveAppearance.BackGroundFill.ColorMirror := clNone;
          InActiveAppearance.BackGroundFill.ColorMirrorTo := clNone;
          InActiveAppearance.BackGroundFill.GradientType := gtSolid;
          InActiveAppearance.BackGroundFill.GradientMirrorType := gtVertical;
          InActiveAppearance.BackGroundFill.BorderColor := $D4D4D4;

          ProcessedAppearance.BackGroundFill.Color := $F2E1D5;
          ProcessedAppearance.BackGroundFill.ColorTo := clNone;
          ProcessedAppearance.BackGroundFill.ColorMirror := clNone;
          ProcessedAppearance.BackGroundFill.ColorMirrorTo := clNone;
          ProcessedAppearance.BackGroundFill.GradientType := gtSolid;
          ProcessedAppearance.BackGroundFill.GradientMirrorType := gtVertical;
          ProcessedAppearance.BackGroundFill.BorderColor := $F2E1D5;

          DisabledAppearance.BackGroundFill.Color := clWhite;
          DisabledAppearance.BackGroundFill.ColorTo := clNone;
          DisabledAppearance.BackGroundFill.ColorMirror := clNone;
          DisabledAppearance.BackGroundFill.ColorMirrorTo := clNone;
          DisabledAppearance.BackGroundFill.GradientType := gtSolid;
          DisabledAppearance.BackGroundFill.GradientMirrorType := gtVertical;
          DisabledAppearance.BackGroundFill.BorderColor := $D4D4D4;

          ActiveAppearance.BackGroundFill.Color := $E3BDA3;
          ActiveAppearance.BackGroundFill.ColorTo :=  clNone;
          ActiveAppearance.BackGroundFill.ColorMirror := clNone;
          ActiveAppearance.BackGroundFill.ColorMirrorTo := clNone;
          ActiveAppearance.BackGroundFill.GradientType := gtSolid;
          ActiveAppearance.BackGroundFill.GradientMirrorType := gtVertical;
          ActiveAppearance.BackGroundFill.BorderColor := $E3BDA3;

          Fill.Color := clWhite;
          Fill.ColorTo := clNone;
        end;
      end;
        tsOffice2016Gray:
      begin
        with Appearance do
        begin
          InActiveAppearance.BackGroundFill.Color := $B2B2B2;
          InActiveAppearance.BackGroundFill.ColorTo := $B2B2B2;
          InActiveAppearance.BackGroundFill.ColorMirror := clNone;
          InActiveAppearance.BackGroundFill.ColorMirrorTo := clNone;
          InActiveAppearance.BackGroundFill.GradientType := gtSolid;
          InActiveAppearance.BackGroundFill.GradientMirrorType := gtVertical;
          InActiveAppearance.BackGroundFill.BorderColor := $444444;

          ProcessedAppearance.BackGroundFill.Color := $F2E1D5;
          ProcessedAppearance.BackGroundFill.ColorTo := clNone;
          ProcessedAppearance.BackGroundFill.ColorMirror := clNone;
          ProcessedAppearance.BackGroundFill.ColorMirrorTo := clNone;
          ProcessedAppearance.BackGroundFill.GradientType := gtSolid;
          ProcessedAppearance.BackGroundFill.GradientMirrorType := gtVertical;
          ProcessedAppearance.BackGroundFill.BorderColor := $F2E1D5;

          DisabledAppearance.BackGroundFill.Color := $B2B2B2;
          DisabledAppearance.BackGroundFill.ColorTo := clNone;
          DisabledAppearance.BackGroundFill.ColorMirror := clNone;
          DisabledAppearance.BackGroundFill.ColorMirrorTo := clNone;
          DisabledAppearance.BackGroundFill.GradientType := gtSolid;
          DisabledAppearance.BackGroundFill.GradientMirrorType := gtVertical;
          DisabledAppearance.BackGroundFill.BorderColor := $444444;

          ActiveAppearance.BackGroundFill.Color := $E3BDA3;
          ActiveAppearance.BackGroundFill.ColorTo :=  clNone;
          ActiveAppearance.BackGroundFill.ColorMirror := clNone;
          ActiveAppearance.BackGroundFill.ColorMirrorTo := clNone;
          ActiveAppearance.BackGroundFill.GradientType := gtSolid;
          ActiveAppearance.BackGroundFill.GradientMirrorType := gtVertical;
          ActiveAppearance.BackGroundFill.BorderColor := $E3BDA3;

          Fill.Color := $B2B2B2;
          Fill.ColorTo := clNone;
       end;
      end;
        tsOffice2016Black:
      begin
        with Appearance do
        begin
          InActiveAppearance.BackGroundFill.Color := $363636;
          InActiveAppearance.BackGroundFill.ColorTo := $363636;
          InActiveAppearance.BackGroundFill.ColorMirror := clNone;
          InActiveAppearance.BackGroundFill.ColorMirrorTo := clNone;
          InActiveAppearance.BackGroundFill.GradientType := gtSolid;
          InActiveAppearance.BackGroundFill.GradientMirrorType := gtVertical;
          InActiveAppearance.BackGroundFill.BorderColor := $444444;

          ProcessedAppearance.BackGroundFill.Color := $6A6A6A;
          ProcessedAppearance.BackGroundFill.ColorTo := clNone;
          ProcessedAppearance.BackGroundFill.ColorMirror := clNone;
          ProcessedAppearance.BackGroundFill.ColorMirrorTo := clNone;
          ProcessedAppearance.BackGroundFill.GradientType := gtSolid;
          ProcessedAppearance.BackGroundFill.GradientMirrorType := gtVertical;
          ProcessedAppearance.BackGroundFill.BorderColor := $6A6A6A;

          DisabledAppearance.BackGroundFill.Color := $363636;
          DisabledAppearance.BackGroundFill.ColorTo := clNone;
          DisabledAppearance.BackGroundFill.ColorMirror := clNone;
          DisabledAppearance.BackGroundFill.ColorMirrorTo := clNone;
          DisabledAppearance.BackGroundFill.GradientType := gtSolid;
          DisabledAppearance.BackGroundFill.GradientMirrorType := gtVertical;
          DisabledAppearance.BackGroundFill.BorderColor := $444444;

          ActiveAppearance.BackGroundFill.Color := $444444;
          ActiveAppearance.BackGroundFill.ColorTo :=  clNone;
          ActiveAppearance.BackGroundFill.ColorMirror := clNone;
          ActiveAppearance.BackGroundFill.ColorMirrorTo := clNone;
          ActiveAppearance.BackGroundFill.GradientType := gtSolid;
          ActiveAppearance.BackGroundFill.GradientMirrorType := gtVertical;
          ActiveAppearance.BackGroundFill.BorderColor := $444444;

          Fill.Color := $363636;
          Fill.ColorTo := clNone;
       end;
      end;

    end;

    SeparatorFill.BorderColor := ActiveAppearance.BackGroundFill.Color;
    SeparatorFill.Color := ActiveAppearance.BackGroundFill.Color;
    SeparatorFill.ColorTo := ActiveAppearance.BackGroundFill.ColorTo;
    SeparatorFill.GradientType := gtVertical;
    SeparatorFill.GradientMirrorType := gtVertical;
    SeparatorFill.ColorMirror := ActiveAppearance.BackGroundFill.ColorMirror;
    SeparatorFill.ColorMirrorTo := ActiveAppearance.BackGroundFill.ColorMirrorTo;

    ActiveAppearance.ShapeColor := InActiveAppearance.BackGroundFill.Color;
    InActiveAppearance.ShapeColor := ActiveAppearance.BackGroundFill.Color;
    ProcessedAppearance.ShapeColor := ActiveAppearance.BackGroundFill.Color;
    DisabledAppearance.ShapeColor := ActiveAppearance.BackGroundFill.Color;

    ProgressBackGround.Assign(InActiveAppearance.BackGroundFill);
    Progress.Assign(ActiveAppearance.BackGroundFill);
    if (Astyle = tsOffice2007Obsidian) and not Transparent and ((Layout = salNone) or (Layout = salProgress)) then
    begin
      InActiveAppearance.CaptionColor := Lighter(ActiveAppearance.BackGroundFill.Color, 75);
      InActiveAppearance.DescriptionColor := Lighter(ActiveAppearance.BackGroundFill.Color, 75);

      ProcessedAppearance.CaptionColor := Lighter(ActiveAppearance.BackGroundFill.Color, 75);
      ProcessedAppearance.DescriptionColor := Lighter(ActiveAppearance.BackGroundFill.Color, 75);

      DisabledAppearance.CaptionColor := Lighter(ActiveAppearance.BackGroundFill.Color, 75);
      DisabledAppearance.DescriptionColor := Lighter(ActiveAppearance.BackGroundFill.Color, 75);

      ActiveAppearance.CaptionColor := Lighter(InActiveAppearance.BackGroundFill.Color, 75);
      ActiveAppearance.DescriptionColor := Lighter(InActiveAppearance.BackGroundFill.Color, 75);
    end
    else
    begin
      InActiveAppearance.CaptionColor := Darker(ActiveAppearance.BackGroundFill.Color, 75);
      InActiveAppearance.DescriptionColor := Darker(ActiveAppearance.BackGroundFill.Color, 75);

      ProcessedAppearance.CaptionColor := Darker(ActiveAppearance.BackGroundFill.Color, 75);
      ProcessedAppearance.DescriptionColor := Darker(ActiveAppearance.BackGroundFill.Color, 75);

      DisabledAppearance.CaptionColor := Darker(ActiveAppearance.BackGroundFill.Color, 75);
      DisabledAppearance.DescriptionColor := Darker(ActiveAppearance.BackGroundFill.Color, 75);

      ActiveAppearance.CaptionColor := Darker(InActiveAppearance.BackGroundFill.Color, 75);
      ActiveAppearance.DescriptionColor := Darker(InActiveAppearance.BackGroundFill.Color, 75);
    end;

    if AStyle = tsOffice2016Black then
    begin
      InActiveAppearance.CaptionColor := $FFFFFF;
      InActiveAppearance.DescriptionColor := $FFFFFF;

      ProcessedAppearance.CaptionColor := $FFFFFF;
      ProcessedAppearance.DescriptionColor := $FFFFFF;

      DisabledAppearance.CaptionColor := $6D6D6D;
      DisabledAppearance.DescriptionColor := $6D6D6D;

      ActiveAppearance.CaptionColor := $FFFFFF;
      ActiveAppearance.DescriptionColor := $FFFFFF;
    end;

    InActiveDescriptionFont.Color := InActiveAppearance.DescriptionColor;
    ActiveDescriptionFont.Color := ActiveAppearance.DescriptionColor;
    ProcessedDescriptionFont.Color := ProcessedAppearance.DescriptionColor;
    DisabledDescriptionFont.Color := DisabledAppearance.DescriptionColor;

    DescriptionFill.Assign(ActiveAppearance.BackGroundFill);

    Fill.BorderColor := InActiveAppearance.BackGroundFill.BorderColor;

  end;
end;

procedure TAdvSmoothStepControl.SetFill(const Value: TGDIPFill);
begin
  if FFill <> Value then
  begin
    FFill.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothStepControl.SetImageList(const Value: TCustomImageList);
begin
  if FImageList <> Value then
  begin
    FImageList := Value;
    Changed;
  end;
end;

procedure TAdvSmoothStepControl.SetPictureContainer(
  const Value: TGDIPPictureContainer);
begin
  if FPictureContainer <> Value then
  begin
    FPictureContainer := Value;
    Changed;
  end;
end;

procedure TAdvSmoothStepControl.SetReadOnly(const Value: Boolean);
begin
  if FReadOnly <> value then
  begin
    FReadOnly := Value;
    Changed;
  end;
end;

procedure TAdvSmoothStepControl.SetShowFocus(const Value: Boolean);
begin
  if FShowFocus <> Value then
  begin
    FShowFocus := Value;
    Changed;
  end;
end;

procedure TAdvSmoothStepControl.SetStepActions(const Value: TStepActions);
begin
  if FStepActions <> Value then
  begin
    FStepActions.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothStepControl.SetStepActionsAppearance(
  const Value: TStepActionsAppearance);
begin
  if FStepActionsAppearance <> Value then
  begin
    FStepActionsAppearance.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothStepControl.SetTransparent(const Value: Boolean);
begin
  if FTransparent <> Value then
  begin
    FTransparent := Value;
    Changed;
  end;
end;

procedure TAdvSmoothStepControl.SetVisibleSteps(const Value: Integer);
begin
  if FVisibleSteps <> Value then
  begin
    FVisibleSteps := Max(0, Value);
    Changed;
  end;
end;

procedure TAdvSmoothStepControl.StepActionsChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothStepControl.UseDefaultStyle;
begin
  FMetroStyle := False;
  with Appearance do
  begin
    ActiveAppearance.BackGroundFill.Color := RGB(244, 244, 244);
    ActiveAppearance.BackGroundFill.ColorTo := clNone;
    ActiveAppearance.BackGroundFill.ColorMirror := RGB(221,221,221);
    ActiveAppearance.BackGroundFill.ColorMirrorTo := RGB(200,200,200);
    ActiveAppearance.BackGroundFill.GradientType := gtSolid;
    ActiveAppearance.BackGroundFill.GradientMirrorType := gtVertical;
    ActiveAppearance.BackGroundFill.BorderColor := clGray;
    ActiveAppearance.CaptionColor := RGB(64, 64, 64);
    ActiveAppearance.DescriptionColor := RGB(64, 64, 64);

    ProcessedAppearance.Assign(InActiveAppearance);
    ProcessedAppearance.CaptionColor := RGB(160, 160, 160);
    ProcessedAppearance.DescriptionColor := RGB(160, 160, 160);

    DisabledAppearance.Assign(ProcessedAppearance);

    InActiveAppearance.BackGroundFill.Color := RGB(253, 199, 80);
    InActiveAppearance.BackGroundFill.ColorTo := clNone;
    InActiveAppearance.BackGroundFill.ColorMirror := RGB(255,148,50);
    InActiveAppearance.BackGroundFill.ColorMirrorTo := RGB(255,111,11);
    InActiveAppearance.BackGroundFill.GradientType := gtSolid;
    InActiveAppearance.BackGroundFill.GradientMirrorType := gtVertical;
    InActiveAppearance.CaptionColor := RGB(64, 64, 64);
    InActiveAppearance.DescriptionColor := RGB(64, 64, 64);
    InActiveAppearance.BackGroundFill.BorderColor := clBlack;

    SeparatorFill.BorderColor := ActiveAppearance.BackGroundFill.Color;
    SeparatorFill.Color := ActiveAppearance.BackGroundFill.Color;
    SeparatorFill.ColorTo := ActiveAppearance.BackGroundFill.ColorTo;
    SeparatorFill.GradientType := gtVertical;
    SeparatorFill.GradientMirrorType := gtVertical;
    SeparatorFill.ColorMirror := ActiveAppearance.BackGroundFill.ColorMirror;
    SeparatorFill.ColorMirrorTo := ActiveAppearance.BackGroundFill.ColorMirrorTo;

    ActiveAppearance.ShapeColor := InActiveAppearance.BackGroundFill.Color;
    InActiveAppearance.ShapeColor := ActiveAppearance.BackGroundFill.Color;
    ProcessedAppearance.ShapeColor := ActiveAppearance.BackGroundFill.Color;
    DisabledAppearance.ShapeColor := ActiveAppearance.BackGroundFill.Color;

    ProgressBackGround.Assign(InActiveAppearance.BackGroundFill);
    Progress.Assign(ActiveAppearance.BackGroundFill);

    Fill.Color := clWhite;
    Fill.BorderColor := clGray;
    Fill.GradientType := gtSolid;

    DescriptionFill.Assign(ActiveAppearance.BackGroundFill);
  end;
end;

procedure TAdvSmoothStepControl.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  if TabStop then
    Message.Result := DLGC_WANTALLKEYS or DLGC_WANTARROWS
  else
    Message.Result := 0;
end;

function TAdvSmoothStepControl.XYToAnchorStep(X, Y: Integer): Integer;
var
  i: integer;
begin
  Result := -1;
  if Self.Width > 0 then
  begin
    for I := GetStartStep to GetStopStep do
    begin
      if ((ActiveStep = I) and StepActions[I].Enabled and PtInGPRect(StepActions[I].FDescRect, Point(X, Y))) or
        PtInGPRect(StepActions[I].FItemRect, Point(X, Y)) then
      begin
        Result := I;
        break;
      end;
    end;
  end;
end;

function TAdvSmoothStepControl.XYToStep(X, Y: Integer): Integer;
var
  i: integer;
begin
  Result := -1;
  if Self.Width > 0 then
  begin
    for I := GetStartStep to GetStopStep do
    begin
      if StepActions[I].Enabled and PtInGPRect(StepActions[I].FItemRect, Point(X, Y)) then
      begin
        Result := I;
        break;
      end;
    end;
  end;
end;

{ TStepActions }

function TStepActions.Add: TStepAction;
begin
  Result := TStepAction(inherited Add);
end;

procedure TStepActions.Clear;
begin
  if Count > 0 then
  begin
    BeginUpdate;
    try
      FOwner.FActiveStep := -1;
      while Count > 0 do
        TCollectionItem(Self[Count - 1]).Free;
    finally
      EndUpdate;
    end;
  end;
end;

constructor TStepActions.Create(AOwner: TAdvSmoothStepControl);
begin
  inherited Create(TStepAction);
  FOwner := AOwner;
end;

procedure TStepActions.Delete(Index: Integer);
begin
  inherited Items[Index].Free;
end;

function TStepActions.GetItem(Index: Integer): TStepAction;
begin
  result := TStepAction(inherited Items[Index]);
end;

function TStepActions.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TStepActions.Insert(Index: Integer): TStepAction;
begin
  result := TStepAction(inherited Insert(Index));
end;

procedure TStepActions.SetItem(Index: Integer; const Value: TStepAction);
begin
  inherited Items[Index] := Value;
end;

{ TStepAction }

procedure TStepAction.Assign(Source: TPersistent);
begin
  if (Source is TStepAction) then
  begin
    FActiveContent.Assign((Source as TStepAction).ActiveContent);
    FInActiveContent.Assign((Source as TStepAction).InActiveContent);
    FDisabledContent.Assign((Source as TStepAction).DisabledContent);
    FProcessedContent.Assign((Source as TStepAction).ProcessedContent);
    FTag := (Source as TStepAction).Tag;
    FItemObject := (Source as TStepAction).ItemObject;
  end;
end;

procedure TStepAction.Changed;
begin
  FOwner.Changed;
end;

constructor TStepAction.Create(Collection: TCollection);
begin
  inherited;
  FOwner := (Collection as TStepActions).FOwner;
  FActiveContent :=  TStepActionContent.Create(Self);
  FInActiveContent := TStepActionContent.Create(Self);
  FProcessedContent := TStepActionContent.Create(Self);
  FDisabledContent := TStepActionContent.Create(Self);
  FEnabled := True;
  FOwner.FCurPos := 0;
  FOwner.Changed;
end;

destructor TStepAction.Destroy;
begin
  FDisabledContent.Free;
  FActiveContent.Free;
  FInActiveContent.Free;
  FProcessedContent.Free;
  inherited;
  FOwner.FCurPos := 0;
  FOwner.Changed;
end;

procedure TStepAction.Draw(g: TGPGraphics; r: TGPRectF);
var
  rtext: TGPRectF;
  s, spc, textspc, border: Integer;
  lb, lbinner: TGPBrush;
  rapp, rappinner: TGPRectF;
  app: TStepActionAppearance;
  f, fdesc: TFont;
  ft, ftdesc: TGPFont;
  ftb, ftbdesc: TGPSolidBrush;
  st, stdesc: TGPStringFormat;
  rcaption, rdescription, rdescriptionFill: TGPRectF;
  imgs: Double;
  appContent: TStepActionContent;
  pic: TAdvGDIPPicture;
  rgn: TGPRegion;
  pth: TGPGraphicsPath;
  pen: TGPPen;
  lbshad: TGPLinearGradientBrush;
  rShad: TGPRectF;
  outline: Double;
  layout: TStepActionsLayout;
  lspcv, lspch: Integer;
  bmp: TBitmap;
  pBorder: TGPPen;
  gpbmp: TGPBitmap;
  gp: TGPGraphics;
  h: HDC;
  ca: TCanvas;
  dofocus: Boolean;
  picw, pich: Integer;
  pthTriangle: TGPGraphicsPath;
  pthgr: TGPPathGradientBrush;
  colors : array[0..0] of TGPColor;
  count: Integer;
  fl: TGDIPFill;
  shpCol: TColor;
  trSz: Double;
  trSpc: Double;
  lbTr: TGPSolidBrush;
begin
  dofocus := FOwner.FFocused and FOwner.TabStop and FOwner.ShowFocus and (FOwner.ActiveStep = Index);
  s := 0;
  if FOwner.Appearance.ShapeStyle <> sasNone then
    s := FOwner.Appearance.ShapeSize;

  spc := 10;
  textspc := 3;
  border := FOwner.Appearance.ShapeBorderWidth;
  outline := 25;
  trSpc := 10;
  trSz := 10 + trSpc;
  lspcv := FOwner.Appearance.LayoutSpacingVertical;
  lspch := FOwner.Appearance.LayoutSpacingHorizontal;

  r := MakeRect(r.X + lspch, r.Y + lspcv, r.Width - lspch * 2, r.Height - lspcv * 2);


  if Enabled then
  begin
    app := FOwner.Appearance.InActiveAppearance;
    f := FOwner.Appearance.InActiveCaptionFont;
    fdesc := FOwner.Appearance.InActiveDescriptionFont;
    appcontent := InActiveContent;

    if (FOwner.ActiveStep = Index) then
    begin
      app := FOwner.Appearance.ActiveAppearance;
      f := FOwner.Appearance.ActiveCaptionFont;
      fdesc := FOwner.Appearance.ActiveDescriptionFont;
      appcontent := ActiveContent;
    end
    else if (FOwner.ActiveStep > Index) then
    begin
      app := FOwner.Appearance.ProcessedAppearance;
      f := FOwner.Appearance.ProcessedCaptionFont;
      fdesc := FOwner.Appearance.ProcessedDescriptionFont;
      appcontent := ProcessedContent;
    end;
  end
  else
  begin
    app := FOwner.Appearance.DisabledAppearance;
    f := FOwner.Appearance.DisabledCaptionFont;
    fdesc := FOwner.Appearance.DisabledDescriptionFont;
    appcontent := DisabledContent;
  end;

  fl := TGDIPFill.Create;
  fl.Assign(app.BackGroundFill);
  if FOwner.FMouseMoveIdx = Index then
  begin
    if fl.Color <> clNone then
      fl.Color := Lighter(fl.Color, 30);
    if fl.ColorTo <> clNone then
      fl.ColorTo := Lighter(fl.ColorTo, 30);
    if fl.ColorMirror <> clNone then
      fl.ColorMirror := Lighter(fl.ColorMirror, 30);
    if fl.ColorMirrorTo <> clNone then
      fl.ColorMirrorTo := Lighter(fl.ColorMirrorTo, 30);
  end;

  shpcol := app.ShapeColor;
  if FOwner.FMouseMoveIdx = Index then
    shpcol := Lighter(shpcol, 30);


//  app.BackGroundFill.Focus := dofocus;
//  app.BackGroundFill.FocusColor := app.BackGroundFill.BorderColor;

  ft := g.MakeFont(f);
  ftdesc := g.MakeFont(fdesc);
  st := TGPStringFormat.Create;
  st.SetFormatFlags(StringFormatFlagsNoWrap);
  st.SetTrimming(StringTrimmingEllipsisWord);

  case appContent.CaptionLocation of
    tlTopCenter: st.SetAlignment(StringAlignmentCenter);
    tlTopRight: st.SetAlignment(StringAlignmentFar);
    tlCenterLeft: st.SetLineAlignment(StringAlignmentCenter);
    tlCenterCenter:
    begin
      st.SetLineAlignment(StringAlignmentCenter);
      st.SetAlignment(StringAlignmentCenter);
    end;
    tlCenterRight:
    begin
      st.SetLineAlignment(StringAlignmentCenter);
      st.SetAlignment(StringAlignmentFar);
    end;
    tlBottomLeft: st.SetLineAlignment(StringAlignmentFar);
    tlBottomCenter:
    begin
      st.SetLineAlignment(StringAlignmentFar);
      st.SetAlignment(StringAlignmentCenter);
    end;
    tlBottomRight:
    begin
      st.SetLineAlignment(StringAlignmentFar);
      st.SetAlignment(StringAlignmentFar);
    end;
  end;

  stdesc := TGPStringFormat.Create;

  ftb := TGPSolidBrush.Create(MakeColor(255, app.CaptionColor));
  ftbdesc := TGPSolidBrush.Create(MakeColor(255, app.DescriptionColor));

  rtext := MakeRect(r.X, r.Y, r.Width, r.Height);

  layout := FOwner.Appearance.Layout;
  if (layout = salPointer) then
  begin
    pth := TGPGraphicsPath.Create;

    if (Index > FOwner.GetStartStep) and (Index < Fowner.GetStopStep) then
    begin
      pth.AddLine(rtext.X - outline / 2, rtext.Y, rtext.X + rtext.Width - outline / 2, rtext.Y);
      pth.AddLine(rtext.X + rText.Width - outline / 2, rText.Y, rText.X + rtext.Width + outline / 2, rText.Y + rText.Height / 2);
      pth.AddLine(rtext.X + rText.Width + outline / 2, rText.Y + rText.Height / 2, rText.X + rtext.Width - outline / 2, rText.Y + rText.Height);
      pth.AddLine(rtext.X + rtext.Width - outline / 2,rtext.Y + rtext.Height,rtext.X - outline / 2, rtext.Y + rtext.Height);
      pth.CloseFigure;
    end
    else if Index = FOwner.GetStopStep then
    begin
      pth.AddRectangle(MakeRect(rtext.X - outline / 2, rtext.Y, rtext.Width + outline / 2, rtext.Height));
    end
    else if Index = FOwner.GetStartStep then
    begin
      pth.AddLine(rtext.X, rtext.Y, rtext.Width - outline / 2, rtext.Y);
      pth.AddLine(rtext.X + rText.Width - (outline / 2) , rText.Y, rText.X + rtext.Width + outline / 2, rText.Y + rText.Height / 2);
      pth.AddLine(rtext.X + rText.Width + outline / 2, rText.Y + rText.Height / 2, rText.X + rtext.Width - outline / 2, rText.Y + rText.Height);
      pth.AddLine(rtext.X + rtext.Width - (outline / 2) , rtext.Y + rtext.Height, rtext.X, rtext.Y + rtext.Height);
      pth.CloseFigure;
    end;

    g.SetClip(pth);

    fl.Fill(g, MakeRect(rText.X - outline , rText.Y, rText.Width + outline * 2, rText.Height));

    g.ResetClip;

    pen := TGPPen.Create(MakeColor(app.BackGroundFill.BorderOpacity, app.BackGroundFill.BorderColor));
    g.DrawPath(pen, pth);

    pen.free;
    pth.Free;


    if not FOwner.FMetroStyle then
    begin
      //shadow
      if index > FOwner.GetStartStep then
        rShad := MakeRect(rtext.X + 1 - outline / 2, rtext.Y + 1, outline, rtext.Height - 2)
      else
        rShad := MakeRect(rtext.X + 1, rtext.Y + 1, outline, rtext.Height - 2);

      lbshad := TGPLinearGradientBrush.Create(MakeRect(rshad.X - 1, rshad.Y - 1, rshad.Width + 2, rshad.Height + 2),
        MakeColor(35, clBlack), MakeColor(0, clBlack), LinearGradientModeHorizontal);

      g.FillRectangle(lbshad, rshad);

      lbshad.Free;

      pen := TGPPen.Create(MakeColor(100, clWhite), 1);

      if (Index < FOwner.GetStopStep) and (Index > FOwner.GetStartStep) then
      begin
        g.DrawLine(pen, rtext.X - outline / 2, rtext.Y + 1, rtext.X + rtext.Width - outline / 2, rtext.Y + 1);
        g.DrawLine(pen, rtext.X + rText.Width - outline / 2, rText.Y + 1, rText.X + rtext.Width + outline / 2, rText.Y + 1 + rText.Height / 2);
      end
      else if Index = FOwner.GetStartStep then
      begin
        g.DrawLine(pen, rtext.X, rtext.Y + 1, rtext.X + rtext.Width - outline / 2, rtext.Y + 1);
        g.DrawLine(pen, rtext.X + rText.Width - outline / 2, rText.Y + 1, rText.X + rtext.Width + outline / 2, rText.Y + 1 + rText.Height / 2);
      end
      else
      begin
        g.DrawLine(pen, rtext.X - outline / 2, rtext.Y + 1, rtext.X + rtext.Width, rtext.Y + 1);
      end;

      pen.Free;
    end;

    if FOwner.Appearance.SeparatorWidth > 0 then
    begin
      pth := TGPGraphicsPath.Create;
      if (Index < FOwner.GetStopStep) then
      begin
        pth.AddLine(rtext.X + rtext.Width - 1 - outline / 2 - FOwner.Appearance.SeparatorWidth, rtext.Y + 1, rtext.X + rtext.Width -1 - outline /2, rtext.Y + 1);
        pth.AddLine(rtext.X + rtext.Width -1 - outline /2, rtext.Y + 1, rtext.X + rtext.Width -2 + outline / 2, rtext.Y + rtext.Height / 2);
        pth.AddLine(rtext.X + rtext.Width -2 + outline /2, rtext.Y + rtext.Height / 2, rtext.X + rtext.Width -1 - outline /2, rtext.Y + rtext.Height - 1);
        pth.AddLine(rtext.X + rtext.Width -1 - outline /2, rtext.Y + rtext.height - 1, rtext.X + rtext.Width - outline / 2 - FOwner.Appearance.SeparatorWidth - 1, rtext.Y + rtext.height - 1);
        pth.AddLine(rtext.X + rtext.Width -1 - outline /2 - Fowner.Appearance.SeparatorWidth, rtext.Y + rtext.Height - 1, rtext.X + rtext.Width -1 + outline / 2 - FOwner.Appearance.SeparatorWidth, rtext.Y + rtext.Height / 2);
        pth.CloseFigure;
      end;

      g.SetClip(pth);

      g.SetSmoothingMode(SmoothingModeAntiAlias);
      FOwner.Appearance.SeparatorFill.Fill(g, MakeRect(rText.X + rText.Width - (outline / 2) - FOwner.Appearance.SeparatorWidth, rtext.Y, outline + FOwner.Appearance.SeparatorWidth, rtext.Height));

      g.ResetClip;

      pen := TGPPEN.Create(MakeColor(FOwner.Appearance.SeparatorFill.BorderOpacity, FOwner.Appearance.SeparatorFill.BorderColor));
      g.DrawPath(pen, pth);
      pen.Free;

      pth.Free;
    end;

    if (Index > FOwner.GetStartStep) then
      rText.X := rText.X - lspch + outline / 2;

    if (Index < FOwner.GetStopStep) and (Index > FOwner.GetStartStep) then
      rText.Width := rtext.Width + lspch - outline
    else
      rText.Width := rtext.Width + lspch - outline / 2;
  end;

  if (layout = salSquare) then
  begin
    pth := GDIPFill.CreateRoundRectangle(MakeRect(r.X, r.Y, r.Width, r.Height), FOwner.Appearance.LayoutRounding, rtBoth, False);

    g.SetClip(pth);

    fl.Fill(g, rText);
    pen := TGPPen.Create(MakeColor(150, clWhite), 1);
    g.DrawLine(pen, rtext.X, rtext.Y + 1, rtext.X + rtext.Width, rtext.Y + 1);
    pen.Free;

    g.ResetClip;

    pBorder := TGPPen.Create(MakeColor(app.BackGroundFill.BorderOpacity, app.BackGroundFill.BorderColor), app.BackGroundFill.BorderWidth);
    g.DrawPath(pBorder, pth);
    pBorder.free;

    pth.Free;

  end;

  if FOwner.Appearance.ShapeStyle <> sasNone then
  begin
    rapp := MakeRect(rtext.X + spc, rtext.Y + (rtext.Height - s) / 2, s, s);
    rappinner := MakeRect(rapp.X + border, rapp.Y + border, rapp.Width - border * 2, rapp.Height - border * 2);
    lb := TGPSolidBrush.Create(MakeColor(125, shpcol));
    lbinner := TGPLinearGradientBrush.Create(MakeRect(rappinner.X - 1, rappinner.Y - 1, rappinner.Width + 1, rappinner.Height + 1),
      MakeColor(255, shpcol), MakeColor(255, Darker(shpcol, 20)), LinearGradientModeVertical);

    case FOwner.Appearance.ShapeStyle of
      sasCircle:
      begin
        g.FillEllipse(lb, rapp);
        g.FillEllipse(lbinner, rappinner);
      end;
      sasSquare:
      begin
        g.FillRectangle(lb, rapp);
        g.FillRectangle(lbinner, rappinner);
      end;
      sasTriangle:
      begin
        rappinner.Y := rappinner.Y + 2;
        pthTriangle := TGPGraphicsPath.Create;
        pthTriangle.AddLine(Makepoint(rapp.X + rapp.Width / 2, rapp.Y), MakePoint(rapp.X + rapp.Width , rapp.Y + rapp.Height));
        pthTriangle.AddLine(MakePoint(rapp.X + rapp.Width , rapp.Y + rapp.Height), MakePoint(rapp.X , rapp.Y + rapp.Height));
        pthTriangle.CloseFigure;
        g.FillPath(lb, pthTriangle);
        pthTriangle.Reset;
        pthTriangle.AddLine(Makepoint(rappinner.X + rappinner.Width / 2, rappinner.Y), MakePoint(rappinner.X + rappinner.Width , rappinner.Y + rappinner.Height));
        pthTriangle.AddLine(MakePoint(rappinner.X + rappinner.Width , rappinner.Y + rappinner.Height), MakePoint(rappinner.X , rappinner.Y + rappinner.Height));
        pthTriangle.CloseFigure;
        pthTriangle.Free;
        pthgr := TGPPathGradientBrush.Create(pthTriangle);
        pthgr.SetCenterColor(MakeColor(200, shpcol));
        count := 1;
        colors[0] := MakeColor(255, Darker(shpcol, 20));
        pthgr.SetSurroundColors(@colors, count);
        g.FillPath(pthgr, pthTriangle);
        pthgr.Free;

      end;
      sasCustom:
      begin
        if Assigned(FOwner.OnStepShapeDraw) then
          FOwner.OnStepShapeDraw(Self, Index, g, rappinner, rapp);
      end;
    end;

    lb.Free;
    lbinner.Free;
  end;

  picw := 0;

  if Assigned(FOwner.PictureContainer) then
  begin
    pic := FOwner.PictureContainer.FindPicture(appContent.ImageName);
    if Assigned(pic) then
    begin
      picw := pic.Width;
      pich := pic.Height;
      pic.GDIPDraw(g, MakeRect(rtext.X + spc + (Max(0, s - picw)) / 2, rtext.Y + (rtext.Height - pich) / 2, picw, pich));
    end;
  end;

  if Assigned(FOwner.ImageList) then
  begin
    if (appContent.ImageIndex >= 0) and (appContent.ImageIndex <= FOwner.ImageList.Count - 1) then
    begin
      bmp := TBitmap.Create;
      FOwner.ImageList.GetBitmap(appContent.ImageIndex, bmp);

      if not bmp.Empty then
      begin
        gpbmp := TGPBitmap.Create(bmp.Width, bmp.Height);
        gp := TGPGraphics.Create(gpbmp);
        h := gp.GetHDC;
        ca := TCanvas.Create;
        ca.Handle := h;
        FOwner.ImageList.Draw(ca, 0, 0, appContent.ImageIndex);
        ca.Free;
        gp.ReleaseHDC(h);
        gp.Free;
        picw := gpbmp.Width;
        pich := gpbmp.Height;
        g.DrawImageRect(gpbmp, MakeRect(rtext.X + spc + (Max(0, s - picw)) / 2, rtext.Y + (rtext.Height - pich) / 2, picw, pich));
        gpbmp.Free;
      end;
      bmp.Free;
    end;
  end;

  rtext.X := rtext.X + Max(s, picw) + spc;
  rtext.Width := rtext.Width - Max(s, picw) - spc;

  if (appContent.Description.Text <> '') and (FOwner.Appearance.DescriptionLayout = dlNormal) then
    rcaption := MakeRect(rtext.X, rtext.Y, rtext.Width, rtext.Height / 2)
  else
    rcaption := MakeRect(rtext.X, rtext.Y, rtext.Width, rtext.Height);

  if (appContent.Description.Text <> '') then
  begin
    case FOwner.Appearance.DescriptionLayout of
      dlNormal:
        rdescription := MakeRect(rtext.X, rtext.Y + rtext.Height / 2, rtext.Width, rtext.Height / 2);
      dlBottom:
        rdescription := MakeRect(lspch, r.Y + rtext.Height + trsz - trSpc, FOwner.Width - 1 - lspch, FOwner.height - rtext.Height - 1 - lspcv - (trsz - trSpc));
    end;

    rdescriptionFill := rdescription;
    rdescription.X := rdescription.X + textspc;
    rdescription.Y := rdescription.Y + textspc;
    rdescription.Width := rdescription.width - textspc * 2;
    rdescription.Height := rdescription.height - textspc * 2;

    if (layout = salProgress) then
      rdescriptionFill.Height := rdescriptionFill.Height - FOwner.Appearance.ProgressSize;
  end;

  rcaption.X := rcaption.X + textspc;
  rcaption.Y := rcaption.Y + textspc;
  rcaption.Width := rcaption.width - textspc * 2;
  rcaption.Height := rcaption.height - textspc * 2;

  if appContent.Caption <> '' then
  begin
    if appContent.CaptionLocation = tlCustom then
      g.DrawString(appcontent.Caption, Length(appcontent.Caption), ft, MakePoint(appcontent.CaptionLeft + rcaption.X, appcontent.CaptionTop + rcaption.Y), st, ftb)
    else
      g.DrawString(appcontent.Caption, Length(appcontent.Caption), ft, rcaption, st, ftb);
  end;


  if appContent.Description.Text <> '' then
  begin
    if (FOwner.Appearance.DescriptionLayout = dlBottom) and (Index = FOwner.ActiveStep) then
    begin
      pthTriangle := TGPGraphicsPath.Create;
      pthTriangle.AddLine(Makepoint(r.X + r.Width / 2, r.Y + r.Height - trSpc), MakePoint(r.X + trSz - trSpc + r.Width / 2, r.Y + r.Height - trSpc + trsz));
      pthTriangle.AddLine(MakePoint(r.X +  trSz - trSpc + r.Width / 2, r.Y + r.Height - trSpc + trSz), MakePoint(r.X -  trSz + trSpc + r.Width / 2, r.Y + r.Height - trSpc + trsz));
      pthTriangle.CloseFigure;
      g.SetClip(pthTriangle);

      lbTr := TGPSolidBrush.Create(MakeColor(FOwner.Appearance.DescriptionFill.Opacity, FOwner.Appearance.DescriptionFill.Color));
      g.FillPath(lbTr, pthTriangle);
      g.ResetClip;
      lbTr.Free;

      pen := TGPPen.Create(MakeColor(FOwner.Appearance.DescriptionFill.BorderOpacity, FOwner.Appearance.DescriptionFill.BorderColor), FOwner.Appearance.DescriptionFill.BorderWidth);
      g.DrawPath(pen, pthTriangle);
      pen.Free;

      pthTriangle.Free;

      FOwner.Appearance.DescriptionFill.Fill(g, rdescriptionFill);

      pen := TGPPen.Create(MakeColor(FOwner.Appearance.DescriptionFill.Opacity, FOwner.Appearance.DescriptionFill.Color), FOwner.Appearance.DescriptionFill.BorderWidth + 1);
      g.DrawLine(pen, r.X + r.Width / 2 - trSz + trSpc + 1, r.Y + r.Height + trSz - trSpc, r.X + r.Width / 2 + trsz - trSpc - 1, r.Y + r.Height + trSz - trSpc);
      pen.Free;

    end;
  end;

  if appContent.Description.Text <> '' then
  begin
    if ((FOwner.Appearance.DescriptionLayout = dlNormal) or ((FOwner.Appearance.DescriptionLayout = dlBottom) and (Index = FOwner.ActiveStep))) then
      if rdescription.Height <> 0 then
        appContent.DrawHTMLText(g, appcontent.Description, rdescription, fdesc, appcontent.Description.Text);
  end;

  if Assigned(FOwner.OnStepDraw) then
    FOwner.OnStepDraw(Self, Index, g, r);

  ftb.Free;
  ftbdesc.Free;
  stdesc.Free;
  st.Free;
  ftdesc.Free;
  ft.Free;
  fl.Free;
end;

function TStepAction.GetAnchorAt(X, Y: Integer): String;
var
  r: TGPRectF;
  bmp: TBitmap;
  g: TGPGraphics;
begin
  r := FItemRect;
  bmp := TBitmap.Create;
  g := TGPGraphics.Create(bmp.Canvas.Handle);
  case FOwner.GetStepMode(Index) of
    mActive: Result := ActiveContent.GetAnchorAt(g, X, Y, R, FOwner.Appearance.ActiveDescriptionFont);
    mInActive: Result := InActiveContent.GetAnchorAt(g, X, Y, R, FOwner.Appearance.InActiveDescriptionFont);
    mProcessed: Result := ProcessedContent.GetAnchorAt(g, X, Y, R, FOwner.Appearance.ProcessedDescriptionFont);
    mDisabled : Result := DisabledContent.GetAnchorAt(g, X, Y, R, FOwner.Appearance.DisabledDescriptionFont);
  end;
  g.Free;
  bmp.Free;
end;

function TStepAction.GetHTMLRect(R: TGPRectF): TGPRectF;
var
  rtext: TGPRectF;
  s, spc, textspc: Integer;
  rdescription: TGPRectF;
  appContent: TStepActionContent;
  pic: TAdvGDIPPicture;
  outline: Double;
  layout: TStepActionsLayout;
  lspcv, lspch: Integer;
  bmp: TBitmap;
  gpbmp: TGPBitmap;
  gp: TGPGraphics;
  h: HDC;
  ca: TCanvas;
  picw: Integer;
  trSz: Double;
  trSpc: Double;
begin
  s := 0;
  if FOwner.Appearance.ShapeStyle <> sasNone then
    s := FOwner.Appearance.ShapeSize;

  spc := 10;
  textspc := 3;
  outline := 25;
  trSpc := 10;
  trSz := 10 + trSpc;
  lspcv := FOwner.Appearance.LayoutSpacingVertical;
  lspch := FOwner.Appearance.LayoutSpacingHorizontal;

  r := MakeRect(r.X + lspch, r.Y + lspcv, r.Width - lspch * 2, r.Height - lspcv * 2);

  if Enabled then
  begin
    appcontent := InActiveContent;
    if FOwner.ActiveStep = Index then
      appcontent := ActiveContent
    else if FOwner.ActiveStep > Index then
      appcontent := ProcessedContent;
  end
  else
    appcontent := DisabledContent;

  rtext := MakeRect(r.X, r.Y, r.Width, r.Height);

  layout := FOwner.Appearance.Layout;
  if (layout = salPointer) then
  begin
    if (Index > FOwner.GetStartStep) then
      rText.X := rText.X - lspch + outline / 2;

    if (Index < FOwner.GetStopStep) and (Index > FOwner.GetStartStep) then
      rText.Width := rtext.Width + lspch - outline
    else
      rText.Width := rtext.Width + lspch - outline / 2;
  end;

  picw := 0;

  if Assigned(FOwner.PictureContainer) then
  begin
    pic := FOwner.PictureContainer.FindPicture(appContent.ImageName);
    if Assigned(pic) then
      picw := pic.Width;
  end;

  if Assigned(FOwner.ImageList) then
  begin
    if (appContent.ImageIndex >= 0) and (appContent.ImageIndex <= FOwner.ImageList.Count - 1) then
    begin
      bmp := TBitmap.Create;
      FOwner.ImageList.GetBitmap(appContent.ImageIndex, bmp);

      if not bmp.Empty then
      begin
        gpbmp := TGPBitmap.Create(bmp.Width, bmp.Height);
        gp := TGPGraphics.Create(gpbmp);
        h := gp.GetHDC;
        ca := TCanvas.Create;
        ca.Handle := h;
        FOwner.ImageList.Draw(ca, 0, 0, appContent.ImageIndex);
        ca.Free;
        gp.ReleaseHDC(h);
        gp.Free;
        picw := gpbmp.Width;
        gpbmp.Free;
      end;
      bmp.Free;
    end;
  end;

  rtext.X := rtext.X + Max(s, picw) + spc;
  rtext.Width := rtext.Width - Max(s, picw) - spc;

  case FOwner.Appearance.DescriptionLayout of
    dlNormal:
      rdescription := MakeRect(rtext.X, rtext.Y + rtext.Height / 2, rtext.Width, rtext.Height / 2);
    dlBottom:
      rdescription := MakeRect(lspch, r.Y + rtext.Height + trsz - trSpc, FOwner.Width - 1 - lspch, FOwner.height - rtext.Height - 1 - lspcv - (trsz - trSpc));
  end;

  rdescription.X := rdescription.X + textspc;
  rdescription.Y := rdescription.Y + textspc;
  rdescription.Width := rdescription.width - textspc * 2;
  rdescription.Height := rdescription.height - textspc * 2;

  result := rdescription;
end;

function TStepAction.Processed: Boolean;
begin
  Result := Index < FOwner.ActiveStep;
end;

procedure TStepAction.SetActiveContent(const Value: TStepActionContent);
begin
  if FActiveContent <> Value then
  begin
    FActiveContent.Assign(Value);
    Changed;
  end;
end;

procedure TStepAction.SetDisabledContent(const Value: TStepActionContent);
begin
  if FDisabledContent <> Value then
  begin
    FDisabledContent.Assign(Value);
    Changed;
  end;
end;

procedure TStepAction.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    Changed;
  end;
end;

procedure TStepAction.SetInActiveContent(const Value: TStepActionContent);
begin
  if FInActiveContent <> Value then
  begin
    FInActiveContent.Assign(Value);
    Changed;
  end;
end;

procedure TStepAction.SetProcessedContent(const Value: TStepActionContent);
begin
  if FProcessedContent <> Value then
  begin
    FProcessedContent.Assign(Value);
    Changed;
  end;
end;

{ TStepActionsAppearance }

procedure TStepActionsAppearance.Assign(Source: TPersistent);
begin
  if (Source is TStepActionsAppearance) then
  begin
    FLayout := (Source as TStepActionsAppearance).Layout;
    FProcessedCaptionFont.Assign((Source as TStepActionsAppearance).ProcessedCaptionFont);
    FDisabledCaptionFont.Assign((Source as TStepActionsAppearance).DisabledCaptionFont);
    FActiveCaptionFont.Assign((Source as TStepActionsAppearance).ActiveCaptionFont);
    FInActiveCaptionFont.Assign((Source as TStepActionsAppearance).InActiveCaptionFont);
    FProcessedDescriptionFont.Assign((Source as TStepActionsAppearance).ProcessedDescriptionFont);
    FDisabledDescriptionFont.Assign((Source as TStepActionsAppearance).DisabledDescriptionFont);
    FActiveDescriptionFont.Assign((Source as TStepActionsAppearance).ActiveDescriptionFont);
    FInActiveDescriptionFont.Assign((Source as TStepActionsAppearance).InActiveDescriptionFont);
    FProcessedAppearance.Assign((Source as TStepActionsAppearance).ProcessedAppearance);
    FDisabledAppearance.Assign((Source as TStepActionsAppearance).DisabledAppearance);
    FActiveAppearance.Assign((Source as TStepActionsAppearance).ActiveAppearance);
    FInActiveAppearance.Assign((Source as TStepActionsAppearance).InActiveAppearance);
    FShapeSize := (Source as TStepActionsAppearance).ShapeSize;
    FShapeStyle := (Source as TStepActionsAppearance).ShapeStyle;
    FShapeBorderWidth := (Source as TStepActionsAppearance).ShapeBorderWidth;
    FLayoutRounding := (Source as TStepActionsAppearance).LayoutRounding;
    FLayoutSpacingVertical := (Source as TStepActionsAppearance).LayoutSpacingVertical;
    FLayoutSpacingHorizontal := (Source as TStepActionsAppearance).LayoutSpacingHorizontal;
    FProgressSize := (Source as TStepActionsAppearance).ProgressSize;
    FProgress.Assign((Source as TStepActionsAppearance).Progress);
    FProgressBackGround.Assign((Source as TStepActionsAppearance).ProgressBackGround);
    FSeparatorWidth := (Source as TStepActionsAppearance).SeparatorWidth;
    FSeparatorFill.Assign((Source as TStepActionsAppearance).SeparatorFill);
    FDescriptionLayout := (Source as TStepActionsAppearance).DescriptionLayout;
    FDescriptionFill.Assign((Source as TStepActionsAppearance).DescriptionFill);
    FDescriptionSize := (Source as TStepActionsAppearance).DescriptionSize;
  end;
end;

procedure TStepActionsAppearance.Changed;
begin
  FOwner.Changed;
end;

constructor TStepActionsAppearance.Create(AOwner: TAdvSmoothStepControl);
begin
  FOwner := AOwner;
  FActiveCaptionFont := TFont.Create;
  FProcessedCaptionFont := TFont.Create;
  FDisabledCaptionFont := TFont.Create;
  FInActiveCaptionFont := TFont.Create;

  FActiveCaptionFont.OnChange := FontChanged;
  FInActiveCaptionFont.OnChange := FontChanged;
  FProcessedCaptionFont.OnChange := FontChanged;
  FDisabledCaptionFont.OnChange := FontChanged;

  FActiveDescriptionFont := TFont.Create;
  FProcessedDescriptionFont := TFont.Create;
  FInActiveDescriptionFont := TFont.Create;
  FDisabledDescriptionFont := TFont.Create;

  FActiveDescriptionFont.OnChange := FontChanged;
  FInActiveDescriptionFont.OnChange := FontChanged;
  FProcessedDescriptionFont.OnChange := FontChanged;
  FDisabledDescriptionFont.OnChange := FontChanged;

  FProcessedAppearance := TStepActionAppearance.Create(FOwner);
  FDisabledAppearance := TStepActionAppearance.Create(FOwner);
  FActiveAppearance := TStepActionAppearance.Create(FOwner);
  FInActiveAppearance := TStepActionAppearance.Create(FOwner);

  FLayout := salPointer;
  FShapeSize := 40;
  FShapeStyle := sasCircle;
  FShapeBorderWidth := 5;
  FLayoutRounding := 0;
  FLayoutSpacingVertical := 0;
  FLayoutSpacingHorizontal := 0;
  FProgressSize := 25;

  FProgressBackGround := TGDIPFill.Create;
  FProgressBackGround.OnChange := FillChanged;

  FProgress := TGDIPFill.Create;
  FProgress.OnChange := FillChanged;

  FSeparatorWidth := 5;
  FSeparatorFill := TGDIPFill.Create;
  FSeparatorFill.OnChange := FillChanged;

  {$IFNDEF DELPHI9_LVL}
  FActiveCaptionFont.Name := 'Tahoma';
  FProcessedCaptionFont.Name := 'Tahoma';
  FInActiveCaptionFont.Name := 'Tahoma';
  FDisabledDescriptionFont.Name := 'Tahoma';
  FActiveDescriptionFont.Name := 'Tahoma';
  FProcessedDescriptionFont.Name := 'Tahoma';
  FInActiveDescriptionFont.Name := 'Tahoma';
  FDisabledCaptionFont.Name := 'Tahoma';
  {$ENDIF}


  FDescriptionLayout := dlNormal;

  FDescriptionFill := TGDIPFill.Create;
  FDescriptionFill.OnChange := FillChanged;

  FDescriptionSize := 50;
end;

destructor TStepActionsAppearance.Destroy;
begin
  FDescriptionFill.Free;
  FActiveCaptionFont.Free;
  FInActiveCaptionFont.Free;
  FProcessedCaptionFont.Free;
  FDisabledCaptionFont.Free;

  FActiveDescriptionFont.Free;
  FInActiveDescriptionFont.Free;
  FProcessedDescriptionFont.Free;
  FDisabledDescriptionFont.Free;

  FProcessedAppearance.Free;
  FDisabledAppearance.Free;
  FActiveAppearance.Free;
  FInActiveAppearance.Free;

  FProgressBackGround.Free;
  FProgress.Free;
  FSeparatorFill.Free;

  inherited;
end;

procedure TStepActionsAppearance.FillChanged(Sender: TObject);
begin
  Changed;
end;

procedure TStepActionsAppearance.FontChanged(Sender: TObject);
begin
  Changed;
end;

procedure TStepActionsAppearance.SetActiveAppearance(
  const Value: TStepActionAppearance);
begin
  if FActiveAppearance <> Value then
  begin
    FActiveAppearance.Assign(Value);
    Changed;
  end;
end;

procedure TStepActionsAppearance.SetActiveCaptionFont(const Value: TFont);
begin
  if FActiveCaptionFont <> Value then
  begin
    FActiveCaptionFont.Assign(Value);
    Changed;
  end;
end;

procedure TStepActionsAppearance.SetActiveDescriptionFont(const Value: TFont);
begin
  if FActiveDescriptionFont <> Value then
  begin
    FActiveDescriptionFont.Assign(Value);
    Changed;
  end;
end;

procedure TStepActionsAppearance.SetDescriptionFill(const Value: TGDIPFill);
begin
  if FDescriptionFill <> Value then
  begin
    FDescriptionFill.Assign(Value);
    Changed;
  end;
end;

procedure TStepActionsAppearance.SetDescriptionLayout(
  const Value: TStepActionsDescriptionLayout);
var
  i: integer;
begin
  if FDescriptionLayout <> Value then
  begin
    if csDesigning in FOwner.ComponentState then
    begin
       for I := 0 to FOwner.StepActions.Count - 1 do
       begin
         case Value of
           dlNormal:
           begin
             FOwner.StepActions[I].ActiveContent.CaptionLocation := tlBottomLeft;
             FOwner.StepActions[I].InActiveContent.CaptionLocation := tlBottomLeft;
             FOwner.StepActions[I].ProcessedContent.CaptionLocation := tlBottomLeft;
             FOwner.StepActions[I].DisabledContent.CaptionLocation := tlBottomLeft;
           end;
           dlBottom:
           begin
             FOwner.StepActions[I].ActiveContent.CaptionLocation := tlCenterCenter;
             FOwner.StepActions[I].InActiveContent.CaptionLocation := tlCenterCenter;
             FOwner.StepActions[I].ProcessedContent.CaptionLocation := tlCenterCenter;
             FOwner.StepActions[I].DisabledContent.CaptionLocation := tlCenterCenter;
           end;
         end;
       end;
    end;
    FDescriptionLayout := Value;
    Changed;
  end;
end;

procedure TStepActionsAppearance.SetDescriptionSize(const Value: Integer);
begin
  if FDescriptionSize <> Value then
  begin
    FDescriptionSize := Value;
    Changed;
  end;
end;

procedure TStepActionsAppearance.SetDisabledAppearance(
  const Value: TStepActionAppearance);
begin
  if FDisabledAppearance <> Value then
  begin
    FDisabledAppearance.Assign(Value);
    Changed;
  end;
end;

procedure TStepActionsAppearance.SetDisabledCaptionFont(const Value: TFont);
begin
  if FDisabledCaptionFont <> Value then
  begin
    FDisabledCaptionFont.Assign(Value);
    Changed;
  end;
end;

procedure TStepActionsAppearance.SetDisabledDescriptionFont(const Value: TFont);
begin
  if FDisabledDescriptionFont <> Value then
  begin
    FDisabledDescriptionFont.Assign(Value);
    Changed;
  end;
end;

procedure TStepActionsAppearance.SetInActiveAppearance(
  const Value: TStepActionAppearance);
begin
  if FInActiveAppearance <> Value then
  begin
    FInActiveAppearance.Assign(Value);
    Changed;
  end;
end;

procedure TStepActionsAppearance.SetInActiveDescriptionFont(const Value: TFont);
begin
  if FInActiveDescriptionFont <> Value then
  begin
    FInActiveDescriptionFont.Assign(Value);
    Changed;
  end;
end;

procedure TStepActionsAppearance.SetInActiveCaptionFont(const Value: TFont);
begin
  if FInActiveCaptionFont <> Value then
  begin
    FInActiveCaptionFont.Assign(Value);
    Changed;
  end;
end;

procedure TStepActionsAppearance.SetLayout(const Value: TStepActionsLayout);
begin
  if FLayout <> Value then
  begin
    FLayout := Value;
    Changed;
  end;
end;

procedure TStepActionsAppearance.SetLayoutRounding(const Value: Integer);
begin
  if FLayoutRounding <> Value then
  begin
    FLayoutRounding := Value;
    Changed;
  end;
end;

procedure TStepActionsAppearance.SetLayoutSpacingHorizontal(
  const Value: Integer);
begin
  if FLayoutSpacingHorizontal <> Value then
  begin
    FLayoutSpacingHorizontal := Value;
    Changed;
  end;
end;

procedure TStepActionsAppearance.SetLayoutSpacingVertical(const Value: Integer);
begin
  if FLayoutSpacingVertical <> Value then
  begin
    FLayoutSpacingVertical := Value;
    Changed;
  end;
end;

procedure TStepActionsAppearance.SetProcessedAppearance(
  const Value: TStepActionAppearance);
begin
  if FProcessedAppearance <> Value then
  begin
    FProcessedAppearance.Assign(Value);
    Changed;
  end;
end;

procedure TStepActionsAppearance.SetProcessedDescriptionFont(const Value: TFont);
begin
  if FProcessedDescriptionFont <> Value then
  begin
    FProcessedDescriptionFont.Assign(Value);
    Changed;
  end;
end;


procedure TStepActionsAppearance.SetProgress(const Value: TGDIPFill);
begin
  if FProgress <> Value then
  begin
    FProgress.Assign(Value);
    Changed;
  end;
end;

procedure TStepActionsAppearance.SetProgressBackGround(const Value: TGDIPFill);
begin
  if FProgressBackGround <> Value then
  begin
    FProgressBackGround.Assign(Value);
    Changed;
  end;
end;

procedure TStepActionsAppearance.SetProgressSize(const Value: Integer);
begin
  if FProgressSize <> Value then
  begin
    FProgressSize := Value;
    Changed;
  end;
end;

procedure TStepActionsAppearance.SetSeparatorFill(const Value: TGDIPFill);
begin
  if FSeparatorFill <> Value then
  begin
    FSeparatorFill := Value;
    Changed;
  end;
end;

procedure TStepActionsAppearance.SetSeparatorWidth(const Value: Integer);
begin
  if FSeparatorWidth <> Value then
  begin
    FSeparatorWidth := Value;
    Changed;
  end;
end;

procedure TStepActionsAppearance.SetShapeBorderWidth(const Value: Integer);
begin
  if FShapeBorderWidth <> Value then
  begin
    FShapeBorderWidth := Value;
    Changed;
  end;
end;

procedure TStepActionsAppearance.SetShapeSize(const Value: Integer);
begin
  if FShapeSize <> Value then
  begin
    FShapeSize := Value;
    Changed;
  end;
end;

procedure TStepActionsAppearance.SetShapeStyle(const Value: TStepActionsShape);
begin
  if FShapeStyle <> value then
  begin
    FShapeStyle := Value;
    Changed;
  end;
end;

procedure TStepActionsAppearance.SetProcessedCaptionFont(const Value: TFont);
begin
  if FProcessedCaptionFont <> Value then
  begin
    FProcessedCaptionFont.Assign(Value);
    Changed;
  end;
end;

{ TStepActionAppearance }

procedure TStepActionAppearance.Assign(Source: TPersistent);
begin
  if Source is TStepActionAppearance then
  begin
    FShapeColor := (Source as TStepActionAppearance).ShapeColor;
    FDescriptionColor := (Source as TStepActionAppearance).DescriptionColor;
    FCaptionColor := (Source as TStepActionAppearance).CaptionColor;
    FBackGroundFill.Assign((Source as TStepActionAppearance).BackGroundFill);
  end;
end;

procedure TStepActionAppearance.Changed;
begin
  FOwner.Changed;
end;

constructor TStepActionAppearance.Create(AOwner: TAdvSmoothStepControl);
begin
  FOwner := AOwner;
  FShapeColor := clGreen;
  FDescriptionColor := clGray;
  FCaptionColor := clBlack;
  FBackGroundFill := TGDIPFill.Create;
  FBackGroundFill.OnChange := FillChanged;
end;

destructor TStepActionAppearance.Destroy;
begin
  FBackGroundFill.Free;
  inherited;
end;

procedure TStepActionAppearance.FillChanged(Sender: TObject);
begin
  Changed;
end;

procedure TStepActionAppearance.SetBackGroundFill(const Value: TGDIPFill);
begin
  if FBackGroundFill <> Value then
  begin
    FBackGroundFill.Assign(Value);
    Changed;
  end;
end;

procedure TStepActionAppearance.SetCaptionColor(const Value: TColor);
begin
  if FCaptionColor <> Value then
  begin
    FCaptionColor := Value;
    Changed;
  end;
end;

procedure TStepActionAppearance.SetShapeColor(const Value: TColor);
begin
  if FShapeColor <> Value then
  begin
    FShapeColor := Value;
    Changed;
  end;
end;

procedure TStepActionAppearance.SetDescriptionColor(const Value: TColor);
begin
  if FDescriptionColor <> Value then
  begin
    FDescriptionColor := Value;
    Changed;
  end;
end;

{ TStepActionContent }

procedure TStepActionContent.Assign(Source: TPersistent);
begin
  if (Source is TStepActionContent) then
  begin
    FCaption := (Source as TStepActionContent).Caption;
    FDescription.Assign((Source as TStepActionContent).Description);
    FImageIndex := (Source as TStepActionContent).ImageIndex;
    FImageName := (Source as TStepActionContent).ImageName;
    FCaptionLocation := (Source as TStepActionContent).CaptionLocation;
    FCaptionLeft := (Source as TStepActionContent).CaptionLeft;
    FCaptionTop := (Source as TStepActionContent).CaptionTop;
  end;
end;

procedure TStepActionContent.Changed;
begin
  FOwner.Changed;
end;

constructor TStepActionContent.Create(AOwner: TStepAction);
begin
  FOwner := AOwner;

  FDescription := TStepActionContentHTMLText.Create(Self);

  if (csDesigning in FOwner.FOwner.ComponentState) and not (csLoading in FOwner.FOwner.ComponentState) then
  begin
    FCaption := 'Step ' + inttostr((FOwner.FOwner.StepActions).Count);
    FDescription.Text := 'Description for ' + FCaption;
    FHint := 'Hint for ' + FCaption;
  end;

  FCaptionLocation := tlBottomLeft;
  FCaptionLeft := 0;
  FCaptionTop := 0;

  FImageIndex := -1;
  FOwner.Changed;
end;

destructor TStepActionContent.Destroy;
begin
  FDescription.Free;
  inherited;
end;

function TStepActionContent.DrawHTMLText(g: TGPGraphics;
  HTML: TStepActionContentHTMLText; r: TGPRectF; ft: TFont; str: String; DoAnchor: Boolean;
  fX, fY: integer): String;
var
  htmlr: TRect;
  a, s, k: String;
  l, m, XSize, YSize: integer;
  hr: TRect;
  x, y: Double;
  rgn: TGPRegion;
begin
  with HTML do
  begin
    if str <> '' then
    begin
      htmlr := Bounds(Round(r.X), Round(r.Y), Round(r.Width), Round(r.Height));

      HTMLDrawGDIP(g, ft, str,htmlr,FOwner.FOwner.FOwner.FImageList, 0,0,-1,-1,FShadowOffset,False,true,false,false,
        False,False,true,1.0,FURLColor,clNone,clNone,FShadowColor,a,s,k,XSize,YSize,l,m,hr,nil,FOwner.FOwner.FOwner.FPictureContainer,2);

      xsize := xsize + 2;
      ysize := ysize + 2;

      if FLocation <> tlCustom then
        GetObjectLocation(x, y, r, XSize, YSize, FLocation)
      else
      begin
        x := FLeft + r.X;
        y := FTop + r.Y;
      end;

      htmlr := Bounds(Round(x), Round(y), Round(r.Width), Round(r.Height));

      rgn := TGPRegion.Create(r);
      g.SetClip(rgn);

      HTMLDrawGDIP(g, ft, str,htmlr,FOwner.FOwner.FOwner.FImageList, fx,fy,-1,-1,FShadowOffset,DoAnchor,false,false,false,
        False,False,true,1.0,FURLColor,clNone,clNone,FShadowColor,a,s,k,XSize,YSize,l,m,hr,nil,FOwner.FOwner.FOwner.FPictureContainer,2);

      g.ResetClip;
      rgn.Free;

      result := a;
    end;
  end;
end;

function TStepActionContent.GetAnchorAt(g: TGPGraphics; X, Y: integer;
  R: TGPRectF; ft: TFont): String;
begin
  result := DrawHTMLText(g, Description, FOwner.GetHTMLRect(R), ft, Description.Text, true, X, Y);
end;

procedure TStepActionContent.SetCaption(const Value: String);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    Changed;
  end;
end;

procedure TStepActionContent.SetCaptionLeft(const Value: Integer);
begin
  if FCaptionLeft <> Value then
  begin
    FCaptionLeft := Value;
    Changed;
  end;
end;

procedure TStepActionContent.SetCaptionLocation(
  const Value: TStepActionTextLocation);
begin
  if FCaptionLocation <> Value then
  begin
    FCaptionLocation := Value;
    Changed;
  end;
end;

procedure TStepActionContent.SetCaptionTop(const Value: Integer);
begin
  if FCaptionTop <> Value then
  begin
    FCaptionTop := Value;
    Changed;
  end;
end;

procedure TStepActionContent.SetDescription(const Value: TStepActionContentHTMLText);
begin
  if FDescription <> Value then
  begin
    FDescription.Assign(Value);
    Changed;
  end;
end;

procedure TStepActionContent.SetHint(const Value: String);
begin
  if FHint <> Value then
  begin
    FHint := Value;
    Changed;
  end;
end;

procedure TStepActionContent.SetImageIndex(const Value: Integer);
begin
  if FImageIndex <> value then
  begin
    FImageIndex := Value;
    Changed;
  end;
end;

procedure TStepActionContent.SetImageName(const Value: string);
begin
  if FImageName <> value then
  begin
    FImageName := Value;
    Changed;
  end;
end;

{ TStepActionContentHTMLText }

procedure TStepActionContentHTMLText.Assign(Source: TPersistent);
begin
  if (Source is TStepActionContentHTMLText) then
  begin
    FURLColor := (Source as TStepActionContentHTMLText).URLColor;
    FShadowOffset := (Source as TStepActionContentHTMLText).ShadowOffset;
    FShadowColor := (Source as TStepActionContentHTMLText).ShadowColor;
    FFont.Assign((Source as TStepActionContentHTMLText).Font);
    FText := (Source as TStepActionContentHTMLText).Text;
    FLocation := (Source as TStepActionContentHTMLText).Location;
    FTop := (Source as TStepActionContentHTMLText).Top;
    FLeft := (Source as TStepActionContentHTMLText).Left;
    Changed;
  end
end;

procedure TStepActionContentHTMLText.Changed;
begin
  FOwner.Changed;
end;

constructor TStepActionContentHTMLText.Create(
  AOwner: TStepActionContent);
begin
  FOwner := AOwner;
  FURLColor := clBlue;
  FShadowOffset := 5;
  FShadowColor := clGray;
  FLocation := tlTopLeft;
  FFont := TFont.Create;
  {$IFNDEF DELPHI9_LVL}
  FFont.Name := 'Tahoma';
  {$ENDIF}
  FTop := 0;
  FLeft := 0;
end;

destructor TStepActionContentHTMLText.Destroy;
begin
  FFont.Free;
  inherited;
end;

procedure TStepActionContentHTMLText.SetFont(const Value: TFont);
begin
  if FFont <> Value then
  begin
    FFont.Assign(Value);
    Changed;
  end;
end;

procedure TStepActionContentHTMLText.SetLeft(const Value: integer);
begin
  if FLeft <> Value then
  begin
    FLeft := Value;
    Changed;
  end;
end;

procedure TStepActionContentHTMLText.SetLocation(
  const Value: TStepActionTextLocation);
begin
  if FLocation <> value then
  begin
    FLocation := Value;
    Changed;
  end;
end;

procedure TStepActionContentHTMLText.SetShadowColor(const Value: TColor);
begin
  if FShadowColor <> value then
  begin
    FShadowColor := Value;
    Changed;
  end;
end;

procedure TStepActionContentHTMLText.SetShadowOffset(const Value: integer);
begin
  if FShadowOffset <> value then
  begin
    FShadowOffset := Value;
    Changed;
  end;
end;

procedure TStepActionContentHTMLText.SetText(const Value: string);
begin
  if FText <> value then
  begin
    FText := Value;
    Changed;
  end;
end;

procedure TStepActionContentHTMLText.SetTop(const Value: integer);
begin
  if FTop <> Value then
  begin
    FTop := Value;
    Changed;
  end;
end;

procedure TStepActionContentHTMLText.SetURLColor(const Value: TColor);
begin
  if FURLColor <> Value then
  begin
    FURLColor := Value;
    Changed;
  end;
end;

end.
