{ ************************************************************************** }
{ TAdvSmoothTimeLine component                                               }
{ for Delphi & C++Builder                                                    }
{                                                                            }
{ written                                                                    }
{ TMS Software                                                               }
{ copyright © 2012 - 2015                                                    }
{ Email : info@tmssoftware.com                                               }
{ Web : http://www.tmssoftware.com                                           }
{                                                                            }
{ The source code is given as is. The author is not responsible              }
{ for any possible damage done due to the use of this code.                  }
{ The component can be freely used in any application. The complete          }
{ source code remains property of the author and may not be distributed,     }
{ published, given or sold in any form as such. No parts of the source       }
{ code can be included in any other component or application without         }
{ written authorization of the author.                                       }
{ ************************************************************************** }

unit AdvSmoothTimeLine;
{$I TMSDEFS.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, StdCtrls, forms,
  Math, AdvStyleIF, ImgList, DateUtils, ExtCtrls,
  GDIPFill, Menus,
  AdvGDIP
  , Types
  {$IFDEF DELPHIXE3_LVL}
  ,Generics.Collections  , System.UITypes
  {$ENDIF}
  ;

const
  MAJ_VER = 2; // Major version nr.
  MIN_VER = 6; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 1; // Build nr.

  // version history
  // v1.0.0.0 : first release
  // v1.0.1.0 : New : Property Fixed to set indicators to a fixed position
  // v1.0.2.0 : New : Support for Windows Vista and Windows Seven Style
  // v1.0.3.0 : New : Zooming and scrolling in timeline.
  //          : New : Draggable and Sizeable Sections
  //          : New : Caption on section
  //          : New : Customizable Annotations.
  //          : New : Built-in support for reduced color set for use with terminal servers
  // v1.1.0.0 : New : Support for Mouse wheel scrolling
  //          : New : ItemObject and Tag property on Indicators and Sections
  //          : Fixed : Issue with automatically calculation of section hints
  //          : Fixed : Issue with section handle animation in Mouse leave situation
  //          : Fixed : issue with invalid argument to date encode
  //          : Improved : issue with section caption in small sections
  //          : Improved : OnIndicatorClick when indicator is fixed
  // v1.1.1.0 : New : ScrollStep to define amount of mouse wheel scrolling
  //          : Fixed : Issue with readonly blocking hints
  //          : Fixed : Mousescrolling occurs when allowscrolling = false
  //          : Fixed : Issue with OnSectionClick, OnIndicatorClick
  //          : Fixed : Issue with focusing control
  //          : Improved : Calculating division and subdivisions
  // v1.1.1.1 : Fixed : Issue with fixed indicators on the same position
  //          : Fixed : Invalid floating point operation when changing divisiontype when divisions / subdivisions are 0
  // v1.1.1.2 : Fixed : Issue with Onindicator click
  //          : Improved : Indicator click at equal positions
  // v1.1.1.3 : Fixed : Issue with TimeLine , Indicator and Section Interaction
  // v1.1.2.0 : New : Delphi 2010 Touch Support
  //          : Fixed : Issue with TimeLine Multiple Section sizing
  // v1.1.2.1 : Fixed : Issue with C++ Builder array declaration
  //          : Fixed : Issue with indicator on top of fixed section
  // v1.1.3.0 : New : event OnDrawTimeLine
  // v2.0.0.0 : New : Database aware version of TAdvSmoothTimeLine
  //          : New : Property AnnotationTextColor
  //          : Fixed : Issue with Division By Zero
  // v2.0.1.0 : New : OnScrollTimeLine event
  //          : Fixed : Issue with while loop
  // v2.0.2.0 : New : Built-in support for Office 2010 colors
  // v2.0.2.1 : Fixed : Issue with labels disappearing and incorrect positioning.
  // v2.0.2.2 : Fixed : Issue with starting and ending labels not displaying
  //          : Fixed : Issue with indenting labels
  // v2.1.0.0 : New : Overlapping sections
  //          : Improved Handle show and hide on multiple section hovering
  // v2.1.1.0 : Fixed : Issue with designtime range values
  //          : Fixed : Issue with sections disappearing when moving
  //          : Improved : Issue with Out of range divisions and subdivisions
  // v2.1.1.1 : Fixed: Issue with hovering sections with same starttime and endtime
  // v2.1.1.2 : Fixed: Issue with resize handles in section out of range
  // v2.1.1.3 : Improved : GetAnnotationRect function which returns the rectangle of the annotation
  // v2.1.1.4 : Improved : Interaction with sections from first to last visible section
  // v2.1.2.0 : Improved : Support for TDateTime < 0
  // v2.2.0.0 : New : PopupMenu support
  //          : New : DB Annotation ImageIndex databinding
  // v2.2.0.1 : Fixed : Issue with changing section parameters and rebuilding
  // v2.3.0.0 : New : Metro style support
  // v2.3.1.0 : New : AnnotationAutoPosition
  // v2.3.2.0 : New : OnIndicatorDblClick event added
  // v2.4.0.0 : New : OnIndicatorPositionCanChange, OnSectionPositionCanChange
  //          : New : Autosizing for divisions and subdivisions
  // v2.4.0.1 : Fixed : Issue with dynamic array in XE3
  // v2.4.0.2 : Fixed : Issue with indicator onchange
  // v2.4.0.3 : Improved : csAcceptControls flag added to allow adding controls inside the control.
  //          : Improved : AnnotationTopHeight and AnnotationBottomHeight to return the used space by the annotations
  // v2.5.0.0 : New : Windows 8, Office 2013 styles added
  // v2.5.0.1 : Fixed : Issue with hint not showing on control
  // v2.5.0.2 : Improved : Added ability to handle negative vertical margins
  // v2.5.0.3 : Fixed : Issue with indicator click index
  // v2.6.0.0 : New : Windows 10, Office 2016 styles added
  // v2.6.0.1 : Improved : ftGraphic support

type
  TWinCtrl = class(TWinControl)
  public
    procedure PaintCtrls(DC: HDC; First: TControl);
  end;

  TAdvSmoothTimeLine = class;

  TAdvSmoothTimeLineCaptionLocation = (cpTopLeft, cpTopCenter, cpTopRight,
    cpCenterLeft, cpCenterCenter, cpCenterRight, cpBottomLeft, cpBottomCenter,
    cpBottomRight, cpCustom);

  TAdvSmoothSectionHint = class(THintWindow)
  private
    FMainBuffer: TGPBitmap;
    FTimeLine: TAdvSmoothTimeLine;
    procedure CMMouseLeave(var Message: TMessage);
    message CM_MOUSELEAVE;
    procedure WMPaint(var Message: TWMPaint);
    message WM_PAINT;
    procedure WMEraseBkGnd(var Message: TWMEraseBkGnd);
    message WM_ERASEBKGND;
    procedure WMNCHitTest(var Msg: TWMNCHitTest);
    message WM_NCHITTEST;
    Procedure CMDialogKey(Var Msg: TWMKey);
    message CM_DIALOGKEY;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  protected
    procedure CreateWnd; override;
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;

    // ---- Calc proc
    procedure UpdateButtons;

    // ---- Paint proc
    procedure Draw(Graphics: TGPGraphics);

    // ---- Paint buffer
    procedure CreateMainBuffer;
    procedure DestroyMainBuffer;
    procedure ClearBuffer(Graphics: TGPGraphics);
    function CreateGraphics: TGPGraphics;

    // ---- Layered window
    procedure SetLayeredWindow;
    procedure UpdateLayered;
    procedure UpdateMainWindow;
    procedure UpdateWindow;
  public
    procedure Init;
    property TimeLine: TAdvSmoothTimeLine read FTimeLine write FTimeLine;
  end;

  TAdvSmoothTimeLineRangeAppearance = class(TPersistent)
  private
    FOwner: TAdvSmoothTimeLine;
    FSubDivisionTickMarkColor: TColor;
    FDivisionTickMarkColor: TColor;
    FSubDivisionFont: TFont;
    FDivisionFont: TFont;
    FSubDivisionTickMarkSize: Integer;
    FDivisionTickMarkSize: Integer;
    FOnChange: TNotifyEvent;
    FSubDivisionTickMarkWidth: Integer;
    FDivisionTickMarkWidth: Integer;
    FDivisionFormat: String;
    FSubDivisionFormat: String;
    FIndentSpacing: Integer;
    FShowSubDivisionValue: Boolean;
    FShowDivisionValue: Boolean;
    procedure SetDivisionTickMarkColor(const Value: TColor);
    procedure SetDivisionFont(const Value: TFont);
    procedure SetSubDivisionFont(const Value: TFont);
    procedure SetSubDivisionTickMarkColor(const Value: TColor);
    procedure SetDivisionTickMarkSize(const Value: Integer);
    procedure SetSubDivisionTickMarkSize(const Value: Integer);
    procedure SetDivisionTickMarkWidth(const Value: Integer);
    procedure SetSubDivisionTickMarkWidth(const Value: Integer);
    procedure SetDivisionFormat(const Value: String);
    procedure SetSubDivisionFormat(const Value: String);
    procedure SetIndentSpacing(const Value: Integer);
    procedure SetShowDivisionValue(const Value: Boolean);
    procedure SetShowSubDivisionValue(const Value: Boolean);
  protected
    procedure Changed;
    procedure FontChanged(Sender: TObject);
  public
    constructor Create(AOwner: TAdvSmoothTimeLine);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property DivisionFont: TFont read FDivisionFont write SetDivisionFont;
    property SubDivisionFont
      : TFont read FSubDivisionFont write SetSubDivisionFont;
    property DivisionTickMarkColor: TColor read FDivisionTickMarkColor write
      SetDivisionTickMarkColor default clBlack;
    property SubDivisionTickMarkColor
      : TColor read FSubDivisionTickMarkColor write SetSubDivisionTickMarkColor
      default clBlack;
    property DivisionTickMarkSize: Integer read FDivisionTickMarkSize write
      SetDivisionTickMarkSize default 10;
    property SubDivisionTickMarkSize
      : Integer read FSubDivisionTickMarkSize write SetSubDivisionTickMarkSize
      default 8;
    property DivisionTickMarkWidth: Integer read FDivisionTickMarkWidth write
      SetDivisionTickMarkWidth default 2;
    property SubDivisionTickMarkWidth
      : Integer read FSubDivisionTickMarkWidth write
      SetSubDivisionTickMarkWidth default 1;
    property DivisionFormat
      : String read FDivisionFormat write SetDivisionFormat;
    property SubDivisionFormat: String read FSubDivisionFormat write
      SetSubDivisionFormat;
    property IndentSpacing
      : Integer read FIndentSpacing write SetIndentSpacing default 20;
    property ShowDivisionValue: Boolean read FShowDivisionValue write
      SetShowDivisionValue default true;
    property ShowSubDivisionValue: Boolean read FShowSubDivisionValue write
      SetShowSubDivisionValue default true;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TAdvsmoothTimelineDivisionType = (dtFixedNumber, dtMilliSecond, dtSecond,
    dtMinute, dtHour, dtDay, dtMonth, dtYear);

  TAdvSmoothTimeLineRange = class(TPersistent)
  private
    FDoAnimateRange, FAnimateRangeFrom, FAnimateRangeTo: Boolean;
    FRangeFromAnim, FRangeToAnim: TDateTime;
    FOwner: TAdvSmoothTimeLine;
    FRangeFrom, FOldRangeFrom: TDateTime;
    FRangeTo, FOldRangeTo: TDateTime;
    FSubDivisions: Integer;
    FDivisions: Integer;
    FOnChange: TNotifyEvent;
    FAllowScrolling: Boolean;
    FMaximumRange: TDateTime;
    FMinimumRange: TDateTime;
    FAllowZooming: Boolean;
    FDivisionType: TAdvsmoothTimelineDivisionType;
    FAutomaticScrolling: Boolean;
    FAllowPartialZooming: Boolean;
    FScrollStep: Integer;
    FAutoDivisions: Boolean;
    FAutoDivisionSpacing: Integer;
    FAutoSubDivisionSpacing: Integer;
    FAutoSubDivisions: Boolean;
    procedure SetRangeFrom(const Value: TDateTime);
    procedure SetRangeTo(const Value: TDateTime);
    procedure SetDivisions(const Value: Integer);
    procedure SetSubDivisions(const Value: Integer);
    procedure SetAllowScrolling(const Value: Boolean);
    procedure SetMaximumRange(const Value: TDateTime);
    procedure SetMinimumRange(const Value: TDateTime);
    procedure SetAllowZooming(const Value: Boolean);
    procedure SetDivisionType(const Value: TAdvsmoothTimelineDivisionType);
    procedure SetAutomaticScrolling(const Value: Boolean);
    procedure SetAllowPartialZooming(const Value: Boolean);
    procedure SetScrollStep(const Value: Integer);
    procedure SetAutoDivisions(const Value: Boolean);
    procedure SetAutoDivisionSpacing(const Value: Integer);
    procedure SetAutoSubDivisions(const Value: Boolean);
    procedure SetAutoSubDivisionSpacing(const Value: Integer);
  protected
    procedure Changed;
    procedure ResetScrollStatus;
    procedure FixDivisions(dv: Integer);
  public
    constructor Create(AOwner: TAdvSmoothTimeLine);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property AutoDivisionSpacing: Integer read FAutoDivisionSpacing write SetAutoDivisionSpacing default 10;
    property AutoDivisions: Boolean read FAutoDivisions write SetAutoDivisions default False;
    property AutoSubDivisions: Boolean read FAutoSubDivisions write SetAutoSubDivisions default False;
    property AutoSubDivisionSpacing: Integer read FAutoSubDivisionSpacing write SetAutoSubDivisionSpacing default 10;
    property MinimumRange: TDateTime read FMinimumRange write SetMinimumRange;
    property MaximumRange: TDateTime read FMaximumRange write SetMaximumRange;
    property RangeTo: TDateTime read FRangeTo write SetRangeTo;
    property RangeFrom: TDateTime read FRangeFrom write SetRangeFrom;
    property AllowPartialZooming: Boolean read FAllowPartialZooming write
      SetAllowPartialZooming default true;
    property AllowScrolling
      : Boolean read FAllowScrolling write SetAllowScrolling default true;
    property ScrollStep: Integer read FScrollStep write SetScrollStep default 5;
    property AllowZooming
      : Boolean read FAllowZooming write SetAllowZooming default true;
    property AutomaticScrolling: Boolean read FAutomaticScrolling write
      SetAutomaticScrolling default true;
    property Divisions: Integer read FDivisions write SetDivisions default 2;
    property SubDivisions
      : Integer read FSubDivisions write SetSubDivisions default 5;
    property DivisionType
      : TAdvsmoothTimelineDivisionType read FDivisionType write SetDivisionType
      default dtFixedNumber;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TAdvSmoothTimeLineBarIndicatorShape = (isTriangleUp, isTriangleDown,
    isSquare, isDiamond, isCircle, isNone, isPicture);

  TAdvSmoothTimeLineBarAnnotationPosition = (apOnTop, apAtBottom);

  TAdvSmoothTimeLineBarIndicator = class(TCollectionItem)
  private
    FOwner: TAdvSmoothTimeLine;
    FPosition: TDateTime;
    FShape: TAdvSmoothTimeLineBarIndicatorShape;
    FBorderColor: TColor;
    FGradientType: TAdvGradientType;
    FOpacity: Byte;
    FOpacityTo: Byte;
    FAngle: Integer;
    FColor: TColor;
    FColorTo: TColor;
    FBorderOpacity: Byte;
    FBorderWidth: Integer;
    FPicture: TAdvGDIPPicture;
    FSize: Integer;
    FHint: String;
    FTickMarkSize: Integer;
    FShowTickMark: Boolean;
    FTickMarkWidth: Integer;
    FTickMarkColor: TColor;
    FPopupMenu: TPopupMenu;
    FFixed: Boolean;
    FAnnotation: String;
    FAnnotationColor: TColor;
    FAnnotationPosition: TAdvSmoothTimeLineBarAnnotationPosition;
    FAnnotationLineColor: TColor;
    FAnnotationOpacity: Byte;
    FAnnotationLineOpacity: Byte;
    FAnnotationImageIndex: Integer;
    FAnnotationRounded: Boolean;
    FItemObject: TObject;
    FTag: Integer;
    FAnnotationTextColor: TColor;
    procedure SetPosition(const Value: TDateTime);
    procedure SetShape(const Value: TAdvSmoothTimeLineBarIndicatorShape);
    procedure SetAngle(const Value: Integer);
    procedure SetBorderColor(const Value: TColor);
    procedure SetBorderOpacity(const Value: Byte);
    procedure SetBorderWidth(const Value: Integer);
    procedure SetColor(const Value: TColor);
    procedure SetColorTo(const Value: TColor);
    procedure SetGradientType(const Value: TAdvGradientType);
    procedure SetOpacity(const Value: Byte);
    procedure SetOpacityTo(const Value: Byte);
    procedure SetPicture(const Value: TAdvGDIPPicture);
    procedure SetSize(const Value: Integer);
    procedure SetHint(const Value: String);
    procedure SetShowTickMark(const Value: Boolean);
    procedure SetTickMarkColor(const Value: TColor);
    procedure SetTickMarkSize(const Value: Integer);
    procedure SetTickMarkWidth(const Value: Integer);
    procedure SetFixed(const Value: Boolean);
    procedure SetAnnotation(const Value: String);
    procedure SetAnnotationColor(const Value: TColor);
    procedure SetAnnotationPostion(const Value:
        TAdvSmoothTimeLineBarAnnotationPosition);
    procedure SetAnnotationLineColor(const Value: TColor);
    procedure SetAnnotationLineOpacity(const Value: Byte);
    procedure SetAnnotationOpacity(const Value: Byte);
    procedure SetAnnotationImageIndex(const Value: Integer);
    function GetPosition: TDateTime;
    procedure SetAnnotationRounded(const Value: Boolean);
    procedure SetAnnotationTextColor(const Value: TColor);
  protected
    FDBKeyValue: String;
    procedure Changed;
    procedure PictureChanged(Sender: TObject);
    function CalculateAnnotation(g: TGPGraphics; f: TGPFont;
      sf: TGPStringFormat; I: Integer): TGPRectF;
    procedure DrawAnnotation(g: TGPGraphics; f: TGPFont; sf: TGPStringFormat;
      bText: TGPSolidBrush; r: TGPRectF);
    procedure DrawAnnotationLine(g: TGPGraphics; r: TGPRectF;
      AnnotationPosition: TAdvSmoothTimeLineBarAnnotationPosition);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignVisuals(Source: TPersistent);
    function GetIndicatorRect: TGPRectF;
    function GetAnnotationRect: TGPRectF;
    property DBKeyValue: String read FDBKeyValue;
  published
    property Annotation: String read FAnnotation write SetAnnotation;
    property AnnotationColor
      : TColor read FAnnotationColor write SetAnnotationColor default clsilver;
    property AnnotationTextColor: TColor read FAnnotationTextColor write
      SetAnnotationTextColor default clNone;
    property AnnotationOpacity: Byte read FAnnotationOpacity write
      SetAnnotationOpacity default 255;
    property AnnotationLineColor: TColor read FAnnotationLineColor write
      SetAnnotationLineColor default clBlack;
    property AnnotationLineOpacity: Byte read FAnnotationLineOpacity write
      SetAnnotationLineOpacity default 255;
    property AnnotationPosition: TAdvSmoothTimeLineBarAnnotationPosition read
      FAnnotationPosition write SetAnnotationPostion default apOnTop;
    property AnnotationImageIndex: Integer read FAnnotationImageIndex write
      SetAnnotationImageIndex default - 1;
    property AnnotationRounded: Boolean read FAnnotationRounded write
      SetAnnotationRounded default false;
    property Position: TDateTime read GetPosition write SetPosition;
    property Shape: TAdvSmoothTimeLineBarIndicatorShape read FShape write
      SetShape default isDiamond;
    property Fixed: Boolean read FFixed write SetFixed default false;
    property Color: TColor read FColor write SetColor default clGray;
    property ColorTo: TColor read FColorTo write SetColorTo default clsilver;
    property Opacity: Byte read FOpacity write SetOpacity default 255;
    property OpacityTo: Byte read FOpacityTo write SetOpacityTo default 255;
    property Angle: Integer read FAngle write SetAngle default 0;
    property GradientType: TAdvGradientType read FGradientType write
      SetGradientType default gtForwardDiagonal;
    property BorderColor
      : TColor read FBorderColor write SetBorderColor default clBlack;
    property BorderOpacity
      : Byte read FBorderOpacity write SetBorderOpacity default 255;
    property BorderWidth
      : Integer read FBorderWidth write SetBorderWidth default 1;
    property Picture: TAdvGDIPPicture read FPicture write SetPicture;
    property Size: Integer read FSize write SetSize default 15;
    property Hint: String read FHint write SetHint;
    property ShowTickMark
      : Boolean read FShowTickMark write SetShowTickMark default true;
    property TickMarkSize
      : Integer read FTickMarkSize write SetTickMarkSize default 10;
    property TickMarkWidth
      : Integer read FTickMarkWidth write SetTickMarkWidth default 1;
    property TickMarkColor
      : TColor read FTickMarkColor write SetTickMarkColor default clBlack;
    property PopupMenu: TPopupMenu read FPopupMenu write FPopupMenu;
    property ItemObject: TObject read FItemObject write FItemObject;
    property Tag: Integer read FTag write FTag;
  end;

  TAdvSmoothTimeLineBarIndicators = class(TCollection)
  private
    FOwner: TAdvSmoothTimeLine;
    FOnChange: TNotifyEvent;
    function GetItem(Index: Integer): TAdvSmoothTimeLineBarIndicator;
    procedure SetItem(Index: Integer;
      const Value: TAdvSmoothTimeLineBarIndicator);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TAdvSmoothTimeLine);
    function Add: TAdvSmoothTimeLineBarIndicator;
    function Insert(Index: Integer): TAdvSmoothTimeLineBarIndicator;
    property Items[Index: Integer]
      : TAdvSmoothTimeLineBarIndicator read GetItem write SetItem; default;
    procedure Delete(Index: Integer);
  published
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TAdvSmoothTimeLineBarSection = class(TCollectionItem)
  private
    FInList: Boolean;
    FSectionRect: TGPRectF;
    FSizeHandleAnimationStarted, FSizeHandleAnimation,
      FDoSizeHandleAnimation: Boolean;
    FSizeHandleOpacity, FSizeHandleOpacityTo: Byte;
    FOwner: TAdvSmoothTimeLine;
    FStartTime: TDateTime;
    FEndTime: TDateTime;
    FFill: TGDIPFill;
    FHint: String;
    FHintFill: TGDIPFill;
    FHintFont: TFont;
    FHintAutoSize: Boolean;
    FHintWidth: Integer;
    FHintHeight: Integer;
    FHandleSize: Integer;
    FHandleColor: TColor;
    FCaptionTop: Integer;
    FCaptionLeft: Integer;
    FCaption: String;
    FCaptionLocation: TAdvSmoothTimeLineCaptionLocation;
    FFixedSize: Boolean;
    FFixedPosition: Boolean;
    FItemObject: TObject;
    FTag: Integer;
    procedure SetEndTime(const Value: TDateTime);
    procedure SetFill(const Value: TGDIPFill);
    procedure SetStartTime(const Value: TDateTime);
    procedure SetHint(const Value: String);
    procedure SetHintFill(const Value: TGDIPFill);
    procedure SetHintFont(const Value: TFont);
    procedure SetHintAutoSize(const Value: Boolean);
    procedure SetHintHeight(const Value: Integer);
    procedure SetHintWidth(const Value: Integer);
    procedure SetHandleColor(const Value: TColor);
    procedure SetHandleSize(const Value: Integer);
    procedure SetCaption(const Value: String);
    procedure SetCaptionLeft(const Value: Integer);
    procedure SetCaptionLocation
      (const Value: TAdvSmoothTimeLineCaptionLocation);
    procedure SetCaptionTop(const Value: Integer);
    procedure SetFixedPosition(const Value: Boolean);
    procedure SetFixedSize(const Value: Boolean);
  protected
    FDBKeyValue: String;
    procedure Changed;
    procedure FillChanged(Sender: TObject);
    procedure FontChanged(Sender: TObject);
    function GetSectionRect: TGPRectF;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property DBKeyValue: String read FDBKeyValue;
  published
    property Caption: String read FCaption write SetCaption;
    property CaptionLocation
      : TAdvSmoothTimeLineCaptionLocation read FCaptionLocation write
      SetCaptionLocation default cpCenterLeft;
    property CaptionLeft
      : Integer read FCaptionLeft write SetCaptionLeft default 0;
    property CaptionTop: Integer read FCaptionTop write SetCaptionTop default 0;
    property HandleSize: Integer read FHandleSize write SetHandleSize default 6;
    property HandleColor
      : TColor read FHandleColor write SetHandleColor default clHighlight;
    property FixedSize
      : Boolean read FFixedSize write SetFixedSize default false;
    property FixedPosition
      : Boolean read FFixedPosition write SetFixedPosition default false;
    property Fill: TGDIPFill read FFill write SetFill;
    property StartTime: TDateTime read FStartTime write SetStartTime;
    property EndTime: TDateTime read FEndTime write SetEndTime;
    property Hint: String read FHint write SetHint;
    property HintFill: TGDIPFill read FHintFill write SetHintFill;
    property HintFont: TFont read FHintFont write SetHintFont;
    property HintAutoSize
      : Boolean read FHintAutoSize write SetHintAutoSize default true;
    property HintWidth: Integer read FHintWidth write SetHintWidth default 50;
    property HintHeight
      : Integer read FHintHeight write SetHintHeight default 20;
    property ItemObject: TObject read FItemObject write FItemObject;
    property Tag: Integer read FTag write FTag;
  end;

  TSortSectionArray = array of TAdvSmoothTimeLineBarSection;

  TAdvSmoothTimeLineBarSections = class(TCollection)
  private
    FOwner: TAdvSmoothTimeLine;
    FOnChange: TNotifyEvent;
    function GetItem(Index: Integer): TAdvSmoothTimeLineBarSection;
    procedure SetItem(Index: Integer;
      const Value: TAdvSmoothTimeLineBarSection);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TAdvSmoothTimeLine);
    function Add: TAdvSmoothTimeLineBarSection;
    function Insert(Index: Integer): TAdvSmoothTimeLineBarSection;
    property Items[Index: Integer]
      : TAdvSmoothTimeLineBarSection read GetItem write SetItem; default;
    procedure Delete(Index: Integer);
  published
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TAnnotationAutoPosition = (apNone, apLeft, apRight);

  TAdvSmoothTimeLineBar = class(TPersistent)
  private
    FOwner: TAdvSmoothTimeLine;
    FSections: TAdvSmoothTimeLineBarSections;
    FFill: TGDIPFill;
    FHeight: Integer;
    FOnChange: TNotifyEvent;
    FAnnotationFont: TFont;
    FSectionCaptionFont: TFont;
    FAnnotationAutoPosition: TAnnotationAutoPosition;
    FOverlappingSections: Boolean;
    procedure SetFill(const Value: TGDIPFill);
    procedure SetHeight(const Value: Integer);
    procedure SetAnnotationFont(const Value: TFont);
     procedure SetSectionCaptionFont(const Value: TFont);
    procedure SetAnnotationAutoPosition(const Value: TAnnotationAutoPosition);
    procedure SetOverlappingSections(const Value: Boolean);
  protected
    procedure Changed;
    procedure FontChanged(Sender: TObject);
    procedure FillChanged(Sender: TObject);
    procedure SectionsChanged(Sender: TObject);
  public
    constructor Create(AOwner: TAdvSmoothTimeLine);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Fill: TGDIPFill read FFill write SetFill;
    property Height: Integer read FHeight write SetHeight default 10;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property AnnotationFont: TFont read FAnnotationFont write SetAnnotationFont;
    property AnnotationAutoPosition: TAnnotationAutoPosition read FAnnotationAutoPosition write
      SetAnnotationAutoPosition default apNone;
    property SectionCaptionFont: TFont read FSectionCaptionFont write SetSectionCaptionFont;
     property OverlappingSections: Boolean read FOverlappingSections write SetOverlappingSections default true;
  end;

  TAdvSmoothTimeLineDivisionText = procedure(Sender: TObject;
    DivisionValue: TDateTime; var DivisionValueString: String) of object;

  TAdvSmoothTimeLineIndicatorClick = procedure(Sender: TObject;
    indicator: TAdvSmoothTimeLineBarIndicator) of object;

  TAdvSmoothTimeLineSectionClick = procedure(Sender: TObject;
    section: TAdvSmoothTimeLineBarSection) of object;

  TAdvSmoothTimeLineIndicatorPosition = procedure(Sender: TObject;
    indicator: TAdvSmoothTimeLineBarIndicator; Position: TDateTime) of object;

  TAdvSmoothTimeLineIndicatorPositionCanChange = procedure(Sender: TObject;
    indicator: TAdvSmoothTimeLineBarIndicator; Position: TDateTime; var CanChange: Boolean) of object;

    TAdvSmoothTimeLineSectionPositionCanChange = procedure(Sender: TObject;
    section: TAdvSmoothTimeLineBarSection; StartTime, EndTime: TDateTime; var CanChange: Boolean)
     of object;

  TAdvSmoothTimeLineSectionPosition = procedure(Sender: TObject;
    section: TAdvSmoothTimeLineBarSection; StartTime, EndTime: TDateTime)
    of object;

  TAdvSmoothTimeLineIndicatorHint = procedure(Sender: TObject;
    indicator: TAdvSmoothTimeLineBarIndicator; var Hint: String) of object;

  TAdvSmoothTimeLineAnnotationItem = class(TCollectionItem)
  private
    FRectangle: TGPRectF;
    FIndicator: TAdvSmoothTimeLineBarIndicator;
  public
    property AnnotationRect: TGPRectF read FRectangle write FRectangle;
    property indicator
      : TAdvSmoothTimeLineBarIndicator read FIndicator write FIndicator;
  end;

  TAdvSmoothTimeLineAnnotationList = class(TCollection)
   private
    FOwner: TAdvSmoothTimeLine;
    procedure SetItem(Index: Integer;
      const Value: TAdvSmoothTimeLineAnnotationItem);
  protected
    function Compare(Item1, Item2: TAdvSmoothTimeLineAnnotationItem): Integer;
      virtual;
    procedure QuickSort(L, r: Integer);
  public
    constructor Create(AOwner: TAdvSmoothTimeLine);
    procedure Clear;
    function Add: TAdvSmoothTimeLineAnnotationItem;
    function Insert(Index: Integer): TAdvSmoothTimeLineAnnotationItem;
    procedure Delete(Index: Integer);
    function GetItem(index: Integer): TAdvSmoothTimeLineAnnotationItem;
    property Items[Index: Integer]
      : TAdvSmoothTimeLineAnnotationItem read GetItem write SetItem; default;
  end;

  TTimeLineValueType = (vtDivision, vtSubDivision);

   TTimeLineValue = record
    d: TDateTime;
    str: String;
    postext: TGPPointF;
    recttext: TGPRectF;
    tp: TTimeLineValueType;
    indent: integer;
  end;

  TTimeLineValueArray = array of TTimeLineValue;

  TAdvSmoothTimeLineDrawEvent = procedure(Sender: TObject; ACanvas: TCanvas;
    ARect: TRect) of object;

  TAdvSmoothTimeLineScrollEvent = procedure(Sender: TObject; CurrentRangeFrom,
    CurrentRangeTo, NextRangeFrom, NextRangeTo: TDateTime) of object;

   {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
   {$ENDIF}
  TAdvSmoothTimeLine = class(TCustomControl, ITMSStyle, ITMSTones)
  private
    FTMSStyle: TTMSStyle;
    FDrawDivisions, FDrawSubDivisions: Integer;
    FPrevHandle: Integer;
    FStartIndent: Integer;
    FOldStartVal: TTimeLineValue;
    FTotalIndent: Integer;
    FTimeLineValues: TTimeLineValueArray;
    FAllowAnimation: Boolean;
    FSp: Double;
    FStartTime, FStopTime: Integer;
    FScrollTimeLine: Boolean;
    FScrollDif: Double;
    FUpdateCount: Integer;
    FReBuildLists: Boolean;
    FAnnotationListTop: TAdvSmoothTimeLineAnnotationList;
    FAnnotationListBottom: TAdvSmoothTimeLineAnnotationList;
    FTimer: TTimer;
     FDesignTime, FMbRight: Boolean;
    FMx, FMy, FCx, FCy, FPx, FPy, FPrevPy, FPrevPx: Integer;
    FSecStartX, FSecEndX: Double;
    FMouseMovedOnIndicator, FMouseMovedOnSection,
      FMouseMovedOnTimeLine: Boolean;
    FMouseDownOnIndicator, FMouseDownOnSection, FMouseDownOnStartSection,
      FMouseDownOnEndSection, FMouseDownOnTimeLine: Boolean;
    FhintSizeCalculated: Boolean;
    Fh: TAdvSmoothSectionHint;
    FHoveredSection, FHoveredIndicator, FSelectedIndicator,
      FSelectedSection: Integer;
    FRange: TAdvSmoothTimeLineRange;
    FRangeAppearance: TAdvSmoothTimeLineRangeAppearance;
    FFill: TGDIPFill;
    FTimeLineBar: TAdvSmoothTimeLineBar;
    FTimeLineBarSections: TAdvSmoothTimeLineBarSections;
    FDefaultHintFill: TGDIPFill;
    FDefaultSectionFill: TGDIPFill;
    FVerticalMargin: Integer;
    FHorizontalMargin: Integer;
     FOnSubDivisionText: TAdvSmoothTimeLineDivisionText;
    FOnDivisionText: TAdvSmoothTimeLineDivisionText;
    FTimeLineIndicators: TAdvSmoothTimeLineBarIndicators;
    FOnIndicatorClick: TAdvSmoothTimeLineIndicatorClick;
    FOnIndicatorPositionChanged: TAdvSmoothTimeLineIndicatorPosition;
    FOnIndicatorHint: TAdvSmoothTimeLineIndicatorHint;
    FDefaultIndicators: TAdvSmoothTimeLineBarIndicators;
    FDefaultIndicator: TAdvSmoothTimeLineBarIndicator;
    FOnIndicatorPositionChanging: TAdvSmoothTimeLineIndicatorPosition;
    FOnSectionPositionChanging: TAdvSmoothTimeLineSectionPosition;
    FOnSectionPositionChanged: TAdvSmoothTimeLineSectionPosition;
    FOnSectionClick: TAdvSmoothTimeLineSectionClick;
    FImageList: TCustomImageList;
    FReadOnly: Boolean;
    FOnChange: TNotifyEvent;
    FOnDrawTimeLine: TAdvSmoothTimeLineDrawEvent;
    FOnScrollTimeLine: TAdvSmoothTimeLineScrollEvent;
    FOnIndicatorDblClick: TAdvSmoothTimeLineIndicatorClick;
    FOnIndicatorPositionCanChange: TAdvSmoothTimeLineIndicatorPositionCanChange;
    FOnSectionPositionCanChange: TAdvSmoothTimeLineSectionPositionCanChange;
     procedure CMHintShow(var Message: TMessage); message CM_HINTSHOW;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure SetRange(const Value: TAdvSmoothTimeLineRange);
    procedure SetRangeAppearance(const Value: TAdvSmoothTimeLineRangeAppearance);
    procedure SetFill(const Value: TGDIPFill);
    procedure SetTimeLineBar(const Value: TAdvSmoothTimeLineBar);
    procedure SetTimeLineBarSections(const Value: TAdvSmoothTimeLineBarSections);
    procedure SetDefaultHintFill(const Value: TGDIPFill);
    procedure SetDefaultSectionFill(const Value: TGDIPFill);
    procedure SetHorizontalMargin(const Value: Integer);
    procedure SetVerticalMargin(const Value: Integer);
    procedure SetTimeLineIndicators(const Value: TAdvSmoothTimeLineBarIndicators);
    procedure SetDefaultIndicator(const Value: TAdvSmoothTimeLineBarIndicator);
    procedure SetImageList(const Value: TCustomImageList);
    procedure SetReadOnly(const Value: Boolean);
    procedure IndicatorDblClick;
  protected
    procedure AnimateTimeLine(Sender: TObject);
     procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure Changed;
    procedure RangeChanged(Sender: TObject);
    procedure RangeAppearanceChanged(Sender: TObject);
    procedure TimeLineBarChanged(Sender: TObject);
    procedure TimeLineBarSectionsChanged(Sender: TObject);
    procedure TimeLineBarIndicatorsChanged(Sender: TObject);
    procedure FillChanged(Sender: TObject);
    procedure DrawBackground(g: TGPGraphics);
    procedure DrawTimeLineValues(g: TGPGraphics);
    procedure DrawTimeLineTickMarks(g: TGPGraphics);
    procedure DrawTimeBarIndicators(g: TGPGraphics);
    procedure DrawTimeBar(g: TGPGraphics);
    procedure DrawTimeBarSections(g: TGPGraphics);
    procedure DrawSectionHint(g: TGPGraphics);
    procedure CalculateHintSize(h: TAdvSmoothSectionHint);
    function InsideRect: TRect;
    function GetTimeLineBarRect: TGPRectF;
     function GetTimeLineRect: TGPRectF;
    function GetRange: Double;
    function SectionAtXY(X, Y: Integer): Integer;
    function SectionMoveAtXY(X, Y: Integer): Integer;
    function SectionSizeStartAtXY(X, Y: Integer): Integer;
    function SectionSizeEndAtXY(X, Y: Integer): Integer;
    function SectionAnimateHandles(X, Y: Integer): Integer;
    function IndicatorAtXY(X, Y: Integer; var OnAnnotation: Boolean; StartIndex: Integer = 0): Integer;
    function GetTotalCountIndicatorsAtXY(X, Y: Integer): Integer;
    function PtInGPRect(r: TGPRectF; pt: TPoint): Boolean;
    function GetXPosition(Value: TDateTime): Double;
    function GetValuePosition(X: Double): TDateTime;
    procedure GetListRectanglesIntersect(ASection: TAdvSmoothTimeLineBarSection; var arrVal: TSortSectionArray);
    function GetAnimatedMargin(DivisionType: TAdvsmoothTimelineDivisionType): Double;
    function GetDivisionValue(DivisionType: TAdvsmoothTimelineDivisionType): Double;
    function GetDeltaDatetime(DivisionType: TAdvsmoothTimelineDivisionType;
      Divisions: Integer; DivisionCount: Integer = 0): TDateTime;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure BuildAnnotations;
    procedure BuildTimeLineValues;
     procedure DoBuildTimeLineValues(var ADivisions: Integer; var ASubDivisions: Integer);
    procedure BuildSections(ForceRebuild: Boolean = False);
    function IsPartialZoomingAllowed(Dif: Double): Boolean;
    function IsPartialZoomingAllowedAnimated(Dif: Double): Boolean;
    function GetTotalIndent: Integer;
    procedure DoSectionPositionChanged(Sender: TObject;
      section: TAdvSmoothTimeLineBarSection; StartTime, EndTime: TDateTime);
      virtual;
    procedure DoIndicatorPositionChanged(Sender: TObject;
      indicator: TAdvSmoothTimeLineBarIndicator; Position: TDateTime); virtual;
    procedure DoIndicatorDown(Sender: TObject;
      indicator: TAdvSmoothTimeLineBarIndicator); virtual;
    procedure DoSectionDown(Sender: TObject; section: TAdvSmoothTimeLineBarSection); virtual;
    procedure DoIndicatorDblClick(indicator: TAdvSmoothTimeLineBarIndicator); virtual;
    procedure ReadTMSStyle(Reader: TReader);
    procedure WriteTMSStyle(Writer: TWriter);
    procedure DefineProperties(Filer: TFiler); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
     procedure Assign(Source: TPersistent); override;
    procedure Paint; override;
    procedure Resize; override;
    procedure Loaded; override;
    procedure SetComponentStyle(AStyle: TTMSStyle);
    function GetComponentStyle: TTMSStyle;
    procedure SetColorTones(ATones: TColorTones);
    procedure SaveToTheme(FileName: String);
    procedure LoadFromTheme(FileName: String);
    function GetThemeID: String;
    function XToDateTime(X: Double): TDateTime;
    function DateTimeToX(Value: TDateTime): Double;
    procedure BeginUpdate;
    procedure EndUpdate;
    function AnnotationUsedTopHeight: Double;
    function AnnotationUsedBottomHeight: Double;
    procedure MouseWheelHandler(var Message: TMessage); override;
    procedure SetTimeLineRange(ARangeFrom, ARangeTo: TDateTime); overload;
    procedure SetTimeLineRange(ARangeFrom, ARangeTo, AMinimumRange,
      AMaximumRange: TDateTime); overload;
     procedure DrawTimeLine(ACanvas: TCanvas);
    procedure SaveToImage(FileName: String; ImageWidth, ImageHeight: Integer;
      ImageType: TImageType = itBMP; ImageQualityPercentage: Integer = 100);
  published
    property DefaultHintFill: TGDIPFill read FDefaultHintFill write
      SetDefaultHintFill;
    property DefaultSectionFill: TGDIPFill read FDefaultSectionFill write
      SetDefaultSectionFill;
    property DefaultIndicator
      : TAdvSmoothTimeLineBarIndicator read FDefaultIndicator write
      SetDefaultIndicator;
    property Range: TAdvSmoothTimeLineRange read FRange write SetRange;
    property RangeAppearance
      : TAdvSmoothTimeLineRangeAppearance read FRangeAppearance write
      SetRangeAppearance;
    property Fill: TGDIPFill read FFill write SetFill;
    property VerticalMargin
      : Integer read FVerticalMargin write SetVerticalMargin default 10;
    property HorizontalMargin: Integer read FHorizontalMargin write
      SetHorizontalMargin default 25;
     property TimeLineBar: TAdvSmoothTimeLineBar read FTimeLineBar write
      SetTimeLineBar;
    property TimeLineSections: TAdvSmoothTimeLineBarSections read
      FTimeLineBarSections write SetTimeLineBarSections;
    property TimeLineIndicators
      : TAdvSmoothTimeLineBarIndicators read FTimeLineIndicators write
      SetTimeLineIndicators;
    property ImageList: TCustomImageList read FImageList write SetImageList;
    property OnIndicatorHint: TAdvSmoothTimeLineIndicatorHint read FOnIndicatorHint write FOnIndicatorHint;
    property OnIndicatorPositionCanChange: TAdvSmoothTimeLineIndicatorPositionCanChange read FOnIndicatorPositionCanChange write FOnIndicatorPositionCanChange;
    property OnSectionPositionCanChange: TAdvSmoothTimeLineSectionPositionCanChange read FOnSectionPositionCanChange write FOnSectionPositionCanChange;
    property OnIndicatorPositionChanged: TAdvSmoothTimeLineIndicatorPosition read FOnIndicatorPositionChanged write FOnIndicatorPositionChanged;
    property OnSectionPositionChanged: TAdvSmoothTimeLineSectionPosition read FOnSectionPositionChanged write FOnSectionPositionChanged;
    property OnIndicatorPositionChanging: TAdvSmoothTimeLineIndicatorPosition read FOnIndicatorPositionChanging write FOnIndicatorPositionChanging;
    property OnSectionPositionChanging: TAdvSmoothTimeLineSectionPosition read FOnSectionPositionChanging write FOnSectionPositionChanging;
    property OnIndicatorClick: TAdvSmoothTimeLineIndicatorClick read FOnIndicatorClick write FOnIndicatorClick;
    property OnIndicatorDblClick: TAdvSmoothTimeLineIndicatorClick read FOnIndicatorDblClick write FOnIndicatorDblClick;
    property OnSectionClick: TAdvSmoothTimeLineSectionClick read FOnSectionClick write FOnSectionClick;
    property OnDivisionText: TAdvSmoothTimeLineDivisionText read FOnDivisionText write FOnDivisionText;
    property OnSubDivisionText: TAdvSmoothTimeLineDivisionText read FOnSubDivisionText write FOnSubDivisionText;
    property OnDrawTimeLine: TAdvSmoothTimeLineDrawEvent read FOnDrawTimeLine write FOnDrawTimeLine;
    property OnScrollTimeLine: TAdvSmoothTimeLineScrollEvent read FOnScrollTimeLine write FOnScrollTimeLine;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly default false;

    property Visible;
    property ShowHint;

    property Align;
    property Anchors;
    property Constraints;
    property DragMode;
    property DragCursor;
    property DragKind;
{$IFDEF DELPHI2006_LVL}
    property OnCanResize;
    property OnConstrainedResize;
    property OnContextPopup;
{$ENDIF}
    property OnDblClick;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property PopupMenu;
    property OnGetSiteInfo;
{$IFDEF DELPHI2006_LVL}
    property OnMouseActivate;
    property OnMouseEnter;
    property OnMouseLeave;
{$ENDIF}
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseDown;
    property OnClick;
    property OnKeyDown;
    property OnKeyUp;
    property OnKeyPress;
    property Enabled;
    property TabStop default true;
{$IFDEF DELPHI_TOUCH}
    property OnGesture;
    property Touch;
{$ENDIF}
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

type
{$HINTS OFF}
  TShadowedCollection = class(TPersistent)
  private
    FItemClass: TCollectionItemClass;
    {$IFDEF DELPHIXE3_LVL}
    FItems: TList<TCollectionItem>;
    {$ELSE}
    FItems: TList;
    {$ENDIF}
  end;
{$HINTS ON}

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

procedure GetObjectLocation(var X, Y: Double; rectangle: TGPRectF;
  objectwidth, objectheight: Double;
  location: TAdvSmoothTimeLineCaptionLocation);
var
  w, h, tw, th: Double;
begin
  tw := objectwidth;
  th := objectheight;
  w := Round(rectangle.Width);
  h := Round(rectangle.Height);
  case location of
    cpTopLeft:
      begin
        X := 0;
        Y := 0;
      end;
    cpTopRight:
      begin
        X := w - tw;
        Y := 0;
      end;
    cpBottomLeft:
      begin
        X := 0;
        Y := h - th;
      end;
    cpBottomRight:
      begin
        X := w - tw;
        Y := h - th;
      end;
    cpTopCenter:
      begin
        X := (w - tw) / 2;
        Y := 0;
      end;
    cpBottomCenter:
      begin
        X := (w - tw) / 2;
        Y := h - th;
      end;
    cpCenterCenter:
      begin
        X := (w - tw) / 2;
        Y := (h - th) / 2;
      end;
    cpCenterLeft:
      begin
        X := 0;
        Y := (h - th) / 2;
      end;
    cpCenterRight:
      begin
        X := w - tw;
        Y := (h - th) / 2;
      end;
  end;

  X := X + rectangle.X;
  Y := Y + rectangle.Y;
end;

{ TAdvSmoothTimeLine }

function AnimateDouble(var Start: Single; Stop, Delta, Margin: Single): Boolean;
begin
  result := true;
  if (Start > Stop - Margin) and (Start < Stop + Margin) then
  begin
    Start := Stop;
    result := false;
  end
  else
  begin
    Delta := max(1, Delta);
    if Start < Stop then
      Start := Round(Start + Delta)
    else
      Start := Round(Start - Delta);
  end;
end;

procedure TAdvSmoothTimeLine.AnimateTimeLine(Sender: TObject);
var
  d: Double;
  Start: Single;
  I: Integer;
  rs, rt: Single;
  fr: Integer;
begin
  if ReadOnly then
    Exit;

  if FScrollTimeLine then
  begin
    BeginUpdate;
    if Assigned(OnScrollTimeLine) then
      OnScrollTimeLine(Self, Range.RangeFrom, Range.RangeTo,
        Range.RangeFrom + FScrollDif, Range.RangeTo + FScrollDif);

    Range.RangeFrom := Range.RangeFrom + FScrollDif;
    Range.RangeTo := Range.RangeTo + FScrollDif;
    EndUpdate;
  end;

  with Range do
  begin
    if FAnimateRangeFrom then
    begin
      rs := RangeFrom;

      fr := 0;
      while Frac(rs) > 0 do
      begin
        rs := rs * 10;
        FRangeFromAnim := FRangeFromAnim * 10;
        Inc(fr);
      end;

      d := Abs(rs - FRangeFromAnim) / max(1, FSp);

      FDoAnimateRange := AnimateDouble(rs, FRangeFromAnim, d, 1);
      if FDoAnimateRange then
      begin
        while fr > 0 do
        begin
          rs := rs / 10;
          FRangeFromAnim := FRangeFromAnim / 10;
          dec(fr);
        end;

        BeginUpdate;
        FOldRangeFrom := FRangeFrom;
        FRangeFrom := rs;
        EndUpdate;
      end
      else
      begin
        FAnimateRangeFrom := false;
        while fr > 0 do
        begin
          FRangeFromAnim := FRangeFromAnim / 10;
          dec(fr);
        end;
        BeginUpdate;
        FOldRangeFrom := FRangeFrom;
        FRangeFrom := FRangeFromAnim;
        EndUpdate;
      end;
    end;

    if FAnimateRangeTo then
    begin
      rt := RangeTo;
      fr := 0;
      while Frac(rt) > 0 do
      begin
        rt := rt * 10;
        FRangeToAnim := FRangeToAnim * 10;
        Inc(fr);
      end;

      d := Abs(rt - FRangeToAnim) / max(1, FSp);
      FDoAnimateRange := AnimateDouble(rt, FRangeToAnim, d, 1);
      if FDoAnimateRange then
      begin
        while fr > 0 do
        begin
          rt := rt / 10;
          FRangeToAnim := FRangeToAnim / 10;
          dec(fr);
        end;

        BeginUpdate;
        FOldRangeTo := FRangeTo;
        FRangeTo := rt;
        EndUpdate;
      end
      else
      begin
        FAnimateRangeTo := false;
        while fr > 0 do
        begin
          FRangeToAnim := FRangeToAnim / 10;
          dec(fr);
        end;
        BeginUpdate;
        FOldRangeTo := FRangeTo;
        FRangeTo := FRangeToAnim;
        EndUpdate;
      end;
    end;
  end;

  for I := 0 to TimeLineSections.Count - 1 do
  begin
    with TimeLineSections[I] do
    begin
      if FSizeHandleAnimation then
      begin
        d := Abs(FSizeHandleOpacity - FSizeHandleOpacityTo) / 5;
        Start := FSizeHandleOpacity;
        FDoSizeHandleAnimation := AnimateDouble
          (Start, FSizeHandleOpacityTo, d, 1);
        if FDoSizeHandleAnimation then
        begin
          FSizeHandleOpacity := Round(Start);
          Changed;
        end
        else
        begin
          FSizeHandleOpacity := FSizeHandleOpacityTo;
          FSizeHandleAnimation := false;
        end;
      end;
    end;
  end;
end;

function TAdvSmoothTimeLine.AnnotationUsedBottomHeight: Double;
var
  mx: Double;
  I: Integer;
begin
  Result := 0;
  mx := -1000;
  if Assigned(FAnnotationListBottom) then
  begin
    for I := 0 to FAnnotationListBottom.Count - 1 do
      mx := Max(mx, FAnnotationListBottom[I].AnnotationRect.Y + FAnnotationListBottom[I].AnnotationRect.Height);

    Result := mx - GetTimeLineBarRect.Y - GetTimeLineBarRect.Height;
  end;
end;

function TAdvSmoothTimeLine.AnnotationUsedTopHeight: Double;
var
  mn: Double;
  I: Integer;
begin
  Result := 0;
  mn := MaxDouble;
  if Assigned(FAnnotationListTop) then
  begin
    for I := 0 to FAnnotationListTop.Count - 1 do
      mn := Min(mn, FAnnotationListTop[I].AnnotationRect.Y);
    Result := GetTimeLineBarRect.Y - mn;
  end;
end;

procedure TAdvSmoothTimeLine.Assign(Source: TPersistent);
begin
  if (Source is TAdvSmoothTimeLine) then
  begin
    FRange.Assign((Source as TAdvSmoothTimeLine).Range);
    FRangeAppearance.Assign((Source as TAdvSmoothTimeLine).RangeAppearance);
    FFill.Assign((Source as TAdvSmoothTimeLine).Fill);
    FVerticalMargin := (Source as TAdvSmoothTimeLine).VerticalMargin;
    FHorizontalMargin := (Source as TAdvSmoothTimeLine).HorizontalMargin;
    FTimeLineBar.Assign((Source as TAdvSmoothTimeLine).TimeLineBar);
    FTimeLineBarSections.Assign((Source as TAdvSmoothTimeLine)
        .TimeLineSections);
    FDefaultHintFill.Assign((Source as TAdvSmoothTimeLine).FDefaultHintFill);
    FDefaultSectionFill.Assign((Source as TAdvSmoothTimeLine)
        .FDefaultSectionFill);
    FTimeLineIndicators.Assign((Source as TAdvSmoothTimeLine)
        .TimeLineIndicators);
    FDefaultIndicator.Assign((Source as TAdvSmoothTimeLine).DefaultIndicator);
    FReadOnly := (Source as TAdvSmoothTimeLine).ReadOnly;
    Changed;
  end;
end;

procedure TAdvSmoothTimeLine.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TAdvSmoothTimeLine.BuildAnnotations;
var
  ff: TGPFontFamily;
  f: TGPFont;
  fs: Integer;
  sf: TGPStringFormat;
  bmp: TBitmap;
  g: TGPGraphics;
  I: Integer;
begin
  if (csDestroying in ComponentState) or (csLoading in ComponentState) or
    (FUpdateCount > 0) then
    Exit;

  if FReBuildLists then
  begin
    FReBuildLists := false;
    FAnnotationListTop.Clear;
    FAnnotationListBottom.Clear;

    for I := 0 to TimeLineIndicators.Count - 1 do
    begin
      if ((TimeLineIndicators[I].Annotation <> '') or
          (TimeLineIndicators[I].AnnotationImageIndex <> -1)) and
        (TimeLineIndicators[I].AnnotationPosition = apOnTop) then
      begin
        FAnnotationListTop.Add.indicator := TimeLineIndicators[I];
      end;
    end;

    for I := 0 to TimeLineIndicators.Count - 1 do
    begin
      if ((TimeLineIndicators[I].Annotation <> '') or
          (TimeLineIndicators[I].AnnotationImageIndex <> -1)) and
        (TimeLineIndicators[I].AnnotationPosition = apAtBottom) then
      begin
        FAnnotationListBottom.Add.indicator := TimeLineIndicators[I];
      end;
    end;

    // Sort indicators on position
    if FAnnotationListTop.Count > 1 then
      FAnnotationListTop.QuickSort(0, pred(FAnnotationListTop.Count));

    // Sort indicators on position
    if FAnnotationListBottom.Count > 1 then
      FAnnotationListBottom.QuickSort(0, pred(FAnnotationListBottom.Count));
  end;

  bmp := TBitmap.Create;
  g := TGPGraphics.Create(bmp.Canvas.Handle);

  with TimeLineBar do
  begin
    ff := TGPFontFamily.Create(AnnotationFont.Name);
    if (ff.Status in [FontFamilyNotFound, FontStyleNotFound]) then
    begin
      ff.Free;
      ff := TGPFontFamily.Create('Arial');
    end;

    fs := 0;
    if (fsBold in AnnotationFont.Style) then
      fs := fs + 1;
    if (fsItalic in AnnotationFont.Style) then
      fs := fs + 2;
    if (fsUnderline in AnnotationFont.Style) then
      fs := fs + 4;

    sf := TGPStringFormat.Create;
    f := TGPFont.Create(ff, AnnotationFont.Size, fs, UnitPoint);
  end;

  for I := 0 to FAnnotationListTop.Count - 1 do
    with FAnnotationListTop.GetItem(I) do
      AnnotationRect := indicator.CalculateAnnotation(g, f, sf, I);

  for I := 0 to FAnnotationListBottom.Count - 1 do
    with FAnnotationListBottom.GetItem(I) do
      AnnotationRect := indicator.CalculateAnnotation(g, f, sf, I);

  ff.Free;
  f.Free;
  sf.Free;
  g.Free;
  bmp.Free;
end;

procedure TAdvSmoothTimeLine.BuildSections(ForceRebuild: Boolean = False);
var
  I: Integer;
  r: TGPRectF;
  xs, xe: Double;
  arrval: TSortSectionArray;
  K: Integer;
begin
  if csDestroying in ComponentState then
    Exit;
  for I := 0 to TimeLineSections.Count - 1 do
  begin
    with TimeLineSections[i] do
    begin
      r := GetTimeLineBarRect;
      if FMouseDownOnStartSection and (Index = FSelectedSection) and not ForceRebuild then
      begin
        xs := min(max(FSecStartX, r.X), r.X + r.Width);
        xe := min(max(DateTimeToX(EndTime), r.X), r.X + r.Width);
      end
      else if FOwner.FMouseDownOnEndSection and (Index = FOwner.FSelectedSection)  and not ForceRebuild
        then
      begin
        xe := min(max(FSecEndX, r.X), r.X + r.Width);
        xs := min(max(DateTimeToX(StartTime), r.X), r.X + r.Width);
      end
      else if FOwner.FMouseMovedOnSection and FOwner.FMouseDownOnSection and
        (Index = FOwner.FSelectedSection) and not ForceRebuild then
      begin
        xs := min(max(FSecStartX, r.X), r.X + r.Width);
        xe := min(max(FSecEndX, r.X), r.X + r.Width);
      end
      else
      begin
        xs := min(max(DateTimeToX(StartTime), r.X), r.X + r.Width);
        xe := min(max(DateTimeToX(EndTime), r.X), r.X + r.Width);
      end;

      FSectionRect := MakeRect(xs, r.Y, xe - xs, r.Height);
    end;
  end;


  if TimeLineBar.OverlappingSections then
    Exit;

  for I := 0 to TimeLineSections.Count - 1 do
  begin
    TimeLineSections[i].FInList := False;
  end;

  for I := 0 to TimeLineSections.Count - 1 do
  begin
    GetListRectanglesIntersect(TimeLineSections[I], arrval);
    for K := 0 to Length(arrval) - 1 do
    begin
      arrval[K].FSectionRect.Y := arrval[K].FSectionRect.Y + (arrval[K].FSectionRect.Height / Length(arrval) * K);
      arrval[K].FSectionRect.Height := arrval[K].FSectionRect.Height / Length(arrval);
    end;

    SetLength(arrval, 0);
  end;
end;


procedure TAdvSmoothTimeLine.BuildTimeLineValues;
var
  dv, dvsub: Integer;
begin
  dv := Range.Divisions;
  dvsub := Range.SubDivisions;
  DoBuildTimeLineValues(dv, dvsub);
  FDrawDivisions := dv;
  FDrawSubDivisions := dvsub;
end;

procedure TAdvSmoothTimeLine.DoBuildTimeLineValues(var ADivisions: Integer; var ASubDivisions: Integer);
var
  X, xsub, w, Y, ysub: Double;
  I, J, K, ind: Integer;
  wdiv: Double;
  wsubdiv: Double;
  ff, ffsub: TGPFontFamily;
  fs: Integer;
  sf, sfsub: TGPStringFormat;
  f, fsub: TGPFont;
  v: TDateTime;
  s: String;
  Layr, sizer: TGPRectF;
  overlap: Boolean;
  totalw: Double;
  timeliner: TGPRectF;
  d, ddiv: TDateTime;
  AllowDraw: Boolean;
  g: TGPGraphics;
  bmp: TBitmap;
  dv, dvsub: Integer;
  val1, val2: TTimeLineValue;
  doval1, doval2: Boolean;
  DoAutoSize: Boolean;

  procedure DoSwap(a, b: Integer);
  var
    h: TTimeLineValue;
  begin
    h := FTimeLineValues[a];
    FTimeLineValues[a] := FTimeLineValues[b];
    FTimeLineValues[b] := h;
  end;

  function DoCompare(a, b: Integer): Integer;
  begin
    Result := CompareDateTime(FTimeLineValues[a].d, FTimeLineValues[b].d);
  end;

  procedure Sort(b, e: Integer);
  var
    I: Integer;
    s: Boolean;
  begin
    s := false;
    while not s do
    begin
      s := true;
      for I := b to e - 1 do
        if DoCompare(I, I + 1) > 0 then
        begin
          DoSwap(I, I + 1);
          s := false;
        end;
    end;
  end;

begin
  if not Assigned(Range) or not Assigned(RangeAppearance) then
    Exit;

  timeliner := GetTimeLineRect;
  w := timeliner.Width;

  if not(w > 10) or not(Range.RangeTo - Range.RangeFrom > 0) then
    Exit;

  if Length(FTimeLineValues) > 0 then
    FOldStartVal := FTimeLineValues[0];


  SetLength(FTimeLineValues, 0);
  bmp := TBitmap.Create;
  g := TGPGraphics.Create(bmp.Canvas.Handle);

  // Division font
  ff := TGPFontFamily.Create(FRangeAppearance.DivisionFont.Name);
  if (ff.Status in [FontFamilyNotFound, FontStyleNotFound]) then
  begin
    ff.Free;
    ff := TGPFontFamily.Create('Arial');
  end;

  fs := 0;
  if (fsBold in FRangeAppearance.DivisionFont.Style) then
    fs := fs + 1;
  if (fsItalic in FRangeAppearance.DivisionFont.Style) then
    fs := fs + 2;
  if (fsUnderline in FRangeAppearance.DivisionFont.Style) then
    fs := fs + 4;

  sf := TGPStringFormat.Create;
  f := TGPFont.Create(ff, FRangeAppearance.DivisionFont.Size, fs, UnitPoint);
  //

  // Sub division font
  ffsub := TGPFontFamily.Create(FRangeAppearance.SubDivisionFont.Name);
  if (ffsub.Status in [FontFamilyNotFound, FontStyleNotFound]) then
  begin
    ffsub.Free;
    ffsub := TGPFontFamily.Create('Arial');
  end;

  fs := 0;
  if (fsBold in FRangeAppearance.SubDivisionFont.Style) then
    fs := fs + 1;
  if (fsItalic in FRangeAppearance.SubDivisionFont.Style) then
    fs := fs + 2;
  if (fsUnderline in FRangeAppearance.SubDivisionFont.Style) then
    fs := fs + 4;

  sfsub := TGPStringFormat.Create;
  fsub := TGPFont.Create(ffsub, FRangeAppearance.SubDivisionFont.Size, fs,
    UnitPoint);
  //
  Layr := MakeRect(0, 0, 10000, 10000);

  X := timeliner.X;
  Y := timeliner.Y + 2 + RangeAppearance.DivisionTickMarkSize;
  ysub := timeliner.Y + 2 + RangeAppearance.SubDivisionTickMarkSize;

  if (Range.DivisionType = dtFixedNumber) then
  begin
    if (ADivisions > 0) then
    begin
      wdiv := w / ADivisions;
      wsubdiv := 0;
      if ASubDivisions > 0 then
        wsubdiv := wdiv / ASubDivisions;

      K := 1;
      for I := 0 to ADivisions do
      begin
        if RangeAppearance.ShowDivisionValue then
        begin
          if ADivisions * I = 0 then
            v := Range.RangeFrom
          else
            v := Range.RangeFrom + (GetRange / ADivisions * I);

          s := formatDatetime(FRangeAppearance.DivisionFormat, v);
          if Assigned(FOnDivisionText) then
            FOnDivisionText(Self, v, s);
          g.MeasureString(s, Length(s), f, Layr, sf, sizer);
          SetLength(FTimeLineValues, Length(FTimeLineValues) + 1);
          FTimeLineValues[Length(FTimeLineValues) - 1].d := v;
          FTimeLineValues[Length(FTimeLineValues) - 1].str := s;
          FTimeLineValues[Length(FTimeLineValues) - 1].postext := MakePoint
            (X - sizer.Width / 2, Y);
          FTimeLineValues[Length(FTimeLineValues) - 1].recttext := sizer;
          FTimeLineValues[Length(FTimeLineValues) - 1].tp := vtDivision;
        end;

        if (I < ADivisions) and (ASubDivisions > 0) then
        begin
          for J := 1 to ASubDivisions - 1 do
          begin
            if RangeAppearance.ShowSubDivisionValue then
            begin
              if (ASubDivisions * ADivisions) = 0 then
                v := Range.RangeFrom
              else
                v := Range.RangeFrom +
                  (GetRange / (ADivisions * ASubDivisions)) * K;

              s := formatDatetime(FRangeAppearance.SubDivisionFormat, v);
              if Assigned(FOnSubDivisionText) then
                FOnSubDivisionText(Self, v, s);

              g.MeasureString(s, Length(s), fsub, Layr, sf, sizer);
              SetLength(FTimeLineValues, Length(FTimeLineValues) + 1);
              FTimeLineValues[Length(FTimeLineValues) - 1].d := v;
              FTimeLineValues[Length(FTimeLineValues) - 1].str := s;
              FTimeLineValues[Length(FTimeLineValues) - 1].postext := MakePoint
                (X + (wsubdiv * J) - sizer.Width / 2, ysub);
              FTimeLineValues[Length(FTimeLineValues) - 1].recttext := sizer;
              FTimeLineValues[Length(FTimeLineValues) - 1].tp := vtSubDivision;

              Inc(K);
            end;
          end;
          Inc(K);
        end;
        X := X + wdiv;
      end;
    end;
  end
  else
  begin
    dv := ADivisions;
    dvsub := ASubDivisions;
    if (dv > 0) and (RangeAppearance.ShowDivisionValue) then
    begin
      // first value
      d := GetDeltaDatetime(Range.DivisionType, dv);
      X := DateTimeToX(d);

      // next values
      I := 0;
      while not(CompareDateTime(d, Range.RangeTo) = 1) {and (d > 0)} do
      begin
        if not(CompareDateTime(d, Range.RangeFrom) = -1) then
        begin
          s := formatDatetime(FRangeAppearance.DivisionFormat, d);
          if Assigned(FOnDivisionText) then
            FOnDivisionText(Self, d, s);
          g.MeasureString(s, Length(s), f, Layr, sf, sizer);
          SetLength(FTimeLineValues, Length(FTimeLineValues) + 1);
          FTimeLineValues[Length(FTimeLineValues) - 1].d := d;
          FTimeLineValues[Length(FTimeLineValues) - 1].str := s;
          FTimeLineValues[Length(FTimeLineValues) - 1].postext := MakePoint
            (X - sizer.Width / 2, Y);
          FTimeLineValues[Length(FTimeLineValues) - 1].recttext := sizer;
          FTimeLineValues[Length(FTimeLineValues) - 1].tp := vtDivision;
        end;

        Inc(I);
        d := GetDeltaDatetime(Range.DivisionType, dv, I);
        X := DateTimeToX(d);

        if CompareDateTime(d, Range.RangeTo) = 1 then
          Break;
      end;
    end;
    if (dvsub > 0) and RangeAppearance.ShowSubDivisionValue then
    begin
      // first value
      d := GetDeltaDatetime(Range.DivisionType, dvsub);
      X := DateTimeToX(d);

      // next values
      I := 0;
      while not (CompareDateTime(d, Range.RangeTo) = 1) {and (d > 0)} do
      begin
        AllowDraw := true;
        K := 0;
        ddiv := GetDeltaDatetime(Range.DivisionType, dv);
        while not(CompareDateTime(ddiv, Range.RangeTo) = 1) {and (ddiv > 0)} do
        begin
          if CompareDateTime(d, ddiv) = 0 then
          begin
            AllowDraw := false;
            Break;
          end;
          Inc(K);
          ddiv := GetDeltaDatetime(Range.DivisionType, dv, K);
        end;

        if AllowDraw then
        begin
          if not (CompareDateTime(d, Range.RangeFrom) = -1) then
          begin
            s := formatDatetime(FRangeAppearance.SubDivisionFormat, d);
            if Assigned(FOnDivisionText) then
              FOnDivisionText(Self, d, s);
            g.MeasureString(s, Length(s), fsub, Layr, sf, sizer);
            SetLength(FTimeLineValues, Length(FTimeLineValues) + 1);
            FTimeLineValues[Length(FTimeLineValues) - 1].d := d;
            FTimeLineValues[Length(FTimeLineValues) - 1].str := s;
            FTimeLineValues[Length(FTimeLineValues) - 1].postext := MakePoint
              (X - sizer.Width / 2, ysub);
            FTimeLineValues[Length(FTimeLineValues) - 1].recttext := sizer;
            FTimeLineValues[Length(FTimeLineValues) - 1].tp := vtSubDivision;
          end;
        end;

        Inc(I);
        d := GetDeltaDatetime(Range.DivisionType, dvsub, I);
        X := DateTimeToX(d);
        if CompareDateTime(d, Range.RangeTo) = 1 then
          Break;
      end;
    end;
  end;

  FTotalIndent := 0;
  ind := 0;
  totalw := 0;
  overlap := true;

  while overlap do
  begin
    for I := 0 to Length(FTimeLineValues) - 1 do
    begin
      with FTimeLineValues[I] do
      begin
        if ind = 0 then
          totalw := totalw + recttext.Width + 5;

        if ind < FTotalIndent then
          Inc(ind)
        else
          ind := 0;
      end;
    end;

    if totalw > w then
    begin
      Inc(FTotalIndent);
      totalw := 0;
    end
    else
      overlap := false;
  end;

  Sort(0, Length(FTimeLineValues) - 1);

  if not (csLoading in ComponentState) and not (csDesigning in ComponentState) then
  begin
    if Length(FTimeLineValues) > 0 then
    begin
      if CompareDateTime(FOldStartVal.d, FTimeLineValues[0].d) = -1 then
      begin
        Inc(FStartIndent);
        if FStartIndent > FTotalIndent then
          FStartIndent := 0;
      end
      else if CompareDateTime(FOldStartVal.d, FtimelineValues[0].d) = 1 then
      begin
        Dec(FStartIndent);
        if FStartIndent < 0 then
          FStartIndent := FTotalIndent;
      end;
    end;
  end;

  ind := Fstartindent;
  for I := 0 to Length(FTimeLineValues) - 1 do
  begin
    with FTimeLineValues[i] do
    begin
      indent := ind;
      if ind < FTotalIndent then
        Inc(ind)
      else
        ind := 0;
    end;
  end;

  ff.Free;
  ffsub.Free;
  f.Free;
  fsub.Free;
  sf.Free;
  sfsub.Free;
  g.Free;
  bmp.Free;

  DoAutoSize := False;
  if Range.AutoDivisions and (ADivisions > 1) then
  begin
    if Length(FTimeLineValues) > 0 then
    begin
      doval1 := False;
      doval2 := False;
      for I := 0 to Length(FTimeLineValues) - 1 do
      begin
        if FTimeLineValues[I].tp = vtDivision then
        begin
          if not doval1 then
          begin
            val1 := FTimeLineValues[I];
            doval1 := True;
          end
          else if not doval2 then
          begin
            val2 := FTimeLineValues[I];
            doval2 := True;
          end;
        end;

        if doval1 and doval2 then
          Break;
      end;

      if doval1 and doval2 then
      begin
        if val1.postext.X + val1.recttext.Width + Range.AutoDivisionSpacing > val2.postext.X then
        begin
          ADivisions := ADivisions - 1;
          DoAutoSize := True;
        end;
      end;
    end;
  end;

  //sub divisions
  if Range.AutoSubDivisions and (ASubDivisions > 1) then
  begin
    if Length(FTimeLineValues) > 0 then
    begin
      doval1 := False;
      doval2 := False;
      for I := 0 to Length(FTimeLineValues) - 1 do
      begin
        if FTimeLineValues[I].tp = vtSubDivision then
        begin
          if not doval1 then
          begin
            val1 := FTimeLineValues[I];
            doval1 := True;
          end
          else if not doval2 then
          begin
            val2 := FTimeLineValues[I];
            doval2 := True;
          end;
        end;

        if doval1 and doval2 then
          Break;
      end;

      if doval1 and doval2 then
      begin
        if val1.postext.X + val1.recttext.Width + Range.AutoSubDivisionSpacing > val2.postext.X then
        begin
          ASubDivisions := ASubDivisions - 1;
          DoAutoSize := True;
        end;
      end;
    end;
  end;


  if DoAutoSize then
    DoBuildTimeLineValues(ADivisions, ASubDivisions);
end;

procedure TAdvSmoothTimeLine.CalculateHintSize(h: TAdvSmoothSectionHint);
var
  ff: TGPFontFamily;
  f: TGPFont;
  fs: Integer;
  sf: TGPStringFormat;
  fillr, Layr, sizer: TGPRectF;
  b: TGPSolidBrush;
  X, Y: Integer;
  g: TGPGraphics;
begin
  if (FHoveredSection >= 0) and (FHoveredSection <= TimeLineSections.Count - 1)
    then
  begin
    with TimeLineSections[FHoveredSection] do
    begin
      g := TGPGraphics.Create(Canvas.Handle);

      ff := TGPFontFamily.Create(HintFont.Name);
      if (ff.Status in [FontFamilyNotFound, FontStyleNotFound]) then
      begin
        ff.Free;
        ff := TGPFontFamily.Create('Arial');
      end;

      fs := 0;
      if (fsBold in HintFont.Style) then
        fs := fs + 1;
      if (fsItalic in HintFont.Style) then
        fs := fs + 2;
      if (fsUnderline in HintFont.Style) then
        fs := fs + 4;

      sf := TGPStringFormat.Create;
      if HintAutoSize then
        Layr := MakeRect(0, 0, 10000, 10000)
      else
        Layr := MakeRect(0, 0, HintWidth, HintHeight);

      f := TGPFont.Create(ff, HintFont.Size, fs, UnitPoint);
      b := TGPSolidBrush.Create(MakeColor(255, HintFont.Color));
      g.MeasureString(Hint, Length(Hint), f, Layr, sf, sizer);

      X := 0;
      Y := 0;

      if HintAutoSize then
        fillr := MakeRect(X, Y, sizer.Width + 5, sizer.Height + 5)
      else
        fillr := MakeRect(X, Y, Layr.Width + 5, Layr.Height + 5);

      Fh.Width := Round(fillr.Width);
      Fh.Height := Round(fillr.Height);

      if not HintFill.Picture.Empty then
      begin
        HintFill.Picture.GetImageSizes;
        Fh.Width := max(Fh.Width, HintFill.Picture.Width);
        Fh.Height := max(Fh.Height, HintFill.Picture.Height);
      end;

      Fh.Width := Fh.Width + 1;
      Fh.Height := Fh.Height + 1;

      b.Free;
      f.Free;
      ff.Free;
      sf.Free;
      g.Free;
    end;
  end;
end;

procedure TAdvSmoothTimeLine.Changed;
begin
  if FUpdateCount = 0 then
    invalidate;

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TAdvSmoothTimeLine.CMHintShow(var Message: TMessage);
var
  Hint: String;
  id: Integer;
  pt: TPoint;
  a: Boolean;
begin
  with TCMHintShow(Message).HintInfo^ do
  begin
    pt := CursorPos;
    id := IndicatorAtXY(pt.X, pt.Y, a);
    if id <> -1 then
    begin
      Hint := TimeLineIndicators[id].Hint;
      if Assigned(OnIndicatorHint) then
        OnIndicatorHint(Self, TimeLineIndicators[id], Hint);
      HintStr := Hint;
      ReshowTimeout := 0;
    end;
  end;
end;

procedure TAdvSmoothTimeLine.CMMouseLeave(var Message: TMessage);
var
  I: Integer;
begin
  inherited;
  FHoveredSection := -1;
  FMouseMovedOnIndicator := false;
  FMouseDownOnIndicator := false;
  FMouseDownOnSection := false;
  for I := 0 to TimeLineSections.Count - 1 do
  begin
    with TimeLineSections[I] do
    begin
      if FSizeHandleAnimationStarted then
      begin
        FSizeHandleOpacityTo := 0;
        FSizeHandleAnimationStarted := false;
        FSizeHandleAnimation := true;
      end;
    end;
  end;

  if Fh.Visible then
  begin
    FhintSizeCalculated := false;
    Fh.Hide;
  end;

  Application.CancelHint;
  Changed;
end;

constructor TAdvSmoothTimeLine.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csAcceptsControls];
  DoubleBuffered := true;
  FAnnotationListTop := TAdvSmoothTimeLineAnnotationList.Create(Self);
  FAnnotationListBottom := TAdvSmoothTimeLineAnnotationList.Create(Self);
  FRange := TAdvSmoothTimeLineRange.Create(Self);
  FRange.OnChange := RangeChanged;
  FRangeAppearance := TAdvSmoothTimeLineRangeAppearance.Create(Self);
  FRangeAppearance.OnChange := RangeAppearanceChanged;
  FFill := TGDIPFill.Create;
  FFill.OnChange := FillChanged;
  FVerticalMargin := 10;
  FHorizontalMargin := 25;
  Width := 500;
  Height := 80;
  FTimeLineBar := TAdvSmoothTimeLineBar.Create(Self);
  FTimeLineBar.OnChange := TimeLineBarChanged;
  FTimeLineBarSections := TAdvSmoothTimeLineBarSections.Create(Self);
  FTimeLineBarSections.OnChange := TimeLineBarSectionsChanged;
  FDefaultHintFill := TGDIPFill.Create;
  FDefaultSectionFill := TGDIPFill.Create;
  FTimeLineIndicators := TAdvSmoothTimeLineBarIndicators.Create(Self);
  FTimeLineIndicators.OnChange := TimeLineBarIndicatorsChanged;
  FHoveredSection := -1;
  FHoveredIndicator := -1;
  FSelectedIndicator := -1;
  FSelectedSection := -1;
  FDefaultIndicators := TAdvSmoothTimeLineBarIndicators.Create(Self);
  FDefaultIndicator := FDefaultIndicators.Add;
  Fh := TAdvSmoothSectionHint.Create(Application);
  Fh.Visible := false;
  Fh.Parent := Self;
  Fh.TimeLine := Self;
  FTimer := TTimer.Create(Self);
  FTimer.Enabled := true;
  FTimer.Interval := 50;
  FTimer.OnTimer := AnimateTimeLine;
  FReadOnly := false;
  FPrevHandle := -1;

  TabStop := true;

  FDesignTime := (csDesigning in ComponentState) and not
    ((csReading in Owner.ComponentState) or (csLoading in Owner.ComponentState)
    );

  if FDesignTime then
    SetComponentStyle(tsOffice2007Luna);
end;

function TAdvSmoothTimeLine.DateTimeToX(Value: TDateTime): Double;
begin
  result := GetXPosition(Value);
end;

destructor TAdvSmoothTimeLine.Destroy;
begin
  FAnnotationListTop.Free;
  FAnnotationListBottom.Free;
  FDefaultIndicator.Free;
  FDefaultIndicator := nil;
  FDefaultIndicators.Free;
  FRange.Free;
  FRangeAppearance.Free;
  FFill.Free;
  FTimeLineBar.Free;
  FTimeLineBarSections.Free;
  FDefaultHintFill.Free;
  FDefaultSectionFill.Free;
  FTimeLineIndicators.Free;
  inherited;
end;

procedure TAdvSmoothTimeLine.DoIndicatorDblClick(indicator: TAdvSmoothTimeLineBarIndicator);
begin
  if Assigned(FOnIndicatorDblClick) then
    FOnIndicatorDblClick(Self, indicator);
end;

procedure TAdvSmoothTimeLine.DoIndicatorDown(Sender: TObject;
  indicator: TAdvSmoothTimeLineBarIndicator);
begin
  //
end;

procedure TAdvSmoothTimeLine.DoIndicatorPositionChanged
  (Sender: TObject; indicator: TAdvSmoothTimeLineBarIndicator;
  Position: TDateTime);
begin
  if Assigned(FOnIndicatorPositionChanged) then
    FOnIndicatorPositionChanged(Sender, indicator, Position);

  indicator.Position := Position;
end;

procedure TAdvSmoothTimeLine.DoSectionDown(Sender: TObject;
  section: TAdvSmoothTimeLineBarSection);
begin
  //
end;

procedure TAdvSmoothTimeLine.DoSectionPositionChanged(Sender: TObject;
  section: TAdvSmoothTimeLineBarSection; StartTime, EndTime: TDateTime);
begin
  if Assigned(FOnSectionPositionChanged) then
    FOnSectionPositionChanged(Sender, section, StartTime, EndTime);

  BuildSections(True);
end;

procedure TAdvSmoothTimeLine.DrawBackground(g: TGPGraphics);
begin
  FFill.Fill(g, MakeRect(0, 0, Width - 1, Height - 1));
end;

procedure TAdvSmoothTimeLine.DrawSectionHint(g: TGPGraphics);
var
  ff: TGPFontFamily;
  f: TGPFont;
  fs: Integer;
  sf: TGPStringFormat;
  fillr, Layr, sizer: TGPRectF;
  b: TGPSolidBrush;
  X, Y: Integer;
  bmp: TGPBitmap;
  bmpg: TGPGraphics;
begin
  if (FHoveredSection >= 0) and (FHoveredSection <= TimeLineSections.Count - 1)
    then
  begin
    with TimeLineSections[FHoveredSection] do
    begin
      if (Hint = '') and HintFill.Picture.Empty then
        Exit;

      ff := TGPFontFamily.Create(HintFont.Name);
      if (ff.Status in [FontFamilyNotFound, FontStyleNotFound]) then
      begin
        ff.Free;
        ff := TGPFontFamily.Create('Arial');
      end;

      fs := 0;
      if (fsBold in HintFont.Style) then
        fs := fs + 1;
      if (fsItalic in HintFont.Style) then
        fs := fs + 2;
      if (fsUnderline in HintFont.Style) then
        fs := fs + 4;

      sf := TGPStringFormat.Create;
      if HintAutoSize then
        Layr := MakeRect(0, 0, 10000, 10000)
      else
        Layr := MakeRect(0, 0, HintWidth, HintHeight);

      f := TGPFont.Create(ff, HintFont.Size, fs, UnitPoint);
      b := TGPSolidBrush.Create(MakeColor(255, HintFont.Color));
      g.MeasureString(Hint, Length(Hint), f, Layr, sf, sizer);

      X := 0;
      Y := 0;

      if HintAutoSize then
        fillr := MakeRect(X, Y, sizer.Width + 5, sizer.Height + 5)
      else
        fillr := MakeRect(X, Y, Layr.Width + 5, Layr.Height + 5);

      if not HintFill.Picture.Empty then
      begin
        HintFill.Picture.GetImageSizes;
        fillr.Width := max(fillr.Width, HintFill.Picture.Width);
        fillr.Height := max(fillr.Height, HintFill.Picture.Height);
      end;

      bmp := TGPBitmap.Create(Width, Height);
      bmpg := g.FromImage(bmp);
      bmpg.SetTextRenderingHint(TextRenderingHintAntiAlias);

      FHintFill.Fill(bmpg, fillr);

      bmpg.DrawString(Hint, Length(Hint), f, MakeRect
          (fillr.X + (fillr.Width - sizer.Width) / 2, fillr.Y +
            (fillr.Height - sizer.Height) / 2, sizer.Width, sizer.Height), sf,
        b);

      bmpg.Free;
      g.DrawImage(bmp, 0, 0);
      bmp.Free;

      b.Free;
      f.Free;
      ff.Free;
      sf.Free;
    end;
  end;
end;

procedure TAdvSmoothTimeLine.DrawTimeBar(g: TGPGraphics);
begin
  TimeLineBar.Fill.Fill(g, GetTimeLineBarRect);
end;

procedure TAdvSmoothTimeLine.DrawTimeBarIndicators(g: TGPGraphics);
type
  TPointArray = array of TGPPointF;
var
  I: Integer;
  fp: TGDIPFillParameters;
  path: TGPGraphicsPath;
  pa: TPointArray;
  s: Integer;
  p: TGPPen;
  ff: TGPFontFamily;
  f: TGPFont;
  fs: Integer;
  sf: TGPStringFormat;
  bText: TGPSolidBrush;
  bdifftext: TGPSolidBrush;
begin
  with TimeLineBar do
  begin
    ff := TGPFontFamily.Create(AnnotationFont.Name);
    if (ff.Status in [FontFamilyNotFound, FontStyleNotFound]) then
    begin
      ff.Free;
      ff := TGPFontFamily.Create('Arial');
    end;

    fs := 0;
    if (fsBold in AnnotationFont.Style) then
      fs := fs + 1;
    if (fsItalic in AnnotationFont.Style) then
      fs := fs + 2;
    if (fsUnderline in AnnotationFont.Style) then
      fs := fs + 4;

    sf := TGPStringFormat.Create;
    sf.SetLineAlignment(StringAlignmentCenter);
    f := TGPFont.Create(ff, AnnotationFont.Size, fs, UnitPoint);
    bText := TGPSolidBrush.Create(ColorToARGB(AnnotationFont.Color));
  end;

  for I := 0 to FAnnotationListTop.Count - 1 do
    with FAnnotationListTop.GetItem(I) do
      indicator.DrawAnnotationLine(g, AnnotationRect,
        indicator.AnnotationPosition);

  for I := 0 to FAnnotationListBottom.Count - 1 do
    with FAnnotationListBottom.GetItem(I) do
      indicator.DrawAnnotationLine(g, AnnotationRect,
        indicator.AnnotationPosition);

  for I := 0 to FAnnotationListTop.Count - 1 do
  begin
    with FAnnotationListTop.GetItem(I) do
    begin
      if indicator.AnnotationTextColor <> clNone then
      begin
        bdifftext := TGPSolidBrush.Create
          (ColorToARGB(indicator.AnnotationTextColor));
        indicator.DrawAnnotation(g, f, sf, bdifftext, AnnotationRect);
        bdifftext.Free;
      end
      else
        indicator.DrawAnnotation(g, f, sf, bText, AnnotationRect);
    end;
  end;

  for I := 0 to FAnnotationListBottom.Count - 1 do
  begin
    with FAnnotationListBottom.GetItem(I) do
    begin
      if indicator.AnnotationTextColor <> clNone then
      begin
        bdifftext := TGPSolidBrush.Create
          (ColorToARGB(indicator.AnnotationTextColor));
        indicator.DrawAnnotation(g, f, sf, bdifftext, AnnotationRect);
        bdifftext.Free;
      end
      else
        indicator.DrawAnnotation(g, f, sf, bText, AnnotationRect);
    end;
  end;

  for I := 0 to TimeLineIndicators.Count - 1 do
  begin
    with TimeLineIndicators[I] do
    begin
      if (Position >= Range.RangeFrom) and (Position <= Range.RangeTo) then
      begin
        fp.r := GetIndicatorRect;
        s := Size;
        fp.Graphics := g;
        fp.ColorFrom := Color;
        fp.ColorTo := ColorTo;
        fp.GT := GradientType;
        fp.OpacityFrom := Opacity;
        fp.OpacityTo := OpacityTo;
        fp.Angle := Angle;
        fp.BorderColor := clNone;
        fp.BorderWidth := 0;
        fp.path := nil;
        fp.Mirror := false;

        case Shape of
          isNone:
            ; // draw nothing
          isCircle:
            begin
              path := TGPGraphicsPath.Create;
              path.AddEllipse(fp.r);
              path.CloseFigure;

              fp.path := path;
              fp.Fillpath := true;
              FillGDIP(fp);

              if (BorderColor <> clNone) and (BorderWidth > 0) then
              begin
                p := TGPPen.Create(MakeColor(BorderOpacity, BorderColor),
                  BorderWidth);
                g.DrawPath(p, path);
                p.Free;
              end;

              path.Free;
            end;
          isSquare:
            begin
              fp.Fillpath := false;
              FillGDIP(fp);
              if (BorderColor <> clNone) and (BorderWidth > 0) then
              begin
                p := TGPPen.Create(MakeColor(BorderOpacity, BorderColor),
                  BorderWidth);
                g.DrawRectangle(p, fp.r);
                p.Free;
              end;
            end;
          isDiamond:
            begin
              SetLength(pa, 4); // DIAMOND 4 POINTS
              pa[0] := MakePoint(fp.r.X, fp.r.Y + s / 2);
              pa[1] := MakePoint(fp.r.X + s / 2, fp.r.Y);
              pa[2] := MakePoint(fp.r.X + s, fp.r.Y + s / 2);
              pa[3] := MakePoint(fp.r.X + s / 2, fp.r.Y + s);

              path := TGPGraphicsPath.Create;
              path.AddPolygon(PGPPointF(pa), 4);
              path.CloseFigure;

              fp.path := path;
              fp.Fillpath := true;
              FillGDIP(fp);
              if (BorderColor <> clNone) and (BorderWidth > 0) then
              begin
                p := TGPPen.Create(MakeColor(BorderOpacity, BorderColor),
                  BorderWidth);
                g.DrawPath(p, path);
                p.Free;
              end;

              path.Free;
            end;
          isTriangleDown:
            begin
              SetLength(pa, 3); // TRIANGLE 3 POINTS
              pa[0] := MakePoint(fp.r.X + s / 2, fp.r.Y + s);
              pa[1] := MakePoint(fp.r.X, fp.r.Y);
              pa[2] := MakePoint(fp.r.X + s, fp.r.Y);

              path := TGPGraphicsPath.Create;
              path.AddPolygon(PGPPointF(pa), 3);
              path.CloseFigure;

              fp.path := path;
              fp.Fillpath := true;
              FillGDIP(fp);
              if (BorderColor <> clNone) and (BorderWidth > 0) then
              begin
                p := TGPPen.Create(MakeColor(BorderOpacity, BorderColor),
                  BorderWidth);
                g.DrawPath(p, path);
                p.Free;
              end;
              path.Free;
            end;
          isTriangleUp:
            begin
              SetLength(pa, 3); // TRIANGLE 3 POINTS
              pa[0] := MakePoint(fp.r.X + s / 2, fp.r.Y);
              pa[1] := MakePoint(fp.r.X, fp.r.Y + s);
              pa[2] := MakePoint(fp.r.X + s, fp.r.Y + s);

              path := TGPGraphicsPath.Create;
              path.AddPolygon(PGPPointF(pa), 3);
              path.CloseFigure;

              fp.path := path;
              fp.Fillpath := true;
              FillGDIP(fp);
              if (BorderColor <> clNone) and (BorderWidth > 0) then
              begin
                p := TGPPen.Create(MakeColor(BorderOpacity, BorderColor),
                  BorderWidth);
                g.DrawPath(p, path);
                p.Free;
              end;
              path.Free;
            end;
          isPicture:
            begin
              if not Picture.Empty then
                Picture.GDIPDraw(g, Bounds(Round(fp.r.X), Round(fp.r.Y), s, s));
            end;
        end;

        if ShowTickMark and (TickMarkColor <> clNone) then
        begin
          p := TGPPen.Create(MakeColor(255, TickMarkColor), TickMarkWidth);
          g.DrawLine(p, fp.r.X + fp.r.Width / 2, fp.r.Y + fp.r.Height,
            fp.r.X + fp.r.Width / 2, fp.r.Y + fp.r.Height + TickMarkSize);
          p.Free;
        end;
      end;
    end;
  end;

  bText.Free;
  f.Free;
  sf.Free;
  ff.Free;
end;

procedure TAdvSmoothTimeLine.DrawTimeBarSections(g: TGPGraphics);
var
  I: Integer;
  rl, rr, r: TGPRectF;
  p: TGPPen;
  bl: TGPLinearGradientBrush;
  bs: TGPSolidBrush;
  opc: Byte;
  s: Double;
  ff: TGPFontFamily;
  fs: Integer;
  f: TGPFont;
  sf: TGPStringFormat;
  b: TGPSolidBrush;
  sri: TGPRectF;
  fillr: TGPRectF;
  X, Y: Double;
  timeliner: TGPRectF;
  flv, frv: Boolean;
begin
  timeliner := GetTimeLineBarRect;
  with TimeLineBar do
  begin
    ff := TGPFontFamily.Create(SectionCaptionFont.Name);
    if (ff.Status in [FontFamilyNotFound, FontStyleNotFound]) then
    begin
      ff.Free;
      ff := TGPFontFamily.Create('Arial');
    end;

    fs := 0;
    if (fsBold in SectionCaptionFont.Style) then
      fs := fs + 1;
    if (fsItalic in SectionCaptionFont.Style) then
      fs := fs + 2;
    if (fsUnderline in SectionCaptionFont.Style) then
      fs := fs + 4;

    sf := TGPStringFormat.Create;
    sf.SetLineAlignment(StringAlignmentCenter);
    f := TGPFont.Create(ff, SectionCaptionFont.Size, fs, UnitPoint);
    b := TGPSolidBrush.Create(ColorToARGB(SectionCaptionFont.Color));

  end;

  for I := 0 to TimeLineSections.Count - 1 do
  begin
    with TimeLineSections[I] do
    begin
      r := GetSectionRect;

      if (r.Width > 0) and (r.X + r.Width >= timeliner.X) and
        (r.X + r.Width <= timeliner.X + timeliner.Width + 0.1) then
      begin
        FFill.Fill(g, r);

        if Caption <> '' then
        begin
          fillr := MakeRect(r.X + 2, r.Y + 2, r.Width - 4, r.Height - 4);
          g.MeasureString(Caption, Length(Caption), f, fillr, sf, sri);

          if CaptionLocation <> cpCustom then
            GetObjectLocation(X, Y, fillr, sri.Width, sri.Height,
              CaptionLocation)
          else
          begin
            X := CaptionLeft;
            Y := CaptionTop;
          end;

          g.DrawString(Caption, Length(Caption), f, MakeRect
              (X, Y, sri.Width, sri.Height), sf, b);
        end;

        // Draw handles
        opc := FSizeHandleOpacity;
        flv := StartTime >= Range.RangeFrom;
        frv := EndTime <= Range.RangeTo;
        if (opc <> 0) and (flv or frv) then
        begin
          s := FHandleSize;
          rl := MakeRect(r.X - (s / 2), r.Y + (r.Height - s) / 2, s, s);
          rr := MakeRect(r.X + r.Width - (s / 2), r.Y + (r.Height - s) / 2, s,
            s);
          bs := TGPSolidBrush.Create(MakeColor(opc, FHandleColor));
          if flv then
            g.FillEllipse(bs, rl);

          if frv then
            g.FillEllipse(bs, rr);
          bs.Free;
          bl := TGPLinearGradientBrush.Create
            (MakeRect(rl.X - 1, rl.Y - 1, rl.Width + 2, rl.Height + 2),
            MakeColor(min(150, opc), clWhite), MakeColor(min(10, opc), clWhite)
              , LinearGradientModeVertical);

          if flv then
            g.FillEllipse(bl, rl);

          if frv then
            g.FillEllipse(bl, rr);
          p := TGPPen.Create(MakeColor(opc, clBlack));

          if flv then
            g.DrawEllipse(p, rl);
          if frv then
            g.DrawEllipse(p, rr);
          p.Free;
        end;
      end;
    end;
  end;

  b.Free;
  f.Free;
  sf.Free;
  ff.Free;
end;

procedure TAdvSmoothTimeLine.DrawTimeLine(ACanvas: TCanvas);
var
  g: TGPGraphics;
begin
  g := TGPGraphics.Create(ACanvas.Handle);
  g.SetSmoothingMode(SmoothingModeAntiAlias);
  g.SetTextRenderingHint(TextRenderingHintClearTypeGridFit);
  DrawBackground(g);
  DrawTimeBar(g);
  DrawTimeLineTickMarks(g);
  DrawTimeLineValues(g);
  DrawTimeBarSections(g);
  DrawTimeBarIndicators(g);
  g.Free;
end;

procedure TAdvSmoothTimeLine.DrawTimeLineTickMarks(g: TGPGraphics);
var
  X, w, Y: Double;
  pdiv, psubdiv: TGPPen;
  I, J, K: Integer;
  wdiv: Double;
  wsubdiv: Double;
  timeliner: TGPRectF;
  d, ddiv: TDateTime;
  AllowDraw: Boolean;
  dv, dvsub: Integer;
begin
  timeliner := GetTimeLineRect;
  w := timeliner.Width;
  if (FDrawDivisions = 0) or not(w > 10) or not
    (Range.RangeTo - Range.RangeFrom > 0) then
    Exit;

  Y := GetTimeLineRect.Y + 2;

  pdiv := TGPPen.Create(MakeColor(255, RangeAppearance.DivisionTickMarkColor),
    RangeAppearance.DivisionTickMarkWidth);
  psubdiv := TGPPen.Create(MakeColor(255,
      RangeAppearance.SubDivisionTickMarkColor),
    RangeAppearance.SubDivisionTickMarkWidth);

  if (Range.DivisionType = dtFixedNumber) then
  begin
    if FDrawDivisions > 0 then
    begin
      wdiv := w / FDrawDivisions;
      wsubdiv := 0;
      if FDrawSubDivisions > 0 then
        wsubdiv := wdiv / FDrawSubDivisions;

      X := timeliner.X;

      for I := 0 to FDrawDivisions do
      begin
        g.DrawLine(pdiv, X, Y, X, Y + RangeAppearance.DivisionTickMarkSize);
        if (I < FDrawDivisions) and (FDrawSubDivisions > 0) then
        begin
          for J := 1 to FDrawSubDivisions - 1 do
          begin
            g.DrawLine(psubdiv, X + (wsubdiv * J), Y, X + (wsubdiv * J),
              Y + RangeAppearance.SubDivisionTickMarkSize);
          end;
        end;
        X := X + wdiv;
      end;
    end;
  end
  else
  begin
    dv := FDrawDivisions;
    dvsub := FDrawSubDivisions;
    if (dv > 0) then
    begin
      // first value
      d := GetDeltaDatetime(Range.DivisionType, dv);
      X := DateTimeToX(d);

      // next values
      I := 0;
      while not(CompareDateTime(d, Range.RangeTo) = 1) {and (d > 0)} do
      begin
        if not(CompareDateTime(d, Range.RangeFrom) = -1) then
          g.DrawLine(pdiv, X, Y, X, Y + RangeAppearance.DivisionTickMarkSize);

        Inc(I);
        d := GetDeltaDatetime(Range.DivisionType, dv, I);
        X := DateTimeToX(d);
        if CompareDateTime(d, Range.RangeTo) = 1 then
          Break;
      end;
    end;

    if (dvsub > 0) then
    begin
      // first value
      d := GetDeltaDatetime(Range.DivisionType, dvsub);
      X := DateTimeToX(d);

      // next values
      I := 0;
      while not(CompareDateTime(d, Range.RangeTo) = 1) {and (d > 0)} do
      begin
        AllowDraw := true;
        K := 0;
        ddiv := GetDeltaDatetime(Range.DivisionType, dv);
        while not(CompareDateTime(ddiv, Range.RangeTo) = 1) {and (ddiv > 0)} do
        begin
          if CompareDateTime(d, ddiv) = 0 then
          begin
            AllowDraw := false;
            Break;
          end;
          Inc(K);
          ddiv := GetDeltaDatetime(Range.DivisionType, dv, K);
          if CompareDateTime(ddiv, Range.RangeTo) = 1 then
            Break;
        end;

        if AllowDraw then
        begin
          if not(CompareDateTime(d, Range.RangeFrom) = -1) then
            g.DrawLine(psubdiv, X, Y, X,
              Y + RangeAppearance.SubDivisionTickMarkSize);
        end;

        Inc(I);
        d := GetDeltaDatetime(Range.DivisionType, dvsub, I);
        X := DateTimeToX(d);
        if CompareDateTime(d, Range.RangeTo) = 1 then
          Break;
      end;
    end;
  end;

  pdiv.Free;
  psubdiv.Free;
end;

procedure TAdvSmoothTimeLine.DrawTimeLineValues(g: TGPGraphics);
var
  I: Integer;
  ff, ffsub: TGPFontFamily;
  fs: Integer;
  sf, sfsub: TGPStringFormat;
  f, fsub: TGPFont;
  b, bsub: TGPSolidBrush;
  pt: TGPPointF;
begin
  // Division font
  ff := TGPFontFamily.Create(FRangeAppearance.DivisionFont.Name);
  if (ff.Status in [FontFamilyNotFound, FontStyleNotFound]) then
  begin
    ff.Free;
    ff := TGPFontFamily.Create('Arial');
  end;

  fs := 0;
  if (fsBold in FRangeAppearance.DivisionFont.Style) then
    fs := fs + 1;
  if (fsItalic in FRangeAppearance.DivisionFont.Style) then
    fs := fs + 2;
  if (fsUnderline in FRangeAppearance.DivisionFont.Style) then
    fs := fs + 4;

  sf := TGPStringFormat.Create;
  f := TGPFont.Create(ff, FRangeAppearance.DivisionFont.Size, fs, UnitPoint);
  b := TGPSolidBrush.Create(MakeColor(255, FRangeAppearance.DivisionFont.Color)
    );
  //

  // Sub division font
  ffsub := TGPFontFamily.Create(FRangeAppearance.SubDivisionFont.Name);
  if (ffsub.Status in [FontFamilyNotFound, FontStyleNotFound]) then
  begin
    ffsub.Free;
    ffsub := TGPFontFamily.Create('Arial');
  end;

  fs := 0;
  if (fsBold in FRangeAppearance.SubDivisionFont.Style) then
    fs := fs + 1;
  if (fsItalic in FRangeAppearance.SubDivisionFont.Style) then
    fs := fs + 2;
  if (fsUnderline in FRangeAppearance.SubDivisionFont.Style) then
    fs := fs + 4;

  sfsub := TGPStringFormat.Create;
  fsub := TGPFont.Create(ffsub, FRangeAppearance.SubDivisionFont.Size, fs,
    UnitPoint);
  bsub := TGPSolidBrush.Create
    (MakeColor(255, FRangeAppearance.SubDivisionFont.Color));

  for I := 0 to Length(FTimeLineValues) - 1 do
  begin
    with FTimeLineValues[I] do
    begin
      pt := postext;
      pt.Y := pt.Y + (RangeAppearance.IndentSpacing * indent);
      case tp of
        vtDivision:
          begin
            if Assigned(FOnDivisionText) then
              FOnDivisionText(Self, d, str);
            g.DrawString(str, Length(str), f, pt, sf, b);
          end;
        vtSubDivision:
          begin
            if Assigned(FOnSubDivisionText) then
              FOnSubDivisionText(Self, d, str);
            g.DrawString(str, Length(str), fsub, pt, sfsub, bsub);
          end;
      end;
    end;
  end;

  ff.Free;
  ffsub.Free;
  f.Free;
  fsub.Free;
  sf.Free;
  sfsub.Free;
  b.Free;
  bsub.Free;
end;

procedure TAdvSmoothTimeLine.EndUpdate;
begin
  dec(FUpdateCount);
  if FUpdateCount = 0 then
  begin
    FReBuildLists := true;
    BuildTimeLineValues;
    BuildAnnotations;
    BuildSections;
    Changed;
  end;
end;

procedure TAdvSmoothTimeLine.FillChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothTimeLineRange.FixDivisions(dv: Integer);
var
  r: Double;
  rt: TGPRectF;
begin
  if (DivisionType <> dtFixedNumber) then
  begin
    if dv > 0 then
    begin
      rt := FOwner.GetTimeLineBarRect;
      r := FOwner.GetRange;
      while ((r / (dv * FOwner.GetDivisionValue(DivisionType))) > rt.Width) do
      begin
        Inc(FDivisionType);
        if FDivisionType = dtFixedNumber then
          Inc(FDivisionType)
      end;
    end;
  end;
end;

function TAdvSmoothTimeLine.GetXPosition(Value: TDateTime): Double;
var
  r: TGPRectF;
  w: Double;
  rf: TDateTime;
begin
  rf := Range.RangeFrom;
  r := GetTimeLineBarRect;
  w := r.Width;
  if GetRange > 0 then
    result := r.X + (w / GetRange) * (Value - rf)
  else
    result := 0;
end;

function TAdvSmoothTimeLine.GetAnimatedMargin
  (DivisionType: TAdvsmoothTimelineDivisionType): Double;
begin
  result := 0;
  case DivisionType of
    dtMilliSecond:
      result := 1 / 24 / 60 / 60 / 1000;
    dtSecond:
      result := 1 / 24 / 60 / 60;
    dtMinute:
      result := 1 / 24 / 60;
    dtHour:
      result := 1 / 24;
    dtDay:
      result := 1;
    dtMonth:
      result := 1;
    dtYear:
      result := 1;
  end;
end;

function TAdvSmoothTimeLine.GetComponentStyle: TTMSStyle;
begin
  Result := FTMSStyle;
end;

function TAdvSmoothTimeLine.GetDeltaDatetime
  (DivisionType: TAdvsmoothTimelineDivisionType; Divisions: Integer;
  DivisionCount: Integer = 0): TDateTime;
var
  Y, m, d, h, n, s, z: Word;
  dim: Integer;
begin
  result := Range.RangeFrom;
  DecodeDateTime(Range.RangeFrom, Y, m, d, h, n, s, z);

  case DivisionType of
    dtMilliSecond:
      begin
        z := Abs((z - ((z mod Divisions) + Divisions))) mod 1000;
        z := max(0, min(z, 1000));
        result := EncodeDateTime(Y, m, d, h, n, s, z);
      end;
    dtSecond:
      begin
        s := Abs((s - ((s mod Divisions) + Divisions))) mod 60;
        s := max(0, min(s, 59));
        result := EncodeDateTime(Y, m, d, h, n, s, 0);
      end;
    dtMinute:
      begin
        n := Abs((n - ((n mod Divisions) + Divisions))) mod 60;
        n := max(0, min(n, 59));
        result := EncodeDateTime(Y, m, d, h, n, 0, 0);
      end;
    dtHour:
      begin
        h := Abs((h - ((h mod Divisions) + Divisions))) mod 24;
        h := max(0, min(h, 23));
        result := EncodeDateTime(Y, m, d, h, 0, 0, 0);
      end;
    dtDay:
    begin
      dim := DaysInMonth(EncodeDateTime(Y, m, 1, h, n, s, z));
      d := Abs((d - ((d mod Divisions) + Divisions))) mod dim;
      d := Max(1, min(d, dim));
      result := encodeDateTime(y, m, d, 0, 0, 0, 0);
    end;
    dtMonth:
      begin
        m := 1 + Abs((m - ((m mod Divisions) + Divisions))) mod 12;
        result := EncodeDate(Y, m, 1);
      end;
    dtYear:
      begin
        Y := Abs(Y - ((Y mod Divisions) + Divisions));
        result := EncodeDate(Y, 1, 1)
      end;
  end;

  if DivisionCount > 0 then
  begin
    case DivisionType of
      dtMilliSecond:
        result := IncMilliSecond(result, Divisions * DivisionCount);
      dtSecond:
        result := IncSecond(result, Divisions * DivisionCount);
      dtMinute:
        result := IncMinute(result, Divisions * DivisionCount);
      dtHour:
        result := IncHour(result, Divisions * DivisionCount);
      dtDay:
        result := IncDay(result, Divisions * DivisionCount);
      dtMonth:
        result := IncMonth(result, Divisions * DivisionCount);
      dtYear:
        result := IncYear(result, Divisions * DivisionCount);
    end;
  end;
end;

function TAdvSmoothTimeLine.GetDivisionValue
  (DivisionType: TAdvsmoothTimelineDivisionType): Double;
begin
  result := 0;
  case DivisionType of
    dtMilliSecond:
      result := 1 / 24 / 60 / 60 / 1000;
    dtSecond:
      result := 1 / 24 / 60 / 60;
    dtMinute:
      result := 1 / 24 / 60;
    dtHour:
      result := 1 / 24;
    dtDay:
      result := 1;
    dtMonth:
      result := 30;
    dtYear:
      result := 365;
  end;
end;

procedure TAdvSmoothTimeLine.GetListRectanglesIntersect(
  ASection: TAdvSmoothTimeLineBarSection;
  var arrVal: TSortSectionArray);
var
  I: Integer;
begin
  for I := 0 to TimeLineSections.Count - 1 do
  begin
    if not TimeLineSections[i].FInList then
    begin
      if ASection <> TimeLineSections[I] then
      begin
        if RectanglesInterSect(ASection.FSectionRect, TimeLineSections[i].FSectionRect) then
        begin
          if not ASection.FInList then
          begin
            SetLength(arrVal, Length(arrVal) + 1);
            arrVal[Length(arrVal)-1] := ASection;
            ASection.FInList := True;
          end;
          SetLength(arrVal, Length(arrVal) + 1);
          arrVal[Length(arrVal)-1] := TimeLineSections[i];
          TimeLineSections[i].FInList := True;
        end;
      end;
    end;
  end;
end;

function TAdvSmoothTimeLine.GetRange: Double;
begin
  result := FRange.RangeTo - FRange.RangeFrom;
end;

function TAdvSmoothTimeLine.GetThemeID: String;
begin
  result := ClassName;
end;

function TAdvSmoothTimeLine.GetTimeLineBarRect: TGPRectF;
var
  r: TRect;
begin
  r := InsideRect;
  result := MakeRect(r.Left + HorizontalMargin, r.Top + VerticalMargin,
    r.Right - r.Left - (HorizontalMargin * 2), TimeLineBar.Height);
end;

function TAdvSmoothTimeLine.GetTimeLineRect: TGPRectF;
var
  r: TRect;
begin
  r := InsideRect;
  result := MakeRect(r.Left + HorizontalMargin,
    r.Top + VerticalMargin + TimeLineBar.Height, r.Right - r.Left -
      (HorizontalMargin * 2), r.Bottom - r.Top - TimeLineBar.Height -
      (VerticalMargin * 2));
end;

function TAdvSmoothTimeLine.GetTotalCountIndicatorsAtXY(X, Y: Integer): Integer;
var
  I: Integer;
begin
  result := 0;
  for I := 0 to TimeLineIndicators.Count - 1 do
  begin
    if PtInGPRect(TimeLineIndicators[I].GetIndicatorRect, Point(X, Y))
      and not TimeLineIndicators[I].Fixed then
    begin
      Inc(result);
    end;
  end;
end;

function TAdvSmoothTimeLine.GetTotalIndent: Integer;
begin
  result := FTotalIndent;
end;

function TAdvSmoothTimeLine.GetValuePosition(X: Double): TDateTime;
var
  r: TGPRectF;
  w: Double;
begin
  r := GetTimeLineBarRect;
  w := r.Width;
  result := Range.RangeFrom;
  if GetRange > 0 then
  begin
    if w / GetRange > 0 then
    begin
      result := (X - r.X) / (w / GetRange) + Range.RangeFrom;
      result := min(max(Range.RangeFrom, result), Range.RangeTo);
    end
  end
end;

function TAdvSmoothTimeLine.IndicatorAtXY
  (X, Y: Integer; var OnAnnotation: Boolean; StartIndex: Integer = 0): Integer;
var
  I: Integer;
begin
  result := -1;
  OnAnnotation := false;
  for I := TimeLineIndicators.Count - 1 downto StartIndex do
  begin
    if PtInGPRect(TimeLineIndicators[I].GetIndicatorRect, Point(X, Y)) then
    begin
      result := I;
      Break;
    end;
  end;

  if result = -1 then
  begin
    for I := 0 to FAnnotationListTop.Count - 1 do
    begin
      if PtInGPRect(FAnnotationListTop[I].AnnotationRect, Point(X, Y)) then
      begin
        if Assigned(FAnnotationListTop[I].indicator) then
          result := FAnnotationListTop[I].indicator.Index;

        OnAnnotation := true;
        Break;
      end;
    end;
  end;

  if result = -1 then
  begin
    for I := 0 to FAnnotationListBottom.Count - 1 do
    begin
      if PtInGPRect(FAnnotationListBottom[I].AnnotationRect, Point(X, Y)) then
      begin
        if Assigned(FAnnotationListBottom[I].indicator) then
          result := FAnnotationListBottom[I].indicator.Index;

        OnAnnotation := true;
        Break;
      end;
    end;
  end;
end;

procedure TAdvSmoothTimeLine.IndicatorDblClick;
begin
  if FMouseDownOnIndicator then
  begin
    if not FMouseMovedOnIndicator then
    begin
      if (FSelectedIndicator >= 0) and (FSelectedIndicator <= TimeLineIndicators.Count - 1) then
      begin
        if not FMbRight and not FMouseMovedOnTimeLine then
          DoIndicatorDblClick(TimeLineIndicators[FSelectedIndicator]);
      end;
    end;
  end;
end;

function TAdvSmoothTimeLine.InsideRect: TRect;
var
  bw: Integer;
begin
  result := Bounds(0, 0, Width, Height);
  // adapt width & height for GDI+ drawing rect
  result.Right := result.Right - 1;
  result.Bottom := result.Bottom - 1;

  if (Fill.BorderColor <> clNone) then
  begin
    if Fill.BorderWidth = 1 then
      bw := 1
    else
      bw := (Fill.BorderWidth + 1) div 2;

    InflateRect(result, -bw, -bw);
  end;

  if (Fill.ShadowOffset <> 0) and (Fill.ShadowColor <> clNone) then
  begin
    result.Right := result.Right - Fill.ShadowOffset;
    result.Bottom := result.Bottom - Fill.ShadowOffset;
  end;

  result.Left := result.Left + 2;
  result.Right := result.Right - 2;
  result.Top := result.Top + 2;
  result.Bottom := result.Bottom - 2;
end;

function TAdvSmoothTimeLine.IsPartialZoomingAllowed(Dif: Double): Boolean;
begin
  result := true;
  if not Range.AllowPartialZooming then
  begin
    if Range.RangeFrom = Range.MinimumRange then
      result := Range.RangeFrom - Dif > Range.MinimumRange;

    if Range.RangeTo = Range.MaximumRange then
      result := Range.RangeTo - Dif < Range.MaximumRange;
  end;
end;

function TAdvSmoothTimeLine.IsPartialZoomingAllowedAnimated(Dif: Double)
  : Boolean;
begin
  result := true;
  if not Range.AllowPartialZooming then
  begin
    if Range.FRangeFromAnim = Range.MinimumRange then
      result := Range.FRangeFromAnim - Dif > Range.MinimumRange;

    if Range.FRangeToAnim = Range.MaximumRange then
      result := Range.FRangeToAnim - Dif < Range.MaximumRange;
  end;
end;

procedure TAdvSmoothTimeLine.Loaded;
begin
  inherited;
  FReBuildLists := true;
  BuildTimeLineValues;
  BuildAnnotations;
  BuildSections;
end;

procedure TAdvSmoothTimeLine.LoadFromTheme(FileName: String);
begin

end;

procedure TAdvSmoothTimeLine.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  ssize, s, I, ss, se: Integer;
  startx, endx: Double;
  a: Boolean;
begin
  inherited;

  if ReadOnly then
    Exit;

  if TabStop then
    SetFocus;

  Range.ResetScrollStatus;
  FAllowAnimation := not(ssCtrl in Shift);
  FMbRight := (Button = mbRight);
  FCx := X;
  FCy := Y;
  FMx := X;
  FMy := Y;
  ss := SectionSizeStartAtXY(X, Y);
  se := SectionSizeEndAtXY(X, Y);
  ssize := SectionAnimateHandles(X, Y);
  FStartTime := GetTickCount;
  if ((ss <> -1) or (se <> -1)) and (ssize <> -1) then
  begin
    FMouseDownOnStartSection := ss <> -1;
    FMouseDownOnEndSection := se <> -1;
    FSelectedSection := ssize;
    startx := DateTimeToX(TimeLineSections[FSelectedSection].StartTime);
    FSecStartX := startx;
    endx := DateTimeToX(TimeLineSections[FSelectedSection].EndTime);
    FSecEndX := endx;
  end
  else
  begin
    s := SectionMoveAtXY(X, Y);
    if s <> -1 then
    begin
      FMouseDownOnSection := true;
      FSelectedSection := s;
    end;

    I := IndicatorAtXY(X, Y, a);
    if (I <> -1) then
    begin
      FMouseDownOnIndicator := true;
      FSelectedIndicator := I;
      if TimeLineIndicators[I].Fixed and not a then
      begin
        if GetTotalCountIndicatorsAtXY(X, Y) > 0 then
        begin
          // get next moveable indicator
          Inc(I);
          while (I <> -1) and not a and (I >= 0) and
            (I <= TimeLineIndicators.Count - 1) and
            (TimeLineIndicators[I].Fixed) do
          begin
            Inc(I);
          end;
          FSelectedIndicator := I;
        end
      end;
    end;

    if FMouseDownOnSection and FMouseDownOnIndicator then
    begin
      // Indicator on same position as Section
      FMouseDownOnSection := false;
    end;

    if (not FMouseDownOnIndicator) and (not FMouseDownOnSection) then
      FMouseDownOnTimeLine := PtInGPRect
        (MakeRect(0, 0, Width, Height), Point(X, Y));

    if FMouseDownOnSection then
    begin
      if (FSelectedSection >= 0) and
        (FSelectedSection <= TimeLineSections.Count - 1) then
        DoSectionDown(Self, TimeLineSections[FSelectedSection]);
    end;

    if FMouseDownOnIndicator then
    begin
      if (FSelectedIndicator >= 0) and
        (FSelectedIndicator <= TimeLineIndicators.Count - 1) then
        DoIndicatorDown(Self, TimeLineIndicators[FSelectedIndicator]);
    end;

  end;
end;

procedure TAdvSmoothTimeLine.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  sc, id: Integer;
  pt: TPoint;
  r: TGPRectF;
  xs: Double;
  v, vstart, vend: TDateTime;
  startx, stopx: Double;
  s: Integer;
  I: Integer;
  MovedPixels: Integer;
  Dif: Double;
  a: Boolean;
  secl, secr: Integer;
  CanChange: Boolean;
  delta: Single;
  tr: TGPRectF;

begin
  inherited;

  if not ReadOnly then
  begin
    secl := SectionSizeStartAtXY(X, Y);
    secr := SectionSizeEndAtXY(X, Y);
    if (secl <> -1) or (secr <> -1) then
      Cursor := crSizeWE
    else
      Cursor := crDefault;
  end;

  if FMouseDownOnStartSection and not ReadOnly then
  begin
    if (FSelectedSection >= 0) and
      (FSelectedSection <= TimeLineSections.Count - 1) then
    begin
      startx := DateTimeToX(TimeLineSections[FSelectedSection].StartTime);
      FSecStartX := startx + X - FCx;
      vstart := XToDateTime(FSecStartX);
      vend := TimeLineSections[FSelectedSection].EndTime;
      if Assigned(FOnSectionPositionChanging) then
        FOnSectionPositionChanging(Self, TimeLineSections[FSelectedSection],
          vstart, vend);
      BuildSections;
      Changed;
    end;
  end
  else if FMouseDownOnEndSection and not ReadOnly then
  begin
    if (FSelectedSection >= 0) and
      (FSelectedSection <= TimeLineSections.Count - 1) then
    begin
      stopx := DateTimeToX(TimeLineSections[FSelectedSection].EndTime);
      FSecEndX := stopx + X - FCx;
      vstart := TimeLineSections[FSelectedSection].StartTime;
      vend := XToDateTime(FSecEndX);
      if Assigned(FOnSectionPositionChanging) then
        FOnSectionPositionChanging(Self, TimeLineSections[FSelectedSection],
          vstart, vend);
      BuildSections;
      Changed;
    end;
  end
  else
  begin
    if not ReadOnly then
    begin
      s := SectionAnimateHandles(X, Y);
      if FPrevHandle <> s then
      begin
        for I := 0 to TimeLineSections.Count - 1 do
        begin
          with TimeLineSections[I] do
          begin
            if FSizeHandleAnimationStarted then
            begin
              FSizeHandleOpacityTo := 0;
              FSizeHandleAnimationStarted := false;
              FSizeHandleAnimation := true;
            end;
          end;
        end;
      end;
      FPrevHandle := s;

      if s <> -1 then
      begin
        with TimeLineSections[s] do
        begin
          FSizeHandleOpacityTo := 255;
          FSizeHandleAnimation := true;
          FSizeHandleAnimationStarted := true;
        end;
      end;
    end;

    id := IndicatorAtXY(X, Y, a);
    if(id <> FHoveredIndicator) then
    begin
      FHoveredIndicator := id;
      Application.CancelHint;
    end;

    if FMbRight then
      Exit;

    if FMouseMovedOnIndicator and FMouseDownOnIndicator and not ReadOnly then
    begin
      if (FSelectedIndicator >= 0) and
        (FSelectedIndicator <= TimeLineIndicators.Count - 1) then
      begin
        if not TimeLineIndicators[FSelectedIndicator].Fixed then
        begin
          FPrevPx := FPx;
          FPrevPy := FPy;
          FPx := X;
          FPy := Y;
          r := TimeLineIndicators[FSelectedIndicator].GetIndicatorRect;
          xs := r.X + r.Width / 2;
          v := XToDateTime(xs);

          CanChange := True;

          if Assigned(OnIndicatorPositionCanChange) then
            OnIndicatorPositionCanChange(Self, TimeLineIndicators[FSelectedIndicator], v, CanChange);

          if not CanChange then
          begin
            FMouseMovedOnIndicator := False;
            FMouseDownOnIndicator := False;
          end;

//          if not CanChange then
//          begin
//            if FMouseDownOnIndicator then
//            begin
//              if FMouseMovedOnIndicator then
//              begin
//                if (FSelectedIndicator >= 0) and
//                  (FSelectedIndicator <= TimeLineIndicators.Count - 1) then
//                begin
//                  if not TimeLineIndicators[FSelectedIndicator].Fixed then
//                  begin
//                    r := TimeLineIndicators[FSelectedIndicator].GetIndicatorRect;
//                    xs := r.X + r.Width / 2;
//                    v := XToDateTime(xs);
//                    FMouseMovedOnIndicator := false;
//                    TimeLineIndicators[FSelectedIndicator].Position := v;
//                    FReBuildLists := true;
//                    BuildTimeLineValues;
//                    BuildAnnotations;
//                    BuildSections;
//                    DoIndicatorPositionChanged
//                      (Self, TimeLineIndicators[FSelectedIndicator],
//                      TimeLineIndicators[FSelectedIndicator].Position);
//                  end;
//                end;
//              end;
//            end;
//            FMouseMovedOnIndicator := False;
//            FMouseDownOnIndicator := False;
//          end;

          if (Range.AllowScrolling and Range.AutomaticScrolling) and
            ((FPx > xs) or (FPx < xs)) then
          begin
            if GetTimeLineBarRect.Width > 0 then
            begin
              FScrollTimeLine := true;
              FScrollDif := (Range.RangeTo - Range.RangeFrom)
                / GetTimeLineBarRect.Width;
              FScrollDif := FScrollDif * (X - xs);
            end;
          end
          else
          begin
            FScrollTimeLine := false;
            FScrollDif := 0;
            FReBuildLists := true;
            BuildTimeLineValues;
            BuildAnnotations;
            BuildSections;
          end;
          if CanChange and Assigned(FOnIndicatorPositionChanging) then
            FOnIndicatorPositionChanging(Self,
              TimeLineIndicators[FSelectedIndicator], v);
        end;
      end;
      Changed;
    end
    else if FMouseDownOnSection and FMouseMovedOnSection and not ReadOnly then
    begin
      if (FSelectedSection >= 0) and
        (FSelectedSection <= TimeLineSections.Count - 1) then
      begin
        startx := DateTimeToX(TimeLineSections[FSelectedSection].StartTime);
        stopx := DateTimeToX(TimeLineSections[FSelectedSection].EndTime);
        FSecStartX := startx + X - FCx;
        FSecEndX := stopx + X - FCx;
        vstart := XToDateTime(FSecStartX);
        vend := XToDateTime(FSecEndX);

        CanChange := True;

        if Assigned(OnSectionPositionCanChange) then
          OnSectionPositionCanChange(Self, TimeLineSections[FSelectedSection], vstart, vend, CanChange);

        if not CanChange then
        begin
          if FMouseDownOnSection then
          begin
            if FMouseMovedOnSection then
            begin
              if (FSelectedSection >= 0) and
                (FSelectedSection <= TimeLineSections.Count - 1) then
              begin
                delta := TimeLineSections[FSelectedSection].EndTime - TimeLineSections[FSelectedSection].StartTime;
                tr := GetTimeLineBarRect;

                if (FSecStartX <= tr.X) then
                begin
                  vend := XToDateTime(FSecEndX);
                  vstart := vend - delta;
                end
                else
                if (FSecEndX >= tr.X + tr.Width) then
                begin
                  vstart := XToDateTime(FSecStartX);
                  vend := vstart + delta;
                end
                else
                begin
                  vstart := XToDateTime(FSecStartX);
                  vend := XToDateTime(FSecEndX);
                end;

                TimeLineSections[FSelectedSection].StartTime := vstart;
                TimeLineSections[FSelectedSection].EndTime := vend;
                FMouseMovedOnSection := false;
                DoSectionPositionChanged(Self, TimeLineSections[FSelectedSection],
                  TimeLineSections[FSelectedSection].StartTime,
                  TimeLineSections[FSelectedSection].EndTime);
              end;
            end
          end;

          FMouseMovedOnSection := False;
          FMouseDownOnSection := False;
        end;

        if CanChange and Assigned(FOnSectionPositionChanging) then
          FOnSectionPositionChanging(Self, TimeLineSections[FSelectedSection],
            vstart, vend);
        BuildSections;
        Changed;
      end;
    end
    else if FMouseDownOnTimeLine and FMouseMovedOnTimeLine and not ReadOnly then
    begin
      if GetTimeLineBarRect.Width > 0 then
      begin
        MovedPixels := (X - FMx);
        FMx := X;
        Dif := (Range.RangeTo - Range.RangeFrom) / GetTimeLineBarRect.Width;
        Dif := Dif * MovedPixels;
        if IsPartialZoomingAllowed(Dif) then
        begin
          if (ssCtrl in Shift) and Range.AllowZooming then
          begin
            BeginUpdate;
            if Assigned(OnScrollTimeLine) then
              OnScrollTimeLine(Self, Range.RangeFrom, Range.RangeTo,
                Range.RangeFrom - Dif, Range.RangeTo + Dif);
            Range.RangeFrom := Range.RangeFrom - Dif;
            Range.RangeTo := Range.RangeTo + Dif;
            EndUpdate;
          end
          else if Range.AllowScrolling then
          begin
            BeginUpdate;
            if Assigned(OnScrollTimeLine) then
              OnScrollTimeLine(Self, Range.RangeFrom, Range.RangeTo,
                Range.RangeFrom - Dif, Range.RangeTo - Dif);
            Range.RangeFrom := Range.RangeFrom - Dif;
            Range.RangeTo := Range.RangeTo - Dif;
            EndUpdate;
          end;
        end;
      end;
    end
    else
    begin
      if FMouseDownOnIndicator or FMouseDownOnSection or FMouseDownOnTimeLine
        then
      begin
        if not((X < FCx + 2) and (X > FCx - 2) and (Y < FCy + 2) and
            (Y > FCy - 2)) and not ReadOnly then
        begin
          if FMouseDownOnTimeLine then
            FMouseMovedOnTimeLine := true;

          if (FSelectedSection >= 0) and
            (FSelectedSection <= TimeLineSections.Count - 1)
            and FMouseDownOnSection then
            FMouseMovedOnSection := not TimeLineSections[FSelectedSection]
              .FixedPosition;

          if (FSelectedIndicator >= 0) and
            (FSelectedIndicator <= TimeLineIndicators.Count - 1)
            and FMouseDownOnIndicator then
            FMouseMovedOnIndicator := not TimeLineIndicators[FSelectedIndicator]
              .Fixed;
        end;
      end
      else
      begin
        sc := SectionAtXY(X, Y);
        if not((sc = -1) and (FHoveredSection = -1)) then
        begin
          if FHoveredSection <> sc then
          begin
            if Fh.Visible then
            begin
              FhintSizeCalculated := false;
              Fh.Hide;
            end;
          end;

          FHoveredSection := sc;
          if not FhintSizeCalculated then
          begin
            CalculateHintSize(Fh);
            FhintSizeCalculated := true;
          end;
          pt := Point(X, Y);
          Fh.Left := ClientToScreen(pt).X;
          Fh.Top := ClientToScreen(pt).Y + 20;
          if not Fh.Visible then
          begin
            Fh.Show;
            Fh.Init;
          end;
        end
        else
        begin
          if Fh.Visible then
          begin
            FhintSizeCalculated := false;
            Fh.Hide;
          end;
        end;
      end;
    end;
  end;
end;

procedure TAdvSmoothTimeLine.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  v, vstart, vend: TDateTime;
  indr, r: TGPRectF;
  xs: Double;
  ptpopup: TPoint;
  MovedPixels: Integer;
  Dif: Double;
  delta: TDatetime;
  tr: TGPRectF;
begin
  inherited;

  if FMouseDownOnIndicator then
  begin
    if FMouseMovedOnIndicator then
    begin
      if (FSelectedIndicator >= 0) and
        (FSelectedIndicator <= TimeLineIndicators.Count - 1) then
      begin
        if not TimeLineIndicators[FSelectedIndicator].Fixed then
        begin
          r := TimeLineIndicators[FSelectedIndicator].GetIndicatorRect;
          xs := r.X + r.Width / 2;
          v := XToDateTime(Round(xs));
          FMouseMovedOnIndicator := false;
          TimeLineIndicators[FSelectedIndicator].Position := v;
          FReBuildLists := true;
          BuildTimeLineValues;
          BuildAnnotations;
          BuildSections;
          DoIndicatorPositionChanged
            (Self, TimeLineIndicators[FSelectedIndicator],
            TimeLineIndicators[FSelectedIndicator].Position);
        end;
      end;
    end
    else
    begin
      if (FSelectedIndicator >= 0) and
        (FSelectedIndicator <= TimeLineIndicators.Count - 1) then
      begin
        if FMbRight then
        begin
          with TimeLineIndicators[FSelectedIndicator] do
          begin
            if Assigned(PopupMenu) then
            begin
              indr := GetIndicatorRect;
              ptpopup := ClientToScreen
                (Point(Round(indr.X + indr.Width / 2), Round(indr.Y)));
              PopupMenu.Popup(ptpopup.X, ptpopup.Y);
            end;
          end
        end
        else
        begin
          if Assigned(FOnIndicatorClick) and not FMouseMovedOnTimeLine then
            FOnIndicatorClick(Self, TimeLineIndicators[FSelectedIndicator]);
        end;
      end;
    end
  end
  else if FMouseDownOnStartSection then
  begin
    if (FSelectedSection >= 0) and
      (FSelectedSection <= TimeLineSections.Count - 1) then
    begin
      vstart := XToDateTime(FSecStartX);
      TimeLineSections[FSelectedSection].StartTime := vstart;
      DoSectionPositionChanged(Self, TimeLineSections[FSelectedSection],
        TimeLineSections[FSelectedSection].StartTime,
        TimeLineSections[FSelectedSection].EndTime);
    end;
  end
  else if FMouseDownOnEndSection then
  begin
    if (FSelectedSection >= 0) and
      (FSelectedSection <= TimeLineSections.Count - 1) then
    begin
      vend := XToDateTime(FSecEndX);
      TimeLineSections[FSelectedSection].EndTime := vend;
      DoSectionPositionChanged(Self, TimeLineSections[FSelectedSection],
        TimeLineSections[FSelectedSection].StartTime,
        TimeLineSections[FSelectedSection].EndTime);
    end;
  end
  else if FMouseDownOnSection then
  begin
    if FMouseMovedOnSection then
    begin
      if (FSelectedSection >= 0) and
        (FSelectedSection <= TimeLineSections.Count - 1) then
      begin
        delta := TimeLineSections[FSelectedSection].EndTime - TimeLineSections[FSelectedSection].StartTime;
        tr := GetTimeLineBarRect;

        if (FSecStartX <= tr.X) then
        begin
          vend := XToDateTime(FSecEndX);
          vstart := vend - delta;
        end
        else
        if (FSecEndX >= tr.X + tr.Width) then
        begin
          vstart := XToDateTime(FSecStartX);
          vend := vstart + delta;
        end
        else
        begin
          vstart := XToDateTime(FSecStartX);
          vend := XToDateTime(FSecEndX);
        end;

        TimeLineSections[FSelectedSection].StartTime := vstart;
        TimeLineSections[FSelectedSection].EndTime := vend;
        FMouseMovedOnSection := false;
        DoSectionPositionChanged(Self, TimeLineSections[FSelectedSection],
          TimeLineSections[FSelectedSection].StartTime,
          TimeLineSections[FSelectedSection].EndTime);
      end;
    end
    else
    begin
      if (FSelectedSection >= 0) and
        (FSelectedSection <= TimeLineSections.Count - 1)
        and not FMouseMovedOnTimeLine then
      begin
        if Assigned(FOnSectionClick) then
          FOnSectionClick(Self, TimeLineSections[FSelectedSection]);
      end;
    end;
  end
  else if FMouseDownOnTimeLine and FAllowAnimation then
  begin
    if FMouseMovedOnTimeLine then
    begin
      FStopTime := GetTickCount;
      MovedPixels := (X - FCx);
      FSp := 0;
      if (FStopTime - FStartTime < 200) and (FStopTime - FStartTime > 0) then
        FSp := Abs(MovedPixels) / (FStopTime - FStartTime);

      if FSp > 1 then
      begin
        if GetTimeLineBarRect.Width > 0 then
        begin
          Dif := (Range.RangeTo - Range.RangeFrom) / GetTimeLineBarRect.Width;
          Dif := Dif * MovedPixels;
          if IsPartialZoomingAllowed(Dif * 10) then
          begin
            Range.FRangeFromAnim := max
              (min(Range.RangeFrom - (Dif * 10), Range.MaximumRange),
              Range.MinimumRange);
            Range.FRangeToAnim := max
              (min(Range.RangeTo - (Dif * 10), Range.MaximumRange),
              Range.MinimumRange);
            if not((Range.FRangeToAnim = Range.MaximumRange) or
                (Range.FRangeFromAnim = Range.MinimumRange)) then
            begin
              Range.FRangeFromAnim := Range.FRangeFromAnim - GetDivisionValue
                (dtMilliSecond);
              Range.FRangeToAnim := Range.FRangeToAnim + GetDivisionValue
                (dtMilliSecond);
              Range.FAnimateRangeFrom := true;
              Range.FAnimateRangeTo := true;
            end;
          end;
        end;
      end;
    end;
  end;

  FMouseDownOnIndicator := false;
  FMouseDownOnSection := false;
  FMouseDownOnStartSection := false;
  FMouseDownOnEndSection := false;
  FMouseDownOnTimeLine := false;
  FMouseMovedOnTimeLine := false;
  FSelectedIndicator := -1;
  FSelectedSection := -1;
  FScrollTimeLine := false;
  FScrollDif := 0;
  Changed;
end;

procedure TAdvSmoothTimeLine.MouseWheelHandler(var Message: TMessage);
var
  d: Double;
begin
  inherited;
  if ReadOnly then
    Exit;

  if TabStop and Focused and Range.AllowScrolling then
  begin
    case Message.Msg of
      WM_MOUSEWHEEL:
        begin
          if GetTimeLineBarRect.Width > 0 then
          begin
            d := ((Range.RangeTo - Range.RangeFrom) / GetTimeLineBarRect.Width)
              * Range.ScrollStep;
            if IsPartialZoomingAllowed(d) then
            begin
              if integer(Message.WParam) < 0 then
              begin
                if Assigned(OnScrollTimeLine) then
                  OnScrollTimeLine(Self, Range.RangeFrom, Range.RangeTo,
                    Range.RangeFrom + d, Range.RangeTo + d);
                Range.RangeFrom := Range.RangeFrom + d;
                Range.RangeTo := Range.RangeTo + d;
              end
              else
              begin
                if Assigned(OnScrollTimeLine) then
                  OnScrollTimeLine(Self, Range.RangeFrom, Range.RangeTo,
                    Range.RangeFrom - d, Range.RangeTo - d);
                Range.RangeFrom := Range.RangeFrom - d;
                Range.RangeTo := Range.RangeTo - d;
              end;
              FReBuildLists := true;
              BuildTimeLineValues;
              BuildAnnotations;
              BuildSections;
            end;
          end;
        end;
    end;
  end;
end;

procedure TAdvSmoothTimeLine.Notification(AComponent: TComponent;
  AOperation: TOperation);
var
  I: Integer;
begin
  inherited;

  if (csDestroying in ComponentState) then
    Exit;

  if (AOperation = opRemove) then
  begin
    for I := 0 to TimeLineIndicators.Count - 1 do
    begin
      with TimeLineIndicators[I] do
      begin
        if (AComponent = FPopupMenu) then
          FPopupMenu := nil;
      end;
    end;

    if (AComponent = FImageList) then
      FImageList := nil;
  end;
end;

procedure TAdvSmoothTimeLine.Paint;
begin
  DrawTimeLine(Canvas);
  if Assigned(OnDrawTimeLine) then
    OnDrawTimeLine(Self, Canvas, ClientRect);
end;

function TAdvSmoothTimeLine.PtInGPRect(r: TGPRectF; pt: TPoint): Boolean;
begin
  result := ((pt.X >= r.X) and (pt.X <= r.X + r.Width)) and
    ((pt.Y >= r.Y) and (pt.Y <= r.Y + r.Height));
end;

procedure TAdvSmoothTimeLine.RangeAppearanceChanged(Sender: TObject);
begin
  BuildTimeLineValues;
  Changed;
end;

procedure TAdvSmoothTimeLine.RangeChanged(Sender: TObject);
begin
  BuildTimeLineValues;
  Changed;
end;

procedure TAdvSmoothTimeLine.Resize;
begin
  inherited;
  FReBuildLists := true;
  BuildTimeLineValues;
  BuildAnnotations;
  BuildSections;
  Changed;
end;

procedure TAdvSmoothTimeLine.SaveToImage(FileName: String;
  ImageWidth, ImageHeight: Integer; ImageType: TImageType;
  ImageQualityPercentage: Integer);
var
  img, finalimg: Graphics.TBitmap;
  gpimg: TGPImage;
  g: TGPGraphics;
  enc: TEncoderParameters;
begin
  img := nil;
  gpimg := nil;
  g := nil;
  finalimg := nil;
  try
    img := Graphics.TBitmap.Create;
    img.Width := Width;
    img.Height := Height;

    DrawTimeLine(img.Canvas);

    finalimg := Graphics.TBitmap.Create;
    finalimg.Width := ImageWidth;
    finalimg.Height := ImageHeight;
    finalimg.Canvas.StretchDraw(Bounds(0, 0, ImageWidth, ImageHeight), img);

    gpimg := TGPImage.Create(CreateStream(finalimg));

    enc := GetEncoderQualityParameters(ImageQualityPercentage);

    gpimg.Save(FileName, GetCLSID(ImageType), @enc);

  finally
    gpimg.Free;
    finalimg.Free;
    g.Free;
    img.Free;
  end;
end;

procedure TAdvSmoothTimeLine.SaveToTheme(FileName: String);
begin

end;

function TAdvSmoothTimeLine.SectionAnimateHandles(X, Y: Integer): Integer;
var
  I: Integer;
  r: TGPRectF;
begin
  result := -1;
  for I := TimeLineSections.Count - 1 downto 0 do
  begin
    if not TimeLineSections[I].FixedSize then
    begin
      r := TimeLineSections[I].GetSectionRect;
      if PtInGPRect(MakeRect(r.X - 5, r.Y, r.Width + 10, r.Height), Point(X, Y)
        ) then
      begin
        result := I;
        Break;
      end;
    end;
  end;
end;

function TAdvSmoothTimeLine.SectionAtXY(X, Y: Integer): Integer;
var
  I: Integer;
begin
  result := -1;
  for I := TimeLineSections.Count - 1 downto 0 do
  begin
    if PtInGPRect(TimeLineSections[I].GetSectionRect, Point(X, Y)) and
      ((TimeLineSections[I].Hint <> '') or not TimeLineSections[I]
        .HintFill.Picture.Empty) then
    begin
      result := I;
      Break;
    end;
  end;
end;

function TAdvSmoothTimeLine.SectionMoveAtXY(X, Y: Integer): Integer;
var
  I: Integer;
begin
  result := -1;
  for I := TimeLineSections.Count - 1 downto 0 do
  begin
    if PtInGPRect(TimeLineSections[I].GetSectionRect, Point(X, Y)) then
    begin
      result := I;
      Break;
    end;
  end;
end;

function TAdvSmoothTimeLine.SectionSizeEndAtXY(X, Y: Integer): Integer;
var
  I: Integer;
  r: TGPRectF;
begin
  result := -1;
  for I := TimeLineSections.Count - 1 downto 0 do
  begin
    if not TimeLineSections[I].FixedSize and (TimeLineSections[i].EndTime <= Range.RangeTo) then
    begin
      r := TimeLineSections[I].GetSectionRect;
      r.X := r.X + r.Width - 3;
      r.Width := 6;
      if PtInGPRect(r, Point(X, Y)) then
      begin
        result := I;
        Break;
      end;
    end;
  end;
end;

function TAdvSmoothTimeLine.SectionSizeStartAtXY(X, Y: Integer): Integer;
var
  I: Integer;
  r: TGPRectF;
begin
  result := -1;
  for I := TimeLineSections.Count - 1 downto 0 do
  begin
    if not TimeLineSections[I].FixedSize and (TimeLineSections[i].StartTime >= Range.RangeFrom) then
    begin
      r := TimeLineSections[I].GetSectionRect;
      r.X := r.X - 3;
      r.Width := 6;
      if PtInGPRect(r, Point(X, Y)) then
      begin
        result := I;
        Break;
      end;
    end;
  end;
end;

procedure TAdvSmoothTimeLine.SetColorTones(ATones: TColorTones);
var
  i: integer;
begin
  Fill.Color := ATones.Background.BrushColor;
  Fill.ColorTo := ATones.Background.BrushColor;
  Fill.BorderColor := ATones.Background.BorderColor;

  TimeLineBar.Fill.Color := ATones.Foreground.BrushColor;
  TimeLineBar.Fill.ColorTo := ATones.Foreground.BrushColor;
  TimeLineBar.Fill.GradientMirrorType := gtNone;
  TimeLineBar.Fill.BorderColor := ATones.Foreground.BorderColor;

  TimeLineBar.SectionCaptionFont.Color := ATones.Selected.TextColor;

  RangeAppearance.DivisionTickMarkColor := ATones.Background.BorderColor;
  RangeAppearance.SubDivisionTickMarkColor := ATones.Background.BorderColor;
  RangeAppearance.DivisionFont.Color := ATones.Background.BorderColor;
  RangeAppearance.SubDivisionFont.Color := ATones.Background.BorderColor;


  DefaultSectionFill.Color := ATones.Selected.BrushColor;
  DefaultSectionFill.ColorTo := ATones.Selected.BrushColor;
  DefaultSectionFill.ColorMirror := clNone;
  DefaultSectionFill.ColorMirrorTo := clNone;
  DefaultSectionFill.BorderColor := ATones.Selected.BorderColor;
  DefaultSectionFill.GradientMirrorType := gtNone;

  DefaultIndicator.Color := ATones.Selected.BrushColor;
  DefaultIndicator.ColorTo := ATones.Selected.BrushColor ;
  DefaultIndicator.AnnotationColor := ATones.Selected.BrushColor;
  DefaultIndicator.AnnotationLineColor := ATones.Selected.BorderColor;

  DefaultHintFill.Color := ATones.Background.BrushColor;
  DefaultHintFill.ColorTo := ATones.Background.BrushColor;
  DefaultHintFill.BorderColor := ATones.Background.BorderColor;

  for I := 0 to TimeLineSections.Count - 1 do
  begin
    TimeLineSections[I].HintFill.Assign(DefaultHintFill);
    TimeLineSections[I].Fill.Assign(DefaultSectionFill);
  end;

  for I := 0 to TimeLineIndicators.Count - 1 do
    TimeLineIndicators[I].AssignVisuals(DefaultIndicator);
end;

procedure TAdvSmoothTimeLine.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('TMSStyle',ReadTMSStyle, WriteTMSStyle, True);
end;

procedure TAdvSmoothTimeLine.ReadTMSStyle(Reader: TReader);
begin
  FTMSStyle := TTMSStyle(Reader.ReadInteger);
end;

procedure TAdvSmoothTimeLine.WriteTMSStyle(Writer: TWriter);
begin
  Writer.WriteInteger(integer(FTMSStyle));
end;

procedure TAdvSmoothTimeLine.SetComponentStyle(AStyle: TTMSStyle);
var
  I: Integer;
begin
  FTMSStyle := AStyle;
  RangeAppearance.DivisionFont.Color := clBlack;
  RangeAppearance.SubDivisionFont.Color := clBlack;
  RangeAppearance.DivisionTickMarkColor := clBlack;
  RangeAppearance.SubDivisionTickMarkColor := clBlack;

  case AStyle of
    tsOffice2003Blue:
      begin
        Fill.Color := $00FFD2AF;
        Fill.ColorTo := $00FFD2AF;
        Fill.BorderColor := clNone;

        TimeLineBar.Fill.Color := $D68759;
        TimeLineBar.Fill.ColorTo := $933803;
        TimeLineBar.Fill.Color := clWhite;
        TimeLineBar.Fill.BorderColor := $962D00;

        DefaultSectionFill.Color := $AAD9FF;
        DefaultSectionFill.ColorTo := $6EBBFF;
        DefaultSectionFill.ColorMirror := $42AEFE;
        DefaultSectionFill.ColorMirrorTo := $7AE1FE;
        DefaultSectionFill.BorderColor := $42AEFE;
        DefaultSectionFill.GradientMirrorType := gtVertical;
      end;
    tsOffice2003Silver:
      begin
        Fill.Color := $00E6D8D8;
        Fill.ColorTo := $00E6D8D8;
        Fill.BorderColor := clNone;

        TimeLineBar.Fill.Color := $BDA4A5;
        TimeLineBar.Fill.ColorTo := $957475;
        TimeLineBar.Fill.BorderColor := $947C7C;

        DefaultSectionFill.Color := $AAD9FF;
        DefaultSectionFill.ColorTo := $6EBBFF;
        DefaultSectionFill.ColorMirror := $42AEFE;
        DefaultSectionFill.ColorMirrorTo := $7AE1FE;
        DefaultSectionFill.BorderColor := $42AEFE;
        DefaultSectionFill.GradientMirrorType := gtVertical;

      end;
    tsOffice2003Olive:
      begin
        Fill.Color := RGB(225, 234, 185);
        Fill.ColorTo := RGB(225, 234, 185);
        Fill.BorderColor := clNone;

        TimeLineBar.Fill.Color := $82C0AF;
        TimeLineBar.Fill.ColorTo := $447A63;
        TimeLineBar.Fill.BorderColor := $588060;

        DefaultSectionFill.Color := $AAD9FF;
        DefaultSectionFill.ColorTo := $6EBBFF;
        DefaultSectionFill.ColorMirror := $42AEFE;
        DefaultSectionFill.ColorMirrorTo := $7AE1FE;
        DefaultSectionFill.BorderColor := $42AEFE;
        DefaultSectionFill.GradientMirrorType := gtVertical;
      end;
    tsOffice2003Classic:
      begin
        Fill.Color := $00F2F2F2;
        Fill.ColorTo := $00F2F2F2;
        Fill.BorderColor := clNone;

        TimeLineBar.Fill.Color := $808080;
        TimeLineBar.Fill.ColorTo := $808080;
        TimeLineBar.Fill.BorderColor := $808080;

        DefaultSectionFill.Color := $B59285;
        DefaultSectionFill.ColorTo := $B59285;
        DefaultSectionFill.ColorMirror := clNone;
        DefaultSectionFill.ColorMirrorTo := clNone;
        DefaultSectionFill.BorderColor := $808080;
        DefaultSectionFill.GradientMirrorType := gtVertical;
      end;
    tsOffice2007Luna:
      begin
        Fill.Color := $00F3E5DA;
        Fill.ColorTo := $00F0DED0;
        Fill.BorderColor := clNone;

        TimeLineBar.Fill.Color := $FFEFE3;
        TimeLineBar.Fill.ColorTo := $FFD2AF;
        TimeLineBar.Fill.BorderColor := $00FFD2AF;

        DefaultSectionFill.Color := $AAD9FF;
        DefaultSectionFill.ColorTo := $6EBBFF;
        DefaultSectionFill.ColorMirror := $42AEFE;
        DefaultSectionFill.ColorMirrorTo := $7AE1FE;
        DefaultSectionFill.BorderColor := $42AEFE;
        DefaultSectionFill.GradientMirrorType := gtVertical;
      end;
    tsOffice2007Obsidian:
      begin
        Fill.Color := $5C534C;
        Fill.ColorTo := $5C534C;
        Fill.BorderColor := clNone;

        TimeLineBar.Fill.Color := $F2F1F0;
        TimeLineBar.Fill.ColorTo := $C9C2BD;
        TimeLineBar.Fill.BorderColor := $5C534C;

        DefaultSectionFill.Color := $AAD9FF;
        DefaultSectionFill.ColorTo := $6EBBFF;
        DefaultSectionFill.ColorMirror := $42AEFE;
        DefaultSectionFill.ColorMirrorTo := $7AE1FE;
        DefaultSectionFill.BorderColor := $42AEFE;
        DefaultSectionFill.GradientMirrorType := gtVertical;

        RangeAppearance.DivisionFont.Color := clWhite;
        RangeAppearance.SubDivisionFont.Color := clWhite;
        RangeAppearance.DivisionTickMarkColor := clWhite;
        RangeAppearance.SubDivisionTickMarkColor := clWhite;
      end;
    tsWindowsXP:
      begin
        Fill.Color := $00B6B6B6;
        Fill.ColorTo := $00B6B6B6;

        TimeLineBar.Fill.Color := clBtnFace;
        TimeLineBar.Fill.ColorTo := clBtnFace;
        TimeLineBar.Fill.BorderColor := clBlack;

        DefaultSectionFill.Color := clInactiveCaption;
        DefaultSectionFill.ColorTo := clInactiveCaption;
        DefaultSectionFill.ColorMirror := clNone;
        DefaultSectionFill.ColorMirrorTo := clNone;
        DefaultSectionFill.BorderColor := clHighlight;
        DefaultSectionFill.GradientMirrorType := gtVertical;
      end;
    tsWhidbey:
      begin
        Fill.Color := $F5F9FA;
        Fill.ColorTo := $F5F9FA;
        Fill.BorderColor := clNone;

        TimeLineBar.Fill.Color := $EBEEEF;
        TimeLineBar.Fill.ColorTo := $7E9898;
        TimeLineBar.Fill.BorderColor := $962D00;

        DefaultSectionFill.Color := $AAD9FF;
        DefaultSectionFill.ColorTo := $6EBBFF;
        DefaultSectionFill.ColorMirror := $42AEFE;
        DefaultSectionFill.ColorMirrorTo := $7AE1FE;
        DefaultSectionFill.BorderColor := $42AEFE;
        DefaultSectionFill.GradientMirrorType := gtVertical;
      end;
    tsCustom:
      ;
    tsOffice2007Silver:
      begin
        Fill.Color := RGB(241, 244, 248);
        Fill.ColorTo := RGB(227, 232, 240);
        Fill.BorderColor := clNone;

        TimeLineBar.Fill.Color := $F8F7F6;
        TimeLineBar.Fill.ColorTo := $E8E0DB;
        TimeLineBar.Fill.BorderColor := $74706F;

        DefaultSectionFill.Color := $AAD9FF;
        DefaultSectionFill.ColorTo := $6EBBFF;
        DefaultSectionFill.ColorMirror := $42AEFE;
        DefaultSectionFill.ColorMirrorTo := $7AE1FE;
        DefaultSectionFill.BorderColor := $42AEFE;
        DefaultSectionFill.GradientMirrorType := gtVertical;
      end;
    tsWindowsVista:
      begin
        Fill.Color := $FFFDF9;
        Fill.ColorTo := $FFFAF0;
        Fill.BorderColor := $FCF2DA;

        TimeLineBar.Fill.Color := $FEF9F0;
        TimeLineBar.Fill.ColorTo := $FDF0D7;
        TimeLineBar.Fill.BorderColor := $FEDF9A;

        DefaultSectionFill.Color := $FCEBDC;
        DefaultSectionFill.ColorTo := $FCDBC1;
        DefaultSectionFill.ColorMirror := clNone;
        DefaultSectionFill.ColorMirrorTo := clNone;
        DefaultSectionFill.BorderColor := $CEA27D;
        DefaultSectionFill.GradientMirrorType := gtVertical;
      end;
    tsWindows7:
      begin
        Fill.Color := $FCEBDC;
        Fill.ColorTo := $FCDBC1;
        Fill.BorderColor := $CEA27D;

        TimeLineBar.Fill.Color := $FEF9F0;
        TimeLineBar.Fill.ColorTo := $FDF0D7;
        TimeLineBar.Fill.BorderColor := $FEDF9A;

        DefaultSectionFill.Color := $FCEBDC;
        DefaultSectionFill.ColorTo := $FCDBC1;
        DefaultSectionFill.ColorMirror := clNone;
        DefaultSectionFill.ColorMirrorTo := clNone;
        DefaultSectionFill.BorderColor := $CEA27D;
        DefaultSectionFill.GradientMirrorType := gtVertical;
      end;
    tsTerminal:
      begin
        Fill.Color := clBtnFace;
        Fill.ColorTo := clBtnFace;
        Fill.BorderColor := clNone;

        TimeLineBar.Fill.Color := clsilver;
        TimeLineBar.Fill.ColorTo := clsilver;
        TimeLineBar.Fill.BorderColor := clGray;

        DefaultSectionFill.Color := clHighlight;
        DefaultSectionFill.ColorTo := clHighlight;
        DefaultSectionFill.ColorMirror := clNone;
        DefaultSectionFill.ColorMirrorTo := clNone;
        DefaultSectionFill.BorderColor := clGray;
      end;
    tsOffice2010Blue:
      begin
        Fill.Color := $F6E6D9;
        Fill.ColorTo := clNone;
        Fill.BorderColor := $C7B29F;

        TimeLineBar.Fill.Color := $EAD3BF;
        TimeLineBar.Fill.ColorTo := clNone;
        TimeLineBar.Fill.BorderColor := $5B391E;

        DefaultSectionFill.Color := $75E5FF;
        DefaultSectionFill.ColorTo := $7DF0FF;
        DefaultSectionFill.ColorMirror := clNone;
        DefaultSectionFill.ColorMirrorTo := clNone;
        DefaultSectionFill.BorderColor := $308AC2;
        DefaultSectionFill.GradientMirrorType := gtVertical;
      end;
    tsOffice2010Silver:
      begin
        Fill.Color := $EDE5E0;
        Fill.ColorTo := clNone;
        Fill.BorderColor := $D2CDC8;

        TimeLineBar.Fill.Color := $D4CFCB;
        TimeLineBar.Fill.ColorTo := clNone;
        TimeLineBar.Fill.BorderColor := $7C6D66;

        DefaultSectionFill.Color := $75E5FF;
        DefaultSectionFill.ColorTo := $7DF0FF;
        DefaultSectionFill.ColorMirror := clNone;
        DefaultSectionFill.ColorMirrorTo := clNone;
        DefaultSectionFill.BorderColor := $308AC2;
        DefaultSectionFill.GradientMirrorType := gtVertical;
      end;
    tsOffice2010Black:
      begin
        Fill.Color := $BFBFBF;
        Fill.ColorTo := clNone;
        Fill.BorderColor := $6D6D6D;

        TimeLineBar.Fill.Color := $919191;
        TimeLineBar.Fill.ColorTo := clNone;
        TimeLineBar.Fill.BorderColor := $656565;

      end;

  tsWindows8, tsWindows10:
      begin
        Fill.Color := $F7F6F5;
        Fill.ColorTo := clNone;
        Fill.BorderColor := $DCDBDA;

        TimeLineBar.Fill.Color := clWhite;
        TimeLineBar.Fill.ColorTo := clNone;
        TimeLineBar.Fill.BorderColor := $DCDBDA;

        DefaultSectionFill.Color := $F7E0C9;
        DefaultSectionFill.ColorTo := $F7E0C9;
        DefaultSectionFill.ColorMirror := clNone;
        DefaultSectionFill.ColorMirrorTo := clNone;
        DefaultSectionFill.BorderColor := $E4A262;
        DefaultSectionFill.GradientMirrorType := gtVertical;
      end;

    tsOffice2013White:
      begin
        Fill.Color := clWhite;
        Fill.ColorTo := clNone;
        Fill.BorderColor := $D4D4D4;

        TimeLineBar.Fill.Color := clWhite;
        TimeLineBar.Fill.ColorTo := clNone;
        TimeLineBar.Fill.BorderColor := $D4D4D4;

        DefaultSectionFill.Color := $FCE2C8;
        DefaultSectionFill.ColorTo := $FCE2C8;
        DefaultSectionFill.ColorMirror := clNone;
        DefaultSectionFill.ColorMirrorTo := clNone;
        DefaultSectionFill.BorderColor := $E59D56;
        DefaultSectionFill.GradientMirrorType := gtVertical;
      end;

    tsOffice2013LightGray:
      begin
        Fill.Color := $FAFAFA;
        Fill.ColorTo := clNone;
        Fill.BorderColor := $C6C6C6;

        TimeLineBar.Fill.Color := clWhite;
        TimeLineBar.Fill.ColorTo := clNone;
        TimeLineBar.Fill.BorderColor := $C6C6C6;

        DefaultSectionFill.Color := $FCE2C8;
        DefaultSectionFill.ColorTo := $FCE2C8;
        DefaultSectionFill.ColorMirror := clNone;
        DefaultSectionFill.ColorMirrorTo := clNone;
        DefaultSectionFill.BorderColor := $E59D56;
        DefaultSectionFill.GradientMirrorType := gtVertical;
      end;
    tsOffice2013Gray:
      begin
        Fill.Color := $F3F3F3;
        Fill.ColorTo := clNone;
        Fill.BorderColor := $ABABAB;

        TimeLineBar.Fill.Color := clWhite;
        TimeLineBar.Fill.ColorTo := clNone;
        TimeLineBar.Fill.BorderColor := $ABABAB;

        DefaultSectionFill.Color := $FCE2C8;
        DefaultSectionFill.ColorTo := $FCE2C8;
        DefaultSectionFill.ColorMirror := clNone;
        DefaultSectionFill.ColorMirrorTo := clNone;
        DefaultSectionFill.BorderColor := $E59D56;
        DefaultSectionFill.GradientMirrorType := gtVertical;
      end;

    tsOffice2016White:
      begin
        Fill.Color := clWhite;
        Fill.ColorTo := clNone;
        Fill.BorderColor := $D4D4D4;

        TimeLineBar.Fill.Color := clWhite;
        TimeLineBar.Fill.ColorTo := clNone;
        TimeLineBar.Fill.BorderColor := $D4D4D4;

        DefaultSectionFill.Color := $E3BDA3;
        DefaultSectionFill.ColorTo := $E3BDA3;
        DefaultSectionFill.ColorMirror := clNone;
        DefaultSectionFill.ColorMirrorTo := clNone;
        DefaultSectionFill.BorderColor := $E3BDA3;
        DefaultSectionFill.GradientMirrorType := gtVertical;
      end;

    tsOffice2016Gray:
      begin
        Fill.Color := $B2B2B2;
        Fill.ColorTo := clNone;
        Fill.BorderColor := $444444;

        TimeLineBar.Fill.Color := $444444;
        TimeLineBar.Fill.ColorTo := clNone;
        TimeLineBar.Fill.BorderColor := $444444;

        DefaultSectionFill.Color := $E3BDA3;
        DefaultSectionFill.ColorTo := $E3BDA3;
        DefaultSectionFill.ColorMirror := clNone;
        DefaultSectionFill.ColorMirrorTo := clNone;
        DefaultSectionFill.BorderColor := $E3BDA3;
        DefaultSectionFill.GradientMirrorType := gtVertical;
      end;
    tsOffice2016Black:
      begin
        Fill.Color := $363636;
        Fill.ColorTo := clNone;
        Fill.BorderColor := $4E4E4E;

        TimeLineBar.Fill.Color := $575757;
        TimeLineBar.Fill.ColorTo := clNone;
        TimeLineBar.Fill.BorderColor := $575757;

        DefaultSectionFill.Color := $444444;
        DefaultSectionFill.ColorTo := $444444;
        DefaultSectionFill.ColorMirror := clNone;
        DefaultSectionFill.ColorMirrorTo := clNone;
        DefaultSectionFill.BorderColor := $444444;
        DefaultSectionFill.GradientMirrorType := gtVertical;

        RangeAppearance.DivisionFont.Color := $FFFFFF;
        RangeAppearance.SubDivisionFont.Color := $FFFFFF;
        RangeAppearance.DivisionTickMarkColor := $FFFFFF;
        RangeAppearance.SubDivisionTickMarkColor := $FFFFFF;

      end;


  end;

  DefaultIndicator.Color := DefaultSectionFill.Color;
  DefaultIndicator.ColorTo := DefaultSectionFill.ColorTo;
  DefaultIndicator.OpacityTo := 150;
  DefaultIndicator.Opacity := 200;
  DefaultIndicator.AnnotationColor := DefaultSectionFill.ColorTo;

  DefaultHintFill.Color := clWhite;
  DefaultHintFill.ColorTo := clWhite;
  DefaultHintFill.Opacity := 225;
  DefaultHintFill.OpacityTo := 175;

  Fill.BorderColor := clsilver;

  DefaultHintFill.BorderColor := clBlack;

  for I := 0 to TimeLineSections.Count - 1 do
  begin
    TimeLineSections[I].HintFill.Assign(DefaultHintFill);
    TimeLineSections[I].Fill.Assign(DefaultSectionFill);
  end;

  for I := 0 to TimeLineIndicators.Count - 1 do
    TimeLineIndicators[I].AssignVisuals(DefaultIndicator);
end;

procedure TAdvSmoothTimeLine.SetDefaultHintFill(const Value: TGDIPFill);
begin
  if FDefaultHintFill <> Value then
  begin
    FDefaultHintFill.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothTimeLine.SetDefaultIndicator
  (const Value: TAdvSmoothTimeLineBarIndicator);
begin
  if FDefaultIndicator <> Value then
  begin
    FDefaultIndicator.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothTimeLine.SetDefaultSectionFill(const Value: TGDIPFill);
begin
  if FDefaultSectionFill <> Value then
  begin
    FDefaultSectionFill.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothTimeLine.SetFill(const Value: TGDIPFill);
begin
  if FFill <> Value then
  begin
    FFill.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothTimeLine.SetHorizontalMargin(const Value: Integer);
begin
  if FHorizontalMargin <> Value then
  begin
    FHorizontalMargin := Value;
    BuildTimeLineValues;
    FReBuildLists := true;
    BuildAnnotations;
    BuildSections;
    Changed;
  end;
end;

procedure TAdvSmoothTimeLine.SetImageList(const Value: TCustomImageList);
begin
  FImageList := Value;
  BuildAnnotations;
  Changed;
end;

procedure TAdvSmoothTimeLine.SetRange(const Value: TAdvSmoothTimeLineRange);
begin
  if FRange <> Value then
  begin
    FRange.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothTimeLine.SetTimeLineRange(ARangeFrom, ARangeTo,
  AMinimumRange, AMaximumRange: TDateTime);
begin
  with FRange do
  begin
    FOldRangeFrom := FRangeFrom;
    FOldRangeTo := FRangeTo;
    FRangeFrom := ARangeFrom;
    FRangeTo := ARangeTo;
    FMinimumRange := AMinimumRange;
    FMaximumRange := AMaximumRange;
  end;
  Changed;
end;

procedure TAdvSmoothTimeLine.SetTimeLineRange(ARangeFrom, ARangeTo: TDateTime);
begin
  with FRange do
  begin
    FMinimumRange := ARangeFrom;
    FMaximumRange := ARangeTo;
    FOldRangeFrom := FRangeFrom;
    FOldRangeTo := FRangeTo;
    FRangeFrom := ARangeFrom;
    FRangeTo := ARangeTo;
  end;
  Changed;
end;

procedure TAdvSmoothTimeLine.SetRangeAppearance
  (const Value: TAdvSmoothTimeLineRangeAppearance);
begin
  if FRangeAppearance <> Value then
  begin
    FRangeAppearance.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothTimeLine.SetReadOnly(const Value: Boolean);
begin
  if FReadOnly <> Value then
  begin
    FReadOnly := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTimeLine.SetTimeLineBar(const Value: TAdvSmoothTimeLineBar);
begin
  if FTimeLineBar <> Value then
  begin
    FTimeLineBar.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothTimeLine.SetTimeLineBarSections
  (const Value: TAdvSmoothTimeLineBarSections);
begin
  if FTimeLineBarSections <> Value then
  begin
    FTimeLineBarSections.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothTimeLine.SetTimeLineIndicators
  (const Value: TAdvSmoothTimeLineBarIndicators);
begin
  if FTimeLineIndicators <> Value then
  begin
    FTimeLineIndicators.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothTimeLine.SetVerticalMargin(const Value: Integer);
begin
  if FVerticalMargin <> Value then
  begin
    FVerticalMargin := Value;
    BuildTimeLineValues;
    FReBuildLists := true;
    BuildAnnotations;
    BuildSections;
    Changed;
  end;
end;

procedure TAdvSmoothTimeLine.TimeLineBarChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothTimeLine.TimeLineBarIndicatorsChanged(Sender: TObject);
begin
  FReBuildLists := true;
  BuildAnnotations;
  Changed;
end;

procedure TAdvSmoothTimeLine.TimeLineBarSectionsChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothTimeLine.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
  SendCancelMode(Self);
  inherited;
  if csCaptureMouse in ControlStyle then
    MouseCapture := True;

  if csClickEvents in ControlStyle then
    IndicatorDblClick;
end;

procedure TAdvSmoothTimeLine.WMPaint(var Message: TWMPaint);
var
  DC, MemDC: HDC;
  MemBitmap, OldBitmap: HBITMAP;
  PS: TPaintStruct;
{$IFNDEF DELPHI_UNICODE}
  dbl: Boolean;
{$ENDIF}
  p: TPoint;
  I: Integer;
begin
  if Assigned(Parent) { and (Fill.ShadowOffset > 0) ? } then
  begin
    DC := Message.DC;
    if DC <> 0 then
    begin
  {$IFNDEF DELPHI_UNICODE}
      dbl := Parent.DoubleBuffered;
      Parent.DoubleBuffered := false;
  {$ENDIF}
      I := SaveDC(DC);
      p := ClientOrigin;
      Windows.ScreenToClient(Parent.Handle, p);
      p.X := -p.X;
      p.Y := -p.Y;
      MoveWindowOrg(DC, p.X, p.Y);
      SendMessage(Parent.Handle, WM_ERASEBKGND, DC, 0);
      SendMessage(Parent.Handle, WM_PAINT, DC, 0);
      if (Parent is TWinCtrl) then (Parent as TWinCtrl)
        .PaintCtrls(DC, nil);
      RestoreDC(DC, I);
  {$IFNDEF DELPHI_UNICODE}
      Parent.DoubleBuffered := dbl;
  {$ENDIF}
    end;
  end;

  if not FDoubleBuffered or (Message.DC <> 0) then
  begin
    if not(csCustomPaint in ControlState) and (ControlCount = 0) then
      inherited
    else
      PaintHandler(Message);
  end
  else
  begin
    DC := GetDC(0);
    MemBitmap := CreateCompatibleBitmap
      (DC, ClientRect.Right, ClientRect.Bottom);
    ReleaseDC(0, DC);
    MemDC := CreateCompatibleDC(0);
    OldBitmap := SelectObject(MemDC, MemBitmap);
    try
      DC := BeginPaint(Handle, PS);
      Perform(WM_ERASEBKGND, MemDC, MemDC);
      Message.DC := MemDC;
      WMPaint(Message);
      Message.DC := 0;
      BitBlt(DC, 0, 0, ClientRect.Right, ClientRect.Bottom, MemDC, 0, 0,
        SRCCOPY);
      EndPaint(Handle, PS);
    finally
      SelectObject(MemDC, OldBitmap);
      DeleteDC(MemDC);
      DeleteObject(MemBitmap);
    end;
  end;
end;

function TAdvSmoothTimeLine.XToDateTime(X: Double): TDateTime;
begin
  result := GetValuePosition(X);
end;

{ TAdvSmoothTimeLineRange }

procedure TAdvSmoothTimeLineRange.Assign(Source: TPersistent);
begin
  if (Source is TAdvSmoothTimeLineRange) then
  begin
    FAutoDivisions := (Source as TAdvSmoothTimeLineRange).AutoDivisions;
    FAutoDivisionSpacing := (Source as TAdvSmoothTimeLineRange).AutoDivisionSpacing;
    FAutoSubDivisions := (Source as TAdvSmoothTimeLineRange).AutoSubDivisions;
    FAutoSubDivisionSpacing := (Source as TAdvSmoothTimeLineRange).AutoSubDivisionSpacing;
    FRangeFrom := (Source as TAdvSmoothTimeLineRange).RangeFrom;
    FRangeTo := (Source as TAdvSmoothTimeLineRange).RangeTo;
    FSubDivisions := (Source as TAdvSmoothTimeLineRange).SubDivisions;
    FDivisions := (Source as TAdvSmoothTimeLineRange).Divisions;
    FMinimumRange := (Source as TAdvSmoothTimeLineRange).MinimumRange;
    FMaximumRange := (Source as TAdvSmoothTimeLineRange).MaximumRange;
    FAllowScrolling := (Source as TAdvSmoothTimeLineRange).AllowScrolling;
    FAllowZooming := (Source as TAdvSmoothTimeLineRange).AllowZooming;
    FDivisionType := (Source as TAdvSmoothTimeLineRange).DivisionType;
    FAutomaticScrolling := (Source as TAdvSmoothTimeLineRange)
      .AutomaticScrolling;
    FAllowPartialZooming := (Source as TAdvSmoothTimeLineRange)
      .AllowPartialZooming;
    FScrollStep := (Source as TAdvSmoothTimeLineRange).ScrollStep;
    Changed;
  end;
end;

procedure TAdvSmoothTimeLineRange.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

constructor TAdvSmoothTimeLineRange.Create(AOwner: TAdvSmoothTimeLine);
var
  Y, m, d, h, n, s, mi: Word;
begin
  FOwner := AOwner;
  if (csDesigning in FOwner.ComponentState) and not (csLoading in FOwner.componentstate) then
  begin
    DecodeDateTime(Now, Y, m, d, h, n, s, mi);
    FOldRangeFrom := FRangeFrom;
    FOldRangeTo := FRangeTo;
    FRangeFrom := EncodeDateTime(Y, m, d, h, n, s, 0);
    FRangeTo := IncSecond(FRangeFrom, 10);
    FMinimumRange := IncHour(FRangeFrom, -1);
    FMaximumRange := IncHour(FRangeTo, 1);
  end;
  FAutoDivisions := False;
  FAutoDivisionSpacing := 10;
  FAutoSubDivisionSpacing := 10;
  FAutoSubDivisions := False;
  FDivisions := 2;
  FSubDivisions := 5;
  FAllowScrolling := true;
  FAllowZooming := true;
  FDivisionType := dtFixedNumber;
  FAutomaticScrolling := true;
  FAllowPartialZooming := true;
  FScrollStep := 5;
end;

destructor TAdvSmoothTimeLineRange.Destroy;
begin
  inherited;
end;

procedure TAdvSmoothTimeLineRange.ResetScrollStatus;
begin
  FAnimateRangeFrom := false;
  FAnimateRangeTo := false;
  FDoAnimateRange := false;
  FOwner.FMouseDownOnTimeLine := false;
  FOwner.FMouseMovedOnTimeLine := false;
end;

procedure TAdvSmoothTimeLineRange.SetAllowPartialZooming(const Value: Boolean);
begin
  if FAllowPartialZooming <> Value then
  begin
    FAllowPartialZooming := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTimeLineRange.SetAllowScrolling(const Value: Boolean);
begin
  if FAllowScrolling <> Value then
  begin
    FAllowScrolling := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTimeLineRange.SetAllowZooming(const Value: Boolean);
begin
  if FAllowZooming <> Value then
  begin
    FAllowZooming := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTimeLineRange.SetAutoDivisions(const Value: Boolean);
begin
  if FAutoDivisions <> Value then
  begin
    FAutoDivisions := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTimeLineRange.SetAutoDivisionSpacing(const Value: Integer);
begin
  if FAutoDivisionSpacing <> Value then
  begin
    FAutoDivisionSpacing := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTimeLineRange.SetAutomaticScrolling(const Value: Boolean);
begin
  if FAutomaticScrolling <> Value then
  begin
    FAutomaticScrolling := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTimeLineRange.SetAutoSubDivisions(const Value: Boolean);
begin
  if FAutoSubDivisions <> Value then
  begin
    FAutoSubDivisions := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTimeLineRange.SetAutoSubDivisionSpacing(
  const Value: Integer);
begin
  if FAutoSubDivisionSpacing <> Value then
  begin
    FAutoSubDivisionSpacing := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTimeLineRange.SetDivisions(const Value: Integer);
begin
  if (FDivisions <> Value) and (Value > 0) then
  begin
    FDivisions := Min(Value, FOwner.Width div 2);
    FixDivisions(Divisions);
    Changed;
  end;
end;

procedure TAdvSmoothTimeLineRange.SetDivisionType
  (const Value: TAdvsmoothTimelineDivisionType);
begin
  if FDivisionType <> Value then
  begin
    FDivisionType := Value;
    FixDivisions(Divisions);
    FixDivisions(SubDivisions);
    Changed;
  end;
end;

procedure TAdvSmoothTimeLineRange.SetMaximumRange(const Value: TDateTime);
begin
  if FMaximumRange <> Value then
  begin
    FMaximumRange := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTimeLineRange.SetMinimumRange(const Value: TDateTime);
begin
  if FMinimumRange <> Value then
  begin
    FMinimumRange := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTimeLineRange.SetRangeFrom(const Value: TDateTime);
var
  fr: TDateTime;
begin
  FOldRangeFrom := FRangeFrom;
  if (FRangeFrom <> Value) then
  begin
    fr := min(max(Value, MinimumRange), MaximumRange);
    if not (csDesigning in FOwner.ComponentState) and not (csLoading in FOwner.ComponentState) then
    begin
      if CompareDateTime(fr, FOldRangeTo) = -1 then
        FRangeFrom := fr
    end
    else
      FRangeFrom := fr;

    FixDivisions(Divisions);
    FixDivisions(SubDivisions);

    Changed;
  end;
end;

procedure TAdvSmoothTimeLineRange.SetRangeTo(const Value: TDateTime);
var
  fr: TDateTime;
begin
  FOldRangeTo := FRangeTo;
  if (FRangeTo <> Value) then
  begin
    fr := min(max(Value, MinimumRange), MaximumRange);
    if not (csDesigning in FOwner.ComponentState) and not (csLoading in FOwner.ComponentState) then
    begin
      if CompareDateTime(fr, FOldRangeFrom) = 1 then
        FRangeTo := fr
    end
    else
      FRangeTo := fr;

    FixDivisions(Divisions);
    FixDivisions(SubDivisions);

    Changed;
  end;
end;

procedure TAdvSmoothTimeLineRange.SetScrollStep(const Value: Integer);
begin
  if FScrollStep <> Value then
  begin
    FScrollStep := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTimeLineRange.SetSubDivisions(const Value: Integer);
begin
  if (FSubDivisions <> Value) and (Value >= 0) then
  begin
    FSubDivisions := Min(Value, (FOwner.Width div 2) div Divisions);
    FixDivisions(SubDivisions);
    Changed;
  end;
end;

{ TAdvSmoothTimeLineRangeAppearance }

procedure TAdvSmoothTimeLineRangeAppearance.Assign(Source: TPersistent);
begin
  if (Source is TAdvSmoothTimeLineRangeAppearance) then
  begin
    FDivisionTickMarkColor := (Source as TAdvSmoothTimeLineRangeAppearance)
      .DivisionTickMarkColor;
    FDivisionTickMarkSize := (Source as TAdvSmoothTimeLineRangeAppearance)
      .DivisionTickMarkSize;
    DivisionFont.Assign((Source as TAdvSmoothTimeLineRangeAppearance)
        .DivisionFont);
    FSubDivisionTickMarkColor := (Source as TAdvSmoothTimeLineRangeAppearance)
      .SubDivisionTickMarkColor;
    FSubDivisionTickMarkSize := (Source as TAdvSmoothTimeLineRangeAppearance)
      .SubDivisionTickMarkSize;
    FSubDivisionFont.Assign((Source as TAdvSmoothTimeLineRangeAppearance)
        .SubDivisionFont);
    FDivisionTickMarkWidth := (Source as TAdvSmoothTimeLineRangeAppearance)
      .DivisionTickMarkWidth;
    FSubDivisionTickMarkWidth := (Source as TAdvSmoothTimeLineRangeAppearance)
      .SubDivisionTickMarkWidth;
    FDivisionFormat := (Source as TAdvSmoothTimeLineRangeAppearance)
      .DivisionFormat;
    FSubDivisionFormat := (Source as TAdvSmoothTimeLineRangeAppearance)
      .SubDivisionFormat;
    FIndentSpacing := (Source as TAdvSmoothTimeLineRangeAppearance)
      .IndentSpacing;
    FShowSubDivisionValue := (Source as TAdvSmoothTimeLineRangeAppearance)
      .ShowSubDivisionValue;
    FShowDivisionValue := (Source as TAdvSmoothTimeLineRangeAppearance)
      .ShowDivisionValue;
    Changed;
  end;
end;

procedure TAdvSmoothTimeLineRangeAppearance.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

constructor TAdvSmoothTimeLineRangeAppearance.Create
  (AOwner: TAdvSmoothTimeLine);
begin
  FOwner := AOwner;
  FDivisionTickMarkColor := clBlack;
  FSubDivisionTickMarkColor := clBlack;
  FDivisionTickMarkSize := 10;
  FSubDivisionTickMarkSize := 8;
  FSubDivisionFont := TFont.Create;
  FSubDivisionFont.OnChange := FontChanged;
  FDivisionFont := TFont.Create;
{$IFNDEF DELPHI9_LVL}
  FDivisionFont.Name := 'Tahoma';
  FSubDivisionFont.Name := 'Tahoma';
{$ENDIF}
  FDivisionFont.OnChange := FontChanged;
  FDivisionTickMarkWidth := 2;
  FSubDivisionTickMarkWidth := 1;
  FDivisionFormat := 'hh:nn:ss';
  FSubDivisionFormat := 'hh:nn:ss';
  FIndentSpacing := 20;
  FShowSubDivisionValue := true;
  FShowDivisionValue := true;
end;

destructor TAdvSmoothTimeLineRangeAppearance.Destroy;
begin
  FDivisionFont.Free;
  FSubDivisionFont.Free;
  inherited;
end;

procedure TAdvSmoothTimeLineRangeAppearance.FontChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothTimeLineRangeAppearance.SetDivisionTickMarkColor
  (const Value: TColor);
begin
  if FDivisionTickMarkColor <> Value then
  begin
    FDivisionTickMarkColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTimeLineRangeAppearance.SetDivisionTickMarkSize
  (const Value: Integer);
begin
  if FDivisionTickMarkSize <> Value then
  begin
    FDivisionTickMarkSize := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTimeLineRangeAppearance.SetDivisionTickMarkWidth
  (const Value: Integer);
begin
  if FDivisionTickMarkWidth <> Value then
  begin
    FDivisionTickMarkWidth := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTimeLineRangeAppearance.SetIndentSpacing
  (const Value: Integer);
begin
  if FIndentSpacing <> Value then
  begin
    FIndentSpacing := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTimeLineRangeAppearance.SetDivisionFont(const Value: TFont);
begin
  if FDivisionFont <> Value then
  begin
    FDivisionFont.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothTimeLineRangeAppearance.SetDivisionFormat
  (const Value: String);
begin
  if FDivisionFormat <> Value then
  begin
    FDivisionFormat := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTimeLineRangeAppearance.SetShowDivisionValue
  (const Value: Boolean);
begin
  if FShowDivisionValue <> Value then
  begin
    FShowDivisionValue := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTimeLineRangeAppearance.SetShowSubDivisionValue
  (const Value: Boolean);
begin
  if FShowSubDivisionValue <> Value then
  begin
    FShowSubDivisionValue := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTimeLineRangeAppearance.SetSubDivisionFont
  (const Value: TFont);
begin
  if FSubDivisionFont <> Value then
  begin
    FSubDivisionFont.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothTimeLineRangeAppearance.SetSubDivisionFormat
  (const Value: String);
begin
  if FSubDivisionFormat <> Value then
  begin
    FSubDivisionFormat := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTimeLineRangeAppearance.SetSubDivisionTickMarkColor
  (const Value: TColor);
begin
  if FSubDivisionTickMarkColor <> Value then
  begin
    FSubDivisionTickMarkColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTimeLineRangeAppearance.SetSubDivisionTickMarkSize
  (const Value: Integer);
begin
  if FSubDivisionTickMarkSize <> Value then
  begin
    FSubDivisionTickMarkSize := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTimeLineRangeAppearance.SetSubDivisionTickMarkWidth
  (const Value: Integer);
begin
  if FSubDivisionTickMarkWidth <> Value then
  begin
    FSubDivisionTickMarkWidth := Value;
    Changed;
  end;
end;

{ TAdvSmoothTimeLineBarSection }

procedure TAdvSmoothTimeLineBarSection.Assign(Source: TPersistent);
begin
  if (Source is TAdvSmoothTimeLineBarSection) then
  begin
    FEndTime := (Source as TAdvSmoothTimeLineBarSection).EndTime;
    FStartTime := (Source as TAdvSmoothTimeLineBarSection).StartTime;
    FFill.Assign((Source as TAdvSmoothTimeLineBarSection).Fill);
    FHintFill.Assign((Source as TAdvSmoothTimeLineBarSection).HintFill);
    FHintFont.Assign((Source as TAdvSmoothTimeLineBarSection).HintFont);
    FHintHeight := (Source as TAdvSmoothTimeLineBarSection).HintHeight;
    FHintWidth := (Source as TAdvSmoothTimeLineBarSection).HintWidth;
    FHintAutoSize := (Source as TAdvSmoothTimeLineBarSection).HintAutoSize;
    FFixedSize := (Source as TAdvSmoothTimeLineBarSection).FixedSize;
    FFixedPosition := (Source as TAdvSmoothTimeLineBarSection).FixedPosition;
    FHandleSize := (Source as TAdvSmoothTimeLineBarSection).HandleSize;
    FHandleColor := (Source as TAdvSmoothTimeLineBarSection).HandleColor;
    FCaptionTop := (Source as TAdvSmoothTimeLineBarSection).CaptionTop;
    FCaptionLeft := (Source as TAdvSmoothTimeLineBarSection).CaptionLeft;
    FCaption := (Source as TAdvSmoothTimeLineBarSection).Caption;
    FCaptionLocation := (Source as TAdvSmoothTimeLineBarSection)
      .CaptionLocation;
    Changed;
  end;
end;

procedure TAdvSmoothTimeLineBarSection.Changed;
begin
  FOwner.Changed;
end;

constructor TAdvSmoothTimeLineBarSection.Create(Collection: TCollection);
begin
  inherited;
  FOwner := (Collection as TAdvSmoothTimeLineBarSections).FOwner;
  FStartTime := IncSecond(FOwner.Range.RangeFrom, 3);
  FEndTime := IncSecond(FOwner.Range.RangeFrom, 5);
  FFill := TGDIPFill.Create;
  FFill.OnChange := FillChanged;
  FHintFill := TGDIPFill.Create;
  FHintFill.OnChange := FillChanged;
  FHintFont := TFont.Create;
  FHintFont.OnChange := FontChanged;
  FHintWidth := 50;
  FHintHeight := 20;
  FHintAutoSize := true;
  FHandleSize := 6;
  FHandleColor := clHighlight;
  FFixedSize := false;
  FFixedPosition := false;
  FCaptionTop := 0;
  FCaptionLeft := 0;
  FCaptionLocation := cpCenterLeft;

  if not(csLoading in FOwner.ComponentState) then
  begin
    FHintFill.Assign(FOwner.FDefaultHintFill);
    FFill.Assign(FOwner.FDefaultSectionFill);
  end;

  FOwner.BuildSections;
  FOwner.Changed;
end;

destructor TAdvSmoothTimeLineBarSection.Destroy;
begin
  FFill.Free;
  FHintFill.Free;
  FOwner.BuildSections;
  FOwner.Changed;
  FHintFont.Free;
  inherited;
end;

procedure TAdvSmoothTimeLineBarSection.FillChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothTimeLineBarSection.FontChanged(Sender: TObject);
begin
  Changed;
end;

function TAdvSmoothTimeLineBarSection.GetSectionRect: TGPRectF;
begin
  result := FSectionRect;
end;

procedure TAdvSmoothTimeLineBarSection.SetCaption(const Value: String);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTimeLineBarSection.SetCaptionLeft(const Value: Integer);
begin
  if FCaptionLeft <> Value then
  begin
    FCaptionLeft := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTimeLineBarSection.SetCaptionLocation
  (const Value: TAdvSmoothTimeLineCaptionLocation);
begin
  if FCaptionLocation <> Value then
  begin
    FCaptionLocation := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTimeLineBarSection.SetCaptionTop(const Value: Integer);
begin
  if FCaptionTop <> Value then
  begin
    FCaptionTop := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTimeLineBarSection.SetEndTime(const Value: TDateTime);
begin
  if FEndTime <> Value then
  begin
    FEndTime := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTimeLineBarSection.SetFill(const Value: TGDIPFill);
begin
  if FFill <> Value then
  begin
    FFill.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothTimeLineBarSection.SetFixedPosition(const Value: Boolean);
begin
  if FFixedPosition <> Value then
  begin
    FFixedPosition := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTimeLineBarSection.SetFixedSize(const Value: Boolean);
begin
  if FFixedSize <> Value then
  begin
    FFixedSize := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTimeLineBarSection.SetHandleColor(const Value: TColor);
begin
  if FHandleColor <> Value then
  begin
    FHandleColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTimeLineBarSection.SetHandleSize(const Value: Integer);
begin
  if FHandleSize <> Value then
  begin
    FHandleSize := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTimeLineBarSection.SetHint(const Value: String);
begin
  if FHint <> Value then
  begin
    FHint := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTimeLineBarSection.SetHintAutoSize(const Value: Boolean);
begin
  if FHintAutoSize <> Value then
  begin
    FHintAutoSize := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTimeLineBarSection.SetHintFill(const Value: TGDIPFill);
begin
  if FHintFill <> Value then
  begin
    FHintFill.Assign(Value);
    FillChanged(Self);
  end;
end;

procedure TAdvSmoothTimeLineBarSection.SetHintFont(const Value: TFont);
begin
  if FHintFont <> Value then
  begin
    FHintFont.Assign(Value);
    FontChanged(Self);
  end;
end;

procedure TAdvSmoothTimeLineBarSection.SetHintHeight(const Value: Integer);
begin
  if FHintHeight <> Value then
  begin
    FHintHeight := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTimeLineBarSection.SetHintWidth(const Value: Integer);
begin
  if FHintWidth <> Value then
  begin
    FHintWidth := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTimeLineBarSection.SetStartTime(const Value: TDateTime);
begin
  if FStartTime <> Value then
  begin
    FStartTime := Value;
    Changed;
  end;
end;

{ TAdvSmoothTimeLineBarSections }

function TAdvSmoothTimeLineBarSections.Add: TAdvSmoothTimeLineBarSection;
begin
  result := TAdvSmoothTimeLineBarSection( inherited Add);
end;

constructor TAdvSmoothTimeLineBarSections.Create(AOwner: TAdvSmoothTimeLine);
begin
  inherited Create(TAdvSmoothTimeLineBarSection);
  FOwner := AOwner;
end;

procedure TAdvSmoothTimeLineBarSections.Delete(Index: Integer);
begin
  inherited Items[Index].Free;
end;

function TAdvSmoothTimeLineBarSections.GetItem(Index: Integer)
  : TAdvSmoothTimeLineBarSection;
begin
  result := TAdvSmoothTimeLineBarSection( inherited Items[Index]);
end;

function TAdvSmoothTimeLineBarSections.GetOwner: TPersistent;
begin
  result := FOwner;
end;

function TAdvSmoothTimeLineBarSections.Insert(Index: Integer)
  : TAdvSmoothTimeLineBarSection;
begin
  result := TAdvSmoothTimeLineBarSection( inherited Insert(Index));
end;

procedure TAdvSmoothTimeLineBarSections.SetItem(Index: Integer;
  const Value: TAdvSmoothTimeLineBarSection);
begin
  inherited Items[Index] := Value;
end;

{ TAdvSmoothTimeLineBar }

procedure TAdvSmoothTimeLineBar.Assign(Source: TPersistent);
begin
  if (Source is TAdvSmoothTimeLineBar) then
  begin
    FFill.Assign((Source as TAdvSmoothTimeLineBar).Fill);
    FHeight := (Source as TAdvSmoothTimeLineBar).Height;
    FAnnotationFont.Assign((Source as TAdvSmoothTimeLineBar).AnnotationFont);
    FSectionCaptionFont.Assign((Source as TAdvSmoothTimeLineBar)
        .SectionCaptionFont);
    FAnnotationAutoPosition := (Source as TAdvSmoothTimeLineBar)
      .AnnotationAutoPosition;
    FOverlappingSections := (Source as TAdvSmoothTimeLineBar).OverlappingSections;
    Changed;
  end;
end;

procedure TAdvSmoothTimeLineBar.Changed;
begin
  FOwner.Changed;
end;

constructor TAdvSmoothTimeLineBar.Create(AOwner: TAdvSmoothTimeLine);
begin
  FOwner := AOwner;
  FFill := TGDIPFill.Create;
  FFill.OnChange := FillChanged;
  FSections := TAdvSmoothTimeLineBarSections.Create(FOwner);
  FSections.OnChange := SectionsChanged;
  FHeight := 10;
  FSectionCaptionFont := TFont.Create;
  FSectionCaptionFont.OnChange := FontChanged;
  FAnnotationFont := TFont.Create;
  FAnnotationFont.OnChange := FontChanged;
  FOverlappingSections := True;

  FAnnotationAutoPosition := apNone;
{$IFNDEF DELPHI9_LVL}
  FSectionCaptionFont.Name := 'Tahoma';
  FAnnotationFont.Name := 'Tahoma';
{$ENDIF}
end;

destructor TAdvSmoothTimeLineBar.Destroy;
begin
  FFill.Free;
  FSections.Free;
  FAnnotationFont.Free;
  FSectionCaptionFont.Free;
  inherited;
end;

procedure TAdvSmoothTimeLineBar.FillChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothTimeLineBar.FontChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothTimeLineBar.SectionsChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothTimeLineBar.SetAnnotationAutoPosition
  (const Value: TAnnotationAutoPosition);
begin
  if FAnnotationAutoPosition <> Value then
  begin
    FAnnotationAutoPosition := Value;
    FOwner.BuildAnnotations;
    Changed;
  end;
end;

procedure TAdvSmoothTimeLineBar.SetAnnotationFont(const Value: TFont);
begin
  if FAnnotationFont <> Value then
  begin
    FAnnotationFont.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothTimeLineBar.SetFill(const Value: TGDIPFill);
begin
  if FFill <> Value then
  begin
    FFill.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothTimeLineBar.SetHeight(const Value: Integer);
begin
  if FHeight <> Value then
  begin
    FHeight := Value;
    FOwner.BuildTimeLineValues;
    FOwner.BuildAnnotations;
    FOwner.BuildSections;
    Changed;
  end;
end;

procedure TAdvSmoothTimeLineBar.SetOverlappingSections(const Value: Boolean);
begin
  if FOverlappingSections <> Value then
  begin
    FOverlappingSections := Value;
    FOwner.BuildSections;
    Changed;
  end;
end;

procedure TAdvSmoothTimeLineBar.SetSectionCaptionFont(const Value: TFont);
begin
  if FSectionCaptionFont <> Value then
  begin
    FSectionCaptionFont.Assign(Value);
    Changed;
  end;
end;

{ TAdvSmoothSectionHint }

procedure TAdvSmoothSectionHint.ClearBuffer(Graphics: TGPGraphics);
var
  g: TGPGraphics;
begin
  g := Graphics;
  if not Assigned(g) then
    g := CreateGraphics;
  g.Clear($00000000);
  if not Assigned(Graphics) then
    g.Free;
end;

procedure TAdvSmoothSectionHint.CMDialogKey(var Msg: TWMKey);
begin

end;

procedure TAdvSmoothSectionHint.CMMouseLeave(var Message: TMessage);
begin
  invalidate;
end;

constructor TAdvSmoothSectionHint.Create(AOwner: TComponent);
begin
  inherited;
  FMainBuffer := nil;

end;

function TAdvSmoothSectionHint.CreateGraphics: TGPGraphics;
begin
  result := nil;
  if Assigned(FMainBuffer) then
    result := TGPGraphics.Create(FMainBuffer);
end;

procedure TAdvSmoothSectionHint.CreateMainBuffer;
begin
  // if not Assigned(FMainBuffer) then
  if Assigned(FMainBuffer) then
    FMainBuffer.Free;

  FMainBuffer := TGPBitmap.Create(Width, Height, PixelFormat32bppARGB);
end;

procedure TAdvSmoothSectionHint.CreateWnd;
begin
  inherited;
  UpdateWindow;
end;

destructor TAdvSmoothSectionHint.Destroy;
begin
  inherited;
  DestroyMainBuffer;
end;

procedure TAdvSmoothSectionHint.DestroyMainBuffer;
begin
  if Assigned(FMainBuffer) then
    FMainBuffer.Free;
end;

procedure TAdvSmoothSectionHint.Draw(Graphics: TGPGraphics);
var
  g: TGPGraphics;
begin
  g := Graphics;
  if not Assigned(g) then
    g := CreateGraphics;

  g.SetSmoothingMode(SmoothingModeAntiAlias);

  if Assigned(TimeLine) then
    TimeLine.DrawSectionHint(g);

  if not Assigned(Graphics) then
    g.Free;
end;

procedure TAdvSmoothSectionHint.Init;
begin
  CreateMainBuffer;
  SetLayeredWindow;
  UpdateLayered;
end;

procedure TAdvSmoothSectionHint.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;

end;

procedure TAdvSmoothSectionHint.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited;

end;

procedure TAdvSmoothSectionHint.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;

end;

procedure TAdvSmoothSectionHint.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;

end;

procedure TAdvSmoothSectionHint.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;

end;

procedure TAdvSmoothSectionHint.Paint;
begin
  inherited;

end;

procedure TAdvSmoothSectionHint.SetLayeredWindow;
begin
  if GetWindowLong(Handle, GWL_EXSTYLE) and WS_EX_LAYERED = 0 then
    SetWindowLong(Handle, GWL_EXSTYLE, GetWindowLong(Handle, GWL_EXSTYLE)
        or WS_EX_LAYERED);

  UpdateLayered;
end;

procedure TAdvSmoothSectionHint.UpdateButtons;
begin

end;

procedure TAdvSmoothSectionHint.UpdateLayered;
begin
  ClearBuffer(nil);

  SetWindowPos(Self.Handle, HWND_TOPMOST, 0, 0, 0, 0,
    SWP_NOMOVE or SWP_NOSIZE or SWP_FRAMECHANGED or SWP_NOACTIVATE);

  Draw(nil);

  UpdateMainWindow;
end;

procedure TAdvSmoothSectionHint.UpdateMainWindow;
var
  ScrDC, MemDC: HDC;
  BitmapHandle, PrevBitmap: HBITMAP;
  BlendFunc: _BLENDFUNCTION;
  Size: TSize;
  p, s: TPoint;
begin
  // while BlendFunc.SourceConstantAlpha < 255 do
  // begin
  ScrDC := CreateCompatibleDC(0);
  MemDC := CreateCompatibleDC(ScrDC);

  FMainBuffer.GetHBITMAP(0, BitmapHandle);
  PrevBitmap := SelectObject(MemDC, BitmapHandle);
  Size.cx := Width;
  Size.cy := Height;
  p := Point(Left, Top);
  s := Point(0, 0);

  with BlendFunc do
  begin
    BlendOp := AC_SRC_OVER;
    BlendFlags := 0;
    SourceConstantAlpha := 255;
    AlphaFormat := AC_SRC_ALPHA;
  end;

  UpdateLayeredWindow(Handle, ScrDC, @p, @Size, MemDC, @s, 0, @BlendFunc,
    ULW_ALPHA);

  SelectObject(MemDC, PrevBitmap);
  DeleteObject(BitmapHandle);

  DeleteDC(MemDC);
  DeleteDC(ScrDC);
  // end;
end;

procedure TAdvSmoothSectionHint.UpdateWindow;
begin
  CreateMainBuffer;
  UpdateLayered;
end;

procedure TAdvSmoothSectionHint.WMEraseBkGnd(var Message: TWMEraseBkGnd);
begin
  inherited;
end;

procedure TAdvSmoothSectionHint.WMNCHitTest(var Msg: TWMNCHitTest);
begin
  inherited;
end;

procedure TAdvSmoothSectionHint.WMPaint(var Message: TWMPaint);
begin
  inherited;
end;

{ TWinCtrl }

procedure TWinCtrl.PaintCtrls(DC: HDC; First: TControl);
begin
  inherited PaintControls(DC, First);
end;

{ TAdvSmoothTimeLineBarIndicators }

function TAdvSmoothTimeLineBarIndicators.Add: TAdvSmoothTimeLineBarIndicator;
begin
  result := TAdvSmoothTimeLineBarIndicator( inherited Add);
end;

constructor TAdvSmoothTimeLineBarIndicators.Create(AOwner: TAdvSmoothTimeLine);
begin
  inherited Create(TAdvSmoothTimeLineBarIndicator);
  FOwner := AOwner;
end;

procedure TAdvSmoothTimeLineBarIndicators.Delete(Index: Integer);
begin
  inherited Items[Index].Free;
end;

function TAdvSmoothTimeLineBarIndicators.GetItem(Index: Integer)
  : TAdvSmoothTimeLineBarIndicator;
begin
  result := TAdvSmoothTimeLineBarIndicator( inherited Items[Index]);
end;

function TAdvSmoothTimeLineBarIndicators.GetOwner: TPersistent;
begin
  result := FOwner;
end;

function TAdvSmoothTimeLineBarIndicators.Insert(Index: Integer)
  : TAdvSmoothTimeLineBarIndicator;
begin
  result := TAdvSmoothTimeLineBarIndicator( inherited Insert(Index));
end;

procedure TAdvSmoothTimeLineBarIndicators.SetItem(Index: Integer;
  const Value: TAdvSmoothTimeLineBarIndicator);
begin
  Items[Index] := Value;
end;

{ TAdvSmoothTimeLineBarIndicator }

procedure TAdvSmoothTimeLineBarIndicator.Assign(Source: TPersistent);
begin
  if (Source is TAdvSmoothTimeLineBarIndicator) then
  begin
    // FPosition := (Source as TAdvSmoothTimeLineBarIndicator).Position;
    FAnnotationTextColor := (Source as TAdvSmoothTimeLineBarIndicator)
      .AnnotationTextColor;
    FShape := (Source as TAdvSmoothTimeLineBarIndicator).Shape;
    FColor := (Source as TAdvSmoothTimeLineBarIndicator).Color;
    FColorTo := (Source as TAdvSmoothTimeLineBarIndicator).ColorTo;
    FOpacity := (Source as TAdvSmoothTimeLineBarIndicator).Opacity;
    FOpacityTo := (Source as TAdvSmoothTimeLineBarIndicator).OpacityTo;
    FAngle := (Source as TAdvSmoothTimeLineBarIndicator).Angle;
    FGradientType := (Source as TAdvSmoothTimeLineBarIndicator).GradientType;
    FBorderColor := (Source as TAdvSmoothTimeLineBarIndicator).BorderColor;
    FBorderOpacity := (Source as TAdvSmoothTimeLineBarIndicator).Opacity;
    FBorderWidth := (Source as TAdvSmoothTimeLineBarIndicator).BorderWidth;
    FPicture.Assign((Source as TAdvSmoothTimeLineBarIndicator).Picture);
    FSize := (Source as TAdvSmoothTimeLineBarIndicator).Size;
    FHint := (Source as TAdvSmoothTimeLineBarIndicator).Hint;
    FShowTickMark := (Source as TAdvSmoothTimeLineBarIndicator).ShowTickMark;
    FTickMarkSize := (Source as TAdvSmoothTimeLineBarIndicator).TickMarkSize;
    FTickMarkWidth := (Source as TAdvSmoothTimeLineBarIndicator).TickMarkWidth;
    FTickMarkColor := (Source as TAdvSmoothTimeLineBarIndicator).TickMarkColor;
    FFixed := (Source as TAdvSmoothTimeLineBarIndicator).Fixed;
    FAnnotation := (Source as TAdvSmoothTimeLineBarIndicator).Annotation;
    FAnnotationColor := (Source as TAdvSmoothTimeLineBarIndicator)
      .AnnotationColor;
    FAnnotationPosition := (Source as TAdvSmoothTimeLineBarIndicator)
      .AnnotationPosition;
    FAnnotationImageIndex := (Source as TAdvSmoothTimeLineBarIndicator)
      .AnnotationImageIndex;
    FAnnotationRounded := (Source as TAdvSmoothTimeLineBarIndicator)
      .AnnotationRounded;
    FAnnotationLineColor := (Source as TAdvSmoothTimeLineBarIndicator)
      .AnnotationLineColor;
    FAnnotationOpacity := (Source as TAdvSmoothTimeLineBarIndicator)
      .AnnotationOpacity;
    FAnnotationLineOpacity := (Source as TAdvSmoothTimeLineBarIndicator)
      .AnnotationLineOpacity;
    Changed;
  end;
end;

procedure TAdvSmoothTimeLineBarIndicator.AssignVisuals(Source: TPersistent);
begin
  if (Source is TAdvSmoothTimeLineBarIndicator) then
  begin
    FShape := (Source as TAdvSmoothTimeLineBarIndicator).Shape;
    FColor := (Source as TAdvSmoothTimeLineBarIndicator).Color;
    FColorTo := (Source as TAdvSmoothTimeLineBarIndicator).ColorTo;
    FOpacity := (Source as TAdvSmoothTimeLineBarIndicator).Opacity;
    FOpacityTo := (Source as TAdvSmoothTimeLineBarIndicator).OpacityTo;
    FAngle := (Source as TAdvSmoothTimeLineBarIndicator).Angle;
    FGradientType := (Source as TAdvSmoothTimeLineBarIndicator).GradientType;
    FBorderColor := (Source as TAdvSmoothTimeLineBarIndicator).BorderColor;
    FBorderOpacity := (Source as TAdvSmoothTimeLineBarIndicator).Opacity;
    FBorderWidth := (Source as TAdvSmoothTimeLineBarIndicator).BorderWidth;
    FPicture.Assign((Source as TAdvSmoothTimeLineBarIndicator).Picture);
    FSize := (Source as TAdvSmoothTimeLineBarIndicator).Size;
    FHint := (Source as TAdvSmoothTimeLineBarIndicator).Hint;
    FShowTickMark := (Source as TAdvSmoothTimeLineBarIndicator).ShowTickMark;
    FTickMarkSize := (Source as TAdvSmoothTimeLineBarIndicator).TickMarkSize;
    FTickMarkWidth := (Source as TAdvSmoothTimeLineBarIndicator).TickMarkWidth;
    FTickMarkColor := (Source as TAdvSmoothTimeLineBarIndicator).TickMarkColor;
    FAnnotationColor := (Source as TAdvSmoothTimeLineBarIndicator)
      .AnnotationColor;
    FAnnotationRounded := (Source as TAdvSmoothTimeLineBarIndicator)
      .AnnotationRounded;
    FAnnotationLineColor := (Source as TAdvSmoothTimeLineBarIndicator)
      .AnnotationLineColor;
    FAnnotationOpacity := (Source as TAdvSmoothTimeLineBarIndicator)
      .AnnotationOpacity;
    FAnnotationLineOpacity := (Source as TAdvSmoothTimeLineBarIndicator)
      .AnnotationLineOpacity;
    Changed;
  end;
end;

function TAdvSmoothTimeLineBarIndicator.CalculateAnnotation
  (g: TGPGraphics; f: TGPFont; sf: TGPStringFormat; I: Integer): TGPRectF;
var
  rind: TGPRectF;
  htext, wtext: Double;
  sizer: TGPRectF;
  rtime: TGPRectF;
  RectChanged: Boolean;
  K: Integer;
  raprev: TGPRectF;
begin
  result := MakeRect(-1, -1, -1, -1);
  if (Position >= FOwner.Range.RangeFrom) and
    (Position <= FOwner.Range.RangeTo) then
  begin
    rtime := FOwner.GetTimeLineBarRect;
    rind := GetIndicatorRect;
    // Annotation
    htext := 0;
    wtext := 0;
    if Annotation <> '' then
    begin
      g.MeasureString(Annotation, Length(Annotation), f, MakeRect
          (0, 0, 10000, 1000), sf, sizer);
      htext := sizer.Height + 5;
      wtext := sizer.Width + 10;
    end;

    if Assigned(FOwner.ImageList) then
    begin
      if (AnnotationImageIndex >= 0) and
        (AnnotationImageIndex <= FOwner.ImageList.Count - 1) then
      begin
        if htext < FOwner.ImageList.Height + 5 then
          htext := FOwner.ImageList.Height + 5;

        if Annotation <> '' then
          wtext := wtext + FOwner.ImageList.Width + 5
        else
          wtext := wtext + FOwner.ImageList.Width + 10
      end;
    end;

    case AnnotationPosition of
      apOnTop:
        begin
          if rind.X < rtime.X + rtime.Width / 2 then
            result := MakeRect(rind.X + rind.Width / 2 + 5,
              rind.Y - 15 - htext, wtext, htext)
          else
            result := MakeRect(rind.X + rind.Width / 2 - 5 - wtext,
              rind.Y - 15 - htext, wtext, htext);

          if I > 0 then
          begin
            repeat
              RectChanged := false;
              for K := I - 1 downto 0 do
              begin
                raprev := FOwner.FAnnotationListTop.GetItem(K).AnnotationRect;

                while RectanglesInterSect
                  (MakeRect(raprev.X - 2, raprev.Y, raprev.Width + 4,
                    raprev.Height + 4), MakeRect(result.X - 2, result.Y - 2,
                    result.Width + 4, result.Height + 4)) do
                begin
                  result := MakeRect(result.X, result.Y - 15, result.Width,
                    result.Height);

                  if  (Result.Y - 5 < FOwner.ClientRect.Top) and (FOwner.TimeLineBar.AnnotationAutoPosition <> apNone) then
                  begin
                    case FOwner.TimeLineBar.AnnotationAutoPosition of
                      apLeft: Result := MakeRect(Result.X - 15, rind.Y - rtime.Height - 15 - ((FOwner.GetTotalIndent + 1) * FOwner.RangeAppearance.IndentSpacing), Result.Width, Result.Height);
                      apRight: Result := MakeRect(Result.X + 15, rind.Y - rtime.Height - 15 - ((FOwner.GetTotalIndent + 1) * FOwner.RangeAppearance.IndentSpacing), Result.Width, Result.Height);
                    end;
                  end;
                  RectChanged := true;
                end;
              end;
            until (not RectChanged);
          end;
        end;
      apAtBottom:
        begin
          if rind.X < rtime.X + rtime.Width / 2 then
            result := MakeRect(rind.X + rind.Width / 2 + 5,
              rind.Y + rind.Height + 15 + ((FOwner.GetTotalIndent + 1)
                  * FOwner.RangeAppearance.IndentSpacing), wtext, htext)
          else
            result := MakeRect(rind.X + rind.Width / 2 - wtext - 5,
              rind.Y + rind.Height + 15 + ((FOwner.GetTotalIndent + 1)
                  * FOwner.RangeAppearance.IndentSpacing), wtext, htext);

          if I <> 0 then
          begin
            repeat
              RectChanged := false;
              for K := I - 1 downto 0 do
              begin
                raprev := FOwner.FAnnotationListBottom.GetItem(K)
                  .AnnotationRect;

                while RectanglesInterSect
                  (MakeRect(raprev.X - 2, raprev.Y, raprev.Width + 4,
                    raprev.Height + 4), MakeRect(result.X - 2, result.Y - 2,
                    result.Width + 4, result.Height + 4)) do
                begin
                  result := MakeRect(result.X, result.Y + 15, result.Width,
                    result.Height);
                  if (Result.Y + Result.Height + 5 > FOwner.ClientRect.Bottom) and (FOwner.TimeLineBar.AnnotationAutoPosition <> apNone) then
                  begin
                    case FOwner.TimeLineBar.AnnotationAutoPosition of
                      apLeft: Result := MakeRect(Result.X - 15, rind.Y + rind.Height + 15 + ((FOwner.GetTotalIndent + 1) * FOwner.RangeAppearance.IndentSpacing), Result.Width, Result.Height);
                      apRight: Result := MakeRect(Result.X + 15, rind.Y + rind.Height + 15 + ((FOwner.GetTotalIndent + 1) * FOwner.RangeAppearance.IndentSpacing), Result.Width, Result.Height);
                    end;
                  end;
                  RectChanged := true;
                end;
              end;
            until (not RectChanged);
          end;
        end;
    end;
  end;
end;

procedure TAdvSmoothTimeLineBarIndicator.Changed;
begin
  FOwner.TimeLineBarIndicatorsChanged(Self);
end;

constructor TAdvSmoothTimeLineBarIndicator.Create(Collection: TCollection);
begin
  inherited;
  FOwner := (Collection as TAdvSmoothTimeLineBarIndicators).FOwner;
  FPosition := IncSecond(FOwner.Range.RangeFrom, 5);
  FShape := isDiamond;
  FColor := clGray;
  FColorTo := clsilver;
  FOpacity := 255;
  FOpacityTo := 255;
  FAngle := 0;
  FGradientType := gtForwardDiagonal;
  FBorderColor := clBlack;
  FBorderOpacity := 255;
  FBorderWidth := 1;
  FPicture := TAdvGDIPPicture.Create;
  FPicture.OnChange := PictureChanged;
  FSize := 15;
  FTickMarkSize := 10;
  FFixed := false;
  FTickMarkWidth := 1;
  FTickMarkColor := clBlack;
  FShowTickMark := true;
  FAnnotationColor := clsilver;
  FAnnotationPosition := apOnTop;
  FAnnotationLineColor := clBlack;
  FAnnotationOpacity := 255;
  FAnnotationLineOpacity := 255;
  FAnnotationImageIndex := -1;
  FAnnotationRounded := false;
  FAnnotationTextColor := clNone;
  if Assigned(FOwner.FDefaultIndicator) then
    Assign(FOwner.FDefaultIndicator);

  Changed;
end;

destructor TAdvSmoothTimeLineBarIndicator.Destroy;
begin
  FPicture.Free;
  inherited;
  Changed;
end;

procedure TAdvSmoothTimeLineBarIndicator.DrawAnnotation
  (g: TGPGraphics; f: TGPFont; sf: TGPStringFormat; bText: TGPSolidBrush;
  r: TGPRectF);
var
  b: TGPSolidBrush;
  bl: TGPLinearGradientBrush;
  ca: TCanvas;
  cahdl: THandle;
  p: TGPPen;
  imageindent: Integer;
  rover: TGPRectF;
  rind: TGPRectF;
  path: TGPGraphicsPath;
begin
  if (Position >= FOwner.Range.RangeFrom) and
    (Position <= FOwner.Range.RangeTo) then
  begin
    rind := GetIndicatorRect;
    if AnnotationColor <> clNone then
    begin
      b := TGPSolidBrush.Create(MakeColor(AnnotationOpacity, AnnotationColor));
      if AnnotationRounded then
      begin
        path := GDIPFill.CreateRoundRectangle(r, 5, rtBoth, false);
        g.Fillpath(b, path);
        path.Free;
      end
      else
        g.FillRectangle(b, r);

      b.Free;

      p := TGPPen.Create(MakeColor(AnnotationLineOpacity, AnnotationLineColor));
      if AnnotationRounded then
      begin
        path := GDIPFill.CreateRoundRectangle(r, 5, rtBoth, false);
        g.DrawPath(p, path);
        path.Free;
      end
      else
        g.DrawRectangle(p, r);

      p.Free;
    end;

    imageindent := 0;
    if Assigned(FOwner.ImageList) then
    begin
      if (AnnotationImageIndex >= 0) and
        (AnnotationImageIndex <= FOwner.ImageList.Count - 1) then
      begin
        cahdl := g.GetHDC;
        ca := TCanvas.Create;
        ca.Handle := cahdl;
        if r.X < rind.X then
        begin
          FOwner.ImageList.Draw
            (ca, Round(r.X + r.Width - 5 - FOwner.ImageList.Width), Round
              (r.Y + (r.Height - FOwner.ImageList.Height) / 2),
            AnnotationImageIndex);
        end
        else
        begin
          FOwner.ImageList.Draw(ca, Round(r.X + 5), Round
              (r.Y + (r.Height - FOwner.ImageList.Height) / 2),
            AnnotationImageIndex);
        end;
        imageindent := 5 + FOwner.ImageList.Width;

        ca.Free;
        g.ReleaseHDC(cahdl);
      end;
    end;

    if r.X < rind.X then
      g.DrawString(Annotation, Length(Annotation), f, MakeRect
          (r.X + 5, r.Y, r.Width, r.Height), sf, bText)
    else
      g.DrawString(Annotation, Length(Annotation), f, MakeRect
          (r.X + 5 + imageindent, r.Y, r.Width, r.Height), sf, bText);

    if AnnotationColor <> clNone then
    begin
      rover := MakeRect(r.X, r.Y, r.Width, r.Height / 2);
      bl := TGPLinearGradientBrush.Create
        (MakeRect(rover.X - 1, rover.Y - 1, rover.Width + 2, rover.Height + 2),
        MakeColor(75, clWhite), MakeColor(20, clWhite),
        LinearGradientModeVertical);
      if AnnotationRounded then
      begin
        path := GDIPFill.CreateRoundRectangle(rover, 5, rtBoth, false);
        g.Fillpath(bl, path);
        path.Free;
      end
      else
        g.FillRectangle(bl, rover);

      bl.Free;
    end;
  end;
end;

procedure TAdvSmoothTimeLineBarIndicator.DrawAnnotationLine
  (g: TGPGraphics; r: TGPRectF;
  AnnotationPosition: TAdvSmoothTimeLineBarAnnotationPosition);
var
  rind: TGPRectF;
  rtime: TGPRectF;
  p: TGPPen;
begin
  if (Position >= FOwner.Range.RangeFrom) and
    (Position <= FOwner.Range.RangeTo) then
  begin
    rtime := FOwner.GetTimeLineBarRect;
    rind := GetIndicatorRect;
    p := TGPPen.Create(MakeColor(AnnotationLineOpacity, AnnotationLineColor));
    case AnnotationPosition of
      apOnTop:
        g.DrawLine(p, rind.X + rind.Width / 2, rtime.Y,
          rind.X + rind.Width / 2, r.Y + r.Height / 2);
      apAtBottom:
        g.DrawLine(p, rind.X + rind.Width / 2, rtime.Y + rtime.Height,
          rind.X + rind.Width / 2, r.Y + r.Height / 2);
    end;
    if r.X < rind.X then
      g.DrawLine(p, rind.X + rind.Width / 2, r.Y + r.Height / 2, r.X + r.Width,
        r.Y + r.Height / 2)
    else
      g.DrawLine(p, rind.X + rind.Width / 2, r.Y + r.Height / 2, r.X,
        r.Y + r.Height / 2);

    p.Free;
  end;
end;

function TAdvSmoothTimeLineBarIndicator.GetAnnotationRect: TGPRectF;
var
  I: Integer;
begin
  for I := 0 to FOwner.FAnnotationListTop.Count - 1 do
  begin
    if FOwner.FAnnotationListTop.Items[i].indicator = self then
    begin
      Result := FOwner.FAnnotationListTop.Items[i].AnnotationRect;
      break;
    end;
  end;

  for I := 0 to FOwner.FAnnotationListBottom.Count - 1 do
  begin
    if FOwner.FAnnotationListBottom.Items[i].indicator = self then
    begin
      Result := FOwner.FAnnotationListBottom.Items[i].AnnotationRect;
      break;
    end;
  end;
end;

function TAdvSmoothTimeLineBarIndicator.GetIndicatorRect: TGPRectF;
var
  r: TGPRectF;
begin
  r := FOwner.GetTimeLineBarRect;
  if FOwner.FMouseMovedOnIndicator and FOwner.FMouseDownOnIndicator and
    (Index = FOwner.FSelectedIndicator) and not Fixed then
    result.X := FOwner.FPx - Size / 2
  else
    result.X := FOwner.DateTimeToX(Position) - Size / 2;

  result.X := max(r.X - Size / 2, min(result.X, r.X + r.Width - Size / 2));

  result.Y := r.Y + r.Height / 2 - Size / 2;
  result.Width := Size;
  result.Height := Size;
end;

function TAdvSmoothTimeLineBarIndicator.GetPosition: TDateTime;
begin
  if FOwner.FMouseMovedOnIndicator and FOwner.FMouseDownOnIndicator and
    (Index = FOwner.FSelectedIndicator) and not Fixed then
    result := FOwner.XToDateTime(Round(FOwner.FPx - Size / 2))
  else
    result := FPosition;
end;

procedure TAdvSmoothTimeLineBarIndicator.PictureChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothTimeLineBarIndicator.SetAngle(const Value: Integer);
begin
  if FAngle <> Value then
  begin
    FAngle := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTimeLineBarIndicator.SetAnnotation(const Value: String);
begin
  if FAnnotation <> Value then
  begin
    FAnnotation := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTimeLineBarIndicator.SetAnnotationColor
  (const Value: TColor);
begin
  if FAnnotationColor <> Value then
  begin
    FAnnotationColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTimeLineBarIndicator.SetAnnotationImageIndex
  (const Value: Integer);
begin
  if FAnnotationImageIndex <> Value then
  begin
    FAnnotationImageIndex := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTimeLineBarIndicator.SetAnnotationLineColor
  (const Value: TColor);
begin
  if FAnnotationLineColor <> Value then
  begin
    FAnnotationLineColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTimeLineBarIndicator.SetAnnotationLineOpacity
  (const Value: Byte);
begin
  if FAnnotationLineOpacity <> Value then
  begin
    FAnnotationLineOpacity := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTimeLineBarIndicator.SetAnnotationOpacity
  (const Value: Byte);
begin
  if FAnnotationOpacity <> Value then
  begin
    FAnnotationOpacity := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTimeLineBarIndicator.SetAnnotationPostion
  (const Value: TAdvSmoothTimeLineBarAnnotationPosition);
begin
  if FAnnotationPosition <> Value then
  begin
    FAnnotationPosition := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTimeLineBarIndicator.SetAnnotationRounded
  (const Value: Boolean);
begin
  if FAnnotationRounded <> Value then
  begin
    FAnnotationRounded := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTimeLineBarIndicator.SetAnnotationTextColor
  (const Value: TColor);
begin
  if FAnnotationTextColor <> Value then
  begin
    FAnnotationTextColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTimeLineBarIndicator.SetBorderColor(const Value: TColor);
begin
  if FBorderColor <> Value then
  begin
    FBorderColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTimeLineBarIndicator.SetBorderOpacity(const Value: Byte);
begin
  if FBorderOpacity <> Value then
  begin
    FBorderOpacity := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTimeLineBarIndicator.SetBorderWidth(const Value: Integer);
begin
  if FBorderWidth <> Value then
  begin
    FBorderWidth := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTimeLineBarIndicator.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTimeLineBarIndicator.SetColorTo(const Value: TColor);
begin
  if FColorTo <> Value then
  begin
    FColorTo := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTimeLineBarIndicator.SetFixed(const Value: Boolean);
begin
  if FFixed <> Value then
  begin
    FFixed := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTimeLineBarIndicator.SetGradientType
  (const Value: TAdvGradientType);
begin
  if FGradientType <> Value then
  begin
    FGradientType := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTimeLineBarIndicator.SetHint(const Value: String);
begin
  if FHint <> Value then
  begin
    FHint := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTimeLineBarIndicator.SetOpacity(const Value: Byte);
begin
  if FOpacity <> Value then
  begin
    FOpacity := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTimeLineBarIndicator.SetOpacityTo(const Value: Byte);
begin
  if FOpacityTo <> Value then
  begin
    FOpacityTo := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTimeLineBarIndicator.SetPicture
  (const Value: TAdvGDIPPicture);
begin
  if FPicture <> Value then
  begin
    FPicture.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothTimeLineBarIndicator.SetPosition(const Value: TDateTime);
begin
  FPosition := Value;
  Changed;
end;

procedure TAdvSmoothTimeLineBarIndicator.SetShape
  (const Value: TAdvSmoothTimeLineBarIndicatorShape);
begin
  if FShape <> Value then
  begin
    FShape := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTimeLineBarIndicator.SetShowTickMark(const Value: Boolean);
begin
  if FShowTickMark <> Value then
  begin
    FShowTickMark := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTimeLineBarIndicator.SetSize(const Value: Integer);
begin
  if FSize <> Value then
  begin
    FSize := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTimeLineBarIndicator.SetTickMarkColor(const Value: TColor);
begin
  if FTickMarkColor <> Value then
  begin
    FTickMarkColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTimeLineBarIndicator.SetTickMarkSize(const Value: Integer);
begin
  if FTickMarkSize <> Value then
  begin
    FTickMarkSize := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTimeLineBarIndicator.SetTickMarkWidth(const Value: Integer);
begin
  if FTickMarkWidth <> Value then
  begin
    FTickMarkWidth := Value;
    Changed;
  end;
end;

{ TAdvSmoothTimeLineAnnotationList }

function TAdvSmoothTimeLineAnnotationList.Add: TAdvSmoothTimeLineAnnotationItem;
begin
  result := TAdvSmoothTimeLineAnnotationItem( inherited Add);
end;

procedure TAdvSmoothTimeLineAnnotationList.Clear;
begin
  if Count > 0 then
  begin
    BeginUpdate;
    try
      while Count > 0 do
      begin
        TAdvSmoothTimeLineAnnotationItem(Items[Count - 1]).indicator := nil;
        TCollectionItem(Items[Count - 1]).Free;
      end;
    finally
      EndUpdate;
    end;
  end;
end;

function TAdvSmoothTimeLineAnnotationList.Compare
  (Item1, Item2: TAdvSmoothTimeLineAnnotationItem): Integer;
var
  pos1, pos2: Double;
begin
  pos1 := Item1.indicator.Position;
  pos2 := Item2.indicator.Position;

  if pos2 < pos1 then
    result := -1
  else if pos2 > pos1 then
    result := 1
  else
    result := 0;
end;

constructor TAdvSmoothTimeLineAnnotationList.Create(AOwner: TAdvSmoothTimeLine);
begin
  inherited Create(TAdvSmoothTimeLineAnnotationItem);
  FOwner := AOwner;
end;

procedure TAdvSmoothTimeLineAnnotationList.Delete(Index: Integer);
begin
  Items[Index].Free;
end;

function TAdvSmoothTimeLineAnnotationList.GetItem(index: Integer)
  : TAdvSmoothTimeLineAnnotationItem;
begin
  result := TAdvSmoothTimeLineAnnotationItem( inherited Items[Index]);
end;

function TAdvSmoothTimeLineAnnotationList.Insert(Index: Integer)
  : TAdvSmoothTimeLineAnnotationItem;
begin
  result := TAdvSmoothTimeLineAnnotationItem( inherited Insert(Index));
end;

procedure TAdvSmoothTimeLineAnnotationList.QuickSort(L, r: Integer);
var
  I, J, p: Integer;
  Save: TCollectionItem;
  {$IFDEF DELPHIXE3_LVL}
  SortList: TList<TCollectionItem>;
  {$ELSE}
  SortList: TList;
  {$ENDIF}
begin
  // This cast allows us to get at the private elements in the base class
  SortList := TShadowedCollection(Self).FItems;

  repeat
    I := L;
    J := r;
    p := (L + r) shr 1;
    repeat
      while Compare(Items[I], Items[p]) < 0 do
        Inc(I);
      while Compare(Items[J], Items[p]) > 0 do
        dec(J);
      if I <= J then
      begin
        Save := SortList.Items[I];
        SortList.Items[I] := SortList.Items[J];
        SortList.Items[J] := Save;
        if p = I then
          p := J
        else if p = J then
          p := I;
        Inc(I);
        dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(L, J);
    L := I;
  until I >= r;
end;

procedure TAdvSmoothTimeLineAnnotationList.SetItem(Index: Integer;
  const Value: TAdvSmoothTimeLineAnnotationItem);
begin
  inherited Items[Index] := Value;
end;

end.
