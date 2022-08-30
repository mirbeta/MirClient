{ ************************************************************************** }
{ TAdvSmoothSlideShow component                                              }
{ for Delphi & C++Builder                                                    }
{                                                                            }
{ written                                                                    }
{ TMS Software                                                               }
{ copyright © 2013 - 2015                                                    }
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

unit AdvSmoothSlideShow;

{$I TMSDEFS.INC}

{$IFDEF BCB}
{$HPPEMIT '#include <shobjidl.h>'}
{$ENDIF}

interface

uses
  Windows, Forms, Classes, Messages, Controls, GDIPFill,
  ComObj, ActiveX, ShlObj, AdvStyleIF,
  AdvGDIP,
  Graphics, ExtCtrls, Math, SysUtils, Types;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 4; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 0; // Build nr.

  // version history
  // v0.9.0.0 : first beta release
  // v1.0.0.0 : first release
  // v1.0.1.0 : Built in support for Office 2010 colors
  //          : Fixed : Issue with C++Builder and shobjidl.h include file
  //          : Fixed : Issue with adding items at designtime with the image property
  // v1.0.1.1 : Fixed : Issue in older delphi versions with TMargins
  // v1.0.2.0 : New : Property ShowDescription
  // v1.0.3.0 : New : Property StretchPercentage in combination with Stretch item mode
  // v1.0.3.1 : Improved : Custom drawing on bitmap for controls
  // v1.0.3.2 : Fixed : Issue with default fill values
  // v1.1.0.0 : New : Metro Style Support
  //          : Fixed : Issue with tif file support
  // v1.1.0.1 : Improved: PlaceHolder and CurrentPlaceHolder rectangles set public
  //          : Fixed: Issue with Animation property disabling timer
  // v1.2.0.0 : New : Windows 8, Office 2013 styles added
  // v1.3.0.0 : New : Events when animation of a placeholder / image is processing / finished
  //          : New : OnDrawPlaceHolder event
  // v1.3.0.1 : Fixed : cached bitmap memory leak
  // v1.4.0.0 : New : Windows 10, Office 2016 styles added

type
  TAdvSmoothSlideShow = class;

  TAdvSmoothSlideShowItem = class;
{$IFNDEF DELPHI2006_LVL}
  TVerticalAlignment = (taAlignTop, taAlignBottom, taVerticalCenter);

  TMargins = class(TPersistent)
  private
    FOwner: TAdvSmoothSlideShow;
    FRight: integer;
    FBottom: integer;
    FTop: integer;
    FLeft: integer;
    FOnChange: TNotifyEvent;
    procedure SetBottom(const Value: integer);
    procedure SetLeft(const Value: integer);
    procedure SetRight(const Value: integer);
    procedure SetTop(const Value: integer);
  protected
    procedure Changed;
  public
    constructor Create(AOwner: TAdvSmoothSlideShow);
    procedure Assign(Source: TPersistent); override;
  published
    property Left: integer read FLeft write SetLeft;
    property Top: integer read FTop write SetTop;
    property Right: integer read FRight write SetRight;
    property Bottom: integer read FBottom write SetBottom;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;
{$ENDIF}

  TImageLoaderThread = class(TThread)
  private
    ItemIndex: Integer;
    FCtrl: TWinControl;
    FCtrlBitmap: TBitmap;
{$IFNDEF BCB}
{$IFDEF DELPHI_TOUCH}
    fMalloc: IMalloc;
    fRunnableTask: IRunnableTask;
{$ENDIF}
{$ENDIF}
    SlideShow: TAdvSmoothSlideShow;
  protected
    procedure Execute; override;
    procedure PaintControl;
{$IFNDEF BCB}
{$IFDEF DELPHI_TOUCH}
    function GetFilePreview(const FileName: string; w, h: integer;
      out bmp: TBitmap; out ErrorInfo: string): boolean;
{$ENDIF}
{$ENDIF}
  public
    constructor Create(ASlideShow: TAdvSmoothSlideShow);
  end;

  TImageThreadStatus = (stNone, stInitialized, stLoading, stLoaded);
{$IFNDEF BCB}
{$IFDEF DELPHI_TOUCH}
  TSlideShowItemKind = (ikImage, ikControl, ikPreview, ikText, ikImageText,
    ikCustom);
{$ELSE}
  TSlideShowItemKind = (ikImage, ikControl, ikText, ikImageText, ikCustom);
{$ENDIF}
{$ELSE}
  TSlideShowItemKind = (ikImage, ikControl, ikText, ikImageText, ikCustom);
{$ENDIF}
  TSlideShowTextLocation = (tlLeft, tlTop, tlRight, tlBottom);

  TDescriptionPosition = (dpTopLeft, dpTopCenter, dpTopRight, dpBottomLeft,
    dpBottomCenter, dpBottomRight);

  TAdvSmoothSlideShowItem = class(TCollectionItem)
  private
    FThumbThreadStatus: TImageThreadStatus;
    FThreadStatus: TImageThreadStatus;
    FThumbnail: TGPImage;
    FOriginalImage: TGPBitmap;
    FItemRect: TGPRectF;
    FPlaceHolderRect: TGPRectF;
    FThumbRect, FThumbItemRect: TGPRectF;
    FOwner: TAdvSmoothSlideShow;
    FImage: TAdvGDIPPicture;
    FLocation: String;
    FControl: TWinControl;
    FKind: TSlideShowItemKind;
    FEnabled: boolean;
    FVisible: boolean;
    FObject: TObject;
    FTag: integer;
    FText: String;
    FHint: String;
    FCustomImageHeight: integer;
    FCustomImageWidth: integer;
    FTextFont: TFont;
    FTextWidth: integer;
    FTextHeight: integer;
    FTextAutoSize: boolean;
    FEllipsis: boolean;
    FWordWrap: boolean;
    FTextLocation: TSlideShowTextLocation;
    FTextVerticalAlignment: TVerticalAlignment;
    FTextHorizontalAlignment: TAlignment;
    FURL: String;
    FDescriptionPosition: TDescriptionPosition;
    FDescription: String;
    FDescriptionFont: TFont;
    procedure SetImage(const Value: TAdvGDIPPicture);
    procedure SetLocation(const Value: String);
    procedure SetControl(const Value: TWinControl);
    procedure SetKind(const Value: TSlideShowItemKind);
    procedure SetEnabled(const Value: boolean);
    procedure SetVisible(const Value: boolean);
    procedure SetText(const Value: String);
    procedure SetHint(const Value: String);
    procedure SetTextFont(const Value: TFont);
    procedure SetTextWidth(const Value: integer);
    procedure SetTextAutoSize(const Value: boolean);
    procedure SetEllipsis(const Value: boolean);
    procedure SetWordWrap(const Value: boolean);
    procedure SetTextHeight(const Value: integer);
    procedure SetTextLocation(const Value: TSlideShowTextLocation);
    procedure SetTextHorizontalAlignment(const Value: TAlignment);
    procedure SetTextVerticalAlignment(const Value: TVerticalAlignment);
    procedure SetURL(const Value: String);
    procedure SetDescription(const Value: String);
    procedure SetDescriptionPosition(const Value: TDescriptionPosition);
    procedure SetDescriptionFont(const Value: TFont);
  protected
    procedure Changed;
    procedure ImageChanged(Sender: TObject);
    procedure DrawPlaceHolder(g: TGPGraphics; R: TGPRectF; Opacity: Byte);
    procedure DrawItem(g: TGPGraphics; R: TGPRectF; Opacity: Byte);
    procedure DrawDescription(g: TGPGraphics; R: TGPRectF; Opacity: Byte);
    procedure GetAspectSize(var w, h: Double; ow, oh, nw, nh: Double; f: Integer);
  public
    property PlaceHolderRect: TGPRectF read FPlaceHolderRect;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Update;
    property CustomImageWidth
      : integer read FCustomImageWidth write FCustomImageWidth default 100;
    property CustomImageHeight
      : integer read FCustomImageHeight write FCustomImageHeight default 100;
  published
    property Image: TAdvGDIPPicture read FImage write SetImage;
    property Location: String read FLocation write SetLocation;
    property Kind: TSlideShowItemKind read FKind write SetKind default ikImage;
    property Control: TWinControl read FControl write SetControl;
    property Enabled: boolean read FEnabled write SetEnabled default true;
    property Visible: boolean read FVisible write SetVisible default true;
    property Tag: integer read FTag write FTag;
    property ItemObject: TObject read FObject write FObject;
    property Text: String read FText write SetText;
    property Description: String read FDescription write SetDescription;
    property DescriptionPosition
      : TDescriptionPosition read FDescriptionPosition write
      SetDescriptionPosition default dpBottomCenter;
    property DescriptionFont
      : TFont read FDescriptionFont write SetDescriptionFont;
    property Hint: String read FHint write SetHint;
    property TextFont: TFont read FTextFont write SetTextFont;
    property TextWidth: integer read FTextWidth write SetTextWidth default 100;
    property TextHeight
      : integer read FTextHeight write SetTextHeight default 100;
    property TextAutoSize
      : boolean read FTextAutoSize write SetTextAutoSize default true;
    property TextHorizontalAlignment
      : TAlignment read FTextHorizontalAlignment write
      SetTextHorizontalAlignment default taCenter;
    property TextVerticalAlignment
      : TVerticalAlignment read FTextVerticalAlignment write
      SetTextVerticalAlignment default taVerticalCenter;
    property TextLocation: TSlideShowTextLocation read FTextLocation write
      SetTextLocation default tlBottom;
    property WordWrap: boolean read FWordWrap write SetWordWrap default true;
    property Ellipsis: boolean read FEllipsis write SetEllipsis default false;
    property URL: String read FURL write SetURL;
  end;

  TAdvSmoothSlideShowItems = class(TCollection)
  private
    FOwner: TAdvSmoothSlideShow;
    FOnChange: TNotifyEvent;
    function GetItem(Index: integer): TAdvSmoothSlideShowItem;
    procedure SetItem(Index: integer; const Value: TAdvSmoothSlideShowItem);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TAdvSmoothSlideShow);
    function Add: TAdvSmoothSlideShowItem;
    function Insert(Index: integer): TAdvSmoothSlideShowItem;
    property Items[Index: integer]
      : TAdvSmoothSlideShowItem read GetItem write SetItem; default;
    procedure Delete(Index: integer);
    procedure Update(Item: TCollectionItem); override;
  published
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TSlideShowItemMode = (imStretch, imNormal);

  TSlideShowTransition = (stFade, stPlaceHolder);

  TSlideShowTransitions = set of TSlideShowTransition;

  TSlideShowAnimation = (saBottomLeftToTopRight, saBottomRightToTopLeft,
    saTopLeftToBottomRight, saTopRightToBottomLeft, saBottomToTop,
    saTopToBottom, saLeftToRight, saRightToLeft, saWipeBottomToTop,
    saWipeTopToBottom, saWipeLeftToRight, saWipeRightToLeft,
    saWipeBottomLeftToTopRight, saWipeBottomRightToTopLeft,
    saWipeTopLeftToBottomRight, saWipeTopRightToBottomLeft, saCheckBoard,
    saVerticalCheckBoard, saHorizontalCheckBoard, saEllipse, saRectangle,
    saRotate, saSpin, saSpiral, saFadeInOut, saTearHorizontal, saTearVertical, saTearRotate);

  TSlideShowAnimations = set of TSlideShowAnimation;

  TThumbNailPosition = (tpLeft, tpTop, tpRight, tpBottom);

  TSlideShowHintEvent = procedure(Sender: TObject; itemindex: integer;
    var Hint: string) of object;

  TSlideShowClickEvent = procedure(Sender: TObject; itemindex: integer)
    of object;

  TSlideShowLoadEvent = procedure(Sender: TObject; itemindex: integer)
    of object;

  TSlideShowCustomDrawEvent = procedure(Sender: TObject; itemindex: integer;
    g: TGPGraphics; R: TGPRectF; Kind: TSlideShowItemKind) of object;

  TSlideShowControlCustomDrawEvent = procedure(Sender: TObject; Control: TControl; itemindex: integer;
    bmp: TBitmap) of object;

  THandleAppearance = class(TPersistent)
  private
    FOwner: TAdvSmoothSlideShow;
    FOpacity: Byte;
    FBorderColor: TColor;
    FArrowColor: TColor;
    FColor: TColor;
    FOnChange: TNotifyEvent;
    FSize: integer;
    procedure SetArrowColor(const Value: TColor);
    procedure SetBorderColor(const Value: TColor);
    procedure SetColor(const Value: TColor);
    procedure SetOpacity(const Value: Byte);
    procedure SetSize(const Value: integer);
  protected
    procedure Changed;
  public
    constructor Create(AOwner: TAdvSmoothSlideShow);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Color: TColor read FColor write SetColor default clSilver;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clSilver;
    property ArrowColor: TColor read FArrowColor write SetArrowColor default clBlack;
    property Opacity: Byte read FOpacity write SetOpacity default 200;
    property Size: integer read FSize write SetSize default 35;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvSmoothSlideShow = class(TCustomControl, ITMSStyle, ITMSTones)
  private
    FTMSStyle: TTMSStyle;
    FhoveredDescription: Integer;
    ShowDes: boolean;
    effect: integer;
    FhoveredItem: integer;
    FHandleOk: boolean;
    FDoHandleTimer: boolean;
    FHandleTimer: TTimer;
    FTimeDownOnArrow: integer;
    MaxScrollPos: Double;
    FHandleHorzLeftDown, FHandleHorzLeftHover, FHandleHorzRightDown,
      FHandleHorzRightHover, FHandleVertTopDown, FHandleVertTopHover,
      FHandleVertBottomDown, FHandleVertBottomHover: boolean;
    FCacheBG: boolean;
    FCacheBackGround: TGPBitmap;
    FControlParent: TWinControl;
    FControlAlign: TAlign;
    FControlLeft, FControlTop, FControlWidth, FControlHeight: integer;
    FFullScreen: TForm;
    RestartLoop: boolean;
    FThreadDone: boolean;
    FImageLoaderThread: TImageLoaderThread;
    FDesignTime: boolean;
    FUpdateCount: integer;
    FColorMatrix: TColorMatrix;
    FDoAnimation: boolean;
    FAnimatePlaceHolder, FAnimateImage, FAnimating: boolean;
    FCurrentPlaceHolder: TGPRectF;
    FCurrentOpacityTo, FNextOpacityTo, FCurrentOpacity, FNextOpacity: Byte;
    FRotateTimer, FAnimateTimer: TTimer;
    FItemIndex, FCurrentItemIndex, FNextItemIndex, FPreviousItemIndex: integer;
    FItems: TAdvSmoothSlideShowItems;
    FFill: TGDIPFill;
    FMargin: TMargins;
    FPlaceHolderFill: TGDIPFill;
    FPlaceHolderSpacing: integer;
    FItemMode: TSlideShowItemMode;
    FAspectRatio: boolean;
    FFadeAnimationFactor: integer;
    FRotationInterval: integer;
    FTransitions: TSlideShowTransitions;
    FRotation: boolean;
    FAnimation: boolean;
    FHolderAnimationFactor: integer;
    FThumbNailWidth: integer;
    FThumbNailHolderFill: TGDIPFill;
    FThumbNailSpacing: integer;
    FThumbNails: boolean;
    FThumbNailHeight: integer;
    FThumbNailHolderMargin: TMargins;
    FThumbnailPosition: TThumbNailPosition;
    FThumbNailHandles: THandleAppearance;
    FAnimations: TSlideShowAnimations;
    FThumbNailSelectedFill: TGDIPFill;
    FScrollPos: Double;
    FOnThumbHint: TSlideShowHintEvent;
    FOnCustomDraw: TSlideShowCustomDrawEvent;
    FPlaceHolderVisible: boolean;
    FThumbNailsFloating: boolean;
    FLoadEffect: boolean;
    FOnThumbDblClick: TSlideShowClickEvent;
    FOnThumbClick: TSlideShowClickEvent;
    FOnThumbLoaded: TSlideShowLoadEvent;
    FOnImageLoaded: TSlideShowLoadEvent;
    FDescriptionFill: TGDIPFill;
    FShowDescription: Boolean;
    FStretchPercentage: Integer;
    FOnCustomDrawControl: TSlideShowControlCustomDrawEvent;
    FOnAnimatePlaceHolder: TSlideShowLoadEvent;
    FOnAnimatePlaceHolderFinished: TSlideShowLoadEvent;
    FOnAnimateImage: TSlideShowLoadEvent;
    FOnAnimateImageFinished: TSlideShowLoadEvent;
    FOnDrawPlaceHolder: TSlideShowCustomDrawEvent;
    procedure SetItems(const Value: TAdvSmoothSlideShowItems);
    procedure SetFill(const Value: TGDIPFill);
    procedure SetMargin(const Value: TMargins);
    procedure SetPlaceHolderFill(const Value: TGDIPFill);
    procedure SetPlaceHolderSpacing(const Value: integer);
    procedure SetItemMode(const Value: TSlideShowItemMode);
    procedure SetAspectRatio(const Value: boolean);
    procedure SetItemIndex(const Value: integer);
    procedure SetRotationInterval(const Value: integer);
    procedure SetTransitions(const Value: TSlideShowTransitions);
    procedure SetRotation(const Value: boolean);
    procedure SetAnimation(const Value: boolean);
    procedure SetHolderAnimationFactor(const Value: integer);
    procedure SetFadeAnimationFactor(const Value: integer);
    procedure SetThumbNailHeight(const Value: integer);
    procedure SetThumbNailHolderFill(const Value: TGDIPFill);
    procedure SetThumbNails(const Value: boolean);
    procedure SetThumbNailSpacing(const Value: integer);
    procedure SetThumbNailWidth(const Value: integer);
    procedure SetThumbNailHolderMargin(const Value: TMargins);
    procedure SetThumbNailPosition(const Value: TThumbNailPosition);
    procedure SetThumbNailHandles(const Value: THandleAppearance);
    procedure CMMouseLeave(var Message: TMessage);
    message CM_MOUSELEAVE;
    procedure CMHintShow(var Message: TMessage);
    message CM_HINTSHOW;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode);
    message WM_GETDLGCODE;
    procedure WMMouseWheel(var Message: TWMMouseWheel);
    message WM_MOUSEWHEEL;
    procedure SetAnimations(const Value: TSlideShowAnimations);
    procedure SetThumbNailSelectedFill(const Value: TGDIPFill);
    procedure SetScrollPos(const Value: Double);
    procedure SetPlaceHolderVisible(const Value: boolean);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    procedure SetThumbNailsFloating(const Value: boolean);
    procedure SetLoadEffect(const Value: boolean);
    procedure SetDescriptionFill(const Value: TGDIPFill);
    procedure SetShowDescription(const Value: Boolean);
    procedure SetStretchPercentage(const Value: Integer);
    function GetFadeAnimationFactor: integer;
  protected
    procedure ReadTMSStyle(Reader: TReader);
    procedure WriteTMSStyle(Writer: TWriter);
    procedure DefineProperties(Filer: TFiler); override;
    procedure Changed;
    procedure MarginChanged(Sender: TObject);
    procedure FillChanged(Sender: TObject);
    procedure PlaceHolderFillChanged(Sender: TObject);
    procedure ThumbNailFillChanged(Sender: TObject);
    procedure HandleTimerChanged(Sender: TObject);
    procedure ItemsChanged(Sender: TObject);
    procedure Draw(g: TGPGraphics);
    procedure DrawBackGround(g: TGPGraphics);
    procedure DrawHandles(g: TGPGraphics);
    procedure DrawThumbs(g: TGPGraphics);
    procedure Animate(Sender: TObject);
    procedure Rotate(Sender: TObject);
    procedure HandlesChanged(Sender: TObject);
    procedure BuildItems;
    function AnimatePlaceHolder: boolean;
    function GetThumbNailRect: TGPRectF;
    function GetThumbNailRectangle: TRect;
    function AnimateImage: boolean;
    procedure ThreadDone(Sender: TObject);
    procedure CreateWnd; override;
    procedure ScrollToItem(Index: integer);
    function XYToThumb(pX, pY: integer): integer;
    function XYToCurrentItem(pX, pY: integer): integer;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
      override;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    function GetHandleHorzLeft: TGPRectF;
    function GetHandleHorzRight: TGPRectF;
    function GetHandleVertTop: TGPRectF;
    function GetHandleVertBottom: TGPRectF;
    function IsHandleHorzLeft: boolean;
    function IsHandleHorzRight: boolean;
    function IsHandleVertTop: boolean;
    function IsHandleVertBottom: boolean;
    procedure PaintThumbnails;
    procedure Notification(AComponent: TComponent; AOperation: TOperation);
      override;
    function GetAnimationCount: integer;
    function GetRandomAnimation: TSlideShowAnimation;
    function GetCountSelectable: integer;
    function GetMaxSelectable: integer;
    function GetMinSelectable: integer;
    property ScrollPos: Double read FScrollPos write SetScrollPos;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure DoExit; override;
    function GetVersionNr: integer;
    procedure DblClick; override;
  public
    property CurrentPlaceHolder: TGPRectF read FCurrentPlaceHolder;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    procedure Resize; override;
    procedure AddItemsFromFolder(AFolder: String; AKind: TSlideShowItemKind;
      DescriptionLocation: boolean = false; CaptionLocation: boolean = false;
      LocationThumbHint: boolean = false);
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure StopThread;
    procedure StartThread;
    procedure FullScreen;
    procedure Normal;
    procedure Next;
    procedure Previous;
    procedure UpdateControlImage(AControl: TWinControl);
    procedure UpdateImages;
    procedure SetComponentStyle(AStyle: TTMSStyle);
    function GetComponentStyle: TTMSStyle;
    procedure SetColorTones(ATones: TColorTones);
  published
    property ShowDescription: Boolean read FShowDescription write SetShowDescription default True;
    property FadeAnimationFactor: integer read GetFadeAnimationFactor write
      SetFadeAnimationFactor default 6;
    property HolderAnimationFactor: integer read FHolderAnimationFactor write
      SetHolderAnimationFactor default 6;
    property PlaceHolderFill: TGDIPFill read FPlaceHolderFill write
      SetPlaceHolderFill;
    property PlaceHolderSpacing: integer read FPlaceHolderSpacing write
      SetPlaceHolderSpacing default 40;
    property PlaceHolder: boolean read FPlaceHolderVisible write
      SetPlaceHolderVisible default true;
    property Fill: TGDIPFill read FFill write SetFill;
    property DescriptionFill: TGDIPFill read FDescriptionFill write
      SetDescriptionFill;
    property Items: TAdvSmoothSlideShowItems read FItems write SetItems;
    property Margin: TMargins read FMargin write SetMargin;
    property ItemMode
      : TSlideShowItemMode read FItemMode write SetItemMode default imNormal;
    property AspectRatio : boolean read FAspectRatio write SetAspectRatio default true;
    property ItemIndex: integer read FItemIndex write SetItemIndex default 0;
    property Rotation: boolean read FRotation write SetRotation default true;
    property Animation: boolean read FAnimation write SetAnimation default true;
    property RotationInterval: integer read FRotationInterval write SetRotationInterval default 3500;
    property Transitions: TSlideShowTransitions read FTransitions write SetTransitions default[stPlaceHolder, stFade];
    property Animations: TSlideShowAnimations read FAnimations write SetAnimations default[saLeftToRight];
    property LoadEffect: boolean read FLoadEffect write SetLoadEffect default true;
    property ThumbNailsFloating: boolean read FThumbNailsFloating write SetThumbNailsFloating default false;
    property ThumbNailHandles: THandleAppearance read FThumbNailHandles write SetThumbNailHandles;
    property ThumbNailSpacing: integer read FThumbNailSpacing write SetThumbNailSpacing default 10;
    property ThumbNails : boolean read FThumbNails write SetThumbNails default true;
    property ThumbNailHolderMargin: TMargins read FThumbNailHolderMargin write SetThumbNailHolderMargin;
    property ThumbNailHolderFill: TGDIPFill read FThumbNailHolderFill write SetThumbNailHolderFill;
    property ThumbNailWidth: integer read FThumbNailWidth write SetThumbNailWidth default 100;
    property ThumbNailHeight: integer read FThumbNailHeight write SetThumbNailHeight default 100;
    property ThumbNailPosition: TThumbNailPosition read FThumbnailPosition write SetThumbNailPosition default tpBottom;
    property ThumbNailSelectedFill: TGDIPFill read FThumbNailSelectedFill write SetThumbNailSelectedFill;
    property OnThumbHint: TSlideShowHintEvent read FOnThumbHint write FOnThumbHint;
    property OnThumbClick: TSlideShowClickEvent read FOnThumbClick write FOnThumbClick;
    property OnThumbDblClick: TSlideShowClickEvent read FOnThumbDblClick write FOnThumbDblClick;
    property OnThumbLoaded: TSlideShowLoadEvent read FOnThumbLoaded write FOnThumbLoaded;
    property OnImageLoaded : TSlideShowLoadEvent read FOnImageLoaded write FOnImageLoaded;
    property OnAnimatePlaceHolderFinished: TSlideShowLoadEvent read FOnAnimatePlaceHolderFinished write FOnAnimatePlaceHolderFinished;
    property OnAnimatePlaceHolder: TSlideShowLoadEvent read FOnAnimatePlaceHolder write FOnAnimatePlaceHolder;
    property OnAnimateImageFinished: TSlideShowLoadEvent read FOnAnimateImageFinished write FOnAnimateImageFinished;
    property OnAnimateImage: TSlideShowLoadEvent read FOnAnimateImage write FOnAnimateImage;
    property OnCustomDraw: TSlideShowCustomDrawEvent read FOnCustomDraw write FOnCustomDraw;
    property OnDrawPlaceHolder: TSlideShowCustomDrawEvent read FOnDrawPlaceHolder write FOnDrawPlaceHolder;
    property StretchPercentage: Integer read FStretchPercentage write SetStretchPercentage default 100;
    property OnCustomDrawControl: TSlideShowControlCustomDrawEvent read FOnCustomDrawControl write FOnCustomDrawControl;
    property Version: string read GetVersion write SetVersion;

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
    property OnResize;
    property OnDblClick;
    property OnClick;
    property OnEnter;
    property OnExit;
     property OnDragDrop;
    property OnDragOver;
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

function Lighter(Color: TColor; Percent: Byte): TColor;
var
  R, g, b: Byte;
begin
  Color := ColorToRGB(Color);
  R := GetRValue(Color);
   g := GetGValue(Color);
  b := GetBValue(Color);
  R := R + muldiv(255 - R, Percent, 100); // Percent% closer to white
  g := g + muldiv(255 - g, Percent, 100);
  b := b + muldiv(255 - b, Percent, 100);
  result := RGB(R, g, b);
end;

function Darker(Color: TColor; Percent: Byte): TColor;
var
  R, g, b: Byte;
begin
  Color := ColorToRGB(Color);
  R := GetRValue(Color);
  g := GetGValue(Color);
  b := GetBValue(Color);
  R := R - muldiv(R, Percent, 100); // Percent% closer to black
  g := g - muldiv(g, Percent, 100);
  b := b - muldiv(b, Percent, 100);
  result := RGB(R, g, b);
 end;

function PtInGPRect(R: TGPRectF; pt: TPoint): boolean;
begin
  result := ((pt.X >= R.X) and (pt.X <= R.X + R.Width)) and
    ((pt.Y >= R.Y) and (pt.Y <= R.Y + R.Height));
end;

function RectanglesInterSect(r1, r2: TGPRectF): boolean;
var
  X, Y, w, h: Double;
begin
  X := max(r1.X, r2.X);
  Y := max(r1.Y, r2.Y);
  w := min(r1.X + r1.Width, r2.X + r2.Width);
  h := min(r1.Y + r1.Height, r2.Y + r2.Height);

  result := ((w > X) and (h > Y));
end;

 function AnimateDouble(var Start: Single; Stop, Delta, Margin: Single): boolean;
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
      Start := Start + Delta
    else
      Start := Start - Delta;
  end;
end;

function AnimateByte(var Start: Byte; Stop, Delta, Margin: Byte): boolean;
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
      Start := Start + Delta
    else
      Start := Start - Delta;
  end;
end;

{ TAdvSmoothSlideShowItem }

procedure TAdvSmoothSlideShowItem.Changed;
begin
   FThreadStatus := stInitialized;
  FThumbThreadStatus := stInitialized;
  FOwner.Changed;
  FOwner.RestartLoop := true;
end;

constructor TAdvSmoothSlideShowItem.Create(Collection: TCollection);
begin
  inherited;
  FOwner := (Collection as TAdvSmoothSlideShowItems).FOwner;
  FImage := TAdvGDIPPicture.Create;
  FImage.OnChange := ImageChanged;
  FKind := ikImage;
  FEnabled := true;
  FVisible := true;
  FThreadStatus := stInitialized;
  FThumbThreadStatus := stInitialized;
  FText := 'Slide Show Item';
  FCustomImageHeight := 100;
  FCustomImageWidth := 100;
   FTextFont := TFont.Create;
  FDescriptionFont := TFont.Create;
  FTextWidth := 100;
  FTextHeight := 100;
  FTextAutoSize := true;
  FTextLocation := tlBottom;
  FTextHorizontalAlignment := taCenter;
  FTextVerticalAlignment := taVerticalCenter;
  FDescriptionPosition := dpBottomCenter;
  FOwner.Changed;
end;

destructor TAdvSmoothSlideShowItem.Destroy;
begin
  if Assigned(FThumbnail) then
    FreeAndNil(FThumbnail);

  if Assigned(FOriginalImage) then
    FreeAndNil(FOriginalImage);
  FImage.Free;
   FTextFont.Free;
  FDescriptionFont.Free;
  inherited;
  FOwner.Changed;
end;

procedure TAdvSmoothSlideShowItem.DrawDescription(g: TGPGraphics; R: TGPRectF;
  Opacity: Byte);
var
  ft: TGPFont;
  sf: TGPStringFormat;
  b: TGPSolidBrush;
  fr, textr: TGPRectF;
  spc: integer;
begin
  if Description <> '' then
  begin
    ft := g.MakeFont(DescriptionFont);
    sf := TGPStringFormat.Create;
    sf.SetLineAlignment(StringAlignmentCenter);
     b := TGPSolidBrush.Create(MakeColor(255, DescriptionFont.Color));

    g.MeasureString(Description, Length(Description), ft, R, sf, textr);

    spc := 5;
    case DescriptionPosition of
      dpBottomLeft, dpTopLeft: sf.SetAlignment(StringAlignmentNear);
      dpTopCenter, dpBottomCenter: sf.SetAlignment(StringAlignmentCenter);
      dpTopRight, dpBottomRight: sf.SetAlignment(StringAlignmentFar);
    end;

    case DescriptionPosition of
      dpTopLeft, dpTopCenter, dpTopRight:
        fr := MakeRect(R.X, R.Y, R.Width, textr.Height + spc * 2);
      dpBottomLeft, dpBottomCenter, dpBottomRight:
        fr := MakeRect(R.X, R.Y + R.Height - textr.Height - spc * 2, R.Width,
          textr.Height + spc * 2);
    end;

    FOwner.DescriptionFill.Fill(g, fr);

     g.DrawString(Description, Length(Description), ft, fr, sf, b);

    sf.Free;
    ft.Free;
    b.Free;
  end;
end;

procedure TAdvSmoothSlideShowItem.DrawItem(g: TGPGraphics; R: TGPRectF;
  Opacity: Byte);
var
  ia: TGPImageAttributes;
  mx: TGPMatrix;
  pth: TGPGraphicsPath;
  X, Y, cw, ch, imgw, imgh, imgwr, imghr, cwoffset, choffset: Double;
  divider: integer;
  row, col: integer;
  rc: TGPRectF;
  animval, animperc: Double;
   percentageAngle, percentageDistance, Rotation, scale, cx, cy, pc1, pc2,
    a: Double;
  last, thispoint: TGPPointF;
  chk: boolean;
  Pixely, PixelX: integer;
  I: Integer;
begin
  chk := Opacity < 255;
  if not FOwner.LoadEffect then
    chk := chk and (Index = FOwner.FCurrentItemIndex);

  if chk then
  begin
    if Assigned(FOriginalImage) then
    begin
      imgw := FOriginalImage.Width;
      imgh := FOriginalImage.Height;
      imgwr := R.Width;
      imghr := R.Height;
      if stFade in FOwner.Transitions then
         FOwner.FColorMatrix[3, 3] := Opacity / 255;

      ia := TGPImageAttributes.Create;
      ia.SetColorMatrix(FOwner.FColorMatrix);

      divider := 4;

      animval := Opacity / 255;
      animperc := animval * 100;

      if animval = 0 then
        Exit;

      case FOwner.effect of
        0: // Bottom Left To Top Right
          begin
            mx := TGPMatrix.Create(1, 0, 0, 1, (imgwr * animval) - imgwr, -
                (imghr * animval) + imghr);
            g.SetTransform(mx);
            g.DrawImage(FOriginalImage, R, 0, 0, imgw, imgh, UnitPixel, ia);
             g.ResetTransform;
            mx.Free;
          end;
        1: // Bottom Right To Top Left
          begin
            mx := TGPMatrix.Create(1, 0, 0, 1, -(imgwr * animval) + imgwr, -
                (imghr * animval) + imghr);
            g.SetTransform(mx);
            g.DrawImage(FOriginalImage, R, 0, 0, imgw, imgh, UnitPixel, ia);
            g.ResetTransform;
            mx.Free;
          end;
        2: // Top Left To Bottom Right
          begin
            mx := TGPMatrix.Create(1, 0, 0, 1, (imgwr * animval) - imgwr,
              (imghr * animval) - imghr);
            g.SetTransform(mx);
            g.DrawImage(FOriginalImage, R, 0, 0, imgw, imgh, UnitPixel, ia);
            g.ResetTransform;
            mx.Free;
          end;
        3: // Top Right To Bottom Left
          begin
            mx := TGPMatrix.Create(1, 0, 0, 1, -(imgwr * animval) + imgwr,
              (imghr * animval) - imghr);
            g.SetTransform(mx);
            g.DrawImage(FOriginalImage, R, 0, 0, imgw, imgh, UnitPixel, ia);
            g.ResetTransform;
            mx.Free;
          end;
        4: // Bottom To Top
          begin
            mx := TGPMatrix.Create(1, 0, 0, 1, 0, -(imghr * animval) + imghr);
            g.SetTransform(mx);
            g.DrawImage(FOriginalImage, R, 0, 0, imgw, imgh, UnitPixel, ia);
            g.ResetTransform;
            mx.Free;
          end;
        5: // Top To Bottom
          begin
            mx := TGPMatrix.Create(1, 0, 0, 1, 0, (imghr * animval) - imghr);
            g.SetTransform(mx);
            g.DrawImage(FOriginalImage, R, 0, 0, imgw, imgh, UnitPixel, ia);
            g.ResetTransform;
            mx.Free;
          end;
        6: // Left To Right
          begin
            mx := TGPMatrix.Create(1, 0, 0, 1, (imgwr * animval) - imgwr, 0);
            g.SetTransform(mx);
            g.DrawImage(FOriginalImage, R, 0, 0, imgw, imgh, UnitPixel, ia);
            g.ResetTransform;
            mx.Free;
          end;
        7: // Right To Left
          begin
            mx := TGPMatrix.Create(1, 0, 0, 1, -(imgwr * animval) + imgwr, 0);
            g.SetTransform(mx);
            g.DrawImage(FOriginalImage, R, 0, 0, imgw, imgh, UnitPixel, ia);
            g.ResetTransform;
            mx.Free;
          end;
        8: // Wipe Bottom To Top
          begin
            pth := TGPGraphicsPath.Create;
            pth.AddRectangle(MakeRect(R.X, R.Y - imghr + (imghr * animval),
                imgwr, imghr));
            g.SetClip(pth, CombineModeReplace);
            g.DrawImage(FOriginalImage, R, 0, 0, imgw, imgh, UnitPixel, ia);
            g.ResetClip;
            pth.Free;
          end;
        9: // Wipe Top To Bottom
          begin
            pth := TGPGraphicsPath.Create;
            pth.AddRectangle(MakeRect(R.X, R.Y + imghr - (imghr * animval),
                imgwr, imghr));
            g.SetClip(pth, CombineModeReplace);
            g.DrawImage(FOriginalImage, R, 0, 0, imgw, imgh, UnitPixel, ia);
            g.ResetClip;
            pth.Free;
          end;
        10: // Wipe Left to Right
          begin
            pth := TGPGraphicsPath.Create;
            pth.AddRectangle(MakeRect(R.X + imgwr - (imgwr * animval), R.Y,
                imgwr, imghr));
            g.SetClip(pth, CombineModeReplace);
            g.DrawImage(FOriginalImage, R, 0, 0, imgw, imgh, UnitPixel, ia);
            g.ResetClip;
            pth.Free;
          end;
        11: // Wipe Right To Left
          begin
            pth := TGPGraphicsPath.Create;
            pth.AddRectangle(MakeRect(R.X - imgwr + (imgwr * animval), R.Y,
                imgwr, imghr));
            g.SetClip(pth, CombineModeReplace);
            g.DrawImage(FOriginalImage, R, 0, 0, imgw, imgh, UnitPixel, ia);
            g.ResetClip;
            pth.Free;
          end;
        12: // Bottom Left To Top Right
          begin
            pth := TGPGraphicsPath.Create;
            pth.AddRectangle
              (MakeRect(R.X + imgwr - (imgwr * animval), R.Y - imghr +
                  (imghr * animval), imgwr, imghr));
            g.SetClip(pth, CombineModeReplace);
            g.DrawImage(FOriginalImage, R, 0, 0, imgw, imgh, UnitPixel, ia);
            g.ResetClip;
            pth.Free;
          end;
        13: // Bottom Right To Top Left
          begin
            pth := TGPGraphicsPath.Create;
            pth.AddRectangle
              (MakeRect(R.X - imgwr + (imgwr * animval), R.Y - imghr +
                  (imghr * animval), imgwr, imghr));
            g.SetClip(pth, CombineModeReplace);
            g.DrawImage(FOriginalImage, R, 0, 0, imgw, imgh, UnitPixel, ia);
            g.ResetClip;
            pth.Free;
          end;
        14: // Wipe Top Left To Bottom Right
          begin
            pth := TGPGraphicsPath.Create;
            pth.AddRectangle
              (MakeRect(R.X + imgwr - (imgwr * animval), R.Y + imghr -
                  (imghr * animval), imgwr, imghr));
            g.SetClip(pth, CombineModeReplace);
            g.DrawImage(FOriginalImage, R, 0, 0, imgw, imgh, UnitPixel, ia);
            g.ResetClip;
            pth.Free;
          end;
        15: // Wipe Top Right To Bottom Left
          begin
            pth := TGPGraphicsPath.Create;
            pth.AddRectangle
              (MakeRect(R.X - imgwr + (imgwr * animval), R.Y + imghr -
                  (imghr * animval), imgwr, imghr));
            g.SetClip(pth, CombineModeReplace);
            g.DrawImage(FOriginalImage, R, 0, 0, imgw, imgh, UnitPixel, ia);
            g.ResetClip;
            pth.Free;
          end;
        16: // checkboard inner to outer
          begin
            pth := TGPGraphicsPath.Create;

            cw := (imgwr * animval) / divider;
            ch := (imghr * animval) / divider;
            cwoffset := (imgwr / divider / 2);
            choffset := (imghr / divider / 2);

            Y := 0;
            while Y < imghr do
            begin
              X := 0;
              while X < imgwr do
              begin
                rc := MakeRect(X + R.X - (cw / 2) + cwoffset, Y + R.Y -
                    (ch / 2) + choffset, cw, ch);
                pth.AddRectangle(rc);
                X := X + (imgwr / divider);
              end;
              Y := Y + (imghr / divider);
            end;

            g.SetClip(pth, CombineModeIntersect);
            g.DrawImage(FOriginalImage, R, 0, 0, imgw, imgh, UnitPixel, ia);
            g.ResetClip;
            pth.Free;
          end;
        17: // vertical checkboard
          begin
            pth := TGPGraphicsPath.Create;

            cw := (imgwr / divider);
            ch := (imghr * animval) / divider;
            col := 0;

            X := 0;
            while X < imgwr do
            begin
              Y := 0;
              while Y < imghr do
              begin
                rc := MakeRect(X + R.X, Y + R.Y, cw, ch);
                if (col AND 1) = 1 Then
                  rc.Y := rc.Y + (imghr / (2 * divider));

                pth.AddRectangle(rc);
                if (animperc >= 50) and (col And 1 = 1) and (Y = 0) then
                begin
                  rc.Y := rc.Y - (imghr / divider);
                  pth.AddRectangle(rc);
                end;
                Y := Y + (imghr / divider);
              end;
              Inc(col);
              X := X + cw;
            end;

            g.SetClip(pth, CombineModeIntersect);
            g.DrawImage(FOriginalImage, R, 0, 0, imgw, imgh, UnitPixel, ia);
            g.ResetClip;
            pth.Free;
          end;
        18: // horizontal checkboard
          begin
            pth := TGPGraphicsPath.Create;

            ch := (imghr / divider);
            cw := (imgwr * animval) / divider;
            row := 0;

            Y := 0;
            while Y < imghr do
            begin
              X := 0;
              while X < imgwr do
              begin
                rc := MakeRect(X + R.X, Y + R.Y, cw, ch);
                if (row AND 1) = 1 Then
                  rc.X := rc.X + (imgwr / (2 * divider));

                pth.AddRectangle(rc);
                if (animperc >= 50) and (row And 1 = 1) and (X = 0) then
                begin
                  rc.X := rc.X - (imgwr / divider);
                  pth.AddRectangle(rc);
                end;
                X := X + (imgwr / divider);
              end;
              Inc(row);
              Y := Y + ch;
            end;

            g.SetClip(pth, CombineModeIntersect);
            g.DrawImage(FOriginalImage, R, 0, 0, imgw, imgh, UnitPixel, ia);
            g.ResetClip;
            pth.Free;
          end;
        19: // ellipse
          begin
            pth := TGPGraphicsPath.Create;
            cw := (imgwr * 1.414) * animperc / 100;
            ch := (imghr * 1.414) * animperc / 200;
            pth.AddEllipse(R.X + imgwr / 2 - cw, R.Y + imghr / 2 - ch, 2 * cw,
              2 * ch);
            g.SetClip(pth, CombineModeReplace);
            g.DrawImage(FOriginalImage, R, 0, 0, imgw, imgh, UnitPixel, ia);
            g.ResetClip;
            pth.Free;
          end;
        20: // rectangle
          begin
            pth := TGPGraphicsPath.Create;
            cw := (imgwr * 1.414) * animperc / 200;
            ch := (imghr * 1.414) * animperc / 200;
            pth.AddRectangle
              (MakeRect(R.X + imgwr / 2 - cw, R.Y + imghr / 2 - ch, 2 * cw,
                2 * ch));
            g.SetClip(pth, CombineModeReplace);
            g.DrawImage(FOriginalImage, R, 0, 0, imgw, imgh, UnitPixel, ia);
            g.ResetClip;
            pth.Free;
          end;
        21: // rotate
          begin
            Rotation := 360 * animval;
            cw := R.X + imgwr / 2;
            ch := R.Y + imghr / 2;
            mx := TGPMatrix.Create(1, 0, 0, 1, cw, ch);
            mx.Rotate(Rotation, MatrixOrderPrepend);
            g.SetTransform(mx);
            g.DrawImage(FOriginalImage, MakeRect(-imgwr / 2, -imghr / 2, imgwr,
                imghr), 0, 0, imgw, imgh, UnitPixel, ia);
            g.ResetTransform;
            mx.Free;
          end;
        22: // spin
          begin
            Rotation := 360 * animval;
            cw := R.X + imgwr / 2;
            ch := R.Y + imghr / 2;

            scale := animval;
            mx := TGPMatrix.Create(scale, 0, 0, scale, cw, ch);
            mx.Rotate(Rotation, MatrixOrderPrepend);
            g.SetTransform(mx);
            g.DrawImage(FOriginalImage, MakeRect(-imgwr / 2, -imghr / 2, imgwr,
                imghr), 0, 0, imgw, imgh, UnitPixel, ia);
            g.ResetTransform;
            mx.Free;
          end;
        23: // spiral
          begin
            If animperc < 100 then
            begin
              percentageAngle := divider * (PI * 2) / 100;
              percentageDistance := max(imgwr, imghr) / 100;
              pth := TGPGraphicsPath.Create(FillModeWinding);

              cx := R.X + imgwr / 2;
              cy := R.Y + imghr / 2;

              pc1 := animperc - 100;
              pc2 := animperc;

              if pc1 < 0 Then
                pc1 := 0;

              a := percentageAngle * pc2;
              last := MakePoint((cx + (pc1 * percentageDistance * Cos(a))),
                (cy + (pc1 * percentageDistance * Sin(a))));
              a := percentageAngle * pc1 / 2;

              while pc1 <= pc2 do
              begin
                thispoint := MakePoint
                  ((cx + (pc1 * percentageDistance * Cos(a))),
                  (cy + (pc1 * percentageDistance * Sin(a))));
                pth.AddLine(last, thispoint);
                last := thispoint;
                pc1 := pc1 + 0.1;
                a := a + percentageAngle / 10;
              end;

              pth.CloseFigure;
              g.SetClip(pth, CombineModeReplace);
              g.DrawImage(FOriginalImage, R, 0, 0, imgw, imgh, UnitPixel, ia);
              g.ResetClip;
              pth.Free;
            end;
          end;
        24:
          g.DrawImage(FOriginalImage, R, 0, 0, imgw, imgh, UnitPixel, ia);
        // fadeinout
        25:
        begin
          pth := TGPGraphicsPath.Create;
          pth.AddRectangle(MakeRect(R.X - imgwr + (imgwr * animval), R.Y,
              imgwr / 2, imghr));
          g.SetClip(pth, CombineModeReplace);
          mx := TGPMatrix.Create(1, 0, 0, 1, (imgwr * animval) - imgwr, 0);
          g.SetTransform(mx);
          g.DrawImage(FOriginalImage, R, 0, 0, imgw, imgh, UnitPixel, ia);
          g.ResetTransform;
          g.ResetClip;
          mx.Free;
          pth.Free;

          pth := TGPGraphicsPath.Create;
          pth.AddRectangle(MakeRect(R.X + imgwr + imgwr / 2 - (imgwr * animval), R.Y,
              imgwr / 2, imghr));
          g.SetClip(pth, CombineModeReplace);
          mx := TGPMatrix.Create(1, 0, 0, 1, -(imgwr * animval) + imgwr, 0);
          g.SetTransform(mx);
          g.DrawImage(FOriginalImage, R, 0, 0, imgw, imgh, UnitPixel, ia);
          g.ResetTransform;
          g.ResetClip;
          mx.Free;
          pth.Free;
        end;
        26:
        begin
          pth := TGPGraphicsPath.Create;
          pth.AddRectangle(MakeRect(R.X, R.Y - imghr + (imghr * animval),
              imgwr, imghr / 2));
          g.SetClip(pth, CombineModeReplace);
          mx := TGPMatrix.Create(1, 0, 0, 1, 0, (imghr * animval) - imghr);
          g.SetTransform(mx);
          g.DrawImage(FOriginalImage, R, 0, 0, imgw, imgh, UnitPixel, ia);
          g.ResetTransform;
          g.ResetClip;
          mx.Free;
          pth.Free;

          pth := TGPGraphicsPath.Create;
          pth.AddRectangle(MakeRect(R.X, R.Y + imghr + imghr / 2 - (imghr * animval),
              imgwr, imghr / 2));
          g.SetClip(pth, CombineModeReplace);
          mx := TGPMatrix.Create(1, 0, 0, 1, 0, -(imghr * animval) + imghr);
          g.SetTransform(mx);
          g.DrawImage(FOriginalImage, R, 0, 0, imgw, imgh, UnitPixel, ia);
          g.ResetTransform;
          g.ResetClip;
          mx.Free;
          pth.Free;
        end;
        27:
        begin
          Rotation := 360 * animval;
          cw := R.X + imgwr / 2;
          ch := R.Y + imghr / 2;


          pth := TGPGraphicsPath.Create;
          pth.AddRectangle(MakeRect(R.X - imgwr + (imgwr * animval), R.Y,
              imgwr / 2, imghr));
          g.SetClip(pth, CombineModeReplace);
          mx := TGPMatrix.Create(1, 0, 0, 1, cw, ch);
          mx.Rotate(Rotation, MatrixOrderPrepend);
          g.SetTransform(mx);
          g.DrawImage(FOriginalImage, MakeRect(-imgwr / 2, -imghr / 2, imgwr,
              imghr), 0, 0, imgw, imgh, UnitPixel, ia);
          g.ResetTransform;
          mx.Free;
          g.ResetClip;
          pth.Free;

          pth := TGPGraphicsPath.Create;
          pth.AddRectangle(MakeRect(R.X + imgwr + imgwr / 2 - (imgwr * animval), R.Y,
              imgwr / 2, imghr));
          g.SetClip(pth, CombineModeReplace);
          mx := TGPMatrix.Create(1, 0, 0, 1, cw, ch);
          mx.Rotate(Rotation, MatrixOrderPrepend);
          g.SetTransform(mx);
          g.DrawImage(FOriginalImage, MakeRect(-imgwr / 2, -imghr / 2, imgwr,
              imghr), 0, 0, imgw, imgh, UnitPixel, ia);
          g.ResetTransform;
          mx.Free;
          g.ResetClip;
          pth.Free;
        end;
      end;
      ia.Free;
    end;
  end
  else
    g.DrawImage(FOriginalImage, R);

  // item description
  if FOwner.ShowDescription and FOwner.ShowDes and (Index = FOwner.FCurrentItemIndex) then
    DrawDescription(g, R, 255);
end;

procedure TAdvSmoothSlideShowItem.DrawPlaceHolder(g: TGPGraphics; R: TGPRectF;
  Opacity: Byte);
begin
  if FOwner.PlaceHolder then
  begin
    FOwner.PlaceHolderFill.Fill(g, R, Opacity, Opacity, Opacity, Opacity);
    if Assigned(FOwner.OnDrawPlaceHolder) then
      FOwner.OnDrawPlaceHolder(Self, Index, g, R, Kind);
  end;
end;

procedure TAdvSmoothSlideShowItem.GetAspectSize
  (var w, h: Double; ow, oh, nw, nh: Double; f: integer);
begin
  if FOwner.AspectRatio then
  begin
    if (ow > 0) and (oh > 0) and (nw > 0) and (nh > 0) then
    begin
      if (ow < nw) and (oh < nh) and (FOwner.ItemMode = imNormal) then
      begin
        w := ow;
        h := oh;
      end
      else
      begin
        if ow / oh < nw / nh then
        begin
          h := nh * (f / 100);
          w := (nh * ow / oh) * (f / 100);
        end
        else
        begin
          w := nw * (f / 100);
          h := (nw * oh / ow) * (f / 100);
        end;
      end;
    end
    else
    begin
      w := 0;
      h := 0;
    end;
  end
  else
  begin
    w := nw;
    h := nh;
  end;
end;

procedure TAdvSmoothSlideShowItem.ImageChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothSlideShowItem.SetControl(const Value: TWinControl);
begin
  if FControl <> Value then
  begin
    FControl := Value;
    if Assigned(FControl) and Assigned(FOwner) then
    begin
      FControl.Parent := FOwner;
      if Index <> FOwner.itemindex then
        FControl.Visible := false;
    end;
    Changed;
  end;
end;

procedure TAdvSmoothSlideShowItem.SetDescription(const Value: String);
begin
  if FDescription <> Value then
  begin
    FDescription := Value;
    Changed;
  end;
end;

procedure TAdvSmoothSlideShowItem.SetDescriptionFont(const Value: TFont);
begin
  if FDescriptionFont <> Value then
  begin
    FDescriptionFont.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothSlideShowItem.SetDescriptionPosition
  (const Value: TDescriptionPosition);
begin
  if FDescriptionPosition <> Value then
  begin
    FDescriptionPosition := Value;
    Changed;
  end;
end;

procedure TAdvSmoothSlideShowItem.SetEllipsis(const Value: boolean);
begin
  if FEllipsis <> Value then
  begin
    FEllipsis := Value;
    Changed;
  end;
end;

procedure TAdvSmoothSlideShowItem.SetEnabled(const Value: boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    Changed;
  end;
end;

procedure TAdvSmoothSlideShowItem.SetHint(const Value: String);
begin
  if FHint <> Value then
  begin
    FHint := Value;
    Changed;
  end;
end;

procedure TAdvSmoothSlideShowItem.SetImage(const Value: TAdvGDIPPicture);
begin
  if FImage <> Value then
  begin
    FImage.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothSlideShowItem.SetKind(const Value: TSlideShowItemKind);
begin
  if FKind <> Value then
  begin
    FKind := Value;
    FImage.Assign(nil);
    Changed;
  end;
end;

procedure TAdvSmoothSlideShowItem.SetLocation(const Value: String);
begin
  if FLocation <> Value then
  begin
    FLocation := Value;
    Changed;
  end;
end;

procedure TAdvSmoothSlideShowItem.SetText(const Value: String);
begin
  if FText <> Value then
  begin
    FText := Value;
    Changed;
  end;
end;

procedure TAdvSmoothSlideShowItem.SetTextAutoSize(const Value: boolean);
begin
  if FTextAutoSize <> Value then
  begin
    FTextAutoSize := Value;
    Changed;
  end;
end;

procedure TAdvSmoothSlideShowItem.SetTextFont(const Value: TFont);
begin
  if FTextFont <> Value then
  begin
    FTextFont.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothSlideShowItem.SetTextHeight(const Value: integer);
begin
  if FTextHeight <> Value then
  begin
    FTextHeight := Value;
    Changed;
  end;
end;

procedure TAdvSmoothSlideShowItem.SetTextHorizontalAlignment
  (const Value: TAlignment);
begin
  if FTextHorizontalAlignment <> Value then
  begin
    FTextHorizontalAlignment := Value;
    Changed;
  end;
end;

procedure TAdvSmoothSlideShowItem.SetTextLocation
  (const Value: TSlideShowTextLocation);
begin
  if FTextLocation <> Value then
  begin
    FTextLocation := Value;
    Changed;
  end;
end;

procedure TAdvSmoothSlideShowItem.SetTextVerticalAlignment
  (const Value: TVerticalAlignment);
begin
  if FTextVerticalAlignment <> Value then
  begin
    FTextVerticalAlignment := Value;
    Changed;
  end;
end;

procedure TAdvSmoothSlideShowItem.SetTextWidth(const Value: integer);
begin
  if FTextWidth <> Value then
  begin
    FTextWidth := Value;
    Changed;
  end;
end;

procedure TAdvSmoothSlideShowItem.SetURL(const Value: String);
begin
  if FURL <> Value then
  begin
    FURL := Value;
    Changed;
  end;
end;

procedure TAdvSmoothSlideShowItem.SetVisible(const Value: boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    Changed;
  end;
end;

procedure TAdvSmoothSlideShowItem.SetWordWrap(const Value: boolean);
begin
  if FWordWrap <> Value then
  begin
    FWordWrap := Value;
    Changed;
  end;
end;

procedure TAdvSmoothSlideShowItem.Update;
begin
  FThumbThreadStatus := stInitialized;
  FThreadStatus := stInitialized;
  if Assigned(FOwner) then
    FOwner.RestartLoop := true;
end;

{ TAdvSmoothSlideShowItems }

function TAdvSmoothSlideShowItems.Add: TAdvSmoothSlideShowItem;
begin
  result := TAdvSmoothSlideShowItem( inherited Add);
end;

constructor TAdvSmoothSlideShowItems.Create(AOwner: TAdvSmoothSlideShow);
begin
  inherited Create(TAdvSmoothSlideShowItem);
  FOwner := AOwner;
end;

procedure TAdvSmoothSlideShowItems.Delete(Index: integer);
begin
  Items[Index].Free;
end;

function TAdvSmoothSlideShowItems.GetItem(Index: integer)
  : TAdvSmoothSlideShowItem;
begin
  result := TAdvSmoothSlideShowItem( inherited Items[Index]);
end;

function TAdvSmoothSlideShowItems.GetOwner: TPersistent;
begin
  result := FOwner;
end;

function TAdvSmoothSlideShowItems.Insert(Index: integer)
  : TAdvSmoothSlideShowItem;
begin
  result := TAdvSmoothSlideShowItem( inherited Insert(Index));
end;

procedure TAdvSmoothSlideShowItems.SetItem(Index: integer;
  const Value: TAdvSmoothSlideShowItem);
begin
  inherited Items[Index] := Value;
end;

procedure TAdvSmoothSlideShowItems.Update(Item: TCollectionItem);
begin
  inherited;
  FOwner.Changed;
end;

{ TAdvSmoothSlideShow }

procedure TAdvSmoothSlideShow.AddItemsFromFolder(AFolder: String;
  AKind: TSlideShowItemKind; DescriptionLocation: boolean = false;
  CaptionLocation: boolean = false; LocationThumbHint: boolean = false);
var
  SR: TSearchRec;

  procedure AddToList(s: string);
  begin
    if FileExists(s) then
    begin
      with Items.Add do
      begin
        FLocation := s;
        FKind := AKind;
        if CaptionLocation then
          FText := s;

        if LocationThumbHint then
          FHint := s;

        if DescriptionLocation then
          FDescription := s;
      end;
    end;
  end;

begin
  if FUpdateCount = 0 then
    StopThread;

  if FindFirst(AFolder, faAnyFile, SR) = 0 then
  begin
    AddToList(ExtractFilePath(AFolder) + SR.Name);
    while FindNext(SR) = 0 do
      AddToList(ExtractFilePath(AFolder) + SR.Name);
  end;
  FindClose(SR);

  if FUpdateCount = 0 then
    StartThread;
end;

procedure TAdvSmoothSlideShow.Animate(Sender: TObject);
begin
  if csDesigning in ComponentState then
    Exit;

  if (FItemIndex >= 0) and (FItemIndex <= Items.Count - 1) then
  begin
    if FDoAnimation then
    begin
      if AnimatePlaceHolder then
      begin
        if AnimateImage then
        begin
          FDoAnimation := false;
          if Assigned(Items[FItemIndex].Control) and
            (Items[FItemIndex].Kind = ikControl) then
            Items[FItemIndex].Control.Visible := true;

          if Rotation then
            FRotateTimer.Enabled := true;
        end;
      end;
    end;
  end;
end;

function TAdvSmoothSlideShow.AnimateImage: boolean;
var
  opccur, opc: Byte;
  dcur, d: Byte;
  bopc, b: boolean;
begin
  result := false;
  opccur := FCurrentOpacity;
  opc := FNextOpacity;
  dcur := Round(Abs(FCurrentOpacityTo - opccur) / max(1, FadeAnimationFactor));
  d := Round(Abs(FNextOpacityTo - opc) / max(1, FadeAnimationFactor));
  bopc := AnimateByte(opccur, FCurrentOpacityTo, dcur, 1);
  b := AnimateByte(opc, FNextOpacityTo, d, 1);
  if b or bopc then
  begin
    FAnimateImage := true;
    FCurrentItemIndex := FPreviousItemIndex;
    FNextItemIndex := FItemIndex;
    if FNextItemIndex > Items.Count then
      FNextItemIndex := 0;
    FCurrentOpacity := opccur;
    FNextOpacity := opc;
    Invalidate;
    if Assigned(OnAnimateImage) then
      OnAnimateImage(Self, FItemIndex);
  end
  else
  begin
    FCurrentItemIndex := FNextItemIndex;
    result := true;
    FAnimateImage := false;
    FCurrentOpacity := FCurrentOpacityTo;
    FNextOpacity := FNextOpacityTo;
    Invalidate;
    if Assigned(OnAnimateImageFinished) then
      OnAnimateImageFinished(Self, FCurrentItemIndex);
  end;
end;

function TAdvSmoothSlideShow.AnimatePlaceHolder: boolean;
var
  R, cur: TGPRectF;
  d: Double;
  bx, by, bw, bh: boolean;
begin
  if PlaceHolder and (stPlaceHolder in Transitions) and (FItemIndex >= 0) and
    (FItemIndex <= Items.Count - 1) then
  begin
    result := false;
    R := Items[FItemIndex].FPlaceHolderRect;
    cur := FCurrentPlaceHolder;
    d := Abs(R.X - cur.X) / max(1, HolderAnimationFactor);
    bx := AnimateDouble(cur.X, R.X, d, 1);
    d := Abs(R.Y - cur.Y) / max(1, HolderAnimationFactor);
    by := AnimateDouble(cur.Y, R.Y, d, 1);
    d := Abs(R.Width - cur.Width) / max(1, HolderAnimationFactor);
    bw := AnimateDouble(cur.Width, R.Width, d, 1);
    d := Abs(R.Height - cur.Height) / max(1, HolderAnimationFactor);
    bh := AnimateDouble(cur.Height, R.Height, d, 1);
    FAnimating := bx or by or bw or bh;
    if FAnimating then
    begin
      FAnimatePlaceHolder := true;
      FCurrentPlaceHolder := cur;
      if Assigned(OnAnimatePlaceHolder) then
        OnAnimatePlaceHolder(Self, FItemIndex);
      Invalidate;
    end
    else
    begin
      FCurrentItemIndex := FItemIndex;
      result := true;
      FCurrentPlaceHolder := R;
      FAnimatePlaceHolder := false;
      Invalidate;
      if Assigned(OnAnimatePlaceHolderFinished) then
        OnAnimatePlaceHolderFinished(Self, FCurrentItemIndex);
    end;
  end
  else
    result := true;
end;

procedure TAdvSmoothSlideShow.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TAdvSmoothSlideShow.BuildItems;
var
  i: integer;
  w, h, nw, nh, ow, oh: Double;
  imgr, plcr: TGPRectF;
  spc, spcholder, spcthumb: integer;
  R, rthumb: TGPRectF;
  tx, ty: Double;
  cnt: integer;
begin
  if Items.Count = 0 then
    Exit;

  spc := PlaceHolderSpacing;
  spcthumb := ThumbNailSpacing;
  spcholder := 0;
  if ThumbNails then
  begin
    case ThumbNailPosition of
      tpRight, tpLeft:
        spcholder := ThumbNailHolderMargin.Left + ThumbNailHolderMargin.Right +
          spcthumb * 2 + ThumbNailWidth;
      tpBottom, tpTop:
        spcholder := ThumbNailHolderMargin.Top + ThumbNailHolderMargin.Bottom +
          spcthumb * 2 + ThumbNailHeight;
    end;
  end;

  if not ThumbNailsFloating then
  begin
    case ThumbNailPosition of
      tpLeft:
        R := MakeRect(Margin.Left + spc + spcholder, Margin.Top + spc,
          Width - Margin.Right - Margin.Left - spcholder - spc * 2,
          Height - Margin.Bottom - Margin.Top - spc * 2);
      tpTop:
        R := MakeRect(Margin.Left + spc, Margin.Top + spcholder + spc,
          Width - Margin.Right - Margin.Left - spc * 2,
          Height - Margin.Bottom - Margin.Top - spcholder - spc * 2);
      tpRight:
        R := MakeRect(Margin.Left + spc, Margin.Top + spc,
          Width - Margin.Right - spcholder - Margin.Left - spc * 2,
          Height - Margin.Bottom - Margin.Top - spc * 2);
      tpBottom:
        R := MakeRect(Margin.Left + spc, Margin.Top + spc,
          Width - Margin.Right - Margin.Left - spc * 2,
          Height - Margin.Bottom - Margin.Top - spcholder - spc * 2);
    end;
  end
  else
  begin
    R := MakeRect(Margin.Left + spc, Margin.Top + spc,
      Width - Margin.Right - Margin.Left - spc * 2,
      Height - Margin.Bottom - Margin.Top - spc * 2);
  end;

  rthumb := MakeRect(0, 0, 0, 0);
  if ThumbNails then
    rthumb := GetThumbNailRect;

  tx := rthumb.X + spcthumb;
  ty := rthumb.Y + spcthumb;
  case ThumbNailPosition of
    tpLeft, tpRight:
      ty := ty - ScrollPos;
    tpTop, tpBottom:
      tx := tx - ScrollPos;
  end;

  cnt := 0;
  for i := 0 to Items.Count - 1 do
  begin
    with Items[i] do
    begin
      if Assigned(Image) and Visible and (FThumbThreadStatus = stLoaded) then
      begin
        Inc(cnt);
        ow := Image.Width;
        oh := Image.Height;
        nw := R.Width;
        nh := R.Height;
        GetAspectSize(w, h, ow, oh, nw, nh, StretchPercentage);
        imgr := MakeRect(R.X + (R.Width - w) / 2, R.Y + (R.Height - h) / 2, w,
          h);
        plcr := MakeRect(imgr.X - spc, imgr.Y - spc, imgr.Width + spc * 2,
          imgr.Height + spc * 2);
        FPlaceHolderRect := plcr;
        FItemRect := imgr;

        FThumbItemRect := MakeRect(tx, ty, ThumbNailWidth, ThumbNailHeight);
        case ThumbNailPosition of
          tpLeft, tpRight:
            ty := ty + ThumbNailHeight + spcthumb;
          tpBottom, tpTop:
            tx := tx + ThumbNailWidth + spcthumb;
        end;

        if Assigned(FThumbnail) and ThumbNails then
        begin
          FThumbRect := MakeRect(FThumbItemRect.X + 3 +
              (FThumbItemRect.Width - FThumbnail.Width - 3) / 2,
            FThumbItemRect.Y + 3 +
              (FThumbItemRect.Height - FThumbnail.Height - 3) / 2,
            FThumbnail.Width - 3, FThumbnail.Height - 3);
        end
        else
          FThumbRect := MakeRect(0, 0, 0, 0); // MakeRect(FThumbItemRect.X, FThumbItemRect.Y, FThumbItemRect.Width, FThumbItemRect.Height);

        if Assigned(Control) and (Kind = ikControl) then
        begin
          Control.Left := Round(FItemRect.X + (FItemRect.Width - Control.Width)
              / 2);
          Control.Top := Round
            (FItemRect.Y + (FItemRect.Height - Control.Height) / 2);
        end;
      end;
    end;
  end;

  if (itemindex >= 0) and (itemindex <= Items.Count - 1) then
    FCurrentPlaceHolder := Items[itemindex].FPlaceHolderRect;

  case ThumbNailPosition of
    tpRight, tpLeft:
      MaxScrollPos := max((ThumbNailSpacing * (cnt + 1)) +
          (ThumbNailHeight * cnt) - rthumb.Height, 0);
    tpBottom, tpTop:
      MaxScrollPos := max((ThumbNailSpacing * (cnt + 1)) +
          (ThumbNailWidth * cnt) - rthumb.Width, 0);
  end;
end;

procedure TAdvSmoothSlideShow.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('TMSStyle',ReadTMSStyle, WriteTMSStyle, True);
end;

procedure TAdvSmoothSlideShow.ReadTMSStyle(Reader: TReader);
begin
  FTMSStyle := TTMSStyle(Reader.ReadInteger);
end;

procedure TAdvSmoothSlideShow.WriteTMSStyle(Writer: TWriter);
begin
  Writer.WriteInteger(integer(FTMSStyle));
end;

procedure TAdvSmoothSlideShow.Changed;
begin
  if FUpdateCount = 0 then
  begin
    BuildItems;
    Invalidate;
  end;
end;

procedure TAdvSmoothSlideShow.CMHintShow(var Message: TMessage);
var
  Item: integer;
  pt: TPoint;
  dItem: TAdvSmoothSlideShowItem;
begin
  with TCMHintShow(Message).HintInfo^ do
  begin
    HintStr := self.Hint;
    pt := CursorPos;
    Item := XYToThumb(pt.X, pt.Y);
    if Item <> -1 then
    begin
      dItem := Items[Item];
      if dItem <> nil then
      begin
        if dItem.Hint <> '' then
          HintStr := dItem.Hint;
        if Assigned(FOnThumbHint) then
          FOnThumbHint(self, Item, HintStr);
      end;
    end;

    ReshowTimeout := 0;
  end;
end;

procedure TAdvSmoothSlideShow.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  FTimeDownOnArrow := 0;
  Application.CancelHint;
  ShowDes := false;
  Changed;
end;

constructor TAdvSmoothSlideShow.Create(AOwner: TComponent);
var
  i, k: integer;
begin
  inherited;
  ControlStyle := ControlStyle + [csAcceptsControls];
  FDesignTime := (csDesigning in ComponentState) and not
    ((csReading in Owner.ComponentState) or (csLoading in Owner.ComponentState)
    );

  FhoveredItem := -1;
  FhoveredDescription := -1;
  DoubleBuffered := true;
  FAnimation := true;
  FRotation := true;
  FItems := TAdvSmoothSlideShowItems.Create(self);
  FItems.OnChange := ItemsChanged;
  {$IFNDEF DELPHI2006_LVL}
  FThumbNailHolderMargin := TMargins.Create(self);
  {$ELSE}
  FThumbNailHolderMargin := TMargins.Create(nil);
  {$ENDIF}
  FThumbNailHolderMargin.OnChange := MarginChanged;
  FFill := TGDIPFill.Create;
  FFill.OnChange := FillChanged;
  FPlaceHolderFill := TGDIPFill.Create;
  FPlaceHolderFill.OnChange := PlaceHolderFillChanged;
  {$IFNDEF DELPHI2006_LVL}
  FMargin := TMargins.Create(Self);
  {$ELSE}
  FMargin := TMargins.Create(nil);
  {$ENDIF}
  FMargin.OnChange := MarginChanged;
  FMargin.Left := 10;
  FMargin.Top := 10;
  FMargin.Bottom := 10;
  FMargin.Right := 10;
  FPlaceHolderSpacing := 40;
  FThumbNailHandles := THandleAppearance.Create(self);
  FThumbNailHandles.OnChange := HandlesChanged;
  FThumbNailWidth := 100;
  FThumbNailHolderFill := TGDIPFill.Create;
  FThumbNailHolderFill.OnChange := ThumbNailFillChanged;
  FThumbNailHolderFill.BeginUpdate;
  FThumbNailHolderFill.GradientType := gtSolid;
  FThumbNailHolderFill.Color := RGB(240, 240, 240); ;
  FThumbNailHolderFill.BorderColor := clSilver;
  FThumbNailHolderFill.Glow := gmGradient;
  FThumbNailHolderFill.EndUpdate;
  FThumbNailSpacing := 10;
  FThumbNails := true;
  FThumbNailHeight := 100;
  FThumbnailPosition := tpBottom;
  FAnimations := [saLeftToRight];
  FThumbNailSelectedFill := TGDIPFill.Create;
  FThumbNailSelectedFill.Color := $AAD9FF;
  FThumbNailSelectedFill.ColorTo := $6EBBFF;
  FThumbNailSelectedFill.ColorMirror := $42AEFE;
  FThumbNailSelectedFill.ColorMirrorTo := $7AE1FE;
  FThumbNailSelectedFill.BorderColor := $42AEFE;
  FThumbNailSelectedFill.GradientType := gtVertical;
  FThumbNailSelectedFill.GradientMirrorType := gtVertical;
  FThumbNailSelectedFill.OnChange := ThumbNailFillChanged;
  FPlaceHolderVisible := true;
  FThumbNailsFloating := false;
  FLoadEffect := true;
  FStretchPercentage := 100;
  FDescriptionFill := TGDIPFill.Create;
  FDescriptionFill.OnChange := FillChanged;
  FDescriptionFill.BeginUpdate;
  FDescriptionFill.Color := clWhite;
  FDescriptionFill.GradientType := gtSolid;
  FDescriptionFill.BorderColor := clSilver;
  FDescriptionFill.EndUpdate;

  TabStop := true;

  for i := 0 to 4 do
    for k := 0 to 4 do
      FColorMatrix[i, k] := 0.0;

  FColorMatrix[0, 0] := 1.0;
  FColorMatrix[1, 1] := 1.0;
  FColorMatrix[2, 2] := 1.0;
  FColorMatrix[3, 3] := 1.0; // OPACITY
  FColorMatrix[4, 4] := 1.0;

  FFill.BeginUpdate;
  FFill.Color := clWhite;
  FFill.GradientType := gtSolid;
  FFill.BorderColor := clSilver;
  FFill.EndUpdate;
  FPlaceHolderFill.BeginUpdate;
  FPlaceHolderFill.GradientType := gtSolid;
  FPlaceHolderFill.Color := clWhite;
  FPlaceHolderFill.BorderColor := clSilver;
  FPlaceHolderFill.EndUpdate;

  FTransitions := [stFade, stPlaceHolder];

  FPreviousItemIndex := 0;
  FItemIndex := 0;
  FCurrentItemIndex := 0;
  FNextItemIndex := 0;
  FItemMode := imNormal;
  FAspectRatio := true;
  FCurrentPlaceHolder := MakeRect(0, 0, 0, 0);
  FAnimatePlaceHolder := false;
  FFadeAnimationFactor := 6;
  FHolderAnimationFactor := 6;
  FRotationInterval := 3500;

  FCurrentOpacity := 255;
  FCurrentOpacityTo := 0;
  FNextOpacity := 0;
  FNextOpacityTo := 255;

  FShowDescription := True;

  FAnimateTimer := TTimer.Create(self);
  FAnimateTimer.Interval := 1;
  FAnimateTimer.OnTimer := Animate;
  FAnimateTimer.Enabled := true;

  FRotateTimer := TTimer.Create(self);
  FRotateTimer.Interval := 3500;
  FRotateTimer.OnTimer := Rotate;
  FRotateTimer.Enabled := true;

  FHandleTimer := TTimer.Create(self);
  FHandleTimer.OnTimer := HandleTimerChanged;
  FHandleTimer.Enabled := true;
  FHandleTimer.Interval := 10;

  Width := 500;
  Height := 400;
end;

procedure TAdvSmoothSlideShow.CreateWnd;
begin
  inherited;
  if not FDesignTime then
  begin
    StopThread;
    FThreadDone := false;
    FHandleOk := true;
    StartThread;
  end;
end;

procedure TAdvSmoothSlideShow.DblClick;
var
  pos: TPoint;
  i: integer;
begin
  inherited;
  pos := ScreenToClient(Mouse.CursorPos);
  i := XYToThumb(pos.X, pos.Y);
  if i <> -1 then
  begin
    if Assigned(OnThumbDblClick) then
      OnThumbDblClick(self, i);
  end;
end;

destructor TAdvSmoothSlideShow.Destroy;
begin
  StopThread;
  if Assigned(FCacheBackGround) then
    FCacheBackGround.Free;
  FHandleTimer.Free;
  FRotateTimer.Free;
  FAnimateTimer.Free;
  FThumbNailHandles.Free;
  FMargin.Free;
  FThumbNailHolderMargin.Free;
  FThumbNailSelectedFill.Free;
  FThumbNailHolderFill.Free;
  FPlaceHolderFill.Free;
  FDescriptionFill.Free;
  FFill.Free;
  FItems.Free;
  inherited;
end;

procedure TAdvSmoothSlideShow.DoExit;
begin
  inherited;
  Application.CancelHint;
end;

procedure TAdvSmoothSlideShow.Draw(g: TGPGraphics);
begin
  if (FItemIndex >= 0) and (FItemIndex <= Items.Count - 1) then
  begin
    if (stPlaceHolder in Transitions) and FAnimatePlaceHolder then
      Items[FItemIndex].DrawPlaceHolder(g, FCurrentPlaceHolder, 255)
    else
      Items[FItemIndex].DrawPlaceHolder(g, Items[FItemIndex].FPlaceHolderRect,
        255);
  end;

  if (FCurrentItemIndex >= 0) and (FCurrentItemIndex <= Items.Count - 1) then
  begin
    if (FNextItemIndex >= 0) and (FNextItemIndex <= Items.Count - 1)
      and FAnimateImage then
    begin
      Items[FNextItemIndex].DrawItem(g, Items[FNextItemIndex].FItemRect,
        FNextOpacity);
      Items[FCurrentItemIndex].DrawItem(g, Items[FCurrentItemIndex].FItemRect,
        FCurrentOpacity);
    end
    else
    begin
      if (Assigned(Items[FNextItemIndex].Control) and
          (Items[FNextItemIndex].Kind = ikControl)) then
      begin
        if Items[FNextItemIndex].Control.Visible = false then
          Items[FNextItemIndex].DrawItem
            (g, Items[FNextItemIndex].FItemRect, 255);
      end
      else
        Items[FNextItemIndex].DrawItem(g, Items[FNextItemIndex].FItemRect, 255);
    end;
  end;
end;

procedure TAdvSmoothSlideShow.DrawBackGround(g: TGPGraphics);
var
  gbmp: TGPGraphics;
begin
  if not FCacheBG then
  begin
    if Assigned(FCacheBackGround) then
      FCacheBackGround.Free;

    FCacheBackGround := TGPBitmap.Create(Width, Height);

    gbmp := g.FromImage(FCacheBackGround);
    Fill.Fill(gbmp, MakeRect(0, 0, Width - 1, Height - 1));
    gbmp.Free;
    FCacheBG := true;
    g.DrawImage(FCacheBackGround, 0, 0);
  end
  else
  begin
    if Assigned(FCacheBackGround) then
    begin
      g.DrawImage(FCacheBackGround, 0, 0);
    end;
  end;
end;

procedure TAdvSmoothSlideShow.DrawHandles(g: TGPGraphics);
var
  chl, chr, cvt, cvb: TColor;
  p: TGPPen;
  b: TGPBrush;
  path: TGPGraphicsPath;
  rhl, rhr, rvt, rvb: TGPRectF;
  doHandleVertTop, doHandleVertBottom, doHandleHorzLeft,
    doHandleHorzRight: boolean;
begin
  with ThumbNailHandles do
  begin
    doHandleVertTop := IsHandleVertTop;
    doHandleVertBottom := IsHandleVertBottom;
    doHandleHorzLeft := IsHandleHorzLeft;
    doHandleHorzRight := IsHandleHorzRight;
    rhl := GetHandleHorzLeft;
    rhr := GetHandleHorzRight;
    rvt := GetHandleVertTop;
    rvb := GetHandleVertBottom;

    chl := Color;
    if FHandleHorzLeftDown then
      chl := Darker(chl, 30)
    else if FHandleHorzLeftHover then
      chl := Lighter(chl, 30);

    chr := Color;
    if FHandleHorzRightDown then
      chr := Darker(chl, 30)
    else if FHandleHorzRightHover then
      chr := Lighter(chl, 30);

    cvt := Color;
    if FHandleVertTopDown then
      cvt := Darker(cvt, 30)
    else if FHandleVertTopHover then
      cvt := Lighter(cvt, 30);

    cvb := Color;
    if FHandleVertBottomDown then
      cvb := Darker(cvb, 30)
    else if FHandleVertBottomHover then
      cvb := Lighter(cvb, 30);

    if doHandleHorzLeft then
    begin
      // horizontal left
      path := TGPGraphicsPath.Create;
      path.AddArc(rhl, 270, 180);
      path.CloseFigure;

      b := TGPSolidBrush.Create(MakeColor(Opacity, chl));
      g.FillPath(b, path);
      b.Free;

      b := TGPLinearGradientBrush.Create
        (MakeRect(rhl.X, rhl.Y, rhl.Height, rhl.Width / 2), MakeColor
          (100, clWhite), MakeColor(0, clWhite), LinearGradientModeHorizontal);
      g.FillPath(b, path);
      b.Free;

      p := TGPPen.Create(MakeColor(Opacity, BorderColor), 1);
      g.DrawPath(p, path);
      p.Free;

      path.Free;

      path := TGPGraphicsPath.Create;
      path.AddLine(MakePoint(rhl.X + rhl.Width * 2 / 3, rhl.Y + rhl.Height / 2)
          , MakePoint(rhl.X + rhl.Width * 4 / 5, rhl.Y + rhl.Height / 3));
      path.AddLine(MakePoint(rhl.X + rhl.Width * 2 / 3, rhl.Y + rhl.Height / 2)
          , MakePoint(rhl.X + rhl.Width * 4 / 5, rhl.Y + rhl.Height * 2 / 3));

      p := TGPPen.Create(MakeColor(Opacity, ArrowColor), 2);
      g.DrawPath(p, path);
      p.Free;

      path.Free;

    end;

    if doHandleHorzRight then
    begin
      // horizontal Right
      path := TGPGraphicsPath.Create;
      path.AddArc(rhr, 90, 180);
      path.CloseFigure;

      b := TGPSolidBrush.Create(MakeColor(Opacity, chr));
      g.FillPath(b, path);
      b.Free;

      b := TGPLinearGradientBrush.Create
        (MakeRect(rhr.X, rhr.Y, rhr.Height, rhr.Width / 2), MakeColor
          (100, clWhite), MakeColor(0, clWhite), LinearGradientModeHorizontal);
      g.FillPath(b, path);
      b.Free;

      p := TGPPen.Create(MakeColor(Opacity, BorderColor), 1);
      g.DrawPath(p, path);
      p.Free;

      path.Free;

      path := TGPGraphicsPath.Create;
      path.AddLine(MakePoint(rhr.X + rhr.Width / 3, rhr.Y + rhr.Height / 2),
        MakePoint(rhr.X + rhr.Width / 5, rhr.Y + rhr.Height / 3));
      path.AddLine(MakePoint(rhr.X + rhr.Width / 3, rhr.Y + rhr.Height / 2),
        MakePoint(rhr.X + rhr.Width / 5, rhr.Y + rhr.Height * 2 / 3));

      p := TGPPen.Create(MakeColor(Opacity, ArrowColor), 2);
      g.DrawPath(p, path);
      p.Free;

      path.Free;
    end;

    if doHandleVertTop then
    begin
      // Vertical Top
      path := TGPGraphicsPath.Create;
      path.AddArc(rvt, 0, 180);
      path.CloseFigure;

      b := TGPSolidBrush.Create(MakeColor(Opacity, cvt));
      g.FillPath(b, path);
      b.Free;

      b := TGPLinearGradientBrush.Create
        (MakeRect(rvt.X, rvt.Y, rvt.Width, rvt.Height / 2), MakeColor
          (100, clWhite), MakeColor(0, clWhite), LinearGradientModeHorizontal);
      g.FillPath(b, path);
      b.Free;

      p := TGPPen.Create(MakeColor(Opacity, BorderColor), 1);
      g.DrawPath(p, path);
      p.Free;

      path.Free;

      path := TGPGraphicsPath.Create;
      path.AddLine(MakePoint(rvt.X + rvt.Width / 2, rvt.Y + rvt.Height * 2 / 3)
          , MakePoint(rvt.X + rhl.Width / 3, rvt.Y + rvt.Height * 4 / 5));
      path.AddLine(MakePoint(rvt.X + rvt.Width / 2, rvt.Y + rvt.Height * 2 / 3)
          , MakePoint(rvt.X + rhl.Width * 2 / 3, rvt.Y + rvt.Height * 4 / 5));

      p := TGPPen.Create(MakeColor(Opacity, ArrowColor), 2);
      g.DrawPath(p, path);
      p.Free;

      path.Free;
    end;

    if doHandleVertBottom then
    begin
      // Vertical Bottom
      path := TGPGraphicsPath.Create;
      path.AddArc(rvb, 180, 180);
      path.CloseFigure;

      b := TGPSolidBrush.Create(MakeColor(Opacity, cvb));
      g.FillPath(b, path);
      b.Free;

      b := TGPLinearGradientBrush.Create
        (MakeRect(rvb.X, rvb.Y, rvb.Height, rvb.Width / 2), MakeColor
          (100, clWhite), MakeColor(0, clWhite), LinearGradientModeHorizontal);
      g.FillPath(b, path);
      b.Free;

      p := TGPPen.Create(MakeColor(Opacity, BorderColor), 1);
      g.DrawPath(p, path);
      p.Free;

      path.Free;

      path := TGPGraphicsPath.Create;
      path.AddLine(MakePoint(rvb.X + rvb.Width / 2, rvb.Y + rvb.Height / 3),
        MakePoint(rvb.X + rvb.Width / 3, rvb.Y + rvb.Height / 5));
      path.AddLine(MakePoint(rvb.X + rvb.Width / 2, rvb.Y + rvb.Height / 3),
        MakePoint(rvb.X + rvb.Width * 2 / 3, rvb.Y + rvb.Height / 5));

      p := TGPPen.Create(MakeColor(Opacity, ArrowColor), 2);
      g.DrawPath(p, path);
      p.Free;

      path.Free;
    end;
  end;
end;

procedure TAdvSmoothSlideShow.DrawThumbs(g: TGPGraphics);
var
  rthumb: TGPRectF;
  i: integer;
  rgn: TGPRegion;
begin
  if ThumbNails then
  begin
    rthumb := GetThumbNailRect;
    FThumbNailHolderFill.Fill(g, rthumb);

    rgn := TGPRegion.Create(rthumb);
    g.SetClip(rgn);

    for i := 0 to Items.Count - 1 do
    begin
      with Items[i] do
      begin
        if Visible and Assigned(FThumbnail) and RectanglesInterSect
          (rthumb, FThumbItemRect) then
        begin
          if i = itemindex then
            ThumbNailSelectedFill.Fill(g, FThumbItemRect);

          g.DrawImage(FThumbnail, FThumbRect);
        end;
      end;
    end;

    g.ResetClip;
    rgn.Free;

    DrawHandles(g);
  end;
end;

procedure TAdvSmoothSlideShow.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then
    Changed;

  StartThread;
end;

procedure TAdvSmoothSlideShow.FillChanged(Sender: TObject);
begin
  FCacheBG := false;
  Changed;
end;

procedure TAdvSmoothSlideShow.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  FFullScreen := nil;
  Align := FControlAlign;
  Left := FControlLeft;
  Top := FControlTop;
  Width := FControlWidth;
  Height := FControlHeight;
  Parent := FControlParent;
  Changed;
end;

procedure TAdvSmoothSlideShow.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE:
      Normal;
  end;
end;

procedure TAdvSmoothSlideShow.FullScreen;
begin
  if not Assigned(FFullScreen) then
  begin
    FFullScreen := TForm.Create(self);
    FFullScreen.OnKeyDown := FormKeyDown;
    FFullScreen.KeyPreview := true;
    FFullScreen.OnClose := FormClose;
    FFullScreen.FormStyle := fsStayOnTop;
    FFullScreen.WindowState := wsMaximized;
    FFullScreen.BorderStyle := bsNone;
    FControlParent := Parent;
    FControlAlign := Align;
    FControlLeft := Left;
    FControlTop := Top;
    FControlWidth := Width;
    FControlHeight := Height;
    FFullScreen.Show;
    Parent := FFullScreen;
    Align := alClient;
    Changed;
  end;
end;

function TAdvSmoothSlideShow.GetAnimationCount: integer;
var
  i: TSlideShowAnimation;
begin
  result := 0;
  for i := low(TSlideShowAnimation) to high(TSlideShowAnimation) do
  begin
    if (i in Animations) then
      Inc(result);
  end;
end;

function TAdvSmoothSlideShow.GetComponentStyle: TTMSStyle;
begin
  Result := FTMSStyle;
end;

function TAdvSmoothSlideShow.GetCountSelectable: integer;
var
  i: integer;
begin
  result := 0;
  for i := 0 to Items.Count - 1 do
    if Items[i].Visible and Items[i].Enabled then
      Inc(result);
end;

function TAdvSmoothSlideShow.GetFadeAnimationFactor: integer;
begin
  if Animation then
    Result := FFadeAnimationFactor
  else
    Result := 0;
end;

function TAdvSmoothSlideShow.GetRandomAnimation: TSlideShowAnimation;
var
  i: TSlideShowAnimation;
  n, R: integer;
begin
  result := low(TSlideShowAnimation);
  R := 0;
  Randomize;
  n := Random(GetAnimationCount);

  for i := low(TSlideShowAnimation) to high(TSlideShowAnimation) do
  begin
    if (i in Animations) then
    begin
      if (R = n) then
      begin
        result := i;
        break;
      end;
      Inc(R);
    end;
  end;
end;

function TAdvSmoothSlideShow.GetHandleHorzLeft: TGPRectF;
var
  R: TGPRectF;
begin
  R := GetThumbNailRect;
  result := MakeRect(R.X - ThumbNailHandles.Size / 2, R.Y +
      (R.Height - ThumbNailHandles.Size) / 2, ThumbNailHandles.Size,
    ThumbNailHandles.Size);
end;

function TAdvSmoothSlideShow.GetHandleHorzRight: TGPRectF;
var
  R: TGPRectF;
begin
  R := GetThumbNailRect;
  result := MakeRect(R.X + (R.Width - ThumbNailHandles.Size / 2) - 1, R.Y +
      (R.Height - ThumbNailHandles.Size) / 2, ThumbNailHandles.Size,
    ThumbNailHandles.Size);
end;

function TAdvSmoothSlideShow.GetHandleVertBottom: TGPRectF;
var
  R: TGPRectF;
begin
  R := GetThumbNailRect;
  result := MakeRect(R.X + (R.Width - ThumbNailHandles.Size) / 2,
    R.Y + R.Height - 1 - ThumbNailHandles.Size / 2, ThumbNailHandles.Size,
    ThumbNailHandles.Size);
end;

function TAdvSmoothSlideShow.GetHandleVertTop: TGPRectF;
var
  R: TGPRectF;
begin
  R := GetThumbNailRect;
  result := MakeRect(R.X + (R.Width - ThumbNailHandles.Size) / 2,
    R.Y - ThumbNailHandles.Size / 2, ThumbNailHandles.Size,
    ThumbNailHandles.Size);
end;

function TAdvSmoothSlideShow.GetMaxSelectable: integer;
var
  i: integer;
begin
  result := 0;
  for i := Items.Count - 1 downto 0 do
  begin
    if Items[i].Visible and Items[i].Enabled then
    begin
      result := i;
      break;
    end;
  end;
end;

function TAdvSmoothSlideShow.GetMinSelectable: integer;
var
  i: integer;
begin
  result := 0;
  for i := 0 to Items.Count - 1 do
  begin
    if Items[i].Visible and Items[i].Enabled then
    begin
      result := i;
      break;
    end;
  end;
end;

function TAdvSmoothSlideShow.GetThumbNailRect: TGPRectF;
var
  spcthumb: integer;
begin
  spcthumb := ThumbNailSpacing;
  case ThumbNailPosition of
    tpLeft:
      result := MakeRect(Margin.Left + ThumbNailHolderMargin.Left,
        Margin.Top + ThumbNailHolderMargin.Top, spcthumb * 2 + ThumbNailWidth,
        Height - ThumbNailHolderMargin.Bottom - ThumbNailHolderMargin.Top -
          Margin.Top - Margin.Bottom);
    tpTop:
      result := MakeRect(Margin.Left + ThumbNailHolderMargin.Left,
        Margin.Top + ThumbNailHolderMargin.Top,
        Width - ThumbNailHolderMargin.Right - ThumbNailHolderMargin.Left -
          Margin.Left - Margin.Right, spcthumb * 2 + ThumbNailHeight);
    tpRight:
      result := MakeRect(Width - Margin.Right - ThumbNailHolderMargin.Right -
          spcthumb * 2 - ThumbNailWidth,
        Margin.Top + ThumbNailHolderMargin.Top, spcthumb * 2 + ThumbNailWidth,
        Height - ThumbNailHolderMargin.Bottom - ThumbNailHolderMargin.Top -
          Margin.Top - Margin.Bottom);
    tpBottom:
      result := MakeRect(Margin.Left + ThumbNailHolderMargin.Left,
        Height - Margin.Bottom - ThumbNailHolderMargin.Bottom - spcthumb * 2 -
          ThumbNailHeight, Width - ThumbNailHolderMargin.Right -
          ThumbNailHolderMargin.Left - Margin.Left - Margin.Right,
        spcthumb * 2 + ThumbNailHeight);
  end;
end;

function TAdvSmoothSlideShow.GetThumbNailRectangle: TRect;
var
  rthumb: TGPRectF;
begin
  rthumb := GetThumbNailRect;
  result := Bounds(Round(rthumb.X), Round(rthumb.Y), Round(rthumb.Width), Round
      (rthumb.Height));
end;

function TAdvSmoothSlideShow.GetVersion: string;
var
  vn: integer;
begin
  vn := GetVersionNr;
  result := IntToStr(Hi(Hiword(vn))) + '.' + IntToStr(Lo(Hiword(vn)))
    + '.' + IntToStr(Hi(Loword(vn))) + '.' + IntToStr(Lo(Loword(vn)));
end;

function TAdvSmoothSlideShow.GetVersionNr: integer;
begin
  result := MakeLong(MakeWord(BLD_VER, REL_VER), MakeWord(MIN_VER, MAJ_VER));
end;

procedure TAdvSmoothSlideShow.HandlesChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothSlideShow.HandleTimerChanged(Sender: TObject);
begin
  if FHandleHorzLeftDown or FHandleHorzRightDown or FHandleVertTopDown or
    FHandleVertBottomDown then
  begin
    Inc(FTimeDownOnArrow);
    if FTimeDownOnArrow >= 10 then
    begin
      FDoHandleTimer := true;
      if FHandleHorzLeftDown and IsHandleHorzLeft then
        ScrollPos := ScrollPos - ThumbNailWidth - ThumbNailSpacing;

      if FHandleHorzRightDown and IsHandleHorzRight then
        ScrollPos := ScrollPos + ThumbNailWidth + ThumbNailSpacing;

      if FHandleVertTopDown and IsHandleVertTop then
        ScrollPos := ScrollPos - ThumbNailHeight - ThumbNailSpacing;

      if FHandleVertBottomDown and IsHandleVertBottom then
        ScrollPos := ScrollPos + ThumbNailHeight + ThumbNailSpacing;
    end;
  end;
end;

procedure TAdvSmoothSlideShow.MarginChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothSlideShow.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  Item: integer;
begin
  inherited;

  SetFocus;
  FTimeDownOnArrow := 0;
  if IsHandleHorzLeft then
  begin
    FHandleHorzLeftDown := PtInGPRect(GetHandleHorzLeft, Point(X, Y));
    PaintThumbnails;
  end;

  if IsHandleHorzRight then
  begin
    FHandleHorzRightDown := PtInGPRect(GetHandleHorzRight, Point(X, Y));
    PaintThumbnails;
  end;

  if IsHandleVertTop then
  begin
    FHandleVertTopDown := PtInGPRect(GetHandleVertTop, Point(X, Y));
    PaintThumbnails;
  end;

  if IsHandleVertBottom then
  begin
    FHandleVertBottomDown := PtInGPRect(GetHandleVertBottom, Point(X, Y));
    PaintThumbnails;
  end;

  if FHandleHorzLeftDown or FHandleHorzRightDown or FHandleVertTopDown or
    FHandleVertBottomDown then
    Exit;

  Item := XYToThumb(X, Y);
  if Item <> -1 then
  begin

  end;
end;

procedure TAdvSmoothSlideShow.MouseMove(Shift: TShiftState; X, Y: integer);
var
  Item, thumb: integer;
  state: boolean;
begin
  inherited;

  state := PtInGPRect(GetHandleHorzLeft, Point(X, Y)) and IsHandleHorzLeft;
  if FHandleHorzLeftHover <> state then
  begin
    FHandleHorzLeftHover := state;
    PaintThumbnails;
  end;

  state := PtInGPRect(GetHandleHorzRight, Point(X, Y)) and IsHandleHorzRight;
  if FHandleHorzRightHover <> state then
  begin
    FHandleHorzRightHover := state;
    PaintThumbnails;
  end;

  state := PtInGPRect(GetHandleVertTop, Point(X, Y)) and IsHandleVertTop;
  if FHandleVertTopHover <> state then
  begin
    FHandleVertTopHover := state;
    PaintThumbnails;
  end;

  FHandleVertBottomHover := PtInGPRect(GetHandleVertBottom, Point(X, Y))
    and IsHandleVertBottom;
  if FHandleVertBottomHover <> state then
  begin
    FHandleVertBottomHover := state;
    PaintThumbnails;
  end;

  if FHandleHorzLeftHover or FHandleHorzRightHover or FHandleVertTopHover or
    FHandleVertBottomHover then
  begin
    Cursor := crDefault;
    Exit;
  end;

  thumb := XYToThumb(X, Y);

  if thumb <> -1 then
  begin
    if thumb <> FhoveredItem then
    begin
      Application.CancelHint;
      FhoveredItem := thumb;
      Cursor := crHandPoint;
      Changed;
    end;
  end
  else
    Cursor := crDefault;

  Item := XYToCurrentItem(X, Y);

  if Item <> -1 then
  begin
    if Item <> FhoveredDescription then
    begin
      FhoveredDescription := Item;
      ShowDes := true;
      Changed;
    end;
  end
  else if Item <> FhoveredDescription then
  begin
    FhoveredDescription := Item;
    ShowDes := false;
    Changed;
  end;
end;

procedure TAdvSmoothSlideShow.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
var
  Item: integer;
begin
  inherited;
  FTimeDownOnArrow := 0;
  if not FDoHandleTimer then
  begin
    if FHandleHorzLeftDown then
      ScrollPos := ScrollPos - ThumbNailWidth - ThumbNailSpacing;

    if FHandleHorzRightDown then
      ScrollPos := ScrollPos + ThumbNailWidth + ThumbNailSpacing;

    if FHandleVertTopDown then
      ScrollPos := ScrollPos - ThumbNailHeight - ThumbNailSpacing;

    if FHandleVertBottomDown then
      ScrollPos := ScrollPos + ThumbNailHeight + ThumbNailSpacing;
  end
  else
    FDoHandleTimer := false;

  if not FHandleHorzLeftDown and not FHandleHorzRightDown and not
    FHandleVertTopDown and not FHandleVertBottomDown then
  begin
    Item := XYToThumb(X, Y);
    if Item <> -1 then
    begin
      itemindex := Item;
      if Assigned(OnThumbClick) then
        OnThumbClick(self, itemindex);
    end;
  end
  else
  begin
    FHandleHorzLeftDown := false;
    FHandleHorzRightDown := false;
    FHandleVertTopDown := false;
    FHandleVertBottomDown := false;
    Invalidate;
  end;
end;

procedure TAdvSmoothSlideShow.Next;
begin
  itemindex := itemindex + 1;
end;

procedure TAdvSmoothSlideShow.Normal;
begin
  if Assigned(FFullScreen) then
    FFullScreen.Close;
end;

procedure TAdvSmoothSlideShow.Notification(AComponent: TComponent;
  AOperation: TOperation);
var
  i: integer;
begin
  inherited;
  if (csDestroying in ComponentState) then
    Exit;

  if (AOperation = opRemove) then
  begin
    for i := 0 to Items.Count - 1 do
    begin
      if (AComponent = Items[i].FControl) then
        Items[i].FControl := nil;
    end;
  end;
end;

procedure TAdvSmoothSlideShow.Paint;
var
  g: TGPGraphics;
begin
  g := TGPGraphics.Create(Canvas.Handle);
  g.SetSmoothingMode(SmoothingModeAntiAlias);
  DrawBackGround(g);
  Draw(g);
  DrawThumbs(g);

  g.Free;
end;

procedure TAdvSmoothSlideShow.PaintThumbnails;
var
  g, gbmp: TGPGraphics;
  bmp: TGPBitmap;
begin
  if ThumbNails then
  begin
    bmp := TGPBitmap.Create(Width, Height);
    gbmp := TGPGraphics.Create(bmp);
    gbmp.SetSmoothingMode(SmoothingModeAntiAlias);
    DrawThumbs(gbmp);
    gbmp.Free;

    g := TGPGraphics.Create(Canvas.Handle);
    g.DrawImage(bmp, 0, 0);
    g.Free;

    bmp.Free;
  end;
end;

procedure TAdvSmoothSlideShow.PlaceHolderFillChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothSlideShow.Previous;
begin
  itemindex := itemindex - 1;
end;

procedure TAdvSmoothSlideShow.Resize;
begin
  inherited;
  FCacheBG := false;
  Changed;
end;

procedure TAdvSmoothSlideShow.Rotate(Sender: TObject);
var
  i: integer;
begin
  if csDesigning in ComponentState then
    Exit;

  i := itemindex;
  Inc(i);
  if i = Items.Count then
    i := 0;

  itemindex := i;
end;

procedure TAdvSmoothSlideShow.ScrollToItem(Index: integer);
var
  it: TAdvSmoothSlideShowItem;
  rt: TGPRectF;
begin
  it := nil;
  if (Index >= 0) and (Index <= Items.Count - 1) then
    it := Items[Index];

  if Assigned(it) then
  begin
    rt := GetThumbNailRect;
    if (ThumbNailPosition = tpRight) or (ThumbNailPosition = tpLeft) then
    begin
      if it.FThumbItemRect.Y + it.FThumbItemRect.Height > rt.Y + rt.Height then
        ScrollPos := it.FThumbItemRect.Y + ScrollPos +
          it.FThumbItemRect.Height + ThumbNailSpacing - rt.Y - rt.Height
      else if it.FThumbItemRect.Y < rt.Y then
        ScrollPos := it.FThumbItemRect.Y + ScrollPos - ThumbNailSpacing - rt.Y;
    end
    else if (ThumbNailPosition = tpTop) or (ThumbNailPosition = tpBottom) then
    begin
      if it.FThumbItemRect.X + it.FThumbItemRect.Width > rt.X + rt.Width then
        ScrollPos := it.FThumbItemRect.X + ScrollPos +
          it.FThumbItemRect.Width + ThumbNailSpacing - rt.X - rt.Width
      else if it.FThumbItemRect.X < rt.X then
        ScrollPos := it.FThumbItemRect.X + ScrollPos - ThumbNailSpacing - rt.X;
    end;
  end;
end;

procedure TAdvSmoothSlideShow.SetAnimation(const Value: boolean);
begin
  if FAnimation <> Value then
  begin
    FAnimation := Value;
    Changed;
  end;
end;

procedure TAdvSmoothSlideShow.SetAnimations(const Value: TSlideShowAnimations);
begin
  if FAnimations <> Value then
  begin
    FAnimations := Value;
    Changed;
  end;
end;

procedure TAdvSmoothSlideShow.SetAspectRatio(const Value: boolean);
begin
  if FAspectRatio <> Value then
  begin
    FAspectRatio := Value;
    Changed;
  end;
end;

procedure TAdvSmoothSlideShow.SetColorTones(ATones: TColorTones);
begin
  Fill.Color := ATones.Background.BrushColor;
  Fill.ColorTo := ATones.Background.BrushColor;
  Fill.BorderColor := ATones.Background.BorderColor;

  ThumbNailHolderFill.Color := ATones.Background.BrushColor;
  ThumbNailHolderFill.ColorTo := ATones.Background.BrushColor;
  ThumbNailHolderFill.BorderColor := ATones.Background.BorderColor;

  ThumbNailSelectedFill.Color := ATones.Selected.BrushColor;
  ThumbNailSelectedFill.ColorTo := ATones.Selected.BrushColor;
  ThumbNailSelectedFill.ColorMirror := ATones.Selected.BrushColor;
  ThumbNailSelectedFill.ColorMirrorTo := ATones.Selected.BrushColor;
  ThumbNailSelectedFill.BorderColor := ATones.Selected.BorderColor;

  PlaceHolderFill.Color := ATones.Background.BrushColor;
  PlaceHolderFill.ColorTo := ATones.Background.BrushColor;
  PlaceHolderFill.BorderColor := ATones.Background.BorderColor;
end;

procedure TAdvSmoothSlideShow.SetComponentStyle(AStyle: TTMSStyle);
begin
  FTMSStyle := AStyle;
  ThumbNailSelectedFill.Glow := gmGradient;
  ThumbNailSelectedFill.Rounding := 2;
  ThumbNailHolderFill.Glow := gmGradient;
  case AStyle of
    tsOffice2003Blue:
      begin
        Fill.Color := $00FFD2AF;
        Fill.ColorTo := $00FFD2AF;

        ThumbNailHolderFill.Color := $D68759;
        ThumbNailHolderFill.ColorTo := $933803;
        ThumbNailHolderFill.BorderColor := $962D00;

        ThumbNailSelectedFill.Color := $94E6FB;
        ThumbNailSelectedFill.ColorTo := $1595EE;
        ThumbNailSelectedFill.ColorMirror := clNone;
        ThumbNailSelectedFill.ColorMirrorTo := clNone;
        ThumbNailSelectedFill.BorderColor := $962D00;
        ThumbNailSelectedFill.GradientMirrorType := gtVertical;
      end;
    tsOffice2003Silver:
      begin
        Fill.Color := $00E6D8D8;
        Fill.ColorTo := $00E6D8D8;

        ThumbNailHolderFill.Color := $BDA4A5;
        ThumbNailHolderFill.ColorTo := $957475;
        ThumbNailHolderFill.BorderColor := $947C7C;

        ThumbNailSelectedFill.Color := $94E6FB;
        ThumbNailSelectedFill.ColorTo := $1595EE;
        ThumbNailSelectedFill.ColorMirror := clNone;
        ThumbNailSelectedFill.ColorMirrorTo := clNone;
        ThumbNailSelectedFill.BorderColor := $947C7C;
        ThumbNailSelectedFill.GradientMirrorType := gtVertical;
      end;
    tsOffice2003Olive:
      begin
        Fill.Color := $CFF0EA;
        Fill.ColorTo := $CFF0EA;

        ThumbNailHolderFill.Color := $82C0AF;
        ThumbNailHolderFill.ColorTo := $447A63;
        ThumbNailHolderFill.BorderColor := $588060;

        ThumbNailSelectedFill.Color := $94E6FB;
        ThumbNailSelectedFill.ColorTo := $1595EE;
        ThumbNailSelectedFill.ColorMirror := clNone;
        ThumbNailSelectedFill.ColorMirrorTo := clNone;
        ThumbNailSelectedFill.BorderColor := $588060;
        ThumbNailSelectedFill.GradientMirrorType := gtVertical;
      end;
    tsOffice2003Classic:
      begin
        Fill.Color := $00F2F2F2;
        Fill.ColorTo := $00F2F2F2;

        ThumbNailHolderFill.Color := $808080;
        ThumbNailHolderFill.ColorTo := $808080;
        ThumbNailHolderFill.BorderColor := $808080;

        ThumbNailSelectedFill.Color := $B59285;
        ThumbNailSelectedFill.ColorTo := $B59285;
        ThumbNailSelectedFill.ColorMirror := clNone;
        ThumbNailSelectedFill.ColorMirrorTo := clNone;
        ThumbNailSelectedFill.BorderColor := $962D00;
        ThumbNailSelectedFill.GradientMirrorType := gtVertical;

      end;
    tsOffice2007Luna:
      begin
        Fill.Color := $DCB698;
        Fill.ColorTo := $DCB698;

        ThumbNailHolderFill.Color := $FFEFE3;
        ThumbNailHolderFill.ColorTo := $FFD2AF;
        ThumbNailHolderFill.BorderColor := $00FFD2AF;

        ThumbNailSelectedFill.Color := $AAD9FF;
        ThumbNailSelectedFill.ColorTo := $6EBBFF;
        ThumbNailSelectedFill.ColorMirror := $42AEFE;
        ThumbNailSelectedFill.ColorMirrorTo := $7AE1FE;
        ThumbNailSelectedFill.BorderColor := $FFD1AD; // $42AEFE;
        ThumbNailSelectedFill.GradientMirrorType := gtVertical;

      end;
    tsOffice2007Obsidian:
      begin
        Fill.Color := $5C534C;
        Fill.ColorTo := $5C534C;

        ThumbNailHolderFill.Color := $F2F1F0;
        ThumbNailHolderFill.ColorTo := $C9C2BD;
        ThumbNailHolderFill.BorderColor := $5C534C;

        ThumbNailSelectedFill.Color := $AAD9FF;
        ThumbNailSelectedFill.ColorTo := $6EBBFF;
        ThumbNailSelectedFill.ColorMirror := $42AEFE;
        ThumbNailSelectedFill.ColorMirrorTo := $7AE1FE;
        ThumbNailSelectedFill.BorderColor := clBlack; // $42AEFE;
        ThumbNailSelectedFill.GradientMirrorType := gtVertical;

      end;
    tsWindowsXP:
      begin
        Fill.Color := $00B6B6B6;
        Fill.ColorTo := $00B6B6B6;

        ThumbNailHolderFill.Color := clBtnFace;
        ThumbNailHolderFill.ColorTo := clBtnFace;
        ThumbNailHolderFill.BorderColor := clBlack;

        ThumbNailSelectedFill.Color := clInactiveCaption;
        ThumbNailSelectedFill.ColorTo := clInactiveCaption;
        ThumbNailSelectedFill.ColorMirror := clNone;
        ThumbNailSelectedFill.ColorMirrorTo := clNone;
        ThumbNailSelectedFill.BorderColor := clBlack;
        ThumbNailSelectedFill.GradientMirrorType := gtVertical;
      end;
    tsWhidbey:
      begin
        Fill.Color := $F5F9FA;
        Fill.ColorTo := $F5F9FA;

        ThumbNailHolderFill.Color := $EBEEEF;
        ThumbNailHolderFill.ColorTo := $7E9898;
        ThumbNailHolderFill.BorderColor := $962D00;

        ThumbNailSelectedFill.Color := $94E6FB;
        ThumbNailSelectedFill.ColorTo := $1595EE;
        ThumbNailSelectedFill.ColorMirror := clNone;
        ThumbNailSelectedFill.ColorMirrorTo := clNone;
        ThumbNailSelectedFill.BorderColor := $962D00;
        ThumbNailSelectedFill.GradientMirrorType := gtVertical;

      end;
    tsCustom:
      ;
    tsOffice2007Silver:
      begin
        Fill.Color := $00CAC1BA;
        Fill.ColorTo := $00CAC1BA;

        ThumbNailHolderFill.Color := $F8F7F6;
        ThumbNailHolderFill.ColorTo := $E8E0DB;
        ThumbNailHolderFill.BorderColor := $74706F;

        ThumbNailSelectedFill.Color := $AAD9FF;
        ThumbNailSelectedFill.ColorTo := $6EBBFF;
        ThumbNailSelectedFill.ColorMirror := $42AEFE;
        ThumbNailSelectedFill.ColorMirrorTo := $7AE1FE;
        ThumbNailSelectedFill.BorderColor := clBlack; // $42AEFE;
        ThumbNailSelectedFill.GradientMirrorType := gtVertical;
      end;
    tsWindowsVista:
      begin
        Fill.Color := $F7EED9;
        Fill.ColorTo := clNone;

        ThumbNailHolderFill.Color := $FCF9F2;
        ThumbNailHolderFill.ColorTo := $F7EED9;
        ThumbNailHolderFill.BorderColor := $F9D996;

        ThumbNailSelectedFill.Color := $FEF9F0;
        ThumbNailSelectedFill.ColorTo := $FDF0D7;
        ThumbNailSelectedFill.ColorMirror := clNone;
        ThumbNailSelectedFill.ColorMirrorTo := clNone;
        ThumbNailSelectedFill.BorderColor := $FEDF9A;
        ThumbNailSelectedFill.GradientMirrorType := gtVertical;

      end;
    tsWindows7:
      begin
        Fill.Color := $F7EED9;
        Fill.ColorTo := clNone;

        ThumbNailHolderFill.Color := $FCF9F2;
        ThumbNailHolderFill.ColorTo := $F7EED9;
        ThumbNailHolderFill.BorderColor := $F9D996;

        ThumbNailSelectedFill.Color := $FCEBDC;
        ThumbNailSelectedFill.ColorTo := $FCDBC1;
        ThumbNailSelectedFill.ColorMirror := clNone;
        ThumbNailSelectedFill.ColorMirrorTo := clNone;
        ThumbNailSelectedFill.BorderColor := $CEA27D;
        ThumbNailSelectedFill.GradientMirrorType := gtVertical;

      end;
    tsTerminal:
      begin
        Fill.Color := clWhite;
        Fill.ColorTo := clWhite;

        ThumbNailHolderFill.Color := clBtnFace;
        ThumbNailHolderFill.ColorTo := clBtnFace;
        ThumbNailHolderFill.BorderColor := clGray;

        ThumbNailSelectedFill.Color := clHighLight;
        ThumbNailSelectedFill.ColorTo := clHighLight;
        ThumbNailSelectedFill.ColorMirror := clNone;
        ThumbNailSelectedFill.ColorMirrorTo := clNone;
        ThumbNailSelectedFill.BorderColor := clGray;

      end;

    tsOffice2010Blue:
    begin
      Fill.Color := $EAD3BF;
      Fill.ColorTo := clNone;

      ThumbNailHolderFill.Color := $FDF6EF;
      ThumbNailHolderFill.ColorTo := $F0DAC7;
      ThumbNailHolderFill.BorderColor := $C7B29F;

      ThumbNailSelectedFill.Color := $6CD0FF;
      ThumbNailSelectedFill.ColorTo := clNone;
      ThumbNailSelectedFill.ColorMirror := clNone;
      ThumbNailSelectedFill.ColorMirrorTo := clNone;
      ThumbNailSelectedFill.BorderColor := $308AC2;
      ThumbNailSelectedFill.GradientMirrorType := gtVertical;
      ThumbNailSelectedFill.Rounding:= 2;
      ThumbNailSelectedFill.Glow:= gmGradient;
      ThumbNailSelectedFill.GlowGradientColor:= $7BEEFF;

    end;
    tsOffice2010Silver:
    begin
      Fill.Color := $D4CFCB;
      Fill.ColorTo := clNone;

      ThumbNailHolderFill.Color := $FFFFFF;
      ThumbNailHolderFill.ColorTo := $EDE5E0;
      ThumbNailHolderFill.BorderColor := $D2CDC8;

      ThumbNailSelectedFill.Color := $6CD0FF;
      ThumbNailSelectedFill.ColorTo := clNone;
      ThumbNailSelectedFill.ColorMirror := clNone;
      ThumbNailSelectedFill.ColorMirrorTo := clNone;
      ThumbNailSelectedFill.BorderColor := $308AC2;
      ThumbNailSelectedFill.GradientMirrorType := gtVertical;
      ThumbNailSelectedFill.Rounding:= 2;
      ThumbNailSelectedFill.Glow:= gmGradient;
      ThumbNailSelectedFill.GlowGradientColor:= $7BEEFF;

    end;
    tsOffice2010Black:
    begin
      Fill.Color := $656565;
      Fill.ColorTo := clNone;

      ThumbNailHolderFill.Color := $BFBFBF;
      ThumbNailHolderFill.ColorTo := $919191;
      ThumbNailHolderFill.BorderColor := $6D6D6D;

      ThumbNailSelectedFill.Color := $6CD0FF;
      ThumbNailSelectedFill.ColorTo := clNone;
      ThumbNailSelectedFill.ColorMirror := clNone;
      ThumbNailSelectedFill.ColorMirrorTo := clNone;
      ThumbNailSelectedFill.BorderColor := $308AC2;
      ThumbNailSelectedFill.GradientMirrorType := gtVertical;
      ThumbNailSelectedFill.Rounding:= 2;
      ThumbNailSelectedFill.Glow:= gmGradient;
      ThumbNailSelectedFill.GlowGradientColor:= $7BEEFF;

    end;

  tsWindows8, tsWindows10:
    begin
      Fill.Color := $F7F6F5;
      Fill.ColorTo := clNone;

      ThumbNailHolderFill.Color := clWhite;
      ThumbNailHolderFill.ColorTo := clNone;
      ThumbNailHolderFill.BorderColor := $DCDBDA;

      ThumbNailSelectedFill.Color := $F7E0C9;
      ThumbNailSelectedFill.ColorTo := clNone;
      ThumbNailSelectedFill.ColorMirror := clNone;
      ThumbNailSelectedFill.ColorMirrorTo := clNone;
      ThumbNailSelectedFill.BorderColor := $E4A262;
      ThumbNailSelectedFill.GradientMirrorType := gtVertical;
      ThumbNailSelectedFill.Rounding:= 0;
      ThumbNailSelectedFill.Glow:= gmNone;


    end;
  tsOffice2013White:
    begin
      Fill.Color := $EEEEEE;
      Fill.ColorTo := clNone;

      ThumbNailHolderFill.Color := clWhite;
      ThumbNailHolderFill.ColorTo := clNone;
      ThumbNailHolderFill.BorderColor := $D4D4D4;

      ThumbNailSelectedFill.Color := $FCE2C8;
      ThumbNailSelectedFill.ColorTo := clNone;
      ThumbNailSelectedFill.ColorMirror := clNone;
      ThumbNailSelectedFill.ColorMirrorTo := clNone;
      ThumbNailSelectedFill.BorderColor := $E59D56;
      ThumbNailSelectedFill.GradientMirrorType := gtVertical;
      ThumbNailSelectedFill.Rounding:= 0;
      ThumbNailSelectedFill.Glow:= gmNone;


    end;
  tsOffice2013LightGray:
    begin
      Fill.Color := $FAFAFA;
      Fill.ColorTo := clNone;

      ThumbNailHolderFill.Color := clWhite;
      ThumbNailHolderFill.ColorTo := clNone;
      ThumbNailHolderFill.BorderColor := $C6C6C6;

      ThumbNailSelectedFill.Color := $FCE2C8;
      ThumbNailSelectedFill.ColorTo := clNone;
      ThumbNailSelectedFill.ColorMirror := clNone;
      ThumbNailSelectedFill.ColorMirrorTo := clNone;
      ThumbNailSelectedFill.BorderColor := $E59D56;
      ThumbNailSelectedFill.GradientMirrorType := gtVertical;
      ThumbNailSelectedFill.Rounding:= 0;
      ThumbNailSelectedFill.Glow:= gmNone;

    end;
  tsOffice2013Gray:
    begin
      Fill.Color := $F3F3F3;
      Fill.ColorTo := clNone;

      ThumbNailHolderFill.Color := clWhite;
      ThumbNailHolderFill.ColorTo := clNone;
      ThumbNailHolderFill.BorderColor := $ABABAB;

      ThumbNailSelectedFill.Color := $FCE2C8;
      ThumbNailSelectedFill.ColorTo := clNone;
      ThumbNailSelectedFill.ColorMirror := clNone;
      ThumbNailSelectedFill.ColorMirrorTo := clNone;
      ThumbNailSelectedFill.BorderColor := $E59D56;
      ThumbNailSelectedFill.GradientMirrorType := gtVertical;
      ThumbNailSelectedFill.Rounding:= 0;
      ThumbNailSelectedFill.Glow:= gmNone;

    end;
 tsOffice2016White:
    begin
      Fill.Color := $EEEEEE;
      Fill.ColorTo := clNone;

      ThumbNailHolderFill.Color := clWhite;
      ThumbNailHolderFill.ColorTo := clNone;
      ThumbNailHolderFill.BorderColor := $D4D4D4;
      ThumbNailHolderFill.Glow := gmNone;

      ThumbNailSelectedFill.Color := $E3BDA3;
      ThumbNailSelectedFill.ColorTo := clNone;
      ThumbNailSelectedFill.ColorMirror := clNone;
      ThumbNailSelectedFill.ColorMirrorTo := clNone;
      ThumbNailSelectedFill.BorderColor := $E3BDA3;
      ThumbNailSelectedFill.GradientMirrorType := gtVertical;
      ThumbNailSelectedFill.Rounding:= 0;
      ThumbNailSelectedFill.Glow:= gmNone;


    end;
  tsOffice2016Gray:
    begin
      Fill.Color := $444444;
      Fill.ColorTo := clNone;

      ThumbNailHolderFill.Color := $B2B2B2;
      ThumbNailHolderFill.ColorTo := clNone;
      ThumbNailHolderFill.BorderColor := $444444;
      ThumbNailHolderFill.Glow := gmNone;

      ThumbNailSelectedFill.Color := $E3BDA3;
      ThumbNailSelectedFill.ColorTo := clNone;
      ThumbNailSelectedFill.ColorMirror := clNone;
      ThumbNailSelectedFill.ColorMirrorTo := clNone;
      ThumbNailSelectedFill.BorderColor := $E3BDA3;
      ThumbNailSelectedFill.GradientMirrorType := gtVertical;
      ThumbNailSelectedFill.Rounding:= 0;
      ThumbNailSelectedFill.Glow:= gmNone;

    end;
  tsOffice2016Black:
    begin
      Fill.Color := $363636;
      Fill.ColorTo := clNone;

      ThumbNailHolderFill.Color := $252525; //$363636;
      ThumbNailHolderFill.ColorTo := clNone;
      ThumbNailHolderFill.BorderColor := $444444;
      ThumbNailHolderFill.Glow := gmNone;

      ThumbNailSelectedFill.Color := $444444;
      ThumbNailSelectedFill.ColorTo := clNone;
      ThumbNailSelectedFill.ColorMirror := clNone;
      ThumbNailSelectedFill.ColorMirrorTo := clNone;
      ThumbNailSelectedFill.BorderColor := $444444;
      ThumbNailSelectedFill.GradientMirrorType := gtVertical;
      ThumbNailSelectedFill.Rounding:= 0;
      ThumbNailSelectedFill.Glow:= gmNone;

    end;

  end;

  PlaceHolderFill.Assign(ThumbNailHolderFill);
end;

procedure TAdvSmoothSlideShow.SetDescriptionFill(const Value: TGDIPFill);
begin
  if FDescriptionFill <> Value then
  begin
    FDescriptionFill.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothSlideShow.SetFadeAnimationFactor(const Value: integer);
begin
  if FFadeAnimationFactor <> Value then
  begin
    FFadeAnimationFactor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothSlideShow.SetFill(const Value: TGDIPFill);
begin
  if FFill <> Value then
  begin
    FFill.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothSlideShow.SetHolderAnimationFactor(const Value: integer);
begin
  if FHolderAnimationFactor <> Value then
  begin
    FHolderAnimationFactor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothSlideShow.SetItemIndex(const Value: integer);
var
  val: integer;
  up: boolean;
begin
  FPreviousItemIndex := FItemIndex;
  val := Value;
  up := val > FPreviousItemIndex;
  if val < 0 then
    val := Items.Count - 1
  else if val > Items.Count - 1 then
    val := 0;

  if GetCountSelectable > 0 then
  begin
    while not(Items[val].Visible and Items[val].Enabled) do
    begin
      if up then
      begin
        Inc(val);
        if val > GetMaxSelectable then
          val := 0;
      end
      else
      begin
        Dec(val);
        if val < GetMinSelectable then
          val := Items.Count - 1;
      end;
    end;
  end
  else
    Exit;

  if (FItemIndex <> Value) then
  begin
    if FDoAnimation then
    begin
      FDoAnimation := false;
      FCurrentItemIndex := FItemIndex;
    end;

    FItemIndex := val;
    ScrollToItem(FItemIndex);
    if FDesignTime then
    begin
      FCurrentItemIndex := val;
      Changed;
    end
    else
    begin
      FDoAnimation := true;
      FCurrentOpacity := 255;
      FCurrentOpacityTo := 0;
      FNextOpacity := 0;
      FNextOpacityTo := 255;

      effect := -1;
      if GetAnimationCount > 0 then
        effect := integer(GetRandomAnimation);

      if (Transitions = []) or not Animation then
        FCurrentItemIndex := FItemIndex;

      if (FPreviousItemIndex >= 0) and (FPreviousItemIndex <= Items.Count - 1)
        then
      begin
        if (Items[FPreviousItemIndex].Kind = ikControl) and Assigned
          (Items[FPreviousItemIndex].Control) then
        begin
          Items[FPreviousItemIndex].Control.Visible := false;
          Items[FPreviousItemIndex].Update;
        end;
      end;

      if Animation then
        FRotateTimer.Enabled := false
      else
        Invalidate;
    end;
  end;
end;

procedure TAdvSmoothSlideShow.SetItemMode(const Value: TSlideShowItemMode);
begin
  if FItemMode <> Value then
  begin
    FItemMode := Value;
    Changed;
  end;
end;

procedure TAdvSmoothSlideShow.SetItems(const Value: TAdvSmoothSlideShowItems);
begin
  if FItems <> Value then
  begin
    FItems.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothSlideShow.SetMargin(const Value: TMargins);
begin
  if FMargin <> Value then
  begin
    FMargin.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothSlideShow.SetPlaceHolderFill(const Value: TGDIPFill);
begin
  if FPlaceHolderFill <> Value then
  begin
    FPlaceHolderFill.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothSlideShow.SetPlaceHolderSpacing(const Value: integer);
begin
  if FPlaceHolderSpacing <> Value then
  begin
    FPlaceHolderSpacing := Value;
    Changed;
  end;
end;

procedure TAdvSmoothSlideShow.SetPlaceHolderVisible(const Value: boolean);
begin
  if FPlaceHolderVisible <> Value then
  begin
    FPlaceHolderVisible := Value;
    Changed;
  end;
end;

procedure TAdvSmoothSlideShow.SetRotation(const Value: boolean);
begin
  if FRotation <> Value then
  begin
    FRotation := Value;
    if Assigned(FRotateTimer) then
      FRotateTimer.Enabled := Value;
    Changed;
  end;
end;

procedure TAdvSmoothSlideShow.SetRotationInterval(const Value: integer);
begin
  if FRotationInterval <> Value then
  begin
    FRotationInterval := Value;
    if Assigned(FRotateTimer) then
      FRotateTimer.Interval := Value;
    Changed;
  end;
end;

procedure TAdvSmoothSlideShow.SetScrollPos(const Value: Double);
begin
  FScrollPos := max(0, min(Value, MaxScrollPos));
  BuildItems;
  PaintThumbnails;
end;

procedure TAdvSmoothSlideShow.SetShowDescription(const Value: Boolean);
begin
  if FShowDescription <> Value then
  begin
    FShowDescription := Value;
    Changed;
  end;
end;

procedure TAdvSmoothSlideShow.SetStretchPercentage(const Value: Integer);
begin
  if FStretchPercentage <> Value then
  begin
    FStretchPercentage := Max(50, Min(100, Value));
    Changed;
  end;
end;

procedure TAdvSmoothSlideShow.SetThumbNailHandles
  (const Value: THandleAppearance);
begin
  if FThumbNailHandles <> Value then
  begin
    FThumbNailHandles.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothSlideShow.SetThumbNailHeight(const Value: integer);
begin
  if FThumbNailHeight <> Value then
  begin
    FThumbNailHeight := Value;
    Changed;
  end;
end;

procedure TAdvSmoothSlideShow.SetThumbNailHolderFill(const Value: TGDIPFill);
begin
  if FThumbNailHolderFill <> Value then
  begin
    FThumbNailHolderFill.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothSlideShow.SetThumbNailHolderMargin(const Value: TMargins);
begin
  if FThumbNailHolderMargin <> Value then
  begin
    FThumbNailHolderMargin.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothSlideShow.SetThumbNailPosition
  (const Value: TThumbNailPosition);
begin
  if FThumbnailPosition <> Value then
  begin
    FThumbnailPosition := Value;
    Changed;
  end;
end;

procedure TAdvSmoothSlideShow.SetThumbNails(const Value: boolean);
begin
  if FThumbNails <> Value then
  begin
    FThumbNails := Value;
    RestartLoop := true;
    Changed;
  end;
end;

procedure TAdvSmoothSlideShow.SetThumbNailSelectedFill(const Value: TGDIPFill);
begin
  if FThumbNailSelectedFill <> Value then
  begin
    FThumbNailSelectedFill.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothSlideShow.SetThumbNailsFloating(const Value: boolean);
begin
  if FThumbNailsFloating <> Value then
  begin
    FThumbNailsFloating := Value;
    Changed;
  end;
end;

procedure TAdvSmoothSlideShow.SetThumbNailSpacing(const Value: integer);
begin
  if FThumbNailSpacing <> Value then
  begin
    FThumbNailSpacing := Value;
    Changed;
  end;
end;

procedure TAdvSmoothSlideShow.SetThumbNailWidth(const Value: integer);
begin
  if FThumbNailWidth <> Value then
  begin
    FThumbNailWidth := Value;
    Changed;
  end;
end;

procedure TAdvSmoothSlideShow.SetTransitions
  (const Value: TSlideShowTransitions);
begin
  if FTransitions <> Value then
  begin
    FTransitions := Value;
    Changed;
  end;
end;

procedure TAdvSmoothSlideShow.SetLoadEffect(const Value: boolean);
begin
  if FLoadEffect <> Value then
  begin
    FLoadEffect := Value;
    Changed;
  end;
end;

procedure TAdvSmoothSlideShow.SetVersion(const Value: string);
begin

end;

function TAdvSmoothSlideShow.IsHandleHorzLeft: boolean;
begin
  result := ((ThumbNailPosition = tpTop) or (ThumbNailPosition = tpBottom)) and
    (ScrollPos > 0);
end;

function TAdvSmoothSlideShow.IsHandleHorzRight: boolean;
begin
  result := ((ThumbNailPosition = tpTop) or (ThumbNailPosition = tpBottom)) and
    (ScrollPos < MaxScrollPos);
end;

function TAdvSmoothSlideShow.IsHandleVertBottom: boolean;
begin
  result := ((ThumbNailPosition = tpLeft) or (ThumbNailPosition = tpRight)) and
    (ScrollPos < MaxScrollPos);
end;

function TAdvSmoothSlideShow.IsHandleVertTop: boolean;
begin
  result := ((ThumbNailPosition = tpLeft) or (ThumbNailPosition = tpRight)) and
    (ScrollPos > 0);
end;

procedure TAdvSmoothSlideShow.ItemsChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothSlideShow.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  case Key of
    VK_UP, VK_LEFT:
      Previous;
    VK_DOWN, VK_RIGHT:
      Next;
    VK_HOME:
      itemindex := 0;
    VK_END:
      itemindex := Items.Count - 1
  end;
end;

procedure TAdvSmoothSlideShow.StartThread;
begin
  if HandleAllocated and not Assigned(FImageLoaderThread) then
  begin
    FImageLoaderThread := TImageLoaderThread.Create(self);
    FImageLoaderThread.Priority := tpLowest;
    FImageLoaderThread.OnTerminate := ThreadDone;
  end;
end;

procedure TAdvSmoothSlideShow.StopThread;
begin
  if Assigned(FImageLoaderThread) then
  begin
    FImageLoaderThread.Terminate;

    while not FThreadDone do
    begin
      Application.ProcessMessages;
    end;

    FreeAndNil(FImageLoaderThread);
  end;
end;

procedure TAdvSmoothSlideShow.ThreadDone(Sender: TObject);
begin
  FThreadDone := true;
end;

procedure TAdvSmoothSlideShow.ThumbNailFillChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothSlideShow.UpdateControlImage(AControl: TWinControl);
var
  i: integer;
begin
  for i := 0 to Items.Count - 1 do
  begin
    if Items[i].Control = AControl then
    begin
      Items[i].Update;
      break;
    end;
  end;
end;

procedure TAdvSmoothSlideShow.UpdateImages;
var
  i: integer;
begin
  for i := 0 to Items.Count - 1 do
    Items[i].Update;
  RestartLoop := true;
end;

procedure TAdvSmoothSlideShow.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  if TabStop then
    Message.result := DLGC_WANTALLKEYS or DLGC_WANTARROWS
  else
    Message.result := 0;
end;

procedure TAdvSmoothSlideShow.WMMouseWheel(var Message: TWMMouseWheel);
begin
  inherited;
  case Message.Keys of
    0:
      begin
        ScrollPos := ScrollPos - Message.WheelDelta;
        BuildItems;
        PaintThumbnails;
      end;
  end;
end;

function TAdvSmoothSlideShow.XYToCurrentItem(pX, pY: integer): integer;
begin
  result := -1;
  if (FCurrentItemIndex >= 0) and (FCurrentItemIndex <= Items.Count - 1) then
    with Items[FCurrentItemIndex] do
      if Enabled and Visible and PtInGPRect(FItemRect, Point(pX, pY)) then
        result := FCurrentItemIndex;
end;

function TAdvSmoothSlideShow.XYToThumb(pX, pY: integer): integer;
var
  i: integer;
  rthumb: TGPRectF;
begin
  rthumb := GetThumbNailRect;
  result := -1;
  for i := 0 to Items.Count - 1 do
  begin
    with Items[i] do
    begin
      if Enabled and Visible and PtInGPRect(FThumbItemRect, Point(pX, pY))
        and RectanglesInterSect(FThumbRect, rthumb) then
      begin
        result := i;
        break;
      end;
    end;
  end;
end;

{ TImageLoaderThread }

constructor TImageLoaderThread.Create(ASlideShow: TAdvSmoothSlideShow);
begin
  SlideShow := ASlideShow;
  inherited Create(false);
end;

procedure TImageLoaderThread.Execute;
var
  i: integer;
  g: TGPGraphics;
  gotimage: boolean;
  ow, oh, nw, nh, w, h: Double;
  bmp: TBitmap;
  gpbmp: TGPBitmap;
  ms, s: TMemoryStream;
  sta: TFixedStreamAdapter;
  rf: TGPRectF;
  ft: TGPFont;
  b: TGPSolidBrush;
  sf: TGPStringFormat;
  textw, texth: integer;
  textoutr: TGPRectF;
  img: TAdvGDIPPicture;
{$IFNDEF BCB}
{$IFDEF DELPHI_TOUCH}
  errorout: string;
{$ENDIF}
{$ENDIF}
begin
  if (SlideShow.FDesignTime) then
    Exit;

  try
{$IFNDEF BCB}
{$IFDEF DELPHI_TOUCH}
    OleCheck(CoInitializeEx(nil, COINIT_APARTMENTTHREADED));
{$ENDIF}
{$ENDIF}
    try
{$IFNDEF BCB}
{$IFDEF DELPHI_TOUCH}
      OleCheck(SHGetMalloc(fMalloc));
{$ENDIF}
{$ENDIF}
      while not Terminated do
      begin
        gotimage := false;
        if (SlideShow.FUpdateCount = 0) and not SlideShow.FDoAnimation then
        begin
          for i := 0 to SlideShow.Items.Count - 1 do
          begin
            if SlideShow.RestartLoop then
            begin
              SlideShow.RestartLoop := false;
              break;
            end;

            if Terminated then
              break;

            with SlideShow.Items[i] do
            begin
              if (FThreadStatus = stInitialized) then
              begin
                FThreadStatus := stLoading;
                gotimage := true;

                if Assigned(FOriginalImage) then
                  FreeAndNil(FOriginalImage);

                case Kind of
                  ikImage:
                    begin
                      if Image.Empty and FileExists(Location) then
                        Image.LoadFromFile(Location);
                    end;
                  ikControl:
                    begin
                      if Assigned(Control) then
                      begin
                        bmp := TBitmap.Create;
                        bmp.Width := Control.Width;
                        bmp.Height := Control.Height;
                        FCtrl := Control;
                        ItemIndex := i;
                        FCtrlBitmap := bmp;
                        Synchronize(PaintControl);
                        ms := TMemoryStream.Create;
                        bmp.SaveToStream(ms);
                        Image.LoadFromStream(ms);
                        ms.Free;
                        bmp.Free;
                        FCtrl := nil;
                        FCtrlBitmap := nil;
                      end;
                    end;
                  ikCustom:
                    begin
                      gpbmp := TGPBitmap.Create(CustomImageWidth,
                        CustomImageHeight);
                      s := TMemoryStream.Create;
                      sta := TFixedStreamAdapter.Create(s);
                      g := TGPGraphics.Create(gpbmp);

                      rf := MakeRect(0, 0, CustomImageWidth, CustomImageHeight);
                      if Assigned(SlideShow.OnCustomDraw) then
                        SlideShow.OnCustomDraw(SlideShow, i, g, rf, ikCustom);

                      gpbmp.Save(sta, GetCLSID(itPNG));
                      s.Position := 0;
                      Image.LoadFromStream(s);

                      gpbmp.Free;
                      g.Free;
                      s.Free;
                    end;
                  ikImageText:
                    begin
                      if Text <> '' then
                      begin
                        img := TAdvGDIPPicture.Create;
                        if FileExists(Location) then
                          img.LoadFromFile(Location);

                        if TextAutoSize then
                        begin
                          gpbmp := TGPBitmap.Create(1, 1);
                          g := TGPGraphics.Create(gpbmp);
                          ft := g.MakeFont(TextFont);
                          sf := TGPStringFormat.Create;
                          if not WordWrap and Ellipsis then
                          begin
                            sf.SetFormatFlags(StringFormatFlagsNoWrap);
                            sf.SetTrimming(StringTrimmingEllipsisWord);
                          end
                          else if not WordWrap then
                            sf.SetFormatFlags(StringFormatFlagsNoWrap);

                          g.MeasureString(Text, Length(Text), ft, MakeRect
                              (0, 0, TextWidth, 10000), sf, textoutr);

                          sf.Free;
                          ft.Free;

                          g.Free;
                          gpbmp.Free;

                          textw := Round(textoutr.Width + 1);
                          texth := Round(textoutr.Height + 1);
                        end
                        else
                        begin
                          texth := TextHeight;
                          textw := TextWidth;
                        end;

                        s := TMemoryStream.Create;
                        sta := TFixedStreamAdapter.Create(s);
                        gpbmp := nil;
                        if not img.Empty then
                        begin
                          case TextLocation of
                            tlLeft, tlRight:
                              begin
                                if img.Height > texth + 4 then
                                  gpbmp := TGPBitmap.Create
                                    (textw + 4 + img.Width, img.Height)
                                else
                                  gpbmp := TGPBitmap.Create
                                    (textw + 4 + img.Width, texth + 4)
                              end;
                            tlTop, tlBottom:
                              begin
                                if img.Width > textw + 4 then
                                  gpbmp := TGPBitmap.Create
                                    (img.Width, texth + 4 + img.Height)
                                else
                                  gpbmp := TGPBitmap.Create
                                    (textw + 4, texth + 4 + img.Height)
                              end;
                          end;
                        end
                        else
                          gpbmp := TGPBitmap.Create(textw + 4, texth + 4);

                        if Assigned(gpbmp) and not img.Empty then
                        begin
                          g := TGPGraphics.Create(gpbmp);
                          g.SetSmoothingMode(SmoothingModeHighQuality);
                          g.SetTextRenderingHint(TextRenderingHintAntiAlias);
                          ft := g.MakeFont(TextFont);
                          b := TGPSolidBrush.Create
                            (MakeColor(255, TextFont.Color));
                          sf := TGPStringFormat.Create;
                          if not WordWrap and Ellipsis then
                          begin
                            sf.SetFormatFlags(StringFormatFlagsNoWrap);
                            sf.SetTrimming(StringTrimmingEllipsisWord);
                          end
                          else if not WordWrap then
                            sf.SetFormatFlags(StringFormatFlagsNoWrap);

                          case TextVerticalAlignment of
                            taAlignTop:
                              sf.SetLineAlignment(StringAlignmentNear);
                            taAlignBottom:
                              sf.SetLineAlignment(StringAlignmentFar);
                            taVerticalCenter:
                              sf.SetLineAlignment(StringAlignmentCenter);
                          end;

                          case TextHorizontalAlignment of
                            taLeftJustify:
                              sf.SetAlignment(StringAlignmentNear);
                            taRightJustify:
                              sf.SetAlignment(StringAlignmentFar);
                            taCenter:
                              sf.SetAlignment(StringAlignmentCenter);
                          end;

                          case TextLocation of
                            tlTop:
                              g.DrawString
                                (Text, Length(Text), ft, MakeRect
                                  (2, 2, gpbmp.Width - 4, texth), sf, b);
                            tlLeft:
                              g.DrawString
                                (Text, Length(Text), ft, MakeRect
                                  (2, 2, textw, gpbmp.Height - 4), sf, b);
                            tlBottom:
                              g.DrawString
                                (Text, Length(Text), ft, MakeRect
                                  (2, int(gpbmp.Height) - texth - 2,
                                  gpbmp.Width - 4, texth), sf, b);
                            tlRight:
                              g.DrawString
                                (Text, Length(Text), ft, MakeRect
                                  (int(gpbmp.Width) - textw - 2, 2, textw,
                                  gpbmp.Height - 4), sf, b);
                          end;

                          if Assigned(img) and not img.Empty then
                          begin
                            case TextLocation of
                              tlBottom:
                                img.GDIPDraw
                                  (g, MakeRect((int(gpbmp.Width) - img.Width)
                                      / 2, 0, img.Width, img.Height));
                              tlTop:
                                img.GDIPDraw
                                  (g, MakeRect((int(gpbmp.Width) - img.Width)
                                      / 2, 2 + texth, img.Width, img.Height));
                              tlLeft:
                                img.GDIPDraw
                                  (g, MakeRect(2 + textw, 0, img.Width,
                                    img.Height));
                              tlRight:
                                img.GDIPDraw
                                  (g, MakeRect(0, 0, img.Width, img.Height));
                            end;
                          end;

                          if Assigned(SlideShow.OnCustomDraw) then
                            SlideShow.OnCustomDraw
                              (SlideShow, i, g, MakeRect
                                (0, 0, gpbmp.Width, gpbmp.Height), ikImageText);

                          gpbmp.Save(sta, GetCLSID(itPNG));
                          s.Position := 0;
                          Image.LoadFromStream(s);
                          gpbmp.Free;
                          g.Free;
                          s.Free;
                          b.Free;
                          sf.Free;
                          ft.Free;
                        end;
                      end
                      else
                      begin
                        if Image.Empty and FileExists(Location) then
                          Image.LoadFromFile(Location);
                      end;
                    end;
                  ikText:
                    begin
                      if Text <> '' then
                      begin
                        if TextAutoSize then
                        begin
                          gpbmp := TGPBitmap.Create(1, 1);
                          g := TGPGraphics.Create(gpbmp);
                          ft := g.MakeFont(TextFont);
                          sf := TGPStringFormat.Create;
                          if not WordWrap and Ellipsis then
                          begin
                            sf.SetFormatFlags(StringFormatFlagsNoWrap);
                            sf.SetTrimming(StringTrimmingEllipsisWord);
                          end
                          else if not WordWrap then
                            sf.SetFormatFlags(StringFormatFlagsNoWrap);

                          g.MeasureString(Text, Length(Text), ft, MakeRect
                              (0, 0, TextWidth, 10000), sf, textoutr);

                          sf.Free;
                          ft.Free;

                          g.Free;
                          gpbmp.Free;

                          textw := Round(textoutr.Width + 1);
                          texth := Round(textoutr.Height + 1);
                        end
                        else
                        begin
                          texth := TextHeight;
                          textw := TextWidth;
                        end;

                        s := TMemoryStream.Create;
                        sta := TFixedStreamAdapter.Create(s);
                        gpbmp := TGPBitmap.Create(textw + 4, texth + 4);
                        g := TGPGraphics.Create(gpbmp);
                        g.SetSmoothingMode(SmoothingModeHighQuality);
                        g.SetTextRenderingHint(TextRenderingHintAntiAlias);
                        ft := g.MakeFont(TextFont);
                        b := TGPSolidBrush.Create
                          (MakeColor(255, TextFont.Color));
                        sf := TGPStringFormat.Create;
                        if not WordWrap and Ellipsis then
                        begin
                          sf.SetFormatFlags(StringFormatFlagsNoWrap);
                          sf.SetTrimming(StringTrimmingEllipsisWord);
                        end
                        else if not WordWrap then
                          sf.SetFormatFlags(StringFormatFlagsNoWrap);

                        case TextVerticalAlignment of
                          taAlignTop:
                            sf.SetLineAlignment(StringAlignmentNear);
                          taAlignBottom:
                            sf.SetLineAlignment(StringAlignmentFar);
                          taVerticalCenter:
                            sf.SetLineAlignment(StringAlignmentCenter);
                        end;

                        case TextHorizontalAlignment of
                          taLeftJustify:
                            sf.SetAlignment(StringAlignmentNear);
                          taRightJustify:
                            sf.SetAlignment(StringAlignmentFar);
                          taCenter:
                            sf.SetAlignment(StringAlignmentCenter);
                        end;

                        g.DrawString(Text, Length(Text), ft, MakeRect
                            (2, 2, textw, texth), sf, b);

                        if Assigned(SlideShow.OnCustomDraw) then
                          SlideShow.OnCustomDraw
                            (SlideShow, i, g, MakeRect
                              (0, 0, gpbmp.Width, gpbmp.Height), ikText);

                        gpbmp.Save(sta, GetCLSID(itPNG));
                        s.Position := 0;
                        Image.LoadFromStream(s);

                        gpbmp.Free;
                        g.Free;
                        s.Free;
                        b.Free;
                        sf.Free;
                        ft.Free;
                      end;
                    end;
{$IFNDEF BCB}
{$IFDEF DELPHI_TOUCH}
                  ikPreview:
                    begin
                      if FileExists(Location) then
                      begin
                        GetFilePreview(Location, 500, 500, bmp, errorout);
                        if Assigned(bmp) then
                        begin
                          Image.Assign(bmp);
                          bmp.Free;
                        end;
                      end;
                    end;
{$ENDIF}
{$ENDIF}
                end;

                if not Image.Empty then
                begin
                  FOriginalImage := TGPBitmap.Create(Image.Width, Image.Height);
                  g := TGPGraphics.Create(FOriginalImage);
                  Image.GDIPDraw(g, MakeRect(0, 0, Image.Width, Image.Height));
                  g.Free;
                  FThreadStatus := stLoaded;
                  SlideShow.Changed;
                  if Assigned(SlideShow.OnImageLoaded) then
                    SlideShow.OnImageLoaded(SlideShow, Index);
                end;
              end;

              if (FThumbThreadStatus = stInitialized) and
                (FThreadStatus = stLoaded) and Assigned(FOriginalImage) then
              begin
                if (FOriginalImage.Width > 0) and (FOriginalImage.Height > 0)
                  then
                begin
                  gotimage := true;
                  if Assigned(FThumbnail) then
                    FreeAndNil(FThumbnail);

                  ow := FOriginalImage.Width;
                  oh := FOriginalImage.Height;
                  nw := SlideShow.ThumbNailWidth;
                  nh := SlideShow.ThumbNailHeight;
                  GetAspectSize(w, h, ow, oh, nw, nh, 100);

                  FThumbnail := FOriginalImage.GetThumbnailImage
                    (Round(w), Round(h));
                  FThumbThreadStatus := stLoaded;
                  SlideShow.Changed;
                  if Assigned(SlideShow.OnThumbLoaded) then
                    SlideShow.OnThumbLoaded(SlideShow, Index);
                end;
              end;
            end;
          end;
        end;
        if not gotimage then
        begin
          Sleep(10);
          Application.ProcessMessages;
        end;
      end;
    finally
{$IFNDEF BCB}
{$IFDEF DELPHI_TOUCH}
      CoUninitialize;
{$ENDIF}
{$ENDIF}
    end;
  except
    on E: Exception do
    begin
      Terminate;
    end;
  end;

end;

{$IFNDEF BCB}
{$IFDEF DELPHI_TOUCH}

function TImageLoaderThread.GetFilePreview
  (const FileName: string; w, h: integer; out bmp: TBitmap;
  out ErrorInfo: string): boolean;
  function GetExtImg(const FileName: string; out ExtImg: IExtractImage)
    : boolean;
  var
    Desktop, Target: IShellFolder;
    PIDL: PItemIDList;
    Attr, Eaten: DWORD;
    res: HResult;
  begin
    OleCheck(SHGetMalloc(fMalloc));
    OleCheck(SHGetDesktopFolder(Desktop));
    OleCheck(Desktop.ParseDisplayName(0, nil, PChar(ExcludeTrailingPathDelimiter
            (ExtractFilePath(FileName))), Eaten, PIDL, Attr));
    try
      OleCheck(Desktop.BindToObject(PIDL, nil, IShellFolder, Target));
    finally
      fMalloc.Free(PIDL);
    end;
    OleCheck(Target.ParseDisplayName(0, nil, PChar(ExtractFileName(FileName)),
        Eaten, PIDL, Attr));

    try
      res := Target.GetUIObjectOf(SlideShow.Handle, 1, PIDL, IExtractImage,
        nil, ExtImg);
      result := S_OK = res;
      if not result then
      begin
        if res = E_NOTIMPL then
          ErrorInfo := 'Thumbnail not available for this file type'
        else
          ErrorInfo := Format('GetExtImg failed: %s', [SysErrorMessage(res)]);
      end;
    finally
      fMalloc.Free(PIDL);
    end;
  end;

  function GetThumbnailBmp(const ExtImg: IExtractImage; w, h: integer): TBitmap;
  var
    Size: TSize;
    Buf: array [0 .. MAX_PATH] of Char;
    BmpHandle: HBITMAP;
    Prio, Flags: DWORD;
    hres: HResult;
  begin
    result := nil;
    fRunnableTask := nil;
    Size.cx := w;
    Size.cy := h;
    Prio := IEIT_PRIORITY_NORMAL;
    Flags := IEIFLAG_ASYNC or IEIFLAG_QUALITY or IEIFLAG_SCREEN;
    hres := ExtImg.GetLocation(Buf, SizeOf(Buf), Prio, Size, 32, Flags);
    if hres = E_PENDING then
      { If QueryInterface for IRunnableTask succeed, the fRunnableTask
        interface can be used to abort later the running extraction process. }
      if Failed(ExtImg.QueryInterface(IRunnableTask, fRunnableTask)) then
        fRunnableTask := nil;
    if (hres = S_OK) or (hres = E_PENDING) then
    begin
      result := TBitmap.Create;
      try
        { The call "Extract" consumes a long time depending on its state in the
          Thumbs.db }
        OleCheck(ExtImg.Extract(BmpHandle));
        result.Handle := BmpHandle;
      except
        on E: Exception do
        begin
          ErrorInfo := Format('Extract failed: %s', [E.Message]);
          FreeAndNil(result);
        end;
      end;
    end
    else
      ErrorInfo := Format('GetThumbnailBmp failed: %s', [SysErrorMessage(hres)]
        );
  end;

  function IsWindowsVistaOrHigher: boolean;
  var
    VerInfo: TOSVersioninfo;
  begin
    VerInfo.dwOSVersionInfoSize := SizeOf(TOSVersioninfo);
    GetVersionEx(VerInfo);
    result := VerInfo.dwMajorVersion >= 6;
  end;

  function GetVistaThumbnail(const FileName: string; w, h: integer): TBitmap;
  var
    siif: IShellItemImageFactory;
    si2: IShellItem2;
    BmpHandle: HBITMAP;
    Size: TSize;
  begin
    Size.cx := w;
    Size.cy := h;
    OleCheck(SHCreateItemFromParsingName(PChar(FileName), nil, IShellItem2, si2)
      );
    OleCheck(si2.QueryInterface(IID_IShellItemImageFactory, siif));
    result := TBitmap.Create;
    try
      OleCheck(siif.GetImage(Size, SIIGBF_RESIZETOFIT or SIIGBF_THUMBNAILONLY,
          BmpHandle));
      result.Handle := BmpHandle;
    except
      on E: Exception do
      begin
        ErrorInfo := Format('GetVistaThumbnail failed: %s', [E.Message]);
        FreeAndNil(result);
      end;
    end;
  end;

var
  ExtImg: IExtractImage;
begin
  ErrorInfo := '';
  bmp := nil;
  try
    if IsWindowsVistaOrHigher then
      bmp := GetVistaThumbnail(FileName, w, h)
    else if GetExtImg(FileName, ExtImg) then
      bmp := GetThumbnailBmp(ExtImg, w, h);
  except
    on E: Exception do
      ErrorInfo := Format('GetFileThumbnail failed: %s', [E.Message]);
  end;
  result := bmp <> nil;
end;
{$ENDIF}
{$ENDIF}

procedure TImageLoaderThread.PaintControl;
begin
  if Assigned(FCtrl) and Assigned(FCtrlBitmap) then
  begin
    if Assigned(SlideShow.OnCustomDrawControl) then
      SlideShow.OnCustomDrawControl(SlideShow, FCtrl, ItemIndex, FCtrlBitmap)
    else
      FCtrl.PaintTo(FCtrlBitmap.Canvas, 0, 0);
  end;
end;

{ THandleAppearance }

procedure THandleAppearance.Assign(Source: TPersistent);
begin
  if (Source is THandleAppearance) then
  begin
    FOpacity := (Source as THandleAppearance).Opacity;
    FBorderColor := (Source as THandleAppearance).BorderColor;
    FColor := (Source as THandleAppearance).Color;
    FSize := (Source as THandleAppearance).Size;
    FArrowColor := (Source as THandleAppearance).ArrowColor;
  end;
end;

procedure THandleAppearance.Changed;
begin
  if Assigned(OnChange) then
    OnChange(self);
end;

constructor THandleAppearance.Create(AOwner: TAdvSmoothSlideShow);
begin
  FOwner := AOwner;
  FColor := clSilver;
  FOpacity := 200;
  FBorderColor := clSilver;
  FSize := 35;
  FArrowColor := clBlack;
end;

destructor THandleAppearance.Destroy;
begin
  inherited;
end;

procedure THandleAppearance.SetArrowColor(const Value: TColor);
begin
  if FArrowColor <> Value then
  begin
    FArrowColor := Value;
    Changed;
  end;
end;

procedure THandleAppearance.SetBorderColor(const Value: TColor);
begin
  if FBorderColor <> Value then
  begin
    FBorderColor := Value;
    Changed;
  end;
end;

procedure THandleAppearance.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Changed;
  end;
end;

procedure THandleAppearance.SetOpacity(const Value: Byte);
begin
  if FOpacity <> Value then
  begin
    FOpacity := Value;
    Changed;
  end;
end;

procedure THandleAppearance.SetSize(const Value: integer);
begin
  if FSize <> Value then
  begin
    FSize := Value;
    Changed;
  end;
end;

{ TMargins }
{$IFNDEF DELPHI2006_LVL}

procedure TMargins.Assign(Source: TPersistent);
begin
  if Source is TMargins then
  begin
    FLeft := (Source as TMargins).Left;
    FTop := (Source as TMargins).Top;
    FRight := (Source as TMargins).Right;
    FBottom := (Source as TMargins).Bottom;
  end;
end;

procedure TMargins.Changed;
begin
  FOwner.Changed;
end;

constructor TMargins.Create(AOwner: TAdvSmoothSlideShow);
begin
  FOwner := AOwner;
  FLeft := 0;
  FBottom := 0;
  FRight := 0;
  FTop := 0;
end;

procedure TMargins.SetBottom(const Value: integer);
begin
  if FBottom <> Value then
  begin
    FBottom := Value;
    Changed;
  end;
end;

procedure TMargins.SetLeft(const Value: integer);
begin
  if FLeft <> Value then
  begin
    FLeft := Value;
    Changed;
  end;
end;

procedure TMargins.SetRight(const Value: integer);
begin
  if FRight <> Value then
  begin
    FRight := Value;
    Changed;
  end;
end;

procedure TMargins.SetTop(const Value: integer);
begin
  if FTop <> Value then
  begin
    FTop := Value;
    Changed;
  end;
end;
{$ENDIF}

end.
