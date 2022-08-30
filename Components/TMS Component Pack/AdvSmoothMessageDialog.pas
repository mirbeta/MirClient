{**************************************************************************}
{ TAdvSmoothMessageDialog component                                        }
{ for Delphi & C++Builder                                                  }
{                                                                          }
{ written                                                                  }
{   TMS Software                                                           }
{   copyright © 2010 - 2015                                                }
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

unit AdvSmoothMessageDialog;

{$I TMSDEFS.INC}

//{$DEFINE USEGDIDRAWING}

interface

uses
  Windows, Messages, Forms, Classes, Dialogs, Controls, ClipBrd,
  Graphics, SysUtils, ExtCtrls, GDIPFill, imglist, GDIPPicturecontainer, AdvStyleIF, Math,
  AdvGDIP, Types
  {$IFDEF USEGDIDRAWING}
  ,PictureContainer
  {$ENDIF}
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 3; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 1; // Build nr.

  // version history
  // v1.0.0.0 : first release
  // v1.0.0.1 : Fixed : Issue with ModalResult when closing form
  // v1.0.0.2 : Fixed : Issue with WordWrapping HTML and Caption text
  //          : Fixed : Issue with font initialisation in Older Delphi versions
  // v1.0.0.3 : Fixed : Issue with closing designtime preview dialog
  // v1.0.0.4 : Improved : VK_RETURN focused button ModalResult
  // v1.0.1.0 : New : Added DefaultButton property
  //          : New : Support for Windows Vista and Windows Seven style
  // v1.0.2.0 : New : Built-in support for reduced color set for use with terminal servers
  //          : New : Support for adding ProgressBar
  //          : New : Ability to show the dialog non modal
  //          : New : Exposed events OnClose and OnCanClose in case of non modal
  // v1.0.2.1 : Improved : Automatic calculation of best possible width and height
  //          : Improved : ProgressBar position and calculation
  // v1.0.2.2 : Improved : Automatic width and height calculation combination with Margin property
  // v1.0.2.3 : Fixed : Issue with unnecessary property FormStyle := fsStayOnTop
  // v1.0.3.0 : New : ColorFocused property for each button
  //          : Fixed : Issue with stay on top when hiding parent form
  //          : Fixed : Issue with auto calculation and htmllocation is hlcustom
  // v1.0.3.1 : Fixed : Issue with Disabled Buttons and keyboard
  // v1.0.4.0 : New : functions to get width and height of dialog
  // v1.0.4.1 : Fixed : Issue with dragging dialog with mouse
  //          : Fixed : Issue with multiple message dialogs
  // v1.0.5.0 : New : Shortcut support
  // v1.0.6.0 : New : Built-in support for Office 2010 colors
  // v1.0.6.1 : Fixed : Issue with closing preview at designtime
  // v1.0.6.2 : Improved : FormStyle property
  // v1.0.6.3 : Fixed : Access violation changing properties after preview is used
  // v1.0.6.4 : Improved : ESC handling only when mrCancel button is available
  //          : Improved : Function to return the internal dialog form
  // v1.0.6.5 : Fixed : Issue with OnCanClose and OnClose not called with modal dialog
  // v1.0.6.6 : Fixed : Issue with design time preview handling
  //          : Fixed : Issue with modal popup mode on Application level
  // v1.0.7.0 : New : Public function CloseDialog
  // v1.0.7.1 : Fixed : Issue with accessing properties after CloseDialog
  // v1.0.7.2 : Fixed : Issue with calling CloseDialog within blocking calls
  // v1.0.7.3 : Fixed : Issue with updating progressbar
  // v1.0.8.0 : Improved : MaximumWidth and MaximumHeight properties in combination with large text
  // v1.1.0.0 : New : Metro style support
  // v1.1.0.1 : Improved : Small improvement to return nil if form is inactive in GetForm
  // v1.1.1.0 : New : Timeout property
  // v1.2.0.0 : New : ClipBoard support
  //          : New : Windows 8, Office 2013 styles added
  // v1.2.1.0 : New : MinimumWidth and MinimumHeight properties added
  // v1.2.1.1 : Fixed : Issue with handling MinimumHeight
  // v1.2.2.0 : New : Default MessageDlg Icon support
  //          : New : Anchor hints
  // v1.2.2.1 : Fixed : Issue with repainting on mousemove
  // v1.2.3.0 : New : OnButtonClick and CloseDialog properties to control button behavior
  // v1.2.3.1 : Fixed : Issue with transparent layer creation blocking message processing
  // v1.2.3.2 : Fixed : Issue with showing preview due to transparent layer fix
  // v1.2.4.0 : New : Button Visible property
  // v1.3.0.0 : New : Windows 10, Office 2016 styles added
  // v1.3.0.1 : Improved : Added SmoothMessageDlg overload for dialog position


type
  TAdvSmoothMessageDialog = class;

  TAdvSmoothMessageDialogForm = class(TForm)
  private
    FCaptionRect, FHTMLRect: TGPRectF;
    FKeyDown: Boolean;
    FButtonidx: integer;
    FMouseDownOnButton: Boolean;
    FMainBuffer: TGPBitmap;
    FDialog: TAdvSmoothMessageDialog;
    procedure CMHintShow(var Message: TMessage); message CM_HINTSHOW;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMActivate(var Message: TWMActivate); message WM_ACTIVATE;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMEraseBkGnd(var Message: TWMEraseBkGnd); message WM_ERASEBKGND;
    procedure WMNCHitTest(var Msg: TWMNCHitTest); message WM_NCHITTEST;
    procedure CMDialogKey(Var Msg: TWMKey); message CM_DIALOGKEY;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
  protected
    procedure CreateWnd; override;
    procedure DoCreate; override;
    procedure DoDestroy; override;
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure CopyToClipBoard(Text: String);

    // ---- Calc proc
    procedure UpdateButtons;

    // ---- Paint proc
    procedure Draw(graphics: TGPGraphics);

    // ---- Paint buffer
    procedure CreateMainBuffer;
    procedure DestroyMainBuffer;
    procedure ClearBuffer(graphics: TGPGraphics);
    function CreateGraphics: TGPGraphics;

    //---- Layered window
    procedure SetLayeredWindow;
    procedure UpdateLayered;
    procedure UpdateMainWindow;
    procedure UpdateWindow;
  public
    procedure Init;
    constructor CreateNew(AOwner: TComponent; Dummy: Integer = 0); override;
    property Dialog: TAdvSmoothMessageDialog read FDialog write FDialog;
  end;

  TAdvSmoothMessageDialogLocation = (hlTopLeft, hlTopCenter, hlTopRight, hlCenterLeft, hlCenterCenter, hlCenterRight, hlBottomLeft, hlBottomCenter, hlBottomRight, hlCustom);

  TAdvSmoothMessageDialogHTMLText = class(TPersistent)
  private
    FOwner: TAdvSmoothMessageDialog;
    FURLColor: TColor;
    FShadowOffset: integer;
    FFont: TFont;
    FText: String;
    FShadowColor: TColor;
    FOnChange: TNotifyEvent;
    FLocation: TAdvSmoothMessageDialogLocation;
    FTop: integer;
    FLeft: integer;
    procedure SetFont(const Value: TFont);
    procedure SetLeft(const Value: integer);
    procedure SetLocation(const Value: TAdvSmoothMessageDialogLocation);
    procedure SetShadowColor(const Value: TColor);
    procedure SetShadowOffset(const Value: integer);
    procedure SetText(const Value: string);
    procedure SetTop(const Value: integer);
    procedure SetURLColor(const Value: TColor);
  protected
    procedure Changed;
  public
    constructor Create(AOwner: TAdvSmoothMessageDialog);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Text: string read FText write SetText;
    property Location: TAdvSmoothMessageDialogLocation read FLocation write SetLocation default hlCenterLeft;
    property Top: integer read FTop write SetTop default 0;
    property Left: integer read FLeft write SetLeft default 0;
    property URLColor: TColor read FURLColor write SetURLColor default clBlue;
    property ShadowColor: TColor read FShadowColor write SetShadowColor default clGray;
    property ShadowOffset: integer read FShadowOffset write SetShadowOffset default 5;
    property Font: TFont read FFont write SetFont;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TAdvSmoothMessageDialogButton = class(TCollectionItem)
  private
    FDown, FHover: Boolean;
    FBtnr, FCapRect: TGPRectF;
    FCapPos: TGPPointF;
    Fowner: TAdvSmoothMessageDialog;
    FOpacity: Byte;
    FPicture: TAdvGDIPPicture;
    FColor: TColor;
    FCaption: string;
    FButtonResult: TModalResult;
    FPictureLocation: TGDIPButtonLayout;
    FSpacing: integer;
    FColorDown: TColor;
    FBorderColor: TColor;
    FBorderWidth: integer;
    FBorderOpacity: Byte;
    FHoverColor: TColor;
    FEnabled: Boolean;
    FColorDisabled: TColor;
    FColorFocused: TColor;
    FCloseDialog: Boolean;
    FVisible: Boolean;
    procedure SetCaption(const Value: string);
    procedure SetColor(const Value: TColor);
    procedure SetOpacity(const Value: Byte);
    procedure SetPicture(const Value: TAdvGDIPPicture);
    procedure SetButtonResult(const Value: TModalResult);
    procedure SetPictureLocation(const Value: TGDIPButtonLayout);
    procedure SetSpacing(const Value: integer);
    procedure SetColorDown(const Value: TColor);
    procedure SetBorderColor(const Value: TColor);
    procedure SetBorderWidth(const Value: integer);
    procedure SetBorderOpacity(const Value: Byte);
    procedure SetHoverColor(const Value: TColor);
    procedure SetEnabled(const Value: Boolean);
    procedure SetColorDisabled(const Value: TColor);
    procedure SetColorFocused(const Value: TColor);
    procedure SetCloseDialog(const Value: Boolean);
    procedure SetVisible(const Value: Boolean);
  protected
    procedure Changed;
    procedure PictureChanged(Sender: TObject);
    procedure ButtonChanged(Sender: TObject);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property PictureLocation: TGDIPButtonLayout read FPictureLocation write SetPictureLocation default blPictureLeft;
    property Spacing: integer read FSpacing write SetSpacing default 3;
    property Caption: string read FCaption write SetCaption;
    property CloseDialog: Boolean read FCloseDialog write SetCloseDialog default True;
    property Color: TColor read FColor write SetColor default clGray;
    property ColorDown: TColor read FColorDown write SetColorDown default cldkGray;
    property ColorDisabled: TColor read FColorDisabled write SetColorDisabled default $797979;
    property ColorFocused: TColor read FColorFocused write SetColorFocused default clSilver;
    property HoverColor: TColor read FHoverColor write SetHoverColor default clLtGray;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clBlack;
    property BorderWidth: integer read FBorderWidth write SetBorderWidth default 1;
    property BorderOpacity: Byte read FBorderOpacity write SetBorderOpacity default 255;
    property Opacity: Byte read FOpacity write SetOpacity default 255;
    property ButtonResult: TModalResult read FButtonResult write SetButtonResult default mrOk;
    property Picture: TAdvGDIPPicture read FPicture write SetPicture;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property Visible: Boolean read FVisible write SetVisible default True;
  end;

  TAdvSmoothMessageDialogButtons = class(TCollection)
  private
    FOwner: TAdvSmoothMessageDialog;
    FOnChange: TNotifyEvent;
    function GetItem(Index: Integer): TAdvSmoothMessageDialogButton;
    procedure SetItem(Index: Integer;
      const Value: TAdvSmoothMessageDialogButton);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TAdvSmoothMessageDialog);
    function Add: TAdvSmoothMessageDialogButton;
    function Insert(Index: Integer): TAdvSmoothMessageDialogButton;
    property Items[Index: Integer]: TAdvSmoothMessageDialogButton read GetItem write SetItem; default;
    procedure Delete(Index: Integer);
  published
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TAdvSmoothMessageDialogButtonLayout = (blHorizontal, blVertical);

  TAdvSmoothMessageDialogAnchorClick = procedure(Sender: TObject; Anchor: String) of object;

  TDialogMaxSize = record
    maxbuttonwidth: Double;
    maxbuttonheight: Double;
    maxhtmlheight: Double;
    maxhtmlwidth: Double;
    maxcaptionheight: Double;
    maxcaptionwidth: Double;
    totalmaxheight: Double;
    totalmaxwidth: Double;
    maxprogressheight: Double;
    maxprogresswidth: Double;
  end;

  TAdvSmoothMessageDialogCanCloseEvent = procedure(Sender: TObject; var CanClose: Boolean; ButtonResult: TModalResult) of object;

  TAdvSmoothMessageDialogButtonClick = procedure(Sender: TObject; ButtonIndex: Integer; ButtonResult: TModalResult) of object;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvSmoothMessageDialog = class(TComponent, ITMSStyle, ITMSTones)
  private
    FTMSStyle: TTMSStyle;
    FTimer: TTimer;
    FFocusedButton: integer;
    FMaxDialog: TDialogMaxSize;
    FDesignTime: Boolean;
    FCaption: string;
    FCaptionFill: TGDIPFill;
    FCaptionHeight: integer;
    FMargin: integer;
    FFill: TGDIPFill;
    FImages: TCustomImageList;
    FContainer: TGDIPPictureContainer;
    FHTMLText: TAdvSmoothMessageDialogHTMLText;
    FButtonAreaFill: TGDIPFill;
    FOnChange: TNotifyEvent;
    frm: TAdvSmoothMessageDialogForm;
    FButtons: TAdvSmoothMessageDialogButtons;
    FCaptionLocation: TAdvSmoothMessageDialogLocation;
    FCaptionTop: integer;
    FCaptionLeft: integer;
    FCaptionFont: TFont;
    FButtonFont: TFont;
    FButtonLayout: TAdvSmoothMessageDialogButtonLayout;
    FHTMLAreaHeight: integer;
    FButtonSpacing: integer;
    FMinimumButtonWidth: integer;
    FMinimumButtonHeight: integer;
    FEnableKeyEvents: Boolean;
    FOnAnchorClick: TAdvSmoothMessageDialogAnchorClick;
    FTabStop: Boolean;
    FPosition: TPosition;
    FTop, FLeft: Integer;
    FProgressMax: Double;
    FProgressAppearance: TGDIPProgress;
    FProgressVisible: Boolean;
    FProgressValue: Double;
    FProgressMin: Double;
    FProgressTop: integer;
    FProgressLeft: integer;
    FProgressHeight: integer;
    FProgressPosition: TAdvSmoothMessageDialogLocation;
    FProgressWidth: integer;
    FAutoClose: Boolean;
    FModal: Boolean;
    FOnCanClose: TAdvSmoothMessageDialogCanCloseEvent;
    FOnClose: TNotifyEvent;
    FFormStyle: TFormStyle;
    frmclosed: boolean;
    FMaximumHeight: Integer;
    FMaximumWidth: Integer;
    FTimeOut: Integer;
    FMinimumWidth: Integer;
    FMinimumHeight: Integer;
    FOnButtonClick: TAdvSmoothMessageDialogButtonClick;
    {$IFDEF USEGDIDRAWING}
    FGDIContainer: TPictureContainer;
    {$ENDIF}
    procedure SetCaptionFill(const Value: TGDIPFill);
    procedure SetCaptionHeight(const Value: integer);
    procedure SetMargin(const Value: integer);
    procedure SetFill(const Value: TGDIPFill);
    procedure SetHTMLText(const Value: TAdvSmoothMessageDialogHTMLText);
    procedure SetButtonAreaFill(const Value: TGDIPFill);
    procedure SetButtons(const Value: TAdvSmoothMessageDialogButtons);
    procedure SetCaptionLocation(const Value: TAdvSmoothMessageDialogLocation);
    procedure SetCaptionLeft(const Value: integer);
    procedure SetCaptionTop(const Value: integer);
    procedure SetCaptionFont(const Value: TFont);
    procedure SetButtonFont(const Value: TFont);
    procedure SetButtonLayout(const Value: TAdvSmoothMessageDialogButtonLayout);
    procedure SetButtonSpacing(const Value: integer);
    procedure SetEnableKeyEvents(const Value: Boolean);
    procedure SetTabStop(const Value: Boolean);
    function GetVersion: String;
    procedure SetVersion(const Value: String);
    procedure SetDefaultButton(const Value: integer);
    procedure SetProgressAppearance(const Value: TGDIPProgress);
    procedure SetProgressHeight(const Value: integer);
    procedure SetProgressLeft(const Value: integer);
    procedure SetProgressMax(const Value: Double);
    procedure SetProgressMin(const Value: Double);
    procedure SetProgressPosition(const Value: TAdvSmoothMessageDialogLocation);
    procedure SetProgressTop(const Value: integer);
    procedure SetProgressValue(const Value: Double);
    procedure SetProgressVisible(const Value: Boolean);
    procedure SetProgressWidth(const Value: integer);
    procedure SetAutoClose(const Value: Boolean);
    procedure SetModal(const Value: Boolean);
    procedure SetFormStyle(const Value: TFormStyle);
    procedure SetMaximumHeight(const Value: Integer);
    procedure SetMaximumWidth(const Value: Integer);
    procedure SetTimeOut(const Value: Integer);
    procedure SetMinimumHeight(const Value: Integer);
    procedure SetMinimumWidth(const Value: Integer);
  protected
    procedure ReadTMSStyle(Reader: TReader);
    procedure WriteTMSStyle(Writer: TWriter);
    procedure DefineProperties(Filer: TFiler); override;
    procedure Changed(ARepaint: Boolean = False); virtual;
    procedure ProgressChanged(Sender: TObject);
    procedure TimerEvent(Sender: TObject);
    procedure CloseForm(Sender: TObject; var Action: TCloseAction);
    procedure ShowForm(Sender: TObject);
    procedure FillChanged(Sender: TObject);
    procedure ButtonsChanged(Sender: TObject);
    procedure Fontchanged(Sender: TObject);
    function DrawHTMLText({$IFDEF USEGDIDRAWING}ca: TCanvas; {$ELSE} g: TGPGraphics;{$ENDIF} HTML: TAdvSmoothMessageDialogHTMLText; r: TGPRectF; str: String;
      DoAnchor: Boolean = false; Focus: Boolean = False; fX: integer = -1; fY: integer = -1): String;
    procedure DrawButton(g: TGPGraphics; btn: TAdvSmoothMessageDialogButton; r: TGPRectF);
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    function GetProgressBarRect(R: TRect): TGPRectF;
    function GetCaptionRect(R: TRect): TGPRectF;
    function GetHTMLRect(R: TRect): TGPRectF;
    function GetButtonRect(R: TRect): TGPRectF;
    function GetButtonIndexAtXY(X, Y: integer): integer;
    function GetAnchorAt(g: TGPGraphics; X, Y: integer; R: TRect; Focus: Boolean = False): String;
    procedure CalculateMaximum;
    procedure InitSample;
    procedure SendModalResult(Result: TModalResult; AButtonIndex: Integer; ACloseDialog: Boolean); overload;
    procedure SetName(const Value: TComponentName); override;
    function GetVersionNr: integer;
    function CreateAndShowDialog: TModalResult;
    function HasCancel: boolean;
  public
    function GetStrippedHTMLText: String;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function ExecuteDialog: TModalResult;
    function Execute: Boolean;
    procedure Preview;
    procedure SetComponentStyle(AStyle: TTMSStyle);
    function GetComponentStyle: TTMSStyle;
    procedure SetColorTones(ATones: TColorTones);
    procedure SaveToTheme(FileName: String);
    procedure LoadFromTheme(FileName: String);
    function GetThemeID: String;
    procedure SendModalResult(Result: TModalResult); overload;
    function GetWidth: integer;
    function GetHeight: integer;
    function GetForm: TAdvSmoothMessageDialogForm;
    procedure CloseDialog;
    {$IFDEF USEGDIDRAWING}
    property GDIPictureContainer: TPictureContainer read FGDIContainer write FGDIContainer;
    {$ENDIF}
  published
    property MaximumWidth: Integer read FMaximumWidth write SetMaximumWidth default 10000;
    property MaximumHeight: Integer read FMaximumHeight write SetMaximumHeight default 10000;
    property MinimumWidth: Integer read FMinimumWidth write SetMinimumWidth default 0;
    property MinimumHeight: Integer read FMinimumHeight write SetMinimumHeight default 0;
    property Modal: Boolean read FModal write SetModal default true;
    property FormStyle: TFormStyle read FFormStyle write SetFormStyle default fsNormal;
    property AutoClose: Boolean read FAutoClose write SetAutoClose default true;
    property EnableKeyEvents: Boolean read FEnableKeyEvents write SetEnableKeyEvents default true;
    property Images: TCustomImageList read FImages write FImages;
    property PictureContainer: TGDIPPictureContainer read FContainer write FContainer;
    property Margin: integer read FMargin write SetMargin default 0;
    property ButtonLayout: TAdvSmoothMessageDialogButtonLayout read FButtonLayout write SetButtonLayout default blHorizontal;
    property ButtonAreaFill: TGDIPFill read FButtonAreaFill write SetButtonAreaFill;
    property Buttons: TAdvSmoothMessageDialogButtons read FButtons write SetButtons;
    property ButtonSpacing: integer read FButtonSpacing write SetButtonSpacing default 5;
    property DefaultButton: integer read FFocusedButton write SetDefaultButton default 0;
    property Caption: string read FCaption write FCaption;
    property CaptionFont: TFont read FCaptionFont write SetCaptionFont;
    property ButtonFont: TFont read FButtonFont write SetButtonFont;
    property CaptionFill: TGDIPFill read FCaptionFill write SetCaptionFill;
    property CaptionLocation: TAdvSmoothMessageDialogLocation read FCaptionLocation write SetCaptionLocation default hlCenterCenter;
    property CaptionHeight: integer read FCaptionHeight write SetCaptionHeight default 30;
    property CaptionLeft: integer read FCaptionLeft write SetCaptionLeft default 0;
    property CaptionTop: integer read FCaptionTop write SetCaptionTop default 0;
    property Fill: TGDIPFill read FFill write SetFill;
    property HTMLText: TAdvSmoothMessageDialogHTMLText read FHTMLText write SetHTMLText;
    property DialogLeft: Integer read FLeft write FLeft default 0;
    property Position: TPosition read FPosition write FPosition default poDefaultPosOnly;
    property DialogTop: Integer read FTop write FTop default 0;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnAnchorClick: TAdvSmoothMessageDialogAnchorClick read FOnAnchorClick write FOnAnchorClick;
    property TabStop: Boolean read FTabStop write SetTabStop default false;
    property Version: String read GetVersion write SetVersion;
    property ProgressMinimum: Double read FProgressMin write SetProgressMin;
    property ProgressMaximum: Double read FProgressMax write SetProgressMax;
    property ProgressValue: Double read FProgressValue write SetProgressValue;
    property ProgressPosition: TAdvSmoothMessageDialogLocation read FProgressPosition write SetProgressPosition default hlCenterCenter;
    property ProgressHeight: integer read FProgressHeight write SetProgressHeight default 15;
    property ProgressWidth: integer read FProgressWidth write SetProgressWidth default 130;
    property ProgressVisible: Boolean read FProgressVisible write SetProgressVisible default false;
    property ProgressLeft: integer read FProgressLeft write SetProgressLeft default 0;
    property ProgressTop: integer read FProgressTop write SetProgressTop default 0;
    property ProgressAppearance: TGDIPProgress read FProgressAppearance write SetProgressAppearance;
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
    property OnCanClose: TAdvSmoothMessageDialogCanCloseEvent read FOnCanClose write FOnCanClose;
    property OnButtonClick: TAdvSmoothMessageDialogButtonClick read FOnButtonClick write FOnButtonClick;
    property TimeOut: Integer read FTimeOut write SetTimeOut default 0;
  end;

procedure ShowSmoothMessage(const Msg: string; AStyle: TTMSStyle = tsOffice2007Luna);

function SmoothMessageDlg(const Title, Msg: String; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: LongInt; APosition: TPosition; AStyle: TTMSStyle = tsOffice2007Luna): Integer; overload;

function SmoothMessageDlg(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; AStyle: TTMSStyle = tsOffice2007Luna): Integer; overload;

function SmoothMessageDlg(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; DefaultButton: TMsgDlgBtn; AStyle: TTMSStyle = tsOffice2007Luna): Integer; overload;

function SmoothMessageDlgPos(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; X, Y: Integer; AStyle: TTMSStyle = tsOffice2007Luna): Integer; overload;

function SmoothMessageDlgPos(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; X, Y: Integer;
  DefaultButton: TMsgDlgBtn; AStyle: TTMSStyle = tsOffice2007Luna): Integer; overload;

  
implementation

uses
  CommCtrl, ShellApi, Consts;

{$IFNDEF USEGDIDRAWING}
{$I GDIPHTMLEngine.pas}
{$ELSE}
{$I HTMLEngo.pas}
{$ENDIF}

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

function SmoothMessageDlgPosInt(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; X, Y: Integer;
  DefaultButton: TMsgDlgBtn; AStyle: TTMSStyle; Position: TPosition): Integer; overload;
var
  i: integer;
  d: TAdvSmoothMessageDialog;
  icon: TIcon;
begin
  d := TAdvSmoothMessageDialog.Create(Application.MainForm);

  d.FFocusedButton := 0;
  i := 0;

  if mbOK in Buttons then
    with d.Buttons.Add do
    begin
      Caption := SMsgDlgOK;
      ButtonResult := mrOK;

      if DefaultButton = mbOK then
        d.FFocusedButton := i;
      inc(i);
    end;
  if mbCancel in Buttons then
    with d.Buttons.Add do
    begin
      Caption := SMsgDlgCancel;
      ButtonResult := mrCancel;
      if DefaultButton = mbCancel then
        d.FFocusedButton := i;
      inc(i);
    end;
  if mbYES in Buttons then
    with d.Buttons.Add do
    begin
      Caption := SMsgDlgYes;
      ButtonResult := mrYes;
      if DefaultButton = mbYes then
        d.FFocusedButton := i;
      inc(i);
    end;
  if mbNO in Buttons then
    with d.Buttons.Add do
    begin
      Caption := SMsgDlgNo;
      ButtonResult := mrNo;
      if DefaultButton = mbNO then
        d.FFocusedButton := i;
      inc(i);
    end;
  if mbAbort in Buttons then
    with d.Buttons.Add do
    begin
      Caption := SMsgDlgAbort;
      ButtonResult := mrAbort;
      if DefaultButton = mbAbort then
        d.FFocusedButton := i;
      inc(i);
    end;
  if mbRetry in Buttons then
    with d.Buttons.Add do
    begin
      Caption := SMsgDlgRetry;
      ButtonResult := mrRetry;
      if DefaultButton = mbRetry then
        d.FFocusedButton := i;
      inc(i);
     end;
  if mbIgnore in Buttons then
    with d.Buttons.Add do
    begin
      Caption := SMsgDlgIgnore;
      ButtonResult := mrIgnore;
      if DefaultButton = mbIgnore then
        d.FFocusedButton := i;
      inc(i);
    end;
  if mbAll in Buttons then
    with d.Buttons.Add do
    begin
      Caption := SMsgDlgAll;
      ButtonResult := mrAll;
      if DefaultButton = mbAll then
        d.FFocusedButton := i;
      inc(i);
    end;
  if mbNoToAll in Buttons then
    with d.Buttons.Add do
    begin
      Caption := SMsgDlgNoToAll;
      ButtonResult := mrNoToAll;
      if DefaultButton = mbNoToAll then
        d.FFocusedButton := i;
      inc(i);
    end;
  if mbYesToAll in Buttons then
    with d.Buttons.Add do
    begin
      Caption := SMsgDlgYesToAll;
      ButtonResult := mrYesToAll;
      if DefaultButton = mbYesToAll then
        d.FFocusedButton := i;
      inc(i);
    end;
  if mbHelp in Buttons then
    with d.Buttons.Add do
    begin
      Caption := 'Help';
      if DefaultButton = mbHelp then
        d.FFocusedButton := i;
    end;

  d.Caption := Title;
  d.CaptionLocation := hlCenterLeft;
  d.HTMLText.Text := Msg;

  if DlgType <> mtCustom then
  begin
    icon := TIcon.Create;
    try
    case DlgType of
      mtWarning: icon.Handle := LoadIcon(icon.Handle, PChar(IDI_WARNING));
      mtError: icon.Handle := LoadIcon(icon.Handle, PChar(IDI_ERROR));
      mtConfirmation, mtInformation: icon.Handle := LoadIcon(icon.Handle, PChar(IDI_INFORMATION));
    end;
     d.Fill.Picture.Assign(icon);
     d.Fill.PicturePosition := ppTopRight;
     d.Fill.PictureLeft := -3;
     d.Fill.PictureTop := 3;
    finally
      icon.Free;
    end;
  end;

  d.DialogLeft := X;
  d.DialogTop := Y;
  d.Position := Position;
  d.SetComponentStyle(Astyle);
  Result := d.CreateAndShowDialog;
  d.Free;
end;


function SmoothMessageDlg(const Title, Msg: String; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: LongInt; APosition: TPosition; AStyle: TTMSStyle = tsOffice2007Luna): Integer;
begin
  Result := SmoothMessageDlgPosInt(Title, Msg, DlgType, Buttons, HelpCtx, 0, 0, mbOK, AStyle, APosition);
end;

function SmoothMessageDlg(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; AStyle: TTMSStyle = tsOffice2007Luna): Integer;
begin
  Result := SmoothMessageDlgPosInt(Title, Msg, DlgType, Buttons, HelpCtx, 0, 0, mbOK, AStyle, poScreenCenter);
end;

function SmoothMessageDlg(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; DefaultButton: TMsgDlgBtn; AStyle: TTMSStyle = tsOffice2007Luna): Integer; overload;
begin
  Result := SmoothMessageDlgPosInt(Title, Msg, DlgType, Buttons, HelpCtx, 0, 0, DefaultButton, AStyle, poScreenCenter);
end;

function SmoothMessageDlgPos(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; X, Y: Integer; AStyle: TTMSStyle = tsOffice2007Luna): Integer; overload;
begin
  Result := SmoothMessageDlgPosInt(Title, Msg, DlgType, Buttons, HelpCtx, X,Y, mbOK, AStyle, poDesigned);
end;

function SmoothMessageDlgPos(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; X, Y: Integer;
  DefaultButton: TMsgDlgBtn; AStyle: TTMSStyle = tsOffice2007Luna): Integer; overload;
begin
  Result := SmoothMessageDlgPosInt(Title, Msg, DlgType, Buttons, HelpCtx, X,Y, DefaultButton, AStyle, poDesigned);
end;

procedure ShowSmoothMessage(const Msg: String; AStyle: TTMSStyle = tsOffice2007Luna);
var
  d: TAdvSmoothMessageDialog;
begin
  d := TAdvSmoothMessageDialog.Create(Application.MainForm);
  d.FFocusedButton := 0;
  d.Buttons.Add.Caption := 'OK';
  d.Caption := Application.Title;
  d.CaptionLocation := hlCenterLeft;
  d.Position := poScreenCenter;
  d.HTMLText.Text := Msg;
  d.SetComponentStyle(Astyle);
  d.ExecuteDialog;
  d.Free;
end;

procedure DrawFocus(g: TGPGraphics; r: TGPRectF; rn: Integer; c: TColor);
var
  pathfocus: TGPGraphicsPath;
  pfocus: TGPPen;
begin
  pathfocus := GDIPFill.CreateRoundRectangle(r, rn, rtBoth, false);
  pfocus := TGPPen.Create(MakeColor(255, c), 1);
  pfocus.SetDashStyle(DashStyleDot);
  g.DrawPath(pfocus, pathfocus);
  pfocus.Free;
  pathfocus.Free;
end;

function PtInGPRect(r: TGPRectF; pt: TPoint): Boolean;
begin
  result := ((pt.X >= r.X) and (pt.X <= r.X + r.Width)) and
     ((pt.Y >= r.Y) and (pt.Y <= r.Y + r.Height));
end;

procedure GetObjectLocation(var x, y: single; rectangle: TGPRectF; objectwidth, objectheight: integer; location: TAdvSmoothMessageDialogLocation);
var
  w, h, tw, th: single;
begin
  tw := objectwidth;
  th := objectheight;
  w := rectangle.Width;
  h := rectangle.Height;
  case location of
    hlTopLeft:
    begin
      x := 0;
      y := 0;
    end;
    hlTopRight:
    begin
      x := w - tw;
      y := 0;
    end;
    hlBottomLeft:
    begin
      x := 0;
      y := h - th;
    end;
    hlBottomRight:
    begin
      x := w - tw;
      y := h - th;
    end;
    hlTopCenter:
    begin
      x := (w - tw) / 2;
      y := 0;
    end;
    hlBottomCenter:
    begin
      x := (w - tw) / 2;
      y := h - th;
    end;
    hlCenterCenter:
    begin
      x := (w - tw) / 2;
      y := (h - th) / 2;
    end;
    hlCenterLeft:
    begin
      x := 0;
      y := (h - th) / 2;
    end;
    hlCenterRight:
    begin
      x := w - tw;
      y := (h - th) / 2;
    end;
  end;

  x := x + Round(rectangle.X);
  y := y + Round(rectangle.Y);
end;

{ TAdvSmoothMessageDialog }

procedure TAdvSmoothMessageDialog.Assign(Source: TPersistent);
begin
  if (Source is TAdvSmoothMessageDialog) then
  begin
    FMargin := (Source as TAdvSmoothMessageDialog).Margin;
    FCaption := (Source as TAdvSmoothMessageDialog).Caption;
    FCaptionFill.Assign((Source as TAdvSmoothMessageDialog).CaptionFill);
    FCaptionHeight := (Source as TAdvSmoothMessageDialog).CaptionHeight;
    FFill.Assign((Source as TAdvSmoothMessageDialog).Fill);
    FHTMLText.Assign((Source as TAdvSmoothMessageDialog).HTMLText);
    FCaptionLocation := (Source as TAdvSmoothMessageDialog).CaptionLocation;
    FCaptionLeft := (Source as TAdvSmoothMessageDialog).CaptionLeft;
    FCaptionTop := (Source as TAdvSmoothMessageDialog).CaptionTop;
    FCaptionFont.Assign((Source as TAdvSmoothMessageDialog).CaptionFont);
    FButtonFont.Assign((Source as TAdvSmoothMessageDialog).ButtonFont);
    FButtonLayout := (Source as TAdvSmoothMessageDialog).ButtonLayout;
    FButtonSpacing := (Source as TAdvSmoothMessageDialog).ButtonSpacing;
    FEnableKeyEvents := (Source as TAdvSmoothMessageDialog).EnableKeyEvents;
    FTabStop := (source as TAdvSmoothMessageDialog).TabStop;
    FProgressMin := (Source as TAdvSmoothMessageDialog).ProgressMinimum;
    FProgressMax := (Source as TAdvSmoothMessageDialog).ProgressMaximum;
    FProgressValue := (Source as TAdvSmoothMessageDialog).ProgressValue;
    FProgressHeight := (Source as TAdvSmoothMessageDialog).ProgressHeight;
    FProgressWidth := (Source as TAdvSmoothMessageDialog).ProgressWidth;
    FProgressPosition := (Source as TAdvSmoothMessageDialog).ProgressPosition;
    FProgressVisible := (Source as TAdvSmoothMessageDialog).ProgressVisible;
    FProgressTop := (Source as TAdvSmoothMessageDialog).ProgressTop;
    FProgressLeft := (Source as TAdvSmoothMessageDialog).ProgressLeft;
    FProgressAppearance.Assign((Source as TAdvSmoothMessageDialog).ProgressAppearance);
    FAutoClose := (Source as TAdvSmoothMessageDialog).AutoClose;
    FModal := (Source as TAdvSmoothMessageDialog).Modal;
    FMaximumHeight := (Source as TAdvSmoothMessageDialog).MaximumHeight;
    FMaximumWidth := (Source as TAdvSmoothMessageDialog).MaximumWidth;
    FMinimumHeight := (Source as TAdvSmoothMessageDialog).MinimumHeight;
    FMinimumWidth := (Source as TAdvSmoothMessageDialog).MinimumWidth;

    Changed;
  end;
end;

procedure TAdvSmoothMessageDialog.ButtonsChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothMessageDialog.CalculateMaximum;
var
  i: integer;
  ff: TGPFontFamily;
  f: TGPFont;
  fs: integer;
  sf: TGPStringFormat;
  sri: TGPRectF;
  g: TGPGraphics;
  mxw, mxh: Double;
  maxr: TGPRectF;
  XSize, YSize: integer;
  a, s, k: string;
  l: integer;
  m: integer;
  hr: TRect;
  bmp: TBitmap;
  {$IFDEF USEGDIDRAWING}
  ca: TCanvas;
  {$ENDIF}
begin
  FMaxDialog.maxhtmlheight := 0;
  FMaxDialog.maxhtmlwidth := 0;
  FMaxDialog.maxbuttonwidth := 0;
  FMaxDialog.maxbuttonheight := 0;
  FMaxDialog.maxcaptionheight := 0;
  FMaxDialog.maxcaptionwidth := 0;
  FMaxDialog.maxprogressheight := 0;
  FMaxDialog.maxprogresswidth := 0;
  FMaxDialog.totalmaxheight := 0;
  FMaxDialog.totalmaxwidth := 0;

  bmp := TBitmap.Create;
  g := TGPGraphics.Create(bmp.Canvas.Handle);
  g.SetTextRenderingHint(TextRenderingHintAntiAlias);

  maxr := MakeRect(0, 0, MaximumWidth, MaximumHeight);

  if Buttons.Count > 0 then
  begin
    //BUTTON maximum width and height
    ff := TGPFontFamily.Create(ButtonFont.Name);
    if (ff.Status in [FontFamilyNotFound, FontStyleNotFound]) then
    begin
      ff.Free;
      ff := TGPFontFamily.Create('Arial');
    end;

    fs := 0;
    if (fsBold in ButtonFont.Style) then
      fs := fs + 1;
    if (fsItalic in ButtonFont.Style) then
      fs := fs + 2;
    if (fsUnderline in ButtonFont.Style) then
      fs := fs + 4;

    sf := TGPStringFormat.Create;
    f := TGPFont.Create(ff, ButtonFont.Size, fs, UnitPoint);

    mxw := 0;
    mxh := 0;
    g.MeasureString('gh', Length('gh'), f, maxr, sf, sri);
    for I := 0 to Buttons.Count - 1 do
    begin
      g.MeasureString(Buttons[i].Caption, length(Buttons[i].Caption), f, maxr, sf, sri);
      if Assigned(Buttons[I].Picture) and not (Buttons[I].Picture.Empty) then
      begin
        Buttons[I].Picture.GetImageSizes;
        if ((Buttons[I].PictureLocation = blPictureLeft) or (Buttons[I].PictureLocation = blPictureRight)) then
          sri.Width := sri.Width + Buttons[I].Picture.Width + Buttons[I].Spacing
        else
          sri.Width := Buttons[I].Picture.Width;
      end;

      sri.Width := Max(75, ((sri.Width + 10) + ButtonSpacing));

      if sri.Width > mxw then
        mxw := sri.Width;

      if Assigned(Buttons[I].Picture) and not (Buttons[I].Picture.Empty) then
      begin
        Buttons[I].Picture.GetImageSizes;
        if ((Buttons[I].PictureLocation = blPictureTop) or (Buttons[I].PictureLocation = blPictureBottom)) then
          sri.Height := sri.Height + Buttons[I].Picture.Height + Buttons[I].Spacing
        else
          sri.Height := Buttons[I].Picture.Height;
      end;

      sri.Height := Max(25, ((sri.Height + 10) + ButtonSpacing));

      if sri.Height > mxh then
        mxh := sri.Height;
    end;

    FMaxDialog.maxbuttonwidth := mxw;
    FMaxDialog.maxbuttonheight := mxh;
    FMaxDialog.totalmaxwidth := FMaxDialog.maxbuttonwidth;
    FMaxDialog.totalmaxheight := FMaxDialog.maxbuttonheight;

    sf.Free;
    f.Free;
    ff.Free;
  end;

  //CAPTION maximum width and height

  if Caption <> '' then
  begin
    ff := TGPFontFamily.Create(CaptionFont.Name);
    if (ff.Status in [FontFamilyNotFound, FontStyleNotFound]) then
    begin
      ff.Free;
      ff := TGPFontFamily.Create('Arial');
    end;

    fs := 0;
    if (fsBold in CaptionFont.Style) then
      fs := fs + 1;
    if (fsItalic in CaptionFont.Style) then
      fs := fs + 2;
    if (fsUnderline in CaptionFont.Style) then
      fs := fs + 4;

    sf := TGPStringFormat.Create;
    f := TGPFont.Create(ff, CaptionFont.Size, fs, UnitPoint);

    g.MeasureString(Caption, Length(Caption), f, maxr, sf, sri);
    FMaxDialog.maxcaptionheight := CaptionHeight;
    FMaxDialog.maxcaptionwidth := sri.Width + 20;

    FMaxDialog.totalmaxheight := FMaxDialog.totalmaxheight + FMaxDialog.maxcaptionheight;

    if FMaxDialog.maxcaptionwidth > FMaxDialog.totalmaxwidth then
      FMaxDialog.totalmaxwidth := FMaxDialog.maxcaptionwidth;

    sf.Free;
    f.Free;
    ff.Free;
  end;

  //HTML maximum width and height
  with HTMLText do
  begin
    if Text <> '' then
    begin
      {$IFDEF USEGDIDRAWING}
      ca := TCanvas.Create;
      ca.Handle := g.GetHDC;
      ca.Font.Assign(FFont);
      HTMLDrawEx(ca, Text,Bounds(Round(maxr.X), Round(maxr.Y), Round(maxr.Width), Round(maxr.Height)),FImages, 0,0,-1,-1,FShadowOffset,
        False,true,false,false, False,False,true,1.0,FURLColor,clNone,clNone,FShadowColor,a,s,k,XSize,YSize,l,m,hr,nil,FGDIContainer,2);
      g.ReleaseHDC(ca.Handle);
      ca.Free;
      {$ELSE}
      HTMLDrawGDIP(g, FFont, Text,Bounds(Round(maxr.X), Round(maxr.Y), Round(maxr.Width), Round(maxr.Height)),FImages, 0,0,-1,-1,FShadowOffset,
        False,true,false,false, False,False,true,1.0,FURLColor,clNone,clNone,FShadowColor,a,s,k,XSize,YSize,l,m,hr,nil,FContainer,2);
      {$ENDIF}

      FMaxDialog.maxhtmlheight := YSize + 20 + Top;
      FMaxDialog.maxhtmlwidth := XSize + 20 + Left;

      FMaxDialog.totalmaxheight := FMaxDialog.totalmaxheight + FMaxDialog.maxhtmlheight;

      if FMaxDialog.maxhtmlwidth > FMaxDialog.totalmaxwidth then
        FMaxDialog.totalmaxwidth := FMaxDialog.maxhtmlwidth;
    end;
  end;


  if ProgressVisible and (ProgressPosition <> hlCustom) then
  begin
    FMaxDialog.maxprogressheight := ProgressHeight + 25;
    FMaxDialog.maxprogresswidth := ProgressWidth + 20;
    FMaxDialog.totalmaxheight := FMaxDialog.totalmaxheight + FMaxDialog.maxprogressheight;

    if FMaxDialog.maxprogresswidth > FMaxDialog.totalmaxwidth then
      FMaxDialog.totalmaxwidth := FMaxDialog.maxprogresswidth;
  end;


  FMaxDialog.totalmaxheight := FMaxDialog.totalmaxheight + Margin;
  FMaxDialog.totalmaxwidth := FMaxDialog.totalmaxwidth + (Margin * 2);

  if (MinimumWidth <> 0) then
    FMaxDialog.totalmaxwidth := Max(MinimumWidth, FMaxDialog.totalmaxwidth);

  if (MinimumHeight <> 0) then
    FMaxDialog.totalmaxheight := Max(MinimumHeight, FMaxDialog.totalmaxheight);

  g.Free;
  bmp.Free;
end;

procedure TAdvSmoothMessageDialog.Changed(ARepaint: Boolean = False);
begin
  if not frmclosed and Assigned(frm) then
  begin
    if not ARepaint then
    begin
      CalculateMaximum;
      frm.Width := Round(FMaxDialog.totalmaxwidth);
      frm.Height := Round(FMaxDialog.totalmaxheight + Margin * 2);
      frm.UpdateButtons;
    end;
    frm.UpdateWindow;
    {$IFDEF USEGDIDRAWING}
    frm.Invalidate;
    {$ENDIF}
  end;
end;

procedure TAdvSmoothMessageDialog.CloseDialog;
begin
  if Assigned(frm) then
  begin
    SendMessage(frm.Handle, WM_CLOSE, 0,0);
    frm := nil;
    Application.ProcessMessages;
  end;
end;

procedure TAdvSmoothMessageDialog.CloseForm(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
  frmclosed := true;
end;

constructor TAdvSmoothMessageDialog.Create(AOwner: TComponent);
begin
  inherited;
  FFill := TGDIPFill.Create;
  FFill.OnChange := fillChanged;
  FCaptionFill := TGDIPFill.Create;
  FCaptionFill.OnChange := fillChanged;
  FCaptionHeight := 30;
  FMargin := 0;
  FHTMLText := TAdvSmoothMessageDialogHTMLText.Create(Self);
  FButtonAreaFill := TGDIPFill.Create;
  FButtonAreaFill.OnChange := fillChanged;
  FHTMLAreaHeight := 50;
  FButtons := TAdvSmoothMessageDialogButtons.Create(Self);
  FButtons.OnChange := ButtonsChanged;
  FCaptionLocation := hlCenterCenter;
  FCaptionLeft := 0;
  FCaptionTop := 0;
  FCaptionFont := TFont.Create;
  FCaptionFont.OnChange := Fontchanged;
  FButtonFont := TFont.Create;
  FButtonFont.OnChange := Fontchanged;
  {$IFNDEF DELPHI9_LVL}
  FCaptionFont.Name := 'Tahoma';
  FButtonFont.Name := 'Tahoma';
  {$ENDIF}
  FButtonLayout := blHorizontal;
  FButtonSpacing := 5;
  FMinimumButtonWidth := 75;
  FMinimumButtonHeight := 50;
  FEnableKeyEvents := true;
  FTabStop := false;
  FFocusedButton := 0;
  FProgressMin := 0;
  FProgressMax := 100;
  FProgressValue := 0;
  FProgressHeight := 15;
  FProgressWidth := 130;
  FProgressPosition := hlCenterCenter;
  FProgressVisible := false;
  FProgressLeft := 0;
  FProgressTop := 0;
  FAutoClose := true;
  FModal := true;
  FProgressAppearance := TGDIPProgress.Create;
  FProgressAppearance.OnChange := ProgressChanged;
  FFormStyle := fsNormal;
  FMaximumHeight := 10000;
  FMaximumWidth := 10000;
  FMinimumWidth := 0;
  FMaximumWidth := 0;

  FTimeOut := 0;
  FTimer := TTimer.Create(Self);
  FTimer.Interval := TimeOut;
  FTimer.Enabled := False;
  FTimer.OnTimer := TimerEvent;

  FDesignTime := (csDesigning in ComponentState) and not
    ((csReading in Owner.ComponentState) or (csLoading in Owner.ComponentState));

  if FDesignTime then
  begin
    InitSample;
    SetComponentStyle(tsOffice2007Luna);
  end;
end;

destructor TAdvSmoothMessageDialog.Destroy;
begin
  FTimer.Free;
  FFill.Free;
  FCaptionFill.Free;
  FHTMLText.Free;
  FButtonAreaFill.Free;
  FButtons.Free;
  FCaptionFont.Free;
  FButtonFont.Free;
  FProgressAppearance.Free;
  inherited;
end;

procedure TAdvSmoothMessageDialog.DrawButton(g: TGPGraphics;
  btn: TAdvSmoothMessageDialogButton; r: TGPRectF);
var
  b: TGDIPDialogButton;
  c: TColor;
begin
  if btn.Visible then
  begin
    b := TGDIPDialogButton.Create;
    b.Spacing := btn.Spacing;
    b.Layout := btn.PictureLocation;
    b.Font := ButtonFont;
    c := btn.Color;

    if FFocusedButton = btn.Index then
      c := btn.ColorFocused;

    if btn.Enabled then
    begin
      if btn.FDown then
        c := btn.ColorDown
      else if btn.FHover then
        c := btn.HoverColor;
    end
    else
      c := btn.ColorDisabled;

      {$IFDEF USEGDIDRAWING}
      b.DrawCaption := False;
      {$ENDIF}

      b.Draw(g, btn.Caption, Round(r.X), Round(r.Y), Round(r.Width), Round(r.Height), c,
        btn.BorderColor, btn.BorderWidth, btn.BorderOpacity, btn.Opacity, (btn.BorderColor <> clNone), btn.FDown, btn.Picture, False);

      btn.FCapRect := b.CaptionRect;
      btn.FCapPos := b.CaptionPos;

      if btn.Index = FFocusedButton then
        DrawFocus(g, MakeRect(r.X - 1 , r.Y - 1, r.Width + 1, r.Height + 1), 4, clBlack );

    b.Free;
  end;
end;

{$IFDEF USEGDIDRAWING}
function TAdvSmoothMessageDialog.DrawHTMLText(ca: TCanvas;
  HTML: TAdvSmoothMessageDialogHTMLText; r: TGPRectF; str: String;
  DoAnchor: Boolean = False; Focus: Boolean = False; fX: Integer = -1; fY: integer = -1): String;
{$ELSE}
function TAdvSmoothMessageDialog.DrawHTMLText(g: TGPGraphics;
  HTML: TAdvSmoothMessageDialogHTMLText; r: TGPRectF; str: String;
  DoAnchor: Boolean = False; Focus: Boolean = False; fX: Integer = -1; fY: integer = -1): String;
{$ENDIF}
var
  htmlr: TRect;
  a, s, k: String;
  l, m, XSize, YSize: integer;
  hr: TRect;
  x, y: single;
begin
  with HTML do
  begin
    if str <> '' then
    begin
      htmlr := Bounds(Round(r.X), Round(r.Y), Round(r.Width), Round(r.Height));

      {$IFDEF USEGDIDRAWING}
      ca.Font.Assign(FFont);
      HTMLDrawEx(ca, str,htmlr,FImages, 0,0,-1,-1,FShadowOffset,False,true,false,false,
        False,False,true,1.0,FURLColor,clNone,clNone,FShadowColor,a,s,k,XSize,YSize,l,m,hr,nil,FGDIContainer,2);
      {$ELSE}
      HTMLDrawGDIP(g, FFont, str,htmlr,FImages, 0,0,-1,-1,FShadowOffset,False,true,false,false,
        False,False,true,1.0,FURLColor,clNone,clNone,FShadowColor,a,s,k,XSize,YSize,l,m,hr,nil,FContainer,2);
      {$ENDIF}

      if FLocation <> hlCustom then
        GetObjectLocation(x, y, r, XSize, YSize, FLocation)
      else
      begin
        x := FLeft;
        y := FTop;
      end;

      htmlr := Bounds(Round(x), Round(y), xsize, ysize);

      {$IFDEF USEGDIDRAWING}
      ca.Font.Assign(FFont);
      HTMLDrawEx(ca, str,htmlr,FImages, fx,fy,-1,-1,FShadowOffset,DoAnchor,false,false,false,
        False,False,true,1.0,FURLColor,clNone,clNone,FShadowColor,a,s,k,XSize,YSize,l,m,hr,nil,FGDIContainer,2);
      {$ELSE}
      HTMLDrawGDIP(g, FFont, str,htmlr,FImages, fx,fy,-1,-1,FShadowOffset,DoAnchor,false,false,false,
        False,False,true,1.0,FURLColor,clNone,clNone,FShadowColor,a,s,k,XSize,YSize,l,m,hr,nil,FContainer,2);
      {$ENDIF}

      if Focus then
        result := k
      else
        result := a;
    end;
  end;
end;

function TAdvSmoothMessageDialog.Execute: Boolean;
var
  res: TModalResult;
begin
  res := ExecuteDialog;
  result := (res = mrOk) or (res = mrYes);
end;

function TAdvSmoothMessageDialog.ExecuteDialog: TModalResult;
begin
  Result := CreateAndShowDialog;
end;

function TAdvSmoothMessageDialog.CreateAndShowDialog: TModalResult;
begin
//  if Assigned(frm) then
//    frm.Free;

  if (Owner is TCustomForm) then
    frm := TAdvSmoothMessageDialogForm.CreateNew(Owner as TCustomForm)
  else
    frm := TAdvSmoothMessageDialogForm.CreateNew(Application.MainForm);

  frm.Dialog := Self;
  frm.Init;
  frm.OnClose := CloseForm;
  frmclosed := false;

  if Modal then
  begin
    frm.OnShow := ShowForm;
    Result := frm.ShowModal;

    if Assigned(OnClose) then
      OnClose(Self);
    //frm.Free;
    frm.Close;
    frm := nil;
  end
  else
  begin
    frm.Show;
    if Assigned(frm) then
      frm.SetLayeredWindow;
    Result := mrOk;
  end;
end;

procedure TAdvSmoothMessageDialog.FillChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothMessageDialog.Fontchanged(Sender: TObject);
begin
  Changed;
end;

function TAdvSmoothMessageDialog.GetAnchorAt(g: TGPGraphics; X, Y: integer; R: TRect; Focus: Boolean = False): String;
{$IFDEF USEGDIDRAWING}
var
  ca: TCanvas;
{$ENDIF}
begin
  with HTMLText do
  begin
    {$IFDEF USEGDIDRAWING}
    ca := TCanvas.Create;
    ca.Handle := g.GetHDC;
    Result := DrawHTMLText(ca, HTMLText, GetHTMLRect(R), HtmlText.Text, True, Focus, X, Y);
    g.ReleaseHDC(ca.Handle);
    ca.Free;
    {$ELSE}
    Result := DrawHTMLText(g, HTMLText, GetHTMLRect(R), HtmlText.Text, True, Focus, X, Y);
    {$ENDIF}
  end;
end;

function TAdvSmoothMessageDialog.GetButtonIndexAtXY(X, Y: integer): integer;
var
  i: integer;
begin
  result := -1;
  for I := 0 to Buttons.Count - 1 do
  begin
    with Buttons[i] do
    begin
      if Buttons[I].Enabled and Buttons[I].Visible then
      begin
        if PtInGPRect(FBtnr, Point(X, Y)) then
        begin
          Result := I;
          break;
        end;
      end;
    end;
  end;
end;

function TAdvSmoothMessageDialog.GetButtonRect(R: TRect): TGPRectF;
var
  totalr, boundsr: TGPRectF;
begin
  totalr := MakeRect(R.Left, R.Top, R.Right - R.Left - 1, R.Bottom - R.Top - 1);
  boundsr := MakeRect(totalr.X + Margin, totalr.Y + Margin, totalr.Width - (Margin * 2), totalr.Height - (Margin * 2));
  result := MakeRect(Boundsr.X, boundsr.Y + FMaxDialog.maxcaptionheight + FMaxDialog.maxhtmlheight + FMaxDialog.maxprogressheight, boundsr.Width
    , Boundsr.Height - FMaxDialog.maxhtmlheight - FMaxDialog.maxcaptionheight - FMaxDialog.maxprogressheight);
end;

function TAdvSmoothMessageDialog.GetCaptionRect(R: TRect): TGPRectF;
var
  totalr, boundsr: TGPRectF;
begin
  totalr := MakeRect(R.Left, R.Top, R.Right - R.Left - 1, R.Bottom - R.Top - 1);
  boundsr := MakeRect(totalr.X + Margin, totalr.Y + Margin, totalr.Width - (Margin * 2), totalr.Height - (Margin * 2));
  result := MakeRect(Boundsr.X, boundsr.Y, boundsr.Width, FMaxDialog.maxcaptionheight);
  result.X := result.X + 5;
  result.Width := result.Width - 5;
end;

function TAdvSmoothMessageDialog.GetComponentStyle: TTMSStyle;
begin
  Result := FTMSStyle;
end;

function TAdvSmoothMessageDialog.GetForm: TAdvSmoothMessageDialogForm;
begin
  if not frmclosed and Assigned(frm) then
    Result := frm
  else
    Result := nil;
end;

function TAdvSmoothMessageDialog.GetHeight: integer;
var
  f: TAdvSmoothMessageDialogForm;
begin
  CalculateMaximum;
  f := TAdvSmoothMessageDialogForm.CreateNew(Self);
  f.Dialog := Self;
  f.Visible := false;
  f.UpdateButtons;
  Result := f.Height;
  f.Free;
end;

function TAdvSmoothMessageDialog.GetHTMLRect(R: TRect): TGPRectF;
var
  totalr, boundsr: TGPRectF;
begin
  totalr := MakeRect(R.Left, R.Top, R.Right - R.Left - 1, R.Bottom - R.Top - 1);
  boundsr := MakeRect(totalr.X + Margin, totalr.Y + Margin, totalr.Width - (Margin * 2), totalr.Height - (Margin * 2));
  result := MakeRect(Boundsr.X, boundsr.Y + FMaxDialog.maxcaptionheight, boundsr.Width, FMaxDialog.maxhtmlheight);
  Result.X := Result.X + 5;
  Result.Width := Result.Width - 10;
end;

function TAdvSmoothMessageDialog.GetProgressBarRect(R: TRect): TGPRectF;
var
  totalr, boundsr: TGPRectF;
begin
  totalr := MakeRect(R.Left, R.Top, R.Right - R.Left - 1, R.Bottom - R.Top - 1);
  boundsr := MakeRect(totalr.X + Margin, totalr.Y + Margin, totalr.Width - (Margin * 2), totalr.Height - (Margin * 2));
  result := MakeRect(Boundsr.X, boundsr.Y + FMaxDialog.maxcaptionheight + FMaxDialog.maxhtmlheight, boundsr.Width, FMaxDialog.maxprogressheight);
end;

function TAdvSmoothMessageDialog.GetStrippedHTMLText: String;
var
  g: TGPGraphics;
  a, s, k: string;
  XSize, YSize: integer;
  l, m: integer;
  hr: TRect;
  bmp: TBitmap;
  {$IFDEF USEGDIDRAWING}
  ca: TCanvas;
  {$ENDIF}
begin
  result := '';
  bmp := TBitmap.Create;
  g := TGPGraphics.Create(bmp.Canvas.Handle);
  with HTMLText do
  begin
    {$IFDEF USEGDIDRAWING}
    ca := TCanvas.Create;
    ca.Handle := g.GetHDC;
    ca.Font.Assign(FFont);
    HTMLDrawEx(ca, Text,Rect(0, 0, 0, 0),FImages, 0,0,-1,-1,FShadowOffset,
      False,true,false,false, False,False,true,1.0,FURLColor,clNone,clNone,FShadowColor,a,s,k,XSize,YSize,l,m,hr,nil,FGDIContainer,2);
    g.ReleaseHDC(ca.Handle);
    ca.Free;
    {$ELSE}
    HTMLDrawGDIP(g, FFont, Text,Rect(0, 0, 0, 0),FImages, 0,0,-1,-1,FShadowOffset,
      False,true,false,false, False,False,true,1.0,FURLColor,clNone,clNone,FShadowColor,a,s,k,XSize,YSize,l,m,hr,nil,FContainer,2);
    {$ENDIF}
    Result := s;
  end;
  g.Free;
  bmp.Free;
end;

function TAdvSmoothMessageDialog.GetThemeID: String;
begin
  Result := ClassName;
end;

function TAdvSmoothMessageDialog.GetVersion: String;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function TAdvSmoothMessageDialog.GetVersionNr: integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

function TAdvSmoothMessageDialog.GetWidth: integer;
var
  f: TAdvSmoothMessageDialogForm;
begin
  CalculateMaximum;
  f := TAdvSmoothMessageDialogForm.CreateNew(Self);
  f.Dialog := Self;
  f.Visible := false;
  f.UpdateButtons;
  Result := f.Width;
  f.Free;
end;

function TAdvSmoothMessageDialog.HasCancel: boolean;
var
  i: integer;
begin
  Result := false;
  for i := 0 to Buttons.Count - 1 do
  begin
    if Buttons[i].ButtonResult = mrCancel then
    begin
      Result := true;
      Break;
    end;
  end;
end;

procedure TAdvSmoothMessageDialog.InitSample;
begin
  Caption := '';
  with Buttons.Add do
  begin
    Caption := 'Ok';
    ButtonResult := mrOk;
  end;
end;

procedure TAdvSmoothMessageDialog.LoadFromTheme(FileName: String);
begin

end;

procedure TAdvSmoothMessageDialog.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  if not (csDestroying in ComponentState) then
  begin
    if (AOperation = opRemove) and (AComponent = FImages) then
      FImages := nil;

    if (AOperation = opRemove) and (AComponent = FContainer) then
      FContainer := nil;

    {$IFDEF USEGDIDRAWING}
    if (AOperation = opRemove) and (AComponent = FGDIContainer) then
      FGDIContainer := nil;
    {$ENDIF}
  end;
  inherited;
end;

procedure TAdvSmoothMessageDialog.Preview;
begin
  FFocusedButton := 0;
  frm := TAdvSmoothMessageDialogForm.CreateNew(Application);
  frm.Dialog := Self;
  frm.Init;
  frm.OnClose := CloseForm;
  frmclosed := false;
  frm.Show;
  if Assigned(frm) then
    frm.SetLayeredWindow;
end;

procedure TAdvSmoothMessageDialog.ProgressChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothMessageDialog.SaveToTheme(FileName: String);
begin

end;

procedure TAdvSmoothMessageDialog.SendModalResult(Result: TModalResult);
var
  canclose: Boolean;
begin
  canclose := true;
  if Assigned(frm) then
  begin
    if (csDesigning in ComponentState) then
    begin
      frm.Close;
      //frm.Free;
      frm := nil;
      Exit;
    end;

    if Modal then
    begin
      if Assigned(OnCanClose) then
        OnCanClose(Self, canclose, result);

      if canclose then
      begin
        frm.OnClose := CloseForm;
        frm.ModalResult := Result;
      end;
    end
    else
    begin
      if Assigned(OnCanClose) then
        OnCanClose(Self, canclose, result);

      if canclose then
      begin
        if Assigned(OnClose) then
          OnClose(Self);
        frm.Close;
        //frm.Free;
        frm := nil;
      end;
    end;
  end;
end;

procedure TAdvSmoothMessageDialog.SendModalResult(Result: TModalResult;
  AButtonIndex: Integer; ACloseDialog: Boolean);
begin
  if ACloseDialog then
    SendModalResult(Result)
  else
  begin
    if Assigned(OnButtonClick) then
      OnButtonClick(Self, AButtonIndex, Result);
  end;
end;

procedure TAdvSmoothMessageDialog.SetAutoClose(const Value: Boolean);
begin
  if FAutoClose <> value then
  begin
    FAutoClose := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMessageDialog.SetButtonAreaFill(const Value: TGDIPFill);
begin
  if FButtonAreaFill <> value then
  begin
    FButtonAreaFill.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothMessageDialog.SetButtonFont(const Value: TFont);
begin
  if FButtonFont <> value then
  begin
    FButtonFont.Assign(Value);
    changed;
  end;
end;

procedure TAdvSmoothMessageDialog.SetButtonLayout(
  const Value: TAdvSmoothMessageDialogButtonLayout);
begin
  if FButtonLayout <> value then
  begin
    FButtonLayout := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMessageDialog.SetButtons(
  const Value: TAdvSmoothMessageDialogButtons);
begin
  if FButtons <> value then
  begin
    FButtons.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothMessageDialog.SetButtonSpacing(const Value: integer);
begin
  if FButtonSpacing <> value then
  begin
    FButtonSpacing := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMessageDialog.SetCaptionFill(const Value: TGDIPFill);
begin
  if FCaptionFill <> value then
  begin
    FCaptionFill.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothMessageDialog.SetCaptionFont(const Value: TFont);
begin
  if FCaptionFont <> value then
  begin
    FCaptionFont.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothMessageDialog.SetCaptionHeight(const Value: integer);
begin
  if FCaptionHeight <> Value then
  begin
    FCaptionHeight := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMessageDialog.SetCaptionLeft(const Value: integer);
begin
  if FCaptionLeft <> value then
  begin
    FCaptionLeft := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMessageDialog.SetCaptionLocation(
  const Value: TAdvSmoothMessageDialogLocation);
begin
  if FCaptionLocation <> Value then
  begin
    FCaptionLocation := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMessageDialog.SetCaptionTop(const Value: integer);
begin
  if FCaptionTop <> Value then
  begin
    FCaptionTop := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMessageDialog.SetColorTones(ATones: TColorTones);
begin
  Fill.RoundingType := rtNone;
  Fill.Color := ATones.Background.BrushColor;
  Fill.ColorTo := ATones.Background.BrushColor;
  Fill.BorderColor := ATones.Background.BorderColor;

  ProgressAppearance.BackGroundFill.Color := ATones.Foreground.BrushColor;
  ProgressAppearance.BackGroundFill.ColorTo := ATones.Foreground.BrushColor;
  ProgressAppearance.BackGroundFill.BorderColor := ATones.Foreground.BorderColor;

  ProgressAppearance.ProgressFill.Color := ATones.Selected.BrushColor;
  ProgressAppearance.ProgressFill.ColorTo := ATones.Selected.BrushColor;
  ProgressAppearance.ProgressFill.ColorMirror := ATones.Selected.BrushColor;
  ProgressAppearance.ProgressFill.ColorMirrorTo := ATones.Selected.BrushColor;
  ProgressAppearance.ProgressFill.BorderColor := ATones.Selected.BorderColor;
end;

procedure TAdvSmoothMessageDialog.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('TMSStyle',ReadTMSStyle, WriteTMSStyle, True);
end;

procedure TAdvSmoothMessageDialog.ReadTMSStyle(Reader: TReader);
begin
  FTMSStyle := TTMSStyle(Reader.ReadInteger);
end;

procedure TAdvSmoothMessageDialog.WriteTMSStyle(Writer: TWriter);
begin
  Writer.WriteInteger(integer(FTMSStyle));
end;

procedure TAdvSmoothMessageDialog.SetComponentStyle(AStyle: TTMSStyle);
var
  i: integer;
  Selected: Boolean;
begin
  FTMSStyle := AStyle;
  Fill.RoundingType := rtBoth;
  Selected := true;
  Captionfont.Color := clBlack;
  // TODO : do color settings here
  case AStyle of
    tsOffice2003Blue:
      begin
        Fill.Color := $00FFD2AF;
        Fill.ColorTo := $00FFD2AF;
      end;
    tsOffice2003Silver:
      begin
        Fill.Color := $00E6D8D8;
        Fill.ColorTo := $00E6D8D8;
      end;
    tsOffice2003Olive:
      begin
        Fill.Color := RGB(225, 234, 185);
        Fill.ColorTo := RGB(225, 234, 185);
      end;
    tsOffice2003Classic:
      begin
        Fill.Color := $00F2F2F2;
        Fill.ColorTo := $00F2F2F2;
      end;
    tsOffice2007Luna:
      begin
        Fill.Color := $00F3E5DA;
        Fill.ColorTo := $00F0DED0;
      end;
    tsOffice2007Obsidian:
      begin
        Fill.Color := $5C534C;
        Fill.ColorTo := $5C534C;
      end;
    tsWindowsXP:
      begin
        Fill.Color := $00B6B6B6;
        Fill.ColorTo := $00B6B6B6;
      end;
    tsWhidbey:
      begin
        Fill.Color := $F5F9FA;
        Fill.ColorTo := $F5F9FA;
      end;
    tsCustom: ;
    tsOffice2007Silver:
      begin
        Fill.Color := RGB(241, 244, 248);
        Fill.ColorTo := RGB(227, 232, 240);
      end;
    tsWindowsVista:
      begin
        Fill.Color := $FDF8F1;
        Fill.ColorTo := $FCEFD5;
        Fill.BorderColor := $FDDE99;
      end;
    tsWindows7:
      begin
        Fill.Color := $FCEBDC;
        Fill.ColorTo := $FCDBC1;
        Fill.BorderColor := $CEA27D;
      end;
      tsTerminal:
      begin
        Fill.Color := clBtnFace;
        Fill.ColorTo := clBtnFace;
        Fill.BorderColor := clGray;
      end;
       tsOffice2010Blue:
      begin
        Fill.Color := $FDF6EF;
        Fill.ColorTo := $F0DAC7;
        Fill.BorderColor := $C7B29F;
      end;
       tsOffice2010Silver:
      begin
        Fill.Color := $FFFFFF;
        Fill.ColorTo := $EDE5E0;
        Fill.BorderColor := $D2CDC8;
      end;
       tsOffice2010Black:
      begin
        Fill.Color := $BFBFBF;
        Fill.ColorTo := $919191;
        Fill.BorderColor := $6D6D6D;
      end;
       tsWindows8, tsWindows10:
      begin
        Fill.Color := $F7F6F5;
        Fill.ColorTo := $F7F6F5;
        Fill.BorderColor := $DCDBDA;
      end;
       tsOffice2013White:
      begin
        Fill.Color := clWhite;
        Fill.ColorTo := clWhite;
        Fill.BorderColor := $D4D4D4;
      end;
       tsOffice2013LightGray:
      begin
        Fill.Color := $FAFAFA;
        Fill.ColorTo := $FAFAFA;
        Fill.BorderColor := $C6C6C6;
      end;
       tsOffice2013Gray:
      begin
        Fill.Color := $F3F3F3;
        Fill.ColorTo := $F3F3F3;
        Fill.BorderColor := $ABABAB;
      end;
       tsOffice2016White:
      begin
        Fill.Color := clWhite;
        Fill.ColorTo := clWhite;
        Fill.BorderColor := $D4D4D4;

      end;
       tsOffice2016Gray:
      begin
        Fill.Color := $B2B2B2;
        Fill.ColorTo := $B2B2B2;
        Fill.BorderColor := $444444;
      end;
       tsOffice2016Black:
      begin
        Fill.Color := $363636;
        Fill.ColorTo := $363636;
        Fill.BorderColor := $444444;
        Captionfont.Color := $FFFFFF;
      end;
  end;

  if AStyle <> tsCustom then
  begin
    Fill.Opacity := 240;
    Fill.OpacityTo := 220;
    Fill.BorderColor := clBlack;
    CaptionFill.Color := clWhite;
    CaptionFill.Opacity := 100;
    CaptionFill.OpacityTo := 0;
    CaptionFill.Rounding := 5;
    CaptionFill.RoundingType := rtTop;
    CaptionFont.Size := 10;
    ButtonFont.Size := 8;

    Fill.Rounding := 5;
    CaptionFill.Rounding := 5;
    CaptionFill.RoundingType := rtTop;
    ButtonAreaFill.Color := clwhite;
    ButtonAreaFill.Colorto := clWhite;
    ButtonAreaFill.Opacity := 0;
    ButtonAreaFill.OpacityTo := 100;
    ButtonAreaFill.Rounding := 5;
    ButtonAreaFill.RoundingType := rtBottom;

    for I := 0 to Buttons.Count - 1 do
    begin
      Buttons[I].Color := Fill.Color;
      Buttons[I].ColorDown := Fill.ColorTo;
      Buttons[I].HoverColor := $F5F9FA;
      Buttons[I].ColorFocused := Lighter(Fill.Color, 10);
      if AStyle = tsWindowsVista then
        Buttons[I].BorderColor := $FDF8F1
      else if Astyle = tsWindows7 then
        Buttons[I].BorderColor := $FCEBDC;
    end;


    with ProgressAppearance do
    begin
      case AStyle of
        tsOffice2003Blue:
        begin
          BackGroundFill.Color := $00FFD2AF;
          BackGroundFill.ColorTo := $00FFD2AF;
          BackGroundFill.BorderColor := $00C0C0C0;

          if not Selected then
          begin
            ProgressFill.Color := $FCE1CB;
            ProgressFill.ColorTo := $E0A57D;
            ProgressFill.ColorMirror := clNone;
            ProgressFill.ColorMirrorTo := clNone;
            ProgressFill.BorderColor := $962D00;
            ProgressFill.GradientMirrorType := gtVertical;
          end
          else
          begin
            ProgressFill.Color := $94E6FB;
            ProgressFill.ColorTo := $1595EE;
            ProgressFill.ColorMirror := clNone;
            ProgressFill.ColorMirrorTo := clNone;
            ProgressFill.BorderColor := $962D00;
            ProgressFill.GradientMirrorType := gtVertical;
          end;
        end;
        tsOffice2003Silver:
        begin
          BackGroundFill.Color := $00E6D8D8;
          BackGroundFill.ColorTo := $00E6D8D8;
          BackGroundFill.BorderColor := $00C0C0C0;

          if not Selected then
          begin
            ProgressFill.Color := $ECE2E1;
            ProgressFill.ColorTo := $B39698;
            ProgressFill.ColorMirror := clNone;
            ProgressFill.ColorMirrorTo := clNone;
            ProgressFill.BorderColor := $947C7C;
            ProgressFill.GradientMirrorType := gtVertical;
          end
          else
          begin
            ProgressFill.Color := $94E6FB;
            ProgressFill.ColorTo := $1595EE;
            ProgressFill.ColorMirror := clNone;
            ProgressFill.ColorMirrorTo := clNone;
            ProgressFill.BorderColor := $947C7C;
            ProgressFill.GradientMirrorType := gtVertical;
          end;
        end;
        tsOffice2003Olive:
        begin
          BackGroundFill.Color := $CFF0EA;
          BackGroundFill.ColorTo := $CFF0EA;
          BackGroundFill.BorderColor := $00C0C0C0;

          if not Selected then
          begin
            ProgressFill.Color := $CFF0EA;
            ProgressFill.ColorTo := $8CC0B1;
            ProgressFill.ColorMirror := clNone;
            ProgressFill.ColorMirrorTo := clNone;
            ProgressFill.BorderColor := $588060;
            ProgressFill.GradientMirrorType := gtVertical;
          end
          else
          begin
            ProgressFill.Color := $94E6FB;
            ProgressFill.ColorTo := $1595EE;
            ProgressFill.ColorMirror := clNone;
            ProgressFill.ColorMirrorTo := clNone;
            ProgressFill.BorderColor := $588060;
            ProgressFill.GradientMirrorType := gtVertical;
          end;
        end;
        tsOffice2003Classic:
        begin
          BackGroundFill.Color := $00F2F2F2;
          BackGroundFill.ColorTo := $00F2F2F2;
          BackGroundFill.BorderColor := $00C0C0C0;

          if not Selected then
          begin
            ProgressFill.Color := clWhite;
            ProgressFill.ColorTo := $C9D1D5;
            ProgressFill.ColorMirror := clNone;
            ProgressFill.ColorMirrorTo := clNone;
            ProgressFill.BorderColor := $808080;
            ProgressFill.GradientMirrorType := gtVertical;
          end
          else
          begin
            ProgressFill.Color := $B59285;
            ProgressFill.ColorTo := $B59285;
            ProgressFill.ColorMirror := clNone;
            ProgressFill.ColorMirrorTo := clNone;
            ProgressFill.BorderColor := $962D00;
            ProgressFill.GradientMirrorType := gtVertical;
          end;
        end;
        tsOffice2007Luna:
        begin
          BackGroundFill.Color := $00FFD2AF;
          BackGroundFill.ColorTo := $00FFD2AF;
          BackGroundFill.BorderColor := $00C0C0C0;

          if not Selected then
          begin
            ProgressFill.Color := $FFEFE3;
            ProgressFill.ColorTo := $FFDDC4;
            ProgressFill.ColorMirror := $FFD1AD;
            ProgressFill.ColorMirrorTo := $FFDBC0;
            ProgressFill.BorderColor := $FFD1AD;
            ProgressFill.GradientMirrorType := gtVertical;
          end
          else
          begin
            ProgressFill.Color := $AAD9FF;
            ProgressFill.ColorTo := $6EBBFF;
            ProgressFill.ColorMirror := $42AEFE;
            ProgressFill.ColorMirrorTo := $7AE1FE;
            ProgressFill.BorderColor := $FFD1AD;//$42AEFE;
            ProgressFill.GradientMirrorType := gtVertical;
          end;
        end;
        tsOffice2007Obsidian:
        begin
          BackGroundFill.Color := $5C534C;
          BackGroundFill.ColorTo := $5C534C;
          BackGroundFill.BorderColor := $00C0C0C0;

          if not Selected then
          begin
            ProgressFill.Color := $F9F8F8;
            ProgressFill.ColorTo := $E4E2DF;
            ProgressFill.ColorMirror := $D1CBC7;
            ProgressFill.ColorMirrorTo := $E2DEDB;
            ProgressFill.BorderColor := clBlack;//$D1CBC7;
            ProgressFill.GradientMirrorType := gtVertical;
          end
          else
          begin
            ProgressFill.Color := $AAD9FF;
            ProgressFill.ColorTo := $6EBBFF;
            ProgressFill.ColorMirror := $42AEFE;
            ProgressFill.ColorMirrorTo := $7AE1FE;
            ProgressFill.BorderColor := clBlack;//$42AEFE;
            ProgressFill.GradientMirrorType := gtVertical;
          end;
        end;
        tsWindowsXP:
        begin
          BackGroundFill.Color := $00B6B6B6;
          BackGroundFill.ColorTo := $00B6B6B6;
          BackGroundFill.BorderColor := $00C0C0C0;

          if not Selected then
          begin
            ProgressFill.Color := clWhite;
            ProgressFill.ColorTo := clBtnFace;
            ProgressFill.ColorMirror := clNone;
            ProgressFill.ColorMirrorTo := clNone;
            ProgressFill.BorderColor := clBlack;
            ProgressFill.GradientMirrorType := gtVertical;
          end
          else
          begin
            ProgressFill.Color := clInActiveCaption;
            ProgressFill.ColorTo := clInActiveCaption;
            ProgressFill.ColorMirror := clNone;
            ProgressFill.ColorMirrorTo := clNone;
            ProgressFill.BorderColor := clBlack;
            ProgressFill.GradientMirrorType := gtVertical;
          end;
        end;
        tsWhidbey:
        begin
          BackGroundFill.Color := $F5F9FA;
          BackGroundFill.ColorTo := $F5F9FA;
          BackGroundFill.BorderColor := $00C0C0C0;

          if not Selected then
          begin
            ProgressFill.Color := $F5F9FA;
            ProgressFill.ColorTo := $A8C0C0;
            ProgressFill.ColorMirror := clNone;
            ProgressFill.ColorMirrorTo := clNone;
            ProgressFill.BorderColor := $962D00;
            ProgressFill.GradientMirrorType := gtVertical;
          end
          else
          begin
            ProgressFill.Color := $94E6FB;
            ProgressFill.ColorTo := $1595EE;
            ProgressFill.ColorMirror := clNone;
            ProgressFill.ColorMirrorTo := clNone;
            ProgressFill.BorderColor := $962D00;
            ProgressFill.GradientMirrorType := gtVertical;
          end;
        end;
        tsCustom: ;
        tsOffice2007Silver:
        begin
          BackGroundFill.Color := $00CAC1BA;
          BackGroundFill.ColorTo := $00CAC1BA;
          BackGroundFill.BorderColor := $00C0C0C0;

          if not Selected then
          begin
            ProgressFill.Color := $FAEEEB;
            ProgressFill.ColorTo := $E5DBD7;
            ProgressFill.ColorMirror := $E2D8D4;
            ProgressFill.ColorMirrorTo := $D1C7C5;
            ProgressFill.BorderColor := clBlack;//$E2D8D4;
            ProgressFill.GradientMirrorType := gtVertical;
          end
          else
          begin
            ProgressFill.Color := $AAD9FF;
            ProgressFill.ColorTo := $6EBBFF;
            ProgressFill.ColorMirror := $42AEFE;
            ProgressFill.ColorMirrorTo := $7AE1FE;
            ProgressFill.BorderColor := clBlack;//$42AEFE;
            ProgressFill.GradientMirrorType := gtVertical;
          end;
        end;
        tsWindowsVista:
        begin
          BackGroundFill.Color := $FDF8F1;
          BackGroundFill.ColorTo := $FDF8F1;
          BackGroundFill.BorderColor := $FDDE99;

          if not Selected then
          begin
            ProgressFill.Color := $FDF8F1;
            ProgressFill.ColorTo := $FCEFD5;
            ProgressFill.ColorMirror := clNone;
            ProgressFill.ColorMirrorTo := clNone;
            ProgressFill.BorderColor := $FDDE99;
            ProgressFill.GradientMirrorType := gtVertical;
          end
          else
          begin
            ProgressFill.Color := $FEF9F0;
            ProgressFill.ColorTo := $FDF0D7;
            ProgressFill.ColorMirror := clNone;
            ProgressFill.ColorMirrorTo := clNone;
            ProgressFill.BorderColor := $FEDF9A;
            ProgressFill.GradientMirrorType := gtVertical;
          end;
        end;
        tsWindows7:
        begin
          BackGroundFill.Color := $FDF8F1;
          BackGroundFill.ColorTo := $FDF8F1;
          BackGroundFill.BorderColor := $FDDE99;

          if not Selected then
          begin
            ProgressFill.Color := $FDFBFA;
            ProgressFill.ColorTo := $FDF3EB;
            ProgressFill.ColorMirror := clNone;
            ProgressFill.ColorMirrorTo := clNone;
            ProgressFill.BorderColor := $FBD6B8;
            ProgressFill.GradientMirrorType := gtVertical;
          end
          else
          begin
            ProgressFill.Color := $FCEBDC;
            ProgressFill.ColorTo := $FCDBC1;
            ProgressFill.ColorMirror := clNone;
            ProgressFill.ColorMirrorTo := clNone;
            ProgressFill.BorderColor := $CEA27D;
            ProgressFill.GradientMirrorType := gtVertical;
          end;
        end;
        tsTerminal:
        begin
          BackGroundFill.Color := clBtnFace;
          BackGroundFill.ColorTo := clBtnFace;
          BackGroundFill.BorderColor := clGray;

          if not Selected then
          begin
            ProgressFill.Color := clSilver;
            ProgressFill.ColorTo := clSilver;
            ProgressFill.ColorMirror := clNone;
            ProgressFill.ColorMirrorTo := clNone;
            ProgressFill.BorderColor := clGray;
          end
          else
          begin
            ProgressFill.Color := clWhite;
            ProgressFill.ColorTo := clWhite;
            ProgressFill.ColorMirror := clNone;
            ProgressFill.ColorMirrorTo := clNone;
            ProgressFill.BorderColor := clGray;
          end;
        end;
        tsOffice2010Blue:
        begin
          BackGroundFill.Color := $FDF6EF;
          BackGroundFill.ColorTo := $F0DAC7;
          BackGroundFill.BorderColor := $C7B29F;

          ProgressFill.Color := $EDDBCD;
          ProgressFill.ColorTo := clNone;
          ProgressFill.ColorMirror := clNone;
          ProgressFill.ColorMirrorTo := clNone;
          ProgressFill.BorderColor := $5B391E;
          ProgressFill.GradientMirrorType := gtVertical;
        end;
        tsOffice2010Silver:
        begin
          BackGroundFill.Color := $FFFFFF;
          BackGroundFill.ColorTo := $EDE5E0;
          BackGroundFill.BorderColor := $D2CDC8;

          ProgressFill.Color := $EDE9E5;
          ProgressFill.ColorTo := clNone;
          ProgressFill.ColorMirror := clNone;
          ProgressFill.ColorMirrorTo := clNone;
          ProgressFill.BorderColor := $7C6D66;
          ProgressFill.GradientMirrorType := gtVertical;
        end;
        tsOffice2010Black:
        begin
          BackGroundFill.Color := $BFBFBF;
          BackGroundFill.ColorTo := $919191;
          BackGroundFill.BorderColor := $D7D7D6;

          ProgressFill.Color := $828282;
          ProgressFill.ColorTo := clNone;
          ProgressFill.ColorMirror := clNone;
          ProgressFill.ColorMirrorTo := clNone;
          ProgressFill.BorderColor := $6D6D6D;
          ProgressFill.GradientMirrorType := gtVertical;
        end;
    tsWindows8, tsWindows10:
      begin
        BackGroundFill.Color := $F7F6F5;
        BackGroundFill.ColorTo := $F7F6F5;
        BackGroundFill.BorderColor := $E4E3E2;

        ProgressFill.Color := $F7E0C9;
        ProgressFill.ColorTo := clNone;
        ProgressFill.ColorMirror := clNone;
        ProgressFill.ColorMirrorTo := clNone;
        ProgressFill.BorderColor := $E4A262;
        ProgressFill.GradientMirrorType := gtVertical;
      end;
    tsOffice2013White:
      begin
        BackGroundFill.Color := clWhite;
        BackGroundFill.ColorTo := clWhite;
        BackGroundFill.BorderColor := $D4D4D4;

        ProgressFill.Color := $FCE2C8;
        ProgressFill.ColorTo := clNone;
        ProgressFill.ColorMirror := clNone;
        ProgressFill.ColorMirrorTo := clNone;
        ProgressFill.BorderColor := $E59D56;
        ProgressFill.GradientMirrorType := gtVertical;
      end;
    tsOffice2013LightGray:
      begin
        BackGroundFill.Color := $F6F6F6;
        BackGroundFill.ColorTo := $F6F6F6;
        BackGroundFill.BorderColor := $C6C6C6;

        ProgressFill.Color := $FCE2C8;
        ProgressFill.ColorTo := clNone;
        ProgressFill.ColorMirror := clNone;
        ProgressFill.ColorMirrorTo := clNone;
        ProgressFill.BorderColor := $E59D56;
        ProgressFill.GradientMirrorType := gtVertical;
      end;

    tsOffice2013Gray:
      begin
        BackGroundFill.Color := $E5E5E5;
        BackGroundFill.ColorTo := $E5E5E5;
        BackGroundFill.BorderColor := $ABABAB;

        ProgressFill.Color := $FCE2C8;
        ProgressFill.ColorTo := clNone;
        ProgressFill.ColorMirror := clNone;
        ProgressFill.ColorMirrorTo := clNone;
        ProgressFill.BorderColor := $E59D56;
        ProgressFill.GradientMirrorType := gtVertical;
      end;
    tsOffice2016White:
      begin
        BackGroundFill.Color := clWhite;
        BackGroundFill.ColorTo := clWhite;
        BackGroundFill.BorderColor := $D4D4D4;

        ProgressFill.Color := $E3BDA3;
        ProgressFill.ColorTo := clNone;
        ProgressFill.ColorMirror := clNone;
        ProgressFill.ColorMirrorTo := clNone;
        ProgressFill.BorderColor := $E3BDA3;
        ProgressFill.GradientMirrorType := gtVertical;
      end;

    tsOffice2016Gray:
      begin
        BackGroundFill.Color := $B2B2B2;
        BackGroundFill.ColorTo := $B2B2B2;
        BackGroundFill.BorderColor := $444444;

        ProgressFill.Color := $E3BDA3;
        ProgressFill.ColorTo := clNone;
        ProgressFill.ColorMirror := clNone;
        ProgressFill.ColorMirrorTo := clNone;
        ProgressFill.BorderColor := $E3BDA3;
        ProgressFill.GradientMirrorType := gtVertical;
      end;

    tsOffice2016Black:
      begin
        BackGroundFill.Color := $363636;
        BackGroundFill.ColorTo := $363636;
        BackGroundFill.BorderColor := $444444;

        ProgressFill.Color := $6A6A6A;
        ProgressFill.ColorTo := clNone;
        ProgressFill.ColorMirror := clNone;
        ProgressFill.ColorMirrorTo := clNone;
        ProgressFill.BorderColor := $6A6A6A;
        ProgressFill.GradientMirrorType := gtVertical;
      end;

      end;
    end;
  end;
end;

procedure TAdvSmoothMessageDialog.SetDefaultButton(const Value: integer);
begin
  if FFocusedButton <> value then
  begin
    FFocusedButton := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMessageDialog.SetEnableKeyEvents(const Value: Boolean);
begin
  if FEnableKeyEvents <> value then
  begin
    FEnableKeyEvents := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMessageDialog.SetFill(const Value: TGDIPFill);
begin
  if FFill <> Value then
  begin
    FFill.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothMessageDialog.SetFormStyle(const Value: TFormStyle);
begin
  if FFormStyle <> Value then
  begin
    FFormStyle := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMessageDialog.SetHTMLText(
  const Value: TAdvSmoothMessageDialogHTMLText);
begin
  if FHTMLText <> Value then
  begin
    FHTMLText.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothMessageDialog.SetMargin(const Value: integer);
begin
  if FMargin <> Value then
  begin
    FMargin := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMessageDialog.SetMaximumHeight(const Value: Integer);
begin
  if FMaximumHeight <> Value then
  begin
    FMaximumHeight := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMessageDialog.SetMaximumWidth(const Value: Integer);
begin
  if FMaximumWidth <> Value then
  begin
    FMaximumWidth := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMessageDialog.SetMinimumHeight(const Value: Integer);
begin
  if FMinimumHeight <> Value then
  begin
    FMinimumHeight := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMessageDialog.SetMinimumWidth(const Value: Integer);
begin
  if FMinimumWidth <> Value then
  begin
    FMinimumWidth := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMessageDialog.SetModal(const Value: Boolean);
begin
  if FModal <> value then
  begin
    FModal := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMessageDialog.SetName(const Value: TComponentName);
begin
  inherited SetName(Value);
  if not (csLoading in ComponentState) then
    Caption := Name;
end;

procedure TAdvSmoothMessageDialog.SetProgressAppearance(
  const Value: TGDIPProgress);
begin
  if FProgressAppearance <> value then
  begin
    FProgressAppearance := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMessageDialog.SetProgressHeight(const Value: integer);
begin
  if FProgressHeight <> Value then
  begin
    FProgressHeight := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMessageDialog.SetProgressLeft(const Value: integer);
begin
  if FProgressLeft <> value then
  begin
    FProgressLeft := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMessageDialog.SetProgressMax(const Value: Double);
begin
  if FProgressMax <> value then
  begin
    FProgressMax := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMessageDialog.SetProgressMin(const Value: Double);
begin
  if FProgressMin <> value then
  begin
    FProgressMin := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMessageDialog.SetProgressPosition(
  const Value: TAdvSmoothMessageDialogLocation);
begin
  if FProgressPosition <> value then
  begin
    FProgressPosition := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMessageDialog.SetProgressTop(const Value: integer);
begin
  if FProgressTop <> value then
  begin
    FProgressTop := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMessageDialog.SetProgressValue(const Value: Double);
begin
  if (FProgressValue <> value) then
  begin
    FProgressValue := Value;
    Changed;
    if Assigned(frm) then
    begin
      if AutoClose and (ProgressValue >= ProgressMaximum) then
      begin
        SendModalResult(mrOk);
      end;
    end;
  end;
end;

procedure TAdvSmoothMessageDialog.SetProgressVisible(const Value: Boolean);
begin
  if FProgressVisible <> value then
  begin
    FProgressVisible := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMessageDialog.SetProgressWidth(const Value: integer);
begin
  if FProgressWidth <> value then
  begin
    FProgressWidth := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMessageDialog.SetTabStop(const Value: Boolean);
begin
  if FTabStop <> Value then
  begin
    FTabStop := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMessageDialog.SetTimeOut(const Value: Integer);
begin
  if FTimeOut <> Value then
  begin
    FTimeOut := Value;
    FTimer.Enabled := FTimeOut > 0;
    FTimer.Interval := FTimeOut;
  end;
end;

procedure TAdvSmoothMessageDialog.SetVersion(const Value: String);
begin

end;

procedure TAdvSmoothMessageDialog.ShowForm(Sender: TObject);
begin
  if Assigned(frm) then
    frm.SetLayeredWindow;

  FTimer.Enabled := TimeOut > 0;
end;

procedure TAdvSmoothMessageDialog.TimerEvent(Sender: TObject);
begin
  SendModalResult(8);
  FTimer.Enabled := False;
end;

{ TAdvSmoothMessageDialogForm }

procedure TAdvSmoothMessageDialogForm.ClearBuffer(graphics: TGPGraphics);
var
  g: TGPGraphics;
begin
  g := graphics;
  if not Assigned(g) then
    g := CreateGraphics;
  g.Clear($00000000);
  if not Assigned(graphics) then
    g.Free;
end;

procedure TAdvSmoothMessageDialogForm.CMDialogChar(var Message: TCMDialogChar);
var
  i: integer;
begin
  if Assigned(Dialog) then
  begin
    for I := 0 to Dialog.Buttons.Count-1 do
    begin
      if IsAccel(Message.CharCode, Dialog.Buttons[i].Caption) then
      begin
        Dialog.SendModalResult(Dialog.Buttons[i].ButtonResult, i, Dialog.Buttons[i].CloseDialog);
        Message.Result := 1;
        Exit;
      end;
    end;
  end;
  inherited;
end;

procedure TAdvSmoothMessageDialogForm.CMDialogKey(var Msg: TWMKey);
begin
  if Assigned(Dialog) and IsWindowVisible(self.Handle) and (msg.charcode = VK_TAB) then
  begin
    if Dialog.FFocusedButton = Dialog.Buttons.Count - 1 then
      Dialog.FFocusedButton := 0
    else
      Inc(Dialog.FFocusedButton);

    Dialog.Changed;
  end;
end;

procedure TAdvSmoothMessageDialogForm.CMHintShow(var Message: TMessage);
var
  g: TGPGraphics;
begin
  if not Assigned(Dialog) then
    Exit;
  with TCMHintShow(Message).HintInfo^ do
  begin
    g := CreateGraphics;
    HintStr := Dialog.GetAnchorAt(g, CursorPos.X, CursorPos.Y, ClientRect, True);
    g.Free;
    ReshowTimeout := 0;
  end;
end;

procedure TAdvSmoothMessageDialogForm.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

function TAdvSmoothMessageDialogForm.CreateGraphics: TGPGraphics;
begin
  Result := nil;
  if Assigned(FMainBuffer) then
    Result := TGPGraphics.Create(FMainBuffer);
end;

procedure TAdvSmoothMessageDialogForm.CreateMainBuffer;
begin
  if Assigned(FMainBuffer) then
    FMainBuffer.Free;

  FMainBuffer := TGPBitmap.Create(Width, Height, PixelFormat32bppARGB);
end;

constructor TAdvSmoothMessageDialogForm.CreateNew(AOwner: TComponent;
  Dummy: Integer);
begin
  inherited;
end;

procedure TAdvSmoothMessageDialogForm.CreateWnd;
begin
  inherited;
  if Assigned(Dialog) then
  begin
    Dialog.CalculateMaximum;
    Width := Round(Dialog.FMaxDialog.totalmaxwidth);
    Height := Round(Dialog.FMaxDialog.totalmaxheight + Dialog.Margin * 2);
    UpdateButtons;
    UpdateWindow;
  end;
end;

procedure TAdvSmoothMessageDialogForm.DestroyMainBuffer;
begin
  if Assigned(FMainBuffer) then
    FMainBuffer.Free;
end;

procedure TAdvSmoothMessageDialogForm.DoCreate;
begin
  inherited;
  FMainBuffer := nil;
end;

procedure TAdvSmoothMessageDialogForm.DoDestroy;
begin
  inherited;
  DestroyMainBuffer;
end;

procedure TAdvSmoothMessageDialogForm.Draw(graphics: TGPGraphics);
var
  R: TRect;
  sri, captionr, htmlr, totalr: TGPRectF;
  boundsr, buttonr: TGPRectF;
  g: TGPGraphics;
  ff: TGPFontFamily;
  fs: integer;
  sf: TGPStringFormat;
  x, y: single;
  f: TGPFont;
  b: TGPSolidBrush;
  pt: TGPPointF;
  i: integer;
  pr: TRect;
begin
  g := graphics;
  if not Assigned(g) then
    g := CreateGraphics;

  g.SetSmoothingMode(SmoothingModeAntiAlias);
  g.SetTextRenderingHint(TextRenderingHintAntiAlias);

  if Assigned(Dialog) then
  begin
    with Dialog do
    begin
      R := ClientRect;
      totalr := MakeRect(R.Left, R.Top, R.Right - R.Left - 1, R.Bottom - R.Top - 1);
      boundsr := MakeRect(totalr.X + Margin, totalr.Y + Margin, totalr.Width - (Margin * 2), totalr.Height - (Margin * 2));

      captionr := GetCaptionRect(R);
      captionr.X := captionr.X - 5;
      captionr.Width := captionr.Width + 5;
      htmlr := GetHTMLRect(R);
      buttonr := GetButtonRect(R);

      //background
      FFill.Fill(g, boundsr);

      if (buttonr.Width <> 0) and (buttonr.Height <> 0) and (Buttons.Count > 0) then
      begin
        //Background button area
        FButtonAreaFill.Fill(g, buttonr);

        //buttons
        for I := 0 to Buttons.Count - 1 do
        begin
          DrawButton(g, Buttons[i], Buttons[I].FBtnr);
        end;
      end;

      //caption
      if captionr.Height <> 0 then
      begin
        FCaptionFill.Fill(g, captionr);
        if FCaption <> '' then
        begin
          ff := TGPFontFamily.Create(CaptionFont.Name);
          if (ff.Status in [FontFamilyNotFound, FontStyleNotFound]) then
          begin
            ff.Free;
            ff := TGPFontFamily.Create('Arial');
          end;

          fs := 0;
          if (fsBold in CaptionFont.Style) then
            fs := fs + 1;
          if (fsItalic in CaptionFont.Style) then
            fs := fs + 2;
          if (fsUnderline in CaptionFont.Style) then
            fs := fs + 4;

          sf := TGPStringFormat.Create;
          f := TGPFont.Create(ff, CaptionFont.Size, fs, UnitPoint);
          b := TGPSolidBrush.Create(ColorToARGB(CaptionFont.Color));
          g.MeasureString(Caption, length(Caption), f, captionr, sf, sri);

          if CaptionLocation <> hlCustom then
            GetObjectLocation(x, y, captionr, Round(sri.Width), Round(sri.Height), CaptionLocation)
          else
          begin
            x := CaptionLeft;
            y := CaptionTop;
          end;

          pt := MakePoint(x, y);
          FCaptionRect := MakeRect(x, y, sri.Width, sri.Height);
          {$IFNDEF USEGDIDRAWING}
          g.DrawString(Caption, Length(Caption), f, MakeRect(x, y, sri.Width, sri.Height), sf, b);
          {$ENDIF}

          b.Free;
          sf.Free;
          f.Free;
          ff.Free;
        end;
      end;
      //html
      if htmlr.Height <> 0 then
      begin
        FHTMLRect := htmlr;
        {$IFNDEF USEGDIDRAWING}
          DrawHTMLText(g, HTMLText, htmlr, HTMLText.Text);
        {$ENDIF}
      end;

      with ProgressAppearance do
      begin
        if ProgressVisible then
        begin
          if ProgressPosition <> hlCustom then
            GetObjectLocation(x, y, GetProgressBarRect(R),
              ProgressWidth, ProgressHeight, ProgressPosition)
          else
          begin
            x := ProgressLeft;
            y := ProgressTop;
          end;

          pr := Bounds(R.Left + Round(x), R.Top + Round(y), ProgressWidth, ProgressHeight);
          Draw(g, pr, ProgressMinimum, ProgressMaximum, ProgressValue, pbdHorizontal);
        end;
      end;
    end;
  end;

  if not Assigned(graphics) then
    g.Free;
end;

procedure TAdvSmoothMessageDialogForm.Init;
begin
  Visible := False;
  BorderIcons := [];
  BorderStyle := bsNone;
  ShowHint := True;
  Ctl3D := false;
  Color := clWhite;
  FormStyle := Dialog.FormStyle;
  Position := Dialog.Position;
  Left := Dialog.DialogLeft;
  Top := Dialog.DialogTop;
  CreateMainBuffer;
end;

procedure TAdvSmoothMessageDialogForm.CopyToClipBoard(Text: String);
begin
  {$IFDEF DELPHI_UNICODE}
  Clipboard.AsText := Text;
  {$ENDIF}

  {$IFNDEF DELPHI_UNICODE}
  ClipBoard.SetTextBuf(PChar(Text));
  {$ENDIF}
end;

procedure TAdvSmoothMessageDialogForm.KeyDown(var Key: Word;
  Shift: TShiftState);
var
  s: String;
begin
  inherited;
  FKeyDown := true;
  if Assigned(Dialog) then
  begin
    if Dialog.EnableKeyEvents then
    begin
      if (Key = Ord('C')) and (ssCtrl in Shift) then
      begin
        s := Dialog.GetStrippedHTMLText;
        if s <> '' then
          CopyToClipBoard(s)
        else if Dialog.Caption <> '' then
          CopyToClipBoard(Dialog.Caption);
      end
      else if (key = VK_ESCAPE) then
      begin
        if Dialog.HasCancel then
          Dialog.SendModalResult(mrCancel)
      end
      else if Key in [VK_LEFT, VK_UP] then
      begin
        if Dialog.FFocusedButton = 0 then
          Dialog.FFocusedButton := Dialog.Buttons.Count - 1
        else
          Dec(Dialog.FFocusedButton);

        Dialog.Changed;
      end
      else if Key in [VK_RIGHT, VK_DOWN] then
      begin
        if Dialog.FFocusedButton = Dialog.Buttons.Count - 1 then
          Dialog.FFocusedButton := 0
        else
          Inc(Dialog.FFocusedButton);

        Dialog.Changed;
      end
      else if (Key = VK_SPACE) or (Key = VK_RETURN) then
      begin
        if (Dialog.FFocusedButton >= 0) and (Dialog.FFocusedButton <= Dialog.Buttons.Count - 1) then
        begin
          if Dialog.Buttons[Dialog.FFocusedButton].Enabled and Dialog.Buttons[Dialog.FFocusedButton].Visible then
          begin
            Dialog.Buttons[Dialog.FFocusedButton].FDown := true;
            Dialog.Changed;
          end;
        end;
      end;
    end;
  end;
end;

procedure TAdvSmoothMessageDialogForm.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if FKeyDown then
  begin
    if Assigned(Dialog) then
    begin
      if (Key = VK_SPACE) or (Key = VK_RETURN) and Dialog.EnableKeyEvents then
      begin
        if (Dialog.FFocusedButton >= 0) and (Dialog.FFocusedButton <= Dialog.Buttons.Count - 1) then
        begin
          if Dialog.Buttons[Dialog.FFocusedButton].Enabled and Dialog.Buttons[Dialog.FFocusedButton].Visible then
          begin
            Dialog.Buttons[Dialog.FFocusedButton].FDown := false;
            Dialog.Changed;
            Dialog.SendModalResult(Dialog.Buttons[Dialog.FFocusedButton].ButtonResult, Dialog.FFocusedButton, Dialog.Buttons[Dialog.FFocusedButton].CloseDialog);
          end;
        end;
      end;
    end;
  end;
end;

procedure TAdvSmoothMessageDialogForm.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  btnidx: integer;
  anchor: String;
  g: TGPGraphics;
begin
  inherited;
  SetFocus;
  if Assigned(Dialog) then
  begin
    if Assigned(Dialog.OnAnchorClick) then
    begin
      g := CreateGraphics;
      anchor := Dialog.GetAnchorAt(g, X, Y, ClientRect);
      if anchor <> '' then
        Dialog.OnAnchorClick(Self, anchor);

      g.Free;
    end;

    btnidx := Dialog.GetButtonIndexAtXY(X, Y);
    if btnidx <> -1 then
    begin
      FMouseDownOnButton := true;
      FButtonidx := btnidx;
      Dialog.Buttons[btnidx].FDown := true;
      Dialog.Changed(True);
    end;
  end;
end;

procedure TAdvSmoothMessageDialogForm.MouseMove(Shift: TShiftState; X,
  Y: Integer);
var
  i, btnidx: integer;
  g: TGPGraphics;
begin
  inherited;

  if FMouseDownOnButton then
    Exit;

  if Assigned(Dialog) then
  begin
    g := CreateGraphics;
    if Dialog.GetAnchorAt(g, X, Y, ClientRect) <> '' then
      Screen.Cursor := crHandPoint
    else
      Screen.Cursor := crDefault;
    g.Free;

    btnidx := Dialog.GetButtonIndexAtXY(X, Y);
    for I := 0 to Dialog.Buttons.Count - 1 do
    begin
      if btnidx <> I then
      begin
        if Dialog.Buttons[I].FHover then
        begin
          Dialog.Buttons[I].FHover := false;
          Dialog.Changed(True);
        end;
      end;
    end;
    if (btnidx <> -1) then
    begin
      if not Dialog.Buttons[btnidx].FHover then
      begin
        Dialog.Buttons[btnidx].FHover := true;
        Dialog.Changed(True);
      end;
    end;
  end;
end;

procedure TAdvSmoothMessageDialogForm.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  i: integer;
  btnidx: integer;
begin
  inherited;
  FMouseDownOnButton := false;
  if Assigned(Dialog) then
  begin
    btnidx := Dialog.GetButtonIndexAtXY(X, Y);
    if (btnidx <> -1) and (btnidx = FButtonidx) then
    begin
      Dialog.SendModalResult(Dialog.Buttons[btnidx].ButtonResult, btnidx, Dialog.Buttons[btnidx].CloseDialog);
    end;

    for I := 0 to Dialog.Buttons.Count - 1 do
    begin
      Dialog.Buttons[I].FDown := false;
      Dialog.Buttons[I].FHover := false;
    end;

    Dialog.Changed(True);
  end;
end;

procedure TAdvSmoothMessageDialogForm.Paint;
{$IFDEF USEGDIDRAWING}
var
  g: TGPGraphics;
  I: Integer;
  cr: TRect;
  btn: TAdvSmoothMessageDialogButton;
  w, h: Integer;
{$ENDIF}
begin
  inherited;
  {$IFDEF USEGDIDRAWING}
  if not Assigned(Dialog) then
    Exit;

  g := TGPGraphics.Create(Canvas.Handle);
  if Assigned(FMainBuffer) then
    g.DrawImage(FMainBuffer, 0, 0, FMainBuffer.Width, FMainBuffer.Height);
  g.Free;

  Canvas.Brush.Style := bsClear;

  if FCaptionRect.Height <> 0 then
  begin
    Canvas.Font.Assign(Dialog.CaptionFont);
    cr := Bounds(Round(FCaptionRect.X), Round(FCaptionRect.Y), Round(FCaptionRect.Width), Round(FCaptionRect.Height));
    DrawText(Canvas.Handle, PChar(Dialog.Caption), Length(Dialog.Caption), cr, DT_CENTER or DT_VCENTER or DT_SINGLELINE);
  end;

  for I := 0 to Dialog.Buttons.Count - 1 do
  begin
    btn := Dialog.Buttons[I];
    Canvas.Font.Assign(Dialog.ButtonFont);
    w := Canvas.TextWidth(btn.Caption);
    h := Canvas.TextWidth(btn.Caption);
    cr := Bounds(Round(btn.FCapPos.X), Round(btn.FCapPos.Y), Round(btn.FCapPos.X + W + 10), Round(btn.FCapRect.Y + h));
    DrawText(Canvas.Handle, PChar(btn.Caption), Length(btn.Caption), cr, DT_LEFT or DT_TOP or DT_SINGLELINE);
  end;

  if FHTMLRect.Height <> 0 then
  begin
    Canvas.Font.Assign(Dialog.HTMLText.Font);
    Dialog.DrawHTMLText(Canvas, Dialog.HTMLText, FHTMLRect, Dialog.HTMLText.Text);
  end;
  {$ENDIF}
end;

procedure TAdvSmoothMessageDialogForm.SetLayeredWindow;
begin
  {$IFNDEF USEGDIDRAWING}
  if (GetWindowLong(Handle, GWL_EXSTYLE) and WS_EX_LAYERED = 0) then
    SetWindowLong(Handle, GWL_EXSTYLE, GetWindowLong(Handle, GWL_EXSTYLE) or WS_EX_LAYERED);
  {$ENDIF}

  UpdateLayered;
end;

procedure TAdvSmoothMessageDialogForm.UpdateButtons;
var
  buttonr: TGPRectF;
  i: integer;
  smaxw, smaxh, bwidth, bheight: Double;
begin
  if Assigned(Dialog) then
  begin
    with Dialog do
    begin
      buttonr := GetButtonRect(ClientRect);
      smaxw := 0;
      smaxh := 0;

      case ButtonLayout of
        blhorizontal:
        begin
          smaxw :=  (FMaxDialog.maxbuttonwidth * Buttons.Count) + ButtonSpacing;
          smaxh :=  FMaxDialog.maxbuttonheight;
        end;
        blVertical:
        begin
          smaxw :=  FMaxDialog.maxbuttonwidth + ButtonSpacing;
          smaxh :=  FMaxDialog.maxbuttonheight * Buttons.Count;
        end;

      end;

      if smaxw > buttonr.Width then
        Self.Width := Round(smaxw + Margin * 2);

      if smaxh > buttonr.Height then
        Self.Height := Round(GetCaptionRect(ClientRect).Height + GetHTMLRect(ClientRect).Height + GetProgressBarRect(ClientRect).Height + smaxh + Margin * 2 + ButtonSpacing);

      bwidth := FMaxDialog.maxbuttonwidth - ButtonSpacing;
      bheight := FMaxDialog.maxbuttonheight - ButtonSpacing;

      buttonr := GetButtonRect(ClientRect);

      case ButtonLayout of
        blHorizontal:
        begin
          if smaxw < buttonr.Width then
            buttonr.X := buttonr.X + Max(0, (buttonr.Width - smaxw) / 2);

          for I := 0 to Buttons.Count - 1 do
            Buttons[I].FBtnr := MakeRect(buttonr.X + (I * bwidth) + ((I + 1) * ButtonSpacing),
              buttonr.Y + (buttonr.Height - bheight) / 2, bwidth, bheight)
        end;
        blVertical:
        begin
          for I := 0 to Buttons.Count - 1 do
            Buttons[I].FBtnr := MakeRect(buttonr.X + (buttonr.Width - bwidth) / 2,
              buttonr.Y + (I * bheight) + ((I + 1) * ButtonSpacing), bwidth, bheight);
        end;
      end;
    end;
  end;
end;

procedure TAdvSmoothMessageDialogForm.UpdateLayered;
begin
  ClearBuffer(nil);

  self.Activate;
  SetWindowPos(Self.Handle, HWND_TOP, 0, 0, 0, 0,
    SWP_NOMOVE or SWP_NOSIZE or SWP_FRAMECHANGED);

  Draw(nil);

  UpdateMainWindow;
end;

procedure TAdvSmoothMessageDialogForm.UpdateMainWindow;
var
  ScrDC, MemDC: HDC;
  BitmapHandle, PrevBitmap: HBITMAP;
  BlendFunc: _BLENDFUNCTION;
  Size: TSize;
  P, S: TPoint;
begin
  {$IFDEF USEGDIDRAWING}
  Exit;
  {$ENDIF}
  ScrDC := CreateCompatibleDC(0);
  MemDC := CreateCompatibleDC(ScrDC);

  FMainBuffer.GetHBITMAP(0, BitmapHandle);
  PrevBitmap := SelectObject(MemDC, BitmapHandle);
  Size.cx := Width;
  Size.cy := Height;
  P := Point(Left, Top);
  S := Point(0, 0);

  with BlendFunc do
  begin
    BlendOp := AC_SRC_OVER;
    BlendFlags := 0;
    SourceConstantAlpha := 255;
    AlphaFormat := AC_SRC_ALPHA;
  end;

  UpdateLayeredWindow(Handle, ScrDC, @P, @Size, MemDC, @S, 0, @BlendFunc, ULW_ALPHA);

  SelectObject(MemDC, PrevBitmap);
  DeleteObject(BitmapHandle);

  DeleteDC(MemDC);
  DeleteDC(ScrDC);
end;

procedure TAdvSmoothMessageDialogForm.UpdateWindow;
begin
  CreateMainBuffer;
  UpdateLayered;
end;

procedure TAdvSmoothMessageDialogForm.WMActivate(var Message: TWMActivate);
begin
  inherited;
  if Assigned(Dialog) then
  begin
    if Dialog.frm = nil then
      Dialog.frm := Self;
  end;
end;

procedure TAdvSmoothMessageDialogForm.WMEraseBkGnd(var Message: TWMEraseBkGnd);
begin
  inherited;
end;

//procedure TAdvSmoothMessageDialogForm.WMMouseActivate(
//  var Msg: TWMMouseActivate);
//begin
//  msg.result := MA_NOACTIVATE;
//end;

procedure TAdvSmoothMessageDialogForm.WMNCHitTest(var Msg: TWMNCHitTest);
var
  pt: TPoint;
  r: TGPRectF;
begin
  inherited;

  if Assigned(Dialog) then
  begin
    with Dialog do
    begin
      pt := ScreenToClient(Point(msg.XPos, msg.YPos));
      r := GetCaptionRect(ClientRect);
      if pt.Y < r.Y + r.Height then
        Msg.Result := HTCAPTION;
    end;
  end;
end;

procedure TAdvSmoothMessageDialogForm.WMPaint(var Message: TWMPaint);
begin
  inherited;
end;

{ TAdvSmoothMessageDialogHTMLText }

procedure TAdvSmoothMessageDialogHTMLText.Assign(Source: TPersistent);
begin
  if (Source is TAdvSmoothMessageDialogHTMLText) then
  begin
    FURLColor := (Source as TAdvSmoothMessageDialogHTMLText).URLColor;
    FShadowOffset := (Source as TAdvSmoothMessageDialogHTMLText).ShadowOffset;
    FShadowColor := (Source as TAdvSmoothMessageDialogHTMLText).ShadowColor;
    FFont.Assign((Source as TAdvSmoothMessageDialogHTMLText).Font);
    FText := (Source as TAdvSmoothMessageDialogHTMLText).Text;
    FLocation := (Source as TAdvSmoothMessageDialogHTMLText).Location;
    FTop := (Source as TAdvSmoothMessageDialogHTMLText).Top;
    FLeft := (Source as TAdvSmoothMessageDialogHTMLText).Left;
    Changed;
  end;
end;

procedure TAdvSmoothMessageDialogHTMLText.Changed;
begin
  FOwner.Changed;
end;

constructor TAdvSmoothMessageDialogHTMLText.Create(
  AOwner: TAdvSmoothMessageDialog);
begin
  FOwner := AOwner;
  FURLColor := clBlue;
  FShadowOffset := 5;
  FShadowColor := clGray;
  FLocation := hlCenterLeft;
  FFont := TFont.Create;
  {$IFNDEF DELPHI9_LVL}
  FFont.Name := 'Tahoma';
  {$ENDIF}
  FTop := 0;
  FLeft := 0;
end;

destructor TAdvSmoothMessageDialogHTMLText.Destroy;
begin
  FFont.Free;
  inherited;
end;

procedure TAdvSmoothMessageDialogHTMLText.SetFont(const Value: TFont);
begin
  if FFont <> Value then
  begin
    FFont.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothMessageDialogHTMLText.SetLeft(const Value: integer);
begin
  if FLeft <> Value then
  begin
    FLeft := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMessageDialogHTMLText.SetLocation(
  const Value: TAdvSmoothMessageDialogLocation);
begin
  if FLocation <> value then
  begin
    FLocation := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMessageDialogHTMLText.SetShadowColor(const Value: TColor);
begin
  if FShadowColor <> value then
  begin
    FShadowColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMessageDialogHTMLText.SetShadowOffset(const Value: integer);
begin
  if FShadowOffset <> value then
  begin
    FShadowOffset := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMessageDialogHTMLText.SetText(const Value: string);
begin
  if FText <> value then
  begin
    FText := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMessageDialogHTMLText.SetTop(const Value: integer);
begin
  if FTop <> Value then
  begin
    FTop := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMessageDialogHTMLText.SetURLColor(const Value: TColor);
begin
  if FURLColor <> Value then
  begin
    FURLColor := Value;
    Changed;
  end;
end;

{ TAdvSmoothMessageDialogButton }

procedure TAdvSmoothMessageDialogButton.Assign(Source: TPersistent);
begin
  if (Source is TAdvSmoothMessageDialogButton) then
  begin
    FOpacity := (Source as TAdvSmoothMessageDialogButton).Opacity;
    FPicture.Assign((Source as TAdvSmoothMessageDialogButton).Picture);
    FColor := (Source as TAdvSmoothMessageDialogButton).Color;
    FColorFocused := (Source as TAdvSmoothMessageDialogButton).ColorFocused;
    FCaption := (Source as TAdvSmoothMessageDialogButton).Caption;
    FSpacing := (Source as TAdvSmoothMessageDialogButton).Spacing;
    FPictureLocation := (Source as TAdvSmoothMessageDialogButton).PictureLocation;
    FButtonResult := (Source as TAdvSmoothMessageDialogButton).ButtonResult;
    FColorDown := (Source as TAdvSmoothMessageDialogButton).ColorDown;
    FBorderColor := (Source as TAdvSmoothMessageDialogButton).BorderColor;
    FBorderWidth := (Source as TAdvSmoothMessageDialogButton).BorderWidth;
    FBorderOpacity := (Source as TAdvSmoothMessageDialogButton).BorderOpacity;
    FHoverColor := (Source as TAdvSmoothMessageDialogButton).HoverColor;
    FEnabled := (Source as TAdvSmoothMessageDialogButton).Enabled;
    FColorDisabled := (Source as TAdvSmoothMessageDialogButton).ColorDisabled;
    FCloseDialog := (Source as TAdvSmoothMessageDialogButton).CloseDialog;
    FVisible := (Source as TAdvSmoothMessageDialogButton).Visible;
    Changed;
  end;
end;

procedure TAdvSmoothMessageDialogButton.ButtonChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothMessageDialogButton.Changed;
begin
  FOwner.Changed;
end;

constructor TAdvSmoothMessageDialogButton.Create(Collection: TCollection);
begin
  inherited;
  Fowner := (Collection as TAdvSmoothMessageDialogButtons).FOwner;
  FOpacity := 255;
  FCloseDialog := True;
  FPicture := TAdvGDIPPicture.Create;
  FPicture.OnChange := PictureChanged;
  FColor := clGray;
  FButtonResult  := mrOk;
  FPictureLocation := blPictureLeft;
  FVisible := True;
  FColorDown := clDkGray;
  FBorderColor := clBlack;
  FHoverColor := clLtGray;
  FBorderWidth := 1;
  FBorderOpacity := 255;
  FEnabled := true;
  FColorDisabled := $797979;
  FColorFocused := clSilver;
end;

destructor TAdvSmoothMessageDialogButton.Destroy;
begin
  FPicture.Free;
  inherited;
end;

procedure TAdvSmoothMessageDialogButton.PictureChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothMessageDialogButton.SetCaption(const Value: string);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMessageDialogButton.SetCloseDialog(const Value: Boolean);
begin
  if FCloseDialog <> Value then
  begin
    FCloseDialog := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMessageDialogButton.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMessageDialogButton.SetColorDisabled(const Value: TColor);
begin
  if FColorDisabled <> value then
  begin
    FColorDisabled := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMessageDialogButton.SetColorDown(const Value: TColor);
begin
  if FColorDown <> value then
  begin
    FColorDown := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMessageDialogButton.SetColorFocused(const Value: TColor);
begin
  if FColorFocused <> value then
  begin
    FColorFocused := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMessageDialogButton.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> value then
  begin
    FEnabled := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMessageDialogButton.SetHoverColor(const Value: TColor);
begin
  if FHoverColor <> value then
  begin
    FHoverColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMessageDialogButton.SetOpacity(const Value: Byte);
begin
  if FOpacity <> value then
  begin
    FOpacity := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMessageDialogButton.SetPicture(
  const Value: TAdvGDIPPicture);
begin
  if FPicture <> value then
  begin
    FPicture.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothMessageDialogButton.SetPictureLocation(
  const Value: TGDIPButtonLayout);
begin
  if FPictureLocation <> value then
  begin
    FPictureLocation := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMessageDialogButton.SetSpacing(const Value: integer);
begin
  if FSpacing <> Value then
  begin
    FSpacing := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMessageDialogButton.SetVisible(const Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMessageDialogButton.SetBorderColor(const Value: TColor);
begin
  if FBorderColor <> value then
  begin
    FBorderColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMessageDialogButton.SetBorderOpacity(const Value: Byte);
begin
  if FBorderOpacity <> Value then
  begin
    FBorderOpacity := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMessageDialogButton.SetBorderWidth(const Value: integer);
begin
  if FBorderWidth <> value then
  begin
    FBorderWidth := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMessageDialogButton.SetButtonResult(const Value: TModalResult);
begin
  if FButtonResult <> Value then
  begin
    FButtonResult := Value;
    Changed;
  end;
end;

{ TAdvSmoothMessageDialogButtons }

function TAdvSmoothMessageDialogButtons.Add: TAdvSmoothMessageDialogButton;
begin
  Result := TAdvSmoothMessageDialogButton(inherited Add);
end;

constructor TAdvSmoothMessageDialogButtons.Create(
  AOwner: TAdvSmoothMessageDialog);
begin
  inherited Create(TAdvSmoothMessageDialogButton);
  FOwner := AOwner;
end;

procedure TAdvSmoothMessageDialogButtons.Delete(Index: Integer);
begin
  Items[Index].Free;
end;

function TAdvSmoothMessageDialogButtons.GetItem(
  Index: Integer): TAdvSmoothMessageDialogButton;
begin
  Result := TAdvSmoothMessageDialogButton(inherited Items[Index]);
end;

function TAdvSmoothMessageDialogButtons.GetOwner: TPersistent;
begin
  result := FOwner;
end;

function TAdvSmoothMessageDialogButtons.Insert(
  Index: Integer): TAdvSmoothMessageDialogButton;
begin
  Result := TAdvSmoothMessageDialogButton(inherited Insert(Index));
end;

procedure TAdvSmoothMessageDialogButtons.SetItem(Index: Integer;
  const Value: TAdvSmoothMessageDialogButton);
begin
  inherited Items[Index] := Value;
end;

{ TMargins }

end.
