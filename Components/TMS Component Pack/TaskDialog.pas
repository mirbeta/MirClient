{***************************************************************************}
{ TTaskDialog component                                                     }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2012 - 2015                                        }
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

unit TaskDialog;

{$R TASKDIALOG.RES}

{$I TMSDEFS.INC}

interface

uses
  Classes, Windows, Messages, Forms, Dialogs, SysUtils, StdCtrls, Graphics, Consts, Math,
  ExtCtrls, Controls, ComCtrls, PictureContainer, ComObj, ShellAPI, CommCtrl, ClipBrd, ImgList, Types
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 8; // Minor version nr.
  REL_VER = 8; // Release nr.
  BLD_VER = 0; // Build nr.

  // version history
  // 1.0.0.0 : First release
  // 1.0.1.0 : Added support for Information icon
  //         : Fixed issue with radiobutton initialization
  // 1.0.2.0 : Various cosmetic fixes for emulated dialog
  //         : Design time preview
  // 1.0.3.0 : Improved wordwrapped content display
  // 1.0.4.0 : Added support to display shield icon on non Vista operating systems
  // 1.0.5.0 : Fixed issue with tiError icon for non Vista operating systems
  // 1.0.5.1 : Fixed issue with tiBlank icon for non Vista operating systems
  // 1.0.5.2 : Removed Close button from dialog caption for non Vista operating systems
  // 1.0.5.3 : Fixed issue with blank FooterIcon
  //         : Fixed issue with content height
  // 1.0.5.4 : Improved : content sizing for non Vista operating systems dialogs
  // 1.0.5.5 : Fixed issue with progress bar for non Vista operating systems dialogs
  // 1.0.5.6 : Fixed issue with Expanded Text size calculation for non Vista operating systems dialogs
  // 1.0.5.7 : Fixed issue with default button for non Vista operating systems dialogs
  // 1.0.5.8 : Fixed issue with Expanded Text size calculation for non Vista operating systems dialogs
  //         : Fixed issue with FooterIcon drawing
  // 1.0.6.0 : New : property DialogPosition added , only applicable for non Vista OS
  //         : Fixed : issue with ESC key handling
  // 1.1.0.0 : Improved : Reflect properties change at run time
  //         : Fixed issues with Footer and its FooterIcon size
  //         : Added ShortCut support in CommandLinks
  // 1.2.0.0 : New : support added for Hyperlinks in expanded text
  //         : New : option to show no default radiobutton added
  //         : New : capability to update instruction, content, expanded text, footer while dialog is displayed
  //         : New : option to allow cancelling the dialog with ESC added
  //         : Improved : text wrapping for verify text
  //         : New : TAdvTaskDialogEx component created using TAdvGlowButton on non Vista emulation
  //         : New : property ApplicationIsParent added
  //         : New : support for custom icons
  // 1.2.1.0 : New : support for Information & Shield footer icon
  //         : Improved : border drawing on Vista in XP compatibility mode
  //         : New : added support for \n linebreaks in Vista emulation mode
  // 1.2.1.1 : Fixed : issue with DefaultRadioButton initialization
  // 1.2.1.2 : Fixed : issue with \n linebreaks with doHyperlinks style
  // 1.2.2.0 : Improved : keyboard handling for CommandLinks dialog on non Vista emulation
  //         : Improved : DefaultButton handling for CommandLinks dialog on non Vista emulation
  // 1.2.2.1 : Fixed : issue with noCommandLinksIcon on non Vista emulation
  // 1.2.2.2 : Fixed : hot painting issue on taskdialog button on non Vista emulation
  // 1.2.3.0 : Improved : allow using \n line separators in footer text on non Vista emulation
  //         : Fixed : issue with doAllowDialogCancel on non Vista emulation
  //         : Fixed : issue with doAllowMinimize on non Vista emulation
  // 1.2.4.0 : Improved : removed limitation on text length of Content, Title, ... in Vista native mode
  //         : Improved : handling of linefeed character on non Vista emulation
  //         : Improved : handling of anchors in Vista native mode
  //         : Improved : handling of ESC with common buttons
  // 1.2.4.1 : Improved : prevent that Alt-F4 can close the dialog
  // 1.2.5.0 : New : support for hotkeys on expand/contract text on non-Vista emulation
  // 1.2.5.1 : Fixed : issue with identical accelerator key for expand/collaps
  // 1.2.6.0 : Improved : taskdialog does not size beyond screen width
  //         : Improved : DefaultButton can be set to -1 to have no default button
  // 1.2.7.0 : New: NonNativeDialog property added
  //         : New: NonNativeMinFormWidth public property added
  // 1.2.8.0 : Improved : display of disabled task button
  // 1.2.8.1 : Fixed : display of long text in non native taskdialog
  // 1.2.8.2 : Fixed : issue with DefaultButton = IdYes, IdNo
  // 1.5.0.0 : New : replacement functions for ShowMessage , MessageDlg
  //         : New : TAdvInputTaskDialog
  //         : New : ElevateButton method added
  //         : Improved : message label set transparent
  //         : Improved : Ctrl-C puts taskdialog text on clipboard
  // 1.5.0.1 : Fixed : Delphi 5 issue with TAdvInputTaskDialog
  // 1.5.0.2 : Fixed : issue with use of TAdvTaskDialog on topmost forms
  // 1.5.0.3 : Improved : automatic height adaption of custom input control
  // 1.5.0.4 : Fixed : issue with removing InputControl at designtime
  // 1.5.0.5 : Improved : width control of custom editor in TAdvInputTaskDialog
  // 1.5.0.6 : Improved : AdvShowMessageBox() handling of ESC key for cancel button
  // 1.5.0.7 : Improved : handling of \n linefeed sequence
  // 1.5.0.8 : Improved : use of dialog constants in AdvMessageDlg procs
  // 1.5.0.9 : Improved : use of question icon in mtConfirmation dialog type
  // 1.5.1.0 : Improved : support for F1 help handling
  //         : Improved : support for HelpContext in message dialog replacements
  //         : New : various new AdvMessageDlg() function overloads to set Title & Caption separately
  // 1.5.1.1 : Fixed : issue with use of dialog on modal StayOnTop forms
  // 1.5.1.2 : Improved : handling of button disabling for non native dialog
  // 1.5.1.3 : Improved : Clear method clears InputText field too
  // 1.5.1.4 : Fixed : issue with handling OnDialogClose and custom input controls in TAdvInputTaskDialog
  // 1.5.1.5 : Fixed : close button shown on emulated dialog when doAllowDialogCancel is set
  // 1.5.1.6 : Improved : when custom input control is wider than taskdialog, adapt width of taskdialog
  // 1.5.2.0 : New : exposed OnDialogHyperlinkClick event for TAdvInputTaskDialog
  // 1.5.2.1 : Fixed : small height calculation issue when VerificationText is used in emulated dialog
  //         : Improved : automatic default button selection
  // 1.5.3.0 : New : design time preview for TAdvInputTaskDialog
  // 1.5.3.1 : Fixed : issue with default button setting for dialogs with only custom buttons
  // 1.5.3.2 : Fixed : issue with default button for common buttons
  // 1.5.3.3 : Improved : verification checkbox position when expanded text is used
  // 1.5.3.4 : Improved : behaviour of dialog replacement calls on topmost forms
  //         : Fixed : command links option not used when no custom buttons are added
  // 1.5.3.5 : Improved : expand/collaps button background painting
  // 1.5.3.6 : Fixed : issue with creating TAdvTaskDialog at runtime
  // 1.5.3.7 : Fixed : issue with runtime native TaskDialog on Windows 7 and Windows Vista 64bit
  // 1.5.4.0 : New : Close method added for automatic closing of the dialog
  // 1.6.0.0 : New : AutoClose capability added
  // 1.6.0.1 : Fixed : Height calculation for dialog with verification checkbox and no buttons
  // 1.6.0.2 : Fixed : Issue with Marquee style progressbar
  // 1.6.0.4 : Fixed : Incorrect image in AdvTaskMessageDlgPosHelp
  // 1.6.0.5 : Fixed : Issue with focusing non enabled buttons
  // 1.6.0.6 : Fixed : Issue with default Close button in emulated dialog
  // 1.6.1.0 : New : InputDropDownCount property added in TAdvInputTaskDialog
  // 1.6.2.0 : New : Added capability to preset date in TAdvInputTaskDialog
  // 1.7.0.0 : New : Support for HTML formatted text added in TAdvInputTaskDialog
  //         : New : OnDialogHyperlinkClick event added in TAdvInputTaskDialog
  //         : New : ExpandedDefault added in TAdvInputTaskDialog
  // 1.7.1.0 : New : Property Options exposed as public property in TAdvInputTaskDialog
  // 1.7.1.1 : Fixed : Issue with OnDialogButtonClick not triggered for non native dialog for common buttons
  // 1.7.1.2 : Fixed : Issue with remembering verify checkbox state
  // 1.7.1.3 : Fixed : Issue with OnDialogButtonClick in TAdvInputTaskDialogEx
  // 1.7.1.4 : Fixed : Issue with combination of verificationcheckbox & footer
  // 1.7.2.0 : New : Event OnValidateInputText added in TAdvInputTaskDialog
  //         : Improved : OnDialogClose event sequence in TAdvInputTaskDialog
  // 1.7.2.1 : Improved : Scaling of dialog dependent on screen DPI
  // 1.7.2.2 : Improved : Removed size limitation on input text of TAdvInputTaskDialog
  // 1.7.3.0 : Improved : Public property MultiLineButtons: boolean added
  // 1.7.3.1 : Improved : Copy additional text when expanded
  // 1.7.3.2 : Fixed : Issue initialization of radiobutton heights
  // 1.7.3.3 : Fixed : Issue in 64bit app
  // 1.7.3.4 : Improved : Date preset capability in TAdvInputTaskDialog
  // 1.8.0.0 : New : Event OnAutoClose added
  // 1.8.0.1 : Fixed : Issue in 64bit app
  // 1.8.1.0 : New : Property InputMaxLength added in TAdvInputTaskDialog
  // 1.8.1.1 : Improved : Positioning & button focus handling for single OK button non-native taskdialog
  // 1.8.1.2 : Fixed : Issue with custom button positioning
  // 1.8.1.3 : Fixed : Issue with TAdvTaskDialog non native dialog Instruction position
  // 1.8.1.4 : Improved : Non-native taskdialog display in specific circumstances on Windows XP
  // 1.8.1.5 : Fixed : Issue with verification checkbox in non native dialog with commandlinks
  // 1.8.2.0 : New : Public property InputItemIndex added in TAdvInputTaskDialog
  // 1.8.3.0 : New : Public property InputDateTime added in TAdvInputTaskDialog
  //         : New : InputType itTime added
  // 1.8.3.1 : Fixed : Issue with using mask editors as custom editors for TAdvInputTaskDialog
  // 1.8.4.0 : Improved : Initialization of verify checkbox state
  // 1.8.4.1 : Fixed : Focus issue with custom controls on TAdvInputTaskDialog with TabStop = false
  // 1.8.4.2 : Fixed : Issue with auto timer close
  // 1.8.4.3 : Fixed : Text result incorrect for itTime input in TAdvInputTaskDialog
  // 1.8.5.0 : New : TAdvInputTaskDialog.DateTimeFormat property added
  // 1.8.5.1 : Fixed : Update of input values from the OnDialogButtonClick event
  // 1.8.6.0 : New : Inputtype itPassword added in TAdvInputTaskDialog
  // 1.8.6.1 : Improved : Footer drawing
  // 1.8.6.2 : Fixed : Issue with cancelling TAdvInputTaskDialog
  // 1.8.6.3 : Fixed : Issue with using input control for TAdvInputTaskDialog that has no parent set
  // 1.8.6.4 : Fixed : Issue with AdvTaskMessageDialogPos & position
  // 1.8.6.5 : Fixed : Issue with automatic parent window assignment
  // 1.8.6.6 : Fixed : Rare issue with runtime created TAdvTaskDialog
  // 1.8.6.7 : Fixed : Rare issue with blank TAdvTaskDialog icon
  // 1.8.7.0 : New : doSizeToContent added under Options
  // 1.8.8.0 : Improved : VCL styles compatibility

type
  TTaskDialogResult = (trNone, trOk, trCancel);

  TNonNativeDialog = (nndAuto, nndAlways);

  TTaskDialogOption = (doHyperlinks, doCommandLinks, doCommandLinksNoIcon, doExpandedDefault,
    doExpandedFooter, doAllowMinimize, doVerifyChecked, doProgressBar, doProgressBarMarquee,
    doTimer, doNoDefaultRadioButton, doAllowDialogCancel, doSizeToContent);

  TTaskDialogOptions = set of TTaskDialogOption;

  TTaskDialogIcon = (tiBlank, tiWarning, tiQuestion, tiError, tiInformation,tiNotUsed,tiShield);
                    //(mtWarning, mtError, mtInformation, mtConfirmation, mtCustom);
  TTaskDialogFooterIcon = (tfiBlank, tfiWarning, tfiQuestion, tfiError, tfiInformation,
    tfiShield);

  TTaskDialogProgressState = (psNormal, psError, psPaused);

  TTaskDialogPosition = (dpScreenCenter, dpOwnerFormCenter);

  TCommonButton = (cbOK, cbYes, cbNo, cbCancel, cbRetry, cbClose);

  TTaskDialogButtonClickEvent = procedure(Sender: TObject; ButtonID: integer) of object;
  TTaskDialogHyperlinkClickEvent = procedure(Sender: TObject; HRef: string) of object;
  TTaskDialogVerifyClickEvent = procedure(Sender: TObject; Checked: boolean) of object;
  TTaskDialogCloseEvent = procedure(Sender: TObject; var CanClose: boolean) of object;

  TTaskDialogProgressEvent = procedure(Sender: TObject; var Pos: integer; var State: TTaskDialogProgressState) of object;

  TCommonButtons = set of TCommonButton;

  TAdvMessageForm = class;

  TInputType = (itEdit, itMemo, itComboEdit, itComboList, itDate, itCustom, itNone, itTime, itPassword);

  TInputGetTextEvent = procedure(Sender: TObject; var Text: string) of object;
  TInputSetTextEvent = procedure(Sender: TObject; Text: string) of object;

  TValidateInputTextEvent = Procedure(Sender: TObject; var NewValue: string; const Data, ModalResult:
    Integer; var IsValid : Boolean) of Object;

  TCustomAdvTaskDialog = class(TComponent)
  private
    FTitle: string;
    FContent: string;
    FFooter: string;
    FInstruction: string;
    FIsNative: boolean;
    FCommonButtons: TCommonButtons;
    FExpandedText: string;
    FCollapsControlText: string;
    FExpandControlText: string;
    FButtonResult: integer;
    FVerifyResult: boolean;
    FVerifyText: string;
    FCustomButtons: TStringList;
    FCustomIcon: TIcon;
    FOptions: TTaskDialogOptions;
    FRadioButtons: TStringList;
    FhWnd: THandle;
    FOnCreated: TNotifyEvent;
    FOnTimer: TNotifyEvent;
    FHelpContext: longint;
    FProgressBarMin: integer;
    FProgressBarMax: integer;
    FAutoClose: Boolean;
    FAutoCloseTimeout: integer;
    FAutoCloseCounter: integer;
    FOnAutoClose: TNotifyEvent;
    FOnDialogHyperlinkClick: TTaskDialogHyperlinkClickEvent;
    FOnDialogClick: TTaskDialogButtonClickEvent;
    FOnDialogRadioClick: TTaskDialogButtonClickEvent;
    FOnDialogVerifyClick: TTaskDialogVerifyClickEvent;
    FOnDialogProgress: TTaskDialogProgressEvent;
    FOnDialogClose: TTaskDialogCloseEvent;
    FOnDialogInputGetText: TInputGetTextEvent;
    FOnDialogInputSetText: TInputSetTextEvent;
    FIcon: TTaskDialogIcon;
    FFooterIcon: TTaskDialogFooterIcon;
    FDefaultButton: integer;
    FDefaultRadioButton: integer;
    FDialogForm: TAdvMessageForm;
    FDlgPosition: TTaskDialogPosition;
    FApplicationIsParent: Boolean;
    FAlwaysOnTop: Boolean;
    FModalParent: THandle;
    FMinFormWidth: Integer;
    FNonNativeDialog: TNonNativeDialog;
    FInputType: TInputType;
    FInputText: string;
    FInputItems: TStrings;
    FInputControl: TWinControl;
    FInputDropDownCount: Integer;
    FFooterColor: TColor;
    FFooterLineColor: TColor;
    FMultiLineButtons: boolean;
    FInputMaxLength: integer;
    FInputItemIndex: integer;
    FInputDateTime: TDateTime;
    FDateTimeFormat: string;
    FNormalFontColor: TColor;
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    function GetVersionNr: Integer;
    procedure SetCustomButtons(const Value: TStringList);
    procedure SetRadioButtons(const Value: TStringList);
    procedure SetContent(const Value: string);
    procedure SetInstruction(const Value: string);
    procedure SetFooter(const Value: string);
    procedure SetExpandedText(const Value: string);
    procedure SetCustomIcon(const Value: TIcon);
    procedure SetInputItems(const Value: TStrings);
  protected
    function CreateButton(AOwner: TComponent): TWinControl; virtual;
    function CreateRadioButton(AOwner: TComponent): TWinControl; virtual;
    procedure InitRadioButton(AOwner: TForm; Btn: TWinControl; btnIndex: Integer; OnClickEvent : TNotifyEvent); virtual;
    procedure SetRadioButtonState(Btn: TWinControl; Checked: boolean); virtual;
    procedure SetRadioButtonCaption(Btn: TWinControl; Value: string); virtual;
    procedure SetButtonCaption(aButton: TWinControl; Value: TCaption); virtual;
    procedure SetButtonCancel(aButton: TWinControl; Value: Boolean); virtual;
    procedure SetButtonDefault(aButton: TWinControl; Value: Boolean); virtual;
    procedure SetButtonModalResult(aButton: TWinControl; Value: Integer); virtual;
    procedure DoAutoClose; virtual;
    function GetButtonModalResult(aButton: TWinControl): Integer; virtual;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure TaskDialogFormCreated(Sender: TObject);
    property CustomButtons: TStringList read FCustomButtons write SetCustomButtons;
    property CustomIcon: TIcon read FCustomIcon write SetCustomIcon;
    property RadioButtons: TStringList read FRadioButtons write SetRadioButtons;
    property CommonButtons: TCommonButtons read FCommonButtons write FCommonButtons;
    property DefaultButton: integer read FDefaultButton write FDefaultButton;
    property DefaultRadioButton: integer read FDefaultRadioButton write FDefaultRadioButton;
    property DialogPosition: TTaskDialogPosition read FDlgPosition write FDlgPosition default dpScreenCenter;
    property ExpandedText: string read FExpandedText write SetExpandedText;
    property Footer: string read FFooter write SetFooter;
    property FooterIcon: TTaskDialogFooterIcon read FFooterIcon write FFooterIcon default tfiBlank;
    property HelpContext: longint read FHelpContext write FHelpContext default 0;
    property Icon: TTaskDialogIcon read FIcon write FIcon default tiBlank;
    property InputDropDownCount: integer read FInputDropDownCount write FInputDropDownCount default 8;
    property InputMaxLength: integer read FInputMaxLength write FInputMaxLength default 0;
    property InputText: string read FInputText write FInputText;
    property InputType: TInputType read FInputType write FInputType;
    property InputItems: TStrings read FInputItems write SetInputItems;
    property InputControl: TWinControl read FInputControl write FInputControl;
    property InputItemIndex: integer read FInputItemIndex write FInputItemIndex;
    property InputDateTime: TDateTime read FInputDateTime write FInputDateTime;
    property Title: string read FTitle write FTitle;
    property Instruction: string read FInstruction write SetInstruction;
    property Content: string read FContent write SetContent;
    property ExpandControlText: string read FExpandControlText write FExpandControlText;
    property CollapsControlText: string read FCollapsControlText write FCollapsControlText;
    property Options: TTaskDialogOptions read FOptions write FOptions;
    property VerificationText: string read FVerifyText write FVerifyText;
    property NonNativeDialog: TNonNativeDialog read FNonNativeDialog write FNonNativeDialog default nndAuto;
    property NonNativeMinFormWidth: integer read FMinFormWidth write FMinFormWidth default 350;
    property AutoClose: boolean read FAutoClose write FAutoClose default false;
    property AutoCloseTimeOut: integer read FAutoCloseTimeOut write FAutoCloseTimeOut default 0;
    property DateTimeFormat: string read FDateTimeFormat write FDateTimeFormat;

    property ProgressBarMin: integer read FProgressBarMin write FProgressBarMin default 0;
    property ProgressBarMax: integer read FProgressBarMax write FProgressBarMax default 100;
    property Version: string read GetVersion write SetVersion;

    property OnAutoClose: TNotifyEvent read FOnAutoClose write FOnAutoClose;
    property OnDialogCreated: TNotifyEvent read FOnCreated write FOnCreated;
    property OnDialogClose: TTaskDialogCloseEvent read FOnDialogClose write FOnDialogClose;
    property OnDialogButtonClick: TTaskDialogButtonClickEvent read FOnDialogClick write FOnDialogClick;
    property OnDialogInputSetText: TInputSetTextEvent read FOnDialogInputSetText write FOnDialogInputSetText;
    property OnDialogInputGetText: TInputGetTextEvent read FOnDialogInputGetText write FOnDialogInputGetText;
    property OnDialogRadioClick: TTaskDialogButtonClickEvent read FOnDialogRadioClick write FOnDialogRadioClick;
    property OnDialogHyperlinkClick: TTaskDialogHyperlinkClickEvent read FOnDialogHyperlinkClick write FOnDialogHyperLinkClick;
    property OnDialogTimer: TNotifyEvent read FOnTimer write FOnTimer;
    property OnDialogVerifyClick: TTaskDialogVerifyClickEvent read FOnDialogVerifyClick write FOnDialogVerifyClick;
    property OnDialogProgress: TTaskDialogProgressEvent read FOnDialogProgress write FOnDialogProgress;
    function ExecutePos(X,Y: integer): integer; virtual;
    procedure InitVCLStyle(init: boolean);
  public
    property ApplicationIsParent: boolean read FApplicationIsParent write FApplicationIsParent default false;
    property AlwaysOnTop: boolean read FAlwaysOnTop write FAlwaysOnTop default false; 
    property hWnd: THandle read FhWnd write FhWnd;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: integer; virtual;
    procedure Clear;
    procedure Close;
    procedure EnableButton(ButtonID: integer; Enabled: boolean);
    procedure ElevateButton(ButtonID: integer; Enabled: boolean);
    procedure ClickButton(ButtonID: integer);
    property RadioButtonResult: integer read FButtonResult write FButtonResult;
    property VerifyResult: boolean read FVerifyResult write FVerifyResult;
    property MultiLineButtons: boolean read FMultiLineButtons write FMultiLineButtons;
    property ModalParent: THandle read FModalParent write FModalParent;
    property FooterColor: TColor read FFooterColor write FFooterColor;
    property FooterLineColor: TColor read FFooterLineColor write FFooterLineColor;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvTaskDialog = class(TCustomAdvTaskDialog)
  public
   function Execute: integer; override;
  published
    property AutoClose;
    property AutoCloseTimeOut;
    property CustomButtons;
    property CustomIcon;
    property RadioButtons;
    property CommonButtons;
    property DefaultButton;
    property DefaultRadioButton;
    property DialogPosition;
    property ExpandedText;
    property Footer;
    property FooterIcon;
    property HelpContext;
    property Icon;
    property Title;
    property Instruction;
    property Content;
    property ExpandControlText;
    property CollapsControlText;
    property Options;
    property ApplicationIsParent;
    property VerificationText;
    property NonNativeDialog;
    property NonNativeMinFormWidth;

    property ProgressBarMin;
    property ProgressBarMax;
    property Version;

    property OnDialogCreated;
    property OnDialogClose;
    property OnDialogButtonClick;
    property OnDialogRadioClick;
    property OnDialogHyperlinkClick;
    property OnDialogTimer;
    property OnDialogVerifyClick;
    property OnDialogProgress;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvInputTaskDialog = class(TCustomAdvTaskDialog)
  private
    FData: Integer;
    FOnValidateInputText : TValidateInputTextEvent;
    FExpandedDefault: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    function Execute: integer; override;
    property Options;
    property ValidateData: Integer read FData write FData;
    property InputItemIndex;
    property InputDateTime;
  published
    property ApplicationIsParent;
    property CustomButtons;
    property CustomIcon;
    property CommonButtons;
    property DateTimeFormat;
    property DefaultButton;
    property DialogPosition;
    property ExpandedDefault: Boolean read FExpandedDefault write FExpandedDefault default False;
    property ExpandedText;
    property Footer;
    property FooterIcon;
    property Icon;
    property InputControl;
    property InputDropDownCount;
    property InputMaxLength;
    property InputType;
    property InputText;
    property InputItems;
    property Instruction;
    property NonNativeMinFormWidth;
    property Title;
    property Content;
    property ExpandControlText;
    property CollapsControlText;
    property VerificationText;
    property OnDialogCreated;
    property OnDialogClose;
    property OnDialogButtonClick;
    property OnDialogVerifyClick;
    property OnDialogInputSetText;
    property OnDialogInputGetText;
    property OnDialogHyperlinkClick;
    property OnValidateInputText: TValidateInputTextEvent read FOnValidateInputText write FOnValidateInputText;
  end;

  TTaskDialogButton = class(TCustomControl)
  private
    FOnMouseLeave: TNotifyEvent;
    FOnMouseEnter: TNotifyEvent;
    FGlyph: TBitmap;
    FGlyphDisabled: TBitmap;
    FGlyphDown: TBitmap;
    FGlyphHot: TBitmap;
    FMouseInControl: Boolean;
    FMouseDown: Boolean;
    FBorderColorDown: TColor;
    FBorderColorHot: TColor;
    FBorderColor: TColor;
    FModalResult: TModalResult;
    FHeadingFont: TFont;
    FAutoFocus: boolean;
    FBackColor: TColor;
    FMultiLine: boolean;
    procedure OnPictureChanged(Sender: TObject);
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure SetGlyph(const Value: TBitmap);
    procedure SetGlyphDisabled(const Value: TBitmap);
    procedure SetGlyphDown(const Value: TBitmap);
    procedure SetGlyphHot(const Value: TBitmap);
    procedure SetHeadingFont(const Value: TFont);
  protected
    procedure Paint; override;
    procedure KeyPress(var Key: char); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click; override;
    procedure DoEnter; override;
    procedure DoExit; override;
    property AutoFocus: boolean read FAutoFocus write FAutoFocus;
  published
    property Anchors;
    property BackColor: TColor read FBackColor write FBackColor default clNone;
    property BorderColor: TColor read FBorderColor write FBorderColor;
    property BorderColorHot: TColor read FBorderColorHot write FBorderColorHot;
    property BorderColorDown: TColor read FBorderColorDown write FBorderColorDown;
    property Constraints;
    property Enabled;
    property HeadingFont: TFont read FHeadingFont write SetHeadingFont;
    property ModalResult: TModalResult read FModalResult write FModalResult default 0;
    property MultiLine: boolean read FMultiLine write FMultiLine default true;
    property Picture: TBitmap read FGlyph write SetGlyph;
    property PictureHot: TBitmap read FGlyphHot write SetGlyphHot;
    property PictureDown: TBitmap read FGlyphDown write SetGlyphDown;
    property PictureDisabled: TBitmap read FGlyphDisabled write SetGlyphDisabled;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  end;

  TAdvMessageForm = class(TForm)
  private
    Message: TLabel;
    FHorzMargin: Integer;
    FVertMargin: Integer;
    FHorzSpacing: Integer;
    FVertSpacing: Integer;
    FExpandButton: TTaskDialogButton;
    FExpanded: Boolean;
    //FExpandLabel: TLabel;
    FExpandControlText: String;
    FCollapsControlText: String;
    FcmBtnList: TList;
    FcsBtnList: TList;
    FTaskDialog: TCustomAdvTaskDialog;
    FFooterIcon: TImage;
    FFooterIconID: PChar;
    FRadioList: TList;
    FVerificationCheck: TCheckBox;
    FProgressBar: TProgressBar;
    FIcon: TImage;
    FFooterXSize: Integer;
    FFooterYSize: Integer;
    FContentXSize: Integer;
    FContentYSize: Integer;
    FExpTextXSize: Integer;
    FExpTextYSize: Integer;
    FExpTextTop: Integer;
    FAnchor: String;
    FTimer: TTimer;
    FWhiteWindowHeight: Integer;
    FHorzParaMargin: Integer;
    FMinFormWidth: Integer;
    FInputEdit: TEdit;
    FInputCombo: TComboBox;
    FInputDate: TDateTimePicker;
    FInputMemo: TMemo;
    FOldParent: TWinControl;
    FFooterColor: TColor;
    FFooterLineColor: TColor;
    procedure WMActivate(var M: TWMActivate); message WM_ACTIVATE;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure OnTimer(Sender: TObject);
    procedure OnExpandButtonClick(Sender: TObject);
    procedure OnVerifyClick(Sender: TObject);
    procedure OnRadioClick(Sender: TObject);
    procedure OnButtonClick(Sender: TObject);
    procedure SetExpandButton(const Value: TTaskDialogButton);
    procedure GetTextSize(Canvas: TCanvas; Text: string;var W, H: Integer);
    //procedure GetMultiLineTextSize(Canvas: TCanvas; Text: string; HeadingFont, ParaFont: TFont; var W, H: Integer);
    //procedure HelpButtonClick(Sender: TObject);
  protected
    procedure SetExpanded(Value: Boolean);
    procedure CustomKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure WriteToClipBoard(Text: String);
    function GetFormText: String;
    procedure Paint; override;
    procedure KeyDown(var Key:Word;Shift:TShiftSTate); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure DoClose(var Action: TCloseAction); override;
    procedure DoUpdateInput; virtual;

    function GetButton(ButtonID: Integer; var TaskButton: TTaskDialogButton): TButton;
    procedure EnableButton(ButtonID: integer; Enabled: boolean);
    procedure ClickButton(ButtonID: integer);
    function IsAnchor(x, y: integer): string;
    function GetFooterRect: TRect;
    function GetContentRect: TRect;
    function GetExpTextRect: TRect;
    procedure DrawExpandedText;
    procedure DrawContent;
    procedure DrawFooter;
    property Expanded: Boolean read FExpanded default true;
    property ExpandButton: TTaskDialogButton read FExpandButton write SetExpandButton;
    procedure DoShow; override;
    property FooterColor: TColor read FFooterColor write FFooterColor;
    property FooterLineColor: TColor read FFooterLineColor write FFooterLineColor;
  public
    constructor CreateNew(AOwner: TComponent; Dummy: Integer); {$IFNDEF BCB} reintroduce; {$ENDIF}
    destructor Destroy; override;
    procedure BuildTaskDialog(TaskDialog: TCustomAdvTaskDialog);
    procedure SetPositions;
    procedure UpdateDialog;
    property MinFormWidth: Integer Read FMinFormWidth Write FMinFormWidth;
  end;

  function AdvMessageDlgPos(TaskDialog: TCustomAdvTaskDialog; X, Y: Integer): Integer;


function AdvShowMessage(const Instruction: string): boolean; overload;
function AdvShowMessage(const Title, Instruction: string): boolean; overload;
function AdvShowmessage(const Title, Instruction: string; tiIcon: tTaskDialogIcon): boolean; overload;
function AdvShowMessage(const Title, Instruction, content, verify: string;
  tiIcon: tTaskDialogIcon): boolean; overload;

function AdvMessageBox(hWnd: HWND; lpInstruction, lpTitle: PChar; flags: UINT; NonNative: boolean = false): Integer;


function AdvShowMessageFmt(const Instruction: string; Parameters: array of const): boolean;

function AdvMessageDlg(const Instruction: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint): Integer; overload;

function AdvMessageDlg(const Instruction: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; DefaultButton: TMsgDlgBtn): Integer; overload;

function AdvTaskMessageDlg(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint): Integer; overload;

function AdvTaskMessageDlg(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; DefaultButton: TMsgDlgBtn): Integer; overload;

function AdvTaskMessageDlgPos(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; X, Y: Integer): Integer; overload;

function AdvTaskMessageDlgPos(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; X, Y: Integer;
  DefaultButton: TMsgDlgBtn): Integer; overload;

function AdvTaskMessageDlgPosHelp(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; X, Y: Integer;
  const HelpFileName: string): Integer; overload;

function AdvTaskMessageDlgPosHelp(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; X, Y: Integer;
  const HelpFileName: string; DefaultButton: TMsgDlgBtn): Integer; overload;

function AdvInputQueryDlg(ACaption, APrompt: string; var Value: string): boolean;

var
  DRAWBORDER: Boolean = True;
  ButtonNames: array[TCommonButton] of string = ('OK', 'Yes', 'No', 'Cancel', 'Retry', 'Abort');
  ButtonCaptions: array[TCommonButton] of Pointer;

procedure Register;

implementation

uses
{$IFDEF DELPHIXE2_LVL}
  VCL.Themes,
{$ENDIF}
  Mask;

{$I HTMLENGO.PAS}

const
   TDE_CONTENT                         = 0;
   TDE_EXPANDED_INFORMATION            = 1;
   TDE_FOOTER                          = 2;
   TDE_MAIN_INSTRUCTION                = 3;

    TDF_ENABLE_HYPERLINKS               = $0001;
    TDF_USE_HICON_MAIN                  = $0002;
    TDF_USE_HICON_FOOTER                = $0004;
    TDF_ALLOW_DIALOG_CANCELLATION       = $0008;
    TDF_USE_COMMAND_LINKS               = $0010;
    TDF_USE_COMMAND_LINKS_NO_ICON       = $0020;
    TDF_EXPAND_FOOTER_AREA              = $0040;
    TDF_EXPANDED_BY_DEFAULT             = $0080;
    TDF_VERIFICATION_FLAG_CHECKED       = $0100;
    TDF_SHOW_PROGRESS_BAR               = $0200;
    TDF_SHOW_MARQUEE_PROGRESS_BAR       = $0400;
    TDF_CALLBACK_TIMER                  = $0800;
    TDF_POSITION_RELATIVE_TO_WINDOW     = $1000;
    TDF_RTL_LAYOUT                      = $2000;
    TDF_NO_DEFAULT_RADIO_BUTTON         = $4000;
    TDF_CAN_BE_MINIMIZED                = $8000;
    TDF_SIZE_TO_CONTENT                 = $1000000;

    TDM_NAVIGATE_PAGE                   = WM_USER+101;
    TDM_CLICK_BUTTON                    = WM_USER+102; // wParam = Button ID
    TDM_SET_MARQUEE_PROGRESS_BAR        = WM_USER+103; // wParam = 0 (nonMarque) wParam != 0 (Marquee)
    TDM_SET_PROGRESS_BAR_STATE          = WM_USER+104; // wParam = new progress state
    TDM_SET_PROGRESS_BAR_RANGE          = WM_USER+105; // lParam = MAKELPARAM(nMinRange, nMaxRange)
    TDM_SET_PROGRESS_BAR_POS            = WM_USER+106; // wParam = new position
    TDM_SET_PROGRESS_BAR_MARQUEE        = WM_USER+107; // wParam = 0 (stop marquee), wParam != 0 (start marquee), lparam = speed (milliseconds between repaints)
    TDM_SET_ELEMENT_TEXT                = WM_USER+108; // wParam = element (TASKDIALOG_ELEMENTS), lParam = new element text (LPCWSTR)
    TDM_CLICK_RADIO_BUTTON              = WM_USER+110; // wParam = Radio Button ID
    TDM_ENABLE_BUTTON                   = WM_USER+111; // lParam = 0 (disable), lParam != 0 (enable), wParam = Button ID
    TDM_ENABLE_RADIO_BUTTON             = WM_USER+112; // lParam = 0 (disable), lParam != 0 (enable), wParam = Radio Button ID
    TDM_CLICK_VERIFICATION              = WM_USER+113; // wParam = 0 (unchecked), 1 (checked), lParam = 1 (set key focus)
    TDM_UPDATE_ELEMENT_TEXT             = WM_USER+114; // wParam = element (TASKDIALOG_ELEMENTS), lParam = new element text (LPCWSTR)
    TDM_SET_BUTTON_ELEVATION_REQUIRED_STATE = WM_USER+115; // wParam = Button ID, lParam = 0 (elevation not required), lParam != 0 (elevation required)
    TDM_UPDATE_ICON                     = WM_USER+116;  // wParam = icon element (TASKDIALOG_ICON_ELEMENTS), lParam = new icon (hIcon if TDF_USE_HICON_* was set, PCWSTR otherwise)

    TDN_CREATED                         = 0;
    TDN_NAVIGATED                       = 1;
    TDN_BUTTON_CLICKED                  = 2;            // wParam = Button ID
    TDN_HYPERLINK_CLICKED               = 3;            // lParam = (LPCWSTR)pszHREF
    TDN_TIMER                           = 4;            // wParam = Milliseconds since dialog created or timer reset
    TDN_DESTROYED                       = 5;
    TDN_RADIO_BUTTON_CLICKED            = 6;            // wParam = Radio Button ID
    TDN_DIALOG_CONSTRUCTED              = 7;
    TDN_VERIFICATION_CLICKED            = 8;             // wParam = 1 if checkbox checked, 0 if not, lParam is unused and always 0
    TDN_HELP                            = 9;
    TDN_EXPANDO_BUTTON_CLICKED          = 10;            // wParam = 0 (dialog is now collapsed), wParam != 0 (dialog is now expanded)

    TDCBF_OK_BUTTON            = $0001; // selected control return value IDOK
    TDCBF_YES_BUTTON           = $0002; // selected control return value IDYES
    TDCBF_NO_BUTTON            = $0004; // selected control return value IDNO
    TDCBF_CANCEL_BUTTON        = $0008; // selected control return value IDCANCEL
    TDCBF_RETRY_BUTTON         = $0010; // selected control return value IDRETRY
    TDCBF_CLOSE_BUTTON         = $0020;  // selected control return value IDCLOSE

    PBST_NORMAL        =     $0001;
    PBST_ERROR         =     $0002;
    PBST_PAUSED        =     $0003;
{
    TD_ICON_BLANK = 100;
    TD_ICON_WARNING = 101;
    TD_ICON_QUESTION = 102;
    TD_ICON_ERROR = 103;
    TD_ICON_INFORMATION = 104;
    TD_ICON_BLANK_AGAIN = 105;
    TD_ICON_SHIELD = 106;
}
    // Well, Microsoft did it again, incorrect TD_ICON_xxx values in the SDK
    // and changing values just between last beta2 & RTM... Gotta love them.
    // These values were obtained emperically by the lack of proper documentation

    TD_ICON_BLANK = 17;
    TD_ICON_WARNING = 84;
    TD_ICON_QUESTION = 99;
    TD_ICON_ERROR = 98;
    TD_ICON_INFORMATION = 81;
    TD_ICON_BLANK_AGAIN = 0;
    TD_ICON_SHIELD = 78;


type
  TProControl = class(TControl);

{$IFDEF DELPHIXE2_LVL}
{$ALIGN 1}
{$ENDIF}

  PTASKDIALOG_BUTTON = ^TTASKDIALOG_BUTTON;
  TTASKDIALOG_BUTTON  = record
    nButtonID: integer;
    pszButtonText: pwidechar;
  end;

  TTaskDialogWideString = array[0..1023] of widechar;

  TTaskDialogButtonArray = array of TTASKDIALOG_BUTTON;
  TTaskDialogWideStringArray = array of TTaskDialogWideString;

  PTASKDIALOGCONFIG = ^TTASKDIALOGCONFIG;
  TTASKDIALOGCONFIG = record
    cbSize: UINT;
    hwndParent: THandle;
    hInstance: THandle;
    dwFlags: DWORD;   // TASKDIALOG_FLAGS dwFlags;
    dwCommonButtons: DWORD; //  TASKDIALOG_COMMON_BUTTON_FLAGS
    pszWindowTitle: pwidechar;
    hMainIcon: HICON;
    pszMainInstruction: pwidechar;
    pszContent: pwidechar;
    cButtons: integer;
    pbuttons: pinteger;  // const TASKDIALOG_BUTTON* pButtons;
    nDefaultButton: integer;
    cRadioButtons: UINT;
    pRadioButtons: pinteger; //const TASKDIALOG_BUTTON* pRadioButtons;
    nDefaultRadioButton: integer;
    pszVerificationText: pwidechar;
    pszExpandedInformation: pwidechar;
    pszExpandedControlText: pwidechar;
    pszCollapsedControlText: pwidechar;
    case Integer of
    0: (hFooterIcon: HICON);
    1: (pszFooterIcon: pwidechar;
        pszFooter: pwidechar;
        pfCallback: pinteger;
        pData: pointer;
        cxWidth: UINT  // width of the Task Dialog's client area in DLU's.
                               // If 0, Task Dialog will calculate the ideal width.
              );
  end;
{$IFDEF DELPHIXE2_LVL}
{$ALIGN ON}
{$ENDIF}


//------------------------------------------------------------------------------
function IsVista: boolean;
var
  hKernel32: HMODULE;
begin
  hKernel32 := GetModuleHandle('kernel32');
  if (hKernel32 > 0) then
  begin
    Result := GetProcAddress(hKernel32, 'GetLocaleInfoEx') <> nil;
  end
  else
    Result := false;
end;

//------------------------------------------------------------------------------

procedure RunElevated(HWND: THandle; pszPath, pszParameters, pszDirectory: string);
var
  shex :  SHELLEXECUTEINFO;
begin
  fillchar(shex, sizeof(shex),0);
  shex.cbSize := sizeof( SHELLEXECUTEINFO );
  shex.fMask := 0;
  shex.wnd := hwnd;
  shex.lpVerb := 'runas';
  shex.lpFile := pchar(pszPath);
  shex.lpParameters := pchar(pszParameters);
  shex.lpDirectory := nil;
  shex.nShow := SW_NORMAL;
  ShellExecuteEx(@shex);
end;

//------------------------------------------------------------------------------

procedure VistaShellOpen(HWND: THandle; Command, Param: string);
begin
  if IsVista then
    RunElevated(HWND, Command, Param, '')
  else
    ShellExecute(HWND, 'open', pchar(Param), nil, nil, SW_NORMAL);
end;

//------------------------------------------------------------------------------

function GetFileVersion(const AFileName: string): Cardinal;
var
  FileName: string;
  InfoSize, Wnd: DWORD;
  VerBuf: Pointer;
  FI: PVSFixedFileInfo;
  VerSize: DWORD;
begin
  Result := Cardinal(-1);
  // GetFileVersionInfo modifies the filename parameter data while parsing.
  // Copy the string const into a local variable to create a writeable copy.
  FileName := AFileName;
  UniqueString(FileName);
  InfoSize := GetFileVersionInfoSize(PChar(FileName), Wnd);
  if InfoSize <> 0 then
  begin
    GetMem(VerBuf, InfoSize);
    try
      if GetFileVersionInfo(PChar(FileName), Wnd, InfoSize, VerBuf) then
        if VerQueryValue(VerBuf, '\', Pointer(FI), VerSize) then
          Result:= FI.dwFileVersionMS;
    finally
      FreeMem(VerBuf);
    end;
  end;
end;


function TaskDialogCallbackProc(hWnd: THandle; msg, wParam, lparam: integer; refData: pointer): integer; stdcall;
var
  td: TAdvTaskDialog;
  SPos: integer;
  State: TTaskDialogProgressState;
  Res: integer;
  CanClose: boolean;
  Anchor: string;

  procedure ShowHelpException(E: Exception);
  var
    Msg: string;
    Flags: Integer;
  begin
    Flags := MB_OK or MB_ICONSTOP;
    if Application.UseRightToLeftReading then
      Flags := Flags or MB_RTLREADING;
    Msg := E.Message;
    if (Msg <> '') and (AnsiLastChar(Msg) > '.') then
      Msg := Msg + '.';
    MessageBox(0, PChar(Msg), PChar(Application.Title), Flags);
  end;

begin
  td := nil;
  if Assigned(refdata) then
    td := TAdvTaskDialog(refdata);

  Res := 0;

  if Assigned(td) then
    td.hWnd := hWnd;

  case msg of
  TDN_CREATED:
    begin
      if Assigned(td) then
      begin
        if Assigned(td.OnDialogCreated) then
          td.OnDialogCreated(td);

        if (doProgressBar in td.Options) then
        begin
          SendMessage(hWnd, TDM_SET_PROGRESS_BAR_RANGE, 0, MakeLParam(td.ProgressBarMin,td.ProgressBarMax));
        end;

        if (doProgressBarMarquee in td.Options) then
        begin
          SendMessage(hWnd, TDM_SET_PROGRESS_BAR_MARQUEE, 1, 10);
        end;
      end;
    end;
  TDN_BUTTON_CLICKED:
    begin
      if Assigned(td) then
      begin
        if Assigned(td.OnDialogButtonClick) then
          td.OnDialogButtonClick(td, wParam);

        if Assigned(td.OnDialogClose) then
        begin
          CanClose := true;
          td.OnDialogClose(td, CanClose);
          if not CanClose then
            Res := 1;
        end;
      end;
    end;
  TDN_RADIO_BUTTON_CLICKED:
    begin
      if Assigned(td) and Assigned(td.OnDialogRadioClick) then
      begin                                                                  
        td.OnDialogRadioClick(td, wParam);
      end;
    end;
  TDN_HYPERLINK_CLICKED:
    begin
      if Assigned(td) then
      begin
        Anchor := WideCharToString(PWideChar(lparam));

        if not Assigned(td.OnDialogHyperlinkClick) then
        begin
          if (Pos('://', Anchor) > 0) then
            VistaShellOpen(0, 'iexplore.exe', Anchor);
        end;

        if Assigned(td.OnDialogHyperlinkClick) then
        begin
          td.OnDialogHyperlinkClick(td, Anchor);
        end;
      end;
    end;
  TDN_VERIFICATION_CLICKED:
    begin
      if Assigned(td) and Assigned(td.OnDialogVerifyClick) then
      begin
        td.OnDialogVerifyClick(td, bool(wparam));
      end;
    end;
  TDN_HELP:
    begin
      if Assigned(td) then
        if td.HelpContext <> 0 then
        try
          Application.HelpContext(td.HelpContext);
        except
          on E: Exception do
            ShowHelpException(E);
        end;
    end;
  TDN_TIMER:
    begin
      if Assigned(td) then
      begin
        if Assigned(td.OnDialogTimer) then
          td.OnDialogTimer(td);

        if Assigned(td.OnDialogProgress) then
        begin
          td.OnDialogProgress(td, SPos, State);
          SendMessage(hWnd,TDM_SET_PROGRESS_BAR_POS,SPos,0);
          case State of
          psNormal: SendMessage(hWnd,TDM_SET_PROGRESS_BAR_STATE, PBST_NORMAL, 0);
          psError: SendMessage(hWnd,TDM_SET_PROGRESS_BAR_STATE, PBST_ERROR, 0);
          psPaused: SendMessage(hWnd,TDM_SET_PROGRESS_BAR_STATE, PBST_PAUSED, 0);
          end;
        end;

        td.FAutoCloseCounter := td.FAutoCloseCounter + 200;

        if (td.FAutoCloseCounter > td.AutoCloseTimeOut) and
           (td.AutoCloseTimeOut > 0) and (td.AutoClose) then
        begin
          td.DoAutoClose;
          td.Close;
        end;
      end;

    end;
  end;

  Result := Res;
end;

//------------------------------------------------------------------------------

function RemoveSpaces(S: String): String;
var
  i: Integer;
begin
  Result := S;
  for i := 1 to Length(s) do
  begin
    if (s[i] = ' ') then
      Result := copy(S, 2, Length(S)-1)
    else
      Break;
  end;

  for i := Length(s) downto 1 do
  begin
    if (s[i] = ' ') then
      Result := copy(S, 1, Length(S)-1)
    else
      Break;
  end;
end;

//------------------------------------------------------------------------------

function HasLf(s:string): boolean;
var
  i,j: integer;
begin
  Result := false;
  i := pos('\n', s);
  if i > 0 then
  begin
    j := pos(':\n',s);

    if (j = -1) or (j <> i - 1) then
      Result := true;
  end;
end;

//------------------------------------------------------------------------------

procedure SplitInToLines(Text: string; sl: TStrings);
var
  i, j: Integer;
  s, rs: string;
begin
  if (Text <> '') and Assigned(sl) then
  begin
    rs := #13;
    if HasLf(Text) or (pos(rs, Text) > 0) then
    begin
      Text := RemoveSpaces(Text);

      while (Length(Text) > 0) do
      begin
        i := Pos('\n', Text);
        j := 2;
        if (i <= 0) then
        begin
          i := pos(rs, Text);
          j := 2;
        end;

        if (i <= 0) then
        begin
          i := Length(Text)+1;
          j := 0;
        end;  
        s := copy(Text, 1, i-1);
        Delete(Text, 1, i-1+j);
        s := RemoveSpaces(s);
        sl.Add(s);
        Text := RemoveSpaces(Text);
      end;
    end
    else
      sl.Add(Text);
  end;
end;

//------------------------------------------------------------------------------

procedure GetMultiLineTextSize(Canvas: TCanvas; Text: string; HeadingFont, ParaFont: TFont; DrawTextBiDiModeFlagsReadingOnly: Longint; var W, H: Integer; MultiLine: boolean; WithSpace: Boolean = True);
var
  R: TRect;
  i, tw, th: Integer;
  s: string;
  OldFont: TFont;
  SL: TStringList;
begin
  if Assigned(Canvas) then
  begin
    OldFont := TFont.Create;
    OldFont.Assign(Canvas.Font);
    if MultiLine and (HasLf(Text) or (pos(#13, Text) > 0)) then
    begin
      tw := 0;
      th := 0;

      SL := TStringList.Create;
      SplitInToLines(Text, SL);
      s := RemoveSpaces(SL[0]);

      if (s <> '') then
      begin
        Canvas.Font.Assign(HeadingFont);
        SetRect(R, 0, 0, 0, 0);
        Windows.DrawText(Canvas.Handle, PChar(s), -1, R,
          DT_CALCRECT or DT_LEFT or DT_SINGLELINE or DrawTextBiDiModeFlagsReadingOnly);
        tw := R.Right;
        th := R.Bottom;
        if WithSpace then
        begin
          tw := tw + 8;
          th := th + 10;
        end;
      end;

      Canvas.Font.Assign(ParaFont);
      for i:= 1 to SL.Count-1 do
      begin
        s := SL[i];
        if (s <> '') then
        begin
          SetRect(R, 0, 0, 0, 0);
          Windows.DrawText(Canvas.Handle, PChar(s), -1, R,
            DT_CALCRECT or DT_LEFT or DT_SINGLELINE or DrawTextBiDiModeFlagsReadingOnly);
          if WithSpace then
          begin
            tw := Max(tw, R.Right + 8);
            th := th + R.Bottom + 2;
          end
          else
          begin
            tw := Max(tw, R.Right);
            th := th + R.Bottom;
          end;
        end;
      end;

      W := tw;
      H := th;
      SL.Free;
    end
    else
    begin
      Canvas.Font.Assign(HeadingFont);
      SetRect(R, 0, 0, 0, 0);
      Windows.DrawText(Canvas.Handle, PChar(Text), -1, R,
        DT_CALCRECT or DT_LEFT or DT_SINGLELINE or DrawTextBiDiModeFlagsReadingOnly);
      W := R.Right;
      H := R.Bottom;
    end;

    Canvas.Font.Assign(OldFont);
    OldFont.Free;
  end;
end;

//------------------------------------------------------------------------------

{ TAdvTaskDialog }

procedure TCustomAdvTaskDialog.Clear;
begin
  CommonButtons := [];
  RadioButtons.Clear;
  CustomButtons.Clear;
  Icon := tiBlank;
  FooterIcon := tfiBlank;
  Instruction := '';
  Title := '';
  Content := '';
  Footer := '';
  VerificationText := '';
  ExpandControlText := '';
  CollapsControlText := '';
  ExpandedText := '';
  DefaultRadioButton := 200;
  DefaultButton := 0;
  Options := [];
  VerifyResult := false;
  InputText := '';
end;

//------------------------------------------------------------------------------

procedure TCustomAdvTaskDialog.ClickButton(ButtonID: integer);
begin
  SendMessage(hWnd, TDM_CLICK_BUTTON, ButtonID, 0);
  if Assigned(FDialogForm) then
    FDialogForm.ClickButton(ButtonID);
end;

procedure TCustomAdvTaskDialog.Close;
begin
  if FIsNative then
  begin
    // click the default button to close
    SendMessage(hWnd, TDM_CLICK_BUTTON, DefaultButton, 0);
  end
  else
    if Assigned(FDialogForm) then
    begin
      FDialogForm.Close;
    end;
end;

//------------------------------------------------------------------------------

constructor TCustomAdvTaskDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCustomButtons := TStringList.Create;
  FRadioButtons := TStringList.Create;
  FProgressBarMin := 0;
  FProgressBarMax := 100;
  FDialogForm := nil;
  FApplicationIsParent := false;
  FAlwaysOnTop := false;
  FModalParent := 0;
  FCustomIcon := TIcon.Create;
  FDefaultRadioButton := 200;
  FMinFormWidth := 350;
  FNonNativeDialog := nndAuto;
  FInputType := itNone;
  FInputItemIndex := -2;
  FInputDateTime := 0;
  FInputItems := TStringList.Create;
  FFooterColor := RGB(240,240,240);
  FFooterLineColor := RGB(223,223,223);
  FAutoClose := false;
  FAutoCloseTimeOut := 0;
  FMultiLineButtons := true;
  FNormalFontColor := clWindowText;
end;

//------------------------------------------------------------------------------

destructor TCustomAdvTaskDialog.Destroy;
begin
  FRadioButtons.Free;
  FCustomButtons.Free;
  FCustomIcon.Free;
  FInputItems.Free;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TCustomAdvTaskDialog.DoAutoClose;
begin
  if Assigned(OnAutoClose) then
    OnAutoClose(Self);
end;

//------------------------------------------------------------------------------

function TCustomAdvTaskDialog.CreateButton(AOwner: TComponent): TWinControl;
begin
  Result := TButton.Create(AOwner);
end;

//------------------------------------------------------------------------------

function TCustomAdvTaskDialog.CreateRadioButton(AOwner: TComponent): TWinControl;
begin
  Result := TRadioButton.Create(AOwner);
end;

procedure TCustomAdvTaskDialog.SetRadioButtonState(Btn: TWinControl; Checked: boolean);
begin
  TRadioButton(Btn).Checked := Checked;
end;

//------------------------------------------------------------------------------

procedure TCustomAdvTaskDialog.InitRadioButton(AOwner: TForm; Btn: TWinControl; btnIndex: Integer; OnClickEvent : TNotifyEvent);
begin
   with TRadioButton(Btn) do
   begin
     Name := 'Radio' + inttostr(btnIndex);
     Parent := AOwner;
     Font.Name := AOwner.Canvas.Font.Name;
     Font.Size := 8;
     BiDiMode := AOwner.BiDiMode;
     OnClick := OnClickEvent;

     {
     BoundsRect := TextRect;
     Left := FHorzParaMargin + FHorzMargin; //ALeft + FHorzMargin;
     Top := Y;
     Width := Self.Width - Left - 4;
     GetTextSize(Canvas, Caption, k, l);
     w := Max(w, Left + k + FHorzMargin + 20);
     }
   end;
end;

//------------------------------------------------------------------------------

procedure TCustomAdvTaskDialog.TaskDialogFormCreated(Sender: TObject);
begin
  hwnd := FDialogForm.Handle;
  if Assigned(OnDialogCreated) then
    OnDialogCreated(Self);
end;

//------------------------------------------------------------------------------

procedure TCustomAdvTaskDialog.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;
  if not (csDestroying in ComponentState) then
  begin
    if (AOperation = opRemove) then
    begin
      if (AComponent = FInputControl) then
        FInputControl := nil;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomAdvTaskDialog.ElevateButton(ButtonID: integer;
  Enabled: boolean);
begin
  SendMessage(hWnd, TDM_SET_BUTTON_ELEVATION_REQUIRED_STATE, ButtonID, integer(Enabled));
end;

procedure TCustomAdvTaskDialog.EnableButton(ButtonID: integer; Enabled: boolean);
begin
  SendMessage(hWnd, TDM_ENABLE_BUTTON, ButtonID, integer(Enabled));
  if Assigned(FDialogForm) then
    FDialogForm.EnableButton(ButtonID, Enabled);
end;

//------------------------------------------------------------------------------

function ConvertNL(s: string): string;
begin
  if Pos('\\n', s) > 0 then
    Result := StringReplace(s, '\\n', '\n', [rfReplaceAll])
  else
  begin
    if pos('\n',s) > 0 then
      Result := StringReplace(s,'\n',#10,[rfReplaceAll])
     else
      Result := s;
  end;
end;

function TCustomAdvTaskDialog.Execute: integer;
begin
  Result := ExecutePos(-1,-1);
end;

//------------------------------------------------------------------------------

function TCustomAdvTaskDialog.ExecutePos(X,Y: integer): integer;
var
  verinfo: TOSVersionInfo;
  DLLHandle: THandle;
  res,radiores: integer;
  verify: boolean;
  sz: integer;
  TaskDialogConfig: TTASKDIALOGCONFIG;

  TaskDialogIndirectProc : function(AConfig: PTASKDIALOGCONFIG; Res: pinteger;  ResRadio: pinteger; VerifyFLag: pboolean): integer cdecl stdcall;
{
  wTitle: TTaskDialogWideString;
  wDesc: TTaskDialogWideString;
  wContent: TTaskDialogWideString;
  wExpanded: TTaskDialogWideString;
  wExpandedControl: TTaskDialogWideString;
  wCollapsedControl: TTaskDialogWideString;
  wFooter: TTaskDialogWideString;
  wVerifyText: TTaskDialogWideString;
}
  TBA: TTaskDialogButtonArray;
  TBWS: TTaskDialogWideStringArray;
  i: integer;

  TRA: TTaskDialogButtonArray;
  TRWS: TTaskDialogWideStringArray;
  ComCtlVersion: integer;


begin
  Result := -1;

  FIsNative := false;
  FVerifyResult := false;

  FAutoCloseCounter := 0;

  VerInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  GetVersionEx(verinfo);

  ComCtlVersion := GetFileVersion('COMCTL32.DLL');
  ComCtlVersion := (ComCtlVersion shr 16) and $FF;

  if (verinfo.dwMajorVersion >= 6) and (ComCtlVersion > 5) and (FNonNativeDialog = nndAuto) then
  begin
    // check COMCTL version ...

    DLLHandle := LoadLibrary('comctl32.dll');
    if DLLHandle >= 32 then
    begin
      FIsNative := true;

      @TaskDialogIndirectProc := GetProcAddress(DLLHandle,'TaskDialogIndirect');

      if Assigned(TaskDialogIndirectProc) then
      begin
        sz := Sizeof(TTASKDIALOGCONFIG);

        FillChar(TaskDialogConfig, sz,0);
        TaskDialogConfig.cbSize := sz;

        if ModalParent <> 0 then
        begin
          TaskDialogConfig.hwndParent := ModalParent
        end
        else
        begin
          if Assigned(Self.Owner) and not ApplicationIsParent and (Self.Owner is TWinControl) then
            TaskDialogConfig.hwndParent := (Self.Owner as TWinControl).Handle
          else
          begin
            if Assigned(Screen) and Assigned(Screen.ActiveForm) and (Screen.ActiveForm.HandleAllocated) then
              TaskDialogConfig.hwndParent := Screen.ActiveForm.Handle
            else
              TaskDialogConfig.hwndParent := Application.Handle;
          end;
        end;

        if FCustomButtons.Count > 0 then
        begin
          SetLength(TBA, FCustomButtons.Count);
          SetLength(TBWS, FCustomButtons.Count);

          for i := 0 to FCustomButtons.Count - 1 do
          begin
            StringToWideChar(ConvertNL(FCustomButtons.Strings[i]), TBWS[i], sizeof(TBWS[i]));
            TBA[i].pszButtonText := TBWS[i];
            TBA[i].nButtonID := i + 100;
          end;

          TaskDialogConfig.cButtons := FCustomButtons.Count;
          TaskDialogConfig.pbuttons := @TBA[0];
        end;

        if FRadioButtons.Count > 0 then
        begin
          SetLength(TRA, FRadioButtons.Count);
          SetLength(TRWS, FRadioButtons.Count);

          for i := 0 to FRadioButtons.Count - 1 do
          begin
            StringToWideChar(ConvertNL(FRadioButtons.Strings[i]), TRWS[i], sizeof(TRWS[i]));
            TRA[i].pszButtonText := TRWS[i];
            TRA[i].nButtonID := i + 200;
          end;

          TaskDialogConfig.cRadioButtons := FRadioButtons.Count;
          TaskDialogConfig.pRadioButtons := @TRA[0];
        end;

        if FTitle <> '' then
        begin
          TaskDialogConfig.pszWindowTitle := PWideChar(WideString(ConvertNL(FTitle)));
        end;

        if FInstruction <> '' then
        begin
          TaskDialogConfig.pszMainInstruction := PWideChar(WideString(ConvertNL(FInstruction)));
        end;

        if FContent <> '' then
        begin
          TaskDialogConfig.pszContent := PWideChar(WideString(ConvertNL(FContent)));
        end;

        if FFooter <> '' then
        begin
          TaskDialogConfig.pszFooter := PWideChar(WideString(ConvertNL(FFooter)));
        end;

        if FExpandControlText <> '' then
        begin
          TaskDialogConfig.pszExpandedControlText := PWideChar(WideString(FExpandControlText));
        end;

        if FCollapsControlText <> '' then
        begin
          TaskDialogConfig.pszCollapsedControlText := PWideChar(WideString(FCollapsControlText));
        end;

        if FExpandedText <> '' then
        begin
          TaskDialogConfig.pszExpandedInformation := PWideChar(WideString(FExpandedText))
        end;

        if FVerifyText <> '' then
        begin
          TaskDialogConfig.pszVerificationText := PWideChar(WideString(FVerifyText));
        end;

        if cbOk in FCommonButtons then
          TaskDialogConfig.dwCommonButtons := TaskDialogConfig.dwCommonButtons or TDCBF_OK_BUTTON;

        if cbYes in FCommonButtons then
          TaskDialogConfig.dwCommonButtons := TaskDialogConfig.dwCommonButtons or TDCBF_YES_BUTTON;

        if cbNo in FCommonButtons then
          TaskDialogConfig.dwCommonButtons := TaskDialogConfig.dwCommonButtons or TDCBF_NO_BUTTON;

        if cbCancel in FCommonButtons then
          TaskDialogConfig.dwCommonButtons := TaskDialogConfig.dwCommonButtons or TDCBF_CANCEL_BUTTON;

        if cbClose in FCommonButtons then
          TaskDialogConfig.dwCommonButtons := TaskDialogConfig.dwCommonButtons or TDCBF_CLOSE_BUTTON;

        if cbRetry in FCommonButtons then
          TaskDialogConfig.dwCommonButtons := TaskDialogConfig.dwCommonButtons or TDCBF_RETRY_BUTTON;

        if (doCommandLinks in FOptions) and (CustomButtons.Count > 0) then
          TaskDialogConfig.dwFlags := TDF_USE_COMMAND_LINKS;

        if doCommandLinksNoIcon in FOptions then
          TaskDialogConfig.dwFlags := TDF_USE_COMMAND_LINKS_NO_ICON;

        if doHyperlinks in FOptions then
          TaskDialogConfig.dwFlags := TaskDialogConfig.dwFlags or TDF_ENABLE_HYPERLINKS;

        if doExpandedDefault in FOptions then
          TaskDialogConfig.dwFlags := TaskDialogConfig.dwFlags or TDF_EXPANDED_BY_DEFAULT;

        if doExpandedFooter in FOptions then
          TaskDialogConfig.dwFlags := TaskDialogConfig.dwFlags or TDF_EXPAND_FOOTER_AREA;

        if doAllowMinimize in FOptions then
          TaskDialogConfig.dwFlags := TaskDialogConfig.dwFlags or TDF_CAN_BE_MINIMIZED;

        if doVerifyChecked in FOptions then
          TaskDialogConfig.dwFlags := TaskDialogConfig.dwFlags or TDF_VERIFICATION_FLAG_CHECKED;

        if doProgressBar in FOptions then
          TaskDialogConfig.dwFlags := TaskDialogConfig.dwFlags or TDF_SHOW_PROGRESS_BAR;

        if doProgressBarMarquee in FOptions then
          TaskDialogConfig.dwFlags := TaskDialogConfig.dwFlags or TDF_SHOW_MARQUEE_PROGRESS_BAR;

        if doSizeToContent in FOptions then
          TaskDialogConfig.dwFlags := TaskDialogConfig.dwFlags or TDF_SIZE_TO_CONTENT;

        if (doProgressBarMarquee in FOptions) or
           (doProgressBar in FOptions) or
           (doTimer in FOptions) or
           (AutoClose) then
           TaskDialogConfig.dwFlags := TaskDialogConfig.dwFlags or TDF_CALLBACK_TIMER;

        if (DialogPosition = dpOwnerFormCenter) then
          TaskDialogConfig.dwFlags := TaskDialogConfig.dwFlags or TDF_POSITION_RELATIVE_TO_WINDOW;

        if doNoDefaultRadioButton in FOptions then
          TaskDialogConfig.dwFlags := TaskDialogConfig.dwFlags or TDF_NO_DEFAULT_RADIO_BUTTON;

        if doAllowDialogCancel in FOptions then
          TaskDialogConfig.dwFlags := TaskDialogConfig.dwFlags or TDF_ALLOW_DIALOG_CANCELLATION;

        TaskDialogConfig.hInstance := 0;

        if not CustomIcon.Empty then
        begin
          TaskDialogConfig.hMainIcon := CustomIcon.Handle;
          TaskDialogConfig.dwFlags := TaskDialogConfig.dwFlags or TDF_USE_HICON_MAIN;
        end
        else
        begin
          case Icon of
            tiWarning: TaskDialogConfig.hMainIcon := TD_ICON_WARNING;
            tiQuestion: TaskDialogConfig.hMainIcon := TD_ICON_QUESTION;
            tiError: TaskDialogConfig.hMainIcon := TD_ICON_ERROR;
            tiShield: TaskDialogConfig.hMainIcon := TD_ICON_SHIELD;
            tiBlank: TaskDialogConfig.hMainIcon := TD_ICON_BLANK;
            tiInformation: TaskDialogConfig.hMainIcon := TD_ICON_INFORMATION;
          end;
        end;

        case FooterIcon of
          tfiWarning: TaskDialogConfig.hFooterIcon := TD_ICON_WARNING;
          tfiQuestion: TaskDialogConfig.hFooterIcon := TD_ICON_QUESTION;
          tfiError: TaskDialogConfig.hFooterIcon := TD_ICON_ERROR;
          tfiInformation: TaskDialogConfig.hFooterIcon := THandle(MAKEINTRESOURCEW(Word(-3)));
          tfiShield: TaskDialogConfig.hFooterIcon := THandle(MAKEINTRESOURCEW(Word(-4)));
        end;

        TaskDialogConfig.pfCallBack := @TaskDialogCallbackProc;
        TaskDialogConfig.pData := Self;
        TaskDialogConfig.nDefaultButton := DefaultButton;
        TaskDialogConfig.nDefaultRadioButton := DefaultRadioButton;

        TaskDialogIndirectProc(@TaskDialogConfig, @res, @radiores, @verify);

        RadioButtonResult := radiores;
        VerifyResult := verify;
        Result := res;

      end;
    end;
  end
  else
    Result := AdvMessageDlgPos(Self, X, Y);
end;

//------------------------------------------------------------------------------

function TCustomAdvTaskDialog.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn))) + '.' + IntToStr(Lo(Hiword(vn))) + '.' +
    IntToStr(Hi(Loword(vn))) + '.' + IntToStr(Lo(Loword(vn)));
end;

//------------------------------------------------------------------------------

function TCustomAdvTaskDialog.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER, REL_VER), MakeWord(MIN_VER, MAJ_VER));
end;

//------------------------------------------------------------------------------

procedure TCustomAdvTaskDialog.SetContent(const Value: string);
begin
  if (FContent <> Value) then
  begin
    FContent := Value;
    SendMessage(hWnd, TDM_UPDATE_ELEMENT_TEXT, TDE_CONTENT, LParam(PWideChar(WideString(FContent))));
    if Assigned(FDialogForm) then
      FDialogForm.UpdateDialog;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomAdvTaskDialog.SetCustomButtons(const Value: TStringList);
begin
  FCustomButtons.Assign(Value);
end;

procedure TCustomAdvTaskDialog.SetCustomIcon(const Value: TIcon);
begin
  FCustomIcon.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TCustomAdvTaskDialog.SetExpandedText(const Value: string);
begin
  if (FExpandedText <> Value) then
  begin
    FExpandedText := Value;
    SendMessage(hWnd, TDM_UPDATE_ELEMENT_TEXT, TDE_EXPANDED_INFORMATION, LParam(PWideChar(WideString(FExpandedText))));
    if Assigned(FDialogForm) then
      FDialogForm.UpdateDialog;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomAdvTaskDialog.SetFooter(const Value: string);
begin
  if (FFooter <> Value) then
  begin
    FFooter := Value;
    SendMessage(hWnd, TDM_UPDATE_ELEMENT_TEXT, TDE_FOOTER, LParam(PWideChar(WideString(FFooter))));
    if Assigned(FDialogForm) then
      FDialogForm.UpdateDialog;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomAdvTaskDialog.SetInputItems(const Value: TStrings);
begin
  FInputItems.Assign(Value);
end;

procedure TCustomAdvTaskDialog.SetInstruction(const Value: string);
begin
  if (FInstruction <> Value) then
  begin
    FInstruction := Value;
    SendMessage(hWnd, TDM_UPDATE_ELEMENT_TEXT, TDE_MAIN_INSTRUCTION, LParam(PWideChar(WideString(FInstruction))));
    if Assigned(FDialogForm) then
      FDialogForm.UpdateDialog;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomAdvTaskDialog.SetRadioButtonCaption(Btn: TWinControl;
  Value: string);
begin
  TRadioButton(Btn).Caption := Value;
end;

//------------------------------------------------------------------------------

procedure TCustomAdvTaskDialog.SetRadioButtons(const Value: TStringList);
begin
  FRadioButtons.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TCustomAdvTaskDialog.SetVersion(const Value: string);
begin

end;

//------------------------------------------------------------------------------

procedure TCustomAdvTaskDialog.SetButtonCancel(aButton: TWinControl; Value: Boolean);
begin
  if not Assigned(aButton) or not (aButton is TButton) then
    Exit;

  TButton(aButton).Cancel := Value;
end;

//------------------------------------------------------------------------------

procedure TCustomAdvTaskDialog.SetButtonDefault(aButton: TWinControl; Value: Boolean);
begin
  if not Assigned(aButton) or not (aButton is TButton) then
    Exit;

  TButton(aButton).Default := Value;
end;

//------------------------------------------------------------------------------

procedure TCustomAdvTaskDialog.SetButtonModalResult(aButton: TWinControl; Value: Integer);
begin
  if not Assigned(aButton) or not (aButton is TButton) then
    Exit;

  TButton(aButton).ModalResult := Value;
end;

//------------------------------------------------------------------------------

function TCustomAdvTaskDialog.GetButtonModalResult(
  aButton: TWinControl): Integer;
begin
  Result := mrNone;
  if not Assigned(aButton) or not (aButton is TButton) then
    Exit;

  Result := TButton(aButton).ModalResult;
end;

//------------------------------------------------------------------------------

procedure TCustomAdvTaskDialog.SetButtonCaption(aButton: TWinControl;
  Value: TCaption);
var
  btn: TButton;

begin
  if not Assigned(aButton) or not (aButton is TButton) then
    Exit;

  btn := (aButton as TButton);
  btn.Caption := Value;
end;


procedure TCustomAdvTaskDialog.InitVCLStyle(init: boolean);
{$IFDEF DELPHIXE2_LVL}
var
  LStyle: TCustomStyleServices;
  LDetails: TThemedElementDetails;
  clr: TColor;
{$ENDIF}
begin
{$IFDEF DELPHIXE2_LVL}
  LStyle := StyleServices;

  if LStyle.Enabled and (LStyle.Name <> 'Windows') then
  begin
    LDetails := LStyle.GetElementDetails(tgCellNormal);
    if LStyle.GetElementColor(LDetails, ecTextColor, clr) and (clr <> clNone) then
      FNormalFontColor := clr;
  end
  else
    FNormalFontColor := clWindowText;
{$ENDIF}
end;


//------------------------------------------------------------------------------

{ TAdvTaskDialog }
function TAdvTaskDialog.Execute: integer;
begin
  Result := inherited Execute;
end;

//------------------------------------------------------------------------------

{ TTaskDialogButton }

constructor TTaskDialogButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FGlyph := TBitmap.Create;
  FGlyph.OnChange := OnPictureChanged;

  FGlyphHot := TBitmap.Create;

  FGlyphDown := TBitmap.Create;

  FGlyphDisabled := TBitmap.Create;
  FGlyphDisabled.OnChange := OnPictureChanged;

  FHeadingFont := TFont.Create;

  SetBounds(0, 0, 23, 22);
  ShowHint := False;
  FBorderColorDown := clNone;
  FBorderColorHot := clNone;
  FBorderColor := clNone;
  FBackColor := clNone;
  FMultiLine := true;
end;

//------------------------------------------------------------------------------

destructor TTaskDialogButton.Destroy;
begin
  FGlyph.Free;
  FGlyphHot.Free;
  FGlyphDown.Free;
  FGlyphDisabled.Free;
  FHeadingFont.Free;
  inherited;
end;

procedure TTaskDialogButton.DoEnter;
begin
  inherited;
  Invalidate;
end;

procedure TTaskDialogButton.DoExit;
begin
  inherited;
  Invalidate;
end;

procedure TTaskDialogButton.KeyPress(var Key: char);
begin
  inherited;
  if (Key = #32) or (Key = #13) then
  begin
    Click;
  end;
end;

//------------------------------------------------------------------------------

procedure TTaskDialogButton.Paint;
var
  Pic: TBitmap;
  x, y, bw, bh, i: Integer;
  R, TR: TRect;
  BrClr: TColor;
  SL: TStringList;
begin
  inherited;

  R := ClientRect;

  if BackColor <> clNone then
  begin
    Canvas.Brush.Color := BackColor;
    Canvas.Pen.Color := BackColor;
    Canvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
  end;

  BrClr := clNone;

  if FMouseDown then
    BrClr := BorderColorDown
  else if FMouseInControl then
    BrClr := BorderColorHot;

  if not Enabled then
    BrClr := clNone;

  if GetFocus = Handle then
    BrClr := BorderColorDown;

  Pic := Picture;
  if FMouseDown and not FGlyphDown.Empty then
    Pic := FGlyphDown
  else if FMouseInControl and not FGlyphHot.Empty then
    Pic := FGlyphHot;

  if not Enabled and not PictureDisabled.Empty then
    Pic := PictureDisabled;

  if Assigned(Pic) and not Pic.Empty then
  begin
    Pic.Transparent := True;
    if (Caption = '') then
    begin
      x := (Width - Pic.Width) div 2;
      y := (Height - Pic.Height) div 2;
    end
    else
    begin
      x := 4;
      y := (Height - Pic.Height) div 2;
    end;

    Canvas.Draw(x, y, Pic);
    R.Left := x + Pic.Width + 3;
  end
  else
    R.Left := R.Left + 2;

  if (Caption <> '') then
  begin
    if MultiLine and (HasLf(Caption) or (pos(#13, Caption) > 0)) then
    begin
      TR := R;
      SL := TStringList.Create;
      SplitInToLines(Caption, SL);
      GetMultiLineTextSize(Canvas, Caption, HeadingFont, Self.Font, DrawTextBiDiModeFlagsReadingOnly, bw, bh, MultiLine);
      TR.Top := 2 + (Height - bh) div 2;

      Canvas.Brush.Style := bsClear;
      if (SL[0] <> '') then
      begin
        Canvas.Font.Assign(HeadingFont);

        if not Enabled then
          Canvas.Font.Color := clSilver;

        DrawText(Canvas.Handle, PChar(SL[0]),Length(SL[0]), TR, DT_LEFT or DT_TOP or DT_SINGLELINE);
        TR.Top := TR.Top + Canvas.TextHeight('gh') + 4;
      end;

      Canvas.Font.Assign(Self.Font);

      if not Enabled then
        Canvas.Font.Color := clSilver;

      for i:= 1 to SL.Count - 1 do
      begin
        DrawText(Canvas.Handle, PChar(SL[i]),Length(SL[i]), TR, DT_LEFT or DT_TOP or DT_SINGLELINE);
        TR.Top := TR.Top + Canvas.TextHeight('gh') + 2;
      end;
      SL.Free;
    end
    else
    begin
      Canvas.Brush.Style := bsClear;
      Canvas.Font.Assign(HeadingFont);
      if not Enabled then
        Canvas.Font.Color := clSilver;
      DrawText(Canvas.Handle,PChar(Caption),Length(Caption), R, DT_LEFT or DT_VCENTER or DT_SINGLELINE);
    end;
  end;

  if (BrClr <> clNone) then
  begin
    R := ClientRect;
    Canvas.Pen.Color := BrClr;
    Canvas.Brush.Style := bsClear;
    Canvas.RoundRect(R.Left, R.Top, R.Right, R.Bottom, 2, 2);
  end;
end;

//------------------------------------------------------------------------------

procedure TTaskDialogButton.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  
  if (ssLeft in Shift) then
  begin
    FMouseDown := True;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TTaskDialogButton.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;

end;

//------------------------------------------------------------------------------

procedure TTaskDialogButton.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  FMouseDown := False;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TTaskDialogButton.SetGlyph(const Value: TBitmap);
begin
  FGlyph.Assign(Value);
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TTaskDialogButton.SetGlyphDown(const Value: TBitmap);
begin
  FGlyphDown.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TTaskDialogButton.SetGlyphHot(const Value: TBitmap);
begin
  FGlyphHot.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TTaskDialogButton.SetGlyphDisabled(const Value: TBitmap);
begin
  FGlyphDisabled.Assign(Value);
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TTaskDialogButton.OnPictureChanged(Sender: TObject);
begin
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TTaskDialogButton.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  FMouseInControl := True;

  if AutoFocus then
    SetFocus;

  Invalidate;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

//------------------------------------------------------------------------------

procedure TTaskDialogButton.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  FMouseInControl := False;
  Invalidate;

  if Assigned(FOnMouseLeave) then
     FOnMouseLeave(Self);
end;

//------------------------------------------------------------------------------

procedure TTaskDialogButton.SetHeadingFont(const Value: TFont);
begin
  FHeadingFont.Assign(Value);
end;

//------------------------------------------------------------------------------

function GetAveCharSize(Canvas: TCanvas): TPoint;
var
  I: Integer;
  Buffer: array[0..51] of Char;
begin
  for I := 0 to 25 do Buffer[I] := Chr(I + Ord('A'));
  for I := 0 to 25 do Buffer[I + 26] := Chr(I + Ord('a'));
  GetTextExtentPoint(Canvas.Handle, Buffer, 52, TSize(Result));
  Result.X := Result.X div 52;
end;

//------------------------------------------------------------------------------

var
  ButtonWidths : array[TCommonButton] of integer;  // initialized to zero
                                                //tiBlank, tiWarning, tiQuestion, tiError, tiInformation,tiNotUsed,tiShield
  IconIDs: array[TTaskDialogIcon] of PChar = (IDI_ASTERISK, IDI_EXCLAMATION, IDI_QUESTION, IDI_ERROR, IDI_INFORMATION, nil, IDI_HAND);
  FooterIconIDs: array[TTaskDialogFooterIcon] of PChar = (nil, IDI_EXCLAMATION, IDI_QUESTION, IDI_HAND, IDI_INFORMATION, IDI_WINLOGO);
  Captions: array[TTaskDialogIcon] of Pointer;
  // = (nil, @SMsgDlgWarning, @SMsgDlgConfirm, @SMsgDlgError, @SMsgDlgInformation);
  ModalResults: array[TCommonButton] of Integer = (mrOk, mrYes, mrNo, mrCancel, mrRetry, mrAbort);
  //(tiBlank, tiWarning, tiQuestion, tiError, tiShield);
  //(mtWarning, mtError, mtInformation, mtConfirmation, mtCustom);

function CreateAdvMessageDlg(TaskDialog: TCustomAdvTaskDialog): TForm;
begin
  Result := nil;
  if not Assigned(TaskDialog) then
    Exit;

  if TaskDialog.ApplicationIsParent then
    Result := TAdvMessageForm.CreateNew(Application,0)
  else
  begin
    if Assigned(TaskDialog.Owner) and (TaskDialog.Owner is TCustomForm) then
      Result := TAdvMessageForm.CreateNew((TaskDialog.Owner) as TCustomForm,0)
    else
      Result := TAdvMessageForm.CreateNew(Application,0);
  end;

  with Result do
  begin
    BiDiMode := Application.BiDiMode;
    BorderIcons := [];

    if doAllowMinimize in TaskDialog.Options then
    begin
      BorderStyle := bsSingle;
      BorderIcons := [biSystemMenu,biMinimize]
    end
    else
    begin
      BorderStyle := bsDialog;
    end;

    if cbCancel in TaskDialog.CommonButtons then
      TaskDialog.Options := TaskDialog.Options + [doAllowDialogCancel];

    if doAllowDialogCancel in TaskDialog.Options then
    begin
      BorderIcons := BorderIcons + [biSystemMenu];
    end;

    if not TaskDialog.ApplicationIsParent then
    begin
      if ((TaskDialog.Owner) is TForm) then
        if ((TaskDialog.Owner) as TForm).FormStyle = fsStayOnTop then
          FormStyle := fsStayOnTop;
    end;

    if TaskDialog.AlwaysOnTop then 
      FormStyle := fsStayOnTop;

    Canvas.Font := Font;
    KeyPreview := True;
    OnKeyDown := TAdvMessageForm(Result).CustomKeyDown;
  end;
  //TaskDialog.Options := TaskDialog.Options + [doAllowDialogCancel];
  TAdvMessageForm(Result).FooterColor := TaskDialog.FooterColor;
  TAdvMessageForm(Result).FooterLineColor := TaskDialog.FooterLineColor;
  TAdvMessageForm(Result).MinFormWidth := TaskDialog.NonNativeMinFormWidth;
  TAdvMessageForm(Result).BuildTaskDialog(TaskDialog);
end;

//------------------------------------------------------------------------------

function AdvMessageDlgPos(TaskDialog: TCustomAdvTaskDialog; X, Y: Integer): Integer;
var
  DlgForm: TAdvMessageForm;
begin
  Result := -1;
  if not Assigned(TaskDialog) then
    Exit;

  DlgForm := TAdvMessageForm(CreateAdvMessageDlg(TaskDialog));

  DlgForm.OnShow := TaskDialog.TaskDialogFormCreated;

  TaskDialog.InitVCLStyle(true);

  TaskDialog.FDialogForm := DlgForm;

  with DlgForm do
    try
      Color := clWindow;
      //HelpContext := HelpCtx;
      //HelpFile := HelpFileName;
      if X >= 0 then Left := X;
      if Y >= 0 then Top := Y;
      if TaskDialog.DialogPosition = dpOwnerFormCenter then
      begin
        if (Y < 0) and (X < 0) then
          Position := poOwnerFormCenter;
      end
      else
      begin
        DefaultMonitor := dmMainForm;
        if (Y < 0) and (X < 0) then
          Position := poScreenCenter;
      end;
      Result := ShowModal;
      Close;
    finally
      TaskDialog.FDialogForm := nil;
      Free;
    end;
end;

//------------------------------------------------------------------------------

procedure TAdvMessageForm.GetTextSize(Canvas: TCanvas; Text: string;var W, H: Integer);
var
  R: TRect;
begin
  if (Text = '') then
  begin
    W := 0;
    H := 0;
    Exit;
  end;

  if Assigned(Canvas) then
  begin
    if W = 0 then
      SetRect(R, 0, 0, 1000, 100)
    else
      SetRect(R, 0, 0, W, 100);

    DrawText(Canvas.Handle, PChar(Text), Length(Text)+1, R,
      DT_EXPANDTABS or DT_CALCRECT or DT_WORDBREAK or DT_NOPREFIX or
      DrawTextBiDiModeFlagsReadingOnly);

    W := R.Right;
    H := R.Bottom;
  end;
end;

//------------------------------------------------------------------------------

const
  mcHorzMargin = 8;
  mcVertMargin = 8;
  mcHorzSpacing = 8;
  mcVertSpacing = 8;
  mcButtonWidth = 50;
  mcButtonHeight = 14;
  mcButtonSpacing = 4;

function GetExeName: string;
var
  s: string;
  fe: string;
begin
  s := ExtractFileName(Application.EXEName);
  fe := ExtractFileExt(s);
  if (Length(fe) > 0) then
    delete(s, length(s) - Length(fe) + 1, length(fe));
  Result := s;
end;

procedure TAdvMessageForm.BuildTaskDialog(TaskDialog: TCustomAdvTaskDialog);
var
  DialogUnits: TPoint;
  ButtonWidth, ButtonHeight, ButtonSpacing, ButtonCount, ButtonGroupWidth,
  IconTextWidth, X, Y, ALeft: Integer;
  B, DefaultButton, CancelButton: TCommonButton;
  IconID: PChar;
  TextRect: TRect;
  Msg: string;
  DlgType: TTaskDialogIcon;
  Buttons: TCommonButtons;
  i, bw, bh, w, j: Integer;
  CmBtnGroupWidth, CsBtnGroupWidth: Integer;
  r, re: trect;
  anchor, stripped: string;
  HyperLinks,MouseLink, k, l, n: Integer;
  Focusanchor: string;
  OldFont, hf, pf: TFont;
  verifTextWidth: Integer;
  v: Boolean;
  szExpandedText,szFooterText: string;
  defIdx: integer;
  trydate: TDateTime;

begin
  if not Assigned(TaskDialog) then
    Exit;

  FTaskDialog := TaskDialog;
  Msg := TaskDialog.Instruction;
  DlgType := TaskDialog.Icon;
  Buttons := TaskDialog.CommonButtons;

  OldFont := TFont.Create;
  OldFont.Assign(Canvas.Font);

  DialogUnits := GetAveCharSize(Canvas);
  FHorzMargin := MulDiv(mcHorzMargin, DialogUnits.X, 4);
  FVertMargin := MulDiv(mcVertMargin, DialogUnits.Y, 8);
  FHorzSpacing := MulDiv(mcHorzSpacing, DialogUnits.X, 4);
  FVertSpacing := MulDiv(mcVertSpacing, DialogUnits.Y, 8);

  w := 0;
  x := 0;

  if TaskDialog.Title <> '' then
    Caption := TaskDialog.Title
  else
    Caption := GetExeName;

  if (Caption <> '') then
  begin
    w := 1000;
    GetTextSize(Canvas, Caption, w, l);
    w := w + 50;
  end;

  ButtonWidth := MulDiv(mcButtonWidth, DialogUnits.X, 4);
  ButtonHeight := MulDiv(mcButtonHeight, DialogUnits.Y, 8) - 2;
  ButtonSpacing := MulDiv(mcButtonSpacing, DialogUnits.X, 4);
  CmBtnGroupWidth := 0;
  CsBtnGroupWidth := 0;
  ButtonCount := 0;
  FHorzParaMargin := FHorzMargin;
  Y := FVertMargin;
  FcmBtnList.Clear;

  DefaultButton := cbOk;
  CancelButton := cbCancel;

  if TaskDialog.DefaultButton <> -1 then
  begin

    if TaskDialog.DefaultButton = 0 then
    begin
      if (cbOk in Buttons) then DefaultButton := cbOk else
        if cbYes in Buttons then DefaultButton := cbYes else
          DefaultButton := cbRetry;
      if cbCancel in Buttons then CancelButton := cbCancel else
        if cbNo in Buttons then CancelButton := cbNo else
          CancelButton := cbOk;
    end
    else
    begin
      case TaskDialog.DefaultButton of
      1: if (cbOk in Buttons) then DefaultButton := cbOK
         else
           DefaultButton := cbYes;
      2: if (cbCancel in Buttons) then DefaultButton := cbCancel
         else
           DefaultButton := cbNo;
      6: if (cbYes in Buttons) then DefaultButton := cbYes;
      7: if (cbNo in Buttons) then DefaultButton := cbNo;
      IDCLOSE: if (cbClose in Buttons) then DefaultButton := cbClose;
      end;
    end;
  end;

  for B := Low(TCommonButton) to High(TCommonButton) do
  begin
    if B in Buttons then
    begin
      if ButtonWidths[B] = 0 then
      begin
        TextRect := Rect(0,0,0,0);
        Windows.DrawText(Canvas.Handle,
          PChar(LoadResString(ButtonCaptions[B])), -1,
          TextRect, DT_CALCRECT or DT_LEFT or DT_SINGLELINE or
          DrawTextBiDiModeFlagsReadingOnly);
        with TextRect do
          ButtonWidths[B] := Right - Left + 16;
      end;

      if ButtonWidths[B] > ButtonWidth then
        ButtonWidth := ButtonWidths[B];

      i := FcmBtnList.Add(TaskDialog.CreateButton(Self));

      with TWinControl(FcmBtnList.Items[i]) do
      begin
        Name := ButtonNames[B];
        Parent := Self;

        TaskDialog.SetButtonCaption(TWinControl(FcmBtnList.Items[i]), LoadResString(ButtonCaptions[B]));

        TaskDialog.SetButtonModalResult(TWinControl(FcmBtnList.Items[i]), ModalResults[B]);
        //ModalResult := ModalResults[B];
        if  B = DefaultButton then
          Tag := 1
        else
          Tag := 0;

        if (TaskDialog.GetButtonModalResult(TWinControl(FcmBtnList.Items[i])) = mrCancel) and
           (doAllowDialogCancel in TaskDialog.Options) then
          TaskDialog.SetButtonCancel(TWinControl(FcmBtnList.Items[i]), True);
          //Cancel := true;

        if (TaskDialog.DefaultButton <> -1) then
        begin
          if (B = DefaultButton) then
          begin
            //Default := True;
            TaskDialog.SetButtonDefault(TWinControl(FcmBtnList.Items[i]), True);
            TabOrder := 0;
          end;
        end;

        if (B = CancelButton) and (doAllowDialogCancel in TaskDialog.Options)  then
          TaskDialog.SetButtonCancel(TWinControl(FcmBtnList.Items[i]), True);

        Width := Max(70, ButtonWidths[B]);
        Height := ButtonHeight;
        cmBtnGroupWidth := cmBtnGroupWidth + Width + ButtonSpacing;
        //if B = mbHelp then
          //OnClick := TMessageForm(Result).HelpButtonClick;

        TProControl(FcmBtnList.Items[i]).OnClick := OnButtonClick;
//        OnClick := OnButtonClick;
        if TaskDialog.DefaultButton = -1 then
          TabStop := false;
      end;
      //Inc(ButtonCount);
    end;
  end;

  FcsBtnList.Clear;
  if not (docommandLinks in TaskDialog.Options) then
  begin
    for i := 0 to TaskDialog.CustomButtons.Count - 1 do
    begin
      TextRect := Rect(0,0,0,0);
      Windows.DrawText(Canvas.Handle,
        PChar(TaskDialog.CustomButtons[i]), -1,
        TextRect, DT_CALCRECT or DT_LEFT or DT_SINGLELINE or
        DrawTextBiDiModeFlagsReadingOnly);

      with TextRect do
        bw := Right - Left + 16;

      if bw > ButtonWidth then
        ButtonWidth := bw;

      defIdx := TaskDialog.DefaultButton;
      // force first custom button as default button
      if (Buttons = []) and (TaskDialog.DefaultButton <= 0) then
        defIdx := 100;

      j := FcsBtnList.Add(TaskDialog.CreateButton(Self));

      with TWinControl(FcsBtnList.Items[j]) do
      begin
        Name := 'CSButton'+inttostr(i);
        Parent := Self;

        //ModalResult := i + 100; //mrAbort;
        TaskDialog.SetButtonModalResult(TWinControl(FcsBtnList.Items[j]), i + 100);
        v := (TaskDialog.GetButtonModalResult(TWinControl(FcsBtnList.Items[j])) = defIdx);
        TaskDialog.SetButtonDefault(TWinControl(FcsBtnList.Items[j]), V);

        TaskDialog.SetButtonCaption(TWinControl(FcsBtnList.Items[j]), TaskDialog.CustomButtons[i]);

        //Default := (ModalResult = TaskDialog.DefaultButton);
        //if V then
        //  TabOrder := 0;
        //if B = DefaultButton then Default := True;
        //if B = CancelButton then Cancel := True;
        Width := Max(70, bw);
        Height := ButtonHeight;
        TProControl(FcsBtnList.Items[j]).OnClick := OnButtonClick;
        CsBtnGroupWidth := CsBtnGroupWidth + Width + ButtonSpacing;
        if defIdx = -1 then
          TabStop := false;
      end;

    end;
  end
  else
  begin
    n := 0;
    hf := TFont.Create;
    pf := TFont.Create;
    hf.Assign(Canvas.Font);
    hf.Size := 11;
    hf.Style := [fsBold];
    pf.Assign(Canvas.Font);


    for i := 0 to TaskDialog.CustomButtons.Count-1 do
    begin
      Canvas.Font.Size := 10;
      Canvas.Font.Style := [];
      bw := 0;
      bh := 0;
      GetMultiLineTextSize(Canvas, TaskDialog.CustomButtons[i], Hf, Pf, DrawTextBiDiModeFlagsReadingOnly, bw, bh, TaskDialog.MultiLineButtons);

      {TextRect := Rect(0,0,0,0);
      Windows.DrawText( Canvas.handle,
        PChar(TaskDialog.CustomButtons[i]), -1,
        TextRect, DT_CALCRECT or DT_LEFT or DT_SINGLELINE or
        DrawTextBiDiModeFlagsReadingOnly);
      with TextRect do bw := (Right - Left) + 8 + 18;}
      bw := bw + 26;
      if bw > ButtonWidth then
        ButtonWidth := bw;

      if bw > n then
        n := bw;

      if not (doCommandLinksNoIcon in TaskDialog.Options) then
        w := Max(w, n + FHorzMargin * 2 + FHorzSpacing + 32)
      else
        w := Max(w, n + FHorzMargin);

      j := FcsBtnList.Add(TTaskDialogButton.Create(Self));
      with TTaskDialogButton(FcsBtnList.Items[j]) do
      begin
        Name := 'Button' + IntToStr(i);
        Parent := Self;
        Caption := TaskDialog.CustomButtons[i];
        Font.Assign(pf);
        Font.Color := RGB(0, 83, 196);
        HeadingFont.Assign(hf);
        HeadingFont.Color := RGB(0, 83, 196);//RGB(21, 28, 85);
        ModalResult := i + 100; //mrAbort;
        //Default := (ModalResult = TaskDialog.DefaultButton);
        BorderColorHot := RGB(108, 225, 255);
        BorderColorDown := RGB(108, 225, 255);
        MultiLine := TaskDialog.MultiLineButtons;
        Width := Max(60, n);
        if TaskDialog.DefaultButton <> -1 then
          AutoFocus := true;

        Height := Max(bh, Max(ButtonHeight, Canvas.TextHeight('gh') + 20));

        if not (doCommandLinksNoIcon in TaskDialog.Options) then
        begin
          Picture.LoadFromResourceName(HInstance, 'TD_ARW');
          Picture.TransparentColor := clFuchsia;

          PictureHot.LoadFromResourceName(HInstance, 'TD_ARWHOT');
          PictureHot.TransparentColor := clFuchsia;

          PictureDown.LoadFromResourceName(HInstance, 'TD_ARWDOWN');
          PictureDown.TransparentColor := clFuchsia;

          PictureDisabled.LoadFromResourceName(HInstance, 'TD_ARWDIS');
          PictureDisabled.TransparentColor := clFuchsia;
        end;

        if TaskDialog.DefaultButton = -1 then
          TabStop := false
        else
          TabStop := true;

        OnClick := OnButtonClick;
        //CsBtnGroupWidth := CsBtnGroupWidth + Width + ButtonSpacing;
      end;

    end;
    Canvas.Font.Assign(OldFont);
    hf.Free;
    pf.Free;
  end;

  // if no button then OK button is added
  if (FcmBtnList.Count = 0) and (FcsBtnList.Count = 0) then
  begin
    b := cbOK;
    TextRect := Rect(0,0,0,0);
    Windows.DrawText(Canvas.Handle,
      PChar(LoadResString(ButtonCaptions[B])), -1,
      TextRect, DT_CALCRECT or DT_LEFT or DT_SINGLELINE or
      DrawTextBiDiModeFlagsReadingOnly);

    with TextRect do ButtonWidths[B] := Right - Left + 8;

    //if ButtonWidths[B] > ButtonWidth then
      //ButtonWidth := ButtonWidths[B];

    i := FcmBtnList.Add(TaskDialog.CreateButton(Self));
    with TWinControl(FcmBtnList.Items[i]) do
    begin
      Name := ButtonNames[B];
      Parent := Self;
      TaskDialog.SetButtonCaption(TWinControl(FcmBtnList.Items[i]), LoadResString(ButtonCaptions[B]));
      TaskDialog.SetButtonModalResult(TWinControl(FcmBtnList.Items[i]), ModalResults[B]);
      //ModalResult := ModalResults[B];
      //Default := True;
      TaskDialog.SetButtonDefault(TWinControl(FcmBtnList.Items[i]), True);

      //Cancel := True; // handle ESC

      TWinControl(FcmBtnList.Items[i]).Tag := 1;

      if doAllowDialogCancel in TaskDialog.Options then
        TaskDialog.SetButtonCancel(TWinControl(FcmBtnList.Items[i]), True);

      Width := Max(70, ButtonWidths[B]);
      Height := ButtonHeight;
      cmBtnGroupWidth := cmBtnGroupWidth + Width + ButtonSpacing;
      //if B = mbHelp then
        //OnClick := TMessageForm(Result).HelpButtonClick;
    end;
  end;

  // Instruction
  Canvas.Font.Size := 11;
  Canvas.Font.Style := [fsBold];

  SetRect(TextRect, 0, 0, Screen.Width div 2, 0);
  DrawText(Canvas.Handle, PChar(Msg), Length(Msg) + 1, TextRect,
    DT_EXPANDTABS or DT_CALCRECT or DT_WORDBREAK or
    DrawTextBiDiModeFlagsReadingOnly);

  Canvas.Font.Assign(OldFont);

  IconID := IconIDs[DlgType];

  IconTextWidth := TextRect.Right;
  //IconTextHeight := TextRect.Bottom;
  if IconID <> nil then
  begin
    Inc(IconTextWidth, 32 + FHorzSpacing);
  //  if IconTextHeight < 32 then IconTextHeight := 32;
  end;

  {if DlgType <> tiBlank then
    Caption := LoadResString(Captions[DlgType]) else
    Caption := Application.Title;}

  if ((IconID <> nil) or not (TaskDialog.CustomIcon.Empty)) {and not (doCommandLinksNoIcon in TaskDialog.Options)} then
  begin
    FIcon := TImage.Create(Self);
    with FIcon do
    begin
      Name := 'Image';
      Parent := Self;

      if not TaskDialog.CustomIcon.Empty then
      begin
        Picture.Icon.Assign(TaskDialog.CustomIcon);
      end
      else
      begin
        case TaskDialog.Icon of
        tiShield: Picture.Bitmap.Handle := LoadBitmap(hInstance, 'TD_SHIELD');
        tiBlank:
          begin
//            Picture.Bitmap.Height := 32;
//            Picture.Bitmap.Width := 32;
//            Picture.Bitmap.Canvas.Brush.Color := clWindow;
//            Picture.Bitmap.Canvas.Pen.Style := psClear;
//            Picture.Bitmap.Canvas.Rectangle(0,0,31,31);
            FIcon.Visible := false;
          end;
        else
          Picture.Icon.Handle := LoadIcon(0, IconID);
        end;
      end;

      SetBounds(FHorzMargin, Y - 5, 32, 32);
    end;
  end;

  Message := TLabel.Create(Self);
  with Message do
  begin
    Name := 'Instr';
    Parent := Self;
    {$IFDEF DELPHI7_LVL}
    WordWrap := True;
    {$ENDIF}
    Caption := Msg;
    Font.Size := 11;
    Font.Color := RGB(0, 83, 196);
    //Font.Style := [fsBold];
    BoundsRect := TextRect;
    BiDiMode := Self.BiDiMode;
    ShowAccelChar := false;
    ALeft := IconTextWidth - TextRect.Right + FHorzMargin;
    if UseRightToLeftAlignment then
      ALeft := Self.ClientWidth - ALeft - Width;
    SetBounds(ALeft, Y,
      TextRect.Right, TextRect.Bottom);
    y := Y + Height + FVertSpacing;
    FHorzParaMargin := ALeft;
  end;

  if (doTimer in TaskDialog.Options) or (TaskDialog.AutoClose) or
     (doProgressBarMarquee in TaskDialog.Options) or (doProgressBar in TaskDialog.Options) then
  begin
    FTimer := TTimer.Create(Self);
    FTimer.Interval := 100;
    FTimer.OnTimer := OnTimer;
    FTimer.Enabled := True;
  end;

  if (doProgressBar in TaskDialog.Options) then
  begin
    FProgressBar := TProgressBar.Create(Self);
    with FProgressBar do
    begin
      Name := 'ProgressBar';
      Parent := Self;
      BoundsRect := Rect(FHorzMargin, Y, Width - FHorzMargin, Y + 12);
      Min := TaskDialog.ProgressBarMin;
      Max := TaskDialog.ProgressBarMax;
      Position := 0;
    end;

    if not Assigned(FTimer) then
    begin
      FTimer := TTimer.Create(Self);
      FTimer.Interval := 100;
      FTimer.OnTimer := OnTimer;
      FTimer.Enabled := True;
    end;

    {$IFDEF DELPHI_UNICODE}
    if (doProgressBarMarquee in TaskDialog.Options) then
    begin
      FProgressBar.MarqueeInterval := 10;
      FProgressBar.Style := pbstMarquee;
    end;
    {$ENDIF}
  end;

  if (TaskDialog.RadioButtons.Count > 0) then
  begin
    k := 0;

    if (doNodefaultRadioButton in FTaskDialog.Options) then
      FTaskDialog.RadioButtonResult := 0
    else
      FTaskDialog.RadioButtonResult := FTaskDialog.DefaultRadioButton;

    for i := 0 to TaskDialog.RadioButtons.Count-1 do
    begin
      j := FRadioList.Add(FTaskDialog.CreateRadioButton(Self));

      TaskDialog.InitRadioButton(self, TWinControl(FRadioList.Items[j]), i, OnRadioClick);

      with TWinControl(FRadioList.Items[j]) do
      begin
        (* 
        Name := 'Radio' + inttostr(i);
        Parent := Self;
        Font.Name := Canvas.Font.Name;
        Font.Size := 8;
        {$IFDEF DELPHI7_LVL}
        //WordWrap := False;
        {$ENDIF}
        OnClick := OnRadioClick;
        BiDiMode := Self.BiDiMode;
        *)

        BoundsRect := TextRect;
        Left := FHorzParaMargin + FHorzMargin; //ALeft + FHorzMargin;
        Top := Y;
        Width := Self.Width - Left - 4;
        GetTextSize(Canvas, TaskDialog.RadioButtons[i], k, l);
        w := Max(w, Left + k + FHorzMargin + 20);
      end;

      TaskDialog.SetRadioButtonCaption(FRadioList.Items[j],TaskDialog.RadioButtons[i]);

      if doNoDefaultRadioButton in TaskDialog.Options then
        TaskDialog.SetRadioButtonState(FRadioList.Items[j], False)
      else
      begin
        if (TaskDialog.DefaultRadioButton > 0) then
          TaskDialog.SetRadioButtonState(FRadioList.Items[j], (j + 200 = TaskDialog.DefaultRadioButton))
        else
        begin
          TaskDialog.SetRadioButtonState(FRadioList.Items[j], (i = 0));
        end;
      end;

      (*
      with TRadioButton(FRadioList.Items[j]) do
      begin
        if doNoDefaultRadioButton in TaskDialog.Options then
          Checked := False
        else
        begin
          if (TaskDialog.DefaultRadioButton > 0) then
            Checked := (j + 200 = TaskDialog.DefaultRadioButton)
          else
          begin
            Checked := (i = 0);
          end;
        end;
      end;
      *)
    end;
  end;

  if (TaskDialog.ExpandedText <> '') then
  begin
    (*FExpandLabel := TLabel.Create(Self);
    with FExpandLabel do
    begin
      Name := 'Expand';
      Parent := Self;
      {$IFDEF DELPHI7_LVL}
      WordWrap := True;
      {$ENDIF}
      ShowAccelChar := false;
      BiDiMode := Self.BiDiMode;
      FExpandLabel.Caption := TaskDialog.ExpandedText;
      Left := ALeft;
      Top := Y;
    end; *)

    FExpTextXSize := 0;
    FExpTextYSize := 0;
    r := Rect(FHorzMargin, Y, 300, Y + 26);


    if (doHyperlinks in FTaskDialog.Options) then
    begin
      szExpandedText := StringReplace(FTaskDialog.ExpandedText,'\n','<br>',[rfReplaceAll]);
      szExpandedText := StringReplace(szExpandedText,#10,'<br>',[rfReplaceAll]);

      HTMLDrawEx(Canvas, szExpandedText, r, nil, x, y, -1, -1, 1, true, false, false, true, true, false, true,
         1.0, clBlue, clNone, clNone, clGray, anchor, stripped, focusanchor, FExpTextXSize, FExpTextYSize, hyperlinks,
         mouselink, re, nil, nil, 0);
    end
    else
    begin
      szExpandedText := StringReplace(FTaskDialog.ExpandedText,'\n',#13,[rfReplaceAll]);

      FExpTextXSize := r.Right - r.Left;
      //szContent := StringReplace(FTaskDialog.Content,'\n',#13,[rfReplaceAll]);
      //GetTextSize(Canvas, szContent, FExpTextXSize, FExpTextYSize);

      GetTextSize(Canvas, szExpandedText, FExpTextXSize, FExpTextYSize);
    end;

    FExpandButton := TTaskDialogButton.Create(Self);
    with FExpandButton do
    begin
      Name := 'ExpandButton';
      Parent := Self;
      Caption := '';
      ModalResult := mrNone;
      Width := 19;
      Height := 19;
      OnClick := OnExpandButtonClick;
      Picture.LoadFromResourceName(HInstance, 'TD_COLP');
      Picture.TransparentColor :=  clFuchsia;

      PictureHot.LoadFromResourceName(HInstance, 'TD_COLPHOT');
      PictureHot.TransparentColor := clFuchsia;

      PictureDown.LoadFromResourceName(HInstance, 'TD_COLPDOWN');
      PictureDown.TransparentColor := clFuchsia;

      if (TaskDialog.CommonButtons <> []) then
        BackColor := RGB(240,240,240);
    end;
  end;

  verifTextWidth := 0;

  if (TaskDialog.VerificationText <> '') then
  begin
    k := 0;
    FVerificationCheck := TCheckBox.Create(Self);
    with FVerificationCheck do
    begin
      Name := 'Verification';
      Parent := Self;
      {$IFDEF DELPHI7_LVL}
      WordWrap := False;
      {$ENDIF}
      BoundsRect := TextRect;
      BiDiMode := Self.BiDiMode;
      Caption := TaskDialog.VerificationText;
      Left := FHorzMargin;
      Top := Y;
      if (doCommandLinks in TaskDialog.Options) then
        Color := clWindow
      else
        Color := TaskDialog.FooterColor;
      OnClick := OnVerifyClick;
      Checked := (doVerifyChecked in TaskDialog.Options);
      TaskDialog.VerifyResult := Checked;
      GetTextSize(Canvas, Caption, k, l);
      verifTextWidth := k + FVertSpacing * 2;
      w := Max(w, Left + k);
      Height := 16;
    end;
  end;

  FFooterXSize := 0;
  FFooterYSize := 0;

  if (TaskDialog.Footer <> '') then
  begin
    r := Rect(FHorzMargin, Y, 300, Y + 26);

    szFooterText := StringReplace(FTaskDialog.Footer,'\n','<br>',[rfReplaceAll]);
    szFooterText := StringReplace(szFooterText,#10,'<br>',[rfReplaceAll]);

    HTMLDrawEx(Canvas, szFooterText, r, nil, x, y, -1, -1, 1, true, false, false, true, true, false, true,
       1.0, clBlue, clNone, clNone, clGray, anchor, stripped, focusanchor, FFooterXSize, FFooterYSize, hyperlinks,
       mouselink, re, nil, nil, 0);

    IconID := FooterIconIDs[TaskDialog.FooterIcon];
    
    //FooterIconTextWidth := TextRect.Right;
    //FooterIconTextHeight := TextRect.Bottom;
    //if IconID <> nil then
    //begin
    //  Inc(FooterIconTextWidth, 24 + FHorzSpacing);
    //  if FooterIconTextHeight < 24 then FooterIconTextHeight := 24;
    //end;
    

    if IconID <> nil then
    begin
      FFooterIcon := TImage.Create(Self);
      FFooterIconID := IconID;

      with FFooterIcon do
      begin
        Name := 'FooterImage';
        Parent := Self;
        Visible := False;
        SetBounds(FHorzMargin, Y, 16, 16);
      end;
    end;
  end;

  ButtonGroupWidth := CmBtnGroupWidth + CsBtnGroupWidth + verifTextWidth;
  if (TaskDialog.ExpandedText <> '') and Assigned(FExpandButton) then
  begin
    k := 0;
    l := 0;
    GetTextSize(Canvas, FTaskDialog.CollapsControlText, k, l);
    GetTextSize(Canvas, FTaskDialog.ExpandControlText, n, l);
    k := Max(k, n);
    ButtonGroupWidth := ButtonGroupWidth + FExpandButton.Width + FHorzSpacing + k + FHorzSpacing;
  end;

  if TaskDialog.Content = '' then
    Y := Y - 20;

  case TaskDialog.InputType of
  itEdit, itPassword:
    begin
      FInputEdit := TEdit.Create(self);
      FInputEdit.Parent := Self;
      FInputEdit.Name := 'TASKDLGEDIT';
      FInputEdit.TabStop := true;
      FInputEdit.Text := TaskDialog.InputText;
      FInputEdit.MaxLength := TaskDialog.InputMaxLength;

      if TaskDialog.InputType = itPassword then
        FInputEdit.PasswordChar := '*'
      else
        FInputEdit.PasswordChar := #0;

      ALeft := IconTextWidth - TextRect.Right + FHorzMargin;
      if UseRightToLeftAlignment then
        ALeft := Self.ClientWidth - ALeft - Width;

      FInputEdit.SetBounds(ALeft, Y + 20, ClientWidth - ALeft, 20);
    end;
  itComboEdit, itComboList:
    begin
      FInputCombo := TComboBox.Create(self);
      FInputCombo.Parent := Self;
      FInputCombo.Name := 'TASKDLGCOMBO';
      FInputCombo.TabStop := true;
      FInputCombo.Text := TaskDialog.InputText;
      FInputCombo.DropDownCount := TaskDialog.InputDropDownCount;
      FInputCombo.Items.Assign(TaskDialog.InputItems);
      FInputCombo.MaxLength := TaskDialog.InputMaxLength;

      if (TaskDialog.InputType = itComboList) then
      begin
        FInputCombo.Style := csDropDownList;
        if TaskDialog.InputItemIndex < -1 then
          FInputCombo.ItemIndex := FInputCombo.Items.IndexOf(TaskDialog.InputText)
        else
          FInputCombo.ItemIndex := TaskDialog.InputItemIndex;
      end;

      ALeft := IconTextWidth - TextRect.Right + FHorzMargin;
      if UseRightToLeftAlignment then
        ALeft := Self.ClientWidth - ALeft - Width;

      FInputCombo.SetBounds(ALeft, Y + 20, ClientWidth - ALeft, 20);
    end;
  itDate, itTime:
    begin
      FInputDate := TDateTimePicker.Create(self);
      FInputDate.Parent := Self;
      FInputDate.Name := 'TASKDLGDATE';
      FInputDate.TabStop := true;
      ALeft := IconTextWidth - TextRect.Right + FHorzMargin;
      if UseRightToLeftAlignment then
        ALeft := Self.ClientWidth - ALeft - Width;

      if TaskDialog.InputType = itDate then
        FInputDate.Kind := dtkDate
      else
        FInputDate.Kind := dtkTime;

      {$IFDEF DELPHI2006_LVL}
      if TaskDialog.InputDateTime <> 0 then
        FInputDate.DateTime := TaskDialog.InputDateTime
      else
        if TryStrToDate(TaskDialog.InputText, trydate) then
          FInputDate.Date := trydate;
      {$ENDIF}

      if TaskDialog.DateTimeFormat <> '' then
        FInputDate.Format := TaskDialog.DateTimeFormat;

      FInputDate.Top := Y + 20;
      FInputDate.Left := ALeft;
    end;
  itMemo:
    begin
      FInputMemo := TMemo.Create(self);
      FInputMemo.Parent := Self;
      FInputMemo.Name := 'TASKDLGMEMO';
      FInputMemo.TabStop := true;
      FInputMemo.WantReturns := false;
      FInputMemo.MaxLength := TaskDialog.InputMaxLength;
      FInputMemo.Lines.Text := TaskDialog.InputText;
      ALeft := IconTextWidth - TextRect.Right + FHorzMargin;
      if UseRightToLeftAlignment then
        ALeft := Self.ClientWidth - ALeft - Width;
      FInputMemo.SetBounds(ALeft, Y + 20, ClientWidth - ALeft, 60);
    end;
  itCustom:
    begin
      if Assigned(TaskDialog.InputControl) then
      begin
        FOldParent := TaskDialog.InputControl.Parent;
        TaskDialog.InputControl.Parent := self;
        TaskDialog.InputControl.Visible := true;
        if Assigned(TaskDialog.OnDialogInputSetText) then
          TaskDialog.OnDialogInputSetText(TaskDialog, TaskDialog.InputText)
        else
        begin
          if not (TaskDialog.InputControl is TMaskEdit) then
            SetWindowText(TaskDialog.InputControl.Handle, PChar(TaskDialog.InputText))
          else
            (TaskDialog.InputControl as TMaskEdit).Text := TaskDialog.InputText;
        end;

        ALeft := IconTextWidth - TextRect.Right + FHorzMargin;
        if UseRightToLeftAlignment then
          ALeft := Self.ClientWidth - ALeft - Width;

        TaskDialog.InputControl.Left := ALeft;
        TaskDialog.InputControl.Top := Y + 20;

        if TaskDialog.InputControl.Width + ALeft > self.Width then
          w := TaskDialog.InputControl.Width + ALeft + ALeft;

        //TaskDialog.InputControl.SetBounds(ALeft, Y + 20, ClientWidth - ALeft, 20);
      end;
    end;
  end;

  //-- setting Form Width
  k := Max(FFooterXSize, Max(IconTextWidth, ButtonGroupWidth)) + FHorzMargin * 2;
  k := Max(FExpTextXSize + FHorzMargin * 2, k);
  w := Max(w, k);

  w := Max(w, Round(FMinFormWidth * Screen.PixelsPerInch / 96));

  if w > (Screen.Width - 2 * GetSystemMetrics(SM_CYEDGE)) then
    w := Screen.Width - 2 * GetSystemMetrics(SM_CYEDGE);

//  if w > 800 then
//    w := 800;

  ClientWidth := w;

  if (TaskDialog.InputType = itCustom) and Assigned(TaskDialog.InputControl) then
  begin
    if TaskDialog.InputControl.Width > ClientWidth - ALeft then
      TaskDialog.InputControl.Width := ClientWidth - ALeft;
  end;

  if (doProgressBar in TaskDialog.Options) then
  begin
    FProgressBar.Width := ClientWidth - FHorzMargin*2;
  end;

  SetPositions;

  if (TaskDialog.ExpandedText <> '') then
  begin
    SetExpanded((doExpandedDefault in TaskDialog.Options));
  end;

  Left := (Screen.Width div 2) - (Width div 2);
  Top := (Screen.Height div 2) - (Height div 2);
  OldFont.Free;
end;

//------------------------------------------------------------------------------

procedure TAdvMessageForm.UpdateDialog;
var
  DialogUnits: TPoint;
  ButtonSpacing, ButtonGroupWidth, IconTextWidth, X, Y: Integer;
  IconID: PChar;
  TextRect: TRect;
  Msg: string;
  DlgType: TTaskDialogIcon;
  i, w: Integer;
  CmBtnGroupWidth, CsBtnGroupWidth: Integer;
  r, re: trect;
  anchor, stripped: string;
  HyperLinks,MouseLink, k, l, n: Integer;
  Focusanchor,szFooterText: string;
  OldFont: TFont;
begin
  if not Assigned(FTaskDialog) then
    Exit;

  Msg := FTaskDialog.Instruction;
  DlgType := FTaskDialog.Icon;
  //Buttons := FTaskDialog.CommonButtons;

  OldFont := TFont.Create;
  OldFont.Assign(Canvas.Font);

  DialogUnits := GetAveCharSize(Canvas);
  w := 0;

  if FTaskDialog.Title <> '' then
    Caption := FTaskDialog.Title
  else
    Caption := GetExeName;

  if (Caption <> '') then
  begin
    w := 1000;
    GetTextSize(Canvas, Caption, w, l);
    w := w + 50;
  end;

  ButtonSpacing := MulDiv(mcButtonSpacing, DialogUnits.X, 4);
  CmBtnGroupWidth := 0;
  CsBtnGroupWidth := 0;
  Y := FVertMargin;
  //ALeft := 0;

  for i := 0 to FcmBtnList.Count-1 do
  begin
    CmBtnGroupWidth := CmBtnGroupWidth + TButton(FcmBtnList.Items[i]).Width + ButtonSpacing;
  end;

  if not (docommandLinks in FTaskDialog.Options) then
  begin
    for i := 0 to FcsBtnList.Count-1 do
    begin
      CsBtnGroupWidth := CsBtnGroupWidth + TButton(FcsBtnList.Items[i]).Width + ButtonSpacing;
    end;
  end;

  // Instruction
  Canvas.Font.Size := 11;
  Canvas.Font.Style := [fsBold];

  SetRect(TextRect, 0, 0, Screen.Width div 2, 0);
  DrawText(Canvas.Handle, PChar(Msg), Length(Msg) + 1, TextRect,
    DT_EXPANDTABS or DT_CALCRECT or DT_WORDBREAK or
    DrawTextBiDiModeFlagsReadingOnly);

  Canvas.Font.Assign(OldFont);

  IconID := IconIDs[DlgType];
  IconTextWidth := TextRect.Right;
  if (IconId <> nil) then
  begin
    Inc(IconTextWidth, 32 + FHorzSpacing);
  end;

  if Assigned(Message) then
  begin
    Message.Caption := Msg;
    //ALeft := IconTextWidth - TextRect.Right + FHorzMargin;
    //if UseRightToLeftAlignment then
      //ALeft := Self.ClientWidth - ALeft - Width;
    y := Y + Height + FVertSpacing;
  end;

  if (FTaskDialog.RadioButtons.Count > 0) then
  begin
    FTaskDialog.RadioButtonResult := FTaskDialog.DefaultRadioButton;

    for i := 0 to FRadioList.Count - 1 do
    begin
      with TRadioButton(FRadioList.Items[i]) do
      begin
        BoundsRect := TextRect;
        Left := FHorzParaMargin + FHorzMargin;
        Top := Y;
        Width := Self.Width - Left - 4;
        GetTextSize(Canvas, Caption, k, l);
        w := Max(w, Left + k + FHorzMargin + 20);
      end;
    end;
  end;

  {if (FTaskDialog.ExpandedText <> '') and Assigned(FExpandLabel) then
  begin
    with FExpandLabel do
    begin
      Left := ALeft;
      Top := Y;
      FExpandLabel.Caption := FTaskDialog.ExpandedText;
    end;
  end; }

  if (FTaskDialog.VerificationText <> '') and Assigned(FVerificationCheck) then
  begin
    k := 0;
    with FVerificationCheck do
    begin
      BoundsRect := TextRect;
      Caption := FTaskDialog.VerificationText;
      Left := FHorzMargin;
      Top := Y;
      GetTextSize(Canvas, Caption, k, l);
      w := Max(w, Left + k);
    end;
  end;

  FFooterXSize := 0;
  FFooterYSize := 0;
  if (FTaskDialog.Footer <> '') then
  begin
    r := Rect(FHorzMargin, Y, 300, Y + 26);
    x := 0;
    szFooterText := StringReplace(FTaskDialog.Footer,'\n','<br>',[rfReplaceAll]);
    szFooterText := StringReplace(szFooterText,#10,'<br>',[rfReplaceAll]);

    HTMLDrawEx(Canvas, szFooterText, r, nil, x, y, -1, -1, 1, true, false, false, true, true, false, true,
       1.0, clBlue, clNone, clNone, clGray, anchor, stripped, focusanchor, FFooterXSize, FFooterYSize, hyperlinks,
       mouselink, re, nil, nil, 0);

    if Assigned(FFooterIcon) then
    begin
      FFooterIcon.SetBounds(FHorzMargin, Y, 16, 16);
    end;
  end;

  ButtonGroupWidth := CmBtnGroupWidth + CsBtnGroupWidth;
  if (FTaskDialog.ExpandedText <> '') and Assigned(FExpandButton) then
  begin
    k := 0;
    l := 0;
    GetTextSize(Canvas, FTaskDialog.CollapsControlText, k, l);
    GetTextSize(Canvas, FTaskDialog.ExpandControlText, n, l);
    k := Max(k, n);
    ButtonGroupWidth := ButtonGroupWidth + FExpandButton.Width + FHorzSpacing + k + FHorzSpacing;
  end;

  //-- setting Form Width
  k := Max(FFooterXSize, Max(IconTextWidth, ButtonGroupWidth)) + FHorzMargin * 2;
  w := Max(w, k);
  w := Max(w, FMinFormWidth);

  ClientWidth := w;

  if (doProgressBar in FTaskDialog.Options) and Assigned(FProgressBar) then
  begin
    FProgressBar.Width := ClientWidth - FHorzMargin*2;
  end;

  SetPositions;

  OldFont.Free;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvMessageForm.SetPositions;
var
  DialogUnits: TPoint;
  HorzMargin, VertMargin, VertSpacing, ButtonSpacing, ButtonGroupWidth, X, Y: Integer;
  i, h: Integer;
  CmBtnGroupWidth, CsBtnGroupWidth, BtnH: Integer;
  X1, y1: Integer;
  r, re, rc: trect;
  anchor, stripped: string;
  HyperLinks,MouseLink: Integer;
  Focusanchor: string;
  ExpTextTop, verifTextWidth, k, l: Integer;
  szContent: string;
  szExpandedText,szFooterText: string;
  //lbl:TLabel;
  //ExH: integer;
begin
  if not Assigned(FTaskDialog) then
    Exit;

  DialogUnits := GetAveCharSize(Canvas);
  HorzMargin := MulDiv(mcHorzMargin, DialogUnits.X, 4);
  VertMargin := MulDiv(mcVertMargin, DialogUnits.Y, 8);
  VertSpacing := MulDiv(mcVertSpacing, DialogUnits.Y, 8);
  ButtonSpacing := MulDiv(mcButtonSpacing, DialogUnits.X, 4);
  CmBtnGroupWidth := 0;
  CsBtnGroupWidth := 0;
  Y := VertMargin;

  {$IFDEF DELPHI7_LVL}
  Message.Transparent := true;
  {$ENDIF}
  
  // Instruction Label
  if (Message.Caption <> '') then
    y := Y + Message.Height + VertSpacing
  else
    Message.Visible := False;

  if (FTaskDialog.Content <> '') then
  begin
    //FContent.Width := ClientWidth - FContent.Left - HorzMargin;
    //FContent.Top := Y;
    //Y := Y + FContent.Height + VertSpacing;
    X1 := 0;
    Y1 := 0;
    r := GetContentRect;
    r := Rect(r.Left, Y, R.Right, Y + 26);

    if (doHyperlinks in FTaskDialog.Options) then
    begin
      szContent := StringReplace(FTaskDialog.Content,'\n','<br>',[rfReplaceAll]);
      szContent := StringReplace(szContent,#10,'<br>',[rfReplaceAll]);      

      HTMLDrawEx(Canvas, szContent, r, nil, x1, y1, -1, -1, 1, true, true, false, true, true, false, true,
         1.0, clBlue, clNone, clNone, clGray, anchor, stripped, focusanchor, FContentXSize, FContentYSize, hyperlinks,
         mouselink, re, nil, nil, 0);
    end
    else
    begin
      if HasLf(FTaskDialog.Content) then
        szContent := StringReplace(FTaskDialog.Content,'\n',#13,[rfReplaceAll])
      else
        szContent := FTaskDialog.Content;

      {
      if (Message.Caption <> '') then
        FContentXSize := Message.Width
      else
        FContentXSize := 360;

      if FContentXSize < 360 then
        FContentXSize := 360;
      }
      FContentXSize := r.Right - r.Left;
      GetTextSize(Canvas, szContent, FContentXSize, FContentYSize);
    end;

    rc := GetContentRect;
    if (fContentXSize > rc.Right - rc.Left) then
      ClientWidth := ClientWidth + (fContentXSize - (rc.Right - rc.Left));

    y1 := FContentYSize;
    if (Message.Caption = '') and Assigned(FIcon) then
    begin
      y1 := Max(FIcon.Height, Y1);
    end;

    Y := Y + Y1 + VertSpacing;

    case FTaskDialog.InputType of
    itEdit, itPassword: FInputEdit.Top := Y - 10;
    itComboEdit,itComboList: FInputCombo.Top := Y - 10;
    itDate,itTime: FInputDate.Top := Y - 10;
    itMemo: FInputMemo.Top := Y - 10;
    itCustom: if Assigned(FTaskDialog.InputControl) then
               FTaskDialog.InputControl.Top := Y - 10;
    end;

  end
  else
  begin
    if (FTaskDialog.RadioButtons.Count = 0) and not (doCommandLinks in FTaskDialog.Options) then
      Y := Y + VertSpacing;

    if (Message.Caption = '') and Assigned(FIcon) then
      Y := Y + VertSpacing + VertMargin;
  end;

  if (FTaskDialog.InputType in [itEdit, itPassword, itComboEdit, itComboList, itDate,itTime]) then
  begin
    Y := Y + 36;
  end;

  if (FTaskDialog.InputType in [itMemo]) then
  begin
    Y := Y + 70;
  end;

  if (FTaskDialog.InputType in [itCustom]) then
  begin
    if Assigned(FTaskDialog.InputControl) then
      Y := Y + FTaskDialog.InputControl.Height + 16
    else
      Y := Y + 36;
  end;

  if (doProgressBar in FTaskDialog.Options) then
  begin
    if Assigned(FIcon) then
    begin
      Y := Max(Y, FIcon.Top + FIcon.Height+3);
    end;
    FProgressBar.Top := Y;
    Y := Y + FProgressBar.Height + VertSpacing;
  end;

  if (FTaskDialog.RadioButtons.Count > 0) then
  begin
    for i:= 0 to FRadioList.Count-1 do
    begin
      TRadioButton(FRadioList.Items[i]).Top := Y;
      TRadioButton(FRadioList.Items[i]).Width := ClientWidth - TRadioButton(FRadioList.Items[i]).Left - HorzMargin;
      Y := Y + TRadioButton(FRadioList.Items[i]).Height + 4;
    end;
    Y := Y + VertSpacing - 4;
  end;

  FExpTextXSize := 0;
  FExpTextYSize := 0;
  ExpTextTop := 0;
  if (FTaskDialog.ExpandedText <> '') then
  begin
    if FExpanded then
    begin
      (*lbl := TLabel.Create(self);
      {$IFDEF DELPHI7_LVL}
      lbl.WordWrap := true;
      {$ENDIF}
      lbl.Width := ClientWidth - FExpandLabel.Left - HorzMargin;
      lbl.Caption := FTaskDialog.FExpandedText;
      ExH := lbl.Height;
      lbl.Free;

      FExpandLabel.Top := Y;
      FExpandLabel.Width := ClientWidth - FExpandLabel.Left - HorzMargin;
      FExpandLabel.Height := ExH;

      Y := Y + FExpandLabel.Height + VertSpacing;
      FExpandLabel.Visible := True;
      *)


      X1 := 0;
      Y1 := 0;
      r := GetExpTextRect;
      r := Rect(r.Left, Y, R.Right, Y + 26);

      if (doHyperlinks in FTaskDialog.Options) then
      begin
        szExpandedText := StringReplace(FTaskDialog.ExpandedText,'\n','<br>',[rfReplaceAll]);
        szExpandedText := StringReplace(szExpandedText,#10,'<br>',[rfReplaceAll]);

        HTMLDrawEx(Canvas, szExpandedText, r, nil, x1, y1, -1, -1, 1, true, true, false, true, true, false, true,
           1.0, clBlue, clNone, clNone, clGray, anchor, stripped, focusanchor, FExpTextXSize, FExpTextYSize, hyperlinks,
           mouselink, re, nil, nil, 0);
      end
      else
      begin
        szExpandedText := StringReplace(FTaskDialog.ExpandedText,'\n',#13,[rfReplaceAll]);

        FExpTextXSize := r.Right - r.Left;
        GetTextSize(Canvas, szExpandedText, FExpTextXSize, FExpTextYSize);
      end;

      ExpTextTop := Y;
      FExpTextTop := ExpTextTop;
      Y := Y + FExpTextYSize + VertSpacing;
    end
    else
    begin
      //FExpandLabel.Visible := False;
    end;
  end;

  if not (docommandLinks in FTaskDialog.Options) then
  begin
    for i:= 0 to FcsBtnList.Count-1 do
    begin
      CsBtnGroupWidth := CsBtnGroupWidth + TButton(FcsBtnList.Items[i]).Width{ + ButtonSpacing};
    end;

    if (FcsBtnList.Count > 0) then
      CsBtnGroupWidth := CsBtnGroupWidth + (FcsBtnList.Count-1) * ButtonSpacing;
  end
  else
  begin
    for i := 0 to FcsBtnList.Count - 1 do
    begin
      if Assigned(FIcon) then
        TTaskDialogButton(FcsBtnList.Items[i]).Left := FHorzParaMargin; // FIcon.Left + FIcon.Width + FHorzSpacing;
      TTaskDialogButton(FcsBtnList.Items[i]).Top := Y;
      TTaskDialogButton(FcsBtnList.Items[i]).Width := ClientWidth - TTaskDialogButton(FcsBtnList.Items[i]).Left - HorzMargin;
      Y := Y + TTaskDialogButton(FcsBtnList.Items[i]).Height + 2;
    end;
    FWhiteWindowHeight := Y;
    Y := Y + VertSpacing;
  end;

  for i := 0 to FcmBtnList.Count - 1 do
  begin
    CmBtnGroupWidth := CmBtnGroupWidth + TButton(FcmBtnList.Items[i]).Width{ + ButtonSpacing};
  end;

  CmBtnGroupWidth := CmBtnGroupWidth + (FcmBtnList.Count - 1) * ButtonSpacing;

  verifTextWidth := 0;

  if (FTaskDialog.VerificationText <> '') then
  begin
    GetTextSize(Canvas, FTaskDialog.VerificationText, k, l);
    verifTextWidth := k + FVertSpacing * 2;
  end;

  ButtonGroupWidth := CsBtnGroupWidth + CmBtnGroupWidth;

  X := (ClientWidth - ButtonGroupWidth - FHorzSpacing); //(ClientWidth - ButtonGroupWidth) div 2;
  h := Y;
  BtnH := 0;

  if (FTaskDialog.ExpandedText <> '') then
  begin
    X := (ClientWidth - ButtonGroupWidth - FHorzSpacing - 4);
    {
    k := 0;
    l := 0;
    GetTextSize(Canvas, FTaskDialog.CollapsControlText, k, l);
    GetTextSize(Canvas, FTaskDialog.ExpandControlText, n, l);
    k := Max(k, n);
    ButtonGroupWidth := ButtonGroupWidth + FExpandButton.Width + ButtonSpacing + k + FHorzSpacing;
    }
  end;

  if (FTaskDialog.ExpandedText <> '') then
  begin
    with FExpandButton do
    begin
      Top := Y + 4;
      Left := FVertMargin; //X;
      //Inc(X, FExpandButton.Width + ButtonSpacing);
      if (FExpandButton.Height > BtnH) then
        BtnH := FExpandButton.Height;
    end;
  end;

  if (FTaskDialog.VerificationText <> '') and Assigned(FVerificationCheck) then
  begin
    FVerificationCheck.Width := verifTextWidth - FVertSpacing; //ClientWidth - FVerificationCheck.Left - HorzMargin;
    FVerificationCheck.Top := Y + BtnH + 12;
    FVerificationCheck.Left := FVertMargin + 3;
    //X := FVerificationCheck.Left + FVerificationCheck.Width + FVertMargin;
  end;

  if not (docommandLinks in FTaskDialog.Options) then
  begin
    for i := 0 to FcsBtnList.Count-1 do
    begin
      with TButton(FcsBtnList.Items[i]) do
      begin
        Top := Y + 8;
        Left := X;
        Inc(X, TButton(FcsBtnList.Items[i]).Width + ButtonSpacing);
        //if (i = 0) then
          //h := h + TButton(FcsBtnList.Items[i]).Height;
        if (TButton(FcsBtnList.Items[i]).Height > BtnH) then
          BtnH := TButton(FcsBtnList.Items[i]).Height;
      end;
    end;
    if (FcsBtnList.Count > 0) then
      FWhiteWindowHeight := TButton(FcsBtnList.items[0]).Top{ - (FVertSpacing div 2)};
  end;

  for i := 0 to FcmBtnList.Count-1 do
  begin
    with TButton(FcmBtnList.Items[i]) do
    begin
      Top := Y + 8;
      Left := X;
      Inc(X, TButton(FcmBtnList.Items[i]).Width + ButtonSpacing);
      //if (i = 0) then
        //h := h + TButton(FcmBtnList.Items[i]).Height;
      if (TButton(FcmBtnList.Items[i]).Height > BtnH) then
        BtnH := TButton(FcmBtnList.Items[i]).Height;
    end;

    if (FcmBtnList.Count > 0) then
      FWhiteWindowHeight := TButton(FcmBtnList.items[0]).Top{ - (FVertSpacing div 2)};
  end;

  if (FTaskDialog.VerificationText <> '') and Assigned(FVerificationCheck) then
  begin
    if ((FcmBtnList.Count > 0) or (FTaskDialog.CustomButtons.Count > 0)) and not (doCommandLinks in FTaskDialog.Options)  then
      h := h + BtnH
    else
      h := h + FVerificationCheck.Height;   //Max(BtnH, FVerificationCheck.Height + VertSpacing);
    if FTaskDialog.ExpandedText <> '' then
      h := h + VertSpacing;
    y := y + Max(BtnH + FVertSpacing, FVerificationCheck.Height + VertSpacing);
  end
  else
  begin
    h := h + BtnH;
    if (BtnH > 0) then
      y := y + BtnH + FVertSpacing;
  end;

  if (FTaskDialog.Footer <> '') then
  begin
    X1 := 0;
    Y1 := 0;
    if Assigned(FFooterIcon) then
      r := Rect(HorzMargin + 20, Y, Width - HorzMargin, Y + 100)
    else
      r := Rect(HorzMargin, Y, Width - HorzMargin, Y + 100);

    szFooterText := StringReplace(FTaskDialog.Footer,'\n','<br>',[rfReplaceAll]);
    szFooterText := StringReplace(szFooterText,#10,'<br>',[rfReplaceAll]);

    HTMLDrawEx(Canvas, szFooterText, r, nil, x1, y1, -1, -1, 1, true, false, false, true, true, false, true,
       1.0, clBlue, clNone, clNone, clGray, anchor, stripped, focusanchor, FFooterXSize, FFooterYSize, hyperlinks,
       mouselink, re, nil, nil, 0);

    y1 := FFooterYSize;
    if Assigned(FFooterIcon) then
    begin
      FFooterIcon.Top := Y;
      y1 := Max(Y1, 20);
    end;
    h := h + Y1 + VertSpacing;
  end;

  h := h + VertMargin;

  if not IsVista and (FTaskDialog.VerificationText <> '') then
    h := h + 4;


  ClientHeight := h;

  if (FcmBtnList.Count = 0) and ((docommandLinks in FTaskDialog.Options) or (not (docommandLinks in FTaskDialog.Options) and (FcsBtnList.Count = 0))) then
    FWhiteWindowHeight := Height;

  if (ExpTextTop > 0) and (doExpandedFooter in FTaskDialog.Options) then
    FWhiteWindowHeight := ExpTextTop;
end;

//------------------------------------------------------------------------------

constructor TAdvMessageForm.CreateNew(AOwner: TComponent; Dummy: Integer);
var
  NonClientMetrics: TNonClientMetrics;
begin
  inherited CreateNew(AOwner);
  NonClientMetrics.cbSize := sizeof(NonClientMetrics);
  if SystemParametersInfo(SPI_GETNONCLIENTMETRICS, 0, @NonClientMetrics, 0) then
    Font.Handle := CreateFontIndirect(NonClientMetrics.lfMessageFont);

  FExpandButton := nil;
  FExpanded := true;
  //FExpandLabel := nil;
  FExpandControlText := '';
  FCollapsControlText := '';
  FcmBtnList := TList.Create;
  FcsBtnList := TList.Create;
  FRadioList := TList.Create;
  FFooterXSize := 0;
  FFooterYSize := 0;
  FWhiteWindowHeight := Height;
  FHorzParaMargin := 0;
  FMinFormWidth := 350;
end;

//------------------------------------------------------------------------------

{procedure TAdvMessageForm.HelpButtonClick(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;}

//------------------------------------------------------------------------------

procedure TAdvMessageForm.CustomKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if ((ssAlt in Shift) and (Key = VK_F4)) then
    Key := 0;

  if (Shift = [ssCtrl]) and (Key = Word('C')) then
  begin
    Beep;
    WriteToClipBoard(GetFormText);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvMessageForm.WMActivate(var M: TWMActivate);
begin
  // only do this when parent form is topmost
  SetWindowPos( Handle, HWND_TOP, 0,0,0,0, SWP_NOMOVE or SWP_NOSIZE );
end;

procedure TAdvMessageForm.WriteToClipBoard(Text: String);
var
  Data: THandle;
  DataPtr: Pointer;
begin
  if OpenClipBoard(0) then
  begin
    try
      Data := GlobalAlloc(GMEM_MOVEABLE+GMEM_DDESHARE, Length(Text) + 1);
      try
        DataPtr := GlobalLock(Data);
        try
          Move(PChar(Text)^, DataPtr^, Length(Text) + 1);
          EmptyClipBoard;
          SetClipboardData(CF_TEXT, Data);
        finally
          GlobalUnlock(Data);
        end;
      except
        GlobalFree(Data);
        raise;
      end;
    finally
      CloseClipBoard;
    end;
  end
  else
    raise Exception.CreateRes(@SCannotOpenClipboard);
end;

//------------------------------------------------------------------------------

function TAdvMessageForm.GetFormText: String;
var
  DividerLine, ButtonCaptions: string;
  I: integer;
begin
  DividerLine := StringOfChar('-', 27) + sLineBreak;
  for I := 0 to ComponentCount - 1 do
    if Components[I] is TButton then
      ButtonCaptions := ButtonCaptions + TButton(Components[I]).Caption +
        StringOfChar(' ', 3);
  ButtonCaptions := StringReplace(ButtonCaptions,'&','', [rfReplaceAll]);
  Result := Format('%s%s%s%s%s%s%s%s%s%s', [DividerLine, Caption, sLineBreak,
    DividerLine, Message.Caption, sLineBreak, DividerLine, ButtonCaptions,
    sLineBreak, DividerLine]);
end;

//------------------------------------------------------------------------------

procedure TAdvMessageForm.SetExpandButton(const Value: TTaskDialogButton);
begin
  if Assigned(FExpandButton) then
    FExpandButton.OnClick := nil;

  FExpandButton := Value;

  if Assigned(FExpandButton) then
    FExpandButton.OnClick := OnExpandButtonClick;
end;

//------------------------------------------------------------------------------

procedure TAdvMessageForm.OnExpandButtonClick(Sender: TObject);
begin
  if Assigned(FExpandButton) then
  begin
    SetExpanded(not Expanded);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvMessageForm.SetExpanded(Value: Boolean);
begin
  if FExpanded then
  begin
    if not Value then
    begin
      FExpandButton.Picture.LoadFromResourceName(HInstance, 'TD_EXP');
      FExpandButton.Picture.TransparentColor := clFuchsia;
      FExpandButton.PictureHot.LoadFromResourceName(HInstance, 'TD_EXPHOT');
      FExpandButton.PictureHot.TransparentColor := clFuchsia;
      FExpandButton.PictureDown.LoadFromResourceName(HInstance, 'TD_EXPDOWN');
      FExpandButton.PictureDown.TransparentColor := clFuchsia;
    end;
  end
  else
  begin
    if Value then
    begin
      FExpandButton.Picture.LoadFromResourceName(HInstance, 'TD_COLP');
      FExpandButton.Picture.TransparentColor := clFuchsia;
      FExpandButton.PictureHot.LoadFromResourceName(HInstance, 'TD_COLPHOT');
      FExpandButton.PictureHot.TransparentColor := clFuchsia;
      FExpandButton.PictureDown.LoadFromResourceName(HInstance, 'TD_COLPDOWN');
      FExpandButton.PictureDown.TransparentColor := clFuchsia;
    end;
  end;
  FExpanded := Value;
  SetPositions;
  Invalidate;
end;

//------------------------------------------------------------------------------

destructor TAdvMessageForm.Destroy;
begin
  FcmBtnList.Free;
  FcsBtnList.Free;
  FRadioList.Free;
  if Assigned(FTimer) then
    FTimer.Free;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvMessageForm.DrawExpandedText;
var
  r, re: trect;
  anchor, stripped: string;
  HyperLinks,MouseLink: Integer;
  Focusanchor: string;
  xsize, ysize: Integer;
  szExpandedText: string;
begin
  if not Assigned(FTaskDialog) or (not FExpanded) then
    Exit;

  R := GetExpTextRect;
  if (FTaskDialog.ExpandedText <> '') then
  begin
    Canvas.Font.Color := FTaskDialog.FNormalFontColor;
    if (doHyperlinks in FTaskDialog.Options) then
    begin
      szExpandedText := StringReplace(FTaskDialog.ExpandedText,'\n','<br>',[rfReplaceAll]);
      szExpandedText := StringReplace(szExpandedText,#10,'<br>',[rfReplaceAll]);      

      HTMLDrawEx(Canvas, szExpandedText, R, nil, 0, 0, -1, -1, 1, false, false, false, false, False, false,
        true, 1.0, clBlue, clNone, clNone, clGray, anchor, stripped, focusanchor, xsize, ysize,
        hyperlinks, mouselink, re, nil , nil, 0);
    end
    else
    begin
      szExpandedText := StringReplace(FTaskDialog.ExpandedText,'\n',#13,[rfReplaceAll]);

      DrawText(Canvas.Handle,PChar(szExpandedText),Length(szExpandedText), R, DT_EXPANDTABS or DT_LEFT or DT_VCENTER or DT_WORDBREAK or DT_NOPREFIX);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvMessageForm.DrawContent;
var
  r, re: trect;
  anchor, stripped: string;
  HyperLinks,MouseLink: Integer;
  Focusanchor: string;
  xsize, ysize: Integer;
  szContent: string;
begin
  if not Assigned(FTaskDialog) then
    Exit;

  R := GetContentRect;
  if (FTaskDialog.Content <> '') then
  begin
    Canvas.Font.Color := FTaskDialog.FNormalFontColor;

    if (doHyperlinks in FTaskDialog.Options) then
    begin
      szContent := StringReplace(FTaskDialog.Content,'\n','<br>',[rfReplaceAll]);
      szContent := StringReplace(szContent,#10,'<br>',[rfReplaceAll]);

      HTMLDrawEx(Canvas, szContent, R, nil, 0, 0, -1, -1, 1, false, false, false, false, False, false,
        true, 1.0, clBlue, clNone, clNone, clGray, anchor, stripped, focusanchor, xsize, ysize,
        hyperlinks, mouselink, re, nil , nil, 0);
    end
    else
    begin
      if HasLf(FTaskDialog.Content) then
        szContent := StringReplace(FTaskDialog.Content,'\n',#13,[rfReplaceAll])
      else
        szContent := FTaskDialog.Content;
      DrawText(Canvas.Handle,PChar(szContent),Length(szContent), R, DT_EXPANDTABS or DT_LEFT or DT_VCENTER or DT_WORDBREAK or DT_NOPREFIX);
    end;
  end;
end;

//------------------------------------------------------------------------------

function TAdvMessageForm.GetContentRect: TRect;
var
  X, Y: Integer;
begin
  Result := Rect(-1, -1, -1, -1);
  if Assigned(FTaskDialog) and (FTaskDialog.Content <> '') then
  begin
    X := FHorzMargin;
    if Assigned(FIcon) then
      X := FIcon.Left + FIcon.Width + FHorzSpacing;
    if (Message.Caption <> '') then
      Y := Message.Top + Message.Height + FVertSpacing
    else
      Y := FVertMargin;
    Result := Rect(X, Y, ClientWidth - FHorzMargin, Y + FContentYSize);
  end;
end;

//------------------------------------------------------------------------------

function TAdvMessageForm.GetExpTextRect: TRect;
var
  X, Y: Integer;
begin
  Result := Rect(-1, -1, -1, -1);
  if Assigned(FTaskDialog) and FExpanded then
  begin
    X := FHorzMargin;
    if Assigned(FIcon) then
      X := FIcon.Left + FIcon.Width + FHorzSpacing;
    {if (Message.Caption <> '') then
      Y := Message.Top + Message.Height + FVertSpacing
    else
      Y := FVertMargin;

    if (FTaskDialog.Content <> '') then
      y := Y + FContentYSize + FVertSpacing;

    if (doProgressBar in FTaskDialog.Options) then
    begin
      if Assigned(FIcon) then
      begin
        Y := Max(Y, FIcon.Top + FIcon.Height+3);
      end;

      if Assigned(FProgressBar) then
        Y := Y + FProgressBar.Height + FVertSpacing;
    end;

    if (FTaskDialog.RadioButtons.Count > 0) then
    begin
      if (FRadioList.Count > 0) then
        Y := Y + TRadioButton(FRadioList.Items[FRadioList.Count-1]).Height + FVertSpacing;
    end;}
    Y := FExpTextTop;

    Result := Rect(X, Y, ClientWidth - FHorzMargin, Y + FExpTextYSize);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvMessageForm.DrawFooter;
var
  r, re: trect;
  anchor, stripped: string;
  HyperLinks,MouseLink: Integer;
  Focusanchor: string;
  xsize, ysize, i: Integer;
  bmp: TBitmap;
  shieldbmp: TBitmap;
  IconH: THandle;
  szFooterText: string;

begin
  if not Assigned(FTaskDialog) then
    Exit;

  if (FTaskDialog.Footer <> '') then
  begin
    R := GetFooterRect;

    i := R.Top - FVertSpacing + 6;
    Canvas.Pen.Color := RGB(223, 223, 223);
    Canvas.MoveTo(2, i);
    Canvas.LineTo(ClientWidth -3, i);
    Canvas.Pen.Color := clWhite;
    Canvas.MoveTo(2, i+1);
    Canvas.LineTo(ClientWidth -3, i+1);

    if Assigned(FFooterIcon) then
    begin

      IconH := LoadImage(0,FFooterIconID,IMAGE_ICON,16,16, LR_SHARED);

      bmp := TBitmap.Create;
      try
        bmp.Width := 16;
        bmp.Height := 16;
        bmp.Transparent := True;
        bmp.Canvas.Brush.Color := FTaskDialog.FooterColor;
        bmp.Canvas.Rectangle(0,0,16,16);
        //DrawIcon(bmp.Canvas.Handle,0, 0, IconH);
        //Canvas.StretchDraw(Rect(R.Left, R.Top-2, R.Left+16, R.Top+14), bmp);

        if FTaskDialog.FooterIcon = tfiShield then
        begin
          shieldbmp := TBitmap.Create;
          shieldbmp.Handle := LoadBitmap(hInstance, 'TD_SHIELD');
          bmp.Canvas.StretchDraw(Rect(0,0,16,16),shieldbmp);
          shieldbmp.Free;
        end
        else
        begin
          DrawIconEx(bmp.Canvas.Handle, 0, 0, IconH, 16, 16, 0, bmp.Canvas.Brush.Handle, DI_NORMAL); //Replaced DrawIcon
        end;
        Canvas.Draw(R.Left, R.Top, bmp);
      finally
        bmp.Free;
      end;

      R.Left := R.Left + 20;
    end;
    szFooterText := StringReplace(FTaskDialog.Footer,'\n','<br>',[rfReplaceAll]);
    szFooterText := StringReplace(szFooterText,#10,'<br>',[rfReplaceAll]);    

    HTMLDrawEx(Canvas, szFooterText, R, nil, 0, 0, -1, -1, 1, false, false, false, false, False, false,
       true, 1.0, clBlue, clNone, clNone, clGray, anchor, stripped, focusanchor, xsize, ysize,
       hyperlinks, mouselink, re, nil , nil, 0);
  end;
end;

//------------------------------------------------------------------------------

function TAdvMessageForm.GetFooterRect: TRect;
begin
  Result := Rect(-1, -1, -1, -1);
  if Assigned(FTaskDialog) and (FTaskDialog.Footer <> '') then
  begin
    Result := Rect(FHorzMargin, ClientHeight - FFooterYSize-10, ClientWidth - FHorzMargin, ClientHeight);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvMessageForm.Paint;
var
  i: Integer;
  R: TRect;
  s: string;
  VerInfo: TOSVersionInfo;

begin
  inherited;

  i := FWhiteWindowHeight;

  {if (FcmBtnList.Count > 0) then
    i := TButton(FcmBtnList.Items[0]).Top
  else if (FcsBtnList.Count > 0) then
    i := TButton(FcsBtnList.Items[0]).Top;}

  VerInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  GetVersionEx(verinfo);

  if (i > 0) then
  begin
    R := ClientRect;
    R.Top := i - (FVertSpacing div 2) ;
    Canvas.Brush.Color := FooterColor;
    Canvas.FillRect(R);
    Canvas.Pen.Color := FooterLineColor;
    Canvas.MoveTo(R.Left, R.Top);
    Canvas.LineTo(R.Right, R.Top);
    R := ClientRect;
    Canvas.Brush.Style := bsClear;

    if (verinfo.dwMajorVersion >= 6) then
      Canvas.Pen.Style := psClear
    else
      Canvas.Pen.Style := psSolid;

    if DRAWBORDER and not IsVista then // only draw on non Vista
    begin
      Canvas.Pen.Color := clGray;
      Canvas.Rectangle(R.Left + 1, R.Top + 1, R.Right - 1, R.Bottom - 1);
    end;
    Canvas.Pen.Style := psSolid;
  end;

  DrawContent;
  DrawExpandedText;
  
  if Assigned(FTaskDialog) and (FTaskDialog.ExpandedText <> '') and Assigned(FExpandButton) then
  begin
    if not FExpanded then
      s := FTaskDialog.CollapsControlText
    else
      s := FTaskDialog.ExpandControlText;

    Canvas.Font.Color := FTaskDialog.FNormalFontColor;
    Canvas.Brush.Style := bsClear;
    R := Rect(FExpandButton.Left + FExpandButton.Width + FHorzSpacing - 5, FExpandButton.Top, ClientRect.Right, FExpandButton.Top + FExpandButton.Height); 
    DrawText(Canvas.Handle,PChar(s),Length(s), R, DT_SINGLELINE or DT_LEFT or DT_VCENTER);
  end;
  DrawFooter;
end;

//------------------------------------------------------------------------------

function TAdvMessageForm.IsAnchor(x, y: integer): string;
var
  r: trect;
  xsize, ysize: integer;
  anchor, stripped: string;

  HyperLinks,MouseLink: Integer;
  Focusanchor: string;
  re: TRect;
  AText: String;
begin
  Result := '';
  if not Assigned(FTaskDialog) then
    Exit;

  AText := '';
  R := GetFooterRect;
  if PtInRect(R, Point(X, Y)) then
  begin
    if Assigned(FFooterIcon) then
    begin
      R.Left := R.Left + 20;
    end;
    AText := FTaskDialog.Footer;
  end
  else
  begin
    R := GetContentRect;
    if PtInRect(R, Point(X, y)) then
      AText := FTaskDialog.Content
    else
    begin
      R := GetExpTextRect;
      if PtInRect(R, Point(X, y)) then
        AText := FTaskDialog.ExpandedText;
    end;
  end;

  AText := StringReplace(AText,'\n','<br>',[rfReplaceAll,rfIgnoreCase]);
  AText := StringReplace(AText,#10,'<br>',[rfReplaceAll,rfIgnoreCase]);

  Anchor := '';
  if (AText <> '') then
  begin
    if HTMLDrawEx(Canvas, AText, r, nil, x, y, -1, -1, 1, true, false, false, true, true, false, true,
       1.0, clBlue, clNone, clNone, clGray, anchor, stripped, focusanchor, xsize, ysize, hyperlinks,
       mouselink, re, nil, nil, 0) then
      Result := anchor;
  end;
end;

procedure TAdvMessageForm.KeyDown(var Key: Word; Shift: TShiftSTate);
var
  s: string;
begin
  inherited;
  if (Key = VK_F1) then
  begin
    if FTaskDialog.HelpContext <> 0 then
      Application.HelpContext(FTaskDialog.HelpContext);
  end;
  if (Key = ord('C')) and (ssCtrl in Shift) then
  begin
    // got ctrl-c
    s := FTaskDialog.FTitle + #13#10;
    s := s + FTaskDialog.FInstruction + #13#10;
    s := s + FTaskDialog.FContent;
    if FExpanded then
       s := s + #13#10 + #13#10 + FTaskDialog.ExpandedText;
    clipboard.Open;
    clipboard.AsText := s;
    clipboard.Close;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvMessageForm.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Anchor: string;
begin
  inherited;
  Anchor := IsAnchor(X, Y);
  if Anchor <> '' then
  begin
    if not Assigned(FTaskDialog.OnDialogHyperlinkClick) then
    begin
      if (Pos('://', anchor) > 0) then
        VistaShellOpen(0, 'iexplore.exe', Anchor);
    end;

    if Assigned(FTaskDialog.OnDialogHyperlinkClick) then
      FTaskDialog.OnDialogHyperlinkClick(FTaskDialog, Anchor);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvMessageForm.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  anchor: string;
begin
  anchor := IsAnchor(x, y);
  if (Anchor <> '') then
  begin
    if (self.Cursor = crDefault) or (fAnchor <> Anchor) then
    begin
      fAnchor := Anchor;
      self.Cursor := crHandPoint;
      //if fAnchorHint then
        //Application.CancelHint;
      //if Assigned(fAnchorEnter) then fAnchorEnter(self, anchor);
    end;
  end
  else
  begin
    if (self.Cursor = crHandPoint) then
    begin
      self.Cursor := crDefault;
      //if assigned(fAnchorExit) then fAnchorExit(self, anchor);
    end;
  end;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvMessageForm.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvMessageForm.OnTimer(Sender: TObject);
var
  State: TTaskDialogProgressState;
  Pos: Integer;
begin
  if Assigned(FTaskDialog) then
  begin
    FTaskDialog.FAutoCloseCounter := FTaskDialog.FAutoCloseCounter + 100;

    if (doTimer in FTaskDialog.Options) and Assigned(FTaskDialog.OnDialogTimer) then
      FTaskDialog.OnDialogTimer(FTaskDialog);

    if Assigned(FTaskDialog.OnDialogProgress) then
    begin
      Pos := FProgressBar.Position;
      FTaskDialog.OnDialogProgress(FTaskDialog, Pos, State);
      FProgressBar.Position := Pos;
    end;

    if (FTaskDialog.FAutoCloseCounter > FTaskDialog.AutoCloseTimeOut) and
       (FTaskDialog.AutoCloseTimeOut > 0) and (FTaskDialog.AutoClose) then
    begin
      FTaskDialog.DoAutoClose;
      FTaskDialog.Close;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvMessageForm.ClickButton(ButtonID: integer);
var
  Btn: TButton;
  TaskBtn: TTaskDialogButton;
begin
  TaskBtn := nil;
  Btn := GetButton(ButtonID, TaskBtn);
  if Assigned(Btn) then
    Btn.Click
  else if Assigned(TaskBtn) then
    TaskBtn.Click;
end;

//------------------------------------------------------------------------------

procedure TAdvMessageForm.EnableButton(ButtonID: integer;
  Enabled: boolean);
var
  Btn: TButton;
  TaskBtn: TTaskDialogButton;
begin
  TaskBtn := nil;
  Btn := GetButton(ButtonID, TaskBtn);
  if Assigned(Btn) then
    Btn.Enabled := Enabled
  else if Assigned(TaskBtn) then
    TaskBtn.Enabled := Enabled;
end;

//------------------------------------------------------------------------------

function TAdvMessageForm.GetButton(ButtonID: Integer; var TaskButton: TTaskDialogButton): TButton;
var
  i, j: Integer;
begin
  j := 0;
  Result := nil;
  for i := 0 to FcmBtnList.Count-1 do
  begin
    Inc(j);
    if (j >= ButtonID) then
    begin
      TButton(FcmBtnList.Items[i]).Enabled := Enabled;
      Result := TButton(FcmBtnList.Items[i]);
      break;
    end;
  end;

  if not Assigned(Result) then
  begin
    j := 99;
    for i := 0 to FcsBtnList.Count-1 do
    begin
      Inc(j);
      if (j >= ButtonID) then
      begin
        if (doCommandLinks in FTaskDialog.Options) then
        begin
          TTaskDialogButton(FcsBtnList.Items[i]).Enabled := Enabled;
          TaskButton := TTaskDialogButton(FcsBtnList.Items[i]);
        end
        else
        begin
          TButton(FcsBtnList.Items[i]).Enabled := Enabled;
          Result := TButton(FcsBtnList.Items[i]);
        end;
        break;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TTaskDialogButton.Click;
var
  Form: TCustomForm;
begin
  Form := GetParentForm(Self);
  if Form <> nil then
    Form.ModalResult := ModalResult;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvMessageForm.OnVerifyClick(Sender: TObject);
begin
  if not Assigned(FTaskDialog) or not Assigned(FVerificationCheck) then
    Exit;

  FTaskDialog.VerifyResult := FVerificationCheck.Checked;

  if Assigned(FVerificationCheck) and Assigned(FTaskDialog.OnDialogVerifyClick) then
    FTAskDialog.OnDialogVerifyClick(FTaskDialog, FVerificationCheck.Checked);
end;

//------------------------------------------------------------------------------

procedure TAdvMessageForm.OnRadioClick(Sender: TObject);
begin
  if not Assigned(FTaskDialog) or not Assigned(FRadioList) then
    Exit;

  FTaskDialog.RadioButtonResult := FRadioList.IndexOf(Sender) + 200;  
  if Assigned(FTaskDialog) and Assigned(FTaskDialog.OnDialogRadioClick) then
    FTAskDialog.OnDialogRadioClick(FTaskDialog, FTaskDialog.RadioButtonResult);
end;

//------------------------------------------------------------------------------

procedure TAdvMessageForm.DoUpdateInput;
var
  al: integer;
  s: string;
  a: array of char;

begin
  case FTaskDialog.InputType of
  itEdit, itPassword: FTaskDialog.InputText := FInputEdit.Text;
  itComboEdit, itComboList:
    begin
      FTaskDialog.InputText := FInputCombo.Text;
      FTaskDialog.InputItemIndex := FInputCombo.ItemIndex;
    end;
  itDate:
    begin
      FTaskDialog.InputDateTime := FInputDate.DateTime;
      FTaskDialog.InputText := DateToStr(FInputDate.Date);
    end;
  itTime:
    begin
      FTaskDialog.InputDateTime := FInputDate.DateTime;
      FTaskDialog.InputText := TimeToStr(FInputDate.Time);
    end;
  itMemo: FTaskDialog.InputText := FInputMemo.Lines.Text;
  itCustom:
    begin
      if Assigned(FTaskDialog.InputControl) and FTaskDialog.InputControl.HandleAllocated then
      begin
        al := GetWindowTextLength(FTaskDialog.InputControl.Handle) + 2;
        SetLength(a, al);
        GetWindowText(FTaskDialog.InputControl.Handle, PChar(a), al);
        s := strpas(PChar(a));
        if Assigned(FTaskDialog.OnDialogInputGetText) then
        begin
          s := '';
          FTaskDialog.OnDialogInputGetText(Self, s);
        end;
        FTaskDialog.InputText := s;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvMessageForm.DoClose(var Action: TCloseAction);
var
  CanClose: Boolean;
  idlg: TAdvInputTaskDialog;
  s: string;

begin
  DoUpdateInput;

  CanClose := True;

  if Assigned(FTaskDialog) and (FTaskDialog is TAdvInputTaskDialog) then
  begin
    idlg := (FTaskDialog as TAdvInputTaskDialog);
    s := FTaskDialog.InputText;
    if Assigned(idlg.OnValidateInputText) then
    begin
      idlg.OnValidateInputText(idlg, s, idlg.FData, ModalResult, CanClose);
      if (s <> FTaskDialog.InputText) and not CanClose then
      begin
        FTaskDialog.InputText := s;
        case FTaskDialog.InputType of
        itEdit, itPassword: FInputEdit.Text := s;
        itComboEdit: FInputCombo.Text := s;
        itDate: if FTaskDialog.InputDateTime = 0  then FInputDate.DateTime := StrToDate(s);
        itTime: if FTaskDialog.InputDateTime = 0  then FInputDate.DateTime := StrToTime(s);
        itMemo: FInputMemo.Lines.Text := s;
        end;
      end;
    end;
  end;

  if Assigned(FTaskDialog) and Assigned(FTaskDialog.OnDialogClose) then
  begin
    FTaskDialog.OnDialogClose(FTaskDialog, CanClose);
  end;

  if not CanClose then
    Action := caNone;

  inherited;

  if Action = caNone then
  begin
    case FTaskDialog.InputType of
    itEdit, itPassword: FInputEdit.SetFocus;
    itComboEdit: FInputCombo.SetFocus;
    itDate,itTime: FInputDate.SetFocus;
    itMemo: FInputMemo.SetFocus;
    itCustom: if Assigned(FTaskDialog.InputControl) then
      begin
        if (FTaskDialog.InputControl.TabStop) then
          FTaskDialog.InputControl.SetFocus;
      end;
    end;
  end;

  if CanClose and (FTaskDialog.InputType = itCustom) and
    Assigned(FTaskDialog.InputControl) then
  begin
    FTaskDialog.InputControl.Visible := false;
    FTaskDialog.InputControl.Parent := FOldParent;
  end;
end;

procedure TAdvMessageForm.DoShow;
var
  defBtn,i: integer;
begin
  inherited;

  defBtn := -1;

  if FTaskDialog.DefaultButton <> -1 then
  begin
    if (FTaskDialog.DefaultButton - 100 >= 0) and (FTaskDialog.DefaultButton - 100 < FTaskDialog.CustomButtons.Count) then
       defBtn := FTaskDialog.DefaultButton - 100;
  end;

  // no common buttons, only custom buttons
  if (FTaskDialog.CommonButtons = []) and (FTaskDialog.CustomButtons.Count > 0) and (FTaskDialog.DefaultButton < 100) then
    defBtn := 0;

  if defBtn <> -1 then
  begin
    if (docommandLinks in FTaskDialog.Options) then
    begin
      if TTaskDialogButton(FcsBtnList[defBtn]).Enabled then
        TTaskDialogButton(FcsBtnList[defBtn]).SetFocus
    end
    else
    begin
      if TCustomControl(FcsBtnList[defBtn]).Enabled then
        TCustomControl(FcsBtnList[defBtn]).SetFocus;
    end;
  end
  else
  begin
    for i := 0 to FCmBtnList.Count - 1 do
    begin
      if TCustomControl(FcmBtnList[i]).Tag = 1 then
      begin
        if TCustomControl(FcmBtnList[i]).Enabled then
          TCustomControl(FcmBtnList[i]).SetFocus;
      end;
    end;
  end;


  case FTaskDialog.InputType of
  itEdit, itPassword: FInputEdit.SetFocus;
  itComboEdit, itComboList: FInputCombo.SetFocus;
  itDate,itTime: FInputDate.SetFocus;
  itMemo: FInputMemo.SetFocus;
  itCustom: if FTaskDialog.InputControl.TabStop then
              FTaskDialog.InputControl.SetFocus;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvMessageForm.OnButtonClick(Sender: TObject);
begin
  if not Assigned(FTaskDialog) or not Assigned(FcsBtnList) then
    Exit;

  DoUpdateInput;

  if Assigned(FTaskDialog) and Assigned(FTaskDialog.OnDialogButtonClick) then
  begin
    if FcsBtnList.IndexOf(Sender) >= 0 then
      FTaskDialog.OnDialogButtonClick(FTaskDialog, FcsBtnList.IndexOf(Sender) + 100)
    else
      FTaskDialog.OnDialogButtonClick(FTaskDialog, FTaskDialog.GetButtonModalResult(Sender as TWinControl));
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvMessageForm.CMDialogChar(var Message: TCMDialogChar);
var
  I: Integer;
begin
  if Assigned(FTaskDialog) and (docommandLinks in FTaskDialog.Options) then
  begin
    for I := 0 to FcsBtnList.Count-1 do
    begin
      if (TControl(FcsBtnList[I]) is TTaskDialogButton) and IsAccel(Message.CharCode, TTaskDialogButton(FcsBtnList[I]).Caption) and CanFocus then
      begin
        TTaskDialogButton(FcsBtnList[I]).Click;
        Message.Result := 1;
        Exit;
      end;
    end;
  end;

  if (FTaskDialog.ExpandControlText <> '') and Expanded then
  begin
    if IsAccel(Message.CharCode, FTaskDialog.FExpandControlText) then
    begin
      OnExpandButtonClick(Self);
    end;
  end
  else
    if (FTaskDialog.CollapsControlText <> '') and not Expanded then
      if IsAccel(Message.CharCode, FTaskDialog.FCollapsControlText) then
      begin
        OnExpandButtonClick(Self);
      end;

  inherited;


  if Assigned(FTaskDialog) and (doAllowDialogCancel in FTaskDialog.Options) and (Message.CharCode = VK_ESCAPE) then
  begin
    Self.Close;
  end;
end;


function CoreShowmessage(
           const Title,    // dialog window title
           Instruction,    // the part of the message shown in blue
           content,        // additional message if desired
           verify: string; // ex Do Not Show this Again
           tiIcon: tTaskDialogIcon): boolean;
var
  td:  TCustomAdvTaskDialog;
begin
  td := TCustomAdvTaskDialog.Create(application);
  td.Title := Title;
  td.Instruction := instruction;
  td.Content := Content;
  td.VerificationText := verify;
  td.icon := tiIcon;
  if td.VerifyResult then
    td.Options := td.Options + [doVerifyChecked];

  td.Execute;
  result := (verify <> '') and td.VerifyResult;
  td.free;
end {CoreShowmessage};

//=====================================================================
// This returns false unless verify is not blank AND the verify checkbox 
// was not checked.
//---------------------------------------------------------------------
function AdvShowMessage(
           const Title,    // dialog window title
           Instruction,    // the part of the message shown in blue
           content,        // additional message if desired
           verify: string; // ex Do Not Show this Again
           tiIcon: tTaskDialogIcon): boolean; overload;
begin
  result := coreShowmessage(title, instruction,content,verify,tiIcon);
end { tmsShowMessage };

function AdvShowmessage(const Instruction: string):boolean; overload;
begin // Only instruction . tiInformation
  result := CoreShowMessage('',Instruction,'','',tiInformation);
end;

function AdvShowmessage(const Title, Instruction: string):boolean; overload;
begin // title, instruction tiInformation
  result := CoreShowMessage(Title,Instruction,'','',tiInformation);
end;

function AdvShowmessage(const Title, Instruction: string;tiIcon: TTaskDialogIcon): boolean; overload;
begin
  result := CoreShowMessage(Title,Instruction,'','',tiIcon);
end;

function AdvShowMessageFmt(const Instruction: string; Parameters: array of const): boolean;
begin
  Result := AdvShowmessage(Format(Instruction,Parameters));
end;

function AdvMessageBox(hWnd: HWND; lpInstruction, lpTitle: PChar; flags: UINT; NonNative: boolean = false): Integer;
const
  MB_CANCELTRYCONTINUE = $00000006;  // missing from windows unit so probably never be used
var
  td:   TCustomAdvTaskDialog;
  res:  integer;
  def:  integer;
  num:  integer;
  task: tCommonButton;
  txt:  string;
begin
  td := TCustomAdvTaskDialog.Create(application);
  td.Title := lptitle;
  td.instruction := lpInstruction;
  if NonNative then
    td.NonNativeDialog := nndAlways;

  // extract the icon from flags
  case MB_ICONMASK and flags of
    MB_ICONEXCLAMATION: td.Icon := tiWarning;     // Exclamation mark= MB_ICONWARNING
    MB_ICONINFORMATION: td.Icon := tiInformation; // Circled I = MB_ICONASTERISK
    MB_ICONQUESTION:    td.Icon := tiQuestion;    // Question (api says don't use any more
    MB_ICONSTOP:        td.Icon := tiError;       //Stop sign = MB_ICONERROR & MB_ICONHAND
  end;

  // extract the buttons from flags
  //   MessageBox() Flags from Windows help file
  //  MB_ABORTRETRYIGNORE
  //  The message box contains three push buttons: Abort, Retry, and Ignore.
  //  MB_CANCELTRYCONTINUE
  //  Microsoft Windows 2000/XP: The message box contains three push buttons: Cancel, Try Again, Continue. Use this message box type instead of MB_ABORTRETRYIGNORE.
  //  MB_HELP
  //  Windows 95/98/Me, Windows NT 4.0 and later: Adds a Help button to the message box. When the user clicks the Help button or presses F1, the system sends a WM_HELP message to the owner.
  //  MB_OK
  //  The message box contains one push button: OK. This is the default.
  //  MB_OKCANCEL
  //  The message box contains two push buttons: OK and Cancel.
  //  MB_RETRYCANCEL
  //  The message box contains two push buttons: Retry and Cancel.
  //  MB_YESNO
  //  The message box contains two push buttons: Yes and No.
  //  MB_YESNOCANCEL
  //  The message box contains three push buttons: Yes, No, and Cancel.
  td.Commonbuttons := [];
  txt := '';
  case MB_TYPEMASK and flags of
    MB_ABORTRETRYIGNORE:  txt := SAbortButton + #10 + SRetryButton + #10 + SIgnoreButton;
    MB_CANCELTRYCONTINUE: txt :=  SCancelButton + #10 + SRetryButton + #10 + SContinue;
    MB_OK:                td.Commonbuttons := [cbOK];
    MB_RETRYCANCEL:       txt := SRetryButton + #10 + SCancelButton;
    MB_OKCANCEL:          td.CommonButtons := [cbOK,cbCancel];
    MB_YESNOCANCEL:       td.Commonbuttons := [cbYes, cbNO, cbCancel];
    MB_YESNO:             td.CommonButtons := [cbYes, cbNO];
  end;



  if MB_HELP and flags <> 0 then
  begin
    if length(txt) > 0 then
      txt := txt + #10;
    txt := txt + SHelpButton;
  end;
  if txt <> '' then
    td.CustomButtons.text := txt;

  // deal with mbDefbutton1, 2, 3 & 4
  def := 0;
  if mb_DefButton1 and flags <> 0 then
    def := 1;
  if mb_DefButton2 and flags <> 0 then
    def := 2;
  if mb_DefButton3 and flags <> 0 then
    def := 3;
  if mb_DefButton4 and flags <> 0 then
    def := 4;
  if def > 0 then
  begin // have to set default button
    num := td.CustomButtons.count;
    if num <= def  then
      td.DefaultButton := 99 + def
    else
    begin
      // I think this compiles on supported delphi compilers
      for task := cbOK to cbClose do
      begin
        if task in td.CommonButtons then
        begin
          inc(num);
          if num = def then
          begin
            case task of
              cbOK:     td.Defaultbutton := idOK;
              cbYes:    td.Defaultbutton := idYES;
              cbNo:     td.Defaultbutton := idNO;
              cbCancel: td.Defaultbutton := idCANCEL;
              cbRetry:  td.Defaultbutton := idRETRY;
              cbClose:  td.Defaultbutton := idCLOSE;
            end;
            break;
          end;
        end;
      end;
    end;
  end;

  if (cbCancel in td.CommonButtons) then
    td.Options := td.Options + [doAllowDialogCancel];

  // Deal with mbAppModal, mbSystemModal and mbtaskModal
  //   not sure what to do with these (I personally haven't used them.
  result := 0;
  res := td.Execute;
  case res of
    1: result := IDOK;
    2: result := IDCANCEL;
    3: result := IDABORT;
    4: result := IDRETRY;
    5: result := IDIGNORE;
    6: result := IDYES;
    7: result := IDNO;
    else
    begin
      case MB_TYPEMASK and flags of
        MB_ABORTRETRYIGNORE:
          case res of
           100:  result := IDABORT;
           101:  result := IDRETRY;
           102:  result := IDIGNORE;
          end;
        MB_CANCELTRYCONTINUE:
          case res of
           100:  result := IDCANCEL;
           {$IFDEF DELPHI9_LVL}
           101:  result := IDTRYAGAIN;
           102:  result := IDCONTINUE;
           {$ENDIF}
          end;
        MB_RETRYCANCEL:
          case res of
            100: result := IDRETRY;
            101: result := IDCANCEL;
          end;
      end;
    end;
  end;
  td.Free;
end;

//==================================================================================================

function AdvTaskMessageDlg(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint): Integer;
begin
  Result := AdvTaskMessageDlgPosHelp(Title, Msg, DlgType, Buttons, HelpCtx, -1, -1, '');
end;

//--------------------------------------------------------------------------------------------------

function AdvTaskMessageDlg(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; DefaultButton: TMsgDlgBtn): Integer; overload;
begin
  Result := AdvTaskMessageDlgPosHelp(Title, Msg, DlgType, Buttons, HelpCtx,
    -1, -1, '', DefaultButton);
end;

//--------------------------------------------------------------------------------------------------

function AdvTaskMessageDlgPos(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; X, Y: Integer): Integer;
begin
  Result := AdvTaskMessageDlgPosHelp(Title, Msg, DlgType, Buttons, HelpCtx, X, Y, '');
end;

//--------------------------------------------------------------------------------------------------

function AdvTaskMessageDlgPos(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; X, Y: Integer;
  DefaultButton: TMsgDlgBtn): Integer; overload;
begin
  Result := AdvTaskMessageDlgPosHelp(Title, Msg, DlgType, Buttons, HelpCtx,
    X, Y, '', DefaultButton);
end;

//--------------------------------------------------------------------------------------------------

function AdvTaskMessageDlgPosHelp(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; X, Y: Integer;
  const HelpFileName: string): Integer;
begin
  Result := AdvTaskMessageDlgPosHelp(Title, Msg, DlgType, Buttons, HelpCtx, X, Y,
    HelpFileName, mbYes);
end;


function AdvMessageDlg(const Instruction: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint): Integer; overload;
begin
  // passes mbHelp as the default button since we can't deal with help anyway
  Result := AdvMessageDlg(Instruction,Dlgtype,Buttons,HelpCtx,mbHelp);
end;

function AdvMessageDlg(const Instruction: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; DefaultButton: TMsgDlgBtn): Integer; overload;
var
  td:  TCustomAdvTaskDialog;
  ray: array[0..3] of integer;
  res: integer;
begin
  td := TCustomAdvTaskDialog.Create(Application);
  td.Instruction := instruction;

  case DlgType of
    mtWarning:
      begin
        td.Icon := tiWarning;
        td.Title := SMsgDlgWarning;
      end;
    mtError:
      begin
        td.Icon := tiError;
        td.Title := SMsgDlgError;
      end;
    mtInformation:
      begin
        td.Icon := tiInformation;
        td.Title := SMsgDlgInformation;
      end;
    mtConfirmation:
      begin
        td.Icon := tiQuestion;
        td.Title := SMsgDlgConfirm;
      end;
  end;

  fillchar(ray,sizeof(ray),0);
  td.CommonButtons := [];

//  TMsgDlgBtn = (mbYes, mbNo, mbOK, mbCancel, mbAbort, mbRetry, mbIgnore,
//    mbAll, mbNoToAll, mbYesToAll, mbHelp);

  if (mbYes in Buttons) then
    td.CommonButtons := td.CommonButtons + [cbYes];

  if (mbNo in Buttons) then
    td.CommonButtons := td.CommonButtons + [cbNo];

  if (mbOK in Buttons) then
    td.CommonButtons := td.CommonButtons + [cbOK];

  if (mbCancel in Buttons) then
    td.CommonButtons := td.CommonButtons + [cbCancel];

  if (mbAbort in Buttons) then
    td.CommonButtons := td.CommonButtons + [cbClose];

  if (mbRetry in Buttons) then
    td.CommonButtons := td.CommonButtons + [cbRetry];

  if (mbIgnore in Buttons) then
  begin
    td.CustomButtons.Add(SMsgDlgIgnore);
    ray[0] := mrIgnore;
  end;

  if (mbAll in Buttons) then
  begin
    ray[td.custombuttons.Count] := mrALL;
    td.CustomButtons.Add(SMsgDlgAll);
  end;

  if (mbNoToAll in buttons) then
  begin
    ray[td.custombuttons.Count] := mrNoToAll;
    td.CustomButtons.Add(SMsgDlgNoToAll);
  end;

  if (mbYesToAll in buttons) then
  begin
    ray[td.custombuttons.Count] := mrYesToAll;
    td.Custombuttons.Add(SMsgDlgYesToAll);
  end;

  if (mbHelp in buttons) then
  begin
    ray[td.Custombuttons.Count] := mrNone;
    td.Custombuttons.Add(SMsgDlgHelp);
  end;

  case DefaultButton of
  mbYes: td.DefaultButton := integer(mrYes);
  mbNo: td.DefaultButton := integer(mrNo);
  mbCancel: td.DefaultButton := integer(mrCancel);
  mbOK: td.DefaultButton := integer(mrOK);
  mbAbort: td.DefaultButton := integer(mrAbort);
  mbRetry: td.DefaultButton := integer(mrRetry);
  mbIgnore: td.DefaultButton := integer(mrIgnore);
  end;

  td.HelpContext := HelpCtx;
  if (cbCancel in td.CommonButtons) then
    td.Options := td.Options + [doAllowDialogCancel];


  result := 0;
  res := td.Execute;

  case res of
  1: Result := mrOk;
  2: Result := mrCancel;
  3: Result := mrAbort;
  4: Result := mrRetry;
  6: Result := mrYes;
  7: Result := mrNo;
  else
    if (res > 99) and (res < 100 + high(ray)) then
    begin
      result := ray[res - 100];

      if (Result = mrNone) and (td.HelpContext > 0) then
      begin
        Application.HelpContext(td.HelpContext);
      end;
    end;
  end;
end;


//--------------------------------------------------------------------------------------------------

function AdvTaskMessageDlgPosHelp(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; X, Y: Integer;
  const HelpFileName: string; DefaultButton: TMsgDlgBtn): Integer;
var
  td:  TAdvTaskDialog;
  ray: array[0..3] of integer;
  res: integer;
  p: TCustomForm;
begin
  p := nil;
  if Assigned(Screen.ActiveControl) then
  {$IFDEF DELPHI9_LVL}
    p := GetParentForm(Screen.ActiveControl, True);
  {$ENDIF}
  {$IFNDEF DELPHI9_LVL}
    p := GetParentForm(Screen.ActiveControl);
  {$ENDIF}

  if Assigned(p) then
    td := TAdvTaskDialog.Create(p)
  else
    td := TAdvTaskDialog.Create(Application);

  try
    td.ApplicationIsParent := not Assigned(p);
    td.Instruction := Title;
    td.Content := msg;

    case DlgType of
      mtWarning:
        begin
          td.Icon := tiWarning;
          td.Title := SMsgDlgWarning;
        end;
      mtError:
        begin
          td.Icon := tiError;
          td.Title := SMsgDlgError;
        end;
      mtInformation:
        begin
          td.Icon := tiInformation;
          td.Title := SMsgDlgInformation;
        end;
      mtConfirmation:
        begin
          td.Icon := tiQuestion;
          td.Title := SMsgDlgConfirm;
        end;
    end;

    fillchar(ray,sizeof(ray),0);
    td.CommonButtons := [];

  //  TMsgDlgBtn = (mbYes, mbNo, mbOK, mbCancel, mbAdvrt, mbRetry, mbIgnore,
  //    mbAll, mbNoToAll, mbYesToAll, mbHelp);

    if (mbYes in Buttons) then
      td.CommonButtons := td.CommonButtons + [cbYes];

    if (mbNo in Buttons) then
      td.CommonButtons := td.CommonButtons + [cbNo];

    if (mbOK in Buttons) then
      td.CommonButtons := td.CommonButtons + [cbOK];

    if (mbCancel in Buttons) then
      td.CommonButtons := td.CommonButtons + [cbCancel];

    if (mbAbort in Buttons) then
      td.CommonButtons := td.CommonButtons + [cbClose];

    if (mbRetry in Buttons) then
      td.CommonButtons := td.CommonButtons + [cbRetry];


    if (mbIgnore in Buttons) then
    begin
      td.CustomButtons.Add(SMsgDlgIgnore);
      ray[0] := mrIgnore;
    end;

    if (mbAll in Buttons) then
    begin
      ray[td.custombuttons.Count] := mrALL;
      td.CustomButtons.Add(SMsgDlgAll);
    end;

    if (mbNoToAll in buttons) then
    begin
      ray[td.custombuttons.Count] := mrNoToAll;
      td.CustomButtons.add(SMsgDlgNoToAll);
    end;

    if (mbYesToAll in buttons) then
    begin
      ray[td.custombuttons.Count] := mrYesToAll;
      td.Custombuttons.Add(SMsgDlgYesToAll);
    end;

    if (mbHelp in buttons) then
    begin
      ray[td.Custombuttons.Count] := mrNone;
      td.Custombuttons.Add(SMsgDlgHelp);
    end;

    case DefaultButton of
    mbYes: td.DefaultButton := integer(mrYes);
    mbNo: td.DefaultButton := integer(mrNo);
    mbCancel: td.DefaultButton := integer(mrCancel);
    mbOK: td.DefaultButton := integer(mrOK);
    mbAbort: td.DefaultButton := integer(mrAbort);
    mbRetry: td.DefaultButton := integer(mrRetry);
    mbIgnore: td.DefaultButton := integer(mrIgnore);
    end;

    td.HelpContext := HelpCtx;
    if (cbCancel in td.CommonButtons) then
      td.Options := td.Options + [doAllowDialogCancel];

    Result := 0;

    if (X >= 0) and (Y >= 0) then
      td.NonNativeDialog := nndAlways;

    res := td.ExecutePos(X,Y);
    case res of
    1: Result := mrOk;
    2: Result := mrCancel;
    3: Result := mrAbort;
    4: Result := mrRetry;
    6: Result := mrYes;
    7: Result := mrNo;
    else
      if (res > 99) and (res < 100+high(ray)) then
      begin
        result := ray[res-100];

        if (Result = mrNone) and (td.HelpContext > 0) then
        begin
          Application.HelpContext(td.HelpContext);
        end;
      end;
    end;
  finally
    td.Free;
  end;
end;


function AdvInputQueryDlg(ACaption, APrompt: string; var Value: string):boolean;
var
  AID: TAdvInputTaskDialog;
begin
  AID := TAdvInputTaskDialog.Create(Application);
  AID.Instruction := APrompt;
  AID.Title := ACaption;
  AID.InputText := Value;
  AID.InputType := itEdit;
  AID.CommonButtons := [cbOK, cbCancel];
  Result := AID.Execute = mrOK;
  Value := AID.InputText;
end;

//------------------------------------------------------------------------------

procedure Register;
begin
  RegisterComponents('TMS',[TAdvTaskDialog, TAdvInputTaskDialog]);
end;

//------------------------------------------------------------------------------


{ TAdvInputTaskDialog }

constructor TAdvInputTaskDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FInputType := itEdit;
  FInputDropDownCount := 8;
  Options := Options + [doHyperLinks];
  if (cbCancel in CommonButtons) then
    Options := Options + [doAllowDialogCancel];
end;

function TAdvInputTaskDialog.Execute: integer;
begin
  if ExpandedDefault then
    Options := Options + [doExpandedDefault]
  else
    Options := Options - [doExpandedDefault];

  if VerifyResult then
    Options := Options + [doVerifyChecked]
  else
    Options := Options - [doVerifyChecked];


  Result := AdvMessageDlgPos(Self, -1, -1);
end;


initialization
  //cbOK, cbYes, cbNo, cbCancel, cbRetry, cbClose);
  ButtonCaptions[cbOK] := @SMsgDlgOK;
  ButtonCaptions[cbYes] := @SMsgDlgYes;
  ButtonCaptions[cbNo] := @SMsgDlgNo;
  ButtonCaptions[cbCancel] := @SMsgDlgCancel;
  ButtonCaptions[cbRetry] := @SMsgDlgRetry;
  ButtonCaptions[cbClose] := @SMsgDlgAbort;

  Captions[tiBlank] := nil;
  Captions[tiWarning] := @SMsgDlgWarning;
  Captions[tiQuestion] := @SMsgDlgConfirm;
  Captions[tiError] := @SMsgDlgError;
  Captions[tiShield] := @SMsgDlgInformation;


{$IFDEF FREEWARE}
   if  (FindWindow('TApplication', nil) = 0) OR
       (FindWindow('TAppBuilder', nil) = 0) then
   begin
     MessageBox(0,'Application uses trial version of TMS components','Info',MB_OK);
   end
{$ENDIF}


end.

