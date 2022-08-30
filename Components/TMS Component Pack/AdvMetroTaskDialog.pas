{***************************************************************************}
{ TAdvMetroTaskDialog component                                             }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2012 - 2014                                        }
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

unit AdvMetroTaskDialog;

{$I TMSDEFS.INC}

interface

uses
  Classes, Windows, Messages, Forms, Dialogs, SysUtils, StdCtrls, Graphics,
  Consts, Math, ExtCtrls, Controls, ComCtrls, PictureContainer, ComObj,
  ShellAPI, CommCtrl, ClipBrd, ImgList, AdvMetroForm, AdvGlowButton, AdvStyleIF,
  AdvGDIPicture, AdvMetroRes, AdvMetroProgressBar, GDIPicture, Types
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;

const

  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 3; // Build nr.

  // version history
  // 1.0.0.0 : First release
  // 1.0.0.1 : Fixed : Issue with content position when Instruction is not used
  // 1.0.0.2 : Fixed : Issue with appearance when VCL styles are used
  // 1.0.0.3 : Fixed : Issue with default & cancel button assignment

type
  TTaskDialogResult = (trNone, trOk, trCancel);

  TTaskDialogOption = (doHyperlinks, doCommandLinks, doCommandLinksNoIcon, doExpandedDefault,
    doExpandedFooter, doAllowMinimize, doVerifyChecked, doProgressBar, doTimer,
    doNoDefaultRadioButton, doAllowDialogCancel);

  TTaskDialogOptions = set of TTaskDialogOption;

  TTaskDialogIcon = (tiBlank, tiWarning, tiQuestion, tiError, tiInformation, tiNotUsed, tiShield);
                    //(mtWarning, mtError, mtInformation, mtConfirmation, mtCustom);
  TTaskDialogFooterIcon = (tfiBlank, tfiWarning, tfiQuestion, tfiError, tfiInformation, tfiShield);

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

  TInputType = (itEdit, itMemo, itComboEdit, itComboList, itDate, itCustom, itNone);

  TInputGetTextEvent = procedure(Sender: TObject; var Text: string) of object;
  TInputSetTextEvent = procedure(Sender: TObject; Text: string) of object;

  TCustomAdvMetroTaskDialog = class(TComponent, ITMSTones)
  private
    FColorTones: TColorTones;
    FTitle: string;
    FContent: string;
    FFooter: string;
    FInstruction: string;
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
    FInputType: TInputType;
    FInputText: string;
    FInputItems: TStrings;
    FInputControl: TWinControl;
    FInputDropDownCount: Integer;
    FFooterColor: TColor;
    FFooterLineColor: TColor;
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
    function GetButtonModalResult(aButton: TWinControl): Integer; virtual;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure TaskDialogFormCreated(Sender: TObject);
    property CustomButtons: TStringList read FCustomButtons write SetCustomButtons;
    property CustomIcon: TIcon read FCustomIcon write SetCustomIcon;
    property RadioButtons: TStringList read FRadioButtons write SetRadioButtons;
    property CommonButtons: TCommonButtons read FCommonButtons write FCommonButtons;
    property DefaultButton: integer read FDefaultButton write FDefaultButton default 0;
    property DefaultRadioButton: integer read FDefaultRadioButton write FDefaultRadioButton default 200;
    property DialogPosition: TTaskDialogPosition read FDlgPosition write FDlgPosition default dpScreenCenter;
    property ExpandedText: string read FExpandedText write SetExpandedText;
    property Footer: string read FFooter write SetFooter;
    property FooterIcon: TTaskDialogFooterIcon read FFooterIcon write FFooterIcon default tfiBlank;
    property HelpContext: longint read FHelpContext write FHelpContext default 0;
    property Icon: TTaskDialogIcon read FIcon write FIcon default tiBlank;
    property InputDropDownCount: integer read FInputDropDownCount write FInputDropDownCount default 8;
    property InputText: string read FInputText write FInputText;
    property InputType: TInputType read FInputType write FInputType;
    property InputItems: TStrings read FInputItems write SetInputItems;
    property InputControl: TWinControl read FInputControl write FInputControl;
    property Title: string read FTitle write FTitle;
    property Instruction: string read FInstruction write SetInstruction;
    property Content: string read FContent write SetContent;
    property ExpandControlText: string read FExpandControlText write FExpandControlText;
    property CollapsControlText: string read FCollapsControlText write FCollapsControlText;
    property Options: TTaskDialogOptions read FOptions write FOptions;
    property VerificationText: string read FVerifyText write FVerifyText;
    property MinFormWidth: integer read FMinFormWidth write FMinFormWidth default 350;
    property AutoClose: boolean read FAutoClose write FAutoClose default false;
    property AutoCloseTimeOut: integer read FAutoCloseTimeOut write FAutoCloseTimeOut default 0;

    property ProgressBarMin: integer read FProgressBarMin write FProgressBarMin default 0;
    property ProgressBarMax: integer read FProgressBarMax write FProgressBarMax default 100;
    property Version: string read GetVersion write SetVersion;

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
  public
    // ITMSTones interface
    procedure SetColorTones(ATones: TColorTones);
    property ApplicationIsParent: boolean read FApplicationIsParent write FApplicationIsParent default false;
    property AlwaysOnTop: boolean read FAlwaysOnTop write FAlwaysOnTop default false; 
    property hWnd: THandle read FhWnd write FhWnd;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: integer; virtual;
    procedure Clear;
    procedure Close;
    procedure EnableButton(ButtonID: integer; Enabled: boolean);
    procedure ClickButton(ButtonID: integer);
    property RadioButtonResult: integer read FButtonResult write FButtonResult;
    property VerifyResult: boolean read FVerifyResult write FVerifyResult;
    property ModalParent: THandle read FModalParent write FModalParent;
    property FooterColor: TColor read FFooterColor write FFooterColor;
    property FooterLineColor: TColor read FFooterLineColor write FFooterLineColor;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvMetroTaskDialog = class(TCustomAdvMetroTaskDialog)
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
    property MinFormWidth;

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
  TAdvInputMetroTaskDialog = class(TCustomAdvMetroTaskDialog)
  private
    FExpandedDefault: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    function Execute: integer; override;
    property Options;
  published
    property ApplicationIsParent;
    property CustomButtons;
    property CustomIcon;
    property CommonButtons;
    property DefaultButton;
    property DialogPosition;
    property ExpandedDefault: Boolean read FExpandedDefault write FExpandedDefault default False;
    property ExpandedText;
    property Footer;
    property FooterIcon;
    property Icon;
    property InputControl;
    property InputDropDownCount;
    property InputType default itEdit;
    property InputText;
    property InputItems;
    property Instruction;
    property Title;
    property Content;
    property ExpandControlText;
    property CollapsControlText;
    property VerificationText;
    property Version;
    property OnDialogCreated;
    property OnDialogClose;
    property OnDialogButtonClick;
    property OnDialogVerifyClick;
    property OnDialogInputSetText;
    property OnDialogInputGetText;
    property OnDialogHyperlinkClick;
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
    FGDIPPicture: TGDIPPicture;
    procedure OnPictureChanged(Sender: TObject);
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure SetGlyph(const Value: TBitmap);
    procedure SetGlyphDisabled(const Value: TBitmap);
    procedure SetGlyphDown(const Value: TBitmap);
    procedure SetGlyphHot(const Value: TBitmap);
    procedure SetHeadingFont(const Value: TFont);
    procedure SetGDIPPicture(const Value: TGDIPPicture);
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
    property GDIPPicture: TGDIPPicture read FGDIPPicture write SetGDIPPicture;
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

  TAdvMessageForm = class(TAdvMetroForm)
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
    FTaskDialog: TCustomAdvMetroTaskDialog;
    FFooterIcon: TAdvGDIPPicture;
    FFooterIconID: string;
    FRadioList: TList;
    FVerificationCheck: TCheckBox;
    FProgressBar: TAdvMetroProgressBar;
    FIcon: TAdvGDIPPicture;
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
    procedure BuildTaskDialog(TaskDialog: TCustomAdvMetroTaskDialog);
    procedure SetPositions;
    procedure UpdateDialog;
    property MinFormWidth: Integer Read FMinFormWidth Write FMinFormWidth;
  end;

  function AdvMessageDlgPos(TaskDialog: TCustomAdvMetroTaskDialog; X, Y: Integer): Integer;


function MetroTaskShowMessage(const Instruction: string): boolean; overload;
function MetroTaskShowMessage(const Title, Instruction: string): boolean; overload;
function MetroTaskShowmessage(const Title, Instruction: string; tiIcon: TTaskDialogIcon): boolean; overload;
function MetroTaskShowMessage(const Title, Instruction, content, verify: string;
  tiIcon: TTaskDialogIcon): boolean; overload;

function MetroTaskMessageBox(hWnd: HWND; lpInstruction, lpTitle: PChar; flags: UINT): Integer;

function MetroTaskShowMessageFmt(const Instruction: string; Parameters: array of const): boolean;

function MetroTaskMessageDlg(const Instruction: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint): Integer; overload;

function MetroTaskMessageDlg(const Instruction: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; DefaultButton: TMsgDlgBtn): Integer; overload;

function MetroTaskMessageDlg(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint): Integer; overload;

function MetroTaskMessageDlg(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; DefaultButton: TMsgDlgBtn): Integer; overload;

function MetroTaskMessageDlgPos(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; X, Y: Integer): Integer; overload;

function MetroTaskMessageDlgPos(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; X, Y: Integer;
  DefaultButton: TMsgDlgBtn): Integer; overload;

function MetroTaskMessageDlgPosHelp(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; X, Y: Integer;
  const HelpFileName: string): Integer; overload;

function MetroTaskMessageDlgPosHelp(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; X, Y: Integer;
  const HelpFileName: string; DefaultButton: TMsgDlgBtn): Integer; overload;

function MetroInputQueryDlg(ACaption, APrompt: string; var Value: string): boolean;

var
  DRAWBORDER: Boolean = True;
  ButtonNames: array[TCommonButton] of string = ('OK', 'Yes', 'No', 'Cancel', 'Retry', 'Abort');
  ButtonCaptions: array[TCommonButton] of Pointer;


implementation

uses
  AdvGDIP, ActiveX;

{$I HTMLENGO.PAS}

const
   METROCAPTIONHEIGHT = 20;

type
  TProControl = class(TControl);

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

procedure GetMultiLineTextSize(Canvas: TCanvas; Text: string; HeadingFont, ParaFont: TFont; DrawTextBiDiModeFlagsReadingOnly: Longint; var W, H: Integer; WithSpace: Boolean = True);
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
    if HasLf(Text) or (pos(#13, Text) > 0) then
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
        Windows.DrawText( Canvas.handle, PChar(s), -1, R,
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
          Windows.DrawText( Canvas.handle, PChar(s), -1, R,
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
      Windows.DrawText( Canvas.handle, PChar(Text), -1, R,
        DT_CALCRECT or DT_LEFT or DT_SINGLELINE or DrawTextBiDiModeFlagsReadingOnly);
      W := R.Right;
      H := R.Bottom;
    end;

    Canvas.Font.Assign(OldFont);
    OldFont.Free;
  end;
end;

//------------------------------------------------------------------------------

{ TAdvMetroTaskDialog }

procedure TCustomAdvMetroTaskDialog.Clear;
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

procedure TCustomAdvMetroTaskDialog.ClickButton(ButtonID: integer);
begin
  if Assigned(FDialogForm) then
    FDialogForm.ClickButton(ButtonID);
end;

procedure TCustomAdvMetroTaskDialog.Close;
begin
  if Assigned(FDialogForm) then
    FDialogForm.Close;
end;

//------------------------------------------------------------------------------

constructor TCustomAdvMetroTaskDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FColorTones := ClearTones;
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
  FInputType := itNone;
  FInputItems := TStringList.Create;
  FFooterColor := RGB(240,240,240);
  FFooterLineColor := RGB(223,223,223);
  FAutoClose := false;
  FAutoCloseTimeOut := 0;
end;

//------------------------------------------------------------------------------

destructor TCustomAdvMetroTaskDialog.Destroy;
begin
  FRadioButtons.Free;
  FCustomButtons.Free;
  FCustomIcon.Free;
  FInputItems.Free;
  inherited;
end;

//------------------------------------------------------------------------------

function TCustomAdvMetroTaskDialog.CreateButton(AOwner: TComponent): TWinControl;
begin
  Result := TAdvGlowButton.Create(AOwner);
  TAdvGlowButton(Result).Font.Size := 10;
end;

//------------------------------------------------------------------------------

function TCustomAdvMetroTaskDialog.CreateRadioButton(AOwner: TComponent): TWinControl;
begin
  Result := TRadioButton.Create(AOwner);
end;

procedure TCustomAdvMetroTaskDialog.SetRadioButtonState(Btn: TWinControl; Checked: boolean);
begin
  TRadioButton(Btn).Checked := Checked;
end;

//------------------------------------------------------------------------------

procedure TCustomAdvMetroTaskDialog.InitRadioButton(AOwner: TForm; Btn: TWinControl; btnIndex: Integer; OnClickEvent : TNotifyEvent);
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

procedure TCustomAdvMetroTaskDialog.TaskDialogFormCreated(Sender: TObject);
begin
  hwnd := FDialogForm.Handle;
  if Assigned(OnDialogCreated) then
    OnDialogCreated(Self);
end;

//------------------------------------------------------------------------------

procedure TCustomAdvMetroTaskDialog.Notification(AComponent: TComponent;
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

procedure TCustomAdvMetroTaskDialog.EnableButton(ButtonID: integer; Enabled: boolean);
begin
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

//------------------------------------------------------------------------------

function TCustomAdvMetroTaskDialog.Execute: integer;
begin
  FAutoCloseCounter := 0;
  Result := AdvMessageDlgPos(Self, -1, -1);
end;

//------------------------------------------------------------------------------

function TCustomAdvMetroTaskDialog.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn))) + '.' + IntToStr(Lo(Hiword(vn))) + '.' +
    IntToStr(Hi(Loword(vn))) + '.' + IntToStr(Lo(Loword(vn)));
end;

//------------------------------------------------------------------------------

function TCustomAdvMetroTaskDialog.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER, REL_VER), MakeWord(MIN_VER, MAJ_VER));
end;

//------------------------------------------------------------------------------

procedure TCustomAdvMetroTaskDialog.SetColorTones(ATones: TColorTones);
begin
  FColorTones := ATones;
end;

//------------------------------------------------------------------------------

procedure TCustomAdvMetroTaskDialog.SetContent(const Value: string);
begin
  if (FContent <> Value) then
  begin
    FContent := Value;
    if Assigned(FDialogForm) then
      FDialogForm.UpdateDialog;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomAdvMetroTaskDialog.SetCustomButtons(const Value: TStringList);
begin
  FCustomButtons.Assign(Value);
end;

procedure TCustomAdvMetroTaskDialog.SetCustomIcon(const Value: TIcon);
begin
  FCustomIcon.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TCustomAdvMetroTaskDialog.SetExpandedText(const Value: string);
begin
  if (FExpandedText <> Value) then
  begin
    FExpandedText := Value;
    if Assigned(FDialogForm) then
      FDialogForm.UpdateDialog;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomAdvMetroTaskDialog.SetFooter(const Value: string);
begin
  if (FFooter <> Value) then
  begin
    FFooter := Value;
    if Assigned(FDialogForm) then
      FDialogForm.UpdateDialog;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomAdvMetroTaskDialog.SetInputItems(const Value: TStrings);
begin
  FInputItems.Assign(Value);
end;

procedure TCustomAdvMetroTaskDialog.SetInstruction(const Value: string);
begin
  if (FInstruction <> Value) then
  begin
    FInstruction := Value;
    if Assigned(FDialogForm) then
      FDialogForm.UpdateDialog;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomAdvMetroTaskDialog.SetRadioButtonCaption(Btn: TWinControl;
  Value: string);
begin
  TRadioButton(Btn).Caption := Value;
end;

//------------------------------------------------------------------------------

procedure TCustomAdvMetroTaskDialog.SetRadioButtons(const Value: TStringList);
begin
  FRadioButtons.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TCustomAdvMetroTaskDialog.SetVersion(const Value: string);
begin

end;

//------------------------------------------------------------------------------

procedure TCustomAdvMetroTaskDialog.SetButtonCancel(aButton: TWinControl; Value: Boolean);
begin

  if not Assigned(aButton) or not (aButton is TAdvGlowButton) then
    Exit;



  TAdvGlowButton(aButton).Cancel := Value;
end;

//------------------------------------------------------------------------------

procedure TCustomAdvMetroTaskDialog.SetButtonDefault(aButton: TWinControl; Value: Boolean);
begin
  if not Assigned(aButton) or not (aButton is TAdvGlowButton) then
    Exit;

  TAdvGlowButton(aButton).Default := Value;
end;

//------------------------------------------------------------------------------

procedure TCustomAdvMetroTaskDialog.SetButtonModalResult(aButton: TWinControl; Value: Integer);
begin
  if not Assigned(aButton) or not (aButton is TAdvGlowButton) then
    Exit;

  TAdvGlowButton(aButton).ModalResult := Value;
end;

//------------------------------------------------------------------------------

function TCustomAdvMetroTaskDialog.GetButtonModalResult(
  aButton: TWinControl): Integer;
begin
  Result := mrNone;
  if not Assigned(aButton) or not (aButton is TAdvGlowButton) then
    Exit;

  Result := TAdvGlowButton(aButton).ModalResult;
end;

//------------------------------------------------------------------------------

procedure TCustomAdvMetroTaskDialog.SetButtonCaption(aButton: TWinControl;
  Value: TCaption);
var
  btn: TAdvGlowButton;

begin
  if not Assigned(aButton) or not (aButton is TAdvGlowButton) then
    Exit;

  btn := (aButton as TAdvGlowButton);
  btn.Caption := Value;
end;

//------------------------------------------------------------------------------

{ TTaskDialogButton }

constructor TTaskDialogButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FGDIPPicture := TGDIPPicture.Create;

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
end;

//------------------------------------------------------------------------------

destructor TTaskDialogButton.Destroy;
begin
  FGDIPPicture.Free;
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

procedure DrawMetroPicture(R: TRect; g: TGPGraphics; Pic: TGDIPPicture; PicClr: TGPColor);
var
  Attr: TGPImageAttributes;
  ColorMatrix: TColorMatrix;
  Img: TGPImage;
  pstm: IStream;
  hGlobal: THandle;
  pcbWrite: Longint;
  ms: TMemoryStream;
  RF: TGPRectF;
  CR: TRect;
  s: Integer;
  rc, gc, bc: Double;
  hr: HResult;
begin
  CR := R;

  InflateRect(CR, -2, -2);

  s := Min(CR.Right - CR.Left, CR.Bottom - CR.Top);
  RF := MakeRect(CR.Left + (CR.Right - CR.Left - s) div 2, CR.Top + (CR.Bottom - CR.Top - s) div 2, s, s);

  if not Assigned(Pic) or Pic.Empty then
    Exit;

  ms := TMemoryStream.Create;
  pic.SaveToStream(ms);
  hGlobal := GlobalAlloc(GMEM_MOVEABLE, ms.Size);
  if (hGlobal = 0) then
  begin
    ms.Free;
    raise Exception.Create('Could not allocate memory for image');
  end;

  pstm := nil;
  pcbWrite := 0;

  try
    // Create IStream* from global memory
    hr := CreateStreamOnHGlobal(hGlobal, TRUE, pstm);
    if hr = S_OK then
    begin
      pstm.Write(ms.Memory, ms.Size, @pcbWrite);

      if (ms.Size = pcbWrite) then
      begin
        Img := TGPImage.Create(pstm);

        rc := ADVGDIP.GetRed(PicClr) / 255;
        gc := ADVGDIP.GetGreen(PicClr)/ 255;
        bc := ADVGDIP.GetBlue(PicClr)/ 255;

        // transformed image color
        ColorMatrix[0,0] := 0;
        ColorMatrix[1,1] := 0;
        ColorMatrix[2,2] := 0;
        ColorMatrix[3,3] := 0;

        ColorMatrix[3,0] := 0;
        ColorMatrix[3,1] := 0;
        ColorMatrix[3,2] := 0;
        ColorMatrix[3,3] := 1; // <- original A

        ColorMatrix[4,0] := rc; // <- desired R
        ColorMatrix[4,1] := gc; // <- desired G
        ColorMatrix[4,2] := bc; // <- desired B
        ColorMatrix[4,3] := 0;

        Attr := TGPImageAttributes.Create;
        Attr.SetColorMatrix(ColorMatrix);
        g.DrawImage(img, RF, 0, 0, Img.Width, Img.Height, UnitPixel, Attr);

        Img.Free;
        attr.Free;
      end;
      pstm := nil;
    end
    else
      GlobalFree(hGlobal);
  finally
    ms.Free;
  end;
end;

//------------------------------------------------------------------------------

procedure TTaskDialogButton.Paint;
var
  Pic: TBitmap;
  x, y, bw, bh, i: Integer;
  R, TR, IR: TRect;
  BrClr: TColor;
  SL: TStringList;
  gp: TGPGraphics;
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

  if Assigned(GDIPPicture) and not GDIPPicture.Empty then
  begin
    if (Caption = '') then
    begin
      x := (Width - GDIPPicture.Width) div 2;
      y := (Height - GDIPPicture.Height) div 2;
    end
    else
    begin
      x := 4;
      y := (Height - GDIPPicture.Height) div 2;
    end;

    gp := TGPGraphics.Create(Canvas.Handle);

    IR := Rect(x ,y + 12,x +24,y + 12 +24);
    try
      DrawMetroPicture(IR, gp, GDIPPicture, MakeColor(255,BorderColorHot));
    finally
      gp.Free;
    end;

    //Canvas.StretchDraw(Rect(x ,y + 12,x +24,y + 12 +24),GDIPPicture);
    //Canvas.Draw(x, y, GDIPPicture);
    R.Left := x + 24 + 3;
  end
  else
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
    if HasLf(Caption) or (pos(#13, Caption) > 0) then
    begin
      TR := R;
      SL := TStringList.Create;
      SplitInToLines(Caption, SL);
      GetMultiLineTextSize(Canvas, Caption, HeadingFont, Self.Font, DrawTextBiDiModeFlagsReadingOnly, bw, bh);
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

procedure TTaskDialogButton.SetGDIPPicture(const Value: TGDIPPicture);
begin
  FGDIPPicture.Assign(Value);
  Invalidate;
end;

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
  IconIDs: array[TTaskDialogIcon] of string = ('', 'METRO_EXCLAMATION', 'METRO_QUESTION', 'METRO_ERROR', 'METRO_INFO', '', 'METRO_LOCK');
  FooterIconIDs: array[TTaskDialogFooterIcon] of string = ('', 'METRO_EXCLAMATION', 'METRO_QUESTION', 'METRO_ERROR', 'METRO_INFO', 'METRO_LOCK');
  Captions: array[TTaskDialogIcon] of Pointer;
  // = (nil, @SMsgDlgWarning, @SMsgDlgConfirm, @SMsgDlgError, @SMsgDlgInformation);
  ModalResults: array[TCommonButton] of Integer = (mrOk, mrYes, mrNo, mrCancel, mrRetry, mrAbort);
  //(tiBlank, tiWarning, tiQuestion, tiError, tiShield);
  //(mtWarning, mtError, mtInformation, mtConfirmation, mtCustom);

function CreateAdvMessageDlg(TaskDialog: TCustomAdvMetroTaskDialog): TForm;
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

  if IsClearTones(MetroFormTones) then
    MetroFormTones := DefaultMetroTones;

  if IsClearTones(TaskDialog.FColorTones) then
    TaskDialog.FColorTones := MetroFormTones;

  with Result do
  begin
    BiDiMode := Application.BiDiMode;
    BorderIcons := [];
    BorderStyle := bsNone;
    (Result as TAdvMetroForm).NoResize := true;

    if doAllowMinimize in TaskDialog.Options then
    begin
    //  BorderStyle := bsSingle;
      BorderIcons := [biSystemMenu, biMinimize]
    end
    else
    begin
    //  BorderStyle := bsDialog;
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

  TAdvMessageForm(Result).SetColorTones(TaskDialog.FColorTones);
  TAdvMessageForm(Result).FooterColor := TaskDialog.FooterColor;
  TAdvMessageForm(Result).FooterLineColor := TaskDialog.FooterLineColor;
  TAdvMessageForm(Result).MinFormWidth := TaskDialog.MinFormWidth;
  TAdvMessageForm(Result).BuildTaskDialog(TaskDialog);
  {$IFDEF DELPHIXE3_LVL}
  TAdvMessageForm(Result).StyleElements  := [];
  {$ENDIF}
  TAdvMessageForm(Result).Appearance.ShowAppIcon := false; 
  TAdvMessageForm(Result).Appearance.CaptionFont.Size := 10;
end;

//------------------------------------------------------------------------------

function AdvMessageDlgPos(TaskDialog: TCustomAdvMetroTaskDialog; X, Y: Integer): Integer;
var
  DlgForm: TAdvMessageForm;
begin
  Result := -1;
  if not Assigned(TaskDialog) then
    Exit;

  DlgForm := TAdvMessageForm(CreateAdvMessageDlg(TaskDialog));

  DlgForm.OnShow := TaskDialog.TaskDialogFormCreated;

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
      {$IFNDEF DELPHI6_LVL}
      Close;
      {$ENDIF}
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
  mcHorzSpacing = 10;
  mcVertSpacing = 10;
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

procedure TAdvMessageForm.BuildTaskDialog(TaskDialog: TCustomAdvMetroTaskDialog);
var
  DialogUnits: TPoint;
  ButtonWidth, ButtonHeight, ButtonSpacing, ButtonGroupWidth,
  IconTextWidth, IconTextHeight, X, Y, ALeft: Integer;
  B, DefaultButton, CancelButton: TCommonButton;
  IconID: string;
  TextRect, FR: TRect;
  Msg: string;
  DlgType: TTaskDialogIcon;
  Buttons: TCommonButtons;
  i, bw, bh, h, w, j: Integer;
  CmBtnGroupWidth, CsBtnGroupWidth: Integer;
  r, re: trect;
  anchor, stripped: string;
  HyperLinks,MouseLink, k, l, n: Integer;
  Focusanchor: string;
  OldFont, hf, pf: TFont;
  verifTextWidth: Integer;
  v: Boolean;
  szContent,szExpandedText,szFooterText: string;
  defIdx: integer;
  trydate: TDateTime;

begin
  if not Assigned(TaskDialog) then
    Exit;

  IconID := '';

  FTaskDialog := TaskDialog;
  Msg := TaskDialog.Instruction;
  DlgType := TaskDialog.Icon;
  Buttons := TaskDialog.CommonButtons;

  OldFont := TFont.Create;
  OldFont.Assign(Canvas.Font);

  Canvas.Font.Name := GetMetroFont;
  Canvas.Font.Size := 10;

  DialogUnits := GetAveCharSize(Canvas);
  FHorzMargin := MulDiv(mcHorzMargin, DialogUnits.X, 4);
  FVertMargin := MulDiv(mcVertMargin, DialogUnits.Y, 8);
  FHorzSpacing := MulDiv(mcHorzSpacing, DialogUnits.X, 4);
  FVertSpacing := MulDiv(mcVertSpacing, DialogUnits.Y, 8);

  w := 0;

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
  ButtonHeight := MulDiv(mcButtonHeight, DialogUnits.Y, 8);
  ButtonSpacing := MulDiv(mcButtonSpacing, DialogUnits.X, 4);
  CmBtnGroupWidth := 0;
  CsBtnGroupWidth := 0;

  FHorzParaMargin := FHorzMargin;

  Y := FVertMargin + METROCAPTIONHEIGHT;

  FcmBtnList.Clear;

  DefaultButton := cbOk;

  if (TaskDialog.DefaultButton <> -1) then
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
        Windows.DrawText( Canvas.Handle,
          PChar(LoadResString(ButtonCaptions[B])), -1,
          TextRect, DT_CALCRECT or DT_LEFT or DT_SINGLELINE or
          DrawTextBiDiModeFlagsReadingOnly);
        with TextRect do
          ButtonWidths[B] := Right - Left + 16;
      end;

      if ButtonWidths[B] > ButtonWidth then
        ButtonWidth := ButtonWidths[B];

      i := FcmBtnList.Add(TaskDialog.CreateButton(Self));

      TAdvGlowButton(FcmBtnList.Items[i]).SetColorTones(TaskDialog.FColorTones);

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

        if (B = CancelButton) and (doAllowDialogCancel in TaskDialog.Options) then
          TaskDialog.SetButtonCancel(TWinControl(FcmBtnList.Items[i]), True);

        Width := Max(60, ButtonWidths[B]);
        Height := ButtonHeight;
        cmBtnGroupWidth := cmBtnGroupWidth + Width + ButtonSpacing;
        //if B = mbHelp then
          //OnClick := TMessageForm(Result).HelpButtonClick;

        TProControl(FcmBtnList.Items[i]).OnClick := OnButtonClick;
//        OnClick := OnButtonClick;
        if TaskDialog.DefaultButton = -1 then
          TabStop := false;
      end;
    end;
  end;

  FcsBtnList.Clear;
  if not (docommandLinks in TaskDialog.Options) then
  begin
    for i := 0 to TaskDialog.CustomButtons.Count - 1 do
    begin
      TextRect := Rect(0,0,0,0);
      Windows.DrawText( Canvas.Handle,
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

      TAdvGlowButton(FcsBtnList.Items[j]).SetColorTones(TaskDialog.FColorTones);

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
        Width := Max(60, bw);
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
      GetMultiLineTextSize(Canvas, TaskDialog.CustomButtons[i], Hf, Pf, DrawTextBiDiModeFlagsReadingOnly, bw, bh);

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
        w := Max(w, n + FHorzMargin*2 + FHorzSpacing + 32)
      else
        w := Max(w, n + FHorzMargin);

      j := FcsBtnList.Add(TTaskDialogButton.Create(Self));

      with TTaskDialogButton(FcsBtnList.Items[j]) do
      begin
        Name := 'Button' + inttostr(i);
        Parent := Self;
        Caption := TaskDialog.CustomButtons[i];
        Font.Assign(pf);
        Font.Color := TaskDialog.FColorTones.Selected.BrushColor;
        HeadingFont.Assign(hf);
        HeadingFont.Color := TaskDialog.FColorTones.Selected.BrushColor;
        ModalResult := i + 100; //mrAbort;
        //Default := (ModalResult = TaskDialog.DefaultButton);
        BorderColorDown := TaskDialog.FColorTones.Selected.BrushColor;
        BorderColorHot := TaskDialog.FColorTones.Selected.BrushColor;
        Width := Max(60, n);
        if TaskDialog.DefaultButton <> -1 then
          AutoFocus := true;

        Height := Max(bh, Max(ButtonHeight, Canvas.TextHeight('gh') + 20));

        // change color of the arrow!
        if not (doCommandLinksNoIcon in TaskDialog.Options) then
          GDIPPicture.LoadFromResourceName(HInstance, 'METRO_ARROW');

        BackColor := TaskDialog.FColorTones.Background.BrushColor;
        AutoFocus := false;

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
    Windows.DrawText( canvas.handle,
      PChar(LoadResString(ButtonCaptions[B])), -1,
      TextRect, DT_CALCRECT or DT_LEFT or DT_SINGLELINE or
      DrawTextBiDiModeFlagsReadingOnly);
    with TextRect do ButtonWidths[B] := Right - Left + 8;

    //if ButtonWidths[B] > ButtonWidth then
      //ButtonWidth := ButtonWidths[B];

    i := FcmBtnList.Add(TaskDialog.CreateButton(Self));

    TAdvGlowButton(FcmBtnList.Items[i]).SetColorTones(TaskDialog.FColorTones);

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

      if doAllowDialogCancel in TaskDialog.Options then
        TaskDialog.SetButtonCancel(TWinControl(FcmBtnList.Items[i]), True);
        
      Width := Max(60, ButtonWidths[B]);
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
  IconTextHeight := TextRect.Bottom;
  if IconID <> '' then
  begin
    Inc(IconTextWidth, 32 + FHorzSpacing);
    if IconTextHeight < 32 then IconTextHeight := 32;
  end;

  {if DlgType <> tiBlank then
    Caption := LoadResString(Captions[DlgType]) else
    Caption := Application.Title;}

  if ((IconID <> '') or not (TaskDialog.CustomIcon.Empty)) {and not (doCommandLinksNoIcon in TaskDialog.Options)} then
  begin
    FIcon := AdvGDIPicture.TAdvGDIPPicture.Create(Self);
    with FIcon do
    begin
      Name := 'Image';
      Parent := Self;

      if not TaskDialog.CustomIcon.Empty then
      begin
        FIcon.Picture.Assign(TaskDialog.CustomIcon);
        //Picture.Icon.Assign();
      end
      else
      begin
        Picture.LoadFromResourceName(HInstance,IconID);
      end;

      SetBounds(FHorzMargin, Y, 48, 48);
    end;
  end;

  Message := TLabel.Create(Self);
  {$IFDEF DELPHIXE3_LVL}
  Message.StyleELements := [seBorder, seClient];
  {$ENDIF}
  with Message do
  begin
    Name := 'Instr';
    Parent := Self;
    WordWrap := True;
    Caption := Msg;
    Font.Size := 11;
    Font.Color := TaskDialog.FColorTones.Hover.BrushColor;
    Font.Style := [fsBold];

    //OffsetRect(TextRect,0,16);
    //y := y + 16;
    BoundsRect := TextRect;
    BiDiMode := Self.BiDiMode;
    ShowAccelChar := false;
    ALeft := IconTextWidth - TextRect.Right + FHorzMargin;

    if UseRightToLeftAlignment then
      ALeft := Self.ClientWidth - ALeft - Width;

    SetBounds(ALeft, Y, TextRect.Right, TextRect.Bottom);

    y := Y + Height + FVertSpacing;
    FHorzParaMargin := ALeft;
  end;

  if (doTimer in TaskDialog.Options) or (TaskDialog.AutoClose) or
     (doProgressBar in TaskDialog.Options) then
  begin
    FTimer := TTimer.Create(Self);
    FTimer.Interval := 100;
    FTimer.OnTimer := OnTimer;
    FTimer.Enabled := True;
  end;

  if (doProgressBar in TaskDialog.Options) then
  begin
    FProgressBar := TAdvMetroProgressBar.Create(Self);
    FProgressBar.SetColorTones(TaskDialog.FColorTones);
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

  end;

  if (TaskDialog.RadioButtons.Count > 0) then
  begin
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
        GetTextSize(Canvas, Caption, k, l);
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
    (*
    FExpandLabel := TLabel.Create(Self);
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
    end;
    *)

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
      Picture.LoadFromResourceName(HInstance, 'METRO_COLP');
      Picture.TransparentColor :=  clFuchsia;

      PictureHot.LoadFromResourceName(HInstance, 'METRO_COLPHOT');
      PictureHot.TransparentColor := clFuchsia;

      PictureDown.LoadFromResourceName(HInstance, 'METRO_COLPDOWN');
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
    {$IFDEF DELPHIXE3_LVL}
    FVerificationCheck.StyleElements := [seBorder];
    {$ENDIF}
    with FVerificationCheck do
    begin
      Name := 'Verification';
      Parent := Self;
      WordWrap := False;
      BoundsRect := TextRect;
      BiDiMode := Self.BiDiMode;
      Caption := TaskDialog.VerificationText;
      Left := FHorzMargin;
      Top := Y;
      Color := TaskDialog.FooterColor;
      OnClick := OnVerifyClick;
      Checked := (doVerifyChecked in TaskDialog.Options);
      GetTextSize(Canvas, Caption, k, l);
      verifTextWidth := k + FVertSpacing *2;
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

    if IconID <> '' then
    begin
      FFooterIcon := AdvGDIPicture.TAdvGDIPPicture.Create(Self);
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
  itEdit:
    begin
      FInputEdit := TEdit.Create(self);
      FInputEdit.Parent := Self;
      FInputEdit.TabStop := true;
      FInputEdit.Text := TaskDialog.InputText;

      ALeft := IconTextWidth - TextRect.Right + FHorzMargin;
      if UseRightToLeftAlignment then
        ALeft := Self.ClientWidth - ALeft - Width;

      FInputEdit.SetBounds(ALeft, Y + 20, ClientWidth - ALeft, 20);
    end;
  itComboEdit, itComboList:
    begin
      FInputCombo := TComboBox.Create(self);
      FInputCombo.Parent := Self;
      FInputCombo.TabStop := true;
      FInputCombo.Text := TaskDialog.InputText;
      FInputCombo.DropDownCount := TaskDialog.InputDropDownCount;
      FInputCombo.Items.Assign(TaskDialog.InputItems);

      if TaskDialog.InputType = itComboList then
      begin
        FInputCombo.Style := csDropDownList;
        FInputCombo.ItemIndex := FInputCombo.Items.IndexOf(TaskDialog.InputText);
      end;

      ALeft := IconTextWidth - TextRect.Right + FHorzMargin;
      if UseRightToLeftAlignment then
        ALeft := Self.ClientWidth - ALeft - Width;

      FInputCombo.SetBounds(ALeft, Y + 20, ClientWidth - ALeft, 20);
    end;
  itDate:
    begin
      FInputDate := TDateTimePicker.Create(self);
      FInputDate.Parent := Self;
      FInputDate.TabStop := true;
      ALeft := IconTextWidth - TextRect.Right + FHorzMargin;
      if UseRightToLeftAlignment then
        ALeft := Self.ClientWidth - ALeft - Width;

      {$IFDEF DELPHI2005_LVL}
      if TryStrToDate(TaskDialog.InputText, trydate) then
        FInputDate.Date := trydate;
      {$ENDIF}

      FInputDate.Top := Y + 20;
      FInputDate.Left := ALeft;
    end;
  itMemo:
    begin
      FInputMemo := TMemo.Create(self);
      FInputMemo.Parent := Self;
      FInputMemo.TabStop := true;
      FInputMemo.WantReturns := false;
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
          SetWindowText(TaskDialog.InputControl.Handle, Pchar(TaskDialog.InputText));

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
  w := Max(w, FMinFormWidth);

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
    FProgressBar.Width := ClientWidth - FHorzMargin * 2;
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
  IconID: string;
  TextRect: TRect;
  Msg: string;
  DlgType: TTaskDialogIcon;
  Buttons: TCommonButtons;
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
  Buttons := FTaskDialog.CommonButtons;

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
  end
  else
  begin
  
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
  if (IconId <> '') then
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
function Get_TextWidth(const Text:String; Font:TFont) : Integer;
var
  LBmp: TBitmap;
begin
  LBmp := TBitmap.Create;
  try
    LBmp.Canvas.Font := Font;
    Result := LBmp.Canvas.TextWidth(Text);
  finally
    LBmp.Free;
  end;
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
  ExpTextTop, verifTextWidth, k: Integer;
  szContent: string;
  szExpandedText,szFooterText: string;
  //lbl:TLabel;
  //ExH: integer;
begin
  if not Assigned(FTaskDialog) then
    Exit;

  Canvas.Font.Name := GetMetroFont;
  Canvas.Font.Size := 10;

  DialogUnits := GetAveCharSize(Canvas);
  HorzMargin := MulDiv(mcHorzMargin, DialogUnits.X, 4);
  VertMargin := MulDiv(mcVertMargin, DialogUnits.Y, 8);
  VertSpacing := MulDiv(mcVertSpacing, DialogUnits.Y, 8);
  ButtonSpacing := MulDiv(mcButtonSpacing, DialogUnits.X, 4);

  CmBtnGroupWidth := 0;
  CsBtnGroupWidth := 0;
  Y := VertMargin + METROCAPTIONHEIGHT;

  Message.Transparent := true;

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
    itEdit: FInputEdit.Top := Y - 10;
    itComboEdit,itComboList: FInputCombo.Top := Y - 10;
    itDate: FInputDate.Top := Y - 10;
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

  if (FTaskDialog.InputType in [itEdit, itComboEdit, itComboList, itDate]) then
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
    for i:= 0 to FcsBtnList.Count-1 do
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

  for i := 0 to FcmBtnList.Count-1 do
  begin
    CmBtnGroupWidth := CmBtnGroupWidth + TButton(FcmBtnList.Items[i]).Width{ + ButtonSpacing};
  end;
  CmBtnGroupWidth := CmBtnGroupWidth + (FcmBtnList.Count-1) * ButtonSpacing;

  verifTextWidth := 0;

  if (FTaskDialog.VerificationText <> '') then
  begin
    k := Get_TextWidth(FTaskDialog.VerificationText, FVerificationCheck.Font);
    verifTextWidth := k + FVertSpacing * 2;
  end;

  ButtonGroupWidth := CsBtnGroupWidth + CmBtnGroupWidth;

  X := (ClientWidth - ButtonGroupWidth - FHorzSpacing - 4); //(ClientWidth - ButtonGroupWidth) div 2;
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
      Top := Y;
      Left := FVertMargin; //X;
      //Inc(X, FExpandButton.Width + ButtonSpacing);
      if (FExpandButton.Height > BtnH) then
        BtnH := FExpandButton.Height;
    end;
  end;

  if (FTaskDialog.VerificationText <> '') and Assigned(FVerificationCheck) then
  begin
    FVerificationCheck.Width := verifTextWidth - FVertSpacing; //ClientWidth - FVerificationCheck.Left - HorzMargin;
    FVerificationCheck.Top := Y + BtnH + 4;
    FVerificationCheck.Left := FVertMargin + 3;
    //X := FVerificationCheck.Left + FVerificationCheck.Width + FVertMargin;
  end;

  if not (docommandLinks in FTaskDialog.Options) then
  begin
    for i:= 0 to FcsBtnList.Count-1 do
    begin
      with TButton(FcsBtnList.Items[i]) do
      begin
        Top := Y;
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
      Top := Y;
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
    if FcmBtnList.Count > 0 then
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
      FExpandButton.Picture.LoadFromResourceName(HInstance,'METRO_EXP');
      FExpandButton.Picture.TransparentColor := clFuchsia;
      FExpandButton.PictureHot.LoadFromResourceName(HInstance,'METRO_EXPHOT');
      FExpandButton.PictureHot.TransparentColor := clFuchsia;
      FExpandButton.PictureDown.LoadFromResourceName(HInstance,'METRO_EXPDOWN');
      FExpandButton.PictureDown.TransparentColor := clFuchsia;
    end;
  end
  else
  begin
    if Value then
    begin
      FExpandButton.Picture.LoadFromResourceName(HInstance,'METRO_COLP');
      FExpandButton.Picture.TransparentColor := clFuchsia;
      FExpandButton.PictureHot.LoadFromResourceName(HInstance,'METRO_COLPHOT');
      FExpandButton.PictureHot.TransparentColor := clFuchsia;
      FExpandButton.PictureDown.LoadFromResourceName(HInstance,'METRO_COLPDOWN');
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
    Canvas.Font.Color := FTaskDialog.FColorTones.Background.TextColor;

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
    Canvas.Font.Color := FTaskDialog.FColorTones.Background.TextColor;

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
      Y := Message.Top + Message.Height
    else
      Y := FVertMargin;

    Y := Y + FVertSpacing;

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
  szFooterText: string;

begin
  if not Assigned(FTaskDialog) then
    Exit;

  if (FTaskDialog.Footer <> '') then
  begin
    R := GetFooterRect;

    i := R.Top - FVertSpacing;
    Canvas.Pen.Color := RGB(223, 223, 223);
    Canvas.MoveTo(2, i);
    Canvas.LineTo(ClientWidth -3, i);
    Canvas.Pen.Color := clWhite;
    Canvas.MoveTo(2, i+1);
    Canvas.LineTo(ClientWidth -3, i+1);

    Canvas.Font.Color := clBlack;

    if Assigned(FFooterIcon) then
    begin
      if (FFooterIconID <> '') then
      begin
        FFooterIcon.Picture.LoadFromResourceName(HInstance, FFooterIconID);
        Canvas.StretchDraw(Rect(r.Left, r.Top - 4, r.Left + 20, r.Top + 20), FFooterIcon.Picture);
        R.Left := R.Left + 20;
      end;
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
    i := TButton(FcmBtnList.Items[0]).Top;
  else if (FcsBtnList.Count > 0) then
    i := TButton(FcsBtnList.Items[0]).Top;}

  VerInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  GetVersionEx(verinfo);

  if (FTaskDialog.CommonButtons = []) and (doCommandLinks in FTaskDialog.Options) then
    i := 0;


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
    Canvas.Font.Color := clBlack;
    if not FExpanded then
      s := FTaskDialog.CollapsControlText
    else
      s := FTaskDialog.ExpandControlText;

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

procedure TAdvMessageForm.DoClose(var Action: TCloseAction);
var
  CanClose: Boolean;
  s: string;
  a: array[0..255] of char;

begin
  CanClose := True;

  if Assigned(FTaskDialog) and Assigned(FTaskDialog.OnDialogClose) then
  begin
    FTaskDialog.OnDialogClose(FTaskDialog, CanClose);
  end;

  case FTaskDialog.InputType of
  itEdit: FTaskDialog.InputText := FInputEdit.Text;
  itComboEdit, itComboList: FTaskDialog.InputText := FInputCombo.Text;
  itDate: FTaskDialog.InputText := DateToStr(FInputDate.Date);
  itMemo: FTaskDialog.InputText := FInputMemo.Lines.Text;
  itCustom:
    begin
      if Assigned(FTaskDialog.InputControl) then
      begin
        GetWindowText(FTaskDialog.InputControl.Handle, a, sizeof(a));
        s := strpas(a);
        if Assigned(FTaskDialog.OnDialogInputGetText) then
        begin
          s := '';
          FTaskDialog.OnDialogInputGetText(Self, s);
        end;
        FTaskDialog.InputText := s;
        if CanClose then
        begin
          FTaskDialog.InputControl.Visible := false;
          FTaskDialog.InputControl.Parent := FOldParent;
        end;
      end;
    end;
  end;

  if not CanClose then
    Action := caNone;
  inherited;
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
  itEdit: FInputEdit.SetFocus;
  itComboEdit, itComboList: FInputCombo.SetFocus;
  itDate: FInputDate.SetFocus;
  itMemo: FInputMemo.SetFocus;
  itCustom: FTaskDialog.InputControl.SetFocus;
  end;

end;

//------------------------------------------------------------------------------

procedure TAdvMessageForm.OnButtonClick(Sender: TObject);
begin
  if not Assigned(FTaskDialog) or not Assigned(FcsBtnList) then
    Exit;

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
  td:  TCustomAdvMetroTaskDialog;
begin
  td := TCustomAdvMetroTaskDialog.Create(application);
  td.Title := Title;
  td.Instruction := instruction;
  td.Content := Content;
  td.VerificationText := verify;
  td.icon := tiIcon;

  td.SetColorTones(MetroFormTones);
  td.Execute;
  result := (verify <> '') and td.VerifyResult;
  td.free;
end {CoreShowmessage};

//=====================================================================
// This returns false unless verify is not blank AND the verify checkbox 
// was not checked.
//---------------------------------------------------------------------
function MetroTaskShowMessage(
           const Title,    // dialog window title
           Instruction,    // the part of the message shown in blue
           content,        // additional message if desired
           verify: string; // ex Do Not Show this Again
           tiIcon: tTaskDialogIcon): boolean; overload;
begin
  result := coreShowmessage(title, instruction,content,verify,tiIcon);
end { tmsShowMessage };

function MetroTaskShowmessage(const Instruction: string):boolean; overload;
begin // Only instruction . tiInformation
  result := CoreShowMessage('',Instruction,'','',tiInformation);
end;

function MetroTaskShowmessage(const Title, Instruction: string):boolean; overload;
begin // title, instruction tiInformation
  result := CoreShowMessage(Title,Instruction,'','',tiInformation);
end;

function MetroTaskShowmessage(const Title, Instruction: string;tiIcon: TTaskDialogIcon): boolean; overload;
begin
  result := CoreShowMessage(Title,Instruction,'','',tiIcon);
end;

function MetroTaskShowMessageFmt(const Instruction: string; Parameters: array of const): boolean;
begin
  Result := MetroTaskShowmessage(Format(Instruction,Parameters));
end;

function MetroTaskMessageBox(hWnd: HWND; lpInstruction, lpTitle: PChar; flags: UINT): Integer;
const
  MB_CANCELTRYCONTINUE = $00000006;  // missing from windows unit so probably never be used
var
  td:   TCustomAdvMetroTaskDialog;
  res:  integer;
  def:  integer;
  num:  integer;
  task: tCommonButton;
  txt:  string;
begin
  td := TCustomAdvMetroTaskDialog.Create(application);
  td.Title := lptitle;
  td.instruction := lpInstruction;

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

function MetroTaskMessageDlg(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint): Integer;
begin
  Result := MetroTaskMessageDlgPosHelp(Title, Msg, DlgType, Buttons, HelpCtx, -1, -1, '');
end;

//--------------------------------------------------------------------------------------------------

function MetroTaskMessageDlg(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; DefaultButton: TMsgDlgBtn): Integer; overload;
begin
  Result := MetroTaskMessageDlgPosHelp(Title, Msg, DlgType, Buttons, HelpCtx,
    -1, -1, '', DefaultButton);
end;

//--------------------------------------------------------------------------------------------------

function MetroTaskMessageDlgPos(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; X, Y: Integer): Integer;
begin
  Result := MetroTaskMessageDlgPosHelp(Title, Msg, DlgType, Buttons, HelpCtx, X, Y, '');
end;

//--------------------------------------------------------------------------------------------------

function MetroTaskMessageDlgPos(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; X, Y: Integer;
  DefaultButton: TMsgDlgBtn): Integer; overload;
begin
  Result := MetroTaskMessageDlgPosHelp(Title, Msg, DlgType, Buttons, HelpCtx,
    X, Y, '', DefaultButton);
end;

//--------------------------------------------------------------------------------------------------

function MetroTaskMessageDlgPosHelp(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; X, Y: Integer;
  const HelpFileName: string): Integer;
begin
  Result := MetroTaskMessageDlgPosHelp(Title, Msg, DlgType, Buttons, HelpCtx, X, Y,
    HelpFileName, mbYes);
end;


function MetroTaskMessageDlg(const Instruction: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint): Integer; overload;
begin
  // passes mbHelp as the default button since we can't deal with help anyway
  Result := MetroTaskMessageDlg(Instruction,Dlgtype,Buttons,HelpCtx,mbHelp);
end;

function MetroTaskMessageDlg(const Instruction: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; DefaultButton: TMsgDlgBtn): Integer; overload;
var
  td:  TCustomAdvMetroTaskDialog;
  ray: array[0..3] of integer;
  res: integer;
begin
  td := TCustomAdvMetroTaskDialog.Create(Application);
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

function MetroTaskMessageDlgPosHelp(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; X, Y: Integer;
  const HelpFileName: string; DefaultButton: TMsgDlgBtn): Integer;
var
  td:  TAdvMetroTaskDialog;
  ray: array[0..3] of integer;
  res: integer;
  p: TCustomForm;
begin
  p := nil;
  if Assigned(screen.ActiveControl) then
  {$IFDEF DELPHI9_LVL}
    p := GetParentForm(screen.ActiveControl, True);
  {$ENDIF}
  {$IFNDEF DELPHI9_LVL}
    p := GetParentForm(screen.ActiveControl);
  {$ENDIF}

  if Assigned(p) then
    td := TAdvMetroTaskDialog.Create(p)
  else
    td := TAdvMetroTaskDialog.Create(Application);

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
    td.Options := td.Options + [doAllowDialogCancel];

    Result := 0;
    res := td.Execute;
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

//------------------------------------------------------------------------------

function MetroInputQueryDlg(ACaption, APrompt: string; var Value: string):boolean;
var
  AID: TAdvInputMetroTaskDialog;
begin
  AID := TAdvInputMetroTaskDialog.Create(Application);
  AID.Instruction := APrompt;
  AID.Title := ACaption;
  AID.InputText := Value;
  AID.InputType := itEdit;
  AID.CommonButtons := [cbOK, cbCancel];
  AID.Options := AID.Options + [doAllowDialogCancel];
  Result := AID.Execute = mrOK;
  Value := AID.InputText;
end;

//------------------------------------------------------------------------------

{ TAdvInputMetroTaskDialog }

constructor TAdvInputMetroTaskDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FInputType := itEdit;
  FInputDropDownCount := 8;
  Options := Options + [doAllowDialogCancel, doHyperLinks];
end;

function TAdvInputMetroTaskDialog.Execute: integer;
begin
  if ExpandedDefault then
    Options := Options + [doExpandedDefault]
  else
    Options := Options - [doExpandedDefault];

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

