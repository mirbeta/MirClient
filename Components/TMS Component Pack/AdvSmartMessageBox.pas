{***************************************************************************}
{ TAdvSmartMessageBox component                                             }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2007 - 2015                                        }
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

unit AdvSmartMessageBox;

{$I TMSDEFS.INC}

{$R AdvSmartMessageBox.res}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, PictureContainer, Math, AdvStyleIF, ImgList, Types
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 5; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 0; // Build nr.

  // version history
  // 1.0.0.0 : first release
  // 1.1.0.0 : New : Hide method added on message
  //         : New : Visible property added on message
  //         : New : MessageVisible added on TAdvSmartMessageBox
  // 1.1.1.0 : New : OnClick event added
  // 1.1.1.1 : Fixed : painting issue in RollInOut mode in Windows Vista
  // 1.1.2.0 : New : Tag property added in the message
  // 1.2.0.0 : New : multimonitor support added
  // 1.2.0.1 : Improved : Assign proc. implemented for TAdvSmartMessage
  // 1.2.1.0 : New : public method AdvSmartMessageBox.GetMsgSize
  // 1.2.2.0 : Improved : when DisplayRelative = drScreen, it is prevented that message will display outside screen boundaries
  // 1.3.0.0 : New : Terminal, Vista & Windows 7 styles
  // 1.3.1.0 : New : support for customizing bullets in HTML UL lists
  // 1.4.0.0 : New : Built in support for Office 2010 colors
  // 1.4.1.0 : New : Added method TAdvSmartMessage.Refresh
  // 1.4.2.0 : New : Method HideAll added
  // 1.4.2.1 : Fixed : Issue with roll in & autosize
  // 1.4.3.0 : New : Windows 8, Office 2013 styles added
  // 1.4.3.1 : Improved : Message positioning algorithm
  // 1.4.4.0 : New : function message.GetXYPosition: TPoint added
  // 1.5.0.0 : New : Windows 10, Office 2016 styles added

  MinWin_Width = 100;
  MinWin_Height = 50;
   
type
  TAdvSmartMessageBox = class;
  TAdvSmartMessageForm = class;

  TDisplayType = (dtSequence, dtStackedInOut);
  TDisplayStyle = (dsFadeInOut, dsRollInOut);
  TDisplayLocation = (dlFixedPos, dlTopLeft, dlTopRight, dlTopCenter, dlBottomLeft, dlBottomRight, dlBottomCenter, dlCenter);
  TGradientDirection = (gdHorizontal, gdVertical);
  TDisplayRelative = (drScreen, drForm);

  TDefaultSmartMessage = class(TPersistent)
  private
    FOnChange: TNotifyEvent;
    FColor: TColor;
    FColorTo: TColor;
    FRounded: Boolean;
    FShadow: Boolean;
    FOpacity: Byte;
    FText: string;
    FMirrorColorTo: TColor;
    FMirrorColor: TColor;
    FBorderColor: TColor;
    FFont: TFont;
    FMarginY: Integer;
    FMarginX: Integer;
    FGradient: TGradientDirection;
    FMaxDuration: integer;
    FMinDuration: integer;
    FTag: integer;
    procedure SetBorderColor(const Value: TColor);
    procedure SetColor(const Value: TColor);
    procedure SetColorTo(const Value: TColor);
    procedure SetFont(const Value: TFont);
    procedure SetMirrorColor(const Value: TColor);
    procedure SetMirrorColorTo(const Value: TColor);
    procedure SetOpacity(const Value: Byte);
    procedure SetRounded(const Value: Boolean);
    procedure SetShadow(const Value: Boolean);
    procedure SetText(const Value: string);
    procedure SetMarginX(const Value: Integer);
    procedure SetMarginY(const Value: Integer);
    procedure SetGradient(const Value: TGradientDirection);
    procedure SetMaxDuration(const Value: integer);
    procedure SetMinDuration(const Value: integer);
  protected
    procedure Changed;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property MirrorColor: TColor read FMirrorColor write SetMirrorColor;
    property MirrorColorTo: TColor read FMirrorColorTo write SetMirrorColorTo;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Font: TFont read FFont write SetFont;
    property Color: TColor read FColor write SetColor default clWhite;
    property ColorTo: TColor read FColorTo write SetColorTo default clSilver;
    property Gradient: TGradientDirection read FGradient write SetGradient default gdVertical;
    property MarginX: Integer read FMarginX write SetMarginX default 6;
    property MarginY: Integer read FMarginY write SetMarginY default 6;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clGray;
    property Rounded: Boolean read FRounded write SetRounded default true;
    property Shadow: Boolean read FShadow write SetShadow default false;
    property Opacity: Byte read FOpacity write SetOpacity default 230;
    property Text: string read FText write SetText;
    property MaxDuration: integer read FMaxDuration write SetMaxDuration;
    property MinDuration: integer read FMinDuration write SetMinDuration;
    property Tag: integer read FTag write FTag default 0;
  end;

  TAdvSmartMessage = class(TCollectionItem)
  private
    FColor: TColor;
    FColorTo: TColor;
    FRounded: Boolean;
    FShadow: Boolean;
    FOpacity: Byte;
    FText: string;
    FMirrorColorTo: TColor;
    FMirrorColor: TColor;
    FBorderColor: TColor;
    FFont: TFont;
    FMsgWindow: TAdvSmartMessageForm;
    FMarginY: Integer;
    FMarginX: Integer;
    FGradient: TGradientDirection;
    FMinDuration: integer;
    FMaxDuration: integer;
    FTag: integer;
    procedure SetColor(const Value: TColor);
    procedure SetColorTo(const Value: TColor);
    procedure SetBorderColor(const Value: TColor);
    procedure SetFont(const Value: TFont);
    procedure SetMirrorColor(const Value: TColor);
    procedure SetMirrorColorTo(const Value: TColor);
    procedure SetOpacity(const Value: Byte);
    procedure SetRounded(const Value: Boolean);
    procedure SetShadow(const Value: Boolean);
    procedure SetText(const Value: string);
    procedure SetMarginX(const Value: Integer);
    procedure SetMarginY(const Value: Integer);
    procedure SetGradient(const Value: TGradientDirection);
    procedure SetMaxDuration(const Value: integer);
    procedure SetMinDuration(const Value: integer);
    function GetVisible: Boolean;
    procedure SetTag(const Value: integer);
  protected
    FDestroying: Boolean;
    procedure Changed;
    property MsgWindow: TAdvSmartMessageForm read FMsgWindow write FMsgWindow;
    property MirrorColor: TColor read FMirrorColor write SetMirrorColor;
    property MirrorColorTo: TColor read FMirrorColorTo write SetMirrorColorTo;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Show;
    procedure Hide;
    function GetXYPosition: TPoint;
    property Visible: Boolean read GetVisible;
    procedure Assign(Source: TPersistent); override;
    procedure Refresh;
  published
    property Font: TFont read FFont write SetFont;
    property Color: TColor read FColor write SetColor;// default clWhite;
    property ColorTo: TColor read FColorTo write SetColorTo;// default clSilver;
    property Gradient: TGradientDirection read FGradient write SetGradient;// default gdVertical;
    property BorderColor: TColor read FBorderColor write SetBorderColor;// default clGray;
    property MarginX: Integer read FMarginX write SetMarginX;// default 6;
    property MarginY: Integer read FMarginY write SetMarginY;// default 6;
    property Rounded: Boolean read FRounded write SetRounded;// default true;
    property Shadow: Boolean read FShadow write SetShadow;// default false;
    property Opacity: Byte read FOpacity write SetOpacity;// default 230; //(0..255) : transparency of the form with the message
    property Text: string read FText write SetText;  // can contain HTML formatted text
    property MaxDuration: integer read FMaxDuration write SetMaxDuration;
    property MinDuration: integer read FMinDuration write SetMinDuration;
    property Tag: integer read FTag write SetTag;
  end;

  TAdvSmartMessages = class(TCollection)
  private
    FOwner: TAdvSmartMessageBox;
    FOnChange: TNotifyEvent;
    function GetItem(Index: Integer): TAdvSmartMessage;
    procedure SetItem(Index: Integer; const Value: TAdvSmartMessage);
  protected
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    constructor Create(AOwner: TAdvSmartMessageBox);
    property Items[Index: Integer]: TAdvSmartMessage read GetItem write SetItem; default;
    function Add: TAdvSmartMessage;
    function Insert(Index: Integer): TAdvSmartMessage;
    procedure Delete(Index: Integer);
    function GetOwner: TPersistent; override;
    procedure ShowAll;
  end;

  TAdvSmartMessageForm = class(TCustomForm)
  private
    FMinShowTimer: TTimer;
    FMaxShowTimer: TTimer;
    FOldXCoOrd: Integer;
    FOldYCoOrd: Integer;
    FSmartMsg: TAdvSmartMessage;
    FAdvSmartMessageBox: TAdvSmartMessageBox;
    FLockHiding: Boolean;
    FDisplayed: Boolean;
    procedure MinShowTimerTimer(Sender: TObject);
    procedure MaxShowTimerTimer(Sender: TObject);
    procedure SetAdvSmartMessageBox(const Value: TAdvSmartMessageBox);
    procedure SetSmartMsg(const Value: TAdvSmartMessage);
    procedure WMEraseBkGnd(var Message: TWMEraseBkGnd); message WM_ERASEBKGND;
    procedure WMActivate(var Message: TWMActivate); message WM_ACTIVATE;
    procedure WMMouseActivate(var Message: TMessage); message WM_MOUSEACTIVATE;
    procedure WMDestroy(var Message: TMessage); message WM_DESTROY;
  protected
    function GetParentWnd: HWnd;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure Activate; override;
    procedure DoCreate; override;
    procedure DoDestroy; override;
    procedure Loaded; override;
    procedure Paint; override;
    procedure DoHide; override;
    procedure DoShow; override;
    procedure Click; override;

    procedure Animate(DoSetVisible: Bool = false);

    function GetMsgSize(Msg: TAdvSmartMessage): TSize;
    procedure UpdateWindow;
    procedure HideEx;

    property SmartMsg: TAdvSmartMessage read FSmartMsg write SetSmartMsg;
    property AdvSmartMessageBox: TAdvSmartMessageBox read FAdvSmartMessageBox write SetAdvSmartMessageBox;
  public
  end;

  TMsgClickEvent = procedure (Sender: TObject; Index: Integer) of object;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvSmartMessageBox = class(TComponent, ITMSStyle)
  private
    FOwner: TComponent;
    FMessages: TAdvSmartMessages;
    FDefaultMessage: TDefaultSmartMessage;
    FDisplayLocation: TDisplayLocation;
    FDisplayStyle: TDisplayStyle;
    FDisplayType: TDisplayType;
    FDisplayOffsetY: Integer;
    FDisplayOffsetX: Integer;
    FMsgForms: TList;
    FDisplayMarginX: integer;
    FDisplayMarginY: integer;
    FImages: TImageList;
    FHoverLink: Integer;
    FImageCache: THTMLPictureCache;
    FContainer: TPictureContainer;
    FAutoDestroyMessage: Boolean;
    FDisplayPointX: Integer;
    FDisplayPointY: Integer;
    FDisplayRelative: TDisplayRelative;
    FStyle: TTMSStyle;
    FOnClick: TMsgClickEvent;
    FProgClose: boolean;
    procedure OnMessagesChanged(Sender: TObject);
    procedure SetMessages(const Value: TAdvSmartMessages);
    procedure SetDefaultMessage(const Value: TDefaultSmartMessage);
    procedure SetDisplayLocation(const Value: TDisplayLocation);
    procedure SetDisplayStyle(const Value: TDisplayStyle);
    procedure SetDisplayType(const Value: TDisplayType);
    procedure SetDisplayOffsetX(const Value: Integer);
    procedure SetDisplayOffsetY(const Value: Integer);
    procedure SetImages(const Value: TImageList);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    procedure SetStyle(const Value: TTMSStyle);
    function GetMessageVisible: Boolean;
  protected
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    function CreateMsgForm(Msg: TAdvSmartMessage): TAdvSmartMessageForm;
    procedure DestroyMsgForm(Msg: TAdvSmartMessage);
    procedure Show(Msg: TAdvSmartMessage); overload;
    procedure Show(Index: Integer); overload;
    procedure ShowAll;
    procedure InitializeMsg(Msg: TAdvSmartMessage);

    procedure CheckKeyDown(Key: word);
    procedure CheckMouseMoved(p: TPoint);
    procedure CheckMouseClicked;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Hide(Msg: TAdvSmartMessage); overload;
    procedure Hide(Index: Integer); overload;
    procedure HideAll;
    function GetMsgSize(Msg: TAdvSmartMessage; Canvas: TCanvas): TSize;    
    function GetVersionNr: Integer; virtual;
    procedure SetComponentStyle(AStyle: TTMSStyle);
    property MessageVisible: Boolean read GetMessageVisible;
  published
    property AutoDestroyMessage: Boolean read FAutoDestroyMessage write FAutoDestroyMessage default true;
    property DefaultMessage: TDefaultSmartMessage read FDefaultMessage write SetDefaultMessage;
    property Messages: TAdvSmartMessages read FMessages write SetMessages;
    property DisplayOffsetX: Integer read FDisplayOffsetX write SetDisplayOffsetX default 10;
    property DisplayOffsetY: Integer read FDisplayOffsetY write SetDisplayOffsetY default 10;
    property DisplayStyle: TDisplayStyle read FDisplayStyle write SetDisplayStyle default dsFadeInOut;
    property DisplayType: TDisplayType read FDisplayType write SetDisplayType default dtSequence;
    property DisplayLocation: TDisplayLocation read FDisplayLocation write SetDisplayLocation default dlCenter;
    property DisplayPointX: Integer read FDisplayPointX write FDisplayPointX;
    property DisplayPointY: Integer read FDisplayPointY write FDisplayPointY;
    property DisplayRelative: TDisplayRelative read FDisplayRelative write FDisplayRelative;
    property Images: TImageList read FImages write SetImages;
    property PictureContainer: TPictureContainer read FContainer write FContainer;
    property DisplayMarginX: integer read FDisplayMarginX write FDisplayMarginX default 0;
    property DisplayMarginY: integer read FDisplayMarginY write FDisplayMarginY default 0;
    property Style: TTMSStyle read FStyle write SetStyle default tsOffice2003Blue;
    property Version: string read GetVersion write SetVersion;
    property OnClick: TMsgClickEvent read FOnClick write FOnClick;
  end;

implementation

uses
  CommCtrl, ShellAPI;

{$I HTMLENGO.PAS}

//----------------------------------------------------------------- DrawGradient

procedure DrawGradient(Canvas: TCanvas; FromColor, ToColor: TColor; Steps: Integer; R: TRect; Direction: Boolean);
var
  diffr, startr, endr: Integer;
  diffg, startg, endg: Integer;
  diffb, startb, endb: Integer;
  rstepr, rstepg, rstepb, rstepw: Real;
  i, stepw: Word;

begin
  if Steps = 0 then
    Steps := 1;

  FromColor := ColorToRGB(FromColor);
  ToColor := ColorToRGB(ToColor);

  startr := (FromColor and $0000FF);
  startg := (FromColor and $00FF00) shr 8;
  startb := (FromColor and $FF0000) shr 16;
  endr := (ToColor and $0000FF);
  endg := (ToColor and $00FF00) shr 8;
  endb := (ToColor and $FF0000) shr 16;

  diffr := endr - startr;
  diffg := endg - startg;
  diffb := endb - startb;

  rstepr := diffr / steps;
  rstepg := diffg / steps;
  rstepb := diffb / steps;

  if Direction then
    rstepw := (R.Right - R.Left) / Steps
  else
    rstepw := (R.Bottom - R.Top) / Steps;

  with Canvas do
  begin
    for i := 0 to steps - 1 do
    begin
      endr := startr + Round(rstepr * i);
      endg := startg + Round(rstepg * i);
      endb := startb + Round(rstepb * i);
      stepw := Round(i * rstepw);
      Pen.Color := endr + (endg shl 8) + (endb shl 16);
      Brush.Color := Pen.Color;
      if Direction then
        Rectangle(R.Left + stepw, R.Top, R.Left + stepw + Round(rstepw) + 1, R.Bottom)
      else
        Rectangle(R.Left, R.Top + stepw, R.Right, R.Top + stepw + Round(rstepw) + 1);
    end;
  end;
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

{ SmartMsgHook }

var
  SmartMsgHook: HHOOK;
  SmartMessageBox: TAdvSmartMessageBox;
  DoHideOnMinDur: Boolean = false;
  LastMousePos: TPoint;

function SmartBoxGetMsgHook(Code: Integer; WParam: Longint; var Msg: TMsg): Longint; stdcall;
begin
  if Assigned(SmartMessageBox) then
  begin
    case Msg.Message of
      WM_KEYDOWN:    SmartMessageBox.CheckKeyDown(Msg.WParam);
      WM_MOUSEMOVE:  SmartMessageBox.CheckMouseMoved(Msg.pt);
      WM_LBUTTONDOWN, WM_RBUTTONDOWN: SmartMessageBox.CheckMouseClicked;
    end;
  end;

  if (Msg.Message = WM_KEYDOWN) or (Msg.Message = WM_CHAR) {or (Msg.Message = WM_KEYUP)} or
     (Msg.Message = WM_LBUTTONDOWN) or (Msg.Message = WM_LBUTTONUP) or
     (Msg.Message = WM_RBUTTONDOWN) or (Msg.Message = WM_RBUTTONUP) then
    DoHideOnMinDur := true;

  if not DoHideOnMinDur and (Msg.Message = WM_MOUSEMOVE) then
  begin
    if (abs(LastMousePos.X - Msg.pt.X) >= 20) or (abs(LastMousePos.Y - Msg.pt.Y) >= 20) then
      DoHideOnMinDur := true;
  end;

  Result := CallNextHookEx(SmartMsgHook, Code, WParam, LParam(@Msg));
end;

//------------------------------------------------------------------------------

procedure InitMsgHooks(SmartMsgBox: TAdvSmartMessageBox);
begin
  if SmartMsgHook = 0 then
  begin
    SmartMessageBox := SmartMsgBox;
    GetCursorPos(LastMousePos);
    SmartMsgHook := SetWindowsHookEx(WH_GETMESSAGE, @SmartBoxGetMsgHook, 0,
      GetCurrentThreadID);
  end;
end;

//------------------------------------------------------------------------------

procedure ReleaseMsgHooks;
begin
  if SmartMsgHook <> 0 then
    UnhookWindowsHookEx(SmartMsgHook);
  SmartMsgHook := 0;
  SmartMessageBox := nil;
  DoHideOnMinDur := False;
end;

//------------------------------------------------------------------------------

{ TAdvSmartMessage }

constructor TAdvSmartMessage.Create(Collection: TCollection);
begin
  inherited;
  FDestroying := False;
  FFont := TFont.Create;
  FColor := clWhite;
  FColorTo := clSilver;
  FBorderColor := clGray;
  FMsgWindow := nil;
  FMarginX := 6;
  FMarginY := 6;
  FGradient := gdVertical;
  FOpacity := 230;
  FMaxDuration := 4000;
  FMinDuration := 2000;
  FRounded := True;
  FShadow := false;
  if Assigned(TAdvSmartMessages(Collection).FOwner) then
    TAdvSmartMessages(Collection).FOwner.InitializeMsg(Self);
end;

//------------------------------------------------------------------------------

destructor TAdvSmartMessage.Destroy;
begin
  if Assigned(TAdvSmartMessages(Collection).FOwner) and Assigned(FMsgWindow) then
  begin
    FDestroying := True;
    TAdvSmartMessages(Collection).FOwner.Hide(Self);
  end;
  FFont.Free;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvSmartMessage.Assign(Source: TPersistent);
begin
  if (Source is TAdvSmartMessage) then
  begin
    FFont.Assign((Source as TAdvSmartMessage).Font);
    FColor := (Source as TAdvSmartMessage).Color;
    FColorTo := (Source as TAdvSmartMessage).ColorTo;
    FGradient := (Source as TAdvSmartMessage).Gradient;
    FBorderColor := (Source as TAdvSmartMessage).BorderColor;
    FMarginX := (Source as TAdvSmartMessage).MarginX;
    FMarginY := (Source as TAdvSmartMessage).MarginY;
    FRounded := (Source as TAdvSmartMessage).Rounded;
    FShadow := (Source as TAdvSmartMessage).Shadow;
    FOpacity := (Source as TAdvSmartMessage).Opacity;
    FText := (Source as TAdvSmartMessage).Text;
    FMaxDuration := (Source as TAdvSmartMessage).MaxDuration;
    FMinDuration := (Source as TAdvSmartMessage).MinDuration;
    FTag := (Source as TAdvSmartMessage).Tag;
  end;
end;

procedure TAdvSmartMessage.Changed;
begin
  if Assigned(TAdvSmartMessages(Collection).OnChange) then
    TAdvSmartMessages(Collection).OnChange(TAdvSmartMessages(Collection));
end;

//------------------------------------------------------------------------------

procedure TAdvSmartMessage.SetBorderColor(const Value: TColor);
begin
  if (FBorderColor <> Value) then
  begin
    FBorderColor := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmartMessage.SetColor(const Value: TColor);
begin
  if (FColor <> Value) then
  begin
    FColor := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmartMessage.SetColorTo(const Value: TColor);
begin
  if (FColorTo <> Value) then
  begin
    FColorTo := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmartMessage.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TAdvSmartMessage.SetMirrorColor(const Value: TColor);
begin
  if (FMirrorColor <> Value) then
  begin
    FMirrorColor := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmartMessage.SetMirrorColorTo(const Value: TColor);
begin
  if (FMirrorColorTo <> Value) then
  begin
    FMirrorColorTo := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmartMessage.SetOpacity(const Value: Byte);
begin
  if (FOpacity <> Value) then
  begin
    FOpacity := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmartMessage.SetRounded(const Value: Boolean);
begin
  if (FRounded <> Value) then
  begin
    FRounded := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmartMessage.SetShadow(const Value: Boolean);
begin
  if (FShadow <> Value) then
  begin
    FShadow := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmartMessage.SetTag(const Value: integer);
begin
  if (FTag <> Value) then
  begin
    FTag := Value;
    Changed;
  end;
end;

procedure TAdvSmartMessage.SetText(const Value: string);
begin
  if (FText <> Value) then
  begin
    FText := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmartMessage.Show;
begin
  if Assigned(TAdvSmartMessages(Collection).FOwner) then
  begin
    TAdvSmartMessages(Collection).FOwner.Show(Self);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmartMessage.Hide;
begin
  if Assigned(TAdvSmartMessages(Collection).FOwner) then
  begin
    TAdvSmartMessages(Collection).FOwner.Hide(Self);
  end;
end;

procedure TAdvSmartMessage.Refresh;
begin
  if Assigned(FMsgWindow) then
    if FMsgWindow.Visible then
      FMsgWindow.Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvSmartMessage.SetMarginX(const Value: Integer);
begin
  if (FMarginX <> Value) then
  begin
    FMarginX := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmartMessage.SetMarginY(const Value: Integer);
begin
  if (FMarginY <> Value) then
  begin
    FMarginY := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmartMessage.SetGradient(const Value: TGradientDirection);
begin
  if (FGradient <> Value) then
  begin
    FGradient := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmartMessage.SetMaxDuration(const Value: integer);
begin
  if (FMaxDuration <> Value) then
  begin
    FMaxDuration := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmartMessage.SetMinDuration(const Value: integer);
begin
  if (FMinDuration <> Value) then
  begin
    FMinDuration := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

function TAdvSmartMessage.GetVisible: Boolean;
begin
  Result := Assigned(FMsgWindow) and (FMsgWindow.Visible);
end;

function TAdvSmartMessage.GetXYPosition: TPoint;
begin
  Result := Point(-1,-1);
  if Assigned(FMsgWindow) and (FMsgWindow.Visible) then
    Result := Point(FMsgWindow.Left, FMsgWindow.Top);
end;

//------------------------------------------------------------------------------

{ TAdvSmartMessages }

function TAdvSmartMessages.Add: TAdvSmartMessage;
begin
  Result := TAdvSmartMessage(inherited Add);
end;

//------------------------------------------------------------------------------

constructor TAdvSmartMessages.Create(AOwner: TAdvSmartMessageBox);
begin
  inherited Create(TAdvSmartMessage);
  FOwner := AOwner;
end;

//------------------------------------------------------------------------------

procedure TAdvSmartMessages.Delete(Index: Integer);
begin
  Items[Index].Free;
end;

//------------------------------------------------------------------------------

function TAdvSmartMessages.GetItem(Index: Integer): TAdvSmartMessage;
begin
  Result := TAdvSmartMessage(inherited Items[Index]);
end;

//------------------------------------------------------------------------------

function TAdvSmartMessages.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

//------------------------------------------------------------------------------

function TAdvSmartMessages.Insert(Index: Integer): TAdvSmartMessage;
begin
  Result := TAdvSmartMessage(inherited Insert(Index));
end;

//------------------------------------------------------------------------------

procedure TAdvSmartMessages.SetItem(Index: Integer;
  const Value: TAdvSmartMessage);
begin
  inherited Items[Index] := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvSmartMessages.ShowAll;
var
  i: Integer;
  ItemList: TList;
begin
  ItemList := TList.Create;
  for i := 0 to Count - 1 do
    ItemList.Add(Items[i]);

  for i := 0 to ItemList.Count - 1 do
    TAdvSmartMessage(ItemList[i]).Show;
  ItemList.Free;
end;

//------------------------------------------------------------------------------

{ TAdvSmartMessageBox }

constructor TAdvSmartMessageBox.Create(AOwner: TComponent);
begin
  inherited;
  FOwner := AOwner;
  FDefaultMessage := TDefaultSmartMessage.Create;
  FMessages := TAdvSmartMessages.Create(Self);
  FMessages.OnChange := OnMessagesChanged;
  FDisplayOffsetX := 10;
  FDisplayOffsetY := 10;
  FDisplayMarginX := 0;
  FDisplayMarginY := 0;
  FImages := nil;
  FHoverLink := -1;
  FImageCache := THTMLPictureCache.Create;
  FMsgForms := TList.Create;
  FAutoDestroyMessage := True;
  FDisplayStyle := dsFadeInOut;
  FDisplayType := dtSequence;
  FDisplayPointX := 0;
  FDisplayPointY := 0;
  FDisplayLocation := dlCenter;
  FDisplayRelative := drScreen;
  FProgClose := false;
end;

//------------------------------------------------------------------------------

destructor TAdvSmartMessageBox.Destroy;
begin
  ReleaseMsgHooks;
  FreeAndNil(FMessages);
  FreeAndNil(FDefaultMessage);
  FreeAndNil(FImageCache);
  FreeAndNil(FMsgForms);
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvSmartMessageBox.OnMessagesChanged(Sender: TObject);
begin

end;

//------------------------------------------------------------------------------

procedure TAdvSmartMessageBox.SetComponentStyle(AStyle: TTMSStyle);
begin

  case AStyle of
    tsOffice2003Blue:
      begin
        DefaultMessage.Color := $FADAC4;
        DefaultMessage.ColorTo := $F5BFA0;
        DefaultMessage.BorderColor := $962D00;
      end;
    tsOffice2003Silver:
      begin
        DefaultMessage.Color := $F7F3F3;
        DefaultMessage.ColorTo := $E6D8D8;
        DefaultMessage.BorderColor := $947C7C;
      end;
    tsOffice2003Olive:
      begin
        DefaultMessage.Color := $E4F1F2;
        DefaultMessage.ColorTo := $AADADA;
        DefaultMessage.BorderColor := $588060;
      end;
    tsOffice2003Classic:
      begin
        DefaultMessage.Color := clWhite;
        DefaultMessage.ColorTo := $C9D1D5;
        DefaultMessage.BorderColor := clGray;
      end;
    tsOffice2007Luna:
      begin
        DefaultMessage.Color := $FFF4E3;
        DefaultMessage.ColorTo := $EDD9C8;
        DefaultMessage.BorderColor := $FFD2AF;
      end;
    tsOffice2007Obsidian:
      begin
        DefaultMessage.Color := $F1F0E6;
        DefaultMessage.ColorTo := $C6BCB5;
        DefaultMessage.BorderColor := $5C534C;
      end;
    tsOffice2007Silver:
      begin
        DefaultMessage.Color := $F8F7F6;
        DefaultMessage.ColorTo := $E8E0DB;
        DefaultMessage.BorderColor := $74706F;
      end;
    tsWindowsXP:
      begin
        DefaultMessage.Color := clWhite;
        DefaultMessage.ColorTo := clBtnFace;
        DefaultMessage.BorderColor := clBlack;
      end;
    tsWindowsVista:
      begin
        DefaultMessage.Color := $FDF8F1;
        DefaultMessage.ColorTo := $FCEFD5;
        DefaultMessage.BorderColor := $FDDE99;
      end;
    tsWindows7:
      begin
        DefaultMessage.Color := $FCEBDC;
        DefaultMessage.ColorTo := $FCDBC1;
        DefaultMessage.BorderColor := $CEA27D;
      end;
    tsTerminal:
      begin
        DefaultMessage.Color := clBtnFace;
        DefaultMessage.ColorTo := clBtnFace;
        DefaultMessage.BorderColor := clGray;
      end;
    tsWhidbey:
      begin
        DefaultMessage.Color := $F5F9FA;
        DefaultMessage.ColorTo := $A8C0C0;
        DefaultMessage.BorderColor := $7E9898;
      end;
    tsOffice2010Blue:
      begin
        DefaultMessage.Color := $FDF6EF;
        DefaultMessage.ColorTo := $F0DAC7;
        DefaultMessage.BorderColor := $C7B29F;
      end;
    tsOffice2010Silver:
      begin
        DefaultMessage.Color := $FFFFFF;
        DefaultMessage.ColorTo := $EDE5E0;
        DefaultMessage.BorderColor := $D2CDC8;
      end;
    tsOffice2010Black:
      begin
        DefaultMessage.Color := $BFBFBF;
        DefaultMessage.ColorTo := $919191;
        DefaultMessage.BorderColor := $6D6D6D;
      end;
    tsWindows8, tsWindows10:
      begin
        DefaultMessage.Color := $F7F6F5;
        DefaultMessage.ColorTo := $F7F6F5;
        DefaultMessage.BorderColor := $E4E3E2;
      end;
   tsOffice2013White:
      begin
        DefaultMessage.Color := clWhite;
        DefaultMessage.ColorTo := clWhite;
        DefaultMessage.BorderColor := $D4D4D4;
      end;
   tsOffice2013LightGray:
      begin
        DefaultMessage.Color := $F6F6F6;
        DefaultMessage.ColorTo := $F6F6F6;
        DefaultMessage.BorderColor := $C6C6C6;
      end;
   tsOffice2013Gray:
      begin
        DefaultMessage.Color := $E5E5E5;
        DefaultMessage.ColorTo := $E5E5E5;
        DefaultMessage.BorderColor := $ABABAB;
      end;
   tsOffice2016White:
      begin
        DefaultMessage.Color := clWhite;
        DefaultMessage.ColorTo := clWhite;
        DefaultMessage.BorderColor := $D4D4D4;
      end;
   tsOffice2016Gray:
      begin
        DefaultMessage.Color := $B2B2B2;
        DefaultMessage.ColorTo := $B2B2B2;
        DefaultMessage.BorderColor := $444444;
      end;
   tsOffice2016Black:
      begin
        DefaultMessage.Color := $6A6A6A;
        DefaultMessage.ColorTo := $6A6A6A;
        DefaultMessage.BorderColor := $363636;
      end;



    tsCustom:
      begin
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmartMessageBox.SetDefaultMessage(
  const Value: TDefaultSmartMessage);
begin
  FDefaultMessage.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TAdvSmartMessageBox.SetDisplayLocation(
  const Value: TDisplayLocation);
begin
  if (FDisplayLocation <> Value) then
  begin
    FDisplayLocation := Value;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmartMessageBox.SetDisplayOffsetX(const Value: Integer);
begin
  FDisplayOffsetX := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvSmartMessageBox.SetDisplayOffsetY(const Value: Integer);
begin
  FDisplayOffsetY := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvSmartMessageBox.SetDisplayStyle(const Value: TDisplayStyle);
begin
  if (FDisplayStyle <> Value) then
  begin
    FDisplayStyle := Value;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmartMessageBox.SetDisplayType(const Value: TDisplayType);
begin
  if (FDisplayType <> Value) then
  begin
    FDisplayType := Value;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmartMessageBox.SetMessages(const Value: TAdvSmartMessages);
begin
  FMessages.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TAdvSmartMessageBox.SetStyle(const Value: TTMSStyle);
begin
  FStyle := Value;
  SetComponentStyle(FStyle);
end;

//------------------------------------------------------------------------------

function TAdvSmartMessageBox.CreateMsgForm(
  Msg: TAdvSmartMessage): TAdvSmartMessageForm;
begin
  Result := nil;
  if not Assigned(Msg) or Assigned(Msg.MsgWindow) then
    Exit;

  Result := TAdvSmartMessageForm.CreateNew(FOwner);
  Result.AdvSmartMessageBox := Self;
  Result.SmartMsg := Msg;
  Result.AlphaBlend := True;
  Result.AlphaBlendValue := 5;
  Result.BorderIcons := [];
  Result.BorderStyle := bsNone;
  Result.Ctl3D := false;
  Result.FormStyle := fsStayOnTop;
  Result.Position := poDesigned;
end;

//------------------------------------------------------------------------------

procedure TAdvSmartMessageBox.DestroyMsgForm(Msg: TAdvSmartMessage);
var
  i: Integer;
begin
  if not Assigned(Msg) or not Assigned(Msg.MsgWindow) or not Assigned(FMsgForms) then
    Exit;

  i := FMsgForms.IndexOf(Msg.MsgWindow);

  if (i >= 0) then
  begin
    TAdvSmartMessageForm(FmsgForms[i]).Free;
    FMsgForms.Delete(i);
  end;

  Msg.MsgWindow := nil;
  if (FMsgForms.Count <= 0) then
    ReleaseMsgHooks;

  if AutoDestroyMessage and not (Msg.FDestroying) then
  begin
    FreeAndNil(Msg);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmartMessageBox.Show(Msg: TAdvSmartMessage);
var
  IsMyMsg: Boolean;
  MsgWin, MsgWin2: TAdvSmartMessageForm;
  P: TPoint;
  sr, sr2: TRect;
  frm: TForm;
  dspx, dspy, lastY, i, gap: integer;
begin
  if not Assigned(Msg) then
    Exit;
  if (csDestroying in ComponentState) then
    Exit;

  // Check for child msg
  IsMyMsg := False;
  FProgClose := False;

  if (Msg.Index >= 0) and (Msg.Index < FMessages.Count) then
    IsMyMsg := (FMessages.Items[Msg.Index] = Msg);
  if not IsMyMsg then
    Exit;

  // Display Message here
  MsgWin := CreateMsgForm(Msg);
  frm := nil;
  if Assigned(MsgWin) then
  begin
    if (DisplayRelative = drForm) and ((Assigned(FOwner) and (FOwner is TForm)) or Assigned(Application.MainForm)) then
    begin
      sr := Rect(0, 0, 800, 600);
      if Assigned(FOwner) and (FOwner is TForm) then
      begin
        frm := TForm(FOwner);
      end
      else if Assigned(Application.MainForm) then
      begin
        frm := Application.MainForm;
      end;

      if Assigned(frm) then
      begin
        sr := frm.ClientRect;
        sr.TopLeft := frm.ClientToScreen(sr.TopLeft);
        sr.BottomRight := frm.ClientToScreen(sr.BottomRight);
      end;
    end
    else  // (DisplayRelative = drScreen)
    begin
      // Set display point
      SystemParametersInfo(SPI_GETWORKAREA, 0, @sr, 0);

      //-- check Multi Monitor
      if (Screen.MonitorCount > 1) then
      begin
        P := Point(-100, -100);
        if Assigned(FOwner) and (FOwner is TForm) then
        begin
          P := Point(TForm(FOwner).Left, TForm(FOwner).Top);
          P := TForm(FOwner).ClientToScreen(P);
        end
        else if Assigned(Application.MainForm) then
        begin
          P := Point(Application.MainForm.Left, Application.MainForm.Top);
          P := Application.MainForm.ClientToScreen(P);
        end;

        if (P.X > 0) then
        begin
          sr := Screen.MonitorFromPoint(P).WorkareaRect;
        end;
      end;
    end;

    P := Point(0, 0);
    if (FMsgForms.Count > 0) then
    begin  // another message is being displayed
      MsgWin2 := FMsgForms[FMsgForms.Count-1];
      case FDisplayType of
        dtSequence:
        begin
          gap := 1;
          P := Point(MsgWin2.Left, MsgWin2.Top + MsgWin2.Height + gap);


          case FDisplayLocation of
            dlFixedPos,
            dlTopLeft,
            dlCenter,
            dlTopCenter:
            begin
              P := Point(MsgWin2.Left, MsgWin2.Top + MsgWin2.Height + gap);
              if (P.Y + MsgWin.Height > sr.Bottom - FDisplayMarginY) then
              begin
                lastY := MaxInt;
                i := FMsgForms.Count - 1;
                while (i >= 0) and (TAdvSmartMessageForm(FMsgForms[i]).Top < lastY) do
                  with TAdvSmartMessageForm(FMsgForms[i]) do
                  begin
                    lastY := Top;
                    if (Left + Width >= P.X) then P.X := Left + Width + gap;
                    Dec(i);
                  end;
                P.Y := sr.Top + FDisplayMarginY;
                if (P.X + MsgWin.Width >= sr.Right - FDisplayMarginX) then
                  P.X := sr.Left + FDisplayMarginX;
              end;
            end;
            dlTopRight:
            begin
              P := Point(MsgWin2.Left + MsgWin2.Width - MsgWin.Width, MsgWin2.Top + MsgWin2.Height + gap);
              if (P.Y + MsgWin.Height > sr.Bottom - FDisplayMarginY) then
              begin
                lastY := -MaxInt;
                i := FMsgForms.Count - 1;
                while (i >= 0) and (TAdvSmartMessageForm(FMsgForms[i]).Top > lastY) do
                  with TAdvSmartMessageForm(FMsgForms[i]) do
                  begin
                    lastY := Top;
                    if (Left <= P.X + MsgWin.Width) then P.X := Left - MsgWin.Width - gap;
                    Dec(i);
                  end;
                P.Y := sr.Top + FDisplayMarginY;
                if (P.X < sr.Left + FDisplayMarginX) then
                  P.X := sr.Right - FDisplayMarginX - MsgWin.Width;
              end;
            end;
            dlBottomLeft,
            dlBottomCenter:
            begin
              P := Point(MsgWin2.Left, MsgWin2.Top - MsgWin.Height - gap);
              if (P.Y < sr.Top + FDisplayMarginY) then
              begin
                lastY := MaxInt;
                i := FMsgForms.Count - 1;
                while (i >= 0) and (TAdvSmartMessageForm(FMsgForms[i]).Top < lastY) do
                  with TAdvSmartMessageForm(FMsgForms[i]) do
                  begin
                    lastY := Top;
                    if (Left + Width >= P.X) then P.X := Left + Width + gap;
                    Dec(i);
                  end;
                P.Y := sr.Bottom - FDisplayMarginY - MsgWin.Height;
                if (P.X + MsgWin.Width >= sr.Right - FDisplayMarginX) then
                  P.X := sr.Left + FDisplayMarginX;
              end;
            end;
            dlBottomRight:
            begin
              P := Point(MsgWin2.Left + MsgWin2.Width - MsgWin.Width, MsgWin2.Top - MsgWin.Height - gap);
              if (P.Y < sr.Top + FDisplayMarginY) then
              begin
                lastY := -MaxInt;
                i := FMsgForms.Count - 1;
                while (i >= 0) and (TAdvSmartMessageForm(FMsgForms[i]).Top > lastY) do
                  with TAdvSmartMessageForm(FMsgForms[i]) do
                  begin
                    lastY := Top;
                    if (Left <= P.X + MsgWin.Width) then P.X := Left - MsgWin.Width - gap;
                    Dec(i);
                  end;
                P.Y := sr.Bottom - FDisplayMarginY - MsgWin.Height;
                if (P.X < sr.Left + FDisplayMarginX) then
                  P.X := sr.Right - FDisplayMarginX - MsgWin.Width;
              end;
            end;
          end;

        end;
        dtStackedInOut:
        begin
          P := Point(MsgWin2.Left + DisplayOffsetX, MsgWin2.Top + DisplayOffsetY);
        end;
      end;
    end
    else
    begin
      if (DisplayRelative = drForm) and Assigned(frm) then
      begin
        sr2 := frm.BoundsRect;
        case FDisplayLocation of
          dlFixedPos:
          begin
            P := Point(sr.Left + DisplayPointX, sr.Top + DisplayPointY);
          end;
          dlTopLeft:
          begin
            P := Point(sr.Left + FDisplayOffsetX, sr.Top + FDisplayOffsetY);
          end;
          dlTopRight:
          begin
            P := Point(sr.Right - MsgWin.Width - FDisplayMarginX, sr.Top + FDisplayOffsetY);
          end;
          dlTopCenter:
          begin
            P := Point((sr.Left + sr.Right - MsgWin.Width) div 2, sr.Top + FDisplayOffsetY);
          end;
          dlBottomLeft:
          begin
            P := Point(sr.Left + FDisplayOffsetX, sr.Bottom - MsgWin.Height - FDisplayOffsetY);
          end;
          dlBottomRight:
          begin
            P := Point(sr.Right - MsgWin.Width - FDisplayOffsetX, sr.Bottom - MsgWin.Height - FDisplayOffsetY);
          end;
         dlBottomCenter:
          begin
            P := Point((sr.Left + sr.Right - MsgWin.Width) div 2, sr.Bottom - MsgWin.Height - FDisplayOffsetY);
          end;
          dlCenter:
          begin
            P := Point((sr.Left + sr.Right - MsgWin.Width) div 2, (sr.Top + sr.Bottom - MsgWin.Height) div 2);
          end;
        end;
      end
      else  // (DisplayRelative = drScreen)
      begin
        case FDisplayLocation of
          dlFixedPos:
            begin
              dspx := DisplayPointX;
              dspy := DisplayPointY;

              if dspx + MsgWin.Width > sr.Right then
                dspx := sr.Right - MsgWin.Width;
              if dspy + MsgWin.Height > sr.Bottom then
                dspy := sr.Bottom - MsgWin.Height;
              P := Point(dspx,dspy);
            end;
          dlTopLeft:      P := Point(sr.Left + FDisplayMarginX, FDisplayMarginY);
          dlTopRight:     P := Point(sr.Right - MsgWin.Width - FDisplayMarginX, FDisplayMarginY);
          dlTopCenter:    P := Point(sr.Left + FDisplayMarginX + ((sr.Right - sr.Left) - MsgWin.Width - (FDisplayMarginX * 2)) div 2, FDisplayMarginY);
          dlBottomLeft:   P := Point(sr.Left + FDisplayMarginX, sr.Bottom - FDisplayMarginY - MsgWin.Height);
          dlBottomRight:  P := Point(sr.Right - MsgWin.Width - FDisplayMarginX, sr.Bottom - FDisplayMarginY - MsgWin.Height);
          dlBottomCenter: P := Point(sr.Left + FDisplayMarginX + ((sr.Right - sr.Left) - MsgWin.Width - (FDisplayMarginX * 2)) div 2, sr.Bottom - FDisplayMarginY - MsgWin.Height);
          dlCenter:       MsgWin.Position := poScreenCenter;
        end;
      end;
    end;

    MsgWin.Left := P.X;
    MsgWin.Top := P.Y;
    FMsgForms.Add(MsgWin);
    InitMsgHooks(Self);

    if (DisplayStyle = dsFadeInOut) then
    begin
      ShowWindow(MsgWin.Handle, SW_SHOWNOACTIVATE);
      MsgWin.Visible := true;
      MsgWin.Animate ;
    end
    else
    begin
      MsgWin.Animate(true);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmartMessageBox.Show(Index: Integer);
begin
  if (Index >= 0) and (Index < FMessages.Count) then
    Show(FMessages.Items[Index]);
end;

//------------------------------------------------------------------------------

procedure TAdvSmartMessageBox.ShowAll;
var
  i: Integer;
begin
  for i := 0 to FMessages.Count - 1 do
  begin
    Show(FMessages.Items[i]);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmartMessageBox.Hide(Msg: TAdvSmartMessage);
var
  IsMyMsg: Boolean;
begin
  if not Assigned(Msg) or (csDestroying in ComponentState) then
    Exit;

  // Check for child msg
  IsMyMsg := False;
  if (Msg.Index >= 0) and (Msg.Index < FMessages.Count) then
    IsMyMsg := (FMessages.Items[Msg.Index] = Msg);
  if not IsMyMsg then
    Exit;

  if not Assigned(Msg) or not Assigned(Msg.MsgWindow) or not (Msg.MsgWindow.Visible) then
    Exit;

  Msg.MsgWindow.MaxShowTimerTimer(Msg.MsgWindow.FMaxShowTimer);
end;

//------------------------------------------------------------------------------

procedure TAdvSmartMessageBox.Hide(Index: Integer);
begin
  if (Index >= 0) and (Index < FMessages.Count) then
    Hide(FMessages.Items[Index]);
end;

//------------------------------------------------------------------------------

procedure TAdvSmartMessageBox.HideAll;
var
  i: Integer;
begin
  FProgClose := true;
  for i := 0 to FMessages.Count - 1 do
  begin
    if Assigned(FMessages.Items[i].FMsgWindow) then
    begin
      FMessages.Items[i].FMsgWindow.FMinShowTimer.Enabled := false;
      FMessages.Items[i].FMsgWindow.FMaxShowTimer.Enabled := false;
    end;
    Hide(FMessages.Items[i]);
  end;
end;

//------------------------------------------------------------------------------

function TAdvSmartMessageBox.GetMsgSize(Msg: TAdvSmartMessage; Canvas: TCanvas): TSize;
var
  r, hr: TRect;
  Anchor, Stripped, FocusAnchor: string;
  XSize, YSize, HyperLinks, MouseLink: integer;
begin
  R := Rect(0, 0, 1000, 4096);
  Canvas.Font.Assign(Msg.Font);

  HTMLDrawEx(Canvas, Msg.Text, R, FImages, 0, 0, -1, FHoverLink, 1, False, True, False, False, True, False{FHover}, True,
    1.0, clBlue{FURLColor}, clNone, clNone, clGray, Anchor, Stripped, FocusAnchor, XSize, YSize,
    HyperLinks, MouseLink, hr, FImageCache, FContainer, 0);
    
  Result.cx := XSize;
  Result.cy := YSize;
end;

//------------------------------------------------------------------------------

function TAdvSmartMessageBox.GetMessageVisible: Boolean;
begin
  Result := Assigned(FMsgForms) and (FMsgForms.Count > 0);
end;

//------------------------------------------------------------------------------

procedure TAdvSmartMessageBox.CheckKeyDown(Key: word);
begin

end;

//------------------------------------------------------------------------------

procedure TAdvSmartMessageBox.CheckMouseClicked;
begin

end;

//------------------------------------------------------------------------------

procedure TAdvSmartMessageBox.CheckMouseMoved(p: TPoint);
begin

end;

//------------------------------------------------------------------------------

procedure TAdvSmartMessageBox.InitializeMsg(Msg: TAdvSmartMessage);
begin
  if not Assigned(Msg) then
    Exit;
  with DefaultMessage do
  begin
    Msg.Font.Assign(Font);
    Msg.Color := Color;
    Msg.ColorTo := ColorTo;
    Msg.MirrorColor := MirrorColor;
    Msg.MirrorColorTo := MirrorColorTo;
    Msg.Gradient := Gradient;
    Msg.BorderColor := BorderColor;
    Msg.MarginX := MarginX;
    Msg.MarginY := MarginY;
    Msg.Rounded := Rounded;
    Msg.Shadow := Shadow;
    Msg.Opacity := Opacity;
    Msg.Text := Text;
    Msg.MaxDuration := MaxDuration;
    Msg.MinDuration := MinDuration;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmartMessageBox.SetImages(const Value: TImageList);
begin
  FImages := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvSmartMessageBox.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;
  
  if (AOperation = opRemove) and not (csDestroying in ComponentState) then
  begin
    if (AComponent = FImages) then
      FImages := nil;

    if (AComponent = FContainer) then
      FContainer := nil;
  end;
end;

//------------------------------------------------------------------------------

function TAdvSmartMessageBox.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

//------------------------------------------------------------------------------

procedure TAdvSmartMessageBox.SetVersion(const Value: string);
begin

end;

//------------------------------------------------------------------------------

function TAdvSmartMessageBox.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

//------------------------------------------------------------------------------

{ TAdvSmartMessageForm }

procedure TAdvSmartMessageForm.Animate(DoSetVisible: Bool = false);
var
  I, h: Integer;
  rgn: THandle;
begin
  if not Assigned(FSmartMsg) or FDisplayed then
    Exit;

  if (csDestroying in ComponentState) then
    Exit;

  // Round the corners of the form.
  if FSmartMsg.Rounded then
  begin
    rgn := CreateRoundRectRgn(0, 0, Width, Height, 10, 10);
    if rgn > 0 then
    begin
      try
        SetWindowRgn(Handle,rgn,true);
      finally
        DeleteObject(rgn);
      end;
    end;
  end;

  if not Assigned(FAdvSmartMessageBox) then
    Exit;

  if (FAdvSmartMessageBox.DisplayStyle = dsFadeInOut) then
  begin
    DoubleBuffered := true;
    I := 4;
    while (I <= FSmartMsg.Opacity) and Self.Visible do
    begin
      AlphaBlendValue := I;
      Application.ProcessMessages;
      Sleep(1);
      if FLockHiding or (DoHideOnMinDur and FLockHiding) then
      begin
        Break;
      end;
      Update();
      Inc(I, 4);
    end;

    if Self.Visible and not FLockHiding then
      AlphaBlendValue := FSmartMsg.Opacity;
  end
  else if (FAdvSmartMessageBox.DisplayStyle = dsRollInOut) then
  begin
    DoubleBuffered := true;
    h := Self.Height;
    Height := 3;

    if (DoSetVisible) then
    begin
      ShowWindow(Handle, SW_SHOWNOACTIVATE);
      Visible := true;
    end;

    AlphaBlendValue := FSmartMsg.Opacity;
    i := 4;
    while (i <= h + 5) do
    begin
      Height := i;
      if IsVista then
        Invalidate;
      Application.ProcessMessages;
      sleep(20);
      update;
      Inc(i, 5);
    end;
  end;

  // Round the corners of the form.
  if FSmartMsg.Rounded and Self.Visible then
  begin
    rgn := CreateRoundRectRgn(0, 0, Width, Height, 10, 10);
    if rgn > 0 then
    begin
      try
        SetWindowRgn(Handle,rgn,true);
      finally
        DeleteObject(rgn);
      end;
    end;
  end;

  FDisplayed := true;
end;

//------------------------------------------------------------------------------

procedure TAdvSmartMessageForm.Activate;
var
  frm: TForm;
begin

  inherited;

  frm := nil;
  if Assigned(Owner) and (Owner is TForm) then
    frm := TForm(Owner)
  else if Assigned(Application.MainForm) then
    frm := Application.MainForm;

  if Assigned(frm) then
    SendMessage(frm.Handle, WM_NCACTIVATE, integer(true), 0);

  Animate;
end;

//------------------------------------------------------------------------------

procedure TAdvSmartMessageForm.DoHide;
var
  I: Integer;
begin
  inherited;
  if (csDestroying in ComponentState) or (csDestroying in FAdvSmartMessageBox.ComponentState) then
    Exit;

  if (FAdvSmartMessageBox.DisplayStyle = dsFadeInOut) then
  begin
    I := AlphaBlendValue;
    while (I > 0) do
    begin
      if (csDestroying in ComponentState) then
        Exit;

      AlphaBlendValue := I;
      Application.ProcessMessages;
      Sleep(1);
      if Visible then
        Update();
      Dec(I, 3);
    end;
  end
  else
  begin
    i := Height - 5;
    while (i > 10) do
    begin
      if (csDestroying in ComponentState) then
        Exit;

      Height := i;
      Application.ProcessMessages;
      sleep(20);
      Update;
      Dec(i, 5);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmartMessageForm.DoShow;
begin
  inherited;

  if not Assigned(FSmartMsg) then
    Exit;

  FMinShowTimer := TTimer.Create(Self);
  FMinShowTimer.Enabled := False;
  FMinShowTimer.Interval := Max(FSmartMsg.MinDuration, 1);
  FMinShowTimer.OnTimer := MinShowTimerTimer;
  FMinShowTimer.Enabled := True;

  FMaxShowTimer := TTimer.Create(Self);
  FMaxShowTimer.Enabled := False;
  FMaxShowTimer.Interval := FSmartMsg.MaxDuration;
  FMaxShowTimer.OnTimer := MaxShowTimerTimer;
  FMaxShowTimer.Enabled := True;
end;

//------------------------------------------------------------------------------

procedure TAdvSmartMessageForm.HideEx;
begin
  if (csDestroying in ComponentState) or (csDestroying in FAdvSmartMessageBox.ComponentState) then
    Exit;

  Hide;

  if not FAdvSmartMessageBox.FProgClose then
    FAdvSmartMessageBox.DestroyMsgForm(FSmartMsg);
end;

//------------------------------------------------------------------------------

procedure TAdvSmartMessageForm.CreateParams(var Params: TCreateParams);
begin
  inherited;

  Params.ExStyle := WS_EX_NOACTIVATE;
  if Assigned(FSmartMsg) and (FSmartMsg.Shadow) and (Win32Platform = VER_PLATFORM_WIN32_NT) and
     ((Win32MajorVersion > 5) or ((Win32MajorVersion = 5) and (Win32MinorVersion >= 1))) then
    Params.WindowClass.Style := Params.WindowClass.Style or CS_DROPSHADOW;
end;

//------------------------------------------------------------------------------

procedure TAdvSmartMessageForm.CreateWnd;
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvSmartMessageForm.DoCreate;
begin
  inherited;
  FAdvSmartMessageBox := nil;
  FSmartMsg := nil;
  FOldXCoOrd := -200000;
  FOldYCoOrd := -200000;
  AlphaBlendValue := 5;
end;

//------------------------------------------------------------------------------

procedure TAdvSmartMessageForm.DoDestroy;
begin
  FMinShowTimer.Enabled := False;
  FMaxShowTimer.Enabled := False;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvSmartMessageForm.Loaded;
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvSmartMessageForm.MaxShowTimerTimer(Sender: TObject);
begin
  if not (csDestroying in FAdvSmartMessageBox.ComponentState) then
  begin
    FMinShowTimer.Enabled := False;
    FMaxShowTimer.Enabled := False;
    if not FLockHiding then
    begin
      FLockHiding := true;
      HideEx;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmartMessageForm.MinShowTimerTimer(Sender: TObject);
begin
  if (csDestroying in ComponentState) then
    Exit;

  if DoHideOnMinDur then
  begin
    if not FLockHiding then
      MaxShowTimerTimer(FMaxShowTimer);
  end
  else
  begin
    if (FMinShowTimer.Interval <> 100) then
      FMinShowTimer.Interval := 100;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmartMessageForm.Paint;
var
  R, TextR, hr: TRect;
  Anchor, Stripped, FocusAnchor: string;
  XSize, YSize, HyperLinks, MouseLink: integer;
begin
  inherited;

  if not Assigned(FSmartMsg) then
    Exit;

  R := ClientRect;
  TextR := Rect(R.Left + FSmartMsg.MarginX, R.Top + FSmartMsg.MarginY, R.Right - FSmartMsg.MarginX, R.Bottom - FSmartMsg.MarginY);

  if (FSmartMsg.ColorTo <> clNone) then
    DrawGradient(Canvas, FSmartMsg.Color, FSmartMsg.ColorTo, 80, R, FSmartMsg.Gradient = gdHorizontal)
  else
  begin
    Canvas.Brush.Color := FSmartMsg.Color;
    Canvas.Pen.Color := FSmartMsg.Color;
    Canvas.FillRect(R);
  end;

  if (FSmartMsg.BorderColor <> clNone) then
  begin
    Canvas.Pen.Color := FSmartMsg.BorderColor;
    Canvas.Brush.Style := bsClear;
    if FSmartMsg.Rounded then
      Canvas.RoundRect(R.Left, R.Top, R.Right - 1, R.Bottom - 1, 14, 14)
    else
      Canvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
  end;

  if (FSmartMsg.Text <> '') and Assigned(FAdvSmartMessageBox) then
  begin
    Canvas.Font.Assign(FSmartMsg.Font);

    HTMLDrawEx(Canvas, FSmartMsg.Text, TextR, FAdvSmartMessageBox.FImages, 0, 0, -1, FAdvSmartMessageBox.FHoverLink, 1, False, False, False, False, True, False{FHover}, True,
      1.0, clBlue{FURLColor}, clNone, clNone, clGray, Anchor, Stripped, FocusAnchor, XSize, YSize,
      HyperLinks, MouseLink, hr, FAdvSmartMessageBox.FImageCache, FAdvSmartMessageBox.FContainer, 0);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmartMessageForm.SetAdvSmartMessageBox(
  const Value: TAdvSmartMessageBox);
begin
  if (FAdvSmartMessageBox <> Value) then
  begin
    FAdvSmartMessageBox := Value;
  end;
end;

//------------------------------------------------------------------------------

function TAdvSmartMessageForm.GetMsgSize(Msg: TAdvSmartMessage): TSize;
(*
var
  r, hr: TRect;
  Anchor, Stripped, FocusAnchor: string;
  XSize, YSize, HyperLinks, MouseLink: integer;
*)  
begin
  Result.cx := 0;
  Result.cy := 0;

  if not Assigned(FAdvSmartMessageBox) then
    Exit;

  if not Assigned(Msg) then
    Exit;

  Result := FAdvSmartMessageBox.GetMsgSize(Msg, Canvas);  
    
  (*
  R := Rect(0, 0, 1000, 4096);

  Canvas.Font.Assign(Msg.Font);

  HTMLDrawEx(Canvas, Msg.Text, R, FAdvSmartMessageBox.FImages, 0, 0, -1, FAdvSmartMessageBox.FHoverLink, 1, False, True, False, False, True, False{FHover}, True,
    1.0, clBlue{FURLColor}, clNone, clNone, clGray, Anchor, Stripped, FocusAnchor, XSize, YSize,
    HyperLinks, MouseLink, hr, FAdvSmartMessageBox.FImageCache, FAdvSmartMessageBox.FContainer, 0);

  Result.cx := XSize;
  Result.cy := YSize;
  *)

end;

//------------------------------------------------------------------------------

procedure TAdvSmartMessageForm.UpdateWindow;
var
  sz: TSize;
begin
  if not Assigned(FSmartMsg) then
    Exit;

  sz := GetMsgSize(FSmartMsg);
  sz.cx := sz.cx + FSmartMsg.MarginX * 2;
  sz.cy := sz.cy + FSmartMsg.MarginY * 2;

  if (sz.cx < MinWin_Width) then
    sz.cx := MinWin_Width;
  if (sz.cy < MinWin_Height) then
    sz.cy := MinWin_Height;

  Self.Width := sz.cx;
  Self.Height := sz.cy;

  // Round the corners of the form.
  //Region := CreateRoundRectRgn(0, 0, Width, Height, 10, 10);
  //SetWindowRgn(Handle, Region, False);
end;

//------------------------------------------------------------------------------

procedure TAdvSmartMessageForm.SetSmartMsg(const Value: TAdvSmartMessage);
begin
  if (FSmartMsg <> Value) then
  begin
    FSmartMsg := Value;
    if Assigned(FSmartMsg) and not Assigned(FSmartMsg.MsgWindow) then
      FSmartMsg.MsgWindow := Self;
    Self.UpdateWindow;
  end;
end;

//------------------------------------------------------------------------------

function TAdvSmartMessageForm.GetParentWnd: HWnd;
var
  Last, P: HWnd;
begin
  P := GetParent((Owner as TWinControl).Handle);
  Last := P;
  while P <> 0 do
  begin
    Last := P;
    P := GetParent(P);
  end;
  Result := Last;
end;

//------------------------------------------------------------------------------

procedure TAdvSmartMessageForm.WMActivate(var Message: TWMActivate);
begin
  Message.Result := 0;
end;

procedure TAdvSmartMessageForm.WMDestroy(var Message: TMessage);
begin
  inherited;
end;

procedure TAdvSmartMessageForm.WMEraseBkGnd(var Message: TWMEraseBkGnd);
begin
  Message.Result := 1;
  //inherited;
end;

procedure TAdvSmartMessageForm.WMMouseActivate(var Message: TMessage);
begin
  Message.Result := MA_NOACTIVATE;
end;

//------------------------------------------------------------------------------

procedure TAdvSmartMessageForm.Click;
begin
  inherited;
  if Assigned(FAdvSmartMessageBox) and Assigned(SmartMsg) then
  begin
    if Assigned(FAdvSmartMessageBox.FOnClick) then
      FAdvSmartMessageBox.FOnClick(FAdvSmartMessageBox, SmartMsg.Index);
  end;
end;

//------------------------------------------------------------------------------

{ TDefaultSmartMessage }

procedure TDefaultSmartMessage.Assign(Source: TPersistent);
begin
  if (Source is TDefaultSmartMessage) then
  begin
    FColor := TDefaultSmartMessage(Source).Color;
    FColorTo := TDefaultSmartMessage(Source).ColorTo;
    FRounded := TDefaultSmartMessage(Source).Rounded;
    FShadow := TDefaultSmartMessage(Source).Shadow;
    FOpacity := TDefaultSmartMessage(Source).Opacity;
    FText := TDefaultSmartMessage(Source).Text;
    FMirrorColorTo := TDefaultSmartMessage(Source).MirrorColorTo;
    FMirrorColor := TDefaultSmartMessage(Source).MirrorColor;
    FMarginX := TDefaultSmartMessage(Source).MarginX;
    FMarginY := TDefaultSmartMessage(Source).MarginY;
    FBorderColor := TDefaultSmartMessage(Source).BorderColor;
    FFont.Assign(TDefaultSmartMessage(Source).Font);
    FGradient := TDefaultSmartMessage(Source).Gradient;
    FMinDuration := TDefaultSmartMessage(Source).FMinDuration;
    FMaxDuration := TDefaultSmartMessage(Source).FMaxDuration;
    FTag := TDefaultSmartMessage(Source).FTag;
  end
  else
    inherited;
end;

//------------------------------------------------------------------------------

constructor TDefaultSmartMessage.Create;
begin
  inherited;
  FFont := TFont.Create;
  FMarginX := 6;
  FMarginY := 6;
  FGradient := gdVertical;
  FOpacity := 230;
  FMaxDuration := 4000;
  FMinDuration := 2000;
  FRounded := True;
  FShadow := false;
  FColor := clWhite;
  FColorTo := clSilver;
  FBorderColor := clGray;
end;

//------------------------------------------------------------------------------

destructor TDefaultSmartMessage.Destroy;
begin
  FFont.Free;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TDefaultSmartMessage.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

//------------------------------------------------------------------------------

procedure TDefaultSmartMessage.SetBorderColor(const Value: TColor);
begin
  if (FBorderColor <> Value) then
  begin
    FBorderColor := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TDefaultSmartMessage.SetColor(const Value: TColor);
begin
  if (FColor <> Value) then
  begin
    FColor := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TDefaultSmartMessage.SetColorTo(const Value: TColor);
begin
  if (FColorTo <> Value) then
  begin
    FColorTo := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TDefaultSmartMessage.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TDefaultSmartMessage.SetMirrorColor(const Value: TColor);
begin
  if (FMirrorColor <> Value) then
  begin
    FMirrorColor := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TDefaultSmartMessage.SetMirrorColorTo(const Value: TColor);
begin
  if (FMirrorColorTo <> Value) then
  begin
    FMirrorColorTo := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TDefaultSmartMessage.SetOpacity(const Value: Byte);
begin
  if (FOpacity <> Value) then
  begin
    FOpacity := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TDefaultSmartMessage.SetRounded(const Value: Boolean);
begin
  if (FRounded <> Value) then
  begin
    FRounded := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TDefaultSmartMessage.SetShadow(const Value: Boolean);
begin
  if (FShadow <> Value) then
  begin
    FShadow := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TDefaultSmartMessage.SetText(const Value: string);
begin
  if (FText <> Value) then
  begin
    FText := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TDefaultSmartMessage.SetMarginX(const Value: Integer);
begin
  if (FMarginX <> Value) then
  begin
    FMarginX := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TDefaultSmartMessage.SetMarginY(const Value: Integer);
begin
  if (FMarginY <> Value) then
  begin
    FMarginY := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TDefaultSmartMessage.SetGradient(
  const Value: TGradientDirection);
begin
  if (FGradient <> Value) then
  begin
    FGradient := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TDefaultSmartMessage.SetMaxDuration(const Value: integer);
begin
  if (FMaxDuration <> Value) then
  begin
    FMaxDuration := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TDefaultSmartMessage.SetMinDuration(const Value: integer);
begin
  if (FMinDuration <> Value) then
  begin
    FMinDuration := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

{$IFDEF FREEWARE}
{$I TRIAL.INC}
{$ENDIF}


end.
