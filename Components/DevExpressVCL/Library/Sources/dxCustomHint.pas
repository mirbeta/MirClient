{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           Express Cross Platform Library classes                   }
{                                                                    }
{           Copyright (c) 2000-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSCROSSPLATFORMLIBRARY AND ALL   }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM       }
{   ONLY.                                                            }
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

unit dxCustomHint;

{$I cxVer.inc}

interface

uses
  Windows, Messages, Types, Classes, Forms, Controls{must be after Forms for D11}, Graphics, SysUtils,
  dxCore, dxCoreClasses, cxClasses, cxControls, cxContainer,
  cxGraphics, cxGeometry, cxLookAndFeels, cxLookAndFeelPainters;

type
  TcxCustomHintStyleController = class;

  { IcxHintWindowHelper }

  IcxHintWindowHelper = interface
    ['{BE228118-851E-4C8D-8A31-58E26AA4F88B}']
    procedure SetHintAreaBounds(const ABounds: TRect; AUseMousePos: Boolean);
  end;

  { IcxHintableObject }

  IcxHintableObject = interface
  ['{228FF1F5-6D0C-40F0-9F7B-8C71CE12CEC8}']
    function HasHintPoint(const P: TPoint): Boolean;
    function IsHintAtMousePos: Boolean;
    function UseHintHidePause: Boolean;
  end;

  { TcxBaseHintWindow }

  TcxHintAnimationStyle = (cxhaSlideFromLeft, cxhaSlideFromRight, cxhaSlideDownward, cxhaSlideUpward,
    cxhaSlideFromCenter, cxhaHide, cxhaActivate, cxhaFadeIn, cxhaAuto, cxhaNone);

  TcxBaseHintWindow = class(THintWindow, IcxHintWindowHelper)
  strict private
    FActivating: Boolean;
    FAnimationStyle: TcxHintAnimationStyle;
    FAnimationDelay: Integer;
    FBorderStyle: TBorderStyle;
    FcxCanvas: TcxCanvas;
    FHasAnimation: Boolean;
    FHintAreaBounds: TRect;
    FLastActive: Cardinal;
    FNeedEraseBackground: Boolean;
    FUseMousePos: Boolean;

    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    procedure WMWindowPosChanging(var Message: TWMWindowPosChanging); message WM_WINDOWPOSCHANGING;
  protected
    FStandardHint: Boolean;

    procedure AdjustActivateRect(var ARect: TRect); virtual;
    procedure CheckExecuteAnimation(const AHint: string);
    procedure CreateParams(var Params: TCreateParams); override;
    function GetAnimationStyle: TcxHintAnimationStyle;
    procedure DisableRegion; virtual;
    procedure EnableRegion; virtual;
    function HasWindowRegion: Boolean; virtual;
    procedure SetHintAreaBounds(const ABounds: TRect; AUseMousePos: Boolean); virtual;
    procedure Show; virtual;

    property HintAreaBounds: TRect read FHintAreaBounds;
    property UseMousePos: Boolean read FUseMousePos;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ActivateHint(ARect: TRect; const AHint: string); override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;

    property AnimationStyle: TcxHintAnimationStyle read FAnimationStyle write FAnimationStyle;
    property AnimationDelay: Integer read FAnimationDelay write FAnimationDelay;
    property BorderStyle: TBorderStyle read FBorderStyle write FBorderStyle;
    property cxCanvas: TcxCanvas read FcxCanvas;
    property NeedEraseBackground: Boolean read FNeedEraseBackground write FNeedEraseBackground;
  end;

  { TcxCustomHintWindow }

  TcxCustomHintWindowClass = class of TcxCustomHintWindow;
  TcxCustomHintWindow = class(TcxBaseHintWindow)
  strict private
    FCaption: string;

    procedure SetStandardHint(const Value: Boolean);
  protected
    function GetPainter: TcxCustomLookAndFeelPainter;
    procedure ShowHint(X, Y: Integer; ACaption, AHint: string; AMaxWidth: Integer = 0); virtual;
    property StandardHint: Boolean read FStandardHint write SetStandardHint;
  public
    function UseRightToLeftAlignment: Boolean; override;
    property Caption: string read FCaption write FCaption;
  end;

  { TcxCustomHintStyle }

  TcxHintStyleClass = class of TcxCustomHintStyle;
  TcxCustomHintStyle = class(TcxInterfacedPersistent)
  strict private
    FOnChanged: TNotifyEvent;
  protected
    procedure ControllerChangedNotification(AStyleController: TcxCustomHintStyleController); virtual;
    procedure ControllerFreeNotification(AHintStyleController: TcxCustomHintStyleController); virtual;
    procedure DoShowHint(var AHintStr: string; var ACanShow: Boolean; var AHintInfo: THintInfo); virtual;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  public
    constructor Create(AComponent: TComponent); reintroduce; virtual;
    procedure ComponentRemoved(AComponent: TComponent); virtual;
    function GetHintWindowClass: TcxCustomHintWindowClass; virtual; abstract;
  end;

  TcxHintStyleChangedEvent = procedure (Sender: TObject; AStyle: TcxCustomHintStyle) of object;
  TcxShowHintEvent = procedure(Sender: TObject; var HintStr: string; var CanShow: Boolean; var HintInfo: THintInfo) of object;
  TcxShowHintExEvent = procedure(Sender: TObject; var Caption, HintStr: string; var CanShow: Boolean; var HintInfo: THintInfo) of object;

  { TcxCustomHintStyleController }

  TcxCustomHintStyleController = class(TcxCustomComponent, IcxLookAndFeelContainer, IdxSkinSupport)
  private
    FGlobal: Boolean;
    FHintShortPause: Integer;
    FHintPause: Integer;
    FHintHidePause: Integer;
    FHintStyleClass: TcxHintStyleClass;
    FHintWindow: TcxCustomHintWindow;
    FListeners: TList;
    FLookAndFeel: TcxLookAndFeel;
    FUpdateCount: Integer;
    FUseHintControlLookAndFeel: Boolean;

    FOnHintStyleChanged: TcxHintStyleChangedEvent;
    FOnShowHint: TcxShowHintEvent;
    FOnShowHintEx: TcxShowHintExEvent;

    procedure DoShowHint(var AHintStr: string; var ACanShow: Boolean; var AHintInfo: THintInfo);
    procedure DoShowHintEx(var AHintStr, AHintCaption: string; var ACanShow: Boolean; var AHintInfo: THintInfo);
    function GetHintStyleClassName: string;
    function GetLookAndFeelPainter: TcxCustomLookAndFeelPainter;
    procedure HintStyleChanged(Sender: TObject);
    procedure SetGlobal(Value: Boolean);
    procedure SetHintStyle(Value: TcxCustomHintStyle);
    procedure SetHintStyleClass(const Value: TcxHintStyleClass);
    procedure SetHintStyleClassName(const Value: string);
    procedure SetLookAndFeel(const Value: TcxLookAndFeel);
    procedure SetHintShortPause(Value: Integer);
    procedure SetHintPause(Value: Integer);
    procedure SetHintHidePause(Value: Integer);
    procedure SetApplicationHintProperties;
  protected
    FHintStyle: TcxCustomHintStyle;

    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    function GetHintStyleClass: TcxHintStyleClass; virtual;
    function GetHintWindowClass: TcxCustomHintWindowClass; virtual;
//    procedure AddListener(AListener: TcxCustomHintStyle); virtual;
//    procedure RemoveListener(AListener: TcxCustomHintStyle); virtual;
    procedure CreateHintStyle;
    procedure DestroyHintStyle;

    procedure BeginUpdate; virtual;
    procedure EndUpdate; virtual;
    procedure Changed;
    procedure DoHintStyleChanged(AStyle: TcxCustomHintStyle); virtual;

    property Global: Boolean read FGlobal write SetGlobal default True;
    property HintHidePause: Integer read FHintHidePause write SetHintHidePause default 2500;
    property HintPause: Integer read FHintPause write SetHintPause default 500;
    property HintShortPause: Integer read FHintShortPause write SetHintShortPause default 50;
//    property HintStyle: TcxCustomHintStyle read FHintStyle write SetHintStyle;
    property HintStyleClass: TcxHintStyleClass read GetHintStyleClass write SetHintStyleClass;
    property HintStyleClassName: string read GetHintStyleClassName write SetHintStyleClassName;
    property Listeners: TList read FListeners;
    property OnHintStyleChanged: TcxHintStyleChangedEvent read FOnHintStyleChanged write FOnHintStyleChanged;
    property OnShowHint: TcxShowHintEvent read FOnShowHint write FOnShowHint;
    property OnShowHintEx: TcxShowHintExEvent read FOnShowHintEx write FOnShowHintEx;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    procedure AddListener(AListener: TcxCustomHintStyle); virtual;  //  to do
    procedure RemoveListener(AListener: TcxCustomHintStyle); virtual; //  to do

    // IcxLookAndFeelContainer
    function IcxLookAndFeelContainer.GetLookAndFeel = GetLookAndFeelValue;
    function GetLookAndFeelValue: TcxLookAndFeel;

    procedure ShowHint(X, Y: Integer; ACaption, AHint: string; AMaxWidth: Integer = 0);
    procedure HideHint;

    property HintStyle: TcxCustomHintStyle read FHintStyle write SetHintStyle;  //  to do
    property HintWindow: TcxCustomHintWindow read FHintWindow;
    property LookAndFeel: TcxLookAndFeel read FLookAndFeel write SetLookAndFeel;
    property LookAndFeelPainter: TcxCustomLookAndFeelPainter read GetLookAndFeelPainter;
    property UseHintControlLookAndFeel: Boolean read FUseHintControlLookAndFeel write FUseHintControlLookAndFeel default False;
  end;

  { TdxCustomHintViewInfoHelper }

  TdxCustomHintViewInfoHelper = class
  strict private
    FBoundsRect: TRect;
    FUseRightToLeftAlignment: Boolean;
    FUseRightToLeftReading: Boolean;
  protected
    function GetScaleFactor: TdxScaleFactor; virtual;
  public
    procedure Calculate(ACanvas: TCanvas); virtual; abstract;
    procedure Paint(ACanvas: TCanvas); virtual; abstract;

    property BoundsRect: TRect read FBoundsRect write FBoundsRect;
    property ScaleFactor: TdxScaleFactor read GetScaleFactor;
    property UseRightToLeftAlignment: Boolean read FUseRightToLeftAlignment write FUseRightToLeftAlignment;
    property UseRightToLeftReading: Boolean read FUseRightToLeftReading write FUseRightToLeftReading;
  end;

  { TdxCustomHintViewInfo }

  TdxCustomHintViewInfo = class
  strict private
    FUseRightToLeftAlignment: Boolean;
    FUseRightToLeftReading: Boolean;
    function GetBoundsRect: TRect;
  protected
    FHelper: TdxCustomHintViewInfoHelper;

    procedure CreateHelper; virtual; abstract;
    procedure DestroyHelper; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Calculate(ACanvas: TCanvas); virtual;
    procedure Paint(ACanvas: TCanvas); virtual;
    property BoundsRect: TRect read GetBoundsRect;
    property UseRightToLeftAlignment: Boolean read FUseRightToLeftAlignment write FUseRightToLeftAlignment;
    property UseRightToLeftReading: Boolean read FUseRightToLeftReading write FUseRightToLeftReading;
  end;

  { TdxHintViewInfoHelper }

  TdxHintViewInfoHelper = class(TdxCustomHintViewInfoHelper)
  strict private
    FPos: TPoint;
    FScaleFactor: TdxScaleFactor;
    FText: string;

    function CalculateMinSize(ACanvas: TCanvas): TSize;
  protected
    procedure CorrectMinSize(var ASize: TSize); virtual;
    function GetScaleFactor: TdxScaleFactor; override;
    function GetTextRect: TRect; virtual;
    procedure DrawText(ACanvas: TCanvas); virtual;
    procedure PrepareCanvasFont(ACanvas: TCanvas); virtual;
    procedure SetTextColor(ACanvas: TCanvas); virtual;

    property Text: string read FText write FText;
    property Pos: TPoint read FPos write FPos;
  public
    constructor Create(const AHint, AShortCut: string; const ACursorPos: TPoint);
    destructor Destroy; override;
    procedure Calculate(ACanvas: TCanvas); override;
    procedure Paint(ACanvas: TCanvas); override;
  end;

  { TcxCustomHintHelper }

  TcxCustomHintHelper = class(TcxIUnknownObject,
    IcxMouseTrackingCaller,
    IcxMouseTrackingCaller2)
  private
    FHintableObject: TObject;
    FHintAreaBounds: TRect;
    FHintCheckerTimer: TcxTimer;
    FHintHideTimer: TcxTimer;
    FHintHidePause: Integer;
    FLastHintableObject: TObject;
    FLastHintMousePos: TPoint;
    FLastHintText: string;
    FHintTextRect: TRect;
    FHintWindow: THintWindow;

    procedure HintCheckerTimerHandler(Sender: TObject);
    procedure HintHideTimerHandler(Sender: TObject);
    procedure RestartHintHideTimer;
    procedure RecreateHintWindow;
  protected
    function IsMouseButtonPressed: Boolean;

    // IcxMouseTrackingCaller2 }
    procedure MouseLeave; virtual;
    function PtInCaller(const P: TPoint): Boolean; virtual;

    function CanShowHint: Boolean; virtual;
    procedure CorrectHintWindowRect(var ARect: TRect); virtual;
    function GetHintWinControl: TWinControl; virtual;
    function GetHintControlBounds: TRect; virtual;
    function GetHintHidePause: Integer; virtual;
    function GetHintWindowClass: THintWindowClass; virtual;
    function GetHintWindowRect(const AHintAreaBounds, ATextRect: TRect; const AText: string; AIsHintMultiLine: Boolean): TRect; virtual;
    function GetOwnerWinControl: TWinControl; virtual;
    function IsHintAtMousePos: Boolean; virtual;
    function IsSuppressHintOnMouseDown: Boolean; virtual;
    procedure StartHintCheckerTimers;
    procedure StopHintCheckerTimers;
    function UseHintHidePause: Boolean; virtual;
  public
    destructor Destroy; override;
    procedure CancelHint;
    procedure HideHint;
    procedure MouseDown; virtual;
    procedure ResetLastHintElement;
    procedure ShowHint(const AHintAreaBounds, ATextRect: TRect; const AText: string;
      AIsHintMultiLine: Boolean; AHintObject: TObject; AFont: TFont = nil);

    property HintableObject: TObject read FHintableObject;
    property HintWindow: THintWindow read FHintWindow;
  end;

  { TcxControlHintHelper }

  TcxControlHintHelper = class(TcxCustomHintHelper)
  protected
    function GetHintControl: TcxControl; virtual;
    function GetHintControlBounds: TRect; override;
    function GetHintWinControl: TWinControl; override;
    function GetOwnerControl: TcxControl; virtual; abstract;
    function GetOwnerWinControl: TWinControl; override;
  end;

function cxGetHintStyleController: TcxCustomHintStyleController;
function cxGetHintStyle: TcxCustomHintStyle;
function cxGetHintWindowClass: THintWindowClass;
function cxGetHintedControl: TControl;
procedure cxRemoveHintedControl;
function cxRegisteredHintStyles: TcxRegisteredClasses;

procedure cxActivateHint(AHintWindow: THintWindow; const ARect: TRect; const AHint: string);
procedure cxActivateHintData(AHintWindow: THintWindow; const ARect: TRect; const AHint: string; AData: Pointer);
function cxProcessControlHintInfo(out AHintInfo: THintInfo;
  var AHintWindow: THintWindow; AControl: TControl; const AHint: string;
  var AHintWinRect: TRect; AData: Pointer = nil; AHintHidePause: Integer = 0): Boolean;

implementation

uses
  Math, dxDPIAwareUtils;

type
  TCustomFormAccess = class(TCustomForm);

procedure cxRecreateHintWindow(var AHintWindow: THintWindow;
  AClass: THintWindowClass; AColor: TColor);
begin
  AHintWindow.Free;
  AHintWindow := AClass.Create(nil);
  AHintWindow.Color := AColor;
end;

procedure ValidateHintWindowParent(AHintWindow: THintWindow);
begin
  if AHintWindow.ParentWindow <> Application.Handle then
  begin
    AHintWindow.Visible := False;
    AHintWindow.ParentWindow := Application.Handle;
  end;
end;

function IsMonitorValid(const P: TPoint): Boolean;
var
  AMonitor: TMonitor;
  AForm: TCustomForm;
  AControl: TWinControl;
begin
  AMonitor := Screen.MonitorFromPoint(P);
  if AMonitor = nil then
  begin
    AControl := FindVCLWindow(P);
    if AControl <> nil then
      AForm := GetParentForm(AControl)
    else
      AForm := Application.MainForm;
    if AForm <> nil then
      AMonitor := AForm.Monitor;
  end;
  Result := AMonitor <> nil;
end;

procedure cxActivateHint(AHintWindow: THintWindow; const ARect: TRect; const AHint: string);
begin
  ValidateHintWindowParent(AHintWindow);
  AHintWindow.ActivateHint(ARect, AHint);
end;

procedure cxActivateHintData(AHintWindow: THintWindow; const ARect: TRect; const AHint: string; AData: Pointer);
begin
  ValidateHintWindowParent(AHintWindow);
  AHintWindow.ActivateHintData(ARect, AHint, AData);
end;

function cxProcessControlHintInfo(out AHintInfo: THintInfo;
  var AHintWindow: THintWindow; AControl: TControl; const AHint: string;
  var AHintWinRect: TRect; AData: Pointer = nil; AHintHidePause: Integer = 0): Boolean;

  procedure ValidateHintWindow(AHintClass: THintWindowClass; AColor: TColor);
  begin
    if AHintClass = nil then AHintClass := HintWindowClass;
    if (AHintWindow = nil) or (AHintWindow.ClassType <> AHintClass) then
      cxRecreateHintWindow(AHintWindow, AHintClass, AColor);
  end;

  function MultiLineWidth(const Value: string): Integer;
  var
    W: Integer;
    P, Start: PChar;
    S: string;
  begin
    Result := 0;
    P := Pointer(Value);
    if P <> nil then
      while P^ <> #0 do
      begin
        Start := P;
        while not dxCharInSet(P^, [#0, #10, #13]) do
          P := StrNextChar(P);
        SetString(S, Start, P - Start);
        W := AHintWindow.Canvas.TextWidth(S);
        if W > Result then
          Result := W;
        if P^ = #13 then Inc(P);
        if P^ = #10 then Inc(P);
      end;
  end;

  function IsEqual(AHintInfo1, AHintInfo2: THintInfo): Boolean;
  begin
    Result := (AHintInfo1.HintControl = AHintInfo2.HintControl) and
      (AHintInfo1.HintWindowClass = AHintInfo2.HintWindowClass) and
      cxPointIsEqual(AHintInfo1.HintPos, AHintInfo2.HintPos) and
      (AHintInfo1.HintMaxWidth = AHintInfo2.HintMaxWidth) and
      (AHintInfo1.HintColor = AHintInfo2.HintColor) and
      cxRectIsEqual(AHintInfo1.CursorRect, AHintInfo2.CursorRect) and
      cxPointIsEqual(AHintInfo1.CursorPos, AHintInfo2.CursorPos) and
      (AHintInfo1.ReshowTimeout = AHintInfo2.ReshowTimeout) and
      (AHintInfo1.HideTimeout = AHintInfo2.HideTimeout) and
      (AHintInfo1.HintStr = AHintInfo2.HintStr) and
      (AHintInfo1.HintData = AHintInfo2.HintData);
  end;

  function IsHintStyleControllerExists: Boolean;
  begin
    Result := cxGetHintStyleController <> nil;
  end;

var
  AClientOrigin, AParentOrigin: TPoint;
  APrevHintInfo: THintInfo;
begin
  Result := True;
  AHintInfo.HintControl := AControl;
  if AHintWindow = nil then
    AHintInfo.HintWindowClass := nil
  else
    AHintInfo.HintWindowClass := THintWindowClass(AHintWindow.ClassType);
  AHintInfo.HintPos := AHintWinRect.TopLeft;
  AHintInfo.HintMaxWidth := Screen.Width;
  AHintInfo.HintColor := Application.HintColor;
  AHintInfo.ReshowTimeout := 0;
  if AHintHidePause <= 0 then
    AHintInfo.HideTimeout := Application.HintHidePause
  else
    AHintInfo.HideTimeout := AHintHidePause;
  AHintInfo.HintStr  := AHint;
  AHintInfo.HintData := AData;
  if AControl <> nil then
  begin
    AHintInfo.CursorPos := AControl.ScreenToClient(GetMouseCursorPos);
    AHintInfo.CursorRect := AControl.BoundsRect;
    AClientOrigin := AControl.ClientOrigin;
    AParentOrigin.X := 0;
    AParentOrigin.Y := 0;
    if AControl.Parent <> nil then
      AParentOrigin := AControl.Parent.ClientOrigin
    else
      if (AControl is TWinControl) and (TWinControl(AControl).ParentWindow <> 0) then
        Windows.ClientToScreen(TWinControl(AControl).ParentWindow, AParentOrigin);
    OffsetRect(AHintInfo.CursorRect, AParentOrigin.X - AClientOrigin.X, AParentOrigin.Y - AClientOrigin.Y);
  end;
  if Assigned(Application.OnShowHint) then
  begin
    APrevHintInfo := AHintInfo;
    Application.OnShowHint(AHintInfo.HintStr, Result, AHintInfo);
    if Result and (AHintInfo.HintStr <> '') and
      (IsHintStyleControllerExists or not IsEqual(AHintInfo, APrevHintInfo)) then
    begin
      ValidateHintWindow(AHintInfo.HintWindowClass, AHintInfo.HintColor);
      if AControl <> nil then
        AHintWindow.BiDiMode := AControl.BiDiMode;
      with AHintInfo do
      begin
        AHintWinRect := AHintWindow.CalcHintRect(HintMaxWidth, HintStr, HintData);
        OffsetRect(AHintWinRect, HintPos.X, HintPos.Y);
      end;
    end;
  end;
end;

var
  FRegisteredHintStyles: TcxRegisteredClasses;

function cxRegisteredHintStyles: TcxRegisteredClasses;
begin
  if FRegisteredHintStyles = nil then
    FRegisteredHintStyles := TcxRegisteredClasses.Create;
  Result := FRegisteredHintStyles;
end;

type
  { TcxHintController }

  TcxHintController = class(TComponent)
  private
    FHintedControl: TControl;

    FControllerList: TList;
    FSavedApplicationOnShowHint: TShowHintEvent;
    FSavedHintWindowClass: THintWindowClass;

    function GetCurrentHintWindowClass: THintWindowClass;
    procedure SetHintedControl(Value: TControl);
    procedure ResetGlobal;

    function FindHintStyleController: TcxCustomHintStyleController;
    function FindHintedControl: TControl;

    procedure ShowHintHandler(var HintStr: string; var CanShow: Boolean;
      var HintInfo: THintInfo);
    procedure DoApplicationShowHint(var HintStr: string; var CanShow: Boolean;
      var HintInfo: THintInfo);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    procedure RegisterHintStyleController(AController: TcxCustomHintStyleController);
    procedure UnRegisterHintStyleController(AController: TcxCustomHintStyleController);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property HintedControl: TControl read FHintedControl write SetHintedControl;
  end;

var
  FdxHintController: TcxHintController;

function cxGetHintStyleController: TcxCustomHintStyleController;
begin
  Result := FdxHintController.FindHintStyleController;
end;

function cxGetHintStyle: TcxCustomHintStyle;
var
  AHintStyleController: TcxCustomHintStyleController;
begin
  Result := nil;
  AHintStyleController := cxGetHintStyleController;
  if AHintStyleController <> nil then
    Result := AHintStyleController.FHintStyle;
end;

function cxGetHintWindowClass: THintWindowClass;
var
  AHintStyleController: TcxCustomHintStyleController;
begin
  AHintStyleController := cxGetHintStyleController;
  if AHintStyleController <> nil then
    Result := AHintStyleController.GetHintWindowClass
  else
    Result := HintWindowClass;
end;

function cxGetHintedControl: TControl;
begin
  Result := FdxHintController.FindHintedControl;
end;

procedure cxRemoveHintedControl;
begin
  FdxHintController.HintedControl := nil;
end;

constructor TcxHintController.Create(AOwner: TComponent);
begin
  inherited;
  FControllerList := TList.Create;
end;

destructor TcxHintController.Destroy;
begin
//  if FControllerList.Count <> 0 then
//    raise EcxEditError.Create('HintStyleControllerList.Count <> 0');
  FreeAndNil(FControllerList);
  HintedControl := nil;
  inherited Destroy;
end;

procedure TcxHintController.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FHintedControl) then
    FHintedControl := nil;
end;

procedure TcxHintController.RegisterHintStyleController(AController: TcxCustomHintStyleController);

  procedure InitGlobalParameters;
  begin
    FSavedApplicationOnShowHint := Application.OnShowHint;
    Application.OnShowHint := ShowHintHandler;
    FSavedHintWindowClass := HintWindowClass;
  end;

begin
  if (AController <> nil) and not (csDesigning in AController.ComponentState) then
  begin
    if FControllerList.Count = 0 then
      InitGlobalParameters;
    HintWindowClass := AController.GetHintWindowClass;
    FControllerList.Add(AController);
  end;
end;

procedure TcxHintController.UnRegisterHintStyleController(AController: TcxCustomHintStyleController);

  procedure UnInitGlobalParameters;
  begin
    Application.OnShowHint := FSavedApplicationOnShowHint;
    FSavedApplicationOnShowHint := nil;
  end;

var
  AIsHintWindowClassNotChanged: Boolean;
begin
  if (AController <> nil) and not (csDesigning in AController.ComponentState) then
  begin
    AIsHintWindowClassNotChanged := HintWindowClass = GetCurrentHintWindowClass;
    if (FControllerList.Remove(AController) > -1) and AIsHintWindowClassNotChanged then
      HintWindowClass := GetCurrentHintWindowClass;
    if FControllerList.Count = 0 then
      UnInitGlobalParameters;
  end;
end;

function TcxHintController.GetCurrentHintWindowClass: THintWindowClass;
begin
  if FControllerList.Count > 0 then
    Result := TcxCustomHintStyleController(FControllerList.Last).GetHintWindowClass
  else
    Result := FSavedHintWindowClass;
end;

procedure TcxHintController.SetHintedControl(Value: TControl);
begin
  if Value <> FHintedControl then
  begin
    if FHintedControl <> nil then
      FHintedControl.RemoveFreeNotification(Self);
    FHintedControl := Value;
    if FHintedControl <> nil then
      FHintedControl.FreeNotification(Self);
  end;
end;

procedure TcxHintController.ResetGlobal;
var
  I: Integer;
begin
  for I := 0 to FControllerList.Count - 1 do
    TcxCustomHintStyleController(FControllerList[I]).FGlobal := False;
end;

function TcxHintController.FindHintStyleController: TcxCustomHintStyleController;

  function FindHintControllerOnParent(AParent: TWinControl): TcxCustomHintStyleController;
  begin
    Result := TcxCustomHintStyleController(cxFindComponent(AParent, TcxCustomHintStyleController));
  end;

  function FindHintControllerOnParents: TcxCustomHintStyleController;
  var
    AHintedControl: TControl;
    AParent: TWinControl;
    AManagedObject: IdxManagedObject;
    AManager: IdxManager;
  begin
    Result := nil;
    AHintedControl := FindHintedControl;
    if AHintedControl <> nil then
    begin
      AParent := AHintedControl.Parent;
      while (AParent <> nil) and (Result = nil) do
      begin
        Result := FindHintControllerOnParent(AParent);
        AParent := AParent.Parent;
      end;
      if (Result = nil) and Supports(AHintedControl, IdxManagedObject, AManagedObject) then
      begin
        AManager := AManagedObject.GetManager;
        if AManager <> nil then
        begin
          AParent := AManager.GetParentForm;
          if AParent <> nil then
            Result := FindHintControllerOnParent(AParent);
        end;
      end;
    end;
  end;

var
  AController: TcxCustomHintStyleController;
  I: Integer;
begin
  Result := FindHintControllerOnParents;
  if Result = nil then
    for I := FControllerList.Count - 1 downto 0 do
    begin
      AController := TcxCustomHintStyleController(FControllerList[I]);
      if AController.Global then
      begin
        Result := AController;
        Break;
      end;
    end;
end;

function TcxHintController.FindHintedControl: TControl;
var
  AWnd: HWND;
begin
  if HintedControl <> nil then
    Result := HintedControl
  else
  begin
    AWnd := GetCapture;
    if AWnd = 0 then
      AWnd := WindowFromPoint(GetMouseCursorPos);
    Result := cxFindVCLControl(AWnd);
  end;
end;

procedure TcxHintController.ShowHintHandler(var HintStr: string; var CanShow: Boolean;
  var HintInfo: THintInfo);
var
  AController: TcxCustomHintStyleController;
begin
  HintedControl := HintInfo.HintControl;
  AController := FindHintStyleController;
  if AController <> nil then
    AController.DoShowHint(HintStr, CanShow, HintInfo);
  DoApplicationShowHint(HintStr, CanShow, HintInfo);
  if not CanShow or (HintStr = '') then
    cxRemoveHintedControl;
end;

procedure TcxHintController.DoApplicationShowHint(var HintStr: string; var CanShow: Boolean;
  var HintInfo: THintInfo);
begin
  if Assigned(FSavedApplicationOnShowHint) then
    FSavedApplicationOnShowHint(HintStr, CanShow, HintInfo);
end;

{ TcxBaseHintWindow }

constructor TcxBaseHintWindow.Create(AOwner: TComponent);
begin
  inherited;
  FAnimationStyle := cxhaAuto;
  FAnimationDelay := 100;
  FcxCanvas := TcxCanvas.Create(Canvas);
end;

destructor TcxBaseHintWindow.Destroy;
begin
  FreeAndNil(FcxCanvas);
  inherited;
end;

procedure TcxBaseHintWindow.ActivateHint(ARect: TRect; const AHint: string);
begin
  if FStandardHint then
    inherited
  else
  begin
    FActivating := True;
    try
      Caption := AHint;
      AdjustActivateRect(ARect);
      UpdateBoundsRect(ARect);
      SetWindowPos(Handle, HWND_TOPMOST, ARect.Left, ARect.Top, Width, Height,
        SWP_NOACTIVATE or SWP_NOOWNERZORDER);
      CheckExecuteAnimation(AHint);
      Show;
      FHintAreaBounds := cxEmptyRect;
    finally
      FLastActive := GetTickCount;
      FActivating := False;
    end;
  end;
end;

procedure TcxBaseHintWindow.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);

  procedure CorrectPosition;
  var
    AWorkArea: TRect;
  begin
    AWorkArea := GetDesktopWorkArea(Point(ALeft, ATop));
    ALeft := Max(Min(ALeft, AWorkArea.Right - AWidth), AWorkArea.Left);
    ATop := Max(Min(ATop, AWorkArea.Bottom - AHeight), AWorkArea.Top);
  end;

begin
  CorrectPosition;
  inherited;
end;

procedure TcxBaseHintWindow.AdjustActivateRect(var ARect: TRect);
begin
  MakeVisibleOnDesktop(ARect, GetMouseCursorPos);
end;

procedure TcxBaseHintWindow.CheckExecuteAnimation(const AHint: string);

  procedure ExecuteAnimation(AAnimationStyle: TcxHintAnimationStyle);
  const
    AAnimationStyleMap: array[TcxHintAnimationStyle] of Integer = (AW_HOR_POSITIVE, AW_HOR_NEGATIVE,
      AW_VER_POSITIVE, AW_VER_NEGATIVE, AW_CENTER, AW_HIDE, AW_ACTIVATE,
      AW_BLEND, AW_ACTIVATE, AW_ACTIVATE);
  begin
    {MSDN.AnimateWindow: Function fails, if the window uses the window region.
     Windows XP: This does not cause the function to fail. }
    if not IsWinXPOrLater then
      DisableRegion;
    AnimateWindowProc(Handle, AnimationDelay, AAnimationStyleMap[AAnimationStyle] or AW_SLIDE);
  end;

var
  AAnimationStyle: TcxHintAnimationStyle;
begin
  FHasAnimation := (GetTickCount - FLastActive > 250) and (Length(AHint) < 100) and
    Assigned(AnimateWindowProc);
  if FHasAnimation then
  begin
    AAnimationStyle := GetAnimationStyle;
    FHasAnimation := AAnimationStyle <> cxhaNone;
    if FHasAnimation then
    begin
      if IsWinXPOrLater and HasWindowRegion then
        EnableRegion
      else
        DisableRegion;
      ExecuteAnimation(AAnimationStyle);
      if not IsWinXPOrLater and HasWindowRegion then
        EnableRegion;
    end;
  end;
  if not HasWindowRegion then
    DisableRegion
  else
    if not FHasAnimation and IsWinXPOrLater then
      EnableRegion;
end;

procedure TcxBaseHintWindow.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  if not FStandardHint then
    with Params do
    begin
      if BorderStyle = bsNone then
        Style := Style and not WS_BORDER;
      ExStyle := ExStyle or WS_EX_TOPMOST;
    end;
end;

function TcxBaseHintWindow.GetAnimationStyle: TcxHintAnimationStyle;
var
  AAnimationEnabled, AFadeEffectAnimation: BOOL;
begin
  Result := FAnimationStyle;
  if Result = cxhaNone then
    Exit;
  dxSystemInfo.GetParameter(SPI_GETTOOLTIPANIMATION, AAnimationEnabled);
  if not AAnimationEnabled then
    Result := cxhaNone
  else
    if Result = cxhaAuto then
    begin
      dxSystemInfo.GetParameter(SPI_GETTOOLTIPFADE, AFadeEffectAnimation);
      if AFadeEffectAnimation then
        Result := cxhaFadeIn
      else
        Result := cxhaSlideDownward;
    end;
end;

procedure TcxBaseHintWindow.DisableRegion;
begin
  SetWindowRegion(Handle, 0);
end;

procedure TcxBaseHintWindow.EnableRegion;
begin
// do nothing
end;

function TcxBaseHintWindow.HasWindowRegion: Boolean;
begin
  Result := False;
end;

procedure TcxBaseHintWindow.SetHintAreaBounds(const ABounds: TRect; AUseMousePos: Boolean);
begin
  FHintAreaBounds := ABounds;
  FUseMousePos := AUseMousePos;
end;

procedure TcxBaseHintWindow.Show;
begin
  DoubleBuffered := HasWindowRegion;
  ShowWindow(Handle, SW_SHOWNOACTIVATE);
  if HasWindowRegion and FHasAnimation then
    cxRedrawWindow(Handle, RDW_INVALIDATE or RDW_FRAME or RDW_ERASE or RDW_UPDATENOW)
  else
    Invalidate;
end;

procedure TcxBaseHintWindow.CMTextChanged(var Message: TMessage);
begin
  if FActivating then Exit;
  inherited;
end;

procedure TcxBaseHintWindow.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
  if NeedEraseBackground or FStandardHint then
    inherited
  else
    Message.Result := 1;
end;

procedure TcxBaseHintWindow.WMWindowPosChanging(
  var Message: TWMWindowPosChanging);
var
  AWindowPos: PWindowPos;
begin
  AWindowPos := Message.WindowPos;
  if not FStandardHint and not FActivating and ((AWindowPos.flags and SWP_SHOWWINDOW) <> 0) then
    AWindowPos.flags := AWindowPos.flags and not SWP_SHOWWINDOW;
  inherited;
end;

{ TcxCustomHintWindow }

function TcxCustomHintWindow.UseRightToLeftAlignment: Boolean;
begin
  Result := StandardHint and inherited UseRightToLeftAlignment;
end;

function TcxCustomHintWindow.GetPainter: TcxCustomLookAndFeelPainter;

  function GetLookAndFeelContainer(out ALookAndFeelContainer: IcxLookAndFeelContainer): Boolean;
  var
    AManagedObject: IdxManagedObject;
    AHintedControl: TControl;
  begin
    AHintedControl := cxGetHintedControl;
    Result := Supports(AHintedControl, IcxLookAndFeelContainer, ALookAndFeelContainer) or
      Supports(AHintedControl, IdxManagedObject, AManagedObject) and
      Supports(AManagedObject.GetManager, IcxLookAndFeelContainer, ALookAndFeelContainer);
  end;

var
  AHintStyleController: TcxCustomHintStyleController;
  ALookAndFeelContainer: IcxLookAndFeelContainer;
begin
  AHintStyleController := cxGetHintStyleController;
  if AHintStyleController.UseHintControlLookAndFeel and
    GetLookAndFeelContainer(ALookAndFeelContainer) then
    Result := ALookAndFeelContainer.GetLookAndFeel.Painter
  else
    Result := AHintStyleController.LookAndFeelPainter;
end;

procedure TcxCustomHintWindow.ShowHint(X, Y: Integer; ACaption, AHint: string;
  AMaxWidth: Integer);
begin
  // to do
end;

procedure TcxCustomHintWindow.SetStandardHint(const Value: Boolean);
begin
  if FStandardHint <> Value then
  begin
    FStandardHint := Value;
    if FStandardHint then
      DoubleBuffered := False;
  end;
end;

{ TcxCustomHintStyle }

constructor TcxCustomHintStyle.Create(AComponent: TComponent);
begin
  inherited Create(AComponent);
end;

procedure TcxCustomHintStyle.ComponentRemoved(AComponent: TComponent);
begin
  //do nothing
end;

procedure TcxCustomHintStyle.ControllerChangedNotification(
  AStyleController: TcxCustomHintStyleController);
begin
  //do nothing
end;

procedure TcxCustomHintStyle.ControllerFreeNotification(
  AHintStyleController: TcxCustomHintStyleController);
begin
  //do nothing
end;

procedure TcxCustomHintStyle.DoShowHint(var AHintStr: string; var ACanShow: Boolean;
  var AHintInfo: THintInfo);
begin
  //do nothing
end;

{ TcxCustomHintStyleController }

constructor TcxCustomHintStyleController.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FUpdateCount := 0;
  FHintShortPause := 50;
  FHintPause := 500;
  FHintHidePause := 2500;
  FListeners := TList.Create;
  FLookAndFeel := TcxLookAndFeel.Create(Self);
  FGlobal := True;
  CreateHintStyle;
end;

destructor TcxCustomHintStyleController.Destroy;
var
  I: Integer;
begin
  for I := 0 to FListeners.Count - 1 do
    TcxCustomHintStyle(FListeners[I]).ControllerFreeNotification(Self);

  DestroyHintStyle;
  FreeAndNil(FLookAndFeel);
  FreeAndNil(FListeners);
  inherited Destroy;
end;

procedure TcxCustomHintStyleController.Assign(Source: TPersistent);
begin
  if (Source is TcxCustomHintStyleController) then
  begin
    BeginUpdate;
    try
      with (Source as TcxCustomHintStyleController) do
      begin
        Self.OnHintStyleChanged := OnHintStyleChanged;
        Self.OnShowHint := OnShowHint;
        Self.OnShowHintEx := OnShowHintEx;
        Self.HintShortPause := HintShortPause;
        Self.HintPause := HintPause;
        Self.HintHidePause := HintHidePause;
        Self.HintStyleClassName := HintStyleClassName;
        Self.HintStyle := HintStyle;
        Self.LookAndFeel := LookAndFeel;
      end;
    finally
      EndUpdate;
    end
  end
  else
    inherited Assign(Source);
end;

function TcxCustomHintStyleController.GetLookAndFeelValue: TcxLookAndFeel;
begin
  Result := LookAndFeel;
end;

procedure TcxCustomHintStyleController.ShowHint(X, Y: Integer; ACaption, AHint: string; AMaxWidth: Integer = 0);
begin
  FdxHintController.HintedControl := FindVCLWindow(Point(X, Y));
  FHintWindow.ShowHint(X, Y, ACaption, AHint, AMaxWidth);
end;

procedure TcxCustomHintStyleController.HideHint;
begin
  cxRemoveHintedControl;
  if FHintWindow.HandleAllocated and IsWindowVisible(FHintWindow.Handle) then
    ShowWindow(FHintWindow.Handle, SW_HIDE);
end;

procedure TcxCustomHintStyleController.Loaded;
begin
  inherited Loaded;
  SetApplicationHintProperties;
  Changed;
end;

procedure TcxCustomHintStyleController.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent <> nil) and (FHintStyle <> nil) then
    FHintStyle.ComponentRemoved(AComponent);
end;

procedure TcxCustomHintStyleController.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TcxCustomHintStyleController.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then
    SetApplicationHintProperties;
end;

procedure TcxCustomHintStyleController.AddListener(AListener: TcxCustomHintStyle);
begin
  if (AListener <> nil) and (FListeners.IndexOf(AListener) < 0) then
    FListeners.Add(AListener);
end;

procedure TcxCustomHintStyleController.RemoveListener(AListener: TcxCustomHintStyle);
begin
  if not (csDestroying in ComponentState) and (AListener <> nil) and (FListeners.IndexOf(AListener) >= 0) then
    FListeners.Remove(AListener);
end;

procedure TcxCustomHintStyleController.CreateHintStyle;
begin
  FHintStyle := HintStyleClass.Create(Self);
  FHintStyle.OnChanged := HintStyleChanged;
  FHintWindow := GetHintWindowClass.Create(Self);
  FdxHintController.RegisterHintStyleController(Self);
end;

procedure TcxCustomHintStyleController.DestroyHintStyle;
begin
  FdxHintController.UnRegisterHintStyleController(Self);
  FreeAndNil(FHintWindow);
  FreeAndNil(FHintStyle);
end;

procedure TcxCustomHintStyleController.Changed;
var
  I: Integer;
begin
  if not (csDestroying in ComponentState) then
  begin
    if (HintStyle <> nil) and Assigned(FOnHintStyleChanged) then
      FOnHintStyleChanged(Self, HintStyle);
    for I := 0 to Listeners.Count - 1 do
      DoHintStyleChanged(TcxCustomHintStyle(Listeners[I]));
  end;
end;

procedure TcxCustomHintStyleController.DoHintStyleChanged(AStyle: TcxCustomHintStyle);
begin
  AStyle.ControllerChangedNotification(Self);
  if Assigned(FOnHintStyleChanged) then
    FOnHintStyleChanged(Self, AStyle);
end;

function TcxCustomHintStyleController.GetHintStyleClass: TcxHintStyleClass;
begin
  Result := FHintStyleClass;
end;

function TcxCustomHintStyleController.GetHintWindowClass: TcxCustomHintWindowClass;
begin
  Result := HintStyle.GetHintWindowClass;
end;

procedure TcxCustomHintStyleController.DoShowHintEx(var AHintStr, AHintCaption: string;
  var ACanShow: Boolean; var AHintInfo: THintInfo);
begin
  if Assigned(FOnShowHintEx) then
    FOnShowHintEx(Self, AHintCaption, AHintStr, ACanShow, AHintInfo);
end;

procedure TcxCustomHintStyleController.DoShowHint(var AHintStr: string;
  var ACanShow: Boolean; var AHintInfo: THintInfo);
var
  AHintCaption: string;
begin
  AHintInfo.HintWindowClass := GetHintWindowClass;
  HintStyle.DoShowHint(AHintStr, ACanShow, AHintInfo);
  if Assigned(FOnShowHint) then
    FOnShowHint(Self, AHintStr, ACanShow, AHintInfo);
  AHintCaption := '';
  DoShowHintEx(AHintStr, AHintCaption, ACanShow, AHintInfo);
  FHintWindow.Caption := AHintCaption;
end;

function TcxCustomHintStyleController.GetHintStyleClassName: string;
begin
  if HintStyle <> nil then
    Result := HintStyle.ClassName
  else
    Result := '';
end;

function TcxCustomHintStyleController.GetLookAndFeelPainter: TcxCustomLookAndFeelPainter;
begin
  Result := LookAndFeel.Painter;
end;

procedure TcxCustomHintStyleController.HintStyleChanged(Sender: TObject);
begin
  Changed;
end;

procedure TcxCustomHintStyleController.SetGlobal(Value: Boolean);
begin
  if FGlobal <> Value then
  begin
    if Value then
      FdxHintController.ResetGlobal;
    FGlobal := Value;
  end;
end;

procedure TcxCustomHintStyleController.SetHintStyle(Value: TcxCustomHintStyle);
begin
  FHintStyle.Assign(Value);
end;

procedure TcxCustomHintStyleController.SetHintStyleClass(
  const Value: TcxHintStyleClass);
begin
  if FHintStyleClass <> Value then
  begin
    DestroyHintStyle;
    FHintStyleClass := Value;
    CreateHintStyle;
  end;
end;

procedure TcxCustomHintStyleController.SetHintStyleClassName(
  const Value: string);
begin
  HintStyleClass := TcxHintStyleClass(cxRegisteredHintStyles.FindByClassName(Value));
end;

procedure TcxCustomHintStyleController.SetLookAndFeel(
  const Value: TcxLookAndFeel);
begin
  LookAndFeel.Assign(Value);
end;

procedure TcxCustomHintStyleController.SetApplicationHintProperties;
begin
  if not (csDesigning in ComponentState) and (FUpdateCount = 0) then
  begin
    Application.HintShortPause := FHintShortPause;
    Application.HintPause := FHintPause;
    Application.HintHidePause := FHintHidePause;
  end;
end;

procedure TcxCustomHintStyleController.SetHintShortPause(Value: Integer);
begin
  if FHintShortPause <> Value then
  begin
    FHintShortPause := Value;
    SetApplicationHintProperties;
  end;
end;

procedure TcxCustomHintStyleController.SetHintPause(Value: Integer);
begin
  if FHintPause <> Value then
  begin
    FHintPause := Value;
    SetApplicationHintProperties;
  end;
end;

procedure TcxCustomHintStyleController.SetHintHidePause(Value: Integer);
begin
  if FHintHidePause <> Value then
  begin
    FHintHidePause := Value;
    SetApplicationHintProperties;
  end;
end;

{ TdxCustomHintViewInfoHelper }

function TdxCustomHintViewInfoHelper.GetScaleFactor: TdxScaleFactor;
begin
  Result := dxSystemScaleFactor;
end;

{ TdxCustomHintViewInfo }

constructor TdxCustomHintViewInfo.Create;
begin
  inherited Create;
  CreateHelper;
end;

destructor TdxCustomHintViewInfo.Destroy;
begin
  DestroyHelper;
  inherited Destroy;
end;

procedure TdxCustomHintViewInfo.Calculate(ACanvas: TCanvas);
begin
  FHelper.UseRightToLeftAlignment := UseRightToLeftAlignment;
  FHelper.UseRightToLeftReading := UseRightToLeftReading;
  FHelper.Calculate(ACanvas);
end;

procedure TdxCustomHintViewInfo.Paint(ACanvas: TCanvas);
begin
  FHelper.Paint(ACanvas);
end;

procedure TdxCustomHintViewInfo.DestroyHelper;
begin
  FreeAndNil(FHelper);
end;

function TdxCustomHintViewInfo.GetBoundsRect: TRect;
begin
  Result := FHelper.BoundsRect;
end;

{ TdxHintViewInfoHelper }

constructor TdxHintViewInfoHelper.Create(const AHint, AShortCut: string; const ACursorPos: TPoint);
begin
  inherited Create;
  if AHint <> '' then
    FText := AHint + AShortCut;
  FPos := ACursorPos;
end;

destructor TdxHintViewInfoHelper.Destroy;
begin
  FreeAndNil(FScaleFactor);
  inherited;
end;

procedure TdxHintViewInfoHelper.Calculate(ACanvas: TCanvas);
var
  ASize: TSize;
begin
  PrepareCanvasFont(ACanvas);
  SetTextColor(ACanvas);
  if FText <> '' then
  begin
    ASize := CalculateMinSize(ACanvas);
    CorrectMinSize(ASize);
    BoundsRect := cxRect(ASize);
  end
  else
    BoundsRect := cxEmptyRect;
end;

procedure TdxHintViewInfoHelper.Paint(ACanvas: TCanvas);
var
  R: TRect;
begin
  PrepareCanvasFont(ACanvas);
  R := BoundsRect;
  DrawEdge(ACanvas.Handle, R, BDR_RAISEDOUTER, BF_RECT);
  DrawText(ACanvas);
end;

procedure TdxHintViewInfoHelper.CorrectMinSize(var ASize: TSize);
begin
  Inc(ASize.cx, 2 * (1 + ScaleFactor.Apply(2)));
  Inc(ASize.cy, 2 * (1 + ScaleFactor.Apply(2)));
end;

function TdxHintViewInfoHelper.GetScaleFactor: TdxScaleFactor;
begin
  if FScaleFactor = nil then
    FScaleFactor := TdxScaleFactor.Create;
  FScaleFactor.Assign(dxGetMonitorDPI(FPos), dxDefaultDPI);
  Result := FScaleFactor;
end;

function TdxHintViewInfoHelper.GetTextRect: TRect;
begin
  Result := cxRectInflate(BoundsRect, -1);
  Result := cxRectInflate(Result, -ScaleFactor.Apply(2));
end;

procedure TdxHintViewInfoHelper.DrawText(ACanvas: TCanvas);
const
  AAlignment: array[Boolean] of Cardinal = (DT_LEFT, DT_RIGHT);
var
  R: TRect;
  ATextFlags: Cardinal;
begin
  R := GetTextRect;
  ATextFlags := DT_NOCLIP or DT_NOPREFIX or DT_WORDBREAK or AAlignment[UseRightToLeftAlignment];
  if UseRightToLeftReading then
    ATextFlags := ATextFlags or DT_RTLREADING;
  cxDrawText(ACanvas.Handle, FText, R, ATextFlags);
end;

procedure TdxHintViewInfoHelper.PrepareCanvasFont(ACanvas: TCanvas);
var
  ANonClientMetrics: TNonClientMetrics;
begin
  if dxSystemInfo.GetParameter(SPI_GETNONCLIENTMETRICS, ANonClientMetrics) then
  begin
    ACanvas.Font.Handle := CreateFontIndirect(ANonClientMetrics.lfStatusFont);
    ACanvas.Font.Height := ScaleFactor.Apply(ACanvas.Font.Height, dxSystemScaleFactor);
  end
  else
    ACanvas.Font.Size := ScaleFactor.Apply(8);
end;

procedure TdxHintViewInfoHelper.SetTextColor(ACanvas: TCanvas);
begin
  ACanvas.Font.Color := Screen.HintFont.Color;
end;

function TdxHintViewInfoHelper.CalculateMinSize(ACanvas: TCanvas): TSize;
var
  R: TRect;
  AWidth: Integer;
begin
  AWidth := cxRectWidth(GetDesktopWorkArea(GetMouseCursorPos));
  R := Rect(0, 0, AWidth, 0);
  cxDrawText(ACanvas.Handle, FText, R, DT_CALCRECT or DT_LEFT or DT_NOPREFIX or DT_WORDBREAK);
  Result := cxSize(R.Right, R.Bottom);
end;

{ TcxControlHintHelper }

destructor TcxCustomHintHelper.Destroy;
begin
  HideHint;
  FreeAndNil(FHintWindow);
  inherited Destroy;
end;

procedure TcxCustomHintHelper.HintCheckerTimerHandler(Sender: TObject);
begin
  if not CanShowHint then
    CancelHint;
end;

procedure TcxCustomHintHelper.HintHideTimerHandler(Sender: TObject);
begin
  HideHint;
end;

function TcxCustomHintHelper.IsMouseButtonPressed: Boolean;
begin
  Result := GetMouseKeys and (MK_LBUTTON or MK_MBUTTON or MK_RBUTTON) <> 0;
end;

procedure TcxCustomHintHelper.MouseLeave;
begin
  CancelHint;
end;

function TcxCustomHintHelper.PtInCaller(const P: TPoint): Boolean;
var
  AIntf: IcxHintableObject;
begin
  if FHintableObject = nil then
    if IsRectEmpty(FHintAreaBounds) then
      Result := PtInRect(GetHintControlBounds, P)
    else
      Result := PtInRect(FHintAreaBounds, P)
  else
    Result := Supports(FHintableObject, IcxHintableObject, AIntf) and
      AIntf.HasHintPoint(P);
end;

procedure TcxCustomHintHelper.CancelHint;
begin
  HideHint;
  ResetLastHintElement;
end;

function TcxCustomHintHelper.GetHintWinControl: TWinControl;
begin
  Result := GetOwnerWinControl;
end;

function TcxCustomHintHelper.GetHintControlBounds: TRect;
begin
  Result := GetHintWinControl.ClientRect;
end;

function TcxCustomHintHelper.GetHintHidePause: Integer;
begin
  Result := Application.HintHidePause;
end;

function TcxCustomHintHelper.GetOwnerWinControl: TWinControl;
begin
  Result := nil;
end;

function TcxCustomHintHelper.IsHintAtMousePos: Boolean;
var
  AIntf: IcxHintableObject;
begin
  Result := Supports(FHintableObject, IcxHintableObject, AIntf) and
    AIntf.IsHintAtMousePos;
end;

function TcxCustomHintHelper.IsSuppressHintOnMouseDown: Boolean;
begin
  Result := True;
end;

procedure TcxCustomHintHelper.ResetLastHintElement;
begin
  FLastHintableObject := nil;
end;

function TcxCustomHintHelper.UseHintHidePause: Boolean;
var
  AIntf: IcxHintableObject;
begin
  Result := (FHintableObject = nil) or
    (Supports(FHintableObject, IcxHintableObject, AIntf) and AIntf.UseHintHidePause);
end;

function TcxCustomHintHelper.CanShowHint: Boolean;
var
  AHintControl: TWinControl;
begin
  if IsSuppressHintOnMouseDown and IsMouseButtonPressed then
  begin
    Result := False;
    Exit;
  end;
  AHintControl := GetHintWinControl;
  Result := cxCanShowHint(AHintControl);
end;

procedure TcxCustomHintHelper.CorrectHintWindowRect(var ARect: TRect);

  function GetTextRectOffset: TPoint;
  begin
    with FHintWindow.ClientToScreen(cxNullPoint) do
    begin
      Result.X := X - FHintWindow.Left + cxTextOffset;
      Result.Y := Y - FHintWindow.Top + cxTextOffset;
    end;
  end;

begin
  with GetTextRectOffset do
    InflateRect(ARect, X, Y);
end;

procedure TcxCustomHintHelper.HideHint;
begin
  StopHintCheckerTimers;
  EndMouseTracking(Self);
  FHintableObject := nil;
  FHintAreaBounds := cxEmptyRect;
  if (FHintWindow <> nil) and FHintWindow.HandleAllocated then
    ShowWindow(FHintWindow.Handle, SW_HIDE);
  if not UseHintHidePause then
    FLastHintText := '';
end;

procedure TcxCustomHintHelper.MouseDown;
begin
  if IsSuppressHintOnMouseDown then
    HideHint;
end;

function TcxCustomHintHelper.GetHintWindowClass: THintWindowClass;
begin
  Result := cxGetHintWindowClass;
end;

function TcxCustomHintHelper.GetHintWindowRect(
  const AHintAreaBounds, ATextRect: TRect; const AText: string;
  AIsHintMultiLine: Boolean): TRect;

  function GetMaxTextWidth: Integer;
  begin
    if AIsHintMultiLine then
      Result := ATextRect.Right - ATextRect.Left
    else
      Result := cxMaxRectSize;
  end;

var
  AOffset: TPoint;
begin
  Result := HintWindow.CalcHintRect(GetMaxTextWidth, AText, nil);
  Dec(Result.Right, 6);
  Dec(Result.Bottom, 2);
  if SysLocale.MiddleEast and (HintWindow.BiDiMode = bdRightToLeft) then
    AOffset := GetHintWinControl.ClientToScreen(Point(ATextRect.Right - Result.Right, ATextRect.Top))
  else
    AOffset := GetHintWinControl.ClientToScreen(ATextRect.TopLeft);
  OffsetRect(Result, AOffset.X, AOffset.Y);
  CorrectHintWindowRect(Result);
  MakeVisibleOnDesktop(Result, Result.TopLeft);
  Dec(Result.Bottom, 4);
end;

procedure TcxCustomHintHelper.ShowHint(const AHintAreaBounds, ATextRect: TRect;
  const AText: string; AIsHintMultiLine: Boolean; AHintObject: TObject; AFont: TFont = nil);
var
  AHintInfo: THintInfo;
  AHintWindowRect: TRect;
  AIntf: IcxHintWindowHelper;
  R: TRect;
begin
  if not CanShowHint then
    Exit;
  if (AHintObject = FLastHintableObject) then
  begin
    if IsHintAtMousePos then
    begin
      if Assigned(FHintHideTimer) and not cxPointIsEqual(FLastHintMousePos, GetMouseCursorPos) then
        RestartHintHideTimer
      else
        if UseHintHidePause then
          Exit;
    end
    else
      if UseHintHidePause then
      begin
        if FLastHintText <> AText then
          ResetLastHintElement;
        Exit;
      end;
  end
  else
    if FLastHintableObject <> nil then
      CancelHint;

  if (FHintWindow = nil) or (FHintWindow.ClassType <> GetHintWindowClass) then
    RecreateHintWindow
  else
    if IsWindowVisible(FHintWindow.Handle) then
    begin
      if (FLastHintText = AText) and cxRectIsEqual(FHintTextRect, ATextRect) then
        Exit;
      EndMouseTracking(Self);
    end;
  if Assigned(AFont) then
    FHintWindow.Canvas.Font := AFont;
  FHintWindow.BiDiMode := GetOwnerWinControl.BiDiMode;
  AHintWindowRect := GetHintWindowRect(AHintAreaBounds, ATextRect, AText, AIsHintMultiLine);
  if cxProcessControlHintInfo(AHintInfo, FHintWindow, GetOwnerWinControl, AText,
    AHintWindowRect, AHintObject, GetHintHidePause) then
  begin
    FLastHintText := AText;
    FHintTextRect := ATextRect;
    FHintAreaBounds := AHintAreaBounds;
    FHintableObject := AHintObject;
    FLastHintableObject := AHintObject;
    if Supports(TObject(FHintWindow), IcxHintWindowHelper, AIntf) then
    begin
      R := AHintAreaBounds;
      with GetHintWinControl.ClientToScreen(cxNullPoint) do
        OffsetRect(R, X, Y);
      AIntf.SetHintAreaBounds(R, IsHintAtMousePos);
    end;
    cxActivateHint(FHintWindow, AHintWindowRect, AHintInfo.HintStr);
    FHintHidePause := AHintInfo.HideTimeout;
    BeginMouseTracking(GetHintWinControl, FHintAreaBounds, Self);
    StartHintCheckerTimers;
  end;
end;

procedure TcxCustomHintHelper.RecreateHintWindow;
begin
  cxRecreateHintWindow(FHintWindow, GetHintWindowClass, Application.HintColor);
end;

procedure TcxCustomHintHelper.StartHintCheckerTimers;
begin
  if FHintCheckerTimer <> nil then Exit;
  FHintCheckerTimer := cxCreateTimer(HintCheckerTimerHandler, 100);
  if UseHintHidePause then
    FHintHideTimer := cxCreateTimer(HintHideTimerHandler, FHintHidePause);
end;

procedure TcxCustomHintHelper.StopHintCheckerTimers;
begin
  FreeAndNil(FHintCheckerTimer);
  FreeAndNil(FHintHideTimer);
end;

procedure TcxCustomHintHelper.RestartHintHideTimer;
begin
  FLastHintMousePos := GetMouseCursorPos;
  FHintHideTimer.Enabled := False;
  FHintHideTimer.Enabled := True;
end;

{ TcxControlHintHelper }

function TcxControlHintHelper.GetHintControl: TcxControl;
begin
  Result := GetOwnerControl;
end;

function TcxControlHintHelper.GetHintControlBounds: TRect;
begin
  Result := GetOwnerControl.Bounds;
end;

function TcxControlHintHelper.GetHintWinControl: TWinControl;
begin
  Result := GetHintControl;
end;

function TcxControlHintHelper.GetOwnerWinControl: TWinControl;
begin
  Result := GetOwnerControl;
end;

initialization
  FdxHintController := TcxHintController.Create(nil);

finalization
  FreeAndNil(FdxHintController);
  FreeAndNil(FRegisteredHintStyles);

end.
