{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           Express Cross Platform Library controls                  }
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

unit dxForms;

{$I cxVer.inc}

interface

uses
  Types, Windows, Messages, Graphics, Forms, Classes, Controls, SysUtils, MultiMon, Contnrs,
  Generics.Collections, Generics.Defaults, ImgList,
  dxCore, dxMessages, cxGeometry, cxClasses, cxGraphics;

{$IFNDEF DELPHI10SEATTLE}
const
  {$EXTERNALSYM WM_DPICHANGED}
  WM_DPICHANGED = $02E0;
{$ENDIF}

type

  { IdxFloatForm }

  IdxFloatForm = interface
  ['{BAED1CE9-B7D7-468D-907B-79883637054D}']
    function GetParentForm: TCustomForm;
  end;

  { TdxCustomForm }

  TdxCustomForm = class abstract(TCustomForm, IdxScaleFactor)
  strict private
    FRightToLeftApplied: Boolean;
    FRightToLeftLayout: TdxDefaultBoolean;
  {$IFNDEF DELPHI101BERLIN}
    FScaled: Boolean;
  {$ENDIF}
    FScaleFactor: TdxScaleFactor;

    function IsRightToLeftLayout: Boolean;
    procedure RightToLeftLayoutChanged;
    procedure SetRightToLeftLayout(const AValue: TdxDefaultBoolean);
    procedure CMBiDiModeChanged(var Message: TMessage); message CM_BIDIMODECHANGED;
    procedure WMDPIChanged(var Message: TMessage); message WM_DPICHANGED;
    // IdxScaleFactor
    function GetScaleFactor: TdxScaleFactor;
  protected
    procedure ChangeScale(M, D: Integer); overload; override; final;
    procedure ChangeScale(M, D: Integer; IsDpiChange: Boolean); {$IFDEF DELPHI101BERLIN}override;{$ELSE}reintroduce; overload; virtual;{$ENDIF}
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DoShow; override;
    function GetClientOrigin: TPoint; override;
    procedure InitializeNewForm; override;
  {$IFDEF DELPHI101BERLIN}
    procedure ScaleControlsForDpi(NewPPI: Integer); override;
    procedure ScaleForCurrentDpi; override;
  {$ELSE}
    procedure ScaleForCurrentDpi; virtual;
  {$ENDIF}
    procedure ScaleFactorChanged(M, D: Integer); virtual;
    procedure SetParent(AParent: TWinControl); override;
    procedure UpdateImageLists; virtual;
    property RightToLeftLayout: TdxDefaultBoolean read FRightToLeftLayout write SetRightToLeftLayout;
    //
    property ScaleFactor: TdxScaleFactor read GetScaleFactor;
  public
    destructor Destroy; override;

    procedure BeforeDestruction; override;
    procedure ScaleBy(M, D: Integer);
    procedure ScaleForPPI(ATargetDPI: Integer; AWindowRect: PRect); reintroduce; overload; virtual;
    procedure ScaleForPPI(ATargetDPI: Integer); overload; {$IFDEF DELPHI101BERLIN}override; final;{$ENDIF}
  {$IFNDEF DELPHI101BERLIN}
    property Scaled: Boolean read FScaled write FScaled default True;
  {$ENDIF}
  end;

  { TdxCustomFloatForm }

  TdxCustomFloatForm = class abstract(TdxCustomForm, IdxFloatForm)
  protected
    // IdxFloatForm
    function GetParentForm: TCustomForm; virtual; abstract;
  public
    constructor CreateNew(AOwner: TComponent; Dummy: Integer  = 0); override;
  end;

  { TdxForm }

  TdxForm = class(TForm, IdxScaleFactor)
  strict private
    FLoadedClientHeight: Integer;
    FLoadedClientWidth: Integer;
    FRightToLeftApplied: Boolean;
    FRightToLeftLayout: TdxDefaultBoolean;
  {$IFNDEF DELPHIXE8}
    FTextHeight: Integer;
  {$ENDIF}
  {$IFNDEF DELPHI101BERLIN}
    FScaled: Boolean;
  {$ENDIF}
    FScaleFactor: TdxScaleFactor;

    // IdxScaleFactor
    function GetScaleFactor: TdxScaleFactor;
    function GetTargetDPI: Integer;
  {$IFNDEF DELPHIXE8}
    function GetTextHeight: Integer;
    procedure ReadTextHeight(Reader: TReader);
    procedure WriteTextHeight(Writer: TWriter);
  {$ENDIF}
    function IsRightToLeftLayout: Boolean;
    procedure RightToLeftLayoutChanged;
    procedure SetClientHeight(Value: Integer);
    procedure SetClientWidth(Value: Integer);
    procedure SetPixelsPerInch(Value: Integer);
    procedure SetRightToLeftLayout(const AValue: TdxDefaultBoolean);
    procedure CMBiDiModeChanged(var Message: TMessage); message CM_BIDIMODECHANGED;
    procedure WMDPIChanged(var Message: TMessage); message WM_DPICHANGED;
  {$IFNDEF DELPHI102TOKYO}
    procedure WMNCCreate(var Message: TWMNCCreate); message WM_NCCREATE;
  {$ENDIF}
  protected
    procedure ChangeScale(M, D: Integer); overload; override; final;
  {$IFDEF DELPHI101BERLIN}
    procedure ChangeScale(M, D: Integer; IsDpiChange: Boolean); override;
  {$ELSE}
    procedure ChangeScale(M, D: Integer; IsDpiChange: Boolean); reintroduce; overload; virtual;
  {$ENDIF}
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure DoShow; override;
    function GetClientOrigin: TPoint; override;
    procedure InitializeNewForm; override;
    procedure ScaleFactorChanged(M, D: Integer); virtual;
    procedure UpdateImageLists; virtual;
  {$IFDEF DELPHI101BERLIN}
    procedure ScaleControlsForDpi(NewPPI: Integer); override;
    procedure ScaleForCurrentDpi; override;
  {$ELSE}
    procedure ScaleForCurrentDpi; virtual;
  {$ENDIF}
  {$IFNDEF DELPHI101BERLIN}
    procedure Loaded; override;
  {$ENDIF}
    procedure ReadState(Reader: TReader); override;
    procedure SetParent(AParent: TWinControl); override;
    property RightToLeftLayout: TdxDefaultBoolean read FRightToLeftLayout write SetRightToLeftLayout default bDefault;
    //
    property ScaleFactor: TdxScaleFactor read GetScaleFactor;
  public
    destructor Destroy; override;

    procedure BeforeDestruction; override;
    procedure ScaleBy(M, D: Integer);
    procedure ScaleForPPI(ATargetDPI: Integer; AWindowRect: PRect); reintroduce; overload; virtual;
    procedure ScaleForPPI(ATargetDPI: Integer); overload; {$IFDEF DELPHI101BERLIN}override; final;{$ENDIF}
  published
    property ClientHeight write SetClientHeight;
    property ClientWidth write SetClientWidth;
    property PixelsPerInch write SetPixelsPerInch;
  {$IFNDEF DELPHI101BERLIN}
    property Scaled: Boolean read FScaled write FScaled default True;
  {$ENDIF}
  end;
  TdxFormClass = class of TdxForm;

  { TdxFormHelper }

  TdxFormHelper = class
  protected type
  {$REGION 'Internal Types'}
    TState = class
    protected
      LockedControls: TComponentList;
      RedrawLocked: Boolean;
    end;
  {$ENDREGION}
  strict private
    class var FScalingContainers: TList;

    class procedure PopulateControls(AControl: TControl; ATargetList: TComponentList);
  protected
    class procedure ScalingBegin(AContainer: TWinControl; out AState: TState);
    class procedure ScalingEnd(AContainer: TWinControl; AState: TState);
    class procedure UpdateScaleFactorOnParentChange(AForm: TCustomForm);
  public
    class function SetAnimation(AValue: Boolean): Boolean;
    class procedure SetWindowTextWithoutRedrawing(AWndHandle: THandle; const AText: string);
    // Redrawing
    class function LockWindowRedrawing(AWndHandle: THandle): Cardinal;
    class procedure UnlockWindowRedrawing(AWndHandle: THandle; APrevStyle: Cardinal);
    // Scaling
    class function IsScaleChanging(AContainer: TWinControl): Boolean;
    class procedure RepaintVisibleWindowArea(AHandle: THandle; var APrevVisibleRegion: TcxRegionHandle);
    class procedure ResetScalingFlags(AControl: TControl);
    class procedure ScaleComponentsForDpi(AOwner: TComponent; NewPPI: Integer);
    class procedure ScaleForPPI(Form: TCustomForm; TargetDPI: Integer; WindowRect: PRect; FromMessageHandler: Boolean);
  end;

var
  dxAutoReplaceImageListReferencesOnDPIChanges: Boolean = True;

function dxGetFormIcon(AHandle: THandle; ASizeX, ASizeY: Integer): HICON;
implementation

uses
  dxDPIAwareUtils, cxControls, dxHooks, dxCoreClasses, dxThreading, TypInfo, Menus;

type
  TControlAccess = class(TControl);
  TCustomFormAccess = class(TCustomForm);
  TCustomFrameAccess = class(TCustomFrame);
  TdxScaleFactorAccess = class(TdxScaleFactor);
  TScrollingWinControlAccess = class(TScrollingWinControl);

  TEnableNonClientDpiScalingProc = function (AHandle: HWND): LongBool; stdcall;

  { TdxFrameController }

  TdxFrameController = class
  strict private
  {$IFNDEF DELPHI101BERLIN}
    FCurrentPPI: Integer;
  {$ENDIF}
    FWindowProcObject: TcxWindowProcLinkedObject;

    function GetCurrentPPI: Integer;
    function GetFrame: TCustomFrameAccess;
    function GetTargetDPI: Integer;
    procedure SetCurrentPPI(const Value: Integer);
  protected
    procedure RestoreParentFont;
    procedure ScaleForPPI(ATargetPPI: Integer; ACanScaleContent: Boolean = True);
    procedure WndProc(var AMessage: TMessage);
  public
    constructor Create(AFrame: TFrame); reintroduce;
    destructor Destroy; override;
    //
    property CurrentPPI: Integer read GetCurrentPPI write SetCurrentPPI;
    property Frame: TCustomFrameAccess read GetFrame;
  end;

  { TdxFrameControllerManager }

  TdxFrameControllerManager = class(TComponent)
  strict private
    class var FFreeNotifier: TcxFreeNotificator;
    class var FMap: TObjectDictionary<TWinControl, TdxFrameController>;

    class procedure FreeNotificationHandler(Sender: TComponent);
  public
    class constructor Create;
    class destructor Destroy;
    class procedure Register(AWnd: THandle);
    class procedure Unregister(AWnd: THandle);
  end;

  { TdxFormImageListHelper }

  TdxFormImageListHelper = class
  strict private
    FReplacementCache: TDictionary<TObject, TObject>;
    FTargetDPI: Integer;

    function GenerateName(const ABaseName: TComponentName; ATargetDPI: Integer): TComponentName;
    function GetBaseImageListName(const AName: TComponentName): TComponentName;
    function GetReplacement(AImageList: TCustomImageList): TCustomImageList;
  protected
    procedure UpdateImageList(AInstance: TObject; APropInfo: PPropInfo; APropValue: TObject);
    procedure UpdateImageListProperties(APersistent: TPersistent);
    procedure UpdateImageLists(AForm: TCustomForm);
  public
    constructor Create(ATargetDPI: Integer);
    destructor Destroy; override;
    class procedure Execute(ATargetDPI: Integer; AForm: TCustomForm);
  end;

  {TdxFormRightToLeftLayoutHelper}

  TdxFormRightToLeftLayoutHelper = class
  strict private
    class function GetBiDiModeStyles: Cardinal;
    class function GetRightToLeftLayoutStyles: Cardinal;
  public
    class procedure AddRightToLeftFormExStyle(var Params: TCreateParams);
    class procedure RemoveBiDiModeStyles(AForm: TCustomForm);
    class procedure SetMenusParentBiDiMode(AForm: TCustomForm);
    class procedure ResetMenusBiDiMode(AForm: TCustomForm);
  end;

var
  FEnableNonClientDpiScaling: TEnableNonClientDpiScalingProc;

function dxGetFormIcon(AHandle: THandle; ASizeX, ASizeY: Integer): HICON;
begin
  Result := SendMessage(AHandle, WM_GETICON, ICON_SMALL, 0);
  if Result = 0 then
    Result := SendMessage(AHandle, WM_GETICON, ICON_BIG, 0);

  Result := CopyImage(Result, IMAGE_ICON, ASizeX, ASizeY, LR_COPYFROMRESOURCE);
end;

procedure dxFrameWndProcHook(ACode: Integer; wParam: WPARAM; lParam: LPARAM; var AHookResult: LRESULT);
var
  AMsg: PCWPStruct;
begin
  if dxIsDesignTime then Exit;

  AMsg := PCWPStruct(lParam);
  case AMsg.message of
    WM_CREATE:
      TdxFrameControllerManager.Register(AMsg.hwnd);
  end;
end;

{ TdxFrameController }

constructor TdxFrameController.Create(AFrame: TFrame);
begin
  FWindowProcObject := cxWindowProcController.Add(AFrame, WndProc);
{$IFNDEF DELPHI101BERLIN}
  if TdxFormHelper.IsScaleChanging(AFrame.Parent) then
    FCurrentPPI := GetTargetDPI
  else
    FCurrentPPI := dxDefaultDPI;
{$ENDIF}
  ScaleForPPI(GetTargetDPI);
end;

destructor TdxFrameController.Destroy;
begin
  if cxWindowProcController <> nil then
    cxWindowProcController.Remove(FWindowProcObject);
  inherited Destroy;
end;

procedure TdxFrameController.RestoreParentFont;
begin
  if Frame.Parent <> nil then
  begin
    Frame.Font := TControlAccess(Frame.Parent).Font;
    Frame.ParentFont := True;
  end;
end;

procedure TdxFrameController.ScaleForPPI(ATargetPPI: Integer; ACanScaleContent: Boolean = True);
var
{$IFNDEF DELPHI101BERLIN}
  AState: TdxFormHelper.TState;
{$ENDIF}
  AParentFont: Boolean;
begin
  if ACanScaleContent and not (csLoading in Frame.ComponentState) then
  begin
    AParentFont := Frame.ParentFont;
  {$IFDEF DELPHI101BERLIN}
    Frame.ScaleForPPI(ATargetPPI);
  {$ELSE}
    TdxFormHelper.ScalingBegin(Frame, AState);
    try
      Frame.ChangeScale(ATargetPPI, CurrentPPI);
    finally
      TdxFormHelper.ScalingEnd(Frame, AState);
    end;
  {$ENDIF}
    if AParentFont then
      RestoreParentFont;
  end;
  CurrentPPI := ATargetPPI;
  TdxFormHelper.ScaleComponentsForDpi(Frame, ATargetPPI);
end;

procedure TdxFrameController.WndProc(var AMessage: TMessage);
begin
  if FWindowProcObject <> nil then
    FWindowProcObject.DefaultProc(TMessage(AMessage));
  case AMessage.Msg of
    WM_CREATE:
      ScaleForPPI(GetTargetDPI);
    DXM_SCALECHANGED:
      ScaleForPPI(GetTargetDPI, False);
    CM_PARENTFONTCHANGED:
      if Frame.ParentFont then
        RestoreParentFont;
  end;
end;

function TdxFrameController.GetCurrentPPI: Integer;
begin
{$IFDEF DELPHI101BERLIN}
  Result := Frame.FCurrentPPI;
{$ELSE}
  Result := FCurrentPPI;
{$ENDIF}
end;

function TdxFrameController.GetFrame: TCustomFrameAccess;
begin
  Result := TCustomFrameAccess(FWindowProcObject.Control);
end;

function TdxFrameController.GetTargetDPI: Integer;
var
  M, D: Integer;
begin
  Result := dxDefaultDPI;
  if dxGetCurrentScaleFactor(Frame.Parent, M, D) then
    Result := MulDiv(Result, M, D);
end;

procedure TdxFrameController.SetCurrentPPI(const Value: Integer);
begin
{$IFDEF DELPHI101BERLIN}
  Frame.FCurrentPPI := Value;
{$ELSE}
  FCurrentPPI := Value;
{$ENDIF}
end;

{ TdxFrameControllerManager }

class constructor TdxFrameControllerManager.Create;
begin
  FMap := TObjectDictionary<TWinControl, TdxFrameController>.Create([doOwnsValues]);
  FFreeNotifier := TcxFreeNotificator.Create(nil);
  FFreeNotifier.OnFreeNotification := FreeNotificationHandler;
end;

class destructor TdxFrameControllerManager.Destroy;
begin
  FreeAndNil(FFreeNotifier);
  FreeAndNil(FMap);
end;

class procedure TdxFrameControllerManager.Register(AWnd: THandle);
var
  AControl: TWinControl;
begin
  if dxIsCurrentProcessWindow(AWnd) then
  begin
    AControl := FindControl(AWnd);
    if (AControl is TFrame) and not (Supports(AControl, IdxScaleFactor) or FMap.ContainsKey(AControl)) then
    begin
      if csDesigning in AControl.ComponentState then
        Exit;
      FFreeNotifier.FreeNotification(AControl);
      FMap.Add(AControl, TdxFrameController.Create(TFrame(AControl)));
    end;
  end;
end;

class procedure TdxFrameControllerManager.Unregister(AWnd: THandle);
begin
  if FMap <> nil then
    FMap.Remove(FindControl(AWnd));
end;

class procedure TdxFrameControllerManager.FreeNotificationHandler(Sender: TComponent);
begin
  if FMap <> nil then
    FMap.Remove(TWinControl(Sender));
end;

{ TdxCustomForm }

destructor TdxCustomForm.Destroy;
begin
  TdxUIThreadSyncService.Unsubscribe(Self);
  inherited Destroy;
  FreeAndNil(FScaleFactor);
end;

procedure TdxCustomForm.ScaleBy(M, D: Integer);
begin
  ScaleForPPI(MulDiv(dxGetFormDPI(Self), M, D));
end;

procedure TdxCustomForm.ScaleForPPI(ATargetDPI: Integer; AWindowRect: PRect);
begin
  TdxFormHelper.ScaleForPPI(TCustomFormAccess(Self), ATargetDPI, AWindowRect, False);
end;

procedure TdxCustomForm.ScaleForPPI(ATargetDPI: Integer);
begin
{$IFDEF DELPHI101BERLIN}
  if TdxFormHelper.IsScaleChanging(Self) then
    inherited
  else
{$ENDIF}
    ScaleForPPI(ATargetDPI, nil);
end;

procedure TdxCustomForm.ChangeScale(M, D: Integer);
begin
  ChangeScale(M, D, False);
end;

procedure TdxCustomForm.BeforeDestruction;
begin
  inherited BeforeDestruction;
  cxBroadcastRemoveNotifications(Self);
end;

procedure TdxCustomForm.ChangeScale(M, D: Integer; IsDpiChange: Boolean);
begin
  ScaleFactor.Change(M, D);
  inherited ChangeScale(M, D{$IFDEF DELPHI101BERLIN}, IsDpiChange{$ENDIF});
  ScaleFactorChanged(M, D);
end;

function TdxCustomForm.IsRightToLeftLayout: Boolean;
begin
  Result := (FRightToLeftLayout = bTrue) or (FRightToLeftLayout = bDefault) and UseRightToLeftAlignment;
end;

procedure TdxCustomForm.RightToLeftLayoutChanged;
begin
  FRightToLeftApplied := IsRightToLeftLayout;
  if FRightToLeftApplied then
    TdxFormRightToLeftLayoutHelper.ResetMenusBiDiMode(Self)
  else
    TdxFormRightToLeftLayoutHelper.SetMenusParentBiDiMode(Self);
  RecreateWnd;
end;

procedure TdxCustomForm.SetRightToLeftLayout(const AValue: TdxDefaultBoolean);
begin
  if FRightToLeftLayout <> AValue then
  begin
    FRightToLeftLayout := AValue;
    if FRightToLeftApplied <> IsRightToLeftLayout then
      RightToLeftLayoutChanged;
  end;
end;

procedure TdxCustomForm.CMBiDiModeChanged(var Message: TMessage);
begin
  inherited;
  if FRightToLeftApplied <> IsRightToLeftLayout then
    if fsShowing in FFormState then
      TdxUIThreadSyncService.EnqueueInvokeInUIThread(Self, RightToLeftLayoutChanged)
    else
      RightToLeftLayoutChanged;
end;

procedure TdxCustomForm.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  if FRightToLeftApplied then
    TdxFormRightToLeftLayoutHelper.AddRightToLeftFormExStyle(Params);
end;

procedure TdxCustomForm.InitializeNewForm;
begin
  inherited;
{$IFDEF DELPHI101BERLIN}
  FCurrentPPI := PixelsPerInch;
{$ELSE}
  Scaled := True;
{$ENDIF}
  FScaleFactor := TdxScaleFactor.Create;
  FScaleFactor.Assign(dxGetFormDPI(Self), dxDefaultDPI);
  FRightToLeftLayout := bDefault;
end;

{$IFDEF DELPHI101BERLIN}
procedure TdxCustomForm.ScaleControlsForDpi(NewPPI: Integer);
var
  ACurPPI: Integer;
begin
  DisableAlign;
  try
    ACurPPI := dxGetFormDPI(Self);
    ScaleFactor.Change(NewPPI, ACurPPI);
    inherited ScaleControlsForDpi(NewPPI);
    TdxFormHelper.ScaleComponentsForDpi(Self, NewPPI);
    ScaleFactorChanged(NewPPI, ACurPPI);
  finally
    EnableAlign;
  end;
end;
{$ENDIF}

procedure TdxCustomForm.ScaleForCurrentDpi;
begin
  DisableAlign;
  try
    if Scaled and not (csDesigning in ComponentState) and (Parent = nil) then
      ScaleForPPI(dxGetTargetDPI(Self));
    TdxFormHelper.ResetScalingFlags(Self);
    Perform(CM_PARENTBIDIMODECHANGED, 0, 0);
  finally
    EnableAlign;
  end;
end;

procedure TdxCustomForm.ScaleFactorChanged(M, D: Integer);
begin
  UpdateImageLists;
end;

procedure TdxCustomForm.SetParent(AParent: TWinControl);
begin
  if TcxControlHelper.CanSetParent(Self, AParent) then
  begin
    inherited SetParent(AParent);
    TdxFormHelper.UpdateScaleFactorOnParentChange(Self);
  end;
end;

procedure TdxCustomForm.UpdateImageLists;
begin
  if dxAutoReplaceImageListReferencesOnDPIChanges then
    TdxFormImageListHelper.Execute(ScaleFactor.TargetDPI, Self);
end;

procedure TdxCustomForm.DoShow;
begin
  inherited DoShow;
  ScaleForCurrentDpi;
end;

function TdxCustomForm.GetClientOrigin: TPoint;
begin
  Result := inherited GetClientOrigin;
  if FRightToLeftApplied then
    Dec(Result.X, ClientWidth);
end;

procedure TdxCustomForm.WMDPIChanged(var Message: TMessage);
begin
  if TdxFormHelper.IsScaleChanging(Self) then
    inherited
  else
    if Scaled then
      TdxFormHelper.ScaleForPPI(Self, LoWord(Message.WParam), PRect(Message.LParam), True);
end;

function TdxCustomForm.GetScaleFactor: TdxScaleFactor;
begin
  Result := FScaleFactor;
end;

{ TdxCustomFloatForm }

constructor TdxCustomFloatForm.CreateNew(AOwner: TComponent; Dummy: Integer  = 0);
begin
  inherited;
  DefaultMonitor := dmDesktop;
end;

{ TdxForm }

destructor TdxForm.Destroy;
begin
  TdxUIThreadSyncService.Unsubscribe(Self);
  inherited Destroy;
  FreeAndNil(FScaleFactor);
end;

procedure TdxForm.ScaleBy(M, D: Integer);
begin
  ScaleForPPI(MulDiv(dxGetFormDPI(Self), M, D));
end;

procedure TdxForm.ScaleForPPI(ATargetDPI: Integer; AWindowRect: PRect);
begin
  TdxFormHelper.ScaleForPPI(Self, ATargetDPI, AWindowRect, False);
end;

procedure TdxForm.ScaleForPPI(ATargetDPI: Integer);
begin
{$IFDEF DELPHI101BERLIN}
  if TdxFormHelper.IsScaleChanging(Self) then
    inherited
  else
{$ENDIF}
    ScaleForPPI(ATargetDPI, nil);
end;

procedure TdxForm.ChangeScale(M, D: Integer);
begin
  ChangeScale(M, D, False);
end;

procedure TdxForm.BeforeDestruction;
begin
  inherited BeforeDestruction;
  cxBroadcastRemoveNotifications(Self);
end;

procedure TdxForm.ChangeScale(M, D: Integer; IsDpiChange: Boolean);
begin
  ScaleFactor.Change(M, D);
  inherited ChangeScale(M, D{$IFDEF DELPHI101BERLIN}, IsDpiChange{$ENDIF});
  TdxFormHelper.ScaleComponentsForDpi(Self, MulDiv(dxGetFormDPI(Self), M, D));
  ScaleFactorChanged(M, D);
end;

procedure TdxForm.CMBiDiModeChanged(var Message: TMessage);
begin
  inherited;
  if FRightToLeftApplied <> IsRightToLeftLayout then
    if fsShowing in FFormState then
      TdxUIThreadSyncService.EnqueueInvokeInUIThread(Self, RightToLeftLayoutChanged)
    else
      RightToLeftLayoutChanged;
end;

procedure TdxForm.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  if FRightToLeftApplied then
    TdxFormRightToLeftLayoutHelper.AddRightToLeftFormExStyle(Params);
end;

procedure TdxForm.CreateWnd;
begin
  inherited CreateWnd;
  if FormStyle = fsMDIForm then
    if FRightToLeftApplied then
      dxSetWindowStyle(ClientHandle, WS_EX_LAYOUTRTL, soAdd, wsiExStyle);
end;

procedure TdxForm.DefineProperties(Filer: TFiler);
begin
{$IFNDEF DELPHIXE8}
  Filer.DefineProperty('TextHeight', ReadTextHeight, WriteTextHeight, not IsControl);
{$ENDIF}
  inherited DefineProperties(Filer);
end;

procedure TdxForm.DoShow;
begin
  inherited DoShow;
  ScaleForCurrentDpi;
end;

function TdxForm.GetClientOrigin: TPoint;
begin
  Result := inherited GetClientOrigin;
  if FRightToLeftApplied then
    Dec(Result.X, ClientWidth);
end;

procedure TdxForm.InitializeNewForm;
begin
  inherited;
{$IFDEF DELPHI101BERLIN}
  FCurrentPPI := PixelsPerInch;
{$ELSE}
  Scaled := True;
{$ENDIF}
  FScaleFactor := TdxScaleFactor.Create;
  FScaleFactor.Assign(dxGetFormDPI(Self), dxDefaultDPI);
  FRightToLeftLayout := bDefault;
end;

procedure TdxForm.ScaleFactorChanged(M, D: Integer);
begin
  UpdateImageLists;
end;

procedure TdxForm.UpdateImageLists;
begin
  if dxAutoReplaceImageListReferencesOnDPIChanges then
    TdxFormImageListHelper.Execute(ScaleFactor.TargetDPI, Self);
end;

{$IFDEF DELPHI101BERLIN}
procedure TdxForm.ScaleControlsForDpi(NewPPI: Integer);
var
  ACurPPI: Integer;
begin
  DisableAlign;
  try
    ACurPPI := dxGetFormDPI(Self);
    ScaleFactor.Change(NewPPI, ACurPPI);
    inherited ScaleControlsForDpi(NewPPI);
    TdxFormHelper.ScaleComponentsForDpi(Self, NewPPI);
    ScaleFactorChanged(NewPPI, ACurPPI);
  finally
    EnableAlign;
  end;
end;
{$ENDIF}

procedure TdxForm.ScaleForCurrentDpi;
begin
  DisableAlign;
  try
    if Scaled and not (csDesigning in ComponentState) and (Parent = nil) then
      ScaleForPPI(GetTargetDPI);
    TdxFormHelper.ResetScalingFlags(Self);
    Perform(CM_PARENTBIDIMODECHANGED, 0, 0);
  finally
    EnableAlign;
  end;
end;

{$IFNDEF DELPHI101BERLIN}
procedure TdxForm.Loaded;
begin
  ScaleForCurrentDpi;
  inherited Loaded;
end;
{$ENDIF}

procedure TdxForm.ReadState(Reader: TReader);

  procedure WinControlReadState;
  var
    AProc: procedure (Reader: TReader) of object;
  begin
    TMethod(AProc).Code := @TScrollingWinControlAccess.ReadState;
    TMethod(AProc).Data := Self;
    AProc(Reader);
  end;

begin
  DisableAlign;
  try
  {$IFNDEF DELPHIXE8}
    FTextHeight := 0;
  {$ENDIF}
    FLoadedClientHeight := 0;
    FLoadedClientWidth := 0;
    if ClassParent = TForm then
      OldCreateOrder := not ModuleIsCpp;
    WinControlReadState;
    if FLoadedClientHeight > 0 then
      inherited ClientHeight := FLoadedClientHeight;
    if FLoadedClientWidth >  0 then
      inherited ClientWidth := FLoadedClientWidth;
  finally
    EnableAlign;
  end;
end;

procedure TdxForm.SetParent(AParent: TWinControl);
begin
  if TcxControlHelper.CanSetParent(Self, AParent) then
  begin
    inherited SetParent(AParent);
    TdxFormHelper.UpdateScaleFactorOnParentChange(Self);
  end;
end;

{$IFNDEF DELPHIXE8}
function TdxForm.GetTextHeight: Integer;
begin
  Result := Canvas.TextHeight('0');
end;

procedure TdxForm.ReadTextHeight(Reader: TReader);
begin
  FTextHeight := Reader.ReadInteger;
end;

procedure TdxForm.WriteTextHeight(Writer: TWriter);
begin
  Writer.WriteInteger(GetTextHeight);
end;
{$ENDIF}

function TdxForm.IsRightToLeftLayout: Boolean;
begin
  Result := (FRightToLeftLayout = bTrue) or (FRightToLeftLayout = bDefault) and UseRightToLeftAlignment;
end;

procedure TdxForm.RightToLeftLayoutChanged;
begin
  FRightToLeftApplied := IsRightToLeftLayout;
  if FRightToLeftApplied then
    TdxFormRightToLeftLayoutHelper.ResetMenusBiDiMode(Self)
  else
    TdxFormRightToLeftLayoutHelper.SetMenusParentBiDiMode(Self);
  RecreateWnd;
end;

procedure TdxForm.SetClientHeight(Value: Integer);
begin
  if csReadingState in ControlState then
  begin
    FLoadedClientHeight := Value;
    ScalingFlags := ScalingFlags + [sfHeight];
  end;
  inherited ClientHeight := Value;
end;

procedure TdxForm.SetClientWidth(Value: Integer);
begin
  if csReadingState in ControlState then
  begin
    FLoadedClientWidth := Value;
    ScalingFlags := ScalingFlags + [sfWidth];
  end;
  inherited ClientWidth := Value;
end;

procedure TdxForm.SetPixelsPerInch(Value: Integer);
begin
  if csReadingState in ControlState then
  begin
    TdxScaleFactorAccess(FScaleFactor).Assign(Value, dxDefaultDPI, True);
  {$IFDEF DELPHI101BERLIN}
    FCurrentPPI := Value;
  {$ENDIF}
  end;
  inherited PixelsPerInch := Value;
end;

procedure TdxForm.SetRightToLeftLayout(const AValue: TdxDefaultBoolean);
begin
  if FRightToLeftLayout <> AValue then
  begin
    FRightToLeftLayout := AValue;
    if FRightToLeftApplied <> IsRightToLeftLayout then
      RightToLeftLayoutChanged;
  end;
end;

procedure TdxForm.WMDPIChanged(var Message: TMessage);
var
  ATargetDPI: Integer;
{$IF DEFINED(DELPHI10SEATTLE) AND NOT DEFINED(DELPHI101BERLIN)}
  APrevPixelsPerInch: Integer;
{$IFEND}
begin
  if TdxFormHelper.IsScaleChanging(Self) then
  begin
    inherited;
    Exit;
  end;

  if [csDesigning, csLoading] * ComponentState = [] then
  begin
    ATargetDPI := LoWord(Message.WParam);
    if (ATargetDPI = 0) or not Scaled then
    begin
      if (Application.MainForm <> nil) and Application.MainForm.Scaled then
        PixelsPerInch := Application.MainForm.PixelsPerInch
      else
        Exit;
    end;

    if (ATargetDPI <> dxGetFormDPI(Self)) and Scaled then
    begin
    {$IF DEFINED(DELPHI10SEATTLE) AND NOT DEFINED(DELPHI101BERLIN)}
      if Assigned(OnBeforeMonitorDpiChanged) then
        OnBeforeMonitorDpiChanged(Self, PixelsPerInch, ATargetDPI);
      APrevPixelsPerInch := PixelsPerInch;
    {$IFEND}
     TdxFormHelper.ScaleForPPI(Self, ATargetDPI, PRect(Message.LParam), True);
    {$IF DEFINED(DELPHI10SEATTLE) AND NOT DEFINED(DELPHI101BERLIN)}
      if Assigned(OnAfterMonitorDpiChanged) then
        OnAfterMonitorDpiChanged(Self, APrevPixelsPerInch, PixelsPerInch);
    {$IFEND}
    end;
    Message.Result := 0;
  end;
end;

function TdxForm.GetScaleFactor: TdxScaleFactor;
begin
  Result := FScaleFactor;
end;

function TdxForm.GetTargetDPI: Integer;
begin
  Result := dxGetTargetDPI(Self);
end;

{$IFNDEF DELPHI102TOKYO}
procedure TdxForm.WMNCCreate(var Message: TWMNCCreate);
begin
  inherited;
  if Assigned(FEnableNonClientDpiScaling) and Scaled and dxIsProcessDPIAware then
    FEnableNonClientDpiScaling(WindowHandle);
end;
{$ENDIF}

{ TdxFormHelper }

class function TdxFormHelper.SetAnimation(AValue: Boolean): Boolean;
var
  AInfo: TAnimationInfo;
begin
  ZeroMemory(@AInfo, SizeOf(AInfo));
  AInfo.cbSize := SizeOf(TAnimationInfo);
  SystemParametersInfo(SPI_GETANIMATION, SizeOf(AInfo), @AInfo, 0);
  Result := AInfo.iMinAnimate <> 0;
  if Result <> AValue then
  begin
    BOOL(AInfo.iMinAnimate) := AValue;
    SystemParametersInfo(SPI_SETANIMATION, SizeOf(AInfo), @AInfo, 0);
  end;
end;

class procedure TdxFormHelper.SetWindowTextWithoutRedrawing(AWndHandle: THandle; const AText: string);
var
  AStyle: Cardinal;
begin
  AStyle := LockWindowRedrawing(AWndHandle);
  try
    DefWindowProc(AWndHandle, WM_SETTEXT, 0, LPARAM(PChar(AText)));
  finally
    UnlockWindowRedrawing(AWndHandle, AStyle);
  end;
end;

class function TdxFormHelper.LockWindowRedrawing(AWndHandle: THandle): Cardinal;
begin
  if dxSystemInfo.IsRemoteSession then
    Result := dxSetWindowStyle(AWndHandle, 0, soAdd)
  else
    Result := dxSetWindowStyle(AWndHandle, WS_VISIBLE, soSubtract);
end;

class procedure TdxFormHelper.UnlockWindowRedrawing(AWndHandle: THandle; APrevStyle: Cardinal);
begin
  dxSetWindowStyle(AWndHandle, APrevStyle, soSet);
end;

class procedure TdxFormHelper.UpdateScaleFactorOnParentChange(AForm: TCustomForm);
{$IFNDEF DELPHI101BERLIN}
var
  ADenominator: Integer;
  ANumerator: Integer;
  ASource: IdxScaleFactor;
{$ENDIF}
begin
{$IFDEF DELPHI101BERLIN}
  TcxControlHelper.UpdateScaleFactorOnParentChange(AForm);
{$ELSE}
  if dxGetCurrentScaleFactor(AForm.Parent, ANumerator, ADenominator) then
  begin
    if not (csLoading in AForm.ComponentState) and Supports(AForm, IdxScaleFactor, ASource) then
    begin
      TCustomFormAccess(AForm).PixelsPerInch := MulDiv(
        TCustomFormAccess(AForm).PixelsPerInch,
        ANumerator * ASource.Value.Denominator,
        ADenominator * ASource.Value.Numerator);
    end;
    TcxControlHelper.ChangeScaleFactor(AForm, ANumerator, ADenominator);
  end;
{$ENDIF}
end;

class function TdxFormHelper.IsScaleChanging(AContainer: TWinControl): Boolean;
begin
  Result := False;
  if FScalingContainers <> nil then
    while AContainer <> nil do
    begin
      if FScalingContainers.IndexOf(AContainer) >= 0 then
        Exit(True);
      AContainer := AContainer.Parent;
    end;
end;

class procedure TdxFormHelper.RepaintVisibleWindowArea(AHandle: THandle; var APrevVisibleRegion: TcxRegionHandle);

  function GetVisibleRegion: TcxRegionHandle;
  var
    AScreenRegion: HRGN;
  begin
    Result := CreateRectRgnIndirect(cxGetWindowRect(AHandle));
    AScreenRegion := CreateRectRgnIndirect(GetMonitorWorkArea(MonitorFromWindow(AHandle, MONITOR_DEFAULTTONEAREST)));
    CombineRgn(Result, Result, AScreenRegion, RGN_AND);
    OffsetRgn(Result, -cxGetWindowRect(AHandle).Left, -cxGetWindowRect(AHandle).Top);
    DeleteObject(AScreenRegion);
  end;

var
  ARegion: TcxRegionHandle;
begin
  ARegion := GetVisibleRegion;
  try
    if not EqualRgn(ARegion, APrevVisibleRegion) then
    begin
      ExchangePointers(ARegion, APrevVisibleRegion);
      if CombineRgn(ARegion, APrevVisibleRegion, ARegion, RGN_DIFF) <> NULLREGION then
        RedrawWindow(AHandle, nil, ARegion, RDW_ERASE or RDW_INVALIDATE or RDW_FRAME or RDW_ALLCHILDREN);
    end;
  finally
    DeleteObject(ARegion);
  end;
end;

class procedure TdxFormHelper.ResetScalingFlags(AControl: TControl);
var
  I: Integer;
begin
  TControlAccess(AControl).ScalingFlags := [];
  if AControl is TWinControl then
  begin
    for I := 0 to TWinControl(AControl).ControlCount - 1 - 1 do
      ResetScalingFlags(TWinControl(AControl).Controls[I]);
  end;
end;

class procedure TdxFormHelper.ScaleComponentsForDpi(AOwner: TComponent; NewPPI: Integer);
var
  AIntf: IcxScalableComponent;
  I: Integer;
begin
  for I := 0 to AOwner.ComponentCount - 1 do
  begin
    if Supports(AOwner.Components[I], IcxScalableComponent, AIntf) then
      AIntf.ScaleForPPI(NewPPI);
  end;
end;

class procedure TdxFormHelper.ScaleForPPI(Form: TCustomForm; TargetDPI: Integer; WindowRect: PRect; FromMessageHandler: Boolean);

  function IsClientSizeStored(AForm: TCustomFormAccess): Boolean;
  begin
    Result := not (AForm.AutoScroll or (AForm.HorzScrollBar.Range <> 0) or (AForm.VertScrollBar.Range <> 0));
  end;

var
  AForm: TCustomFormAccess;
  APrevBounds: TRect;
  APrevClientRect: TRect;
  APrevDPI: Integer;
  APrevParentFont: Boolean;
  AState: TState;
{$IFNDEF DELPHI101BERLIN}
  I: Integer;
{$ENDIF}
begin
  APrevDPI := dxGetFormDPI(Form);
  if (TargetDPI <> APrevDPI) and (TargetDPI >= dxMinDPI) and not IsScaleChanging(Form) then
  begin
    AForm := TCustomFormAccess(Form);
    ScalingBegin(AForm, AState);
    try
      APrevBounds := AForm.BoundsRect;
      APrevClientRect := AForm.ClientRect;
      APrevParentFont := AForm.ParentFont;
    {$IFDEF DELPHI101BERLIN}
      if (WindowRect <> nil) and FromMessageHandler then
        AForm.Perform(WM_DPICHANGED, MakeWParam(TargetDPI, TargetDPI), LParam(WindowRect))
      else
        AForm.ScaleForPPI(TargetDPI);
    {$ELSE}
      AForm.ChangeScale(TargetDPI, APrevDPI);
      for I := 0 to AForm.MDIChildCount - 1 do
        AForm.MDIChildren[I].ScaleBy(TargetDPI, APrevDPI);
    {$ENDIF}
      AForm.ParentFont := APrevParentFont;
      AForm.PixelsPerInch := TargetDPI;
      if WindowRect <> nil then
        AForm.BoundsRect := WindowRect^
      else
        if IsClientSizeStored(AForm) then
        begin
          if AForm.WindowState <> wsMaximized then
          begin
            AForm.SetBounds(APrevBounds.Left, APrevBounds.Top,
              cxRectWidth(APrevBounds) - APrevClientRect.Right + MulDiv(APrevClientRect.Right, TargetDPI, APrevDPI),
              cxRectHeight(APrevBounds) - APrevClientRect.Bottom + MulDiv(APrevClientRect.Bottom, TargetDPI, APrevDPI));
          end
          else
            AForm.BoundsRect := APrevBounds;
        end;
    finally
      ScalingEnd(AForm, AState);
    end;
  end;
end;

class procedure TdxFormHelper.PopulateControls(AControl: TControl; ATargetList: TComponentList);
var
  I: Integer;
begin
  ATargetList.Add(AControl);
  if AControl is TCustomForm then
  begin
    for I := 0 to TCustomFormAccess(AControl).MDIChildCount - 1 do
      PopulateControls(TCustomFormAccess(AControl).MDIChildren[I], ATargetList);
  end;
  if AControl is TWinControl then
  begin
    for I := 0 to TWinControl(AControl).ControlCount - 1 do
      PopulateControls(TWinControl(AControl).Controls[I], ATargetList);
  end;
end;

class procedure TdxFormHelper.ScalingBegin(AContainer: TWinControl; out AState: TState);
var
  I: Integer;
begin
  if FScalingContainers = nil then
    FScalingContainers := TList.Create;
  FScalingContainers.Add(AContainer);

  AState := TState.Create;
  AState.RedrawLocked := IsWinVistaOrLater and IsWindowVisible(AContainer.Handle);
  AState.LockedControls := TComponentList.Create(False);
  PopulateControls(AContainer, AState.LockedControls);
  AContainer.Perform(DXM_SCALECHANGING, 0, 0);
  for I := 0 to AState.LockedControls.Count - 1 do
    TControl(AState.LockedControls[I]).Perform(DXM_SCALECHANGING, 0, 0);
  if AState.RedrawLocked then
    SendMessage(AContainer.Handle, WM_SETREDRAW, 0, 0);
  AContainer.DisableAlign;
end;

class procedure TdxFormHelper.ScalingEnd(AContainer: TWinControl; AState: TState);
var
  I: Integer;
begin
  AContainer.EnableAlign;
  AContainer.Realign;
  if AState.RedrawLocked then
    SendMessage(AContainer.Handle, WM_SETREDRAW, 1, 0);
  for I := AState.LockedControls.Count - 1 downto 0 do
    TControl(AState.LockedControls[I]).Perform(DXM_SCALECHANGED, 0, 0);
  AContainer.Perform(DXM_SCALECHANGED, 0, 0);
  if AState.RedrawLocked then
    cxRedrawWindow(AContainer.Handle, RDW_INVALIDATE or RDW_ALLCHILDREN or RDW_ERASE);

  FScalingContainers.Remove(AContainer);
  if FScalingContainers.Count = 0 then
    FreeAndNil(FScalingContainers);
  FreeAndNil(AState.LockedControls);
  FreeAndNil(AState);
end;

{ TdxFormImageListHelper }

constructor TdxFormImageListHelper.Create(ATargetDPI: Integer);
begin
  FTargetDPI := ATargetDPI;
  FReplacementCache := TDictionary<TObject, TObject>.Create;
end;

destructor TdxFormImageListHelper.Destroy;
begin
  FreeAndNil(FReplacementCache);
  inherited Destroy;
end;

class procedure TdxFormImageListHelper.Execute(ATargetDPI: Integer; AForm: TCustomForm);
begin
  with TdxFormImageListHelper.Create(ATargetDPI) do
  try
    UpdateImageLists(AForm);
  finally
    Free;
  end;
end;

procedure TdxFormImageListHelper.UpdateImageList(AInstance: TObject; APropInfo: PPropInfo; APropValue: TObject);
var
  ANewValue: TObject;
begin
  if not FReplacementCache.TryGetValue(APropValue, ANewValue) then
  begin
    ANewValue := GetReplacement(TCustomImageList(APropValue));
    FReplacementCache.Add(APropValue, ANewValue);
  end;
  if APropValue <> ANewValue then
    SetObjectProp(AInstance, APropInfo, ANewValue);
end;

procedure TdxFormImageListHelper.UpdateImageListProperties(APersistent: TPersistent);

  function EnumProperties(AObject: TObject; out AList: PPropList; out ACount: Integer): Boolean;
  begin
    Result := False;
    if AObject <> nil then
    begin
      ACount := GetTypeData(AObject.ClassInfo)^.PropCount;
      Result := ACount > 0;
      if Result then
      begin
        AList := AllocMem(ACount * SizeOf(Pointer));
        GetPropInfos(AObject.ClassInfo, AList);
      end;
    end;
  end;

var
  AProperties: PPropList;
  APropertyCount: Integer;
  APropInfo: PPropInfo;
  APropValue: TObject;
  I: Integer;
begin
  if EnumProperties(APersistent, AProperties, APropertyCount) then
  try
    for I := 0 to APropertyCount - 1 do
    begin
      APropInfo := AProperties^[I];
      if APropInfo.PropType^.Kind = tkClass then
      begin
        APropValue := GetObjectProp(APersistent, APropInfo);
        if APropValue is TComponent then
        begin
          if APropValue is TCustomImageList then
            UpdateImageList(APersistent, APropInfo, APropValue);
        end
        else
          if APropValue is TPersistent then
            UpdateImageListProperties(TPersistent(APropValue));
      end;
    end;
  finally
    FreeMem(AProperties);
  end;
end;

procedure TdxFormImageListHelper.UpdateImageLists(AForm: TCustomForm);
var
  I: Integer;
begin
  for I := 0 to AForm.ComponentCount - 1 do
    UpdateImageListProperties(AForm.Components[I]);
end;

function TdxFormImageListHelper.GenerateName(const ABaseName: TComponentName; ATargetDPI: Integer): TComponentName;
begin
  Result := Format('%s%d', [ABaseName, ATargetDPI]);
end;

function TdxFormImageListHelper.GetBaseImageListName(const AName: TComponentName): TComponentName;
var
  ALength: Integer;
begin
  Result := AName;
  ALength := Length(Result);
  while (ALength > 0) and dxCharIsNumeric(Result[ALength]) do
    Dec(ALength);
  SetLength(Result, ALength);
end;

function TdxFormImageListHelper.GetReplacement(AImageList: TCustomImageList): TCustomImageList;

  function CheckReference(const AReference: TComponent; var AResult: TCustomImageList): Boolean;
  begin
    Result := AReference is TCustomImageList;
    if Result then
      AResult := TCustomImageList(AReference);
  end;

  function TryFind(const ABaseName: TComponentName; ATargetDPI: Integer; var AResult: TCustomImageList): Boolean;
  begin
    Result := CheckReference(AImageList.Owner.FindComponent(GenerateName(ABaseName, ATargetDPI)), AResult);
    if not Result and (ATargetDPI = dxDefaultDPI) then
      Result := CheckReference(AImageList.Owner.FindComponent(ABaseName), AResult);
  end;

var
  ABaseName: TComponentName;
  I: Integer;
begin
  Result := AImageList;

  ABaseName := GetBaseImageListName(AImageList.Name);
  if (ABaseName <> '') and (AImageList.Owner <> nil) then
  begin
    if not TryFind(ABaseName, FTargetDPI, Result)  then
      for I := High(dxDefaultDPIValues) downto Low(dxDefaultDPIValues) do
      begin
        if (dxDefaultDPIValues[I] < FTargetDPI) and TryFind(ABaseName, dxDefaultDPIValues[I], Result) then
          Break;
      end;
  end;
end;

{ TdxFormRightToLeftLayoutHelper }

class procedure TdxFormRightToLeftLayoutHelper.AddRightToLeftFormExStyle(var Params: TCreateParams);
begin
  Params.ExStyle := (Params.ExStyle or GetRightToLeftLayoutStyles) and not GetBiDiModeStyles;
end;

class procedure TdxFormRightToLeftLayoutHelper.RemoveBiDiModeStyles(AForm: TCustomForm);
begin
  if AForm.HandleAllocated then
    dxSetWindowStyle(AForm.Handle, GetBiDiModeStyles, soSubtract, wsiExStyle);
end;

class procedure TdxFormRightToLeftLayoutHelper.SetMenusParentBiDiMode(AForm: TCustomForm);
var
  I: Integer;
begin
  for I := 0 to AForm.ComponentCount - 1 do
    if AForm.Components[I] is TMenu then
      TMenu(AForm.Components[I]).ParentBiDiMode := True;
end;

class procedure TdxFormRightToLeftLayoutHelper.ResetMenusBiDiMode(AForm: TCustomForm);
var
  I: Integer;
begin
  for I := 0 to AForm.ComponentCount - 1 do
    if AForm.Components[I] is TMenu then
      TMenu(AForm.Components[I]).BiDiMode := bdLeftToRight;
end;

class function TdxFormRightToLeftLayoutHelper.GetBiDiModeStyles: Cardinal;
begin
  Result := WS_EX_LEFTSCROLLBAR or WS_EX_RIGHT or WS_EX_RTLREADING;
end;

class function TdxFormRightToLeftLayoutHelper.GetRightToLeftLayoutStyles: Cardinal;
begin
  Result := WS_EX_LAYOUTRTL or WS_EX_NOINHERITLAYOUT;
end;

initialization
  @FEnableNonClientDpiScaling := GetProcAddress(GetModuleHandle(user32), 'EnableNonClientDpiScaling');
  dxSetHook(htWndProc, dxFrameWndProcHook);

finalization
  dxReleaseHook(dxFrameWndProcHook);
end.
