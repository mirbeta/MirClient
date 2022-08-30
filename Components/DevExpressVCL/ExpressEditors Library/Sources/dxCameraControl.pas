{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressEditors                                           }
{                                                                    }
{           Copyright (c) 1998-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSEDITORS AND ALL                }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY. }
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

unit dxCameraControl;

{$I cxVer.inc}

interface

uses
  Types, Windows, Messages, SysUtils, Classes, Controls, ActiveX, Contnrs, Menus, Forms, Graphics,
  DirectShow9, {$NOINCLUDE DirectShow9}
  dxCore, dxCoreGraphics, dxGDIPlusClasses, dxMessages, cxGeometry, cxClasses, cxControls, cxGraphics, cxLookAndFeels,
  cxLookAndFeelPainters, dxForms;

(*$HPPEMIT '#include <dxMediaUtils.hpp>	// Pascal unit'*)

const
  DXM_GRAPHNOTIFY = WM_DX + 7;

type
  TdxCameraRenderingMode = (rmNull, rmWindowLess, rmWindowed);

  TdxCamera = class;
  TdxCustomCameraControl = class;

  { TdxCameraEventWindow }

  TdxCameraEventWindow = class(TcxMessageWindow)
  private
    FCamera: TdxCamera;
  protected
    procedure WndProc(var Message: TMessage); override;
  end;

  { TdxCameraControlSettingsToolbarItem }

  TdxCameraState = (csInitializing, csRunning, csDeviceIsBusy);

  TdxCameraControlSettingsToolbarItem = class
  private
    FBounds: TRect;
    FCameraControl: TdxCustomCameraControl;
    FCaption: string;
    FChecked: Boolean;
    FParentHandle: THandle;
    FState: TcxEditCheckState;
    FDescription: string;

    procedure SetChecked(AChecked: Boolean);
    procedure SetState(AState: TcxEditCheckState);
  protected
    procedure Draw(ACanvas: TcxCanvas);
    procedure DrawBackground(ACanvas: TcxCanvas);
    procedure DrawText(ACanvas: TcxCanvas);

    property Checked: Boolean read FChecked write SetChecked;
    property State: TcxEditCheckState read FState write SetState;
  end;

  { TdxCameraControlSettingsToolbar }

  TdxCameraControlSettingsToolbarPage = (stpSettings, stpDevices, stpResolutions);

  TdxCameraControlSettingsToolbar = class(TdxCustomFloatForm)
  private
    FCameraControl: TdxCustomCameraControl;
    FColumnCount: Integer;
    FcxCanvas: TcxCanvas;
    FHotItem: TdxCameraControlSettingsToolbarItem;
    FItems: TObjectList;
    FToolbarPage: TdxCameraControlSettingsToolbarPage;

    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure DXMScaleChanged(var Message: TMessage); message DXM_SCALECHANGED;

    function GetItems(AIndex: Integer): TdxCameraControlSettingsToolbarItem;
    procedure SetHotItem(AValue: TdxCameraControlSettingsToolbarItem);
  protected
    procedure CreateWindowHandle(const Params: TCreateParams); override;
    procedure Paint; override;

    procedure DoShow; override;

    procedure MouseLeave;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

    procedure CalculateItemsBounds(const ARect: TRect);
    procedure ClickItem(AItemIndex: Integer);
    function CreateItem(const ACaption, ADescription: string): TdxCameraControlSettingsToolbarItem;
    procedure DrawItems(ACanvas: TcxCanvas);
    function GetCheckedItemIndex: Integer;
    function GetItemIndexAtPoint(const P: TPoint): Integer;

    // initialize
    procedure InitDevicesPage;
    procedure Initialize(AToolbarPage: TdxCameraControlSettingsToolbarPage);
    procedure InitResolutionsPage;
    procedure InitSettingsPage;
    procedure Reinitilize;

    // misc
    function GetNewBounds: TRect;
    function GetContentRectSize: TSize;

    // IdxFloatForm
    function GetParentForm: TCustomForm; override;

    property HotItem: TdxCameraControlSettingsToolbarItem read FHotItem write SetHotItem;
    property Items[Index: Integer]: TdxCameraControlSettingsToolbarItem read GetItems;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TdxCamera = class(TcxCustomComponent)
  private
    // Graph
    FBuilder: ICaptureGraphBuilder2;
    FGraph: IGraphBuilder;
    FCameraFilter: IBaseFilter;
    FGrabberFilter: IBaseFilter;
    FSampleGrabber: ISampleGrabber;
    FStreamConfig: IAMStreamConfig;
    FRenderer: IBaseFilter;
    FMux: IBaseFilter;

    // Media
    FMediaControl: IMediaControl;
    FMediaEvent: IMediaEventEx;
    FEventWindow: TdxCameraEventWindow;
    FVideoWindow: IVideoWindow;
    FWindowlessControl: IVMRWindowlessControl;

    // Device
    FDeviceIndex: Integer;
    FDeviceName: OLEvariant;
    FMediaTypes: TList;
    FMediaTypeIndex: Integer;
    FMoniker: IMoniker;

    // Listeners
    FPreviewBitmap: TcxBitmap32;
    FTimer: TcxTimer;
    FListeners: TComponentList;

    // Flags
    FInitialized: Boolean;
    FRecording: Boolean;
    FRendered: Boolean;
    FConnected: Boolean;
    FIsRunning: Boolean;
    FFailsCount: Integer;
    FState: TdxCameraState;

    // Events
    FOnDeviceLost: TNotifyEvent;
    FOnDeviceNotUsed: TNotifyEvent;
    FStateChangedHandlers: TcxEventHandlerCollection;

    procedure OnTimer(Sender: TObject);
    procedure NotifyListeners;

    function InitializeGraph: Boolean;
    function InitializeMedia: Boolean;
    function InitializeDevice(ADeviceIndex: Integer): Boolean;
    function InitializeCaptureGrabber: Boolean;
    function InitializeStreamConfig: Boolean;

    function InitializeNullRenderer: Boolean;
    function InitializeWindowlessVMR(AHandle: THandle; const R: TRect): Boolean;
    function InitializeWindowedVMR(AHandle: THandle; const R: TRect): Boolean;

    procedure ApplySelectedMediaType;
    function ConnectGraph(const AFileName: string = ''): Boolean;
    function DisconnectGraph: Boolean;
    procedure FindMediaTypes;
    procedure SetConnected(AValue: Boolean);
    procedure SetMediaTypeIndex(AValue: Integer);
  protected
    procedure WndProc(var Message: TMessage);

    function Capture(ABitmap: TcxBitmap32): Boolean;
    procedure CheckState;
    function Initialize(ADeviceIndex: Integer): Boolean;
    procedure Finalize;
    function GetCurrentMediaType: TAMMediaType;
    procedure Reinitialize;
    function RenderStream(ARenderingMode: TdxCameraRenderingMode; AHandle: THandle; const R: TRect; const AOutputFileName: string): Boolean;
    procedure SetVideoPosition(const R: TRect);

    function Play(AControl: TdxCustomCameraControl): Boolean;
    procedure Stop(AControl: TdxCustomCameraControl);

    function StartRecording(const AFileName: string): Boolean;
    procedure StopRecording;

    property PreviewBitmap: TcxBitmap32 read FPreviewBitmap;
    property Connected: Boolean read FConnected write SetConnected;
    property Initialized: Boolean read FInitialized;
    property MediaTypes: TList read FMediaTypes;
    property MediaTypeIndex: Integer read FMediaTypeIndex write SetMediaTypeIndex;
    property State: TdxCameraState read FState;

    property OnDeviceLost: TNotifyEvent read FOnDeviceLost write FOnDeviceLost;
    property OnDeviceNotUsed: TNotifyEvent read FOnDeviceNotUsed write FOnDeviceNotUsed;
    property StateChangedHandlers: TcxEventHandlerCollection read FStateChangedHandlers;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  { TdxCameraManager }

  TdxCameraManager = class
  private
    FDevEnum: ICreateDevEnum;
    FDevices: TStrings;
    FCameraList: TComponentList;
    FRequestList: TComponentList;
    FRequestTimer: TcxTimer;

    procedure RequestTimer(Sender: TObject);
    procedure DeviceLost(Sender: TObject);
  protected
    procedure RefreshDeviceList;

    function GetDevice(AIndex: Integer; out AMoniker: IMoniker): Boolean;
    function GetDeviceCount: Integer;

    procedure AddRequest(AControl: TdxCustomCameraControl);
    procedure CancelRequest(AControl: TdxCustomCameraControl);

    function GetCamera(ADeviceIndex: Integer): TdxCamera;

    property Devices: TStrings read FDevices;
  public
    constructor Create;
    destructor Destroy; override;

    function StartRecording(ADeviceIndex: Integer; const AFileName: string): Boolean;
    procedure StopRecording(ADeviceIndex: Integer);
  end;

  { TdxCustomCameraControl }

  TdxCameraControlState = (ccsInactive, ccsInitializing, ccsRunning, ccsPaused, ccsNoDevice, ccsDeviceIsBusy);

  TdxCustomCameraControl = class(TcxControl, IdxSkinSupport)
  private
    FCamera: TdxCamera;
    FCapturedBitmap: TcxBitmap32;
    FFitMode: TcxImageFitMode;
    FDeviceIndex: Integer;
    FSettingsButtonAlpha: Integer;
    FSettingsButtonGlyph: TdxGPImage;
    FSettingsButtonShowingIncrement: Integer;
    FSettingsButtonShowingTimer: TcxTimer;
    FShowSettingsButton: Boolean;
    FState: TdxCameraControlState;

    FActive: Boolean;
    FPaused: Boolean;

    FOnStateChanged: TNotifyEvent;

    function GetDisplayText: string;

    procedure SetActive(AValue: Boolean);
    procedure SetCamera(AValue: TdxCamera);
    procedure SetFitMode(AValue: TcxImageFitMode);
    function GetCurrentMediaType: TAMMediaType;
    function GetDeviceName: string;
    function GetResolutionCount: Integer;
    function GetResolutionIndex: Integer;
    function GetResolutions(AIndex: Integer): TSize;
    function GetMediaTypeIndex: Integer;
    function GetMediaTypes: TList;
    procedure SetDeviceIndex(Value: Integer);
    procedure SetResolutionIndex(AValue: Integer);
    procedure SetMediaTypeIndex(AValue: Integer);
    procedure SetShowSettingsButton(AValue: Boolean);

    procedure CameraStateChanged(Sender: TObject; const AEventArgs);
    procedure DrawSettingsButton;
    function GetSettingsButtonRect: TRect;
    procedure SettingsButtonShowingTimer(Sender: TObject);
  protected
    // Input
    procedure Click; override;
    procedure MouseEnter(AControl: TControl); override;
    procedure MouseLeave(AControl: TControl); override;
    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;

    // TCustomControl
    procedure Paint; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    // TcxControl
    function IsDoubleBufferedNeeded: Boolean; override;
    procedure LookAndFeelChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues); override;

    procedure CameraPictureChanged;
    procedure CheckState;

    property Camera: TdxCamera read FCamera write SetCamera;
    property CurrentMediaType: TAMMediaType read GetCurrentMediaType;
    property DeviceIndex: Integer read FDeviceIndex write SetDeviceIndex;
    property FitMode: TcxImageFitMode read FFitMode write SetFitMode;
    property MediaTypes: TList read GetMediaTypes;
    property MediaTypeIndex: Integer read GetMediaTypeIndex write SetMediaTypeIndex;

    property OnStateChanged: TNotifyEvent read FOnStateChanged write FOnStateChanged;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Capture;
    procedure Pause;
    procedure Play;
    procedure Stop;

    procedure ShowSettingsToolbar;

    property Active: Boolean read FActive write SetActive;
    property CapturedBitmap: TcxBitmap32 read FCapturedBitmap;
    property DeviceName: string read GetDeviceName;
    property ResolutionCount: Integer read GetResolutionCount;
    property ResolutionIndex: Integer read GetResolutionIndex write SetResolutionIndex;
    property Resolutions[AIndex: Integer]: TSize read GetResolutions;
    property ShowSettingsButton: Boolean read FShowSettingsButton write SetShowSettingsButton;
    property State: TdxCameraControlState read FState;
  end;

  TdxCameraControl = class(TdxCustomCameraControl)
  published
    property DeviceIndex default 0;
    property Active default False;
    property Align;
    property Anchors;
    property BiDiMode;
    property BorderStyle default cxcbsDefault;
    property Enabled;
    property FitMode default ifmProportionalStretch;
    property Font;
    property LookAndFeel;
    property ParentBiDiMode;
    property PopupMenu;
    property ShowSettingsButton default True;
    property Visible;

    property OnContextPopup;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStateChanged;
  end;


function dxCameraManager: TdxCameraManager;
function dxIsCameraAvailable: Boolean;

implementation

{$R dxCameraControlRes.res}

uses
  Math,
  dxHooks, cxEditConsts;

var
  FCameraManager: TdxCameraManager;
  FSettingsToolbar: TdxCameraControlSettingsToolbar;


procedure dxSettingsToolbarMouseHook(ACode: Integer; wParam: WPARAM; lParam: LPARAM; var AHookResult: LRESULT);
var
  AMHS: PMouseHookStruct;
begin
  if FSettingsToolbar <> nil then
  begin
    AMHS := PMouseHookStruct(lParam);
    if IsMouseDownMessage(wParam) and not PtInRect(FSettingsToolbar.BoundsRect, AMHS.pt) then
      FSettingsToolbar.Release;
  end;
end;

procedure dxSettingsToolbarWndProcHook(ACode: Integer; AWParam: WPARAM; ALParam: LPARAM; var AHookResult: LRESULT);

  function IsAnotherApplicatonWindow(AWnd: HWND): Boolean;
  var
    AProcessId: Cardinal;
  begin
    GetWindowThreadProcessId(AWnd, @AProcessId);
    Result := (AWnd = 0) or (AProcessId <> GetCurrentProcessId);
  end;

var
  AMsg: PCWPStruct;
begin
  AMsg := PCWPStruct(ALParam);
  if (AMsg.message = WM_KILLFOCUS) and (HWND(AMsg.wParam) <> FSettingsToolbar.Handle) then
    FSettingsToolbar.Release;
end;

procedure dxShowCameraControlSettingsToolbar(ACameraControl: TdxCustomCameraControl);
begin
  if (FSettingsToolbar <> nil) and (FSettingsToolbar.FCameraControl <> ACameraControl) then
    FreeAndNil(FSettingsToolbar);

  if FSettingsToolbar = nil then
  begin
    FSettingsToolbar := TdxCameraControlSettingsToolbar.Create(nil);
    FSettingsToolbar.FCameraControl := ACameraControl;
    FSettingsToolbar.DoShow;
  end;
end;

function dxCameraManager: TdxCameraManager;
begin
  if FCameraManager = nil then
    FCameraManager := TdxCameraManager.Create;
  Result := FCameraManager;
end;

function dxIsCameraAvailable: Boolean;
begin
  Result := dxCameraManager.GetDeviceCount > 0;
end;

{ TdxCamera }

function dxMediaResult(AStatus: HRESULT): Boolean;
begin
  Result := AStatus = S_OK;
end;

procedure dxMediaCheck(AStatus: HRESULT; AMessage: string = '');
begin
  if AMessage = '' then
    AMessage := 'dxMediaCheck fails';
  if not dxMediaResult(AStatus) then
    raise EdxException.Create(AMessage);
end;

constructor TdxCameraManager.Create;
begin
  inherited;

  FDevices := TStringList.Create;
  FCameraList := TComponentList.Create;
  FRequestList := TComponentList.Create(False);
  FRequestTimer := TcxTimer.Create(nil);
  FRequestTimer.Enabled := False;
  FRequestTimer.OnTimer := RequestTimer;

  CoCreateInstance(CLSID_SystemDeviceEnum, nil, CLSCTX_INPROC, IID_ICreateDevEnum, FDevEnum);
end;

destructor TdxCameraManager.Destroy;
begin
  FreeAndNil(FRequestTimer);
  FreeAndNil(FRequestList);
  FreeAndNil(FCameraList);
  FreeAndNil(FDevices);
  inherited;
end;

procedure TdxCameraManager.AddRequest(AControl: TdxCustomCameraControl);
begin
  if FRequestList.IndexOf(AControl) = -1 then
  begin
    FRequestList.Add(AControl);
    FRequestTimer.Enabled := True;
    FRequestTimer.OnTimer(nil);
  end;
end;

procedure TdxCameraManager.CancelRequest(AControl: TdxCustomCameraControl);
begin
  FRequestList.Remove(AControl);
end;

function TdxCameraManager.GetCamera(ADeviceIndex: Integer): TdxCamera;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FCameraList.Count - 1 do
  begin
    Result := TdxCamera(FCameraList[I]);
    if (csDestroying in Result.ComponentState) or (Result.FDeviceIndex <> ADeviceIndex) then
      Result := nil
    else
      Break;
  end;

  if Result = nil then
  begin
    Result := TdxCamera.Create(nil);
    Result.Initialize(ADeviceIndex);
    if Result.Initialized then
    begin
      FCameraList.Add(Result);
      Result.OnDeviceLost := DeviceLost;
      Result.OnDeviceNotUsed := DeviceLost;
    end
    else
      FreeAndNil(Result);
  end;
end;

function TdxCameraManager.GetDevice(AIndex: Integer; out AMoniker: IMoniker): Boolean;
var
  AEnumMoniker: IEnumMoniker;
begin
  AMoniker := nil;

  FDevEnum.CreateClassEnumerator(CLSID_VideoInputDeviceCategory, AEnumMoniker, 0);
  Result := (AEnumMoniker <> nil) and dxMediaResult(AEnumMoniker.Reset);
  Result := Result and (dxMediaResult(AEnumMoniker.Skip(AIndex)) or (AIndex = 0));
  Result := Result and dxMediaResult(AEnumMoniker.Next(1, AMoniker, nil));
end;

function TdxCameraManager.GetDeviceCount: Integer;
begin
  RefreshDeviceList;
  Result := FDevices.Count;
end;

function TdxCameraManager.StartRecording(ADeviceIndex: Integer; const AFileName: string): Boolean;
var
  ACamera: TdxCamera;
begin
  ACamera := GetCamera(ADeviceIndex);
  Result := (ACamera <> nil) and ACamera.StartRecording(AFileName);
end;

procedure TdxCameraManager.StopRecording(ADeviceIndex: Integer);
var
  ACamera: TdxCamera;
begin
  ACamera := GetCamera(ADeviceIndex);
  if ACamera <> nil then
    ACamera.StopRecording;
end;

procedure TdxCameraManager.RefreshDeviceList;
var
  AMoniker: IMoniker;
  APropertyBag: IPropertyBag;
  ADeviceName: OLEvariant;
  AEnumMoniker: IEnumMoniker;
begin
  FDevices.Clear;

  FDevEnum.CreateClassEnumerator(CLSID_VideoInputDeviceCategory, AEnumMoniker, 0);
  if AEnumMoniker <> nil then
  begin
    AEnumMoniker.Reset;
    repeat
      AEnumMoniker.Next(1, AMoniker, nil);
      if AMoniker <> nil then
      begin
        if dxMediaResult(AMoniker.BindToStorage(nil, nil, IPropertyBag, APropertyBag)) then
          if dxMediaResult(APropertyBag.Read('FriendlyName', ADeviceName, nil)) then
            FDevices.Add(ADeviceName);
      end;
    until AMoniker = nil
  end;
end;

procedure TdxCameraManager.RequestTimer(Sender: TObject);
var
  I: Integer;
  AControl: TdxCustomCameraControl;
begin
  RefreshDeviceList;
  for I := FRequestList.Count - 1 downto 0 do
  begin
    AControl := FRequestList[I] as TdxCustomCameraControl;
    AControl.Camera := GetCamera(AControl.DeviceIndex);
  end;

  if FRequestList.Count = 0 then
    FRequestTimer.Enabled := False;
end;

procedure TdxCameraManager.DeviceLost(Sender: TObject);
begin
  Sender.Free;
end;

{ TdxCameraEventWindow }

procedure TdxCameraEventWindow.WndProc(var Message: TMessage);
begin
  FCamera.WndProc(Message);
  inherited;
end;

{ TdxCamera }

constructor TdxCamera.Create(AOwner: TComponent);
begin
  inherited;
  FTimer := TcxTimer.Create(nil);
  FTimer.Interval := 100;
  FTimer.Enabled := False;
  FTimer.OnTimer := OnTimer;
  FPreviewBitmap := TcxBitmap32.Create;
  FListeners := TComponentList.Create(False);
  FEventWindow := TdxCameraEventWindow.Create;
  FEventWindow.FCamera := Self;
  FStateChangedHandlers := TcxEventHandlerCollection.Create;
  FMediaTypes := TList.Create;
end;

destructor TdxCamera.Destroy;
begin
  Finalize;
  FreeAndNil(FMediaTypes);
  FreeAndNil(FStateChangedHandlers);
  FreeAndNil(FEventWindow);
  FreeAndNil(FListeners);
  FreeAndNil(FPreviewBitmap);
  FreeAndNil(FTimer);
  inherited;
end;

function TdxCamera.Capture(ABitmap: TcxBitmap32): Boolean;
var
  ASize: Integer;
  AColors: TRGBColors;
  AMediaType: TAMMediaType;
  AInfoHeader: TVideoInfoHeader;
begin
  Result := False;
  if not FIsRunning then
    Exit;

  if dxMediaResult(FSampleGrabber.GetCurrentBuffer(ASize, nil)) then
    if dxMediaResult(FSampleGrabber.GetConnectedMediaType(AMediaType)) then
    begin
      AInfoHeader := TVideoInfoHeader(AMediaType.pbFormat^);
      if (AInfoHeader.bmiHeader.biWidth * AInfoHeader.bmiHeader.biHeight) * 4 = ASize then
      begin
        SetLength(AColors, ASize div 4);
        if dxMediaResult(FSampleGrabber.GetCurrentBuffer(ASize, @AColors[0])) then
        begin
          ABitmap.SetSize(AInfoHeader.bmiHeader.biWidth, AInfoHeader.bmiHeader.biHeight);
          ABitmap.SetBitmapColors(AColors);
          Connected := True;
          Result := True;
        end;
      end;
    end;

  if not Result then
    ABitmap.SetSize(0, 0);
end;

procedure TdxCamera.CheckState;
var
  APrevState: TdxCameraState;
begin
  APrevState := FState;
  if Connected then
    FState := csRunning
  else
    if (FTimer = nil) or not FTimer.Enabled or (FFailsCount < 30) then
      FState := csInitializing
    else
      FState := csDeviceIsBusy;

  if FState <> APrevState then
    StateChangedHandlers.CallEvents(Self, []);
end;

procedure TdxCamera.OnTimer(Sender: TObject);
begin
  if not Capture(FPreviewBitmap) then
  begin
    Inc(FFailsCount);
    if not FRendered and (FFailsCount mod 10 = 0) then
    begin
      RenderStream(rmNull, 0, cxNullRect, '');
      Play(nil);
    end
    else
      if FRendered and (FFailsCount mod 30 = 0) then
      begin
        FMediaControl.Stop;
        FMediaControl.Run;
      end;
  end;
  CheckState;
  NotifyListeners;
end;

procedure TdxCamera.NotifyListeners;
var
  I: Integer;
begin
  for I := 0 to FListeners.Count - 1 do
    (FListeners[I] as TdxCustomCameraControl).CameraPictureChanged;
end;

function TdxCamera.InitializeGraph: Boolean;
begin
  FBuilder := nil;
  FGraph := nil;

  CoCreateInstance(CLSID_CaptureGraphBuilder2, nil, CLSCTX_INPROC, IID_ICaptureGraphBuilder2, FBuilder);
  CoCreateInstance(CLSID_FilterGraph, nil, CLSCTX_INPROC, IID_IGraphBuilder, FGraph);
  Result := (FBuilder <> nil) and (FGraph <> nil) and dxMediaResult(FBuilder.SetFiltergraph(FGraph));
end;

function TdxCamera.InitializeMedia: Boolean;
begin
  FMediaControl := nil;
  FMediaEvent := nil;

  Result := dxMediaResult(FGraph.QueryInterface(IID_IMediaControl, FMediaControl)) and
   dxMediaResult(FGraph.QueryInterface(IID_IMediaEventEx, FMediaEvent)) and
   dxMediaResult(FMediaEvent.SetNotifyWindow(FEventWindow.Handle, DXM_GRAPHNOTIFY, 0));
end;

function TdxCamera.InitializeDevice(ADeviceIndex: Integer): Boolean;
var
  AFilter: IBaseFilter;
  AMoniker: IMoniker;
  APropertyBag: IPropertyBag;
  ADeviceName: OLEvariant;
begin
  Result := False;
  FCameraFilter := nil;

  if dxCameraManager.GetDevice(ADeviceIndex, AMoniker) then
    if dxMediaResult(AMoniker.BindToObject(nil, nil, IID_IBaseFilter, AFilter)) then
        if dxMediaResult(AMoniker.BindToStorage(nil, nil, IPropertyBag, APropertyBag)) then
          if dxMediaResult(APropertyBag.Read('FriendlyName', ADeviceName, nil)) then
            if dxMediaResult(FGraph.AddFilter(AFilter, 'Video Capture')) then
            begin
              FDeviceIndex := ADeviceIndex;
              FDeviceName := ADeviceName;
              FCameraFilter := AFilter;
              FMoniker := AMoniker;
              Result := InitializeStreamConfig;
            end;
end;

function TdxCamera.InitializeCaptureGrabber: Boolean;
var
  AMediaType: TAMMediaType;
begin
  Result := False;
  FSampleGrabber := nil;
  FGrabberFilter := nil;

  if dxMediaResult(CoCreateInstance(CLSID_SampleGrabber, nil, CLSCTX_INPROC, IID_IBaseFilter, FGrabberFilter)) then
    if dxMediaResult(FGraph.AddFilter(FGrabberFilter, 'Video Grabber')) then
      if dxMediaResult(FGrabberFilter.QueryInterface(IID_ISampleGrabber, FSampleGrabber)) then
      begin
        dxMediaCheck(FSampleGrabber.SetBufferSamples(True), 'SetBufferSamples fails');

        cxZeroMemory(@AMediaType, SizeOf(TAMMediaType));
        AMediaType.majortype := MEDIATYPE_Video;
        AMediaType.subtype := MEDIASUBTYPE_RGB32;
        dxMediaCheck(FSampleGrabber.SetMediaType(AMediaType), 'SetMediaType fails');

        Result := True;
      end;
end;

function TdxCamera.InitializeStreamConfig: Boolean;
begin
  Result := dxMediaResult(FBuilder.FindInterface(@PIN_CATEGORY_CAPTURE, @MEDIATYPE_Video, FCameraFilter, IID_IAMStreamConfig, FStreamConfig));
end;

function TdxCamera.InitializeNullRenderer: Boolean;
begin
  Result := False;
  if dxMediaResult(CoCreateInstance(CLSID_NullRenderer, nil, CLSCTX_INPROC, IID_IBaseFilter, FRenderer)) then
    Result := dxMediaResult(FGraph.AddFilter(FRenderer, 'Null Filter'));
end;

function TdxCamera.InitializeWindowlessVMR(AHandle: THandle; const R: TRect): Boolean;
var
  AConfig: IVMRFilterConfig;
begin
  Result := False;
  if FWindowlessControl = nil then
    if dxMediaResult(CoCreateInstance(CLSID_VideoMixingRenderer, nil, CLSCTX_INPROC, IID_IBaseFilter, FRenderer)) then
      if dxMediaResult(FGraph.AddFilter(FRenderer, 'Video Mixing Renderer')) then
        if dxMediaResult(FRenderer.QueryInterface(IID_IVMRFilterConfig, AConfig)) then
          if dxMediaResult(AConfig.SetRenderingMode(VMRMode_Windowless)) then
            FRenderer.QueryInterface(IID_IVMRWindowlessControl, FWindowlessControl);

  if FWindowlessControl <> nil then
    if dxMediaResult(FWindowlessControl.SetVideoClippingWindow(AHandle)) then
       Result := dxMediaResult(FWindowlessControl.SetVideoPosition(nil, @R));
end;

function TdxCamera.InitializeWindowedVMR(AHandle: THandle; const R: TRect): Boolean;
begin
  Result := False;
  if FVideoWindow = nil then
    FGraph.QueryInterface(IID_IVideoWindow, FVideoWindow);

  if FVideoWindow <> nil then
    if dxMediaResult(FVideoWindow.put_Owner(AHandle)) then
      if dxMediaResult(FVideoWindow.put_WindowStyle(WS_CHILD or WS_CLIPCHILDREN)) then
        if dxMediaResult(FVideoWindow.SetWindowPosition(R.Left, R.Top, cxRectWidth(R), cxRectHeight(R))) then
          Result := dxMediaResult(FVideoWindow.put_Visible(True));
end;

procedure TdxCamera.ApplySelectedMediaType;
begin
{$IFDEF DELPHI22}
  dxMediaCheck(FStreamConfig.SetFormat(MediaTypes[FMediaTypeIndex]), 'FStreamConfig.SetFormat fails');
{$ELSE}
  dxMediaCheck(FStreamConfig.SetFormat(TAMMediaType(MediaTypes[FMediaTypeIndex]^)), 'FStreamConfig.SetFormat fails');
{$ENDIF}
end;

function TdxCamera.ConnectGraph(const AFileName: string): Boolean;
begin
  Result := RenderStream(rmNull, 0, cxNullRect, AFileName);
  FMediaControl.Run;
end;

function TdxCamera.DisconnectGraph: Boolean;
var
  AEnumFilters: IEnumFilters;
  AFilter: IBaseFilter;
begin
  FRendered := False;
  Result := dxMediaResult(FMediaControl.Stop);
  Result := Result and dxMediaResult(FGraph.EnumFilters(AEnumFilters));
  while Result and dxMediaResult(AEnumFilters.Next(1, AFilter, nil)) do
    if AFilter <> FCameraFilter then
      Result := Result and dxMediaResult(FGraph.RemoveFilter(AFilter)) and dxMediaResult(AEnumFilters.Reset);
end;

procedure TdxCamera.FindMediaTypes;
var
  ACurrentSubtype: Cardinal;
  ACount, ASize: Integer;
  AMediaType: PAMMediaType;
  I: Integer;
  AStreamConfigCaps: TVideoStreamConfigCaps;
begin
  ACurrentSubtype := GetCurrentMediaType.subtype.D1;
  FStreamConfig.GetNumberOfCapabilities(ACount, ASize);
  for I := 0 to ACount - 1 do
  begin
    dxMediaResult(FStreamConfig.GetStreamCaps(I, AMediaType, AStreamConfigCaps));
    if CompareMem(@FORMAT_VideoInfo, @TAMMediaType(AMediaType^).formattype, SizeOf(MEDIATYPE_Video)) and (TAMMediaType(AMediaType^).subtype.D1 = ACurrentSubtype) then
      FMediaTypes.Add(AMediaType);
  end;
end;

procedure TdxCamera.SetConnected(AValue: Boolean);
begin
  if FConnected <> AValue then
  begin
    FConnected := AValue;
    FFailsCount := 0;
    StateChangedHandlers.CallEvents(Self, []);
  end;
end;

procedure TdxCamera.SetMediaTypeIndex(AValue: Integer);
begin
  if not FRecording and (MediaTypes.Count > 0) and (FMediaTypeIndex <> AValue) and
    DisconnectGraph and InitializeCaptureGrabber then
  begin
    FMediaTypeIndex := AValue;
    ApplySelectedMediaType;
    ConnectGraph;
  end;
end;

function TdxCamera.Initialize(ADeviceIndex: Integer): Boolean;
begin
  Result := False;
  if not FInitialized then
  begin
    if InitializeGraph then
      if InitializeMedia then
        if InitializeDevice(ADeviceIndex) then
          FInitialized := InitializeCaptureGrabber;
    if not FInitialized then
      Finalize;
    Result := FInitialized;
  end;
end;

procedure TdxCamera.Finalize;
begin
  Connected := False;
  FInitialized := False;
  FRendered := False;
  FIsRunning := False;

  FMux := nil;
  FRenderer := nil;
  FVideoWindow := nil;
  FWindowlessControl := nil;
  FMediaEvent := nil;
  FMediaControl := nil;
  FMoniker := nil;
  FSampleGrabber := nil;
  FCameraFilter  := nil;
  FGrabberFilter  := nil;
  FGraph := nil;
  FBuilder := nil;
  FStreamConfig := nil;
end;

function TdxCamera.GetCurrentMediaType: TAMMediaType;
var
  AAMMediaTypePointer: PAMMediaType;
begin
  if FStreamConfig <> nil then
    dxMediaCheck(FStreamConfig.GetFormat(AAMMediaTypePointer), 'GetCurrentMediaType')
  else
    AAMMediaTypePointer := nil;

  Result := AAMMediaTypePointer^;
end;

procedure TdxCamera.Reinitialize;
begin
  Finalize;
  if Initialize(FDeviceIndex) then
    ApplySelectedMediaType
end;

procedure TdxCamera.SetVideoPosition(const R: TRect);
begin
  if FWindowlessControl <> nil then
    FWindowlessControl.SetVideoPosition(nil, @R);
  if FVideoWindow <> nil then
    FVideoWindow.SetWindowPosition(R.Left, R.Top, cxRectWidth(R), cxRectHeight(R));
end;

function TdxCamera.RenderStream(ARenderingMode: TdxCameraRenderingMode; AHandle: THandle; const R: TRect; const AOutputFileName: string): Boolean;
var
  ASink: IFileSinkFilter;
begin
  Result := False;
  FRecording := False;
  if FInitialized and not FRendered then
  begin
    case ARenderingMode of
      rmNull:
        InitializeNullRenderer;
      rmWindowLess:
        InitializeWindowlessVMR(AHandle, R);
      rmWindowed:
        InitializeWindowedVMR(AHandle, R);
    end;
    if AOutputFileName <> '' then
    begin
      FBuilder.SetOutputFileName(MEDIASUBTYPE_Avi, PWideChar(AOutputFileName), FMux, ASink);
      FRendered := dxMediaResult(FBuilder.RenderStream(@PIN_CATEGORY_CAPTURE, @MEDIATYPE_Video, FCameraFilter, FGrabberFilter, FMux));
      if ARenderingMode <> rmNull then
        FBuilder.RenderStream(@PIN_CATEGORY_PREVIEW, @MEDIATYPE_Video, FCameraFilter, nil, FRenderer);
    end
    else
      if FRenderer <> nil then
        FRendered := dxMediaResult(FBuilder.RenderStream(@PIN_CATEGORY_CAPTURE, @MEDIATYPE_Video, FCameraFilter, FGrabberFilter, FRenderer));
    Result := FRendered;
  end;
  if (MediaTypes.Count = 0) and Result then
    FindMediaTypes;
end;

procedure TdxCamera.WndProc(var Message: TMessage);
var
  ACode: Integer;
{$IFDEF DELPHI17}
  AParam1, AParam2: TdxNativeInt;
{$ELSE}
  AParam1, AParam2: Longint;
{$ENDIF}
begin
  if Message.Msg = DXM_GRAPHNOTIFY then
  begin
    FMediaEvent.GetEvent(ACode, AParam1, AParam2, INFINITE);
    if (ACode = EC_DEVICE_LOST) and (AParam2 = 0) then
    begin
      Finalize;
      CallNotify(OnDeviceLost, Self);
    end;
  end;
end;

function TdxCamera.Play(AControl: TdxCustomCameraControl): Boolean;
begin
  Result := FInitialized;
  if Result then
  begin
    if not FIsRunning and FRendered then
    begin
      Result := FMediaControl.Run = 0;
      FIsRunning := True;
    end;
    if (AControl <> nil) and (FListeners.IndexOf(AControl) = -1) then
    begin
      cxAddFreeNotification(AControl, Self);
      FListeners.Add(AControl);
      FTimer.Enabled := True;
    end;
  end;
  CheckState;
end;

procedure TdxCamera.Stop(AControl: TdxCustomCameraControl);
begin
  if csDestroying in ComponentState then
    Exit;

  cxRemoveFreeNotification(AControl, Self);
  FListeners.Remove(AControl);
  if (FListeners.Count = 0) and not FRecording then
  begin
    FTimer.Enabled := False;
    FMediaControl.Stop;
    FIsRunning := False;
    Connected := False;
    dxCallNotify(OnDeviceNotUsed, Self);
  end
  else
    CheckState;
end;

function TdxCamera.StartRecording(const AFileName: string): Boolean;
begin
  Result := DisconnectGraph and InitializeCaptureGrabber and ConnectGraph(AFileName) and FileExists(AFileName);
  FRecording := Result;
  if not Result then
    StopRecording;
end;

procedure TdxCamera.StopRecording;
begin
  Reinitialize;
end;

{ TdxCameraControlSettingsToolbar1Item }

procedure TdxCameraControlSettingsToolbarItem.Draw(ACanvas: TcxCanvas);
begin
  DrawBackground(ACanvas);
  DrawText(ACanvas);
end;

procedure TdxCameraControlSettingsToolbarItem.DrawBackground(ACanvas: TcxCanvas);
var
  AColor: TColor;
begin
  ACanvas.Pen.Color := $B2B2B2;

  case State of
    ecsHot:
      if FChecked then
        AColor := $555555
      else
        AColor := $D3D3D3;
    ecsPressed:
      if FChecked then
        AColor := $D3D3D3
      else
        AColor := $696969;
  else {ecsNormal}
    if FChecked then
      AColor := $898989
    else
      AColor := $B2B2B2;
  end;

  ACanvas.Brush.Color := AColor;
  ACanvas.Rectangle(FBounds);
end;

procedure TdxCameraControlSettingsToolbarItem.DrawText(ACanvas: TcxCanvas);

  function GetHorizontalAlignFlags(const ADesiredFlags: Integer): Integer;
  begin
    Result := ADesiredFlags;
    if FCameraControl.UseRightToLeftAlignment then
    begin
      if Result = DT_LEFT then
        Result := DT_RIGHT
      else
        Result := DT_LEFT;
    end;
    if FCameraControl.UseRightToLeftReading then
      Result := Result or DT_RTLREADING;
  end;

var
  ARect: TRect;
begin
  ARect := cxRectInflate(FBounds, -5, 0);
  cxDrawText(ACanvas.Handle, FCaption, ARect, GetHorizontalAlignFlags(DT_LEFT) or DT_VCENTER or DT_SINGLELINE);
  cxDrawText(ACanvas.Handle, FDescription, ARect, GetHorizontalAlignFlags(DT_RIGHT) or DT_VCENTER or DT_SINGLELINE);
end;

procedure TdxCameraControlSettingsToolbarItem.SetChecked(AChecked: Boolean);
begin
  if FChecked <> AChecked then
  begin
    FChecked := AChecked;
    cxInvalidateRect(FParentHandle, FBounds);
  end;
end;

procedure TdxCameraControlSettingsToolbarItem.SetState(AState: TcxEditCheckState);
begin
  if FState <> AState then
  begin
    FState := AState;
    cxInvalidateRect(FParentHandle, FBounds);
  end;
end;

{ TdxCameraControlSettingsToolbar }

constructor TdxCameraControlSettingsToolbar.Create(AOwner: TComponent);
begin
  CreateNew(AOwner);
  BorderStyle := bsNone;
  FormStyle := fsStayOnTop;
  FcxCanvas := TcxCanvas.Create(Canvas);
  FItems := TObjectList.Create;
  dxSetHook(htMouse, dxSettingsToolbarMouseHook);
  dxSetHook(htWndProc, dxSettingsToolbarWndProcHook);
end;

destructor TdxCameraControlSettingsToolbar.Destroy;
begin
  FSettingsToolbar := nil;
  dxReleaseHook(dxSettingsToolbarWndProcHook);
  dxReleaseHook(dxSettingsToolbarMouseHook);
  FreeAndNil(FItems);
  FreeAndNil(FcxCanvas);
  inherited;
end;


procedure TdxCameraControlSettingsToolbar.CreateWindowHandle(const Params: TCreateParams);
begin
  inherited;
  cxSetLayeredWindowAttributes(Handle, 200);
end;

procedure TdxCameraControlSettingsToolbar.Paint;
var
  AWindowRegion: TcxRegion;
begin
  AWindowRegion := TcxRegion.Create;
  try
    if GetWindowRgn(Handle, AWindowRegion.Handle) in [NULLREGION, SIMPLEREGION, COMPLEXREGION] then
      FcxCanvas.DrawRegion(AWindowRegion, $B2B2B2, $7F7F7F);
  finally
    AWindowRegion.Free;
  end;
  DrawItems(FcxCanvas);
end;

procedure TdxCameraControlSettingsToolbar.DoShow;
begin
  InitSettingsPage;
  BoundsRect := GetNewBounds;
  CalculateItemsBounds(cxRectSetNullOrigin(BoundsRect));
  SetWindowRgn(Handle, CreateRoundRectRgn(0, 0, Width + 1, Height + 1, 5, 5), False);
  ShowWindow(Handle, SW_SHOWNA);
end;

procedure TdxCameraControlSettingsToolbar.MouseLeave;
begin
  HotItem := nil;
end;

procedure TdxCameraControlSettingsToolbar.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  AItemIndex: Integer;
begin
  inherited;
  AItemIndex := GetItemIndexAtPoint(Point(X, Y));
  if AItemIndex = -1 then
    HotItem := nil
  else
    if Items[AItemIndex] <> HotItem then
      HotItem := Items[AItemIndex];
end;

procedure TdxCameraControlSettingsToolbar.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  AItemIndex: Integer;
begin
  inherited;
  AItemIndex := GetItemIndexAtPoint(Point(X, Y));
  if AItemIndex > -1 then
    ClickItem(AItemIndex);
end;

procedure TdxCameraControlSettingsToolbar.CalculateItemsBounds(const ARect: TRect);
var
  I, AColumnIndex: Integer;
  AColumnItemCount: Integer;
  AItemHeight, AItemWidth: Integer;
begin
  AItemWidth := cxRectWidth(ARect) div FColumnCount;
  AItemHeight := cxTextHeight(Font) * 2;
  AColumnItemCount := FItems.Count div FColumnCount;
  Items[0].FBounds := cxRectContent(cxRectOffset(cxRect(cxSize(cxRectWidth(ARect), AItemHeight)),0, ScaleFactor.Apply(5)),
    ScaleFactor.Apply(Rect(1, 0, 1, 0)));
  for I := 1 to FItems.Count - 1 do
  begin
    AColumnIndex := (I - 1) div AColumnItemCount;
    Items[I].FBounds := cxRectContent(cxRectOffset(cxRect(cxSize(AItemWidth, AItemHeight)), AItemWidth * AColumnIndex,
      ScaleFactor.Apply(5) + (I - AColumnIndex * AColumnItemCount) * AItemHeight), ScaleFactor.Apply(Rect(1, 0, 1, 0)));
  end;
  if UseRightToLeftAlignment then
    for I := 0 to FItems.Count - 1 do
      Items[I].FBounds := TdxRightToLeftLayoutConverter.ConvertRect(Items[I].FBounds, ARect);
end;


procedure TdxCameraControlSettingsToolbar.ClickItem(AItemIndex: Integer);

  procedure ClickSettingsItem;
  begin
    case AItemIndex of
      0: Release;
      1: Initialize(stpDevices);
      2: Initialize(stpResolutions);
    end;
  end;

  procedure ClickDevicesItem;
  var
    ACheckedIndex: Integer;
  begin
    if AItemIndex = 0 then
      Initialize(stpSettings)
    else
    begin
      ACheckedIndex := GetCheckedItemIndex;
      if not FCameraControl.Active or (ACheckedIndex <> AItemIndex) then
        FCameraControl.DeviceIndex := AItemIndex - 1;
    end;
  end;

  procedure ClickResolutionsItem;
  var
    ACheckedIndex: Integer;
  begin
    if AItemIndex = 0 then
      Initialize(stpSettings)
    else
    begin
      ACheckedIndex := GetCheckedItemIndex;
      if ACheckedIndex <> AItemIndex then
        FCameraControl.ResolutionIndex := AItemIndex - 1;
    end;
  end;

begin
  case FToolbarPage of
    stpSettings: ClickSettingsItem;
    stpDevices: ClickDevicesItem;
    stpResolutions: ClickResolutionsItem;
  end;
end;

function TdxCameraControlSettingsToolbar.CreateItem(const ACaption, ADescription: string): TdxCameraControlSettingsToolbarItem;
begin
  Result := TdxCameraControlSettingsToolbarItem.Create;
  Result.FDescription := ADescription;
  Result.FCameraControl := FCameraControl;
  Result.FCaption := ACaption;
  Result.FState := ecsNormal;
  Result.FParentHandle := Handle;
end;

procedure TdxCameraControlSettingsToolbar.DrawItems(ACanvas: TcxCanvas);
var
  I: Integer;
begin
  for I := 0 to FItems.Count - 1 do
    Items[I].Draw(ACanvas);
end;

function TdxCameraControlSettingsToolbar.GetCheckedItemIndex: Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to FItems.Count - 1 do
    if Items[I].FChecked then
    begin
      Result := I;
      Break;
    end;
end;

function TdxCameraControlSettingsToolbar.GetItemIndexAtPoint(const P: TPoint): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to FItems.Count - 1 do
    if cxRectPtIn(Items[I].FBounds, P) then
    begin
      Result := I;
      Break;
    end;
end;

procedure TdxCameraControlSettingsToolbar.InitDevicesPage;
var
  I: Integer;
  AItem: TdxCameraControlSettingsToolbarItem;
begin
  FToolbarPage := stpDevices;
  FItems.Clear;
  dxCameraManager.RefreshDeviceList;
  FItems.Add(CreateItem('<   ' + cxGetResourceString(@sdxCameraControlSettingsFormDevices), ''));
  for I := 0 to dxCameraManager.GetDeviceCount - 1 do
  begin
    AItem := CreateItem('', dxCameraManager.Devices[I]);
    if FCameraControl.Active and (I = FCameraControl.DeviceIndex) then
      AItem.FChecked := True;
    FItems.Add(AItem);
  end;
end;

procedure TdxCameraControlSettingsToolbar.Initialize(AToolbarPage: TdxCameraControlSettingsToolbarPage);
begin
  FHotItem := nil;
  case AToolbarPage of
    stpDevices:
      InitDevicesPage;
    stpResolutions:
      InitResolutionsPage;
    else
      InitSettingsPage;
  end;
  BoundsRect := GetNewBounds;
  CalculateItemsBounds(cxRectSetNullOrigin(BoundsRect));
  SetWindowRgn(Handle, CreateRoundRectRgn(0, 0, Width + 1, Height + 1, 5, 5), False);
  Invalidate;
end;

procedure TdxCameraControlSettingsToolbar.InitResolutionsPage;
var
  I: Integer;
  AItem: TdxCameraControlSettingsToolbarItem;
begin
  FToolbarPage := stpResolutions;
  FItems.Clear;
  dxCameraManager.RefreshDeviceList;
  FItems.Add(CreateItem('<   ' + cxGetResourceString(@sdxCameraControlSettingsFormResolutions), ''));
  for I := 0 to FCameraControl.ResolutionCount - 1 do
  begin
    AItem := CreateItem('', IntToStr(FCameraControl.Resolutions[I].cx) + 'x' + IntToStr(FCameraControl.Resolutions[I].cy));
    if I = FCameraControl.ResolutionIndex then
      AItem.FChecked := True;
    FItems.Add(AItem);
  end;
end;

procedure TdxCameraControlSettingsToolbar.InitSettingsPage;
begin
  FToolbarPage := stpSettings;
  FItems.Clear;
  dxCameraManager.RefreshDeviceList;
  FItems.Add(CreateItem('<   ' +  cxGetResourceString(@sdxCameraControlSettingsFormSettings), ''));
  if (dxCameraManager.GetDeviceCount > 0) and FCameraControl.Active then
  begin
    FItems.Add(CreateItem(cxGetResourceString(@sdxCameraControlSettingsFormDevice), FCameraControl.DeviceName + '   >'));
    if (FCameraControl.Camera <> nil) and FCameraControl.Camera.Connected then
    begin
      FItems.Add(CreateItem(cxGetResourceString(@sdxCameraControlSettingsFormResolution),
        IntToStr(FCameraControl.Resolutions[FCameraControl.ResolutionIndex].cx) +
        'x' + IntToStr(FCameraControl.Resolutions[FCameraControl.ResolutionIndex].cy) + '   >'));
    end
  end;
end;

procedure TdxCameraControlSettingsToolbar.Reinitilize;
begin
  Initialize(FToolbarPage);
end;

function TdxCameraControlSettingsToolbar.GetNewBounds: TRect;
var
  ARect: TRect;
begin
  Result := cxRectInflate(cxRect(GetContentRectSize), ScaleFactor.Apply(5));
  Result := cxRectSetBottom(Result, FCameraControl.ClientToScreen(FCameraControl.Bounds.BottomRight).Y);
  Result := cxRectSetRight(Result, FCameraControl.ClientToScreen(FCameraControl.Bounds.BottomRight).X);
  Result := cxRectOffset(Result, ScaleFactor.Apply(Point(5, 5)), False);
  if FCameraControl.UseRightToLeftAlignment then
  begin
    ARect.TopLeft := FCameraControl.ScreenToClient(Result.TopLeft);
    ARect.BottomRight := FCameraControl.ScreenToClient(Result.BottomRight);
    ARect := TdxRightToLeftLayoutConverter.ConvertRect(ARect, FCameraControl.ClientRect);
    Result.TopLeft := FCameraControl.ClientToScreen(ARect.TopLeft);
    Result.BottomRight := FCameraControl.ClientToScreen(ARect.BottomRight);
  end;
end;

function TdxCameraControlSettingsToolbar.GetContentRectSize: TSize;
var
  I: Integer;
  AMaxWidthCaption: Integer;
begin
  Result := Size(0, 0);
  FColumnCount := 1;
  if FItems.Count > 1 then
  begin
    AMaxWidthCaption := 0;
    for I := 1 to FItems.Count - 1 do
      AMaxWidthCaption := Max(AMaxWidthCaption, cxTextWidth(Font, Items[I].FCaption));

    for I := 1 to FItems.Count - 1 do
      if Items[I].FDescription = '' then
        Result.cx := Max(Result.cx, cxTextWidth(Font, Items[I].FCaption) + ScaleFactor.Apply(5))
      else
        if Items[I].FCaption = '' then
          Result.cx := Max(Result.cx, cxTextWidth(Font, Items[I].FDescription) + ScaleFactor.Apply(5))
        else
          Result.cx := Max(Result.cx, cxTextWidth(Font, Items[I].FDescription) + AMaxWidthCaption + ScaleFactor.Apply(20));
    FColumnCount := Ceil(Sqrt(FItems.Count * cxTextHeight(Font) * 2 * Result.cx) / Result.cx);
  end;
  Result.cx := Max(Result.cx, cxTextWidth(Font, Items[0].FCaption) + ScaleFactor.Apply(5));
  Result := cxSize(Result.cx * FColumnCount, (Ceil((FItems.Count - 1) / FColumnCount) + 1) * cxTextHeight(Font) * 2);
end;

procedure TdxCameraControlSettingsToolbar.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  MouseLeave;
end;

procedure TdxCameraControlSettingsToolbar.DXMScaleChanged(var Message: TMessage);
begin
  inherited;
  DoShow;
end;

function TdxCameraControlSettingsToolbar.GetItems(AIndex: Integer): TdxCameraControlSettingsToolbarItem;
begin
  Result := FItems[AIndex] as TdxCameraControlSettingsToolbarItem;
end;

procedure TdxCameraControlSettingsToolbar.SetHotItem(AValue: TdxCameraControlSettingsToolbarItem);
begin
  if (FItems <> nil) and (FHotItem <> AValue) then
  begin
    if FHotItem <> nil then
      FHotItem.State := ecsNormal;
    FHotItem := AValue;
    if FHotItem <> nil then
      FHotItem.State := ecsHot;
  end;
end;

function TdxCameraControlSettingsToolbar.GetParentForm: TCustomForm;
begin
  Result := Forms.GetParentForm(FCameraControl.Parent);
end;

{ TdxCustomCameraControl }

constructor TdxCustomCameraControl.Create(AOwner: TComponent);
begin
  inherited;

  FSettingsButtonShowingTimer := TcxTimer.Create(nil);
  FSettingsButtonShowingTimer.OnTimer := SettingsButtonShowingTimer;
  FSettingsButtonShowingTimer.Interval := 50;
  FSettingsButtonShowingTimer.Enabled := False;

  FSettingsButtonGlyph := TdxGPImage.Create;
  FSettingsButtonGlyph.LoadFromResource(HInstance, 'DXCAMERACONTROL_SETTINGSBUTTON', 'PNG');

  ControlStyle := ControlStyle - [csParentBackground];
  BorderStyle := cxcbsDefault;

  Width := 200;
  Height := 150;

  FCapturedBitmap := TcxBitmap32.Create;
  FFitMode := ifmProportionalStretch;
  FShowSettingsButton := True;
end;

destructor TdxCustomCameraControl.Destroy;
begin
  Active := False;
  Camera := nil;
  FreeAndNil(FCapturedBitmap);
  FreeAndNil(FSettingsButtonGlyph);
  FreeAndNil(FSettingsButtonShowingTimer);
  FreeAndNil(FSettingsToolbar);
  inherited;
end;

procedure TdxCustomCameraControl.Capture;
begin
  FCamera.Capture(FCapturedBitmap);
  cxInvalidateRect(Self);
end;

procedure TdxCustomCameraControl.CheckState;
const
  dxCameraStateToCamerControlState: array[TdxCameraState] of TdxCameraControlState = (ccsInitializing, ccsRunning, ccsDeviceIsBusy);
var
  APrevState: TdxCameraControlState;
begin
  APrevState := FState;
  if Active then
  begin
    if FPaused then
      FState := ccsPaused
    else
      if FCamera = nil then
        FState := ccsNoDevice
      else
        FState := dxCameraStateToCamerControlState[Camera.State];

    if not IsDestroying and (FState <> APrevState) and (FSettingsToolbar <> nil) and
      ((APrevState = ccsRunning) or (FState = ccsRunning)) then
        FSettingsToolbar.Reinitilize;
  end
  else
    FState := ccsInactive;

  if not IsDestroying and (FState <> APrevState) then
    dxCallNotify(OnStateChanged, Self);
end;

procedure TdxCustomCameraControl.Click;
var
  AMousePos: TPoint;
begin
  inherited;
  if ShowSettingsButton then
  begin
    AMousePos := cxPointOffset(GetMouseCursorClientPos, Bounds.TopLeft);
    if cxRectPtIn(GetSettingsButtonRect, AMousePos) then
      ShowSettingsToolbar;
  end;
end;

procedure TdxCustomCameraControl.MouseEnter(AControl: TControl);
begin
  inherited;
  FSettingsButtonShowingIncrement := 25;
  FSettingsButtonShowingTimer.Enabled := True;
end;

procedure TdxCustomCameraControl.MouseLeave(AControl: TControl);
begin
  inherited;
  FSettingsButtonShowingIncrement := -25;
  if FSettingsButtonAlpha > 150 then
    FSettingsButtonAlpha := 150;
  FSettingsButtonShowingTimer.Enabled := True;
end;

procedure TdxCustomCameraControl.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if cxRectPtIn(GetSettingsButtonRect, Point(X, Y)) then
  begin
    FSettingsButtonAlpha := 255;
    InvalidateRect(GetSettingsButtonRect, True);
  end
  else
    if FSettingsButtonAlpha = 255 then
    begin
      FSettingsButtonAlpha := 150;
      InvalidateRect(GetSettingsButtonRect, True);
    end;
end;

procedure TdxCustomCameraControl.Paint;

  procedure DrawBackground;
  begin
    case State of
      ccsRunning, ccsPaused:
        Canvas.FillRect(ClientRect, clBlack);
      else
        Canvas.FillRect(ClientRect, clWhite);
    end;
  end;

begin
  inherited;
  Canvas.Font := Font;
  DrawBackground;
  case State of
    ccsRunning:
      cxDrawImage(Canvas, ClientRect, FCamera.PreviewBitmap, FFitMode, nil, nil, True);
    ccsPaused:
      cxDrawImage(Canvas, ClientRect, CapturedBitmap, FFitMode, nil, nil, True);
    else
      cxDrawMultilineText(Canvas, GetDisplayText, ClientRect, DT_VCENTER or DT_CENTER);
  end;
  if ((FSettingsToolbar = nil) or (FSettingsToolbar.FCameraControl <> Self)) and ShowSettingsButton then
    DrawSettingsButton;
end;

procedure TdxCustomCameraControl.Pause;
begin
  if State = ccsRunning then
  begin
    Capture;
    FPaused := True;
  end;
  CheckState;
end;

procedure TdxCustomCameraControl.Play;
begin
  FPaused := False;
  Active := True;
  CheckState;
  cxInvalidateRect(Self);
end;

procedure TdxCustomCameraControl.Stop;
begin
  FPaused := False;
  Active := False;
  CheckState;
end;

procedure TdxCustomCameraControl.ShowSettingsToolbar;
begin
  dxShowCameraControlSettingsToolbar(Self);
end;

procedure TdxCustomCameraControl.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FCamera) then
    Camera := nil;
end;

function TdxCustomCameraControl.IsDoubleBufferedNeeded: Boolean;
begin
  Result := True;
end;

procedure TdxCustomCameraControl.LookAndFeelChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues);
begin
  inherited;
  Invalidate;
end;

function TdxCustomCameraControl.GetDisplayText: string;
begin
  if IsDesigning then
    Result := 'No rendering at design time'
  else
    case State of
      ccsInactive:
        Result := cxGetResourceString(@sdxCameraInactive);
      ccsRunning:
        Result := cxGetResourceString(@sdxCameraRunning);
      ccsPaused:
        Result := cxGetResourceString(@sdxCameraPaused);
      ccsNoDevice:
        Result := cxGetResourceString(@sdxCameraNotDetected);
      ccsInitializing:
        Result := cxGetResourceString(@sdxCameraInitializing);
      ccsDeviceIsBusy:
        Result := cxGetResourceString(@sdxCameraIsBusy);
    else
      Result := '';
    end;
end;

procedure TdxCustomCameraControl.SetActive(AValue: Boolean);
begin
  if FActive <> AValue then
  begin
    if csDesigning in ComponentState then
      FActive := AValue
    else
    begin
      FActive := AValue;
      if AValue then
        dxCameraManager.AddRequest(Self)
      else
        Camera := nil;
    end;
    cxInvalidateRect(Self);
  end;
  CheckState;
end;

procedure TdxCustomCameraControl.SetCamera(AValue: TdxCamera);
begin
  if AValue <> FCamera then
  begin
    if FCamera <> nil then
    begin
      FCamera.StateChangedHandlers.Remove(CameraStateChanged);
      FCamera.Stop(Self);
    end
    else
      dxCameraManager.CancelRequest(Self);

    FCamera := AValue;

    if FCamera <> nil then
    begin
      FCamera.Play(Self);
      FCamera.StateChangedHandlers.Add(CameraStateChanged);
    end
    else
      if Active then
        dxCameraManager.AddRequest(Self);

    cxInvalidateRect(Self);
  end;
  CheckState;
end;

procedure TdxCustomCameraControl.SetFitMode(AValue: TcxImageFitMode);
begin
  if FitMode <> AValue then
  begin
    FFitMode := AValue;
    cxInvalidateRect(Self);
  end;
end;

procedure TdxCustomCameraControl.SetMediaTypeIndex(AValue: Integer);
begin
  if Camera <> nil then
  begin
    Camera.MediaTypeIndex := AValue;
    if FSettingsToolbar <> nil then
      FSettingsToolbar.Reinitilize;
  end;
end;

procedure TdxCustomCameraControl.SetShowSettingsButton(AValue: Boolean);
begin
  if FShowSettingsButton <> AValue then
  begin
    FShowSettingsButton := AValue;
    if FShowSettingsButton  then
    begin
      if cxRectPtIn(BoundsRect, ScreenToClient(GetMouseCursorPos)) then
        FSettingsButtonAlpha := 150
    end
    else
      FSettingsButtonAlpha := 0;

    InvalidateRect(GetSettingsButtonRect, True);
  end;
end;

function TdxCustomCameraControl.GetCurrentMediaType: TAMMediaType;
begin
  Result := Camera.GetCurrentMediaType;
end;

function TdxCustomCameraControl.GetDeviceName: string;
begin
  if FCamera <> nil then
    Result := FCamera.FDeviceName
  else
    Result := '';
end;

function TdxCustomCameraControl.GetResolutionCount: Integer;
begin
  if MediaTypes <> nil then
    Result := MediaTypes.Count
  else
    Result := 0;
end;

function TdxCustomCameraControl.GetResolutionIndex: Integer;
begin
  if Camera <> nil then
    Result := Camera.MediaTypeIndex
  else
    Result := -1;
end;

function TdxCustomCameraControl.GetResolutions(AIndex: Integer): TSize;
begin
  if (MediaTypes <> nil) and (AIndex > -1) and (AIndex < ResolutionCount) then
  begin
    Result.cx := TVideoInfoHeader(TAMMediaType(MediaTypes[AIndex]^).pbFormat^).bmiHeader.biWidth;
    Result.cy := TVideoInfoHeader(TAMMediaType(MediaTypes[AIndex]^).pbFormat^).bmiHeader.biHeight;
  end
  else
    Result := cxNullSize;
end;

function TdxCustomCameraControl.GetMediaTypeIndex: Integer;
begin
  if Camera <> nil then
    Result := Camera.MediaTypeIndex
  else
    Result := -1;
end;

function TdxCustomCameraControl.GetMediaTypes: TList;
begin
  if Camera <> nil then
    Result := Camera.MediaTypes
  else
    Result := nil;
end;

procedure TdxCustomCameraControl.SetDeviceIndex(Value: Integer);
begin
  if Active then
  begin
    Active := False;
    FDeviceIndex := Value;
    Active := True;
  end
  else
    FDeviceIndex := Value;

  if FSettingsToolbar <> nil then
    FSettingsToolbar.Reinitilize;
end;

procedure TdxCustomCameraControl.SetResolutionIndex(AValue: Integer);
begin
  if DeviceIndex > -1 then
    MediaTypeIndex := AValue;
end;

procedure TdxCustomCameraControl.CameraPictureChanged;
begin
  if State = ccsRunning then
    cxInvalidateRect(Self);
end;

procedure TdxCustomCameraControl.CameraStateChanged(Sender: TObject; const AEventArgs);
begin
  CheckState;
end;

procedure TdxCustomCameraControl.DrawSettingsButton;
begin
  FSettingsButtonGlyph.StretchDraw(Canvas.Handle, GetSettingsButtonRect, cxRect(FSettingsButtonGlyph.Size), FSettingsButtonAlpha);
end;

function TdxCustomCameraControl.GetSettingsButtonRect: TRect;
var
  ASize: TSize;
begin
  ASize := ScaleFactor.Apply(FSettingsButtonGlyph.Size);
  Result := cxRectBounds(
    Bounds.Right - ASize.cx - ScaleFactor.Apply(5),
    Bounds.Bottom - ASize.cy - ScaleFactor.Apply(5), ASize);
  if UseRightToLeftAlignment then
    Result := TdxRightToLeftLayoutConverter.ConvertRect(Result, Bounds);
end;

procedure TdxCustomCameraControl.SettingsButtonShowingTimer(Sender: TObject);
var
  ANewAlpha: Integer;
begin
  ANewAlpha := FSettingsButtonAlpha + FSettingsButtonShowingIncrement;
  if (ANewAlpha < 0) or (ANewAlpha > 150) then
    FSettingsButtonShowingTimer.Enabled := False
  else
  begin
    FSettingsButtonAlpha := ANewAlpha;
    InvalidateRect(GetSettingsButtonRect, True);
  end;
end;

initialization

finalization
  FreeAndNil(FCameraManager);

end.
