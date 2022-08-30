unit PXL.Devices.GLES.X;
(*
 * This file is part of Asphyre Framework, also known as Platform eXtended Library (PXL).
 * Copyright (c) 2015 - 2017 Yuriy Kotsarenko. All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
 * compliance with the License. You may obtain a copy of the License at
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License is
 * distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and limitations under the License.
 *)
interface

{$INCLUDE PXL.Config.inc}

uses
{$IFDEF LCLGTK2}
  gtk2,
{$ENDIF}

{$IFDEF LCLQT}
  qtwidgets,
{$ENDIF}

  gles20, PXL.TypeDef, PXL.Types, PXL.Devices, PXL.SwapChains, PXL.Types.GLES;

type
  TGLESDevice = class(TCustomSwapChainDevice)
  private
    FDeviceContext: TGLESDeviceContext;

    FDisplay: EGLDisplay;
    FSurface: EGLSurface;
    FContext: EGLContext;

    FViewportSize: TPoint2i;

  {$IFDEF LCLGTK2}
    FWidget: PGtkWidget;
  {$ENDIF}

  {$IFDEF LCLQT}
    FWidget: TQtWidget;
  {$ENDIF}

    FWindow: EGLNativeWindowType;

  {$IF DEFINED(LCLGTK2) OR DEFINED(LCLQT)}
    function CreateNativeWindow(const Handle: TUntypedHandle): Boolean;
    procedure DestroyNativeWindow;
  {$ENDIF}

    function InitializeDisplay: Boolean;
    procedure FinalizeDisplay;
  protected
    function GetDeviceContext: TCustomDeviceContext; override;

    function InitDevice: Boolean; override;
    procedure DoneDevice; override;

    function ResizeSwapChain(const SwapChainIndex: Integer; const NewSwapChainInfo: PSwapChainInfo): Boolean; override;
  public
    constructor Create(const AProvider: TCustomDeviceProvider);
    destructor Destroy; override;

    function Clear(const ClearTypes: TClearTypes; const ColorValue: TIntColor; const DepthValue: Single = 1.0;
      const StencilValue: Cardinal = 0): Boolean; override;

    function BeginScene(const SwapChainIndex: Integer = 0): Boolean; override;
    function EndScene: Boolean; override;

    property Display: EGLDisplay read FDisplay;
    property Surface: EGLSurface read FSurface;
    property Context: EGLContext read FContext;
  end;

implementation

uses
{$IFDEF LCLGTK2}
  gdk2x, gtk2proc,
{$ENDIF}

{$IFDEF LCLQT}
  qt4,
{$ENDIF}

  SysUtils;

constructor TGLESDevice.Create(const AProvider: TCustomDeviceProvider);
begin
  inherited;

  FDeviceContext := TGLESDeviceContext.Create(Self);

  FTechnology := TDeviceTechnology.OpenGL_ES;
  FTechVersion := $200;
end;

destructor TGLESDevice.Destroy;
begin
  inherited;
  
  FDeviceContext.Free;
end;

function TGLESDevice.GetDeviceContext: TCustomDeviceContext;
begin
  Result := FDeviceContext;
end;

{$IF DEFINED(LCLGTK2) OR DEFINED(LCLQT)}
function TGLESDevice.CreateNativeWindow(const Handle: TUntypedHandle): Boolean;
begin
  {$IFDEF LCLGTK2}
  FWidget := GetFixedWidget(PGtkWidget(Handle));
  if FWidget = nil then
    Exit(False);

  gtk_widget_realize(FWidget);

  FWindow := EGLNativeWindowType(gdk_x11_drawable_get_xid(FWidget.window));
  if FWindow = 0 then
  begin
    gtk_widget_unrealize(FWidget);
    FWidget := nil;
  end;
  {$ENDIF}

  {$IFDEF LCLQT}
  FWidget := TQtWidget(Pointer(Handle));
  if FWidget = nil then
    Exit(False);

  FWidget.setAttribute(QtWA_NoSystemBackground);

  FWindow := EGLNativeWindowType(QWidget_winID(FWidget.Widget));
  if FWindow = 0 then
    Exit(False);
  {$ENDIF}

  Result := True;
end;

procedure TGLESDevice.DestroyNativeWindow;
begin
  {$IFDEF LCLGTK2}
  if FWidget <> nil then
  begin
    gtk_widget_unrealize(FWidget);
    FWidget := nil;
  end;
  {$ENDIF}

  {$IFDEF LCLQT}
  FWidget := nil;
  {$ENDIF}

  FWindow := 0;
end;
{$ENDIF}

function TGLESDevice.InitializeDisplay: Boolean;
const
  ContextAttribs: array[0..2] of EGLint = (EGL_CONTEXT_CLIENT_VERSION, 2, EGL_NONE);
const
  ConfigAttribs: array[0..10] of EGLint = (
    EGL_RENDERABLE_TYPE, EGL_OPENGL_ES2_BIT,
    EGL_SURFACE_TYPE, EGL_WINDOW_BIT,
    EGL_BLUE_SIZE, 8,
    EGL_GREEN_SIZE, 8,
    EGL_RED_SIZE, 8,
    EGL_NONE);
var
  SwapChainInfo: PSwapChainInfo;
  Config: EGLConfig;
  NumConfigs, Value: EGLint;
begin
  SwapChainInfo := SwapChains[0];
  if SwapChainInfo = nil then
    Exit(False);

{$IF DEFINED(LCLGTK2) OR DEFINED(LCLQT)}
  if not CreateNativeWindow(SwapChainInfo.WindowHandle) then
    Exit(False);
{$ENDIF}

  FDisplay := eglGetDisplay(EGL_DEFAULT_DISPLAY);
  if FDisplay = nil then
    Exit(False);

  if eglInitialize(FDisplay, nil, nil) <> EGL_TRUE then
    Exit(False);

  if eglChooseConfig(FDisplay, @ConfigAttribs[0], @Config, 1, @NumConfigs) <> EGL_TRUE then
  begin
    FinalizeDisplay;
    Exit(False);
  end;

{$IF DEFINED(LCLGTK2) OR DEFINED(LCLQT)}
  FSurface := eglCreateWindowSurface(FDisplay, Config, FWindow, nil);
{$ELSE}
  FSurface := eglCreateWindowSurface(FDisplay, Config, SwapChainInfo.WindowHandle, nil);
{$ENDIF}

  if FSurface = EGL_NO_SURFACE then
  begin
    FinalizeDisplay;
    Exit(False);
  end;

  FContext := eglCreateContext(FDisplay, Config, EGL_NO_CONTEXT, @ContextAttribs[0]);
  if FContext = EGL_NO_CONTEXT then
  begin
    FinalizeDisplay;
    Exit(False);
  end;

  if eglMakeCurrent(FDisplay, FSurface, FSurface, FContext) <> EGL_TRUE then
  begin
    FinalizeDisplay;
    Exit(False);
  end;

  FViewportSize := Point2i(SwapChainInfo.Width, SwapChainInfo.Height);
  Result := True;
end;

procedure TGLESDevice.FinalizeDisplay;
begin
  if FDisplay <> EGL_NO_DISPLAY then
  begin
    eglMakeCurrent(FDisplay, EGL_NO_SURFACE, EGL_NO_SURFACE, EGL_NO_CONTEXT);

    if FContext <> EGL_NO_CONTEXT then
    begin
      eglDestroyContext(FDisplay, FContext);
      FContext := EGL_NO_CONTEXT;
    end;

    if FSurface <> EGL_NO_SURFACE then
    begin
      eglDestroySurface(FDisplay, FSurface);
      FSurface := EGL_NO_SURFACE;
    end;

    eglTerminate(FDisplay);
    FDisplay := EGL_NO_DISPLAY;
  end;

{$IF DEFINED(LCLGTK2) OR DEFINED(LCLQT)}
  DestroyNativeWindow;
{$ENDIF}
end;

function TGLESDevice.InitDevice: Boolean;
begin
  if not InitializeDisplay then
    Exit(False);

  Result := True;
end;

procedure TGLESDevice.DoneDevice;
begin
  FinalizeDisplay;
end;

function TGLESDevice.ResizeSwapChain(const SwapChainIndex: Integer; const NewSwapChainInfo: PSwapChainInfo): Boolean;
begin
  if SwapChainIndex <> 0 then
    Exit(False);

  FViewportSize := Point2i(NewSwapChainInfo.Width, NewSwapChainInfo.Height);
  Result := True;
end;

function TGLESDevice.Clear(const ClearTypes: TClearTypes; const ColorValue: TIntColor;
  const DepthValue: Single; const StencilValue: Cardinal): Boolean;
var
  Flags: Cardinal;
begin
  if ClearTypes = [] then
    Exit(False);

  Flags := 0;

  if TClearType.Color in ClearTypes then
  begin
    glClearColor(TIntColorRec(ColorValue).Red / 255.0, TIntColorRec(ColorValue).Green / 255.0,
      TIntColorRec(ColorValue).Blue / 255.0, TIntColorRec(ColorValue).Alpha / 255.0);
    Flags := Flags or GL_COLOR_BUFFER_BIT;
  end;

  if TClearType.Depth in ClearTypes then
  begin
    glClearDepthf(DepthValue);
    Flags := Flags or GL_DEPTH_BUFFER_BIT;
  end;

  if TClearType.Stencil in ClearTypes then
  begin
    glClearStencil(StencilValue);
    Flags := Flags or GL_STENCIL_BUFFER_BIT;
  end;

  glClear(Flags);

  Result := glGetError = GL_NO_ERROR;
end;

function TGLESDevice.BeginScene(const SwapChainIndex: Integer): Boolean;
begin
  if SwapChainIndex <> 0 then
    Exit(False);

  glViewport(0, 0, FViewportSize.X, FViewportSize.Y);
  Result := True;
end;

function TGLESDevice.EndScene: Boolean;
begin
  eglSwapBuffers(FDisplay, FSurface);
  Result := glGetError = GL_NO_ERROR;
end;

end.
