unit PXL.Devices.FM.GLES;
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
  System.Messaging, PXL.Types, PXL.Devices, PXL.Types.GLES;

type
  TFireGLESDevice = class(TCustomDevice)
  private
    FContext: TGLESDeviceContext;

    ContextResetId: Integer;
    ContextLostId: Integer;

    procedure ContextResetHandler(const Sender: TObject; const Msg: TMessage);
    procedure ContextLostHandler(const Sender: TObject; const Msg: TMessage);
  protected
    function GetDeviceContext: TCustomDeviceContext; override;
  public
    constructor Create(const AProvider: TCustomDeviceProvider);
    destructor Destroy; override;

    function Clear(const ClearTypes: TClearTypes; const ColorValue: TIntColor; const DepthValue: Single = 1.0;
      const StencilValue: Cardinal = 0): Boolean; override;
  end;

implementation

uses
{$IFDEF ANDROID}
  Androidapi.Gles2,
{$ENDIF}

{$IFDEF IOS}
  iOSapi.OpenGLES,
{$ENDIF}

  FMX.Types3D;

constructor TFireGLESDevice.Create;
begin
  inherited;

  FContext := TGLESDeviceContext.Create(Self);

  FTechnology := TDeviceTechnology.OpenGL_ES;
  FTechVersion := $200;

  ContextResetId := TMessageManager.DefaultManager.SubscribeToMessage(TContextResetMessage, ContextResetHandler);
  ContextLostId := TMessageManager.DefaultManager.SubscribeToMessage(TContextLostMessage, ContextLostHandler);
end;

destructor TFireGLESDevice.Destroy;
begin
  inherited;
  
  TMessageManager.DefaultManager.Unsubscribe(TContextLostMessage, ContextLostId);
  TMessageManager.DefaultManager.Unsubscribe(TContextResetMessage, ContextResetId);

  FContext.Free;
end;

function TFireGLESDevice.GetDeviceContext: TCustomDeviceContext;
begin
  Result := FContext;
end;

procedure TFireGLESDevice.ContextResetHandler(const Sender: TObject; const Msg: TMessage);
begin
  OnRestore.Notify(Self);
end;

procedure TFireGLESDevice.ContextLostHandler(const Sender: TObject; const Msg: TMessage);
begin
  OnRelease.Notify(Self);
end;

function TFireGLESDevice.Clear(const ClearTypes: TClearTypes; const ColorValue: TIntColor; const DepthValue: Single;
  const StencilValue: Cardinal): Boolean;
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

end.
