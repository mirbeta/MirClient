unit PXL.Devices.FM.GL;
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
  PXL.Types, PXL.Devices, PXL.Types.GL;

type
  TFireGLDevice = class(TCustomDevice)
  private
    FContext: TGLDeviceContext;
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
  Macapi.OpenGL;

constructor TFireGLDevice.Create(const AProvider: TCustomDeviceProvider);
begin
  inherited;

  FContext := TGLDeviceContext.Create(Self);

  FTechnology := TDeviceTechnology.OpenGL;

  FTechVersion := Cardinal(FContext.Extensions.MajorVersion) shl 8;

  FTechFeatureVersion :=
    (Cardinal(FContext.Extensions.MajorVersion) shl 8) or
    (Cardinal(FContext.Extensions.MinorVersion and $0F) shl 4);
end;

destructor TFireGLDevice.Destroy;
begin
  inherited;
  
  FContext.Free;
end;

function TFireGLDevice.GetDeviceContext: TCustomDeviceContext;
begin
  Result := FContext;
end;

function TFireGLDevice.Clear(const ClearTypes: TClearTypes; const ColorValue: TIntColor; const DepthValue: Single;
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
    glClearDepth(DepthValue);
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
