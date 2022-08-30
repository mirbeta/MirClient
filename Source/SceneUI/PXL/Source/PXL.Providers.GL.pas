unit PXL.Providers.GL;
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

{ Remove the dot to enable Legacy Canvas that uses OpenGL 1.x for rendering.
  By default, newer canvas uses OpenGL 2.x for rendering. }
{.$DEFINE UseLagacy1xCanvas}

uses
  PXL.Devices, PXL.Textures, PXL.Canvas, PXL.Providers;

type
  TGLProvider = class(TGraphicsDeviceProvider)
  public
    function CreateDevice: TCustomDevice; override;
    function CreateCanvas(const Device: TCustomDevice): TCustomCanvas; override;
    function CreateLockableTexture(const Device: TCustomDevice;
      const AutoSubscribe: Boolean): TCustomLockableTexture; override;
    function CreateDrawableTexture(const Device: TCustomDevice;
      const AutoSubscribe: Boolean): TCustomDrawableTexture; override;
  end;

implementation

uses
{$IFDEF MSWINDOWS}
  PXL.Devices.GL.Win,
{$ELSE}
  {$IFDEF DARWIN}
    {$IFDEF LCLCARBON}
      PXL.Devices.GL.Carbon,
    {$ELSE}
      PXL.Devices.GL.Cocoa,
    {$ENDIF}
  {$ELSE}
    PXL.Devices.GL.X,
  {$ENDIF}
{$ENDIF}

{$IFDEF UseLagacy1xCanvas}
  PXL.Canvas.GL.GL1,
{$ELSE}
  PXL.Canvas.GL,
{$ENDIF}

  PXL.Textures.GL;

function TGLProvider.CreateDevice: TCustomDevice;
begin
  Result := TGLDevice.Create(Self);
end;

function TGLProvider.CreateCanvas(const Device: TCustomDevice): TCustomCanvas;
begin
  Result := TGLCanvas.Create(Device);
end;

function TGLProvider.CreateLockableTexture(const Device: TCustomDevice;
  const AutoSubscribe: Boolean): TCustomLockableTexture;
begin
  Result := TGLLockableTexture.Create(Device, AutoSubscribe);
end;

function TGLProvider.CreateDrawableTexture(const Device: TCustomDevice;
  const AutoSubscribe: Boolean): TCustomDrawableTexture;
begin
  Result := TGLDrawableTexture.Create(Device, AutoSubscribe);
end;

end.
