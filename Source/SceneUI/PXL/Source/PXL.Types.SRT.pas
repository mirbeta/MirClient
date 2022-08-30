unit PXL.Types.SRT;
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
  PXL.Types, PXL.Surfaces, PXL.Devices;

type
  TSRTDeviceContextWriter = class abstract(TCustomDeviceContextWriter)
  private
    procedure SetSurface(const Value: TConceptualPixelSurface); virtual; abstract;
    procedure SetSurfaceSize(const Value: TPoint2i); virtual; abstract;
  public
    property Surface: TConceptualPixelSurface write SetSurface;
    property SurfaceSize: TPoint2i write SetSurfaceSize;
  end;

  TSRTDeviceContext = class(TCustomDeviceContext)
  private type
    TWriter = class(TSRTDeviceContextWriter)
    protected
      procedure SetSurface(const Value: TConceptualPixelSurface); override;
      procedure SetSurfaceSize(const Value: TPoint2i); override;
    end;
  private
    FSurface: TConceptualPixelSurface;
    FSurfaceSize: TPoint2i;
  public
    constructor Create(const ADevice: TCustomDevice; out AWriter: TSRTDeviceContextWriter);

    property Surface: TConceptualPixelSurface read FSurface;
    property SurfaceSize: TPoint2i read FSurfaceSize;
  end;

implementation

{$REGION 'TSRTDeviceContext.TWriter'}

procedure TSRTDeviceContext.TWriter.SetSurface(const Value: TConceptualPixelSurface);
begin
  TSRTDeviceContext(Context).FSurface := Value;
end;

procedure TSRTDeviceContext.TWriter.SetSurfaceSize(const Value: TPoint2i);
begin
  TSRTDeviceContext(Context).FSurfaceSize := Value;
end;

{$ENDREGION}
{$REGION 'TSRTDeviceContext'}

constructor TSRTDeviceContext.Create(const ADevice: TCustomDevice; out AWriter: TSRTDeviceContextWriter);
begin
  inherited Create(ADevice);

  AWriter := TWriter.Create(Self);
end;

{$ENDREGION}

end.
