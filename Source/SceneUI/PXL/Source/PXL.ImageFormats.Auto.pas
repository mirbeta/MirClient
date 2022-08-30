unit PXL.ImageFormats.Auto;
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
{< Automatic cross-platform image format handler instance creation. }
interface

{$INCLUDE PXL.Config.inc}

uses
  PXL.ImageFormats;

{ Enable the following option to use Vampyre Imaging Library on non-Windows platforms instead of LCL. This provides
  superior performance but might not work on all platforms. }
{.$DEFINE USE_VAMPYRE_IMAGING}

{$IFDEF DELPHI}
  {$DEFINE USE_VAMPYRE_IMAGING}
{$ENDIF}

{ Creates image format handler by default choosen depending on platform, OS and available support. }
function CreateDefaultImageFormatHandler(const ImageFormatManager: TImageFormatManager): TCustomImageFormatHandler;

implementation

uses
{$IFDEF MSWINDOWS}
  PXL.ImageFormats.WIC
{$ELSE}
  {$IFDEF USE_VAMPYRE_IMAGING}
    PXL.ImageFormats.Vampyre
  {$ELSE}
    PXL.ImageFormats.FCL
  {$ENDIF}
{$ENDIF};

function CreateDefaultImageFormatHandler(const ImageFormatManager: TImageFormatManager): TCustomImageFormatHandler;
begin
{$IFDEF MSWINDOWS}
  Result := TWICImageFormatHandler.Create(ImageFormatManager);
{$ELSE}
  {$IFDEF USE_VAMPYRE_IMAGING}
    Result := TVampyreImageFormatHandler.Create(ImageFormatManager);
  {$ELSE}
    Result := TFCLImageFormatHandler.Create(ImageFormatManager);
  {$ENDIF}
{$ENDIF}
end;

end.
