unit PXL.Sysfs.Core;
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

uses
  PXL.Boards.Types;

type
  TSysfsSystemCore = class(TCustomSystemCore)
  protected
    function GetTickCount: TTickCounter; override;
    procedure MicroDelay(const Microseconds: Cardinal); override;
  end;

implementation

uses
  PXL.Timing;

function TSysfsSystemCore.GetTickCount: TTickCounter;
begin
  Result := Round(GetSystemTimeValue * 1000.0);
end;

procedure TSysfsSystemCore.MicroDelay(const Microseconds: Cardinal);
begin
  MicroSleep(Microseconds);
end;

end.
