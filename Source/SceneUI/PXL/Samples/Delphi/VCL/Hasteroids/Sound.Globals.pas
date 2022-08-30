unit Sound.Globals;
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

{ Special note: this code was ported multiple times from earliest framework releases predating Asphyre. }

uses
  bass;

var
  MusicModule: HMusic = 0;
  EffectSamples: array[0..3] of HSample;

procedure PlaySample(const Sample: HSample; const Volume: Integer);

implementation

procedure PlaySample(const Sample: HSample; const Volume: Integer);
var
  Channel: HChannel;
begin
  Channel := BASS_SampleGetChannel(Sample, False);
  if Channel <> 0 then
  begin
    BASS_ChannelSetAttribute(Channel, BASS_ATTRIB_VOL, Volume / 100.0);
    BASS_ChannelPlay(Channel, True);
  end;
end;

end.
