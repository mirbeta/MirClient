program Blinky;
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
{
  This example illustrates how to blink a LED on Intel Galileo.

  Attention! Please follow these instructions before running the sample:

   1. PinLED constant should contain pin number to which positive LED terminal is connected to.

   2. After compiling and uploading this sample, change its attributes to executable. Something like this:
        chmod +x Blinky
        ./Blinky

   3. Check the accompanying diagram and photo to see an example on how the wiring should be made.
}
uses
  Crt, PXL.Timing, PXL.Boards.Types, PXL.Boards.Galileo;

const
  PinLED = 7;

var
  GPIO: TCustomGPIO = nil;
  TurnedOn: Boolean = False;
begin
  GPIO := TGalileoGPIO.Create;
  try
    // Switch LED pin for output.
    GPIO.PinMode[PinLED] := TPinMode.Output;

    WriteLn('Blinking LED, press any key to exit...');

    while not KeyPressed do
    begin
      TurnedOn := not TurnedOn;

      if TurnedOn then
        GPIO.PinValue[PinLED] := TPinValue.High
      else
        GPIO.PinValue[PinLED] := TPinValue.Low;

      MicroSleep(500000); // wait for 500 ms
    end;

    // Eat the key pressed so it won't go to terminal after we exit.
    ReadKey;

    // Turn the LED off after we are done and switch it to "Input" just to be safe.
    GPIO.PinMode[PinLED] := TPinMode.Input;
  finally
    GPIO.Free;
  end;
end.

