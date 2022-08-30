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
  This example illustrates a simple use of GPIO on Raspberry PI to blink a LED.

  Make sure to connect the LED to RPi pin #16 (BCM 23), please take a look at the accompanying diagram and photo.
}
uses
  Crt, PXL.Boards.Types, PXL.Boards.RPi;

const
  PinLED = 16;

var
  SystemCore: TFastSystemCore = nil;
  GPIO: TFastGPIO = nil;
  TurnedOn: Boolean = False;
begin
  SystemCore := TFastSystemCore.Create;
  try
    GPIO := TFastGPIO.Create(SystemCore);
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

        SystemCore.Delay(500000); // wait for 500 ms
      end;

      // Eat the key pressed so it won't go to terminal after we exit.
      ReadKey;

      // Turn the LED off after we are done and switch it to "Input" just to be safe.
      GPIO.PinMode[PinLED] := TPinMode.Input;
    finally
      GPIO.Free;
    end;
  finally
    SystemCore.Free;
  end;
end.

