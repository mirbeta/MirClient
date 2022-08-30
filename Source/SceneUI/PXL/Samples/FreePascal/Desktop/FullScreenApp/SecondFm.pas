unit SecondFm;
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
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs;

type
  TSecondForm = class(TForm)
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  SecondForm: TSecondForm;

implementation
{$R *.lfm}

uses
  LCLType, PXL.Types, MainFm;

procedure TSecondForm.FormResize(Sender: TObject);
begin
  if MainForm <> nil then
  begin
    MainForm.SecondarySize := Point2i(ClientWidth, ClientHeight);

    if MainForm.EngineDevice <> nil then
      MainForm.EngineDevice.Resize(1, MainForm.SecondarySize);
  end;
end;

procedure TSecondForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if (MainForm <> nil) and MainForm.Visible then
    MainForm.Close;
end;

procedure TSecondForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    Close;
end;

end.

