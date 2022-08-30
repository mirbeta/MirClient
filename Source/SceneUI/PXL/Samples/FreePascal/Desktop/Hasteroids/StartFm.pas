unit StartFm;
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
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, Buttons, PXL.TypeDef, PXL.Providers;

type
  TStartForm = class(TForm)
    LogoImage: TImage;
    TopBevel: TBevel;
    NameGroup: TGroupBox;
    NameEdit: TEdit;
    ConfigGroup: TGroupBox;
    PlayButton: TBitBtn;
    CloseButton: TBitBtn;
    VSyncBox: TCheckBox;
    ProviderBox: TComboBox;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    function GetVSync: Boolean;
    function GetPlayerName: UniString;
  public
    { Public declarations }
    function CreateProvider: TGraphicsDeviceProvider;

    property VSync: Boolean read GetVSync;
    property PlayerName: UniString read GetPlayerName;
  end;

var
  StartForm: TStartForm;

implementation
{$R *.lfm}

uses
{$IFDEF MSWINDOWS}
  {$IFDEF CPUX86}PXL.Providers.DX7,{$ENDIF} PXL.Providers.DX9, PXL.Providers.DX11,
{$ELSE}
  {$IFNDEF DARWIN}
    PXL.Providers.GLES,
  {$ENDIF}
{$ENDIF}

  PXL.Providers.SRT, PXL.Providers.GL;

procedure TStartForm.FormCreate(Sender: TObject);
begin
  ProviderBox.Items.Add('Software');
{$IFDEF MSWINDOWS}
  ProviderBox.Items.Add('DirectX 11');
  ProviderBox.Items.Add('DirectX 9');
  ProviderBox.Items.Add('OpenGL');
  {$IFDEF CPUX86}
    ProviderBox.Items.Add('DirectX 7');
  {$ENDIF}
{$ELSE}
  ProviderBox.Items.Add('OpenGL');
  {$IFNDEF DARWIN}
    ProviderBox.Items.Add('OpenGL ES');
  {$ENDIF}
{$ENDIF}
  ProviderBox.ItemIndex := 1;
end;

function TStartForm.GetVSync: Boolean;
begin
  Result := VSyncBox.Checked;
end;

function TStartForm.GetPlayerName: UniString;
begin
  Result := NameEdit.Text;
end;

function TStartForm.CreateProvider: TGraphicsDeviceProvider;
begin
  case ProviderBox.ItemIndex of
    0: Result := TSRTProvider.Create(nil);
  {$IFDEF MSWINDOWS}
    1: Result := TDX11Provider.Create(nil);
    2: Result := TDX9Provider.Create(nil);
    3: Result := TGLProvider.Create(nil);
    {$IFDEF CPUX86}
      4: Result := TDX7Provider.Create(nil);
    {$ENDIF}
  {$ELSE}
    1: Result := TGLProvider.Create(nil);
    {$IFNDEF DARWIN}
      2: Result := TGLESProvider.Create(nil);
    {$ENDIF}
  {$ENDIF}
  end;
end;

end.
