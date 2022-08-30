unit PXL.Types.GLES;
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
  Classes, PXL.TypeDef, PXL.Devices;

type
  TGLESExtensions = class
  private
    FStrings: TStringList;

    FEXT_texture_format_BGRA8888: Boolean;
    FAPPLE_texture_format_BGRA8888: Boolean;
    FOES_depth24: Boolean;
    FOES_packed_depth_stencil: Boolean;
    FOES_texture_npot: Boolean;

    procedure Populate;
    procedure CheckPopularExtensions;
    function GetExtension(const ExtName: StdString): Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    property Strings: TStringList read FStrings;
    property Extension[const ExtName: StdString]: Boolean read GetExtension; default;

    property EXT_texture_format_BGRA8888: Boolean read FEXT_texture_format_BGRA8888;
    property APPLE_texture_format_BGRA8888: Boolean read FAPPLE_texture_format_BGRA8888;
    property OES_depth24: Boolean read FOES_depth24;
    property OES_packed_depth_stencil: Boolean read FOES_packed_depth_stencil;
    property OES_texture_npot: Boolean read FOES_texture_npot;
  end;

  TGLESDeviceContext = class(TCustomDeviceContext)
  private
    FExtensions: TGLESExtensions;
    FFrameBufferLevel: Integer;

    function GetExtensions: TGLESExtensions;
  public
    destructor Destroy; override;

    procedure FrameBufferLevelIncrement;
    procedure FrameBufferLevelDecrement;

    property Extensions: TGLESExtensions read GetExtensions;
    property FrameBufferLevel: Integer read FFrameBufferLevel;
  end;

{$IF DEFINED(FPC) AND NOT DEFINED(ANDROID)}
const
  GL_BGRA_EXT = $80E1;
{$ENDIF}

implementation

uses
{$IFDEF FPC}
  {$IFDEF ANDROID}
    Android.GLES2,
  {$ELSE}
    gles20,
  {$ENDIF}
{$ELSE}
  {$IFDEF ANDROID}
    Androidapi.Gles2,
  {$ENDIF}
  {$IFDEF IOS}
    iOSapi.OpenGLES,
  {$ENDIF}
{$ENDIF}

  SysUtils;

{$REGION 'Global Types and Functions'}

const
  GL_EXT_texture_format_BGRA8888 = 'GL_EXT_texture_format_BGRA8888';
  GL_APPLE_texture_format_BGRA8888 = 'GL_APPLE_texture_format_BGRA8888';
  GL_OES_depth24 = 'GL_OES_depth24';
  GL_OES_packed_depth_stencil = 'GL_OES_packed_depth_stencil';
  GL_OES_texture_npot = 'GL_OES_texture_npot';

{$ENDREGION}
{$REGION 'TGLESExtensions'}

constructor TGLESExtensions.Create;
begin
  inherited;

  FStrings := TStringList.Create;
  FStrings.CaseSensitive := False;

  Populate;
  CheckPopularExtensions;
end;

destructor TGLESExtensions.Destroy;
begin
  FStrings.Free;

  inherited;
end;

procedure TGLESExtensions.Populate;
var
  SpacePos: Integer;
{$IFDEF FPC}
  ExtText: AnsiString;
{$ELSE}
  ExtText: StdString;
{$ENDIF}
begin
{$IFDEF FPC}
  ExtText := PAnsiChar(glGetString(GL_EXTENSIONS));
{$ELSE}
  ExtText := StdString(MarshaledAString(glGetString(GL_EXTENSIONS)));
{$ENDIF}

  if glGetError <> GL_NO_ERROR then
    Exit;

  while Length(ExtText) > 0 do
  begin
    SpacePos := Pos(' ', ExtText);
    if SpacePos <> 0 then
    begin
      FStrings.Add(Trim(Copy(ExtText, 1, SpacePos - 1)));
      ExtText := Copy(ExtText, SpacePos + 1, Length(ExtText) - SpacePos);
    end
    else
    begin
      if Length(ExtText) > 0 then
        FStrings.Add(Trim(ExtText));

      Break;
    end;
  end;

  FStrings.Sorted := True;
end;

procedure TGLESExtensions.CheckPopularExtensions;
begin
  FEXT_texture_format_BGRA8888 := GetExtension(GL_EXT_texture_format_BGRA8888);
  FAPPLE_texture_format_BGRA8888 := GetExtension(GL_APPLE_texture_format_BGRA8888);
  FOES_depth24 := GetExtension(GL_OES_depth24);
  FOES_packed_depth_stencil := GetExtension(GL_OES_packed_depth_stencil);
  FOES_texture_npot := GetExtension(GL_OES_texture_npot);
end;

function TGLESExtensions.GetExtension(const ExtName: StdString): Boolean;
begin
  Result := FStrings.IndexOf(ExtName) <> -1;
end;

{$ENDREGION}
{$REGION 'TGLESDeviceContext'}

destructor TGLESDeviceContext.Destroy;
begin
  FExtensions.Free;

  inherited;
end;

function TGLESDeviceContext.GetExtensions: TGLESExtensions;
begin
  if FExtensions = nil then
    FExtensions := TGLESExtensions.Create;

  Result := FExtensions;
end;

procedure TGLESDeviceContext.FrameBufferLevelIncrement;
begin
  Inc(FFrameBufferLevel);
end;

procedure TGLESDeviceContext.FrameBufferLevelDecrement;
begin
  if FFrameBufferLevel > 0 then
    Dec(FFrameBufferLevel);
end;

{$ENDREGION}

end.
