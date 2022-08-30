unit PXL.Shaders.GLES;
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

  SysUtils, PXL.Types, PXL.Canvas;

type
  TShaderString = {$IFDEF FPC}AnsiString{$ELSE}string{$ENDIF};

  TGLESCanvasEffect = class(TCustomCanvasEffect)
  private
    FVertexShader: GLuint;
    FFragmentShader: GLuint;
    FProgram: GLuint;
    FOnApply: TStandardNotifyEvent;

    procedure CreateAndLinkProgram;
  public
    constructor Create(const AVertexShader, AFragmentShader: GLuint); overload;
    constructor Create(const VertexShaderText, FragmentShaderText: TShaderString); overload;

    procedure Apply;

    property VertexShader: GLuint read FVertexShader;
    property FragmentShader: GLuint read FFragmentShader;
    property &Program: GLuint read FProgram;

    property OnApply: TStandardNotifyEvent read FOnApply write FOnApply;
  end;

  EGLESGeneric = class(Exception);
  EGLESCompileShader = class(EGLESGeneric);
  EGLESLinkShader = class(EGLESGeneric);
  EGLESInvalidShader = class(EGLESGeneric);

function CreateAndCompileShader(const ShaderType: GLenum; const Text: TShaderString): GLuint;
procedure DestroyAndReleaseShader(var Shader: GLuint);

var
  ShaderErrorText: TShaderString = '';

resourcestring
  SGLESCompileShader = 'Failed compiling shader [%s].';
  SGLESLinkShader = 'Failed linking shader [%s].';
  SGLESInvalidShader = 'The specified shader is invalid.';

implementation

const
  MaxShaderErrorLength = 16384;

function CreateAndCompileShader(const ShaderType: GLenum; const Text: TShaderString): GLuint;
var
{$IFDEF FPC}
  ShaderSource: PAnsiChar;
{$ELSE}
  TempBytes: TBytes;
{$ENDIF}
  TextLen: GLint;
  CompileStatus: GLint;
  ErrLength: GLsizei;
begin
  glGetError;

  TextLen := Length(Text);
  if TextLen < 1 then
    Exit(0);

  Result := glCreateShader(ShaderType);
  if Result = 0 then
    Exit;

{$IFDEF FPC}
  ShaderSource := @Text[1];
{$ELSE}
  SetLength(TempBytes, TextLen);
  TMarshal.WriteStringAsAnsi(TPtrWrapper.Create(@TempBytes[0]), Text, TextLen);
{$ENDIF}

{$IFDEF FPC}
  glShaderSource(Result, 1, @ShaderSource, @TextLen);
{$ELSE}
  glShaderSource(Result, 1, @TempBytes, @TextLen);
{$ENDIF}

  glCompileShader(Result);

  glGetShaderiv(Result, GL_COMPILE_STATUS, @CompileStatus);
  if (CompileStatus <> GL_TRUE) or (glGetError <> GL_NO_ERROR) then
  begin
    SetLength(ShaderErrorText, MaxShaderErrorLength);

    glGetShaderInfoLog(Result, MaxShaderErrorLength, @ErrLength, @ShaderErrorText[1]);
    SetLength(ShaderErrorText, ErrLength);

    glDeleteShader(Result);
    Exit(0);
  end;
end;

procedure DestroyAndReleaseShader(var Shader: GLuint);
begin
  if Shader <> 0 then
  begin
    glDeleteShader(Shader);
    Shader := 0;
  end;
end;

constructor TGLESCanvasEffect.Create(const AVertexShader, AFragmentShader: GLuint);
begin
  inherited Create;

  if (AVertexShader = 0) or (AFragmentShader = 0) then
    raise EGLESInvalidShader.Create(SGLESInvalidShader);

  FVertexShader := AVertexShader;
  FFragmentShader := AFragmentShader;

  CreateAndLinkProgram;
end;

constructor TGLESCanvasEffect.Create(const VertexShaderText, FragmentShaderText: TShaderString);
var
  VertexShader, FragmentShader: GLuint;
begin
  VertexShader := CreateAndCompileShader(GL_VERTEX_SHADER, VertexShaderText);
  if VertexShader = 0 then
    raise EGLESCompileShader.Create(Format(SGLESCompileShader, [ShaderErrorText]));

  FragmentShader := CreateAndCompileShader(GL_FRAGMENT_SHADER, FragmentShaderText);
  if FragmentShader = 0 then
    raise EGLESCompileShader.Create(Format(SGLESCompileShader, [ShaderErrorText]));

  Create(VertexShader, FragmentShader);
end;

procedure TGLESCanvasEffect.CreateAndLinkProgram;
var
  LinkStatus, InfoLength: GLint;
  ErrLength: GLsizei;
begin
  FProgram := glCreateProgram;

  glAttachShader(FProgram, FVertexShader);
  glAttachShader(FProgram, FFragmentShader);

  glBindAttribLocation(FProgram, 0, 'InPos');
  glBindAttribLocation(FProgram, 1, 'InpColor');
  glBindAttribLocation(FProgram, 2, 'InpTexCoord');

  glLinkProgram(FProgram);
  glGetProgramiv(FProgram, GL_LINK_STATUS, @LinkStatus);
  glGetProgramiv(FProgram,  GL_INFO_LOG_LENGTH, @InfoLength);

  if (LinkStatus <> GL_TRUE) or (glGetError <> GL_NO_ERROR) then
  begin
    SetLength(ShaderErrorText, MaxShaderErrorLength);

    glGetProgramInfoLog(FProgram, MaxShaderErrorLength, @ErrLength, @ShaderErrorText[1]);
    SetLength(ShaderErrorText, ErrLength);

    glDeleteProgram(FProgram);
    FProgram := 0;

    raise EGLESLinkShader.Create(Format(SGLESLinkShader, [ShaderErrorText]));
  end;
end;

procedure TGLESCanvasEffect.Apply;
begin
  glUseProgram(FProgram);

  if Assigned(FOnApply) then
    FOnApply(Self);
end;

end.
