unit PXL.Canvas.GLES;
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

  PXL.Types, PXL.Textures, PXL.Canvas, PXL.Types.GLES, PXL.Shaders.GLES;

type
  TGLESCanvas = class(TCustomCanvas)
  private const
    { The following parameters roughly affect the rendering performance. The higher values means that more primitives
      will fit in cache, but it will also occupy more bandwidth, even when few primitives are rendered.
      These parameters can be fine-tuned in a finished product to improve the overall performance. }
    MaxAllowedVertices = 2048;
    MaxAllowedIndices = 3072;
  private const
    ATTRIB_VERTEX = 0;
    ATTRIB_COLOR = 1;
    ATTRIB_TEXCOORD = 2;

    ATTRIB_VERTEX_NAME = 'InPos';
    ATTRIB_COLOR_NAME = 'InpTexCoord';
    ATTRIB_TEXCOORD_NAME = 'InpColor';
    ATTRIB_TEXTURE_NAME = 'SourceTex';
  private type
    TTopology = (Unknown, Points, Lines, Triangles);

    TVertexPoint = record
      X, Y: Single;
    end;

    TVertexIndex = Word;

    TVertexColor = LongWord;
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF} FContext: TGLESDeviceContext;

    FActiveTopology: TTopology;
    FActiveTexture: TCustomBaseTexture;
    FActiveBlendingEffect: TBlendingEffect;
    FActiveAttributes: TCanvasAttributes;
    FActivePremultipliedAlpha: Boolean;

    FVertexArray: array of TVertexPoint;
    FTexCoordArray: array of TVertexPoint;
    FColorArray: array of TVertexColor;
    FIndexArray: array of TVertexIndex;

    FCurVertexCount: Integer;
    FCurIndexCount: Integer;

    FGenericVertexShader: GLuint;
    FSolidPixelShader: GLuint;
    FTexturedPixelShader: GLuint;

    FSolidProgram: GLuint;
    FTexturedProgram: GLuint;
    FTextureLocation: GLuint;
    FCustomEffect: TGLESCanvasEffect;

    FViewNormSize: TPoint2f;

    procedure PrepareArrays;

    function CreateShaders: Boolean;
    procedure DestroyShaders;

    function CreateSolidProgram: Boolean;
    procedure DestroySolidProgram;

    function CreateTexturedProgram: Boolean;
    procedure DestroyTexturedProgram;

    function CreateResources: Boolean;
    procedure DestroyResources;

    procedure ResetStates;
    function DrawBuffers: Boolean;
    procedure ResetScene;

    procedure UpdateBlendingEffect(const BlendingEffect: TBlendingEffect; const PremultipliedAlpha: Boolean);
    procedure UpdateTexture(const Texture: TCustomBaseTexture);

    function RequestCache(const Topology: TTopology; const VertexCount, IndexCount: Integer;
      const BlendingEffect: TBlendingEffect; const Texture: TCustomBaseTexture): Boolean;

    procedure InsertVertex(const Position, TexCoord: TPoint2f; const Color: TIntColor);
    procedure InsertIndex(const Index: Integer);
  protected
    function InitCanvas: Boolean; override;
    procedure DoneCanvas; override;

    function BeginDraw: Boolean; override;
    procedure EndDraw; override;

    function DeviceRestore: Boolean; override;
    procedure DeviceRelease; override;

    function GetClipRect: TIntRect; override;
    procedure SetClipRect(const Value: TIntRect); override;
  public
    procedure PutPixel(const Point: TPoint2f; const Color: TIntColor); override;
    procedure Line(const SrcPoint, DestPoint: TPoint2f; const Color: TColorPair); override;

    procedure DrawIndexedTriangles(const Vertices: PPoint2f; const Colors: PIntColor; const Indices: PLongInt;
      const VertexCount, TriangleCount: Integer; const BlendingEffect: TBlendingEffect); override;

    procedure DrawTexturedTriangles(const Texture: TCustomBaseTexture; const Vertices, TexCoords: PPoint2f;
      const Colors: PIntColor; const Indices: PLongInt; const VertexCount, TriangleCount: Integer;
      const BlendingEffect: TBlendingEffect); override;

    procedure Flush; override;
    procedure Reset; override;

    function SetEffect(const AEffect: TCustomCanvasEffect): Boolean; override;

    property Context: TGLESDeviceContext read FContext;
  end;

implementation

uses
  SysUtils;

{$INCLUDE PXL.Canvas.GLES.VertexShader.inc}
{$INCLUDE PXL.Canvas.GLES.SolidPixelShader.inc}
{$INCLUDE PXL.Canvas.GLES.TexturedPixelShader.inc}

procedure TGLESCanvas.PrepareArrays;
begin
  SetLength(FVertexArray, MaxAllowedVertices);
  SetLength(FTexCoordArray, MaxAllowedVertices);
  SetLength(FColorArray, MaxAllowedVertices);
  SetLength(FIndexArray, MaxAllowedIndices);
end;

function TGLESCanvas.CreateShaders: Boolean;
begin
  FGenericVertexShader := CreateAndCompileShader(GL_VERTEX_SHADER, VertexShaderCode);
  if FGenericVertexShader = 0 then
    Exit(False);

  FSolidPixelShader := CreateAndCompileShader(GL_FRAGMENT_SHADER, SolidPixelShaderCode);
  if FSolidPixelShader = 0 then
  begin
    DestroyAndReleaseShader(FGenericVertexShader);
    Exit(False);
  end;

  FTexturedPixelShader := CreateAndCompileShader(GL_FRAGMENT_SHADER, TexturedPixelShaderCode);
  if FTexturedPixelShader = 0 then
  begin
    DestroyAndReleaseShader(FSolidPixelShader);
    DestroyAndReleaseShader(FGenericVertexShader);
    Exit(False);
  end;

  Result := True;
end;

procedure TGLESCanvas.DestroyShaders;
begin
  DestroyAndReleaseShader(FTexturedPixelShader);
  DestroyAndReleaseShader(FSolidPixelShader);
  DestroyAndReleaseShader(FGenericVertexShader);
end;

function TGLESCanvas.CreateSolidProgram: Boolean;
var
  LinkStatus: GLint;
begin
  FSolidProgram := glCreateProgram;

  glAttachShader(FSolidProgram, FGenericVertexShader);
  glAttachShader(FSolidProgram, FSolidPixelShader);

  glBindAttribLocation(FSolidProgram, ATTRIB_VERTEX, ATTRIB_VERTEX_NAME);
  glBindAttribLocation(FSolidProgram, ATTRIB_TEXCOORD, ATTRIB_COLOR_NAME);
  glBindAttribLocation(FSolidProgram, ATTRIB_COLOR, ATTRIB_TEXCOORD_NAME);

  glLinkProgram(FSolidProgram);
  glGetProgramiv(FSolidProgram, GL_LINK_STATUS, @LinkStatus);

  Result := (LinkStatus <> 0) and (glGetError = GL_NO_ERROR);
  if not Result then
  begin
    glDeleteProgram(FSolidProgram);
    FSolidProgram := 0;
    Exit;
  end;
end;

procedure TGLESCanvas.DestroySolidProgram;
begin
  if FSolidProgram <> 0 then
  begin
    glDeleteProgram(FSolidProgram);
    FSolidProgram := 0;
  end;
end;

function TGLESCanvas.CreateTexturedProgram: Boolean;
var
  LinkStatus: Integer;
begin
  FTexturedProgram := glCreateProgram;

  glAttachShader(FTexturedProgram, FGenericVertexShader);
  glAttachShader(FTexturedProgram, FTexturedPixelShader);

  glBindAttribLocation(FTexturedProgram, ATTRIB_VERTEX, ATTRIB_VERTEX_NAME);
  glBindAttribLocation(FTexturedProgram, ATTRIB_TEXCOORD, ATTRIB_COLOR_NAME);
  glBindAttribLocation(FTexturedProgram, ATTRIB_COLOR, ATTRIB_TEXCOORD_NAME);

  glLinkProgram(FTexturedProgram);
  glGetProgramiv(FTexturedProgram, GL_LINK_STATUS, @LinkStatus);

  Result := (LinkStatus <> 0) and (glGetError = GL_NO_ERROR);
  if not Result then
  begin
    glDeleteProgram(FTexturedProgram);
    FTexturedPixelShader := 0;
    Exit;
  end;

  FTextureLocation := glGetUniformLocation(FTexturedProgram, ATTRIB_TEXTURE_NAME);
end;

procedure TGLESCanvas.DestroyTexturedProgram;
begin
  FTextureLocation := 0;

  if FTexturedProgram <> 0 then
  begin
    glDeleteProgram(FTexturedProgram);
    FTexturedProgram := 0;
  end;
end;

function TGLESCanvas.CreateResources: Boolean;
begin
  if not CreateShaders then
    Exit(False);

  if not CreateSolidProgram then
    Exit(False);

  Result := CreateTexturedProgram;
end;

procedure TGLESCanvas.DestroyResources;
begin
  DestroyTexturedProgram;
  DestroySolidProgram;
  DestroyShaders;
end;

function TGLESCanvas.InitCanvas: Boolean;
begin
  if (Device = nil) or (not (Device.Context is TGLESDeviceContext)) then
    Exit(False);

  FContext := TGLESDeviceContext(Device.Context);

  PrepareArrays;
  Result := CreateResources;
end;

procedure TGLESCanvas.DoneCanvas;
begin
  DestroyResources;
  FContext := nil;
end;

function TGLESCanvas.BeginDraw: Boolean;
begin
  ResetStates;
  Result := True;
end;

procedure TGLESCanvas.EndDraw;
begin
  Flush;
end;

function TGLESCanvas.DeviceRestore: Boolean;
begin
  Result := CreateResources;
end;

procedure TGLESCanvas.DeviceRelease;
begin
  DestroyResources;
end;

function TGLESCanvas.GetClipRect: TIntRect;
var
  ScissorValues: array[0..3] of GLint;
begin
  glGetIntegerv(GL_SCISSOR_BOX, @ScissorValues[0]);

  Result.Left := ScissorValues[0];
  Result.Top := Round(FViewNormSize.Y * 2) - (ScissorValues[1] + ScissorValues[3]);
  Result.Right := Result.Left + ScissorValues[2];
  Result.Bottom := Result.Top + ScissorValues[3];
end;

procedure TGLESCanvas.SetClipRect(const Value: TIntRect);
begin
  glScissor(Value.Left, Round(FViewNormSize.Y * 2) - Value.Bottom, Value.Width, Value.Height);
end;

procedure TGLESCanvas.ResetStates;
var
  Viewport: array[0..3] of GLint;
begin
  FActiveTopology := TTopology.Unknown;
  FActiveBlendingEffect := TBlendingEffect.Unknown;
  FActiveTexture := nil;
  FActivePremultipliedAlpha := False;

  FCurVertexCount := 0;
  FCurIndexCount := 0;

  glGetIntegerv(GL_VIEWPORT, @Viewport[0]);

  FViewNormSize.X := Viewport[2] / 2;
  FViewNormSize.Y := Viewport[3] / 2;

  glDisable(GL_CULL_FACE);
  glDisable(GL_DEPTH_TEST);

  glBindBuffer(GL_ARRAY_BUFFER, 0);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);

  glDisable(GL_STENCIL_TEST);
  glColorMask(GL_TRUE, GL_TRUE, GL_TRUE, GL_TRUE);
  glActiveTexture(GL_TEXTURE0);

  glScissor(Viewport[0], Viewport[1], Viewport[2], Viewport[3]);
  glEnable(GL_SCISSOR_TEST);
end;

function TGLESCanvas.DrawBuffers: Boolean;
begin
  if FCustomEffect <> nil then
    FCustomEffect.Apply
  else if FActiveTexture <> nil then
  begin
    if FTexturedProgram = 0 then
      Exit(False);

     glUseProgram(FTexturedProgram);
     glUniform1i(FTextureLocation, 0);
  end
  else
  begin
    if FSolidProgram = 0 then
      Exit(False);

     glUseProgram(FSolidProgram);
  end;

  glVertexAttribPointer(ATTRIB_COLOR, 4, GL_UNSIGNED_BYTE, GL_TRUE, SizeOf(TVertexColor), @FColorArray[0]);
  glEnableVertexAttribArray(ATTRIB_COLOR);

  glVertexAttribPointer(ATTRIB_TEXCOORD, 2, GL_FLOAT, GL_FALSE, SizeOf(TVertexPoint), @FTexCoordArray[0]);
  glEnableVertexAttribArray(ATTRIB_TEXCOORD);

  glVertexAttribPointer(ATTRIB_VERTEX, 2, GL_FLOAT, GL_FALSE, SizeOf(TVertexPoint), @FVertexArray[0]);
  glEnableVertexAttribArray(ATTRIB_VERTEX);

  case FActiveTopology of
    TTopology.Points:
      glDrawElements(GL_POINTS, FCurIndexCount, GL_UNSIGNED_SHORT, @FIndexArray[0]);

    TTopology.Lines:
      glDrawElements(GL_LINES, FCurIndexCount, GL_UNSIGNED_SHORT, @FIndexArray[0]);

    TTopology.Triangles:
      glDrawElements(GL_TRIANGLES, FCurIndexCount, GL_UNSIGNED_SHORT, @FIndexArray[0]);
  end;

  glDisableVertexAttribArray(ATTRIB_VERTEX);
  glDisableVertexAttribArray(ATTRIB_COLOR);
  glDisableVertexAttribArray(ATTRIB_TEXCOORD);

  Result := True;
end;

procedure TGLESCanvas.ResetScene;
begin
  if FActiveTopology <> TTopology.Unknown then
  begin
    if (FCurVertexCount > 0) and (FCurIndexCount > 0) then
    begin
      DrawBuffers;
      NextDrawCall;
    end;

    FCurVertexCount := 0;
    FCurIndexCount := 0;

    FActiveTopology := TTopology.Unknown;
    glUseProgram(0);
  end;
end;

procedure TGLESCanvas.UpdateBlendingEffect(const BlendingEffect: TBlendingEffect; const PremultipliedAlpha: Boolean);
begin
  if (FActiveBlendingEffect = BlendingEffect) and (FActivePremultipliedAlpha = PremultipliedAlpha) then
    Exit;

  if BlendingEffect = TBlendingEffect.Unknown then
  begin
    glBlendFunc(GL_ONE, GL_ZERO);
    glDisable(GL_BLEND);
  end
  else
    glEnable(GL_BLEND);

  case BlendingEffect of
    TBlendingEffect.Normal:
      if not PremultipliedAlpha then
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
      else
        glBlendFunc(GL_ONE, GL_ONE_MINUS_SRC_ALPHA);

    TBlendingEffect.Shadow:
      glBlendFunc(GL_ZERO, GL_ONE_MINUS_SRC_ALPHA);

    TBlendingEffect.Add:
      if not PremultipliedAlpha then
        glBlendFunc(GL_SRC_ALPHA, GL_ONE)
      else
        glBlendFunc(GL_ONE, GL_ONE);

    TBlendingEffect.Multiply:
      glBlendFunc(GL_ZERO, GL_SRC_COLOR);

    TBlendingEffect.InverseMultiply:
       glBlendFunc(GL_ZERO, GL_ONE_MINUS_SRC_COLOR);

    TBlendingEffect.SourceColor:
      glBlendFunc(GL_SRC_COLOR, GL_ONE_MINUS_SRC_COLOR);

    TBlendingEffect.SourceColorAdd:
      glBlendFunc(GL_SRC_COLOR, GL_ONE);
  end;

  FActiveBlendingEffect := BlendingEffect;
  FActivePremultipliedAlpha := PremultipliedAlpha;
end;

procedure TGLESCanvas.UpdateTexture(const Texture: TCustomBaseTexture);
begin
  if (FActiveTexture = Texture) and (FActiveAttributes = Attributes) then
    Exit;

  if Texture <> nil then
  begin
    Texture.Bind(0);

    if TCanvasAttribute.Antialias in Attributes then
    begin
      if Texture.MipMapping and (TCanvasAttribute.MipMapping in Attributes) then
        glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR)
      else
        glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);

      glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    end
    else
    begin
      if Texture.MipMapping and (TCanvasAttribute.MipMapping in Attributes) then
        glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_NEAREST)
      else
        glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);

      glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    end;
  end
  else
    glBindTexture(GL_TEXTURE_2D, 0);

  FActiveTexture := Texture;

  if FActiveTexture <> nil then
    FActiveAttributes := Attributes
  else
    FActiveAttributes := [];
end;

function TGLESCanvas.RequestCache(const Topology: TTopology; const VertexCount, IndexCount: Integer;
  const BlendingEffect: TBlendingEffect; const Texture: TCustomBaseTexture): Boolean;
var
  PremultipliedAlpha: Boolean;
begin
  if (VertexCount > MaxAllowedVertices) or (IndexCount > MaxAllowedIndices) then
    Exit(False);

  if (FCurVertexCount + VertexCount > MaxAllowedVertices) or (FCurIndexCount + IndexCount > MaxAllowedIndices) or
    (FActiveTopology <> Topology) or (FActiveBlendingEffect <> BlendingEffect) or (FActiveTexture <> Texture) or
    (FActiveAttributes <> Attributes) then
    ResetScene;

  PremultipliedAlpha := False;

  if Texture <> nil then
    PremultipliedAlpha := Texture.PremultipliedAlpha;

  UpdateBlendingEffect(BlendingEffect, PremultipliedAlpha);
  UpdateTexture(Texture);

  FActiveTopology := Topology;

  Result := True;
end;

procedure TGLESCanvas.InsertVertex(const Position, TexCoord: TPoint2f; const Color: TIntColor);
begin
  FVertexArray[FCurVertexCount].X := (Position.X - FViewNormSize.X) / FViewNormSize.X;

  if FContext.FrameBufferLevel <= 0 then
    FVertexArray[FCurVertexCount].Y := (FViewNormSize.Y - Position.Y) / FViewNormSize.Y
  else
    FVertexArray[FCurVertexCount].Y := (Position.Y - FViewNormSize.Y) / FViewNormSize.Y;

  FTexCoordArray[FCurVertexCount].X := TexCoord.X;
  FTexCoordArray[FCurVertexCount].Y := TexCoord.Y;

  if not FActivePremultipliedAlpha then
    FColorArray[FCurVertexCount] := DisplaceRB(Color)
  else
    FColorArray[FCurVertexCount] := DisplaceRB(PremultiplyAlpha(Color));

  Inc(FCurVertexCount);
end;

procedure TGLESCanvas.InsertIndex(const Index: Integer);
begin
  FIndexArray[FCurIndexCount] := Index;
  Inc(FCurIndexCount);
end;

procedure TGLESCanvas.PutPixel(const Point: TPoint2f; const Color: TIntColor);
var
  BaseIndex: Integer;
begin
  if not RequestCache(TTopology.Points, 1, 1, TBlendingEffect.Normal, nil) then
    Exit;

  BaseIndex:= FCurVertexCount;

  InsertVertex(Point + Point2f(0.5, 0.5), ZeroPoint2f, Color);
  InsertIndex(BaseIndex);
end;

procedure TGLESCanvas.Line(const SrcPoint, DestPoint: TPoint2f; const Color: TColorPair);
var
  BaseIndex: Integer;
begin
  if not RequestCache(TTopology.Lines, 2, 2, TBlendingEffect.Normal, nil) then
    Exit;

  BaseIndex:= FCurVertexCount;

  InsertVertex(SrcPoint + Point2f(0.5, 0.5), ZeroPoint2f, Color.First);
  InsertVertex(DestPoint + Point2f(0.5, 0.5), ZeroPoint2f, Color.Second);

  InsertIndex(BaseIndex);
  InsertIndex(BaseIndex + 1);
end;

procedure TGLESCanvas.DrawIndexedTriangles(const Vertices: PPoint2f; const Colors: PIntColor; const Indices: PLongInt;
  const VertexCount, TriangleCount: Integer; const BlendingEffect: TBlendingEffect);
var
  SourceIndex: PLongInt;
  SourceVertex: PPoint2f;
  SourceColor: PIntColor;
  I: Integer;
begin
  if not RequestCache(TTopology.Triangles, VertexCount, TriangleCount * 3, BlendingEffect, nil) then
    Exit;

  SourceIndex := Indices;

  for I := 0 to (TriangleCount * 3) - 1 do
  begin
    InsertIndex(FCurVertexCount + SourceIndex^);
    Inc(SourceIndex);
  end;

  SourceVertex := Vertices;
  SourceColor := Colors;

  for I := 0 to VertexCount - 1 do
  begin
    InsertVertex(SourceVertex^, ZeroPoint2f, SourceColor^);

    Inc(SourceVertex);
    Inc(SourceColor);
  end;
end;

procedure TGLESCanvas.DrawTexturedTriangles(const Texture: TCustomBaseTexture; const Vertices, TexCoords: PPoint2f;
  const Colors: PIntColor; const Indices: PLongInt; const VertexCount, TriangleCount: Integer;
  const BlendingEffect: TBlendingEffect);
var
  SourceIndex: PLongInt;
  SourceVertex: PPoint2f;
  SourceTexCoord: PPoint2f;
  SourceColor: PIntColor;
  I: Integer;
begin
  if not RequestCache(TTopology.Triangles, VertexCount, TriangleCount * 3, BlendingEffect, Texture) then
    Exit;

  SourceIndex := Indices;

  for I := 0 to (TriangleCount * 3) - 1 do
  begin
    InsertIndex(FCurVertexCount + SourceIndex^);
    Inc(SourceIndex);
  end;

  SourceVertex := Vertices;
  SourceTexCoord := TexCoords;
  SourceColor := Colors;

  for I := 0 to VertexCount - 1 do
  begin
    InsertVertex(SourceVertex^, SourceTexCoord^, SourceColor^);

    Inc(SourceVertex);
    Inc(SourceTexCoord);
    Inc(SourceColor);
  end;
end;

procedure TGLESCanvas.Flush;
begin
  ResetScene;
  UpdateTexture(nil);
end;

procedure TGLESCanvas.Reset;
begin
  RequestCache(TTopology.Unknown, 0, 0, TBlendingEffect.Unknown, nil);
end;

function TGLESCanvas.SetEffect(const AEffect: TCustomCanvasEffect): Boolean;
begin
  if AEffect is TGLESCanvasEffect then
  begin
    if AEffect <> FCustomEffect then
    begin
      Flush;
      FCustomEffect := TGLESCanvasEffect(AEffect);
    end;
    Result := True;
  end
  else
    Result := False;
end;

end.
