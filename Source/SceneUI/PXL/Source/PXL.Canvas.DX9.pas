unit PXL.Canvas.DX9;
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
  Jedi.Direct3D9, PXL.Types, PXL.Textures, PXL.Canvas, PXL.Types.DX9;

type
  TDX9Canvas = class(TCustomCanvas)
  private const
    VertexFVFType = D3DFVF_XYZRHW or D3DFVF_DIFFUSE or D3DFVF_TEX1;

    { The following parameters roughly affect the rendering performance. The higher values means that more primitives
      will fit in cache, but it will also occupy more bandwidth, even when few primitives are rendered.

      These parameters can be fine-tuned in a finished product to improve the overall performance. }
    MaxCachedPrimitives = 3072;
    MaxCachedIndices = 4096;
    MaxCachedVertices = 4096;

  private type
    TTopology = (Unknown, Points, Lines, Triangles);

    PVertexRecord = ^TVertexRecord;
    TVertexRecord = packed record
      Vertex: TD3DVector;
      Rhw: Single;
      Color: LongWord;
      U: Single;
      V: Single;
    end;

    TVertexIndex = Word;
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF} FContext: TDX9DeviceContext;

    FVertexBuffer: IDirect3DVertexBuffer9;
    FIndexBuffer: IDirect3DIndexBuffer9;

    FVertexArray: packed array of TVertexRecord;
    FIndexArray: packed array of TVertexIndex;

    FMaxAllowedVertices: Integer;
    FMaxAllowedIndices: Integer;
    FMaxAllowedPrimitives: Integer;

    FCurrentVertexCount: Integer;
    FCurrentIndexCount: Integer;
    FCurrentPrimitiveCount: Integer;

    FActiveTopology: TTopology;
    FActiveTexture: TCustomBaseTexture;
    FActiveBlendingEffect: TBlendingEffect;
    FActivePremultipliedAlpha: Boolean;

    procedure UpdateMaxAllowedQuantities;
    procedure PrepareArrays;

    function CreateVideoBuffers: Boolean;
    procedure DestroyVideoBuffers;

    function UploadVertexBuffer: Boolean;
    function UploadIndexBuffer: Boolean;
    procedure DrawBuffers;

    procedure SetEffectStates(const BlendingEffect: TBlendingEffect; const PremultipliedAlpha: Boolean);

    function RequestCache(const Topology: TTopology; const Vertices, Indices: Integer;
      const BlendingEffect: TBlendingEffect; const Texture: TCustomBaseTexture): Boolean;

    function NextVertexEntry: PVertexRecord;
    procedure AddIndexEntry(const Index: Integer);
  protected
    function InitCanvas: Boolean; override;
    procedure DoneCanvas; override;

    function BeginDraw: Boolean; override;
    procedure EndDraw; override;

    function DeviceRestore: Boolean; override;
    procedure DeviceRelease; override;

    procedure UpdateAttributes; override;
    function GetClipRect: TIntRect; override;
    procedure SetClipRect(const Value: TIntRect); override;
  public
    procedure PutPixel(const Point: TPoint2f; const Color: TIntColor); override;
    procedure Line(const Point1, Point2f: TPoint2f; const Color: TColorPair); override;

    procedure DrawIndexedTriangles(const Vertices: PPoint2f; const Colors: PIntColor; const Indices: PLongInt;
      const VertexCount, TriangleCount: Integer;
      const BlendingEffect: TBlendingEffect = TBlendingEffect.Normal); override;

    procedure DrawTexturedTriangles(const Texture: TCustomBaseTexture; const Vertices, TexCoords: PPoint2f;
      const Colors: PIntColor; const Indices: PLongInt; const VertexCount, TriangleCount: Integer;
      const BlendingEffect: TBlendingEffect = TBlendingEffect.Normal); override;

    procedure Flush; override;
    procedure Reset; override;

    property Context: TDX9DeviceContext read FContext;
  end;

implementation

uses
  Windows, Math;

procedure TDX9Canvas.UpdateMaxAllowedQuantities;
begin
  with FContext.Caps do
  begin
    FMaxAllowedPrimitives := Min(MaxCachedPrimitives, MaxPrimitiveCount);
    FMaxAllowedVertices := Min(MaxCachedVertices, MaxVertexIndex);
    FMaxAllowedIndices := Min(MaxCachedIndices, MaxVertexIndex);
  end;
end;

procedure TDX9Canvas.PrepareArrays;
var
  I: Integer;
begin
  SetLength(FVertexArray, FMaxAllowedVertices);
  SetLength(FIndexArray, FMaxAllowedIndices);

  for I := 0 to Length(FVertexArray) - 1 do
  begin
    FVertexArray[I].Vertex.z := 0;
    FVertexArray[I].Rhw := 1;
  end;
end;

function TDX9Canvas.CreateVideoBuffers: Boolean;
begin
  if FContext.Direct3DDevice = nil then
    Exit(False);

  if Failed(FContext.Direct3DDevice.CreateVertexBuffer(FMaxAllowedVertices * SizeOf(TVertexRecord),
    D3DUSAGE_WRITEONLY or D3DUSAGE_DYNAMIC, VertexFVFType, D3DPOOL_DEFAULT, FVertexBuffer, nil)) then
    Exit(False);

  Result := Succeeded(FContext.Direct3DDevice.CreateIndexBuffer(FMaxAllowedIndices * SizeOf(Word),
    D3DUSAGE_WRITEONLY or D3DUSAGE_DYNAMIC, D3DFMT_INDEX16, D3DPOOL_DEFAULT, FIndexBuffer, nil));
end;

procedure TDX9Canvas.DestroyVideoBuffers;
begin
  FIndexBuffer := nil;
  FVertexBuffer := nil;
end;

function TDX9Canvas.UploadVertexBuffer: Boolean;
var
  MemAddr: Pointer;
  SizeToLock: Integer;
begin
  if FVertexBuffer = nil then
    Exit(False);

  SizeToLock := FCurrentVertexCount * SizeOf(TVertexRecord);

  if Failed(FVertexBuffer.Lock(0, SizeToLock, MemAddr, D3DLOCK_DISCARD)) then
    Exit(False);

  try
    Move(FVertexArray[0], MemAddr^, SizeToLock);
  finally
    FVertexBuffer.Unlock;
  end;

  Result := True;
end;

function TDX9Canvas.UploadIndexBuffer: Boolean;
var
  MemAddr: Pointer;
  SizeToLock: Integer;
begin
  if FIndexBuffer = nil then
    Exit(False);

  SizeToLock := FCurrentIndexCount * SizeOf(TVertexIndex);

  if Failed(FIndexBuffer.Lock(0, SizeToLock, MemAddr, D3DLOCK_DISCARD)) then
    Exit(False);

  try
    Move(FIndexArray[0], MemAddr^, SizeToLock);
  finally
    FIndexBuffer.Unlock;
  end;

  Result := True;
end;

procedure TDX9Canvas.DrawBuffers;
begin
  if FContext.Direct3DDevice = nil then
    Exit;

  with FContext.Direct3DDevice do
  begin
    SetStreamSource(0, FVertexBuffer, 0, SizeOf(TVertexRecord));
    SetIndices(FIndexBuffer);
    SetVertexShader(nil);
    SetFVF(VertexFVFType);

    case FActiveTopology of
      TTopology.Points:
        DrawPrimitive(D3DPT_POINTLIST, 0, FCurrentPrimitiveCount);

      TTopology.Lines:
        DrawPrimitive(D3DPT_LINELIST, 0, FCurrentPrimitiveCount);

      TTopology.Triangles:
        DrawIndexedPrimitive(D3DPT_TRIANGLELIST, 0, 0, FCurrentVertexCount, 0, FCurrentPrimitiveCount);
    end;
  end;

  NextDrawCall;
end;

procedure TDX9Canvas.Reset;
begin
  inherited;

  FCurrentVertexCount := 0;
  FCurrentIndexCount := 0;
  FCurrentPrimitiveCount := 0;

  FActiveTopology := TTopology.Unknown;
  FActiveBlendingEffect := TBlendingEffect.Unknown;
  FActiveTexture := nil;
  FActivePremultipliedAlpha := False;

  if FContext.Direct3DDevice = nil then
    Exit;

  with FContext.Direct3DDevice do
  begin
    SetPixelShader(nil);

    SetRenderState(D3DRS_LIGHTING, iFalse);
    SetRenderState(D3DRS_CULLMODE, D3DCULL_NONE);
    SetRenderState(D3DRS_ZENABLE, D3DZB_FALSE);
    SetRenderState(D3DRS_FOGENABLE, iFalse);

{$IFDEF PXL_NOMSAA_ANTIALIASED_LINES}
    SetRenderState(D3DRS_ANTIALIASEDLINEENABLE, iTrue);
{$ELSE}
    SetRenderState(D3DRS_ANTIALIASEDLINEENABLE, iFalse);
{$ENDIF}

    SetRenderState(D3DRS_ALPHATESTENABLE, iTrue);
    SetRenderState(D3DRS_ALPHAFUNC, D3DCMP_GREATEREQUAL);
    SetRenderState(D3DRS_ALPHAREF, $00000001);

    SetRenderState(D3DRS_ALPHABLENDENABLE, iTrue);

    SetTextureStageState(0, D3DTSS_COLOROP, D3DTOP_MODULATE);
    SetTextureStageState(0, D3DTSS_COLORARG1, D3DTA_TEXTURE);
    SetTextureStageState(0, D3DTSS_COLORARG2, D3DTA_DIFFUSE);
    SetTextureStageState(1, D3DTSS_COLOROP, D3DTOP_DISABLE);

    SetTextureStageState(0, D3DTSS_ALPHAOP, D3DTOP_MODULATE);
    SetTextureStageState(0, D3DTSS_ALPHAARG1, D3DTA_TEXTURE);
    SetTextureStageState(0, D3DTSS_ALPHAARG2, D3DTA_DIFFUSE);
    SetTextureStageState(1, D3DTSS_ALPHAOP, D3DTOP_DISABLE);

    SetSamplerState(0, D3DSAMP_MAGFILTER, D3DTEXF_LINEAR);
    SetSamplerState(0, D3DSAMP_MINFILTER, D3DTEXF_LINEAR);
    SetSamplerState(0, D3DSAMP_MIPFILTER, D3DTEXF_NONE);

    SetRenderState(D3DRS_FILLMODE, D3DFILL_SOLID);
  end;
end;

procedure TDX9Canvas.Flush;
begin
  if (FCurrentVertexCount > 0) and (FCurrentPrimitiveCount > 0) and UploadVertexBuffer and UploadIndexBuffer then
    DrawBuffers;

  FCurrentVertexCount := 0;
  FCurrentIndexCount := 0;
  FCurrentPrimitiveCount := 0;
  FActiveTopology := TTopology.Unknown;
  FActiveBlendingEffect := TBlendingEffect.Unknown;
  FActivePremultipliedAlpha := False;

  if FContext.Direct3DDevice <> nil then
    FContext.Direct3DDevice.SetTexture(0, nil);

  FActiveTexture := nil;
end;

procedure TDX9Canvas.SetEffectStates(const BlendingEffect: TBlendingEffect; const PremultipliedAlpha: Boolean);
begin
  if FContext.Direct3DDevice = nil then
    Exit;
  FContext.Direct3DDevice.SetPixelShader(nil);
  with FContext.Direct3DDevice do
    case BlendingEffect of
      TBlendingEffect.Normal, TBlendingEffect.None:
        begin
          if not PremultipliedAlpha then
            SetRenderState(D3DRS_SRCBLEND, D3DBLEND_SRCALPHA)
          else
            SetRenderState(D3DRS_SRCBLEND, D3DBLEND_ONE);

          SetRenderState(D3DRS_DESTBLEND, D3DBLEND_INVSRCALPHA);
          SetTextureStageState(0, D3DTSS_COLOROP, D3DTOP_MODULATE);
          SetTextureStageState(0, D3DTSS_ALPHAOP, D3DTOP_MODULATE);
        end;

      TBlendingEffect.Shadow:
        begin
          SetRenderState(D3DRS_SRCBLEND, D3DBLEND_ZERO);
          SetRenderState(D3DRS_DESTBLEND, D3DBLEND_INVSRCALPHA);
          SetTextureStageState(0, D3DTSS_COLOROP, D3DTOP_MODULATE);
          SetTextureStageState(0, D3DTSS_ALPHAOP, D3DTOP_MODULATE);
        end;

      TBlendingEffect.Add:
        begin
          if not PremultipliedAlpha then
            SetRenderState(D3DRS_SRCBLEND, D3DBLEND_SRCALPHA)
          else
            SetRenderState(D3DRS_SRCBLEND, D3DBLEND_ONE);

          SetRenderState(D3DRS_DESTBLEND, D3DBLEND_ONE);
          SetTextureStageState(0, D3DTSS_COLOROP, D3DTOP_MODULATE);
          SetTextureStageState(0, D3DTSS_ALPHAOP, D3DTOP_MODULATE);
        end;

      TBlendingEffect.Multiply:
        begin
          SetRenderState(D3DRS_SRCBLEND, D3DBLEND_ZERO);
          SetRenderState(D3DRS_DESTBLEND, D3DBLEND_SRCCOLOR);
          SetTextureStageState(0, D3DTSS_COLOROP, D3DTOP_MODULATE);
          SetTextureStageState(0, D3DTSS_ALPHAOP, D3DTOP_MODULATE);
//     SetRenderState(D3DRS_SRCBLEND,  D3DBLEND_ZERO);
//     SetRenderState(D3DRS_DESTBLEND, D3DBLEND_SRCCOLOR);
//     SetTextureStageState(0, D3DTSS_COLOROP, D3DTOP_MODULATE);
//     SetTextureStageState(0, D3DTSS_ALPHAOP, D3DTOP_MODULATE);
        end;

      TBlendingEffect.InverseMultiply:
        begin
          SetRenderState(D3DRS_SRCBLEND, D3DBLEND_ZERO);
          SetRenderState(D3DRS_DESTBLEND, D3DBLEND_INVSRCCOLOR);
          SetTextureStageState(0, D3DTSS_COLOROP, D3DTOP_MODULATE);
          SetTextureStageState(0, D3DTSS_ALPHAOP, D3DTOP_MODULATE);
        end;

      TBlendingEffect.SourceColor:
        begin
          SetRenderState(D3DRS_SRCBLEND, D3DBLEND_SRCCOLOR);
          SetRenderState(D3DRS_DESTBLEND, D3DBLEND_INVSRCCOLOR);
          SetTextureStageState(0, D3DTSS_COLOROP, D3DTOP_MODULATE);
          SetTextureStageState(0, D3DTSS_ALPHAOP, D3DTOP_MODULATE);
        end;

      TBlendingEffect.SourceColorAdd:
        begin
          SetRenderState(D3DRS_SRCBLEND, D3DBLEND_SRCCOLOR);
          SetRenderState(D3DRS_DESTBLEND, D3DBLEND_ONE);
          SetTextureStageState(0, D3DTSS_COLOROP, D3DTOP_MODULATE);
          SetTextureStageState(0, D3DTSS_ALPHAOP, D3DTOP_MODULATE);
        end;

      TBlendingEffect.beSrcColorAdd:
        begin
         SetRenderState(D3DRS_SRCBLEND,  D3DBLEND_SRCCOLOR);
         SetRenderState(D3DRS_DESTBLEND, D3DBLEND_ONE);
         SetTextureStageState(0, D3DTSS_COLOROP, D3DTOP_MODULATE);
         SetTextureStageState(0, D3DTSS_ALPHAOP, D3DTOP_MODULATE);
        end;
      TBlendingEffect.beBlend:
        begin
          SetRenderState(D3DRS_SRCBLEND, D3DBLEND_SRCALPHA);
          SetRenderState(D3DRS_DESTBLEND, D3DBLEND_INVSRCCOLOR);
          SetTextureStageState(0, D3DTSS_COLOROP, D3DTOP_MODULATE);
          SetTextureStageState(0, D3DTSS_ALPHAOP, D3DTOP_MODULATE);
        end;
      TBlendingEffect.beBright:
        begin
          SetRenderState(D3DRS_SRCBLEND, D3DBLEND_SRCALPHA);
          SetRenderState(D3DRS_DESTBLEND, D3DBLEND_INVSRCALPHA);
          SetTextureStageState(0, D3DTSS_COLOROP, D3DTOP_MODULATE2X);
          SetTextureStageState(0, D3DTSS_ALPHAOP, D3DTOP_MODULATE);
        end;
      TBlendingEffect.beGrayscale:
        begin
          SetRenderState(D3DRS_SRCBLEND, D3DBLEND_SRCALPHA);
          SetRenderState(D3DRS_DESTBLEND, D3DBLEND_INVSRCALPHA);
          SetTextureStageState(0, D3DTSS_COLOROP, D3DTOP_MODULATE);
          SetTextureStageState(0, D3DTSS_ALPHAOP, D3DTOP_MODULATE);
          SetPixelShader(FPixelShader);
        end;
      TBlendingEffect.fxBlend:
        begin
          SetRenderState(D3DRS_ALPHABLENDENABLE, iTrue);
          SetRenderState(D3DRS_SRCBLEND, D3DBLEND_SRCALPHA);
          SetRenderState(D3DRS_DESTBLEND, D3DBLEND_INVSRCALPHA);
          //FD3DDevice.SetRenderState(D3DRS_BLENDOP, D3DBLENDOP_ADD);
          SetTextureStageState(0, D3DTSS_COLOROP, D3DTOP_MODULATE);
          SetTextureStageState(0, D3DTSS_ALPHAOP, D3DTOP_MODULATE);
        end;
      TBlendingEffect.fxAnti:
        begin
          SetRenderState(D3DRS_ALPHABLENDENABLE, iTrue);
          SetRenderState(D3DRS_SRCBLEND, D3DBLEND_INVDESTCOLOR);
          SetRenderState(D3DRS_DESTBLEND, D3DBLEND_ONE);
        end;
    end;
end;

function TDX9Canvas.RequestCache(const Topology: TTopology; const Vertices, Indices: Integer;
  const BlendingEffect: TBlendingEffect; const Texture: TCustomBaseTexture): Boolean;
var
  PremultipliedAlpha: Boolean;
begin
  if (Vertices > FMaxAllowedVertices) or (Indices > FMaxAllowedIndices) then
    Exit(False);

  if (FCurrentVertexCount + Vertices > FMaxAllowedVertices) or (FCurrentIndexCount + Indices > FMaxAllowedIndices) or
    (FActiveTopology = TTopology.Unknown) or (FActiveTopology <> Topology) or (FActiveTexture <> Texture) or
    (FActiveBlendingEffect = TBlendingEffect.Unknown) or (FActiveBlendingEffect <> BlendingEffect) then
  begin
    Flush;

    PremultipliedAlpha := False;

    if Texture <> nil then
      PremultipliedAlpha := Texture.PremultipliedAlpha;

    if (FActiveBlendingEffect = TBlendingEffect.Unknown) or (FActiveBlendingEffect <> BlendingEffect) or
      (FActivePremultipliedAlpha <> PremultipliedAlpha) then
      SetEffectStates(BlendingEffect, PremultipliedAlpha);

    if (FContext.Direct3DDevice <> nil) and ((FActiveBlendingEffect = TBlendingEffect.Unknown) or
      (FActiveTexture <> Texture)) then
    begin
      if Texture <> nil then
        Texture.Bind(0)
      else
        FContext.Direct3DDevice.SetTexture(0, nil);
    end;

    FActiveTopology := Topology;
    FActiveBlendingEffect := BlendingEffect;
    FActiveTexture := Texture;
    FActivePremultipliedAlpha := PremultipliedAlpha;
  end;

  Result := True;
end;

function TDX9Canvas.NextVertexEntry: PVertexRecord;
begin
  Result := @FVertexArray[FCurrentVertexCount];
end;

procedure TDX9Canvas.AddIndexEntry(const Index: Integer);
begin
  FIndexArray[FCurrentIndexCount] := Index;
  Inc(FCurrentIndexCount);
end;

procedure TDX9Canvas.PutPixel(const Point: TPoint2f; const Color: TIntColor);
var
  VertexEntry: PVertexRecord;
begin
  if not RequestCache(TTopology.Points, 1, 0, TBlendingEffect.Normal, nil) then
    Exit;

  VertexEntry := NextVertexEntry;
  VertexEntry.Vertex.X := Point.X;
  VertexEntry.Vertex.Y := Point.Y;
  VertexEntry.Color := Color;

  Inc(FCurrentVertexCount);
  Inc(FCurrentPrimitiveCount);
end;

procedure TDX9Canvas.Line(const Point1, Point2f: TPoint2f; const Color: TColorPair);
var
  VertexEntry: PVertexRecord;
begin
  if not RequestCache(TTopology.Lines, 2, 0, TBlendingEffect.Normal, nil) then
    Exit;

  VertexEntry := NextVertexEntry;
  VertexEntry.Vertex.X := Point1.X;
  VertexEntry.Vertex.Y := Point1.Y;
  VertexEntry.Color := Color.First;
  Inc(FCurrentVertexCount);

  VertexEntry := NextVertexEntry;
  VertexEntry.Vertex.X := Point2f.X;
  VertexEntry.Vertex.Y := Point2f.Y;
  VertexEntry.Color := Color.Second;
  Inc(FCurrentVertexCount);

  Inc(FCurrentPrimitiveCount);
end;

procedure TDX9Canvas.DrawIndexedTriangles(const Vertices: PPoint2f; const Colors: PIntColor; const Indices: PLongInt;
  const VertexCount, TriangleCount: Integer; const BlendingEffect: TBlendingEffect);
var
  VertexEntry: PVertexRecord;
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
    AddIndexEntry(FCurrentVertexCount + SourceIndex^);
    Inc(SourceIndex);
  end;

  SourceVertex := Vertices;
  SourceColor := Colors;

  for i := 0 to VertexCount - 1 do
  begin
    VertexEntry := NextVertexEntry;
    VertexEntry.Vertex.X := SourceVertex.X - 0.5;
    VertexEntry.Vertex.Y := SourceVertex.Y - 0.5;
    VertexEntry.Color := SourceColor^;

    Inc(FCurrentVertexCount);
    Inc(SourceVertex);
    Inc(SourceColor);
  end;

  Inc(FCurrentPrimitiveCount, TriangleCount);
end;

procedure TDX9Canvas.DrawTexturedTriangles(const Texture: TCustomBaseTexture; const Vertices, TexCoords: PPoint2f;
  const Colors: PIntColor; const Indices: PLongInt; const VertexCount, TriangleCount: Integer;
  const BlendingEffect: TBlendingEffect);
var
  VertexEntry: PVertexRecord;
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
    AddIndexEntry(FCurrentVertexCount + SourceIndex^);
    Inc(SourceIndex);
  end;

  SourceVertex := Vertices;
  SourceTexCoord := TexCoords;
  SourceColor := Colors;

  for I := 0 to VertexCount - 1 do
  begin
    VertexEntry := NextVertexEntry;
    VertexEntry.Vertex.X := SourceVertex.X - 0.5;
    VertexEntry.Vertex.Y := SourceVertex.Y - 0.5;

    if not FActivePremultipliedAlpha then
      VertexEntry.Color := SourceColor^
    else
      VertexEntry.Color := PremultiplyAlpha(SourceColor^);

    VertexEntry.U := SourceTexCoord.X;
    VertexEntry.V := SourceTexCoord.Y;

    Inc(FCurrentVertexCount);
    Inc(SourceVertex);
    Inc(SourceTexCoord);
    Inc(SourceColor);
  end;

  Inc(FCurrentPrimitiveCount, TriangleCount);
end;

function TDX9Canvas.InitCanvas: Boolean;
begin
  if (Device = nil) or (not (Device.Context is TDX9DeviceContext)) then
    Exit(False);

  FContext := TDX9DeviceContext(Device.Context);

  UpdateMaxAllowedQuantities;
  PrepareArrays;
  Result := CreateVideoBuffers;

  if Assigned(FPixelShader) then
    FPixelShader := nil;
  FContext.Direct3DDevice.CreatePixelShader(FPixelShaderBuffer.GetBufferPointer, FPixelShader);
end;

procedure TDX9Canvas.DoneCanvas;
begin
  DestroyVideoBuffers;
  FContext := nil;
end;

function TDX9Canvas.BeginDraw: Boolean;
begin
  Reset;
  Result := True;
end;

procedure TDX9Canvas.EndDraw;
begin
  Flush;
end;

function TDX9Canvas.DeviceRestore: Boolean;
begin
  UpdateMaxAllowedQuantities;
  PrepareArrays;
  Result := CreateVideoBuffers;
end;

procedure TDX9Canvas.DeviceRelease;
begin
  DestroyVideoBuffers;
end;

procedure TDX9Canvas.UpdateAttributes;
begin
  inherited;

  if FContext.Direct3DDevice = nil then
    Exit;

  Flush;

  with FContext.Direct3DDevice do
  begin
    if TCanvasAttribute.Antialias in Attributes then
    begin
      SetSamplerState(0, D3DSAMP_MAGFILTER, D3DTEXF_LINEAR);
      SetSamplerState(0, D3DSAMP_MINFILTER, D3DTEXF_LINEAR);
    end
    else
    begin
      SetSamplerState(0, D3DSAMP_MAGFILTER, D3DTEXF_POINT);
      SetSamplerState(0, D3DSAMP_MINFILTER, D3DTEXF_POINT);
    end;

    if TCanvasAttribute.MipMapping in Attributes then
      SetSamplerState(0, D3DSAMP_MIPFILTER, D3DTEXF_LINEAR)
    else
      SetSamplerState(0, D3DSAMP_MIPFILTER, D3DTEXF_NONE);
  end;
end;

function TDX9Canvas.GetClipRect: TIntRect;
var
  Viewport: D3DVIEWPORT9;
begin
  if FContext.Direct3DDevice = nil then
    Exit(IntRect(0, 0, 0, 0));

  FillChar(Viewport, SizeOf(D3DVIEWPORT9), 0);

  if Failed(FContext.Direct3DDevice.GetViewport(Viewport)) then
    Exit(IntRect(0, 0, 0, 0));

  Result.Left := Viewport.X;
  Result.Top := Viewport.Y;
  Result.Right := Viewport.X + Viewport.Width;
  Result.Bottom := Viewport.Y + Viewport.Height;
end;

procedure TDX9Canvas.SetClipRect(const Value: TIntRect);
var
  NewViewport, PrevViewport: D3DVIEWPORT9;
begin
  if FContext.Direct3DDevice = nil then
    Exit;

  FillChar(PrevViewport, SizeOf(D3DVIEWPORT9), 0);

  if Failed(FContext.Direct3DDevice.GetViewport(PrevViewport)) then
    Exit;

  NewViewport.X := Value.Left;
  NewViewport.Y := Value.Top;
  NewViewport.Width := Value.Width;
  NewViewport.Height := Value.Height;
  NewViewport.MinZ := PrevViewport.MinZ;
  NewViewport.MaxZ := PrevViewport.MaxZ;

  if (PrevViewport.X <> NewViewport.X) or (PrevViewport.Y <> NewViewport.Y) or
    (PrevViewport.Width <> NewViewport.Width) or (PrevViewport.Height <> NewViewport.Height) then
    begin
      Flush;
      FContext.Direct3DDevice.SetViewport(NewViewport);
    end;
end;

end.
