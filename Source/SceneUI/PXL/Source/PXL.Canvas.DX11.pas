unit PXL.Canvas.DX11;
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
  PXL.Windows.D3D11, PXL.TypeDef, PXL.Types, PXL.Textures, PXL.Canvas, PXL.Types.DX11, PXL.Shaders.DX11;

type
  TDX11Canvas = class(TCustomCanvas)
  protected const
    { The following parameters roughly affect the rendering performance. The higher values means that more primitives
      will fit in cache, but it will also occupy more bandwidth, even when few primitives are rendered.
      These parameters can be fine-tuned in a finished product to improve the  overall performance. }
    MaxCachedIndices = 8192;
    MaxCachedVertices = 8192;
  protected type
    TTopology = (Unknown, Points, Lines, Triangles);
    TProgram = (Unknown, Solid, Textured, TexturedL, TexturedLA, TexturedA, TexturedI);

    PVertexEntry = ^TVertexEntry;
    TVertexEntry = packed record
      X, Y: Single;
      Color: LongWord;
      U, V: Single;
    end;

    TIndexEntry = Word;
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF} FContext: TDX11DeviceContext;

    FEffectSolid: TDX11ShaderEffect;
    FEffectTextured: TDX11ShaderEffect;
    FEffectTexturedL: TDX11ShaderEffect;
    FEffectTexturedLA: TDX11ShaderEffect;
    FEffectTexturedA: TDX11ShaderEffect;
    FEffectTexturedI: TDX11ShaderEffect;

    FRasterState: ID3D11RasterizerState;
    FDepthStencilState: ID3D11DepthStencilState;

    FPointSampler: ID3D11SamplerState;
    FLinearSampler: ID3D11SamplerState;
    FMipMapSampler: ID3D11SamplerState;

    FVertexBuffer: ID3D11Buffer;
    FIndexBuffer: ID3D11Buffer;

    FBlendingStates: array[TBlendingEffect, Boolean] of ID3D11BlendState;

    FVertexArray: Pointer;
    FIndexArray: Pointer;

    FCurrentVertexCount: Integer;
    FCurrentIndexCount: Integer;

    FActiveTopology: TTopology;
    FActiveTexture: TCustomBaseTexture;
    FActiveProgram: TProgram;
    FActiveShaderEffect: TDX11ShaderEffect;
    FActiveBlendingEffect: TBlendingEffect;
    FActivePremultipliedAlpha: Boolean;

    FPaletteTexture: TCustomLockableTexture;

    FNormalSize: TPoint2f;
    FScissorRect: TIntRect;
    FViewport: D3D11_VIEWPORT;

    procedure CreateEffects;
    procedure DestroyEffects;

    function InitializeEffects: Boolean;
    procedure FinalizeEffects;

    procedure CreateStaticObjects;
    procedure DestroyStaticObjects;

    function CreateDynamicBuffers: Boolean;
    procedure DestroyDynamicBuffers;

    function CreateSamplerStates: Boolean;
    procedure DestroySamplerStates;

    function CreateDeviceStates: Boolean;
    procedure DestroyDeviceStates;

    procedure ReleaseBlendingStates;

    function CreateDynamicObjects: Boolean;
    procedure DestroyDynamicObjects;

    function RetrieveViewport: Boolean;

    procedure ResetRasterState;
    procedure ResetDepthStencilState;
    procedure ResetShaderViews;
    procedure ResetBlendingState;

    procedure UpdateSamplerState(const NewProgram: TProgram);
    procedure ResetSamplerState;

    function UploadVertexBuffer: Boolean;
    function UploadIndexBuffer: Boolean;
    procedure SetBuffersAndTopology;
    procedure DrawPrimitives;

    procedure GetBlendingParams(const BlendingEffect: TBlendingEffect; const PremultipliedAlpha: Boolean;
      out SrcColor, DestColor, SrcAlpha, DestAlpha: D3D11_BLEND);
    function SetEffectStates(const BlendingEffect: TBlendingEffect; const PremultipliedAlpha: Boolean): Boolean;

    function RequestCache(const ATopology: TTopology; AProgram: TProgram; const Vertices,
      Indices: Integer; const BlendingEffect: TBlendingEffect; const Texture: TCustomBaseTexture): Boolean;

    function NextVertexEntry: Pointer;
    procedure AddVertexEntry(const Position, TexCoord: TPoint2f; const Color: Cardinal);
    procedure AddIndexEntry(const Index: Integer);
  protected
    function InitCanvas: Boolean; override;
    procedure DoneCanvas; override;

    function BeginDraw: Boolean; override;
    procedure EndDraw; override;

    function DeviceRestore: Boolean; override;
    procedure DeviceRelease; override;

    function GetClipRect: TIntRect; override;
    procedure SetClipRect(const Value: TIntRect); override;
    procedure UpdateAttributes; override;
  public
    destructor Destroy; override;

    procedure PutPixel(const Point: TPoint2f; const Color: TIntColor); override;
    procedure Line(const SrcPoint, DestPoint: TPoint2f; const Color: TColorPair); override;

    procedure DrawIndexedTriangles(const Vertices: PPoint2f; const Colors: PIntColor; const Indices: PLongInt;
      const VertexCount, TriangleCount: Integer;
      const BlendingEffect: TBlendingEffect = TBlendingEffect.Normal); override;

    procedure DrawTexturedTriangles(const Texture: TCustomBaseTexture; const Vertices, TexCoords: PPoint2f;
      const Colors: PIntColor; const Indices: PLongInt; const VertexCount, TriangleCount: Integer;
      const BlendingEffect: TBlendingEffect = TBlendingEffect.Normal); override;

    procedure Flush; override;
    procedure Reset; override;

    function SetPalette(const Palette: TIntColorPalette): Boolean; override;
    procedure ResetPalette; override;

    property Context: TDX11DeviceContext read FContext;
  end;

implementation

uses
  Windows, SysUtils, PXL.Windows.D3DCommon, PXL.Windows.DXGI, PXL.Consts, PXL.Logs, PXL.Providers;

{$REGION 'Global Types and Functions'}

{$INCLUDE PXL.Canvas.DX11.Shaders.inc}

const
  CanvasVertexLayout: array[0..2] of D3D11_INPUT_ELEMENT_DESC =
    ((SemanticName: 'POSITION';
    SemanticIndex: 0;
    Format: DXGI_FORMAT_R32G32_FLOAT;
    InputSlot: 0;
    AlignedByteOffset: 0;
    InputSlotClass: D3D11_INPUT_PER_VERTEX_DATA;
    InstanceDataStepRate: 0),

    (SemanticName: 'COLOR';
    SemanticIndex: 0;
    Format: DXGI_FORMAT_R8G8B8A8_UNORM;
    InputSlot: 0;
    AlignedByteOffset: 8;
    InputSlotClass: D3D11_INPUT_PER_VERTEX_DATA;
    InstanceDataStepRate: 0),

    (SemanticName: 'TEXCOORD';
    SemanticIndex: 0;
    Format: DXGI_FORMAT_R32G32_FLOAT;
    InputSlot: 0;
    AlignedByteOffset: 12;
    InputSlotClass: D3D11_INPUT_PER_VERTEX_DATA;
    InstanceDataStepRate: 0));

{$ENDREGION}
{$REGION 'TDX11Canvas'}

destructor TDX11Canvas.Destroy;
begin
  FreeAndNil(FPaletteTexture);

  inherited;
end;

procedure TDX11Canvas.CreateEffects;
begin
  // Solid (non-textured)
  FEffectSolid := TDX11ShaderEffect.Create(FContext);

  FEffectSolid.SetVertexLayout(@CanvasVertexLayout[0], High(CanvasVertexLayout) + 1);
  FEffectSolid.SetShaderCodes(@CanvasVertex[0], High(CanvasVertex) + 1, @CanvasSolid[0], High(CanvasSolid) + 1);

  // Textured (color / texture)
  FEffectTextured := TDX11ShaderEffect.Create(FContext);

  FEffectTextured.SetVertexLayout(@CanvasVertexLayout[0], High(CanvasVertexLayout) + 1);
  FEffectTextured.SetShaderCodes(@CanvasVertex[0], High(CanvasVertex) + 1, @CanvasTextured[0],
    High(CanvasTextured) + 1);

  // Luminance Textured (color / texture having only luminance channel)
  FEffectTexturedL := TDX11ShaderEffect.Create(FContext);

  FEffectTexturedL.SetVertexLayout(@CanvasVertexLayout[0], High(CanvasVertexLayout) + 1);
  FEffectTexturedL.SetShaderCodes(@CanvasVertex[0], High(CanvasVertex) + 1, @CanvasTexturedL[0],
    High(CanvasTexturedL) + 1);

  // Luminance-Alpha Textured (color / texture having luminance and alpha channels)
  FEffectTexturedLA := TDX11ShaderEffect.Create(FContext);

  FEffectTexturedLA.SetVertexLayout(@CanvasVertexLayout[0], High(CanvasVertexLayout) + 1);
  FEffectTexturedLA.SetShaderCodes(@CanvasVertex[0], High(CanvasVertex) + 1, @CanvasTexturedLA[0],
    High(CanvasTexturedLA) + 1);

  // Alpha Textured (color / texture having only alpha channel)
  FEffectTexturedA := TDX11ShaderEffect.Create(FContext);

  FEffectTexturedA.SetVertexLayout(@CanvasVertexLayout[0], High(CanvasVertexLayout) + 1);
  FEffectTexturedA.SetShaderCodes(@CanvasVertex[0], High(CanvasVertex) + 1, @CanvasTexturedA[0],
    High(CanvasTexturedA) + 1);

  // Indexed Textured (color / texture using 256-color palette in a separate texture)
  FEffectTexturedI := TDX11ShaderEffect.Create(FContext);

  FEffectTexturedI.SetVertexLayout(@CanvasVertexLayout[0], High(CanvasVertexLayout) + 1);
  FEffectTexturedI.SetShaderCodes(@CanvasVertex[0], High(CanvasVertex) + 1, @CanvasTexturedI[0],
    High(CanvasTexturedI) + 1);
end;

procedure TDX11Canvas.DestroyEffects;
begin
  FEffectTexturedI.Free;
  FEffectTexturedA.Free;
  FEffectTexturedLA.Free;
  FEffectTexturedL.Free;
  FEffectTextured.Free;
  FEffectSolid.Free;
end;

function TDX11Canvas.InitializeEffects: Boolean;
begin
  CreateEffects;

  if not FEffectSolid.Initialize then
    Exit(False);

  if not FEffectTextured.Initialize then
  begin
    FEffectSolid.Finalize;
    Exit(False);
  end;

  Result := FEffectTexturedL.Initialize;
  if not Result then
  begin
    FEffectTextured.Finalize;
    FEffectSolid.Finalize;
  end;

  Result := FEffectTexturedLA.Initialize;
  if not Result then
  begin
    FEffectTexturedL.Finalize;
    FEffectTextured.Finalize;
    FEffectSolid.Finalize;
  end;

  Result := FEffectTexturedA.Initialize;
  if not Result then
  begin
    FEffectTexturedLA.Finalize;
    FEffectTexturedL.Finalize;
    FEffectTextured.Finalize;
    FEffectSolid.Finalize;
  end;

  Result := FEffectTexturedI.Initialize;
  if not Result then
  begin
    FEffectTexturedA.Finalize;
    FEffectTexturedLA.Finalize;
    FEffectTexturedL.Finalize;
    FEffectTextured.Finalize;
    FEffectSolid.Finalize;
  end;
end;

procedure TDX11Canvas.FinalizeEffects;
begin
  if FEffectTexturedLA <> nil then
    FEffectTexturedLA.Finalize;

  if FEffectTextured <> nil then
    FEffectTextured.Finalize;

  if FEffectSolid <> nil then
    FEffectSolid.Finalize;

  DestroyEffects;
end;

procedure TDX11Canvas.CreateStaticObjects;
begin
  FVertexArray := AllocMem(MaxCachedVertices * SizeOf(TVertexEntry));
  FIndexArray := AllocMem(MaxCachedIndices * SizeOf(TIndexEntry));
end;

procedure TDX11Canvas.DestroyStaticObjects;
begin
  FreeMemAndNil(FIndexArray);
  FreeMemAndNil(FVertexArray);
end;

function TDX11Canvas.CreateDynamicBuffers: Boolean;
var
  Desc: D3D11_BUFFER_DESC;
begin
  if (FContext = nil) or (FContext.Device = nil) then
    Exit(False);

  // Create Vertex Buffer.
  FillChar(Desc, SizeOf(D3D11_BUFFER_DESC), 0);

  Desc.ByteWidth := SizeOf(TVertexEntry) * MaxCachedVertices;
  Desc.Usage := D3D11_USAGE_DYNAMIC;
  Desc.BindFlags := Ord(D3D11_BIND_VERTEX_BUFFER);
  Desc.MiscFlags := 0;
  Desc.CPUAccessFlags := Ord(D3D11_CPU_ACCESS_WRITE);

  PushClearFPUState;
  try
    if Failed(FContext.Device.CreateBuffer(Desc, nil, @FVertexBuffer)) then
      Exit(False);
  finally
    PopFPUState;
  end;

  // Create Index Buffer.
  FillChar(Desc, SizeOf(D3D11_BUFFER_DESC), 0);

  Desc.ByteWidth := SizeOf(TIndexEntry) * MaxCachedIndices;
  Desc.Usage := D3D11_USAGE_DYNAMIC;
  Desc.BindFlags := Ord(D3D11_BIND_INDEX_BUFFER);
  Desc.MiscFlags := 0;
  Desc.CPUAccessFlags := Ord(D3D11_CPU_ACCESS_WRITE);

  PushClearFPUState;
  try
    if Failed(FContext.Device.CreateBuffer(Desc, nil, @FIndexBuffer)) then
    begin
      FVertexBuffer := nil;
      Exit(False);
    end;
  finally
    PopFPUState;
  end;

  Result := True;
end;

procedure TDX11Canvas.DestroyDynamicBuffers;
begin
  FIndexBuffer := nil;
  FVertexBuffer := nil;
end;

function TDX11Canvas.CreateSamplerStates: Boolean;
var
  Desc: D3D11_SAMPLER_DESC;
begin
  if (FContext = nil) or (FContext.Device = nil) then
    Exit(False);

  FillChar(Desc, SizeOf(D3D11_SAMPLER_DESC), 0);

  // Create Point Sampler.
  Desc.Filter := D3D11_FILTER_MIN_MAG_MIP_POINT;
  Desc.AddressU := D3D11_TEXTURE_ADDRESS_WRAP;
  Desc.AddressV := D3D11_TEXTURE_ADDRESS_WRAP;
  Desc.AddressW := D3D11_TEXTURE_ADDRESS_WRAP;
  Desc.MaxAnisotropy := 1;
  Desc.ComparisonFunc := D3D11_COMPARISON_NEVER;
  Desc.BorderColor[0] := 1.0;
  Desc.BorderColor[1] := 1.0;
  Desc.BorderColor[2] := 1.0;
  Desc.BorderColor[3] := 1.0;
  Desc.BorderColor[0] := 1.0;

  if FContext.FeatureLevel < D3D_FEATURE_LEVEL_10_0 then
  begin
    Desc.MinLOD := -D3D11_FLOAT32_MAX;
    Desc.MaxLOD := D3D11_FLOAT32_MAX;
  end;

  PushClearFPUState;
  try
    if Failed(FContext.Device.CreateSamplerState(Desc, @FPointSampler)) then
      Exit(False);
  finally
    PopFPUState;
  end;

  // Create Linear Sampler.
  Desc.Filter := D3D11_FILTER_MIN_MAG_LINEAR_MIP_POINT;

  PushClearFPUState;
  try
    if Failed(FContext.Device.CreateSamplerState(Desc, @FLinearSampler)) then
    begin
      FPointSampler := nil;
      Exit(False);
    end;
  finally
    PopFPUState;
  end;

  // Create Mipmap Sampler.
  Desc.Filter := D3D11_FILTER_MIN_MAG_MIP_LINEAR;
  Desc.MinLOD := -D3D11_FLOAT32_MAX;
  Desc.MaxLOD := D3D11_FLOAT32_MAX;

  PushClearFPUState;
  try
    if Failed(FContext.Device.CreateSamplerState(Desc, @FMipMapSampler)) then
    begin
      FLinearSampler := nil;
      FPointSampler := nil;
      Exit(False);
    end;
  finally
    PopFPUState;
  end;

  Result := True;
end;

procedure TDX11Canvas.DestroySamplerStates;
begin
  FMipMapSampler := nil;
  FLinearSampler := nil;
  FPointSampler := nil;
end;

function TDX11Canvas.CreateDeviceStates: Boolean;
var
  RasterDesc: D3D11_RASTERIZER_DESC;
  DepthStencilDesc: D3D11_DEPTH_STENCIL_DESC;
begin
  if (FContext = nil) or (FContext.Device = nil) then
    Exit(False);

  // Create Raster state.
  FillChar(RasterDesc, SizeOf(D3D11_RASTERIZER_DESC), 0);

  RasterDesc.CullMode := D3D11_CULL_NONE;
  RasterDesc.FillMode := D3D11_FILL_SOLID;

  RasterDesc.DepthClipEnable := True;
  RasterDesc.ScissorEnable := True;

  RasterDesc.MultisampleEnable := True;
  RasterDesc.AntialiasedLineEnable := False;

  PushClearFPUState;
  try
    if Failed(FContext.Device.CreateRasterizerState(RasterDesc, @FRasterState)) then
      Exit(False);
  finally
    PopFPUState;
  end;

  // Create Depth/Stencil state.
  FillChar(DepthStencilDesc, SizeOf(D3D11_DEPTH_STENCIL_DESC), 0);

  DepthStencilDesc.DepthEnable := False;
  DepthStencilDesc.StencilEnable := False;

  PushClearFPUState;
  try
    if Failed(FContext.Device.CreateDepthStencilState(DepthStencilDesc, @FDepthStencilState)) then
    begin
      FRasterState := nil;
      Exit(False);
    end;
  finally
    PopFPUState;
  end;

  Result := True;
end;

procedure TDX11Canvas.DestroyDeviceStates;
begin
  FDepthStencilState := nil;
  FRasterState := nil;
end;

procedure TDX11Canvas.ReleaseBlendingStates;
var
  State: TBlendingEffect;
begin
  for State := High(TBlendingEffect) downto Low(TBlendingEffect) do
  begin
    FBlendingStates[State, False] := nil;
    FBlendingStates[State, True] := nil;
  end;
end;

function TDX11Canvas.CreateDynamicObjects: Boolean;
begin
  if not InitializeEffects then
    Exit(False);

  if not CreateDynamicBuffers then
  begin
    FinalizeEffects;
    Exit(False);
  end;

  if not CreateSamplerStates then
  begin
    DestroyDynamicBuffers;
    FinalizeEffects;
    Exit(False);
  end;

  if not CreateDeviceStates then
  begin
    DestroySamplerStates;
    DestroyDynamicBuffers;
    FinalizeEffects;
    Exit(False);
  end;

  Result := True;
end;

procedure TDX11Canvas.DestroyDynamicObjects;
begin
  ReleaseBlendingStates;
  DestroyDeviceStates;
  DestroyDynamicBuffers;
  DestroySamplerStates;
  FinalizeEffects;
end;

function TDX11Canvas.RetrieveViewport: Boolean;
var
  NumViewports: LongWord;
begin
  if (FContext = nil) or (FContext.Context = nil) then
    Exit(False);

  NumViewports := 1;

  PushClearFPUState;
  try
    FContext.Context.RSGetViewports(NumViewports, @FViewport);
  finally
    PopFPUState;
  end;

  if NumViewports < 1 then
    FillChar(FViewport, SizeOf(D3D11_VIEWPORT), 0);

  Result := NumViewports > 0;
end;

procedure TDX11Canvas.ResetRasterState;
var
  TempRect: D3D11_RECT;
begin
  if (FContext = nil) or (FContext.Context = nil) or (FRasterState = nil) then
    Exit;

  FScissorRect := IntRect(Round(FViewport.TopLeftX), Round(FViewport.TopLeftY), Round(FViewport.Width),
    Round(FViewport.Height));

  TempRect.Left := FScissorRect.Left;
  TempRect.Top := FScissorRect.Top;
  TempRect.Right := FScissorRect.Right;
  TempRect.Bottom := FScissorRect.Bottom;

  PushClearFPUState;
  try
    FContext.Context.RSSetState(FRasterState);
    FContext.Context.RSSetScissorRects(1, @TempRect);
  finally
    PopFPUState;
  end;
end;

procedure TDX11Canvas.ResetDepthStencilState;
begin
  if (FContext = nil) or (FContext.Context = nil) or (FDepthStencilState = nil) then
    Exit;

  PushClearFPUState;
  try
    FContext.Context.OMSetDepthStencilState(FDepthStencilState, 0);
  finally
    PopFPUState;
  end;
end;

procedure TDX11Canvas.ResetShaderViews;
var
  NullView: ID3D11ShaderResourceView;
begin
  if (FContext = nil) or (FContext.Context = nil) then
    Exit;

  NullView := nil;

  PushClearFPUState;
  try
    FContext.Context.PSSetShaderResources(0, 1, @NullView);
    FContext.Context.PSSetShaderResources(1, 1, @NullView);
  finally
    PopFPUState;
  end;
end;

procedure TDX11Canvas.ResetBlendingState;
begin
  if (FContext = nil) or (FContext.Context = nil) then
    Exit;

  PushClearFPUState;
  try
    FContext.Context.OMSetBlendState(nil, nil, $FFFFFFFF);
  finally
    PopFPUState;
  end;
end;

procedure TDX11Canvas.UpdateSamplerState(const NewProgram: TProgram);
var
  NullSampler: ID3D11SamplerState;
begin
  if (FContext = nil) or (FContext.Context = nil) then
    Exit;

  PushClearFPUState;
  try
    if NewProgram <> TProgram.TexturedI then
    begin
      if (TCanvasAttribute.Antialias in Attributes) and (TCanvasAttribute.MipMapping in Attributes) then
        FContext.Context.PSSetSamplers(0, 1, @FMipMapSampler)
      else if (TCanvasAttribute.Antialias in Attributes) and (not (TCanvasAttribute.MipMapping in Attributes)) then
        FContext.Context.PSSetSamplers(0, 1, @FLinearSampler)
      else
        FContext.Context.PSSetSamplers(0, 1, @FPointSampler);

      NullSampler := nil;
      FContext.Context.PSSetSamplers(1, 1, @NullSampler);
    end
    else
    begin
      FContext.Context.PSSetSamplers(0, 1, @FPointSampler);
      FContext.Context.PSSetSamplers(1, 1, @FPointSampler);
    end;
  finally
    PopFPUState;
  end;
end;

procedure TDX11Canvas.ResetSamplerState;
var
  NullSampler: ID3D11SamplerState;
begin
  if (FContext = nil) or (FContext.Context = nil) then
    Exit;

  NullSampler := nil;

  PushClearFPUState;
  try
    FContext.Context.PSSetSamplers(0, 1, @NullSampler);
    FContext.Context.PSSetSamplers(1, 1, @NullSampler);
  finally
    PopFPUState;
  end;
end;

procedure TDX11Canvas.Reset;
begin
  inherited;

  FCurrentVertexCount := 0;
  FCurrentIndexCount := 0;

  FActiveTopology := TTopology.Unknown;
  FActiveTexture := nil;
  FActiveProgram := TProgram.Unknown;
  FActiveShaderEffect := nil;
  FActiveBlendingEffect := TBlendingEffect.Unknown;
  FActivePremultipliedAlpha := False;

  if RetrieveViewport then
  begin
    FNormalSize.X := FViewport.Width * 0.5;
    FNormalSize.Y := FViewport.Height * 0.5;
  end
  else
    FNormalSize := UnityPoint2f;

  ResetRasterState;
  ResetDepthStencilState;
  ResetShaderViews;
  ResetSamplerState;
  ResetBlendingState;
end;

function TDX11Canvas.UploadVertexBuffer: Boolean;
var
  Mapped: D3D11_MAPPED_SUBRESOURCE;
begin
  if (FContext = nil) or (FContext.Context = nil) or (FVertexBuffer = nil) then
    Exit(False);

  if Failed(FContext.Context.Map(FVertexBuffer, 0, D3D11_MAP_WRITE_DISCARD, 0, Mapped)) then
    Exit(False);
  try
    Move(FVertexArray^, Mapped.Data^, FCurrentVertexCount * SizeOf(TVertexEntry));
  finally
    FContext.Context.Unmap(FVertexBuffer, 0);
  end;

  Result := True;
end;

function TDX11Canvas.UploadIndexBuffer: Boolean;
var
  Mapped: D3D11_MAPPED_SUBRESOURCE;
begin
  if (FContext = nil) or (FContext.Context = nil) or (FIndexBuffer = nil) then
    Exit(False);

  if Failed(FContext.Context.Map(FIndexBuffer, 0, D3D11_MAP_WRITE_DISCARD, 0, Mapped)) then
    Exit(False);
  try
    Move(FIndexArray^, Mapped.Data^, FCurrentIndexCount * SizeOf(TIndexEntry));
  finally
    FContext.Context.Unmap(FIndexBuffer, 0);
  end;

  Result := True;
end;

procedure TDX11Canvas.SetBuffersAndTopology;
var
  VertexStride, VertexOffset: LongWord;
begin
  if (FContext = nil) or (FContext.Context = nil) then
    Exit;

  VertexStride := SizeOf(TVertexEntry);
  VertexOffset := 0;

  FContext.Context.IASetVertexBuffers(0, 1, @FVertexBuffer, @VertexStride, @VertexOffset);

  case FActiveTopology of
    TTopology.Points:
      FContext.Context.IASetPrimitiveTopology(D3D11_PRIMITIVE_TOPOLOGY_POINTLIST);

    TTopology.Lines:
      FContext.Context.IASetPrimitiveTopology(D3D11_PRIMITIVE_TOPOLOGY_LINELIST);

    TTopology.Triangles:
    begin
      FContext.Context.IASetIndexBuffer(FIndexBuffer, DXGI_FORMAT_R16_UINT, 0);
      FContext.Context.IASetPrimitiveTopology(D3D11_PRIMITIVE_TOPOLOGY_TRIANGLELIST);
    end;
  end;
end;

procedure TDX11Canvas.DrawPrimitives;
begin
  if (FContext = nil) or (FContext.Context = nil) then
    Exit;

  case FActiveTopology of
    TTopology.Points,
    TTopology.Lines:
      FContext.Context.Draw(FCurrentVertexCount, 0);

    TTopology.Triangles:
      FContext.Context.DrawIndexed(FCurrentIndexCount, 0, 0);
  end;
end;

procedure TDX11Canvas.Flush;
begin
  if FCurrentVertexCount > 0 then
  begin
    PushClearFPUState;
    try
      if UploadVertexBuffer and UploadIndexBuffer then
      begin
        SetBuffersAndTopology;
        DrawPrimitives;
      end;
    finally
      PopFPUState;
    end;

    NextDrawCall;
  end;

  ResetShaderViews;
  ResetSamplerState;

  if FActiveShaderEffect <> nil then
  begin
    FActiveShaderEffect.Deactivate;
    FActiveShaderEffect := nil;
  end;

  FCurrentVertexCount := 0;
  FCurrentIndexCount := 0;
  FActiveTopology := TTopology.Unknown;
  FActiveProgram := TProgram.Unknown;
  FActiveBlendingEffect := TBlendingEffect.Unknown;

  FActiveTexture := nil;
end;

procedure TDX11Canvas.GetBlendingParams(const BlendingEffect: TBlendingEffect; const PremultipliedAlpha: Boolean;
  out SrcColor, DestColor, SrcAlpha, DestAlpha: D3D11_BLEND);
begin
  case BlendingEffect of
    TBlendingEffect.None:
      begin
        SrcColor := D3D11_BLEND_ONE;
        DestColor := D3D11_BLEND_ZERO;
        SrcAlpha := D3D11_BLEND_ONE;
        DestAlpha := D3D11_BLEND_ZERO;
      end;

    TBlendingEffect.Normal:
      if not PremultipliedAlpha then
      begin
        SrcColor := D3D11_BLEND_SRC_ALPHA;
        DestColor := D3D11_BLEND_INV_SRC_ALPHA;
        SrcAlpha := D3D11_BLEND_ONE;
        DestAlpha := D3D11_BLEND_ONE;
      end
      else
      begin
        SrcColor := D3D11_BLEND_ONE;
        DestColor := D3D11_BLEND_INV_SRC_ALPHA;
        SrcAlpha := D3D11_BLEND_ONE;
        DestAlpha := D3D11_BLEND_ONE;
      end;

    TBlendingEffect.Shadow:
      begin
        SrcColor := D3D11_BLEND_ZERO;
        DestColor := D3D11_BLEND_INV_SRC_ALPHA;
        SrcAlpha := D3D11_BLEND_ZERO;
        DestAlpha := D3D11_BLEND_INV_SRC_ALPHA;
      end;

    TBlendingEffect.Add:
      if not PremultipliedAlpha then
      begin
        SrcColor := D3D11_BLEND_SRC_ALPHA;
        DestColor := D3D11_BLEND_ONE;
        SrcAlpha := D3D11_BLEND_ONE;
        DestAlpha := D3D11_BLEND_ONE;
      end
      else
      begin
        SrcColor := D3D11_BLEND_ONE;
        DestColor := D3D11_BLEND_ONE;
        SrcAlpha := D3D11_BLEND_ONE;
        DestAlpha := D3D11_BLEND_ONE;
      end;

    TBlendingEffect.Multiply:
      begin
        SrcColor := D3D11_BLEND_ZERO;
        DestColor := D3D11_BLEND_SRC_COLOR;
        SrcAlpha := D3D11_BLEND_ZERO;
        DestAlpha := D3D11_BLEND_SRC_ALPHA;
      end;

    TBlendingEffect.InverseMultiply:
      begin
        SrcColor := D3D11_BLEND_ZERO;
        DestColor := D3D11_BLEND_INV_SRC_COLOR;
        SrcAlpha := D3D11_BLEND_ZERO;
        DestAlpha := D3D11_BLEND_INV_SRC_ALPHA;
      end;

    TBlendingEffect.SourceColor:
      begin
        SrcColor := D3D11_BLEND_SRC_COLOR;
        DestColor := D3D11_BLEND_INV_SRC_COLOR;
        SrcAlpha := D3D11_BLEND_SRC_ALPHA;
        DestAlpha := D3D11_BLEND_INV_SRC_ALPHA;
      end;

    TBlendingEffect.SourceColorAdd:
      begin
        SrcColor := D3D11_BLEND_SRC_COLOR;
        DestColor := D3D11_BLEND_ONE;
        SrcAlpha := D3D11_BLEND_SRC_ALPHA;
        DestAlpha := D3D11_BLEND_ONE;
      end;
  end;
end;

function TDX11Canvas.SetEffectStates(const BlendingEffect: TBlendingEffect; const PremultipliedAlpha: Boolean): Boolean;
var
  BlendDesc: D3D11_BLEND_DESC;
begin
  if FContext = nil then
    Exit(False);

  if FBlendingStates[BlendingEffect, PremultipliedAlpha] = nil then
  begin
    if FContext.Device = nil then
      Exit(False);

    FillChar(BlendDesc, SizeOf(D3D11_BLEND_DESC), 0);

    BlendDesc.RenderTarget[0].BlendEnable := True;
    BlendDesc.RenderTarget[0].BlendOp := D3D11_BLEND_OP_ADD;
    BlendDesc.RenderTarget[0].BlendOpAlpha := D3D11_BLEND_OP_ADD;
    BlendDesc.RenderTarget[0].RenderTargetWriteMask := Ord(D3D11_COLOR_WRITE_ENABLE_ALL);

    GetBlendingParams(BlendingEffect, PremultipliedAlpha, BlendDesc.RenderTarget[0].SrcBlend,
      BlendDesc.RenderTarget[0].DestBlend, BlendDesc.RenderTarget[0].SrcBlendAlpha,
      BlendDesc.RenderTarget[0].DestBlendAlpha);

    PushClearFPUState;
    try
      if Failed(FContext.Device.CreateBlendState(BlendDesc, @FBlendingStates[BlendingEffect, PremultipliedAlpha])) then
        Exit(False);
    finally
      PopFPUState;
    end;
  end;

  if FContext.Context = nil then
    Exit(False);

  PushClearFPUState;
  try
    FContext.Context.OMSetBlendState(FBlendingStates[BlendingEffect, PremultipliedAlpha], nil, $FFFFFFFF);
  finally
    PopFPUState;
  end;

  Result := True;
end;

function TDX11Canvas.RequestCache(const ATopology: TTopology; AProgram: TProgram; const Vertices, Indices: Integer;
  const BlendingEffect: TBlendingEffect; const Texture: TCustomBaseTexture): Boolean;
var
  PremultipliedAlpha: Boolean;
begin
  if (Vertices > MaxCachedVertices) or (Indices > MaxCachedIndices) then
  begin
    LogText(SCanvasGeometryTooBig, TLogType.Error);
    Exit(False);
  end;

  Result := True;

  if (Texture <> nil) and (AProgram = TProgram.Textured) then
  begin
    if Texture.PixelFormat in [TPixelFormat.L8, TPixelFormat.L16] then
      AProgram := TProgram.TexturedL
    else if Texture.PixelFormat in [TPixelFormat.A4L4, TPixelFormat.A8L8] then
      AProgram := TProgram.TexturedLA
    else if Texture.PixelFormat = TPixelFormat.A8 then
      AProgram := TProgram.TexturedA
    else if Texture.PixelFormat = TPixelFormat.I8 then
      AProgram := TProgram.TexturedI;
  end;

  if (FCurrentVertexCount + Vertices > MaxCachedVertices) or (FCurrentIndexCount + Indices > MaxCachedIndices) or
    (FActiveTopology = TTopology.Unknown) or (FActiveTopology <> ATopology) or
    (FActiveProgram = TProgram.Unknown) or (FActiveProgram <> AProgram) or
    (FActiveBlendingEffect = TBlendingEffect.Unknown) or (FActiveBlendingEffect <> BlendingEffect) or
    (FActiveTexture <> Texture) then
  begin
    Flush;

    PremultipliedAlpha := False;

    if Texture <> nil then
      PremultipliedAlpha := Texture.PremultipliedAlpha;

    if (FActiveBlendingEffect = TBlendingEffect.Unknown) or (FActiveBlendingEffect <> BlendingEffect) or
      (FActivePremultipliedAlpha <> PremultipliedAlpha) then
      SetEffectStates(BlendingEffect, PremultipliedAlpha);

    if (FActiveTexture <> Texture) or (FActiveProgram <> AProgram) then
    begin
      if Texture <> nil then
      begin
        Texture.Bind(0);

        if (FPaletteTexture <> nil) and (AProgram = TProgram.TexturedI) then
          FPaletteTexture.Bind(1);

        UpdateSamplerState(AProgram);
      end
      else
      begin
        ResetShaderViews;
        ResetSamplerState;
      end;
    end;

    if (FActiveProgram = TProgram.Unknown) or (FActiveProgram <> AProgram) then
    begin
      case AProgram of
        TProgram.Solid:
          FActiveShaderEffect := FEffectSolid;

        TProgram.Textured:
          FActiveShaderEffect := FEffectTextured;

        TProgram.TexturedL:
          FActiveShaderEffect := FEffectTexturedL;

        TProgram.TexturedLA:
          FActiveShaderEffect := FEffectTexturedLA;

        TProgram.TexturedA:
          FActiveShaderEffect := FEffectTexturedA;

        TProgram.TexturedI:
          FActiveShaderEffect := FEffectTexturedI;

        else
          FActiveShaderEffect := nil;
      end;

      if (FActiveShaderEffect <> nil) and (not FActiveShaderEffect.Activate) then
      begin
        LogText(SCouldNotActivateShaderEffect, TLogType.Error);
        Result := False;
      end;
    end;

    FActiveTopology := ATopology;
    FActiveTexture := Texture;
    FActiveProgram := AProgram;
    FActiveBlendingEffect := BlendingEffect;
    FActivePremultipliedAlpha := PremultipliedAlpha;
  end;
end;

function TDX11Canvas.NextVertexEntry: Pointer;
begin
  Result := Pointer(PtrInt(FVertexArray) + FCurrentVertexCount * SizeOf(TVertexEntry));
end;

procedure TDX11Canvas.AddIndexEntry(const Index: Integer);
var
  Entry: PWord;
begin
  Entry := Pointer(PtrInt(FIndexArray) + FCurrentIndexCount * SizeOf(TIndexEntry));
  Entry^ := Index;

  Inc(FCurrentIndexCount);
end;

procedure TDX11Canvas.AddVertexEntry(const Position, TexCoord: TPoint2f; const Color: Cardinal);
var
  Entry: PVertexEntry;
begin
  Entry := NextVertexEntry;
  Entry.X := (Position.X - FNormalSize.X) / FNormalSize.X;
  Entry.Y := (FNormalSize.Y - Position.Y) / FNormalSize.Y;
  Entry.Color := DisplaceRB(Color);
  Entry.U := TexCoord.X;
  Entry.V := TexCoord.Y;

  Inc(FCurrentVertexCount);
end;

procedure TDX11Canvas.PutPixel(const Point: TPoint2f; const Color: TIntColor);
begin
  RequestCache(TTopology.Points, TProgram.Solid, 1, 0, TBlendingEffect.Normal, nil);

  AddVertexEntry(Point + Point2f(0.5, 0.5), ZeroPoint2f, Color);
end;

procedure TDX11Canvas.Line(const SrcPoint, DestPoint: TPoint2f; const Color: TColorPair);
begin
  RequestCache(TTopology.Lines, TProgram.Solid, 2, 0, TBlendingEffect.Normal, nil);

  AddVertexEntry(SrcPoint + Point2f(0.5, 0.5), ZeroPoint2f, Color.First);
  AddVertexEntry(DestPoint + Point2f(0.5, 0.5), ZeroPoint2f, Color.Second);
end;

procedure TDX11Canvas.DrawIndexedTriangles(const Vertices: PPoint2f; const Colors: PIntColor; const Indices: PLongInt;
  const VertexCount, TriangleCount: Integer; const BlendingEffect: TBlendingEffect);
var
  Index: PLongInt;
  Vertex: PPoint2f;
  Color: PIntColor;
  I: Integer;
begin
  RequestCache(TTopology.Triangles, TProgram.Solid, VertexCount, TriangleCount * 3, BlendingEffect, nil);

  Index := Indices;

  for I := 0 to (TriangleCount * 3) - 1 do
  begin
    AddIndexEntry(FCurrentVertexCount + Index^);
    Inc(Index);
  end;

  Vertex := Vertices;
  Color := Colors;

  for I := 0 to VertexCount - 1 do
  begin
    AddVertexEntry(Vertex^, ZeroPoint2f, Color^);

    Inc(Vertex);
    Inc(Color);
  end;
end;

procedure TDX11Canvas.DrawTexturedTriangles(const Texture: TCustomBaseTexture; const Vertices, TexCoords: PPoint2f;
  const Colors: PIntColor; const Indices: PLongInt; const VertexCount, TriangleCount: Integer;
  const BlendingEffect: TBlendingEffect);
var
  Index: PLongInt;
  Vertex, TexCoord: PPoint2f;
  Color: PIntColor;
  I: Integer;
begin
  RequestCache(TTopology.Triangles, TProgram.Textured, VertexCount, TriangleCount * 3, BlendingEffect, Texture);

  Index := Indices;

  for I := 0 to (TriangleCount * 3) - 1 do
  begin
    AddIndexEntry(FCurrentVertexCount + Index^);
    Inc(Index);
  end;

  Vertex := Vertices;
  TexCoord := TexCoords;
  Color := Colors;

  for I := 0 to VertexCount - 1 do
  begin
    AddVertexEntry(Vertex^, TexCoord^, Color^);

    Inc(Vertex);
    Inc(TexCoord);
    Inc(Color);
  end;
end;

function TDX11Canvas.InitCanvas: Boolean;
begin
  if (Device = nil) or (not (Device.Context is TDX11DeviceContext)) then
    Exit(False);

  FContext := TDX11DeviceContext(Device.Context);

  CreateStaticObjects;

  if not CreateDynamicObjects then
  begin
    DestroyStaticObjects;
    Exit(False);
  end;

  Result := True;
end;

procedure TDX11Canvas.DoneCanvas;
begin
  DestroyDynamicObjects;
  DestroyStaticObjects;
  FContext := nil;
end;

function TDX11Canvas.BeginDraw: Boolean;
begin
  Reset;
  Result := True;
end;

procedure TDX11Canvas.EndDraw;
begin
  Flush;
end;

function TDX11Canvas.DeviceRestore: Boolean;
begin
  Result := CreateDynamicObjects;
end;

procedure TDX11Canvas.DeviceRelease;
begin
  DestroyDynamicObjects;
end;

procedure TDX11Canvas.UpdateAttributes;
begin
  if FCurrentVertexCount > 0 then
    Flush;
end;

function TDX11Canvas.GetClipRect: TIntRect;
begin
  Result := FScissorRect;
end;

procedure TDX11Canvas.SetClipRect(const Value: TIntRect);
var
  TempRect: D3D11_RECT;
begin
  if FScissorRect <> Value then
  begin
    FScissorRect := Value;

    if (FContext <> nil) and (FContext.Device <> nil) then
    begin
      if FCurrentVertexCount > 0 then
        Flush;

      TempRect.Left := FScissorRect.Left;
      TempRect.Top := FScissorRect.Top;
      TempRect.Right := FScissorRect.Right;
      TempRect.Bottom := FScissorRect.Bottom;

      PushClearFPUState;
      try
        FContext.Context.RSSetScissorRects(1, @TempRect);
      finally
        PopFPUState;
      end;
    end;
  end;
end;

function TDX11Canvas.SetPalette(const Palette: TIntColorPalette): Boolean;
var
  LockedPixels: TLockedPixels;
  I: Integer;
begin
  if FPaletteTexture = nil then
  begin
    if (Device <> nil) and (not (Device.Provider is TGraphicsDeviceProvider)) then
      Exit(False);

    FPaletteTexture := TGraphicsDeviceProvider(Device.Provider).CreateLockableTexture(Device);
    if FPaletteTexture = nil then
      Exit(False);

    FPaletteTexture.Width := 256;
    FPaletteTexture.Height := 1;
    FPaletteTexture.PixelFormat := TPixelFormat.A8R8G8B8;
    FPaletteTexture.DynamicTexture := True;

    if not FPaletteTexture.Initialize then
    begin
      FreeAndNil(FPaletteTexture);
      Exit(False);
    end;
  end;

  Flush;

  if not FPaletteTexture.Lock(LockedPixels) then
    Exit(False);
  try
    for I := 0 to 255 do
      LockedPixels.Pixels[I, 0] := Palette[I];
  finally
    Result := FPaletteTexture.Unlock;
  end;
end;

procedure TDX11Canvas.ResetPalette;
begin
  Flush;
  FreeAndNil(FPaletteTexture);
end;

{$ENDREGION}

end.
