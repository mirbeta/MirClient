unit PXL.Canvas;
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
{< Canvas specification that can draw variety of shapes including lines, triangles, hexagons and images with different
   blending effects, colors and transparency. }
interface

{$INCLUDE PXL.Config.inc}

uses
  Jedi.Direct3D9, PXL.TypeDef, PXL.Types, PXL.Lists, PXL.Devices, PXL.Textures;

type
  { The blending effect that should be applied when drawing 2D primitives. }
  TBlendingEffect = (
    { Undefined blending effect. This means that blending effect has not been defined - this is used internally and
      should not be used otherwise. @br @br }
    Unknown,

    { Blending effect disabled. In this case, drawing operation is just copy operation. }
    None,

    { Normal blending effect. If drawing primitive has alpha-channel supplied, it will be alpha-blended to the
      destination depending on source alpha values. @br @br }
    Normal,

    { Shadow drawing effect. The destination surface will be multiplied by alpha-channel of the source primitive;
      thus, the rendered image will look like a shadow. @br @br }
    Shadow,

    { Additive blending effect. The source primitive will be multiplied by its alpha-channel and then added to the
      destination with saturation. @br @br }
    Add,

    { Multiplication blending effect. The destination surface will be multiplied by the source primitive. @br @br }
    Multiply,

    { Inverse multiplication effect. The destination surface will be multiplied by an inverse of the source
      primitive. @br @br }
    InverseMultiply,

    { Source color blending effect. Instead of using alpha-channel, the grayscale value of source primitive's pixels
      will be used as an alpha value for blending on destination. @br @br }
    SourceColor,

    { Source color additive blending effect. Instead of using alpha-channel, the grayscale value of source primitive's
      pixels will be used as an alpha value for multiplying source pixels, which will then be added to destination
      with saturation. @br @br }
    SourceColorAdd,
    beGrayscale,
    beSrcColorAdd,
    beBlend,
    beBright,
    fxBlend,
    fxAnti);

  { Canvas attribute that defines rendering behavior attributes. }
  TCanvasAttribute = (
    { Antialiasing should be used when rendering images. For typical implementations this means that bilinear
      filtering will be used when interpolating image pixels. }
    Antialias,

    { Mipmapping should be used when rendering images. If this attribute is not included, then mipmapping will be
      disabled even if the image to be rendered contains mipmaps. }
    MipMapping,

    { Custom shader effect will be used when rendering images. This effect needs to be set and configured prior
      drawing.  }
    CustomEffect);

  { A set of one or multiple canvas attributes. }
  TCanvasAttributes = set of TCanvasAttribute;

  TCustomCanvas = class;

  { Abstract class definition that can hold a child reference to canvas. Basically, this is an object that can own
    canvas internally and have functions that are executed along with canvas's @italic(BeginScene) and
    @italic(EndScene) calls. }
  TCustomCanvasParent = class abstract
  protected
    { Sets Parent property of specified canvas to the given value. This method is the only way to change
      @italic(TCanvas.Parent) attribute. }
    procedure SetCanvasParent(const Canvas: TCustomCanvas; const Parent: TCustomCanvasParent);

    { This function calls @italic(TCanvas.InternalBeginScene) and returns its result. }
    function CanvasInternalBeginScene(const Canvas: TCustomCanvas): Boolean;

    { This function calls @italic(TCanvas.InternalEndScene). }
    procedure CanvasInternalEndScene(const Canvas: TCustomCanvas);

    { This function is invoked when @italic(TCanvas.BeginScene) is called for owned canvas to notify this object
      that the rendering should start. }
    function CanvasBeginScene: Boolean; virtual;

    { This function is invoked when @italic(TCanvas.EndScene) is called for owned canvas to notify this object
      that the rendering should be finished. }
    procedure CanvasEndScene; virtual;
  end;

  { Abstract image that can be rendered with canvas. This provides methods that are called by canvas to obtain minimal
    information about textures and regions. }
  TCustomCanvasImage = class abstract(TCustomCanvasParent)
  protected
    { Returns the number of textures available in the image. }
    function GetTextureCount: Integer; virtual; abstract;

    { Provides access to individual textures through the specified index. If the index is outside of valid range,
      this should return @nil. }
    function GetTexture(const Index: Integer): TCustomBaseTexture; virtual; abstract;

    { Provides access to information about individual regions in the image, including texture number and region
      coordinates on such texture. These regions are also called "patterns" or "tiles" in different media. }
    function GetRegion(const Index: Integer): TIntRectList.PItem; virtual; abstract;
  end;

  { Abstract definition for canvas effect. The actual implementation varies depending on provider and platform. }
  TCustomCanvasEffect = class abstract
  end;

  { Abstract canvas definition that provides few basic functions that need to be implemented by derived classes and many
    different rendering functions that internally use basic functions to do the rendering. }
  TCustomCanvas = class abstract
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF} FParent: TCustomCanvasParent;
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF} FDevice: TCustomDevice;

    FInitialized: Boolean;
    FCacheStall: Integer;
    FAttributes: TCanvasAttributes;

    FHexagonVertices: array[0..5] of TPoint2f;

    FSceneBeginCount: Integer;
    FInitialClipRect: TIntRect;
    FClipRectQueue: TIntRectList;

    FDeviceRestoreHandle: Cardinal;
    FDeviceReleaseHandle: Cardinal;

    procedure ComputeHexagonVertices;
    procedure SetAttributes(const Value: TCanvasAttributes);

    procedure OnDeviceRestore(const Sender: TObject; const EventData, UserData: Pointer);
    procedure OnDeviceRelease(const Sender: TObject; const EventData, UserData: Pointer);

    procedure WuLineHorizontal(X1, Y1, X2, Y2: VectorFloat; const Color: TColorPair);
    procedure WuLineVertical(X1, Y1, X2, Y2: VectorFloat; const Color: TColorPair);

    function InternalBeginScene: Boolean;
    procedure InternalEndScene;
  protected
    FPixelShaderBuffer: ID3DXBuffer;
    FPixelShader: IDirect3DPixelShader9;
    { Currently defined texture for rendering. }
    {$IFDEF AUTOREFCOUNT}[unsafe]{$ENDIF} FCurrentTexture: TCustomBaseTexture;

    { Currently defined coordinates within the texture set in @link(FCurrentTexture). }
    FCurrentTextureMapping: TQuad;

    { Currently set rendering mode in relation to premultiplied or non-premultiplied alpha. }
    FCurrentPremultipliedAlpha: Boolean;

    { Depending on actual implementation, this indicates whether the canvas requires initialization or not. When this
      method returns @False, then the canvas becomes initialized at the creation and cannot be "finalized". }
    function NeedsInitialization: Boolean; virtual;

    { Creates any implementation specific resources for rendering, including hardware and/or GPU resources.
      Returns @True when successful and @False otherwise. }
    function InitCanvas: Boolean; virtual;

    { Releases any implementation specific resources for rendering, including hardware and/or GPU resources. }
    procedure DoneCanvas; virtual;

    { Prepares the canvas for rendering, after which any number of rendering calls can be made. Returns @True when
      successful and @False otherwise.}
    function BeginDraw: Boolean; virtual;

    { Finishes rendering and depending on implementation may present results on destination surface. }
    procedure EndDraw; virtual;

    { Restores the canvas after its resources have been lost (that is, after @link(DeviceRelease) call). This may be
      implemented by derived classes to handle "device lost" scenario. }
    function DeviceRestore: Boolean; virtual;

    { Releases the resources of canvas when the device has been lost. This may be implemented by derived classes to
      handle "device lost" scenario. }
    procedure DeviceRelease; virtual;

    { Returns the currently set clipping rectangle. }
    function GetClipRect: TIntRect; virtual; abstract;

    { Specifies new clipping rectangle for rendering. }
    procedure SetClipRect(const Value: TIntRect); virtual; abstract;

    { Makes the necessary arrangements so that newly set canvas attributes are taken into account for next rendering
      calls. }
    procedure UpdateAttributes; virtual;

    { This method can be called by specific implementations to indicate that canvas buffer is full and new rendering
      stage begins. Basically, this increments @link(CacheStall) by one, which by default is reset back to zero after
      call to @link(BeginScene). }
    procedure NextDrawCall; virtual;
  public
    { Creates new instance of canvas bound to the specific device. }
    constructor Create(const ADevice: TCustomDevice);

    { @exclude } destructor Destroy; override;

    { Initializes the canvas so it can be used for rendering. Note that for actual rendering to take place,
      @link(BeginScene) needs to be called first, assuming that initialization succeeded. This results @True when
      successful and @False otherwise. }
    function Initialize: Boolean;

    { Finalizes the canvas and releases any resources that were previously allocated during initialization. }
    procedure Finalize;

    { Prepares the canvas to start the rendering. Any rendering calls can be made after this method succeeds.
      Returns @True when successful and @False otherwise. }
    function BeginScene: Boolean;

    { Finishes rendering phase in the canvas. }
    procedure EndScene;

    { Draws a single pixel on the destination surface with the specified position and color (alpha-blended).
      This method is considered basic functionality and should always be implemented by derived classes. }
    procedure PutPixel(const Point: TPoint2f; const Color: TIntColor); overload; virtual; abstract;

    { Draws a single pixel on the destination surface with the specified coordinates and color (alpha-blended). }
    procedure PutPixel(const X, Y: VectorFloat; const Color: TIntColor); overload;

    { Draws line between two specified positions and filled with color gradient.
      This method is considered basic functionality and should always be implemented by derived classes. }
    procedure Line(const SrcPoint, DestPoint: TPoint2f; const Color: TColorPair); overload; virtual; abstract;

    { Draws line between two specified positions and filled with single color. }
    procedure Line(const SrcPoint, DestPoint: TPoint2f; const Color: TIntColor); overload;

    { Draws line between specified coordinate pairs and filled with color gradient. }
    procedure Line(const X1, Y1, X2, Y2: VectorFloat; const Color: TColorPair); overload;

    { Draws line between specified coordinate pairs and filled with single color. }
    procedure Line(const X1, Y1, X2, Y2: VectorFloat; const Color: TIntColor); overload;

    { Draws series of lines between specified vertices using solid color. }
    procedure LineArray(const Points: PPoint2f; const ElementCount: Integer; const Color: TIntColor); virtual;

    { Draws antialiased "wu-line" using @link(PutPixel) primitive between specified positions filled with single color. }
    procedure WuLine(const Point1, Point2f: TPoint2f; const Color: TColorPair);

    { Draws ellipse with given origin, radiuses and color. This function uses @link(Line) primitive. @code(Steps)
      parameter indicates number of divisions in the ellipse. @code(UseWuLines) indicates whether to use
      @link(WuLine) primitive instead. }
    procedure Ellipse(const Origin, Radius: TPoint2f; const Steps: Integer; const Color: TIntColor;
      const UseWuLines: Boolean = False);

    { Draws circle with given origin, radius and color. This function uses @link(Line) primitive. @code(Steps)
      parameter indicates number of divisions in the ellipse. @code(UseWuLines) determines whether to use
      @link(WuLine) primitive instead. }
    procedure Circle(const Origin: TPoint2f; const Radius: VectorFloat; const Steps: Integer; const Color: TIntColor;
      const UseWuLines: Boolean = False);

    { Draws lines between the specified vertices (making it a wireframe quadrilateral) and vertex colors. Note that
      this may not necessarily respect last pixel rendering rule). This method uses @link(Line) primitive.
      @code(UseWuLines) determines whether to use @link(WuLine) primitive instead. }
    procedure WireQuad(const Points: TQuad; const Colors: TColorRect; const UseWuLines: Boolean = False);

    { Draws lines between each vertex in hexagon. The vertices are spaced 0.5 pixels apart from its center (so diameter
      is 1) in all directions, multiplied by the given matrix and filled with gradient of six colors at the
      corresponding vertices. The size, position and rotation of hexagon can be given using one or a combination of
      several 3x3 matrices multiplied together. This method uses @link(Line) primitive. @code(UseWuLines) determines
      whether to use @link(WuLine) primitive instead.}
    procedure WireHexagon(const Matrix: TMatrix3f; const Color1, Color2, Color3, Color4, Color5, Color6: TIntColor;
      const UseWuLines: Boolean = False);

    { Draws one or more triangles filled with color gradient, specified by vertex, color and index buffers.
      This method is considered basic functionality and should always be implemented by derived classes. }
    procedure DrawIndexedTriangles(const Vertices: PPoint2f; const Colors: PIntColor; const Indices: PLongInt;
      const VertexCount, TriangleCount: Integer;
      const BlendingEffect: TBlendingEffect = TBlendingEffect.Normal); virtual; abstract;

    { Draws triangle filled with color gradient specified by given positions and colors. }
    procedure FillTri(const Point1, Point2f, Point3: TPoint2f; const Color1, Color2, Color3: TIntColor;
      const BlendingEffect: TBlendingEffect = TBlendingEffect.Normal);

    { Draws quadrilateral with color gradient specified by given vertices and colors. }
    procedure FillQuad(const Points: TQuad; const Colors: TColorRect;
      const BlendingEffect: TBlendingEffect = TBlendingEffect.Normal);

    { Draws rectangle with color gradient specified by given margins and colors. }
    procedure FillRect(const Rect: TFloatRect; const Colors: TColorRect;
      const BlendingEffect: TBlendingEffect = TBlendingEffect.Normal); overload;

    { Draws rectangle filled with single color and specified by given margins, and colors. }
    procedure FillRect(const Rect: TFloatRect; const Color: TIntColor;
      const BlendingEffect: TBlendingEffect = TBlendingEffect.Normal); overload;

    { Draws rectangle filled with single color and specified by given coordinates, and colors. }
    procedure FillRect(const Left, Top, Width, Height: VectorFloat; const Color: TIntColor;
      const BlendingEffect: TBlendingEffect = TBlendingEffect.Normal); overload;

    { Draws rectangle with line width of one pixel specified by given vertices and colors. Although this may receive
      coordinates for shapes other than rectangle (for example, quadrilateral), the result may be unpredictable.
      This method unlike other line drawing methods uses filled shapes and assumes that four vertices are aligned to
      form rectangle. The produced result respects last pixel rule and can be used for drawing UI elements (whereas
      methods like @link(WireQuad) may produce incorrectly sized rectangles depending on implementation). }
    procedure FrameRect(const Points: TQuad; const Colors: TColorRect;
      const BlendingEffect: TBlendingEffect = TBlendingEffect.Normal); overload;

    { Draws rectangle with line width of one pixel specified by given margins and colors. This works in similar
      fashion as other overloaded @italic(FrameRect) method by drawing filled shapes instead of lines, and is meant
      for rendering UI elements. }
    procedure FrameRect(const Rect: TFloatRect; const Colors: TColorRect;
      const BlendingEffect: TBlendingEffect = TBlendingEffect.Normal); overload;

    { Draws horizontal line with specified coordinates and color gradient. This method uses filled shapes instead of
      actual lines to produce accurate results and is meant for rendering UI elements. }
    procedure HorizLine(const Left, Top, Width: VectorFloat; const Colors: TColorPair;
      const BlendingEffect: TBlendingEffect = TBlendingEffect.Normal); overload;

    { Draws horizontal line with specified coordinates and single color. This method uses filled shapes instead of
      actual lines to produce accurate results and is meant for rendering UI elements. }
    procedure HorizLine(const Left, Top, Width: VectorFloat; const Color: TIntColor;
      const BlendingEffect: TBlendingEffect = TBlendingEffect.Normal); overload;

    { Draws vertical line with specified coordinates and color gradient. This method uses filled shapes instead of
      actual lines to produce accurate results and is meant for rendering UI elements. }
    procedure VertLine(const Left, Top, Height: VectorFloat; const Colors: TColorPair;
      const BlendingEffect: TBlendingEffect = TBlendingEffect.Normal); overload;

    { Draws vertical line with specified coordinates and single color. This method uses filled shapes instead of
      actual lines to produce accurate results and is meant for rendering UI elements. }
    procedure VertLine(const Left, Top, Height: VectorFloat; const Color: TIntColor;
      const BlendingEffect: TBlendingEffect = TBlendingEffect.Normal); overload;

    { Draws hexagon where vertices are spaced 0.5 pixels apart from its center (so diameter is 1) in all directions,
      multiplied by the given matrix and filled with gradient of six colors at the corresponding vertices. The size,
      position and rotation of hexagon can be given using one or a combination of several 3x3 matrices multiplied
      together. }
    procedure FillHexagon(const Matrix: TMatrix3f; const Color1, Color2, Color3, Color4, Color5, Color6: TIntColor;
      const BlendingEffect: TBlendingEffect = TBlendingEffect.Normal);

    { Draws filled arc at the given position and radius. The arc begins at @code(InitAngle) and ends at
      @code(EndAngle) (in radians), subdivided into a number of triangles specified in @code(Steps). The arc's shape is
      filled with four color gradient. }
    procedure FillArc(const Origin, Radius: TPoint2f; const InitAngle, EndAngle: VectorFloat; const Steps: Integer;
      const Colors: TColorRect; const BlendingEffect: TBlendingEffect = TBlendingEffect.Normal); overload;

    { Draws filled arc at the given coordinates and radius. The arc begins at @code(InitAngle) and ends at
      @code(EndAngle) (in radians), subdivided into a number of triangles specified in @code(Steps). The arc's shape is
      filled with four color gradient. }
    procedure FillArc(const X, Y, Radius, InitAngle, EndAngle: VectorFloat; const Steps: Integer;
      const Colors: TColorRect; const BlendingEffect: TBlendingEffect); overload;

    { Draws filled ellipse at the given position and radius. The ellipse is subdivided into a number of triangles
      specified in @code(Steps). The shape of ellipse is filled with four color gradient. }
    procedure FillEllipse(const Origin, Radius: TPoint2f; Steps: Integer; const Colors: TColorRect;
      const BlendingEffect: TBlendingEffect = TBlendingEffect.Normal);

    { Draws filled circle at the given position and radius. The circle is subdivided into a number of triangles
      specified in @code(Steps). The shape of circle is filled with four color gradient. }
    procedure FillCircle(X, Y, Radius: VectorFloat; Steps: Integer; const Colors: TColorRect;
      const BlendingEffect: TBlendingEffect = TBlendingEffect.Normal);

    { Draws filled ribbon at the given position between inner and outer radiuses. The ribbon begins at
      @code(InitAngle) and ends at @code(EndAngle) (in radians), subdivided into a number of triangles specified in
      @code(Steps). The ribbon's shape is filled with four color gradient. }
    procedure FillRibbon(const Origin, InsideRadius, OutsideRadius: TPoint2f; const InitAngle, EndAngle: VectorFloat;
      const Steps: Integer; const Colors: TColorRect;
      const BlendingEffect: TBlendingEffect = TBlendingEffect.Normal); overload;

    { Draws filled ribbon at the given position between inner and outer radiuses. The ribbon begins at
      @code(InitAngle) and ends at @code(EndAngle) (in radians), subdivided into a number of triangles specified in
      @code(Steps). The ribbon's shape is filled with continuous gradient set by three pairs of inner and outer
      colors. }
    procedure FillRibbon(const Origin, InsideRadius, OutsideRadius: TPoint2f; const InitAngle, EndAngle: VectorFloat;
      const Steps: Integer; const InsideColor1, InsideColor2, InsideColor3, OutsideColor1, OutsideColor2,
      OutsideColor3: TIntColor; const BlendingEffect: TBlendingEffect = TBlendingEffect.Normal); overload;

    { Draws a filled rectangle at the given position and size with a hole (in form of ellipse) inside at the given
      center and radius. The quality of the hole is defined by the value of @code(Steps) in number of subdivisions.
      This entire shape is filled with gradient starting from outer color at the edges of rectangle and inner color
      ending at the edge of hole. This shape can be particularly useful for highlighting items on the screen by
      darkening the entire area except the one inside the hole. }
    procedure QuadHole(const AreaTopLeft, AreaSize, HoleOrigin, HoleRadius: TPoint2f; const OutsideColor,
      InsideColor: TIntColor; const Steps: Integer; const BlendingEffect: TBlendingEffect = TBlendingEffect.Normal);

    { Draws one or more triangles filled with texture and color gradient, specified by vertex, texture coordinates,
      color and index buffers.This method is considered basic functionality and should always be implemented by derived
      classes. }
    procedure DrawTexturedTriangles(const Texture: TCustomBaseTexture; const Vertices, TexCoords: PPoint2f;
      const Colors: PIntColor; const Indices: PLongInt; const VertexCount, TriangleCount: Integer;
      const BlendingEffect: TBlendingEffect = TBlendingEffect.Normal); virtual; abstract;

    { Defines the specified texture to be used in next call to @link(TexQuad). The coordinates inside the texture are
      defined in logical units in range of [0..1]. }
    procedure UseTexture(const Texture: TCustomBaseTexture; const Mapping: TQuad);

    { Defines the specified texture to be used in next call to @link(TexQuad). The coordinates inside the texture are
      defined in pixels using floating-point coordinates. }
    procedure UseTexturePx(const Texture: TCustomBaseTexture; const Mapping: TQuad);

    { Defines the specified image to be used in next call to @link(TexQuad). If the image has multiple textures, then
      the first one will be used. }
    procedure UseImage(const Image: TCustomCanvasImage); overload;

    { Defines the specified image with one of its textures to be used in next call to @link(TexQuad). The coordinates
      inside the texture are defined in logical units in range of [0..1]. }
    procedure UseImage(const Image: TCustomCanvasImage; const Mapping: TQuad;
      const TextureIndex: Integer = 0); overload;

    { Defines the specified image with one of its textures to be used in next call to @link(TexQuad). The coordinates
      inside the texture are defined in pixels using floating-point coordinates. }
    procedure UseImagePx(const Image: TCustomCanvasImage; const Mapping: TQuad; const TextureIndex: Integer = 0);

    { Defines the specified region of the image to be used in next call to @link(TexQuad). If the image has none or
      just one region defined, the value of @code(Region) should be set to zero; in this case, the entire texture is
      used instead. }
    procedure UseImageRegion(const Image: TCustomCanvasImage; const Region: Integer = 0); overload;

    { Defines the specified region of the image to be used in next call to @link(TexQuad). Only a certain portion of
      that region is used for rendering defined by the given coordinates; these coordinates can also be mirrored
      horizontally and/or flipped vertically, if needed. If the image has no or just one pattern, the value of
      @code(Region) should be set to zero; in this case, the entire texture is used instead. }
    procedure UseImageRegion(const Image: TCustomCanvasImage; const Region: Integer; const SrcRect: TIntRect;
      const Mirror: Boolean = False; const Flip: Boolean = False); overload;

    { Defines the specified region of the image to be used in next call to @link(TexQuad). That region can also be
      mirrored horizontally and/or flipped vertically, if needed. If the image has no or just one pattern, the value of
      @code(Region) should be set to zero; in this case, the entire texture is used instead. }
    procedure UseImageRegion(const Image: TCustomCanvasImage; const Region: Integer;
      const Mirror, Flip: Boolean); overload;

    { Draws textured rectangle at given vertices and multiplied by the specified four color gradient. The texture must
      be set prior to this call by one of @code(UseTexture[...]) or @code(UseImage[...]) calls. For every call of
      @code(TexQuad) there must be a preceding @code(UseTexture[...]) or @code(UseImage[...]) call to specify the image
      or texture. All pixels of the rendered texture are multiplied by the gradient color before applying
      alpha-blending. If the texture has no alpha-channel present, alpha value of the gradient will be used instead. }
    procedure TexQuad(const Points: TQuad; const Colors: TColorRect;
      const BlendingEffect: TBlendingEffect = TBlendingEffect.Normal);

    { Flushes the canvas cache and presents the pending primitives on the destination surface. This can be useful to
      make sure that nothing remains in canvas cache before starting to render, for instance, a 3D scene. }
    procedure Flush; virtual; abstract;

    { Resets all the states necessary for canvas operation. This can be useful when custom state changes have been
      made (for instance, in a 3D scene) so to restore the canvas to its working condition this method should be
      called. }
    procedure Reset; virtual;

    { Sets the palette to be used for rendering 8-bit indexed images. Support for such images varies depending on
      provider and platform. This returns @True when successful and @False otherwise. }
    function SetPalette(const Palette: TIntColorPalette): Boolean; virtual;

    { Resets the palette to be used for rendering 8-bit indexed images that was previously set by @link(SetPalette). }
    procedure ResetPalette; virtual;

    { Sets custom shader effect to be used for rendering. This functionality may be provider and platform dependent.
      Also, for this to work, @italic(TCanvasAttribute.CustomEffect) should be set in @link(Attributes). }
    function SetEffect(const AEffect: TCustomCanvasEffect): Boolean; virtual;

    { Provides access to canvas parent if there is such. }
    property Parent: TCustomCanvasParent read FParent;

    { The device to which this canvas is bound to. }
    property Device: TCustomDevice read FDevice;

    { Indicates whether the canvas has been initialized by using @link(Initialize) function. }
    property Initialized: Boolean read FInitialized;

    { Number of times that rendering cache was reset during last rendering frame. Each cache reset is typically a
      time-consuming operation so high number of such events could be detrimental to the application's rendering
      performance. If this parameter happens to be considerably high in the rendered scene, the rendering code should
      be revised for better grouping of images, shapes and blending types. }
    property CacheStall: Integer read FCacheStall;

    { The clipping rectangle in which the rendering will be made. This can be useful for restricting the rendering to a
      certain portion of surface. }
    property ClipRect: TIntRect read GetClipRect write SetClipRect;

    { Defines one or more canvas attributes that affect the rendering behavior. }
    property Attributes: TCanvasAttributes read FAttributes write SetAttributes;
  end;

implementation

uses
  Math, PXL.Consts, PXL.Logs;

const
 ShaderCode = 'ps_1_1'#13#10 +
  'def c0, 0, 1, 0, 0'#13#10 +
  'def c1, 1, 0, 0, 0'#13#10 +
  'def c2, 0.333333343, 0.333333343, 0.333333343, 0'#13#10 +
  'tex t0'#13#10 +
  'dp3 r0, c0, t0'#13#10 +
  'dp3 r1, c1, t0'#13#10 +
  'add r0.w, r0.w, r1.w'#13#10 +
  'add r0.w, t0.z, r0.w'#13#10 +
  'mov r1.w, t0.w'#13#10 +
  '+ mul r1.xyz, r0.w, c2'#13#10 +
  'mov r0, r1';

{$REGION 'Global Functions'}

procedure SwapFloat(var Value1, Value2: VectorFloat);
var
  Temp: VectorFloat;
begin
  Temp := Value1;
  Value1 := Value2;
  Value2 := Temp;
end;

{$ENDREGION}
{$REGION 'TCustomCanvasParent'}

procedure TCustomCanvasParent.SetCanvasParent(const Canvas: TCustomCanvas; const Parent: TCustomCanvasParent);
begin
  Canvas.FParent := Parent;
end;

function TCustomCanvasParent.CanvasBeginScene: Boolean;
begin
  Result := True;
end;

procedure TCustomCanvasParent.CanvasEndScene;
begin
end;

function TCustomCanvasParent.CanvasInternalBeginScene(const Canvas: TCustomCanvas): Boolean;
begin
  Result := Canvas.InternalBeginScene;
end;

procedure TCustomCanvasParent.CanvasInternalEndScene(const Canvas: TCustomCanvas);
begin
  Canvas.InternalEndScene;
end;

{$ENDREGION}
{$REGION 'TCustomCanvas'}

constructor TCustomCanvas.Create(const ADevice: TCustomDevice);
begin
  inherited Create;

  FDevice := ADevice;

  try
    FClipRectQueue := TIntRectList.Create;

    ComputeHexagonVertices;

    if FDevice <> nil then
    begin
      if FDevice.OnRestore <> nil then
        FDeviceRestoreHandle := FDevice.OnRestore.Subscribe(OnDeviceRestore);

      if FDevice.OnRelease <> nil then
        FDeviceReleaseHandle := FDevice.OnRelease.Subscribe(OnDeviceRelease);
      LoadDirect3D9_33DLL();
      D3DXAssembleShader(ShaderCode, Length(ShaderCode), nil, nil,  0, @FPixelShaderBuffer, nil);
    end;
  finally
    Increment_PXL_ClassInstances;
  end;

  if not NeedsInitialization then
    FInitialized := True;
end;

destructor TCustomCanvas.Destroy;
begin
  try
    if FDevice <> nil then
    begin
      if FDevice.OnRelease <> nil then
        FDevice.OnRelease.Unsubscribe(FDeviceReleaseHandle);

      if FDevice.OnRestore <> nil then
        FDevice.OnRestore.Unsubscribe(FDeviceRestoreHandle);
    end
    else
    begin
      FDeviceReleaseHandle := 0;
      FDeviceRestoreHandle := 0;
    end;

    if NeedsInitialization then
      Finalize;

    FClipRectQueue.Free;
    FDevice := nil;
  finally
    Decrement_PXL_ClassInstances;
  end;

  inherited;
end;

procedure TCustomCanvas.ComputeHexagonVertices;
const
  HexDelta = 1.154700538;
  AngleInc = Pi / 6;
  AngleMul = 2 * Pi / 6;
var
  I: Integer;
  Angle, SinAngle, CosAngle: VectorFloat;
begin
  for I := 0 to 5 do
  begin
    Angle := I * AngleMul + AngleInc;

    SinCos(Angle, SinAngle, CosAngle);
    FHexagonVertices[I].X := CosAngle * HexDelta;
    FHexagonVertices[I].Y := -SinAngle * HexDelta;
  end;
end;

function TCustomCanvas.NeedsInitialization: Boolean;
begin
  Result := True;
end;

function TCustomCanvas.InitCanvas: Boolean;
begin
  Result := True;
end;

procedure TCustomCanvas.DoneCanvas;
begin
end;

function TCustomCanvas.BeginDraw: Boolean;
begin
  Result := True;
end;

procedure TCustomCanvas.EndDraw;
begin
end;

function TCustomCanvas.DeviceRestore: Boolean;
begin
  Result := True;
end;

procedure TCustomCanvas.DeviceRelease;
begin
end;

procedure TCustomCanvas.SetAttributes(const Value: TCanvasAttributes);
begin
  if FAttributes <> Value then
  begin
    FAttributes := Value;
    UpdateAttributes;
  end;
end;

procedure TCustomCanvas.UpdateAttributes;
begin
end;

procedure TCustomCanvas.NextDrawCall;
begin
  Inc(FCacheStall);
end;

procedure TCustomCanvas.PutPixel(const X, Y: VectorFloat; const Color: TIntColor);
begin
  PutPixel(Point2f(X, Y), Color);
end;

procedure TCustomCanvas.Line(const SrcPoint, DestPoint: TPoint2f; const Color: TIntColor);
begin
  Line(SrcPoint, DestPoint, ColorPair(Color));
end;

procedure TCustomCanvas.Line(const X1, Y1, X2, Y2: VectorFloat; const Color: TColorPair);
begin
  Line(Point2f(X1, Y1), Point2f(X2, Y2), Color);
end;

procedure TCustomCanvas.Line(const X1, Y1, X2, Y2: VectorFloat; const Color: TIntColor);
begin
  Line(Point2f(X1, Y1), Point2f(X2, Y2), ColorPair(Color));
end;

procedure TCustomCanvas.LineArray(const Points: PPoint2f; const ElementCount: Integer; const Color: TIntColor);
var
  I: Integer;
  CurrentPoint, NextPoint: PPoint2f;
begin
  CurrentPoint := Points;

  for I := 0 to ElementCount - 2 do
  begin
    NextPoint := CurrentPoint;
    Inc(NextPoint);

    Line(CurrentPoint^, NextPoint^, ColorPair(Color));

    CurrentPoint := NextPoint;
  end;
end;

procedure TCustomCanvas.WuLineHorizontal(X1, Y1, X2, Y2: VectorFloat; const Color: TColorPair);
var
  TempColor: TIntColor;
  DeltaX, DeltaY, Gradient, FinalY: VectorFloat;
  EndX, X, IntX1, IntX2, IntY1, IntY2: Integer;
  EndY, GapX, Alpha1, Alpha2, Alpha, AlphaInc: VectorFloat;
begin
  DeltaX := X2 - X1;
  DeltaY := Y2 - Y1;

  if X1 > X2 then
  begin
    SwapFloat(X1, X2);
    SwapFloat(Y1, Y2);

    DeltaX := X2 - X1;
    DeltaY := Y2 - Y1;
  end;

  Gradient := DeltaY / DeltaX;

  // End Point 1
  EndX := Trunc(X1 + 0.5);
  EndY := Y1 + Gradient * (EndX - X1);

  GapX := 1 - Frac(X1 + 0.5);

  IntX1 := EndX;
  IntY1 := Trunc(EndY);

  Alpha1 := (1 - Frac(EndY)) * GapX;
  Alpha2 := Frac(EndY) * GapX;

  PutPixel(Point2f(IntX1, IntY1), IntColor(Color.First, Alpha1));
  PutPixel(Point2f(IntX1, IntY1 + 1), IntColor(Color.First, Alpha2));

  FinalY := EndY + Gradient;

  // End Point 2
  EndX := Trunc(X2 + 0.5);
  EndY := Y2 + Gradient * (EndX - X2);

  GapX := 1 - Frac(X2 + 0.5);

  IntX2 := EndX;
  IntY2 := Trunc(EndY);

  Alpha1 := (1 - Frac(EndY)) * GapX;
  Alpha2 := Frac(EndY) * GapX;

  PutPixel(Point2f(IntX2, IntY2), IntColor(Color.Second, Alpha1));
  PutPixel(Point2f(IntX2, IntY2 + 1), IntColor(Color.Second, Alpha2));

  Alpha := 0;
  AlphaInc := 1 / DeltaX;

  // Main Loop
  for X := IntX1 + 1 to IntX2 - 1 do
  begin
    Alpha1 := 1 - Frac(FinalY);
    Alpha2 := Frac(FinalY);

    TempColor := LerpPixels(Color.First, Color.Second, Alpha);

    PutPixel(Point2f(X, Int(FinalY)), IntColor(TempColor, Alpha1));
    PutPixel(Point2f(X, Int(FinalY) + 1), IntColor(TempColor, Alpha2));

    FinalY := FinalY + Gradient;
    Alpha := Alpha + AlphaInc;
  end;
end;

procedure TCustomCanvas.WuLineVertical(X1, Y1, X2, Y2: VectorFloat; const Color: TColorPair);
var
  TempColor: TIntColor;
  DeltaX, DeltaY, Gradient, FinalX: VectorFloat;
  EndY, Y, IntX1, IntX2, IntY1, IntY2: Integer;
  EndX, yGap, Alpha1, Alpha2, Alpha, AlphaInc: VectorFloat;
begin
  DeltaX := X2 - X1;
  DeltaY := Y2 - Y1;

  if Y1 > Y2 then
  begin
    SwapFloat(X1, X2);
    SwapFloat(Y1, Y2);

    DeltaX := X2 - X1;
    DeltaY := Y2 - Y1;
  end;

  Gradient := DeltaX / DeltaY;

  // End Point 1
  EndY := Trunc(Y1 + 0.5);
  EndX := X1 + Gradient * (EndY - Y1);

  yGap := 1 - Frac(Y1 + 0.5);

  IntX1 := Trunc(EndX);
  IntY1 := EndY;

  Alpha1 := (1 - Frac(EndX)) * yGap;
  Alpha2 := Frac(EndX) * yGap;

  PutPixel(Point2f(IntX1, IntY1), IntColor(Color.First, Alpha1));
  PutPixel(Point2f(IntX1 + 1, IntY1), IntColor(Color.First, Alpha2));

  FinalX := EndX + Gradient;

  // End Point 2
  EndY := Trunc(Y2 + 0.5);
  EndX := X2 + Gradient * (EndY - Y2);

  yGap := 1 - Frac(Y2 + 0.5);

  IntX2 := Trunc(EndX);
  IntY2 := EndY;

  Alpha1 := (1 - Frac(EndX)) * yGap;
  Alpha2 := Frac(EndX) * yGap;

  PutPixel(Point2f(IntX2, IntY2), IntColor(Color.Second, Alpha1));
  PutPixel(Point2f(IntX2 + 1, IntY2), IntColor(Color.Second, Alpha2));

  Alpha := 0;
  AlphaInc := 1 / DeltaY;

  // Main Loop
  for Y := IntY1 + 1 to IntY2 - 1 do
  begin
    Alpha1 := 1 - Frac(FinalX);
    Alpha2 := Frac(FinalX);

    TempColor := LerpPixels(Color.First, Color.Second, Alpha);

    PutPixel(Point2f(Int(FinalX), Y), IntColor(TempColor, Alpha1));
    PutPixel(Point2f(Int(FinalX) + 1, Y), IntColor(TempColor, Alpha2));

    FinalX := FinalX + Gradient;
    Alpha := Alpha + AlphaInc;
  end;
end;

procedure TCustomCanvas.WuLine(const Point1, Point2f: TPoint2f; const Color: TColorPair);
begin
  if (Abs(Point2f.X - Point1.X) > Abs(Point2f.Y - Point1.Y)) then
    WuLineHorizontal(Point1.X, Point1.Y, Point2f.X, Point2f.Y, Color)
  else
    WuLineVertical(Point1.X, Point1.Y, Point2f.X, Point2f.Y, Color)
end;

procedure TCustomCanvas.Ellipse(const Origin, Radius: TPoint2f; const Steps: Integer; const Color: TIntColor;
  const UseWuLines: Boolean);
var
  I: Integer;
  Vertex, PreVertex: TPoint2f;
  Alpha, SinAlpha, CosAlpha: VectorFloat;
begin
  Vertex := ZeroPoint2f;

  for I := 0 to Steps do
  begin
    Alpha := I * (Pi * 2) / Steps;

    PreVertex := Vertex;

    SinCos(Alpha, SinAlpha, CosAlpha);
    Vertex.X := Int(Origin.X + CosAlpha * Radius.X);
    Vertex.Y := Int(Origin.Y - SinAlpha * Radius.Y);

    if I > 0 then
    begin
      if UseWuLines then
        WuLine(PreVertex, Vertex, ColorPair(Color))
      else
        Line(PreVertex, Vertex, ColorPair(Color));
    end;
  end;
end;

procedure TCustomCanvas.Circle(const Origin: TPoint2f; const Radius: VectorFloat; const Steps: Integer;
  const Color: TIntColor; const UseWuLines: Boolean);
begin
  Ellipse(Origin, Point2f(Radius, Radius), Steps, Color, UseWuLines);
end;

procedure TCustomCanvas.WireQuad(const Points: TQuad; const Colors: TColorRect; const UseWuLines: Boolean);
begin
  if UseWuLines then
  begin
    WuLine(Points.TopLeft, Points.TopRight, ColorPair(Colors.TopLeft, Colors.TopRight));
    WuLine(Points.TopRight, Points.BottomRight, ColorPair(Colors.TopRight, Colors.BottomRight));
    WuLine(Points.BottomRight, Points.BottomLeft, ColorPair(Colors.BottomRight, Colors.BottomLeft));
    WuLine(Points.BottomLeft, Points.TopLeft, ColorPair(Colors.BottomLeft, Colors.TopLeft));
  end
  else
  begin
    Line(Points.TopLeft, Points.TopRight, ColorPair(Colors.TopLeft, Colors.TopRight));
    Line(Points.TopRight, Points.BottomRight, ColorPair(Colors.TopRight, Colors.BottomRight));
    Line(Points.BottomRight, Points.BottomLeft, ColorPair(Colors.BottomRight, Colors.BottomLeft));
    Line(Points.BottomLeft, Points.TopLeft, ColorPair(Colors.BottomLeft, Colors.TopLeft));
  end;
end;

procedure TCustomCanvas.WireHexagon(const Matrix: TMatrix3f; const Color1, Color2, Color3, Color4, Color5,
  Color6: TIntColor; const UseWuLines: Boolean);
var
  I: Integer;
  NextPos, PrevPos: TPoint2f;
  VertexColors: array[0..5] of TIntColor;
  PrevColor, NextColor: TIntColor;
begin
  NextPos := ZeroPoint2f;
  NextColor := IntColorTranslucentBlack;

  VertexColors[0] := Color1;
  VertexColors[1] := Color2;
  VertexColors[2] := Color3;
  VertexColors[3] := Color4;
  VertexColors[4] := Color5;
  VertexColors[5] := Color6;

  for I := 0 to 6 do
  begin
    PrevPos := NextPos;
    NextPos := FHexagonVertices[I mod 6] * Matrix;

    PrevColor := NextColor;
    NextColor := VertexColors[I mod 6];

    if I > 0 then
    begin
      if UseWuLines then
        WuLine(PrevPos, NextPos, ColorPair(PrevColor, NextColor))
      else
        Line(PrevPos, NextPos, ColorPair(PrevColor, NextColor));
    end;
  end;
end;

procedure TCustomCanvas.FillTri(const Point1, Point2f, Point3: TPoint2f; const Color1, Color2, Color3: TIntColor;
 const BlendingEffect: TBlendingEffect);
const
  Indices: packed array[0..2] of LongInt = (0, 1, 2);
var
  Vertices: packed array[0..2] of TPoint2f;
  Colors: packed array[0..2] of TIntColor;
begin
  Vertices[0] := Point1;
  Vertices[1] := Point2f;
  Vertices[2] := Point3;

  Colors[0] := Color1;
  Colors[1] := Color2;
  Colors[2] := Color3;

  DrawIndexedTriangles(@Vertices[0], @Colors[0], @Indices[0], 3, 1, BlendingEffect);
end;

procedure TCustomCanvas.FillQuad(const Points: TQuad; const Colors: TColorRect;
  const BlendingEffect: TBlendingEffect);
const
  Indices: packed array[0..5] of LongInt = (0, 1, 2, 2, 3, 0);
var
  Vertices: packed array[0..3] of TPoint2f;
  VertexColors: packed array[0..3] of TIntColor;
begin
  Vertices[0] := Points.TopLeft;
  Vertices[1] := Points.TopRight;
  Vertices[2] := Points.BottomRight;
  Vertices[3] := Points.BottomLeft;

  VertexColors[0] := Colors.TopLeft;
  VertexColors[1] := Colors.TopRight;
  VertexColors[2] := Colors.BottomRight;
  VertexColors[3] := Colors.BottomLeft;

  DrawIndexedTriangles(@Vertices[0], @VertexColors[0], @Indices[0], 4, 2, BlendingEffect);
end;

procedure TCustomCanvas.FillRect(const Rect: TFloatRect; const Colors: TColorRect;
  const BlendingEffect: TBlendingEffect);
begin
  FillQuad(Quad(Rect), Colors, BlendingEffect);
end;

procedure TCustomCanvas.FillRect(const Rect: TFloatRect; const Color: TIntColor; const BlendingEffect: TBlendingEffect);
begin
  FillRect(Rect, ColorRect(Color), BlendingEffect);
end;

procedure TCustomCanvas.FillRect(const Left, Top, Width, Height: VectorFloat; const Color: TIntColor;
  const BlendingEffect: TBlendingEffect = TBlendingEffect.Normal);
begin
  FillRect(FloatRect(Left, Top, Width, Height), Color, BlendingEffect);
end;

procedure TCustomCanvas.FrameRect(const Points: TQuad; const Colors: TColorRect;
  const BlendingEffect: TBlendingEffect);
const
  Indices: array [0..23] of LongInt = (0, 1, 4, 4, 1, 5, 1, 2, 5, 5, 2, 6, 2, 3, 6, 6, 3, 7, 3, 0, 7, 7, 0, 4);
var
  Vertices: array[0..7] of TPoint2f;
  VertexColors: array[0..7] of TIntColor;
begin
  Vertices[0] := Points.TopLeft;
  Vertices[1] := Points.TopRight;
  Vertices[2] := Points.BottomRight;
  Vertices[3] := Points.BottomLeft;
  Vertices[4] := Point2f(Points.Values[0].X + 1.0, Points.Values[0].Y + 1.0);
  Vertices[5] := Point2f(Points.Values[1].X - 1.0, Points.Values[1].Y + 1.0);
  Vertices[6] := Point2f(Points.Values[2].X - 1.0, Points.Values[2].Y - 1.0);
  Vertices[7] := Point2f(Points.Values[3].X + 1.0, Points.Values[3].Y - 1.0);

  VertexColors[0] := Colors.TopLeft;
  VertexColors[1] := Colors.TopRight;
  VertexColors[2] := Colors.BottomRight;
  VertexColors[3] := Colors.BottomLeft;
  VertexColors[4] := Colors.TopLeft;
  VertexColors[5] := Colors.TopRight;
  VertexColors[6] := Colors.BottomRight;
  VertexColors[7] := Colors.BottomLeft;

  DrawIndexedTriangles(@Vertices[0], @VertexColors[0], @Indices[0], 8, 8);
end;

procedure TCustomCanvas.FrameRect(const Rect: TFloatRect; const Colors: TColorRect;
  const BlendingEffect: TBlendingEffect);
begin
  FrameRect(Quad(Rect), Colors, BlendingEffect);
end;

procedure TCustomCanvas.HorizLine(const Left, Top, Width: VectorFloat; const Colors: TColorPair;
  const BlendingEffect: TBlendingEffect);
begin
  FillQuad(Quad(Left, Top, Width, 1.0), ColorRect(Colors.First, Colors.Second, Colors.Second,
    Colors.First), BlendingEffect);
end;

procedure TCustomCanvas.HorizLine(const Left, Top, Width: VectorFloat; const Color: TIntColor;
  const BlendingEffect: TBlendingEffect);
begin
  HorizLine(Left, Top, Width, ColorPair(Color), BlendingEffect);
end;

procedure TCustomCanvas.VertLine(const Left, Top, Height: VectorFloat; const Colors: TColorPair;
  const BlendingEffect: TBlendingEffect);
begin
  FillQuad(Quad(Left, Top, 1, Height), ColorRect(Colors.First, Colors.First, Colors.Second,
    Colors.Second), BlendingEffect);
end;

procedure TCustomCanvas.VertLine(const Left, Top, Height: VectorFloat; const Color: TIntColor;
  const BlendingEffect: TBlendingEffect);
begin
  VertLine(Left, Top, Height, ColorPair(Color), BlendingEffect);
end;

procedure TCustomCanvas.FillHexagon(const Matrix: TMatrix3f; const Color1, Color2, Color3, Color4, Color5,
  Color6: TIntColor; const BlendingEffect: TBlendingEffect);
const
  Indices: packed array[0..17] of LongInt = (0, 1, 2, 0, 2, 3, 0, 3, 4, 0, 4, 5, 0, 5, 6, 0, 6, 1);
var
  Vertices: packed array[0..6] of TPoint2f;
  VertexColors: packed array[0..6] of TIntColor;
begin
  Vertices[0] := ZeroPoint2f * Matrix;
  VertexColors[0] := AverageSixPixels(Color1, Color2, Color3, Color4, Color5, Color6);

  Vertices[1] := FHexagonVertices[0] * Matrix;
  VertexColors[1] := Color1;

  Vertices[2] := FHexagonVertices[1] * Matrix;
  VertexColors[2] := Color2;

  Vertices[3] := FHexagonVertices[2] * Matrix;
  VertexColors[3] := Color3;

  Vertices[4] := FHexagonVertices[3] * Matrix;
  VertexColors[4] := Color4;

  Vertices[5] := FHexagonVertices[4] * Matrix;
  VertexColors[5] := Color5;

  Vertices[6] := FHexagonVertices[5] * Matrix;
  VertexColors[6] := Color6;

  DrawIndexedTriangles(@Vertices[0], @VertexColors[0], @Indices[0], 7, 6, BlendingEffect);
end;

procedure TCustomCanvas.FillArc(const Origin, Radius: TPoint2f; const InitAngle, EndAngle: VectorFloat;
  const Steps: Integer; const Colors: TColorRect; const BlendingEffect: TBlendingEffect);
var
  Vertices: packed array of TPoint2f;
  VertexColors: packed array of TIntColor;
  Indices: packed array of LongInt;
  MarginTopLeft, MarginBottomRight: TPoint2f;
  I, CurVertexCount, AlphaX, AlphaY: Integer;
  Alpha, SinAlpha, CosAlpha: VectorFloat;
begin
  if Steps < 1 then
    Exit;

  MarginTopLeft := Origin - Radius;
  MarginBottomRight := Origin + Radius;

  SetLength(Vertices, Steps + 2);
  SetLength(VertexColors, Length(Vertices));
  SetLength(Indices, Steps * 3);

  CurVertexCount := 0;

  Vertices[CurVertexCount] := Origin;
  VertexColors[CurVertexCount] := AverageFourPixels(Colors.TopLeft, Colors.TopRight, Colors.BottomRight,
    Colors.BottomLeft);
  Inc(CurVertexCount);

  for I := 0 to Steps - 1 do
  begin
    Alpha := (I * (EndAngle - InitAngle) / Steps) + InitAngle;

    SinCos(Alpha, SinAlpha, CosAlpha);
    Vertices[CurVertexCount].X := Origin.X + CosAlpha * Radius.X;
    Vertices[CurVertexCount].Y := Origin.Y - SinAlpha * Radius.Y;

    AlphaX := Round((Vertices[CurVertexCount].X - MarginTopLeft.X) * 255.0 / (MarginBottomRight.X - MarginTopLeft.X));
    AlphaY := Round((Vertices[CurVertexCount].Y - MarginTopLeft.Y) * 255.0 / (MarginBottomRight.Y - MarginTopLeft.Y));

    VertexColors[CurVertexCount] := BlendFourPixels(Colors.TopLeft, Colors.TopRight, Colors.BottomRight,
      Colors.BottomLeft, AlphaX, AlphaY);

    Indices[(I * 3) + 0] := 0;
    Indices[(I * 3) + 1] := CurVertexCount;
    Indices[(I * 3) + 2] := CurVertexCount + 1;

    Inc(CurVertexCount);
  end;

  SinCos(EndAngle, SinAlpha, CosAlpha);
  Vertices[CurVertexCount].X := Origin.X + CosAlpha * Radius.X;
  Vertices[CurVertexCount].Y := Origin.Y - SinAlpha * Radius.Y;

  AlphaX := Round((Vertices[CurVertexCount].X - MarginTopLeft.X) * 255.0 / (MarginBottomRight.X - MarginTopLeft.X));
  AlphaY := Round((Vertices[CurVertexCount].Y - MarginTopLeft.Y) * 255.0 / (MarginBottomRight.Y - MarginTopLeft.Y));

  VertexColors[CurVertexCount] := BlendFourPixels(Colors.TopLeft, Colors.TopRight, Colors.BottomRight,
    Colors.BottomLeft, AlphaX, AlphaY);

  DrawIndexedTriangles(@Vertices[0], @VertexColors[0], @Indices[0], Length(Vertices), Steps, BlendingEffect);
end;

procedure TCustomCanvas.FillArc(const X, Y, Radius, InitAngle, EndAngle: VectorFloat; const Steps: Integer;
  const Colors: TColorRect; const BlendingEffect: TBlendingEffect);
begin
  FillArc(Point2f(X, Y), Point2f(Radius, Radius), InitAngle, EndAngle, Steps, Colors, BlendingEffect);
end;

procedure TCustomCanvas.FillEllipse(const Origin, Radius: TPoint2f; Steps: Integer; const Colors: TColorRect;
  const BlendingEffect: TBlendingEffect);
begin
  FillArc(Origin, Radius, 0, Pi * 2.0, Steps, Colors, BlendingEffect);
end;

procedure TCustomCanvas.FillCircle(X, Y, Radius: VectorFloat; Steps: Integer; const Colors: TColorRect;
  const BlendingEffect: TBlendingEffect);
begin
  FillArc(Point2f(X, Y), Point2f(Radius, Radius), 0, Pi * 2.0, Steps, Colors, BlendingEffect);
end;

procedure TCustomCanvas.FillRibbon(const Origin, InsideRadius, OutsideRadius: TPoint2f; const InitAngle,
  EndAngle: VectorFloat; const Steps: Integer; const Colors: TColorRect; const BlendingEffect: TBlendingEffect);
var
  Vertices: packed array of TPoint2f;
  VertexColors: packed array of TIntColor;
  Indices: packed array of LongInt;
  MarginTopLeft, MarginBottomRight: TPoint2f;
  I, CurVertexCount, CurIndexCount, AlphaX, AlphaY: Integer;
  Alpha, SinAlpha, CosAlpha: VectorFloat;
begin
  if Steps < 1 then
    Exit;

  MarginTopLeft := Origin - OutsideRadius;
  MarginBottomRight := Origin + OutsideRadius;

  SetLength(Vertices, (Steps * 2) + 2);
  SetLength(VertexColors, Length(Vertices));
  SetLength(Indices, Steps * 6);

  CurVertexCount := 0;

  SinCos(InitAngle, SinAlpha, CosAlpha);

  Vertices[CurVertexCount].X := Origin.X + (CosAlpha * InsideRadius.X);
  Vertices[CurVertexCount].Y := Origin.Y - (SinAlpha * InsideRadius.Y);

  AlphaX := Round((Vertices[CurVertexCount].X - MarginTopLeft.X) * 255.0 / (MarginBottomRight.X - MarginTopLeft.X));
  AlphaY := Round((Vertices[CurVertexCount].Y - MarginTopLeft.Y) * 255.0 / (MarginBottomRight.Y - MarginTopLeft.Y));

  VertexColors[CurVertexCount] := BlendFourPixels(Colors.TopLeft, Colors.TopRight, Colors.BottomRight,
    Colors.BottomLeft, AlphaX, AlphaY);

  Inc(CurVertexCount);

  Vertices[CurVertexCount].X := Origin.X + (CosAlpha * OutsideRadius.X);
  Vertices[CurVertexCount].Y := Origin.Y - (SinAlpha * OutsideRadius.Y);

  AlphaX := Round((Vertices[CurVertexCount].X - MarginTopLeft.X) * 255.0 / (MarginBottomRight.X - MarginTopLeft.X));
  AlphaY := Round((Vertices[CurVertexCount].Y - MarginTopLeft.Y) * 255.0 / (MarginBottomRight.Y - MarginTopLeft.Y));

  VertexColors[CurVertexCount] := BlendFourPixels(Colors.TopLeft, Colors.TopRight, Colors.BottomRight,
    Colors.BottomLeft, AlphaX, AlphaY);

  Inc(CurVertexCount);

  for I := 1 to Steps do
  begin
    Alpha := (I * (EndAngle - InitAngle) / Steps) + InitAngle;
    SinCos(Alpha, SinAlpha, CosAlpha);

    // Inner vertex
    Vertices[CurVertexCount].X := Origin.X + (CosAlpha * InsideRadius.X);
    Vertices[CurVertexCount].Y := Origin.Y - (SinAlpha * InsideRadius.Y);

    AlphaX := Round((Vertices[CurVertexCount].X - MarginTopLeft.X) * 255.0 / (MarginBottomRight.X - MarginTopLeft.X));
    AlphaY := Round((Vertices[CurVertexCount].Y - MarginTopLeft.Y) * 255.0 / (MarginBottomRight.Y - MarginTopLeft.Y));

    VertexColors[CurVertexCount] := BlendFourPixels(Colors.TopLeft, Colors.TopRight, Colors.BottomRight,
      Colors.BottomLeft, AlphaX, AlphaY);

    Inc(CurVertexCount);

    // Outer vertex
    Vertices[CurVertexCount].X := Origin.X + (CosAlpha * OutsideRadius.X);
    Vertices[CurVertexCount].Y := Origin.Y - (SinAlpha * OutsideRadius.Y);

    AlphaX := Round((Vertices[CurVertexCount].X - MarginTopLeft.X) * 255.0 / (MarginBottomRight.X - MarginTopLeft.X));
    AlphaY := Round((Vertices[CurVertexCount].Y - MarginTopLeft.Y) * 255.0 / (MarginBottomRight.Y - MarginTopLeft.Y));

    VertexColors[CurVertexCount] := BlendFourPixels(Colors.TopLeft, Colors.TopRight, Colors.BottomRight,
      Colors.BottomLeft, AlphaX, AlphaY);

    Inc(CurVertexCount);
  end;

  CurIndexCount := 0;
  for I := 0 to Steps - 1 do
  begin
    Indices[(I * 6) + 0] := CurIndexCount;
    Indices[(I * 6) + 1] := CurIndexCount + 1;
    Indices[(I * 6) + 2] := CurIndexCount + 2;

    Indices[(I * 6) + 3] := CurIndexCount + 1;
    Indices[(I * 6) + 4] := CurIndexCount + 3;
    Indices[(I * 6) + 5] := CurIndexCount + 2;

    Inc(CurIndexCount, 2);
  end;

  DrawIndexedTriangles(@Vertices[0], @VertexColors[0], @Indices[0], Length(Vertices), Steps * 2, BlendingEffect);
end;

procedure TCustomCanvas.FillRibbon(const Origin, InsideRadius, OutsideRadius: TPoint2f; const InitAngle,
  EndAngle: VectorFloat; const Steps: Integer; const InsideColor1, InsideColor2, InsideColor3, OutsideColor1,
  OutsideColor2, OutsideColor3: TIntColor; const BlendingEffect: TBlendingEffect);
var
  Vertices: packed array of TPoint2f;
  VertexColors: packed array of TIntColor;
  Indices: packed array of LongInt;
  InsideColor, OutsideColor: TIntColor;
  Alpha, Theta, SinAlpha, CosAlpha: VectorFloat;
  I, CurVertexCount, CurIndexCount: Integer;
begin
  if Steps < 1 then
    Exit;

  SetLength(Vertices, (Steps * 2) + 2);
  SetLength(VertexColors, Length(Vertices));
  SetLength(Indices, Steps * 6);

  CurVertexCount := 0;
  SinCos(InitAngle, SinAlpha, CosAlpha);

  Vertices[CurVertexCount].X := Origin.X + CosAlpha * InsideRadius.X;
  Vertices[CurVertexCount].Y := Origin.Y - SinAlpha * InsideRadius.Y;
  VertexColors[CurVertexCount] := InsideColor1;
  Inc(CurVertexCount);

  Vertices[CurVertexCount].X := Origin.X + CosAlpha * OutsideRadius.X;
  Vertices[CurVertexCount].Y := Origin.Y - SinAlpha * OutsideRadius.Y;
  VertexColors[CurVertexCount] := OutsideColor1;
  Inc(CurVertexCount);

  for I := 1 to Steps do
  begin
    Alpha := (I * (EndAngle - InitAngle) / Steps) + InitAngle;
    SinCos(Alpha, SinAlpha, CosAlpha);

    Theta := I / Steps;
    if Theta < 0.5 then
    begin
      Theta := 2.0 * Theta;

      InsideColor := LerpPixels(InsideColor1, InsideColor2, Theta);
      OutsideColor := LerpPixels(OutsideColor1, OutsideColor2, Theta);
    end
    else
    begin
      Theta := (Theta - 0.5) * 2.0;

      InsideColor := LerpPixels(InsideColor2, InsideColor3, Theta);
      OutsideColor := LerpPixels(OutsideColor2, OutsideColor3, Theta);
    end;

    // Inner vertex
    Vertices[CurVertexCount].X := Origin.X + CosAlpha * InsideRadius.X;
    Vertices[CurVertexCount].Y := Origin.Y - SinAlpha * InsideRadius.Y;
    VertexColors[CurVertexCount] := InsideColor;
    Inc(CurVertexCount);

    // Outer vertex
    Vertices[CurVertexCount].X := Origin.X + CosAlpha * OutsideRadius.X;
    Vertices[CurVertexCount].Y := Origin.Y - SinAlpha * OutsideRadius.Y;
    VertexColors[CurVertexCount] := OutsideColor;
    Inc(CurVertexCount);
  end;

  CurIndexCount := 0;
  for I := 0 to Steps - 1 do
  begin
    Indices[(I * 6) + 0] := CurIndexCount;
    Indices[(I * 6) + 1] := CurIndexCount + 1;
    Indices[(I * 6) + 2] := CurIndexCount + 2;

    Indices[(I * 6) + 3] := CurIndexCount + 1;
    Indices[(I * 6) + 4] := CurIndexCount + 3;
    Indices[(I * 6) + 5] := CurIndexCount + 2;

    Inc(CurIndexCount, 2);
  end;

  DrawIndexedTriangles(@Vertices[0], @VertexColors[0], @Indices[0], Length(Vertices), Steps * 2, BlendingEffect);
end;

procedure TCustomCanvas.QuadHole(const AreaTopLeft, AreaSize, HoleOrigin, HoleRadius: TPoint2f; const OutsideColor,
  InsideColor: TIntColor; const Steps: Integer; const BlendingEffect: TBlendingEffect);
var
  Vertices: packed array of TPoint2f;
  VertexColors: packed array of TIntColor;
  Indices: packed array of LongInt;
  Theta, Angle, SinAngle, CosAngle: VectorFloat;
  I, BaseIndex: Integer;
begin
  SetLength(Vertices, Steps * 2);
  SetLength(VertexColors, Steps * 2);
  SetLength(Indices, (Steps - 1) * 6);

  for I := 0 to Steps - 2 do
  begin
    BaseIndex := I * 6;

    Indices[BaseIndex + 0] := I;
    Indices[BaseIndex + 1] := I + 1;
    Indices[BaseIndex + 2] := Steps + I;

    Indices[BaseIndex + 3] := I + 1;
    Indices[BaseIndex + 4] := Steps + I + 1;
    Indices[BaseIndex + 5] := Steps + I;
  end;

  for I := 0 to Steps - 1 do
  begin
    Theta := I / (Steps - 1);

    Vertices[I].X := AreaTopLeft.X + Theta * AreaSize.X;
    Vertices[I].Y := AreaTopLeft.Y;
    VertexColors[I] := OutsideColor;

    Angle := Pi * 0.25 + Pi * 0.5 - Theta * Pi * 0.5;
    SinCos(Angle, SinAngle, CosAngle);

    Vertices[Steps + I].X := HoleOrigin.X + CosAngle * HoleRadius.X;
    Vertices[Steps + I].Y := HoleOrigin.Y - SinAngle * HoleRadius.Y;
    VertexColors[Steps + I] := InsideColor;
  end;

  DrawIndexedTriangles(@Vertices[0], @VertexColors[0], @Indices[0], Length(Vertices), Length(Indices) div 3,
    BlendingEffect);

  for I := 0 to Steps - 1 do
  begin
    Theta := I / (Steps - 1);

    Vertices[I].X := AreaTopLeft.X + AreaSize.X;
    Vertices[I].Y := AreaTopLeft.Y + Theta * AreaSize.Y;
    VertexColors[I] := OutsideColor;

    Angle := Pi * 0.25 - Theta * Pi * 0.5;
    SinCos(Angle, SinAngle, CosAngle);

    Vertices[Steps + I].X := HoleOrigin.X + CosAngle * HoleRadius.X;
    Vertices[Steps + I].Y := HoleOrigin.Y - SinAngle * HoleRadius.Y;
    VertexColors[Steps + I] := InsideColor;
  end;

  DrawIndexedTriangles(@Vertices[0], @VertexColors[0], @Indices[0], Length(Vertices), Length(Indices) div 3,
    BlendingEffect);

  for I := 0 to Steps - 1 do
  begin
    Theta := I / (Steps - 1);

    Vertices[I].X := AreaTopLeft.X;
    Vertices[I].Y := AreaTopLeft.Y + Theta * AreaSize.Y;
    VertexColors[I] := OutsideColor;

    Angle := Pi * 0.75 + Theta * Pi * 0.5;
    SinCos(Angle, SinAngle, CosAngle);

    Vertices[Steps + I].X := HoleOrigin.X + CosAngle * HoleRadius.X;
    Vertices[Steps + I].Y := HoleOrigin.Y - SinAngle * HoleRadius.Y;
    VertexColors[Steps + I] := InsideColor;
  end;

  DrawIndexedTriangles(@Vertices[0], @VertexColors[0], @Indices[0], Length(Vertices), Length(Indices) div 3,
    BlendingEffect);

  for I := 0 to Steps - 1 do
  begin
    Theta := I / (Steps - 1);

    Vertices[I].X := AreaTopLeft.X + Theta * AreaSize.X;
    Vertices[I].Y := AreaTopLeft.Y + AreaSize.Y;
    VertexColors[I] := OutsideColor;

    Angle := Pi * 1.25 + Theta * Pi * 0.5;
    SinCos(Angle, SinAngle, CosAngle);

    Vertices[Steps + I].X := HoleOrigin.X + CosAngle * HoleRadius.X;
    Vertices[Steps + I].Y := HoleOrigin.Y - SinAngle * HoleRadius.Y;
    VertexColors[Steps + I] := InsideColor;
  end;

  DrawIndexedTriangles(@Vertices[0], @VertexColors[0], @Indices[0], Length(Vertices), Length(Indices) div 3,
    BlendingEffect);
end;

function TCustomCanvas.Initialize: Boolean;
begin
  if not NeedsInitialization then
    Exit(True);

  if FInitialized then
    Exit(False);

  if not InitCanvas then
    Exit(False);

  FInitialized := True;
  Result := True;
end;

procedure TCustomCanvas.Finalize;
begin
  if FInitialized and NeedsInitialization then
  begin
    DoneCanvas;
    FInitialized := False;
  end;
end;

function TCustomCanvas.InternalBeginScene: Boolean;
begin
  if not FInitialized then
    Exit(False);

  if FSceneBeginCount < 1 then
  begin
    FCacheStall := 0;

    if not BeginDraw then
      Exit(False);

    FInitialClipRect := GetClipRect;
    FClipRectQueue.Clear;

    FAttributes := [TCanvasAttribute.Antialias];
    UpdateAttributes;
  end
  else
  begin
    FClipRectQueue.Add(GetClipRect);
    SetClipRect(FInitialClipRect);
  end;

  Inc(FSceneBeginCount);
  Result := True;
end;

procedure TCustomCanvas.InternalEndScene;
begin
  if FInitialized and (FSceneBeginCount > 0) then
  begin
    Dec(FSceneBeginCount);

    if FSceneBeginCount > 0 then
    begin
      if FClipRectQueue.Count > 0 then
      begin
        SetClipRect(FClipRectQueue[FClipRectQueue.Count - 1].Rect);
        FClipRectQueue.Remove(FClipRectQueue.Count - 1);
      end;
    end
    else
      EndDraw;
  end;
end;

function TCustomCanvas.BeginScene: Boolean;
begin
  if FParent <> nil then
    Result := FParent.CanvasBeginScene
  else
    Result := InternalBeginScene;
end;

procedure TCustomCanvas.EndScene;
begin
  if FParent <> nil then
    FParent.CanvasEndScene
  else
    InternalEndScene;
end;

function TCustomCanvas.SetPalette(const Palette: TIntColorPalette): Boolean;
begin
  Result := False;
end;

procedure TCustomCanvas.ResetPalette;
begin
end;

function TCustomCanvas.SetEffect(const AEffect: TCustomCanvasEffect): Boolean;
begin
  Result := False;
end;

procedure TCustomCanvas.Reset;
begin
  FCurrentTexture := nil;
  FCurrentTextureMapping := Quad(0.0, 0.0, 0.0, 0.0);
end;

procedure TCustomCanvas.OnDeviceRestore(const Sender: TObject; const EventData, UserData: Pointer);
begin
  if not DeviceRestore then
    LogText(SCannotRestoreCanvas, TLogType.Error);
end;

procedure TCustomCanvas.OnDeviceRelease(const Sender: TObject; const EventData, UserData: Pointer);
begin
  DeviceRelease;
end;

procedure TCustomCanvas.UseTexture(const Texture: TCustomBaseTexture; const Mapping: TQuad);
begin
  FCurrentTexture := Texture;
  FCurrentTextureMapping := Mapping;

  if Texture <> nil then
    FCurrentPremultipliedAlpha := Texture.PremultipliedAlpha;
end;

procedure TCustomCanvas.UseTexturePx(const Texture: TCustomBaseTexture; const Mapping: TQuad);

  function PixelToLogical(const Texture: TCustomBaseTexture; const Position: TPoint2f): TPoint2f; inline;
  begin
    if Texture.Width > 0 then
      Result.X := Position.X / Texture.Width
    else
      Result.X := Position.X;

    if Texture.Height > 0 then
      Result.Y := Position.Y / Texture.Height
    else
      Result.Y := Position.Y;
  end;

begin
  if Texture <> nil then
  begin
    FCurrentTextureMapping.Values[0] := PixelToLogical(Texture, Mapping.Values[0]);
    FCurrentTextureMapping.Values[1] := PixelToLogical(Texture, Mapping.Values[1]);
    FCurrentTextureMapping.Values[2] := PixelToLogical(Texture, Mapping.Values[2]);
    FCurrentTextureMapping.Values[3] := PixelToLogical(Texture, Mapping.Values[3]);
  end
  else
    FCurrentTextureMapping := Mapping;

  FCurrentTexture := Texture;

  if Texture <> nil then
    FCurrentPremultipliedAlpha := Texture.PremultipliedAlpha;
end;

procedure TCustomCanvas.UseImage(const Image: TCustomCanvasImage);
begin
  UseTexture(Image.GetTexture(0), Quad(0.0, 0.0, 1.0, 1.0));
end;

procedure TCustomCanvas.UseImage(const Image: TCustomCanvasImage; const Mapping: TQuad;
  const TextureIndex: Integer);
begin
  if Image <> nil then
    UseTexture(Image.GetTexture(TextureIndex), Mapping)
  else
    UseTexture(nil, Mapping);
end;

procedure TCustomCanvas.UseImagePx(const Image: TCustomCanvasImage; const Mapping: TQuad;
  const TextureIndex: Integer);
begin
  if Image <> nil then
    UseTexturePx(Image.GetTexture(TextureIndex), Mapping)
  else
    UseTexturePx(nil, Mapping);
end;

procedure TCustomCanvas.UseImageRegion(const Image: TCustomCanvasImage; const Region: Integer);
var
  RegItem: TIntRectList.PItem;
begin
  if Image <> nil then
  begin
    RegItem := Image.GetRegion(Region);
    if RegItem <> nil then
      UseTexturePx(Image.GetTexture(SizeInt(RegItem.Data)), Quad(RegItem.Rect))
    else
      UseTexture(nil, Quad(0.0, 0.0, 0.0, 0.0));
  end
  else
    UseTexture(nil, Quad(0.0, 0.0, 0.0, 0.0));
end;

procedure TCustomCanvas.UseImageRegion(const Image: TCustomCanvasImage;
  const Region: Integer; const SrcRect: TIntRect; const Mirror: Boolean;
  const Flip: Boolean);
var
  RegItem: TIntRectList.PItem;
  Mapping: TQuad;
begin
  if Image <> nil then
  begin
    RegItem := Image.GetRegion(Region);
    if RegItem <> nil then
    begin
      Mapping := Quad(RegItem.Rect.Left + SrcRect.Left, RegItem.Rect.Top + SrcRect.Top, SrcRect.Width,
        SrcRect.Height);

      if Mirror then
        Mapping := Mapping.Mirror;

      if Flip then
        Mapping := Mapping.Flip;

      UseTexturePx(Image.GetTexture(SizeInt(RegItem.Data)), Mapping);
    end
    else
      UseTexture(nil, Quad(0.0, 0.0, 0.0, 0.0));
  end
  else
    UseTexture(nil, Quad(0.0, 0.0, 0.0, 0.0));
end;

procedure TCustomCanvas.UseImageRegion(const Image: TCustomCanvasImage; const Region: Integer; const Mirror,
  Flip: Boolean);
var
  RegItem: TIntRectList.PItem;
  Mapping: TQuad;
begin
  if Image <> nil then
  begin
    RegItem := Image.GetRegion(Region);
    if RegItem <> nil then
    begin
      Mapping := Quad(RegItem.Rect);

      if Mirror then
        Mapping := Mapping.Mirror;

      if Flip then
        Mapping := Mapping.Flip;

      UseTexturePx(Image.GetTexture(SizeInt(RegItem.Data)), Mapping);
    end
    else
      UseTexture(nil, Quad(0.0, 0.0, 0.0, 0.0));
  end
  else
    UseTexture(nil, Quad(0.0, 0.0, 0.0, 0.0));
end;

procedure TCustomCanvas.TexQuad(const Points: TQuad; const Colors: TColorRect;
  const BlendingEffect: TBlendingEffect);
const
  Indices: packed array[0..5] of LongInt = (0, 1, 2, 2, 3, 0);
var
  Vertices: packed array[0..3] of TPoint2f;
  VertexColors: packed array[0..3] of TIntColor;
begin
  Vertices[0] := Points.TopLeft;
  Vertices[1] := Points.TopRight;
  Vertices[2] := Points.BottomRight;
  Vertices[3] := Points.BottomLeft;

  VertexColors[0] := Colors.TopLeft;
  VertexColors[1] := Colors.TopRight;
  VertexColors[2] := Colors.BottomRight;
  VertexColors[3] := Colors.BottomLeft;

  DrawTexturedTriangles(FCurrentTexture, @Vertices[0], @FCurrentTextureMapping.Values[0], @VertexColors[0],
    @Indices[0], 4, 2, BlendingEffect);
end;

{$ENDREGION}

end.
