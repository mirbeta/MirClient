unit PXL.Types;
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
{< Essential types, constants and functions working with vectors, colors, pixels and rectangles that are
   used throughout the entire framework. }
interface

{$INCLUDE PXL.Config.inc}

uses
  PXL.TypeDef;

{$REGION 'Basic Types'}

type
  { This type is used to pass @link(TPixelFormat) by reference. }
  PPixelFormat = ^TPixelFormat;

  { Defines how individual pixels and their colors are encoded in images and textures. The order of letters
    in the constants defines the order of the  encoded components; R stands for Red, G for Green, B for Blue,
    A for Alpha, L for Luminance and X for Not Used (or discarded); F at the end means floating-point format. }
  TPixelFormat = (

    { Unknown pixel format. It is usually returned when no valid pixel format is available. In some cases, it
      can be specified to indicate that the format should be selected by default or automatically. @br @br }
    Unknown,

    { 32-bit RGBA pixel format. The most commonly used pixel format for storing and loading textures and
      images. @br @br }
    A8R8G8B8,

    { 32-bit RGB pixel format that has no alpha-channel. Should be used for images and textures that have no
      transparency information in them. @br @br }
    X8R8G8B8,

    { 16-bit RGBA pixel format with 4 bits for each channel. This format can be used as a replacement for
      @italic(A8R8G8B8) format in cases where memory footprint is important at the expense of visual
      quality. @br @br }
    A4R4G4B4,

    { 16-bit RGB pixel format with 4 bits unused. It is basically @italic(A4R4G4B4) with alpha-channel
      discarded. This format is widely supported, but in typical applications it is more convenient to use
      @italic(R5G6B5) instead. @br @br }
    X4R4G4B4,

    { 16-bit RGB pixel format. This format can be used as an alternative to A8R8G8B8 in cases where memory
      footprint is important at the expense of visual quality. @br @br }
    R5G6B5,

    { 16-bit RGBA pixel format with one bit dedicated for alpha-channel. This format can be used for images
      where a transparency mask is used; that is, the pixel is either transparent or not, typical for those
      images where a single color is picked to be transparent. In this product, there is little need for this
      format because @italic(AlphaTool) can be used to generate alpha channel for images with masked color,
      which then can be used with any other higher-quality format. @br @br }
    A1R5G5B5,

    { 16-bit RGB pixel format with only 15 bits used for actual storage. This format was common on legacy
      hardware but recently it is rarely used or even supported. @br @br }
    X1R5G5B5,

    { 8-bit RGBA pixel format that was originally supported by OpenGL in earlier implementations. This format
      can significantly save disk space and memory consumption (if supported in hardware) but at the expense
      of very low visual quality. @br @br }
    A2R2G2B2,

    { 8-bit RGB pixel format. An extreme low-quality format useful only in special circumstances and mainly
      for storage. It is more commonly supported on ATI video cards than on Nvidia, being really scarce on
      newer hardware. @br @br }
    R3G3B2,

    { 16-bit RGBA pixel format with uneven bit distribution among the components. It is more supported on
      AMD video cards and can be rarely found on newer hardware. In many cases it is more useful to use
      @italic(A4R4G4B4) format. @br @br }
    A8R3G3B2,

    { 32-bit RGBA pixel format with 10 bits used for each component of red, green and blue, being a
      higher-quality variant of @italic(A8R8G8B8). It is more commonly supported on some video cards than its
      more practical cousin @italic(A2R10G10B10). @br @br }
    A2B10G10R10,

    { 64-bit RGBA pixel format with each channel having 16 bits. @br @br }
    A16B16G16R16,

    { 16-bit luminance pixel format. One of the best formats to be used with bitmap fonts, which is also
      widely supported. @br @br }
    A8L8,

    { 8-bit luminance pixel format. This format can be used as a low quality replacement for @italic(A8L8) to
      represent bitmap fonts. @br @br }
    A4L4,

    { 16-bit luminance pixel format that can be used to represent high-quality grayscale images and
      textures. @br @br }
    L16,

    { 8-bit luminance pixel format. This format can be used for grayscale images and textures. @br @br }
    L8,

    { 16-bit floating-point pixel format, which has only one component. This is useful in shaders either as
      a render target or as a data source. @br @br }
    R16F,

    { 32-bit floating-point pixel format containing two components with 16 bits each. This can be used in
      shaders as a data source. @br @br }
    G16R16F,

    { 64-bit floating-point RGBA pixel format with each component having 16 bits. It can be used as a special
      purpose texture or a render target with shaders. @br @br }
    A16B16G16R16F,

    { 32-bit floating-point pixel format, which has only one component. This format is typically used as
      render target for shadow mapping. @br @br }
    R32F,

    { 64-bit floating-point pixel format containing two components with 32 bits each, mainly useful in
      shaders as a data source. @br @br }
    G32R32F,

    { 128-bit floating-point RGBA pixel format with each component having 32 bits. It can be used as a
      special purpose texture or a render target with shaders. @br @br }
    A32B32G32R32F,

    { 8-bit alpha pixel format. This format can be used as an alpha-channel format for applications that
      require low memory footprint and require transparency information only. Its usefulness, however, is
      severely limited because it is only supported only on newer video cards and when converted in hardware
      to @italic(A8R8G8B8), it has zero values for red, green and blue components; in other words, it is
      basically a black color that also has alpha channel. @br @br }
    A8,

    { 32-bit pixel format that has only green and red components 16 bits each. This format is more useful for
      shaders where only one or two components are needed but with extra resolution. @br @br }
    G16R16,

    { 32-bit RGBA pixel format with 10 bits used for each component of red, green and blue, with only 2 bits
      dedicated to alpha channel. @br @br }
    A2R10G10B10,

    { 32-bit RGB pixel format with 10 bits used for each component of red, green and blue, with the
      remaining 2 bits unused (and/or undefined). @br @br }
    X2R10G10B10,

    { 32-bit BGRA pixel format. This is similar to @italic(A8R8G8B8) format but with red and blue components
      exchanged. @br @br }
    A8B8G8R8,

    { 32-bit BGR pixel format that has no alpha-channel, similar to @italic(X8R8G8B8) but with red and blue
      components exchanged. @br @br }
    X8B8G8R8,

    { 24-bit RGB pixel format. This format can be used for storage and it is unsuitable for rendering both on
      @italic(DirectX) and @italic(OpenGL). @br @br }
    R8G8B8,

    { 32-bit ABGR pixel format. This format is common to some MSB configurations such as
      @italic(Apple Carbon) interface. @br @br }
    B8G8R8A8,

    { 32-bit BGR pixel format that has no alpha-channel. This format is common to some MSB configurations
      such as the one used by @italic(LCL) in @italic(Apple Carbon) interface. @br @br }
    B8G8R8X8,

    { 8-bit palette indexed format, where each value points to a list of colors, which was popular in DOS
      applications. @br @br }
    I8
  );

  { Pointer to @link(TDepthStencil). }
  PDepthStencil = ^TDepthStencil;

  { Support level for depth and stencil buffers. }
  TDepthStencil = (
    { No depth or stencil buffers should be supported. @br @br }
    None,

    { Depth but not stencil buffers should be supported. @br @br }
    DepthOnly,

    { Both depth and stencil buffers should be supported. }
    Full);

  { Defines how alpha-channel should be handled in the loaded image. }
  TAlphaFormatRequest = (
    { Alpha-channel can be handled either way. @br @br }
    DontCare,

    { Alpha-channel in the image should not be premultiplied. Under normal circumstances, this is the
      recommended approach as it preserves RGB color information in its original form. However, when using
      mipmapping for images that have alpha-channel, @italic(Premultiplied) gives more accurate
      results. @br @br }
    NonPremultiplied,

    { Alpha-channel in the image should be premultiplied. Under normal circumstances, this is not recommended
      as the image would lose information after RGB components are premultiplied by alpha (and for smaller
      alpha values, less information is preserved). However, when using mipmapping for images that have
      alpha-channel, this gives more accurate results. }
    Premultiplied);

  { Standard notification event used throughout the framework. }
  TStandardNotifyEvent = procedure(const Sender: TObject) of object;

{$ENDREGION}
{$REGION 'TIntColor'}

type
  // Pointer to @link(TIntColorValue).
  PIntColorValue = ^TIntColorValue;

  { Raw (untyped) color value is represented as a raw 32-bit unsigned integer, with components allocated according to
    @italic(TPixelFormat.A8R8G8B8) format. }
  TIntColorValue = {$IFDEF NEXTGEN}Cardinal{$ELSE}LongWord{$ENDIF};

  // Pointer to @link(TIntColor).
  PIntColor = ^TIntColor;

  { General-purpose color value that is represented as 32-bit unsigned integer, with components allocated according to
    @italic(TPixelFormat.A8R8G8B8) format. }
  TIntColor = {$IFDEF NEXTGEN}Cardinal{$ELSE}LongWord{$ENDIF};

  // Pointer to @link(TIntColorRec).
  PIntColorRec = ^TIntColorRec;

  { Alternative representation of @link(TIntColor), where each element can be accessed as an individual value.
    This can be safely typecast to @link(TIntColor) and vice-versa. }
  TIntColorRec = record
    case Cardinal of
      0: (// Blue value ranging from 0 (no intensity) to 255 (fully intense).
          Blue: Byte;
          // Green value ranging from 0 (no intensity) to 255 (fully intense).
          Green: Byte;
          // Red value ranging from 0 (no intensity) to 255 (fully intense).
          Red: Byte;
          // Alpha-channel value ranging from 0 (translucent) to 255 (opaque).
          Alpha: Byte;);
      1: { Values represented as an array, with indexes corresponding to blue (0), green (1), red (2) and
          alpha-channel (3). }
         (Values: packed array[0..3] of Byte);
  end;

  // Pointer to @link(TIntColorPalette).
  PIntColorPalette = ^TIntColorPalette;

  // A fixed palette of 256 colors, typically used to emulate legacy 8-bit indexed modes.
  TIntColorPalette = array[0..255] of TIntColor;

const
  // Predefined constant for opaque Black color.
  IntColorBlack = $FF000000;

  // Predefined constant for opaque White color.
  IntColorWhite = $FFFFFFFF;

  // Predefined constant for translucent Black color.
  IntColorTranslucentBlack = $00000000;

  // Predefined constant for translucent White color.
  IntColorTranslucentWhite = $00FFFFFF;

{ Creates 32-bit RGBA color with the specified color value, having its alpha-channel multiplied by the
  specified coefficient and divided by 255. }
function IntColor(const Color: TIntColor; const Alpha: Integer): TIntColor; overload; inline;

{ Creates 32-bit RGBA color where the specified color value has its alpha-channel multiplied by the given
  coefficient. }
function IntColor(const Color: TIntColor; const Alpha: VectorFloat): TIntColor; overload; inline;

{ Creates 32-bit RGBA color where the original color value has its components multiplied by the given
  grayscale value and alpha-channel multiplied by the specified coefficient, and all components divided
  by 255. }
function IntColor(const Color: TIntColor; const Gray, Alpha: Integer): TIntColor; overload; inline;

{ Creates 32-bit RGBA color where the original color value has its components multiplied by the given
  grayscale value and alpha-channel multiplied by the specified coefficient. }
function IntColor(const Color: TIntColor; const Gray, Alpha: VectorFloat): TIntColor; overload; inline;

// Creates 32-bit RGBA color using specified individual components for red, green, blue and alpha channel.
function IntColorRGB(const Red, Green, Blue: Integer; const Alpha: Integer = 255): TIntColor; overload;

// Creates 32-bit RGBA color using specified grayscale and alpha values.
function IntColorGray(const Gray: Integer; const Alpha: Integer = 255): TIntColor; overload; inline;

// Creates 32-bit RGBA color using specified grayscale and alpha-channel values (both multiplied by 255).
function IntColorGray(const Gray: VectorFloat; const Alpha: VectorFloat = 1.0): TIntColor; overload; inline;

{ Creates 32-bit RGBA color with the specified alpha-channel and each of red, green and blue components set
  to 255. }
function IntColorAlpha(const Alpha: Integer): TIntColor; overload; inline;

{ Creates 32-bit RGBA color with alpha-channel specified by the given coefficient (multiplied by 255) and
  the rest of components set to 255. }
function IntColorAlpha(const Alpha: VectorFloat): TIntColor; overload; inline;

// Switches red and blue channels in 32-bit RGBA color value.
function DisplaceRB(const Color: TIntColor): TIntColor;

// Inverts each of the components in the pixel, including alpha-channel.
function InvertPixel(const Color: TIntColor): TIntColor;

{ Takes 32-bit RGBA color with unpremultiplied alpha and multiplies each of red, green, and blue components
  by its alpha channel, resulting in premultiplied alpha color. }
function PremultiplyAlpha(const Color: TIntColor): TIntColor;

{ Takes 32-bit RGBA color with premultiplied alpha channel and divides each of its red, green, and blue
  components by alpha, resulting in unpremultiplied alpha color. }
function UnpremultiplyAlpha(const Color: TIntColor): TIntColor;

// Adds two 32-bit RGBA color values together clamping the resulting values if necessary.
function AddPixels(const Color1, Color2: TIntColor): TIntColor;

// Subtracts two 32-bit RGBA color values clamping the resulting values if necessary.
function SubtractPixels(const Color1, Color2: TIntColor): TIntColor;

// Multiplies two 32-bit RGBA color values together.
function MultiplyPixels(const Color1, Color2: TIntColor): TIntColor;

// Computes average of two given 32-bit RGBA color values.
function AveragePixels(const Color1, Color2: TIntColor): TIntColor;

// Computes average of four given 32-bit RGBA color values.
function AverageFourPixels(const Color1, Color2, Color3, Color4: TIntColor): TIntColor;

// Computes average of six given 32-bit RGBA color values.
function AverageSixPixels(const Color1, Color2, Color3, Color4, Color5, Color6: TIntColor): TIntColor;

{ Computes alpha-blending for a pair of 32-bit RGBA colors values.
    @italic(Alpha) can be in [0..255] range. }
function BlendPixels(const Color1, Color2: TIntColor; const Alpha: Integer): TIntColor;

{ Computes resulting alpha-blended value between four 32-bit RGBA colors using linear interpolation.
    @italic(AlphaX) and @italic(AlphaY) can be in [0..255] range. }
function BlendFourPixels(const TopLeft, TopRight, BottomRight, BottomLeft: TIntColor; const AlphaX,
  AlphaY: Integer): TIntColor;

{ Computes alpha-blending for a pair of 32-bit RGBA colors values using floating-point approach. For a faster
  alternative, use @link(BlendPixels).
    @italic(Alpha) can be in [0..1] range.  }
function LerpPixels(const Color1, Color2: TIntColor; const Alpha: VectorFloat): TIntColor;

{ Computes resulting alpha-blended value between four 32-bit RGBA colors using linear interpolation.
    @italic(AlphaX) and @italic(AlphaY) can be in [0..1] range. }
function LerpFourPixels(const TopLeft, TopRight, BottomRight, BottomLeft: TIntColor; const AlphaX,
  AlphaY: VectorFloat): TIntColor;

{ Returns grayscale value in range of [0..255] from the given 32-bit RGBA color value. The resulting value
  can be considered the color's @italic(luma). The alpha-channel is ignored. }
function PixelToGray(const Color: TIntColor): Integer;

{ Returns grayscale value in range of [0..65535] from the given 32-bit RGBA color value. The resulting value
  can be considered the color's @italic(luma). The alpha-channel is ignored. }
function PixelToGray16(const Color: TIntColor): Integer;

{ Returns grayscale value in range of [0..1] from the given 32-bit RGBA color value. The resulting value can
  be considered the color's @italic(luma). The alpha-channel is ignored. }
function PixelToGrayFloat(const Color: TIntColor): VectorFloat;

{ Extracts alpha-channel from two grayscale samples. The sample must be rendered with the same color on two
  different backgrounds, preferably on black and white; the resulting colors are provided in @italic(Src1)
  and @italic(Src2), with original backgrounds in @italic(Bk1) and @italic(Bk2). The resulting alpha-channel
  and original color are computed and returned. This method is particularly useful for calculating
  alpha-channel when rendering GDI fonts or in tools that generate resulting images without providing
  alpha-channel (therefore rendering the same image on two backgrounds is sufficient to calculate its
  alpha-channel). }
procedure ExtractGrayAlpha(const SourceGray1, SourceGray2, Background1, Background2: VectorFloat;
  out Alpha, Gray: VectorFloat); overload;

{$ENDREGION}
{$REGION 'TColorPair'}

type
  // Pointer to @link(TColorPair).
  PColorPair = ^TColorPair;

  { A combination of two colors, primarily used for displaying text with the first color being on top and the
    second being on bottom. The format for specifying colors is defined as @italic(TPixelFormat.A8R8G8B8). }
  TColorPair = record
  public
    { @exclude } class operator Implicit(const Color: TIntColor): TColorPair; inline;

    { Returns @True if two colors are different at least in one of their red, green, blue or alpha
      components. }
    function HasGradient: Boolean;

    // Returns @True if at least one of the colors has non-zero alpha channel.
    function HasAlpha: Boolean;
  public
    case Cardinal of
      0: (// First color entry, which can be reinterpreted as top or left color depending on context.
          First: TIntColor;

          // Second color entry, which can be reinterpreted as bottom or right color depending on context.
          Second: TIntColor;);
      1:// Two color pair represented as an array.
        (Values: array[0..1] of TIntColor;);
  end;

const
  // Predefined constant for a pair of opaque Black colors.
  ColorPairBlack: TColorPair = (First: $FF000000; Second: $FF000000);

  // Predefined constant for a pair of opaque White colors.
  ColorPairWhite: TColorPair = (First: $FFFFFFFF; Second: $FFFFFFFF);

  // Predefined constant for a pair of translucent Black colors.
  ColorPairTranslucentBlack: TColorPair = (First: $00000000; Second: $00000000);

  // Predefined constant for a pair of translucent White colors.
  ColorPairTranslucentWhite: TColorPair = (First: $00FFFFFF; Second: $00FFFFFF);

// Creates two 32-bit RGBA color gradient from specified pair of values.
function ColorPair(const First, Second: TIntColor): TColorPair; overload; inline;

// Creates two 32-bit RGBA color gradient with both values set to specified color.
function ColorPair(const Color: TIntColor): TColorPair; overload; inline;

{$ENDREGION}
{$REGION 'TColorRect'}

type
  // Pointer to @link(TColorRect).
  PColorRect = ^TColorRect;

  { A combination of four colors, primarily used for displaying colored quads, where each color corresponds
    to top/left,top/right, bottom/right and bottom/left accordingly (clockwise). The format for specifying
    colors is defined as @italic(TPixelFormat.A8R8G8B8). }
  TColorRect = record
  public
    { @exclude } class operator Implicit(const Color: TIntColor): TColorRect; inline;

    { Returns @True if at least one of four colors is different from others in red, green, blue or alpha
      components. }
    function HasGradient: Boolean;

    // Returns @True if at least one of the colors has non-zero alpha channel.
    function HasAlpha: Boolean;
  public
    case Cardinal of
      0: (// Color corresponding to top/left corner.
          TopLeft: TIntColor;
          // Color corresponding to top/right corner.
          TopRight: TIntColor;
          // Color corresponding to bottom/right corner.
          BottomRight: TIntColor;
          // Color corresponding to bottom/left corner.
          BottomLeft: TIntColor;
        );
      1: // Four colors represented as an array.
        (Values: array[0..3] of TIntColor);
  end;

const
  // Predefined constant for four opaque Black colors.
  ColorRectBlack: TColorRect = (TopLeft: $FF000000; TopRight: $FF000000; BottomRight: $FF000000;
    BottomLeft: $FF000000);

  // Predefined constant for four opaque White colors.
  ColorRectWhite: TColorRect = (TopLeft: $FFFFFFFF; TopRight: $FFFFFFFF; BottomRight: $FFFFFFFF;
    BottomLeft: $FFFFFFFF);

  // Predefined constant for four translucent Black colors.
  ColorRectTranslucentBlack: TColorRect = (TopLeft: $00000000; TopRight: $00000000; BottomRight: $00000000;
    BottomLeft: $00000000);

  // Predefined constant for four translucent White colors.
  ColorRectTranslucentWhite: TColorRect = (TopLeft: $00FFFFFF; TopRight: $00FFFFFF; BottomRight: $00FFFFFF;
    BottomLeft: $00FFFFFF);

// Creates a construct of four colors using individual components.
function ColorRect(const TopLeft, TopRight, BottomRight, BottomLeft: TIntColor): TColorRect; overload; inline;

// Creates a construct of four colors having the same component in each corner.
function ColorRect(const Color: TIntColor): TColorRect; overload; inline;

// Creates a construct of four colors from two color pair to create horizontal gradient.
function ColorRectH(const Color: TColorPair): TColorRect; overload; inline;

// Creates a construct of four colors from two color values to create horizontal gradient.
function ColorRectH(const Left, Right: TIntColor): TColorRect; overload; inline;

// Creates a construct of four colors from two color pair to create vertical gradient.
function ColorRectV(const Color: TColorPair): TColorRect; overload; inline;

// Creates a construct of four colors from two color values to create vertical gradient.
function ColorRectV(const Top, Bottom: TIntColor): TColorRect; overload; inline;

{$ENDREGION}
{$REGION 'TFloatColor'}

type
  // Pointer to @link(TFloatColor).
  PFloatColor = ^TFloatColor;

  { A special high-precision color value that has each individual component represented as 32-bit
    floating-point value in range of [0, 1]. Although components may have values outside of aforementioned
    range, such colors cannot be reliably displayed on the screen. }
  TFloatColor = record
  public
    { @exclude } class operator Add(const Color1, Color2: TFloatColor): TFloatColor;
    { @exclude } class operator Subtract(const Color1, Color2: TFloatColor): TFloatColor;
    { @exclude } class operator Multiply(const Color1, Color2: TFloatColor): TFloatColor;
    { @exclude } class operator Divide(const Color1, Color2: TFloatColor): TFloatColor;
    { @exclude } class operator Multiply(const Color: TFloatColor; const Theta: VectorFloat): TFloatColor;
    { @exclude } class operator Multiply(const Theta: VectorFloat; const Color: TFloatColor): TFloatColor;
    { @exclude } class operator Divide(const Color: TFloatColor; const Theta: VectorFloat): TFloatColor;
    { @exclude } class operator Equal(const Color1, Color2: TFloatColor): Boolean;
    { @exclude } class operator NotEqual(const Color1, Color2: TFloatColor): Boolean; inline;

    // Inverts current color by applying formula "Xn = 1.0 - X" for each component.
    function Invert: TFloatColor;

    { Takes current color with unpremultiplied alpha and multiplies each of red, green, and blue components
      by its alpha channel, resulting in premultiplied alpha color. }
    function PremultiplyAlpha: TFloatColor;

    { Takes current color with premultiplied alpha channel and divides each of its red, green, and blue
      components by alpha, resulting in unpremultiplied alpha color. }
    function UnpremultiplyAlpha: TFloatColor;

    // Computes average between current and destination floating-point color values.
    function Average(const Color: TFloatColor): TFloatColor;

    { Computes alpha-blending between current and destination floating-point colors values.
        @italic(Alpha) can be in [0..1] range. }
    function Lerp(const Color: TFloatColor; const Alpha: VectorFloat): TFloatColor;

    { Returns grayscale value in range of [0..1] from the current color value. The resulting value can be
      considered the color's @italic(luma). The alpha-channel is ignored. }
    function Gray: VectorFloat;

    // Clamps each component of current color to [0, 1] range.
    function Saturate: TFloatColor;

    // Converts current floating-point color to 32-bit integer representation.
    function ToInt: TIntColor;
  public
    case Cardinal of
      0: (// Red value ranging from 0.0 (no intensity) to 1.0 (fully intense).
          Red: VectorFloat;
          // Green value ranging from 0.0 (no intensity) to 1.0 (fully intense).
          Green: VectorFloat;
          // Blue value ranging from 0.0 (no intensity) to 1.0 (fully intense).
          Blue: VectorFloat;
          // Alpha-channel value ranging from 0.0 (translucent) to 1.0 (opaque).
          Alpha: VectorFloat;);
      1: { Values represented as an array, with indexes corresponding to red (0), green (1), blue (2) and
           alpha-channel (3). }
         (Values: array[0..3] of VectorFloat);
  end;

const
  // Predefined constant for opaque Black color.
  FloatColorBlack: TFloatColor = (Red: 0.0; Green: 0.0; Blue: 0.0; Alpha: 1.0);

  // Predefined constant for opaque White color.
  FloatColorWhite: TFloatColor = (Red: 1.0; Green: 1.0; Blue: 1.0; Alpha: 1.0);

  // Predefined constant for translucent Black color.
  FloatColorTranslucentBlack: TFloatColor = (Red: 0.0; Green: 0.0; Blue: 0.0; Alpha: 0.0);

  // Predefined constant for translucent White color.
  FloatColorTranslucentWhite: TFloatColor = (Red: 1.0; Green: 1.0; Blue: 1.0; Alpha: 0.0);

// Creates floating-point color from its 32-bit integer representation.
function FloatColor(const Color: TIntColor): TFloatColor; overload;

// Creates floating-point color using specified individual components for red, green, blue and alpha channel.
function FloatColor(const Red, Green, Blue: VectorFloat;
  const Alpha: VectorFloat = 1.0): TFloatColor; overload;

{$ENDREGION}
{$REGION 'TPoint2i declarations'}

type
  // Pointer to @link(TPoint2i).
  PPoint2i = ^TPoint2i;

  // 2D integer vector.
  TPoint2i = record
    // The coordinate in 2D space.
    X, Y: VectorInt;

    { @exclude } class operator Add(const Point1, Point2f: TPoint2i): TPoint2i; inline;
    { @exclude } class operator Subtract(const Point1, Point2f: TPoint2i): TPoint2i; inline;
    { @exclude } class operator Multiply(const Point1, Point2f: TPoint2i): TPoint2i; inline;
    { @exclude } class operator Divide(const Point1, Point2f: TPoint2i): TPoint2i; inline;
    { @exclude } class operator Negative(const Point: TPoint2i): TPoint2i; inline;
    { @exclude } class operator Multiply(const Point: TPoint2i; const Theta: VectorInt): TPoint2i; inline;
    { @exclude } class operator Multiply(const Theta: VectorInt; const Point: TPoint2i): TPoint2i; inline;
    { @exclude } class operator Divide(const Point: TPoint2i; const Theta: VectorInt): TPoint2i; inline;
    { @exclude } class operator Divide(const Point: TPoint2i; const Theta: VectorFloat): TPoint2i; inline;
    { @exclude } class operator Equal(const Point1, Point2f: TPoint2i): Boolean; inline;
    { @exclude } class operator NotEqual(const Point1, Point2f: TPoint2i): Boolean; inline;

    // Returns vector with X and Y swapped.
    function Swap: TPoint2i; inline;

    // Tests whether both X and Y are zero.
    function Empty: Boolean; inline;

    // Returns length of current vector.
    function Length: VectorFloat; inline;

    // Calculates distance between current and given points.
    function Distance(const Point: TPoint2i): VectorFloat; inline;

    // Returns an angle (in radians) at which the current vector is pointing at.
    function Angle: VectorFloat; inline;

    { Calculates a dot product between current and the specified 2D vector. The dot product is an indirect
      measure of the angle between two vectors. }
    function Dot(const Point: TPoint2i): VectorInt; inline;

    // Calculates a cross product between current and the specified 2D vector, or analog of thereof.
    function Cross(const Point: TPoint2i): VectorInt; inline;

    { Interpolates between current and destination 2D integer vectors.
        @param(Point The destination vector to be used in the interpolation)
        @param(Theta The mixture of the two vectors with the a range of [0..1].) }
    function Lerp(const Point: TPoint2i; const Theta: VectorFloat): TPoint2i;

    // Tests whether current point is inside the triangle specified by given three vertices.
    function InsideTriangle(const Vertex1, Vertex2, Vertex3: TPoint2i): Boolean;
  end;

const
  // Predefined constant, where X and Y are zero.
  ZeroPoint2i: TPoint2i = (X: 0; Y: 0);

  // Predefined constant, where X and Y are one.
  UnityPoint2i: TPoint2i = (X: 1; Y: 1);

  // Predefined constant, where X = 1 and Y = 0.
  AxisXPoint2i: TPoint2i = (X: 1; Y: 0);

  // Predefined constant, where X = 0 and Y = 1.
  AxisYPoint2i: TPoint2i = (X: 0; Y: 1);

  // Predefined constant for "negative infinity".
  MinusInfinity2i: TPoint2i = (X: Low(VectorInt) + 1; Y: Low(VectorInt) + 1);

  // Predefined constant for "positive infinity".
  PlusInfinity2i: TPoint2i = (X: High(VectorInt); Y: High(VectorInt));

  // Predefined constant that can be interpreted as "undefined value".
  Undefined2i: TPoint2i = (X: Low(VectorInt); Y: Low(VectorInt));

// Creates a @link(TPoint2i) record using the specified coordinates.
function Point2i(const X, Y: VectorInt): TPoint2i;

{$ENDREGION}
{$REGION 'TPoint2f declarations'}

type
  // Pointer to @link(TPoint2f).
  PPoint2f = ^TPoint2f;

  // 2D floating-point vector.
  TPoint2f = record
    // The coordinate in 2D space.
    X, Y: VectorFloat;

    { @exclude } class operator Add(const APoint1, APoint2: TPoint2f): TPoint2f; inline;
    { @exclude } class operator Subtract(const APoint1, APoint2: TPoint2f): TPoint2f; inline;
    { @exclude } class operator Multiply(const APoint1, APoint2: TPoint2f): TPoint2f; inline;
    { @exclude } class operator Divide(const APoint1, APoint2: TPoint2f): TPoint2f; inline;
    { @exclude } class operator Negative(const Point: TPoint2f): TPoint2f; inline;
    { @exclude } class operator Multiply(const Point: TPoint2f; const Theta: VectorFloat): TPoint2f; inline;
    { @exclude } class operator Multiply(const Theta: VectorFloat; const Point: TPoint2f): TPoint2f; inline;
    { @exclude } class operator Divide(const Point: TPoint2f; const Theta: VectorFloat): TPoint2f; inline;
    { @exclude } class operator Divide(const Point: TPoint2f; const Theta: Integer): TPoint2f; inline;
    { @exclude } class operator Implicit(const Point: TPoint2i): TPoint2f; inline;
    { @exclude } class operator Equal(const APoint1, APoint2: TPoint2f): Boolean; inline;
    { @exclude } class operator NotEqual(const APoint1, APoint2: TPoint2f): Boolean; inline;

  {$IFDEF POINT2_FLOAT_TO_INT_IMPLICIT}
    { This implicit conversion is only allowed as a compiler directive because it causes ambiguity and precision
      problems when excessively used. In addition, it can make code more confusing. }
    { @exclude } class operator Implicit(const Point: TPoint2f): TPoint2i;
  {$ENDIF}

    // Returns vector with X and Y swapped.
    function Swap: TPoint2f; inline;

    // Tests whether both X and Y are nearly zero.
    function Empty: Boolean; inline;

    // Returns the length of current 2D vector.
    function Length: VectorFloat; inline;

    // Calculates distance between current and given points.
    function Distance(const Point: TPoint2f): VectorFloat; inline;

    // Returns an angle (in radians) at which current is pointing at.
    function Angle: VectorFloat; inline;

    { Calculates a dot product between current and the specified 2D vectors. The dot product is an indirect
      measure of the angle between two vectors. }
    function Dot(const Point: TPoint2f): VectorFloat; inline;

    // Calculates a cross product between current and the specified 2D vectors, or analog of thereof.
    function Cross(const Point: TPoint2f): VectorFloat; inline;

    // Normalizes current vector to unity length. If the vector is of zero length, it will remain unchanged.
    function Normalize: TPoint2f;

    { Interpolates between current and destination 2D vectors.
        @param(Point The destination vector to be used in the interpolation)
        @param(Theta The mixture of the two vectors with the a range of [0..1].) }
    function Lerp(const Point: TPoint2f; const Theta: VectorFloat): TPoint2f;

    // Tests whether current point is inside the triangle specified by given three vertices.
    function InsideTriangle(const Vertex1, Vertex2, Vertex3: TPoint2f): Boolean;

    // Converts @link(TPoint2f) to @link(TPoint2i) by using floating-point rounding.
    function ToInt: TPoint2i;
  end;

const
  // Predefined constant, where X and Y are zero.
  ZeroPoint2f: TPoint2f = (X: 0.0; Y: 0.0);

  { Predefined constant, where X and Y are one. }
  UnityPoint2f: TPoint2f = (X: 1.0; Y: 1.0);

  { Predefined constant, where X = 1 and Y = 0. }
  AxisXPoint2f: TPoint2f = (X: 1.0; Y: 0.0);

  { Predefined constant, where X = 0 and Y = 1. }
  AxisYPoint2f: TPoint2f = (X: 0.0; Y: 1.0);

{ Creates a @link(TPoint2f) record using the specified coordinates. }
function Point2f(const X, Y: VectorFloat): TPoint2f; inline;

{$ENDREGION}
{$REGION 'TVector3i declarations'}

type
  // Pointer to @link(TVector3i).
  PVector3i = ^TVector3i;

  // 3D integer vector.
  TVector3i = record
    // The coordinate in 3D space.
    X, Y, Z: VectorInt;

    { @exclude } class operator Add(const Vector1, Vector2: TVector3i): TVector3i;
    { @exclude } class operator Subtract(const Vector1, Vector2: TVector3i): TVector3i;
    { @exclude } class operator Multiply(const Vector1, Vector2: TVector3i): TVector3i;
    { @exclude } class operator Divide(const Vector1, Vector2: TVector3i): TVector3i;
    { @exclude } class operator Negative(const Vector: TVector3i): TVector3i;
    { @exclude } class operator Multiply(const Vector: TVector3i; const Theta: VectorInt): TVector3i;
    { @exclude } class operator Multiply(const Theta: VectorInt; const Vector: TVector3i): TVector3i;
    { @exclude } class operator Divide(const Vector: TVector3i; const Theta: VectorInt): TVector3i;
    { @exclude } class operator Equal(const Vector1, Vector2: TVector3i): Boolean;
    { @exclude } class operator NotEqual(const Vector1, Vector2: TVector3i): Boolean; inline;

    // Returns length of current vector.
    function Length: VectorFloat;

    { Calculates a dot product between current and the specified 3D vector. The dot product is an indirect
      measure of angle between two vectors. }
    function Dot(const Vector: TVector3i): VectorInt; inline;

    { Calculates a cross product between current and the specified 3D vector. The resulting vector is
      perpendicular to both vectors and normal to the plane containing them. }
    function Cross(const Vector: TVector3i): TVector3i;

    // Calculates angle between current and the specified 3D vector. The returned value has range of [0..Pi].
    function Angle(const Vector: TVector3i): VectorFloat;

    { Interpolates between current and destination integer 3D vectors.
        @param(Vector The destination vector to be used in the interpolation)
        @param(Theta The mixture of the two vectors with the a range of [0..1].) }
    function Lerp(const Vector: TVector3i; const Theta: VectorFloat): TVector3i;

    // Returns (x, y) portion of 3D vector as @link(TPoint2i).
    function GetXY: TPoint2i; inline;
  end;

const
  // Predefined constant, where X, Y and Z are zero.
  ZeroVector3i: TVector3i = (X: 0; Y: 0; Z: 0);

  // Predefined constant, where X, Y and Z are one.
  UnityVector3i: TVector3i = (X: 1; Y: 1; Z: 1);

  // Predefined constant, where X = 1, Y = 0 and Z = 0.
  AxisXVector3i: TVector3i = (X: 1; Y: 0; Z: 0);

  // Predefined constant, where X = 0, Y = 1 and Z = 0.
  AxisYVector3i: TVector3i = (X: 0; Y: 1; Z: 0);

  // Predefined constant, where X = 0, Y = 0 and Z = 1.
  AxisZVector3i: TVector3i = (X: 0; Y: 0; Z: 1);

// Creates @link(TVector3i) record using the specified coordinates.
function Vector3i(const X, Y, Z: VectorInt): TVector3i; overload; inline;

// Creates @link(TVector3i) record using 2D vector and specified Z coordinate.
function Vector3i(const Point: TPoint2i; const Z: VectorInt = 0): TVector3i; overload; inline;

{$ENDREGION}
{$REGION 'TVector3f declarations'}

type
  // Pointer to @link(TVector3f).
  PVector3f = ^TVector3f;

  // 3D floating-point vector.
  TVector3f = record
    // The coordinate in 3D space.
    X, Y, Z: VectorFloat;

    { @exclude } class operator Add(const Vector1, Vector2: TVector3f): TVector3f; inline;
    { @exclude } class operator Subtract(const Vector1, Vector2: TVector3f): TVector3f; inline;
    { @exclude } class operator Multiply(const Vector1, Vector2: TVector3f): TVector3f; inline;
    { @exclude } class operator Divide(const Vector1, Vector2: TVector3f): TVector3f; inline;
    { @exclude } class operator Negative(const Vector: TVector3f): TVector3f; inline;
    { @exclude } class operator Multiply(const Vector: TVector3f;
      const Theta: VectorFloat): TVector3f; inline;
    { @exclude } class operator Multiply(const Theta: VectorFloat;
      const Vector: TVector3f): TVector3f; inline;
    { @exclude } class operator Divide(const Vector: TVector3f; const Theta: VectorFloat): TVector3f; inline;
    { @exclude } class operator Implicit(const Vector: TVector3i): TVector3f; inline;
    { @exclude } class operator Equal(const Vector1, Vector2: TVector3f): Boolean;
    { @exclude } class operator NotEqual(const Vector1, Vector2: TVector3f): Boolean; inline;

    // Tests whether X, Y and Z are nearly zero.
    function Empty: Boolean;

    // Returns length of current vector.
    function Length: VectorFloat;

    // Calculates distance between current and given vectors.
    function Distance(const Vector: TVector3f): VectorFloat;

    { Calculates a dot product between current and the specified 3D vector. The dot product is an indirect
      measure of angle between two vectors. }
    function Dot(const Vector: TVector3f): VectorFloat; inline;

    { Calculates a cross product between current and the specified 3D vector. The resulting vector is
      perpendicular to both vectors and normal to the plane containing them. }
    function Cross(const Vector: TVector3f): TVector3f;

    // Calculates angle between current and the specified 3D vector. The returned value has range of [0..Pi].
    function Angle(const Vector: TVector3f): VectorFloat;

    // Normalizes current vector to unity length. If current length is zero, the same vector is returned.
    function Normalize: TVector3f;

    // Calculates a portion of current vector that is parallel to the direction vector.
    function Parallel(const Direction: TVector3f): TVector3f;

    // Calculates a portion of current vector that is perpendicular to the direction vector.
    function Perpendicular(const Direction: TVector3f): TVector3f;

    // Calculates 3D vector that is a reflection of current vector from surface given by specified normal.
    function Reflect(const Normal: TVector3f): TVector3f;

    { Interpolates between current and destination 3D vectors.
        @param(Vector The destination vector to be used in the interpolation)
        @param(Theta The mixture of the two vectors with the a range of [0..1].) }
    function Lerp(const Vector: TVector3f; const Theta: VectorFloat): TVector3f;

    // Converts @link(TVector3f) to @link(TVector3i) by using floating-point rounding.
    function ToInt: TVector3i;

    // Returns (x, y) portion of 3D vector as @link(TPoint2f).
    function GetXY: TPoint2f; inline;
  end;

const
  // Predefined constant, where X, Y and Z are zero.
  ZeroVector3f: TVector3f = (X: 0.0; Y: 0.0; Z: 0.0);

  // Predefined constant, where X, Y and Z are one.
  UnityVector3f: TVector3f = (X: 1.0; Y: 1.0; Z: 1.0);

  // Predefined constant, where X = 1, Y = 0 and Z = 0.
  AxisXVector3f: TVector3f = (X: 1.0; Y: 0.0; Z: 0.0);

  // Predefined constant, where X = 0, Y = 1 and Z = 0.
  AxisYVector3f: TVector3f = (X: 0.0; Y: 1.0; Z: 0.0);

  // Predefined constant, where X = 0, Y = 0 and Z = 1.
  AxisZVector3f: TVector3f = (X: 0.0; Y: 0.0; Z: 1.0);

// Creates @link(TVector3f) record using the specified coordinates.
function Vector3f(const X, Y, Z: VectorFloat): TVector3f; overload; inline;

// Creates @link(TVector3f) record using 2D vector and specified Z coordinate.
function Vector3f(const Point: TPoint2f; const Z: VectorFloat = 0.0): TVector3f; overload; inline;

{$ENDREGION}
{$REGION 'TVector4f declarations'}

type
  // Pointer to @link(TVector4f).
  PVector4f = ^TVector4f;

  // 4D (3D + w) floating-point vector.
  TVector4f = record
    // The coordinate in 3D space.
    X, Y, Z: VectorFloat;

    { Homogeneous transform coordinate, mostly used for perspective projection.
      Typically, this component is set to 1.0. }
    W: VectorFloat;

    { @exclude } class operator Add(const Vector1, Vector2: TVector4f): TVector4f;
    { @exclude } class operator Subtract(const Vector1, Vector2: TVector4f): TVector4f;
    { @exclude } class operator Multiply(const Vector1, Vector2: TVector4f): TVector4f;
    { @exclude } class operator Divide(const Vector1, Vector2: TVector4f): TVector4f;
    { @exclude } class operator Negative(const Vector: TVector4f): TVector4f; inline;
    { @exclude } class operator Multiply(const Vector: TVector4f;
      const Theta: VectorFloat): TVector4f; inline;
    { @exclude } class operator Multiply(const Theta: VectorFloat;
      const Vector: TVector4f): TVector4f; inline;
    { @exclude } class operator Divide(const Vector: TVector4f; const Theta: VectorFloat): TVector4f; inline;
    { @exclude } class operator Equal(const Vector1, Vector2: TVector4f): Boolean;
    { @exclude } class operator NotEqual(const Vector1, Vector2: TVector4f): Boolean; inline;

    { Interpolates between current and the specified 4D vector.
        @param(Vector The destination vector to be used in the interpolation)
        @param(Theta The mixture of the two vectors with the a range of [0..1].) }
    function Lerp(const Vector: TVector4f; const Theta: VectorFloat): TVector4f;

    { Returns (X, Y, Z) portion of 4D vector. }
    function GetXYZ: TVector3f; inline;

    { Returns (X, Y, Z) portion of current 4D (3D + W) vector, projecting to W = 1 whenever necessary. }
    function ProjectToXYZ: TVector3f;
  end;

const
  // Predefined constant, where all components are zero.
  ZeroVector4: TVector4f = (X: 0.0; Y: 0.0; Z: 0.0; W: 0.0);

  // Predefined constant, where X, Y and Z are zero, while W = 1.
  ZeroVector4H: TVector4f = (X: 0.0; Y: 0.0; Z: 0.0; W: 1.0);

  // Predefined constant, where all components are one.
  UnityVector4: TVector4f = (X: 1.0; Y: 1.0; Z: 1.0; W: 1.0);

  // Predefined constant, where X = 1, Y = 0, Z = 0 and W = 1.
  AxisXVector4H: TVector4f = (X: 1.0; Y: 0.0; Z: 0.0; W: 1.0);

  // Predefined constant, where X = 0, Y = 1, Z = 0 and W = 1.
  AxisYVector4H: TVector4f = (X: 0.0; Y: 1.0; Z: 0.0; W: 1.0);

  // Predefined constant, where X = 0, Y = 0, Z = 1 and W = 1.
  AxisZVector4H: TVector4f = (X: 0.0; Y: 0.0; Z: 1.0; W: 1.0);

  // Predefined constant, where X = 1, Y = 0, Z = 0 and W = 0.
  AxisXVector4: TVector4f = (X: 1.0; Y: 0.0; Z: 0.0; W: 0.0);

  // Predefined constant, where X = 0, Y = 1, Z = 0 and W = 0.
  AxisYVector4: TVector4f = (X: 0.0; Y: 1.0; Z: 0.0; W: 0.0);

  // Predefined constant, where X = 0, Y = 0, Z = 1 and W = 0.
  AxisZVector4: TVector4f = (X: 0.0; Y: 0.0; Z: 1.0; W: 0.0);

  // Predefined constant, where X = 0, Y = 0, Z = 0 and W = 1.
  AxisWVector4: TVector4f = (X: 0.0; Y: 0.0; Z: 0.0; W: 1.0);

// Creates a @link(TVector4f) record using the specified X, Y, Z and W coordinates.
function Vector4f(const X, Y, Z: VectorFloat; const W: VectorFloat = 1.0): TVector4f; overload; inline;

// Creates a @link(TVector4f) record using the specified 3D vector and W coordinate.
function Vector4f(const Vector: TVector3f; const W: VectorFloat = 1.0): TVector4f; overload; inline;

// Creates a @link(TVector4f) record using the specified 2D point, Z and W coordinates.
function Vector4f(const Point: TPoint2f; const Z: VectorFloat = 0.0;
  const W: VectorFloat = 1.0): TVector4f; overload; inline;

{$ENDREGION}
{$REGION 'TMatrix3f declarations'}

type
  // Pointer to @link(TMatrix3f).
  PMatrix3f = ^TMatrix3f;

  // 3x3 transformation matrix.
  TMatrix3f = record
    // Individual matrix values.
    Data: array[0..2, 0..2] of VectorFloat;

    { @exclude } class operator Add(const Matrix1, Matrix2: TMatrix3f): TMatrix3f;
    { @exclude } class operator Subtract(const Matrix1, Matrix2: TMatrix3f): TMatrix3f;
    { @exclude } class operator Multiply(const Matrix1, Matrix2: TMatrix3f): TMatrix3f;
    { @exclude } class operator Multiply(const Matrix: TMatrix3f; const Theta: VectorFloat): TMatrix3f;
    { @exclude } class operator Multiply(const Theta: VectorFloat; const Matrix: TMatrix3f): TMatrix3f; inline;
    { @exclude } class operator Divide(const Matrix: TMatrix3f; const Theta: VectorFloat): TMatrix3f;
    { @exclude } class operator Multiply(const Point: TPoint2f; const Matrix: TMatrix3f): TPoint2f;

    // Calculates determinant of current matrix.
    function Determinant: VectorFloat;

    // Returns current matrix transposed. That is, rows become columns and vice-versa.
    function Transpose: TMatrix3f;

    // Calculates adjoint matrix for the current matrix.
    function Adjoint: TMatrix3f;

    // Calculates inverse matrix of the current matrix.
    function Inverse: TMatrix3f;

    // Creates 2D translation matrix with specified offset.
    class function Translate(const Offset: TPoint2f): TMatrix3f; overload; static;

    // Creates 2D translation matrix with specified coordinates.
    class function Translate(const X, Y: VectorFloat): TMatrix3f; overload; static; inline;

    // Creates 2D rotation matrix with specified angle (in radiants).
    class function Rotate(const Angle: VectorFLoat): TMatrix3f; static;

    // Creates 2D scaling matrix with specified coefficients.
    class function Scale(const Scale: TPoint2f): TMatrix3f; overload; static;

    // Creates 2D scaling matrix with specified individual coefficients.
    class function Scale(const X, Y: VectorFloat): TMatrix3f; overload; static; inline;

    // Creates 2D scaling matrix with with X and Y equal to the specified coefficient.
    class function Scale(const Scale: VectorFloat): TMatrix3f; overload; static; inline;
  end;

const
  // Predefined constant with values corresponding to @italic(Identity) matrix.
  IdentityMatrix3f: TMatrix3f = (Data: ((1.0, 0.0, 0.0), (0.0, 1.0, 0.0), (0.0, 0.0, 1.0)));

  // Predefined constant, where all matrix values are zero.
  ZeroMatrix3f: TMatrix3f = (Data: ((0.0, 0.0, 0.0), (0.0, 0.0, 0.0), (0.0, 0.0, 0.0)));

{$ENDREGION}
{$REGION 'TMatrix4f declarations'}

type
  // Pointer to @link(TMatrix4f).
  PMatrix4f = ^TMatrix4f;

  // 4x4 transformation matrix.
  TMatrix4f = record
  private
    class function DetSub3(const A1, A2, A3, B1, B2, B3, C1, C2, C3: VectorFloat): VectorFloat; static;
  public
    // Individual matrix values.
    Data: array[0..3, 0..3] of VectorFloat;

    { @exclude } class operator Add(const Matrix1, Matrix2: TMatrix4f): TMatrix4f;
    { @exclude } class operator Subtract(const Matrix1, Matrix2: TMatrix4f): TMatrix4f;
    { @exclude } class operator Multiply(const Matrix1, Matrix2: TMatrix4f): TMatrix4f;
    { @exclude } class operator Multiply(const Matrix: TMatrix4f; const Theta: VectorFloat): TMatrix4f;
    { @exclude } class operator Multiply(const Theta: VectorFloat;
      const Matrix: TMatrix4f): TMatrix4f; inline;
    { @exclude } class operator Divide(const Matrix: TMatrix4f; const Theta: VectorFloat): TMatrix4f;
    { @exclude } class operator Multiply(const Vector: TVector3f; const Matrix: TMatrix4f): TVector3f;
    { @exclude } class operator Multiply(const Vector: TVector4f; const Matrix: TMatrix4f): TVector4f;

    // Calculates determinant of current matrix.
    function Determinant: VectorFloat;

    { Assuming that the current matrix is a view matrix, this method calculates the 3D position where
      the camera (or "eye") is supposedly located. }
    function EyePos: TVector3f;

    { Assuming that the specified matrix is a world matrix, this method calculates the 3D position where
      the object (or "world") is supposedly located. }
    function WorldPos: TVector3f;

    // Transposes current matrix. That is, rows become columns and vice-versa.
    function Transpose: TMatrix4f;

    // Calculates adjoint values for the current matrix.
    function Adjoint: TMatrix4f;

    { Calculates inverse of current matrix. In other words, the new matrix will "undo" any transformations
      that the given matrix would have made. }
    function Inverse: TMatrix4f;

    { Multiplies the given 3D vector as if it was 4D (3D + 1) vector by current matrix, then divides each of
      the resulting components by the resulting value of W (which is then discarded), and converts absolute
      coordinates to screen pixels by using the given target size. }
    function Project(const Vector: TVector3f; const TargetSize: TPoint2f): TPoint2f;

    // Creates 3D translation matrix with specified offset.
    class function Translate(const Offset: TVector3f): TMatrix4f; overload; static;

    // Creates 3D translation matrix with specified X, Y and (optional) Z coordinates.
    class function Translate(const X, Y: VectorFloat;
      const Z: VectorFloat = 0.0): TMatrix4f; overload; static; inline;

    // Creates 3D rotation matrix around X axis with the specified angle.
    class function RotateX(const Angle: VectorFloat): TMatrix4f; static;

    // Creates 3D rotation matrix around Y axis with the specified angle.
    class function RotateY(const Angle: VectorFloat): TMatrix4f; static;

    // Creates 3D rotation matrix around Z axis with the specified angle.
    class function RotateZ(const Angle: VectorFloat): TMatrix4f; static;

    // Creates 3D rotation matrix around specified axis and angle (in radiants).
    class function Rotate(const Axis: TVector3f; const Angle: VectorFloat): TMatrix4f; static;

    // Creates 3D translation matrix with specified coefficients.
    class function Scale(const Scale: TVector3f): TMatrix4f; overload; static;

    // Creates 3D translation matrix with specified X, Y and (optional) Z coefficients.
    class function Scale(const X, Y: VectorFloat;
      const Z: VectorFloat = 0.0): TMatrix4f; overload; static; inline;

    // Creates 3D translation matrix with X, Y and Z equal to the specified coefficient.
    class function Scale(const Scale: VectorFloat): TMatrix4f; overload; static; inline;

    { Creates 3D rotation matrix based on parameters similar to flight dynamics, specifically heading, pitch
      and Bank. Each of the components is specified individually. }
    class function HeadingPitchBank(const Heading, Pitch, Bank: VectorFloat): TMatrix4f; overload; static;

    { Creates 3D rotation matrix based on parameters similar to flight dynamics, specifically heading, pitch
      and Bank. The components are taken from the specified vector with Y corresponding to heading, X to
      pitch and Z to bank. }
    class function HeadingPitchBank(const Vector: TVector3f): TMatrix4f; overload; static;

    { Creates 3D rotation matrix based on parameters similar to flight dynamics, specifically yaw, pitch and
      roll. Each of the components is specified individually. }
    class function YawPitchRoll(const Yaw, Pitch, Roll: VectorFloat): TMatrix4f; overload; static;

    { Creates 3D rotation matrix based on parameters similar to flight dynamics, specifically yaw, pitch and
      roll. The components are taken from the specified vector with Y corresponding to yaw, X to pitch and Z
      to roll. }
    class function YawPitchRoll(const Vector: TVector3f): TMatrix4f; overload; static;

    // Creates a reflection matrix specified by the given vector defining the orientation of the reflection.
    class function Reflect(const Axis: TVector3f): TMatrix4f; static;

    { Creates a view matrix that is defined by the camera's position, its target and vertical
      axis or "ceiling". }
    class function LookAt(const Origin, Target, Ceiling: TVector3f): TMatrix4f; static;

    { Creates perspective projection matrix defined by a field of view on Y axis. This is a common way for
      typical 3D applications. In 3D shooters special care is to be taken because on wide-screen monitors the
      visible area will be bigger when using this constructor. The parameters that define the viewed range
      are important for defining the precision of the depth transformation or a depth-buffer.
        @param(FieldOfView The camera's field of view in radians. For example Pi/4.)
        @param(AspectRatio The screen's aspect ratio. Can be calculated as y/x.)
        @param(MinRange The closest range at which the scene will be viewed.)
        @param(MaxRange The farthest range at which the scene will be viewed.) }
    class function PerspectiveFOVY(const FieldOfView, AspectRatio, MinRange,
      MaxRange: VectorFloat): TMatrix4f; static;

    { Creates perspective projection matrix defined by a field of view on X axis. In 3D shooters the field of
      view needs to be adjusted to allow more visible area on wide-screen monitors. The parameters that
      define the viewed range are important for defining the precision of the depth transformation or a
      depth-buffer.
        @param(FieldOfView The camera's field of view in radians. For example Pi/4.)
        @param(AspectRatio The screen's aspect ratio. Can be calculated as x/y.)
        @param(MinRange The closest range at which the scene will be viewed.)
        @param(MaxRange The farthest range at which the scene will be viewed.) }
    class function PerspectiveFOVX(const FieldOfView, AspectRatio, MinRange,
      MaxRange: VectorFloat): TMatrix4f; static;

    // Creates perspective projection matrix defined by the viewing volume in 3D space.
    class function PerspectiveVOL(const Width, Height, MinRange, MaxRange: VectorFloat): TMatrix4f; static;

    // Creates perspective projection matrix defined by the individual axis's boundaries.
    class function PerspectiveBDS(const Left, Right, Top, Bottom, MinRange,
      MaxRange: VectorFloat): TMatrix4f; static;

    // Creates orthogonal projection matrix defined by the viewing volume in 3D space.
    class function OrthogonalVOL(const Width, Height, MinRange, MaxRange: VectorFloat): TMatrix4f; static;

    // Creates orthogonal projection matrix defined by the individual axis's boundaries.
    class function OrthogonalBDS(const Left, Right, Top, Bottom, MinRange,
      MaxRange: VectorFloat): TMatrix4f; static;
  end;

const
  // Predefined constant with values corresponding to @italic(Identity) matrix.
  IdentityMatrix4f: TMatrix4f = (Data: ((1.0, 0.0, 0.0, 0.0), (0.0, 1.0, 0.0, 0.0), (0.0, 0.0, 1.0, 0.0),
    (0.0, 0.0, 0.0, 1.0)));

  // Predefined constant, where all matrix values are zero.
  ZeroMatrix4f: TMatrix4f = (Data: ((0.0, 0.0, 0.0, 0.0), (0.0, 0.0, 0.0, 0.0), (0.0, 0.0, 0.0, 0.0),
    (0.0, 0.0, 0.0, 0.0)));

{$ENDREGION}
{$REGION 'TQuaternion declarations'}

type
  // Pointer to @link(TQuaternion).
  PQuaternion = ^TQuaternion;

  // 3D quaternion.
  TQuaternion = record
    // Individual quaternion values.
    X, Y, Z, W: VectorFloat;

    { @exclude } class operator Multiply(const Quaternion1, Quaternion2: TQuaternion): TQuaternion;
    { @exclude } class operator Implicit(const Quaternion: TQuaternion): TMatrix4f;
    { @exclude } class operator Explicit(const Matrix: TMatrix4f): TQuaternion;

    // Returns the magnitude of current quaternion.
    function Length: VectorFloat;

    { Normalizes the current quaternion. Note that normally quaternions are always normalized (of course,
      within limits of numerical precision). This function is provided mainly to combat floating point
      "error creep", which occurs after many successive quaternion operations. }
    function Normalize: TQuaternion;

    // Returns rotational angle "theta" that is present in current quaternion.
    function Angle: VectorFloat;

    // Returns rotational axis that is present in current quaternion.
    function Axis: TVector3f;

    { Computes quaternion's conjugate. The resulting quaternion has opposite rotation to the original
      quaternion. }
    function Conjugate: TQuaternion;

    // Computes exponentiation of the given quaternion.
    function Exponentiate(const Exponent: VectorFloat): TQuaternion;

    // Computes the dot product of two given quaternions.
    function Dot(const Quaternion: TQuaternion): VectorFloat;

    { Applies spherical linear interpolation between current and given quaternions.
        @param(Quaternion Destination quaternion to be used in the interpolation)
        @param(Theta The mixture of the two quaternions with range of [0..1].) }
    function Slerp(const Quaternion: TQuaternion; const Theta: VectorFloat): TQuaternion;

    // Creates 3D quaternion containing rotation around X axis with given angle (in radians).
    class function RotateX(const Angle: VectorFloat): TQuaternion; static;

    // Creates 3D quaternion containing rotation around Y axis with given angle (in radians).
    class function RotateY(const Angle: VectorFloat): TQuaternion; static;

    // Creates 3D quaternion containing rotation around Z axis with given angle (in radians).
    class function RotateZ(const Angle: VectorFloat): TQuaternion; static;

    // Creates 3D quaternion containing rotation around an arbitrary axis with given angle (in radians).
    class function Rotate(const Axis: TVector3f; const Angle: VectorFloat): TQuaternion; static;

    { Creates 3D quaternion setup to perform Object-To-Inertial rotation using the angles specified in
      Euler format. }
    class function RotateObjectToIntertial(const Heading, Pitch, Bank: VectorFloat): TQuaternion; static;

    { Creates 3D quaternion setup to perform Inertial-To-Object rotation using the angles specified in
      Euler format. }
    class function RotateInertialToObject(const Heading, Pitch, Bank: VectorFloat): TQuaternion; static;
  end;

const
  // Identity quaternion that can be used to specify an object with no rotation.
  IdentityQuaternion: TQuaternion = (X: 0.0; Y: 0.0; Z: 0.0; W: 1.0);

{$ENDREGION}
{$REGION 'TIntRect declarations'}
type
  // Pointer to @link(TIntRect).
  PIntRect = ^TIntRect;

  // General-purpose integer rectangle POD type defined by top and left margins, width and height.
  TIntRect = record
  private
    function GetRight: VectorInt; inline;
    procedure SetRight(const Value: VectorInt); inline;
    function GetBottom: VectorInt; inline;
    procedure SetBottom(const Value: VectorInt); inline;
    function GetBottomRight: TPoint2i; inline;
    procedure SetBottomRight(const Value: TPoint2i); inline;
  public
    { @exclude } class operator Equal(const Rect1, Rect2: TIntRect): Boolean;
    { @exclude } class operator NotEqual(const Rect1, Rect2: TIntRect): Boolean; inline;

    // Tests whether the rectangle is empty, that is, having width and height of zero or less.
    function Empty: Boolean; inline;

    // Tests whether the given point is inside specified current rectangle.
    function Contains(const Point: TPoint2i): Boolean; overload; inline;

    // Tests whether the given rectangle is contained within current rectangle.
    function Contains(const Rect: TIntRect): Boolean; overload; inline;

    // Tests whether the given rectangle overlaps current one.
    function Overlaps(const Rect: TIntRect): Boolean; inline;

    // Calculates rectangle that results from intersection between current and the given rectangles.
    function Intersect(const Rect: TIntRect): TIntRect; inline;

    // Calculates rectangle that results from union between current and the given rectangles.
    function Union(const Rect: TIntRect): TIntRect; inline;

    // Displaces current rectangle by the given offset.
    function Offset(const Delta: TPoint2i): TIntRect; overload; inline;

    // Displaces current rectangle by each of the given offset values.
    function Offset(const DeltaX, DeltaY: VectorInt): TIntRect; overload; inline;

    // Returns rectangle with left and top decremented, while right and bottom incremented by given offset.
    function Inflate(const Delta: TPoint2i): TIntRect; overload; inline;

    // Returns rectangle with left and top decremented, while right and bottom incremented by given offset.
    function Inflate(const DeltaX, DeltaY: VectorInt): TIntRect; overload;

    { Takes source and destination sizes, source rectangle and destination position, then applies clipping to ensure that
      final rectangle stays within valid boundaries of both source and destination sizes. }
    class function ClipCoords(const SourceSize, DestSize: TPoint2i; var SourceRect: TIntRect;
      var DestPos: TPoint2i): Boolean; static;

    // Right (non-inclusive) margin of the rectangle.
    property Right: VectorInt read GetRight write SetRight;

    // Bottom (non-inclusive) margin of the rectangle.
    property Bottom: VectorInt read GetBottom write SetBottom;

    // Bottom/right (non-inclusive) corner of the rectangle.
    property BottomRight: TPoint2i read GetBottomRight write SetBottomRight;

    case Integer of
      0: (// Left position of the rectangle.
          Left: VectorInt;
          // Top position of the rectangle.
          Top: VectorInt;
          // Width of the rectangle.
          Width: VectorInt;
          // Height of the rectangle.
          Height: VectorInt;);
      1: (// Top/left corner (position) of the rectangle.
          TopLeft: TPoint2i;
          // Size of the rectangle.
          Size: TPoint2i;);
{      2: // Individual values represented as an array.
        (Values: array[0..3] of VectorInt);}
  end;

const
  // Zero (empty) rectangle with integer coordinates.
  ZeroIntRect: TIntRect = (Left: 0; Top: 0; Width: 0; Height: 0);

// Creates rectangle based on top/left position, width and height.
function IntRect(const Left, Top, Width, Height: VectorInt): TIntRect; overload;

// Creates rectangle based on top/left position and size.
function IntRect(const Origin, Size: TPoint2i): TIntRect; overload;

// Creates rectangle based on individual margin bounds.
function IntRectBDS(const Left, Top, Right, Bottom: VectorInt): TIntRect; overload;

// Creates rectangle based on top/left and bottom/right margin bounds.
function IntRectBDS(const TopLeft, BottomRight: TPoint2i): TIntRect; overload;

{$ENDREGION}
{$REGION 'TFloatRect declarations'}

type
  // Pointer to @link(TFloatRect).
  PFloatRect = ^TFloatRect;

  // General-purpose floating-point rectangle POD type defined by top and left margins, width and height.
  TFloatRect = record
  private
    function GetRight: VectorFloat; inline;
    procedure SetRight(const Value: VectorFloat); inline;
    function GetBottom: VectorFloat; inline;
    procedure SetBottom(const Value: VectorFloat); inline;
    function GetBottomRight: TPoint2f; inline;
    procedure SetBottomRight(const Value: TPoint2f); inline;
  public
    { @exclude } class operator Equal(const Rect1, Rect2: TFloatRect): Boolean;
    { @exclude } class operator NotEqual(const Rect1, Rect2: TFloatRect): Boolean; inline;

    // Tests whether the rectangle is empty, that is, having width and height of zero or less.
    function Empty: Boolean;

    // Tests whether the given point is inside specified current rectangle.
    function Contains(const Point: TPoint2f): Boolean; overload;

    // Tests whether the given rectangle is contained within current rectangle.
    function Contains(const Rect: TFloatRect): Boolean; overload; inline;

    // Tests whether the given rectangle overlaps current one.
    function Overlaps(const Rect: TFloatRect): Boolean;

    // Calculates rectangle that results from intersection between current and the given rectangles.
    function Intersect(const Rect: TFloatRect): TFloatRect;

    // Calculates rectangle that results from union between current and the given rectangles.
    function Union(const Rect: TFloatRect): TFloatRect;

    // Displaces current rectangle by the given offset.
    function Offset(const Delta: TPoint2f): TFloatRect; overload;

    // Displaces current rectangle by each of the given offset values.
    function Offset(const DeltaX, DeltaY: VectorFloat): TFloatRect; overload; inline;

    // Returns rectangle with left and top decremented, while right and bottom incremented by given offset.
    function Inflate(const Delta: TPoint2f): TFloatRect; overload;

    // Returns rectangle with left and top decremented, while right and bottom incremented by given offset.
    function Inflate(const DeltaX, DeltaY: VectorFloat): TFloatRect; overload; inline;

    // Converts floating-point rectangle to integer rectangle by rounding margins down.
    function ToInt: TIntRect;

    { Takes source and destination sizes, source and destination rectangles, then applies clipping to ensure
      that final rectangle stays within valid boundaries of both source and destination sizes. }
    class function ClipCoords(const SourceSize, DestSize: TPoint2f; var SourceRect,
      DestRect: TFloatRect): Boolean; static;

    // Right (non-inclusive) margin of the rectangle.
    property Right: VectorFloat read GetRight write SetRight;

    // Bottom (non-inclusive) margin of the rectangle.
    property Bottom: VectorFloat read GetBottom write SetBottom;

    // Bottom/right (non-inclusive) corner of the rectangle.
    property BottomRight: TPoint2f read GetBottomRight write SetBottomRight;

    case Integer of
      0: (// Left position of the rectangle.
          Left: VectorFloat;
          // Top position of the rectangle.
          Top: VectorFloat;
          // Width of the rectangle.
          Width: VectorFloat;
          // Height of the rectangle.
          Height: VectorFloat;);
      1: (// Top/left corner (position) of the rectangle.
          TopLeft: TPoint2f;
          // Size of the rectangle.
          Size: TPoint2f;);
{      2: // Individual values represented as an array.
         (Values: array[0..3] of VectorFloat);}
  end;

const
  // Zero (empty) rectangle with floating-point coordinates.
  ZeroFloatRect: TFloatRect = (Left: 0.0; Top: 0.0; Width: 0.0; Height: 0.0);

// Creates rectangle based on top/left position, width and height.
function FloatRect(const Left, Top, Width, Height: VectorFloat): TFloatRect; overload;

// Creates rectangle based on top/left position and size.
function FloatRect(const Origin, Size: TPoint2f): TFloatRect; overload;

// Creates rectangle based on individual margin bounds.
function FloatRectBDS(const Left, Top, Right, Bottom: VectorFloat): TFloatRect; overload;

// Creates rectangle based on top/left and bottom/right margin bounds.
function FloatRectBDS(const TopLeft, BottomRight: TPoint2f): TFloatRect; overload;

{$ENDREGION}
{$REGION 'TQuad declarations'}

type
  // Pointer to @link(TQuad).
  PQuad = ^TQuad;

  { Special floating-point quadrilateral defined by four vertices starting from top/left in clockwise order.
    This is typically used for rendering color filled and textured quads. }
  TQuad = record
    { @exclude } class operator Equal(const Rect1, Rect2: TQuad): Boolean;
    { @exclude } class operator NotEqual(const Rect1, Rect2: TQuad): Boolean; inline;

    { Rescales vertices of the given quadrilateral by provided coefficient, optionally centering them around
      zero origin. }
    function Scale(const Scale: VectorFloat; const Centered: Boolean = True): TQuad;

    { Creates quadrilateral from another quadrilateral but having left vertices exchanged with the right
      ones, effectively mirroring it horizontally. }
    function Mirror: TQuad;

    { Creates quadrilateral from another quadrilateral but having top vertices exchanged with the bottom
      ones, effectively flipping it vertically. }
    function Flip: TQuad;

    // Transforms (multiplies) vertices of given quadrilateral by the specified matrix.
    function Transform(const Matrix: TMatrix3f): TQuad;

    // Displaces vertices of given quadrilateral by the specified offset.
    function Offset(const Delta: TPoint2f): TQuad; overload;

    // Displaces vertices of given quadrilateral by the specified displacement values.
    function Offset(const DeltaX, DeltaY: VectorFloat): TQuad; overload; inline;

    // Tests whether the point is inside current quadrilateral.
    function Contains(const Point: TPoint2f): Boolean;

    { Creates quadrilateral with the specified top left corner and the given dimensions, which are scaled by
      the provided coefficient. }
    class function Scaled(const Left, Top, Width, Height, Scale: VectorFloat;
      const Centered: Boolean = True): TQuad; static;

    { Creates quadrilateral specified by its dimensions. The rectangle is then rotated and scaled around the
      given middle point (assumed to be inside rectangle's dimensions) and placed in center of the specified
      origin. }
    class function Rotated(const RotationOrigin, Size, RotationCenter: TPoint2f; const Angle: VectorFloat;
      const Scale: VectorFloat = 1.0): TQuad; overload; static;

    { Creates quadrilateral specified by its dimensions. The rectangle is then rotated and scaled around its
      center and placed at the specified origin. }
    class function Rotated(const RotationOrigin, Size: TPoint2f; const Angle: VectorFloat;
      const Scale: VectorFloat = 1.0): TQuad; overload; static; inline;

    { Creates quadrilateral specified by top-left corner and size. The rectangle is then rotated and scaled
      around the specified middle point (assumed to be inside rectangle's dimensions) and placed in the
      center of the specified origin. The difference between this method and @link(Rotated) is that the
      rotation does not preserve centering of the rectangle in case where middle point is not actually
      located in the middle. }
    class function RotatedTL(const TopLeft, Size, RotationCenter: TPoint2f; const Angle: VectorFloat;
      const Scale: VectorFloat = 1.0): TQuad; static; inline;

    case Integer of
      0:( // Top/left vertex position.
          TopLeft: TPoint2f;
          // Top/right vertex position.
          TopRight: TPoint2f;
          // Bottom/right vertex position.
          BottomRight: TPoint2f;
          // Bottom/left vertex position.
          BottomLeft: TPoint2f;);
      1: // Quadrilateral vertices represented as an array. }
         (Values: array[0..3] of TPoint2f);
  end;

// Creates quadrilateral with individually specified vertex coordinates.
function Quad(const TopLeftX, TopLeftY, TopRightX, TopRightY, BottomRightX, BottomRightY, BottomLeftX,
  BottomLeftY: VectorFloat): TQuad; overload;

// Creates quadrilateral with individually specified vertices.
function Quad(const TopLeft, TopRight, BottomRight, BottomLeft: TPoint2f): TQuad; overload;

// Creates quadrilateral rectangle with top/left position, width and height.
function Quad(const Left, Top, Width, Height: VectorFloat): TQuad; overload;

// Creates quadrilateral rectangle from specified floating-point rectangle.
function Quad(const Rect: TFloatRect): TQuad; overload;

// Creates quadrilateral rectangle from specified integer rectangle.
function Quad(const Rect: TIntRect): TQuad; overload;

{$ENDREGION}
{$REGION 'Global declarations'}

{ Interpolates between two values linearly, where @italic(Theta) must be specified in [0..1] range. }
function Lerp(const Value1, Value2, Theta: VectorFloat): VectorFloat;
{$IFNDEF PASDOC} overload; {$ENDIF}

{$IF SIZEOF(VectorFloat) > 4} { @exclude }
function Lerp(const Value1, Value2, Theta: Single): Single; overload;
{$ENDIF}

{ Ensures that the given value stays within specified range limit, clamping it if necessary. }
function Saturate(const Value, MinLimit, MaxLimit: VectorFloat): VectorFloat; inline;
{$IFNDEF PASDOC} overload; {$ENDIF}

{$IF SIZEOF(VectorFloat) > 4} { @exclude }
function Saturate(const Value, MinLimit, MaxLimit: Single): Single; overload; inline;
{$ENDIF}

{ Ensures that the given value stays within specified range limit, clamping it if necessary. }
function Saturate(const Value, MinLimit, MaxLimit: VectorInt): VectorInt; inline;
{$IFNDEF PASDOC} overload; {$ENDIF}

{$IF SIZEOF(VectorInt) <> 4} { @exclude }
function Saturate(const Value, MinLimit, MaxLimit: LongInt): LongInt; overload; inline;
{$ENDIF}

{ Returns @True if the specified value is a power of two or @False otherwise. }
function IsPowerOfTwo(const Value: VectorInt): Boolean;

{ Returns the least power of two greater or equal to the specified value. }
function CeilPowerOfTwo(const Value: VectorInt): VectorInt;

{ Returns the greatest power of two lesser or equal to the specified value. }
function FloorPowerOfTwo(const Value: VectorInt): VectorInt;

{ Transforms value in range of [0, 1] using sine wave (accelerate, decelerate). }
function SineTransform(const Value: VectorFloat): VectorFloat;

{ Transforms value in range of [0, 1] using sine wave accelerating. The curve starts at 0 with almost zero
  acceleration, but in the end going almost linearly. }
function SineAccelerate(const Value: VectorFloat): VectorFloat;

{ Transforms value in range of [0, 1] using sine wave decelerating. The curve starts accelerating quickly at 0, but
  slowly stops at 1. }
function SineDecelerate(const Value: VectorFloat): VectorFloat;

{ Transforms value in range of [0, 1] to go through full cycle  (0 -> 1 -> 0).
  Note that the curve goes from 0 to 1 and then back to 0. }
function SineCycle(const Value: VectorFloat): VectorFloat;

{ Transforms value in range of [0, 1] to go full cycle through sine function (0 -> 1 -> 0 -> -1 -> 0).}
function SineSymCycle(const Value: VectorFloat): VectorFloat;

{$ENDREGION}
{$REGION 'Embedded declarations'}

{$IFDEF EMBEDDED}
  {$DEFINE INTERFACE}
  {$INCLUDE 'PXL.Types.inc'}
  {$UNDEF INTERFACE}
{$ENDIF}

{$ENDREGION}

implementation

{$IFNDEF EMBEDDED}
uses
  Math;
{$ENDIF}

{$REGION 'Embedded declarations'}

{$IFDEF EMBEDDED}
  {$DEFINE IMPLEMENTATION}
  {$INCLUDE 'PXL.Types.inc'}
  {$UNDEF IMPLEMENTATION}
{$ENDIF}

{$ENDREGION}
{$REGION 'Global Functions'}

const
  TwoPi = Pi * 2.0;
  PiHalf = Pi * 0.5;

function Lerp(const Value1, Value2, Theta: VectorFloat): VectorFloat;
begin
  Result := Value1 + (Value2 - Value1) * Theta;
end;

{$IF SIZEOF(VectorFloat) > 4}
function Lerp(const Value1, Value2, Theta: Single): Single;
begin
  Result := Value1 + (Value2 - Value1) * Theta;
end;
{$ENDIF}

function Saturate(const Value, MinLimit, MaxLimit: VectorFloat): VectorFloat;
begin
  Result := Value;

  if Result < MinLimit then
    Result := MinLimit;

  if Result > MaxLimit then
    Result := MaxLimit;
end;

{$IF SIZEOF(VectorFloat) > 4}
function Saturate(const Value, MinLimit, MaxLimit: Single): Single;
begin
  Result := Value;

  if Result < MinLimit then
    Result := MinLimit;

  if Result > MaxLimit then
    Result := MaxLimit;
end;
{$ENDIF}

function Saturate(const Value, MinLimit, MaxLimit: VectorInt): VectorInt;
begin
  Result := Value;

  if Result < MinLimit then
    Result := MinLimit;

  if Result > MaxLimit then
    Result := MaxLimit;
end;

{$IF SIZEOF(VectorInt) <> 4}
function Saturate(const Value, MinLimit, MaxLimit: LongInt): LongInt; overload;
begin
  Result := Value;

  if Result < MinLimit then
    Result := MinLimit;

  if Result > MaxLimit then
    Result := MaxLimit;
end;
{$ENDIF}

function IsPowerOfTwo(const Value: VectorInt): Boolean;
begin
  Result := (Value >= 1) and ((Value and (Value - 1)) = 0);
end;

function CeilPowerOfTwo(const Value: VectorInt): VectorInt;
begin
{$IFDEF EMBEDDED}
  if (Value <= 2) or IsPowerOfTwo(Value) then
    Exit(Value);

  Result := Value shl 1;

  while not IsPowerOfTwo(Result) do
    Result := Result and (Result - 1);
{$ELSE}
  Result := Round(Power(2, Ceil(Log2(Value))))
{$ENDIF}
end;

function FloorPowerOfTwo(const Value: VectorInt): VectorInt;
begin
{$IFDEF EMBEDDED}
  if Value <= 2 then
    Exit(Value);

  Result := Value;

  while not IsPowerOfTwo(Result) do
    Result := Result and (Result - 1);
{$ELSE}
  Result := Round(Power(2, Floor(Log2(Value))))
{$ENDIF}
end;

function SineTransform(const Value: VectorFloat): VectorFloat;
begin
  Result := 0.5 + 0.5 * Sin(Value * Pi - PiHalf);
end;

function SineAccelerate(const Value: VectorFloat): VectorFloat;
begin
  Result := 1.0 + Sin(Value * PiHalf - PiHalf);
end;

function SineDecelerate(const Value: VectorFloat): VectorFloat;
begin
  Result := Sin(Value * PiHalf);
end;

function SineCycle(const Value: VectorFloat): VectorFloat;
begin
  Result := 0.5 + 0.5 * Sin(Value * TwoPi - PiHalf);
end;

function SineSymCycle(const Value: VectorFloat): VectorFloat;
var
  Theta: Single;
begin
  Theta := Abs(Frac(Value));

  if Theta < 0.5 then
    Result := SineCycle(Theta * 2.0)
  else
    Result := -SineCycle((Theta - 0.5) * 2.0);
end;

{$ENDREGION}
{$REGION 'TIntColor'}

function IntColor(const Color: TIntColor; const Alpha: Integer): TIntColor;
begin
  Result := (Color and $00FFFFFF) or TIntColorValue((Integer(Color shr 24) * Alpha) div 255) shl 24;
end;

function IntColor(const Color: TIntColor; const Alpha: VectorFloat): TIntColor;
begin
  Result := IntColor(Color, Integer(Round(Alpha * 255.0)));
end;

function IntColor(const Color: TIntColor; const Gray, Alpha: Integer): TIntColor;
begin
  Result := TIntColorValue((Integer(Color and $FF) * Gray) div 255) or
    (TIntColorValue((Integer((Color shr 8) and $FF) * Gray) div 255) shl 8) or
    (TIntColorValue((Integer((Color shr 16) and $FF) * Gray) div 255) shl 16) or
    (TIntColorValue((Integer((Color shr 24) and $FF) * Alpha) div 255) shl 24);
end;

function IntColor(const Color: TIntColor; const Gray, Alpha: VectorFloat): TIntColor;
begin
  Result := IntColor(Color, Integer(Round(Gray * 255.0)), Integer(Round(Alpha * 255.0)));
end;

function IntColorRGB(const Red, Green, Blue: Integer; const Alpha: Integer = 255): TIntColor;
begin
  Result := TIntColorValue(Blue) or (TIntColorValue(Green) shl 8) or (TIntColorValue(Red) shl 16) or
    (TIntColorValue(Alpha) shl 24);
end;

function IntColorGray(const Gray: Integer; const Alpha: Integer = 255): TIntColor;
begin
  Result := ((TIntColorValue(Gray) and $FF) or ((TIntColorValue(Gray) and $FF) shl 8) or
    ((TIntColorValue(Gray) and $FF) shl 16)) or ((TIntColorValue(Alpha) and $FF) shl 24);
end;

function IntColorGray(const Gray: VectorFloat; const Alpha: VectorFloat = 1.0): TIntColor;
begin
  Result := IntColorGray(Integer(Round(Gray * 255.0)), Integer(Round(Alpha * 255.0)));
end;

function IntColorAlpha(const Alpha: Integer): TIntColor;
begin
  Result := $00FFFFFF or ((TIntColorValue(Alpha) and $FF) shl 24);
end;

function IntColorAlpha(const Alpha: VectorFloat): TIntColor;
begin
  Result := IntColorAlpha(Integer(Round(Alpha * 255.0)));
end;

function DisplaceRB(const Color: TIntColor): TIntColor;
begin
  Result := ((Color and $FF) shl 16) or (Color and $FF00FF00) or ((Color shr 16) and $FF);
end;

function InvertPixel(const Color: TIntColor): TIntColor;
begin
  Result := (255 - (Color and $FF)) or ((255 - ((Color shr 8) and $FF)) shl 8) or
    ((255 - ((Color shr 16) and $FF)) shl 16) or ((255 - ((Color shr 24) and $FF)) shl 24);
end;

function PremultiplyAlpha(const Color: TIntColor): TIntColor;
begin
  Result :=
    (((Color and $FF) * (Color shr 24)) div 255) or
    (((((Color shr 8) and $FF) * (Color shr 24)) div 255) shl 8) or
    (((((Color shr 16) and $FF) * (Color shr 24)) div 255) shl 16) or
    (Color and $FF000000);
end;

function UnpremultiplyAlpha(const Color: TIntColor): TIntColor;
var
  Alpha: Cardinal;
begin
  Alpha := Color shr 24;

  if Alpha > 0 then
    Result := (((Color and $FF) * 255) div Alpha) or (((((Color shr 8) and $FF) * 255) div Alpha) shl 8) or
      (((((Color shr 16) and $FF) * 255) div Alpha) shl 16) or (Color and $FF000000)
  else
    Result := Color;
end;

function AddPixels(const Color1, Color2: TIntColor): TIntColor;
begin
  Result :=
    TIntColorValue(Min(Integer(Color1 and $FF) + Integer(Color2 and $FF), 255)) or
    (TIntColorValue(Min(Integer((Color1 shr 8) and $FF) + Integer((Color2 shr 8) and $FF), 255)) shl 8) or
    (TIntColorValue(Min(Integer((Color1 shr 16) and $FF) + Integer((Color2 shr 16) and $FF), 255)) shl 16) or
    (TIntColorValue(Min(Integer((Color1 shr 24) and $FF) + Integer((Color2 shr 24) and $FF), 255)) shl 24);
end;

function SubtractPixels(const Color1, Color2: TIntColor): TIntColor;
begin
  Result :=
    TIntColorValue(Max(Integer(Color1 and $FF) - Integer(Color2 and $FF), 0)) or
    (TIntColorValue(Max(Integer((Color1 shr 8) and $FF) - Integer((Color2 shr 8) and $FF), 0)) shl 8) or
    (TIntColorValue(Max(Integer((Color1 shr 16) and $FF) - Integer((Color2 shr 16) and $FF), 0)) shl 16) or
    (TIntColorValue(Max(Integer((Color1 shr 24) and $FF) - Integer((Color2 shr 24) and $FF), 0)) shl 24);
end;

function MultiplyPixels(const Color1, Color2: TIntColor): TIntColor;
begin
  Result :=
    TIntColorValue((Integer(Color1 and $FF) * Integer(Color2 and $FF)) div 255) or
    (TIntColorValue((Integer((Color1 shr 8) and $FF) * Integer((Color2 shr 8) and $FF)) div 255) shl 8) or
    (TIntColorValue((Integer((Color1 shr 16) and $FF) * Integer((Color2 shr 16) and $FF)) div 255) shl 16) or
    (TIntColorValue((Integer((Color1 shr 24) and $FF) * Integer((Color2 shr 24) and $FF)) div 255) shl 24);
end;

function AveragePixels(const Color1, Color2: TIntColor): TIntColor;
begin
  Result :=
    (((Color1 and $FF) + (Color2 and $FF)) div 2) or
    (((((Color1 shr 8) and $FF) + ((Color2 shr 8) and $FF)) div 2) shl 8) or
    (((((Color1 shr 16) and $FF) + ((Color2 shr 16) and $FF)) div 2) shl 16) or
    (((((Color1 shr 24) and $FF) + ((Color2 shr 24) and $FF)) div 2) shl 24);
end;

function AverageFourPixels(const Color1, Color2, Color3, Color4: TIntColor): TIntColor;
begin
  Result :=
    (((Color1 and $FF) + (Color2 and $FF) + (Color3 and $FF) + (Color4 and $FF)) div 4) or

    (((((Color1 shr 8) and $FF) + ((Color2 shr 8) and $FF) + ((Color3 shr 8) and $FF) +
    ((Color4 shr 8) and $FF)) div 4) shl 8) or

    (((((Color1 shr 16) and $FF) + ((Color2 shr 16) and $FF) + ((Color3 shr 16) and $FF) +
    ((Color4 shr 16) and $FF)) div 4) shl 16) or

    (((((Color1 shr 24) and $FF) + ((Color2 shr 24) and $FF) + ((Color3 shr 24) and $FF) +
    ((Color4 shr 24) and $FF)) div 4) shl 24);
end;

function AverageSixPixels(const Color1, Color2, Color3, Color4, Color5, Color6: TIntColor): TIntColor;
begin
  Result :=
    (((Color1 and $FF) + (Color2 and $FF) + (Color3 and $FF) + (Color4 and $FF) + (Color5 and $FF) +
    (Color6 and $FF)) div 6) or

    (((((Color1 shr 8) and $FF) + ((Color2 shr 8) and $FF) + ((Color3 shr 8) and $FF) + ((Color4 shr 8) and $FF) +
    ((Color5 shr 8) and $FF) + ((Color6 shr 8) and $FF)) div 6) shl 8) or

    (((((Color1 shr 16) and $FF) + ((Color2 shr 16) and $FF) + ((Color3 shr 16) and $FF) + ((Color4 shr 16) and $FF) +
    ((Color5 shr 16) and $FF) + ((Color6 shr 16) and $FF)) div 6) shl 16) or

    (((((Color1 shr 24) and $FF) + ((Color2 shr 24) and $FF) + ((Color3 shr 24) and $FF) + ((Color4 shr 24) and $FF) +
    ((Color5 shr 24) and $FF) + ((Color6 shr 24) and $FF)) div 6) shl 24);
end;

function BlendPixels(const Color1, Color2: TIntColor; const Alpha: Integer): TIntColor;
begin
  Result :=
    TIntColorValue(Integer(Color1 and $FF) + (((Integer(Color2 and $FF) -
    Integer(Color1 and $FF)) * Alpha) div 255)) or

    (TIntColorValue(Integer((Color1 shr 8) and $FF) + (((Integer((Color2 shr 8) and $FF) -
    Integer((Color1 shr 8) and $FF)) * Alpha) div 255)) shl 8) or

    (TIntColorValue(Integer((Color1 shr 16) and $FF) + (((Integer((Color2 shr 16) and $FF) -
    Integer((Color1 shr 16) and $FF)) * Alpha) div 255)) shl 16) or

    (TIntColorValue(Integer((Color1 shr 24) and $FF) + (((Integer((Color2 shr 24) and $FF) -
    Integer((Color1 shr 24) and $FF)) * Alpha) div 255)) shl 24);
end;

function BlendFourPixels(const TopLeft, TopRight, BottomRight, BottomLeft: TIntColor; const AlphaX,
  AlphaY: Integer): TIntColor;
begin
  Result := BlendPixels(BlendPixels(TopLeft, TopRight, AlphaX), BlendPixels(BottomLeft, BottomRight, AlphaX), AlphaY);
end;

function LerpPixels(const Color1, Color2: TIntColor; const Alpha: VectorFloat): TIntColor;
begin
  Result :=
    TIntColorValue(Integer(Color1 and $FF) + Round((Integer(Color2 and $FF) - Integer(Color1 and $FF)) * Alpha)) or

    (TIntColorValue(Integer((Color1 shr 8) and $FF) + Round((Integer((Color2 shr 8) and $FF) -
    Integer((Color1 shr 8) and $FF)) * Alpha)) shl 8) or

    (TIntColorValue(Integer((Color1 shr 16) and $FF) + Round((Integer((Color2 shr 16) and $FF) -
    Integer((Color1 shr 16) and $FF)) * Alpha)) shl 16) or

    (TIntColorValue(Integer((Color1 shr 24) and $FF) + Round((Integer((Color2 shr 24) and $FF) -
    Integer((Color1 shr 24) and $FF)) * Alpha)) shl 24);
end;

function LerpFourPixels(const TopLeft, TopRight, BottomRight, BottomLeft: TIntColor; const AlphaX,
  AlphaY: VectorFloat): TIntColor;
begin
  Result := LerpPixels(LerpPixels(TopLeft, TopRight, AlphaX), LerpPixels(BottomLeft, BottomRight, AlphaX), AlphaY);
end;

function PixelToGray(const Color: TIntColor): Integer;
begin
  Result := ((Integer(Color and $FF) * 77) + (Integer((Color shr 8) and $FF) * 150) +
    (Integer((Color shr 16) and $FF) * 29)) div 256;
end;

function PixelToGray16(const Color: TIntColor): Integer;
begin
  Result := ((Integer(Color and $FF) * 19588) + (Integer((Color shr 8) and $FF) * 38445) +
    (Integer((Color shr 16) and $FF) * 7503)) div 256;
end;

function PixelToGrayFloat(const Color: TIntColor): VectorFloat;
begin
  Result := ((Color and $FF) * 0.299 + ((Color shr 8) and $FF) * 0.587 + ((Color shr 16) and $FF) * 0.114) / 255.0;
end;

procedure ExtractGrayAlpha(const SourceGray1, SourceGray2, Background1, Background2: VectorFloat; out Alpha,
  Gray: VectorFloat);
begin
  Alpha := (1.0 - (SourceGray2 - SourceGray1)) / (Background2 - Background1);

  if Alpha > VectorEpsilon then
    Gray := (SourceGray1 - (1.0 - Alpha) * Background1) / Alpha
  else
    Gray := SourceGray1;
end;

{$ENDREGION}
{$REGION 'TColorPair'}

class operator TColorPair.Implicit(const Color: TIntColor): TColorPair;
begin
  Result.First := Color;
  Result.Second := Color;
end;

function TColorPair.HasGradient: Boolean;
begin
  Result := First <> Second;
end;

function TColorPair.HasAlpha: Boolean;
begin
  Result := ((First shr 24) > 0) or ((Second shr 24) > 0);
end;

function ColorPair(const First, Second: TIntColor): TColorPair;
begin
  Result.First := First;
  Result.Second := Second;
end;

function ColorPair(const Color: TIntColor): TColorPair;
begin
  Result.First := Color;
  Result.Second := Color;
end;

{$ENDREGION}
{$REGION 'TColorRect'}

class operator TColorRect.Implicit(const Color: TIntColor): TColorRect;
begin
  Result.TopLeft := Color;
  Result.TopRight := Color;
  Result.BottomRight := Color;
  Result.BottomLeft := Color;
end;

function TColorRect.HasGradient: Boolean;
begin
  Result := (TopLeft <> TopRight) or (TopRight <> BottomRight) or (BottomRight <> BottomLeft);
end;

function TColorRect.HasAlpha: Boolean;
begin
  Result := (TopLeft shr 24 > 0) or (TopRight shr 24 > 0) or (BottomRight shr 24 > 0) or (BottomLeft shr 24 > 0);
end;

function ColorRect(const TopLeft, TopRight, BottomRight, BottomLeft: TIntColor): TColorRect;
begin
  Result.TopLeft := TopLeft;
  Result.TopRight := TopRight;
  Result.BottomRight := BottomRight;
  Result.BottomLeft := BottomLeft;
end;

function ColorRect(const Color: TIntColor): TColorRect;
begin
  Result.TopLeft := Color;
  Result.TopRight := Color;
  Result.BottomRight := Color;
  Result.BottomLeft := Color;
end;

function ColorRectH(const Color: TColorPair): TColorRect;
begin
  Result.TopLeft := Color.First;
  Result.TopRight := Color.Second;
  Result.BottomRight := Color.Second;
  Result.BottomLeft := Color.First;
end;

function ColorRectH(const Left, Right: TIntColor): TColorRect;
begin
  Result.TopLeft := Left;
  Result.TopRight := Right;
  Result.BottomRight := Right;
  Result.BottomLeft := Left;
end;

function ColorRectV(const Color: TColorPair): TColorRect;
begin
  Result.TopLeft := Color.First;
  Result.TopRight := Color.First;
  Result.BottomRight := Color.Second;
  Result.BottomLeft := Color.Second;
end;

function ColorRectV(const Top, Bottom: TIntColor): TColorRect;
begin
  Result.TopLeft := Top;
  Result.TopRight := Top;
  Result.BottomRight := Bottom;
  Result.BottomLeft := Bottom;
end;

{$ENDREGION}
{$REGION 'TFloatColor'}

class operator TFloatColor.Add(const Color1, Color2: TFloatColor): TFloatColor;
begin
  Result.Red := Color1.Red + Color2.Red;
  Result.Green := Color1.Green + Color2.Green;
  Result.Blue := Color1.Blue + Color2.Blue;
  Result.Alpha := Color1.Alpha + Color2.Alpha;
end;

class operator TFloatColor.Subtract(const Color1, Color2: TFloatColor): TFloatColor;
begin
  Result.Red := Color1.Red - Color2.Red;
  Result.Green := Color1.Green - Color2.Green;
  Result.Blue := Color1.Blue - Color2.Blue;
  Result.Alpha := Color1.Alpha - Color2.Alpha;
end;

class operator TFloatColor.Multiply(const Color1, Color2: TFloatColor): TFloatColor;
begin
  Result.Red := Color1.Red * Color2.Red;
  Result.Green := Color1.Green * Color2.Green;
  Result.Blue := Color1.Blue * Color2.Blue;
  Result.Alpha := Color1.Alpha * Color2.Alpha;
end;

class operator TFloatColor.Divide(const Color1, Color2: TFloatColor): TFloatColor;
begin
  Result.Red := Color1.Red / Color2.Red;
  Result.Green := Color1.Green / Color2.Green;
  Result.Blue := Color1.Blue / Color2.Blue;
  Result.Alpha := Color1.Alpha / Color2.Alpha;
end;

class operator TFloatColor.Multiply(const Color: TFloatColor; const Theta: VectorFloat): TFloatColor;
begin
  Result.Red := Color.Red * Theta;
  Result.Green := Color.Green * Theta;
  Result.Blue := Color.Blue * Theta;
  Result.Alpha := Color.Alpha * Theta;
end;

class operator TFloatColor.Multiply(const Theta: VectorFloat; const Color: TFloatColor): TFloatColor;
begin
  Result.Red := Theta * Color.Red;
  Result.Green := Theta * Color.Green;
  Result.Blue := Theta * Color.Blue;
  Result.Alpha := Theta * Color.Alpha;
end;

class operator TFloatColor.Divide(const Color: TFloatColor; const Theta: VectorFloat): TFloatColor;
begin
  Result.Red := Color.Red / Theta;
  Result.Green := Color.Green / Theta;
  Result.Blue := Color.Blue / Theta;
  Result.Alpha := Color.Alpha / Theta;
end;

class operator TFloatColor.Equal(const Color1, Color2: TFloatColor): Boolean;
begin
  Result := (Abs(Color1.Red - Color2.Red) < VectorEpsilon) and (Abs(Color1.Green - Color2.Green) < VectorEpsilon)
    and (Abs(Color1.Blue - Color2.Blue) < VectorEpsilon) and (Abs(Color1.Alpha - Color2.Alpha) < VectorEpsilon);
end;

class operator TFloatColor.NotEqual(const Color1, Color2: TFloatColor): Boolean;
begin
  Result := not (Color1 = Color2);
end;

function TFloatColor.Invert: TFloatColor;
begin
  Result.Red := 1.0 - Red;
  Result.Green := 1.0 - Green;
  Result.Blue := 1.0 - Blue;
  Result.Alpha := 1.0 - Alpha;
end;

function TFloatColor.PremultiplyAlpha: TFloatColor;
begin
  Result.Red := Red * Alpha;
  Result.Green := Green * Alpha;
  Result.Blue := Blue * Alpha;
  Result.Alpha := Alpha;
end;

function TFloatColor.UnpremultiplyAlpha: TFloatColor;
begin
  if Alpha > 0.0 then
  begin
    Result.Red := Red / Alpha;
    Result.Green := Green / Alpha;
    Result.Blue := Blue / Alpha;
    Result.Alpha := Alpha;
  end
  else
    Result := Self;
end;

function TFloatColor.Average(const Color: TFloatColor): TFloatColor;
begin
  Result.Red := (Red + Color.Red) * 0.5;
  Result.Green := (Green + Color.Green) * 0.5;
  Result.Blue := (Blue + Color.Blue) * 0.5;
  Result.Alpha := (Alpha + Color.Alpha) * 0.5;
end;

function TFloatColor.Lerp(const Color: TFloatColor; const Alpha: VectorFloat): TFloatColor;
begin
  Result.Red := Red + (Color.Red - Red) * Alpha;
  Result.Green := Green + (Color.Green - Green) * Alpha;
  Result.Blue := Blue + (Color.Blue - Blue) * Alpha;
  Result.Alpha := Self.Alpha + (Color.Alpha - Self.Alpha) * Alpha;
end;

function TFloatColor.Gray: VectorFloat;
begin
  Result := Red * 0.299 + Green * 0.587 + Blue * 0.114;
end;

function TFloatColor.Saturate: TFloatColor;
begin
  Result.Red := PXL.Types.Saturate(Red, 0.0, 1.0);
  Result.Green := PXL.Types.Saturate(Green, 0.0, 1.0);
  Result.Blue := PXL.Types.Saturate(Blue, 0.0, 1.0);
  Result.Alpha := PXL.Types.Saturate(Alpha, 0.0, 1.0);
end;

function TFloatColor.ToInt: TIntColor;
begin
  Result := Cardinal(Round(Blue * 255.0)) or (Cardinal(Round(Green * 255.0)) shl 8) or
    (Cardinal(Round(Red * 255.0)) shl 16) or (Cardinal(Round(Alpha * 255.0)) shl 24);
end;

function FloatColor(const Color: TIntColor): TFloatColor;
begin
  Result.Red := ((Color shr 16) and $FF) / 255.0;
  Result.Green := ((Color shr 8) and $FF) / 255.0;
  Result.Blue := (Color and $FF) / 255.0;
  Result.Alpha := ((Color shr 24) and $FF) / 255.0;
end;

function FloatColor(const Red, Green, Blue, Alpha: VectorFloat): TFloatColor;
begin
  Result.Red := Red;
  Result.Green := Green;
  Result.Blue := Blue;
  Result.Alpha := Alpha;
end;

{$ENDREGION}
{$REGION 'TPoint2i functions'}

class operator TPoint2i.Add(const Point1, Point2f: TPoint2i): TPoint2i;
begin
  Result.X := Point1.X + Point2f.X;
  Result.Y := Point1.Y + Point2f.Y;
end;

class operator TPoint2i.Subtract(const Point1, Point2f: TPoint2i): TPoint2i;
begin
  Result.X := Point1.X - Point2f.X;
  Result.Y := Point1.Y - Point2f.Y;
end;

class operator TPoint2i.Multiply(const Point1, Point2f: TPoint2i): TPoint2i;
begin
  Result.X := Point1.X * Point2f.X;
  Result.Y := Point1.Y * Point2f.Y;
end;

class operator TPoint2i.Divide(const Point1, Point2f: TPoint2i): TPoint2i;
begin
  Result.X := Point1.X div Point2f.X;
  Result.Y := Point1.Y div Point2f.Y;
end;

class operator TPoint2i.Negative(const Point: TPoint2i): TPoint2i;
begin
  Result.X := -Point.X;
  Result.Y := -Point.Y;
end;

class operator TPoint2i.Multiply(const Point: TPoint2i; const Theta: VectorInt): TPoint2i;
begin
  Result.X := Point.X * Theta;
  Result.Y := Point.Y * Theta;
end;

class operator TPoint2i.Multiply(const Theta: VectorInt; const Point: TPoint2i): TPoint2i;
begin
  Result.X := Theta * Point.X;
  Result.Y := Theta * Point.Y;
end;

class operator TPoint2i.Divide(const Point: TPoint2i; const Theta: VectorInt): TPoint2i;
begin
  Result.X := Point.X div Theta;
  Result.Y := Point.Y div Theta;
end;

class operator TPoint2i.Divide(const Point: TPoint2i; const Theta: VectorFloat): TPoint2i;
begin
  Result.X := Round(Point.X / Theta);
  Result.Y := Round(Point.Y / Theta);
end;

class operator TPoint2i.Equal(const Point1, Point2f: TPoint2i): Boolean;
begin
  Result := (Point1.X = Point2f.X) and (Point1.Y = Point2f.Y);
end;

class operator TPoint2i.NotEqual(const Point1, Point2f: TPoint2i): Boolean;
begin
  Result := (Point1.X <> Point2f.X) or (Point1.Y <> Point2f.Y);
end;

function TPoint2i.Swap: TPoint2i;
begin
  Result.X := Y;
  Result.Y := X;
end;

function TPoint2i.Empty: Boolean;
begin
  Result := (X = 0) and (Y = 0);
end;

function TPoint2i.Length: VectorFloat;
begin
  Result := Hypot(X, Y);
end;

function TPoint2i.Distance(const Point: TPoint2i): VectorFloat;
begin
  Result := (Self - Point).Length;
end;

function TPoint2i.Angle: VectorFloat;
begin
  if X <> 0 then
    Result := ArcTan2(-Y, X)
  else
    Result := 0;
end;

function TPoint2i.Dot(const Point: TPoint2i): VectorInt;
begin
  Result := (X * Point.X) + (Y * Point.Y);
end;

function TPoint2i.Cross(const Point: TPoint2i): VectorInt;
begin
  Result := (X * Point.Y) - (Y * Point.X);
end;

function TPoint2i.Lerp(const Point: TPoint2i; const Theta: VectorFloat): TPoint2i;
begin
  Result.X := Round(X + (Point.X - X) * Theta);
  Result.Y := Round(Y + (Point.Y - Y) * Theta);
end;

function TPoint2i.InsideTriangle(const Vertex1, Vertex2, Vertex3: TPoint2i): Boolean;
var
  Det: VectorInt;
begin
  Det := (Y - Vertex2.Y) * (Vertex3.X - Vertex2.X) - (X - Vertex2.X) * (Vertex3.Y - Vertex2.Y);

  Result := (Det * ((Y - Vertex1.Y) * (Vertex2.X - Vertex1.X) - (X - Vertex1.X) *
    (Vertex2.Y - Vertex1.Y)) > 0) and (Det * ((Y - Vertex3.Y) * (Vertex1.X - Vertex3.X) -
    (X - Vertex3.X) * (Vertex1.Y - Vertex3.Y)) > 0);
end;

function Point2i(const X, Y: VectorInt): TPoint2i;
begin
  Result.X := X;
  Result.Y := Y;
end;

{$ENDREGION}
{$REGION 'TPoint2f functions'}

class operator TPoint2f.Add(const APoint1, APoint2: TPoint2f): TPoint2f;
begin
  Result.X := APoint1.X + APoint2.X;
  Result.Y := APoint1.Y + APoint2.Y;
end;

class operator TPoint2f.Subtract(const APoint1, APoint2: TPoint2f): TPoint2f;
begin
  Result.X := APoint1.X - APoint2.X;
  Result.Y := APoint1.Y - APoint2.Y;
end;

class operator TPoint2f.Multiply(const APoint1, APoint2: TPoint2f): TPoint2f;
begin
  Result.X := APoint1.X * APoint2.X;
  Result.Y := APoint1.Y * APoint2.Y;
end;

class operator TPoint2f.Divide(const APoint1, APoint2: TPoint2f): TPoint2f;
begin
  Result.X := APoint1.X / APoint2.X;
  Result.Y := APoint1.Y / APoint2.Y;
end;

class operator TPoint2f.Negative(const Point: TPoint2f): TPoint2f;
begin
  Result.X := -Point.X;
  Result.Y := -Point.Y;
end;

class operator TPoint2f.Multiply(const Point: TPoint2f; const Theta: VectorFloat): TPoint2f;
begin
  Result.X := Point.X * Theta;
  Result.Y := Point.Y * Theta;
end;

class operator TPoint2f.Multiply(const Theta: VectorFloat; const Point: TPoint2f): TPoint2f;
begin
  Result.X := Theta * Point.X;
  Result.Y := Theta * Point.Y;
end;

class operator TPoint2f.Divide(const Point: TPoint2f; const Theta: VectorFloat): TPoint2f;
begin
  Result.X := Point.X / Theta;
  Result.Y := Point.Y / Theta;
end;

class operator TPoint2f.Divide(const Point: TPoint2f; const Theta: Integer): TPoint2f;
begin
  Result.X := Point.X / Theta;
  Result.Y := Point.Y / Theta;
end;

class operator TPoint2f.Implicit(const Point: TPoint2i): TPoint2f;
begin
  Result.X := Point.X;
  Result.Y := Point.Y;
end;

class operator TPoint2f.Equal(const APoint1, APoint2: TPoint2f): Boolean;
begin
  Result := (Abs(APoint1.X - APoint2.X) < VectorEpsilon) and (Abs(APoint1.Y - APoint2.Y) < VectorEpsilon);
end;

class operator TPoint2f.NotEqual(const APoint1, APoint2: TPoint2f): Boolean;
begin
  Result := not (APoint1 = APoint2);
end;

{$IFDEF POINT_FLOAT_TO_INT_IMPLICIT}
class operator TPoint2f.Implicit(const Point: TPoint2f): TPoint2i;
begin
  Result.X := Round(Point.X);
  Result.Y := Round(Point.Y);
end;
{$ENDIF}

function TPoint2f.Swap: TPoint2f;
begin
  Result.X := Y;
  Result.Y := X;
end;

function TPoint2f.Empty: Boolean;
begin
  Result := (Abs(X) < VectorEpsilon) and (Abs(Y) < VectorEpsilon);
end;

function TPoint2f.Length: VectorFloat;
begin
  Result := Hypot(X, Y);
end;

function TPoint2f.Distance(const Point: TPoint2f): VectorFloat;
begin
  Result := (Self - Point).Length;
end;

function TPoint2f.Angle: VectorFloat;
begin
  if Abs(X) > VectorEpsilon then
    Result := ArcTan2(-Y, X)
  else
    Result := 0.0;
end;

function TPoint2f.Dot(const Point: TPoint2f): VectorFloat;
begin
  Result := (X * Point.X) + (Y * Point.Y);
end;

function TPoint2f.Cross(const Point: TPoint2f): VectorFloat;
begin
  Result := (X * Point.Y) - (Y * Point.X);
end;

function TPoint2f.Normalize: TPoint2f;
var
  Amp: VectorFloat;
begin
  Amp := Length;
  if Abs(Amp) > VectorEpsilon then
    Result := Self / Amp
  else
    Result := Self;
end;

function TPoint2f.Lerp(const Point: TPoint2f; const Theta: VectorFloat): TPoint2f;
begin
  Result.X := X + (Point.X - X) * Theta;
  Result.Y := Y + (Point.Y - Y) * Theta;
end;

function TPoint2f.InsideTriangle(const Vertex1, Vertex2, Vertex3: TPoint2f): Boolean;
var
  Det: VectorFloat;
begin
  Det := (Y - Vertex2.Y) * (Vertex3.X - Vertex2.X) - (X - Vertex2.X) * (Vertex3.Y - Vertex2.Y);

  Result := (Det * ((Y - Vertex1.Y) * (Vertex2.X - Vertex1.X) - (X - Vertex1.X) *
    (Vertex2.Y - Vertex1.Y)) > 0.0) and (Det * ((Y - Vertex3.Y) * (Vertex1.X - Vertex3.X) -
    (X - Vertex3.X) * (Vertex1.Y - Vertex3.Y)) > 0.0);
end;

function TPoint2f.ToInt: TPoint2i;
begin
  Result.X := Round(X);
  Result.Y := Round(Y);
end;

function Point2f(const X, Y: VectorFloat): TPoint2f; inline;
begin
  Result.X := X;
  Result.Y := Y;
end;

{$ENDREGION}
{$REGION 'TVector3i functions'}

class operator TVector3i.Add(const Vector1, Vector2: TVector3i): TVector3i;
begin
  Result.X := Vector1.X + Vector2.X;
  Result.Y := Vector1.Y + Vector2.Y;
  Result.Z := Vector1.Z + Vector2.Z;
end;

class operator TVector3i.Subtract(const Vector1, Vector2: TVector3i): TVector3i;
begin
  Result.X := Vector1.X - Vector2.X;
  Result.Y := Vector1.Y - Vector2.Y;
  Result.Z := Vector1.Z - Vector2.Z;
end;

class operator TVector3i.Multiply(const Vector1, Vector2: TVector3i): TVector3i;
begin
  Result.X := Vector1.X * Vector2.X;
  Result.Y := Vector1.Y * Vector2.Y;
  Result.Z := Vector1.Z * Vector2.Z;
end;

class operator TVector3i.Divide(const Vector1, Vector2: TVector3i): TVector3i;
begin
  Result.X := Vector1.X div Vector2.X;
  Result.Y := Vector1.Y div Vector2.Y;
  Result.Z := Vector1.Z div Vector2.Z;
end;

class operator TVector3i.Negative(const Vector: TVector3i): TVector3i;
begin
  Result.X := -Vector.X;
  Result.Y := -Vector.Y;
  Result.Z := -Vector.Z;
end;

class operator TVector3i.Multiply(const Vector: TVector3i; const Theta: VectorInt): TVector3i;
begin
  Result.X := Vector.X * Theta;
  Result.Y := Vector.Y * Theta;
  Result.Z := Vector.Z * Theta;
end;

class operator TVector3i.Multiply(const Theta: VectorInt; const Vector: TVector3i): TVector3i;
begin
  Result.X := Theta * Vector.X;
  Result.Y := Theta * Vector.Y;
  Result.Z := Theta * Vector.Z;
end;

class operator TVector3i.Divide(const Vector: TVector3i; const Theta: VectorInt): TVector3i;
begin
  Result.X := Vector.X div Theta;
  Result.Y := Vector.Y div Theta;
  Result.Z := Vector.Z div Theta;
end;

class operator TVector3i.Equal(const Vector1, Vector2: TVector3i): Boolean;
begin
  Result := (Vector1.X = Vector2.X) and (Vector1.Y = Vector2.Y) and (Vector1.Z = Vector2.Z);
end;

class operator TVector3i.NotEqual(const Vector1, Vector2: TVector3i): Boolean;
begin
  Result := not (Vector1 = Vector2);
end;

function TVector3i.Length: VectorFloat;
begin
  Result := Sqrt(Dot(Self));
end;

function TVector3i.Dot(const Vector: TVector3i): VectorInt;
begin
  Result := (X * Vector.X) + (Y * Vector.Y) + (Z * Vector.Z);
end;

function TVector3i.Cross(const Vector: TVector3i): TVector3i;
begin
  Result.X := (Y * Vector.Z) - (Z * Vector.Y);
  Result.Y := (Z * Vector.X) - (X * Vector.Z);
  Result.Z := (X * Vector.Y) - (Y * Vector.X);
end;

function TVector3i.Angle(const Vector: TVector3i): VectorFloat;
var
  Amp, CosValue: VectorFloat;
begin
  Amp := Sqrt(Dot(Self) * Vector.Dot(Vector));

  if Amp > VectorEpsilon then
    CosValue := Dot(Vector) / Amp
  else
    CosValue := Dot(Vector) / VectorEpsilon;

  Result := ArcCos(Saturate(CosValue, -1.0, 1.0));
end;

function TVector3i.Lerp(const Vector: TVector3i; const Theta: VectorFloat): TVector3i;
begin
  Result.X := Round(X + (Vector.X - X) * Theta);
  Result.Y := Round(Y + (Vector.Y - Y) * Theta);
  Result.Z := Round(Z + (Vector.Z - Z) * Theta);
end;

function TVector3i.GetXY: TPoint2i;
begin
  Result.X := X;
  Result.Y := Y;
end;

function Vector3i(const X, Y, Z: VectorInt): TVector3i;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := Z;
end;

function Vector3i(const Point: TPoint2i; const Z: VectorInt): TVector3i; overload;
begin
  Result.X := Point.X;
  Result.Y := Point.Y;
  Result.Z := Z;
end;

{$ENDREGION}
{$REGION 'TVector3f functions'}

class operator TVector3f.Add(const Vector1, Vector2: TVector3f): TVector3f;
begin
  Result.X := Vector1.X + Vector2.X;
  Result.Y := Vector1.Y + Vector2.Y;
  Result.Z := Vector1.Z + Vector2.Z;
end;

class operator TVector3f.Subtract(const Vector1, Vector2: TVector3f): TVector3f;
begin
  Result.X := Vector1.X - Vector2.X;
  Result.Y := Vector1.Y - Vector2.Y;
  Result.Z := Vector1.Z - Vector2.Z;
end;

class operator TVector3f.Multiply(const Vector1, Vector2: TVector3f): TVector3f;
begin
  Result.X := Vector1.X * Vector2.X;
  Result.Y := Vector1.Y * Vector2.Y;
  Result.Z := Vector1.Z * Vector2.Z;
end;

class operator TVector3f.Divide(const Vector1, Vector2: TVector3f): TVector3f;
begin
  Result.X := Vector1.X / Vector2.X;
  Result.Y := Vector1.Y / Vector2.Y;
  Result.Z := Vector1.Z / Vector2.Z;
end;

class operator TVector3f.Negative(const Vector: TVector3f): TVector3f;
begin
  Result.X := -Vector.X;
  Result.Y := -Vector.Y;
  Result.Z := -Vector.Z;
end;

class operator TVector3f.Multiply(const Vector: TVector3f; const Theta: VectorFloat): TVector3f;
begin
  Result.X := Vector.X * Theta;
  Result.Y := Vector.Y * Theta;
  Result.Z := Vector.Z * Theta;
end;

class operator TVector3f.Multiply(const Theta: VectorFloat; const Vector: TVector3f): TVector3f;
begin
  Result.X := Theta * Vector.X;
  Result.Y := Theta * Vector.Y;
  Result.Z := Theta * Vector.Z;
end;

class operator TVector3f.Divide(const Vector: TVector3f; const Theta: VectorFloat): TVector3f;
var
  InvTheta: VectorFloat;
begin
  InvTheta := 1.0 / Theta;
  Result.X := Vector.X * InvTheta;
  Result.Y := Vector.Y * InvTheta;
  Result.Z := Vector.Z * InvTheta;
end;

class operator TVector3f.Implicit(const Vector: TVector3i): TVector3f;
begin
  Result.X := Vector.X;
  Result.Y := Vector.Y;
  Result.Z := Vector.Z;
end;

class operator TVector3f.Equal(const Vector1, Vector2: TVector3f): Boolean;
begin
  Result :=
    (Abs(Vector1.X - Vector2.X) < VectorEpsilon) and
    (Abs(Vector1.Y - Vector2.Y) < VectorEpsilon) and
    (Abs(Vector1.Z - Vector2.Z) < VectorEpsilon);
end;

class operator TVector3f.NotEqual(const Vector1, Vector2: TVector3f): Boolean;
begin
  Result := not (Vector1 = Vector2);
end;

function TVector3f.Empty: Boolean;
begin
  Result := (Abs(X) < VectorEpsilon) and (Abs(Y) < VectorEpsilon) and (Abs(Z) < VectorEpsilon);
end;

function TVector3f.Length: VectorFloat;
begin
  Result := Sqrt(Dot(Self));
end;

function TVector3f.Distance(const Vector: TVector3f): VectorFloat;
begin
  Result := (Self - Vector).Length;
end;

function TVector3f.Dot(const Vector: TVector3f): VectorFloat;
begin
  Result := (X * Vector.X) + (Y * Vector.Y) + (Z * Vector.Z);
end;

function TVector3f.Cross(const Vector: TVector3f): TVector3f;
begin
  Result.X := (Y * Vector.Z) - (Z * Vector.Y);
  Result.Y := (Z * Vector.X) - (X * Vector.Z);
  Result.Z := (X * Vector.Y) - (Y * Vector.X);
end;

function TVector3f.Angle(const Vector: TVector3f): VectorFloat;
var
  Amp, CosValue: VectorFloat;
begin
  Amp := Sqrt(Dot(Self) * Vector.Dot(Vector));

  if Amp > VectorEpsilon then
    CosValue := Self.Dot(Vector) / Amp
  else
    CosValue := Self.Dot(Vector) / VectorEpsilon;

  Result := ArcCos(Saturate(CosValue, -1.0, 1.0));
end;

function TVector3f.Normalize: TVector3f;
var
  Amp, InvAmp: VectorFloat;
begin
  Amp := Length;
  if Abs(Amp) > VectorEpsilon then
  begin
    InvAmp := 1.0 / Amp;
    Result := Self * InvAmp;
  end
  else
    Result := Self;
end;

function TVector3f.Parallel(const Direction: TVector3f): TVector3f;
begin
  Result := Direction * (Self.Dot(Direction) / Sqr(Direction.Length));
end;

function TVector3f.Perpendicular(const Direction: TVector3f): TVector3f;
begin
  Result := Self - Self.Parallel(Direction);
end;

function TVector3f.Reflect(const Normal: TVector3f): TVector3f;
begin
  Result := Self - (Normal * (Self.Dot(Normal) * 2.0));
end;

function TVector3f.Lerp(const Vector: TVector3f; const Theta: VectorFloat): TVector3f;
begin
  Result.X := X + (Vector.X - X) * Theta;
  Result.Y := Y + (Vector.Y - Y) * Theta;
  Result.Z := Z + (Vector.Z - Z) * Theta;
end;

function TVector3f.ToInt: TVector3i;
begin
  Result.X := Round(X);
  Result.Y := Round(Y);
  Result.Z := Round(Z);
end;

function TVector3f.GetXY: TPoint2f;
begin
  Result.X := X;
  Result.Y := Y;
end;

function Vector3f(const X, Y, Z: VectorFloat): TVector3f;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := Z;
end;

function Vector3f(const Point: TPoint2f; const Z: VectorFloat): TVector3f; overload;
begin
  Result.X := Point.X;
  Result.Y := Point.Y;
  Result.Z := Z;
end;

{$ENDREGION}
{$REGION 'TVector4f functions'}

class operator TVector4f.Add(const Vector1, Vector2: TVector4f): TVector4f;
begin
  Result.X := Vector1.X + Vector2.X;
  Result.Y := Vector1.Y + Vector2.Y;
  Result.Z := Vector1.Z + Vector2.Z;
  Result.W := Vector1.W + Vector2.W;
end;

class operator TVector4f.Subtract(const Vector1, Vector2: TVector4f): TVector4f;
begin
  Result.X := Vector1.X - Vector2.X;
  Result.Y := Vector1.Y - Vector2.Y;
  Result.Z := Vector1.Z - Vector2.Z;
  Result.W := Vector1.W - Vector2.W;
end;

class operator TVector4f.Multiply(const Vector1, Vector2: TVector4f): TVector4f;
begin
  Result.X := Vector1.X * Vector2.X;
  Result.Y := Vector1.Y * Vector2.Y;
  Result.Z := Vector1.Z * Vector2.Z;
  Result.W := Vector1.W * Vector2.W;
end;

class operator TVector4f.Divide(const Vector1, Vector2: TVector4f): TVector4f;
begin
  Result.X := Vector1.X / Vector2.X;
  Result.Y := Vector1.Y / Vector2.Y;
  Result.Z := Vector1.Z / Vector2.Z;
  Result.W := Vector1.W / Vector2.W;
end;

class operator TVector4f.Negative(const Vector: TVector4f): TVector4f;
begin
  Result.X := -Vector.X;
  Result.Y := -Vector.Y;
  Result.Z := -Vector.Z;
  Result.W := -Vector.W;
end;

class operator TVector4f.Multiply(const Vector: TVector4f; const Theta: VectorFloat): TVector4f;
begin
  Result.X := Vector.X * Theta;
  Result.Y := Vector.Y * Theta;
  Result.Z := Vector.Z * Theta;
  Result.W := Vector.W * Theta;
end;

class operator TVector4f.Multiply(const Theta: VectorFloat; const Vector: TVector4f): TVector4f;
begin
  Result.X := Theta * Vector.X;
  Result.Y := Theta * Vector.Y;
  Result.Z := Theta * Vector.Z;
  Result.W := Theta * Vector.W;
end;

class operator TVector4f.Divide(const Vector: TVector4f; const Theta: VectorFloat): TVector4f;
begin
  Result.X := Vector.X / Theta;
  Result.Y := Vector.Y / Theta;
  Result.Z := Vector.Z / Theta;
  Result.W := Vector.W / Theta;
end;

class operator TVector4f.Equal(const Vector1, Vector2: TVector4f): Boolean;
begin
  Result := (Abs(Vector1.X - Vector2.X) < VectorEpsilon) and (Abs(Vector1.Y - Vector2.Y) < VectorEpsilon) and
    (Abs(Vector1.Z - Vector2.Z) < VectorEpsilon) and (Abs(Vector1.W - Vector2.W) < VectorEpsilon);
end;

class operator TVector4f.NotEqual(const Vector1, Vector2: TVector4f): Boolean;
begin
  Result := not (Vector1 = Vector2);
end;

function TVector4f.Lerp(const Vector: TVector4f; const Theta: VectorFloat): TVector4f;
begin
  Result.X := X + (Vector.X - X) * Theta;
  Result.Y := Y + (Vector.Y - Y) * Theta;
  Result.Z := Z + (Vector.Z - Z) * Theta;
  Result.W := W + (Vector.W - W) * Theta;
end;

function TVector4f.GetXYZ: TVector3f;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := Z;
end;

function TVector4f.ProjectToXYZ: TVector3f;
var
  InvW: VectorFloat;
begin
  if Abs(W) > VectorEpsilon then
    Result := GetXYZ * (1.0 / W)
  else
    Result := GetXYZ;
end;

function Vector4f(const X, Y, Z, W: VectorFloat): TVector4f;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := Z;
  Result.W := W;
end;

function Vector4f(const Vector: TVector3f; const W: VectorFloat): TVector4f;
begin
  Result.X := Vector.X;
  Result.Y := Vector.Y;
  Result.Z := Vector.Z;
  Result.W := W;
end;

function Vector4f(const Point: TPoint2f; const Z, W: VectorFloat): TVector4f;
begin
  Result.X := Point.X;
  Result.Y := Point.Y;
  Result.Z := Z;
  Result.W := W;
end;

{$ENDREGION}
{$REGION 'TMatrix3f functions'}

class operator TMatrix3f.Add(const Matrix1, Matrix2: TMatrix3f): TMatrix3f;
var
  I, J: Integer;
begin
  for J := 0 to 2 do
    for I := 0 to 2 do
      Result.Data[J, I] := Matrix1.Data[J, I] + Matrix2.Data[J, I];
end;

class operator TMatrix3f.Subtract(const Matrix1, Matrix2: TMatrix3f): TMatrix3f;
var
  I, J: Integer;
begin
  for J := 0 to 2 do
    for I := 0 to 2 do
      Result.Data[J, I] := Matrix1.Data[J, I] - Matrix2.Data[J, I];
end;

class operator TMatrix3f.Multiply(const Matrix1, Matrix2: TMatrix3f): TMatrix3f;
var
  I, J: Integer;
begin
  for J := 0 to 2 do
    for I := 0 to 2 do
      Result.Data[J, I] := (Matrix1.Data[J, 0] * Matrix2.Data[0, I]) + (Matrix1.Data[J, 1] * Matrix2.Data[1, I]) +
        (Matrix1.Data[J, 2] * Matrix2.Data[2, I]);
end;

class operator TMatrix3f.Multiply(const Matrix: TMatrix3f; const Theta: VectorFloat): TMatrix3f;
var
  I, J: Integer;
begin
  for J := 0 to 2 do
    for I := 0 to 2 do
      Result.Data[J, I] := Matrix.Data[J, I] * Theta;
end;

class operator TMatrix3f.Multiply(const Theta: VectorFloat; const Matrix: TMatrix3f): TMatrix3f;
begin
  Result := Matrix * Theta;
end;

class operator TMatrix3f.Divide(const Matrix: TMatrix3f; const Theta: VectorFloat): TMatrix3f;
var
  I, J: Integer;
begin
  for J := 0 to 2 do
    for I := 0 to 2 do
      Result.Data[J, I] := Matrix.Data[J, I] / Theta;
end;

class operator TMatrix3f.Multiply(const Point: TPoint2f; const Matrix: TMatrix3f): TPoint2f;
begin
  Result.X := (Point.X * Matrix.Data[0, 0]) + (Point.Y * Matrix.Data[1, 0]) + Matrix.Data[2, 0];
  Result.Y := (Point.X * Matrix.Data[0, 1]) + (Point.Y * Matrix.Data[1, 1]) + Matrix.Data[2, 1];
end;

function TMatrix3f.Determinant: VectorFloat;
begin
  Result := Self.Data[0, 0] * (Self.Data[1, 1] * Self.Data[2, 2] - Self.Data[2, 1] * Self.Data[1, 2]) -
    Self.Data[0, 1] * (Self.Data[1, 0] * Self.Data[2, 2] - Self.Data[2, 0] * Self.Data[1, 2]) + Self.Data[0, 2] *
    (Self.Data[1, 0] * Self.Data[2, 1] - Self.Data[2, 0] * Self.Data[1, 1]);
end;

function TMatrix3f.Transpose: TMatrix3f;
var
  I, J: Integer;
begin
  for I := 0 to 2 do
    for J := 0 to 2 do
      Result.Data[I, J] := Data[J, I];
end;

function TMatrix3f.Adjoint: TMatrix3f;
begin
  Result.Data[0, 0] := Data[1, 1] * Data[2, 2] - Data[2, 1] * Data[1, 2];
  Result.Data[0, 1] := Data[2, 1] * Data[0, 2] - Data[0, 1] * Data[2, 2];
  Result.Data[0, 2] := Data[0, 1] * Data[1, 2] - Data[1, 1] * Data[0, 2];
  Result.Data[1, 0] := Data[2, 0] * Data[1, 2] - Data[1, 0] * Data[2, 2];
  Result.Data[1, 1] := Data[0, 0] * Data[2, 2] - Data[2, 0] * Data[0, 2];
  Result.Data[1, 2] := Data[1, 0] * Data[0, 2] - Data[0, 0] * Data[1, 2];
  Result.Data[2, 0] := Data[1, 0] * Data[2, 1] - Data[2, 0] * Data[1, 1];
  Result.Data[2, 1] := Data[2, 0] * Data[0, 1] - Data[0, 0] * Data[2, 1];
  Result.Data[2, 2] := Data[0, 0] * Data[1, 1] - Data[1, 0] * Data[0, 1];
end;

function TMatrix3f.Inverse: TMatrix3f;
var
  Det: VectorFloat;
begin
  Det := Determinant;
  if Abs(Det) > VectorEpsilon then
    Result := Adjoint * (1.0 / Det)
  else
    Result := IdentityMatrix3f;
end;

class function TMatrix3f.Translate(const Offset: TPoint2f): TMatrix3f;
begin
  Result := IdentityMatrix3f;
  Result.Data[2, 0] := Offset.X;
  Result.Data[2, 1] := Offset.Y;
end;

class function TMatrix3f.Translate(const X, Y: VectorFloat): TMatrix3f;
begin
  Result := Translate(Point2f(X, Y));
end;

class function TMatrix3f.Rotate(const Angle: VectorFLoat): TMatrix3f;
var
  SinValue, CosValue: VectorFloat;
begin
  SinCos(Angle, SinValue, CosValue);

  Result := IdentityMatrix3f;
  Result.Data[0, 0] := CosValue;
  Result.Data[0, 1] := SinValue;
  Result.Data[1, 0] := -Result.Data[0, 1];
  Result.Data[1, 1] := Result.Data[0, 0];
end;

class function TMatrix3f.Scale(const Scale: TPoint2f): TMatrix3f;
begin
  Result := IdentityMatrix3f;
  Result.Data[0, 0] := Scale.X;
  Result.Data[1, 1] := Scale.Y;
end;

class function TMatrix3f.Scale(const X, Y: VectorFloat): TMatrix3f;
begin
  Result := Scale(Point2f(X, Y));
end;

class function TMatrix3f.Scale(const Scale: VectorFloat): TMatrix3f;
begin
  Result := TMatrix3f.Scale(Point2f(Scale, Scale));
end;

{$ENDREGION}
{$REGION 'TMatrix4f functions'}

class operator TMatrix4f.Add(const Matrix1, Matrix2: TMatrix4f): TMatrix4f;
var
  I, J: Integer;
begin
  for J := 0 to 3 do
    for I := 0 to 3 do
      Result.Data[J, I] := Matrix1.Data[J, I] + Matrix2.Data[J, I];
end;

class operator TMatrix4f.Subtract(const Matrix1, Matrix2: TMatrix4f): TMatrix4f;
var
  I, J: Integer;
begin
  for J := 0 to 3 do
    for I := 0 to 3 do
      Result.Data[J, I] := Matrix1.Data[J, I] - Matrix2.Data[J, I];
end;

class operator TMatrix4f.Multiply(const Matrix1, Matrix2: TMatrix4f): TMatrix4f;
var
  I, J: Integer;
begin
  for J := 0 to 3 do
    for I := 0 to 3 do
      Result.Data[J, I] := (Matrix1.Data[J, 0] * Matrix2.Data[0, I]) + (Matrix1.Data[J, 1] * Matrix2.Data[1, I]) +
        (Matrix1.Data[J, 2] * Matrix2.Data[2, I]) + (Matrix1.Data[J, 3] * Matrix2.Data[3, I]);
end;

class operator TMatrix4f.Multiply(const Matrix: TMatrix4f; const Theta: VectorFloat): TMatrix4f;
var
  I, J: Integer;
begin
  for J := 0 to 3 do
    for I := 0 to 3 do
      Result.Data[J, I] := Matrix.Data[J, I] * Theta;
end;

class operator TMatrix4f.Multiply(const Theta: VectorFloat; const Matrix: TMatrix4f): TMatrix4f;
begin
  Result := Matrix * Theta;
end;

class operator TMatrix4f.Divide(const Matrix: TMatrix4f; const Theta: VectorFloat): TMatrix4f;
var
  I, J: Integer;
begin
  for J := 0 to 3 do
    for I := 0 to 3 do
      Result.Data[J, I] := Matrix.Data[J, I] / Theta;
end;

class operator TMatrix4f.Multiply(const Vector: TVector3f; const Matrix: TMatrix4f): TVector3f;
begin
  Result.X := Vector.X * Matrix.Data[0, 0] + Vector.Y * Matrix.Data[1, 0] + Vector.Z * Matrix.Data[2, 0] +
    Matrix.Data[3, 0];
  Result.Y := Vector.X * Matrix.Data[0, 1] + Vector.Y * Matrix.Data[1, 1] + Vector.Z * Matrix.Data[2, 1] +
    Matrix.Data[3, 1];
  Result.Z := Vector.X * Matrix.Data[0, 2] + Vector.Y * Matrix.Data[1, 2] + Vector.Z * Matrix.Data[2, 2] +
    Matrix.Data[3, 2];
end;

class operator TMatrix4f.Multiply(const Vector: TVector4f; const Matrix: TMatrix4f): TVector4f;
begin
  Result.X := Vector.X * Matrix.Data[0, 0] + Vector.Y * Matrix.Data[1, 0] + Vector.Z * Matrix.Data[2, 0] + Vector.W *
    Matrix.Data[3, 0];
  Result.Y := Vector.X * Matrix.Data[0, 1] + Vector.Y * Matrix.Data[1, 1] + Vector.Z * Matrix.Data[2, 1] + Vector.W *
    Matrix.Data[3, 1];
  Result.Z:= Vector.X * Matrix.Data[0, 2] + Vector.Y * Matrix.Data[1, 2] + Vector.Z * Matrix.Data[2, 2] + Vector.W *
    Matrix.Data[3, 2];
  Result.W:= Vector.X * Matrix.Data[0, 3] + Vector.Y * Matrix.Data[1, 3] + Vector.Z * Matrix.Data[2, 3] + Vector.W *
    Matrix.Data[3, 3];
end;

class function TMatrix4f.DetSub3(const A1, A2, A3, B1, B2, B3, C1, C2, C3: VectorFloat): VectorFloat;
begin
  Result := A1 * (B2 * C3 - B3 * C2) - B1 * (A2 * C3 - A3 * C2) + C1 * (A2 * B3 - A3 * B2);
end;

function TMatrix4f.Determinant: VectorFloat;
begin
  Result := Self.Data[0, 0] * DetSub3(Self.Data[1, 1], Self.Data[2, 1], Self.Data[3, 1], Self.Data[1, 2],
    Self.Data[2, 2], Self.Data[3, 2], Self.Data[1, 3], Self.Data[2, 3], Self.Data[3, 3]) - Self.Data[0, 1] *
    DetSub3(Self.Data[1, 0], Self.Data[2, 0], Self.Data[3, 0], Self.Data[1, 2], Self.Data[2, 2], Self.Data[3, 2],
    Self.Data[1, 3], Self.Data[2, 3], Self.Data[3, 3]) + Self.Data[0, 2] * DetSub3(Self.Data[1, 0], Self.Data[2, 0],
    Self.Data[3, 0], Self.Data[1, 1], Self.Data[2, 1], Self.Data[3, 1], Self.Data[1, 3], Self.Data[2, 3],
    Self.Data[3, 3]) - Self.Data[0, 3] * DetSub3(Self.Data[1, 0], Self.Data[2, 0], Self.Data[3, 0], Self.Data[1, 1],
    Self.Data[2, 1], Self.Data[3, 1], Self.Data[1, 2], Self.Data[2, 2], Self.Data[3, 2]);
end;

function TMatrix4f.EyePos: TVector3f;
begin
  Result.X := -Self.Data[0, 0] * Self.Data[3, 0] - Self.Data[0, 1] * Self.Data[3, 1] - Self.Data[0, 2] *
    Self.Data[3, 2];
  Result.Y := -Self.Data[1, 0] * Self.Data[3, 0] - Self.Data[1, 1] * Self.Data[3, 1] - Self.Data[1, 2] *
    Self.Data[3, 2];
  Result.Z := -Self.Data[2, 0] * Self.Data[3, 0] - Self.Data[2, 1] * Self.Data[3, 1] - Self.Data[2, 2] *
    Self.Data[3, 2];
end;

function TMatrix4f.WorldPos: TVector3f;
begin
  Result.X := Self.Data[3, 0];
  Result.Y := Self.Data[3, 1];
  Result.Z := Self.Data[3, 2];
end;

function TMatrix4f.Transpose: TMatrix4f;
var
  I, J: Integer;
begin
  for I := 0 to 3 do
    for J := 0 to 3 do
      Result.Data[I, J] := Data[J, I];
end;

function TMatrix4f.Adjoint: TMatrix4f;
begin
  Result.Data[0, 0] := DetSub3(Data[1, 1], Data[2, 1], Data[3, 1], Data[1, 2], Data[2, 2], Data[3, 2],
    Data[1, 3], Data[2, 3], Data[3, 3]);
  Result.Data[1, 0] := -DetSub3(Data[1, 0], Data[2, 0], Data[3, 0], Data[1, 2], Data[2, 2], Data[3, 2],
    Data[1, 3], Data[2, 3], Data[3, 3]);
  Result.Data[2, 0] := DetSub3(Data[1, 0], Data[2, 0], Data[3, 0], Data[1, 1], Data[2, 1], Data[3, 1],
    Data[1, 3], Data[2, 3], Data[3, 3]);
  Result.Data[3, 0] := -DetSub3(Data[1, 0], Data[2, 0], Data[3, 0], Data[1, 1], Data[2, 1], Data[3, 1],
    Data[1, 2], Data[2, 2], Data[3, 2]);

  Result.Data[0, 1] := -DetSub3(Data[0, 1], Data[2, 1], Data[3, 1], Data[0, 2], Data[2, 2], Data[3, 2],
    Data[0, 3], Data[2, 3], Data[3, 3]);
  Result.Data[1, 1] := DetSub3(Data[0, 0], Data[2, 0], Data[3, 0], Data[0, 2], Data[2, 2], Data[3, 2],
    Data[0, 3], Data[2, 3], Data[3, 3]);
  Result.Data[2, 1] := -DetSub3(Data[0, 0], Data[2, 0], Data[3, 0], Data[0, 1], Data[2, 1], Data[3, 1],
    Data[0, 3], Data[2, 3], Data[3, 3]);
  Result.Data[3, 1] := DetSub3(Data[0, 0], Data[2, 0], Data[3, 0], Data[0, 1], Data[2, 1], Data[3, 1],
    Data[0, 2], Data[2, 2], Data[3, 2]);

  Result.Data[0, 2] := DetSub3(Data[0, 1], Data[1, 1], Data[3, 1], Data[0, 2], Data[1, 2], Data[3, 2],
    Data[0, 3], Data[1, 3], Data[3, 3]);
  Result.Data[1, 2] := -DetSub3(Data[0, 0], Data[1, 0], Data[3, 0], Data[0, 2], Data[1, 2], Data[3, 2],
    Data[0, 3], Data[1, 3], Data[3, 3]);
  Result.Data[2, 2] := DetSub3(Data[0, 0], Data[1, 0], Data[3, 0], Data[0, 1], Data[1, 1], Data[3, 1],
    Data[0, 3], Data[1, 3], Data[3, 3]);
  Result.Data[3, 2] := -DetSub3(Data[0, 0], Data[1, 0], Data[3, 0], Data[0, 1], Data[1, 1], Data[3, 1],
    Data[0, 2], Data[1, 2], Data[3, 2]);

  Result.Data[0, 3] := -DetSub3(Data[0, 1], Data[1, 1], Data[2, 1], Data[0, 2], Data[1, 2], Data[2, 2],
    Data[0, 3], Data[1, 3], Data[2, 3]);
  Result.Data[1, 3] := DetSub3(Data[0, 0], Data[1, 0], Data[2, 0], Data[0, 2], Data[1, 2], Data[2, 2],
    Data[0, 3], Data[1, 3], Data[2, 3]);
  Result.Data[2, 3] := -DetSub3(Data[0, 0], Data[1, 0], Data[2, 0], Data[0, 1], Data[1, 1], Data[2, 1],
    Data[0, 3], Data[1, 3], Data[2, 3]);
  Result.Data[3, 3] := DetSub3(Data[0, 0], Data[1, 0], Data[2, 0], Data[0, 1], Data[1, 1], Data[2, 1],
    Data[0, 2], Data[1, 2], Data[2, 2]);
end;

function TMatrix4f.Inverse: TMatrix4f;
var
  Det: VectorFloat;
begin
  Det := Determinant;
  if Abs(Det) > VectorEpsilon then
    Result := Adjoint * (1.0 / Det)
  else
    Result := IdentityMatrix4f;
end;

function TMatrix4f.Project(const Vector: TVector3f; const TargetSize: TPoint2f): TPoint2f;
var
  Last: VectorFloat;
begin
  Last := Vector.X * Data[0, 3] + Vector.Y * Data[1, 3] + Vector.Z * Data[2, 3] + Data[3, 3];

  if Abs(Last) > VectorEpsilon then
  begin
    Result.X := Vector.X * Data[0, 0] + Vector.Y * Data[1, 0] + Vector.Z * Data[2, 0] + Data[3, 0];
    Result.Y := Vector.X * Data[0, 1] + Vector.Y * Data[1, 1] + Vector.Z * Data[2, 1] + Data[3, 1];

    Result := Result / Last;

    Result.X := (Result.X * 0.5 + 0.5) * TargetSize.X;
    Result.Y := (0.5 - Result.Y * 0.5) * TargetSize.Y;
  end
  else
    Result := Vector.GetXY;
end;

class function TMatrix4f.Translate(const Offset: TVector3f): TMatrix4f;
begin
  Result := IdentityMatrix4f;
  Result.Data[3, 0] := Offset.X;
  Result.Data[3, 1] := Offset.Y;
  Result.Data[3, 2] := Offset.Z;
end;

class function TMatrix4f.Translate(const X, Y, Z: VectorFloat): TMatrix4f;
begin
  Result := Translate(Vector3f(X, Y, Z));
end;

class function TMatrix4f.RotateX(const Angle: VectorFloat): TMatrix4f;
var
  SinValue, CosValue: VectorFloat;
begin
  SinCos(Angle, SinValue, CosValue);

  Result := IdentityMatrix4f;
  Result.Data[1, 1] := CosValue;
  Result.Data[1, 2] := SinValue;
  Result.Data[2, 1] := -Result.Data[1, 2];
  Result.Data[2, 2] := Result.Data[1, 1];
end;

class function TMatrix4f.RotateY(const Angle: VectorFloat): TMatrix4f;
var
  SinValue, CosValue: VectorFloat;
begin
  SinCos(Angle, SinValue, CosValue);

  Result := IdentityMatrix4f;
  Result.Data[0, 0] := CosValue;
  Result.Data[0, 2] := -SinValue;
  Result.Data[2, 0] := -Result.Data[0, 2];
  Result.Data[2, 2] := Result.Data[0, 0];
end;

class function TMatrix4f.RotateZ(const Angle: VectorFloat): TMatrix4f;
var
  SinValue, CosValue: VectorFloat;
begin
  SinCos(Angle, SinValue, CosValue);

  Result := IdentityMatrix4f;
  Result.Data[0, 0] := CosValue;
  Result.Data[0, 1] := SinValue;
  Result.Data[1, 0] := -Result.Data[0, 1];
  Result.Data[1, 1] := Result.Data[0, 0];
end;

class function TMatrix4f.Rotate(const Axis: TVector3f; const Angle: VectorFloat): TMatrix4f;
var
  CosValue, InvCosValue, SinValue: VectorFloat;
  XYMul, XZMul, YZMul, XSin, YSin, ZSin: VectorFloat;
begin
  SinCos(Angle, SinValue, CosValue);
  InvCosValue := 1.0 - CosValue;

  XYMul := Axis.X * Axis.Y * InvCosValue;
  XZMul := Axis.X * Axis.Z * InvCosValue;
  YZMul := Axis.Y * Axis.Z * InvCosValue;

  XSin := Axis.X * SinValue;
  YSin := Axis.Y * SinValue;
  ZSin := Axis.Z * SinValue;

  Result := IdentityMatrix4f;
  Result.Data[0, 0] := Sqr(Axis.X) * InvCosValue + CosValue;
  Result.Data[0, 1] := XYMul + ZSin;
  Result.Data[0, 2] := XZMul - YSin;
  Result.Data[1, 0] := XYMul - ZSin;
  Result.Data[1, 1] := Sqr(Axis.Y) * InvCosValue + CosValue;
  Result.Data[1, 2] := YZMul + XSin;
  Result.Data[2, 0] := XZMul + YSin;
  Result.Data[2, 1] := YZMul - XSin;
  Result.Data[2, 2] := Sqr(Axis.Z) * InvCosValue + CosValue;
end;

class function TMatrix4f.Scale(const Scale: TVector3f): TMatrix4f;
begin
  Result := IdentityMatrix4f;
  Result.Data[0, 0] := Scale.X;
  Result.Data[1, 1] := Scale.Y;
  Result.Data[2, 2] := Scale.Z;
end;

class function TMatrix4f.Scale(const X, Y, Z: VectorFloat): TMatrix4f;
begin
  Result := Scale(Vector3f(X, Y, Z));
end;

class function TMatrix4f.Scale(const Scale: VectorFloat): TMatrix4f;
begin
  Result := TMatrix4f.Scale(Vector3f(Scale, Scale, Scale));
end;

class function TMatrix4f.HeadingPitchBank(const Heading, Pitch, Bank: VectorFloat): TMatrix4f;
var
  CosHeading, SinHeading, CosPitch, SinPitch, CosBank, SinBank: VectorFloat;
begin
  SinCos(Heading, SinHeading, CosHeading);
  SinCos(Pitch, SinPitch, CosPitch);
  SinCos(Bank, SinBank, CosBank);

  Result := IdentityMatrix4f;
  Result.Data[0, 0] := CosHeading * CosBank + SinHeading * SinPitch * SinBank;
  Result.Data[0, 1] := SinHeading * SinPitch * CosBank - CosHeading * SinBank;
  Result.Data[0, 2] := SinHeading * CosPitch;
  Result.Data[1, 0] := SinBank * CosPitch;
  Result.Data[1, 1] := CosBank * CosPitch;
  Result.Data[1, 2] := - SinPitch;
  Result.Data[2, 0] := CosHeading * SinPitch * SinBank - SinHeading * CosBank;
  Result.Data[2, 1] := SinBank * SinHeading + CosHeading * SinPitch * CosBank;
  Result.Data[2, 2] := CosHeading * CosPitch;
end;

class function TMatrix4f.HeadingPitchBank(const Vector: TVector3f): TMatrix4f;
begin
  Result := HeadingPitchBank(Vector.Y, Vector.X, Vector.Z);
end;

class function TMatrix4f.YawPitchRoll(const Yaw, Pitch, Roll: VectorFloat): TMatrix4f;
var
  SinYaw, CosYaw, SinPitch, CosPitch, SinRoll, CosRoll: VectorFloat;
begin
  SinCos(Yaw, SinYaw, CosYaw);
  SinCos(Pitch, SinPitch, CosPitch);
  SinCos(Roll, SinRoll, CosRoll);

  Result := IdentityMatrix4f;
  Result.Data[0, 0] := CosRoll * CosYaw + SinPitch * SinRoll * SinYaw;
  Result.Data[0, 1] := CosYaw * SinPitch * SinRoll - CosRoll * SinYaw;
  Result.Data[0, 2] := -CosPitch * SinRoll;
  Result.Data[1, 0] := CosPitch * SinYaw;
  Result.Data[1, 1] := CosPitch * CosYaw;
  Result.Data[1, 2] := SinPitch;
  Result.Data[2, 0] := CosYaw * SinRoll - CosRoll * SinPitch * SinYaw;
  Result.Data[2, 1] := -CosRoll * CosYaw * SinPitch - SinRoll * SinYaw;
  Result.Data[2, 2] := CosPitch * CosRoll;
end;

class function TMatrix4f.YawPitchRoll(const Vector: TVector3f): TMatrix4f;
begin
  Result := YawPitchRoll(Vector.Y, Vector.X, Vector.Z);
end;

class function TMatrix4f.Reflect(const Axis: TVector3f): TMatrix4f;
var
  XYMul, YZMul, XZMul: VectorFloat;
begin
  XYMul := -2.0 * Axis.X * Axis.Y;
  XZMul := -2.0 * Axis.X * Axis.Z;
  YZMul := -2.0 * Axis.Y * Axis.Z;

  Result := IdentityMatrix4f;
  Result.Data[0, 0] := 1.0 - (2.0 * Sqr(Axis.X));
  Result.Data[0, 1] := XYMul;
  Result.Data[0, 2] := XZMul;
  Result.Data[1, 0] := XYMul;
  Result.Data[1, 1] := 1.0 - (2.0 * Sqr(Axis.Y));
  Result.Data[1, 2] := YZMul;
  Result.Data[2, 0] := XZMul;
  Result.Data[2, 1] := YZMul;
  Result.Data[2, 2] := 1.0 - (2.0 * Sqr(Axis.Z));
end;

class function TMatrix4f.LookAt(const Origin, Target, Ceiling: TVector3f): TMatrix4f;
var
  XAxis, YAxis, ZAxis: TVector3f;
begin
  ZAxis := (Target - Origin).Normalize;
  XAxis := Ceiling.Cross(ZAxis).Normalize;
  YAxis := ZAxis.Cross(XAxis);

  Result.Data[0, 0] := XAxis.X;
  Result.Data[0, 1] := YAxis.X;
  Result.Data[0, 2] := ZAxis.X;
  Result.Data[0, 3] := 0.0;

  Result.Data[1, 0] := XAxis.Y;
  Result.Data[1, 1] := YAxis.Y;
  Result.Data[1, 2] := ZAxis.Y;
  Result.Data[1, 3] := 0.0;

  Result.Data[2, 0] := XAxis.Z;
  Result.Data[2, 1] := YAxis.Z;
  Result.Data[2, 2] := ZAxis.Z;
  Result.Data[2, 3] := 0.0;

  Result.Data[3, 0] := -XAxis.Dot(Origin);
  Result.Data[3, 1] := -YAxis.Dot(Origin);
  Result.Data[3, 2] := -ZAxis.Dot(Origin);
  Result.Data[3, 3] := 1.0;
end;

class function TMatrix4f.PerspectiveFOVY(const FieldOfView, AspectRatio, MinRange,
  MaxRange: VectorFloat): TMatrix4f;
var
  XScale, YScale, ZCoef: VectorFloat;
begin
  Result := ZeroMatrix4f;

  YScale := Cot(FieldOfView * 0.5);
  XScale := YScale / AspectRatio;
  ZCoef := MaxRange / (MaxRange - MinRange);

  Result.Data[0, 0] := XScale;
  Result.Data[1, 1] := YScale;
  Result.Data[2, 2] := ZCoef;
  Result.Data[2, 3] := 1.0;
  Result.Data[3, 2] := -MinRange * ZCoef;
end;

class function TMatrix4f.PerspectiveFOVX(const FieldOfView, AspectRatio, MinRange,
  MaxRange: VectorFloat): TMatrix4f;
var
  XScale, YScale, ZCoef: VectorFloat;
begin
  Result := ZeroMatrix4f;

  XScale := Cot(FieldOfView * 0.5);
  YScale := XScale / AspectRatio;
  ZCoef := MaxRange / (MaxRange - MinRange);

  Result.Data[0, 0] := XScale;
  Result.Data[1, 1] := YScale;
  Result.Data[2, 2] := ZCoef;
  Result.Data[2, 3] := 1.0;
  Result.Data[3, 2] := -MinRange * ZCoef;
end;

class function TMatrix4f.PerspectiveVOL(const Width, Height, MinRange, MaxRange: VectorFloat): TMatrix4f;
begin
  Result := ZeroMatrix4f;

  Result.Data[0, 0] := (2.0 * MinRange) / Width;
  Result.Data[1, 1] := (2.0 * MinRange) / Height;
  Result.Data[2, 2] := MaxRange / (MaxRange - MinRange);
  Result.Data[2, 3] := 1.0;
  Result.Data[3, 2] := MinRange * (MinRange - MaxRange);
end;

class function TMatrix4f.PerspectiveBDS(const Left, Right, Top, Bottom, MinRange,
  MaxRange: VectorFloat): TMatrix4f;
begin
  Result := ZeroMatrix4f;

  Result.Data[0, 0] := (2.0 * MinRange) / (Right - Left);
  Result.Data[1, 1] := (2.0 * MinRange) / (Top - Bottom);

  Result.Data[2, 0] := (Left + Right) / (Left - Right);
  Result.Data[2, 1] := (Top + Bottom) / (Bottom - Top);
  Result.Data[2, 2] := MaxRange / (MaxRange - MinRange);
  Result.Data[2, 3] := 1.0;
  Result.Data[3, 2] := MinRange * MaxRange / (MinRange - MaxRange);
end;

class function TMatrix4f.OrthogonalVOL(const Width, Height, MinRange, MaxRange: VectorFloat): TMatrix4f;
begin
  Result := ZeroMatrix4f;

  Result.Data[0, 0] := 2.0 / Width;
  Result.Data[1, 1] := 2.0 / Height;
  Result.Data[2, 2] := 1.0 / (MaxRange - MinRange);
  Result.Data[2, 3] := MinRange / (MinRange - MaxRange);
  Result.Data[3, 3] := 1.0;
end;

class function TMatrix4f.OrthogonalBDS(const Left, Right, Top, Bottom, MinRange,
  MaxRange: VectorFloat): TMatrix4f;
begin
  Result := ZeroMatrix4f;

  Result.Data[0, 0] := 2.0 / (Right - Left);
  Result.Data[1, 1] := 2.0 / (Top - Bottom);
  Result.Data[2, 2] := 1.0 / (MaxRange - MinRange);
  Result.Data[2, 3] := MinRange / (MinRange - MaxRange);
  Result.Data[3, 0] := (Left + Right) / (Left - Right);
  Result.Data[3, 1] := (Top + Bottom) / (Bottom - Top);
  Result.Data[3, 2] := MinRange / (MinRange - MaxRange);
  Result.Data[3, 3] := 1.0;
end;

{$ENDREGION}
{$REGION 'TQuaternion functions'}

class operator TQuaternion.Multiply(const Quaternion1, Quaternion2: TQuaternion): TQuaternion;
begin
  Result.X := Quaternion2.W * Quaternion1.X + Quaternion2.X * Quaternion1.W + Quaternion2.Z * Quaternion1.Y -
    Quaternion2.Y * Quaternion1.Z;
  Result.Y := Quaternion2.W * Quaternion1.Y + Quaternion2.Y * Quaternion1.W + Quaternion2.X * Quaternion1.Z -
    Quaternion2.Z * Quaternion1.X;
  Result.Z := Quaternion2.W * Quaternion1.Z + Quaternion2.Z * Quaternion1.W + Quaternion2.Y * Quaternion1.X -
    Quaternion2.X * Quaternion1.Y;
  Result.W := Quaternion2.W * Quaternion1.W - Quaternion2.X * Quaternion1.X - Quaternion2.Y * Quaternion1.Y -
    Quaternion2.Z * Quaternion1.Z;
end;

class operator TQuaternion.Implicit(const Quaternion: TQuaternion): TMatrix4f;
begin
  Result.Data[0, 0] := 1.0 - 2.0 * Quaternion.Y * Quaternion.Y - 2.0 * Quaternion.Z * Quaternion.Z;
  Result.Data[0, 1] := 2.0 * Quaternion.X * Quaternion.Y + 2.0 * Quaternion.W * Quaternion.Z;
  Result.Data[0, 2] := 2.0 * Quaternion.X * Quaternion.Z - 2.0 * Quaternion.W * Quaternion.Y;
  Result.Data[0, 3] := 0.0;
  Result.Data[1, 0] := 2.0 * Quaternion.X * Quaternion.Y - 2.0 * Quaternion.W * Quaternion.Z;
  Result.Data[1, 1] := 1.0 - 2.0 * Quaternion.X * Quaternion.X - 2 * Quaternion.Z * Quaternion.Z;
  Result.Data[1, 2] := 2.0 * Quaternion.Y * Quaternion.Z + 2.0 * Quaternion.W * Quaternion.X;
  Result.Data[1, 3] := 0.0;
  Result.Data[2, 0] := 2.0 * Quaternion.X * Quaternion.Z + 2.0 * Quaternion.W * Quaternion.Y;
  Result.Data[2, 1] := 2.0 * Quaternion.Y * Quaternion.Z - 2.0 * Quaternion.W * Quaternion.X;
  Result.Data[2, 2] := 1.0 - 2.0 * Quaternion.X * Quaternion.X - 2.0 * Quaternion.Y * Quaternion.Y;
  Result.Data[2, 3] := 0.0;
  Result.Data[3, 0] := 0.0;
  Result.Data[3, 1] := 0.0;
  Result.Data[3, 2] := 0.0;
  Result.Data[3, 3] := 1.0;
end;

class operator TQuaternion.Explicit(const Matrix: TMatrix4f): TQuaternion;
var
  MaxValue, HighValue, MultValue: VectorFloat;
  TempQuat: TQuaternion;
  Index: Integer;
begin
  // Determine wich of W, X, Y, Z has the largest absolute value.
  TempQuat.X := Matrix.Data[0, 0] - Matrix.Data[1, 1] - Matrix.Data[2, 2];
  TempQuat.Y := Matrix.Data[1, 1] - Matrix.Data[0, 0] - Matrix.Data[2, 2];
  TempQuat.Z := Matrix.Data[2, 2] - Matrix.Data[0, 0] - Matrix.Data[1, 1];
  TempQuat.W := Matrix.Data[0, 0] + Matrix.Data[1, 1] + Matrix.Data[2, 2];

  Index := 0;
  MaxValue := TempQuat.W;
  if TempQuat.X > MaxValue then
  begin
    MaxValue := TempQuat.X;
    Index := 1;
  end;
  if TempQuat.Y > MaxValue then
  begin
    MaxValue := TempQuat.Y;
    Index := 2;
  end;
  if TempQuat.Z > MaxValue then
  begin
    MaxValue := TempQuat.Z;
    Index := 3;
  end;

  // Perform square root and division.
  HighValue := Sqrt(MaxValue + 1.0) * 0.5;
  MultValue := 0.25 / HighValue;

  // Apply table to compute quaternion values.
  case Index of
    0:
      begin
        Result.W := HighValue;
        Result.X := (Matrix.Data[1, 2] - Matrix.Data[2, 1]) * MultValue;
        Result.Y := (Matrix.Data[2, 0] - Matrix.Data[0, 2]) * MultValue;
        Result.Z := (Matrix.Data[0, 1] - Matrix.Data[1, 0]) * MultValue;
      end;
    1:
      begin
        Result.X := HighValue;
        Result.W := (Matrix.Data[1, 2] - Matrix.Data[2, 1]) * MultValue;
        Result.Z := (Matrix.Data[2, 0] + Matrix.Data[0, 2]) * MultValue;
        Result.Y := (Matrix.Data[0, 1] + Matrix.Data[1, 0]) * MultValue;
      end;
    2:
      begin
        Result.Y := HighValue;
        Result.Z := (Matrix.Data[1, 2] + Matrix.Data[2, 1]) * MultValue;
        Result.W := (Matrix.Data[2, 0] - Matrix.Data[0, 2]) * MultValue;
        Result.X := (Matrix.Data[0, 1] + Matrix.Data[1, 0]) * MultValue;
      end;
  else
    begin
      Result.Z := HighValue;
      Result.Y := (Matrix.Data[1, 2] + Matrix.Data[2, 1]) * MultValue;
      Result.X := (Matrix.Data[2, 0] + Matrix.Data[0, 2]) * MultValue;
      Result.W := (Matrix.Data[0, 1] - Matrix.Data[1, 0]) * MultValue;
    end;
  end;
end;

function TQuaternion.Length: VectorFloat;
begin
  Result := Sqrt((X * X) + (Y * Y) + (Z * Z) + (W * W));
end;

function TQuaternion.Normalize: TQuaternion;
var
  Amp, InvMag: VectorFloat;
begin
  Amp := Length;
  if Amp > VectorEpsilon then
  begin
    InvMag := 1.0 / Amp;
    Result.X := X * InvMag;
    Result.Y := Y * InvMag;
    Result.Z := Z * InvMag;
    Result.W := W * InvMag;
  end
  else
    Result := Self;
end;

function TQuaternion.Angle: VectorFloat;
begin
  Result := ArcCos(W) * 2.0;
end;

function TQuaternion.Axis: TVector3f;
var
  Temp1, Temp2: VectorFloat;
begin
  Temp1 := 1.0 - (W * W);
  if Temp1 <= 0.0 then
    Exit(AxisYVector3f);

  Temp2 := 1.0 / Sqrt(Temp1);

  Result.X := X * Temp2;
  Result.Y := Y * Temp2;
  Result.Z := Z * Temp2;
end;

function TQuaternion.Conjugate: TQuaternion;
begin
  Result.X := -X;
  Result.Y := -Y;
  Result.Z := -Z;
  Result.W := W;
end;

function TQuaternion.Exponentiate(const Exponent: VectorFloat): TQuaternion;
var
  Alpha, NewAlpha, CosNewAlpha, SinNewAlpha, CompMult: VectorFloat;
begin
  if Abs(W) > 1.0 - VectorEpsilon then
    Exit(Self);

  Alpha := ArcCos(W);
  NewAlpha := Alpha * Exponent;

  SinCos(NewAlpha, SinNewAlpha, CosNewAlpha);

  CompMult := SinNewAlpha / Sin(Alpha);

  Result.X := X * CompMult;
  Result.Y := Y * CompMult;
  Result.Z := Z * CompMult;
  Result.W := CosNewAlpha;
end;

function TQuaternion.Dot(const Quaternion: TQuaternion): VectorFloat;
begin
  Result := (X * Quaternion.X) + (Y * Quaternion.Y) + (Z * Quaternion.Z) + (W * Quaternion.W);
end;

function TQuaternion.Slerp(const Quaternion: TQuaternion; const Theta: VectorFloat): TQuaternion;
var
  TempQuat: TQuaternion;
  SinOmega, CosOmega, Omega, Kappa1, Kappa2: VectorFloat;
  OneOverSinOmega: VectorFloat;
begin
  if Theta <= 0.0 then
    Exit(Self)
  else if Theta >= 1.0 then
    Exit(Quaternion);

  CosOmega := Dot(Quaternion);
  TempQuat := Self;

  if CosOmega < 0.0 then
  begin
    TempQuat.X := -TempQuat.X;
    TempQuat.Y := -TempQuat.Y;
    TempQuat.Z := -TempQuat.Z;
    TempQuat.W := -TempQuat.W;
    CosOmega := -CosOmega;
  end;

  if CosOmega > 1.0 - VectorEpsilon then
  begin
    Kappa1 := 1.0 - Theta;
    Kappa2 := Theta;
  end
  else
  begin
    SinOmega := Sqrt(1.0 - CosOmega * CosOmega);
    Omega := ArcTan2(SinOmega, CosOmega);

    OneOverSinOmega := 1.0 / SinOmega;

    Kappa1 := Sin((1.0 - Theta) * Omega) * OneOverSinOmega;
    Kappa2 := Sin(Theta * Omega) * OneOverSinOmega;
  end;

  Result.Z := Kappa1 * Z + Kappa2 * TempQuat.X;
  Result.Y := Kappa1 * Y + Kappa2 * TempQuat.Y;
  Result.Z := Kappa1 * Z + Kappa2 * TempQuat.Z;
  Result.W := Kappa1 * W + Kappa2 * TempQuat.W;
end;

class function TQuaternion.RotateX(const Angle: VectorFloat): TQuaternion;
var
  SinValue, CosValue: VectorFloat;
begin
  SinCos(Angle * 0.5, SinValue, CosValue);

  Result.X := SinValue;
  Result.Y := 0.0;
  Result.Z := 0.0;
  Result.W := CosValue;
end;

class function TQuaternion.RotateY(const Angle: VectorFloat): TQuaternion;
var
  SinValue, CosValue: VectorFloat;
begin
  SinCos(Angle * 0.5, SinValue, CosValue);

  Result.X := 0.0;
  Result.Y := SinValue;
  Result.Z := 0.0;
  Result.W := CosValue;
end;

class function TQuaternion.RotateZ(const Angle: VectorFloat): TQuaternion;
var
  SinValue, CosValue: VectorFloat;
begin
  SinCos(Angle * 0.5, SinValue, CosValue);

  Result.X := 0.0;
  Result.Y := 0.0;
  Result.Z := SinValue;
  Result.W := CosValue;
end;

class function TQuaternion.Rotate(const Axis: TVector3f; const Angle: VectorFloat): TQuaternion;
var
  Amp, SinValue, CosValue, SinDivAmp: VectorFloat;
begin
  Amp := Axis.Length;
  if Amp > VectorEpsilon then
  begin
    SinCos(Angle * 0.5, SinValue, CosValue);
    SinDivAmp := SinValue / Amp;

    Result.X := Axis.X * SinDivAmp;
    Result.Y := Axis.Y * SinDivAmp;
    Result.Z := Axis.Z * SinDivAmp;
    Result.W := CosValue;
  end
  else
    Result := IdentityQuaternion;
end;

class function TQuaternion.RotateObjectToIntertial(const Heading, Pitch, Bank: VectorFloat): TQuaternion;
var
  SinPitch, SinBank, SinHeading, CosPitch, CosBank, CosHeading: VectorFloat;
begin
  SinCos(Heading * 0.5, SinHeading, CosHeading);
  SinCos(Pitch * 0.5, SinPitch, CosPitch);
  SinCos(Bank * 0.5, SinBank, CosBank);

  Result.X := CosHeading * SinPitch * CosBank + SinHeading * CosPitch * SinBank;
  Result.Y := -CosHeading * SinPitch * SinBank + SinHeading * CosPitch * CosBank;
  Result.Z := -SinHeading * SinPitch * CosBank + CosHeading * CosPitch * SinBank;
  Result.W := CosHeading * CosPitch * CosBank + SinHeading * SinPitch * SinBank;
end;

class function TQuaternion.RotateInertialToObject(const Heading, Pitch, Bank: VectorFloat): TQuaternion;
var
  SinPitch, SinBank, SinHeading, CosPitch, CosBank, CosHeading: VectorFloat;
begin
  SinCos(Heading * 0.5, SinHeading, CosHeading);
  SinCos(Pitch * 0.5, SinPitch, CosPitch);
  SinCos(Bank * 0.5, SinBank, CosBank);

  Result.X := -CosHeading * SinPitch * CosBank - SinHeading * CosPitch * SinBank;
  Result.Y := CosHeading * SinPitch * SinBank - SinHeading * CosPitch * CosBank;
  Result.Z := SinHeading * SinPitch * CosBank - CosHeading * CosPitch * SinBank;
  Result.W := CosHeading * CosPitch * CosBank + SinHeading * SinPitch * SinBank;
end;

{$ENDREGION}
{$REGION 'TIntRect'}

class operator TIntRect.Equal(const Rect1, Rect2: TIntRect): Boolean;
begin
  Result := (Rect1.TopLeft = Rect2.TopLeft) and (Rect1.Size = Rect2.Size);
end;

class operator TIntRect.NotEqual(const Rect1, Rect2: TIntRect): Boolean;
begin
  Result := not (Rect1 = Rect2);
end;

function TIntRect.GetRight: VectorInt;
begin
  Result := Left + Width;
end;

procedure TIntRect.SetRight(const Value: VectorInt);
begin
  Width := Value - Left;
end;

function TIntRect.GetBottom: VectorInt;
begin
  Result := Top + Height;
end;

procedure TIntRect.SetBottom(const Value: VectorInt);
begin
  Height := Value - Top;
end;

function TIntRect.GetBottomRight: TPoint2i;
begin
  Result.X := GetRight;
  Result.Y := GetBottom;
end;

procedure TIntRect.SetBottomRight(const Value: TPoint2i);
begin
  SetRight(Value.X);
  SetBottom(Value.Y);
end;

function TIntRect.Empty: Boolean;
begin
  Result := (Width <= 0) or (Height <= 0);
end;

function TIntRect.Contains(const Point: TPoint2i): Boolean;
begin
  Result := (Point.X >= Left) and (Point.X < Right) and (Point.Y >= Top) and (Point.Y < Bottom);
end;

function TIntRect.Contains(const Rect: TIntRect): Boolean;
begin
  Result := (Rect.Left >= Left) and (Rect.Right <= Right) and (Rect.Top >= Top) and (Rect.Bottom <= Bottom);
end;

function TIntRect.Overlaps(const Rect: TIntRect): Boolean;
begin
  Result := (Rect.Left < Right) and (Rect.Right > Left) and (Rect.Top < Bottom) and (Rect.Bottom > Top);
end;

function TIntRect.Intersect(const Rect: TIntRect): TIntRect;
begin
  if Overlaps(Rect) then
    Result := IntRectBDS(
      Max(Left, Rect.Left), Max(Top, Rect.Top),
      Min(Right, Rect.Right), Min(Bottom, Rect.Bottom))
  else
    Result := ZeroIntRect;
end;

function TIntRect.Union(const Rect: TIntRect): TIntRect;
begin
  Result := IntRectBDS(
    Min(Left, Rect.Left), Min(Top, Rect.Top),
    Max(Right, Rect.Right), Max(Bottom, Rect.Bottom));
end;

function TIntRect.Offset(const Delta: TPoint2i): TIntRect;
begin
  Result := IntRectBDS(TopLeft + Delta, BottomRight + Delta);
end;

function TIntRect.Offset(const DeltaX, DeltaY: VectorInt): TIntRect;
begin
  Result := Offset(Point2i(DeltaX, DeltaY));
end;

function TIntRect.Inflate(const Delta: TPoint2i): TIntRect;
begin
  Result := IntRectBDS(TopLeft - Delta, BottomRight + Delta);
end;

function TIntRect.Inflate(const DeltaX, DeltaY: VectorInt): TIntRect;
begin
  Result := Inflate(Point2i(DeltaX, DeltaY));
end;

class function TIntRect.ClipCoords(const SourceSize, DestSize: TPoint2i; var SourceRect: TIntRect;
  var DestPos: TPoint2i): Boolean;
var
  Delta: VectorInt;
begin
  if SourceRect.Left < 0 then
  begin
    Delta := -SourceRect.Left;
    Inc(SourceRect.Left, Delta);
    Inc(DestPos.X, Delta);
  end;

  if SourceRect.Top < 0 then
  begin
    Delta := -SourceRect.Top;
    Inc(SourceRect.Top, Delta);
    Inc(DestPos.Y, Delta);
  end;

  if SourceRect.Right > SourceSize.X then
    SourceRect.Right := SourceSize.X;

  if SourceRect.Bottom > SourceSize.Y then
    SourceRect.Bottom := SourceSize.Y;

  if DestPos.X < 0 then
  begin
    Delta := -DestPos.X;
    Inc(DestPos.X, Delta);
    Inc(SourceRect.Left, Delta);
  end;

  if DestPos.Y < 0 then
  begin
    Delta := -DestPos.Y;
    Inc(DestPos.Y, Delta);
    Inc(SourceRect.Top, Delta);
  end;

  if DestPos.X + SourceRect.Width > DestSize.X then
  begin
    Delta := DestPos.X + SourceRect.Width - DestSize.X;
    Dec(SourceRect.Width, Delta);
  end;

  if DestPos.Y + SourceRect.Height > DestSize.Y then
  begin
    Delta := DestPos.Y + SourceRect.Height - DestSize.Y;
    Dec(SourceRect.Height, Delta);
  end;

  Result := not SourceRect.Empty;
end;

function IntRect(const Left, Top, Width, Height: VectorInt): TIntRect;
begin
  Result.Left := Left;
  Result.Top := Top;
  Result.Width := Width;
  Result.Height := Height;
end;

function IntRect(const Origin, Size: TPoint2i): TIntRect;
begin
  Result.TopLeft := Origin;
  Result.Size := Size;
end;

function IntRectBDS(const Left, Top, Right, Bottom: VectorInt): TIntRect;
begin
  Result.Left := Left;
  Result.Top := Top;
  Result.Right := Right;
  Result.Bottom := Bottom;
end;

function IntRectBDS(const TopLeft, BottomRight: TPoint2i): TIntRect;
begin
  Result.TopLeft := TopLeft;
  Result.BottomRight := BottomRight;
end;

{$ENDREGION}
{$REGION 'TFloatRect'}

class operator TFloatRect.Equal(const Rect1, Rect2: TFloatRect): Boolean;
begin
  Result := (Rect1.TopLeft = Rect2.TopLeft) and (Rect1.BottomRight = Rect2.BottomRight);
end;

class operator TFloatRect.NotEqual(const Rect1, Rect2: TFloatRect): Boolean;
begin
  Result := not (Rect1 = Rect2);
end;

function TFloatRect.GetRight: VectorFloat;
begin
  Result := Left + Width;
end;

procedure TFloatRect.SetRight(const Value: VectorFloat);
begin
  Width := Value - Left;
end;

function TFloatRect.GetBottom: VectorFloat;
begin
  Result := Top + Height;
end;

procedure TFloatRect.SetBottom(const Value: VectorFloat);
begin
  Height := Value - Top;
end;

function TFloatRect.GetBottomRight: TPoint2f;
begin
  Result.X := GetRight;
  Result.Y := GetBottom;
end;

procedure TFloatRect.SetBottomRight(const Value: TPoint2f);
begin
  SetRight(Value.X);
  SetBottom(Value.Y);
end;

function TFloatRect.Empty: Boolean;
begin
  Result := (Width < VectorEpsilon) or (Height < VectorEpsilon);
end;

function TFloatRect.Contains(const Point: TPoint2f): Boolean;
begin
  Result := (Point.X >= Left) and (Point.X < Right) and (Point.Y >= Top) and (Point.Y < Bottom);
end;

function TFloatRect.Contains(const Rect: TFloatRect): Boolean;
begin
  Result := (Rect.Left >= Left) and (Rect.Right <= Right) and (Rect.Top >= Top) and (Rect.Bottom <= Bottom);
end;

function TFloatRect.Overlaps(const Rect: TFloatRect): Boolean;
begin
  Result := (Rect.Left < Right) and (Rect.Right > Left) and (Rect.Top < Bottom) and (Rect.Bottom > Top);
end;

function TFloatRect.Intersect(const Rect: TFloatRect): TFloatRect;
begin
  if Overlaps(Rect) then
    Result := FloatRectBDS(
      Max(Left, Rect.Left), Max(Top, Rect.Top),
      Min(Right, Rect.Right), Min(Bottom, Rect.Bottom))
  else
    Result := ZeroFloatRect;
end;

function TFloatRect.Union(const Rect: TFloatRect): TFloatRect;
begin
  Result := FloatRectBDS(
    Min(Left, Rect.Left), Min(Top, Rect.Top),
    Max(Right, Rect.Right), Max(Bottom, Rect.Bottom));
end;

function TFloatRect.Offset(const Delta: TPoint2f): TFloatRect;
begin
  Result := FloatRectBDS(TopLeft + Delta, BottomRight + Delta);
end;

function TFloatRect.Offset(const DeltaX, DeltaY: VectorFloat): TFloatRect;
begin
  Result := Offset(Point2f(DeltaX, DeltaY));
end;

function TFloatRect.Inflate(const Delta: TPoint2f): TFloatRect;
begin
  Result := FloatRectBDS(TopLeft - Delta, BottomRight + Delta);
end;

function TFloatRect.Inflate(const DeltaX, DeltaY: VectorFloat): TFloatRect;
begin
  Result := Inflate(Point2f(DeltaX, DeltaY));
end;

function TFloatRect.ToInt: TIntRect;
begin
  Result.Left := Round(Left);
  Result.Top := Round(Top);
  Result.Width := Round(Width);
  Result.Height := Round(Height);
end;

class function TFloatRect.ClipCoords(const SourceSize, DestSize: TPoint2f; var SourceRect,
  DestRect: TFloatRect): Boolean;
var
  Delta: VectorFloat;
  Scale: TPoint2f;
begin
  if SourceRect.Empty or DestRect.Empty then
    Exit(False);

  Scale.X := DestRect.Width / SourceRect.Width;
  Scale.Y := DestRect.Height / SourceRect.Height;

  if SourceRect.Left < 0 then
  begin
    Delta := -SourceRect.Left;
    SourceRect.Left := SourceRect.Left + Delta;
    DestRect.Left := DestRect.Left + (Delta * Scale.X);
  end;

  if SourceRect.Top < 0 then
  begin
    Delta := -SourceRect.Top;
    SourceRect.Top := SourceRect.Top + Delta;
    DestRect.Top := DestRect.Top + (Delta * Scale.Y);
  end;

  if SourceRect.Right > SourceSize.X then
  begin
    Delta := SourceRect.Right - SourceSize.X;
    SourceRect.Right := SourceRect.Right - Delta;
    DestRect.Right := DestRect.Right - (Delta * Scale.X);
  end;

  if SourceRect.Bottom > SourceSize.Y then
  begin
    Delta := SourceRect.Bottom - SourceSize.Y;
    SourceRect.Bottom := SourceRect.Bottom - Delta;
    DestRect.Bottom := DestRect.Bottom - (Delta * Scale.Y);
  end;

  if DestRect.Left < 0 then
  begin
    Delta := -DestRect.Left;
    DestRect.Left := DestRect.Left + Delta;
    SourceRect.Left := SourceRect.Left + (Delta / Scale.X);
  end;

  if DestRect.Top < 0 then
  begin
    Delta := -DestRect.Top;
    DestRect.Top := DestRect.Top + Delta;
    SourceRect.Top := SourceRect.Top + (Delta / Scale.Y);
  end;

  if DestRect.Right > DestSize.X then
  begin
    Delta := DestRect.Right - DestSize.X;
    DestRect.Right := DestRect.Right - Delta;
    SourceRect.Right := SourceRect.Right - (Delta / Scale.X);
  end;

  if DestRect.Bottom > DestSize.Y then
  begin
    Delta := DestRect.Bottom - DestSize.Y;
    DestRect.Bottom := DestRect.Bottom - Delta;
    SourceRect.Bottom := SourceRect.Bottom - (Delta / Scale.Y);
  end;

  Result := (not SourceRect.Empty) and (not DestRect.Empty);
end;

function FloatRect(const Left, Top, Width, Height: VectorFloat): TFloatRect;
begin
  Result.Left := Left;
  Result.Top := Top;
  Result.Width := Width;
  Result.Height := Height;
end;

function FloatRect(const Origin, Size: TPoint2f): TFloatRect;
begin
  Result.TopLeft := Origin;
  Result.Size := Size;
end;

function FloatRectBDS(const Left, Top, Right, Bottom: VectorFloat): TFloatRect;
begin
  Result.Left := Left;
  Result.Top := Top;
  Result.Right := Right;
  Result.Bottom := Bottom;
end;

function FloatRectBDS(const TopLeft, BottomRight: TPoint2f): TFloatRect;
begin
  Result.TopLeft := TopLeft;
  Result.BottomRight := BottomRight;
end;

{$ENDREGION}
{$REGION 'TQuad'}

class operator TQuad.Equal(const Rect1, Rect2: TQuad): Boolean;
begin
  Result := (Rect1.TopLeft = Rect2.TopLeft) and (Rect1.TopRight = Rect2.TopRight) and
    (Rect1.BottomRight = Rect2.BottomRight) and (Rect1.BottomLeft = Rect2.BottomLeft);
end;

class operator TQuad.NotEqual(const Rect1, Rect2: TQuad): Boolean;
begin
  Result := not (Rect1 = Rect2);
end;

function TQuad.Scale(const Scale: VectorFloat; const Centered: Boolean): TQuad;
var
  Center: TPoint2f;
begin
  if Abs(Scale - 1.0) <= VectorEpsilon then
    Exit(Self);

  if Centered then
  begin
    Center := (TopLeft + TopRight + BottomRight + BottomLeft) * 0.25;
    Result.TopLeft := Center.Lerp(TopLeft, Scale);
    Result.TopRight := Center.Lerp(TopRight, Scale);
    Result.BottomRight := Center.Lerp(BottomRight, Scale);
    Result.BottomLeft := Center.Lerp(BottomLeft, Scale);
  end
  else
  begin
    Result.TopLeft := TopLeft * Scale;
    Result.TopRight := TopRight * Scale;
    Result.BottomRight := BottomRight * Scale;
    Result.BottomLeft := BottomLeft * Scale;
  end;
end;

function TQuad.Mirror: TQuad;
begin
  Result.TopLeft := TopRight;
  Result.TopRight := TopLeft;
  Result.BottomRight := BottomLeft;
  Result.BottomLeft := BottomRight;
end;

function TQuad.Flip: TQuad;
begin
  Result.TopLeft := BottomLeft;
  Result.TopRight := BottomRight;
  Result.BottomRight := TopRight;
  Result.BottomLeft := TopLeft;
end;

function TQuad.Transform(const Matrix: TMatrix3f): TQuad;
begin
  Result.TopLeft := TopLeft * Matrix;
  Result.TopRight := TopRight * Matrix;
  Result.BottomRight := BottomRight * Matrix;
  Result.BottomLeft := BottomLeft * Matrix;
end;

function TQuad.Offset(const Delta: TPoint2f): TQuad;
begin
  Result.TopLeft := TopLeft + Delta;
  Result.TopRight := TopRight + Delta;
  Result.BottomRight := BottomRight + Delta;
  Result.BottomLeft := BottomLeft + Delta;
end;

function TQuad.Offset(const DeltaX, DeltaY: VectorFloat): TQuad;
begin
  Result := Offset(Point2f(DeltaX, DeltaY));
end;

function TQuad.Contains(const Point: TPoint2f): Boolean;
begin
  Result :=
    Point.InsideTriangle(Values[0], Values[1], Values[2]) or
    Point.InsideTriangle(Values[2], Values[3], Values[0]);
end;

class function TQuad.Scaled(const Left, Top, Width, Height, Scale: VectorFloat;
  const Centered: Boolean): TQuad;
var
  NewLeft, NewTop, NewWidth, NewHeight: VectorFloat;
begin
  if Abs(Scale - 1.0) <= VectorEpsilon then
    Exit(Quad(Left, Top, Width, Height));

  if Centered then
  begin
    NewWidth := Width * Scale;
    NewHeight := Height * Scale;
    NewLeft := Left + (Width - NewWidth) * 0.5;
    NewTop := Top + (Height - NewHeight) * 0.5;

    Result := Quad(NewLeft, NewTop, NewWidth, NewHeight);
  end
  else
    Result := Quad(Left, Top, Width * Scale, Height * Scale);
end;

class function TQuad.Rotated(const RotationOrigin, Size, RotationCenter: TPoint2f; const Angle,
  Scale: VectorFloat): TQuad;
var
  SinAngle, CosAngle: VectorFloat;
  IsScaled: Boolean;
  NewPoint: TPoint2f;
  Index: Integer;
begin
  SinCos(Angle, SinAngle, CosAngle);

  Result := Quad(-RotationCenter.X, -RotationCenter.Y, Size.X, Size.Y);

  IsScaled := Abs(Scale - 1.0) > VectorEpsilon;

  for Index := 0 to High(Result.Values) do
  begin
    if IsScaled then
      Result.Values[Index] := Result.Values[Index] * Scale;

    NewPoint.X := Result.Values[Index].X * CosAngle - Result.Values[Index].Y * SinAngle;
    NewPoint.Y := Result.Values[Index].Y * CosAngle + Result.Values[Index].X * SinAngle;

    Result.Values[Index] := NewPoint + RotationOrigin;
  end;
end;

class function TQuad.Rotated(const RotationOrigin, Size: TPoint2f; const Angle, Scale: VectorFloat): TQuad;
begin
  Result := Rotated(RotationOrigin, Size, Size * 0.5, Angle, Scale);
end;

class function TQuad.RotatedTL(const TopLeft, Size, RotationCenter: TPoint2f; const Angle: VectorFloat;
  const Scale: VectorFloat): TQuad;
begin
  Result := Rotated(TopLeft, Size, RotationCenter, Angle, Scale).Offset(RotationCenter);
end;

function Quad(const TopLeftX, TopLeftY, TopRightX, TopRightY, BottomRightX, BottomRightY, BottomLeftX,
  BottomLeftY: VectorFloat): TQuad;
begin
  Result.TopLeft.X := TopLeftX;
  Result.TopLeft.Y := TopLeftY;
  Result.TopRight.X := TopRightX;
  Result.TopRight.Y := TopRightY;
  Result.BottomRight.X := BottomRightX;
  Result.BottomRight.Y := BottomRightY;
  Result.BottomLeft.X := BottomLeftX;
  Result.BottomLeft.Y := BottomLeftY;
end;

function Quad(const TopLeft, TopRight, BottomRight, BottomLeft: TPoint2f): TQuad;
begin
  Result.TopLeft := TopLeft;
  Result.TopRight := TopRight;
  Result.BottomRight := BottomRight;
  Result.BottomLeft := BottomLeft;
end;

function Quad(const Left, Top, Width, Height: VectorFloat): TQuad;
begin
  Result.TopLeft.X := Left;
  Result.TopLeft.Y := Top;
  Result.TopRight.X := Left + Width;
  Result.TopRight.Y := Top;
  Result.BottomRight.X := Result.TopRight.X;
  Result.BottomRight.Y := Top + Height;
  Result.BottomLeft.X := Left;
  Result.BottomLeft.Y := Result.BottomRight.Y;
end;

function Quad(const Rect: TFloatRect): TQuad;
begin
  Result.TopLeft := Rect.TopLeft;
  Result.TopRight := Point2f(Rect.Right, Rect.Top);
  Result.BottomRight := Rect.BottomRight;
  Result.BottomLeft := Point2f(Rect.Left, Rect.Bottom);
end;

function Quad(const Rect: TIntRect): TQuad;
begin
  Result.TopLeft := Rect.TopLeft;
  Result.TopRight := Point2f(Rect.Right, Rect.Top);
  Result.BottomRight := Rect.BottomRight;
  Result.BottomLeft := Point2f(Rect.Left, Rect.Bottom);
end;

{$ENDREGION}

end.
