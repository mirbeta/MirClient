{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressCore Library                                      }
{                                                                    }
{           Copyright (c) 1998-2019 Developer Express Inc.           }
{           ALL RIGHTS RESERVED                                      }
{                                                                    }
{   The entire contents of this file is protected by U.S. and        }
{   International Copyright Laws. Unauthorized reproduction,         }
{   reverse-engineering, and distribution of all or any portion of   }
{   the code contained in this file is strictly prohibited and may   }
{   result in severe civil and criminal penalties and will be        }
{   prosecuted to the maximum extent possible under the law.         }
{                                                                    }
{   RESTRICTIONS                                                     }
{                                                                    }
{   THIS SOURCE CODE AND ALL RESULTING INTERMEDIATE FILES            }
{   (DCU, OBJ, DLL, ETC.) ARE CONFIDENTIAL AND PROPRIETARY TRADE     }
{   SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER IS    }
{   LICENSED TO DISTRIBUTE THE EXPRESSCORE LIBRARY AND ALL           }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY. }
{                                                                    }
{   THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED       }
{   FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE         }
{   COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE        }
{   AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT   }
{   AND PERMISSION FROM DEVELOPER EXPRESS INC.                       }
{                                                                    }
{   CONSULT THE END USER LICENSE AGREEMENT FOR INFORMATION ON        }
{   ADDITIONAL RESTRICTIONS.                                         }
{                                                                    }
{********************************************************************}

unit dxCoreGraphics;

{$I cxVer.inc}

{$ALIGN ON}
{$MINENUMSIZE 4}

interface

uses
  Windows, Messages, Types, SysUtils, Classes, Graphics,  Generics.Defaults, Generics.Collections, dxCore, dxCoreClasses;

const
  dxMeasurePattern = 'Zq';

  AlphaShift  = 24;
  {$EXTERNALSYM AlphaShift}
  RedShift    = 16;
  {$EXTERNALSYM RedShift}
  GreenShift  = 8;
  {$EXTERNALSYM GreenShift}
  BlueShift   = 0;
  {$EXTERNALSYM BlueShift}

  AlphaMask   = $ff000000;
  {$EXTERNALSYM AlphaMask}
  RedMask     = $00ff0000;
  {$EXTERNALSYM RedMask}
  GreenMask   = $0000ff00;
  {$EXTERNALSYM GreenMask}
  BlueMask    = $000000ff;
  {$EXTERNALSYM BlueMask}

type
  TdxAlphaState = (asNoAlpha, asSemitransparent, asOpaque);

  TdxAlphaColor = type DWORD;
  PdxAlphaColor  = ^TdxAlphaColor;

  { TdxAlphaColor }

  TdxAlphaColors = record
  strict private type
  {$REGION 'private types'}
      // "System" colors
      TSystemColor = (
        cActiveBorder, cActiveCaption, cActiveCaptionText, cAppWorkspace, cButtonFace, cButtonHighlight,
        cButtonShadow, cControl, cControlDark, cControlDarkDark, cControlLight, cControlLightLight,
        cControlText, cDesktop, cGradientActiveCaption, cGradientInactiveCaption, cGrayText, cHighlight,
        cHighlightText, cHotTrack, cInactiveBorder, cInactiveCaption, cInactiveCaptionText, cInfo,
        cInfoText, cMenu, cMenuBar, cMenuHighlight, cMenuText, cScrollBar, cWindow, cWindowFrame, cWindowText);

      TSystemColorData = record
        Name: string;
        Index: Integer;
      end;

      TColorInfo = record
        Color: TdxAlphaColor;
        Name: string;
      end;

      TUpdateSystemColorsListener = class
      strict private
        FHandle: HWND;
      public
        constructor Create;
        destructor Destroy; override;
        procedure WndProc(var Message: TMessage);
      end;
  {$ENDREGION}
  strict private const
  {$REGION 'private const'}
      SystemColorsData: array[TSystemColor] of TSystemColorData = (
        (Name: 'ActiveBorder';          Index: $0A), (Name: 'ActiveCaption';           Index: $02),
        (Name: 'ActiveCaptionText';     Index: $09), (Name: 'AppWorkspace';            Index: $0C),
        (Name: 'ButtonFace';            Index: $0F), (Name: 'ButtonHighlight';         Index: $14),
        (Name: 'ButtonShadow';          Index: $10), (Name: 'Control';                 Index: $0F),
        (Name: 'ControlDark';           Index: $10), (Name: 'ControlDarkDark';         Index: $15),
        (Name: 'ControlLight';          Index: $16), (Name: 'ControlLightLight';       Index: $14),
        (Name: 'ControlText';           Index: $12), (Name: 'Desktop';                 Index: $01),
        (Name: 'GradientActiveCaption'; Index: $1B), (Name: 'GradientInactiveCaption'; Index: $1C),
        (Name: 'GrayText';              Index: $11), (Name: 'Highlight';               Index: $0D),
        (Name: 'HighlightText';         Index: $0E), (Name: 'HotTrack';                Index: $1A),
        (Name: 'InactiveBorder';        Index: $0B), (Name: 'InactiveCaption';         Index: $03),
        (Name: 'InactiveCaptionText';   Index: $13), (Name: 'Info';                    Index: $18),
        (Name: 'InfoText';              Index: $17), (Name: 'Menu';                    Index: $04),
        (Name: 'MenuBar';               Index: $1E), (Name: 'MenuHighlight';           Index: $1D),
        (Name: 'MenuText';              Index: $07), (Name: 'ScrollBar';               Index: $00),
        (Name: 'Window';                Index: $05), (Name: 'WindowFrame';             Index: $06),
        (Name: 'WindowText';            Index: $08));
  {$ENDREGION}
    class var
      FNameToColorMap: TDictionary<string, TdxAlphaColor>;
      FHtmlNameToColorMap: TDictionary<string, TdxAlphaColor>;
      FColorToNameMap: TList<TColorInfo>;
      FSystemColors: array[TSystemColor] of TdxAlphaColor;
      FSystemColorsListener: TUpdateSystemColorsListener;
{$IFDEF DELPHIXE}
    class constructor Initialize;
    class destructor Finalize;
{$ELSE}
  public
    class procedure Initialize; static;
    class procedure Finalize; static;
  private
{$ENDIF}
    class procedure CreateColors; static;
    class procedure CreateHtmlColors; static;
    class procedure AddNamedColor(const AName: string; AColor: TdxAlphaColor); static;
    class function GetSystemAlphaColor(AColor: TSystemColor): TdxAlphaColor; static;
  public
    class procedure UpdateSystemColors; static;
    class procedure UpdateHtmlSystemColors; static;
  public const
    // special colors
    Empty                = $00000000;
    Transparent          = $00FFFFFF;
    Default              = $00010203;
    // standard colors
    AliceBlue            = $FFF0F8FF;
    AntiqueWhite         = $FFFAEBD7;
    Aqua                 = $FF00FFFF;
    Aquamarine           = $FF7FFFD4;
    Azure                = $FFF0FFFF;
    Beige                = $FFF5F5DC;
    Bisque               = $FFFFE4C4;
    Black                = $FF000000;
    BlanchedAlmond       = $FFFFEBCD;
    Blue                 = $FF0000FF;
    BlueViolet           = $FF8A2BE2;
    Brown                = $FFA52A2A;
    BurlyWood            = $FFDEB887;
    CadetBlue            = $FF5F9EA0;
    Chartreuse           = $FF7FFF00;
    Chocolate            = $FFD2691E;
    Coral                = $FFFF7F50;
    CornflowerBlue       = $FF6495ED;
    Cornsilk             = $FFFFF8DC;
    Crimson              = $FFDC143C;
    Cyan                 = $FF00FFFF;
    DarkBlue             = $FF00008B;
    DarkCyan             = $FF008B8B;
    DarkGoldenrod        = $FFB8860B;
    DarkGray             = $FFA9A9A9;
    DarkGreen            = $FF006400;
    DarkKhaki            = $FFBDB76B;
    DarkMagenta          = $FF8B008B;
    DarkOliveGreen       = $FF556B2F;
    DarkOrange           = $FFFF8C00;
    DarkOrchid           = $FF9932CC;
    DarkRed              = $FF8B0000;
    DarkSalmon           = $FFE9967A;
    DarkSeaGreen         = $FF8FBC8B;
    DarkSlateBlue        = $FF483D8B;
    DarkSlateGray        = $FF2F4F4F;
    DarkTurquoise        = $FF00CED1;
    DarkViolet           = $FF9400D3;
    DeepPink             = $FFFF1493;
    DeepSkyBlue          = $FF00BFFF;
    DimGray              = $FF696969;
    DodgerBlue           = $FF1E90FF;
    Firebrick            = $FFB22222;
    FloralWhite          = $FFFFFAF0;
    ForestGreen          = $FF228B22;
    Fuchsia              = $FFFF00FF;
    Gainsboro            = $FFDCDCDC;
    GhostWhite           = $FFF8F8FF;
    Gold                 = $FFFFD700;
    Goldenrod            = $FFDAA520;
    Gray                 = $FF808080;
    Green                = $FF008000;
    GreenYellow          = $FFADFF2F;
    Honeydew             = $FFF0FFF0;
    HotPink              = $FFFF69B4;
    IndianRed            = $FFCD5C5C;
    Indigo               = $FF4B0082;
    Ivory                = $FFFFFFF0;
    Khaki                = $FFF0E68C;
    Lavender             = $FFE6E6FA;
    LavenderBlush        = $FFFFF0F5;
    LawnGreen            = $FF7CFC00;
    LemonChiffon         = $FFFFFACD;
    LightBlue            = $FFADD8E6;
    LightCoral           = $FFF08080;
    LightCyan            = $FFE0FFFF;
    LightGoldenrodYellow = $FFFAFAD2;
    LightGray            = $FFD3D3D3;
    LightGreen           = $FF90EE90;
    LightPink            = $FFFFB6C1;
    LightSalmon          = $FFFFA07A;
    LightSeaGreen        = $FF20B2AA;
    LightSkyBlue         = $FF87CEFA;
    LightSlateGray       = $FF778899;
    LightSteelBlue       = $FFB0C4DE;
    LightYellow          = $FFFFFFE0;
    Lime                 = $FF00FF00;
    LimeGreen            = $FF32CD32;
    Linen                = $FFFAF0E6;
    Magenta              = $FFFF00FF;
    Maroon               = $FF800000;
    MediumAquamarine     = $FF66CDAA;
    MediumBlue           = $FF0000CD;
    MediumOrchid         = $FFBA55D3;
    MediumPurple         = $FF9370DB;
    MediumSeaGreen       = $FF3CB371;
    MediumSlateBlue      = $FF7B68EE;
    MediumSpringGreen    = $FF00FA9A;
    MediumTurquoise      = $FF48D1CC;
    MediumVioletRed      = $FFC71585;
    MidnightBlue         = $FF191970;
    MintCream            = $FFF5FFFA;
    MistyRose            = $FFFFE4E1;
    Moccasin             = $FFFFE4B5;
    NavajoWhite          = $FFFFDEAD;
    Navy                 = $FF000080;
    OldLace              = $FFFDF5E6;
    Olive                = $FF808000;
    OliveDrab            = $FF6B8E23;
    Orange               = $FFFFA500;
    OrangeRed            = $FFFF4500;
    Orchid               = $FFDA70D6;
    PaleGoldenrod        = $FFEEE8AA;
    PaleGreen            = $FF98FB98;
    PaleTurquoise        = $FFAFEEEE;
    PaleVioletRed        = $FFDB7093;
    PapayaWhip           = $FFFFEFD5;
    PeachPuff            = $FFFFDAB9;
    Peru                 = $FFCD853F;
    Pink                 = $FFFFC0CB;
    Plum                 = $FFDDA0DD;
    PowderBlue           = $FFB0E0E6;
    Purple               = $FF800080;
    Red                  = $FFFF0000;
    RosyBrown            = $FFBC8F8F;
    RoyalBlue            = $FF4169E1;
    SaddleBrown          = $FF8B4513;
    Salmon               = $FFFA8072;
    SandyBrown           = $FFF4A460;
    SeaGreen             = $FF2E8B57;
    SeaShell             = $FFFFF5EE;
    Sienna               = $FFA0522D;
    Silver               = $FFC0C0C0;
    SkyBlue              = $FF87CEEB;
    SlateBlue            = $FF6A5ACD;
    SlateGray            = $FF708090;
    Snow                 = $FFFFFAFA;
    SpringGreen          = $FF00FF7F;
    SteelBlue            = $FF4682B4;
    Tan                  = $FFD2B48C;
    Teal                 = $FF008080;
    Thistle              = $FFD8BFD8;
    Tomato               = $FFFF6347;
    Turquoise            = $FF40E0D0;
    Violet               = $FFEE82EE;
    Wheat                = $FFF5DEB3;
    White                = $FFFFFFFF;
    WhiteSmoke           = $FFF5F5F5;
    Yellow               = $FFFFFF00;
    YellowGreen          = $FF9ACD32;

    // system colors aliases
    class function _3DDkShadow: TdxAlphaColor; static; inline;
    class function _3DLight: TdxAlphaColor; static; inline;
    class function ActiveBorder: TdxAlphaColor; static; inline;
    class function ActiveCaption: TdxAlphaColor; static; inline;
    class function ActiveCaptionText: TdxAlphaColor; static; inline;
    class function AppWorkSpace: TdxAlphaColor; static; inline;
    class function Background: TdxAlphaColor; static; inline;
    class function BtnFace: TdxAlphaColor; static; inline;
    class function BtnHighlight: TdxAlphaColor; static; inline;
    class function BtnShadow: TdxAlphaColor; static; inline;
    class function BtnText: TdxAlphaColor; static; inline;
    class function ButtonFace: TdxAlphaColor; static; inline;
    class function ButtonHighlight: TdxAlphaColor; static; inline;
    class function ButtonShadow: TdxAlphaColor; static; inline;
    class function CaptionText: TdxAlphaColor; static; inline;
    class function Control: TdxAlphaColor; static; inline;
    class function ControlDark: TdxAlphaColor; static; inline;
    class function ControlDarkDark: TdxAlphaColor; static; inline;
    class function ControlLight: TdxAlphaColor; static; inline;
    class function ControlLightLight: TdxAlphaColor; static; inline;
    class function ControlText: TdxAlphaColor; static; inline;
    class function Desktop: TdxAlphaColor; static; inline;
    class function GradientActiveCaption: TdxAlphaColor; static; inline;
    class function GradientInactiveCaption: TdxAlphaColor; static; inline;
    class function GrayText: TdxAlphaColor; static; inline;
    class function Highlight: TdxAlphaColor; static; inline;
    class function HighlightText: TdxAlphaColor; static; inline;
    class function HotLight: TdxAlphaColor; static; inline;
    class function HotTrack: TdxAlphaColor; static; inline;
    class function InactiveBorder: TdxAlphaColor; static; inline;
    class function InactiveCaption: TdxAlphaColor; static; inline;
    class function InactiveCaptionText: TdxAlphaColor; static; inline;
    class function Info: TdxAlphaColor; static; inline;
    class function InfoBk: TdxAlphaColor; static; inline;
    class function InfoText: TdxAlphaColor; static; inline;
    class function Menu: TdxAlphaColor; static; inline;
    class function MenuBar: TdxAlphaColor; static; inline;
    class function MenuHighlight: TdxAlphaColor; static; inline;
    class function MenuText: TdxAlphaColor; static; inline;
    class function ScrollBar: TdxAlphaColor; static; inline;
    class function Window: TdxAlphaColor; static; inline;
    class function WindowFrame: TdxAlphaColor; static; inline;
    class function WindowText: TdxAlphaColor; static; inline;

    class function Blend(AColor, ABackgroundColor: TdxAlphaColor): TdxAlphaColor; overload; static; inline;
    class function CalculateNearestColor(const AColorsToChooseFrom: array of TdxAlphaColor; AValue: TdxAlphaColor): TdxAlphaColor; static;
    class function ChangeBrightness(AColor: TdxAlphaColor; ABrightness: Single): TdxAlphaColor; static;
    class function GetBrightness(AColor: TdxAlphaColor): Single; static;
    class function GetHue(AColor: TdxAlphaColor): Single; static;
    class function GetSaturation(AColor: TdxAlphaColor): Single; static;
    class function IsEmpty(AColor: TdxAlphaColor): Boolean; overload; static; inline;
    class function IsTransparentOrEmpty(const AColor: TdxAlphaColor): Boolean; overload; static; inline;
    class function FromArgb(ARed, AGreen, ABlue: Byte): TdxAlphaColor; overload; static; inline;
    class function FromArgb(AAlpha, ARed, AGreen, ABlue: Byte): TdxAlphaColor; overload; static; inline;
    class function FromArgb(AAlpha: Byte; ABaseColor: TdxAlphaColor): TdxAlphaColor; overload; static; inline;
    class function FromColor(const AColor: TColor): TdxAlphaColor; overload; static; inline;
    class function FromHSL(H, S, L: Single): TdxAlphaColor; overload; static;
    class function ToArgb(const AColor: TdxAlphaColor): Cardinal; static; inline;
    class function ToColor(const AColor: TdxAlphaColor): TColor; overload; static; inline;
    class procedure ToHSL(AColor: TdxAlphaColor; out H, S, L: Single); static;

    class function R(AColor: TdxAlphaColor): Byte; static; inline;
    class function G(AColor: TdxAlphaColor): Byte; static; inline;
    class function B(AColor: TdxAlphaColor): Byte; static; inline;
    class function Alpha(AColor: TdxAlphaColor): Byte; static; inline;
    class function GetColorName(AColor: TdxAlphaColor): string; static;
    class function IsKnownColor(AColor: TdxAlphaColor): Boolean; static;
    class function FromName(const AName: string): TdxAlphaColor; static;
    class function FromHexCode(const AHexCode: string): TdxAlphaColor; static; // for internal use
    class function FromHtml(const AHtmlColor: string): TdxAlphaColor; static;
    class function ToHexCode(AColor: TdxAlphaColor; AUseAlpha: Boolean; APrefix: Char = #0000): string; static; // for internal use
    class function ToHtml(AColor: TdxAlphaColor; AUseNamedColors: Boolean = True): string; static;
  end;

  TRGBColors = array of TRGBQuad;

  TdxGraphicUnit = (
    guWorld,      // 0 -- World coordinate (non-physical unit)
    guDisplay,    // 1 -- Variable -- for PageTransform only
    guPixel,      // 2 -- Each unit is one device pixel.
    guPoint,      // 3 -- Each unit is a printer's point, or 1/72 inch.
    guInch,       // 4 -- Each unit is 1 inch.
    guDocument,   // 5 -- Each unit is 1/300 inch.
    guMillimeter  // 6 -- Each unit is 1 millimeter.
  );

  { IdxColorPalette }

  IdxColorPalette = interface
  ['{D0CB7E80-BE5A-4532-972C-5046B346FDA5}']
    function GetID: TGUID;
    function GetFillColor(const ID: string): TdxAlphaColor;
    function GetStrokeColor(const ID: string): TdxAlphaColor;
  end;

  { TdxCustomColorPalette }

  TdxCustomColorPalette = class(TInterfacedObject, IdxColorPalette)
  protected
    FID: TGUID;
  public
    procedure AfterConstruction; override;
    // IdxColorPalette
    function GetID: TGUID;
    function GetFillColor(const ID: string): TdxAlphaColor; virtual; abstract;
    function GetStrokeColor(const ID: string): TdxAlphaColor; virtual; abstract;
  end;

  { TdxSimpleColorPalette }

  TdxSimpleColorPalette = class(TdxCustomColorPalette)
  strict private
    FFillColor: TdxAlphaColor;
    FStrokeColor: TdxAlphaColor;
  public
    constructor Create(AFillColor, AStrokeColor: TdxAlphaColor);
    // IdxColorPalette
    function GetFillColor(const ID: string): TdxAlphaColor; override;
    function GetStrokeColor(const ID: string): TdxAlphaColor; override;
    //
    property FillColor: TdxAlphaColor read FFillColor write FFillColor;
    property StrokeColor: TdxAlphaColor read FStrokeColor write FStrokeColor;
  end;

  { TdxAdvancedColorPalette }

  TdxAdvancedColorPalette = class(TdxCustomColorPalette)
  strict private
    FFillColors: TDictionary<string, TdxAlphaColor>;
    FStrokeColors: TDictionary<string, TdxAlphaColor>;

    procedure HandlerKeyChanged(Sender: TObject; const Item: string; Action: TCollectionNotification);
    procedure HandlerValueChanged(Sender: TObject; const Item: TdxAlphaColor; Action: TCollectionNotification);
    procedure SetFillColor(const ID: string; const Value: TdxAlphaColor);
    procedure SetStrokeColor(const ID: string; const Value: TdxAlphaColor);
  protected
    procedure Changed; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    // IdxColorPalette
    function GetFillColor(const ID: string): TdxAlphaColor; override;
    function GetStrokeColor(const ID: string): TdxAlphaColor; override;
    //
    property FillColors[const ID: string]: TdxAlphaColor read GetFillColor write SetFillColor;
    property StrokeColors[const ID: string]: TdxAlphaColor read GetStrokeColor write SetStrokeColor;
  end;

  { TdxCustomValueCacheManager }

  TdxCustomValueCacheManager<TKey, TValue> = class
  strict private
    FCapacity: Integer;
    FData: TList<TPair<TKey, TValue>>;
    FLastKey: TKey;
    FLastKeyIndex: Integer;

    function Find(const Key: TKey; out AFoundIndex: Integer): Boolean;
    procedure ValueHandler(Sender: TObject; const Item: TPair<TKey, TValue>; Action: TCollectionNotification);
  protected
    function Compare(const Key1, Key2: TKey): Integer; virtual; abstract;
    function CreateValue(const Key: TKey): TValue; virtual; abstract;
    procedure DoRemove(const Value: TValue); virtual;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    procedure BeforeDestruction; override;
    procedure Clear;
    function GetValue(const Key: TKey): TValue;
  end;

  { TdxGdiClippingHelper }

  TdxGdiClippingHelper = record
  public
    class procedure AddClippingRectangle(DC: HDC; const R: TRect); static;
    class procedure AddRectangleToRegion(var ARegion: HRGN; const R: TRect); static;
    class procedure ExcludeRectangleFromRegion(var ARegion: HRGN; const R: TRect); static;
    class procedure RestoreClipping(DC: HDC; ARegion: HRGN); static;
    class procedure SaveClipping(DC: HDC; out ARegion: HRGN); static;
  end;

  { TdxRTLReadingCharacterPlacementHelper }

  TdxRTLReadingCharacterPlacementHelper = record
  private
    const GCPFlags: array[Boolean] of Integer = (GCP_USEKERNING or GCP_LIGATE, GCP_USEKERNING or GCP_LIGATE or GCP_REORDER);
    class function CalculateCharactersBounds(const AGcpResults: TGCPResults; ALength: Integer; const ABounds: TRect): TArray<TRect>; static;
    class function TryMeasureCharactersSlow(ADC: THandle; const AText: PChar; ALength: Integer; var AGcpResults: TGCPResults;
      AFlags: Integer): Integer; static;
  public
    class function GetSelectionRangeRegion(ADC: HDC; const AText: PChar; ALength: Integer; const ABounds: TRect;
      ASelStart, ASelLength: Integer; AFont: HFONT = 0; AIsRTLReading: Boolean = False): HRGN; static;
    class function MeasureCharactersBounds(ADC: HDC; const AText: PChar; ALength: Integer;
      const ABounds: TRect; AFont: HFONT = 0; AIsRTLReading: Boolean = False): TArray<TRect>; static;
  end;

const
  dxacDefault = TdxAlphaColors.Default;
  dxacNone = TdxAlphaColors.Empty;

// dxAlphaColor functions
function dxAlphaColorToColor(AColor: TdxAlphaColor): TColor; overload;
function dxAlphaColorToColor(AColor: TdxAlphaColor; out AAlpha: Byte): TColor; overload;
function dxAlphaColorToRGBQuad(AColor: TdxAlphaColor): TRGBQuad;
function dxColorToAlphaColor(AColor: TColor; AAlpha: Byte = 255): TdxAlphaColor;
function dxColorRefToAlphaColor(AREF: COLORREF): TdxAlphaColor;
function dxAlphaColorToColorRef(AColor: TdxAlphaColor): COLORREF;
function dxGetAlpha(AColor: TdxAlphaColor): Byte;
function dxGetRed(AColor: TdxAlphaColor): Byte;
function dxGetGreen(AColor: TdxAlphaColor): Byte;
function dxGetBlue(AColor: TdxAlphaColor): Byte;
function dxMakeAlphaColor(R, G, B: Byte): TdxAlphaColor; overload;
function dxMakeAlphaColor(A, R, G, B: Byte): TdxAlphaColor; overload;
function dxMakeAlphaColor(AColor: TColor; AAlpha: Byte = 255): TdxAlphaColor; overload;

// graphic functions
function dxColorToRGBQuad(AColor: TColor; AReserved: Byte = 0): TRGBQuad;
function dxRGBQuadToColor(const ARGB: TRGBQuad): TColor;
function cxGetBitmapPixelFormat(ABitmap: TBitmap): Integer;
procedure dxFillBitmapInfoHeader(out AHeader: TBitmapInfoHeader; ABitmap: TBitmap; ATopDownDIB: WordBool); overload;
procedure dxFillBitmapInfoHeader(out AHeader: TBitmapInfoHeader; AWidth, AHeight: Integer; ATopDownDIB: WordBool); overload;
function GetBitmapBits(ABitmap: TBitmap; var AColors: TRGBColors; ATopDownDIB: Boolean): Boolean;
procedure GetBitmapBitsByScanLine(ABitmap: TBitmap; var AColors: TRGBColors);
procedure SetBitmapBits(ABitmap: TBitmap; const AColors: TRGBColors; ATopDownDIB: Boolean);
function dxGetAlphaState(const AColors: TRGBColors): TdxAlphaState; overload;
function dxGetAlphaState(const ABitmap: TBitmap): TdxAlphaState; overload;
procedure dxChangeColor(AColors: PRGBQuad; ACount: Integer; AColor: TColor);
function dxIsAlphaUsed(ABitmap: TBitmap): Boolean;
function dxGetColorPaletteID(const AIntf: IdxColorPalette): TGUID;

// getting graphic objects' data
function dxGetBitmapData(ABitmapHandle: HBITMAP; out ABitmapData: Windows.TBitmap): Boolean;
function dxGetBrushData(ABrushHandle: HBRUSH; out ALogBrush: TLogBrush): Boolean; overload;
function dxGetBrushData(ABrushHandle: HBRUSH): TLogBrush; overload;
function dxGetFontData(AFontHandle: HFONT; out ALogFont: TLogFont): Boolean; overload;
function dxGetFontData(AFontHandle: HFONT): TLogFont; overload;
function dxGetPenData(APenHandle: HPEN; out ALogPen: TLogPen): Boolean;

function dxGraphicIsEquals(AGraphic1, AGraphic2: TGraphic): Boolean;

implementation

uses
  Math, dxTypeHelpers;

// graphic functions
type
  TRGBA = packed record
    R: Byte;
    G: Byte;
    B: Byte;
    A: Byte;
  end;

  TAlphaColorChannelMap = packed record
    B, G, R, Alpha: Byte;
  end;

  TUINTArray = array[0..8192] of UINT;
  PUINTArray = ^TUINTArray;

// dxAlphaColor functions
function dxAlphaColorToColor(AColor: TdxAlphaColor): TColor;
var
  AAlpha: Byte;
begin
  Result := dxAlphaColorToColor(AColor, AAlpha);
end;

function dxAlphaColorToColor(AColor: TdxAlphaColor; out AAlpha: Byte): TColor;
begin
  AAlpha := 0;
  if AColor = dxacNone then
    Result := clNone
  else
    if AColor = dxacDefault then
      Result := clDefault
    else
    begin
      AAlpha := AColor shr AlphaShift;
      Result := RGB(Byte(AColor shr RedShift), Byte(AColor shr GreenShift), Byte(AColor shr BlueShift));
    end;
end;

function dxAlphaColorToRGBQuad(AColor: TdxAlphaColor): TRGBQuad;
begin
  Result.rgbBlue := Byte(AColor shr BlueShift);
  Result.rgbGreen := Byte(AColor shr GreenShift);
  Result.rgbRed := Byte(AColor shr RedShift);
  Result.rgbReserved := AColor shr AlphaShift;
end;

function dxColorToAlphaColor(AColor: TColor; AAlpha: Byte = 255): TdxAlphaColor;
begin
  if AColor = clNone then
    Result := dxacNone
  else
    if AColor = clDefault then
      Result := dxacDefault
    else
      Result := TdxAlphaColor(dxColorToRGBQuad(AColor, AAlpha));
end;

function dxColorRefToAlphaColor(AREF: COLORREF): TdxAlphaColor;
begin
  Result := dxMakeAlphaColor(255, GetRValue(AREF), GetGValue(AREF), GetBValue(AREF));
end;

function dxAlphaColorToColorRef(AColor: TdxAlphaColor): COLORREF;
begin
  Result := RGB(dxGetRed(AColor), dxGetGreen(AColor), dxGetBlue(AColor));
end;

function dxGetAlpha(AColor: TdxAlphaColor): Byte;
begin
  Result := Byte(AColor shr AlphaShift);
end;

function dxGetRed(AColor: TdxAlphaColor): Byte;
begin
  Result := Byte(AColor shr RedShift);
end;

function dxGetGreen(AColor: TdxAlphaColor): Byte;
begin
  Result := Byte(AColor shr GreenShift);
end;

function dxGetBlue(AColor: TdxAlphaColor): Byte;
begin
  Result := Byte(AColor shr BlueShift);
end;

function dxMakeAlphaColor(R, G, B: Byte): TdxAlphaColor; overload;
begin
  Result := dxMakeAlphaColor(255, R, G, B);
end;

function dxMakeAlphaColor(A, R, G, B: Byte): TdxAlphaColor; overload;
begin
  Result := (DWORD(B) shl BlueShift) or (DWORD(G) shl GreenShift) or (DWORD(R) shl RedShift) or (DWORD(A) shl AlphaShift);
end;

function dxMakeAlphaColor(AColor: TColor; AAlpha: Byte = 255): TdxAlphaColor; overload;
begin
  Result := dxColorToAlphaColor(AColor, AAlpha);
end;

function dxColorToRGBQuad(AColor: TColor; AReserved: Byte = 0): TRGBQuad;
var
  ATemp: TRGBA;
begin
  DWORD(ATemp) := ColorToRGB(AColor);
  Result.rgbBlue := ATemp.B;
  Result.rgbRed := ATemp.R;
  Result.rgbGreen := ATemp.G;
  Result.rgbReserved := AReserved;
end;

function dxRGBQuadToColor(const ARGB: TRGBQuad): TColor;
var
  ATemp: TRGBA;
begin
  ATemp.B := ARGB.rgbBlue;
  ATemp.R := ARGB.rgbRed;
  ATemp.G := ARGB.rgbGreen;
  ATemp.A := ARGB.rgbReserved;
  Result := DWORD(ATemp);
end;

function cxGetBitmapPixelFormat(ABitmap: TBitmap): Integer;
const
  ABitCounts: array [pf1Bit..pf32Bit] of Byte = (1,4,8,16,16,24,32);
begin
  case ABitmap.PixelFormat of
    pf1bit..pf32Bit: Result := ABitCounts[ABitmap.PixelFormat]
  else
    Result := GetDeviceCaps(ABitmap.Canvas.Handle, BITSPIXEL);
  end;
end;

procedure dxFillBitmapInfoHeader(out AHeader: TBitmapInfoHeader; AWidth, AHeight: Integer; ATopDownDIB: WordBool);
begin
  cxZeroMemory(@AHeader, SizeOf(AHeader));
  AHeader.biSize := SizeOf(TBitmapInfoHeader);
  AHeader.biWidth := AWidth;
  if ATopDownDIB then
    AHeader.biHeight := -AHeight
  else
    AHeader.biHeight := AHeight;
  AHeader.biPlanes := 1;
  AHeader.biBitCount := 32;
  AHeader.biCompression := BI_RGB;
end;

procedure dxFillBitmapInfoHeader(out AHeader: TBitmapInfoHeader; ABitmap: TBitmap; ATopDownDIB: WordBool);
begin
  dxFillBitmapInfoHeader(AHeader, ABitmap.Width, ABitmap.Height, ATopDownDIB);
end;

function InternalGetDIB(ABitmap: TBitmap; const AColors: TRGBColors; ATopDownDIB: WordBool): Boolean;

  function GetStartScan(AIndex: Integer): Integer;
  begin
    if ATopDownDIB then
      Result := ABitmap.Height - 1 - AIndex
    else
      Result := AIndex;
  end;

var
  ADC: HDC;
  ABitmapInfo: TBitmapInfo;
  I: Integer;
  AScanLineResult: boolean;
  P: Pointer;
begin
  if (ABitmap.Width <> 0) and (ABitmap.Height <> 0) then
  begin
    dxFillBitmapInfoHeader(ABitmapInfo.bmiHeader, ABitmap, ATopDownDIB);
    ADC := CreateCompatibleDC(0);
    try
      Result := GetDIBits(ADC, ABitmap.Handle, 0, ABitmap.Height, AColors, ABitmapInfo, DIB_RGB_COLORS) <> 0;
      if not Result then
      begin
        Result := True;
        for I := 0 to ABitmap.Height - 1 do
        begin
          AScanLineResult := GetDIBits(ADC, ABitmap.Handle, GetStartScan(I), 1, @AColors[ABitmap.Width * I], ABitmapInfo, DIB_RGB_COLORS) <> 0;
          if not AScanLineResult then
          begin
            P := cxAllocMem(ABitmap.Width * SizeOf(TRGBQuad));
            try
              AScanLineResult := GetDIBits(ADC, ABitmap.Handle, GetStartScan(I), 1, P, ABitmapInfo, DIB_RGB_COLORS) <> 0;
              cxCopyData(P, @AColors[ABitmap.Width * I], ABitmap.Width * SizeOf(TRGBQuad));
            finally
              cxFreeMem(P);
            end;
          end;
          Result := Result and AScanLineResult;
        end;
      end;
    finally
      DeleteDC(ADC);
    end;
  end
  else
    Result := False;
end;

function InternalSetDIB(ABitmap: TBitmap; const AColors: TRGBColors; ATopDownDIB: WordBool): Boolean;
var
  ADC: HDC;
  ABitmapInfo: TBitmapInfo;
begin
  if (ABitmap.Width <> 0) and (ABitmap.Height <> 0) then
  begin
    dxFillBitmapInfoHeader(ABitmapInfo.bmiHeader, ABitmap, ATopDownDIB);
    ADC := CreateCompatibleDC(0);
    try
      Result := SetDIBits(ADC, ABitmap.Handle, 0, ABitmap.Height, AColors, ABitmapInfo, DIB_RGB_COLORS) <> 0;
    finally
      DeleteDC(ADC);
    end;
  end
  else
    Result := False;
end;

function GetBitmapBits(ABitmap: TBitmap; var AColors: TRGBColors; ATopDownDIB: Boolean): Boolean;
begin
  SetLength(AColors, ABitmap.Width * ABitmap.Height);
  Result := InternalGetDIB(ABitmap, AColors, ATopDownDIB);
end;

procedure GetBitmapBitsByScanLine(ABitmap: TBitmap; var AColors: TRGBColors);
var
  AIndex: Integer;
  AQuad: PRGBQuad;
  I, J: Integer;
begin
  // todo: try to get bitmap bits if GetDIBits fail
  if ABitmap.PixelFormat = pf32bit then
  begin
    if Length(AColors) <> ABitmap.Width * ABitmap.Height then
      SetLength(AColors, ABitmap.Width * ABitmap.Height);
    AIndex := 0;
    for J := 0 to ABitmap.Height - 1 do
    begin
      AQuad := ABitmap.ScanLine[J];
      for I := 0 to ABitmap.Width - 1 do
      begin
        AColors[AIndex] := AQuad^;
        Inc(AQuad);
        Inc(AIndex);
      end;
    end;
  end;
end;

procedure SetBitmapBits(ABitmap: TBitmap; const AColors: TRGBColors; ATopDownDIB: Boolean);
begin
  InternalSetDIB(ABitmap, AColors, ATopDownDIB);
end;

function dxGetAlphaState(const AColors: TRGBColors): TdxAlphaState;
var
  AAlpha, I: Integer;
  AHasNullValues: Boolean;
  AHasOpaqueValues: Boolean;
  AHasTransparentValues: Boolean;
begin
  AHasNullValues := False;
  AHasOpaqueValues := False;
  AHasTransparentValues := False;
  for I := Low(AColors) to High(AColors) do
  begin
    AAlpha := AColors[I].rgbReserved;
    AHasNullValues := AHasNullValues or (AAlpha = 0);
    AHasOpaqueValues := AHasOpaqueValues or (AAlpha = $FF);
    AHasTransparentValues := AHasTransparentValues or ((AAlpha > 0) and (AAlpha < $FF));
    if AHasTransparentValues or AHasNullValues and AHasOpaqueValues then
      Break;
  end;

  if AHasTransparentValues or AHasNullValues and AHasOpaqueValues then
    Result := asSemitransparent
  else
    if AHasOpaqueValues then
      Result := asOpaque
    else
      Result := asNoAlpha;
end;

function dxGetAlphaState(const ABitmap: TBitmap): TdxAlphaState;
var
  AColors: TRGBColors;
begin
  if cxGetBitmapPixelFormat(ABitmap) >= 32 then
  begin
    GetBitmapBits(ABitmap, AColors, False);
    Result := dxGetAlphaState(AColors);
  end
  else
    Result := asNoAlpha;
end;

procedure dxChangeColor(AColors: PRGBQuad; ACount: Integer; AColor: TColor);
var
  R, G, B, A: Byte;
begin
  AColor := ColorToRGB(AColor);
  R := GetRValue(AColor);
  G := GetGValue(AColor);
  B := GetBValue(AColor);

  while ACount > 0 do
  begin
    A := AColors^.rgbReserved;
    AColors^.rgbBlue := MulDiv(B, A, MaxByte);
    AColors^.rgbGreen := MulDiv(G, A, MaxByte);
    AColors^.rgbRed := MulDiv(R, A, MaxByte);
    Inc(AColors);
    Dec(ACount);
  end;
end;

function dxIsAlphaUsed(ABitmap: TBitmap): Boolean;
begin
  Result := dxGetAlphaState(ABitmap) <> asNoAlpha;
end;

function dxGetColorPaletteID(const AIntf: IdxColorPalette): TGUID;
begin
  if AIntf <> nil then
    Result := AIntf.GetID
  else
    ZeroMemory(@Result, SizeOf(Result));
end;

// getting graphic objects' data
function dxGetBitmapData(ABitmapHandle: HBITMAP; out ABitmapData: Windows.TBitmap): Boolean;
begin
  Result := GetObject(ABitmapHandle, SizeOf(Windows.TBitmap), @ABitmapData) <> 0;
end;

function dxGetBrushData(ABrushHandle: HBRUSH; out ALogBrush: TLogBrush): Boolean;
begin
  Result := GetObject(ABrushHandle, SizeOf(TLogBrush), @ALogBrush) <> 0;
end;

function dxGetBrushData(ABrushHandle: HBRUSH): TLogBrush;
begin
  dxGetBrushData(ABrushHandle, Result);
end;

function dxGetFontData(AFontHandle: HFONT; out ALogFont: TLogFont): Boolean;
begin
  Result := GetObject(AFontHandle, SizeOf(TLogFont), @ALogFont) <> 0;
end;

function dxGetFontData(AFontHandle: HFONT): TLogFont;
begin
  dxGetFontData(AFontHandle, Result);
end;

function dxGetPenData(APenHandle: HPEN; out ALogPen: TLogPen): Boolean;
begin
  Result := GetObject(APenHandle, SizeOf(TLogPen), @ALogPen) <> 0;
end;

function dxGraphicIsEquals(AGraphic1, AGraphic2: TGraphic): Boolean;
var
  S1, S2: TMemoryStream;
begin
  Result := (AGraphic1 <> nil) and (AGraphic2 <> nil) and (AGraphic1.ClassType = AGraphic2.ClassType) and
    (AGraphic1.Width = AGraphic2.Width) and (AGraphic1.Height = AGraphic2.Height);
  if Result then
  begin
    S1 := TMemoryStream.Create;
    S2 := TMemoryStream.Create;
    try
      AGraphic1.SaveToStream(S1);
      AGraphic2.SaveToStream(S2);
      Result := (S1.Size = S2.Size) and CompareMem(S1.Memory, S2.Memory, S1.Size);
    finally
      S1.Free;
      S2.Free;
    end;
  end;
end;

{ TdxAlphaColors.TUpdateSystemColorsListener }

constructor TdxAlphaColors.TUpdateSystemColorsListener.Create;
begin
  inherited Create;
  FHandle := AllocateHWnd(WndProc);
end;

destructor TdxAlphaColors.TUpdateSystemColorsListener.Destroy;
begin
  DeallocateHWnd(FHandle);
  inherited Destroy;
end;

procedure TdxAlphaColors.TUpdateSystemColorsListener.WndProc(var Message: TMessage);
begin
  if Message.Msg = WM_SYSCOLORCHANGE then
    TdxAlphaColors.UpdateSystemColors;
  Message.Result := DefWindowProc(FHandle, Message.Msg, Message.wParam, Message.lParam);
end;

{ TdxAlphaColors }

{$IFDEF DELPHIXE}
class constructor TdxAlphaColors.Initialize;
{$ELSE}
class procedure TdxAlphaColors.Initialize;
{$ENDIF}
begin
  FColorToNameMap := TList<TColorInfo>.Create;
  FColorToNameMap.Capacity := 140 + 26;
  CreateColors;
  CreateHtmlColors;
  FSystemColorsListener := TUpdateSystemColorsListener.Create;
  UpdateSystemColors;
end;

{$IFDEF DELPHIXE}
class destructor TdxAlphaColors.Finalize;
{$ELSE}
class procedure TdxAlphaColors.Finalize;
{$ENDIF}
begin
  FSystemColorsListener.Free;
  FNameToColorMap.Free;
  FHtmlNameToColorMap.Free;
  FColorToNameMap.Free;
end;

class function TdxAlphaColors.GetSystemAlphaColor(AColor: TSystemColor): TdxAlphaColor;
var
  ATemp: TRGBA;
begin
  DWORD(ATemp) := GetSysColor(SystemColorsData[AColor].Index);
  Result := FromArgb(255, ATemp.R, ATemp.G, ATemp.B);
end;

class procedure TdxAlphaColors.UpdateSystemColors;
var
  I: TSystemColor;
  AColor: TdxAlphaColor;
begin
  for I := Low(TSystemColor) to High(TSystemColor) do
  begin
    AColor := GetSystemAlphaColor(I);
    FSystemColors[I] := AColor;
    FNameToColorMap.AddOrSetValue(LowerCase(SystemColorsData[I].Name), AColor);
  end;
  UpdateHtmlSystemColors;
end;

class procedure TdxAlphaColors.UpdateHtmlSystemColors;
begin
  FHtmlNameToColorMap['activeborder'] := ActiveBorder;
  FHtmlNameToColorMap['activecaption'] := ActiveCaption;
  FHtmlNameToColorMap['appworkspace'] := AppWorkspace;
  FHtmlNameToColorMap['background'] := Desktop;
  FHtmlNameToColorMap['buttonface'] := Control;
  FHtmlNameToColorMap['buttonhighlight'] := ControlLightLight;
  FHtmlNameToColorMap['buttonshadow'] := ControlDark;
  FHtmlNameToColorMap['buttontext'] := ControlText;
  FHtmlNameToColorMap['captiontext'] := ActiveCaptionText;
  FHtmlNameToColorMap['graytext'] := GrayText;
  FHtmlNameToColorMap['highlight'] := Highlight;
  FHtmlNameToColorMap['highlighttext'] := HighlightText;
  FHtmlNameToColorMap['inactiveborder'] := InactiveBorder;
  FHtmlNameToColorMap['inactivecaption'] := InactiveCaption;
  FHtmlNameToColorMap['inactivecaptiontext'] := InactiveCaptionText;
  FHtmlNameToColorMap['infobackground'] := Info;
  FHtmlNameToColorMap['infotext'] := InfoText;
  FHtmlNameToColorMap['menu'] := Menu;
  FHtmlNameToColorMap['menutext'] := MenuText;
  FHtmlNameToColorMap['scrollbar'] := ScrollBar;
  FHtmlNameToColorMap['threeddarkshadow'] := ControlDarkDark;
  FHtmlNameToColorMap['threedface'] := Control;
  FHtmlNameToColorMap['threedhighlight'] := ControlLight;
  FHtmlNameToColorMap['threedlightshadow'] := ControlLightLight;
  FHtmlNameToColorMap['window'] := Window;
  FHtmlNameToColorMap['windowframe'] := WindowFrame;
  FHtmlNameToColorMap['windowtext'] := WindowText;
end;

class procedure TdxAlphaColors.AddNamedColor(const AName: string; AColor: TdxAlphaColor);
var
  AInfo: TColorInfo;
begin
  FNameToColorMap.Add(LowerCase(AName), AColor);
  AInfo.Color := AColor;
  AInfo.Name := AName;
  FColorToNameMap.Add(AInfo);
end;

class procedure TdxAlphaColors.CreateColors;
begin
  FNameToColorMap := TDictionary<string, TdxAlphaColor>.Create(140 + 28);
  AddNamedColor('AliceBlue', AliceBlue);
  AddNamedColor('AntiqueWhite', AntiqueWhite);
  AddNamedColor('Aqua', Aqua);
  AddNamedColor('Aquamarine', Aquamarine);
  AddNamedColor('Azure', Azure);
  AddNamedColor('Beige', Beige);
  AddNamedColor('Bisque', Bisque);
  AddNamedColor('Black', Black);
  AddNamedColor('BlanchedAlmond', BlanchedAlmond);
  AddNamedColor('Blue', Blue);
  AddNamedColor('BlueViolet', BlueViolet);
  AddNamedColor('Brown', Brown);
  AddNamedColor('BurlyWood', BurlyWood);
  AddNamedColor('CadetBlue', CadetBlue);
  AddNamedColor('Chartreuse', Chartreuse);
  AddNamedColor('Chocolate', Chocolate);
  AddNamedColor('Coral', Coral);
  AddNamedColor('CornflowerBlue', CornflowerBlue);
  AddNamedColor('Cornsilk', Cornsilk);
  AddNamedColor('Crimson', Crimson);
  AddNamedColor('Cyan', Cyan);
  AddNamedColor('DarkBlue', DarkBlue);
  AddNamedColor('DarkCyan', DarkCyan);
  AddNamedColor('DarkGoldenrod', DarkGoldenrod);
  AddNamedColor('DarkGray', DarkGray);
  AddNamedColor('DarkGreen', DarkGreen);
  AddNamedColor('DarkKhaki', DarkKhaki);
  AddNamedColor('DarkMagenta', DarkMagenta);
  AddNamedColor('DarkOliveGreen', DarkOliveGreen);
  AddNamedColor('DarkOrange', DarkOrange);
  AddNamedColor('DarkOrchid', DarkOrchid);
  AddNamedColor('DarkRed', DarkRed);
  AddNamedColor('DarkSalmon', DarkSalmon);
  AddNamedColor('DarkSeaGreen', DarkSeaGreen);
  AddNamedColor('DarkSlateBlue', DarkSlateBlue);
  AddNamedColor('DarkSlateGray', DarkSlateGray);
  AddNamedColor('DarkTurquoise', DarkTurquoise);
  AddNamedColor('DarkViolet', DarkViolet);
  AddNamedColor('DeepPink', DeepPink);
  AddNamedColor('DeepSkyBlue', DeepSkyBlue);
  AddNamedColor('DimGray', DimGray);
  AddNamedColor('DodgerBlue', DodgerBlue);
  AddNamedColor('Firebrick', Firebrick);
  AddNamedColor('FloralWhite', FloralWhite);
  AddNamedColor('ForestGreen', ForestGreen);
  AddNamedColor('Fuchsia', Fuchsia);
  AddNamedColor('Gainsboro', Gainsboro);
  AddNamedColor('GhostWhite', GhostWhite);
  AddNamedColor('Gold', Gold);
  AddNamedColor('Goldenrod', Goldenrod);
  AddNamedColor('Gray', Gray);
  AddNamedColor('Grey', Gray);
  AddNamedColor('Green', Green);
  AddNamedColor('GreenYellow', GreenYellow);
  AddNamedColor('Honeydew', Honeydew);
  AddNamedColor('HotPink', HotPink);
  AddNamedColor('IndianRed', IndianRed);
  AddNamedColor('Indigo', Indigo);
  AddNamedColor('Ivory', Ivory);
  AddNamedColor('Khaki', Khaki);
  AddNamedColor('Lavender', Lavender);
  AddNamedColor('LavenderBlush', LavenderBlush);
  AddNamedColor('LawnGreen', LawnGreen);
  AddNamedColor('LemonChiffon', LemonChiffon);
  AddNamedColor('LightBlue', LightBlue);
  AddNamedColor('LightCoral', LightCoral);
  AddNamedColor('LightCyan', LightCyan);
  AddNamedColor('LightGoldenrodYellow', LightGoldenrodYellow);
  AddNamedColor('LightGray', LightGray);
  AddNamedColor('LightGreen', LightGreen);
  AddNamedColor('LightPink', LightPink);
  AddNamedColor('LightSalmon', LightSalmon);
  AddNamedColor('LightSeaGreen', LightSeaGreen);
  AddNamedColor('LightSkyBlue', LightSkyBlue);
  AddNamedColor('LightSlateGray', LightSlateGray);
  AddNamedColor('LightSteelBlue', LightSteelBlue);
  AddNamedColor('LightYellow', LightYellow);
  AddNamedColor('Lime', Lime);
  AddNamedColor('LimeGreen', LimeGreen);
  AddNamedColor('Linen', Linen);
  AddNamedColor('Magenta', Magenta);
  AddNamedColor('Maroon', Maroon);
  AddNamedColor('MediumAquamarine', MediumAquamarine);
  AddNamedColor('MediumBlue', MediumBlue);
  AddNamedColor('MediumOrchid', MediumOrchid);
  AddNamedColor('MediumPurple', MediumPurple);
  AddNamedColor('MediumSeaGreen', MediumSeaGreen);
  AddNamedColor('MediumSlateBlue', MediumSlateBlue);
  AddNamedColor('MediumSpringGreen', MediumSpringGreen);
  AddNamedColor('MediumTurquoise', MediumTurquoise);
  AddNamedColor('MediumVioletRed', MediumVioletRed);
  AddNamedColor('MidnightBlue', MidnightBlue);
  AddNamedColor('MintCream', MintCream);
  AddNamedColor('MistyRose', MistyRose);
  AddNamedColor('Moccasin', Moccasin);
  AddNamedColor('NavajoWhite', NavajoWhite);
  AddNamedColor('Navy', Navy);
  AddNamedColor('OldLace', OldLace);
  AddNamedColor('Olive', Olive);
  AddNamedColor('OliveDrab', OliveDrab);
  AddNamedColor('Orange', Orange);
  AddNamedColor('OrangeRed', OrangeRed);
  AddNamedColor('Orchid', Orchid);
  AddNamedColor('PaleGoldenrod', PaleGoldenrod);
  AddNamedColor('PaleGreen', PaleGreen);
  AddNamedColor('PaleTurquoise', PaleTurquoise);
  AddNamedColor('PaleVioletRed', PaleVioletRed);
  AddNamedColor('PapayaWhip', PapayaWhip);
  AddNamedColor('PeachPuff', PeachPuff);
  AddNamedColor('Peru', Peru);
  AddNamedColor('Pink', Pink);
  AddNamedColor('Plum', Plum);
  AddNamedColor('PowderBlue', PowderBlue);
  AddNamedColor('Purple', Purple);
  AddNamedColor('Red', Red);
  AddNamedColor('RosyBrown', RosyBrown);
  AddNamedColor('RoyalBlue', RoyalBlue);
  AddNamedColor('SaddleBrown', SaddleBrown);
  AddNamedColor('Salmon', Salmon);
  AddNamedColor('SandyBrown', SandyBrown);
  AddNamedColor('SeaGreen', SeaGreen);
  AddNamedColor('SeaShell', SeaShell);
  AddNamedColor('Sienna', Sienna);
  AddNamedColor('Silver', Silver);
  AddNamedColor('SkyBlue', SkyBlue);
  AddNamedColor('SlateBlue', SlateBlue);
  AddNamedColor('SlateGray', SlateGray);
  AddNamedColor('Snow', Snow);
  AddNamedColor('SpringGreen', SpringGreen);
  AddNamedColor('SteelBlue', SteelBlue);
  AddNamedColor('Tan', Tan);
  AddNamedColor('Teal', Teal);
  AddNamedColor('Thistle', Thistle);
  AddNamedColor('Tomato', Tomato);
  AddNamedColor('Turquoise', Turquoise);
  AddNamedColor('Violet', Violet);
  AddNamedColor('Wheat', Wheat);
  AddNamedColor('White', White);
  AddNamedColor('WhiteSmoke', WhiteSmoke);
  AddNamedColor('Yellow', Yellow);
  AddNamedColor('YellowGreen', YellowGreen);
end;

class procedure TdxAlphaColors.CreateHtmlColors;
begin
  FHtmlNameToColorMap := TDictionary<string, TdxAlphaColor>.Create(28);
  FHtmlNameToColorMap.Add('activeborder', ActiveBorder);
  FHtmlNameToColorMap.Add('activecaption', ActiveCaption);
  FHtmlNameToColorMap.Add('appworkspace', AppWorkspace);
  FHtmlNameToColorMap.Add('background', Desktop);
  FHtmlNameToColorMap.Add('buttonface', Control);
  FHtmlNameToColorMap.Add('buttonhighlight', ControlLightLight);
  FHtmlNameToColorMap.Add('buttonshadow', ControlDark);
  FHtmlNameToColorMap.Add('buttontext', ControlText);
  FHtmlNameToColorMap.Add('captiontext', ActiveCaptionText);
  FHtmlNameToColorMap.Add('graytext', GrayText);
  FHtmlNameToColorMap.Add('highlight', Highlight);
  FHtmlNameToColorMap.Add('highlighttext', HighlightText);
  FHtmlNameToColorMap.Add('inactiveborder', InactiveBorder);
  FHtmlNameToColorMap.Add('inactivecaption', InactiveCaption);
  FHtmlNameToColorMap.Add('inactivecaptiontext', InactiveCaptionText);
  FHtmlNameToColorMap.Add('infobackground', Info);
  FHtmlNameToColorMap.Add('infotext', InfoText);
  FHtmlNameToColorMap.Add('menu', Menu);
  FHtmlNameToColorMap.Add('menutext', MenuText);
  FHtmlNameToColorMap.Add('scrollbar', ScrollBar);
  FHtmlNameToColorMap.Add('threeddarkshadow', ControlDarkDark);
  FHtmlNameToColorMap.Add('threedface', Control);
  FHtmlNameToColorMap.Add('threedhighlight', ControlLight);
  FHtmlNameToColorMap.Add('threedlightshadow', ControlLightLight);
  FHtmlNameToColorMap.Add('window', Window);
  FHtmlNameToColorMap.Add('windowframe', WindowFrame);
  FHtmlNameToColorMap.Add('windowtext', WindowText);
  FHtmlNameToColorMap.Add('lightgrey', LightGray);
end;

class function TdxAlphaColors.Blend(AColor, ABackgroundColor: TdxAlphaColor): TdxAlphaColor;
var
  Alpha, AOneAlpha: Single;
begin
  if dxGetAlpha(AColor) = 255 then
    Result := AColor
  else
  begin
    Alpha := dxGetAlpha(AColor) / 255;
    AOneAlpha := 1 - Alpha;

    Result := FromArgb(
      Round(R(AColor) * Alpha + R(ABackgroundColor) * AOneAlpha),
      Round(G(AColor) * Alpha + G(ABackgroundColor) * AOneAlpha),
      Round(B(AColor) * Alpha + B(ABackgroundColor) * AOneAlpha));
  end;
end;

class function TdxAlphaColors.CalculateNearestColor(const AColorsToChooseFrom: array of TdxAlphaColor; AValue: TdxAlphaColor): TdxAlphaColor;
var
  AValueHue, AValueBrightness, AValueSaturation, AMinDistance, AHue, ASaturation, ABrightness, ADistance: Single;
  AColor: TdxAlphaColor;
begin
  AValueHue := GetHue(AValue);
  AValueBrightness := GetBrightness(AValue);
  AValueSaturation := GetSaturation(AValue);
  Result := Empty;
  AMinDistance := MaxSingle;
  for AColor in AColorsToChooseFrom do
  begin
    AHue := Abs(GetHue(AColor) - AValueHue);
    if AHue > 180.0 then
      AHue := 360.0 - AHue;
    ASaturation := GetSaturation(AColor) - AValueSaturation;
    ABrightness := GetBrightness(AColor) - AValueBrightness;
    ADistance := AHue * AHue + ASaturation * ASaturation + ABrightness * ABrightness;
    if ADistance < AMinDistance then
    begin
      AMinDistance := ADistance;
      Result := AColor;
    end;
  end;
end;

class function TdxAlphaColors.ChangeBrightness(AColor: TdxAlphaColor; ABrightness: Single): TdxAlphaColor;
var
  H, S, L: Single;
begin
  Assert((ABrightness >= -1) and (ABrightness <= 1), '');
  if IsZero(ABrightness) then
    Exit(AColor);
  ToHSL(AColor, H, S, L);
  if ABrightness > 0 then
    L := L + (1 - L) *  ABrightness
  else
    L := L + L * ABrightness;
  Result := FromArgb(TAlphaColorChannelMap(AColor).Alpha, FromHSL(H, S, Min(1, Max(0, L))));
end;

class function TdxAlphaColors.GetBrightness(AColor: TdxAlphaColor): Single;
var
  ARedF, AGreenF, ABlueF, AMax, AMin: Single;
begin
  ARedF   := R(AColor) / 255.0;
  AGreenF := G(AColor) / 255.0;
  ABlueF  := B(AColor) / 255.0;

  AMax := ARedF;
  AMin := ARedF;

  if AGreenF > AMax then
    AMax := AGreenF;
  if ABlueF > AMax then
    AMax := ABlueF;

  if AGreenF < AMin then
    AMin := AGreenF;
  if ABlueF < AMin then
    AMin := ABlueF;

  Result := (AMax + AMin) / 2;
end;

class function TdxAlphaColors.GetHue(AColor: TdxAlphaColor): Single;
var
  ARed, AGreen, ABlue: Byte;
  ARedF, AGreenF, ABlueF, AMax, AMin, ADelta: Single;
begin
  ARed   := R(AColor);
  AGreen := G(AColor);
  ABlue  := B(AColor);
  if (ARed = AGreen) and (AGreen = ABlue) then
    Exit(0);

  ARedF   := ARed   / 255.0;
  AGreenF := AGreen / 255.0;
  ABlueF  := ABlue  / 255.0;

  Result := 0.0;

  AMax := ARedF;
  AMin := ARedF;

  if AGreenF > AMax then
    AMax := AGreenF;
  if ABlueF > AMax then
    AMax := ABlueF;

  if AGreenF < AMin then
    AMin := AGreenF;
  if ABlueF < AMin then
    AMin := ABlueF;

  ADelta := AMax - AMin;

  if ARedF = AMax then
    Result := (AGreenF - ABlueF) / ADelta
  else
    if AGreenF = AMax then
      Result := 2 + (ABlueF - ARedF) / ADelta
    else
      if ABlueF = AMax then
        Result := 4 + (ARedF - AGreenF) / ADelta;

  Result := Result * 60;

  if Result < 0.0 then
    Result := Result + 360.0;
end;

class function TdxAlphaColors.GetSaturation(AColor: TdxAlphaColor): Single;
var
  ARedF, AGreenF, ABlueF, AMax, AMin, L: Single;
begin
  ARedF   := R(AColor) / 255.0;
  AGreenF := G(AColor) / 255.0;
  ABlueF  := B(AColor) / 255.0;

  Result := 0;

  AMax := ARedF;
  AMin := ARedF;

  if AGreenF > AMax then
    AMax := AGreenF;
  if ABlueF > AMax then
    AMax := ABlueF;

  if AGreenF < AMin then
    AMin := AGreenF;
  if ABlueF < AMin then
    AMin := ABlueF;
  if AMax <> AMin then
  begin
    L := (AMax + AMin) / 2;

    if L <= 0.5 then
      Result := (AMax - AMin) / (AMax + AMin)
    else
      Result := (AMax - AMin) / (2 - AMax - AMin);
  end;
end;

class function TdxAlphaColors.IsEmpty(AColor: TdxAlphaColor): Boolean;
begin
  Result := AColor = Empty;
end;

class function TdxAlphaColors.IsTransparentOrEmpty(const AColor: TdxAlphaColor): Boolean;
begin
  Result := (AColor = Empty) or (AColor = Transparent);
end;

class function TdxAlphaColors.FromArgb(ARed, AGreen, ABlue: Byte): TdxAlphaColor;
begin
  Result := FromArgb(255, ARed, AGreen, ABlue);
end;

class function TdxAlphaColors.FromArgb(AAlpha, ARed, AGreen, ABlue: Byte): TdxAlphaColor;
begin
  Result := dxMakeAlphaColor(AAlpha, ARed, AGreen, ABlue);
end;

class function TdxAlphaColors.FromArgb(AAlpha: Byte; ABaseColor: TdxAlphaColor): TdxAlphaColor;
begin
  Result := ABaseColor;
  TAlphaColorChannelMap(Result).Alpha := AAlpha;
end;

class function TdxAlphaColors.FromColor(const AColor: TColor): TdxAlphaColor;
begin
  Result := dxColorToAlphaColor(AColor);
end;

class function TdxAlphaColors.FromHSL(H, S, L: Single): TdxAlphaColor;

  function HueToColor(const AHue, M1, M2: Double): Byte;
  var
    AValue, AHue6: Double;
  begin
    AHue6 := 6 * (AHue - Floor(AHue));
    if AHue6 < 1 then
      AValue := M1 + (M2 - M1) * AHue6
    else

    if AHue6 < 3 then
      AValue := M2
    else

    if AHue6 < 4 then
      AValue := M1 + (M2 - M1) * (4 - AHue6)
    else
      AValue := M1;

    Result := Round(255 * AValue);
  end;

var
  M1, M2: Double;
  I: Byte;
begin
  if S = 0 then
  begin
    I := Round(255 * L);
    TAlphaColorChannelMap(Result).R := I;
    TAlphaColorChannelMap(Result).G := I;
    TAlphaColorChannelMap(Result).B := I;
  end
  else
  begin
    if L <= 0.5 then
      M2 := L * (1 + S)
    else
      M2 := L * (1 - S) + S;

    M1 := 2 * L - M2;
    TAlphaColorChannelMap(Result).R := HueToColor(H + 1 / 3, M1, M2);
    TAlphaColorChannelMap(Result).G := HueToColor(H, M1, M2);
    TAlphaColorChannelMap(Result).B := HueToColor(H - 1 / 3, M1, M2);
  end;
  TAlphaColorChannelMap(Result).Alpha := 255;
end;

class function TdxAlphaColors.ToArgb(const AColor: TdxAlphaColor): Cardinal;
begin
  TRGBA(Result).R := TAlphaColorChannelMap(AColor).R;
  TRGBA(Result).G := TAlphaColorChannelMap(AColor).G;
  TRGBA(Result).B := TAlphaColorChannelMap(AColor).B;
  TRGBA(Result).A := TAlphaColorChannelMap(AColor).Alpha;
end;

class function TdxAlphaColors.ToColor(const AColor: TdxAlphaColor): TColor;
begin
  Result := dxAlphaColorToColor(AColor);
end;

class procedure TdxAlphaColors.ToHSL(AColor: TdxAlphaColor; out H, S, L: Single);
var
  R, G, B, D, CMax, CMin: Double;
begin
  R := TAlphaColorChannelMap(AColor).R / 255.0;
  G := TAlphaColorChannelMap(AColor).G / 255.0;
  B := TAlphaColorChannelMap(AColor).B / 255.0;

  CMax := Max(R, Max(G, B));
  CMin := Min(R, Min(G, B));
  L := (CMax + CMin) / 2;

  D := CMax - CMin;
  if D <> 0 then
  begin
    if L < 0.5 then
      S := D / (CMax + CMin)
    else
      S := D / (2 - CMax - CMin);

    if R = CMax then
      H := (G - B) / D
    else
      if G = Cmax then
        H := 2 + (B - R) / D
      else
        H := 4 + (R - G) / D;

    H := H / 6;
    if H < 0 then
      H := H + 1;
  end
  else
  begin
    S := 0;
    H := 0;
  end;
end;

class function TdxAlphaColors.R(AColor: TdxAlphaColor): Byte;
begin
  Result := TAlphaColorChannelMap(AColor).R;
end;

class function TdxAlphaColors.G(AColor: TdxAlphaColor): Byte;
begin
  Result := TAlphaColorChannelMap(AColor).G;
end;

class function TdxAlphaColors.B(AColor: TdxAlphaColor): Byte;
begin
  Result := TAlphaColorChannelMap(AColor).B;
end;

class function TdxAlphaColors.Alpha(AColor: TdxAlphaColor): Byte;
begin
  Result := TAlphaColorChannelMap(AColor).Alpha;
end;

class function TdxAlphaColors.GetColorName(AColor: TdxAlphaColor): string;
var
  I: Integer;
begin
  for I := 0 to FColorToNameMap.Count - 1 do
    if FColorToNameMap[I].Color = AColor then
    begin
      Result := FColorToNameMap[I].Name;
      Exit;
    end;
  Result := '';
end;

class function TdxAlphaColors.IsKnownColor(AColor: TdxAlphaColor): Boolean;
begin
  Result := FNameToColorMap.ContainsValue(AColor);
end;

class function TdxAlphaColors.FromName(const AName: string): TdxAlphaColor;
begin
  if not FNameToColorMap.TryGetValue(LowerCase(AName), Result) then
    Result := Empty;
end;

class function TdxAlphaColors.FromHexCode(const AHexCode: string): TdxAlphaColor;
begin
  case Length(AHexCode) of
    6: Result := FromArgb(
        StrToInt('$' + Copy(AHexCode, 1, 2)),
        StrToInt('$' + Copy(AHexCode, 3, 2)),
        StrToInt('$' + Copy(AHexCode, 5, 2)));
    7: Result := FromArgb(
        StrToInt('$' + Copy(AHexCode, 2, 2)),
        StrToInt('$' + Copy(AHexCode, 4, 2)),
        StrToInt('$' + Copy(AHexCode, 6, 2)));
    8: Result := FromArgb(
        StrToInt('$' + Copy(AHexCode, 1, 2)),
        StrToInt('$' + Copy(AHexCode, 3, 2)),
        StrToInt('$' + Copy(AHexCode, 5, 2)),
        StrToInt('$' + Copy(AHexCode, 7, 2)));
    9: Result := FromArgb(
        StrToInt('$' + Copy(AHexCode, 2, 2)),
        StrToInt('$' + Copy(AHexCode, 4, 2)),
        StrToInt('$' + Copy(AHexCode, 6, 2)),
        StrToInt('$' + Copy(AHexCode, 8, 2)));
  else
    Result := Empty;
  end;
end;

class function TdxAlphaColors.FromHtml(const AHtmlColor: string): TdxAlphaColor;
var
  AColorName: string;
begin
  Result := Empty;
  if AHtmlColor = '' then
    Exit;
  if (AHtmlColor[1] = '#') and ((Length(AHtmlColor) = 7) or (Length(AHtmlColor) = 4)) then
  begin
    if Length(AHtmlColor) = 7 then
    begin
      Result := FromArgb(
        StrToInt('$' + Copy(AHtmlColor, 2, 2)),
        StrToInt('$' + Copy(AHtmlColor, 4, 2)),
        StrToInt('$' + Copy(AHtmlColor, 6, 2)));
    end
    else
    begin
      Result := FromArgb(
        StrToInt('$' + AHtmlColor[2] + AHtmlColor[2]),
        StrToInt('$' + AHtmlColor[3] + AHtmlColor[3]),
        StrToInt('$' + AHtmlColor[4] + AHtmlColor[4]));
    end;
  end;
  if IsEmpty(Result) then
  begin
    AColorName := LowerCase(AHtmlColor);
    if not FHtmlNameToColorMap.TryGetValue(AColorName, Result) then
      FNameToColorMap.TryGetValue(AColorName, Result);
  end;
end;

class function TdxAlphaColors.ToHexCode(AColor: TdxAlphaColor; AUseAlpha: Boolean; APrefix: Char = #0000): string;
begin
  if AUseAlpha then
    Result := Format('%.2x%.2x%.2x%.2x', [Alpha(AColor), R(AColor), G(AColor), B(AColor)])
  else
    Result := Format('%.2x%.2x%.2x', [R(AColor), G(AColor), B(AColor)]);
  if APrefix <> #0000 then
    Result := APrefix + Result;
end;

class function TdxAlphaColors.ToHtml(AColor: TdxAlphaColor; AUseNamedColors: Boolean = True): string;
begin
  Result := '';

  if IsEmpty(AColor) then
    Exit;

  if AUseNamedColors then
  begin
    if AColor = LightGray then
    begin
      Result := 'LightGrey';
      Exit;
    end;

    Result := GetColorName(AColor);
    if Result = '' then
    begin
      if AColor = ActiveBorder then
        Result := 'activeborder'
      else if (AColor = GradientActiveCaption) or (AColor = ActiveCaption) then
        Result := 'activecaption'
      else if AColor = AppWorkspace then
        Result := 'appworkspace'
      else if AColor = Desktop then
        Result := 'background'
      else if AColor = Control then
        Result := 'buttonface'
      else if AColor = ControlLight then
        Result := 'buttonface'
      else if AColor = ControlDark then
        Result := 'buttonshadow'
      else if AColor = ControlText then
        Result := 'buttontext'
      else if AColor = ActiveCaptionText then
        Result := 'captiontext'
      else if AColor = GrayText then
        Result := 'graytext'
      else if (AColor = HotTrack) and (AColor = Highlight) then
        Result := 'highlight'
      else if (AColor = MenuHighlight) and (AColor = HighlightText) then
        Result := 'highlighttext'
      else if AColor = InactiveBorder then
        Result := 'inactiveborder'
      else if (AColor = GradientInactiveCaption) and (AColor = InactiveCaption) then
        Result := 'inactivecaption'
      else if AColor = InactiveCaptionText then
        Result := 'inactivecaptiontext'
      else if AColor = Info then
        Result := 'infobackground'
      else if AColor = InfoText then
        Result := 'infotext'
      else if (AColor = MenuBar) and (AColor = Menu) then
        Result := 'menu'
      else if AColor = MenuText then
        Result := 'menutext'
      else if AColor = ScrollBar then
        Result := 'scrollbar'
      else if AColor = ControlDarkDark then
        Result := 'threeddarkshadow'
      else if AColor = ControlLightLight then
        Result := 'buttonhighlight'
      else if AColor = Window then
        Result := 'window'
      else if AColor = WindowFrame then
        Result := 'windowframe'
      else if AColor = WindowText then
        Result := 'windowtext'
      else
        Result := Format('#%.2x%.2x%.2x', [R(AColor), G(AColor), B(AColor)]);
    end
  end
  else
    Result := Format('#%.2x%.2x%.2x', [R(AColor), G(AColor), B(AColor)]);
end;

// system color aliases

class function TdxAlphaColors.ActiveBorder: TdxAlphaColor;
begin
  Result := FSystemColors[cActiveBorder];
end;

class function TdxAlphaColors.ActiveCaption: TdxAlphaColor;
begin
  Result := FSystemColors[cActiveCaption];
end;

class function TdxAlphaColors.ActiveCaptionText: TdxAlphaColor;
begin
  Result := FSystemColors[cActiveCaptionText];
end;

class function TdxAlphaColors.AppWorkSpace: TdxAlphaColor;
begin
  Result := FSystemColors[cAppWorkSpace];
end;

class function TdxAlphaColors.Background: TdxAlphaColor;
begin
  Result := FSystemColors[cDesktop];
end;

class function TdxAlphaColors.BtnFace: TdxAlphaColor;
begin
  Result := FSystemColors[cButtonFace];
end;

class function TdxAlphaColors.BtnHighlight: TdxAlphaColor;
begin
  Result := FSystemColors[cButtonHighlight];
end;

class function TdxAlphaColors.BtnShadow: TdxAlphaColor;
begin
  Result := FSystemColors[cButtonShadow];
end;

class function TdxAlphaColors.BtnText: TdxAlphaColor;
begin
  Result := FSystemColors[cControlText];
end;

class function TdxAlphaColors.ButtonFace: TdxAlphaColor;
begin
  Result := FSystemColors[cButtonFace];
end;

class function TdxAlphaColors.ButtonHighlight: TdxAlphaColor;
begin
  Result := FSystemColors[cButtonHighlight];
end;

class function TdxAlphaColors.ButtonShadow: TdxAlphaColor;
begin
  Result := FSystemColors[cButtonShadow];
end;

class function TdxAlphaColors.CaptionText: TdxAlphaColor;
begin
  Result := FSystemColors[cActiveCaptionText];
end;

class function TdxAlphaColors.Control: TdxAlphaColor;
begin
  Result := FSystemColors[cControl];
end;

class function TdxAlphaColors.ControlDark: TdxAlphaColor;
begin
  Result := FSystemColors[cControlDark];
end;

class function TdxAlphaColors.ControlDarkDark: TdxAlphaColor;
begin
  Result := FSystemColors[cControlDarkDark];
end;

class function TdxAlphaColors.ControlLight: TdxAlphaColor;
begin
  Result := FSystemColors[cControlLight];
end;

class function TdxAlphaColors.ControlLightLight: TdxAlphaColor;
begin
  Result := FSystemColors[cControlLightLight];
end;

class function TdxAlphaColors.ControlText: TdxAlphaColor;
begin
  Result := FSystemColors[cControlText];
end;

class function TdxAlphaColors.Desktop: TdxAlphaColor;
begin
  Result := FSystemColors[cDesktop];
end;

class function TdxAlphaColors.GradientActiveCaption: TdxAlphaColor;
begin
  Result := FSystemColors[cGradientActiveCaption];
end;

class function TdxAlphaColors.GradientInactiveCaption: TdxAlphaColor;
begin
  Result := FSystemColors[cGradientInactiveCaption];
end;

class function TdxAlphaColors.GrayText: TdxAlphaColor;
begin
  Result := FSystemColors[cGrayText];
end;

class function TdxAlphaColors.Highlight: TdxAlphaColor;
begin
  Result := FSystemColors[cHighlight];
end;

class function TdxAlphaColors.HighlightText: TdxAlphaColor;
begin
  Result := FSystemColors[cHighlightText];
end;

class function TdxAlphaColors.HotLight: TdxAlphaColor;
begin
  Result := FSystemColors[cHotTrack];
end;

class function TdxAlphaColors.HotTrack: TdxAlphaColor;
begin
  Result := FSystemColors[cHotTrack];
end;

class function TdxAlphaColors.InactiveBorder: TdxAlphaColor;
begin
  Result := FSystemColors[cInactiveBorder];
end;

class function TdxAlphaColors.InactiveCaption: TdxAlphaColor;
begin
  Result := FSystemColors[cInactiveCaption];
end;

class function TdxAlphaColors.InactiveCaptionText: TdxAlphaColor;
begin
  Result := FSystemColors[cInactiveCaptionText];
end;

class function TdxAlphaColors.Info: TdxAlphaColor;
begin
  Result := FSystemColors[cInfo];
end;

class function TdxAlphaColors.InfoBk: TdxAlphaColor;
begin
  Result := FSystemColors[cInfo];
end;

class function TdxAlphaColors.InfoText: TdxAlphaColor;
begin
  Result := FSystemColors[cInfoText];
end;

class function TdxAlphaColors._3DDkShadow: TdxAlphaColor;
begin
  Result := FSystemColors[cControlDarkDark];
end;

class function TdxAlphaColors._3DLight: TdxAlphaColor;
begin
  Result := FSystemColors[cControlLight];
end;

class function TdxAlphaColors.Menu: TdxAlphaColor;
begin
  Result := FSystemColors[cMenu];
end;

class function TdxAlphaColors.MenuBar: TdxAlphaColor;
begin
  Result := FSystemColors[cMenuBar];
end;

class function TdxAlphaColors.MenuHighlight: TdxAlphaColor;
begin
  Result := FSystemColors[cMenuHighlight];
end;

class function TdxAlphaColors.MenuText: TdxAlphaColor;
begin
  Result := FSystemColors[cMenuText];
end;

class function TdxAlphaColors.ScrollBar: TdxAlphaColor;
begin
  Result := FSystemColors[cScrollBar];
end;

class function TdxAlphaColors.Window: TdxAlphaColor;
begin
  Result := FSystemColors[cWindow];
end;

class function TdxAlphaColors.WindowFrame: TdxAlphaColor;
begin
  Result := FSystemColors[cWindowFrame];
end;

class function TdxAlphaColors.WindowText: TdxAlphaColor;
begin
  Result := FSystemColors[cWindowText];
end;

{ TdxCustomColorPalette }

procedure TdxCustomColorPalette.AfterConstruction;
begin
  inherited;
  CreateGUID(FID);
end;

function TdxCustomColorPalette.GetID: TGUID;
begin
  Result := FID;
end;

{ TdxSimpleColorPalette }

constructor TdxSimpleColorPalette.Create(AFillColor, AStrokeColor: TdxAlphaColor);
begin
  FFillColor := AFillColor;
  FStrokeColor := AStrokeColor;
end;

function TdxSimpleColorPalette.GetFillColor(const ID: string): TdxAlphaColor;
begin
  Result := FFillColor;
end;

function TdxSimpleColorPalette.GetStrokeColor(const ID: string): TdxAlphaColor;
begin
  Result := FStrokeColor;
end;

{ TdxAdvancedColorPalette }

constructor TdxAdvancedColorPalette.Create;
begin
  inherited Create;

  FFillColors := TDictionary<string, TdxAlphaColor>.Create;
  FFillColors.OnKeyNotify := HandlerKeyChanged;
  FFillColors.OnValueNotify := HandlerValueChanged;

  FStrokeColors := TDictionary<string, TdxAlphaColor>.Create;
  FStrokeColors.OnKeyNotify := HandlerKeyChanged;
  FStrokeColors.OnValueNotify := HandlerValueChanged;
end;

destructor TdxAdvancedColorPalette.Destroy;
begin
  FreeAndNil(FStrokeColors);
  FreeAndNil(FFillColors);
  inherited;
end;

function TdxAdvancedColorPalette.GetFillColor(const ID: string): TdxAlphaColor;
begin
  if not FFillColors.TryGetValue(ID, Result) then
    Result := TdxAlphaColors.Default;
end;

function TdxAdvancedColorPalette.GetStrokeColor(const ID: string): TdxAlphaColor;
begin
  if not FStrokeColors.TryGetValue(ID, Result) then
    Result := TdxAlphaColors.Default;
end;

procedure TdxAdvancedColorPalette.Changed;
begin
  CreateGUID(FID);
end;

procedure TdxAdvancedColorPalette.HandlerKeyChanged(Sender: TObject; const Item: string; Action: TCollectionNotification);
begin
  Changed;
end;

procedure TdxAdvancedColorPalette.HandlerValueChanged(Sender: TObject; const Item: TdxAlphaColor; Action: TCollectionNotification);
begin
  Changed;
end;

procedure TdxAdvancedColorPalette.SetFillColor(const ID: string; const Value: TdxAlphaColor);
begin
  if GetFillColor(ID) <> Value then
  begin
    if Value = TdxAlphaColors.Default then
      FFillColors.Remove(ID)
    else
      FFillColors.AddOrSetValue(ID, Value);
  end;
end;

procedure TdxAdvancedColorPalette.SetStrokeColor(const ID: string; const Value: TdxAlphaColor);
begin
  if GetStrokeColor(ID) <> Value then
  begin
    if Value = TdxAlphaColors.Default then
      FStrokeColors.Remove(ID)
    else
      FStrokeColors.AddOrSetValue(ID, Value);
  end;
end;

{ TdxCustomValueCacheManager<TKey, TValue> }

constructor TdxCustomValueCacheManager<TKey, TValue>.Create(ACapacity: Integer);
begin
  FCapacity := ACapacity;
  FData := TList<TPair<TKey, TValue>>.Create;
  FData.OnNotify := ValueHandler;
  FLastKeyIndex := -1;
end;

destructor TdxCustomValueCacheManager<TKey, TValue>.Destroy;
begin
  FreeAndNil(FData);
  inherited;
end;

procedure TdxCustomValueCacheManager<TKey, TValue>.BeforeDestruction;
begin
  inherited;
  Clear;
end;

procedure TdxCustomValueCacheManager<TKey, TValue>.Clear;
begin
  FLastKeyIndex := -1;
  FData.Count := 0;
end;

function TdxCustomValueCacheManager<TKey, TValue>.GetValue(const Key: TKey): TValue;
var
  AIndex: Integer;
begin
  if (FLastKeyIndex >= 0) and (Compare(FLastKey, Key) = 0) then
    AIndex := FLastKeyIndex
  else
  begin
    if not Find(Key, AIndex) then
    begin
      if FCapacity = FData.Count then
      begin
        if AIndex >= FData.Count then
          Dec(AIndex);
        FData.Delete(AIndex);
      end;
      FData.Insert(AIndex, TPair<TKey, TValue>.Create(Key, CreateValue(Key)));
    end;
    FLastKey := Key;
    FLastKeyIndex := AIndex;
  end;
  Result := FData{$IFDEF DELPHIXE3}.List{$ENDIF}[AIndex].Value
end;

procedure TdxCustomValueCacheManager<TKey, TValue>.DoRemove(const Value: TValue);
begin
  // do nothing
end;

function TdxCustomValueCacheManager<TKey, TValue>.Find(const Key: TKey; out AFoundIndex: Integer): Boolean;
var
  ACompareResult: Integer;
  AMiddle: Integer;
  ALow, AHigh: Integer;
begin
  Result := False;
  ALow := 0;
  AHigh := FData.Count - 1;
  while ALow <= AHigh do
  begin
    AMiddle := ALow + (AHigh - ALow) shr 1;
    ACompareResult := Compare(FData{$IFDEF DELPHIXE3}.List{$ENDIF}[AMiddle].Key, Key);
    if ACompareResult >= 0 then
    begin
      AHigh := AMiddle - 1;
      if ACompareResult = 0 then
        Result := True;
    end
    else
      ALow := AMiddle + 1;
  end;
  AFoundIndex := ALow;
end;

procedure TdxCustomValueCacheManager<TKey, TValue>.ValueHandler(
  Sender: TObject; const Item: TPair<TKey, TValue>; Action: TCollectionNotification);
begin
  if Action = cnRemoved then
    DoRemove(Item.Value);
end;

{ TdxGdiClippingHelper }

class procedure TdxGdiClippingHelper.AddClippingRectangle(DC: HDC; const R: TRect);
var
  ARegion: HRGN;
begin
  with R do
    ARegion := CreateRectRgn(Left, Top, Right, Bottom);
  ExtSelectClipRgn(DC, ARegion, RGN_OR);
  DeleteObject(ARegion);
end;

class procedure TdxGdiClippingHelper.AddRectangleToRegion(var ARegion: HRGN; const R: TRect);
var
  ATempRegion: HRGN;
begin
  ATempRegion := CreateRectRgnIndirect(R);
  CombineRgn(ARegion, ARegion, ATempRegion, RGN_OR);
  DeleteObject(ATempRegion);
end;

class procedure TdxGdiClippingHelper.ExcludeRectangleFromRegion(var ARegion: HRGN; const R: TRect);
var
  ATempRegion: HRGN;
begin
  ATempRegion := CreateRectRgnIndirect(R);
  CombineRgn(ARegion, ARegion, ATempRegion, RGN_DIFF);
  DeleteObject(ATempRegion);
end;

class procedure TdxGdiClippingHelper.RestoreClipping(DC: HDC; ARegion: HRGN);
begin
  SelectClipRgn(DC, ARegion);
  if ARegion <> 0 then
    DeleteObject(ARegion);
end;

class procedure TdxGdiClippingHelper.SaveClipping(DC: HDC; out ARegion: HRGN);
begin
  ARegion := CreateRectRgn(0, 0, 0, 0);
  if GetClipRgn(DC, ARegion) <> 1 then
  begin
    DeleteObject(ARegion);
    ARegion := 0;
  end;
end;

{ TdxRTLReadingCharacterPlacementHelper }

class function TdxRTLReadingCharacterPlacementHelper.MeasureCharactersBounds(
  ADC: HDC; const AText: PChar; ALength: Integer; const ABounds: TRect;
  AFont: HFONT = 0; AIsRTLReading: Boolean = False): TArray<TRect>;
var
  AGcpResults: TGCPResults;
  ASize: Cardinal;
  AOldFont: HFONT;
begin
  if ALength <= 0 then
    Exit(nil);
  if AFont <> 0 then
    AOldFont := SelectObject(ADC, AFont)
  else
    AOldFont := 0;
  ZeroMemory(@AGcpResults, SizeOf(AGcpResults));
  try
    AGcpResults.lStructSize := SizeOf(GCP_RESULTS);
    AGcpResults.nGlyphs := ALength;
    ASize := SizeOf(Integer) * ALength;
    if AIsRTLReading then
      GetMem(AGcpResults.lpOrder, ASize);
    GetMem(AGcpResults.lpDx, ASize);
    try
      ASize := GetCharacterPlacement(ADC, PChar(AText), ALength, 0, AGcpResults, GCPFlags[AIsRTLReading]);
      if (ASize = 0) and (ALength > 0) then
        if TryMeasureCharactersSlow(ADC, AText, ALength, AGcpResults, GCPFlags[AIsRTLReading]) = 0 then
          Exit(nil);
      Result := CalculateCharactersBounds(AGcpResults, ALength, ABounds);
    finally
      FreeMem(AGcpResults.lpOrder);
      FreeMem(AGcpResults.lpDx);
    end;
  finally
    if AFont <> 0 then
      SelectObject(ADC, AOldFont);
  end;
end;

class function TdxRTLReadingCharacterPlacementHelper.TryMeasureCharactersSlow(ADC: THandle; const AText: PChar;
  ALength: Integer; var AGcpResults: TGCPResults; AFlags: Integer): Integer;
var
  I, AAdd, AStep, ASize: Integer;
begin
  AStep := Max(1, ALength div 2);
  AAdd := AStep;
  for I := 0 to 2 do
  begin
    ASize := SizeOf(Integer) * (ALength + AAdd);
    if AGcpResults.lpDx <> nil then
      ReallocMem(AGcpResults.lpDx, ASize);
    if AGcpResults.lpOrder <> nil then
      ReallocMem(AGcpResults.lpOrder, ASize);
    if AGcpResults.lpCaretPos <> nil then
      ReallocMem(AGcpResults.lpCaretPos, ASize);
    AGcpResults.nGlyphs := ALength + AAdd;
    Result := GetCharacterPlacement(ADC, PChar(AText), ALength, 0, AGcpResults, AFlags);
    if Result <> 0 then
      Break;
  end;
end;

class function TdxRTLReadingCharacterPlacementHelper.CalculateCharactersBounds(const AGcpResults: TGCPResults;
  ALength: Integer; const ABounds: TRect): TArray<TRect>;
var
  I, APrevPos: Integer;
  ADX: PINT;
  AOrder: PUINTArray;
  ADisplayOrderBounds: TArray<TRect>;
begin
  SetLength(ADisplayOrderBounds, ALength);
  ADX := AGcpResults.lpDx;
  APrevPos := ABounds.Left;
  for I := 0 to ALength - 1 do
  begin
    ADisplayOrderBounds[I].InitSize(APrevPos, ABounds.Top, ADX^, ABounds.Height);
    Inc(APrevPos, ADX^);
    Inc(ADX);
  end;
  AOrder := Pointer(AGcpResults.lpOrder);
  if AOrder = nil then
    Exit(ADisplayOrderBounds);
  SetLength(Result, ALength);
  for I := 0 to ALength - 1 do
    Result[I] := ADisplayOrderBounds[AOrder[I]];
end;

class function TdxRTLReadingCharacterPlacementHelper.GetSelectionRangeRegion(ADC: HDC; const AText: PChar; ALength: Integer;
  const ABounds: TRect; ASelStart, ASelLength: Integer; AFont: HFONT = 0; AIsRTLReading: Boolean = False): HRGN;
var
  I, ASelFinish: Integer;
  ACharacterBounds: TArray<TRect>;
  ARgn: HRGN;
  R: TRect;
begin
  Result := 0;
  if ASelLength <= 0 then
    Exit;
  if ASelStart > ALength then
    Exit;

  ACharacterBounds := MeasureCharactersBounds(ADC, AText, ALength, ABounds, AFont, AIsRTLReading);
  if Length(ACharacterBounds) = 0 then
    Exit;

  ASelFinish := Min(ALength, ASelStart + ASelLength) - 1;
  for I := ASelStart to ASelFinish do
  begin
    R := ACharacterBounds[I];
    if R.Right <= R.Left then
      continue;
    if Result = 0 then
      Result := CreateRectRgn(R.Left, R.Top, R.Right, R.Bottom)
    else
    begin
      ARgn := CreateRectRgn(R.Left, R.Top, R.Right, R.Bottom);
      CombineRgn(Result, Result, ARgn, RGN_OR);
      DeleteObject(ARgn);
    end;
  end;
end;

{$IFNDEF DELPHIXE}
initialization
  TdxAlphaColors.Initialize;

finalization
  TdxAlphaColors.Finalize;

{$ENDIF}
end.
