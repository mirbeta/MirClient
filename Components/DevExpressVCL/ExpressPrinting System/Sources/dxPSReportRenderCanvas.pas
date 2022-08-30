{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressPrinting System                                   }
{                                                                    }
{           Copyright (C) 1998-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSPRINTING SYSTEM AND            }
{   ALL ACCOMPANYING VCL CONTROLS AS PART OF AN                      }
{   EXECUTABLE PROGRAM ONLY.                                         }
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

unit dxPSReportRenderCanvas;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Windows, SysUtils, Classes, Graphics, cxClasses, cxGraphics, cxGeometry,
  cxDrawTextUtils, dxPSFillPatterns, dxCore, ImgList, dxGDIPlusAPI, dxGDIPlusClasses;

type
  TdxPicturePaintMode = (ppmCenter, ppmStretch, ppmTile, ppmProportional);
  TdxCheckButtonEdgeStyle = (cbesNone, cbes3D, cbesSoft3D, cbesBoldFlat, cbesUltraFlat, cbesSingle);

  TdxPSReportRenderCanvasMappingMode = (rrmmDefault, rrmmText, rrmmLoMetric, rrmmHiMetric,
    rrmmLoEnglish, rrmmHiEnglish, rrmmTwips, rrmmIsotropic, rrmmAnisotropic);

  { TdxDrawGraphicHelper }

  TdxDrawGraphicHelperExportProc = reference to procedure (AGraphic: TGraphic; AMask: TcxRegion);

  TdxDrawGraphicHelper = class
  strict private type
  {$REGION 'Private Types'}

    TDIBHelper = class
    strict private
      FBitmapInfoMemory: HGLOBAL;
      FBitsMemory: HGLOBAL;
      FBitmapInfo: PBitmapInfo;
      FBits: Pointer;
      FBitmap: TBitmap;

      function GetDIBColor: Integer;
      procedure Initialize;
      procedure Finalize;
    public
      constructor Create(ABitmap: TBitmap);
      destructor Destroy; override;
      procedure Draw(ACanvas: TCanvas; const ABounds: TRect; ARop: DWORD);

      property BitmapInfo: PBitmapInfo read FBitmapInfo;
      property Bits: Pointer read FBits;
    end;

    { TAbstractGraphicDrawer }

    TAbstractGraphicDrawerClass = class of TAbstractGraphicDrawer;
    TAbstractGraphicDrawer = class abstract
    strict private
      FOwner: TdxDrawGraphicHelper;
      FSavedStretchMode: Integer;

      function CreateGraphics(ACanvas: TCanvas): GpGraphics;
      procedure DeleteGraphics(AGraphics: GpGraphics);
      function GetGraphic: TGraphic;
      function GetPrinting: Boolean;
      function GetTransparent: Boolean;
    strict protected
      function BeginGdiPlusPaint(ACanvas: TCanvas): GpGraphics;
      procedure EndGdiPlusPaint(ACanvas: TCanvas; AGraphics: GpGraphics);
      procedure PrepareCanvas(ACanvas: TCanvas);
      procedure UnprepareCanvas(ACanvas: TCanvas);

      property Graphic: TGraphic read GetGraphic;
      property Owner: TdxDrawGraphicHelper read FOwner;
      property Printing: Boolean read GetPrinting;
      property Transparent: Boolean read GetTransparent;
    public
      constructor Create(AOwner: TdxDrawGraphicHelper); virtual;
      procedure Initialize; virtual;
      procedure Export(AProc: TdxDrawGraphicHelperExportProc); virtual;
      procedure Paint(ACanvas: TCanvas; const ADestRect: TRect); virtual; abstract;
      procedure Print(ACanvas: TCanvas; const ADestRect: TRect); virtual;
    end;

    { TMetaFileDrawer }

    TMetaFileDrawer = class(TAbstractGraphicDrawer)
    public
      procedure Paint(ACanvas: TCanvas; const ADestRect: TRect); override;
    end;

    { TCustomBitmapDrawer }

    TCustomBitmapDrawer = class(TAbstractGraphicDrawer)
    strict private
      FBitmap: TBitmap;
    strict protected
      function GetStretchBltMode(ABitmap: TBitmap): Integer;
      procedure Mask(ADestDC: HDC; const ADestRect: TRect; ASrcDC: HDC; const ASrcRect: TRect);
      procedure DrawBitmap(ACanvas: TCanvas; ABitmap: TBitmap; const ADestRect: TRect);
      procedure DrawBitmapImage; virtual;
      procedure PrintBitmap(ACanvas: TCanvas; ABitmap: TBitmap; const ADestRect: TRect);
      procedure PrintTransparentBitmap(ACanvas: TCanvas; ABitmap: TBitmap; const ADestRect: TRect; AMemoryDC: HDC);
      procedure PaintTransparentBitmap(ACanvas: TCanvas; ABitmap: TBitmap; const ADestRect: TRect; AMemoryDC: HDC);
      procedure DrawTransparentBitmap(ACanvas: TCanvas; ABitmap: TBitmap; const ADestRect: TRect);
      procedure ValidateBitmap(AWidth, AHeight: Integer);
    public
      destructor Destroy; override;
      procedure Initialize; override;
      procedure Export(AProc: TdxDrawGraphicHelperExportProc); override;
      procedure Paint(ACanvas: TCanvas; const ADestRect: TRect); override;
      procedure Print(ACanvas: TCanvas; const ADestRect: TRect); override;
      //
      property Bitmap: TBitmap read FBitmap;
    end;

    { TBitmapDrawer }

    TBitmapDrawer = class(TCustomBitmapDrawer)
    public
      procedure Initialize; override;
    end;

    { TImageListDrawer }

    TImageListDrawer = class(TCustomBitmapDrawer)
    strict protected
      procedure DrawBitmapImage; override;
    public
      procedure Initialize; override;
      procedure Paint(ACanvas: TCanvas; const ADestRect: TRect); override;
    end;

    { TIconDrawer }

    TIconDrawer = class(TAbstractGraphicDrawer)
    strict private
      FBits: TDIBHelper;
      FSize: TSize;
    public
      destructor Destroy; override;
      procedure Initialize; override;
      procedure Paint(ACanvas: TCanvas; const ADestRect: TRect); override;
    end;

    { TGenericDrawer }

    TGenericDrawer = class(TCustomBitmapDrawer)
    public
      procedure Paint(ACanvas: TCanvas; const ADestRect: TRect); override;
      procedure Print(ACanvas: TCanvas; const ADestRect: TRect); override;
    end;

    { TSmartImageDrawer }

    TSmartImageDrawer = class(TCustomBitmapDrawer)
    public type
      TdxGPImageHelper = class helper for TdxGPImage
        function GetGdipImageHandle: GPImage;
      end;
    strict protected
      FGPImage: GPImage;
      FUseTempImage: Boolean;
      function GetGdipImage: GpImage;
    public
      procedure Initialize; override;
      procedure Export(AProc: TdxDrawGraphicHelperExportProc); override;
      procedure Paint(ACanvas: TCanvas; const ADestRect: TRect); override;
      procedure Print(ACanvas: TCanvas; const ADestRect: TRect); override;
    end;

  {$ENDREGION}
  strict private
    FBackgroundColor: TColor;
    FDrawer: TAbstractGraphicDrawer;
    FGraphic: TGraphic;
    FImageIndex: Integer;
    FImageList: TCustomImageList;
    FPrinting: Boolean;
    FTransparent: Boolean;
    FTransparentColor: TColor;
  protected
    function GetDrawerClass: TAbstractGraphicDrawerClass; virtual;
    function CreateDrawer: TAbstractGraphicDrawer; virtual;
    procedure Initialize(ABackgroundColor, ATransparentColor: TColor; AIsTransparent, APrinting: Boolean); overload;
    procedure ValidateDrawer;

    property BackgroundColor: TColor read FBackgroundColor;
    property Drawer: TAbstractGraphicDrawer read FDrawer;
    property Printing: Boolean read FPrinting;
    property Transparent: Boolean read FTransparent;
    property TransparentColor: TColor read FTransparentColor;
  public
    destructor Destroy; override;
    procedure Draw(ACanvas: TCanvas; const ADestRect: TRect);
    procedure Export(AProc: TdxDrawGraphicHelperExportProc);
    procedure Initialize(AGraphic: TGraphic;
      ABackgroundColor, ATransparentColor: TColor; AIsTransparent, APrinting: Boolean); overload;
    procedure Initialize(AImageList: TCustomImageList; AImageIndex: Integer;
      ABackgroundColor, ATransparentColor: TColor; AIsTransparent, APrinting: Boolean); overload;

    property Graphic: TGraphic read FGraphic;
    property ImageIndex: Integer read FImageIndex;
    property ImageList: TCustomImageList read FImageList;
  end;

  { TdxPSReportRenderCustomCanvas }

  TdxPSReportRenderCustomCanvas = class(TObject)
  strict private
    FFont: TFont;

    procedure AssignFont(ATarget, ASource: TFont);
    procedure FontChangeHandler(Sender: TObject);
    procedure SetFont(AValue: TFont);
  protected
    function GetBrush: TBrush; virtual; abstract;
    function GetBrushOrg: TPoint; virtual; abstract;
    function GetPixelsPerInch: Integer; virtual; abstract;
    function GetSupportsTransparentImagesDrawing: Boolean; virtual;
    function GetWindowExt: TSize; virtual; abstract;
    function GetWindowOrg: TPoint; virtual; abstract;
    function GetWorldTransform: TXForm; virtual; abstract;
    procedure AssignCanvasFont(ACanvas: TcxCanvas);
    procedure DoFillEdge(ARegion: TcxRegionHandle; ABackColor, AEdgeColor: TColor;
      AIsVerticalOrientation: Boolean; AEdgePattern: TClass{TdxPSEdgePatternClass}); virtual;
    procedure FontChanged; virtual;
    procedure PrepareCanvasForCustomDraw(AFont: TFont; AColor: TColor); virtual;
    procedure SetBrush(AValue: TBrush); virtual; abstract;
    procedure SetBrushOrg(const AValue: TPoint); virtual; abstract;
    procedure SetWindowExt(const ASize: TSize); virtual; abstract;
    procedure SetWindowOrg(const P: TPoint); virtual; abstract;
    procedure SetWorldTransform(const Value: TXForm); virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;
    function CalculateLineThickness(AUnitsPerPixel, AUnitsPerPoint: Integer): Integer; virtual;
    function IsPrinterCanvas: Boolean; virtual; abstract;

    procedure DeviceToLogicalCoordinates(var R: TRect); virtual; abstract;
    procedure FixupRect(var R: TRect); virtual;
    procedure LogicalToDeviceCoordinates(var R: TRect); virtual; abstract;

    procedure RestoreState; virtual; abstract;
    procedure SaveState; virtual; abstract;

    procedure PrepareLogicalUnits; virtual;
    procedure UnprepareLogicalUnits; virtual;

    // Custom Draw
    function BeginCustomDraw(const AClipBounds: TRect; AFont: TFont; AColor: TColor): TCanvas; virtual; abstract;
    procedure EndCustomDraw(var ACanvas: TCanvas); virtual; abstract;

    // Text
    function CalculateTextParams(ADrawTextFormat: Integer): TcxTextParams; virtual; abstract;
    function CalculateTextRect(const AText: string; var R: TRect; AFormat: TcxTextOutFormat = CXTO_DEFAULT_FORMAT;
      AFont: TFont = nil; AMaxLineCount: Integer = 0; ALeftIndent: Integer = 0; ARightIndent: Integer = 0;
      ATextColor: TColor = clDefault; ALineSpacing: Single = 1.0): Integer; virtual; abstract;
    procedure DrawText(var R: TRect; const AText: string; AFont: TFont; AFormat: Cardinal;
      ATextColor: TColor = clDefault; AMaxLineCount: Integer = 0; ALineSpacing: Single = 1.0); virtual; abstract;
    procedure ExtTextOut(const ABounds: TRect; AFont: TFont; const AText: string;
      AGlyphs: PWord; AGlyphCount: Integer; AGlyphWidths: PInteger); virtual; abstract;
    function MakeTextRows(AText: PChar; ATextLength: Integer; const R: TRect; const ATextParams: TcxTextParams;
      var ATextRows: TcxTextRows; out ACount: Integer; AMaxLineCount: Integer = 0): Boolean; virtual; abstract;
    procedure RotatedTextOut(const ABounds: TRect; const AText: string; AFont: TFont;
      AAlignHorz: TcxTextAlignX = taCenterX; AAlignVert: TcxTextAlignY = taCenterY; AWordBreak: Boolean = True;
      ADirection: TcxVerticalTextOutDirection = vtdBottomToTop); virtual; abstract;
    function TextSize(const AText: string): TSize; virtual; abstract;

    // Orgs and Clipping
    function ExcludeClipRect(const R: TRect): Integer; virtual; abstract;
    function IntersectClipRgn(const R: TRect): Integer; virtual; abstract;
    function IsRectVisible(const R: TRect): Boolean; virtual; abstract;
    function OffsetWindowOrg(const P: TPoint): TPoint; virtual; abstract;
    procedure SetCanvasExts(const APageSize: TPoint; AMappingMode: TdxPSReportRenderCanvasMappingMode;
      AScaleNumerator, AScaleDenominator: Integer; const AViewPort: TRect); virtual; abstract;
    procedure RestoreClipRgn; virtual; abstract;
    procedure SaveClipRgn; virtual; abstract;

    procedure DrawCheckBox(var R: TRect; AChecked, AEnabled, AIsRadio: Boolean; AEdgeStyle: TdxCheckButtonEdgeStyle;
      AMarlettFont: TFont; ALineThickness: Integer; ABorderColor: TColor = clWindowText); virtual; abstract;
    procedure DrawEllipseFrame(const R: TRect; AColor: TColor; AThickness: Integer); virtual; abstract;
    procedure DrawExpandButton(R: TRect; AEdgeStyle: TdxCheckButtonEdgeStyle; AMarlettFont, ASymbolFont: TFont;
      AExpanded, AShadow, AFillInterior: Boolean; ABorderColor, ABackgroundColor: TColor; ALineThickness: Integer); virtual; abstract;
    procedure DrawFrame(const R: TRect; ATopLeftColor, ARightBottomColor: TColor;
      ABorderWidth: Integer = 1; ABorders: TcxBorders = cxBordersAll); virtual; abstract;
    procedure DrawGlyph(const R: TRect; AGlyphFont: TFont; AGlyphIndex: Byte;
      ACenterOnRect: Boolean; ATextColor: TColor = clWindowText); virtual; abstract;
    procedure DrawGraphic(AHelper: TdxDrawGraphicHelper; const R: TRect); virtual; abstract;
    procedure DrawPicture(APicture: TGraphic; const R: TRect; APictureMode: TdxPicturePaintMode;
      ANumerator, ADenominator: Integer; AOffsetX: Integer = 0; AOffsetY: Integer = 0); virtual; abstract;
    procedure DrawRoundFrame(const R: TRect; AEllipseWidth, AEllipseHeight: Integer; AColor: TColor; AThickness: Integer); virtual; abstract;

    procedure FillEdge(const R: TRect; ABackColor, AEdgeColor: TColor;
      AIsVerticalOrientation: Boolean; AEdgePattern: TClass{TdxPSEdgePatternClass}); overload;
    procedure FillEdge(ARegion: TcxRegionHandle; ABackColor, AEdgeColor: TColor;
      AIsVerticalOrientation: Boolean; AEdgePattern: TClass{TdxPSEdgePatternClass}); overload;
    procedure FillEllipse(const R: TRect; ABackColor, AForeColor: TColor;
      APattern: TdxPSFillPatternClass; APatternBrush: TBrush); virtual; abstract;
    procedure FillRect(const R: TRect; AColor: TColor); virtual; abstract;
    procedure FillRegion(ARegion: TcxRegionHandle; ABackColor, AForeColor: TColor;
      APattern: TdxPSFillPatternClass; APatternBrush: TBrush); virtual; abstract;
    procedure FillRoundRect(R: TRect; AEllipseWidth, AEllipseHeight: Integer;
      ABackColor, AForeColor: TColor; APattern: TdxPSFillPatternClass; APatternBrush: TBrush); virtual; abstract;

    procedure Polyline(const APoints: array of TPoint; AColor: TColor; ALineWidth: Integer); virtual; abstract;
    procedure Polygon(const APoints: array of TPoint; AColor, ABackgroundColor: TColor;
      ALineWidth: Integer; AFillMode: Integer = ALTERNATE); virtual; abstract;

    property Brush: TBrush read GetBrush write SetBrush;
    property BrushOrg: TPoint read GetBrushOrg write SetBrushOrg;
    property Font: TFont read FFont write SetFont;
    property PixelsPerInch: Integer read GetPixelsPerInch;
    property SupportsTransparentImagesDrawing: Boolean read GetSupportsTransparentImagesDrawing;
    property WindowExt: TSize read GetWindowExt write SetWindowExt;
    property WindowOrg: TPoint read GetWindowOrg write SetWindowOrg;
    property WorldTransform: TXForm read GetWorldTransform write SetWorldTransform;
  end;

  { TdxPSReportRenderCanvas }

  TdxPSReportRenderCanvas = class(TdxPSReportRenderCustomCanvas)
  strict private
    FCanvas: TcxCanvas;
  protected
    procedure GetBorderColors(AEdgeStyle: TdxCheckButtonEdgeStyle; ADefaultBorderColor: TColor;
      AInnerBorder: Boolean; out ATopLeftBorderColor, ARightBottomBorderColor: TColor);
    function GetBrush: TBrush; override;
    function GetBrushOrg: TPoint; override;
    function GetPixelsPerInch: Integer; override;
    function GetWindowExt: TSize; override;
    function GetWindowOrg: TPoint; override;
    function GetWorldTransform: TXForm; override;
    function IsSolidFillPattern(APattern: TdxPSFillPatternClass; APatternBrush: TBrush): Boolean;
    procedure FontChanged; override;
    procedure SetBrush(AValue: TBrush); override;
    procedure SetBrushOrg(const AValue: TPoint); override;
    procedure SetWindowExt(const AValue: TSize); override;
    procedure SetWindowOrg(const P: TPoint); override;
    procedure SetWorldTransform(const Value: TXForm); override;

    function CalculatePictureRect(APicture: TGraphic; const R: TRect;
      APictureMode: TdxPicturePaintMode; ANumerator, ADenominator: Integer): TRect;
    function CreatePatternBitmap(ABackColor, AForeColor: TColor;
      const R: TRect; ARequiredBrushOrigin: Boolean; ABrush: TBrush): TcxBitmap;
    procedure InternalDrawFrameControl(const R: TRect; AType, AState: Integer); virtual;
    procedure InternalFillRectByBrushBitmap(const R: TRect;
      ABackColor, AForeColor: TColor; ARequiredBrushOrigin: Boolean; ABrush: TBrush);

    procedure DrawPictureStretch(APicture: TGraphic;
      const R: TRect; ANumerator, ADenominator: Integer); virtual;
    procedure DrawPictureTile(APicture: TGraphic; const R: TRect;
      APictureWidth, APictureHeight, ANumerator, ADenominator: Integer); virtual;
  public
    constructor Create(ACanvas: TCanvas);
    destructor Destroy; override;

    function IsPrinterCanvas: Boolean; override;
    procedure DeviceToLogicalCoordinates(var R: TRect); override;
    procedure LogicalToDeviceCoordinates(var R: TRect); override;

    procedure RestoreState; override;
    procedure SaveState; override;

    // Custom Draw
    function BeginCustomDraw(const AClipBounds: TRect; AFont: TFont; AColor: TColor): TCanvas; override;
    procedure EndCustomDraw(var ACanvas: TCanvas); override;

    // Text
    function CalculateTextParams(ADrawTextFormat: Integer): TcxTextParams; override;
    function CalculateTextRect(const AText: string; var R: TRect; AFormat: TcxTextOutFormat = CXTO_DEFAULT_FORMAT;
      AFont: TFont = nil; AMaxLineCount: Integer = 0; ALeftIndent: Integer = 0; ARightIndent: Integer = 0;
      ATextColor: TColor = clDefault; ALineSpacing: Single = 1.0): Integer; override;
    procedure DrawText(var R: TRect; const AText: string; AFont: TFont; AFormat: Cardinal;
      ATextColor: TColor = clDefault; AMaxLineCount: Integer = 0; ALineSpacing: Single = 1.0); override;
    procedure ExtTextOut(const ABounds: TRect; AFont: TFont; const AText: string;
      AGlyphs: PWord; AGlyphCount: Integer; AGlyphWidths: PInteger); override;
    function MakeTextRows(AText: PChar; ATextLength: Integer; const R: TRect; const ATextParams: TcxTextParams;
      var ATextRows: TcxTextRows; out ACount: Integer; AMaxLineCount: Integer = 0): Boolean; override;
    procedure RotatedTextOut(const ABounds: TRect; const AText: string; AFont: TFont; AAlignHorz: TcxTextAlignX = taCenterX;
      AAlignVert: TcxTextAlignY = taCenterY; AWordBreak: Boolean = True; ADirection: TcxVerticalTextOutDirection = vtdBottomToTop); override;
    function TextSize(const AText: string): TSize; override;

    // Orgs and Clipping
    function ExcludeClipRect(const R: TRect): Integer; override;
    function IntersectClipRgn(const R: TRect): Integer; override;
    function IsRectVisible(const R: TRect): Boolean; override;
    function OffsetWindowOrg(const P: TPoint): TPoint; override;
    procedure SetCanvasExts(const APageSize: TPoint; AMappingMode: TdxPSReportRenderCanvasMappingMode;
      AScaleNumerator, AScaleDenominator: Integer; const AViewPort: TRect); override;
    procedure RestoreClipRgn; override;
    procedure SaveClipRgn; override;

    // Filling
    procedure DrawCheckBox(var R: TRect; AChecked, AEnabled, AIsRadio: Boolean; AEdgeStyle: TdxCheckButtonEdgeStyle;
      AMarlettFont: TFont; ALineThickness: Integer; ABorderColor: TColor = clWindowText); override;
    procedure DrawEllipseFrame(const R: TRect; AColor: TColor; AThickness: Integer); override;
    procedure DrawExpandButton(R: TRect; AEdgeStyle: TdxCheckButtonEdgeStyle; AMarlettFont, ASymbolFont: TFont;
      AExpanded, AShadow, AFillInterior: Boolean; ABorderColor, ABackgroundColor: TColor; ALineThickness: Integer); override;
    procedure DrawFrame(const R: TRect; ATopLeftColor, ARightBottomColor: TColor;
      ABorderWidth: Integer = 1; ABorders: TcxBorders = cxBordersAll); override;
    procedure DrawGlyph(const R: TRect; AGlyphFont: TFont; AGlyphIndex: Byte;
      ACenterOnRect: Boolean; ATextColor: TColor = clWindowText); override;
    procedure DrawGraphic(AHelper: TdxDrawGraphicHelper; const R: TRect); override;
    procedure DrawPicture(APicture: TGraphic; const R: TRect; APictureMode: TdxPicturePaintMode;
      ANumerator, ADenominator: Integer; AOffsetX: Integer = 0; AOffsetY: Integer = 0); override;
    procedure DrawRoundFrame(const R: TRect; AEllipseWidth, AEllipseHeight: Integer; AColor: TColor; AThickness: Integer); override;
    procedure FillEllipse(const R: TRect; ABackColor, AForeColor: TColor;
      APattern: TdxPSFillPatternClass; APatternBrush: TBrush); override;
    procedure FillRect(const R: TRect; AColor: TColor); override;
    procedure FillRegion(ARegion: TcxRegionHandle; ABackColor, AForeColor: TColor;
      APattern: TdxPSFillPatternClass; APatternBrush: TBrush); override;
    procedure FillRoundRect(R: TRect; AEllipseWidth, AEllipseHeight: Integer;
      ABackColor, AForeColor: TColor; APattern: TdxPSFillPatternClass; APatternBrush: TBrush); override;
    procedure Polyline(const APoints: array of TPoint; AColor: TColor; ALineWidth: Integer); override;
    procedure Polygon(const APoints: array of TPoint; AColor, ABackgroundColor: TColor;
      ALineWidth: Integer; AFillMode: Integer = ALTERNATE); override;

    property Canvas: TcxCanvas read FCanvas;
  end;

  { TdxPSReportRenderScreenCanvas }

  TdxPSReportRenderScreenCanvas = class(TdxPSReportRenderCanvas)
  private
    FSourceCanvas: TCanvas;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  { TdxPSAdjustFontSizeHelper }

  TdxPSAdjustFontSizeHelper = class(TdxAdjustFontSizeCustomHelper)
  strict private
    FCanvas: TdxPSReportRenderCustomCanvas;
  protected
    function CheckFontSize(ALastSuitableSize, AOriginalSize, AMinSize: Integer): Integer; override;
    function GetMinFontSize: Integer; override;
    function GetTextHeight(const AText: string; AWidth: Integer; ALineSpacing: Single): Integer; override;
    function GetTextWidth(const AText: string): Integer; override;
  public
    constructor Create(ACanvas: TdxPSReportRenderCustomCanvas);
  end;

procedure cxBkgndDrawPicture(APicture: TGraphic; ACanvas: TCanvas; const R: TRect;
  APictureMode: TdxPicturePaintMode; APixelsNumerator, APixelsDenominator: Integer;
  AOffsetX: Integer = 0; AOffsetY: Integer = 0);
implementation

uses
  Math, Types, ActiveX, AxCtrls, dxPSUtl, dxPSCore, dxPSEdgePatterns, dxTypeHelpers, dxCoreGraphics, dxSmartImage;

procedure cxBkgndDrawPicture(APicture: TGraphic; ACanvas: TCanvas; const R: TRect;
  APictureMode: TdxPicturePaintMode; APixelsNumerator, APixelsDenominator: Integer;
  AOffsetX: Integer = 0; AOffsetY: Integer = 0);
var
  ARenderCanvas: TdxPSReportRenderCanvas;
begin
  ARenderCanvas := TdxPSReportRenderCanvas.Create(ACanvas);
  try
    ARenderCanvas.DrawPicture(APicture, R, APictureMode,
      APixelsNumerator, APixelsDenominator, AOffsetX, AOffsetY);
  finally
    ARenderCanvas.Free;
  end;
end;

{ TdxDrawGraphicHelper.TDIBHelper }

constructor TdxDrawGraphicHelper.TDIBHelper.Create(ABitmap: TBitmap);
begin
  FBitmap := ABitmap;
  Initialize;
end;

destructor TdxDrawGraphicHelper.TDIBHelper.Destroy;
begin
  Finalize;
  inherited Destroy;
end;

procedure TdxDrawGraphicHelper.TDIBHelper.Draw(ACanvas: TCanvas;
  const ABounds: TRect; ARop: DWORD);
begin
  StretchDIBits(ACanvas.Handle, ABounds.Left, ABounds.Top, ABounds.Width, ABounds.Height,
    0, 0, BitmapInfo.bmiHeader.biWidth, BitmapInfo.bmiHeader.biHeight,
    Bits, BitmapInfo^, GetDIBColor, ARop);
end;

procedure TdxDrawGraphicHelper.TDIBHelper.Initialize;
var
  ABitmapInfoSize, ABitsSize: Cardinal;
begin
  GetDIBSizes(FBitmap.Handle, ABitmapInfoSize, ABitsSize);
  FBitmapInfoMemory := GlobalAlloc(GMEM_MOVEABLE or GMEM_SHARE, ABitmapInfoSize);
  FBitmapInfo := PBitmapInfo(GlobalLock(FBitmapInfoMemory));
  try
    FBitsMemory := GlobalAlloc(GMEM_MOVEABLE or GMEM_SHARE, ABitsSize);
    FBits := Pointer(GlobalLock(FBitsMemory));
    if not GetDIB(FBitmap.Handle, FBitmap.Palette, FBitmapInfo^, FBits^) then
      Finalize;
  except
    Finalize;
  end;
end;

procedure TdxDrawGraphicHelper.TDIBHelper.Finalize;
begin
  if FBitmapInfoMemory <> 0 then
  begin
    GlobalUnlock(FBitmapInfoMemory);
    GlobalFree(FBitmapInfoMemory);
    FBitmapInfoMemory := 0;
  end;
  if FBitsMemory <> 0 then
  begin
    GlobalUnlock(FBitsMemory);
    GlobalFree(FBitsMemory);
    FBitsMemory := 0;
  end;
  FBits := nil;
end;

function TdxDrawGraphicHelper.TDIBHelper.GetDIBColor: Integer;
begin
  if FBitmap.PixelFormat in [pf4bit..pf16bit] then
    Result := DIB_PAL_COLORS
  else
    Result := DIB_RGB_COLORS;
end;

{ TdxDrawGraphicHelper.TAbstractGraphicDrawer }

constructor TdxDrawGraphicHelper.TAbstractGraphicDrawer.Create(AOwner: TdxDrawGraphicHelper);
begin
  FOwner := AOwner;
end;

function TdxDrawGraphicHelper.TAbstractGraphicDrawer.BeginGdiPlusPaint(ACanvas: TCanvas): GpGraphics;
begin
  PrepareCanvas(ACanvas);
  ACanvas.Lock;
  Result := CreateGraphics(ACanvas);
end;

procedure TdxDrawGraphicHelper.TAbstractGraphicDrawer.EndGdiPlusPaint(ACanvas: TCanvas; AGraphics: GpGraphics);
begin
  DeleteGraphics(AGraphics);
  ACanvas.Unlock;
  UnprepareCanvas(ACanvas);
end;

procedure TdxDrawGraphicHelper.TAbstractGraphicDrawer.PrepareCanvas(ACanvas: TCanvas);
begin
  if FSmoothlyStretchImages then
    FSavedStretchMode := SetStretchBltMode(ACanvas.Handle, STRETCH_HALFTONE)
  else
    FSavedStretchMode := -1;
end;

procedure TdxDrawGraphicHelper.TAbstractGraphicDrawer.UnprepareCanvas(ACanvas: TCanvas);
begin
  if FSavedStretchMode > -1 then
    SetStretchBltMode(ACanvas.Handle, FSavedStretchMode);
end;

procedure TdxDrawGraphicHelper.TAbstractGraphicDrawer.Export(AProc: TdxDrawGraphicHelperExportProc);
begin
  AProc(Graphic, nil);
end;

procedure TdxDrawGraphicHelper.TAbstractGraphicDrawer.Print(ACanvas: TCanvas; const ADestRect: TRect);
begin
  Paint(ACanvas, ADestRect);
end;

procedure TdxDrawGraphicHelper.TAbstractGraphicDrawer.Initialize;
begin
end;

function TdxDrawGraphicHelper.TAbstractGraphicDrawer.CreateGraphics(ACanvas: TCanvas): GpGraphics;
begin
  GdipCheck(GdipCreateFromHDC(ACanvas.Handle, Result));
  if not FSmoothlyStretchImages then
    GdipCheck(GdipSetInterpolationMode(Result, InterpolationModeNearestNeighbor));
  if Printing then
    GdipCheck(GdipSetPageUnit(Result, TdxGraphicUnit.guPixel));
end;

procedure TdxDrawGraphicHelper.TAbstractGraphicDrawer.DeleteGraphics(AGraphics: GpGraphics);
begin
  GdipCheck(GdipDeleteGraphics(AGraphics));
end;

function TdxDrawGraphicHelper.TAbstractGraphicDrawer.GetGraphic: TGraphic;
begin
  Result := Owner.Graphic;
end;

function TdxDrawGraphicHelper.TAbstractGraphicDrawer.GetPrinting: Boolean;
begin
  Result := Owner.Printing;
end;

function TdxDrawGraphicHelper.TAbstractGraphicDrawer.GetTransparent: Boolean;
begin
  Result := Owner.Transparent;
end;

{ TdxDrawGraphicHelper.TMetaFileDrawer }

procedure TdxDrawGraphicHelper.TMetaFileDrawer.Paint(ACanvas: TCanvas; const ADestRect: TRect);
var
  AOldTransparent: Boolean;
begin
  AOldTransparent := Graphic.Transparent;
  try
    Graphic.Transparent := Transparent;
    ACanvas.StretchDraw(ADestRect, Graphic);
  finally
    Graphic.Transparent := AOldTransparent;
  end;
end;

{ TdxDrawGraphicHelper.TCustomBitmapDrawer }

destructor TdxDrawGraphicHelper.TCustomBitmapDrawer.Destroy;
begin
  FreeAndNil(FBitmap);
  inherited Destroy;
end;

procedure TdxDrawGraphicHelper.TCustomBitmapDrawer.Export(AProc: TdxDrawGraphicHelperExportProc);
var
  AMask: TcxRegion;
begin
  if Transparent and cxColorIsValid(Owner.TransparentColor) then
  begin
    AMask := TcxRegion.Create(cxCreateRegionFromBitmap(Bitmap, Owner.TransparentColor));
    try
      AProc(Bitmap, AMask);
    finally
      AMask.Free;
    end;
  end
  else
    AProc(Bitmap, nil);
end;

procedure TdxDrawGraphicHelper.TCustomBitmapDrawer.Paint(ACanvas: TCanvas; const ADestRect: TRect);
var
  ASavedTransparent: Boolean;
begin
  ASavedTransparent := Graphic.Transparent;
  try
    Graphic.Transparent := Transparent;
    if Transparent and cxColorIsValid(Owner.TransparentColor) then
      DrawTransparentBitmap(ACanvas, TBitmap(Graphic), ADestRect)
    else
    begin
      PrepareCanvas(ACanvas);
      try
        ACanvas.StretchDraw(ADestRect, Graphic);
      finally
        UnprepareCanvas(ACanvas);
      end;
    end;
  finally
    Graphic.Transparent := ASavedTransparent;
  end;
end;

procedure TdxDrawGraphicHelper.TCustomBitmapDrawer.Print(ACanvas: TCanvas; const ADestRect: TRect);
begin
  if Transparent then
    DrawTransparentBitmap(ACanvas, FBitmap, ADestRect)
  else
    PrintBitmap(ACanvas, Bitmap, ADestRect)
end;

procedure TdxDrawGraphicHelper.TCustomBitmapDrawer.DrawBitmapImage;
var
  ASavedTransparent: Boolean;
begin
  ASavedTransparent := Graphic.Transparent;
  try
    Graphic.Transparent := Transparent;
    FBitmap.Canvas.Draw(0, 0, Graphic);
  finally
    Graphic.Transparent := ASavedTransparent;
  end;
end;

procedure TdxDrawGraphicHelper.TCustomBitmapDrawer.Initialize;
begin
  ValidateBitmap(Graphic.Width, Graphic.Height);
end;

procedure TdxDrawGraphicHelper.TCustomBitmapDrawer.DrawBitmap(ACanvas: TCanvas; ABitmap: TBitmap; const ADestRect: TRect);
var
  AOldStretchMode: Integer;
  AOldBitmap: HBITMAP;
  AMemoryDC: HDC;
begin
  AMemoryDC := CreateCompatibleDC(ACanvas.Handle);
  AOldBitmap := SelectObject(AMemoryDC, ABitmap.Handle);
  try
    AOldStretchMode := SetStretchBltMode(ACanvas.Handle, STRETCH_HALFTONE);
    StretchBlt(ACanvas.Handle, ADestRect.Left, ADestRect.Top, ADestRect.Width, ADestRect.Height, AMemoryDC,
      0, 0, ABitmap.Width, ABitmap.Height, SRCCOPY);
    SetStretchBltMode(ACanvas.Handle, AOldStretchMode);
  finally
    SelectObject(AMemoryDC, AOldBitmap);
    DeleteDC(AMemoryDC);
  end;
end;

procedure TdxDrawGraphicHelper.TCustomBitmapDrawer.DrawTransparentBitmap(ACanvas: TCanvas; ABitmap: TBitmap; const ADestRect: TRect);
var
  AMemoryDC: HDC;
  AOldStretchBltMode: Integer;
  AOldObject: HGDIOBJ;
begin
  AOldStretchBltMode := SetStretchBltMode(ACanvas.Handle, GetStretchBltMode(ABitmap));
  AMemoryDC := CreateCompatibleDC(ACanvas.Handle);
  try
    AOldObject := SelectObject(AMemoryDC, ABitmap.Handle);
    if Printing then
      PrintTransparentBitmap(ACanvas, ABitmap, ADestRect, AMemoryDC)
    else
      PaintTransparentBitmap(ACanvas, ABitmap, ADestRect, AMemoryDC);

    SelectObject(AMemoryDC, AOldObject);
  finally
    DeleteDC(AMemoryDC);
    SetStretchBltMode(ACanvas.Handle, AOldStretchBltMode);
  end;
end;

function TdxDrawGraphicHelper.TCustomBitmapDrawer.GetStretchBltMode(ABitmap: TBitmap): Integer;
begin
  if ABitmap.PixelFormat = pf1bit then
    Result := BLACKONWHITE
  else
    Result := STRETCH_HALFTONE;
end;

procedure TdxDrawGraphicHelper.TCustomBitmapDrawer.Mask(ADestDC: HDC; const ADestRect: TRect; ASrcDC: HDC; const ASrcRect: TRect);
var
  ABitmap, AOldBitmap: HBITMAP;
  ABitmapDC: HDC;
  ABitmapSize: TSize;
  ASaveDestBkColor: Cardinal;
  ASaveSrcBkColor: Cardinal;
  ASaveTextColor: Cardinal;
  AMaskColor: TColor;
begin
  AMaskColor := Owner.TransparentColor;
  if AMaskColor = clNone then
    AMaskColor := Owner.BackgroundColor;
  ASaveSrcBkColor := SetBkColor(ASrcDC, AMaskColor);
  ASaveTextColor := SetTextColor(ADestDC, 0);
  ASaveDestBkColor := SetBkColor(ADestDC, clWhite);

  ABitmapDC := CreateCompatibleDC(ADestDC);
  ABitmapSize := cxSize(ADestRect);
  if not Owner.Printing then
    ABitmapSize := cxSizeScale(ABitmapSize, PixelsDenominator, PixelsNumerator);

  ABitmap := CreateBitmap(ABitmapSize.cx, ABitmapSize.cy, 1, 1, nil);
  AOldBitmap := SelectObject(ABitmapDC, ABitmap);
  StretchBlt(ABitmapDC, 0, 0, ABitmapSize.cx, ABitmapSize.cy, ASrcDC, ASrcRect.Left, ASrcRect.Top, ASrcRect.Width, ASrcRect.Height, SRCCOPY);
  if (ABitmapSize.cx = ADestRect.Width) and (ABitmapSize.cy = ADestRect.Height) then
    BitBlt(ADestDC, ADestRect.Left, ADestRect.Top, ADestRect.Width, ADestRect.Height, ABitmapDC, 0, 0, SRCAND)
  else
    StretchBlt(ADestDC, ADestRect.Left, ADestRect.Top, ADestRect.Width, ADestRect.Height, ABitmapDC, 0, 0, ABitmapSize.cx, ABitmapSize.cy, SRCAND);

  SelectObject(ABitmapDC, AOldBitmap);
  DeleteDC(ABitmapDC);
  DeleteObject(ABitmap);
  SetTextColor(ADestDC, ASaveTextColor);
  SetBkColor(ADestDC, ASaveDestBkColor);
  SetBkColor(ASrcDC, ASaveSrcBkColor);
end;

procedure TdxDrawGraphicHelper.TCustomBitmapDrawer.PaintTransparentBitmap(ACanvas: TCanvas; ABitmap: TBitmap; const ADestRect: TRect; AMemoryDC: HDC);
begin
  StretchBlt(ACanvas.Handle, ADestRect.Left, ADestRect.Top, ADestRect.Width, ADestRect.Height, AMemoryDC, 0, 0, ABitmap.Width, ABitmap.Height, SRCINVERT);
  Mask(ACanvas.Handle, ADestRect, AMemoryDC, cxRect(0, 0, ABitmap.Width, ABitmap.Height));
  StretchBlt(ACanvas.Handle, ADestRect.Left, ADestRect.Top, ADestRect.Width, ADestRect.Height, AMemoryDC, 0, 0, ABitmap.Width, ABitmap.Height, SRCINVERT);
end;

procedure TdxDrawGraphicHelper.TCustomBitmapDrawer.PrintBitmap(ACanvas: TCanvas; ABitmap: TBitmap; const ADestRect: TRect);
var
  ADibHelper: TDIBHelper;
begin
  ADibHelper := TDIBHelper.Create(ABitmap);
  try
    PrepareCanvas(ACanvas);
    if ADibHelper.Bits <> nil then
      ADibHelper.Draw(ACanvas, ADestRect, SRCCOPY)
    else
      ACanvas.StretchDraw(ADestRect, ABitmap);
  finally
    UnprepareCanvas(ACanvas);
    ADibHelper.Free;
  end;
end;

procedure TdxDrawGraphicHelper.TCustomBitmapDrawer.PrintTransparentBitmap(ACanvas: TCanvas; ABitmap: TBitmap; const ADestRect: TRect; AMemoryDC: HDC);
var
  ADibHelper: TDIBHelper;
begin
  ADibHelper := TDIBHelper.Create(ABitmap);
  try
    if ADibHelper.Bits <> nil then
    begin
      ADibHelper.Draw(ACanvas, ADestRect, SRCINVERT);
      Mask(ACanvas.Handle, ADestRect, AMemoryDC, cxRect(0, 0, ABitmap.Width, ABitmap.Height));
      ADibHelper.Draw(ACanvas, ADestRect, SRCINVERT);
    end
    else
      PaintTransparentBitmap(ACanvas, ABitmap, ADestRect, AMemoryDC);
  finally
    ADibHelper.Free;
  end;
end;

procedure TdxDrawGraphicHelper.TCustomBitmapDrawer.ValidateBitmap(AWidth, AHeight: Integer);
var
  AReguiredPixelFormat: TPixelFormat;
  AFillColor: TColor;
begin
  if FBitmap = nil then
  begin
    FBitmap := TBitmap.Create;
    if Printing then
      AReguiredPixelFormat := pf32bit
    else
      AReguiredPixelFormat := pf24bit;
    FBitmap.HandleType := bmDIB;
    FBitmap.PixelFormat := AReguiredPixelFormat;
    FBitmap.Transparent := Transparent;
    if Transparent and (Owner.TransparentColor <> clNone) then
      FBitmap.TransparentColor := Owner.TransparentColor;
  end;
  FBitmap.SetSize(AWidth, AHeight);
  if Owner.TransparentColor <> clNone then
    AFillColor := Owner.TransparentColor
  else
    AFillColor := Owner.BackgroundColor;

  FBitmap.Canvas.Brush.Color := AFillColor;
  FBitmap.Canvas.FillRect(Rect(0, 0, AWidth, AHeight));
  DrawBitmapImage;
end;

{ TdxDrawGraphicHelper.TBitmapDrawer }

procedure TdxDrawGraphicHelper.TBitmapDrawer.Initialize;
begin
  if Printing then
    inherited Initialize;
end;

{ TdxDrawGraphicHelper.TImageListDrawer }

procedure TdxDrawGraphicHelper.TImageListDrawer.DrawBitmapImage;
begin
  Owner.ImageList.Draw(Bitmap.Canvas, 0, 0, Owner.ImageIndex, dsNormal, itImage);
end;

procedure TdxDrawGraphicHelper.TImageListDrawer.Initialize;
begin
  ValidateBitmap(Owner.ImageList.Width, Owner.ImageList.Height);
end;

procedure TdxDrawGraphicHelper.TImageListDrawer.Paint(ACanvas: TCanvas; const ADestRect: TRect);
begin
  DrawTransparentBitmap(ACanvas, Bitmap, ADestRect);
end;

{ TdxDrawGraphicHelper.TIconDrawer }

destructor TdxDrawGraphicHelper.TIconDrawer.Destroy;
begin
  FreeAndNil(FBits);
  inherited Destroy;
end;

procedure TdxDrawGraphicHelper.TIconDrawer.Initialize;
var
  ABitmap: TBitmap;
begin
  FBits.Free;
  ABitmap := TBitmap.Create;
  try
    ABitmap.Assign(Graphic);
    Assert(ABitmap.PixelFormat = pf32bit);
    FSize := TSize.Create(ABitmap.Width, ABitmap.Height);
    FBits := TDIBHelper.Create(ABitmap);
  finally
    ABitmap.Free;
  end;
end;

procedure TdxDrawGraphicHelper.TIconDrawer.Paint(ACanvas: TCanvas; const ADestRect: TRect);
var
  AGraphics: GPGraphics;
  AImage: GpImage;
begin
  AGraphics := BeginGdiPlusPaint(ACanvas);
  try
    GdipCheck(GdipCreateBitmapFromScan0(FSize.cx, FSize.cy, SizeOf(TdxAlphaColor) * FSize.cx, PixelFormat32bppARGB, FBits.Bits, AImage));
    try
      GdipCheck(GdipDrawImageRectI(AGraphics, AImage, ADestRect.Left, ADestRect.Top, ADestRect.Width, ADestRect.Height));
    finally
      GdipCheck(GdipDisposeImage(AImage));
    end;
  finally
    EndGdiPlusPaint(ACanvas, AGraphics);
  end;
end;

{ TdxDrawGraphicHelper.TGenericDrawer }

procedure TdxDrawGraphicHelper.TGenericDrawer.Paint(ACanvas: TCanvas; const ADestRect: TRect);
var
  ASavedTransparent: Boolean;
begin
  ASavedTransparent := Graphic.Transparent;
  try
    Graphic.Transparent := Transparent;
    if Transparent then
      DrawTransparentBitmap(ACanvas, Bitmap, ADestRect)
    else
      ACanvas.StretchDraw(ADestRect, Graphic);
  finally
    Graphic.Transparent := ASavedTransparent;
  end;
end;

procedure TdxDrawGraphicHelper.TGenericDrawer.Print(ACanvas: TCanvas; const ADestRect: TRect);
var
  ASavedTransparent: Boolean;
begin
  ASavedTransparent := Graphic.Transparent;
  try
    Graphic.Transparent := Transparent;
    if Transparent then
      DrawTransparentBitmap(ACanvas, Bitmap, ADestRect)
    else
      PrintBitmap(ACanvas, Bitmap, ADestRect);
  finally
    Graphic.Transparent := ASavedTransparent;
  end;
end;

{ TdxDrawGraphicHelper.TSmartImageDrawer.TdxGPImageHelper }

function TdxDrawGraphicHelper.TSmartImageDrawer.TdxGPImageHelper.GetGdipImageHandle: GPImage;
begin
  if HandleAsObject is TdxGPImageHandle then
    Result := TdxGPImageHandle(HandleAsObject).Handle
  else
    Result := nil;
end;

{ TdxDrawGraphicHelper.TSmartImageDrawer }

function TdxDrawGraphicHelper.TSmartImageDrawer.GetGdipImage: GpImage;
begin
  if Graphic is TdxGPImage then
    Result := TdxGPImage(Graphic).GetGdipImageHandle
  else
    Result := nil;
end;

procedure TdxDrawGraphicHelper.TSmartImageDrawer.Initialize;
begin
  if Printing then
  begin
    FGPImage := GetGdipImage;
    FUseTempImage := FGPImage = nil;
    if FUseTempImage then
      inherited Initialize;
  end;
end;

procedure TdxDrawGraphicHelper.TSmartImageDrawer.Export(AProc: TdxDrawGraphicHelperExportProc);
begin
  if FUseTempImage then
    inherited
  else
    AProc(Graphic, nil);
end;

procedure TdxDrawGraphicHelper.TSmartImageDrawer.Paint(ACanvas: TCanvas; const ADestRect: TRect);
begin
  ACanvas.StretchDraw(ADestRect, Graphic);
end;

procedure TdxDrawGraphicHelper.TSmartImageDrawer.Print(ACanvas: TCanvas; const ADestRect: TRect);
var
  AImage: GpImage;
  AGraphics: GpGraphics;
begin
  if FUseTempImage then
    GdipCheck(GdipCreateBitmapFromHBITMAP(Bitmap.Handle, Bitmap.Palette, AImage))
  else
    AImage := FGPImage;
  try
    AGraphics := BeginGdiPlusPaint(ACanvas);
    try
      GdipCheck(GdipDrawImageRectI(AGraphics, AImage, ADestRect.Left, ADestRect.Top, ADestRect.Width, ADestRect.Height));
    finally
      EndGdiPlusPaint(ACanvas, AGraphics);
    end;
  finally
    if FUseTempImage then
      GdipCheck(GdipDisposeImage(AImage));
  end;
end;

{ TdxDrawGraphicHelper }

destructor TdxDrawGraphicHelper.Destroy;
begin
  FreeAndNil(FDrawer);
  inherited Destroy;
end;

function TdxDrawGraphicHelper.CreateDrawer: TAbstractGraphicDrawer;
begin
  Result := GetDrawerClass.Create(Self);
end;

function TdxDrawGraphicHelper.GetDrawerClass: TAbstractGraphicDrawerClass;
begin
  if Graphic = nil then
    Result := TImageListDrawer
  else if Graphic is TdxGPImage then
    Result := TSmartImageDrawer
  else if Graphic is TBitmap then
    Result := TBitmapDrawer
  else if Graphic is TMetafile then
    Result := TMetaFileDrawer
  else if Graphic is TIcon then
    Result := TIconDrawer
  else
    Result := TGenericDrawer;
end;

procedure TdxDrawGraphicHelper.Draw(ACanvas: TCanvas; const ADestRect: TRect);
begin
  if FPrinting then
    Drawer.Print(ACanvas, ADestRect)
  else
    Drawer.Paint(ACanvas, ADestRect);
end;

procedure TdxDrawGraphicHelper.Export(AProc: TdxDrawGraphicHelperExportProc);
begin
  Drawer.Export(AProc);
end;

procedure TdxDrawGraphicHelper.Initialize(ABackgroundColor, ATransparentColor: TColor; AIsTransparent, APrinting: Boolean);
begin
  FBackgroundColor := ColorToRgb(ABackgroundColor);
  FTransparentColor := ColorToRgb(ATransparentColor);
  FTransparent := AIsTransparent;
  FPrinting := APrinting;
  FImageIndex := -1;
  FImageList := nil;
  FGraphic := nil;
end;

procedure TdxDrawGraphicHelper.Initialize(AGraphic: TGraphic;
  ABackgroundColor, ATransparentColor: TColor; AIsTransparent, APrinting: Boolean);
begin
  Initialize(ABackgroundColor, ATransparentColor, AIsTransparent, APrinting);
  FGraphic := AGraphic;
  ValidateDrawer;
end;

procedure TdxDrawGraphicHelper.Initialize(AImageList: TCustomImageList; AImageIndex: Integer;
  ABackgroundColor, ATransparentColor: TColor; AIsTransparent, APrinting: Boolean);
begin
  Initialize(ABackgroundColor, ATransparentColor, AIsTransparent, APrinting);
  FImageList := AImageList;
  FImageIndex := AImageIndex;
  ValidateDrawer;
end;

procedure TdxDrawGraphicHelper.ValidateDrawer;
begin
  if FDrawer = nil then
    FDrawer := CreateDrawer
  else
    if FDrawer.ClassType <> GetDrawerClass then
    begin
      FDrawer.Free;
      FDrawer := CreateDrawer;
    end;
  FDrawer.Initialize;
end;

{ TdxPSReportRenderCustomCanvas }

constructor TdxPSReportRenderCustomCanvas.Create;
begin
  inherited Create;
  FFont := TFont.Create;
  FFont.OnChange := FontChangeHandler;
end;

destructor TdxPSReportRenderCustomCanvas.Destroy;
begin
  FreeAndNil(FFont);
  inherited Destroy;
end;

function TdxPSReportRenderCustomCanvas.CalculateLineThickness(
  AUnitsPerPixel, AUnitsPerPoint: Integer): Integer;
begin
  if IsPrinterCanvas then
    Result := Max(MulDiv(AUnitsPerPoint, 1, 2), 1)
  else
    Result := AUnitsPerPixel;
end;

procedure TdxPSReportRenderCustomCanvas.AssignFont(ATarget, ASource: TFont);
begin
  ATarget.Assign(ASource);
  if ATarget.PixelsPerInch <> ASource.PixelsPerInch then
    ATarget.Height := ASource.Height;
end;

procedure TdxPSReportRenderCustomCanvas.FontChangeHandler(Sender: TObject);
begin
  FontChanged;
end;

procedure TdxPSReportRenderCustomCanvas.FixupRect(var R: TRect);
begin
  LogicalToDeviceCoordinates(R);
  DeviceToLogicalCoordinates(R);
end;

procedure TdxPSReportRenderCustomCanvas.PrepareLogicalUnits;
begin
end;

procedure TdxPSReportRenderCustomCanvas.UnprepareLogicalUnits;
begin
end;

procedure TdxPSReportRenderCustomCanvas.AssignCanvasFont(ACanvas: TcxCanvas);
begin
  AssignFont(ACanvas.Font, Font);
end;

procedure TdxPSReportRenderCustomCanvas.FontChanged;
begin
end;

function TdxPSReportRenderCustomCanvas.GetSupportsTransparentImagesDrawing: Boolean;
begin
  Result := True;
end;

procedure TdxPSReportRenderCustomCanvas.FillEdge(const R: TRect;
  ABackColor, AEdgeColor: TColor; AIsVerticalOrientation: Boolean; AEdgePattern: TClass);
var
  ARegion: TcxRegionHandle;
begin
  ARegion := CreateRectRgnIndirect(R);
  FillEdge(ARegion, ABackColor, AEdgeColor, AIsVerticalOrientation, AEdgePattern);
  DeleteObject(ARegion);
end;

procedure TdxPSReportRenderCustomCanvas.FillEdge(ARegion: TcxRegionHandle;
  ABackColor, AEdgeColor: TColor; AIsVerticalOrientation: Boolean; AEdgePattern: TClass);
var
  ABrush: TBrush;
begin
  if (AEdgePattern <> nil) and AEdgePattern.InheritsFrom(TdxPSEdgePattern) then
  begin
    if TdxPSEdgePatternClass(AEdgePattern).Solid then
    begin
      ABrush := TBrush.Create;
      try
        ABrush.Color := AEdgeColor;
        FillRegion(ARegion, ABackColor, AEdgeColor, TdxPSSolidFillPattern, ABrush);
      finally
        ABrush.Free;
      end;
    end
    else
      DoFillEdge(ARegion, ABackColor, AEdgeColor, AIsVerticalOrientation, AEdgePattern);
  end;
end;

procedure TdxPSReportRenderCustomCanvas.DoFillEdge(ARegion: TcxRegionHandle; ABackColor, AEdgeColor: TColor;
  AIsVerticalOrientation: Boolean; AEdgePattern: TClass{TdxPSEdgePatternClass});
const
  OrientationMap: array[Boolean] of TdxPSCellEdgePatternOrientation = (cepoHorizontal, cepoVertical);
var
  APatternItem: TdxPSEdgePatternItem;
begin
  APatternItem := dxPSEdgePatternFactory.Items[TdxPSEdgePatternClass(AEdgePattern), IsPrinterCanvas];
  FillRegion(ARegion, ABackColor, AEdgeColor, nil, APatternItem.Brushes[OrientationMap[AIsVerticalOrientation]]);
end;

procedure TdxPSReportRenderCustomCanvas.PrepareCanvasForCustomDraw(AFont: TFont; AColor: TColor);
begin
  if AColor <> clDefault then
    Brush.Color := AColor;
  if Assigned(AFont) then
  begin
    Font := AFont;
    Font.PixelsPerInch := AFont.PixelsPerInch;
  end;
end;

procedure TdxPSReportRenderCustomCanvas.SetFont(AValue: TFont);
begin
  if AValue <> nil then
    AssignFont(FFont, AValue)
end;

{ TdxPSReportRenderCanvas }

constructor TdxPSReportRenderCanvas.Create(ACanvas: TCanvas);
begin
  inherited Create;
  FCanvas := TcxCanvas.Create(ACanvas);
end;

destructor TdxPSReportRenderCanvas.Destroy;
begin
  FreeAndNil(FCanvas);
  inherited Destroy;
end;

function TdxPSReportRenderCanvas.CalculatePictureRect(APicture: TGraphic;
  const R: TRect; APictureMode: TdxPicturePaintMode; ANumerator, ADenominator: Integer): TRect;
var
  W, H, V: Integer;
begin
  case APictureMode of
    ppmCenter:
      Result := cxRectCenter(R,
        MulDiv(APicture.Width, ANumerator, ADenominator),
        MulDiv(APicture.Height, ANumerator, ADenominator));

    ppmProportional:
      begin
        W := cxRectWidth(R);
        H := cxRectHeight(R);
        if APicture.Width / APicture.Height > W / H then
        begin
          V := MulDiv(APicture.Height, W, APicture.Width);
          Result := Bounds(R.Left, R.Top + (H - V) div 2, W, V);
        end
        else
        begin
          V := MulDiv(APicture.Width, H, APicture.Height);
          Result := Bounds(R.Left + (W - V) div 2, R.Top, V, H);
        end;
      end;

    else
      Result := R
  end;
end;

function TdxPSReportRenderCanvas.CreatePatternBitmap(ABackColor, AForeColor: TColor;
  const R: TRect; ARequiredBrushOrigin: Boolean; ABrush: TBrush): TcxBitmap;

  procedure PrepareBrushOrigin(ABitmap: TcxBitmap; R: TRect);
  var
    P: TPoint;
  begin
    if ARequiredBrushOrigin then
    begin
      LogicalToDeviceCoordinates(R);
      P := Point(R.Left mod ABitmap.Width, R.Top mod ABitmap.Height);
    end
    else
      P := BrushOrg;

    SetBrushOrgEx(ABitmap.Canvas.Handle, P.X, P.Y, nil);
  end;

begin
  Result := TcxBitmap.CreateSize(ABrush.Bitmap.Width, ABrush.Bitmap.Height);
  Result.Canvas.Font.Assign(Font);
  Result.Canvas.Font.Color := AForeColor;
  Result.Canvas.Brush.Assign(ABrush);
  SetBkColor(Result.Canvas.Handle, ColorToRGB(ABackColor));
  PrepareBrushOrigin(Result, R);
  Result.Canvas.FillRect(Result.ClientRect);
end;

function TdxPSReportRenderCanvas.GetBrush: TBrush;
begin
  Result := Canvas.Brush;
end;

function TdxPSReportRenderCanvas.GetBrushOrg: TPoint;
begin
  GetBrushOrgEx(Canvas.Handle, Result);
end;

function TdxPSReportRenderCanvas.GetPixelsPerInch: Integer;
begin
  Result := GetDeviceCaps(Canvas.Handle, LOGPIXELSX);
end;

procedure TdxPSReportRenderCanvas.InternalDrawFrameControl(
  const R: TRect; AType, AState: Integer);
begin
  DrawFrameControl(Canvas.Handle, R, AType, AState);
end;

procedure TdxPSReportRenderCanvas.InternalFillRectByBrushBitmap(const R: TRect;
  ABackColor, AForeColor: TColor; ARequiredBrushOrigin: Boolean; ABrush: TBrush);
var
  ABitmap: TcxBitmap;
begin
  ABitmap := CreatePatternBitmap(ABackColor, AForeColor, R, ARequiredBrushOrigin, ABrush);
  try
    DrawPicture(ABitmap, R, ppmTile, PixelsNumerator, PixelsDenominator);
  finally
    ABitmap.Free;
  end;
end;

procedure TdxPSReportRenderCanvas.DrawPictureStretch(APicture: TGraphic;
  const R: TRect; ANumerator, ADenominator: Integer);
var
  ABitmap: TcxBitmap;
  AWidth, AHeight: Integer;
begin
  if APicture is TMetafile then
    Canvas.Canvas.StretchDraw(R, APicture)
  else
  begin
    AWidth := MulDiv(cxRectWidth(R), ADenominator, ANumerator);
    AHeight := MulDiv(cxRectHeight(R), ADenominator, ANumerator);
    ABitmap := TcxBitmap.CreateSize(AWidth, AHeight, pf24bit);
    try
      cxStretchBlt(ABitmap.Canvas.Handle, Canvas.Handle, ABitmap.ClientRect, R, SRCCOPY);
      ABitmap.Canvas.StretchDraw(ABitmap.ClientRect, APicture);
      cxStretchBlt(Canvas.Handle, ABitmap.Canvas.Handle, R, ABitmap.ClientRect, SRCCOPY);
    finally
      ABitmap.Free;
    end;
  end;
end;

procedure TdxPSReportRenderCanvas.DrawPictureTile(APicture: TGraphic;
  const R: TRect; APictureWidth, APictureHeight, ANumerator, ADenominator: Integer);
var
  ACountX, ACountY: Integer;
  AImageRect: TRect;
  X, Y: Integer;
begin
  AImageRect := Bounds(0, 0, APictureWidth, APictureHeight);
  ACountX := cxRectWidth(R) div APictureWidth;
  ACountY := cxRectHeight(R) div APictureHeight;
  for X := 0 to ACountX do
    for Y := 0 to ACountY do
    begin
      OffsetRect(AImageRect, -AImageRect.Left, -AImageRect.Top);
      OffsetRect(AImageRect, R.Left + X * APictureWidth, R.Top + Y * APictureHeight);
      DrawPictureStretch(APicture, AImageRect, ANumerator, ADenominator);
    end;
end;

procedure TdxPSReportRenderCanvas.DrawCheckBox(var R: TRect;
  AChecked, AEnabled, AIsRadio: Boolean; AEdgeStyle: TdxCheckButtonEdgeStyle;
  AMarlettFont: TFont; ALineThickness: Integer; ABorderColor: TColor = clWindowText);

  procedure DrawCheckBoxGlyph(AGlyphIndex: Byte; AColor: TColor = clDefault);
  begin
    DrawGlyph(cxRectInflate(R, ALineThickness, ALineThickness),
      AMarlettFont, AGlyphIndex, True, AColor);
  end;

  procedure DrawCheckBoxBorders(AIsRadio: Boolean;
    AEdgeStyle: TdxCheckButtonEdgeStyle; ABorderColor: TColor);
  const
    TopLeftArcInnerIndexes: array[Boolean] of Integer =
      (CheckTopLeftArcInnerIndex, RadioTopLeftArcInnerIndex);
    BottomRightArcInnerIndexes: array[Boolean] of Integer =
      (CheckBottomRightArcInnerIndex, RadioBottomRightArcInnerIndex);
    TopLeftArcOuterIndexes: array[Boolean] of Integer =
      (CheckTopLeftArcOuterIndex, RadioTopLeftArcOuterIndex);
    BottomRightArcOuterIndexes: array[Boolean] of Integer =
      (CheckBottomRightArcOuterIndex, RadioBottomRightArcOuterIndex);
  var
    ATopLeftColor, ARightBottomColor: TColor;
  begin
    if AEdgeStyle <> cbesNone then
    begin
      GetBorderColors(AEdgeStyle, ABorderColor, True, ATopLeftColor, ARightBottomColor);
      DrawCheckBoxGlyph(TopLeftArcInnerIndexes[AIsRadio], ATopLeftColor);
      DrawCheckBoxGlyph(BottomRightArcInnerIndexes[AIsRadio], ARightBottomColor);
      if AEdgeStyle in [cbes3D, cbesSoft3D, cbesBoldFlat] then
      begin
        GetBorderColors(AEdgeStyle, ABorderColor, False, ATopLeftColor, ARightBottomColor);
        DrawCheckBoxGlyph(TopLeftArcOuterIndexes[AIsRadio], ATopLeftColor);
        DrawCheckBoxGlyph(BottomRightArcOuterIndexes[AIsRadio], ARightBottomColor);
      end;
    end;
  end;

const
  EnabledColorMap: array[Boolean] of TColor = (clBtnFace, clWindow);
  InteriorIndexes: array[Boolean] of Integer = (CheckInteriorIndex, RadioInteriorIndex);
  MarkIndexes: array[Boolean] of Integer = (CheckMarkIndex, RadioBeanIndex);
begin
  if IsRectVisible(R) then
  begin
    SaveState;
    try
      DrawCheckBoxGlyph(InteriorIndexes[AIsRadio], EnabledColorMap[AEnabled]);
      if AChecked then
        DrawCheckBoxGlyph(MarkIndexes[AIsRadio], clWindowText);
      DrawCheckBoxBorders(AIsRadio, AEdgeStyle, ABorderColor);
    finally
      RestoreState;
    end;
  end;
end;

procedure TdxPSReportRenderCanvas.DrawEllipseFrame(
  const R: TRect; AColor: TColor; AThickness: Integer);
var
  AOuterRgn, AInnerRgn: TcxRegionHandle;
begin
  AOuterRgn := CreateEllipticRgnIndirect(R);
  AInnerRgn := CreateEllipticRgnIndirect(cxRectInflate(R, -AThickness, -AThickness));
  CombineRgn(AOuterRgn, AOuterRgn, AInnerRgn, RGN_DIFF);
  Canvas.FillRegion(AOuterRgn, AColor);
  DeleteObject(AInnerRgn);
  DeleteObject(AOuterRgn);
end;

procedure TdxPSReportRenderCanvas.DrawExpandButton(R: TRect;
  AEdgeStyle: TdxCheckButtonEdgeStyle; AMarlettFont, ASymbolFont: TFont;
  AExpanded, AShadow, AFillInterior: Boolean; ABorderColor, ABackgroundColor: TColor;
  ALineThickness: Integer);

  procedure DrawExpandButtonBorders(const R: TRect; AMarlettFont: TFont;
    ABorderColor: TColor; AEdgeStyle: TdxCheckButtonEdgeStyle; ALineThickness: Integer);
  var
    ATopLeftColor, ARightBottomColor: TColor;
  begin
    if AEdgeStyle <> cbesNone then
    begin
      Font := AMarlettFont;
      GetBorderColors(AEdgeStyle, ABorderColor, True, ARightBottomColor, ATopLeftColor);
      DrawGlyph(R, AMarlettFont, CheckTopLeftArcInnerIndex, True, ATopLeftColor);
      DrawGlyph(R, AMarlettFont, CheckBottomRightArcInnerIndex, True, ARightBottomColor);
      if AEdgeStyle in [cbes3D, cbesSoft3D, cbesBoldFlat] then
      begin
        GetBorderColors(AEdgeStyle, ABorderColor, False, ARightBottomColor, ATopLeftColor);
        DrawGlyph(R, AMarlettFont, CheckTopLeftArcOuterIndex, True, ATopLeftColor);
        DrawGlyph(R, AMarlettFont, CheckBottomRightArcOuterIndex, True, ARightBottomColor);
      end;
    end;
  end;

  procedure DrawExpandButtonGlyph(R: TRect; ASymbolFont: TFont;
    ATextColor: TColor; AExpanded: Boolean; ALineThickness: Integer);
  const
    CrossHireIndexes: array[Boolean] of Byte = (PlusSignIndex, MinusSignIndex);
  begin
    DrawGlyph(cxRectOffset(R, 0, -ALineThickness),
      ASymbolFont, CrossHireIndexes[AExpanded], True, ATextColor);
  end;

begin
  if IsRectVisible(R) then
  begin
    SaveState;
    try
      if AFillInterior then
        FillRect(cxRectInflate(R, -ALineThickness, -ALineThickness), ABackgroundColor);
      R := cxRectInflate(R, ALineThickness, ALineThickness);
      DrawExpandButtonBorders(R, AMarlettFont, ABorderColor, AEdgeStyle, ALineThickness);
      DrawExpandButtonGlyph(R, ASymbolFont, ABorderColor, AExpanded, ALineThickness);
    finally
      RestoreState;
    end;
  end;
end;

procedure TdxPSReportRenderCanvas.DrawFrame(
  const R: TRect; ATopLeftColor, ARightBottomColor: TColor;
  ABorderWidth: Integer = 1; ABorders: TcxBorders = cxBordersAll);
begin
  Canvas.DrawComplexFrame(R, ATopLeftColor, ARightBottomColor, ABorders, ABorderWidth);
end;

procedure TdxPSReportRenderCanvas.DrawGlyph(const R: TRect; AGlyphFont: TFont;
  AGlyphIndex: Byte; ACenterOnRect: Boolean; ATextColor: TColor = clWindowText);
const
  TextFormatMap: array[Boolean] of Integer = (
    CXTO_LEFT, CXTO_CENTER_HORIZONTALLY or CXTO_CENTER_VERTICALLY
  );
var
  R1: TRect;
begin
  R1 := R;
  Brush.Style := bsClear;
  if ATextColor = clDefault then
    ATextColor := clWindowText;
  DrawText(R1, Chr(AGlyphIndex), AGlyphFont, TextFormatMap[ACenterOnRect], ATextColor);
end;

procedure TdxPSReportRenderCanvas.DrawGraphic(AHelper: TdxDrawGraphicHelper; const R: TRect);
begin
  AHelper.Draw(Canvas.Canvas, R);
end;

procedure TdxPSReportRenderCanvas.DrawPicture(APicture: TGraphic; const R: TRect;
  APictureMode: TdxPicturePaintMode; ANumerator, ADenominator: Integer;
  AOffsetX: Integer = 0; AOffsetY: Integer = 0);
var
  W, H: Integer;
begin
  if Assigned(APicture) and not APicture.Empty then
  begin
    if APictureMode <> ppmTile then
      DrawPictureStretch(APicture, CalculatePictureRect(APicture, R, APictureMode, ANumerator, ADenominator), ANumerator, ADenominator)
    else
    begin
      SaveClipRgn;
      try
        IntersectClipRgn(R);
        W := MulDiv(APicture.Width, ANumerator, ADenominator);
        H := MulDiv(APicture.Height, ANumerator, ADenominator);
        DrawPictureTile(APicture, cxRectOffset(R, AOffsetX mod W, AOffsetY mod H), W, H, ANumerator, ADenominator);
      finally
        RestoreClipRgn;
      end;
    end;
  end;
end;

procedure TdxPSReportRenderCanvas.DrawRoundFrame(const R: TRect;
  AEllipseWidth, AEllipseHeight: Integer; AColor: TColor; AThickness: Integer);
var
  ARegion: TcxRegion;
begin
  ARegion := TcxRegion.CreateRoundCorners(R, AEllipseWidth, AEllipseHeight);
  try
    AEllipseHeight := MulDiv(AEllipseHeight,
      cxRectHeight(R) - 2 * AThickness, cxRectHeight(R));
    AEllipseWidth := MulDiv(AEllipseWidth,
      cxRectWidth(R) - 2 * AThickness, cxRectWidth(R));
    ARegion.Combine(TcxRegion.CreateRoundCorners(
      cxRectInflate(R, -AThickness, -AThickness), AEllipseWidth, AEllipseHeight),
      roSubtract);
    Canvas.FillRegion(ARegion, AColor);
  finally
    ARegion.Free;
  end;
end;

procedure TdxPSReportRenderCanvas.FillEllipse(const R: TRect;
  ABackColor, AForeColor: TColor; APattern: TdxPSFillPatternClass; APatternBrush: TBrush);
var
  ARegion: TcxRegionHandle;
begin
  ARegion := CreateEllipticRgnIndirect(R);
  FillRegion(ARegion, ABackColor, AForeColor, APattern, APatternBrush);
  DeleteObject(ARegion);
end;

procedure TdxPSReportRenderCanvas.FillRect(const R: TRect; AColor: TColor);
begin
  Canvas.FillRect(R, AColor);
end;

procedure TdxPSReportRenderCanvas.FillRegion(ARegion: TcxRegionHandle;
  ABackColor, AForeColor: TColor; APattern: TdxPSFillPatternClass; APatternBrush: TBrush);
var
  AClipRegion: TcxRegion;
  R: TRect;
begin
  if GetRgnBox(ARegion, R) <> NULLREGION then
  begin
    SaveState;
    try
      if IsSolidFillPattern(APattern, APatternBrush) then
        Canvas.FillRegion(ARegion, APatternBrush.Color)
      else
      begin
        SaveClipRgn;
        try
          AClipRegion := Canvas.GetClipRegion(True);
          CombineRgn(AClipRegion.Handle, AClipRegion.Handle, ARegion, RGN_OR);
          Canvas.SetClipRegion(AClipRegion, roSet);
          InternalFillRectByBrushBitmap(R, ABackColor, AForeColor,
            Assigned(APattern) and APattern.RequiredBrushOrigin, APatternBrush);
        finally
          RestoreClipRgn;
        end;
      end;
    finally
      RestoreState;
    end;
  end;
end;

procedure TdxPSReportRenderCanvas.FillRoundRect(R: TRect;
  AEllipseWidth, AEllipseHeight: Integer; ABackColor, AForeColor: TColor;
  APattern: TdxPSFillPatternClass; APatternBrush: TBrush);
var
  ARegion: TcxRegion;
begin
  ARegion := TcxRegion.CreateRoundCorners(R, AEllipseWidth, AEllipseHeight);
  try
    FillRegion(ARegion.Handle, ABackColor, AForeColor, APattern, APatternBrush);
  finally
    ARegion.Free;
  end;
end;

procedure TdxPSReportRenderCanvas.Polyline(
  const APoints: array of TPoint; AColor: TColor; ALineWidth: Integer);
begin
  SaveState;
  try
    if AColor <> clDefault then
      Canvas.Pen.Color := AColor;
    Canvas.Pen.Width := ALineWidth;
    Canvas.Polyline(APoints);
  finally
    RestoreState;
  end;
end;

procedure TdxPSReportRenderCanvas.Polygon(
  const APoints: array of TPoint; AColor, ABackgroundColor: TColor;
  ALineWidth: Integer; AFillMode: Integer = ALTERNATE);
begin
  SaveState;
  try
    if AColor <> clDefault then
      Canvas.Pen.Color := AColor;
    if ABackgroundColor <> clDefault then
      Canvas.Brush.Color := ABackgroundColor;
    SetPolyFillMode(Canvas.Handle, AFillMode);
    Canvas.Pen.Width := ALineWidth;
    Canvas.Polygon(APoints);
  finally
    RestoreState;
  end;
end;

procedure TdxPSReportRenderCanvas.GetBorderColors(
  AEdgeStyle: TdxCheckButtonEdgeStyle; ADefaultBorderColor: TColor;
  AInnerBorder: Boolean; out ATopLeftBorderColor, ARightBottomBorderColor: TColor);
const
  Soft3DTopLeftBorders: array[Boolean] of TColor = (clBtnShadow, clBtnFace);
  Soft3DRightBottomBorders: array[Boolean] of TColor = (clBtnHighlight, clBtnFace);
  Custom3DTopLeftBorders: array[Boolean] of TColor = (clBtnShadow, cl3DDkShadow);
  Custom3DRightBottomBorders: array[Boolean] of TColor = (clBtnHighlight, cl3DLight);
begin
  case AEdgeStyle of
    cbesSingle, cbesBoldFlat:
      ATopLeftBorderColor := ADefaultBorderColor;
    cbesUltraFlat:
      ATopLeftBorderColor := clWindowText;
    cbesSoft3D:
      ATopLeftBorderColor := Soft3DTopLeftBorders[AInnerBorder];
    cbes3D:
      ATopLeftBorderColor := Custom3DTopLeftBorders[AInnerBorder];
    else
      ATopLeftBorderColor := clDefault;
  end;
  case AEdgeStyle of
    cbes3D:
      ARightBottomBorderColor := Custom3DRightBottomBorders[AInnerBorder];
    cbesSoft3D:
      ARightBottomBorderColor := Soft3DRightBottomBorders[AInnerBorder];
    else
      ARightBottomBorderColor := ATopLeftBorderColor;
  end;
end;

function TdxPSReportRenderCanvas.IsRectVisible(const R: TRect): Boolean;
begin
  Result := RectVisible(Canvas.Handle, R);
end;

procedure TdxPSReportRenderCanvas.SetBrush(AValue: TBrush);
begin
  Canvas.Brush.Assign(AValue);
end;

procedure TdxPSReportRenderCanvas.SetBrushOrg(const AValue: TPoint);
begin
  SetBrushOrgEx(Canvas.Handle, AValue.X, AValue.Y, nil);
end;

procedure TdxPSReportRenderCanvas.FontChanged;
begin
  AssignCanvasFont(Canvas);
end;

function TdxPSReportRenderCanvas.OffsetWindowOrg(const P: TPoint): TPoint;
var
  ANewWindowOrg: TPoint;
begin
  Result := WindowOrg;
  ANewWindowOrg := Result;
  Inc(ANewWindowOrg.X, P.X);
  Inc(ANewWindowOrg.Y, P.Y);
  SetWindowOrg(ANewWindowOrg);
end;

function TdxPSReportRenderCanvas.GetWindowExt: TSize;
begin
  GetWindowExtEx(Canvas.Handle, Result);
end;

function TdxPSReportRenderCanvas.GetWindowOrg: TPoint;
begin
  GetWindowOrgEx(Canvas.Handle, Result);
end;

function TdxPSReportRenderCanvas.GetWorldTransform: TXForm;
begin
  if not Windows.GetWorldTransform(Canvas.Handle, Result) then
    Result := TXForm.CreateIdentityMatrix;
end;

function TdxPSReportRenderCanvas.IsSolidFillPattern(APattern: TdxPSFillPatternClass; APatternBrush: TBrush): Boolean;
begin
  Result := (Assigned(APattern) and APattern.Solid) or
    (APatternBrush.Style = bsSolid) and
    ((APatternBrush.Bitmap = nil) or APatternBrush.Bitmap.Empty);
end;

procedure TdxPSReportRenderCanvas.SetWindowExt(const AValue: TSize);
begin
  SetWindowExtEx(Canvas.Handle, AValue.cx, AValue.cy, nil);
end;

procedure TdxPSReportRenderCanvas.SetWindowOrg(const P: TPoint);
begin
  SetWindowOrgEx(Canvas.Handle, P.X, P.Y, nil);
end;

procedure TdxPSReportRenderCanvas.SetWorldTransform(const Value: TXForm);
begin
  SetGraphicsMode(Canvas.Handle, GM_ADVANCED);
  Windows.SetWorldTransform(Canvas.Handle, Value);
end;

function TdxPSReportRenderCanvas.CalculateTextParams(ADrawTextFormat: Integer): TcxTextParams;
begin
  Result := cxCalcTextParams(Canvas.Handle, ADrawTextFormat);
end;

function TdxPSReportRenderCanvas.CalculateTextRect(const AText: string; var R: TRect;
  AFormat: TcxTextOutFormat = CXTO_DEFAULT_FORMAT; AFont: TFont = nil; AMaxLineCount: Integer = 0;
  ALeftIndent: Integer = 0; ARightIndent: Integer = 0; ATextColor: TColor = clDefault; ALineSpacing: Single = 1): Integer;
begin
  Result := cxTextOut(Canvas.Handle, AText, R, AFormat, AFont, AMaxLineCount, ALeftIndent, ARightIndent, ATextColor, ALineSpacing);
end;

procedure TdxPSReportRenderCanvas.DrawText(var R: TRect; const AText: string; AFont: TFont;
  AFormat: Cardinal; ATextColor: TColor = clDefault; AMaxLineCount: Integer = 0; ALineSpacing: Single = 1.0);
begin
  cxTextOut(Canvas.Handle, AText, R, AFormat, AFont, AMaxLineCount, 0, 0, ATextColor, ALineSpacing);
end;

procedure TdxPSReportRenderCanvas.ExtTextOut(const ABounds: TRect; AFont: TFont;
  const AText: string; AGlyphs: PWord; AGlyphCount: Integer; AGlyphWidths: PInteger);
var
  APrevBkMode: Integer;
  APrevObject: HFONT;
begin
  APrevObject := SelectObject(Canvas.Handle, AFont.Handle);
  try
    APrevBkMode := SetBkMode(Canvas.Handle, TRANSPARENT);
    try
      SetTextColor(Canvas.Handle, ColorToRGB(AFont.Color));
      Windows.ExtTextOut(Canvas.Handle, ABounds.Left, ABounds.Top,
        ETO_GLYPH_INDEX, ABounds, PChar(AGlyphs), AGlyphCount, AGlyphWidths);
    finally
      SetBkMode(Canvas.Handle, APrevBkMode);
    end;
  finally
    SelectObject(Canvas.Handle, APrevObject);
  end;
end;

function TdxPSReportRenderCanvas.MakeTextRows(AText: PChar; ATextLength: Integer;
  const R: TRect; const ATextParams: TcxTextParams; var ATextRows: TcxTextRows;
  out ACount: Integer; AMaxLineCount: Integer = 0): Boolean;
begin
  cxResetTextRows(ATextRows);
  Result := cxMakeTextRows(Canvas.Handle, AText, ATextLength, R, ATextParams, ATextRows, ACount, AMaxLineCount);
end;

function TdxPSReportRenderCanvas.TextSize(const AText: string): TSize;
begin
  Result := Canvas.TextExtent(AText);
end;

procedure TdxPSReportRenderCanvas.RotatedTextOut(const ABounds: TRect; const AText: string; AFont: TFont;
  AAlignHorz: TcxTextAlignX = taCenterX; AAlignVert: TcxTextAlignY = taCenterY; AWordBreak: Boolean = True;
  ADirection: TcxVerticalTextOutDirection = vtdBottomToTop);
begin
  cxRotatedTextOut(Canvas.Handle, ABounds, AText, AFont,
    AAlignHorz, AAlignVert, AWordBreak, True, True, ADirection);
end;

procedure TdxPSReportRenderCanvas.SetCanvasExts(const APageSize: TPoint;
  AMappingMode: TdxPSReportRenderCanvasMappingMode; AScaleNumerator: Integer;
  AScaleDenominator: Integer; const AViewPort: TRect);
const
  MappingModeFlags: array[TdxPSReportRenderCanvasMappingMode] of Integer = (
    0, MM_TEXT, MM_LOMETRIC, MM_HIMETRIC, MM_LOENGLISH, MM_HIENGLISH, MM_TWIPS,
    MM_ISOTROPIC, MM_ANISOTROPIC);
begin
  if AMappingMode <> rrmmDefault then
    SetMapMode(Canvas.Handle, MappingModeFlags[AMappingMode]);
  WindowExt := cxSize(APageSize.X, APageSize.Y);
  ScaleWindowExtEx(Canvas.Handle, AScaleNumerator, AScaleDenominator, AScaleNumerator, AScaleDenominator, nil);
  SetViewPortExtEx(Canvas.Handle, cxRectWidth(AViewPort), cxRectHeight(AViewPort), nil);
  SetViewPortOrgEx(Canvas.Handle, AViewPort.Left, AViewPort.Top, nil);
end;

function TdxPSReportRenderCanvas.IsPrinterCanvas: Boolean;
begin
  Result := IsPrinterDC(Canvas.Handle);
end;

procedure TdxPSReportRenderCanvas.DeviceToLogicalCoordinates(var R: TRect);
begin
  DPtoLP(Canvas.Handle, R, 2);
end;

procedure TdxPSReportRenderCanvas.LogicalToDeviceCoordinates(var R: TRect);
begin
  LPtoDP(Canvas.Handle, R, 2);
end;

function TdxPSReportRenderCanvas.ExcludeClipRect(const R: TRect): Integer;
begin
  with R do
    Result := Windows.ExcludeClipRect(Canvas.Handle, Left, Top, Right, Bottom);
end;

function TdxPSReportRenderCanvas.IntersectClipRgn(const R: TRect): Integer;
begin
  with R do
    Result := Windows.IntersectClipRect(Canvas.Handle, Left, Top, Right, Bottom);
end;

function TdxPSReportRenderCanvas.BeginCustomDraw(const AClipBounds: TRect; AFont: TFont; AColor: TColor): TCanvas;
begin
  Canvas.SaveState;
  Result := Canvas.Canvas;
  PrepareCanvasForCustomDraw(AFont, AColor);
  IntersectClipRgn(AClipBounds);
end;

procedure TdxPSReportRenderCanvas.EndCustomDraw(var ACanvas: TCanvas);
begin
  Canvas.RestoreState;
end;

procedure TdxPSReportRenderCanvas.RestoreState;
begin
  Canvas.RestoreState;
end;

procedure TdxPSReportRenderCanvas.SaveState;
begin
  Canvas.SaveState;
  AssignCanvasFont(Canvas);
end;

procedure TdxPSReportRenderCanvas.RestoreClipRgn;
begin
  Canvas.RestoreClipRegion;
end;

procedure TdxPSReportRenderCanvas.SaveClipRgn;
begin
  Canvas.SaveClipRegion;
end;

{ TdxPSReportRenderScreenCanvas }

constructor TdxPSReportRenderScreenCanvas.Create;
begin
  FSourceCanvas := TCanvas.Create;
  FSourceCanvas.Lock;
  FSourceCanvas.Handle := GetDC(0);
  inherited Create(FSourceCanvas);
end;

destructor TdxPSReportRenderScreenCanvas.Destroy;
begin
  inherited Destroy;
  ReleaseDC(0, FSourceCanvas.Handle);
  FSourceCanvas.Handle := 0;
  FSourceCanvas.Unlock;
  FreeAndNil(FSourceCanvas);
end;

{ TdxPSAdjustFontSizeHelper }

constructor TdxPSAdjustFontSizeHelper.Create(ACanvas: TdxPSReportRenderCustomCanvas);
begin
  inherited Create;
  FCanvas := ACanvas;
end;

function TdxPSAdjustFontSizeHelper.CheckFontSize(ALastSuitableSize, AOriginalSize, AMinSize: Integer): Integer;
begin
  if ALastSuitableSize > 0 then
    Result := ALastSuitableSize
  else
    Result := AOriginalSize;
end;

function TdxPSAdjustFontSizeHelper.GetMinFontSize: Integer;
begin
  Result := MulDiv(Font.Size, 2, 3);
end;

function TdxPSAdjustFontSizeHelper.GetTextHeight(const AText: string; AWidth: Integer; ALineSpacing: Single): Integer;
var
  R: TRect;
begin
  R := cxRect(0, 0, AWidth, MaxWord);
  FCanvas.Font := Font;
  FCanvas.CalculateTextRect(AText, R, FFlags or CXTO_CALCRECT, nil, 0, 0, 0, clDefault, ALineSpacing);
  Result := cxRectHeight(R);
end;

function TdxPSAdjustFontSizeHelper.GetTextWidth(const AText: string): Integer;
begin
  FCanvas.Font := Font;
  Result := FCanvas.TextSize(AText).cx;
end;

end.
