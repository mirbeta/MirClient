{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressRichEditControl                                   }
{                                                                    }
{           Copyright (c) 2000-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSRICHEDITCONTROL AND ALL        }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM       }
{   ONLY.                                                            }
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

unit dxRichEdit.Platform.Win.Painter;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
{$IFDEF DELPHIXE2}
  System.UITypes,
{$ENDIF}
  Windows, Classes, Types, SysUtils, Graphics, Generics.Defaults, Generics.Collections,
  dxCore, cxGeometry, dxCoreClasses, dxCoreGraphics, cxGraphics, dxGDIPlusAPI, dxGDIPlusClasses,

  dxRichEdit.Utils.Graphics,
  dxGenerics,
  dxRichEdit.Utils.Types,
  dxRichEdit.Utils.OfficeImage,
  dxRichEdit.Utils.ModelUnitConverter,
  dxRichEdit.DocumentModel.InlineObjectFormatting,
  dxRichEdit.DocumentModel.Boxes,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.LayoutEngine.BoxMeasurer,
  dxRichEdit.Platform.Font,
  dxRichEdit.Platform.Win.Font,
  dxRichEdit.Platform.PatternLinePainter,
  dxRichEdit.DocumentModel.PatternLine,
  dxRichEdit.DocumentLayout.UnitConverter;

type
  { TdxGdiStringViewInfo }

  TdxGdiStringViewInfo = record
    GlyphCount: Integer;
    Glyphs: TSmallIntDynArray;
    CharacterWidths: TIntegerDynArray;
    procedure Release;
  end;

  IdxGraphicsCache = interface
  ['{DB80F601-3309-4E78-BC94-DA40B8DAFA24}']
  end;

  TdxGraphicsCache = class(TcxIUnknownObject, IdxGraphicsCache)
  end;

  TdxDrawStringActualImplementationDelegate =
    procedure (AHdc: THandle; const AText: string; AFontInfo: TdxGdiFontInfo; const ABounds: TRect; AStringFormat: TdxGPStringFormat) of object;

  TdxPainter = class abstract (TcxIUnknownObject, IdxPatternLinePaintingSupport)
  private
    FAllowChangeTextForeColor: Boolean;
    FGraphics: TdxGraphics;
    FTextForeColor: TdxAlphaColor;
    procedure SetTextForeColor(const Value: TdxAlphaColor);
  protected
    function GetHasTransform: Boolean; virtual;
    function GetRectangularClipBounds: TdxRectF; virtual; abstract;
    function GetDpiY: Integer; virtual; abstract;
    procedure SetClipBounds(const Value: TdxRectF); virtual; abstract;
  public
    constructor Create(AGraphics: TdxGraphics); virtual;

    function ApplyClipBounds(const AClipBounds: TdxRectF): TdxRectF; virtual;
    procedure RestoreClipBounds(const AClipBounds: TdxRectF); virtual;

    procedure DrawString(const AText: string; AFontInfo: TdxFontInfo; AForeColor: TdxAlphaColor; const ARectangle: TRect); overload;
    procedure DrawString(const AText: string; AFontInfo: TdxFontInfo; AForeColor: TdxAlphaColor; const ARectangle: TRect; AStringFormat: TdxGPStringFormat); overload;
    procedure DrawString(const AText: string; ABrush: TdxGPBrush; AFont: TFont; X, Y: Double); overload; virtual; abstract;
    procedure DrawString(const AText: string; AFontInfo: TdxFontInfo; const ARectangle: TRect); overload; virtual; abstract;
    procedure DrawString(const AText: string; AFontInfo: TdxFontInfo; const ARectangle: TRect; AStringFormat: TdxGPStringFormat); overload; virtual; abstract;

    property RectangularClipBounds: TdxRectF read GetRectangularClipBounds;
    property ClipBounds: TdxRectF read GetRectangularClipBounds write SetClipBounds;
    property TextForeColor: TdxAlphaColor read FTextForeColor write SetTextForeColor;
    property DpiY: Integer read GetDpiY;
    property AllowChangeTextForeColor: Boolean read FAllowChangeTextForeColor write FAllowChangeTextForeColor;

    procedure FillRectangle(ABrush: TdxGPBrush; const ABounds: TdxRectF); overload; virtual; abstract;
    procedure FillRectangle(AColor: TdxAlphaColor; const ABounds: TdxRectF); overload; virtual;
    procedure FillRectangle(ABrush: TdxGPBrush; const ABounds: TRect); overload; virtual; abstract;
    procedure FillRectangle(AColor: TdxAlphaColor; const ABounds: TRect); overload; virtual; abstract;

    procedure DrawRectangle(APen: TdxGPPen; const ABounds: TRect); virtual; abstract;
    procedure FillEllipse(ABrush: TdxGPBrush; const ABounds: TRect); overload; virtual; abstract;
    procedure FillEllipse(ABrush: TdxGPBrush; const ABounds: TdxRectF); overload; virtual;
    procedure FillEllipse(ABrush: TdxGPBrush; X, Y, AWidth, AHeight: Single); overload; virtual;
    function MeasureString(const AText: string; AFont: TdxGPFont): TdxSizeF; virtual; abstract;
    procedure DrawString(const AText: string; ABrush: TdxGPBrush; AFont: TdxGPFont; X: Single; Y: Single); overload; virtual; abstract;
    procedure DrawSpacesString(const AText: string; AFontInfo: TdxFontInfo; AForeColor: TdxAlphaColor; const ARectangle: TRect); overload; virtual;
    procedure DrawSpacesString(const AText: string; AFontInfo: TdxFontInfo; const ARectangle: TRect); overload; virtual; abstract;

    procedure DrawImage(AImg: TdxOfficeImage; const ABounds: TRect; ASizing: TdxImageSizeMode = TdxImageSizeMode.Normal); overload; virtual; abstract;
    procedure DrawImage(AImg: TdxOfficeImage; const ABounds: TRect; const AImgActualSizeInLayoutUnits: TSize; ASizing: TdxImageSizeMode); overload; virtual;

    procedure DrawLine(APen: TdxGPPen; X1, Y1, X2, Y2: Single); virtual; abstract;
    procedure DrawLines(APen: TdxGPPen; const APoints: TArray<TdxPointF>); virtual; abstract;
    procedure FillPolygon(ABrush: TdxGPBrush; const APoints: TArray<TdxPointF>); virtual; abstract;
    procedure ExcludeCellBounds(const ARect, ARowBounds: TRect); virtual; abstract;
    procedure ResetCellBoundsClip; virtual; abstract;
    procedure FinishPaint; virtual;
    procedure SnapWidths(var AWidths: TArray<Single>); virtual;
    procedure SnapHeights(var AHeights: TArray<Single>); virtual;
    function GetSnappedPoint(const APoint: TdxPointF): TdxPointF; virtual;

    procedure TransformToPixels(var APoints: TArray<TdxPointF>); virtual; abstract;
    procedure TransformToLayoutUnits(var APoints: TArray<TdxPointF>); virtual; abstract;

    function GetPen(AColor: TdxAlphaColor): TdxGPColorPen; overload; virtual; abstract;
    function GetPen(AColor: TdxAlphaColor; AThickness: Single): TdxGPColorPen; overload; virtual; abstract;
    procedure ReleasePen(APen: TdxGPPen); virtual; abstract;
    function GetBrush(AColor: TdxAlphaColor): TdxGPBrush; virtual; abstract;
    procedure ReleaseBrush(ABrush: TdxGPBrush); virtual; abstract;
    function TryPushRotationTransform(const ACenter: TPoint; AAngleInDegrees: Single): Boolean;
    procedure PushRotationTransform(const ACenter: TPoint; AAngleInDegrees: Single); virtual; abstract;
    procedure PopTransform; virtual; abstract;
    procedure PushSmoothingMode(AHighQuality: Boolean); virtual; abstract;
    procedure PopSmoothingMode; virtual; abstract;
    procedure PushPixelOffsetMode(AHighQualtity: Boolean); virtual; abstract;
    procedure PopPixelOffsetMode; virtual; abstract;

    property Graphics: TdxGraphics read FGraphics;
    property HasTransform: Boolean read GetHasTransform;
  end;

  { TdxEmptyPainter }

  TdxEmptyPainter = class(TdxPainter)
  protected
    function GetRectangularClipBounds: TdxRectF; override;
    function GetDpiY: Integer; override;
    procedure SetClipBounds(const ABounds: TdxRectF); override;
  public
    procedure FillRectangle(ABrush: TdxGPBrush; const ABounds: TRect); override;
    procedure FillRectangle(AColor: TdxAlphaColor; const ABounds: TRect); override;
    procedure FillEllipse(ABrush: TdxGPBrush; const ABounds: TRect); override;
    procedure DrawRectangle(APen: TdxGPPen; const ABounds: TRect); override;
    procedure DrawString(const AText: string; AFontInfo: TdxFontInfo; const ARectangle: TRect); override;
    procedure DrawString(const AText: string; AFontInfo: TdxFontInfo; const ARectangle: TRect; AStringFormat: TdxGPStringFormat); override;
    procedure DrawString(const AText: string; ABrush: TdxGPBrush; AFont: TdxGPFont; X: Single; Y: Single); override;
    function MeasureString(const AText: string; AFont: TdxGPFont): TdxSizeF; override;
    procedure DrawSpacesString(const AText: string; AFontInfo: TdxFontInfo; const ARectangle: TRect); override;
    procedure DrawImage(AImg: TdxOfficeImage; const ABounds: TRect; ASizing: TdxImageSizeMode = TdxImageSizeMode.Normal); override;
    procedure DrawLine(APen: TdxGPPen; AX1: Single; AY1: Single; AX2: Single; AY2: Single); override;
    procedure DrawLines(APen: TdxGPPen; const APoints: TArray<TdxPointF>); override;
    procedure FillPolygon(ABrush: TdxGPBrush; const APoints: TArray<TdxPointF>); override;
    procedure ExcludeCellBounds(const ARect: TRect; const ARowBounds: TRect); override;
    procedure ResetCellBoundsClip; override;
    function GetPen(AColor: TdxAlphaColor): TdxGPColorPen; override;
    function GetPen(AColor: TdxAlphaColor; AThickness: Single): TdxGPColorPen; override;
    procedure ReleasePen(APen: TdxGPPen); override;
    function GetBrush(AColor: TdxAlphaColor): TdxGPBrush; override;
    procedure ReleaseBrush(ABrush: TdxGPBrush); override;
    procedure SnapWidths(var AWidths: TArray<Single>); override;
    procedure SnapHeights(var AHeights: TArray<Single>); override;
    function GetSnappedPoint(const APoint: TdxPointF): TdxPointF; override;
    procedure TransformToPixels(var APoints: TArray<TdxPointF>); override;
    procedure TransformToLayoutUnits(var APoints: TArray<TdxPointF>); override;
    procedure PushRotationTransform(const ACenter: TPoint; AAngleInRadians: Single); override;
    procedure PopTransform; override;
    procedure PushSmoothingMode(AHighQuality: Boolean); override;
    procedure PopSmoothingMode; override;
    procedure PushPixelOffsetMode(AHighQualtity: Boolean); override;
    procedure PopPixelOffsetMode; override;
  end;

  TdxGdiPlusPainterBase = class abstract (TdxPainter)
  private
    FStringFormat: TdxGPStringFormat;
  protected
    class function CreateStringFormat: TdxGPStringFormat;
    function CorrectTextDrawingBounds(AFontInfo: TdxFontInfo; const ATextBounds: TRect): TRect;
  public
    constructor Create(AGraphics: TdxGraphics); override;
    destructor Destroy; override;
    procedure DrawSpacesString(const AText: string; AFontInfo: TdxFontInfo; const ABounds: TRect); override;

    property StringFormat: TdxGPStringFormat read FStringFormat;
  end;

  { TdxGdiPlusPainter }

  TdxGdiPlusPainter = class(TdxGdiPlusPainterBase)
  strict private
    FCache: IdxGraphicsCache;
    FTransformsStackCount: Integer;
    FSmoothingmodeStack: TStack<TdxGPSmoothingMode>;
    FPixelmodeStack: TStack<TdxGPPixelOffsetMode>;
    FClipRegions: TdxObjectStack<TdxGPRegion>;
    FRectangularBounds: TStack<TdxRectF>;
    FRectangularClipBounds: TdxRectF;
    FDpiY: Integer;
  protected
    function GetDpiY: Integer; override;
    function GetHasTransform: Boolean; override;
    function GetRectangularClipBounds: TdxRectF; override;
    procedure SetClipBounds(const ABounds: TdxRectF); override;
  public
    constructor Create(AGraphics: TdxGraphics); override;
    destructor Destroy; override;

    procedure SnapHeights(var AHeights: TArray<Single>); override;
    procedure SnapWidths(var AWidths: TArray<Single>); override;
    procedure FillRectangle(ABrush: TdxGPBrush; const AActualBounds: TRect); override;
    procedure FillRectangle(AColor: TdxAlphaColor; const AActualBounds: TRect); override;
    procedure FillEllipse(ABrush: TdxGPBrush; const ABounds: TRect); override;
    procedure DrawRectangle(APen: TdxGPPen; const ABounds: TRect); override;
    procedure DrawString(const AText: string; AFontInfo: TdxFontInfo; const ABounds: TRect); override;
    procedure DrawString(const AText: string; AFontInfo: TdxFontInfo; const ABounds: TRect; AStringFormat: TdxGPStringFormat); override;
    procedure DrawString(const AText: string; ABrush: TdxGPBrush; AFont: TdxGPFont; X: Single; Y: Single); override;
    function MeasureString(const AText: string; AFont: TdxGPFont): TdxSizeF; override;
    procedure DrawImageCore(AImg: TdxOfficeImage; const ABounds: TdxRectF; ASizing: TdxImageSizeMode); virtual;
    procedure DrawImage(AImg: TdxOfficeImage; const ABounds: TRect; ASizing: TdxImageSizeMode = TdxImageSizeMode.Normal); override;
    procedure DrawImage(AImg: TdxOfficeImage; const ABounds: TRect; const AImgActualSizeInLayoutUnits: TSize; ASizing: TdxImageSizeMode); override;
    procedure DrawLine(APen: TdxGPPen; AX1: Single; AY1: Single; AX2: Single; AY2: Single); override;
    procedure DrawLines(APen: TdxGPPen; const APoints: TArray<TdxPointF>); override;
    procedure FillPolygon(ABrush: TdxGPBrush; const APoints: TArray<TdxPointF>); override;
    procedure ExcludeCellBounds(const ARect: TRect; const ARowBounds: TRect); override;
    procedure ResetCellBoundsClip; override;
    procedure FillRectangle(AColor: TdxAlphaColor; const ABounds: TdxRectF); override;
    procedure FillRectangle(ABrush: TdxGPBrush; const ABounds: TdxRectF); override;
    function GetPen(AColor: TdxAlphaColor): TdxGPColorPen; override;
    function GetPen(AColor: TdxAlphaColor; AThickness: Single): TdxGPColorPen; override;
    procedure ReleasePen(APen: TdxGPPen); override;
    function GetBrush(AColor: TdxAlphaColor): TdxGPBrush; override;
    procedure ReleaseBrush(ABrush: TdxGPBrush); override;
    procedure PushRotationTransform(const ACenter: TPoint; AAngleInDegrees: Single); override;
    procedure PopTransform; override;
    procedure PushSmoothingMode(AHighQuality: Boolean); override;
    procedure PopSmoothingMode; override;
    procedure PushPixelOffsetMode(AHighQualtity: Boolean); override;
    procedure PopPixelOffsetMode; override;
    procedure TransformToPixels(var APoints: TArray<TdxPointF>); override;
    procedure TransformToLayoutUnits(var APoints: TArray<TdxPointF>); override;

    property Cache: IdxGraphicsCache read FCache;
  end;

  { TdxGdiPainter }

  TdxGdiPainter = class abstract(TdxGdiPlusPainter)
  strict private
    FUnitConverter: TdxDocumentLayoutUnitConverter;
    FInitialBufferItemCount: Integer;
    FGlyphsBufferSize: Integer;
    FGlyphsBuffer: Pointer;
    FCharacterWidthsBufferSize: Integer;
    FCharacterWidthsBuffer: Pointer;
  protected
    procedure SetClipBounds(const ABounds: TdxRectF); override;
    procedure APISetClip(const ABounds: TRect);
    class function SetClipToHDC(const AClipBounds: TRect; AHdc: THandle): TRect; static;
    class function ExcludeClipFromHDC(const ABounds: TRect; AHdc: THandle): TRect; static;
    procedure APIExcludeClip(const ABounds: TRect);
    function GetCharacterWidthsBuffer(AItemsCount: Integer): Pointer;
    function GetGlyphsBuffer(AItemsCount: Integer): Pointer;
    procedure DefaultDrawStringImplementation(AHdc: THandle; const AText: string; AFontInfo: TdxGdiFontInfo;
      const ABounds: TRect; AStringFormat: TdxGPStringFormat); virtual;
    procedure SpacesPlaceholdersDrawStringImplementation(AHdc: THandle; const AText: string; AFontInfo: TdxGdiFontInfo;
      const ABounds: TRect; AStringFormat: TdxGPStringFormat);
    procedure DrawStringImpl(const AText: string; AFontInfo: TdxGdiFontInfo; AForeColor: TdxAlphaColor;
      const ABounds: TRect; AStringFormat: TdxGPStringFormat; AImpl: TdxDrawStringActualImplementationDelegate);
    function GetMeasureHdc(AHdc: THandle): THandle; virtual;
    procedure ReleaseMeasureHdc(AMeasureHdc: THandle); virtual;
    procedure DrawNonCachedString(AHdc: THandle; AFontInfo: TdxFontInfo; const AText: string; const ABounds: TRect;
      AStringFormat: TdxGPStringFormat); virtual;
    procedure DrawNonCachedStringCore(AHdc: THandle; AMeasureHdc: THandle; const AText: string; const ABounds: TRect;
      AStringFormat: TdxGPStringFormat); virtual;
    procedure DrawNonCachedStringCoreAlignTopLeft(AHdc: THandle; AMeasureHdc: THandle; const AText: string;
      const ABounds: TRect); virtual;
    procedure DrawNonCachedStringCoreAligned(AHdc: THandle; AMeasureHdc: THandle; const AText: string;
      const ABounds: TRect; AStringFormat: TdxGPStringFormat); virtual;
    function GetAlignedTextPosition(const ABounds: TRect; const ATextSize: TSize; AStringFormat: TdxGPStringFormat): TPoint;
    function GetAlignedTextLeft(const ABounds: TRect; ATextWidth: Integer; AStringFormat: TdxGPStringFormat): Integer;
    function GetAlignedTextTop(const ABounds: TRect; ATextHeight: Integer; AStringFormat: TdxGPStringFormat): Integer;
    function GenerateStringViewInfo(AHdc: THandle; const AText: string): TdxGdiStringViewInfo;
  public
    constructor Create(AGraphics: TdxGraphics{const ACache: IdxGraphicsCache}; AUnitConverter: TdxDocumentLayoutUnitConverter); reintroduce;
    destructor Destroy; override;

    procedure ExcludeCellBounds(const ARect: TRect; const ARowBounds: TRect); override;
    procedure DrawSpacesString(const AText: string; AFontInfo: TdxFontInfo; const ABounds: TRect); override;
    procedure DrawString(const AText: string; AFontInfo: TdxFontInfo; const ABounds: TRect); override;
    procedure DrawString(const AText: string; AFontInfo: TdxFontInfo; const ABounds: TRect; AStringFormat: TdxGPStringFormat); override;
  end;

  { TdxRichEditGdiPainter }

  TdxRichEditGdiPainter = class(TdxGdiPainter)
  private
    FMeasurer: TdxGdiBoxMeasurer;
  public
    constructor Create(AGraphics: TdxGraphics; AMeasurer: TdxGdiBoxMeasurer); reintroduce;

    function GetMeasureHdc(AHdc: THandle): THandle; override;
    procedure ReleaseMeasureHdc(AHdc: THandle); override;
    procedure DefaultDrawStringImplementation(AHdc: THandle; const AText: string; AFontInfo: TdxGdiFontInfo;
      const ABounds: TRect; AStringFormat: TdxGPStringFormat); override;
    procedure DrawImageCore(AImg: TdxOfficeImage; const ABounds: TdxRectF; ASizing: TdxImageSizeMode); override;
    procedure DrawStringCore(AHdc: THandle; const AText: string; const ABounds: TRect; ATextInfo: TdxGdiTextViewInfo;
      AFontInfo: TdxGdiFontInfo); virtual;

    property Measurer: TdxGdiBoxMeasurer read FMeasurer;
  end;

  { TdxTableCornerPainter }

  TdxTableCornerPainter = class
  public
    procedure DrawCorner(const APainter: IdxPainterWrapper; X: Integer; Y: Integer; ACorner: TdxCornerViewInfoBase); virtual;
    function GetDrawingColor(AColor: TdxAlphaColor; ACornerType: TdxCornerViewInfoType): TdxAlphaColor;
    function GetSnappedWidths(const APainter: IdxPainterWrapper; ACorner: TdxCornerViewInfoBase): TArray<Single>;
    function GetSnappedHeights(const APainter: IdxPainterWrapper; ACorner: TdxCornerViewInfoBase): TArray<Single>;
    function GetCornerSnappedPoint(const APainter: IdxPainterWrapper; X: Single; Y: Single; ACorner: TdxCornerViewInfoBase): TdxPointF;
    function GetCornerWidth(const APainter: IdxPainterWrapper; ACorner: TdxCornerViewInfoBase): Single;
    function GetCornerHeight(const APainter: IdxPainterWrapper; ACorner: TdxCornerViewInfoBase): Single;
    function GetTotalSize(const AWidths: TArray<Single>): Single;
  end;

  { TdxTableBorderPainter }

  TdxTableBorderPainter = class abstract
  private
    FWidthF: Single;
    FHalfBorderWidth: Single;
    FPainter: IdxPainterWrapper;
    FCornerPainter: TdxTableCornerPainter;
  protected
    function GetStartSnappedPoint(const X, Y: Single): TdxPointF;
    procedure DrawHorizontalLines(AColor: TdxAlphaColor; const AOffsets: TArray<Single>; const ALeft: TdxPointF;
      const AWidth: Single); overload; virtual;
    procedure DrawHorizontalLines(AColor: TdxAlphaColor; const AOffsets: TArray<Single>; const ALeft: TdxPointF;
      const AWidth: Single; AUnderline: TdxUnderline); overload; virtual;
    procedure DrawVerticalLines(AColor: TdxAlphaColor; const AOffsets: TArray<Single>; const ATop: TdxPointF;
      const AHeight: Single); overload; virtual;
    procedure DrawVerticalLines(AColor: TdxAlphaColor; const AOffsets: TArray<Single>; const ATop: TdxPointF;
      const AHeight: Single; AUnderline: TdxUnderline); overload; virtual;

    property HalfBorderWidth: Single read FHalfBorderWidth;
    property Painter: IdxPainterWrapper read FPainter;
  public
    constructor Create(const APainter: IdxPainterWrapper; AWidth: Single);
    destructor Destroy; override;
    procedure DrawBorder(const ABorderViewInfo: TdxTableBorderViewInfoBase; const ACellBounds: TRect); virtual;
    function GetDrawingColor(AColor: TdxAlphaColor): TdxAlphaColor;
    procedure DrawHorizontalBorder(AColor: TdxAlphaColor; const ALeft: TdxPointF; AWidth: Single); virtual; abstract;
    procedure DrawVerticalBorder(AColor: TdxAlphaColor; const ATop: TdxPointF; AHeight: Single); virtual; abstract;

    property Width: Single read FWidthF;
  end;

  { TdxSingleBorderPainter }

  TdxSingleBorderPainter = class(TdxTableBorderPainter)
  private
    FOffsets: TArray<Single>;
    FUnderline: TdxUnderline;
  public
    constructor Create(const APainter: IdxPainterWrapper; AWidth: Single; AUnderline: TdxUnderline);
    procedure DrawVerticalBorder(AColor: TdxAlphaColor; const ATop: TdxPointF; AHeight: Single); override;
    procedure DrawHorizontalBorder(AColor: TdxAlphaColor; const ALeft: TdxPointF; AWidth: Single); override;
  end;

  { TdxDoubleBorderPainter }

  TdxDoubleBorderPainter = class(TdxSingleBorderPainter)
  private
    FOffsets: TArray<Single>;
  public
    constructor Create(const APainter: IdxPainterWrapper; const ACompoundArray: TArray<Single>; AWidth: Single);
    procedure DrawHorizontalBorder(AColor: TdxAlphaColor; const ALeft: TdxPointF; AWidth: Single); override;
    procedure DrawVerticalBorder(AColor: TdxAlphaColor; const ATop: TdxPointF; AHeight: Single); override;
  end;

  { TdxTripleBorderPainter }

  TdxTripleBorderPainter = class(TdxDoubleBorderPainter);

implementation

uses
  Math, Contnrs, dxTypeHelpers,
  dxRichEdit.Utils.Exceptions,
  dxRichEdit.DocumentModel.Core;

procedure TransformPoints(AGraphics: TdxGraphics; ADestSpace, ASourceSpace: TdxGpCoordinateSpace; var APoints: TArray<TdxPointF>);
begin
  if Length(APoints) = 0 then
    Exit;
  GdipCheck(GdipTransformPoints(AGraphics.Handle, ADestSpace, ASourceSpace, @APoints[0], Length(APoints)));
end;

type
  { TdxSnapToDevicePixelsHelper }

  TdxSnapToDevicePixelsHelper = class
  public
    class function GetCorrectedBounds(AGraphics: TdxGraphics; const ASizeInPixel: TSize; const ABounds: TdxRectF): TdxRectF;
  end;

class function TdxSnapToDevicePixelsHelper.GetCorrectedBounds(AGraphics: TdxGraphics; const ASizeInPixel: TSize;
  const ABounds: TdxRectF): TdxRectF;
var
  APoints: TArray<TdxPointF>;
  AScreenWidth, AScreenHeight: Integer;
  AWidth, AHeight: Single;
begin
  SetLength(APoints, 2);
  APoints[0] := dxPointF(ABounds.Left, ABounds.Top);
  APoints[1] := dxPointF(ABounds.Right, ABounds.Bottom);
  TransformPoints(AGraphics, TdxGpCoordinateSpace.CoordinateSpaceDevice, TdxGpCoordinateSpace.CoordinateSpaceWorld, APoints);
  AScreenWidth := Trunc(APoints[1].X - APoints[0].X);
  AScreenHeight := Trunc(APoints[1].Y - APoints[0].Y);
  if (Abs(ASizeInPixel.cx - AScreenWidth) <= 5) and (Abs(ASizeInPixel.cy - AScreenHeight) <= 5) then
  begin
    APoints[0].X := Trunc(APoints[0].X + 0.5);
    APoints[0].Y := Trunc(APoints[0].Y + 0.5);
    APoints[1].X := APoints[0].X + ASizeInPixel.cx;
    APoints[1].Y := APoints[0].Y + ASizeInPixel.cy;
    TransformPoints(AGraphics, TdxGpCoordinateSpace.CoordinateSpaceWorld, TdxGpCoordinateSpace.CoordinateSpaceDevice, APoints);
    AWidth := APoints[1].X - APoints[0].X;
    AHeight := APoints[1].Y - APoints[0].Y;
    Result := cxRectFBounds(APoints[0].X, APoints[0].Y, AWidth, AHeight);
  end
  else
    Result := ABounds;
end;

{ TdxGdiStringViewInfo }

procedure TdxGdiStringViewInfo.Release;
begin
  CharacterWidths := nil;
  Glyphs := nil;
end;

{ TdxPainter }

function TdxPainter.ApplyClipBounds(const AClipBounds: TdxRectF): TdxRectF;
var
  AOldClipBounds: TdxRectF;
begin
  AOldClipBounds := ClipBounds;
  SetClipBounds(AClipBounds);
  Result := AOldClipBounds;
end;


procedure TdxPainter.RestoreClipBounds(const AClipBounds: TdxRectF);
begin
  SetClipBounds(AClipBounds);
end;

procedure TdxPainter.DrawString(const AText: string; AFontInfo: TdxFontInfo;
  AForeColor: TdxAlphaColor; const ARectangle: TRect);
begin
  TextForeColor := AForeColor;
  DrawString(AText, AFontInfo, ARectangle);
end;

procedure TdxPainter.DrawString(const AText: string; AFontInfo: TdxFontInfo;
  AForeColor: TdxAlphaColor; const ARectangle: TRect; AStringFormat: TdxGPStringFormat);
begin
  TextForeColor := AForeColor;
  DrawString(AText, AFontInfo, ARectangle, AStringFormat);
end;

procedure TdxPainter.SetTextForeColor(const Value: TdxAlphaColor);
begin
  if FAllowChangeTextForeColor then
    FTextForeColor := Value;
end;

procedure TdxPainter.SnapHeights(var AHeights: TArray<Single>);
var
  I: Integer;
  APoints: TArray<TdxPointF>;
begin
  SetLength(APoints, Length(AHeights) + 1);
  APoints[0] := dxPointF(0, 0);
  for I := 0 to Length(AHeights) - 1 do
    APoints[I + 1] := dxPointF(0, AHeights[I]);
  TransformToPixels(APoints);
  for I := Length(AHeights) downto 1 do
    if AHeights[I - 1] <> 0 then
      APoints[I].Y := Math.Max(Trunc(APoints[I].Y - APoints[0].Y + 0.5), 1)
     else
       APoints[I].Y := 0;
  APoints[0].Y := 0;
  TransformToLayoutUnits(APoints);
  for I := Length(AHeights) downto 1 do
    AHeights[I - 1] := APoints[I].Y - APoints[0].Y;
end;

procedure TdxPainter.SnapWidths(var AWidths: TArray<Single>);
var
  I: Integer;
  APoints: TArray<TdxPointF>;
begin
  SetLength(APoints, Length(AWidths) + 1);
  APoints[0] := dxPointF(0, 0);
  for I := 0 to Length(AWidths) - 1 do
    APoints[I + 1] := dxPointF(AWidths[I], 0);
  TransformToPixels(APoints);
  for I := Length(AWidths) downto 1 do
    if AWidths[I - 1] <> 0 then
      APoints[I].X := Math.Max(Trunc(APoints[I].X - APoints[0].X + 0.5), 1)
    else
      APoints[I].X := 0;
  APoints[0].X := 0;
  TransformToLayoutUnits(APoints);
  for I := Length(AWidths) downto 1 do
    AWidths[I - 1] := APoints[I].X - APoints[0].X;
end;

function TdxPainter.TryPushRotationTransform(const ACenter: TPoint; AAngleInDegrees: Single): Boolean;
begin
  if IsZero(Single(dxFMod(AAngleInDegrees, 360))) then
    Exit(False);
  PushRotationTransform(ACenter, AAngleInDegrees);
  Result := True;
end;

constructor TdxPainter.Create(AGraphics: TdxGraphics);
begin
  inherited Create;
  FAllowChangeTextForeColor := True;
  FGraphics := AGraphics;
end;

procedure TdxPainter.FillEllipse(ABrush: TdxGPBrush; const ABounds: TdxRectF);
begin
  FillEllipse(ABrush, TRect.Round(ABounds));
end;

procedure TdxPainter.FillEllipse(ABrush: TdxGPBrush; X, Y, AWidth, AHeight: Single);
begin
  FillEllipse(ABrush, TRect.CreateSize(Trunc(X), Trunc(Y), Trunc(AWidth), Trunc(AHeight)));
end;

procedure TdxPainter.FillRectangle(AColor: TdxAlphaColor; const ABounds: TdxRectF);
begin
  FillRectangle(AColor, TRect.Round(ABounds));
end;

procedure TdxPainter.FinishPaint;
begin
end;

function TdxPainter.GetHasTransform: Boolean;
begin
  Result := False;
end;

function TdxPainter.GetSnappedPoint(const APoint: TdxPointF): TdxPointF;
var
  APoints: TArray<TdxPointF>;
begin
  SetLength(APoints, 1);
  APoints[0] := APoint;
  TransformToPixels(APoints);
  APoints[0].X := Trunc(APoints[0].X);
  APoints[0].Y := Trunc(APoints[0].Y);
  TransformToLayoutUnits(APoints);
  Result := APoints[0];
end;

procedure TdxPainter.DrawSpacesString(const AText: string; AFontInfo: TdxFontInfo; AForeColor: TdxAlphaColor;
  const ARectangle: TRect);
begin
  TextForeColor := AForeColor;
  DrawSpacesString(AText, AFontInfo, ARectangle);
end;

procedure TdxPainter.DrawImage(AImg: TdxOfficeImage; const ABounds: TRect; const AImgActualSizeInLayoutUnits: TSize;
  ASizing: TdxImageSizeMode);
begin
end;

{ TdxEmptyPainter }

function TdxEmptyPainter.GetRectangularClipBounds: TdxRectF;
begin
  Result.Empty;
end;

function TdxEmptyPainter.GetDpiY: Integer;
begin
  Result := 96;
end;

procedure TdxEmptyPainter.SetClipBounds(const ABounds: TdxRectF);
begin
end;

procedure TdxEmptyPainter.FillRectangle(ABrush: TdxGPBrush; const ABounds: TRect);
begin
end;

procedure TdxEmptyPainter.FillRectangle(AColor: TdxAlphaColor; const ABounds: TRect);
begin
end;

procedure TdxEmptyPainter.FillEllipse(ABrush: TdxGPBrush; const ABounds: TRect);
begin
end;

procedure TdxEmptyPainter.DrawRectangle(APen: TdxGPPen; const ABounds: TRect);
begin
end;

procedure TdxEmptyPainter.DrawString(const AText: string; AFontInfo: TdxFontInfo; const ARectangle: TRect);
begin
end;

procedure TdxEmptyPainter.DrawString(const AText: string; AFontInfo: TdxFontInfo; const ARectangle: TRect; AStringFormat: TdxGPStringFormat);
begin
end;

procedure TdxEmptyPainter.DrawString(const AText: string; ABrush: TdxGPBrush; AFont: TdxGPFont; X: Single; Y: Single);
begin
end;

function TdxEmptyPainter.MeasureString(const AText: string; AFont: TdxGPFont): TdxSizeF;
begin
  Result := TdxSizeF.Null;
end;

procedure TdxEmptyPainter.DrawSpacesString(const AText: string; AFontInfo: TdxFontInfo; const ARectangle: TRect);
begin
end;

procedure TdxEmptyPainter.DrawImage(AImg: TdxOfficeImage; const ABounds: TRect; ASizing: TdxImageSizeMode = TdxImageSizeMode.Normal);
begin
end;

procedure TdxEmptyPainter.DrawLine(APen: TdxGPPen; AX1: Single; AY1: Single; AX2: Single; AY2: Single);
begin
end;

procedure TdxEmptyPainter.DrawLines(APen: TdxGPPen; const APoints: TArray<TdxPointF>);
begin
end;

procedure TdxEmptyPainter.FillPolygon(ABrush: TdxGPBrush; const APoints: TArray<TdxPointF>);
begin
end;

procedure TdxEmptyPainter.ExcludeCellBounds(const ARect: TRect; const ARowBounds: TRect);
begin
end;

procedure TdxEmptyPainter.ResetCellBoundsClip;
begin
end;

function TdxEmptyPainter.GetPen(AColor: TdxAlphaColor): TdxGPColorPen;
begin
  Result := TdxGPColorPen.Create(AColor);
end;

function TdxEmptyPainter.GetPen(AColor: TdxAlphaColor; AThickness: Single): TdxGPColorPen;
begin
  Result := TdxGPColorPen.Create(AColor, AThickness);
end;

procedure TdxEmptyPainter.ReleasePen(APen: TdxGPPen);
begin
end;

function TdxEmptyPainter.GetBrush(AColor: TdxAlphaColor): TdxGPBrush;
begin
  Result := TdxGPBrush.Create;
  Result.Color := TdxAlphaColors.Black;
end;

procedure TdxEmptyPainter.ReleaseBrush(ABrush: TdxGPBrush);
begin
end;

procedure TdxEmptyPainter.SnapWidths(var AWidths: TArray<Single>);
begin
end;

procedure TdxEmptyPainter.SnapHeights(var AHeights: TArray<Single>);
begin
end;

function TdxEmptyPainter.GetSnappedPoint(const APoint: TdxPointF): TdxPointF;
begin
  Result := APoint;
end;

procedure TdxEmptyPainter.TransformToPixels(var APoints: TArray<TdxPointF>);
begin
end;

procedure TdxEmptyPainter.TransformToLayoutUnits(var APoints: TArray<TdxPointF>);
begin
end;

procedure TdxEmptyPainter.PushRotationTransform(const ACenter: TPoint; AAngleInRadians: Single);
begin
end;

procedure TdxEmptyPainter.PopTransform;
begin
end;

procedure TdxEmptyPainter.PushSmoothingMode(AHighQuality: Boolean);
begin
end;

procedure TdxEmptyPainter.PopSmoothingMode;
begin
end;

procedure TdxEmptyPainter.PushPixelOffsetMode(AHighQualtity: Boolean);
begin
end;

procedure TdxEmptyPainter.PopPixelOffsetMode;
begin
end;

{ TdxGdiPlusPainterBase }

constructor TdxGdiPlusPainterBase.Create(AGraphics: TdxGraphics);
begin
  inherited Create(AGraphics);
  FStringFormat := CreateStringFormat;
end;

destructor TdxGdiPlusPainterBase.Destroy;
begin
  FStringFormat.Free;
  inherited Destroy;
end;

class function TdxGdiPlusPainterBase.CreateStringFormat: TdxGPStringFormat;
begin
  Result := TdxGPStringFormat.GenericTypographic.Clone;
  Result.FormatFlags :=
    (Result.FormatFlags or Ord(StringFormatFlagsNoClip) or Ord(StringFormatFlagsNoWrap) or
     Ord(StringFormatFlagsMeasureTrailingSpaces)) and not Ord(StringFormatFlagsLineLimit);
end;

function TdxGdiPlusPainterBase.CorrectTextDrawingBounds(AFontInfo: TdxFontInfo; const ATextBounds: TRect): TRect;
begin
  Result := ATextBounds;
  Result.Offset(0, AFontInfo._Free - AFontInfo.DrawingOffset);
end;

procedure TdxGdiPlusPainterBase.DrawSpacesString(const AText: string; AFontInfo: TdxFontInfo; const ABounds: TRect);
begin
  DrawString(AText, AFontInfo, ABounds);
end;

{ TdxGdiPlusPainter }

constructor TdxGdiPlusPainter.Create(AGraphics: TdxGraphics);
begin
  inherited Create(AGraphics);
  FSmoothingmodeStack := TStack<TdxGPSmoothingMode>.Create;
  FPixelmodeStack := TStack<TdxGPPixelOffsetMode>.Create;
  FClipRegions := TdxObjectStack<TdxGPRegion>.Create(True);
  FRectangularBounds := TStack<TdxRectF>.Create;
  FDpiY := Trunc(Graphics.DpiY);
  FRectangularClipBounds := Graphics.ClipBounds;
end;

destructor TdxGdiPlusPainter.Destroy;
begin
  FreeAndNil(FSmoothingmodeStack);
  FreeAndNil(FPixelmodeStack);
  FreeAndNil(FClipRegions);
  FreeAndNil(FRectangularBounds);
  inherited Destroy;
end;

function TdxGdiPlusPainter.GetDpiY: Integer;
begin
  Result := FDpiY;
end;

function TdxGdiPlusPainter.GetRectangularClipBounds: TdxRectF;
begin
  Result := FRectangularClipBounds;
end;

function TdxGdiPlusPainter.GetHasTransform: Boolean;
begin
  Result := FTransformsStackCount > 0;
end;

procedure TdxGdiPlusPainter.SnapHeights(var AHeights: TArray<Single>);
begin
  if FTransformsStackCount = 0 then
    inherited SnapHeights(AHeights);
end;

procedure TdxGdiPlusPainter.SnapWidths(var AWidths: TArray<Single>);
begin
  if FTransformsStackCount = 0 then
    inherited SnapWidths(AWidths);
end;

procedure TdxGdiPlusPainter.SetClipBounds(const ABounds: TdxRectF);
var
  ARegion: TdxGPRegion;
begin
  if FClipRegions.Count > 0 then
  begin
    ARegion := FClipRegions.Peek;
    ARegion.CombineRegionRect(ABounds, gmIntersect);
    Graphics.SetClip(ARegion);
    FRectangularClipBounds := ABounds;
  end
  else
  begin
    Graphics.SetClip(ABounds);
    FRectangularClipBounds := Graphics.ClipBounds;
  end;
end;

procedure TdxGdiPlusPainter.FillRectangle(ABrush: TdxGPBrush; const AActualBounds: TRect);
begin
  Graphics.FillRectangle(AActualBounds, ABrush);
end;

procedure TdxGdiPlusPainter.FillRectangle(AColor: TdxAlphaColor; const AActualBounds: TRect);
begin
  Graphics.FillRectangle(AActualBounds, AColor);
end;

procedure TdxGdiPlusPainter.FillEllipse(ABrush: TdxGPBrush; const ABounds: TRect);
begin
  Graphics.Ellipse(ABounds, nil, ABrush);
end;

procedure TdxGdiPlusPainter.DrawRectangle(APen: TdxGPPen; const ABounds: TRect);
begin
  Graphics.Rectangle(ABounds, APen, nil);
end;

procedure TdxGdiPlusPainter.DrawString(const AText: string; AFontInfo: TdxFontInfo; const ABounds: TRect);
begin
  DrawString(AText, AFontInfo, ABounds, StringFormat);
end;

procedure TdxGdiPlusPainter.DrawString(const AText: string; AFontInfo: TdxFontInfo; const ABounds: TRect; AStringFormat: TdxGPStringFormat);
var
  AGdiFontInfo: TdxGdiFontInfo absolute AFontInfo;
  AForeBrush: TdxGPBrush;
  AFont: TdxGPFont;
begin
  AForeBrush := TdxGPBrush.Create;
  try
    AForeBrush.Color := TextForeColor;
    AFont := TdxGPFont.FromHfont(AGdiFontInfo.GdiFontHandle);
    try
      Graphics.DrawString(AText, AFont, AForeBrush, CorrectTextDrawingBounds(AFontInfo, ABounds).ToRectF, AStringFormat);
    finally
      AFont.Free;
    end;
  finally
    AForeBrush.Free;
  end;
end;

procedure TdxGdiPlusPainter.DrawString(const AText: string; ABrush: TdxGPBrush; AFont: TdxGPFont; X: Single; Y: Single);
begin
  Graphics.DrawString(AText, AFont, ABrush, TdxRectF.CreateSize(X, Y, MaxInt div 4, MaxInt div 4), StringFormat);
end;

function TdxGdiPlusPainter.MeasureString(const AText: string; AFont: TdxGPFont): TdxSizeF;
begin
  Result := Graphics.MeasureString(AText, AFont, MaxInt div 4, StringFormat);
end;

procedure TdxGdiPlusPainter.DrawImageCore(AImg: TdxOfficeImage; const ABounds: TdxRectF; ASizing: TdxImageSizeMode);
begin
  Graphics.Draw(AImg, ABounds);
end;

procedure TdxGdiPlusPainter.DrawImage(AImg: TdxOfficeImage; const ABounds: TRect; ASizing: TdxImageSizeMode = TdxImageSizeMode.Normal);
var
  ACorrectedBounds: TdxRectF;
begin
  ACorrectedBounds := TdxSnapToDevicePixelsHelper.GetCorrectedBounds(Graphics, AImg.Size, ABounds.ToRectF);
  DrawImageCore(AImg, ACorrectedBounds, ASizing);
end;

procedure TdxGdiPlusPainter.DrawImage(AImg: TdxOfficeImage; const ABounds: TRect;
  const AImgActualSizeInLayoutUnits: TSize; ASizing: TdxImageSizeMode);
var
  AOldClip, ANewClip: TdxRectF;
  AImgRect: TRect;
begin
  AOldClip := Graphics.ClipBounds;
  try
    AImgRect := ABounds;
    ANewClip := AOldClip;
    ANewClip.Intersect(ABounds.ToRectF);
    Graphics.SetClip(ANewClip);
    DrawImage(AImg, AImgRect, ASizing);
  finally
    Graphics.SetClip(AOldClip);
  end;
end;

procedure TdxGdiPlusPainter.DrawLine(APen: TdxGPPen; AX1: Single; AY1: Single; AX2: Single; AY2: Single);
begin
  Graphics.Line(AX1, AY1, AX2, AY2, APen);
end;

procedure TdxGdiPlusPainter.DrawLines(APen: TdxGPPen; const APoints: TArray<TdxPointF>);
begin
  Graphics.DrawLines(APen, APoints);
end;

procedure TdxGdiPlusPainter.FillPolygon(ABrush: TdxGPBrush; const APoints: TArray<TdxPointF>);
var
  AOldSmoothingMode: TdxGPSmoothingMode;
begin
  AOldSmoothingMode := Graphics.SmoothingMode;
  Graphics.SmoothingMode := smAntiAlias;
  Graphics.FillPolygon(ABrush, APoints);
  Graphics.SmoothingMode := AOldSmoothingMode;
end;

procedure TdxGdiPlusPainter.ExcludeCellBounds(const ARect: TRect; const ARowBounds: TRect);
begin
  Graphics.ExcludeClip(ARect);
end;

procedure TdxGdiPlusPainter.ResetCellBoundsClip;
begin
end;


procedure TdxGdiPlusPainter.FillRectangle(AColor: TdxAlphaColor; const ABounds: TdxRectF);
begin
  Graphics.FillRectangle(ABounds, AColor);
end;

procedure TdxGdiPlusPainter.FillRectangle(ABrush: TdxGPBrush; const ABounds: TdxRectF);
begin
  Graphics.FillRectangle(ABounds, ABrush);
end;

function TdxGdiPlusPainter.GetPen(AColor: TdxAlphaColor): TdxGPColorPen;
begin
  Result := TdxGPColorPen.Create(AColor);
end;

function TdxGdiPlusPainter.GetPen(AColor: TdxAlphaColor; AThickness: Single): TdxGPColorPen;
begin
  Result := TdxGPColorPen.Create(AColor, AThickness);
end;

procedure TdxGdiPlusPainter.ReleasePen(APen: TdxGPPen);
begin
  APen.Free;
end;

function TdxGdiPlusPainter.GetBrush(AColor: TdxAlphaColor): TdxGPBrush;
begin
  Result := TdxGPBrush.Create;
  Result.Color := AColor;
end;

procedure TdxGdiPlusPainter.ReleaseBrush(ABrush: TdxGPBrush);
begin
  ABrush.Free;
end;

procedure TdxGdiPlusPainter.TransformToPixels(var APoints: TArray<TdxPointF>);
begin
  TransformPoints(Graphics, TdxGpCoordinateSpace.CoordinateSpaceDevice, TdxGpCoordinateSpace.CoordinateSpaceWorld, APoints);
end;

procedure TdxGdiPlusPainter.TransformToLayoutUnits(var APoints: TArray<TdxPointF>);
begin
  TransformPoints(Graphics, TdxGpCoordinateSpace.CoordinateSpaceWorld, TdxGpCoordinateSpace.CoordinateSpaceDevice, APoints);
end;

procedure TdxGdiPlusPainter.PushRotationTransform(const ACenter: TPoint; AAngleInDegrees: Single);
begin
  Graphics.SaveWorldTransform;
  Inc(FTransformsStackCount);
  FRectangularBounds.Push(FRectangularClipBounds);
  Graphics.RotateWorldTransform(AAngleInDegrees, ACenter);
  Graphics.SaveClipRegion;
  FRectangularClipBounds := Graphics.ClipBounds;
end;

procedure TdxGdiPlusPainter.PopTransform;
begin
  Graphics.RestoreClipRegion;
  Graphics.RestoreWorldTransform;
  Dec(FTransformsStackCount);
  SetClipBounds(FRectangularBounds.Pop);
end;

procedure TdxGdiPlusPainter.PushSmoothingMode(AHighQuality: Boolean);
begin
  FSmoothingmodeStack.Push(Graphics.SmoothingMode);
  if AHighQuality then
    Graphics.SmoothingMode := smHighQuality
  else
    Graphics.SmoothingMode := smDefault;
end;

procedure TdxGdiPlusPainter.PopSmoothingMode;
begin
  Graphics.SmoothingMode := FSmoothingmodeStack.Pop;
end;

procedure TdxGdiPlusPainter.PushPixelOffsetMode(AHighQualtity: Boolean);
begin
  FPixelmodeStack.Push(Graphics.PixelOffsetMode);
  if AHighQualtity then
    Graphics.PixelOffsetMode := PixelOffsetModeHighQuality
  else
    Graphics.PixelOffsetMode := PixelOffsetModeDefault;
end;

procedure TdxGdiPlusPainter.PopPixelOffsetMode;
begin
  Graphics.PixelOffsetMode := FPixelmodeStack.Pop;
end;

{ TdxGdiPainter }

constructor TdxGdiPainter.Create(AGraphics: TdxGraphics{const ACache: IdxGraphicsCache}; AUnitConverter: TdxDocumentLayoutUnitConverter);
begin
  inherited Create(AGraphics {ACache});
  FInitialBufferItemCount := 64;
  Assert(AUnitConverter is TdxDocumentLayoutUnitConverter);
  FUnitConverter := AUnitConverter;
  GetCharacterWidthsBuffer(FInitialBufferItemCount);
  GetGlyphsBuffer(FInitialBufferItemCount);
end;

destructor TdxGdiPainter.Destroy;
begin
  if FGlyphsBuffer <> nil then
    FreeMem(FGlyphsBuffer);
  if FCharacterWidthsBuffer <> nil then
    FreeMem(FCharacterWidthsBuffer);
  inherited Destroy;
end;

procedure TdxGdiPainter.SetClipBounds(const ABounds: TdxRectF);
begin
  inherited SetClipBounds(ABounds);
  APISetClip(TRect.Round(ABounds));
end;

procedure TdxGdiPainter.APISetClip(const ABounds: TRect);
var
  AHdc: THandle;
begin
  AHdc := Graphics.GetHdc;
  try
    SetClipToHDC(ABounds, AHdc);
  finally
    Graphics.ReleaseHdc(AHdc);
  end;
end;

class function TdxGdiPainter.SetClipToHDC(const AClipBounds: TRect; AHdc: THandle): TRect;
var
  APoints: array[0..1] of TPoint;
  AHRgn: THandle;
begin
  APoints[0] := AClipBounds.TopLeft;
  APoints[1] := AClipBounds.BottomRight;
  LPtoDP(AHdc, APoints, Length(APoints));
  AHRgn := CreateRectRgn(APoints[0].X, APoints[0].Y, APoints[1].X, APoints[1].Y);
  try
    ExtSelectClipRgn(AHdc, AHRgn, RGN_COPY);
  finally
    DeleteObject(AHRgn);
  end;
  Result := AClipBounds;
end;

class function TdxGdiPainter.ExcludeClipFromHDC(const ABounds: TRect; AHdc: THandle): TRect;
var
  APoints: array[0..1] of TPoint;
begin
  APoints[0] := ABounds.TopLeft;
  APoints[1] := ABounds.BottomRight;
  LPtoDP(AHdc, APoints, Length(APoints));
  ExcludeClipRect(AHdc, APoints[0].X, APoints[0].Y, APoints[1].X, APoints[1].Y);
  Result := ABounds;
end;

procedure TdxGdiPainter.APIExcludeClip(const ABounds: TRect);
var
  AHdc: THandle;
begin
  if (ABounds.Width < 1) or (ABounds.Height < 1) then
    Exit;

  AHdc := Graphics.GetHdc;
  try
    ExcludeClipFromHDC(ABounds, AHdc);
  finally
    Graphics.ReleaseHdc(AHdc);
  end;
end;

procedure TdxGdiPainter.ExcludeCellBounds(const ARect: TRect; const ARowBounds: TRect);
begin
  inherited ExcludeCellBounds(ARect, ARowBounds);
  APIExcludeClip(ARect);
end;

function TdxGdiPainter.GetCharacterWidthsBuffer(AItemsCount: Integer): Pointer;
var
  ASize: Integer;
begin
  ASize := SizeOf(Integer) * AItemsCount;
  if ASize > FCharacterWidthsBufferSize then
  begin
    FCharacterWidthsBuffer := ReallocMemory(FCharacterWidthsBuffer, ASize);
    FCharacterWidthsBufferSize := ASize;
  end;
  Result := FCharacterWidthsBuffer;
end;

function TdxGdiPainter.GetGlyphsBuffer(AItemsCount: Integer): Pointer;
var
  ASize: Integer;
begin
  ASize := SizeOf(Word) * AItemsCount;
  if ASize > FGlyphsBufferSize then
  begin
    FGlyphsBuffer := ReallocMemory(FGlyphsBuffer, ASize);
    FGlyphsBufferSize := ASize;
  end;
  Result := FGlyphsBuffer;
end;

procedure TdxGdiPainter.DrawSpacesString(const AText: string; AFontInfo: TdxFontInfo; const ABounds: TRect);
begin
  if HasTransform then
    inherited DrawSpacesString(AText, AFontInfo, ABounds)
  else
    DrawStringImpl(AText, TdxGdiFontInfo(AFontInfo), TextForeColor, ABounds, StringFormat, SpacesPlaceholdersDrawStringImplementation);
end;

procedure TdxGdiPainter.DrawString(const AText: string; AFontInfo: TdxFontInfo; const ABounds: TRect);
begin
  DrawString(AText, AFontInfo, ABounds, StringFormat);
end;

procedure TdxGdiPainter.DrawString(const AText: string; AFontInfo: TdxFontInfo; const ABounds: TRect; AStringFormat: TdxGPStringFormat);
begin
  if HasTransform then
    inherited DrawString(AText, AFontInfo, ABounds, AStringFormat)
  else
    DrawStringImpl(AText, TdxGdiFontInfo(AFontInfo), TextForeColor, ABounds, AStringFormat, DefaultDrawStringImplementation);
end;

procedure TdxGdiPainter.DefaultDrawStringImplementation(AHdc: THandle; const AText: string;
  AFontInfo: TdxGdiFontInfo; const ABounds: TRect; AStringFormat: TdxGPStringFormat);
begin
  DrawNonCachedString(AHdc, AFontInfo, AText, ABounds, AStringFormat);
end;

procedure TdxGdiPainter.SpacesPlaceholdersDrawStringImplementation(AHdc: THandle; const AText: string;
  AFontInfo: TdxGdiFontInfo; const ABounds: TRect; AStringFormat: TdxGPStringFormat);
var
  ACount, ASpaceWidth, ARemainder, I, AWidth: Integer;
  AStringViewInfo: TdxGdiStringViewInfo;
  AClipRect: TRect;
begin
  ACount := Length(AText);
  if ACount <= 0 then
    Exit;

  AStringViewInfo := GenerateStringViewInfo(AHdc, AText);
  AClipRect := ABounds;

  ASpaceWidth := ABounds.Width div ACount;
  ARemainder := ABounds.Width mod ACount;
  for I := 0 to ACount - 1 do
  begin
    AWidth := ASpaceWidth;
    if ARemainder > 0 then
    begin
      Inc(AWidth);
      Dec(ARemainder);
    end;
    AStringViewInfo.CharacterWidths[I] := AWidth;
  end;

  ExtTextOut(AHdc, ABounds.Left, ABounds.Top, ETO_GLYPH_INDEX, @AClipRect,
    @AStringViewInfo.Glyphs[0], AStringViewInfo.GlyphCount, @AStringViewInfo.CharacterWidths[0]);
end;

procedure TdxGdiPainter.DrawStringImpl(const AText: string; AFontInfo: TdxGdiFontInfo; AForeColor: TdxAlphaColor;
  const ABounds: TRect; AStringFormat: TdxGPStringFormat; AImpl: TdxDrawStringActualImplementationDelegate);
var
  AHdc: THandle;
  ABoundsRect: TRect;
  ATextColor: TColor;
begin
  ABoundsRect := CorrectTextDrawingBounds(AFontInfo, ABounds);
  ABoundsRect.X := FUnitConverter.SnapToPixels(ABoundsRect.X, Graphics.DpiX);
  ABoundsRect.Offset(0, -AFontInfo.GdiOffset);
  AHdc := Graphics.GetHdc;
  try
    if not RectVisible(AHdc, ABoundsRect) then
      Exit;


    SelectObject(AHdc, AFontInfo.GdiFontHandle);
    ATextColor := TdxAlphaColors.ToColor(AForeColor);
    SetTextColor(AHdc, ATextColor);
    SetBkMode(AHdc, TRANSPARENT);
    AImpl(AHdc, AText, AFontInfo, ABoundsRect, AStringFormat);
  finally
    Graphics.ReleaseHdc(AHdc);
  end;
end;

function TdxGdiPainter.GetMeasureHdc(AHdc: THandle): THandle;
begin
  Result := AHdc;
end;

procedure TdxGdiPainter.ReleaseMeasureHdc(AMeasureHdc: THandle);
begin
end;

procedure TdxGdiPainter.DrawNonCachedString(AHdc: THandle; AFontInfo: TdxFontInfo;
  const AText: string; const ABounds: TRect; AStringFormat: TdxGPStringFormat);
var
  AMeasureHdc: THandle;
  AGdiFontInfo: TdxGdiFontInfo absolute AFontInfo;
begin
  AMeasureHdc := GetMeasureHdc(AHdc);
  try
    SelectObject(AMeasureHdc, AGdiFontInfo.GdiFontHandle);
    DrawNonCachedStringCore(AHdc, AMeasureHdc, AText, ABounds, AStringFormat);
  finally
    ReleaseMeasureHdc(AMeasureHdc);
  end;
end;

procedure TdxGdiPainter.DrawNonCachedStringCore(AHdc: THandle; AMeasureHdc: THandle;
  const AText: string; const ABounds: TRect; AStringFormat: TdxGPStringFormat);
var
  APrevValue: Integer;
begin
  if (AStringFormat.Alignment = StringAlignmentNear) and (AStringFormat.LineAlignment = StringAlignmentNear) then
    DrawNonCachedStringCoreAlignTopLeft(AHdc, AMeasureHdc, AText, ABounds)
  else
  begin
    APrevValue := SetTextAlign(AHdc, TA_LEFT or TA_TOP);
    try
      DrawNonCachedStringCoreAligned(AHdc, AMeasureHdc, AText, ABounds, AStringFormat);
    finally
      SetTextAlign(AHdc, APrevValue);
    end;
  end;
end;

procedure TdxGdiPainter.DrawNonCachedStringCoreAlignTopLeft(AHdc: THandle; AMeasureHdc: THandle; const AText: string; const ABounds: TRect);
begin
  ExtTextOut(AHdc, ABounds.Left, ABounds.Top, ETO_IGNORELANGUAGE, ABounds, AText, Length(AText), nil);
end;

procedure TdxGdiPainter.DrawNonCachedStringCoreAligned(AHdc: THandle; AMeasureHdc: THandle; const AText: string; const ABounds: TRect; AStringFormat: TdxGPStringFormat);
begin
  NotImplemented;
end;

function TdxGdiPainter.GetAlignedTextPosition(const ABounds: TRect; const ATextSize: TSize; AStringFormat: TdxGPStringFormat): TPoint;
begin
  Result.Init(GetAlignedTextLeft(ABounds, ATextSize.Width, AStringFormat), GetAlignedTextTop(ABounds, ATextSize.Height, AStringFormat));
end;

function TdxGdiPainter.GetAlignedTextLeft(const ABounds: TRect; ATextWidth: Integer; AStringFormat: TdxGPStringFormat): Integer;
begin
  case AStringFormat.Alignment of
    StringAlignmentFar:
      Result := ABounds.Left + ABounds.Width - ATextWidth;
    StringAlignmentCenter:
      Result := ABounds.Left + (ABounds.Width - ATextWidth) div 2;
    else
      Result := ABounds.Left;
  end;
end;

function TdxGdiPainter.GetAlignedTextTop(const ABounds: TRect; ATextHeight: Integer; AStringFormat: TdxGPStringFormat): Integer;
begin
  case AStringFormat.LineAlignment of
    StringAlignmentFar:
      Result := ABounds.Top + ABounds.Height - ATextHeight;
    StringAlignmentCenter:
      Result := ABounds.Top + (ABounds.Height - ATextHeight) div 2;
    else
      Result := ABounds.Top;
  end;
end;

function TdxGdiPainter.GenerateStringViewInfo(AHdc: THandle; const AText: string): TdxGdiStringViewInfo;
var
  AGcpResults: TGCPResults;
  ASz: Cardinal;
  ALength: Integer;
  ALpDx: TIntegerDynArray;
  ALpGlyphs: TSmallIntDynArray;
  AWord: PWord;
begin
  ALength := Length(AText);
  cxZeroMemory(@AGcpResults, SizeOf(TGCPResults));
  AGcpResults.lStructSize := SizeOf(GCP_RESULTS);
  AGcpResults.lpOutString := nil;
  AGcpResults.lpOrder := nil;

  SetLength(ALpDx, ALength);
  AGcpResults.lpDx := @ALpDx[0];
  AGcpResults.lpCaretPos := nil;
  AGcpResults.lpClass := nil;

  SetLength(ALpGlyphs, ALength);
  AGcpResults.lpGlyphs := @ALpGlyphs[0];
  AGcpResults.nGlyphs := ALength;

  if ALength > 0 then
  begin
    AWord := Pointer(AGcpResults.lpGlyphs);
    AWord^ := 0;
  end;

  ASz := GetCharacterPlacement(AHdc, PChar(AText), ALength, 0, AGcpResults, GCP_USEKERNING or GCP_LIGATE);
  if (ASz = 0) and (ALength > 0) then
  begin
    AGcpResults.lpDx := nil;
    AGcpResults.lpGlyphs := nil;
  end;

  Result.Glyphs := ALpGlyphs;
  Result.GlyphCount := AGcpResults.nGlyphs;
  Result.CharacterWidths := ALpDx;
end;


{ TdxRichEditGdiPainter }

constructor TdxRichEditGdiPainter.Create(AGraphics: TdxGraphics; AMeasurer: TdxGdiBoxMeasurer);
begin
  inherited Create(AGraphics, AMeasurer.DocumentModel.LayoutUnitConverter);
  Assert(AMeasurer <> nil);
  FMeasurer := AMeasurer;
end;

procedure TdxRichEditGdiPainter.DefaultDrawStringImplementation(AHdc: THandle; const AText: string; AFontInfo: TdxGdiFontInfo;
  const ABounds: TRect; AStringFormat: TdxGPStringFormat);
var
  ATextInfo: TdxGdiTextViewInfo;
begin
  ATextInfo := TdxGdiTextViewInfo(Measurer.TextViewInfoCache.TryGetTextViewInfo(AText, AFontInfo));
  if ATextInfo <> nil then
    DrawStringCore(AHdc, AText, ABounds, ATextInfo, AFontInfo)
  else
    DrawNonCachedString(AHdc, AFontInfo, AText, ABounds, AStringFormat);
end;

procedure TdxRichEditGdiPainter.DrawImageCore(AImg: TdxOfficeImage; const ABounds: TdxRectF; ASizing: TdxImageSizeMode);
var
  ABrush: TdxGPBrush;
begin
  if ASizing <> TdxImageSizeMode.Tile then
    inherited DrawImageCore(AImg, ABounds, ASizing)
  else
  begin
    ABrush := TdxGPBrush.Create;
    try
      ABrush.Style := TdxGPBrushStyle.gpbsTexture;
      ABrush.Texture := AImg;
      Graphics.FillRectangle(ABounds, ABrush);
    finally
      ABrush.Free;
    end;
  end;
end;

procedure TdxRichEditGdiPainter.DrawStringCore(AHdc: THandle; const AText: string; const ABounds: TRect;
  ATextInfo: TdxGdiTextViewInfo; AFontInfo: TdxGdiFontInfo);
begin
  ExtTextOut(AHdc, ABounds.Left, ABounds.Top,
    ETO_GLYPH_INDEX or ETO_IGNORELANGUAGE,
    @ABounds, @ATextInfo.Glyphs[0], ATextInfo.GlyphCount, @ATextInfo.CharacterWidths[0]);
end;

function TdxRichEditGdiPainter.GetMeasureHdc(AHdc: THandle): THandle;
begin
  Result := Measurer.Graphics.GetHdc;
end;

procedure TdxRichEditGdiPainter.ReleaseMeasureHdc(AHdc: THandle);
begin
  Measurer.Graphics.ReleaseHdc(AHdc);
end;

{ TdxTableCornerPainter }

procedure TdxTableCornerPainter.DrawCorner(const APainter: IdxPainterWrapper; X: Integer; Y: Integer; ACorner: TdxCornerViewInfoBase);
var
  APattern: TArray<TArray<Boolean>>;
  ARowPattern: TArray<Boolean>;
  AWidths, AHeights: TArray<Single>;
  ALeftTop: TdxPointF;
  AColumnIndex, AColumnCount, ARowCount, ARowIndex: Integer;
  ACurrentX, ACurrentY, ANextX, ANextY: Single;
begin
  APattern := ACorner.Pattern;
  AWidths := GetSnappedWidths(APainter, ACorner);
  AHeights := GetSnappedHeights(APainter, ACorner);
  ALeftTop := GetCornerSnappedPoint(APainter, X, Y, ACorner);
  ARowCount := Length(APattern);
  ACurrentY := ALeftTop.Y;
  ANextY := ACurrentY + AHeights[0];
  for ARowIndex := 0 to ARowCount - 1 do
  begin
    ARowPattern := APattern[ARowIndex];
    AColumnCount := Length(ARowPattern);
    ACurrentY := ANextY;
    ANextY := ACurrentY + AHeights[ARowIndex + 1];
    ACurrentX := ALeftTop.X;
    ANextX := ACurrentX + AWidths[0];
    for AColumnIndex := 0 to AColumnCount - 1 do
    begin
      ACurrentX := ANextX;
      ANextX := ACurrentX + AWidths[AColumnIndex + 1];
      if ARowPattern[AColumnIndex] then
        APainter.FillRectangle(GetDrawingColor(ACorner.Color, ACorner.CornerType), cxRectFBounds(ACurrentX, ACurrentY, AWidths[AColumnIndex + 1], AHeights[ARowIndex + 1]));
    end;
  end;
end;

function TdxTableCornerPainter.GetDrawingColor(AColor: TdxAlphaColor; ACornerType: TdxCornerViewInfoType): TdxAlphaColor;
begin
  if TdxAlphaColors.IsEmpty(AColor) then
    Result := TdxAlphaColors.Black
  else
    Result := AColor;
end;

function TdxTableCornerPainter.GetSnappedWidths(const APainter: IdxPainterWrapper; ACorner: TdxCornerViewInfoBase): TArray<Single>;
var
  I: Integer;
begin
  SetLength(Result, Length(ACorner.Widths));
  Result[0] := ACorner.Widths[0] * ACorner.WidthF;
  for I := 1 to Length(ACorner.Widths) - 1 do
    Result[I] := (ACorner.Widths[I] - ACorner.Widths[I - 1]) * ACorner.WidthF;
  APainter.SnapWidths(Result);
end;

function TdxTableCornerPainter.GetSnappedHeights(const APainter: IdxPainterWrapper; ACorner: TdxCornerViewInfoBase): TArray<Single>;
var
  I: Integer;
begin
  SetLength(Result, Length(ACorner.Heights));
  Result[0] := ACorner.Heights[0] * ACorner.HeightF;
  for I := 1 to Length(ACorner.Heights) - 1 do
    Result[I] := (ACorner.Heights[I] - ACorner.Heights[I - 1]) * ACorner.HeightF;
  APainter.SnapHeights(Result);
end;

function TdxTableCornerPainter.GetCornerSnappedPoint(const APainter: IdxPainterWrapper; X: Single; Y: Single; ACorner: TdxCornerViewInfoBase): TdxPointF;
begin
  case ACorner.CornerType of
    TdxCornerViewInfoType.Normal:
      Result := APainter.GetSnappedPoint(dxPointF(X - (ACorner.Width + ACorner.Width mod 2) / 2, Y));
    TdxCornerViewInfoType.OuterHorizontalStart:
      Result := APainter.GetSnappedPoint(dxPointF(X - ACorner.Width, Y));
    TdxCornerViewInfoType.OuterHorizontalEnd:
      Result := APainter.GetSnappedPoint(dxPointF(X, Y));
    TdxCornerViewInfoType.OuterVerticalStart:
      Result := APainter.GetSnappedPoint(dxPointF(X - (ACorner.Width + ACorner.Width mod 2) / 2, Y - ACorner.Height));
    TdxCornerViewInfoType.OuterVerticalEnd:
      Result := APainter.GetSnappedPoint(dxPointF(X - (ACorner.Width + ACorner.Width mod 2) / 2, Y));
    TdxCornerViewInfoType.InnerTopLeft:
      Result := APainter.GetSnappedPoint(dxPointF(0, 0));
    TdxCornerViewInfoType.InnerTopMiddle:
      Result := APainter.GetSnappedPoint(dxPointF(X - (ACorner.Width + ACorner.Width mod 2) / 2, 0));
    TdxCornerViewInfoType.InnerTopRight:
      Result := APainter.GetSnappedPoint(dxPointF(X - ACorner.Width, 0));
    TdxCornerViewInfoType.InnerLeftMiddle:
      Result := APainter.GetSnappedPoint(dxPointF(0, Y - (ACorner.Height + ACorner.Height mod 2) / 2));
    TdxCornerViewInfoType.InnerRightMiddle:
      Result := APainter.GetSnappedPoint(dxPointF(X - ACorner.Width, Y - (ACorner.Height + ACorner.Height mod 2) / 2));
    TdxCornerViewInfoType.InnerBottomLeft:
      Result := APainter.GetSnappedPoint(dxPointF(0, Y - ACorner.Height));
    TdxCornerViewInfoType.InnerBottomMiddle:
      Result := APainter.GetSnappedPoint(dxPointF(X - (ACorner.Width + ACorner.Width mod 2) / 2, Y - ACorner.Height));
    TdxCornerViewInfoType.InnerBottomRight:
      Result := APainter.GetSnappedPoint(dxPointF(X - ACorner.Width, Y - ACorner.Height));
    TdxCornerViewInfoType.InnerNormal:
      Result := APainter.GetSnappedPoint(dxPointF(X - (ACorner.Width + ACorner.Width mod 2) / 2, Y - (ACorner.Height + ACorner.Height mod 2) / 2));
  else
    Result := dxPointF(0, 0);
    TdxRichEditExceptions.ThrowInternalException;
  end;
end;

function TdxTableCornerPainter.GetCornerWidth(const APainter: IdxPainterWrapper; ACorner: TdxCornerViewInfoBase): Single;
var
  AWidths: TArray<Single>;
begin
  AWidths := GetSnappedWidths(APainter, ACorner);
  Result := GetTotalSize(AWidths);
end;

function TdxTableCornerPainter.GetCornerHeight(const APainter: IdxPainterWrapper; ACorner: TdxCornerViewInfoBase): Single;
var
  AHeights: TArray<Single>;
begin
  AHeights := GetSnappedHeights(APainter, ACorner);
  Result := GetTotalSize(AHeights);
end;

function TdxTableCornerPainter.GetTotalSize(const AWidths: TArray<Single>): Single;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Length(AWidths) - 1 do
    Result := Result + AWidths[I];
end;

{ TdxTableBorderPainter }

constructor TdxTableBorderPainter.Create(const APainter: IdxPainterWrapper; AWidth: Single);
begin
  inherited Create;
  FWidthF := AWidth;
  FHalfBorderWidth := AWidth / 2;
  FPainter := APainter;
  FCornerPainter := TdxTableCornerPainter.Create;
end;

destructor TdxTableBorderPainter.Destroy;
begin
  FCornerPainter.Free;
  inherited Destroy;
end;

function TdxTableBorderPainter.GetStartSnappedPoint(const X, Y: Single): TdxPointF;
var
  AWidth: Integer;
begin
  AWidth := Trunc(FWidthF);
  Result := FPainter.GetSnappedPoint(dxPointF(X - (AWidth + AWidth mod 2) / 2, Y));
end;

procedure TdxTableBorderPainter.DrawBorder(const ABorderViewInfo: TdxTableBorderViewInfoBase; const ACellBounds: TRect);
var
  ALeft, ARight, ATop, ABottom: TdxPointF;
  X, Y, AStartCornerWidth, AStartCornerHeight: Single;
begin
  if ABorderViewInfo.BorderType and TdxBorderTypes.Horizontal <> 0 then
  begin
    X := ACellBounds.Left;
    if ABorderViewInfo.BorderType = TdxBorderTypes.Top then
      Y := ACellBounds.Top
    else
      Y := ACellBounds.Bottom;
    if ABorderViewInfo.HasStartCorner then
      ALeft := FCornerPainter.GetCornerSnappedPoint(Painter, X, Y, ABorderViewInfo.StartCorner)
    else
      ALeft := Painter.GetSnappedPoint(dxPointF(X, Y));
    if ABorderViewInfo.HasEndCorner then
      ARight := FCornerPainter.GetCornerSnappedPoint(Painter, X + ACellBounds.Width, Y, ABorderViewInfo.EndCorner)
    else
      ARight := Painter.GetSnappedPoint(dxPointF(X + ACellBounds.Width, Y));
    AStartCornerWidth := 0;
    if ABorderViewInfo.HasStartCorner then
      AStartCornerWidth := FCornerPainter.GetCornerWidth(Painter, ABorderViewInfo.StartCorner);
    ALeft.X := ALeft.X + AStartCornerWidth;

    DrawHorizontalBorder(GetDrawingColor(ABorderViewInfo.Border.Color), ALeft, ARight.X - ALeft.X);
  end;
  if ABorderViewInfo.BorderType and TdxBorderTypes.Vertical <> 0 then
  begin
    if ABorderViewInfo.BorderType = TdxBorderTypes.Left then
      X := ACellBounds.Left
    else
      X := ACellBounds.Right;
    Y := ACellBounds.Top;
    if ABorderViewInfo.HasStartCorner then
      ATop := FCornerPainter.GetCornerSnappedPoint(Painter, X, Y, ABorderViewInfo.StartCorner)
    else
      ATop := Painter.GetSnappedPoint(dxPointF(X, Y));
    if ABorderViewInfo.HasStartCorner and (ABorderViewInfo.StartCorner.CornerType <= TdxCornerViewInfoType.OuterVerticalEnd) then
      ATop.X := GetStartSnappedPoint(X, Y).X;
    if ABorderViewInfo.HasEndCorner then
      ABottom := FCornerPainter.GetCornerSnappedPoint(Painter, X, Y + ACellBounds.Height, ABorderViewInfo.EndCorner)
    else
      ABottom := Painter.GetSnappedPoint(dxPointF(X, Y + ACellBounds.Height));
    AStartCornerHeight := 0;
    if ABorderViewInfo.HasStartCorner then
      AStartCornerHeight := FCornerPainter.GetCornerHeight(Painter, ABorderViewInfo.StartCorner);
    ATop.Y := ATop.Y + AStartCornerHeight;
    DrawVerticalBorder(GetDrawingColor(ABorderViewInfo.Border.Color), ATop, ABottom.Y - ATop.Y);
  end;
end;

function TdxTableBorderPainter.GetDrawingColor(AColor: TdxAlphaColor): TdxAlphaColor;
begin
  if TdxAlphaColors.IsEmpty(AColor) then
    Result := TdxAlphaColors.Black
  else
    Result := AColor;
end;

procedure TdxTableBorderPainter.DrawHorizontalLines(AColor: TdxAlphaColor; const AOffsets: TArray<Single>; const ALeft: TdxPointF; const AWidth: Single);
begin
  DrawHorizontalLines(AColor, AOffsets, ALeft, AWidth, nil);
end;

procedure TdxTableBorderPainter.DrawHorizontalLines(AColor: TdxAlphaColor; const AOffsets: TArray<Single>;
  const ALeft: TdxPointF; const AWidth: Single; AUnderline: TdxUnderline);
var
  ABounds: TdxRectF;
  Y, AHeight: Single;
  I, ALength: Integer;
  AHeights: TArray<Single>;
  ALinePainter: IdxUnderlinePainter;
begin
  SetLength(AHeights, Length(AOffsets));
  AHeights[0] := AOffsets[0];
  for I := 0 to Length(AOffsets) - 2 do
    AHeights[I + 1] := AOffsets[I + 1] - AOffsets[I];
  Painter.SnapHeights(AHeights);

  Y := ALeft.Y + AHeights[0];
  ALength := Length(AHeights);
  I := 1;
  while I < ALength do
  begin
    AHeight := AHeights[I];

    if AUnderline <> nil then
    begin
      ABounds := cxRectFBounds(ALeft.X, Y + AHeight / 2, AWidth, AHeight);
      ALinePainter := Painter.HorizontalLinePainter as IdxUnderlinePainter;
      AUnderline.Draw(ALinePainter, ABounds, AColor);
    end
    else
    begin
      ABounds := cxRectFBounds(ALeft.X, Y, AWidth, AHeight);
      Painter.FillRectangle(AColor, ABounds);
    end;
    Y := Y + AHeight;
    if I + 1 < Length(AHeights) then
        Y := Y + AHeights[I + 1];
    Inc(I, 2);
  end;
end;

procedure TdxTableBorderPainter.DrawVerticalLines(AColor: TdxAlphaColor; const AOffsets: TArray<Single>; const ATop: TdxPointF;
  const AHeight: Single);
begin
  DrawVerticalLines(AColor, AOffsets, ATop, AHeight, nil);
end;

procedure TdxTableBorderPainter.DrawVerticalLines(AColor: TdxAlphaColor; const AOffsets: TArray<Single>; const
  ATop: TdxPointF; const AHeight: Single; AUnderline: TdxUnderline);
var
  I, ALength: Integer;
  X, AWidth: Single;
  AWidths: TArray<Single>;
  ABounds: TdxRectF;
  ALinePainter: IdxUnderlinePainter;
begin
  SetLength(AWidths, Length(AOffsets));
  AWidths[0] := AOffsets[0];
  for I := 0 to Length(AOffsets) - 2 do
    AWidths[I + 1] := AOffsets[I + 1] - AOffsets[I];
  Painter.SnapWidths(AWidths);
  X := ATop.X + AWidths[0];
  ALength := Length(AWidths);
  I := 1;
  while I < ALength do
  begin
    AWidth := AWidths[I];
    if AUnderline <> nil then
    begin
      ABounds := cxRectFBounds(X + AWidth / 2, ATop.Y, AWidth, AHeight);
      ALinePainter := Painter.VerticalLinePainter as IdxUnderlinePainter;
      AUnderline.Draw(ALinePainter, ABounds, AColor);
    end
    else
    begin
      ABounds := cxRectFBounds(X, ATop.Y, AWidth, AHeight);
      Painter.FillRectangle(AColor, ABounds);
    end;
    X := X + AWidth;
    if I + 1 < Length(AWidths) then
      X := X + AWidths[I + 1];
    Inc(I, 2);
  end;
end;

{ TdxSingleBorderPainter }

constructor TdxSingleBorderPainter.Create(const APainter: IdxPainterWrapper; AWidth: Single;
  AUnderline: TdxUnderline);
begin
  if AWidth = 0 then
    AWidth := 1;
  inherited Create(APainter, AWidth);
  SetLength(FOffsets, 2);
  FOffsets[0] := 0;
  FOffsets[1] := AWidth;
  FUnderline := AUnderline;
end;

procedure TdxSingleBorderPainter.DrawVerticalBorder(AColor: TdxAlphaColor; const ATop: TdxPointF; AHeight: Single);
begin
  DrawVerticalLines(AColor, FOffsets, ATop, AHeight, FUnderline);
end;

procedure TdxSingleBorderPainter.DrawHorizontalBorder(AColor: TdxAlphaColor; const ALeft: TdxPointF; AWidth: Single);
begin
  DrawHorizontalLines(AColor, FOffsets, ALeft, AWidth, FUnderline);
end;

{ TdxDoubleBorderPainter }

constructor TdxDoubleBorderPainter.Create(const APainter: IdxPainterWrapper; const ACompoundArray: TArray<Single>;
  AWidth: Single);
var
  I: Integer;
begin
  inherited Create(APainter, AWidth, nil);
  SetLength(FOffsets, Length(ACompoundArray));
  for I := 0 to Length(ACompoundArray) - 1 do
    FOffsets[I] := ACompoundArray[I] * AWidth;
end;

procedure TdxDoubleBorderPainter.DrawHorizontalBorder(AColor: TdxAlphaColor; const ALeft: TdxPointF; AWidth: Single);
begin
  DrawHorizontalLines(AColor, FOffsets, ALeft, AWidth);
end;

procedure TdxDoubleBorderPainter.DrawVerticalBorder(AColor: TdxAlphaColor; const ATop: TdxPointF; AHeight: Single);
begin
  DrawVerticalLines(AColor, FOffsets, ATop, AHeight);
end;

end.
