{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressPDFViewer                                         }
{                                                                    }
{           Copyright (c) 2015-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSPDFVIEWER AND ALL              }
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

unit dxPDFCommandConstructor;

{$I cxVer.inc}

interface

uses
  Types, Classes, Generics.Defaults, Generics.Collections, SysUtils, dxCoreClasses, cxGeometry, dxPDFBase, dxPDFTypes,
  dxPDFCore, dxPDFCommandInterpreter, dxPDFInteractivity, dxPDFFontUtils;

type
  TdxPDFHotkeyPrefix = (hpNone, hpHide); // for internal use
  TdxPDFStringAlignment = (saNear, saCenter, saFar); // for internal use
  TdxPDFStringFormatFlags = (sffMeasureTrailingSpaces = 1, sffNoWrap = 2, sffLineLimit = 4, sffNoClip = 8); // for internal use
  TdxPDFStringTrimming = (stNone, stCharacter, stWord, stEllipsisCharacter, stEllipsisWord); // for internal use

  { TdxPDFStringFormat }

  TdxPDFStringFormat = record // for internal use
  strict private
    FAlignment: TdxPDFStringAlignment;
    FDirectionRightToLeft: Boolean;
    FFormatFlags: TdxPDFStringFormatFlags;
    FHotkeyPrefix: TdxPDFHotkeyPrefix;
    FLeadingMarginFactor: Double;
    FLineAlignment: TdxPDFStringAlignment;
    FTabStopInterval: Double;
    FTrailingMarginFactor: Double;
    FTrimming: TdxPDFStringTrimming;
  public
    class function Create: TdxPDFStringFormat; overload; static;
    class function Create(AFormatFlags: TdxPDFStringFormatFlags): TdxPDFStringFormat; overload; static;
    class function Create(AFormat: TdxPDFStringFormat): TdxPDFStringFormat; overload; static;
    class function CreateGenericDefault: TdxPDFStringFormat; overload; static;
    class function CreateGenericTypographic: TdxPDFStringFormat; overload; static;
    function Clone: TdxPDFStringFormat;

    property Alignment: TdxPDFStringAlignment read FAlignment write FAlignment;
    property DirectionRightToLeft: Boolean read FDirectionRightToLeft write FDirectionRightToLeft;
    property FormatFlags: TdxPDFStringFormatFlags read FFormatFlags write FFormatFlags;
    property HotkeyPrefix: TdxPDFHotkeyPrefix read FHotkeyPrefix write FHotkeyPrefix;
    property LeadingMarginFactor: Double read FLeadingMarginFactor write FLeadingMarginFactor;
    property LineAlignment: TdxPDFStringAlignment read FLineAlignment write FLineAlignment;
    property TabStopInterval: Double read FTabStopInterval write FTabStopInterval;
    property TrailingMarginFactor: Double read FTrailingMarginFactor write FTrailingMarginFactor;
    property Trimming: TdxPDFStringTrimming read FTrimming write FTrimming;
  end;

  { TdxPDFCommandConstructor }

  TdxPDFCommandConstructor = class // for internal use
  strict private type
    TdxPDFResourcesAccess = class(TdxPDFResources);
  strict private const
    Pattern = '/Pattern ';
    UseEvenOddRule = Byte('*');
  strict private
    FEllipticFactor: Single;
    FCurrentTransformationMatrix: TdxPDFTransformationMatrix;
    FMatrixStack: TStack<TdxPDFTransformationMatrix>;
    FResources: TdxPDFResourcesAccess;
    FStream: TdxPDFWriterStream;
    function GetCommands: TBytes;
    procedure AppendBezierCurve(const P1, P2, P3: TdxPointF);
    procedure AppendEllipse(const ARect: TdxRectF);
    procedure AppendPolygon(const APoints: TdxPDFPoints);
    procedure DrawXObject(AXObjectNumber: Integer; AMatrix: TdxPDFTransformationMatrix);
    procedure GeneratePathCommands(APath: TdxPDFGraphicsPath);
    procedure SetCurrentTransformationMatrix(const AValue: TdxPDFTransformationMatrix);
    procedure WriteAppendBezierCurveCommand;
    procedure WriteAppendLineSegmentCommand;
    procedure WriteBeginPathCommand;
    procedure WriteClosePathCommand;
    procedure WriteCloseBracket;
    procedure WriteCommand(const ACommandClass: TdxPDFCustomCommandClass);
    procedure WriteDouble(AValue: Double);
    procedure WriteIntersectClipCommand(ANonZero: Boolean);
    procedure WriteFillPathCommand(ANonZero: Boolean);
    procedure WriteHexadecimalString(const AData: TBytes);
    procedure WriteNameDelimiter;
    procedure WriteOpenBracket;
    procedure WriteOperationData(const AData: string);
    procedure WritePoint(const P: TdxPointF);
    procedure WriteSpace;
    procedure WriteString(const S: string);
    procedure WriteStrokePathCommand;
  public
    constructor Create(AResources: TdxPDFResources);
    destructor Destroy; override;

    procedure AddCommands(const AData: TBytes);
    procedure AppendRectangle(const ARect: TdxRectF);
    procedure BeginMarkedContent;
    procedure BeginText;
    procedure CloseAndStrokePath;
    procedure EndMarkedContent;
    procedure EndText;
    procedure DrawEllipse(const ARect: TdxRectF);
    procedure DrawImage(AXObjectNumber: Integer; const ARect: TdxRectF);
    procedure DrawLine(const P1, P2: TdxPointF);
    procedure DrawLines(const AValue: TdxPDFPoints);
    procedure DrawPolygon(const AValue: TdxPDFPoints);
    procedure DrawRectangle(const ARect: TdxRectF);
    procedure IntersectClip(const ARect: TdxRectF); overload;
    procedure IntersectClip(APath: TdxPDFGraphicsPath; ANonZero: Boolean = True); overload;
    procedure IntersectClip(APaths: TList<TdxPDFGraphicsPath>; ANonZero: Boolean); overload;
    procedure FillEllipse(const ARect: TdxRectF);
    procedure FillPath(APaths: TList<TdxPDFGraphicsPath>; ANonZero: Boolean);
    procedure FillPolygon(const APoints: TdxPDFPoints; ANonZero: Boolean);
    procedure FillRectangle(const ARect: TdxRectF);
    procedure ModifyTransformationMatrix(AMatrix: TdxPDFTransformationMatrix);
    procedure SaveGraphicsState;
    procedure SetCharacterSpacing(AValue: Single);
    procedure SetColorForNonStrokingOperations(AColor: TdxPDFColor);
    procedure SetColorForStrokingOperations(AColor: TdxPDFColor); overload;
    procedure SetColorForStrokingOperations(AColor: TdxPDFARGBColor); overload;
    procedure SetGraphicsStateParameters(AParameters: TdxPDFGraphicsStateParameters);
    procedure SetLineCapStyle(AValue: TdxPDFLineCapStyle);
    procedure SetLineJoinStyle(AValue: TdxPDFLineJoinStyle);
    procedure SetLineStyle(AValue: TdxPDFLineStyle);
    procedure SetLineWidth(AValue: Single);
    procedure SetMiterLimit(AValue: Single);
    procedure SetObliqueTextMatrix(X, Y: Single);
    procedure SetTextFont(AFont: TObject; AFontSize: Single); overload;
    procedure SetTextFont(const AFontName: string; AFontSize: Single); overload;
    procedure SetTextHorizontalScaling(AValue: Single);
    procedure SetTextRenderingMode(AValue: TdxPDFTextRenderingMode);
    procedure SetWordSpacing(AValue: Single);
    procedure ShowText(const AText: TBytes; AGlyphOffsets: TDoubleDynArray);
    procedure StartTextLineWithOffsets(X, Y: Single);
    procedure RestoreGraphicsState;

    property Commands: TBytes read GetCommands;
    property CurrentTransformationMatrix: TdxPDFTransformationMatrix read FCurrentTransformationMatrix write
      SetCurrentTransformationMatrix;
  end;

  { TdxPDFFormCommandConstructor }

  TdxPDFFormCommandConstructor = class(TdxPDFCommandConstructor) // for internal use
  strict private
    FContentSquare: TdxRectF;
    FForm: TdxPDFForm;
    function GetBoundingBox: TdxRectF;
  public
    constructor Create(AForm: TdxPDFForm);

    property BoundingBox: TdxRectF read GetBoundingBox;
    property ContentSquare: TdxRectF read FContentSquare;
  end;

  { TdxPDFAnnotationAppearanceBuilder<T> }

  TdxPDFAnnotationAppearanceBuilder<T: TdxPDFCustomAnnotation> = class(TInterfacedObject, IdxPDFAnnotationAppearanceBuilder) // for internal use
  strict private
    FAnnotation: T;
  protected
    procedure Rebuild(AConstructor: TdxPDFFormCommandConstructor); virtual; abstract;

    property Annotation: T read FAnnotation;
  public
    constructor Create(AAnnotation: T);
    procedure RebuildAppearance(AForm: TdxPDFForm);
  end;

  { TdxPDFMarkupAnnotationAppearanceBuilder<T> }

  TdxPDFMarkupAnnotationAppearanceBuilder<T: TdxPDFMarkupAnnotation> =
    class(TdxPDFAnnotationAppearanceBuilder<TdxPDFMarkupAnnotation>) // for internal use
  protected
    procedure Rebuild(AConstructor: TdxPDFFormCommandConstructor); override;
  end;

  { TdxPDFWidgetAnnotationAppearanceBuilder<T> }

  TdxPDFWidgetAnnotationAppearanceBuilder<T: TdxPDFInteractiveFormField> =
    class(TdxPDFAnnotationAppearanceBuilder<TdxPDFWidgetAnnotation>) // for internal use
  strict private
    FBorderWidth: Double;
    FFormField: T;
    procedure DrawStyledBorder(AConstructor: TdxPDFFormCommandConstructor; const AContentRect: TdxRectF;
      ABorderStyle: TdxPDFAnnotationBorderStyle);
  protected
    procedure Rebuild(AConstructor: TdxPDFFormCommandConstructor); override;

    function GetBackgroundColor: TdxPDFColor; virtual;
    procedure DrawBeveledBorder(AConstructor: TdxPDFFormCommandConstructor); virtual; abstract;
    procedure DrawContent(AConstructor: TdxPDFFormCommandConstructor; const AContentRect: TdxRectF); virtual; abstract;
    procedure DrawInsetBorder(AConstructor: TdxPDFFormCommandConstructor); virtual; abstract;
    procedure DrawSolidBorder(AConstructor: TdxPDFFormCommandConstructor; const AContentRect: TdxRectF); virtual; abstract;
    procedure DrawUnderlineBorder(AConstructor: TdxPDFFormCommandConstructor); virtual; abstract;
    procedure FillBackground(AConstructor: TdxPDFFormCommandConstructor); virtual; abstract;

    procedure DrawBorder(AConstructor: TdxPDFFormCommandConstructor; const AContentRect: TdxRectF);
    procedure DrawRectangularBeveledBorder(AConstructor: TdxPDFFormCommandConstructor);
    procedure DrawRectangularBorderBottomRightStroke(AConstructor: TdxPDFFormCommandConstructor);
    procedure DrawRectangularBorderStroke(AConstructor: TdxPDFFormCommandConstructor);
    procedure DrawRectangularBorderUpperLeftStroke(AConstructor: TdxPDFFormCommandConstructor);
    procedure DrawRectangularInsetBorder(AConstructor: TdxPDFFormCommandConstructor);
    procedure DrawRectangularUnderlineBorder(AConstructor: TdxPDFFormCommandConstructor);
    procedure DrawTextCombs(AConstructor: TdxPDFFormCommandConstructor; const AContentRect: TdxRectF; AMaxLen: Integer);
    procedure FillBackgroundEllipse(AConstructor: TdxPDFFormCommandConstructor; const ARect: TdxRectF; AColor: TdxPDFColor);
    procedure FillBackgroundRectangle(AConstructor: TdxPDFFormCommandConstructor; const ARect: TdxRectF; AColor: TdxPDFColor);

    property BackgroundColor: TdxPDFColor read GetBackgroundColor;
    property BorderWidth: Double read FBorderWidth;
    property FormField: T read FFormField;
  public
    constructor Create(AWidget: TdxPDFWidgetAnnotation; AFormField: T);
  end;

  { TdxPDFTextBasedFormFieldAppearanceBuilder<T> }

  TdxPDFTextBasedFormFieldAppearanceBuilder<T: TdxPDFInteractiveFormField> =
    class(TdxPDFWidgetAnnotationAppearanceBuilder<T>) // for internal use
  strict private
    FFontData: TdxPDFEditableFontData;
    FFontDataStorage: TdxPDFFontDataStorage;
    FFontSize: Single;
  protected
    procedure FillBackground(AConstructor: TdxPDFFormCommandConstructor); override;
    procedure Rebuild(AConstructor: TdxPDFFormCommandConstructor); override;

    function CalculateCenteredLineYOffset(const AClipRect: TdxRectF): Double;
    function GetTextWidth(const AText: string; AFontSize: Double): Double;
    procedure DrawTextBoxText(AConstructor: TdxPDFFormCommandConstructor; const AOffset: TdxPointF; const AText: string);
    procedure EndDrawTextBox(AConstructor: TdxPDFFormCommandConstructor);
    procedure RemoveFontFromStorage(AFont: TdxPDFCustomFont);
    procedure StartDrawTextBox(AConstructor: TdxPDFFormCommandConstructor; AForeColor: TdxPDFColor);

    property FontData: TdxPDFEditableFontData read FFontData;
    property FontSize: Single read FFontSize;
  public
    constructor Create(AWidget: TdxPDFWidgetAnnotation; AFormField: T; AState: TdxPDFDocumentState); reintroduce;
  end;

  { TdxPDFTextFieldAppearanceBuilder<T> }

  TdxPDFTextFieldAppearanceBuilder<T: TdxPDFInteractiveFormField> = class(TdxPDFTextBasedFormFieldAppearanceBuilder<T>) // for internal use
  protected
    function CreateStringFormat: TdxPDFStringFormat; virtual;
    function GetMultiline: Boolean; virtual; abstract;

    property Multiline: Boolean read GetMultiline;
  end;

  { TdxPDFTextFormFieldAppearanceBuilder }

  TdxPDFTextFormFieldAppearanceBuilder = class(TdxPDFTextFieldAppearanceBuilder<TdxPDFTextFormField>) // for internal use
  strict private
    procedure DrawTextField(AConstructor: TdxPDFFormCommandConstructor; const AContentRect: TdxRectF; const AText: string);
  protected
    function GetMultiline: Boolean; override;
    procedure DrawBeveledBorder(AConstructor: TdxPDFFormCommandConstructor); override;
    procedure DrawContent(AConstructor: TdxPDFFormCommandConstructor; const AContentRect: TdxRectF); override;
    procedure DrawInsetBorder(AConstructor: TdxPDFFormCommandConstructor); override;
    procedure DrawSolidBorder(AConstructor: TdxPDFFormCommandConstructor; const AContentRect: TdxRectF); override;
    procedure DrawUnderlineBorder(AConstructor: TdxPDFFormCommandConstructor); override;
  end;

const
  dxPDFFieldTextAlignmentMap: array[TdxPDFTextJustification] of TdxPDFStringAlignment = (saNear,
    saCenter, saFar); // for internal use

implementation

uses
  Math, dxTypeHelpers, dxStringHelper, dxCore, dxPDFCommand, dxPDFUtils, dxFontFile, dxPDFText, dxPDFCharacterMapping;

type
  TdxPDFFormAccess = class(TdxPDFForm);
  TdxPDFLineFormatter = class;

  TdxPDFStringLine = record
  strict private
    FBeginPoint: TdxPointF;
    FEndPoint: TdxPointF;
  public
    class function Create(const ABeginPoint, AEndPoint: TdxPointF): TdxPDFStringLine; static;
    property BeginPoint: TdxPointF read FBeginPoint;
    property EndPoint: TdxPointF read FEndPoint;
  end;

  { TdxPDFCustomSpacing }

  TdxPDFCustomSpacing = class
  public
    function GetValue(AFontSize: Single): Single; virtual; abstract;
  end;

  TdxPDFLineSpacing = class(TdxPDFCustomSpacing)
  strict private
    FMetrics: TdxFontFileFontMetrics;
  protected
    property Metrics: TdxFontFileFontMetrics read FMetrics;
  public
    constructor Create(const AMetrics: TdxFontFileFontMetrics);
    function GetValue(AFontSize: Single): Single; override;
  end;

  { TdxPDFMultilineWidgetLineSpacing }

  TdxPDFMultilineWidgetLineSpacing = class(TdxPDFCustomSpacing)
  strict private
    FActualSpacing: Double;
  public
    constructor Create(ADescriptor: TdxPDFFontDescriptor);
    function GetValue(AFontSize: Single): Single; override;
  end;

  { TdxPDFStringMeasurer }

  TdxPDFStringMeasurer = class
  strict private
    FEmFactor: Double;
    FFontSize: Double;
    FFormat: TdxPDFStringFormat;
    FLeadingOffset: Double;
    FLineSpacing: TdxPDFCustomSpacing;
    FMetrics: TdxFontFileFontMetrics;
    FTrailingOffset: Double;
  protected
    property EmFactor: Double read FEmFactor;
  public
    constructor Create(const AFontInfo: TdxPDFFontInfo; const AFormat: TdxPDFStringFormat); overload;
    constructor Create(const AFontInfo: TdxPDFFontInfo; const AFormat: TdxPDFStringFormat;
      ALineSpacing: TdxPDFCustomSpacing); overload;

    function MeasureWidth(ALine: TdxPDFGlyphRun): Double; virtual;

    function MeasureStringWidth(ALine: TdxPDFGlyphRun): Double;
    function MeasureStringHeight(ALineCount: Integer): Double;

    property Format: TdxPDFStringFormat read FFormat;
    property LeadingOffset: Double read FLeadingOffset;
    property TrailingOffset: Double read FTrailingOffset;
  end;

  { TdxPDFWidgetStringMeasurer }

  TdxPDFWidgetStringMeasurer = class(TdxPDFStringMeasurer)
  strict private
    FCharSpacing: Double;
    FHorizontalScaling: Double;
    FSpaceGlyph: TdxPDFGlyph;
    FWordSpacing: Double;
  public
    constructor Create(const AFontInfo: TdxPDFFontInfo; const AFormat: TdxPDFStringFormat;
      ATextState: TdxPDFInteractiveFormFieldTextState; ASpaceGlyphRun: TdxPDFGlyphRun; ALineSpacing: TdxPDFCustomSpacing);
    function MeasureWidth(ALine: TdxPDFGlyphRun): Double; override;
  end;

  { TdxPDFStringPaintingStrategy }

  TdxPDFStringPaintingStrategy = class
  strict private
    FConstructor: TdxPDFCommandConstructor;
    FMeasurer: TdxPDFStringMeasurer;
  protected
    property CommandConstructor: TdxPDFCommandConstructor read FConstructor;
  public
    constructor Create(AConstructor: TdxPDFCommandConstructor; AMeasurer: TdxPDFStringMeasurer);

    function GetFirstLineVerticalPosition(ALineCount: Integer): Double; virtual; abstract;
    function GetHorizontalPosition(ALine: TdxPDFGlyphRun): Double; virtual; abstract;
    procedure Clip; virtual; abstract;
    procedure ShowText(ALine: TdxPDFGlyphRun; AUseKerning: Boolean); virtual;

    property Measurer: TdxPDFStringMeasurer read FMeasurer;
  end;

  { TdxPDFStringPaintingInsideRectStrategy }

  TdxPDFStringPaintingInsideRectStrategy = class(TdxPDFStringPaintingStrategy)
  strict private
    FClip: Boolean;
    FRect: TdxRectF;
  public
    constructor Create(AConstructor: TdxPDFCommandConstructor; AMeasurer: TdxPDFStringMeasurer; const ARect: TdxRectF);
    function GetFirstLineVerticalPosition(ALineCount: Integer): Double; override;
    function GetHorizontalPosition(ALine: TdxPDFGlyphRun): Double; override;
    procedure Clip; override;
  end;

  { TdxPDFStringPaintingAtPointStrategy }

  TdxPDFStringPaintingAtPointStrategy = class(TdxPDFStringPaintingStrategy)
  strict private
    FLocation: TdxPointF;
  public
    constructor Create(AConstructor: TdxPDFCommandConstructor; AMeasurer: TdxPDFStringMeasurer; const ALocation: TdxPointF);

    function GetFirstLineVerticalPosition(ALineCount: Integer): Double; override;
    function GetHorizontalPosition(ALine: TdxPDFGlyphRun): Double; override;
    procedure Clip; override;
  end;

  { TdxPDFStringPainter }

  TdxPDFStringPainter = class
  strict private
    FConstructor: TdxPDFCommandConstructor;
  protected
    function CreateMeasurer(const AFontInfo: TdxPDFFontInfo; const AFormat: TdxPDFStringFormat): TdxPDFStringMeasurer; virtual;
    function GetLineSpacing(const AMetrics: TdxFontFileFontMetrics): TdxPDFCustomSpacing; virtual;
    procedure BeginTextDrawing; virtual;
    procedure EndTextDrawing; virtual;
  public
    constructor Create(AConstructor: TdxPDFCommandConstructor);

    procedure DrawLines(ALines: TdxPDFGlyphRunList; const AFontInfo: TdxPDFFontInfo;
      const ALayoutRect: TdxRectF; const AFormat: TdxPDFStringFormat; AShowWithGlyphOffsets: Boolean); overload;
    procedure DrawLines(ALines: TdxPDFGlyphRunList; const AFontInfo: TdxPDFFontInfo;
      const ALocation: TdxPointF; const AFormat: TdxPDFStringFormat; AShowWithGlyphOffsets: Boolean); overload;
    procedure DrawLines(ALines: TdxPDFGlyphRunList; const AFontInfo: TdxPDFFontInfo;
      AStrategy: TdxPDFStringPaintingStrategy; AShowWithGlyphOffsets: Boolean); overload;
  end;

  { TdxPDFWidgetStringPainter }

  TdxPDFWidgetStringPainter = class(TdxPDFStringPainter)
  strict private
    FLineSpacing: TdxPDFCustomSpacing;
    FSpaceGlyphRun: TdxPDFGlyphRun;
    FState: TdxPDFInteractiveFormFieldTextState;
  protected
    function CreateMeasurer(const AFontInfo: TdxPDFFontInfo; const AFormat: TdxPDFStringFormat): TdxPDFStringMeasurer; override;
    function GetLineSpacing(const AMetrics: TdxFontFileFontMetrics): TdxPDFCustomSpacing; override;
    procedure BeginTextDrawing; override;
    procedure EndTextDrawing; override;
  public
    constructor Create(AConstructor: TdxPDFCommandConstructor; ASpaceGlyphRun: TdxPDFGlyphRun; ALineSpacing: TdxPDFCustomSpacing);
    destructor Destroy; override;

    procedure UpdateSpacing(AState: TdxPDFInteractiveFormFieldTextState);
  end;

  { TdxPDFLineTrimmingAlgorithm }

  TdxPDFLineTrimmingAlgorithm = class abstract
  strict private
    FEllipsis: TdxPDFGlyphRun;
    FEllipsisPosition: Integer;
    FFormatter: TdxPDFLineFormatter;
  protected
    function GetUseEllipsis: Boolean; virtual;
    procedure TryInsertEllipsis;
    procedure SaveEllipsisPosition;

    property Ellipsis: TdxPDFGlyphRun read FEllipsis;
    property Formatter: TdxPDFLineFormatter read FFormatter;
    property UseEllipsis: Boolean read GetUseEllipsis;
  public
    constructor Create(AFormatter: TdxPDFLineFormatter; AEllipsis: TdxPDFGlyphRun);

    class function CreateAlgorithm(ATrimming: TdxPDFStringTrimming; AFormatter: TdxPDFLineFormatter; AEllipsis: TdxPDFGlyphRun): TdxPDFLineTrimmingAlgorithm; static;

    function ProcessWord(AWord: TdxPDFGlyphRun): Boolean; virtual; abstract;
  end;

  { TdxPDFLineTrimmingCharAlgorithm }

  TdxPDFLineTrimmingCharAlgorithm = class(TdxPDFLineTrimmingAlgorithm)
  public
    function ProcessWord(AWord: TdxPDFGlyphRun): Boolean; override;
  end;

  { TdxPDFLineTrimmingWordAlgorithm }

  TdxPDFLineTrimmingWordAlgorithm = class(TdxPDFLineTrimmingCharAlgorithm)
  public
    function ProcessWord(AWord: TdxPDFGlyphRun): Boolean; override;
  end;

  { TdxPDFLineTrimmingEllipsisCharAlgorithm }

  TdxPDFLineTrimmingEllipsisCharAlgorithm = class(TdxPDFLineTrimmingCharAlgorithm)
  protected
    function GetUseEllipsis: Boolean; override;
  end;

  { TdxPDFLineTrimmingEllipsisWordAlgorithm }

  TdxPDFLineTrimmingEllipsisWordAlgorithm = class(TdxPDFLineTrimmingWordAlgorithm)
  protected
    function GetUseEllipsis: Boolean; override;
  end;

  { TdxPDFFormatterWord }

  TdxPDFFormatterWord = record
  strict private
    FEndsWithSoftHyphen: Boolean;
    FGlyphs: TdxPDFGlyphRun;
    FText: string;
    function GetWidth: Double;
    function GetTextWithoutLeadingSpaces: string;
  public
    class function Create(const AText: string; AGlyphs: TdxPDFGlyphRun; AEndsWithSoftHyphen: Boolean): TdxPDFFormatterWord; static;

    property EndsWithSoftHyphen: Boolean read FEndsWithSoftHyphen;
    property Glyphs: TdxPDFGlyphRun read FGlyphs;
    property TextWithoutLeadingSpaces: string read GetTextWithoutLeadingSpaces;
    property Width: Double read GetWidth;
  end;

  { TdxPDFLineFormatter }

  TdxPDFLineFormatter = class
  strict private
    FCurrentLine: TdxPDFGlyphRun;
    FEmTabStopInterval: Double;
    FFlags: TdxPDFGlyphMappingFlags;
    FFontData: TdxPDFEditableFontData;
    FHyphenRun: TdxPDFGlyphRun;
    FLayoutLineCount: Integer;
    FLayoutWidth: Double;
    FLines: TdxPDFGlyphRunList;
    FNonBreakingSpace: Char;
    FNoWrap: Boolean;
    FShouldAppendHyphen: Boolean;
    FSoftHyphenChar: Char;
    FSoftHyphenString: string;
    FTrimmingAlgorithm: TdxPDFLineTrimmingAlgorithm;

    class function IsBeginWordSymbol(C: Char): Boolean; static;
    class function IsEndWordSymbol(C: Char): Boolean; static;
    class function CanBreak(C: Char): Boolean; overload; static;
    class function CanBreak(APrevious: Char; ANext: Char): Boolean; overload; static;

    function GetHyphenRun: TdxPDFGlyphRun;
    function GetIsCurrentLineEmpty: Boolean;
    function GetCurrentLineGlyphCount: Integer;
    function GetCurrentLineWidth: Double;
    function GetLines: TdxPDFGlyphRunList;
  protected
    function GetWordGlyphs(AActualWord: TdxPDFGlyphRun): TdxPDFGlyphList; virtual;

    procedure EnsureCurrentLine;
    function MapString(const AStr: string): TdxPDFFormatterWord;
    function AppendWord(AActualWord: TdxPDFFormatterWord): Boolean;
    procedure FinishLine(AIgnoreHyphen: Boolean);
    procedure ApplyTabStops(AWord: TdxPDFFormatterWord; ATabCount: Integer);

    property HyphenRun: TdxPDFGlyphRun read GetHyphenRun;
    property CurrentLine: TdxPDFGlyphRun read FCurrentLine;
  public
    constructor Create(ALayoutWidth: Double; ALayoutLineCount: Integer; const AFormat: TdxPDFStringFormat;
      AEllipsis: TdxPDFGlyphRun; AFontData: TdxPDFEditableFontData; AEmTabStopInterval: Double);
    destructor Destroy; override;

    procedure AddGlyph(const AGlyph: TdxPDFGlyph); virtual;
    procedure AddWord(const AWord: TdxPDFGlyphRun); virtual;
    procedure RemoveLastGlyph;
    procedure FormatLine(const ALine: string; AFlags: TdxPDFGlyphMappingFlags);
    procedure Clear;

    property CurrentLineGlyphCount: Integer read GetCurrentLineGlyphCount;
    property CurrentLineWidth: Double read GetCurrentLineWidth;
    property IsCurrentLineEmpty: Boolean read GetIsCurrentLineEmpty;
    property LayoutWidth: Double read FLayoutWidth;
    property Lines: TdxPDFGlyphRunList read GetLines;
  end;

  { TdxPDFRTLLineFormatter }

  TdxPDFRTLLineFormatter = class(TdxPDFLineFormatter)
  strict private
    FSpaceGlyphIndex: Integer;
  protected
    function GetWordGlyphs(AActualWord: TdxPDFGlyphRun): TdxPDFGlyphList; override;
  public
    constructor Create(ALayoutWidth: Double; ASpaceGlyphIndex, ALayoutLineCount: Integer;
      const AFormat: TdxPDFStringFormat; AEllipsis: TdxPDFGlyphRun; AFontData: TdxPDFEditableFontData; AEmTabStopInterval: Double);

    procedure AddGlyph(const AGlyph: TdxPDFGlyph); override;
    procedure AddWord(const AWord: TdxPDFGlyphRun); override;
  end;

  { TdxPDFStringFormatter }

  TdxPDFStringFormatter = class
  strict private
    FEllipsis: TdxPDFGlyphRun;
    FFontData: TdxPDFEditableFontData;
    FFontSize: Double;
    FLineAppearanceFactor: Double;
    FLineSpacing: TdxPDFCustomSpacing;
    FShowWithGlyphOffsets: Boolean;
    FSpaceGlyphIndex: Integer;
    class function ProcessHotkeyPrefixes(const AText: string; APrefix: TdxPDFHotkeyPrefix): string; static;
  protected
    function ShouldApplyKerning(ALinesCount: Integer): Boolean; virtual;
  public
    constructor Create(const AFontInfo: TdxPDFFontInfo); overload;
    constructor Create(const AFontInfo: TdxPDFFontInfo; ALineSpacing: TdxPDFCustomSpacing); overload;
    destructor Destroy; override;

    function FormatString(const AStr: string; const APoint: TdxPointF; const AFormat: TdxPDFStringFormat;
      AUseKerning: Boolean; AEmTabStopInterval: Double): TdxPDFGlyphRunList; overload;
    function FormatString(const AStr: string; const ALayoutRect: TdxRectF; const AFormat: TdxPDFStringFormat;
      AUseKerning: Boolean; AEmTabStopInterval: Double): TdxPDFGlyphRunList; overload;
    function FormatString(const AText: TArray<string>; AFormatter: TdxPDFLineFormatter; ALimitedLines: Boolean;
      ALineCount: Integer; const AFormat: TdxPDFStringFormat; AUseKerning: Boolean): TdxPDFGlyphRunList; overload;

    property ShowWithGlyphOffsets: Boolean read FShowWithGlyphOffsets;
  end;

  { TdxPDFTabbedStringFormatter }

  TdxPDFTabbedStringFormatter = class
  strict private
    FFontData: TdxPDFEditableFontData;
    FEmTabStop: Double;
    FShouldUseKerning: Boolean;
    FGlyphRun: TdxPDFGlyphRun;
    FRightToLeft: Boolean;
    procedure AppendGlyphRun(const ATabPiece: string; ATabCount: Integer);
    function MapString(const ALine: string): TdxPDFGlyphRun;
  public
    constructor Create(AFontData: TdxPDFEditableFontData; AEmTabStopInterval: Double;
      ARightToLeft: Boolean; AShouldUseKerning: Boolean);
    function FormatString(const ALine: string): TdxPDFGlyphRun;
  end;


function HasFlag(ASourceFlags, AFlags: TdxPDFStringFormatFlags): Boolean; overload;
begin
  Result := (Integer(ASourceFlags) and Integer(AFlags)) <> 0;
end;

function HasFlag(ASourceFlags, AFlags: TdxPDFInteractiveFormFieldFlags): Boolean; overload;
begin
  Result := (Integer(ASourceFlags) and Integer(AFlags)) = 0;
end;

{ TdxPDFStringLine }

class function TdxPDFStringLine.Create(const ABeginPoint, AEndPoint: TdxPointF): TdxPDFStringLine;
begin
  Result.FBeginPoint := ABeginPoint;
  Result.FEndPoint := AEndPoint;
end;

{ TdxPDFLineSpacing }

constructor TdxPDFLineSpacing.Create(const AMetrics: TdxFontFileFontMetrics);
begin
  inherited Create;
  FMetrics := AMetrics;
end;

function TdxPDFLineSpacing.GetValue(AFontSize: Single): Single;
begin
  Result := FMetrics.GetLineSpacing(AFontSize);
end;

{ TdxPDFMultilineWidgetLineSpacing }

constructor TdxPDFMultilineWidgetLineSpacing.Create(ADescriptor: TdxPDFFontDescriptor);
begin
  inherited Create;
  FActualSpacing := ADescriptor.FontBBox.Height / 1000;
end;

function TdxPDFMultilineWidgetLineSpacing.GetValue(AFontSize: Single): Single;
begin
  Result := FActualSpacing * AFontSize;
end;

{ TdxPDFStringFormat }

class function TdxPDFStringFormat.Create(AFormat: TdxPDFStringFormat): TdxPDFStringFormat;
begin
  Result.FLeadingMarginFactor := 1.0 / 6.0;
  Result.FTrailingMarginFactor := 1.0 / 6.0;
  Result.FAlignment := saNear;
  Result.FLineAlignment := saNear;
  Result.FTrimming := stCharacter;
  Result.FHotkeyPrefix := hpNone;
  Result.FFormatFlags := AFormat.FormatFlags;
  Result.FAlignment := AFormat.Alignment;
  Result.FLineAlignment := AFormat.LineAlignment;
  Result.FTrimming := AFormat.Trimming;
  Result.FHotkeyPrefix := AFormat.HotkeyPrefix;
  Result.FLeadingMarginFactor := AFormat.LeadingMarginFactor;
  Result.FTrailingMarginFactor := AFormat.TrailingMarginFactor;
  Result.FTabStopInterval := AFormat.TabStopInterval;
  Result.FDirectionRightToLeft := AFormat.DirectionRightToLeft;
end;

class function TdxPDFStringFormat.CreateGenericDefault: TdxPDFStringFormat;
begin
  Result := TdxPDFStringFormat.CreateGenericTypographic;
end;

class function TdxPDFStringFormat.CreateGenericTypographic: TdxPDFStringFormat;
begin
  Result := TdxPDFStringFormat.Create(TdxPDFStringFormatFlags(Integer(sffLineLimit) or Integer(sffNoClip)));
  Result.FTrimming := stNone;
  Result.FLeadingMarginFactor := 0;
  Result.FTrailingMarginFactor := 0;
end;

class function TdxPDFStringFormat.Create(AFormatFlags: TdxPDFStringFormatFlags): TdxPDFStringFormat;
begin
  Result.FLeadingMarginFactor := 1.0 / 6.0;
  Result.FTrailingMarginFactor := 1.0 / 6.0;
  Result.FAlignment := saNear;
  Result.FLineAlignment := saNear;
  Result.FTrimming := stCharacter;
  Result.FHotkeyPrefix := hpNone;
  Result.FFormatFlags := AFormatFlags;
end;

class function TdxPDFStringFormat.Create: TdxPDFStringFormat;
begin
  Result.FLeadingMarginFactor := 1.0 / 6.0;
  Result.FTrailingMarginFactor := 1.0 / 6.0;
  Result.FAlignment := saNear;
  Result.FLineAlignment := saNear;
  Result.FTrimming := stCharacter;
  Result.FHotkeyPrefix := hpNone;
end;

function TdxPDFStringFormat.Clone: TdxPDFStringFormat;
begin
  Result := TdxPDFStringFormat.Create(Self);
end;

{ TdxPDFStringMeasurer }

constructor TdxPDFStringMeasurer.Create(const AFontInfo: TdxPDFFontInfo; const AFormat: TdxPDFStringFormat);
begin
  Create(AFontInfo, AFormat, TdxPDFLineSpacing.Create((AFontInfo.FontData as TdxPDFEditableFontData).Metrics));
end;

constructor TdxPDFStringMeasurer.Create(const AFontInfo: TdxPDFFontInfo; const AFormat: TdxPDFStringFormat;
  ALineSpacing: TdxPDFCustomSpacing);
begin
  inherited Create;
  FMetrics := (AFontInfo.FontData as TdxPDFEditableFontData).Metrics;
  FFontSize := AFontInfo.FontSize;
  FLineSpacing := ALineSpacing;
  FLeadingOffset := AFormat.LeadingMarginFactor * FFontSize;
  FTrailingOffset := AFormat.TrailingMarginFactor * FFontSize;
  FFormat := TdxPDFStringFormat.Create(AFormat);
  FEmFactor := FFontSize / 1000;
end;

function TdxPDFStringMeasurer.MeasureWidth(ALine: TdxPDFGlyphRun): Double;
begin
  Result := ALine.Width * FEmFactor;
end;

function TdxPDFStringMeasurer.MeasureStringWidth(ALine: TdxPDFGlyphRun): Double;
begin
  Result := MeasureWidth(ALine);
  if (FLeadingOffset <> 0) or (FTrailingOffset <> 0) then
    Result := Result * 1.03 + FLeadingOffset + FTrailingOffset
end;

function TdxPDFStringMeasurer.MeasureStringHeight(ALineCount: Integer): Double;
var
  ALineSpacing: Double;
begin
  Result := 0;
  if ALineCount > 0 then
  begin
    ALineSpacing := FLineSpacing.GetValue(FFontSize);
    if ALineCount = 1 then
      Result := FMetrics.GetAscent(FFontSize) + FMetrics.GetDescent(FFontSize)
    else
      Result := ALineCount * ALineSpacing;
    Result :=  Result + FTrailingOffset * 3 / 4;
  end;
end;

{ TdxPDFWidgetStringMeasurer }

constructor TdxPDFWidgetStringMeasurer.Create(const AFontInfo: TdxPDFFontInfo; const AFormat: TdxPDFStringFormat;
  ATextState: TdxPDFInteractiveFormFieldTextState; ASpaceGlyphRun: TdxPDFGlyphRun; ALineSpacing: TdxPDFCustomSpacing);
begin
  inherited Create(AFontInfo, AFormat, ALineSpacing);
  FHorizontalScaling := 1;
  if ATextState <> nil then
  begin
    FCharSpacing := ATextState.CharacterSpacing;
    FWordSpacing := ATextState.WordSpacing;
    FHorizontalScaling := ATextState.HorizontalScaling / 100;
  end;
  if ASpaceGlyphRun.Empty then
    FSpaceGlyph := TdxPDFGlyph.Create(0, 0, 0)
  else
    FSpaceGlyph := ASpaceGlyphRun.Glyphs[0];
end;

function TdxPDFWidgetStringMeasurer.MeasureWidth(ALine: TdxPDFGlyphRun): Double;
var
  AGlyphWidth: Double;
  AGlyph: TdxPDFGlyph;
begin
  Result := 0;
  for AGlyph in ALine.Glyphs do
  begin
    AGlyphWidth := AGlyph.ActualWidth * EmFactor * FHorizontalScaling;
    AGlyphWidth := IfThen(AGlyphWidth > 0, AGlyphWidth + FCharSpacing, 0);
    if AGlyph.Index = FSpaceGlyph.Index then
      AGlyphWidth := AGlyphWidth + FWordSpacing;
    Result := Result + AGlyphWidth;
  end;
end;

{ TdxPDFStringPaintingStrategy }

constructor TdxPDFStringPaintingStrategy.Create(AConstructor: TdxPDFCommandConstructor; AMeasurer: TdxPDFStringMeasurer);
begin
  inherited Create;
  FConstructor := AConstructor;
  FMeasurer := AMeasurer;
end;

procedure TdxPDFStringPaintingStrategy.ShowText(ALine: TdxPDFGlyphRun; AUseKerning: Boolean);
var
  AOffsets: TDoubleDynArray;
begin
  if AUseKerning then
    AOffsets := ALine.GlyphOffsets
  else
    SetLength(AOffsets, 0);
  FConstructor.ShowText(ALine.TextData, AOffsets);
end;

{ TdxPDFStringPaintingInsideRectStrategy }

constructor TdxPDFStringPaintingInsideRectStrategy.Create(AConstructor: TdxPDFCommandConstructor;
  AMeasurer: TdxPDFStringMeasurer; const ARect: TdxRectF);
begin
  inherited Create(AConstructor, AMeasurer);
  FRect := ARect;
  FClip := not (Integer(AMeasurer.Format.FormatFlags) and Integer(sffNoClip) <> 0)
end;

function TdxPDFStringPaintingInsideRectStrategy.GetFirstLineVerticalPosition(ALineCount: Integer): Double;
begin
  case Measurer.Format.LineAlignment of
    saCenter:
      Result := FRect.Bottom - (FRect.Height - Measurer.MeasureStringHeight(ALineCount)) / 2;
    saFar:
      Result := FRect.Bottom - (FRect.Height - Measurer.MeasureStringHeight(ALineCount));
  else
    Result := FRect.Bottom;
  end;
end;

function TdxPDFStringPaintingInsideRectStrategy.GetHorizontalPosition(ALine: TdxPDFGlyphRun): Double;
begin
  case Measurer.Format.Alignment of
    saCenter:
      Result := FRect.Left + (FRect.Width - Measurer.MeasureWidth(ALine)) / 2;
    saFar:
      Result := FRect.Right - Measurer.MeasureWidth(ALine) - Measurer.TrailingOffset;
  else
    Result := FRect.Left + Measurer.LeadingOffset;
  end;
end;

procedure TdxPDFStringPaintingInsideRectStrategy.Clip;
begin
  if FClip then
    CommandConstructor.IntersectClip(FRect);
end;

{ TdxPDFStringPaintingAtPointStrategy }

constructor TdxPDFStringPaintingAtPointStrategy.Create(AConstructor: TdxPDFCommandConstructor;
  AMeasurer: TdxPDFStringMeasurer; const ALocation: TdxPointF);
begin
  inherited Create(AConstructor, AMeasurer);
  FLocation := ALocation;
end;

function TdxPDFStringPaintingAtPointStrategy.GetFirstLineVerticalPosition(ALineCount: Integer): Double;
begin
  case Measurer.Format.LineAlignment of
    saCenter:
      Result := FLocation.Y + Measurer.MeasureStringHeight(ALineCount) / 2;
    saFar:
      Result := FLocation.Y + Measurer.MeasureStringHeight(ALineCount);
  else
    Result := FLocation.Y;
  end;
end;

function TdxPDFStringPaintingAtPointStrategy.GetHorizontalPosition(ALine: TdxPDFGlyphRun): Double;
begin
  case Measurer.Format.Alignment of
    saCenter:
      Result := FLocation.X - Measurer.MeasureWidth(ALine) / 2;
    saFar:
      Result := FLocation.X - Measurer.MeasureWidth(ALine) - Measurer.TrailingOffset;
  else
    Result := FLocation.X + Measurer.LeadingOffset;
  end;
end;

procedure TdxPDFStringPaintingAtPointStrategy.Clip;
begin
// do nothing
end;

{ TdxPDFStringPainter }

constructor TdxPDFStringPainter.Create(AConstructor: TdxPDFCommandConstructor);
begin
  inherited Create;
  FConstructor := AConstructor;
end;

procedure TdxPDFStringPainter.DrawLines(ALines: TdxPDFGlyphRunList; const AFontInfo: TdxPDFFontInfo;
  const ALayoutRect: TdxRectF; const AFormat: TdxPDFStringFormat; AShowWithGlyphOffsets: Boolean);
var
  AMeasurer: TdxPDFStringMeasurer;
  AStrategy: TdxPDFStringPaintingInsideRectStrategy;
begin
  AMeasurer := CreateMeasurer(AFontInfo, AFormat);
  try
    AStrategy := TdxPDFStringPaintingInsideRectStrategy.Create(FConstructor, AMeasurer, ALayoutRect);
    try
      DrawLines(ALines, AFontInfo, AStrategy, AShowWithGlyphOffsets);
    finally
      AStrategy.Free;
    end;
  finally
    AMeasurer.Free;
  end;
end;

procedure TdxPDFStringPainter.DrawLines(ALines: TdxPDFGlyphRunList; const AFontInfo: TdxPDFFontInfo;
  const ALocation: TdxPointF; const AFormat: TdxPDFStringFormat; AShowWithGlyphOffsets: Boolean);
var
  AMeasurer: TdxPDFStringMeasurer;
  AStrategy: TdxPDFStringPaintingAtPointStrategy;
begin
  AMeasurer := CreateMeasurer(AFontInfo, AFormat);
  try
    AStrategy := TdxPDFStringPaintingAtPointStrategy.Create(FConstructor, AMeasurer, ALocation);
    try
      DrawLines(ALines, AFontInfo, AStrategy, AShowWithGlyphOffsets);
    finally
      AStrategy.Free;
    end;
  finally
    AMeasurer.Free;
  end;
end;

procedure TdxPDFStringPainter.DrawLines(ALines: TdxPDFGlyphRunList; const AFontInfo: TdxPDFFontInfo;
  AStrategy: TdxPDFStringPaintingStrategy; AShowWithGlyphOffsets: Boolean);
var
  ABaselineY, AUnderlineY, AStrikeoutY: Double;
  ACount, I: Integer;
  AFontData: TdxPDFEditableFontData;
  AFontSize, ALineSpacing, AAscent, AFirstLineY, ABoldLineWidth, APreviousXOffset, X, ADescent, ARight: Double;
  ALine: TdxPDFGlyphRun;
  AOblique, AEmulateBold, AShouldDrawLines: Boolean;
  AStringLine: TdxPDFStringLine;
  AStringLines: TList<TdxPDFStringLine>;
begin
  ACount := 0;
  if ALines <> nil then
    ACount := ALines.Count;
  if ACount > 0 then
  begin
    AFontData := AFontInfo.FontData as TdxPDFEditableFontData;
    AOblique := AFontData.NeedEmulateItalic;
    AFontSize := AFontInfo.FontSize;
    BeginTextDrawing;
    ALineSpacing := GetLineSpacing(AFontData.Metrics).GetValue(AFontSize);
    AAscent := AFontData.Metrics.GetAscent(AFontSize);
    AFirstLineY := AStrategy.GetFirstLineVerticalPosition(ACount) - AAscent;
    AStrategy.Clip;
    AEmulateBold := AFontData.NeedEmulateBold;
    FConstructor.BeginText;
    FConstructor.SetTextFont(AFontData.Font, AFontSize);
    ABoldLineWidth := AFontSize / 30;
    if AEmulateBold then
    begin
      FConstructor.SetLineWidth(ABoldLineWidth);
      FConstructor.SetTextRenderingMode(trmFillAndStroke);
    end;
    APreviousXOffset := 0;
    AStringLines := TList<TdxPDFStringLine>.Create;
    try
      AShouldDrawLines := AFontData.Underline or AFontData.Strikeout;
      for I := 0 to ACount - 1 do
      begin
        ALine := ALines[I];
        X := AStrategy.GetHorizontalPosition(ALine);
        if AOblique then
          FConstructor.SetObliqueTextMatrix(X, AFirstLineY - ALineSpacing * I)
        else
          FConstructor.StartTextLineWithOffsets(X - APreviousXOffset, IfThen(I = 0, AFirstLineY, -ALineSpacing));
        if not ALine.Empty then
        begin
          AStrategy.ShowText(ALine, AShowWithGlyphOffsets);
          if AShouldDrawLines then
          begin
            ADescent := AFontData.Metrics.GetDescent(AFontSize);
            ARight := X + AStrategy.Measurer.MeasureWidth(ALine);
            ABaselineY := AFirstLineY - ALineSpacing * I;
            if AFontData.Underline then
            begin
              AUnderlineY := ABaselineY - ADescent / 2;
              AStringLines.Add(TdxPDFStringLine.Create(TdxPointF.Create(X, AUnderlineY), TdxPointF.Create(ARight, AUnderlineY)));
            end;
            if AFontData.Strikeout then
            begin
              AStrikeoutY := ABaselineY + AAscent / 2 - ADescent;
              AStringLines.Add(TdxPDFStringLine.Create(TdxPointF.Create(X, AStrikeoutY), TdxPointF.Create(ARight, AStrikeoutY)));
            end;
          end;
        end;
        APreviousXOffset := IfThen(AOblique, 0, X);
      end;
      FConstructor.EndText;
      if AStringLines.Count <> 0 then
      begin
        FConstructor.SetLineWidth(AFontInfo.FontLineSize);
        for AStringLine in AStringLines do
          FConstructor.DrawLine(AStringLine.BeginPoint, AStringLine.EndPoint);
      end;
    finally
      AStringLines.Free;
    end;
    EndTextDrawing;
  end;
end;

function TdxPDFStringPainter.CreateMeasurer(const AFontInfo: TdxPDFFontInfo;
  const AFormat: TdxPDFStringFormat): TdxPDFStringMeasurer;
begin
  Result := TdxPDFStringMeasurer.Create(AFontInfo, AFormat);
end;

function TdxPDFStringPainter.GetLineSpacing(const AMetrics: TdxFontFileFontMetrics): TdxPDFCustomSpacing;
begin
  Result := TdxPDFLineSpacing.Create(AMetrics);
end;

procedure TdxPDFStringPainter.BeginTextDrawing;
begin
  FConstructor.SaveGraphicsState;
end;

procedure TdxPDFStringPainter.EndTextDrawing;
begin
  FConstructor.RestoreGraphicsState;
end;

{ TdxPDFWidgetStringPainter }

constructor TdxPDFWidgetStringPainter.Create(AConstructor: TdxPDFCommandConstructor; ASpaceGlyphRun: TdxPDFGlyphRun;
  ALineSpacing: TdxPDFCustomSpacing);
begin
  inherited Create(AConstructor);
  FLineSpacing := ALineSpacing;
  FSpaceGlyphRun := ASpaceGlyphRun;
end;

destructor TdxPDFWidgetStringPainter.Destroy;
begin
  FreeAndNil(FSpaceGlyphRun);
  inherited Destroy;
end;

procedure TdxPDFWidgetStringPainter.UpdateSpacing(AState: TdxPDFInteractiveFormFieldTextState);
begin
  FState := AState;
end;

function TdxPDFWidgetStringPainter.CreateMeasurer(const AFontInfo: TdxPDFFontInfo;
  const AFormat: TdxPDFStringFormat): TdxPDFStringMeasurer;
begin
  Result := TdxPDFWidgetStringMeasurer.Create(AFontInfo, AFormat, FState, FSpaceGlyphRun, FLineSpacing);
end;

function TdxPDFWidgetStringPainter.GetLineSpacing(const AMetrics: TdxFontFileFontMetrics): TdxPDFCustomSpacing;
begin
  Result := FLineSpacing;
end;

procedure TdxPDFWidgetStringPainter.BeginTextDrawing;
begin
// do nothing
end;

procedure TdxPDFWidgetStringPainter.EndTextDrawing;
begin
// do nothing
end;

{ TdxPDFStringFormatter }

constructor TdxPDFStringFormatter.Create(const AFontInfo: TdxPDFFontInfo);
begin
  Create(AFontInfo, TdxPDFLineSpacing.Create((AFontInfo.FontData as TdxPDFEditableFontData).Metrics));
end;

constructor TdxPDFStringFormatter.Create(const AFontInfo: TdxPDFFontInfo; ALineSpacing: TdxPDFCustomSpacing);
var
  ASpaceGlyphRun: TdxPDFGlyphRun;
begin
  inherited Create;
  FLineAppearanceFactor := 0.25;
  FFontData := AFontInfo.FontData as TdxPDFEditableFontData;
  FFontSize := AFontInfo.FontSize;
  FLineSpacing := ALineSpacing;
  FEllipsis := FFontData.ProcessString('...', mfNone);
  ASpaceGlyphRun := FFontData.ProcessString(' ', mfNone);
  try
    if (ASpaceGlyphRun.Glyphs = nil) or (ASpaceGlyphRun.Glyphs.Count = 0) then
      FSpaceGlyphIndex := 32
    else
      FSpaceGlyphIndex := ASpaceGlyphRun.Glyphs[0].Index;
  finally
    ASpaceGlyphRun.Free;
  end;
end;

destructor TdxPDFStringFormatter.Destroy;
begin
  FreeAndNil(FEllipsis);
  inherited Destroy;
end;

class function TdxPDFStringFormatter.ProcessHotkeyPrefixes(const AText: string; APrefix: TdxPDFHotkeyPrefix): string;
var
  ABuilder: TStringBuilder;
  ACh: Char;
  AInPrefix: Boolean;
begin
  Result := AText;
  if APrefix <> hpNone then
  begin
    AInPrefix := False;
    ABuilder := TStringBuilder.Create;
    try
      for ACh in AText do
        if ACh = '&' then
          if AInPrefix then
          begin
            AInPrefix := False;
            ABuilder.Append(ACh);
          end
          else
            AInPrefix := True
        else
          ABuilder.Append(ACh);
      Result := ABuilder.ToString;
    finally
      ABuilder.Free;
    end;
  end;
end;

function TdxPDFStringFormatter.FormatString(const AStr: string; const APoint: TdxPointF;
  const AFormat: TdxPDFStringFormat; AUseKerning: Boolean; AEmTabStopInterval: Double): TdxPDFGlyphRunList;
var
  ALine: string;
  AFormatter: TdxPDFTabbedStringFormatter;
begin
  Result := TdxPDFGlyphRunList.Create;
  FShowWithGlyphOffsets := AUseKerning or (AEmTabStopInterval <> 0);
  AFormatter := TdxPDFTabbedStringFormatter.Create(FFontData, AEmTabStopInterval, AFormat.DirectionRightToLeft, AUseKerning);
  try
    for ALine in TdxPDFUtils.Split(ProcessHotkeyPrefixes(AStr, AFormat.HotkeyPrefix), [#10#13]) do
      Result.Add(AFormatter.FormatString(ALine));
  finally
    AFormatter.Free;
  end;
end;

function TdxPDFStringFormatter.FormatString(const AStr: string; const ALayoutRect: TdxRectF;
  const AFormat: TdxPDFStringFormat; AUseKerning: Boolean; AEmTabStopInterval: Double): TdxPDFGlyphRunList;

  procedure CalculateLineCount(out ALayoutWidth: Double; out ALineCount: Integer; out ALimitedLines: Boolean);
  var
    ALineSpacing, AFirstLineHeight, ALayoutHeight, APartialLineCount: Double;
  begin
    ALimitedLines := HasFlag(AFormat.FormatFlags, sffLineLimit);
    ALineSpacing := FLineSpacing.GetValue(FFontSize);
    AFirstLineHeight := FFontData.Metrics.GetAscent(FFontSize) + FFontData.Metrics.GetDescent(FFontSize);
    ALayoutHeight := ALayoutRect.Height;
    if ALayoutHeight < AFirstLineHeight then
      ALineCount := IfThen(not ALimitedLines and (ALayoutHeight >= ALineSpacing * FLineAppearanceFactor), 1, 0)
    else
    begin
      APartialLineCount := (ALayoutHeight - AFirstLineHeight) / ALineSpacing;
      ALineCount := Floor(APartialLineCount);
      if not ALimitedLines and ((APartialLineCount - ALineCount) >= FLineAppearanceFactor) then
        Inc(ALineCount);
      Inc(ALineCount);
    end;
    ALimitedLines := ALimitedLines or (AFormat.Trimming <> stNone);
    ALayoutWidth := Max(0, (ALayoutRect.Width - (AFormat.TrailingMarginFactor + AFormat.LeadingMarginFactor) *
      FFontSize) * 1000 / FFontSize);
  end;

var
  AFormatter: TdxPDFLineFormatter;
  ALayoutWidth: Double;
  ALimitedLines: Boolean;
  ALineCount, I: Integer;
  ALines: TdxPDFGlyphRunList;
  AText: TArray<string>;
begin
  FShowWithGlyphOffsets := AEmTabStopInterval <> 0;
  CalculateLineCount(ALayoutWidth, ALineCount, ALimitedLines);
  if TdxPDFTextUtils.HasRTLMarker(AStr) then
    AFormatter := TdxPDFRTLLineFormatter.Create(ALayoutWidth, FSpaceGlyphIndex, ALineCount, AFormat, FEllipsis,
      FFontData, AEmTabStopInterval)
  else
    AFormatter := TdxPDFLineFormatter.Create(ALayoutWidth, ALineCount, AFormat, FEllipsis, FFontData, AEmTabStopInterval);
  try
    AText := TdxPDFUtils.Split(ProcessHotkeyPrefixes(AStr, AFormat.HotkeyPrefix), [#10#13]);
    ALines := FormatString(AText, AFormatter, ALimitedLines, ALineCount, AFormat, AUseKerning or ShouldApplyKerning(Length(AText)));
    Result := TdxPDFGlyphRunList.Create;
    for I := 0 to ALines.Count - 1 do
    begin
      Result.Add(ALines[I]);
      if ALimitedLines and (I >= ALineCount) then
        Break;
    end;
  finally
    AFormatter.Free;
  end;
end;

function TdxPDFStringFormatter.FormatString(const AText: TArray<string>; AFormatter: TdxPDFLineFormatter;
  ALimitedLines: Boolean; ALineCount: Integer; const AFormat: TdxPDFStringFormat; AUseKerning: Boolean): TdxPDFGlyphRunList;
var
  AFlags: TdxPDFGlyphMappingFlags;
  ALine: string;
begin
  FShowWithGlyphOffsets := FShowWithGlyphOffsets or AUseKerning;
  AFlags := mfNone;
  if AUseKerning then
    AFlags := mfUseKerning;
  if AFormat.DirectionRightToLeft then
    AFlags := TdxPDFGlyphMappingFlags(Integer(AFlags) or Integer(mfDirectionRightToLeft));
  for ALine in AText do
  begin
    AFormatter.FormatLine(ALine, AFlags);
    if not AUseKerning and ShouldApplyKerning(AFormatter.Lines.Count) then
    begin
      AFormatter.Clear;
      Exit(FormatString(AText, AFormatter, ALimitedLines, ALineCount, AFormat, True));
    end;
    if ALimitedLines and (AFormatter.Lines.Count >= ALineCount) then
      Break;
  end;
  Result := AFormatter.Lines;
end;

function TdxPDFStringFormatter.ShouldApplyKerning(ALinesCount: Integer): Boolean;
begin
  Result := False;
end;

{ TdxPDFTabbedStringFormatter }

constructor TdxPDFTabbedStringFormatter.Create(AFontData: TdxPDFEditableFontData; AEmTabStopInterval: Double;
  ARightToLeft: Boolean; AShouldUseKerning: Boolean);
begin
  FFontData := AFontData;
  FEmTabStop := AEmTabStopInterval;
  FRightToLeft := ARightToLeft;
  FShouldUseKerning := AShouldUseKerning;
end;

procedure TdxPDFTabbedStringFormatter.AppendGlyphRun(const ATabPiece: string; ATabCount: Integer);
var
  AGlyph: TdxPDFGlyph;
  AGlyphRunWidth, ATabOffset: Double;
  APieceGlyphRun: TdxPDFGlyphRun;
begin
  APieceGlyphRun := MapString(ATabPiece);
  if FEmTabStop <> 0 then
  begin
    AGlyph := APieceGlyphRun.Glyphs[0];
    AGlyphRunWidth := 0;
    if FGlyphRun <> nil then
      AGlyphRunWidth := FGlyphRun.Width;
    ATabOffset := (Round(AGlyphRunWidth / FEmTabStop) + ATabCount) * FEmTabStop - AGlyphRunWidth;
    APieceGlyphRun.Glyphs[0] := TdxPDFGlyph.Create(AGlyph.Index, AGlyph.Width, -ATabOffset);
  end;
  if FGlyphRun = nil then
    FGlyphRun := APieceGlyphRun
  else
    FGlyphRun.Append(APieceGlyphRun);
end;

function TdxPDFTabbedStringFormatter.MapString(const ALine: string): TdxPDFGlyphRun;
var
  AFlags: TdxPDFGlyphMappingFlags;
begin
  AFlags := mfNone;
  if FRightToLeft then
    AFlags := TdxPDFGlyphMappingFlags(Integer(AFlags) or Integer(mfDirectionRightToLeft));
  if FShouldUseKerning then
    AFlags := TdxPDFGlyphMappingFlags(Integer(AFlags) or Integer(mfUseKerning));
  Result := FFontData.ProcessString(ALine, AFlags);
end;

function TdxPDFTabbedStringFormatter.FormatString(const ALine: string): TdxPDFGlyphRun;
var
  ATabbedPiece: string;
  ATabbedPieces: TArray<string>;
  ATabCount: Integer;
begin
  if TdxStringHelper.Trim(ALine, []) <> '' then
  begin
    FGlyphRun := nil;
    ATabCount := 0;
    ATabbedPieces := TdxPDFUtils.Split(ALine, [#9]);
    for ATabbedPiece in ATabbedPieces do
      if ATabbedPiece = '' then
        Inc(ATabCount)
      else
      begin
        AppendGlyphRun(ATabbedPiece, ATabCount);
        ATabCount := 1;
      end;
    Result := FGlyphRun;
  end
  else
    Result := MapString(ALine);
end;

{ TdxPDFCommandConstructor }

constructor TdxPDFCommandConstructor.Create(AResources: TdxPDFResources);
begin
  inherited Create;
  FStream := TdxPDFWriterStream.Create;
  FMatrixStack := TStack<TdxPDFTransformationMatrix>.Create;
  FCurrentTransformationMatrix := TdxPDFTransformationMatrix.Create;
  FResources := TdxPDFResourcesAccess(AResources);
  FEllipticFactor := 0.5 - (1 / Sqrt(2) - 0.5) / 0.75;
end;

destructor TdxPDFCommandConstructor.Destroy;
begin
  CurrentTransformationMatrix := nil;
  FreeAndNil(FMatrixStack);
  FreeAndNil(FStream);
  inherited Destroy;
end;

procedure TdxPDFCommandConstructor.AddCommands(const AData: TBytes);
begin
  if Length(AData) > 0 then
  begin
    WriteSpace;
    FStream.WriteArray(AData);
  end;
end;

procedure TdxPDFCommandConstructor.AppendRectangle(const ARect: TdxRectF);
begin
  WriteDouble(ARect.Left);
  WriteDouble(ARect.Top);
  WriteDouble(ARect.Width);
  WriteDouble(Abs(ARect.Height));
  WriteCommand(TdxPDFAppendRectangleCommand);
end;

procedure TdxPDFCommandConstructor.BeginMarkedContent;
begin
  WriteOperationData('/Tx BMC');
end;

procedure TdxPDFCommandConstructor.BeginText;
begin
  WriteCommand(TdxPDFBeginTextCommand);
  WriteSpace;
end;

procedure TdxPDFCommandConstructor.CloseAndStrokePath;
begin
  WriteCommand(TdxPDFCloseAndStrokePathCommand);
end;

procedure TdxPDFCommandConstructor.EndMarkedContent;
begin
  WriteOperationData('EMC');
end;

procedure TdxPDFCommandConstructor.EndText;
begin
  WriteCommand(TdxPDFEndTextCommand);
end;

procedure TdxPDFCommandConstructor.DrawEllipse(const ARect: TdxRectF);
begin
  AppendEllipse(ARect);
  CloseAndStrokePath;
end;

procedure TdxPDFCommandConstructor.DrawImage(AXObjectNumber: Integer; const ARect: TdxRectF);
var
  AMatrix: TdxPDFTransformationMatrix;
begin
  AMatrix := TdxPDFTransformationMatrix.CreateEx(ARect.Width, 0, 0, Abs(ARect.Height), ARect.Left, ARect.Bottom);
  try
    DrawXObject(AXObjectNumber, AMatrix);
  finally
    AMatrix.Free;
  end;
end;

procedure TdxPDFCommandConstructor.DrawLine(const P1, P2: TdxPointF);
begin
  WritePoint(P1);
  WriteBeginPathCommand;
  WritePoint(P2);
  WriteAppendLineSegmentCommand;
  WriteStrokePathCommand;
end;

procedure TdxPDFCommandConstructor.DrawLines(const AValue: TdxPDFPoints);
begin
  AppendPolygon(AValue);
  WriteStrokePathCommand;
end;

procedure TdxPDFCommandConstructor.DrawPolygon(const AValue: TdxPDFPoints);
begin
  AppendPolygon(AValue);
  CloseAndStrokePath;
end;

procedure TdxPDFCommandConstructor.DrawRectangle(const ARect: TdxRectF);
begin
  AppendRectangle(ARect);
  WriteStrokePathCommand;
end;

procedure TdxPDFCommandConstructor.IntersectClip(const ARect: TdxRectF);
begin
  AppendRectangle(ARect);
  WriteCommand(TdxPDFModifyClippingPathUsingNonzeroWindingNumberRuleCommand);
  WriteCommand(TdxPDFEndPathWithoutFillingAndStrokingCommand);
end;

procedure TdxPDFCommandConstructor.IntersectClip(APath: TdxPDFGraphicsPath; ANonZero: Boolean = True);
begin
  GeneratePathCommands(APath);
  WriteIntersectClipCommand(ANonZero);
end;

procedure TdxPDFCommandConstructor.IntersectClip(APaths: TList<TdxPDFGraphicsPath>; ANonZero: Boolean);
var
  APath: TdxPDFGraphicsPath;
begin
  for APath in APaths do
    GeneratePathCommands(APath);
  WriteIntersectClipCommand(ANonZero);
end;

procedure TdxPDFCommandConstructor.FillEllipse(const ARect: TdxRectF);
begin
  AppendEllipse(ARect);
  WriteClosePathCommand;
  WriteFillPathCommand(True);
end;

procedure TdxPDFCommandConstructor.FillPath(APaths: TList<TdxPDFGraphicsPath>; ANonZero: Boolean);
var
  APath: TdxPDFGraphicsPath;
begin
  for APath in APaths do
    GeneratePathCommands(APath);
  WriteFillPathCommand(ANonZero);
end;

procedure TdxPDFCommandConstructor.FillPolygon(const APoints: TdxPDFPoints; ANonZero: Boolean);
begin
  AppendPolygon(APoints);
  WriteFillPathCommand(ANonZero);
end;

procedure TdxPDFCommandConstructor.FillRectangle(const ARect: TdxRectF);
begin
  AppendRectangle(ARect);
  WriteFillPathCommand(True);
end;

procedure TdxPDFCommandConstructor.ModifyTransformationMatrix(AMatrix: TdxPDFTransformationMatrix);
begin
  CurrentTransformationMatrix := TdxPDFTransformationMatrix.MultiplyMatrix(AMatrix, CurrentTransformationMatrix);
  WriteDouble(AMatrix.A);
  WriteDouble(AMatrix.B);
  WriteDouble(AMatrix.C);
  WriteDouble(AMatrix.D);
  WriteDouble(AMatrix.E);
  WriteDouble(AMatrix.F);
  WriteCommand(TdxPDFModifyTransformationMatrixCommand);
end;

procedure TdxPDFCommandConstructor.SaveGraphicsState;
begin
  FMatrixStack.Push(FCurrentTransformationMatrix);
  WriteCommand(TdxPDFSaveGraphicsStateCommand);
end;

procedure TdxPDFCommandConstructor.SetCharacterSpacing(AValue: Single);
begin
  WriteDouble(AValue);
  WriteCommand(TdxPDFSetCharacterSpacingCommand);
end;

procedure TdxPDFCommandConstructor.SetColorForNonStrokingOperations(AColor: TdxPDFColor);
var
  AComponent: Double;
  AComponents: TDoubleDynArray;
begin
  if AColor <> nil then
    if AColor.Pattern = nil then
    begin
      AComponents := AColor.Components;
      case Length(AComponents) of
        1:
          begin
            WriteDouble(AComponents[0]);
            WriteCommand(TdxPDFSetGrayColorSpaceForNonStrokingOperationsCommand);
          end;
        4:
          begin
            WriteDouble(AComponents[0]);
            WriteDouble(AComponents[1]);
            WriteDouble(AComponents[2]);
            WriteDouble(AComponents[3]);
            WriteCommand(TdxPDFSetCMYKColorSpaceForNonStrokingOperationsCommand);
          end;
      else
        WriteDouble(AComponents[0]);
        WriteDouble(AComponents[1]);
        WriteDouble(AComponents[2]);
        WriteCommand(TdxPDFSetRGBColorSpaceForNonStrokingOperationsCommand);
      end;
    end
    else
    begin
      WriteOperationData(Pattern + TdxPDFSetColorSpaceForNonStrokingOperationsCommand.GetName);
      for AComponent in AColor.Components do
        WriteDouble(AComponent);
      WriteSpace;
      if AColor.Pattern <> nil then
      begin
        WriteNameDelimiter;
        WriteString(FResources.AddPattern(AColor.Pattern as TdxPDFCustomPattern));
      end;
      WriteCommand(TdxPDFSetColorAdvancedForNonStrokingOperationsCommand);
    end;
end;

procedure TdxPDFCommandConstructor.SetColorForStrokingOperations(AColor: TdxPDFColor);
var
  AComponent: Double;
  AComponents: TDoubleDynArray;
begin
  if AColor <> nil then
    if AColor.Pattern = nil then
    begin
      AComponents := AColor.Components;
      case Length(AComponents) of
        1:
          begin
            WriteDouble(AComponents[0]);
            WriteCommand(TdxPDFSetGrayColorSpaceForStrokingOperationsCommand);
          end;
        4:
          begin
            WriteDouble(AComponents[0]);
            WriteDouble(AComponents[1]);
            WriteDouble(AComponents[2]);
            WriteDouble(AComponents[3]);
            WriteCommand(TdxPDFSetCMYKColorSpaceForStrokingOperationsCommand);
          end;
      else
        WriteDouble(AComponents[0]);
        WriteDouble(AComponents[1]);
        WriteDouble(AComponents[2]);
        WriteCommand(TdxPDFSetRGBColorSpaceForStrokingOperationsCommand);
      end;
    end
    else
    begin
      WriteOperationData(Pattern + TdxPDFSetColorSpaceForStrokingOperationsCommand.GetName);
      for AComponent in AColor.Components do
        WriteDouble(AComponent);
      WriteSpace;
      if AColor.Pattern <> nil then
      begin
        WriteNameDelimiter;
        WriteString(FResources.AddPattern(AColor.Pattern as TdxPDFCustomPattern));
      end;
      WriteCommand(TdxPDFSetColorAdvancedForStrokingOperationsCommand);
    end;
end;

procedure TdxPDFCommandConstructor.SetColorForStrokingOperations(AColor: TdxPDFARGBColor);
begin
  WriteDouble(AColor.Red);
  WriteDouble(AColor.Green);
  WriteDouble(AColor.Blue);
  WriteCommand(TdxPDFSetRGBColorSpaceForStrokingOperationsCommand);
end;

procedure TdxPDFCommandConstructor.SetGraphicsStateParameters(AParameters: TdxPDFGraphicsStateParameters);
begin
  WriteNameDelimiter;
  FStream.WriteString(FResources.AddGraphicsStateParameters(AParameters));
  WriteCommand(TdxPDFSaveGraphicsStateCommand);
end;

procedure TdxPDFCommandConstructor.SetLineCapStyle(AValue: TdxPDFLineCapStyle);
begin
  WriteSpace;
  WriteString(IntToStr(Integer(AValue)));
  WriteCommand(TdxPDFSetLineCapStyleCommand);
end;

procedure TdxPDFCommandConstructor.SetLineJoinStyle(AValue: TdxPDFLineJoinStyle);
begin
  WriteSpace;
  FStream.WriteByte(Byte(AValue));
  WriteCommand(TdxPDFSetLineJoinStyleCommand);
end;

procedure TdxPDFCommandConstructor.SetLineStyle(AValue: TdxPDFLineStyle);
var
  I: Integer;
begin
  if AValue <> nil then
  begin
    WriteSpace;
    WriteOpenBracket;
    if Length(AValue.Pattern) > 0 then
      for I := Low(AValue.Pattern) to High(AValue.Pattern) do
        WriteDouble(AValue.Pattern[I]);
    WriteCloseBracket;
    WriteDouble(AValue.Phase);
    WriteCommand(TdxPDFSetLineStyleCommand);
  end;
end;

procedure TdxPDFCommandConstructor.SetLineWidth(AValue: Single);
begin
  WriteDouble(AValue);
  WriteCommand(TdxPDFSetLineWidthCommand);
end;

procedure TdxPDFCommandConstructor.SetMiterLimit(AValue: Single);
begin
  WriteDouble(AValue);
  WriteCommand(TdxPDFSetMiterLimitCommand);
end;

procedure TdxPDFCommandConstructor.SetObliqueTextMatrix(X, Y: Single);
begin
  WriteOperationData('1 0 0.333 1');
  WriteDouble(X);
  WriteDouble(Y);
  WriteCommand(TdxPDFSetTextMatrixCommand);
end;

procedure TdxPDFCommandConstructor.SetTextFont(AFont: TObject; AFontSize: Single);
begin
  SetTextFont(FResources.AddFont(AFont as TdxPDFCustomFont), AFontSize);
end;

procedure TdxPDFCommandConstructor.SetTextFont(const AFontName: string; AFontSize: Single);
begin
  WriteNameDelimiter;
  WriteString(AFontName);
  WriteDouble(AFontSize);
  WriteCommand(TdxPDFSetTextFontCommand);
end;

procedure TdxPDFCommandConstructor.SetTextHorizontalScaling(AValue: Single);
begin
  WriteDouble(AValue);
  WriteCommand(TdxPDFSetTextHorizontalScalingCommand);
end;

procedure TdxPDFCommandConstructor.SetTextRenderingMode(AValue: TdxPDFTextRenderingMode);
begin
  WriteSpace;
  WriteString(IntToStr(Integer(AValue)));
  WriteCommand(TdxPDFSetTextRenderingModeCommand);
end;

procedure TdxPDFCommandConstructor.SetWordSpacing(AValue: Single);
begin
  WriteDouble(AValue);
  WriteCommand(TdxPDFSetWordSpacingCommand);
end;

procedure TdxPDFCommandConstructor.ShowText(const AText: TBytes; AGlyphOffsets: TDoubleDynArray);

  procedure WriteGlyphOffset(AOffset: Double);
  begin
    if AOffset <> 0 then
    begin
      WriteSpace;
      WriteDouble(AOffset);
    end;
  end;

var
  ALength, APosition: Integer;
  AStr: TBytes;
begin
  ALength := Length(AText);
  if ALength > 0 then
  begin
    WriteSpace;
    if Length(AGlyphOffsets) = 0 then
    begin
      WriteHexadecimalString(AText);
      WriteCommand(TdxPDFShowTextCommand);
    end
    else
    begin
      WriteOpenBracket;
      APosition := 0;
      while APosition < ALength do
      begin
        WriteGlyphOffset(AGlyphOffsets[APosition]);
        WriteSpace;
        SetLength(AStr, 1);
        AStr[0] := AText[APosition];
        Inc(APosition);
        while (APosition < ALength) and (AGlyphOffsets[APosition] = 0) do
        begin
          TdxPDFUtils.AddByte(AText[APosition], AStr);
          Inc(APosition);
        end;
        WriteHexadecimalString(AStr);
      end;
      WriteGlyphOffset(AGlyphOffsets[Length(AGlyphOffsets) - 1]);
      WriteCloseBracket;
      WriteCommand(TdxPDFShowTextWithGlyphPositioningCommand);
    end;
  end;
end;

procedure TdxPDFCommandConstructor.StartTextLineWithOffsets(X, Y: Single);
begin
  WriteDouble(X);
  WriteDouble(Y);
  WriteCommand(TdxPDFStartTextLineWithOffsetsCommand);
end;

procedure TdxPDFCommandConstructor.RestoreGraphicsState;
begin
  if FMatrixStack.Count > 0 then
    CurrentTransformationMatrix := FMatrixStack.Extract;
  WriteCommand(TdxPDFRestoreGraphicsStateCommand);
end;

function TdxPDFCommandConstructor.GetCommands: TBytes;
begin
  Result := FStream.Data;
end;

procedure TdxPDFCommandConstructor.AppendBezierCurve(const P1, P2, P3: TdxPointF);
begin
  WritePoint(P1);
  WritePoint(P2);
  WritePoint(P3);
  WriteAppendBezierCurveCommand;
end;

procedure TdxPDFCommandConstructor.AppendEllipse(const ARect: TdxRectF);

  procedure AppendCurveCommand(const P11, P12, P21, P22, p31, P32: Single);
  begin
    AppendBezierCurve(dxPointF(P11, P12), dxPointF(P21, P22), dxPointF(P31, P32));
  end;

var
  ABottom, ATop, ACenterY, AVerticalOffset, ABottomControlPoint, ATopControlPoint: Double;
  ALeft, ARight, ACenterX, AHorizontalOffset, ALeftControlPoint, ARightControlPoint: Double;
begin
  ALeft := ARect.Left;
  ARight := ARect.Right;
  ACenterX := (ALeft + ARight) / 2;
  AHorizontalOffset := (ARight - ALeft) * FEllipticFactor;
  ALeftControlPoint := ALeft + AHorizontalOffset;
  ARightControlPoint := ARight - AHorizontalOffset;
  ABottom := ARect.Bottom;
  ATop := ARect.Top;
  ACenterY := (ABottom + ATop) / 2;
  AVerticalOffset := (ATop - ABottom) * FEllipticFactor;
  ABottomControlPoint := ABottom + AVerticalOffset;
  ATopControlPoint := ATop - AVerticalOffset;

  WritePoint(dxPointF(ARight, ACenterY));
  WriteBeginPathCommand;

  AppendCurveCommand(ARight, ATopControlPoint, ARightControlPoint, ATop, ACenterX, ATop);
  AppendCurveCommand(ALeftControlPoint, ATop, ALeft, ATopControlPoint, ALeft, ACenterY);
  AppendCurveCommand(ALeft, ABottomControlPoint, ALeftControlPoint, ABottom, ACenterX, ABottom);
  AppendCurveCommand(ARightControlPoint, ABottom, ARight, ABottomControlPoint, ARight, ACenterY);
end;

procedure TdxPDFCommandConstructor.AppendPolygon(const APoints: TdxPDFPoints);
var
  I, APointCount: Integer;
begin
  APointCount := Length(APoints);
  if APointCount >= 2 then
  begin
    WritePoint(APoints[0]);
    WriteBeginPathCommand;
    for I := 1 to APointCount - 1 do
    begin
      WritePoint(APoints[I]);
      WriteAppendLineSegmentCommand;
    end;
  end;
end;

procedure TdxPDFCommandConstructor.DrawXObject(AXObjectNumber: Integer; AMatrix: TdxPDFTransformationMatrix);
begin
  SaveGraphicsState;
  ModifyTransformationMatrix(AMatrix);
  WriteNameDelimiter;
  WriteString(FResources.AddXObject(AXObjectNumber));
  WriteCommand(TdxPDFPaintXObjectCommand);
  RestoreGraphicsState;
end;

procedure TdxPDFCommandConstructor.GeneratePathCommands(APath: TdxPDFGraphicsPath);
var
  ABezierSegment: TdxPDFBezierGraphicsPathSegment;
  ASegment: TdxPDFGraphicsPathSegment;
begin
  WritePoint(APath.StartPoint);
  WriteBeginPathCommand;
  for ASegment in APath.Segments do
    if ASegment is TdxPDFBezierGraphicsPathSegment then
    begin
      ABezierSegment := TdxPDFBezierGraphicsPathSegment(ASegment);
      WritePoint(ABezierSegment.ControlPoint1);
      WritePoint(ABezierSegment.ControlPoint2);
      WritePoint(ABezierSegment.EndPoint);
      WriteAppendBezierCurveCommand;
    end
    else
    begin
      if ASegment is TdxPDFLineGraphicsPathSegment then
      begin
        WritePoint(TdxPDFLineGraphicsPathSegment(ASegment).EndPoint);
        WriteAppendLineSegmentCommand;
      end;
    end;
  if APath.IsClosed then
    WriteClosePathCommand;
end;

procedure TdxPDFCommandConstructor.SetCurrentTransformationMatrix(const AValue: TdxPDFTransformationMatrix);
begin
  if (FCurrentTransformationMatrix <> nil) and (FCurrentTransformationMatrix <> AValue) then
  begin
    FreeAndNil(FCurrentTransformationMatrix);
    FCurrentTransformationMatrix := AValue;
  end;
end;

procedure TdxPDFCommandConstructor.WriteAppendBezierCurveCommand;
begin
  WriteCommand(TdxPDFAppendBezierCurveCommand);
end;

procedure TdxPDFCommandConstructor.WriteAppendLineSegmentCommand;
begin
  WriteCommand(TdxPDFAppendLineSegmentCommand);
end;

procedure TdxPDFCommandConstructor.WriteBeginPathCommand;
begin
  WriteCommand(TdxPDFBeginPathCommand);
end;

procedure TdxPDFCommandConstructor.WriteClosePathCommand;
begin
  WriteCommand(TdxPDFClosePathCommand);
end;

procedure TdxPDFCommandConstructor.WriteCloseBracket;
begin
  FStream.WriteCloseBracket;
end;

procedure TdxPDFCommandConstructor.WriteCommand(const ACommandClass: TdxPDFCustomCommandClass);
begin
  WriteOperationData(ACommandClass.GetName);
end;

procedure TdxPDFCommandConstructor.WriteDouble(AValue: Double);
begin
  FStream.WriteDouble(AValue);
end;

procedure TdxPDFCommandConstructor.WriteIntersectClipCommand(ANonZero: Boolean);
begin
  if ANonZero then
    WriteCommand(TdxPDFModifyClippingPathUsingNonzeroWindingNumberRuleCommand)
  else
    WriteCommand(TdxPDFModifyClippingPathUsingEvenOddRuleCommand);
  WriteCommand(TdxPDFEndPathWithoutFillingAndStrokingCommand);
end;

procedure TdxPDFCommandConstructor.WriteFillPathCommand(ANonZero: Boolean);
begin
  if ANonZero then
    WriteCommand(TdxPDFFillPathUsingNonzeroWindingNumberRuleCommand)
  else
    WriteCommand(TdxPDFFillPathUsingEvenOddRuleCommand);
end;

procedure TdxPDFCommandConstructor.WriteHexadecimalString(const AData: TBytes);

  procedure DoWriteByte(B: Byte);
  begin
    FStream.WriteByte(Byte(IfThen(B > 9, B + $37, B + $30)));
  end;

var
  ABytes: TBytes;
  B: Byte;
  I, L: Integer;
begin
  L := Length(AData);
  SetLength(ABytes, L * 2 + 2);
  FStream.WriteByte(Byte('<'));
  for I := 0 to L - 1 do
  begin
    B := AData[I];
    DoWriteByte(B shr 4);
    DoWriteByte(B and 15);
  end;
  FStream.WriteByte(Byte('>'));
end;

procedure TdxPDFCommandConstructor.WriteNameDelimiter;
begin
  FStream.WriteByte(Byte('/'));
end;

procedure TdxPDFCommandConstructor.WriteOpenBracket;
begin
  FStream.WriteOpenBracket;
end;

procedure TdxPDFCommandConstructor.WriteOperationData(const AData: string);
begin
  WriteString(' ' + AData);
end;

procedure TdxPDFCommandConstructor.WritePoint(const P: TdxPointF);
begin
  WriteDouble(P.X);
  WriteDouble(P.Y);
end;

procedure TdxPDFCommandConstructor.WriteSpace;
begin
  FStream.WriteSpace;
end;

procedure TdxPDFCommandConstructor.WriteString(const S: string);
begin
  FStream.WriteString(S);
end;

procedure TdxPDFCommandConstructor.WriteStrokePathCommand;
begin
  WriteCommand(TdxPDFStrokePathCommand);
end;

{ TdxPDFFormCommandConstructor }

constructor TdxPDFFormCommandConstructor.Create(AForm: TdxPDFForm);
var
  ACenter: TdxPointF;
  AHalfDimension: Double;
begin
  inherited Create(AForm.Resources);
  FForm := AForm;
  AHalfDimension := TdxPDFUtils.Min(AForm.BBox.Width, Abs(AForm.BBox.Height)) / 2;
  ACenter := AForm.BBox.CenterPoint;
  FContentSquare := TdxRectF.CreateSize(ACenter.X - AHalfDimension, ACenter.Y - AHalfDimension,
    ACenter.X + AHalfDimension, ACenter.Y + AHalfDimension);
end;

function TdxPDFFormCommandConstructor.GetBoundingBox: TdxRectF;
begin
  Result := FForm.BBox;
end;

{ TdxPDFAnnotationAppearanceBuilder<T> }

constructor TdxPDFAnnotationAppearanceBuilder<T>.Create(AAnnotation: T);
begin
  inherited Create;
  FAnnotation := AAnnotation;
end;

procedure TdxPDFAnnotationAppearanceBuilder<T>.RebuildAppearance(AForm: TdxPDFForm);
var
  AConstructor: TdxPDFFormCommandConstructor;
begin
  AConstructor := TdxPDFFormCommandConstructor.Create(AForm);
  try
    Rebuild(AConstructor);
    AForm.ReplaceCommands(AConstructor.Commands);
  finally
    AConstructor.Free;
  end;
end;

{ TdxPDFMarkupAnnotationAppearanceBuilder<T> }

procedure TdxPDFMarkupAnnotationAppearanceBuilder<T>.Rebuild(AConstructor: TdxPDFFormCommandConstructor);
var
  AParameters: TdxPDFGraphicsStateParameters;
begin
  if not SameValue(Annotation.Opacity, 1.0) then
  begin
    AParameters := TdxPDFGraphicsStateParameters.Create(nil);
    AParameters.NonStrokingColorAlpha := Annotation.Opacity;
    AParameters.StrokingColorAlpha := Annotation.Opacity;
    AConstructor.SetGraphicsStateParameters(AParameters);
  end;
end;

{ TdxPDFWidgetAnnotationAppearanceBuilder }

constructor TdxPDFWidgetAnnotationAppearanceBuilder<T>.Create(AWidget: TdxPDFWidgetAnnotation; AFormField: T);
begin
  inherited Create(AWidget);
  FFormField := AFormField;
  if AWidget.BorderStyle = nil then
  begin
    if AWidget.Border <> nil then
      FBorderWidth := AWidget.Border.LineWidth;
  end
  else
    FBorderWidth := AWidget.BorderStyle.Width;
end;

procedure TdxPDFWidgetAnnotationAppearanceBuilder<T>.Rebuild(AConstructor: TdxPDFFormCommandConstructor);
var
  AContentRect: TdxRectF;
begin
  AContentRect := Annotation.AppearanceContentRectangle;
  AConstructor.SaveGraphicsState;
  FillBackground(AConstructor);
  DrawBorder(AConstructor, AContentRect);
  AConstructor.RestoreGraphicsState;
  AConstructor.BeginMarkedContent;
  AConstructor.SaveGraphicsState;
  AConstructor.IntersectClip(AContentRect);
  DrawContent(AConstructor, AContentRect);
  AConstructor.RestoreGraphicsState;
  AConstructor.EndMarkedContent;
end;

function TdxPDFWidgetAnnotationAppearanceBuilder<T>.GetBackgroundColor: TdxPDFColor;
begin
  Result := Annotation.BackgroundColor;
end;

procedure TdxPDFWidgetAnnotationAppearanceBuilder<T>.DrawBorder(AConstructor: TdxPDFFormCommandConstructor;
  const AContentRect: TdxRectF);
var
  AAppearanceCharacteristics: TdxPDFWidgetAppearanceCharacteristics;
  AWidget: TdxPDFWidgetAnnotation;
begin
  AWidget := Annotation;
  AAppearanceCharacteristics := AWidget.AppearanceCharacteristics;
  if (AAppearanceCharacteristics <> nil) and (AAppearanceCharacteristics.BorderColor <> nil) then
  begin
    AConstructor.SetColorForStrokingOperations(AAppearanceCharacteristics.BorderColor);
    if AWidget.BorderStyle = nil then
    begin
      if AWidget.Border <> nil then
      begin
        AConstructor.SetLineWidth(AWidget.Border.LineWidth);
        AConstructor.SetLineStyle(AWidget.Border.LineStyle);
        DrawSolidBorder(AConstructor, AContentRect);
      end;
    end
    else
      DrawStyledBorder(AConstructor, AContentRect, AWidget.BorderStyle);
  end;
end;

procedure TdxPDFWidgetAnnotationAppearanceBuilder<T>.DrawRectangularBeveledBorder(AConstructor: TdxPDFFormCommandConstructor);
begin
  AConstructor.SetColorForNonStrokingOperations(TdxPDFColor.Create([1.0]));
  DrawRectangularBorderUpperLeftStroke(AConstructor);
  AConstructor.SetColorForNonStrokingOperations(TdxPDFColor.Create([0.5]));
  DrawRectangularBorderBottomRightStroke(AConstructor);
end;

procedure TdxPDFWidgetAnnotationAppearanceBuilder<T>.DrawRectangularBorderBottomRightStroke(
  AConstructor: TdxPDFFormCommandConstructor);
var
  AAnnotationRect: TdxRectF;
  AAnnotationHeight, ARight, ALeft, ADoubleBorderWidth: Double;
  APoints: TdxPDFPoints;
begin
  AAnnotationRect := AConstructor.BoundingBox;
  AAnnotationHeight := AAnnotationRect.Height;
  ARight := AAnnotationRect.Width - FBorderWidth;
  ALeft := ARight - FBorderWidth;
  ADoubleBorderWidth := 2 * FBorderWidth;
  SetLength(APoints, 7);
  APoints[0] := TdxPointF.Create(FBorderWidth, FBorderWidth);
  APoints[1] := TdxPointF.Create(ARight, FBorderWidth);
  APoints[2] := TdxPointF.Create(ARight, AAnnotationHeight - FBorderWidth);
  APoints[3] := TdxPointF.Create(ALeft, AAnnotationHeight - ADoubleBorderWidth);
  APoints[4] := TdxPointF.Create(ALeft, ADoubleBorderWidth);
  APoints[5] := TdxPointF.Create(ADoubleBorderWidth, ADoubleBorderWidth);
  APoints[6] := TdxPointF.Create(FBorderWidth, FBorderWidth);
  AConstructor.FillPolygon(APoints, True);
end;

procedure TdxPDFWidgetAnnotationAppearanceBuilder<T>.DrawRectangularBorderStroke(AConstructor: TdxPDFFormCommandConstructor);
var
  AHalfBorderWidth, W, H: Double;
  R: TdxRectF;
begin
  AHalfBorderWidth := FBorderWidth / 2;
  W := AConstructor.BoundingBox.Width - AHalfBorderWidth;
  H := AConstructor.BoundingBox.Height - AHalfBorderWidth;
  R := TdxRectF.CreateSize(TdxPDFUtils.Min(AHalfBorderWidth, W), TdxPDFUtils.Min(AHalfBorderWidth, H),
    TdxPDFUtils.Max(AHalfBorderWidth, W), TdxPDFUtils.Max(AHalfBorderWidth, H));
  AConstructor.AppendRectangle(R);
  AConstructor.CloseAndStrokePath;
end;

procedure TdxPDFWidgetAnnotationAppearanceBuilder<T>.DrawRectangularBorderUpperLeftStroke(
  AConstructor: TdxPDFFormCommandConstructor);
var
  AAnnotationRect: TdxRectF;
  AAnnotationWidth, ATop, ABottom, ADoubleBorderWidth: Double;
  APoints: TdxPDFPoints;
begin
  AAnnotationRect := AConstructor.BoundingBox;
  AAnnotationWidth := AAnnotationRect.Width;
  ATop := AAnnotationRect.Height - FBorderWidth;
  ABottom := ATop - FBorderWidth;
  ADoubleBorderWidth := 2 * FBorderWidth;
  SetLength(APoints, 7);
  APoints[0] := TdxPointF.Create(FBorderWidth, FBorderWidth);
  APoints[1] := TdxPointF.Create(FBorderWidth, ATop);
  APoints[2] := TdxPointF.Create(AAnnotationWidth - FBorderWidth, ATop);
  APoints[3] := TdxPointF.Create(AAnnotationWidth - ADoubleBorderWidth, ABottom);
  APoints[4] := TdxPointF.Create(ADoubleBorderWidth, ABottom);
  APoints[5] := TdxPointF.Create(ADoubleBorderWidth, ADoubleBorderWidth);
  APoints[6] := TdxPointF.Create(FBorderWidth, FBorderWidth);
  AConstructor.FillPolygon(APoints, True);
end;

procedure TdxPDFWidgetAnnotationAppearanceBuilder<T>.DrawRectangularInsetBorder(AConstructor: TdxPDFFormCommandConstructor);
begin
  AConstructor.SetColorForNonStrokingOperations(TdxPDFColor.Create([0.5]));
  DrawRectangularBorderUpperLeftStroke(AConstructor);
  AConstructor.SetColorForNonStrokingOperations(TdxPDFColor.Create([0.75]));
  DrawRectangularBorderBottomRightStroke(AConstructor);
end;

procedure TdxPDFWidgetAnnotationAppearanceBuilder<T>.DrawRectangularUnderlineBorder(AConstructor: TdxPDFFormCommandConstructor);
var
  AHalfBorderWidth: Double;
  APoints: TdxPDFPoints;
begin
  AHalfBorderWidth := BorderWidth / 2;
  SetLength(APoints, 2);
  APoints[0] := TdxPointF.Create(0, AHalfBorderWidth);
  APoints[1] := TdxPointF.Create(AConstructor.BoundingBox.Width, AHalfBorderWidth);
  AConstructor.DrawLines(APoints);
end;

procedure TdxPDFWidgetAnnotationAppearanceBuilder<T>.DrawTextCombs(AConstructor: TdxPDFFormCommandConstructor;
  const AContentRect: TdxRectF; AMaxLen: Integer);
var
  I: Integer;
  AHalfBorderWidth, AStep, X: Double;
begin
  if AMaxLen <> 0 then
  begin
    AHalfBorderWidth := FBorderWidth / 2;
    AStep := AContentRect.Width / AMaxLen;
    for I := 1 to AMaxLen - 1 do
    begin
      X := I * AStep + AHalfBorderWidth;
      AConstructor.DrawLine(TdxPointF.Create(X, AContentRect.Top), TdxPointF.Create(X, AContentRect.Bottom));
    end;
  end;
end;

procedure TdxPDFWidgetAnnotationAppearanceBuilder<T>.FillBackgroundRectangle(AConstructor: TdxPDFFormCommandConstructor;
  const ARect: TdxRectF; AColor: TdxPDFColor);
begin
  if AColor <> nil then
  begin
    AConstructor.SetColorForNonStrokingOperations(AColor);
    AConstructor.FillRectangle(ARect);
  end;
end;

procedure TdxPDFWidgetAnnotationAppearanceBuilder<T>.FillBackgroundEllipse(AConstructor: TdxPDFFormCommandConstructor;
  const ARect: TdxRectF; AColor: TdxPDFColor);
begin
  if AColor <> nil then
  begin
    AConstructor.SetColorForNonStrokingOperations(AColor);
    AConstructor.FillEllipse(ARect);
  end;
end;

procedure TdxPDFWidgetAnnotationAppearanceBuilder<T>.DrawStyledBorder(AConstructor: TdxPDFFormCommandConstructor;
  const AContentRect: TdxRectF; ABorderStyle: TdxPDFAnnotationBorderStyle);
begin
  AConstructor.SetLineWidth(ABorderStyle.Width);
  if ABorderStyle.StyleName = 'D' then
  begin
    AConstructor.SetLineStyle(ABorderStyle.LineStyle);
    DrawSolidBorder(AConstructor, AContentRect);
  end
  else
    if ABorderStyle.StyleName = 'B' then
    begin
      DrawSolidBorder(AConstructor, AContentRect);
      DrawBeveledBorder(AConstructor);
    end
    else
      if ABorderStyle.StyleName = 'I' then
      begin
        DrawSolidBorder(AConstructor, AContentRect);
        DrawInsetBorder(AConstructor);
      end
      else
        if ABorderStyle.StyleName = 'U' then
          DrawUnderlineBorder(AConstructor)
        else
          DrawSolidBorder(AConstructor, AContentRect);
end;

{ TdxPDFTextBasedFormFieldAppearanceBuilder<T> }

constructor TdxPDFTextBasedFormFieldAppearanceBuilder<T>.Create(AWidget: TdxPDFWidgetAnnotation; AFormField: T;
  AState: TdxPDFDocumentState);
var
  AFontInfo: TdxPDFFontInfo;
begin
  inherited Create(AWidget, AFormField);
  FFontDataStorage := AState.FontDataStorage;
  AFontInfo := FormField.GetFontInfo(AState);
  FFontData := AFontInfo.FontData as TdxPDFEditableFontData;
  FFontSize := AFontInfo.FontSize;
end;

procedure TdxPDFTextBasedFormFieldAppearanceBuilder<T>.FillBackground(AConstructor: TdxPDFFormCommandConstructor);
begin
  FillBackgroundRectangle(AConstructor, AConstructor.BoundingBox, BackgroundColor);
end;

procedure TdxPDFTextBasedFormFieldAppearanceBuilder<T>.Rebuild(AConstructor: TdxPDFFormCommandConstructor);
begin
  if FFontData <> nil then
    inherited Rebuild(AConstructor);
end;

function TdxPDFTextBasedFormFieldAppearanceBuilder<T>.CalculateCenteredLineYOffset(const AClipRect: TdxRectF): Double;
begin
  Result := AClipRect.Top + (AClipRect.Height - FFontData.Metrics.GetLineSpacing(FFontSize)) / 2 +
    FFontData.Metrics.GetDescent(FFontSize);
end;

function TdxPDFTextBasedFormFieldAppearanceBuilder<T>.GetTextWidth(const AText: string; AFontSize: Double): Double;
begin
  Result := FFontData.GetTextWidth(AText, AFontSize, FormField.TextState);
end;

procedure TdxPDFTextBasedFormFieldAppearanceBuilder<T>.DrawTextBoxText(AConstructor: TdxPDFFormCommandConstructor;
  const AOffset: TdxPointF; const AText: string);
var
  ARun: TdxPDFGlyphRun;
begin
  ARun := FFontData.ProcessString(AText, TdxPDFGlyphMappingFlags.mfNone);
  try
    AConstructor.StartTextLineWithOffsets(AOffset.X, AOffset.Y);
    AConstructor.ShowText(ARun.TextData, ARun.GlyphOffsets);
    FFontData.UpdateFont;
  finally
    ARun.Free;
  end;
end;

procedure TdxPDFTextBasedFormFieldAppearanceBuilder<T>.EndDrawTextBox(AConstructor: TdxPDFFormCommandConstructor);
begin
  AConstructor.EndText;
end;

procedure TdxPDFTextBasedFormFieldAppearanceBuilder<T>.RemoveFontFromStorage(AFont: TdxPDFCustomFont);
begin
  FFontDataStorage.Delete(AFont);
end;

procedure TdxPDFTextBasedFormFieldAppearanceBuilder<T>.StartDrawTextBox(AConstructor: TdxPDFFormCommandConstructor;
  AForeColor: TdxPDFColor);
begin
  RemoveFontFromStorage(FFontData.Font);
  AConstructor.BeginText;
  if AForeColor = nil then
    AConstructor.AddCommands(FormField.TextState.CommandsAsBytes)
  else
    AConstructor.SetColorForNonStrokingOperations(AForeColor);
  AConstructor.SetTextFont(FFontData.Font, FFontSize);
end;

{ TdxPDFTextFieldAppearanceBuilder<T> }

function TdxPDFTextFieldAppearanceBuilder<T>.CreateStringFormat: TdxPDFStringFormat;
begin
  Result := TdxPDFStringFormat.Create(sffNoClip);
  Result.LeadingMarginFactor := 0;
  Result.TrailingMarginFactor := 0;
  Result.Trimming := stNone;
  Result.Alignment := dxPDFFieldTextAlignmentMap[FormField.TextJustification];
  if not Multiline then
  begin
    Result.FormatFlags := TdxPDFStringFormatFlags(Integer(Result.FormatFlags) or Integer(sffNoWrap));
    Result.LineAlignment := saCenter;
  end;
end;

{ TdxPDFTextFormFieldAppearanceBuilder }

function TdxPDFTextFormFieldAppearanceBuilder.GetMultiline: Boolean;
begin
  Result := (Integer(FormField.Flags) and Integer(TdxPDFInteractiveFormFieldFlags.ffMultiline)) <> 0;
end;

procedure TdxPDFTextFormFieldAppearanceBuilder.DrawBeveledBorder(AConstructor: TdxPDFFormCommandConstructor);
begin
// do nothing
end;

procedure TdxPDFTextFormFieldAppearanceBuilder.DrawContent(AConstructor: TdxPDFFormCommandConstructor;
  const AContentRect: TdxRectF);
var
  ACh: string;
  AMaxLen, ALength, I: Integer;
  AStep, APreviousCharWidth, AInitialPosition, ACharWidth: Double;
  AChars: TArray<Char>;
begin
  if FormField.Text <> '' then
  begin
    AMaxLen := IfThen(TdxPDFUtils.IsIntegerValid(FormField.MaxLen), FormField.MaxLen, 0);
    if HasFlag(FormField.Flags, TdxPDFInteractiveFormFieldFlags.ffCombo) and (AMaxLen <> 0) then
    begin
      StartDrawTextBox(AConstructor, nil);
      AStep := AConstructor.BoundingBox.Width / AMaxLen;
    {$IFDEF DELPHIXE3}
      AChars := FormField.Text.ToCharArray;
    {$ELSE}
      ALength := Length(FormField.Text);
      SetLength(AChars, ALength);
      if ALength > 0 then
        Move(PChar(FormField.Text)^, AChars[0], ALength * SizeOf(Char));
    {$ENDIF}
      ACh := AChars[0];
      APreviousCharWidth := GetTextWidth(ACh, FontSize);
      AInitialPosition := (AStep - APreviousCharWidth) / 2;
      ALength := Min(Length(AChars), AMaxLen);
      case FormField.TextJustification of
        TdxPDFTextJustification.tjCentered:
          AInitialPosition := AInitialPosition + AStep * (AMaxLen / 2 - ALength / 2);
        TdxPDFTextJustification.tjRightJustified:
          AInitialPosition := AInitialPosition + AStep * (AMaxLen - ALength);
      end;
      DrawTextBoxText(AConstructor, dxPointF(AInitialPosition, CalculateCenteredLineYOffset(AContentRect)), ACh);
      for I := 1 to ALength - 1 do
      begin
        ACh := AChars[I];
        ACharWidth := GetTextWidth(ACh, FontSize);
        DrawTextBoxText(AConstructor, dxPointF(AStep + (APreviousCharWidth - ACharWidth) / 2, 0), ACh);
        APreviousCharWidth := ACharWidth;
      end;
      EndDrawTextBox(AConstructor);
    end
    else
      DrawTextField(AConstructor, AContentRect, FormField.Text);
  end;
end;

procedure TdxPDFTextFormFieldAppearanceBuilder.DrawInsetBorder(AConstructor: TdxPDFFormCommandConstructor);
begin
// do nothing
end;

procedure TdxPDFTextFormFieldAppearanceBuilder.DrawSolidBorder(AConstructor: TdxPDFFormCommandConstructor;
  const AContentRect: TdxRectF);
begin
// do nothing
end;

procedure TdxPDFTextFormFieldAppearanceBuilder.DrawUnderlineBorder(AConstructor: TdxPDFFormCommandConstructor);
begin
// do nothing
end;

procedure TdxPDFTextFormFieldAppearanceBuilder.DrawTextField(AConstructor: TdxPDFFormCommandConstructor;
  const AContentRect: TdxRectF; const AText: string);

  procedure FormatAndDrawString(APainter: TdxPDFWidgetStringPainter; ALineSpacing: TdxPDFCustomSpacing;
    const ARect: TdxRectF; const AText: string);
  var
    AFontInfo: TdxPDFFontInfo;
    AFormat: TdxPDFStringFormat;
    AFormatter: TdxPDFStringFormatter;
    AGlyphRun: TdxPDFGlyphRun;
    ALines: TdxPDFGlyphRunList;
  begin
    AFormat := CreateStringFormat;
    AFontInfo.FontData := FontData;
    AFontInfo.FontSize := FontSize;
    AFormatter := TdxPDFStringFormatter.Create(AFontInfo, ALineSpacing);
    try
      ALines := AFormatter.FormatString(AText, ARect, AFormat, False, 0);
      try
        if not Multiline and (ALines.Count > 1) then
          ALines.DeleteRange(1, ALines.Count - 1);
        APainter.DrawLines(ALines, AFontInfo, ARect, AFormat, False);
        FontData.UpdateFont;
        while ALines.Count > 0 do
        begin
          AGlyphRun := ALines[0];
          ALines.Delete(0);
          AGlyphRun.Free;
        end;
      finally
        ALines.Free;
      end;
    finally
      AFormatter.Free;
    end;
  end;

  function IsMultiline: Boolean;
  begin
    Result := Integer(Annotation.InteractiveFormField.Flags) and Integer(ffMultiline) <> 0;
  end;

var
  ALineSpacing: TdxPDFCustomSpacing;
  ATextRect: TdxRectF;
  AFontCommand: TdxPDFSetTextFontCommand;
  APainter: TdxPDFWidgetStringPainter;
begin
  RemoveFontFromStorage(FontData.Font);
  ATextRect := AContentRect;
  AFontCommand := nil;
  if (FormField.TextState.FontCommand <> nil) and (FormField.TextState.FontCommand is TdxPDFSetTextFontCommand) then
    AFontCommand := TdxPDFSetTextFontCommand(FormField.TextState.FontCommand);
  if (AFontCommand <> nil) and (AFontCommand.Font <> nil) then
    ALineSpacing := TdxPDFMultilineWidgetLineSpacing.Create(AFontCommand.Font.FontDescriptor)
  else
    ALineSpacing := TdxPDFMultilineWidgetLineSpacing.Create(FontData.Font.FontDescriptor);
  try
    if IsMultiline and (ALineSpacing.GetValue(FontSize) < AContentRect.Height) then
    begin
      ATextRect.Top := AContentRect.Bottom;
      ATextRect.Bottom := AContentRect.Top - ALineSpacing.GetValue(FontSize) + FontData.Metrics.GetAscent(FontSize);
    end
    else
    begin
      ALineSpacing.Free;
      ALineSpacing := TdxPDFLineSpacing.Create(FontData.Metrics);
    end;
    APainter := TdxPDFWidgetStringPainter.Create(AConstructor, FontData.ProcessString(' ', mfNone), ALineSpacing);
    try
      if FormField.TextState <> nil then
      begin
        APainter.UpdateSpacing(FormField.TextState);
        AConstructor.AddCommands(FormField.TextState.CommandsAsBytes);
      end;
      FormatAndDrawString(APainter, ALineSpacing, ATextRect, AText);
    finally
      APainter.Free;
    end;
  finally
    ALineSpacing.Free;
  end;
end;

{ TdxPDFLineTrimmingAlgorithm }

constructor TdxPDFLineTrimmingAlgorithm.Create(AFormatter: TdxPDFLineFormatter; AEllipsis: TdxPDFGlyphRun);
begin
  inherited Create;
  FFormatter := AFormatter;
  FEllipsis := AEllipsis;
end;

class function TdxPDFLineTrimmingAlgorithm.CreateAlgorithm(ATrimming: TdxPDFStringTrimming;
  AFormatter: TdxPDFLineFormatter; AEllipsis: TdxPDFGlyphRun): TdxPDFLineTrimmingAlgorithm;
begin
  case ATrimming of
    stCharacter:
      Result := TdxPDFLineTrimmingCharAlgorithm.Create(AFormatter, AEllipsis);
    stWord:
      Result := TdxPDFLineTrimmingWordAlgorithm.Create(AFormatter, AEllipsis);
    stEllipsisCharacter:
      Result := TdxPDFLineTrimmingEllipsisCharAlgorithm.Create(AFormatter, AEllipsis);
    stEllipsisWord:
      Result := TdxPDFLineTrimmingEllipsisWordAlgorithm.Create(AFormatter, AEllipsis);
  else
    Result := nil;
  end;
end;

function TdxPDFLineTrimmingAlgorithm.GetUseEllipsis: Boolean;
begin
  Result := False;
end;

procedure TdxPDFLineTrimmingAlgorithm.TryInsertEllipsis;
var
  AGlyphsToRemove, I: Integer;
begin
  if FEllipsisPosition <> 0 then
  begin
    AGlyphsToRemove := FFormatter.CurrentLineGlyphCount - FEllipsisPosition;
    for I := 0 to AGlyphsToRemove - 1 do
      FFormatter.RemoveLastGlyph;
    if FFormatter.CurrentLineWidth + FEllipsis.Width <= FFormatter.LayoutWidth then
      FFormatter.AddWord(FEllipsis);
  end;
end;

procedure TdxPDFLineTrimmingAlgorithm.SaveEllipsisPosition;
begin
  if FFormatter.CurrentLineWidth + FEllipsis.Width <= FFormatter.LayoutWidth then
    FEllipsisPosition := FFormatter.CurrentLineGlyphCount;
end;

{ TdxPDFLineTrimmingCharAlgorithm }

function TdxPDFLineTrimmingCharAlgorithm.ProcessWord(AWord: TdxPDFGlyphRun): Boolean;
var
  AGlyph: TdxPDFGlyph;
  AWidthWithGlyph: Double;
begin
  Result := False;
  for AGlyph in AWord.Glyphs do
  begin
    AWidthWithGlyph := Formatter.CurrentLineWidth + AGlyph.Width;
    if UseEllipsis then
    begin
      SaveEllipsisPosition;
      if AWidthWithGlyph > Formatter.LayoutWidth then
      begin
        TryInsertEllipsis;
        if Formatter.CurrentLineGlyphCount = 0 then
          Formatter.AddGlyph(AGlyph);
        Exit(True);
      end;
    end
    else
      if Formatter.CurrentLineWidth + AGlyph.Width > Formatter.LayoutWidth then
        Exit(True);
    Formatter.AddGlyph(AGlyph);
  end;
end;

{ TdxPDFLineTrimmingWordAlgorithm }

function TdxPDFLineTrimmingWordAlgorithm.ProcessWord(AWord: TdxPDFGlyphRun): Boolean;
var
  AWidthWithWord: Double;
begin
  if Formatter.IsCurrentLineEmpty and (AWord.Width > Formatter.LayoutWidth) then
    Result := inherited ProcessWord(AWord)
  else
  begin
    AWidthWithWord := Formatter.CurrentLineWidth + AWord.Width;
    if UseEllipsis then
    begin
      SaveEllipsisPosition;
      if AWidthWithWord > Formatter.LayoutWidth then
      begin
        TryInsertEllipsis;
        Exit(True);
      end;
    end
    else
      if Formatter.CurrentLineWidth + AWord.Width > Formatter.LayoutWidth then
        Exit(True);
    Formatter.AddWord(AWord);
    Result := False;
  end;
end;

{ TdxPDFLineTrimmingEllipsisCharAlgorithm }

function TdxPDFLineTrimmingEllipsisCharAlgorithm.GetUseEllipsis: Boolean;
begin
  Result := True;
end;

{ TdxPDFLineTrimmingEllipsisWordAlgorithm }

function TdxPDFLineTrimmingEllipsisWordAlgorithm.GetUseEllipsis: Boolean;
begin
  Result := True;
end;

{ TdxPDFFormatterWord }

class function TdxPDFFormatterWord.Create(const AText: string; AGlyphs: TdxPDFGlyphRun;
  AEndsWithSoftHyphen: Boolean): TdxPDFFormatterWord;
begin
  Result.FText := AText;
  Result.FGlyphs := AGlyphs;
  Result.FEndsWithSoftHyphen := AEndsWithSoftHyphen;
end;

function TdxPDFFormatterWord.GetWidth: Double;
begin
  Result := FGlyphs.Width;
end;

function TdxPDFFormatterWord.GetTextWithoutLeadingSpaces: string;
begin
  Result := TdxStringHelper.TrimStart(FText, [' ']);
end;

{ TdxPDFLineFormatter }

constructor TdxPDFLineFormatter.Create(ALayoutWidth: Double; ALayoutLineCount: Integer;
  const AFormat: TdxPDFStringFormat; AEllipsis: TdxPDFGlyphRun; AFontData: TdxPDFEditableFontData;
  AEmTabStopInterval: Double);
begin
  inherited Create;
  FNonBreakingSpace := #$00AD;
  FSoftHyphenChar := #$00AD;
  FSoftHyphenString := #$00AD;
  FLayoutWidth := ALayoutWidth;
  FLayoutLineCount := ALayoutLineCount;
  FFontData := AFontData;
  FEmTabStopInterval := AEmTabStopInterval;
  FNoWrap := HasFlag(AFormat.FormatFlags, sffNoWrap);

  FLines := TdxPDFGlyphRunList.Create;
  FTrimmingAlgorithm := TdxPDFLineTrimmingAlgorithm.CreateAlgorithm(AFormat.Trimming, Self, AEllipsis);
  FHyphenRun := FFontData.ProcessString(FSoftHyphenString, mfNone);
end;

destructor TdxPDFLineFormatter.Destroy;
begin
  FLines.Remove(FCurrentLine);
  FreeAndNil(FCurrentLine);
  FreeAndNil(FLines);
  FreeAndNil(FHyphenRun);
  FreeAndNil(FTrimmingAlgorithm);
  inherited Destroy;
end;

class function TdxPDFLineFormatter.IsBeginWordSymbol(C: Char): Boolean;
begin
  Result := (C = '(') or (C = '[') or (C = '{') or (C = #10#13) or (C = ' ');
end;

class function TdxPDFLineFormatter.IsEndWordSymbol(C: Char): Boolean;
begin
  Result := (C = ')') or (C = ']') or (C = '}') or (C = '?') or (C = '!') or (C = #10#13);
end;

class function TdxPDFLineFormatter.CanBreak(C: Char): Boolean;
begin
  Result := (C = #9) or (C = '%');
end;

class function TdxPDFLineFormatter.CanBreak(APrevious: Char; ANext: Char): Boolean;
begin
  Result := not (IsBeginWordSymbol(APrevious) or IsEndWordSymbol(ANext));
  if Result then
    Result := CanBreak(APrevious) or CanBreak(ANext) or IsBeginWordSymbol(ANext) or IsEndWordSymbol(APrevious);
end;

function TdxPDFLineFormatter.GetHyphenRun: TdxPDFGlyphRun;
begin
  Result := FHyphenRun;
end;

function TdxPDFLineFormatter.GetIsCurrentLineEmpty: Boolean;
begin
  Result := (FCurrentLine = nil) or FCurrentLine.Empty;
end;

function TdxPDFLineFormatter.GetCurrentLineGlyphCount: Integer;
begin
  Result := 0;
  if FCurrentLine <> nil then
    Result := FCurrentLine.Glyphs.Count;
end;

function TdxPDFLineFormatter.GetCurrentLineWidth: Double;
begin
  Result := 0;
  if FCurrentLine <> nil then
    Result := FCurrentLine.Width;
end;

function TdxPDFLineFormatter.GetLines: TdxPDFGlyphRunList;
var
  ALastIndex: Integer;
begin
  ALastIndex := FLines.Count - 1;
  if (ALastIndex < 0) or not FLines[ALastIndex].Empty then
    Result := FLines
  else
  begin
    Result := TdxPDFGlyphRunList.Create(FLines);
    Result.Delete(ALastIndex);
  end;
end;

procedure TdxPDFLineFormatter.AddGlyph(const AGlyph: TdxPDFGlyph);
begin
  EnsureCurrentLine;
  FCurrentLine.Append(AGlyph);
end;

procedure TdxPDFLineFormatter.AddWord(const AWord: TdxPDFGlyphRun);
begin
  EnsureCurrentLine;
  FCurrentLine.Append(AWord);
end;

procedure TdxPDFLineFormatter.RemoveLastGlyph;
begin
  FCurrentLine.RemoveLast;
end;

procedure TdxPDFLineFormatter.FormatLine(const ALine: string; AFlags: TdxPDFGlyphMappingFlags);
var
  ALastSubstringEndIndex, ALength, ATabCount, I: Integer;
  AWord: TdxPDFFormatterWord;
  ALastWord: string;
begin
  FFlags := AFlags;
  EnsureCurrentLine;
  ALastSubstringEndIndex := 0;
  ALength := Length(ALine);
  ATabCount := 0;
  for I := 0 to ALength - 2 do
  begin
    if CanBreak(ALine[I + 1], ALine[I + 2]) then
    begin
      if ALine[I] = #9 then
      begin
        Inc(ATabCount);
        Inc(ALastSubstringEndIndex);
        Continue;
      end;
      AWord := MapString(TdxStringHelper.Substring(ALine, ALastSubstringEndIndex, I - ALastSubstringEndIndex + 1));
      try
        ApplyTabStops(AWord, ATabCount);
        ATabCount := 0;
        if AppendWord(AWord) then
        begin
          FinishLine(True);
          Exit;
        end;
        ALastSubstringEndIndex := I + 1;
      finally
        AWord.Glyphs.Free;
      end;
    end;
  end;
  ALastWord := TdxStringHelper.TrimEnd(TdxStringHelper.Substring(ALine, ALastSubstringEndIndex,
    ALength - ALastSubstringEndIndex), [' ']);
  if Length(ALastWord) > 0 then
  begin
    AWord := MapString(ALastWord);
    try
      ApplyTabStops(AWord, ATabCount);
      AppendWord(AWord);
    finally
      AWord.Glyphs.Free;
    end;
  end;
  FinishLine(True);
end;

procedure TdxPDFLineFormatter.Clear;
begin
  FLines.Clear;
  FCurrentLine := nil;
end;

function TdxPDFLineFormatter.GetWordGlyphs(AActualWord: TdxPDFGlyphRun): TdxPDFGlyphList;
begin
  Result := AActualWord.Glyphs;
end;

procedure TdxPDFLineFormatter.EnsureCurrentLine;
begin
  if FCurrentLine = nil then
    FCurrentLine := FFontData.CreateGlyphRun;
end;

function TdxPDFLineFormatter.MapString(const AStr: string): TdxPDFFormatterWord;
var
  S: string;
  AEndsWithSoftHyphen: Boolean;
begin
  AEndsWithSoftHyphen := TdxStringHelper.EndsWith(AStr, FSoftHyphenString);
  S := AStr;
  if AEndsWithSoftHyphen then
    S := TdxStringHelper.TrimEnd(AStr, FSoftHyphenChar);
  Result := TdxPDFFormatterWord.Create(S, FFontData.ProcessString(AStr, FFlags), AEndsWithSoftHyphen);
end;


function TdxPDFLineFormatter.AppendWord(AActualWord: TdxPDFFormatterWord): Boolean;
var
  AGlyph: TdxPDFGlyph;
begin
  Result := False;
  FShouldAppendHyphen := AActualWord.EndsWithSoftHyphen;
  if (FLines.Count > 0) and IsCurrentLineEmpty then
    AActualWord := MapString(AActualWord.TextWithoutLeadingSpaces);
  if (FTrimmingAlgorithm <> nil) and (FNoWrap or (FLines.Count = FLayoutLineCount - 1)) then
    Result := FTrimmingAlgorithm.ProcessWord(AActualWord.Glyphs)
  else
    if FNoWrap then
      AddWord(AActualWord.Glyphs)
    else
      if IfThen(AActualWord.EndsWithSoftHyphen, AActualWord.Width + HyphenRun.Width, AActualWord.Width) + CurrentLineWidth <= FLayoutWidth then
        AddWord(AActualWord.Glyphs)
      else
        if not IsCurrentLineEmpty then
        begin
          FinishLine(False);
          AppendWord(AActualWord);
        end
        else
          for AGlyph in GetWordGlyphs(AActualWord.Glyphs) do
            if CurrentLineWidth + AGlyph.Width <= FLayoutWidth then
              AddGlyph(AGlyph)
            else
              if (AGlyph.Width > FLayoutWidth) and IsCurrentLineEmpty then
              begin
                AddGlyph(AGlyph);
                FinishLine(False);
              end
              else
              begin
                FinishLine(False);
                AddGlyph(AGlyph);
              end;
end;

procedure TdxPDFLineFormatter.FinishLine(AIgnoreHyphen: Boolean);
begin
  if FCurrentLine <> nil then
  begin
    if FShouldAppendHyphen and not AIgnoreHyphen then
      FCurrentLine.Append(HyphenRun);
    FShouldAppendHyphen := False;
    FLines.Add(FCurrentLine);
    FCurrentLine := FFontData.CreateGlyphRun;
  end;
end;

procedure TdxPDFLineFormatter.ApplyTabStops(AWord: TdxPDFFormatterWord; ATabCount: Integer);
var
  AGlyph: TdxPDFGlyph;
begin
  if (FEmTabStopInterval <> 0) and (ATabCount <> 0) and (AWord.Glyphs.Glyphs.Count <> 0) then
  begin
    AGlyph := AWord.Glyphs.Glyphs[0];
    AWord.Glyphs.Glyphs[0] := TdxPDFGlyph.Create(AGlyph.Index, AGlyph.Width,
      FCurrentLine.Width - (Ceil(FCurrentLine.Width / FEmTabStopInterval) + ATabCount - 1) * FEmTabStopInterval);
  end;
end;

{ TdxPDFRTLLineFormatter }

constructor TdxPDFRTLLineFormatter.Create(ALayoutWidth: Double; ASpaceGlyphIndex, ALayoutLineCount: Integer;
  const AFormat: TdxPDFStringFormat; AEllipsis: TdxPDFGlyphRun; AFontData: TdxPDFEditableFontData; AEmTabStopInterval: Double);
begin
  inherited Create(ALayoutWidth, ALayoutLineCount, AFormat, AEllipsis, AFontData, AEmTabStopInterval);
  FSpaceGlyphIndex := ASpaceGlyphIndex;
end;

procedure TdxPDFRTLLineFormatter.AddGlyph(const AGlyph: TdxPDFGlyph);
begin
  CurrentLine.Prepend(AGlyph);
end;

procedure TdxPDFRTLLineFormatter.AddWord(const AWord: TdxPDFGlyphRun);
begin
  CurrentLine.Prepend(AWord, FSpaceGlyphIndex);
end;

function TdxPDFRTLLineFormatter.GetWordGlyphs(AActualWord: TdxPDFGlyphRun): TdxPDFGlyphList;
var
  I: Integer;
begin
  Result := TdxPDFGlyphList.Create;
  for I := AActualWord.Glyphs.Count - 1 downto 0 do
    Result.Add(AActualWord.Glyphs[I]);
end;

end.
