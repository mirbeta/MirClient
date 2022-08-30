{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressGaugeControl                                      }
{                                                                    }
{           Copyright (c) 2013-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSGAUGECONTROL AND ALL           }
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

unit dxGaugeDigitalScale;

{$I cxVer.inc}

interface

uses
  Types, Graphics, Classes, Generics.Defaults, Generics.Collections, cxGeometry, dxCoreGraphics, cxGraphics,
  dxGDIPlusClasses, dxXMLDoc, dxCompositeShape, dxGaugeCustomScale;

const
  dxGaugeDigitalScaleDefaultDigitCount = 5;
  dxGaugeDigitalScaleDefaultDigitSpacingFactor = 0;

type
  TdxGaugeCustomDigitalScale = class;
  TdxGaugeDigitalScaleDisplayMode = (sdmFourteenSegment, sdmSevenSegment, sdmMatrix5x8Dots, sdmMatrix5x8Squares,
    sdmMatrix8x14Dots, sdmMatrix8x14Squares);
  TdxGaugeDigitalScaleCustomSection = class;
  TdxGaugeDigitalScaleCustomSectionClass = class of TdxGaugeDigitalScaleCustomSection;
  TdxGaugeDigitalScaleCustomSectionsController = class;
  TdxGaugeDigitalScaleCustomSectionsControllerClass = class of TdxGaugeDigitalScaleCustomSectionsController;
  TdxGaugeDigitalScaleCustomSectionCalculator = class;
  TdxGaugeDigitalScaleCustomSectionCalculatorClass = class of TdxGaugeDigitalScaleCustomSectionCalculator;

  { TdxGaugeDigitalScaleCustomSection }

  TdxGaugeDigitalScaleCustomSection = class
  public
    Bounds: TdxRectF;
  end;

  { TdxGaugeDigitalScaleSection }

  TdxGaugeDigitalScaleSection = class(TdxGaugeDigitalScaleCustomSection)
  public
    Mask: Integer;
  end;

  { TdxGaugeDigitalScaleMatrixSection }

  TdxGaugeDigitalScaleMatrixSection = class(TdxGaugeDigitalScaleCustomSection)
  public
    Flags: TArray<Boolean>;
  end;

  { TdxGaugeDigitalScaleDefaultParameters }

  TdxGaugeDigitalScaleDefaultParameters = class(TdxGaugeCustomScaleDefaultParameters)
  public
    SegmentColorOff: TdxAlphaColor;
    SegmentColorOn: TdxAlphaColor;
  end;

  { TdxGaugeDigitalScaleParameters }

  TdxGaugeDigitalScaleParameters = class(TdxGaugeCustomScaleParameters)
  public
    DigitCount: Integer;
    DigitSpacingFactor: Single;
    DisplayMode: TdxGaugeDigitalScaleDisplayMode;
    SegmentColorOff: TdxAlphaColor;
    SegmentColorOn: TdxAlphaColor;
  end;

  { TdxGaugeDigitalScaleOptionsView }

  TdxGaugeDigitalScaleOptionsView = class(TdxGaugeCustomScaleOptionsView)
  private
    function GetDigitCount: Integer;
    function GetDigitSpacingFactor: Single;
    function GetDisplayMode: TdxGaugeDigitalScaleDisplayMode;
    function GetSegmentColorOff: TdxAlphaColor;
    function GetSegmentColorOn: TdxAlphaColor;
    procedure SetDigitCount(const AValue: Integer);
    procedure SetDigitSpacingFactor(const AValue: Single);
    procedure SetDisplayMode(const AValue: TdxGaugeDigitalScaleDisplayMode);
    procedure SetSegmentColorOff(const AValue: TdxAlphaColor);
    procedure SetSegmentColorOn(const AValue: TdxAlphaColor);

    function GetScale: TdxGaugeCustomDigitalScale;
    function IsSegmentColorOffStored: Boolean;
    function IsSegmentColorOnStored: Boolean;
  published
    property DigitCount: Integer read GetDigitCount write SetDigitCount default dxGaugeDigitalScaleDefaultDigitCount;
    property DigitSpacingFactor: Single read GetDigitSpacingFactor write SetDigitSpacingFactor;
    property DisplayMode: TdxGaugeDigitalScaleDisplayMode read GetDisplayMode write SetDisplayMode
      default sdmFourteenSegment;
    property SegmentColorOff: TdxAlphaColor read GetSegmentColorOff write SetSegmentColorOff stored IsSegmentColorOffStored
      default dxacDefault;
    property SegmentColorOn: TdxAlphaColor read GetSegmentColorOn write SetSegmentColorOn stored IsSegmentColorOnStored
      default dxacDefault;
  end;

  { TdxGaugeDigitalScaleViewInfo }

  TdxGaugeDigitalScaleViewInfo = class(TdxGaugeCustomScaleViewInfo)
  private
    FSectionsController: TdxGaugeDigitalScaleCustomSectionsController;

    FBackgroundLeftPart: TdxCompositeShape;
    FBackgroundMiddlePart: TdxCompositeShape;
    FBackgroundRightPart: TdxCompositeShape;

    FBackgroundLeftPartBounds: TdxRectF;
    FBackgroundMiddlePartBounds: TdxRectF;
    FBackgroundRightPartBounds: TdxRectF;

    function GetSectionsControllerClass(ADisplayMode: TdxGaugeDigitalScaleDisplayMode):
      TdxGaugeDigitalScaleCustomSectionsControllerClass;

    function GetDefaultParameters: TdxGaugeDigitalScaleDefaultParameters;
    function GetParameters: TdxGaugeDigitalScaleParameters;
    function GetRealDigitCount: Integer;
    function GetSegmentColorOff: TdxAlphaColor;
    function GetSegmentColorOn: TdxAlphaColor;
    procedure CalculateBackground(const AScaleFactor: TdxPointF);
    procedure CalculateProportionalBackground(const AScaleFactor: TdxPointF);
    procedure CalculateSections;
    procedure CalculateStretchableBackground(const AScaleFactor: TdxPointF);
    procedure CreateSectionsController(ADisplayMode: TdxGaugeDigitalScaleDisplayMode);
    procedure DrawSections(AGPGraphics: TdxGPGraphics);
    procedure RecreateSectionController(ADisplayMode: TdxGaugeDigitalScaleDisplayMode);
  protected
    function GetParametersClass: TdxGaugeCustomScaleParametersClass; override;

    function CanDrawBackground: Boolean; override;
    function GetContentBounds: TdxRectF; override;
    function GetContentOriginalSize: TdxSizeF; override;
    procedure CalculateContent; override;
    procedure CalculateScaleFactor; override;
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;
    procedure DoDrawBackground(AGPGraphics: TdxGPGraphics); override;
    procedure DoDrawLayer(AGPGraphics: TdxGPGraphics; ALayerIndex: Integer); override;
    procedure LoadScaleElements; override;
  end;

  { TdxGaugeCustomDigitalScale }

  TdxGaugeCustomDigitalScale = class(TdxGaugeCustomScale)
  private
    function GetDigitCount: Integer;
    function GetDigitSpacingFactor: Single;
    function GetDisplayMode: TdxGaugeDigitalScaleDisplayMode;
    function GetOptionsLayout: TdxGaugeScaleOptionsRectangularLayout;
    function GetOptionsView: TdxGaugeDigitalScaleOptionsView;
    function GetSegmentColorOff: TdxAlphaColor;
    function GetSegmentColorOn: TdxAlphaColor;
    procedure SetDigitCount(const AValue: Integer);
    procedure SetDigitSpacingFactor(const AValue: Single);
    procedure SetDisplayMode(const AValue: TdxGaugeDigitalScaleDisplayMode);
    procedure SetOptionsLayout(const AValue: TdxGaugeScaleOptionsRectangularLayout);
    procedure SetOptionsView(const AValue: TdxGaugeDigitalScaleOptionsView);
    procedure SetSegmentColorOff(const AValue: TdxAlphaColor);
    procedure SetSegmentColorOn(const AValue: TdxAlphaColor);
    procedure SetValue(const AValue: string);

    function GetParameters: TdxGaugeDigitalScaleParameters;
    function GetViewInfo: TdxGaugeDigitalScaleViewInfo;
    function IsDefaultColor(const AColor: TdxAlphaColor): Boolean;

    function IsSegmentColorOffStored: Boolean;
    function IsSegmentColorOnStored: Boolean;
    function IsValueStored: Boolean;
  protected
    class function GetLayerCount: Integer; override;
    class function GetScaleName: string; override;
    class function GetScaleType: TdxGaugeScaleType; override;
    class function GetStyleReaderClass: TdxGaugeCustomScaleStyleReaderClass; override;

    function GetCaptionClass: TdxGaugeCustomCaptionClass; override;
    function GetOptionsLayoutClass: TdxGaugeScaleOptionsCustomLayoutClass; override;
    function GetOptionsViewClass: TdxGaugeCustomScaleOptionsViewClass; override;
    function GetParametersClass: TdxGaugeCustomScaleParametersClass; override;
    function GetViewInfoClass: TdxGaugeCustomScaleViewInfoClass; override;

    function GetValueChangedLayers: TdxGaugeScaleChangedLayers; override;
    procedure DoAssign(AScale: TdxGaugeCustomScale); override;
    procedure InitParameters; override;
    procedure InternalRestoreStyleParameters; override;

    function GetValue: string;

    property OptionsLayout: TdxGaugeScaleOptionsRectangularLayout read GetOptionsLayout write SetOptionsLayout;
    property OptionsView: TdxGaugeDigitalScaleOptionsView read GetOptionsView write SetOptionsView;
    property Value: string read GetValue write SetValue stored IsValueStored;
  end;

  { TdxGaugeDigitalScale }

  TdxGaugeDigitalScale = class(TdxGaugeCustomDigitalScale)
  published
    property AnchorScaleIndex;
    property Captions;
    property OptionsLayout;
    property OptionsView;
    property StyleName;
    property Value;
    property Visible;
  end;

  { TdxGaugeCustomDigitalScaleStyleReader }

  TdxGaugeCustomDigitalScaleStyleReader = class(TdxGaugeCustomScaleStyleReader)
  protected
    class function GetResourceNamePrefix: string; override;
    function GetDefaultParametersClass: TdxGaugeCustomScaleDefaultParametersClass; override;
    procedure ReadParameters(ANode: TdxXMLNode; AParameters: TdxGaugeCustomScaleDefaultParameters); override;
  end;

  { TdxGaugeDigitalScaleCustomSectionCalculator }

  TdxGaugeDigitalScaleCustomSectionCalculator = class
  private
    FCharset: TDictionary<Char, Integer>;

    function GetCurrentChar(const AText: string; AIndex: Integer): Char;
    function GetPreviousChar(const AText: string; AIndex: Integer): Char;
    function GetNextChar(const AText: string; AIndex: Integer): Char;
    function IsBottomPoint(AChar: Char): Boolean;
    function IsColon(AChar: Char): Boolean;
    function IsEmpty(AChar: Char): Boolean;
    function IsPointOrColon(AChar: Char): Boolean;
    function SetBitState(const AFlags: Integer; AIndex: Integer): Integer;
  protected
    function AcceptChar(APreviousChar, ACurrentChar, ANextChar: Char): Boolean; virtual;
    function GetMask(APreviousChar, ACurrentChar, ANextChar: Char; var ACharIndex: Integer): Integer; virtual;
    function IsNonZeroLengthSymbol(APreviousChar, ACurrentChar, ANextChar: Char): Boolean; virtual;
    function IsPoint(AChar: Char): Boolean; virtual;
    function IsTopPoint(AChar: Char): Boolean; virtual;
    procedure AppendColonLeftPartMask(var AMask: Integer); virtual; abstract;
    procedure AppendColonRightPartMask(var AMask: Integer); virtual; abstract;
    procedure AppendBottomPointMask(var AMask: Integer); virtual; abstract;
    procedure AppendTopPointMask(var AMask: Integer); virtual; abstract;
    procedure InitCharset; virtual;

    procedure Calculate(const AText: string; ASections: TObjectList<TdxGaugeDigitalScaleCustomSection>); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;

  { TdxGaugeDigitalScaleSevenSegmentSectionCalculator }

  TdxGaugeDigitalScaleSevenSegmentSectionCalculator = class(TdxGaugeDigitalScaleCustomSectionCalculator)
  protected
    procedure AppendColonLeftPartMask(var AMask: Integer); override;
    procedure AppendColonRightPartMask(var AMask: Integer); override;
    procedure AppendBottomPointMask(var AMask: Integer); override;
    procedure AppendTopPointMask(var AMask: Integer); override;
    procedure InitCharset; override;
  end;

  { TdxGaugeDigitalScaleFourteenSegmentSectionCalculator }

  TdxGaugeDigitalScaleFourteenSegmentSectionCalculator = class(TdxGaugeDigitalScaleCustomSectionCalculator)
  protected
    function AcceptChar(APreviousChar, ACurrentChar, ANextChar: Char): Boolean; override;
    function GetMask(APreviousChar, ACurrentChar, ANextChar: Char; var ACharIndex: Integer): Integer; override;
    function IsNonZeroLengthSymbol(APreviousChar, ACurrentChar, ANextChar: Char): Boolean; override;
    function IsPoint(AChar: Char): Boolean; override;
    function IsTopPoint(AChar: Char): Boolean; override;
    procedure AppendColonLeftPartMask(var AMask: Integer); override;
    procedure AppendColonRightPartMask(var AMask: Integer); override;
    procedure AppendBottomPointMask(var AMask: Integer); override;
    procedure AppendTopPointMask(var AMask: Integer); override;
    procedure InitCharset; override;
  end;

  { TdxGaugeDigitalScaleCustomMatrixSectionCalculator }

  TdxGaugeDigitalScaleCustomMatrixSectionCalculator = class(TdxGaugeDigitalScaleCustomSectionCalculator)
  private
    FCache: TDictionary<Char, TArray<Boolean>>;
    FGpFont: TdxGPFont;
    FGpFontFamily: TdxGPFontFamily;
    FGpFormat: TdxGPStringFormat;
    FGpBrush: TdxGPBrush;

    function GetCharColors(AChar: Char): TRGBColors;
    procedure DrawChar(AChar: Char; ABitmap: TcxBitmap32);
    procedure ResetSectionDots(ASections: TObjectList<TdxGaugeDigitalScaleCustomSection>);
  protected
    function GetCharOffset: Integer; virtual; abstract;
    function GetFontSize: Integer; virtual; abstract;
    function GetFontStyle: TdxGPFontStyle; virtual; abstract;
    function GetMatrixSize: TSize; virtual; abstract;
    procedure Calculate(const AText: string; ASections: TObjectList<TdxGaugeDigitalScaleCustomSection>); override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  { TdxGaugeDigitalScaleMatrix5x8SegmentSectionCalculator }

  TdxGaugeDigitalScaleMatrix5x8SegmentSectionCalculator = class(TdxGaugeDigitalScaleCustomMatrixSectionCalculator)
  protected
    function GetCharOffset: Integer; override;
    function GetFontSize: Integer; override;
    function GetFontStyle: TdxGPFontStyle; override;
    function GetMatrixSize: TSize; override;
  end;

  { TdxGaugeDigitalScaleMatrix8x14SegmentSectionCalculator }

  TdxGaugeDigitalScaleMatrix8x14SegmentSectionCalculator = class(TdxGaugeDigitalScaleCustomMatrixSectionCalculator)
  protected
    function GetCharOffset: Integer; override;
    function GetFontSize: Integer; override;
    function GetFontStyle: TdxGPFontStyle; override;
    function GetMatrixSize: TSize; override;
  end;

  { TdxGaugeDigitalScaleCustomSectionsController }

  TdxGaugeDigitalScaleCustomSectionsController = class
  private
    FDigit: TdxCompositeShape;
    FDisplayMode: TdxGaugeDigitalScaleDisplayMode;
    FSections: TObjectList<TdxGaugeDigitalScaleCustomSection>;
    FSectionCalculator: TdxGaugeDigitalScaleCustomSectionCalculator;

    function GetBitState(const AFlags: Integer; AIndex: Integer): Boolean;
    function GetDigitResourceNameSuffix: string;
    function GetDigitSize: TdxSizeF;
    function GetOffset: Single;
    function GetResourceName: string;
    function GetSectionIndent(const ASectionSize: TdxSizeF; AIndentFactor: Single): TdxRectF;
    function GetSectionSize: TdxSizeF;
    procedure LoadDigit;
    procedure PrepareSections(ASectionCount: Integer);
  protected
    function GetSectionClass: TdxGaugeDigitalScaleCustomSectionClass; virtual;
    function GetCalculatorClass: TdxGaugeDigitalScaleCustomSectionCalculatorClass; virtual; abstract;

    function GetDigitResourceName: string; virtual; abstract;
    function GetSegmentCount: Integer; virtual; abstract;
    function GetSegmentName(ASegmentIndex: Integer): string; virtual; abstract;

    function GetBackgroundOriginalSize(ASectionCount: Integer): TdxSizeF;
    function GetBackgroundPartWidth(const AScaleFactor: TdxPointF): Single;
    procedure Calculate(AValue: Variant; const ABounds: TdxRectF; ASectionCount: Integer;
      ASectionIndentFactor: Single; AStretch: Boolean);
    procedure CalculateSectionsBounds(const ABounds: TdxRectF; ASectionsIndent: Single; AStretch: Boolean);
    procedure CalculateSectionsMasks(const AScaleTextValue: string);
    procedure ColorizeDigit(const ASectionMask: Integer; AColorOn, AColorOff: TdxAlphaColor);
    procedure ColorizeSection(ASectionIndex: Integer; AColorOn, AColorOff: TdxAlphaColor); virtual;
    procedure DrawSections(AGPGraphics: TdxGPGraphics; AColorOn, AColorOff: TdxAlphaColor);
  public
    constructor Create(ADisplayMode: TdxGaugeDigitalScaleDisplayMode);
    destructor Destroy; override;
  end;

  { TdxGaugeDigitalScaleSevenSegmentSectionsController }

  TdxGaugeDigitalScaleSevenSegmentSectionsController = class(TdxGaugeDigitalScaleCustomSectionsController)
  protected
    function GetCalculatorClass: TdxGaugeDigitalScaleCustomSectionCalculatorClass; override;
    function GetDigitResourceName: string; override;
    function GetSegmentCount: Integer; override;
    function GetSegmentName(ASegmentIndex: Integer): string; override;
  end;

  { TdxGaugeDigitalScaleFourteenSegmentSectionsController }

  TdxGaugeDigitalScaleFourteenSegmentSectionsController = class(TdxGaugeDigitalScaleCustomSectionsController)
  protected
    function GetCalculatorClass: TdxGaugeDigitalScaleCustomSectionCalculatorClass; override;
    function GetDigitResourceName: string; override;
    function GetSegmentCount: Integer; override;
    function GetSegmentName(ASegmentIndex: Integer): string; override;
  end;

  { TdxGaugeDigitalScaleCustomMatrixSectionsController }

  TdxGaugeDigitalScaleCustomMatrixSectionsController = class(TdxGaugeDigitalScaleCustomSectionsController)
  protected
    function GetMatrixSize: TSize; virtual; abstract;
    function GetCalculatorClass: TdxGaugeDigitalScaleCustomSectionCalculatorClass; override;
    function GetSectionClass: TdxGaugeDigitalScaleCustomSectionClass; override;
    function GetSegmentCount: Integer; override;
    function GetSegmentName(ASegmentIndex: Integer): string; override;
    procedure ColorizeSection(ASectionIndex: Integer; AColorOn, AColorOff: TdxAlphaColor); override;
  end;

  { TdxGaugeDigitalScaleMatrix5x8SegmentSectionsController }

  TdxGaugeDigitalScaleMatrix5x8SegmentSectionsController = class(TdxGaugeDigitalScaleCustomMatrixSectionsController)
  protected
    function GetCalculatorClass: TdxGaugeDigitalScaleCustomSectionCalculatorClass; override;
    function GetDigitResourceName: string; override;
    function GetMatrixSize: TSize; override;
  end;

  { TdxGaugeDigitalScaleMatrix8x14SegmentSectionsController }

  TdxGaugeDigitalScaleMatrix8x14SegmentSectionsController = class(TdxGaugeDigitalScaleCustomMatrixSectionsController)
  protected
    function GetCalculatorClass: TdxGaugeDigitalScaleCustomSectionCalculatorClass; override;
    function GetDigitResourceName: string; override;
    function GetMatrixSize: TSize; override;
  end;

  { TdxGaugeDigitalScaleCaption }

  TdxGaugeDigitalScaleCaption = class(TdxGaugeCustomCaption)
  published
    property OptionsLayout;
    property OptionsView;
    property Text;
    property Visible;
  end;

implementation

uses
  Windows, SysUtils, Math, dxCore, dxGDIPlusAPI, dxGaugeUtils;

{$R DigitalSegments.res}

const
  dxGaugeDigitalScaleDefaultValue = '0';
  dxGaugeDigitalScaleDigitOffset = 0.35;
  dxGaugeDigitalScaleMaxDigitCount = 255;

type
  TdxCompositeShapeAccess = class(TdxCompositeShape);
  TdxGaugeScaleStyleAccess = class(TdxGaugeScaleStyle);
  TdxGaugeCustomScaleViewInfoAccess = class(TdxGaugeCustomScaleViewInfo);

{ TdxGaugeDigitalScaleOptionsView }

function TdxGaugeDigitalScaleOptionsView.GetDigitCount: Integer;
begin
  Result := GetScale.GetDigitCount;
end;

function TdxGaugeDigitalScaleOptionsView.GetDisplayMode: TdxGaugeDigitalScaleDisplayMode;
begin
  Result := GetScale.GetDisplayMode;
end;

function TdxGaugeDigitalScaleOptionsView.GetDigitSpacingFactor: Single;
begin
  Result := GetScale.GetDigitSpacingFactor;
end;

function TdxGaugeDigitalScaleOptionsView.GetSegmentColorOff: TdxAlphaColor;
begin
  Result := GetScale.GetSegmentColorOff;
end;

function TdxGaugeDigitalScaleOptionsView.GetSegmentColorOn: TdxAlphaColor;
begin
  Result := GetScale.GetSegmentColorOn;
end;

procedure TdxGaugeDigitalScaleOptionsView.SetDigitCount(const AValue: Integer);
begin
  GetScale.SetDigitCount(AValue);
end;

procedure TdxGaugeDigitalScaleOptionsView.SetDigitSpacingFactor(const AValue: Single);
begin
  GetScale.SetDigitSpacingFactor(AValue);
end;

procedure TdxGaugeDigitalScaleOptionsView.SetDisplayMode(const AValue: TdxGaugeDigitalScaleDisplayMode);
begin
  GetScale.SetDisplayMode(AValue);
end;

procedure TdxGaugeDigitalScaleOptionsView.SetSegmentColorOff(const AValue: TdxAlphaColor);
begin
  GetScale.SetSegmentColorOff(AValue);
end;

procedure TdxGaugeDigitalScaleOptionsView.SetSegmentColorOn(const AValue: TdxAlphaColor);
begin
  GetScale.SetSegmentColorOn(AValue);
end;

function TdxGaugeDigitalScaleOptionsView.GetScale: TdxGaugeCustomDigitalScale;
begin
  Result := inherited GetScale as TdxGaugeCustomDigitalScale;
end;

function TdxGaugeDigitalScaleOptionsView.IsSegmentColorOffStored: Boolean;
begin
  Result := GetScale.IsSegmentColorOffStored;
end;

function TdxGaugeDigitalScaleOptionsView.IsSegmentColorOnStored: Boolean;
begin
  Result := GetScale.IsSegmentColorOnStored;
end;

{ TdxGaugeDigitalScaleViewInfo }

function TdxGaugeDigitalScaleViewInfo.GetParametersClass: TdxGaugeCustomScaleParametersClass;
begin
  Result := TdxGaugeDigitalScaleParameters;
end;

function TdxGaugeDigitalScaleViewInfo.CanDrawBackground: Boolean;
begin
  Result := CanDrawImage(FBackgroundLeftPart) or CanDrawImage(FBackgroundMiddlePart) or
    CanDrawImage(FBackgroundRightPart);
end;

function TdxGaugeDigitalScaleViewInfo.GetContentBounds: TdxRectF;
begin
  Result := cxRectUnion(cxRectUnion(FBackgroundLeftPartBounds, FBackgroundMiddlePartBounds), FBackgroundRightPartBounds);
end;

function TdxGaugeDigitalScaleViewInfo.GetContentOriginalSize: TdxSizeF;
begin
  Result := FSectionsController.GetBackgroundOriginalSize(GetRealDigitCount);
end;

procedure TdxGaugeDigitalScaleViewInfo.CalculateContent;
begin
  inherited CalculateContent;
  CalculateBackground(cxPointF(1, 1));
  CalculateSections;
end;

procedure TdxGaugeDigitalScaleViewInfo.CalculateScaleFactor;
var
  ARatio: Single;
begin
  inherited CalculateScaleFactor;
  CalculateBackground(FScaleFactor);
  if GetContentBounds.Width > ScaleInfo.Bounds.Width then
  begin
    ARatio := ScaleInfo.Bounds.Width / GetContentBounds.Width;
    FScaleFactor.X := FScaleFactor.X * ARatio;
    FScaleFactor.Y := FScaleFactor.Y * ARatio;
  end;
end;

procedure TdxGaugeDigitalScaleViewInfo.CreateSubClasses;
begin
  inherited CreateSubClasses;
  CreateSectionsController(GetParameters.DisplayMode);
end;

procedure TdxGaugeDigitalScaleViewInfo.DestroySubClasses;
begin
  FreeAndNil(FSectionsController);
  inherited DestroySubClasses;
end;

procedure TdxGaugeDigitalScaleViewInfo.DoDrawBackground(AGPGraphics: TdxGPGraphics);
begin
  dxGaugeDrawImage(AGPGraphics, FBackgroundLeftPart, FBackgroundLeftPartBounds);
  dxGaugeDrawImage(AGPGraphics, FBackgroundMiddlePart, FBackgroundMiddlePartBounds);
  dxGaugeDrawImage(AGPGraphics, FBackgroundRightPart, FBackgroundRightPartBounds);
end;

procedure TdxGaugeDigitalScaleViewInfo.DoDrawLayer(AGPGraphics: TdxGPGraphics; ALayerIndex: Integer);
begin
  inherited DoDrawLayer(AGPGraphics, ALayerIndex);
  if ALayerIndex = 2 then
    DrawSections(AGPGraphics);
end;

procedure TdxGaugeDigitalScaleViewInfo.LoadScaleElements;
begin
  inherited LoadScaleElements;
  FBackgroundLeftPart := TdxGaugeScaleStyleAccess(Style).GetElement(etDigitalBackgroundStart);
  FBackgroundMiddlePart := TdxGaugeScaleStyleAccess(Style).GetElement(etDigitalBackgroundMiddle);
  FBackgroundRightPart := TdxGaugeScaleStyleAccess(Style).GetElement(etDigitalBackgroundEnd);
end;

function TdxGaugeDigitalScaleViewInfo.GetSectionsControllerClass(ADisplayMode: TdxGaugeDigitalScaleDisplayMode):
  TdxGaugeDigitalScaleCustomSectionsControllerClass;
begin
  case ADisplayMode of
    sdmSevenSegment:
      Result := TdxGaugeDigitalScaleSevenSegmentSectionsController;
    sdmFourteenSegment:
      Result := TdxGaugeDigitalScaleFourteenSegmentSectionsController;
    sdmMatrix5x8Dots, sdmMatrix5x8Squares:
      Result := TdxGaugeDigitalScaleMatrix5x8SegmentSectionsController;
    sdmMatrix8x14Dots, sdmMatrix8x14Squares:
      Result := TdxGaugeDigitalScaleMatrix8x14SegmentSectionsController;
  else
    Result := nil;
  end;
end;

function TdxGaugeDigitalScaleViewInfo.GetDefaultParameters: TdxGaugeDigitalScaleDefaultParameters;
begin
  Result := DefaultParameters as TdxGaugeDigitalScaleDefaultParameters;
end;

function TdxGaugeDigitalScaleViewInfo.GetParameters: TdxGaugeDigitalScaleParameters;
begin
  Result := FParameters as TdxGaugeDigitalScaleParameters;
end;

function TdxGaugeDigitalScaleViewInfo.GetRealDigitCount: Integer;

  function GetText: string;
  begin
    Result := dxVariantToString(GetParameters.Value);
    if Length(Result) = 0 then
      Result := ' ';
  end;

var
  AScaleParameters: TdxGaugeDigitalScaleParameters;
begin
  AScaleParameters := GetParameters;
  if AScaleParameters.DigitCount = 0 then
    Result := Min(Length(GetText), dxGaugeDigitalScaleMaxDigitCount)
  else
    Result := AScaleParameters.DigitCount;
end;

function TdxGaugeDigitalScaleViewInfo.GetSegmentColorOff: TdxAlphaColor;
var
  AScaleParameters: TdxGaugeDigitalScaleParameters;
begin
  AScaleParameters := GetParameters;
  if AScaleParameters.SegmentColorOff = dxacDefault then
    Result := GetDefaultParameters.SegmentColorOff
  else
    Result := AScaleParameters.SegmentColorOff;
end;

function TdxGaugeDigitalScaleViewInfo.GetSegmentColorOn: TdxAlphaColor;
var
  AScaleParameters: TdxGaugeDigitalScaleParameters;
begin
  AScaleParameters := GetParameters;
  if AScaleParameters.SegmentColorOn = dxacDefault then
    Result := GetDefaultParameters.SegmentColorOn
  else
    Result := AScaleParameters.SegmentColorOn;
end;

procedure TdxGaugeDigitalScaleViewInfo.CalculateBackground(const AScaleFactor: TdxPointF);
begin
  if not GetParameters.Stretch then
    CalculateProportionalBackground(AScaleFactor)
  else
    CalculateStretchableBackground(AScaleFactor);
end;

procedure TdxGaugeDigitalScaleViewInfo.CalculateProportionalBackground(const AScaleFactor: TdxPointF);

  function GetBackgroundPartOriginalSizeRatio(ABackgroundPart: TdxCompositeShape): Single;
  begin
    Result := TdxCompositeShapeAccess(ABackgroundPart).WidthF / TdxCompositeShapeAccess(ABackgroundPart).HeightF;
  end;

var
  ABounds: TdxRectF;
  ABackgroundPartWidth: Single;
begin
  ABounds := GetScaledRect(GetContentOriginalSize, AScaleFactor);
  ABackgroundPartWidth := FSectionsController.GetBackgroundPartWidth(AScaleFactor);

  FBackgroundMiddlePartBounds := ABounds;
  FBackgroundMiddlePartBounds.Left := FBackgroundMiddlePartBounds.Left + ABackgroundPartWidth;
  FBackgroundMiddlePartBounds.Right := FBackgroundMiddlePartBounds.Right - ABackgroundPartWidth;

  FBackgroundLeftPartBounds := FBackgroundMiddlePartBounds;
  FBackgroundLeftPartBounds.Left := FBackgroundLeftPartBounds.Left - ABounds.Height *
    GetBackgroundPartOriginalSizeRatio(FBackgroundLeftPart);

  FBackgroundLeftPartBounds.Right := FBackgroundMiddlePartBounds.Left + 1;

  FBackgroundRightPartBounds := FBackgroundMiddlePartBounds;
  FBackgroundRightPartBounds.Right := FBackgroundRightPartBounds.Right + ABounds.Height *
    GetBackgroundPartOriginalSizeRatio(FBackgroundRightPart);

  FBackgroundRightPartBounds.Left := FBackgroundMiddlePartBounds.Right - 1;
end;

procedure TdxGaugeDigitalScaleViewInfo.CalculateSections;
begin
  FSectionsController.Calculate(GetParameters.Value, FBackgroundMiddlePartBounds, GetRealDigitCount,
    GetParameters.DigitSpacingFactor, GetParameters.Stretch);
end;

procedure TdxGaugeDigitalScaleViewInfo.CalculateStretchableBackground(const AScaleFactor: TdxPointF);
var
  ABounds: TdxRectF;
  ABackgroundPartWidth: Single;
begin
  ABounds := GetScaledRect(GetContentOriginalSize, AScaleFactor);
  ABackgroundPartWidth := FSectionsController.GetBackgroundPartWidth(AScaleFactor);

  FBackgroundLeftPartBounds := ABounds;
  FBackgroundLeftPartBounds.Right := FBackgroundLeftPartBounds.Left + ABackgroundPartWidth;

  FBackgroundRightPartBounds := ABounds;
  FBackgroundRightPartBounds.Left := FBackgroundRightPartBounds.Right - ABackgroundPartWidth;

  FBackgroundMiddlePartBounds := ABounds;
  FBackgroundMiddlePartBounds.Left := FBackgroundLeftPartBounds.Right - 1;
  FBackgroundMiddlePartBounds.Right := FBackgroundRightPartBounds.Left + 1;
end;

procedure TdxGaugeDigitalScaleViewInfo.CreateSectionsController(ADisplayMode: TdxGaugeDigitalScaleDisplayMode);
begin
  FSectionsController := GetSectionsControllerClass(ADisplayMode).Create(ADisplayMode);
end;

procedure TdxGaugeDigitalScaleViewInfo.DrawSections(AGPGraphics: TdxGPGraphics);
begin
  FSectionsController.DrawSections(AGPGraphics, GetSegmentColorOn, GetSegmentColorOff);
end;

procedure TdxGaugeDigitalScaleViewInfo.RecreateSectionController(ADisplayMode: TdxGaugeDigitalScaleDisplayMode);
begin
  FreeAndNil(FSectionsController);
  CreateSectionsController(ADisplayMode);
end;

{ TdxGaugeCustomDigitalScale }

class function TdxGaugeCustomDigitalScale.GetLayerCount: Integer;
begin
  Result := 3;
end;

class function TdxGaugeCustomDigitalScale.GetScaleName: string;
begin
  Result := 'Digital';
end;

class function TdxGaugeCustomDigitalScale.GetScaleType: TdxGaugeScaleType;
begin
  Result := stDigitalScale;
end;

class function TdxGaugeCustomDigitalScale.GetStyleReaderClass: TdxGaugeCustomScaleStyleReaderClass;
begin
  Result := TdxGaugeCustomDigitalScaleStyleReader;
end;

function TdxGaugeCustomDigitalScale.GetCaptionClass: TdxGaugeCustomCaptionClass;
begin
  Result := TdxGaugeDigitalScaleCaption;
end;

function TdxGaugeCustomDigitalScale.GetOptionsLayoutClass: TdxGaugeScaleOptionsCustomLayoutClass;
begin
  Result := TdxGaugeScaleOptionsRectangularLayout;
end;

function TdxGaugeCustomDigitalScale.GetOptionsViewClass: TdxGaugeCustomScaleOptionsViewClass;
begin
  Result := TdxGaugeDigitalScaleOptionsView;
end;

function TdxGaugeCustomDigitalScale.GetParametersClass: TdxGaugeCustomScaleParametersClass;
begin
  Result := TdxGaugeDigitalScaleParameters;
end;

function TdxGaugeCustomDigitalScale.GetViewInfoClass: TdxGaugeCustomScaleViewInfoClass;
begin
  Result := TdxGaugeDigitalScaleViewInfo;
end;

function TdxGaugeCustomDigitalScale.GetValueChangedLayers: TdxGaugeScaleChangedLayers;
begin
  Result := inherited GetValueChangedLayers;
  if OptionsView.DigitCount = 0 then
    Include(Result, sclStaticLayer);
end;

procedure TdxGaugeCustomDigitalScale.DoAssign(AScale: TdxGaugeCustomScale);
begin
  OptionsView.DisplayMode := (AScale as TdxGaugeCustomDigitalScale).OptionsView.DisplayMode;
  inherited DoAssign(AScale);
end;

procedure TdxGaugeCustomDigitalScale.InitParameters;
begin
  inherited InitParameters;
  GetParameters.Value := dxGaugeDigitalScaleDefaultValue;
  GetParameters.SegmentColorOff := dxacDefault;
  GetParameters.SegmentColorOn := dxacDefault;
  GetParameters.DigitCount := dxGaugeDigitalScaleDefaultDigitCount;
  GetParameters.DigitSpacingFactor := dxGaugeDigitalScaleDefaultDigitSpacingFactor;
end;

procedure TdxGaugeCustomDigitalScale.InternalRestoreStyleParameters;
var
  AScaleParameters: TdxGaugeDigitalScaleParameters;
begin
  AScaleParameters := GetParameters;
  if not IsDefaultColor(AScaleParameters.SegmentColorOff) then
    AScaleParameters.SegmentColorOff := dxacDefault;
  if not IsDefaultColor(AScaleParameters.SegmentColorOn) then
    AScaleParameters.SegmentColorOn := dxacDefault;
  inherited InternalRestoreStyleParameters;
end;

function TdxGaugeCustomDigitalScale.GetValue: string;
begin
  Result := dxVariantToString(GetParameters.Value);
end;

function TdxGaugeCustomDigitalScale.GetDigitCount: Integer;
begin
  Result := GetParameters.DigitCount;
end;

function TdxGaugeCustomDigitalScale.GetDigitSpacingFactor: Single;
begin
  Result := GetParameters.DigitSpacingFactor;
end;

function TdxGaugeCustomDigitalScale.GetDisplayMode: TdxGaugeDigitalScaleDisplayMode;
begin
  Result := GetParameters.DisplayMode;
end;

function TdxGaugeCustomDigitalScale.GetOptionsLayout: TdxGaugeScaleOptionsRectangularLayout;
begin
  Result := inherited OptionsLayout as TdxGaugeScaleOptionsRectangularLayout;
end;

function TdxGaugeCustomDigitalScale.GetOptionsView: TdxGaugeDigitalScaleOptionsView;
begin
  Result := inherited OptionsView as TdxGaugeDigitalScaleOptionsView;
end;

function TdxGaugeCustomDigitalScale.GetSegmentColorOff: TdxAlphaColor;
begin
  Result := GetParameters.SegmentColorOff;
end;

function TdxGaugeCustomDigitalScale.GetSegmentColorOn: TdxAlphaColor;
begin
  Result := GetParameters.SegmentColorOn;
end;

procedure TdxGaugeCustomDigitalScale.SetDigitCount(const AValue: Integer);
var
  AScaleParameters: TdxGaugeDigitalScaleParameters;
begin
  AScaleParameters := GetParameters;
  if (AScaleParameters.DigitCount <> AValue) and (AValue >= 0) then
  begin
    AScaleParameters.DigitCount := Max(Min(AValue, dxGaugeDigitalScaleMaxDigitCount), 0);
    ScaleChanged([sclStaticLayer, sclDynamicLayer]);
  end;
end;

procedure TdxGaugeCustomDigitalScale.SetDigitSpacingFactor(const AValue: Single);
var
  AScaleParameters: TdxGaugeDigitalScaleParameters;
begin
  AScaleParameters := GetParameters;
  if not SameValue(AScaleParameters.DigitSpacingFactor, AValue) then
  begin
    AScaleParameters.DigitSpacingFactor := Max(Min(AValue, 1), 0);
    ScaleChanged([sclDynamicLayer]);
  end;
end;

procedure TdxGaugeCustomDigitalScale.SetDisplayMode(const AValue: TdxGaugeDigitalScaleDisplayMode);
var
  AScaleParameters: TdxGaugeDigitalScaleParameters;
begin
  AScaleParameters := GetParameters;
  if AScaleParameters.DisplayMode <> AValue then
  begin
    AScaleParameters.DisplayMode := AValue;
    GetViewInfo.RecreateSectionController(AScaleParameters.DisplayMode);
    ScaleChanged([sclStaticLayer, sclDynamicLayer]);
  end;
end;

procedure TdxGaugeCustomDigitalScale.SetOptionsLayout(const AValue: TdxGaugeScaleOptionsRectangularLayout);
begin
  inherited OptionsLayout := AValue;
end;

procedure TdxGaugeCustomDigitalScale.SetOptionsView(const AValue: TdxGaugeDigitalScaleOptionsView);
begin
  inherited OptionsView := AValue;
end;

procedure TdxGaugeCustomDigitalScale.SetSegmentColorOff(const AValue: TdxAlphaColor);
var
  AScaleParameters: TdxGaugeDigitalScaleParameters;
begin
  AScaleParameters := GetParameters;
  if AScaleParameters.SegmentColorOff <> AValue then
  begin
    AScaleParameters.SegmentColorOff := AValue;
    ScaleChanged([sclDynamicLayer]);
  end;
end;

procedure TdxGaugeCustomDigitalScale.SetSegmentColorOn(const AValue: TdxAlphaColor);
var
  AScaleParameters: TdxGaugeDigitalScaleParameters;
begin
  AScaleParameters := GetParameters;
  if AScaleParameters.SegmentColorOn <> AValue then
  begin
    AScaleParameters.SegmentColorOn := AValue;
    ScaleChanged([sclDynamicLayer]);
  end;
end;

procedure TdxGaugeCustomDigitalScale.SetValue(const AValue: string);
begin
  if GetParameters.DigitCount = 0 then
    AddChangeLayer(sclStaticLayer);
  InternalSetValue(AValue);
end;

function TdxGaugeCustomDigitalScale.GetParameters: TdxGaugeDigitalScaleParameters;
begin
  Result := Parameters as TdxGaugeDigitalScaleParameters;
end;

function TdxGaugeCustomDigitalScale.GetViewInfo: TdxGaugeDigitalScaleViewInfo;
begin
  Result := inherited ViewInfo as TdxGaugeDigitalScaleViewInfo;
end;

function TdxGaugeCustomDigitalScale.IsDefaultColor(const AColor: TdxAlphaColor): Boolean;
begin
  Result := AColor = dxacDefault;
end;

function TdxGaugeCustomDigitalScale.IsSegmentColorOffStored: Boolean;
var
  AScaleParameters: TdxGaugeDigitalScaleParameters;
begin
  AScaleParameters := GetParameters;
  Result := not IsDefaultColor(AScaleParameters.SegmentColorOff) and
    (AScaleParameters.SegmentColorOff <> GetViewInfo.GetDefaultParameters.SegmentColorOff);
end;

function TdxGaugeCustomDigitalScale.IsSegmentColorOnStored: Boolean;
var
  AScaleParameters: TdxGaugeDigitalScaleParameters;
begin
  AScaleParameters := GetParameters;
  Result := not IsDefaultColor(AScaleParameters.SegmentColorOn) and
    (AScaleParameters.SegmentColorOn <> GetViewInfo.GetDefaultParameters.SegmentColorOn);
end;

function TdxGaugeCustomDigitalScale.IsValueStored: Boolean;
begin
  Result := not SameText(dxVariantToString(GetParameters.Value), dxGaugeDigitalScaleDefaultValue);
end;

{ TdxGaugeCustomDigitalScaleStyleReader }

class function TdxGaugeCustomDigitalScaleStyleReader.GetResourceNamePrefix: string;
begin
  Result := 'Digital';
end;

function TdxGaugeCustomDigitalScaleStyleReader.GetDefaultParametersClass: TdxGaugeCustomScaleDefaultParametersClass;
begin
  Result := TdxGaugeDigitalScaleDefaultParameters;
end;

procedure TdxGaugeCustomDigitalScaleStyleReader.ReadParameters(ANode: TdxXMLNode;
  AParameters: TdxGaugeCustomScaleDefaultParameters);
var
  AScaleParameters: TdxGaugeDigitalScaleDefaultParameters;
begin
  AScaleParameters := AParameters as TdxGaugeDigitalScaleDefaultParameters;
  AScaleParameters.SegmentColorOff := GetAttributeValueAsAlphaColor(GetChildNode(ANode, 'SegmentColorOff'), 'Value');
  AScaleParameters.SegmentColorOn := GetAttributeValueAsAlphaColor(GetChildNode(ANode, 'SegmentColorOn'), 'Value');
end;

{ TdxGaugeDigitalScaleCustomSectionCalculator }

constructor TdxGaugeDigitalScaleCustomSectionCalculator.Create;
begin
  inherited Create;
  InitCharset;
end;

destructor TdxGaugeDigitalScaleCustomSectionCalculator.Destroy;
begin
  FreeAndNil(FCharset);
  inherited Destroy;
end;

function TdxGaugeDigitalScaleCustomSectionCalculator.AcceptChar(APreviousChar, ACurrentChar, ANextChar: Char): Boolean;
begin
  Result := FCharset.ContainsKey(ACurrentChar) or not (IsEmpty(APreviousChar) and IsColon(ACurrentChar)) and
    (IsNonZeroLengthSymbol(APreviousChar, ACurrentChar, ANextChar) or IsNonZeroLengthSymbol(ANextChar, ACurrentChar,
    APreviousChar));
end;

function TdxGaugeDigitalScaleCustomSectionCalculator.GetMask(APreviousChar, ACurrentChar, ANextChar: Char;
  var ACharIndex: Integer): Integer;
begin
  FCharset.TryGetValue(ACurrentChar, Result);

  if IsBottomPoint(ACurrentChar) then
  begin
    if FCharset.TryGetValue(APreviousChar, Result) then
      Dec(ACharIndex);
    AppendBottomPointMask(Result);
  end;

  if IsBottomPoint(APreviousChar) and not FCharset.ContainsKey(ACurrentChar) or
    IsBottomPoint(ANextChar) and not IsColon(ACurrentChar) then
    AppendBottomPointMask(Result);

  if IsColon(ACurrentChar) then
  begin
    if FCharset.TryGetValue(APreviousChar, Result) or IsBottomPoint(APreviousChar) then
    begin
      Dec(ACharIndex);
      if IsBottomPoint(APreviousChar) then
        AppendBottomPointMask(Result);
    end;
    AppendColonLeftPartMask(Result);
  end;

  if IsColon(ANextChar) then
    AppendColonLeftPartMask(Result);

  if IsColon(APreviousChar) then
    AppendColonRightPartMask(Result);
end;

function TdxGaugeDigitalScaleCustomSectionCalculator.IsNonZeroLengthSymbol(APreviousChar, ACurrentChar,
  ANextChar: Char): Boolean;
begin
  Result := IsBottomPoint(APreviousChar) and IsBottomPoint(ACurrentChar) or
    IsColon(APreviousChar) and IsColon(ACurrentChar) or
    IsColon(APreviousChar) and IsBottomPoint(ACurrentChar) or
    IsColon(APreviousChar) or
    IsBottomPoint(ACurrentChar) and FCharset.ContainsKey(ANextChar) or
    IsPointOrColon(ACurrentChar) and IsEmpty(APreviousChar) and IsEmpty(ANextChar);
end;

function TdxGaugeDigitalScaleCustomSectionCalculator.IsPoint(AChar: Char): Boolean;
begin
  Result := IsBottomPoint(AChar);
end;

procedure TdxGaugeDigitalScaleCustomSectionCalculator.InitCharset;
begin
  FCharset := TDictionary<Char, Integer>.Create;
end;

procedure TdxGaugeDigitalScaleCustomSectionCalculator.Calculate(const AText: string;
  ASections: TObjectList<TdxGaugeDigitalScaleCustomSection>);
var
  S: string;
  ACurrentChar, APreviousChar, ANextChar: Char;
  ACharIndex, AMaskIndex, I: Integer;
  AMasks: array of Integer;
begin
  S := #0 + UpperCase(AText);
  SetLength(AMasks, ASections.Count);
  ACharIndex := Length(S);
  AMaskIndex := High(AMasks);
  while (ACharIndex > 0) and (AMaskIndex >= 0) do
  begin
    ACurrentChar := GetCurrentChar(S, ACharIndex);
    APreviousChar := GetPreviousChar(S, ACharIndex);
    ANextChar := GetNextChar(S, ACharIndex);
    if AcceptChar(APreviousChar, ACurrentChar, ANextChar) then
    begin
      AMasks[AMaskIndex] := GetMask(APreviousChar, ACurrentChar, ANextChar, ACharIndex);
      if IsColon(GetPreviousChar(S, ACharIndex)) and (FCharset.ContainsKey(APreviousChar) or IsPoint(APreviousChar)) then
        AppendColonRightPartMask(AMasks[AMaskIndex]);
      if IsTopPoint(GetPreviousChar(S, ACharIndex)) and (FCharset.ContainsKey(APreviousChar)) then
      begin
        AppendTopPointMask(AMasks[AMaskIndex]);
        Dec(ACharIndex);
      end;
      Dec(AMaskIndex);
    end;
    Dec(ACharIndex);
  end;

  for I := 0 to ASections.Count - 1 do
    TdxGaugeDigitalScaleSection(ASections[I]).Mask := AMasks[I];
end;

function TdxGaugeDigitalScaleCustomSectionCalculator.GetCurrentChar(const AText: string; AIndex: Integer): Char;
begin
  if (AIndex > 0) and (AIndex <= Length(AText)) then
    Result := AText[AIndex]
  else
    Result := #0;
end;

function TdxGaugeDigitalScaleCustomSectionCalculator.GetPreviousChar(const AText: string; AIndex: Integer): Char;
begin
  if AIndex < 1 then
    Result := #0
  else
    Result := AText[AIndex - 1];
end;

function TdxGaugeDigitalScaleCustomSectionCalculator.GetNextChar(const AText: string; AIndex: Integer): Char;
begin
  if AIndex = Length(AText) then
    Result := #0
  else
    Result := AText[AIndex + 1];
end;

function TdxGaugeDigitalScaleCustomSectionCalculator.IsBottomPoint(AChar: Char): Boolean;
begin
  Result := (AChar = '.') or (AChar = ',');
end;

function TdxGaugeDigitalScaleCustomSectionCalculator.IsColon(AChar: Char): Boolean;
begin
  Result := (AChar = ':') or (AChar = ';');
end;

function TdxGaugeDigitalScaleCustomSectionCalculator.IsEmpty(AChar: Char): Boolean;
begin
  Result := AChar = #0;
end;

function TdxGaugeDigitalScaleCustomSectionCalculator.IsPointOrColon(AChar: Char): Boolean;
begin
  Result := IsPoint(AChar) or IsColon(AChar);
end;

function TdxGaugeDigitalScaleCustomSectionCalculator.IsTopPoint(AChar: Char): Boolean;
begin
  Result := False;
end;

function TdxGaugeDigitalScaleCustomSectionCalculator.SetBitState(const AFlags: Integer; AIndex: Integer): Integer;
begin
  Result := AFlags or (1 shl AIndex);
end;

{ TdxGaugeDigitalScaleSevenSegmentSectionCalculator }

procedure TdxGaugeDigitalScaleSevenSegmentSectionCalculator.AppendColonLeftPartMask(var AMask: Integer);
begin
  AMask := SetBitState(AMask, 10);
  AMask := SetBitState(AMask, 11);
end;

procedure TdxGaugeDigitalScaleSevenSegmentSectionCalculator.AppendColonRightPartMask(var AMask: Integer);
begin
  AMask := SetBitState(AMask, 8);
  AMask := SetBitState(AMask, 9);
end;

procedure TdxGaugeDigitalScaleSevenSegmentSectionCalculator.AppendBottomPointMask(var AMask: Integer);
begin
  AMask := SetBitState(AMask, 7);
end;

procedure TdxGaugeDigitalScaleSevenSegmentSectionCalculator.AppendTopPointMask(var AMask: Integer);
begin
// do nothing
end;

procedure TdxGaugeDigitalScaleSevenSegmentSectionCalculator.InitCharset;
begin
  inherited InitCharset;
  FCharset.AddOrSetValue('0', $7E);
  FCharset.AddOrSetValue('1', $30);
  FCharset.AddOrSetValue('2', $6D);
  FCharset.AddOrSetValue('3', $79);
  FCharset.AddOrSetValue('4', $33);
  FCharset.AddOrSetValue('5', $5B);
  FCharset.AddOrSetValue('6', $5F);
  FCharset.AddOrSetValue('7', $70);
  FCharset.AddOrSetValue('8', $7F);
  FCharset.AddOrSetValue('9', $7B);
  FCharset.AddOrSetValue('-', $1);
  FCharset.AddOrSetValue('_', $8);
  FCharset.AddOrSetValue(' ', $0);
end;

{ TdxGaugeDigitalScaleFourteenSegmentSectionCalculator }

function TdxGaugeDigitalScaleFourteenSegmentSectionCalculator.AcceptChar(APreviousChar, ACurrentChar,
  ANextChar: Char): Boolean;
begin
  Result := inherited AcceptChar(APreviousChar, ACurrentChar, ANextChar) and
    not (IsEmpty(ANextChar) and IsColon(ACurrentChar) and IsTopPoint(APreviousChar) or
    IsEmpty(APreviousChar) and IsEmpty(ACurrentChar) and IsTopPoint(ANextChar));
end;

function TdxGaugeDigitalScaleFourteenSegmentSectionCalculator.GetMask(APreviousChar, ACurrentChar, ANextChar: Char;
  var ACharIndex: Integer): Integer;
begin
  Result := inherited GetMask(APreviousChar, ACurrentChar, ANextChar, ACharIndex);

  if IsTopPoint(ACurrentChar) and IsEmpty(ANextChar) and IsBottomPoint(APreviousChar) then
  begin
    Result := 0;
    AppendTopPointMask(Result);
  end;

  if IsTopPoint(ACurrentChar) or IsTopPoint(APreviousChar) or
    IsTopPoint(ACurrentChar) and IsColon(APreviousChar) and IsEmpty(ANextChar) then
  begin
    AppendTopPointMask(Result);
    if IsTopPoint(ACurrentChar) and IsColon(APreviousChar) then
      AppendColonRightPartMask(Result);
  end;

  if IsTopPoint(APreviousChar) and (IsColon(ACurrentChar) or FCharset.ContainsKey(ACurrentChar)) or
    IsTopPoint(ACurrentChar) and IsEmpty(APreviousChar) and IsEmpty(ANextChar) or
    IsTopPoint(ANextChar) and IsEmpty(ACurrentChar) and IsEmpty(APreviousChar) then
  begin
    AppendTopPointMask(Result);
    Dec(ACharIndex);
  end;
end;

function TdxGaugeDigitalScaleFourteenSegmentSectionCalculator.IsNonZeroLengthSymbol(APreviousChar,
  ACurrentChar, ANextChar: Char): Boolean;
begin
  Result := inherited IsNonZeroLengthSymbol(APreviousChar, ACurrentChar, ANextChar) or
    IsBottomPoint(ACurrentChar) or IsTopPoint(APreviousChar) or
    IsTopPoint(ACurrentChar) and (IsEmpty(ANextChar) or IsEmpty(APreviousChar));
end;

function TdxGaugeDigitalScaleFourteenSegmentSectionCalculator.IsPoint(AChar: Char): Boolean;
begin
  Result := inherited IsBottomPoint(AChar) or IsTopPoint(AChar);
end;

function TdxGaugeDigitalScaleFourteenSegmentSectionCalculator.IsTopPoint(AChar: Char): Boolean;
begin
  Result := (AChar = '"') or (AChar = '''') or (AChar = '*') or (AChar = '`');
end;

procedure TdxGaugeDigitalScaleFourteenSegmentSectionCalculator.AppendColonLeftPartMask(var AMask: Integer);
begin
  AMask := SetBitState(AMask, 16);
  AMask := SetBitState(AMask, 17);
end;

procedure TdxGaugeDigitalScaleFourteenSegmentSectionCalculator.AppendColonRightPartMask(var AMask: Integer);
begin
  AMask := SetBitState(AMask, 18);
  AMask := SetBitState(AMask, 19);
end;

procedure TdxGaugeDigitalScaleFourteenSegmentSectionCalculator.AppendBottomPointMask(var AMask: Integer);
begin
  AMask := SetBitState(AMask, 14);
end;

procedure TdxGaugeDigitalScaleFourteenSegmentSectionCalculator.AppendTopPointMask(var AMask: Integer);
begin
  AMask := SetBitState(AMask, 15);
end;

procedure TdxGaugeDigitalScaleFourteenSegmentSectionCalculator.InitCharset;
begin
  inherited InitCharset;
  FCharset.AddOrSetValue('0', $3F22);
  FCharset.AddOrSetValue('1', $1800);
  FCharset.AddOrSetValue('2', $3611);
  FCharset.AddOrSetValue('3', $3C11);
  FCharset.AddOrSetValue('4', $1911);
  FCharset.AddOrSetValue('5', $2D11);
  FCharset.AddOrSetValue('6', $2F11);
  FCharset.AddOrSetValue('7', $3800);
  FCharset.AddOrSetValue('8', $3F11);
  FCharset.AddOrSetValue('9', $3D11);
  FCharset.AddOrSetValue('-', $11);
  FCharset.Add('~', $11);
  FCharset.AddOrSetValue('+', $55);
  FCharset.AddOrSetValue('_', $400);
  FCharset.AddOrSetValue('/', $22);
  FCharset.AddOrSetValue('\', $88);
  FCharset.AddOrSetValue('|', $44);
  FCharset.AddOrSetValue(' ', $0);
  FCharset.AddOrSetValue('Q', $3F08);
  FCharset.AddOrSetValue('W', $1B0A);
  FCharset.AddOrSetValue('E', $2711);
  FCharset.AddOrSetValue('R', $3319);
  FCharset.AddOrSetValue('T', $2044);
  FCharset.AddOrSetValue('Y', $1115);
  FCharset.AddOrSetValue('U', $1F00);
  FCharset.AddOrSetValue('I', $2444);
  FCharset.Add('O', $3F00);
  FCharset.AddOrSetValue('P', $3311);
  FCharset.AddOrSetValue('A', $3B11);
  FCharset.Add('S', $2D11);
  FCharset.AddOrSetValue('D', $3C44);
  FCharset.AddOrSetValue('F', $2311);
  FCharset.AddOrSetValue('G', $2F10);
  FCharset.AddOrSetValue('H', $1B11);
  FCharset.AddOrSetValue('J', $1E00);
  FCharset.AddOrSetValue('K', $329);
  FCharset.AddOrSetValue('L', $700);
  FCharset.AddOrSetValue('Z', $2422);
  FCharset.AddOrSetValue('X', $AA);
  FCharset.AddOrSetValue('C', $2700);
  FCharset.AddOrSetValue('V', $322);
  FCharset.AddOrSetValue('B', $3C54);
  FCharset.AddOrSetValue('N', $1B88);
  FCharset.AddOrSetValue('M', $1BA0);
  FCharset.AddOrSetValue('(', $2700);
  FCharset.Add('[', $2700);
  FCharset.Add('{', $2700);
  FCharset.AddOrSetValue(')', $3C00);
  FCharset.Add(']', $3C00);
  FCharset.Add('}', $3C00);
  FCharset.AddOrSetValue('>', $82);
  FCharset.AddOrSetValue('<', $28);
  FCharset.AddOrSetValue('=', $411);
end;

{ TdxGaugeDigitalScaleCustomMatrixSectionCalculator }

constructor TdxGaugeDigitalScaleCustomMatrixSectionCalculator.Create;
begin
  inherited Create;
  FCache := TDictionary<Char, TArray<Boolean>>.Create;
  FGpFontFamily := TdxGPFontFamily.Create('Lucida Console');

  if FGpFontFamily.IsStyleAvailable(Integer(GetFontStyle)) then
    FGpFont := TdxGPFont.Create(FGpFontFamily, GetFontSize, GetFontStyle, TdxGraphicUnit.guPixel)
  else
    FGpFont := TdxGPFont.Create(FGpFontFamily, GetFontSize, TdxGraphicUnit.guPixel);

  FGpFormat := TdxGPStringFormat.Create;
  FGpFormat.Alignment := TdxGpStringAlignment.StringAlignmentCenter;
  FGpFormat.LineAlignment := TdxGpStringAlignment.StringAlignmentCenter;
  FGpFormat.FormatFlags := Ord(StringFormatFlagsNoClip) or Ord(StringFormatFlagsNoWrap);
  FGpFormat.Trimming := TdxGpStringTrimming.StringTrimmingNone;

  FGpBrush := TdxGPBrush.Create;
  FGpBrush.Style := gpbsSolid;
  FGpBrush.Color := dxColorToAlphaColor(clWhite);
end;

destructor TdxGaugeDigitalScaleCustomMatrixSectionCalculator.Destroy;
begin
  FreeAndNil(FGpBrush);
  FreeAndNil(FGpFormat);
  FreeAndNil(FGpFont);
  FreeAndNil(FGpFontFamily);
  FreeAndNil(FCache);
  inherited Destroy;
end;

procedure TdxGaugeDigitalScaleCustomMatrixSectionCalculator.Calculate(const AText: string;
  ASections: TObjectList<TdxGaugeDigitalScaleCustomSection>);
var
  ACharIndex, ASectionIndex, I: Integer;
  AColors: TRGBColors;
  AChar: Char;
begin
  ResetSectionDots(ASections);
  ACharIndex := Length(AText);
  ASectionIndex := ASections.Count - 1;
  while (ACharIndex > 0) and (ASectionIndex >= 0) do
  begin
    AChar := GetCurrentChar(AText, ACharIndex);
    if not FCache.ContainsKey(AChar) then
    begin
      AColors := GetCharColors(AChar);
      for I := Low(AColors) to High(AColors) do
        TdxGaugeDigitalScaleMatrixSection(ASections[ASectionIndex]).Flags[I] := dxRGBQuadToColor(AColors[I]) <> clBlack;
      FCache.Add(AChar, TdxGaugeDigitalScaleMatrixSection(ASections[ASectionIndex]).Flags);
    end
    else
      FCache.TryGetValue(AChar, TdxGaugeDigitalScaleMatrixSection(ASections[ASectionIndex]).Flags);
    Dec(ACharIndex);
    Dec(ASectionIndex);
  end;
end;

function TdxGaugeDigitalScaleCustomMatrixSectionCalculator.GetCharColors(AChar: Char): TRGBColors;
var
  ABitmap: TcxBitmap32;
begin
  ABitmap := TcxBitmap32.CreateSize(GetMatrixSize.cx, GetMatrixSize.cy);
  try
    DrawChar(AChar, ABitmap);
    ABitmap.GetBitmapColors(Result);
  finally
    ABitmap.Free;
  end;
end;

procedure TdxGaugeDigitalScaleCustomMatrixSectionCalculator.DrawChar(AChar: Char; ABitmap: TcxBitmap32);
var
  AGPGraphics: TdxGPGraphics;
  AHeight: Single;
  characterPlace: TdxRectF;
begin
  ABitmap.Clear;
  AGPGraphics := dxGpBeginPaint(ABitmap.Canvas.Handle, ABitmap.ClientRect);
  try
    AGPGraphics.SmoothingMode := smNone;
    AGPGraphics.TextRenderingHint := TextRenderingHintSingleBitPerPixelGridFit;

    AHeight := FGpFont.GetHeight(AGPGraphics);
    characterPlace := cxRectF(0, GetCharOffset + (GetMatrixSize.cy - AHeight) * 0.5,
      GetMatrixSize.cx, (GetMatrixSize.cy + AHeight) * 0.5);
    AGPGraphics.DrawString(string(AChar), FGpFont, FGpBrush, characterPlace, FGpFormat);
  finally
    dxGpEndPaint(AGPGraphics);
  end;
end;

procedure TdxGaugeDigitalScaleCustomMatrixSectionCalculator.ResetSectionDots(
  ASections: TObjectList<TdxGaugeDigitalScaleCustomSection>);
var
  I: Integer;
  ASize: TSize;
begin
  ASize := GetMatrixSize;
  for I := 0 to ASections.Count - 1 do
    SetLength(TdxGaugeDigitalScaleMatrixSection(ASections[I]).Flags, ASize.cx * ASize.cy);
end;

{ TdxGaugeDigitalScaleMatrix5x8SegmentSectionCalculator }

function TdxGaugeDigitalScaleMatrix5x8SegmentSectionCalculator.GetCharOffset: Integer;
begin
  Result := 0;
end;

function TdxGaugeDigitalScaleMatrix5x8SegmentSectionCalculator.GetFontSize: Integer;
begin
  Result := 8;
end;

function TdxGaugeDigitalScaleMatrix5x8SegmentSectionCalculator.GetFontStyle: TdxGPFontStyle;
begin
  Result := TdxGPFontStyle.FontStyleRegular;
end;

function TdxGaugeDigitalScaleMatrix5x8SegmentSectionCalculator.GetMatrixSize: TSize;
begin
  Result := cxSize(5, 8);
end;

{ TdxGaugeDigitalScaleMatrix8x14SegmentSectionCalculator }

function TdxGaugeDigitalScaleMatrix8x14SegmentSectionCalculator.GetCharOffset: Integer;
begin
  Result := 1;
end;

function TdxGaugeDigitalScaleMatrix8x14SegmentSectionCalculator.GetFontSize: Integer;
begin
  Result := 14;
end;

function TdxGaugeDigitalScaleMatrix8x14SegmentSectionCalculator.GetFontStyle: TdxGPFontStyle;
begin
  Result := TdxGPFontStyle.FontStyleBold;
end;

function TdxGaugeDigitalScaleMatrix8x14SegmentSectionCalculator.GetMatrixSize: TSize;
begin
  Result := cxSize(8, 14);
end;

{ TdxGaugeDigitalScaleCustomSectionsController }

constructor TdxGaugeDigitalScaleCustomSectionsController.Create(ADisplayMode: TdxGaugeDigitalScaleDisplayMode);
begin
  inherited Create;
  FDisplayMode := ADisplayMode;
  FSections := TObjectList<TdxGaugeDigitalScaleCustomSection>.Create;
  FSectionCalculator := GetCalculatorClass.Create;
  LoadDigit;
end;

destructor TdxGaugeDigitalScaleCustomSectionsController.Destroy;
begin
  FreeAndNil(FDigit);
  FreeAndNil(FSectionCalculator);
  FreeAndNil(FSections);
  inherited Destroy;
end;

function TdxGaugeDigitalScaleCustomSectionsController.GetSectionClass: TdxGaugeDigitalScaleCustomSectionClass;
begin
  Result := TdxGaugeDigitalScaleSection;
end;

function TdxGaugeDigitalScaleCustomSectionsController.GetBackgroundOriginalSize(ASectionCount: Integer): TdxSizeF;
begin
  Result.cx := GetDigitSize.cx * ASectionCount + GetOffset;
  Result.cy := GetSectionSize.cy;
end;

function TdxGaugeDigitalScaleCustomSectionsController.GetBackgroundPartWidth(const AScaleFactor: TdxPointF): Single;
var
  ADigitSize: TdxSizeF;
begin
  ADigitSize := GetDigitSize;
  Result := GetSectionSize.cx * AScaleFactor.X * dxGaugeDigitalScaleDigitOffset * ADigitSize.cx / ADigitSize.cy;
end;

procedure TdxGaugeDigitalScaleCustomSectionsController.Calculate(AValue: Variant; const ABounds: TdxRectF;
  ASectionCount: Integer; ASectionIndentFactor: Single; AStretch: Boolean);
begin
  PrepareSections(ASectionCount);
  CalculateSectionsBounds(ABounds, ASectionIndentFactor, AStretch);
  CalculateSectionsMasks(dxVariantToString(AValue));
end;

procedure TdxGaugeDigitalScaleCustomSectionsController.CalculateSectionsBounds(const ABounds: TdxRectF;
  ASectionsIndent: Single; AStretch: Boolean);

  procedure CalculateSectionSize(const ABounds: TdxRectF; var ASectionSize: TdxSizeF);
  begin
    ASectionSize.cx := ABounds.Width / FSections.Count;
    if not AStretch then
      ASectionSize.cy := ASectionSize.cx / GetDigitSize.cx * GetDigitSize.cy
    else
      ASectionSize.cy := GetDigitSize.cy;
  end;

var
  I: Integer;
  ASectionSize: TdxSizeF;
  ASectionsBounds: TdxRectF;
begin
  ASectionsBounds := cxRectContent(ABounds, cxRectF(1, 0, 1, 0));
  CalculateSectionSize(ASectionsBounds, ASectionSize);
  for I := 0 to FSections.Count - 1 do
  begin
    FSections[I].Bounds.Top := cxRectCenter(ASectionsBounds).Y - ASectionSize.cy / 2;
    FSections[I].Bounds.Left := ASectionsBounds.Left + ASectionSize.cx * I;
    FSections[I].Bounds.Right := FSections[I].Bounds.Left + ASectionSize.cx;
    FSections[I].Bounds.Bottom := FSections[I].Bounds.Top + ASectionSize.cy;
    FSections[I].Bounds := cxRectContent(FSections[I].Bounds, GetSectionIndent(ASectionSize, ASectionsIndent));
  end;
end;

procedure TdxGaugeDigitalScaleCustomSectionsController.CalculateSectionsMasks(const AScaleTextValue: string);
begin
  FSectionCalculator.Calculate(AScaleTextValue, FSections);
end;

procedure TdxGaugeDigitalScaleCustomSectionsController.ColorizeDigit(const ASectionMask: Integer;
  AColorOn, AColorOff: TdxAlphaColor);
var
  I: Integer;
  AColor: TdxAlphaColor;
begin
  for I := 0 to GetSegmentCount do
  begin
    if GetBitState(ASectionMask, I) then
      AColor := AColorOn
    else
      AColor := AColorOff;
    TdxCompositeShapeAccess(FDigit).SetShapesColor(AColor, GetSegmentName(I));
  end;
end;

procedure TdxGaugeDigitalScaleCustomSectionsController.ColorizeSection(ASectionIndex: Integer;
  AColorOn, AColorOff: TdxAlphaColor);
begin
  ColorizeDigit(TdxGaugeDigitalScaleSection(FSections[ASectionIndex]).Mask, AColorOn, AColorOff);
end;

procedure TdxGaugeDigitalScaleCustomSectionsController.DrawSections(AGPGraphics: TdxGPGraphics;
  AColorOn, AColorOff: TdxAlphaColor);
var
  I: Integer;
begin
  for I := 0 to FSections.Count - 1 do
  begin
    ColorizeSection(I, AColorOn, AColorOff);
    dxGaugeDrawImage(AGPGraphics, FDigit, FSections[I].Bounds);
  end;
end;

function TdxGaugeDigitalScaleCustomSectionsController.GetBitState(const AFlags: Integer; AIndex: Integer): Boolean;
begin
  Result := (AFlags and (1 shl AIndex)) <> 0;
end;

function TdxGaugeDigitalScaleCustomSectionsController.GetDigitResourceNameSuffix: string;
begin
  case FDisplayMode of
    sdmMatrix5x8Dots, sdmMatrix8x14Dots:
      Result := 'Dots';
    sdmMatrix5x8Squares, sdmMatrix8x14Squares:
      Result := 'Squares';
    else
      Result := '';
  end;
end;

function TdxGaugeDigitalScaleCustomSectionsController.GetDigitSize: TdxSizeF;
begin
  Result.cx := TdxCompositeShapeAccess(FDigit).WidthF;
  Result.cy := TdxCompositeShapeAccess(FDigit).HeightF;
end;

function TdxGaugeDigitalScaleCustomSectionsController.GetOffset: Single;
begin
  Result := 2 * GetDigitSize.cy * dxGaugeDigitalScaleDigitOffset;
end;

function TdxGaugeDigitalScaleCustomSectionsController.GetResourceName: string;
begin
  Result := GetDigitResourceName + GetDigitResourceNameSuffix;
end;

function TdxGaugeDigitalScaleCustomSectionsController.GetSectionIndent(const ASectionSize: TdxSizeF;
  AIndentFactor: Single): TdxRectF;
begin
  Result.TopLeft := dxPointF(ASectionSize.cx * AIndentFactor / 2, ASectionSize.cy * AIndentFactor / 2);
  Result.BottomRight := Result.TopLeft;
end;

function TdxGaugeDigitalScaleCustomSectionsController.GetSectionSize: TdxSizeF;
var
  ADigitSize: TdxSizeF;
  AOffset: Single;
begin
  ADigitSize := GetDigitSize;
  AOffset := GetOffset;
  Result.cx := ADigitSize.cx + AOffset;
  Result.cy := ADigitSize.cy + AOffset;
end;

procedure TdxGaugeDigitalScaleCustomSectionsController.LoadDigit;
var
  AStream: TResourceStream;
begin
  AStream := TResourceStream.Create(HInstance, GetResourceName, RT_RCDATA);
  try
    FDigit := TdxCompositeShape.Create;
    FDigit.LoadFromStream(AStream);
    TdxCompositeShapeAccess(FDigit).NeedNormalize := True;
  finally
    AStream.Free;
  end;
end;

procedure TdxGaugeDigitalScaleCustomSectionsController.PrepareSections(ASectionCount: Integer);
var
  I: Integer;
  ASectionClass: TdxGaugeDigitalScaleCustomSectionClass;
begin
  FSections.Clear;
  ASectionClass := GetSectionClass;
  for I := 0 to ASectionCount - 1 do
    FSections.Add(ASectionClass.Create);
end;

{ TdxGaugeDigitalScaleSevenSegmentSectionsController }

function TdxGaugeDigitalScaleSevenSegmentSectionsController.GetCalculatorClass: TdxGaugeDigitalScaleCustomSectionCalculatorClass;
begin
  Result := TdxGaugeDigitalScaleSevenSegmentSectionCalculator;
end;

function TdxGaugeDigitalScaleSevenSegmentSectionsController.GetDigitResourceName: string;
begin
  Result := 'DigitalScaleSevenSegments';
end;

function TdxGaugeDigitalScaleSevenSegmentSectionsController.GetSegmentCount: Integer;
begin
  Result := 11;
end;

function TdxGaugeDigitalScaleSevenSegmentSectionsController.GetSegmentName(ASegmentIndex: Integer): string;
const
  SegmentNameMap: array[0..11] of string = ('Path_0', 'Path_1', 'Path_2', 'Path_3', 'Path_4', 'Path_5',
    'Path_6', 'Path_7', 'Path_8_1', 'Path_8_2', 'Path_9_1', 'Path_9_2');
begin
  Result := SegmentNameMap[ASegmentIndex];
end;

{ TdxGaugeDigitalScaleFourteenSegmentSectionsController }

function TdxGaugeDigitalScaleFourteenSegmentSectionsController.GetCalculatorClass: TdxGaugeDigitalScaleCustomSectionCalculatorClass;
begin
  Result := TdxGaugeDigitalScaleFourteenSegmentSectionCalculator;
end;

function TdxGaugeDigitalScaleFourteenSegmentSectionsController.GetDigitResourceName: string;
begin
  Result := 'DigitalScaleFourteenSegments';
end;

function TdxGaugeDigitalScaleFourteenSegmentSectionsController.GetSegmentCount: Integer;
begin
  Result := 19;
end;

function TdxGaugeDigitalScaleFourteenSegmentSectionsController.GetSegmentName(ASegmentIndex: Integer): string;
begin
  Result := 'Path_' + IntToStr(ASegmentIndex);
end;

{ TdxGaugeDigitalScaleCustomMatrixSectionsController }

function TdxGaugeDigitalScaleCustomMatrixSectionsController.GetCalculatorClass: TdxGaugeDigitalScaleCustomSectionCalculatorClass;
begin
  Result := TdxGaugeDigitalScaleCustomMatrixSectionCalculator;
end;

function TdxGaugeDigitalScaleCustomMatrixSectionsController.GetSectionClass: TdxGaugeDigitalScaleCustomSectionClass;
begin
  Result := TdxGaugeDigitalScaleMatrixSection;
end;

function TdxGaugeDigitalScaleCustomMatrixSectionsController.GetSegmentCount: Integer;
begin
  Result := GetMatrixSize.cx * GetMatrixSize.cy;
end;

function TdxGaugeDigitalScaleCustomMatrixSectionsController.GetSegmentName(ASegmentIndex: Integer): string;
var
  ARowIndex: Integer;
begin
  ARowIndex := ASegmentIndex div GetMatrixSize.cx;
  Result := 'Dot_' + IntToStr(GetMatrixSize.cy - 1 - ARowIndex) + '_' + IntToStr(ASegmentIndex - GetMatrixSize.cx * ARowIndex);
end;

procedure TdxGaugeDigitalScaleCustomMatrixSectionsController.ColorizeSection(ASectionIndex: Integer; AColorOn,
  AColorOff: TdxAlphaColor);
var
  I: Integer;
  AColor: TdxAlphaColor;
begin
  for I := 0 to GetSegmentCount do
  begin
    if TdxGaugeDigitalScaleMatrixSection(FSections[ASectionIndex]).Flags[I] then
      AColor := AColorOn
    else
      AColor := AColorOff;
    TdxCompositeShapeAccess(FDigit).SetShapesColor(AColor, GetSegmentName(I));
  end;
end;

{ TdxGaugeDigitalScaleMatrix5x8SegmentSectionsController }

function TdxGaugeDigitalScaleMatrix5x8SegmentSectionsController.GetCalculatorClass: TdxGaugeDigitalScaleCustomSectionCalculatorClass;
begin
  Result := TdxGaugeDigitalScaleMatrix5x8SegmentSectionCalculator;
end;

function TdxGaugeDigitalScaleMatrix5x8SegmentSectionsController.GetDigitResourceName: string;
begin
  Result := 'DigitalScaleMatrix5x8';
end;

function TdxGaugeDigitalScaleMatrix5x8SegmentSectionsController.GetMatrixSize: TSize;
begin
  Result := cxSize(5, 8);
end;

{ TdxGaugeDigitalScaleCustomMatrixSectionsController }

function TdxGaugeDigitalScaleMatrix8x14SegmentSectionsController.GetCalculatorClass: TdxGaugeDigitalScaleCustomSectionCalculatorClass;
begin
  Result := TdxGaugeDigitalScaleMatrix8x14SegmentSectionCalculator;
end;

function TdxGaugeDigitalScaleMatrix8x14SegmentSectionsController.GetDigitResourceName: string;
begin
  Result := 'DigitalScaleMatrix8x14';
end;

function TdxGaugeDigitalScaleMatrix8x14SegmentSectionsController.GetMatrixSize: TSize;
begin
  Result := cxSize(8, 14);
end;

initialization
  dxGaugeRegisterScale(TdxGaugeDigitalScale);
  Classes.RegisterClass(TdxGaugeDigitalScaleCaption);

finalization
  Classes.UnRegisterClass(TdxGaugeDigitalScaleCaption);
  dxGaugeUnregisterScale(TdxGaugeDigitalScale);

end.
