{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressEditors                                           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSEDITORS AND ALL                }
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

unit dxColorPicker;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI17}
  UITypes,
{$ENDIF}
  Types, Windows, Classes, Messages, Controls, Graphics, cxControls, cxGraphics, dxCoreClasses,
  dxGDIPlusClasses, dxGDIPlusAPI, cxLookAndFeelPainters, cxLookAndFeels, dxCore, dxCoreGraphics,
  cxClasses, Math, cxEdit, cxSpinEdit, cxMaskEdit, cxGeometry;

type
  TdxCustomColorPicker = class;
  TdxColorPickerController = class;
  TdxColorPickerCustomViewInfoCell = class;
  TdxColorPickerOptionsView = class;

  TdxColorPickerRGBHexNotation = (cphnHTML, cphnDelphi);
  TdxColorPickerStyle = (cpsGamutAndLightnessSlider, cpsGamutAndHueSlider);

  { IdxColorPickerChangeListener }

  IdxColorPickerChangeListener = interface
  ['{35603909-DDD1-4DDA-9D31-A65013FA16DF}']
    procedure ColorChanged;
  end;

  { TdxColorPickerCustomObject }

  TdxColorPickerCustomObject = class(TcxIUnknownObject)
  strict private
    FControl: TdxCustomColorPicker;

    function GetScaleFactor: TdxScaleFactor;
    function GetUseRightToLeftAlignment: Boolean;
  public
    constructor Create(AControl: TdxCustomColorPicker); virtual;
    //
    property Control: TdxCustomColorPicker read FControl;
    property ScaleFactor: TdxScaleFactor read GetScaleFactor;
    property UseRightToLeftAlignment: Boolean read GetUseRightToLeftAlignment;
  end;

  { TdxColorPickerCustomOptions }

  TdxColorPickerCustomOptions = class(TcxOwnedPersistent)
  protected
    procedure Changed(AType: TdxChangeType = ctHard);
  end;

  { TdxColorPickerColorInfo }

  TdxColorPickerColorInfo = class(TObject)
  private
    FAlpha: Byte;
    FHSL: TdxHSL;
    FOnChanged: TNotifyEvent;

    function GetAlphaColor: TdxAlphaColor;
    function GetColor: TColor;
    function GetGrayScale: Double;
    function GetR: Byte;
    function GetG: Byte;
    function GetB: Byte;
    procedure SetAlpha(AValue: Byte);
    procedure SetAlphaColor(AValue: TdxAlphaColor);
    procedure SetColor(const AValue: TColor);
    procedure SetR(const AValue: Byte);
    procedure SetG(const AValue: Byte);
    procedure SetB(const AValue: Byte);
    procedure SetH(const AValue: Double);
    procedure SetL(const AValue: Double);
    procedure SetS(const AValue: Double);
  protected
    procedure Changed;
  public
    property Alpha: Byte read FAlpha write SetAlpha;
    property AlphaColor: TdxAlphaColor read GetAlphaColor write SetAlphaColor;
    property Color: TColor read GetColor write SetColor;
    // HSL
    property H: Double read FHSL.H write SetH;
    property S: Double read FHSL.S write SetS;
    property L: Double read FHSL.L write SetL;
    // RGB
    property R: Byte read GetR write SetR;
    property G: Byte read GetG write SetG;
    property B: Byte read GetB write SetB;
    // Utils
    property GrayScale: Double read GetGrayScale;
    //
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  end;

  { TdxColorPickerHitTest }

  TdxColorPickerHitTest = class(TdxColorPickerCustomObject)
  private
    FHitObject: TObject;
    FHitPoint: TPoint;
    function GetHitObjectAsViewInfoCell: TdxColorPickerCustomViewInfoCell;
    function GetHitObjectIsViewInfoCell: Boolean;
  public
    procedure Calculate; overload;
    procedure Calculate(const AHitPoint: TPoint); overload;
    procedure Calculate(X, Y: Integer); overload;
    //
    property HitObject: TObject read FHitObject write FHitObject;
    property HitObjectAsViewInfoCell: TdxColorPickerCustomViewInfoCell read GetHitObjectAsViewInfoCell;
    property HitObjectIsViewInfoCell: Boolean read GetHitObjectIsViewInfoCell;
    property HitPoint: TPoint read FHitPoint;
  end;

  { TdxColorPickerCustomViewInfoCell }

  TdxColorPickerCustomViewInfoCellClass = class of TdxColorPickerCustomViewInfoCell;
  TdxColorPickerCustomViewInfoCell = class(TdxColorPickerCustomObject)
  private
    FBounds: TRect;
    FMeasureHeight: Integer;
    FMeasureWidth: Integer;
    function GetController: TdxColorPickerController; inline;
    function GetIsCaptured: Boolean;
    function GetPainter: TcxCustomLookAndFeelPainter; inline;
    procedure SetBounds(const AValue: TRect);
  protected
    procedure DoDraw(ACanvas: TcxCanvas); virtual;
    function DoMeasureHeight: Integer; virtual;
    function DoMeasureWidth: Integer; virtual;
    //
    property IsCaptured: Boolean read GetIsCaptured;
  public
    constructor Create(AControl: TdxCustomColorPicker); override;
    procedure Calculate(AType: TdxChangeType); virtual;
    procedure CalculateHitTest(AHitTest: TdxColorPickerHitTest); virtual;
    procedure DragMove(X, Y: Integer); virtual;
    procedure Draw(ACanvas: TcxCanvas);
    procedure Invalidate;
    function MeasureHeight: Integer;
    function MeasureWidth: Integer;
    //
    property Bounds: TRect read FBounds write SetBounds;
    property Controller: TdxColorPickerController read GetController;
    property Painter: TcxCustomLookAndFeelPainter read GetPainter;
  end;

  { TdxColorPickerCustomViewInfoCells }

  TdxColorPickerCustomViewInfoCells = class(TcxObjectList)
  private
    function GetItem(AIndex: Integer): TdxColorPickerCustomViewInfoCell;
  public
    procedure Add(ACell: TdxColorPickerCustomViewInfoCell);
    procedure Calculate(AType: TdxChangeType); virtual;
    procedure CalculateHitTest(AHitTest: TdxColorPickerHitTest); virtual;
    procedure Draw(ACanvas: TcxCanvas);
    //
    property Items[AIndex: Integer]: TdxColorPickerCustomViewInfoCell read GetItem; default;
  end;

  { TdxColorPickerCustomContainerViewInfoCell }

  TdxColorPickerCustomContainerViewInfoCell = class(TdxColorPickerCustomViewInfoCell)
  private
    FCells: TdxColorPickerCustomViewInfoCells;
  protected
    procedure CreateCell(ACellClass: TdxColorPickerCustomViewInfoCellClass); overload;
    procedure CreateCell(ACellClass: TdxColorPickerCustomViewInfoCellClass; var ACell); overload;
    procedure CreateSubCells; virtual; abstract;
    procedure DoDraw(ACanvas: TcxCanvas); override;
  public
    constructor Create(AControl: TdxCustomColorPicker); override;
    destructor Destroy; override;
    procedure Calculate(AType: TdxChangeType); override;
    procedure CalculateHitTest(AHitTest: TdxColorPickerHitTest); override;
    procedure CalculateSubCellsBounds(R: TRect); virtual; abstract;
    //
    property Cells: TdxColorPickerCustomViewInfoCells read FCells;
  end;

  { TdxColorPickerCustomColorModifierViewInfoCell }

  TdxColorPickerCustomColorModifierViewInfoCell = class(TdxColorPickerCustomViewInfoCell, IdxColorPickerChangeListener)
  private
    function GetColorInfo: TdxColorPickerColorInfo; inline;
  protected
    procedure UpdateEditValue; virtual; abstract;
     // IdxColorPickerChangeListener
    procedure ColorChanged; virtual;
  public
    constructor Create(AControl: TdxCustomColorPicker); override;
    destructor Destroy; override;
    procedure Calculate(AType: TdxChangeType); override;
    //
    property ColorInfo: TdxColorPickerColorInfo read GetColorInfo;
  end;

  { TdxColorPickerCustomVisualColorModifierViewInfoCell }

  TdxColorPickerCustomVisualColorModifierViewInfoCell = class(TdxColorPickerCustomColorModifierViewInfoCell)
  private
    FContentCache: TcxBitmap32;
    FCursorPosition: TPoint;
    procedure SetCursorPosition(const AValue: TPoint);
  protected
    function CalculateCursorPosition: TPoint; virtual; abstract;
    procedure DoDraw(ACanvas: TcxCanvas); override;
    procedure DrawContent(ACanvas: TcxCanvas); virtual;
    procedure FlushContentCache; virtual;
    function GetContentBounds: TRect; virtual;
    function GetContentFrameRect: TRect; virtual;
    function GetCursorColor: TColor; virtual;
    procedure UpdateContentCache; overload;
    procedure UpdateContentCache(ACanvas: TcxCanvas; AWidth, AHeight: Integer); overload; virtual; abstract;
    procedure UpdateEditValue; override;
  public
    destructor Destroy; override;
    procedure Calculate(AType: TdxChangeType); override;
    //
    property ContentBounds: TRect read GetContentBounds;
    property ContentFrameRect: TRect read GetContentFrameRect;
    property CursorPosition: TPoint read FCursorPosition write SetCursorPosition;
  end;

  { TdxColorPickerCustomGamutViewInfoCell }

  TdxColorPickerCustomGamutViewInfoCell = class(TdxColorPickerCustomVisualColorModifierViewInfoCell)
  protected
    function DoMeasureHeight: Integer; override;
    function DoMeasureWidth: Integer; override;
    procedure DrawContent(ACanvas: TcxCanvas); override;
    procedure DrawCursor(ACanvas: TcxCanvas); virtual;
  end;

  { TdxColorPickerCustomSliderViewInfoCell }

  TdxColorPickerCustomSliderViewInfoCell = class(TdxColorPickerCustomVisualColorModifierViewInfoCell)
  protected
    procedure DoDraw(ACanvas: TcxCanvas); override;
    function DoMeasureHeight: Integer; override;
    function DoMeasureWidth: Integer; override;
    procedure DrawArrow(ACanvas: TcxCanvas); virtual;
    function GetArrowBounds: TRect; virtual;
    function GetContentBounds: TRect; override;
  public
    property ArrowBounds: TRect read GetArrowBounds;
  end;

  { TdxColorPickerAlphaSliderViewInfoCell }

  TdxColorPickerAlphaSliderViewInfoCell = class(TdxColorPickerCustomSliderViewInfoCell)
  protected
    function CalculateCursorPosition: TPoint; override;
    procedure UpdateContentCache(ACanvas: TcxCanvas; AWidth: Integer; AHeight: Integer); override;
  public
    procedure DragMove(X, Y: Integer); override;
  end;

  { TdxColorPickerGamutAndLightnessSliderStyleGamutViewInfoCell }

  TdxColorPickerGamutAndLightnessSliderStyleGamutViewInfoCell = class(TdxColorPickerCustomGamutViewInfoCell)
  protected
    function CalculateCursorPosition: TPoint; override;
    procedure UpdateContentCache(ACanvas: TcxCanvas; AWidth: Integer; AHeight: Integer); override;
  public
    procedure DragMove(X, Y: Integer); override;
  end;

  { TdxColorPickerGamutAndLightnessSliderStyleSliderViewInfoCell }

  TdxColorPickerGamutAndLightnessSliderStyleSliderViewInfoCell = class(TdxColorPickerCustomSliderViewInfoCell)
  protected
    function CalculateCursorPosition: TPoint; override;
    procedure UpdateContentCache(ACanvas: TcxCanvas; AWidth: Integer; AHeight: Integer); override;
  public
    procedure DragMove(X, Y: Integer); override;
  end;

  { TdxColorPickerGamutAndHueSliderStyleGamutViewInfoCell }

  TdxColorPickerGamutAndHueSliderStyleGamutViewInfoCell = class(TdxColorPickerCustomGamutViewInfoCell)
  protected
    function CalculateCursorPosition: TPoint; override;
    function GetCursorColor: TColor; override;
    procedure UpdateContentCache(ACanvas: TcxCanvas; AWidth: Integer; AHeight: Integer); override;
  public
    procedure DragMove(X, Y: Integer); override;
  end;

  { TdxColorPickerGamutAndHueSliderStyleSliderViewInfoCell }

  TdxColorPickerGamutAndHueSliderStyleSliderViewInfoCell = class(TdxColorPickerCustomSliderViewInfoCell)
  protected
    function CalculateCursorPosition: TPoint; override;
    procedure UpdateContentCache(ACanvas: TcxCanvas; AWidth: Integer; AHeight: Integer); override;
  public
    procedure DragMove(X, Y: Integer); override;
  end;

  { TdxColorPickerPreviewViewInfoCell }

  TdxColorPickerPreviewViewInfoCell = class(TdxColorPickerCustomColorModifierViewInfoCell)
  protected
    procedure DoDraw(ACanvas: TcxCanvas); override;
    function DoMeasureHeight: Integer; override;
    function DoMeasureWidth: Integer; override;
    procedure UpdateEditValue; override;
  public
    constructor Create(AControl: TdxCustomColorPicker); override;
    destructor Destroy; override;
  end;

  { TdxColorPickerCustomEditViewInfoCell }

  TdxColorPickerCustomEditViewInfoCell = class(TdxColorPickerCustomColorModifierViewInfoCell)
  private
    procedure ChangeHandler(Sender: TObject);
    function GetCaptionTextColor: TColor;
    function GetFont: TFont;  inline;
  protected
    FCaptionRect: TRect;
    FEdit: TcxCustomEdit;

    function CalculateTextFieldSize(ADigits: Integer): Integer; virtual;
    procedure DoDraw(ACanvas: TcxCanvas); override;
    function DoMeasureHeight: Integer; override;
    function DoMeasureWidth: Integer; override;
    procedure EditValueChanged; virtual; abstract;
    function GetCaption: string; virtual; abstract;
    function GetEditClass: TcxCustomEditClass; virtual; abstract;
    procedure InitializeEdit; virtual;
    function MeasureEditWidth: Integer; virtual; abstract;
  public
    constructor Create(AControl: TdxCustomColorPicker); override;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure Calculate(AType: TdxChangeType); override;
    //
    property Caption: string read GetCaption;
    property CaptionRect: TRect read FCaptionRect;
    property CaptionTextColor: TColor read GetCaptionTextColor;
    property Edit: TcxCustomEdit read FEdit;
    property Font: TFont read GetFont;
  end;

  { TdxColorPickerCustomEditBoxViewInfoCell }

  TdxColorPickerCustomEditBoxViewInfoCell = class(TdxColorPickerCustomContainerViewInfoCell)
  protected
    function DoMeasureHeight: Integer; override;
    function DoMeasureWidth: Integer; override;
  public
    procedure CalculateSubCellsBounds(R: TRect); override;
  end;

  { TdxColorPickerCustomSpinEditViewInfoCell }

  TdxColorPickerCustomSpinEditViewInfoCell = class(TdxColorPickerCustomEditViewInfoCell)
  private
    function GetEdit: TcxSpinEdit; inline;
    function GetEditValue: Integer; inline;
    procedure SetEditValue(AValue: Integer); inline;
  protected
    function GetEditClass: TcxCustomEditClass; override;
    procedure InitializeEdit; override;
    function MeasureEditWidth: Integer; override;
  public
    property Edit: TcxSpinEdit read GetEdit;
    property EditValue: Integer read GetEditValue write SetEditValue;
  end;

  { TdxColorPickerAlphaEditViewInfoCell }

  TdxColorPickerAlphaEditViewInfoCell = class(TdxColorPickerCustomSpinEditViewInfoCell)
  protected
    procedure EditValueChanged; override;
    function GetCaption: string; override;
    procedure UpdateEditValue; override;
  end;

  { TdxColorPickerHueEditViewInfoCell }

  TdxColorPickerHueEditViewInfoCell = class(TdxColorPickerCustomSpinEditViewInfoCell)
  protected
    procedure EditValueChanged; override;
    function GetCaption: string; override;
    procedure UpdateEditValue; override;
  end;

  { TdxColorPickerSaturationEditViewInfoCell }

  TdxColorPickerSaturationEditViewInfoCell = class(TdxColorPickerCustomSpinEditViewInfoCell)
  protected
    procedure EditValueChanged; override;
    function GetCaption: string; override;
    procedure UpdateEditValue; override;
  end;

  { TdxColorPickerLigthnessEditViewInfoCell }

  TdxColorPickerLigthnessEditViewInfoCell = class(TdxColorPickerCustomSpinEditViewInfoCell)
  protected
    procedure EditValueChanged; override;
    function GetCaption: string; override;
    procedure UpdateEditValue; override;
  end;

  { TdxColorPickerRedEditViewInfoCell }

  TdxColorPickerRedEditViewInfoCell = class(TdxColorPickerCustomSpinEditViewInfoCell)
  protected
    procedure EditValueChanged; override;
    function GetCaption: string; override;
    procedure UpdateEditValue; override;
  end;

  { TdxColorPickerGreenEditViewInfoCell }

  TdxColorPickerGreenEditViewInfoCell = class(TdxColorPickerCustomSpinEditViewInfoCell)
  protected
    procedure EditValueChanged; override;
    function GetCaption: string; override;
    procedure UpdateEditValue; override;
  end;

  { TdxColorPickerBlueEditViewInfoCell }

  TdxColorPickerBlueEditViewInfoCell = class(TdxColorPickerCustomSpinEditViewInfoCell)
  protected
    procedure EditValueChanged; override;
    function GetCaption: string; override;
    procedure UpdateEditValue; override;
  end;

  { TdxColorPickerHSLEditViewInfoCell }

  TdxColorPickerHSLEditViewInfoCell = class(TdxColorPickerCustomEditBoxViewInfoCell)
  protected
    procedure CreateSubCells; override;
  end;

  { TdxColorPickerRGBEditViewInfoCell }

  TdxColorPickerRGBEditViewInfoCell = class(TdxColorPickerCustomEditBoxViewInfoCell)
  protected
    procedure CreateSubCells; override;
  end;

  { TdxColorPickerRGBHexEditViewInfoCell }

  TdxColorPickerRGBHexEditViewInfoCell = class(TdxColorPickerCustomEditViewInfoCell)
  private
    function GetEdit: TcxMaskEdit; inline;
    function GetHexNotation: TdxColorPickerRGBHexNotation; inline;
  protected
    procedure EditValueChanged; override;
    function GetCaption: string; override;
    function GetEditClass: TcxCustomEditClass; override;
    procedure InitializeEdit; override;
    function MeasureEditWidth: Integer; override;
    procedure UpdateEditValue; override;
  public
    property Edit: TcxMaskEdit read GetEdit;
    property HexNotation: TdxColorPickerRGBHexNotation read GetHexNotation;
  end;

  { TdxColorPickerViewInfo }

  TdxColorPickerViewInfo = class(TdxColorPickerCustomContainerViewInfoCell)
  private
    function GetContentOffset: Integer;
    function GetEditBoxWidth: Integer;
    function GetOptionsView: TdxColorPickerOptionsView; inline;
  protected
    FAlphaEdit: TdxColorPickerCustomEditViewInfoCell;
    FGamut: TdxColorPickerCustomGamutViewInfoCell;
    FHSLEditors: TdxColorPickerCustomEditBoxViewInfoCell;
    FPreview: TdxColorPickerPreviewViewInfoCell;
    FRGBEditors: TdxColorPickerCustomEditBoxViewInfoCell;
    FRGBHexEdit: TdxColorPickerCustomEditViewInfoCell;
    FSlider1: TdxColorPickerCustomSliderViewInfoCell;
    FSlider2: TdxColorPickerCustomSliderViewInfoCell;

    procedure CalculateEditBoxes(var R: TRect); virtual;
    procedure CreateSubCells; override;
    function DoMeasureHeight: Integer; override;
    function DoMeasureWidth: Integer; override;
  public
    procedure CalculateSubCellsBounds(R: TRect); override;
    //
    property OptionsView: TdxColorPickerOptionsView read GetOptionsView;
  end;

  { TdxColorPickerController }

  TdxColorPickerController = class(TdxColorPickerCustomObject)
  private
    FCapturedCell: TdxColorPickerCustomViewInfoCell;
    FColorInfo: TdxColorPickerColorInfo;
    FHitTest: TdxColorPickerHitTest;
    FListeners: TInterfaceList;
    procedure ColorInfoChangeHandler(Sender: TObject);
  protected
    function CreateColorInfo: TdxColorPickerColorInfo; virtual;
    function CreateHitTest: TdxColorPickerHitTest; virtual;
  public
    constructor Create(AControl: TdxCustomColorPicker); override;
    destructor Destroy; override;
    procedure ListenerAdd(AListener: IdxColorPickerChangeListener);
    procedure ListenerRemove(AListener: IdxColorPickerChangeListener);
    //
    procedure MouseDown(AButton: TMouseButton; AShift: TShiftState; X, Y: Integer); virtual;
    procedure MouseMove(AShift: TShiftState; X, Y: Integer); virtual;
    procedure MouseUp(AButton: TMouseButton; AShift: TShiftState; X, Y: Integer); virtual;
    //
    property CapturedCell: TdxColorPickerCustomViewInfoCell read FCapturedCell write FCapturedCell;
    property ColorInfo: TdxColorPickerColorInfo read FColorInfo;
    property HitTest: TdxColorPickerHitTest read FHitTest;
  end;

  { TdxColorPickerOptionsView }

  TdxColorPickerOptionsView = class(TcxOwnedPersistent)
  private
    FAllowEditAlpha: Boolean;
    FRGBHexNotation: TdxColorPickerRGBHexNotation;
    FShowHSLEditors: Boolean;
    FShowPreview: Boolean;
    FShowRGBEditors: Boolean;
    FShowRGBHexEditor: Boolean;
    FStyle: TdxColorPickerStyle;

    FOnChanged: TNotifyEvent;

    procedure SetAllowEditAlpha(AValue: Boolean);
    procedure SetRGBHexNotation(AValue: TdxColorPickerRGBHexNotation);
    procedure SetShowHSLEditors(AValue: Boolean);
    procedure SetShowPreview(AValue: Boolean);
    procedure SetShowRGBEditors(AValue: Boolean);
    procedure SetShowRGBHexEditor(AValue: Boolean);
    procedure SetStyle(AValue: TdxColorPickerStyle);
  protected
    procedure Changed; virtual;
    procedure DoAssign(Source: TPersistent); override;
    //
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  public
    constructor Create(AOwner: TPersistent); override;
  published
    property AllowEditAlpha: Boolean read FAllowEditAlpha write SetAllowEditAlpha default True;
    property RGBHexNotation: TdxColorPickerRGBHexNotation read FRGBHexNotation write SetRGBHexNotation default cphnHTML;
    property ShowHSLEditors: Boolean read FShowHSLEditors write SetShowHSLEditors default True;
    property ShowPreview: Boolean read FShowPreview write SetShowPreview default True;
    property ShowRGBEditors: Boolean read FShowRGBEditors write SetShowRGBEditors default True;
    property ShowRGBHexEditor: Boolean read FShowRGBHexEditor write SetShowRGBHexEditor default True;
    property Style: TdxColorPickerStyle read FStyle write SetStyle default cpsGamutAndLightnessSlider;
  end;

  { TdxCustomColorPicker }

  TdxCustomColorPicker = class(TcxControl)
  private
    FColor: TdxAlphaColor;
    FController: TdxColorPickerController;
    FLockUpdate: Integer;
    FOptionsView: TdxColorPickerOptionsView;
    FViewInfo: TdxColorPickerViewInfo;

    FOnColorChanged: TNotifyEvent;

    function IsColorStored: Boolean;
    procedure OptionsViewChangeHandler(Sender: TObject);
    procedure SetColor(AValue: TdxAlphaColor);
    procedure SetOptionsView(AValue: TdxColorPickerOptionsView);
  protected
    procedure BoundsChanged; override;
    function CanAutoSize(var NewWidth: Integer; var NewHeight: Integer): Boolean; override;
    function CanResize(var NewWidth: Integer; var NewHeight: Integer): Boolean; override;
    procedure CheckColor(var AValue: TdxAlphaColor); virtual;
    function CreateController: TdxColorPickerController; virtual;
    procedure CreateHandle; override;
    function CreateOptionsView: TdxColorPickerOptionsView; virtual;
    function CreateViewInfo: TdxColorPickerViewInfo; virtual;
    procedure DoColorChanged; virtual;
    procedure DoPaint; override;
    procedure EraseBackground(ACanvas: TcxCanvas; const ARect: TRect); override;
    procedure FontChanged; override;
    function GetMinHeight: Integer;
    function GetMinWidth: Integer;
    function HasBackground: Boolean; override;
    function IsTransparentBackground: Boolean; override;
    procedure LayoutChanged(AType: TdxChangeType = ctHard); virtual;
    procedure Loaded; override;
    procedure LookAndFeelChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    function NeedRedrawOnResize: Boolean; override;

    property AutoSize default True;
    property BorderStyle default cxcbsDefault;
    property Color: TdxAlphaColor read FColor write SetColor stored IsColorStored;
    property Controller: TdxColorPickerController read FController;
    property OptionsView: TdxColorPickerOptionsView read FOptionsView write SetOptionsView;
    property ViewInfo: TdxColorPickerViewInfo read FViewInfo;

    property OnColorChanged: TNotifyEvent read FOnColorChanged write FOnColorChanged;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure FullRefresh;
    procedure TranslationChanged; override;
  end;

  { TdxColorPicker }

  TdxColorPicker = class(TdxCustomColorPicker)
  published
    property Align;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property Enabled;
    property Font;
    property ParentBiDiMode;
    property PopupMenu;
    property Visible;

    property BorderStyle;
    property Color;
    property LookAndFeel;
    property OptionsView;
    property Transparent;

    property OnClick;
    property OnColorChanged;
    property OnContextPopup;
    property OnDblClick;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

implementation

uses
  SysUtils, StrUtils, cxEditConsts;

const
  dxcpSliderArrowWidth = 6;
  dxcpIndentBetweenElements = 6;

type
  TcxCustomEditAccess = class(TcxCustomEdit);

{ TdxColorPickerCustomObject }

constructor TdxColorPickerCustomObject.Create(AControl: TdxCustomColorPicker);
begin
  inherited Create;
  FControl := AControl;
end;

function TdxColorPickerCustomObject.GetScaleFactor: TdxScaleFactor;
begin
  Result := Control.ScaleFactor;
end;

function TdxColorPickerCustomObject.GetUseRightToLeftAlignment: Boolean;
begin
  Result := Control.UseRightToLeftAlignment;
end;

{ TdxColorPickerCustomOptions }

procedure TdxColorPickerCustomOptions.Changed(AType: TdxChangeType = ctHard);
begin
  (Owner as TdxCustomColorPicker).LayoutChanged(AType);
end;

{ TdxColorPickerColorInfo }

procedure TdxColorPickerColorInfo.Changed;
begin
  dxCallNotify(OnChanged, Self);
end;

function TdxColorPickerColorInfo.GetAlphaColor: TdxAlphaColor;
begin
  Result := dxColorToAlphaColor(Color, Alpha);
end;

function TdxColorPickerColorInfo.GetColor: TColor;
begin
  Result := TdxColorSpaceConverter.HSLToColor(FHSL);
end;

function TdxColorPickerColorInfo.GetGrayScale: Double;
begin
  Result := R * 0.3 + G * 0.59 + B * 0.11;
end;

function TdxColorPickerColorInfo.GetR: Byte;
begin
  Result := GetRValue(Color);
end;

function TdxColorPickerColorInfo.GetG: Byte;
begin
  Result := GetGValue(Color);
end;

function TdxColorPickerColorInfo.GetB: Byte;
begin
  Result := GetBValue(Color);
end;

procedure TdxColorPickerColorInfo.SetAlpha(AValue: Byte);
begin
  if FAlpha <> AValue then
  begin
    FAlpha := AValue;
    Changed;
  end;
end;

procedure TdxColorPickerColorInfo.SetAlphaColor(AValue: TdxAlphaColor);
begin
  if AlphaColor <> AValue then
  begin
    FHSL := TdxColorSpaceConverter.ColorToHSL(dxAlphaColorToColor(AValue, FAlpha));
    Changed;
  end;
end;

procedure TdxColorPickerColorInfo.SetColor(const AValue: TColor);
begin
  AlphaColor := dxColorToAlphaColor(AValue, Alpha);
end;

procedure TdxColorPickerColorInfo.SetB(const AValue: Byte);
begin
  AlphaColor := dxMakeAlphaColor(Alpha, R, G, AValue);
end;

procedure TdxColorPickerColorInfo.SetG(const AValue: Byte);
begin
  AlphaColor := dxMakeAlphaColor(Alpha, R, AValue, B);
end;

procedure TdxColorPickerColorInfo.SetR(const AValue: Byte);
begin
  AlphaColor := dxMakeAlphaColor(Alpha, AValue, G, B);
end;

procedure TdxColorPickerColorInfo.SetH(const AValue: Double);
begin
  if H <> AValue then
  begin
    FHSL.H := AValue;
    Changed;
  end;
end;

procedure TdxColorPickerColorInfo.SetL(const AValue: Double);
begin
  if L <> AValue then
  begin
    FHSL.L := AValue;
    Changed;
  end;
end;

procedure TdxColorPickerColorInfo.SetS(const AValue: Double);
begin
  if S <> AValue then
  begin
    FHSL.S := AValue;
    Changed;
  end;
end;

{ TdxColorPickerHitTest }

procedure TdxColorPickerHitTest.Calculate;
begin
  FHitObject := nil;
  Control.ViewInfo.CalculateHitTest(Self);
end;

procedure TdxColorPickerHitTest.Calculate(const AHitPoint: TPoint);
begin
  FHitPoint := AHitPoint;
  Calculate;
end;

procedure TdxColorPickerHitTest.Calculate(X, Y: Integer);
begin
  Calculate(Point(X, Y));
end;

function TdxColorPickerHitTest.GetHitObjectAsViewInfoCell: TdxColorPickerCustomViewInfoCell;
begin
  if HitObjectIsViewInfoCell then
    Result := TdxColorPickerCustomViewInfoCell(HitObject)
  else
    Result := nil;
end;

function TdxColorPickerHitTest.GetHitObjectIsViewInfoCell: Boolean;
begin
  Result := HitObject is TdxColorPickerCustomViewInfoCell;
end;

{ TdxColorPickerCustomViewInfoCell }

constructor TdxColorPickerCustomViewInfoCell.Create(AControl: TdxCustomColorPicker);
begin
  inherited Create(AControl);
  FMeasureHeight := -1;
  FMeasureWidth := -1;
end;

procedure TdxColorPickerCustomViewInfoCell.Calculate(AType: TdxChangeType);
begin
  if AType >= ctMedium then
  begin
    FMeasureHeight := -1;
    FMeasureWidth := -1;
  end;
end;

procedure TdxColorPickerCustomViewInfoCell.CalculateHitTest(AHitTest: TdxColorPickerHitTest);
begin
  if cxRectPtIn(Bounds, AHitTest.HitPoint) then
    AHitTest.HitObject := Self;
end;

procedure TdxColorPickerCustomViewInfoCell.DragMove(X, Y: Integer);
begin
  // do nothing
end;

procedure TdxColorPickerCustomViewInfoCell.Draw(ACanvas: TcxCanvas);
begin
  if ACanvas.RectVisible(Bounds) then
  begin
    ACanvas.SaveClipRegion;
    try
      ACanvas.IntersectClipRect(Bounds);
      DoDraw(ACanvas);
    finally
      ACanvas.RestoreClipRegion;
    end;
  end;
end;

procedure TdxColorPickerCustomViewInfoCell.Invalidate;
begin
  Control.InvalidateRect(Bounds, True);
end;

function TdxColorPickerCustomViewInfoCell.MeasureHeight: Integer;
begin
  if FMeasureHeight < 0 then
    FMeasureHeight := DoMeasureHeight;
  Result := FMeasureHeight;
end;

function TdxColorPickerCustomViewInfoCell.MeasureWidth: Integer;
begin
  if FMeasureWidth < 0 then
    FMeasureWidth := DoMeasureWidth;
  Result := FMeasureWidth;
end;

procedure TdxColorPickerCustomViewInfoCell.DoDraw(ACanvas: TcxCanvas);
begin
  // do nothing
end;

function TdxColorPickerCustomViewInfoCell.DoMeasureHeight: Integer;
begin
  Result := 0;
end;

function TdxColorPickerCustomViewInfoCell.DoMeasureWidth: Integer;
begin
  Result := 0;
end;

function TdxColorPickerCustomViewInfoCell.GetController: TdxColorPickerController;
begin
  Result := Control.Controller;
end;

function TdxColorPickerCustomViewInfoCell.GetIsCaptured: Boolean;
begin
  Result := Controller.CapturedCell = Self;
end;

function TdxColorPickerCustomViewInfoCell.GetPainter: TcxCustomLookAndFeelPainter;
begin
  Result := Control.LookAndFeelPainter;
end;

procedure TdxColorPickerCustomViewInfoCell.SetBounds(const AValue: TRect);
begin
  if not cxRectIsEqual(AValue, Bounds) then
  begin
    FBounds := AValue;
    Calculate(ctMedium);
  end;
end;

{ TdxColorPickerCustomViewInfoCells }

procedure TdxColorPickerCustomViewInfoCells.Add(ACell: TdxColorPickerCustomViewInfoCell);
begin
  inherited Add(ACell);
end;

procedure TdxColorPickerCustomViewInfoCells.Calculate(AType: TdxChangeType);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].Calculate(AType);
end;

procedure TdxColorPickerCustomViewInfoCells.CalculateHitTest(AHitTest: TdxColorPickerHitTest);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].CalculateHitTest(AHitTest);
end;

procedure TdxColorPickerCustomViewInfoCells.Draw(ACanvas: TcxCanvas);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].Draw(ACanvas);
end;

function TdxColorPickerCustomViewInfoCells.GetItem(AIndex: Integer): TdxColorPickerCustomViewInfoCell;
begin
  Result := TdxColorPickerCustomViewInfoCell(inherited Items[AIndex]);
end;

{ TdxColorPickerCustomContainerViewInfoCell }

constructor TdxColorPickerCustomContainerViewInfoCell.Create(AControl: TdxCustomColorPicker);
begin
  inherited Create(AControl);
  FCells := TdxColorPickerCustomViewInfoCells.Create;
  CreateSubCells;
end;

destructor TdxColorPickerCustomContainerViewInfoCell.Destroy;
begin
  FreeAndNil(FCells);
  inherited Destroy;
end;

procedure TdxColorPickerCustomContainerViewInfoCell.Calculate(AType: TdxChangeType);
begin
  inherited Calculate(AType);
  if AType = ctHard then
  begin
    Cells.Clear;
    CreateSubCells;
  end;
  CalculateSubCellsBounds(Bounds);
  if AType <> ctHard then
    Cells.Calculate(AType);
end;

procedure TdxColorPickerCustomContainerViewInfoCell.CalculateHitTest(AHitTest: TdxColorPickerHitTest);
begin
  inherited CalculateHitTest(AHitTest);
  Cells.CalculateHitTest(AHitTest);
end;

procedure TdxColorPickerCustomContainerViewInfoCell.CreateCell(ACellClass: TdxColorPickerCustomViewInfoCellClass);
var
  ACell: TObject;
begin
  CreateCell(ACellClass, ACell);
end;

procedure TdxColorPickerCustomContainerViewInfoCell.CreateCell(ACellClass: TdxColorPickerCustomViewInfoCellClass; var ACell);
var
  ATemp: TdxColorPickerCustomViewInfoCell;
begin
  ATemp := ACellClass.Create(Control);
  Cells.Add(ATemp);
  TObject(ACell) := ATemp;
end;

procedure TdxColorPickerCustomContainerViewInfoCell.DoDraw(ACanvas: TcxCanvas);
begin
  Cells.Draw(ACanvas);
end;

{ TdxColorPickerCustomColorModifierViewInfoCell }

constructor TdxColorPickerCustomColorModifierViewInfoCell.Create(AControl: TdxCustomColorPicker);
begin
  inherited Create(AControl);
  Controller.ListenerAdd(Self);
end;

destructor TdxColorPickerCustomColorModifierViewInfoCell.Destroy;
begin
  Controller.ListenerRemove(Self);
  inherited Destroy;
end;

procedure TdxColorPickerCustomColorModifierViewInfoCell.Calculate(AType: TdxChangeType);
begin
  inherited Calculate(AType);
  if AType >= ctMedium then
    UpdateEditValue;
end;

procedure TdxColorPickerCustomColorModifierViewInfoCell.ColorChanged;
begin
  if not IsCaptured then
    UpdateEditValue;
end;

function TdxColorPickerCustomColorModifierViewInfoCell.GetColorInfo: TdxColorPickerColorInfo;
begin
  Result := Controller.ColorInfo;
end;

{ TdxColorPickerCustomVisualColorModifierViewInfoCell }

destructor TdxColorPickerCustomVisualColorModifierViewInfoCell.Destroy;
begin
  FlushContentCache;
  inherited Destroy;
end;

procedure TdxColorPickerCustomVisualColorModifierViewInfoCell.Calculate(AType: TdxChangeType);
begin
  inherited Calculate(AType);
  if (FContentCache = nil) or (AType = ctHard) or
    (FContentCache.Width <> cxRectWidth(ContentBounds)) or
    (FContentCache.Height <> cxRectHeight(ContentBounds)) then
  begin
    FlushContentCache;
    UpdateEditValue;
  end;
end;

procedure TdxColorPickerCustomVisualColorModifierViewInfoCell.DoDraw(ACanvas: TcxCanvas);
begin
  if not cxRectIsEmpty(ContentFrameRect) then
    Painter.DrawBorder(ACanvas, ContentFrameRect);
  if not cxRectIsEmpty(ContentBounds) then
  begin
    ACanvas.SaveClipRegion;
    try
      ACanvas.IntersectClipRect(ContentBounds);
      if FContentCache = nil then
        UpdateContentCache;
      DrawContent(ACanvas);
    finally
      ACanvas.RestoreClipRegion;
    end;
  end;
end;

procedure TdxColorPickerCustomVisualColorModifierViewInfoCell.DrawContent(ACanvas: TcxCanvas);
begin
  ACanvas.Draw(ContentBounds.Left, ContentBounds.Top, FContentCache);
end;

procedure TdxColorPickerCustomVisualColorModifierViewInfoCell.FlushContentCache;
begin
  FreeAndNil(FContentCache);
end;

function TdxColorPickerCustomVisualColorModifierViewInfoCell.GetContentBounds: TRect;
begin
  Result := cxRectInflate(Bounds, -Painter.BorderSize);
end;

function TdxColorPickerCustomVisualColorModifierViewInfoCell.GetContentFrameRect: TRect;
begin
  Result := cxRectInflate(ContentBounds, Painter.BorderSize);
end;

function TdxColorPickerCustomVisualColorModifierViewInfoCell.GetCursorColor: TColor;
begin
  Result := clBlack;
end;

procedure TdxColorPickerCustomVisualColorModifierViewInfoCell.UpdateContentCache;
begin
  FlushContentCache;
  FContentCache := TcxBitmap32.CreateSize(cxRectWidth(ContentBounds), cxRectHeight(ContentBounds), True);
  UpdateContentCache(FContentCache.cxCanvas, FContentCache.Width, FContentCache.Height);
end;

procedure TdxColorPickerCustomVisualColorModifierViewInfoCell.UpdateEditValue;
begin
  CursorPosition := CalculateCursorPosition;
  FlushContentCache;
  Invalidate;
end;

procedure TdxColorPickerCustomVisualColorModifierViewInfoCell.SetCursorPosition(const AValue: TPoint);
begin
  if not cxPointIsEqual(FCursorPosition, AValue) then
  begin
    FCursorPosition := AValue;
    Invalidate;
  end;
end;

{ TdxColorPickerCustomGamutViewInfoCell }

function TdxColorPickerCustomGamutViewInfoCell.DoMeasureHeight: Integer;
begin
  Result := MeasureWidth;
end;

function TdxColorPickerCustomGamutViewInfoCell.DoMeasureWidth: Integer;
begin
  Result := ScaleFactor.Apply(200);
end;

procedure TdxColorPickerCustomGamutViewInfoCell.DrawContent(ACanvas: TcxCanvas);
begin
  inherited DrawContent(ACanvas);
  DrawCursor(ACanvas);
end;

procedure TdxColorPickerCustomGamutViewInfoCell.DrawCursor(ACanvas: TcxCanvas);
const
  LineThin = 3;
  LineWidth = 5;
  AreaSize = (LineWidth + LineThin) * 2 + LineThin;
var
  R: TRect;
begin
  R := cxRectBounds(CursorPosition.X - AreaSize div 2, CursorPosition.Y - AreaSize div 2, AreaSize, AreaSize);
  R := cxRectOffset(R, ContentBounds.TopLeft);

  ACanvas.Brush.Color := GetCursorColor;
  ACanvas.FillRect(cxRectSetWidth(cxRectCenterVertically(R, LineThin), LineWidth));
  ACanvas.FillRect(cxRectSetRight(cxRectCenterVertically(R, LineThin), R.Right, LineWidth));
  ACanvas.FillRect(cxRectSetHeight(cxRectCenterHorizontally(R, LineThin), LineWidth));
  ACanvas.FillRect(cxRectSetBottom(cxRectCenterHorizontally(R, LineThin), R.Bottom, LineWidth));
end;

{ TdxColorPickerCustomSliderViewInfoCell }

procedure TdxColorPickerCustomSliderViewInfoCell.DoDraw(ACanvas: TcxCanvas);
begin
  inherited DoDraw(ACanvas);
  DrawArrow(ACanvas);
end;

function TdxColorPickerCustomSliderViewInfoCell.DoMeasureHeight: Integer;
begin
  Result := ScaleFactor.Apply(20);
end;

function TdxColorPickerCustomSliderViewInfoCell.DoMeasureWidth: Integer;
begin
  Result := ScaleFactor.Apply(20);
end;

procedure TdxColorPickerCustomSliderViewInfoCell.DrawArrow(ACanvas: TcxCanvas);
var
  R1, R2: TRect;
  I: Integer;
begin
  R1 := ArrowBounds;
  R2 := cxRectSetRight(R1, R1.Right, 1);
  ACanvas.Brush.Color := Painter.DefaultContentTextColor;
  for I := R1.Left to R1.Right - 1 do
  begin
    ACanvas.FillRect(R2);
    R2 := cxRectInflate(R2, 0, -1);
    R2 := cxRectOffset(R2, -1, 0);
  end;
end;

function TdxColorPickerCustomSliderViewInfoCell.GetArrowBounds: TRect;
begin
  Result := cxRectSetRight(Bounds, Bounds.Right, ScaleFactor.Apply(dxcpSliderArrowWidth));
  Result := cxRectSetHeight(Result, ScaleFactor.Apply(dxcpSliderArrowWidth) * 2 - 1);
  Result := cxRectOffset(Result, 0, CursorPosition.Y);
end;

function TdxColorPickerCustomSliderViewInfoCell.GetContentBounds: TRect;
begin
  Result := inherited GetContentBounds;
  Result := cxRectInflate(Result, 0, -ScaleFactor.Apply(dxcpSliderArrowWidth) + Painter.BorderSize);
  Result.Right := ArrowBounds.Left - ScaleFactor.Apply(cxTextOffset) - Painter.BorderSize;
end;

{ TdxColorPickerAlphaSliderViewInfoCell }

function TdxColorPickerAlphaSliderViewInfoCell.CalculateCursorPosition: TPoint;
begin
  Result := Point(0, MulDiv(cxRectHeight(ContentBounds), ColorInfo.Alpha, MaxByte));
end;

procedure TdxColorPickerAlphaSliderViewInfoCell.DragMove(X, Y: Integer);
begin
  Y := Min(cxRectHeight(ContentBounds), Max(0, Y - ContentBounds.Top));
  ColorInfo.Alpha := Round(MaxByte * Y / cxRectHeight(ContentBounds));
  CursorPosition := Point(0, Y);
end;

procedure TdxColorPickerAlphaSliderViewInfoCell.UpdateContentCache(ACanvas: TcxCanvas; AWidth, AHeight: Integer);
begin
  cxDrawTransparencyCheckerboard(ACanvas, Rect(0, 0, AWidth, AHeight), 2);
  dxGpFillRectByGradient(ACanvas.Handle, Rect(0, 0, AWidth, AHeight),
    clNone, ColorInfo.Color, LinearGradientModeVertical, 0);
end;

{ TdxColorPickerGamutAndLightnessSliderStyleGamutViewInfoCell }

procedure TdxColorPickerGamutAndLightnessSliderStyleGamutViewInfoCell.DragMove(X, Y: Integer);
begin
  X := Min(cxRectWidth(ContentBounds), Max(0, X - ContentBounds.Left));
  Y := Min(cxRectHeight(ContentBounds), Max(0, Y - ContentBounds.Top));
  ColorInfo.H := X / cxRectWidth(ContentBounds);
  ColorInfo.S := 1 - Y / cxRectHeight(ContentBounds);
  CursorPosition := Point(X, Y);
end;

function TdxColorPickerGamutAndLightnessSliderStyleGamutViewInfoCell.CalculateCursorPosition: TPoint;
begin
  Result := Point(Round(cxRectWidth(ContentBounds) * ColorInfo.H), Round(cxRectHeight(ContentBounds) * (1 - ColorInfo.S)));
end;

procedure TdxColorPickerGamutAndLightnessSliderStyleGamutViewInfoCell.UpdateContentCache(ACanvas: TcxCanvas; AWidth: Integer; AHeight: Integer);
var
  I: Integer;
begin
  for I := 0 to AWidth - 1 do
  begin
    dxGpFillRectByGradient(
      ACanvas.Handle, Rect(I, 0, I + 1, AHeight),
      TdxColorSpaceConverter.HSLToColor(I / AWidth, 1, 0.5),
      TdxColorSpaceConverter.HSLToColor(I / AWidth, 0, 0.5),
      LinearGradientModeVertical);
  end;
end;

{ TdxColorPickerGamutAndLightnessSliderStyleSliderViewInfoCell }

procedure TdxColorPickerGamutAndLightnessSliderStyleSliderViewInfoCell.DragMove(X, Y: Integer);
begin
  Y := Min(cxRectHeight(ContentBounds), Max(0, Y - ContentBounds.Top));
  ColorInfo.L := 1 - Y / cxRectHeight(ContentBounds);
  CursorPosition := Point(0, Y);
end;

function TdxColorPickerGamutAndLightnessSliderStyleSliderViewInfoCell.CalculateCursorPosition: TPoint;
begin
  Result := Point(0, Round((1 - ColorInfo.L) * cxRectHeight(ContentBounds)));
end;

procedure TdxColorPickerGamutAndLightnessSliderStyleSliderViewInfoCell.UpdateContentCache(ACanvas: TcxCanvas; AWidth, AHeight: Integer);
var
  AColor: TColor;
begin
  AColor := TdxColorSpaceConverter.HSLToColor(ColorInfo.H, ColorInfo.S, 0.5);
  dxGpFillRectByGradient(ACanvas.Handle, Rect(0, 0, AWidth, AHeight div 2), clWhite, AColor, LinearGradientModeVertical);
  dxGpFillRectByGradient(ACanvas.Handle, Rect(0, AHeight div 2, AWidth, AHeight), AColor, clBlack, LinearGradientModeVertical);
end;

{ TdxColorPickerGamutAndHueSliderStyleGamutViewInfoCell }

procedure TdxColorPickerGamutAndHueSliderStyleGamutViewInfoCell.DragMove(X, Y: Integer);
begin
  X := Min(cxRectWidth(ContentBounds), Max(0, X - ContentBounds.Left));
  Y := Min(cxRectHeight(ContentBounds), Max(0, Y - ContentBounds.Top));
  ColorInfo.S := X / cxRectWidth(ContentBounds);
  ColorInfo.L := (1 - ColorInfo.S * 0.5) * (1 - Y / cxRectHeight(ContentBounds));
  CursorPosition := Point(X, Y);
end;

function TdxColorPickerGamutAndHueSliderStyleGamutViewInfoCell.CalculateCursorPosition: TPoint;
begin
  Result.X := Round(cxRectWidth(ContentBounds) * ColorInfo.S);
  Result.Y := Round(cxRectHeight(ContentBounds) * (1 - ColorInfo.L / (1 - ColorInfo.S * 0.5)));
end;

function TdxColorPickerGamutAndHueSliderStyleGamutViewInfoCell.GetCursorColor: TColor;
begin
  if ColorInfo.GrayScale > 92 then
    Result := clBlack
  else
    Result := clWhite;
end;

procedure TdxColorPickerGamutAndHueSliderStyleGamutViewInfoCell.UpdateContentCache(ACanvas: TcxCanvas; AWidth, AHeight: Integer);
var
  I: Integer;
begin
  for I := 0 to AHeight - 1 do
  begin
    dxGpFillRectByGradient(ACanvas.Handle, Rect(0, I, AWidth, I + 1),
      TdxColorSpaceConverter.HSLToColor(ColorInfo.H, 0, 1 - I / AHeight),
      TdxColorSpaceConverter.HSLToColor(ColorInfo.H, 1, 0.5 * (1 - I / AHeight)),
      LinearGradientModeHorizontal);
  end;
end;

{ TdxColorPickerGamutAndHueSliderStyleSliderViewInfoCell }

procedure TdxColorPickerGamutAndHueSliderStyleSliderViewInfoCell.DragMove(X, Y: Integer);
begin
  Y := Min(cxRectHeight(ContentBounds), Max(0, Y - ContentBounds.Top));
  ColorInfo.H := Y / cxRectHeight(ContentBounds);
  CursorPosition := Point(0, Y);
end;

function TdxColorPickerGamutAndHueSliderStyleSliderViewInfoCell.CalculateCursorPosition: TPoint;
begin
  Result := Point(0, Round(ColorInfo.H * cxRectHeight(ContentBounds)));
end;

procedure TdxColorPickerGamutAndHueSliderStyleSliderViewInfoCell.UpdateContentCache(ACanvas: TcxCanvas; AWidth, AHeight: Integer);
var
  I: Integer;
begin
  for I := 0 to AHeight - 1 do
  begin
    dxGpFillRectByGradient(
      ACanvas.Handle, Rect(0, I, AWidth, I + 1),
      TdxColorSpaceConverter.HSLToColor(I / AHeight, 1, 0.5),
      TdxColorSpaceConverter.HSLToColor(I / AHeight, 1, 0.5),
      LinearGradientModeVertical);
  end;
end;

{ TdxColorPickerPreviewViewInfoCell }

constructor TdxColorPickerPreviewViewInfoCell.Create(AControl: TdxCustomColorPicker);
begin
  inherited Create(AControl);
  Controller.ListenerAdd(Self);
end;

destructor TdxColorPickerPreviewViewInfoCell.Destroy;
begin
  Controller.ListenerRemove(Self);
  inherited Destroy;
end;

procedure TdxColorPickerPreviewViewInfoCell.DoDraw(ACanvas: TcxCanvas);
var
  R: TRect;
begin
  R := Bounds;
  Painter.DrawBorder(ACanvas, R);
  R := cxRectInflate(R, -Painter.BorderSize);
  cxDrawTransparencyCheckerboard(ACanvas, R, 2);
  dxGpFillRect(ACanvas.Handle, R, Controller.ColorInfo.Color, Controller.ColorInfo.Alpha);
end;

function TdxColorPickerPreviewViewInfoCell.DoMeasureHeight: Integer;
begin
  Result := MeasureWidth;
end;

function TdxColorPickerPreviewViewInfoCell.DoMeasureWidth: Integer;
begin
  Result := ScaleFactor.Apply(48);
end;

procedure TdxColorPickerPreviewViewInfoCell.UpdateEditValue;
begin
  Invalidate;
end;

{ TdxColorPickerCustomEditViewInfoCell }

constructor TdxColorPickerCustomEditViewInfoCell.Create(AControl: TdxCustomColorPicker);
begin
  inherited Create(AControl);
  FEdit := GetEditClass.Create(Control);
  FEdit.Parent := Control;
  FEdit.Style.LookAndFeel.MasterLookAndFeel := Control.LookAndFeel;
  TcxCustomEditAccess(FEdit).Properties.OnChange := ChangeHandler;
  if Control.IsDesigning then
    TcxCustomEditAccess(FEdit).SetDesigning(True);
  InitializeEdit;
end;

destructor TdxColorPickerCustomEditViewInfoCell.Destroy;
begin
  FreeAndNil(FEdit);
  inherited Destroy;
end;

procedure TdxColorPickerCustomEditViewInfoCell.AfterConstruction;
begin
  inherited AfterConstruction;
  UpdateEditValue;
end;

procedure TdxColorPickerCustomEditViewInfoCell.Calculate(AType: TdxChangeType);
begin
  inherited Calculate(AType);

  if UseRightToLeftAlignment then
  begin
    Edit.BoundsRect := cxRectSetWidth(Bounds, MeasureEditWidth);
    FCaptionRect := Bounds;
    FCaptionRect.Left := Edit.BoundsRect.Right + cxTextOffset;
  end
  else
  begin
    Edit.BoundsRect := cxRectSetRight(Bounds, Bounds.Right, MeasureEditWidth);
    FCaptionRect := Bounds;
    FCaptionRect.Right := Edit.Left - cxTextOffset;
  end;

  Dec(FCaptionRect.Bottom, cxTextOffset);
end;

function TdxColorPickerCustomEditViewInfoCell.CalculateTextFieldSize(ADigits: Integer): Integer;
const
  AllowedChars: array[0..21] of Char = (
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F', 'a', 'b', 'c', 'd', 'e', 'f'
  );
var
  I: Integer;
begin
  Result := 0;
  cxScreenCanvas.Font := Font;
  for I := 0 to Length(AllowedChars) - 1 do
    Result := Max(Result, cxScreenCanvas.TextWidth(AllowedChars[I]));
  cxScreenCanvas.Dormant;
  Result := Result * ADigits;
end;

procedure TdxColorPickerCustomEditViewInfoCell.DoDraw(ACanvas: TcxCanvas);
const
  AlignMap: array[Boolean] of TAlignment = (taRightJustify, taLeftJustify);
begin
  inherited DoDraw(ACanvas);
  if not cxRectIsEmpty(CaptionRect) then
  begin
    ACanvas.SaveState;
    try
      ACanvas.Font := Font;
      ACanvas.Font.Color := CaptionTextColor;
      ACanvas.Brush.Style := bsClear;
      ACanvas.DrawTexT(Caption, CaptionRect, AlignMap[UseRightToLeftAlignment], vaCenter, False, False);
    finally
      ACanvas.RestoreState;
    end;
  end;
end;

function TdxColorPickerCustomEditViewInfoCell.DoMeasureHeight: Integer;
begin
  Result := Edit.Height;
end;

function TdxColorPickerCustomEditViewInfoCell.DoMeasureWidth: Integer;
begin
  Result := cxTextWidth(Font, Caption) + cxTextOffset + MeasureEditWidth;
end;

procedure TdxColorPickerCustomEditViewInfoCell.InitializeEdit;
begin
  Edit.Style.TransparentBorder := False;
end;

procedure TdxColorPickerCustomEditViewInfoCell.ChangeHandler(Sender: TObject);
begin
  if Edit.Focused then
    EditValueChanged;
end;

function TdxColorPickerCustomEditViewInfoCell.GetCaptionTextColor: TColor;
begin
  if Painter.LookAndFeelStyle = lfsSkin then
    Result := Painter.DefaultContentTextColor
  else
    Result := clDefault;

  if Result = clDefault then
    Result := Font.Color;
end;

function TdxColorPickerCustomEditViewInfoCell.GetFont: TFont;
begin
  Result := Control.Font;
end;

{ TdxColorPickerCustomEditBoxViewInfoCell }

procedure TdxColorPickerCustomEditBoxViewInfoCell.CalculateSubCellsBounds(R: TRect);
var
  ACell: TdxColorPickerCustomViewInfoCell;
  I: Integer;
begin
  for I := 0 to Cells.Count - 1 do
  begin
    ACell := Cells[I];
    ACell.Bounds := cxRectSetHeight(R, ACell.MeasureHeight);
    R.Top := ACell.Bounds.Bottom + ScaleFactor.Apply(dxcpIndentBetweenElements);
  end;
end;

function TdxColorPickerCustomEditBoxViewInfoCell.DoMeasureHeight: Integer;
var
  I: Integer;
begin
  Result := (Cells.Count - 1) * ScaleFactor.Apply(dxcpIndentBetweenElements);
  for I := 0 to Cells.Count - 1 do
    Inc(Result, Cells[I].MeasureHeight);
end;

function TdxColorPickerCustomEditBoxViewInfoCell.DoMeasureWidth: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Cells.Count - 1 do
    Result := Max(Result, Cells[I].MeasureWidth);
end;

{ TdxColorPickerCustomSpinEditViewInfoCell }

function TdxColorPickerCustomSpinEditViewInfoCell.GetEditClass: TcxCustomEditClass;
begin
  Result := TcxSpinEdit;
end;

procedure TdxColorPickerCustomSpinEditViewInfoCell.InitializeEdit;
begin
  inherited InitializeEdit;
  Edit.Properties.MinValue := 0;
  Edit.Properties.MaxValue := 255;
  Edit.Properties.MaxLength := 3;
end;

function TdxColorPickerCustomSpinEditViewInfoCell.MeasureEditWidth: Integer;
begin
  Edit.HandleNeeded;
  Result := CalculateTextFieldSize(3) + 2 * cxTextOffset +
    cxRectWidth(TcxCustomEditAccess(Edit).ViewInfo.Bounds) -
    cxRectWidth(TcxCustomEditAccess(Edit).ViewInfo.InnerEditRect);
end;

function TdxColorPickerCustomSpinEditViewInfoCell.GetEdit: TcxSpinEdit;
begin
  Result := TcxSpinEdit(inherited Edit);
end;

function TdxColorPickerCustomSpinEditViewInfoCell.GetEditValue: Integer;
begin
  Result := Max(Min(Edit.Value, 255), 0);
end;

procedure TdxColorPickerCustomSpinEditViewInfoCell.SetEditValue(AValue: Integer);
begin
  Edit.Value := AValue;
end;

{ TdxColorPickerAlphaEditViewInfoCell }

procedure TdxColorPickerAlphaEditViewInfoCell.EditValueChanged;
begin
  ColorInfo.Alpha := EditValue;
end;

procedure TdxColorPickerAlphaEditViewInfoCell.UpdateEditValue;
begin
  EditValue := ColorInfo.Alpha;
end;

function TdxColorPickerAlphaEditViewInfoCell.GetCaption: string;
begin
  Result := cxGetResourceString(@sdxColorPickerAlphaLabel);
end;

{ TdxColorPickerHueEditViewInfoCell }

procedure TdxColorPickerHueEditViewInfoCell.EditValueChanged;
begin
  ColorInfo.H := EditValue / 255;
end;

function TdxColorPickerHueEditViewInfoCell.GetCaption: string;
begin
  Result := cxGetResourceString(@sdxColorPickerHueLabel);
end;

procedure TdxColorPickerHueEditViewInfoCell.UpdateEditValue;
begin
  EditValue := Round(255 * ColorInfo.H);
end;

{ TdxColorPickerSaturationEditViewInfoCell }

procedure TdxColorPickerSaturationEditViewInfoCell.EditValueChanged;
begin
  ColorInfo.S := EditValue / 255;
end;

function TdxColorPickerSaturationEditViewInfoCell.GetCaption: string;
begin
  Result := cxGetResourceString(@sdxColorPickerSaturationLabel);
end;

procedure TdxColorPickerSaturationEditViewInfoCell.UpdateEditValue;
begin
  EditValue := Round(255 * ColorInfo.S);
end;

{ TdxColorPickerLigthnessEditViewInfoCell }

procedure TdxColorPickerLigthnessEditViewInfoCell.EditValueChanged;
begin
  ColorInfo.L := EditValue / 255;
end;

function TdxColorPickerLigthnessEditViewInfoCell.GetCaption: string;
begin
  Result := cxGetResourceString(@sdxColorPickerLightnessLabel);
end;

procedure TdxColorPickerLigthnessEditViewInfoCell.UpdateEditValue;
begin
  EditValue := Round(255 * ColorInfo.L);
end;

{ TdxColorPickerRedEditViewInfoCell }

procedure TdxColorPickerRedEditViewInfoCell.EditValueChanged;
begin
  ColorInfo.R := EditValue;
end;

function TdxColorPickerRedEditViewInfoCell.GetCaption: string;
begin
  Result := cxGetResourceString(@sdxColorPickerRedLabel);
end;

procedure TdxColorPickerRedEditViewInfoCell.UpdateEditValue;
begin
  EditValue := ColorInfo.R;
end;

{ TdxColorPickerGreenEditViewInfoCell }

procedure TdxColorPickerGreenEditViewInfoCell.EditValueChanged;
begin
  ColorInfo.G := EditValue;
end;

function TdxColorPickerGreenEditViewInfoCell.GetCaption: string;
begin
  Result := cxGetResourceString(@sdxColorPickerGreenLabel);
end;

procedure TdxColorPickerGreenEditViewInfoCell.UpdateEditValue;
begin
  EditValue := ColorInfo.G;
end;

{ TdxColorPickerBlueEditViewInfoCell }

procedure TdxColorPickerBlueEditViewInfoCell.EditValueChanged;
begin
  ColorInfo.B := EditValue;
end;

function TdxColorPickerBlueEditViewInfoCell.GetCaption: string;
begin
  Result := cxGetResourceString(@sdxColorPickerBlueLabel);
end;

procedure TdxColorPickerBlueEditViewInfoCell.UpdateEditValue;
begin
  EditValue := ColorInfo.B;
end;

{ TdxColorPickerHSLEditViewInfoCell }

procedure TdxColorPickerHSLEditViewInfoCell.CreateSubCells;
begin
  CreateCell(TdxColorPickerHueEditViewInfoCell);
  CreateCell(TdxColorPickerSaturationEditViewInfoCell);
  CreateCell(TdxColorPickerLigthnessEditViewInfoCell);
end;

{ TdxColorPickerRGBEditViewInfoCell }

procedure TdxColorPickerRGBEditViewInfoCell.CreateSubCells;
begin
  CreateCell(TdxColorPickerRedEditViewInfoCell);
  CreateCell(TdxColorPickerGreenEditViewInfoCell);
  CreateCell(TdxColorPickerBlueEditViewInfoCell);
end;

{ TdxColorPickerRGBHexEditViewInfoCell }

procedure TdxColorPickerRGBHexEditViewInfoCell.EditValueChanged;
begin
  ColorInfo.AlphaColor := TdxColorHelper.HexCodeToAlphaColor(Edit.EditingValue, HexNotation = cphnDelphi);
end;

function TdxColorPickerRGBHexEditViewInfoCell.GetCaption: string;
begin
  if HexNotation = cphnHTML then
    Result := cxGetResourceString(@sdxColorPickerHexCodeLabel)
  else
    Result := HexDisplayPrefix;
end;

function TdxColorPickerRGBHexEditViewInfoCell.GetEditClass: TcxCustomEditClass;
begin
  Result := TcxMaskEdit;
end;

procedure TdxColorPickerRGBHexEditViewInfoCell.InitializeEdit;
const
  MaskMap: array[Boolean] of string = ('(\d | [A-F] | [a-f]){1,6}', '(\d | [A-F] | [a-f]){1,8}');
begin
  inherited InitializeEdit;
  Edit.Properties.MaskKind := emkRegExpr;
  Edit.Properties.EditMask := MaskMap[Control.OptionsView.AllowEditAlpha];
end;

function TdxColorPickerRGBHexEditViewInfoCell.MeasureEditWidth: Integer;
begin
  Edit.HandleNeeded;
  Result := CalculateTextFieldSize(6 + 2 * Ord(Control.OptionsView.AllowEditAlpha)) + cxTextOffset +
    cxRectWidth(TcxCustomEditAccess(Edit).ViewInfo.Bounds) -
    cxRectWidth(TcxCustomEditAccess(Edit).ViewInfo.InnerEditRect);
end;

procedure TdxColorPickerRGBHexEditViewInfoCell.UpdateEditValue;
begin
  if not Edit.Focused then
  begin
    Edit.EditValue := TdxColorHelper.AlphaColorToHexCode(
      ColorInfo.AlphaColor, HexNotation = cphnDelphi, Control.OptionsView.AllowEditAlpha);
  end;
end;

function TdxColorPickerRGBHexEditViewInfoCell.GetEdit: TcxMaskEdit;
begin
  Result := TcxMaskEdit(inherited Edit);
end;

function TdxColorPickerRGBHexEditViewInfoCell.GetHexNotation: TdxColorPickerRGBHexNotation;
begin
  Result := Control.OptionsView.RGBHexNotation;
end;

{ TdxColorPickerViewInfo }

procedure TdxColorPickerViewInfo.CalculateSubCellsBounds(R: TRect);

  procedure PlaceSlider(ASlider: TdxColorPickerCustomSliderViewInfoCell; var R: TRect; AIndent: Integer);
  var
    R1: TRect;
  begin
    if ASlider <> nil then
    begin
      R1 := cxRectInflate(R, 0, ScaleFactor.Apply(dxcpSliderArrowWidth) - Painter.BorderSize);
      if UseRightToLeftAlignment then
      begin
        ASlider.Bounds := cxRectSetWidth(R1, ASlider.MeasureWidth);
        R.Left := ASlider.Bounds.Right + AIndent;
      end
      else
      begin
        ASlider.Bounds := cxRectSetRight(R1, R1.Right, ASlider.MeasureWidth);
        R.Right := ASlider.Bounds.Left - AIndent;
      end;
    end;
  end;

begin
  R := cxRectInflate(R, -GetContentOffset);
  CalculateEditBoxes(R);
  PlaceSlider(FSlider2, R, ScaleFactor.Apply(dxcpIndentBetweenElements));
  PlaceSlider(FSlider1, R, ScaleFactor.Apply(dxcpIndentBetweenElements) * 2);
  if FGamut <> nil then
    FGamut.Bounds := R;
end;

procedure TdxColorPickerViewInfo.CalculateEditBoxes(var R: TRect);

  procedure CalculateEditBox(var R: TRect; AEdit: TdxColorPickerCustomViewInfoCell);
  begin
    if AEdit <> nil then
    begin
      AEdit.Bounds := cxRectSetHeight(R, AEdit.MeasureHeight);
      R.Top := AEdit.Bounds.Bottom + 2 * ScaleFactor.Apply(dxcpIndentBetweenElements);
    end;
  end;

var
  ARect: TRect;
begin
  if UseRightToLeftAlignment then
  begin
    ARect := cxRectSetLeft(R, R.Left, GetEditBoxWidth);
    R.Left := ARect.Right + ScaleFactor.Apply(dxcpIndentBetweenElements);
  end
  else
  begin
    ARect := cxRectSetRight(R, R.Right, GetEditBoxWidth);
    R.Right := ARect.Left - ScaleFactor.Apply(dxcpIndentBetweenElements);
  end;

  if FPreview <> nil then
  begin
    FPreview.Bounds := cxRectSetHeight(ARect, cxRectWidth(ARect));
    ARect.Top := FPreview.Bounds.Bottom + 2 * ScaleFactor.Apply(dxcpIndentBetweenElements);
  end;

  CalculateEditBox(ARect, FAlphaEdit);
  CalculateEditBox(ARect, FHSLEditors);
  CalculateEditBox(ARect, FRGBEditors);
  CalculateEditBox(ARect, FRGBHexEdit);
end;

procedure TdxColorPickerViewInfo.CreateSubCells;

  procedure InternalCreateCell(AVisible: Boolean; ACellClass: TdxColorPickerCustomViewInfoCellClass; var ACell);
  begin
    if AVisible then
      CreateCell(ACellClass, ACell)
    else
      TObject(ACell) := nil;
  end;

begin
  FGamut := nil;
  FSlider1 := nil;
  case OptionsView.Style of
    cpsGamutAndLightnessSlider:
      begin
        CreateCell(TdxColorPickerGamutAndLightnessSliderStyleGamutViewInfoCell, FGamut);
        CreateCell(TdxColorPickerGamutAndLightnessSliderStyleSliderViewInfoCell, FSlider1);
      end;

    cpsGamutAndHueSlider:
      begin
        CreateCell(TdxColorPickerGamutAndHueSliderStyleGamutViewInfoCell, FGamut);
        CreateCell(TdxColorPickerGamutAndHueSliderStyleSliderViewInfoCell, FSlider1);
      end;
  end;

  InternalCreateCell(OptionsView.AllowEditAlpha, TdxColorPickerAlphaSliderViewInfoCell, FSlider2);
  InternalCreateCell(OptionsView.ShowPreview, TdxColorPickerPreviewViewInfoCell, FPreview);
  InternalCreateCell(OptionsView.AllowEditAlpha and (OptionsView.ShowRGBEditors or OptionsView.ShowHSLEditors),
    TdxColorPickerAlphaEditViewInfoCell, FAlphaEdit);
  InternalCreateCell(OptionsView.ShowHSLEditors, TdxColorPickerHSLEditViewInfoCell, FHSLEditors);
  InternalCreateCell(OptionsView.ShowRGBEditors, TdxColorPickerRGBEditViewInfoCell, FRGBEditors);
  InternalCreateCell(OptionsView.ShowRGBHexEditor, TdxColorPickerRGBHexEditViewInfoCell, FRGBHexEdit);
end;

function TdxColorPickerViewInfo.DoMeasureHeight: Integer;
begin
  Result := -2 * ScaleFactor.Apply(dxcpIndentBetweenElements);
  if FPreview <> nil then
    Inc(Result, GetEditBoxWidth + 2 * ScaleFactor.Apply(dxcpIndentBetweenElements));
  if FAlphaEdit <> nil then
    Inc(Result, FAlphaEdit.MeasureHeight + 2 * ScaleFactor.Apply(dxcpIndentBetweenElements));
  if FHSLEditors <> nil then
    Inc(Result, FHSLEditors.MeasureHeight + 2 * ScaleFactor.Apply(dxcpIndentBetweenElements));
  if FRGBEditors <> nil then
    Inc(Result, FRGBEditors.MeasureHeight + 2 * ScaleFactor.Apply(dxcpIndentBetweenElements));
  if FRGBHexEdit <> nil then
    Inc(Result, FRGBHexEdit.MeasureHeight + 2 * ScaleFactor.Apply(dxcpIndentBetweenElements));
  if FGamut <> nil then
    Result := Max(Result, FGamut.MeasureHeight);
  Inc(Result, 2 * GetContentOffset);
end;

function TdxColorPickerViewInfo.DoMeasureWidth: Integer;
begin
  Result := MeasureHeight + GetEditBoxWidth;
  if FSlider1 <> nil then
    Inc(Result, FSlider1.MeasureWidth + ScaleFactor.Apply(dxcpIndentBetweenElements));
  if FSlider2 <> nil then
    Inc(Result, FSlider2.MeasureWidth + ScaleFactor.Apply(dxcpIndentBetweenElements));
end;

function TdxColorPickerViewInfo.GetContentOffset: Integer;
begin
  Result := ScaleFactor.Apply(dxcpIndentBetweenElements);
  if (FSlider1 <> nil) or (FSlider2 <> nil) then
    Inc(Result, ScaleFactor.Apply(dxcpSliderArrowWidth));
end;

function TdxColorPickerViewInfo.GetEditBoxWidth: Integer;
begin
  Result := 0;
  if FRGBEditors <> nil then
    Result := Max(Result, FRGBEditors.MeasureWidth);
  if FAlphaEdit <> nil then
    Result := Max(Result, FAlphaEdit.MeasureWidth);
  if FHSLEditors <> nil then
    Result := Max(Result, FHSLEditors.MeasureWidth);
  if FRGBHexEdit <> nil then
    Result := Max(Result, FRGBHexEdit.MeasureWidth);
  if (Result = 0) and (FPreview <> nil) then
    Result := FPreview.MeasureWidth;
end;

function TdxColorPickerViewInfo.GetOptionsView: TdxColorPickerOptionsView;
begin
  Result := Control.OptionsView;
end;

{ TdxColorPickerController }

constructor TdxColorPickerController.Create(AControl: TdxCustomColorPicker);
begin
  inherited Create(AControl);
  FListeners := TInterfaceList.Create;
  FHitTest := CreateHitTest;
  FColorInfo := CreateColorInfo;
  FColorInfo.OnChanged := ColorInfoChangeHandler;
end;

destructor TdxColorPickerController.Destroy;
begin
  FreeAndNil(FColorInfo);
  FreeAndNil(FHitTest);
  FreeAndNil(FListeners);
  inherited Destroy;
end;

procedure TdxColorPickerController.ListenerAdd(AListener: IdxColorPickerChangeListener);
begin
  FListeners.Add(AListener);
end;

procedure TdxColorPickerController.ListenerRemove(AListener: IdxColorPickerChangeListener);
begin
  FListeners.Remove(AListener);
end;

procedure TdxColorPickerController.MouseDown(AButton: TMouseButton; AShift: TShiftState; X, Y: Integer);
begin
  Control.MouseCapture := True;
  HitTest.Calculate(X, Y);
  if AButton = mbLeft then
  begin
    CapturedCell := HitTest.HitObjectAsViewInfoCell;
    if CapturedCell <> nil then
      CapturedCell.DragMove(X, Y);
  end;
end;

procedure TdxColorPickerController.MouseMove(AShift: TShiftState; X, Y: Integer);
begin
  if CapturedCell <> nil then
    CapturedCell.DragMove(X, Y);
end;

procedure TdxColorPickerController.MouseUp(AButton: TMouseButton; AShift: TShiftState; X, Y: Integer);
begin
  CapturedCell := nil;
  Control.MouseCapture := False;
end;

function TdxColorPickerController.CreateColorInfo: TdxColorPickerColorInfo;
begin
  Result := TdxColorPickerColorInfo.Create;
end;

function TdxColorPickerController.CreateHitTest: TdxColorPickerHitTest;
begin
  Result := TdxColorPickerHitTest.Create(Control);
end;

procedure TdxColorPickerController.ColorInfoChangeHandler(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to FListeners.Count - 1 do
    (FListeners[I] as IdxColorPickerChangeListener).ColorChanged;
  Control.Color := ColorInfo.AlphaColor;
end;

{ TdxColorPickerOptionsView }

constructor TdxColorPickerOptionsView.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FAllowEditAlpha := True;
  FShowHSLEditors := True;
  FShowRGBEditors := True;
  FShowRGBHexEditor := True;
  FShowPreview := True;
  FRGBHexNotation := cphnHTML;
end;

procedure TdxColorPickerOptionsView.Changed;
begin
  dxCallNotify(OnChanged, Self);
end;

procedure TdxColorPickerOptionsView.DoAssign(Source: TPersistent);
begin
  inherited DoAssign(Source);
  if Source is TdxColorPickerOptionsView then
  begin
    AllowEditAlpha := TdxColorPickerOptionsView(Source).AllowEditAlpha;
    RGBHexNotation := TdxColorPickerOptionsView(Source).RGBHexNotation;
    ShowHSLEditors := TdxColorPickerOptionsView(Source).ShowHSLEditors;
    ShowPreview := TdxColorPickerOptionsView(Source).ShowPreview;
    ShowRGBEditors := TdxColorPickerOptionsView(Source).ShowRGBEditors;
    ShowRGBHexEditor := TdxColorPickerOptionsView(Source).ShowRGBHexEditor;
    Style := TdxColorPickerOptionsView(Source).Style;
  end;
end;

procedure TdxColorPickerOptionsView.SetAllowEditAlpha(AValue: Boolean);
begin
  if FAllowEditAlpha <> AValue then
  begin
    FAllowEditAlpha := AValue;
    Changed;
  end;
end;

procedure TdxColorPickerOptionsView.SetRGBHexNotation(AValue: TdxColorPickerRGBHexNotation);
begin
  if FRGBHexNotation <> AValue then
  begin
    FRGBHexNotation := AValue;
    Changed;
  end;
end;

procedure TdxColorPickerOptionsView.SetShowHSLEditors(AValue: Boolean);
begin
  if FShowHSLEditors <> AValue then
  begin
    FShowHSLEditors := AValue;
    Changed;
  end;
end;

procedure TdxColorPickerOptionsView.SetShowPreview(AValue: Boolean);
begin
  if FShowPreview <> AValue then
  begin
    FShowPreview := AValue;
    Changed;
  end;
end;

procedure TdxColorPickerOptionsView.SetShowRGBEditors(AValue: Boolean);
begin
  if FShowRGBEditors <> AValue then
  begin
    FShowRGBEditors := AValue;
    Changed;
  end;
end;

procedure TdxColorPickerOptionsView.SetShowRGBHexEditor(AValue: Boolean);
begin
  if FShowRGBHexEditor <> AValue then
  begin
    FShowRGBHexEditor := AValue;
    Changed;
  end;
end;

procedure TdxColorPickerOptionsView.SetStyle(AValue: TdxColorPickerStyle);
begin
  if FStyle <> AValue then
  begin
    FStyle := AValue;
    Changed;
  end;
end;

{ TdxCustomColorPicker }

constructor TdxCustomColorPicker.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOptionsView := CreateOptionsView;
  FOptionsView.OnChanged := OptionsViewChangeHandler;
  FController := CreateController;
  FViewInfo := CreateViewInfo;
  Color := dxMakeAlphaColor(clBlack);
  BorderStyle := cxcbsDefault;
  AutoSize := True;
end;

destructor TdxCustomColorPicker.Destroy;
begin
  FreeAndNil(FViewInfo);
  FreeAndNil(FController);
  FreeAndNil(FOptionsView);
  inherited Destroy;
end;

procedure TdxCustomColorPicker.FullRefresh;
begin
  LayoutChanged;
end;

procedure TdxCustomColorPicker.TranslationChanged;
begin
  inherited TranslationChanged;
  FullRefresh;
end;

procedure TdxCustomColorPicker.BoundsChanged;
begin
  inherited BoundsChanged;
  LayoutChanged(ctMedium);
end;

procedure TdxCustomColorPicker.LayoutChanged(AType: TdxChangeType = ctHard);
begin
  if HandleAllocated then
  begin
    Inc(FLockUpdate);
    try
      ViewInfo.Bounds := ClientBounds;
      ViewInfo.Calculate(AType);
    finally
      Dec(FLockUpdate);
    end;
    if AType >= ctMedium then
      AdjustSize;
    Invalidate;
  end;
end;

function TdxCustomColorPicker.CanAutoSize(var NewWidth, NewHeight: Integer): Boolean;
begin
  Result := FLockUpdate = 0;
  if Result then
  begin
    NewWidth := GetMinWidth;
    NewHeight := GetMinHeight;
  end;
end;

function TdxCustomColorPicker.CanResize(var NewWidth, NewHeight: Integer): Boolean;
begin
  Result := True;
  if (FLockUpdate = 0) and not AutoSize then
  begin
    NewHeight := Max(NewHeight, GetMinHeight);
    NewWidth := Max(NewWidth, GetMinWidth);
  end;
end;

procedure TdxCustomColorPicker.CheckColor(var AValue: TdxAlphaColor);
begin
  if not OptionsView.AllowEditAlpha then
    AValue := dxMakeAlphaColor(dxAlphaColorToColor(AValue));
end;

function TdxCustomColorPicker.CreateController: TdxColorPickerController;
begin
  Result := TdxColorPickerController.Create(Self);
end;

procedure TdxCustomColorPicker.CreateHandle;
begin
  inherited CreateHandle;
  LayoutChanged;
end;

function TdxCustomColorPicker.CreateOptionsView: TdxColorPickerOptionsView;
begin
  Result := TdxColorPickerOptionsView.Create(Self);
end;

function TdxCustomColorPicker.CreateViewInfo: TdxColorPickerViewInfo;
begin
  Result := TdxColorPickerViewInfo.Create(Self);
end;

procedure TdxCustomColorPicker.DoColorChanged;
begin
  dxCallNotify(OnColorChanged, Self);
end;

procedure TdxCustomColorPicker.DoPaint;
begin
  inherited DoPaint;
  ViewInfo.Draw(Canvas);
end;

procedure TdxCustomColorPicker.EraseBackground(ACanvas: TcxCanvas; const ARect: TRect);
begin
  if IsTransparentBackground then
    cxDrawTransparentControlBackground(Self, ACanvas, ARect)
  else
    LookAndFeelPainter.DrawPanelContent(ACanvas, ARect, False);
end;

procedure TdxCustomColorPicker.FontChanged;
begin
  inherited FontChanged;
  LayoutChanged(ctMedium);
end;

function TdxCustomColorPicker.GetMinHeight: Integer;
begin
  Result := ViewInfo.MeasureHeight + 2 * BorderSize;
end;

function TdxCustomColorPicker.GetMinWidth: Integer;
begin
  Result := ViewInfo.MeasureWidth + 2 * BorderSize;
end;

function TdxCustomColorPicker.HasBackground: Boolean;
begin
  Result := True;
end;

function TdxCustomColorPicker.IsTransparentBackground: Boolean;
begin
  Result := Transparent;
end;

procedure TdxCustomColorPicker.Loaded;
begin
  inherited Loaded;
  LayoutChanged;
end;

procedure TdxCustomColorPicker.LookAndFeelChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues);
begin
  LayoutChanged(ctMedium);
end;

procedure TdxCustomColorPicker.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  Controller.MouseDown(Button, Shift, X, Y);
end;

procedure TdxCustomColorPicker.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  Controller.MouseMove(Shift, X, Y);
end;

procedure TdxCustomColorPicker.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  Controller.MouseUp(Button, Shift, X, Y);
end;

function TdxCustomColorPicker.NeedRedrawOnResize: Boolean;
begin
  Result := True;
end;

function TdxCustomColorPicker.IsColorStored: Boolean;
begin
  Result := FColor <> dxMakeAlphaColor(clBlack);
end;

procedure TdxCustomColorPicker.OptionsViewChangeHandler(Sender: TObject);
begin
  Color := Color;
  LayoutChanged;
end;

procedure TdxCustomColorPicker.SetColor(AValue: TdxAlphaColor);
begin
  CheckColor(AValue);
  if FColor <> AValue then
  begin
    FColor := AValue;
    Controller.ColorInfo.AlphaColor := Color;
    DoColorChanged;
  end;
end;

procedure TdxCustomColorPicker.SetOptionsView(AValue: TdxColorPickerOptionsView);
begin
  FOptionsView.Assign(AValue);
end;

end.
