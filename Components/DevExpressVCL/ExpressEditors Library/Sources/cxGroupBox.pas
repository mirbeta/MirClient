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

unit cxGroupBox;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Graphics, Forms,
  dxCore, dxMessages, cxControls, cxGraphics, cxClasses, cxLookAndFeels, cxContainer,
  cxEditPaintUtils, cxEdit, cxTextEdit, cxCheckBox, cxLookAndFeelPainters, dxFading;

const
  cxGroupBox_SupportNonClientArea: Boolean = True;

type
  TcxCaptionAlignment = (alTopLeft, alTopCenter, alTopRight,
    alBottomLeft, alBottomCenter, alBottomRight,
    alLeftTop, alLeftCenter, alLeftBottom,
    alRightTop, alRightCenter, alRightBottom,
    alCenterCenter);

  TcxPanelOffice11BackgroundKind = (pobkGradient, pobkOffice11Color, pobkStyleColor);

  { TcxGroupBoxButtonViewInfo }

  TcxGroupBoxButtonViewInfo = class(TcxEditButtonViewInfo)
  public
    Caption: string;
    Enabled: Boolean;
    TextColor: TColor;
    Column, Row: Integer;
    function GetGlyphRect(const AGlyphSize: TSize; AAlignment: TLeftRight; AIsPaintCopy: Boolean): TRect; virtual;
  end;

  { TcxGroupBoxViewInfo }

  TcxCustomGroupBox = class;

  TcxGroupBoxViewInfo = class(TcxCustomTextEditViewInfo)
  private
    FCheckBoxVisible: Boolean;
    FCheckBoxCheckState: TcxCheckBoxState;
    FCheckBoxViewState: TcxEditCheckState;
    FCheckPressed: Boolean;
    FCheckBoxRect: TRect;
    FFocusRect: TRect;
    FMouseFocusingRect: TRect;

    // CheckBox
    procedure CalculateCheckBoxAndTextRects(ACombinedRect: TRect);
    procedure InvalidateCheckBoxRect;
    function PointInCheckBox(X, Y: Integer): Boolean;

    // Get
    function GetCaptionRect(ACanvas: TcxCanvas; const ARect: TRect): TRect;
    function GetCaptionRectIndent: TRect;
    function GetCaptionPosition: TcxGroupBoxCaptionPosition;
    function GetCaptionRectLeftBound: Integer;
    function GetCheckBoxCaptionIndent: Integer;
    function GetCheckBoxRect(ACanvas: TcxCanvas): TRect;
    function GetControlRect: TRect;
    function GetEdit: TcxCustomGroupBox;
    function GetFrameBounds: TRect;
    function GetCombinedCaptionRect: TRect;
    function GetBoundsForPanel: TRect;
    function GetThemeBackgroundRect(ACanvas: TcxCanvas): TRect;

    // Draw
    procedure DrawCheckBox(ACanvas: TcxCanvas);
    procedure DrawHorizontalTextCaption(ACanvas: TcxCanvas);
    procedure DrawVerticalTextCaption(ACanvas: TcxCanvas);
    procedure DrawFrame(ACanvas: TcxCanvas; R: TRect);
    procedure DrawNativeBackground(ACanvas: TcxCanvas; const ACaptionRect: TRect);
    procedure DrawNativeGroupBoxBackground(ACanvas: TcxCanvas);
    procedure DrawNativePanelBackground(ACanvas: TcxCanvas; const ACaptionRect: TRect);
    procedure DrawOffice11PanelBackground(ACanvas: TcxCanvas; const R: TRect);
    procedure DrawParentBackground(ACanvas: TcxCanvas; R: TRect);
    procedure DrawUsualBackground(ACanvas: TcxCanvas);
    procedure InternalDrawBackground(ACanvas: TcxCanvas);
    procedure InternalDrawBackgroundByPainter(ACanvas: TcxCanvas);
  protected
    procedure CalculateFocusRect;
    procedure CalculateRectsForPanel(ACanvas: TcxCanvas);
    procedure CalculateRectsForStandardPainter(ACanvas: TcxCanvas; AShadowWidth: Integer);
    procedure CalculateRectsForSkinPainter(ACanvas: TcxCanvas; AHasNonClientArea: Boolean);

    procedure DrawCaption(ACanvas: TcxCanvas); virtual;
    function GetBiDiModeCaptionPosition: TcxGroupBoxCaptionPosition;
    function GetButtonViewInfoClass: TcxEditButtonViewInfoClass; override;
    procedure InternalPaint(ACanvas: TcxCanvas); override;
    function IsPanelStyle: Boolean;

    property CaptionPosition: TcxGroupBoxCaptionPosition read GetCaptionPosition;
    property CaptionRectIndent: TRect read GetCaptionRectIndent;

    property CheckBoxVisible: Boolean read FCheckBoxVisible write FCheckBoxVisible;
    property ControlRect: TRect read GetControlRect;
    property FocusRect: TRect read FFocusRect write FFocusRect;
  public
    Alignment: TLeftRight;
    CaptionRect: TRect;
    IsDesigning: Boolean;
    TextRect: TRect;
    constructor Create; override;
    destructor Destroy; override;
    procedure InvalidateCaption;

    property CheckBoxCheckState: TcxCheckBoxState read FCheckBoxCheckState write FCheckBoxCheckState;
    property Edit: TcxCustomGroupBox read GetEdit;
    property MouseFocusingRect: TRect read FMouseFocusingRect write FMouseFocusingRect;
  end;

  { TcxGroupBoxViewData }

  TcxGroupBoxViewData = class(TcxCustomEditViewData)
  private
    function GetCaptionPosition: TcxGroupBoxCaptionPosition;
    function GetEdit: TcxCustomGroupBox;
    function GetShadowWidth: Integer;
    function HasNonClientArea: Boolean;
    procedure CalculateRects(ACanvas: TcxCanvas; AEditViewInfo: TcxGroupBoxViewInfo);
  protected
    function GetContainerState(const ABounds: TRect; const P: TPoint;
      Button: TcxMouseButton; Shift: TShiftState;
      AIsMouseEvent: Boolean): TcxContainerState; override;
    function GetContentOffsetsByPainter(AViewInfo: TcxGroupBoxViewInfo): TRect;
    function IsPanelStyle: Boolean;
    property CaptionPosition: TcxGroupBoxCaptionPosition read GetCaptionPosition;
  public
    procedure Calculate(ACanvas: TcxCanvas; const ABounds: TRect; const P: TPoint;
      Button: TcxMouseButton; Shift: TShiftState; AViewInfo: TcxCustomEditViewInfo;
      AIsMouseEvent: Boolean); override;
    procedure CalculateButtonsViewInfo(ACanvas: TcxCanvas; const ABounds: TRect; const P: TPoint;
      Button: TcxMouseButton; Shift: TShiftState; AViewInfo: TcxCustomEditViewInfo;
      AIsMouseEvent: Boolean); override;
    procedure DoRightToLeftConversion(AViewInfo: TcxCustomEditViewInfo; const ABounds: TRect); override;
    function GetBorderColor: TColor; override;
    function GetBorderExtent: TRect; override;
    function GetClientExtent(ACanvas: TcxCanvas;
      AViewInfo: TcxCustomEditViewInfo): TRect; override;
    function HasShadow: Boolean; override;
    class function IsNativeStyle(ALookAndFeel: TcxLookAndFeel): Boolean; override;
    property Edit: TcxCustomGroupBox read GetEdit;
  end;

  { TcxButtonGroupViewData }

  TcxCustomButtonGroupProperties = class;

  TcxEditMetrics = record
    AutoHeightColumnWidthCorrection, AutoHeightWidthCorrection,
    ColumnWidthCorrection, WidthCorrection: Integer;
    ClientLeftBoundCorrection, ClientWidthCorrection, ColumnOffset: Integer;
    ButtonSize: TSize;
  end;

  { TcxButtonGroupViewInfo }

  TcxButtonGroupViewInfo = class(TcxGroupBoxViewInfo)
  protected
    procedure DrawEditButton(ACanvas: TcxCanvas; AButtonVisibleIndex: Integer); override;
    procedure DrawButtonCaption(ACanvas: TcxCanvas;
      AButtonViewInfo: TcxGroupBoxButtonViewInfo; const AGlyphRect: TRect); virtual; abstract;
    procedure DrawButtonGlyph(ACanvas: TcxCanvas;
      AButtonViewInfo: TcxGroupBoxButtonViewInfo; const AGlyphRect: TRect); virtual; abstract;
    function GetButtonViewInfoClass: TcxEditButtonViewInfoClass; override;
    function GetGlyphSize: TSize; virtual;
    function IsButtonGlyphTransparent(AButtonViewInfo: TcxGroupBoxButtonViewInfo): Boolean; virtual; abstract;
  public
    CaptionExtent: TRect;
    GlyphSize: TSize;
  end;

  { TcxButtonGroupButtonViewInfo }

  TcxButtonGroupButtonViewInfo = class(TcxGroupBoxButtonViewInfo)
  protected
    function CreateFadingHelper: TcxEditButtonFadingHelper; override;
  end;

  { TcxButtonGroupButtonFadingHelper }

  TcxButtonGroupButtonFadingHelper = class(TcxEditButtonFadingHelper)
  private
    function InternalGetEditViewInfo: TcxButtonGroupViewInfo;
    function GetViewInfo: TcxButtonGroupButtonViewInfo;
  protected
    procedure DrawButton(ACanvas: TcxCanvas); override;
    function GetButtonRect: TRect; override;
    function PrepareFadingImage(AState: TcxEditButtonState): TcxBitmap32; override;
  public
    property EditViewInfo: TcxButtonGroupViewInfo read InternalGetEditViewInfo;
    property ViewInfo: TcxButtonGroupButtonViewInfo read GetViewInfo;
  end;

  { TcxButtonGroupViewData }

  TcxButtonGroupViewData = class(TcxGroupBoxViewData)
  private
    function GetProperties: TcxCustomButtonGroupProperties;
  protected
    procedure CalculateButtonPositions(ACanvas: TcxCanvas;
      AViewInfo: TcxCustomEditViewInfo); virtual;
    procedure CalculateButtonViewInfos(AViewInfo: TcxCustomEditViewInfo); virtual;
    function GetDrawTextFlags: Integer; virtual;
    procedure GetEditMetrics(AAutoHeight: Boolean; ACanvas: TcxCanvas;
      out AMetrics: TcxEditMetrics); virtual; abstract;
    function GetCaptionRectExtent: TRect; virtual;
  public
    procedure Calculate(ACanvas: TcxCanvas; const ABounds: TRect; const P: TPoint;
      Button: TcxMouseButton; Shift: TShiftState; AViewInfo: TcxCustomEditViewInfo;
      AIsMouseEvent: Boolean); override;
    procedure CalculateButtonsViewInfo(ACanvas: TcxCanvas; const ABounds: TRect;
      const P: TPoint; Button: TcxMouseButton; Shift: TShiftState;
      AViewInfo: TcxCustomEditViewInfo; AIsMouseEvent: Boolean); override;
    function GetEditConstantPartSize(ACanvas: TcxCanvas;
      const AEditSizeProperties: TcxEditSizeProperties;
      var MinContentSize: TSize; AViewInfo: TcxCustomEditViewInfo = nil): TSize; override;
    class function IsButtonNativeStyle(ALookAndFeel: TcxLookAndFeel): Boolean; virtual;
    property Properties: TcxCustomButtonGroupProperties read GetProperties;
  end;

  TcxButtonGroupViewDataClass = class of TcxButtonGroupViewData;

  { TcxCustomGroupBoxProperties }

  TcxCustomGroupBoxProperties = class(TcxCustomEditProperties)
  protected
    class function GetViewDataClass: TcxCustomEditViewDataClass; override;
    function HasDisplayValue: Boolean; override;
  public
    function CanCompareEditValue: Boolean; override;
    class function GetContainerClass: TcxContainerClass; override;
    class function GetViewInfoClass: TcxContainerViewInfoClass; override;
  end;

  { TcxButtonGroupItem }

  TcxButtonGroupItem = class(TcxCaptionItem)
  private
    FEnabled: Boolean;
    function GetIsCollectionDestroying: Boolean;
    procedure SetEnabled(Value: Boolean);
  protected
    procedure DoChanged(ACollection: TCollection; ACollectionOperation: TcxCollectionOperation;
      AIndex: Integer = -1);
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property IsCollectionDestroying: Boolean read GetIsCollectionDestroying;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  end;

  { TcxButtonGroupItems }

  TcxButtonGroupItems = class(TcxCaptionItems)
  private
    FChangedItemIndex: Integer;
    FChangedItemOperation: TcxCollectionOperation;
    FItemChanged: Boolean;
    function GetItem(Index: Integer): TcxButtonGroupItem;
    function GetOwningProperties: TcxCustomEditProperties;
    procedure SetItem(Index: Integer; Value: TcxButtonGroupItem);
  protected
    procedure Update(Item: TCollectionItem); override;
    property ChangedItemIndex: Integer read FChangedItemIndex;
    property ChangedItemOperation: TcxCollectionOperation read FChangedItemOperation;
    property ItemChanged: Boolean read FItemChanged;
    property OwningProperties: TcxCustomEditProperties read GetOwningProperties;
  public
    procedure Assign(Source: TPersistent); override;
    procedure InternalNotify(AItem: TcxButtonGroupItem; AItemIndex: Integer; AItemOperation: TcxCollectionOperation);
    property Items[Index: Integer]: TcxButtonGroupItem read GetItem write SetItem; default;
  end;

  TcxButtonGroupItemsClass = class of TcxButtonGroupItems;

  { TcxCustomButtonGroupProperties }

  TcxCustomButtonGroupProperties = class(TcxCustomGroupBoxProperties)
  private
    FColumns: Integer;
    FItems: TcxButtonGroupItems;
    FShowEndEllipsis: Boolean;
    FWordWrap: Boolean;
    procedure SetColumns(Value: Integer);
    procedure SetItems(Value: TcxButtonGroupItems);
    procedure SetShowEndEllipsis(Value: Boolean);
    procedure SetWordWrap(Value: Boolean);
  protected
    procedure DoAssign(AProperties: TcxCustomEditProperties); override;
    class function GetViewDataClass: TcxCustomEditViewDataClass; override;
    function CreateItems: TcxButtonGroupItems; virtual;
    function GetButtonsPerColumn: Integer;
    function GetColumnCount: Integer; virtual;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    function CreatePreviewProperties: TcxCustomEditProperties; override;
    class function GetContainerClass: TcxContainerClass; override;
    function GetSpecialFeatures: TcxEditSpecialFeatures; override;
    function GetSupportedOperations: TcxEditSupportedOperations; override;
    class function GetViewInfoClass: TcxContainerViewInfoClass; override;
    // !!!
    property Columns: Integer read FColumns write SetColumns default 1;
    property Items: TcxButtonGroupItems read FItems write SetItems;
    property ShowEndEllipsis: Boolean read FShowEndEllipsis write SetShowEndEllipsis default False;
    property WordWrap: Boolean read FWordWrap write SetWordWrap default False;
  end;

  { TcxPanelStyle }

  TcxPanelStyle = class(TPersistent)
  private
    FActive: Boolean;
    FBorderWidth: TBorderWidth;
    FCaptionIndent: Integer;
    FEdit: TcxCustomGroupBox;
    FOfficeBackgroundKind: TcxPanelOffice11BackgroundKind;
    FWordWrap: Boolean;

    procedure ScaleFactorChangeHandler(Sender: TObject; M, D: Integer; IsLoading: Boolean);
    procedure SetActive(AValue: Boolean);
    procedure SetBorderWidth(AValue: TBorderWidth);
    procedure SetCaptionIndent(AValue: Integer);
    procedure SetOfficeBackgroundKind(AValue: TcxPanelOffice11BackgroundKind);
    procedure SetWordWrap(AValue: Boolean);
  protected
    procedure ChangeScale(M, D: Integer); virtual;
    procedure Update;
    property Edit: TcxCustomGroupBox read FEdit;
  public
    constructor Create(AOwner: TcxCustomGroupBox); virtual;
    destructor Destroy; override;
    procedure Assign(ASource: TPersistent); override;
  published
    property Active: Boolean read FActive write SetActive default False;
    property BorderWidth: TBorderWidth read FBorderWidth write SetBorderWidth default 0;
    property CaptionIndent: Integer read FCaptionIndent write SetCaptionIndent default 2;
    property OfficeBackgroundKind: TcxPanelOffice11BackgroundKind read FOfficeBackgroundKind
      write SetOfficeBackgroundKind default pobkOffice11Color;
    property WordWrap: Boolean read FWordWrap write SetWordWrap default False;
  end;

  { TcxCustomGroupBox }

  TcxGroupBoxCustomDrawEvent = procedure (Sender: TcxCustomGroupBox; var ADone: Boolean) of object;
  TcxGroupBoxMeasureCaptionHeightEvent = procedure (Sender: TcxCustomGroupBox;
    const APainter: TcxCustomLookAndFeelPainter; var ACaptionHeight: Integer) of object;
  TcxGroupBoxCustomDrawElementEvent = procedure (Sender: TcxCustomGroupBox;
    ACanvas: TCanvas; const ABounds: TRect; const APainter: TcxCustomLookAndFeelPainter; var ADone: Boolean) of object;

  TcxCustomGroupBox = class(TcxCustomEdit)
  private
    FAlignment: TcxCaptionAlignment;
    FCaptionBkColor: TColor; // deprecated
    FIsAccelCharHandling: Boolean;
    FPanelStyle: TcxPanelStyle;
    FRedrawOnResize: Boolean;
    FOnCustomDraw: TcxGroupBoxCustomDrawEvent;
    FOnCustomDrawCaption: TcxGroupBoxCustomDrawElementEvent;
    FOnCustomDrawContentBackground: TcxGroupBoxCustomDrawElementEvent;
    FOnMeasureCaptionHeight: TcxGroupBoxMeasureCaptionHeightEvent;

    function GetCheckBoxCheckState: TcxCheckBoxState;
    function GetCheckBoxViewState: TcxEditCheckState;
    procedure SetCheckBoxCheckState(AValue: TcxCheckBoxState);
    procedure SetCheckBoxViewState(AValue: TcxEditCheckState);

    function GetCaptionBkColor: TColor; // deprecated
    function GetColor: TColor; // deprecated
    function GetFont: TFont; // deprecated
    function GetViewInfo: TcxGroupBoxViewInfo;
    function IsSkinAvailable: Boolean;

    procedure UpdateCaption;
    procedure UpdateContentExtents;
    function GetHorizontalCaptionIndent: Integer;
    function GetPanelStyleCaptionDrawingFlags: Cardinal;
    function GetVerticalCaptionIndent: Integer;

    procedure SetAlignment(Value: TcxCaptionAlignment);
    procedure SetCaptionBkColor(Value: TColor); // deprecated
    procedure SetColor(Value: TColor); // deprecated
    procedure SetFont(Value: TFont); // deprecated
    procedure SetPanelStyle(AValue: TcxPanelStyle);

    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure CMUpdateNonClientArea(var Message: TMessage); message DXM_UPDATENONCLIENTAREA;
    procedure WMNCCalcSize(var Message: TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure WMPRINT(var Message: TMessage); message WM_PRINT;
  protected
    FCaptionFont: TFont;

    procedure AdjustClientRect(var Rect: TRect); override;
    procedure CalculateViewInfo(P: TPoint; Button: TcxMouseButton;
      Shift: TShiftState; AIsMouseEvent: Boolean); overload; override;
    procedure CreateHandle; override;
    function CreatePanelStyle: TcxPanelStyle; virtual;
    function DefaultParentColor: Boolean; override;
    function GetCheckBoxVisible: Boolean; virtual;
    function GetClientOffsets: TRect; override;
    function HasBackground: Boolean; override;
    procedure Initialize; override;
    function InternalGetActiveStyle: TcxContainerStyle; override;
    function InternalGetNotPublishedStyleValues: TcxEditStyleValues; override;
    function PointInCheckBox(APoint: TPoint): Boolean;
    procedure ScaleFactorChanged; override;
    procedure Toggle; virtual;

    procedure DoEditKeyDown(var Key: Word; Shift: TShiftState); override;
    procedure DoEditKeyUp(var Key: Word; Shift: TShiftState); override;

    procedure UpdateCheckBoxState(Shift: TShiftState; X, Y: Integer);

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseLeave(AControl: TControl); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    procedure PaintNonClientArea(ACanvas: TcxCanvas); override;

    // IcxMouseTrackingCaller3
    function PtInCaller(const P: TPoint): Boolean; override;

    // Conditions
    function CanAutoSize: Boolean; override;
    function CanFocusOnClick: Boolean; override;
    function CanHaveTransparentBorder: Boolean; override;
    function InternalCanFocusOnClick: Boolean; virtual;
    function IsContainerClass: Boolean; override;
    function IsNativeBackground: Boolean; override;
    function IsPanelStyle: Boolean;
    function NeedRedrawOnResize: Boolean; override;
    // Fading
    function FadingCanFadeBackground: Boolean; override;

    // Notifications
    procedure ContainerStyleChanged(Sender: TObject); override;
    procedure LookAndFeelChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues); override;
    procedure TextChanged; override;

    // Shadow
    function HasShadow: Boolean; override;
    function GetShadowBounds: TRect; override;
    function GetShadowBoundsExtent: TRect; override;

    function DoCustomDrawCaption(ACanvas: TcxCanvas;
      const ABounds: TRect; APainter: TcxCustomLookAndFeelPainter): Boolean; virtual;
    function DoCustomDrawContentBackground(ACanvas: TcxCanvas;
      const ABounds: TRect; APainter: TcxCustomLookAndFeelPainter): Boolean; virtual;
    procedure DoMeasureCaptionHeight(APainter: TcxCustomLookAndFeelPainter; var ACaptionHeight: Integer);

    procedure AdjustCanvasFontSettings(ACanvas: TcxCanvas);
    function DoCustomDraw: Boolean;
    function GetCaptionDrawingFlags: Cardinal;
    function HasNonClientArea: Boolean; override;
    function IsNonClientAreaSupported: Boolean; virtual;
    function IsVerticalText: Boolean;
    procedure CalculateCaptionFont;
    procedure RedrawNonClientArea;
    procedure Resize; override;

    property CheckBoxCheckState: TcxCheckBoxState read GetCheckBoxCheckState write SetCheckBoxCheckState;
    property CheckBoxViewState: TcxEditCheckState read GetCheckBoxViewState write SetCheckBoxViewState;
    property Ctl3D;
    property PanelStyle: TcxPanelStyle read FPanelStyle write SetPanelStyle;
    property ParentBackground;
    property RedrawOnResize: Boolean read FRedrawOnResize write FRedrawOnResize default True;
    property TabStop default False;
    property ViewInfo: TcxGroupBoxViewInfo read GetViewInfo;
    property OnCustomDraw: TcxGroupBoxCustomDrawEvent read FOnCustomDraw write FOnCustomDraw;
    property OnCustomDrawCaption: TcxGroupBoxCustomDrawElementEvent read FOnCustomDrawCaption write FOnCustomDrawCaption;
    property OnCustomDrawContentBackground: TcxGroupBoxCustomDrawElementEvent read FOnCustomDrawContentBackground write FOnCustomDrawContentBackground;
    property OnMeasureCaptionHeight: TcxGroupBoxMeasureCaptionHeightEvent read FOnMeasureCaptionHeight write FOnMeasureCaptionHeight;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    function GetBiDiModeDependentAlignment: TcxCaptionAlignment;

    property Alignment: TcxCaptionAlignment read FAlignment write SetAlignment
      default alTopLeft;
    property Transparent;
  published
    property CaptionBkColor: TColor read GetCaptionBkColor write SetCaptionBkColor stored False; // deprecated hidden
    property Color: TColor read GetColor write SetColor stored False; // deprecated hidden
    property Font: TFont read GetFont write SetFont stored False; // deprecated hidden
    property LookAndFeel stored False; // deprecated hidden
    property StyleFocused stored False; // deprecated hidden
    property StyleHot stored False; // deprecated hidden
  end;

  { TcxGroupBox }

  TcxGroupBox = class(TcxCustomGroupBox)
  published
    property Alignment;
    property Anchors;
    property BiDiMode;
    property Caption;
    property Constraints;
    property Ctl3D;
    property UseDockManager;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property PanelStyle;
    property ParentBackground;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property RedrawOnResize;
    property ShowHint;
    property Style;
    property StyleDisabled;
    property TabOrder;
    property TabStop;
    property Transparent;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnCustomDraw;
    property OnCustomDrawCaption;
    property OnCustomDrawContentBackground;
    property OnDblClick;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnMeasureCaptionHeight;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

  { TcxCustomButtonGroup }

  TcxCustomButtonGroup = class(TcxCustomGroupBox)
  private
    FButtons: TList;
    FIsButtonsUpdating: Boolean;

    procedure DoButtonDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure DoButtonDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure DoButtonKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure DoButtonKeyPress(Sender: TObject; var Key: Char);
    procedure DoButtonKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure DoButtonMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure DoButtonMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure DoButtonMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure DoButtonMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    function GetProperties: TcxCustomButtonGroupProperties;
    function GetActiveProperties: TcxCustomButtonGroupProperties;
    procedure SetProperties(Value: TcxCustomButtonGroupProperties);
  protected
    procedure CreateHandle; override;

    function CanAutoSize: Boolean; override;
    procedure ContainerStyleChanged(Sender: TObject); override;
    procedure CursorChanged; override;
    procedure DoEditKeyDown(var Key: Word; Shift: TShiftState); override;
    procedure EnabledChanged; override;
    procedure Initialize; override;
    function IsButtonDC(ADC: THandle): Boolean; override;
    function IsContainerClass: Boolean; override;
    function IsNonClientAreaSupported: Boolean; override;
    procedure PropertiesChanged(Sender: TObject); override;
    function DoRefreshContainer(const P: TPoint; Button: TcxMouseButton; Shift: TShiftState;
      AIsMouseEvent: Boolean): Boolean; override;

    procedure ArrangeButtons; virtual;
    function GetButtonDC(AButtonIndex: Integer): THandle; virtual; abstract;
    function GetButtonIndexAt(const P: TPoint): Integer;
    function GetButtonInstance: TWinControl; virtual; abstract;
    function GetFocusedButtonIndex: Integer;
    procedure InitButtonInstance(AButton: TWinControl); virtual;
    procedure InternalUpdateButtons; virtual;
    procedure SetButtonCount(Value: Integer); virtual;
    procedure SynchronizeButtonsStyle; virtual;
    procedure UpdateButtons;

    property InternalButtons: TList read FButtons;
    property TabStop default True;
  public
    destructor Destroy; override;
    procedure ActivateByMouse(Shift: TShiftState; X, Y: Integer; var AEditData: TcxCustomEditData); override;
    function Focused: Boolean; override;
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    procedure GetTabOrderList(List: TList); override;
    function IsButtonNativeStyle: Boolean;
    property AutoSize default False;
    property ActiveProperties: TcxCustomButtonGroupProperties read GetActiveProperties;
    property Properties: TcxCustomButtonGroupProperties read GetProperties write SetProperties;
  end;

implementation

uses
  Variants, Math, Types, TypInfo,
  dxThemeConsts, cxEditUtils, dxOffice11, dxThemeManager,
  dxUxTheme, cxDrawTextUtils, cxGeometry;

const
  cxNativeState: array[Boolean] of Integer = (GBS_DISABLED, GBS_NORMAL);

type
  TControlAccess = class(TControl);
  TWinControlAccess = class(TWinControl);
  TcxContainerStyleAccess = class(TcxContainerStyle);

function cxGroupBoxAlignmentToGroupBoxCaptionPosition(AAlignment: TcxCaptionAlignment): TcxGroupBoxCaptionPosition;
begin
  case AAlignment of
    alTopLeft, alTopCenter, alTopRight: Result := cxgpTop;
    alBottomLeft, alBottomCenter, alBottomRight: Result := cxgpBottom;
    alLeftTop, alLeftCenter, alLeftBottom: Result := cxgpLeft;
    alRightTop, alRightCenter, alRightBottom: Result := cxgpRight;
  else
    Result := cxgpCenter;
  end;
end;

{ TcxGroupBoxButtonViewInfo }

function TcxGroupBoxButtonViewInfo.GetGlyphRect(
  const AGlyphSize: TSize; AAlignment: TLeftRight; AIsPaintCopy: Boolean): TRect;
begin
  Result.Top := Bounds.Top + (Bounds.Bottom - Bounds.Top - AGlyphSize.cy) div 2;
  Result.Bottom := Result.Top + AGlyphSize.cy;
  if AAlignment = taRightJustify then
  begin
    Result.Left := Bounds.Left;
    Result.Right := Result.Left + AGlyphSize.cx;
  end
  else
  begin
    Result.Right := Bounds.Right;
    Result.Left := Result.Right - AGlyphSize.cx;
  end;
end;

{ TcxGroupBoxViewInfo }

constructor TcxGroupBoxViewInfo.Create;
begin
  inherited Create;
end;

destructor TcxGroupBoxViewInfo.Destroy;
begin
  inherited Destroy;
end;

procedure TcxGroupBoxViewInfo.InvalidateCaption;
begin
  Edit.InvalidateRect(CaptionRect, False);
end;

procedure TcxGroupBoxViewInfo.CalculateFocusRect;
var
  ARect: TRect;
begin
  if CheckBoxVisible and Edit.Focused then
  begin
    ARect := TextRect;
    Dec(ARect.Left, 2);
    Inc(ARect.Right);
    Inc(ARect.Bottom);
    FocusRect := ARect;
  end
  else
    FocusRect := cxEmptyRect;
end;

procedure TcxGroupBoxViewInfo.CalculateRectsForPanel(ACanvas: TcxCanvas);

  function CalcOffsetBounds: TRect;
  var
    ABorderSize: TRect;
    ACaptionIndentRect: TRect;
    ABorderWidth: Integer;
  begin
    ABorderWidth := GetContainerBorderWidth(TcxContainerBorderStyle(BorderStyle));
    ABorderSize := Rect(ABorderWidth, ABorderWidth, ABorderWidth, ABorderWidth);
    ACaptionIndentRect := cxEmptyRect;
    case Edit.Alignment of
      alTopLeft, alLeftTop, alLeftCenter, alLeftBottom, alBottomLeft:
        ACaptionIndentRect.Left := Edit.GetHorizontalCaptionIndent;
      alTopRight, alRightTop, alRightCenter, alRightBottom, alBottomRight:
        ACaptionIndentRect.Right := Edit.GetHorizontalCaptionIndent;
    end;
    case Edit.Alignment of
      alLeftTop, alTopLeft, alTopCenter, alTopRight, alRightTop:
        ACaptionIndentRect.Top := Edit.GetVerticalCaptionIndent;
      alLeftBottom, alBottomLeft, alBottomCenter, alBottomRight, alRightBottom:
        ACaptionIndentRect.Bottom := Edit.GetVerticalCaptionIndent;
    end;
    Result.Left := ABorderSize.Left + ACaptionIndentRect.Left;
    Result.Top := ABorderSize.Top + ACaptionIndentRect.Top;
    Result.Right := ABorderSize.Right + ACaptionIndentRect.Right;
    Result.Bottom := ABorderSize.Bottom + ACaptionIndentRect.Bottom;
  end;

  function GetBounds: TRect;
  begin
    Result := cxRectContent(GetBoundsForPanel, CalcOffsetBounds);
  end;

  function GetTextRect: TRect;
  begin
    Result := GetCaptionRect(ACanvas, GetBounds);
  end;

  procedure AdjustTextRect;
  var
    ATextWidth, ATextHeight: Integer;
    R: TRect;
  begin
    ATextWidth := cxRectWidth(TextRect);
    ATextHeight := cxRectHeight(TextRect);

    R := GetBounds;
    OffsetRect(TextRect, R.Left - TextRect.Left, R.Top - TextRect.Top);
    case Edit.Alignment of
      alTopCenter, alBottomCenter, alCenterCenter:
        OffsetRect(TextRect, (R.Right - R.Left - ATextWidth - TextRect.Left) div 2, 0);
      alTopRight, alRightTop, alRightCenter, alRightBottom, alBottomRight:
        OffsetRect(TextRect, R.Right - ATextWidth - TextRect.Left, 0);
    end;
    case Edit.Alignment of
      alLeftCenter, alRightCenter, alCenterCenter:
        OffsetRect(TextRect, 0, (R.Bottom - R.Top - ATextHeight - TextRect.Top) div 2);
      alLeftBottom, alBottomLeft, alBottomCenter, alBottomRight, alRightBottom:
        OffsetRect(TextRect, 0, R.Bottom - ATextHeight - TextRect.Top);
    end;
  end;

  procedure AdjustCaptionRect;
  var
    R: TRect;
  begin
    R := GetBounds;
    CaptionRect.Right := Min(CaptionRect.Right, R.Right);
    CaptionRect.Bottom := Min(CaptionRect.Bottom, R.Bottom);
    CaptionRect.Left := Max(CaptionRect.Left, R.Left);
    CaptionRect.Top := Max(CaptionRect.Top, R.Top);
  end;

begin
  TextRect := GetTextRect;
  AdjustTextRect;
  CaptionRect := TextRect;
  AdjustCaptionRect;
end;

procedure TcxGroupBoxViewInfo.CalculateRectsForStandardPainter(ACanvas: TcxCanvas; AShadowWidth: Integer);

  procedure AdjustHorizontalCaptionRect(var R: TRect);
  begin
    case Edit.Alignment of
      alTopCenter, alBottomCenter, alCenterCenter:
        OffsetRect(R, -R.Left + (Edit.Width - AShadowWidth - (R.Right - R.Left)) div 2, 0);
      alTopRight, alBottomRight:
        OffsetRect(R, Edit.Width - R.Right - R.Left - AShadowWidth - GetCaptionRectLeftBound, 0);
      alTopLeft, alBottomLeft:
        OffsetRect(R, GetCaptionRectLeftBound, 0);
    end;
    case Edit.Alignment of
      alCenterCenter:
        OffsetRect(R, 0, -R.Top + (Edit.Height - AShadowWidth -
          (R.Bottom - R.Top)) div 2);
      alBottomLeft, alBottomCenter, alBottomRight:
        R := Rect(R.Left, Edit.Height - R.Top -
          (R.Bottom - R.Top), R.Right, Edit.Height - R.Top);
    end
  end;

  procedure AdjustVerticalCaptionRect(var R: TRect);
  var
    ATextWidth: Integer;
  begin
    ATextWidth := cxRectWidth(R);

    case Edit.Alignment of
      alLeftTop:
        begin
          R := Rect(R.Top, R.Left, R.Bottom, 0);
          R.Bottom := R.Top + ATextWidth + 1;
        end;
      alLeftCenter:
        begin
          R := Rect(R.Top, 0, R.Bottom,
          Edit.Height - AShadowWidth - (Edit.Height - AShadowWidth - ATextWidth) div 2);
          R.Top := R.Bottom - ATextWidth - 1;
        end;
      alLeftBottom:
        begin
          R := Rect(R.Top, 0, R.Bottom,
          Edit.Height - AShadowWidth - R.Left);
          R.Top := R.Bottom - ATextWidth - 1;
        end;
      alRightTop:
        R := Rect(Edit.Width - R.Bottom, R.Left,
          Edit.Width - R.Top, R.Left + ATextWidth);
      alRightCenter:
        begin
          R := Rect(Edit.Width - R.Bottom,
            (Edit.Height - ATextWidth) div 2, Edit.Width - R.Top, 0);
          R.Bottom := R.Top + ATextWidth;
        end;
      alRightBottom:
        R := Rect(Edit.Width - R.Bottom,
          Edit.Height - R.Left - ATextWidth, Edit.Width - R.Top,
          Edit.Height - R.Left);
    end;
    case Edit.Alignment of
      alLeftTop, alRightTop: OffsetRect(R, 0, GetCaptionRectLeftBound);
      alLeftBottom, alRightBottom: OffsetRect(R, 0, - GetCaptionRectLeftBound);
    end;
  end;

  procedure AdjustCombinedCaptionRect(var ACombinedRect: TRect);
  begin
    if Edit.IsVerticalText then
      AdjustVerticalCaptionRect(ACombinedRect)
    else
      AdjustHorizontalCaptionRect(ACombinedRect);
  end;

  function GetTextRect: TRect;
  begin
    if Edit.Caption = '' then
      Result := cxEmptyRect
    else
      Result := GetCaptionRect(ACanvas, cxEmptyRect);
  end;

var
  ACombinedRect: TRect;
begin
  FCheckBoxRect := GetCheckBoxRect(ACanvas);
  TextRect := GetTextRect;
  ACombinedRect := GetCombinedCaptionRect;
  AdjustCombinedCaptionRect(ACombinedRect);
  CaptionRect := ACombinedRect;
  CalculateCheckBoxAndTextRects(ACombinedRect);
end;

procedure TcxGroupBoxViewInfo.CalculateRectsForSkinPainter(ACanvas: TcxCanvas; AHasNonClientArea: Boolean);

  procedure CalculateBorderRect(var R: TRect; const AIndent: TRect);
  begin
    if AHasNonClientArea then
      case CaptionPosition of
        cxgpTop:
          Dec(R.Bottom, AIndent.Top);
        cxgpLeft:
          Dec(R.Right, AIndent.Left);
      end;
  end;

  procedure AdjustCaptionRect(var ACaptionRect: TRect; const AIndent, ACombinedRect: TRect; out ANeedCorrection: Boolean);
  var
    AHeight, AStoredHeight: Integer;
  begin
    AHeight := cxRectHeight(ACombinedRect);
    AStoredHeight := AHeight;
    Edit.DoMeasureCaptionHeight(Painter, AHeight);
    ANeedCorrection := AStoredHeight <> AHeight;
    case CaptionPosition of
      cxgpBottom:
        ACaptionRect.Top := ACaptionRect.Bottom - AHeight - AIndent.Bottom;
      cxgpRight:
        ACaptionRect.Left := ACaptionRect.Right - AHeight - AIndent.Right;
      cxgpCenter:
        ACaptionRect := cxRectCenter(ACaptionRect, cxSize(cxRectWidth(ACombinedRect), AHeight));
      cxgpLeft:
        begin
          ACaptionRect.Right := ACaptionRect.Left + AHeight;
          if AHasNonClientArea then
            Dec(ACaptionRect.Left, AIndent.Left)
          else
            Inc(ACaptionRect.Right, AIndent.Left);
        end;
      cxgpTop:
        begin
          ACaptionRect.Bottom := ACaptionRect.Top + AHeight;
          if AHasNonClientArea then
            Dec(ACaptionRect.Top, AIndent.Top)
          else
            Inc(ACaptionRect.Bottom, AIndent.Top);
        end;
    end;
  end;

  procedure AdjustCombinedCaptionRect(var ACombinedRect: TRect; const ACaptionRect, ABorderSize: TRect);
  var
    ACombinedRectWidth: Integer;
    ATempCombinedRect: TRect;
  begin
    ACombinedRectWidth := cxRectWidth(ACombinedRect);
    ATempCombinedRect := cxRectContent(ACaptionRect, ABorderSize);
    ACombinedRect := ATempCombinedRect;
    case Edit.Alignment of
      alTopLeft, alBottomLeft: ACombinedRect.Right := ACombinedRect.Left + ACombinedRectWidth;
      alTopRight, alBottomRight: ACombinedRect.Left := ACombinedRect.Right - ACombinedRectWidth;
      alTopCenter, alBottomCenter:
        begin
          ACombinedRect.Left := (ACombinedRect.Left + ACombinedRect.Right - ACombinedRectWidth) div 2;
          ACombinedRect.Right := ACombinedRect.Left + ACombinedRectWidth;
        end;
      alLeftTop, alRightTop: ACombinedRect.Bottom := ACombinedRect.Top + ACombinedRectWidth;
      alLeftBottom, alRightBottom: ACombinedRect.Top := ACombinedRect.Bottom - ACombinedRectWidth;
      alLeftCenter, alRightCenter:
        begin
          ACombinedRect.Top := (ACombinedRect.Bottom + ACombinedRect.Top - ACombinedRectWidth) div 2;
          ACombinedRect.Bottom := ACombinedRect.Top + ACombinedRectWidth;
        end;
    end;
    ACombinedRect.Bottom := Min(ACombinedRect.Bottom, ATempCombinedRect.Bottom);
    ACombinedRect.Left := Max(ACombinedRect.Left, ATempCombinedRect.Left);
    ACombinedRect.Right := Min(ACombinedRect.Right, ATempCombinedRect.Right);
    ACombinedRect.Top := Max(ACombinedRect.Top, ATempCombinedRect.Top);
  end;

  procedure CheckBoxRectCenteringRelativeToCaptionRect;
  begin
    case Edit.Alignment of
      alTopLeft, alTopRight, alTopCenter, alBottomLeft, alBottomRight, alBottomCenter, alCenterCenter:
        OffsetRect(FCheckBoxRect, 0, CaptionRect.Top + (cxRectHeight(CaptionRect) - cxRectHeight(FCheckBoxRect)) div 2 - FCheckBoxRect.Top);
    else
      OffsetRect(FCheckBoxRect, CaptionRect.Left + (cxRectWidth(CaptionRect) - cxRectWidth(FCheckBoxRect)) div 2 - FCheckBoxRect.Left, 0);
    end;
  end;

var
  ACombinedRect, ACaptionRectIndent: TRect;
  ANeedCorrection: Boolean;
begin
  BorderRect := GetControlRect;
  CaptionRect := BorderRect;
  TextRect := GetCaptionRect(ACanvas, cxEmptyRect);
  FCheckBoxRect := GetCheckBoxRect(ACanvas);
  ACombinedRect := GetCombinedCaptionRect;
  ACaptionRectIndent := CaptionRectIndent;
  AdjustCaptionRect(CaptionRect, ACaptionRectIndent, ACombinedRect, ANeedCorrection);
  AdjustCombinedCaptionRect(ACombinedRect, CaptionRect, Painter.GroupBoxBorderSize(True, CaptionPosition));
  CalculateCheckBoxAndTextRects(ACombinedRect);
  CheckBoxRectCenteringRelativeToCaptionRect;
  CalculateBorderRect(BorderRect, ACaptionRectIndent);
  if ANeedCorrection then
    AdjustCaptionRect(CaptionRect, cxNullRect, ACombinedRect, ANeedCorrection);
end;

procedure TcxGroupBoxViewInfo.DrawCaption(ACanvas: TcxCanvas);

  function GetCaptionContentRect: TRect;
  begin
    if CheckBoxVisible then
      Result := cxRectUnion(TextRect, FCheckBoxRect)
    else
      Result := TextRect;
  end;

  procedure AdjustRectForBordersNone(var R: TRect; ACaptionPosition: TcxGroupBoxCaptionPosition);
  begin
    case ACaptionPosition of
      cxgpTop:
        ACaptionPosition := cxgpBottom;
      cxgpBottom:
        ACaptionPosition := cxgpTop;
      cxgpLeft:
        ACaptionPosition := cxgpRight;
      cxgpRight:
        ACaptionPosition := cxgpLeft;
    end;
    R := cxRectInflate(R, Painter.GroupBoxBorderSize(False, ACaptionPosition));
  end;

  procedure DrawSkinnedCaptionBackground;
  var
    ACaptionPosition: TcxGroupBoxCaptionPosition;
    ACaptionRect: TRect;
  begin
    ACaptionRect := CaptionRect;
    ACaptionPosition := GetBiDiModeCaptionPosition;
    if BorderStyle = ebsNone then
    begin
      if Painter.IsGroupBoxCaptionTextDrawnOverBorder(ACaptionPosition) then
        Exit;
      AdjustRectForBordersNone(ACaptionRect, ACaptionPosition);
    end;
    Painter.DrawGroupBoxCaption(ACanvas, ACaptionRect, GetCaptionContentRect, ACaptionPosition);
  end;

begin
  ACanvas.SaveClipRegion;
  try
    ACanvas.IntersectClipRect(CaptionRect);
    if (Edit.Caption = '') or Edit.DoCustomDrawCaption(ACanvas, CaptionRect, Painter) then
      Exit;

    Edit.AdjustCanvasFontSettings(ACanvas);
    if UseSkins and not IsPanelStyle then
      DrawSkinnedCaptionBackground;

    ACanvas.Brush.Style := bsClear;
    if Edit.IsVerticalText then
      DrawVerticalTextCaption(ACanvas)
    else
      DrawHorizontalTextCaption(ACanvas);
  finally
    ACanvas.RestoreClipRegion;
  end;
end;

function TcxGroupBoxViewInfo.GetBiDiModeCaptionPosition: TcxGroupBoxCaptionPosition;
begin
  Result := cxGroupBoxAlignmentToGroupBoxCaptionPosition(Edit.GetBiDiModeDependentAlignment);
end;

function TcxGroupBoxViewInfo.GetButtonViewInfoClass: TcxEditButtonViewInfoClass;
begin
  Result := TcxGroupBoxButtonViewInfo;
end;

procedure TcxGroupBoxViewInfo.InternalPaint(ACanvas: TcxCanvas);
var
  AFrameBounds: TRect;
begin
  if IsInplace then
  begin
    if Edit = nil then
      inherited InternalPaint(ACanvas)
    else
      if IsCustomBackground then
        DrawBackground(ACanvas)
      else
        cxEditFillRect(ACanvas, Bounds, BackgroundColor);
    Exit;
  end;

  InternalDrawBackground(ACanvas);
  DrawCaption(ACanvas);
  DrawCheckBox(ACanvas);

  if not IsPanelStyle then
    ACanvas.ExcludeClipRect(CaptionRect);

  AFrameBounds := GetFrameBounds;
  if Edit.UseRightToLeftAlignment then
    AFrameBounds := TdxRightToLeftLayoutConverter.ConvertRect(AFrameBounds, Bounds);
  DrawFrame(ACanvas, AFrameBounds);

  if Edit.IsDBEditPaintCopyDrawing then
    DrawButtons(ACanvas);
end;

function TcxGroupBoxViewInfo.IsPanelStyle: Boolean;
begin
  Result := (Edit <> nil) and Edit.IsPanelStyle;
end;

procedure TcxGroupBoxViewInfo.CalculateCheckBoxAndTextRects(ACombinedRect: TRect);
begin
  MouseFocusingRect := ACombinedRect;
  TextRect := ACombinedRect;
  if CheckBoxVisible then
  begin
    if Edit.Alignment in [alTopLeft, alTopCenter, alTopRight, alCenterCenter, alBottomLeft, alBottomCenter, alBottomRight] then
    begin
      FCheckBoxRect := cxRectSetLeft(FCheckBoxRect, ACombinedRect.Left);
      FCheckBoxRect := cxRectSetTop(FCheckBoxRect, ACombinedRect.Top +
        (cxRectHeight(ACombinedRect) - cxRectHeight(FCheckBoxRect)) div 2) ;
      TextRect.Left := FCheckBoxRect.Right + GetCheckBoxCaptionIndent;
      TextRect := cxRectSetTop(TextRect, ACombinedRect.Top);
    end
    else
    begin
      FCheckBoxRect := cxRectSetLeft(FCheckBoxRect, ACombinedRect.Left +
        (cxRectWidth(ACombinedRect) - cxRectWidth(FCheckBoxRect)) div 2);
      TextRect := cxRectSetLeft(TextRect, ACombinedRect.Left);
      if Edit.Alignment in [alRightTop, alRightCenter, alRightBottom] then
      begin
        FCheckBoxRect := cxRectSetTop(FCheckBoxRect, ACombinedRect.Top);
        TextRect := cxRectSetTop(TextRect, FCheckBoxRect.Bottom + GetCheckBoxCaptionIndent);
      end
      else
      begin
        FCheckBoxRect := cxRectSetBottom(FCheckBoxRect, ACombinedRect.Bottom);
        TextRect := cxRectSetBottom(TextRect, FCheckBoxRect.Top - GetCheckBoxCaptionIndent);
      end;
    end;
  end;
end;

procedure TcxGroupBoxViewInfo.InvalidateCheckBoxRect;
begin
  Edit.InvalidateRect(FCheckBoxRect, False);
end;

function TcxGroupBoxViewInfo.PointInCheckBox(X, Y: Integer): Boolean;
begin
  Result := PtInRect(FMouseFocusingRect, Point(X, Y));
end;

function TcxGroupBoxViewInfo.GetCaptionRect(ACanvas: TcxCanvas; const ARect: TRect): TRect;
var
  AFlags: Cardinal;
begin
  AFlags := Edit.GetCaptionDrawingFlags;
  Result := ARect;
  cxDrawText(ACanvas.Handle, Edit.Caption, Result, AFlags or DT_CALCRECT);
end;

function TcxGroupBoxViewInfo.GetCaptionRectIndent: TRect;
var
  ACaptionPosition: TcxGroupBoxCaptionPosition;
  R1: TRect;
begin
  Result := cxNullRect;
  if Assigned(Edit) and Assigned(Edit.Style.LookAndFeel.SkinPainter) and not IsPanelStyle then
  begin
    ACaptionPosition := CaptionPosition;
    R1 := Edit.Style.LookAndFeel.SkinPainter.GroupBoxBorderSize(True, ACaptionPosition);
    case ACaptionPosition of
      cxgpTop:
        Result.Top := R1.Top + R1.Bottom;
      cxgpBottom:
        Result.Bottom := R1.Top + R1.Bottom;
      cxgpLeft:
        Result.Left := R1.Right + R1.Left;
      cxgpRight:
        Result.Right := R1.Right + R1.Left;
    end;
  end;
end;

function TcxGroupBoxViewInfo.GetCaptionPosition: TcxGroupBoxCaptionPosition;
begin
  Result := cxGroupBoxAlignmentToGroupBoxCaptionPosition(Edit.Alignment);
end;

function TcxGroupBoxViewInfo.GetCaptionRectLeftBound: Integer;
const
  cxCaptionRectLeftBound = 8;
begin
  Result := ScaleFactor.Apply(cxCaptionRectLeftBound);
end;

function TcxGroupBoxViewInfo.GetCheckBoxCaptionIndent: Integer;
const
  cxCheckBoxCaptionIndent = 4;
begin
  Result := ScaleFactor.Apply(cxCheckBoxCaptionIndent);
end;

function TcxGroupBoxViewInfo.GetCheckBoxRect(ACanvas: TcxCanvas): TRect;
var
  ACheckBoxSize: TSize;
begin
  Result := cxEmptyRect;
  if CheckBoxVisible then
  begin
    ACheckBoxSize := GetScaledEditCheckSize(ACanvas, NativeStyle, nil, 0, Painter, ScaleFactor);
    Result.Right := ACheckBoxSize.cx;
    Result.Bottom := ACheckBoxSize.cy;
  end;
end;

function TcxGroupBoxViewInfo.GetControlRect: TRect;
begin
  Result := cxContainer.GetControlRect(Edit);
end;

function TcxGroupBoxViewInfo.GetEdit: TcxCustomGroupBox;
begin
  Result := TcxCustomGroupBox(FEdit);
end;

function TcxGroupBoxViewInfo.GetFrameBounds: TRect;
begin
  if IsPanelStyle then
    Result := GetBoundsForPanel
  else
  begin
    Result := BorderRect;
    ExtendRectByBorders(Result,
      GetContainerBorderWidth(TcxContainerBorderStyle(BorderStyle)), Edges);
  end;
end;

procedure TcxGroupBoxViewInfo.DrawCheckBox(ACanvas: TcxCanvas);

  function GetPainter: TcxCustomLookAndFeelPainter;
  begin
    if UseSkins then
      Result := Painter
    else
      Result := nil;
  end;

begin
  if CheckBoxVisible then
    DrawScaledEditCheck(ACanvas, FCheckBoxRect, FCheckBoxCheckState, FCheckBoxViewState, nil, 0,
      Edit.Style.BorderStyle, NativeStyle, BorderColor, BackgroundColor, DrawBackground(ACanvas),
      IsDesigning, Focused, True, GetPainter, ScaleFactor);
  if not IsRectEmpty(FocusRect) then
    ACanvas.DrawFocusRect(FocusRect);
end;

procedure TcxGroupBoxViewInfo.DrawHorizontalTextCaption(ACanvas: TcxCanvas);
begin
  cxDrawText(ACanvas.Handle, Edit.Caption, TextRect, Edit.GetCaptionDrawingFlags);
end;

procedure TcxGroupBoxViewInfo.DrawVerticalTextCaption(ACanvas: TcxCanvas);
const
  ARotationAngleMap: array [Boolean] of TcxRotationAngle = (raMinus90, raPlus90);
var
  AFlags: Cardinal;
begin
  AFlags := 0;
  if Edit.UseRightToLeftReading then
    AFlags := AFlags or DT_RTLREADING;
  cxDrawText(ACanvas, Edit.Caption, TextRect, AFlags, clDefault,
    ARotationAngleMap[CaptionPosition = cxgpLeft]);
end;

procedure TcxGroupBoxViewInfo.DrawFrame(ACanvas: TcxCanvas; R: TRect);
begin
  if NativeStyle then
  begin
    if BorderStyle <> ebsNone then
    begin
      if IsPanelStyle then
        Edit.LookAndFeelPainter.DrawBorder(ACanvas, GetBoundsForPanel)
      else
      begin
        R := GetThemeBackgroundRect(ACanvas);
        if Edit.UseRightToLeftAlignment then
          R := TdxRightToLeftLayoutConverter.ConvertRect(R, Bounds);
        cxLookAndFeelPaintersManager.GetPainter(lfsNative).DrawGroupBoxFrame(ACanvas, R, Enabled, cxgpCenter);
      end;
    end;
  end
  else
  begin
    if not UseSkins then
    begin
      case BorderStyle of
        ebsSingle:
          ACanvas.FrameRect(R, BorderColor, 1, Edit.ActiveStyle.Edges, True);
        ebsThick:
          ACanvas.FrameRect(R, BorderColor, 2, Edit.ActiveStyle.Edges, True);
        ebsFlat:
          cxLookAndFeelPaintersManager.GetPainter(lfsFlat).DrawGroupBoxFrame(
            ACanvas, R, Enabled, cxgpCenter, Edit.ActiveStyle.Edges);
        ebs3D:
          if Edit.Ctl3D then
            cxLookAndFeelPaintersManager.GetPainter(lfsStandard).DrawGroupBoxFrame(
              ACanvas, R, Enabled, cxgpCenter, Edit.ActiveStyle.Edges)
          else
          begin
            ACanvas.FrameRect(R, clWindowFrame, 1, Edit.ActiveStyle.Edges, True);
            InflateRect(R, -1, -1);
            ACanvas.FrameRect(R, BackgroundColor, 1, Edit.ActiveStyle.Edges, True);
          end;
      end;
    end;
  end;
end;

function TcxGroupBoxViewInfo.GetCombinedCaptionRect: TRect;
begin
  Result := TextRect;
  Result.Bottom := Max(Result.Bottom, FCheckBoxRect.Bottom);
  if CheckBoxVisible then
  begin
    Result.Right := Result.Right + cxRectWidth(FCheckBoxRect);
    if not IsRectEmpty(TextRect) then
      Result.Right := Result.Right + GetCheckBoxCaptionIndent;
  end;
end;

function TcxGroupBoxViewInfo.GetThemeBackgroundRect(
  ACanvas: TcxCanvas): TRect;
begin
  Result := ControlRect;
  if not IsPanelStyle then
    case CaptionPosition of
      cxgpTop:
        Result.Top := cxTextHeight(ACanvas.Handle) div 2;
      cxgpBottom:
        Dec(Result.Bottom, cxTextHeight(ACanvas.Handle) div 2);
      cxgpLeft:
        Result.Left := cxTextHeight(ACanvas.Handle) div 2;
      cxgpRight:
        Dec(Result.Right, cxTextHeight(ACanvas.Handle) div 2);
    end;
end;

function TcxGroupBoxViewInfo.GetBoundsForPanel: TRect;
begin
  Result := Bounds;
  if not NativeStyle and not UseSkins then
    if Edit.HasShadow then
    begin
      Dec(Result.Right, cxContainerShadowWidth);
      Dec(Result.Bottom, cxContainerShadowWidth);
    end;
end;

procedure TcxGroupBoxViewInfo.DrawParentBackground(ACanvas: TcxCanvas; R: TRect);
var
  P: TPoint;
begin
  P := R.TopLeft;
  if Edit.HandleAllocated then
  begin
    R := dxMapWindowRect(Edit.Handle, Edit.Parent.Handle, R);
    R := cxRectOffset(R, Edit.BoundsRect.TopLeft, False);
  end;
  cxDrawTransparentControlBackground(Edit, ACanvas, R, P);
end;

procedure TcxGroupBoxViewInfo.DrawUsualBackground(ACanvas: TcxCanvas);
begin
  if Edit.HasShadow then
    DrawContainerShadow(ACanvas, GetFrameBounds);

  if Edit.IsTransparent then
    DrawParentBackground(ACanvas, ControlRect)
  else
    cxEditFillRect(ACanvas, ControlRect, BackgroundColor);
end;

procedure TcxGroupBoxViewInfo.DrawNativeBackground(ACanvas: TcxCanvas;
  const ACaptionRect: TRect);
begin
  if IsPanelStyle then
    DrawNativePanelBackground(ACanvas, ACaptionRect)
  else
    DrawNativeGroupBoxBackground(ACanvas);
end;

procedure TcxGroupBoxViewInfo.DrawNativeGroupBoxBackground(ACanvas: TcxCanvas);
var
  AClipRgn: TcxRegion;
  ANativeState: Integer;
  ATheme: TdxTheme;
begin
  AClipRgn := ACanvas.GetClipRegion;
  try
    ATheme := OpenTheme(totButton);
    ANativeState := cxNativeState[Enabled];
    if Edit.IsTransparent then
      DrawParentBackground(ACanvas, Bounds)
    else
      if Edit.IsNativeBackground and
          IsThemeBackgroundPartiallyTransparent(ATheme, BP_GROUPBOX, ANativeState) then
      begin
        cxDrawThemeParentBackground(Edit, ACanvas, Bounds);
        ACanvas.Canvas.Refresh; // SC-B31215
      end
      else
        ACanvas.FillRect(Bounds, BackgroundColor);
  finally
    ACanvas.SetClipRegion(AClipRgn, roSet);
  end;
end;

procedure TcxGroupBoxViewInfo.DrawNativePanelBackground(
  ACanvas: TcxCanvas; const ACaptionRect: TRect);
var
  ABackgroundRect: TRect;
begin
  ABackgroundRect := GetBoundsForPanel;
  if BorderStyle <> ebsNone then
    InflateRect(ABackgroundRect, -Edit.LookAndFeelPainter.BorderSize, -Edit.LookAndFeelPainter.BorderSize);

  if Edit.IsTransparent then
  begin
    ACanvas.SaveClipRegion;
    try
      ACanvas.SetClipRegion(TcxRegion.Create(ABackgroundRect), roIntersect);
      Edit.LookAndFeelPainter.DrawPanelBackground(ACanvas, Edit, GetBoundsForPanel);
    finally
      ACanvas.RestoreClipRegion;
    end;
  end
  else
    if Edit.LookAndFeel.NativeStyle then
      if Edit.IsNativeBackground then
        cxDrawThemeParentBackground(Edit, ACanvas, ABackgroundRect)
      else
        Edit.LookAndFeelPainter.DrawPanelBackground(ACanvas, Edit, ABackgroundRect, BackgroundColor)
    else
      if Edit.LookAndFeel.Kind = lfOffice11 then
        DrawOffice11PanelBackground(ACanvas, ABackgroundRect);
end;

procedure TcxGroupBoxViewInfo.DrawOffice11PanelBackground(ACanvas: TcxCanvas; const R: TRect);
begin
  with Edit.LookAndFeelPainter do
    case Edit.PanelStyle.OfficeBackgroundKind of
      pobkGradient:
        DrawPanelBackground(ACanvas, Edit, R, dxOffice11ToolbarsColor1, dxOffice11ToolbarsColor2);
      pobkOffice11Color:
        DrawPanelBackground(ACanvas, Edit, R, dxGetMiddleRGB(dxOffice11ToolbarsColor1, dxOffice11ToolbarsColor2, 50));
      pobkStyleColor:
        DrawPanelBackground(ACanvas, Edit, R, BackgroundColor);
    end;
end;

procedure TcxGroupBoxViewInfo.InternalDrawBackground(ACanvas: TcxCanvas);
begin
  if NativeStyle or (IsPanelStyle and Assigned(Edit.LookAndFeelPainter) and
    (Edit.LookAndFeelPainter.LookAndFeelStyle = lfsOffice11))
  then
    DrawNativeBackground(ACanvas, CaptionRect)
  else
  begin
    ACanvas.SaveClipRegion;
    try
      if UseSkins then
        InternalDrawBackgroundByPainter(ACanvas)
      else
        DrawUsualBackground(ACanvas);
    finally
      ACanvas.RestoreClipRegion;
    end;
  end;
end;

procedure TcxGroupBoxViewInfo.InternalDrawBackgroundByPainter(ACanvas: TcxCanvas);
var
  ACaptionPos: TcxGroupBoxCaptionPosition;
  AContentRect: TRect;
begin
  if IsPanelStyle then
    AContentRect := GetBoundsForPanel
  else
    if BorderStyle = ebsNone then
      AContentRect := Bounds
    else
      AContentRect := BorderRect;

  if not Edit.DoCustomDrawContentBackground(ACanvas, AContentRect, Painter) then
  begin
    if IsPanelStyle then
      Painter.DrawPanelContent(ACanvas, AContentRect, BorderStyle <> ebsNone)
    else
    begin
      if BorderStyle = ebsNone then
        Painter.DrawGroupBoxBackground(ACanvas, AContentRect, AContentRect)
      else
      begin
        if Edit.Caption = '' then
          ACaptionPos := cxgpCenter
        else
          ACaptionPos := GetBiDiModeCaptionPosition;

        if Painter.IsGroupBoxTransparent(False, ACaptionPos) or Painter.IsGroupBoxTransparent(True, ACaptionPos) then
          DrawParentBackground(ACanvas, ControlRect);

        case ACaptionPos of
          cxgpTop:
            AContentRect.Top := CaptionRect.Bottom;
          cxgpBottom:
            AContentRect.Bottom := CaptionRect.Top;
          cxgpLeft:
            AContentRect.Left := CaptionRect.Right;
          cxgpRight:
            AContentRect.Right := CaptionRect.Left;
        end;

        Painter.DrawGroupBoxContent(ACanvas, AContentRect, ACaptionPos, Edges);
      end;
    end;
  end;
end;

{ TcxGroupBoxViewData }

procedure TcxGroupBoxViewData.Calculate(ACanvas: TcxCanvas;
  const ABounds: TRect; const P: TPoint; Button: TcxMouseButton; Shift: TShiftState;
  AViewInfo: TcxCustomEditViewInfo; AIsMouseEvent: Boolean);
begin
  TcxGroupBoxViewInfo(AViewInfo).IsDesigning := IsDesigning;
  inherited Calculate(ACanvas, ABounds, P, Button, Shift, AViewInfo, AIsMouseEvent);
  if Edit <> nil then
    TcxGroupBoxViewInfo(AViewInfo).CheckBoxVisible := Edit.GetCheckBoxVisible;
end;

procedure TcxGroupBoxViewData.CalculateButtonsViewInfo(ACanvas: TcxCanvas; const ABounds: TRect; const P: TPoint;
  Button: TcxMouseButton; Shift: TShiftState; AViewInfo: TcxCustomEditViewInfo;
  AIsMouseEvent: Boolean);
begin
  if not IsInplace then
  begin
    Edit.CalculateCaptionFont;
    CalculateRects(ACanvas, TcxGroupBoxViewInfo(AViewInfo));
  end;
end;

function TcxGroupBoxViewData.GetBorderColor: TColor;
begin
  if Style.BorderStyle in [ebsUltraFlat, ebsOffice11] then
  begin
    if Enabled then
      Result := GetEditBorderHighlightColor(Style.BorderStyle = ebsOffice11)
    else
      Result := clBtnShadow;
  end
  else
    Result := Style.BorderColor;
end;

function TcxGroupBoxViewData.GetBorderExtent: TRect;
var
  AHeaderSideBorderOffset: Integer;
begin
  Result := inherited GetBorderExtent;
  if not IsInplace and (Edit.Alignment <> alCenterCenter) and not IsPanelStyle then
  begin
    AHeaderSideBorderOffset := cxTextHeight(Edit.FCaptionFont) div 2 - 1 + cxEditMaxBorderWidth;
    case CaptionPosition of
      cxgpTop:
        Result.Top := AHeaderSideBorderOffset;
      cxgpBottom:
        Result.Bottom := AHeaderSideBorderOffset;
      cxgpLeft:
        Result.Left := AHeaderSideBorderOffset - 1;
      cxgpRight:
        Result.Right := AHeaderSideBorderOffset - 1;
    end;
  end;
end;

function TcxGroupBoxViewData.GetClientExtent(
  ACanvas: TcxCanvas; AViewInfo: TcxCustomEditViewInfo): TRect;

  function GetHeaderSideClientExtent: Integer;
  var
    R: TRect;
  begin
    if Style.LookAndFeel.SkinPainter = nil then
      Result := cxTextHeight(Edit.FCaptionFont)
    else
    begin
      R := TcxGroupBoxViewInfo(AViewInfo).CaptionRect;
      R := cxRectContent(R, TcxGroupBoxViewInfo(AViewInfo).CaptionRectIndent);
      case CaptionPosition of
        cxgpTop, cxgpBottom:
          Result := cxRectHeight(R);
        cxgpLeft, cxgpRight:
          Result := cxRectWidth(R);
      else
        Result := 0;
      end;
    end;
  end;

var
  AContentOffsets: TRect;
  AHeaderSideClientExtent: Integer;
begin
  Result := inherited GetBorderExtent;
  if not (IsInplace or (AViewInfo = nil) or not AViewInfo.UseSkins) then
  begin
    AContentOffsets := GetContentOffsetsByPainter(TcxGroupBoxViewInfo(AViewInfo));
    Inc(Result.Top, AContentOffsets.Top);
    Inc(Result.Left, AContentOffsets.Left);
    Inc(Result.Right, AContentOffsets.Right);
    Inc(Result.Bottom, AContentOffsets.Bottom);
  end;

  if not (IsInplace or IsPanelStyle) and (Edit.Alignment <> alCenterCenter) then
  begin
    AHeaderSideClientExtent := Result.Top + GetHeaderSideClientExtent;
    if (AViewInfo = nil) or not AViewInfo.UseSkins then
      Inc(AHeaderSideClientExtent, cxEditMaxBorderWidth + 1);
    case CaptionPosition of
      cxgpTop:
        Result.Top := AHeaderSideClientExtent;
      cxgpBottom:
        Result.Bottom := AHeaderSideClientExtent;
      cxgpLeft:
        Result.Left := AHeaderSideClientExtent;
      cxgpRight:
        Result.Right := AHeaderSideClientExtent;
    end;
  end;
end;

function TcxGroupBoxViewData.GetContentOffsetsByPainter(AViewInfo: TcxGroupBoxViewInfo): TRect;
begin
  if IsPanelStyle then
    Result := AViewInfo.Painter.PanelBorderSize
  else
    Result := AViewInfo.Painter.GroupBoxBorderSize(False, CaptionPosition);

  case CaptionPosition of
    cxgpTop:
      if HasNonClientArea then
        Inc(Result.Bottom, AViewInfo.CaptionRectIndent.Top)
      else
        Inc(Result.Top, AViewInfo.CaptionRectIndent.Top);

    cxgpLeft:
      if HasNonClientArea then
        Inc(Result.Right, AViewInfo.CaptionRectIndent.Left)
      else
        Inc(Result.Left, AViewInfo.CaptionRectIndent.Left);
  end;
end;

function TcxGroupBoxViewData.HasShadow: Boolean;
begin
  Result := Edit.HasShadow and inherited HasShadow;
end;

class function TcxGroupBoxViewData.IsNativeStyle(ALookAndFeel: TcxLookAndFeel): Boolean;
begin
  Result := AreVisualStylesMustBeUsed(
    ALookAndFeel.NativeStyle or (ALookAndFeel.Kind = lfOffice11), totEdit) and
      (ALookAndFeel.SkinPainter = nil);
end;

function TcxGroupBoxViewData.GetContainerState(const ABounds: TRect;
  const P: TPoint; Button: TcxMouseButton; Shift: TShiftState;
  AIsMouseEvent: Boolean): TcxContainerState;
begin
  if Enabled then
    Result := [csNormal]
  else
    Result := [csDisabled];
end;

function TcxGroupBoxViewData.IsPanelStyle: Boolean;
begin
  Result := (Edit <> nil) and Edit.IsPanelStyle;
end;

function TcxGroupBoxViewData.HasNonClientArea: Boolean;
begin
  Result := not IsInplace and Edit.HasNonClientArea;
end;

function TcxGroupBoxViewData.GetShadowWidth: Integer;
begin
  Result := 0;
  if HasShadow then
    Result := cxContainerShadowWidth;
end;

function TcxGroupBoxViewData.GetCaptionPosition: TcxGroupBoxCaptionPosition;
begin
  Result := cxGroupBoxAlignmentToGroupBoxCaptionPosition(Edit.Alignment);
end;

function TcxGroupBoxViewData.GetEdit: TcxCustomGroupBox;
begin
  Result := TcxCustomGroupBox(FEdit);
end;

procedure TcxGroupBoxViewData.CalculateRects(ACanvas: TcxCanvas; AEditViewInfo: TcxGroupBoxViewInfo);
begin
  Edit.AdjustCanvasFontSettings(ACanvas);
  if IsPanelStyle then
    AEditViewInfo.CalculateRectsForPanel(ACanvas)
  else
    if Style.LookAndFeel.SkinPainter = nil then
      AEditViewInfo.CalculateRectsForStandardPainter(ACanvas, GetShadowWidth)
    else
      AEditViewInfo.CalculateRectsForSkinPainter(ACanvas, HasNonClientArea);
  AEditViewInfo.CalculateFocusRect;
end;

procedure TcxGroupBoxViewData.DoRightToLeftConversion(AViewInfo: TcxCustomEditViewInfo; const ABounds: TRect);
var
  AGroupBoxViewInfo: TcxGroupBoxViewInfo;
  R: TRect;
  AIndent: TRect;
begin
  R := ABounds;
  AGroupBoxViewInfo := TcxGroupBoxViewInfo(AViewInfo);
  AIndent := AGroupBoxViewInfo.CaptionRectIndent;
  if HasNonClientArea then
    case AGroupBoxViewInfo.CaptionPosition of
      cxgpLeft:
        Dec(R.Right, AIndent.Left);
      cxgpRight:
        Dec(R.Right, AIndent.Right);
    end;
  inherited DoRightToLeftConversion(AViewInfo, R);
  AGroupBoxViewInfo.CaptionRect := TdxRightToLeftLayoutConverter.ConvertRect(AGroupBoxViewInfo.CaptionRect, R);
  AGroupBoxViewInfo.TextRect := TdxRightToLeftLayoutConverter.ConvertRect(AGroupBoxViewInfo.TextRect, R);
  AGroupBoxViewInfo.FCheckBoxRect := TdxRightToLeftLayoutConverter.ConvertRect(AGroupBoxViewInfo.FCheckBoxRect, R);
  AGroupBoxViewInfo.MouseFocusingRect := TdxRightToLeftLayoutConverter.ConvertRect(AGroupBoxViewInfo.MouseFocusingRect, R);
  AGroupBoxViewInfo.FocusRect := TdxRightToLeftLayoutConverter.ConvertRect(AGroupBoxViewInfo.FocusRect, R);
end;

{ TcxButtonGroupButtonViewInfo }

function TcxButtonGroupButtonViewInfo.CreateFadingHelper: TcxEditButtonFadingHelper;
begin
  Result := TcxButtonGroupButtonFadingHelper.Create(Self);
end;

{ TcxButtonGroupButtonFadingHelper }

procedure TcxButtonGroupButtonFadingHelper.DrawButton(ACanvas: TcxCanvas);
begin
  EditViewInfo.DrawButtonGlyph(ACanvas, ViewInfo, GetButtonRect);
end;

function TcxButtonGroupButtonFadingHelper.GetButtonRect: TRect;
begin
  Result := ViewInfo.GetGlyphRect(EditViewInfo.GetGlyphSize, EditViewInfo.Alignment, False);
end;

function TcxButtonGroupButtonFadingHelper.GetViewInfo: TcxButtonGroupButtonViewInfo;
begin
  Result := TcxButtonGroupButtonViewInfo(inherited ViewInfo);
end;

function TcxButtonGroupButtonFadingHelper.InternalGetEditViewInfo: TcxButtonGroupViewInfo;
begin
  Result := inherited EditViewInfo as TcxButtonGroupViewInfo;
end;

function TcxButtonGroupButtonFadingHelper.PrepareFadingImage(AState: TcxEditButtonState): TcxBitmap32;
begin
  Result := inherited PrepareFadingImage(AState);
  if EditViewInfo.IsInplace and not (EditViewInfo.NativeStyle or EditViewInfo.IsBackgroundTransparent) then
    Result.MakeOpaque;
end;

{ TcxButtonGroupViewInfo }

procedure TcxButtonGroupViewInfo.DrawEditButton(ACanvas: TcxCanvas; AButtonVisibleIndex: Integer);
var
  AButtonViewInfo: TcxGroupBoxButtonViewInfo;
  AGlyphRect: TRect;
begin
  AButtonViewInfo := TcxGroupBoxButtonViewInfo(ButtonsInfo[AButtonVisibleIndex]);
  AGlyphRect := AButtonViewInfo.GetGlyphRect(GetGlyphSize, Alignment, IsDBEditPaintCopyDrawing);
  if not IsDBEditPaintCopyDrawing then
    DrawEditBackground(ACanvas, AButtonViewInfo.Bounds, AGlyphRect,
      IsButtonGlyphTransparent(AButtonViewInfo));
  if not AButtonViewInfo.FadingHelper.DrawImage(ACanvas.Handle, AGlyphRect) then
    DrawButtonGlyph(ACanvas, AButtonViewInfo, AGlyphRect);
  DrawButtonCaption(ACanvas, AButtonViewInfo, AGlyphRect);
end;

function TcxButtonGroupViewInfo.GetButtonViewInfoClass: TcxEditButtonViewInfoClass;
begin
  Result := TcxButtonGroupButtonViewInfo;
end;

function TcxButtonGroupViewInfo.GetGlyphSize: TSize;
begin
  Result := GlyphSize;
end;

{ TcxButtonGroupViewData }

procedure TcxButtonGroupViewData.Calculate(ACanvas: TcxCanvas;
  const ABounds: TRect; const P: TPoint; Button: TcxMouseButton;
  Shift: TShiftState; AViewInfo: TcxCustomEditViewInfo; AIsMouseEvent: Boolean);
var
  AGroupViewInfo: TcxButtonGroupViewInfo;
begin
  inherited Calculate(ACanvas, ABounds, P, Button, Shift, AViewInfo, AIsMouseEvent);
  AGroupViewInfo := AViewInfo as TcxButtonGroupViewInfo;
  AGroupViewInfo.DrawTextFlags := GetDrawTextFlags;
  AGroupViewInfo.CaptionExtent := GetCaptionRectExtent;
end;

procedure TcxButtonGroupViewData.CalculateButtonsViewInfo(ACanvas: TcxCanvas;
  const ABounds: TRect; const P: TPoint; Button: TcxMouseButton;
  Shift: TShiftState; AViewInfo: TcxCustomEditViewInfo; AIsMouseEvent: Boolean);

  procedure CalculateButtonStates;
  var
    AButtonsCount, APrevPressedButton, I: Integer;
    AButtonViewInfo: TcxGroupBoxButtonViewInfo;
    ACapturePressing, AHoldPressing, AIsButtonPressed, AMouseButtonPressing: Boolean;
  begin
    AButtonsCount := Properties.Items.Count;
    AViewInfo.IsButtonReallyPressed := False;
    if AIsMouseEvent then
      APrevPressedButton := AViewInfo.PressedButton
    else
      APrevPressedButton := -1;
    AViewInfo.PressedButton := -1;
    AViewInfo.SelectedButton := -1;

    for I := 0 to AButtonsCount - 1 do
    begin
      AButtonViewInfo := TcxGroupBoxButtonViewInfo(AViewInfo.ButtonsInfo[I]);
      AButtonViewInfo.ButtonIndex := I;
      AButtonViewInfo.Data.NativeStyle := IsButtonNativeStyle(Style.LookAndFeel);
      AButtonViewInfo.Data.Transparent := (Self.Style.ButtonTransparency = ebtAlways) or
        (Self.Style.ButtonTransparency = ebtInactive) and not Selected;

      AButtonViewInfo.Data.BackgroundColor := AViewInfo.BackgroundColor;
      AIsButtonPressed := IsButtonPressed(AViewInfo, I);
      with AButtonViewInfo do
      begin
        if not Enabled then
          Data.State := ebsDisabled
        else
          if AIsButtonPressed or (not IsDesigning and PtInRect(AButtonViewInfo.Bounds, P)) then
          begin
            ACapturePressing := (Button = cxmbNone) and (ssLeft in Shift) and
              (Data.State = ebsNormal) and (GetCaptureButtonVisibleIndex = I);
            AMouseButtonPressing := (Button = ButtonTocxButton(mbLeft)) and
              cxShiftStateLeftOnly(Shift, True);
            AHoldPressing := (Data.State = ebsPressed) and (ssLeft in Shift);
            if AIsButtonPressed or AMouseButtonPressing or AHoldPressing or ACapturePressing then
              AViewInfo.IsButtonReallyPressed := True;
            if not AIsButtonPressed and cxShiftStateMoveOnly(Shift) and not ACapturePressing then
            begin
              Data.State := ebsSelected;
              AViewInfo.SelectedButton := I;
            end
            else
              if (AIsButtonPressed or ACapturePressing and CanPressButton(AViewInfo, I) or
                cxShiftStateLeftOnly(Shift, True) and ((Button = cxmbLeft) and CanPressButton(AViewInfo, I) or
                (APrevPressedButton = I))) or AHoldPressing then
              begin
                Data.State := ebsPressed;
                AViewInfo.PressedButton := I;
              end
              else
                Data.State := ebsNormal;
          end
          else
            Data.State := ebsNormal;
        CalculateButtonNativeInfo(AButtonViewInfo);
      end;
    end;
  end;

var
  AButtonsCount: Integer;
begin
  inherited;
  AButtonsCount := Properties.Items.Count;
  TcxGroupBoxViewInfo(AViewInfo).SetButtonCount(AButtonsCount);
  if AButtonsCount = 0 then
    Exit;

  CalculateButtonViewInfos(AViewInfo);
  CalculateButtonPositions(ACanvas, AViewInfo);
  CalculateButtonStates;
end;

function TcxButtonGroupViewData.GetEditConstantPartSize(ACanvas: TcxCanvas;
  const AEditSizeProperties: TcxEditSizeProperties;
  var MinContentSize: TSize; AViewInfo: TcxCustomEditViewInfo = nil): TSize;
var
  AButtonsCount, AButtonsPerColumn, AColumnsCount: Integer;
  ACaption: string;
  AColumnWidth, AMaxButtonHeight: Integer;
  ADefaultButtonHeight, AButtonHeight: Integer;
  AFlags: Integer;
  AMaxColumnWidth: Integer;
  ASizeCorrection: TSize;
  ATextWidth: Integer;
  I: Integer;
  R: TRect;
  AEditMetrics: TcxEditMetrics;
begin
  MinContentSize := cxNullSize;
  ACanvas.Font := Style.GetVisibleFont;
  ASizeCorrection := Self.GetEditContentSizeCorrection;
  AButtonsCount := Properties.Items.Count;
  AColumnsCount := Properties.GetColumnCount;
  GetEditMetrics(AEditSizeProperties.Width >= 0, ACanvas, AEditMetrics);
  ADefaultButtonHeight := ACanvas.TextHeight('Zg') + ASizeCorrection.cy;
  if AEditSizeProperties.Width >= 0 then
  begin
    Result.cx := AEditSizeProperties.Width;
    if AButtonsCount = 0 then
      Result.cy := ADefaultButtonHeight
    else
    begin
      Result.cy := 0;
      AButtonsPerColumn := Properties.GetButtonsPerColumn;
      AColumnWidth := AEditSizeProperties.Width - ContentOffset.Left -
        ContentOffset.Right + AEditMetrics.AutoHeightWidthCorrection -
        AEditMetrics.ColumnOffset * (AColumnsCount - 1);
      AColumnWidth := AColumnWidth div AColumnsCount - AEditMetrics.ButtonSize.cx -
        AEditMetrics.AutoHeightColumnWidthCorrection;
      if AColumnWidth <= 0 then
        AColumnWidth := 1;
      AMaxButtonHeight := ADefaultButtonHeight;
      Include(PaintOptions, epoAutoHeight);
      AFlags := GetDrawTextFlags and not cxAlignVCenter or cxAlignTop;
      for I := 0 to AButtonsCount - 1 do
      begin
        R := Rect(0, 0, AColumnWidth, cxMaxRectSize);
        ACaption := Properties.Items[I].Caption;
        if Properties.WordWrap and (ACaption <> '') then
        begin
          ACanvas.TextExtent(ACaption, R, AFlags);
          AButtonHeight := R.Bottom - R.Top + ASizeCorrection.cy;
          if AMaxButtonHeight < AButtonHeight then
            AMaxButtonHeight := AButtonHeight;
        end;
      end;
      Result.cy := AMaxButtonHeight * AButtonsPerColumn;
      if not IsInplace then
      begin
        R := GetClientExtent(ACanvas, AViewInfo);
        Result.cy := Result.cy + R.Top + R.Bottom;
      end;
    end;
  end else
  begin
    if AButtonsCount = 0 then
    begin
      Result.cx := 0;
      Result.cy := ACanvas.TextHeight('Zg') + ASizeCorrection.cy;
    end else
    begin
      AMaxColumnWidth := 0;
      AButtonsPerColumn := Properties.GetButtonsPerColumn;
      for I := 0 to AButtonsCount - 1 do
      begin
        ATextWidth := ACanvas.TextWidth(Properties.Items[I].Caption);
        if ATextWidth > AMaxColumnWidth then
          AMaxColumnWidth := ATextWidth;
      end;
      Result.cx := (AMaxColumnWidth + AEditMetrics.ColumnWidthCorrection + AEditMetrics.ButtonSize.cx) *
        AColumnsCount + AEditMetrics.ColumnOffset * (AColumnsCount - 1) + AEditMetrics.WidthCorrection;
      if ADefaultButtonHeight > AEditMetrics.ButtonSize.cy then
        Result.cy := ADefaultButtonHeight
      else
        Result.cy := AEditMetrics.ButtonSize.cy;
      Result.cy := Result.cy * AButtonsPerColumn;
    end;
  end;
end;

class function TcxButtonGroupViewData.IsButtonNativeStyle(
  ALookAndFeel: TcxLookAndFeel): Boolean;
begin
  Result := AreVisualStylesMustBeUsed(ALookAndFeel.NativeStyle, totButton);
end;

procedure TcxButtonGroupViewData.CalculateButtonPositions(ACanvas: TcxCanvas;
  AViewInfo: TcxCustomEditViewInfo);
var
  AButtonsCount, AButtonsPerColumn, AButtonHeight, AButtonWidth, AClientHeight,
    AColumnsCount, ATopOffset, I: Integer;
  AButtonViewInfo: TcxGroupBoxButtonViewInfo;
  AClientExtent: TRect;
  AEditMetrics: TcxEditMetrics;
begin
  AButtonsCount := Properties.Items.Count;
  AColumnsCount := Properties.GetColumnCount;
  AButtonsPerColumn := Properties.GetButtonsPerColumn;
  AClientExtent := GetClientExtent(ACanvas, AViewInfo);
  GetEditMetrics(False, nil, AEditMetrics);
  AButtonWidth := (cxRectWidth(AViewInfo.Bounds) - AClientExtent.Left - AClientExtent.Right +
    AEditMetrics.ClientWidthCorrection - AEditMetrics.ColumnOffset * (AColumnsCount - 1)) div AColumnsCount;
  AClientHeight := cxRectHeight(AViewInfo.Bounds) - AClientExtent.Top - AClientExtent.Bottom;

  ATopOffset := AViewInfo.Bounds.Top + AClientExtent.Top + (AClientHeight mod AButtonsPerColumn) div 2;

    AButtonHeight := AClientHeight div AButtonsPerColumn;

  for I := 0 to AButtonsCount - 1 do
  begin
    AButtonViewInfo := TcxGroupBoxButtonViewInfo(AViewInfo.ButtonsInfo[I]);
    AButtonViewInfo.Bounds.Left := AViewInfo.Bounds.Left + AClientExtent.Left +
      AButtonViewInfo.Column * (AButtonWidth + AEditMetrics.ColumnOffset) +
      AEditMetrics.ClientLeftBoundCorrection;
    AButtonViewInfo.Bounds.Top := ATopOffset + AButtonViewInfo.Row * AButtonHeight;
    AButtonViewInfo.Bounds.Right := AButtonViewInfo.Bounds.Left + AButtonWidth;
    AButtonViewInfo.Bounds.Bottom := AButtonViewInfo.Bounds.Top + AButtonHeight;
    if UseRightToLeftAlignment then
      AButtonViewInfo.Bounds := TdxRightToLeftLayoutConverter.ConvertRect(AButtonViewInfo.Bounds, AViewInfo.Bounds);
    AButtonViewInfo.VisibleBounds := AButtonViewInfo.Bounds;
  end;
end;

procedure TcxButtonGroupViewData.CalculateButtonViewInfos(AViewInfo: TcxCustomEditViewInfo);

  function GetButtonStyle: TcxEditButtonStyle;
  const
    AButtonInplaceStyleMap: array[TcxLookAndFeelKind] of TcxEditButtonStyle =
      (btsFlat, bts3D, btsUltraFlat,
      btsOffice11);
    AButtonStyleMap: array [TcxEditBorderStyle] of TcxEditButtonStyle =
      (bts3D, btsFlat, btsFlat, btsFlat, bts3D, btsUltraFlat,
      btsOffice11);
  begin
    if IsInplace then
      Result := AButtonInplaceStyleMap[Style.LookAndFeel.Kind]
    else
      case Style.BorderStyle of
        ebsUltraFlat:
          Result := btsUltraFlat;
        ebsOffice11:
          Result := btsOffice11;
        else
          Result := AButtonStyleMap[AViewInfo.BorderStyle];
      end;
  end;

var
  AButtonsCount, AButtonsPerColumn, I: Integer;
  AButtonStyle: TcxEditButtonStyle;
  AButtonViewInfo: TcxGroupBoxButtonViewInfo;
  AFillColor: TColor;
begin
  AButtonStyle := GetButtonStyle;
  AButtonsCount := Properties.Items.Count;
  AButtonsPerColumn := Properties.GetButtonsPerColumn;

  for I := 0 to AButtonsCount - 1 do
  begin
    AButtonViewInfo := TcxGroupBoxButtonViewInfo(AViewInfo.ButtonsInfo[I]);
    with AButtonViewInfo do
    begin
      HasBackground := AViewInfo.HasBackground;
      Data.Style := AButtonStyle;
      Caption := Properties.FItems[I].Caption;
      Enabled := Properties.FItems[I].Enabled;
      if IsInplace then
      begin
        if not Enabled then
          TextColor := TcxContainerStyleAccess(Style).DefaultDisabledTextColor
        else
          TextColor := AViewInfo.TextColor;
      end
      else
        GetColorSettings(AViewInfo, AFillColor, TextColor);

      Column := I div AButtonsPerColumn;
      Row := I mod AButtonsPerColumn;
    end;
  end;
end;

function TcxButtonGroupViewData.GetDrawTextFlags: Integer;
begin
  Result := cxAlignLeft;
  if UseRightToLeftAlignment then
    Result := TdxRightToLeftLayoutConverter.ConvertcxDrawTextAlignment(Result);
  if UseRightToLeftReading then
    Result := Result or cxRtlReading;
  Result := Result or cxAlignVCenter or cxShowPrefix;
  if (epoAutoHeight in PaintOptions) and Properties.WordWrap then
  begin
    Result := Result or cxDontClip;
    Result := Result or cxWordBreak;
  end
  else
  begin
    Result := Result or cxSingleLine;
    if Properties.ShowEndEllipsis then
      Result := Result or cxShowEndEllipsis;
  end;
end;

function TcxButtonGroupViewData.GetCaptionRectExtent: TRect;
begin
  Result := cxEmptyRect;
end;

function TcxButtonGroupViewData.GetProperties: TcxCustomButtonGroupProperties;
begin
  Result := TcxCustomButtonGroupProperties(FProperties);
end;

{ TcxCustomGroupBoxProperties }

function TcxCustomGroupBoxProperties.CanCompareEditValue: Boolean;
begin
  Result := True;
end;

class function TcxCustomGroupBoxProperties.GetContainerClass: TcxContainerClass;
begin
  Result := TcxCustomGroupBox;
end;

class function TcxCustomGroupBoxProperties.GetViewInfoClass: TcxContainerViewInfoClass;
begin
  Result := TcxGroupBoxViewInfo;
end;

class function TcxCustomGroupBoxProperties.GetViewDataClass: TcxCustomEditViewDataClass;
begin
  Result := TcxGroupBoxViewData;
end;

function TcxCustomGroupBoxProperties.HasDisplayValue: Boolean;
begin
  Result := True;
end;

{ TcxButtonGroupItem }

constructor TcxButtonGroupItem.Create(Collection: TCollection);
begin
  if Assigned(Collection) then
    Collection.BeginUpdate;
  try
    inherited Create(Collection);
    FEnabled := True;
    DoChanged(Collection, copAdd);
  finally
    if Assigned(Collection) then
      Collection.EndUpdate;
  end;
end;

destructor TcxButtonGroupItem.Destroy;
var
  ACollection: TCollection;
  AIndex: Integer;
begin
  ACollection := Collection;
  if not IsCollectionDestroying then
    AIndex := Index
  else
    AIndex := -1;
  if Assigned(ACollection) then
    ACollection.BeginUpdate;
  try
    inherited Destroy;
    DoChanged(ACollection, copDelete, AIndex);
  finally
    if Assigned(ACollection) then
      ACollection.EndUpdate;
  end;
end;

procedure TcxButtonGroupItem.Assign(Source: TPersistent);
begin
  if Source is TcxButtonGroupItem then
    Enabled := TcxButtonGroupItem(Source).Enabled;
  inherited Assign(Source);
end;

procedure TcxButtonGroupItem.DoChanged(ACollection: TCollection;
  ACollectionOperation: TcxCollectionOperation; AIndex: Integer = -1);
begin
  if Assigned(ACollection) then
    if AIndex = -1 then
      TcxButtonGroupItems(ACollection).InternalNotify(Self, AIndex, ACollectionOperation)
    else
      TcxButtonGroupItems(ACollection).InternalNotify(nil, AIndex, ACollectionOperation);
end;

function TcxButtonGroupItem.GetIsCollectionDestroying: Boolean;
begin
  Result := (Collection <> nil) and TcxButtonGroupItems(Collection).IsDestroying;
end;

procedure TcxButtonGroupItem.SetEnabled(Value: Boolean);
begin
  if Value <> FEnabled then
  begin
    FEnabled := Value;
    DoChanged(Collection, copChanged);
  end;
end;

{ TcxButtonGroupItems }

procedure TcxButtonGroupItems.Assign(Source: TPersistent);
begin
  OwningProperties.BeginUpdate;
  try
    inherited Assign(Source);
  finally
    OwningProperties.EndUpdate;
  end;
end;

procedure TcxButtonGroupItems.InternalNotify(AItem: TcxButtonGroupItem;
  AItemIndex: Integer; AItemOperation: TcxCollectionOperation);
begin
  if OwningProperties.ChangedLocked or IsDestroying then
    Exit;
  if AItem <> nil then
    FChangedItemIndex := AItem.Index
  else
    FChangedItemIndex := AItemIndex;
  FChangedItemOperation := AItemOperation;
  FItemChanged := True;
  try
    OwningProperties.Changed;
  finally
    FItemChanged := False;
  end;
end;

procedure TcxButtonGroupItems.Update(Item: TCollectionItem);
begin
  if IsDestroying  then Exit;
  if not OwningProperties.ChangedLocked then
  begin
    if Item <> nil then
    begin
      FChangedItemIndex := Item.Index;
      FChangedItemOperation := copChanged;
    end;
    inherited;
  end;

  OwningProperties.Changed;
end;

function TcxButtonGroupItems.GetItem(Index: Integer): TcxButtonGroupItem;
begin
  Result := TcxButtonGroupItem(inherited Items[Index]);
end;

function TcxButtonGroupItems.GetOwningProperties: TcxCustomEditProperties;
begin
  Result := GetOwner as TcxCustomEditProperties;
end;

procedure TcxButtonGroupItems.SetItem(Index: Integer; Value: TcxButtonGroupItem);
begin
  inherited Items[Index] := Value;
end;

{ TcxCustomButtonGroupProperties }

constructor TcxCustomButtonGroupProperties.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FColumns := 1;
  FItems := CreateItems;
end;

destructor TcxCustomButtonGroupProperties.Destroy;
begin
  BeginUpdate;
  try
    FreeAndNil(FItems);
  finally
    EndUpdate(False);
  end;
  inherited Destroy;
end;

function TcxCustomButtonGroupProperties.CreatePreviewProperties: TcxCustomEditProperties;
const
  AItemCaptions: array [0..2] of string = ('A', 'B', 'C');
var
  I: Integer;
begin
  Result := inherited CreatePreviewProperties;
  for I := 0 to High(AItemCaptions) do
    TcxButtonGroupItem(TcxCustomButtonGroupProperties(Result).Items.Add).Caption := AItemCaptions[I];
  TcxCustomButtonGroupProperties(Result).Columns := 3;
end;

class function TcxCustomButtonGroupProperties.GetContainerClass: TcxContainerClass;
begin
  Result := TcxCustomButtonGroup;
end;

function TcxCustomButtonGroupProperties.GetSpecialFeatures: TcxEditSpecialFeatures;
begin
  Result := inherited GetSpecialFeatures + [esfMinSize];
end;

function TcxCustomButtonGroupProperties.GetSupportedOperations: TcxEditSupportedOperations;
begin
  Result := [esoAlwaysHotTrack, esoAutoHeight, esoEditing, esoFiltering,
    esoShowingCaption, esoSorting, esoTransparency, esoNeedHandle];
  if Items.Count > 0 then
    Include(Result, esoHotTrack);
end;

class function TcxCustomButtonGroupProperties.GetViewInfoClass: TcxContainerViewInfoClass;
begin
  Result := TcxButtonGroupViewInfo;
end;

procedure TcxCustomButtonGroupProperties.DoAssign(AProperties: TcxCustomEditProperties);
begin
  inherited;
  if AProperties is TcxCustomButtonGroupProperties then
    with TcxCustomButtonGroupProperties(AProperties) do
    begin
      Self.Columns := Columns;
      Self.Items := Items;
      Self.ShowEndEllipsis := ShowEndEllipsis;
      Self.WordWrap := WordWrap;
    end;
end;

class function TcxCustomButtonGroupProperties.GetViewDataClass: TcxCustomEditViewDataClass;
begin
  Result := TcxButtonGroupViewData;
end;

function TcxCustomButtonGroupProperties.GetColumnCount: Integer;
var
  AButtonCount, AButtonsPerColumn: Integer;
begin
  Result := Columns;
  AButtonCount := Items.Count;
  if Result > AButtonCount then
    Result := AButtonCount;
  if AButtonCount > 0 then
  begin
    if Result = 0 then
      Result := 1;
    AButtonsPerColumn := (AButtonCount + Result - 1) div Result;
    Result := (AButtonCount + AButtonsPerColumn - 1) div AButtonsPerColumn;
  end;
end;

function TcxCustomButtonGroupProperties.CreateItems: TcxButtonGroupItems;
begin
  Result := TcxButtonGroupItems.Create(Self, TcxButtonGroupItem);
end;

function TcxCustomButtonGroupProperties.GetButtonsPerColumn: Integer;
var
  AColumnsCount: Integer;
begin
  AColumnsCount := GetColumnCount;
  Result := (Items.Count + AColumnsCount - 1) div AColumnsCount;
end;

procedure TcxCustomButtonGroupProperties.SetColumns(Value: Integer);
begin
  if Value < 1 then
    Value := 1;
  if Value <> FColumns then
  begin
    FColumns := Value;
    Changed;
  end;
end;

procedure TcxCustomButtonGroupProperties.SetItems(Value: TcxButtonGroupItems);
begin
  FItems.Assign(Value);
end;

procedure TcxCustomButtonGroupProperties.SetShowEndEllipsis(Value: Boolean);
begin
  if FShowEndEllipsis <> Value then
  begin
    FShowEndEllipsis := Value;
    Changed;
  end;
end;

procedure TcxCustomButtonGroupProperties.SetWordWrap(Value: Boolean);
begin
  if FWordWrap <> Value then
  begin
    FWordWrap := Value;
    Changed;
  end;
end;

{ TcxPanelStyle }

constructor TcxPanelStyle.Create(AOwner: TcxCustomGroupBox);
begin
  inherited Create;
  FEdit := AOwner;
  FCaptionIndent := 2;
  FActive := False;
  FOfficeBackgroundKind := pobkOffice11Color;
  FWordWrap := False;
  Edit.ScaleFactor.ListenerAdd(ScaleFactorChangeHandler);
end;

destructor TcxPanelStyle.Destroy;
begin
  Edit.ScaleFactor.ListenerRemove(ScaleFactorChangeHandler);
  inherited Destroy;
end;

procedure TcxPanelStyle.Assign(ASource: TPersistent);
begin
  if ASource is TcxPanelStyle then
  begin
    Active := TcxPanelStyle(ASource).Active;
    CaptionIndent := TcxPanelStyle(ASource).CaptionIndent;
    WordWrap := TcxPanelStyle(ASource).WordWrap;
    OfficeBackgroundKind := TcxPanelStyle(ASource).OfficeBackgroundKind;
  end
  else
    inherited Assign(ASource);
end;

procedure TcxPanelStyle.ScaleFactorChangeHandler(Sender: TObject; M, D: Integer; IsLoading: Boolean);
begin
  if not IsLoading then
    ChangeScale(M, D);
end;

procedure TcxPanelStyle.SetActive(AValue: Boolean);
begin
  if AValue <> FActive then
  begin
    FActive := AValue;
    Update;
  end;
end;

procedure TcxPanelStyle.SetBorderWidth(AValue: TBorderWidth);
begin
  if AValue <> FBorderWidth then
  begin
    FBorderWidth := AValue;
    if Active then
      Update;
  end;
end;

procedure TcxPanelStyle.SetCaptionIndent(AValue: Integer);
begin
  AValue := Max(2, AValue);
  if AValue <> FCaptionIndent then
  begin
    FCaptionIndent := AValue;
    if FActive then
      Update;
  end;
end;

procedure TcxPanelStyle.SetOfficeBackgroundKind(
  AValue: TcxPanelOffice11BackgroundKind);
begin
  if FOfficeBackgroundKind <> AValue then
  begin
    FOfficeBackgroundKind := AValue;
    if FActive and (Edit.LookAndFeel.Kind = lfOffice11) then
      Update;
  end;
end;

procedure TcxPanelStyle.SetWordWrap(AValue: Boolean);
begin
  if AValue <> FWordWrap then
  begin
    FWordWrap := AValue;
    if FActive then
      Update;
  end;
end;

procedure TcxPanelStyle.ChangeScale(M, D: Integer);
begin
  BorderWidth := MulDiv(BorderWidth, M, D);
  CaptionIndent := MulDiv(CaptionIndent, M, D);
end;

procedure TcxPanelStyle.Update;
begin
  Edit.UpdateCaption;
end;

{ TcxCustomGroupBox }

constructor TcxCustomGroupBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRedrawOnResize := True;
  ControlStyle := ControlStyle + [csParentBackground];
end;

destructor TcxCustomGroupBox.Destroy;
begin
  FreeAndNil(FCaptionFont);
  FreeAndNil(FPanelStyle);
  inherited Destroy;
end;

class function TcxCustomGroupBox.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxCustomGroupBoxProperties;
end;

function TcxCustomGroupBox.GetBiDiModeDependentAlignment: TcxCaptionAlignment;
begin
  Result := Alignment;
  if UseRightToLeftAlignment then
    case Result of
      alTopLeft:
        Result := alTopRight;
      alTopRight:
        Result := alTopLeft;
      alBottomLeft:
        Result := alBottomRight;
      alBottomRight:
        Result := alBottomLeft;
      alLeftTop:
        Result := alRightTop;
      alLeftCenter:
        Result := alRightCenter;
      alLeftBottom:
        Result := alRightBottom;
      alRightTop:
        Result := alLeftTop;
      alRightCenter:
        Result := alLeftCenter;
      alRightBottom:
        Result := alLeftBottom;
    end;
end;

function TcxCustomGroupBox.GetCheckBoxCheckState: TcxCheckBoxState;
begin
  if VarIsNull(EditValue) or (EditValue = 0) then
    Result := cbsUnchecked
  else
    Result := cbsChecked
end;

function TcxCustomGroupBox.GetCheckBoxViewState: TcxEditCheckState;
begin
  Result := ViewInfo.FCheckBoxViewState;
end;

procedure TcxCustomGroupBox.SetCheckBoxCheckState(AValue: TcxCheckBoxState);
var
  AEditValue: TcxEditValue;
begin
  if CheckBoxCheckState <> AValue then
  begin
    PrepareEditValue(AValue, AEditValue, InternalFocused);
    InternalEditValue := AEditValue;
  end;
end;

procedure TcxCustomGroupBox.SetCheckBoxViewState(AValue: TcxEditCheckState);
begin
  if CheckBoxViewState <> AValue then
  begin
    ViewInfo.FCheckBoxViewState := AValue;
    ViewInfo.InvalidateCheckBoxRect;
    if HasNonClientArea then
      RedrawNonClientArea;
  end;
end;

function TcxCustomGroupBox.GetCaptionBkColor: TColor;
begin
  Result := FCaptionBkColor; // for CBuilder 10
end;

function TcxCustomGroupBox.GetColor: TColor;
begin
  Result := Style.Color;
end;

function TcxCustomGroupBox.GetFont: TFont;
begin
  Result := Style.GetVisibleFont;
end;

function TcxCustomGroupBox.GetViewInfo: TcxGroupBoxViewInfo;
begin
  Result := (inherited ViewInfo) as TcxGroupBoxViewInfo;
end;

function TcxCustomGroupBox.IsSkinAvailable: Boolean;
begin
  Result := (LookAndFeel <> nil) and (LookAndFeel.SkinPainter <> nil);
end;

procedure TcxCustomGroupBox.UpdateCaption;
begin
  CalculateCaptionFont;
  ShortRefreshContainer(False);
  UpdateContentExtents;
end;

procedure TcxCustomGroupBox.UpdateContentExtents;
begin
  if HandleAllocated and IsNonClientAreaSupported and not IsScaleChanging then
  begin
    dxRecalculateNonClientPart(Handle);
    PostMessage(Handle, DXM_UPDATENONCLIENTAREA, 0, 0);
  end;
  Realign;
end;

procedure TcxCustomGroupBox.SetPanelStyle(AValue: TcxPanelStyle);
begin
  if AValue <> FPanelStyle then
  begin
    FPanelStyle.Assign(AValue);
    UpdateCaption;
  end;
end;

function TcxCustomGroupBox.GetHorizontalCaptionIndent: Integer;
begin
  Result := 0;
  if IsPanelStyle then
  begin
    if Alignment in [alTopCenter, alCenterCenter, alBottomCenter] then
      Result := ScaleFactor.Apply(2)
    else
      Result := PanelStyle.CaptionIndent;
  end;
end;

function TcxCustomGroupBox.GetVerticalCaptionIndent: Integer;
begin
  Result := 0;
  if IsPanelStyle then
  begin
    if Alignment in [alLeftCenter, alCenterCenter, alRightCenter] then
      Result := ScaleFactor.Apply(2)
    else
      Result := PanelStyle.CaptionIndent;
  end;
end;

function TcxCustomGroupBox.GetPanelStyleCaptionDrawingFlags: Cardinal;
begin
  Result := 0;
  case Alignment of
    alTopLeft, alLeftTop, alLeftCenter, alLeftBottom, alBottomLeft:
      Result := Result or DT_LEFT;
    alTopCenter, alCenterCenter, alBottomCenter:
      Result := Result or DT_CENTER;
    alTopRight, alRightTop, alRightCenter, alRightBottom, alBottomRight:
      Result := Result or DT_RIGHT;
  end;
  case Alignment of
    alLeftTop, alTopLeft, alTopCenter, alTopRight, alRightTop:
      Result := Result or DT_TOP;
    alLeftCenter, alCenterCenter, alRightCenter:
      Result := Result or DT_VCENTER;
    alLeftBottom, alBottomLeft, alBottomCenter, alBottomRight, alRightBottom:
      Result := Result or DT_BOTTOM;
  end;
end;

procedure TcxCustomGroupBox.SetAlignment(Value: TcxCaptionAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    UpdateCaption;
  end;
end;

procedure TcxCustomGroupBox.SetCaptionBkColor(Value: TColor);
begin
  FCaptionBkColor := Value; // for CBuilder 10
end;

procedure TcxCustomGroupBox.SetColor(Value: TColor);
begin
  Style.Color := Value;
end;

procedure TcxCustomGroupBox.SetFont(Value: TFont);
begin
  Style.Font := Value;
end;

procedure TcxCustomGroupBox.CMDialogChar(var Message: TCMDialogChar);
begin
  with Message do
    if IsAccel(CharCode, Caption) and CanFocus then
    begin
      FIsAccelCharHandling := True;
      try
        SelectFirst;
        Result := 1;
      finally
        FIsAccelCharHandling := False;
      end;
    end
    else
      inherited;
end;

procedure TcxCustomGroupBox.CMTextChanged(var Message: TMessage);
begin
  inherited;
  UpdateContentExtents;
end;

procedure TcxCustomGroupBox.CMUpdateNonClientArea(var Message: TMessage);
begin
  if IsNonClientAreaSupported then
  begin
    cxRedrawWindow(Handle, RDW_ERASE or RDW_INVALIDATE or RDW_FRAME or RDW_ALLCHILDREN);
    Realign;
  end;
end;

procedure TcxCustomGroupBox.WMNCCalcSize(var Message: TWMNCCalcSize);
var
  R: TRect;
begin
  inherited;
  if HasNonClientArea then
  begin
    R := Message.CalcSize_Params^.rgrc[0];
    R := cxRectContent(R, GetClientOffsets);
    Message.CalcSize_Params^.rgrc[0] := R;
    Message.Result := 0;
  end;
end;

procedure TcxCustomGroupBox.WMPRINT(var Message: TMessage);
begin
  if ((PRF_NONCLIENT and Message.LParam) <> 0) and HasNonClientArea then
  begin
    cxPaintCanvas.BeginPaint(Message.WParam);
    try
      PaintNonClientArea(cxPaintCanvas);
    finally
      cxPaintCanvas.EndPaint;
    end;
  end;
  inherited;
end;

function TcxCustomGroupBox.IsNonClientAreaSupported: Boolean;
begin
  Result := cxGroupBox_SupportNonClientArea;
end;

procedure TcxCustomGroupBox.LookAndFeelChanged(Sender: TcxLookAndFeel;
  AChangedValues: TcxLookAndFeelValues);
begin
  inherited LookAndFeelChanged(Sender, AChangedValues);
  UpdateContentExtents;
  Invalidate;
end;

procedure TcxCustomGroupBox.AdjustClientRect(var Rect: TRect);
var
  AViewData: TcxCustomEditViewData;
begin
  if IsDestroying then
    Exit;
  AViewData := TcxCustomEditViewData(CreateViewData);
  try
    InitializeViewData(AViewData);
    Rect := cxRectContent(GetControlRect(Self), AViewData.GetClientExtent(Canvas, ViewInfo));
    if IsPanelStyle then
      InflateRect(Rect, -PanelStyle.BorderWidth, -PanelStyle.BorderWidth);
  finally
    FreeAndNil(AViewData);
  end;
end;

procedure TcxCustomGroupBox.CalculateViewInfo(P: TPoint; Button: TcxMouseButton;
  Shift: TShiftState; AIsMouseEvent: Boolean);
begin
  inherited;
  UpdateCheckBoxState(Shift, P.X, P.Y);
end;

function TcxCustomGroupBox.CanAutoSize: Boolean;
begin
  Result := False;
end;

function TcxCustomGroupBox.CanFocusOnClick: Boolean;
begin
  Result := InternalCanFocusOnClick and inherited CanFocusOnClick;
end;

function TcxCustomGroupBox.CanHaveTransparentBorder: Boolean;
begin
  Result := False;
end;

function TcxCustomGroupBox.InternalCanFocusOnClick: Boolean;
begin
  Result := False;
end;

procedure TcxCustomGroupBox.ContainerStyleChanged(Sender: TObject);
begin
  inherited ContainerStyleChanged(Sender);
  CalculateCaptionFont;
  UpdateContentExtents;
end;

procedure TcxCustomGroupBox.CreateHandle;
begin
  inherited CreateHandle;
  UpdateContentExtents;
end;

function TcxCustomGroupBox.CreatePanelStyle: TcxPanelStyle;
begin
  Result := TcxPanelStyle.Create(Self);
end;

function TcxCustomGroupBox.DefaultParentColor: Boolean;
begin
  Result := True;
end;

function TcxCustomGroupBox.GetCheckBoxVisible: Boolean;
begin
  Result := False;
end;

function TcxCustomGroupBox.GetClientOffsets: TRect;
begin
  Result := ViewInfo.CaptionRectIndent;
  if UseRightToLeftAlignment then
    Result := TdxRightToLeftLayoutConverter.ConvertOffsets(Result);
end;

function TcxCustomGroupBox.FadingCanFadeBackground: Boolean;
begin
  Result := False;
end;

procedure TcxCustomGroupBox.Initialize;
begin
  inherited Initialize;
  ControlStyle := ControlStyle + [csAcceptsControls, csCaptureMouse, csClickEvents];
  SetBounds(Left, Top, 185, 105);
  FCaptionFont := TFont.Create;
  CalculateCaptionFont;
  TabStop := False;
  FPanelStyle := CreatePanelStyle;
end;

function TcxCustomGroupBox.HasBackground: Boolean;
begin
  Result := not IsNativeBackground and inherited HasBackground;
end;

function TcxCustomGroupBox.InternalGetActiveStyle: TcxContainerStyle;
begin
  if csDisabled in ViewInfo.ContainerState then
    Result := FStyles.StyleDisabled
  else
    Result := FStyles.Style;
end;

function TcxCustomGroupBox.InternalGetNotPublishedStyleValues: TcxEditStyleValues;
begin
  Result := inherited InternalGetNotPublishedStyleValues;
  Include(Result, svHotTrack);
end;

function TcxCustomGroupBox.PointInCheckBox(APoint: TPoint): Boolean;
begin
  Result := ViewInfo.PointInCheckBox(APoint.X, APoint.Y);
end;

procedure TcxCustomGroupBox.ScaleFactorChanged;
begin
  inherited;
  UpdateContentExtents;
end;

procedure TcxCustomGroupBox.Toggle;
begin
  LockChangeEvents(True);
  try
    BeginUserAction;
    try
      case CheckBoxCheckState of
        cbsUnchecked:
          CheckBoxCheckState := cbsChecked;
        cbsChecked:
          CheckBoxCheckState := cbsUnchecked;
      end;
    finally
      EndUserAction;
    end;
    if ActiveProperties.ImmediatePost and CanPostEditValue and InternalValidateEdit then
      InternalPostEditValue;
  finally
    LockChangeEvents(False);
  end;
end;

procedure TcxCustomGroupBox.DoEditKeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited DoEditKeyDown(Key, Shift);
  if Key = VK_SPACE then
  begin
    if ViewInfo.CheckBoxVisible and (CheckBoxViewState in [ecsNormal, ecsHot]) then
    begin
      CheckBoxViewState := ecsPressed;
      Key := 0;
    end;
  end;
end;

procedure TcxCustomGroupBox.DoEditKeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited DoEditKeyUp(Key, Shift);
  case Key of
    VK_SPACE:
      if ViewInfo.CheckBoxVisible then
      begin
        if CheckBoxViewState = ecsPressed then
        begin
          CheckBoxViewState := ecsNormal;
          Toggle;
          Key := 0;
        end;
      end;
  end;
end;

procedure TcxCustomGroupBox.UpdateCheckBoxState(Shift: TShiftState; X, Y: Integer);
begin
  if not Enabled then
    CheckBoxViewState := ecsDisabled
  else
    if not IsDesigning and ViewInfo.PointInCheckBox(X, Y) and (ViewInfo.FCheckPressed or not (ssLeft in Shift)) then
    begin
      if ViewInfo.FCheckPressed then
        CheckBoxViewState := ecsPressed
      else
        CheckBoxViewState := ecsHot
    end
    else
       CheckBoxViewState := ecsNormal;
end;

procedure TcxCustomGroupBox.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if GetCheckBoxVisible then
  begin
    if (Button = mbLeft) and not IsDesigning and ViewInfo.PointInCheckBox(X, Y) then
      ViewInfo.FCheckPressed := True;
    UpdateCheckBoxState(Shift, X, Y);
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TcxCustomGroupBox.MouseLeave(AControl: TControl);
begin
  if GetCheckBoxVisible then
    UpdateCheckBoxState([], -1, -1);
  inherited MouseLeave(AControl);
end;

procedure TcxCustomGroupBox.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if GetCheckBoxVisible then
    UpdateCheckBoxState(Shift, X, Y);
  inherited MouseMove(Shift, X, Y);
end;

procedure TcxCustomGroupBox.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if GetCheckBoxVisible then
  begin
    UpdateCheckBoxState(Shift, X, Y);
    if (Button = mbLeft) and ViewInfo.PointInCheckBox(X, Y) and ViewInfo.FCheckPressed then
      Toggle;
    if Button = mbLeft then
      ViewInfo.FCheckPressed := False;
  end;
  inherited MouseUp(Button, Shift, X, Y);
end;

function TcxCustomGroupBox.IsContainerClass: Boolean;
begin
  Result := True;
end;

function TcxCustomGroupBox.IsNativeBackground: Boolean;
begin
  Result := IsNativeStyle and ParentBackground and not IsInplace and
    not Transparent;
end;

function TcxCustomGroupBox.IsPanelStyle: Boolean;
begin
  Result := PanelStyle.Active;
end;

procedure TcxCustomGroupBox.Paint;
begin
  if not DoCustomDraw then
    inherited Paint;
end;

procedure TcxCustomGroupBox.PaintNonClientArea(ACanvas: TcxCanvas);
var
  ASavedOrg: TPoint;
  R, R1: TRect;
begin
  ASavedOrg := ACanvas.WindowOrg;
  try
    R := ViewInfo.CaptionRectIndent;
    if UseRightToLeftAlignment then
      R := TdxRightToLeftLayoutConverter.ConvertOffsets(R);
    R1 := ViewInfo.CaptionRect;
    case ViewInfo.GetBiDiModeCaptionPosition of
      cxgpLeft:
        R1.Right := R1.Left + R.Left;
      cxgpRight:
        R1.Left := R1.Right - R.Right;
      cxgpTop:
        R1.Bottom := R1.Top + R.Top;
      cxgpBottom:
        R1.Top := R1.Bottom - R.Bottom;
    end;
    MoveWindowOrg(ACanvas.Handle, R.Left, R.Top);
    ACanvas.IntersectClipRect(R1);
    ViewInfo.InternalPaint(ACanvas);
  finally
    ACanvas.WindowOrg := ASavedOrg;
  end;
end;

function TcxCustomGroupBox.PtInCaller(const P: TPoint): Boolean;
begin
  Result := WindowFromPoint(ClientToScreen(P)) = Handle;
end;

procedure TcxCustomGroupBox.TextChanged;
begin
  inherited TextChanged;
  ShortRefreshContainer(False);
end;

function TcxCustomGroupBox.HasShadow: Boolean;
begin
  Result := inherited HasShadow;
  if Result then
  begin
    if not IsPanelStyle then
      Result := Alignment in [alLeftTop, alLeftCenter, alLeftBottom, alTopLeft, alTopCenter, alTopRight];
    Result := Result and not (ViewInfo.NativeStyle or IsSkinAvailable);
  end;
end;

procedure TcxCustomGroupBox.AdjustCanvasFontSettings(ACanvas: TcxCanvas);

  function GetCaptionTextColorByPainter(out ATextColor: TColor): Boolean;
  var
    APainter: TcxCustomLookAndFeelPainter;
  begin
    ATextColor := clDefault;
    if not IsStyleAssigned(csvTextColor) then
    begin
      if IsNativeStyle then
        APainter := cxLookAndFeelPaintersManager.GetPainter(lfsNative)
      else
        APainter := ViewInfo.Painter;

      if APainter <> nil then
      begin
        if IsPanelStyle then
          ATextColor := APainter.PanelTextColor
        else
          ATextColor := APainter.GroupBoxTextColor(Enabled, ViewInfo.CaptionPosition);
      end;
    end;
    Result := ATextColor <> clDefault;
  end;

  procedure AdjustCaptionFont(AFont: TFont);
  var
    ATextColor: TColor;
  begin
    if GetCaptionTextColorByPainter(ATextColor) then
      AFont.Color := ATextColor;
    if not (IsPanelStyle or IsStyleAssigned(csvFont)) then
    begin
      if ViewInfo.Painter <> nil then
        ViewInfo.Painter.GroupBoxAdjustCaptionFont(AFont, ViewInfo.CaptionPosition);
    end;
  end;

begin
  ACanvas.Font.Assign(FCaptionFont);
  AdjustCaptionFont(ACanvas.Font);
end;

procedure TcxCustomGroupBox.CalculateCaptionFont;
begin
  FCaptionFont.Assign(VisibleFont);
end;

procedure TcxCustomGroupBox.RedrawNonClientArea;
begin
  if HandleAllocated then
    cxRedrawWindow(Handle, RDW_FRAME or RDW_INVALIDATE);
end;

function TcxCustomGroupBox.DoCustomDraw: Boolean;
begin
  Result := False;
  if Assigned(FOnCustomDraw) then
    FOnCustomDraw(Self, Result);
end;

function TcxCustomGroupBox.DoCustomDrawCaption(ACanvas: TcxCanvas;
  const ABounds: TRect; APainter: TcxCustomLookAndFeelPainter): Boolean;
begin
  Result := False;
  if Assigned(FOnCustomDrawCaption) then
    FOnCustomDrawCaption(Self, ACanvas.Canvas, ABounds, APainter, Result);
end;

function TcxCustomGroupBox.DoCustomDrawContentBackground(ACanvas: TcxCanvas;
  const ABounds: TRect; APainter: TcxCustomLookAndFeelPainter): Boolean;
begin
  Result := False;
  if Assigned(FOnCustomDrawContentBackground) then
    FOnCustomDrawContentBackground(Self, ACanvas.Canvas, ABounds, APainter, Result);
end;

procedure TcxCustomGroupBox.DoMeasureCaptionHeight(
  APainter: TcxCustomLookAndFeelPainter; var ACaptionHeight: Integer);
begin
  if Assigned(FOnMeasureCaptionHeight) then
    FOnMeasureCaptionHeight(Self, APainter, ACaptionHeight);
end;

procedure TcxCustomGroupBox.Resize;
begin
  inherited Resize;
  if HasNonClientArea then
    RedrawNonClientArea;
end;

function TcxCustomGroupBox.GetCaptionDrawingFlags: Cardinal;
const
  DrawTextFlagsMap: array[Boolean] of Cardinal = (DT_SINGLELINE, DT_WORDBREAK);
begin
  if IsPanelStyle then
    Result := DrawTextFlagsMap[PanelStyle.WordWrap] or GetPanelStyleCaptionDrawingFlags
  else
    Result := DT_SINGLELINE;
  if UseRightToLeftReading then
    Result := Result or DT_RTLREADING;
end;

function TcxCustomGroupBox.GetShadowBounds: TRect;
begin
  if IsPanelStyle then
    Result := ViewInfo.GetBoundsForPanel
  else
    Result := inherited GetShadowBounds;

  case Alignment of
    alTopLeft, alTopCenter, alTopRight:
      Result.Top := 0;
    alBottomLeft, alBottomCenter, alBottomRight:
      Result.Bottom := Height;
    alLeftTop, alLeftCenter, alLeftBottom:
      Result.Left := 0;
    alRightTop, alRightCenter, alRightBottom:
      Result.Right := Width;
  end;
end;

function TcxCustomGroupBox.GetShadowBoundsExtent: TRect;
begin
  Result := inherited GetShadowBoundsExtent;
  case Alignment of
    alTopLeft, alTopCenter, alTopRight:
      Result.Top := ViewInfo.GetFrameBounds.Top - GetShadowBounds.Top;
    alLeftTop, alLeftCenter, alLeftBottom:
      Result.Left := ViewInfo.GetFrameBounds.Left - GetShadowBounds.Left;
  end;
end;

function TcxCustomGroupBox.HasNonClientArea: Boolean;
begin
  Result := not IsPanelStyle and IsNonClientAreaSupported and
    IsSkinAvailable and (Caption <> '') and (Alignment <> alCenterCenter);
end;

function TcxCustomGroupBox.IsVerticalText: Boolean;
begin
  Result := (FAlignment in [alLeftTop, alLeftCenter, alLeftBottom, alRightTop,
    alRightCenter, alRightBottom]) and not IsPanelStyle;
end;

function TcxCustomGroupBox.NeedRedrawOnResize: Boolean;
begin
  Result := RedrawOnResize and (IsNativeStyle or IsSkinAvailable or cxIsVCLThemesEnabled);
end;

{ TcxCustomButtonGroup }

destructor TcxCustomButtonGroup.Destroy;
begin
  SetButtonCount(0);
  FreeAndNil(FButtons);
  inherited Destroy;
end;

procedure TcxCustomButtonGroup.ActivateByMouse(Shift: TShiftState; X, Y: Integer;
  var AEditData: TcxCustomEditData);
var
  P: TPoint;
  AButtonIndex: Integer;
begin
  Activate(AEditData, CanFocusOnClick(X, Y));
  P := Parent.ClientToScreen(Point(X, Y));
  P := ScreenToClient(P);
  AButtonIndex := GetButtonIndexAt(P);
  if AButtonIndex <> -1 then
  begin
    with ViewInfo.ButtonsInfo[AButtonIndex].Bounds do
    begin
      P.X := (Right - Left) div 2;
      P.Y := (Bottom - Top) div 2;
    end;
    if ssLeft in Shift then
    begin
      SendMouseEvent(TWinControl(FButtons[AButtonIndex]), WM_MOUSEMOVE, [], P);
      SendMouseEvent(TWinControl(FButtons[AButtonIndex]), WM_LBUTTONDOWN, Shift, P);
    end
    else
      SendMouseEvent(TWinControl(FButtons[AButtonIndex]), WM_LBUTTONUP, Shift, P);
  end;
end;

function TcxCustomButtonGroup.Focused: Boolean;
var
  I: Integer;
begin
  Result := inherited Focused;
  if not Result and not FIsCreating then
    for I := 0 to FButtons.Count - 1 do
      if TWinControl(FButtons[I]).Focused then
      begin
        Result := True;
        Break;
      end;
end;

class function TcxCustomButtonGroup.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxCustomButtonGroupProperties;
end;

procedure TcxCustomButtonGroup.GetTabOrderList(List: TList);
begin
  if IsInplace and Visible then
    List.Remove(Parent);
end;

function TcxCustomButtonGroup.IsButtonNativeStyle: Boolean;
begin
  Result := TcxButtonGroupViewDataClass(Properties.GetViewDataClass).IsButtonNativeStyle(Style.LookAndFeel);
end;

procedure TcxCustomButtonGroup.PropertiesChanged(Sender: TObject);
begin
  UpdateButtons;
  SynchronizeDisplayValue;
  inherited PropertiesChanged(Sender);
end;

function TcxCustomButtonGroup.CanAutoSize: Boolean;
begin
  Result := not IsInplace and AutoSize;
end;

procedure TcxCustomButtonGroup.ContainerStyleChanged(Sender: TObject);
begin
  inherited ContainerStyleChanged(Sender);
  UpdateButtons;
end;

procedure TcxCustomButtonGroup.CursorChanged;
begin
  UpdateButtons;
end;

procedure TcxCustomButtonGroup.DoEditKeyDown(var Key: Word; Shift: TShiftState);

  function SetButtonFocus(AIndex: Integer): Boolean;
  begin
    Result := (TObject(FButtons[AIndex]) as TWinControl).Enabled;
    if Result then
      TWinControl(FButtons[AIndex]).SetFocus;
  end;

var
  AButtonsInColumn, AButtonsPerColumn: Integer;
  AFocusedButtonIndex: Integer;
  AColumn, ARow: Integer;
begin
  AFocusedButtonIndex := GetFocusedButtonIndex;
  if AFocusedButtonIndex = -1 then
    Exit;
  AButtonsPerColumn := ActiveProperties.GetButtonsPerColumn;
  AButtonsInColumn := AButtonsPerColumn;
  with TcxGroupBoxButtonViewInfo(ViewInfo.ButtonsInfo[AFocusedButtonIndex]) do
  begin
    AColumn := Column;
    ARow := Row;
  end;
  if AFocusedButtonIndex - ARow + AButtonsInColumn - 1 >= ActiveProperties.Items.Count then
    AButtonsInColumn := ActiveProperties.Items.Count - (AFocusedButtonIndex - ARow);
  case Key of
    VK_DOWN:
      if (ARow < AButtonsInColumn - 1) and SetButtonFocus(AFocusedButtonIndex + 1) then
        Key := 0;
    VK_LEFT:
      if (AColumn > 0) and SetButtonFocus(AFocusedButtonIndex - AButtonsPerColumn) then
        Key := 0;
    VK_RIGHT:
      if (AFocusedButtonIndex + AButtonsPerColumn < FButtons.Count) and SetButtonFocus(AFocusedButtonIndex + AButtonsPerColumn) then
        Key := 0;
    VK_UP:
      if (ARow > 0) and SetButtonFocus(AFocusedButtonIndex - 1) then
        Key := 0;
  end;
  inherited DoEditKeyDown(Key, Shift);
end;

procedure TcxCustomButtonGroup.EnabledChanged;
begin
  inherited EnabledChanged;
  UpdateButtons;
end;

procedure TcxCustomButtonGroup.Initialize;
begin
  inherited Initialize;
  FButtons := TList.Create;
  AutoSize := False;
  TabStop := True;
end;

function TcxCustomButtonGroup.IsButtonDC(ADC: THandle): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to InternalButtons.Count - 1 do
    if GetButtonDC(I) = ADC then
    begin
      Result := True;
      Break;
    end;
end;

function TcxCustomButtonGroup.IsContainerClass: Boolean;
begin
  Result := FIsAccelCharHandling;
end;

function TcxCustomButtonGroup.DoRefreshContainer(const P: TPoint;
  Button: TcxMouseButton; Shift: TShiftState; AIsMouseEvent: Boolean): Boolean;
begin
  Result := inherited DoRefreshContainer(P, Button, Shift, AIsMouseEvent);
  ArrangeButtons;
end;

procedure TcxCustomButtonGroup.CreateHandle;
begin
  inherited CreateHandle;
  UpdateButtons;
  SynchronizeDisplayValue;
end;

procedure TcxCustomButtonGroup.ArrangeButtons;
var
  AButtonViewInfo: TcxGroupBoxButtonViewInfo;
  I: Integer;
  R: TRect;
begin
  for I := 0 to FButtons.Count - 1 do
    with TWinControl(FButtons[I]) do
    begin
      AButtonViewInfo := TcxGroupBoxButtonViewInfo(ViewInfo.ButtonsInfo[I]);
      R := AButtonViewInfo.Bounds;
      SetBounds(R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top);
    end;
end;

function TcxCustomButtonGroup.GetButtonIndexAt(const P: TPoint): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to ActiveProperties.Items.Count - 1 do
    if PtInRect(ViewInfo.ButtonsInfo[I].Bounds, P) then
    begin
      Result := I;
      Break;
    end;
end;

function TcxCustomButtonGroup.GetFocusedButtonIndex: Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to ActiveProperties.Items.Count - 1 do
    if TWinControl(FButtons[I]).Focused then
    begin
      Result := I;
      Break;
    end;
end;

procedure TcxCustomButtonGroup.InitButtonInstance(AButton: TWinControl);
begin
  TControlAccess(AButton).ParentShowHint := False;
  AButton.Parent := Self;

  TControlAccess(AButton).OnDragDrop := DoButtonDragDrop;
  TControlAccess(AButton).OnDragOver := DoButtonDragOver;
  TWinControlAccess(AButton).OnKeyDown := DoButtonKeyDown;
  TWinControlAccess(AButton).OnKeyPress := DoButtonKeyPress;
  TWinControlAccess(AButton).OnKeyUp := DoButtonKeyUp;
  TControlAccess(AButton).OnMouseDown := DoButtonMouseDown;
  TControlAccess(AButton).OnMouseMove := DoButtonMouseMove;
  TControlAccess(AButton).OnMouseUp := DoButtonMouseUp;
  TControlAccess(AButton).OnMouseWheel := DoButtonMouseWheel;
end;

procedure TcxCustomButtonGroup.InternalUpdateButtons;
var
  AButton: TWinControl;
  I: Integer;
begin
  SetButtonCount(ActiveProperties.Items.Count);
  ShortRefreshContainer(False);
  for I := 0 to FButtons.Count - 1 do
  begin
    AButton := TWinControl(FButtons[I]);
    AButton.Enabled := Enabled and ActiveProperties.Items[I].Enabled;
  end;
  SynchronizeButtonsStyle;
  for I := 0 to FButtons.Count - 1 do
  begin
    AButton := TWinControl(FButtons[I]);
    AButton.Cursor := Cursor;
  end;
end;

procedure TcxCustomButtonGroup.SetButtonCount(Value: Integer);
begin
  with ActiveProperties.Items do
    if ItemChanged then
    begin
      if ChangedItemOperation = copAdd then
        InitButtonInstance(GetButtonInstance)
      else
        if ChangedItemOperation = copDelete then
          TWinControl(FButtons[ChangedItemIndex]).Free;
    end
    else
      if Value <> FButtons.Count then
      begin
        DisableAlign;
        try
          if Value < FButtons.Count then
            while FButtons.Count > Value do
              TWinControl(FButtons.Last).Free
          else
            while FButtons.Count < Value do
              InitButtonInstance(GetButtonInstance);
        finally
          EnableAlign;
        end;
      end;
end;

function TcxCustomButtonGroup.IsNonClientAreaSupported: Boolean;
begin
  Result := False;
end;

procedure TcxCustomButtonGroup.SynchronizeButtonsStyle;
var
  AButton: TWinControlAccess;
//  ATempFont: TFont;
  I: Integer;
begin
{B38193
  ATempFont := TFont.Create;
  try
    for I := 0 to FButtons.Count - 1 do
    begin
      AButton := TWinControlAccess(FButtons[I]);
      AButton.Color := GetBackgroundColor;
      ATempFont.Assign(Style.GetVisibleFont);
      ATempFont.Color := VisibleFont.Color;
      AssignFonts(AButton.Font, ATempFont);
    end;
  finally
    ATempFont.Free;
  end;
}
  for I := 0 to FButtons.Count - 1 do
  begin
    AButton := TWinControlAccess(FButtons[I]);
    AButton.Color := GetBackgroundColor;
  end;
end;

procedure TcxCustomButtonGroup.UpdateButtons;
begin
  if FIsButtonsUpdating or IsLoading or FIsCreating then
    Exit;
  FIsButtonsUpdating := True;
  InternalUpdateButtons;
  FIsButtonsUpdating := False;
end;

procedure TcxCustomButtonGroup.DoButtonDragDrop(Sender, Source: TObject; X, Y: Integer);
begin
  with TWinControl(Sender) do
    Self.DragDrop(Source, Left + X, Top + Y);
end;

procedure TcxCustomButtonGroup.DoButtonDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  with TWinControl(Sender) do
    Self.DragOver(Source, Left + X, Top + Y, State, Accept);
end;

procedure TcxCustomButtonGroup.DoButtonKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  KeyDown(Key, Shift);
end;

procedure TcxCustomButtonGroup.DoButtonKeyPress(Sender: TObject; var Key: Char);
begin
  KeyPress(Key);
end;

procedure TcxCustomButtonGroup.DoButtonKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  KeyUp(Key, Shift);
end;

procedure TcxCustomButtonGroup.DoButtonMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  InnerControlMouseDown := True;
  try
    with TWinControl(Sender) do
      Self.MouseDown(Button, Shift, X + Left, Y + Top);
  finally
    InnerControlMouseDown := False;
  end;
end;

procedure TcxCustomButtonGroup.DoButtonMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  with TWinControl(Sender) do
    Self.MouseMove(Shift, X + Left, Y + Top);
end;

procedure TcxCustomButtonGroup.DoButtonMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  with TWinControl(Sender) do
    Self.MouseUp(Button, Shift, X + Left, Y + Top);
end;

procedure TcxCustomButtonGroup.DoButtonMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer;  MousePos: TPoint; var Handled: Boolean);
begin
  Handled := False;
end;

function TcxCustomButtonGroup.GetProperties: TcxCustomButtonGroupProperties;
begin
  Result := TcxCustomButtonGroupProperties(inherited Properties);
end;

function TcxCustomButtonGroup.GetActiveProperties: TcxCustomButtonGroupProperties;
begin
  Result := TcxCustomButtonGroupProperties(InternalGetActiveProperties);
end;

procedure TcxCustomButtonGroup.SetProperties(Value: TcxCustomButtonGroupProperties);
begin
  Properties.Assign(Value);
end;

end.
