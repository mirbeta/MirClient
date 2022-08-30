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

unit cxButtons;

{$I cxVer.inc}

interface

uses
  Windows, Messages, Types, Classes, Controls, Graphics, StdCtrls, Forms, Menus,
  ImgList, Buttons, CommCtrl,
  dxCore, dxMessages, dxThemeManager, cxGeometry, cxGraphics, cxLookAndFeels, dxGDIPlusClasses,
  cxLookAndFeelPainters, cxControls, cxContainer, cxClasses, dxFading, dxCoreGraphics;

type
  TcxButtonColors = class;
  TcxButtonGlyph = class;
  TcxButtonImageOptions = class;
  TcxButtonViewInfo = class;
  TcxCustomButton = class;
  TcxSpeedButtonOptions = class;

  TcxButtonKind = (cxbkStandard, cxbkDropDown, cxbkDropDownButton, cxbkCommandLink, cxbkOfficeDropDown);
  TcxButtonPaintStyle = (bpsDefault, bpsCaption, bpsGlyph);

  TcxButtonAssignedColor = (cxbcDefault, cxbcDefaultText, cxbcDisabled, cxbcDisabledText,
    cxbcHot, cxbcHotText, cxbcNormal, cxbcNormalText, cxbcPressed, cxbcPressedText);
  TcxButtonAssignedColors = set of TcxButtonAssignedColor;

  TcxButtonGetDrawParamsEvent = procedure(Sender: TcxCustomButton;
    AState: TcxButtonState; var AColor: TColor; AFont: TFont) of object;

  TcxButtonInternalState = (bisDown, bisHot, bisPressed, bisMenuButtonPressed, bisMenuVisible, bisAccelExecute);
  TcxButtonInternalStates = set of TcxButtonInternalState;

  { TcxButtonCustomPersistent }

  TcxButtonCustomPersistent = class(TPersistent)
  private
    FButton: TcxCustomButton;
  protected
    function GetOwner: TPersistent; override;
    //
    property Button: TcxCustomButton read FButton;
  public
    constructor Create(AOwner: TcxCustomButton); virtual;
  end;

  { TcxButtonColors }

  TcxButtonColors = class(TcxButtonCustomPersistent)
  private
    FAssignedColors: TcxButtonAssignedColors;
    FColors: array[TcxButtonAssignedColor] of TColor;
    function GetColor(const Index: Integer): TColor;
    function IsColorStored(const Index: Integer): Boolean;
    procedure SetAssignedColors(Value: TcxButtonAssignedColors);
    procedure SetColor(const Index: Integer; const Value: TColor);
    function ButtonStateToButtonAssignedColor(AState: TcxButtonState; AIsTextColor: Boolean): TcxButtonAssignedColor;
  protected
    function GetColorByState(const AState: TcxButtonState): TColor;
    function GetTextColorByState(const AState: TcxButtonState): TColor;
  public
    constructor Create(AOwner: TcxCustomButton); override;
    procedure Assign(Source: TPersistent); override;
  published
    property AssignedColors: TcxButtonAssignedColors read FAssignedColors write SetAssignedColors stored False;
    property Default: TColor index Ord(cxbcDefault) read GetColor write SetColor stored IsColorStored;
    property DefaultText: TColor index Ord(cxbcDefaultText) read GetColor write SetColor stored IsColorStored;
    property Normal: TColor index Ord(cxbcNormal) read GetColor write SetColor stored IsColorStored;
    property NormalText: TColor index Ord(cxbcNormalText) read GetColor write SetColor stored IsColorStored;
    property Hot: TColor index Ord(cxbcHot) read GetColor write SetColor stored IsColorStored;
    property HotText: TColor index Ord(cxbcHotText) read GetColor write SetColor stored IsColorStored;
    property Pressed: TColor index Ord(cxbcPressed) read GetColor write SetColor stored IsColorStored;
    property PressedText: TColor index Ord(cxbcPressedText) read GetColor write SetColor stored IsColorStored;
    property Disabled: TColor index Ord(cxbcDisabled) read GetColor write SetColor stored IsColorStored;
    property DisabledText: TColor index Ord(cxbcDisabledText) read GetColor write SetColor stored IsColorStored;
  end;

  { TcxButtonGlyph }

  TcxButtonGlyphClass = class of TcxButtonGlyph;
  TcxButtonGlyph = class(TcxButtonCustomPersistent)
  private
    FGlyph: TdxSmartGlyph;
    FImageChangeLink: TChangeLink;
    FImageIndex: Integer;
    FImages: TCustomImageList;
    FNumGlyphs: TNumGlyphs;
    FOnChange: TNotifyEvent;

    procedure Changed(Sender: TObject);
    procedure GlyphChanged;
    procedure ImageListChange(Sender: TObject);
    procedure SetGlyph(Value: TdxSmartGlyph);
    procedure SetImages(Value: TCustomImageList);
    procedure SetImageIndex(Value: Integer);
    procedure SetNumGlyphs(Value: TNumGlyphs);
    function GetImageSize: TSize;
    function GetTransparentColor: TColor;
  protected
    procedure FreeNotification(Sender: TComponent); virtual;
  public
    constructor Create(AOwner: TcxCustomButton); override;
    destructor Destroy; override;
    function IsImageAssigned: Boolean;

    property Glyph: TdxSmartGlyph read FGlyph write SetGlyph;
    property ImageIndex: Integer read FImageIndex write SetImageIndex;
    property Images: TCustomImageList read FImages write SetImages;
    property ImageSize: TSize read GetImageSize;  // obsolete (call from ViewInfo)

    property NumGlyphs: TNumGlyphs read FNumGlyphs write SetNumGlyphs;
    property TransparentColor: TColor read GetTransparentColor;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;
  { TcxButtonActionLink }

  TcxButtonActionLink = class(TButtonActionLink)
  private
    function GetClient: TcxCustomButton;
    property Client: TcxCustomButton read GetClient;
  protected
    function IsImageIndexLinked: Boolean; override;
    procedure SetChecked(Value: Boolean); override;
    procedure SetGroupIndex(Value: Integer); override;
    procedure SetImageIndex(Value: Integer); override;
  public
    destructor Destroy; override;
  end;

  { TcxSpeedButtonOptions }

  TcxSpeedButtonOptions = class(TcxButtonCustomPersistent)
  strict private
    FAllowAllUp: Boolean;
    FCanBeFocused: Boolean;
    FGroupIndex: Integer;
    FFlat: Boolean;
    FLockCount: Integer;
    FTransparent: Boolean;

    function GetActive: Boolean;
    function GetDown: Boolean;
    function IsDownStored: Boolean;
    function IsGroupIndexStored: Boolean;
    procedure SetAllowAllUp(AValue: Boolean);
    procedure SetCanBeFocused(AValue: Boolean);
    procedure SetDown(AValue: Boolean);
    procedure SetFlat(AValue: Boolean);
    procedure SetGroupIndex(AValue: Integer);
    procedure SetTransparent(AValue: Boolean);
    procedure UpdateGroup;
  protected
    procedure InternalSetDown(AValue: Boolean);
    procedure UpdateGroupValues(const AGroupIndex: Integer; const ASpeedButtonOptions: TcxSpeedButtonOptions);
    function NeedDrawParts: Boolean;
    //
    property LockCount: Integer read FLockCount;
  public
    constructor Create(AOwner: TcxCustomButton); override;
    procedure Assign(Source: TPersistent); override;

    procedure BeginUpdate;
    procedure CancelUpdate;
    procedure EndUpdate;

    property Active: Boolean read GetActive;
  published
    property GroupIndex: Integer read FGroupIndex write SetGroupIndex stored IsGroupIndexStored default 0;
    property CanBeFocused: Boolean read FCanBeFocused write SetCanBeFocused default True;
    property AllowAllUp: Boolean read FAllowAllUp write SetAllowAllUp default False;
    property Down: Boolean read GetDown write SetDown stored IsDownStored default False;
    property Flat: Boolean read FFlat write SetFlat default False;
    property Transparent: Boolean read FTransparent write SetTransparent default False;
  end;

  { TcxButtonImageOptions }

  TcxButtonImageOptions = class(TcxButtonCustomPersistent)
  private
    FImageIndex: TcxImageIndex;
    FImages: TCustomImageList;
    FLayout: TButtonLayout;
    FMargin: Integer;
    FSpacing: Integer;

    function GetGlyph: TdxSmartGlyph;
    function GetNumGlyphs: TNumGlyphs;
    function IsImageIndexStored: Boolean;
    procedure SetGlyph(Value: TdxSmartGlyph);
    procedure SetImageIndex(Value: TcxImageIndex);
    procedure SetImages(Value: TCustomImageList);
    procedure SetLayout(Value: TButtonLayout);
    procedure SetMargin(Value: Integer);
    procedure SetNumGlyphs(Value: TNumGlyphs);
    procedure SetSpacing(Value: Integer);
  protected
    procedure Changed; virtual;
    procedure FreeNotification(Sender: TComponent); virtual;
  public
    constructor Create(AOwner: TcxCustomButton); override;
    procedure Assign(Source: TPersistent); override;
  published
    property Glyph: TdxSmartGlyph read GetGlyph write SetGlyph;
    property ImageIndex: TcxImageIndex read FImageIndex write SetImageIndex stored IsImageIndexStored default -1;
    property Images: TCustomImageList read FImages write SetImages;
    property Layout: TButtonLayout read FLayout write SetLayout default blGlyphLeft;
    property Margin: Integer read FMargin write SetMargin default -1;
    property NumGlyphs: TNumGlyphs read GetNumGlyphs write SetNumGlyphs default 1;
    property Spacing: Integer read FSpacing write SetSpacing default 4;
  end;

  { TcxButtonPainter }

  TcxButtonPainter = class(TcxButtonCustomPersistent)
  private
    function GetLookAndFeelPainter: TcxCustomLookAndFeelPainter;
    function GetLookAndFeelStyle: TcxLookAndFeelStyle;
    function GetScaleFactor: TdxScaleFactor;
    function GetViewInfo: TcxButtonViewInfo;
  protected
    property ScaleFactor: TdxScaleFactor read GetScaleFactor;
  public
    procedure DrawButton(ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState;
      AColor: TColor; APart: TcxButtonPart); virtual;
    procedure DrawDropDownArrow(ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState); virtual;
    procedure DrawFocusRect(ACanvas: TcxCanvas; const R: TRect); virtual;
    procedure DrawParentBackground(ACanvas: TcxCanvas; const R: TRect); virtual;
    function GetDropDownArrowSize: Integer; virtual;
    function GetDropDownButtonSize: Integer; virtual;
    // Text
    procedure DrawText(ACanvas: TcxCanvas; const AText: TCaption;
      R: TRect; ADrawTextFlags: Integer); overload; virtual;
    procedure DrawText(ACanvas: TcxCanvas; const AText: TCaption;
      R: TRect; AState: TcxButtonState; ADrawTextFlags: Integer); overload; virtual;
    function GetBiDiFlags: Integer;
    function GetDescriptionTextColor(AState: TcxButtonState; ADefaultColor: TColor = clWindowText): TColor; virtual;
    function GetPressedTextShift: TPoint; virtual;
    function GetTextColor(AState: TcxButtonState; ADefaultColor: TColor = clWindowText): TColor; virtual;
    // CommandLink
    procedure DrawCommandLinkButton(ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState; AColor: TColor); virtual;
    function GetCommandLinkGlyphSize: TSize; virtual;
    function GetCommandLinkMargins: TRect; virtual;
    // SpeedButton
    procedure DrawSpeedButton(ACanvas: TcxCanvas; R: TRect; AState: TcxButtonState; AColor: TColor; APart: TcxButtonPart); virtual;
    //
    property LookAndFeelPainter: TcxCustomLookAndFeelPainter read GetLookAndFeelPainter;
    property LookAndFeelStyle: TcxLookAndFeelStyle read GetLookAndFeelStyle;
    property ViewInfo: TcxButtonViewInfo read GetViewInfo;
  end;

  { TcxButtonCustomLayoutCalculator }

  TcxButtonCustomLayoutCalculator = class(TcxButtonCustomPersistent)
  strict private
    function GetOptionsImage: TcxButtonImageOptions;
    function GetPainter: TcxButtonPainter;
    function GetUseRightToLeftAlignment: Boolean;
    function GetViewInfo: TcxButtonViewInfo;
  public
    procedure Calculate; virtual; abstract;
    //
    property OptionsImage: TcxButtonImageOptions read GetOptionsImage;
    property Painter: TcxButtonPainter read GetPainter;
    property UseRightToLeftAlignment: Boolean read GetUseRightToLeftAlignment;
    property ViewInfo: TcxButtonViewInfo read GetViewInfo;
  end;

  { TcxButtonCommandLinkLayoutCalculator }

  TcxButtonCommandLinkLayoutCalculator = class(TcxButtonCustomLayoutCalculator)
  protected
    procedure CalculateLayout(const AContentRect: TRect; AMargin, ASpacing: Integer); virtual;
  public
    procedure Calculate; override;
  end;

  { TcxButtonStandardLayoutCalculator }

  TcxButtonStandardLayoutCalculator = class(TcxButtonCustomLayoutCalculator)
  strict private
    function GetGlyphPos: TPoint;
    function GetGlyphSize: TSize;
    function GetLayout: TButtonLayout;
    function GetScaleFactor: TdxScaleFactor;
    procedure SetGlyphPos(const Value: TPoint);
  protected
    FMargin: Integer;
    FSpacing: Integer;
    FTextAreaSize: TSize;

    procedure CalculateDropDownArrowRect; virtual;
    procedure CalculateGlyphPosition(const R: TRect); virtual;
    procedure CalculateLayout(R: TRect); virtual;
    procedure CalculateMargins(const R: TRect; var AMargin, ASpacing: Integer); virtual;
    function CalculateTextAreaBounds(const R: TRect): TRect; virtual;
    procedure CalculateTextAreaLayout(const R: TRect); virtual;
    procedure CalculateTextAreaSize(const R: TRect); virtual;
    function GetTextOffsets: TRect; virtual;

    property ScaleFactor: TdxScaleFactor read GetScaleFactor;
  public
    procedure Calculate; override;
    //
    property GlyphPos: TPoint read GetGlyphPos write SetGlyphPos;
    property GlyphSize: TSize read GetGlyphSize;
    property Layout: TButtonLayout read GetLayout;
    property Margin: Integer read FMargin;
    property Spacing: Integer read FSpacing;
    property TextAreaSize: TSize read FTextAreaSize;
  end;

  { TcxButtonOfficeDropDownLayoutCalculator }

  TcxButtonOfficeDropDownLayoutCalculator = class(TcxButtonStandardLayoutCalculator)
  strict private
    function GetScaleFactor: TdxScaleFactor;
  protected
    FCaptionSize: TSize;
    FDescriptionSize: TSize;

    procedure CalculateDropDownArrowRect; override;
    procedure CalculateLayout(R: TRect); override;
    procedure CalculateTextAreaLayout(const R: TRect); override;
    procedure CalculateTextAreaSize(const R: TRect); override;
    function GetTextOffsets: TRect; override;

    property ScaleFactor: TdxScaleFactor read GetScaleFactor;
  public
    property CaptionSize: TSize read FCaptionSize;
    property DescriptionSize: TSize read FDescriptionSize;
  end;

  { TcxButtonViewInfo }

  TcxButtonViewInfo = class(TcxButtonCustomPersistent)
  strict private
    FColor: TColor;
    FDescriptionFont: TFont;
    FFont: TFont;
    FLayout: TButtonLayout;
    FPainter: TcxButtonPainter;
    function GetButtonGlyph: TcxButtonGlyph;
    function GetCaption: string;
    function GetColorPalette: IdxColorPalette;
    function GetDescription: string;
    function GetDropDownButtonState: TcxButtonState;
    function InternalGetImageSize: TSize;
    function GetIsDrawOnGlass: Boolean;
    function GetKind: TcxButtonKind;
    function GetOptionsImage: TcxButtonImageOptions;
    function GetScaleFactor: TdxScaleFactor;
    function GetSpeedButtonOptions: TcxSpeedButtonOptions;
  protected
    FBounds: TRect;
    FButtonRect: TRect;
    FCaptionRect: TRect;
    FDescriptionRect: TRect;
    FDropDownArrowRect: TRect;
    FDropDownButtonRect: TRect;
    FGlyphPos: TPoint;
    FState: TcxButtonState;

    // Calculation
    procedure CalculateButtonParts; virtual;
    procedure CalculateContentLayout; virtual;

    function CanDrawCaption: Boolean; virtual;
    function CanDrawGlyph: Boolean; virtual;
    function CreateLayoutCalculator(AKind: TcxButtonKind): TcxButtonCustomLayoutCalculator; virtual;
    function CreatePainter: TcxButtonPainter; virtual;

    procedure CheckLayout(var ALayout: TButtonLayout); virtual;
    procedure DrawContent(ACanvas: TcxCanvas); virtual;
    procedure InitializeFont; virtual;
    function GetCaptionDrawTextFlags: Integer; virtual;
    function GetDescriptionDrawTextFlags: Integer; virtual;
    function GetImageSize(AScaleFactor: TdxScaleFactor): TSize;

    property OptionsImage: TcxButtonImageOptions read GetOptionsImage;
    property ScaleFactor: TdxScaleFactor read GetScaleFactor;
    property SpeedButtonOptions: TcxSpeedButtonOptions read GetSpeedButtonOptions;
  public
    constructor Create(AOwner: TcxCustomButton); override;
    destructor Destroy; override;

    procedure Calculate(const ABounds: TRect); virtual;
    procedure Draw(ACanvas: TcxCanvas); virtual;
    procedure DrawBackground(ACanvas: TcxCanvas); virtual;
    procedure DrawGlyph(ACanvas: TcxCanvas); virtual;
    function GetContentRect(AState: TcxButtonState = cxbsDefault): TRect; virtual;

    function HasDropDownButton: Boolean;
    function NeedDrawDropDownArrow: Boolean;
    function NeedDrawFocusRect: Boolean;

    property Bounds: TRect read FBounds;
    property ButtonGlyph: TcxButtonGlyph read GetButtonGlyph;
    property ButtonRect: TRect read FButtonRect;
    property Caption: string read GetCaption;
    property CaptionDrawTextFlags: Integer read GetCaptionDrawTextFlags;
    property CaptionRect: TRect read FCaptionRect;
    property Color: TColor read FColor;
    property Description: string read GetDescription;
    property DescriptionDrawTextFlags: Integer read GetDescriptionDrawTextFlags;
    property DescriptionFont: TFont read FDescriptionFont;
    property DescriptionRect: TRect read FDescriptionRect;
    property DropDownArrowRect: TRect read FDropDownArrowRect;
    property DropDownButtonRect: TRect read FDropDownButtonRect;
    property DropDownButtonState: TcxButtonState read GetDropDownButtonState;
    property Font: TFont read FFont;
    property GlyphPos: TPoint read FGlyphPos;
    property ImageSize: TSize read InternalGetImageSize;
    property IsDrawOnGlass: Boolean read GetIsDrawOnGlass;
    property Kind: TcxButtonKind read GetKind;
    property Layout: TButtonLayout read FLayout;
    property Painter: TcxButtonPainter read FPainter;
    property State: TcxButtonState read FState;
  end;

  { TcxCustomButton }

  TcxButtonCustomDrawEvent = procedure (Sender: TObject; ACanvas: TcxCanvas;
    AViewInfo: TcxButtonViewInfo; var AHandled: Boolean) of object;

  TcxButtonDropDownMenuPopupEvent = procedure (Sender: TObject;
    var APopupMenu: TPopupMenu; var AHandled: Boolean) of object;
  TcxButtonDropDownMenuPopupExEvent = procedure (Sender: TObject;
    var APopupMenu: TComponent; var AHandled: Boolean) of object;

// for navigation
  TcxBaseButton = TCustomButton;

  { TcxCustomButton }

  TcxCustomButton = class(TcxBaseButton,
    IcxMouseTrackingCaller,
    IdxSkinSupport,
    IcxLookAndFeelContainer,
    IdxFadingObject,
    IdxScaleFactor,
    IdxAdornerTargetElement,
    IdxAdornerRootTargetElement)
  private
    FAutoSize: Boolean;
    FButtonPressTimer: TcxTimer;
    FCanvas: TcxCanvas;
    FColors: TcxButtonColors;
    FControlCanvas: TControlCanvas;
    FDescription: string;
    FDropDownMenu: TComponent;
    FGlyph: TcxButtonGlyph;
    FInternalStates: TcxButtonInternalStates;
    FIsDropDownMenuClosing: Boolean;
    FIsFocused: Boolean;
    FIsPaintDefault: Boolean;
    FIsPaintPressed: Boolean;
    FKind: TcxButtonKind;
    FLookAndFeel: TcxLookAndFeel;
    FOptionsImage: TcxButtonImageOptions;
    FPaintStyle: TcxButtonPaintStyle;
    FPopupAlignment: TPopupAlignment;
    FPopupMenu: TComponent;
    FRepeatClick: Boolean;
    FScaleFactor: TdxScaleFactor;
    FShowFocusRect: Boolean;
    FSpeedButtonOptions: TcxSpeedButtonOptions;
    FUseSystemPaint: Boolean; // deprecated
    FViewInfo: TcxButtonViewInfo;

    // events
    FOnDropDownMenuPopup: TcxButtonDropDownMenuPopupEvent;
    FOnDropDownMenuPopupEx: TcxButtonDropDownMenuPopupExEvent;
    FOnGetDrawParams: TcxButtonGetDrawParamsEvent;

    FOnCustomDraw: TcxButtonCustomDrawEvent;

    procedure DoButtonPressTimer(Sender: TObject);
    procedure Draw(DC: HDC);
    procedure DrawItem(const DrawItemStruct: TDrawItemStruct);
    function GetColors: TcxButtonColors;
    function GetCommandLinkHint: string;
    function GetGlyph: TdxSmartGlyph;
    function GetLayout: TButtonLayout;
    function GetMargin: Integer;
    function GetNumGlyphs: TNumGlyphs;
    function GetScaleFactor: TdxScaleFactor;
    function GetSpacing: Integer;
    procedure SetGlyph(Value: TdxSmartGlyph);
    procedure SetLayout(Value: TButtonLayout);
    procedure SetMargin(Value: Integer);
    procedure SetNumGlyphs(Value: TNumGlyphs);
    procedure SetOptionsImage(Value: TcxButtonImageOptions);
    procedure SetSpacing(Value: Integer);
    procedure UpdateImageInfo;

    procedure BCMSetNote(var Message: TMessage); message BCM_SETNOTE;
    procedure CMBiDiModeChanged(var Message: TMessage); message CM_BIDIMODECHANGED;
    procedure CMButtonPressed(var Message: TMessage); message CM_BUTTONPRESSED;
    procedure CMCloseUpPopupMenu(var Message: TMessage); message DXM_CLOSEUPPOPUPMENU;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CMDropDownPopupMenu(var Message: TMessage); message DXM_DROPDOWNPOPUPMENU;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMHintShow(var Message: TCMHintShow); message CM_HINTSHOW;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;
    procedure CNCtlColorBtn(var Message: TWMCtlColorBtn); message CN_CTLCOLORBTN;
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    procedure CNKeyDown(var Message: TWMKeyDown); message CN_KEYDOWN;
    procedure CNMeasureItem(var Message: TWMMeasureItem); message CN_MEASUREITEM;
    procedure CNSysKeyDown(var Message: TWMSysKeyDown); message CN_SYSKEYDOWN;
    procedure WMCaptureChanged(var Message: TMessage); message WM_CAPTURECHANGED;
    procedure WMContextMenu(var Message: TWMContextMenu); message WM_CONTEXTMENU;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMWindowPosChanged(var Message: TWMWindowPosChanged); message WM_WINDOWPOSCHANGED;

    function CanHotTrack: Boolean;
    procedure CheckStartFading(APrevButtonState, AButtonState: TcxButtonState);
    procedure DoDropDownMenu;
    procedure GlyphChanged(Sender: TObject);
    function IsColorsStored: Boolean;
    procedure LookAndFeelChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues);
    procedure ScaleFactorChangeHandler(Sender: TObject; M, D: Integer; IsLoading: Boolean);
    procedure SetButtonAutoSize(Value: Boolean);
    procedure SetColors(const Value: TcxButtonColors);
    procedure SetCommandLinkHint(const Value: string);
    procedure SetDescription(const Value: string);
    procedure SetKind(const Value: TcxButtonKind);
    procedure SetLookAndFeel(Value: TcxLookAndFeel);
    procedure SetPaintStyle(Value: TcxButtonPaintStyle);
    procedure SetPopupMenu(Value: TComponent);

    // speed button support
    procedure CheckPressedState(const P: TPoint);
    procedure CheckShowMenu(const P: TPoint);
    function GetAllowAllUp: Boolean;
    function GetCanBeFocused: Boolean;
    function GetDown: Boolean;
    function GetGroupIndex: Integer;
    function GetMenuButtonBounds: TRect;
    function GetSpeedButtonMode: Boolean;
    procedure SetAllowAllUp(AValue: Boolean);
    procedure SetCanBeFocused(AValue: Boolean);
    procedure SetDown(AValue: Boolean);
    procedure SetDropDownMenu(AValue: TComponent);
    procedure SetGroupIndex(AValue: Integer);
    procedure SetInternalStates(AValue: TcxButtonInternalStates);
    procedure SetSpeedButtonOptions(AValue: TcxSpeedButtonOptions);
  protected
    // IdxAdornerTargetElement
    function IdxAdornerTargetElement.GetControl = GetAdornerTargetElementControl;
    function GetAdornerTargetElementControl: TWinControl; virtual;
    function IdxAdornerTargetElement.GetBounds = GetAdornerTargetElementBounds;
    function GetAdornerTargetElementBounds: TRect; virtual;
    function IdxAdornerTargetElement.GetVisible = GetAdornerTargetElementVisible;
    function GetAdornerTargetElementVisible: Boolean; virtual;

    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
    function CanResize(var NewWidth, NewHeight: Integer): Boolean; override;
  {$IFDEF DELPHIBERLIN}
    procedure ChangeScale(M, D: Integer; isDpiChange: Boolean); override;
  {$ELSE}
    procedure ChangeScale(M, D: Integer); override;
  {$ENDIF}
    procedure CreateParams(var Params: TCreateParams); override;
    function CreateOptionsImage: TcxButtonImageOptions; virtual;
    function CreateViewInfo: TcxButtonViewInfo; virtual;
    procedure DestroyWindowHandle; override;
    procedure DoContextPopup(MousePos: TPoint; var Handled: Boolean); override;
    procedure DoGetDrawParams(AState: TcxButtonState; var AColor: TColor; AFont: TFont); virtual;
    function DoDropDownMenuPopup(var APopupMenu: TComponent): Boolean; virtual;
    function DoOnDropDownMenuPopup(var APopupMenu: TPopupMenu): Boolean; virtual;
    function DoOnDropDownMenuPopupEx(var APopupMenu: TComponent): Boolean; virtual;
    function DoShowPopupMenu(APopupMenu: TComponent; X, Y: Integer): Boolean; virtual;
    function GetActionLinkClass: TControlActionLinkClass; override;
    function GetAllowSpeedButtonOptions: Boolean; virtual;
    function GetButtonState: TcxButtonState;
    function GetChecked: Boolean; override;
    function GetControlCanvas: TControlCanvas;
    function GetGlyphClass: TcxButtonGlyphClass; virtual;
    function GetPainter: TcxCustomLookAndFeelPainter;
    function GetPalette: HPALETTE; override;
    function IsDesigning: Boolean;
    function StandardButton: Boolean; virtual;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure ScaleFactorChanged(M, D: Integer); virtual;
    procedure SetChecked(Value: Boolean); override;
    procedure SetButtonStyle(ADefault: Boolean); override;
    procedure SetParent(AValue: TWinControl); override;
    procedure UpdateSize;
    procedure WndProc(var Message: TMessage); override;

    //CustomDraw
    function DoCustomDraw(ACanvas: TcxCanvas; AViewInfo: TcxButtonViewInfo): Boolean;

    // Mouse Events
    procedure DblClick; override;
    procedure MouseEnter; dynamic;
    procedure MouseLeave; dynamic;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

    //IcxMouseTrackingCaller
    procedure IcxMouseTrackingCaller.MouseLeave = MouseLeave;

    // IcxLookAndFeelContainer
    function GetLookAndFeel: TcxLookAndFeel;

    // IdxFadingObject
    function CanFade: Boolean; virtual;
    procedure GetFadingImages(out AFadeOutImage, AFadeInImage: TcxBitmap); virtual;
    procedure IdxFadingObject.DrawFadeImage = Invalidate;

    property RepeatClick: Boolean read FRepeatClick write FRepeatClick default False;
    property AllowSpeedButtonOptions: Boolean read GetAllowSpeedButtonOptions;
    property AutoSize: Boolean read FAutoSize write SetButtonAutoSize default False;
    property Colors: TcxButtonColors read GetColors write SetColors stored IsColorsStored;
    property CommandLinkHint: string read GetCommandLinkHint write SetCommandLinkHint; //deprecated, use Description instead
    property Description: string read FDescription write SetDescription;
    property DropDownMenu: TComponent read FDropDownMenu write SetDropDownMenu;
    property InternalStates: TcxButtonInternalStates read FInternalStates write SetInternalStates;
    property IsFocused: Boolean read FIsFocused;
    property Kind: TcxButtonKind read FKind write SetKind default cxbkStandard;
    property OptionsImage: TcxButtonImageOptions read FOptionsImage write SetOptionsImage;
    property PaintStyle: TcxButtonPaintStyle read FPaintStyle write SetPaintStyle default bpsDefault;
    property PopupAlignment: TPopupAlignment read FPopupAlignment write FPopupAlignment default paLeft;
    property ShowFocusRect: Boolean read FShowFocusRect write FShowFocusRect;
    property UseSystemPaint: Boolean read FUseSystemPaint write FUseSystemPaint default False; // deprecated

    // speed button support
    property AllowAllUp: Boolean read GetAllowAllUp write SetAllowAllUp default False;
    property CanBeFocused: Boolean read GetCanBeFocused write SetCanBeFocused default True;
    property Down: Boolean read GetDown write SetDown default False;
    property GroupIndex: Integer read GetGroupIndex write SetGroupIndex default 0;
    property Painter: TcxCustomLookAndFeelPainter read GetPainter;
    property SpeedButtonMode: Boolean read GetSpeedButtonMode;
    property SpeedButtonOptions: TcxSpeedButtonOptions read FSpeedButtonOptions write SetSpeedButtonOptions;

    //CustomDrawEvents
    property OnCustomDraw: TcxButtonCustomDrawEvent read FOnCustomDraw write FOnCustomDraw;

    property OnDropDownMenuPopup: TcxButtonDropDownMenuPopupEvent read FOnDropDownMenuPopup write FOnDropDownMenuPopup;
    property OnDropDownMenuPopupEx: TcxButtonDropDownMenuPopupExEvent read FOnDropDownMenuPopupEx write FOnDropDownMenuPopupEx;
    property OnGetDrawParams: TcxButtonGetDrawParamsEvent read FOnGetDrawParams write FOnGetDrawParams;

    property ViewInfo: TcxButtonViewInfo read FViewInfo;

    //obsolete GlyphOptions
    property Glyph: TdxSmartGlyph read GetGlyph write SetGlyph stored false;
    property Layout: TButtonLayout read GetLayout write SetLayout default blGlyphLeft;
    property Margin: Integer read GetMargin write SetMargin default -1;
    property NumGlyphs: TNumGlyphs read GetNumGlyphs write SetNumGlyphs default 1;
    property ScaleFactor: TdxScaleFactor read FScaleFactor;
    property Spacing: Integer read GetSpacing write SetSpacing default 4;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click; override;
    function CanFocus: Boolean; override;
    procedure DefaultHandler(var Message); override;
    function GetOptimalSize: TSize; virtual;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    function UseRightToLeftAlignment: Boolean; override;

    property LookAndFeel: TcxLookAndFeel read FLookAndFeel write SetLookAndFeel;
    property PopupMenu: TComponent read FPopupMenu write SetPopupMenu;

    property DragCursor;
    property DragKind;
    property DragMode;
    property Font;
    property ParentFont;
    property WordWrap;
  end;

  { TcxButton }

  TcxButton = class(TcxCustomButton)
  published
    //obsolete
    property CommandLinkHint stored False;
    property Glyph stored False;
    property Layout stored False;
    property Margin stored False;
    property NumGlyphs stored False;
    property Spacing stored False;
    property UseSystemPaint;

    property Align;
//    property AutoSize;
    property CanBeFocused stored False;
    property GroupIndex stored False;
    property Down stored False;
    property AllowAllUp stored False;
    property Action;
    property Anchors;
    property BiDiMode;
    property Cancel;
    property Caption;
    property Colors;
    property Constraints;
    property Default;
    property Description;
    property DropDownMenu;
    property Enabled;
    property Kind;
    property LookAndFeel;
    property ModalResult;
    property OptionsImage;
    property PaintStyle;
    property ParentBiDiMode;
    property ParentShowHint;
    property PopupAlignment;
    property PopupMenu;
    property RepeatClick;
    property ShowHint;
    property SpeedButtonOptions;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnDropDownMenuPopup;
    property OnDropDownMenuPopupEx;
    property OnEnter;
    property OnExit;
    property OnGetDrawParams;
    property OnMouseEnter;
    property OnMouseLeave;

    //CustomDrawEvents
    property OnCustomDraw;

    property DragCursor;
    property DragKind;
    property DragMode;
    property Font;
    property ParentFont;
    property WordWrap;
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    property OnMouseActivate;
  end;

function GetButtonPainter(ALookAndFeel: TcxLookAndFeel): TcxCustomLookAndFeelPainter;

implementation

uses
  SysUtils, Consts, Dialogs, Math, ActnList,
  dxUxTheme, cxDWMApi, dxThemeConsts, cxLibraryConsts, dxDPIAwareUtils;

const
  cxBtnStdVertTextOffsetCorrection = -1;
  TextRectCorrection: TRect = (Left: 1; Top: 0; Right: 2; Bottom: 0);

type
  TControlActionLinkAccess = class(TControlActionLink);

function GetButtonPainter(ALookAndFeel: TcxLookAndFeel): TcxCustomLookAndFeelPainter;
begin
  Result := ALookAndFeel.Painter;
  if ALookAndFeel.SkinPainter = nil then
  begin
    if Result.LookAndFeelStyle = lfsOffice11 then
    begin
      if AreVisualStylesAvailable(totButton) then
        Result := cxLookAndFeelPaintersManager.GetPainter(lfsNative)
      else
        Result := cxLookAndFeelPaintersManager.GetPainter(lfsStandard);
    end;
  end;
end;

{ TcxButtonCustomPersistent }

constructor TcxButtonCustomPersistent.Create(AOwner: TcxCustomButton);
begin
  inherited Create;
  FButton := AOwner;
end;

function TcxButtonCustomPersistent.GetOwner: TPersistent;
begin
  Result := Button;
end;

{ TcxButtonColors }

constructor TcxButtonColors.Create(AOwner: TcxCustomButton);
var
  AState: TcxButtonAssignedColor;
begin
  inherited Create(AOwner);
  for AState := Low(AState) to High(AState) do
    FColors[AState] := clDefault;
end;

function TcxButtonColors.GetColor(const Index: Integer): TColor;
begin
  Result := FColors[TcxButtonAssignedColor(Index)];
end;

function TcxButtonColors.IsColorStored(const Index: Integer): Boolean;
begin
  Result := TcxButtonAssignedColor(Index) in FAssignedColors;
end;

procedure TcxButtonColors.SetAssignedColors(Value: TcxButtonAssignedColors);
var
  AState: TcxButtonAssignedColor;
begin
  if (FAssignedColors <> Value) and FButton.IsDesigning then
  begin
    for AState := Low(AState) to High(AState) do
      if not (AState in Value) then
        FColors[AState] := clDefault
      else
        if FColors[AState] = clDefault then
          Exclude(Value, AState);
    FAssignedColors := Value;
    FButton.Invalidate;
  end;
end;

procedure TcxButtonColors.SetColor(const Index: Integer; const Value: TColor);
begin
  if (Value = clNone) or (Value = clDefault) then
  begin
    FColors[TcxButtonAssignedColor(Index)] := clDefault;
    Exclude(FAssignedColors, TcxButtonAssignedColor(Index));
    FButton.Invalidate;
  end
  else
    if GetColor(Index) <> Value then
    begin
      FColors[TcxButtonAssignedColor(Index)] := Value;
      Include(FAssignedColors, TcxButtonAssignedColor(Index));
      FButton.Invalidate;
    end;
end;

function TcxButtonColors.ButtonStateToButtonAssignedColor(AState: TcxButtonState;
  AIsTextColor: Boolean): TcxButtonAssignedColor;
const
  StateToAssignedColor: array[TcxButtonState, Boolean] of TcxButtonAssignedColor =
    ((cxbcDefault, cxbcDefaultText), (cxbcNormal, cxbcNormalText), (cxbcHot, cxbcHotText),
    (cxbcPressed, cxbcPressedText), (cxbcDisabled, cxbcDisabledText));
begin
  Result := StateToAssignedColor[AState, AIsTextColor];
end;

function TcxButtonColors.GetColorByState(const AState: TcxButtonState): TColor;
var
  AButtonColor: TcxButtonAssignedColor;
begin
  AButtonColor := ButtonStateToButtonAssignedColor(AState, False);
  if AButtonColor in AssignedColors then
    Result := FColors[AButtonColor]
  else
    if AButtonColor = cxbcNormal then
      Result := FColors[cxbcDefault]
    else
      Result := FColors[cxbcNormal];
end;

function TcxButtonColors.GetTextColorByState(const AState: TcxButtonState): TColor;
var
  AButtonColor: TcxButtonAssignedColor;
begin
  AButtonColor := ButtonStateToButtonAssignedColor(AState, True);
  if AButtonColor in AssignedColors then
    Result := FColors[AButtonColor]
  else
    if AButtonColor = cxbcNormalText then
      Result := FColors[cxbcDefaultText]
    else
      Result := FColors[cxbcNormalText];
end;

procedure TcxButtonColors.Assign(Source: TPersistent);
var
  AButtonColors: TcxButtonColors;
begin
  if Source is TcxButtonColors then
  begin
    AButtonColors := TcxButtonColors(Source);
    Self.FColors := AButtonColors.FColors;
    Self.FAssignedColors := AButtonColors.FAssignedColors;
    Self.FButton.Invalidate;
  end
  else
    inherited Assign(Source);
end;

{ TcxButtonGlyph }

constructor TcxButtonGlyph.Create(AOwner: TcxCustomButton);
begin
  inherited Create(AOwner);
  FGlyph := TdxSmartGlyph.Create;
  FGlyph.OnChange := Changed;
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
  FImageIndex := -1;
  FNumGlyphs := 1;
end;

destructor TcxButtonGlyph.Destroy;
begin
  Images := nil;
  FreeAndNil(FImageChangeLink);
  FreeAndNil(FGlyph);
  inherited Destroy;
end;

procedure TcxButtonGlyph.Changed(Sender: TObject);
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TcxButtonGlyph.GlyphChanged;
var
  ANumGlyphs: Integer;
begin
  ANumGlyphs := 1;
  if (FGlyph <> nil) and (FGlyph.SourceHeight > 0) and (FGlyph.SourceWidth mod FGlyph.SourceHeight = 0) then
  begin
    ANumGlyphs := FGlyph.SourceWidth div FGlyph.SourceHeight;
    if ANumGlyphs > 4 then
      ANumGlyphs := 1;
  end;
  FNumGlyphs := ANumGlyphs;
  Changed(Self);
end;

function TcxButtonGlyph.IsImageAssigned: Boolean;
begin
  Result := IsGlyphAssigned(Glyph) or cxGraphics.IsImageAssigned(Images, ImageIndex);
end;

procedure TcxButtonGlyph.SetGlyph(Value: TdxSmartGlyph);
begin
  FGlyph.Assign(Value);
  GlyphChanged;
end;

procedure TcxButtonGlyph.SetImageIndex(Value: Integer);
begin
  if ImageIndex <> Value then
  begin
    FImageIndex := Value;
    if not IsGlyphAssigned(Glyph) and (Images <> nil) then
      GlyphChanged;
  end;
end;

procedure TcxButtonGlyph.SetImages(Value: TCustomImageList);
begin
  cxSetImageList(Value, FImages, FImageChangeLink, FButton);
end;

procedure TcxButtonGlyph.SetNumGlyphs(Value: TNumGlyphs);
begin
  Value := Min(Max(Value, 1), 4);
  if Value <> FNumGlyphs then
  begin
    FNumGlyphs := Value;
    Changed(Self);
  end;
end;

function TcxButtonGlyph.GetImageSize: TSize;
begin
  Result := Button.ViewInfo.ImageSize;
end;

function TcxButtonGlyph.GetTransparentColor: TColor;
begin
  Result := clNone;
end;

procedure TcxButtonGlyph.ImageListChange(Sender: TObject);
begin
  if Sender = Images then
    GlyphChanged;
end;

procedure TcxButtonGlyph.FreeNotification(Sender: TComponent);
begin
  if Sender = FImages then
    Images := nil;
end;

{ TcxButtonActionLink }

destructor TcxButtonActionLink.Destroy;
begin
  if not (csDestroying in Client.ComponentState) then
  begin
    Client.FGlyph.Images := nil;
    Client.FGlyph.ImageIndex := -1;
  end;
  inherited;
end;

procedure TcxButtonActionLink.SetChecked(Value: Boolean);
begin
  inherited;
  if IsCheckedLinked then
    Client.SpeedButtonOptions.InternalSetDown(Value);
end;

procedure TcxButtonActionLink.SetGroupIndex(Value: Integer);
begin
  inherited;
  if IsGroupIndexLinked then
    Client.GroupIndex := Value;
end;

procedure TcxButtonActionLink.SetImageIndex(Value: Integer);
begin
  inherited;
  if IsImageIndexLinked then
    Client.OptionsImage.ImageIndex := Value;
end;

function TcxButtonActionLink.GetClient: TcxCustomButton;
begin
  Result := TcxCustomButton(FClient);
end;

function TcxButtonActionLink.IsImageIndexLinked: Boolean;
begin
  Result := inherited IsImageIndexLinked and
    (Client.OptionsImage.ImageIndex = (Action as TCustomAction).ImageIndex);
end;

{ TcxSpeedButtonOptions }

constructor TcxSpeedButtonOptions.Create(AOwner: TcxCustomButton);
begin
  inherited Create(AOwner);
  CanBeFocused := True;
  GroupIndex := 0;
end;

procedure TcxSpeedButtonOptions.Assign(Source: TPersistent);
var
  ASpeedButtonOptions: TcxSpeedButtonOptions;
begin
  if Source is TcxSpeedButtonOptions then
  begin
    ASpeedButtonOptions := TcxSpeedButtonOptions(Source);
    Self.GroupIndex := ASpeedButtonOptions.GroupIndex;
    Self.AllowAllUp := ASpeedButtonOptions.AllowAllUp;
    Self.CanBeFocused := ASpeedButtonOptions.CanBeFocused;
    Self.Down := ASpeedButtonOptions.Down;
    Self.Flat := ASpeedButtonOptions.Flat;
    Self.Transparent := ASpeedButtonOptions.Transparent;
  end
  else
    inherited Assign(Source);
end;

procedure TcxSpeedButtonOptions.BeginUpdate;
begin
  Inc(FLockCount);
end;

procedure TcxSpeedButtonOptions.CancelUpdate;
begin
  Dec(FLockCount);
end;

procedure TcxSpeedButtonOptions.EndUpdate;
begin
  Dec(FLockCount);
  UpdateGroup;
end;

procedure TcxSpeedButtonOptions.InternalSetDown(AValue: Boolean);

  function AllowCheck: Boolean;
  begin
    Result := (AValue <> Down) and
      ((not Down or FAllowAllUp or (FGroupIndex = 0) or (LockCount <> 0)) or Assigned(Button.Action) or
      (Button.SpeedButtonMode and Button.IsDesigning));
  end;

begin
  if (FGroupIndex = 0) and not Assigned(Button.Action) then
    AValue := False;

  if AllowCheck then
  begin
    if AValue then
    begin
      Button.InternalStates := Button.InternalStates + [bisDown];
      UpdateGroup;
    end
    else
      Button.InternalStates := Button.InternalStates - [bisDown];

    Button.Invalidate;
  end;
end;

procedure TcxSpeedButtonOptions.UpdateGroupValues(const AGroupIndex: Integer; const ASpeedButtonOptions: TcxSpeedButtonOptions);
begin
  BeginUpdate;
  try
    CanBeFocused := ASpeedButtonOptions.CanBeFocused;
    AllowAllUp := ASpeedButtonOptions.AllowAllUp;
    Flat := ASpeedButtonOptions.Flat;
    Transparent := ASpeedButtonOptions.Transparent;
    if ASpeedButtonOptions.Down and Down and (not Assigned(Button.Action) or (Button.Action <> ASpeedButtonOptions.Button.Action)) then
    begin
      Down := False;
      if (Button.Action is TCustomAction) then
        TCustomAction(Button.Action).Checked := False;
    end;
  finally
    CancelUpdate;
  end;
end;

function TcxSpeedButtonOptions.NeedDrawParts: Boolean;
begin
  Result := not CanBeFocused and (Transparent or Flat);
end;

function TcxSpeedButtonOptions.GetActive: Boolean;
begin
  Result := (Button <> nil) and ((FGroupIndex <> 0) or not FCanBeFocused);
end;

function TcxSpeedButtonOptions.GetDown: Boolean;
begin
  Result := Button.Down;
end;

function TcxSpeedButtonOptions.IsDownStored: Boolean;
begin
  Result := (Button.ActionLink = nil) or not TControlActionLinkAccess(Button.ActionLink).IsCheckedLinked;
end;

function TcxSpeedButtonOptions.IsGroupIndexStored: Boolean;
begin
  Result := (Button.ActionLink = nil) or (TCustomAction(Button.Action).GroupIndex <> FGroupIndex);
end;

procedure TcxSpeedButtonOptions.SetAllowAllUp(AValue: Boolean);
begin
  if AValue <> FAllowAllUp then
  begin
    FAllowAllUp := AValue;
    UpdateGroup;
  end;
end;

procedure TcxSpeedButtonOptions.SetCanBeFocused(AValue: Boolean);
begin
  if AValue <> FCanBeFocused then
  begin
    FCanBeFocused := AValue;
    UpdateGroup;
    Button.Repaint;
  end;
end;

procedure TcxSpeedButtonOptions.SetDown(AValue: Boolean);
begin
  if not Assigned(Button.Action) then
    InternalSetDown(AValue);
end;

procedure TcxSpeedButtonOptions.SetFlat(AValue: Boolean);
begin
  if FFlat <> AValue then
  begin
    FFlat := AValue;
    UpdateGroup;
    if Active then
      Button.Invalidate;
  end;
end;

procedure TcxSpeedButtonOptions.SetGroupIndex(AValue: Integer);
begin
  if AValue <> FGroupIndex then
  begin
    FGroupIndex := AValue;
    if FGroupIndex = 0 then
      Down := False
    else
      UpdateGroup;
  end;
end;

procedure TcxSpeedButtonOptions.SetTransparent(AValue: Boolean);
begin
  if FTransparent <> AValue then
  begin
    FTransparent := AValue;
    UpdateGroup;
    if Active then
      Button.Invalidate;
  end;
end;

procedure TcxSpeedButtonOptions.UpdateGroup;
var
  AMsg: TMessage;
begin
  if (LockCount = 0) and Active and (FGroupIndex <> 0) and (Button.Parent <> nil) then
  begin
    AMsg.Msg := CM_BUTTONPRESSED;
    AMsg.WParam := FGroupIndex;
    AMsg.LParam := LPARAM(Button);
    AMsg.Result := 0;
    Button.Parent.Broadcast(AMsg);
  end;
end;

{ TcxCustomButton }

constructor TcxCustomButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FViewInfo := CreateViewInfo;
  FGlyph := GetGlyphClass.Create(Self);
  FGlyph.OnChange := GlyphChanged;
  FColors := TcxButtonColors.Create(Self);
  FControlCanvas := TControlCanvas.Create;
  FControlCanvas.Control := Self;
  FCanvas := TcxCanvas.Create(FControlCanvas);
  FLookAndFeel := TcxLookAndFeel.Create(Self);
  FLookAndFeel.OnChanged := LookAndFeelChanged;
  FScaleFactor := TdxScaleFactor.Create;
  FScaleFactor.ListenerAdd(ScaleFactorChangeHandler);
  FSpeedButtonOptions := TcxSpeedButtonOptions.Create(Self);
  FOptionsImage := CreateOptionsImage;
  FButtonPressTimer := cxCreateTimer(DoButtonPressTimer, 1000, False);

  FInternalStates := [];
  FKind := cxbkStandard;
  FPopupAlignment := paLeft;
  FShowFocusRect := True;
  DoubleBuffered := True;
  ControlStyle := ControlStyle + [{$IFDEF DELPHI16} csOverrideStylePaint,{$ENDIF} csReflector, csOpaque];
end;

destructor TcxCustomButton.Destroy;
begin
  dxFader.Remove(Self);
  EndMouseTracking(Self);
  FreeAndNil(FButtonPressTimer);
  FreeAndNil(FOptionsImage);
  FreeAndNil(FSpeedButtonOptions);
  FreeAndNil(FLookAndFeel);
  FScaleFactor.ListenerRemove(ScaleFactorChangeHandler);
  FreeAndNil(FScaleFactor);
  FreeAndNil(FColors);
  FreeAndNil(FCanvas);
  FreeAndNil(FControlCanvas);
  FreeAndNil(FGlyph);
  FreeAndNil(FViewInfo);
  inherited Destroy;
end;

procedure TcxCustomButton.SetGlyph(Value: TdxSmartGlyph);
begin
  OptionsImage.Glyph := Value;
end;

function TcxCustomButton.GetGlyph: TdxSmartGlyph;
begin
  Result := OptionsImage.Glyph;
end;

procedure TcxCustomButton.SetLayout(Value: TButtonLayout);
begin
  OptionsImage.Layout := Value;
end;

function TcxCustomButton.GetNumGlyphs: TNumGlyphs;
begin
  Result := OptionsImage.NumGlyphs;
end;

procedure TcxCustomButton.SetNumGlyphs(Value: TNumGlyphs);
begin
  OptionsImage.NumGlyphs := Value;
end;

function TcxCustomButton.GetScaleFactor: TdxScaleFactor;
begin
  Result := FScaleFactor;
end;

procedure TcxCustomButton.SetSpacing(Value: Integer);
begin
  OptionsImage.Spacing := Value;
end;

procedure TcxCustomButton.SetMargin(Value: Integer);
begin
  OptionsImage.Margin := Value;
end;

procedure TcxCustomButton.DoContextPopup(MousePos: TPoint;
  var Handled: Boolean);
var
  P: TPoint;
begin
  inherited DoContextPopup(MousePos, Handled);
  if not Handled then
  begin
    if (MousePos.X = -1) and (MousePos.Y = -1) then
      P := ClientToScreen(Point(0, 0))
    else
      P := ClientToScreen(MousePos);
    Handled := DoShowPopupMenu(PopupMenu, P.X, P.Y);
  end;
end;

function TcxCustomButton.DoCustomDraw(ACanvas: TcxCanvas; AViewInfo: TcxButtonViewInfo): Boolean;
begin
  Result := False;
  if Assigned(FOnCustomDraw) then
    FOnCustomDraw(Self, ACanvas, AViewInfo, Result);
end;

function TcxCustomButton.DoOnDropDownMenuPopup(var APopupMenu: TPopupMenu): Boolean;
begin
  Result := False;
  if Assigned(OnDropDownMenuPopup) then
    OnDropDownMenuPopup(Self, APopupMenu, Result);
end;

function TcxCustomButton.DoOnDropDownMenuPopupEx(var APopupMenu: TComponent): Boolean;
begin
  Result := False;
  if Assigned(OnDropDownMenuPopupEx) then
    OnDropDownMenuPopupEx(Self, APopupMenu, Result);
end;

function TcxCustomButton.DoShowPopupMenu(APopupMenu: TComponent; X, Y: Integer): Boolean;
begin
  Result := ShowPopupMenu(Self, APopupMenu, X, Y);
end;

function TcxCustomButton.GetGlyphClass: TcxButtonGlyphClass;
begin
  Result := TcxButtonGlyph;
end;

function TcxCustomButton.GetPainter: TcxCustomLookAndFeelPainter;
begin
  Result := GetButtonPainter(LookAndFeel);
end;

function TcxCustomButton.IsDesigning: Boolean;
begin
  Result := csDesigning in ComponentState;
end;

function TcxCustomButton.StandardButton: Boolean;
begin
  Result := False;
end;

procedure TcxCustomButton.UpdateSize;
var
  ASize: TSize;
begin
  if AutoSize then
  begin
    if csLoading in ComponentState then
      ASize := GetOptimalSize
    else
      ASize := cxNullSize;

    SetBounds(Left, Top, ASize.cx, ASize.cy);
  end;
end;

function TcxCustomButton.UseRightToLeftAlignment: Boolean;
begin
  if StandardButton then
    Result := inherited
  else
    Result := SysLocale.MiddleEast and (BiDiMode = bdRightToLeft);
end;

// IcxLookAndFeelContainer
function TcxCustomButton.GetLookAndFeel: TcxLookAndFeel;
begin
  Result := LookAndFeel;
end;

procedure TcxCustomButton.Click;
var
  AExecuteClick: Boolean;
begin
  AExecuteClick := (bisPressed in InternalStates) or
    ([bisPressed, bisMenuButtonPressed] * InternalStates = []);
  if SpeedButtonMode and not Assigned(Action) and CanBeFocused and AExecuteClick and not (bisAccelExecute in InternalStates) then
    SetDown(not Down);
  if Kind in [cxbkDropDown, cxbkOfficeDropDown] then
    DoDropDownMenu
  else
    inherited Click;
end;

function TcxCustomButton.CanFade: Boolean;
begin
  Result := (Painter.LookAndFeelStyle in [lfsSkin, lfsNative]) and
    (IsWinVistaOrLater or (Kind <> cxbkCommandLink)) and CanHotTrack and not Assigned(FOnCustomDraw);
end;

function TcxCustomButton.CanFocus: Boolean;
begin
  Result := inherited CanFocus and (CanBeFocused or IsDesigning);
end;

procedure TcxCustomButton.DefaultHandler(var Message);
begin
  if not TcxControlDefaultHandlerHelper.Process(Message) then
    inherited DefaultHandler(Message);
end;

function TcxCustomButton.GetOptimalSize: TSize;
var
  ACanvas: TcxScreenCanvas;
  ACaption: TCaption;
begin
  ACanvas := TcxScreenCanvas.Create;
  try
    ACanvas.Font := Font;
    ACaption := RemoveAccelChars(Caption);
    if ACaption = '' then
      ACaption := ' ';
    Result.cx := ACanvas.TextWidth(ACaption) + ACanvas.TextWidth('R') * 3;
    Result.cy := MulDiv(cxTextHeight(ACanvas.Handle), 14, 8);
  finally
    ACanvas.Free;
  end;
end;

function TcxCustomButton.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action);
  UpdateImageInfo;
end;

procedure TcxCustomButton.LookAndFeelChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues);
begin
  if UseSystemPaint then
    RecreateWnd
  else
    Invalidate;
end;

procedure TcxCustomButton.ScaleFactorChangeHandler(Sender: TObject; M, D: Integer; IsLoading: Boolean);
begin
  if not IsLoading then
    ScaleFactorChanged(M, D);
end;

procedure TcxCustomButton.SetButtonAutoSize(Value: Boolean);
begin
  if Value <> FAutoSize then
  begin
    FAutoSize := Value;
    UpdateSize;
  end;
end;

procedure TcxCustomButton.SetColors(const Value: TcxButtonColors);
begin
  FColors.Assign(Value);
end;

procedure TcxCustomButton.SetCommandLinkHint(const Value: string);
begin
  Description := Value;
end;

procedure TcxCustomButton.SetKind(const Value: TcxButtonKind);
begin
  if FKind <> Value then
  begin
    if Value = cxbkCommandLink then
      Style := bsCommandLink
    else
      Style := bsPushButton;

    FKind := Value;
    RecreateWnd;
  end
end;

procedure TcxCustomButton.SetLookAndFeel(Value: TcxLookAndFeel);
begin
  FLookAndFeel.Assign(Value);
end;

procedure TcxCustomButton.SetPaintStyle(Value: TcxButtonPaintStyle);
begin
  if FPaintStyle <> Value then
  begin
    FPaintStyle := Value;
    Invalidate;
  end;
end;

procedure TcxCustomButton.SetPopupMenu(Value: TComponent);
begin
  if not IsPopupMenu(Value) then
    Value := nil;
  if FPopupMenu <> Value then
  begin
    cxRemoveFreeNotification(Self, FPopupMenu);
    FPopupMenu := Value;
    cxAddFreeNotification(Self, FPopupMenu);
  end;
end;

procedure TcxCustomButton.CheckPressedState(const P: TPoint);
begin
  if (Kind in [cxbkStandard, cxbkCommandLink]) or not PtInRect(GetMenuButtonBounds, P) then
    InternalStates := InternalStates + [bisPressed]
  else
    InternalStates := InternalStates + [bisMenuButtonPressed];
end;

procedure TcxCustomButton.CheckShowMenu(const P: TPoint);
begin
  if PtInRect(GetMenuButtonBounds, P) then
    DoDropDownMenu
end;

procedure TcxCustomButton.CheckStartFading(APrevButtonState, AButtonState: TcxButtonState);
begin
  if APrevButtonState <> AButtonState then
  begin
    if (APrevButtonState in [cxbsDefault, cxbsNormal]) and (AButtonState = cxbsHot) then
      dxFader.FadeIn(Self)
    else
      if (AButtonState in [cxbsDefault, cxbsNormal]) and (APrevButtonState = cxbsHot) then
        dxFader.FadeOut(Self)
      else
        dxFader.Remove(Self, False);
  end;
end;

function TcxCustomButton.GetButtonState: TcxButtonState;
begin
  if not Enabled then
    Result := cxbsDisabled
  else
    if FIsPaintPressed then
      Result := cxbsPressed
    else
      if (bisHot in InternalStates) and CanHotTrack then
        Result := cxbsHot
      else
        if FIsPaintDefault then
          Result := cxbsDefault
        else
          Result := cxbsNormal;
end;

function TcxCustomButton.GetDown: Boolean;
begin
  Result := bisDown in InternalStates;
end;

function TcxCustomButton.GetMenuButtonBounds: TRect;
begin
  case Kind of
    cxbkStandard:
      Result := cxNullRect;
    cxbkDropDownButton:
      Result := cxRectSetRight(ClientRect, ClientRect.Right, ViewInfo.Painter.GetDropDownButtonSize);
    else
      Result := ClientRect;
  end;
end;

function TcxCustomButton.GetAllowAllUp: Boolean;
begin
  Result := FSpeedButtonOptions.AllowAllUp;
end;

function TcxCustomButton.GetAllowSpeedButtonOptions: Boolean;
begin
  Result := Kind <> cxbkCommandLink;
end;

function TcxCustomButton.GetCanBeFocused: Boolean;
begin
  Result := (SpeedButtonOptions <> nil) and SpeedButtonOptions.CanBeFocused or not AllowSpeedButtonOptions;
end;

function TcxCustomButton.GetGroupIndex: Integer;
begin
  if AllowSpeedButtonOptions then
    Result := FSpeedButtonOptions.GroupIndex
  else
    Result := 0;
end;

function TcxCustomButton.GetSpeedButtonMode: Boolean;
begin
  Result := not CanBeFocused or (GroupIndex <> 0);
end;

procedure TcxCustomButton.SetAllowAllUp(AValue: Boolean);
begin
  FSpeedButtonOptions.AllowAllUp := AValue;
end;

procedure TcxCustomButton.SetCanBeFocused(AValue: Boolean);
begin
  FSpeedButtonOptions.CanBeFocused := AValue;
end;

procedure TcxCustomButton.SetDescription(const Value: string);
begin
  if FDescription <> Value then
  begin
    FDescription := Value;
    Invalidate;
  end;
end;

procedure TcxCustomButton.SetDown(AValue: Boolean);
begin
  FSpeedButtonOptions.Down := AValue;
end;

procedure TcxCustomButton.SetDropDownMenu(AValue: TComponent);
begin
  if not IsPopupMenu(AValue) then
    AValue := nil;
  if FDropDownMenu <> AValue then
  begin
    cxRemoveFreeNotification(Self, FDropDownMenu);
    FDropDownMenu := AValue;
    cxAddFreeNotification(Self, FDropDownMenu);
  end;
end;

procedure TcxCustomButton.SetGroupIndex(AValue: Integer);
begin
  FSpeedButtonOptions.GroupIndex := AValue;
end;

procedure TcxCustomButton.SetInternalStates(AValue: TcxButtonInternalStates);
var
  APrevButtonState: TcxButtonState;
begin
  if InternalStates <> AValue then
  begin
    APrevButtonState := GetButtonState;
    FInternalStates := AValue;
    CheckStartFading(APrevButtonState, GetButtonState);
    Invalidate;
  end;
end;

procedure TcxCustomButton.SetSpeedButtonOptions(AValue: TcxSpeedButtonOptions);
begin
  SpeedButtonOptions.Assign(AValue);
end;

procedure TcxCustomButton.WndProc(var Message: TMessage);
begin
  if SpeedButtonMode and not IsDesigning and not CanBeFocused then
    case Message.Msg of
      WM_LBUTTONDOWN:
        MouseDown(mbLeft, KeysToShiftState(Message.WParam), Message.LParamLo, Message.LParamHi);
      WM_LBUTTONDBLCLK:
        DblClick;
      else
        inherited WndProc(Message);
    end
  else
    inherited WndProc(Message);
end;

procedure TcxCustomButton.CMTextChanged(var Message: TMessage);
begin
  inherited;
  UpdateSize;
end;

procedure TcxCustomButton.CNCommand(var Message: TWMCommand);

  function GetExecuteClick: Boolean;
  begin
    Result := (bisPressed in InternalStates) or
      ([bisPressed, bisMenuButtonPressed] * InternalStates = []);
  end;

begin
  if (Message.NotifyCode <> BN_CLICKED) or (Kind <> cxbkDropDownButton) or GetExecuteClick then
    inherited;
end;

procedure TcxCustomButton.DoDropDownMenu;
begin
  if not (bisMenuVisible in InternalStates) and not FIsDropDownMenuClosing then
    PostMessage(Handle, DXM_DROPDOWNPOPUPMENU, 0, 0);
end;

function TcxCustomButton.DoDropDownMenuPopup(var APopupMenu: TComponent): Boolean;
begin
  Result := DoOnDropDownMenuPopupEx(APopupMenu);
  if not Result and ((APopupMenu = nil) or (APopupMenu is TPopupMenu)) then
    Result := DoOnDropDownMenuPopup(TPopupMenu(APopupMenu));
end;

procedure TcxCustomButton.DoGetDrawParams(AState: TcxButtonState;
  var AColor: TColor; AFont: TFont);
begin
  if Assigned(FOnGetDrawParams) then
    FOnGetDrawParams(Self, AState, AColor, AFont);
end;

function TcxCustomButton.GetColors: TcxButtonColors;
begin
  Result := FColors;
end;

function TcxCustomButton.GetCommandLinkHint: string;
begin
  Result := Description;
end;

function TcxCustomButton.GetControlCanvas: TControlCanvas;
begin
  Result := FControlCanvas;
end;

procedure TcxCustomButton.GetFadingImages(out AFadeOutImage, AFadeInImage: TcxBitmap);

  function CalculateButtonState(AHotTracked: Boolean): TcxButtonState;
  begin
    if AHotTracked then
      Result := cxbsHot
    else
      if CanBeFocused and IsFocused then
        Result := cxbsDefault
      else
        Result := cxbsNormal;
  end;

  function PrepareFadingImage(AHotTracked: Boolean): TcxBitmap32;
  var
    APrevState: TcxButtonState;
  begin
    APrevState := ViewInfo.State;
    try
      Result := TcxBitmap32.CreateSize(ViewInfo.Bounds, True);
      Result.cxCanvas.UseRightToLeftAlignment := UseRightToLeftAlignment;
      Result.cxCanvas.WindowOrg := ViewInfo.Bounds.TopLeft;
      ViewInfo.FState := CalculateButtonState(AHotTracked);
      ViewInfo.DrawBackground(Result.cxCanvas);
      Result.cxCanvas.WindowOrg := cxNullPoint;
    finally
      ViewInfo.FState := APrevState;
    end;
  end;

begin
  AFadeOutImage := PrepareFadingImage(False);
  AFadeInImage := PrepareFadingImage(True);
end;

function TcxCustomButton.IsColorsStored: Boolean;
begin
  Result := FColors.AssignedColors <> [];
end;

function TcxCustomButton.CanHotTrack: Boolean;
begin
  Result := not StandardButton and Painter.IsButtonHotTrack or (Kind = cxbkCommandLink) and Enabled;
end;

function TcxCustomButton.GetAdornerTargetElementControl: TWinControl;
begin
  Result := Self;
end;

function TcxCustomButton.GetAdornerTargetElementBounds: TRect;
begin
  Result := ClientRect;
end;

function TcxCustomButton.GetAdornerTargetElementVisible: Boolean;
begin
  Result := Visible;
end;

procedure TcxCustomButton.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  inherited ActionChange(Sender, CheckDefaults);
  if not CheckDefaults or (OptionsImage.ImageIndex = -1) then
  begin
    OptionsImage.ImageIndex := TCustomAction(Action).ImageIndex;
    SpeedButtonOptions.GroupIndex := TCustomAction(Action).GroupIndex;
    SpeedButtonOptions.InternalSetDown(TCustomAction(Action).Checked);
  end;
  UpdateImageInfo;
end;

function TcxCustomButton.CanResize(var NewWidth, NewHeight: Integer): Boolean;
var
  AOptimalSize: TSize;
begin
  if AutoSize then
  begin
    AOptimalSize := GetOptimalSize;
    NewWidth := AOptimalSize.cx;
    NewHeight := AOptimalSize.cy;
  end;
  Result := inherited CanResize(NewWidth, NewHeight);
end;

{$IFDEF DELPHIBERLIN}
procedure TcxCustomButton.ChangeScale(M, D: Integer; isDpiChange: Boolean);
{$ELSE}
procedure TcxCustomButton.ChangeScale(M, D: Integer);
{$ENDIF}
begin
  ScaleFactor.Change(M, D);
  inherited;
  LookAndFeel.Refresh;
end;

function TcxCustomButton.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TcxButtonActionLink;
end;

function TcxCustomButton.GetChecked: Boolean;
begin
  Result := Down;
end;

procedure TcxCustomButton.ScaleFactorChanged(M, D: Integer);
begin
  if OptionsImage.Margin <> -1 then
    OptionsImage.Margin := MulDiv(OptionsImage.Margin, M, D);
  if OptionsImage.Spacing <> -1 then
    OptionsImage.Spacing := MulDiv(OptionsImage.Spacing, M, D);
end;

procedure TcxCustomButton.SetChecked(Value: Boolean);
begin
  Down := Value;
end;

procedure TcxCustomButton.DblClick;
begin
  if not CanBeFocused then
    SetCapture(Handle);
  if GroupIndex <> 0 then
    inherited DblClick;
  CheckPressedState(ScreenToClient(GetMouseCursorPos));
  Invalidate;
end;

procedure TcxCustomButton.MouseEnter;
begin
  BeginMouseTracking(Self, Rect(0, 0, Width, Height), Self);
  InternalStates := InternalStates + [bisHot];
end;

procedure TcxCustomButton.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (Kind = cxbkDropDownButton) and (Key in [VK_UP, VK_DOWN]) and
    ((ssAlt in Shift) or (ssShift in Shift)) then
  begin
    if not (bisMenuVisible in InternalStates) then
      DoDropDownMenu;
    Key := 0;
  end
  else
    inherited KeyDown(Key, Shift);
end;

procedure TcxCustomButton.MouseLeave;
begin
  EndMouseTracking(Self);
  InternalStates := InternalStates - [bisHot];
end;

procedure TcxCustomButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if not CanBeFocused then
    SetCapture(Handle);
  inherited MouseDown(Button, Shift, X, Y);
  if (Button = mbLeft) then
  begin
    CheckPressedState(Point(X, Y));
    if RepeatClick and (bisPressed in InternalStates) then
    begin
      FButtonPressTimer.Interval := cxButtonsRepeatInitPause;
      FButtonPressTimer.Enabled := True;
    end;
    if Kind = cxbkDropDownButton then
      CheckShowMenu(Point(X, Y));
    Invalidate;
  end;
end;

procedure TcxCustomButton.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  if PtInRect(ClientRect, Point(X, Y)) then
    MouseEnter;
  if (bisHot in InternalStates) and not PtInRect(ClientRect, Point(X, Y)) then
    MouseLeave;
end;

procedure TcxCustomButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  AExecuteClick: Boolean;
begin
  FButtonPressTimer.Enabled := False;
  AExecuteClick := not CanBeFocused and (Button = mbLeft) and (bisHot in InternalStates) and
    ((bisPressed in InternalStates) or ((bisMenuButtonPressed in InternalStates) and (Kind in [cxbkDropDown, cxbkOfficeDropDown])));
  if SpeedButtonMode and not Assigned(Action) and AExecuteClick then
    SetDown(not Down);
  inherited MouseUp(Button, Shift, X, Y);
  InternalStates := InternalStates - [bisPressed, bisMenuButtonPressed];
  if not CanBeFocused and (GetCapture = Handle) then
    ReleaseCapture;
  if AExecuteClick then
    Click;
  Invalidate;
  if not (bisMenuVisible in InternalStates) then
    FIsDropDownMenuClosing := False;
end;

procedure TcxCustomButton.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if OptionsImage <> nil then
      OptionsImage.FreeNotification(AComponent);
    if FGlyph <> nil then
      FGlyph.FreeNotification(AComponent);
    if AComponent = DropDownMenu then
      DropDownMenu := nil;
    if AComponent = PopupMenu then
      PopupMenu := nil;
  end;
end;

procedure TcxCustomButton.DoButtonPressTimer(Sender: TObject);

  function NeedStopTimer: Boolean;
  begin
    Result := not (ssLeft in KeyboardStateToShiftState);
  end;

begin
  if NeedStopTimer then
    FButtonPressTimer.Enabled := False
  else
  begin
    FButtonPressTimer.Interval := cxButtonsRepeatPause;
    if (bisHot in InternalStates) and (bisPressed in InternalStates) then
      Click;
  end;
end;

procedure TcxCustomButton.Draw(DC: HDC);
begin
  FCanvas.Lock;
  try
    FCanvas.Canvas.Handle := DC;
    try
      if not DoCustomDraw(FCanvas, ViewInfo) then
        ViewInfo.Draw(FCanvas);
    finally
      FCanvas.Canvas.Handle := 0;
    end;
  finally
    FCanvas.Unlock;
  end;
end;

procedure TcxCustomButton.DrawItem(const DrawItemStruct: TDrawItemStruct);

  procedure InternalDrawItem;
  var
    ABitmap: TcxBitmap;
    AMemDC: HDC;
    APaintBuffer: TdxPaintBuffer;
    ARect: TRect;
  begin
    if ViewInfo.IsDrawOnGlass then
    begin
      ARect := ClientRect;
      APaintBuffer := BeginBufferedPaint(DrawItemStruct.hDC, @ARect, BPBF_COMPOSITED, nil, AMemDC);
      if APaintBuffer <> 0 then
      try
        Draw(AMemDC);
      finally
        EndBufferedPaint(APaintBuffer, True);
      end;
    end
    else
    begin
      ABitmap := TcxBitmap.CreateSize(ClientRect);
      try
        ABitmap.Canvas.Lock;
        try
          Draw(ABitmap.Canvas.Handle);
          cxBitBlt(DrawItemStruct.hDC, ABitmap.Canvas.Handle, ClientRect, cxNullPoint, SRCCOPY);
        finally
          ABitmap.Canvas.Unlock;
        end;
      finally
        ABitmap.Free;
      end;
    end;
  end;

var
  APrevButtonState: TcxButtonState;
begin
  APrevButtonState := GetButtonState;
  FIsPaintPressed := ((DrawItemStruct.itemState and ODS_SELECTED <> 0) and not (bisMenuButtonPressed in InternalStates)) or
    ((bisMenuVisible in InternalStates) and (Kind in [cxbkDropDown, cxbkOfficeDropDown])) or
    (Down or ([bisPressed, bisHot] * InternalStates = [bisPressed, bisHot]));
  FIsPaintDefault := ((DrawItemStruct.itemState and ODS_FOCUS <> 0) or
    (Default and IsDesigning) or FIsFocused) and CanBeFocused;
  CheckStartFading(APrevButtonState, GetButtonState);
  ViewInfo.Calculate(ClientRect);
  InternalDrawItem;
end;

procedure TcxCustomButton.UpdateImageInfo;

  function GetImageList: TCustomImageList;
  begin
    if OptionsImage.Images <> nil then
      Result := OptionsImage.Images
    else
      if (Action is TCustomAction) and (TCustomAction(Action).ActionList <> nil) then
        Result := TCustomAction(Action).ActionList.Images
      else
        Result := nil;
  end;

begin
  FGlyph.Images := GetImageList;
  FGlyph.ImageIndex := OptionsImage.ImageIndex;
end;

procedure TcxCustomButton.BCMSetNote(var Message: TMessage);
begin
  Invalidate;
end;

procedure TcxCustomButton.CNCtlColorBtn(var Message: TWMCtlColorBtn);
begin
  // do nothing
end;

procedure TcxCustomButton.WMCaptureChanged(var Message: TMessage);
var
  P: TPoint;
begin
  inherited;
  if not IsDesigning then
  begin
    GetCursorPos(P);
    if WindowFromPoint(P) <> Handle then
      InternalStates := InternalStates - [bisHot, bisPressed, bisMenuButtonPressed];
  end;
end;

procedure TcxCustomButton.WMContextMenu(var Message: TWMContextMenu);

  function IsPtInRect(var P: TPoint): Boolean;
  begin
    P := SmallPointToPoint(Message.Pos);
    if (P.X = -1) or (P.Y = -1) then
      Result := False
    else
    begin
      P := ScreenToClient(P);
      Result := PtInRect(ClientRect, P);
    end;
  end;

var
  AHandled: Boolean;
  P: TPoint;
begin
  if Message.Result = 0 then
    if not IsDesigning and not IsPtInRect(P) then
    begin
      AHandled := False;
      DoContextPopup(P, AHandled);
      Message.Result := Ord(AHandled);
      if not AHandled then
        inherited;
    end
    else
      inherited;
end;

procedure TcxCustomButton.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.Result := 1;
end;

procedure TcxCustomButton.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
  Perform(WM_LBUTTONDOWN, Message.Keys,
    LPARAM(Word(Message.XPos) or (Word(Message.YPos) shr 16)));
end;

procedure TcxCustomButton.WMPaint(var Message: TWMPaint);
begin
  DefaultHandler(Message);
end;

procedure TcxCustomButton.WMWindowPosChanged(var Message: TWMWindowPosChanged);
begin
  inherited;
  if not (csDestroying in ComponentState) and HandleAllocated and
    SpeedButtonOptions.Transparent and not CanBeFocused
  then
    Invalidate;
end;

procedure TcxCustomButton.CMCloseUpPopupMenu(var Message: TMessage);
var
  Msg: TMsg;
begin
  Msg.Message := 0;
  FIsDropDownMenuClosing := (bisMenuButtonPressed in InternalStates) or
    ((PeekMessage(Msg, Handle, WM_LBUTTONDOWN, WM_LBUTTONDOWN, PM_NOREMOVE) or
      PeekMessage(Msg, Handle, WM_LBUTTONDBLCLK, WM_LBUTTONDBLCLK, PM_NOREMOVE)) and
      PtInRect(GetMenuButtonBounds, ScreenToClient(Msg.pt)));
  InternalStates := InternalStates - [bisMenuVisible];
end;

procedure TcxCustomButton.CMDialogChar(var Message: TCMDialogChar);
begin
  if IsAccel(Message.CharCode, Caption) and inherited CanFocus then
  begin
    InternalStates := InternalStates + [bisAccelExecute];
    Click;
    InternalStates := InternalStates - [bisAccelExecute];
    Message.Result := 1;
  end
  else
    inherited;
end;

procedure TcxCustomButton.CMDropDownPopupMenu(var Message: TMessage);
var
  APopupMenu: TComponent;
begin
  if Kind <> cxbkStandard then
  begin
    APopupMenu := FDropDownMenu;
    if not DoDropDownMenuPopup(APopupMenu) and (APopupMenu <> nil) then
    begin
      InternalStates := InternalStates + [bisMenuVisible] - [bisMenuButtonPressed];
      FIsDropDownMenuClosing := False;
      Repaint;

      ShowPopupMenu(Self, APopupMenu, cxRectOffset(ClientRect, ClientOrigin), FPopupAlignment);
      PostMessage(Handle, DXM_CLOSEUPPOPUPMENU, 0, 0);
    end;
  end;
end;

procedure TcxCustomButton.CMFontChanged(var Message: TMessage);
begin
  inherited;
  UpdateSize;
  Invalidate;
end;

procedure TcxCustomButton.CMHintShow(var Message: TCMHintShow);
begin
  inherited;
  Message.HintInfo^.CursorRect := ClientRect;
end;

procedure TcxCustomButton.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  if not Enabled then
    InternalStates := InternalStates - [bisHot];
  Invalidate;
end;

procedure TcxCustomButton.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if IsDesigning then Exit;
  MouseEnter;
end;

procedure TcxCustomButton.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if IsDesigning then Exit;
  MouseLeave;
end;

procedure TcxCustomButton.CNDrawItem(var Message: TWMDrawItem);
begin
  if not(csDestroying in ComponentState) then
    DrawItem(Message.DrawItemStruct^);
end;

procedure TcxCustomButton.CNKeyDown(var Message: TWMKeyDown);
begin
  if IsPopupMenuShortCut(PopupMenu, Message) then
    Message.Result := 1
  else
    inherited;
end;

procedure TcxCustomButton.CNMeasureItem(var Message: TWMMeasureItem);
var
  ATempVar: TMeasureItemStruct;
begin
  ATempVar := Message.MeasureItemStruct^;
  ATempVar.itemWidth := Width;
  ATempVar.itemHeight := Height;
  Message.MeasureItemStruct^ := ATempVar;
end;

procedure TcxCustomButton.CNSysKeyDown(var Message: TWMSysKeyDown);
begin
  if IsPopupMenuShortCut(PopupMenu, Message) then
    Message.Result := 1
  else
    inherited;
end;

procedure TcxCustomButton.CMBiDiModeChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TcxCustomButton.CMButtonPressed(var Message: TMessage);
var
  ASender: TcxCustomButton;
begin
  if SpeedButtonMode then
  begin
    if (Message.WParam = WParam(GroupIndex)) and (GroupIndex <> 0) and
      (TObject(Message.LParam) is TcxCustomButton) then
    begin
      ASender := TcxCustomButton(Message.LParam);
      if ASender <> Self then
      begin
        SpeedButtonOptions.UpdateGroupValues(GroupIndex, ASender.SpeedButtonOptions);
        Invalidate;
      end;
    end;
  end;
end;

function TcxCustomButton.CreateOptionsImage: TcxButtonImageOptions;
begin
  Result := TcxButtonImageOptions.Create(Self);
end;

procedure TcxCustomButton.CreateParams(var Params: TCreateParams);
const
  CommandLinkStyles: array[Boolean] of Cardinal = (BS_COMMANDLINK, BS_DEFCOMMANDLINK);
  SplitButtonStyles: array[Boolean] of Cardinal = (BS_SPLITBUTTON, BS_DEFSPLITBUTTON);
begin
  inherited CreateParams(Params);
  if IsWinVistaOrLater then
  begin
    case Style of
      bsCommandLink: Params.Style := Params.Style xor CommandLinkStyles[Default];
      bsSplitButton: Params.Style := Params.Style xor SplitButtonStyles[Default];
    end;
  end;
  if not StandardButton then
    Params.Style := Params.Style or BS_OWNERDRAW;
end;

function TcxCustomButton.CreateViewInfo: TcxButtonViewInfo;
begin
  Result := TcxButtonViewInfo.Create(Self);
end;

procedure TcxCustomButton.DestroyWindowHandle;
begin
  if bisMenuVisible in InternalStates then
    SendMessage(Handle, DXM_CLOSEUPPOPUPMENU, 0, 0);
  inherited DestroyWindowHandle;
end;

function TcxCustomButton.GetPalette: HPALETTE;
begin
  Result := Glyph.Palette;
end;

procedure TcxCustomButton.SetButtonStyle(ADefault: Boolean);
begin
  if StandardButton then
    inherited SetButtonStyle(ADefault)
  else
    if ADefault <> FIsFocused then
    begin
      FIsFocused := ADefault;
      Refresh;
    end;
end;

procedure TcxCustomButton.SetParent(AValue: TWinControl);
begin
  if TcxControlHelper.CanSetParent(Self, AValue) then
  begin
    inherited SetParent(AValue);
    TcxControlHelper.UpdateScaleFactorOnParentChange(Self);
  end;
end;

procedure TcxCustomButton.SetOptionsImage(Value: TcxButtonImageOptions);
begin
  FOptionsImage.Assign(Value);
end;

function TcxCustomButton.GetLayout: TButtonLayout;
begin
  Result := OptionsImage.Layout;
end;

function TcxCustomButton.GetMargin: Integer;
begin
  Result := OptionsImage.Margin;
end;

function TcxCustomButton.GetSpacing: Integer;
begin
  Result := OptionsImage.Spacing;
end;

procedure TcxCustomButton.GlyphChanged(Sender: TObject);
begin
  Invalidate;
end;

{ TcxButtonImageOptions }

constructor TcxButtonImageOptions.Create(AOwner: TcxCustomButton);
begin
  inherited Create(AOwner);
  FLayout := blGlyphLeft;
  FSpacing := 4;
  FMargin := -1;
  FImageIndex := -1;
end;

procedure TcxButtonImageOptions.Assign(Source: TPersistent);
var
  AButtonImageOptions: TcxButtonImageOptions;
begin
  if Source is TcxButtonImageOptions then
  begin
    AButtonImageOptions := TcxButtonImageOptions(Source);
    Glyph := AButtonImageOptions.Glyph;
    ImageIndex := AButtonImageOptions.ImageIndex;
    Images := AButtonImageOptions.Images;
    Layout := AButtonImageOptions.Layout;
    Margin := AButtonImageOptions.Margin;
    NumGlyphs := AButtonImageOptions.NumGlyphs;
    Spacing := AButtonImageOptions.Spacing;
  end
  else
    inherited Assign(Source);
end;

procedure TcxButtonImageOptions.Changed;
begin
  Button.UpdateImageInfo;
  Button.Invalidate;
end;

procedure TcxButtonImageOptions.FreeNotification(Sender: TComponent);
begin
  if Images = Sender then
    Images := nil;
end;

function TcxButtonImageOptions.GetGlyph: TdxSmartGlyph;
begin
  Result := Button.FGlyph.Glyph;
end;

function TcxButtonImageOptions.GetNumGlyphs: TNumGlyphs;
begin
  Result := Button.FGlyph.NumGlyphs;
end;

function TcxButtonImageOptions.IsImageIndexStored: Boolean;
begin
  Result := (Button.ActionLink = nil) or not TControlActionLinkAccess(Button.ActionLink).IsImageIndexLinked;
end;

procedure TcxButtonImageOptions.SetGlyph(Value: TdxSmartGlyph);
begin
  Button.FGlyph.Glyph := Value;
end;

procedure TcxButtonImageOptions.SetImageIndex(Value: TcxImageIndex);
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    Changed;
  end;
end;

procedure TcxButtonImageOptions.SetImages(Value: TCustomImageList);
begin
  if Value <> FImages then
  begin
    FImages := Value;
    Changed;
  end;
end;

procedure TcxButtonImageOptions.SetLayout(Value: TButtonLayout);
begin
  if FLayout <> Value then
  begin
    FLayout := Value;
    Changed;
  end;
end;

procedure TcxButtonImageOptions.SetMargin(Value: Integer);
begin
  if (Value <> FMargin) and (Value >= - 1) then
  begin
    FMargin := Value;
    Changed;
  end;
end;

procedure TcxButtonImageOptions.SetNumGlyphs(Value: TNumGlyphs);
begin
  if Value <> NumGlyphs then
  begin
    Button.FGlyph.NumGlyphs := Value;
    Changed;
  end;
end;

procedure TcxButtonImageOptions.SetSpacing(Value: Integer);
begin
  if FSpacing <> Value then
  begin
    FSpacing := Value;
    Changed;
  end;
end;

{ TcxButtonCustomLayoutCalculator }

function TcxButtonCustomLayoutCalculator.GetOptionsImage: TcxButtonImageOptions;
begin
  Result := ViewInfo.OptionsImage;
end;

function TcxButtonCustomLayoutCalculator.GetPainter: TcxButtonPainter;
begin
  Result := ViewInfo.Painter;
end;

function TcxButtonCustomLayoutCalculator.GetUseRightToLeftAlignment: Boolean;
begin
  Result := Button.UseRightToLeftAlignment;
end;

function TcxButtonCustomLayoutCalculator.GetViewInfo: TcxButtonViewInfo;
begin
  Result := Button.ViewInfo;
end;

{ TcxButtonCommandLinkLayoutCalculator }

procedure TcxButtonCommandLinkLayoutCalculator.Calculate;
begin
  CalculateLayout(ViewInfo.GetContentRect, OptionsImage.Margin, OptionsImage.Spacing);
end;

procedure TcxButtonCommandLinkLayoutCalculator.CalculateLayout(const AContentRect: TRect; AMargin, ASpacing: Integer);

  function GetInternalContentRect: TRect;
  begin
    Result := cxRectContent(AContentRect, Painter.GetCommandLinkMargins);
    if AMargin > -1 then
    begin
      Result.Right := AContentRect.Right - AMargin;
      Result.Left := AContentRect.Left + AMargin;
    end;
  end;

  function GetCaptionRect(AGlyphWidth: Integer): TRect;
  begin
    Result := GetInternalContentRect;
    if AGlyphWidth > 0 then
      Inc(Result.Left, AGlyphWidth + Max(0, ASpacing));
    cxGetTextRect(Result, ViewInfo.Caption, ViewInfo.Font, DT_WORDBREAK or DT_END_ELLIPSIS or Painter.GetBiDiFlags);
  end;

  function GetDescriptionRect(const ACaptionRect: TRect): TRect;
  begin
    Result := GetInternalContentRect;
    Result.Left := ACaptionRect.Left;
    if cxRectHeight(ACaptionRect) > 0 then
      Result.Top := ACaptionRect.Bottom + 2;
  end;

begin
  ViewInfo.FGlyphPos := GetInternalContentRect.TopLeft;
  ViewInfo.FCaptionRect := GetCaptionRect(ViewInfo.ImageSize.cx);
  ViewInfo.FDescriptionRect := GetDescriptionRect(ViewInfo.CaptionRect);

  if UseRightToLeftAlignment then
  begin
    ViewInfo.FCaptionRect := TdxRightToLeftLayoutConverter.ConvertRect(ViewInfo.FCaptionRect, AContentRect);
    ViewInfo.FDescriptionRect := TdxRightToLeftLayoutConverter.ConvertRect(ViewInfo.FDescriptionRect, AContentRect);
    ViewInfo.FGlyphPos := TdxRightToLeftLayoutConverter.ConvertPoint(ViewInfo.FGlyphPos, AContentRect);
    Dec(ViewInfo.FGlyphPos.X, ViewInfo.ImageSize.cx);
  end;
end;

{ TcxButtonOfficeDropDownLayoutCalculator }

procedure TcxButtonOfficeDropDownLayoutCalculator.CalculateDropDownArrowRect;
var
  R: TRect;
begin
  if Layout in [blGlyphLeft, blGlyphRight] then
  begin
    R := ViewInfo.Bounds;
    R := cxRectSetRight(R, R.Right - cxTextOffset, Painter.GetDropDownArrowSize);
  end
  else
  begin
    R := cxRectUnion(ViewInfo.CaptionRect, ViewInfo.DescriptionRect);
    R := cxRectCenterVertically(R, Painter.GetDropDownArrowSize);
    R := cxRectSetLeft(R, R.Right, Painter.GetDropDownArrowSize);
  end;
  if UseRightToLeftAlignment then
    ViewInfo.FDropDownArrowRect := TdxRightToLeftLayoutConverter.ConvertRect(R, ViewInfo.Bounds)
  else
    ViewInfo.FDropDownArrowRect := R;
end;

procedure TcxButtonOfficeDropDownLayoutCalculator.CalculateLayout(R: TRect);
const
  LayoutMap: array[Boolean] of TButtonLayout = (blGlyphRight, blGlyphLeft);
begin
  if Layout in [blGlyphLeft, blGlyphRight] then
  begin
    if UseRightToLeftAlignment then
      Inc(R.Left, Painter.GetDropDownArrowSize + cxTextOffset)
    else
      Dec(R.Right, Painter.GetDropDownArrowSize + cxTextOffset);

    if FMargin < 0 then
    begin
      if Layout <> LayoutMap[UseRightToLeftAlignment] then
        FMargin := ScaleFactor.Apply(7)
      else
        FMargin := 0;
    end;
  end;
  inherited CalculateLayout(R);
end;

procedure TcxButtonOfficeDropDownLayoutCalculator.CalculateTextAreaLayout(const R: TRect);
begin
  ViewInfo.FCaptionRect := cxRectSetHeight(R, CaptionSize.cy);
  ViewInfo.FDescriptionRect := cxRectSetBottom(R, R.Bottom, DescriptionSize.cy);

  case Layout of
    blGlyphTop, blGlyphBottom:
      begin
        ViewInfo.FCaptionRect := cxRectCenterHorizontally(ViewInfo.CaptionRect, CaptionSize.cx);
        ViewInfo.FDescriptionRect := cxRectCenterHorizontally(ViewInfo.DescriptionRect, DescriptionSize.cx);
      end;

    blGlyphLeft:
      begin
        ViewInfo.FCaptionRect := cxRectSetWidth(ViewInfo.CaptionRect, CaptionSize.cx);
        ViewInfo.FDescriptionRect := cxRectSetWidth(ViewInfo.DescriptionRect, DescriptionSize.cx);
      end;

    blGlyphRight:
      begin
        ViewInfo.FCaptionRect := cxRectSetRight(ViewInfo.CaptionRect, ViewInfo.CaptionRect.Right, CaptionSize.cx);
        ViewInfo.FDescriptionRect := cxRectSetRight(ViewInfo.DescriptionRect, ViewInfo.DescriptionRect.Right, DescriptionSize.cx);
      end;
  end;
end;

procedure TcxButtonOfficeDropDownLayoutCalculator.CalculateTextAreaSize(const R: TRect);
var
  ARect: TRect;
  ATextBounds: TRect;
  ATextOffsets: TRect;
begin
  FCaptionSize := cxNullSize;
  FTextAreaSize := cxNullSize;
  FDescriptionSize := cxNullSize;

  if ViewInfo.CanDrawCaption then
  begin
    ATextOffsets := GetTextOffsets;
    ATextBounds := cxRectContent(cxRect(0, 0, cxRectWidth(R), 0), ATextOffsets);
    if Layout in [blGlyphLeft, blGlyphRight] then
      Dec(ATextBounds.Right, GlyphSize.cx + Spacing + Max(Margin, 0));

    ARect := ATextBounds;
    cxGetTextRect(ARect, ViewInfo.Caption, ViewInfo.Font, Painter.GetBiDiFlags or IfThen(Button.WordWrap, DT_WORDBREAK));
    FCaptionSize := cxRectSize(ARect);
    if Layout in [blGlyphLeft, blGlyphRight] then
      FCaptionSize.cx := Min(FCaptionSize.cx, cxRectWidth(ATextBounds));

    ARect := ATextBounds;
    cxGetTextRect(ARect, ViewInfo.Description, ViewInfo.DescriptionFont, Painter.GetBiDiFlags or DT_WORDBREAK);
    FDescriptionSize := cxRectSize(ARect);

    FTextAreaSize := CaptionSize;
    if (DescriptionSize.cx > 0) and (DescriptionSize.cy > 0) then
    begin
      FTextAreaSize.cx := Max(TextAreaSize.cx, DescriptionSize.cx);
      FTextAreaSize.cy := TextAreaSize.cy + cxTextOffset + DescriptionSize.cy;
    end;

    Inc(FTextAreaSize.cx, cxMarginsWidth(ATextOffsets));
    Inc(FTextAreaSize.cy, cxMarginsHeight(ATextOffsets));
  end;
end;

function TcxButtonOfficeDropDownLayoutCalculator.GetTextOffsets: TRect;
begin
  Result := inherited GetTextOffsets;
  if Layout in [blGlyphTop, blGlyphBottom] then
  begin
    Inc(Result.Right, Painter.GetDropDownArrowSize + cxTextOffset);
    Inc(Result.Left, Painter.GetDropDownArrowSize + cxTextOffset);
  end;
end;

function TcxButtonOfficeDropDownLayoutCalculator.GetScaleFactor: TdxScaleFactor;
begin
  Result := Button.ScaleFactor;
end;

{ TcxButtonStandardLayoutCalculator }

procedure TcxButtonStandardLayoutCalculator.Calculate;
begin
  FSpacing := OptionsImage.Spacing;
  FMargin := OptionsImage.Margin;
  CalculateLayout(ViewInfo.GetContentRect);
end;

procedure TcxButtonStandardLayoutCalculator.CalculateDropDownArrowRect;
begin
  if ViewInfo.HasDropDownButton then
    ViewInfo.FDropDownArrowRect := ViewInfo.DropDownButtonRect
  else
    ViewInfo.FDropDownArrowRect := cxNullRect;
end;

procedure TcxButtonStandardLayoutCalculator.CalculateGlyphPosition(const R: TRect);
var
  P: TPoint;
begin
  case Layout of
    blGlyphLeft:
      P.X := Margin + R.Left;
    blGlyphRight:
      P.X := cxRectWidth(R) - Margin - GlyphSize.cx + R.Left;
    blGlyphTop:
      P.Y := Margin + R.Top;
    blGlyphBottom:
      P.Y := cxRectHeight(R) - Margin - GlyphSize.cy + R.Top;
  end;

  if Layout in [blGlyphLeft, blGlyphRight] then
    P.Y := (cxRectHeight(R) - GlyphSize.cy) div 2 + R.Top
  else
    P.X := (cxRectWidth(R) - GlyphSize.cx) div 2 + R.Left;

  GlyphPos := P;
end;

procedure TcxButtonStandardLayoutCalculator.CalculateLayout(R: TRect);
begin
  if ViewInfo.State = cxbsPressed then
    R := cxRectOffset(R, Painter.GetPressedTextShift);

  CalculateTextAreaSize(R);
  CalculateMargins(R, FMargin, FSpacing);
  CalculateGlyphPosition(R);
  CalculateTextAreaLayout(CalculateTextAreaBounds(R));
  CalculateDropDownArrowRect;
end;

procedure TcxButtonStandardLayoutCalculator.CalculateMargins(const R: TRect; var AMargin, ASpacing: Integer);

  function GetMargin(const ASize1, ASize2: TSize; ACoefficient: Integer): Integer;
  begin
    if Layout in [blGlyphLeft, blGlyphRight] then
      Result := (ASize1.cx - ASize2.cx) div ACoefficient
    else
      Result := (ASize1.cy - ASize2.cy) div ACoefficient;
  end;

var
  ASize: TSize;
begin
  if (TextAreaSize.cx = 0) or (GlyphSize.cx = 0) then
    ASpacing := 0;

  if AMargin = -1 then
  begin
    ASize := cxSize(GlyphSize.cx + TextAreaSize.cx, GlyphSize.cy + TextAreaSize.cy);
    if ASpacing = -1 then
    begin
      AMargin := GetMargin(cxRectSize(R), ASize, 3);
      ASpacing := AMargin;
    end
    else
    begin
      Inc(ASize.cx, ASpacing);
      Inc(ASize.cy, ASpacing);
      AMargin := GetMargin(cxRectSize(R), ASize, 2);
    end;
  end
  else
    if ASpacing = -1 then
    begin
      ASize := cxSize(cxRectWidth(R) - (AMargin + GlyphSize.cx), cxRectHeight(R) - (AMargin + GlyphSize.cy));
      ASpacing := GetMargin(ASize, TextAreaSize, 2);
    end;
end;

function TcxButtonStandardLayoutCalculator.CalculateTextAreaBounds(const R: TRect): TRect;
var
  ATextOffsets: TRect;
begin
  ATextOffsets := GetTextOffsets;
  Result := cxRectContent(cxRect(0, 0, cxRectWidth(R), 0), ATextOffsets);
  Result := cxRectSetSize(Result,
    TextAreaSize.cx - cxMarginsWidth(ATextOffsets),
    TextAreaSize.cy - cxMarginsHeight(ATextOffsets));

  case Layout of
    blGlyphLeft:
      Result := cxRectOffsetHorz(Result, GlyphPos.X + GlyphSize.cx + Spacing);
    blGlyphRight:
      Result := cxRectOffsetHorz(Result, GlyphPos.X - Spacing - TextAreaSize.cx);
    blGlyphTop:
      Result := cxRectOffsetVert(Result, GlyphPos.Y + GlyphSize.cy + Spacing);
    blGlyphBottom:
      Result := cxRectOffsetVert(Result, GlyphPos.Y - Spacing - TextAreaSize.cy);
  end;

  if Layout in [blGlyphLeft, blGlyphRight] then
    Result := cxRectOffsetVert(Result, (cxRectHeight(R) - TextAreaSize.cy + cxBtnStdVertTextOffsetCorrection) div 2 + R.Top)
  else
    Result := cxRectOffsetHorz(Result, (cxRectWidth(R) - TextAreaSize.cx) div 2 + R.Left);
end;

procedure TcxButtonStandardLayoutCalculator.CalculateTextAreaLayout(const R: TRect);
begin
  ViewInfo.FCaptionRect := R;
end;

procedure TcxButtonStandardLayoutCalculator.CalculateTextAreaSize(const R: TRect);
var
  ATextOffsets: TRect;
  ATextBounds: TRect;
begin
  if ViewInfo.CanDrawCaption then
  begin
    ATextOffsets := GetTextOffsets;
    ATextBounds := cxRectContent(Rect(0, 0, cxRectWidth(R), 0), ATextOffsets);
    if Layout in [blGlyphLeft, blGlyphRight] then
      Dec(ATextBounds.Right, GlyphSize.cx + Spacing + Max(Margin, 0));
    cxGetTextRect(ATextBounds, ViewInfo.Caption, ViewInfo.Font, Painter.GetBiDiFlags or IfThen(Button.WordWrap, DT_WORDBREAK));
    FTextAreaSize := cxRectSize(ATextBounds);
    Inc(FTextAreaSize.cx, cxMarginsWidth(ATextOffsets));
    Inc(FTextAreaSize.cy, cxMarginsHeight(ATextOffsets));
  end
  else
    FTextAreaSize := cxNullSize;
end;

function TcxButtonStandardLayoutCalculator.GetGlyphPos: TPoint;
begin
  Result := ViewInfo.GlyphPos;
end;

function TcxButtonStandardLayoutCalculator.GetGlyphSize: TSize;
begin
  if ViewInfo.CanDrawGlyph then
    Result := ViewInfo.ImageSize
  else
    Result := cxNullSize;
end;

function TcxButtonStandardLayoutCalculator.GetLayout: TButtonLayout;
begin
  Result := ViewInfo.Layout;
end;

function TcxButtonStandardLayoutCalculator.GetScaleFactor: TdxScaleFactor;
begin
  Result := Button.ScaleFactor;
end;

function TcxButtonStandardLayoutCalculator.GetTextOffsets: TRect;
begin
  if ViewInfo.CanDrawGlyph then
    Result := cxNullRect
  else
    Result := TextRectCorrection;
end;

procedure TcxButtonStandardLayoutCalculator.SetGlyphPos(const Value: TPoint);
begin
  ViewInfo.FGlyphPos := Value;
end;

{ TcxButtonViewInfo }

constructor TcxButtonViewInfo.Create(AOwner: TcxCustomButton);
begin
  inherited Create(AOwner);
  FFont := TFont.Create;
  FDescriptionFont := TFont.Create;
  FPainter := CreatePainter;
end;

destructor TcxButtonViewInfo.Destroy;
begin
  FreeAndNil(FDescriptionFont);
  FreeAndNil(FPainter);
  FreeAndNil(FFont);
  inherited Destroy;
end;

procedure TcxButtonViewInfo.Calculate(const ABounds: TRect);
begin
  FBounds := ABounds;
  FState := Button.GetButtonState;
  FLayout := OptionsImage.Layout;
  CheckLayout(FLayout);
  InitializeFont;
  CalculateButtonParts;
  CalculateContentLayout;
end;

procedure TcxButtonViewInfo.CalculateButtonParts;
begin
  FButtonRect := Bounds;
  if HasDropDownButton then
  begin
    if Button.UseRightToLeftAlignment then
    begin
      FDropDownButtonRect := cxRectSetWidth(Bounds, Painter.GetDropDownButtonSize);
      FButtonRect.Left := FDropDownButtonRect.Right;
      if (Painter.LookAndFeelStyle <> lfsSkin) and not SpeedButtonOptions.NeedDrawParts then
        Dec(FButtonRect.Left, 2);
    end
    else
    begin
      FDropDownButtonRect := cxRectSetRight(Bounds, Bounds.Right, Painter.GetDropDownButtonSize);
      FButtonRect.Right := FDropDownButtonRect.Left;
      if (Painter.LookAndFeelStyle <> lfsSkin) and not SpeedButtonOptions.NeedDrawParts then
        Inc(FButtonRect.Right, 2);
    end;
  end;
end;

procedure TcxButtonViewInfo.CalculateContentLayout;
begin
  FCaptionRect := cxNullRect;
  FDescriptionRect := cxNullRect;
  FDropDownArrowRect := cxNullRect;
  FGlyphPos := cxNullPoint;

  with CreateLayoutCalculator(Kind) do
  try
    Calculate;
  finally
    Free;
  end;
end;

function TcxButtonViewInfo.CanDrawCaption: Boolean;
begin
  Result := ((Button.PaintStyle in [bpsDefault, bpsCaption]) or
    (Kind = cxbkCommandLink)) and (Length(Button.Caption) > 0);
end;

function TcxButtonViewInfo.CanDrawGlyph: Boolean;
begin
  Result := (Button.PaintStyle in [bpsDefault, bpsGlyph]) and ButtonGlyph.IsImageAssigned;
end;

procedure TcxButtonViewInfo.CheckLayout(var ALayout: TButtonLayout);
begin
  if Painter.GetBiDiFlags and DT_RIGHT = DT_RIGHT then
  begin
    if ALayout = blGlyphLeft then
      ALayout := blGlyphRight
    else
      if ALayout = blGlyphRight then
        ALayout := blGlyphLeft;
  end;
end;

function TcxButtonViewInfo.CreatePainter: TcxButtonPainter;
begin
  Result := TcxButtonPainter.Create(Button);
end;

function TcxButtonViewInfo.CreateLayoutCalculator(AKind: TcxButtonKind): TcxButtonCustomLayoutCalculator;
begin
  case AKind of
    cxbkCommandLink:
      Result := TcxButtonCommandLinkLayoutCalculator.Create(Button);
    cxbkOfficeDropDown:
      Result := TcxButtonOfficeDropDownLayoutCalculator.Create(Button);
  else
    Result := TcxButtonStandardLayoutCalculator.Create(Button);
  end;
end;

procedure TcxButtonViewInfo.Draw(ACanvas: TcxCanvas);
begin
  if not Button.StandardButton then
  begin
    Painter.DrawParentBackground(ACanvas, Bounds);
    if not dxFader.DrawFadeImage(Button, ACanvas.Handle, Bounds) then
      DrawBackground(ACanvas);
    DrawContent(ACanvas);
    if NeedDrawFocusRect then
      Painter.DrawFocusRect(ACanvas, ButtonRect);
  end;
end;

procedure TcxButtonViewInfo.DrawBackground(ACanvas: TcxCanvas);
begin
  if HasDropDownButton then
  begin
    Painter.DrawButton(ACanvas, ButtonRect, State, Color, cxbpDropDownLeftPart);
    Painter.DrawButton(ACanvas, DropDownButtonRect, DropDownButtonState, Color, cxbpDropDownRightPart);
  end
  else
    Painter.DrawButton(ACanvas, ButtonRect, State, Color, cxbpButton);
end;

procedure TcxButtonViewInfo.DrawGlyph(ACanvas: TcxCanvas);

  function GetStandardButtonState(AState: TcxButtonState): TButtonState;
  const
    States: array[TcxButtonState] of TButtonState = (bsUp, bsUp, bsUp, bsDown, bsDisabled);
    //cxbsDefault, cxbsNormal, cxbsHot, cxbsPressed, cxbsDisabled;
  begin
    Result := States[AState];
    if (Result = bsDown) and (ButtonGlyph.NumGlyphs < 3) then
      Result := bsUp;
  end;

  procedure InternalDrawGlyph(AStandardButtonState: TButtonState; const AImageBounds: TRect);
  var
    ABuffer: TcxBitmap32;
    AOffset: Integer;
    ASrcRect: TRect;
  begin
    AOffset := Ord(AStandardButtonState);
    if AOffset >= ButtonGlyph.NumGlyphs then
      AOffset := 0;

    ASrcRect := cxRect(cxSize(ButtonGlyph.Glyph.Width div ButtonGlyph.NumGlyphs, ButtonGlyph.Glyph.Height));
    ASrcRect := cxRectOffsetHorz(ASrcRect, AOffset * cxRectWidth(ASrcRect));

    if cxSizeIsEqual(AImageBounds, ASrcRect) then
      ButtonGlyph.Glyph.StretchDraw(ACanvas.Handle, AImageBounds, ASrcRect, MaxByte, GetColorPalette)
    else
    begin
      ABuffer := TcxBitmap32.CreateSize(AImageBounds, True);
      try
        cxBitBlt(ABuffer.Canvas.Handle, ACanvas.Handle, ABuffer.ClientRect, AImageBounds.TopLeft, SRCCOPY);
        ButtonGlyph.Glyph.StretchDraw(ABuffer.Canvas.Handle, ABuffer.ClientRect, ASrcRect, MaxByte, GetColorPalette);
        cxBitBlt(ACanvas.Handle, ABuffer.Canvas.Handle, AImageBounds, cxNullPoint, SRCCOPY);
      finally
        ABuffer.Free;
      end;
    end;
  end;

  procedure InternalDrawImage(AStandardButtonState: TButtonState; const AImageBounds: TRect);
  const
    Mode: array[Boolean] of TcxImageDrawMode = (idmNormal, idmDisabled);
  begin
    cxDrawImage(ACanvas, AImageBounds, ButtonGlyph.Glyph, ButtonGlyph.Images, ButtonGlyph.ImageIndex,
      ifmStretch, Mode[AStandardButtonState = bsDisabled], True, GetColorPalette, ScaleFactor, True);
  end;

begin
  if IsGlyphAssigned(ButtonGlyph.Glyph) and (ButtonGlyph.NumGlyphs > 1) then
    InternalDrawGlyph(GetStandardButtonState(State), cxRectSetOrigin(cxRect(ImageSize), GlyphPos))
  else
    InternalDrawImage(GetStandardButtonState(State), cxRectSetOrigin(cxRect(ImageSize), GlyphPos));
end;

function TcxButtonViewInfo.GetContentRect(AState: TcxButtonState = cxbsDefault): TRect;
var
  ABorderSize: Integer;
begin
  if Kind <> cxbkCommandLink then
    ABorderSize := Painter.LookAndFeelPainter.ButtonBorderSize(AState)
  else
    ABorderSize := 0;

  Result := cxRectInflate(ButtonRect, -ABorderSize, -ABorderSize);
end;

procedure TcxButtonViewInfo.DrawContent(ACanvas: TcxCanvas);
begin
  if NeedDrawDropDownArrow then
    Painter.DrawDropDownArrow(ACanvas, DropDownArrowRect, DropDownButtonState);

  ACanvas.SaveClipRegion;
  try
    ACanvas.IntersectClipRect(GetContentRect(State));

    if CanDrawGlyph then
      DrawGlyph(ACanvas);

    if CanDrawCaption and not cxRectIsEmpty(CaptionRect) then
    begin
      ACanvas.Font := Font;
      Painter.DrawText(ACanvas, Caption, CaptionRect, State, CaptionDrawTextFlags);
    end;

    if not cxRectIsEmpty(DescriptionRect) then
    begin
      ACanvas.Font := DescriptionFont;
      Painter.DrawText(ACanvas, Description, DescriptionRect, State, DescriptionDrawTextFlags);
    end;
  finally
    ACanvas.RestoreClipRegion;
  end;
end;

procedure TcxButtonViewInfo.InitializeFont;
begin
  Font.Assign(Button.Font);
  Font.Color := Painter.GetTextColor(State, Font.Color);
  DescriptionFont.Assign(Font);
  DescriptionFont.Color := Painter.GetDescriptionTextColor(State, DescriptionFont.Color);
  if Kind = cxbkCommandLink then
  begin
    DescriptionFont.Height := MulDiv(Font.Height, 3, 4);
    DescriptionFont.Size := Max(DescriptionFont.Size, 1);
  end;
  FColor := Button.Colors.GetColorByState(State);
  Button.DoGetDrawParams(State, FColor, FFont);
end;

function TcxButtonViewInfo.GetButtonGlyph: TcxButtonGlyph;
begin
  Result := Button.FGlyph;
end;

function TcxButtonViewInfo.GetCaption: string;
begin
  Result := Button.Caption;
end;

function TcxButtonViewInfo.GetColorPalette: IdxColorPalette;
begin
  Result := Button.Painter.ButtonColorPalette(State);
end;

function TcxButtonViewInfo.GetCaptionDrawTextFlags: Integer;
begin
  case Kind of
    cxbkCommandLink:
      Result := DT_END_ELLIPSIS or DT_WORDBREAK;
    cxbkOfficeDropDown:
      Result := DT_VCENTER or IfThen(Layout in [blGlyphTop, blGlyphBottom], DT_CENTER) or IfThen(Button.WordWrap, DT_WORDBREAK);
  else
    Result := DT_CENTER or DT_VCENTER or IfThen(Button.WordWrap, DT_WORDBREAK) or DT_NOCLIP;
  end;
end;

function TcxButtonViewInfo.GetDescription: string;
begin
  Result := Button.Description;
end;

function TcxButtonViewInfo.GetDescriptionDrawTextFlags: Integer;
const
  LayoutMap: array[TButtonLayout] of Integer = (DT_LEFT, DT_RIGHT, DT_CENTER, DT_CENTER);
begin
  Result := DT_WORDBREAK;
  if Kind = cxbkOfficeDropDown then
    Result := Result or LayoutMap[Layout];
end;

function TcxButtonViewInfo.GetImageSize(AScaleFactor: TdxScaleFactor): TSize;
begin
  if ButtonGlyph.IsImageAssigned then
  begin
    if IsGlyphAssigned(ButtonGlyph.Glyph) then
    begin
      dxTestCheck((ButtonGlyph.Glyph.SourceWidth <> 0) and (ButtonGlyph.Glyph.SourceHeight <> 0), 'TcxButtonGlyph.GetImageSize fails');
      Result := Size(ButtonGlyph.Glyph.SourceWidth div ButtonGlyph.NumGlyphs, ButtonGlyph.Glyph.SourceHeight);
      Result := AScaleFactor.Apply(Result, ButtonGlyph.Glyph.SourceDPI, dxDefaultDPI);
    end
    else
      Result := dxGetImageSize(ButtonGlyph.Images, AScaleFactor);
  end
  else
    if FButton.Kind = cxbkCommandLink then
      Result := FButton.Painter.GetScaledCommandLinkGlyphSize(AScaleFactor)
    else
      Result := cxNullSize;
end;

function TcxButtonViewInfo.GetDropDownButtonState: TcxButtonState;
begin
  if bisMenuVisible in Button.InternalStates then
    Result := cxbsPressed
  else
    if State <> cxbsPressed then
      Result := State
    else
      if Button.IsFocused then
        Result := cxbsHot
      else
        Result := cxbsNormal;
end;

function TcxButtonViewInfo.InternalGetImageSize: TSize;
begin
  Result := GetImageSize(ScaleFactor);
end;

function TcxButtonViewInfo.HasDropDownButton: Boolean;
begin
  Result := Kind = cxbkDropDownButton;
end;

function TcxButtonViewInfo.NeedDrawDropDownArrow: Boolean;
begin
  Result := not (cxRectIsEmpty(DropDownArrowRect) or HasDropDownButton and (Painter.LookAndFeelStyle = lfsSkin));
end;

function TcxButtonViewInfo.NeedDrawFocusRect: Boolean;
begin
  Result := Button.CanFocus and Button.ShowFocusRect and Button.Focused and not (bisMenuVisible in Button.InternalStates);
end;

function TcxButtonViewInfo.GetIsDrawOnGlass: Boolean;
begin
  Result := dxIsPaintOnGlass(Button);
end;

function TcxButtonViewInfo.GetKind: TcxButtonKind;
begin
  Result := Button.Kind;
end;

function TcxButtonViewInfo.GetOptionsImage: TcxButtonImageOptions;
begin
  Result := Button.OptionsImage;
end;

function TcxButtonViewInfo.GetScaleFactor: TdxScaleFactor;
begin
  Result := Button.ScaleFactor;
end;

function TcxButtonViewInfo.GetSpeedButtonOptions: TcxSpeedButtonOptions;
begin
  Result := Button.SpeedButtonOptions;
end;

{ TcxButtonPainter }

procedure TcxButtonPainter.DrawButton(ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState;
  AColor: TColor; APart: TcxButtonPart);
begin
  if Button.Kind = cxbkCommandLink then
    DrawCommandLinkButton(ACanvas, R, AState, AColor)
  else
    if Button.SpeedButtonOptions.NeedDrawParts then
      DrawSpeedButton(ACanvas, R, AState, AColor, APart)
    else
      LookAndFeelPainter.DrawScaledButton(ACanvas, R, '', AState,
        ScaleFactor, True, AColor, ACanvas.Font.Color, Button.WordWrap, False, APart);
end;

procedure TcxButtonPainter.DrawText(ACanvas: TcxCanvas; const AText: TCaption; R: TRect; ADrawTextFlags: Integer);
begin
  if ViewInfo.IsDrawOnGlass then
    dxDrawTextOnGlass(ACanvas.Handle, AText, ACanvas.Font, R, ACanvas.Font.Color, ADrawTextFlags, 0, True)
  else
    cxDrawText(ACanvas.Handle, AText, R, ADrawTextFlags)
end;

procedure TcxButtonPainter.DrawText(ACanvas: TcxCanvas;
  const AText: TCaption; R: TRect; AState: TcxButtonState; ADrawTextFlags: Integer);
var
  ABrushStyle: TBrushStyle;
  AFontColor: TColor;
begin
  if (AText <> '') and ACanvas.RectVisible(R) then
  begin
    ADrawTextFlags := ADrawTextFlags or GetBiDiFlags;

    ABrushStyle := ACanvas.Brush.Style;
    try
      ACanvas.Brush.Style := bsClear;
      if (AState = cxbsDisabled) and not (LookAndFeelStyle in [lfsSkin, lfsNative]) then
      begin
        OffsetRect(R, 1, 1);
        AFontColor := ACanvas.Font.Color;
        ACanvas.Font.Color := clBtnHighlight;
        DrawText(ACanvas, AText, R, ADrawTextFlags);
        OffsetRect(R, -1, -1);
        ACanvas.Font.Color := AFontColor;
      end;
      DrawText(ACanvas, AText, R, ADrawTextFlags);
    finally
      ACanvas.Brush.Style := ABrushStyle;
    end;
  end;
end;

procedure TcxButtonPainter.DrawCommandLinkButton(
  ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState; AColor: TColor);
begin
  LookAndFeelPainter.DrawScaledCommandLinkBackground(ACanvas, R, AState, ScaleFactor, AColor);
  if not ViewInfo.ButtonGlyph.IsImageAssigned then
    LookAndFeelPainter.DrawScaledCommandLinkGlyph(ACanvas, ViewInfo.GlyphPos, AState, ScaleFactor);
end;

procedure TcxButtonPainter.DrawDropDownArrow(ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState);
begin
  LookAndFeelPainter.DrawScaledScrollBarArrow(ACanvas, R, AState, adDown, ScaleFactor);
end;

procedure TcxButtonPainter.DrawFocusRect(ACanvas: TcxCanvas; const R: TRect);
begin
  ACanvas.DrawFocusRect(LookAndFeelPainter.ScaledButtonFocusRect(ACanvas, R, ScaleFactor));
end;

procedure TcxButtonPainter.DrawParentBackground(ACanvas: TcxCanvas; const R: TRect);
var
  ATheme: TdxTheme;
begin
  case LookAndFeelPainter.LookAndFeelStyle of
    lfsSkin:
      cxDrawTransparentControlBackground(Button, ACanvas, R);
    lfsNative:
      begin
        ATheme := OpenTheme(totButton);
        if (ATheme <> TC_NONE) and IsThemeBackgroundPartiallyTransparent(ATheme, BP_PUSHBUTTON, PBS_NORMAL) then
          cxDrawThemeParentBackground(Button, ACanvas, R);
      end;
  end;
end;

procedure TcxButtonPainter.DrawSpeedButton(ACanvas: TcxCanvas;
  R: TRect; AState: TcxButtonState; AColor: TColor; APart: TcxButtonPart);

  function IsDrawBorder: Boolean;
  begin
    Result := (LookAndFeelStyle <> lfsSkin) and (not Button.SpeedButtonOptions.Flat or
      Button.IsDesigning or ((AState <> cxbsDisabled) and ((bisHot in Button.InternalStates) or (AState <> cxbsNormal))));
    Result := Result and not ((LookAndFeelStyle = lfsNative) or (LookAndFeelStyle = lfsOffice11));
  end;

  function IsNotTransparent: Boolean;
  begin
    Result := not Button.SpeedButtonOptions.Transparent or
      ((AState <> cxbsDisabled) and
      (not (bisHot in Button.InternalStates) and (AState <> cxbsNormal)) or
      ((bisHot in Button.InternalStates) and LookAndFeelPainter.IsButtonHotTrack));
  end;

begin
  if IsDrawBorder then
  begin
    LookAndFeelPainter.DrawButtonBorder(ACanvas, R, AState);
    InflateRect(R,
      -LookAndFeelPainter.ButtonBorderSize(AState),
      -LookAndFeelPainter.ButtonBorderSize(AState));
  end;

  ACanvas.SaveClipRegion;
  try
    ACanvas.SetClipRegion(TcxRegion.Create(R), roSet);
    if IsNotTransparent then
      LookAndFeelPainter.DrawScaledButton(ACanvas, R, '', AState, ScaleFactor, False, AColor, ACanvas.Font.Color, Button.WordWrap, True, APart)
    else
      cxDrawTransparentControlBackground(Button, ACanvas, ViewInfo.Bounds);
  finally
    ACanvas.RestoreClipRegion;
  end;
end;

function TcxButtonPainter.GetBiDiFlags: Integer;
begin
  Result := Button.DrawTextBiDiModeFlags(0);
end;

function TcxButtonPainter.GetCommandLinkGlyphSize: TSize;
begin
  Result := LookAndFeelPainter.GetScaledCommandLinkGlyphSize(ScaleFactor);
end;

function TcxButtonPainter.GetCommandLinkMargins: TRect;
begin
  Result := LookAndFeelPainter.GetScaledCommandLinkMargins(ScaleFactor);
end;

function TcxButtonPainter.GetDescriptionTextColor(AState: TcxButtonState; ADefaultColor: TColor): TColor;
begin
  Result := LookAndFeelPainter.ButtonDescriptionTextColor(AState, ADefaultColor);
end;

function TcxButtonPainter.GetDropDownArrowSize: Integer;
begin
  Result := GetDropDownButtonSize;
end;

function TcxButtonPainter.GetDropDownButtonSize: Integer;
begin
  Result := LookAndFeelPainter.GetScaledDropDownButtonRightPartSize(ScaleFactor);
end;

function TcxButtonPainter.GetLookAndFeelPainter: TcxCustomLookAndFeelPainter;
begin
  Result := Button.Painter;
end;

function TcxButtonPainter.GetLookAndFeelStyle: TcxLookAndFeelStyle;
begin
  if Button.LookAndFeel.SkinPainter <> nil then
    Result := lfsSkin
  else
    if Button.LookAndFeel.NativeStyle then
      Result := lfsNative
    else
      Result := cxLookAndFeelStyleMap[Button.LookAndFeel.Kind];
end;

function TcxButtonPainter.GetPressedTextShift: TPoint;
begin
  Result := cxPoint(
    LookAndFeelPainter.ScaledButtonTextShift(ScaleFactor),
    LookAndFeelPainter.ScaledButtonTextShift(ScaleFactor));
end;

function TcxButtonPainter.GetTextColor(AState: TcxButtonState; ADefaultColor: TColor): TColor;
begin
  Result := Button.Colors.GetTextColorByState(AState);
  if Result = clDefault then
  begin
    if Button.Kind = cxbkCommandLink then
      Result := LookAndFeelPainter.DefaultCommandLinkTextColor(AState, ADefaultColor)
    else
      Result := LookAndFeelPainter.ButtonSymbolColor(AState, ADefaultColor);
  end;
end;

function TcxButtonPainter.GetScaleFactor: TdxScaleFactor;
begin
  Result := Button.ScaleFactor;
end;

function TcxButtonPainter.GetViewInfo: TcxButtonViewInfo;
begin
  Result := Button.ViewInfo;
end;

end.
