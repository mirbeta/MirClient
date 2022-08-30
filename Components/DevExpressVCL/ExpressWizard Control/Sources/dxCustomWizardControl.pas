{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressWizardControl                                     }
{                                                                    }
{           Copyright (c) 2012-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSWIZARDCONTROL AND ALL          }
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

unit dxCustomWizardControl;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Windows, SysUtils, Messages, Classes, Graphics, Controls, Math, Forms, Buttons,
  ImgList, dxCore, cxClasses, cxControls, cxGraphics, cxGeometry, cxDrawTextUtils,
  cxLookAndFeels, cxLookAndFeelPainters, dxWizardControlStrs, dxGDIPlusClasses,
  dxAnimation, dxFading, dxSkinsCore, dxCoreClasses;

const
  dxWizardControlDefaultButtonHeight = 25;
  dxWizardControlDefaultButtonWidth = 75;
  dxWizardControlMinHeight: Integer = 422;
  dxWizardControlMinWidth: Integer = 600;

type
  TdxCustomWizardControl = class;
  TdxWizardControlButtons = class;
  TdxWizardControlButtonViewInfo = class;
  TdxWizardControlCellViewInfo = class;
  TdxWizardControlController = class;
  TdxWizardControlCustomButton = class;
  TdxWizardControlCustomButtonCollection = class;
  TdxWizardControlCustomButtonCollectionItem = class;
  TdxWizardControlCustomButtons = class;
  TdxWizardControlCustomPage = class;
  TdxWizardControlCustomPageClass = class of TdxWizardControlCustomPage;
  TdxWizardControlHeader = class;
  TdxWizardControlInfoPanel = class;
  TdxWizardControlInfoPanelViewInfo = class;
  TdxWizardControlPageHeader = class;

  TdxWizardControlChange = (wccContent, wccLayout, wccStruct);
  TdxWizardControlChanges = set of TdxWizardControlChange;

  TdxWizardControlButtonKind = (wcbkBack, wcbkNext, wcbkCancel, wcbkHelp, wcbkFinish);
  TdxWizardControlCustomButtonsAlignment = (wccbaRight, wccbaLeft);
  TdxWizardControlElementVisibility = (wcevDefault, wcevAlwaysVisible, wcevAlwaysHidden);
  TdxWizardControlResizeAnimation = (wcraDefault, wcraEnabled, wcraDisabled);
  TdxWizardControlTransitionEffect = (wcteDefault, wcteNone, wcteFade, wcteSlide);
  TdxWizardControlViewStyle = (wcvsWizard97, wcvsAero);

  { Events procedures }

  TdxWizardControlButtonClickEvent = procedure (Sender: TObject; AKind: TdxWizardControlButtonKind;
    var AHandled: Boolean) of object;
  TdxWizardControlInfoPanelClickEvent = procedure (Sender: TObject; var AHandled: Boolean) of object;
  TdxWizardControlPageChangingEvent = procedure (Sender: TObject;
    ANewPage: TdxWizardControlCustomPage; var AAllow: Boolean) of object;
  TdxWizardControlHandleChildControlKeyEvent = procedure (const Message: TCMChildKey; var AHandled: Boolean) of object;

  { IdxWizardControlSelectableItem }

  IdxWizardControlSelectableItem = interface
  ['{D5E058AB-1C90-4D21-BE0A-EB48530EF53B}']
    procedure SelectionChanged;
  end;

  { IdxWizardControlFormHelper }

  IdxWizardControlFormHelper = interface
  ['{7DA6D67B-F19A-4D53-A6CB-C55EF65245D7}']
    procedure LayoutChanged;
    function ProcessHitTest(var Message: TWMNCHitTest): Boolean;
  end;

  { EdxWizardControlException }

  EdxWizardControlException = class(EdxException);

  { TdxWizardControlCustomObject }

  TdxWizardControlCustomObject = class
  strict private
    FWizardControl: TdxCustomWizardControl;

    function GetScaleFactor: TdxScaleFactor;
  public
    constructor Create(AWizardControl: TdxCustomWizardControl); virtual;

    property ScaleFactor: TdxScaleFactor read GetScaleFactor;
    property WizardControl: TdxCustomWizardControl read FWizardControl;
  end;

  { TdxWizardControlCustomPersistent }

  TdxWizardControlChangeEvent = procedure (Sender: TObject; AChanges: TdxWizardControlChanges) of object;

  TdxWizardControlCustomPersistent = class(TcxOwnedPersistent)
  private
    FChanges: TdxWizardControlChanges;
    FLockCount: Integer;
    FOnChange: TdxWizardControlChangeEvent;
  protected
    procedure Changed(AChanges: TdxWizardControlChanges);
    procedure ChangeScale(M, D: Integer); virtual;
    procedure DoChanged(AChanges: TdxWizardControlChanges); virtual;

    property OnChange: TdxWizardControlChangeEvent read FOnChange write FOnChange;
  public
    procedure Assign(Source: TPersistent); override;
    procedure BeginUpdate;
    procedure EndUpdate;
  end;

  { TdxWizardControlHitTest }

  TdxWizardControlHitTest = class(TdxWizardControlCustomObject)
  private
    FCursor: TCursor;
    FHitObject: TdxWizardControlCellViewInfo;
    FHitPoint: TPoint;
    function GetHitAtButton: Boolean;
    function GetHitAtCommandArea: Boolean;
    function GetHitAtHeader: Boolean;
    function GetHitAtInfoPanel: Boolean;
    function GetHitAtTitle: Boolean;
    function GetHitAtWatermark: Boolean;
    function GetHitObjectAsButton: TdxWizardControlCustomButton;
    procedure SetHitPoint(const AValue: TPoint);
  public
    procedure Calculate(const APoint: TPoint);

    property Cursor: TCursor read FCursor write FCursor;
    property HitAtButton: Boolean read GetHitAtButton;
    property HitAtCommandArea: Boolean read GetHitAtCommandArea;
    property HitAtHeader: Boolean read GetHitAtHeader;
    property HitAtInfoPanel: Boolean read GetHitAtInfoPanel;
    property HitAtTitle: Boolean read GetHitAtTitle;
    property HitAtWatermark: Boolean read GetHitAtWatermark;
    property HitObject: TdxWizardControlCellViewInfo read FHitObject write FHitObject;
    property HitObjectAsButton: TdxWizardControlCustomButton read GetHitObjectAsButton;
    property HitPoint: TPoint read FHitPoint write SetHitPoint;
  end;

  { TdxWizardControlCustomPainter }

  TdxWizardControlCustomPainter = class(TdxWizardControlCustomObject)
  private
    FHeaderDescriptionFont: TFont;
    FHeaderTitleFont: TFont;
    function GetIsFadingAvailable: Boolean;
    function GetIsSkinUsed: Boolean;
    function GetLookAndFeelPainter: TcxCustomLookAndFeelPainter;
  protected
    function GetCommandAreaDisabledTextColor: TColor; virtual;
    function GetCommandAreaTextColor: TColor; virtual;
    function GetHeaderDescriptionOffset: Integer; virtual;
    function GetHeaderDescriptionTextColor: TColor; virtual;
    function GetHeaderTitleTextColor: TColor; virtual;
    function GetHyperlinkColor: TColor; virtual;
    function GetSeparatorSize: Integer; virtual;
    function GetTitleTextColor: TColor; virtual;
    procedure InitializeFonts; virtual;

    property LookAndFeelPainter: TcxCustomLookAndFeelPainter read GetLookAndFeelPainter;
  public
    constructor Create(AWizardControl: TdxCustomWizardControl); override;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure DrawBackground(ACanvas: TcxCanvas; const ABounds: TRect); virtual;
    procedure DrawButton(ACanvas: TcxCanvas; const ABounds: TRect; AState: TcxButtonState);
    procedure DrawCommandAreaBackground(ACanvas: TcxCanvas; const ABounds: TRect); virtual;
    procedure DrawHeaderBackground(ACanvas: TcxCanvas; const ABounds: TRect); virtual;
    procedure DrawSeparator(ACanvas: TcxCanvas; const R: TRect); virtual;
    procedure DrawTitleBackground(ACanvas: TcxCanvas; const R: TRect); virtual;
    function GetButtonFocusRect(ACanvas: TcxCanvas; const ABounds: TRect): TRect; virtual;
    function GetButtonOffset(AButtonState: TcxButtonState): TPoint; virtual;
    function GetButtonTextColor(AButtonState: TcxButtonState): TColor; virtual;

    property CommandAreaDisabledTextColor: TColor read GetCommandAreaDisabledTextColor;
    property CommandAreaTextColor: TColor read GetCommandAreaTextColor;
    property HeaderDescriptionFont: TFont read FHeaderDescriptionFont;
    property HeaderDescriptionOffset: Integer read GetHeaderDescriptionOffset;
    property HeaderDescriptionTextColor: TColor read GetHeaderDescriptionTextColor;
    property HeaderTitleFont: TFont read FHeaderTitleFont;
    property HeaderTitleTextColor: TColor read GetHeaderTitleTextColor;
    property HyperlinkColor: TColor read GetHyperlinkColor;
    property IsFadingAvailable: Boolean read GetIsFadingAvailable;
    property IsSkinUsed: Boolean read GetIsSkinUsed;
    property SeparatorSize: Integer read GetSeparatorSize;
    property TitleTextColor: TColor read GetTitleTextColor;
  end;

  { TdxWizardControlImage }

  TdxWizardControlImage = class(TdxWizardControlCustomPersistent)
  strict private
    FImage: TdxSkinGlyph;

    function GetEmpty: Boolean;
    function GetHeight: Integer;
    function GetImage: TdxSmartGlyph;
    function GetMargins: TcxMargin;
    function GetStretch: TdxSkinStretchMode;
    function GetWidth: Integer;
    procedure ImageChanged(Sender: TObject);
    procedure SetImage(AValue: TdxSmartGlyph);
    procedure SetMargins(AValue: TcxMargin);
    procedure SetStretch(const Value: TdxSkinStretchMode);
  protected
    procedure DoAssign(Source: TPersistent); override;
    procedure Reset; virtual;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure Draw(ACanvas: TcxCanvas; const ARect: TRect); overload;
    procedure Draw(ACanvas: TcxCanvas; const ARect: TRect; AScaleFactor: TdxScaleFactor); overload;

    property Empty: Boolean read GetEmpty;
    property Height: Integer read GetHeight;
    property Width: Integer read GetWidth;
  published
    property Image: TdxSmartGlyph read GetImage write SetImage;
    property Margins: TcxMargin read GetMargins write SetMargins;
    property Stretch: TdxSkinStretchMode read GetStretch write SetStretch default smStretch;
  end;

  { TdxWizardControlHeader }

  TdxWizardControlHeaderAssignedValue = (wchvBackgroundImage, wchvDescriptionFont,
    wchvDescriptionVisibility, wchvDescriptionOffset, wchvGlyph, wchvGlyphVisibility, wchvTitleFont, wchvVisible);
  TdxWizardControlHeaderAssignedValues = set of TdxWizardControlHeaderAssignedValue;

  TdxWizardControlHeader = class(TdxWizardControlCustomPersistent)
  private
    FAssignedValues: TdxWizardControlHeaderAssignedValues;
    FBackgroundImage: TdxWizardControlImage;
    FDescriptionFont: TFont;
    FDescriptionOffset: Integer;
    FDescriptionVisibility: TdxWizardControlElementVisibility;
    FGlyph: TdxSmartGlyph;
    FGlyphVisibility: TdxWizardControlElementVisibility;
    FTitleFont: TFont;
    FVisible: TdxWizardControlElementVisibility;
    function CheckResetValue(AValue: TdxWizardControlHeaderAssignedValue;
      const ANewAssignedValues: TdxWizardControlHeaderAssignedValues): Boolean;
    function IsBackgroundImageStored: Boolean;
    function IsDescriptionFontStored: Boolean;
    function IsDescriptionOffsetStored: Boolean;
    function IsDescriptionVisibilityStored: Boolean;
    function IsGlyphStored: Boolean;
    function IsGlyphVisibilityStored: Boolean;
    function IsTitleFontStored: Boolean;
    function IsVisibleStored: Boolean;
    procedure BackgroundImageChanged(Sender: TObject; AChanges: TdxWizardControlChanges);
    procedure DescriptionFontChanged(Sender: TObject);
    procedure GlyphChanged(Sender: TObject);
    procedure TitleFontChanged(Sender: TObject);
    //
    procedure SetAssignedValues(AValue: TdxWizardControlHeaderAssignedValues);
    procedure SetBackgroundImage(AValue: TdxWizardControlImage);
    procedure SetDescriptionFont(AValue: TFont);
    procedure SetDescriptionOffset(const AValue: Integer);
    procedure SetDescriptionVisibility(AValue: TdxWizardControlElementVisibility);
    procedure SetGlyph(AValue: TdxSmartGlyph);
    procedure SetGlyphVisibility(AValue: TdxWizardControlElementVisibility);
    procedure SetTitleFont(AValue: TFont);
    procedure SetVisible(AValue: TdxWizardControlElementVisibility);
  protected
    procedure ChangeScale(M, D: Integer); override;
    procedure DoAssign(Source: TPersistent); override;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
  published
    property AssignedValues: TdxWizardControlHeaderAssignedValues read FAssignedValues write SetAssignedValues default [];
    property BackgroundImage: TdxWizardControlImage read FBackgroundImage write SetBackgroundImage stored IsBackgroundImageStored;
    property DescriptionFont: TFont read FDescriptionFont write SetDescriptionFont stored IsDescriptionFontStored;
    property DescriptionOffset: Integer read FDescriptionOffset write SetDescriptionOffset stored IsDescriptionOffsetStored;
    property DescriptionVisibility: TdxWizardControlElementVisibility read FDescriptionVisibility write SetDescriptionVisibility stored IsDescriptionVisibilityStored;
    property Glyph: TdxSmartGlyph read FGlyph write SetGlyph stored IsGlyphStored;
    property GlyphVisibility: TdxWizardControlElementVisibility read FGlyphVisibility write SetGlyphVisibility stored IsGlyphVisibilityStored;
    property TitleFont: TFont read FTitleFont write SetTitleFont stored IsTitleFontStored;
    property Visible: TdxWizardControlElementVisibility read FVisible write SetVisible stored IsVisibleStored default wcevDefault;
  end;

  { TdxWizardControlTitle }

  TdxWizardControlTitle = class(TdxWizardControlCustomPersistent)
  private
    FFont: TFont;
    FFontChanged: Boolean;
    FGlyph: TdxSmartGlyph;
    FText: string;
    procedure ChangeHandler(Sender: TObject);
    function IsFontStored: Boolean;
    procedure SetFont(AValue: TFont);
    procedure SetGlyph(AValue: TdxSmartGlyph);
    procedure SetText(const AValue: string);
  protected
    procedure ChangeScale(M, D: Integer); override;
    procedure DoAssign(Source: TPersistent); override;
    procedure InitializeFont; virtual;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
  published
    property Font: TFont read FFont write SetFont stored IsFontStored;
    property Glyph: TdxSmartGlyph read FGlyph write SetGlyph;
    property Text: string read FText write SetText;
  end;

  { TdxWizardControlWatermark }

  TdxWizardControlWatermarkAssignedValue = (wcwavBackgroundImage, wcwavVisibility);
  TdxWizardControlWatermarkAssignedValues = set of TdxWizardControlWatermarkAssignedValue;

  TdxWizardControlWatermark = class(TdxWizardControlCustomPersistent)
  private
    FAssignedValues: TdxWizardControlWatermarkAssignedValues;
    FBackgroundImage: TdxWizardControlImage;
    FVisibility: TdxWizardControlElementVisibility;
    function CheckResetValue(AValue: TdxWizardControlWatermarkAssignedValue;
      const ANewAssignedValues: TdxWizardControlWatermarkAssignedValues): Boolean;
    procedure BackgroundImageChanged(Sender: TObject; AChanges: TdxWizardControlChanges);
    procedure SetAssignedValues(AValue: TdxWizardControlWatermarkAssignedValues);
    procedure SetBackgroundImage(AValue: TdxWizardControlImage);
    procedure SetVisibility(AValue: TdxWizardControlElementVisibility);
  protected
    procedure DoAssign(Source: TPersistent); override;
    function IsVisibilityStored: Boolean; virtual;
    //
    property AssignedValues: TdxWizardControlWatermarkAssignedValues read FAssignedValues write SetAssignedValues;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
  published
    property BackgroundImage: TdxWizardControlImage read FBackgroundImage write SetBackgroundImage;
    property Visibility: TdxWizardControlElementVisibility read FVisibility write SetVisibility stored IsVisibilityStored;
  end;

  { TdxWizardControlPageWatermark }

  TdxWizardControlPageWatermark = class(TdxWizardControlWatermark)
  protected
    function IsBackgroundImageStored: Boolean;
    function IsVisibilityStored: Boolean; override;
  published
    property AssignedValues default [];
    property BackgroundImage stored IsBackgroundImageStored;
  end;

  { TdxWizardControlInfoPanel }

  TdxWizardControlInfoPanel = class(TdxWizardControlCustomPersistent)
  private
    FCaption: string;
    FEnabled: Boolean;
    FFont: TFont;
    FFontChanged: Boolean;
    FHyperlink: string;
    FHyperlinkColor: TColor;
    FVisible: Boolean;
    function IsFontStored: Boolean;
    procedure FontChangeHandler(Sender: TObject);
    procedure SetCaption(const AValue: string);
    procedure SetEnabled(const AValue: Boolean);
    procedure SetFont(AValue: TFont);
    procedure SetHyperlink(const AValue: string);
    procedure SetHyperlinkColor(const Value: TColor);
    procedure SetVisible(const AValue: Boolean);
  protected
    procedure ChangeScale(M, D: Integer); override;
    procedure DoAssign(Source: TPersistent); override;
    procedure InitializeFont; virtual;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
  published
    property Caption: string read FCaption write SetCaption;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property Font: TFont read FFont write SetFont stored IsFontStored;
    property Hyperlink: string read FHyperlink write SetHyperlink;
    property HyperlinkColor: TColor read FHyperlinkColor write SetHyperlinkColor default clDefault;
    property Visible: Boolean read FVisible write SetVisible default True;
  end;

  { TdxWizardControlPageHeader }

  TdxWizardControlPageHeader = class(TdxWizardControlHeader)
  private
    FDescription: string;
    FTitle: string;
    function IsDescriptionStored: Boolean;
    function IsTitleStored: Boolean;
    procedure ReadDescription(Reader: TReader);
    procedure ReadTitle(Reader: TReader);
    procedure SetDescription(const AValue: string);
    procedure SetTitle(const AValue: string);
    procedure WriteDescription(Writer: TWriter);
    procedure WriteTitle(Writer: TWriter);
  protected
    function GetDefaultDescription: string;
    function GetDefaultTitle: string;
    procedure DefineProperties(Filer: TFiler); override;
    procedure DoAssign(Source: TPersistent); override;
  public
    constructor Create(AOwner: TPersistent); override;
  published
    property Description: string read FDescription write SetDescription stored IsDescriptionStored;
    property Title: string read FTitle write SetTitle stored IsTitleStored;
  end;

  { TdxWizardControlCustomButton }

  TdxWizardControlCustomButton = class(TdxWizardControlCustomPersistent)
  private
    FCaption: string;
    FCaptionIsDefault: Boolean;
    FEnabled: Boolean;
    FGlyph: TdxSmartGlyph;
    FGlyphAlignment: TButtonLayout;
    FImageIndex: TcxImageIndex;
    FVisible: Boolean;
    FWidth: Integer;

    FOnClick: TNotifyEvent;

    function IsCaptionStored: Boolean;
    function GetButtons: TdxWizardControlButtons;
    function GetFocused: Boolean;
    function GetWizardControl: TdxCustomWizardControl;
    procedure GlyphChangeHandler(Sender: TObject);
    procedure SetCaption(const AValue: string);
    procedure SetEnabled(const AValue: Boolean);
    procedure SetFocused(const AValue: Boolean);
    procedure SetGlyph(AValue: TdxSmartGlyph);
    procedure SetGlyphAlignment(const AValue: TButtonLayout);
    procedure SetImageIndex(AValue: TcxImageIndex);
    procedure SetVisible(const AValue: Boolean);
    procedure SetWidth(const AValue: Integer);
  protected
    function GetActuallyEnabled: Boolean; virtual;
    function GetDefaultCaption: string; virtual;
    procedure ChangeScale(M: Integer; D: Integer); override;
    procedure DoAssign(Source: TPersistent); override;
    procedure DoClick; virtual;
    //
    property ActuallyEnabled: Boolean read GetActuallyEnabled;
    property CaptionIsDefault: Boolean read FCaptionIsDefault;
    property Visible: Boolean read FVisible write SetVisible default True;
    property WizardControl: TdxCustomWizardControl read GetWizardControl;
    // Events
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;

    procedure Click; virtual;

    property Buttons: TdxWizardControlButtons read GetButtons;
    property Focused: Boolean read GetFocused write SetFocused;
  published
    property Caption: string read FCaption write SetCaption stored IsCaptionStored;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property Glyph: TdxSmartGlyph read FGlyph write SetGlyph;
    property GlyphAlignment: TButtonLayout read FGlyphAlignment write SetGlyphAlignment default blGlyphLeft;
    property ImageIndex: TcxImageIndex read FImageIndex write SetImageIndex default -1;
    property Width: Integer read FWidth write SetWidth default 0;
  end;

  { TdxWizardControlBackButton }

  TdxWizardControlBackButton = class(TdxWizardControlCustomButton)
  protected
    function GetActuallyEnabled: Boolean; override;
    function GetDefaultCaption: string; override;
    procedure DoClick; override;
  end;

  { TdxWizardControlCancelButton }

  TdxWizardControlCancelButton = class(TdxWizardControlCustomButton)
  protected
    function GetDefaultCaption: string; override;
    procedure DoClick; override;
  published
    property Visible;
  end;

  { TdxWizardControlFinishButton }

  TdxWizardControlFinishButton = class(TdxWizardControlCustomButton)
  private
    FAlwaysVisible: Boolean;
    procedure SetAlwaysVisible(const AValue: Boolean);
  protected
    function GetDefaultCaption: string; override;
    procedure DoAssign(Source: TPersistent); override;
    procedure DoClick; override;
  published
    property AlwaysVisible: Boolean read FAlwaysVisible write SetAlwaysVisible default False;
  end;

  { TdxWizardControlHelpButton }

  TdxWizardControlHelpButton = class(TdxWizardControlCustomButton)
  protected
    function GetDefaultCaption: string; override;
    procedure DoClick; override;
  published
    property Visible;
  end;

  { TdxWizardControlNextButton }

  TdxWizardControlNextButton = class(TdxWizardControlCustomButton)
  protected
    function GetActuallyEnabled: Boolean; override;
    function GetDefaultCaption: string; override;
    procedure DoClick; override;
  end;

  { TdxWizardControlOptionsButtons }

  TdxWizardControlOptionsButtons = class(TdxWizardControlCustomPersistent)
  private
    FEnabled: Boolean;
    FHeight: Integer;
    FWidth: Integer;
    procedure SetEnabled(const AValue: Boolean);
    procedure SetHeight(const AValue: Integer);
    procedure SetWidth(const AValue: Integer);
  protected
    procedure ChangeScale(M: Integer; D: Integer); override;
    procedure DoAssign(Source: TPersistent); override;
  public
    constructor Create(AOwner: TPersistent); override;
  published
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property Height: Integer read FHeight write SetHeight default 0;
    property Width: Integer read FWidth write SetWidth default 0;
  end;

  { TdxWizardControlCustomButtonCollectionItem }

  TdxWizardControlCustomButtonCollectionItem = class(TCollectionItem)
  private
    FButton: TdxWizardControlCustomButton;

    function GetCaption: string;
    function GetEnabled: Boolean;
    function GetFocused: Boolean;
    function GetGlyph: TdxSmartGlyph;
    function GetGlyphAlignment: TButtonLayout;
    function GetImageIndex: TcxImageIndex;
    function GetOnClick: TNotifyEvent;
    function GetVisible: Boolean;
    function GetWidth: Integer;
    procedure SetCaption(const AValue: string);
    procedure SetEnabled(const AValue: Boolean);
    procedure SetFocused(const AValue: Boolean);
    procedure SetGlyph(AValue: TdxSmartGlyph);
    procedure SetGlyphAlignment(const AValue: TButtonLayout);
    procedure SetImageIndex(AValue: TcxImageIndex);
    procedure SetOnClick(AValue: TNotifyEvent);
    procedure SetVisible(const AValue: Boolean);
    procedure SetWidth(const AValue: Integer);
  protected
    procedure ChangeScale(M: Integer; D: Integer); virtual;

    property Button: TdxWizardControlCustomButton read FButton;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
    procedure Click; virtual;

    property Focused: Boolean read GetFocused write SetFocused;
  published
    property Caption: string read GetCaption write SetCaption;
    property Enabled: Boolean read GetEnabled write SetEnabled default True;
    property Glyph: TdxSmartGlyph read GetGlyph write SetGlyph;
    property GlyphAlignment: TButtonLayout read GetGlyphAlignment write SetGlyphAlignment default blGlyphLeft;
    property ImageIndex: TcxImageIndex read GetImageIndex write SetImageIndex default -1;
    property Visible: Boolean read GetVisible write SetVisible default True;
    property Width: Integer read GetWidth write SetWidth default 0;
    // Events
    property OnClick: TNotifyEvent read GetOnClick write SetOnClick;
  end;

  { TdxWizardControlCustomButtonCollection }

  TdxWizardControlCustomButtonCollection = class(TOwnedCollection)
  private
    function GetCustomButtons: TdxWizardControlCustomButtons;
    function GetItem(Index: Integer): TdxWizardControlCustomButtonCollectionItem;
    procedure SetItem(Index: Integer; AValue: TdxWizardControlCustomButtonCollectionItem);
  protected
    procedure ChangeScale(M: Integer; D: Integer); virtual;
    procedure Update(Item: TCollectionItem); override;

    property CustomButtons: TdxWizardControlCustomButtons read GetCustomButtons;
  public
    function Add: TdxWizardControlCustomButtonCollectionItem;

    property Items[Index: Integer]: TdxWizardControlCustomButtonCollectionItem read GetItem write SetItem; default;
  end;

  { TdxWizardControlCustomButtons }

  TdxWizardControlCustomButtons = class(TdxWizardControlCustomPersistent)
  private
    FAlignment: TdxWizardControlCustomButtonsAlignment;
    FButtons: TdxWizardControlCustomButtonCollection;

    function GetOwnerButtons: TdxWizardControlButtons;
    procedure SetAlignment(const AValue: TdxWizardControlCustomButtonsAlignment);
    procedure SetButtons(AValue: TdxWizardControlCustomButtonCollection);
  protected
    procedure ChangeScale(M: Integer; D: Integer); override;
    procedure DoAssign(Source: TPersistent); override;

    property OwnerButtons: TdxWizardControlButtons read GetOwnerButtons;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
  published
    property Alignment: TdxWizardControlCustomButtonsAlignment read FAlignment write SetAlignment default wccbaRight;
    property Buttons: TdxWizardControlCustomButtonCollection read FButtons write SetButtons;
  end;

  { TdxWizardControlButtons }

  TdxWizardControlButtons = class(TdxWizardControlCustomPersistent)
  private
    FBack: TdxWizardControlBackButton;
    FCancel: TdxWizardControlCancelButton;
    FCommon: TdxWizardControlOptionsButtons;
    FCustomButtons: TdxWizardControlCustomButtons;
    FFinish: TdxWizardControlFinishButton;
    FFreeNotificator: TcxFreeNotificator;
    FHelp: TdxWizardControlHelpButton;
    FImages: TCustomImageList;
    FImageChangeLink: TChangeLink;
    FNext: TdxWizardControlNextButton;
    procedure ChangeHandler(Sender: TObject; AChanges: TdxWizardControlChanges);
    procedure ImageListChange(Sender: TObject);
    procedure SetBack(AValue: TdxWizardControlBackButton);
    procedure SetCancel(AValue: TdxWizardControlCancelButton);
    procedure SetCommon(AValue: TdxWizardControlOptionsButtons);
    procedure SetCustomButtons(AValue: TdxWizardControlCustomButtons);
    procedure SetFinish(AValue: TdxWizardControlFinishButton);
    procedure SetHelp(AValue: TdxWizardControlHelpButton);
    procedure SetImages(AValue: TCustomImageList);
    procedure SetNext(AValue: TdxWizardControlNextButton);
  protected
    procedure ChangeScale(M: Integer; D: Integer); override;
    procedure DoAssign(Source: TPersistent); override;
    procedure FreeNotification(Sender: TComponent); virtual;
    procedure UpdateTranslation; virtual;

    property FreeNotificator: TcxFreeNotificator read FFreeNotificator;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
  published
    property Back: TdxWizardControlBackButton read FBack write SetBack;
    property Cancel: TdxWizardControlCancelButton read FCancel write SetCancel;
    property Common: TdxWizardControlOptionsButtons read FCommon write SetCommon;
    property CustomButtons: TdxWizardControlCustomButtons read FCustomButtons write SetCustomButtons;
    property Finish: TdxWizardControlFinishButton read FFinish write SetFinish;
    property Help: TdxWizardControlHelpButton read FHelp write SetHelp;
    property Images: TCustomImageList read FImages write SetImages;
    property Next: TdxWizardControlNextButton read FNext write SetNext;
  end;

  { TdxWizardControlOptionsAnimate }

  TdxWizardControlOptionsAnimate = class(TdxWizardControlCustomPersistent)
  private
    FResizeAnimation: TdxWizardControlResizeAnimation;
    FTransitionEffect: TdxWizardControlTransitionEffect;
  protected
    procedure DoAssign(Source: TPersistent); override;
  published
    property ResizeAnimation: TdxWizardControlResizeAnimation read FResizeAnimation write FResizeAnimation default wcraDefault;
    property TransitionEffect: TdxWizardControlTransitionEffect read FTransitionEffect write FTransitionEffect default wcteDefault;
  end;

  { TdxWizardControlOptionsViewStyleAero }

  TdxWizardControlOptionsViewStyleAero = class(TdxWizardControlCustomPersistent)
  private
    FEnableTitleAero: Boolean;
    FTitle: TdxWizardControlTitle;
    procedure ChangeHandler(Sender: TObject; AChanges: TdxWizardControlChanges);
    procedure SetEnableTitleAero(AValue: Boolean);
    procedure SetTitle(AValue: TdxWizardControlTitle);
  protected
    procedure ChangeScale(M, D: Integer); override;
    procedure DoAssign(Source: TPersistent); override;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
  published
    property EnableTitleAero: Boolean read FEnableTitleAero write SetEnableTitleAero default True;
    property Title: TdxWizardControlTitle read FTitle write SetTitle;
  end;

  { TdxWizardControlCellViewInfo }

  TdxWizardControlCellViewInfoClass = class of TdxWizardControlCellViewInfo;
  TdxWizardControlCellViewInfo = class(TdxWizardControlCustomObject)
  private
    FBounds: TRect;
    function GetController: TdxWizardControlController;
    function GetPainter: TdxWizardControlCustomPainter;
    function GetVisible: Boolean;
    procedure SetBounds(const ABounds: TRect);
  protected
    function CanDrawContent: Boolean; virtual;
    procedure DoRightToLeftConversion; virtual;
    procedure DrawBackground(ACanvas: TcxCanvas); virtual;
    procedure DrawChildren(ACanvas: TcxCanvas); virtual;
    procedure DrawContent(ACanvas: TcxCanvas); virtual;
    function GetRightToLeftConvertedRect(const ARect: TRect): TRect; virtual;
    function IsFocused: Boolean; virtual;
    function IsHot: Boolean; virtual;
    function IsPressed: Boolean; virtual;
    function ProcessDialogChar(ACharCode: Word): Boolean; virtual;
  public
    procedure BeforeDestruction; override;
    procedure Calculate; virtual;
    function CalculateHitTest(AHitTest: TdxWizardControlHitTest): Boolean; virtual;
    function CanFocus: Boolean; virtual;
    procedure Draw(ACanvas: TcxCanvas);
    procedure Invalidate; virtual;
    function MeasureHeight: Integer; virtual;
    function MeasureWidth: Integer; virtual;
    procedure RefreshState; virtual;

    property Bounds: TRect read FBounds write SetBounds;
    property Controller: TdxWizardControlController read GetController;
    property Painter: TdxWizardControlCustomPainter read GetPainter;
    property Visible: Boolean read GetVisible;
  end;

  { TdxWizardControlCellViewInfoList }

  TdxWizardControlCellViewInfoList = class(TcxObjectList)
  private
    function GetItem(Index: Integer): TdxWizardControlCellViewInfo;
  public
    function Add(ACustomCellViewInfo: TdxWizardControlCellViewInfo): Integer;
    function CalculateHitTest(AHitTest: TdxWizardControlHitTest): Boolean;
    function MaxMeasureHeight: Integer;
    function MaxMeasureWidth: Integer;
    function ProcessDialogChar(ACharCode: Word): Boolean;
    procedure Draw(ACanvas: TcxCanvas);
    procedure RefreshState;

    property Items[Index: Integer]: TdxWizardControlCellViewInfo read GetItem; default;
  end;

  { TdxWizardControlContainerCellViewInfo }

  TdxWizardControlContainerCellViewInfo = class(TdxWizardControlCellViewInfo)
  private
    FCellList: TdxWizardControlCellViewInfoList;
  protected
    function AddCell(ACellClass: TdxWizardControlCellViewInfoClass): TdxWizardControlCellViewInfo;
    function ProcessDialogChar(ACharCode: Word): Boolean; override;
    procedure DrawChildren(ACanvas: TcxCanvas); override;
  public
    constructor Create(AWizardControl: TdxCustomWizardControl); override;
    destructor Destroy; override;
    function CalculateHitTest(AHitTest: TdxWizardControlHitTest): Boolean; override;
    procedure RefreshState; override;
    //
    property CellList: TdxWizardControlCellViewInfoList read FCellList;
  end;

  { TdxWizardControlButtonFadingHelper }

  TdxWizardControlButtonFadingHelper = class(TdxFadingObjectHelper)
  private
    FButtonViewInfo: TdxWizardControlButtonViewInfo;
  protected
    function CanFade: Boolean; override;
    procedure DrawFadeImage; override;
    procedure GetFadingImages(out AFadeOutImage, AFadeInImage: TcxBitmap); override;

    property ButtonViewInfo: TdxWizardControlButtonViewInfo read FButtonViewInfo;
  public
    constructor Create(AViewInfo: TdxWizardControlButtonViewInfo); virtual;
  end;

  { TdxWizardControlButtonViewInfo }

  TdxWizardControlButtonViewInfoClass = class of TdxWizardControlButtonViewInfo;
  TdxWizardControlButtonViewInfo = class(TdxWizardControlCellViewInfo)
  private
    FButton: TdxWizardControlCustomButton;
    FFadingHelper: TdxWizardControlButtonFadingHelper;
    FState: TcxButtonState;
    function GetFont: TFont;
    function GetGlyphSize: TSize;
    procedure SetState(const AState: TcxButtonState);
  protected
    FCaptionRect: TRect;
    FGlyphRect: TRect;
    procedure CalculateCaptionRect; virtual;
    procedure CalculateGlyphRect; virtual;
    function CalculateState: TcxButtonState; virtual;
    procedure DoRightToLeftConversion; override;
    procedure DrawButtonBackground(ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState); virtual;
    procedure DrawContent(ACanvas: TcxCanvas); override;
    function GetMargins: TRect; virtual;
    function IsDefault: Boolean; virtual;
    function IsImageAvailable: Boolean;
    function ProcessDialogChar(ACharCode: Word): Boolean; override;

    property Button: TdxWizardControlCustomButton read FButton;
    property FadingHelper: TdxWizardControlButtonFadingHelper read FFadingHelper;
  public
    constructor Create(AControl: TdxCustomWizardControl; AButton: TdxWizardControlCustomButton); reintroduce; virtual;
    destructor Destroy; override;
    procedure Calculate; override;
    function CanFocus: Boolean; override;
    function IsFadingAvailable: Boolean; virtual;
    function MeasureHeight: Integer; override;
    function MeasureWidth: Integer; override;
    procedure RefreshState; override;

    property CaptionRect: TRect read FCaptionRect;
    property Font: TFont read GetFont;
    property GlyphRect: TRect read FGlyphRect;
    property GlyphSize: TSize read GetGlyphSize;
    property Margins: TRect read GetMargins;
    property State: TcxButtonState read FState write SetState;
  end;

  { TdxWizardControlInfoPanelViewInfo }

  TdxWizardControlInfoPanelViewInfo = class(TdxWizardControlCellViewInfo)
  private
    FInfoPanel: TdxWizardControlInfoPanel;
    FState: TcxButtonState;
    function GetFont: TFont;
    function GetFontStyle: TFontStyles;
    function GetIsHyperlinkMode: Boolean;
    function GetTextColor: TColor;
    procedure SetState(const AState: TcxButtonState);
  protected
    procedure DrawContent(ACanvas: TcxCanvas); override;
  public
    constructor Create(AControl: TdxCustomWizardControl); override;
    function CalculateHitTest(AHitTest: TdxWizardControlHitTest): Boolean; override;
    function MeasureHeight: Integer; override;
    function MeasureWidth: Integer; override;
    procedure RefreshState; override;

    property Font: TFont read GetFont;
    property FontStyle: TFontStyles read GetFontStyle;
    property InfoPanel: TdxWizardControlInfoPanel read FInfoPanel;
    property IsHyperlinkMode: Boolean read GetIsHyperlinkMode;
    property State: TcxButtonState read FState write SetState;
    property TextColor: TColor read GetTextColor;
  end;

  { TdxWizardControlCustomCommandAreaViewInfo }

  TdxWizardControlCustomCommandAreaViewInfo = class(TdxWizardControlContainerCellViewInfo)
  private
    FCancelButtonViewInfo: TdxWizardControlButtonViewInfo;
    FCustomButtonViewInfos: TList;
    FFinishButtonViewInfo: TdxWizardControlButtonViewInfo;
    FHelpButtonViewInfo: TdxWizardControlButtonViewInfo;
    FInfoPanelViewInfo: TdxWizardControlCellViewInfo;
    FNextButtonViewInfo: TdxWizardControlButtonViewInfo;
    function GetActualNextButtonViewInfo: TdxWizardControlButtonViewInfo;
    function GetButtons: TdxWizardControlButtons;
    function GetCustomButtonViewInfos(Index: Integer): TdxWizardControlButtonViewInfo;
    function GetCustomButtonViewInfosCount: Integer;
    function GetHasCustomButtonsAlignedLeft: Boolean;
    function GetHasCustomButtonsAlignedRight: Boolean;
  protected
    FButtonsMaxMeasureHeight: Integer;

    function AddButtonCell(AButton: TdxWizardControlCustomButton;
      AViewInfoClass: TdxWizardControlButtonViewInfoClass): TdxWizardControlButtonViewInfo;
    function AlignButtonToRightSide(AButtonViewInfo: TdxWizardControlButtonViewInfo; var R: TRect): Boolean;
    function MeasureButtonWidth(AButtonViewInfo: TdxWizardControlButtonViewInfo; AIndent: Integer): Integer;

    procedure CalculateButtons(var ABounds: TRect); virtual;
    procedure CalculateCustomButtons(var ABounds: TRect); virtual;
    procedure CalculateInfoPanel(var ABounds: TRect); virtual;
    procedure CalculateButtonsMaxMeasureHeight; virtual;
    procedure DoRightToLeftConversion; override;
    procedure DrawBackground(ACanvas: TcxCanvas); override;
    function GetButtonViewInfo(AButton: TdxWizardControlCustomButton): TdxWizardControlButtonViewInfo; virtual;
    function GetContentMargins: TRect; virtual; abstract;
    function GetIndentBetweenElementsInGroup: Integer; virtual; abstract;
    function GetIndentBetweenGroups: Integer; virtual; abstract;

    property ActualNextButtonViewInfo: TdxWizardControlButtonViewInfo read GetActualNextButtonViewInfo;
    property ButtonsMaxMeasureHeight: Integer read FButtonsMaxMeasureHeight;
    property CancelButtonViewInfo: TdxWizardControlButtonViewInfo read FCancelButtonViewInfo;
    property CustomButtonViewInfos[Index: Integer]: TdxWizardControlButtonViewInfo read GetCustomButtonViewInfos;
    property CustomButtonViewInfosCount: Integer read GetCustomButtonViewInfosCount;
    property FinishButtonViewInfo: TdxWizardControlButtonViewInfo read FFinishButtonViewInfo;
    property HasCustomButtonsAlignedLeft: Boolean read GetHasCustomButtonsAlignedLeft;
    property HasCustomButtonsAlignedRight: Boolean read GetHasCustomButtonsAlignedRight;
    property HelpButtonViewInfo: TdxWizardControlButtonViewInfo read FHelpButtonViewInfo;
    property InfoPanelViewInfo: TdxWizardControlCellViewInfo read FInfoPanelViewInfo;
    property NextButtonViewInfo: TdxWizardControlButtonViewInfo read FNextButtonViewInfo;
  public
    constructor Create(AWizardControl: TdxCustomWizardControl); override;
    destructor Destroy; override;

    procedure Calculate; override;
    function MeasureHeight: Integer; override;
    function MeasureWidth: Integer; override;

    property Buttons: TdxWizardControlButtons read GetButtons;
    property ContentMargins: TRect read GetContentMargins;
    property IndentBetweenElementsInGroup: Integer read GetIndentBetweenElementsInGroup;
    property IndentBetweenGroups: Integer read GetIndentBetweenGroups;
  end;

  { TdxWizardControlCustomHeaderViewInfo }

  TdxWizardControlCustomHeaderViewInfo = class(TdxWizardControlCellViewInfo)
  private
    function GetBackgroundImage: TdxWizardControlImage;
    function GetDescriptionFont: TFont;
    function GetDescriptionOffset: Integer;
    function GetGlyph: TdxSmartImage;
    function GetGlyphSize: TSize;
    function GetHeader: TdxWizardControlPageHeader;
    function GetTitleFont: TFont;
  protected
    FDescriptionRect: TRect;
    FGlyphRect: TRect;
    FTitleRect: TRect;

    procedure DoRightToLeftConversion; override;
    procedure DrawBackground(ACanvas: TcxCanvas); override;
    procedure DrawContent(ACanvas: TcxCanvas); override;
    function GetActualSettings(AValue: TdxWizardControlHeaderAssignedValue;
      out AActualSettings: TdxWizardControlHeader): Boolean; virtual;
    function GetBackgroundImageIsAvailable: Boolean; virtual;
    function GetContentMargins: TRect; virtual; abstract;
    function GetDescriptionIsVisible: Boolean; virtual; abstract;
    function GetGlyphIsVisible: Boolean; virtual; abstract;
    function GetIndentBetweenElements: Integer; virtual; abstract;
    function GetIsVisible: Boolean; virtual;

    property Header: TdxWizardControlPageHeader read GetHeader;
  public
    procedure Calculate; override;
    function MeasureHeight: Integer; override;
    function MeasureWidth: Integer; override;

    property BackgroundImage: TdxWizardControlImage read GetBackgroundImage;
    property BackgroundImageIsAvailable: Boolean read GetBackgroundImageIsAvailable;
    property ContentMargins: TRect read GetContentMargins;
    property DescriptionFont: TFont read GetDescriptionFont;
    property DescriptionIsVisible: Boolean read GetDescriptionIsVisible;
    property DescriptionOffset: Integer read GetDescriptionOffset;
    property DescriptionRect: TRect read FDescriptionRect;
    property Glyph: TdxSmartImage read GetGlyph;
    property GlyphIsVisible: Boolean read GetGlyphIsVisible;
    property GlyphRect: TRect read FGlyphRect;
    property GlyphSize: TSize read GetGlyphSize;
    property IndentBetweenElements: Integer read GetIndentBetweenElements;
    property TitleFont: TFont read GetTitleFont;
    property TitleRect: TRect read FTitleRect;
  end;

  { TdxWizardControlCustomTitleViewInfo }

  TdxWizardControlCustomTitleViewInfo = class(TdxWizardControlContainerCellViewInfo)
  private
    FIsPaintOnGlass: Boolean;
    function GetFont: TFont;
    function GetTitle: TdxWizardControlTitle;
  protected
    procedure DrawBackground(ACanvas: TcxCanvas); override;
  public
    property IsPaintOnGlass: Boolean read FIsPaintOnGlass write FIsPaintOnGlass;
    property Title: TdxWizardControlTitle read GetTitle;
    property Font: TFont read GetFont;
  end;

  { TdxWizardControlWatermarkViewInfo }

  TdxWizardControlWatermarkViewInfo = class(TdxWizardControlCellViewInfo)
  private
    function GetWatermarkImage: TdxWizardControlImage;
    function GetWatermarkVisibility: TdxWizardControlElementVisibility;
  protected
    function GetIsImageAvailable: Boolean; virtual;
    function GetIsImageVisible: Boolean; virtual; abstract;
    procedure DrawBackground(ACanvas: TcxCanvas); override;
    procedure DrawContent(ACanvas: TcxCanvas); override;
  public
    function MeasureHeight: Integer; override;
    function MeasureWidth: Integer; override;

    property IsImageAvailable: Boolean read GetIsImageAvailable;
    property IsImageVisible: Boolean read GetIsImageVisible;
    property WatermarkImage: TdxWizardControlImage read GetWatermarkImage;
    property WatermarkVisibility: TdxWizardControlElementVisibility read GetWatermarkVisibility;
  end;

  { TdxWizardControlViewInfo }

  TdxWizardControlViewInfo = class(TdxWizardControlContainerCellViewInfo)
  private
    FContentBounds: TRect;
    FCommandAreaViewInfo: TdxWizardControlCustomCommandAreaViewInfo;
    FHeaderViewInfo: TdxWizardControlCellViewInfo;
    FPainter: TdxWizardControlCustomPainter;
    FTitleViewInfo: TdxWizardControlCustomTitleViewInfo;
    FWatermarkViewInfo: TdxWizardControlWatermarkViewInfo;
  protected
    FPageArea: TRect;
    procedure CalculateCells(var ABounds: TRect); virtual;
    procedure CalculateCommandArea(var ABounds: TRect); virtual;
    procedure CalculateHeader(var ABounds: TRect); virtual;
    procedure CalculateTitleViewInfo(var ABounds: TRect); virtual;
    procedure CalculateWatermark(var ABounds: TRect); virtual;
    function CreateCommandAreaViewInfo: TdxWizardControlCustomCommandAreaViewInfo; virtual; abstract;
    function CreateHeaderViewInfo: TdxWizardControlCustomHeaderViewInfo; virtual; abstract;
    function CreatePainter: TdxWizardControlCustomPainter; virtual; abstract;
    function CreateTitleViewInfo: TdxWizardControlCustomTitleViewInfo; virtual;
    function CreateWatermarkViewInfo: TdxWizardControlWatermarkViewInfo; virtual; abstract;
    procedure CellsCreate; virtual;
    procedure CellsFree; virtual;
    procedure DoRightToLeftConversion; override;
    procedure DrawBackground(ACanvas: TcxCanvas); override;
    // Animation
    function GetDefaultResizeAnimation: TdxWizardControlResizeAnimation; virtual; abstract;
    function GetDefaultTransitionEffect: TdxWizardControlTransitionEffect; virtual; abstract;
    function GetTransitionEffectAreaBounds: TRect; virtual;
  public
    constructor Create(AWizardControl: TdxCustomWizardControl); override;
    destructor Destroy; override;
    procedure Calculate; override;
    procedure GetTabOrderList(AList: TdxWizardControlCellViewInfoList); virtual;
    function MeasureHeight: Integer; override;
    function MeasureWidth: Integer; override;
    procedure RecreateCells;

    property ContentBounds: TRect read FContentBounds;
    property PageArea: TRect read FPageArea;
    property Painter: TdxWizardControlCustomPainter read FPainter;
    //
    property CommandAreaViewInfo: TdxWizardControlCustomCommandAreaViewInfo read FCommandAreaViewInfo;
    property HeaderViewInfo: TdxWizardControlCellViewInfo read FHeaderViewInfo;
    property TitleViewInfo: TdxWizardControlCustomTitleViewInfo read FTitleViewInfo;
    property WatermarkViewInfo: TdxWizardControlWatermarkViewInfo read FWatermarkViewInfo;
    //
    property DefaultResizeAnimation: TdxWizardControlResizeAnimation read GetDefaultResizeAnimation;
    property DefaultTransitionEffect: TdxWizardControlTransitionEffect read GetDefaultTransitionEffect;
    property TransitionEffectAreaBounds: TRect read GetTransitionEffectAreaBounds;
  end;

  { TdxWizardControlController }

  TdxWizardControlController = class(TdxWizardControlCustomObject)
  strict private
    FFocusedCell: TdxWizardControlCellViewInfo;
    FHitTest: TdxWizardControlHitTest;
    FHotCell: TdxWizardControlCellViewInfo;
    FPressedCell: TdxWizardControlCellViewInfo;
    FPrevFocusedCell: TdxWizardControlCellViewInfo;

    function GetViewInfo: TdxWizardControlViewInfo;
    procedure SetHotCell(AValue: TdxWizardControlCellViewInfo);
    procedure SetPressedCell(AValue: TdxWizardControlCellViewInfo);
    procedure SetFocusedCell(AValue: TdxWizardControlCellViewInfo);
  protected
    procedure CalculateHitTest(X, Y: Integer); virtual;
    procedure CellRemoving(ACell: TdxWizardControlCellViewInfo); virtual;
    procedure ProcessAccel(ACell: TdxWizardControlCellViewInfo); virtual;
    procedure ProcessClick(ACell: TdxWizardControlCellViewInfo); virtual;
    procedure ProcessInfoPanelClick(AInfoPanel: TdxWizardControlInfoPanel); virtual;
  public
    constructor Create(AWizardControl: TdxCustomWizardControl); override;
    destructor Destroy; override;
    procedure RefreshState; virtual;
    // Focus
    function FindNextFocusableCell(ACell: TdxWizardControlCellViewInfo; AGoForward: Boolean): TdxWizardControlCellViewInfo;
    procedure FocusKill; virtual;
    procedure FocusNextCell(AGoForward: Boolean);
    procedure FocusSet; virtual;
    procedure ValidateFocusedCell;
    // Keyboard
    function ProcessChildKey(AKey: Word; AShiftState: TShiftState): Boolean; virtual;
    function ProcessDialogChar(ACharCode: Word): Boolean; virtual;
    procedure KeyDown(AKey: Word; AShiftState: TShiftState); virtual;
    procedure KeyUp(AKey: Word; AShiftState: TShiftState); virtual;
    // Mouse
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure MouseLeave(AControl: TControl); virtual;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); virtual;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;

    property FocusedCell: TdxWizardControlCellViewInfo read FFocusedCell write SetFocusedCell;
    property HitTest: TdxWizardControlHitTest read FHitTest;
    property HotCell: TdxWizardControlCellViewInfo read FHotCell write SetHotCell;
    property PressedCell: TdxWizardControlCellViewInfo read FPressedCell write SetPressedCell;
    property ViewInfo: TdxWizardControlViewInfo read GetViewInfo;
  end;

  { TdxWizardControlDesignSelectorHelper }

  TdxWizardControlDesignSelectorHelper = class(TdxControlsDesignSelectorHelper)
  protected
    function IsHitTestTransparent(const P: TPoint): Boolean; override;
  end;

  { TdxWizardControlAnimationController }

  TdxWizardControlAnimationControllerStage = (wctacsPrepare, wctacsTransition, wctacsResizing);

  TdxWizardControlAnimationController = class(TdxWizardControlCustomObject)
  private
    FActive: Boolean;
    FAnimatedRect: TRect;
    FAnimation: TdxImageAnimationTransition;
    FSuppressContentDrawing: Boolean;
    function GetActualResizeAnimation: Boolean;
    function GetActualTransitionEffect: TdxWizardControlTransitionEffect;
    function GetAutoSize: Boolean;
    function GetViewInfo: TdxWizardControlViewInfo;
    procedure AnimationHandler(Sender: TdxAnimationTransition; var APosition: Integer; var AFinished: Boolean);
  protected
    FStage: TdxWizardControlAnimationControllerStage;
    function CalculateAnimationMode(ANewPage, APrevPage: TdxWizardControlCustomPage): TdxDrawAnimationMode;
    function PrepareBlankBitmap: TcxBitmap;
    procedure PrepareForAnimation;
    function PreparePageBitmap(APage: TdxWizardControlCustomPage): TcxBitmap;
    procedure ProcessMultiStageAnimation(ANewPage: TdxWizardControlCustomPage;
      APrevPage: TdxWizardControlCustomPage; AAnimationMode: TdxDrawAnimationMode);
    procedure ProcessSingleStageAnimation(ANewPage: TdxWizardControlCustomPage;
      APrevPage: TdxWizardControlCustomPage; AAnimationMode: TdxDrawAnimationMode);
    procedure RedrawArea(const ARect: TRect);
    procedure RunAnimation(AStartImage, AFinishImage: TBitmap; ATime: Cardinal; AMode: TdxDrawAnimationMode);
    //
    property AutoSize: Boolean read GetAutoSize;
  public
    procedure AnimateTransition(ANewPage, APrevPage: TdxWizardControlCustomPage);
    procedure Draw(ACanvas: TcxCanvas);
    //
    property Active: Boolean read FActive;
    property ActualResizeAnimation: Boolean read GetActualResizeAnimation;
    property ActualTransitionEffect: TdxWizardControlTransitionEffect read GetActualTransitionEffect;
    property AnimatedRect: TRect read FAnimatedRect;
    property Animation: TdxImageAnimationTransition read FAnimation;
    property Stage: TdxWizardControlAnimationControllerStage read FStage;
    property SuppressContentDrawing: Boolean read FSuppressContentDrawing;
    property ViewInfo: TdxWizardControlViewInfo read GetViewInfo;
  end;

  { TdxCustomWizardControl }

  TdxCustomWizardControl = class(TcxControl, IdxSkinSupport)
  private
    FActivePage: TdxWizardControlCustomPage;
    FAnimationController: TdxWizardControlAnimationController;
    FButtons: TdxWizardControlButtons;
    FController: TdxWizardControlController;
    FHeader: TdxWizardControlHeader;
    FInfoPanel: TdxWizardControlInfoPanel;
    FOptionsAnimate: TdxWizardControlOptionsAnimate;
    FOptionsViewStyleAero: TdxWizardControlOptionsViewStyleAero;
    FPages: TList;
    FViewInfo: TdxWizardControlViewInfo;
    FViewStyle: TdxWizardControlViewStyle;
    FWatermark: TdxWizardControlWatermark;

    FOnButtonClick: TdxWizardControlButtonClickEvent;
    FOnHandleChildControlKey: TdxWizardControlHandleChildControlKeyEvent;
    FOnInfoPanelClick: TdxWizardControlInfoPanelClickEvent;
    FOnPageChanged: TNotifyEvent;
    FOnPageChanging: TdxWizardControlPageChangingEvent;

    function GetActivePageIndex: Integer;
    function GetHitTest: TdxWizardControlHitTest;
    function GetIsResizingAnimationActive: Boolean;
    function GetPage(APageIndex: Integer): TdxWizardControlCustomPage;
    function GetPageCount: Integer;
    procedure ChangeHandler(Sender: TObject; AChanges: TdxWizardControlChanges);
    procedure InternalSetActivePage(APage: TdxWizardControlCustomPage);
    procedure SetActivePage(APage: TdxWizardControlCustomPage);
    procedure SetActivePageIndex(AValue: Integer);
    procedure SetButtons(AValue: TdxWizardControlButtons);
    procedure SetHeader(AValue: TdxWizardControlHeader);
    procedure SetInfoPanel(AValue: TdxWizardControlInfoPanel);
    procedure SetOptionsAnimate(AValue: TdxWizardControlOptionsAnimate);
    procedure SetOptionsViewStyleAero(AValue: TdxWizardControlOptionsViewStyleAero);
    procedure SetViewStyle(AValue: TdxWizardControlViewStyle);
    procedure SetWatermark(AValue: TdxWizardControlWatermark);
    // Messages
    procedure CMChildKey(var Message: TCMChildKey); message CM_CHILDKEY;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMTabStopChanged(var Message: TMessage); message CM_TABSTOPCHANGED;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
  protected
    FFormHelper: IdxWizardControlFormHelper;

    procedure AdjustClientRect(var Rect: TRect); override;
    procedure AlignControls(AControl: TControl; var Rect: TRect); override;
    procedure BoundsChanged; override;
    procedure CalculateAutoSize(var AWidth, AHeight: Integer); virtual;
    function CanActivatePage(APage: TdxWizardControlCustomPage): Boolean; virtual;
    function CanAutoSize(var ANewWidth, ANewHeight: Integer): Boolean; override;
    function CanUseTransitionEffect(ANewPage, APrevPage: TdxWizardControlCustomPage): Boolean; virtual;
    procedure Changed(AChanges: TdxWizardControlChanges); virtual;
    procedure ChangeScaleEx(M, D: Integer; IsDpiChange: Boolean); override;
    function CreateAnimationController: TdxWizardControlAnimationController; virtual;
    function CreateController: TdxWizardControlController; virtual;
    function CreateOptionsAnimate: TdxWizardControlOptionsAnimate; virtual;
    function CreateOptionsButtons: TdxWizardControlButtons; virtual;
    function CreateOptionsHeader: TdxWizardControlHeader; virtual;
    function CreateOptionsInfoPanel: TdxWizardControlInfoPanel; virtual;
    function CreateOptionsViewStyleAero: TdxWizardControlOptionsViewStyleAero; virtual;
    function CreateOptionsWatermark: TdxWizardControlWatermark; virtual;
    function CreateViewInfo: TdxWizardControlViewInfo; virtual;
    procedure CreateHandle; override;
    procedure DoPaint; override;
    procedure EraseBackground(DC: HDC); override;
    function GetDesignHitTest(X, Y: Integer; Shift: TShiftState): Boolean; override;
    function IsDoubleBufferedNeeded: Boolean; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure LayoutChanged; virtual;
    procedure Loaded; override;
    procedure LookAndFeelChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseLeave(AControl: TControl); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    function NeedsScrollBars: Boolean; override;
    procedure RecreateViewInfo; virtual;
    procedure SelectActivePage;
    procedure SetChildOrder(Child: TComponent; Order: Integer); override;
    procedure UpdatePagesVisibility; virtual;
    procedure ValidateActivePage; virtual;
    procedure ValidateInsert(AComponent: TComponent); override;

    // Events
    function DoButtonClick(AButtonKind: TdxWizardControlButtonKind): Boolean; virtual;
    function DoHandleChildControlKey(const Message: TCMChildKey): Boolean; virtual;
    function DoInfoPanelClick: Boolean; virtual;
    procedure DoPageChanged; virtual;
    function DoPageChanging(NewPage: TdxWizardControlCustomPage): Boolean; virtual;

    procedure PageAdded(APage: TdxWizardControlCustomPage); virtual;
    procedure PageRemoving(APage: TdxWizardControlCustomPage); virtual;

    property AnimationController: TdxWizardControlAnimationController read FAnimationController;
    property Controller: TdxWizardControlController read FController;
    property FormHelper: IdxWizardControlFormHelper read FFormHelper;
    property IsResizingAnimationActive: Boolean read GetIsResizingAnimationActive;
    property ViewInfo: TdxWizardControlViewInfo read FViewInfo;

    property OnButtonClick: TdxWizardControlButtonClickEvent read FOnButtonClick write FOnButtonClick;
    property OnHandleChildControlKey: TdxWizardControlHandleChildControlKeyEvent read FOnHandleChildControlKey write FOnHandleChildControlKey;
    property OnInfoPanelClick: TdxWizardControlInfoPanelClickEvent read FOnInfoPanelClick write FOnInfoPanelClick;
    property OnPageChanged: TNotifyEvent read FOnPageChanged write FOnPageChanged;
    property OnPageChanging: TdxWizardControlPageChangingEvent read FOnPageChanging write FOnPageChanging;
  public
    constructor Create(AComponent: TComponent); override;
    destructor Destroy; override;
    function AddPage(APageClass: TdxWizardControlCustomPageClass): TdxWizardControlCustomPage; overload; virtual;
    procedure DeletePage(APage: TdxWizardControlCustomPage); overload; virtual;
    procedure FullRefresh;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    function CanGoToNextPage: Boolean; virtual;
    function CanGoToPrevPage: Boolean; virtual;
    function GetNextPage(ACurrentPage: TdxWizardControlCustomPage; ASearchForward: Boolean): TdxWizardControlCustomPage;
    procedure GoToNextPage; virtual;
    procedure GoToPrevPage; virtual;
    // IdxLocalizerListener
    procedure TranslationChanged; override;

    property ActivePage: TdxWizardControlCustomPage read FActivePage write SetActivePage;
    property ActivePageIndex: Integer read GetActivePageIndex write SetActivePageIndex;
    property Align default alClient;
    property Buttons: TdxWizardControlButtons read FButtons write SetButtons;
    property Header: TdxWizardControlHeader read FHeader write SetHeader;
    property HitTest: TdxWizardControlHitTest read GetHitTest;
    property InfoPanel: TdxWizardControlInfoPanel read FInfoPanel write SetInfoPanel;
    property OptionsAnimate: TdxWizardControlOptionsAnimate read FOptionsAnimate write SetOptionsAnimate;
    property OptionsViewStyleAero: TdxWizardControlOptionsViewStyleAero read FOptionsViewStyleAero write SetOptionsViewStyleAero;
    property PageCount: Integer read GetPageCount;
    property Pages[Index: Integer]: TdxWizardControlCustomPage read GetPage;
    property ViewStyle: TdxWizardControlViewStyle read FViewStyle write SetViewStyle default wcvsWizard97;
    property Watermark: TdxWizardControlWatermark read FWatermark write SetWatermark;
  end;

  { TdxWizardControlOptionsSize }

  TdxWizardControlOptionsSize = class(TdxWizardControlCustomPersistent)
  private
    FMinHeight: Integer;
    FMinWidth: Integer;
    procedure SetMinHeight(AValue: Integer);
    procedure SetMinWidth(AValue: Integer);
  protected
    procedure DoAssign(Source: TPersistent); override;
  published
    property MinHeight: Integer read FMinHeight write SetMinHeight default 0;
    property MinWidth: Integer read FMinWidth write SetMinWidth default 0;
  end;

  { TdxWizardControlCustomPage }

  TdxWizardControlCustomPage = class(TcxControl, IdxWizardControlSelectableItem)
  private
    FDesignSelector: TdxWizardControlDesignSelectorHelper;
    FHeader: TdxWizardControlPageHeader;
    FOptionsSize: TdxWizardControlOptionsSize;
    FPageVisible: Boolean;
    FWatermark: TdxWizardControlPageWatermark;

    function GetActive: Boolean;
    function GetAlign: TAlign;
    function GetPageIndex: Integer;
    function GetWizardControl: TdxCustomWizardControl;
    procedure ChangeHandler(Sender: TObject; AChanges: TdxWizardControlChanges);
    procedure SetAlign(AValue: TAlign);
    procedure SetHeader(AValue: TdxWizardControlPageHeader);
    procedure SetOptionsSize(AValue: TdxWizardControlOptionsSize);
    procedure SetPageIndex(AValue: Integer);
    procedure SetPageVisible(AValue: Boolean);
    procedure SetWatermark(AValue: TdxWizardControlPageWatermark);
    procedure SetWizardControl(AValue: TdxCustomWizardControl);
  protected
    FDesignHelper: IcxDesignHelper;

    procedure AlignControls(AControl: TControl; var Rect: TRect); override;
    procedure BoundsChanged; override;
    procedure CalculateAutoSize(var AWidth, AHeight: Integer); virtual;
    procedure Changed(AChanges: TdxWizardControlChanges); virtual;
    procedure DoPaint; override;
    function NeedsScrollBars: Boolean; override;
    procedure SetParent(AParent: TWinControl); override;
    procedure UpdateDesignSelectorBounds;
    // DesignTime
    function IsObjectSelected: Boolean;
    procedure SelectObject;
    // IdxWizardControlSelectableItem
    procedure SelectionChanged;
    //
    property DesignSelector: TdxWizardControlDesignSelectorHelper read FDesignSelector;
  public
    constructor Create(AComponent: TComponent); override;
    destructor Destroy; override;
    procedure Activate;

    property Active: Boolean read GetActive;
    property Align: TAlign read GetAlign write SetAlign default alNone;
    property Header: TdxWizardControlPageHeader read FHeader write SetHeader;
    property OptionsSize: TdxWizardControlOptionsSize read FOptionsSize write SetOptionsSize;
    property PageIndex: Integer read GetPageIndex write SetPageIndex stored False;
    property PageVisible: Boolean read FPageVisible write SetPageVisible default True;
    property Watermark: TdxWizardControlPageWatermark read FWatermark write SetWatermark;
    property WizardControl: TdxCustomWizardControl read GetWizardControl write SetWizardControl;
  published
    property Height stored False;
    property Left stored False;
    property Top stored False;
    property Width stored False;
  end;

var
  FOnRegisterWizardControlPage: TcxNotifyProcedure;
  FOnUnregisterWizardControlPage: TcxNotifyProcedure;

implementation

uses
  Types, ShellAPI, dxWizardControlViewStyleWizard97, dxWizardControlViewStyleAero, dxDPIAwareUtils;

const
  dxWizardControlMultiStageAnimationFadeInTime = 200;
  dxWizardControlMultiStageAnimationFadeOutTime = 100;
  dxWizardControlMultiStageAnimationResizeTime = 200;
  dxWizardControlSingleStageAnimationTime = 200;

procedure RegisterWizardControlPage(APage: TdxWizardControlCustomPage);
begin
  if Assigned(FOnRegisterWizardControlPage) then
    FOnRegisterWizardControlPage(APage);
end;

procedure UnregisterWizardControlPage(APage: TdxWizardControlCustomPage);
begin
  if Assigned(FOnUnregisterWizardControlPage) then
    FOnUnregisterWizardControlPage(APage);
end;

procedure ResetFont(AFont: TFont);
begin
  cxResetFont(AFont);
  AFont.Color := clDefault;
end;

{ TdxWizardControlWizardProperty }

constructor TdxWizardControlCustomObject.Create(AWizardControl: TdxCustomWizardControl);
begin
  inherited Create;
  FWizardControl := AWizardControl;
end;

function TdxWizardControlCustomObject.GetScaleFactor: TdxScaleFactor;
begin
  Result := WizardControl.ScaleFactor;
end;

{ TdxWizardControlCustomPersistent }

procedure TdxWizardControlCustomPersistent.Assign(Source: TPersistent);
begin
  BeginUpdate;
  try
    DoAssign(Source);
  finally
    EndUpdate;
  end;
end;

procedure TdxWizardControlCustomPersistent.BeginUpdate;
begin
  Inc(FLockCount);
end;

procedure TdxWizardControlCustomPersistent.Changed(AChanges: TdxWizardControlChanges);
begin
  FChanges := FChanges + AChanges;
  if FLockCount = 0 then
  begin
    AChanges := FChanges;
    FChanges := [];
    DoChanged(AChanges);
  end;
end;

procedure TdxWizardControlCustomPersistent.ChangeScale(M, D: Integer);
begin
  // do nothing
end;

procedure TdxWizardControlCustomPersistent.DoChanged(AChanges: TdxWizardControlChanges);
begin
  if Assigned(OnChange) then
    OnChange(Self, AChanges);
end;

procedure TdxWizardControlCustomPersistent.EndUpdate;
begin
  Dec(FLockCount);
  if FLockCount = 0 then
    Changed(FChanges);
end;

{ TdxWizardControlHitTest }

procedure TdxWizardControlHitTest.Calculate(const APoint: TPoint);
begin
  HitPoint := APoint;
end;

function TdxWizardControlHitTest.GetHitAtButton: Boolean;
begin
  Result := HitObject is TdxWizardControlButtonViewInfo;
end;

function TdxWizardControlHitTest.GetHitAtCommandArea: Boolean;
begin
  Result := HitObject is TdxWizardControlCustomCommandAreaViewInfo;
end;

function TdxWizardControlHitTest.GetHitAtHeader: Boolean;
begin
  Result := HitObject is TdxWizardControlCustomHeaderViewInfo;
end;

function TdxWizardControlHitTest.GetHitAtInfoPanel: Boolean;
begin
  Result := HitObject is TdxWizardControlInfoPanelViewInfo;
end;

function TdxWizardControlHitTest.GetHitAtTitle: Boolean;
begin
  Result := HitObject is TdxWizardControlCustomTitleViewInfo;
end;

function TdxWizardControlHitTest.GetHitAtWatermark: Boolean;
begin
  Result := HitObject is TdxWizardControlWatermarkViewInfo;
end;

function TdxWizardControlHitTest.GetHitObjectAsButton: TdxWizardControlCustomButton;
begin
  if HitAtButton then
    Result := TdxWizardControlButtonViewInfo(HitObject).Button
  else
    Result := nil;
end;

procedure TdxWizardControlHitTest.SetHitPoint(const AValue: TPoint);
begin
  FCursor := crDefault;
  FHitObject := nil;
  FHitPoint := AValue;
  WizardControl.ViewInfo.CalculateHitTest(Self);
end;

{ TdxWizardControlCustomPainter }

constructor TdxWizardControlCustomPainter.Create(AWizardControl: TdxCustomWizardControl);
begin
  inherited Create(AWizardControl);
  FHeaderTitleFont := TFont.Create;
  FHeaderDescriptionFont := TFont.Create;
end;

destructor TdxWizardControlCustomPainter.Destroy;
begin
  FreeAndNil(FHeaderDescriptionFont);
  FreeAndNil(FHeaderTitleFont);
  inherited Destroy;
end;

procedure TdxWizardControlCustomPainter.AfterConstruction;
begin
  inherited AfterConstruction;
  InitializeFonts;
end;

procedure TdxWizardControlCustomPainter.DrawBackground(ACanvas: TcxCanvas; const ABounds: TRect);
begin
  LookAndFeelPainter.DrawWindowContent(ACanvas, ABounds);
end;

procedure TdxWizardControlCustomPainter.DrawButton(
  ACanvas: TcxCanvas; const ABounds: TRect; AState: TcxButtonState);
begin
  LookAndFeelPainter.DrawScaledButton(ACanvas, ABounds, '', AState, ScaleFactor);
end;

procedure TdxWizardControlCustomPainter.DrawCommandAreaBackground(ACanvas: TcxCanvas; const ABounds: TRect);
begin
  DrawBackground(ACanvas, ABounds);
  DrawSeparator(ACanvas, cxRectSetHeight(ABounds, SeparatorSize));
end;

procedure TdxWizardControlCustomPainter.DrawHeaderBackground(ACanvas: TcxCanvas; const ABounds: TRect);
begin
  DrawBackground(ACanvas, ABounds);
end;

procedure TdxWizardControlCustomPainter.DrawSeparator(ACanvas: TcxCanvas; const R: TRect);
begin
  LookAndFeelPainter.DrawLabelLine(ACanvas, R, clDefault, clDefault, False);
end;

procedure TdxWizardControlCustomPainter.DrawTitleBackground(ACanvas: TcxCanvas; const R: TRect);
begin
  DrawBackground(ACanvas, R);
end;

procedure TdxWizardControlCustomPainter.InitializeFonts;
begin
  HeaderDescriptionFont.Assign(WizardControl.Font);
  HeaderDescriptionFont.Color := clDefault;
  HeaderTitleFont.Assign(WizardControl.Font);
  HeaderTitleFont.Color := clDefault;
end;

function TdxWizardControlCustomPainter.GetButtonFocusRect(
  ACanvas: TcxCanvas; const ABounds: TRect): TRect;
begin
  Result := LookAndFeelPainter.ScaledButtonFocusRect(ACanvas, ABounds, ScaleFactor);
end;

function TdxWizardControlCustomPainter.GetButtonOffset(AButtonState: TcxButtonState): TPoint;
begin
  if AButtonState = cxbsPressed then
    Result := Point(LookAndFeelPainter.ScaledButtonTextShift(ScaleFactor), LookAndFeelPainter.ScaledButtonTextShift(ScaleFactor))
  else
    Result := cxNullPoint;
end;

function TdxWizardControlCustomPainter.GetButtonTextColor(AButtonState: TcxButtonState): TColor;
begin
  Result := LookAndFeelPainter.ButtonSymbolColor(AButtonState);
end;

function TdxWizardControlCustomPainter.GetCommandAreaDisabledTextColor: TColor;
begin
  Result := LookAndFeelPainter.DefaultEditorTextColor(True);
  if Result = clDefault then
    Result := clGrayText;
end;

function TdxWizardControlCustomPainter.GetCommandAreaTextColor: TColor;
begin
  Result := LookAndFeelPainter.GetWindowContentTextColor;
end;

function TdxWizardControlCustomPainter.GetHeaderDescriptionOffset: Integer;
begin
  Result := ScaleFactor.Apply(22);
end;

function TdxWizardControlCustomPainter.GetHeaderDescriptionTextColor: TColor;
begin
  Result := HeaderTitleTextColor;
end;

function TdxWizardControlCustomPainter.GetHeaderTitleTextColor: TColor;
begin
  Result := LookAndFeelPainter.DefaultContentTextColor;
end;

function TdxWizardControlCustomPainter.GetHyperlinkColor: TColor;
begin
  Result := LookAndFeelPainter.DefaultHyperlinkTextColor;
end;

function TdxWizardControlCustomPainter.GetIsFadingAvailable: Boolean;
begin
  Result := (LookAndFeelPainter.LookAndFeelStyle = lfsNative) or IsSkinUsed;
end;

function TdxWizardControlCustomPainter.GetIsSkinUsed: Boolean;
begin
  Result := LookAndFeelPainter.LookAndFeelStyle = lfsSkin;
end;

function TdxWizardControlCustomPainter.GetLookAndFeelPainter: TcxCustomLookAndFeelPainter;
begin
  Result := WizardControl.LookAndFeelPainter;
end;

function TdxWizardControlCustomPainter.GetSeparatorSize: Integer;
begin
  Result := ScaleFactor.Apply(LookAndFeelPainter.LabelLineHeight);
end;

function TdxWizardControlCustomPainter.GetTitleTextColor: TColor;
begin
  Result := clWindowText;
end;

{ TdxWizardControlCellViewInfo }

procedure TdxWizardControlCellViewInfo.BeforeDestruction;
begin
  inherited BeforeDestruction;
  Controller.CellRemoving(Self);
end;

procedure TdxWizardControlCellViewInfo.Calculate;
begin
  // do nothing
end;

function TdxWizardControlCellViewInfo.CalculateHitTest(AHitTest: TdxWizardControlHitTest): Boolean;
begin
  Result := Visible and cxRectPtIn(Bounds, AHitTest.HitPoint);
  if Result then
    AHitTest.HitObject := Self;
end;

function TdxWizardControlCellViewInfo.CanDrawContent: Boolean;
begin
  Result := (WizardControl.AnimationController = nil) or
    not WizardControl.AnimationController.SuppressContentDrawing;
end;

procedure TdxWizardControlCellViewInfo.DoRightToLeftConversion;
begin
  FBounds := GetRightToLeftConvertedRect(Bounds);
end;

function TdxWizardControlCellViewInfo.CanFocus: Boolean;
begin
  Result := False;
end;

procedure TdxWizardControlCellViewInfo.Draw(ACanvas: TcxCanvas);
begin
  if Visible and ACanvas.RectVisible(Bounds) then
  begin
    ACanvas.SaveClipRegion;
    try
      ACanvas.IntersectClipRect(Bounds);
      DrawBackground(ACanvas);
      if CanDrawContent then
        DrawContent(ACanvas);
      DrawChildren(ACanvas);
    finally
      ACanvas.RestoreClipRegion;
    end;
  end;
end;

procedure TdxWizardControlCellViewInfo.DrawBackground(ACanvas: TcxCanvas);
begin
  // do nothing
end;

procedure TdxWizardControlCellViewInfo.DrawChildren(ACanvas: TcxCanvas);
begin
  // do nothing
end;

procedure TdxWizardControlCellViewInfo.DrawContent(ACanvas: TcxCanvas);
begin
  // do nothing
end;

function TdxWizardControlCellViewInfo.GetRightToLeftConvertedRect(const ARect: TRect): TRect;
begin
  Result := TdxRightToLeftLayoutConverter.ConvertRect(ARect, cxGetClientRect(WizardControl));
end;

procedure TdxWizardControlCellViewInfo.Invalidate;
begin
  WizardControl.InvalidateRect(Bounds, True);
end;

function TdxWizardControlCellViewInfo.IsFocused: Boolean;
begin
  Result := Self = Controller.FocusedCell;
end;

function TdxWizardControlCellViewInfo.IsHot: Boolean;
begin
  Result := Self = Controller.HotCell;
end;

function TdxWizardControlCellViewInfo.IsPressed: Boolean;
begin
  Result := Self = Controller.PressedCell;
end;

function TdxWizardControlCellViewInfo.MeasureHeight: Integer;
begin
  Result := 0;
end;

function TdxWizardControlCellViewInfo.MeasureWidth: Integer;
begin
  Result := 0;
end;

function TdxWizardControlCellViewInfo.ProcessDialogChar(ACharCode: Word): Boolean;
begin
  Result := False;
end;

procedure TdxWizardControlCellViewInfo.RefreshState;
begin
end;

function TdxWizardControlCellViewInfo.GetController: TdxWizardControlController;
begin
  Result := WizardControl.Controller;
end;

function TdxWizardControlCellViewInfo.GetPainter: TdxWizardControlCustomPainter;
begin
  Result := WizardControl.ViewInfo.Painter;
end;

function TdxWizardControlCellViewInfo.GetVisible: Boolean;
begin
  Result := not cxRectIsEmpty(Bounds);
end;

procedure TdxWizardControlCellViewInfo.SetBounds(const ABounds: TRect);
begin
  FBounds := ABounds;
  Calculate;
end;

{ TdxWizardControlContainerCellViewInfo }

constructor TdxWizardControlContainerCellViewInfo.Create(AWizardControl: TdxCustomWizardControl);
begin
  inherited Create(AWizardControl);
  FCellList := TdxWizardControlCellViewInfoList.Create;
end;

destructor TdxWizardControlContainerCellViewInfo.Destroy;
begin
  FreeAndNil(FCellList);
  inherited Destroy;
end;

procedure TdxWizardControlContainerCellViewInfo.DrawChildren(ACanvas: TcxCanvas);
begin
  CellList.Draw(ACanvas);
end;

function TdxWizardControlContainerCellViewInfo.ProcessDialogChar(ACharCode: Word): Boolean;
begin
  Result := inherited ProcessDialogChar(ACharCode) or CellList.ProcessDialogChar(ACharCode);
end;

procedure TdxWizardControlContainerCellViewInfo.RefreshState;
begin
  inherited RefreshState;
  CellList.RefreshState;
end;

function TdxWizardControlContainerCellViewInfo.AddCell(
  ACellClass: TdxWizardControlCellViewInfoClass): TdxWizardControlCellViewInfo;
begin
  Result := ACellClass.Create(WizardControl);
  CellList.Add(Result);
end;

function TdxWizardControlContainerCellViewInfo.CalculateHitTest(AHitTest: TdxWizardControlHitTest): Boolean;
begin
  Result := inherited CalculateHitTest(AHitTest) and CellList.CalculateHitTest(AHitTest);
end;

{ TdxWizardControlCellViewInfoList }

function TdxWizardControlCellViewInfoList.Add(ACustomCellViewInfo: TdxWizardControlCellViewInfo): Integer;
begin
  if ACustomCellViewInfo <> nil then
    Result := inherited Add(ACustomCellViewInfo)
  else
    Result := -1;
end;

function TdxWizardControlCellViewInfoList.CalculateHitTest(AHitTest: TdxWizardControlHitTest): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Count - 1 do
    if Items[I].CalculateHitTest(AHitTest) then
    begin
      Result := True;
      Break;
    end;
end;

procedure TdxWizardControlCellViewInfoList.Draw(ACanvas: TcxCanvas);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].Draw(ACanvas);
end;

function TdxWizardControlCellViewInfoList.MaxMeasureHeight: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do
    Result := Max(Result, Items[I].MeasureHeight);
end;

function TdxWizardControlCellViewInfoList.MaxMeasureWidth: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do
    Result := Max(Result, Items[I].MeasureWidth);
end;

function TdxWizardControlCellViewInfoList.ProcessDialogChar(ACharCode: Word): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Count - 1 do
    if Items[I].ProcessDialogChar(ACharCode) then
    begin
      Result := True;
      Break;
    end;
end;

procedure TdxWizardControlCellViewInfoList.RefreshState;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].RefreshState;
end;

function TdxWizardControlCellViewInfoList.GetItem(Index: Integer): TdxWizardControlCellViewInfo;
begin
  Result := TdxWizardControlCellViewInfo(inherited Items[Index]);
end;

{ TdxWizardControlViewInfo }

constructor TdxWizardControlViewInfo.Create(AWizardControl: TdxCustomWizardControl);
begin
  inherited Create(AWizardControl);
  FPainter := CreatePainter;
  CellsCreate;
end;

destructor TdxWizardControlViewInfo.Destroy;
begin
  CellsFree;
  FreeAndNil(FPainter);
  inherited Destroy;
end;

procedure TdxWizardControlViewInfo.Calculate;
begin
  FBounds := cxGetClientRect(WizardControl);
  FContentBounds := Bounds;
  CalculateCells(FContentBounds);
  if WizardControl.UseRightToLeftAlignment then
    DoRightToLeftConversion;
  FPageArea := ContentBounds;
end;

procedure TdxWizardControlViewInfo.CalculateHeader(var ABounds: TRect);
begin
  HeaderViewInfo.Bounds := cxRectSetHeight(ABounds, HeaderViewInfo.MeasureHeight);
  ABounds.Top := HeaderViewInfo.Bounds.Bottom;
end;

procedure TdxWizardControlViewInfo.CalculateTitleViewInfo(var ABounds: TRect);
begin
  TitleViewInfo.Bounds := cxRectSetHeight(ABounds, TitleViewInfo.MeasureHeight);
  ABounds.Top := TitleViewInfo.Bounds.Bottom;
end;

procedure TdxWizardControlViewInfo.CalculateCells(var ABounds: TRect);
begin
  CalculateTitleViewInfo(ABounds);
  CalculateCommandArea(ABounds);
  CalculateWatermark(ABounds);
  CalculateHeader(ABounds);
end;

procedure TdxWizardControlViewInfo.CalculateCommandArea(var ABounds: TRect);
begin
  CommandAreaViewInfo.Bounds := cxRectSetBottom(
    ABounds, ABounds.Bottom, CommandAreaViewInfo.MeasureHeight);
  ABounds.Bottom := CommandAreaViewInfo.Bounds.Top;
end;

function TdxWizardControlViewInfo.CreateTitleViewInfo: TdxWizardControlCustomTitleViewInfo;
begin
  Result := TdxWizardControlCustomTitleViewInfo.Create(WizardControl);
end;

procedure TdxWizardControlViewInfo.CalculateWatermark(var ABounds: TRect);
begin
  WatermarkViewInfo.Bounds := cxRectSetWidth(ABounds, WatermarkViewInfo.MeasureWidth);
  ABounds.Left := WatermarkViewInfo.Bounds.Right;
end;

procedure TdxWizardControlViewInfo.CellsCreate;
begin
  FHeaderViewInfo := CreateHeaderViewInfo;
  FCommandAreaViewInfo := CreateCommandAreaViewInfo;
  FWatermarkViewInfo := CreateWatermarkViewInfo;
  FTitleViewInfo := CreateTitleViewInfo;
  CellList.Add(FTitleViewInfo);
  CellList.Add(FCommandAreaViewInfo);
  CellList.Add(FHeaderViewInfo);
  CellList.Add(FWatermarkViewInfo);
end;

procedure TdxWizardControlViewInfo.CellsFree;
begin
  FTitleViewInfo := nil;
  FCommandAreaViewInfo := nil;
  FHeaderViewInfo := nil;
  FWatermarkViewInfo := nil;
  CellList.Clear;
end;

procedure TdxWizardControlViewInfo.DoRightToLeftConversion;
begin
  inherited DoRightToLeftConversion;
  FContentBounds := GetRightToLeftConvertedRect(ContentBounds);
  if TitleViewInfo <> nil then
    TitleViewInfo.DoRightToLeftConversion;
  if CommandAreaViewInfo <> nil then
    CommandAreaViewInfo.DoRightToLeftConversion;
  if WatermarkViewInfo <> nil then
    WatermarkViewInfo.DoRightToLeftConversion;
  if HeaderViewInfo <> nil then
    HeaderViewInfo.DoRightToLeftConversion;
end;

procedure TdxWizardControlViewInfo.DrawBackground(ACanvas: TcxCanvas);
begin
  Painter.DrawBackground(ACanvas, ContentBounds);
end;

procedure TdxWizardControlViewInfo.GetTabOrderList(AList: TdxWizardControlCellViewInfoList);
begin
  // do nothing
end;

function TdxWizardControlViewInfo.GetTransitionEffectAreaBounds: TRect;
begin
  Result := Bounds;
  Result.Bottom := CommandAreaViewInfo.Bounds.Top;
end;

function TdxWizardControlViewInfo.MeasureHeight: Integer;
begin
  Result := Max(ScaleFactor.Apply(dxWizardControlMinHeight),
    CommandAreaViewInfo.MeasureHeight + Max(HeaderViewInfo.MeasureHeight, WatermarkViewInfo.MeasureHeight));
end;

function TdxWizardControlViewInfo.MeasureWidth: Integer;
begin
  Result := Max(ScaleFactor.Apply(dxWizardControlMinWidth),
    Max(CommandAreaViewInfo.MeasureWidth, HeaderViewInfo.MeasureWidth + WatermarkViewInfo.MeasureWidth));
end;

procedure TdxWizardControlViewInfo.RecreateCells;
begin
  CellsFree;
  CellsCreate;
end;

{ TdxWizardControlCustomHeaderViewInfo }

procedure TdxWizardControlCustomHeaderViewInfo.Calculate;
var
  AGlyphSize: TSize;
  R: TRect;
begin
  R := cxRectContent(Bounds, ContentMargins);
  if GlyphIsVisible then
  begin
    AGlyphSize := GlyphSize;
    FGlyphRect := cxRectCenterVertically(R, AGlyphSize.cy);
    FGlyphRect := cxRectSetRight(FGlyphRect, R.Right, AGlyphSize.cx);
  end
  else
    FGlyphRect := cxRectSetRight(R, R.Right, 0);

  R.Right := GlyphRect.Left - IndentBetweenElements;
  FTitleRect := R;
  if DescriptionIsVisible then
  begin
    FTitleRect := cxRectSetHeight(TitleRect, cxTextHeight(TitleFont));

    FDescriptionRect := R;
    FDescriptionRect.Top := TitleRect.Bottom + ScaleFactor.Apply(cxTextSpace);
    Inc(FDescriptionRect.Left, DescriptionOffset);
  end
  else
    FDescriptionRect := cxNullRect;
end;

procedure TdxWizardControlCustomHeaderViewInfo.DoRightToLeftConversion;
begin
  inherited DoRightToLeftConversion;
  FGlyphRect := GetRightToLeftConvertedRect(GlyphRect);
  FTitleRect := GetRightToLeftConvertedRect(TitleRect);
  FDescriptionRect := GetRightToLeftConvertedRect(DescriptionRect);
end;

procedure TdxWizardControlCustomHeaderViewInfo.DrawBackground(ACanvas: TcxCanvas);
begin
  Painter.DrawHeaderBackground(ACanvas, Bounds);
  if BackgroundImageIsAvailable then
    BackgroundImage.Draw(ACanvas, Bounds, ScaleFactor);
end;

procedure TdxWizardControlCustomHeaderViewInfo.DrawContent(ACanvas: TcxCanvas);
var
  AFormat: Cardinal;
begin
  if not cxRectIsEmpty(GlyphRect) then
    Glyph.StretchDraw(ACanvas.Handle, GlyphRect);

  ACanvas.SaveState;
  try
    ACanvas.Brush.Style := bsClear;
    AFormat := DT_VCENTER or DT_END_ELLIPSIS;
    if WizardControl.UseRightToLeftAlignment then
      AFormat := AFormat or DT_RIGHT
    else
      AFormat := AFormat or DT_LEFT;
    if WizardControl.UseRightToLeftReading then
      AFormat := AFormat or DT_RTLREADING;
    if not cxRectIsEmpty(TitleRect) then
    begin
      ACanvas.Font := TitleFont;
      ACanvas.Font.Color := cxGetActualColor(ACanvas.Font.Color, Painter.HeaderTitleTextColor);
      cxDrawText(ACanvas, Header.Title, TitleRect, AFormat or DT_SINGLELINE);
    end;
    if not cxRectIsEmpty(DescriptionRect) then
    begin
      ACanvas.Font := DescriptionFont;
      ACanvas.Font.Color := cxGetActualColor(ACanvas.Font.Color, Painter.HeaderDescriptionTextColor);
      cxDrawText(ACanvas, Header.Description, DescriptionRect, AFormat);
    end;
  finally
    ACanvas.RestoreState;
  end;
end;

function TdxWizardControlCustomHeaderViewInfo.MeasureHeight: Integer;
begin
  if Header <> nil then
  begin
    Result := cxTextHeight(TitleFont);
    if DescriptionIsVisible then
      Inc(Result, 2 * cxTextHeight(DescriptionFont) + ScaleFactor.Apply(cxTextSpace));
    if GlyphIsVisible then
      Result := Max(Result, GlyphSize.cy);
    Result := Result + cxMarginsHeight(ContentMargins);
  end
  else
    Result := 0;
end;

function TdxWizardControlCustomHeaderViewInfo.MeasureWidth: Integer;
begin
  Result := 0;
end;

function TdxWizardControlCustomHeaderViewInfo.GetActualSettings(
  AValue: TdxWizardControlHeaderAssignedValue; out AActualSettings: TdxWizardControlHeader): Boolean;
begin
  if (Header <> nil) and (AValue in Header.AssignedValues) then
    AActualSettings := Header
  else
    if (AValue in WizardControl.Header.AssignedValues) then
      AActualSettings := WizardControl.Header
    else
      AActualSettings := nil;

  Result := AActualSettings <> nil;
end;

function TdxWizardControlCustomHeaderViewInfo.GetBackgroundImageIsAvailable: Boolean;
var
  ASettings: TdxWizardControlHeader;
begin
  if GetActualSettings(wchvBackgroundImage, ASettings) then
    Result := not ASettings.BackgroundImage.Empty
  else
    Result := False;
end;

function TdxWizardControlCustomHeaderViewInfo.GetIsVisible: Boolean;
begin
  Result := (WizardControl.Header.Visible = wcevAlwaysVisible) or
    ((WizardControl.Header.Visible = wcevDefault) and (Header <> nil) and (Header.Visible <> wcevAlwaysHidden));
end;

function TdxWizardControlCustomHeaderViewInfo.GetBackgroundImage: TdxWizardControlImage;
var
  ASettings: TdxWizardControlHeader;
begin
  if GetActualSettings(wchvBackgroundImage, ASettings) then
    Result := ASettings.BackgroundImage
  else
    Result := nil;
end;

function TdxWizardControlCustomHeaderViewInfo.GetDescriptionFont: TFont;
var
  ASettings: TdxWizardControlHeader;
begin
  if GetActualSettings(wchvDescriptionFont, ASettings) then
    Result := ASettings.DescriptionFont
  else
    Result := Painter.HeaderDescriptionFont;
end;

function TdxWizardControlCustomHeaderViewInfo.GetDescriptionOffset: Integer;
var
  ASettings: TdxWizardControlHeader;
begin
  if GetActualSettings(wchvDescriptionOffset, ASettings) then
    Result := ASettings.DescriptionOffset
  else
    Result := Painter.HeaderDescriptionOffset;
end;

function TdxWizardControlCustomHeaderViewInfo.GetGlyph: TdxSmartImage;
var
  ASettings: TdxWizardControlHeader;
begin
  if GetActualSettings(wchvGlyph, ASettings) then
    Result := ASettings.Glyph
  else
    Result := nil;
end;

function TdxWizardControlCustomHeaderViewInfo.GetGlyphSize: TSize;
begin
  Result := dxGetImageSize(Glyph, ScaleFactor);
end;

function TdxWizardControlCustomHeaderViewInfo.GetHeader: TdxWizardControlPageHeader;
begin
  if WizardControl.ActivePage <> nil then
    Result := WizardControl.ActivePage.Header
  else
    Result := nil;
end;

function TdxWizardControlCustomHeaderViewInfo.GetTitleFont: TFont;
var
  ASettings: TdxWizardControlHeader;
begin
  if GetActualSettings(wchvTitleFont, ASettings) then
    Result := ASettings.TitleFont
  else
    Result := Painter.HeaderTitleFont;
end;

{ TdxWizardControlCustomCommandAreaViewInfo }

constructor TdxWizardControlCustomCommandAreaViewInfo.Create(AWizardControl: TdxCustomWizardControl);
var
  I: Integer;
  ACustomButton: TdxWizardControlButtonViewInfo;
begin
  inherited Create(AWizardControl);
  if WizardControl.InfoPanel.Visible then
    FInfoPanelViewInfo := AddCell(TdxWizardControlInfoPanelViewInfo);
  FHelpButtonViewInfo := AddButtonCell(Buttons.Help, TdxWizardControlButtonViewInfo);
  FNextButtonViewInfo := AddButtonCell(Buttons.Next, TdxWizardControlButtonViewInfo);
  FFinishButtonViewInfo := AddButtonCell(Buttons.Finish, TdxWizardControlButtonViewInfo);
  FCancelButtonViewInfo := AddButtonCell(Buttons.Cancel, TdxWizardControlButtonViewInfo);
  FCustomButtonViewInfos := TList.Create;
  for I := Buttons.CustomButtons.Buttons.Count - 1 downto 0 do
  begin
    ACustomButton := AddButtonCell(Buttons.CustomButtons.Buttons.Items[I].Button, TdxWizardControlButtonViewInfo);
    if ACustomButton <> nil then
      FCustomButtonViewInfos.Add(ACustomButton);
  end;
end;

destructor TdxWizardControlCustomCommandAreaViewInfo.Destroy;
begin
  FreeAndNil(FCustomButtonViewInfos);
  inherited Destroy;
end;

function TdxWizardControlCustomCommandAreaViewInfo.AddButtonCell(
  AButton: TdxWizardControlCustomButton;
  AViewInfoClass: TdxWizardControlButtonViewInfoClass): TdxWizardControlButtonViewInfo;
begin
  if (AButton <> nil) and AButton.Visible then
  begin
    Result := AViewInfoClass.Create(WizardControl, AButton);
    CellList.Add(Result);
  end
  else
    Result := nil;
end;

function TdxWizardControlCustomCommandAreaViewInfo.AlignButtonToRightSide(
  AButtonViewInfo: TdxWizardControlButtonViewInfo; var R: TRect): Boolean;
var
  ARect: TRect;
begin
  Result := AButtonViewInfo <> nil;
  if Result then
  begin
    ARect := cxRectSetRight(R, R.Right, AButtonViewInfo.MeasureWidth);
    AButtonViewInfo.Bounds := cxRectCenterVertically(ARect, ButtonsMaxMeasureHeight);
    R.Right := AButtonViewInfo.Bounds.Left;
  end;
end;

procedure TdxWizardControlCustomCommandAreaViewInfo.Calculate;
var
  AContentBounds: TRect;
begin
  AContentBounds := cxRectContent(Bounds, ContentMargins);
  CalculateButtonsMaxMeasureHeight;
  CalculateButtons(AContentBounds);
  CalculateInfoPanel(AContentBounds);
end;

procedure TdxWizardControlCustomCommandAreaViewInfo.CalculateButtons(var ABounds: TRect);
begin
  if NextButtonViewInfo <> nil then
    NextButtonViewInfo.Bounds := cxNullRect;
  if FinishButtonViewInfo <> nil then
    FinishButtonViewInfo.Bounds := cxNullRect;
end;

procedure TdxWizardControlCustomCommandAreaViewInfo.CalculateCustomButtons(var ABounds: TRect);
var
  I: Integer;
begin
  for I := 0 to CustomButtonViewInfosCount - 2 do
    if AlignButtonToRightSide(CustomButtonViewInfos[I], ABounds) then
      Dec(ABounds.Right, IndentBetweenElementsInGroup);
  if (CustomButtonViewInfosCount > 0) and
    AlignButtonToRightSide(CustomButtonViewInfos[CustomButtonViewInfosCount - 1], ABounds) then
    Dec(ABounds.Right, IndentBetweenGroups);
end;

procedure TdxWizardControlCustomCommandAreaViewInfo.CalculateInfoPanel(var ABounds: TRect);
begin
  if InfoPanelViewInfo <> nil then
    InfoPanelViewInfo.Bounds := cxRectSetWidth(ABounds,
      Min(InfoPanelViewInfo.MeasureWidth, cxRectWidth(ABounds)));
end;

procedure TdxWizardControlCustomCommandAreaViewInfo.CalculateButtonsMaxMeasureHeight;
var
  I: Integer;
begin
  FButtonsMaxMeasureHeight := 0;
  if Buttons.Finish.AlwaysVisible then
  begin
    if NextButtonViewInfo <> nil then
      FButtonsMaxMeasureHeight := Max(FButtonsMaxMeasureHeight, NextButtonViewInfo.MeasureHeight);
    if FinishButtonViewInfo <> nil then
      FButtonsMaxMeasureHeight := Max(FButtonsMaxMeasureHeight, FinishButtonViewInfo.MeasureHeight);
  end
  else
    if ActualNextButtonViewInfo <> nil then
      FButtonsMaxMeasureHeight := Max(FButtonsMaxMeasureHeight, ActualNextButtonViewInfo.MeasureHeight);
  if CancelButtonViewInfo <> nil then
    FButtonsMaxMeasureHeight := Max(FButtonsMaxMeasureHeight, CancelButtonViewInfo.MeasureHeight);
  if HelpButtonViewInfo <> nil then
    FButtonsMaxMeasureHeight := Max(FButtonsMaxMeasureHeight, HelpButtonViewInfo.MeasureHeight);
  for I := 0 to CustomButtonViewInfosCount - 1 do
    FButtonsMaxMeasureHeight := Max(FButtonsMaxMeasureHeight, CustomButtonViewInfos[I].MeasureHeight);
end;

procedure TdxWizardControlCustomCommandAreaViewInfo.DoRightToLeftConversion;
var
  I: Integer;
begin
  inherited DoRightToLeftConversion;
  if ActualNextButtonViewInfo <> nil then
    ActualNextButtonViewInfo.DoRightToLeftConversion;
  if CancelButtonViewInfo <> nil then
    CancelButtonViewInfo.DoRightToLeftConversion;
  if HelpButtonViewInfo <> nil then
    HelpButtonViewInfo.DoRightToLeftConversion;
  for I := 0 to CustomButtonViewInfosCount - 1 do
    CustomButtonViewInfos[I].DoRightToLeftConversion;
  if InfoPanelViewInfo <> nil then
    InfoPanelViewInfo.DoRightToLeftConversion;
end;

procedure TdxWizardControlCustomCommandAreaViewInfo.DrawBackground(ACanvas: TcxCanvas);
begin
  Painter.DrawCommandAreaBackground(ACanvas, Bounds);
end;

function TdxWizardControlCustomCommandAreaViewInfo.GetButtonViewInfo(
  AButton: TdxWizardControlCustomButton): TdxWizardControlButtonViewInfo;
var
  AButtonViewInfo: TdxWizardControlButtonViewInfo;
  I: Integer;
begin
  Result := nil;
  for I := 0 to CellList.Count - 1 do
    if CellList.Items[I] is TdxWizardControlButtonViewInfo then
    begin
      AButtonViewInfo := TdxWizardControlButtonViewInfo(CellList.Items[I]);
      if AButtonViewInfo.Button = AButton then
      begin
        Result := AButtonViewInfo;
        Break;
      end;
    end;
end;

function TdxWizardControlCustomCommandAreaViewInfo.MeasureButtonWidth(
  AButtonViewInfo: TdxWizardControlButtonViewInfo; AIndent: Integer): Integer;
begin
  if AButtonViewInfo <> nil then
    Result := AButtonViewInfo.MeasureWidth + AIndent
  else
    Result := 0;
end;

function TdxWizardControlCustomCommandAreaViewInfo.MeasureHeight: Integer;
begin
  Result := CellList.MaxMeasureHeight + cxMarginsHeight(ContentMargins);
end;

function TdxWizardControlCustomCommandAreaViewInfo.MeasureWidth: Integer;
var
  I: Integer;
begin
  Result := cxMarginsWidth(ContentMargins);
  if InfoPanelViewInfo <> nil then
    Inc(Result, InfoPanelViewInfo.MeasureWidth);
  if Buttons.Finish.AlwaysVisible then
  begin
    Inc(Result, MeasureButtonWidth(NextButtonViewInfo, IndentBetweenElementsInGroup));
    Inc(Result, MeasureButtonWidth(FinishButtonViewInfo, IndentBetweenElementsInGroup));
  end
  else
    Inc(Result, MeasureButtonWidth(ActualNextButtonViewInfo, IndentBetweenElementsInGroup));
  Inc(Result, MeasureButtonWidth(CancelButtonViewInfo, IndentBetweenGroups));
  Inc(Result, MeasureButtonWidth(HelpButtonViewInfo, IndentBetweenGroups));
  for I := 0 to CustomButtonViewInfosCount - 1 do
    Inc(Result, MeasureButtonWidth(CustomButtonViewInfos[I], IndentBetweenElementsInGroup));
end;

function TdxWizardControlCustomCommandAreaViewInfo.GetActualNextButtonViewInfo: TdxWizardControlButtonViewInfo;
begin
  if WizardControl.CanGoToNextPage then
    Result := NextButtonViewInfo
  else
    Result := FinishButtonViewInfo;
end;

function TdxWizardControlCustomCommandAreaViewInfo.GetButtons: TdxWizardControlButtons;
begin
  Result := WizardControl.Buttons;
end;

function TdxWizardControlCustomCommandAreaViewInfo.GetCustomButtonViewInfos(Index: Integer):
  TdxWizardControlButtonViewInfo;
begin
  Result := TdxWizardControlButtonViewInfo(FCustomButtonViewInfos.Items[Index]);
end;

function TdxWizardControlCustomCommandAreaViewInfo.GetCustomButtonViewInfosCount: Integer;
begin
  Result := FCustomButtonViewInfos.Count;
end;

function TdxWizardControlCustomCommandAreaViewInfo.GetHasCustomButtonsAlignedLeft: Boolean;
begin
  Result := (Buttons.CustomButtons.Buttons.Count > 0) and (Buttons.CustomButtons.Alignment = wccbaLeft);
end;

function TdxWizardControlCustomCommandAreaViewInfo.GetHasCustomButtonsAlignedRight: Boolean;
begin
  Result := (Buttons.CustomButtons.Buttons.Count > 0) and (Buttons.CustomButtons.Alignment = wccbaRight);
end;

{ TdxWizardControlButtonFadingHelper }

constructor TdxWizardControlButtonFadingHelper.Create(AViewInfo: TdxWizardControlButtonViewInfo);
begin
  inherited Create;
  FButtonViewInfo := AViewInfo;
end;

function TdxWizardControlButtonFadingHelper.CanFade: Boolean;
begin
  Result := ButtonViewInfo.IsFadingAvailable and not ButtonViewInfo.WizardControl.IsDesigning;
end;

procedure TdxWizardControlButtonFadingHelper.DrawFadeImage;
begin
  ButtonViewInfo.Invalidate;
end;

procedure TdxWizardControlButtonFadingHelper.GetFadingImages(out AFadeOutImage, AFadeInImage: TcxBitmap);

  function PrepareImage(AState: TcxButtonState): TcxBitmap32;
  begin
    Result := TcxBitmap32.CreateSize(ButtonViewInfo.Bounds, True);
    ButtonViewInfo.DrawButtonBackground(Result.cxCanvas, Result.ClientRect, AState);
  end;

const
  StateMap: array[Boolean] of TcxButtonState = (cxbsNormal, cxbsDefault);
begin
  AFadeOutImage := PrepareImage(StateMap[ButtonViewInfo.IsDefault or ButtonViewInfo.IsFocused]);
  AFadeInImage := PrepareImage(cxbsHot);
end;

{ TdxWizardControlButtonViewInfo }

constructor TdxWizardControlButtonViewInfo.Create(
  AControl: TdxCustomWizardControl; AButton: TdxWizardControlCustomButton);
begin
  inherited Create(AControl);
  FButton := AButton;
  FState := cxbsNormal;
  FFadingHelper := TdxWizardControlButtonFadingHelper.Create(Self);
end;

destructor TdxWizardControlButtonViewInfo.Destroy;
begin
  FreeAndNil(FFadingHelper);
  inherited Destroy;
end;

procedure TdxWizardControlButtonViewInfo.Calculate;
begin
  CalculateCaptionRect;
  CalculateGlyphRect;
end;

procedure TdxWizardControlButtonViewInfo.CalculateCaptionRect;
begin
  FCaptionRect := cxRectContent(Bounds, Margins);
end;

procedure TdxWizardControlButtonViewInfo.CalculateGlyphRect;

  procedure PlaceGlyphRect(const AContentBounds: TRect; const AWidth, AHeight: Integer);
  begin
    case Button.GlyphAlignment of
      blGlyphLeft:
        begin
          FGlyphRect := cxRectCenterVertically(AContentBounds, AHeight);
          FGlyphRect := cxRectSetLeft(GlyphRect, AContentBounds.Left, AWidth);
        end;
      blGlyphRight:
        begin
          FGlyphRect := cxRectCenterVertically(AContentBounds, AHeight);
          FGlyphRect := cxRectSetRight(GlyphRect, AContentBounds.Right, AWidth);
        end;
      blGlyphTop:
        begin
          FGlyphRect := cxRectCenterHorizontally(AContentBounds, AWidth);
          FGlyphRect := cxRectSetTop(GlyphRect, AContentBounds.Top, AHeight);
        end;
      blGlyphBottom:
        begin
          FGlyphRect := cxRectCenterHorizontally(AContentBounds, AWidth);
          FGlyphRect := cxRectSetBottom(GlyphRect, AContentBounds.Bottom, AHeight);
        end;
    end;
  end;

  procedure CheckGlyphRectSize(const AContentBounds: TRect; const AGlyphRect: TRect);
  begin
    if (cxRectWidth(AContentBounds) < cxRectWidth(AGlyphRect)) or
      (cxRectHeight(AContentBounds) < cxRectHeight(AGlyphRect)) then
      FGlyphRect := cxRectProportionalStretch(AContentBounds,
        cxRectWidth(AGlyphRect), cxRectHeight(AGlyphRect));
    PlaceGlyphRect(AContentBounds, cxRectWidth(GlyphRect), cxRectHeight(GlyphRect));
  end;

var
  AContentBounds: TRect;
begin
  if Button.Glyph.Empty and not IsImageAvailable then
  begin
    FGlyphRect := cxNullRect;
    Exit;
  end;

  AContentBounds := cxRectContent(Bounds, Margins);
  PlaceGlyphRect(AContentBounds, GlyphSize.cx, GlyphSize.cy);
  CheckGlyphRectSize(AContentBounds, GlyphRect);

  case Button.GlyphAlignment of
    blGlyphLeft:
      FCaptionRect.Left := GlyphRect.Right + ScaleFactor.Apply(cxTextOffset);
    blGlyphRight:
      FCaptionRect.Right := GlyphRect.Left - ScaleFactor.Apply(cxTextOffset);
    blGlyphTop:
      FCaptionRect.Top := GlyphRect.Bottom + ScaleFactor.Apply(cxTextOffset);
    blGlyphBottom:
      FCaptionRect.Bottom := GlyphRect.Top - ScaleFactor.Apply(cxTextOffset);
  end;
end;

function TdxWizardControlButtonViewInfo.CalculateState: TcxButtonState;
begin
  if not Button.ActuallyEnabled then
    Result := cxbsDisabled
  else
    if IsPressed and (not WizardControl.MouseCapture or IsHot) then
      Result := cxbsPressed
    else
      if IsHot then
        Result := cxbsHot
      else
        if IsDefault or IsFocused then
          Result := cxbsDefault
        else
          Result := cxbsNormal;
end;

function TdxWizardControlButtonViewInfo.CanFocus: Boolean;
begin
  Result := Button.ActuallyEnabled and Visible;
end;

procedure TdxWizardControlButtonViewInfo.DoRightToLeftConversion;
begin
  inherited DoRightToLeftConversion;
  FCaptionRect := GetRightToLeftConvertedRect(CaptionRect);
  FGlyphRect := GetRightToLeftConvertedRect(GlyphRect);
end;

procedure TdxWizardControlButtonViewInfo.DrawButtonBackground(
  ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState);
begin
  Painter.DrawButton(ACanvas, R, AState);
end;

procedure TdxWizardControlButtonViewInfo.DrawContent(ACanvas: TcxCanvas);
const
  EnabledImageDrawModeMap: array [Boolean] of TcxImageDrawMode = (idmNormal, idmDisabled);
var
  AFlags: Integer;
  AGlyphBitmap: TBitmap;
  AOffset: TPoint;
begin
  if not FadingHelper.DrawImage(ACanvas.Handle, Bounds) then
    DrawButtonBackground(ACanvas, Bounds, State);
  AOffset := Painter.GetButtonOffset(State);

  if not cxRectIsEmpty(GlyphRect) then
  begin
    if not Button.Glyph.Empty then
      AGlyphBitmap := Button.Glyph.GetAsBitmap
    else
      AGlyphBitmap := nil;
    try
      cxDrawImage(ACanvas.Handle, cxRectOffset(GlyphRect, AOffset, not WizardControl.UseRightToLeftAlignment),
        cxRectOffset(GlyphRect, AOffset, not WizardControl.UseRightToLeftAlignment), AGlyphBitmap,
        WizardControl.Buttons.Images, Button.ImageIndex, EnabledImageDrawModeMap[State = cxbsDisabled], True);
    finally
      AGlyphBitmap.Free;
    end;
  end;

  ACanvas.SaveState;
  try
    ACanvas.Font := Font;
    ACanvas.Font.Color := Painter.GetButtonTextColor(State);
    if not cxRectIsEmpty(CaptionRect) then
    begin
      AFlags := DT_CENTER or DT_VCENTER or DT_SINGLELINE;
      if WizardControl.UseRightToLeftReading then
        AFlags := AFlags or DT_RTLREADING;
      cxDrawText(ACanvas, Button.Caption, cxRectOffset(CaptionRect, AOffset, not WizardControl.UseRightToLeftAlignment),
        AFlags);
    end;
    if IsFocused then
      ACanvas.DrawFocusRect(Painter.GetButtonFocusRect(ACanvas, Bounds));
  finally
    ACanvas.RestoreState;
  end;
end;

function TdxWizardControlButtonViewInfo.MeasureHeight: Integer;

  function AutoHeightValue: Integer;
  var
    AActualMeasureWidth: Integer;
  begin
    if Button.GlyphAlignment in [blGlyphLeft, blGlyphRight] then
      Result := Max(GlyphSize.cy, cxTextHeight(Font)) + cxMarginsHeight(Margins)
    else
    begin
      Result := cxTextHeight(Font) + cxMarginsHeight(Margins);
      if (not Button.Glyph.Empty) or IsImageAvailable then
        Result := Result + GlyphSize.cy + ScaleFactor.Apply(cxTextOffset);
    end;
    AActualMeasureWidth := MeasureWidth;
    if GlyphSize.cx > AActualMeasureWidth then
      Result := Trunc(Result * (AActualMeasureWidth/GlyphSize.cx));
  end;

var
  AActualButtonHeight: Integer;
begin
  AActualButtonHeight := Button.Buttons.Common.Height;
  if AActualButtonHeight <= 0 then
    Result := AutoHeightValue
  else
    Result := AActualButtonHeight;
end;

function TdxWizardControlButtonViewInfo.MeasureWidth: Integer;
var
  AActualButtonWidth: Integer;
begin
  AActualButtonWidth := Button.Width;
  if AActualButtonWidth <= 0 then
    AActualButtonWidth := Button.Buttons.Common.Width;
  if AActualButtonWidth <= 0 then
    AActualButtonWidth := ScaleFactor.Apply(dxWizardControlDefaultButtonWidth);
  Result := AActualButtonWidth;
end;

function TdxWizardControlButtonViewInfo.ProcessDialogChar(ACharCode: Word): Boolean;
begin
  Result := False;
  if CanFocus then
  begin
    Result := (ACharCode = VK_RETURN) and (IsDefault or IsFocused);
    if Result then
      Controller.ProcessClick(Self)
    else
    begin
      Result := IsAccel(ACharCode, Button.Caption);
      if Result then
        Controller.ProcessAccel(Self);
    end;
  end;
end;

function TdxWizardControlButtonViewInfo.IsDefault: Boolean;
begin
  Result := (Controller.FocusedCell = nil) and (Controller.ViewInfo.CommandAreaViewInfo.ActualNextButtonViewInfo = Self);
end;

function TdxWizardControlButtonViewInfo.IsImageAvailable: Boolean;
begin
  Result := (WizardControl.Buttons.Images <> nil) and (Button.ImageIndex <> -1) and
    IsImageAssigned(WizardControl.Buttons.Images, Button.ImageIndex);
end;

function TdxWizardControlButtonViewInfo.IsFadingAvailable: Boolean;
begin
  Result := Painter.IsFadingAvailable;
end;

procedure TdxWizardControlButtonViewInfo.RefreshState;
begin
  State := CalculateState;
end;

function TdxWizardControlButtonViewInfo.GetFont: TFont;
begin
  Result := WizardControl.Font;
end;

function TdxWizardControlButtonViewInfo.GetGlyphSize: TSize;
begin
  if not Button.Glyph.Empty or IsImageAvailable then
    Result := dxGetImageSize(Button.Glyph, WizardControl.Buttons.Images, Button.ImageIndex, ScaleFactor)
  else
    Result := cxNullSize;
end;

function TdxWizardControlButtonViewInfo.GetMargins: TRect;
begin
  Result := ScaleFactor.Apply(Rect(6, 6, 6, 6));
end;

procedure TdxWizardControlButtonViewInfo.SetState(const AState: TcxButtonState);
begin
  if State <> AState then
  begin
    FadingHelper.CheckStartFading(State, AState);
    FState := AState;
    Invalidate;
  end;
end;

{ TdxWizardControlInfoPanelViewInfo }

constructor TdxWizardControlInfoPanelViewInfo.Create(AControl: TdxCustomWizardControl);
begin
  inherited Create(AControl);
  FInfoPanel := AControl.InfoPanel;
  State := cxbsNormal;
end;

function TdxWizardControlInfoPanelViewInfo.CalculateHitTest(AHitTest: TdxWizardControlHitTest): Boolean;
begin
  Result := inherited CalculateHitTest(AHitTest);
  if (AHitTest.HitObject = Self) and IsHyperlinkMode then
    AHitTest.Cursor := crHandPoint;
end;

function TdxWizardControlInfoPanelViewInfo.MeasureHeight: Integer;
begin
  Result := cxTextHeight(Font) + ScaleFactor.Apply(cxTextOffset) * 2;
end;

function TdxWizardControlInfoPanelViewInfo.MeasureWidth: Integer;
begin
  Result := cxTextWidth(Font, InfoPanel.Caption) + ScaleFactor.Apply(cxTextOffset) * 2;
end;

procedure TdxWizardControlInfoPanelViewInfo.RefreshState;
begin
  if IsHot and IsPressed then
    State := cxbsPressed
  else
    if IsHot then
      State := cxbsHot
    else
      State := cxbsNormal;
end;

procedure TdxWizardControlInfoPanelViewInfo.DrawContent(ACanvas: TcxCanvas);
var
  AFormat: Cardinal;
begin
  ACanvas.SaveState;
  try
    ACanvas.Font := Font;
    ACanvas.Font.Style := FontStyle;
    ACanvas.Font.Color := TextColor;
    AFormat := DT_VCENTER or DT_END_ELLIPSIS or DT_SINGLELINE;
    if WizardControl.UseRightToLeftAlignment then
      AFormat := AFormat or DT_RIGHT
    else
      AFormat := AFormat or DT_LEFT;
    if WizardControl.UseRightToLeftReading then
      AFormat := AFormat or DT_RTLREADING;
    cxDrawText(ACanvas, InfoPanel.Caption, Bounds, AFormat);
  finally
    ACanvas.RestoreState;
  end;
end;

function TdxWizardControlInfoPanelViewInfo.GetFont: TFont;
begin
  Result := InfoPanel.Font;
end;

function TdxWizardControlInfoPanelViewInfo.GetFontStyle: TFontStyles;
begin
  Result := Font.Style;
  if IsHyperlinkMode and (State = cxbsHot) then
    Include(Result, fsUnderline);
end;

function TdxWizardControlInfoPanelViewInfo.GetIsHyperlinkMode: Boolean;
begin
  Result := InfoPanel.Enabled and (InfoPanel.Hyperlink <> '');
end;

function TdxWizardControlInfoPanelViewInfo.GetTextColor: TColor;
begin
  if IsHyperlinkMode then
    Result := cxGetActualColor(InfoPanel.HyperlinkColor, Painter.HyperlinkColor)
  else
    if InfoPanel.Enabled then
      Result := Painter.CommandAreaTextColor
    else
      Result := Painter.CommandAreaDisabledTextColor;
end;

procedure TdxWizardControlInfoPanelViewInfo.SetState(const AState: TcxButtonState);
begin
  if State <> AState then
  begin
    FState := AState;
    Invalidate;
  end;
end;

{ TdxWizardControlImage }

constructor TdxWizardControlImage.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FImage := TdxSkinGlyph.Create(nil);
  FImage.OnChange := ImageChanged;
end;

destructor TdxWizardControlImage.Destroy;
begin
  FreeAndNil(FImage);
  inherited Destroy;
end;

procedure TdxWizardControlImage.DoAssign(Source: TPersistent);
begin
  inherited DoAssign(Source);
  if Source is TdxWizardControlImage then
  begin
    Image := TdxWizardControlImage(Source).Image;
    Margins := TdxWizardControlImage(Source).Margins;
  end;
end;

procedure TdxWizardControlImage.Draw(ACanvas: TcxCanvas; const ARect: TRect);
begin
  Draw(ACanvas, ARect, dxSystemScaleFactor);
end;

procedure TdxWizardControlImage.Draw(ACanvas: TcxCanvas; const ARect: TRect; AScaleFactor: TdxScaleFactor);
begin
  FImage.Draw(ACanvas.Handle, ARect, AScaleFactor);
end;

procedure TdxWizardControlImage.ImageChanged(Sender: TObject);
begin
  Changed([wccLayout]);
end;

procedure TdxWizardControlImage.Reset;
begin
  BeginUpdate;
  try
    FImage.Clear;
    Margins.Margin := cxNullRect;
  finally
    EndUpdate;
  end;
end;

function TdxWizardControlImage.GetEmpty: Boolean;
begin
  Result := FImage.Empty;
end;

function TdxWizardControlImage.GetHeight: Integer;
begin
  Result := FImage.Size.cy;
end;

function TdxWizardControlImage.GetImage: TdxSmartGlyph;
begin
  Result := FImage.Texture;
end;

function TdxWizardControlImage.GetMargins: TcxMargin;
begin
  Result := FImage.Margins;
end;

function TdxWizardControlImage.GetStretch: TdxSkinStretchMode;
begin
  Result := FImage.Stretch;
end;

function TdxWizardControlImage.GetWidth: Integer;
begin
  Result := FImage.Size.cx;
end;

procedure TdxWizardControlImage.SetImage(AValue: TdxSmartGlyph);
begin
  FImage.Texture.Assign(AValue);
end;

procedure TdxWizardControlImage.SetMargins(AValue: TcxMargin);
begin
  FImage.Margins.Assign(AValue);
end;

procedure TdxWizardControlImage.SetStretch(const Value: TdxSkinStretchMode);
begin
  FImage.Stretch := Value;
end;

{ TdxWizardControlCustomHeader }

constructor TdxWizardControlHeader.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FBackgroundImage := TdxWizardControlImage.Create(Self);
  FBackgroundImage.OnChange := BackgroundImageChanged;
  FGlyph := TdxSmartGlyph.Create;
  FGlyph.OnChange := GlyphChanged;
  FDescriptionFont := TFont.Create;
  FDescriptionFont.Color := clDefault;
  FDescriptionFont.OnChange := DescriptionFontChanged;
  FTitleFont := TFont.Create;
  FTitleFont.Color := clDefault;
  FTitleFont.OnChange := TitleFontChanged;
end;

destructor TdxWizardControlHeader.Destroy;
begin
  FreeAndNil(FDescriptionFont);
  FreeAndNil(FTitleFont);
  FreeAndNil(FGlyph);
  FreeAndNil(FBackgroundImage);
  inherited Destroy;
end;

procedure TdxWizardControlHeader.ChangeScale(M, D: Integer);
begin
  if IsDescriptionFontStored then
    DescriptionFont.Height := MulDiv(DescriptionFont.Height, M, D);
  if IsDescriptionOffsetStored then
    DescriptionOffset := MulDiv(DescriptionOffset, M, D);
  if IsTitleFontStored then
    TitleFont.Height := MulDiv(TitleFont.Height, M, D);
end;

function TdxWizardControlHeader.CheckResetValue(AValue: TdxWizardControlHeaderAssignedValue;
  const ANewAssignedValues: TdxWizardControlHeaderAssignedValues): Boolean;
begin
  Result := (AValue in AssignedValues) and not (AValue in ANewAssignedValues);
end;

procedure TdxWizardControlHeader.DoAssign(Source: TPersistent);
begin
  inherited DoAssign(Source);
  if Source is TdxWizardControlHeader then
  begin
    BackgroundImage := TdxWizardControlHeader(Source).BackgroundImage;
    DescriptionFont := TdxWizardControlHeader(Source).DescriptionFont;
    DescriptionVisibility := TdxWizardControlHeader(Source).DescriptionVisibility;
    DescriptionOffset := TdxWizardControlHeader(Source).DescriptionOffset;
    TitleFont := TdxWizardControlHeader(Source).TitleFont;
    Glyph := TdxWizardControlHeader(Source).Glyph;
    GlyphVisibility := TdxWizardControlHeader(Source).GlyphVisibility;
    Visible := TdxWizardControlHeader(Source).Visible;
    AssignedValues := TdxWizardControlHeader(Source).AssignedValues;
  end;
end;

procedure TdxWizardControlHeader.BackgroundImageChanged(Sender: TObject; AChanges: TdxWizardControlChanges);
begin
  Include(FAssignedValues, wchvBackgroundImage);
  Changed([wccLayout]);
end;

procedure TdxWizardControlHeader.DescriptionFontChanged(Sender: TObject);
begin
  Include(FAssignedValues, wchvDescriptionFont);
  Changed([wccLayout]);
end;

procedure TdxWizardControlHeader.GlyphChanged(Sender: TObject);
begin
  Include(FAssignedValues, wchvGlyph);
  Changed([wccLayout]);
end;

procedure TdxWizardControlHeader.TitleFontChanged(Sender: TObject);
begin
  Include(FAssignedValues, wchvTitleFont);
  Changed([wccLayout]);
end;

function TdxWizardControlHeader.IsBackgroundImageStored: Boolean;
begin
  Result := wchvBackgroundImage in AssignedValues;
end;

function TdxWizardControlHeader.IsDescriptionFontStored: Boolean;
begin
  Result := wchvDescriptionFont in AssignedValues;
end;

function TdxWizardControlHeader.IsDescriptionOffsetStored: Boolean;
begin
  Result := wchvDescriptionOffset in AssignedValues;
end;

function TdxWizardControlHeader.IsDescriptionVisibilityStored: Boolean;
begin
  Result := wchvDescriptionVisibility in AssignedValues;
end;

function TdxWizardControlHeader.IsGlyphVisibilityStored: Boolean;
begin
  Result := wchvGlyphVisibility in AssignedValues;
end;

function TdxWizardControlHeader.IsGlyphStored: Boolean;
begin
  Result := wchvGlyph in AssignedValues;
end;

function TdxWizardControlHeader.IsTitleFontStored: Boolean;
begin
  Result := wchvTitleFont in AssignedValues;
end;

function TdxWizardControlHeader.IsVisibleStored: Boolean;
begin
  Result := wchvVisible in AssignedValues;
end;

procedure TdxWizardControlHeader.SetAssignedValues(AValue: TdxWizardControlHeaderAssignedValues);
begin
  if FAssignedValues <> AValue then
  begin
    BeginUpdate;
    try
      if CheckResetValue(wchvBackgroundImage, AValue) then
        BackgroundImage.Reset;
      if CheckResetValue(wchvGlyph, AValue) then
        Glyph.Clear;
      if CheckResetValue(wchvTitleFont, AValue) then
        ResetFont(TitleFont);
      if CheckResetValue(wchvDescriptionFont, AValue) then
        ResetFont(DescriptionFont);
      if CheckResetValue(wchvDescriptionOffset, AValue) then
        DescriptionOffset := 0;
      if CheckResetValue(wchvDescriptionVisibility, AValue) then
        DescriptionVisibility := wcevDefault;
      if CheckResetValue(wchvGlyphVisibility, AValue) then
        GlyphVisibility := wcevDefault;
      if CheckResetValue(wchvVisible, AValue) then
        Visible := wcevDefault;
      FAssignedValues := AValue;
      Changed([wccLayout]);
    finally
      EndUpdate;
    end;
  end;
end;

procedure TdxWizardControlHeader.SetBackgroundImage(AValue: TdxWizardControlImage);
begin
  BackgroundImage.Assign(AValue);
end;

procedure TdxWizardControlHeader.SetDescriptionFont(AValue: TFont);
begin
  FDescriptionFont.Assign(AValue);
end;

procedure TdxWizardControlHeader.SetDescriptionOffset(const AValue: Integer);
begin
  if DescriptionOffset <> AValue then
  begin
    FDescriptionOffset := AValue;
    Include(FAssignedValues, wchvDescriptionOffset);
    Changed([wccLayout]);
  end;
end;

procedure TdxWizardControlHeader.SetDescriptionVisibility(AValue: TdxWizardControlElementVisibility);
begin
  if DescriptionVisibility <> AValue then
  begin
    Include(FAssignedValues, wchvDescriptionVisibility);
    FDescriptionVisibility := AValue;
    Changed([wccLayout]);
  end;
end;

procedure TdxWizardControlHeader.SetGlyph(AValue: TdxSmartGlyph);
begin
  FGlyph.Assign(AValue);
end;

procedure TdxWizardControlHeader.SetGlyphVisibility(AValue: TdxWizardControlElementVisibility);
begin
  if FGlyphVisibility <> AValue then
  begin
    Include(FAssignedValues, wchvGlyphVisibility);
    FGlyphVisibility := AValue;
    Changed([wccLayout]);
  end;
end;

procedure TdxWizardControlHeader.SetTitleFont(AValue: TFont);
begin
  FTitleFont.Assign(AValue);
end;

procedure TdxWizardControlHeader.SetVisible(AValue: TdxWizardControlElementVisibility);
begin
  if FVisible <> AValue then
  begin
    Include(FAssignedValues, wchvVisible);
    FVisible := AValue;
    Changed([wccLayout]);
  end;
end;

{ TdxWizardControlTitle }

constructor TdxWizardControlTitle.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FGlyph := TdxSmartGlyph.Create;
  FGlyph.OnChange := ChangeHandler;
  FFont := TFont.Create;
  FFont.Color := clDefault;
  FFont.OnChange := ChangeHandler;
end;

destructor TdxWizardControlTitle.Destroy;
begin
  FreeAndNil(FGlyph);
  FreeAndNil(FFont);
  inherited Destroy;
end;

procedure TdxWizardControlTitle.ChangeScale(M, D: Integer);
begin
  inherited ChangeScale(M, D);
  if IsFontStored then
    Font.Height := MulDiv(Font.Height, M, D);
end;

procedure TdxWizardControlTitle.DoAssign(Source: TPersistent);
begin
  inherited DoAssign(Source);
  if Source is TdxWizardControlTitle then
  begin
    Font := TdxWizardControlTitle(Source).Font;
    Glyph := TdxWizardControlTitle(Source).Glyph;
    Text := TdxWizardControlTitle(Source).Text;
  end;
end;

procedure TdxWizardControlTitle.ChangeHandler(Sender: TObject);
begin
  if Sender = Font then
    FFontChanged := True;
  Changed([wccLayout]);
end;

procedure TdxWizardControlTitle.InitializeFont;
begin
  if not IsFontStored then
  begin
    Font.Assign(TdxCustomWizardControl(TdxWizardControlOptionsViewStyleAero(Owner).Owner).Font);
    FFontChanged := False;
  end;
end;

function TdxWizardControlTitle.IsFontStored: Boolean;
begin
  Result := FFontChanged;
end;

procedure TdxWizardControlTitle.SetFont(AValue: TFont);
begin
  FFont.Assign(AValue);
end;

procedure TdxWizardControlTitle.SetGlyph(AValue: TdxSmartGlyph);
begin
  FGlyph.Assign(AValue);
end;

procedure TdxWizardControlTitle.SetText(const AValue: string);
begin
  if AValue <> FText then
  begin
    FText := AValue;
    Changed([wccLayout]);
  end;
end;

{ TdxWizardControlCustomTitleViewInfo }

procedure TdxWizardControlCustomTitleViewInfo.DrawBackground(ACanvas: TcxCanvas);
begin
  if not IsPaintOnGlass then
    Painter.DrawTitleBackground(ACanvas, Bounds);
end;

function TdxWizardControlCustomTitleViewInfo.GetFont: TFont;
begin
  Result := Title.Font;
end;

function TdxWizardControlCustomTitleViewInfo.GetTitle: TdxWizardControlTitle;
begin
  Result := WizardControl.OptionsViewStyleAero.Title;
end;

{ TdxWizardControlWatermarkViewInfo }

procedure TdxWizardControlWatermarkViewInfo.DrawBackground(ACanvas: TcxCanvas);
begin
  Painter.DrawBackground(ACanvas, Bounds);
end;

procedure TdxWizardControlWatermarkViewInfo.DrawContent(ACanvas: TcxCanvas);
begin
  if IsImageAvailable then
    WatermarkImage.Draw(ACanvas, Bounds, ScaleFactor);
end;

function TdxWizardControlWatermarkViewInfo.MeasureHeight: Integer;
begin
  if IsImageAvailable then
    Result := dxGetImageSize(WatermarkImage.Image, ScaleFactor).cy
  else
    Result := 0;
end;

function TdxWizardControlWatermarkViewInfo.MeasureWidth: Integer;
begin
  if IsImageAvailable then
    Result := dxGetImageSize(WatermarkImage.Image, ScaleFactor).cx
  else
    Result := 0;
end;

function TdxWizardControlWatermarkViewInfo.GetIsImageAvailable: Boolean;
begin
  Result := IsImageVisible and not WatermarkImage.Empty;
end;

function TdxWizardControlWatermarkViewInfo.GetWatermarkImage: TdxWizardControlImage;
begin
  if (WizardControl.ActivePage <> nil) and (wcwavBackgroundImage in WizardControl.ActivePage.Watermark.AssignedValues) then
    Result := WizardControl.ActivePage.Watermark.BackgroundImage
  else
    Result := WizardControl.Watermark.BackgroundImage;
end;

function TdxWizardControlWatermarkViewInfo.GetWatermarkVisibility: TdxWizardControlElementVisibility;
begin
  if (WizardControl.ActivePage <> nil) and (wcwavVisibility in WizardControl.ActivePage.Watermark.AssignedValues) then
    Result := WizardControl.ActivePage.Watermark.Visibility
  else
    Result := WizardControl.Watermark.Visibility;
end;

{ TdxWizardControlPageHeader }

constructor TdxWizardControlPageHeader.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FTitle := GetDefaultTitle;
  FDescription := GetDefaultDescription;
end;

procedure TdxWizardControlPageHeader.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('Description', ReadDescription, WriteDescription, IsDescriptionStored and (Description = ''));
  Filer.DefineProperty('Title', ReadTitle, WriteTitle, IsTitleStored and (Title = ''));
end;

procedure TdxWizardControlPageHeader.DoAssign(Source: TPersistent);
begin
  inherited DoAssign(Source);
  if Source is TdxWizardControlPageHeader then
  begin
    AssignedValues := TdxWizardControlPageHeader(Source).AssignedValues;
    Description := TdxWizardControlPageHeader(Source).Description;
    Title := TdxWizardControlPageHeader(Source).Title;
  end;
end;

function TdxWizardControlPageHeader.IsDescriptionStored: Boolean;
begin
  Result := Description <> GetDefaultDescription;
end;

function TdxWizardControlPageHeader.IsTitleStored: Boolean;
begin
  Result := Title <> GetDefaultTitle;
end;

function TdxWizardControlPageHeader.GetDefaultDescription: string;
begin
  Result := cxGetResourceString(@sdxWizardControlPageDefaultDescription);
end;

function TdxWizardControlPageHeader.GetDefaultTitle: string;
begin
  Result := cxGetResourceString(@sdxWizardControlPageDefaultTitle);
end;

procedure TdxWizardControlPageHeader.ReadDescription(Reader: TReader);
begin
  Description := Reader.ReadString;
end;

procedure TdxWizardControlPageHeader.ReadTitle(Reader: TReader);
begin
  Title := Reader.ReadString;
end;

procedure TdxWizardControlPageHeader.SetDescription(const AValue: string);
begin
  if Description <> AValue then
  begin
    FDescription := AValue;
    Changed([wccLayout]);
  end;
end;

procedure TdxWizardControlPageHeader.SetTitle(const AValue: string);
begin
  if Title <> AValue then
  begin
    FTitle := AValue;
    Changed([wccLayout]);
  end;
end;

procedure TdxWizardControlPageHeader.WriteDescription(Writer: TWriter);
begin
  Writer.WriteString(Description);
end;

procedure TdxWizardControlPageHeader.WriteTitle(Writer: TWriter);
begin
  Writer.WriteString(Title);
end;

{ TdxWizardControlCustomButton }

constructor TdxWizardControlCustomButton.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FEnabled := True;
  FVisible := True;
  FGlyph := TdxSmartGlyph.Create;
  FGlyph.OnChange := GlyphChangeHandler;
  Caption := GetDefaultCaption;
  FImageIndex := -1;
end;

destructor TdxWizardControlCustomButton.Destroy;
begin
  FreeAndNil(FGlyph);
  inherited Destroy;
end;

procedure TdxWizardControlCustomButton.ChangeScale(M: Integer; D: Integer);
begin
  inherited ChangeScale(M, D);
  Width := MulDiv(Width, M ,D);
end;

procedure TdxWizardControlCustomButton.Click;
begin
  if ActuallyEnabled then
    DoClick;
end;

procedure TdxWizardControlCustomButton.DoAssign(Source: TPersistent);
begin
  if Source is TdxWizardControlCustomButton then
  begin
    Caption := TdxWizardControlCustomButton(Source).Caption;
    Enabled := TdxWizardControlCustomButton(Source).Enabled;
    Glyph := TdxWizardControlCustomButton(Source).Glyph;
    GlyphAlignment := TdxWizardControlCustomButton(Source).GlyphAlignment;
    ImageIndex := TdxWizardControlCustomButton(Source).ImageIndex;
    Visible := TdxWizardControlCustomButton(Source).Visible;
    Width := TdxWizardControlCustomButton(Source).Width;
  end;
end;

procedure TdxWizardControlCustomButton.DoClick;
begin
  dxCallNotify(OnClick, WizardControl);
end;

function TdxWizardControlCustomButton.GetActuallyEnabled: Boolean;
begin
  Result := Buttons.Common.Enabled and Enabled;
end;

function TdxWizardControlCustomButton.GetDefaultCaption: string;
begin
  Result := '';
end;

procedure TdxWizardControlCustomButton.GlyphChangeHandler(Sender: TObject);
begin
  Changed([wccLayout]);
end;

function TdxWizardControlCustomButton.IsCaptionStored: Boolean;
begin
  Result := not FCaptionIsDefault;
end;

function TdxWizardControlCustomButton.GetButtons: TdxWizardControlButtons;
begin
  Result := Owner as TdxWizardControlButtons;
end;

function TdxWizardControlCustomButton.GetFocused: Boolean;
var
  AViewInfo: TdxWizardControlCellViewInfo;
begin
  AViewInfo := WizardControl.ViewInfo.CommandAreaViewInfo.GetButtonViewInfo(Self);
  Result := (AViewInfo <> nil) and AViewInfo.IsFocused;
end;

function TdxWizardControlCustomButton.GetWizardControl: TdxCustomWizardControl;
begin
  Result := Buttons.Owner as TdxCustomWizardControl;
end;

procedure TdxWizardControlCustomButton.SetCaption(const AValue: string);
begin
  if Caption <> AValue then
  begin
    FCaption := AValue;
    FCaptionIsDefault := Caption = GetDefaultCaption;
    Changed([wccLayout]);
  end;
end;

procedure TdxWizardControlCustomButton.SetEnabled(const AValue: Boolean);
begin
  if FEnabled <> AValue then
  begin
    FEnabled := AValue;
    Changed([wccContent]);
  end;
end;

procedure TdxWizardControlCustomButton.SetFocused(const AValue: Boolean);
var
  AViewInfo: TdxWizardControlCellViewInfo;
begin
  if AValue then
  begin
    AViewInfo := WizardControl.ViewInfo.CommandAreaViewInfo.GetButtonViewInfo(Self);
    if (AViewInfo <> nil) and AViewInfo.CanFocus then
    begin
      WizardControl.SetFocus;
      if WizardControl.Focused then
        WizardControl.Controller.FocusedCell := AViewInfo;
    end;
  end;
end;

procedure TdxWizardControlCustomButton.SetGlyph(AValue: TdxSmartGlyph);
begin
  FGlyph.Assign(AValue);
end;

procedure TdxWizardControlCustomButton.SetGlyphAlignment(const AValue: TButtonLayout);
begin
  if GlyphAlignment <> AValue then
  begin
    FGlyphAlignment := AValue;
    Changed([wccLayout]);
  end;
end;

procedure TdxWizardControlCustomButton.SetImageIndex(AValue: TcxImageIndex);
begin
  if FImageIndex <> AValue then
  begin
    FImageIndex := AValue;
    Changed([wccLayout]);
  end;
end;

procedure TdxWizardControlCustomButton.SetVisible(const AValue: Boolean);
begin
  if Visible <> AValue then
  begin
    FVisible := AValue;
    Changed([wccStruct]);
  end;
end;

procedure TdxWizardControlCustomButton.SetWidth(const AValue: Integer);
begin
  if Width <> AValue then
  begin
    FWidth := AValue;
    Changed([wccLayout]);
  end;
end;

{ TdxWizardControlBackButton }

function TdxWizardControlBackButton.GetActuallyEnabled: Boolean;
begin
  Result := inherited GetActuallyEnabled and WizardControl.CanGoToPrevPage;
end;

function TdxWizardControlBackButton.GetDefaultCaption: string;
begin
  Result := cxGetResourceString(@sdxWizardControlButtonBack);
end;

procedure TdxWizardControlBackButton.DoClick;
begin
  if not WizardControl.DoButtonClick(wcbkBack) then
    WizardControl.GoToPrevPage;
end;

{ TdxWizardControlCancelButton }

function TdxWizardControlCancelButton.GetDefaultCaption: string;
begin
  Result := cxGetResourceString(@sdxWizardControlButtonCancel);
end;

procedure TdxWizardControlCancelButton.DoClick;
begin
  WizardControl.DoButtonClick(wcbkCancel);
end;

{ TdxWizardControlFinishButton }

function TdxWizardControlFinishButton.GetDefaultCaption: string;
begin
  Result := cxGetResourceString(@sdxWizardControlButtonFinish);
end;

procedure TdxWizardControlFinishButton.DoAssign(Source: TPersistent);
begin
  inherited DoAssign(Source);
  if Source is TdxWizardControlFinishButton then
  begin
    AlwaysVisible := TdxWizardControlFinishButton(Source).AlwaysVisible;
  end;
end;

procedure TdxWizardControlFinishButton.DoClick;
begin
  WizardControl.DoButtonClick(wcbkFinish);
end;

procedure TdxWizardControlFinishButton.SetAlwaysVisible(const AValue: Boolean);
begin
  if AlwaysVisible <> AValue then
  begin
    FAlwaysVisible := AValue;
    Changed([wccLayout]);
  end;
end;

{ TdxWizardControlHelpButton }

function TdxWizardControlHelpButton.GetDefaultCaption: string;
begin
  Result := cxGetResourceString(@sdxWizardControlButtonHelp);
end;

procedure TdxWizardControlHelpButton.DoClick;
begin
  WizardControl.DoButtonClick(wcbkHelp);
end;

{ TdxWizardControlNextButton }

function TdxWizardControlNextButton.GetActuallyEnabled: Boolean;
begin
  Result := inherited GetActuallyEnabled and WizardControl.CanGoToNextPage;
end;

function TdxWizardControlNextButton.GetDefaultCaption: string;
begin
  Result := cxGetResourceString(@sdxWizardControlButtonNext);
end;

procedure TdxWizardControlNextButton.DoClick;
begin
  if not WizardControl.DoButtonClick(wcbkNext) then
    WizardControl.GoToNextPage;
end;

{ TdxWizardControlOptionsButtons }

constructor TdxWizardControlOptionsButtons.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FEnabled := True;
end;

procedure TdxWizardControlOptionsButtons.ChangeScale(M: Integer; D: Integer);
begin
  inherited ChangeScale(M, D);
  Height := MulDiv(Height, M, D);
  Width := MulDiv(Width, M, D);
end;

procedure TdxWizardControlOptionsButtons.DoAssign(Source: TPersistent);
begin
  inherited DoAssign(Source);
  if Source is TdxWizardControlOptionsButtons then
  begin
    Enabled := TdxWizardControlOptionsButtons(Source).Enabled;
    Height := TdxWizardControlOptionsButtons(Source).Height;
    Width := TdxWizardControlOptionsButtons(Source).Width;
  end;
end;

procedure TdxWizardControlOptionsButtons.SetEnabled(const AValue: Boolean);
begin
  if Enabled <> AValue then
  begin
    FEnabled := AValue;
    Changed([wccContent]);
  end;
end;

procedure TdxWizardControlOptionsButtons.SetHeight(const AValue: Integer);
begin
  if Height <> AValue then
  begin
    FHeight := AValue;
    Changed([wccLayout]);
  end;
end;

procedure TdxWizardControlOptionsButtons.SetWidth(const AValue: Integer);
begin
  if Width <> AValue then
  begin
    FWidth := AValue;
    Changed([wccLayout]);
  end;
end;

{ TdxWizardControlCustomButtonCollection }

function TdxWizardControlCustomButtonCollection.Add: TdxWizardControlCustomButtonCollectionItem;
begin
  Result := TdxWizardControlCustomButtonCollectionItem(inherited Add);
end;

procedure TdxWizardControlCustomButtonCollection.ChangeScale(M: Integer; D: Integer);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].ChangeScale(M, D);
end;

procedure TdxWizardControlCustomButtonCollection.Update(Item: TCollectionItem);
begin
  CustomButtons.Changed([wccStruct]);
end;

function TdxWizardControlCustomButtonCollection.GetCustomButtons: TdxWizardControlCustomButtons;
begin
  Result := Owner as TdxWizardControlCustomButtons;
end;

function TdxWizardControlCustomButtonCollection.GetItem(Index: Integer): TdxWizardControlCustomButtonCollectionItem;
begin
  Result := TdxWizardControlCustomButtonCollectionItem(inherited GetItem(Index));
end;

procedure TdxWizardControlCustomButtonCollection.SetItem(Index: Integer;
  AValue: TdxWizardControlCustomButtonCollectionItem);
begin
  inherited SetItem(Index, AValue);
end;

{ TdxWizardControlCustomButtonCollectionItem }

constructor TdxWizardControlCustomButtonCollectionItem.Create(Collection: TCollection);
var
  ACustomButtonCollection: TdxWizardControlCustomButtonCollection;
begin
  inherited Create(Collection);
  ACustomButtonCollection := Collection as TdxWizardControlCustomButtonCollection;
  FButton := TdxWizardControlCustomButton.Create(ACustomButtonCollection.CustomButtons.OwnerButtons);
  FButton.OnChange := ACustomButtonCollection.CustomButtons.OnChange;
end;

destructor TdxWizardControlCustomButtonCollectionItem.Destroy;
begin
  FreeAndNil(FButton);
  inherited Destroy;
end;

procedure TdxWizardControlCustomButtonCollectionItem.Assign(Source: TPersistent);
begin
  if Source is TdxWizardControlCustomButtonCollectionItem then
    FButton.Assign(TdxWizardControlCustomButtonCollectionItem(Source).Button)
  else
    if Source is TdxWizardControlCustomButton then
      FButton.Assign(TdxWizardControlCustomButton(Source))
    else
      inherited Assign(Source);
end;

procedure TdxWizardControlCustomButtonCollectionItem.ChangeScale(M: Integer; D: Integer);
begin
  Button.ChangeScale(M, D);
end;

procedure TdxWizardControlCustomButtonCollectionItem.Click;
begin
  Button.Click;
end;

function TdxWizardControlCustomButtonCollectionItem.GetCaption: string;
begin
  Result := Button.Caption;
end;

function TdxWizardControlCustomButtonCollectionItem.GetEnabled: Boolean;
begin
  Result := Button.Enabled;
end;

function TdxWizardControlCustomButtonCollectionItem.GetFocused: Boolean;
begin
  Result := Button.Focused;
end;

function TdxWizardControlCustomButtonCollectionItem.GetGlyph: TdxSmartGlyph;
begin
  Result := Button.Glyph;
end;

function TdxWizardControlCustomButtonCollectionItem.GetGlyphAlignment: TButtonLayout;
begin
  Result := Button.GlyphAlignment;
end;

function TdxWizardControlCustomButtonCollectionItem.GetImageIndex: TcxImageIndex;
begin
  Result := Button.ImageIndex;
end;

function TdxWizardControlCustomButtonCollectionItem.GetOnClick: TNotifyEvent;
begin
  Result := Button.OnClick;
end;

function TdxWizardControlCustomButtonCollectionItem.GetVisible: Boolean;
begin
  Result := Button.Visible;
end;

function TdxWizardControlCustomButtonCollectionItem.GetWidth: Integer;
begin
  Result := Button.Width;
end;

procedure TdxWizardControlCustomButtonCollectionItem.SetCaption(const AValue: string);
begin
  Button.Caption := AValue;
end;

procedure TdxWizardControlCustomButtonCollectionItem.SetEnabled(const AValue: Boolean);
begin
  Button.Enabled := AValue;
end;

procedure TdxWizardControlCustomButtonCollectionItem.SetFocused(const AValue: Boolean);
begin
  Button.Focused := AValue;
end;

procedure TdxWizardControlCustomButtonCollectionItem.SetGlyph(AValue: TdxSmartGlyph);
begin
  Button.Glyph := AValue;
end;

procedure TdxWizardControlCustomButtonCollectionItem.SetGlyphAlignment(const AValue: TButtonLayout);
begin
  Button.GlyphAlignment := AValue;
end;

procedure TdxWizardControlCustomButtonCollectionItem.SetImageIndex(AValue: TcxImageIndex);
begin
  Button.ImageIndex := AValue;
end;

procedure TdxWizardControlCustomButtonCollectionItem.SetOnClick(AValue: TNotifyEvent);
begin
  Button.OnClick := AValue;
end;

procedure TdxWizardControlCustomButtonCollectionItem.SetVisible(const AValue: Boolean);
begin
  Button.Visible := AValue;
end;

procedure TdxWizardControlCustomButtonCollectionItem.SetWidth(const AValue: Integer);
begin
  Button.Width := AValue;
end;

{ TdxWizardControlCustomButtons }

constructor TdxWizardControlCustomButtons.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FButtons := TdxWizardControlCustomButtonCollection.Create(Self, TdxWizardControlCustomButtonCollectionItem);
end;

destructor TdxWizardControlCustomButtons.Destroy;
begin
  FreeAndNil(FButtons);
  inherited Destroy;
end;

procedure TdxWizardControlCustomButtons.ChangeScale(M: Integer; D: Integer);
begin
  inherited ChangeScale(M, D);
  Buttons.ChangeScale(M, D);
end;

procedure TdxWizardControlCustomButtons.DoAssign(Source: TPersistent);
begin
  inherited DoAssign(Source);
  if Source is TdxWizardControlCustomButtons then
  begin
    Alignment := TdxWizardControlCustomButtons(Source).Alignment;
    Buttons := TdxWizardControlCustomButtons(Source).Buttons;
  end;
end;

function TdxWizardControlCustomButtons.GetOwnerButtons: TdxWizardControlButtons;
begin
  Result := Owner as TdxWizardControlButtons;
end;

procedure TdxWizardControlCustomButtons.SetAlignment(const AValue: TdxWizardControlCustomButtonsAlignment);
begin
  if Alignment <> AValue then
  begin
    FAlignment := AValue;
    Changed([wccStruct]);
  end;
end;

procedure TdxWizardControlCustomButtons.SetButtons(AValue: TdxWizardControlCustomButtonCollection);
begin
  FButtons.Assign(AValue);
end;

{ TdxWizardControlButtons }

constructor TdxWizardControlButtons.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FBack := TdxWizardControlBackButton.Create(Self);
  FBack.OnChange := ChangeHandler;
  FNext := TdxWizardControlNextButton.Create(Self);
  FNext.OnChange := ChangeHandler;
  FHelp := TdxWizardControlHelpButton.Create(Self);
  FHelp.OnChange := ChangeHandler;
  FCancel := TdxWizardControlCancelButton.Create(Self);
  FCancel.OnChange := ChangeHandler;
  FFinish := TdxWizardControlFinishButton.Create(Self);
  FFinish.OnChange := ChangeHandler;
  FFreeNotificator := TcxFreeNotificator.Create(nil);
  FFreeNotificator.OnFreeNotification := FreeNotification;
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
  FCommon := TdxWizardControlOptionsButtons.Create(Self);
  FCommon.OnChange := ChangeHandler;
  FCustomButtons := TdxWizardControlCustomButtons.Create(Self);
  FCustomButtons.OnChange := ChangeHandler;
end;

destructor TdxWizardControlButtons.Destroy;
begin
  FreeAndNil(FBack);
  FreeAndNil(FCancel);
  FreeAndNil(FFinish);
  FreeAndNil(FHelp);
  FreeAndNil(FNext);
  FreeAndNil(FFreeNotificator);
  FreeAndNil(FImageChangeLink);
  FreeAndNil(FCommon);
  FreeAndNil(FCustomButtons);
  inherited Destroy;
end;

procedure TdxWizardControlButtons.ChangeHandler(Sender: TObject; AChanges: TdxWizardControlChanges);
begin
  Changed(AChanges);
end;

procedure TdxWizardControlButtons.ChangeScale(M: Integer; D: Integer);
begin
  inherited ChangeScale(M, D);
  Back.ChangeScale(M, D);
  Cancel.ChangeScale(M, D);
  Common.ChangeScale(M, D);
  CustomButtons.ChangeScale(M, D);
  Finish.ChangeScale(M, D);
  Help.ChangeScale(M, D);
  Next.ChangeScale(M, D);
end;

procedure TdxWizardControlButtons.ImageListChange(Sender: TObject);
begin
  if Sender = Images then
    Changed([wccStruct]);
end;

procedure TdxWizardControlButtons.DoAssign(Source: TPersistent);
begin
  inherited DoAssign(Source);
  if Source is TdxWizardControlButtons then
  begin
    Help := TdxWizardControlButtons(Source).Help;
    Back := TdxWizardControlButtons(Source).Back;
    Next := TdxWizardControlButtons(Source).Next;
    Cancel := TdxWizardControlButtons(Source).Cancel;
    Finish := TdxWizardControlButtons(Source).Finish;
    Images := TdxWizardControlButtons(Source).Images;
    Common := TdxWizardControlButtons(Source).Common;
    CustomButtons := TdxWizardControlButtons(Source).CustomButtons;
  end;
end;

procedure TdxWizardControlButtons.FreeNotification(Sender: TComponent);
begin
  if Sender = Images then
    Images := nil;
end;

procedure TdxWizardControlButtons.UpdateTranslation;

  procedure UpdateButtonTranslation(AButton: TdxWizardControlCustomButton);
  begin
    if AButton.CaptionIsDefault then
      AButton.Caption := AButton.GetDefaultCaption;
  end;

begin
  UpdateButtonTranslation(Back);
  UpdateButtonTranslation(Cancel);
  UpdateButtonTranslation(Finish);
  UpdateButtonTranslation(Help);
  UpdateButtonTranslation(Next);
end;

procedure TdxWizardControlButtons.SetBack(AValue: TdxWizardControlBackButton);
begin
  FBack.Assign(AValue);
end;

procedure TdxWizardControlButtons.SetCancel(AValue: TdxWizardControlCancelButton);
begin
  FCancel.Assign(AValue);
end;

procedure TdxWizardControlButtons.SetCommon(AValue: TdxWizardControlOptionsButtons);
begin
  FCommon.Assign(AValue);
end;

procedure TdxWizardControlButtons.SetCustomButtons(AValue: TdxWizardControlCustomButtons);
begin
  FCustomButtons.Assign(AValue);
end;

procedure TdxWizardControlButtons.SetFinish(AValue: TdxWizardControlFinishButton);
begin
  Finish.Assign(AValue);
end;

procedure TdxWizardControlButtons.SetHelp(AValue: TdxWizardControlHelpButton);
begin
  FHelp.Assign(AValue);
end;

procedure TdxWizardControlButtons.SetImages(AValue: TCustomImageList);
begin
  cxSetImageList(AValue, FImages, FImageChangeLink, FreeNotificator);
end;

procedure TdxWizardControlButtons.SetNext(AValue: TdxWizardControlNextButton);
begin
  FNext.Assign(AValue);
end;

{ TdxWizardControlOptionsAnimate }

procedure TdxWizardControlOptionsAnimate.DoAssign(Source: TPersistent);
begin
  inherited DoAssign(Source);
  if Source is TdxWizardControlOptionsAnimate then
  begin
    ResizeAnimation := TdxWizardControlOptionsAnimate(Source).ResizeAnimation;
    TransitionEffect := TdxWizardControlOptionsAnimate(Source).TransitionEffect;
  end;
end;

{ TdxWizardControlOptionsViewStyleAero }

constructor TdxWizardControlOptionsViewStyleAero.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FTitle := TdxWizardControlTitle.Create(Self);
  FTitle.OnChange := ChangeHandler;
  FEnableTitleAero := True;
end;

destructor TdxWizardControlOptionsViewStyleAero.Destroy;
begin
  FreeAndNil(FTitle);
  inherited Destroy;
end;

procedure TdxWizardControlOptionsViewStyleAero.ChangeScale(M, D: Integer);
begin
  inherited ChangeScale(M, D);
  Title.ChangeScale(M, D);
end;

procedure TdxWizardControlOptionsViewStyleAero.DoAssign(Source: TPersistent);
begin
  inherited DoAssign(Source);
  if Source is TdxWizardControlOptionsViewStyleAero then
  begin
    EnableTitleAero := TdxWizardControlOptionsViewStyleAero(Source).EnableTitleAero;
    Title := TdxWizardControlOptionsViewStyleAero(Source).Title;
  end;
end;

procedure TdxWizardControlOptionsViewStyleAero.ChangeHandler(
  Sender: TObject; AChanges: TdxWizardControlChanges);
begin
  Changed(AChanges);
end;

procedure TdxWizardControlOptionsViewStyleAero.SetEnableTitleAero(AValue: Boolean);
begin
  if AValue <> FEnableTitleAero then
  begin
    FEnableTitleAero := AValue;
    Changed([wccStruct]);
  end;
end;

procedure TdxWizardControlOptionsViewStyleAero.SetTitle(AValue: TdxWizardControlTitle);
begin
  FTitle.Assign(AValue);
end;

{ TdxWizardControlWatermark }

constructor TdxWizardControlWatermark.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FBackgroundImage := TdxWizardControlImage.Create(Self);
  FBackgroundImage.OnChange := BackgroundImageChanged;
end;

destructor TdxWizardControlWatermark.Destroy;
begin
  FreeAndNil(FBackgroundImage);
  inherited Destroy;
end;

procedure TdxWizardControlWatermark.BackgroundImageChanged(
  Sender: TObject; AChanges: TdxWizardControlChanges);
begin
  BeginUpdate;
  try
    AssignedValues := AssignedValues + [wcwavBackgroundImage];
    Changed(AChanges);
  finally
    EndUpdate;
  end;
end;

function TdxWizardControlWatermark.CheckResetValue(AValue: TdxWizardControlWatermarkAssignedValue;
  const ANewAssignedValues: TdxWizardControlWatermarkAssignedValues): Boolean;
begin
  Result := (AValue in AssignedValues) and not (AValue in ANewAssignedValues);
end;

procedure TdxWizardControlWatermark.DoAssign(Source: TPersistent);
begin
  inherited DoAssign(Source);
  if Source is TdxWizardControlWatermark then
  begin
    BackgroundImage := TdxWizardControlWatermark(Source).BackgroundImage;
    Visibility := TdxWizardControlWatermark(Source).Visibility;
    AssignedValues := TdxWizardControlWatermark(Source).AssignedValues;
  end;
end;

function TdxWizardControlWatermark.IsVisibilityStored: Boolean;
begin
  Result := Visibility <> wcevDefault;
end;

procedure TdxWizardControlWatermark.SetAssignedValues(AValue: TdxWizardControlWatermarkAssignedValues);
begin
  if FAssignedValues <> AValue then
  begin
    BeginUpdate;
    try
      if CheckResetValue(wcwavBackgroundImage, AValue) then
        BackgroundImage.Reset;
      if CheckResetValue(wcwavVisibility, AValue) then
        Visibility := wcevDefault;
      FAssignedValues := AValue;
      Changed([wccLayout]);
    finally
      EndUpdate;
    end;
  end;
end;

procedure TdxWizardControlWatermark.SetBackgroundImage(AValue: TdxWizardControlImage);
begin
  FBackgroundImage.Assign(AValue);
end;

procedure TdxWizardControlWatermark.SetVisibility(AValue: TdxWizardControlElementVisibility);
begin
  BeginUpdate;
  try
    AssignedValues := AssignedValues + [wcwavVisibility];
    if FVisibility <> AValue then
    begin
      FVisibility := AValue;
      Changed([wccLayout]);
    end;
  finally
    EndUpdate;
  end;
end;

{ TdxWizardControlPageWatermark }

function TdxWizardControlPageWatermark.IsBackgroundImageStored: Boolean;
begin
  Result := wcwavBackgroundImage in AssignedValues;
end;

function TdxWizardControlPageWatermark.IsVisibilityStored: Boolean;
begin
  Result := wcwavVisibility in AssignedValues;
end;

{ TdxWizardControlInfoPanel }

constructor TdxWizardControlInfoPanel.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FFont := TFont.Create;
  FFont.Color := clDefault;
  FFont.OnChange := FontChangeHandler;
  FHyperlinkColor := clDefault;
  FEnabled := True;
  FVisible := True;
end;

destructor TdxWizardControlInfoPanel.Destroy;
begin
  FreeAndNil(FFont);
  inherited Destroy;
end;

procedure TdxWizardControlInfoPanel.ChangeScale(M, D: Integer);
begin
  inherited ChangeScale(M, D);
  if IsFontStored then
    Font.Height := MulDiv(Font.Height, M, D);
end;

procedure TdxWizardControlInfoPanel.DoAssign(Source: TPersistent);
begin
  inherited DoAssign(Source);
  if Source is TdxWizardControlInfoPanel then
  begin
    Caption := TdxWizardControlInfoPanel(Source).Caption;
    Enabled := TdxWizardControlInfoPanel(Source).Enabled;
    Font := TdxWizardControlInfoPanel(Source).Font;
    Hyperlink := TdxWizardControlInfoPanel(Source).Hyperlink;
    HyperlinkColor := TdxWizardControlInfoPanel(Source).HyperlinkColor;
    Visible := TdxWizardControlInfoPanel(Source).Visible;
  end;
end;

procedure TdxWizardControlInfoPanel.FontChangeHandler(Sender: TObject);
begin
  FFontChanged := True;
  Changed([wccLayout]);
end;

procedure TdxWizardControlInfoPanel.InitializeFont;
begin
  if not IsFontStored then
  begin
    Font.Assign(TdxCustomWizardControl(Owner).Font);
    FFontChanged := False;
  end;
end;

function TdxWizardControlInfoPanel.IsFontStored: Boolean;
begin
  Result := FFontChanged;
end;

procedure TdxWizardControlInfoPanel.SetCaption(const AValue: string);
begin
  if Caption <> AValue then
  begin
    FCaption := AValue;
    Changed([wccLayout]);
  end;
end;

procedure TdxWizardControlInfoPanel.SetEnabled(const AValue: Boolean);
begin
  if Enabled <> AValue then
  begin
    FEnabled := AValue;
    Changed([wccContent]);
  end;
end;

procedure TdxWizardControlInfoPanel.SetFont(AValue: TFont);
begin
  FFont.Assign(AValue);
end;

procedure TdxWizardControlInfoPanel.SetHyperlink(const AValue: string);
begin
  if Hyperlink <> AValue then
  begin
    FHyperlink := AValue;
    Changed([wccLayout]);
  end;
end;

procedure TdxWizardControlInfoPanel.SetHyperlinkColor(const Value: TColor);
begin
  if FHyperlinkColor <> Value then
  begin
    FHyperlinkColor := Value;
    Changed([wccContent]);
  end;
end;

procedure TdxWizardControlInfoPanel.SetVisible(const AValue: Boolean);
begin
  if Visible <> AValue then
  begin
    FVisible := AValue;
    Changed([wccStruct]);
  end;
end;

{ TdxWizardControlController }

constructor TdxWizardControlController.Create(AWizardControl: TdxCustomWizardControl);
begin
  inherited Create(AWizardControl);
  FHitTest := TdxWizardControlHitTest.Create(WizardControl);
end;

destructor TdxWizardControlController.Destroy;
begin
  FreeAndNil(FHitTest);
  inherited Destroy;
end;

procedure TdxWizardControlController.CalculateHitTest(X, Y: Integer);
begin
  HitTest.Calculate(Point(X, Y));
  WizardControl.Cursor := HitTest.Cursor;
end;

procedure TdxWizardControlController.CellRemoving(ACell: TdxWizardControlCellViewInfo);
begin
  if HotCell = ACell then
    FHotCell := nil;
  if PressedCell = nil then
    FPressedCell := nil;
  if FPrevFocusedCell = ACell then
    FPrevFocusedCell := nil;
  if FocusedCell = ACell then
    FFocusedCell := nil;
end;

function TdxWizardControlController.FindNextFocusableCell(
  ACell: TdxWizardControlCellViewInfo; AGoForward: Boolean): TdxWizardControlCellViewInfo;
const
  StepMap: array[Boolean] of Integer = (-1, 1);
var
  AIndex: Integer;
  ATabOrderList: TdxWizardControlCellViewInfoList;
begin
  Result := nil;
  ATabOrderList := TdxWizardControlCellViewInfoList.Create(False);
  try
    ViewInfo.GetTabOrderList(ATabOrderList);
    AIndex := ATabOrderList.IndexOf(ACell) + StepMap[AGoForward];
    while (AIndex >= 0) and (AIndex < ATabOrderList.Count) do
    begin
      if ATabOrderList[AIndex].CanFocus then
      begin
        Result := ATabOrderList[AIndex];
        Break;
      end;
      Inc(AIndex, StepMap[AGoForward]);
    end;
  finally
    ATabOrderList.Free;
  end;
end;

procedure TdxWizardControlController.FocusKill;
begin
  FPrevFocusedCell := FocusedCell;
  FocusedCell := nil;
end;

procedure TdxWizardControlController.FocusNextCell(AGoForward: Boolean);
begin
  FocusedCell := FindNextFocusableCell(FocusedCell, AGoForward);
end;

procedure TdxWizardControlController.FocusSet;
begin
  if FocusedCell = nil then
    FocusedCell := FPrevFocusedCell;
  if FocusedCell = nil then
    FocusNextCell(True);
end;

procedure TdxWizardControlController.KeyDown(AKey: Word; AShiftState: TShiftState);
begin
  if AKey = VK_SPACE then
  begin
    if [ssCtrl, ssShift, ssAlt] * AShiftState = [] then
      PressedCell := FocusedCell;
  end;
end;

procedure TdxWizardControlController.KeyUp(AKey: Word; AShiftState: TShiftState);
begin
  if AKey = VK_SPACE then
  begin
    if PressedCell = FocusedCell then
      ProcessClick(PressedCell);
    PressedCell := nil;
  end;
end;

procedure TdxWizardControlController.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  CalculateHitTest(X, Y);
  PressedCell := HitTest.HitObject;
  FocusedCell := HitTest.HitObject;
end;

procedure TdxWizardControlController.MouseLeave(AControl: TControl);
begin
  HotCell := nil;
end;

procedure TdxWizardControlController.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  CalculateHitTest(X, Y);
  HotCell := HitTest.HitObject;
end;

procedure TdxWizardControlController.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  CalculateHitTest(X, Y);
  if HitTest.HitObject = PressedCell then
    ProcessClick(PressedCell);
  PressedCell := nil;
end;

procedure TdxWizardControlController.ProcessAccel(ACell: TdxWizardControlCellViewInfo);
begin
  FocusedCell := ACell;
  ProcessClick(ACell);
end;

function TdxWizardControlController.ProcessChildKey(AKey: Word; AShiftState: TShiftState): Boolean;
begin
  Result := False;
  if [ssAlt, ssCtrl] * AShiftState = [] then
  begin
    case AKey of
      VK_TAB:
        begin
          FocusNextCell(GetKeyState(VK_SHIFT) >= 0);
          Result := FocusedCell <> nil;
        end;

      VK_LEFT, VK_RIGHT, VK_UP, VK_DOWN:
        begin
          FocusNextCell((AKey = VK_RIGHT) or (AKey = VK_DOWN));
          Result := FocusedCell <> nil;
        end;
    end;
  end
  else
    if [ssAlt, ssCtrl] * AShiftState = [ssAlt] then
    begin
      case AKey of
        VK_LEFT:
          WizardControl.Buttons.Back.Click;
        VK_RIGHT:
          WizardControl.Buttons.Next.Click;
      end;
    end;
end;

procedure TdxWizardControlController.ProcessClick(ACell: TdxWizardControlCellViewInfo);
begin
  if ACell is TdxWizardControlButtonViewInfo then
    TdxWizardControlButtonViewInfo(ACell).Button.Click
  else

  if ACell is TdxWizardControlInfoPanelViewInfo then
    ProcessInfoPanelClick(TdxWizardControlInfoPanelViewInfo(ACell).InfoPanel);
end;

function TdxWizardControlController.ProcessDialogChar(ACharCode: Word): Boolean;
begin
  Result := WizardControl.ViewInfo.ProcessDialogChar(ACharCode);
end;

procedure TdxWizardControlController.ProcessInfoPanelClick(AInfoPanel: TdxWizardControlInfoPanel);
begin
  if not WizardControl.DoInfoPanelClick then
  begin
    if AInfoPanel.Hyperlink <> '' then
      dxShellExecute(WizardControl.Handle, AInfoPanel.Hyperlink);
  end;
end;

procedure TdxWizardControlController.RefreshState;
begin
  if not WizardControl.IsDestroying then
    ViewInfo.RefreshState;
end;

procedure TdxWizardControlController.ValidateFocusedCell;
begin
  if (FocusedCell <> nil) and not FocusedCell.CanFocus then
    FocusedCell := FindNextFocusableCell(FocusedCell, True);
end;

function TdxWizardControlController.GetViewInfo: TdxWizardControlViewInfo;
begin
  Result := WizardControl.ViewInfo;
end;

procedure TdxWizardControlController.SetFocusedCell(AValue: TdxWizardControlCellViewInfo);
var
  APrevFocusedCell: TdxWizardControlCellViewInfo;
begin
  if (FFocusedCell <> AValue) and ((AValue = nil) or AValue.CanFocus) then
  begin
    APrevFocusedCell := FocusedCell;
    FFocusedCell := AValue;
    RefreshState;
    if APrevFocusedCell <> nil then
      APrevFocusedCell.Invalidate;
    if FocusedCell <> nil then
      FocusedCell.Invalidate;
  end;
end;

procedure TdxWizardControlController.SetHotCell(AValue: TdxWizardControlCellViewInfo);
begin
  if FHotCell <> AValue then
  begin
    FHotCell := AValue;
    RefreshState;
  end;
end;

procedure TdxWizardControlController.SetPressedCell(AValue: TdxWizardControlCellViewInfo);
begin
  if FPressedCell <> AValue then
  begin
    FPressedCell := AValue;
    RefreshState;
  end;
end;

{ TdxWizardControlDesignSelectorHelper }

function TdxWizardControlDesignSelectorHelper.IsHitTestTransparent(const P: TPoint): Boolean;
begin
  Result := False;
end;

{ TdxWizardControlAnimationController }

procedure TdxWizardControlAnimationController.AnimateTransition(ANewPage, APrevPage: TdxWizardControlCustomPage);
var
  AAnimationMode: TdxDrawAnimationMode;
begin
  FActive := True;
  try
    PrepareForAnimation;
    AAnimationMode := CalculateAnimationMode(ANewPage, APrevPage);
    if (AAnimationMode = amFade) or AutoSize then
      ProcessMultiStageAnimation(ANewPage, APrevPage, AAnimationMode)
    else
      ProcessSingleStageAnimation(ANewPage, APrevPage, AAnimationMode);
  finally
    FActive := False;
  end;
end;

procedure TdxWizardControlAnimationController.AnimationHandler(
  Sender: TdxAnimationTransition; var APosition: Integer; var AFinished: Boolean);
begin
  if AFinished then
    RedrawArea(WizardControl.ClientRect)
  else
    RedrawArea(AnimatedRect);
end;

function TdxWizardControlAnimationController.CalculateAnimationMode(
  ANewPage, APrevPage: TdxWizardControlCustomPage): TdxDrawAnimationMode;
begin
  case WizardControl.OptionsAnimate.TransitionEffect of
    wcteSlide:
      if ANewPage.PageIndex > APrevPage.PageIndex then
        Result := amScrollLeft
      else
        Result := amScrollRight;

    else
      Result := amFade;
  end;
end;

procedure TdxWizardControlAnimationController.Draw(ACanvas: TcxCanvas);
begin
  if Animation <> nil then
    Animation.Draw(ACanvas.Canvas, AnimatedRect)
end;

function TdxWizardControlAnimationController.PrepareBlankBitmap: TcxBitmap;
begin
  Result := TcxBitmap.CreateSize(AnimatedRect, pf24bit);
  Result.Canvas.Lock;
  try
    Result.cxCanvas.WindowOrg := AnimatedRect.TopLeft;
    FSuppressContentDrawing := True;
    ViewInfo.Draw(Result.cxCanvas);
    FSuppressContentDrawing := False;
    Result.cxCanvas.WindowOrg := cxNullPoint;
  finally
    Result.Canvas.Unlock;
  end;
end;

procedure TdxWizardControlAnimationController.PrepareForAnimation;
begin
  WizardControl.Controller.HotCell := nil;
  WizardControl.Controller.PressedCell := nil;
end;

function TdxWizardControlAnimationController.PreparePageBitmap(
  APage: TdxWizardControlCustomPage): TcxBitmap;
begin
  Result := TcxBitmap.CreateSize(AnimatedRect, pf24bit);
  Result.Canvas.Lock;
  try
    Result.cxCanvas.WindowOrg := AnimatedRect.TopLeft;
    try
      ViewInfo.Draw(Result.cxCanvas);
      if APage.Visible then
        cxPaintTo(APage, Result.cxCanvas, ViewInfo.PageArea.TopLeft, APage.ClientRect)
      else
      begin
        WizardControl.DisableAlign;
        try
          APage.BoundsRect := cxRectOffset(ViewInfo.PageArea, -WizardControl.Width, 0);
          APage.Visible := True;
          cxPaintTo(APage, Result.cxCanvas, ViewInfo.PageArea.TopLeft, APage.ClientRect);
          APage.Visible := False;
        finally
          WizardControl.EnableAlign;
        end;
      end;
    finally
      Result.cxCanvas.WindowOrg := cxNullPoint;
    end;
  finally
    Result.Canvas.Unlock;
  end;
end;

procedure TdxWizardControlAnimationController.ProcessMultiStageAnimation(
  ANewPage, APrevPage: TdxWizardControlCustomPage; AAnimationMode: TdxDrawAnimationMode);

  procedure FadeOutToBlank(APage: TdxWizardControlCustomPage; AAnimationTime: Cardinal);
  var
    APageBitmap, ABlankBitmap: TcxBitmap;
  begin
    FStage := wctacsTransition;
    FAnimatedRect := ViewInfo.TransitionEffectAreaBounds;
    ABlankBitmap := PrepareBlankBitmap;
    try
      APageBitmap := PreparePageBitmap(APage);
      try
        APage.Visible := False;
        RunAnimation(APageBitmap, ABlankBitmap, AAnimationTime, AAnimationMode);
      finally
        APageBitmap.Free;
      end;
    finally
      ABlankBitmap.Free;
    end;
  end;

  procedure CorrectAutoSizeByAlign(var ASize: TSize);
  begin
    if WizardControl.Align in [alTop, alBottom, alClient] then
      ASize.cx := WizardControl.Width;
    if WizardControl.Align in [alLeft, alRight, alClient] then
      ASize.cy := WizardControl.Height;
  end;

  procedure SwitchToNextPage(APage: TdxWizardControlCustomPage; AAnimationTime: Cardinal);
  var
    ANewSize: TSize;
    AProgress: Single;
    ATime, AFinishTime, AStartTime: Int64;
  begin
    if AutoSize then
    begin
      FStage := wctacsResizing;
      FAnimatedRect := cxNullRect;
      APage.HandleNeeded;
      ANewSize := cxSize(WizardControl.BoundsRect);
      WizardControl.InternalSetActivePage(APage);
      WizardControl.CalculateAutoSize(ANewSize.cx, ANewSize.cy);
      CorrectAutoSizeByAlign(ANewSize);
      if cxSizeIsEqual(ANewSize, cxSize(WizardControl.BoundsRect)) then
        Exit;

      FSuppressContentDrawing := True;
      try
        if ActualResizeAnimation then
        begin
          AStartTime := dxGetExactTickCount;
          AFinishTime := AStartTime + Max(dxTimeToTickCount(AAnimationTime), 1);
          repeat
            ATime := Min(AFinishTime, dxGetExactTickCount);
            AProgress := (ATime - AStartTime) / (AFinishTime - AStartTime);
            WizardControl.SetBounds(WizardControl.Left, WizardControl.Top,
              Trunc(WizardControl.Width * (1 - AProgress) + ANewSize.cx * AProgress),
              Trunc(WizardControl.Height * (1 - AProgress) + ANewSize.cy * AProgress));
            RedrawArea(WizardControl.ClientRect);
          until ATime = AFinishTime;
        end
        else
        begin
          WizardControl.SetBounds(WizardControl.Left, WizardControl.Top, ANewSize.cx, ANewSize.cy);
          WizardControl.Update;
        end;
      finally
        FSuppressContentDrawing := False;
      end;
    end
    else
      WizardControl.InternalSetActivePage(ANewPage);
  end;

  procedure FadeInFromBlank(APage: TdxWizardControlCustomPage; AAnimationTime: Cardinal);
  var
    APageBitmap, ABlankBitmap: TcxBitmap;
  begin
    FStage := wctacsTransition;
    FAnimatedRect := ViewInfo.TransitionEffectAreaBounds;
    ABlankBitmap := PrepareBlankBitmap;
    try
      APageBitmap := PreparePageBitmap(APage);
      try
        RunAnimation(ABlankBitmap, APageBitmap, AAnimationTime, AAnimationMode);
        APage.Visible := True;
      finally
        APageBitmap.Free;
      end;
    finally
      ABlankBitmap.Free;
    end;
  end;

begin
  FadeOutToBlank(APrevPage, dxWizardControlMultiStageAnimationFadeOutTime);
  SwitchToNextPage(ANewPage, dxWizardControlMultiStageAnimationResizeTime);
  FadeInFromBlank(ANewPage, dxWizardControlMultiStageAnimationFadeInTime);
end;

procedure TdxWizardControlAnimationController.ProcessSingleStageAnimation(
  ANewPage, APrevPage: TdxWizardControlCustomPage; AAnimationMode: TdxDrawAnimationMode);
var
  ANewPageBitmap, APrevPageBitmap: TcxBitmap;
begin
  FStage := wctacsTransition;
  FAnimatedRect := ViewInfo.TransitionEffectAreaBounds;
  APrevPageBitmap := PreparePageBitmap(APrevPage);
  try
    APrevPage.Visible := False;
    WizardControl.InternalSetActivePage(ANewPage);
    ANewPageBitmap := PreparePageBitmap(ANewPage);
    try
      RunAnimation(APrevPageBitmap, ANewPageBitmap,
        dxWizardControlSingleStageAnimationTime, AAnimationMode);
      ANewPage.Visible := True;
    finally
      ANewPageBitmap.Free;
    end;
  finally
    APrevPageBitmap.Free;
  end;
end;

procedure TdxWizardControlAnimationController.RedrawArea(const ARect: TRect);
begin
  WizardControl.InvalidateRect(ARect, False);
  WizardControl.Update;
end;

procedure TdxWizardControlAnimationController.RunAnimation(
  AStartImage, AFinishImage: TBitmap; ATime: Cardinal; AMode: TdxDrawAnimationMode);
begin
  FAnimation := TdxImageAnimationTransition.Create(AStartImage, AFinishImage, ATime, AMode);
  try
    FAnimation.FreeOnTerminate := False;
    FAnimation.OnAnimate := AnimationHandler;
    FAnimation.ImmediateAnimation;
  finally
    FreeAndNil(FAnimation);
  end;
end;

function TdxWizardControlAnimationController.GetActualResizeAnimation: Boolean;
var
  AAnimationMode: TdxWizardControlResizeAnimation;
begin
  AAnimationMode := WizardControl.OptionsAnimate.ResizeAnimation;
  if AAnimationMode = wcraDefault then
    AAnimationMode := ViewInfo.DefaultResizeAnimation;
  Result := AAnimationMode = wcraEnabled;
end;

function TdxWizardControlAnimationController.GetActualTransitionEffect: TdxWizardControlTransitionEffect;
begin
  Result := WizardControl.OptionsAnimate.TransitionEffect;
  if Result = wcteDefault then
    Result := ViewInfo.DefaultTransitionEffect;
  if Result = wcteDefault then
    Result := wcteNone;
end;

function TdxWizardControlAnimationController.GetAutoSize: Boolean;
begin
  Result := WizardControl.AutoSize and (WizardControl.Align <> alClient);
end;

function TdxWizardControlAnimationController.GetViewInfo: TdxWizardControlViewInfo;
begin
  Result := WizardControl.ViewInfo;
end;

{ TdxCustomWizardControl }

constructor TdxCustomWizardControl.Create(AComponent: TComponent);
begin
  inherited Create(AComponent);
  ControlStyle := ControlStyle + [csAcceptsControls];
  FPages := TList.Create;

  FButtons := CreateOptionsButtons;
  FButtons.OnChange := ChangeHandler;
  FHeader := CreateOptionsHeader;
  FHeader.OnChange := ChangeHandler;
  FInfoPanel := CreateOptionsInfoPanel;
  FInfoPanel.OnChange := ChangeHandler;
  FOptionsAnimate := CreateOptionsAnimate;
  FOptionsAnimate.OnChange := ChangeHandler;
  FOptionsViewStyleAero := CreateOptionsViewStyleAero;
  FOptionsViewStyleAero.OnChange := ChangeHandler;
  FWatermark := CreateOptionsWatermark;
  FWatermark.OnChange := ChangeHandler;

  FAnimationController := CreateAnimationController;
  FController := CreateController;
  FViewInfo := CreateViewInfo;

  BorderStyle := cxcbsNone;
  Align := alClient;
  TabStop := True;
end;

destructor TdxCustomWizardControl.Destroy;
begin
  FreeAndNil(FAnimationController);
  FreeAndNil(FViewInfo);
  FreeAndNil(FController);
  FreeAndNil(FWatermark);
  FreeAndNil(FOptionsViewStyleAero);
  FreeAndNil(FOptionsAnimate);
  FreeAndNil(FInfoPanel);
  FreeAndNil(FButtons);
  FreeAndNil(FHeader);
  FreeAndNil(FPages);
  inherited Destroy;
end;

function TdxCustomWizardControl.AddPage(APageClass: TdxWizardControlCustomPageClass): TdxWizardControlCustomPage;
begin
  Result := APageClass.Create(Owner);
  Result.WizardControl := Self;
end;

procedure TdxCustomWizardControl.AdjustClientRect(var Rect: TRect);
begin
  Rect := ViewInfo.PageArea;
end;

procedure TdxCustomWizardControl.AlignControls(AControl: TControl; var Rect: TRect);
var
  I: Integer;
begin
  if not IsLoading then
  begin
    AdjustClientRect(Rect);
    for I := 0 to PageCount - 1 do
      Pages[I].BoundsRect := Rect;
  end;
end;

procedure TdxCustomWizardControl.BoundsChanged;
begin
  inherited BoundsChanged;
  Changed([wccLayout]);
end;

procedure TdxCustomWizardControl.CalculateAutoSize(var AWidth, AHeight: Integer);
begin
  if ActivePage <> nil then
  begin
    AWidth := cxRectWidth(ViewInfo.PageArea);
    AHeight := cxRectHeight(ViewInfo.PageArea);
    ActivePage.CalculateAutoSize(AWidth, AHeight);
    Inc(AWidth, ViewInfo.PageArea.Left + Width - ViewInfo.PageArea.Right);
    Inc(AHeight, ViewInfo.PageArea.Top + Height - ViewInfo.PageArea.Bottom);
  end;
  AHeight := Max(AHeight, ViewInfo.MeasureHeight);
  AWidth := Max(AWidth, ViewInfo.MeasureWidth);
end;

function TdxCustomWizardControl.CanActivatePage(APage: TdxWizardControlCustomPage): Boolean;
begin
  Result := (APage = nil) or (APage.WizardControl = Self) and (IsDesigning or APage.PageVisible);
end;

function TdxCustomWizardControl.CanAutoSize(var ANewWidth, ANewHeight: Integer): Boolean;
begin
  Result := (ActivePage <> nil) and not IsResizingAnimationActive;
  if Result then
    CalculateAutoSize(ANewWidth, ANewHeight);
end;

function TdxCustomWizardControl.CanUseTransitionEffect(
  ANewPage, APrevPage: TdxWizardControlCustomPage): Boolean;
begin
  Result := (AnimationController.ActualTransitionEffect <> wcteNone) and
    not IsDesigning and (ANewPage <> nil) and (APrevPage <> nil) and (APrevPage <> ANewPage) and
    HandleAllocated and Visible and IsWindowVisible(Handle);
end;

procedure TdxCustomWizardControl.Changed(AChanges: TdxWizardControlChanges);
begin
  if not (IsDestroying or IsLoading) then
  begin
    if [wccStruct] * AChanges <> [] then
      ViewInfo.RecreateCells;
    if [wccStruct, wccLayout] * AChanges <> [] then
    begin
      ViewInfo.Calculate;
      LayoutChanged;
    end;
    if [wccStruct, wccLayout, wccContent] * AChanges <> [] then
    begin
      Controller.RefreshState;
      Controller.ValidateFocusedCell;
      Invalidate;
    end;
  end;
end;

procedure TdxCustomWizardControl.ChangeHandler(Sender: TObject; AChanges: TdxWizardControlChanges);
begin
  Changed(AChanges);
end;

procedure TdxCustomWizardControl.ChangeScaleEx(M, D: Integer; IsDpiChange: Boolean);
begin
  inherited;
  OptionsViewStyleAero.ChangeScale(M, D);
  InfoPanel.ChangeScale(M, D);
  Header.ChangeScale(M, D);
  Buttons.ChangeScale(M, D);
  ViewInfo.Painter.InitializeFonts;
end;

function TdxCustomWizardControl.CreateAnimationController: TdxWizardControlAnimationController;
begin
  Result := TdxWizardControlAnimationController.Create(Self);
end;

function TdxCustomWizardControl.CreateController: TdxWizardControlController;
begin
  Result := TdxWizardControlController.Create(Self);
end;

procedure TdxCustomWizardControl.CreateHandle;
begin
  inherited CreateHandle;
  Changed([wccLayout]);
end;

function TdxCustomWizardControl.CreateOptionsAnimate: TdxWizardControlOptionsAnimate;
begin
  Result := TdxWizardControlOptionsAnimate.Create(Self);
end;

function TdxCustomWizardControl.CreateOptionsButtons: TdxWizardControlButtons;
begin
  Result := TdxWizardControlButtons.Create(Self);
end;

function TdxCustomWizardControl.CreateOptionsHeader: TdxWizardControlHeader;
begin
  Result := TdxWizardControlHeader.Create(Self);
end;

function TdxCustomWizardControl.CreateOptionsInfoPanel: TdxWizardControlInfoPanel;
begin
  Result := TdxWizardControlInfoPanel.Create(Self);
end;

function TdxCustomWizardControl.CreateOptionsViewStyleAero: TdxWizardControlOptionsViewStyleAero;
begin
  Result := TdxWizardControlOptionsViewStyleAero.Create(Self);
end;

function TdxCustomWizardControl.CreateOptionsWatermark: TdxWizardControlWatermark;
begin
  Result := TdxWizardControlWatermark.Create(Self);
end;

function TdxCustomWizardControl.CreateViewInfo: TdxWizardControlViewInfo;
begin
  if ViewStyle = wcvsWizard97 then
    Result := TdxWizardControlViewStyleWizard97ViewInfo.Create(Self)
  else
    Result := TdxWizardControlViewStyleAeroViewInfo.Create(Self);
end;

procedure TdxCustomWizardControl.DeletePage(APage: TdxWizardControlCustomPage);
begin
  APage.Free;
end;

function TdxCustomWizardControl.DoButtonClick(AButtonKind: TdxWizardControlButtonKind): Boolean;
begin
  Result := False;
  if Assigned(OnButtonClick) then
    OnButtonClick(Self, AButtonKind, Result);
end;

function TdxCustomWizardControl.DoHandleChildControlKey(const Message: TCMChildKey): Boolean;
begin
  Result := False;
  if Assigned(OnHandleChildControlKey) then
    OnHandleChildControlKey(Message, Result);
end;

function TdxCustomWizardControl.DoInfoPanelClick: Boolean;
begin
  Result := False;
  if Assigned(OnInfoPanelClick) then
    OnInfoPanelClick(Self, Result);
end;

procedure TdxCustomWizardControl.DoPageChanged;
begin
  dxCallNotify(OnPageChanged, Self);
end;

function TdxCustomWizardControl.DoPageChanging(NewPage: TdxWizardControlCustomPage): Boolean;
begin
  Result := True;
  if Assigned(OnPageChanging) then
    OnPageChanging(Self, NewPage, Result);
end;

procedure TdxCustomWizardControl.DoPaint;
begin
  if AnimationController.Active then
  begin
    Canvas.SaveClipRegion;
    try
      AnimationController.Draw(Canvas);
      Canvas.ExcludeClipRect(AnimationController.AnimatedRect);
      if Canvas.RectVisible(ClientRect) then
        ViewInfo.Draw(Canvas);
    finally
      Canvas.RestoreClipRegion;
    end;
  end
  else
    ViewInfo.Draw(Canvas);
end;

procedure TdxCustomWizardControl.EraseBackground(DC: HDC);
var
  ASaveIndex: Integer;
begin
  if ViewInfo.TitleViewInfo.IsPaintOnGlass then
  begin
    ASaveIndex := SaveDC(DC);
    try
      with ViewInfo.TitleViewInfo.Bounds do
        ExcludeClipRect(DC, Left, Top, Right, Bottom);
      inherited EraseBackground(DC);
    finally
      RestoreDC(DC, ASaveIndex);
    end;
  end
  else
    inherited EraseBackground(DC);
end;

procedure TdxCustomWizardControl.FullRefresh;
begin
  Changed([wccStruct]);
end;

function TdxCustomWizardControl.CanGoToNextPage: Boolean;
begin
  Result := GetNextPage(ActivePage, True) <> nil;
end;

function TdxCustomWizardControl.CanGoToPrevPage: Boolean;
begin
  Result := GetNextPage(ActivePage, False) <> nil;
end;

procedure TdxCustomWizardControl.GoToNextPage;
begin
  if CanGoToNextPage then
  begin
    ActivePage := GetNextPage(ActivePage, True);
    SelectActivePage;
  end;
end;

procedure TdxCustomWizardControl.GoToPrevPage;
begin
  if CanGoToPrevPage then
  begin
    ActivePage := GetNextPage(ActivePage, False);
    SelectActivePage;
  end;
end;

procedure TdxCustomWizardControl.InternalSetActivePage(APage: TdxWizardControlCustomPage);
begin
  FActivePage := APage;
  Changed([wccLayout]);
end;

function TdxCustomWizardControl.IsDoubleBufferedNeeded: Boolean;
begin
  Result := inherited IsDoubleBufferedNeeded or not IsWinSeven;
end;

procedure TdxCustomWizardControl.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  Controller.KeyDown(Key, Shift);
end;

procedure TdxCustomWizardControl.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited KeyUp(Key, Shift);
  Controller.KeyUp(Key, Shift);
end;

procedure TdxCustomWizardControl.LayoutChanged;
begin
  if AutoSize then
    AdjustSize;
  if FormHelper <> nil then
    FormHelper.LayoutChanged;
  Realign;
end;

procedure TdxCustomWizardControl.Loaded;
begin
  inherited Loaded;
  ValidateActivePage;
  Changed([wccStruct, wccLayout]);
end;

procedure TdxCustomWizardControl.LookAndFeelChanged(
  Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues);
begin
  inherited LookAndFeelChanged(Sender, AChangedValues);
  Changed([wccLayout]);
  InvalidateWithChildren;
end;

procedure TdxCustomWizardControl.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Controller.MouseDown(Button, Shift, X, Y);
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TdxCustomWizardControl.MouseLeave(AControl: TControl);
begin
  Controller.MouseLeave(AControl);
  inherited MouseLeave(AControl);
end;

procedure TdxCustomWizardControl.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  Controller.MouseMove(Shift, X, Y);
  inherited MouseMove(Shift, X, Y);
end;

procedure TdxCustomWizardControl.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Controller.MouseUp(Button, Shift, X, Y);
  inherited MouseUp(Button, Shift, X, Y);
end;

function TdxCustomWizardControl.NeedsScrollBars: Boolean;
begin
  Result := False;
end;

procedure TdxCustomWizardControl.PageAdded(APage: TdxWizardControlCustomPage);
begin
  FPages.Add(APage);
  if ActivePage = nil then
    ActivePage := APage;
  UpdatePagesVisibility;
end;

procedure TdxCustomWizardControl.PageRemoving(APage: TdxWizardControlCustomPage);
var
  ANewActivePage: TdxWizardControlCustomPage;
begin
  if FPages <> nil then
  begin
    if APage = ActivePage then
    begin
      ANewActivePage := GetNextPage(ActivePage, True);
      if ANewActivePage = nil then
        ANewActivePage := GetNextPage(ActivePage, False);
      ActivePage := ANewActivePage;
    end;
    FPages.Remove(APage);
    Changed([wccStruct]);
  end;
end;

procedure TdxCustomWizardControl.RecreateViewInfo;
begin
  FreeAndNil(FViewInfo);
  FViewInfo := CreateViewInfo;
  Changed([wccLayout]);
end;

procedure TdxCustomWizardControl.TranslationChanged;
begin
  inherited TranslationChanged;
  Buttons.UpdateTranslation;
end;

procedure TdxCustomWizardControl.UpdatePagesVisibility;
var
  I: Integer;
begin
  for I := 0 to PageCount - 1 do
    Pages[I].Visible := ActivePage = Pages[I];
  if ActivePage <> nil then
    ActivePage.BringToFront;
end;

procedure TdxCustomWizardControl.ValidateActivePage;
var
  APage: TdxWizardControlCustomPage;
begin
  if (ActivePage = nil) or not CanActivatePage(ActivePage) then
  begin
    APage := GetNextPage(ActivePage, True);
    if APage = nil then
      APage := GetNextPage(ActivePage, False);
    ActivePage := APage;
  end;
end;

procedure TdxCustomWizardControl.ValidateInsert(AComponent: TComponent);
begin
  inherited ValidateInsert(AComponent);
  if not ((AComponent is TControl) and IsInternalControl(TControl(AComponent))) then
  begin
    if not ((AComponent is TdxWizardControlCustomPage) or (AComponent is TdxWizardControlDesignSelectorHelper)) then
      raise EdxWizardControlException.Create(cxGetResourceString(@sdxWizardControlErrorWrongChild));
  end;
end;

function TdxCustomWizardControl.GetActivePageIndex: Integer;
begin
  Result := FPages.IndexOf(ActivePage);
end;

procedure TdxCustomWizardControl.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  I: Integer;
begin
  for I := 0 to PageCount - 1 do
    Proc(Pages[I]);
end;

function TdxCustomWizardControl.GetHitTest: TdxWizardControlHitTest;
begin
  Result := Controller.HitTest;
end;

function TdxCustomWizardControl.GetIsResizingAnimationActive: Boolean;
begin
  Result := AnimationController.Active and (AnimationController.Stage = wctacsResizing);
end;

function TdxCustomWizardControl.GetNextPage(
  ACurrentPage: TdxWizardControlCustomPage; ASearchForward: Boolean): TdxWizardControlCustomPage;

  function DoSearch(AStartIndex, AStep: Integer): TdxWizardControlCustomPage;
  var
    AIndex: Integer;
  begin
    Result := nil;
    AIndex := AStartIndex + AStep;
    while (AIndex >= 0) and (AIndex < PageCount) do
    begin
      if CanActivatePage(Pages[AIndex]) then
      begin
        Result := Pages[AIndex];
        Break;
      end;
      AIndex := AIndex + AStep;
    end;
  end;

const
  StepMap: array[Boolean] of Integer = (-1, 1);
begin
  if IsDestroying then
    Result := nil
  else
    Result := DoSearch(FPages.IndexOf(ACurrentPage), StepMap[ASearchForward]);
end;

function TdxCustomWizardControl.GetPage(APageIndex: Integer): TdxWizardControlCustomPage;
begin
  Result := TdxWizardControlCustomPage(FPages[APageIndex]);
end;

function TdxCustomWizardControl.GetPageCount: Integer;
begin
  Result := FPages.Count;
end;

procedure TdxCustomWizardControl.SelectActivePage;
begin
  if ActivePage <> nil then
    ActivePage.SelectObject;
end;

procedure TdxCustomWizardControl.SetChildOrder(Child: TComponent; Order: Integer);
begin
  TdxWizardControlCustomPage(Child).PageIndex := Order;
end;

procedure TdxCustomWizardControl.SetActivePage(APage: TdxWizardControlCustomPage);
begin
  if not (csLoading in ComponentState) then
    if (ActivePage <> APage) and CanActivatePage(APage) then
    begin
      if DoPageChanging(APage) then
      begin
        if CanUseTransitionEffect(APage, ActivePage) then
          AnimationController.AnimateTransition(APage, ActivePage)
        else
          InternalSetActivePage(APage);

        UpdatePagesVisibility;
        DoPageChanged;
      end;
    end;
end;

procedure TdxCustomWizardControl.SetActivePageIndex(AValue: Integer);
begin
  AValue := Max(Min(AValue, PageCount - 1), -1);
  if (AValue >= 0) and (AValue < PageCount) then
    ActivePage := Pages[AValue]
  else
    ActivePage := nil;
end;

procedure TdxCustomWizardControl.SetButtons(AValue: TdxWizardControlButtons);
begin
  FButtons.Assign(AValue);
end;

procedure TdxCustomWizardControl.SetHeader(AValue: TdxWizardControlHeader);
begin
  FHeader.Assign(AValue);
end;

procedure TdxCustomWizardControl.SetInfoPanel(AValue: TdxWizardControlInfoPanel);
begin
  FInfoPanel.Assign(AValue);
end;

procedure TdxCustomWizardControl.SetOptionsAnimate(AValue: TdxWizardControlOptionsAnimate);
begin
  FOptionsAnimate.Assign(AValue);
end;

procedure TdxCustomWizardControl.SetOptionsViewStyleAero(AValue: TdxWizardControlOptionsViewStyleAero);
begin
  FOptionsViewStyleAero.Assign(AValue);
end;

procedure TdxCustomWizardControl.SetViewStyle(AValue: TdxWizardControlViewStyle);
begin
  if ViewStyle <> AValue then
  begin
    FViewStyle := AValue;
    RecreateViewInfo;
  end;
end;

procedure TdxCustomWizardControl.SetWatermark(AValue: TdxWizardControlWatermark);
begin
  FWatermark.Assign(AValue);
end;

procedure TdxCustomWizardControl.CMChildKey(var Message: TCMChildKey);

  function CanProcessSpecialKey: Boolean;
  begin
    Result := ([ssCtrl, ssAlt, ssShift] * KeyboardStateToShiftState = []) and
      (Message.Sender.Perform(CM_WANTSPECIALKEY, Message.CharCode, 0) = 0) and
      (Message.Sender.Perform(WM_GETDLGCODE, 0, 0) and DLGC_WANTALLKEYS = 0);
  end;

begin
  if (Self = Message.Sender) and Controller.ProcessChildKey(Message.CharCode, KeyboardStateToShiftState) then
    Message.Result := 1
  else
    if (Message.Sender = Self) or (Message.Sender is TdxWizardControlCustomPage) or
      not DoHandleChildControlKey(Message) then
      case Message.CharCode of
        VK_RETURN:
          Message.Result := Ord(CanProcessSpecialKey and Controller.ProcessDialogChar(VK_RETURN));
        VK_ESCAPE:
          Message.Result := Ord(CanProcessSpecialKey and DoButtonClick(wcbkCancel));
      end;

  if Message.Result = 0 then
    inherited;
end;

function TdxCustomWizardControl.GetDesignHitTest(X, Y: Integer; Shift: TShiftState): Boolean;
begin
  HitTest.HitPoint := Point(X, Y);
  Result := HitTest.HitObjectAsButton <> nil;
  if not Result then
    Controller.HotCell := nil;
end;

procedure TdxCustomWizardControl.CMDialogChar(var Message: TCMDialogChar);
begin
  if Controller.ProcessDialogChar(Message.CharCode) then
    Message.Result := 1
  else
    inherited;
end;

procedure TdxCustomWizardControl.CMFontChanged(var Message: TMessage);
begin
  inherited;
  InfoPanel.InitializeFont;
  OptionsViewStyleAero.Title.InitializeFont;
  ViewInfo.Painter.InitializeFonts;
  Changed([wccLayout]);
end;

procedure TdxCustomWizardControl.CMTabStopChanged(var Message: TMessage);
begin
  inherited;
  TabStop := True;
end;

procedure TdxCustomWizardControl.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  Message.Result := Message.Result or DLGC_WANTARROWS;
end;

procedure TdxCustomWizardControl.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;
  Controller.FocusKill;
end;

procedure TdxCustomWizardControl.WMNCHitTest(var Message: TWMNCHitTest);
begin
  if (FormHelper = nil) or not FormHelper.ProcessHitTest(Message) then
    inherited;
end;

procedure TdxCustomWizardControl.WMPaint(var Message: TWMPaint);
begin
  if (Message.DC = 0) and (ViewInfo <> nil) and ViewInfo.TitleViewInfo.IsPaintOnGlass then
    dxPaintWindowOnGlass(Handle, True)
  else
    inherited;
end;

procedure TdxCustomWizardControl.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;
  Controller.FocusSet;
end;

{ TdxWizardControlOptionsSize }

procedure TdxWizardControlOptionsSize.DoAssign(Source: TPersistent);
begin
  inherited DoAssign(Source);
  if Source is TdxWizardControlOptionsSize then
  begin
    MinHeight := TdxWizardControlOptionsSize(Source).MinHeight;
    MinWidth := TdxWizardControlOptionsSize(Source).MinWidth;
  end;
end;

procedure TdxWizardControlOptionsSize.SetMinHeight(AValue: Integer);
begin
  AValue := Max(AValue, 0);
  if FMinHeight <> AValue then
  begin
    FMinHeight := AValue;
    Changed([wccLayout]);
  end;
end;

procedure TdxWizardControlOptionsSize.SetMinWidth(AValue: Integer);
begin
  AValue := Max(AValue, 0);
  if FMinWidth <> AValue then
  begin
    FMinWidth := AValue;
    Changed([wccLayout]);
  end;
end;

{ TdxWizardCustomPage }

constructor TdxWizardControlCustomPage.Create(AComponent: TComponent);
begin
  inherited Create(AComponent);
  FPageVisible := True;
  ControlStyle := ControlStyle + [csAcceptsControls];
  FHeader := TdxWizardControlPageHeader.Create(Self);
  FHeader.OnChange := ChangeHandler;
  FOptionsSize := TdxWizardControlOptionsSize.Create(Self);
  FOptionsSize.OnChange := ChangeHandler;
  FWatermark := TdxWizardControlPageWatermark.Create(Self);
  FWatermark.OnChange := ChangeHandler;
  if IsDesigning then
    FDesignSelector := TdxWizardControlDesignSelectorHelper.Create(Self);
  RegisterWizardControlPage(Self);
end;

destructor TdxWizardControlCustomPage.Destroy;
begin
  UnregisterWizardControlPage(Self);
  WizardControl := nil;
  FreeAndNil(FDesignSelector);
  FreeAndNil(FOptionsSize);
  FreeAndNil(FWatermark);
  FreeAndNil(FHeader);
  inherited Destroy;
end;

procedure TdxWizardControlCustomPage.Activate;
begin
  if WizardControl <> nil then
    WizardControl.ActivePage := Self;
end;

procedure TdxWizardControlCustomPage.AlignControls(AControl: TControl; var Rect: TRect);
begin
  inherited AlignControls(AControl, Rect);
  if Active then
  begin
    if WizardControl.AutoSize then
      WizardControl.AdjustSize;
  end;
end;

procedure TdxWizardControlCustomPage.BoundsChanged;
begin
  inherited BoundsChanged;
  UpdateDesignSelectorBounds;
end;

procedure TdxWizardControlCustomPage.CalculateAutoSize(var AWidth, AHeight: Integer);
begin
  CanAutoSize(AWidth, AHeight);
  AHeight := Max(AHeight, OptionsSize.MinHeight);
  AWidth := Max(AWidth, OptionsSize.MinWidth);
end;

procedure TdxWizardControlCustomPage.Changed(AChanges: TdxWizardControlChanges);
begin
  if WizardControl <> nil then
    WizardControl.Changed(AChanges);
end;

procedure TdxWizardControlCustomPage.DoPaint;
begin
  if WizardControl <> nil then
    WizardControl.ViewInfo.Painter.DrawBackground(Canvas, ClientBounds);
end;

function TdxWizardControlCustomPage.NeedsScrollBars: Boolean;
begin
  Result := False;
end;

procedure TdxWizardControlCustomPage.SetParent(AParent: TWinControl);
begin
  if (AParent <> nil) and not (AParent is TdxCustomWizardControl) then
    raise EdxWizardControlException.Create(cxGetResourceString(@sdxWizardControlErrorWrongParent));

  if AParent <> Parent then
  begin
    if Parent is TdxCustomWizardControl then
      TdxCustomWizardControl(Parent).PageRemoving(Self);
    inherited SetParent(AParent);
    if Parent is TdxCustomWizardControl then
      TdxCustomWizardControl(Parent).PageAdded(Self);
  end;
end;

procedure TdxWizardControlCustomPage.UpdateDesignSelectorBounds;
begin
  if DesignSelector <> nil then
    DesignSelector.SelectorBounds := TdxControlsDesignSelectorHelper.CalculateBounds(ClientBounds, ScaleFactor);
end;

function TdxWizardControlCustomPage.IsObjectSelected: Boolean;
begin
  Result := (FDesignHelper <> nil) and FDesignHelper.IsObjectSelected(Self, Self);
end;

procedure TdxWizardControlCustomPage.SelectObject;
begin
  if not IsObjectSelected then
  begin
    if FDesignHelper <> nil then
      FDesignHelper.SelectObject(Self, Self);
  end;
end;

procedure TdxWizardControlCustomPage.SelectionChanged;
begin
  if IsObjectSelected then
    Activate;
end;

function TdxWizardControlCustomPage.GetActive: Boolean;
begin
  Result := (WizardControl <> nil) and (WizardControl.ActivePage = Self);
end;

function TdxWizardControlCustomPage.GetAlign: TAlign;
begin
  Result := inherited Align;
end;

function TdxWizardControlCustomPage.GetPageIndex: Integer;
begin
  if WizardControl = nil then
    Result := -1
  else
    Result := WizardControl.FPages.IndexOf(Self);
end;

function TdxWizardControlCustomPage.GetWizardControl: TdxCustomWizardControl;
begin
  if Parent is TdxCustomWizardControl then
    Result := TdxCustomWizardControl(Parent)
  else
    Result := nil;
end;

procedure TdxWizardControlCustomPage.ChangeHandler(
  Sender: TObject; AChanges: TdxWizardControlChanges);
begin
  Changed(AChanges);
end;

procedure TdxWizardControlCustomPage.SetAlign(AValue: TAlign);
begin
  inherited Align := alNone;
end;

procedure TdxWizardControlCustomPage.SetHeader(AValue: TdxWizardControlPageHeader);
begin
  FHeader.Assign(AValue);
end;

procedure TdxWizardControlCustomPage.SetOptionsSize(AValue: TdxWizardControlOptionsSize);
begin
  FOptionsSize.Assign(AValue);
end;

procedure TdxWizardControlCustomPage.SetPageIndex(AValue: Integer);
begin
  if WizardControl <> nil then
  begin
    if (AValue < 0) or (AValue >= WizardControl.PageCount) then
      raise EdxWizardControlException.CreateFmt(sdxWizardControlErrorWrongPageIndex, [AValue, WizardControl.PageCount - 1]);
    WizardControl.FPages.Move(PageIndex, AValue);
    WizardControl.Changed([wccLayout]);
  end;
end;

procedure TdxWizardControlCustomPage.SetPageVisible(AValue: Boolean);
begin
  if PageVisible <> AValue then
  begin
    FPageVisible := AValue;
    if Active then
      WizardControl.ValidateActivePage;
  end;
end;

procedure TdxWizardControlCustomPage.SetWatermark(AValue: TdxWizardControlPageWatermark);
begin
  FWatermark.Assign(AValue);
end;

procedure TdxWizardControlCustomPage.SetWizardControl(AValue: TdxCustomWizardControl);
begin
  Parent := AValue;
end;

end.
