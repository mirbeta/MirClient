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

unit cxEdit;

{$I cxVer.inc}

interface

uses
  Types, Windows, Messages, Variants, Forms, ComCtrls, Classes, Controls, Graphics,
  Menus, StdCtrls, SysUtils, ActnList, ImgList,
  dxCore, dxCoreClasses, dxMessages, dxGDIPlusClasses, cxGeometry, dxDPIAwareUtils,
  cxClasses, cxContainer, cxControls, cxCustomData, dxSpellCheckerCore,
  cxDataStorage, cxDataUtils, cxGraphics, cxLookAndFeels, cxVariants, dxCustomHint,
  cxLookAndFeelPainters, dxFading, dxThemeManager, dxUxTheme, cxEditPaintUtils, dxCoreGraphics;

type
  TcxBlobKind = (bkNone, bkBlob, bkGraphic, bkMemo, bkOle);
  TcxEditBorderStyle = (ebsNone, ebsSingle, ebsThick, ebsFlat, ebs3D, ebsUltraFlat, ebsOffice11);
  TcxEditButtonKind = (bkEllipsis, bkDown, bkGlyph, bkText);
  TcxEditButtonState = (ebsDisabled, ebsNormal, ebsPressed, ebsSelected);
  TcxEditButtonStyle = (btsDefault, bts3D, btsFlat, btsSimple, btsHotFlat, btsUltraFlat, btsOffice11);
  TcxEditButtonsViewStyle = (bvsNormal, bvsButtonsOnly, bvsButtonsAutoWidth);
  TcxEditButtonTransparency = (ebtNone, ebtInactive, ebtAlways, ebtHideInactive, ebtHideUnselected);
  TcxEditGradientDirection = dirLeft..dirDown;

  TcxEditStyleValue = TcxContainerStyleValue;
  TcxEditStyleValues = TcxContainerStyleValues;

  TcxEditHorzAlignment = TAlignment;
  TcxEditVertAlignment = (taTopJustify, taBottomJustify, taVCenter);

  TcxInplaceEditAutoHeight = (eahNone, eahEditor, eahRow);
  TcxItemInplaceEditAutoHeight = (ieahDefault, ieahNone);

const
  cxEditDefaultPrecision = 15;
  cxEditDefaultUseLeftAlignmentOnEditing = True;
  cxEditDefaultHorzAlignment: TcxEditHorzAlignment = taLeftJustify;
  cxEditDefaultVertAlignment: TcxEditVertAlignment = taTopJustify;
  cxInplaceEditOffset = 1;

  ekDefault = 0;

  svBorderColor        = csvBorderColor;
  svBorderStyle        = csvBorderStyle;
  svColor              = csvColor;
  svEdges              = csvEdges;
  svFont               = csvFont;
  svHotTrack           = csvHotTrack;
  svShadow             = csvShadow;
  svTextColor          = csvTextColor;
  svTextStyle          = csvTextStyle;
  svTransparentBorder  = csvTransparentBorder;
  svButtonStyle        = cxContainerStyleValueCount;
  svButtonTransparency = cxContainerStyleValueCount + 1;
  svPopupBorderStyle   = cxContainerStyleValueCount + 2;
  svGradientButtons    = cxContainerStyleValueCount + 3;
  svGradient           = cxContainerStyleValueCount + 4;
  svGradientDirection  = cxContainerStyleValueCount + 5;

  cxEditStyleValueCount = cxContainerStyleValueCount + 6;

  cxEditStyleValueNameA: array[0..cxEditStyleValueCount - cxContainerStyleValueCount - 1] of string = (
    'ButtonStyle',
    'ButtonTransparency',
    'PopupBorderStyle',
    'GradientButtons',
    'Gradient',
    'GradientDirection'
  );

  EditContentDefaultOffsets: array [Boolean] of TRect = (
    (Left: 1; Top: 1; Right: 1; Bottom: 3),
    (Left: 1; Top: 1; Right: 1; Bottom: 1)
  );

  EditBtnKindToEditBtnPainterKind: array [TcxEditButtonKind] of TcxEditBtnKind =
    (cxbkEllipsisBtn, cxbkComboBtn, cxbkEditorBtn, cxbkEditorBtn);
  EditBtnStateToButtonState: array[TcxEditButtonState] of TcxButtonState =
    (cxbsDisabled, cxbsNormal, cxbsPressed, cxbsHot);
  EditBtnPositionMap: array[Boolean] of TcxEditBtnPosition = (cxbpRight, cxbpLeft);

  ecpNone = -3;
  ecpErrorIcon = -2;
  ecpControl = -1;
  ecpButton = 0;

type
  TcxEditDisplayFormatOption = (dfoSupports, dfoNoCurrencyValue);
  TcxEditDisplayFormatOptions = set of TcxEditDisplayFormatOption;
  TcxEditBackgroundPaintingStyle = (bpsSolid, bpsComboEdit, bpsComboListEdit);
  TcxEditEditingStyle = (esEdit, esEditList, esFixedList, esNoEdit);
  TcxEditPaintOption = (epoAllowZeroHeight, epoAutoHeight, epoHasExternalBorder,
    epoShowEndEllipsis, epoShowFocusRectWhenInplace);
  TcxEditPaintOptions = set of TcxEditPaintOption;
  TcxEditSpecialFeatures = set of (esfBlobEditValue, esfMinSize,
    esfNoContentPart, esfClickable, esfMultiRow);
  TcxEditSupportedOperation = (esoAlwaysHotTrack, esoAutoHeight, esoEditing,
    esoFiltering, esoHorzAlignment, esoHotTrack, esoIncSearch,
    esoShowingCaption, esoSorting, esoSortingByDisplayText, esoTransparency,
    esoEditingAutoHeight, esoNeedHandle, esoAutoWidth);
  TcxEditSupportedOperations = set of TcxEditSupportedOperation;
  TcxEditValue = Variant;
  PcxEditValue = ^TcxEditValue;
  TcxEditValidateEvent = procedure(Sender: TObject; var DisplayValue: TcxEditValue;
    var ErrorText: TCaption; var Error: Boolean) of object;
  TcxEditErrorKind = Integer;
  TcxEditValidationOptions = set of (evoRaiseException, evoShowErrorIcon, evoAllowLoseFocus);

  TcxCustomEdit = class;
  TcxCustomEditStyle = class;
  TcxCustomEditButton = class;
  TcxCustomEditButtons = class;
  TcxCustomEditViewData = class;
  TcxCustomEditViewInfo = class;
  TcxEditButtonViewInfo = class;
  TcxEditButtons = class;
  TcxEditButton = class;

  EcxEditError = class(EdxException);
  EcxEditValidationError = class(EcxEditError);

  IcxEditorFieldLink = interface
  ['{E21A0DDE-85DF-42CC-9063-D9E5DF45F02F}']
    function CreateFieldControls(X, Y: Integer; ADataSource: TObject{TDataSource}; AFieldList: TList): Boolean;
  end;

  IcxEditorFieldLink2 = interface(IcxEditorFieldLink)
  ['{4A1B4837-FF36-4408-AF91-D195D16F2BA0}']
    procedure DoCreateFieldControl(AControl: TControl; AField: TObject{TField});
    function NeedCreateCaption: Boolean;
  end;

  { IcxCustomInnerEdit }

  IcxCustomInnerEdit = interface(IcxContainerInnerControl)
  ['{468D21B5-48AA-4077-8ED5-4C6112D460B1}']
    function CallDefWndProc(AMsg: UINT; WParam: WPARAM; LParam: LPARAM): LRESULT;
    function CanProcessClipboardMessages: Boolean;
    function GetEditValue: TcxEditValue;
    function GetOnChange: TNotifyEvent;
    function GetReadOnly: Boolean;
    procedure LockBounds(ALock: Boolean);
    procedure SafelySetFocus;
    procedure SetEditValue(const Value: TcxEditValue);
    procedure SetParent(Value: TWinControl);
    procedure SetOnChange(Value: TNotifyEvent);
    procedure SetReadOnly(Value: Boolean);
    property EditValue: TcxEditValue read GetEditValue write SetEditValue;
    property Parent: TWinControl write SetParent;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly;
    property OnChange: TNotifyEvent read GetOnChange write SetOnChange;
  end;

  { IcxInnerEditHelper }

  IcxInnerEditHelper = interface
  ['{35667555-6DC8-40D5-B705-B08D5697C621}']
    function GetHelper: IcxCustomInnerEdit;
  end;

  { IcxEditOwner }

  IcxEditOwner = interface
  ['{4129100C-7FC6-436E-8711-47A5C978CA73}']
    function GetViewData(out AIsViewDataCreated: Boolean): TcxCustomEditViewData;
    procedure Invalidate(const R: TRect; AEraseBackground: Boolean);
  end;

  { IcxInplaceEditIMEHelper }

  IcxInplaceEditIMEHelper = interface
  ['{DC5EF4E1-2847-4950-854A-F8BFE29706F5}']
    procedure IMEComposition(var AMessage: TMessage);
    procedure IMEStartComposition;
  end;

  TcxCustomEditProperties = class;
  TcxCustomEditPropertiesClass = class of TcxCustomEditProperties;
  TcxEditRepository = class;
  TcxEditRepositoryItem = class;

  { IcxEditRepositoryItemListener }

  IcxEditRepositoryItemListener = interface
    ['{4E27D642-022B-4CD2-AB96-64C7CF9B3299}']
    procedure ItemRemoved(Sender: TcxEditRepositoryItem);
    procedure PropertiesChanged(Sender: TcxEditRepositoryItem);
  end;

  { TcxEditRepositoryItem }

  TcxEditRepositoryItem = class(TComponent, IdxScaleFactor)
  strict private
    FListenerList: IInterfaceList;
    FProperties: TcxCustomEditProperties;
    FPropertiesEvents: TNotifyEvent;
    FRepository: TcxEditRepository;
    FScaleFactor: TdxOwnedScaleFactor;

    procedure SetProperties(Value: TcxCustomEditProperties);
    procedure SetRepository(Value: TcxEditRepository);
  protected
    function ArePropertiesCompatible(APropertiesClass: TClass): Boolean;
    procedure PropertiesChanged(Sender: TObject); virtual;
    procedure ReadState(Reader: TReader); override;
    // IdxScaleFactor
    function GetScaleFactor: TdxScaleFactor;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddListener(AListener: IcxEditRepositoryItemListener); virtual;
    class function GetEditPropertiesClass: TcxCustomEditPropertiesClass; virtual;
    function GetBaseName: string; virtual;
    function GetParentComponent: TComponent; override;
    function HasParent: Boolean; override;
    procedure RemoveListener(AListener: IcxEditRepositoryItemListener); virtual;
    procedure SetParentComponent(AParent: TComponent); override;
    property Properties: TcxCustomEditProperties read FProperties write SetProperties;
    property Repository: TcxEditRepository read FRepository write SetRepository;
  published
    property PropertiesEvents: TNotifyEvent read FPropertiesEvents write FPropertiesEvents;
  end;

  TcxEditRepositoryItemClass = class of TcxEditRepositoryItem;

  { IcxEditDefaultValuesProvider }

  IcxEditDefaultValuesProvider = interface
  ['{AE727882-6FDF-4E3A-AB35-E58AB28EFE7B}']
    function CanSetEditMode: Boolean;
    procedure ClearUsers;
    function DefaultAlignment: TAlignment;
    function DefaultBlobKind: TcxBlobKind;
    function DefaultCanModify: Boolean;
    function DefaultDisplayFormat: string;
    function DefaultEditFormat: string;
    function DefaultEditMask: string;
    function DefaultIsFloatValue: Boolean;
    function DefaultMaxLength: Integer;
    function DefaultMaxValue: Double;
    function DefaultMinValue: Double;
    function DefaultPrecision: Integer;
    function DefaultReadOnly: Boolean;
    function DefaultRequired: Boolean;
    function GetInstance: TObject;
    function IsDataStorage: Boolean;
    function IsBlob: Boolean;
    function IsCurrency: Boolean;
    function IsDataAvailable: Boolean;
    function IsDisplayFormatDefined(AIsCurrencyValueAccepted: Boolean): Boolean;
    function IsOnGetTextAssigned: Boolean;
    function IsOnSetTextAssigned: Boolean;
    function IsValidChar(AChar: Char): Boolean;
  end;

  { TcxEditRepository }

  TcxEditRepository = class(TcxScalableComponent)
  strict private
    FItems: TList;

    function GetCount: Integer;
    function GetItem(Index: Integer): TcxEditRepositoryItem;
  protected
    procedure AddItem(AItem: TcxEditRepositoryItem);
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure RemoveItem(AItem: TcxEditRepositoryItem);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    function CreateItem(ARepositoryItemClass: TcxEditRepositoryItemClass): TcxEditRepositoryItem; virtual;
    function CreateItemEx(ARepositoryItemClass: TcxEditRepositoryItemClass; AOwner: TComponent): TcxEditRepositoryItem; virtual;
    function ItemByName(const ARepositoryItemName: string): TcxEditRepositoryItem;
    //
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TcxEditRepositoryItem read GetItem; default;
  end;

  { TcxEditButtonViewInfoData }

  PcxEditButtonViewInfoData = ^TcxEditButtonViewInfoData;
  TcxEditButtonViewInfoData = record
    ActionLink: TActionLink;
    BackgroundColor: TColor;
    Caption: string;
    ContentAlignment: TAlignment;
    Default: Boolean;
    Gradient: Boolean;
    Hint: string;
    IsInplace: Boolean;
    Kind: TcxEditButtonKind;
    LeftAlignment: Boolean;
    Leftmost: Boolean;
    NativeStyle: Boolean;
    Rightmost: Boolean;
    State: TcxEditButtonState;
    Style: TcxEditButtonStyle;
    TextColor: TColor;
    Transparent: Boolean;
    VisibleCaption: string;
    BackgroundPartiallyTransparent: Boolean;
    ComboBoxStyle: Boolean;
    NativePart: Integer;
    NativeState: Integer;
    UseSkins: Boolean;
  end;

  { TcxCustomEditFadingHelper }

  TcxCustomEditFadingHelper = class(TdxFadingObjectHelper)
  private
    function GetEditOwner: IcxEditOwner;
  protected
    function CanFade: Boolean; override;
    function GetEditViewInfo: TcxCustomEditViewInfo; virtual; abstract;
    procedure DrawFadeImage; override;
  public
    procedure Invalidate; overload; virtual; abstract;
    procedure Invalidate(const R: TRect; AEraseBackground: Boolean); overload;
    //
    property EditOwner: IcxEditOwner read GetEditOwner;
    property EditViewInfo: TcxCustomEditViewInfo read GetEditViewInfo;
  end;

  { TcxEditButtonFadingHelper }

  TcxEditButtonFadingHelper = class(TcxCustomEditFadingHelper)
  private
    FState: TcxEditButtonState;
    FViewInfo: TcxEditButtonViewInfo;
  protected
    function CanFade: Boolean; override;
    procedure DrawButton(ACanvas: TcxCanvas); virtual;
    function GetButtonRect: TRect; virtual;
    function GetEditViewInfo: TcxCustomEditViewInfo; override;
    procedure GetFadingImages(out AFadeOutImage, AFadeInImage: TcxBitmap); override;
    function PrepareFadingImage(AState: TcxEditButtonState): TcxBitmap32; virtual;
  public
    constructor Create(AViewInfo: TcxEditButtonViewInfo); virtual;
    procedure Invalidate; override;
    procedure UpdateState; virtual;
    //
    property ButtonRect: TRect read GetButtonRect;
    property State: TcxEditButtonState read FState;
    property ViewInfo: TcxEditButtonViewInfo read FViewInfo;
  end;

  { TcxEditButtonViewInfo }

  TcxEditButtonViewInfoClass = class of TcxEditButtonViewInfo;
  TcxEditButtonViewInfo = class(TPersistent)
  strict private
    function GetScaleFactor: TdxScaleFactor;
  protected
    function CreateFadingHelper: TcxEditButtonFadingHelper; virtual;
  public
    Bounds: TRect;
    ButtonIndex: Integer;
    ButtonVisibleIndex: Integer;
    Data: TcxEditButtonViewInfoData;
    EditViewInfo: TcxCustomEditViewInfo;
    FadingHelper: TcxEditButtonFadingHelper;
    Glyph: TdxSmartGlyph;
    HasBackground: Boolean;
    ImageIndex: TcxImageIndex;
    Images: TCustomImageList;
    Stretchable: Boolean;
    VisibleBounds: TRect;
    Width: Integer;

    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure Assign(Source: TPersistent); override;
    function GetUpdateRegion(AViewInfo: TcxEditButtonViewInfo): TcxRegion; virtual;
    function Repaint(AControl: TWinControl; AViewInfo: TcxEditButtonViewInfo; const AEditPosition: TPoint): Boolean; virtual;
    //
    property ScaleFactor: TdxScaleFactor read GetScaleFactor;
  end;

  { TcxEditButtonActionLink }

  TcxEditButtonActionLink = class(TActionLink)
  protected
    FClient: TcxCustomEditButton;
    procedure AssignClient(AClient: TObject); override;
    function IsCaptionLinked: Boolean; override;
    function IsEnabledLinked: Boolean; override;
    function IsImageIndexLinked: Boolean; override;
    function IsHintLinked: Boolean; override;
    function IsVisibleLinked: Boolean; override;
    procedure SetCaption(const Value: string); override;
    procedure SetEnabled(Value: Boolean); override;
    procedure SetImageIndex(Value: Integer); override;
    procedure SetHint(const Value: string); override;
    procedure SetVisible(Value: Boolean); override;
  public
    function PrepareHint(var HintStr: string): Boolean; virtual;
  end;

  TcxEditButtonActionLinkClass = class of TcxEditButtonActionLink;

  { TcxCustomEditButton }

  TcxEditButtonClass = class of TcxCustomEditButton;
  TcxCustomEditButton = class(TCollectionItem)
  private
    FActionLink: TcxEditButtonActionLink;
    FCaption: TCaption;
    FContentAlignment: TAlignment;
    FDefault: Boolean;
    FEnabled: Boolean;
    FGlyph: TdxSmartGlyph;
    FHint: string;
    FImageIndex: TcxImageIndex;
    FKind: TcxEditButtonKind;
    FLeftAlignment: Boolean;
    FStretchable: Boolean;
    FTag: TcxTag;
    FTextColor: TColor;
    FVisible: Boolean;
    FVisibleCaption: TCaption;
    FWidth: Integer;

    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); dynamic;
    procedure DoActionChange(Sender: TObject);
    function GetCollection: TcxCustomEditButtons;
    procedure GlyphChanged(Sender: TObject);
    function IsCaptionStored: Boolean;
    function IsEnabledStored: Boolean;
    function IsHintStored: Boolean;
    function IsImageIndexStored: Boolean;
    function IsTagStored: Boolean;
    function IsVisibleStored: Boolean;
    procedure SetAction(Value: TBasicAction);
    procedure SetCaption(const Value: TCaption);
    procedure SetContentAlignment(Value: TAlignment);
    procedure SetDefault(Value: Boolean);
    procedure SetEnabled(Value: Boolean);
    procedure SetGlyph(Value: TdxSmartGlyph);
    procedure SetImageIndex(Value: TcxImageIndex);
    procedure SetKind(Value: TcxEditButtonKind);
    procedure SetLeftAlignment(Value: Boolean);
    procedure SetStretchable(Value: Boolean);
    procedure SetTextColor(Value: TColor);
    procedure SetVisible(Value: Boolean);
    procedure SetWidth(Value: Integer);
  protected
    procedure ChangeScale(M, D: Integer); virtual;
    procedure FreeNotification(Sender: TComponent); virtual;
    function GetAction: TBasicAction; virtual;
    function GetActionLinkClass: TcxEditButtonActionLinkClass; virtual;
    procedure InitiateAction; virtual;

    property Action: TBasicAction read GetAction write SetAction;
    property ActionLink: TcxEditButtonActionLink read FActionLink;
    property Caption: TCaption read FCaption write SetCaption stored IsCaptionStored;
    property Collection: TcxCustomEditButtons read GetCollection;
    property ContentAlignment: TAlignment read FContentAlignment write SetContentAlignment default taCenter;
    property Default: Boolean read FDefault write SetDefault default False;
    property Enabled: Boolean read FEnabled write SetEnabled stored IsEnabledStored  default True;
    property Glyph: TdxSmartGlyph read FGlyph write SetGlyph;
    property Hint: string read FHint write FHint stored IsHintStored;
    property ImageIndex: TcxImageIndex read FImageIndex write SetImageIndex stored IsImageIndexStored default -1 ;
    property Kind: TcxEditButtonKind read FKind write SetKind default bkDown;
    property LeftAlignment: Boolean read FLeftAlignment write SetLeftAlignment default False;
    property Stretchable: Boolean read FStretchable write SetStretchable default True;
    property Tag: TcxTag read FTag write FTag stored IsTagStored;
    property TextColor: TColor read FTextColor write SetTextColor default clBtnText; // TODO published
    property Visible: Boolean read FVisible write SetVisible stored IsVisibleStored  default True;
    property VisibleCaption: TCaption read FVisibleCaption;
    property Width: Integer read FWidth write SetWidth default 0;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  end;


  { TcxEditButton }

  TcxEditButton = class(TcxCustomEditButton)
  published
    property Action;
    property Caption;
    property ContentAlignment;
    property Default;
    property Enabled;
    property ImageIndex;
    property Glyph;
    property Hint;
    property Kind;
    property LeftAlignment;
    property Stretchable;
    property Tag;
    property Visible;
    property Width;
  end;

  { TcxCustomEditButtons }

  TcxCustomEditButtons = class(TCollection)
  private
    FFreeNotificator: TcxFreeNotificator;
    FOwner: TPersistent;
    FImages: TCustomImageList;
    FOnChange: TNotifyEvent;

    function GetItem(Index: Integer): TcxCustomEditButton;
    function GetVisibleCount: Integer;
    procedure SetItem(Index: Integer; Value: TcxCustomEditButton);
  protected
    procedure AddFreeNotificatorSender(Sender: TComponent);
    procedure ChangeScale(M, D: Integer); virtual;
    procedure DoFreeNotification(Sender: TComponent); virtual;
    class function GetButtonClass: TcxEditButtonClass; virtual;
    function GetOwner: TPersistent; override;
    procedure InitiateActions; virtual;
    procedure RemoveFreeNotificatorSender(Sender: TComponent);
    procedure Update(Item: TCollectionItem); override;

    property FreeNotificator: TcxFreeNotificator read FFreeNotificator;
    property Images: TCustomImageList read FImages write FImages;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    constructor Create(AOwner: TPersistent; AButtonClass: TcxEditButtonClass); virtual;
    destructor Destroy; override;
    //
    property Items[Index: Integer]: TcxCustomEditButton read GetItem write SetItem; default;
    property VisibleCount: Integer read GetVisibleCount;
  end;

  { TcxEditButtons }

  TcxEditButtons = class(TcxCustomEditButtons)
  strict private
    function GetItem(Index: Integer): TcxEditButton;
    procedure SetItem(Index: Integer; Value: TcxEditButton);
  protected
    class function GetButtonClass: TcxEditButtonClass; override;
  public
    function Add: TcxEditButton;
    //
    property Items[Index: Integer]: TcxEditButton read GetItem write SetItem; default;
  end;

  TcxEditButtonsClass = class of TcxEditButtons;

  { TcxCustomEditViewInfo }

  TcxEditAlignment = class;

  TcxEditCanStartButtonFadingEvent = procedure (Sender: TcxCustomEditViewInfo; var ACanStart: Boolean) of object;
  TcxEditDrawBackgroundEvent = procedure (Sender: TcxCustomEditViewInfo;
    ACanvas: TcxCanvas; var AHandled: Boolean) of object;
  TcxEditDrawButtonEvent = procedure (Sender: TcxEditButtonViewInfo;
    ACanvas: TcxCanvas; var AHandled: Boolean) of object;
  TcxEditDrawButtonBackgroundEvent = procedure (Sender: TcxEditButtonViewInfo;
    ACanvas: TcxCanvas; const ARect: TRect; var AHandled: Boolean) of object;
  TcxEditDrawButtonBorderEvent = procedure (Sender: TcxEditButtonViewInfo;
    ACanvas: TcxCanvas; var ABackgroundRect, AContentRect: TRect; var AHandled: Boolean) of object;
  TcxEditGetButtonContentColorEvent = procedure (Sender: TcxEditButtonViewInfo; var AColor: TColor) of object;
  TcxEditGetButtonStateEvent = procedure (Sender: TcxEditButtonViewInfo; var AState: TcxEditButtonState) of object;
  TcxEditPaintEvent = procedure (Sender: TcxCustomEditViewInfo; ACanvas: TcxCanvas) of object;
  TcxEditPrepareButtonFadingImageEvent = procedure (ASender: TcxEditButtonViewInfo;
    AState: TcxEditButtonState; out AImage: TcxBitmap32; var AHandled: Boolean) of object;

  TcxDrawBackgroundStyle = (dbsCustom, dbsCustomEdit, dbsNone, dbsSimpleFill, dbsSimpleParent, dbsThemeParent);
  TcxEditViewInfoState = (evsPaintButtons);
  TcxEditViewInfoStates = set of TcxEditViewInfoState;
  TcxEditErrorType = (eetNone, eetError, eetWarning, eetInfo, eetCustom);

  { TcxEditValidateInfo }

  TcxEditValidateInfo = class
  strict private
    FErrorIcon: TdxSmartGlyph;
    FErrorText: string;
    FErrorType: TcxEditErrorType;

    function GetErrorIcon: TdxSmartGlyph;
    procedure SetErrorType(AValue: TcxEditErrorType);
    procedure SetErrorIcon(AValue: TdxSmartGlyph);
  protected
    function IsError: Boolean;
    procedure Reset;
  public
    constructor Create;
    destructor Destroy; override;

    property ErrorIcon: TdxSmartGlyph read GetErrorIcon write SetErrorIcon;
    property ErrorText: string read FErrorText write FErrorText;
    property ErrorType: TcxEditErrorType read FErrorType write SetErrorType;
  end;

  { TcxCustomEditViewInfo }

  TcxCustomEditViewInfo = class(TcxContainerViewInfo)
  strict private type
  {$REGION 'strict private type'}
    TAfterDrawEvent = procedure(ACanvas: TcxCanvas) of object;
    TAfterDrawEventHandler = TdxMulticastMethod<TAfterDrawEvent>;
    TCanDrawEditValueEvent = procedure(Sender: TcxCustomEditViewInfo; var Allow: Boolean) of object;
    TCanDrawEditValueEventHandler = TdxMulticastMethod<TCanDrawEditValueEvent>;
    TCalculateEditorBoundsEvent = procedure(AViewInfo: TcxCustomEditViewInfo; var R: TRect) of object;
    TCalculateEditorBoundsEventHandler = TdxMulticastMethod<TCalculateEditorBoundsEvent>;
  {$ENDREGION}
  private
    FScaleFactor: TdxScaleFactor;

    FOnAfterDrawBackground: TAfterDrawEventHandler;
    FOnAfterDrawValue: TAfterDrawEventHandler;
    FOnCalculateEditorBounds: TCalculateEditorBoundsEventHandler;
    FOnCanDrawEditValue: TCanDrawEditValueEventHandler;
    FOnCanStartButtonFading: TcxEditCanStartButtonFadingEvent;
    FOnDrawBackground: TcxEditDrawBackgroundEvent;
    FOnDrawButton: TcxEditDrawButtonEvent;
    FOnDrawButtonBackground: TcxEditDrawButtonBackgroundEvent;
    FOnDrawButtonBorder: TcxEditDrawButtonBorderEvent;
    FOnGetButtonContentColor: TcxEditGetButtonContentColorEvent;
    FOnGetButtonState: TcxEditGetButtonStateEvent;
    FOnPaint: TcxEditPaintEvent;
    FOnPrepareButtonFadingImage: TcxEditPrepareButtonFadingImageEvent;

    function DoCanStartButtonFading: Boolean;
    function DoDrawBackground(ACanvas: TcxCanvas): Boolean;
    function DoDrawButton(ACanvas: TcxCanvas; AViewInfo: TcxEditButtonViewInfo): Boolean;
    function DoDrawButtonBackground(ACanvas: TcxCanvas; const ARect: TRect; AViewInfo: TcxEditButtonViewInfo): Boolean;
    function DoDrawButtonBorder(ACanvas: TcxCanvas; AViewInfo: TcxEditButtonViewInfo; var ABackgroundRect, AContentRect: TRect): Boolean;
    function DoPrepareButtonFadingImage(ASender: TcxEditButtonViewInfo; AState: TcxEditButtonState; out AImage: TcxBitmap32): Boolean;
    function GetDrawBackgroundStyle: TcxDrawBackgroundStyle;
    function IsNativeBackground: Boolean;
    function IsTransparent: Boolean;

    function GetButtonColorPalette(AState: TcxEditButtonState): IdxColorPalette;
    function GetHintTextRect(const P: TPoint; APart: Integer): TRect;
  protected
    FEdit: TcxCustomEdit;
    FState: TcxEditViewInfoStates;
    FErrorBounds: TRect;
    FEditorBounds: TRect;
    FErrorData: TcxEditValidateInfo;
    FNeedShowErrorIcon: Boolean;

    function GetCurrentCursor(const AMousePos: TPoint): TCursor; virtual;
    procedure InplaceMouseDown(AButton: TMouseButton; const AShift: TShiftState; X, Y: Integer); virtual;

    function GetBackgroundPaintingStyle: TcxEditBackgroundPaintingStyle; virtual;
    function GetButtonViewInfoClass: TcxEditButtonViewInfoClass; virtual;
    procedure GetColorSettingsByPainter(out ABackground, ATextColor: TColor); virtual;
    function GetContainerBorderStyle: TcxContainerBorderStyle; override;
    function GetHasButtonsStateChanges: Boolean;
    function GetHintText(APart: Integer): string; virtual;
    function GetPart(const P: TPoint): Integer; virtual;
    function GetPartRect(APart: Integer): TRect; virtual;
    function HasNonClientArea: Boolean;
    procedure InternalPaint(ACanvas: TcxCanvas); override;
    procedure InternalPrepareEditButtonBackground(ACanvas: TcxCanvas; AViewInfo: TcxEditButtonViewInfo;
      out AContentRect, ABackgroundRect: TRect; out APenColor, ABrushColor: TColor; ACalculateOnly: Boolean);
    function IsRepaintOnStateChangingNeeded: Boolean; virtual;
    procedure DrawEditButton(ACanvas: TcxCanvas; AButtonVisibleIndex: Integer); virtual;
    procedure SetOnDrawBackground(AValue: TcxEditDrawBackgroundEvent); virtual;
    procedure StoreLastState; virtual;

    procedure DoAfterDrawBackground(ACanvas: TcxCanvas);
    procedure DoAfterDrawValue(ACanvas: TcxCanvas);

    procedure Draw3DButtonBorder(ACanvas: TcxCanvas; AButtonViewInfo: TcxEditButtonViewInfo;
      var ARect: TRect; out AContentRect: TRect; var APenColor, ABrushColor: TColor); virtual;
    procedure DrawFlatButtonBorder(ACanvas: TcxCanvas; AButtonViewInfo: TcxEditButtonViewInfo;
      var ARect: TRect; out AContentRect: TRect; var APenColor, ABrushColor: TColor); virtual;
    procedure DrawHotFlatButtonBorder(ACanvas: TcxCanvas; AButtonViewInfo: TcxEditButtonViewInfo;
      var ARect: TRect; out AContentRect: TRect; var APenColor, ABrushColor: TColor); virtual;
    procedure DrawNativeButtonBorder(ACanvas: TcxCanvas; AButtonViewInfo: TcxEditButtonViewInfo;
      var ARect: TRect; out AContentRect: TRect; var APenColor, ABrushColor: TColor); virtual;
    procedure DrawSimpleButtonBorder(ACanvas: TcxCanvas; AButtonViewInfo: TcxEditButtonViewInfo;
      var ARect: TRect; out AContentRect: TRect; var APenColor, ABrushColor: TColor); virtual;
    procedure DrawUltraFlatButtonBorder(ACanvas: TcxCanvas; AButtonViewInfo: TcxEditButtonViewInfo;
      AIsOffice11Style: Boolean; var ARect: TRect; var AContentRect: TRect; out APenColor, ABrushColor: TColor); virtual;

    procedure CalculateEditorBounds;
    procedure CalculateNativeInfo(out AThemedObjectType: TdxThemedObjectType;
      out ANativePart: Integer);
    procedure DrawButtonBorderByPainter(AButtonViewInfo: TcxEditButtonViewInfo;
      var ARect: TRect; out AContentRect: TRect; var APenColor, ABrushColor: TColor);
    procedure DrawNativeButtonBackground(ACanvas: TcxCanvas;
      AButtonViewInfo: TcxEditButtonViewInfo; const ARect: TRect); virtual;
    procedure DrawUsualButtonBackground(ACanvas: TcxCanvas;
      AButtonViewInfo: TcxEditButtonViewInfo; const ARect: TRect; ABrushColor: TColor); virtual;
  public
    BorderExtent: TRect;
    BorderStyle: TcxEditBorderStyle;
    ButtonsInfo: array of TcxEditButtonViewInfo;
    EditProperties: TcxCustomEditProperties;
    Data: Integer;
    Enabled: Boolean;
    Focused: Boolean;
    Font: TFont;
    HasBackground: Boolean;
    HasContentOffsets: Boolean;
    HasInnerEdit: Boolean;
    HasTextButtons: Boolean;
    HitTestInfo: Integer;
    HotState: TcxContainerHotState;
    InnerEditRect: TRect;
    IsButtonReallyPressed: Boolean;
    IsContainerInnerControl: Boolean;
    IsDBEditPaintCopyDrawing: Boolean;
    IsDesigning: Boolean;
    IsInplace: Boolean;
    IsSelected: Boolean;
    LastPressedButton: Integer;
    LastSelectedButton: Integer;
    Left: Integer;
    PaintOptions: TcxEditPaintOptions;
    PopupBorderStyle: TcxEditPopupBorderStyle;
    PressedButton: Integer;
    SelectedButton: Integer;
    ShadowRect: TRect;
    TextColor: TColor;
    Top: Integer;
    Transparent: Boolean;
    WindowHandle: HWND;
    UseRightToLeftAlignment: Boolean;

    // Base
    constructor Create; override;
    destructor Destroy; override;
    procedure Assign(Source: TObject); override;

    // Draw
    function CanDrawEditValue: Boolean; virtual;
    function DrawBackground(ACanvas: TcxCanvas): Boolean; overload;
    function DrawBackground(ACanvas: TcxCanvas; const APos: TPoint): Boolean; overload;
    procedure DrawCustomEdit(ACanvas: TcxCanvas; ACanDrawBackground, ACanDrawValidationMark: Boolean);
    procedure DrawCustomEditValidationMark(ACanvas: TcxCanvas);
    procedure DrawEditBackground(ACanvas: TcxCanvas; ARect, AGlyphRect: TRect; AGlyphTransparent: Boolean);
    procedure DrawNativeStyleEditBackground(ACanvas: TcxCanvas;
      ADrawBackground: Boolean; ABackgroundBrush: TBrushHandle); virtual;

    // Button Draw
    procedure DrawButton(ACanvas: TcxCanvas; AButtonVisibleIndex: Integer); virtual;
    procedure DrawButtons(ACanvas: TcxCanvas);
    procedure DrawButtonBackground(ACanvas: TcxCanvas; AViewInfo: TcxEditButtonViewInfo;
      const ARect, AContentRect: TRect; ABrushColor: TColor);
    procedure DrawButtonBorder(ACanvas: TcxCanvas; AViewInfo: TcxEditButtonViewInfo;
      var ARect, AContentRect: TRect; var APenColor, ABrushColor: TColor); virtual;
    procedure DrawButtonContent(ACanvas: TcxCanvas; AViewInfo: TcxEditButtonViewInfo;
      const AContentRect: TRect; APenColor, ABrushColor: TColor; ANeedOffsetContent: Boolean); virtual;

    //Flags
    function IsBackgroundPartiallyTransparent: Boolean; virtual;
    function IsBackgroundTransparent: Boolean;
    function IsCustomBackground: Boolean;
    function IsCustomButton(AButtonVisibleIndex: Integer = 0): Boolean;
    function IsCustomButtonBackground(AButtonVisibleIndex: Integer = 0): Boolean;
    function IsCustomButtonBorder(AButtonVisibleIndex: Integer = 0): Boolean;
    function IsCustomDrawButton(AButtonVisibleIndex: Integer = 0): Boolean;
    function IsFadingAvailable: Boolean; virtual;
    function IsHotTrack: Boolean; overload; virtual;
    function IsHotTrack(P: TPoint): Boolean; overload; virtual;
    function NeedShowHint(ACanvas: TcxCanvas; const P: TPoint;
      const AVisibleBounds: TRect; out AText: TCaption;
      out AIsMultiLine: Boolean; out ATextRect: TRect; AMaxLineCount: Integer = 0): Boolean; virtual;

    // Main Functions
    procedure Paint(ACanvas: TcxCanvas); override;
    procedure PaintEx(ACanvas: TcxCanvas);

    // Subsidiary functions
    function GetUpdateRegion(AViewInfo: TcxContainerViewInfo): TcxRegion; override;
    procedure Offset(DX, DY: Integer); override;
    procedure PrepareCanvasFont(ACanvas: TCanvas); virtual;
    function Repaint(AControl: TWinControl;
      AViewInfo: TcxContainerViewInfo = nil): Boolean; overload; virtual;
    function Repaint(AControl: TWinControl; const AInnerEditRect: TRect;
      AViewInfo: TcxContainerViewInfo = nil): Boolean; overload; virtual;
    procedure ResetValidationInfo;
    procedure SetButtonCount(ACount: Integer);
    function SetOrigin(const APoint: TPoint): TPoint;

    //
    property BackgroundPaintingStyle: TcxEditBackgroundPaintingStyle read GetBackgroundPaintingStyle;
    property Edit: TcxCustomEdit read FEdit;
    property ErrorData: TcxEditValidateInfo read FErrorData;
    property ScaleFactor: TdxScaleFactor read FScaleFactor;

    //
    property OnAfterDrawBackground: TAfterDrawEventHandler read FOnAfterDrawBackground;
    property OnAfterDrawValue: TAfterDrawEventHandler read FOnAfterDrawValue;
    property OnCalculateEditorBounds: TCalculateEditorBoundsEventHandler read FOnCalculateEditorBounds;
    property OnCanDrawEditValue: TCanDrawEditValueEventHandler read FOnCanDrawEditValue;
    //
    property OnCanStartButtonFading: TcxEditCanStartButtonFadingEvent read FOnCanStartButtonFading write FOnCanStartButtonFading;
    property OnDrawBackground: TcxEditDrawBackgroundEvent read FOnDrawBackground write SetOnDrawBackground;
    property OnDrawButton: TcxEditDrawButtonEvent read FOnDrawButton write FOnDrawButton;
    property OnDrawButtonBackground: TcxEditDrawButtonBackgroundEvent read FOnDrawButtonBackground write FOnDrawButtonBackground;
    property OnDrawButtonBorder: TcxEditDrawButtonBorderEvent read FOnDrawButtonBorder write FOnDrawButtonBorder;
    property OnGetButtonContentColor: TcxEditGetButtonContentColorEvent read FOnGetButtonContentColor write FOnGetButtonContentColor;
    property OnGetButtonState: TcxEditGetButtonStateEvent read FOnGetButtonState write FOnGetButtonState;
    property OnPaint: TcxEditPaintEvent read FOnPaint write FOnPaint;
    property OnPrepareButtonFadingImage: TcxEditPrepareButtonFadingImageEvent read FOnPrepareButtonFadingImage write FOnPrepareButtonFadingImage;
  end;

  { TcxCustomEditViewData }

  TcxInplaceEditPosition = record
    Item: TObject;
    RecordIndex: TdxNativeInt;
  end;

  TcxInplaceEditParams = record
    MultiRowParent: Boolean;
    Position: TcxInplaceEditPosition;
  end;

  TcxEditSizeProperties = record
    Height: Integer;
    MaxLineCount: Integer;
    Width: Integer;
  end;
  PcxEditSizeProperties = ^TcxEditSizeProperties;

  TcxEditContentOption = (ecoShowFocusRectWhenInplace, ecoOffsetButtonContent);
  TcxEditContentOptions = set of TcxEditContentOption;

  TcxEditContentParams = record
    ExternalBorderBounds: TRect;
    Offsets: TRect;
    Options: TcxEditContentOptions;
    SizeCorrection: TSize;
  end;

  TcxEditGetDefaultButtonWidthEvent = procedure(Sender: TcxCustomEditViewData;
    AIndex: Integer; var ADefaultWidth: Integer) of object;
  TcxEditViewDataGetDisplayTextEvent = procedure(Sender: TcxCustomEditViewData;
    var AText: string) of object;

  TcxCustomEditViewData = class(TPersistent)
  private
    FData: TObject; // internal for OnGetDisplayText event
    FIsInplace: Boolean;
    FLeftSideLeftmostButtonIndex, FLeftSideRightmostButtonIndex: Integer;
    FRightSideLeftmostButtonIndex, FRightSideRightmostButtonIndex: Integer;
    FScaleFactor: TdxScaleFactor;
    FValidationErrorIconAlignment: TLeftRight;

    FOnGetDefaultButtonWidth: TcxEditGetDefaultButtonWidthEvent;
    FOnGetDisplayText: TcxEditViewDataGetDisplayTextEvent;

    procedure DoGetButtonState(AViewInfo: TcxEditButtonViewInfo; var AState: TcxEditButtonState);
    function DoGetDefaultButtonWidth(AIndex: Integer): Integer;
    function GetStyle: TcxCustomEditStyle;
    procedure SetStyle(Value: TcxCustomEditStyle);
  protected
    FEdit: TcxCustomEdit;
    FProperties: TcxCustomEditProperties;
    FStyle: TcxCustomEditStyle;

    // ButtonNativeInfo
    procedure CalculateButtonNativeInfo(AButtonViewInfo: TcxEditButtonViewInfo); virtual;
    procedure CalculateButtonNativePartInfo(ATheme: TdxTheme; AButtonViewInfo: TcxEditButtonViewInfo); virtual;
    function GetButtonNativeTheme(AButtonViewInfo: TcxEditButtonViewInfo): TdxTheme; virtual;
    //
    function CalculatePaintOptions: TcxEditPaintOptions; virtual;
    procedure CalculateViewInfo(AViewInfo: TcxCustomEditViewInfo; AIsMouseEvent: Boolean);
    function CanPressButton(AViewInfo: TcxCustomEditViewInfo; AButtonVisibleIndex: Integer):  Boolean; virtual;
    procedure CheckSizeConstraints(var AEditSize: TSize);
    procedure CorrectBorderStyle(var ABorderStyle: TcxEditBorderStyle);
    procedure DoOnGetDisplayText(var AText: string);
    function EditValueToDisplayText(AEditValue: TcxEditValue): string;
    function GetButtonsStyle: TcxEditButtonStyle;
    function GetCaptureButtonVisibleIndex: Integer;
    procedure GetColorSettings(AViewInfo: TcxCustomEditViewInfo; var FillColor, TextColor: TColor);
    function GetContainerState(const ABounds: TRect; const P: TPoint;
      Button: TcxMouseButton; Shift: TShiftState; AIsMouseEvent: Boolean): TcxContainerState; virtual;
    function GetEditContentDefaultOffsets: TRect; virtual;
    function GetEditNativeState(AViewInfo: TcxCustomEditViewInfo): Integer; virtual;
    function GetErrorIconWidth(AErrorData: TcxEditValidateInfo): Integer;
    procedure InitCacheData; virtual;
    procedure InitEditContentParams(var AParams: TcxEditContentParams); virtual;
    procedure Initialize; virtual;
    function InternalEditValueToDisplayText(AEditValue: TcxEditValue): string; virtual;
    function InternalFocused: Boolean;
    function InternalGetEditConstantPartSize(ACanvas: TcxCanvas; AIsInplace: Boolean;
      AEditSizeProperties: TcxEditSizeProperties; var MinContentSize: TSize; AViewInfo: TcxCustomEditViewInfo): TSize; virtual;
    function InternalGetEditContentSize(ACanvas: TcxCanvas; const AEditValue: TcxEditValue;
      const AEditSizeProperties: TcxEditSizeProperties): TSize; virtual;
    function InternalGetEditSize(ACanvas: TcxCanvas; const AEditValue: TcxEditValue;
      AEditSizeProperties: TcxEditSizeProperties; AViewInfo: TcxCustomEditViewInfo = nil; AIsEditing: Boolean = False): TSize;
    function IsButtonPressed(AViewInfo: TcxCustomEditViewInfo; AButtonVisibleIndex: Integer): Boolean; virtual;
    function NeedShowErrorIcon(AErrorData: TcxEditValidateInfo): Boolean;
  public
    ButtonsOnlyStyle: Boolean;
    ButtonVisibleCount: Integer;
    ContainerState: TcxContainerState;
    ContentOffset: TRect;
    EditContentParams: TcxEditContentParams;
    Enabled: Boolean;
    Focused: Boolean;
    HorzAlignment: TcxEditHorzAlignment;
    HScrollBar: IcxControlScrollBar;
    InnerEdit: IcxCustomInnerEdit;
    InplaceEditParams: TcxInplaceEditParams;
    IsDesigning: Boolean;
    IsSelected: Boolean; // Row selected
    IsTouchScrollUIMode: Boolean;
    IsValueSource: Boolean;
    MaxLineCount: Integer;
    NativeStyle: Boolean;
    PaintOptions: TcxEditPaintOptions;
    PreviewMode: Boolean;
    Selected: Boolean;
    SelStart, SelLength: Integer;
    SelTextColor, SelBackgroundColor: TColor;
    SupportsTouchMode: Boolean;
    UseRightToLeftAlignment: Boolean;
    UseRightToLeftReading: Boolean;
    UseRightToLeftScrollBar: Boolean;
    VertAlignment: TcxEditVertAlignment;
    VScrollBar: IcxControlScrollBar;
    WindowHandle: HWND;

    constructor Create(AProperties: TcxCustomEditProperties; AStyle: TcxCustomEditStyle; AIsInplace: Boolean); reintroduce; virtual;
    destructor Destroy; override;

    procedure Calculate(ACanvas: TcxCanvas; const ABounds: TRect; const P: TPoint;
      Button: TcxMouseButton; Shift: TShiftState; AViewInfo: TcxCustomEditViewInfo; AIsMouseEvent: Boolean); virtual;
    procedure CalculateButtonBounds(ACanvas: TcxCanvas; AViewInfo: TcxCustomEditViewInfo;
      AButtonVisibleIndex: Integer; var ButtonsRect: TRect); virtual;
    procedure CalculateButtonsViewInfo(ACanvas: TcxCanvas; const ABounds: TRect; const P: TPoint;
      Button: TcxMouseButton; Shift: TShiftState; AViewInfo: TcxCustomEditViewInfo; AIsMouseEvent: Boolean); virtual;
    procedure CalculateButtonViewInfo(ACanvas: TcxCanvas; AViewInfo: TcxCustomEditViewInfo;
      AButtonVisibleIndex: Integer; var ButtonsRect: TRect); virtual;
    procedure DoCalculateButtonViewInfos(ACanvas: TcxCanvas;
      var AButtonsRect: TRect; AViewInfo: TcxCustomEditViewInfo); virtual;
    procedure CalculateEx(ACanvas: TcxCanvas; const ABounds: TRect; const P: TPoint;
      Button: TcxMouseButton; Shift: TShiftState; AViewInfo: TcxCustomEditViewInfo; AIsMouseEvent: Boolean);
    procedure CheckButtonsOnly(AViewInfo: TcxCustomEditViewInfo; APrevButtonsWidth, AButtonsWidth: Integer); virtual;
    procedure CheckStartButtonsFading(AViewInfo: TcxCustomEditViewInfo); virtual;
    procedure DoRightToLeftConversion(AViewInfo: TcxCustomEditViewInfo; const ABounds: TRect); virtual;
    procedure EditValueToDrawValue(const AEditValue: TcxEditValue; AViewInfo: TcxCustomEditViewInfo); virtual;
    procedure InitializeButtonInfo(AViewInfo: TcxCustomEditViewInfo; AButtonVisibleIndex: Integer); virtual;

    function GetBorderColor: TColor; virtual;
    function GetBorderColorByPainter(AIsHighlight: Boolean): TColor; virtual;
    function GetBorderExtent: TRect; virtual;
    function GetBorderExtentByEdges(ABorderWidth: Integer): TRect; virtual;
    function GetBorderExtentByPainter: TRect; virtual;
    function GetBorderStyle: TcxEditBorderStyle; virtual;
    function GetButtonsExtent(ACanvas: TcxCanvas): TRect; virtual;
    function GetClientExtent(ACanvas: TcxCanvas; AViewInfo: TcxCustomEditViewInfo): TRect; virtual;
    function GetEditConstantPartSize(ACanvas: TcxCanvas; const AEditSizeProperties: TcxEditSizeProperties;
      var MinContentSize: TSize; AViewInfo: TcxCustomEditViewInfo = nil): TSize; virtual;
    function GetEditContentSize(ACanvas: TcxCanvas; const AEditValue: TcxEditValue;
      const AEditSizeProperties: TcxEditSizeProperties; AErrorData: TcxEditValidateInfo = nil): TSize; virtual;
    function GetEditingContentSize(ACanvas: TcxCanvas; const AEditValue: TcxEditValue;
      const AEditSizeProperties: TcxEditSizeProperties): TSize; virtual;
    function GetEditContentSizeCorrection: TSize; virtual;
    function GetEditSize(ACanvas: TcxCanvas; const AEditValue: TcxEditValue;
      AEditSizeProperties: TcxEditSizeProperties; AViewInfo: TcxCustomEditViewInfo = nil): TSize;
    function GetEditingSize(ACanvas: TcxCanvas; const AEditValue: TcxEditValue;
      AEditSizeProperties: TcxEditSizeProperties; AViewInfo: TcxCustomEditViewInfo = nil): TSize;
    function HasShadow: Boolean; virtual;
    function IgnoreButtonWhileStretching(AButtonVisibleIndex: Integer): Boolean; virtual;
    function IsButtonLeftAligned(AViewInfo: TcxCustomEditViewInfo; AButtonVisibleIndex: Integer): Boolean; virtual;
    class function IsNativeStyle(ALookAndFeel: TcxLookAndFeel): Boolean; virtual;

    property Data: TObject read FData write FData;
    property Edit: TcxCustomEdit read FEdit write FEdit;
    property IsInplace: Boolean read FIsInplace;
    property Properties: TcxCustomEditProperties read FProperties;
    property ScaleFactor: TdxScaleFactor read FScaleFactor;
    property Style: TcxCustomEditStyle read GetStyle write SetStyle;

    property OnGetDefaultButtonWidth: TcxEditGetDefaultButtonWidthEvent read FOnGetDefaultButtonWidth write FOnGetDefaultButtonWidth;
    property OnGetDisplayText: TcxEditViewDataGetDisplayTextEvent read FOnGetDisplayText write FOnGetDisplayText;
  end;

  TcxCustomEditViewDataClass = class of TcxCustomEditViewData;

  { TcxEditStyleController }

  TcxEditStyle = class;

  TcxEditStyleController = class(TcxStyleController)
  strict private
    function GetInternalStyle(AState: TcxContainerStateItem): TcxCustomEditStyle;
    function GetStyle: TcxEditStyle;
    function GetStyleDisabled: TcxEditStyle;
    function GetStyleFocused: TcxEditStyle;
    function GetStyleHot: TcxEditStyle;
    procedure SetInternalStyle(AState: TcxContainerStateItem; Value: TcxCustomEditStyle);
    procedure SetStyle(Value: TcxEditStyle);
    procedure SetStyleDisabled(Value: TcxEditStyle);
    procedure SetStyleFocused(Value: TcxEditStyle);
    procedure SetStyleHot(Value: TcxEditStyle);
  protected
    function GetStyleClass: TcxContainerStyleClass; override;
  public
    property Styles[AState: TcxContainerStateItem]: TcxCustomEditStyle read GetInternalStyle write SetInternalStyle;
  published
    property Style: TcxEditStyle read GetStyle write SetStyle;
    property StyleDisabled: TcxEditStyle read GetStyleDisabled write SetStyleDisabled;
    property StyleFocused: TcxEditStyle read GetStyleFocused write SetStyleFocused;
    property StyleHot: TcxEditStyle read GetStyleHot write SetStyleHot;
    property OnStyleChanged;
  end;

  { TcxCustomEditStyle }

  TcxCustomEditStyleClass = class of TcxCustomEditStyle;
  TcxCustomEditStyle = class(TcxContainerStyle)
  private
    FButtonStyle: TcxEditButtonStyle;
    FButtonTransparency: TcxEditButtonTransparency;
    FGradient: Boolean;
    FGradientButtons: Boolean;
    FGradientDirection: TcxEditGradientDirection;
    FPopupBorderStyle: TcxEditPopupBorderStyle;
    FPopupCloseButton: Boolean;

    function GetActiveStyleController: TcxEditStyleController;
    function GetAssignedValues: TcxEditStyleValues;
    function GetBaseStyle: TcxCustomEditStyle;
    function GetBorderStyle: TcxEditBorderStyle;
    function GetButtonStyle: TcxEditButtonStyle;
    function GetButtonTransparency: TcxEditButtonTransparency;
    function GetEdit: TcxCustomEdit;
    function GetGradient: Boolean;
    function GetGradientButtons: Boolean;
    function GetGradientDirection: TcxEditGradientDirection;
    function GetPopupBorderStyle: TcxEditPopupBorderStyle;
    function GetPopupCloseButton: Boolean;
    function GetStyleController: TcxEditStyleController;

    function InternalGetButtonStyle(var ButtonStyle: TcxEditButtonStyle): Boolean;
    function InternalGetButtonTransparency(var ButtonTransparency: TcxEditButtonTransparency): Boolean;
    function InternalGetGradient(var Gradient: Boolean): Boolean;
    function InternalGetGradientButtons(var GradientButtons: Boolean): Boolean;
    function InternalGetGradientDirection(var GradientDirection: TcxEditGradientDirection): Boolean;
    function InternalGetPopupBorderStyle(var PopupBorderStyle: TcxEditPopupBorderStyle): Boolean;

    function IsBorderStyleStored: Boolean;
    function IsButtonStyleStored: Boolean;
    function IsButtonTransparencyStored: Boolean;
    function IsGradientStored: Boolean;
    function IsGradientButtonsStored: Boolean;
    function IsGradientDirectionStored: Boolean;
    function IsPopupBorderStyleStored: Boolean;
    function IsStyleControllerStored: Boolean;

    procedure SetAssignedValues(Value: TcxEditStyleValues);
    procedure SetBorderStyle(Value: TcxEditBorderStyle);
    procedure SetButtonStyle(Value: TcxEditButtonStyle);
    procedure SetButtonTransparency(Value: TcxEditButtonTransparency);
    procedure SetGradient(Value: Boolean);
    procedure SetGradientButtons(Value: Boolean);
    procedure SetGradientDirection(Value: TcxEditGradientDirection);
    procedure SetPopupBorderStyle(Value: TcxEditPopupBorderStyle);
    procedure SetPopupCloseButton(Value: Boolean);
    procedure SetStyleController(Value: TcxEditStyleController);
  protected
    function GetDefaultStyleController: TcxStyleController; override;
    function InternalGetNotPublishedExtendedStyleValues: TcxEditStyleValues; override;
    function DefaultButtonStyle: TcxEditButtonStyle; virtual;
    function DefaultButtonTransparency: TcxEditButtonTransparency; virtual;
    function DefaultGradient: Boolean; virtual;
    function DefaultGradientButtons: Boolean; virtual;
    function DefaultGradientDirection: TcxEditGradientDirection; virtual;
    function DefaultPopupBorderStyle: TcxEditPopupBorderStyle; virtual;
    property PopupCloseButton: Boolean read GetPopupCloseButton write SetPopupCloseButton default True;
  public
    constructor Create(AOwner: TPersistent; ADirectAccessMode: Boolean;
      AParentStyle: TcxContainerStyle = nil; AState: TcxContainerStateItem = csNormal); override;
    procedure Assign(Source: TPersistent); override;
    function GetStyleValueCount: Integer; override;
    function GetStyleValueName(AStyleValue: TcxEditStyleValue; out StyleValueName: string): Boolean; override;
    function IsValueAssigned(AValue: TcxEditStyleValue): Boolean; override;
    procedure Init(AParams: TcxViewParams);

    property ActiveStyleController: TcxEditStyleController read GetActiveStyleController;
    property AssignedValues: TcxEditStyleValues read GetAssignedValues write SetAssignedValues stored False;
    property BaseStyle: TcxCustomEditStyle read GetBaseStyle;
    property BorderStyle: TcxEditBorderStyle read GetBorderStyle write SetBorderStyle stored IsBorderStyleStored;
    property ButtonStyle: TcxEditButtonStyle read GetButtonStyle write SetButtonStyle stored IsButtonStyleStored;
    property ButtonTransparency: TcxEditButtonTransparency read GetButtonTransparency write SetButtonTransparency stored IsButtonTransparencyStored;
    property Edit: TcxCustomEdit read GetEdit;
    property Gradient: Boolean read GetGradient write SetGradient stored IsGradientStored;
    property GradientButtons: Boolean read GetGradientButtons write SetGradientButtons stored IsGradientButtonsStored;
    property GradientDirection: TcxEditGradientDirection read GetGradientDirection write SetGradientDirection stored IsGradientDirectionStored;
    property PopupBorderStyle: TcxEditPopupBorderStyle read GetPopupBorderStyle write SetPopupBorderStyle stored IsPopupBorderStyleStored;
    property StyleController: TcxEditStyleController read GetStyleController write SetStyleController stored IsStyleControllerStored;
  end;

  { TcxEditStyle }

  TcxEditStyle = class(TcxCustomEditStyle)
  published
    property AssignedValues;
    property BorderColor;
    property BorderStyle;
    property ButtonStyle;
    property ButtonTransparency;
    property Color;
    property Edges;
    property Font;
    property Gradient;
//    property GradientButtons;
    property GradientDirection;
    property HotTrack;
    property LookAndFeel;
    property PopupBorderStyle;
    property Shadow;
    property StyleController;
    property TextColor;
    property TextStyle;
    property TransparentBorder;
  end;

  { TcxCustomEditPropertiesValues }

  TcxCustomEditPropertiesValues = class(TPersistent)
  private
    FOwner: TPersistent;
    FMaxValue: Boolean;
    FMinValue: Boolean;
    FReadOnly: Boolean;
    function GetProperties: TcxCustomEditProperties;
    procedure SetMaxValue(Value: Boolean);
    procedure SetMinValue(Value: Boolean);
    procedure SetReadOnly(Value: Boolean);
  protected
    function GetOwner: TPersistent; override;
    procedure Changed;
    function IsMaxValueStored: Boolean;
    function IsMinValueStored: Boolean;
    function IsPropertiesPropertyVisible(const APropertyName: string): Boolean;
    property MaxValue: Boolean read FMaxValue write SetMaxValue stored IsMaxValueStored;
    property MinValue: Boolean read FMinValue write SetMinValue stored IsMinValueStored;
    property Properties: TcxCustomEditProperties read GetProperties;
  public
    constructor Create(AOwner: TPersistent); virtual;
    procedure Assign(Source: TPersistent); override;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure RestoreDefaults; virtual;
  published
    property ReadOnly: Boolean read FReadOnly write SetReadOnly stored False;
  end;

  TcxCustomEditPropertiesValuesClass = class of TcxCustomEditPropertiesValues;

  { TcxCustomEditDefaultValuesProvider }

  TcxCustomEditDefaultValuesProvider = class(TcxInterfacedPersistent,
    IUnknown, IcxEditDefaultValuesProvider)
  public
    destructor Destroy; override;
    function CanSetEditMode: Boolean; virtual;
    procedure ClearUsers;
    function DefaultAlignment: TAlignment; virtual;
    function DefaultBlobKind: TcxBlobKind; virtual;
    function DefaultCanModify: Boolean; virtual;
    function DefaultDisplayFormat: string; virtual;
    function DefaultEditFormat: string; virtual;
    function DefaultEditMask: string; virtual;
    function DefaultIsFloatValue: Boolean; virtual;
    function DefaultMaxLength: Integer; virtual;
    function DefaultMaxValue: Double; virtual;
    function DefaultMinValue: Double; virtual;
    function DefaultPrecision: Integer; virtual;
    function DefaultReadOnly: Boolean; virtual;
    function DefaultRequired: Boolean; virtual;
    function GetInstance: TObject;
    function IsBlob: Boolean; virtual;
    function IsCurrency: Boolean; virtual;
    function IsDataAvailable: Boolean; virtual;
    function IsDataStorage: Boolean; virtual;
    function IsDisplayFormatDefined(AIsCurrencyValueAccepted: Boolean): Boolean; virtual;
    function IsOnGetTextAssigned: Boolean; virtual;
    function IsOnSetTextAssigned: Boolean; virtual;
    function IsValidChar(AChar: Char): Boolean; virtual;
  end;

  TcxCustomEditDefaultValuesProviderClass = class of TcxCustomEditDefaultValuesProvider;

  { TcxCustomEditProperties }

  TcxEditButtonClickEvent = procedure (Sender: TObject; AButtonIndex: Integer) of object;
  TcxEditEditingEvent = procedure(Sender: TObject; var CanEdit: Boolean) of object;
  TcxEditCloseUpReason = (crUnknown, crTab, crClose, crCancel, crEnter);
  TcxEditClosePopupEvent = procedure(Sender: TcxControl; AReason: TcxEditCloseUpReason) of object;

  TcxCustomEditProperties = class(TcxInterfacedPersistent, IdxScaleFactor)
  private
    FAutoSelect: Boolean;
    FBeepOnError: Boolean;
    FButtons: TcxEditButtons;
    FButtonsViewStyle: TcxEditButtonsViewStyle;
    FChangedOccurred: Boolean;
    FClearKey: TShortCut;
    FClickKey: TShortCut;
    FFreeNotificator: TcxFreeNotificator;
    FImageChangeLink: TChangeLink;
    FImmediatePost: Boolean;
    FInnerAlignment: TcxEditAlignment;
    FIsChangingCount: Integer;
    FMaxValue: Double;
    FMinValue: Double;
    FReadOnly: Boolean;
    FScaleFactor: TdxOwnedScaleFactor;
    FTransparent: Boolean;
    FUpdateCount: Integer;
    FUseLeftAlignmentOnEditing: Boolean;
    FUseMouseWheel: Boolean;

    FErrorIcon: TdxSmartImage;
    FValidateOnEnter: Boolean;
    FValidationErrorIconAlignment: TLeftRight;
    FValidationOptions: TcxEditValidationOptions;
    FInternalValidationOptions: TcxEditValidationOptions;

    FOnButtonClick: TcxEditButtonClickEvent;
    FOnChange: TNotifyEvent;
    FOnClosePopup: TcxEditClosePopupEvent;
    FOnEditValueChanged: TNotifyEvent;
    FOnValidate: TcxEditValidateEvent;
    FOnPropertiesChanged: TNotifyEvent;

    function BaseGetAlignment: TcxEditAlignment;
    function GetImages: TCustomImageList;
    function GetIsChanging: Boolean;
    function GetReadOnly: Boolean;
    function GetScaleFactor: TdxScaleFactor;
    procedure ImageListChange(Sender: TObject);
    function IsAlignmentStored: Boolean;
    function IsUseLeftAlignmentOnEditingStored: Boolean;
    function IsReadOnlyStored: Boolean;
    procedure SetAssignedValues(Value: TcxCustomEditPropertiesValues);
    procedure SetAutoSelect(Value: Boolean);
    procedure SetButtons(Value: TcxEditButtons);
    procedure SetButtonsViewStyle(Value: TcxEditButtonsViewStyle);
    procedure SetErrorIcon(Value: TdxSmartImage);
    procedure SetImages(Value: TCustomImageList);
    procedure SetUseLeftAlignmentOnEditing(Value: Boolean);
    procedure SetIDefaultValuesProvider(Value: IcxEditDefaultValuesProvider);
    procedure SetMaxValue(Value: Double);
    procedure SetMinValue(Value: Double);
    procedure SetReadOnly(Value: Boolean);
    procedure SetTransparent(Value: Boolean);
    procedure SetValidationErrorIconAlignment(AValue: TLeftRight);
    procedure SetValidationOptions(Value: TcxEditValidationOptions);
    procedure ScaleFactorChangeHandler(Sender: TObject; M, D: Integer; IsLoading: Boolean);
  protected
    FAlignment: TcxEditAlignment;
    FAssignedValues: TcxCustomEditPropertiesValues;
    FChangedLocked: Boolean;
    FIDefaultValuesProvider: IcxEditDefaultValuesProvider;

    procedure AlignmentChangedHandler(Sender: TObject); virtual;
    procedure BaseSetAlignment(Value: TcxEditAlignment); virtual;
    procedure ChangeScale(M, D: Integer); virtual;
    function CanModify: Boolean;
    function CanValidate: Boolean; virtual;
    procedure ChangeHandler(Sender: TObject); virtual;
    function DefaultUseLeftAlignmentOnEditing: Boolean; virtual;
    procedure DefaultValuesProviderDestroyed; virtual;
    procedure DoAssign(AProperties: TcxCustomEditProperties); virtual;
    procedure DoChanged; virtual;
    procedure DoValidate(var ADisplayValue: TcxEditValue; var AErrorText: TCaption;
      var AError: Boolean; AEdit: TcxCustomEdit; out AIsUserErrorDisplayValue: Boolean);
    procedure FillMinMaxValues(AMinValue, AMaxValue: Double);
    procedure FreeNotification(Sender: TComponent); virtual;
    class function GetAssignedValuesClass: TcxCustomEditPropertiesValuesClass; virtual;
    function GetDefaultHorzAlignment: TAlignment; virtual;
    function GetDefaultVertAlignment: TcxEditVertAlignment; virtual;
    function GetDisplayFormatOptions: TcxEditDisplayFormatOptions; virtual;
    function GetMaxValue: Double; virtual;
    function GetMinValue: Double; virtual;
    function GetValidateErrorText(AErrorKind: TcxEditErrorKind): string; virtual;
    function GetValueEditorEditValueSource(AEditFocused: Boolean): TcxDataEditValueSource; virtual;
    class function GetViewDataClass: TcxCustomEditViewDataClass; virtual;
    function HasDisplayValue: Boolean; virtual;
    function InnerEditNeedsTabs: Boolean; virtual;
    function IsDesigning: Boolean;
    function IsEditValueConversionDependOnFocused: Boolean; virtual;
    function IsMaxValueStored: Boolean;
    function IsMinValueStored: Boolean;
    procedure InitiateActions; virtual;
    function SupportsMultiThreading: Boolean; virtual;

    property AssignedValues: TcxCustomEditPropertiesValues read FAssignedValues write SetAssignedValues;
    property ButtonsViewStyle: TcxEditButtonsViewStyle read FButtonsViewStyle write SetButtonsViewStyle default bvsNormal;
    property DisplayFormatOptions: TcxEditDisplayFormatOptions read GetDisplayFormatOptions;
    property FreeNotificator: TcxFreeNotificator read FFreeNotificator;
    property MaxValue: Double read GetMaxValue write SetMaxValue stored IsMaxValueStored;
    property MinValue: Double read GetMinValue write SetMinValue stored IsMinValueStored;
    property ScaleFactor: TdxOwnedScaleFactor read FScaleFactor;
    property Transparent: Boolean read FTransparent write SetTransparent stored False; // deprecated
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure AfterConstruction; override;

    procedure Assign(Source: TPersistent); override;
    function CanCompareEditValue: Boolean; virtual;
    function Clone(AOwner: TPersistent): TcxCustomEditProperties; virtual;
    class function GetButtonsClass: TcxEditButtonsClass; virtual;
    class function GetContainerClass: TcxContainerClass; virtual;
    class function GetStyleClass: TcxCustomEditStyleClass; virtual;
    class function GetViewInfoClass: TcxContainerViewInfoClass; virtual;

    // Changes
    procedure BeginUpdate;
    procedure Changed;
    function ChangedLocked: Boolean;
    procedure DoUpdate(AProperties: TcxCustomEditProperties); virtual;
    procedure EndUpdate(AInvokeChanged: Boolean = True);
    procedure Update(AProperties: TcxCustomEditProperties);
    procedure LockUpdate(ALock: Boolean);

    function AllowRepositorySharing: Boolean; virtual;
    procedure RefreshNonShareable; virtual;

    function CompareDisplayValues(const AEditValue1, AEditValue2: TcxEditValue): Boolean; virtual;
    function CreatePreviewProperties: TcxCustomEditProperties; virtual;
    function CreateViewData(AStyle: TcxCustomEditStyle; AIsInplace: Boolean; APreviewMode: Boolean = False): TcxCustomEditViewData; virtual;
    procedure DataChanged; virtual;
    function GetDisplayText(const AEditValue: TcxEditValue; AFullText: Boolean = False; AIsInplace: Boolean = True): string; virtual;
    function GetEditConstantPartSize(ACanvas: TcxCanvas; AEditStyle: TcxCustomEditStyle;
      AIsInplace: Boolean; const AEditSizeProperties: TcxEditSizeProperties; var MinContentSize: TSize): TSize;
    function GetEditContentSize(ACanvas: TcxCanvas; AEditStyle: TcxCustomEditStyle;
      AIsInplace: Boolean;  const AEditValue: TcxEditValue; const AEditSizeProperties: TcxEditSizeProperties): TSize;
    function GetEditSize(ACanvas: TcxCanvas; AEditStyle: TcxCustomEditStyle;
      AIsInplace: Boolean; const AEditValue: TcxEditValue; AEditSizeProperties: TcxEditSizeProperties): TSize;
    function GetSpecialFeatures: TcxEditSpecialFeatures; virtual;
    function GetSupportedOperations: TcxEditSupportedOperations; virtual;
    function IsActivationKey(AKey: Char): Boolean; virtual;
    function IsEditValueValid(var EditValue: TcxEditValue; AEditFocused: Boolean): Boolean; virtual;
    function IsResetEditClass: Boolean; virtual;
    function IsValueFormattedByProperties: Boolean; virtual;
    function IsValueFormattedByProvider: Boolean; virtual;
    procedure PrepareDisplayValue(const AEditValue: TcxEditValue; var DisplayValue: TcxEditValue; AEditFocused: Boolean); virtual;
    procedure RestoreDefaults; virtual;
    procedure ValidateDisplayValue(var ADisplayValue: TcxEditValue; var AErrorText: TCaption; var AError: Boolean; AEdit: TcxCustomEdit); virtual;
    function GetEditValueSource(AEditFocused: Boolean): TcxDataEditValueSource; virtual;

    property Buttons: TcxEditButtons read FButtons write SetButtons;
    property IDefaultValuesProvider: IcxEditDefaultValuesProvider read FIDefaultValuesProvider write SetIDefaultValuesProvider;
    property Images: TCustomImageList read GetImages write SetImages;
    property IsChanging: Boolean read GetIsChanging;
    property OnPropertiesChanged: TNotifyEvent read FOnPropertiesChanged write FOnPropertiesChanged;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly stored IsReadOnlyStored;
    property UseLeftAlignmentOnEditing: Boolean read FUseLeftAlignmentOnEditing write SetUseLeftAlignmentOnEditing stored IsUseLeftAlignmentOnEditingStored;
    property UseMouseWheel: Boolean read FUseMouseWheel write FUseMouseWheel default True;
    // !!!
    property Alignment: TcxEditAlignment read BaseGetAlignment write BaseSetAlignment stored IsAlignmentStored;
    property AutoSelect: Boolean read FAutoSelect write SetAutoSelect default True;
    property BeepOnError: Boolean read FBeepOnError write FBeepOnError default False;
    property ClearKey: TShortCut read FClearKey write FClearKey default 0;
    property ClickKey: TShortCut read FClickKey write FClickKey default VK_RETURN + scCtrl;
    property ImmediatePost: Boolean read FImmediatePost write FImmediatePost default False;
    property ValidateOnEnter: Boolean read FValidateOnEnter write FValidateOnEnter default False;
    property ValidationErrorIconAlignment: TLeftRight read FValidationErrorIconAlignment write SetValidationErrorIconAlignment default taLeftJustify;
    property ValidationOptions: TcxEditValidationOptions read FValidationOptions write SetValidationOptions default [evoRaiseException];

    property ErrorIcon: TdxSmartImage read FErrorIcon write SetErrorIcon;

    property OnButtonClick: TcxEditButtonClickEvent read FOnButtonClick write FOnButtonClick;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnClosePopup: TcxEditClosePopupEvent read FOnClosePopup write FOnClosePopup;
    property OnEditValueChanged: TNotifyEvent read FOnEditValueChanged write FOnEditValueChanged;
    property OnValidate: TcxEditValidateEvent read FOnValidate write FOnValidate;
  end;

  { TcxDataBinding }

  TcxDataBinding = class(TcxCustomDataBinding);

  { TcxEditDataBinding }

  TInterfacedObjectClass = class of TInterfacedObject;

  TcxEditDataBinding = class(TPersistent)
  strict private
    FIDefaultValuesProvider: TcxCustomEditDefaultValuesProvider;

    function GetIDefaultValuesProvider: IcxEditDefaultValuesProvider;
  protected
    FEdit: TcxCustomEdit;

    function GetOwner: TPersistent; override;

    procedure DefaultValuesChanged; virtual;
    function GetDisplayValue: TcxEditValue; virtual;
    function GetIsDataAvailable: Boolean; virtual;
    function GetEditDataBindingInstance: TcxEditDataBinding;
    function GetEditing: Boolean; virtual;
    function GetModified: Boolean; virtual;
    function GetStoredValue: TcxEditValue; virtual;
    function IsInnerControlHaveToBeReadOnly: Boolean; virtual;
    function IsNull: Boolean; virtual;
    procedure Reset; virtual;
    procedure SetInternalDisplayValue(const Value: TcxEditValue);
    procedure SetDisplayValue(const Value: TcxEditValue); virtual;
    function SetEditMode: Boolean; virtual;
    procedure SetStoredValue(const Value: TcxEditValue); virtual;

    property Edit: TcxCustomEdit read FEdit;
  public
    constructor Create(AEdit: TcxCustomEdit); virtual;
    destructor Destroy; override;
    function CanCheckEditorValue: Boolean; virtual;
    function CanModify: Boolean; virtual;
    function CanPostEditorValue: Boolean; virtual;
    function ExecuteAction(Action: TBasicAction): Boolean; virtual;
    class function GetDefaultValuesProviderClass: TcxCustomEditDefaultValuesProviderClass; virtual;
    procedure SetModified; virtual;
    function UpdateAction(Action: TBasicAction): Boolean; virtual;
    procedure UpdateDisplayValue; virtual;
    procedure UpdateNotConnectedDBEditDisplayValue; virtual;
    property DisplayValue: TcxEditValue read GetDisplayValue write SetDisplayValue;
    property Editing: Boolean read GetEditing;
    property IDefaultValuesProvider: IcxEditDefaultValuesProvider read GetIDefaultValuesProvider;
    property IsDataAvailable: Boolean read GetIsDataAvailable;
    property Modified: Boolean read GetModified;
    property StoredValue: TcxEditValue read GetStoredValue write SetStoredValue;
  end;

  TcxEditDataBindingClass = class of TcxEditDataBinding;

  { TcxCustomEdit }

  TcxEditModifiedState = record
    Modified: Boolean;
    ModifiedAfterEnter: Boolean;
  end;

  { TcxCustomEditData }

  TcxCustomEditData = class(TObject)
  private
    FCleared: Boolean;
    FEdit: TcxCustomEdit;
    FFreeNotificator: TcxFreeNotificator;
    procedure FreeNotification(AComponent: TComponent);
  protected
    property Cleared: Boolean read FCleared write FCleared;
  public
    constructor Create(AEdit: TcxCustomEdit); virtual;
    destructor Destroy; override;
    procedure Clear;
  end;

  TcxCustomEditDataClass = class of TcxCustomEditData;

  { TcxEditChangeEventsCatcher }

  TcxEditChangeEventsCatcher = class
  private
    FEdit: TcxCustomEdit;
    FLockCount: Integer;
    FOnChangeEvent: Boolean;
    FOnEditValueChangedEvent: Boolean;
  public
    constructor Create(AEdit: TcxCustomEdit);
    function IsLocked: Boolean;
    procedure Lock(ALock: Boolean; AInvokeChangedOnUnlock: Boolean = True);
    procedure InvokeEditChangedEvents;

    property OnChangeEvent: Boolean read FOnChangeEvent write FOnChangeEvent;
    property OnEditValueChangedEvent: Boolean read FOnEditValueChangedEvent
      write FOnEditValueChangedEvent;
  end;

  TcxCustomEditContainer = class(TcxContainer, IcxTransparentControl, IcxMouseTrackingCaller2, IcxMouseTrackingCaller3)
  private
    procedure CMGestureManagerChanged(var Message: TMessage); message CM_GESTUREMANAGERCHANGED;

    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
  protected
    // IcxMouseTrackingCaller3
    procedure IcxMouseTrackingCaller2.MouseLeave = MouseTrackingCallerMouseLeave;
    procedure IcxMouseTrackingCaller3.MouseLeave = MouseTrackingCallerMouseLeave;
    function IsCaptureMouse: Boolean; virtual;
    function PtInCaller(const P: TPoint): Boolean; virtual;

    //IcxTransparentControl
    function IsTransparentRegionsPresent: Boolean;

    procedure MouseEnter(AControl: TControl); override;
    procedure MouseLeave(AControl: TControl); override;
    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
  published
     property Align;
  end;

  { TcxCustomEdit }

  TcxCustomEdit = class(TcxCustomEditContainer,
    IcxEditRepositoryItemListener,
    IdxSkinSupport,
    IdxSpellCheckerControl,
    IdxFadingObject,
    IcxEditOwner,
    IdxTouchModeSupport)
  private
    FAnchorX: Integer;
    FAnchorY: Integer;
    FAnchorScalingFlags: TScalingFlags;
    FAutoHeight: Boolean;
    FAutoWidth: Boolean;
    FCaptureButtonVisibleIndex: Integer;
    FChangeEventsCatcher: TcxEditChangeEventsCatcher;
    FClickLockCount: Integer;
    FDblClickTimer: TcxTimer;
    FEditData: TcxCustomEditData;
    FEditModeSetting: Boolean;
    FFocused: Boolean;
    FHandleAllocating: Boolean;
    FInnerEdit: IcxCustomInnerEdit;
    FUserActionCount: Integer;
    FUserActionDisabledCount: Integer;
    FModified: Boolean;
    FModifiedAfterEnter: Boolean;
    FNeedFocusOnClick: Boolean;
    FPrevModifiedList: array of TcxEditModifiedState;
    FPropertiesEvents: TNotifyEvent;
    FRepositoryItem: TcxEditRepositoryItem;
    FSupportsTouchMode: Boolean;
    FUpdate: Boolean;
    FWindowProcObject: TcxWindowProcLinkedObject;

    FIsBarControl: Boolean;
    FIsContentParamsInitialized: Boolean;
    FIsKeyPressHandled: Boolean;
    FIsFirstSetSize: Boolean;
    FIsHiding: Boolean;
    FIsInplace: Boolean;
    FIsInplaceInitializing: Boolean;
    FIsJustCreated: Boolean;
    FIsPosting: Boolean;

    FIsEditValidated: Boolean;
    FIsEditValidating: Boolean;
    FLockValidate: Integer;
    FValidateErrorProcessing: Boolean;
    FValidationError: Boolean;
    FValidationErrorText: TCaption;
    FPrevEditValue: TcxEditValue;
    FPrevValidationError: Boolean;
    FEditValueChangingLockCount: Integer;

    FOnAfterKeyDown: TKeyEvent;
    FOnEditing: TcxEditEditingEvent;
    FOnPostEditValue: TNotifyEvent;

    function CheckButtonShortCuts(AKey: Integer): Boolean;
    procedure DblClickTimerHandler(Sender: TObject);
    procedure DoClearEditData(AEditData: TcxCustomEditData);
    function GetActiveProperties: TcxCustomEditProperties;
    function GetAutoSize: Boolean;
    function GetEditActiveStyle: TcxCustomEditStyle;
    function GetHeight: Integer;
    function GetInternalStyle(AState: TcxContainerStateItem): TcxCustomEditStyle;
    function GetStyle: TcxEditStyle;
    function GetStyleDisabled: TcxEditStyle;
    function GetStyleFocused: TcxEditStyle;
    function GetStyleHot: TcxEditStyle;
    function GetViewInfo: TcxCustomEditViewInfo;
    procedure HandleCutMessage;
    procedure HandlePasteMessage;
    procedure InitContentParams;
    procedure ReadAnchorX(Reader: TReader);
    procedure ReadAnchorY(Reader: TReader);
    procedure ReadHeight(Reader: TReader);
    procedure ReadWidth(Reader: TReader);
    procedure SetAutoHeight(AValue: Boolean);
    procedure SetAutoWidth(AValue: Boolean);
    procedure SetDataBinding(Value: TcxEditDataBinding);
    procedure SetHeight(Value: Integer);
    procedure SetInternalStyle(AState: TcxContainerStateItem; Value: TcxCustomEditStyle);
    procedure SetInternalReadOnly(Value: Boolean);
    procedure SetModified(Value: Boolean);
    procedure SetModifiedAfterEnter(Value: Boolean);
    procedure SetModifiedAfterEnterValue(Value: Boolean);
    procedure SetPrevEditValue(Value: TcxEditValue);
    procedure SetProperties(Value: TcxCustomEditProperties);
    procedure SetRepositoryItem(Value: TcxEditRepositoryItem);
    procedure SetReplicatableFlag;
    procedure SetStyle(Value: TcxEditStyle);
    procedure SetStyleDisabled(Value: TcxEditStyle);
    procedure SetStyleFocused(Value: TcxEditStyle);
    procedure SetStyleHot(Value: TcxEditStyle);
    procedure WriteAnchorX(Writer: TWriter);
    procedure WriteAnchorY(Writer: TWriter);
    procedure WriteHeight(Writer: TWriter);
    procedure WriteWidth(Writer: TWriter);

    procedure WMCopy(var Message: TMessage); message WM_COPY;
    procedure WMCut(var Message: TMessage); message WM_CUT;
    procedure CMHintShow(var Message: TCMHintShow); message CM_HINTSHOW;
    procedure WMPaste(var Message: TMessage); message WM_PASTE;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMShowingChanged(var Message: TMessage); message CM_SHOWINGCHANGED;

    procedure DXMUpdateEditvalue(var Message: TMessage); message DXM_UPDATEEDITVALUE;
  protected
    FDataBinding: TcxEditDataBinding;
    FEditValue: TcxEditValue;
    FProperties: TcxCustomEditProperties;

    // TWinControl
    function CanResize(var NewWidth, NewHeight: Integer): Boolean; override;
    procedure ChangeScaleEx(M, D: Integer; IsDPIChanged: Boolean); override;
    procedure Click; override;
    procedure CreateHandle; override;
    procedure DblClick; override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure DoEnter; override;
    procedure DoExit; override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseLeave(AControl: TControl); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Paint; override;
    procedure ReadState(Reader: TReader); override;
    procedure RequestAlign; override;
    procedure SetName(const Value: TComponentName); override;
    procedure SetParent(AParent: TWinControl); override;

    // TcxContainer
    function CanContainerHandleTabs: Boolean; override;
    function CanFocusOnClick: Boolean; override;
    function DoInnerControlDefaultHandler(var Message: TMessage): Boolean; override;
    function DoRefreshContainer(const P: TPoint; Button: TcxMouseButton; Shift: TShiftState; AIsMouseEvent: Boolean): Boolean; override;
    procedure DoSetSize; override;
    procedure FocusChanged; override;
    function GetBorderExtent: TRect; override;
    function GetEditStateColorKind: TcxEditStateColorKind; override;
    function GetStatusHint(const APoint: TPoint): string; override;
    function GetStyleClass: TcxContainerStyleClass; override;
    function GetViewInfoClass: TcxContainerViewInfoClass; override;
    function GetWindowRegionAddon: TRect; override;
    function InternalGetNotPublishedStyleValues: TcxEditStyleValues; override;
    function IsNativeStyle: Boolean; override;
    function IsReadOnly: Boolean; override;
    function IsTransparentBackground: Boolean; override;
    procedure SafeSelectionFocusInnerControl; override;
    procedure TransparentChanged; override;

    procedure AcceleratorClick; virtual;
    procedure AdjustInnerEditPosition; virtual;
    procedure AfterPosting; virtual;
    procedure BeforePosting; virtual;
    function ButtonVisibleIndexAt(const P: TPoint): Integer;
    procedure CalculateViewInfo(AIsMouseEvent: Boolean); reintroduce; overload;
    procedure CalculateViewInfo(P: TPoint; Button: TcxMouseButton;
      Shift: TShiftState; AIsMouseEvent: Boolean); reintroduce; overload; virtual;
    procedure CalculateViewInfo(AViewInfo: TcxCustomEditViewInfo; P: TPoint;
      Button: TcxMouseButton; Shift: TShiftState; AIsMouseEvent: Boolean); reintroduce; overload;
    procedure CalculateAnchors; virtual;
    procedure ChangeHandler(Sender: TObject); virtual;
    procedure CheckAutoSizeChanges(AValue: Boolean);
    procedure CheckHandle;
    function CreateInnerEdit: IcxCustomInnerEdit; virtual;
    function CreateViewData: TcxCustomEditViewData; virtual;
    procedure DefaultButtonClick; virtual;
    procedure DoAfterKeyDown(var Key: Word; Shift: TShiftState);
    procedure DoAutoSizeChanged; virtual;
    procedure DoButtonClick(AButtonVisibleIndex: Integer); virtual;
    procedure DoButtonDown(AButtonVisibleIndex: Integer); virtual;
    procedure DoButtonUp(AButtonVisibleIndex: Integer); virtual;
    procedure DoChange; virtual;
    procedure DoClick;
    procedure DoClosePopup(AReason: TcxEditCloseUpReason);
    procedure DoEditValueChanged; virtual;
    procedure DoEditKeyDown(var Key: Word; Shift: TShiftState); virtual;
    procedure DoEditKeyPress(var Key: Char); virtual;
    procedure DoEditKeyUp(var Key: Word; Shift: TShiftState); virtual;
    procedure DoEditProcessTab(Shift: TShiftState); virtual;
    procedure DoFocusChanged; virtual;
    procedure DoHideEdit(AExit: Boolean); virtual;
    procedure DoOnChange; virtual;
    procedure DoOnEditValueChanged; virtual;
    procedure DoPostEditValue;
    procedure DoProcessEventsOnViewInfoChanging; override;
    procedure DoSetFocusWhenActivate; virtual;
    procedure DoShowEdit; virtual;
    procedure DoValidateOnEnter; virtual;
    procedure EditingChanged; virtual;
    function GetClearValue: TcxEditValue; virtual;
    class function GetDataBindingClass: TcxEditDataBindingClass; reintroduce; virtual;
    function GetDefaultButtonVisibleIndex: Integer;
    function GetDisplayText: string; virtual;
    function GetDisplayValue: TcxEditValue; virtual;
    function GetEditDataClass: TcxCustomEditDataClass; virtual;
    function GetEditingValue: TcxEditValue; virtual;
    function GetEditValue: TcxEditValue; virtual;
    function GetHintText(APart: Integer): string; virtual;
    function GetInnerEditClass: TControlClass; virtual;
    function HandleMouseWheel(Shift: TShiftState): Boolean;
    procedure PopulateSizeProperties(var AEditSizeProperties: TcxEditSizeProperties); virtual;

    // initialize & prepare
    procedure Initialize; virtual;
    procedure InitializeEditData; virtual;
    procedure InitializeInnerEdit; virtual;
    procedure InitializeViewData(AViewData: TcxCustomEditViewData); virtual;
    procedure PrepareEditForInplaceActivation; virtual;

    // internal
    procedure InternalCanResize(var ANewWidth, ANewHeight: Integer);
    function InternalDoEditing: Boolean; virtual;
    function InternalGetActiveProperties: TcxCustomEditProperties;
    function InternalGetEditingValue: TcxEditValue; virtual;
    procedure InternalPostEditValue(AValidateEdit: Boolean = False);
    procedure InternalPostValue;
    procedure InternalSetDisplayValue(const Value: TcxEditValue); virtual;
    procedure InternalSetEditValue(const Value: TcxEditValue; AValidateEditValue: Boolean); virtual;
    procedure InternalStoreEditValue(const Value: TcxEditValue); virtual;

    // conditions
    function CanAutoSize: Boolean; reintroduce; virtual;
    function CanAutoHeight: Boolean; virtual;
    function CanAutoWidth: Boolean; virtual;
    function IsAutoHeight: Boolean;
    function IsAutoWidth: Boolean;
    function IsHeightDependOnWidth: Boolean; virtual;
    function NeedStoreHeight: Boolean;
    function NeedStoreWidth: Boolean;

    function CanKeyDownModifyEdit(Key: Word; Shift: TShiftState): Boolean; virtual;
    function CanKeyPressModifyEdit(Key: Char): Boolean; virtual;
    function CanModify: Boolean; virtual;
    function HasInnerEdit: Boolean;
    function IsActiveControl: Boolean; virtual;
    function IsButtonDC(ADC: THandle): Boolean; virtual;
    function IsClickEnabledDuringLoading: Boolean; virtual;
    function IsDBEdit: Boolean;
    function IsDBEditPaintCopyDrawing: Boolean;
    function IsDefaultButtonKey(AShortCut: TShortCut): Boolean;
    function IsDoubleBufferedNeeded: Boolean; override;
    function IsEditorKey(Key: Word; Shift: TShiftState): Boolean; virtual;
    function IsEditValueStored: Boolean; virtual;
    function IsNativeBackground: Boolean; virtual;
    function IsOnChangeEventAssigned: Boolean;
    function IsOnEditValueChangedEventAssigned: Boolean;
    function IsResetEditClass: Boolean;
    function IsSpecialKey(Key: Word; Shift: TShiftState): Boolean;
    function IsTransparent: Boolean; virtual;
    function IsValidChar(AChar: Char): Boolean; virtual;
    function NeedsInvokeAfterKeyDown(AKey: Word; AShift: TShiftState): Boolean; virtual;
    function UseAnchors: Boolean; virtual;
    function UseAnchorX: Boolean; virtual;
    function UseAnchorY: Boolean; virtual;

    procedure BeginUserAction;
    procedure EndUserAction;
    function IsUserAction: Boolean;
    procedure DisableUserAction;
    procedure EnableUserAction;
    function IsUserActionDisabled: Boolean;

    // validation
    function IsOnValidateEventAssigned: Boolean;
    function CanShowValidationErrorOnPostEditValue: Boolean; virtual;
    procedure DisableValidate;
    procedure DoOnValidate(var ADisplayValue: TcxEditValue; var AErrorText: TCaption; var AError: Boolean);
    procedure DoValidateDisplayValue(var ADisplayValue: TcxEditValue; var AErrorText: TCaption; var AError: Boolean); virtual;
    function InternalGetValueToValidate: TcxEditValue; virtual;
    procedure InternalValidateDisplayValue(const ADisplayValue: TcxEditValue); virtual;
    function InternalValidateEdit(AValidationOptions: TcxEditValidationOptions; AForceValidation: Boolean = False): Boolean; overload;
    function InternalValidateEdit(AForceValidation: Boolean = False): Boolean; overload;
    procedure HandleValidationError(const AErrorText: string; AValidationOptions: TcxEditValidationOptions;
      ACanAbortExecution: Boolean); virtual;
    procedure EnableValidate;

    procedure PaintCopyDraw; virtual;
    procedure PrepareDisplayValue(const AEditValue: TcxEditValue; var DisplayValue: TcxEditValue; AEditFocused: Boolean); virtual;
    procedure PrepareForInplaceActivation; virtual;
    procedure PropertiesChanged(Sender: TObject); virtual;
    function PropertiesChangeLocked: Boolean;
    function RealReadOnly: Boolean; virtual;
    procedure RepositoryItemAssigned; virtual;
    procedure RepositoryItemAssigning; virtual;
    procedure ResetEditValue; virtual;
    procedure ResetErrorState;
    procedure RollBackErrorState;

    procedure RestoreModified;
    procedure SaveModified;
    function SendActivationKey(Key: Char): Boolean; virtual;
    function SetDisplayText(const Value: string): Boolean; virtual;
    procedure SetEditAutoSize(AValue: Boolean); virtual;
    procedure SetEditValue(const Value: TcxEditValue); virtual;
    procedure SetInternalEditValue(const Value: TcxEditValue); virtual;
    procedure SetInternalDisplayValue(Value: TcxEditValue); virtual;
    procedure SynchronizeDisplayValue; virtual;
    procedure SynchronizeEditValue; virtual;
    function TabsNeeded: Boolean; virtual;
    function UpdateContentOnFocusChanging: Boolean; virtual;
    procedure UpdateDrawValue; virtual;
    procedure UpdateInnerEditReadOnly;
    function ValidateKeyDown(var Key: Word; Shift: TShiftState): Boolean; virtual;
    function ValidateKeyPress(var Key: Char): Boolean; virtual;
    function WantNavigationKeys: Boolean; virtual;
    procedure LockedInnerEditWindowProc(var Message: TMessage); virtual;
    procedure LockInnerEditRepainting; virtual;
    procedure UnlockInnerEditRepainting; virtual;

    // IcxEditOwner
    function GetViewData(out AIsViewDataCreated: Boolean): TcxCustomEditViewData;
    procedure IcxEditOwner.Invalidate = InvalidateRect;

    // IcxEditRepositoryItemListener
    procedure IcxEditRepositoryItemListener.ItemRemoved = RepositoryItemListenerItemRemoved;
    procedure IcxEditRepositoryItemListener.PropertiesChanged = RepositoryItemListenerPropertiesChanged;
    procedure RepositoryItemListenerItemRemoved(Sender: TcxEditRepositoryItem);
    procedure RepositoryItemListenerPropertiesChanged(Sender: TcxEditRepositoryItem);

    // IdxSpellCheckerSupport
    procedure IdxSpellCheckerControl.SetValue = SpellCheckerSetValue;
    procedure IdxSpellCheckerControl.SetIsBarControl = SpellCheckerSetIsBarControl;
    procedure IdxSpellCheckerControl.SetSelText = SpellCheckerSetSelText;
    function SupportsSpelling: Boolean; virtual;
    procedure SpellCheckerSetIsBarControl(AValue: Boolean); virtual;
    procedure SpellCheckerSetSelText(const AValue: string; APost: Boolean = False); virtual;
    procedure SpellCheckerSetValue(const AValue: Variant); virtual;

    // IdxTouchModeSupport
    procedure IdxTouchModeSupport.Disable = DisableTouchModeSupport;
    procedure IdxTouchModeSupport.Enable = EnableTouchModeSupport;
    procedure DisableTouchModeSupport;
    procedure EnableTouchModeSupport;

    // IdxFadingObject
    function IdxFadingObject.CanFade = FadingCanFadeBackground;
    procedure IdxFadingObject.DrawFadeImage = FadingInvalidateBackground;
    procedure IdxFadingObject.GetFadingImages = FadingGetBackgroundImages;
    // Fading
    function FadingCanFadeBackground: Boolean; virtual;
    procedure FadingGetBackgroundImages(out AFadeOutImage, AFadeInImage: TcxBitmap); virtual;
    procedure FadingInvalidateBackground;

    property ActiveStyle: TcxCustomEditStyle read GetEditActiveStyle;
    property AnchorX: Integer read FAnchorX;
    property AnchorY: Integer read FAnchorY;
    property AutoHeight: Boolean read FAutoHeight write SetAutoHeight;
    property AutoSize: Boolean read GetAutoSize write SetEditAutoSize default True;
    property AutoWidth: Boolean read FAutoWidth write SetAutoWidth;
    property CaptureButtonVisibleIndex: Integer read FCaptureButtonVisibleIndex write FCaptureButtonVisibleIndex;
    property ChangeEventsCatcher: TcxEditChangeEventsCatcher read FChangeEventsCatcher;
    property DataBinding: TcxEditDataBinding read FDataBinding write SetDataBinding;
    property DisplayText: string read GetDisplayText;
    property DisplayValue: TcxEditValue read GetDisplayValue;
    property EditData: TcxCustomEditData read FEditData;
    property EditModeSetting: Boolean read FEditModeSetting;
    property InnerEdit: IcxCustomInnerEdit read FInnerEdit;
    property IsEditValidated: Boolean read FIsEditValidated write FIsEditValidated;

    property IsInplaceInitializing: Boolean read FIsInplaceInitializing;
    property IsKeyPressHandled: Boolean read FIsKeyPressHandled write FIsKeyPressHandled;
    property NeedFocusOnClick: Boolean read FNeedFocusOnClick write FNeedFocusOnClick;
    property PrevEditValue: TcxEditValue read FPrevEditValue write SetPrevEditValue;
    property Properties: TcxCustomEditProperties read FProperties write SetProperties;
    property SupportsTouchMode: Boolean read FSupportsTouchMode write FSupportsTouchMode;
    property ValidateErrorProcessing: Boolean read FValidateErrorProcessing;
  public
    ContentParams: TcxEditContentParams;
    InplaceParams: TcxInplaceEditParams;

    constructor Create(AOwner: TComponent); overload; override;
    constructor Create(AOwner: TComponent; AIsInplace: Boolean); reintroduce; overload; virtual;
    destructor Destroy; override;

    // ancestor
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    procedure GetTabOrderList(List: TList); override;
    function InnerControlMenuHandler(var Message: TMessage): Boolean; override;
    function IsInplace: Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;

    // clipboard
    procedure CopyToClipboard; virtual;
    procedure CutToClipboard; virtual;
    procedure PasteFromClipboard; virtual;

    procedure Activate(var AEditData: TcxCustomEditData; ANeedSetFocus: Boolean = True); virtual;
    procedure ActivateByKey(Key: Char; var AEditData: TcxCustomEditData); virtual;
    procedure ActivateByMouse(Shift: TShiftState; X, Y: Integer; var AEditData: TcxCustomEditData); virtual;
    function AreChangeEventsLocked: Boolean;
    function CanPostEditValue: Boolean;
    procedure Clear; virtual;
    function Deactivate: Boolean; virtual;
    function DoEditing: Boolean;
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; virtual;
    procedure InitiateAction; override;
    function InternalFocused: Boolean;
    function IsChildWindow(AWnd: THandle): Boolean; virtual;
    function IsEditClass: Boolean; virtual;
    function IsRepositoryItemAcceptable(ARepositoryItem: TcxEditRepositoryItem): Boolean; virtual;
    procedure LockChangeEvents(ALock: Boolean; AInvokeChangedOnUnlock: Boolean = True);
    procedure LockClick(ALock: Boolean);
    procedure LockEditValueChanging(ALock: Boolean);
    procedure PostEditValue;
    procedure PrepareEditValue(const ADisplayValue: TcxEditValue;
      out EditValue: TcxEditValue; AEditFocused: Boolean); virtual;
    procedure Reset;
    procedure SelectAll; virtual;
    procedure SetValidatableEditValue(const AEditValue: TcxEditValue);
    function ValidateEdit(ARaiseExceptionOnError: Boolean): Boolean; overload;
    function ValidateEdit: Boolean; overload;
    function GetTextBaseLine: Integer; virtual;
    function HasTextBaseLine: Boolean; virtual;

    property ActiveProperties: TcxCustomEditProperties read GetActiveProperties;
    property EditingValue: TcxEditValue read GetEditingValue;
    property EditModified: Boolean read FModified write SetModified;
    property EditValue: TcxEditValue read GetEditValue write SetEditValue stored IsEditValueStored;
    property InternalEditValue: TcxEditValue read GetEditValue write SetInternalEditValue stored False;
    property InternalProperties: TcxCustomEditProperties read FProperties;
    property IsEditValidating: Boolean read FIsEditValidating;
    property IsHiding: Boolean read FIsHiding;
    property IsPosting: Boolean read FIsPosting;
    property ModifiedAfterEnter: Boolean read FModifiedAfterEnter write SetModifiedAfterEnter;
    property Style: TcxEditStyle read GetStyle write SetStyle;
    property StyleDisabled: TcxEditStyle read GetStyleDisabled write SetStyleDisabled;
    property StyleFocused: TcxEditStyle read GetStyleFocused write SetStyleFocused;
    property StyleHot: TcxEditStyle read GetStyleHot write SetStyleHot;
    property Styles[AState: TcxContainerStateItem]: TcxCustomEditStyle read GetInternalStyle write SetInternalStyle;
    property TabStop default True;
    property ViewInfo: TcxCustomEditViewInfo read GetViewInfo;

    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;

    property OnAfterKeyDown: TKeyEvent read FOnAfterKeyDown write FOnAfterKeyDown;
    property OnEditing: TcxEditEditingEvent read FOnEditing write FOnEditing;
    property OnPostEditValue: TNotifyEvent read FOnPostEditValue write FOnPostEditValue;
  published
    property Height: Integer read GetHeight write SetHeight stored False;
    property PropertiesEvents: TNotifyEvent read FPropertiesEvents write FPropertiesEvents;
    property RepositoryItem: TcxEditRepositoryItem read FRepositoryItem write SetRepositoryItem;
    property Width stored False;
    property OnFocusChanged;
  end;

  TcxCustomEditClass = class of TcxCustomEdit;

  { TcxEditAlignment }

  TcxEditAlignment = class(TPersistent)
  private
    FDefaultVertAlignment: TcxEditVertAlignment;
    FHorz: TcxEditHorzAlignment;
    FDefaultHorzAlignment: TcxEditHorzAlignment;
    FIsHorzAssigned: Boolean;
    FOwner: TPersistent;
    FVert: TcxEditVertAlignment;
    FOnChanged: TNotifyEvent;
    procedure SetHorz(const Value: TcxEditHorzAlignment);
    procedure SetVert(const Value: TcxEditVertAlignment);
  protected
    procedure DoChanged;
    function GetOwner: TPersistent; override;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  public
    constructor Create(AOwner: TPersistent); overload; virtual;
    constructor Create(AOwner: TPersistent; ADefaultVertAlignment: TcxEditVertAlignment); overload; virtual;
    constructor Create(AOwner: TPersistent; ADefaultHorzAlignment: TcxEditHorzAlignment;
      ADefaultVertAlignment: TcxEditVertAlignment); overload; virtual;
    procedure Assign(Source: TPersistent); override;
    function IsHorzStored: Boolean;
    function IsVertStored: Boolean;
    procedure Reset;
  published
    property Horz: TcxEditHorzAlignment read FHorz write SetHorz stored IsHorzStored;
    property Vert: TcxEditVertAlignment read FVert write SetVert stored IsVertStored;
  end;

  TcxEditListItem = record
    Edit: TcxCustomEdit;
    Properties: TcxCustomEditProperties;
  end;

  { TcxInplaceEditList }

  TcxInplaceEditList = class
  private
    FItems: array of TcxEditListItem;
    FEditorOwner: TComponent;
    function CreateEdit(APropertiesClass: TcxCustomEditPropertiesClass): TcxCustomEdit;
    procedure DestroyItems;
    function FindItem(AProperties: TcxCustomEditProperties;
      ACanUseFreeEditors: Boolean): Integer; overload;
    function FindItem(APropertiesClass: TcxCustomEditPropertiesClass): Integer; overload;
    function GetCount: Integer;
    function GetEdit(AItemIndex: Integer): TcxCustomEdit; overload;
    procedure InitEdit(AEdit: TcxCustomEdit; AProperties: TcxCustomEditProperties);
    procedure RemoveItem(AIndex: Integer); overload;
  protected
    property Count: Integer read GetCount;
    property EditorOwner: TComponent read FEditorOwner;
  public
    constructor Create(AEditorOwner: TComponent); virtual;
    destructor Destroy; override;
    procedure DisconnectProperties(AProperties: TcxCustomEditProperties);
    function FindEdit(AProperties: TcxCustomEditProperties): TcxCustomEdit; overload;
    function FindEdit(APropertiesClass: TcxCustomEditPropertiesClass): TcxCustomEdit; overload;
    function GetEdit(AProperties: TcxCustomEditProperties): TcxCustomEdit; overload;
    function GetEdit(APropertiesClass: TcxCustomEditPropertiesClass): TcxCustomEdit; overload;
    procedure RemoveItem(AProperties: TcxCustomEditProperties); overload;
    procedure RemoveItem(APropertiesClass: TcxCustomEditPropertiesClass); overload;
  end;

  { TcxDefaultEditStyleController }

  TcxDefaultEditStyleController = class(TcxScalableComponent)
  private
    function GetEmulateStandardControlDrawing: Boolean;
    function GetInternalStyle(AState: TcxContainerStateItem): TcxCustomEditStyle;
    function GetOnStyleChanged: TNotifyEvent;
    function GetStyle: TcxEditStyle;
    function GetStyleDisabled: TcxEditStyle;
    function GetStyleFocused: TcxEditStyle;
    function GetStyleHot: TcxEditStyle;
    procedure SetEmulateStandardControlDrawing(Value: Boolean);
    procedure SetInternalStyle(AState: TcxContainerStateItem;
      Value: TcxCustomEditStyle);
    procedure SetOnStyleChanged(Value: TNotifyEvent);
    procedure SetStyle(Value: TcxEditStyle);
    procedure SetStyleDisabled(Value: TcxEditStyle);
    procedure SetStyleFocused(Value: TcxEditStyle);
    procedure SetStyleHot(Value: TcxEditStyle);
  protected
    procedure ChangeScale(M, D: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure RestoreStyles;
    property Styles[AState: TcxContainerStateItem]: TcxCustomEditStyle
      read GetInternalStyle write SetInternalStyle;
  published
    property Scalable;
    property EmulateStandardControlDrawing: Boolean read GetEmulateStandardControlDrawing write SetEmulateStandardControlDrawing default False;
    property Style: TcxEditStyle read GetStyle write SetStyle;
    property StyleDisabled: TcxEditStyle read GetStyleDisabled write SetStyleDisabled;
    property StyleFocused: TcxEditStyle read GetStyleFocused write SetStyleFocused;
    property StyleHot: TcxEditStyle read GetStyleHot write SetStyleHot;
    property OnStyleChanged: TNotifyEvent read GetOnStyleChanged write SetOnStyleChanged;
  end;

  { TcxCustomEditingController }

  TcxCustomEditingController = class(TcxIUnknownObject)
  private
    FEditList: TcxInplaceEditList;
    FEditShowingTimer: TcxTimer;
    FEditUpdateNeeded: Boolean;
    FInitiatingEditing: Boolean;
    FPrevEditOnChange: TNotifyEvent;
    FPrevEditOnEditValueChanged: TNotifyEvent;

    procedure EditShowingTimerHandler(Sender: TObject);
    function GetChangeEventsCatcher: TcxEditChangeEventsCatcher;
    procedure ImmediateEventHandler(ADataController: TcxCustomDataController);
  protected
    FEdit: TcxCustomEdit;
    FEditHiding: Boolean;
    FEditPlaceBounds: TRect;
    FEditPreparing: Boolean;
    FIsErrorOnPost: Boolean;

    procedure CancelEditUpdatePost;
    function CanHideEdit: Boolean; virtual;
    function CanInitEditing: Boolean; virtual; abstract;
    function CanRemoveEditFocus: Boolean; virtual;
    function CanUpdateEditValue: Boolean; virtual;
    procedure CheckEditUpdatePost;
    procedure ClearEditingItem; virtual; abstract;
    procedure ClearErrorState;
    procedure CloseEdit;
    procedure DoEditChanged; virtual;
    procedure DoHideEdit(Accept: Boolean); virtual; abstract;
    procedure DoUpdateEdit; virtual; abstract;
    function GetCancelEditingOnExit: Boolean; virtual; abstract;
    function GetEditParent: TWinControl; virtual; abstract;
    function GetFocusRectBounds: TRect; virtual;
    function GetFocusedCellBounds: TRect; virtual; abstract;
    function GetHideEditOnExit: Boolean; virtual;
    function GetHideEditOnFocusedRecordChange: Boolean; virtual; abstract;
    function GetIsEditing: Boolean; virtual; abstract;
    function HasHiddenEditorOnScrollContent: Boolean; virtual;
    procedure HideEditOnScrollContent; virtual;
    procedure HideInplaceEditor; virtual;
    procedure InitEdit; virtual;
    function IsEditVisible: Boolean;
    function IsNeedInvokeEditChangedEventsBeforePost: Boolean; virtual;
    procedure InvokeEditChangedEvents;
    procedure MultilineEditTextChanged; virtual;
    procedure PostEditUpdate;
    procedure ShowEditAfterScrollContent; virtual;
    procedure StartEditAutoHeight(AHeightChanged: Boolean); virtual;
    procedure StartEditingByTimer; virtual; abstract;
    procedure StartEditShowingTimer;
    procedure UninitEdit; virtual;
    procedure UpdateInplaceParamsPosition; virtual; abstract;
    //editing value
    function GetDataController: TcxCustomDataController; virtual;
    function GetValue: TcxEditValue; virtual; abstract;
    procedure SetImmediatePostEventHandler(ASet: Boolean);
    procedure SetValue(const AValue: TcxEditValue); virtual; abstract;
    procedure UpdateEditValue;
    procedure UpdateValue;

    procedure AssignOverridableEditEvents; virtual;
    procedure AssignStaticEditEvents; virtual;
    procedure UnassignEditEvents; virtual;
    procedure EditAfterKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState); virtual;
    procedure EditChanged(Sender: TObject); virtual;
    procedure EditDblClick(Sender: TObject); virtual;
    procedure EditEditing(Sender: TObject; var CanEdit: Boolean); virtual;
    procedure EditExit(Sender: TObject); virtual;
    procedure EditFocusChanged(Sender: TObject); virtual;
    procedure EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState); virtual;
    procedure EditKeyPress(Sender: TObject; var Key: Char); virtual;
    procedure EditKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState); virtual;
    procedure EditPostEditValue(Sender: TObject); virtual;
    procedure EditValueChanged(Sender: TObject); virtual;

    property ChangeEventsCatcher: TcxEditChangeEventsCatcher read GetChangeEventsCatcher;
    property EditHiding: Boolean read FEditHiding;
    property EditList: TcxInplaceEditList read FEditList;
    property EditShowingTimer: TcxTimer read FEditShowingTimer;
    property EditPreparing: Boolean read FEditPreparing;
    property EditUpdateNeeded: Boolean read FEditUpdateNeeded;
    property HideEditOnExit: Boolean read GetHideEditOnExit;
    property HideEditOnFocusedRecordChange: Boolean read GetHideEditOnFocusedRecordChange;
    property InitiatingEditing: Boolean read FInitiatingEditing;
    property IsErrorOnPost: Boolean read FIsErrorOnPost;
  public
    constructor Create(AEditorOwner: TComponent);
    destructor Destroy; override;

    function IMEComposition(var AMessage: TMessage): Boolean; virtual;
    function IMEStartComposition: Boolean; virtual;

    procedure HideEdit(Accept: Boolean); virtual;
    procedure RemoveEdit(AProperties: TcxCustomEditProperties); virtual;
    procedure ShowEdit; virtual; abstract;

    procedure StopEditShowingTimer;
    procedure UpdateEdit; virtual;

    property Edit: TcxCustomEdit read FEdit;
    property FocusRectBounds: TRect read GetFocusRectBounds;
    property IsEditing: Boolean read GetIsEditing;
  end;

var
  cxEditErrorIcon: TBitmap;
  cxEditWarningIcon: TBitmap;
  cxEditInfoIcon: TBitmap;

const
  cxDefaultEditSizeProperties: TcxEditSizeProperties = (Height: -1; MaxLineCount: 0; Width: -1);

function ButtonToShift(Button: TMouseButton): TShiftState;
function cxButtonToShift(Button: TcxMouseButton): TShiftState;
function cxGetScaledEditProperties(var AInternalProperties: TcxCustomEditProperties;
  AProperties: TcxCustomEditProperties; AScaleFactor: TdxScaleFactor; AOwner: TPersistent = nil): TcxCustomEditProperties;
function cxEditVarEquals(const V1, V2: Variant): Boolean;
function DefaultEditStyleController: TcxEditStyleController;
function EmulateStandardControlDrawing: Boolean;
function GetDefaultEditRepository: TcxEditRepository;
function GetOwnerComponent(APersistent: TPersistent): TComponent;
function GetRegisteredEditProperties: TcxRegisteredClasses;
function GetStandaloneEventSender(AEdit: TcxCustomEdit): TObject;
function InternalVarEqualsExact(const V1, V2: Variant): Boolean;
function IsSpaceChar(C: AnsiChar): Boolean; overload;
function IsSpaceChar(C: WideChar): Boolean; overload;
function NeedAssignRepositoryItem(AItemProperties: TcxCustomEditProperties;
  ARepositoryItem: TcxEditRepositoryItem; var AEditingProperties: TcxCustomEditProperties): Boolean;
procedure SendMouseEvent(AReceiver: TWinControl; AMessage: DWORD; AShift: TShiftState; const APoint: TPoint);
procedure SendKeyDown(AReceiver: TWinControl; Key: Word; Shift: TShiftState);
procedure SendKeyPress(AReceiver: TWinControl; Key: Char);
procedure SendKeyUp(AReceiver: TWinControl; Key: Word; Shift: TShiftState);
procedure SetStandardControlDrawingEmulationMode(AEmulate: Boolean);
procedure UniteRegions(ADestRgn, ASrcRgn: TcxRegion);

implementation

{$R cxEdit.res}
{$R cxScrollCursors.res}

uses
  TypInfo, cxDateUtils, cxEditConsts, cxEditUtils, cxFilterConsts, dxOffice11,
  dxThemeConsts, cxDWMApi, Math, cxLibraryConsts;

const
  EditContentMaxTotalDefaultHorzOffset = 3;

type
  TControlAccess = class(TControl);
  TcxLookAndFeelAccess = class(TcxLookAndFeel);
  TWinControlAccess = class(TWinControl);

var
  FCreatedEditPropertiesList: TList;
  FDefaultEditRepository: TcxEditRepository;
  FDefaultEditStyleController: TcxEditStyleController;
  FDefaultEditStyleControllerCount: Integer;
  FEmulateStandardControlDrawing: Boolean;
  FInplaceEditLists: TList;
  FRegisteredEditProperties: TcxRegisteredClasses;
  FHintWindow: THintWindow;

function CreatedEditPropertiesList: TList;
begin
  if FCreatedEditPropertiesList = nil then
    FCreatedEditPropertiesList := TList.Create;
  Result := FCreatedEditPropertiesList;
end;

function ButtonToShift(Button: TMouseButton): TShiftState;
const
  AButtonMap: array[TMouseButton] of TShiftState = ([ssLeft], [ssRight], [ssMiddle]);
begin
  Result := AButtonMap[Button];
end;

procedure ClearPropertiesDestroyingListeners(AProperties: TcxCustomEditProperties);
var
  I: Integer;
begin
  if FInplaceEditLists <> nil then
    for I := 0 to FInplaceEditLists.Count - 1 do
      TcxInplaceEditList(FInplaceEditLists[I]).DisconnectProperties(AProperties);
end;

function cxButtonToShift(Button: TcxMouseButton): TShiftState;
const
  AButtonMap: array[TcxMouseButton] of TShiftState = ([], [ssLeft], [ssRight], [ssMiddle]);
begin
  Result := AButtonMap[Button];
end;

function cxGetScaledEditProperties(var AInternalProperties: TcxCustomEditProperties;
  AProperties: TcxCustomEditProperties; AScaleFactor: TdxScaleFactor; AOwner: TPersistent = nil): TcxCustomEditProperties;
begin
  Result := AProperties;
  if (Result <> nil) and not AScaleFactor.Equals(Result.ScaleFactor) then
  begin
    if (AInternalProperties = nil) or (AInternalProperties.ClassType <> Result.ClassType) then
    begin
      FreeAndNil(AInternalProperties);
      AInternalProperties := Result.Clone(AOwner);
    end
    else
      AInternalProperties.Assign(Result);

    Result := AInternalProperties;
    Result.ScaleFactor.Assign(AScaleFactor);
  end;
end;

function cxEditVarEquals(const V1, V2: Variant): Boolean;

  function VarTypeEquals: Boolean;
  begin
    Result := VarIsNumericEx(V1) and VarIsNumericEx(V2) or (VarType(V1) = VarType(V2));
    if not Result then
      Result := VarIsStr(V1) and VarIsStr(V2);
  end;

begin
  Result := VarTypeEquals and VarEqualsExact(V1, V2);
end;

procedure DrawComplexFrameEx(ACanvas: TcxCanvas; var ARect: TRect; ALeftTopColor, ARightBottomColor: TColor);
begin
  ACanvas.DrawComplexFrame(ARect, ALeftTopColor, ARightBottomColor);
  InflateRect(ARect, -1, -1);
end;

procedure FrameRectEx(ACanvas: TcxCanvas; var ARect: TRect; AColor: TColor);
begin
  ACanvas.FrameRect(ARect, AColor);
  InflateRect(ARect, -1, -1);
end;

function DefaultEditStyleController: TcxEditStyleController;
begin
  Result := FDefaultEditStyleController;
end;

function EmulateStandardControlDrawing: Boolean;
begin
  Result := FEmulateStandardControlDrawing;
end;

function GetDefaultEditRepository: TcxEditRepository;
begin
  if FDefaultEditRepository = nil then
    FDefaultEditRepository := TcxEditRepository.Create(nil);
  Result := FDefaultEditRepository;
end;

function GetOwnerComponent(APersistent: TPersistent): TComponent;
begin
  while (APersistent <> nil) and not(APersistent is TComponent) do
    APersistent := GetPersistentOwner(APersistent);
  Result := TComponent(APersistent);
end;

function GetRegisteredEditProperties: TcxRegisteredClasses;
begin
  if FRegisteredEditProperties = nil then
  begin
    FRegisteredEditProperties := TcxRegisteredClasses.Create;
    FRegisteredEditProperties.Sorted := True;
  end;
  Result := FRegisteredEditProperties;
end;

function GetStandaloneEventSender(AEdit: TcxCustomEdit): TObject;
begin
  if not AEdit.IsInplace then
    Result := AEdit
  else
    Result := nil;
end;

function InternalVarEqualsExact(const V1, V2: Variant): Boolean;
begin
  Result := (VarType(V1) = VarType(V2)) and VarEqualsExact(V1, V2);
end;

function IsSpaceChar(C: AnsiChar): Boolean; overload;
begin
  Result := (C = ' ') or (C = #0) or (C = #9) or (C = #10) or (C = #12) or (C = #13);
end;

function IsSpaceChar(C: WideChar): Boolean; overload;
begin
  Result := (C = ' ') or (C = #0) or (C = #9) or (C = #10) or (C = #12) or (C = #13);
end;

function IsRegionEmpty(ARgn: TcxRegion): Boolean;
var
  R: TRect;
begin
  Result := GetRgnBox(ARgn.Handle, R) = NULLREGION;
end;

function NeedAssignRepositoryItem(AItemProperties: TcxCustomEditProperties;
  ARepositoryItem: TcxEditRepositoryItem; var AEditingProperties: TcxCustomEditProperties): Boolean;
begin
  Result := (AItemProperties <> nil) and (ARepositoryItem <> nil) and
    (AEditingProperties = ARepositoryItem.Properties) and
    ARepositoryItem.ArePropertiesCompatible(AItemProperties.ClassType);
  if Result then
    AEditingProperties := AItemProperties;
end;

procedure SendMouseEvent(AReceiver: TWinControl; AMessage: DWORD;
  AShift: TShiftState; const APoint: TPoint);
begin
  SendMessage(AReceiver.Handle, AMessage, ShiftStateToKeys(AShift), MakeLParam(APoint.X, APoint.Y));
end;

procedure SendKeyEvent(AReceiver: TWinControl; AMessage: DWORD; AKey: Word; AShift: TShiftState);
begin
  SendMessage(AReceiver.Handle, AMessage, AKey, 0);
end;

procedure SendKeyDown(AReceiver: TWinControl; Key: Word; Shift: TShiftState);
begin
  SendKeyEvent(AReceiver, WM_KEYDOWN, Key, Shift);
end;

procedure SendKeyPress(AReceiver: TWinControl; Key: Char);
begin
  SendKeyEvent(AReceiver, WM_CHAR, Integer(Key), []);
end;

procedure SendKeyUp(AReceiver: TWinControl; Key: Word; Shift: TShiftState);
begin
  SendKeyEvent(AReceiver, WM_KEYUP, Key, Shift);
end;

procedure SetStandardControlDrawingEmulationMode(AEmulate: Boolean);
begin
  if AEmulate <> FEmulateStandardControlDrawing then
  begin
    FEmulateStandardControlDrawing := AEmulate;
    if RootLookAndFeel <> nil then
      TcxLookAndFeelAccess(RootLookAndFeel).NotifyChanged;
  end;
end;

procedure UniteRegions(ADestRgn, ASrcRgn: TcxRegion);
begin
  with ADestRgn do
    CombineRgn(Handle, Handle, ASrcRgn.Handle, RGN_OR);
end;

{ TcxCustomEditData }

constructor TcxCustomEditData.Create(AEdit: TcxCustomEdit);
begin
  inherited Create;
  FEdit := AEdit;
  FFreeNotificator := TcxFreeNotificator.Create(nil);
  FFreeNotificator.OnFreeNotification := FreeNotification;
  FFreeNotificator.AddSender(AEdit);
  Clear;
end;

destructor TcxCustomEditData.Destroy;
begin
  FFreeNotificator.Free;
  inherited Destroy;
end;

procedure TcxCustomEditData.Clear;
begin
  FCleared := True;
  if FEdit <> nil then
    FEdit.DoClearEditData(Self);
end;

procedure TcxCustomEditData.FreeNotification(AComponent: TComponent);
begin
  if AComponent = FEdit then
    FEdit := nil;
end;

{ TcxEditChangeEventsCatcher }

constructor TcxEditChangeEventsCatcher.Create(AEdit: TcxCustomEdit);
begin
  inherited Create;
  FEdit := AEdit;
end;

function TcxEditChangeEventsCatcher.IsLocked: Boolean;
begin
  Result := FLockCount > 0;
end;

procedure TcxEditChangeEventsCatcher.Lock(ALock: Boolean;
  AInvokeChangedOnUnlock: Boolean = True);
begin
  if ALock then
  begin
    if FLockCount = 0 then
    begin
      FOnChangeEvent := False;
      FOnEditValueChangedEvent := False;
    end;
    Inc(FLockCount);
  end
  else
    if FLockCount > 0 then
    begin
      Dec(FLockCount);
      if AInvokeChangedOnUnlock and (FLockCount = 0) then
        InvokeEditChangedEvents;
    end;
end;

procedure TcxEditChangeEventsCatcher.InvokeEditChangedEvents;
begin
  if not (FEdit.IsLoading or FEdit.IsDestroying) then
  begin
    if OnChangeEvent and FEdit.IsOnChangeEventAssigned then
    begin
      FEdit.DoOnChange;
      FOnChangeEvent := False;
    end;
    if OnEditValueChangedEvent and FEdit.IsOnEditValueChangedEventAssigned then
    begin
      FEdit.DoOnEditValueChanged;
      FOnEditValueChangedEvent := False;
    end;
  end;
end;


{ TcxEditDataBinding }

constructor TcxEditDataBinding.Create(AEdit: TcxCustomEdit);
begin
  inherited Create;
  FEdit := AEdit;
  FIDefaultValuesProvider := GetDefaultValuesProviderClass.Create(nil);
end;

destructor TcxEditDataBinding.Destroy;
begin
  FreeAndNil(FIDefaultValuesProvider);
  inherited Destroy;
end;

function TcxEditDataBinding.CanCheckEditorValue: Boolean;
begin
  Result := Edit.IsDesigning or not Edit.ModifiedAfterEnter;
end;

function TcxEditDataBinding.CanModify: Boolean;
begin
  Result := IsDataAvailable;
end;

function TcxEditDataBinding.CanPostEditorValue: Boolean;
begin
  Result := Modified;
end;

function TcxEditDataBinding.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := False;
end;

class function TcxEditDataBinding.GetDefaultValuesProviderClass: TcxCustomEditDefaultValuesProviderClass;
begin
  Result := TcxCustomEditDefaultValuesProvider;
end;

procedure TcxEditDataBinding.SetModified;
begin
  if Edit.Focused then
    Edit.ModifiedAfterEnter := True;
end;

function TcxEditDataBinding.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := False;
end;

procedure TcxEditDataBinding.UpdateDisplayValue;
begin
  Edit.SynchronizeDisplayValue;
end;

procedure TcxEditDataBinding.UpdateNotConnectedDBEditDisplayValue;
begin
end;

procedure TcxEditDataBinding.DefaultValuesChanged;
var
  AProperties: TcxCustomEditProperties;
begin
  AProperties := Edit.ActiveProperties;
  if Edit.RepositoryItem = nil then
    AProperties.Changed
  else
  begin
    AProperties.BeginUpdate;
    try
      AProperties.Changed;
    finally
      AProperties.EndUpdate(False);
    end;
    Edit.PropertiesChanged(AProperties);
  end;
end;

function TcxEditDataBinding.GetDisplayValue: TcxEditValue;
begin
  Result := Edit.DisplayText;
end;

function TcxEditDataBinding.GetEditDataBindingInstance: TcxEditDataBinding;
begin
  Result := Self;
end;

function TcxEditDataBinding.GetEditing: Boolean;
begin
  Result := Edit.ModifiedAfterEnter;
end;

function TcxEditDataBinding.GetModified: Boolean;
begin
  Result := Edit.ModifiedAfterEnter;
end;

function TcxEditDataBinding.GetOwner: TPersistent;
begin
  Result := FEdit;
end;

function TcxEditDataBinding.GetStoredValue: TcxEditValue;
begin
  Result := Edit.EditValue;
end;

function TcxEditDataBinding.IsInnerControlHaveToBeReadOnly: Boolean;
begin
  Result := not IsDataAvailable;
end;

function TcxEditDataBinding.IsNull: Boolean;
begin
  Result := False;
end;

procedure TcxEditDataBinding.Reset;
begin
  Edit.ResetEditValue;
end;

procedure TcxEditDataBinding.SetInternalDisplayValue(const Value: TcxEditValue);
begin
  Edit.SetInternalDisplayValue(Value);
end;

procedure TcxEditDataBinding.SetDisplayValue(const Value: TcxEditValue);
begin
  SetInternalDisplayValue(Value);
end;

function TcxEditDataBinding.SetEditMode: Boolean;
begin
  Edit.ModifiedAfterEnter := True;
  Result := True;
end;

procedure TcxEditDataBinding.SetStoredValue(const Value: TcxEditValue);
begin
end;

function TcxEditDataBinding.GetIDefaultValuesProvider: IcxEditDefaultValuesProvider;
begin
  Result := FIDefaultValuesProvider as IcxEditDefaultValuesProvider;
end;

function TcxEditDataBinding.GetIsDataAvailable: Boolean;
begin
  Result := IDefaultValuesProvider.IsDataAvailable;
end;


{ TcxCustomEditContainer }

function TcxCustomEditContainer.IsCaptureMouse: Boolean;
begin
  Result := HandleAllocated and IsChildEx(Handle, GetCapture);
end;

function TcxCustomEditContainer.PtInCaller(const P: TPoint): Boolean;
begin
  Result := cxRectPtIn(Bounds, P);
end;

function TcxCustomEditContainer.IsTransparentRegionsPresent: Boolean;
begin
  Result := IsTransparentBackground;
end;

procedure TcxCustomEditContainer.MouseEnter(AControl: TControl);
begin
  if AControl = InnerControl then
    AControl := Self;

  inherited;
end;

procedure TcxCustomEditContainer.MouseLeave(AControl: TControl);
begin
  if AControl = InnerControl then
    AControl := Self;

  inherited;
end;

procedure TcxCustomEditContainer.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if not IsMouseTracking then
    MouseEnter(Self);

  inherited;
end;

procedure TcxCustomEditContainer.CMGestureManagerChanged(var Message: TMessage);
begin
  if ([csDestroying, csDesigning] * ComponentState = []) and (InnerControl <> nil) then
    InnerControl.Touch.GestureManager := Touch.GestureManager;
end;

procedure TcxCustomEditContainer.CMMouseLeave(var Message: TMessage);
begin
  if not IsMouseTracking then
    inherited;
end;

{ TcxCustomEdit }

constructor TcxCustomEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csActionClient];
  FAnchorX := MaxInt;
  FAnchorY := MaxInt;
  FIsCreating := True;
  FNeedFocusOnClick := True;
  Initialize;
  FIsCreating := False;
  if HandleAllocated then
    ShortRefreshContainer(False);
end;

constructor TcxCustomEdit.Create(AOwner: TComponent; AIsInplace: Boolean);
begin
  FIsInplace := AIsInplace;
  Create(AOwner);
end;

destructor TcxCustomEdit.Destroy;
begin
  dxFader.Remove(Self);
  if Assigned(TdxSpellCheckerInstance.ISpellChecker) then
    TdxSpellCheckerInstance.ISpellChecker.CheckFinish;
  FreeAndNil(FDblClickTimer);
  if FRepositoryItem <> nil then
    FRepositoryItem.RemoveListener(Self);
  if HasInnerEdit then
    FInnerEdit := nil;

  Properties.OnPropertiesChanged := nil;
  if not FIsInplace then
    Properties.IDefaultValuesProvider := nil;
  FreeAndNil(FDataBinding);

  FPrevModifiedList := nil;
  FreeAndNil(FProperties);
  FreeAndNil(FChangeEventsCatcher);

  inherited Destroy;
end;

function TcxCustomEdit.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or FDataBinding.ExecuteAction(Action);
end;

procedure TcxCustomEdit.GetTabOrderList(List: TList);
begin
  inherited GetTabOrderList(List);
  if IsInplace and Visible then
    List.Remove(Parent);
end;

function TcxCustomEdit.InnerControlMenuHandler(var Message: TMessage): Boolean;
begin
  Result := inherited InnerControlMenuHandler(Message);
  if not Result and (Message.Msg = WM_CONTEXTMENU) then
  begin
    SetFocus;
    with Message do
      Result := Perform(Msg, Handle, LParam);
    Result := Message.Result <> 0;
  end;
end;

function TcxCustomEdit.IsInplace: Boolean;
begin
  Result := FIsInplace;
end;

function TcxCustomEdit.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or FDataBinding.UpdateAction(Action);
end;

procedure TcxCustomEdit.Activate(var AEditData: TcxCustomEditData; ANeedSetFocus: Boolean = True);
begin
  if (AEditData = nil) and (GetEditDataClass <> nil) then
    AEditData := GetEditDataClass.Create(Self);
  FEditData := AEditData;
  DoClearEditData(FEditData);
  Visible := True;
  SynchronizeDisplayValue;
  EditModified := False;
  InplaceParams.MultiRowParent := True;

  if HandleAllocated and CanFocus and ANeedSetFocus then
    DoSetFocusWhenActivate;
end;

procedure TcxCustomEdit.ActivateByKey(Key: Char; var AEditData: TcxCustomEditData);
begin
  LockInnerEditRepainting;
  try
    Activate(AEditData, True);
    if SendActivationKey(Key) then
    begin
      IsKeyPressHandled := False;
      SendKeyPress(Self, Key);
    end;
  finally
    UnlockInnerEditRepainting;
  end;
end;

procedure TcxCustomEdit.ActivateByMouse(Shift: TShiftState; X, Y: Integer; var AEditData: TcxCustomEditData);
var
  P: TPoint;
begin
  Activate(AEditData, FNeedFocusOnClick);
  P := Parent.ClientToScreen(Point(X, Y));
  P := ScreenToClient(P);
  if ssLeft in Shift then
  begin
    SendMouseEvent(Self, WM_MOUSEMOVE, [], P);
    SendMouseEvent(Self, WM_LBUTTONDOWN, Shift, P);
    if (GetCaptureControl = Self) and not(ssLeft in KeyboardStateToShiftState) then
      SetCaptureControl(nil);
  end
  else
    SendMouseEvent(Self, WM_LBUTTONUP, Shift, P);
  FDblClickTimer.Enabled := True;
end;

function TcxCustomEdit.AreChangeEventsLocked: Boolean;
begin
  Result := ChangeEventsCatcher.IsLocked;
end;

function TcxCustomEdit.CanPostEditValue: Boolean;
begin
  Result := not IsDesigning and DataBinding.CanPostEditorValue;
end;

procedure TcxCustomEdit.Clear;
begin
  EditValue := GetClearValue;
end;

procedure TcxCustomEdit.CopyToClipboard;
begin
end;

procedure TcxCustomEdit.CutToClipboard;
begin
end;

function TcxCustomEdit.Deactivate: Boolean;

  procedure ForceConvertingDisplayValueToEditValue;
  begin
    FUserActionCount := 0;
  end;

begin
  FDblClickTimer.Enabled := False;
  Result := False;
  try
    ForceConvertingDisplayValueToEditValue;
    Result := InternalValidateEdit;
  finally
    if Result then
    begin
      SynchronizeDisplayValue;
      IsEditValidated := True;
    end;
  end;
end;

function TcxCustomEdit.DoEditing: Boolean;

  procedure DoOnEditing;
  begin
    if Assigned(FOnEditing) then
      FOnEditing(Self, Result);
  end;

  procedure StandaloneDoEditing;
  begin
    if not DataBinding.Editing then
    begin
      DoOnEditing;
      if Result then
      begin
        LockEditValueChanging(True);
        try
          Result := DataBinding.SetEditMode;
        finally
          LockEditValueChanging(False);
        end;
      end;
    end
    else
    begin
      DoOnEditing;
      if Result then
        DataBinding.SetModified;
    end;
  end;

  procedure InplaceDoEditing;
  begin
    LockEditValueChanging(True);
    try
      DoOnEditing;
    finally
      LockEditValueChanging(False);
    end;
  end;

begin
  Result := InternalDoEditing;
  if not Result then
    Exit;
  if DataBinding.Modified then
  begin
    Result := True;
    Exit;
  end;
  Result := not IsUserAction or CanModify;
  if not Result then
    Exit;

  FEditModeSetting := True;
  try
    if IsInplace then
      InplaceDoEditing
    else
      StandaloneDoEditing;
  finally
    FEditModeSetting := False;
  end;
end;

class function TcxCustomEdit.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxCustomEditProperties;
end;

procedure TcxCustomEdit.PasteFromClipboard;
begin
end;

procedure TcxCustomEdit.PostEditValue;
begin
  if CanPostEditValue then
    InternalPostEditValue(True)
  else
    if Focused then
      InternalPostValue;
end;

procedure TcxCustomEdit.PrepareEditValue(const ADisplayValue: TcxEditValue;
  out EditValue: TcxEditValue; AEditFocused: Boolean);
begin
end;

procedure TcxCustomEdit.Reset;
begin
  DataBinding.Reset;
end;

procedure TcxCustomEdit.SelectAll;
begin
end;

procedure TcxCustomEdit.SetValidatableEditValue(const AEditValue: TcxEditValue);
begin
  BeginUserAction;
  try
    EditValue := AEditValue;
  finally
    EndUserAction;
  end;
end;

function TcxCustomEdit.InternalValidateEdit(AValidationOptions: TcxEditValidationOptions; AForceValidation: Boolean): Boolean;

  function GetValueToValidate: TcxEditValue;
  begin
    if FModifiedAfterEnter or VarIsNull(EditValue) then
      Result := DisplayValue
    else
      Result := InternalGetValueToValidate;
  end;

  function ValueEquals(AValue1, AValue2: Variant): Boolean;
  begin
    Result := (VarType(AValue1) = VarType(AValue2)) and VarEquals(AValue1, AValue2);
  end;

var
  ADisplayValue: TcxEditValue;
  APrevValidationError: Boolean;
begin
  DisableUserAction;
  try
    if not (FModifiedAfterEnter and not FIsEditValidated and (FLockValidate = 0)) and not AForceValidation then
    begin
      Result := not FValidationError;
      Exit;
    end;

    FIsEditValidating := True;
    try
      ADisplayValue := GetValueToValidate;
      FValidationErrorText := '';
      APrevValidationError := FValidationError;
      FValidationError := False;
      ActiveProperties.FInternalValidationOptions := AValidationOptions;
      DoValidateDisplayValue(ADisplayValue, FValidationErrorText, FValidationError);
      AValidationOptions := ActiveProperties.FInternalValidationOptions;
      if ((FValidationError <> APrevValidationError) or FValidationError) and HandleAllocated then
        PostMessage(Handle, DXM_SHORTREFRESHCONTAINER, 0, 0);
      Result := not FValidationError;

      if FValidationError then
        try
          HandleValidationError(FValidationErrorText, AValidationOptions, not AForceValidation);
        finally
          if not VarEquals(ADisplayValue, GetValueToValidate) then
          begin
            SetInternalDisplayValue(ADisplayValue);
            if Visible then
            begin
              SelectAll;
              if not IsEditClass then
                UpdateDrawValue;
            end;
          end;
        end;

      if not FValidationError or (evoAllowLoseFocus in AValidationOptions) then
      begin
        if (FModifiedAfterEnter or not ValueEquals(ADisplayValue, GetValueToValidate)) and
           not (IsUserAction and not DoEditing) then
        begin
          InternalValidateDisplayValue(ADisplayValue);
          if not IsInplace and not Focused then
            ModifiedAfterEnter := False;
          FIsEditValidated := True;
        end;
      end;
    finally
      FIsEditValidating := False;
    end;
  finally
    EnableUserAction;
  end;
end;

function TcxCustomEdit.InternalValidateEdit(AForceValidation: Boolean): Boolean;
begin
  Result := InternalValidateEdit(ActiveProperties.ValidationOptions, AForceValidation);
end;

function TcxCustomEdit.ValidateEdit(ARaiseExceptionOnError: Boolean): Boolean;
begin
  if ARaiseExceptionOnError then
    Result := InternalValidateEdit([evoRaiseException], True)
  else
    Result := InternalValidateEdit([], True);
end;

function TcxCustomEdit.ValidateEdit: Boolean;
begin
  Result := InternalValidateEdit(True);
end;

function TcxCustomEdit.GetTextBaseLine: Integer;
begin
  Result := 0;
end;

function TcxCustomEdit.HasTextBaseLine: Boolean;
begin
  Result := False;
end;

procedure TcxCustomEdit.CalculateAnchors;
begin
  if [csLoading, csReading] * ComponentState <> [] then
    Exit;
  if UseAnchorX then
  begin
    if ActiveProperties.Alignment.Horz = taCenter then
      FAnchorX := Left + Width div 2 + Integer(Width div 2 <> Width / 2)
    else
      FAnchorX := Left + Width;
  end;

  if UseAnchorY then
  begin
    if ActiveProperties.Alignment.Vert = taVCenter then
      FAnchorY := Top + Height div 2 + Integer(Height div 2 <> Height / 2)
    else
      FAnchorY := Top + Height;
  end;
end;

function TcxCustomEdit.CanContainerHandleTabs: Boolean;
begin
  Result := False;
end;

function TcxCustomEdit.FadingCanFadeBackground: Boolean;
begin
  Result := ViewInfo.IsFadingAvailable and not IsInplace;
end;

procedure TcxCustomEdit.FadingGetBackgroundImages(out AFadeOutImage, AFadeInImage: TcxBitmap);

  function GetHitPoint(AHotTracked: Boolean): TPoint;
  begin
    if AHotTracked then
      Result := cxRectCenter(GetControlRect(Self))
    else
      Result := cxInvalidPoint;
  end;

  function PrepareFadingImage(AHotTracked: Boolean): TcxBitmap32;
  var
    AViewInfo: TcxCustomEditViewInfo;
  begin
    AViewInfo := TcxCustomEditViewInfo(GetViewInfoClass.Create);
    try
      AViewInfo.FEdit := Self;
      CalculateViewInfo(AViewInfo, GetHitPoint(AHotTracked), cxmbNone, [], True);
      AViewInfo.BackgroundColor := TdxFadingHelper.CheckColor(AViewInfo.BackgroundColor);
      AViewInfo.BorderColor := TdxFadingHelper.CheckColor(AViewInfo.BorderColor);
      Result := TcxBitmap32.CreateSize(AViewInfo.Bounds, True);
      Result.cxCanvas.WindowOrg := AViewInfo.Bounds.TopLeft;
      DrawCustomEditBackground(Result.cxCanvas, AViewInfo, True);
      Result.cxCanvas.WindowOrg := cxNullPoint;
    finally
      AViewInfo.Free;
    end;
  end;

begin
  AFadeOutImage := PrepareFadingImage(False);
  AFadeInImage := PrepareFadingImage(True);
end;

procedure TcxCustomEdit.FadingInvalidateBackground;
begin
  if HandleAllocated then
    cxInvalidateRect(Handle, False);
end;

function TcxCustomEdit.CanFocusOnClick: Boolean;
begin
  Result := inherited CanFocusOnClick and FNeedFocusOnClick;
end;

function TcxCustomEdit.CanResize(var NewWidth, NewHeight: Integer): Boolean;
begin
  Result := inherited CanResize(NewWidth, NewHeight);
  if Result and not IsLoading then
    InternalCanResize(NewWidth, NewHeight);
end;

procedure TcxCustomEdit.ChangeScaleEx(M, D: Integer; IsDPIChanged: Boolean);
begin
  if (M <> D) and (not (csLoading in ComponentState) or (FAnchorScalingFlags <> [])) then
  begin
    if sfLeft in FAnchorScalingFlags then
      FAnchorX := MulDiv(FAnchorX, M, D);
    if sfTop in FAnchorScalingFlags then
      FAnchorY := MulDiv(FAnchorY, M, D);
  end;
  FAnchorScalingFlags := [];
  inherited ChangeScaleEx(M, D, IsDPIChanged);
end;

procedure TcxCustomEdit.Click;
begin
  if not IsDestroying and (FClickLockCount = 0) and
    (not IsLoading or IsClickEnabledDuringLoading) then
      inherited Click;
end;

procedure TcxCustomEdit.DblClick;
var
  P: TPoint;
  AButton: TMouseButton;
  AShiftState: TShiftState;
begin
  P := GetMouseCursorClientPos;
  if ButtonVisibleIndexAt(P) <> -1 then
  begin
    AButton := mbLeft;
    AShiftState := KeyboardStateToShiftState;
    AShiftState := AShiftState + ButtonToShift(AButton);
    MouseDown(AButton, AShiftState, P.X, P.Y);
    Click;
  end
  else
    inherited DblClick;
end;

procedure TcxCustomEdit.DefineProperties(Filer: TFiler);

  function HasHeight: Boolean;
  begin
    Result := (Filer.Ancestor = nil) or (TcxCustomEdit(Filer.Ancestor).Height <> Height);
  end;

  function HasWidth: Boolean;
  begin
    Result := (Filer.Ancestor = nil) or (TcxCustomEdit(Filer.Ancestor).Width <> Width);
  end;

begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('Height', ReadHeight, WriteHeight, NeedStoreHeight and HasHeight);
  Filer.DefineProperty('Width', ReadWidth, WriteWidth, NeedStoreWidth and HasWidth);
  Filer.DefineProperty('AnchorX', ReadAnchorX, WriteAnchorX, UseAnchorX);
  Filer.DefineProperty('AnchorY', ReadAnchorY, WriteAnchorY, UseAnchorY);
end;

procedure TcxCustomEdit.DoEnter;
begin
  if not IsDestroying then
  begin
    DoShowEdit;
    ShortRefreshContainer(False);
    if Assigned(TdxSpellCheckerInstance.ISpellChecker) then
      TdxSpellCheckerInstance.ISpellChecker.CheckStart(InnerControl);
  end;
end;

procedure TcxCustomEdit.DoExit;
begin
  DisableUserAction;
  try
    if Assigned(TdxSpellCheckerInstance.ISpellChecker) then
      TdxSpellCheckerInstance.ISpellChecker.CheckFinish;
    DoHideEdit(True);
  finally
    EnableUserAction;
  end;
end;

function TcxCustomEdit.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean;
begin
  BeginUserAction;
  try
    Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
  finally
    EndUserAction;
  end;
end;

function TcxCustomEdit.DoInnerControlDefaultHandler(var Message: TMessage): Boolean;
begin
  Result := inherited DoInnerControlDefaultHandler(Message);
  if not Result then
  begin
    case Message.Msg of
      WM_CUT:
        begin
          if not InnerEdit.CanProcessClipboardMessages then
          begin
            HandleCutMessage;
            Result := True;
          end;
        end;
      WM_PASTE:
        begin
          if not InnerEdit.CanProcessClipboardMessages then
          begin
            HandlePasteMessage;
            Result := True;
          end;
        end;
      WM_UNDO:
        begin
          if Focused then
            BeginUserAction;
          try
            DoEditing;
          finally
            if Focused then
              EndUserAction;
          end;
        end;
    end;
  end;
end;

function TcxCustomEdit.DoRefreshContainer(const P: TPoint;
  Button: TcxMouseButton; Shift: TShiftState; AIsMouseEvent: Boolean): Boolean;
var
  AViewInfo: TcxCustomEditViewInfo;
  R: TRect;
  ARepaintDone: Boolean;
begin
  if (FCaptureButtonVisibleIndex <> -1) and (GetCaptureControl <> Self) then
    FCaptureButtonVisibleIndex := -1;
  if AIsMouseEvent then
    AViewInfo := TcxCustomEditViewInfo(Properties.GetViewInfoClass.Create)
  else
    AViewInfo := nil;
  try
    if AViewInfo <> nil then
      AViewInfo.Assign(ViewInfo);
    ViewInfo.StoreLastState;
    CalculateViewInfo(P, Button, Shift, AIsMouseEvent);
    if HasInnerEdit and IsEditClass then
      R := InnerEdit.Control.BoundsRect
    else
      R := cxEmptyRect;

    ARepaintDone := ViewInfo.Repaint(Self, R, AViewInfo);
    if ARepaintDone or (AViewInfo = nil) then
    begin
      SetSize;
      CalculateViewInfo(P, Button, Shift, AIsMouseEvent);
      UpdateWindowRegion;
    end;
  finally
    AViewInfo.Free;
  end;
  Result := True;
end;

procedure TcxCustomEdit.FocusChanged;
var
  AFocused: Boolean;
begin
  if IsDestroying or FValidateErrorProcessing then
    Exit;
  AFocused := Focused and Application.Active;
  if FFocused = AFocused then
    Exit;
  FFocused := not FFocused;
  inherited FocusChanged;
  DoFocusChanged;
end;

function TcxCustomEdit.GetBorderExtent: TRect;
begin
  Result.Left := ViewInfo.ShadowRect.Left - ViewInfo.Bounds.Left;
  Result.Right := ViewInfo.Bounds.Right - ViewInfo.ShadowRect.Right;
  Result.Top := ViewInfo.ShadowRect.Top - ViewInfo.Bounds.Top;
  Result.Bottom := ViewInfo.Bounds.Bottom - ViewInfo.ShadowRect.Bottom;
end;

function TcxCustomEdit.GetEditStateColorKind: TcxEditStateColorKind;
begin
  Result := inherited GetEditStateColorKind;
  if (Result <> esckDisabled) and ActiveProperties.ReadOnly then
    Result := esckReadOnly;
end;

function TcxCustomEdit.GetStyleClass: TcxContainerStyleClass;
begin
  Result := GetPropertiesClass.GetStyleClass;
end;

function TcxCustomEdit.GetViewInfoClass: TcxContainerViewInfoClass;
begin
  Result := TcxCustomEditPropertiesClass(GetPropertiesClass).GetViewInfoClass;
end;

function TcxCustomEdit.GetWindowRegionAddon: TRect;
begin
  Result := ViewInfo.FErrorBounds;
end;

function TcxCustomEdit.InternalGetNotPublishedStyleValues:
  TcxEditStyleValues;
begin
  Result := [svButtonStyle, svButtonTransparency, svGradient, svGradientButtons,
    svGradientDirection, svPopupBorderStyle];
end;

function TcxCustomEdit.IsReadOnly: Boolean;
begin
  Result := ActiveProperties.ReadOnly;
end;

function TcxCustomEdit.IsTransparentBackground: Boolean;
begin
  Result := IsNativeBackground or IsTransparent;
end;

procedure TcxCustomEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  BeginUserAction;
  try
    IsKeyPressHandled := False;
    if not IsEditorKey(Key, Shift) then
    begin
      DisableUserAction;
      try
        inherited KeyDown(Key, Shift);
      finally
        EnableUserAction;
      end;
    end;
    if Key <> 0 then
      DoEditKeyDown(Key, Shift);
    if (Key <> 0) and NeedsInvokeAfterKeyDown(Key, Shift) then
      DoAfterKeyDown(Key, Shift);
    if (Key <> 0) and Assigned(TdxSpellCheckerInstance.ISpellChecker2) then
      TdxSpellCheckerInstance.ISpellChecker2.KeyDown(Key, Shift);
  finally
    EndUserAction;
  end;
end;

procedure TcxCustomEdit.KeyPress(var Key: Char);
begin
  if IsKeyPressHandled then
  begin
    IsKeyPressHandled := False;
    Key := #0;
    Exit;
  end;

  inherited KeyPress(Key);
  if Key <> #0 then
  begin
    BeginUserAction;
    try
      DoEditKeyPress(Key);
    finally
      EndUserAction;
    end;
  end;
  if (Key <> #0) and Assigned(TdxSpellCheckerInstance.ISpellChecker) then
    TdxSpellCheckerInstance.ISpellChecker.KeyPress(Key);
end;

procedure TcxCustomEdit.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited KeyUp(Key, Shift);
  if Key <> 0 then
    DoEditKeyUp(Key, Shift);
  if (Key <> 0) and Assigned(TdxSpellCheckerInstance.ISpellChecker2) then
    TdxSpellCheckerInstance.ISpellChecker2.KeyUp(Key, Shift);
end;

procedure TcxCustomEdit.Loaded;
begin
  FIsFirstSetSize := True;
  inherited Loaded;
  LockChangeEvents(True);
  LockClick(True);
  try
    DataBinding.UpdateNotConnectedDBEditDisplayValue;
    ShortRefreshContainer(False);
    if FRepositoryItem = nil then
      Properties.OnPropertiesChanged := PropertiesChanged;
    PropertiesChanged(ActiveProperties);
    ViewInfo.Shadow := False;
    ContainerStyleChanged(FStyles.Style); // TODO remove
  finally
    LockClick(False);
    LockChangeEvents(False, False);
  end;
end;

procedure TcxCustomEdit.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
const
  AMouseMessages: array[TMouseButton] of UINT =
    (WM_LBUTTONDOWN, WM_RBUTTONDOWN, WM_MBUTTONDOWN);
var
  AButtonVisibleIndex: Integer;
  AControl: TWinControl;
  P: TPoint;
begin
  if (Button = mbLeft) and FDblClickTimer.Enabled then
  begin
    FDblClickTimer.Enabled := False;
    if ButtonVisibleIndexAt(GetMouseCursorClientPos) = -1 then
      DblClick;
  end;

  inherited MouseDown(Button, Shift, X, Y);
  P := GetMouseCursorPos;
  if HandleAllocated and (WindowFromPoint(P) = Handle) then
  begin
    AControl := FindControl(WindowFromPoint(P));
    if (AControl <> nil) and (AControl <> Self) then
    begin
      P := AControl.ScreenToClient(P);
      CallWindowProc(TWinControlAccess(AControl).DefWndProc, AControl.Handle,
        AMouseMessages[Button], ShiftStateToKeys(KeyboardStateToShiftState),
        MakeLong(P.X, P.Y));
    end;
  end;

  if (Button = mbLeft) and (GetCaptureControl = Self) then
  begin
    AButtonVisibleIndex := ButtonVisibleIndexAt(Point(X, Y));
    if (AButtonVisibleIndex <> -1) and (ViewInfo.ButtonsInfo[AButtonVisibleIndex].Data.State = ebsPressed) then
      FCaptureButtonVisibleIndex := AButtonVisibleIndex;
  end;
end;

procedure TcxCustomEdit.MouseLeave(AControl: TControl);
//var
//  I: Integer;
begin
  inherited MouseLeave(AControl);
//  for I := 0 to Length(ViewInfo.ButtonsInfo) - 1 do
//    with ViewInfo.ButtonsInfo[I] do
//      if Data.State = ebsSelected then
//      begin
//        Data.State := ebsNormal;
//        InvalidateRect(Bounds, HasBackground);
//      end;
end;

procedure TcxCustomEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FRepositoryItem) then
    RepositoryItem := nil;
end;

procedure TcxCustomEdit.Paint;
begin
  if IsDBEditPaintCopyDrawing then
    PaintCopyDraw
  else
  begin
    CheckIsViewInfoCalculated;
    if IsViewInfoCalculated then
      ViewInfo.Paint(Canvas);
  end;
  UpdateInternalControlsState;
end;

procedure TcxCustomEdit.ReadState(Reader: TReader);
begin
  Properties.OnPropertiesChanged := nil;
  inherited ReadState(Reader);
end;

procedure TcxCustomEdit.RequestAlign;
begin
  inherited RequestAlign;
  ShortRefreshContainer(False);
end;

procedure TcxCustomEdit.SetName(const Value: TComponentName);
begin
  inherited SetName(Value);
  DataBinding.UpdateNotConnectedDBEditDisplayValue;
end;

procedure TcxCustomEdit.SetParent(AParent: TWinControl);
begin
  if FIsJustCreated and (AParent <> nil) then
  begin
    FIsJustCreated := False;
    if not IsInplaceInitializing then
    begin
      DataBinding.UpdateNotConnectedDBEditDisplayValue;
      PropertiesChanged(Properties);
      DataBinding.UpdateDisplayValue;
    end;
  end;
  inherited SetParent(AParent);
end;

procedure TcxCustomEdit.DoSetSize;

  procedure CalculateLeftTop(var ALeft, ATop: Integer; ANewWidth, ANewHeight: Integer);
  begin
    ALeft := Left;
    ATop := Top;
    if not IsScaleChanging and UseAnchors and IsAutoWidth and not IsHeightDependOnWidth then
    begin
      if not FIsFirstSetSize then
        CalculateAnchors;
      FIsFirstSetSize := False;
      if UseAnchorX and (AnchorX <> MaxInt) then
      begin
        if ActiveProperties.Alignment.Horz = taCenter then
          ALeft := AnchorX - ANewWidth div 2 - Integer((ANewWidth div 2) <> (ANewWidth / 2))
        else
          ALeft := AnchorX - ANewWidth;
      end;
      if UseAnchorY and (AnchorY <> MaxInt) then
      begin
        if ActiveProperties.Alignment.Vert = taVCenter then
          ATop := AnchorY - ANewHeight div 2 - Integer((ANewHeight div 2) <> (ANewHeight / 2))
        else
          ATop := AnchorY - ANewHeight;
      end;
    end;
  end;

var
  ANewHeight, ANewWidth: Integer;
  ALeft, ATop: Integer;
begin
  if HandleAllocated and not IsDestroying then
  begin
    ANewWidth := Width;
    ANewHeight := Height;
    InternalCanResize(ANewWidth, ANewHeight);
    CalculateLeftTop(ALeft, ATop, ANewWidth, ANewHeight);
    SetBounds(ALeft, ATop, ANewWidth, ANewHeight);
    AdjustInnerEditPosition;
  end;
end;

procedure TcxCustomEdit.CreateHandle;
begin
  inherited CreateHandle;
  if HasInnerEdit and inherited Focused then
    InnerEdit.SafelySetFocus;
end;

function TcxCustomEdit.IsNativeStyle: Boolean;
begin
  Result := Properties.GetViewDataClass.IsNativeStyle(Style.LookAndFeel);
end;

procedure TcxCustomEdit.SafeSelectionFocusInnerControl;
begin
  InnerEdit.SafelySetFocus;
end;

procedure TcxCustomEdit.TransparentChanged;
begin
  if Transparent then
    ControlStyle := ControlStyle - [csOpaque] + [csParentBackground]
  else
    ControlStyle := ControlStyle + [csOpaque] - [csParentBackground];
  ShortRefreshContainer(False);
end;

function TcxCustomEdit.GetStatusHint(const APoint: TPoint): string;
var
  APart: Integer;
begin
  APart := ViewInfo.GetPart(APoint);
  if APart < 0 then
    Result := Hint
  else
    Result := ViewInfo.GetHintText(APart);
end;

procedure TcxCustomEdit.AcceleratorClick;
begin
//do nothing
end;

procedure TcxCustomEdit.AdjustInnerEditPosition;
var
  R: TRect;
begin
  if not HasInnerEdit then
    Exit;
  R := ViewInfo.InnerEditRect;
  InnerEdit.Control.SetBounds(R.Left, R.Top, cxRectWidth(R), cxRectHeight(R));
  AlignControls(InnerEdit.Control, R);
end;

procedure TcxCustomEdit.AfterPosting;
begin
  FIsPosting := False;
end;

procedure TcxCustomEdit.BeforePosting;
begin
  FIsPosting := True;
end;

function TcxCustomEdit.ButtonVisibleIndexAt(const P: TPoint): Integer;
var
  I: Integer;
begin
  Result := -1;
  with ViewInfo do
    for I := 0 to Length(ButtonsInfo) - 1 do
      if PtInRect(ButtonsInfo[I].Bounds, P) then
      begin
        Result := I;
        Break;
      end;
end;

procedure TcxCustomEdit.CalculateViewInfo(AIsMouseEvent: Boolean);
begin
  CalculateViewInfo(GetMouseCursorClientPos, cxmbNone, KeyboardStateToShiftState, False);
end;

procedure TcxCustomEdit.CalculateViewInfo(P: TPoint;
  Button: TcxMouseButton; Shift: TShiftState; AIsMouseEvent: Boolean);
var
  APrevContainerState: TcxContainerState;
begin
  APrevContainerState := ViewInfo.ContainerState;
  CalculateViewInfo(ViewInfo, P, Button, Shift, AIsMouseEvent);
  if (csHotTrack in ViewInfo.ContainerState) <> (csHotTrack in APrevContainerState) then
  begin
    if csHotTrack in ViewInfo.ContainerState then
      dxFader.FadeIn(Self)
    else
      dxFader.FadeOut(Self);
  end;
end;

procedure TcxCustomEdit.CalculateViewInfo(AViewInfo: TcxCustomEditViewInfo;
  P: TPoint; Button: TcxMouseButton; Shift: TShiftState; AIsMouseEvent: Boolean);
var
  AViewData: TcxCustomEditViewData;
  R: TRect;
begin
  AViewData := TcxCustomEditViewData(CreateViewData);
  try
    R := GetControlRect(Self);
    AViewData.Calculate(Canvas, R, P, Button, Shift, AViewInfo, AIsMouseEvent);
    if UseRightToLeftAlignment then
      AViewData.DoRightToLeftConversion(AViewInfo, R);
  finally
    AViewData.Free;
  end;
end;

function TcxCustomEdit.CanAutoSize: Boolean;
begin
  Result := not IsInplace and AutoSize;
end;

function TcxCustomEdit.CanAutoHeight: Boolean;
begin
  Result := True;
end;

function TcxCustomEdit.CanAutoWidth: Boolean;
begin
  Result := ActiveProperties.ButtonsViewStyle = bvsButtonsAutoWidth;
end;

function TcxCustomEdit.IsAutoHeight: Boolean;
begin
  Result := CanAutoSize and CanAutoHeight and AutoHeight and
    not ((akTop in Anchors) and (akBottom in Anchors));
end;

function TcxCustomEdit.IsAutoWidth: Boolean;
begin
  Result := CanAutoSize and CanAutoWidth and AutoWidth and
    not ((akLeft in Anchors) and (akRight in Anchors));
end;

function TcxCustomEdit.IsHeightDependOnWidth: Boolean;
begin
  Result := False;
end;

function TcxCustomEdit.NeedStoreHeight: Boolean;
begin
  Result := not CanAutoSize;
end;

function TcxCustomEdit.NeedStoreWidth: Boolean;
begin
  Result := not (CanAutoSize and CanAutoWidth and not IsHeightDependOnWidth);
end;

function TcxCustomEdit.CanShowValidationErrorOnPostEditValue: Boolean;
begin
  Result := not IsInplace;
end;

function TcxCustomEdit.CanKeyDownModifyEdit(Key: Word; Shift: TShiftState): Boolean;
begin
  Result := False;
end;

function TcxCustomEdit.CanKeyPressModifyEdit(Key: Char): Boolean;
begin
  Result := False;
end;

function TcxCustomEdit.CanModify: Boolean;
begin
  if IsInplace then
    Result := ActiveProperties.CanModify
  else
    with ActiveProperties do
    begin
      Result := AssignedValues.ReadOnly and not FReadOnly;
      if not Result then
      begin
        Result := not AssignedValues.ReadOnly or not FReadOnly;
        Result := Result and DataBinding.IDefaultValuesProvider.DefaultCanModify and
          DataBinding.IDefaultValuesProvider.CanSetEditMode;
      end;
    end;
end;

procedure TcxCustomEdit.ChangeHandler(Sender: TObject);
begin
  if Focused then
    ModifiedAfterEnter := True
  else
    if not ActiveProperties.IsChanging then
      DataBinding.SetModified;
  DoChange;
end;

procedure TcxCustomEdit.CheckAutoSizeChanges(AValue: Boolean);
begin
  DoAutoSizeChanged;
  if AValue then
  begin
    BeginRefreshContainer;
    try
      CheckHandle;
      SetSize;
    finally
      EndRefreshContainer;
    end;
  end;
end;

procedure TcxCustomEdit.CheckHandle;
begin
  if not (FHandleAllocating or HandleAllocated or IsDestroying) and CanAllocateHandle(Self) then
  begin
    FHandleAllocating := True;
    try
      HandleNeeded;
    finally
      FHandleAllocating := False;
    end;
  end;
end;

function TcxCustomEdit.CreateInnerEdit: IcxCustomInnerEdit;
var
  AIInnerEditHelper: IcxInnerEditHelper;
  AInnerEdit: TControl;
  AInnerEditClass: TControlClass;
begin
  AInnerEditClass := GetInnerEditClass;
  if AInnerEditClass <> nil then
  begin
    AInnerEdit := AInnerEditClass.Create(Self);
  {$IFDEF DELPHI16}
    AInnerEdit.ControlStyle := AInnerEdit.ControlStyle + [csOverrideStylePaint];
  {$ENDIF}
    if Supports(AInnerEdit, IcxInnerEditHelper, AIInnerEditHelper) then
      Result := AIInnerEditHelper.GetHelper
    else
      Supports(AInnerEdit, IcxCustomInnerEdit, Result)
  end
  else
    Result := nil;
end;

function TcxCustomEdit.CreateViewData: TcxCustomEditViewData;
begin
  Result := ActiveProperties.CreateViewData(ActiveStyle, IsInplace);
  Result.ScaleFactor.Assign(ScaleFactor);
  Result.SupportsTouchMode := FSupportsTouchMode;
  if FIsContentParamsInitialized and IsInplace then
    Result.EditContentParams := ContentParams;
  if PropertiesChangeLocked then
    Result.Edit := nil
  else
    Result.Edit := Self;
  if HandleAllocated then
    Result.WindowHandle := Handle
  else
    Result.WindowHandle := 0;
  InitializeViewData(Result);
end;

procedure TcxCustomEdit.DefaultButtonClick;
var
  AIndex: Integer;
begin
  AIndex := GetDefaultButtonVisibleIndex;
  if AIndex <> -1 then
    DoButtonClick(AIndex);
end;

procedure TcxCustomEdit.DisableValidate;
begin
  Inc(FLockValidate);
end;

procedure TcxCustomEdit.DoAfterKeyDown(var Key: Word; Shift: TShiftState);
begin
  DisableUserAction;
  try
    if Assigned(FOnAfterKeyDown) then
      FOnAfterKeyDown(Self, Key, Shift);
  finally
    EnableUserAction;
  end;
end;

procedure TcxCustomEdit.DoAutoSizeChanged;
begin
end;

procedure TcxCustomEdit.DoButtonClick(AButtonVisibleIndex: Integer);
var
  AEditButtonViewInfo: TcxEditButtonViewInfo;
begin
  DisableUserAction;
  try
    AEditButtonViewInfo := ViewInfo.ButtonsInfo[AButtonVisibleIndex];
    if AEditButtonViewInfo.Data.ActionLink <> nil then
      AEditButtonViewInfo.Data.ActionLink.Execute(Self);

    if Assigned(Properties.FOnButtonClick) then
      Properties.FOnButtonClick(Self, AEditButtonViewInfo.ButtonIndex);
    if (RepositoryItem <> nil) and Assigned(ActiveProperties.FOnButtonClick) then
      ActiveProperties.FOnButtonClick(Self, AEditButtonViewInfo.ButtonIndex);
  finally
    EnableUserAction;
  end;
end;

procedure TcxCustomEdit.DoButtonDown(AButtonVisibleIndex: Integer);
begin
end;

procedure TcxCustomEdit.DoButtonUp(AButtonVisibleIndex: Integer);
begin
end;

procedure TcxCustomEdit.DoChange;
begin
  if IsLoading or IsDestroying then
    Exit;
  if IsOnChangeEventAssigned then
    if AreChangeEventsLocked then
      FChangeEventsCatcher.OnChangeEvent := True
    else
    begin
      if IsInplace then
      begin
        SaveModified;
        FModified := True;
        SetModifiedAfterEnterValue(True);
      end;
      try
        DoOnChange;
      finally
        if IsInplace then
          RestoreModified;
      end;
    end;
  if not ActiveProperties.HasDisplayValue and not ActiveProperties.CanCompareEditValue then
    DoEditValueChanged;
end;

procedure TcxCustomEdit.DoClick;
begin
  ModifiedAfterEnter := True;
  if FClickLockCount = 0 then
    Click;
end;

procedure TcxCustomEdit.DoClosePopup(AReason: TcxEditCloseUpReason);
begin
  with Properties do
    if Assigned(OnClosePopup) then
      OnClosePopup(Self, AReason);
  if RepositoryItem <> nil then
    with ActiveProperties do
      if Assigned(OnClosePopup) then
        OnClosePopup(Self, AReason);
end;

procedure TcxCustomEdit.DoEditValueChanged;
begin
  if IsLoading or IsDestroying then
    Exit;
  if IsOnEditValueChangedEventAssigned then
    if AreChangeEventsLocked then
      FChangeEventsCatcher.OnEditValueChangedEvent := True
    else
    begin
      if IsInplace then
      begin
        SaveModified;
        FModified := True;
        SetModifiedAfterEnterValue(True);
      end;
      try
        DoOnEditValueChanged;
      finally
        if IsInplace then
          RestoreModified;
      end;
    end;
end;

procedure TcxCustomEdit.DoEditKeyDown(var Key: Word; Shift: TShiftState);

  procedure CheckClearKey(AShortCut: TShortCut);
  begin
    if AShortCut = ActiveProperties.ClearKey then
    begin
      LockChangeEvents(True);
      BeginUserAction;
      try
        InternalEditValue := GetClearValue;
        if ActiveProperties.ImmediatePost and CanPostEditValue then
          InternalPostEditValue;
        IsEditValidated := True;
      finally
        EndUserAction;
        LockChangeEvents(False);
      end;
      Key := 0;
    end;
  end;

  procedure CheckClickKey(AShortCut: TShortCut);
  begin
    if IsDefaultButtonKey(AShortCut) then
    begin
      dxMessagesController.KillMessages(Handle, WM_CHAR, WM_CHAR, False);
      DefaultButtonClick;
      Key := 0;
    end;
  end;

var
  AShortCut: TShortCut;
begin
  if Key = 0 then
    Exit;
  AShortCut := ShortCut(Key, Shift);

  CheckClearKey(AShortCut);
  if Key = 0 then
    Exit;

  CheckClickKey(AShortCut);
  if Key = 0 then
    Exit;

  if not ValidateKeyDown(Key, Shift) then
  begin
    DoAfterKeyDown(Key, Shift);
    Key := 0;
    Exit;
  end;

  DisableUserAction;
  try
    case Key of
      VK_ESCAPE:
        begin
          if FModifiedAfterEnter and IsResetEditClass then
          begin
            LockChangeEvents(True);
            try
              DataBinding.Reset;
              EditModified := True;
            finally
              LockChangeEvents(False);
            end;
            IsKeyPressHandled := True;
            Key := 0;
          end;
        end;
      VK_TAB:
        if Focused and (Shift * [ssAlt, ssCtrl] = []) and not ActiveProperties.InnerEditNeedsTabs then
        begin
          EnableUserAction;
          try
            DoEditProcessTab(Shift);
          finally
            DisableUserAction;
          end;
          DoAfterKeyDown(Key, Shift);
          if Key = 0 then
            Exit;
          Key := 0;
          if GetParentForm(Self) <> nil then
            TWinControlAccess(GetParentForm(Self)).SelectNext(GetParentForm(Self).ActiveControl,
              not(ssShift in Shift), True);
          if HandleAllocated and HasInnerEdit and (GetFocus = Handle) then
            InnerEdit.SafelySetFocus;
        end;
      VK_RETURN:
        if ActiveProperties.ValidateOnEnter then
        begin
          Key := 0;
          DoValidateOnEnter;
        end;
    end;

    if not WantNavigationKeys then
      case Key of
        VK_LEFT, VK_RIGHT, VK_UP, VK_DOWN, VK_HOME, VK_END, VK_PRIOR, VK_NEXT:
          begin
            DoAfterKeyDown(Key, Shift);
            Key := 0;
          end;
      end;
  finally
    EnableUserAction;
  end;
end;

procedure TcxCustomEdit.DoEditKeyPress(var Key: Char);
begin
  ValidateKeyPress(Key);
end;

procedure TcxCustomEdit.DoEditKeyUp(var Key: Word; Shift: TShiftState);
begin
end;

procedure TcxCustomEdit.DoEditProcessTab(Shift: TShiftState);
begin
end;

procedure TcxCustomEdit.DoFocusChanged;

  function NeedValidate: Boolean;
  var
    AParentForm: TCustomForm;
  begin
    AParentForm := GetParentForm(Self);
    Result := (AParentForm <> nil) and not AParentForm.Visible and
      (fsModal in AParentForm.FormState) and not (AParentForm.ModalResult in [mrNone, mrCancel]);
  end;

begin
  UpdateDrawValue;
  if NeedValidate then
    InternalValidateEdit;
end;

procedure TcxCustomEdit.DoHideEdit(AExit: Boolean);
begin
  if IsDestroying or FValidateErrorProcessing then
    Exit;
  SaveModified;
  FIsHiding := True;
  try
    LockChangeEvents(True);
    try
      InternalValidateEdit;

      if not IsInplace and CanPostEditValue then
        InternalPostEditValue;
    finally
      LockChangeEvents(False);
    end;
    if UpdateContentOnFocusChanging then
      DataBinding.UpdateDisplayValue;
    UpdateDrawValue;
    ShortRefreshContainer(False);
    RestoreModified;
    try
      if AExit then
        inherited DoExit;
    finally
      ModifiedAfterEnter := False;
      SaveModified;
    end;
  finally
    FIsHiding := False;
    RestoreModified;
  end;
end;

procedure TcxCustomEdit.DoOnValidate(var ADisplayValue: TcxEditValue;
  var AErrorText: TCaption; var AError: Boolean);
begin
  with Properties do
    if Assigned(OnValidate) then
      OnValidate(Self, ADisplayValue, AErrorText, AError);
  if RepositoryItem <> nil then
    with ActiveProperties do
      if Assigned(OnValidate) then
        OnValidate(Self, ADisplayValue, AErrorText, AError);
end;

procedure TcxCustomEdit.DoValidateDisplayValue(
  var ADisplayValue: TcxEditValue; var AErrorText: TCaption; var AError: Boolean);
begin
  if ActiveProperties.CanValidate then
    ActiveProperties.ValidateDisplayValue(ADisplayValue, AErrorText, AError, Self);
end;

function TcxCustomEdit.InternalGetValueToValidate: TcxEditValue;
begin
  Result := VarToStr(EditValue);
end;

procedure TcxCustomEdit.DoOnChange;
begin
  DisableUserAction;
  try
    dxCallNotify(Properties.OnChange, Self);
    if RepositoryItem <> nil then
      dxCallNotify(ActiveProperties.OnChange, Self);
  finally
    EnableUserAction;
  end;
end;

procedure TcxCustomEdit.DoOnEditValueChanged;
begin
  DisableUserAction;
  try
    dxCallNotify(Properties.OnEditValueChanged, Self);
    if RepositoryItem <> nil then
      dxCallNotify(ActiveProperties.OnEditValueChanged, Self);
  finally
    EnableUserAction;
  end;
end;

procedure TcxCustomEdit.DoPostEditValue;
begin
  DisableUserAction;
  try
    dxCallNotify(FOnPostEditValue, Self);
  finally
    EnableUserAction;
  end;
end;

procedure TcxCustomEdit.DoProcessEventsOnViewInfoChanging;
begin
  if ViewInfo.GetHasButtonsStateChanges then
  begin
    if ViewInfo.LastPressedButton <> -1 then
    begin
      if ViewInfo.LastPressedButton = ViewInfo.SelectedButton then
      begin
        DoButtonUp(ViewInfo.LastPressedButton);
        DoButtonClick(ViewInfo.LastPressedButton);
      end
      else
        DoButtonUp(ViewInfo.LastPressedButton);
    end;

    if (ViewInfo.PressedButton <> -1) and (ViewInfo.LastPressedButton = -1) then
    begin
      ViewInfo.ButtonsInfo[ViewInfo.PressedButton].FadingHelper.StopFading;
      DoButtonDown(ViewInfo.PressedButton);
    end;
  end;
end;

procedure TcxCustomEdit.DoSetFocusWhenActivate;
begin
  SetFocus;
  if ActiveProperties.AutoSelect then
    SelectAll;
end;

procedure TcxCustomEdit.DoShowEdit;
begin
  SaveModified;
  try
    if not FValidateErrorProcessing then
    begin
      inherited DoEnter;
      if UpdateContentOnFocusChanging then
        DataBinding.UpdateDisplayValue;
    end;
    if ActiveProperties.IsResetEditClass then
      PrevEditValue := EditValue;
    if UpdateContentOnFocusChanging then
      SynchronizeDisplayValue;
  finally
    RestoreModified;
    SetModifiedAfterEnterValue(False);
  end;
end;

procedure TcxCustomEdit.DoValidateOnEnter;
begin
  LockChangeEvents(True);
  try
    if InternalValidateEdit and CanPostEditValue then
      InternalPostEditValue;
  finally
    LockChangeEvents(False);
  end;
end;

procedure TcxCustomEdit.EditingChanged;
begin
  UpdateInnerEditReadOnly;
end;

procedure TcxCustomEdit.EnableValidate;
begin
  Dec(FLockValidate);
end;

procedure TcxCustomEdit.PopulateSizeProperties(var AEditSizeProperties: TcxEditSizeProperties);
begin
  AEditSizeProperties := cxDefaultEditSizeProperties;
  AEditSizeProperties.MaxLineCount := 1;
  if IsAutoWidth and not IsHeightDependOnWidth then
    AEditSizeProperties.Width := cxMaxRectSize
  else
    AEditSizeProperties.Width := Width;
end;

function TcxCustomEdit.GetClearValue: TcxEditValue;
begin
  Result := Null;
end;

class function TcxCustomEdit.GetDataBindingClass: TcxEditDataBindingClass;
begin
  Result := TcxEditDataBinding;
end;

function TcxCustomEdit.GetDefaultButtonVisibleIndex: Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Length(ViewInfo.ButtonsInfo) - 1 do
    if ViewInfo.ButtonsInfo[I].Data.Default then
    begin
      Result := I;
      Break;
    end;
end;

function TcxCustomEdit.GetDisplayText: string;
begin
  Result := '';
end;

function TcxCustomEdit.GetDisplayValue: TcxEditValue;
begin
  Result := GetDisplayText;
end;

function TcxCustomEdit.GetEditDataClass: TcxCustomEditDataClass;
begin
  Result := nil;
end;

function TcxCustomEdit.GetEditingValue: TcxEditValue;
begin
  if Focused and not IsEditValidated and ModifiedAfterEnter then
    Result := InternalGetEditingValue
  else
    Result := EditValue;
end;

function TcxCustomEdit.GetInnerEditClass: TControlClass;
begin
  Result := nil;
end;

function TcxCustomEdit.HandleMouseWheel(Shift: TShiftState): Boolean;
begin
  Result := not IsDesigning and (not IsInplace or ActiveProperties.UseMouseWheel or (ssCtrl in Shift)) and Focused;
end;

procedure TcxCustomEdit.HandleValidationError(const AErrorText: string;
  AValidationOptions: TcxEditValidationOptions; ACanAbortExecution: Boolean);

  function AllowLoseFocus: Boolean;
  begin
    Result := (evoAllowLoseFocus in ActiveProperties.ValidationOptions) and
      ({not IsInplace or} (AErrorText = '') or not (evoRaiseException in AValidationOptions));
  end;

  procedure ProcessFocusFeatures;
  var
    AControl: TWinControl;
  begin
    if not CanFocus then
      IsEditValidated := True
    else
      if HandleAllocated and IsWindowVisible(Handle) then
      begin
        if not Focused and not AllowLoseFocus then
        begin
          if HasInnerEdit then
            AControl := InnerEdit.Control
          else
            AControl := Self;
          FValidateErrorProcessing := True;
          try
            AControl.SetFocus;
          finally
            FValidateErrorProcessing := False;
          end;
        end;

        if Focused then
          SelectAll;
      end;
  end;

begin
  ProcessFocusFeatures;

  if ActiveProperties.BeepOnError then
    Beep;

  if (AErrorText <> '') and (evoRaiseException in AValidationOptions) then
    raise EcxEditValidationError.Create(AErrorText)
  else
    if ACanAbortExecution and not (evoAllowLoseFocus in AValidationOptions) then
      Abort;
end;

function TcxCustomEdit.HasInnerEdit: Boolean;
begin
  Result := Assigned(InnerEdit);
end;

procedure TcxCustomEdit.Initialize;

  procedure CreateDblClickTimer;
  begin
    FDblClickTimer := cxCreateTimer(DblClickTimerHandler, GetDblClickInterval, False);
  end;

begin
  FChangeEventsCatcher := TcxEditChangeEventsCatcher.Create(Self);
  FProperties := GetPropertiesClass.Create(Self);
  InitContentParams;

  ControlStyle := ControlStyle + [csSetCaption, csCaptureMouse];
  TabStop := True;

  FInnerEdit := CreateInnerEdit;
  if HasInnerEdit then
  begin
    InnerControl := FInnerEdit.Control;
    InitializeInnerEdit;
  end;
  SetReplicatableFlag;

  FDataBinding := GetDataBindingClass.Create(Self);

  FAutoHeight := not IsInplace;
  FAutoWidth := not IsInplace;

  FCaptureButtonVisibleIndex := -1;
  FEditValue := Null;
  FLockValidate := 0;
  FUpdate := False;

  Properties.OnPropertiesChanged := PropertiesChanged;

  if IsInplace then
  begin
    Keys := Keys + [kAll, kArrows];
    if GetInnerEditClass = nil then
      Keys := Keys + [kTab];
  end;
  CreateDblClickTimer;
  if not IsInplace then
    Properties.FIDefaultValuesProvider := FDataBinding.IDefaultValuesProvider;

  ViewInfo.FEdit := Self;
  FIsJustCreated := True;
end;

procedure TcxCustomEdit.InitializeEditData;
begin
end;

procedure TcxCustomEdit.InitializeInnerEdit;
begin
  InnerEdit.Parent := Self;
  if ActiveProperties.HasDisplayValue then
    InnerEdit.OnChange := ChangeHandler;
  TControlAccess(InnerEdit.Control).ParentShowHint := False;
end;

procedure TcxCustomEdit.InitializeViewData(AViewData: TcxCustomEditViewData);
begin
  AViewData.Enabled := Enabled;
  AViewData.Focused := Focused;
  AViewData.IsTouchScrollUIMode := IsPopupScrollBars;
  AViewData.UseRightToLeftScrollBar := UseRightToLeftScrollBar;
  AViewData.UseRightToLeftAlignment := UseRightToLeftAlignment;
  AViewData.UseRightToLeftReading := UseRightToLeftReading;
  if IsScrollBarActive(sbHorizontal) then
    AViewData.HScrollBar := HScrollBar
  else
    AViewData.HScrollBar := nil;
  AViewData.InnerEdit := InnerEdit;
  AViewData.IsDesigning := IsDesigning;
  if IsScrollBarActive(sbVertical) then
    AViewData.VScrollBar := VScrollBar
  else
    AViewData.VScrollBar := nil;
end;

procedure TcxCustomEdit.PrepareEditForInplaceActivation;
begin
//do nothing
end;

function TcxCustomEdit.InternalDoEditing: Boolean;
begin
  Result := True;
end;

procedure TcxCustomEdit.InitiateAction;
begin
  inherited InitiateAction;
  ActiveProperties.InitiateActions;
end;

function TcxCustomEdit.InternalFocused: Boolean;
begin
  Result := not ActiveProperties.IsEditValueConversionDependOnFocused or Focused;
end;

function TcxCustomEdit.IsChildWindow(AWnd: THandle): Boolean;
begin
  Result := cxContainer.IsRelatedWindow(Self, AWnd);
end;

function TcxCustomEdit.IsEditClass: Boolean;
begin
  Result := False;
end;

function TcxCustomEdit.IsRepositoryItemAcceptable(
  ARepositoryItem: TcxEditRepositoryItem): Boolean;
begin
  Result := (ARepositoryItem = nil) or
    ARepositoryItem.ArePropertiesCompatible(GetPropertiesClass);
end;

procedure TcxCustomEdit.LockChangeEvents(ALock: Boolean; AInvokeChangedOnUnlock: Boolean = True);
begin
  ChangeEventsCatcher.Lock(ALock, AInvokeChangedOnUnlock);
end;

procedure TcxCustomEdit.LockClick(ALock: Boolean);
begin
  if ALock then
    Inc(FClickLockCount)
  else
    if FClickLockCount > 0 then
      Dec(FClickLockCount);
end;

procedure TcxCustomEdit.LockEditValueChanging(ALock: Boolean);
begin
  if ALock then
    Inc(FEditValueChangingLockCount)
  else
    if FEditValueChangingLockCount > 0 then
      Dec(FEditValueChangingLockCount);
end;

function TcxCustomEdit.InternalGetActiveProperties: TcxCustomEditProperties;
begin
  if FRepositoryItem = nil then
    Result := Properties
  else
  begin
    Result := FRepositoryItem.Properties;
    Result.FIDefaultValuesProvider := DataBinding.IDefaultValuesProvider;
  end;
end;

function TcxCustomEdit.InternalGetEditingValue: TcxEditValue;
begin
  Result := EditValue;
end;

procedure TcxCustomEdit.InternalPostEditValue(AValidateEdit: Boolean = False);
begin
  if AValidateEdit then
  begin
    DisableUserAction;
    try
      if not InternalValidateEdit then
        Exit;
    finally
      EnableUserAction;
    end;
  end;
  try
    BeforePosting;
    try
      if IsInplace then
      begin
        if not AValidateEdit then
          DisableValidate;
        DoPostEditValue;
        if not AValidateEdit then
          EnableValidate;
        EditModified := False;
      end
      else
        if DataBinding.Modified then
        begin
          SaveModified;
          try
            DataBinding.StoredValue := EditValue;
          finally
            RestoreModified;
          end;
        end;
    finally
      AfterPosting;
    end;
  except
    if CanShowValidationErrorOnPostEditValue then
    begin
      HandleValidationError('', ActiveProperties.ValidationOptions, False);
      raise;
    end;
  end;
end;

procedure TcxCustomEdit.InternalPostValue;
begin
  if DoEditing then
  begin
    ModifiedAfterEnter := True;
    try
      IsEditValidated := True;
      InternalPostEditValue(False);
    finally
      ModifiedAfterEnter := False;
    end;
  end;
end;

procedure TcxCustomEdit.InternalSetDisplayValue(const Value: TcxEditValue);
begin
end;

procedure TcxCustomEdit.InternalSetEditValue(const Value: TcxEditValue;
  AValidateEditValue: Boolean);
var
  AEditValueChanged: Boolean;
begin
  if IsUserAction and not DoEditing then
    Exit;
  AEditValueChanged := False;
  if ActiveProperties.CanCompareEditValue then
    AEditValueChanged := not cxEditVarEquals(Value, FEditValue);
  InternalStoreEditValue(Value);
  SynchronizeDisplayValue;
  if IsUserAction then
    ModifiedAfterEnter := True
  else
    EditModified := False;
  if AEditValueChanged then
  begin
    DoEditValueChanged;
    if not ActiveProperties.HasDisplayValue then
      DoChange;
  end;
end;

procedure TcxCustomEdit.InternalStoreEditValue(const Value: TcxEditValue);
begin
  FEditValue := Value;
end;

procedure TcxCustomEdit.InternalValidateDisplayValue(const ADisplayValue: TcxEditValue);
begin
  SaveModified;
  InternalSetDisplayValue(ADisplayValue);
  RestoreModified;

//  DataBinding.UpdateDisplayValue; //# T328053
end;

function TcxCustomEdit.IsActiveControl: Boolean;
var
  AParentForm: TCustomForm;
begin
  Result := Focused;
  if Result then
    Exit;
  AParentForm := GetParentForm(Self);
  if AParentForm <> nil then
  begin
    Result := AParentForm.ActiveControl = Self;
    Result := Result or HasInnerEdit and (AParentForm.ActiveControl = InnerEdit.Control);
  end;
end;

function TcxCustomEdit.IsButtonDC(ADC: THandle): Boolean;
begin
  Result := False;
end;

function TcxCustomEdit.IsClickEnabledDuringLoading: Boolean;
begin
  Result := False;
end;

function TcxCustomEdit.IsDBEdit: Boolean;
begin
  Result := DataBinding.IDefaultValuesProvider.IsDataStorage;
end;

function TcxCustomEdit.IsDBEditPaintCopyDrawing: Boolean;

  function IsParentDBCtrlGrid: Boolean;
  var
    AParent: TWinControl;
  begin
    Result := False;
    AParent := Parent;
    while (AParent <> nil) and not Result do
    begin
      Result := AParent.ClassName = 'TDBCtrlPanel';
      AParent := AParent.Parent;
    end;
  end;

begin
  Result := not ModifiedAfterEnter and (csPaintCopy in ControlState) and IsDBEdit and IsParentDBCtrlGrid;
end;

function TcxCustomEdit.IsDefaultButtonKey(AShortCut: TShortCut): Boolean;
var
  AIndex: Integer;
begin
  AIndex := GetDefaultButtonVisibleIndex;
  Result := (AShortCut = ActiveProperties.ClickKey) and
    (AIndex <> -1) and (ViewInfo.ButtonsInfo[AIndex].Data.State <> ebsDisabled);
end;

function TcxCustomEdit.IsDoubleBufferedNeeded: Boolean;
begin
  Result := inherited IsDoubleBufferedNeeded or dxFader.Contains(Self);
end;

function TcxCustomEdit.IsEditorKey(Key: Word; Shift: TShiftState): Boolean;
begin
  Result := IsDefaultButtonKey(ShortCut(Key, Shift));
end;

function TcxCustomEdit.IsEditValueStored: Boolean;
begin
  Result := not VarIsNull(EditValue);
end;

function TcxCustomEdit.IsNativeBackground: Boolean;
begin
  Result := False;
end;

function TcxCustomEdit.IsOnChangeEventAssigned: Boolean;
begin
  Result := Assigned(Properties.OnChange) or
    Assigned(ActiveProperties.OnChange);
end;

function TcxCustomEdit.IsOnEditValueChangedEventAssigned: Boolean;
begin
  Result := Assigned(Properties.OnEditValueChanged) or
    Assigned(ActiveProperties.OnEditValueChanged);
end;

function TcxCustomEdit.IsOnValidateEventAssigned: Boolean;
begin
  Result := Assigned(Properties.OnValidate) or
    Assigned(ActiveProperties.OnValidate);
end;

function TcxCustomEdit.IsResetEditClass: Boolean;
begin
  Result := IsDBEdit or ActiveProperties.IsResetEditClass;
end;

function TcxCustomEdit.IsSpecialKey(Key: Word; Shift: TShiftState): Boolean;
var
  AShortCut: TShortCut;
begin
  AShortCut := ShortCut(Key, Shift);
  Result := (AShortCut = ActiveProperties.ClearKey) or IsDefaultButtonKey(AShortCut);
end;

function TcxCustomEdit.IsTransparent: Boolean;
begin
  Result := not IsInplace and Transparent;
end;

function TcxCustomEdit.IsUserAction: Boolean;
begin
  Result := not IsUserActionDisabled and (FUserActionCount > 0);
end;

procedure TcxCustomEdit.DisableUserAction;
begin
  Inc(FUserActionDisabledCount);
end;

procedure TcxCustomEdit.EnableUserAction;
begin
  dxTestCheck(FUserActionDisabledCount > 0, 'TcxCustomEdit.EnableUserAction fails');
  Dec(FUserActionDisabledCount);
end;

function TcxCustomEdit.IsUserActionDisabled: Boolean;
begin
  Result := FUserActionDisabledCount <> 0;
end;

function TcxCustomEdit.IsValidChar(AChar: Char): Boolean;
begin
  with ActiveProperties do
    Result := (IDefaultValuesProvider = nil) or
      IDefaultValuesProvider.IsValidChar(AChar) or
      IDefaultValuesProvider.IsOnSetTextAssigned;
end;

function TcxCustomEdit.NeedsInvokeAfterKeyDown(AKey: Word; AShift: TShiftState): Boolean;
begin
  case AKey of
    VK_INSERT:
      Result := AShift * [ssCtrl, ssShift] = [];
    VK_DELETE:
      Result := not (ssShift in AShift);
  else
    Result := False;
  end;
end;

procedure TcxCustomEdit.PaintCopyDraw;
var
  AViewInfo: TcxCustomEditViewInfo;
  AViewData: TcxCustomEditViewData;
begin
  SetVisibleBoundsClipRect;
  AViewInfo := TcxCustomEditViewInfo(Properties.GetViewInfoClass.Create);
  try
    AViewInfo.FEdit := Self;
    AViewData := TcxCustomEditViewData(CreateViewData);
    try
      AViewData.EditValueToDrawValue(DataBinding.StoredValue, AViewInfo);
      AViewData.Calculate(Canvas, GetControlRect(Self), cxInvalidPoint, cxmbNone, [], AViewInfo, False);

      AViewInfo.Paint(Canvas);
    finally
      FreeAndNil(AViewData);
    end;
  finally
    FreeAndNil(AViewInfo);
  end;
  Canvas.ExcludeClipRect(GetControlRect(Self));
end;

procedure TcxCustomEdit.PrepareDisplayValue(const AEditValue: TcxEditValue;
  var DisplayValue: TcxEditValue; AEditFocused: Boolean);
begin
  ActiveProperties.PrepareDisplayValue(AEditValue, DisplayValue, AEditFocused);
end;

procedure TcxCustomEdit.PrepareForInplaceActivation;
begin
//do nothing
end;

procedure TcxCustomEdit.PropertiesChanged(Sender: TObject);
begin
  FIsEditValidated := False;
  UpdateInnerEditReadOnly;
  if ModifiedAfterEnter and RealReadOnly then
    ModifiedAfterEnter := False;
  if ActiveProperties.Transparent then
    Transparent := True;
  CalculateAnchors;
  ShortRefreshContainer(False);
end;

function TcxCustomEdit.PropertiesChangeLocked: Boolean;
begin
  Result := not IsDestroying;
  if Result then
    Result := (GetInnerEditClass = nil) or (InnerEdit <> nil);
  Result := not Result;
end;

function TcxCustomEdit.RealReadOnly: Boolean;
begin
  Result := ActiveProperties.ReadOnly or DataBinding.IsInnerControlHaveToBeReadOnly;
end;

procedure TcxCustomEdit.RepositoryItemAssigned;
begin
end;

procedure TcxCustomEdit.RepositoryItemAssigning;
begin
end;

procedure TcxCustomEdit.ResetEditValue;
begin
  if FModifiedAfterEnter and ActiveProperties.IsResetEditClass then
  begin
    LockChangeEvents(True);
    try
      SetModifiedAfterEnterValue(False);
      if IsDBEdit then
        InternalEditValue := DataBinding.StoredValue
      else
        InternalEditValue := FPrevEditValue;

      RollBackErrorState;

      EditModified := True;
      SelectAll;
      if ActiveProperties.ImmediatePost and CanPostEditValue then
        InternalPostEditValue;
    finally
      LockChangeEvents(False);
    end;
  end;
end;

procedure TcxCustomEdit.ResetErrorState;
begin
  FValidationError := False;
  FPrevValidationError := False;
  if HandleAllocated then
    PostMessage(Handle, DXM_SHORTREFRESHCONTAINER, 0, 0);
end;

procedure TcxCustomEdit.RollBackErrorState;
begin
  FValidationError := FPrevValidationError;
  FPrevValidationError := False;
  if HandleAllocated then
    PostMessage(Handle, DXM_SHORTREFRESHCONTAINER, 0, 0);
end;

procedure TcxCustomEdit.RestoreModified;
begin
  with FPrevModifiedList[Length(FPrevModifiedList) - 1] do
  begin
    FModified := Modified;
    SetModifiedAfterEnterValue(ModifiedAfterEnter);
  end;
  SetLength(FPrevModifiedList, Length(FPrevModifiedList) - 1);
end;

procedure TcxCustomEdit.SaveModified;
begin
  SetLength(FPrevModifiedList, Length(FPrevModifiedList) + 1);
  with FPrevModifiedList[Length(FPrevModifiedList) - 1] do
  begin
    Modified := FModified;
    ModifiedAfterEnter := FModifiedAfterEnter;
  end;
end;

function TcxCustomEdit.SendActivationKey(Key: Char): Boolean;
begin
  Result := True;
end;

function TcxCustomEdit.SetDisplayText(const Value: string): Boolean;
begin
  Result := False;
end;

procedure TcxCustomEdit.SetEditAutoSize(AValue: Boolean);
begin
  if (AutoSize <> AVAlue) and not IsInplace then
  begin
    FAutoWidth := AValue;
    FAutoHeight := AValue;
    CheckAutoSizeChanges(AValue);
  end;
end;

procedure TcxCustomEdit.SetInternalDisplayValue(Value: TcxEditValue);
begin
end;

procedure TcxCustomEdit.SetEditValue(const Value: TcxEditValue);
begin
  if FEditValueChangingLockCount > 0 then
    Exit;
  LockClick(True);
  try
    if not(IsUserAction and not DoEditing) then
      InternalSetEditValue(Value, True);
  finally
    LockClick(False);
  end;
end;

procedure TcxCustomEdit.SetInternalEditValue(const Value: TcxEditValue);
begin
  if FEditValueChangingLockCount > 0 then
    Exit;
  if not (IsUserAction and not DoEditing) then
    InternalSetEditValue(Value, True);
end;

procedure TcxCustomEdit.SynchronizeDisplayValue;
begin
end;

procedure TcxCustomEdit.SynchronizeEditValue;
begin
end;

function TcxCustomEdit.TabsNeeded: Boolean;
begin
  Result := IsInplace;
end;

function TcxCustomEdit.UpdateContentOnFocusChanging: Boolean;
begin
  Result := True;
end;

procedure TcxCustomEdit.UpdateDrawValue;
begin
end;

procedure TcxCustomEdit.UpdateInnerEditReadOnly;
begin
  if not PropertiesChangeLocked and (InnerEdit <> nil) then
    InnerEdit.ReadOnly := RealReadOnly;
end;

function TcxCustomEdit.ValidateKeyDown(var Key: Word; Shift: TShiftState): Boolean;
begin
  Result := not(not DataBinding.Modified and CanKeyDownModifyEdit(Key, Shift) and
    not DoEditing);
end;

function TcxCustomEdit.ValidateKeyPress(var Key: Char): Boolean;
begin
  if IsTextChar(Key) and not IsValidChar(Key) then
  begin
    Key := #0;
    Beep;
  end
  else
    if CanKeyPressModifyEdit(Key) and not DoEditing then
      Key := #0;
  Result := Key <> #0;
end;

function TcxCustomEdit.WantNavigationKeys: Boolean;
begin
  Result := False;
end;

procedure TcxCustomEdit.LockedInnerEditWindowProc(var Message: TMessage);
begin
  FWindowProcObject.DefaultProc(Message);
end;

procedure TcxCustomEdit.LockInnerEditRepainting;
begin
  if HasInnerEdit then
    FWindowProcObject := cxWindowProcController.Add(InnerEdit.Control.Handle, LockedInnerEditWindowProc);
end;

procedure TcxCustomEdit.UnlockInnerEditRepainting;
begin
  cxWindowProcController.Remove(FWindowProcObject);
end;

function TcxCustomEdit.UseAnchors: Boolean;
begin
  Result := UseAnchorX or UseAnchorY;
end;

function TcxCustomEdit.UseAnchorX: Boolean;
begin
  Result := False;
end;

function TcxCustomEdit.UseAnchorY: Boolean;
begin
  Result := False;
end;

procedure TcxCustomEdit.BeginUserAction;
begin
  Inc(FUserActionCount);
end;

procedure TcxCustomEdit.EndUserAction;
begin
  if FUserActionCount > 0 then
    Dec(FUserActionCount);
end;

// IcxEditRepositoryItemListener
procedure TcxCustomEdit.RepositoryItemListenerItemRemoved(Sender: TcxEditRepositoryItem);
begin
  RepositoryItem := nil;
end;

procedure TcxCustomEdit.RepositoryItemListenerPropertiesChanged(Sender: TcxEditRepositoryItem);
begin
  if ComponentState * [csLoading, csDestroying] = [] then
    PropertiesChanged(Sender.Properties);
end;

//IdxSpellCheckerSetEditValue

function TcxCustomEdit.SupportsSpelling: Boolean;
begin
  Result := False;
end;

procedure TcxCustomEdit.SpellCheckerSetIsBarControl(AValue: Boolean);
begin
  FIsBarControl := AValue;
end;

procedure TcxCustomEdit.SpellCheckerSetSelText(const AValue: string; APost: Boolean = False);
begin
end;

procedure TcxCustomEdit.SpellCheckerSetValue(const AValue: Variant);
begin
  InternalSetEditValue(AValue, False);
end;

procedure TcxCustomEdit.DisableTouchModeSupport;
begin
  SupportsTouchMode := False;
end;

procedure TcxCustomEdit.EnableTouchModeSupport;
begin
  SupportsTouchMode := True;
end;

function TcxCustomEdit.GetEditValue: TcxEditValue;
begin
  Result := FEditValue;
end;

function TcxCustomEdit.GetHintText(APart: Integer): string;
begin
  Result := ViewInfo.GetHintText(APart);
  if ShowHint and (Result = '') then
    Result := Hint;
end;

function TcxCustomEdit.CheckButtonShortCuts(AKey: Integer): Boolean;
var
  I: Integer;
  AButtonViewInfo: TcxEditButtonViewInfo;
begin
  Result := False;
  for I := 0 to High(ViewInfo.ButtonsInfo) do
  begin
    AButtonViewInfo := ViewInfo.ButtonsInfo[I];
    if (AButtonViewInfo.Data.State <> ebsDisabled) and IsAccel(AKey, AButtonViewInfo.Data.Caption) then
    begin
      Result := True;
      DoButtonClick(I);
      Break;
    end;
  end;
end;

procedure TcxCustomEdit.DblClickTimerHandler(Sender: TObject);
begin
  FDblClickTimer.Enabled := False;
end;

procedure TcxCustomEdit.DoClearEditData(AEditData: TcxCustomEditData);
begin
  if (FEditData = AEditData) and (FEditData <> nil) and FEditData.Cleared then
  begin
    InitializeEditData;
    FEditData.Cleared := False;
  end;
end;

function TcxCustomEdit.GetActiveProperties: TcxCustomEditProperties;
begin
  Result := InternalGetActiveProperties;
end;

function TcxCustomEdit.GetAutoSize: Boolean;
begin
  Result := AutoWidth or AutoHeight;
end;

function TcxCustomEdit.GetEditActiveStyle: TcxCustomEditStyle;
begin
  Result := TcxCustomEditStyle(inherited ActiveStyle);
end;

function TcxCustomEdit.GetHeight: Integer;
begin
  CheckHandle;
  Result := inherited Height;
end;

function TcxCustomEdit.GetInternalStyle(AState: TcxContainerStateItem): TcxCustomEditStyle;
begin
  Result := TcxCustomEditStyle(FStyles[AState]);
end;

function TcxCustomEdit.GetStyle: TcxEditStyle;
begin
  Result := TcxEditStyle(FStyles.Style);
end;

function TcxCustomEdit.GetStyleDisabled: TcxEditStyle;
begin
  Result := TcxEditStyle(FStyles.StyleDisabled);
end;

function TcxCustomEdit.GetStyleFocused: TcxEditStyle;
begin
  Result := TcxEditStyle(FStyles.StyleFocused);
end;

function TcxCustomEdit.GetStyleHot: TcxEditStyle;
begin
  Result := TcxEditStyle(FStyles.StyleHot);
end;

function TcxCustomEdit.GetViewData(out AIsViewDataCreated: Boolean): TcxCustomEditViewData;
begin
  Result := CreateViewData;
  AIsViewDataCreated := True;
end;

function TcxCustomEdit.GetViewInfo: TcxCustomEditViewInfo;
begin
  Result := TcxCustomEditViewInfo(FViewInfo);
end;

procedure TcxCustomEdit.HandleCutMessage;
begin
  if IsReadOnly then
    Exit;
  if Focused then
    BeginUserAction;
  try
    if DoEditing then
      CutToClipboard
    else
      CopyToClipboard;
  finally
    if Focused then
      EndUserAction;
  end;
end;

procedure TcxCustomEdit.HandlePasteMessage;
begin
  if IsReadOnly then
    Exit;
  LockChangeEvents(True);
  try
    if Focused then
      BeginUserAction;
    try
      if DoEditing then
        PasteFromClipboard;
    finally
      if Focused then
        EndUserAction;
    end;
  finally
    LockChangeEvents(False);
  end;
end;

procedure TcxCustomEdit.InitContentParams;
var
  AViewData: TcxCustomEditViewData;
begin
  AViewData := CreateViewData;
  try
    AViewData.InitEditContentParams(ContentParams);
    FIsContentParamsInitialized := True;
  finally
    FreeAndNil(AViewData);
  end;
end;

procedure TcxCustomEdit.InternalCanResize(var ANewWidth, ANewHeight: Integer);
var
  AEditSizeProperties: TcxEditSizeProperties;
  ASize: TSize;
  AViewData: TcxCustomEditViewData;
begin
  if CanAutoSize then
  begin
    PopulateSizeProperties(AEditSizeProperties);
    AEditSizeProperties.Height := ANewHeight;
    AEditSizeProperties.Width := ANewWidth;

    AViewData := TcxCustomEditViewData(CreateViewData);
    try
      ASize := AViewData.GetEditSize(cxScreenCanvas, EditValue, AEditSizeProperties, ViewInfo);
      cxScreenCanvas.Dormant;
    finally
      FreeAndNil(AViewData);
    end;

    if IsAutoWidth then
      ANewWidth := ASize.cx;
    if IsAutoHeight then
      ANewHeight := ASize.cy;
  end;
end;

procedure TcxCustomEdit.ReadAnchorX(Reader: TReader);
begin
  FAnchorX := Reader.ReadInteger;
  Include(FAnchorScalingFlags, sfLeft);
end;

procedure TcxCustomEdit.ReadAnchorY(Reader: TReader);
begin
  FAnchorY := Reader.ReadInteger;
  Include(FAnchorScalingFlags, sfTop);
end;

procedure TcxCustomEdit.ReadHeight(Reader: TReader);
begin
  Height := Reader.ReadInteger;
end;

procedure TcxCustomEdit.ReadWidth(Reader: TReader);
begin
  Width := Reader.ReadInteger;
end;

procedure TcxCustomEdit.SetAutoHeight(AValue: Boolean);
begin
  if FAutoHeight <> AValue then
  begin
    FAutoHeight := AValue;
    CheckAutoSizeChanges(AValue);
  end;
end;

procedure TcxCustomEdit.SetAutoWidth(AValue: Boolean);
begin
  if FAutoWidth <> AValue then
  begin
    FAutoWidth := AValue;
    CheckAutoSizeChanges(AValue);
  end;
end;

procedure TcxCustomEdit.SetDataBinding(Value: TcxEditDataBinding);
begin
  FDataBinding.Assign(Value.GetEditDataBindingInstance);
end;

procedure TcxCustomEdit.SetHeight(Value: Integer);
begin
  inherited Height := Value;
end;

procedure TcxCustomEdit.SetInternalStyle(AState: TcxContainerStateItem;
  Value: TcxCustomEditStyle);
begin
  FStyles[AState] := Value;
end;

procedure TcxCustomEdit.SetInternalReadOnly(Value: Boolean);
begin
  if InnerEdit <> nil then
    InnerEdit.ReadOnly := Value;
end;

procedure TcxCustomEdit.SetModified(Value: Boolean);
begin
  FModified := Value;
  if not Value then
    SetModifiedAfterEnterValue(False);
end;

procedure TcxCustomEdit.SetModifiedAfterEnter(Value: Boolean);
begin
  SetModifiedAfterEnterValue(Value);
  if Value then
  begin
    FIsEditValidated := False;
    FModified := True;
  end;
end;

procedure TcxCustomEdit.SetModifiedAfterEnterValue(Value: Boolean);
begin
  FModifiedAfterEnter := Value;
end;

procedure TcxCustomEdit.SetPrevEditValue(Value: TcxEditValue);
begin
  if not FValidateErrorProcessing then
  begin
    FPrevEditValue := Value;
    FPrevValidationError := FValidationError;
  end;
end;

procedure TcxCustomEdit.SetProperties(Value: TcxCustomEditProperties);
begin
  Properties.Assign(Value);
end;

procedure TcxCustomEdit.SetRepositoryItem(Value: TcxEditRepositoryItem);
begin
  if Value = FRepositoryItem then
    Exit;
  if not IsRepositoryItemAcceptable(Value) then
    raise EcxEditError.Create(cxGetResourceString(@cxSEditInvalidRepositoryItem));
  RepositoryItemAssigning;
  if FRepositoryItem <> nil then
    FRepositoryItem.RemoveListener(Self);

  FRepositoryItem := Value;
  if FRepositoryItem <> nil then
    FRepositoryItem.AddListener(Self);
  if FRepositoryItem = nil then
  begin
    if ComponentState * [csLoading, csDestroying] = [] then
      Properties.OnPropertiesChanged := PropertiesChanged;
    Properties.IDefaultValuesProvider := DataBinding.IDefaultValuesProvider;
  end
  else
  begin
    Properties.FIDefaultValuesProvider := nil;
    RepositoryItemListenerPropertiesChanged(FRepositoryItem);
    Properties.OnPropertiesChanged := nil;
  end;
  RepositoryItemAssigned;
end;

procedure TcxCustomEdit.SetReplicatableFlag;

  procedure SetControlReplicatable(AControl: TControl);
  begin
    AControl.ControlStyle := AControl.ControlStyle + [csReplicatable];
  end;

  procedure SetReplicatable(AControl: TControl);
  var
    I: Integer;
  begin
    if AControl = nil then Exit;
    SetControlReplicatable(AControl);
    if AControl is TWinControl then
      for I := 0 to TWinControl(AControl).ControlCount - 1 do
        SetReplicatable(TWinControl(AControl).Controls[I]);
  end;

begin
  SetReplicatable(Self);
  if NeedsScrollBars and not IsPopupScrollBars then
  begin
    SetControlReplicatable(HScrollBar.Control);
    SetControlReplicatable(VScrollBar.Control);
    SetControlReplicatable(SizeGrip);
  end;
end;

procedure TcxCustomEdit.SetStyle(Value: TcxEditStyle);
begin
  FStyles.Style := Value;
end;

procedure TcxCustomEdit.SetStyleDisabled(Value: TcxEditStyle);
begin
  FStyles.StyleDisabled := Value;
end;

procedure TcxCustomEdit.SetStyleFocused(Value: TcxEditStyle);
begin
  FStyles.StyleFocused := Value;
end;

procedure TcxCustomEdit.SetStyleHot(Value: TcxEditStyle);
begin
  FStyles.StyleHot := Value;
end;

procedure TcxCustomEdit.WriteAnchorX(Writer: TWriter);
begin
  CalculateAnchors;
  Writer.WriteInteger(AnchorX);
end;

procedure TcxCustomEdit.WriteAnchorY(Writer: TWriter);
begin
  CalculateAnchors;
  Writer.WriteInteger(AnchorY);
end;

procedure TcxCustomEdit.WriteHeight(Writer: TWriter);
begin
  Writer.WriteInteger(Height);
end;

procedure TcxCustomEdit.WriteWidth(Writer: TWriter);
begin
  Writer.WriteInteger(Width);
end;

procedure TcxCustomEdit.WMCopy(var Message: TMessage);
begin
  CopyToClipboard;
end;

procedure TcxCustomEdit.WMCut(var Message: TMessage);
begin
  if DataBinding.IsDataAvailable then
    HandleCutMessage;
end;

procedure TcxCustomEdit.CMHintShow(var Message: TCMHintShow);
var
  APart: Integer;
begin
  with Message.HintInfo^ do
  begin
    APart := ViewInfo.GetPart(CursorPos);
    if APart <> ecpNone then
    begin
      HintStr := GetShortHint(GetHintText(APart));
      CursorRect := ViewInfo.GetPartRect(APart);
    end
    else
      Message.Result := -1;
  end;
end;

procedure TcxCustomEdit.WMPaste(var Message: TMessage);
begin
  if DataBinding.IsDataAvailable then
    HandlePasteMessage;
end;

procedure TcxCustomEdit.CMDialogChar(var Message: TCMDialogChar);
begin
  if CheckButtonShortCuts(Message.CharCode) then
    Message.Result := 1
  else
    inherited;
end;

procedure TcxCustomEdit.CMEnter(var Message: TCMEnter);
begin
  inherited;
  if DataBinding.CanModify and not ActiveProperties.ReadOnly then
    SetInternalReadOnly(False);
end;

procedure TcxCustomEdit.CMShowingChanged(var Message: TMessage);
begin
  inherited;
  if Showing then
    SetSize;
end;

procedure TcxCustomEdit.DXMUpdateEditvalue(var Message: TMessage);
begin
  if IsDBEdit then
    DoHideEdit(True);
end;

{ TcxCustomEditProperties }

constructor TcxCustomEditProperties.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  CreatedEditPropertiesList.Add(Self);
  FFreeNotificator := TcxFreeNotificator.Create(nil);
  FFreeNotificator.OnFreeNotification := FreeNotification;
  FAlignment := TcxEditAlignment.Create(Self, GetDefaultVertAlignment);
  FAlignment.OnChanged := AlignmentChangedHandler;
  FAssignedValues := GetAssignedValuesClass.Create(Self);
  FAutoSelect := True;
  FButtons := GetButtonsClass.Create(Self, GetButtonsClass.GetButtonClass);
  FButtons.OnChange := ChangeHandler;
  FButtonsViewStyle := bvsNormal;
  FClickKey := VK_RETURN + scCtrl;
  FUseLeftAlignmentOnEditing := DefaultUseLeftAlignmentOnEditing;
  FUseMouseWheel := True;
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
  FInnerAlignment := TcxEditAlignment.Create(Self, GetDefaultVertAlignment);
  FValidationOptions := [evoRaiseException];
  FErrorIcon := TdxSmartImage.Create;
  FScaleFactor := TdxOwnedScaleFactor.Create;
  FScaleFactor.ListenerAdd(ScaleFactorChangeHandler);
end;

destructor TcxCustomEditProperties.Destroy;
begin
  FScaleFactor.ListenerRemove(ScaleFactorChangeHandler);
  FreeAndNil(FErrorIcon);
  ClearPropertiesDestroyingListeners(Self);
  FIDefaultValuesProvider := nil;
  FreeAndNil(FButtons);
  FreeAndNil(FInnerAlignment);
  FreeAndNil(FAssignedValues);
  FreeAndNil(FAlignment);
  FreeAndNil(FFreeNotificator);
  FreeAndNil(FImageChangeLink);
  FreeAndNil(FScaleFactor);
  if FCreatedEditPropertiesList <> nil then
    FCreatedEditPropertiesList.Remove(Self);
  inherited Destroy;
end;

procedure TcxCustomEditProperties.AfterConstruction;
begin
  inherited AfterConstruction;
  FScaleFactor.Owner := dxGetScaleFactor(Owner);
end;

procedure TcxCustomEditProperties.Assign(Source: TPersistent);
begin
  if Source is TcxCustomEditProperties then
  begin
    BeginUpdate;
    try
      DoAssign(TcxCustomEditProperties(Source));
    finally
      EndUpdate;
    end
  end
  else
    inherited Assign(Source);
end;

function TcxCustomEditProperties.CanCompareEditValue: Boolean;
begin
  Result := False;
end;

function TcxCustomEditProperties.Clone(AOwner: TPersistent): TcxCustomEditProperties;
begin
  Result := TcxCustomEditPropertiesClass(ClassType).Create(AOwner);
  Result.Assign(Self);
end;

class function TcxCustomEditProperties.GetButtonsClass: TcxEditButtonsClass;
begin
  Result := TcxEditButtons;
end;

class function TcxCustomEditProperties.GetContainerClass: TcxContainerClass;
begin
  Result := TcxCustomEdit;
end;

class function TcxCustomEditProperties.GetStyleClass: TcxCustomEditStyleClass;
begin
  Result := TcxEditStyle;
end;

class function TcxCustomEditProperties.GetViewInfoClass: TcxContainerViewInfoClass;
begin
  Result := TcxCustomEditViewInfo;
end;

procedure TcxCustomEditProperties.BeginUpdate;
begin
  if FUpdateCount = 0 then
    FChangedOccurred := False;
  Inc(FUpdateCount);
end;

procedure TcxCustomEditProperties.Changed;

  function CanFireEvent: Boolean;
  begin
    Result := not ChangedLocked and not((GetOwner is TComponent) and
      (csDestroying in TComponent(GetOwner).ComponentState));
  end;

begin
  if not CanFireEvent then
    FChangedOccurred := True
  else
    DoChanged;
end;

function TcxCustomEditProperties.ChangedLocked: Boolean;
begin
  Result := FChangedLocked or (FUpdateCount > 0);
end;

procedure TcxCustomEditProperties.DoUpdate(AProperties: TcxCustomEditProperties);
begin
//do nothing
end;

function TcxCustomEditProperties.CompareDisplayValues(
  const AEditValue1, AEditValue2: TcxEditValue): Boolean;
var
  ADisplayValue1, ADisplayValue2: TcxEditValue;
begin
  PrepareDisplayValue(AEditValue1, ADisplayValue1, False);
  PrepareDisplayValue(AEditValue2, ADisplayValue2, False);
  Result := VarEquals(ADisplayValue1, ADisplayValue2);
end;

function TcxCustomEditProperties.CreatePreviewProperties: TcxCustomEditProperties;
begin
  Result := TcxCustomEditPropertiesClass(ClassType).Create(nil);
end;

function TcxCustomEditProperties.CreateViewData(AStyle: TcxCustomEditStyle;
  AIsInplace: Boolean; APreviewMode: Boolean = False): TcxCustomEditViewData;
begin
  Result := GetViewDataClass.Create(Self, AStyle, AIsInplace);
  Result.PreviewMode := APreviewMode;
end;

procedure TcxCustomEditProperties.DataChanged;
begin
end;

procedure TcxCustomEditProperties.EndUpdate(AInvokeChanged: Boolean = True);
begin
  if FUpdateCount > 0 then
  begin
    Dec(FUpdateCount);
    if (FUpdateCount = 0) and AInvokeChanged and FChangedOccurred then
      Changed;
  end;
end;

function TcxCustomEditProperties.GetDisplayText(const AEditValue: TcxEditValue;
  AFullText: Boolean = False; AIsInplace: Boolean = True): string;
begin
  Result := '';
end;

function TcxCustomEditProperties.GetEditConstantPartSize(ACanvas: TcxCanvas;
  AEditStyle: TcxCustomEditStyle; AIsInplace: Boolean;
  const AEditSizeProperties: TcxEditSizeProperties; var MinContentSize: TSize): TSize;
var
  AViewData: TcxCustomEditViewData;
begin
  AViewData := TcxCustomEditViewData(CreateViewData(AEditStyle, AIsInplace));
  try
    Result := AViewData.GetEditConstantPartSize(ACanvas, AEditSizeProperties,
      MinContentSize);
  finally
    FreeAndNil(AViewData);
  end;
end;

function TcxCustomEditProperties.GetEditContentSize(ACanvas: TcxCanvas;
  AEditStyle: TcxCustomEditStyle; AIsInplace: Boolean; const AEditValue: TcxEditValue;
  const AEditSizeProperties: TcxEditSizeProperties): TSize;
var
  AViewData: TcxCustomEditViewData;
begin
  AViewData := TcxCustomEditViewData(CreateViewData(AEditStyle, AIsInplace));
  try
    Result := AViewData.GetEditContentSize(ACanvas, AEditValue, AEditSizeProperties);
  finally
    FreeAndNil(AViewData);
  end;
end;

function TcxCustomEditProperties.GetEditSize(ACanvas: TcxCanvas;
  AEditStyle: TcxCustomEditStyle; AIsInplace: Boolean; const AEditValue: TcxEditValue;
  AEditSizeProperties: TcxEditSizeProperties): TSize;
var
  AViewData: TcxCustomEditViewData;
begin
  AViewData := TcxCustomEditViewData(CreateViewData(AEditStyle, AIsInplace));
  try
    Result := AViewData.GetEditSize(ACanvas, AEditValue, AEditSizeProperties);
  finally
    FreeAndNil(AViewData);
  end;
end;

function TcxCustomEditProperties.GetSpecialFeatures: TcxEditSpecialFeatures;
begin
  Result := [];
end;

function TcxCustomEditProperties.GetSupportedOperations: TcxEditSupportedOperations;
begin
  if Buttons.Count > 0 then
    Result := [esoHotTrack]
  else
    Result := [];
end;

function TcxCustomEditProperties.IsActivationKey(AKey: Char): Boolean;
begin
  Result := False;
end;

function TcxCustomEditProperties.IsEditValueValid(var EditValue: TcxEditValue; AEditFocused: Boolean): Boolean;
begin
  Result := True;
end;

procedure TcxCustomEditProperties.RestoreDefaults;
begin
  FInnerAlignment.Reset;
  BeginUpdate;
  try
    AssignedValues.RestoreDefaults;
  finally
    EndUpdate;
  end;
end;

procedure TcxCustomEditProperties.Update(AProperties: TcxCustomEditProperties);
begin
  if (AProperties is ClassType) and (AProperties <> Self) then
    DoUpdate(AProperties);
end;

procedure TcxCustomEditProperties.ValidateDisplayValue(
  var ADisplayValue: TcxEditValue; var AErrorText: TCaption; var AError: Boolean; AEdit: TcxCustomEdit);
var
  AIsUserErrorDisplayValue: Boolean;
begin
  if CanValidate then
  begin
    if AErrorText = '' then
      AErrorText := GetValidateErrorText(ekDefault);
    DoValidate(ADisplayValue, AErrorText, AError, AEdit, AIsUserErrorDisplayValue);
  end;
end;

procedure TcxCustomEditProperties.AlignmentChangedHandler(Sender: TObject);
begin
  FInnerAlignment.Assign(FAlignment);
  Changed;
end;

procedure TcxCustomEditProperties.BaseSetAlignment(Value: TcxEditAlignment);
begin
  FInnerAlignment.Assign(Value);
  Changed;
end;

procedure TcxCustomEditProperties.ChangeScale(M, D: Integer);
begin
  FButtons.ChangeScale(M, D);
end;

function TcxCustomEditProperties.CanModify: Boolean;
begin
  Result := AssignedValues.ReadOnly and not FReadOnly;
  if not Result then
  begin
    Result := not ReadOnly;
    if Result and (IDefaultValuesProvider <> nil) then
      Result := IDefaultValuesProvider.DefaultCanModify and IDefaultValuesProvider.CanSetEditMode;
  end;
end;

function TcxCustomEditProperties.CanValidate: Boolean;
begin
  Result := False;
end;

procedure TcxCustomEditProperties.ChangeHandler(Sender: TObject);
begin
  Changed;
end;

procedure TcxCustomEditProperties.FillMinMaxValues(AMinValue, AMaxValue: Double);
begin
  if AssignedValues.MaxValue and (AMaxValue = FMaxValue) and
      AssignedValues.MinValue and (AMinValue = FMinValue) then
    Exit;

  AssignedValues.FMaxValue := True;
  FMaxValue := AMaxValue;
  AssignedValues.FMinValue := True;
  FMinValue := AMinValue;
  Changed;
end;

class function TcxCustomEditProperties.GetAssignedValuesClass: TcxCustomEditPropertiesValuesClass;
begin
  Result := TcxCustomEditPropertiesValues;
end;

function TcxCustomEditProperties.GetDefaultHorzAlignment: TAlignment;
begin
  if IDefaultValuesProvider <> nil then
    Result := IDefaultValuesProvider.DefaultAlignment
  else
    Result := FAlignment.Horz;
end;

function TcxCustomEditProperties.GetDefaultVertAlignment: TcxEditVertAlignment;
begin
  Result := cxEditDefaultVertAlignment;
end;

function TcxCustomEditProperties.GetDisplayFormatOptions: TcxEditDisplayFormatOptions;
begin
  Result := [];
end;

function TcxCustomEditProperties.GetMaxValue: Double;
begin
  if AssignedValues.MaxValue then
    Result := FMaxValue
  else
    if IDefaultValuesProvider = nil then
      Result := 0
    else
      Result := IDefaultValuesProvider.DefaultMaxValue;
end;

function TcxCustomEditProperties.GetMinValue: Double;
begin
  if AssignedValues.MinValue then
    Result := FMinValue
  else
    if IDefaultValuesProvider = nil then
      Result := 0
    else
      Result := IDefaultValuesProvider.DefaultMinValue;
end;

function TcxCustomEditProperties.GetEditValueSource(AEditFocused: Boolean): TcxDataEditValueSource;
begin
  Result := evsValue;
end;

function TcxCustomEditProperties.GetValidateErrorText(AErrorKind: TcxEditErrorKind): string;
begin
  Result := cxGetResourceString(@cxSEditValidateErrorText);
end;

function TcxCustomEditProperties.GetValueEditorEditValueSource(AEditFocused: Boolean): TcxDataEditValueSource;
begin
  Result := evsValue;
  if not AEditFocused and (dfoSupports in DisplayFormatOptions) and (IDefaultValuesProvider <> nil) and
      IDefaultValuesProvider.IsDisplayFormatDefined(not(dfoNoCurrencyValue in DisplayFormatOptions)) then
    Result := evsText;
end;

class function TcxCustomEditProperties.GetViewDataClass: TcxCustomEditViewDataClass;
begin
  Result := TcxCustomEditViewData;
end;

function TcxCustomEditProperties.HasDisplayValue: Boolean;
begin
  Result := False;
end;

function TcxCustomEditProperties.InnerEditNeedsTabs: Boolean;
begin
  Result := False;
end;

function TcxCustomEditProperties.IsDesigning: Boolean;
begin
  Result := (Owner is TComponent) and (csDesigning in TComponent(Owner).ComponentState);
end;

function TcxCustomEditProperties.IsEditValueConversionDependOnFocused: Boolean;
begin
  Result := True;
end;

function TcxCustomEditProperties.IsMaxValueStored: Boolean;
begin
  Result := AssignedValues.MaxValue;
end;

function TcxCustomEditProperties.IsMinValueStored: Boolean;
begin
  Result := AssignedValues.MinValue;
end;

function TcxCustomEditProperties.IsResetEditClass: Boolean;
begin
  Result := False;
end;

function TcxCustomEditProperties.IsValueFormattedByProperties: Boolean;
begin
  Result := False;
end;

function TcxCustomEditProperties.IsValueFormattedByProvider: Boolean;
begin
  Result := not IsValueFormattedByProperties and (dfoSupports in DisplayFormatOptions) and
    (GetEditValueSource(True) = evsValue) and (GetEditValueSource(False) = evsText) and
    (IDefaultValuesProvider <> nil) and
    IDefaultValuesProvider.IsDisplayFormatDefined(not(dfoNoCurrencyValue in DisplayFormatOptions));
end;

procedure TcxCustomEditProperties.LockUpdate(ALock: Boolean);
begin
  FChangedLocked := ALock;
end;

function TcxCustomEditProperties.AllowRepositorySharing: Boolean;
begin
  Result := True;
end;

procedure TcxCustomEditProperties.RefreshNonShareable;
begin
end;

procedure TcxCustomEditProperties.PrepareDisplayValue(const AEditValue: TcxEditValue;
  var DisplayValue: TcxEditValue; AEditFocused: Boolean);
begin
  DisplayValue := AEditValue;
end;

function TcxCustomEditProperties.DefaultUseLeftAlignmentOnEditing: Boolean;
begin
  Result := cxEditDefaultUseLeftAlignmentOnEditing;
end;

procedure TcxCustomEditProperties.DefaultValuesProviderDestroyed;
begin
  FIDefaultValuesProvider := nil;
end;

procedure TcxCustomEditProperties.DoAssign(AProperties: TcxCustomEditProperties);
begin
  Images := AProperties.Images;
  AutoSelect := AProperties.AutoSelect;
  Buttons.Assign(AProperties.Buttons);
  Buttons.ChangeScale(ScaleFactor.TargetDPI, AProperties.ScaleFactor.TargetDPI);
  ButtonsViewStyle := AProperties.ButtonsViewStyle;
  BeepOnError := AProperties.BeepOnError;
  ClearKey := AProperties.ClearKey;
  ClickKey := AProperties.ClickKey;
  FUseLeftAlignmentOnEditing := AProperties.FUseLeftAlignmentOnEditing;
  FIDefaultValuesProvider := AProperties.FIDefaultValuesProvider;
  Alignment := AProperties.Alignment;

  AssignedValues.MaxValue := False;
  if AProperties.AssignedValues.MaxValue then
    MaxValue := AProperties.MaxValue;

  AssignedValues.MinValue := False;
  if AProperties.AssignedValues.MinValue then
    MinValue := AProperties.MinValue;

  AssignedValues.ReadOnly := False;
  if AProperties.AssignedValues.ReadOnly then
    ReadOnly := AProperties.ReadOnly;

  ImmediatePost := AProperties.ImmediatePost;
  Transparent := AProperties.Transparent;
  UseMouseWheel := AProperties.UseMouseWheel;
  ValidateOnEnter := AProperties.ValidateOnEnter;
  ValidationOptions := AProperties.ValidationOptions;
  ValidationErrorIconAlignment := AProperties.ValidationErrorIconAlignment;

  OnButtonClick := AProperties.OnButtonClick;
  OnChange := AProperties.OnChange;
  OnEditValueChanged := AProperties.OnEditValueChanged;
  OnValidate := AProperties.OnValidate;
end;

procedure TcxCustomEditProperties.DoChanged;
begin
  if Assigned(FOnPropertiesChanged) then
  begin
    Inc(FIsChangingCount);
    try
      FOnPropertiesChanged(Self);
    finally
      Dec(FIsChangingCount);
    end;
  end;
end;

procedure TcxCustomEditProperties.DoValidate(
  var ADisplayValue: TcxEditValue; var AErrorText: TCaption;
  var AError: Boolean; AEdit: TcxCustomEdit; out AIsUserErrorDisplayValue: Boolean);
var
  APrevDisplayValue: TcxEditValue;
begin
  AIsUserErrorDisplayValue := False;
  if AEdit.IsOnValidateEventAssigned then
  begin
    APrevDisplayValue := ADisplayValue;
    AEdit.DoOnValidate(ADisplayValue, AErrorText, AError);
    if AError then
    begin
      AIsUserErrorDisplayValue := not VarEquals(APrevDisplayValue, ADisplayValue);
      if not AIsUserErrorDisplayValue then
        ADisplayValue := AEdit.DisplayValue;
    end;
  end;
end;

procedure TcxCustomEditProperties.FreeNotification(Sender: TComponent);
begin
  if Sender = Images then
    Images := nil;
end;

function TcxCustomEditProperties.BaseGetAlignment: TcxEditAlignment;
var
  AOwnerComponent: TComponent;
begin
  FAlignment.OnChanged := nil;
  FAlignment.Assign(FInnerAlignment);
  Result := FAlignment;
  if IsAlignmentStored then
  begin
    AOwnerComponent := GetOwnerComponent(Self);
    if (AOwnerComponent <> nil) and (csWriting in AOwnerComponent.ComponentState) then
      Exit;
  end;

  if not FInnerAlignment.IsHorzStored then
    Result.FHorz := GetDefaultHorzAlignment;
  FAlignment.OnChanged := AlignmentChangedHandler;
end;

function TcxCustomEditProperties.GetImages: TCustomImageList;
begin
  Result := Buttons.Images;
end;

function TcxCustomEditProperties.GetIsChanging: Boolean;
begin
  Result := FIsChangingCount > 0;
end;

function TcxCustomEditProperties.GetReadOnly: Boolean;
begin
  if AssignedValues.ReadOnly then
    Result := FReadOnly
  else
    if IDefaultValuesProvider = nil then
      Result := False
    else
      Result := IDefaultValuesProvider.DefaultReadOnly;
end;

function TcxCustomEditProperties.GetScaleFactor: TdxScaleFactor;
begin
  Result := FScaleFactor;
end;

procedure TcxCustomEditProperties.InitiateActions;
begin
  Buttons.InitiateActions;
end;

function TcxCustomEditProperties.SupportsMultiThreading: Boolean;
begin
  Result := AllowRepositorySharing;
end;

function TcxCustomEditProperties.IsAlignmentStored: Boolean;
begin
  with FInnerAlignment do
    Result := IsHorzStored or IsVertStored;
end;

function TcxCustomEditProperties.IsUseLeftAlignmentOnEditingStored: Boolean;
begin
  Result := FUseLeftAlignmentOnEditing <> DefaultUseLeftAlignmentOnEditing;
end;

function TcxCustomEditProperties.IsReadOnlyStored: Boolean;
begin
  Result := AssignedValues.ReadOnly;
end;

procedure TcxCustomEditProperties.SetAssignedValues(
  Value: TcxCustomEditPropertiesValues);
begin
  FAssignedValues.Assign(Value);
end;

procedure TcxCustomEditProperties.SetAutoSelect(Value: Boolean);
begin
  if Value <> FAutoSelect then
  begin
    FAutoSelect := Value;
    Changed;
  end;
end;

procedure TcxCustomEditProperties.SetButtons(Value: TcxEditButtons);
begin
  FButtons.Assign(Value);
end;

procedure TcxCustomEditProperties.SetButtonsViewStyle(Value: TcxEditButtonsViewStyle);
begin
  if Value <> FButtonsViewStyle then
  begin
    FButtonsViewStyle := Value;
    Changed;
  end;
end;

procedure TcxCustomEditProperties.SetErrorIcon(Value: TdxSmartImage);
begin
  FErrorIcon.Assign(Value);
end;

procedure TcxCustomEditProperties.SetUseLeftAlignmentOnEditing(Value: Boolean);
begin
  if Value <> FUseLeftAlignmentOnEditing then
  begin
    FUseLeftAlignmentOnEditing := Value;
    Changed;
  end;
end;

procedure TcxCustomEditProperties.SetIDefaultValuesProvider(Value: IcxEditDefaultValuesProvider);
begin
  FIDefaultValuesProvider := Value;
  if Owner is TcxCustomEdit then
    Changed;
end;

procedure TcxCustomEditProperties.SetMaxValue(Value: Double);
begin
  if AssignedValues.MaxValue and (Value = FMaxValue) then
    Exit;

  AssignedValues.FMaxValue := True;
  FMaxValue := Value;
  Changed;
end;

procedure TcxCustomEditProperties.SetMinValue(Value: Double);
begin
  if AssignedValues.MinValue and (Value = FMinValue) then
    Exit;

  AssignedValues.FMinValue := True;
  FMinValue := Value;
  Changed;
end;

procedure TcxCustomEditProperties.SetReadOnly(Value: Boolean);
begin
  if AssignedValues.ReadOnly and (Value = FReadOnly) then
    Exit;
  AssignedValues.FReadOnly := True;
  FReadOnly := Value;
  Changed;
end;

procedure TcxCustomEditProperties.SetTransparent(Value: Boolean);
begin
  if Value <> FTransparent then
  begin
    FTransparent := Value;
    Changed;
  end;
end;

procedure TcxCustomEditProperties.SetValidationErrorIconAlignment(AValue: TLeftRight);
begin
  if FValidationErrorIconAlignment <> AValue then
  begin
    FValidationErrorIconAlignment := AValue;
    Changed;
  end;
end;

procedure TcxCustomEditProperties.SetValidationOptions(Value: TcxEditValidationOptions);
begin
  if FValidationOptions <> Value then
  begin
    FValidationOptions := Value;
    FInternalValidationOptions := Value;
    Changed;
  end;
end;

procedure TcxCustomEditProperties.ScaleFactorChangeHandler(Sender: TObject; M, D: Integer; IsLoading: Boolean);
begin
  if not IsLoading then
    ChangeScale(M, D);
end;

procedure TcxCustomEditProperties.ImageListChange(Sender: TObject);
begin
  if Sender = Images then
    Changed;
end;

procedure TcxCustomEditProperties.SetImages(Value: TCustomImageList);
begin
  cxSetImageList(Value, Buttons.FImages, FImageChangeLink, FreeNotificator);
end;

{ TcxCustomEditDefaultValuesProvider }

destructor TcxCustomEditDefaultValuesProvider.Destroy;
begin
  ClearUsers;
  inherited Destroy;
end;

function TcxCustomEditDefaultValuesProvider.CanSetEditMode: Boolean;
begin
  Result := True;
end;

procedure TcxCustomEditDefaultValuesProvider.ClearUsers;
var
  I: Integer;
begin
  if FCreatedEditPropertiesList <> nil then
    for I := 0 to FCreatedEditPropertiesList.Count - 1 do
      with TcxCustomEditProperties(FCreatedEditPropertiesList[I]) do
        if (IDefaultValuesProvider <> nil) and (IDefaultValuesProvider.GetInstance = Self) then
          DefaultValuesProviderDestroyed;
end;

function TcxCustomEditDefaultValuesProvider.DefaultAlignment: TAlignment;
begin
  Result := cxEditDefaultHorzAlignment;
end;

function TcxCustomEditDefaultValuesProvider.DefaultBlobKind: TcxBlobKind;
begin
  Result := bkMemo;
end;

function TcxCustomEditDefaultValuesProvider.DefaultCanModify: Boolean;
begin
  Result := True;
end;

function TcxCustomEditDefaultValuesProvider.DefaultDisplayFormat: string;
begin
  Result := '';
end;

function TcxCustomEditDefaultValuesProvider.DefaultEditFormat: string;
begin
  Result := '';
end;

function TcxCustomEditDefaultValuesProvider.DefaultEditMask: string;
begin
  Result := '';
end;

function TcxCustomEditDefaultValuesProvider.DefaultIsFloatValue: Boolean;
begin
  Result := False;
end;

function TcxCustomEditDefaultValuesProvider.DefaultMaxLength: Integer;
begin
  Result := 0;
end;

function TcxCustomEditDefaultValuesProvider.DefaultMaxValue: Double;
begin
  Result := 0;
end;

function TcxCustomEditDefaultValuesProvider.DefaultMinValue: Double;
begin
  Result := 0;
end;

function TcxCustomEditDefaultValuesProvider.DefaultPrecision: Integer;
begin
  Result := cxEditDefaultPrecision;
end;

function TcxCustomEditDefaultValuesProvider.DefaultReadOnly: Boolean;
begin
  Result := False;
end;

function TcxCustomEditDefaultValuesProvider.DefaultRequired: Boolean;
begin
  Result := False;
end;

function TcxCustomEditDefaultValuesProvider.GetInstance: TObject;
begin
  Result := Self;
end;

function TcxCustomEditDefaultValuesProvider.IsBlob: Boolean;
begin
  Result := False;
end;

function TcxCustomEditDefaultValuesProvider.IsCurrency: Boolean;
begin
  Result := False;
end;

function TcxCustomEditDefaultValuesProvider.IsDataAvailable: Boolean;
begin
  Result := True;
end;

function TcxCustomEditDefaultValuesProvider.IsDataStorage: Boolean;
begin
  Result := False;
end;

function TcxCustomEditDefaultValuesProvider.IsDisplayFormatDefined(
  AIsCurrencyValueAccepted: Boolean): Boolean;
begin
  Result := False;
end;

function TcxCustomEditDefaultValuesProvider.IsOnGetTextAssigned: Boolean;
begin
  Result := False;
end;

function TcxCustomEditDefaultValuesProvider.IsOnSetTextAssigned: Boolean;
begin
  Result := False;
end;

function TcxCustomEditDefaultValuesProvider.IsValidChar(AChar: Char): Boolean;
begin
  Result := True;
end;

{ TcxEditAlignment }

constructor TcxEditAlignment.Create(AOwner: TPersistent);
begin
  Create(AOwner, cxEditDefaultVertAlignment);
end;

constructor TcxEditAlignment.Create(AOwner: TPersistent; ADefaultVertAlignment: TcxEditVertAlignment);
begin
  Create(AOwner, cxEditDefaultHorzAlignment, ADefaultVertAlignment);
end;

constructor TcxEditAlignment.Create(AOwner: TPersistent; ADefaultHorzAlignment: TcxEditHorzAlignment;
  ADefaultVertAlignment: TcxEditVertAlignment);
begin
  inherited Create;
  FOwner := AOwner;
  FDefaultHorzAlignment := ADefaultHorzAlignment;
  FDefaultVertAlignment := ADefaultVertAlignment;
  FHorz := FDefaultHorzAlignment;
  FVert := FDefaultVertAlignment;
end;

procedure TcxEditAlignment.Assign(Source: TPersistent);
begin
  if Source is TcxEditAlignment then
    with Source as TcxEditAlignment do
    begin
      Self.FVert := FVert;
      Self.FHorz := FHorz;
      Self.FIsHorzAssigned := FIsHorzAssigned;
      Self.DoChanged;
    end
  else
    inherited Assign(Source);
end;

function TcxEditAlignment.IsHorzStored: Boolean;
begin
  Result := FIsHorzAssigned or (FHorz <> FDefaultHorzAlignment);
end;

function TcxEditAlignment.IsVertStored: Boolean;
begin
  Result := FVert <> FDefaultVertAlignment;
end;

procedure TcxEditAlignment.Reset;
begin
  FIsHorzAssigned := False;
  DoChanged;
end;

procedure TcxEditAlignment.DoChanged;
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

function TcxEditAlignment.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TcxEditAlignment.SetHorz(const Value: TcxEditHorzAlignment);
begin
  if FIsHorzAssigned and (Value = FHorz) then
    Exit;
  FIsHorzAssigned := True;
  FHorz := Value;
  DoChanged;
end;

procedure TcxEditAlignment.SetVert(const Value: TcxEditVertAlignment);
begin
  if Value <> FVert then
  begin
    FVert := Value;
    DoChanged;
  end;
end;

{ TcxCustomEditPropertiesValues }

constructor TcxCustomEditPropertiesValues.Create(AOwner: TPersistent);
begin
  inherited Create;
  FOwner := AOwner;
end;

procedure TcxCustomEditPropertiesValues.Assign(Source: TPersistent);
begin
  if Source is TcxCustomEditPropertiesValues then
  begin
    BeginUpdate;
    try
      with Source as TcxCustomEditPropertiesValues do
      begin
        Self.MaxValue := MaxValue;
        Self.MinValue := MinValue;
        Self.ReadOnly := ReadOnly;
      end;
    finally
      EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;

procedure TcxCustomEditPropertiesValues.BeginUpdate;
begin
  Properties.BeginUpdate;
end;

procedure TcxCustomEditPropertiesValues.EndUpdate;
begin
  Properties.EndUpdate;
end;

procedure TcxCustomEditPropertiesValues.RestoreDefaults;
begin
  BeginUpdate;
  try
    MaxValue := False;
    MinValue := False;
    ReadOnly := False;
  finally
    EndUpdate;
  end;
end;

function TcxCustomEditPropertiesValues.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TcxCustomEditPropertiesValues.Changed;
begin
  Properties.Changed;
end;

function TcxCustomEditPropertiesValues.IsMaxValueStored: Boolean;
begin
  Result := MaxValue and
    (Properties.MaxValue = 0{TcxCustomEditProperties(Properties).GetDefaultMaxValue}) and
    IsPropertiesPropertyVisible('MaxValue');
end;

function TcxCustomEditPropertiesValues.IsMinValueStored: Boolean;
begin
  Result := MinValue and
    (Properties.MinValue = 0{TcxCustomEditProperties(Properties).GetDefaultMinValue}) and
    IsPropertiesPropertyVisible('MinValue');
end;

function TcxCustomEditPropertiesValues.IsPropertiesPropertyVisible(
  const APropertyName: string): Boolean;
begin
  Result := TypInfo.GetPropInfo(Properties, APropertyName) <> nil;
end;

function TcxCustomEditPropertiesValues.GetProperties: TcxCustomEditProperties;
begin
  Result := TcxCustomEditProperties(FOwner);
end;

procedure TcxCustomEditPropertiesValues.SetMaxValue(Value: Boolean);
begin
  if Value <> FMaxValue then
  begin
    FMaxValue := Value;
    Changed;
  end;
end;

procedure TcxCustomEditPropertiesValues.SetMinValue(Value: Boolean);
begin
  if Value <> FMinValue then
  begin
    FMinValue := Value;
    Changed;
  end;
end;

procedure TcxCustomEditPropertiesValues.SetReadOnly(Value: Boolean);
begin
  if Value <> FReadOnly then
  begin
    FReadOnly := Value;
    Changed;
  end;
end;

{ TcxEditStyleController }

function TcxEditStyleController.GetStyleClass: TcxContainerStyleClass;
begin
  Result := TcxEditStyle;
end;

function TcxEditStyleController.GetInternalStyle(AState: TcxContainerStateItem): TcxCustomEditStyle;
begin
  Result := TcxCustomEditStyle(FStyles[AState]);
end;

function TcxEditStyleController.GetStyle: TcxEditStyle;
begin
  Result := TcxEditStyle(FStyles[csNormal]);
end;

function TcxEditStyleController.GetStyleDisabled: TcxEditStyle;
begin
  Result := TcxEditStyle(FStyles[csDisabled]);
end;

function TcxEditStyleController.GetStyleFocused: TcxEditStyle;
begin
  Result := TcxEditStyle(FStyles[csActive]);
end;

function TcxEditStyleController.GetStyleHot: TcxEditStyle;
begin
  Result := TcxEditStyle(FStyles[csHotTrack]);
end;

procedure TcxEditStyleController.SetInternalStyle(AState: TcxContainerStateItem;
  Value: TcxCustomEditStyle);
begin
  FStyles[AState] := Value;
end;

procedure TcxEditStyleController.SetStyle(Value: TcxEditStyle);
begin
  FStyles[csNormal] := Value;
end;

procedure TcxEditStyleController.SetStyleDisabled(Value: TcxEditStyle);
begin
  FStyles[csDisabled] := Value;
end;

procedure TcxEditStyleController.SetStyleFocused(Value: TcxEditStyle);
begin
  FStyles[csActive] := Value;
end;

procedure TcxEditStyleController.SetStyleHot(Value: TcxEditStyle);
begin
  FStyles[csHotTrack] := Value;
end;

{ TcxCustomEditStyle }

constructor TcxCustomEditStyle.Create(AOwner: TPersistent;
  ADirectAccessMode: Boolean; AParentStyle: TcxContainerStyle = nil;
  AState: TcxContainerStateItem = csNormal);
begin
  inherited Create(AOwner, ADirectAccessMode, AParentStyle, AState);
  FPopupCloseButton := True;
end;

procedure TcxCustomEditStyle.Assign(Source: TPersistent);
begin
  if Source is TcxCustomEditStyle then
  begin
    BeginUpdate;
    try
      with Source as TcxCustomEditStyle do
      begin
        Self.FButtonStyle := FButtonStyle;
        Self.FButtonTransparency := FButtonTransparency;
        Self.FGradient := FGradient;
        Self.FGradientButtons := FGradientButtons;
        Self.FGradientDirection := FGradientDirection;
        Self.FPopupBorderStyle := FPopupBorderStyle;
        Self.FPopupCloseButton := FPopupCloseButton;
      end;
      inherited Assign(Source);
    finally
      Changed;
      EndUpdate;
    end
  end
  else
    inherited Assign(Source);
end;

function TcxCustomEditStyle.GetStyleValueCount: Integer;
begin
  Result := cxEditStyleValueCount;
end;

function TcxCustomEditStyle.GetStyleValueName(AStyleValue: TcxEditStyleValue;
  out StyleValueName: string): Boolean;
begin
  Result := inherited GetStyleValueName(AStyleValue, StyleValueName);
  if not Result then
  begin
    Result := AStyleValue < cxEditStyleValueCount;
    if Result then
      StyleValueName := cxEditStyleValueNameA[AStyleValue - cxContainerStyleValueCount];
  end;
end;

function TcxCustomEditStyle.IsValueAssigned(AValue: TcxEditStyleValue): Boolean;
var
  AButtonStyle: TcxEditButtonStyle;
  AButtonTransparency: TcxEditButtonTransparency;
  AGradientDirection: TcxEditGradientDirection;
  APopupBorderStyle: TcxEditPopupBorderStyle;
  ATempBool: Boolean;
begin
  case AValue of
    svButtonStyle:
      Result := InternalGetButtonStyle(AButtonStyle);
    svButtonTransparency:
      Result := InternalGetButtonTransparency(AButtonTransparency);
    svGradient:
      Result := InternalGetGradient(ATempBool);
    svGradientButtons:
      Result := InternalGetGradientButtons(ATempBool);
    svGradientDirection:
      Result := InternalGetGradientDirection(AGradientDirection);
    svPopupBorderStyle:
      Result := InternalGetPopupBorderStyle(APopupBorderStyle);
    else
      Result := inherited IsValueAssigned(AValue);
  end;
end;

procedure TcxCustomEditStyle.Init(AParams: TcxViewParams);
begin
  BeginUpdate;
  try
    Color := AParams.Color;
    Font := AParams.Font;
    TextColor := AParams.TextColor;
  finally
    EndUpdate;
  end;
end;

function TcxCustomEditStyle.GetAssignedValues: TcxEditStyleValues;
begin
  Result := TcxEditStyleValues(inherited AssignedValues);
end;

function TcxCustomEditStyle.GetBaseStyle: TcxCustomEditStyle;
begin
  Result := TcxCustomEditStyle(inherited BaseStyle);
end;

function TcxCustomEditStyle.GetDefaultStyleController: TcxStyleController;
var
  AEdit: TcxCustomEdit;
begin
  Result := nil;
  if not DirectAccessMode then
  begin
    AEdit := Edit;
    if not((AEdit <> nil) and AEdit.IsInplace) then
      Result := DefaultEditStyleController;
  end;
end;

function TcxCustomEditStyle.InternalGetNotPublishedExtendedStyleValues: TcxEditStyleValues;
begin
  Result := [svButtonTransparency, svEdges, svFont, svGradient,
    svGradientButtons, svGradientDirection, svHotTrack, svPopupBorderStyle,
    svShadow, svTransparentBorder];
end;

function TcxCustomEditStyle.DefaultButtonStyle: TcxEditButtonStyle;
begin
  if IsBaseStyle then
    Result := btsDefault
  else
    Result := TcxCustomEditStyle(ParentStyle).ButtonStyle;
end;

function TcxCustomEditStyle.DefaultButtonTransparency: TcxEditButtonTransparency;
begin
  if IsBaseStyle then
    Result := ebtNone
  else
    Result := TcxCustomEditStyle(ParentStyle).ButtonTransparency;
end;

function TcxCustomEditStyle.DefaultGradient: Boolean;
begin
  if IsBaseStyle then
    Result := False
  else
    Result := TcxCustomEditStyle(ParentStyle).Gradient;
end;

function TcxCustomEditStyle.DefaultGradientButtons: Boolean;
begin
  if IsBaseStyle then
    Result := False
  else
    Result := TcxCustomEditStyle(ParentStyle).GradientButtons;
end;

function TcxCustomEditStyle.DefaultGradientDirection: TcxEditGradientDirection;
begin
  if IsBaseStyle then
    Result := dirDown
  else
    Result := TcxCustomEditStyle(ParentStyle).GradientDirection;
end;

function TcxCustomEditStyle.DefaultPopupBorderStyle: TcxEditPopupBorderStyle;
begin
  if IsBaseStyle then
    Result := epbsDefault
  else
    Result := TcxCustomEditStyle(ParentStyle).PopupBorderStyle;
end;

function TcxCustomEditStyle.GetActiveStyleController: TcxEditStyleController;
begin
  Result := TcxEditStyleController(inherited ActiveStyleController);
end;

function TcxCustomEditStyle.GetBorderStyle: TcxEditBorderStyle;
begin
  Result := TcxEditBorderStyle(inherited BorderStyle);
end;

function TcxCustomEditStyle.GetButtonStyle: TcxEditButtonStyle;
begin
  if DirectAccessMode then
    if svButtonStyle in FAssignedValues then
      Result := FButtonStyle
    else
      Result := DefaultButtonStyle
  else
    if not InternalGetButtonStyle(Result) then
      Result := DefaultButtonStyle;
end;

function TcxCustomEditStyle.GetButtonTransparency: TcxEditButtonTransparency;
begin
  if DirectAccessMode then
    if svButtonTransparency in FAssignedValues then
      Result := FButtonTransparency
    else
      Result := DefaultButtonTransparency
  else
    if not InternalGetButtonTransparency(Result) then
      Result := DefaultButtonTransparency;
end;

function TcxCustomEditStyle.GetEdit: TcxCustomEdit;
var
  AOwner: TPersistent;
begin
  AOwner := GetOwner;
  if AOwner is TcxCustomEdit then
    Result := TcxCustomEdit(AOwner)
  else
    Result := nil;
end;

function TcxCustomEditStyle.GetGradient: Boolean;
begin
  if DirectAccessMode then
    if svGradient in FAssignedValues then
      Result := FGradient
    else
      Result := DefaultGradient
  else
    if not InternalGetGradient(Result) then
      Result := DefaultGradient;
end;

function TcxCustomEditStyle.GetGradientButtons: Boolean;
begin
  if DirectAccessMode then
    if svGradientButtons in FAssignedValues then
      Result := FGradientButtons
    else
      Result := DefaultGradientButtons
  else
    if not InternalGetGradientButtons(Result) then
      Result := DefaultGradientButtons;
end;

function TcxCustomEditStyle.GetGradientDirection: TcxEditGradientDirection;
begin
  if DirectAccessMode then
    if svGradientDirection in FAssignedValues then
      Result := FGradientDirection
    else
      Result := DefaultGradientDirection
  else
    if not InternalGetGradientDirection(Result) then
      Result := DefaultGradientDirection;
end;

function TcxCustomEditStyle.GetPopupBorderStyle: TcxEditPopupBorderStyle;
begin
  if DirectAccessMode then
    if svPopupBorderStyle in FAssignedValues then
      Result := FPopupBorderStyle
    else
      Result := DefaultPopupBorderStyle
  else
    if not InternalGetPopupBorderStyle(Result) then
      Result := DefaultPopupBorderStyle;
end;

function TcxCustomEditStyle.GetStyleController: TcxEditStyleController;
begin
  Result := TcxEditStyleController(BaseGetStyleController);
end;

function TcxCustomEditStyle.GetPopupCloseButton: Boolean;
begin
  if IsBaseStyle then
    Result := FPopupCloseButton
  else
    Result := TcxCustomEditStyle(ParentStyle).PopupCloseButton;
end;

function TcxCustomEditStyle.InternalGetButtonStyle(var ButtonStyle: TcxEditButtonStyle): Boolean;
begin
  Result := svButtonStyle in FAssignedValues;
  if Result then
    ButtonStyle := FButtonStyle
  else
    if ActiveStyleController <> nil then
      Result := ActiveStyleController.Styles[TcxContainerStateItem(State)].InternalGetButtonStyle(ButtonStyle);
end;

function TcxCustomEditStyle.InternalGetButtonTransparency(var ButtonTransparency: TcxEditButtonTransparency): Boolean;
begin
  Result := svButtonTransparency in FAssignedValues;
  if Result then
    ButtonTransparency := FButtonTransparency
  else
    if ActiveStyleController <> nil then
      Result := ActiveStyleController.Styles[TcxContainerStateItem(State)].InternalGetButtonTransparency(ButtonTransparency);
end;

function TcxCustomEditStyle.InternalGetGradient(var Gradient: Boolean): Boolean;
begin
  Result := svGradient in FAssignedValues;
  if Result then
    Gradient := FGradient
  else
    if ActiveStyleController <> nil then
      Result := ActiveStyleController.Styles[TcxContainerStateItem(State)].InternalGetGradient(Gradient);
end;

function TcxCustomEditStyle.InternalGetGradientButtons(var GradientButtons: Boolean): Boolean;
begin
  Result := svGradientButtons in FAssignedValues;
  if Result then
    GradientButtons := FGradientButtons
  else
    if ActiveStyleController <> nil then
      Result := ActiveStyleController.Styles[TcxContainerStateItem(State)].InternalGetGradientButtons(GradientButtons);
end;

function TcxCustomEditStyle.InternalGetGradientDirection(
  var GradientDirection: TcxEditGradientDirection): Boolean;
begin
  Result := svGradientDirection in FAssignedValues;
  if Result then
    GradientDirection := FGradientDirection
  else
    if ActiveStyleController <> nil then
      Result := ActiveStyleController.Styles[TcxContainerStateItem(State)].InternalGetGradientDirection(GradientDirection);
end;

function TcxCustomEditStyle.InternalGetPopupBorderStyle(var PopupBorderStyle:
  TcxEditPopupBorderStyle): Boolean;
begin
  Result := svPopupBorderStyle in FAssignedValues;
  if Result then
    PopupBorderStyle := FPopupBorderStyle
  else
    if ActiveStyleController <> nil then
      Result := ActiveStyleController.Styles[TcxContainerStateItem(State)].InternalGetPopupBorderStyle(PopupBorderStyle);
end;

function TcxCustomEditStyle.IsBorderStyleStored: Boolean;
begin
  Result := (svBorderStyle in FAssignedValues) and ((Edit = nil) or
    Edit.IsStylePropertyPublished('BorderStyle', State <> csNormal));
end;

function TcxCustomEditStyle.IsButtonStyleStored: Boolean;
begin
  Result := (svButtonStyle in FAssignedValues) and ((Edit = nil) or
    Edit.IsStylePropertyPublished('ButtonStyle', State <> csNormal));
end;

function TcxCustomEditStyle.IsButtonTransparencyStored: Boolean;
begin
  Result := (svButtonTransparency in FAssignedValues) and ((Edit = nil) or
    Edit.IsStylePropertyPublished('ButtonTransparency', State <> csNormal));
end;

function TcxCustomEditStyle.IsGradientStored: Boolean;
begin
  Result := (svGradient in FAssignedValues) and ((Edit = nil) or
    Edit.IsStylePropertyPublished('Gradient', State <> csNormal));
end;

function TcxCustomEditStyle.IsGradientButtonsStored: Boolean;
begin
  Result := (svGradientButtons in FAssignedValues) and ((Edit = nil) or
    Edit.IsStylePropertyPublished('GradientButtons', State <> csNormal));
end;

function TcxCustomEditStyle.IsGradientDirectionStored: Boolean;
begin
  Result := (svGradientDirection in FAssignedValues) and ((Edit = nil) or
    Edit.IsStylePropertyPublished('GradientDirection', State <> csNormal));
end;

function TcxCustomEditStyle.IsPopupBorderStyleStored: Boolean;
begin
  Result := (svPopupBorderStyle in FAssignedValues) and ((Edit = nil) or
    Edit.IsStylePropertyPublished('PopupBorderStyle', State <> csNormal));
end;

function TcxCustomEditStyle.IsStyleControllerStored: Boolean;
begin
  Result := State = csNormal;
end;

procedure TcxCustomEditStyle.SetAssignedValues(Value: TcxEditStyleValues);
begin
  inherited AssignedValues := Value;
end;

procedure TcxCustomEditStyle.SetBorderStyle(Value: TcxEditBorderStyle);
begin
  inherited BorderStyle := TcxContainerBorderStyle(Value);
end;

procedure TcxCustomEditStyle.SetButtonStyle(Value: TcxEditButtonStyle);
begin
  if (svButtonStyle in FAssignedValues) and (Value = FButtonStyle) then
    Exit;
  FButtonStyle := Value;
  Include(FAssignedValues, svButtonStyle);
  Changed;
end;

procedure TcxCustomEditStyle.SetButtonTransparency(Value: TcxEditButtonTransparency);
begin
  if IsBaseStyle then
  begin
    if (svButtonTransparency in FAssignedValues) and (Value = FButtonTransparency) then
      Exit;
    FButtonTransparency := Value;
    Include(FAssignedValues, svButtonTransparency);
    Changed;
  end
  else
    TcxCustomEditStyle(ParentStyle).ButtonTransparency := Value;
end;

procedure TcxCustomEditStyle.SetGradient(Value: Boolean);
begin
  if IsBaseStyle then
  begin
    if (svGradient in FAssignedValues) and (Value = FGradient) then
      Exit;
    FGradient := Value;
    Include(FAssignedValues, svGradient);
    Changed;
  end
  else
    TcxCustomEditStyle(ParentStyle).Gradient := Value;
end;

procedure TcxCustomEditStyle.SetGradientButtons(Value: Boolean);
begin
  if IsBaseStyle then
  begin
    if (svGradientButtons in FAssignedValues) and (Value = FGradientButtons) then
      Exit;
    FGradientButtons := Value;
    Include(FAssignedValues, svGradientButtons);
    Changed;
  end
  else
    TcxCustomEditStyle(ParentStyle).GradientButtons := Value;
end;

procedure TcxCustomEditStyle.SetGradientDirection(Value: TcxEditGradientDirection);
begin
  if IsBaseStyle then
  begin
    if (svGradientDirection in FAssignedValues) and (Value = FGradientDirection) then
      Exit;
    FGradientDirection := Value;
    Include(FAssignedValues, svGradientDirection);
    Changed;
  end
  else
    TcxCustomEditStyle(ParentStyle).GradientDirection := Value;
end;

procedure TcxCustomEditStyle.SetPopupBorderStyle(Value: TcxEditPopupBorderStyle);
begin
  if IsBaseStyle then
  begin
    if (svPopupBorderStyle in FAssignedValues) and (Value = FPopupBorderStyle) then
      Exit;
    FPopupBorderStyle := Value;
    Include(FAssignedValues, svPopupBorderStyle);
    Changed;
  end
  else
    TcxCustomEditStyle(ParentStyle).PopupBorderStyle := Value;
end;

procedure TcxCustomEditStyle.SetPopupCloseButton(Value: Boolean);
begin
  if IsBaseStyle then
  begin
    if Value <> FPopupCloseButton then
    begin
      FPopupCloseButton := Value;
      Changed;
    end;
  end
  else
    TcxCustomEditStyle(ParentStyle).PopupCloseButton := Value;
end;

procedure TcxCustomEditStyle.SetStyleController(Value: TcxEditStyleController);
begin
  BaseSetStyleController(Value);
end;


{ TcxInplaceEditList }

constructor TcxInplaceEditList.Create(AEditorOwner: TComponent);
begin
  inherited Create;
  if FInplaceEditLists = nil then
    FInplaceEditLists := TList.Create;
  FInplaceEditLists.Add(Self);
  FEditorOwner := AEditorOwner;
end;

destructor TcxInplaceEditList.Destroy;
begin
  FInplaceEditLists.Remove(Self);
  if FInplaceEditLists.Count = 0 then
    FreeAndNil(FInplaceEditLists);
  DestroyItems;
  inherited Destroy;
end;

procedure TcxInplaceEditList.DisconnectProperties(
  AProperties: TcxCustomEditProperties);
var
  AItemIndex: Integer;
begin
  AItemIndex := FindItem(AProperties, False);
  if AItemIndex <> -1 then
    FItems[AItemIndex].Properties := nil;
end;

function TcxInplaceEditList.FindEdit(AProperties: TcxCustomEditProperties): TcxCustomEdit;
begin
  Result := GetEdit(FindItem(AProperties, True));
end;

function TcxInplaceEditList.FindEdit(APropertiesClass: TcxCustomEditPropertiesClass): TcxCustomEdit;
begin
  Result := GetEdit(FindItem(APropertiesClass));
end;

function TcxInplaceEditList.GetEdit(AProperties: TcxCustomEditProperties): TcxCustomEdit;
begin
  Result := FindEdit(AProperties);
  if Result = nil then
  begin
    Result := CreateEdit(TcxCustomEditPropertiesClass(AProperties.ClassType));
    SetLength(FItems, Count + 1);
    with FItems[Count - 1] do
    begin
      Edit := Result;
      Properties := AProperties;
    end;
  end;
  InitEdit(Result, AProperties);
end;

function TcxInplaceEditList.GetEdit(APropertiesClass: TcxCustomEditPropertiesClass): TcxCustomEdit;
begin
  Result := FindEdit(APropertiesClass);
  if Result = nil then
  begin
    Result := CreateEdit(APropertiesClass);
    SetLength(FItems, Count + 1);
    with FItems[Count - 1] do
    begin
      Edit := Result;
      Properties := nil;
    end;
  end;
end;

procedure TcxInplaceEditList.RemoveItem(AProperties: TcxCustomEditProperties);
begin
  RemoveItem(FindItem(AProperties, False));
end;

procedure TcxInplaceEditList.RemoveItem(APropertiesClass: TcxCustomEditPropertiesClass);
var
  AItemIndex: Integer;
begin
  repeat
    AItemIndex := FindItem(APropertiesClass);
    RemoveItem(AItemIndex);
  until AItemIndex = -1;
end;

function TcxInplaceEditList.CreateEdit(APropertiesClass: TcxCustomEditPropertiesClass): TcxCustomEdit;
begin
  Result := TcxCustomEditClass(APropertiesClass.GetContainerClass).Create(EditorOwner, True);
  Result.Visible := False;
end;

procedure TcxInplaceEditList.DestroyItems;
begin
  while Count <> 0 do
    RemoveItem(0);
end;

function TcxInplaceEditList.FindItem(AProperties: TcxCustomEditProperties;
  ACanUseFreeEditors: Boolean): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Count - 1 do
    if FItems[I].Properties = AProperties then
    begin
      Result := I;
      Break;
    end;
  if (Result = -1) and ACanUseFreeEditors then
  begin
    Result := FindItem(TcxCustomEditPropertiesClass(AProperties.ClassType));
    if Result <> -1 then
      FItems[Result].Properties := AProperties;
  end;
end;

function TcxInplaceEditList.FindItem(APropertiesClass: TcxCustomEditPropertiesClass): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Count - 1 do
    if (FItems[I].Properties = nil) and (FItems[I].Edit.ClassType = APropertiesClass.GetContainerClass) then
    begin
      Result := I;
      Break;
    end;
end;

function TcxInplaceEditList.GetCount: Integer;
begin
  Result := Length(FItems);
end;

function TcxInplaceEditList.GetEdit(AItemIndex: Integer): TcxCustomEdit;
begin
  if AItemIndex <> -1 then
    Result := FItems[AItemIndex].Edit
  else
    Result := nil;
end;

procedure TcxInplaceEditList.InitEdit(AEdit: TcxCustomEdit; AProperties: TcxCustomEditProperties);
begin
  AEdit.LockChangeEvents(True);
  AEdit.FIsInplaceInitializing := True;
  try
    AEdit.Properties := AProperties;
  finally
    AEdit.FIsInplaceInitializing := False;
    AEdit.LockChangeEvents(False, False);
  end;
end;

procedure TcxInplaceEditList.RemoveItem(AIndex: Integer);
begin
  if AIndex <> -1 then
  begin
    FItems[AIndex].Edit.Parent := nil;
    FItems[AIndex].Edit.Free;
    if AIndex < Count - 1 then
      Move(FItems[AIndex + 1], FItems[AIndex], SizeOf(TcxEditListItem) * (Count - AIndex - 1));
    SetLength(FItems, Length(FItems) - 1);
  end;
end;


{ TcxEditValidateInfo }

constructor TcxEditValidateInfo.Create;
begin
  inherited Create;
  FErrorIcon := TdxSmartGlyph.Create;
end;

destructor TcxEditValidateInfo.Destroy;
begin
  FreeAndNil(FErrorIcon);
  inherited Destroy;
end;

function TcxEditValidateInfo.IsError: Boolean;
begin
  Result := ErrorType <> eetNone;
end;

procedure TcxEditValidateInfo.Reset;
begin
  ErrorType := eetNone;
  ErrorText := '';
  ErrorIcon := nil;
end;

function TcxEditValidateInfo.GetErrorIcon: TdxSmartGlyph;
begin
  Result := FErrorIcon;
end;

procedure TcxEditValidateInfo.SetErrorType(AValue: TcxEditErrorType);
begin
  if ErrorType <> AValue then
  begin
    FErrorType := AValue;
    case FErrorType of
      eetError:
        ErrorIcon.Assign(cxEditErrorIcon);
      eetWarning:
        ErrorIcon.Assign(cxEditWarningIcon);
      eetInfo:
        ErrorIcon.Assign(cxEditInfoIcon);
    end;
    FErrorIcon.SourceDPI := dxDefaultDPI;
  end;
end;

procedure TcxEditValidateInfo.SetErrorIcon(AValue: TdxSmartGlyph);
begin
  FErrorIcon.Assign(AValue);
end;

{ TcxCustomEditViewInfo }

constructor TcxCustomEditViewInfo.Create;
begin
  inherited Create;
  LastSelectedButton := -1;
  LastPressedButton := -1;
  SelectedButton := -1;
  PressedButton := -1;

  FErrorData := TcxEditValidateInfo.Create;
  FScaleFactor := TdxScaleFactor.Create;
end;

destructor TcxCustomEditViewInfo.Destroy;
begin
  FreeAndNil(FScaleFactor);
  FreeAndNil(FErrorData);
  SetButtonCount(0);
  inherited Destroy;
end;

procedure TcxCustomEditViewInfo.Assign(Source: TObject);
var
  I: Integer;
  ASource: TcxCustomEditViewInfo;
begin
  if Source is TcxCustomEditViewInfo then
  begin
    ASource := TcxCustomEditViewInfo(Source);
    BackgroundColor := ASource.BackgroundColor;
    BorderColor := ASource.BorderColor;
    BorderStyle := ASource.BorderStyle;
    ContainerState := ASource.ContainerState;
    NativeState := ASource.NativeState;
    SetButtonCount(Length(ASource.ButtonsInfo));
    for I := 0 to Length(ASource.ButtonsInfo) - 1 do
      ButtonsInfo[I].Assign(ASource.ButtonsInfo[I]);
    PressedButton := ASource.PressedButton;
    SelectedButton := ASource.SelectedButton;
  end;
  inherited Assign(Source);
end;

procedure TcxCustomEditViewInfo.CalculateEditorBounds;
begin
  FEditorBounds := Bounds;
  if not FOnCalculateEditorBounds.Empty then
    FOnCalculateEditorBounds.Invoke(Self, FEditorBounds);
end;

procedure TcxCustomEditViewInfo.CalculateNativeInfo(
  out AThemedObjectType: TdxThemedObjectType; out ANativePart: Integer);
begin
  AThemedObjectType := totEdit;
  if not IsWinVistaOrLater then
    ANativePart := EP_EDITTEXT
  else
    if BackgroundPaintingStyle = bpsComboListEdit then
    begin
      AThemedObjectType := totButton;
      ANativePart := BP_PUSHBUTTON;
    end
    else
      ANativePart := EP_EDITBORDER_NOSCROLL;
end;

function TcxCustomEditViewInfo.GetUpdateRegion(AViewInfo: TcxContainerViewInfo): TcxRegion;
var
  I: Integer;
  AEquals: Boolean;
  AEditViewInfo: TcxCustomEditViewInfo;
begin
  Result := inherited GetUpdateRegion(AViewInfo);
  if not(AViewInfo is TcxCustomEditViewInfo) then
    Exit;
  AEditViewInfo := TcxCustomEditViewInfo(AViewInfo);
  AEquals := (BorderColor = AEditViewInfo.BorderColor) or (BorderStyle = ebsNone);
  AEquals := AEquals and (BorderStyle = AEditViewInfo.BorderStyle);
  AEquals := AEquals and (Length(ButtonsInfo) = Length(AEditViewInfo.ButtonsInfo));
  AEquals := AEquals and (NativeState = AEditViewInfo.NativeState);
  if not AEquals then
  begin
    if not IsRectEmpty(Bounds) then
      Result.Combine(Bounds, roAdd);
    Exit;
  end;
  for I := 0 to Length(Self.ButtonsInfo) - 1 do
    Result.Combine(ButtonsInfo[I].GetUpdateRegion(AEditViewInfo.ButtonsInfo[I]), roAdd);
end;

function TcxCustomEditViewInfo.GetCurrentCursor(const AMousePos: TPoint): TCursor;
begin
  Result := crDefault;
end;

procedure TcxCustomEditViewInfo.InplaceMouseDown(AButton: TMouseButton; const AShift: TShiftState; X, Y: Integer);
begin
end;

procedure TcxCustomEditViewInfo.Offset(DX, DY: Integer);
var
  I: Integer;
begin
  inherited Offset(DX, DY);
  OffsetRect(InnerEditRect, DX, DY);
  OffsetRect(ShadowRect, DX, DY);
  OffsetRect(FEditorBounds, DX, DY);
  OffsetRect(FErrorBounds, DX, DY);
  for I := 0 to Length(ButtonsInfo) - 1 do
    with ButtonsInfo[I] do
    begin
      OffsetRect(Bounds, DX, DY);
      OffsetRect(VisibleBounds, DX, DY);
    end;
end;

function TcxCustomEditViewInfo.CanDrawEditValue: Boolean;
begin
  Result := True;
  if not FOnCanDrawEditValue.Empty then
    FOnCanDrawEditValue.Invoke(Self, Result);
end;

function TcxCustomEditViewInfo.DrawBackground(ACanvas: TcxCanvas): Boolean;
begin
  Result := IsInplace and DoDrawBackground(ACanvas);
end;

function TcxCustomEditViewInfo.DrawBackground(ACanvas: TcxCanvas; const APos: TPoint): Boolean;
var
  APrevWindowOrg: TPoint;
begin
  APrevWindowOrg := ACanvas.WindowOrg;
  ACanvas.WindowOrg := Point(APrevWindowOrg.X + APos.X, APrevWindowOrg.Y + APos.Y);
  try
    Result := DrawBackground(ACanvas);
  finally
    ACanvas.WindowOrg := APrevWindowOrg;
  end;
end;

procedure TcxCustomEditViewInfo.DrawCustomEdit(ACanvas: TcxCanvas; ACanDrawBackground, ACanDrawValidationMark: Boolean);
begin
  if not IsInplace and (Edit <> nil) then
  begin
    if IsBackgroundPartiallyTransparent or dxFader.Contains(Edit) then
      cxDrawTransparentControlBackground(Edit, ACanvas, Bounds);
  end;
  if not dxFader.DrawFadeImage(Edit, ACanvas.Handle, Bounds) then
    DrawCustomEditBackground(ACanvas, Self, ACanDrawBackground);
  DoAfterDrawBackground(ACanvas);
  if ACanDrawValidationMark then
    DrawCustomEditValidationMark(ACanvas);

  DrawButtons(ACanvas);
end;

procedure TcxCustomEditViewInfo.DrawCustomEditValidationMark(ACanvas: TcxCanvas);
var
  ASize: TSize;
begin
  if (ErrorData.ErrorType <> eetNone) and IsImageAssigned(ErrorData.ErrorIcon) then
  begin
    ASize := dxGetImageSize(ErrorData.ErrorIcon, ScaleFactor);
    ASize.cx := Min(ASize.cx, cxRectWidth(FErrorBounds));
    ASize.cy := Min(ASize.cy, cxRectHeight(FErrorBounds));
    cxDrawImage(ACanvas.Handle, cxRectCenter(FErrorBounds, ASize),
      FErrorBounds, ErrorData.ErrorIcon, nil, -1, idmNormal, True);
  end;
end;

procedure TcxCustomEditViewInfo.DrawButton(ACanvas: TcxCanvas; AButtonVisibleIndex: Integer);
var
  APrevClipRegion: TcxRegion;
  AButtonViewInfo: TcxEditButtonViewInfo;
begin
  AButtonViewInfo := ButtonsInfo[AButtonVisibleIndex];
  AButtonViewInfo.Data.BackgroundColor := BackgroundColor;
  if not IsRectEmpty(AButtonViewInfo.VisibleBounds) then
  begin
    APrevClipRegion := nil;
    try
      if (AButtonViewInfo.Bounds.Left < BorderRect.Left) or
        (AButtonViewInfo.Bounds.Right > BorderRect.Right) or
        (AButtonViewInfo.Bounds.Top < BorderRect.Top) or
        (AButtonViewInfo.Bounds.Bottom > BorderRect.Bottom) then
      begin
        APrevClipRegion := ACanvas.GetClipRegion;
        ACanvas.IntersectClipRect(AButtonViewInfo.VisibleBounds);
      end;
      if not DoDrawButton(ACanvas, AButtonViewInfo) then
        DrawEditButton(ACanvas, AButtonVisibleIndex);
    finally
      if APrevClipRegion <> nil then
        ACanvas.SetClipRegion(APrevClipRegion, roSet);
    end;
  end;
end;

procedure TcxCustomEditViewInfo.DrawButtons(ACanvas: TcxCanvas);
var
  I: Integer;
begin
  if (evsPaintButtons in FState) or ((Edit <> nil) and (Edit.IsButtonDC(ACanvas.Handle))) then
    Exit;
  Include(FState, evsPaintButtons);
  try
    for I := 0 to Length(ButtonsInfo) - 1 do
      DrawButton(ACanvas, I);
  finally
    Exclude(FState, evsPaintButtons)
  end;
end;

procedure TcxCustomEditViewInfo.DrawEditBackground(ACanvas: TcxCanvas; ARect, AGlyphRect: TRect; AGlyphTransparent: Boolean);
begin
  ACanvas.SaveDC;
  try
    if not AGlyphTransparent then
      ACanvas.SetClipRegion(TcxRegion.Create(AGlyphRect), roSubtract);
    case GetDrawBackgroundStyle of
      dbsSimpleParent:
        cxDrawTransparentControlBackground(Edit, ACanvas, ARect);
      dbsThemeParent:
        cxDrawThemeParentBackground(Edit, ACanvas, ARect);
      dbsCustomEdit:
        DrawCustomEdit(ACanvas, True, False);
      dbsCustom:
        begin
          ACanvas.SetClipRegion(TcxRegion.Create(ARect), roIntersect);
          DrawBackground(ACanvas);
        end;
      dbsSimpleFill:
        cxEditFillRect(ACanvas, ARect, BackgroundColor);
    end;
  finally
    ACanvas.RestoreDC;
  end;
end;

procedure TcxCustomEditViewInfo.DrawButtonBackground(ACanvas: TcxCanvas;
  AViewInfo: TcxEditButtonViewInfo; const ARect, AContentRect: TRect; ABrushColor: TColor);
begin
  if not DoDrawButtonBackground(ACanvas, ARect, AViewInfo) then
  begin
    if AViewInfo.Data.NativeStyle then
      DrawNativeButtonBackground(ACanvas, AViewInfo, ARect)
    else
      if UseSkins then
      begin
        Painter.DrawScaledEditorButton(ACanvas, AContentRect,
          EditBtnKindToEditBtnPainterKind[AViewInfo.Data.Kind],
          EditBtnStateToButtonState[AViewInfo.Data.State],
          AViewInfo.ScaleFactor,
          EditBtnPositionMap[AViewInfo.Data.LeftAlignment]);
      end
      else
        DrawUsualButtonBackground(ACanvas, AViewInfo, ARect, ABrushColor);
  end;
end;

procedure TcxCustomEditViewInfo.DrawButtonBorderByPainter(
  AButtonViewInfo: TcxEditButtonViewInfo; var ARect: TRect; out AContentRect: TRect;
  var APenColor, ABrushColor: TColor);
const
  ButtonColorsMap: array[Boolean] of TColor = (clWindow, clBtnFace);
begin
  AContentRect := ARect;
  GetColorSettingsByPainter(ABrushColor, APenColor);
  if ABrushColor = clDefault then
  begin
    if Edit = nil then
      ABrushColor := ButtonColorsMap[AButtonViewInfo.Data.State = ebsDisabled]
    else
      ABrushColor := FEdit.GetBackgroundColor;
  end;
  if APenColor = clDefault then
    APenColor := clBtnText;
end;

procedure TcxCustomEditViewInfo.DrawButtonBorder(ACanvas: TcxCanvas;
  AViewInfo: TcxEditButtonViewInfo; var ARect, AContentRect: TRect;
  var APenColor, ABrushColor: TColor);
var
  AButtonStyle: TcxEditButtonStyle;
begin
  if DoDrawButtonBorder(ACanvas, AViewInfo, ARect, AContentRect) then
    Exit;

  if AViewInfo.Data.NativeState <> TC_NONE then
    DrawNativeButtonBorder(ACanvas, AViewInfo, ARect, AContentRect, APenColor, ABrushColor)
  else
    if UseSkins then
      DrawButtonBorderByPainter(AViewInfo, ARect, AContentRect, APenColor, ABrushColor)
    else
    begin
      AButtonStyle := AViewInfo.Data.Style;
      if (AViewInfo.Data.State in [ebsPressed, ebsSelected]) and (AButtonStyle = btsSimple) then
        AButtonStyle := btsFlat;
      case AButtonStyle of
        bts3D:
          Draw3DButtonBorder(ACanvas, AViewInfo, ARect, AContentRect, APenColor, ABrushColor);
        btsFlat:
          DrawFlatButtonBorder(ACanvas, AViewInfo, ARect, AContentRect, APenColor, ABrushColor);
        btsSimple:
          DrawSimpleButtonBorder(ACanvas, AViewInfo, ARect, AContentRect, APenColor, ABrushColor);
        btsHotFlat:
          DrawHotFlatButtonBorder(ACanvas, AViewInfo, ARect, AContentRect, APenColor, ABrushColor);
        btsUltraFlat, btsOffice11:
          DrawUltraFlatButtonBorder(ACanvas, AViewInfo, AButtonStyle = btsOffice11, ARect, AContentRect, APenColor, ABrushColor);
      end;
    end;
end;

procedure TcxCustomEditViewInfo.DrawButtonContent(ACanvas: TcxCanvas;
  AViewInfo: TcxEditButtonViewInfo; const AContentRect: TRect;
  APenColor, ABrushColor: TColor; ANeedOffsetContent: Boolean);

  function GetContentPosition(const AContentSize: TSize; AOffsetContent: Boolean): TPoint;
  var
    AHorzSpace, AVertSpace: Integer;
  begin
    AHorzSpace := cxRectWidth(AContentRect) - AContentSize.cx;
    AVertSpace := cxRectHeight(AContentRect) - AContentSize.cy;
    Result.X := AContentRect.Left + AHorzSpace div 2;
    Result.Y := AContentRect.Top + AVertSpace div 2;
    if AOffsetContent then
    begin
      if Result.X + AContentSize.cx < AContentRect.Right then
        Inc(Result.X);
      if Result.Y + AContentSize.cy < AContentRect.Bottom then
        Inc(Result.Y);
    end;
  end;

  procedure DrawArrowButtonContent;

    procedure DrawArrow(const R: TRect; AColor: TColor);
    var
      APainter: TcxCustomLookAndFeelPainter;
    begin
      if cxLookAndFeelPaintersManager.GetPainter(lfsStandard, APainter) then
        APainter.DrawArrow(ACanvas, R, adDown, AColor);
    end;

  var
    AButtonHeight, AButtonWidth: Integer;
    R: TRect;
  begin
    AButtonWidth := cxRectWidth(AViewInfo.Bounds) - cxRectWidth(AContentRect);
    R := AContentRect;
    if not Odd(AButtonWidth) then
    begin
      Dec(R.Left, AButtonWidth div 2);
      Inc(R.Right, AButtonWidth div 2);
      if ANeedOffsetContent then
        OffsetRect(R, 1, 0);
    end
    else
      if not ANeedOffsetContent then
      begin
        Inc(R.Right, AButtonWidth div 2);
        Dec(R.Left, AButtonWidth - AButtonWidth div 2);
      end else
      begin
        Dec(R.Left, AButtonWidth div 2);
        Inc(R.Right, AButtonWidth - AButtonWidth div 2);
      end;
    if ANeedOffsetContent then
      OffsetRect(R, 0, 1);

    if IsInplace then
    begin
      AButtonHeight := AViewInfo.Bounds.Bottom - AViewInfo.Bounds.Top;
      Dec(AButtonHeight, AContentRect.Bottom - AContentRect.Top);
      if not Odd(AButtonHeight) then
      begin
        Dec(R.Top, AButtonHeight div 2);
        Inc(R.Bottom, AButtonHeight div 2);
      end
      else
        if not ANeedOffsetContent then
        begin
          Inc(R.Bottom, AButtonHeight div 2);
          Dec(R.Top, AButtonHeight - AButtonHeight div 2);
        end else
        begin
          Dec(R.Top, AButtonHeight div 2);
          Inc(R.Bottom, AButtonHeight - AButtonHeight div 2);
        end;
    end;

    if AViewInfo.Data.State <> ebsDisabled then
      DrawArrow(R, APenColor)
    else
    begin
      DrawArrow(cxRectOffset(R, 1, 1), clBtnHighlight);
      DrawArrow(R, clBtnShadow);
    end;
  end;

  procedure DrawEllipsisButtonContent;

    procedure DrawEllipsis(X, Y, ASize: Integer; AColor: TColor);
    begin
      ACanvas.FillRect(Rect(X, Y, X + ASize, Y + ASize), AColor);
      ACanvas.FillRect(Rect(X + ASize + 2, Y, X + ASize * 2 + 2, Y + ASize), AColor);
      ACanvas.FillRect(Rect(X + ASize * 2 + 4, Y, X + ASize * 3 + 4, Y + ASize), AColor);
    end;

  var
    P: TPoint;
    AContentSize: TSize;
  begin
    if AContentRect.Right - AContentRect.Left < 12 then
      AContentSize.cy := 1
    else
      AContentSize.cy := 2;
    AContentSize.cx := AContentSize.cy * 3 + 4;
    P := GetContentPosition(AContentSize, ANeedOffsetContent);
    if AViewInfo.Data.State <> ebsDisabled then
      DrawEllipsis(P.X, P.Y, AContentSize.cy, APenColor)
    else
    begin
      DrawEllipsis(P.X + 1, P.Y + 1, AContentSize.cy, clBtnHighlight);
      DrawEllipsis(P.X, P.Y, AContentSize.cy, clBtnShadow);
    end;
  end;

  procedure DrawGlyphButtonContent;
  var
    AImageRect: TRect;
    P: TPoint;
    ASize: TSize;
  begin
    if IsImageAssigned(AViewInfo.Glyph, AViewInfo.Images, AViewInfo.ImageIndex) then
    begin
      ASize := dxGetImageSize(AViewInfo.Glyph, AViewInfo.Images, 0, ScaleFactor);
      P := GetContentPosition(ASize, ANeedOffsetContent);
      AImageRect := Rect(P.X, P.Y, P.X + ASize.cx, P.Y + ASize.cy);
      cxDrawImage(ACanvas.Handle, AImageRect, AContentRect,
        AViewInfo.Glyph, AViewInfo.Images, AViewInfo.ImageIndex,
        EnabledImageDrawModeMap[not (AViewInfo.Data.State = ebsDisabled)],
        False, 0, clDefault, True, GetButtonColorPalette(AViewInfo.Data.State));
    end;
  end;

  procedure DrawTextButtonContent;

    procedure DrawText(const R: TRect; AColor: TColor);
    begin
      ACanvas.Font.Color := AColor;
      ACanvas.DrawText(AViewInfo.Data.Caption, R,
        cxAlignmentsHorz[AViewInfo.Data.ContentAlignment] or
        cxAlignVCenter or cxSingleLine or cxShowPrefix or cxShowEndEllipsis);
    end;

  var
    R: TRect;
    AColor: TColor;
  begin
    ACanvas.Brush.Style := bsClear;
    ACanvas.Font := Font;

    R := AContentRect;
    if not IsInplace then
      InflateRect(R, -1, -1)
    else
      InflateRect(R, -1, 0);
    Dec(R.Right);
    if AViewInfo.Data.State = ebsDisabled then
    begin
      DrawText(cxRectOffset(R, 1, 1), clBtnHighlight);
      DrawText(R, clBtnShadow);
    end
    else
    begin
      if (AViewInfo.Data.Style = btsHotFlat) and (AViewInfo.Data.State in [ebsPressed, ebsSelected]) then
        AColor := APenColor
      else
        if AViewInfo.Data.TextColor = clDefault then
          AColor := TextColor
        else
          AColor := AViewInfo.Data.TextColor;

      if ANeedOffsetContent then
        OffsetRect(R, 1, 1);
      DrawText(R, AColor);
    end;
    ACanvas.Brush.Style := bsSolid;
  end;

begin
  ACanvas.SaveState;
  try
    ACanvas.SetClipRegion(TcxRegion.Create(AContentRect), roIntersect);
    if UseSkins then
    begin
      Painter.DrawScaledEditorButtonGlyph(ACanvas, AContentRect,
        EditBtnKindToEditBtnPainterKind[AViewInfo.Data.Kind],
        EditBtnStateToButtonState[AViewInfo.Data.State],
        AViewInfo.ScaleFactor,
        EditBtnPositionMap[AViewInfo.Data.LeftAlignment]);

      case AViewInfo.Data.Kind of
        bkText:
          DrawTextButtonContent;
        bkGlyph:
          DrawGlyphButtonContent;
      end;
    end
    else
      case AViewInfo.Data.Kind of
        bkDown:
          DrawArrowButtonContent;
        bkEllipsis:
          DrawEllipsisButtonContent;
        bkGlyph:
          DrawGlyphButtonContent;
        bkText:
          DrawTextButtonContent;
      end;
  finally
    ACanvas.RestoreState;
  end;
end;

procedure TcxCustomEditViewInfo.DrawNativeStyleEditBackground(
  ACanvas: TcxCanvas; ADrawBackground: Boolean; ABackgroundBrush: TBrushHandle);

  function GetContentRect(ATheme: TdxTheme; ANativePart: Integer): TRect;
  begin
    case BackgroundPaintingStyle of
      bpsSolid:
        if IsWinVistaOrLater then
          Result := cxRectInflate(FEditorBounds, -2, -2)
        else
          GetThemeBackgroundContentRect(ATheme, ACanvas.Handle, ANativePart, NativeState, FEditorBounds, Result);

      bpsComboEdit:
        begin
          Result := cxRectInflate(FEditorBounds, -(cxEditMaxBorderWidth + 1));
          Result.Right := Result.Right - 1;
        end;

      bpsComboListEdit:
        if IsWinVistaOrLater then
          Result := cxEmptyRect
        else
          Result := ClientRect;
    end;
  end;

  procedure DrawBorder(ATheme: TdxTheme; ANativePart: Integer; const AContentRect: TRect);
  var
    R: TRect;
  begin
    ACanvas.SaveClipRegion;
    try
      ACanvas.ExcludeClipRect(AContentRect);
      if IsWinVistaOrLater and (BackgroundPaintingStyle = bpsComboListEdit) then
        R := cxRectInflate(FEditorBounds, 1, 1)
      else
        R := FEditorBounds;
      DrawThemeBackground(ATheme, ACanvas.Handle, ANativePart, NativeState, R);
    finally
      ACanvas.RestoreClipRegion;
    end;
  end;

  procedure DrawBackground(const AContentRect: TRect);
  begin
    if ADrawBackground or IsInplace or (BorderStyle = ebsNone) then
      cxEditFillRect(ACanvas.Handle, AContentRect, ABackgroundBrush)
    else
      if BackgroundPaintingStyle <> bpsComboListEdit then
        InternalFillRect(ACanvas, AContentRect, BorderRect, ABackgroundBrush);
  end;

var
  AContentRect: TRect;
  ANativePart: Integer;
  ATheme: TdxTheme;
  AThemedObjectType: TdxThemedObjectType;
begin
  if IsInplace or (BorderStyle = ebsNone) then
    AContentRect := Bounds
  else
  begin
    CalculateNativeInfo(AThemedObjectType, ANativePart);
    ATheme := OpenTheme(AThemedObjectType);
    AContentRect := GetContentRect(ATheme, ANativePart);
    DrawBorder(ATheme, ANativePart, AContentRect);
  end;

  if not Transparent then
    DrawBackground(AContentRect);
end;

function TcxCustomEditViewInfo.IsBackgroundPartiallyTransparent: Boolean;
var
  ANativePart: Integer;
  AThemedObjectType: TdxThemedObjectType;
begin
  Result := ErrorData.IsError and IsImageAssigned(ErrorData.ErrorIcon);
  if not Result and NativeStyle and (BorderStyle <> ebsNone) then
  begin
    CalculateNativeInfo(AThemedObjectType, ANativePart);
    Result := IsThemeBackgroundPartiallyTransparent(OpenTheme(AThemedObjectType), ANativePart, NativeState);
  end;
end;

function TcxCustomEditViewInfo.IsBackgroundTransparent: Boolean;
begin
  Result := not (GetDrawBackgroundStyle in [dbsCustomEdit, dbsSimpleFill]);
end;

function TcxCustomEditViewInfo.IsCustomBackground: Boolean;
begin
  Result := DrawBackground(nil);
end;

function TcxCustomEditViewInfo.IsCustomButton(AButtonVisibleIndex: Integer = 0): Boolean;
begin
  Result := DoDrawButton(nil, ButtonsInfo[AButtonVisibleIndex]);
end;

function TcxCustomEditViewInfo.IsCustomButtonBackground(AButtonVisibleIndex: Integer = 0): Boolean;
begin
  Result := DoDrawButtonBackground(nil, cxEmptyRect, ButtonsInfo[AButtonVisibleIndex]);
end;

function TcxCustomEditViewInfo.IsCustomButtonBorder(AButtonVisibleIndex: Integer = 0): Boolean;
var
  ARect: TRect;
begin
  Result := DoDrawButtonBorder(nil, ButtonsInfo[AButtonVisibleIndex], ARect, ARect);
end;

function TcxCustomEditViewInfo.IsCustomDrawButton(AButtonVisibleIndex: Integer = 0): Boolean;
begin
  Result := IsCustomButtonBorder(AButtonVisibleIndex) or IsCustomButtonBackground(AButtonVisibleIndex) or IsCustomButton(AButtonVisibleIndex);
end;

function TcxCustomEditViewInfo.IsHotTrack: Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Length(ButtonsInfo) - 1 do
    if ButtonsInfo[I].Data.State = ebsSelected then
    begin
      Result := True;
      Break;
    end;
end;

function TcxCustomEditViewInfo.IsHotTrack(P: TPoint): Boolean;
var
  I: Integer;
begin
  Result := False;
  Dec(P.X, Left);
  Dec(P.Y, Top);
  for I := 0 to Length(ButtonsInfo) - 1 do
    if PtInRect(ButtonsInfo[I].Bounds, P) then
    begin
      Result := True;
      Break;
    end;
end;

function TcxCustomEditViewInfo.NeedShowHint(ACanvas: TcxCanvas;
  const P: TPoint; const AVisibleBounds: TRect; out AText: TCaption;
  out AIsMultiLine: Boolean; out ATextRect: TRect; AMaxLineCount: Integer = 0): Boolean;
var
  APart: Integer;
begin
  APart := GetPart(Point(P.X - Left, P.Y - Top));
  if (APart >= ecpButton) or (APart = ecpErrorIcon) then
  begin
    AText := GetShortHint(GetHintText(APart));
    AIsMultiLine := False;
    ATextRect := GetHintTextRect(P, APart);
    Result := Length(AText) > 0;
  end
  else
  begin
    AText := '';
    AIsMultiLine := False;
    ATextRect := cxEmptyRect;
    Result := False;
  end;
end;

procedure TcxCustomEditViewInfo.Paint(ACanvas: TcxCanvas);
begin
  if Assigned(FOnPaint) then
    FOnPaint(Self, ACanvas);
  inherited;
  DoAfterDrawValue(ACanvas);
end;

procedure TcxCustomEditViewInfo.PaintEx(ACanvas: TcxCanvas);
var
  P: TPoint;
  ACanvasHandle: HDC;
begin
  ACanvasHandle := ACanvas.Handle;
  GetWindowOrgEx(ACanvasHandle, P);
  Dec(P.X, Left);
  Dec(P.Y, Top);
  SetWindowOrgEx(ACanvasHandle, P.X, P.Y, @P);
  try
    Paint(ACanvas);
  finally
    SetWindowOrgEx(ACanvasHandle, P.X, P.Y, nil);
  end;
end;

procedure TcxCustomEditViewInfo.PrepareCanvasFont(ACanvas: TCanvas);
begin
end;

function TcxCustomEditViewInfo.Repaint(
  AControl: TWinControl; AViewInfo: TcxContainerViewInfo = nil): Boolean;
begin
  Result := Repaint(AControl, cxEmptyRect, AViewInfo);
end;

function TcxCustomEditViewInfo.Repaint(AControl: TWinControl;
  const AInnerEditRect: TRect; AViewInfo: TcxContainerViewInfo = nil): Boolean;

  procedure CheckRect(var R: TRect);
  begin
    R.Left := Max(R.Left, ClientRect.Left);
    R.Top := Max(R.Top, ClientRect.Top);
    R.Right := Min(R.Right, ClientRect.Right);
    R.Bottom := Min(R.Bottom, ClientRect.Bottom);
  end;

  function GetInnerEditRect: TRect;
  begin
    Result := AInnerEditRect;
    CheckRect(Result);
  end;

  function RepaintButtons: Boolean;
  var
    I: Integer;
    AEditPosition: TPoint;
  begin
    Result := False;
    AEditPosition := Point(Left, Top);
    for I := 0 to Length(ButtonsInfo) - 1 do
      if ButtonsInfo[I].Repaint(AControl, TcxCustomEditViewInfo(AViewInfo).ButtonsInfo[I], AEditPosition) then
        Result := True;
  end;

var
  R, R1: TRect;
begin
  Result := AControl.HandleAllocated;
  if not Result then
    Exit;

  R := cxRectoffset(Bounds, Left, Top);
  Result := AViewInfo <> nil;
  if not Result then
  begin
    InternalInvalidate(AControl.Handle, R, GetInnerEditRect, HasBackground, HasNonClientArea);
    Exit;
  end;
  with AViewInfo as TcxCustomEditViewInfo do
  begin
    if ((Self.NativeState <> NativeState) or
      (Self.BackgroundColor <> BackgroundColor) or
      (Self.ContainerState <> ContainerState) and Self.IsRepaintOnStateChangingNeeded) or
       (Length(Self.ButtonsInfo) <> Length(ButtonsInfo)) then
      R1 := GetInnerEditRect
    else
      if (Self.BorderColor <> BorderColor) and (Self.BorderStyle <> ebsNone) or
         (Self.BorderStyle <> BorderStyle) then
      begin
        R1 := Self.BorderRect;
        OffsetRect(R1, Self.Left, Self.Top);
      end
      else
        Result := False;
    if Result then
      if not IsRectEmpty(Self.Bounds) and not EqualRect(R, R1) then
        InternalInvalidate(AControl.Handle, R, R1, HasBackground, HasNonClientArea);
    if (Length(Self.ButtonsInfo) = Length(ButtonsInfo)) and RepaintButtons then
      Result := True;
  end;
end;

procedure TcxCustomEditViewInfo.ResetValidationInfo;
begin
  ErrorData.Reset;
end;

procedure TcxCustomEditViewInfo.SetButtonCount(ACount: Integer);
var
  I: Integer;
  APrevLength: Integer;
begin
  APrevLength := Length(ButtonsInfo);
  if APrevLength <> ACount then
  begin
    if ACount < APrevLength then
    begin
      for I := Length(ButtonsInfo) - 1 downto ACount do
        ButtonsInfo[I].Free;
      SetLength(ButtonsInfo, ACount);
    end
    else
    begin
      SetLength(ButtonsInfo, ACount);
      for I := APrevLength to ACount - 1 do
      begin
        ButtonsInfo[I] := GetButtonViewInfoClass.Create;
        ButtonsInfo[I].EditViewInfo := Self;
      end;
    end;
  end;
end;

function TcxCustomEditViewInfo.SetOrigin(const APoint: TPoint): TPoint;
begin
  Result := Point(Left, Top);
  Left := APoint.X;
  Top := APoint.Y;
end;

function TcxCustomEditViewInfo.GetBackgroundPaintingStyle: TcxEditBackgroundPaintingStyle;
begin
  Result := bpsSolid;
end;

function TcxCustomEditViewInfo.GetButtonViewInfoClass: TcxEditButtonViewInfoClass;
begin
  Result := TcxEditButtonViewInfo;
end;

procedure TcxCustomEditViewInfo.GetColorSettingsByPainter(out ABackground, ATextColor: TColor);
begin
  ABackground := clDefault;
  ATextColor := clDefault;
  if not (IsInplace or (FEdit = nil)) then
    FEdit.GetColorSettingsByPainter(ABackground, ATextColor);
end;

function TcxCustomEditViewInfo.GetContainerBorderStyle: TcxContainerBorderStyle;
begin
  Result := TcxContainerBorderStyle(BorderStyle);
end;

function TcxCustomEditViewInfo.GetHasButtonsStateChanges: Boolean;
begin
  Result := (LastPressedButton <> PressedButton) or (LastSelectedButton <> SelectedButton);
end;

function TcxCustomEditViewInfo.GetHintText(APart: Integer): string;
begin
  if (APart >= ecpButton) and (APart < Length(ButtonsInfo)) then
    Result := ButtonsInfo[APart].Data.Hint
  else
    if APart = ecpErrorIcon then
      Result := ErrorData.ErrorText
    else
      Result := '';
end;

function TcxCustomEditViewInfo.GetPart(const P: TPoint): Integer;
var
  I: Integer;
begin
  if ErrorData.IsError and PtInRect(FErrorBounds, P) then
    Result := ecpErrorIcon
  else
    if PtInRect(BorderRect, P) then
      Result := ecpControl
    else
      Result := ecpNone;

  for I := Low(ButtonsInfo) to High(ButtonsInfo) do
    if PtInRect(ButtonsInfo[I].Bounds, P) then
    begin
      Result := I;
      Break;
    end;
end;

function TcxCustomEditViewInfo.GetPartRect(APart: Integer): TRect;
var
  I: Integer;
begin
  Result := cxNullRect;
  if Apart = ecpErrorIcon then
    Result := FErrorBounds
  else
    if APart = ecpControl then
    begin
      Result := BorderRect;
      for I := Low(ButtonsInfo) to High(ButtonsInfo) do
        if (ButtonsInfo[I].Data.LeftAlignment) and (ButtonsInfo[I].Data.Rightmost) then
          Result.Left := ButtonsInfo[I].Bounds.Right
        else
          if not (ButtonsInfo[I].Data.LeftAlignment) and (ButtonsInfo[I].Data.Leftmost) then
            Result.Right := ButtonsInfo[I].Bounds.Left;
    end
    else
      if (APart >= ecpButton) and (APart < Length(ButtonsInfo)) then
        Result := ButtonsInfo[APart].Bounds;
end;

function TcxCustomEditViewInfo.HasNonClientArea: Boolean;
begin
  Result := (Edit <> nil) and Edit.HasNonClientArea;
end;

procedure TcxCustomEditViewInfo.InternalPaint(ACanvas: TcxCanvas);
begin
  DrawCustomEdit(ACanvas, True, True);
end;

procedure TcxCustomEditViewInfo.InternalPrepareEditButtonBackground(
  ACanvas: TcxCanvas; AViewInfo: TcxEditButtonViewInfo; out AContentRect, ABackgroundRect: TRect;
  out APenColor, ABrushColor: TColor; ACalculateOnly: Boolean);
begin
  ABrushColor := 0;
  APenColor := clBtnText;
  ABackgroundRect := AViewInfo.Bounds;
  AContentRect := ABackgroundRect;

  ACanvas.SaveClipRegion;
  try
    if ACalculateOnly then
      ACanvas.ExcludeClipRect(ABackgroundRect);
    DrawButtonBorder(ACanvas, AViewInfo, ABackgroundRect, AContentRect, APenColor, ABrushColor);
    if not IsRectEmpty(ABackgroundRect) then
      DrawButtonBackground(ACanvas, AViewInfo, ABackgroundRect, AContentRect, ABrushColor);
  finally
    ACanvas.RestoreClipRegion;
  end;
end;

function TcxCustomEditViewInfo.IsRepaintOnStateChangingNeeded: Boolean;
begin
  Result := True;
end;

procedure TcxCustomEditViewInfo.DrawEditButton(ACanvas: TcxCanvas; AButtonVisibleIndex: Integer);

  function NeedOffsetContent(AViewInfo: TcxEditButtonViewInfo): Boolean;
  begin
    Result := (AViewInfo.Data.State = ebsPressed) and
      ((Edit = nil) or (ecoOffsetButtonContent in Edit.ContentParams.Options)); // to support the solution in B782
  end;

var
  AButtonViewInfo: TcxEditButtonViewInfo;
  AContentRect, ABackgroundRect: TRect;
  AFadingActive: Boolean;
  APenColor, ABrushColor: TColor;
begin
  AButtonViewInfo := ButtonsInfo[AButtonVisibleIndex];
  AFadingActive := AButtonViewInfo.FadingHelper.Active;
  if AFadingActive then
    AButtonViewInfo.FadingHelper.DrawImage(ACanvas.Handle, AButtonViewInfo.Bounds);
  InternalPrepareEditButtonBackground(ACanvas, AButtonViewInfo,
    AContentRect, ABackgroundRect, APenColor, ABrushColor, AFadingActive);
  if not IsRectEmpty(AContentRect) then
    DrawButtonContent(ACanvas, AButtonViewInfo, AContentRect,
      APenColor, ABrushColor, NeedOffsetContent(AButtonViewInfo));
end;

procedure TcxCustomEditViewInfo.SetOnDrawBackground(AValue: TcxEditDrawBackgroundEvent);
begin
  FOnDrawBackground := AValue;
end;

procedure TcxCustomEditViewInfo.StoreLastState;
begin
  LastPressedButton := PressedButton;
  LastSelectedButton := SelectedButton;
end;

procedure TcxCustomEditViewInfo.DoAfterDrawBackground(ACanvas: TcxCanvas);
begin
  if not FOnAfterDrawBackground.Empty then
    FOnAfterDrawBackground.Invoke(ACanvas);
end;

procedure TcxCustomEditViewInfo.DoAfterDrawValue(ACanvas: TcxCanvas);
begin
  if not FOnAfterDrawValue.Empty then
    FOnAfterDrawValue.Invoke(ACanvas);
end;

procedure TcxCustomEditViewInfo.Draw3DButtonBorder(ACanvas: TcxCanvas; AButtonViewInfo: TcxEditButtonViewInfo;
  var ARect: TRect; out AContentRect: TRect; var APenColor, ABrushColor: TColor);
begin
  APenColor := clBtnText;
  if AButtonViewInfo.Data.Transparent then
    ABrushColor := AButtonViewInfo.Data.BackgroundColor
  else
    ABrushColor := clBtnFace;

  if AButtonViewInfo.Data.State = ebsPressed then
  begin
    FrameRectEx(ACanvas, ARect, clBtnShadow);
    FrameRectEx(ACanvas, ARect, ABrushColor);
  end
  else
  begin
    DrawComplexFrameEx(ACanvas, ARect, cl3DLight, cl3DDkShadow);
    DrawComplexFrameEx(ACanvas, ARect, clBtnHighlight, clBtnShadow);
  end;

  AContentRect := ARect;
end;

procedure TcxCustomEditViewInfo.DrawFlatButtonBorder(ACanvas: TcxCanvas; AButtonViewInfo: TcxEditButtonViewInfo;
  var ARect: TRect; out AContentRect: TRect; var APenColor, ABrushColor: TColor);
begin
  if not AButtonViewInfo.Data.Transparent then
    ABrushColor := clBtnFace
  else
    ABrushColor := AButtonViewInfo.Data.BackgroundColor;

  cxEditUtils.DrawButtonBorder(ACanvas, ARect, [bLeft], ABrushColor);
  if AButtonViewInfo.Data.State = ebsPressed then
    DrawComplexFrameEx(ACanvas, ARect, clBtnShadow, clBtnHighlight)
  else
    DrawComplexFrameEx(ACanvas, ARect, clBtnHighlight, clBtnShadow);
  cxEditUtils.DrawButtonBorder(ACanvas, ARect, [bRight], ABrushColor);
  AContentRect := ARect;
end;

procedure TcxCustomEditViewInfo.DrawHotFlatButtonBorder(ACanvas: TcxCanvas; AButtonViewInfo: TcxEditButtonViewInfo;
  var ARect: TRect; out AContentRect: TRect; var APenColor, ABrushColor: TColor);
const
  ABrushColorA: array [TcxEditButtonState] of TColor = (
    clBtnFace, clBtnFace, clBtnText, clBtnShadow
  );
begin
  with AButtonViewInfo do
  begin
    ABrushColor := ABrushColorA[Data.State];
    if Data.Transparent then
      ABrushColor := Data.BackgroundColor;

    APenColor := clBtnShadow;
    if (Data.LeftAlignment and Data.Rightmost) or (not Data.LeftAlignment and Data.Leftmost) then
      FrameRectEx(ACanvas, ARect, APenColor)
    else
      if Data.LeftAlignment then
      begin
        cxEditUtils.DrawButtonBorder(ACanvas, ARect, [bBottom, bLeft, bTop], APenColor);
        cxEditUtils.DrawButtonBorder(ACanvas, ARect, [bRight], ABrushColor);
      end
      else
      begin
        cxEditUtils.DrawButtonBorder(ACanvas, ARect, [bTop, bRight, bBottom], APenColor);
        cxEditUtils.DrawButtonBorder(ACanvas, ARect, [bLeft], ABrushColor);
      end;

    if Data.State in [ebsPressed, ebsSelected] then
      if Data.Transparent and (Data.State = ebsSelected) then
        APenColor := clBtnShadow
      else
        APenColor := clWindow
    else
      APenColor := clBtnText;
    cxEditUtils.DrawButtonBorder(ACanvas, ARect, [bLeft, bRight], ABrushColor);
    AContentRect := ARect;
  end;
end;

procedure TcxCustomEditViewInfo.DrawNativeButtonBorder(ACanvas: TcxCanvas; AButtonViewInfo: TcxEditButtonViewInfo;
  var ARect: TRect; out AContentRect: TRect; var APenColor, ABrushColor: TColor);

  function GetThemeContentRect(AThemeObject: TdxThemedObjectType; APart: Integer): TRect;
  var
    ATheme: TdxTheme;
  begin
    ATheme := OpenTheme(AThemeObject);
    GetThemeBackgroundContentRect(ATheme, ACanvas.Handle, APart,
      AButtonViewInfo.Data.NativeState, AButtonViewInfo.Bounds, Result);
  end;

  function GetContentRect: TRect;
  begin
    if IsCustomDrawButton then
      Result := ARect
    else
      if AButtonViewInfo.Data.ComboBoxStyle then
        Result := cxEmptyRect
      else
        Result := GetThemeContentRect(totButton, BP_PUSHBUTTON)
  end;

begin
  AContentRect := GetContentRect;
end;

procedure TcxCustomEditViewInfo.DrawSimpleButtonBorder(ACanvas: TcxCanvas; AButtonViewInfo: TcxEditButtonViewInfo;
  var ARect: TRect; out AContentRect: TRect; var APenColor, ABrushColor: TColor);
begin
  if not Transparent then
    ACanvas.FrameRect(ARect, AButtonViewInfo.Data.BackgroundColor);
  InflateRect(ARect, -1, -1);
  if not AButtonViewInfo.Data.Transparent then
    ABrushColor := clBtnFace
  else
    ABrushColor := AButtonViewInfo.Data.BackgroundColor;
  cxEditUtils.DrawButtonBorder(ACanvas, ARect, [bLeft, bRight], ABrushColor);
  AContentRect := ARect;
end;

procedure TcxCustomEditViewInfo.DrawUltraFlatButtonBorder(ACanvas: TcxCanvas; AButtonViewInfo: TcxEditButtonViewInfo;
  AIsOffice11Style: Boolean; var ARect: TRect; var AContentRect: TRect; out APenColor, ABrushColor: TColor);
var
  ABackgroundRect: TRect;
  AHighlightColor: TColor;
begin
  if AButtonViewInfo.Data.Transparent then
    ABrushColor := AButtonViewInfo.Data.BackgroundColor
  else
    if AButtonViewInfo.Data.State = ebsDisabled then
      ABrushColor := clBtnFace
    else
      if AButtonViewInfo.Data.State = ebsNormal then
        if AIsOffice11Style then
          ABrushColor := dxOffice11DockColor1
        else
          ABrushColor := clBtnFace
      else
        ABrushColor := GetEditButtonHighlightColor(
          AButtonViewInfo.Data.State = ebsPressed, AIsOffice11Style);

  AHighlightColor := GetEditBorderHighlightColor(AIsOffice11Style);

  if (AButtonViewInfo.Data.State in [ebsDisabled, ebsNormal]) or
    not AButtonViewInfo.Data.IsInplace and (BorderStyle = ebsNone) or
    AButtonViewInfo.Data.IsInplace and not (epoHasExternalBorder in PaintOptions) then
  begin
      if not(AButtonViewInfo.Data.State in [ebsDisabled, ebsNormal]) then
        ACanvas.FrameRect(ARect, AHighlightColor)
      else
        if not Transparent then
          ACanvas.FrameRect(ARect, AButtonViewInfo.Data.BackgroundColor);
    InflateRect(ARect, -1, -1);
    ABackgroundRect := ARect;
    ARect := cxRectContent(ARect, Rect(1, 0, 1, 0));
  end
  else
  begin
    ABackgroundRect := ARect;
    if AButtonViewInfo.Data.LeftAlignment then
    begin
      if AButtonViewInfo.Data.Leftmost then
        Inc(ARect.Left)
      else
      begin
        cxEditUtils.DrawButtonBorder(ACanvas, ARect, [bLeft], AHighlightColor);
        Inc(ABackgroundRect.Left);
      end;
      if ARect.Right = BorderRect.Right then
        Dec(ARect.Right)
      else
      begin
        cxEditUtils.DrawButtonBorder(ACanvas, ARect, [bRight], AHighlightColor);
        Dec(ABackgroundRect.Right);
      end;
    end
    else
    begin
      if AButtonViewInfo.Data.Rightmost then
        Dec(ARect.Right)
      else
      begin
        cxEditUtils.DrawButtonBorder(ACanvas, ARect, [bRight], AHighlightColor);
        Dec(ABackgroundRect.Right);
      end;
      if ARect.Left = BorderRect.Left then
        Inc(ARect.Left)
      else
      begin
        cxEditUtils.DrawButtonBorder(ACanvas, ARect, [bLeft], AHighlightColor);
        Inc(ABackgroundRect.Left);
      end;
    end;
    InflateRect(ARect, -1, -1);
  end;

  AContentRect := ARect;
  ARect := ABackgroundRect;
end;

procedure TcxCustomEditViewInfo.DrawNativeButtonBackground(ACanvas: TcxCanvas; AButtonViewInfo: TcxEditButtonViewInfo; const ARect: TRect);
var
  APart: Integer;
  AThemeObject: TdxThemedObjectType;
begin
  if AButtonViewInfo.Data.ComboBoxStyle then
  begin
    AThemeObject := totComboBox;
    if IsWinVistaOrLater and not IsInplace then
      if AButtonViewInfo.Data.LeftAlignment then
        APart := CP_DROPDOWNBUTTONLEFT
      else
        APart := CP_DROPDOWNBUTTONRIGHT
    else
      APart := CP_DROPDOWNBUTTON;
  end
  else
  begin
    AThemeObject := totButton;
    APart := BP_PUSHBUTTON;
  end;
  DrawThemeBackground(OpenTheme(AThemeObject), ACanvas.Handle, APart, AButtonViewInfo.Data.NativeState, ARect);
end;

procedure TcxCustomEditViewInfo.DrawUsualButtonBackground(ACanvas: TcxCanvas; AButtonViewInfo: TcxEditButtonViewInfo; const ARect: TRect; ABrushColor: TColor);

  procedure GetBackgroundParams(out AGradientDrawing: Boolean;
    out AColor1, AColor2: TColor);
  begin
    AGradientDrawing := (AButtonViewInfo.Data.Style = btsOffice11) and
      AButtonViewInfo.Data.Gradient and not AButtonViewInfo.Data.Transparent and
      (AButtonViewInfo.Data.State <> ebsDisabled);
    if AGradientDrawing then
      case AButtonViewInfo.Data.State of
        ebsNormal:
          begin
            AColor1 := dxOffice11ToolbarsColor1;
            AColor2 := dxOffice11ToolbarsColor2;
          end;
        ebsPressed:
          begin
            AColor1 := dxOffice11SelectedDownColor1;
            AColor2 := dxOffice11SelectedDownColor2;
          end;
        ebsSelected:
          begin
            AColor1 := dxOffice11SelectedColor1;
            AColor2 := dxOffice11SelectedColor2;
          end;
      end;
  end;

var
  AClipRgn: TcxRegion;
  AColor1, AColor2: TColor;
  AGradientDrawing: Boolean;
  R: TRect;
begin
  GetBackgroundParams(AGradientDrawing, AColor1, AColor2);
  if not AGradientDrawing then
    ACanvas.FillRect(ARect, ABrushColor)
  else
  begin
    AClipRgn := ACanvas.GetClipRegion;
    try
      R := Rect(ARect.Left, BorderRect.Top, ARect.Right, BorderRect.Bottom);
      if AButtonViewInfo.Data.State = ebsNormal then
        R := cxRectContent(R, Rect(0, 1, 0, 1));
      ACanvas.SetClipRegion(TcxRegion.Create(ARect), roIntersect);
      FillGradientRect(ACanvas.Handle, R, AColor1, AColor2, False);
    finally
      ACanvas.SetClipRegion(AClipRgn, roSet);
    end;
  end;
end;

function TcxCustomEditViewInfo.IsFadingAvailable: Boolean;
begin
  Result := NativeStyle or UseSkins and (Painter <> nil);
end;

function TcxCustomEditViewInfo.DoCanStartButtonFading: Boolean;
begin
  Result := True;
  if Assigned(OnCanStartButtonFading) then
    OnCanStartButtonFading(Self, Result);
end;

function TcxCustomEditViewInfo.DoDrawBackground(ACanvas: TcxCanvas): Boolean;
begin
  Result := False;
  if Assigned(FOnDrawBackground) then
    FOnDrawBackground(Self, ACanvas, Result);
end;

function TcxCustomEditViewInfo.DoDrawButton(
  ACanvas: TcxCanvas; AViewInfo: TcxEditButtonViewInfo): Boolean;
begin
  Result := False;
  if Assigned(FOnDrawButton) then
    FOnDrawButton(AViewInfo, ACanvas, Result);
end;

function TcxCustomEditViewInfo.DoDrawButtonBackground(
  ACanvas: TcxCanvas; const ARect: TRect; AViewInfo: TcxEditButtonViewInfo): Boolean;
begin
  Result := False;
  if Assigned(FOnDrawButtonBackground) then
    FOnDrawButtonBackground(AViewInfo, ACanvas, ARect, Result);
end;

function TcxCustomEditViewInfo.DoDrawButtonBorder(ACanvas: TcxCanvas;
  AViewInfo: TcxEditButtonViewInfo; var ABackgroundRect, AContentRect: TRect): Boolean;
begin
  Result := False;
  if Assigned(FOnDrawButtonBorder) then
    FOnDrawButtonBorder(AViewInfo, ACanvas, ABackgroundRect, AContentRect, Result);
end;

function TcxCustomEditViewInfo.DoPrepareButtonFadingImage(
  ASender: TcxEditButtonViewInfo; AState: TcxEditButtonState; out AImage: TcxBitmap32): Boolean;
begin
  Result := False;
  if Assigned(OnPrepareButtonFadingImage) then
    OnPrepareButtonFadingImage(ASender, AState, AImage, Result);
end;

function TcxCustomEditViewInfo.GetDrawBackgroundStyle: TcxDrawBackgroundStyle;
begin
  Result := dbsSimpleFill;
  if IsInplace then
  begin
    if IsCustomBackground then
      Result := dbsCustom
    else
      if IsTransparent then
        Result := dbsNone
  end
  else
  begin
    if IsTransparent then
      Result := dbsSimpleParent
    else
      if IsNativeBackground then
        Result := dbsThemeParent
      else
        if not NativeStyle then
          Result := dbsCustomEdit;
  end;
end;

function TcxCustomEditViewInfo.IsNativeBackground: Boolean;
begin
  Result := (Edit <> nil) and TcxCustomEdit(Edit).IsNativeBackground;
end;

function TcxCustomEditViewInfo.IsTransparent: Boolean;
begin
  Result := Transparent or (Edit <> nil) and TcxCustomEdit(Edit).Transparent;
end;

function TcxCustomEditViewInfo.GetButtonColorPalette(AState: TcxEditButtonState): IdxColorPalette;
begin
  if Painter <> nil then
    Result := Painter.EditButtonColorPalette(EditBtnStateToButtonState[AState])
  else
    Result := nil;
end;

function TcxCustomEditViewInfo.GetHintTextRect(const P: TPoint; APart: Integer): TRect;
begin
  if (FHintWindow = nil) or (FHintWindow.ClassType <> cxGetHintWindowClass) then
  begin
    FHintWindow.Free;
    FHintWindow := cxGetHintWindowClass.Create(nil);
  end;
  Result := FHintWindow.CalcHintRect(Screen.Width, GetHintText(APart), nil);
  OffsetRect(Result, P.X, P.Y + cxGetCursorSize.cy);
end;

{ TcxCustomEditViewData }

constructor TcxCustomEditViewData.Create(AProperties: TcxCustomEditProperties; AStyle: TcxCustomEditStyle; AIsInplace: Boolean);
begin
  inherited Create;
  FIsInplace := AIsInplace;
  FProperties := AProperties;
  FStyle := AStyle;
  FScaleFactor := TdxScaleFactor.Create;
  FScaleFactor.Assign(Properties.ScaleFactor);
  Initialize;
  SelTextColor := clDefault;
  SelBackgroundColor := clDefault;
  InitCacheData;
end;

destructor TcxCustomEditViewData.Destroy;
begin
  FreeAndNil(FScaleFactor);
  inherited Destroy;
end;

procedure TcxCustomEditViewData.Calculate(ACanvas: TcxCanvas; const ABounds: TRect;
  const P: TPoint; Button: TcxMouseButton; Shift: TShiftState; AViewInfo: TcxCustomEditViewInfo; AIsMouseEvent: Boolean);

  procedure CalculateContainerState;
  begin
    AViewInfo.ContainerState := GetContainerState(ABounds, P, Button, Shift, AIsMouseEvent);

    Selected := ContainerState * [csActive, csHotTrack] <> [];
    if IsDesigning or not Style.HotTrack or IsInplace then
      AViewInfo.HotState := chsNoHotTrack
    else
      if Selected then
        AViewInfo.HotState := chsSelected
      else
        AViewInfo.HotState := chsNormal;
    if Edit <> nil then
      Style := Edit.ActiveStyle;

    if not IsInplace and NativeStyle then
      AViewInfo.BorderStyle := Style.BaseStyle.BorderStyle
    else
      AViewInfo.BorderStyle := GetBorderStyle;
  end;

  procedure CalculatePopupBorderStyle;
  const
    ADefaultPopupBorderStyleMap: array [TcxEditBorderStyle] of TcxEditPopupBorderStyle =
      (epbsFlat, epbsSingle, epbsSingle, epbsFlat, epbsSingle, epbsSingle, epbsSingle);
    AInplaceDefaultPopupBorderStyleMap: array [TcxLookAndFeelKind] of TcxEditPopupBorderStyle =
      (epbsFlat, epbsFrame3D, epbsSingle, epbsSingle);
  begin
    AViewInfo.PopupBorderStyle := Style.PopupBorderStyle;
    if (AViewInfo.PopupBorderStyle = epbsDefault) and (Edit <> nil) then
      if IsInplace then
        AViewInfo.PopupBorderStyle := AInplaceDefaultPopupBorderStyleMap[Style.LookAndFeel.Kind]
      else
        if (AViewInfo.BorderStyle = ebsNone) and (Length(AViewInfo.ButtonsInfo) > 0) then
          AViewInfo.PopupBorderStyle := AInplaceDefaultPopupBorderStyleMap[Edit.Style.LookAndFeel.Kind]
        else
          AViewInfo.PopupBorderStyle := ADefaultPopupBorderStyleMap[AViewInfo.BorderStyle];
  end;

var
  AClientExtent: TRect;
  APrevBorderWidth: Integer;
begin
  Shift := Shift - [ssShift, ssAlt, ssCtrl, ssTouch, ssPen];

  APrevBorderWidth := GetContainerBorderWidth(TcxContainerBorderStyle(AViewInfo.BorderStyle));
  CalculateContainerState;
  if not IsInplace and not NativeStyle and
    (GetContainerBorderWidth(TcxContainerBorderStyle(AViewInfo.BorderStyle)) < APrevBorderWidth) then
      CalculateContainerState;
  TcxContainerViewInfo(AViewInfo).BorderStyle := TcxContainerBorderStyle(AViewInfo.BorderStyle);
  ContainerState := AViewInfo.ContainerState;

  AViewInfo.EditProperties := Properties;
  AViewInfo.ScaleFactor.Assign(ScaleFactor);
  AViewInfo.UseRightToLeftAlignment := UseRightToLeftAlignment;
  if IsInplace then
    ButtonsOnlyStyle := (ABounds.Right <> cxMaxRectSize) and (Properties.ButtonsViewStyle <> bvsNormal)
  else
    ButtonsOnlyStyle := Properties.ButtonsViewStyle = bvsButtonsOnly;

  AViewInfo.UpdateStyle(Style);
  AViewInfo.Bounds := ABounds;

  AViewInfo.Enabled := Enabled;
  AViewInfo.Focused := Focused;
  AViewInfo.HasBackground := (Edit <> nil) and Edit.HasBackground;
  AViewInfo.HasInnerEdit := (InnerEdit <> nil) and (InnerEdit.Control.Visible);
  AViewInfo.IsDBEditPaintCopyDrawing := (Edit <> nil) and Edit.IsDBEditPaintCopyDrawing;
  AViewInfo.IsContainerInnerControl := (Edit <> nil) and Supports(TObject(Edit), IcxContainerInnerControl);
  AViewInfo.IsDesigning := IsDesigning;
  AViewInfo.IsInplace := IsInplace;
  AViewInfo.IsSelected := IsSelected;
  AViewInfo.PaintOptions := CalculatePaintOptions;
  AViewInfo.Edges := Style.Edges;
  FValidationErrorIconAlignment := Properties.ValidationErrorIconAlignment;
  if UseRightToLeftAlignment then
    if FValidationErrorIconAlignment = taLeftJustify then
      FValidationErrorIconAlignment := taRightJustify
    else
      FValidationErrorIconAlignment := taLeftJustify;

  if not IsInplace and (Edit <> nil) then
  begin
    if Edit.FValidationError then
      AViewInfo.ErrorData.ErrorType := eetError
    else
      AViewInfo.ErrorData.ErrorType := eetNone;

    if IsImageAssigned(Properties.ErrorIcon) then
      AViewInfo.ErrorData.ErrorIcon.Assign(Properties.ErrorIcon);
    AViewInfo.ErrorData.ErrorText := Edit.FValidationErrorText;
  end;

  AViewInfo.FNeedShowErrorIcon := NeedShowErrorIcon(AViewInfo.ErrorData);

  if Style.DirectAccessMode then
    AViewInfo.Font := Style.Font
  else
    AViewInfo.Font := Style.GetVisibleFont;

  AViewInfo.Shadow := not IsInplace and HasShadow;
  AViewInfo.WindowHandle := WindowHandle;

  CalculateViewInfo(AViewInfo, AIsMouseEvent);
  CalculateButtonsViewInfo(ACanvas, AViewInfo.FEditorBounds, P, Button, Shift, AViewInfo, AIsMouseEvent);

  AViewInfo.ClientRect := AViewInfo.FEditorBounds;
  AClientExtent := GetClientExtent(ACanvas, AViewInfo);
  AViewInfo.ClientRect := cxRectContent(AViewInfo.ClientRect, AClientExtent);

  if InnerEdit <> nil then
    AViewInfo.InnerEditRect := InnerEdit.Control.BoundsRect
  else
    AViewInfo.InnerEditRect := AViewInfo.ClientRect;
//    AViewInfo.InnerEditRect := AViewInfo.FEditorBounds; //# todo

  CalculatePopupBorderStyle;
  CheckStartButtonsFading(AViewInfo);
end;

procedure TcxCustomEditViewData.CalculateButtonBounds(ACanvas: TcxCanvas;
  AViewInfo: TcxCustomEditViewInfo; AButtonVisibleIndex: Integer;
  var ButtonsRect: TRect);
var
  AButtonVisibleWidth, AButtonWidth: Integer;
begin
  with AViewInfo.ButtonsInfo[AButtonVisibleIndex] do
  begin
    if IsRectEmpty(ButtonsRect) then
    begin
      Bounds := cxEmptyRect;
      VisibleBounds := Bounds;
    end
    else
    begin
      Bounds.Top := ButtonsRect.Top;
      Bounds.Bottom := ButtonsRect.Bottom;
      AButtonWidth := CalculateEditDefaultButtonWidth(ACanvas, AViewInfo.ButtonsInfo[AButtonVisibleIndex]);
      if AButtonWidth > ButtonsRect.Right - ButtonsRect.Left then
        AButtonVisibleWidth := ButtonsRect.Right - ButtonsRect.Left
      else
        AButtonVisibleWidth := AButtonWidth;
      if Data.LeftAlignment then
      begin
        Bounds.Left := ButtonsRect.Left;
        Bounds.Right := Bounds.Left + AButtonWidth;
        VisibleBounds := Bounds;
        VisibleBounds.Right := VisibleBounds.Left + AButtonVisibleWidth;
        Inc(ButtonsRect.Left, AButtonVisibleWidth);
        if FLeftSideLeftmostButtonIndex = -1 then
          FLeftSideLeftmostButtonIndex := AButtonVisibleIndex;
        FLeftSideRightmostButtonIndex := AButtonVisibleIndex;
      end else
      begin
        Bounds.Right := ButtonsRect.Right;
        Bounds.Left := Bounds.Right - AButtonWidth;
        VisibleBounds := Bounds;
        VisibleBounds.Left := VisibleBounds.Right - AButtonVisibleWidth;
        Dec(ButtonsRect.Right, AButtonVisibleWidth);
        if FRightSideRightmostButtonIndex = -1 then
          FRightSideRightmostButtonIndex := AButtonVisibleIndex;
        FRightSideLeftmostButtonIndex := AButtonVisibleIndex;
      end;
    end;
  end;
end;

procedure TcxCustomEditViewData.CalculateButtonsViewInfo(ACanvas: TcxCanvas;
  const ABounds: TRect; const P: TPoint; Button: TcxMouseButton; Shift: TShiftState;
  AViewInfo: TcxCustomEditViewInfo; AIsMouseEvent: Boolean);

  procedure CalculateButtonStates(APrevPressedButton: Integer);
  var
    AButtonViewInfo: TcxEditButtonViewInfo;
    AButtonVisibleIndex: Integer;
    ACapturePressing: Boolean;
    AHoldPressing: Boolean;
    AIsButtonPressed: Boolean;
    AMouseButtonPressing: Boolean;
  begin
    for AButtonVisibleIndex := 0 to High(AViewInfo.ButtonsInfo) do
    begin
      AIsButtonPressed := IsButtonPressed(AViewInfo, AButtonVisibleIndex);
      AButtonViewInfo := AViewInfo.ButtonsInfo[AButtonVisibleIndex];
      if not Enabled or not Properties.Buttons[AButtonViewInfo.ButtonIndex].Enabled then
        AButtonViewInfo.Data.State := ebsDisabled
      else
        if AIsButtonPressed or (not IsDesigning and PtInRect(AButtonViewInfo.VisibleBounds, P)) then
        begin
          ACapturePressing := (Button = cxmbNone) and (ssLeft in Shift) and
            (AButtonViewInfo.Data.State = ebsNormal) and (GetCaptureButtonVisibleIndex = AButtonVisibleIndex);
          AMouseButtonPressing := (Button = cxmbLeft) and cxShiftStateLeftOnly(Shift, True);
          AHoldPressing := (AButtonViewInfo.Data.State = ebsPressed) and (ssLeft in Shift);
          if AIsButtonPressed or AMouseButtonPressing or AHoldPressing or ACapturePressing then
            AViewInfo.IsButtonReallyPressed := True;
          if not AIsButtonPressed and cxShiftStateMoveOnly(Shift) and not ACapturePressing then
          begin
            AButtonViewInfo.Data.State := ebsSelected;
            AViewInfo.SelectedButton := AButtonVisibleIndex;
          end
          else
            if (AIsButtonPressed or ACapturePressing and CanPressButton(AViewInfo, AButtonVisibleIndex) or cxShiftStateLeftOnly(Shift, True) and
              ((Button = cxmbLeft) and CanPressButton(AViewInfo, AButtonVisibleIndex) or
              (APrevPressedButton = AButtonVisibleIndex))) or AHoldPressing then
            begin
              AButtonViewInfo.Data.State := ebsPressed;
              AViewInfo.PressedButton := AButtonVisibleIndex;
            end
            else
              AButtonViewInfo.Data.State := ebsNormal;
        end
        else
          AButtonViewInfo.Data.State := ebsNormal;

      DoGetButtonState(AButtonViewInfo, AButtonViewInfo.Data.State);
      CalculateButtonNativeInfo(AButtonViewInfo);
    end;
  end;

  procedure CorrectButtonBounds;
  var
    AButtonsRect: TRect;
    I: Integer;
  begin
    AButtonsRect := cxRectContent(ABounds, GetButtonsExtent(ACanvas));
    if FLeftSideLeftmostButtonIndex <> -1 then
      AViewInfo.ButtonsInfo[FLeftSideLeftmostButtonIndex].Data.Leftmost := True;
    if (FLeftSideRightmostButtonIndex <> -1) and (not ButtonsOnlyStyle or (FRightSideLeftmostButtonIndex = -1)) then
      AViewInfo.ButtonsInfo[FLeftSideRightmostButtonIndex].Data.Rightmost := True;
    if (FRightSideLeftmostButtonIndex <> -1) and (not ButtonsOnlyStyle or (FLeftSideRightmostButtonIndex = -1)) then
      AViewInfo.ButtonsInfo[FRightSideLeftmostButtonIndex].Data.Leftmost := True;
    if FRightSideRightmostButtonIndex <> -1 then
      AViewInfo.ButtonsInfo[FRightSideRightmostButtonIndex].Data.Rightmost := True;
    if ButtonsOnlyStyle then
      for I := 0 to High(AViewInfo.ButtonsInfo) do
        AViewInfo.ButtonsInfo[I].Data.LeftAlignment := False;
  end;

var
  APrevPressedButton: Integer;
  AButtonsRect, APrevButtonsRect: TRect;
begin
  if ButtonVisibleCount = 0 then
  begin
    AViewInfo.SetButtonCount(ButtonVisibleCount);
    Exit;
  end;
  if not(csActive in ContainerState) then
    if (Style.ButtonTransparency = ebtHideInactive) or
      (not(csHotTrack in ContainerState) and (Style.ButtonTransparency = ebtHideUnselected)) then
    begin
      AViewInfo.SetButtonCount(0);
      Exit;
    end;
  AViewInfo.SetButtonCount(ButtonVisibleCount);
  AViewInfo.IsButtonReallyPressed := False;
  if AIsMouseEvent then
    APrevPressedButton := AViewInfo.PressedButton
  else
    APrevPressedButton := -1;
  AViewInfo.PressedButton := -1;
  AViewInfo.SelectedButton := -1;

  FLeftSideLeftmostButtonIndex := -1;
  FLeftSideRightmostButtonIndex := -1;
  FRightSideLeftmostButtonIndex := -1;
  FRightSideRightmostButtonIndex := -1;

  if IsInplace or not (AViewInfo.NativeStyle and AViewInfo.ButtonsInfo[0].Data.ComboBoxStyle and IsWinVistaOrLater) then
    AButtonsRect := cxRectContent(ABounds, GetButtonsExtent(ACanvas))
  else
    AButtonsRect := ABounds;
  APrevButtonsRect := AButtonsRect;
  DoCalculateButtonViewInfos(ACanvas, AButtonsRect, AViewInfo);
  CheckButtonsOnly(AViewInfo, APrevButtonsRect.Right - APrevButtonsRect.Left,
    AButtonsRect.Right - AButtonsRect.Left);

  CalculateButtonStates(APrevPressedButton);
  CorrectButtonBounds;
end;

procedure TcxCustomEditViewData.CalculateButtonViewInfo(ACanvas: TcxCanvas;
  AViewInfo: TcxCustomEditViewInfo; AButtonVisibleIndex: Integer; var ButtonsRect: TRect);
begin
  InitializeButtonInfo(AViewInfo, AButtonVisibleIndex);
  CalculateButtonBounds(ACanvas, AViewInfo, AButtonVisibleIndex, ButtonsRect);
end;

procedure TcxCustomEditViewData.DoCalculateButtonViewInfos(ACanvas: TcxCanvas;
  var AButtonsRect: TRect; AViewInfo: TcxCustomEditViewInfo);
var
  AButton: TcxEditButton;
  AButtonIndex, AButtonVisibleIndex: Integer;
  AButtonViewInfo: TcxEditButtonViewInfo;
begin
  AButtonVisibleIndex := Properties.Buttons.VisibleCount - 1;
  AViewInfo.HasTextButtons := False;
  for AButtonIndex := Properties.Buttons.Count - 1 downto 0 do
  begin
    AButton := Properties.Buttons[AButtonIndex];
    if AButton.Visible then
    begin
      if (AButton.Kind = bkText) and not AViewInfo.HasTextButtons then
      begin
        AViewInfo.HasTextButtons := True;
        ACanvas.Font := Style.GetVisibleFont;
        AViewInfo.PrepareCanvasFont(ACanvas.Canvas);
      end;

      AButtonViewInfo := AViewInfo.ButtonsInfo[AButtonVisibleIndex];
      AButtonViewInfo.ButtonIndex := AButtonIndex;
      AButtonViewInfo.ButtonVisibleIndex := AButtonVisibleIndex;
      AButtonViewInfo.Data.Style := GetButtonsStyle;
      CalculateButtonViewInfo(ACanvas, AViewInfo, AButtonVisibleIndex, AButtonsRect);
      Dec(AButtonVisibleIndex);
    end;
  end;
end;

procedure TcxCustomEditViewData.CalculateEx(ACanvas: TcxCanvas; const ABounds: TRect;
  const P: TPoint; Button: TcxMouseButton; Shift: TShiftState;
  AViewInfo: TcxCustomEditViewInfo; AIsMouseEvent: Boolean);
var
  ANewBounds: TRect;
  ANewP: TPoint;
begin
  AViewInfo.Left := ABounds.Left;
  AViewInfo.Top := ABounds.Top;
  ANewBounds := ABounds;
  OffsetRect(ANewBounds, -ABounds.Left, -ABounds.Top);
  ANewP := Point(P.X - ABounds.Left, P.Y - ABounds.Top);
  Calculate(ACanvas, ANewBounds, ANewP, Button, Shift, AViewInfo, AIsMouseEvent);
end;

procedure TcxCustomEditViewData.CheckButtonsOnly(
  AViewInfo: TcxCustomEditViewInfo; APrevButtonsWidth, AButtonsWidth: Integer);
var
  AVisibleButtonCount: Integer;

  procedure FindStretchableButtons(out AStretchableButtonCount,
    AStretchableButtonsTotalWidth: Integer; out AAllButtonsAreStretchable: Boolean);
  var
    I: Integer;
  begin
    AStretchableButtonCount := 0;
    AStretchableButtonsTotalWidth := 0;
    for I := 0 to AVisibleButtonCount - 1 do
      with AViewInfo.ButtonsInfo[I] do
        if Stretchable and not IgnoreButtonWhileStretching(I) then
        begin
          Inc(AStretchableButtonCount);
          Inc(AStretchableButtonsTotalWidth, Bounds.Right - Bounds.Left);
        end;
    AAllButtonsAreStretchable := AStretchableButtonCount = 0;
    if AAllButtonsAreStretchable then
    begin
      AStretchableButtonCount := AVisibleButtonCount;
      AStretchableButtonsTotalWidth := APrevButtonsWidth - AButtonsWidth;
    end;
  end;

  procedure StretchButton(AButtonVisibleIndex: Integer;
    AButtonViewInfo: TcxEditButtonViewInfo; AButtonWidthCorrection: Integer);
  var
    J: Integer;
  begin
    if AButtonWidthCorrection = 0 then
      Exit;
    if AButtonViewInfo.Data.LeftAlignment then
    begin
      for J := 0 to High(AViewInfo.ButtonsInfo) do
        with AViewInfo.ButtonsInfo[J] do
          if Data.LeftAlignment and (Bounds.Left >= AButtonViewInfo.Bounds.Right) then
          begin
            Inc(Bounds.Left, AButtonWidthCorrection);
            Inc(Bounds.Right, AButtonWidthCorrection);
            VisibleBounds := Bounds;
          end;
      with AButtonViewInfo do
      begin
        Inc(Bounds.Right, AButtonWidthCorrection);
        VisibleBounds.Right := Bounds.Right;
      end;
    end
    else
    begin
      for J := 0 to High(AViewInfo.ButtonsInfo) do
        with AViewInfo.ButtonsInfo[J] do
          if not Data.LeftAlignment and (Bounds.Right <= AButtonViewInfo.Bounds.Left) then
          begin
            Dec(Bounds.Left, AButtonWidthCorrection);
            Dec(Bounds.Right, AButtonWidthCorrection);
            VisibleBounds := Bounds;
          end;
      with AButtonViewInfo do
      begin
        Dec(Bounds.Left, AButtonWidthCorrection);
        VisibleBounds.Left := Bounds.Left;
      end;
    end;
  end;

var
  AAllButtonsAreStretchable: Boolean;
  AButtonViewInfo: TcxEditButtonViewInfo;
  AFirstStretchableButtonWidthCorrection, AButtonWidthCorrection: Integer;
  AStretchableButtonCount, AStretchableButtonsTotalWidth: Integer;
  I: Integer;
begin
  if not ButtonsOnlyStyle or (AButtonsWidth <= 0) then
    Exit;
  AVisibleButtonCount := Length(AViewInfo.ButtonsInfo);
  FindStretchableButtons(AStretchableButtonCount, AStretchableButtonsTotalWidth,
    AAllButtonsAreStretchable);
  AFirstStretchableButtonWidthCorrection := AButtonsWidth;
  for I := AVisibleButtonCount - 1 downto 0 do
  begin
    AButtonViewInfo := AViewInfo.ButtonsInfo[I];
    if not AAllButtonsAreStretchable and (not AButtonViewInfo.Stretchable or IgnoreButtonWhileStretching(I)) then
      Continue;
    Dec(AStretchableButtonCount);
    if AStretchableButtonCount = 0 then
      AButtonWidthCorrection := AFirstStretchableButtonWidthCorrection
    else
    begin
      if AStretchableButtonsTotalWidth < 1 then
        AButtonWidthCorrection := 0
      else
        with AButtonViewInfo.Bounds do
          AButtonWidthCorrection := AButtonsWidth * (Right - Left) div AStretchableButtonsTotalWidth;
      Dec(AFirstStretchableButtonWidthCorrection, AButtonWidthCorrection);
    end;
    StretchButton(I, AButtonViewInfo, AButtonWidthCorrection);
  end;
end;

procedure TcxCustomEditViewData.EditValueToDrawValue(
  const AEditValue: TcxEditValue; AViewInfo: TcxCustomEditViewInfo);
begin
end;

procedure TcxCustomEditViewData.InitializeButtonInfo(AViewInfo: TcxCustomEditViewInfo; AButtonVisibleIndex: Integer);

  function GetButtonWidth(AButton: TcxEditButton): Integer;
  begin
    Result := ScaleFactor.Apply(AButton.Width, Properties.ScaleFactor);
    if Result = 0 then
      Result := DoGetDefaultButtonWidth(AButton.Index);
  end;

var
  AButton: TcxEditButton;
  AButtonContentAlignment: TAlignment;
begin
  with AViewInfo.ButtonsInfo[AButtonVisibleIndex] do
  begin
    AButton := Properties.Buttons[ButtonIndex];
    Data.ComboBoxStyle := (Length(AViewInfo.ButtonsInfo) = 1) and (AButton.Kind = bkDown);
    Data.UseSkins := AViewInfo.UseSkins;
    Data.Kind := AButton.Kind;
    if Data.Kind = bkText then
    begin
      Data.Caption := AButton.Caption;
      if AViewInfo.UseSkins then
        Data.TextColor := AViewInfo.Painter.EditButtonTextColor
      else
        Data.TextColor := AButton.TextColor;
      Data.VisibleCaption := AButton.FVisibleCaption;
    end;
    AButtonContentAlignment := AButton.ContentAlignment;
    if UseRightToLeftAlignment then
      ChangeBiDiModeAlignment(AButtonContentAlignment);
    Data.ContentAlignment := AButtonContentAlignment;
    Data.Default := AButton.Default;
    Images := Properties.Images;
    ImageIndex := AButton.ImageIndex;
    Glyph := AButton.Glyph;
    HasBackground := AViewInfo.HasBackground;
    Data.Gradient := Self.Style.GradientButtons;
    Data.Hint := AButton.Hint;
    if AButton.ActionLink <> nil then
      AButton.ActionLink.PrepareHint(Data.Hint);
    Data.ActionLink := AButton.ActionLink;
    Data.IsInplace := Self.IsInplace;
    Data.LeftAlignment := IsButtonLeftAligned(AViewInfo, AButtonVisibleIndex);
    Data.Leftmost := False;
    Data.Rightmost := False;
    Data.NativeStyle := AViewInfo.NativeStyle;
    Stretchable := AButton.Stretchable;
    Width := GetButtonWidth(AButton);
    Data.Transparent := (Self.Style.ButtonTransparency = ebtAlways) or (Self.Style.ButtonTransparency = ebtInactive) and not Selected;
    Data.BackgroundColor := AViewInfo.BackgroundColor;
  end;
end;

function TcxCustomEditViewData.GetBorderColor: TColor;
var
  AIsHighlightBorder: Boolean;
begin
  AIsHighlightBorder := (csActive in ContainerState) or (csHotTrack in ContainerState) and Style.HotTrack;

  Result := GetBorderColorByPainter(AIsHighlightBorder);
  if Result = clDefault then
  begin
    if Style.BorderStyle in [ebsUltraFlat, ebsOffice11] then
      Result := GetContainerBorderColor(AIsHighlightBorder or IsDesigning and Enabled, Style.BorderStyle = ebsOffice11)
    else
      Result := Style.BorderColor;
  end;
end;

function TcxCustomEditViewData.GetBorderColorByPainter(AIsHighlight: Boolean): TColor;
begin
  Result := clDefault;
  if not (IsInplace or (FEdit = nil)) then
    Result := FEdit.GetBorderColorByPainter(AIsHighlight);
end;

function TcxCustomEditViewData.GetBorderExtent: TRect;
var
  ANativeStyle: Boolean;
  AStyle: TcxCustomEditStyle;
begin
  if IsInplace then
    Result := ContentOffset
  else
  begin
    AStyle := Style;
    ANativeStyle := IsNativeStyle(AStyle.LookAndFeel);
    if AStyle.LookAndFeel.SkinPainter <> nil then
      Result := GetBorderExtentByPainter
    else
      if AStyle.TransparentBorder then
        Result := cxContainerDefaultBorderExtent
      else
        if not AStyle.HasBorder or ANativeStyle and (AStyle.BaseStyle.BorderStyle = ebsNone) or (Edit <> nil) and Edit.Transparent then
          Result := cxEmptyRect
        else
          if ANativeStyle then
            Result := cxContainerDefaultBorderExtent
          else
            Result := GetBorderExtentByEdges(GetContainerBorderWidth(
              TcxContainerBorderStyle(GetBorderStyle)));

      if HasShadow then
      begin
        Inc(Result.Right, cxEditShadowWidth);
        Inc(Result.Bottom, cxEditShadowWidth);
      end;
  end;
end;

function TcxCustomEditViewData.GetBorderExtentByEdges(ABorderWidth: Integer): TRect;
begin
  Result := cxEmptyRect;
  if bLeft in Style.Edges then
    Result.Left := ABorderWidth;
  if bTop in Style.Edges then
    Result.Top := ABorderWidth;
  if bRight in Style.Edges then
    Result.Right := ABorderWidth;
  if bBottom in Style.Edges then
    Result.Bottom := ABorderWidth;
end;

function TcxCustomEditViewData.GetBorderExtentByPainter: TRect;
var
  ABorderStyle: TcxEditBorderStyle;
  ABorderWidth: Integer;
begin
  Result := cxEmptyRect;
  if Style.HasBorder and ((Style.BorderStyle <> ebsNone) or Style.TransparentBorder) then
  begin
    if Style.TransparentBorder then
      ABorderStyle := ebsThick
    else
      ABorderStyle := GetBorderStyle;
    ABorderWidth := Style.LookAndFeel.Painter.GetContainerBorderWidth(
      TcxContainerBorderStyle(ABorderStyle));
    Result := GetBorderExtentByEdges(ABorderWidth);
  end;
end;

function TcxCustomEditViewData.GetBorderStyle: TcxEditBorderStyle;
begin
  if IsInplace then
    Result := ebsNone
  else
  begin
    Result := Style.BorderStyle;
    CorrectBorderStyle(Result);
  end;
end;

function TcxCustomEditViewData.GetButtonNativeTheme(AButtonViewInfo: TcxEditButtonViewInfo): TdxTheme;
begin
  if AButtonViewInfo.Data.ComboBoxStyle then
    Result := OpenTheme(totComboBox)
  else
    Result := OpenTheme(totButton);
end;

function TcxCustomEditViewData.GetButtonsExtent(ACanvas: TcxCanvas): TRect;
var
  ATheme: TdxTheme;
  R, CR: TRect;
begin
  if IsInplace then
    Result := ContentOffset
  else
    if NativeStyle then
    begin
      if not Style.TransparentBorder and (Style.BaseStyle.BorderStyle = ebsNone) then
        Result := cxEmptyRect
      else
        if (Style.BaseStyle.BorderStyle = ebsNone) then
          Result := cxContainerDefaultBorderExtent
        else
        begin
          R := Rect(0, 0, 100, 100);
          ATheme := OpenTheme(totEdit);
          GetThemeBackgroundContentRect(ATheme, ACanvas.Handle, EP_EDITTEXT,
            ETS_NORMAL, R, CR);
          Result := CR;
          Result.Right := R.Right - CR.Right;
          Result.Bottom := R.Bottom - CR.Bottom;
        end;
    end
    else
      Result := GetBorderExtent;
end;

function TcxCustomEditViewData.GetClientExtent(ACanvas: TcxCanvas; AViewInfo: TcxCustomEditViewInfo): TRect;
var
  I: Integer;
  AButtonViewInfo: TcxEditButtonViewInfo;
begin
  Result := GetBorderExtent;
  if (Properties.ButtonsViewStyle <> bvsNormal) and (Length(AViewInfo.ButtonsInfo) > 0) then
    with GetButtonsExtent(ACanvas) do
    begin
      Result.Left := Left;
      Result.Right := Right;
    end;

  if not IsTouchScrollUIMode then
  begin
    if HScrollBar <> nil then
      Inc(Result.Bottom, HScrollBar.Height);
    if VScrollBar <> nil then
      if UseRightToLeftScrollBar then
        Inc(Result.Left, VScrollBar.Width)
      else
        Inc(Result.Right, VScrollBar.Width);
  end;

  for I := 0 to Length(AViewInfo.ButtonsInfo) - 1 do
  begin
    AButtonViewInfo := AViewInfo.ButtonsInfo[I];
    if AButtonViewInfo.Data.LeftAlignment then
      Result.Left := Max(Result.Left, AButtonViewInfo.Bounds.Right - AViewInfo.FEditorBounds.Left)
    else
      Result.Right := Max(Result.Right, AViewInfo.FEditorBounds.Right - AButtonViewInfo.Bounds.Left);
  end;
end;

function TcxCustomEditViewData.GetEditConstantPartSize(ACanvas: TcxCanvas;
  const AEditSizeProperties: TcxEditSizeProperties; var MinContentSize: TSize;
  AViewInfo: TcxCustomEditViewInfo = nil): TSize;
var
  ATempViewInfo: TcxCustomEditViewInfo;
begin
  if AViewInfo = nil then
    ATempViewInfo := TcxCustomEditViewInfo(Properties.GetViewInfoClass.Create)
  else
    ATempViewInfo := AViewInfo;
  try
    if AViewInfo = nil then
      Calculate(ACanvas, Rect(0, 0, cxMaxRectSize, cxMaxRectSize), cxInvalidPoint, cxmbNone,
        [], ATempViewInfo, False);
    Result := InternalGetEditConstantPartSize(ACanvas, IsInplace,
      AEditSizeProperties, MinContentSize, ATempViewInfo);
  finally
    if AViewInfo = nil then
      FreeAndNil(ATempViewInfo);
  end;
end;

function TcxCustomEditViewData.GetEditContentSize(ACanvas: TcxCanvas; const AEditValue:
  TcxEditValue; const AEditSizeProperties: TcxEditSizeProperties; AErrorData: TcxEditValidateInfo): TSize;
begin
  if Properties.ButtonsViewStyle <> bvsNormal then
  begin
    Result.cx := 0;
    ACanvas.Font := Style.GetVisibleFont;
    Result.cy := cxTextHeight(ACanvas.Handle) + Self.GetEditContentSizeCorrection.cy;
  end
  else
    Result := InternalGetEditContentSize(ACanvas, AEditValue, AEditSizeProperties);

  if AErrorData <> nil then
    Result.cx := Result.cx + GetErrorIconWidth(AErrorData);

  if SupportsTouchMode then
    dxAdjustToTouchableSize(Result, ScaleFactor);
end;

function TcxCustomEditViewData.GetEditingContentSize(ACanvas: TcxCanvas;
  const AEditValue: TcxEditValue; const AEditSizeProperties: TcxEditSizeProperties): TSize;
begin
  Result := GetEditContentSize(ACanvas, AEditValue, AEditSizeProperties);
end;

function TcxCustomEditViewData.GetEditContentSizeCorrection: TSize;
begin
  with EditContentParams do
  begin
    Result.cx := Offsets.Left + Offsets.Right + SizeCorrection.cx;
    Result.cy := Offsets.Top + Offsets.Bottom + SizeCorrection.cy;
  end;
end;

function TcxCustomEditViewData.GetEditSize(ACanvas: TcxCanvas;
  const AEditValue: TcxEditValue; AEditSizeProperties: TcxEditSizeProperties;
  AViewInfo: TcxCustomEditViewInfo = nil): TSize;
begin
  Result := InternalGetEditSize(ACanvas, AEditValue, AEditSizeProperties, AViewInfo, False);
end;

function TcxCustomEditViewData.GetEditingSize(ACanvas: TcxCanvas;
  const AEditValue: TcxEditValue; AEditSizeProperties: TcxEditSizeProperties;
  AViewInfo: TcxCustomEditViewInfo = nil): TSize;
begin
  Result := InternalGetEditSize(ACanvas, AEditValue, AEditSizeProperties, AViewInfo, True);
end;

function TcxCustomEditViewData.HasShadow: Boolean;
begin
  Result := IsShadowDrawingNeeded(Self);
end;

function TcxCustomEditViewData.IgnoreButtonWhileStretching(
  AButtonVisibleIndex: Integer): Boolean;
begin
  Result := False;
end;

function TcxCustomEditViewData.IsButtonLeftAligned(AViewInfo: TcxCustomEditViewInfo; AButtonVisibleIndex: Integer): Boolean;
begin
  Result := Properties.Buttons[AViewInfo.ButtonsInfo[AButtonVisibleIndex].ButtonIndex].LeftAlignment xor UseRightToLeftAlignment;
end;

class function TcxCustomEditViewData.IsNativeStyle(ALookAndFeel: TcxLookAndFeel): Boolean;
begin
  Result := AreVisualStylesMustBeUsed(ALookAndFeel.NativeStyle, totEdit);
end;

procedure TcxCustomEditViewData.CalculateButtonNativeInfo(AButtonViewInfo: TcxEditButtonViewInfo);
var
  ATheme: TdxTheme;
begin
  if AButtonViewInfo.Data.NativeStyle then
    ATheme := GetButtonNativeTheme(AButtonViewInfo)
  else
    ATheme := 0;

  CalculateButtonNativePartInfo(ATheme, AButtonViewInfo);
  AButtonViewInfo.Data.BackgroundPartiallyTransparent := AButtonViewInfo.Data.UseSkins or
    (AButtonViewInfo.Data.NativePart <> TC_NONE) and IsThemeBackgroundPartiallyTransparent(
      ATheme, AButtonViewInfo.Data.NativePart, AButtonViewInfo.Data.NativeState);
end;

procedure TcxCustomEditViewData.CalculateButtonNativePartInfo(
  ATheme: TdxTheme; AButtonViewInfo: TcxEditButtonViewInfo);
const
  AlignmentMap: array [Boolean] of Integer = (
    CP_DROPDOWNBUTTONRIGHT, CP_DROPDOWNBUTTONLEFT
  );
  ButtonStateMap: array [Boolean, TcxEditButtonState] of Integer = (
    (PBS_DISABLED, PBS_NORMAL, PBS_PRESSED, PBS_HOT),
    (CBXS_DISABLED, CBXS_NORMAL, CBXS_PRESSED, CBXS_HOT)
  );
begin
  if ATheme = 0 then
    AButtonViewInfo.Data.NativePart := TC_NONE
  else
    if not AButtonViewInfo.Data.ComboBoxStyle then
      AButtonViewInfo.Data.NativePart := BP_PUSHBUTTON
    else
      if IsWinVistaOrLater and not IsInplace then
        AButtonViewInfo.Data.NativePart := AlignmentMap[AButtonViewInfo.Data.LeftAlignment]
      else
        AButtonViewInfo.Data.NativePart := CP_DROPDOWNBUTTON;

  if AButtonViewInfo.Data.NativePart = TC_NONE then
    AButtonViewInfo.Data.NativeState := TC_NONE
  else
    AButtonViewInfo.Data.NativeState := ButtonStateMap[
      AButtonViewInfo.Data.ComboBoxStyle, AButtonViewInfo.Data.State];
end;

function TcxCustomEditViewData.CalculatePaintOptions: TcxEditPaintOptions;
begin
  Result := PaintOptions;
  if not IsRectEmpty(EditContentParams.ExternalBorderBounds) then
    Include(Result, epoHasExternalBorder);
  if ecoShowFocusRectWhenInplace in EditContentParams.Options then
    Include(Result, epoShowFocusRectWhenInplace);
end;

procedure TcxCustomEditViewData.CalculateViewInfo(AViewInfo: TcxCustomEditViewInfo; AIsMouseEvent: Boolean);
var
  ABackgroundColor: TColor;
begin
  AViewInfo.FCalculated := True;
  AViewInfo.NativeStyle := NativeStyle;
  if AViewInfo.NativeStyle then
    AViewInfo.NativeState := GetEditNativeState(AViewInfo)
  else
    AViewInfo.NativeState := TC_NONE;

  if Edit <> nil then
    AViewInfo.Transparent := Edit.IsTransparent;

  AViewInfo.CalculateEditorBounds;

  if AViewInfo.FNeedShowErrorIcon then
  begin
    AViewInfo.FErrorBounds := AViewInfo.FEditorBounds;
    if AViewInfo.Edit = nil then
      AViewInfo.FErrorBounds.Left := AViewInfo.FErrorBounds.Left + 1;

    case FValidationErrorIconAlignment of
      taLeftJustify:
        begin
          AViewInfo.FErrorBounds.Right := AViewInfo.FErrorBounds.Left + GetErrorIconWidth(AViewInfo.ErrorData);
          AViewInfo.FEditorBounds.Left := AViewInfo.FErrorBounds.Right;
        end;
      taRightJustify:
        begin
          AViewInfo.FErrorBounds.Left := AViewInfo.FErrorBounds.Right - GetErrorIconWidth(AViewInfo.ErrorData);
          AViewInfo.FEditorBounds.Right := AViewInfo.FErrorBounds.Left;
        end;
    end;
  end
  else
    AViewInfo.FErrorBounds := cxNullRect;

  AViewInfo.BorderExtent := GetBorderExtent;
  AViewInfo.BorderRect := cxRectContent(AViewInfo.FEditorBounds, AViewInfo.BorderExtent);
  AViewInfo.ShadowRect := AViewInfo.BorderRect;
  AViewInfo.HasContentOffsets := not cxRectIsEqual(ContentOffset, cxNullRect);

  if not IsInplace then
    AViewInfo.BorderColor := GetBorderColor;
  GetColorSettings(AViewInfo, ABackgroundColor, AViewInfo.TextColor);
  AViewInfo.BackgroundColor := ABackgroundColor;
end;

function TcxCustomEditViewData.CanPressButton(AViewInfo: TcxCustomEditViewInfo;
  AButtonVisibleIndex: Integer): Boolean;
begin
  Result := True;
end;

procedure TcxCustomEditViewData.CheckSizeConstraints(var AEditSize: TSize);
begin
  with Edit.Constraints do
  begin
    if (MaxHeight <> 0) and (AEditSize.cy > MaxHeight) then
      AEditSize.cy := MaxHeight;
    if (MinHeight <> 0) and (AEditSize.cy < MinHeight) then
      AEditSize.cy := MinHeight;
    if (MaxWidth <> 0) and (AEditSize.cx > MaxWidth) then
      AEditSize.cx := MaxWidth;
    if (MinWidth <> 0) and (AEditSize.cx < MinWidth) then
      AEditSize.cx := MinWidth;
  end;
end;

procedure TcxCustomEditViewData.CheckStartButtonsFading(AViewInfo: TcxCustomEditViewInfo);
var
  I: Integer;
begin
  for I := 0 to High(AViewInfo.ButtonsInfo) do
    AViewInfo.ButtonsInfo[I].FadingHelper.UpdateState;
end;

procedure TcxCustomEditViewData.DoRightToLeftConversion(AViewInfo: TcxCustomEditViewInfo; const ABounds: TRect);
begin
end;

procedure TcxCustomEditViewData.DoOnGetDisplayText(var AText: string);
begin
  if Assigned(FOnGetDisplayText) then
    FOnGetDisplayText(Self, AText);
end;

procedure TcxCustomEditViewData.CorrectBorderStyle(var ABorderStyle: TcxEditBorderStyle);
begin
  if ABorderStyle in [ebsUltraFlat, ebsOffice11] then
    ABorderStyle := ebsSingle;
end;

function TcxCustomEditViewData.EditValueToDisplayText(AEditValue: TcxEditValue): string;
begin
  Result := InternalEditValueToDisplayText(AEditValue);
  DoOnGetDisplayText(Result);
end;

function TcxCustomEditViewData.GetButtonsStyle: TcxEditButtonStyle;

  function GetDefaultButtonStyle: TcxEditButtonStyle;
  const
    AButtonStyles: array[TcxEditBorderStyle] of TcxEditButtonStyle =
      (btsSimple, btsHotFlat, bts3D, btsFlat, bts3D, btsUltraFlat, btsOffice11);
    AInplaceButtonStyles: array[TcxLookAndFeelKind] of TcxEditButtonStyle =
      (btsFlat, bts3D, btsUltraFlat, btsOffice11);
  begin
    if IsInplace or (Style.BorderStyle = ebsNone) and not Style.HotTrack then
      Result := AInplaceButtonStyles[Style.LookAndFeel.Kind]
    else
      Result := AButtonStyles[Style.BorderStyle];
  end;

begin
  Result := Style.ButtonStyle;
  if Result = btsDefault then
    Result := GetDefaultButtonStyle;
end;

function TcxCustomEditViewData.GetCaptureButtonVisibleIndex: Integer;
begin
  Result := -1;
  if Edit <> nil then
    Result := Edit.FCaptureButtonVisibleIndex;
end;

function TcxCustomEditViewData.GetErrorIconWidth(AErrorData: TcxEditValidateInfo): Integer;
begin
  if NeedShowErrorIcon(AErrorData) then
    Result := dxGetImageSize(AErrorData.ErrorIcon, ScaleFactor).cx + ScaleFactor.Apply(3)
  else
    Result := 0;
end;

procedure TcxCustomEditViewData.GetColorSettings(AViewInfo: TcxCustomEditViewInfo; var FillColor, TextColor: TColor);
const
  ANativePart: array [Boolean] of Integer = (EP_EDITTEXT, EP_BACKGROUND);
var
  AColor: COLORREF;
begin
  AViewInfo.GetColorSettingsByPainter(FillColor, TextColor);
  if TextColor = clDefault then
    TextColor := Style.TextColor;
  if (Edit <> nil) and Edit.DefaultParentColor then
    FillColor := Style.Color
  else
    if FillColor = clDefault then
    begin
      if AViewInfo.NativeStyle and not Enabled and not Style.IsValueAssigned(svColor) then
  //    if AViewInfo.NativeStyle and (AViewInfo.NativeState in [ETS_DISABLED, ETS_READONLY]) and
  //      not Style.IsValueAssigned(svColor) then
      begin
        GetThemeColor(OpenTheme(totEdit), ANativePart[IsWinVistaOrLater],
          AViewInfo.NativeState, TMT_FILLCOLOR, AColor);
        FillColor := AColor;
      end
      else
        FillColor := Style.Color;
    end;
end;

function TcxCustomEditViewData.GetContainerState(const ABounds: TRect;
  const P: TPoint; Button: TcxMouseButton; Shift: TShiftState;
  AIsMouseEvent: Boolean): TcxContainerState;

  function GetEditVisibleBounds: TRect;
  begin
    if Edit <> nil then
      Result := Edit.GetVisibleBounds
    else
      Result := cxRectContent(ABounds, ContentOffset);
  end;

begin
  if Enabled then
    if IsDesigning then
      Result := [csNormal]
    else
    begin
      if Focused then
        Result := [csActive]
      else
        Result := [csNormal];
      if PtInRect(GetEditVisibleBounds, P) and
        (cxShiftStateMoveOnly(Shift) or (Edit <> nil) and (GetCaptureControl = Edit)) then
          Include(Result, csHotTrack);
    end
  else
    Result := [csDisabled];
end;

function TcxCustomEditViewData.GetEditContentDefaultOffsets: TRect;
begin
  Result := EditContentDefaultOffsets[IsInplace];
end;

procedure TcxCustomEditViewData.InitEditContentParams(
  var AParams: TcxEditContentParams);
begin
  AParams.Offsets := GetEditContentDefaultOffsets;
  with AParams.Offsets do
    AParams.SizeCorrection.cx := EditContentMaxTotalDefaultHorzOffset - (Left + Right);
  if not NativeStyle then
    AParams.SizeCorrection.cy := 0
  else
    AParams.SizeCorrection.cy :=
      GetNativeInnerTextEditContentHeightCorrection(Properties, IsInplace);
  AParams.ExternalBorderBounds := cxEmptyRect;
  AParams.Options := [ecoOffsetButtonContent];
end;

function TcxCustomEditViewData.GetEditNativeState(AViewInfo: TcxCustomEditViewInfo): Integer;
begin
  if not Enabled then
    Result := ETS_DISABLED
  else if Focused then
    Result := ETS_SELECTED
  else if csHotTrack in ContainerState then
    Result := ETS_HOT
  else if Properties.ReadOnly then
    Result := ETS_READONLY
  else
    Result := ETS_NORMAL;
end;

procedure TcxCustomEditViewData.InitCacheData;
begin
  ButtonVisibleCount := Properties.Buttons.VisibleCount;
  IsValueSource := Properties.GetEditValueSource(False) = evsValue;
  HorzAlignment := Properties.Alignment.Horz;
  VertAlignment := Properties.Alignment.Vert;
  NativeStyle := IsNativeStyle(Style.LookAndFeel);
  InitEditContentParams(EditContentParams);
end;

procedure TcxCustomEditViewData.Initialize;
begin
  ContentOffset := cxEmptyRect;
  Enabled := True;
end;

function TcxCustomEditViewData.InternalEditValueToDisplayText(
  AEditValue: TcxEditValue): string;
begin
  Result := '';
end;

function TcxCustomEditViewData.InternalFocused: Boolean;
begin
  if Edit <> nil then
    Result := Edit.InternalFocused
  else
    Result := Focused;
end;

function TcxCustomEditViewData.InternalGetEditConstantPartSize(ACanvas: TcxCanvas;
  AIsInplace: Boolean; AEditSizeProperties: TcxEditSizeProperties;
  var MinContentSize: TSize; AViewInfo: TcxCustomEditViewInfo): TSize;
var
  AButton: TcxEditButton;
  AMaxButtonContentHeight, I: Integer;
  AExtend: TRect;
begin
  MinContentSize := cxNullSize;
  if cxRectIsEmpty(AViewInfo.Bounds) or cxRectIsEmpty(AViewInfo.ClientRect) then
  begin
    AExtend := GetClientExtent(ACanvas, AViewInfo);
    Result := Size(AExtend.Left + AExtend.Right, AExtend.Top + AExtend.Bottom);
  end
  else
  begin
    Result.cx := cxRectWidth(AViewInfo.Bounds) - cxRectWidth(AViewInfo.ClientRect);
    Result.cy := cxRectHeight(AViewInfo.Bounds) - cxRectHeight(AViewInfo.ClientRect);
  end;

  AMaxButtonContentHeight := 0;
  if IsInplace then
    for I := 0 to Properties.Buttons.Count - 1 do
    begin
      AButton := Properties.Buttons[I];
      if AButton.Visible and (AButton.Kind = bkText) then
      begin
        ACanvas.Font := Style.GetVisibleFont;
        AViewInfo.PrepareCanvasFont(ACanvas.Canvas);
        Break;
      end;
    end;
  for I := 0 to Properties.Buttons.Count - 1 do
  begin
    AButton := Properties.Buttons[I];
    if not AButton.Visible then
      Continue;
    if (AButton.Kind = bkGlyph) and IsImageAssigned(AButton.Glyph, Properties.Images, AButton.ImageIndex) then
    begin
      AMaxButtonContentHeight := Max(AMaxButtonContentHeight,
        dxGetImageSize(AButton.Glyph, Properties.Images, AButton.ImageIndex, ScaleFactor).cy);
    end;
    if IsInplace and (AButton.Kind = bkText) and (Length(AButton.VisibleCaption) > 0) then
      AMaxButtonContentHeight := Max(AMaxButtonContentHeight, ACanvas.TextHeight(AButton.VisibleCaption));
  end;
  if AMaxButtonContentHeight > 0 then
  begin
    if Style.LookAndFeel.SkinPainter = nil then
      Inc(AMaxButtonContentHeight, cxMarginsHeight(GetButtonsExtent(ACanvas)));
    Inc(AMaxButtonContentHeight, GetEditButtonsContentVerticalOffset(ACanvas, GetButtonsStyle, AViewInfo.NativeStyle));
    MinContentSize.cy := Max(0, AMaxButtonContentHeight - Result.cy);
  end;
//  Result.cx := Result.cx + cxRectWidth(AViewInfo.FErrorBounds); //#AB ToDo:
end;

function TcxCustomEditViewData.InternalGetEditContentSize(ACanvas: TcxCanvas;
  const AEditValue: TcxEditValue; const AEditSizeProperties: TcxEditSizeProperties): TSize;
begin
  Result := cxNullSize;
end;

function TcxCustomEditViewData.InternalGetEditSize(ACanvas: TcxCanvas;
  const AEditValue: TcxEditValue; AEditSizeProperties: TcxEditSizeProperties;
  AViewInfo: TcxCustomEditViewInfo = nil; AIsEditing: Boolean = False): TSize;
var
  AContentSize, AMinContentSize: TSize;
begin
  Result := GetEditConstantPartSize(ACanvas, AEditSizeProperties, AMinContentSize, AViewInfo);
  AEditSizeProperties.Height := AEditSizeProperties.Height - Result.cy;
  AEditSizeProperties.Width := AEditSizeProperties.Width - Result.cx;
  if AIsEditing and (esoEditingAutoHeight in Properties.GetSupportedOperations) then
    AContentSize := GetEditingContentSize(ACanvas, AEditValue, AEditSizeProperties)
  else
    AContentSize := GetEditContentSize(ACanvas, AEditValue, AEditSizeProperties);

  AContentSize := cxSizeMax(AContentSize, AMinContentSize);
  Inc(Result.cx, AContentSize.cx);
  Inc(Result.cy, AContentSize.cy);
  if not IsInplace and (Edit <> nil) then
    CheckSizeConstraints(Result);
end;

function TcxCustomEditViewData.IsButtonPressed(AViewInfo: TcxCustomEditViewInfo;
  AButtonVisibleIndex: Integer): Boolean;
begin
  Result := False;
end;

function TcxCustomEditViewData.NeedShowErrorIcon(AErrorData: TcxEditValidateInfo): Boolean;
begin
  Result := AErrorData.IsError and (evoShowErrorIcon in Properties.ValidationOptions) and IsImageAssigned(AErrorData.ErrorIcon);
end;

procedure TcxCustomEditViewData.DoGetButtonState(
  AViewInfo: TcxEditButtonViewInfo; var AState: TcxEditButtonState);
begin
  if Assigned(AViewInfo.EditViewInfo.FOnGetButtonState) then
    AViewInfo.EditViewInfo.FOnGetButtonState(AViewInfo, AState);
end;

function TcxCustomEditViewData.DoGetDefaultButtonWidth(AIndex: Integer): Integer;
begin
  Result := 0;
  if Assigned(FOnGetDefaultButtonWidth) then
    FOnGetDefaultButtonWidth(Self, AIndex, Result);
end;

function TcxCustomEditViewData.GetStyle: TcxCustomEditStyle;
begin
  Result := TcxCustomEditStyle(FStyle);
end;

procedure TcxCustomEditViewData.SetStyle(Value: TcxCustomEditStyle);
begin
  FStyle := Value;
end;

{ TcxEditButtonViewInfo }

destructor TcxEditButtonViewInfo.Destroy;
begin
  FreeAndNil(FadingHelper);
  inherited Destroy;
end;

procedure TcxEditButtonViewInfo.AfterConstruction;
begin
  inherited AfterConstruction;
  FadingHelper := CreateFadingHelper;
end;

procedure TcxEditButtonViewInfo.Assign(Source: TPersistent);
begin
  if Source is TcxEditButtonViewInfo then
    with Source as TcxEditButtonViewInfo do
    begin
      Self.Data.NativeState := Data.NativeState;
      Self.Data.BackgroundColor := Data.BackgroundColor;
      Self.Bounds := Bounds;
      Self.Data.Style := Data.Style;
      Self.Data.State := Data.State;
    end
  else
    inherited Assign(Source);
end;

function TcxEditButtonViewInfo.CreateFadingHelper: TcxEditButtonFadingHelper;
begin
  Result := TcxEditButtonFadingHelper.Create(Self);
end;

function TcxEditButtonViewInfo.GetUpdateRegion(AViewInfo: TcxEditButtonViewInfo): TcxRegion;
var
  AEquals: Boolean;
begin
  with AViewInfo do
  begin
    AEquals := Self.Data.Style = Data.Style;
    AEquals := AEquals and (Self.Data.State = Data.State);
    AEquals := AEquals and (Self.Data.BackgroundColor = Data.BackgroundColor);
    AEquals := AEquals and (Self.Data.NativeState = Data.NativeState);
  end;
  if AEquals then
    Result := TcxRegion.Create
  else
    Result := TcxRegion.Create(Bounds);
end;

function TcxEditButtonViewInfo.Repaint(AControl: TWinControl;
  AViewInfo: TcxEditButtonViewInfo; const AEditPosition: TPoint): Boolean;
begin
  Result := Data.Style <> AViewInfo.Data.Style;
  Result := Result or (Data.State <> AViewInfo.Data.State);
  Result := Result or (Data.BackgroundColor <> AViewInfo.Data.BackgroundColor);
  Result := Result or (Data.NativeState <> AViewInfo.Data.NativeState);
  Result := Result or (FadingHelper.Active);
  if Result then
    cxRedrawWindow(AControl.Handle, cxRectOffset(Bounds, AEditPosition), HasBackground);
end;

function TcxEditButtonViewInfo.GetScaleFactor: TdxScaleFactor;
begin
  Result := EditViewInfo.ScaleFactor;
end;

{ TcxCustomEditFadingHelper }

function TcxCustomEditFadingHelper.CanFade: Boolean;
begin
  Result := EditViewInfo.IsFadingAvailable and (EditOwner <> nil);
end;

procedure TcxCustomEditFadingHelper.DrawFadeImage;
begin
  Invalidate;
end;

function TcxCustomEditFadingHelper.GetEditOwner: IcxEditOwner;
begin
  if EditViewInfo.Edit <> nil then
    Result := EditViewInfo.Edit
  else
    if not Supports(EditViewInfo.Owner, IcxEditOwner, Result) then
      Result := nil;
end;

procedure TcxCustomEditFadingHelper.Invalidate(const R: TRect; AEraseBackground: Boolean);
begin
  if EditOwner <> nil then
    EditOwner.Invalidate(R, AEraseBackground);
end;

{ TcxEditButtonFadingHelper }

constructor TcxEditButtonFadingHelper.Create(AViewInfo: TcxEditButtonViewInfo);
begin
  inherited Create;
  FViewInfo := AViewInfo;
end;

procedure TcxEditButtonFadingHelper.Invalidate;
begin
  Invalidate(ButtonRect, ViewInfo.HasBackground);
end;

procedure TcxEditButtonFadingHelper.UpdateState;
const
  EditButtonStateToButtonState: array[TcxEditButtonState] of TcxButtonState = (
    cxbsDisabled, cxbsNormal, cxbsPressed, cxbsHot
  );
var
  ANewState: TcxEditButtonState;
begin
  ANewState := ViewInfo.Data.State;
  if ANewState <> State then
  begin
    CheckStartFading(
      EditButtonStateToButtonState[State],
      EditButtonStateToButtonState[ANewState]);
    FState := ANewState;
  end;
end;

function TcxEditButtonFadingHelper.CanFade: Boolean;
begin
  Result := inherited CanFade and EditViewInfo.DoCanStartButtonFading;
end;

procedure TcxEditButtonFadingHelper.DrawButton(ACanvas: TcxCanvas);
var
  AContentRect, ABackgroundRect: TRect;
  APenColor, ABrushColor: TColor;
begin
  EditViewInfo.InternalPrepareEditButtonBackground(ACanvas,
    ViewInfo, AContentRect, ABackgroundRect, APenColor, ABrushColor, False);
end;

function TcxEditButtonFadingHelper.GetButtonRect: TRect;
begin
  Result := ViewInfo.Bounds;
end;

function TcxEditButtonFadingHelper.GetEditViewInfo: TcxCustomEditViewInfo;
begin
  Result := ViewInfo.EditViewInfo;
end;

procedure TcxEditButtonFadingHelper.GetFadingImages(out AFadeOutImage, AFadeInImage: TcxBitmap);
var
  APrevNativeState: Integer;
  APrevState: TcxEditButtonState;
begin
  APrevState := ViewInfo.Data.State;
  APrevNativeState := ViewInfo.Data.NativeState;
  try
    AFadeOutImage := PrepareFadingImage(ebsNormal);
    AFadeInImage := PrepareFadingImage(ebsSelected);
  finally
    ViewInfo.Data.NativeState := APrevNativeState;
    ViewInfo.Data.State := APrevState;
  end;
end;

function TcxEditButtonFadingHelper.PrepareFadingImage(AState: TcxEditButtonState): TcxBitmap32;
var
  AIsViewDataCreated: Boolean;
  AViewData: TcxCustomEditViewData;
begin
  if not EditViewInfo.DoPrepareButtonFadingImage(ViewInfo, AState, Result) then
  begin
    ViewInfo.Data.State := AState;
    AViewData := EditOwner.GetViewData(AIsViewDataCreated);
    try
      AViewData.CalculateButtonNativeInfo(ViewInfo);
      Result := TcxBitmap32.CreateSize(ButtonRect, True);
      Result.cxCanvas.WindowOrg := ButtonRect.TopLeft;
      DrawButton(Result.cxCanvas);
      Result.cxCanvas.WindowOrg := cxNullPoint;
    finally
      if AIsViewDataCreated then
        AViewData.Free;
    end;
  end;
end;

{ TcxEditButtonActionLink }

procedure TcxEditButtonActionLink.AssignClient(AClient: TObject);
begin
  FClient := AClient as TcxCustomEditButton;
end;

function TcxEditButtonActionLink.IsCaptionLinked: Boolean;
begin
  Result := inherited IsCaptionLinked and
    (FClient.Caption = (Action as TCustomAction).Caption);
end;

function TcxEditButtonActionLink.IsEnabledLinked: Boolean;
begin
  Result := inherited IsEnabledLinked and
    (FClient.Enabled = (Action as TCustomAction).Enabled);
end;

function TcxEditButtonActionLink.IsHintLinked: Boolean;
begin
  Result := inherited IsHintLinked and
    (FClient.Hint = (Action as TCustomAction).Hint);
end;

function TcxEditButtonActionLink.IsImageIndexLinked: Boolean;
begin
  Result := inherited IsImageIndexLinked and
    (FClient.ImageIndex = (Action as TCustomAction).ImageIndex);
end;

function TcxEditButtonActionLink.IsVisibleLinked: Boolean;
begin
  Result := inherited IsVisibleLinked and
    (FClient.Visible = (Action as TCustomAction).Visible);
end;

function TcxEditButtonActionLink.PrepareHint(var HintStr: string): Boolean;
begin
  Result := True;
  if Action is TCustomAction then
  begin
    if TCustomAction(Action).DoHint(HintStr) and Application.HintShortCuts and
      (TCustomAction(Action).ShortCut <> scNone) then
    begin
      if HintStr <> '' then
        HintStr := Format('%s (%s)', [HintStr,
          ShortCutToText(TCustomAction(Action).ShortCut)]);
    end;
  end;
end;

procedure TcxEditButtonActionLink.SetCaption(const Value: string);
begin
  if IsCaptionLinked then
    FClient.Caption := Value;
end;

procedure TcxEditButtonActionLink.SetEnabled(Value: Boolean);
begin
  if IsEnabledLinked then
    FClient.Enabled := Value;
end;

procedure TcxEditButtonActionLink.SetHint(const Value: string);
begin
  if IsHintLinked then
    FClient.Hint := Value;
end;

procedure TcxEditButtonActionLink.SetImageIndex(Value: Integer);
begin
  if IsImageIndexLinked then
    FClient.ImageIndex := Value;
end;

procedure TcxEditButtonActionLink.SetVisible(Value: Boolean);
begin
  if IsVisibleLinked then
    FClient.Visible := Value;
end;

{ TcxCustomEditButton }

constructor TcxCustomEditButton.Create(Collection: TCollection);
begin
  Collection.BeginUpdate;
  try
    inherited Create(Collection);
    FGlyph := TdxSmartGlyph.Create;
    FGlyph.OnChange := GlyphChanged;
    FContentAlignment := taCenter;
    FEnabled := True;
    FKind := bkDown;
    FStretchable := True;
    FTextColor := clBtnText;
    FVisible := True;
    FImageIndex := -1;
  finally
    Collection.EndUpdate;
  end;
end;

destructor TcxCustomEditButton.Destroy;
begin
  Action := nil;
  FreeAndNil(FGlyph);
  inherited Destroy;
end;

procedure TcxCustomEditButton.Assign(Source: TPersistent);
begin
  if Source is TcxCustomEditButton then
  begin
    if Assigned(Collection) then Collection.BeginUpdate;
    try
      with Source as TcxCustomEditButton do
      begin
        Self.Caption := Caption;
        Self.Action := Action;
        Self.ImageIndex := ImageIndex;
        Self.FVisibleCaption := FVisibleCaption;
        Self.ContentAlignment := ContentAlignment;
        Self.Default := Default;
        Self.Enabled := Enabled;
        Self.Glyph := Glyph;
        Self.Kind := Kind;
        Self.LeftAlignment := LeftAlignment;
        Self.Stretchable := Stretchable;
        Self.Tag := Tag;
        Self.TextColor := TextColor;
        Self.Visible := Visible;
        Self.Width := Width;
        Self.Hint := Hint;
      end
    finally
      if Assigned(Collection) then Collection.EndUpdate;
    end
  end
  else
    inherited Assign(Source);
end;

function TcxCustomEditButton.GetCollection: TcxCustomEditButtons;
begin
  Result := (inherited Collection as TcxCustomEditButtons);
end;

procedure TcxCustomEditButton.GlyphChanged(Sender: TObject);
begin
  Changed(False);
end;

function TcxCustomEditButton.IsTagStored: Boolean;
begin
  Result := Tag <> 0;
end;

procedure TcxCustomEditButton.SetCaption(const Value: TCaption);
begin
  if Value <> FCaption then
  begin
    FCaption := Value;
    FVisibleCaption := RemoveAccelChars(FCaption);
    Changed(False);
  end;
end;

procedure TcxCustomEditButton.SetContentAlignment(Value: TAlignment);
begin
  if Value <> FContentAlignment then
  begin
    FContentAlignment := Value;
    Changed(False);
  end;
end;

procedure TcxCustomEditButton.SetDefault(Value: Boolean);
var
  I: Integer;
begin
  if FDefault <> Value then
  begin
    if Value and Assigned(Collection) then
      with Collection do
      begin
        for I := 0 to Count - 1 do
          Items[I].FDefault := False;
      end;
    FDefault := Value;
    Changed(True);
  end;
end;

procedure TcxCustomEditButton.SetEnabled(Value: Boolean);
begin
  if Value <> FEnabled then
  begin
    FEnabled := Value;
    Changed(False);
  end;
end;

procedure TcxCustomEditButton.SetGlyph(Value: TdxSmartGlyph);
begin
  Glyph.Assign(Value)
end;

procedure TcxCustomEditButton.SetKind(Value: TcxEditButtonKind);
begin
  if FKind <> Value then
  begin
    FKind := Value;
    Changed(False);
  end;
end;

procedure TcxCustomEditButton.SetLeftAlignment(Value: Boolean);
begin
  if FLeftAlignment <> Value then
  begin
    FLeftAlignment := Value;
    Changed(False);
  end;
end;

procedure TcxCustomEditButton.SetStretchable(Value: Boolean);
begin
  if Value <> FStretchable then
  begin
    FStretchable := Value;
    Changed(False);
  end;
end;

procedure TcxCustomEditButton.SetTextColor(Value: TColor);
begin
  if Value <> FTextColor then
  begin
    FTextColor := Value;
    Changed(False);
  end;
end;

procedure TcxCustomEditButton.SetVisible(Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    Changed(False);
  end;
end;

procedure TcxCustomEditButton.SetWidth(Value: Integer);
begin
  if (FWidth >= 0) and (FWidth <> Value) then
  begin
    FWidth := Value;
    Changed(False);
  end;
end;

function TcxCustomEditButton.GetAction: TBasicAction;
begin
  if ActionLink <> nil then
    Result := ActionLink.Action
  else
    Result := nil;
end;

procedure TcxCustomEditButton.SetAction(Value: TBasicAction);
begin
  if Value <> Action then
  begin
    if Action <> nil then
    begin
      Collection.RemoveFreeNotificatorSender(Action);
      FreeAndNil(FActionLink);
    end;
    if Value <> nil then
    begin
      FActionLink := GetActionLinkClass.Create(Self);
      FActionLink.Action := Value;
      FActionLink.OnChange := DoActionChange;
      ActionChange(Value, csLoading in Value.ComponentState);
      Collection.AddFreeNotificatorSender(Value);
    end;
  end;
end;

procedure TcxCustomEditButton.DoActionChange(Sender: TObject);
begin
  if Sender = Action then
    ActionChange(Sender, False);
end;

procedure TcxCustomEditButton.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  if Sender is TCustomAction then
    with TCustomAction(Sender) do
    begin
      if not CheckDefaults or (Self.Caption = '') then
        Self.Caption := Caption;
      if not CheckDefaults or (Self.Enabled) then
        Self.Enabled := Enabled;
      if not CheckDefaults or (Self.Hint = '') then
        Self.Hint := Hint;
      if not CheckDefaults or (Self.Visible) then
        Self.Visible := Visible;
      if not CheckDefaults or (Self.ImageIndex = -1) then
        Self.ImageIndex := ImageIndex;
    end;
end;

procedure TcxCustomEditButton.ChangeScale(M, D: Integer);
begin
  Width := MulDiv(Width, M, D);
end;

procedure TcxCustomEditButton.FreeNotification(Sender: TComponent);
begin
  if Sender = Action then
    Action := nil;
end;

function TcxCustomEditButton.GetActionLinkClass: TcxEditButtonActionLinkClass;
begin
  Result := TcxEditButtonActionLink;
end;

procedure TcxCustomEditButton.InitiateAction;
begin
  if ActionLink <> nil then
    ActionLink.Update;
end;

procedure TcxCustomEditButton.SetImageIndex(Value: TcxImageIndex);
begin
  if (FImageIndex <> Value) then
  begin
    FImageIndex := Value;
    Changed(False);
  end;
end;

function TcxCustomEditButton.IsCaptionStored: Boolean;
begin
  Result := (ActionLink = nil) or not FActionLink.IsCaptionLinked;
end;

function TcxCustomEditButton.IsEnabledStored: Boolean;
begin
  Result := (ActionLink = nil) or not FActionLink.IsEnabledLinked;
end;

function TcxCustomEditButton.IsHintStored: Boolean;
begin
  Result := (ActionLink = nil) or not FActionLink.IsHintLinked;
end;

function TcxCustomEditButton.IsImageIndexStored: Boolean;
begin
  Result := (ActionLink = nil) or not FActionLink.IsImageIndexLinked;
end;

function TcxCustomEditButton.IsVisibleStored: Boolean;
begin
  Result := (ActionLink = nil) or not FActionLink.IsVisibleLinked;
end;

{ TcxCustomEditButtons }

constructor TcxCustomEditButtons.Create(AOwner: TPersistent; AButtonClass: TcxEditButtonClass);
begin
  FOwner := AOwner;
  inherited Create(AButtonClass);
  FFreeNotificator := TcxFreeNotificator.Create(nil);
  FFreeNotificator.OnFreeNotification := DoFreeNotification;
end;

destructor TcxCustomEditButtons.Destroy;
begin
  FreeAndNil(FFreeNotificator);
  inherited Destroy;
end;

procedure TcxCustomEditButtons.AddFreeNotificatorSender(Sender: TComponent);
begin
  FreeNotificator.AddSender(Sender);
end;

procedure TcxCustomEditButtons.ChangeScale(M, D: Integer);
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := 0 to Count - 1 do
      Items[I].ChangeScale(M, D);
  finally
    EndUpdate;
  end;
end;

procedure TcxCustomEditButtons.DoFreeNotification(Sender: TComponent);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].FreeNotification(Sender);
end;

class function TcxCustomEditButtons.GetButtonClass: TcxEditButtonClass;
begin
  Result := TcxCustomEditButton;
end;

function TcxCustomEditButtons.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TcxCustomEditButtons.RemoveFreeNotificatorSender(Sender: TComponent);
begin
  if FreeNotificator <> nil then
    FreeNotificator.RemoveSender(Sender);
end;

procedure TcxCustomEditButtons.Update(Item: TCollectionItem);
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

function TcxCustomEditButtons.GetItem(Index: Integer): TcxCustomEditButton;
begin
  Result := TcxCustomEditButton(inherited GetItem(Index));
end;

function TcxCustomEditButtons.GetVisibleCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do
    if Items[I].Visible then
      Inc(Result);
end;

procedure TcxCustomEditButtons.InitiateActions;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].InitiateAction;
end;

procedure TcxCustomEditButtons.SetItem(Index: Integer; Value: TcxCustomEditButton);
begin
  inherited SetItem(Index, Value);
end;


{ TcxEditButtons }

function TcxEditButtons.Add: TcxEditButton;
begin
  Result := TcxEditButton(inherited Add);
end;

class function TcxEditButtons.GetButtonClass: TcxEditButtonClass;
begin
  Result := TcxEditButton;
end;

function TcxEditButtons.GetItem(Index: Integer): TcxEditButton;
begin
  Result := TcxEditButton(inherited GetItem(Index));
end;

procedure TcxEditButtons.SetItem(Index: Integer; Value: TcxEditButton);
begin
  inherited SetItem(Index, Value);
end;

{ TcxEditRepositoryItem }

constructor TcxEditRepositoryItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FListenerList := TInterfaceList.Create;
  FScaleFactor := TdxOwnedScaleFactor.Create;
  FProperties := GetEditPropertiesClass.Create(Self);
  Properties.OnPropertiesChanged := PropertiesChanged;
end;

destructor TcxEditRepositoryItem.Destroy;

  procedure RemoveNotification;
  var
    AListener: IcxEditRepositoryItemListener;
  begin
    while FListenerList.Count > 0 do
    begin
      AListener := IcxEditRepositoryItemListener(FListenerList.Last);
      AListener.ItemRemoved(Self);
      RemoveListener(AListener);
    end;
  end;

begin
  RemoveNotification;
  Repository := nil;
  FreeAndNil(FProperties);
  FreeAndNil(FScaleFactor);
  inherited Destroy;
end;

procedure TcxEditRepositoryItem.AddListener(AListener: IcxEditRepositoryItemListener);
begin
  if FListenerList.IndexOf(AListener) = -1 then
    FListenerList.Add(AListener);
end;

class function TcxEditRepositoryItem.GetEditPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxCustomEditProperties;
end;

function TcxEditRepositoryItem.GetBaseName: string;
const
  SSubStr = 'TcxEditRepository';
var
  I: Integer;
begin
  I := Pos(SSubStr, ClassName);
  if I > 0 then
    Result := Copy(ClassName, I + Length(SSubStr), Length(ClassName))
  else
    Result := '';
  Result := Repository.Name + Result;
end;

function TcxEditRepositoryItem.GetParentComponent: TComponent;
begin
  Result := Repository;
end;

function TcxEditRepositoryItem.GetScaleFactor: TdxScaleFactor;
begin
  Result := FScaleFactor;
end;

function TcxEditRepositoryItem.HasParent: Boolean;
begin
  Result := Repository <> nil;
end;

procedure TcxEditRepositoryItem.RemoveListener(AListener: IcxEditRepositoryItemListener);
begin
  FListenerList.Remove(AListener);
end;

procedure TcxEditRepositoryItem.SetParentComponent(AParent: TComponent);
begin
  if not (csLoading in ComponentState) then
    Repository := AParent as TcxEditRepository;
end;

function TcxEditRepositoryItem.ArePropertiesCompatible(APropertiesClass: TClass): Boolean;
begin
  Result := Properties.InheritsFrom(APropertiesClass);
end;

procedure TcxEditRepositoryItem.PropertiesChanged(Sender: TObject);
var
  I: Integer;
begin
  for I := FListenerList.Count - 1 downto 0 do
    IcxEditRepositoryItemListener(FListenerList[I]).PropertiesChanged(Self);
end;

procedure TcxEditRepositoryItem.ReadState(Reader: TReader);
begin
  inherited ReadState(Reader);
  Repository := TcxEditRepository(Reader.Parent);
end;

procedure TcxEditRepositoryItem.SetProperties(Value: TcxCustomEditProperties);
begin
  Properties.Assign(Value);
end;

procedure TcxEditRepositoryItem.SetRepository(Value: TcxEditRepository);
begin
  if FRepository <> Value then
  begin
    if FRepository <> nil then
    begin
      FScaleFactor.Owner := nil;
      FRepository.RemoveItem(Self);
      FRepository := nil;
    end;
    if Value <> nil then
    begin
      FRepository := Value;
      FRepository.AddItem(Self);
      FScaleFactor.Owner := Repository.ScaleFactor;
    end;
  end;
end;

{ TcxEditRepository }

constructor TcxEditRepository.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItems := TList.Create;
end;

destructor TcxEditRepository.Destroy;
begin
  Clear;
  FreeAndNil(FItems);
  inherited Destroy;
end;

procedure TcxEditRepository.Clear;
begin
  while Count > 0 do
    Items[Count - 1].Free;
end;

function TcxEditRepository.CreateItem(ARepositoryItemClass: TcxEditRepositoryItemClass): TcxEditRepositoryItem;
begin
  Result := CreateItemEx(ARepositoryItemClass, Self);
end;

function TcxEditRepository.CreateItemEx(ARepositoryItemClass: TcxEditRepositoryItemClass; AOwner: TComponent): TcxEditRepositoryItem;
begin
  Result := ARepositoryItemClass.Create(AOwner);
  Result.Repository := Self;
end;

function TcxEditRepository.ItemByName(const ARepositoryItemName: string): TcxEditRepositoryItem;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    if AnsiCompareText(Items[I].Name, ARepositoryItemName) = 0 then
      Exit(Items[I]);
  end;
  Result := nil;
end;

procedure TcxEditRepository.AddItem(AItem: TcxEditRepositoryItem);
var
  AIndex: Integer;
begin
  AIndex := FItems.IndexOf(AItem);
  if AIndex = -1 then
    FItems.Add(AItem);
end;

procedure TcxEditRepository.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  AItem: TcxEditRepositoryItem;
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    AItem := Items[I];
    if AItem.Owner = Root then
      Proc(AItem);
  end;
end;

procedure TcxEditRepository.RemoveItem(AItem: TcxEditRepositoryItem);
begin
  FItems.Remove(AItem);
end;

function TcxEditRepository.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TcxEditRepository.GetItem(Index: Integer): TcxEditRepositoryItem;
begin
  Result := TcxEditRepositoryItem(FItems[Index]);
end;

{ TcxDefaultEditStyleController }

constructor TcxDefaultEditStyleController.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Inc(FDefaultEditStyleControllerCount);
end;

destructor TcxDefaultEditStyleController.Destroy;

  procedure ResetDefaultEditStyleController;
  begin
    if DefaultEditStyleController <> nil then
    begin
      DefaultEditStyleController.RestoreStyles;
      DefaultEditStyleController.OnStyleChanged := nil;
    end;
  end;

begin
  Dec(FDefaultEditStyleControllerCount);
  if FDefaultEditStyleControllerCount = 0 then
    ResetDefaultEditStyleController;
  inherited Destroy;
end;

procedure TcxDefaultEditStyleController.RestoreStyles;
begin
  DefaultEditStyleController.RestoreStyles;
end;

procedure TcxDefaultEditStyleController.ChangeScale(M, D: Integer);
begin
  inherited;
  DefaultEditStyleController.ChangeScale(M, D);
end;

function TcxDefaultEditStyleController.GetEmulateStandardControlDrawing: Boolean;
begin
  Result := cxEdit.EmulateStandardControlDrawing;
end;

function TcxDefaultEditStyleController.GetInternalStyle(
  AState: TcxContainerStateItem): TcxCustomEditStyle;
begin
  Result := DefaultEditStyleController.Styles[AState];
end;

function TcxDefaultEditStyleController.GetOnStyleChanged: TNotifyEvent;
begin
  Result := DefaultEditStyleController.OnStyleChanged;
end;

function TcxDefaultEditStyleController.GetStyle: TcxEditStyle;
begin
  Result := DefaultEditStyleController.Style;
end;

function TcxDefaultEditStyleController.GetStyleDisabled: TcxEditStyle;
begin
  Result := DefaultEditStyleController.StyleDisabled;
end;

function TcxDefaultEditStyleController.GetStyleFocused: TcxEditStyle;
begin
  Result := DefaultEditStyleController.StyleFocused;
end;

function TcxDefaultEditStyleController.GetStyleHot: TcxEditStyle;
begin
  Result := DefaultEditStyleController.StyleHot;
end;

procedure TcxDefaultEditStyleController.SetEmulateStandardControlDrawing(
  Value: Boolean);
begin
  SetStandardControlDrawingEmulationMode(Value);
end;

procedure TcxDefaultEditStyleController.SetInternalStyle(
  AState: TcxContainerStateItem; Value: TcxCustomEditStyle);
begin
  DefaultEditStyleController.Styles[AState] := Value;
end;

procedure TcxDefaultEditStyleController.SetOnStyleChanged(Value: TNotifyEvent);
begin
  DefaultEditStyleController.OnStyleChanged := Value;
end;

procedure TcxDefaultEditStyleController.SetStyle(Value: TcxEditStyle);
begin
  DefaultEditStyleController.Style := Value;
end;

procedure TcxDefaultEditStyleController.SetStyleDisabled(Value: TcxEditStyle);
begin
  DefaultEditStyleController.StyleDisabled := Value;
end;

procedure TcxDefaultEditStyleController.SetStyleFocused(Value: TcxEditStyle);
begin
  DefaultEditStyleController.StyleFocused := Value;
end;

procedure TcxDefaultEditStyleController.SetStyleHot(Value: TcxEditStyle);
begin
  DefaultEditStyleController.StyleHot := Value;
end;


{ TcxCustomEditingController }

constructor TcxCustomEditingController.Create(AEditorOwner: TComponent);
begin
  inherited Create;
  FEditList := TcxInplaceEditList.Create(AEditorOwner);
end;

destructor TcxCustomEditingController.Destroy;
begin
  StopEditShowingTimer;
  FEditList.Free;
  inherited Destroy;
end;

procedure TcxCustomEditingController.EditShowingTimerHandler(Sender: TObject);
begin
  StopEditShowingTimer;
  StartEditingByTimer;
end;

procedure TcxCustomEditingController.CancelEditUpdatePost;
begin
  FEditUpdateNeeded := False;
end;

function TcxCustomEditingController.CanHideEdit: Boolean;
begin
  Result := not FEditHiding and IsEditing;
end;

function TcxCustomEditingController.CanRemoveEditFocus: Boolean;
begin
  Result := not (IsEditing and FEdit.IsFocused and not FEditHiding);
end;

function TcxCustomEditingController.CanUpdateEditValue: Boolean;
begin
  Result := IsEditing and
    not (FEditHiding or FInitiatingEditing or FEdit.IsPosting or FEdit.IsEditValidating);
end;

procedure TcxCustomEditingController.CheckEditUpdatePost;
begin
  if FEditUpdateNeeded then
    DoUpdateEdit;
end;

procedure TcxCustomEditingController.ClearErrorState;
begin
  FIsErrorOnPost := False;
end;

procedure TcxCustomEditingController.CloseEdit;
begin
  if IsEditing then
  begin
    UpdateValue;
    HideEdit(False);
  end;
end;

procedure TcxCustomEditingController.DoEditChanged;
begin
end;

procedure TcxCustomEditingController.StartEditAutoHeight(AHeightChanged: Boolean);
begin
end;

procedure TcxCustomEditingController.StartEditShowingTimer;
begin
  StopEditShowingTimer;
  FEditShowingTimer := cxCreateTimer(EditShowingTimerHandler, GetDblClickInterval);
end;

function TcxCustomEditingController.GetChangeEventsCatcher: TcxEditChangeEventsCatcher;
begin
  if Assigned(FEdit) then
    Result := FEdit.ChangeEventsCatcher
  else
    Result := nil;
end;

procedure TcxCustomEditingController.ImmediateEventHandler(ADataController: TcxCustomDataController);
begin
  InvokeEditChangedEvents;
end;

function TcxCustomEditingController.GetFocusRectBounds: TRect;
begin
  Result := cxNullRect;
end;

function TcxCustomEditingController.GetHideEditOnExit: Boolean;
begin
  Result := True;
end;

function TcxCustomEditingController.HasHiddenEditorOnScrollContent: Boolean;
begin
  Result := IsEditing and (Edit.Left = cxInvisibleCoordinate);
end;

procedure TcxCustomEditingController.HideEditOnScrollContent;
begin
  Edit.Left := cxInvisibleCoordinate;
end;

function TcxCustomEditingController.IsNeedInvokeEditChangedEventsBeforePost: Boolean;
begin
  Result := False;
end;

procedure TcxCustomEditingController.InvokeEditChangedEvents;
begin
  if Assigned(Edit) and
    Edit.IsPosting and
   (ChangeEventsCatcher.IsLocked and IsNeedInvokeEditChangedEventsBeforePost) then
      ChangeEventsCatcher.InvokeEditChangedEvents;
end;

procedure TcxCustomEditingController.HideInplaceEditor;
begin
  FEdit.Parent := nil;
end;

procedure TcxCustomEditingController.InitEdit;
begin
  FPrevEditOnChange := FEdit.InternalProperties.OnChange;
  FPrevEditOnEditValueChanged := FEdit.InternalProperties.OnEditValueChanged;
  FEdit.InternalProperties.OnChange := nil;
  FEdit.InternalProperties.OnEditValueChanged := nil;
  UpdateInplaceParamsPosition;
  FEdit.LockChangeEvents(True);
  FEdit.FIsInplaceInitializing := True;
  try
    FEdit.Left := cxInvisibleCoordinate;
    FEdit.PrepareForInplaceActivation;
    FEdit.Parent := GetEditParent;
  finally
    FEdit.FIsInplaceInitializing := False;
    FEdit.LockChangeEvents(False, False);
  end;
  FEditPlaceBounds := cxEmptyRect;
  AssignOverridableEditEvents;
  UpdateEdit;
  UpdateEditValue;
  AssignStaticEditEvents;
  if not IsRectEmpty(FEditPlaceBounds) then
    FEdit.BoundsRect := FEditPlaceBounds;
end;

function TcxCustomEditingController.IsEditVisible: Boolean;
begin
  Result := IsEditing and FEdit.Visible and (FEdit.Left <> cxInvisibleCoordinate);
end;

procedure TcxCustomEditingController.MultilineEditTextChanged;
begin
end;

procedure TcxCustomEditingController.PostEditUpdate;
begin
  FEditUpdateNeeded := True;
end;

procedure TcxCustomEditingController.ShowEditAfterScrollContent;
begin
  DoUpdateEdit;
end;

procedure TcxCustomEditingController.UninitEdit;
begin
  UnassignEditEvents;
end;

function TcxCustomEditingController.GetDataController: TcxCustomDataController;
begin
  Result := nil;
end;

procedure TcxCustomEditingController.SetImmediatePostEventHandler(ASet: Boolean);
var
  ADataController: TcxCustomDataController;
begin
  ADataController := GetDataController;
  if Assigned(ADataController) then
  begin
    if ASet then
      ADataController.OnBeforeImmediatePost := ImmediateEventHandler
    else
      ADataController.OnBeforeImmediatePost := nil;
  end;
end;

procedure TcxCustomEditingController.UpdateEditValue;
begin
  if CanUpdateEditValue then
  begin
    UpdateInplaceParamsPosition;
    FEdit.LockChangeEvents(True);
    try
      FEdit.EditValue := GetValue;
    finally
      FEdit.LockChangeEvents(False, False);
    end;
    FEdit.SelectAll;
  end;
end;

procedure TcxCustomEditingController.UpdateValue;
var
  AModifiedAfterEnter: Boolean;
begin
  if IsEditing and FEdit.EditModified then
  begin
    FEdit.InternalValidateEdit;
    if FEdit <> nil then
    begin
      AModifiedAfterEnter := FEdit.ModifiedAfterEnter;
      SetImmediatePostEventHandler(True);
      try
        try
          SetValue(FEdit.EditValue);
          AModifiedAfterEnter := False;
          ClearErrorState;
        except
          if not HideEditOnFocusedRecordChange and (FEdit <> nil) then
            FEdit.SetFocus;
          FIsErrorOnPost := True;
          raise;
        end;
      finally
        SetImmediatePostEventHandler(False);
        if FEdit <> nil then
          FEdit.ModifiedAfterEnter := AModifiedAfterEnter;
      end;
    end;
  end;
end;

procedure TcxCustomEditingController.AssignOverridableEditEvents;
begin
  with FEdit do
  begin
    OnAfterKeyDown := EditAfterKeyDown;
    OnDblClick := EditDblClick;
    OnKeyDown := EditKeyDown;
    OnKeyPress := EditKeyPress;
    OnKeyUp := EditKeyUp;
  end;
end;

procedure TcxCustomEditingController.AssignStaticEditEvents;
begin
  with FEdit do
  begin
    OnEditing := EditEditing;
    OnPostEditValue := EditPostEditValue;
    OnExit := EditExit;
    OnFocusChanged := EditFocusChanged;
    InternalProperties.OnChange := EditChanged;
    InternalProperties.OnEditValueChanged := EditValueChanged;
  end;
end;

procedure TcxCustomEditingController.UnassignEditEvents;
begin
  with FEdit do
  begin
    OnAfterKeyDown := nil;
    OnEditing := nil;
    OnDblClick := nil;
    OnPostEditValue := nil;
    OnExit := nil;
    OnFocusChanged := nil;
    OnKeyDown := nil;
    OnKeyPress := nil;
    OnKeyUp := nil;
    InternalProperties.OnChange := FPrevEditOnChange;
    InternalProperties.OnEditValueChanged := FPrevEditOnEditValueChanged;
  end;
end;

procedure TcxCustomEditingController.EditAfterKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
end;

procedure TcxCustomEditingController.EditChanged(Sender: TObject);
begin
  DoEditChanged;
  if Assigned(FPrevEditOnChange) then
    FPrevEditOnChange(Sender);
end;

procedure TcxCustomEditingController.EditDblClick(Sender: TObject);
begin
end;

procedure TcxCustomEditingController.EditEditing(Sender: TObject; var CanEdit: Boolean);
begin
  FInitiatingEditing := True;
  try
    CanEdit := CanInitEditing;
  finally
    FInitiatingEditing := False;
  end;
end;

procedure TcxCustomEditingController.EditExit(Sender: TObject);
begin
  if HideEditOnExit then
    try
      HideEdit(not GetCancelEditingOnExit);
    except
      if IsEditing then
      begin
        FEdit.SetFocus;
        FIsErrorOnPost := True;
      end;
      raise;
    end;
end;

procedure TcxCustomEditingController.EditFocusChanged(Sender: TObject);
begin
end;

procedure TcxCustomEditingController.EditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
end;

procedure TcxCustomEditingController.EditKeyPress(Sender: TObject; var Key: Char);
begin
end;

procedure TcxCustomEditingController.EditKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
end;

procedure TcxCustomEditingController.EditPostEditValue(Sender: TObject);
begin
  UpdateValue;
end;

procedure TcxCustomEditingController.EditValueChanged(Sender: TObject);
begin
  if Assigned(FPrevEditOnEditValueChanged) then
    FPrevEditOnEditValueChanged(Sender);
end;

function TcxCustomEditingController.IMEComposition(var AMessage: TMessage): Boolean;
var
  AIntf: IcxInplaceEditIMEHelper;
begin
  Result := IsEditing and Supports(Edit, IcxInplaceEditIMEHelper, AIntf);
  if Result then
    AIntf.IMEComposition(AMessage);
end;

function TcxCustomEditingController.IMEStartComposition: Boolean;
var
  AIntf: IcxInplaceEditIMEHelper;
begin
  if not IsEditing then
    ShowEdit;

  Result := IsEditing;
  if Result then
  begin
    if Supports(Edit, IcxInplaceEditIMEHelper, AIntf) then
      AIntf.IMEStartComposition;
  end;
end;

procedure TcxCustomEditingController.HideEdit(Accept: Boolean);
begin
  CancelEditUpdatePost;
  StopEditShowingTimer;
  if not CanHideEdit then
    Exit;
  FEditHiding := True;
  try
    DoHideEdit(Accept);
    if FEdit <> nil then
    begin
      HideInplaceEditor;
      FEdit.RepositoryItem := nil;
      FEdit := nil;
    end;
    ClearErrorState;
  finally
    FEditHiding := False;
  end;
end;

procedure TcxCustomEditingController.RemoveEdit(AProperties: TcxCustomEditProperties);
begin
  if IsEditing and (Edit = FEditList.FindEdit(AProperties)) then
    ClearEditingItem;
  FEditList.RemoveItem(AProperties);
end;

procedure TcxCustomEditingController.UpdateEdit;
begin
  if (FEdit = nil) or cxRectIsEmpty(GetFocusedCellBounds) then
    Exit;
  if not FInitiatingEditing and FEditPreparing then
    DoUpdateEdit
  else
    PostEditUpdate;
end;

procedure TcxCustomEditingController.StopEditShowingTimer;
begin
  FreeAndNil(FEditShowingTimer);
end;


procedure RegisterAssistants;
begin
  Screen.Cursors[crcxEditMouseWheel] := LoadCursor(HInstance, 'CXEDIT_MOUSEWHEEL');
  FDefaultEditStyleController := TcxEditStyleController.Create(nil);
  cxContainerDefaultStyleController := FDefaultEditStyleController;
  cxFilterConsts.cxFilterGetResourceStringFunc := cxGetResourceString;
  cxEditErrorIcon := TBitmap.Create;
  cxEditErrorIcon.LoadFromResourceName(HInstance, 'CXEDIT_GLYPH_ERROR');
  cxEditWarningIcon := TBitmap.Create;
  cxEditWarningIcon.LoadFromResourceName(HInstance, 'CXEDIT_GLYPH_WARNING');
  cxEditInfoIcon := TBitmap.Create;
  cxEditInfoIcon.LoadFromResourceName(HInstance, 'CXEDIT_GLYPH_INFO');
end;

procedure UnregisterAssistants;
begin
  FreeAndNil(cxEditErrorIcon);
  FreeAndNil(cxEditWarningIcon);
  FreeAndNil(cxEditInfoIcon);
  cxContainerDefaultStyleController := nil;
  FreeAndNil(FDefaultEditStyleController);
  FreeAndNil(FHintWindow);
end;

initialization
  dxUnitsLoader.AddUnit(@RegisterAssistants, @UnregisterAssistants);

finalization
  dxUnitsLoader.RemoveUnit(@UnregisterAssistants);
  FreeAndNil(FRegisteredEditProperties);
  FreeAndNil(FCreatedEditPropertiesList);
  FreeAndNil(FDefaultEditRepository);
end.
