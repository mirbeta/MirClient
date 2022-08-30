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

unit dxTokenEdit;

{$I cxVer.inc}

interface

uses
  Windows, Types, SysUtils, Classes, Graphics, Controls, Math, Variants, StdCtrls, Forms, Messages, ImgList,
  dxCore, dxCoreGraphics, dxCoreClasses, cxClasses, cxControls, cxGraphics, cxGeometry, cxLookAndFeelPainters,
  cxContainer, cxEdit, cxTextEdit, cxGroupBox, dxAutoCompleteWindow, dxFading, dxSmartImage, dxGDIPlusClasses,
  cxListBox, cxDataUtils;

const
  dxTokenEditDefaultDisplayMaskDisplayText = '[DisplayText]';
  dxTokenEditDefaultDisplayMaskText = '[Text]';
  dxTokenEditDefaultDropDownRows = 10;
  dxTokenEditDefaultEditValueDelimiter = ';';
  dxTokenEditDefaultFadeFrameCount: Integer = 5;
  dxTokenEditDefaultInnerEditMinWidth: Integer = 50;
  dxTokenEditDefaultInputDelimiters = ',;';

type
  TdxCustomTokenEdit = class;
  TdxTokenEditController = class;
  TdxTokenEditInnerEdit = class;
  TdxTokenEditCustomTokenViewInfo = class;
  TdxTokenEditTokenViewInfo = class;
  TdxTokenEditViewInfo = class;

  { IdxInnerTokenEdit }

  IdxInnerTokenEdit = interface(IcxInnerTextEdit)
  ['{794E90E0-9825-4BD0-9FAF-C44A032507D7}']
    procedure LockChanges(ALock: Boolean);
    procedure PostToken;
  end;

  { TdxTokenEditToken }

  TdxTokenEditToken = class(TcxButtonGroupItem)
  strict private
    FGlyph: TdxSmartGlyph;
    FHint: string;
    FImageIndex: TcxImageIndex;
    FText: string;

    function GetDisplayText: string;
    procedure SetDisplayText(const Value: string);
    procedure SetGlyph(const Value: TdxSmartGlyph);
    procedure SetHint(const Value: string);
    procedure SetImageIndex(const Value: TcxImageIndex);
    procedure SetText(const Value: string);
  protected
    procedure Changed; virtual;
    function GetDisplayName: string; override;
    procedure GlyphChangeHandler(Sender: TObject); virtual;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property DisplayText: string read GetDisplayText write SetDisplayText;
    property Glyph: TdxSmartGlyph read FGlyph write SetGlyph;
    property Hint: string read FHint write SetHint;
    property ImageIndex: TcxImageIndex read FImageIndex write SetImageIndex default -1;
    property Text: string read FText write SetText;
    property Tag;
  end;

  { TdxTokenEditTokens }

  TdxTokenEditTokens = class(TcxButtonGroupItems)
  strict private
    function GetItem(AIndex: Integer): TdxTokenEditToken;
    procedure SetItem(AIndex: Integer; const AValue: TdxTokenEditToken);
  public
    function Add: TdxTokenEditToken; overload;
    function Add(const AText: string; const ADisplayText: string = ''; const AHint: string = '';
      AGlyph: TdxSmartGlyph = nil; AImageIndex: TcxImageIndex = -1): TdxTokenEditToken; overload;
    function FindByDisplayText(const AText: string): TdxTokenEditToken;
    function FindByText(const AText: string): TdxTokenEditToken;
    //
    property Items[Index: Integer]: TdxTokenEditToken read GetItem write SetItem; default;
  end;

  { TdxTokenEditPropertiesOptionsLookup }

  TdxTokenEditLookupFilterMode = (tefmStartsWith, tefmContains);
  TdxTokenEditLookupFilterSource = (tefsText, tefsDisplayText);
  TdxTokenEditLookupFilterSources = set of TdxTokenEditLookupFilterSource;

  TdxTokenEditPropertiesOptionsLookup = class(TcxOwnedPersistent)
  strict private
    FActive: Boolean;
    FDropDownRows: Integer;
    FFilterMode: TdxTokenEditLookupFilterMode;
    FFilterSources: TdxTokenEditLookupFilterSources;
    FDisplayMask: string;
    FSorted: Boolean;

    procedure SetActive(const Value: Boolean);
    procedure SetDropDownRows(AValue: Integer);
    procedure SetFilterMode(AValue: TdxTokenEditLookupFilterMode);
    procedure SetFilterSources(AValue: TdxTokenEditLookupFilterSources);
    procedure SetDisplayMask(const AValue: string);
    procedure SetSorted(const Value: Boolean);
  protected
    procedure Changed;
    procedure DoAssign(Source: TPersistent); override;
  public
    constructor Create(AOwner: TPersistent); override;
  published
    property Active: Boolean read FActive write SetActive default True;
    property DropDownRows: Integer read FDropDownRows write SetDropDownRows default dxTokenEditDefaultDropDownRows;
    property FilterMode: TdxTokenEditLookupFilterMode read FFilterMode write SetFilterMode default tefmContains;
    property FilterSources: TdxTokenEditLookupFilterSources read FFilterSources write SetFilterSources default [tefsDisplayText, tefsText];
    property DisplayMask: string read FDisplayMask write SetDisplayMask;
    property Sorted: Boolean read FSorted write SetSorted default True;
  end;

  { TdxTokenEditProperties }

  TdxTokenEditElementPosition = (teepNone, teepLeft, teepRight);

  TdxTokenEditCustomDrawTokenEvent = procedure (Sender: TObject;
    ACanvas: TcxCanvas; AViewInfo: TdxTokenEditCustomTokenViewInfo; var AHandled: Boolean) of object;
  TdxTokenEditTokenClickEvent = procedure (Sender: TObject;
    const ATokenText: string; AToken: TdxTokenEditToken) of object;
  TdxTokenEditTokenCollectionChangingEvent = procedure (Sender: TObject;
    const ATokenText: string; AToken: TdxTokenEditToken; var AAllow: Boolean) of object;

  TdxTokenEditProperties = class(TcxCustomTextEditProperties)
  strict private
    FAllowAddCustomTokens: Boolean;
    FCloseGlyphPosition: TdxTokenEditElementPosition;
    FEditValueDelimiter: Char;
    FFirstInvalidTokenIndex: Integer;
    FGlyphPosition: TdxTokenEditElementPosition;
    FInputDelimiters: string;
    FLookup: TdxTokenEditPropertiesOptionsLookup;
    FMaxLineCount: Integer;
    FPostEditValueOnFocusLeave: Boolean;
    FTokens: TdxTokenEditTokens;

    FOnCustomDrawToken: TdxTokenEditCustomDrawTokenEvent;
    FOnTokenAdd: TdxTokenEditTokenCollectionChangingEvent;
    FOnTokenClick: TdxTokenEditTokenClickEvent;
    FOnTokenDelete: TdxTokenEditTokenCollectionChangingEvent;
    FOnTokenGlyphClick: TdxTokenEditTokenClickEvent;

    function IsInputDelimitersStored: Boolean;
    procedure SetAllowAddCustomTokens(AValue: Boolean);
    procedure SetCloseGlyphPosition(const AValue: TdxTokenEditElementPosition);
    procedure SetEditValueDelimiter(const AValue: Char);
    procedure SetGlyphPosition(const AValue: TdxTokenEditElementPosition);
    procedure SetInputDelimiters(const Value: string);
    procedure SetLookup(AValue: TdxTokenEditPropertiesOptionsLookup);
    procedure SetMaxLineCount(AValue: Integer);
    procedure SetPostEditValueOnFocusLeave(const AValue: Boolean);
    procedure SetTokens(AValue: TdxTokenEditTokens);
    procedure TokensChanged(Sender: TObject; AToken: TCollectionItem);
  protected
    function CreateTokens: TdxTokenEditTokens; virtual;
    procedure DoAssign(AProperties: TcxCustomEditProperties); override;
    function EditValueToTokens(const AString, ADelimiters: string): TStrings; overload; virtual;
    class function GetViewDataClass: TcxCustomEditViewDataClass; override;
    procedure ValidateTokens(ATokens: TStrings); virtual;
    function UseLookupData: Boolean; override;

    property FirstInvalidTokenIndex: Integer read FFirstInvalidTokenIndex;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    function EditValueToTokens(const AEditValue: TcxEditValue): TStrings; overload;
    class function GetContainerClass: TcxContainerClass; override;
    class function GetViewInfoClass: TcxContainerViewInfoClass; override;
    function GetEditValueSource(AEditFocused: Boolean): TcxDataEditValueSource; override;
    function GetSpecialFeatures: TcxEditSpecialFeatures; override;
    function GetSupportedOperations: TcxEditSupportedOperations; override;
    function IsResetEditClass: Boolean; override;
    procedure PrepareDisplayValue(const AEditValue: Variant; var DisplayValue: Variant; AEditFocused: Boolean); override;
    function TokensToEditValue(const ATokens: TStrings): TcxEditValue; virtual;
    procedure ValidateDisplayValue(var ADisplayValue: TcxEditValue; var AErrorText: TCaption; var AError: Boolean; AEdit: TcxCustomEdit); override;
  published
    property AllowAddCustomTokens: Boolean read FAllowAddCustomTokens write SetAllowAddCustomTokens default True;
    property AssignedValues;
    property ClearKey;
    property CloseGlyphPosition: TdxTokenEditElementPosition read FCloseGlyphPosition write SetCloseGlyphPosition default teepRight;
    property EditValueDelimiter: Char read FEditValueDelimiter write SetEditValueDelimiter default dxTokenEditDefaultEditValueDelimiter;
    property GlyphPosition: TdxTokenEditElementPosition read FGlyphPosition write SetGlyphPosition default teepLeft;
    property Images;
    property ImeMode;
    property ImeName;
    property ImmediatePost;
    property InputDelimiters: string read FInputDelimiters write SetInputDelimiters stored IsInputDelimitersStored;
    property Lookup: TdxTokenEditPropertiesOptionsLookup read FLookup write SetLookup;
    property MaxLength;
    property MaxLineCount: Integer read FMaxLineCount write SetMaxLineCount default 0;
    property PostEditValueOnFocusLeave: Boolean read FPostEditValueOnFocusLeave write SetPostEditValueOnFocusLeave default False;
    property ReadOnly;
    property Tokens: TdxTokenEditTokens read FTokens write SetTokens;
    property ValidateOnEnter;
    property ValidationErrorIconAlignment;
    property ValidationOptions;
    //
    property OnChange;
    property OnEditValueChanged;
    property OnCustomDrawToken: TdxTokenEditCustomDrawTokenEvent read FOnCustomDrawToken write FOnCustomDrawToken;
    property OnTokenAdd: TdxTokenEditTokenCollectionChangingEvent read FOnTokenAdd write FOnTokenAdd;
    property OnTokenClick: TdxTokenEditTokenClickEvent read FOnTokenClick write FOnTokenClick;
    property OnTokenDelete: TdxTokenEditTokenCollectionChangingEvent read FOnTokenDelete write FOnTokenDelete;
    property OnTokenGlyphClick: TdxTokenEditTokenClickEvent read FOnTokenGlyphClick write FOnTokenGlyphClick;
    property OnValidate;
  end;

  { TdxTokenEditViewData }

  TdxTokenEditViewData = class(TcxCustomTextEditViewData)
  strict private
    function GetProperties: TdxTokenEditProperties;
  protected
    function InternalGetEditConstantPartSize(ACanvas: TcxCanvas; AIsInplace: Boolean;
      AEditSizeProperties: TcxEditSizeProperties; var MinContentSize: TSize; AViewInfo: TcxCustomEditViewInfo): TSize; override;
    function InternalGetEditContentSize(ACanvas: TcxCanvas; const AEditValue: Variant;
      const AEditSizeProperties: TcxEditSizeProperties): TSize; override;
    function ParseEditValue(const AEditValue: Variant): TStrings; virtual;
  public
    procedure Calculate(ACanvas: TcxCanvas; const ABounds: TRect; const P: TPoint; Button: TcxMouseButton;
      Shift: TShiftState; AViewInfo: TcxCustomEditViewInfo; AIsMouseEvent: Boolean); override;
    procedure EditValueToDrawValue(const AEditValue: Variant; AViewInfo: TcxCustomEditViewInfo); override;
    function GetClientExtent(Canvas: TcxCanvas; ViewInfo: TcxCustomEditViewInfo): TRect; override;
    //
    property Properties: TdxTokenEditProperties read GetProperties;
  end;

  { TdxTokenEditHitTest }

  TdxTokenEditHitTest = class
  public
    HitObject: TObject;
    HitObjectPartID: Integer;
    Point: TPoint;

    procedure Reset;
  end;

  { TdxTokenEditCustomTokenViewInfo }

  TdxTokenEditCustomTokenViewInfo = class abstract(TcxIUnknownObject)
  strict private
    FBounds: TRect;
    FOwner: TdxTokenEditViewInfo;

    function GetPainter: TcxCustomLookAndFeelPainter; inline;
    function GetScaleFactor: TdxScaleFactor; inline;
  protected
    FCaptionBounds: TRect;
    FState: TcxButtonState;

    function DoCustomDraw(ACanvas: TcxCanvas): Boolean; virtual;
    procedure DrawBackground(ACanvas: TcxCanvas); virtual;
    procedure DrawContent(ACanvas: TcxCanvas); virtual;
    function GetCaption: string; virtual; abstract;
    function GetCaptionAlignment: TAlignment; virtual;
    function GetContentOffsets: TRect; virtual;
    function GetTextColor(const AState: TcxButtonState): TColor;
  public
    constructor Create(AOwner: TdxTokenEditViewInfo);
    procedure Calculate(const ABounds: TRect); overload; virtual;
    procedure Calculate(const ALeft, ATop, AWidth, AHeight: Integer); overload;
    function CalculateHitTest(AHitTest: TdxTokenEditHitTest): Boolean; virtual;
    procedure Draw(ACanvas: TcxCanvas);
    function Equals(Obj: TObject): Boolean; override;
    function GetHint: string; virtual;
    function IsVisible: Boolean;
    function MeasureSize: TSize; virtual;
    procedure UpdateState(AController: TdxTokenEditController); virtual;
    //
    property Bounds: TRect read FBounds;
    property Caption: string read GetCaption;
    property CaptionAlignment: TAlignment read GetCaptionAlignment;
    property CaptionBounds: TRect read FCaptionBounds;
    property Owner: TdxTokenEditViewInfo read FOwner;
    property Painter: TcxCustomLookAndFeelPainter read GetPainter;
    property ScaleFactor: TdxScaleFactor read GetScaleFactor;
    property State: TcxButtonState read FState;
  end;

  { TdxTokenEditTokenViewInfo }

  TdxTokenEditTokenViewInfoClass = class of TdxTokenEditTokenViewInfo;
  TdxTokenEditTokenViewInfo = class(TdxTokenEditCustomTokenViewInfo, IdxFadingObject)
  strict private const
    GlyphOffset = 4;
  strict private
    FCloseGlyphBounds: TRect;
    FCloseGlyphState: TcxButtonState;
    FGlyphBounds: TRect;
    FItem: TdxTokenEditToken;
    FText: string;
    FTokenIndex: Integer;

    function GetCloseGlyphPosition: TdxTokenEditElementPosition; inline;
    function GetGlyphPosition: TdxTokenEditElementPosition; inline;
    function GetImages: TCustomImageList; inline;
  protected
    procedure CalculateGlyphBounds(var ABounds: TRect; const ASize: TSize; APosition: TdxTokenEditElementPosition); virtual;
    procedure CalculateStates(AController: TdxTokenEditController; out AState, ACloseGlyphState: TcxButtonState); virtual;
    procedure DrawBackground(ACanvas: TcxCanvas); override;
    procedure DrawContent(ACanvas: TcxCanvas); override;
    procedure DrawGlyph(ACanvas: TcxCanvas; const R: TRect); virtual;
    function GetCaption: string; override;
    function GetGlyphSize: TSize; virtual;
    function IsCloseGlyphVisible: Boolean; virtual;
    function IsGlyphVisible: Boolean; virtual;

    // Fading
    function CanFade: Boolean;
    function CreateFadeImage: TcxBitmap32;
    procedure GetFadingImages(out AFadeOutImage, AFadeInImage: TcxBitmap);
    procedure DrawFadeImage;

    property Text: string read FText;
    property TokenIndex: Integer read FTokenIndex;
  public
    constructor Create(AOwner: TdxTokenEditViewInfo); virtual;
    destructor Destroy; override;
    procedure Calculate(const ABounds: TRect); override;
    function CalculateHitTest(AHitTest: TdxTokenEditHitTest): Boolean; override;
    function Clone: TdxTokenEditTokenViewInfo;
    function Equals(Obj: TObject): Boolean; override;
    procedure Initialize(const AText: string; ATokenIndex: Integer; AItem: TdxTokenEditToken);
    function GetHint: string; override;
    function MeasureSize: TSize; override;
    procedure UpdateState(AController: TdxTokenEditController); override;
    //
    property CloseGlyphBounds: TRect read FCloseGlyphBounds;
    property CloseGlyphPosition: TdxTokenEditElementPosition read GetCloseGlyphPosition;
    property CloseGlyphState: TcxButtonState read FCloseGlyphState;
    property GlyphBounds: TRect read FGlyphBounds;
    property GlyphPosition: TdxTokenEditElementPosition read GetGlyphPosition;
    property GlyphSize: TSize read GetGlyphSize;
    property Images: TCustomImageList read GetImages;
    property Item: TdxTokenEditToken read FItem;
  end;

  { TdxTokenEditTokenViewInfoList }

  TdxTokenEditTokenViewInfoList = class(TdxFastObjectList)
  strict private
    function GetItem(Index: Integer): TdxTokenEditTokenViewInfo;
    procedure SetItem(Index: Integer; AValue: TdxTokenEditTokenViewInfo);
  public
    procedure Assign(ASource: TdxTokenEditTokenViewInfoList);
    procedure CalculateHitTest(AHitTest: TdxTokenEditHitTest); virtual;
    function Equals(Obj: TObject): Boolean; override;
    function IndexOf(AToken: TdxTokenEditToken): Integer;
    function First: TdxTokenEditTokenViewInfo;
    function Last: TdxTokenEditTokenViewInfo;
    function LastVisible: TdxTokenEditTokenViewInfo;
    //
    property Items[Index: Integer]: TdxTokenEditTokenViewInfo read GetItem write SetItem; default;
  end;

  { TdxTokenEditMoreTokensViewInfo }

  TdxTokenEditMoreTokensViewInfo = class(TdxTokenEditCustomTokenViewInfo)
  protected
    function GetCaption: string; override;
    function GetCaptionAlignment: TAlignment; override;
  public
    function CalculateHitTest(AHitTest: TdxTokenEditHitTest): Boolean; override;
    function GetHint: string; override;
  end;

  { TdxTokenEditViewInfo }

  TdxTokenEditViewInfo = class(TcxCustomTextEditViewInfo)
  strict private const
    Padding = 1;
  strict private
    FContentOffset: TPoint;
    FEditValue: Variant;
    FMoreTokensItem: TdxTokenEditCustomTokenViewInfo;
    FTokens: TdxTokenEditTokenViewInfoList;

    function CalculateClientSize(const AContentSize: TSize): TSize;
    function GetContentDisplayRect: TRect;
    function GetContentSize: TSize;
    function GetProperties: TdxTokenEditProperties;
    procedure SetContentOffset(const Value: TPoint);
  protected
    EditingTokenIndex: Integer;
    IndentBetweenItems: Integer;
    InnerEditSize: TSize;
    UseRightToLeftAlignment: Boolean;
    UseRightToLeftReading: Boolean;

    // Layout
    function CalculateInnerEditSize: TSize; virtual;
    procedure CalculateLayout; virtual;
    procedure CalculateLayoutCore(const AContentRect: TRect; ARowHeight, AMaxRowCount, AMinItemSize: Integer); virtual;
    procedure CalculateMoreTokensItem(const AContentRect: TRect; ARowHeight, AMinItemSize: Integer); virtual;
    function CalculateRowHeight: Integer; virtual;
    function CanGrowByHeight(AMinItemWidth: Integer): Boolean;
    function IsTooSmallForDisplay(const R, AArea: TRect; AMeasuredSize, AMinSize: Integer): Boolean;

    procedure DrawContent(ACanvas: TcxCanvas); virtual;
    function GetDefaultTokenItemSize: TSize; virtual;
    function GetTokenItemViewInfoClass: TdxTokenEditTokenViewInfoClass; virtual;
    procedure InternalPaint(ACanvas: TcxCanvas); override;
    procedure RefreshTokensViewInfo; virtual;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Assign(Source: TObject); override;
    procedure Calculate; virtual;
    function CalculateClientAutoSize: TSize; virtual;
    function CalculateClientMinSize: TSize; virtual;
    procedure CalculateHitTest(const AHitTest: TdxTokenEditHitTest); virtual;
    function Equals(Obj: TObject): Boolean; override;
    function IsHotTrack(P: TPoint): Boolean; override;
    function IsHotTrack: Boolean; override;

    function GetHintText(APart: Integer): string; override;
    function GetItemByPart(APart: Integer; out AItem: TdxTokenEditCustomTokenViewInfo): Boolean; virtual;
    function GetPart(const P: TPoint): Integer; override;
    function GetPartRect(APart: Integer): TRect; override;
    function NeedShowHint(ACanvas: TcxCanvas; const P: TPoint; const AVisibleBounds: TRect;
      out AText: TCaption; out AIsMultiLine: Boolean; out ATextRect: TRect; AMaxLineCount: Integer = 0): Boolean; override;

    function Repaint(AControl: TWinControl; const AInnerEditRect: TRect; AViewInfo: TcxContainerViewInfo = nil): Boolean; override;
    function UpdateEditValue(const Value: Variant): Boolean;
    procedure UpdateState(AController: TdxTokenEditController); virtual;

    property ContentDisplayRect: TRect read GetContentDisplayRect;
    property ContentOffset: TPoint read FContentOffset write SetContentOffset;
    property ContentSize: TSize read GetContentSize;
    property EditValue: Variant read FEditValue;
    property MoreTokensItem: TdxTokenEditCustomTokenViewInfo read FMoreTokensItem;
    property Properties: TdxTokenEditProperties read GetProperties;
    property Tokens: TdxTokenEditTokenViewInfoList read FTokens;
  end;

  { TdxTokenEditController }

  TdxTokenEditController = class(TcxIUnknownObject)
  strict private
    FFocusedObject: TObject;
    FHitTest: TdxTokenEditHitTest;
    FPressedObject: TObject;
    FViewInfo: TdxTokenEditViewInfo;

    function GetEdit: TdxCustomTokenEdit;
    function GetEditingTokenIndex: Integer;
    function GetInnerEdit: IdxInnerTokenEdit;
    procedure SetEditingTokenIndex(AValue: Integer);
    procedure SetFocusedObject(AValue: TObject);
    procedure SetPressedObject(AValue: TObject);
  protected
    procedure DeleteToken(const ATokenIndex: Integer); virtual;
    procedure ReplaceToken(const ATokenIndex: Integer; const ATokenText: string); virtual;

    function IsEditing: Boolean; virtual;
    procedure StartEditing(ATokenIndex: Integer); virtual;
    procedure StopEditing(APostToken: Boolean); virtual;

    procedure ProcessClick; virtual;
    procedure ProcessDblClick; virtual;
  public
    constructor Create(AViewInfo: TdxTokenEditViewInfo);
    destructor Destroy; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure MouseLeave; virtual;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); virtual;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure UpdateHitTest(const P: TPoint); overload;
    procedure UpdateHitTest; overload;
    procedure UpdateStates; virtual;
    //
    property EditingTokenIndex: Integer read GetEditingTokenIndex write SetEditingTokenIndex;
    property FocusedObject: TObject read FFocusedObject write SetFocusedObject;
    property PressedObject: TObject read FPressedObject write SetPressedObject;
    //
    property Edit: TdxCustomTokenEdit read GetEdit;
    property HitTest: TdxTokenEditHitTest read FHitTest;
    property InnerEdit: IdxInnerTokenEdit read GetInnerEdit;
    property ViewInfo: TdxTokenEditViewInfo read FViewInfo;
  end;

  { TdxTokenEditInnerEdit }

  TdxTokenEditInnerEdit = class(TcxCustomInnerTextEdit, IdxScaleFactor)
  strict private
    FAutoCompleteWindow: TdxCustomAutoCompleteWindow;
    FAutoCompleteWindowCustomSize: TSize;
    FLockCount: Integer;

    procedure AutoCompleteWindowSelectItemHandler(Sender: TObject);
    procedure AutoCompleteWindowStoreSizeHandler(Sender: TObject);
    function GetActiveProperties: TdxTokenEditProperties;
    function GetContainer: TdxCustomTokenEdit;
    function GetController: TdxTokenEditController;
    // IdxScaleFactor
    function GetScaleFactor: TdxScaleFactor;
  protected
    function CreateHelper: TcxCustomInnerTextEditHelper; override;

    procedure Change; override;
    procedure DeletePrevToken;
    procedure DoExit; override;
    procedure LockChanges(ALock: Boolean);
    procedure PostToken;

    // Keyboard
    function CheckShiftState(Shift: TShiftState): Boolean;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    function ProcessKeyBackspace(Shift: TShiftState): Boolean; virtual;
    function ProcessKeyEscape(Shift: TShiftState): Boolean; virtual;
    function ProcessKeyReturn(Shift: TShiftState): Boolean; virtual;

    // AutoComplete
    procedure HideAutoCompleteWindow; virtual;
    function IsAutoCompleteWindowVisible: Boolean;
    procedure RefreshAutoCompleteSuggestions; overload;
    procedure RefreshAutoCompleteSuggestions(const ASearchText: string;
      const AMode: TdxTokenEditLookupFilterMode; const ASources: TdxTokenEditLookupFilterSources); overload;
    procedure RefreshAutoCompleteWindow(ASuggestions: TStrings); virtual;

    property ActiveProperties: TdxTokenEditProperties read GetActiveProperties;
    property Container: TdxCustomTokenEdit read GetContainer;
    property Controller: TdxTokenEditController read GetController;
  public
    destructor Destroy; override;
  end;

  { TdxTokenEditInnerEditHelper }

  TdxTokenEditInnerEditHelper = class(TcxCustomInnerTextEditHelper, IdxInnerTokenEdit)
  public
    // IdxInnerTokenEdit
    procedure LockChanges(ALock: Boolean);
    procedure PostToken;
  end;

  { TdxTokenEditAutoCompleteListBox }

  TdxTokenEditAutoCompleteListBox = class(TdxCustomAutoCompleteInnerListBox)
  protected
    function IsItemWithText(AItem: TdxCustomListBoxItem; const AText: string): Boolean; override;
  end;

  { TdxTokenEditAutoCompleteWindow }

  TdxTokenEditAutoCompleteWindow = class(TdxCustomAutoCompleteWindow)
  protected
    function CalculateSize: TSize; override;
    function CreateInnerListBox: TdxCustomAutoCompleteInnerListBox; override;
  public
    constructor Create(AInnerEdit: TdxTokenEditInnerEdit); reintroduce;
  end;

  { TdxCustomTokenEdit }

  TdxCustomTokenEdit = class(TcxCustomTextEdit)
  strict private
    FController: TdxTokenEditController;

    function GetActiveProperties: TdxTokenEditProperties;
    function GetProperties: TdxTokenEditProperties;
    function GetViewInfo: TdxTokenEditViewInfo;
    procedure SetProperties(Value: TdxTokenEditProperties);
  protected
    procedure AdjustInnerEditPosition; override;
    procedure BoundsChanged; override;
    function CanAutoHeight: Boolean; override;
    function CreateController: TdxTokenEditController; virtual;
    procedure EnabledChanged; override;
    procedure DoFocusChanged; override;
    procedure DoHideEdit(AExit: Boolean); override;
    procedure DoSetSize; override;
    procedure DoShowEdit; override;
    function GetDisplayText: string; override;
    function GetHintText(APart: Integer): string; override;
    function GetInnerEditClass: TControlClass; override;
    function GetViewInfoClass: TcxContainerViewInfoClass; override;
    procedure Initialize; override;

    // Events
    function CanAddToken(AProperties: TdxTokenEditProperties; const ATokenText: string;
      AToken: TdxTokenEditToken): Boolean; virtual;
    function DoTokenAdd(const ATokenText: string; AToken: TdxTokenEditToken): Boolean; virtual;
    procedure DoTokenClick(const ATokenText: string; AToken: TdxTokenEditToken); virtual;
    function DoTokenDelete(const ATokenText: string; AToken: TdxTokenEditToken): Boolean; virtual;
    procedure DoTokenGlyphClick(const ATokenText: string; AToken: TdxTokenEditToken); virtual;

    // Scrolling
    procedure InitScrollBarsParameters; override;
    function NeedsScrollBars: Boolean; override;
    procedure Scroll(AScrollBarKind: TScrollBarKind; AScrollCode: TScrollCode; var AScrollPos: Integer); override;
    procedure ScrollToInnerEdit; virtual;
    function UseInnerControlScrollBarParameters: Boolean; override;

    // Mouse
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseLeave(AControl: TControl); override;
    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;

    // Data
    procedure ReplaceToken(const AText: string; ATokenIndex: Integer = -1); virtual;
    procedure UpdateEditValue(ATokens: TStrings); virtual;

    function InternalGetEditingValue: TcxEditValue; override;
    function InternalSetText(const Value: string): Boolean; override;
    procedure PopulateSizeProperties(var AEditSizeProperties: TcxEditSizeProperties); override;
    procedure PropertiesChanged(Sender: TObject); override;
    procedure SetInternalDisplayValue(Value: Variant); override;
    function SupportsSpelling: Boolean; override;
    procedure SynchronizeDisplayValue; override;
    function TabsNeeded: Boolean; override;
    procedure UpdateHitTest; virtual;
    procedure UpdateInnerEditMaxLength;
    //
    property Controller: TdxTokenEditController read FController;
    property ViewInfo: TdxTokenEditViewInfo read GetViewInfo;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    function HasPopupWindow: Boolean; override;
    //
    property ActiveProperties: TdxTokenEditProperties read GetActiveProperties;
    property Properties: TdxTokenEditProperties read GetProperties write SetProperties;
  end;

  { TdxTokenEdit }

  TdxTokenEdit = class(TdxCustomTokenEdit)
  published
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Properties;
    property ShowHint;
    property Style;
    property StyleDisabled;
    property StyleFocused;
    property StyleHot;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    //
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEditing;
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
  end;

implementation

uses
  RTLConsts, cxEditConsts, StrUtils, dxDPIAwareUtils, dxStringHelper;

const
  ecpTokenSignature   = $00FF0000;

  tehtObject          = 0;
  tehtToken           = 1;
  tehtTokenCloseGlyph = 2;
  tehtTokenGlyph      = 3;
  tehtInnerEdit       = 4;
  tehtMoreTokensItem  = 5;

function dxCallCustomDrawItemEvent(AEvent: TdxTokenEditCustomDrawTokenEvent;
  ASender: TObject; ACanvas: TcxCanvas; AViewInfo: TdxTokenEditCustomTokenViewInfo): Boolean;
begin
  Result := False;
  if Assigned(AEvent) then
    AEvent(ASender, ACanvas, AViewInfo, Result);
end;

procedure dxCallTokenClickEvent(AEvent: TdxTokenEditTokenClickEvent;
  ASender: TObject; const ATokenText: string; AToken: TdxTokenEditToken);
begin
  if Assigned(AEvent) then
    AEvent(ASender, ATokenText, AToken);
end;

function dxCallTokenCollectionChangingEvent(AEvent: TdxTokenEditTokenCollectionChangingEvent;
  ASender: TObject; const ATokenText: string; AToken: TdxTokenEditToken): Boolean;
begin
  Result := True;
  if Assigned(AEvent) then
    AEvent(ASender, ATokenText, AToken, Result);
end;

{ TdxTokenEditToken }

constructor TdxTokenEditToken.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FImageIndex := -1;
  FGlyph := TdxSmartGlyph.Create;
  FGlyph.OnChange := GlyphChangeHandler;
  FText := 'Token' + IntToStr(Index);
end;

destructor TdxTokenEditToken.Destroy;
begin
  FreeAndNil(FGlyph);
  inherited Destroy;
end;

procedure TdxTokenEditToken.Assign(Source: TPersistent);
begin
  if Source is TdxTokenEditToken then
  begin
    ImageIndex := TdxTokenEditToken(Source).ImageIndex;
    Hint := TdxTokenEditToken(Source).Hint;
    Glyph.Assign(TdxTokenEditToken(Source).Glyph);
    Text := TdxTokenEditToken(Source).Text;
  end;
  inherited Assign(Source);
end;

procedure TdxTokenEditToken.Changed;
begin
  DoChanged(Collection, copChanged);
end;

function TdxTokenEditToken.GetDisplayName: string;
begin
  if DisplayText <> '' then
    Result := DisplayText
  else
    Result := Text;
end;

procedure TdxTokenEditToken.GlyphChangeHandler(Sender: TObject);
begin
  Changed;
end;

function TdxTokenEditToken.GetDisplayText: string;
begin
  Result := Caption;
end;

procedure TdxTokenEditToken.SetDisplayText(const Value: string);
begin
  Caption := Value;
end;

procedure TdxTokenEditToken.SetGlyph(const Value: TdxSmartGlyph);
begin
  Glyph.Assign(Value);
end;

procedure TdxTokenEditToken.SetHint(const Value: string);
begin
  if Value <> FHint then
  begin
    FHint := Value;
    Changed;
  end;
end;

procedure TdxTokenEditToken.SetImageIndex(const Value: TcxImageIndex);
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    Changed;
  end;
end;

procedure TdxTokenEditToken.SetText(const Value: string);
begin
  if Value <> FText then
  begin
    FText := Value;
    Changed;
  end;
end;

{ TdxTokenEditTokens }

function TdxTokenEditTokens.Add: TdxTokenEditToken;
begin
  Result := TdxTokenEditToken(inherited Add);
end;

function TdxTokenEditTokens.Add(const AText: string; const ADisplayText: string = '';
  const AHint: string = ''; AGlyph: TdxSmartGlyph = nil; AImageIndex: TcxImageIndex = -1): TdxTokenEditToken;
begin
  BeginUpdate;
  try
    Result := Add;
    Result.Text := AText;
    Result.DisplayText := ADisplayText;
    Result.ImageIndex := AImageIndex;
    Result.Hint := AHint;
    Result.Glyph := AGlyph;
  finally
    EndUpdate;
  end;
end;

function TdxTokenEditTokens.FindByDisplayText(const AText: string): TdxTokenEditToken;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    if dxSameText(AText, Items[I].DisplayText) then
      Exit(Items[I]);
  end;
  Result := nil;
end;

function TdxTokenEditTokens.FindByText(const AText: string): TdxTokenEditToken;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    if dxSameText(AText, Items[I].Text) then
      Exit(Items[I]);
  end;
  Result := nil;
end;

function TdxTokenEditTokens.GetItem(AIndex: Integer): TdxTokenEditToken;
begin
  Result := TdxTokenEditToken(inherited Items[AIndex]);
end;

procedure TdxTokenEditTokens.SetItem(AIndex: Integer; const AValue: TdxTokenEditToken);
begin
  inherited Items[AIndex] := AValue;
end;

{ TdxTokenEditPropertiesOptionsLookup }

constructor TdxTokenEditPropertiesOptionsLookup.Create(AOwner: TPersistent);
begin
  inherited;
  FDropDownRows := dxTokenEditDefaultDropDownRows;
  FFilterMode := tefmContains;
  FFilterSources := [tefsText, tefsDisplayText];
  FActive := True;
  FSorted := True;
  FDisplayMask := '';
end;

procedure TdxTokenEditPropertiesOptionsLookup.Changed;
begin
  (Owner as TdxTokenEditProperties).Changed;
end;

procedure TdxTokenEditPropertiesOptionsLookup.DoAssign(Source: TPersistent);
begin
  if Source is TdxTokenEditPropertiesOptionsLookup then
  begin
    Active := TdxTokenEditPropertiesOptionsLookup(Source).Active;
    DropDownRows := TdxTokenEditPropertiesOptionsLookup(Source).DropDownRows;
    FilterMode := TdxTokenEditPropertiesOptionsLookup(Source).FilterMode;
    FilterSources := TdxTokenEditPropertiesOptionsLookup(Source).FilterSources;
    DisplayMask := TdxTokenEditPropertiesOptionsLookup(Source).DisplayMask;
    Sorted := TdxTokenEditPropertiesOptionsLookup(Source).Sorted;
  end;
end;

procedure TdxTokenEditPropertiesOptionsLookup.SetActive(const Value: Boolean);
begin
  if FActive <> Value then
  begin
    FActive := Value;
    Changed;
  end;
end;

procedure TdxTokenEditPropertiesOptionsLookup.SetDropDownRows(AValue: Integer);
begin
  AValue := Max(AValue, 1);
  if AValue <> FDropDownRows then
  begin
    FDropDownRows := AValue;
    Changed;
  end;
end;

procedure TdxTokenEditPropertiesOptionsLookup.SetFilterMode(AValue: TdxTokenEditLookupFilterMode);
begin
  if FFilterMode <> AValue then
  begin
    FFilterMode := AValue;
    Changed;
  end;
end;

procedure TdxTokenEditPropertiesOptionsLookup.SetFilterSources(AValue: TdxTokenEditLookupFilterSources);
begin
  if FilterSources <> AValue then
  begin
    FFilterSources := AValue;
    Changed;
  end;
end;

procedure TdxTokenEditPropertiesOptionsLookup.SetDisplayMask(const AValue: string);
begin
  if FDisplayMask <> AValue then
  begin
    FDisplayMask := AValue;
    Changed;
  end;
end;

procedure TdxTokenEditPropertiesOptionsLookup.SetSorted(const Value: Boolean);
begin
  if FSorted <> Value then
  begin
    FSorted := Value;
    Changed;
  end;
end;

{ TdxTokenEditProperties }

constructor TdxTokenEditProperties.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FTokens := CreateTokens;
  FTokens.OnChange := TokensChanged;
  FLookup := TdxTokenEditPropertiesOptionsLookup.Create(Self);
  FEditValueDelimiter := dxTokenEditDefaultEditValueDelimiter;
  FInputDelimiters := dxTokenEditDefaultInputDelimiters;
  FCloseGlyphPosition := teepRight;
  FGlyphPosition := teepLeft;
  FAllowAddCustomTokens := True;
  FPostEditValueOnFocusLeave := False;
end;

destructor TdxTokenEditProperties.Destroy;
begin
  FreeAndNil(FTokens);
  FreeAndNil(FLookup);
  inherited Destroy;
end;

function TdxTokenEditProperties.EditValueToTokens(const AEditValue: Variant): TStrings;
begin
  Result := EditValueToTokens(VarToStr(AEditValue), EditValueDelimiter);
end;

class function TdxTokenEditProperties.GetContainerClass: TcxContainerClass;
begin
  Result := TdxTokenEdit;
end;

function TdxTokenEditProperties.GetEditValueSource(AEditFocused: Boolean): TcxDataEditValueSource;
begin
  if (IDefaultValuesProvider <> nil) and IDefaultValuesProvider.IsBlob then
    Result := evsValue
  else
    Result := evsText;
end;

function TdxTokenEditProperties.GetSpecialFeatures: TcxEditSpecialFeatures;
begin
  Result := inherited GetSpecialFeatures + [esfMultiRow, esfMinSize];
end;

function TdxTokenEditProperties.GetSupportedOperations: TcxEditSupportedOperations;
begin
  Result := [esoAlwaysHotTrack, esoAutoHeight, esoEditing, esoNeedHandle, esoFiltering, esoSorting];
end;

function TdxTokenEditProperties.IsResetEditClass: Boolean;
begin
  Result := False;
end;

class function TdxTokenEditProperties.GetViewInfoClass: TcxContainerViewInfoClass;
begin
  Result := TdxTokenEditViewInfo;
end;

procedure TdxTokenEditProperties.PrepareDisplayValue(
  const AEditValue: Variant; var DisplayValue: Variant; AEditFocused: Boolean);
var
  ATokens: TStrings;
begin
  ATokens := EditValueToTokens(AEditValue);
  try
    DisplayValue := TokensToEditValue(ATokens);
  finally
    ATokens.Free;
  end;
end;

function TdxTokenEditProperties.TokensToEditValue(const ATokens: TStrings): TcxEditValue;
var
  AEditValueBuilder: TStringBuilder;
  ATempList: TStringList;
  I: Integer;
begin
  if ATokens.Count = 0 then
    Exit('');

  ATempList := TStringList.Create;
  try
    ATempList.Assign(ATokens);
    ValidateTokens(ATempList);

    AEditValueBuilder := TdxStringBuilderManager.Get;
    try
      for I := 0 to ATempList.Count - 1 do
      begin
        if I > 0 then
          AEditValueBuilder.Append(EditValueDelimiter);
        AEditValueBuilder.Append(ATempList[I]);
      end;
      Result := AEditValueBuilder.ToString;
    finally
      TdxStringBuilderManager.Release(AEditValueBuilder);
    end;
  finally
    ATempList.Free;
  end;
end;

procedure TdxTokenEditProperties.ValidateDisplayValue(
  var ADisplayValue: TcxEditValue; var AErrorText: TCaption; var AError: Boolean; AEdit: TcxCustomEdit);
var
  AIsUserErrorDisplayValue: Boolean;
  AToken: TcxEditValue;
  ATokens: TStrings;
  I: Integer;
begin
  if CanValidate then
  begin
    ATokens := EditValueToTokens(ADisplayValue);
    try
      FFirstInvalidTokenIndex := -1;
      if ATokens.Count = 0 then
      begin
        AToken := EmptyStr;
        DoValidate(AToken, AErrorText, AError, AEdit, AIsUserErrorDisplayValue);
        ATokens.DelimitedText := AToken;
      end
      else
        for I := 0 to ATokens.Count - 1 do
        begin
          AToken := ATokens[I];
          DoValidate(AToken, AErrorText, AError, AEdit, AIsUserErrorDisplayValue);
          if AError and (FirstInvalidTokenIndex = -1) then
            FFirstInvalidTokenIndex := I;
          ATokens[I] := AToken;
        end;

      ADisplayValue := TokensToEditValue(ATokens);
      if AError then
      begin
        if AErrorText = '' then
          AErrorText := GetValidateErrorText(ekDefault);
      end;
    finally
      ATokens.Free;
    end;
  end;
end;

function TdxTokenEditProperties.CreateTokens: TdxTokenEditTokens;
begin
  Result := TdxTokenEditTokens.Create(Self, TdxTokenEditToken);
end;

procedure TdxTokenEditProperties.DoAssign(AProperties: TcxCustomEditProperties);
begin
  inherited DoAssign(AProperties);

  if AProperties is TdxTokenEditProperties then
  begin
    Lookup := TdxTokenEditProperties(AProperties).Lookup;
    AllowAddCustomTokens := TdxTokenEditProperties(AProperties).AllowAddCustomTokens;
    CloseGlyphPosition := TdxTokenEditProperties(AProperties).CloseGlyphPosition;
    GlyphPosition := TdxTokenEditProperties(AProperties).GlyphPosition;
    EditValueDelimiter := TdxTokenEditProperties(AProperties).EditValueDelimiter;
    InputDelimiters := TdxTokenEditProperties(AProperties).InputDelimiters;
    MaxLineCount := TdxTokenEditProperties(AProperties).MaxLineCount;
    PostEditValueOnFocusLeave := TdxTokenEditProperties(AProperties).PostEditValueOnFocusLeave;
    Tokens.Assign(TdxTokenEditProperties(AProperties).Tokens);

    OnCustomDrawToken := TdxTokenEditProperties(AProperties).OnCustomDrawToken;
    OnTokenAdd := TdxTokenEditProperties(AProperties).OnTokenAdd;
    OnTokenClick := TdxTokenEditProperties(AProperties).OnTokenClick;
    OnTokenDelete := TdxTokenEditProperties(AProperties).OnTokenDelete;
    OnTokenGlyphClick := TdxTokenEditProperties(AProperties).OnTokenGlyphClick;
  end;
end;

function TdxTokenEditProperties.EditValueToTokens(const AString, ADelimiters: string): TStrings;
var
  P, P1: PChar;
  S: string;
  L: Integer;
begin
  Result := TStringList.Create;
  P := PChar(AString);
  L := Length(AString);
  while L > 0 do
  begin
    P1 := P;
    while (L > 0) and (Pos(P^, ADelimiters) = 0) do
    begin
      Inc(P);
      Dec(L);
    end;
    SetString(S, P1, P - P1);

    S := Trim(S);
    if (S <> '') and (Result.IndexOf(S) < 0) then
      Result.AddObject(S, Tokens.FindByText(S));
    if (L > 0) and (Pos(P^, ADelimiters) > 0) then
    begin
      Inc(P);
      Dec(L);
    end;
  end;
end;

class function TdxTokenEditProperties.GetViewDataClass: TcxCustomEditViewDataClass;
begin
  Result := TdxTokenEditViewData;
end;

procedure TdxTokenEditProperties.ValidateTokens(ATokens: TStrings);
var
  AToken: string;
  I: Integer;
begin
  for I := ATokens.Count - 1 downto 0 do
  begin
    AToken := Trim(ATokens[I]);
    if AToken <> '' then
      ATokens[I] := AToken
    else
      ATokens.Delete(I);
  end;

  // Remove duplicates
  for I := ATokens.Count - 1 downto 0 do
  begin
    if ATokens.IndexOf(ATokens[I]) <> I then
      ATokens.Delete(I);
  end;
end;

function TdxTokenEditProperties.UseLookupData: Boolean;
begin
  Result := False;
end;

function TdxTokenEditProperties.IsInputDelimitersStored: Boolean;
begin
  Result := FInputDelimiters <> dxTokenEditDefaultInputDelimiters;
end;

procedure TdxTokenEditProperties.SetAllowAddCustomTokens(AValue: Boolean);
begin
  if AValue <> FAllowAddCustomTokens then
  begin
    FAllowAddCustomTokens := AValue;
    Changed;
  end;
end;

procedure TdxTokenEditProperties.SetCloseGlyphPosition(const AValue: TdxTokenEditElementPosition);
begin
  if AValue <> FCloseGlyphPosition then
  begin
    FCloseGlyphPosition := AValue;
    Changed;
  end;
end;

procedure TdxTokenEditProperties.SetEditValueDelimiter(const AValue: Char);
begin
  if AValue <> FEditValueDelimiter then
  begin
    FEditValueDelimiter := AValue;
    Changed;
  end;
end;

procedure TdxTokenEditProperties.SetGlyphPosition(const AValue: TdxTokenEditElementPosition);
begin
  if AValue <> FGlyphPosition then
  begin
    FGlyphPosition := AValue;
    Changed;
  end;
end;

procedure TdxTokenEditProperties.SetInputDelimiters(const Value: string);
begin
  if (Value <> FInputDelimiters) and (Value <> '') then
  begin
    FInputDelimiters := Value;
    Changed;
  end;
end;

procedure TdxTokenEditProperties.SetLookup(AValue: TdxTokenEditPropertiesOptionsLookup);
begin
  FLookup.Assign(AValue);
end;

procedure TdxTokenEditProperties.SetMaxLineCount(AValue: Integer);
begin
  AValue := Max(AValue, 0);
  if MaxLineCount <> AValue then
  begin
    FMaxLineCount := AValue;
    Changed;
  end;
end;

procedure TdxTokenEditProperties.SetPostEditValueOnFocusLeave(const AValue: Boolean);
begin
  if FPostEditValueOnFocusLeave <> AValue then
  begin
    FPostEditValueOnFocusLeave := AValue;
    Changed;
  end;
end;

procedure TdxTokenEditProperties.SetTokens(AValue: TdxTokenEditTokens);
begin
  FTokens.Assign(AValue);
  Changed;
end;

procedure TdxTokenEditProperties.TokensChanged(Sender: TObject; AToken: TCollectionItem);
begin
  Changed;
end;

{ TdxTokenEditViewData }

procedure TdxTokenEditViewData.Calculate(ACanvas: TcxCanvas; const ABounds: TRect; const P: TPoint;
  Button: TcxMouseButton; Shift: TShiftState; AViewInfo: TcxCustomEditViewInfo; AIsMouseEvent: Boolean);
var
  AController: TdxTokenEditController;
begin
  inherited Calculate(ACanvas, ABounds, P, Button, Shift, AViewInfo, AIsMouseEvent);
  TdxTokenEditViewInfo(AViewInfo).UseRightToLeftAlignment := UseRightToLeftAlignment;
  TdxTokenEditViewInfo(AViewInfo).UseRightToLeftReading := UseRightToLeftReading;
  TdxTokenEditViewInfo(AViewInfo).Calculate;

  if (Edit = nil) and AIsMouseEvent then
  begin
    AController := TdxTokenEditController.Create(TdxTokenEditViewInfo(AViewInfo));
    try
      AController.UpdateHitTest(P);
      AController.UpdateStates;
    finally
      AController.Free;
    end;
  end;
end;

procedure TdxTokenEditViewData.EditValueToDrawValue(const AEditValue: Variant; AViewInfo: TcxCustomEditViewInfo);
var
  ADisplayValue: Variant;
begin
  Properties.PrepareDisplayValue(AEditValue, ADisplayValue, InternalFocused);
  TdxTokenEditViewInfo(AViewInfo).UpdateEditValue(ADisplayValue);
end;

function TdxTokenEditViewData.InternalGetEditConstantPartSize(ACanvas: TcxCanvas; AIsInplace: Boolean;
  AEditSizeProperties: TcxEditSizeProperties; var MinContentSize: TSize; AViewInfo: TcxCustomEditViewInfo): TSize;
begin
  Result := inherited InternalGetEditConstantPartSize(ACanvas, AIsInplace, AEditSizeProperties, MinContentSize, AViewInfo);
  if (AViewInfo <> nil) and (TdxTokenEditViewInfo(AViewInfo).Painter <> nil) then
    MinContentSize := cxSizeMax(MinContentSize, TdxTokenEditViewInfo(AViewInfo).CalculateClientMinSize);
end;

function TdxTokenEditViewData.InternalGetEditContentSize(ACanvas: TcxCanvas;
  const AEditValue: Variant; const AEditSizeProperties: TcxEditSizeProperties): TSize;
var
  ASize: TSize;
  AViewInfo: TdxTokenEditViewInfo;
begin
  AViewInfo := Properties.GetViewInfoClass.Create as TdxTokenEditViewInfo;
  try
    AViewInfo.FEdit := Edit;
    EditValueToDrawValue(AEditValue, AViewInfo);

    MaxLineCount := AEditSizeProperties.MaxLineCount;
    if Properties.MaxLineCount > 0 then
    begin
      if MaxLineCount > 0 then
        MaxLineCount := Min(MaxLineCount, Properties.MaxLineCount)
      else
        MaxLineCount := Properties.MaxLineCount;
    end;

    ASize := cxSize(cxMaxRectSize, cxMaxRectSize);
    if AEditSizeProperties.Width > 0 then
      ASize.cx := AEditSizeProperties.Width;
    inherited Calculate(ACanvas, cxRect(ASize), cxInvalidPoint, cxmbNone, [], AViewInfo, False);
    AViewInfo.ClientRect := AViewInfo.Bounds;
    TdxTokenEditViewInfo(AViewInfo).Calculate;
    Result := AViewInfo.CalculateClientAutoSize;
  finally
    AViewInfo.Free;
  end;
end;

function TdxTokenEditViewData.ParseEditValue(const AEditValue: Variant): TStrings;
begin
  Result := Properties.EditValueToTokens(AEditValue);
end;

function TdxTokenEditViewData.GetClientExtent(Canvas: TcxCanvas; ViewInfo: TcxCustomEditViewInfo): TRect;
var
  AButtonViewInfo: TcxEditButtonViewInfo;
  AViewInfo: TdxTokenEditViewInfo;
  I: Integer;
begin
  Result := GetBorderExtent;
  AViewInfo := ViewInfo as TdxTokenEditViewInfo;
  for I := 0 to Length(AViewInfo.ButtonsInfo) - 1 do
  begin
    AButtonViewInfo := AViewInfo.ButtonsInfo[I];
    if AButtonViewInfo.Data.LeftAlignment then
      Result.Left := Max(Result.Left, AButtonViewInfo.Bounds.Right - AViewInfo.FEditorBounds.Left)
    else
      Result.Right := Max(Result.Right, AViewInfo.FEditorBounds.Right - AButtonViewInfo.Bounds.Left);
  end;
  if not IsTouchScrollUIMode and (VScrollBar <> nil) then
    if not UseRightToLeftScrollBar then
      Inc(Result.Right, VScrollBar.Width)
    else
      Inc(Result.Left, VScrollBar.Width);
end;

function TdxTokenEditViewData.GetProperties: TdxTokenEditProperties;
begin
  Result := TdxTokenEditProperties(FProperties);
end;

{ TdxTokenEditHitTest }

procedure TdxTokenEditHitTest.Reset;
begin
  Point := cxNullPoint;
  HitObjectPartID := 0;
  HitObject := nil;
end;

{ TdxTokenEditCustomTokenViewInfo }

constructor TdxTokenEditCustomTokenViewInfo.Create(AOwner: TdxTokenEditViewInfo);
begin
  inherited Create;
  FOwner := AOwner;
  FState := cxbsNormal;
end;

procedure TdxTokenEditCustomTokenViewInfo.Calculate(const ABounds: TRect);
begin
  FBounds := ABounds;
  FCaptionBounds := cxRectContent(ABounds, GetContentOffsets);
end;

procedure TdxTokenEditCustomTokenViewInfo.Calculate(const ALeft, ATop, AWidth, AHeight: Integer);
begin
  Calculate(cxRectBounds(ALeft, ATop, AWidth, AHeight));
end;

function TdxTokenEditCustomTokenViewInfo.CalculateHitTest(AHitTest: TdxTokenEditHitTest): Boolean;
begin
  Result := PtInRect(Bounds, AHitTest.Point);
  if Result then
  begin
    AHitTest.HitObject := Self;
    AHitTest.HitObjectPartID := tehtObject;
  end;
end;

procedure TdxTokenEditCustomTokenViewInfo.Draw(ACanvas: TcxCanvas);
begin
  if ACanvas.RectVisible(Bounds) then
  begin
    ACanvas.SaveClipRegion;
    try
      ACanvas.IntersectClipRect(Bounds);
      if not DoCustomDraw(ACanvas) then
      begin
        DrawBackground(ACanvas);
        ACanvas.IntersectClipRect(cxRectContent(Bounds, GetContentOffsets));
        DrawContent(ACanvas);
      end;
    finally
      ACanvas.RestoreClipRegion;
    end;
  end;
end;

function TdxTokenEditCustomTokenViewInfo.GetCaptionAlignment: TAlignment;
begin
  if not Owner.UseRightToLeftAlignment then
    Result := taLeftJustify
  else
    Result := taRightJustify;
end;

function TdxTokenEditCustomTokenViewInfo.GetContentOffsets: TRect;
begin
  Result := Painter.GetScaledTokenContentOffsets(ScaleFactor);
  if Owner.UseRightToLeftAlignment then
    Result := TdxRightToLeftLayoutConverter.ConvertOffsets(Result);
end;

function TdxTokenEditCustomTokenViewInfo.Equals(Obj: TObject): Boolean;
begin
  Result := (Obj.ClassType = ClassType) and
    cxRectIsEqual(TdxTokenEditCustomTokenViewInfo(Obj).Bounds, Bounds) and
    cxRectIsEqual(TdxTokenEditCustomTokenViewInfo(Obj).CaptionBounds, CaptionBounds) and
    (State = TdxTokenEditCustomTokenViewInfo(Obj).State);
end;

function TdxTokenEditCustomTokenViewInfo.GetHint: string;
begin
  Result := '';
end;

function TdxTokenEditCustomTokenViewInfo.MeasureSize: TSize;
var
  AMargins: TRect;
begin
  AMargins := GetContentOffsets;
  Result := cxTextSize(Owner.Font, Caption);
  Inc(Result.cx, cxMarginsWidth(AMargins));
  Inc(Result.cy, cxMarginsHeight(AMargins));
end;

procedure TdxTokenEditCustomTokenViewInfo.UpdateState(AController: TdxTokenEditController);
begin
  // do nothing
end;

function TdxTokenEditCustomTokenViewInfo.DoCustomDraw(ACanvas: TcxCanvas): Boolean;
begin
  Result := dxCallCustomDrawItemEvent(Owner.Properties.OnCustomDrawToken, Owner, ACanvas, Self);
  if Result and (Owner.Edit <> nil) and (Owner.Edit.RepositoryItem <> nil) then
    Result := dxCallCustomDrawItemEvent(
      TdxTokenEditProperties(Owner.Edit.ActiveProperties).OnCustomDrawToken, Owner, ACanvas, Self);
end;

procedure TdxTokenEditCustomTokenViewInfo.DrawBackground(ACanvas: TcxCanvas);
begin
  Painter.DrawScaledTokenBackground(ACanvas, Bounds, State, ScaleFactor);
end;

procedure TdxTokenEditCustomTokenViewInfo.DrawContent(ACanvas: TcxCanvas);
const
  CAlignmentTextFlagMap: array [TAlignment] of Cardinal = (DT_LEFT, DT_RIGHT, DT_CENTER);
var
  ATextFlags: Cardinal;
begin
  if not IsRectEmpty(CaptionBounds) then
  begin
    ACanvas.Font := Owner.Font;
    ATextFlags := CAlignmentTextFlagMap[CaptionAlignment] or DT_VCENTER or DT_SINGLELINE or DT_END_ELLIPSIS;
    if Owner.UseRightToLeftReading then
      ATextFlags := ATextFlags or DT_RTLREADING;
    cxDrawText(ACanvas, Caption, CaptionBounds, ATextFlags, GetTextColor(State));
  end;
end;

function TdxTokenEditCustomTokenViewInfo.GetTextColor(const AState: TcxButtonState): TColor;
begin
  Result := Painter.GetTokenTextColor(AState);
  if (Result = clDefault) or
    ((Owner.Edit <> nil) and (TdxCustomTokenEdit(Owner.Edit).IsStyleAssigned(csvTextColor))) then
    Result := Owner.Font.Color;
end;

function TdxTokenEditCustomTokenViewInfo.IsVisible: Boolean;
begin
  Result := not cxRectIsEmpty(Bounds)
end;

function TdxTokenEditCustomTokenViewInfo.GetPainter: TcxCustomLookAndFeelPainter;
begin
  Result := Owner.Painter;
end;

function TdxTokenEditCustomTokenViewInfo.GetScaleFactor: TdxScaleFactor;
begin
  Result := Owner.ScaleFactor;
end;

{ TdxTokenEditTokenViewInfo }

constructor TdxTokenEditTokenViewInfo.Create(AOwner: TdxTokenEditViewInfo);
begin
  inherited Create(AOwner);
  FCloseGlyphState := cxbsNormal;
end;

destructor TdxTokenEditTokenViewInfo.Destroy;
begin
  dxFader.Remove(Self);
  inherited Destroy;
end;

procedure TdxTokenEditTokenViewInfo.Calculate(const ABounds: TRect);
var
  ASize: TSize;
begin
  inherited Calculate(ABounds);

  if IsCloseGlyphVisible then
    CalculateGlyphBounds(FCloseGlyphBounds, Painter.GetScaledTokenCloseGlyphSize(ScaleFactor), CloseGlyphPosition)
  else
    FCloseGlyphBounds := cxNullRect;

  if IsGlyphVisible then
  begin
    ASize := cxRectSize(CaptionBounds);
    ASize := cxSizeProportionalStretch(cxSize(ASize.cy, ASize.cy), GlyphSize);
    CalculateGlyphBounds(FGlyphBounds, ASize, GlyphPosition);
    FGlyphBounds := cxRectCenter(FGlyphBounds, cxSizeProportionalStretch(cxSize(FGlyphBounds), ASize));
  end
  else
    FGlyphBounds := cxNullRect;
end;

function TdxTokenEditTokenViewInfo.CalculateHitTest(AHitTest: TdxTokenEditHitTest): Boolean;
begin
  Result := inherited CalculateHitTest(AHitTest);
  if Result then
  begin
    if PtInRect(CloseGlyphBounds, AHitTest.Point) then
      AHitTest.HitObjectPartID := tehtTokenCloseGlyph
    else
      if PtInRect(GlyphBounds, AHitTest.Point) then
        AHitTest.HitObjectPartID := tehtTokenGlyph
      else
        AHitTest.HitObjectPartID := tehtToken;
  end;
end;

function TdxTokenEditTokenViewInfo.Clone: TdxTokenEditTokenViewInfo;
begin
  Result := TdxTokenEditTokenViewInfo.Create(Owner);
  Result.Initialize(Text, TokenIndex, Item);
end;

function TdxTokenEditTokenViewInfo.MeasureSize: TSize;
var
  ASize: TSize;
begin
  Result := inherited MeasureSize;
  if IsCloseGlyphVisible then
  begin
    ASize := Painter.GetScaledTokenCloseGlyphSize(ScaleFactor);
    Result.cx := Result.cx + ASize.cx + ScaleFactor.Apply(GlyphOffset);
    Result.cy := Max(Result.cy, ASize.cy + cxMarginsHeight(GetContentOffsets));
  end;
  if IsGlyphVisible then
  begin
    ASize.cy := Result.cy - cxMarginsHeight(GetContentOffsets);
    ASize := cxSizeProportionalStretch(cxSize(ASize.cy, ASize.cy), GlyphSize);
    Result.cx := Result.cx + ASize.cx + ScaleFactor.Apply(GlyphOffset);
  end;
end;

function TdxTokenEditTokenViewInfo.Equals(Obj: TObject): Boolean;
begin
  Result := inherited Equals(Obj) and
    cxRectIsEqual(CloseGlyphBounds, TdxTokenEditTokenViewInfo(Obj).CloseGlyphBounds) and
    cxRectIsEqual(GlyphBounds, TdxTokenEditTokenViewInfo(Obj).GlyphBounds) and
    (CloseGlyphState = TdxTokenEditTokenViewInfo(Obj).CloseGlyphState) and
    (TokenIndex = TdxTokenEditTokenViewInfo(Obj).TokenIndex);
end;

procedure TdxTokenEditTokenViewInfo.Initialize(const AText: string; ATokenIndex: Integer; AItem: TdxTokenEditToken);
begin
  FTokenIndex := ATokenIndex;
  FItem := AItem;
  FText := AText;
end;

function TdxTokenEditTokenViewInfo.GetHint: string;
begin
  if Item <> nil then
    Result := Item.Hint
  else
    Result := '';
end;

procedure TdxTokenEditTokenViewInfo.UpdateState(AController: TdxTokenEditController);
var
  ACloseGlyphState: TcxButtonState;
  APrevStateSnapshot: TcxBitmap32;
  AState: TcxButtonState;
begin
  CalculateStates(AController, AState, ACloseGlyphState);
  if (AState <> State) or (ACloseGlyphState <> FCloseGlyphState) then
  begin
    if CanFade then
    begin
      APrevStateSnapshot := CreateFadeImage;
      FCloseGlyphState := ACloseGlyphState;
      FState := AState;
      dxFader.Fade(Self, APrevStateSnapshot, CreateFadeImage, dxTokenEditDefaultFadeFrameCount, dxFadeOutDefaultAnimationFrameDelay);
    end
    else
    begin
      FState := AState;
      FCloseGlyphState := ACloseGlyphState;
    end;
    DrawFadeImage;
  end;
end;

procedure TdxTokenEditTokenViewInfo.CalculateGlyphBounds(
  var ABounds: TRect; const ASize: TSize; APosition: TdxTokenEditElementPosition);
begin
  ABounds := cxRectCenterVertically(CaptionBounds, ASize.cy);
  if Owner.UseRightToLeftAlignment and (APosition <> teepNone) then
  begin
    if APosition = teepLeft then
      APosition := teepRight
    else
      APosition := teepLeft;
  end;
  case APosition of
    teepLeft:
      begin
        ABounds := cxRectSetWidth(ABounds, ASize.cx);
        FCaptionBounds.Left := ABounds.Right + ScaleFactor.Apply(GlyphOffset);
      end;
    teepRight:
      begin
        ABounds := cxRectSetRight(ABounds, ABounds.Right, ASize.cx);
        FCaptionBounds.Right := ABounds.Left - ScaleFactor.Apply(GlyphOffset);
      end
  else //teepNone
    ABounds := cxRectSetWidth(ABounds, 0);
  end;
end;

procedure TdxTokenEditTokenViewInfo.CalculateStates(
  AController: TdxTokenEditController; out AState, ACloseGlyphState: TcxButtonState);
begin
  if not Owner.Enabled then
    AState := cxbsDisabled
  else if (AController.FocusedObject = Self) and (AController.PressedObject = Self) then
    AState := cxbsPressed
  else if (AController.FocusedObject = Self) and (AController.PressedObject = nil) then
    AState := cxbsHot
  else
    AState := cxbsNormal;

  ACloseGlyphState := AState;
  if AState = cxbsPressed then
  begin
    if AController.HitTest.HitObjectPartID <> tehtToken then
      AState := cxbsHot;
    if AController.HitTest.HitObjectPartID <> tehtTokenCloseGlyph then
      ACloseGlyphState := cxbsHot;
  end;
end;

procedure TdxTokenEditTokenViewInfo.DrawBackground(ACanvas: TcxCanvas);
begin
  if not dxFader.DrawFadeImage(Self, ACanvas.Handle, Bounds) then
    inherited DrawBackground(ACanvas);
end;

procedure TdxTokenEditTokenViewInfo.DrawContent(ACanvas: TcxCanvas);
begin
  inherited DrawContent(ACanvas);
  if not cxRectIsEmpty(CloseGlyphBounds) then
    Painter.DrawScaledTokenCloseGlyph(ACanvas, CloseGlyphBounds, CloseGlyphState, ScaleFactor);
  if not cxRectIsEmpty(GlyphBounds) then
    DrawGlyph(ACanvas, GlyphBounds);
end;

procedure TdxTokenEditTokenViewInfo.DrawGlyph(ACanvas: TcxCanvas; const R: TRect);
const
  DrawMode: array[Boolean] of TcxImageDrawMode = (idmNormal, idmDisabled);
begin
  cxDrawImage(ACanvas, R, Item.Glyph, Images, Item.ImageIndex, ifmFit,
    DrawMode[State = cxbsDisabled], False, Painter.GetTokenColorPalette(State), ScaleFactor);
end;

function TdxTokenEditTokenViewInfo.GetCaption: string;
begin
  if (Item <> nil) and (Item.DisplayText <> '') then
    Result := Item.DisplayText
  else
    Result := FText;
end;

function TdxTokenEditTokenViewInfo.GetGlyphSize: TSize;
begin
  if Item <> nil then
    Result := dxGetImageSize(Item.Glyph, Images, Item.ImageIndex, Owner.ScaleFactor)
  else
    Result := cxNullSize;
end;

function TdxTokenEditTokenViewInfo.IsCloseGlyphVisible: Boolean;
begin
  Result := (CloseGlyphPosition <> teepNone) and not Owner.Properties.ReadOnly;
end;

function TdxTokenEditTokenViewInfo.IsGlyphVisible: Boolean;
begin
  Result := (GlyphPosition <> teepNone) and not cxSizeIsEmpty(GlyphSize);
end;

function TdxTokenEditTokenViewInfo.CanFade: Boolean;
begin
  Result := Supports(Owner.Owner, IcxEditOwner) and not Assigned(Owner.Properties.OnCustomDrawToken);
end;

function TdxTokenEditTokenViewInfo.CreateFadeImage: TcxBitmap32;
begin
  Result := TcxBitmap32.CreateSize(Bounds, True);
  Result.cxCanvas.WindowOrg := Bounds.TopLeft;
  DrawBackground(Result.cxCanvas);
  Result.cxCanvas.WindowOrg := cxNullPoint;
end;

procedure TdxTokenEditTokenViewInfo.GetFadingImages(out AFadeOutImage, AFadeInImage: TcxBitmap);
begin
  AFadeInImage := nil;
  AFadeOutImage := nil;
end;

procedure TdxTokenEditTokenViewInfo.DrawFadeImage;
var
  AIntf: IcxEditOwner;
begin
  if Supports(Owner.Owner, IcxEditOwner, AIntf) then
    AIntf.Invalidate(cxRectOffset(Bounds, Owner.ContentOffset), True);
end;

function TdxTokenEditTokenViewInfo.GetCloseGlyphPosition: TdxTokenEditElementPosition;
begin
  Result := Owner.Properties.CloseGlyphPosition;
end;

function TdxTokenEditTokenViewInfo.GetGlyphPosition: TdxTokenEditElementPosition;
begin
  Result := Owner.Properties.GlyphPosition;
end;

function TdxTokenEditTokenViewInfo.GetImages: TCustomImageList;
begin
  Result := Owner.Properties.Images;
end;

{ TdxTokenEditTokenViewInfoList }

procedure TdxTokenEditTokenViewInfoList.Assign(ASource: TdxTokenEditTokenViewInfoList);
var
  I: Integer;
begin
  Clear;
  Capacity := ASource.Count;
  for I := 0 to ASource.Count - 1 do
    Add(ASource[I].Clone)
end;

procedure TdxTokenEditTokenViewInfoList.CalculateHitTest(AHitTest: TdxTokenEditHitTest);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    if Items[I].CalculateHitTest(AHitTest) then
      Break;
  end;
end;

function TdxTokenEditTokenViewInfoList.Equals(Obj: TObject): Boolean;
var
  I: Integer;
begin
  Result := (Obj.ClassType = ClassType) and (Count = TdxTokenEditTokenViewInfoList(Obj).Count);
  if Result then
  begin
    for I := 0 to Count - 1 do
      Result := Result and Items[I].Equals(TdxTokenEditTokenViewInfoList(Obj).Items[I]);
  end;
end;

function TdxTokenEditTokenViewInfoList.IndexOf(AToken: TdxTokenEditToken): Integer;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    if Items[I].Item = AToken then
      Exit(I);
  end;
  Result := -1;
end;

function TdxTokenEditTokenViewInfoList.First: TdxTokenEditTokenViewInfo;
begin
  Result := TdxTokenEditTokenViewInfo(inherited First);
end;

function TdxTokenEditTokenViewInfoList.Last: TdxTokenEditTokenViewInfo;
begin
  Result := TdxTokenEditTokenViewInfo(inherited Last);
end;

function TdxTokenEditTokenViewInfoList.LastVisible: TdxTokenEditTokenViewInfo;
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
  begin
    if Items[I].IsVisible then
      Exit(Items[I]);
  end;
  Result := nil;
end;

function TdxTokenEditTokenViewInfoList.GetItem(Index: Integer): TdxTokenEditTokenViewInfo;
begin
  Result := TdxTokenEditTokenViewInfo(inherited Items[Index]);
end;

procedure TdxTokenEditTokenViewInfoList.SetItem(Index: Integer; AValue: TdxTokenEditTokenViewInfo);
begin
  inherited Items[Index] := AValue;
end;

{ TdxTokenEditMoreTokensViewInfo }

function TdxTokenEditMoreTokensViewInfo.GetCaptionAlignment: TAlignment;
begin
  Result := taCenter;
end;

function TdxTokenEditMoreTokensViewInfo.CalculateHitTest(AHitTest: TdxTokenEditHitTest): Boolean;
begin
  Result := inherited CalculateHitTest(AHitTest);
  if Result then
    AHitTest.HitObjectPartID := tehtMoreTokensItem;
end;

function TdxTokenEditMoreTokensViewInfo.GetHint: string;
var
  AHiddenTokenCount: Integer;
  AHiddenTokens: TStringBuilder;
  AToken: TdxTokenEditTokenViewInfo;
  I: Integer;
begin
  AHiddenTokens := TdxStringBuilderManager.Get;
  try
    AHiddenTokenCount := 0;
    for I := 0 to Owner.Tokens.Count - 1 do
    begin
      AToken := Owner.Tokens[I];
      if not AToken.IsVisible then
      begin
        if AHiddenTokenCount > 0 then
        begin
          AHiddenTokens.Append(Owner.Properties.InputDelimiters[1]);
          AHiddenTokens.Append(' ');
        end;
        AHiddenTokens.Append(AToken.Caption);
        Inc(AHiddenTokenCount);
      end;
    end;
    Result := Format(cxGetResourceString(@sdxTokenEditMoreTokensHint), [AHiddenTokenCount, AHiddenTokens.ToString]);
  finally
    TdxStringBuilderManager.Release(AHiddenTokens);
  end;
end;

function TdxTokenEditMoreTokensViewInfo.GetCaption: string;
begin
  Result := cxGetResourceString(@sdxTokenEditMoreTokensCaption);
end;

{ TdxTokenEditViewInfo }

constructor TdxTokenEditViewInfo.Create;
begin
  inherited Create;
  FEditValue := EmptyStr;
  FTokens := TdxTokenEditTokenViewInfoList.Create;
  FMoreTokensItem := TdxTokenEditMoreTokensViewInfo.Create(Self);
  IndentBetweenItems := Padding;
  EditingTokenIndex := -1;
end;

destructor TdxTokenEditViewInfo.Destroy;
begin
  FreeAndNil(FMoreTokensItem);
  FreeAndNil(FTokens);
  inherited Destroy;
end;

procedure TdxTokenEditViewInfo.Assign(Source: TObject);
begin
  inherited Assign(Source);

  if Source is TdxTokenEditViewInfo then
  begin
    Bounds := TdxTokenEditViewInfo(Source).Bounds;
    IndentBetweenItems := TdxTokenEditViewInfo(Source).IndentBetweenItems;
    UpdateEditValue(TdxTokenEditViewInfo(Source).EditValue);
    Tokens.Assign(TdxTokenEditViewInfo(Source).Tokens);
    ContentOffset := TdxTokenEditViewInfo(Source).ContentOffset;
  end;
end;

procedure TdxTokenEditViewInfo.Calculate;
begin
  RefreshTokensViewInfo;
  CalculateLayout;
  ContentOffset := ContentOffset;
end;

function TdxTokenEditViewInfo.CalculateClientAutoSize: TSize;
begin
  Result := CalculateClientSize(cxSizeMax(ContentSize, GetDefaultTokenItemSize));
end;

function TdxTokenEditViewInfo.CalculateClientMinSize: TSize;
begin
  Result := CalculateClientSize(GetDefaultTokenItemSize);
end;

procedure TdxTokenEditViewInfo.CalculateHitTest(const AHitTest: TdxTokenEditHitTest);
begin
  if PtInRect(Bounds, AHitTest.Point) then
  begin
    AHitTest.Point := cxPointOffset(AHitTest.Point, ContentOffset, False);
    try
      if PtInRect(InnerEditRect, AHitTest.Point) then
      begin
        AHitTest.HitObject := Self;
        AHitTest.HitObjectPartID := tehtInnerEdit;
      end
      else
        if not MoreTokensItem.CalculateHitTest(AHitTest) then
          Tokens.CalculateHitTest(AHitTest);
    finally
      AHitTest.Point := cxPointOffset(AHitTest.Point, ContentOffset);
    end;
  end;
end;

function TdxTokenEditViewInfo.Equals(Obj: TObject): Boolean;
begin
  Result := (Obj.ClassType = ClassType) and
    cxRectIsEqual(ClientRect, TdxTokenEditViewInfo(Obj).ClientRect) and
    cxPointIsEqual(ContentOffset, TdxTokenEditViewInfo(Obj).ContentOffset) and
    MoreTokensItem.Equals(TdxTokenEditViewInfo(Obj).MoreTokensItem) and
    Tokens.Equals(TdxTokenEditViewInfo(Obj).Tokens);
end;

function TdxTokenEditViewInfo.IsHotTrack: Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Tokens.Count - 1 do
    Result := Result or (Tokens[I].State = cxbsHot);
end;

function TdxTokenEditViewInfo.IsHotTrack(P: TPoint): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Tokens.Count - 1 do
    Result := Result or cxRectPtIn(Tokens[I].Bounds, P);
end;

function TdxTokenEditViewInfo.GetHintText(APart: Integer): string;
var
  AItem: TdxTokenEditCustomTokenViewInfo;
begin
  if GetItemByPart(APart, AItem) then
    Result := AItem.GetHint
  else
    Result := inherited GetHintText(APart);
end;

function TdxTokenEditViewInfo.GetItemByPart(APart: Integer; out AItem: TdxTokenEditCustomTokenViewInfo): Boolean;
var
  ATokenIndex: Integer;
begin
  Result := (APart > 0) and (APart and ecpTokenSignature <> 0);
  if Result then
  begin
    ATokenIndex := SmallInt(LoWord(APart));
    if ATokenIndex < 0 then
      AItem := MoreTokensItem
    else
      if InRange(ATokenIndex, 0, Tokens.Count - 1) then
        AItem := Tokens[ATokenIndex]
      else
        Result := False;
  end;
end;

function TdxTokenEditViewInfo.GetPart(const P: TPoint): Integer;
var
  AHitTest: TdxTokenEditHitTest;
begin
  Result := inherited GetPart(P);
  if Result = ecpControl then
  begin
    AHitTest := TdxTokenEditHitTest.Create;
    try
      AHitTest.Point := P;
      CalculateHitTest(AHitTest);
      case AHitTest.HitObjectPartID of
        tehtToken, tehtTokenCloseGlyph, tehtTokenGlyph:
          Result := ecpTokenSignature or Word(TdxTokenEditTokenViewInfo(AHitTest.HitObject).TokenIndex);
        tehtMoreTokensItem:
          Result := ecpTokenSignature or Word(-1);
      end;
    finally
      AHitTest.Free;
    end;
  end;
end;

function TdxTokenEditViewInfo.GetPartRect(APart: Integer): TRect;
var
  AItem: TdxTokenEditCustomTokenViewInfo;
begin
  if GetItemByPart(APart, AItem) then
    Result := AItem.Bounds
  else
    Result := inherited GetPartRect(APart);
end;

function TdxTokenEditViewInfo.NeedShowHint(ACanvas: TcxCanvas; const P: TPoint; const AVisibleBounds: TRect;
  out AText: TCaption; out AIsMultiLine: Boolean; out ATextRect: TRect; AMaxLineCount: Integer = 0): Boolean;
var
  AHitTest: TdxTokenEditHitTest;
  AItemViewInfo: TdxTokenEditCustomTokenViewInfo;
begin
  Result := False;
  AHitTest := TdxTokenEditHitTest.Create;
  try
    AHitTest.Point := Point(P.X - Left, P.Y - Top);
    CalculateHitTest(AHitTest);
    if AHitTest.HitObject is TdxTokenEditCustomTokenViewInfo then
    begin
      AItemViewInfo := TdxTokenEditCustomTokenViewInfo(AHitTest.HitObject);
      if AItemViewInfo.MeasureSize.cx > cxRectWidth(AItemViewInfo.Bounds) then
        AText := AItemViewInfo.Caption
      else
        AText := AItemViewInfo.GetHint;

      if AText <> '' then
      begin
        ATextRect.TopLeft := cxPointOffset(P, 0, cxGetCursorSize.cy + cxTextOffset);
        ATextRect := cxRectSetSize(ATextRect, cxTextSize(Font, AText));
        AIsMultiLine := True;
        Result := True;
      end;
    end;
  finally
    AHitTest.Free;
  end;
end;

function TdxTokenEditViewInfo.Repaint(AControl: TWinControl;
  const AInnerEditRect: TRect; AViewInfo: TcxContainerViewInfo = nil): Boolean;
begin
  Result := inherited Repaint(AControl, AInnerEditRect, AViewInfo) or (AViewInfo <> nil) and not Equals(AViewInfo);
end;

function TdxTokenEditViewInfo.UpdateEditValue(const Value: Variant): Boolean;
begin
  Result := FEditValue <> Value;
  if Result then
    FEditValue := Value;
end;

function TdxTokenEditViewInfo.CalculateInnerEditSize: TSize;
begin
  if HasInnerEdit then
  begin
    Result.cx := ScaleFactor.Apply(dxTokenEditDefaultInnerEditMinWidth);
    Result.cy := cxTextHeight(Font);
  end
  else
    Result := cxNullSize;
end;

procedure TdxTokenEditViewInfo.CalculateLayout;
var
  AContentRect: TRect;
  AMaxRowCount: Integer;
  ARowHeight: Integer;
begin
  InnerEditSize := CalculateInnerEditSize;
  IndentBetweenItems := ScaleFactor.Apply(Padding);
  ARowHeight := CalculateRowHeight;

  AContentRect := ContentDisplayRect;
  if CanGrowByHeight(ARowHeight) then
    AContentRect.Bottom := cxMaxRectSize;
  if MaxLineCount > 0 then
    AMaxRowCount := MaxLineCount
  else
    AMaxRowCount := cxRectHeight(AContentRect) div ARowHeight;

  CalculateLayoutCore(AContentRect, ARowHeight, AMaxRowCount, ARowHeight);
end;

procedure TdxTokenEditViewInfo.CalculateLayoutCore(const AContentRect: TRect; ARowHeight, AMaxRowCount, AMinItemSize: Integer);

  function CalculateItemBounds(var AOffset: TPoint; AItemWidth: Integer): TRect;
  begin
    if (AOffset.X <> AContentRect.Left) and (AOffset.X + AItemWidth > AContentRect.Right) and (AMaxRowCount > 1) then
    begin
      AOffset.X := AContentRect.Left;
      AOffset.Y := AOffset.Y + ARowHeight + IndentBetweenItems;
      Dec(AMaxRowCount);
    end;

    Result := cxRectBounds(AOffset.X, AOffset.Y, AItemWidth, ARowHeight);
    Result.Right := Max(Result.Left, Min(Result.Right, AContentRect.Right));
    if IsTooSmallForDisplay(Result, AContentRect, AItemWidth, AMinItemSize) then
      Result := cxRectSetWidth(Result, 0);

    if Result.Right > Result.Left then
      AOffset.X := Result.Right + IndentBetweenItems;

    if UseRightToLeftAlignment then
      Result := TdxRightToLeftLayoutConverter.ConvertRect(Result, AContentRect);
  end;

var
  AIndex: Integer;
  AItem: TdxTokenEditTokenViewInfo;
  AOffset: TPoint;
begin
  InnerEditRect := cxNullRect;

  AOffset := AContentRect.TopLeft;
  for AIndex := 0 to Tokens.Count - 1 do
  begin
    AItem := Tokens[AIndex];
    if AItem.TokenIndex = EditingTokenIndex then
    begin
      InnerEditRect := CalculateItemBounds(AOffset, Max(AItem.MeasureSize.cx, InnerEditSize.cx));
      InnerEditRect := cxRectCenterVertically(InnerEditRect, InnerEditSize.cy);
      AItem.Calculate(InnerEditRect);
    end
    else
      AItem.Calculate(CalculateItemBounds(AOffset, AItem.MeasureSize.cx));
  end;

  CalculateMoreTokensItem(AContentRect, ARowHeight, AMinItemSize);

  if EditingTokenIndex < 0 then
  begin
    if MoreTokensItem.IsVisible then
      InnerEditRect := cxNullRect
    else
    begin
      InnerEditRect := cxRectCenterVertically(CalculateItemBounds(AOffset, InnerEditSize.cx), InnerEditSize.cy);
      if not UseRightToLeftAlignment then
        InnerEditRect.Right := AContentRect.Right
      else
        InnerEditRect.Left := AContentRect.Left;
    end;
  end;
end;

procedure TdxTokenEditViewInfo.CalculateMoreTokensItem(const AContentRect: TRect; ARowHeight, AMinItemSize: Integer);

  function GetFreeSpaceForMoreTokensItem(AToken: TdxTokenEditTokenViewInfo; ARequiredSpace: Integer): Boolean;

    function GetNeededSpace: Integer;
    begin
      Result := ARequiredSpace + IndentBetweenItems;
      if not UseRightToLeftAlignment then
        Result := Result - (AContentRect.Right - AToken.Bounds.Right)
      else
        Result := Result - (AToken.Bounds.Left - AContentRect.Left);
    end;

    procedure SetTokenWidth(var ATokenBounds: TRect; const AWidth: Integer);
    begin
      if not UseRightToLeftAlignment then
        ATokenBounds := cxRectSetWidth(ATokenBounds, AWidth)
      else
        ATokenBounds := cxRectSetRight(ATokenBounds, ATokenBounds.Right, AWidth);
    end;

  var
    ATokenBounds: TRect;
  begin
    Result := GetNeededSpace <= 0;
    if not Result then
    begin
      ATokenBounds := AToken.Bounds;
      SetTokenWidth(ATokenBounds, Max(0, cxRectWidth(ATokenBounds) - GetNeededSpace));
      if IsTooSmallForDisplay(ATokenBounds, AContentRect, AToken.MeasureSize.cx, AMinItemSize) then
        SetTokenWidth(ATokenBounds, 0);
      AToken.Calculate(ATokenBounds);
      Result := GetNeededSpace <= 0;
    end;
  end;

var
  AItemBounds: TRect;
  AItemSize: TSize;
  ALeft: Integer;
  AToken: TdxTokenEditTokenViewInfo;
begin
  if (Tokens.Count > 0) and (Tokens.LastVisible <> Tokens.Last) then
  begin
    AItemSize := MoreTokensItem.MeasureSize;
    repeat
      AToken := Tokens.LastVisible;
    until (AToken = nil) or GetFreeSpaceForMoreTokensItem(AToken, AItemSize.cx);

    if AToken <> nil then
    begin
      if not UseRightToLeftAlignment then
        ALeft := AToken.Bounds.Right + IfThen(AToken.IsVisible, IndentBetweenItems)
      else
        ALeft := AToken.Bounds.Left - IfThen(AToken.IsVisible, IndentBetweenItems);
      AItemBounds := cxRectBounds(ALeft, AToken.Bounds.Top, 0, 0);
    end
    else
      AItemBounds := AContentRect;

    if not UseRightToLeftAlignment then
      AItemBounds := cxRectSetSize(AItemBounds, AItemSize.cx, ARowHeight)
    else
      AItemBounds := cxRectSetHeight(cxRectSetRight(AItemBounds, AItemBounds.Right, AItemSize.cx), ARowHeight);
    MoreTokensItem.Calculate(AItemBounds);
  end
  else
    MoreTokensItem.Calculate(cxNullRect);
end;

function TdxTokenEditViewInfo.CalculateRowHeight: Integer;
begin
  Result := Max(InnerEditSize.cy, GetDefaultTokenItemSize.cy);
end;

function TdxTokenEditViewInfo.CanGrowByHeight(AMinItemWidth: Integer): Boolean;
begin
  Result := (Edit <> nil) and (TdxCustomTokenEdit(Edit).IsAutoHeight and (MaxLineCount = 0) or
    (AMinItemWidth <= cxRectWidth(Bounds) - cxMarginsHeight(BorderExtent) -
      2 * ScaleFactor.Apply(Padding) - dxGetSystemMetrics(SM_CXVSCROLL, ScaleFactor)));
end;

function TdxTokenEditViewInfo.IsTooSmallForDisplay(const R, AArea: TRect; AMeasuredSize, AMinSize: Integer): Boolean;
begin
  Result := (R.Bottom > AArea.Bottom) or (cxRectWidth(R) < AMinSize) and (cxRectWidth(R) < AMeasuredSize);
end;

procedure TdxTokenEditViewInfo.DrawContent(ACanvas: TcxCanvas);
var
  I: Integer;
begin
  ACanvas.SaveState;
  try
    ACanvas.IntersectClipRect(ContentDisplayRect);
    MoveWindowOrg(ACanvas.Handle, ContentOffset.X, ContentOffset.Y);
    for I := 0 to Tokens.Count - 1 do
      Tokens[I].Draw(ACanvas);
    MoreTokensItem.Draw(ACanvas);
  finally
    ACanvas.RestoreState;
  end;
end;

function TdxTokenEditViewInfo.GetDefaultTokenItemSize: TSize;
var
  AItem: TdxTokenEditTokenViewInfo;
begin
  AItem := GetTokenItemViewInfoClass.Create(Self);
  try
    AItem.Initialize(dxMeasurePattern, -1, nil);
    Result := AItem.MeasureSize;
  finally
    AItem.Free;
  end;
end;

function TdxTokenEditViewInfo.GetTokenItemViewInfoClass: TdxTokenEditTokenViewInfoClass;
begin
  Result := TdxTokenEditTokenViewInfo;
end;

procedure TdxTokenEditViewInfo.InternalPaint(ACanvas: TcxCanvas);
var
  ABufferBitmap: TcxBitmap32;
begin
  ABufferBitmap := TcxBitmap32.CreateSize(Bounds);
  try
    cxBitBlt(ABufferBitmap.Canvas.Handle, ACanvas.Handle, ABufferBitmap.ClientRect, Bounds.TopLeft, SRCCOPY);
    ABufferBitmap.cxCanvas.WindowOrg := Bounds.TopLeft;
    DrawCustomEdit(ABufferBitmap.cxCanvas, not Transparent, True);
    DrawContent(ABufferBitmap.cxCanvas);
    ABufferBitmap.cxCanvas.WindowOrg := cxNullPoint;
    cxBitBlt(ACanvas.Handle, ABufferBitmap.Canvas.Handle, Bounds, cxNullPoint, SRCCOPY);
  finally
    ABufferBitmap.Free;
  end;
end;

procedure TdxTokenEditViewInfo.RefreshTokensViewInfo;
var
  ATokens: TStrings;
  I: Integer;
begin
  ATokens := Properties.EditValueToTokens(EditValue);
  try
    Tokens.Count := ATokens.Count;
    for I := 0 to Tokens.Count - 1 do
    begin
      if Tokens[I] = nil then
        Tokens[I] := GetTokenItemViewInfoClass.Create(Self);
    end;
    for I := 0 to Tokens.Count - 1 do
      Tokens[I].Initialize(ATokens[I], I, TdxTokenEditToken(ATokens.Objects[I]));
  finally
    ATokens.Free;
  end;
end;

procedure TdxTokenEditViewInfo.UpdateState(AController: TdxTokenEditController);
var
  I: Integer;
begin
  for I := 0 to Tokens.Count - 1 do
    Tokens[I].UpdateState(AController);
end;

function TdxTokenEditViewInfo.CalculateClientSize(const AContentSize: TSize): TSize;
var
  AMargins: TRect;
begin
  AMargins := cxMargins(ClientRect, ContentDisplayRect);
  Result := AContentSize;
  Inc(Result.cx, cxMarginsWidth(AMargins));
  Inc(Result.cy, cxMarginsHeight(AMargins));
end;

function TdxTokenEditViewInfo.GetContentDisplayRect: TRect;
begin
  Result := cxRectInflate(ClientRect, -ScaleFactor.Apply(Padding));
end;

function TdxTokenEditViewInfo.GetContentSize: TSize;
var
  AContentRect: TRect;
begin
  if Tokens.Count > 0 then
    AContentRect := cxRectUnion(Tokens.First.Bounds, Tokens.Last.Bounds)
  else
    AContentRect := cxRectSetSize(ContentDisplayRect, 0, 0);

  if not cxRectIsEmpty(InnerEditRect) then
    AContentRect := cxRectUnion(AContentRect, InnerEditRect);
  if not cxRectIsEmpty(MoreTokensItem.Bounds) then
    AContentRect := cxRectUnion(AContentRect, MoreTokensItem.Bounds);

  Result := cxSize(AContentRect);
end;

function TdxTokenEditViewInfo.GetProperties: TdxTokenEditProperties;
begin
  Result := TdxTokenEditProperties(EditProperties);
end;

procedure TdxTokenEditViewInfo.SetContentOffset(const Value: TPoint);
begin
  FContentOffset.X := 0;
  FContentOffset.Y := Min(0, Max(Value.Y, cxRectHeight(ContentDisplayRect) - ContentSize.cy));
end;

{ TdxTokenEditController }

constructor TdxTokenEditController.Create(AViewInfo: TdxTokenEditViewInfo);
begin
  inherited Create;
  FViewInfo := AViewInfo;
  FHitTest := TdxTokenEditHitTest.Create;
end;

destructor TdxTokenEditController.Destroy;
begin
  FreeAndNil(FHitTest);
  inherited Destroy;
end;

procedure TdxTokenEditController.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Edit <> nil) and not Edit.InnerControlMouseDown then
    StopEditing(False);

  UpdateHitTest(Point(X, Y));
  if Button = mbLeft then
  begin
    PressedObject := HitTest.HitObject;
    if ssDouble in Shift then
    begin
      ProcessDblClick;
      PressedObject := nil;
    end;
  end;
end;

procedure TdxTokenEditController.MouseLeave;
begin
  UpdateHitTest;
end;

procedure TdxTokenEditController.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  UpdateHitTest(Point(X, Y));
end;

procedure TdxTokenEditController.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  UpdateHitTest(Point(X, Y));
  if PressedObject <> nil then
  begin
    if PressedObject = HitTest.HitObject then
    try
      ProcessClick;
    except
      // do nothing
    end;
    PressedObject := nil;
    UpdateHitTest;
  end;
end;

procedure TdxTokenEditController.DeleteToken(const ATokenIndex: Integer);
begin
  ReplaceToken(ATokenIndex, '');
end;

procedure TdxTokenEditController.ReplaceToken(const ATokenIndex: Integer; const ATokenText: string);
begin
  if Edit <> nil then
    Edit.ReplaceToken(ATokenText, ATokenIndex);
end;

function TdxTokenEditController.IsEditing: Boolean;
begin
  Result := (InnerEdit.EditValue <> '') or (EditingTokenIndex >= 0);
end;

procedure TdxTokenEditController.StartEditing(ATokenIndex: Integer);
begin
  InnerEdit.LockChanges(True);
  try
    EditingTokenIndex := ATokenIndex;
    InnerEdit.EditValue := ViewInfo.Tokens[ATokenIndex].Caption;
    InnerEdit.SelectAll;
  finally
    InnerEdit.LockChanges(False);
  end;
end;

procedure TdxTokenEditController.StopEditing(APostToken: Boolean);
begin
  if IsEditing then
  begin
    InnerEdit.LockChanges(True);
    try
      if APostToken then
        InnerEdit.PostToken;
      EditingTokenIndex := -1;
      InnerEdit.EditValue := '';
    finally
      InnerEdit.LockChanges(False);
    end;
  end;
end;

procedure TdxTokenEditController.ProcessClick;
begin
  case HitTest.HitObjectPartID of
    tehtToken:
      Edit.DoTokenClick(
        TdxTokenEditTokenViewInfo(HitTest.HitObject).Text,
        TdxTokenEditTokenViewInfo(HitTest.HitObject).Item);

    tehtTokenCloseGlyph:
      DeleteToken(TdxTokenEditTokenViewInfo(HitTest.HitObject).TokenIndex);

    tehtTokenGlyph:
      Edit.DoTokenGlyphClick(
        TdxTokenEditTokenViewInfo(HitTest.HitObject).Text,
        TdxTokenEditTokenViewInfo(HitTest.HitObject).Item);
  end;
end;

procedure TdxTokenEditController.ProcessDblClick;
begin
  if HitTest.HitObjectPartID = tehtToken then
    StartEditing(TdxTokenEditTokenViewInfo(HitTest.HitObject).TokenIndex);
end;

procedure TdxTokenEditController.UpdateHitTest(const P: TPoint);
begin
  HitTest.Reset;
  HitTest.Point := P;
  ViewInfo.CalculateHitTest(HitTest);
  FocusedObject := HitTest.HitObject;
end;

procedure TdxTokenEditController.UpdateHitTest;
begin
  if (ViewInfo.Edit <> nil) and ViewInfo.Edit.HandleAllocated then
    UpdateHitTest(ViewInfo.Edit.ScreenToClient(GetMouseCursorPos));
end;

procedure TdxTokenEditController.UpdateStates;
begin
  ViewInfo.UpdateState(Self);
end;

function TdxTokenEditController.GetEdit: TdxCustomTokenEdit;
begin
  Result := TdxCustomTokenEdit(ViewInfo.Edit);
end;

function TdxTokenEditController.GetEditingTokenIndex: Integer;
begin
  Result := ViewInfo.EditingTokenIndex;
end;

function TdxTokenEditController.GetInnerEdit: IdxInnerTokenEdit;
begin
  Result := Edit.InnerTextEdit as IdxInnerTokenEdit;
end;

procedure TdxTokenEditController.SetEditingTokenIndex(AValue: Integer);
begin
  if EditingTokenIndex <> AValue then
  begin
    ViewInfo.EditingTokenIndex := AValue;
    Edit.ShortRefreshContainer(False);
    Edit.UpdateInnerEditMaxLength;
  end;
end;

procedure TdxTokenEditController.SetFocusedObject(AValue: TObject);
begin
  if FFocusedObject <> AValue then
  begin
    FFocusedObject := AValue;
    UpdateStates;
  end;
end;

procedure TdxTokenEditController.SetPressedObject(AValue: TObject);
begin
  if FPressedObject <> AValue then
  begin
    FPressedObject := AValue;
    UpdateStates;
  end;
end;

{ TdxTokenEditInnerEdit }

destructor TdxTokenEditInnerEdit.Destroy;
begin
  HideAutoCompleteWindow;
  inherited Destroy;
end;

procedure TdxTokenEditInnerEdit.DeletePrevToken;
var
  AIndex: Integer;
begin
  AIndex := Container.ViewInfo.Tokens.Count - 1;
  if AIndex >= 0 then
    Controller.DeleteToken(AIndex);
end;

procedure TdxTokenEditInnerEdit.DoExit;
begin
  inherited DoExit;
  if ActiveProperties.PostEditValueOnFocusLeave and (Text <> '') then
    ProcessKeyReturn([]);
  HideAutoCompleteWindow;
end;

procedure TdxTokenEditInnerEdit.LockChanges(ALock: Boolean);
begin
  if ALock then
    Inc(FLockCount)
  else
    Dec(FLockCount);
end;

procedure TdxTokenEditInnerEdit.PostToken;
var
  AEditingTokenIndex: Integer;
begin
  AEditingTokenIndex := Controller.EditingTokenIndex;
  try
    Controller.ReplaceToken(AEditingTokenIndex, Text);
    Controller.EditingTokenIndex := -1;
    Text := '';
  except
    if AEditingTokenIndex = -1 then
      AEditingTokenIndex := Container.Properties.FirstInvalidTokenIndex;
    Controller.StartEditing(AEditingTokenIndex);
    raise;
  end;
end;

function TdxTokenEditInnerEdit.CreateHelper: TcxCustomInnerTextEditHelper;
begin
  Result := TdxTokenEditInnerEditHelper.Create(Self);
end;

procedure TdxTokenEditInnerEdit.Change;
begin
  inherited Change;
  if FLockCount = 0 then
  begin
    Container.ScrollToInnerEdit;
    RefreshAutoCompleteSuggestions;
  end;
end;

function TdxTokenEditInnerEdit.CheckShiftState(Shift: TShiftState): Boolean;
begin
  Result := [ssShift, ssCtrl, ssAlt] * Shift = [];
end;

procedure TdxTokenEditInnerEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_UP, VK_DOWN:
      begin
        if FAutoCompleteWindow <> nil then
          FAutoCompleteWindow.ProcessNavigationKey(Key, Shift);
        Key := 0;
      end;

    VK_RETURN:
      if FAutoCompleteWindow <> nil then
      begin
        if FAutoCompleteWindow.HasSelectedItem then
          FAutoCompleteWindow.ProcessNavigationKey(Key, Shift)
        else
          ProcessKeyReturn(Shift);

        HideAutoCompleteWindow;
        Key := 0;
      end
      else
        if ProcessKeyReturn(Shift) then
          Key := 0;

    VK_ESCAPE:
      if FAutoCompleteWindow <> nil then
        HideAutoCompleteWindow
      else
        if ProcessKeyEscape(Shift) then
          Key := 0;

    VK_TAB:
      if ActiveProperties.PostEditValueOnFocusLeave then
      begin
        if FAutoCompleteWindow <> nil then
        begin
          if not FAutoCompleteWindow.HasSelectedItem then
            TdxTokenEditAutoCompleteWindow(FAutoCompleteWindow).InnerListBox.ItemIndex := 0;
          Key := VK_RETURN;
          FAutoCompleteWindow.ProcessNavigationKey(Key, []);
          HideAutoCompleteWindow;
          Key := 0;
        end
        else
          ProcessKeyReturn([]);
      end;
  end;
  if Key <> 0 then
    inherited KeyDown(Key, Shift);
end;

procedure TdxTokenEditInnerEdit.KeyPress(var Key: Char);
begin
  case Ord(Key) of
    VK_ESCAPE:
      Key := #0;
    VK_BACK:
      if ProcessKeyBackspace([]) then
        Key := #0;
  else
    if Pos(Key, ActiveProperties.InputDelimiters) > 0 then
    begin
      PostToken;
      Key := #0;
    end;
  end;

  if Key <> #0 then
    inherited KeyPress(Key);
end;

function TdxTokenEditInnerEdit.ProcessKeyBackspace(Shift: TShiftState): Boolean;
begin
  Result := (Text = '') and CheckShiftState(Shift);
  if Result then
  begin
    if Controller.EditingTokenIndex < 0 then
      DeletePrevToken;
    RefreshAutoCompleteSuggestions;
  end;
end;

function TdxTokenEditInnerEdit.ProcessKeyEscape(Shift: TShiftState): Boolean;
begin
  Result := CheckShiftState(Shift) and ((Text <> '') or (Controller.EditingTokenIndex >= 0) or not Container.IsInplace);
  if Result then
  begin
    Controller.StopEditing(False);
    Text := '';
  end;
end;

function TdxTokenEditInnerEdit.ProcessKeyReturn(Shift: TShiftState): Boolean;
begin
  Result := False;
  if CheckShiftState(Shift) then
  begin
    LockChanges(True);
    try
      Result := (Text <> '') or (Controller.EditingTokenIndex >= 0) or not Container.IsInplace;
      PostToken;
      if Result then
      begin
        if ActiveProperties.ValidateOnEnter then
          Container.DoValidateOnEnter;
      end;
    finally
      LockChanges(False);
    end;
  end;
end;

procedure TdxTokenEditInnerEdit.HideAutoCompleteWindow;
begin
  FreeAndNil(FAutoCompleteWindow);
  FAutoCompleteWindowCustomSize := cxNullSize;
end;

function TdxTokenEditInnerEdit.IsAutoCompleteWindowVisible: Boolean;
begin
  Result := FAutoCompleteWindow <> nil;
end;

procedure TdxTokenEditInnerEdit.RefreshAutoCompleteSuggestions;
begin
  RefreshAutoCompleteSuggestions(Text,
    ActiveProperties.Lookup.FilterMode,
    ActiveProperties.Lookup.FilterSources);
end;

procedure TdxTokenEditInnerEdit.RefreshAutoCompleteSuggestions(const ASearchText: string;
  const AMode: TdxTokenEditLookupFilterMode; const ASources: TdxTokenEditLookupFilterSources);

  function IsValidSuggestion(const AText: string): Boolean;
  begin
    if AText = '' then
      Result := False
    else
      if AMode = tefmStartsWith then
        Result := AnsiStartsText(ASearchText, AText)
      else
        Result := AnsiContainsText(AText, ASearchText);
  end;

var
  ASuggestion: string;
  ASuggestions: TStringList;
  AToken: TdxTokenEditToken;
  ATokenIndex: Integer;
  I: Integer;
begin
  ASuggestions := TStringList.Create;
  try
    for I := 0 to ActiveProperties.Tokens.Count - 1 do
    begin
      AToken := ActiveProperties.Tokens[I];
      ATokenIndex := Container.ViewInfo.Tokens.IndexOf(AToken);
      if (ATokenIndex < 0) or (Controller.EditingTokenIndex = ATokenIndex) then
      begin
        if ActiveProperties.Lookup.DisplayMask <> EmptyStr then
        begin
          if ((tefsText in ASources) and IsValidSuggestion(AToken.Text)) or
            ((tefsDisplayText in ASources) and IsValidSuggestion(AToken.DisplayText)) then
          begin
            ASuggestion := StringReplace(ActiveProperties.Lookup.DisplayMask, dxTokenEditDefaultDisplayMaskText,
              AToken.Text, [rfReplaceAll]);
            ASuggestion := StringReplace(ASuggestion, dxTokenEditDefaultDisplayMaskDisplayText,
              AToken.DisplayText, [rfReplaceAll]);
            ASuggestions.AddObject(ASuggestion, AToken);
          end;
        end
        else
        begin
          if (tefsText in ASources) and IsValidSuggestion(AToken.Text) then
            ASuggestions.AddObject(AToken.Text, AToken);
          if (tefsDisplayText in ASources) and IsValidSuggestion(AToken.DisplayText) then
            ASuggestions.AddObject(AToken.DisplayText, AToken);
        end;
      end;
    end;
    if ActiveProperties.Lookup.Sorted then
      ASuggestions.Sort;
    RefreshAutoCompleteWindow(ASuggestions);
  finally
    ASuggestions.Free;
  end;
end;

procedure TdxTokenEditInnerEdit.RefreshAutoCompleteWindow(ASuggestions: TStrings);
begin
  if FLockCount <> 0 then
    Exit;
  if (ASuggestions.Count > 0) and ActiveProperties.Lookup.Active then
  begin
    if FAutoCompleteWindow = nil then
    begin
      FAutoCompleteWindow := TdxTokenEditAutoCompleteWindow.Create(Self);
      FAutoCompleteWindow.OnSelectItem := AutoCompleteWindowSelectItemHandler;
      FAutoCompleteWindow.OnStoreSize := AutoCompleteWindowStoreSizeHandler;
      FAutoCompleteWindow.BiDiMode := Container.BiDiMode;
    end;
    FAutoCompleteWindow.Populate(ASuggestions);
    FAutoCompleteWindow.SearchText := Text;

    FAutoCompleteWindow.OwnerBounds := ClientRect;
    FAutoCompleteWindow.Adjustable := cxSizeIsEmpty(FAutoCompleteWindowCustomSize);
    if not FAutoCompleteWindow.Adjustable then
      FAutoCompleteWindow.SetSize(FAutoCompleteWindowCustomSize);

    FAutoCompleteWindow.Popup(Self);
  end
  else
    HideAutoCompleteWindow;
end;

procedure TdxTokenEditInnerEdit.AutoCompleteWindowSelectItemHandler(Sender: TObject);
begin
  if FAutoCompleteWindow.HasSelectedItem then
  begin
    Text := (FAutoCompleteWindow.SelectedObject as TdxTokenEditToken).Text;
    PostToken;
  end;
end;

procedure TdxTokenEditInnerEdit.AutoCompleteWindowStoreSizeHandler(Sender: TObject);
begin
  if FAutoCompleteWindow <> nil then
    FAutoCompleteWindowCustomSize := cxSize(FAutoCompleteWindow.BoundsRect);
end;

function TdxTokenEditInnerEdit.GetActiveProperties: TdxTokenEditProperties;
begin
  Result := Container.ActiveProperties;
end;

function TdxTokenEditInnerEdit.GetContainer: TdxCustomTokenEdit;
begin
  Result := TdxCustomTokenEdit(inherited Container);
end;

function TdxTokenEditInnerEdit.GetController: TdxTokenEditController;
begin
  Result := Container.Controller;
end;

function TdxTokenEditInnerEdit.GetScaleFactor: TdxScaleFactor;
begin
  Result := Container.ScaleFactor;
end;

{ TdxTokenEditInnerEditHelper }

procedure TdxTokenEditInnerEditHelper.LockChanges(ALock: Boolean);
begin
  TdxTokenEditInnerEdit(Edit).LockChanges(ALock);
end;

procedure TdxTokenEditInnerEditHelper.PostToken;
begin
  TdxTokenEditInnerEdit(Edit).PostToken;
end;

{ TdxTokenEditAutoCompleteListBox }

function TdxTokenEditAutoCompleteListBox.IsItemWithText(AItem: TdxCustomListBoxItem; const AText: string): Boolean;
begin
  Result := AnsiContainsText(AItem.Caption, AText);
end;

{ TdxTokenEditAutoCompleteWindow }

constructor TdxTokenEditAutoCompleteWindow.Create(AInnerEdit: TdxTokenEditInnerEdit);
begin
  inherited Create(AInnerEdit);
  ItemsFont := AInnerEdit.Container.ActiveStyle.GetVisibleFont;
  LookAndFeel.MasterLookAndFeel := AInnerEdit.Container.ActiveStyle.LookAndFeel;
  DisplayRowsCount := AInnerEdit.ActiveProperties.Lookup.DropDownRows;
  HighlightSearchText := True;
end;

function TdxTokenEditAutoCompleteWindow.CalculateSize: TSize;
begin
  if Adjustable then
  begin
    InnerListBox.LayoutChanged;
    Result := InnerListBox.CalculateContentSize(DisplayRowsCount);
    Inc(Result.cx, 2 * dxAutoCompleteWindowBorderSize + NCWidth);
    Inc(Result.cy, 2 * dxAutoCompleteWindowBorderSize);
    Result.cx := Max(Result.cx, cxRectWidth(OwnerBounds));
  end
  else
    Result := cxNullSize;
end;

function TdxTokenEditAutoCompleteWindow.CreateInnerListBox: TdxCustomAutoCompleteInnerListBox;
begin
  Result := TdxTokenEditAutoCompleteListBox.Create(Self);
end;

{ TdxCustomTokenEdit }

constructor TdxCustomTokenEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FController := CreateController;
end;

destructor TdxCustomTokenEdit.Destroy;
begin
  FreeAndNil(FController);
  inherited Destroy;
end;

class function TdxCustomTokenEdit.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TdxTokenEditProperties;
end;

function TdxCustomTokenEdit.HasPopupWindow: Boolean;
begin
  Result := HasInnerEdit and (InnerEdit.Control as TdxTokenEditInnerEdit).IsAutoCompleteWindowVisible;
end;

procedure TdxCustomTokenEdit.AdjustInnerEditPosition;
var
  AClipRect: TRect;
  ARect: TRect;
begin
  if HasInnerEdit and HandleAllocated then
  begin
    ARect := cxRectOffset(ViewInfo.InnerEditRect, ViewInfo.ContentOffset);
    cxRectIntersect(AClipRect, ARect, ViewInfo.ContentDisplayRect);
    InnerEdit.Control.SetBounds(ARect.Left, ARect.Top, cxRectWidth(ARect), cxRectHeight(ARect));
    SetWindowRegion(InnerEdit.Control, cxRectOffset(AClipRect, ARect.TopLeft, False));
    AlignControls(InnerEdit.Control, ARect);
  end;
end;

procedure TdxCustomTokenEdit.BoundsChanged;
begin
  if UpdatingScrollBars then
    ShortRefreshContainer(False);
  inherited BoundsChanged;
  UpdateHitTest;
end;

function TdxCustomTokenEdit.CanAutoHeight: Boolean;
begin
  Result := True;
end;

function TdxCustomTokenEdit.CreateController: TdxTokenEditController;
begin
  Result := TdxTokenEditController.Create(ViewInfo);
end;

procedure TdxCustomTokenEdit.EnabledChanged;
begin
  inherited EnabledChanged;
  Controller.UpdateStates;
end;

procedure TdxCustomTokenEdit.DoFocusChanged;
begin
  inherited DoFocusChanged;
  UpdateScrollBars;
  UpdateHitTest;
end;

procedure TdxCustomTokenEdit.DoHideEdit(AExit: Boolean);
begin
  inherited DoHideEdit(AExit);
  if AExit then
  begin
    Controller.StopEditing(True);
    ViewInfo.ContentOffset := cxNullPoint;
    AdjustInnerEditPosition;
  end;
end;

procedure TdxCustomTokenEdit.DoSetSize;
begin
  inherited DoSetSize;
  UpdateScrollBars;
end;

procedure TdxCustomTokenEdit.DoShowEdit;
begin
  inherited DoShowEdit;
  if not ValidateErrorProcessing then
    InnerEdit.EditValue := Null;
end;

function TdxCustomTokenEdit.GetDisplayText: string;
begin
  Result := VarToStr(EditValue);
end;

function TdxCustomTokenEdit.GetHintText(APart: Integer): string;
begin
  Result := ViewInfo.GetHintText(APart);
  if ShowHint and (APart <= 0) and (Result = '') then
    Result := Hint;
end;

function TdxCustomTokenEdit.GetInnerEditClass: TControlClass;
begin
  Result := TdxTokenEditInnerEdit;
end;

function TdxCustomTokenEdit.GetViewInfoClass: TcxContainerViewInfoClass;
begin
  Result := TdxTokenEditViewInfo;
end;

procedure TdxCustomTokenEdit.Initialize;
begin
  inherited Initialize;
  BeepOnEnter := False;
end;

function TdxCustomTokenEdit.CanAddToken(AProperties: TdxTokenEditProperties; const ATokenText: string;
  AToken: TdxTokenEditToken): Boolean;
begin
  Result := (AProperties.AllowAddCustomTokens or (AToken <> nil)) and
    dxCallTokenCollectionChangingEvent(AProperties.OnTokenAdd, Self, ATokenText, AToken);
end;

function TdxCustomTokenEdit.DoTokenAdd(const ATokenText: string; AToken: TdxTokenEditToken): Boolean;
begin
  DisableUserAction;
  try
    Result := CanAddToken(Properties, ATokenText, AToken);
    if Result and (RepositoryItem <> nil) then
      Result := CanAddToken(ActiveProperties, ATokenText, AToken);
  finally
    EnableUserAction;
  end;
end;

procedure TdxCustomTokenEdit.DoTokenClick(const ATokenText: string; AToken: TdxTokenEditToken);
begin
  DisableUserAction;
  try
    dxCallTokenClickEvent(Properties.OnTokenClick, Self, ATokenText, AToken);
    if RepositoryItem <> nil then
      dxCallTokenClickEvent(ActiveProperties.OnTokenClick, Self, ATokenText, AToken);
  finally
    EnableUserAction;
  end;
end;

function TdxCustomTokenEdit.DoTokenDelete(const ATokenText: string; AToken: TdxTokenEditToken): Boolean;
begin
  DisableUserAction;
  try
    Result := dxCallTokenCollectionChangingEvent(Properties.OnTokenDelete, Self, ATokenText, AToken);
    if Result and (RepositoryItem <> nil) then
      Result := dxCallTokenCollectionChangingEvent(ActiveProperties.OnTokenDelete, Self, ATokenText, AToken);
  finally
    EnableUserAction;
  end;
end;

procedure TdxCustomTokenEdit.DoTokenGlyphClick(const ATokenText: string; AToken: TdxTokenEditToken);
begin
  DisableUserAction;
  try
    dxCallTokenClickEvent(Properties.OnTokenGlyphClick, Self, ATokenText, AToken);
    if RepositoryItem <> nil then
      dxCallTokenClickEvent(ActiveProperties.OnTokenGlyphClick, Self, ATokenText, AToken);
  finally
    EnableUserAction;
  end;
end;

procedure TdxCustomTokenEdit.InitScrollBarsParameters;
begin
  SetScrollBarInfo(sbVertical, 0, ViewInfo.ContentSize.cy - 1, 1,
    cxRectHeight(ViewInfo.ContentDisplayRect), -ViewInfo.ContentOffset.Y, True,
    (ViewInfo.Tokens.Count = 0) or (ViewInfo.Tokens.LastVisible <> nil));
end;

function TdxCustomTokenEdit.NeedsScrollBars: Boolean;
begin
  Result := True;
end;

procedure TdxCustomTokenEdit.Scroll(AScrollBarKind: TScrollBarKind; AScrollCode: TScrollCode; var AScrollPos: Integer);
begin
  case AScrollCode of
    scTop:
      AScrollPos := 0;
    scBottom:
      AScrollPos := ViewInfo.ContentSize.cy;
    scLineUp, scPageUp:
      AScrollPos := -ViewInfo.ContentOffset.Y - 1;
    scLineDown, scPageDown:
      AScrollPos := -ViewInfo.ContentOffset.Y + 1;
  end;
  ViewInfo.ContentOffset := cxPoint(0, -AScrollPos);
  ShortRefreshContainer(False);
  UpdateHitTest;
end;

procedure TdxCustomTokenEdit.ScrollToInnerEdit;

  function GetScrollDelta(const AInnerEditRect, AContentRect: TRect): Integer;
  begin
    if AInnerEditRect.Top < AContentRect.Top then
      Result := AInnerEditRect.Top - AContentRect.Top
    else
      if AInnerEditRect.Bottom > AContentRect.Bottom then
        Result := Min(AInnerEditRect.Bottom - AContentRect.Bottom, AInnerEditRect.Top - AContentRect.Top)
      else
        Result := 0;
  end;

var
  AScrollPos: Integer;
begin
  if HandleAllocated then
  begin
    UpdateScrollBars;
    if IsScrollBarActive(sbVertical) then
    begin
      AScrollPos := VScrollBar.Position + GetScrollDelta(ViewInfo.InnerEditRect, ViewInfo.ContentDisplayRect);
      Scroll(sbVertical, scPosition, AScrollPos);
    end;
  end;
end;

function TdxCustomTokenEdit.UseInnerControlScrollBarParameters: Boolean;
begin
  Result := False;
end;

procedure TdxCustomTokenEdit.MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  Controller.MouseDown(Button, Shift, X, Y);
end;

procedure TdxCustomTokenEdit.MouseLeave(AControl: TControl);
begin
  inherited;
  Controller.MouseLeave;
end;

procedure TdxCustomTokenEdit.MouseMove(Shift: TShiftState; X: Integer; Y: Integer);
begin
  inherited;
  Controller.MouseMove(Shift, X, Y);
end;

procedure TdxCustomTokenEdit.MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer);
begin
  inherited;
  Controller.MouseUp(Button, Shift, X, Y);
end;

procedure TdxCustomTokenEdit.ReplaceToken(const AText: string; ATokenIndex: Integer = -1);

  procedure InsertToken(ATokens: TStrings; var AIndex: Integer; const AText: string; AToken: TdxTokenEditToken);
  begin
    if (ATokens.IndexOf(AText) < 0) and DoTokenAdd(AText, AToken) then
    begin
      ATokens.InsertObject(AIndex, AText, AToken);
      Inc(AIndex);
    end;
  end;

var
  AToken: TdxTokenEditToken;
  ATokens: TStrings;
  ATokensToAdd: TStrings;
  I: Integer;
begin
  if ActiveProperties.ReadOnly or IsUserActionDisabled then Exit;

  BeginUserAction;
  try
    ATokens := ActiveProperties.EditValueToTokens(EditValue);
    try
      ATokensToAdd := ActiveProperties.EditValueToTokens(AText, ActiveProperties.InputDelimiters);
      try
        for I := 0 to ATokensToAdd.Count - 1 do
          if ATokensToAdd.Objects[I] = nil then
          begin
            AToken := ActiveProperties.Tokens.FindByDisplayText(ATokensToAdd[I]);
            if AToken <> nil then
            begin
              ATokensToAdd.Strings[I] := AToken.Text;
              ATokensToAdd.Objects[I] := AToken;
            end;
          end;

        if InRange(ATokenIndex, 0, ATokens.Count - 1) then
        begin
          if (ATokensToAdd.Count = 1) and (ATokens[ATokenIndex] = ATokensToAdd[0]) then
            Exit;
          if DoTokenDelete(ATokens[ATokenIndex], TdxTokenEditToken(ATokens.Objects[ATokenIndex])) then
            ATokens.Delete(ATokenIndex)
          else
            Abort;
        end;

        if not InRange(ATokenIndex, 0, ATokens.Count - 1) then
          ATokenIndex := ATokens.Count;
        for I := 0 to ATokensToAdd.Count - 1 do
          InsertToken(ATokens, ATokenIndex, ATokensToAdd[I], TdxTokenEditToken(ATokensToAdd.Objects[I]));
      finally
        ATokensToAdd.Free;
      end;
      UpdateEditValue(ATokens);
    finally
      ATokens.Free;
    end;
  finally
    EndUserAction;
  end;
end;

procedure TdxCustomTokenEdit.UpdateEditValue(ATokens: TStrings);
begin
  if DoEditing then
  begin
    LockChangeEvents(True);
    try
      EditValue := Properties.TokensToEditValue(ATokens);
      ModifiedAfterEnter := True;
      if ActiveProperties.ImmediatePost and CanPostEditValue and InternalValidateEdit then
      begin
        InternalPostEditValue;
        SynchronizeDisplayValue;
      end;
    finally
      LockChangeEvents(False);
    end;
  end;
end;

function TdxCustomTokenEdit.InternalGetEditingValue: TcxEditValue;
begin
  if InnerTextEdit <> nil then
    Result := InnerTextEdit.EditValue
  else
    Result := inherited InternalGetEditingValue;
end;

function TdxCustomTokenEdit.InternalSetText(const Value: string): Boolean;
begin
  Result := not (IsUserAction and not DoEditing);
  if Result then
    EditValue := Value;
end;

procedure TdxCustomTokenEdit.PopulateSizeProperties(var AEditSizeProperties: TcxEditSizeProperties);
begin
  inherited PopulateSizeProperties(AEditSizeProperties);
  AEditSizeProperties.MaxLineCount := ActiveProperties.MaxLineCount;
end;

procedure TdxCustomTokenEdit.PropertiesChanged(Sender: TObject);
begin
  inherited PropertiesChanged(Sender);
  UpdateInnerEditMaxLength;
end;

procedure TdxCustomTokenEdit.SetInternalDisplayValue(Value: Variant);
begin
  if IsHiding then
    inherited SetInternalDisplayValue(Null);
end;

function TdxCustomTokenEdit.SupportsSpelling: Boolean;
begin
  Result := IsTextInputMode;
end;

procedure TdxCustomTokenEdit.SynchronizeDisplayValue;
var
  ADisplayValue: Variant;
begin
  ActiveProperties.PrepareDisplayValue(EditValue, ADisplayValue, Focused);
  if ViewInfo.UpdateEditValue(ADisplayValue) then
    ShortRefreshContainer(False);
  UpdateInnerEditMaxLength;
end;

function TdxCustomTokenEdit.TabsNeeded: Boolean;
begin
  Result := True;
end;

procedure TdxCustomTokenEdit.UpdateHitTest;
begin
  if Controller <> nil then
    Controller.UpdateHitTest;
end;

procedure TdxCustomTokenEdit.UpdateInnerEditMaxLength;

  function CalculateInnerEditMaxLength(AMaxLength: Integer): Integer;
  var
    I: Integer;
  begin
    Result := AMaxLength;
    for I := 0 to ViewInfo.Tokens.Count - 1 do
    begin
      if I <> Controller.EditingTokenIndex then
        Dec(Result, Length(ViewInfo.Tokens[I].Text));
    end;
    Dec(Result, ViewInfo.Tokens.Count - Ord(Controller.EditingTokenIndex >= 0));
  end;

var
  AMaxLength: Integer;
begin
  if InnerTextEdit <> nil then
  begin
    AMaxLength := ActiveProperties.MaxLength;
    if AMaxLength > 0 then
    begin
      AMaxLength := CalculateInnerEditMaxLength(AMaxLength);
      InnerTextEdit.MaxLength := Max(AMaxLength, 1);
      InnerTextEdit.ReadOnly := AMaxLength <= 0;
    end
    else
      InnerTextEdit.MaxLength := 0;
  end;
end;

function TdxCustomTokenEdit.GetActiveProperties: TdxTokenEditProperties;
begin
  Result := TdxTokenEditProperties(InternalGetActiveProperties);
end;

function TdxCustomTokenEdit.GetProperties: TdxTokenEditProperties;
begin
  Result := TdxTokenEditProperties(inherited Properties);
end;

function TdxCustomTokenEdit.GetViewInfo: TdxTokenEditViewInfo;
begin
  Result := TdxTokenEditViewInfo(inherited ViewInfo);
end;

procedure TdxCustomTokenEdit.SetProperties(Value: TdxTokenEditProperties);
begin
  Properties.Assign(Value);
end;

initialization
  GetRegisteredEditProperties.Register(TdxTokenEditProperties, scxSEditRepositoryTokenItem);

finalization
  GetRegisteredEditProperties.Unregister(TdxTokenEditProperties);
end.
