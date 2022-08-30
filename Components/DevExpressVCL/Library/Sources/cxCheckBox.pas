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

unit cxCheckBox;

{$I cxVer.inc}

interface

uses
  Messages, Windows, Types, Variants, Classes, Controls, Forms, Graphics, SysUtils,
  dxCore, cxClasses, cxContainer, cxControls, cxDataStorage, dxGDIPlusClasses,
  cxGeometry, dxDPIAwareUtils, cxEdit, cxGraphics, cxLookAndFeels, cxTextEdit,
  cxDropDownEdit, cxVariants, cxFilterControlUtils, cxLookAndFeelPainters, dxFading;

const
  cxEditCheckBoxSingleBorderDefaultColor = clBtnShadow;

type
  TcxCustomCheckBox = class;
  TcxCustomCheckBoxViewInfo = class;

  TcxCheckBoxNullValueShowingStyle = (nssUnchecked, nssInactive, nssGrayedChecked);
  TcxCheckStatesValueFormatEx = (cvfCaptions, cvfIndices, cvfInteger, cvfStatesString, cvfCustom);
  TcxCheckStatesValueFormat = cvfCaptions..cvfStatesString;
  TcxCheckStates = array of TcxCheckBoxState;
  TcxEditCheckBoxBorderStyle = TcxEditBorderStyle;

  TcxCheckStatesToValueEvent = procedure(Sender: TObject;
    const ACheckStates: TcxCheckStates; out AValue: TcxEditValue) of object;
  TcxValueToCheckStatesEvent = procedure(Sender: TObject;
    const AValue: TcxEditValue; var ACheckStates: TcxCheckStates) of object;

  { IcxCheckItems }

  IcxCheckItems = interface
  ['{5BF13228-CF05-4741-9833-F2B8FBFD57ED}']
    function GetCaption(Index: Integer): string;
    function GetCount: Integer;
    property Captions[Index: Integer]: string read GetCaption;
    property Count: Integer read GetCount;
  end;

  { TcxCaptionItem }

  TcxCaptionItem = class(TCollectionItem)
  private
    FCaption: TCaption;
    FTag: TcxTag;

    function IsTagStored: Boolean;
    procedure SetCaption(AValue: TCaption);
  protected
    property Caption: TCaption read FCaption write SetCaption;
    property Tag: TcxTag read FTag write FTag stored IsTagStored;
  public
    procedure Assign(Source: TPersistent); override;
  end;

  { TcxCaptionItems }

  TcxCaptionItems = class(TcxOwnedInterfacedCollection, IcxCheckItems)
  private
    function GetItem(AIndex: Integer): TcxCaptionItem;
    procedure SetItem(AIndex: Integer; AValue: TcxCaptionItem);
  protected
    function DoCompareItems(AItem1, AItem2: TcxInterfacedCollectionItem): Integer; override;

    // IcxCheckItems
    function GetCaption(AIndex: Integer): string;
    function GetCount: Integer;
  public
    property Items[AIndex: Integer]: TcxCaptionItem read GetItem write SetItem; default;
  end;

  { TcxCustomCheckControlFadingHelper }

  TcxCustomCheckControlFadingHelper = class(TcxCustomEditFadingHelper)
  private
    FViewInfo: TcxCustomCheckBoxViewInfo;
    FState: TcxEditCheckState;
  protected
    function CanFade: Boolean; override;
    function GetEditViewInfo: TcxCustomEditViewInfo; override;
  public
    constructor Create(AViewInfo: TcxCustomCheckBoxViewInfo); virtual;

    procedure UpdateState;

    property State: TcxEditCheckState read FState;
    property ViewInfo: TcxCustomCheckBoxViewInfo read FViewInfo;
  end;

  { TcxCheckBoxFadingHelper }

  TcxCheckBoxFadingHelper = class(TcxCustomCheckControlFadingHelper)
  protected
    procedure GetFadingImages(out AFadeOutImage, AFadeInImage: TcxBitmap); override;
  public
    procedure Invalidate; override;
  end;

  TcxCheckControlFadingHelperClass = class of TcxCustomCheckControlFadingHelper;

  { TcxCustomCheckBoxViewInfo }

  TcxCustomCheckBoxViewInfo = class(TcxCustomTextEditViewInfo)
  private
    function GetEdit: TcxCustomCheckBox;
  protected
    function GetCheckControlFadingHelperClass: TcxCheckControlFadingHelperClass; virtual;
    procedure InternalPaint(ACanvas: TcxCanvas); override;
    function IsTextEnabled: Boolean;
    procedure StoreLastState; override;
  public
    Alignment: TAlignment;
    CheckBorderOffset: Integer;
    CheckBoxBorderStyle: TcxEditCheckBoxBorderStyle;
    CheckBoxGlyph: TdxSmartGlyph;
    CheckBoxGlyphCount: Integer;
    CheckBoxLastState: TcxEditCheckState;
    CheckBoxRect: TRect;
    CheckBoxSize: TSize;
    CheckBoxState: TcxEditCheckState;
    DrawCaptionFlags: Integer;
    FadingHelper: TcxCustomCheckControlFadingHelper;
    FocusRect: TRect;
    HasGlyph: Boolean;
    IsTextColorAssigned: Boolean;
    NullValueShowingStyle: TcxCheckBoxNullValueShowingStyle;
    State: TcxCheckBoxState;
    constructor Create; override;
    destructor Destroy; override;
    procedure Assign(Source: TObject); override;
    procedure DrawText(ACanvas: TcxCanvas); override;
    function GetUpdateRegion(AViewInfo: TcxContainerViewInfo): TcxRegion; override;
    function IsHotTrack: Boolean; override;
    function IsHotTrack(P: TPoint): Boolean; override;
    function NeedShowHint(ACanvas: TcxCanvas;
      const P: TPoint; const AVisibleBounds: TRect; out AText: TCaption;
      out AIsMultiLine: Boolean; out ATextRect: TRect; AMaxLineCount: Integer = 0): Boolean; override;
    procedure Offset(DX, DY: Integer); override;
    function Repaint(AControl: TWinControl; const AInnerEditRect: TRect;
      AViewInfo: TcxContainerViewInfo = nil): Boolean; override;
    //
    property Edit: TcxCustomCheckBox read GetEdit;
  end;

  { TcxCustomCheckBoxViewData }

  TcxCustomCheckBoxProperties = class;

  TcxCustomCheckBoxViewData = class(TcxCustomEditViewData)
  private
    function GetProperties: TcxCustomCheckBoxProperties;
    function GetTextFlags(ADraw: Boolean): Integer;
  protected
    function InternalGetEditConstantPartSize(ACanvas: TcxCanvas; AIsInplace: Boolean;
      AEditSizeProperties: TcxEditSizeProperties;
      var MinContentSize: TSize; AViewInfo: TcxCustomEditViewInfo): TSize; override;
    function GetCalculateTextFlags: Integer;
    function GetDrawTextFlags: Integer; virtual;
    function GetIsEditClass: Boolean;
    function GetTextGap: Integer; virtual;
    function IsCheckPressed: Boolean;

    function CalculateCheckBoxRect(AViewInfo: TcxCustomCheckBoxViewInfo): TRect; virtual;
    function CalculateCheckBoxState(AViewInfo: TcxCustomCheckBoxViewInfo;
      const P: TPoint; AButton: TcxMouseButton; AShift: TShiftState): TcxEditCheckState; virtual;
    procedure CalculateFocusRect(AViewInfo: TcxCustomCheckBoxViewInfo); virtual;
    function CalculateTextRect(AViewInfo: TcxCustomCheckBoxViewInfo): TRect; virtual;
  public
    procedure Calculate(ACanvas: TcxCanvas; const ABounds: TRect; const P: TPoint;
      Button: TcxMouseButton; Shift: TShiftState; AViewInfo: TcxCustomEditViewInfo;
      AIsMouseEvent: Boolean); override;
    procedure EditValueToDrawValue(const AEditValue: TcxEditValue;
      AViewInfo: TcxCustomEditViewInfo); override;
    function GetBorderStyle: TcxEditBorderStyle; override;
    function GetContentOffset: Integer; virtual;
    function GetEditContentSize(ACanvas: TcxCanvas; const AEditValue: Variant;
      const AEditSizeProperties: TcxEditSizeProperties; AErrorData: TcxEditValidateInfo = nil): TSize; override;

    property Properties: TcxCustomCheckBoxProperties read GetProperties;
  end;

  { TcxCheckBoxStyle }

  TcxCheckBoxStyle = class(TcxEditStyle)
  public
    function HasBorder: Boolean; override;
  end;

  { TcxCustomCheckBoxProperties }

  TcxCustomCheckBoxProperties = class(TcxCustomEditProperties, IdxMultiPartGlyphSupport)
  private
    FAllowGrayed: Boolean;
    FCaption: TCaption; // obsolete
    FDisplayCheckState: array [TcxCheckBoxState] of WideString;
    FFullFocusRect: Boolean;
    FGlyph: TdxSmartGlyph;
    FGlyphCount: Integer;
    FIsCaptionAssigned: Boolean; // obsolete
    FIsDisplayCheckStateAssigned: array [TcxCheckBoxState] of Boolean;
    FMultiLine: Boolean;
    FNullStyle: TcxCheckBoxNullValueShowingStyle;
    FShowEndEllipsis: Boolean;
    FUseAlignmentWhenInplace: Boolean;
    FValueChecked: TcxEditValue;
    FValueGrayed: TcxEditValue;
    FValueUnchecked: TcxEditValue;
    function GetAlignment: TAlignment;
    function GetDefaultDisplayCheckState(AState: TcxCheckBoxState): string;
    function GetDisplayCheckState(const Index: Integer): WideString;
    function GetGlyph: TdxSmartGlyph;
    function GetInternalAlignment: TcxEditAlignment;
    function IsAlignmentStored: Boolean;
    function IsDisplayCheckStateStored(Index: Integer): Boolean;
    function IsLoading: Boolean;
    function IsValueCheckedStored: Boolean;
    function IsValueGrayedStored: Boolean;
    function IsValueUncheckedStored: Boolean;
    procedure ReadCaption(Reader: TReader); // obsolete
    procedure SetAlignment(Value: TAlignment);
    procedure SetCaption(const Value: TCaption); // obsolete
    procedure SetDisplayCheckState(const Index: Integer;
      const Value: WideString);
    procedure SetFullFocusRect(Value: Boolean);
    procedure SetGlyph(Value: TdxSmartGlyph);
    procedure SetGlyphCount(Value: Integer);
    procedure SetMultiLine(Value: Boolean);
    procedure SetNullStyle(Value: TcxCheckBoxNullValueShowingStyle);
    procedure SetShowEndEllipsis(Value: Boolean);
    procedure SetStateValues(const AValueChecked, AValueGrayed, AValueUnchecked: TcxEditValue);
    procedure SetUseAlignmentWhenInplace(Value: Boolean);
    procedure SetValueChecked(const Value: TcxEditValue);
    procedure SetValueGrayed(const Value: TcxEditValue);
    procedure SetValueUnchecked(const Value: TcxEditValue);
  protected
    // IdxMultiPartGlyphSupport
    function GetStateCaption(AIndex: Integer): string;
    function GetGlyphCount: Integer;

    function CanValidate: Boolean; override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure DoAssign(AProperties: TcxCustomEditProperties); override;
    class function GetViewDataClass: TcxCustomEditViewDataClass; override;
    function HasDisplayValue: Boolean; override;

    function CheckValue(const AValue: TcxEditValue): Boolean;
    function GetState(const AEditValue: TcxEditValue): TcxCheckBoxState;
    function InternalGetGlyph: TdxSmartGlyph; virtual;
    function IsEmbeddedEdit: Boolean; virtual;
    property InternalAlignment: TcxEditAlignment read GetInternalAlignment;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    function CanCompareEditValue: Boolean; override;
    function CompareDisplayValues(
      const AEditValue1, AEditValue2: TcxEditValue): Boolean; override;
    class function GetContainerClass: TcxContainerClass; override;
    function GetDisplayText(const AEditValue: TcxEditValue;
      AFullText: Boolean = False; AIsInplace: Boolean = True): string; override;
    class function GetStyleClass: TcxCustomEditStyleClass; override;
    function GetSpecialFeatures: TcxEditSpecialFeatures; override;
    function GetSupportedOperations: TcxEditSupportedOperations; override;
    class function GetViewInfoClass: TcxContainerViewInfoClass; override;
    function IsActivationKey(AKey: Char): Boolean; override;
    function IsEditValueValid(var EditValue: TcxEditValue; AEditFocused: Boolean): Boolean; override;
    function IsResetEditClass: Boolean; override;
    procedure PrepareDisplayValue(const AEditValue: TcxEditValue;
      var DisplayValue: TcxEditValue; AEditFocused: Boolean); override;
    // !!!
    property Alignment: TAlignment read GetAlignment write SetAlignment
      stored IsAlignmentStored;
//    property AlignmentVert: TcxAlignmentVert read GetAlignmentVert
//      write SetAlignmentVert stored IsAlignmentVertStored;
    property AllowGrayed: Boolean read FAllowGrayed write FAllowGrayed default False;
    property Caption: TCaption read FCaption write SetCaption stored False; // obsolete
    property DisplayChecked: WideString index cbsChecked read GetDisplayCheckState write SetDisplayCheckState
      stored IsDisplayCheckStateStored;
    property DisplayGrayed: WideString index cbsGrayed read GetDisplayCheckState write SetDisplayCheckState
      stored IsDisplayCheckStateStored;
    property DisplayUnchecked: WideString index cbsUnchecked read GetDisplayCheckState write SetDisplayCheckState
      stored IsDisplayCheckStateStored;
    property FullFocusRect: Boolean read FFullFocusRect write SetFullFocusRect default False;
    property Glyph: TdxSmartGlyph read GetGlyph write SetGlyph;
    property GlyphCount: Integer read FGlyphCount write SetGlyphCount default 6;
    property MultiLine: Boolean read FMultiLine write SetMultiLine default False;
    property NullStyle: TcxCheckBoxNullValueShowingStyle read FNullStyle write
      SetNullStyle default nssGrayedChecked;
    property ShowEndEllipsis: Boolean read FShowEndEllipsis write SetShowEndEllipsis default False;
    property UseAlignmentWhenInplace: Boolean read FUseAlignmentWhenInplace
      write SetUseAlignmentWhenInplace default False;
    property ValueChecked: TcxEditValue read FValueChecked write SetValueChecked
      stored IsValueCheckedStored;
    property ValueGrayed: TcxEditValue read FValueGrayed write SetValueGrayed
      stored IsValueGrayedStored;
    property ValueUnchecked: TcxEditValue read FValueUnchecked write SetValueUnchecked
      stored IsValueUncheckedStored;
  end;

  { TcxCheckBoxProperties }

  TcxCheckBoxProperties = class(TcxCustomCheckBoxProperties)
  published
    property Alignment;
    property AllowGrayed;
    property AssignedValues;
    property Caption; // obsolete
    property ClearKey;
    property DisplayChecked;
    property DisplayUnchecked;
    property DisplayGrayed;
    property FullFocusRect;
    property Glyph;
    property GlyphCount;
    property ImmediatePost;
    property MultiLine;
    property NullStyle;
    property ReadOnly;
    property ShowEndEllipsis;
    property UseAlignmentWhenInplace;
    property ValidationErrorIconAlignment;
    property ValidationOptions;
    property ValueChecked;
    property ValueGrayed;
    property ValueUnchecked;
    property OnChange;
    property OnEditValueChanged;
    property OnValidate;
  end;

  { TcxCustomCheckBox }

  TcxCustomCheckBox = class(TcxCustomEdit)
  private
    FIsCheckPressed: Boolean;
    FIsLoaded: Boolean;
    FIsLoadingStateAssigned: Boolean;
    FLoadingState: TcxCheckBoxState;

    function GetChecked: Boolean;
    function GetProperties: TcxCustomCheckBoxProperties;
    function GetActiveProperties: TcxCustomCheckBoxProperties;
    function GetState: TcxCheckBoxState;
    function GetStyle: TcxCheckBoxStyle;
    function GetViewInfo: TcxCustomCheckBoxViewInfo;
    function IsStateStored: Boolean;
    procedure SetChecked(Value: Boolean);
    procedure SetProperties(Value: TcxCustomCheckBoxProperties);
    procedure SetState(Value: TcxCheckBoxState);
    procedure SetStyle(Value: TcxCheckBoxStyle);
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMLButtonUp(var Message: TWMLButtonUp); message WM_LBUTTONUP;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CMParentColorChanged(var Message: TMessage); message CM_PARENTCOLORCHANGED;
  protected
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
    function GetActionLinkClass: TControlActionLinkClass; override;
    procedure Loaded; override;

    function GetDefaultTextColorByPainter: TColor; override;

    procedure AcceleratorClick; override;
    function CanAutoHeight: Boolean; override;
    function CanAutoWidth: Boolean; override;
    function IsHeightDependOnWidth: Boolean; override;
    function CanHaveTransparentBorder: Boolean; override;
    function DefaultParentColor: Boolean; override;
    procedure DoEditKeyDown(var Key: Word; Shift: TShiftState); override;
    procedure DoEditKeyPress(var Key: Char); override;
    procedure DoEditKeyUp(var Key: Word; Shift: TShiftState); override;
    procedure DoExit; override;
    procedure DoProcessEventsOnViewInfoChanging; override;
    procedure PopulateSizeProperties(var AEditSizeProperties: TcxEditSizeProperties); override;
    function GetEditStateColorKind: TcxEditStateColorKind; override;
    function GetDisplayValue: TcxEditValue; override;
    function GetShadowBounds: TRect; override;
    procedure Initialize; override;
    function InternalGetNotPublishedStyleValues: TcxEditStyleValues; override;
    procedure InternalSetEditValue(const Value: TcxEditValue;
      AValidateEditValue: Boolean); override;
    function IsClickEnabledDuringLoading: Boolean; override;
    function IsNativeBackground: Boolean; override;
    procedure PropertiesChanged(Sender: TObject); override;
    procedure TextChanged; override;
    procedure SetInternalDisplayValue(Value: TcxEditValue); override;

    procedure InvalidateCheckRect;
    procedure Toggle; virtual;
    //
    property Caption;
    property Checked: Boolean read GetChecked write SetChecked;
    property ViewInfo: TcxCustomCheckBoxViewInfo read GetViewInfo;
  public
    procedure Clear; override;
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    procedure PrepareEditValue(const ADisplayValue: TcxEditValue;
      out EditValue: TcxEditValue; AEditFocused: Boolean); override;
    //
    property ActiveProperties: TcxCustomCheckBoxProperties read GetActiveProperties;
    property Properties: TcxCustomCheckBoxProperties read GetProperties write SetProperties;
    property State: TcxCheckBoxState read GetState write SetState stored IsStateStored default cbsUnchecked;
    property Style: TcxCheckBoxStyle read GetStyle write SetStyle;
    property Transparent;
  end;

  { TcxCheckBox }

  TcxCheckBox = class(TcxCustomCheckBox)
  private
    function GetActiveProperties: TcxCheckBoxProperties;
    function GetProperties: TcxCheckBoxProperties;
    procedure SetProperties(Value: TcxCheckBoxProperties);
  public
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    property ActiveProperties: TcxCheckBoxProperties read GetActiveProperties;
  published
    property Action;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property Caption;
    property Checked stored False;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentBackground;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Properties: TcxCheckBoxProperties read GetProperties
      write SetProperties;
    property ShowHint;
    property State;
    property Style;
    property StyleDisabled;
    property StyleFocused;
    property StyleHot;
    property TabOrder;
    property TabStop;
    property Transparent;
    property Visible;
    property OnClick;
    property OnContextPopup;
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

  { TcxFilterCheckBoxHelper }

  TcxFilterCheckBoxHelper = class(TcxFilterComboBoxHelper)
  public
    class function GetFilterDataType(AValueTypeClass: TcxValueTypeClass): TcxFilterDataType; override;
    class procedure GetFilterValue(AEdit: TcxCustomEdit;
      AEditProperties: TcxCustomEditProperties; var V: Variant; var S: TCaption); override;
    class function GetSupportedFilterOperators(
      AProperties: TcxCustomEditProperties;
      AValueTypeClass: TcxValueTypeClass;
      AExtendedSet: Boolean = False): TcxFilterControlOperators; override;
    class procedure InitializeProperties(AProperties,
      AEditProperties: TcxCustomEditProperties; AHasButtons: Boolean); override;
    class procedure SetFilterValue(AEdit: TcxCustomEdit; AEditProperties: TcxCustomEditProperties;
      AValue: Variant); override;
    class function UseDisplayValue: Boolean; override;
  end;

  { TcxCheckBoxActionLink }

  TcxCheckBoxActionLink = class(TWinControlActionLink)
  protected
    FClient: TcxCustomCheckBox;
    procedure AssignClient(AClient: TObject); override;
    function IsCheckedLinked: Boolean; override;
    procedure SetChecked(Value: Boolean); override;
    procedure InternalSetChecked(Value: Boolean);
  end;

function CalculateCheckStatesValue(const ACheckStates: TcxCheckStates;
  AItems: IcxCheckItems; AValueFormat: TcxCheckStatesValueFormat): TcxEditValue;
function CalculateCheckStates(const AValue: TcxEditValue;
  AItems: IcxCheckItems; AValueFormat: TcxCheckStatesValueFormat;
  var ACheckStates: TcxCheckStates): Boolean;
procedure DrawEditCheck(ACanvas: TcxCanvas; const ACheckRect: TRect;
  ACheckState: TcxEditCheckState; AViewInfo: TcxCustomCheckBoxViewInfo); overload;
procedure DrawEditCheck(ACanvas: TcxCanvas; const ACheckRect: TRect;
  AState: TcxCheckBoxState; ACheckState: TcxEditCheckState; AGlyph: TdxSmartGlyph;
  AGlyphCount: Integer; ABorderStyle: TcxEditCheckBoxBorderStyle;
  ANativeStyle: Boolean; ABorderColor: TColor; ABackgroundColor: TColor;
  ADrawBackground, AIsDesigning, AFocused, ASupportGrayed: Boolean;
  APainter: TcxCustomLookAndFeelPainter;
  AGrayedShowingStyle: TcxCheckBoxNullValueShowingStyle = nssGrayedChecked); overload;
procedure DrawScaledEditCheck(ACanvas: TcxCanvas; const ACheckRect: TRect;
  ACheckState: TcxEditCheckState; AViewInfo: TcxCustomCheckBoxViewInfo; AScaleFactor: TdxScaleFactor); overload;
procedure DrawScaledEditCheck(ACanvas: TcxCanvas; const ACheckRect: TRect;
  AState: TcxCheckBoxState; ACheckState: TcxEditCheckState; AGlyph: TdxSmartGlyph;
  AGlyphCount: Integer; ABorderStyle: TcxEditCheckBoxBorderStyle;
  ANativeStyle: Boolean; ABorderColor: TColor; ABackgroundColor: TColor;
  ADrawBackground, AIsDesigning, AFocused, ASupportGrayed: Boolean;
  APainter: TcxCustomLookAndFeelPainter; AScaleFactor: TdxScaleFactor;
  AGrayedShowingStyle: TcxCheckBoxNullValueShowingStyle = nssGrayedChecked); overload;
function GetCheckBoxStateCaptionByGlyphIndex(AIndex: Integer): string;
function GetEditCheckBorderOffset(ACheckBorderStyle: TcxContainerBorderStyle;
  ANativeStyle, AHasGlyph: Boolean; APainter: TcxCustomLookAndFeelPainter): Integer; overload;
function GetEditCheckBorderOffset(ACheckBorderStyle: TcxEditBorderStyle;
  ANativeStyle, AHasGlyph: Boolean; APainter: TcxCustomLookAndFeelPainter): Integer; overload;
function GetEditCheckBorderOffset(ALookAndFeelKind: TcxLookAndFeelKind;
  ANativeStyle, AHasGlyph: Boolean; APainter: TcxCustomLookAndFeelPainter): Integer; overload;
function GetEditCheckGlyphIndex(AState: TcxCheckBoxState;
  ACheckState: TcxEditCheckState; ASupportGrayed: Boolean;
  AGlyphCount: Integer): Integer;
function GetEditCheckSize(ACanvas: TcxCanvas; ANativeStyle: Boolean;
  AGlyph: TdxSmartGlyph; AGlyphCount: Integer; APainter: TcxCustomLookAndFeelPainter): TSize; overload;
function GetScaledEditCheckSize(ACanvas: TcxCanvas; ANativeStyle: Boolean;
  AGlyph: TdxSmartGlyph; AGlyphCount: Integer; APainter: TcxCustomLookAndFeelPainter; AScaleFactor: TdxScaleFactor): TSize; overload;

procedure DrawCheckBoxText(ACanvas: TcxCanvas; AText: string; AFont: TFont;
  ATextColor: TColor; ATextRect: TRect; ADrawTextFlags: Integer; AEnabled: Boolean);

implementation

uses
  Math, ActnList, dxThemeConsts, dxThemeManager, dxUxTheme, cxEditConsts,
  cxEditPaintUtils, cxDrawTextUtils, cxEditUtils, dxSmartImage;

const
  ACheckTextGap = 3;
  AUsualContentOffset = 2;
  AInplaceContentOffset = 1;

const
  CheckStateToButtonState: array [TcxEditCheckState] of TcxButtonState = (
    cxbsNormal, cxbsHot, cxbsPressed, cxbsDisabled
  );

type
  TdxSmartGlyphAccess = class(TdxSmartGlyph);

procedure CalculateCheckBoxViewInfo(AViewData: TcxCustomCheckBoxViewData; AViewInfo:
  TcxCustomCheckBoxViewInfo; AIsMouseEvent: Boolean);
begin
  AViewData.CalculateViewInfo(AViewInfo, AIsMouseEvent);
  AViewInfo.TextRect := cxRectContent(AViewInfo.ClientRect, GetTextEditDrawTextOffset(AViewData));
end;

function CalculateCheckStatesValue(const ACheckStates: TcxCheckStates;
  AItems: IcxCheckItems; AValueFormat: TcxCheckStatesValueFormat): TcxEditValue;

  function CalculateCaptionsValue: TcxEditValue;
  var
    ACheckedCaptions, AGrayedCaptions: string;
    I: Integer;
  begin
    ACheckedCaptions := '';
    AGrayedCaptions := '';
    for I := 0 to Length(ACheckStates) - 1 do
    begin
      if ACheckStates[I] <> cbsUnchecked then
        if ACheckStates[I] = cbsGrayed then
          AGrayedCaptions := AGrayedCaptions +
            IntToStr(Length(AItems.Captions[I])) + ':' + AItems.Captions[I]
        else
          ACheckedCaptions := ACheckedCaptions +
            IntToStr(Length(AItems.Captions[I])) + ':' + AItems.Captions[I];
    end;
    Result := AGrayedCaptions;
    if Length(ACheckedCaptions) > 0 then
      Result := Result + ';' + ACheckedCaptions;
  end;

  function CalculateIndicesValue: TcxEditValue;
  var
    ACheckedCaptions, AGrayedCaptions: string;
    I: Integer;
  begin
    ACheckedCaptions := '';
    AGrayedCaptions := '';
    for I := 0 to Length(ACheckStates) - 1 do
      if ACheckStates[I] <> cbsUnchecked then
        if ACheckStates[I] = cbsGrayed then
        begin
          if AGrayedCaptions <> '' then
            AGrayedCaptions := AGrayedCaptions + ',';
          AGrayedCaptions := AGrayedCaptions + IntToStr(I);
        end
        else
        begin
          if ACheckedCaptions <> '' then
            ACheckedCaptions := ACheckedCaptions + ',';
          ACheckedCaptions := ACheckedCaptions + IntToStr(I);
        end;
    Result := AGrayedCaptions;
    if Length(ACheckedCaptions) > 0 then
      Result := Result + ';' + ACheckedCaptions;
  end;

  function CalculateStatesIntegerValue: TcxEditValue;
  var
    V: Int64;
    I, L: Integer;
  begin
    V := 0;
    L := Length(ACheckStates);
    if L > SizeOf(Int64) * 8 then
      L := SizeOf(Int64) * 8;
    for I := L - 1 downto 0 do
    begin
      V := V shl 1;
      V := V + Int64(ACheckStates[I] = cbsChecked);
    end;
    Result := V;
    VarCast(Result, Result, varInt64);
  end;

  function CalculateStatesStringValue: TcxEditValue;
  var
    I: Integer;
    S: string;
  begin
    SetLength(S, Length(ACheckStates));
    for I := 0 to High(ACheckStates) do
      S[I + 1] := Char(Integer(ACheckStates[I]) + Ord('0'));
    Result := S;
  end;

begin
  Result := Null;
  case AValueFormat of
    cvfCaptions:
      Result := CalculateCaptionsValue;
    cvfIndices:
      Result := CalculateIndicesValue;
    cvfInteger:
      Result := CalculateStatesIntegerValue;
    cvfStatesString:
      Result := CalculateStatesStringValue;
  end;
end;

function CalculateCheckStates(const AValue: TcxEditValue;
  AItems: IcxCheckItems; AValueFormat: TcxCheckStatesValueFormat;
  var ACheckStates: TcxCheckStates): Boolean;

  function GetNumber(var ANumber, AIndex: Integer;
    const S: string): Boolean;

    function IsDigit(C: Char): Boolean;
    begin
      Result := (C >= '0') and (C <= '9');
    end;

  const
    AMaxLength = MaxInt div 10;
    AMaxIntLastDigit = MaxInt mod 10;
  var
    L: Integer;
    D: Integer;
  begin
    Result := False;
    L := Length(S);
    if (AIndex > L) or not IsDigit(S[AIndex]) then
      Exit;

    ANumber := 0;
    repeat
      D := StrToInt(S[AIndex]);
      if (ANumber > AMaxLength) or
        ((ANumber = AMaxLength) and (D > AMaxIntLastDigit)) then
          Exit;
      ANumber := ANumber * 10 + D;
      Inc(AIndex);
    until (AIndex > L) or not IsDigit(S[AIndex]);
    Result := True;
  end;

  function CalculateItemsByCaptionsValue: Boolean;

    function GetCaptions(ACaptions: TStringList): Boolean;
    var
      S: string;
      ACaptionLength, AIndex, AValueLength: Integer;
      AChecked: Boolean;
    begin
      S := VarToStr(AValue);
      Result := S = '';
      if Result then
        Exit;

      Result := False;
      AChecked := False;
      AValueLength := Length(S);
      AIndex := 1;
      repeat
        if (S[AIndex] = ';') and not AChecked then
        begin
          AChecked := True;
          Inc(AIndex);
        end;
        if not GetNumber(ACaptionLength, AIndex, S) then
          Exit;
        if (AIndex > AValueLength) or (S[AIndex] <> ':') then
          Exit;
        Inc(AIndex);
        if AIndex + ACaptionLength - 1 > AValueLength then
          Exit;
        ACaptions.AddObject(Copy(S, AIndex, ACaptionLength),
          Pointer(AChecked));
        Inc(AIndex, ACaptionLength);
        if AIndex > AValueLength then
          Break;
      until (AIndex > AValueLength);
      Result := True;
    end;

    function CalculateStates(ACaptions: TStringList): Boolean;
    var
      AIndex, I: Integer;
    begin
      for I := 0 to AItems.Count - 1 do
      begin
        AIndex := ACaptions.IndexOf(AItems.Captions[I]);
        if AIndex = -1 then
          ACheckStates[I] := cbsUnchecked
        else
        begin
          if Boolean(ACaptions.Objects[AIndex]) then
            ACheckStates[I] := cbsChecked
          else
            ACheckStates[I] := cbsGrayed;
          ACaptions.Delete(AIndex);
        end;
      end;
      Result := ACaptions.Count = 0;
    end;

  var
    ACaptions: TStringList;
  begin
    if not(VarIsNull(AValue) or VarIsStr(AValue)) then
    begin
      Result := False;
      Exit;
    end;
    ACaptions := TStringList.Create;
    try
      Result := GetCaptions(ACaptions);
      if not Result then
        Exit;
      ACaptions.Sort;
      Result := CalculateStates(ACaptions);
    finally
      FreeAndNil(ACaptions);
    end;
  end;

  procedure SetStatesToUnchecked;
  var
    I: Integer;
  begin
    for I := 0 to AItems.Count - 1 do
      ACheckStates[I] := cbsUnchecked;
  end;

  function CalculateItemsByIndicesValue: Boolean;
  var
    AChecked: Boolean;
    AIndex, AItemIndex, L: Integer;
    S: string;
  begin
    Result := VarIsNull(AValue) or VarIsStr(AValue);
    if not Result then
      Exit;

    S := VarToStr(AValue);
    SetStatesToUnchecked;
    if S = '' then
      Exit;

    Result := False;
    AIndex := 1;
    L := Length(S);
    AChecked := False;
    repeat
      if (S[AIndex] = ';') and not AChecked then
      begin
        AChecked := True;
        Inc(AIndex);
      end;
      if not GetNumber(AItemIndex, AIndex, S) or (AItemIndex >= AItems.Count) then
        Exit;
      if AChecked then
        ACheckStates[AItemIndex] := cbsChecked
      else
        ACheckStates[AItemIndex] := cbsGrayed;
      if AIndex > L then
        Break;
      if S[AIndex] = ',' then
        Inc(AIndex);
    until AIndex > L;
    Result := True;
  end;

  function CalculateItemsByStatesIntegerValue: Boolean;
  var
    V: Int64;
    I, ACode: Integer;
  begin
    Result := VarIsNumericEx(AValue) or VarIsStr(AValue) or VarIsDate(AValue) or
      VarIsNull(AValue);
    if Result then
    begin
      if VarIsNull(AValue) then
        V := 0
      else
        if VarIsStr(AValue) then
        begin
          Val(AValue, V, ACode);
          Result := ACode = 0;
          if not Result then
            Exit;
        end
        else
          V := VarAsType(AValue, varInt64);
      for I := 0 to AItems.Count - 1 do
      begin
        if V and 1 = 0 then
          ACheckStates[I] := cbsUnchecked
        else
          ACheckStates[I] := cbsChecked;
        V := V shr 1;
      end;
    end
  end;

  function CalculateItemsByStatesStringValue: Boolean;
  var
    AItemCount, I: Integer;
    S: string;
  begin
    Result := VarIsNull(AValue) or VarIsStr(AValue);
    if not Result then
      Exit;

    Result := False;
    S := VarToStr(AValue);
    AItemCount := Length(S);
    if AItemCount > AItems.Count then
      AItemCount := AItems.Count;
    for I := 1 to AItemCount do
      if (S[I] < '0') or (S[I] > '2') then
        Exit
      else
        ACheckStates[I - 1] := TcxCheckBoxState(Ord(S[I]) - Ord('0'));
    if AItemCount < AItems.Count then
      for I := AItemCount to AItems.Count - 1 do
        ACheckStates[I] := cbsUnchecked;
    Result := True;
  end;

var
  I: Integer;
begin
  SetLength(ACheckStates, AItems.Count);

  case AValueFormat of
    cvfCaptions:
      Result := CalculateItemsByCaptionsValue;
    cvfIndices:
      Result := CalculateItemsByIndicesValue;
    cvfInteger:
      Result := CalculateItemsByStatesIntegerValue;
    cvfStatesString:
      Result := CalculateItemsByStatesStringValue;
    else
      Result := False;
  end;

  if not Result then
    for I := 0 to AItems.Count - 1 do
      ACheckStates[I] := cbsUnchecked;
end;

procedure DrawCheckBoxText(ACanvas: TcxCanvas; AText: string; AFont: TFont;
  ATextColor: TColor; ATextRect: TRect; ADrawTextFlags: Integer; AEnabled: Boolean);
begin
  ACanvas.Font := AFont;
  ACanvas.Font.Color := ATextColor;
  ACanvas.Brush.Style := bsClear;
  ACanvas.DrawText(AText, ATextRect, ADrawTextFlags, AEnabled);
  ACanvas.Brush.Style := bsSolid;
end;

function IsGlyphValid(AGlyph: TGraphic; AGlyphCount: Integer): Boolean;
begin
  Result := (AGlyphCount > 0) and IsGlyphAssigned(AGlyph);
end;

function GetCheckNativeState(AState: TcxCheckBoxState; ACheckState: TcxEditCheckState): Integer;
const
  ANativeCheckStateMap: array[TcxCheckBoxState, TcxEditCheckState] of Integer = (
    (CBS_UNCHECKEDNORMAL, CBS_UNCHECKEDHOT, CBS_UNCHECKEDPRESSED, CBS_UNCHECKEDDISABLED),
    (CBS_CHECKEDNORMAL, CBS_CHECKEDHOT, CBS_CHECKEDPRESSED, CBS_CHECKEDDISABLED),
    (CBS_MIXEDNORMAL, CBS_MIXEDHOT, CBS_MIXEDPRESSED, CBS_MIXEDDISABLED)
  );
begin
  Result := ANativeCheckStateMap[AState, ACheckState];
end;

procedure DrawCustomCheckBox(ACanvas: TcxCanvas; AViewInfo: TcxCustomCheckBoxViewInfo; AScaleFactor: TdxScaleFactor); overload;

  function IsCheckTransparent: Boolean;
  begin
    Result := AViewInfo.IsBackgroundTransparent and (AViewInfo.UseSkins or
      AViewInfo.HasGlyph or AViewInfo.NativeStyle and IsThemeBackgroundPartiallyTransparent(
        OpenTheme(totButton), BP_CHECKBOX, GetCheckNativeState(AViewInfo.State, AViewInfo.CheckBoxState)));
  end;

var
  ACheckRect: TRect;
begin
  ACheckRect := AViewInfo.CheckBoxRect;
  InflateRect(ACheckRect, -AViewInfo.CheckBorderOffset, -AViewInfo.CheckBorderOffset);
  AViewInfo.DrawEditBackground(ACanvas, AViewInfo.Bounds, cxNullRect, IsCheckTransparent);
  AViewInfo.DrawCustomEditValidationMark(ACanvas);

  if AViewInfo.CanDrawEditValue then
  begin
    if not AViewInfo.FadingHelper.DrawImage(ACanvas.Handle, ACheckRect) then
      DrawScaledEditCheck(ACanvas, AViewInfo.CheckBoxRect, AViewInfo.CheckBoxState, AViewInfo, AScaleFactor);

    if AViewInfo.Alignment <> taCenter then
      DrawCheckBoxText(ACanvas, AViewInfo.Text, AViewInfo.Font, AViewInfo.TextColor,
        AViewInfo.TextRect, AViewInfo.DrawTextFlags, AViewInfo.IsTextEnabled);
  end;

  if not IsRectEmpty(AViewInfo.FocusRect) then
    ACanvas.DrawFocusRect(AViewInfo.FocusRect);
end;

procedure DrawScaledEditCheck(ACanvas: TcxCanvas; const ACheckRect: TRect;
  ACheckState: TcxEditCheckState; AViewInfo: TcxCustomCheckBoxViewInfo; AScaleFactor: TdxScaleFactor);

  function GetPainter: TcxCustomLookAndFeelPainter;
  begin
    if AViewInfo.UseSkins then
      Result := AViewInfo.Painter
    else
      Result := nil;
  end;

begin
  DrawScaledEditCheck(ACanvas, ACheckRect, AViewInfo.State, ACheckState,
    AViewInfo.CheckBoxGlyph, AViewInfo.CheckBoxGlyphCount,
    AViewInfo.CheckBoxBorderStyle, AViewInfo.NativeStyle,
    AViewInfo.BorderColor, AViewInfo.BackgroundColor, not AViewInfo.IsBackgroundTransparent,
    AViewInfo.IsDesigning, AViewInfo.Focused, True, GetPainter, AScaleFactor, AViewInfo.NullValueShowingStyle);
end;

procedure DrawEditCheck(ACanvas: TcxCanvas; const ACheckRect: TRect; ACheckState: TcxEditCheckState;
  AViewInfo: TcxCustomCheckBoxViewInfo);
begin
  DrawScaledEditCheck(ACanvas, ACheckRect, ACheckState, AViewInfo, dxSystemScaleFactor);
end;

procedure DrawScaledEditCheck(ACanvas: TcxCanvas; const ACheckRect: TRect;
  AState: TcxCheckBoxState; ACheckState: TcxEditCheckState; AGlyph: TdxSmartGlyph;
  AGlyphCount: Integer; ABorderStyle: TcxEditCheckBoxBorderStyle;
  ANativeStyle: Boolean; ABorderColor: TColor; ABackgroundColor: TColor;
  ADrawBackground, AIsDesigning, AFocused, ASupportGrayed: Boolean;
  APainter: TcxCustomLookAndFeelPainter; AScaleFactor: TdxScaleFactor;
  AGrayedShowingStyle: TcxCheckBoxNullValueShowingStyle = nssGrayedChecked);

  procedure DrawCheckBoxGlyph(AScaleFactor: TdxScaleFactor);
  var
    ABitmap: TcxBitmap;
    AGlyphIndex: Integer;
    AGlyphWidth: Integer;
  begin
    AGlyphIndex := GetEditCheckGlyphIndex(AState, ACheckState, ASupportGrayed, AGlyphCount);
    AGlyphWidth := AGlyph.SourceWidth div AGlyphCount;

    ABitmap := TcxBitmap.Create;
    try
      if AGlyph.IsAlphaUsed then
        ABitmap.PixelFormat := pf32bit;

      ABitmap.Transparent := True;
      if Supports(AGlyph, IdxVectorImage) then
      begin
        ABitmap.SetSize(AScaleFactor.Apply(AGlyphWidth), AScaleFactor.Apply(AGlyph.SourceHeight));
        AScaleFactor := dxDefaultScaleFactor;
      end
      else
        ABitmap.SetSize(AGlyphWidth, AGlyph.SourceHeight);

      cxClearBitmap(ABitmap);
      AGlyph.StretchDraw(ABitmap.Canvas.Handle, ABitmap.ClientRect,
        cxRectBounds((AGlyph.Width div AGlyphCount) * AGlyphIndex, 0, (AGlyph.Width div AGlyphCount), AGlyph.Height));

      if ADrawBackground then
        ACanvas.FillRect(ACheckRect, ABackgroundColor);
      cxDrawImage(ACanvas, ACheckRect, ABitmap, nil, -1, ifmStretch,
        EnabledImageDrawModeMap[ACheckState <> ecsDisabled], True, nil, AScaleFactor);
    finally
      ABitmap.Free;
    end;
  end;

  procedure DrawCheckBoxBorder;
  var
    ACheckBorderOffset: Integer;
    R: TRect;
  begin
    if ANativeStyle then
    begin
      cxLookAndFeelPaintersManager.GetPainter(lfsNative).DrawScaledCheck(
        ACanvas, ACheckRect, CheckStateToButtonState[ACheckState], AState, clDefault, AScaleFactor);
      Exit;
    end;

    R := ACheckRect;
    ACheckBorderOffset := GetEditCheckBorderOffset(ABorderStyle, False, False, APainter);

    if ADrawBackground and (ACheckBorderOffset > 0) then
      ACanvas.FrameRect(R, ABackgroundColor, ACheckBorderOffset);
    InflateRect(R, -ACheckBorderOffset, -ACheckBorderOffset);
    with ACanvas do
    begin
      case ABorderStyle of
        ebsSingle:
          FrameRect(R, ABorderColor);
        ebsThick:
          FrameRect(R, ABorderColor, 2);
        ebsFlat:
          begin
            DrawEdge(R, True, True, cxBordersAll);
            InflateRect(R, -1, -1);
            FrameRect(R, clBtnFace);
          end;
        ebs3D:
          begin
            DrawEdge(R, True, True, cxBordersAll);
            InflateRect(R, -1, -1);
            DrawComplexFrame(R, cl3DDkShadow, cl3DLight, cxBordersAll);
          end;
        ebsUltraFlat, ebsOffice11:
          begin
            if (ABorderStyle = ebsOffice11) and (ACheckState = ecsNormal) and
              not AIsDesigning and not AFocused then
                ABorderColor := clBtnText
            else
              if (ACheckState in [ecsHot, ecsPressed]) or
                AIsDesigning and (ACheckState <> ecsDisabled) or
                (ACheckState = ecsNormal) and AFocused then
                  ABorderColor := GetEditBorderHighlightColor(
                    ABorderStyle = ebsOffice11)
              else
                ABorderColor := clBtnShadow;
            FrameRect(R, ABorderColor);
          end;
      end;
    end;
  end;

  procedure DrawCheck(R: TRect; AColor: TColor);
  begin
    ACanvas.SaveClipRegion;
    try
      ACanvas.IntersectClipRect(R);
      cxLookAndFeelPaintersManager.GetPainter(lfsUltraFlat).DrawScaledCheck(
        ACanvas, R, CheckStateToButtonState[ACheckState], AState, AColor, AScaleFactor);
    finally
      ACanvas.RestoreClipRegion;
    end;
  end;

  function GetCheckBoxContentColor: TColor;
  const
    AColors: array[TcxEditCheckState] of TColor = (clWindow, clWindow, clBtnFace, clBtnFace);
  begin
    if ABorderStyle in [ebsUltraFlat, ebsOffice11] then
      case ACheckState of
        ecsHot, ecsPressed:
          Result := GetEditButtonHighlightColor(ACheckState = ecsPressed, ABorderStyle = ebsOffice11);
        ecsNormal:
          if (AState = cbsGrayed) and (AGrayedShowingStyle = nssInactive) then
            Result := clBtnFace
          else
            Result := clWindow;
        else
          Result := clBtnFace;
      end
    else
      if (AState = cbsGrayed) and (AGrayedShowingStyle = nssInactive) then
        Result := clBtnFace
      else
        Result := AColors[ACheckState];
  end;

  procedure InternalDrawCheckBoxContent(AContentRect: TRect);
  var
    ACheckColor: TColor;
  begin
    ACanvas.FillRect(AContentRect, GetCheckBoxContentColor);
    ACanvas.Brush.Style := bsClear;
    if (AState = cbsUnchecked) or
      (AState = cbsGrayed) and (AGrayedShowingStyle <> nssGrayedChecked) then
        Exit;
    if (ACheckState = ecsDisabled) or (AState = cbsGrayed) then
      ACheckColor := clBtnShadow
    else
      ACheckColor := clBtnText;
    DrawCheck(AContentRect, ACheckColor);
  end;

  procedure DrawWindowsCheckBoxContent(AContentRect: TRect);
  const
    ABorder3DStyleMap: array [Boolean] of Integer = (DFCS_FLAT, 0);
    AGrayedShowingStyleMap: array [TcxCheckBoxNullValueShowingStyle] of Integer =
      (0, DFCS_INACTIVE, DFCS_CHECKED);
  var
    AClipRgnExists: Boolean;
    AFlags: Integer;
    APrevClipRgn: HRGN;
  begin
    if ACheckState = ecsDisabled then
    begin
      AFlags := DFCS_BUTTON3STATE or DFCS_PUSHED;
      if (AState = cbsUnchecked) or ((AState = cbsGrayed) and (AGrayedShowingStyle <> nssGrayedChecked)) then
        AFlags := AFlags or DFCS_INACTIVE
      else
        AFlags := AFlags or DFCS_CHECKED;
    end
    else
    begin
      AFlags := 0;
      case AState of
        cbsGrayed:
          AFlags := DFCS_BUTTON3STATE or
            AGrayedShowingStyleMap[AGrayedShowingStyle];
        cbsChecked:
          AFlags := DFCS_CHECKED;
      end;
      if ACheckState = ecsPressed then
        AFlags := AFlags or DFCS_PUSHED;
    end;

    APrevClipRgn := CreateRectRgn(0, 0, 0, 0);
    AClipRgnExists := GetClipRgn(ACanvas.Handle, APrevClipRgn) = 1;
    with AContentRect do
      IntersectClipRect(ACanvas.Handle, Left, Top, Right, Bottom);
    InflateRect(AContentRect, cxEditMaxCheckBoxBorderWidth, cxEditMaxCheckBoxBorderWidth);

    DrawFrameControl(ACanvas.Handle, AContentRect, DFC_BUTTON, DFCS_BUTTONCHECK or AFlags or ABorder3DStyleMap[ABorderStyle = ebs3D]);

    if AClipRgnExists then
      SelectClipRgn(ACanvas.Handle, APrevClipRgn)
    else
      SelectClipRgn(ACanvas.Handle, 0);
    DeleteObject(APrevClipRgn);
  end;

  procedure DrawCheckBoxContent;
  var
    ACheckBoxBorderWidth: Integer;
    R: TRect;
  begin
    if ANativeStyle then
      Exit;

    ACheckBoxBorderWidth := cxEditMaxCheckBoxBorderWidth;
    R := ACheckRect;
    InflateRect(R, -ACheckBoxBorderWidth, -ACheckBoxBorderWidth);

    if ABorderStyle in [ebsUltraFlat, ebsOffice11] then
      InternalDrawCheckBoxContent(R)
    else
      DrawWindowsCheckBoxContent(R);
  end;

begin
  if cxRectIsEmpty(ACheckRect) then
    Exit;
  if (AState = cbsGrayed) and (AGrayedShowingStyle = nssUnchecked) then
    AState := cbsUnchecked;

  if IsGlyphValid(AGlyph, AGlyphCount) then
    DrawCheckBoxGlyph(AScaleFactor)
  else
  begin
    if APainter <> nil then
    begin
      if ADrawBackground then
        ACanvas.FillRect(ACheckRect, ABackgroundColor);
      APainter.DrawScaledCheckButton(ACanvas, ACheckRect, CheckStateToButtonState[ACheckState], AState, AScaleFactor);
    end
    else
    begin
      DrawCheckBoxBorder;
      DrawCheckBoxContent;
    end;
  end;
end;

function GetCheckBoxStateCaptionByGlyphIndex(AIndex: Integer): string;
begin
  case AIndex of
    0: Result := 'Unchecked';
    1: Result := 'Checked';
    2: Result := 'Grayed';
    3: Result := 'Unchecked Pressed';
    4: Result := 'Checked Pressed';
    5: Result := 'Grayed Pressed';
  else
    Result := 'Not Used';
  end;
end;

procedure DrawEditCheck(ACanvas: TcxCanvas; const ACheckRect: TRect;
  AState: TcxCheckBoxState; ACheckState: TcxEditCheckState; AGlyph: TdxSmartGlyph;
  AGlyphCount: Integer; ABorderStyle: TcxEditCheckBoxBorderStyle;
  ANativeStyle: Boolean; ABorderColor: TColor; ABackgroundColor: TColor;
  ADrawBackground, AIsDesigning, AFocused, ASupportGrayed: Boolean;
  APainter: TcxCustomLookAndFeelPainter;
  AGrayedShowingStyle: TcxCheckBoxNullValueShowingStyle = nssGrayedChecked);
begin
  DrawScaledEditCheck(ACanvas, ACheckRect, AState, ACheckState, AGlyph, AGlyphCount, ABorderStyle,
    ANativeStyle, ABorderColor, ABackgroundColor, ADrawBackground, AIsDesigning, AFocused, ASupportGrayed,
    APainter, dxSystemScaleFactor, AGrayedShowingStyle);
end;

function GetEditCheckBorderOffset(ACheckBorderStyle: TcxContainerBorderStyle;
  ANativeStyle, AHasGlyph: Boolean; APainter: TcxCustomLookAndFeelPainter): Integer;
begin
  if ANativeStyle or AHasGlyph or (APainter <> nil) then
    Result := 0
  else
    Result := cxContainerMaxBorderWidth - GetContainerBorderWidth(ACheckBorderStyle);
end;

function GetEditCheckBorderOffset(ACheckBorderStyle: TcxEditBorderStyle;
  ANativeStyle, AHasGlyph: Boolean; APainter: TcxCustomLookAndFeelPainter): Integer; overload;
begin
  Result := GetEditCheckBorderOffset(
    TcxContainerBorderStyle(ACheckBorderStyle), ANativeStyle, AHasGlyph, APainter);
end;

function GetEditCheckBorderOffset(ALookAndFeelKind: TcxLookAndFeelKind;
  ANativeStyle, AHasGlyph: Boolean; APainter: TcxCustomLookAndFeelPainter): Integer;
begin
  if ANativeStyle or AHasGlyph or (APainter <> nil) then
    Result := 0
  else
    Result := cxContainerMaxBorderWidth - GetContainerBorderWidth(ALookAndFeelKind);
end;

function GetEditCheckGlyphIndex(AState: TcxCheckBoxState;
  ACheckState: TcxEditCheckState; ASupportGrayed: Boolean;
  AGlyphCount: Integer): Integer;
var
  AStateCount: Integer;
begin
  if AGlyphCount = 1 then
    Result := 0
  else
  begin
    AStateCount := Integer(High(TcxCheckBoxState)) -
      Integer(Low(TcxCheckBoxState)) + 1;
    if not ASupportGrayed and (AGlyphCount mod 3 <> 0) and (AGlyphCount mod 2 = 0) then
      Dec(AStateCount);
    case AState of
      cbsUnchecked:
        Result := 0;
      cbsChecked:
        Result := 1;
      else
        Result := 2;
    end;
    if ACheckState = ecsPressed then
      Inc(Result, AStateCount);
    if (Result >= AGlyphCount) and (Result > AStateCount - 1) then
      Result := Result mod AStateCount;
  end;
end;

function GetScaledEditCheckSize(ACanvas: TcxCanvas; ANativeStyle: Boolean;
  AGlyph: TdxSmartGlyph; AGlyphCount: Integer; APainter: TcxCustomLookAndFeelPainter; AScaleFactor: TdxScaleFactor): TSize;
var
  AIntf: IdxSourceSize;
begin
  if IsGlyphValid(AGlyph, AGlyphCount) then
  begin
    if Supports(AGlyph, IdxSourceSize, AIntf) then
      Result := AIntf.GetSourceSize
    else
      Result := AGlyph.Size;

    Result.cx := Result.cx div AGlyphCount;
    Result := AScaleFactor.Apply(Result, dxGetImageSourceDPI(AGlyph), dxDefaultDPI);
  end
  else
  begin
    if APainter = nil then
    begin
      if AreVisualStylesMustBeUsed(ANativeStyle, totButton) then
        APainter := cxLookAndFeelPaintersManager.GetPainter(lfsNative)
      else
        APainter := cxLookAndFeelPaintersManager.GetPainter(lfsStandard);
    end;
    Result := APainter.ScaledCheckButtonSize(AScaleFactor);
  end;
end;

function GetEditCheckSize(ACanvas: TcxCanvas; ANativeStyle: Boolean;
  AGlyph: TdxSmartGlyph; AGlyphCount: Integer; APainter: TcxCustomLookAndFeelPainter): TSize;
begin
  Result := GetScaledEditCheckSize(ACanvas, ANativeStyle, AGlyph, AGlyphCount, APainter, dxSystemScaleFactor);
end;

{ TcxCaptionItem }

procedure TcxCaptionItem.Assign(Source: TPersistent);
begin
  if Source is TcxCaptionItem then
  begin
    Caption := TcxCaptionItem(Source).Caption;
    Tag := TcxCaptionItem(Source).Tag;
  end
  else
    inherited;
end;

function TcxCaptionItem.IsTagStored: Boolean;
begin
  Result := FTag <> 0;
end;

procedure TcxCaptionItem.SetCaption(AValue: TCaption);
begin
  if Caption <> AValue then
  begin
    FCaption := AValue;
    if (Collection <> nil) and TcxCaptionItems(Collection).Sorted then
      TcxCaptionItems(Collection).Sort
    else
      Changed(False);
  end;
end;

{ TcxCaptionItems }

function TcxCaptionItems.DoCompareItems(AItem1, AItem2: TcxInterfacedCollectionItem): Integer;
begin
  Result := AnsiCompareText(TcxCaptionItem(AItem1).Caption, TcxCaptionItem(AItem2).Caption);
end;

function TcxCaptionItems.GetCaption(AIndex: Integer): string;
begin
  Result := Items[AIndex].Caption;
end;

function TcxCaptionItems.GetCount: Integer;
begin
  Result := Count;
end;

function TcxCaptionItems.GetItem(AIndex: Integer): TcxCaptionItem;
begin
  Result := TcxCaptionItem(inherited Items[AIndex]);
end;

procedure TcxCaptionItems.SetItem(AIndex: Integer; AValue: TcxCaptionItem);
begin
  inherited Items[AIndex] := AValue;
end;

{ TcxCustomCheckBoxViewInfo }

constructor TcxCustomCheckBoxViewInfo.Create;
begin
  inherited Create;
  FadingHelper := GetCheckControlFadingHelperClass.Create(Self);
end;

destructor TcxCustomCheckBoxViewInfo.Destroy;
begin
  FreeAndNil(FadingHelper);
  inherited Destroy;
end;

procedure TcxCustomCheckBoxViewInfo.Assign(Source: TObject);
begin
  if Source is TcxCustomCheckBoxViewInfo then
    with Source as TcxCustomCheckBoxViewInfo do
    begin
      Self.CheckBoxState := CheckBoxState;
      Self.State := State;
    end;
  inherited Assign(Source);
end;

procedure TcxCustomCheckBoxViewInfo.DrawText(ACanvas: TcxCanvas);
begin
  DrawCheckBoxText(ACanvas, Text, Font, TextColor, TextRect, DrawTextFlags, IsTextEnabled);
end;

function TcxCustomCheckBoxViewInfo.GetUpdateRegion(AViewInfo: TcxContainerViewInfo): TcxRegion;
var
  AEquals: Boolean;
  ATempRgn: TcxRegion;
begin
  Result := inherited GetUpdateRegion(AViewInfo);
  if not(AViewInfo is TcxCustomCheckBoxViewInfo) then
    Exit;
  with TcxCustomCheckBoxViewInfo(AViewInfo) do
    AEquals := (Self.CheckBoxState = CheckBoxState) and (Self.State = State);
  if not AEquals then
  begin
    ATempRgn := TcxRegion.Create(CheckBoxRect);
    try
      UniteRegions(Result, ATempRgn);
    finally
      ATempRgn.Free;
    end;
  end;
end;

function TcxCustomCheckBoxViewInfo.IsHotTrack: Boolean;
begin
  Result := True;
end;

function TcxCustomCheckBoxViewInfo.IsHotTrack(P: TPoint): Boolean;
begin
  Result := IsHotTrack;
end;

function TcxCustomCheckBoxViewInfo.NeedShowHint(ACanvas: TcxCanvas;
  const P: TPoint; const AVisibleBounds: TRect; out AText: TCaption;
  out AIsMultiLine: Boolean; out ATextRect: TRect; AMaxLineCount: Integer = 0): Boolean;
begin
  Result := False;
end;

procedure TcxCustomCheckBoxViewInfo.Offset(DX, DY: Integer);
begin
  inherited Offset(DX, DY);
  OffsetRect(CheckBoxRect, DX, DY);
  OffsetRect(FocusRect, DX, DY);
end;

function TcxCustomCheckBoxViewInfo.Repaint(AControl: TWinControl;
  const AInnerEditRect: TRect; AViewInfo: TcxContainerViewInfo = nil): Boolean;
var
  ACheckBoxViewInfo: TcxCustomCheckBoxViewInfo;
begin
  Result := AControl.HandleAllocated;
  if not Result then
    Exit;

  Result := inherited Repaint(AControl, AInnerEditRect, AViewInfo);
  ACheckBoxViewInfo := AViewInfo as TcxCustomCheckBoxViewInfo;
  Result := Result or (ACheckBoxViewInfo <> nil) and ((CheckBoxState <> ACheckBoxViewInfo.CheckBoxState) or (State <> ACheckBoxViewInfo.State));
  if (ACheckBoxViewInfo = nil) or (CheckBoxState <> ACheckBoxViewInfo.CheckBoxState) or (State <> ACheckBoxViewInfo.State) then
    cxRedrawWindow(AControl.Handle, cxRectOffset(CheckBoxRect, Left, Top), False);
end;

function TcxCustomCheckBoxViewInfo.IsTextEnabled: Boolean;
begin
  Result := IsContainerInnerControl or Enabled or IsTextColorAssigned or NativeStyle;
end;

procedure TcxCustomCheckBoxViewInfo.InternalPaint(ACanvas: TcxCanvas);
begin
  DrawCustomCheckBox(ACanvas, Self, ScaleFactor);
end;

procedure TcxCustomCheckBoxViewInfo.StoreLastState;
begin
  inherited StoreLastState;
  CheckBoxLastState := CheckBoxState;
end;

function TcxCustomCheckBoxViewInfo.GetCheckControlFadingHelperClass: TcxCheckControlFadingHelperClass;
begin
  Result := TcxCheckBoxFadingHelper;
end;

function TcxCustomCheckBoxViewInfo.GetEdit: TcxCustomCheckBox;
begin
  Result := TcxCustomCheckBox(FEdit);
end;

{ TcxCustomCheckBoxViewData }

procedure TcxCustomCheckBoxViewData.Calculate(ACanvas: TcxCanvas; const ABounds: TRect;
  const P: TPoint; Button: TcxMouseButton; Shift: TShiftState; AViewInfo: TcxCustomEditViewInfo;
  AIsMouseEvent: Boolean);

  function GetPainter: TcxCustomLookAndFeelPainter;
  begin
    if AViewInfo.UseSkins then
      Result := AViewInfo.Painter
    else
      Result := nil;
  end;

  function GetCheckBoxBorderStyle(AEditHotState: TcxContainerHotState): TcxEditBorderStyle;
  const
    ABorderStyles: array[TcxContainerHotState, TcxEditBorderStyle] of TcxEditBorderStyle = (
      (ebsNone, ebsSingle, ebsThick, ebsFlat, ebs3D, ebsUltraFlat, ebsOffice11),
      (ebsNone, ebsSingle, ebsThick, ebsFlat, ebs3D, ebsUltraFlat, ebsOffice11),
      (ebsFlat, ebsThick, ebsThick, ebs3D, ebs3D, ebsUltraFlat, ebsOffice11)
    );
  begin
    Result := ABorderStyles[AEditHotState, Style.BorderStyle];
  end;

var
  ACheckBoxViewInfo: TcxCustomCheckBoxViewInfo;
begin
  inherited Calculate(ACanvas, ABounds, P, Button, Shift, AViewInfo, AIsMouseEvent);

  ACheckBoxViewInfo := TcxCustomCheckBoxViewInfo(AViewInfo);
  ACheckBoxViewInfo.IsEditClass := GetIsEditClass;
  ACheckBoxViewInfo.DrawSelectionBar := False;
  ACheckBoxViewInfo.HasPopupWindow := False;
  ACheckBoxViewInfo.DrawTextFlags := GetDrawTextFlags;
  CalculateCheckBoxViewInfo(Self, ACheckBoxViewInfo, AIsMouseEvent);

  if Edit <> nil then
    ACheckBoxViewInfo.Text := TcxCustomCheckBox(Edit).Caption;

  if IsInplace and not (Properties.IsEmbeddedEdit or
    Properties.UseAlignmentWhenInplace or (ACheckBoxViewInfo.Text <> '')) then
      ACheckBoxViewInfo.Alignment := taCenter
  else
  begin
    ACheckBoxViewInfo.Alignment := Properties.Alignment;
    if UseRightToLeftAlignment then
      ChangeBiDiModeAlignment(ACheckBoxViewInfo.Alignment);
  end;

  ACheckBoxViewInfo.CheckBoxBorderStyle := GetCheckBoxBorderStyle(AViewInfo.HotState);
  ACheckBoxViewInfo.CheckBoxGlyph := Properties.Glyph;
  ACheckBoxViewInfo.CheckBoxGlyphCount := Properties.GlyphCount;
  ACheckBoxViewInfo.NullValueShowingStyle := Properties.NullStyle;
  ACheckBoxViewInfo.HasGlyph := IsGlyphValid(Properties.Glyph, Properties.GlyphCount);
  ACheckBoxViewInfo.CheckBorderOffset :=GetEditCheckBorderOffset(
    ACheckBoxViewInfo.CheckBoxBorderStyle, NativeStyle, ACheckBoxViewInfo.HasGlyph, GetPainter);
  ACheckBoxViewInfo.IsTextColorAssigned := Style.IsValueAssigned(svTextColor) or
    (ACheckBoxViewInfo.UseSkins and (ACheckBoxViewInfo.Painter.DefaultEditorTextColor(True) <> clDefault));

  ACheckBoxViewInfo.CheckBoxSize := GetScaledEditCheckSize(ACanvas, NativeStyle, Properties.Glyph,
    Properties.GlyphCount, AViewInfo.Painter, ScaleFactor);
  ACheckBoxViewInfo.CheckBoxRect := CalculateCheckBoxRect(ACheckBoxViewInfo);
  ACheckBoxViewInfo.TextRect := CalculateTextRect(ACheckBoxViewInfo);
  ACheckBoxViewInfo.CheckBoxState := CalculateCheckBoxState(ACheckBoxViewInfo, P, Button, Shift);
  ACheckBoxViewInfo.BackgroundColor := Style.Color;

  CalculateFocusRect(ACheckBoxViewInfo);

  if IsInplace and (ACheckBoxViewInfo.CheckBoxBorderStyle = ebsSingle) then
  begin
    if (ACheckBoxViewInfo.CheckBoxState = ecsHot) or (IsDesigning and (ACheckBoxViewInfo.CheckBoxState <> ecsDisabled)) then
      ACheckBoxViewInfo.BorderColor := clHighlight
    else
      ACheckBoxViewInfo.BorderColor := clBtnShadow;
  end;
  TcxCheckBoxFadingHelper(ACheckBoxViewInfo.FadingHelper).UpdateState;
end;

procedure TcxCustomCheckBoxViewData.EditValueToDrawValue(
  const AEditValue: TcxEditValue; AViewInfo: TcxCustomEditViewInfo);
begin
  if PreviewMode then
    TcxCustomCheckBoxViewInfo(AViewInfo).State := cbsChecked
  else
    TcxCustomCheckBoxViewInfo(AViewInfo).State := Properties.GetState(AEditValue);
  if IsInplace and not Properties.IsEmbeddedEdit then
    TcxCustomCheckBoxViewInfo(AViewInfo).Text := '';
  CalculateFocusRect(TcxCustomCheckBoxViewInfo(AViewInfo));
end;

function TcxCustomCheckBoxViewData.GetBorderStyle: TcxEditBorderStyle;
begin
  Result := ebsNone;
end;

function TcxCustomCheckBoxViewData.GetContentOffset: Integer;
begin
  if IsInplace then
    Result := AInplaceContentOffset
  else
    Result := AUsualContentOffset;
  Result := ScaleFactor.Apply(Result);
end;

function TcxCustomCheckBoxViewData.InternalGetEditConstantPartSize(ACanvas: TcxCanvas;
  AIsInplace: Boolean; AEditSizeProperties: TcxEditSizeProperties;
  var MinContentSize: TSize; AViewInfo: TcxCustomEditViewInfo): TSize;

  function IsCaptionVisible: Boolean;
  begin
    Result := (TcxCustomCheckBoxViewInfo(AViewInfo).Alignment <> taCenter) and not IsInplace and (TcxCustomCheckBoxViewInfo(AViewInfo).Text <> '');
  end;

var
  ASize1: TSize;
  AOffset: Integer;
begin
  Result := inherited InternalGetEditConstantPartSize(ACanvas, AIsInplace,
    AEditSizeProperties, MinContentSize, AViewInfo);

  ASize1 := GetScaledEditCheckSize(ACanvas, NativeStyle, Properties.Glyph, Properties.GlyphCount, Style.LookAndFeel.Painter,
    ScaleFactor);
  AOffset := GetContentOffset;
  Result.cx := Result.cx + ASize1.cx + 2 * AOffset;
  MinContentSize.cy := ASize1.cy + 2 * AOffset;
end;

function TcxCustomCheckBoxViewData.GetCalculateTextFlags: Integer;
begin
  Result := GetTextFlags(False);
end;

function TcxCustomCheckBoxViewData.GetDrawTextFlags: Integer;
begin
  Result := GetTextFlags(True);
end;

function TcxCustomCheckBoxViewData.GetEditContentSize(ACanvas: TcxCanvas; const AEditValue: Variant;
  const AEditSizeProperties: TcxEditSizeProperties; AErrorData: TcxEditValidateInfo): TSize;

  function GetEditDisplayText: string;
  begin
    if Edit <> nil then
      Result := TcxCustomCheckBox(Edit).Caption
    else
      Result := '';
  end;

var
  ARect: TRect;
  AText: string;
begin
  ACanvas.SaveState;
  try
    AText := GetEditDisplayText;
    if AText <> '' then
    begin
      ARect := cxRectSetWidth(ARect, Max(AEditSizeProperties.Width - GetTextGap, 1));
      ACanvas.Font := Style.GetVisibleFont;
      cxDrawText(ACanvas.Handle, AText, ARect, DT_CALCRECT or cxFlagsToDTFlags(GetCalculateTextFlags or cxDontBreakChars));
      Result := cxRectSize(ARect);
      Result.cx := Result.cx + GetTextGap;
      Result.cy := Result.cy + 2 * GetContentOffset;
    end
    else
      Result := cxNullSize;
  finally
    ACanvas.RestoreState;
  end;
end;

function TcxCustomCheckBoxViewData.GetIsEditClass: Boolean;
begin
  Result := True;
end;

function TcxCustomCheckBoxViewData.GetTextGap: Integer;
begin
  Result := ScaleFactor.Apply(ACheckTextGap);
end;

function TcxCustomCheckBoxViewData.IsCheckPressed: Boolean;
begin
  Result := (Edit <> nil) and TcxCustomCheckBox(Edit).FIsCheckPressed;
end;

function TcxCustomCheckBoxViewData.CalculateCheckBoxRect(AViewInfo: TcxCustomCheckBoxViewInfo): TRect;
var
  AAvailableArea: TRect;
  AAlignment: TAlignment;
  AOffset: Integer;
  AClientRect: TRect;
  ACheckSize: TSize;
begin
  AClientRect := AViewInfo.ClientRect;
  ACheckSize := AViewInfo.CheckBoxSize;

  AAlignment := AViewInfo.Alignment;
  AAvailableArea := AClientRect;

  Result := cxRectCenter(AAvailableArea, Min(ACheckSize.cx, cxRectWidth(AAvailableArea)), Min(ACheckSize.cy, cxRectHeight(AAvailableArea)));
  if cxRectWidth(AAvailableArea) > ACheckSize.cx then
  begin
    if IsInplace and Properties.IsEmbeddedEdit then
      AOffset := 0
    else
      AOffset := GetContentOffset;

    case AAlignment of
      taLeftJustify:
        begin
          Result.Left := AAvailableArea.Left + AOffset;
          Result.Right := Result.Left + ACheckSize.cx;
        end;
      taRightJustify:
        begin
          Result.Right := AAvailableArea.Right - AOffset;
          Result.Left := Result.Right - ACheckSize.cx;
        end;
    end;
  end;
end;

function TcxCustomCheckBoxViewData.CalculateCheckBoxState(
  AViewInfo: TcxCustomCheckBoxViewInfo; const P: TPoint;
  AButton: TcxMouseButton; AShift: TShiftState): TcxEditCheckState;
begin
  if not Enabled then
    Result := ecsDisabled
  else
    if IsCheckPressed then
      Result := ecsPressed
    else
      if not IsDesigning and PtInRect(AViewInfo.BorderRect, P) then
      begin
        if ([ssLeft, ssRight, ssMiddle] * AShift) = [] then
          Result := ecsHot
        else
          if (ssLeft in AShift) and ((AButton = cxmbLeft) or (AViewInfo.CheckBoxState = ecsPressed)) then
            Result := ecsPressed
          else
            Result := ecsNormal
      end
      else
        Result := ecsNormal;
end;

procedure TcxCustomCheckBoxViewData.CalculateFocusRect(AViewInfo: TcxCustomCheckBoxViewInfo);

  procedure CheckFocusRectBounds;
  var
    AMaxRect: TRect;
  begin
    if AViewInfo.Alignment = taCenter then
      AMaxRect := Rect(AViewInfo.FocusRect.Left, AViewInfo.ClientRect.Top, AViewInfo.FocusRect.Right, AViewInfo.ClientRect.Bottom)
    else
    begin
      AMaxRect := Rect(AViewInfo.TextRect.Left - 1, AViewInfo.TextRect.Top - 1 + 2 * Integer(IsInplace),
        AViewInfo.TextRect.Right + 1, AViewInfo.TextRect.Bottom + 1 - 2 * Integer(IsInplace));
      AMaxRect.Right := Min(AMaxRect.Right, AViewInfo.BorderRect.Right - 1);
    end;

    AViewInfo.FocusRect.Left := Max(AViewInfo.FocusRect.Left, AMaxRect.Left);
    AViewInfo.FocusRect.Top := Max(AViewInfo.FocusRect.Top, AMaxRect.Top);
    AViewInfo.FocusRect.Right := Min(AViewInfo.FocusRect.Right, AMaxRect.Right);
    AViewInfo.FocusRect.Bottom := Min(AViewInfo.FocusRect.Bottom, AMaxRect.Bottom);
  end;

begin
  if
    (Focused and ((not IsInplace or Properties.IsEmbeddedEdit) and (AViewInfo.Alignment <> taCenter) or
    IsInplace and (AViewInfo.Alignment = taCenter) and (epoShowFocusRectWhenInplace in PaintOptions))) then
  begin
    if AViewInfo.Alignment = taCenter then
    begin
      AViewInfo.FocusRect := AViewInfo.ClientRect;
      InflateRect(AViewInfo.FocusRect, -1, -1);
    end
    else
      if Length(AViewInfo.Text) <> 0 then
      begin
        AViewInfo.FocusRect := AViewInfo.TextRect;
        if not Properties.FullFocusRect then
        begin
          cxScreenCanvas.Font := AViewInfo.Font;
          cxScreenCanvas.TextExtent(AViewInfo.Text, AViewInfo.FocusRect, AViewInfo.DrawTextFlags);
          cxScreenCanvas.Dormant;
          InflateRect(AViewInfo.FocusRect, 1, 1);
        end
        else
          InflateRect(AViewInfo.FocusRect, 1, 0);
      end
      else
        AViewInfo.FocusRect := cxEmptyRect;
  end
  else
    AViewInfo.FocusRect := cxEmptyRect;
  if not IsRectEmpty(AViewInfo.FocusRect) then
    CheckFocusRectBounds;
end;

function TcxCustomCheckBoxViewData.CalculateTextRect(AViewInfo: TcxCustomCheckBoxViewInfo): TRect;
var
  AAlignment: TAlignment;
  AOffset: Integer;
  AClientRect, ACheckBoxRect: TRect;
begin
  AClientRect := AViewInfo.ClientRect;
  ACheckBoxRect := AViewInfo.CheckBoxRect;
  AAlignment := AViewInfo.Alignment;
  Result := AClientRect;
  AOffset := GetContentOffset;
  InflateRect(Result, -AOffset, 0);
  if cxRectWidth(ACheckBoxRect) < cxRectWidth(AClientRect) then
    case AAlignment of
      taLeftJustify:
        Result.Left := ACheckBoxRect.Right + GetTextGap;
      taRightJustify:
        Result.Right := ACheckBoxRect.Left - GetTextGap;
    end
  else
    Result.Right := Result.Left;

  if not AViewInfo.IsTextEnabled and (Style.LookAndFeel.SkinPainter = nil) then
  begin
    Inc(Result.Right);
    Inc(Result.Bottom);
  end;
end;

function TcxCustomCheckBoxViewData.GetProperties: TcxCustomCheckBoxProperties;
begin
  Result := TcxCustomCheckBoxProperties(inherited Properties);
end;

function TcxCustomCheckBoxViewData.GetTextFlags(ADraw: Boolean): Integer;
const
  AHorzAlignmentFlags: array [TcxEditHorzAlignment] of Integer = (cxAlignLeft, cxAlignLeft, cxAlignHCenter);
begin
  Result := cxShowPrefix;
  if UseRightToLeftAlignment then
    Result := Result or
      TdxRightToLeftLayoutConverter.ConvertcxDrawTextAlignment(AHorzAlignmentFlags[Properties.InternalAlignment.Horz]);
  if ADraw then
    Result := Result or cxAlignVCenter;
  if Properties.MultiLine then
    Result := Result or cxDontClip or cxWordBreak
  else
  begin
    Result := Result or cxSingleLine;
    if Properties.ShowEndEllipsis then
      Result := Result or cxShowEndEllipsis;
  end;
  if UseRightToLeftReading then
    Result := Result or cxRtlReading;
end;

{ TcxCheckBoxStyle }

function TcxCheckBoxStyle.HasBorder: Boolean;
begin
  Result := False;
end;

{ TcxCustomCheckBoxProperties }

constructor TcxCustomCheckBoxProperties.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FGlyph := TdxSmartGlyph.Create;
  TdxSmartGlyphAccess(FGlyph).FTransparent := False;
  FGlyph.OnChange := ChangeHandler;
  FGlyphCount := 6;
  FNullStyle := nssGrayedChecked;
  FValueChecked := True;
  FValueGrayed := Null;
  FValueUnchecked := False;
end;

destructor TcxCustomCheckBoxProperties.Destroy;
begin
  FreeAndNil(FGlyph);
  inherited Destroy;
end;

function TcxCustomCheckBoxProperties.CanCompareEditValue: Boolean;
begin
  Result := True;
end;

function TcxCustomCheckBoxProperties.CompareDisplayValues(
  const AEditValue1, AEditValue2: TcxEditValue): Boolean;
begin
  Result := GetState(AEditValue1) = GetState(AEditValue2);
end;

class function TcxCustomCheckBoxProperties.GetContainerClass: TcxContainerClass;
begin
  Result := TcxCheckBox;
end;

function TcxCustomCheckBoxProperties.GetDisplayText(const AEditValue: TcxEditValue;
  AFullText: Boolean = False; AIsInplace: Boolean = True): string;
begin
  Result := GetDisplayCheckState(Ord(GetState(AEditValue)));
end;

class function TcxCustomCheckBoxProperties.GetStyleClass: TcxCustomEditStyleClass;
begin
  Result := TcxCheckBoxStyle;
end;

function TcxCustomCheckBoxProperties.GetSpecialFeatures: TcxEditSpecialFeatures;
begin
  Result := inherited GetSpecialFeatures + [esfNoContentPart, esfClickable];
end;

function TcxCustomCheckBoxProperties.GetSupportedOperations: TcxEditSupportedOperations;
begin
  Result := [esoAlwaysHotTrack, esoEditing, esoFiltering, esoHotTrack,
    esoShowingCaption, esoSorting, esoTransparency, esoAutoWidth];
end;

class function TcxCustomCheckBoxProperties.GetViewInfoClass: TcxContainerViewInfoClass;
begin
  Result := TcxCustomCheckBoxViewInfo;
end;

function TcxCustomCheckBoxProperties.IsActivationKey(AKey: Char): Boolean;
begin
  Result := AKey = ' ';
end;

function TcxCustomCheckBoxProperties.IsEditValueValid(var EditValue: TcxEditValue; AEditFocused: Boolean): Boolean;
begin
  Result := inherited IsEditValueValid(EditValue, AEditFocused);
  if Result then
    Result := not CheckValue(EditValue);
end;

function TcxCustomCheckBoxProperties.IsResetEditClass: Boolean;
begin
  Result := True;
end;

procedure TcxCustomCheckBoxProperties.PrepareDisplayValue(const AEditValue:
  TcxEditValue; var DisplayValue: TcxEditValue; AEditFocused: Boolean);
begin
  if VarEqualsExact(AEditValue, FValueChecked) then
    DisplayValue := cbsChecked
  else
    if VarEqualsExact(AEditValue, FValueUnchecked) then
      DisplayValue := cbsUnchecked
    else
      DisplayValue := cbsGrayed;
end;

function TcxCustomCheckBoxProperties.GetStateCaption(AIndex: Integer): string;
begin
  Result := GetCheckBoxStateCaptionByGlyphIndex(AIndex);
end;

function TcxCustomCheckBoxProperties.GetGlyphCount: Integer;
begin
  Result := FGlyphCount;
end;

function TcxCustomCheckBoxProperties.CanValidate: Boolean;
begin
  Result := True;
end;

procedure TcxCustomCheckBoxProperties.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('Caption', ReadCaption, nil, False);
end;

procedure TcxCustomCheckBoxProperties.DoAssign(AProperties: TcxCustomEditProperties);
begin
  inherited;
  if AProperties is TcxCustomCheckBoxProperties then
    with TcxCustomCheckBoxProperties(AProperties) do
    begin
      Self.AllowGrayed := AllowGrayed;
      Self.DisplayChecked := DisplayChecked;
      Self.DisplayGrayed := DisplayGrayed;
      Self.DisplayUnchecked := DisplayUnchecked;
      Self.FullFocusRect := FullFocusRect;
      Self.Glyph := Glyph;
      Self.GlyphCount := GlyphCount;
      Self.MultiLine := MultiLine;
      Self.NullStyle := NullStyle;
      Self.UseAlignmentWhenInplace := UseAlignmentWhenInplace;
      Self.SetStateValues(ValueChecked, ValueGrayed, ValueUnchecked);
      Self.ShowEndEllipsis := ShowEndEllipsis;
    end;
end;

class function TcxCustomCheckBoxProperties.GetViewDataClass: TcxCustomEditViewDataClass;
begin
  Result := TcxCustomCheckBoxViewData;
end;

function TcxCustomCheckBoxProperties.HasDisplayValue: Boolean;
begin
  Result := True;
end;

function TcxCustomCheckBoxProperties.CheckValue(const AValue: TcxEditValue): Boolean;
begin
  Result := not(InternalVarEqualsExact(AValue, FValueChecked) or InternalVarEqualsExact(AValue,
    FValueGrayed) or InternalVarEqualsExact(AValue, FValueUnchecked));
end;

function TcxCustomCheckBoxProperties.GetState(
  const AEditValue: TcxEditValue): TcxCheckBoxState;
var
  ADisplayValue: TcxEditValue;
begin
  PrepareDisplayValue(AEditValue, ADisplayValue, False);
  Result := TcxCheckBoxState(ADisplayValue);
end;

function TcxCustomCheckBoxProperties.InternalGetGlyph: TdxSmartGlyph;
begin
  Result := FGlyph;
end;

function TcxCustomCheckBoxProperties.IsEmbeddedEdit: Boolean;
begin
  Result := False;
end;

function TcxCustomCheckBoxProperties.GetAlignment: TAlignment;
begin
  Result := inherited Alignment.Horz;
end;

function TcxCustomCheckBoxProperties.GetDefaultDisplayCheckState(AState: TcxCheckBoxState): string;
begin
  case AState of
    cbsChecked:
      Result := cxGetResourceString(@cxSEditCheckBoxChecked);
    cbsUnchecked:
      Result := cxGetResourceString(@cxSEditCheckBoxUnchecked);
    cbsGrayed:
      Result := cxGetResourceString(@cxSEditCheckBoxGrayed);
  end;
end;

function TcxCustomCheckBoxProperties.GetDisplayCheckState(
  const Index: Integer): WideString;
var
  ACheckState: TcxCheckBoxState;
begin
  ACheckState := TcxCheckBoxState(Index);
  if FIsDisplayCheckStateAssigned[ACheckState] then
    Result := FDisplayCheckState[ACheckState]
  else
    Result := GetDefaultDisplayCheckState(ACheckState);
end;

function TcxCustomCheckBoxProperties.GetGlyph: TdxSmartGlyph;
begin
  Result := InternalGetGlyph;
end;

function TcxCustomCheckBoxProperties.GetInternalAlignment: TcxEditAlignment;
begin
  Result := inherited Alignment;
end;

function TcxCustomCheckBoxProperties.IsAlignmentStored: Boolean;
begin
  Result := inherited Alignment.IsHorzStored;
end;

function TcxCustomCheckBoxProperties.IsDisplayCheckStateStored(Index: Integer): Boolean;
begin
  Result := FIsDisplayCheckStateAssigned[TcxCheckBoxState(Index)];
end;

function TcxCustomCheckBoxProperties.IsLoading: Boolean;
begin
  Result := (GetOwnerComponent(Self) <> nil) and (csLoading in GetOwnerComponent(Self).ComponentState);
end;

function TcxCustomCheckBoxProperties.IsValueCheckedStored: Boolean;
begin
  Result := not InternalVarEqualsExact(FValueChecked, True);
end;

function TcxCustomCheckBoxProperties.IsValueGrayedStored: Boolean;
begin
  Result := not VarIsNull(FValueGrayed);
end;

function TcxCustomCheckBoxProperties.IsValueUncheckedStored: Boolean;
begin
  Result := not InternalVarEqualsExact(FValueUnchecked, False);
end;

// obsolete
procedure TcxCustomCheckBoxProperties.ReadCaption(Reader: TReader);
begin
  Caption := Reader.ReadString;
end;

procedure TcxCustomCheckBoxProperties.SetAlignment(Value: TAlignment);
begin
  inherited Alignment.Horz := Value;
end;

// obsolete
procedure TcxCustomCheckBoxProperties.SetCaption(const Value: TCaption);
begin
  FIsCaptionAssigned := True;
  if not InternalCompareString(Value, FCaption, True) then
  begin
    FCaption := Value;
    Changed;
  end;
end;

procedure TcxCustomCheckBoxProperties.SetDisplayCheckState(const Index: Integer;
  const Value: WideString);
var
  ACheckState: TcxCheckBoxState;
begin
  ACheckState := TcxCheckBoxState(Index);
  if not InternalCompareString(Value, FDisplayCheckState[ACheckState], True) then
  begin
    FDisplayCheckState[ACheckState] := Value;
    FIsDisplayCheckStateAssigned[ACheckState] := not InternalCompareString(Value,
      GetDefaultDisplayCheckState(ACheckState), True);
  end;
end;

procedure TcxCustomCheckBoxProperties.SetFullFocusRect(Value: Boolean);
begin
  if Value <> FFullFocusRect then
  begin
    FFullFocusRect := Value;
    Changed;
  end;
end;

procedure TcxCustomCheckBoxProperties.SetGlyph(Value: TdxSmartGlyph);
begin
  Glyph.Assign(Value);
  Changed;
end;

procedure TcxCustomCheckBoxProperties.SetGlyphCount(Value: Integer);
begin
  if FGlyphCount <> Value then
  begin
    FGlyphCount := Value;
    if FGlyph <> nil then
      Changed;
  end;
end;

procedure TcxCustomCheckBoxProperties.SetMultiLine(Value: Boolean);
begin
  if Value <> FMultiLine then
  begin
    FMultiLine := Value;
    Changed;
  end;
end;

procedure TcxCustomCheckBoxProperties.SetNullStyle(Value: TcxCheckBoxNullValueShowingStyle);
begin
  if Value <> FNullStyle then
  begin
    FNullStyle := Value;
    Changed;
  end;
end;

procedure TcxCustomCheckBoxProperties.SetShowEndEllipsis(Value: Boolean);
begin
  if FShowEndEllipsis <> Value then
  begin
    FShowEndEllipsis := Value;
    Changed;
  end;
end;

procedure TcxCustomCheckBoxProperties.SetStateValues(const AValueChecked, AValueGrayed, AValueUnchecked: TcxEditValue);
var
  AIsValuesValid: Boolean;
begin
  AIsValuesValid := not(InternalVarEqualsExact(AValueChecked, AValueGrayed) or
    InternalVarEqualsExact(AValueGrayed, AValueUnchecked) or
    InternalVarEqualsExact(AValueChecked, AValueUnchecked) or
    VarIsNull(AValueChecked) or VarIsNull(AValueUnchecked));
  if AIsValuesValid then
  begin
    FValueChecked := AValueChecked;
    FValueGrayed := AValueGrayed;
    FValueUnchecked := AValueUnchecked;
    Changed;
  end;
end;

procedure TcxCustomCheckBoxProperties.SetUseAlignmentWhenInplace(Value: Boolean);
begin
  if Value <> FUseAlignmentWhenInplace then
  begin
    FUseAlignmentWhenInplace := Value;
    Changed;
  end;
end;

procedure TcxCustomCheckBoxProperties.SetValueChecked(const Value: TcxEditValue);
begin
  if IsLoading or CheckValue(Value) and not VarIsNull(Value) then
  begin
    FValueChecked := Value;
    Changed;
  end;
end;

procedure TcxCustomCheckBoxProperties.SetValueGrayed(const Value: TcxEditValue);
begin
  if IsLoading or CheckValue(Value) then
  begin
    FValueGrayed := Value;
    Changed;
  end;
end;

procedure TcxCustomCheckBoxProperties.SetValueUnchecked(const Value: TcxEditValue);
begin
  if IsLoading or CheckValue(Value) and not VarIsNull(Value) then
  begin
    FValueUnchecked := Value;
    Changed;
  end;
end;

{ TcxCustomCheckBox }

procedure TcxCustomCheckBox.Clear;
begin
  Checked := False;
end;

class function TcxCustomCheckBox.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxCustomCheckBoxProperties;
end;

procedure TcxCustomCheckBox.PrepareEditValue(const ADisplayValue: TcxEditValue;
  out EditValue: TcxEditValue; AEditFocused: Boolean);
begin
  case TcxCheckBoxState(ADisplayValue) of
    cbsUnchecked:
      EditValue := ActiveProperties.FValueUnchecked;
    cbsChecked:
      EditValue := ActiveProperties.FValueChecked;
    cbsGrayed:
      EditValue := ActiveProperties.FValueGrayed;
  end;
end;

procedure TcxCustomCheckBox.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  inherited ActionChange(Sender, CheckDefaults);
  if (IsLoading or FIsLoaded) and FIsLoadingStateAssigned or not(Sender is TCustomAction) then
    Exit;
  if not CheckDefaults or not Checked then
    TcxCheckBoxActionLink(ActionLink).InternalSetChecked(TCustomAction(Sender).Checked);
end;

function TcxCustomCheckBox.GetDefaultTextColorByPainter: TColor;
var
  AIntf: IdxCustomSkinnedContainer;
begin
  Result := inherited GetDefaultTextColorByPainter;
  if Supports(Parent, IdxCustomSkinnedContainer, AIntf) then
    Result := cxGetActualColor(AIntf.GetDefaultTextColor(Enabled), Result);
end;

procedure TcxCustomCheckBox.AcceleratorClick;
begin
  Toggle;
end;

function TcxCustomCheckBox.CanAutoHeight: Boolean;
begin
  Result := True;
end;

function TcxCustomCheckBox.CanAutoWidth: Boolean;
begin
  Result := Properties.Alignment <> taCenter;
end;

function TcxCustomCheckBox.IsHeightDependOnWidth: Boolean;
begin
  Result := ActiveProperties.MultiLine;
end;

function TcxCustomCheckBox.CanHaveTransparentBorder: Boolean;
begin
  Result := not IsInplace and not ActiveProperties.IsEmbeddedEdit or inherited CanHaveTransparentBorder;
end;

function TcxCustomCheckBox.DefaultParentColor: Boolean;
begin
  Result := True;
end;

procedure TcxCustomCheckBox.DoEditKeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited DoEditKeyDown(Key, Shift);
  if Key = VK_SPACE then
  begin
    if ViewInfo.CheckBoxState in [ecsNormal, ecsHot] then
    begin
      FIsCheckPressed := True;
      ViewInfo.CheckBoxState := ecsPressed;
      ShortRefreshContainer(False);
      Key := 0;
    end;
  end;
end;

procedure TcxCustomCheckBox.DoEditKeyPress(var Key: Char);
begin
  inherited DoEditKeyPress(Key);
  if Key = #0 then
    Exit;

  if IsInplace and (Key = #32) and (ViewInfo.CheckBoxState in [ecsNormal, ecsHot]) then
  begin
    Toggle;
    Key := #0;
  end;
end;

procedure TcxCustomCheckBox.DoEditKeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited DoEditKeyUp(Key, Shift);
  if Key = 0 then
    Exit;

  case Key of
    VK_SPACE:
      begin
        if ViewInfo.CheckBoxState = ecsPressed then
        begin
          FIsCheckPressed := False;
          ViewInfo.CheckBoxState := ecsNormal;
          InvalidateCheckRect;
          Toggle;
        end;
      end;
  end;
end;

procedure TcxCustomCheckBox.DoExit;
begin
  FIsCheckPressed := False;
  inherited DoExit;
end;

procedure TcxCustomCheckBox.DoProcessEventsOnViewInfoChanging;
begin
  if (ViewInfo.CheckBoxLastState = ecsPressed) and (ViewInfo.CheckBoxState = ecsHot) and (IsInplace or Focused) then
    Toggle;
end;

procedure TcxCustomCheckBox.PopulateSizeProperties(var AEditSizeProperties: TcxEditSizeProperties);
begin
  AEditSizeProperties := cxDefaultEditSizeProperties;
  if not ActiveProperties.MultiLine then
    AEditSizeProperties.MaxLineCount := 1;
  AEditSizeProperties.Width := cxRectWidth(ViewInfo.TextRect);
end;

function TcxCustomCheckBox.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TcxCheckBoxActionLink;
end;

function TcxCustomCheckBox.GetEditStateColorKind: TcxEditStateColorKind;
begin
  Result := cxEditStateColorKindMap[Enabled];
end;

function TcxCustomCheckBox.GetDisplayValue: TcxEditValue;
begin
  Result := InternalEditValue;
end;

function TcxCustomCheckBox.GetShadowBounds: TRect;
begin
  if not IsInplace and not ActiveProperties.IsEmbeddedEdit and
    ViewInfo.NativeStyle and Style.TransparentBorder then
  begin
    Result := GetControlRect(Self);
    InflateRect(Result, -cxContainerMaxBorderWidth, -cxContainerMaxBorderWidth);
  end
  else
    Result := inherited GetShadowBounds;
end;

procedure TcxCustomCheckBox.Initialize;
begin
  inherited Initialize;
  ControlStyle := ControlStyle - [csDoubleClicks] + [csParentBackground];
  Width := 121;
  Height := 21;
  PrepareEditValue(cbsUnchecked, FEditValue, False);
end;

function TcxCustomCheckBox.InternalGetNotPublishedStyleValues: TcxEditStyleValues;
begin
  Result := inherited InternalGetNotPublishedStyleValues;
  Include(Result, svEdges);
end;

procedure TcxCustomCheckBox.InternalSetEditValue(const Value: TcxEditValue;
  AValidateEditValue: Boolean);
var
  APrevState: TcxCheckBoxState;
begin
  APrevState := State;
  inherited InternalSetEditValue(Value, AValidateEditValue);
  if APrevState <> State then
  begin
    ViewInfo.State := State;
    Click;
    DoChange;
  end;
  ShortRefreshContainer(False);
end;

function TcxCustomCheckBox.IsClickEnabledDuringLoading: Boolean;
begin
  Result := IsDBEdit;
end;

function TcxCustomCheckBox.IsNativeBackground: Boolean;
begin
  Result := IsNativeStyle and ParentBackground and not IsInplace and not Transparent;
end;

procedure TcxCustomCheckBox.Loaded;
begin
  FIsLoaded := True;
  LockChangeEvents(True);
  LockClick(True);
  try
    inherited Loaded;
    if FIsLoadingStateAssigned then
      State := FLoadingState
    else
      if not IsDBEdit then
        State := cbsUnchecked;
  finally
    LockClick(False);
    LockChangeEvents(False, False);
    FIsLoaded := False;
  end;
end;

procedure TcxCustomCheckBox.PropertiesChanged(Sender: TObject);
begin
  if ActiveProperties.FIsCaptionAssigned then
  begin
    Caption := ActiveProperties.Caption;
    ActiveProperties.FIsCaptionAssigned := False;
  end;

  if ViewInfo.State <> State then
  begin
    ViewInfo.State := State;
    InvalidateCheckRect;
    if not AreChangeEventsLocked then
    begin
      Click;
      DoChange;
    end;
  end;
  inherited PropertiesChanged(Sender);
end;

procedure TcxCustomCheckBox.TextChanged;
begin
  inherited TextChanged;
  ShortRefreshContainer(False);
end;

procedure TcxCustomCheckBox.SetInternalDisplayValue(Value: TcxEditValue);
begin
  InternalEditValue := Value;
end;

procedure TcxCustomCheckBox.InvalidateCheckRect;
begin
  InvalidateRect(ViewInfo.CheckBoxRect, False);
end;

procedure TcxCustomCheckBox.Toggle;
begin
  LockChangeEvents(True);
  try
    BeginUserAction;
    try
      begin
        case State of
          cbsUnchecked:
            if ActiveProperties.AllowGrayed then
              State := cbsGrayed
            else
              State := cbsChecked;
          cbsChecked:
            State := cbsUnchecked;
          cbsGrayed:
            State := cbsChecked;
        end;
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

function TcxCustomCheckBox.GetChecked: Boolean;
begin
  Result := State = cbsChecked;
end;

function TcxCustomCheckBox.GetProperties: TcxCustomCheckBoxProperties;
begin
  Result := TcxCustomCheckBoxProperties(inherited Properties);
end;

function TcxCustomCheckBox.GetActiveProperties: TcxCustomCheckBoxProperties;
begin
  Result := TcxCustomCheckBoxProperties(InternalGetActiveProperties);
end;

function TcxCustomCheckBox.GetState: TcxCheckBoxState;
begin
  if IsLoading and FIsLoadingStateAssigned then
    Result := FLoadingState
  else
    Result := ActiveProperties.GetState(EditValue);
end;

function TcxCustomCheckBox.GetStyle: TcxCheckBoxStyle;
begin
  Result := TcxCheckBoxStyle(FStyles.Style);
end;

function TcxCustomCheckBox.GetViewInfo: TcxCustomCheckBoxViewInfo;
begin
  Result := TcxCustomCheckBoxViewInfo(FViewInfo);
end;

function TcxCustomCheckBox.IsStateStored: Boolean;
const
  AStates: array[Boolean] of TcxCheckBoxState = (cbsUnchecked, cbsChecked);
begin
  Result := not (Action is TCustomAction) or
    (State <> AStates[TCustomAction(Action).Checked]);
end;

procedure TcxCustomCheckBox.SetChecked(Value: Boolean);
begin
  if Value then
    State := cbsChecked
  else
    State := cbsUnchecked;
end;

procedure TcxCustomCheckBox.SetProperties(Value: TcxCustomCheckBoxProperties);
begin
  Properties.Assign(Value);
end;

procedure TcxCustomCheckBox.SetState(Value: TcxCheckBoxState);
var
  AEditValue: TcxEditValue;
begin
  if IsLoading then
  begin
    FLoadingState := Value;
    FIsLoadingStateAssigned := True;
  end
  else
    if Value <> State then
    begin
      PrepareEditValue(Value, AEditValue, InternalFocused);
      InternalEditValue := AEditValue;
    end;
end;

procedure TcxCustomCheckBox.SetStyle(Value: TcxCheckBoxStyle);
begin
  FStyles.Style := Value;
end;

procedure TcxCustomCheckBox.WMLButtonUp(var Message: TWMLButtonUp);
begin
  ControlState := ControlState - [csClicked];
  inherited;
end;

procedure TcxCustomCheckBox.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  if ModifiedAfterEnter and (GetKeyState(VK_ESCAPE) < 0) then
    Message.Result := Message.Result or DLGC_WANTALLKEYS;
end;

procedure TcxCustomCheckBox.CMDialogChar(var Message: TCMDialogChar);
begin
  with Message do
    if IsAccel(CharCode, Caption) {D3 - bug "&&"} and CanFocus then
    begin
      SetFocus;
      if Focused then
        Toggle;
      Result := 1;
    end
    else
      inherited;
end;

procedure TcxCustomCheckBox.CMParentColorChanged(var Message: TMessage);
begin
  inherited;
  if ViewInfo.NativeStyle and ParentBackground then
    Invalidate;
end;

{ TcxCheckBox }

class function TcxCheckBox.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxCheckBoxProperties;
end;

function TcxCheckBox.GetActiveProperties: TcxCheckBoxProperties;
begin
  Result := TcxCheckBoxProperties(InternalGetActiveProperties);
end;

function TcxCheckBox.GetProperties: TcxCheckBoxProperties;
begin
  Result := TcxCheckBoxProperties(inherited Properties);
end;

procedure TcxCheckBox.SetProperties(Value: TcxCheckBoxProperties);
begin
  Properties.Assign(Value);
end;

{ TcxFilterCheckBoxHelper }

class function TcxFilterCheckBoxHelper.GetFilterDataType(AValueTypeClass: TcxValueTypeClass): TcxFilterDataType;
begin
  Result := fdtCheck;
end;

class procedure TcxFilterCheckBoxHelper.GetFilterValue(AEdit: TcxCustomEdit;
  AEditProperties: TcxCustomEditProperties; var V: Variant; var S: TCaption);
begin
  with TcxComboBox(AEdit) do
  begin
    case ItemIndex of
      -1:
        V := Null;
      0:
        V := TcxCustomCheckBoxProperties(AEditProperties).ValueChecked;
      1:
        V := TcxCustomCheckBoxProperties(AEditProperties).ValueUnchecked;
    end;
    if ItemIndex = -1 then
      S := ''
    else
      S := TcxCustomCheckBoxProperties(AEditProperties).GetDisplayText(V);
  end;
end;

class function TcxFilterCheckBoxHelper.GetSupportedFilterOperators(
  AProperties: TcxCustomEditProperties;
  AValueTypeClass: TcxValueTypeClass;
  AExtendedSet: Boolean = False): TcxFilterControlOperators;
begin
  Result := [fcoEqual, fcoNotEqual, fcoBlanks, fcoNonBlanks];
end;

class procedure TcxFilterCheckBoxHelper.InitializeProperties(AProperties,
  AEditProperties: TcxCustomEditProperties; AHasButtons: Boolean);
var
  ACheckBoxProperties: TcxCustomCheckBoxProperties;
begin
  ACheckBoxProperties := TcxCustomCheckBoxProperties(AEditProperties);
  with TcxComboBoxProperties(AProperties).Items do
  begin
    Clear;
    Add(ACheckBoxProperties.GetDisplayText(ACheckBoxProperties.ValueChecked));
    Add(ACheckBoxProperties.GetDisplayText(ACheckBoxProperties.ValueUnchecked));
  end;
  TcxComboBoxProperties(AProperties).DropDownListStyle := lsFixedList;
  TcxComboBoxProperties(AProperties).IDefaultValuesProvider := nil;
  ClearPropertiesEvents(AProperties);
end;

class procedure TcxFilterCheckBoxHelper.SetFilterValue(AEdit: TcxCustomEdit;
  AEditProperties: TcxCustomEditProperties; AValue: Variant);
const
  AItemIndexMap: array [TcxCheckBoxState] of Integer = (1, 0, -1);
var
  V: TcxEditValue;
begin
  AEditProperties.PrepareDisplayValue(AValue, V, AEdit.Focused);
  TcxComboBox(AEdit).ItemIndex := AItemIndexMap[TcxCheckBoxState((V))];
end;

class function TcxFilterCheckBoxHelper.UseDisplayValue: Boolean;
begin
  Result := True;
end;

{ TcxCheckBoxActionLink }

procedure TcxCheckBoxActionLink.AssignClient(AClient: TObject);
begin
  inherited AssignClient(AClient);
  FClient := AClient as TcxCustomCheckBox;
end;

function TcxCheckBoxActionLink.IsCheckedLinked: Boolean;
begin
  Result := inherited IsCheckedLinked and (FClient.State <> cbsGrayed) and
    (FClient.Checked = TCustomAction(Action).Checked);
end;

procedure TcxCheckBoxActionLink.SetChecked(Value: Boolean);
begin
  if IsCheckedLinked then
    InternalSetChecked(Value);
end;

procedure TcxCheckBoxActionLink.InternalSetChecked(Value: Boolean);
begin
  FClient.LockClick(True);
  FClient.LockChangeEvents(True);
  try
    FClient.Checked := Value;
  finally
    FClient.LockChangeEvents(False, False);
    FClient.LockClick(False);
  end;
end;

{ TcxCustomCheckControlFadingHelper }

constructor TcxCustomCheckControlFadingHelper.Create(AViewInfo: TcxCustomCheckBoxViewInfo);
begin
  inherited Create;
  FViewInfo := AViewInfo;
end;

procedure TcxCustomCheckControlFadingHelper.UpdateState;
var
  ANewState: TcxEditCheckState;
begin
  ANewState := ViewInfo.CheckBoxState;
  if State <> ANewState then
  begin
    CheckStartFading(
      CheckStateToButtonState[State],
      CheckStateToButtonState[ANewState]);
    FState := ANewState;
  end;
end;

function TcxCustomCheckControlFadingHelper.CanFade: Boolean;
begin
  Result := inherited CanFade and not cxRectIsEmpty(ViewInfo.CheckBoxRect);
end;

function TcxCustomCheckControlFadingHelper.GetEditViewInfo: TcxCustomEditViewInfo;
begin
  Result := ViewInfo;
end;

{ TcxCheckBoxFadingHelper }

procedure TcxCheckBoxFadingHelper.Invalidate;
begin
  Invalidate(ViewInfo.CheckBoxRect, False);
end;

procedure TcxCheckBoxFadingHelper.GetFadingImages(out AFadeOutImage, AFadeInImage: TcxBitmap);

  function HasBackground: Boolean;
  begin
    Result := not ViewInfo.IsBackgroundTransparent and
      (ViewInfo.BackgroundColor <> clNone) and (ViewInfo.Painter <> nil);
  end;

  function PrepareFadingImage(AState: TcxEditCheckState): TcxBitmap32;
  begin
    Result := TcxBitmap32.CreateSize(ViewInfo.CheckBoxRect, True);
    Result.Canvas.Lock;
    try
      DrawScaledEditCheck(Result.cxCanvas, Result.ClientRect, AState, ViewInfo, ViewInfo.ScaleFactor);
      if HasBackground then
        Result.MakeOpaque;
    finally
      Result.Canvas.Unlock;
    end;
  end;

begin
  AFadeOutImage := PrepareFadingImage(ecsNormal);
  AFadeInImage := PrepareFadingImage(ecsHot);
end;

initialization
  GetRegisteredEditProperties.Register(TcxCheckBoxProperties, scxSEditRepositoryCheckBoxItem);
  FilterEditsController.Register(TcxCheckBoxProperties, TcxFilterCheckBoxHelper);

finalization
  FilterEditsController.Unregister(TcxCheckBoxProperties, TcxFilterCheckBoxHelper);

end.
