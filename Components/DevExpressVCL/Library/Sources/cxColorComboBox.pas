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

unit cxColorComboBox;

interface

{$I cxVer.inc}

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Variants, Windows, Messages, TypInfo, SysUtils, Classes, Controls, Graphics, StdCtrls, Forms, Dialogs, Types,
  cxClasses, cxControls, cxContainer, cxGraphics, cxDataStorage, cxDataUtils, cxVariants, dxCoreClasses, cxEdit,
  cxTextEdit, cxEditUtils, cxMaskEdit, cxDropDownEdit, cxImageComboBox, cxExtEditConsts, cxFilterControlUtils,
  cxLookAndFeels, cxGeometry;

type
  TcxColorBoxAlign = (cbaLeft, cbaRight);
  TcxColorNamingConvention = (cxncNone, cxncDelphi, cxncHTML4, cxncX11);
  TcxColorPrepareList = (cxplNone, cxplDelphi, cxplHTML4, cxplX11, cxplX11Ordered);
  TcxColorValueFormat = (cxcvRGB, cxcvHexadecimal, cxcvInteger);
  TcxDefaultColorStyle = (cxdcClear, cxdcColor, cxdcHatched, cxdcText, cxdcCustom);
  TcxMRUColorAction = (mcaNone, mcaMoved, mcaAdded, mcaDeleted);

  TcxColorComboStyle = (cxccsComboList, cxccsComboEdit);
  TcxColorDialogType = (cxcdtDefault, cxcdtCustom, cxcdtAdvanced);

  { TcxColorDialogHelper }

  TcxColorDialogHelper = class(TObject)
  private
    FColor: TColor;
    FCustomColors: TStrings;
    FDialogIsShown: Boolean;
    FDialogShowFull: Boolean;
    FDialogType: TcxColorDialogType;

    procedure SetCustomColors(AValue: TStrings);
  protected
    FDialog: TObject;

    function ExecuteAdvancedDialog(var AColor: TColor): Boolean;
    function ExecuteStandardDialog(var AColor: TColor): Boolean;
    function GetDialogHandle: THandle; virtual;
    function GetLookAndFeel: TcxLookAndFeel; virtual;
    function GetOwner: TComponent; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function Execute: Boolean; virtual;
    //
    property Color: TColor read FColor write FColor;
    property CustomColors: TStrings read FCustomColors write SetCustomColors;
    property DialogHandle: THandle read GetDialogHandle;
    property DialogIsShown: Boolean read FDialogIsShown;
    property DialogShowFull: Boolean read FDialogShowFull write FDialogShowFull;
    property DialogType: TcxColorDialogType read FDialogType write FDialogType;
  end;

  { TcxColorComboBoxColorDialogHelper }

  TcxColorComboBoxColorDialogHelper = class(TcxColorDialogHelper)
  private
    FOwner: TcxCustomComboBox;
  protected
    function GetLookAndFeel: TcxLookAndFeel; override;
    function GetOwner: TComponent; override;
  public
    constructor Create(AOwner: TcxCustomComboBox); reintroduce;
    function Execute: Boolean; override;
  end;

  { TcxColorComboBoxItem }

  TcxColorComboBoxItem = class(TCollectionItem)
  private
    FColor: TColor;
    FDescription: TCaption;
    FIsCustomColor: Boolean;
    FTag: TcxTag;
    function GetDescription: TCaption;
    function IsTagStored: Boolean;
    procedure SetColor(const Value: TColor);
    procedure SetDescription(const Value: TCaption);
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;

    property IsCustomColor: Boolean read FIsCustomColor;
  published
    property Color: TColor read FColor write SetColor;
    property Description: TCaption read GetDescription write SetDescription;
    property Tag: TcxTag read FTag write FTag stored IsTagStored;
  end;

  { TcxColorComboBoxItems }

  TcxCustomColorComboBoxProperties = class;

  TcxColorComboBoxItems = class(TOwnedCollection)
  private
    FUpdateLocked: Boolean;
    FOnUpdate: TNotifyEvent;
    function GetItems(Index: Integer): TcxColorComboBoxItem;
    procedure SetItems(Index: Integer; const Value: TcxColorComboBoxItem);
  protected
    procedure Update(Item: TCollectionItem); override;

    property OnUpdate: TNotifyEvent read FOnUpdate write FOnUpdate;
  public
    function Add: TcxColorComboBoxItem;
    function AddColor(const AColor: TColor; const ADescription: string): TcxColorComboBoxItem; virtual;
    function Insert(Index: Integer): TcxColorComboBoxItem;
    function InsertColor(Index: Integer; const AColor: TColor; const ADescription: string): TcxColorComboBoxItem; virtual;

    function FindColorItem(const AColor: TColor): TcxColorComboBoxItem; virtual;
    function GetColorByIndex(AIndex: Integer; ADefaultColor: TColor): TColor;
    function GetIndexByColor(AColor: TColor): Integer;

    procedure CancelUpdate;
    procedure ClearCustom; virtual;
    procedure ClearNonCustom; virtual;
    procedure Move(CurIndex, NewIndex: Integer); virtual;
    function Owner: TcxCustomColorComboBoxProperties;

    property Items[Index: Integer]: TcxColorComboBoxItem read GetItems write SetItems; default;
  end;

  { TcxCustomColorComboBoxViewInfo }

  TcxCustomColorComboBoxViewInfo = class(TcxCustomTextEditViewInfo)
  private
    FBkColor: TColor;
    FColorBoxAlign: TcxColorBoxAlign;
    FColorBoxColor: TColor;
    FColorBoxFrameColor: TColor;
    FColorBoxRect: TRect;
    FColorBoxWidth: Integer;
    FDefaultColorStyle: TcxDefaultColorStyle;
    FFoundItem: Boolean;
    FShowDescriptions: Boolean;
  public
    procedure Offset(DX, DY: Integer); override;
    procedure Paint(ACanvas: TcxCanvas); override;

    property BkColor: TColor read FBkColor write FBkColor;
    property ColorBoxWidth: Integer read FColorBoxWidth write FColorBoxWidth;
    property ColorBoxAlign: TcxColorBoxAlign read FColorBoxAlign write FColorBoxAlign;
    property ColorBoxFrameColor: TColor read FColorBoxFrameColor write FColorBoxFrameColor;
    property ColorBoxColor: TColor read FColorBoxColor write FColorBoxColor;
    property ColorBoxRect: TRect read FColorBoxRect write FColorBoxRect;
    property DefaultColorStyle: TcxDefaultColorStyle read FDefaultColorStyle write FDefaultColorStyle;
    property ShowDescriptions: Boolean read FShowDescriptions write FShowDescriptions;
    property FoundItem: Boolean read FFoundItem write FFoundItem;
  end;

  { TcxCustomColorComboBoxViewData }

  TcxCustomColorComboBoxViewData = class(TcxCustomDropDownEditViewData)
  private
    function GetProperties: TcxCustomColorComboBoxProperties;
  protected
    procedure CalculateViewInfoProperties(AViewInfo: TcxCustomEditViewInfo); virtual;
    function InternalEditValueToDisplayText(AEditValue: TcxEditValue): string; override;
    function InternalGetEditConstantPartSize(ACanvas: TcxCanvas; AIsInplace: Boolean;
      AEditSizeProperties: TcxEditSizeProperties; var MinContentSize: TSize; AViewInfo: TcxCustomEditViewInfo): TSize; override;
    function IsComboBoxStyle: Boolean; override;
  public
    procedure Calculate(ACanvas: TcxCanvas; const ABounds: TRect; const P: TPoint; Button: TcxMouseButton;
      Shift: TShiftState; AViewInfo: TcxCustomEditViewInfo; AIsMouseEvent: Boolean); override;
    procedure DisplayValueToDrawValue(const ADisplayValue: TcxEditValue; AViewInfo: TcxCustomEditViewInfo); override;
    procedure EditValueToDrawValue(const AEditValue: TcxEditValue; AViewInfo: TcxCustomEditViewInfo); override;
    //
    property Properties: TcxCustomColorComboBoxProperties read GetProperties;
  end;

  { TcxColorComboBoxListBox }

  TcxCustomColorComboBox = class;

  TcxCustomColorComboBoxListBox = class(TcxCustomComboBoxListBox)
  strict private
    function GetEdit: TcxCustomColorComboBox;
  protected
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
    function GetDrawTextFlags: Cardinal; override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    function GetItemHeight(AIndex: Integer = -1): Integer; override;

    property Edit: TcxCustomColorComboBox read GetEdit;
  end;

  { TcxColorComboBoxLookupData }

  TcxColorComboBoxLookupData = class(TcxComboBoxLookupData)
  private
    function GetActiveProperties: TcxCustomColorComboBoxProperties;
  protected
    function GetItem(Index: Integer): string; override;
    function GetItemCount: Integer; override;
    function GetListBoxClass: TcxCustomEditListBoxClass; override;
    function NeedLocateItemWithFullString: Boolean; override;

    property ActiveProperties: TcxCustomColorComboBoxProperties read GetActiveProperties;
  public
    function GetVisualAreaPreferredSize(AMaxHeight: Integer; AWidth: Integer = 0): TSize; override;
  end;

  TcxChangeItemIndexEvent = procedure(Sender: TObject; const AIndex: Integer) of object;
  TcxGetDefaultColorEvent = procedure(Sender: TObject; var AColor: TColor) of object;
  TcxNamingConventionEvent = procedure(Sender: TObject; const AColor: TColor; var AColorDescription: string) of object;
  TcxSelectCustomColorEvent = procedure(Sender: TObject; var AColor: TColor;
    var AColorDescription: string; var AddToList: Boolean) of object;

  { TcxCustomColorComboBoxProperties }

  TcxCustomColorComboBoxProperties = class(TcxCustomComboBoxProperties)
  private
    FAllowSelectColor: Boolean;
    FColorBoxAlign: TcxColorBoxAlign;
    FColorBoxFrameColor: TColor;
    FColorBoxWidth: Integer;
    FColorComboStyle: TcxColorComboStyle;
    FColorDialogShowFull: Boolean;
    FColorDialogType: TcxColorDialogType;
    FColorValueFormat: TcxColorValueFormat;
    FDefaultColor: TColor;
    FDefaultColorStyle: TcxDefaultColorStyle;
    FDefaultDescription: string;

    FCustomColors: TcxColorComboBoxItems;
    FItems: TcxColorComboBoxItems;
    FMRUColors: TcxColorComboBoxItems;

    FMaxMRUColors: Byte;
    FNamingConvention: TcxColorNamingConvention;
    FPrepareList: TcxColorPrepareList;
    FShowDescriptions: Boolean;

    FOnAddedMRUColor: TNotifyEvent;
    FOnDeletedMRUColor: TNotifyEvent;
    FOnGetDefaultColor: TcxGetDefaultColorEvent;
    FOnLoadColorList: TNotifyEvent;
    FOnNamingConvention: TcxNamingConventionEvent;
    FOnSelectCustomColor: TcxSelectCustomColorEvent;

    function ColorItemByIndex(AIndex: Integer): TcxColorComboBoxItem;
    procedure DeleteOverMRUColors;
    function DoConvertNaming(AIndex: Integer): string;
    function GetDropDownListStyle: TcxEditDropDownListStyle;
    function GetItems: TcxColorComboBoxItems;
    procedure InternalGetColorComboBoxDisplayValue(AItemIndex: Integer; const AEditValue: TcxEditValue;
      out AColor: TColor; out ADescription: string; out AColorFound: Boolean);
    procedure InternalPrepareColorList(APrepareList: TcxColorPrepareList); overload;
    procedure InternalPrepareColorList(AColorValues: array of TColor; AColorNames: array of string); overload;
    function IsDefaultDescriptionStored: Boolean;
    procedure ReadCustomColors(Reader: TReader);
    procedure ReadPrepareInfo(Reader: TReader);
    procedure SetAllowSelectColor(Value: Boolean);
    procedure SetColorBoxAlign(Value: TcxColorBoxAlign);
    procedure SetColorBoxFrameColor(Value: TColor);
    procedure SetColorBoxWidth(Value: Integer);
    procedure SetColorComboStyle(Value: TcxColorComboStyle);
    procedure SetColorValueFormat(Value: TcxColorValueFormat);
    procedure SetDefaultColor(Value: TColor);
    procedure SetDefaultColorStyle(Value: TcxDefaultColorStyle);
    procedure SetDefaultDescription(Value: string);
    procedure SetDropDownListStyle(AValue: TcxEditDropDownListStyle);

    procedure SetCustomColors(Value: TcxColorComboBoxItems);
    procedure SetItems(const Value: TcxColorComboBoxItems);
    procedure SetMaxMRUColors(Value: Byte);
    procedure SetMRUColors(Value: TcxColorComboBoxItems);
    procedure SetNamingConvention(Value: TcxColorNamingConvention);
    procedure SetPrepareList(Value: TcxColorPrepareList);
    procedure SetShowDescriptions(const Value: Boolean);
    procedure SynchronizeCustomColors;
    procedure CustomColorChanged(ASender: TObject);
    procedure ValidateMRUColors;
  protected
    procedure ChangeScale(M: Integer; D: Integer); override;
    procedure DefineProperties(Filer: TFiler); override;
    function IsEditValueNumeric: Boolean; override;
    class function GetLookupDataClass: TcxInterfacedPersistentClass; override;
    class function GetPopupWindowClass: TcxCustomEditPopupWindowClass; override;
    class function GetViewDataClass: TcxCustomEditViewDataClass; override;

    function EditValueToColorValue(const AEditValue: Variant): TColor;
    function GetColorByIndex(AIndex: Integer): TColor;
    function GetColorByDescription(const ADescription: string; out AColor: TColor): Boolean;
    function GetIndexByColor(AColor: TColor): Integer;
    function GetDescriptionByIndex(AIndex: Integer): string;
    function IndexByValue(const AValue: TcxEditValue): Integer;
    function IsDisplayValueNumeric: Boolean; virtual;
    function ShowColorBox(AColorFound: Boolean): Boolean;
    function StrToColor(const S: string; out AColor: TColor): Boolean;

    function AddMRUColor(const AColor: TColor): TcxMRUColorAction; virtual;
    function DelMRUColor(const AColor: TColor): TcxMRUColorAction; virtual;
    procedure ClearMRUColors; virtual;
    procedure DoGetDefaultColor(var AColor: TColor); virtual;
    procedure TranslateValues(const AEditValue: TcxEditValue; var AColor: TColor; var ADescription: string; ANeedDescription: Boolean = False);
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function CompareDisplayValues(const AEditValue1, AEditValue2: TcxEditValue): Boolean; override;
    class function GetContainerClass: TcxContainerClass; override;
    class function GetViewInfoClass: TcxContainerViewInfoClass; override;
    function GetEditValueSource(AEditFocused: Boolean): TcxDataEditValueSource; override;
    function IsDisplayValueValid(var DisplayValue: TcxEditValue; AEditFocused: Boolean): Boolean; override;
    procedure PrepareDisplayValue(const AEditValue: TcxEditValue; var DisplayValue: TcxEditValue; AEditFocused: Boolean); override;
    function GetSupportedOperations: TcxEditSupportedOperations; override;
    function GetDisplayText(const AEditValue: TcxEditValue; AFullText: Boolean = False; AIsInplace: Boolean = True): string; override;
    procedure DoUpdate(AProperties: TcxCustomEditProperties); override;
    procedure GetColorComboBoxDisplayValue(const AEditValue: TcxEditValue;
      out AColor: TColor; out ADescription: string; out AColorFound: Boolean);

    procedure PrepareColorList(APrepareList: TcxColorPrepareList; ASaveCustom, ASaveMRU: Boolean);
    procedure PrepareDelphiColorList(const ASaveCustom, ASaveMRU: Boolean);
    procedure PrepareHTML4ColorList(const ASaveCustom, ASaveMRU: Boolean);
    procedure PrepareX11ColorList(const ASaveCustom, ASaveMRU: Boolean);
    procedure PrepareX11OrderedColorList(const ASaveCustom, ASaveMRU: Boolean);
    // !!!
    property AllowSelectColor: Boolean read FAllowSelectColor write SetAllowSelectColor default False;
    property ColorBoxAlign: TcxColorBoxAlign read FColorBoxAlign write SetColorBoxAlign default cbaLeft;
    property ColorBoxFrameColor: TColor read FColorBoxFrameColor write SetColorBoxFrameColor default clBtnShadow;
    property ColorBoxWidth: Integer read FColorBoxWidth write SetColorBoxWidth default 30;
    property ColorComboStyle: TcxColorComboStyle read FColorComboStyle write SetColorComboStyle default cxccsComboEdit;
    property ColorDialogShowFull: Boolean read FColorDialogShowFull write FColorDialogShowFull default False;
    property ColorDialogType: TcxColorDialogType read FColorDialogType write FColorDialogType default cxcdtDefault;
    property ColorValueFormat: TcxColorValueFormat read FColorValueFormat write SetColorValueFormat default cxcvRGB;
    property DefaultColor: TColor read FDefaultColor write SetDefaultColor default clWindow;
    property DefaultColorStyle: TcxDefaultColorStyle read FDefaultColorStyle write SetDefaultColorStyle default cxdcColor;
    property DefaultDescription: string read FDefaultDescription write SetDefaultDescription stored IsDefaultDescriptionStored;
    property DropDownListStyle: TcxEditDropDownListStyle read GetDropDownListStyle write SetDropDownListStyle default lsFixedList;

    property CustomColors: TcxColorComboBoxItems read FCustomColors write SetCustomColors;
    property Items: TcxColorComboBoxItems read GetItems write SetItems;
    property MaxMRUColors: Byte read FMaxMRUColors write SetMaxMRUColors default 10;
    property MRUColors: TcxColorComboBoxItems read FMRUColors write SetMRUColors;

    property NamingConvention: TcxColorNamingConvention read FNamingConvention write SetNamingConvention default cxncDelphi;
    property PrepareList: TcxColorPrepareList read FPrepareList write SetPrepareList default cxplDelphi;
    property ShowDescriptions: Boolean read FShowDescriptions write SetShowDescriptions default True;

    property OnAddedMRUColor: TNotifyEvent read FOnAddedMRUColor write FOnAddedMRUColor;
    property OnDeletedMRUColor: TNotifyEvent read FOnDeletedMRUColor write FOnDeletedMRUColor;
    property OnGetDefaultColor: TcxGetDefaultColorEvent read FOnGetDefaultColor write FOnGetDefaultColor;
    property OnNamingConvention: TcxNamingConventionEvent read FOnNamingConvention write FOnNamingConvention;
    property OnSelectCustomColor: TcxSelectCustomColorEvent read FOnSelectCustomColor write FOnSelectCustomColor;
  end;

  { TcxColorComboBoxProperties }

  TcxColorComboBoxProperties = class(TcxCustomColorComboBoxProperties)
  published
    property Alignment;
    property AllowSelectColor;
    property AssignedValues;
    property BeepOnError;
    property ButtonGlyph;
    property CharCase;
    property ClearKey;
    property ColorBoxAlign;
    property ColorBoxFrameColor;
    property ColorBoxWidth;
    property ColorComboStyle;
    property ColorDialogShowFull;
    property ColorDialogType;
    property ColorValueFormat;
    property CustomColors;
    property DefaultColor;
    property DefaultColorStyle;
    property DefaultDescription;
    property DropDownAutoWidth;
    property DropDownListStyle;
    property DropDownRows;
    property DropDownSizeable;
    property DropDownWidth;
    property ImeMode;
    property ImeName;
    property ImmediateDropDownWhenActivated;
    property ImmediateDropDownWhenKeyPressed;
    property ImmediatePost;
    property ImmediateUpdateText;
    property IncrementalFiltering;
    property IncrementalFilteringOptions;
    property MaxMRUColors;
    property NamingConvention;
    property OEMConvert;
    property PopupAlignment;
    property PostPopupValueOnTab;
    property PrepareList;
    property ReadOnly;
    property Revertable;
    property ShowDescriptions;
    property ValidateOnEnter;
    property OnAddedMRUColor;
    property OnChange;
    property OnCloseUp;
    property OnDeletedMRUColor;
    property OnEditValueChanged;
    property OnGetDefaultColor;
    property OnInitPopup;
    property OnNamingConvention;
    property OnNewLookupDisplayText;
    property OnPopup;
    property OnSelectCustomColor;
  end;

  { TcxCustomColorComboBoxInnerEdit }

  TcxCustomColorComboBoxInnerEdit = class(TcxCustomComboBoxInnerEdit);

  { TcxColorComboBoxPopupWindow }

  TcxColorComboBoxPopupWindow = class(TcxComboBoxPopupWindow)
  public
    property ViewInfo;
    property SysPanelStyle;
  end;

  { TcxCustomColorComboBox }

  TcxCustomColorComboBox = class(TcxCustomComboBox)
  private
    FColorDialogHelper: TcxColorDialogHelper;
    function GetColorDialogHelper: TcxColorDialogHelper;
    function GetColorValue: TColor;
    function IsColorValueStored: Boolean;
    procedure SetColorValue(Value: TColor);
    function GetLookupData: TcxColorComboBoxLookupData;
    function GetProperties: TcxCustomColorComboBoxProperties;
    function GetActiveProperties: TcxCustomColorComboBoxProperties;
    procedure SetProperties(Value: TcxCustomColorComboBoxProperties);
    procedure PropertiesLoadColorListHandler(Sender: TObject);
    procedure FlushEditValue;
  protected
    function CanSelectItem(AFindSelection: Boolean): Boolean; override;
    function CreateColorDialogHelper: TcxColorDialogHelper; virtual;
    procedure PropertiesChanged(Sender: TObject); override;
    procedure DoButtonClick(AButtonVisibleIndex: Integer); override;
    function GetInnerEditClass: TControlClass; override;
    procedure Initialize; override;
    procedure InitializeInnerEdit; override;
    procedure InitializePopupWindow; override;
    procedure InternalValidateDisplayValue(const ADisplayValue: TcxEditValue); override;
    function LookupKeyToEditValue(const AKey: TcxEditValue): TcxEditValue; override;
    procedure DoEditKeyDown(var Key: Word; Shift: TShiftState); override;
    function DoSelectCustomColor(AColor: TColor; ANeedFireEvent: Boolean): Integer; virtual;
    procedure ClearEditValue; virtual;
    function DoOnSelectCustomColor(var AColor: TColor; out AColorDescription: string; ANeedFireEvent: Boolean): Boolean;
    procedure HandleSelectItem(Sender: TObject); override;
    function NeedResetInvalidTextWhenPropertiesChanged: Boolean; override;
    procedure SelectCustomColor(Sender: TObject); virtual;

    property ColorDialogHelper: TcxColorDialogHelper read GetColorDialogHelper;
    property ColorValue: TColor read GetColorValue write SetColorValue stored IsColorValueStored;
    property LookupData: TcxColorComboBoxLookupData read GetLookupData;
  public
    destructor Destroy; override;

    function Focused: Boolean; override;
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    function IsChildWindow(AWnd: THandle): Boolean; override;
    procedure PrepareEditValue(const ADisplayValue: TcxEditValue; out EditValue: TcxEditValue; AEditFocused: Boolean); override;

    function AddMRUColor(const AColor: TColor): TcxMRUColorAction;
    function DelMRUColor(const AColor: TColor): TcxMRUColorAction;

    procedure PrepareColorList(APrepareList: TcxColorPrepareList; ASaveCustom, ASaveMRU: Boolean);
    procedure PrepareDelphiColorList(const ASaveCustom, ASaveMRU: Boolean);
    procedure PrepareHTML4ColorList(const ASaveCustom, ASaveMRU: Boolean);
    procedure PrepareX11ColorList(const ASaveCustom, ASaveMRU: Boolean);
    procedure PrepareX11OrderedColorList(const ASaveCustom, ASaveMRU: Boolean);

    property ActiveProperties: TcxCustomColorComboBoxProperties read GetActiveProperties;
    property Properties: TcxCustomColorComboBoxProperties read GetProperties write SetProperties;
  end;

  { TcxColorComboBox }

  TcxColorComboBox = class(TcxCustomColorComboBox)
  private
    function GetActiveProperties: TcxColorComboBoxProperties;
    function GetProperties: TcxColorComboBoxProperties;
    procedure SetProperties(Value: TcxColorComboBoxProperties);
  public
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    property ActiveProperties: TcxColorComboBoxProperties read GetActiveProperties;
  published
    property Anchors;
    property AutoSize;
    property BeepOnEnter;
    property BiDiMode;
    property ColorValue;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentColor;
    property ParentFont;
    property ParentBiDiMode;
    property ParentShowHint;
    property PopupMenu;
    property Properties: TcxColorComboBoxProperties read GetProperties write SetProperties;
    property ShowHint;
    property Style;
    property StyleDisabled;
    property StyleFocused;
    property StyleHot;
    property TabOrder;
    property TabStop;
    property Visible;
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

  { TcxFilterColorComboBoxHelper }

  TcxFilterColorComboBoxHelper = class(TcxFilterComboBoxHelper)
  public
    class function GetFilterEditClass: TcxCustomEditClass; override;
    class function GetSupportedFilterOperators(AProperties: TcxCustomEditProperties;
      AValueTypeClass: TcxValueTypeClass; AExtendedSet: Boolean = False): TcxFilterControlOperators; override;
    class procedure InitializeProperties(AProperties, AEditProperties: TcxCustomEditProperties; AHasButtons: Boolean); override;
  end;

  { TcxColorComboBoxHelper }

  TcxColorComboBoxHelper = class
  public
    class procedure DrawColorBox(ACanvas: TcxCanvas; ARect: TRect;
      const AFrameColor, ABoxColor, ABkColor: TColor; const DefaultColorStyle: TcxDefaultColorStyle); overload;
    class procedure DrawColorBox(ACanvas: TcxCanvas; ARect: TRect; const AFrameColor, ABoxColor, ABkColor: TColor;
      const DefaultColorStyle: TcxDefaultColorStyle; AScaleFactor: TdxScaleFactor); overload;
    class function GetColorName(AColor: TColor; const ADescription: string;
      const ANamingConvention: TcxColorNamingConvention; const AColorValueFormat: TcxColorValueFormat): string;
  end;

implementation

uses
  dxCore, cxExtEditUtils, dxThemeConsts, dxThemeManager, dxUxTheme, Math, dxColorDialog, dxCoreGraphics, dxColorPicker,
  dxDPIAwareUtils;

type
  TCanvasAccess = class(TCanvas);

{ TcxColorDialogHelper }

constructor TcxColorDialogHelper.Create;
begin
  inherited Create;
  FCustomColors := TStringList.Create;
end;

destructor TcxColorDialogHelper.Destroy;
begin
  FreeAndNil(FCustomColors);
  inherited Destroy;
end;

function TcxColorDialogHelper.Execute: Boolean;
begin
  FDialogIsShown := True;
  try
    if DialogType = cxcdtAdvanced then
      Result := ExecuteAdvancedDialog(FColor)
    else
      Result := ExecuteStandardDialog(FColor);
  finally
    FDialogIsShown := False;
  end;
end;

function TcxColorDialogHelper.ExecuteAdvancedDialog(var AColor: TColor): Boolean;
var
  ADialog: TdxColorDialog;
begin
  FDialog := TdxColorDialog.Create(GetOwner);
  try
    ADialog := TdxColorDialog(FDialog);
    ADialog.Color := dxColorToAlphaColor(AColor);
    ADialog.CustomColors := CustomColors;
    ADialog.LookAndFeel.MasterLookAndFeel := GetLookAndFeel;
    ADialog.Options.ColorPicker.AllowEditAlpha := False;
    ADialog.Options.ColorPicker.DefaultVisible := DialogShowFull;
    ADialog.Options.ColorPicker.RGBHexNotation := cphnDelphi;
    Result := ADialog.Execute;
    if Result then
    begin
      CustomColors := ADialog.CustomColors;
      AColor := dxAlphaColorToColor(ADialog.Color);
    end;
  finally
    FreeAndNil(FDialog);
  end;
end;

function TcxColorDialogHelper.ExecuteStandardDialog(var AColor: TColor): Boolean;
var
  ADialog: TColorDialog;
begin
  FDialog := TColorDialog.Create(GetOwner);
  try
    ADialog := TColorDialog(FDialog);
    ADialog.Color := AColor;
    ADialog.CustomColors := CustomColors;
    if DialogShowFull then
      ADialog.Options := ADialog.Options + [cdFullOpen];
    Result := ADialog.Execute;
    if Result then
    begin
      CustomColors := ADialog.CustomColors;
      AColor := ADialog.Color;
    end;
  finally
    FreeAndNil(FDialog);
  end;
end;

function TcxColorDialogHelper.GetDialogHandle: THandle;
begin
  if FDialog is TColorDialog then
    Result := TColorDialog(FDialog).Handle
  else
    if FDialog is TdxColorDialog then
      Result := TdxColorDialog(FDialog).Handle
    else
      raise EdxException.Create('Invalid dialog type');
end;

function TcxColorDialogHelper.GetLookAndFeel: TcxLookAndFeel;
begin
  Result := RootLookAndFeel;
end;

function TcxColorDialogHelper.GetOwner: TComponent;
begin
  Result := nil;
end;

procedure TcxColorDialogHelper.SetCustomColors(AValue: TStrings);
begin
  FCustomColors.Assign(AValue);
end;

{ TcxColorComboBoxColorDialogHelper }

constructor TcxColorComboBoxColorDialogHelper.Create(AOwner: TcxCustomComboBox);
begin
  inherited Create;
  FOwner := AOwner;
end;

function TcxColorComboBoxColorDialogHelper.Execute: Boolean;
begin
  DisableAppWindows;
  try
    Result := inherited Execute;
  finally
    EnableAppWindows;
  end;
end;

function TcxColorComboBoxColorDialogHelper.GetLookAndFeel: TcxLookAndFeel;
begin
  Result := FOwner.Style.LookAndFeel;
end;

function TcxColorComboBoxColorDialogHelper.GetOwner: TComponent;
begin
  Result := FOwner;
end;

{ TcxColorComboBoxItem }

constructor TcxColorComboBoxItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FIsCustomColor := True;
end;

procedure TcxColorComboBoxItem.Assign(Source: TPersistent);
begin
  if Source is TcxColorComboBoxItem then
  begin
    Color := TcxColorComboBoxItem(Source).Color;
    Description := TcxColorComboBoxItem(Source).Description;
    FIsCustomColor := TcxColorComboBoxItem(Source).IsCustomColor;
    Tag := TcxColorComboBoxItem(Source).Tag;
  end
  else
    inherited Assign(Source);
end;

function TcxColorComboBoxItem.GetDescription: TCaption;
begin
  Result := FDescription;
end;

function TcxColorComboBoxItem.IsTagStored: Boolean;
begin
  Result := FTag <> 0;
end;

procedure TcxColorComboBoxItem.SetDescription(const Value: TCaption);
begin
  if FDescription <> Value then
  begin
    FDescription := Value;
    Changed(False);
  end;
end;

procedure TcxColorComboBoxItem.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Changed(False);
  end;
end;

{ TcxColorComboBoxItems }

function TcxColorComboBoxItems.Add: TcxColorComboBoxItem;
begin
  Result := TcxColorComboBoxItem(inherited Add);
end;

function TcxColorComboBoxItems.AddColor(const AColor: TColor; const ADescription: string): TcxColorComboBoxItem;
begin
  Result := nil;
  if FindColorItem(AColor) = nil then
  begin
    BeginUpdate;
    try
      Result := Add;
      Result.Color := AColor;
      Result.Description := ADescription;
    finally
      EndUpdate;
    end;
  end;
end;

function TcxColorComboBoxItems.Insert(Index: Integer): TcxColorComboBoxItem;
begin
  Result := TcxColorComboBoxItem(inherited Insert(Index));
end;

function TcxColorComboBoxItems.InsertColor(Index: Integer;
  const AColor: TColor; const ADescription: string): TcxColorComboBoxItem;
begin
  Result := nil;
  if FindColorItem(AColor) = nil then
  begin
    BeginUpdate;
    try
      Result := Insert(Index);
      Result.Color := AColor;
      Result.Description := ADescription;
    finally
      EndUpdate;
    end;
  end;
end;

function TcxColorComboBoxItems.FindColorItem(const AColor: TColor): TcxColorComboBoxItem;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
    if Items[I].Color = AColor then
    begin
      Result := Items[I];
      Break;
    end;
end;

function TcxColorComboBoxItems.GetIndexByColor(AColor: TColor): Integer;
var
  I : Integer;
begin
  Result := -1;
  for I := 0 to Count - 1 do
    if Items[I].Color = AColor then
    begin
      Result := I;
      Break;
    end;
end;

function TcxColorComboBoxItems.GetColorByIndex(AIndex: Integer; ADefaultColor: TColor): TColor;
begin
  Result := ADefaultColor;
  if (AIndex >= 0) and (AIndex <= (Count - 1)) then
    Result := Items[AIndex].Color;
end;

procedure TcxColorComboBoxItems.CancelUpdate;
begin
  FUpdateLocked := True;
  try
    EndUpdate;
  finally
    FUpdateLocked := False;
  end;
end;

procedure TcxColorComboBoxItems.ClearCustom;
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if Items[I].IsCustomColor then
      Delete(I);
end;

procedure TcxColorComboBoxItems.ClearNonCustom;
var
  I: Integer;
begin
  for I := (Count - 1) downto 0 do
    if not Items[I].IsCustomColor then
      Delete(I);
end;

procedure TcxColorComboBoxItems.Move(CurIndex, NewIndex: Integer);
var
  ANewColorItem, AOldColorItem: TcxColorComboBoxItem;
begin
  if CurIndex <> NewIndex then
  begin
    AOldColorItem := Items[CurIndex];
    ANewColorItem := Insert(NewIndex);
    ANewColorItem.Assign(AOldColorItem);
    AOldColorItem.Free;
  end;
end;

function TcxColorComboBoxItems.Owner: TcxCustomColorComboBoxProperties;
begin
  if GetOwner is TcxCustomColorComboBoxProperties then
    Result := TcxCustomColorComboBoxProperties(GetOwner)
  else
    Result := nil;
end;

procedure TcxColorComboBoxItems.Update(Item: TCollectionItem);
begin
  if FUpdateLocked then
    Exit;
  if Assigned(OnUpdate) then
    OnUpdate(Item);
  if Owner <> nil then
    Owner.Changed;
end;

function TcxColorComboBoxItems.GetItems(Index: Integer): TcxColorComboBoxItem;
begin
  Result := TcxColorComboBoxItem(inherited Items[Index]);
end;

procedure TcxColorComboBoxItems.SetItems(Index: Integer;const Value: TcxColorComboBoxItem);
begin
  inherited Items[Index] := Value;
end;

{ TcxCustomColorComboBoxViewInfo }

procedure TcxCustomColorComboBoxViewInfo.Offset(DX, DY: Integer);
begin
  inherited Offset(DX, DY);
  OffsetRect(FColorBoxRect, DX, DY);
end;

procedure TcxCustomColorComboBoxViewInfo.Paint(ACanvas: TcxCanvas);
var
  ARealDefaultColorStyle: TcxDefaultColorStyle;
begin
  inherited Paint(ACanvas);
  if not FoundItem and (DefaultColorStyle = cxdcText) then
    Exit;

  ARealDefaultColorStyle := DefaultColorStyle;
  if (DefaultColorStyle <> cxdcText) and FoundItem then
    ARealDefaultColorStyle := cxdcColor;
  TcxColorComboBoxHelper.DrawColorBox(ACanvas, ColorBoxRect,
    ColorBoxFrameColor, ColorBoxColor, BkColor, ARealDefaultColorStyle, ScaleFactor);

  if not IsInplace and not ShowDescriptions and Focused and not HasPopupWindow then
  begin
    ACanvas.Font.Color := clBtnText;
    ACanvas.Brush.Color := BackgroundColor;
    TCanvasAccess(ACanvas.Canvas).RequiredState([csFontValid]);
    ACanvas.Canvas.DrawFocusRect(ClientRect);
  end;
end;

{ TcxCustomColorComboBoxViewData }

procedure TcxCustomColorComboBoxViewData.Calculate(ACanvas: TcxCanvas; const ABounds: TRect; const P: TPoint;
  Button: TcxMouseButton; Shift: TShiftState; AViewInfo: TcxCustomEditViewInfo; AIsMouseEvent: Boolean);
var
  R: TRect;
  AColorViewInfo: TcxCustomColorComboBoxViewInfo;
begin
  inherited Calculate(ACanvas, ABounds, P, Button, Shift, AViewInfo, AIsMouseEvent);

  if IsRectEmpty(ABounds) or (ABounds.Right = cxMaxRectSize) or (ABounds.Bottom = cxMaxRectSize) then
    Exit;

  AColorViewInfo := TcxCustomColorComboBoxViewInfo(AViewInfo);
  CalculateViewInfoProperties(AViewInfo);
  R := AViewInfo.ClientRect;
  AColorViewInfo.FColorBoxRect := R;
  if (AColorViewInfo.DefaultColorStyle = cxdcText) and not AColorViewInfo.FoundItem then
    AColorViewInfo.FColorBoxRect.Right := AColorViewInfo.FColorBoxRect.Left
  else
  begin
    if AColorViewInfo.ShowDescriptions then
    begin
      if AColorViewInfo.ColorBoxAlign = cbaLeft then
      begin
        AColorViewInfo.FColorBoxRect.Right := AColorViewInfo.FColorBoxRect.Left + AColorViewInfo.ColorBoxWidth;
        R.Left := AColorViewInfo.FColorBoxRect.Right;
      end
      else
      begin
        AColorViewInfo.FColorBoxRect.Left := AColorViewInfo.FColorBoxRect.Right - AColorViewInfo.ColorBoxWidth;
        R.Right := AColorViewInfo.FColorBoxRect.Left;
      end;
    end;
  end;
  AColorViewInfo.ClientRect := R;
  InflateRect(R, -2, -2 + Ord(IsInplace));

  AColorViewInfo.TextRect := R;
  if not AColorViewInfo.ShowDescriptions then
    AColorViewInfo.Text := '';
  if not IsInplace then
    AColorViewInfo.DrawSelectionBar := False;
end;

procedure TcxCustomColorComboBoxViewData.DisplayValueToDrawValue(
  const ADisplayValue: TcxEditValue; AViewInfo: TcxCustomEditViewInfo);
var
  AViewInfoAccess: TcxCustomColorComboBoxViewInfo;
begin
  if (Edit = nil) or IsVarEmpty(ADisplayValue) then
    Exit;
  AViewInfoAccess := TcxCustomColorComboBoxViewInfo(AViewInfo);
  Properties.InternalGetColorComboBoxDisplayValue(TcxCustomColorComboBox(Edit).ILookupData.CurrentKey,
    Edit.EditingValue, AViewInfoAccess.FColorBoxColor, AViewInfoAccess.Text, AViewInfoAccess.FFoundItem);
  inherited DisplayValueToDrawValue(ADisplayValue, AViewInfo);
end;

procedure TcxCustomColorComboBoxViewData.EditValueToDrawValue(
  const AEditValue: TcxEditValue; AViewInfo: TcxCustomEditViewInfo);
var
  AColorComboViewInfo: TcxCustomColorComboBoxViewInfo;
begin
  AColorComboViewInfo := AViewInfo as TcxCustomColorComboBoxViewInfo;
  Properties.GetColorComboBoxDisplayValue(AEditValue,
    AColorComboViewInfo.FColorBoxColor, AColorComboViewInfo.Text, AColorComboViewInfo.FFoundItem);
  if PreviewMode then
    AColorComboViewInfo.Text := '';
  DoOnGetDisplayText(AColorComboViewInfo.Text);
end;

procedure TcxCustomColorComboBoxViewData.CalculateViewInfoProperties(AViewInfo: TcxCustomEditViewInfo);
var
  AProperties: TcxCustomColorComboBoxProperties;
begin
  AProperties := TcxCustomColorComboBoxProperties(Properties);
  with TcxCustomColorComboBoxViewInfo(AViewInfo) do
  begin
    BkColor := BackgroundColor;
    if UseRightToLeftAlignment then
      if AProperties.ColorBoxAlign = cbaLeft then
        ColorBoxAlign := cbaRight
      else
        ColorBoxAlign := cbaLeft
    else
      ColorBoxAlign := AProperties.ColorBoxAlign;
    ColorBoxWidth := ScaleFactor.Apply(AProperties.ColorBoxWidth, AProperties.ScaleFactor);
    ColorBoxFrameColor := AProperties.ColorBoxFrameColor;
    ShowDescriptions := AProperties.ShowDescriptions;
    DefaultColorStyle := AProperties.DefaultColorStyle;
    if (DefaultColorStyle = cxdcCustom) and (not FoundItem) then
      AProperties.DoGetDefaultColor(FColorBoxColor);
  end;
end;

function TcxCustomColorComboBoxViewData.InternalEditValueToDisplayText(AEditValue: TcxEditValue): string;
var
  AColor: TColor;
  AColorFound: Boolean;
  ADisplayText: string;
begin
  Properties.GetColorComboBoxDisplayValue(AEditValue, AColor, ADisplayText, AColorFound);
  Result := ADisplayText;
end;

function TcxCustomColorComboBoxViewData.InternalGetEditConstantPartSize(ACanvas: TcxCanvas; AIsInplace: Boolean;
  AEditSizeProperties: TcxEditSizeProperties; var MinContentSize: TSize; AViewInfo: TcxCustomEditViewInfo): TSize;
var
  AProperties: TcxCustomColorComboBoxProperties;
begin
  Result := inherited InternalGetEditConstantPartSize(ACanvas, AIsInplace, AEditSizeProperties, MinContentSize, AViewInfo);
  AProperties := TcxCustomColorComboBoxProperties(Properties);
  if AProperties.ShowDescriptions then
    Inc(Result.cx, ScaleFactor.Apply(AProperties.ColorBoxWidth, AProperties.ScaleFactor) + 6);
end;

function TcxCustomColorComboBoxViewData.IsComboBoxStyle: Boolean;
begin
  Result := True;
end;

function TcxCustomColorComboBoxViewData.GetProperties: TcxCustomColorComboBoxProperties;
begin
  Result := TcxCustomColorComboBoxProperties(FProperties);
end;

{ TcxCustomColorComboBoxListBox }

function TcxCustomColorComboBoxListBox.GetItemHeight(AIndex: Integer = -1): Integer;
var
  AProperties: TcxCustomColorComboBoxProperties;
begin
  AProperties := Edit.ActiveProperties;
  if AProperties.ItemHeight > 0 then
    Result := AProperties.ItemHeight
  else
    Result := Max(AProperties.ScaleFactor.Apply(16), inherited GetItemHeight);

  if (AIndex >= 0) and Edit.IsOnMeasureItemEventAssigned then
    Edit.DoOnMeasureItem(AIndex, Canvas, Result);
  if AIndex = AProperties.FMRUColors.Count - 1 then
    Inc(Result, MRUDelimiterWidth);
end;

procedure TcxCustomColorComboBoxListBox.DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  AColorBoxRect, ATextRect: TRect;
  AText: string;
  AViewInfo: TcxCustomColorComboBoxViewInfo;
begin
  SaveCanvasParametersForFocusRect;
  try
    if DoDrawItem(Index, Rect, State) then
      Exit;

    Canvas.FillRect(Rect);
    AColorBoxRect := Rect;
    if Index = Edit.ActiveProperties.MRUColors.Count - 1 then
      Dec(AColorBoxRect.Bottom, MRUDelimiterWidth);
    ATextRect := AColorBoxRect;
    AViewInfo := Edit.ViewInfo as TcxCustomColorComboBoxViewInfo;
    if AViewInfo.ShowDescriptions then
    begin
      if AViewInfo.ColorBoxAlign = cbaRight then
      begin
        Dec(ATextRect.Right, AViewInfo.ColorBoxWidth);
        AColorBoxRect.Left := ATextRect.Right;
      end
      else
      begin
        Inc(ATextRect.Left, AViewInfo.ColorBoxWidth);
        AColorBoxRect.Right := ATextRect.Left;
      end;
      AText := GetItem(Index);
      if IsHighlightSearchText then
        DrawItemText(AText, ATextRect)
      else
        Canvas.DrawText(AText, ATextRect, GetDrawTextFlags);
    end
    else
      ATextRect.Left := AColorBoxRect.Right;

    TcxColorComboBoxHelper.DrawColorBox(Canvas, AColorBoxRect, Edit.ActiveProperties.ColorBoxFrameColor,
      Edit.ActiveProperties.GetColorByIndex(Edit.LookupData.GetLookupItemIndexFromFilteredItemIndex(Index)),
      Canvas.Brush.Color, cxdcColor, Edit.ScaleFactor);
    if Index = Edit.ActiveProperties.FMRUColors.Count - 1 then
      DrawMRUDelimiter(Canvas.Canvas, Rect, odSelected in State);
  finally
    RestoreCanvasParametersForFocusRect;
  end;
end;

function TcxCustomColorComboBoxListBox.GetDrawTextFlags: Cardinal;
begin
  Result := inherited GetDrawTextFlags;
  if not IsHighlightSearchText then
    Result := Result and not DT_VCENTER;
end;

procedure TcxCustomColorComboBoxListBox.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Edit.BeginUserAction;
  try
    inherited MouseUp(Button, Shift, X, Y);
    if Button = mbLeft then
    begin
      SetCaptureControl(nil);
      Edit.CloseUp(crEnter);
    end;
  finally
    Edit.EndUserAction;
  end;
end;

function TcxCustomColorComboBoxListBox.GetEdit: TcxCustomColorComboBox;
begin
  Result := TcxCustomColorComboBox(inherited Edit);
end;

{ TcxColorComboBoxLookupData }

function TcxColorComboBoxLookupData.GetVisualAreaPreferredSize(AMaxHeight: Integer; AWidth: Integer = 0): TSize;
var
  AScrollWidth: Integer;
begin
  Result := inherited GetVisualAreaPreferredSize(AMaxHeight, AWidth);
  Inc(Result.cx, ActiveProperties.ColorBoxWidth);
  AScrollWidth := List.ScrollWidth;
  Inc(AScrollWidth, ActiveProperties.ColorBoxWidth);
  List.ScrollWidth := 0;
  List.ScrollWidth := AScrollWidth;
end;

function TcxColorComboBoxLookupData.GetItem(Index: Integer): string;
begin
  Result := ActiveProperties.GetDescriptionByIndex(Index);
end;

function TcxColorComboBoxLookupData.GetItemCount: Integer;
begin
  Result := ActiveProperties.MRUColors.Count + ActiveProperties.Items.Count;
end;

function TcxColorComboBoxLookupData.GetListBoxClass: TcxCustomEditListBoxClass;
begin
  Result := TcxCustomColorComboBoxListBox;
end;

function TcxColorComboBoxLookupData.NeedLocateItemWithFullString: Boolean;
begin
  Result := True;
end;

function TcxColorComboBoxLookupData.GetActiveProperties: TcxCustomColorComboBoxProperties;
begin
  Result := inherited ActiveProperties as TcxCustomColorComboBoxProperties;
end;

{ TcxCustomColorComboBoxProperties }

constructor TcxCustomColorComboBoxProperties.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FItems := TcxColorComboBoxItems.Create(Self, TcxColorComboBoxItem);
  FCustomColors := TcxColorComboBoxItems.Create(Self, TcxColorComboBoxItem);
  FCustomColors.OnUpdate := CustomColorChanged;
  FMRUColors := TcxColorComboBoxItems.Create(Self, TcxColorComboBoxItem);

  DropDownListStyle := lsFixedList;
  FColorBoxAlign := cbaLeft;
  FColorBoxWidth := 30;
  FColorBoxFrameColor := clBtnShadow;
  FColorDialogType := cxcdtDefault;
  FShowDescriptions := True;
  FDefaultColor := clWindow;
  FDefaultDescription := cxGetResourceString(@cxSColorComboBoxDefaultDescription);
  FDefaultColorStyle := cxdcColor;
  FAllowSelectColor := False;
  FColorComboStyle := cxccsComboEdit;
  FNamingConvention := cxncDelphi;
  FColorValueFormat := cxcvRGB;
  FMaxMRUColors := 10;
  PrepareList := cxplDelphi;
  Buttons.Add;
  Buttons[1].Kind := bkEllipsis;
  Buttons[1].Default := False;
  Buttons[1].Visible := False;
end;

destructor TcxCustomColorComboBoxProperties.Destroy;
begin
  FreeAndNil(FMRUColors);
  FreeAndNil(FCustomColors);
  FreeAndNil(FItems);
  inherited;
end;

procedure TcxCustomColorComboBoxProperties.Assign(Source: TPersistent);
begin
  if Source is TcxCustomColorComboBoxProperties then
  begin
    BeginUpdate;
    try
      inherited Assign(Source);
      with Source as TcxCustomColorComboBoxProperties do
      begin
        Self.ColorBoxWidth := Self.ScaleFactor.Apply(ColorBoxWidth, ScaleFactor);
        Self.ColorBoxAlign := ColorBoxAlign;
        Self.ColorBoxFrameColor := ColorBoxFrameColor;
        Self.ColorDialogType := ColorDialogType;
        Self.ColorDialogShowFull := ColorDialogShowFull;
        Self.DefaultColor := DefaultColor;
        Self.DefaultDescription := DefaultDescription;
        Self.DefaultColorStyle := DefaultColorStyle;
        Self.DropDownListStyle := DropDownListStyle;
        Self.ShowDescriptions := ShowDescriptions;
        Self.AllowSelectColor := AllowSelectColor;
        Self.ColorComboStyle := ColorComboStyle;
        Self.MaxMRUColors := MaxMRUColors;
        Self.NamingConvention := NamingConvention;
        Self.PrepareList := PrepareList;
        Self.ColorValueFormat := ColorValueFormat;
        Self.OnSelectCustomColor := OnSelectCustomColor;
        Self.OnNamingConvention := OnNamingConvention;
        Self.OnGetDefaultColor := OnGetDefaultColor;
        Self.OnAddedMRUColor := OnAddedMRUColor;
        Self.OnDeletedMRUColor := OnDeletedMRUColor;
        Self.CustomColors := CustomColors;
        Self.Items := Items;
        Self.MRUColors.Assign(MRUColors);
      end;
    finally
      EndUpdate;
    end
  end
  else
    inherited Assign(Source);
end;

function TcxCustomColorComboBoxProperties.CompareDisplayValues(const AEditValue1, AEditValue2: TcxEditValue): Boolean;
var
  AColor1, AColor2: TColor;
  AColorFound1, AColorFound2: Boolean;
  ADescription1, ADescription2: string;
begin
  GetColorComboBoxDisplayValue(AEditValue1, AColor1, ADescription1, AColorFound1);
  GetColorComboBoxDisplayValue(AEditValue2, AColor2, ADescription2, AColorFound2);

  Result := ShowColorBox(AColorFound1) = ShowColorBox(AColorFound2);
  if Result then
    if ShowColorBox(AColorFound1) then
      Result := (AColor1 = AColor2) and (not ShowDescriptions or InternalCompareString(ADescription1, ADescription2, True))
    else
      Result := not ShowDescriptions or InternalCompareString(ADescription1, ADescription2, True);
end;

class function TcxCustomColorComboBoxProperties.GetContainerClass: TcxContainerClass;
begin
  Result := TcxColorComboBox;
end;

class function TcxCustomColorComboBoxProperties.GetViewInfoClass: TcxContainerViewInfoClass;
begin
  Result := TcxCustomColorComboBoxViewInfo;
end;

function TcxCustomColorComboBoxProperties.GetEditValueSource(AEditFocused: Boolean):
  TcxDataEditValueSource;
begin
  Result := evsValue;
end;

function TcxCustomColorComboBoxProperties.IsDisplayValueValid(
  var DisplayValue: TcxEditValue; AEditFocused: Boolean): Boolean;
begin
  Result := True;
end;

procedure TcxCustomColorComboBoxProperties.PrepareDisplayValue(
  const AEditValue: TcxEditValue; var DisplayValue: TcxEditValue; AEditFocused: Boolean);
var
  AValue: TColor;
  AValueText: string;
begin
  TranslateValues(AEditValue, AValue, AValueText);
  DisplayValue := AValueText;
end;

function TcxCustomColorComboBoxProperties.GetSupportedOperations: TcxEditSupportedOperations;
begin
  Result := [esoEditing, esoFiltering, esoHorzAlignment, esoSorting,
    esoSortingByDisplayText];
  if Buttons.Count > 0 then
    Include(Result, esoHotTrack);
  if ShowDescriptions then
    Include(Result, esoIncSearch);
end;

function TcxCustomColorComboBoxProperties.GetDisplayText(const AEditValue: TcxEditValue;
  AFullText: Boolean = False; AIsInplace: Boolean = True): string;
var
  AColor: TColor;
  ADescription: string;
begin
  TranslateValues(AEditValue, AColor, ADescription, True);
  Result := ADescription;
end;

procedure TcxCustomColorComboBoxProperties.DoUpdate(AProperties: TcxCustomEditProperties);
var
  AColorComboBoxProperties: TcxCustomColorComboBoxProperties;
begin
  AColorComboBoxProperties := AProperties as TcxCustomColorComboBoxProperties;
  AColorComboBoxProperties.Items.Assign(Items);
  AColorComboBoxProperties.CustomColors.Assign(CustomColors);
  AColorComboBoxProperties.MRUColors.Assign(MRUColors);
end;

procedure TcxCustomColorComboBoxProperties.GetColorComboBoxDisplayValue(
  const AEditValue: TcxEditValue; out AColor: TColor; out ADescription: string; out AColorFound: Boolean);
begin
  InternalGetColorComboBoxDisplayValue(IndexByValue(AEditValue), AEditValue,
    AColor, ADescription, AColorFound);
end;

procedure TcxCustomColorComboBoxProperties.PrepareColorList(
  APrepareList: TcxColorPrepareList; ASaveCustom, ASaveMRU: Boolean);
begin
  LockUpdate(True);
  try
    if not ASaveCustom then
      Items.Clear
    else
      Items.ClearNonCustom;
    if not ASaveMRU then
      ClearMRUColors;
    InternalPrepareColorList(APrepareList);
    if ASaveMRU then
      ValidateMRUColors;
    SynchronizeCustomColors;
    if Assigned(FOnLoadColorList) then
      FOnLoadColorList(Self);
  finally
    LockUpdate(False);
  end;
end;

procedure TcxCustomColorComboBoxProperties.PrepareDelphiColorList(const ASaveCustom, ASaveMRU: Boolean);
begin
  PrepareColorList(cxplDelphi, ASaveCustom, ASaveMRU);
end;

procedure TcxCustomColorComboBoxProperties.PrepareHTML4ColorList(const ASaveCustom, ASaveMRU: Boolean);
begin
  PrepareColorList(cxplHTML4, ASaveCustom, ASaveMRU);
end;

procedure TcxCustomColorComboBoxProperties.PrepareX11ColorList(const ASaveCustom, ASaveMRU: Boolean);
begin
  PrepareColorList(cxplX11, ASaveCustom, ASaveMRU);
end;

procedure TcxCustomColorComboBoxProperties.PrepareX11OrderedColorList(const ASaveCustom, ASaveMRU: Boolean);
begin
  PrepareColorList(cxplX11Ordered, ASaveCustom, ASaveMRU);
end;

procedure TcxCustomColorComboBoxProperties.ChangeScale(M: Integer; D: Integer);
begin
  inherited ChangeScale(M, D);
  ColorBoxWidth := MulDiv(ColorBoxWidth, M, D);
end;

procedure TcxCustomColorComboBoxProperties.DefineProperties(Filer: TFiler);
begin
  Filer.DefineProperty('Items', ReadCustomColors, nil, False);
  Filer.DefineProperty('PrepareInfo', ReadPrepareInfo, nil, False);
end;

function TcxCustomColorComboBoxProperties.ShowColorBox(AColorFound: Boolean): Boolean;
begin
  Result := not (not AColorFound and (DefaultColorStyle = cxdcText));
end;

class function TcxCustomColorComboBoxProperties.GetLookupDataClass: TcxInterfacedPersistentClass;
begin
  Result := TcxColorComboBoxLookupData;
end;

class function TcxCustomColorComboBoxProperties.GetPopupWindowClass: TcxCustomEditPopupWindowClass;
begin
  Result := TcxColorComboBoxPopupWindow;
end;

class function TcxCustomColorComboBoxProperties.GetViewDataClass: TcxCustomEditViewDataClass;
begin
  Result := TcxCustomColorComboBoxViewData;
end;

function TcxCustomColorComboBoxProperties.IsEditValueNumeric: Boolean;
begin
  Result := True;
end;

function TcxCustomColorComboBoxProperties.EditValueToColorValue(const AEditValue: Variant): TColor;
begin
  if IsVarEmpty(AEditValue) or not StrToColor(VarToStr(AEditValue), Result) then
    Result := DefaultColor;
end;

function TcxCustomColorComboBoxProperties.GetColorByIndex(AIndex: Integer): TColor;
begin
  if AIndex <= (MRUColors.Count - 1) then
    Result := MRUColors.GetColorByIndex(AIndex, DefaultColor)
  else
    Result := Items.GetColorByIndex(AIndex - MRUColors.Count, DefaultColor);
end;

function TcxCustomColorComboBoxProperties.GetColorByDescription(const ADescription: string; out AColor: TColor): Boolean;
var
  I: Integer;
  ACount: Integer;
begin
  Result := False;
  ACount := Items.Count + MRUColors.Count;
  for I := 0 to ACount - 1 do
  begin
    Result := CompareText(GetDescriptionByIndex(I), ADescription) = 0;
    if Result then
    begin
      AColor := GetColorByIndex(I);
      Break;
    end;
  end;
end;

function TcxCustomColorComboBoxProperties.GetIndexByColor(AColor: TColor): Integer;
begin
  Result := MRUColors.GetIndexByColor(AColor);
  if Result = -1 then
  begin
    Result := Items.GetIndexByColor(AColor);
    if Result <> -1 then
      Result := Result + MRUColors.Count;
  end;
end;

function TcxCustomColorComboBoxProperties.GetDescriptionByIndex(AIndex: Integer): string;
begin
  if AIndex = -1 then
    Result := DefaultDescription
  else
    Result := DoConvertNaming(AIndex);
end;

function TcxCustomColorComboBoxProperties.IndexByValue(const AValue: TcxEditValue): Integer;
var
  AColor: TColor;
  AIsValueValid: Boolean;
  I: Integer;
begin
  Result := -1;
  if IsVarEmpty(AValue) then
    Exit;
  AIsValueValid := StrToColor(VarToStr(AValue), AColor);
  if not AIsValueValid then
    Exit;
  for I := 0 to MRUColors.Count - 1 do
    if AColor = MRUColors[I].Color then
    begin
      Result := I;
      Break;
    end;
  if Result = -1 then
    for I := 0 to Items.Count - 1 do
      if AColor = Items[I].Color then
      begin
        Result := I + MRUColors.Count;
        Break;
      end;
end;

function TcxCustomColorComboBoxProperties.IsDisplayValueNumeric: Boolean;
begin
  Result := False;
end;

function TcxCustomColorComboBoxProperties.StrToColor(const S: string; out AColor: TColor): Boolean;
begin
  Result := (Length(Trim(S)) > 0) and (GetColorByDescription(S, AColor) or cxStrToColor(S, AColor));
end;

function TcxCustomColorComboBoxProperties.AddMRUColor(const AColor: TColor): TcxMRUColorAction;
var
  AIndex: Integer;
begin
  Result := mcaNone;
  if MaxMRUColors = 0 then Exit;
  AIndex := FMRUColors.GetIndexByColor(AColor);
  BeginUpdate;
  try
    if AIndex <> -1 then
    begin
      if (AIndex > 0) and (AIndex < FMRUColors.Count) then
      begin
        Result := mcaMoved;
        FMRUColors.Move(AIndex, 0);
        dxCallNotify(OnAddedMRUColor, Self);
      end
      else
        Result := mcaNone;
    end
    else
      Result := mcaAdded;

    if Result = mcaAdded then
    begin
      AIndex := GetIndexByColor(AColor);
      if AIndex > -1 then
      begin
        FMRUColors.InsertColor(0, AColor, ColorItemByIndex(AIndex).Description);
        DeleteOverMRUColors;
        dxCallNotify(OnAddedMRUColor, Self);
      end
      else
        Result := mcaNone;
    end;
  finally
    EndUpdate(False);
    if Result <> mcaNone then
      Changed;
  end;
end;

function TcxCustomColorComboBoxProperties.DelMRUColor(const AColor: TColor): TcxMRUColorAction;
var
  AIndex: Integer;
begin
  Result := mcaNone;
  AIndex := Items.GetIndexByColor(AColor);
  if AIndex < 0 then
    Exit;

  if FMRUColors.FindColorItem(AColor) <> nil then
  begin
    FMRUColors.Delete(AIndex);
    Result := mcaDeleted;
    dxCallNotify(OnDeletedMRUColor, Self);
  end;
end;

procedure TcxCustomColorComboBoxProperties.ClearMRUColors;
begin
  FMRUColors.Clear;
  Changed;
end;

procedure TcxCustomColorComboBoxProperties.DoGetDefaultColor(var AColor: TColor);
begin
  if Assigned(OnGetDefaultColor) then
    OnGetDefaultColor(Self, AColor);
end;

procedure TcxCustomColorComboBoxProperties.TranslateValues(const AEditValue: TcxEditValue;
  var AColor: TColor; var ADescription: string; ANeedDescription: Boolean = False);
var
  AFoundIndex: Integer;
  AValid: Boolean;
  S: string;
begin
  AFoundIndex := IndexByValue(AEditValue);
  if ((AFoundIndex <> -1) and (not ANeedDescription or ShowDescriptions) or
    ((AFoundIndex = -1) and (ColorComboStyle = cxccsComboList))) and not IsVarEmpty(AEditValue) then
  begin
    ADescription := GetDescriptionByIndex(AFoundIndex);
    AColor := GetColorByIndex(AFoundIndex);
  end
  else
  begin
    AValid := StrToColor(VarToStr(AEditValue), AColor);
    if AValid then
    begin
      S := TcxColorComboBoxHelper.GetColorName(AColor, '', NamingConvention, ColorValueFormat);
      if Assigned(OnNamingConvention) then
        OnNamingConvention(Self, AColor, S);
      ADescription := S;
    end
    else
    begin
      AColor := DefaultColor;
      ADescription := DefaultDescription;
    end;
  end;
end;

function TcxCustomColorComboBoxProperties.ColorItemByIndex(AIndex: Integer): TcxColorComboBoxItem;
begin
  if AIndex = -1 then
    Result := nil
  else
    if AIndex <= MRUColors.Count - 1 then
      Result := MRUColors.Items[AIndex]
    else
      Result := Items[AIndex - MRUColors.Count];
end;

procedure TcxCustomColorComboBoxProperties.DeleteOverMRUColors;
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := FMRUColors.Count - 1 downto 0 do
    begin
      if I >= FMaxMRUColors then
      begin
        FMRUColors.Delete(I);
        dxCallNotify(OnDeletedMRUColor, Self);
      end
      else
        Break;
    end;
  finally
    EndUpdate;
  end;
end;

function TcxCustomColorComboBoxProperties.DoConvertNaming(AIndex: Integer): string;
var
  FItem: TcxColorComboBoxItem;
begin
  FItem := ColorItemByIndex(AIndex);
  if FItem = nil then
    Result := ''
  else
    Result := TcxColorComboBoxHelper.GetColorName(FItem.Color, FItem.Description, NamingConvention, ColorValueFormat);

  if Assigned(OnNamingConvention) then
  begin
    if FItem = nil then
      OnNamingConvention(Self, DefaultColor, Result)
    else
      OnNamingConvention(Self, FItem.Color, Result);
  end;
end;

function TcxCustomColorComboBoxProperties.GetDropDownListStyle: TcxEditDropDownListStyle;
begin
  Result := inherited DropDownListStyle;
end;

function TcxCustomColorComboBoxProperties.GetItems: TcxColorComboBoxItems;
begin
  Result := FItems;
end;

procedure TcxCustomColorComboBoxProperties.InternalGetColorComboBoxDisplayValue(AItemIndex: Integer;
  const AEditValue: TcxEditValue; out AColor: TColor; out ADescription: string; out AColorFound: Boolean);
begin
  AColorFound := not (IsVarEmpty(AEditValue) or VarIsSoftNull(AEditValue));
  ADescription := GetDescriptionByIndex(AItemIndex);
  if not AColorFound and (AItemIndex <> -1) then
  begin
    AColor := GetColorByIndex(AItemIndex);
    AColorFound := True;
  end
  else
  begin
    if (AItemIndex = -1) and (ColorComboStyle = cxccsComboList) and AColorFound then
      AColor := DefaultColor
    else
    begin
      if AItemIndex <> -1 then
        AColor := EditValueToColorValue(AEditValue)
      else
        TranslateValues(AEditValue, AColor, ADescription);
    end;
  end;
end;

procedure TcxCustomColorComboBoxProperties.InternalPrepareColorList(APrepareList: TcxColorPrepareList);
begin
  case APrepareList of
    cxplDelphi:
      InternalPrepareColorList(cxDelphiColorValues, cxDelphiColorNames);
    cxplHTML4:
      InternalPrepareColorList(cxHTML4ColorValues, cxHTML4ColorNames);
    cxplX11:
      InternalPrepareColorList(cxX11ColorValues, cxX11ColorNames);
    cxplX11Ordered:
      InternalPrepareColorList(cxX11OrderedColorValues, cxX11OrderedColorNames);
  end;
end;

procedure TcxCustomColorComboBoxProperties.InternalPrepareColorList(
  AColorValues: array of TColor; AColorNames: array of string);
var
  I: Integer;
  AItem: TcxColorComboBoxItem;
begin
  Items.BeginUpdate;
  try
    for I:= Low(AColorValues) to High(AColorValues) do
    begin
      AItem := Items.AddColor(AColorValues[I], AColorNames[I]);
      if AItem <> nil then
        AItem.FIsCustomColor := False;
    end;
  finally
    Items.CancelUpdate;
  end;
end;

function TcxCustomColorComboBoxProperties.IsDefaultDescriptionStored: Boolean;
begin
  Result := DefaultDescription <> cxGetResourceString(@cxSColorComboBoxDefaultDescription);
end;

procedure TcxCustomColorComboBoxProperties.ReadCustomColors(Reader: TReader);
begin
  Reader.ReadValue;
  Reader.ReadCollection(FCustomColors);
end;

procedure TcxCustomColorComboBoxProperties.ReadPrepareInfo(Reader: TReader);
begin
  Reader.ReadString;
end;

procedure TcxCustomColorComboBoxProperties.SetAllowSelectColor(Value: Boolean);
begin
  if FAllowSelectColor <> Value then
  begin
    FAllowSelectColor := Value;
    BeginUpdate;
    try
      Buttons[1].Visible := Value;
      GlyphButtonIndex := Ord(Value);
      if not AllowSelectColor and (DropDownListStyle = lsEditList) then
        DropDownListStyle := lsFixedList;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TcxCustomColorComboBoxProperties.SetColorBoxAlign(Value : TcxColorBoxAlign);
begin
  if FColorBoxAlign <> Value then
  begin
    FColorBoxAlign := Value;
    Changed;
  end;
end;

procedure TcxCustomColorComboBoxProperties.SetColorBoxFrameColor(Value: TColor);
begin
  if FColorBoxFrameColor <> Value then
  begin
    FColorBoxFrameColor := Value;
    Changed;
  end;
end;

procedure TcxCustomColorComboBoxProperties.SetColorBoxWidth(Value: Integer);
begin
  Value := Max(Value, 0);
  if FColorBoxWidth <> Value then
  begin
    FColorBoxWidth := Value;
    Changed;
  end;
end;

procedure TcxCustomColorComboBoxProperties.SetColorComboStyle(Value: TcxColorComboStyle);
begin
  if FColorComboStyle <> Value then
  begin
    FColorComboStyle := Value;
    Changed;
  end;
end;

procedure TcxCustomColorComboBoxProperties.SetColorValueFormat(Value: TcxColorValueFormat);
begin
  if FColorValueFormat <> Value then
  begin
    FColorValueFormat := Value;
    Changed;
  end;
end;

procedure TcxCustomColorComboBoxProperties.SetDefaultColor(Value: TColor);
begin
  if FDefaultColor <> Value then
  begin
    FDefaultColor := Value;
    Changed;
  end;
end;

procedure TcxCustomColorComboBoxProperties.SetDefaultDescription(Value: string);
begin
  if FDefaultDescription <> Value then
  begin
    FDefaultDescription := Value;
    Changed;
  end;
end;

procedure TcxCustomColorComboBoxProperties.SetDropDownListStyle(AValue: TcxEditDropDownListStyle);
begin
  if DropDownListStyle <> AValue then
  begin
    inherited DropDownListStyle := AValue;
    if DropDownListStyle = lsEditList then
      AllowSelectColor := True;
  end;
end;

procedure TcxCustomColorComboBoxProperties.SetDefaultColorStyle(Value: TcxDefaultColorStyle);
begin
  if FDefaultColorStyle <> Value then
  begin
    FDefaultColorStyle := Value;
    Changed;
  end;
end;

procedure TcxCustomColorComboBoxProperties.SetCustomColors(Value: TcxColorComboBoxItems);
begin
  FCustomColors.Assign(Value);
end;

procedure TcxCustomColorComboBoxProperties.SetItems(const Value: TcxColorComboBoxItems);
begin
  FItems.Assign(Value);
end;

procedure TcxCustomColorComboBoxProperties.SetMaxMRUColors(Value: Byte);
var
  FOldMaxMRUColors: Byte;
begin
  if FMaxMRUColors <> Value then
  begin
    FOldMaxMRUColors := FMaxMRUColors;
    FMaxMRUColors := Value;
    if FOldMaxMRUColors > Value then
      DeleteOverMRUColors;
  end;
end;

procedure TcxCustomColorComboBoxProperties.SetMRUColors(Value: TcxColorComboBoxItems);
begin
  FMRUColors.Assign(Value);
end;

procedure TcxCustomColorComboBoxProperties.SetNamingConvention(Value: TcxColorNamingConvention);
begin
  if FNamingConvention <> Value then
  begin
    FNamingConvention := Value;
    Changed;
  end;
end;

procedure TcxCustomColorComboBoxProperties.SetPrepareList(Value: TcxColorPrepareList);
begin
  if FPrepareList <> Value then
  begin
    FPrepareList := Value;
    PrepareColorList(FPrepareList, True, True);
  end;
end;

procedure TcxCustomColorComboBoxProperties.SetShowDescriptions(const Value: Boolean);
begin
  if FShowDescriptions <> Value then
  begin
    FShowDescriptions := Value;
    Changed;
  end;
end;

procedure TcxCustomColorComboBoxProperties.SynchronizeCustomColors;
var
  I: Integer;
begin
  Items.BeginUpdate;
  try
    Items.ClearCustom;
    for I := CustomColors.Count - 1 downto 0 do
      Items.InsertColor(0, CustomColors[I].Color, CustomColors[I].Description);
  finally
    Items.CancelUpdate;
  end;
end;

procedure TcxCustomColorComboBoxProperties.CustomColorChanged(ASender: TObject);
begin
  SynchronizeCustomColors;
end;

procedure TcxCustomColorComboBoxProperties.ValidateMRUColors;
var
  I: Integer;
begin
  for I := MRUColors.Count - 1 downto 0 do
    if Items.GetIndexByColor(MRUColors[I].Color) = -1 then
      MRUColors.Delete(I);
end;

{ TcxCustomColorComboBox }

destructor TcxCustomColorComboBox.Destroy;
begin
  FreeAndNil(FColorDialogHelper);
  inherited Destroy;
end;

function TcxCustomColorComboBox.Focused: Boolean;
begin
  Result := ColorDialogHelper.DialogIsShown or inherited Focused;
end;

class function TcxCustomColorComboBox.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxCustomColorComboBoxProperties;
end;

function TcxCustomColorComboBox.IsChildWindow(AWnd: THandle): Boolean;
begin
  Result := inherited IsChildWindow(AWnd) or ColorDialogHelper.DialogIsShown and
    IsChildEx(ColorDialogHelper.DialogHandle, AWnd);
end;

procedure TcxCustomColorComboBox.PrepareEditValue(
  const ADisplayValue: TcxEditValue; out EditValue: TcxEditValue; AEditFocused: Boolean);
var
  AColor: TColor;
  AIndex: Integer;
begin
  if ActiveProperties.StrToColor(ADisplayValue, AColor) then
  begin
    AIndex := ActiveProperties.GetIndexByColor(AColor);
    if AIndex = -1 then
      AIndex := DoSelectCustomColor(AColor, False);
  end
  else
    AIndex := ILookupData.CurrentKey;

  EditValue := LookupKeyToEditValue(AIndex);
end;

function TcxCustomColorComboBox.CanSelectItem(AFindSelection: Boolean): Boolean;
begin
  Result := inherited CanSelectItem(AFindSelection) or (ItemIndex <> -1);
end;

function TcxCustomColorComboBox.CreateColorDialogHelper: TcxColorDialogHelper;
begin
  Result := TcxColorComboBoxColorDialogHelper.Create(Self);
end;

procedure TcxCustomColorComboBox.PropertiesChanged(Sender: TObject);
begin
  InitializeInnerEdit;
  inherited;
end;

function TcxCustomColorComboBox.GetColorDialogHelper: TcxColorDialogHelper;
begin
  if FColorDialogHelper = nil then
    FColorDialogHelper := CreateColorDialogHelper;
  Result := FColorDialogHelper;
end;

function TcxCustomColorComboBox.GetColorValue: TColor;
begin
  Result := ActiveProperties.EditValueToColorValue(EditValue);
end;

function TcxCustomColorComboBox.IsColorValueStored: Boolean;
begin
  Result := ColorValue <> ActiveProperties.DefaultColor;
end;

procedure TcxCustomColorComboBox.SetColorValue(Value: TColor);
begin
  EditValue := Value;
end;

procedure TcxCustomColorComboBox.DoButtonClick(AButtonVisibleIndex: Integer);
begin
  inherited DoButtonClick(AButtonVisibleIndex);
  if AButtonVisibleIndex = 1 then
    SelectCustomColor(Self);
end;

function TcxCustomColorComboBox.DoSelectCustomColor(AColor: TColor; ANeedFireEvent: Boolean): Integer;
var
  AColorDescription: string;
begin
  if DoOnSelectCustomColor(AColor, AColorDescription, ANeedFireEvent) then
    ActiveProperties.CustomColors.AddColor(AColor, AColorDescription);
  AddMRUColor(AColor);
  Result := ActiveProperties.GetIndexByColor(AColor);
end;

function TcxCustomColorComboBox.GetInnerEditClass: TControlClass;
begin
  Result := TcxCustomColorComboBoxInnerEdit;
end;

procedure TcxCustomColorComboBox.Initialize;
begin
  inherited Initialize;
  ControlStyle := ControlStyle - [csClickEvents];
  ActiveProperties.FOnLoadColorList := PropertiesLoadColorListHandler;
end;

procedure TcxCustomColorComboBox.InitializeInnerEdit;
begin
  inherited;
  InnerEdit.Control.Visible := ActiveProperties.ShowDescriptions;
end;

procedure TcxCustomColorComboBox.InitializePopupWindow;
begin
  inherited InitializePopupWindow;
  TcxColorComboBoxPopupWindow(PopupWindow).SysPanelStyle := ActiveProperties.PopupSizeable;
end;

procedure TcxCustomColorComboBox.InternalValidateDisplayValue(const ADisplayValue: TcxEditValue);
var
  AColor: TColor;
begin
  if ActiveProperties.StrToColor(ADisplayValue, AColor) then
    DoSelectCustomColor(AColor, False);
  inherited InternalValidateDisplayValue(ADisplayValue);
end;

procedure TcxCustomColorComboBox.ClearEditValue;
begin
  InternalEditValue := Null;
  ModifiedAfterEnter := True;
  SynchronizeDisplayValue;
  if ActiveProperties.ImmediatePost and CanPostEditValue then
    InternalPostEditValue;
end;

function TcxCustomColorComboBox.DoOnSelectCustomColor(
  var AColor: TColor; out AColorDescription: string; ANeedFireEvent: Boolean): Boolean;
begin
  Result := ActiveProperties.GetIndexByColor(AColor) = -1;
  if Result or ANeedFireEvent then
  begin
    AColorDescription := '';
    with Properties do
      if Assigned(OnSelectCustomColor) then
        OnSelectCustomColor(Self, AColor, AColorDescription, Result);
    if RepositoryItem <> nil then
      with ActiveProperties do
        if Assigned(OnSelectCustomColor) then
          OnSelectCustomColor(Self, AColor, AColorDescription, Result);
  end;
end;

procedure TcxCustomColorComboBox.HandleSelectItem(Sender: TObject);
begin
  inherited HandleSelectItem(Sender);
  if not LookupItemsScrolling and DoEditing then
  begin
    AddMRUColor(ColorValue);
    LookupData.InternalSetCurrentKey(ActiveProperties.GetIndexByColor(ColorValue));
    UpdateDrawValue;
  end;
end;

function TcxCustomColorComboBox.NeedResetInvalidTextWhenPropertiesChanged: Boolean;
begin
  Result := False;
end;

procedure TcxCustomColorComboBox.SelectCustomColor(Sender: TObject);
var
  ASelectedColor: TColor;
  AKey: TcxEditValue;
  APrevColor: TColor;
begin
  ASelectedColor := clNone;
  if ActiveProperties.ColorDialogType <> cxcdtCustom then
  begin
    ColorDialogHelper.DialogType := ActiveProperties.ColorDialogType;
    ColorDialogHelper.DialogShowFull := ActiveProperties.ColorDialogShowFull;
    ColorDialogHelper.Color := ColorValue;
    if ColorDialogHelper.Execute then
      ASelectedColor := ColorDialogHelper.Color
    else
    begin
      DoClosePopup(crCancel);
      Exit;
    end;
  end;
  APrevColor := ActiveProperties.GetColorByIndex(LookupData.CurrentKey);
  AKey := DoSelectCustomColor(ASelectedColor, ActiveProperties.ColorDialogType = cxcdtCustom);
  if VarEqualsExact(AKey, LookupData.CurrentKey) and
      (ActiveProperties.GetColorByIndex(LookupData.CurrentKey) <> APrevColor) then
    LookupData.InternalSetCurrentKey(ActiveProperties.GetIndexByColor(APrevColor));
  LookupData.SetCurrentKey(AKey);
  DoClosePopup(crEnter);
end;

function TcxCustomColorComboBox.GetProperties: TcxCustomColorComboBoxProperties;
begin
  Result := TcxCustomColorComboBoxProperties(inherited Properties);
end;

function TcxCustomColorComboBox.GetActiveProperties: TcxCustomColorComboBoxProperties;
begin
  Result := TcxCustomColorComboBoxProperties(InternalGetActiveProperties);
end;

function TcxCustomColorComboBox.GetLookupData: TcxColorComboBoxLookupData;
begin
  Result := TcxColorComboBoxLookupData(FLookupData);
end;

procedure TcxCustomColorComboBox.SetProperties(Value: TcxCustomColorComboBoxProperties);
begin
  Properties.Assign(Value);
end;

function TcxCustomColorComboBox.AddMRUColor(const AColor: TColor): TcxMRUColorAction;
begin
  Result := ActiveProperties.AddMRUColor(AColor);
end;

function TcxCustomColorComboBox.DelMRUColor(const AColor: TColor): TcxMRUColorAction;
begin
  Result := ActiveProperties.DelMRUColor(AColor);
end;

function TcxCustomColorComboBox.LookupKeyToEditValue(const AKey: TcxEditValue): TcxEditValue;
begin
  if not VarEqualsExact(AKey, -1) then
    Result := ActiveProperties.GetColorByIndex(AKey)
  else
    Result := Null;
end;

procedure TcxCustomColorComboBox.DoEditKeyDown(var Key: Word; Shift: TShiftState);
var
  APrevKey: Word;
  AColor: TColor;
begin
  APrevKey := Key;
  inherited DoEditKeyDown(Key, Shift);
  if (APrevKey = VK_RETURN) and not (ssAlt in Shift) and
     (ItemIndex = -1) and ActiveProperties.StrToColor(Text, AColor)
  then
    LookupData.SetCurrentKey(DoSelectCustomColor(AColor, False));
end;

procedure TcxCustomColorComboBox.PropertiesLoadColorListHandler(Sender: TObject);
begin
  if not IsLoading then
    FlushEditValue;
end;

procedure TcxCustomColorComboBox.FlushEditValue;
begin
  LookupData.InternalSetCurrentKey(ActiveProperties.GetIndexByColor(ColorValue));
  ColorValue := ColorValue;
end;

procedure TcxCustomColorComboBox.PrepareColorList(
  APrepareList: TcxColorPrepareList; ASaveCustom, ASaveMRU: Boolean);
var
  FBeforeLoadColor: TColor;
  FNewIndex: Integer;
begin
  FBeforeLoadColor := ColorValue;

  ActiveProperties.FOnLoadColorList := nil;
  ActiveProperties.PrepareColorList(APrepareList, ASaveCustom, ASaveMRU);
  ActiveProperties.FOnLoadColorList := PropertiesLoadColorListHandler;
  FNewIndex := ActiveProperties.Items.GetIndexByColor(FBeforeLoadColor);
  if FNewIndex = -1 then
    FlushEditValue
  else
  begin
    FEditValue := FBeforeLoadColor;
    InternalSetEditValue(FBeforeLoadColor, False);
  end;
end;

procedure TcxCustomColorComboBox.PrepareDelphiColorList(const ASaveCustom, ASaveMRU: Boolean);
begin
  PrepareColorList(cxplDelphi, ASaveCustom, ASaveMRU);
end;

procedure TcxCustomColorComboBox.PrepareHTML4ColorList(const ASaveCustom, ASaveMRU: Boolean);
begin
  PrepareColorList(cxplHTML4, ASaveCustom, ASaveMRU);
end;

procedure TcxCustomColorComboBox.PrepareX11ColorList(const ASaveCustom, ASaveMRU: Boolean);
begin
  PrepareColorList(cxplX11, ASaveCustom, ASaveMRU);
end;

procedure TcxCustomColorComboBox.PrepareX11OrderedColorList(const ASaveCustom, ASaveMRU: Boolean);
begin
  PrepareColorList(cxplX11Ordered, ASaveCustom, ASaveMRU);
end;

{ TcxColorComboBox }

class function TcxColorComboBox.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxColorComboBoxProperties;
end;

function TcxColorComboBox.GetActiveProperties: TcxColorComboBoxProperties;
begin
  Result := TcxColorComboBoxProperties(InternalGetActiveProperties);
end;

function TcxColorComboBox.GetProperties: TcxColorComboBoxProperties;
begin
  Result := TcxColorComboBoxProperties(inherited Properties);
end;

procedure TcxColorComboBox.SetProperties(Value: TcxColorComboBoxProperties);
begin
  Properties.Assign(Value);
end;

{ TcxFilterColorComboBoxHelper }

class function TcxFilterColorComboBoxHelper.GetFilterEditClass: TcxCustomEditClass;
begin
  Result := TcxColorComboBox;
end;

class function TcxFilterColorComboBoxHelper.GetSupportedFilterOperators(
  AProperties: TcxCustomEditProperties; AValueTypeClass: TcxValueTypeClass;
  AExtendedSet: Boolean = False): TcxFilterControlOperators;
begin
  Result := [fcoEqual, fcoNotEqual, fcoBlanks, fcoNonBlanks];
  if AExtendedSet then
    Result := Result + [fcoInList, fcoNotInList];
end;

class procedure TcxFilterColorComboBoxHelper.InitializeProperties(
  AProperties, AEditProperties: TcxCustomEditProperties; AHasButtons: Boolean);
begin
  inherited InitializeProperties(AProperties, AEditProperties, AHasButtons);
  TcxCustomColorComboBoxProperties(AProperties).DropDownListStyle := lsFixedList;
end;

{ TcxColorComboBoxHelper }

class procedure TcxColorComboBoxHelper.DrawColorBox(ACanvas: TcxCanvas; ARect: TRect;
  const AFrameColor, ABoxColor, ABkColor: TColor; const DefaultColorStyle: TcxDefaultColorStyle);
begin
  DrawColorBox(ACanvas, ARect, AFrameColor, ABoxColor, ABkColor, DefaultColorStyle, dxSystemScaleFactor);
end;

class procedure TcxColorComboBoxHelper.DrawColorBox(ACanvas: TcxCanvas; ARect: TRect; const AFrameColor, ABoxColor,
  ABkColor: TColor; const DefaultColorStyle: TcxDefaultColorStyle; AScaleFactor: TdxScaleFactor);
begin
  if not cxRectIsEmpty(ARect) then
  begin
    ACanvas.Brush.Style := bsSolid;
    ACanvas.Brush.Color := ABkColor;
    ACanvas.FrameRect(ARect);
    ARect := cxRectInflate(ARect, AScaleFactor.Apply(-1));
    ACanvas.Brush.Color := AFrameColor;
    ACanvas.FrameRect(ARect);
    InflateRect(ARect, -1, -1);

    case DefaultColorStyle of
      cxdcClear:
        ACanvas.FillRect(ARect, ABkColor);
      cxdcColor, cxdcCustom, cxdcText:
        begin
          if ABoxColor = clDefault then
            ACanvas.Brush.Color := clBlack;
          ACanvas.FillRect(ARect, ABoxColor);
        end;
      cxdcHatched:
        begin
          ACanvas.FillRect(ARect, ABkColor);
          ACanvas.Pen.Color := ABkColor;
          ACanvas.Brush.Color := ABoxColor;
          ACanvas.Brush.Style := bsDiagCross;
          ACanvas.Rectangle(cxRectInflate(ARect, 1, 1));
          ACanvas.Pen.Color := AFrameColor;
          ACanvas.Polyline([Point(ARect.Left -1 , ARect.Top - 1), Point(ARect.Right, ARect.Top - 1),
            Point(ARect.Right, ARect.Bottom), Point(ARect.Left - 1, ARect.Bottom),
            Point(ARect.Left - 1, ARect.Top - 1)]);
        end;
    end;
    ACanvas.Brush.Style := bsSolid;
  end;
end;

class function TcxColorComboBoxHelper.GetColorName(AColor: TColor; const ADescription: string;
  const ANamingConvention: TcxColorNamingConvention; const AColorValueFormat: TcxColorValueFormat): string;

  function GetNameOfUnknownColor: string;
  var
    RGB: Cardinal;
  begin
    RGB := ColorToRGB(AColor);
    case AColorValueFormat of
      cxcvRGB:
        Result := Format('%d.%d.%d', [GetRValue(RGB), GetGValue(RGB), GetBValue(RGB)]);
      cxcvHexadecimal:
        Result := HexDisplayPrefix + IntToHex(RGB, 8);
      else
        Result := IntToStr(AColor);
    end;
  end;

  function DoGetColorName(const AColorValues: array of TColor; const AColorNames: array of string): string;
  var
    I: Integer;
  begin
    for I := Low(AColorValues) to High(AColorValues) do
      if AColorValues[I] = AColor then
      begin
        Result := AColorNames[I];
        Exit;
      end;

    Result := GetNameOfUnknownColor;
  end;

begin
  case ANamingConvention of
    cxncNone:
      Result := ADescription;
    cxncDelphi:
      Result := DoGetColorName(cxDelphiColorValues, cxDelphiColorNames);
    cxncHTML4:
      Result := DoGetColorName(cxHTML4ColorValues, cxHTML4ColorNames);
    cxncX11:
      Result := DoGetColorName(cxX11ColorValues, cxX11ColorNames);
  end;
end;

initialization
  GetRegisteredEditProperties.Register(TcxColorComboBoxProperties, scxSEditRepositoryColorComboBoxItem);
  FilterEditsController.Register(TcxColorComboBoxProperties, TcxFilterColorComboBoxHelper);

finalization
  FilterEditsController.Unregister(TcxColorComboBoxProperties, TcxFilterColorComboBoxHelper);
  GetRegisteredEditProperties.Unregister(TcxColorComboBoxProperties);
end.
