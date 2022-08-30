{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressVerticalGrid                                      }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSVERTICALGRID AND ALL           }
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
unit cxOI;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  TypInfo, RTLConsts,
  dxCore, dxCoreClasses, dxMessages, cxGraphics, cxClasses, cxControls, cxLookAndFeels,
  cxEdit, cxInplaceContainer, cxVGrid;

const
  cxComponentPropertyRowTextColor: TColor = clMaroon;
  cxSubComponentPropertyRowTextColor: TColor = clGreen;

type
  TcxPropertyEditor = class;
  TcxRTTIInspectorController = class;
  TcxCustomRTTIInspector = class;

  { TcxComponentList }

  TcxComponentList = class(TObject)
  private
    FList: TList;
    function GetItem(Index: Integer): TPersistent;
    function GetCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    function Add(Item: TPersistent): Integer;
    function Equals(List: TcxComponentList): Boolean; reintroduce;

    property Count: Integer read GetCount;
    property Items[Index: Integer]: TPersistent read GetItem; default;
  end;

  TcxPropertyAttribute =
    (ipaValueList, ipaSubProperties, ipaDialog, ipaMultiSelect, ipaAutoUpdate,
     ipaSortList, ipaReadOnly, ipaRevertable);
  TcxPropertyAttributes = set of TcxPropertyAttribute;

  TcxInstProp = record
    Instance: TPersistent;
    PropInfo: PPropInfo;
  end;

  PcxInstPropList = ^TcxInstPropList;
  TcxInstPropList = array[0..1023] of TcxInstProp;

  TcxGetPropEditProc = procedure(APropertyEditor: TcxPropertyEditor) of object;

  { TcxPropertyEditor }

  TcxPropertyEditor = class
  private
    FInspector: TcxCustomRTTIInspector;
    FAncestorList: TList;
    FPropCount: Integer;
    FPropList: PcxInstPropList;
    FOwner: TComponent;
    FRoot: TComponent;
    FAncestor: TPersistent;
    FRootAncestor: TComponent;
    FLookingFor: TComponent;
    FDoneLooking: Boolean;
    procedure AddAncestor(Component: TComponent);
    procedure GetLookupInfo(var Ancestor: TPersistent;
      var Root, LookupRoot, RootAncestor: TComponent);
    procedure SetPropEntry(Index: Integer; AInstance: TPersistent;
      APropInfo: PPropInfo);
    procedure WriteComponentSimulation(Component: TComponent);
  protected
    procedure AdjustInnerEditProperties(AProperties: TcxCustomEditProperties); virtual;
    function GetFloatValue: Extended;
    function GetFloatValueAt(Index: Integer): Extended;
    function GetInt64Value: Int64;
    function GetInt64ValueAt(Index: Integer): Int64;
    function GetOrdValue: TdxNativeInt;
    function GetOrdValueAt(Index: Integer): TdxNativeInt;
    function GetPropInfo: PPropInfo;
    function GetStrValue: string;
    function GetStrValueAt(Index: Integer): string;
    function GetVarValue: Variant;
    function GetVarValueAt(Index: Integer): Variant;
    function FindRoot: TComponent;
    procedure PostChangedNotification;
    procedure SetFloatValue(Value: Extended);
    procedure SetInt64Value(Value: Int64);
    procedure SetOrdValue(const Value: TdxNativeInt);
    procedure SetStrValue(const Value: string);
    procedure SetVarValue(const Value: Variant);

    property Inspector: TcxCustomRTTIInspector read FInspector;
    property PropList: PcxInstPropList read FPropList;
  public
    constructor Create(AOwner: TComponent; AInspector: TcxCustomRTTIInspector;
      APropCount: Integer);
    destructor Destroy; override;

    function AllEqual: Boolean; virtual;
    function AutoFill: Boolean; virtual;
    procedure Edit; virtual;
    function GetAttributes: TcxPropertyAttributes; virtual;
    function GetComponent(Index: Integer): TPersistent;
    function GetEditLimit: Integer; virtual;
    function GetName: string; virtual;
    procedure GetProperties(AOwner: TComponent; Proc: TcxGetPropEditProc); virtual;
    function GetPropType: PTypeInfo;
    function GetValue: string; virtual;
    procedure GetValues(Proc: TGetStrProc); virtual;
    procedure SetValue(const Value: string); virtual;
    function IsDefaultValue: Boolean; virtual;
    function ValueAvailable: Boolean;

    property PropCount: Integer read FPropCount;
    property Value: string read GetValue write SetValue;
  end;

  TcxPropertyEditorClass = class of TcxPropertyEditor;

  { TcxOrdinalProperty }

  TcxOrdinalProperty = class(TcxPropertyEditor)
    function AllEqual: Boolean; override;
    function GetEditLimit: Integer; override;
  end;

  { TcxIntegerProperty }

  TcxIntegerProperty = class(TcxOrdinalProperty)
  public
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

  { TcxCharProperty }

  TcxCharProperty = class(TcxOrdinalProperty)
  public
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

  { TcxEnumProperty }

  TcxEnumProperty = class(TcxOrdinalProperty)
  public
    function GetAttributes: TcxPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

  { TcxBoolProperty }

  TcxBoolProperty = class(TcxEnumProperty)
  end deprecated;

  { TcxFloatProperty }

  TcxFloatProperty = class(TcxPropertyEditor)
  public
    function AllEqual: Boolean; override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

  { TInt64Property }

  TcxInt64Property = class(TcxPropertyEditor)
  public
    function AllEqual: Boolean; override;
    function GetEditLimit: Integer; override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

  { TcxStringProperty}

  TcxStringProperty = class(TcxPropertyEditor)
  public
    function AllEqual: Boolean; override;
    function GetEditLimit: Integer; override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

  { TcxSetElementProperty }

  TcxSetElementProperty = class(TcxPropertyEditor)
  private
    FElement: Integer;
  protected
    constructor Create(APropList: PcxInstPropList; APropCount: Integer; AElement: Integer); reintroduce;
    property Element: Integer read FElement;
  public
    destructor Destroy; override;
    function AllEqual: Boolean; override;
    function GetAttributes: TcxPropertyAttributes; override;
    function GetName: string; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
    function IsDefaultValue: Boolean; override;
  end;

  { TcxSetProperty }

  TcxSetProperty = class(TcxOrdinalProperty)
  public
    function GetAttributes: TcxPropertyAttributes; override;
    procedure GetProperties(AOwner: TComponent; Proc: TcxGetPropEditProc); override;
    function GetValue: string; override;
  end;

  { TcxClassProperty }

  TcxClassProperty = class(TcxPropertyEditor)
  public
    function GetAttributes: TcxPropertyAttributes; override;
    procedure GetProperties(AOwner: TComponent; Proc: TcxGetPropEditProc); override;
    function GetValue: string; override;
  end;

  { TcxComponentProperty }

  TcxComponentProperty = class(TcxPropertyEditor)
  private
    function GetFullName(AComponent: TComponent): string;
  protected
    function GetComponentReference: TComponent; virtual;
    function IsValidComponent(AComponent: TComponent): Boolean; virtual;
  public
    function GetAttributes: TcxPropertyAttributes; override;
    procedure GetProperties(AOwner: TComponent; Proc: TcxGetPropEditProc); override;
    function GetEditLimit: Integer; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

  { TcxComponentNameProperty }

  TcxComponentNameProperty = class(TcxStringProperty)
  public
    function GetAttributes: TcxPropertyAttributes; override;
    function GetEditLimit: Integer; override;
  end;

  { TcxFontNameProperty }

  TcxFontNameProperty = class(TcxStringProperty)
  public
    function GetAttributes: TcxPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  { TcxFontCharsetProperty }

  TcxFontCharsetProperty = class(TcxIntegerProperty)
  public
    function GetAttributes: TcxPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

  { TcxImeNameProperty }

  TcxImeNameProperty = class(TcxStringProperty)
  public
    function GetAttributes: TcxPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  { TColorProperty }

  TcxColorProperty = class(TcxIntegerProperty)
  public
    procedure Edit; override;
    function GetAttributes: TcxPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  { TcxCursorProperty }

  TcxCursorProperty = class(TcxIntegerProperty)
  public
    function GetAttributes: TcxPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

  { TcxFontProperty }

  TcxFontProperty = class(TcxClassProperty)
  public
    procedure Edit; override;
    function GetAttributes: TcxPropertyAttributes; override;
  end;

  { TcxStringsProperty }

  TcxStringsProperty = class(TcxClassProperty)
  public
    procedure Edit; override;
    function GetAttributes: TcxPropertyAttributes; override;
  end;

  { TcxGraphicProperty }

  TcxGraphicProperty = class(TcxClassProperty)
  private
    function HasGraphic: Boolean;
  protected
    function GetGraphic: TGraphic; virtual;
    procedure SetGraphic(Value: TGraphic); virtual;

    function GetClipboardFormat: Word; virtual;
    function GetGraphicFilter: string; virtual;
    function GraphicClass: TGraphicClass;
  public
    procedure Edit; override;
    function GetAttributes: TcxPropertyAttributes; override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

  { TdxSmartGlyphProperty }

  TdxSmartGlyphProperty = class(TcxGraphicProperty)
  public
    function GetAttributes: TcxPropertyAttributes; override;
  end;

  { TcxPictureProperty }

  TcxPictureProperty = class(TcxGraphicProperty)
  protected
    function GetGraphic: TGraphic; override;
    procedure SetGraphic(Value: TGraphic); override;
  end;

  { TcxModalResultProperty }

  TcxModalResultProperty = class(TcxIntegerProperty)
  public
    function GetAttributes: TcxPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

  { TcxShortCutProperty }

  TcxShortCutProperty = class(TcxOrdinalProperty)
  public
    function GetAttributes: TcxPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

  { TcxMPFilenameProperty }

  TcxMPFilenameProperty = class(TcxStringProperty)
  public
    procedure Edit; override;
    function GetAttributes: TcxPropertyAttributes; override;
  end;

  { TcxTabOrderProperty }

  TcxTabOrderProperty = class(TcxIntegerProperty)
  public
    function GetAttributes: TcxPropertyAttributes; override;
  end;

  { TCaptionProperty }

  TcxCaptionProperty = class(TcxStringProperty)
  public
    function GetAttributes: TcxPropertyAttributes; override;
  end;

  { TcxDateProperty }

  TcxDateProperty = class(TcxPropertyEditor)
  public
    function GetAttributes: TcxPropertyAttributes; override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

  { TcxTimeProperty }

  TcxTimeProperty = class(TcxPropertyEditor)
  public
    function GetAttributes: TcxPropertyAttributes; override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

  { TcxDateTimeProperty }

  TcxDateTimeProperty = class(TcxPropertyEditor)
  public
    function GetAttributes: TcxPropertyAttributes; override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

  { TcxVariantProperty }

  TcxVariantProperty = class(TcxPropertyEditor)
    function GetAttributes: TcxPropertyAttributes; override;
    procedure GetProperties(AOwner: TComponent; Proc: TcxGetPropEditProc); override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

  // the support DevExpress editor's properties

  { TcxEditPropertiesProperty }

  TcxEditPropertiesProperty = class(TcxClassProperty)
  protected
    function HasSubProperties: Boolean; virtual;
  public
    function GetAttributes: TcxPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

  { TcxSkinNameProperty }

  TcxSkinNameProperty = class(TcxStringProperty)
  public
    function GetAttributes: TcxPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  EcxPropertyError = class(EdxException);

  { TcxPropertyRow }

  TcxPropertyRow = class(TcxEditorRow)
  private
    FIsDefaultValue: Boolean;
    FPropertyEditor: TcxPropertyEditor;
  public
    property IsDefaultValue: Boolean read FIsDefaultValue;
    property PropertyEditor: TcxPropertyEditor read FPropertyEditor;
  end;

  { TcxRTTIInspectorEditingController }

  TcxRTTIInspectorEditingController = class(TcxEditingController)
  private
    FDeactivating: Boolean;
    function GetController: TcxRTTIInspectorController;
    function GetInspector: TcxCustomRTTIInspector;
  protected
    procedure DoHideEdit(Accept: Boolean); override;

    property Controller: TcxRTTIInspectorController read GetController;
    property Deactivating: Boolean read FDeactivating;
    property Inspector: TcxCustomRTTIInspector read GetInspector;
  end;

  { TcxRTTIInspectorController }

  TcxRTTIInspectorController = class(TcxvgController)
  private
    FFocusChanging: Boolean;
    FNeedValidate: Boolean;
    function GetEditingController: TcxRTTIInspectorEditingController;
    function GetInspector: TcxCustomRTTIInspector;
    function GetRowAbsoluteIndex(ARow: TcxCustomRow): Integer;
    function GetRowAbsoluteIndexFromCellEdit(Value: TcxCustomInplaceEditContainer): Integer;
    function GetRowPropertyPathFromCellEdit(Value: TcxCustomInplaceEditContainer): string;
  protected
    procedure BeforeEditKeyDown(var Key: Word; var Shift: TShiftState); override;
    procedure DoEditDblClick(Sender: TObject); override;
    procedure DoUpdateRowAndCell(ANewRow: TcxCustomRow; ANewCellIndex: Integer); override;
    procedure FocusChanged; override;
    function IsKeyForController(AKey: Word; AShift: TShiftState): Boolean; override;
    procedure PostValidateFocusedItem;
    procedure SetFocusedItem(Value: TcxCustomInplaceEditContainer); override;
    procedure SetFocusedRowAndCell(Value: TcxCustomRow; ACellIndex: Integer); override;
    procedure ValidateFocusingItem(var AItem: TcxCustomInplaceEditContainer);
  public
    procedure SetFocusedRecordItem(ARecordIndex: TdxNativeInt;
      AItem: TcxCustomInplaceEditContainer); override;
    property EditingController: TcxRTTIInspectorEditingController read GetEditingController;
    property Inspector: TcxCustomRTTIInspector read GetInspector;
  end;

  { TcxRTTIInspectorOptionsView }

  TcxRTTIInspectorOptionsView = class(TcxvgOptionsView)
  strict private
    FShowReadOnlyProperties: Boolean;
    FSorted: Boolean;

    procedure SetSorted(const Value: Boolean);
    procedure SetShowReadOnlyProperties(const Value: Boolean);
    function GetInspector: TcxCustomRTTIInspector;
  protected
    function GetDefaultPaintStyle: TcxvgPaintStyle; override;
    function GetDefaultShowEditButtons: TcxEditingControlEditShowButtons; override;
  public
    constructor Create(AOwner: TPersistent); override;
    procedure Assign(Source: TPersistent); override;
    property Inspector: TcxCustomRTTIInspector read GetInspector;
  published
    property ShowReadOnlyProperties: Boolean read FShowReadOnlyProperties write SetShowReadOnlyProperties default False;
    property Sorted: Boolean read FSorted write SetSorted default True;
  end;

  { TcxRTTIInspectorOptionsBehavior }

  TcxRTTIInspectorOptionsBehavior = class(TcxvgOptionsBehavior)
  public
    constructor Create(AOwner: TPersistent); override;
  published
    property AlwaysShowEditor default True;
  end;

  { TcxRTTIInspectorStyles }

  TcxRTTIInspectorStyles = class(TcxVerticalGridStyles)
  protected
    procedure GetDefaultViewParams(Index: Integer; AData: TObject; out AParams: TcxViewParams); override;
  end;

  { TcxCustomRTTIInspector }

  IcxRTTIInspectorHelper = interface
  ['{EA7182FA-139D-4525-9C5F-4D8BBAB5FEEE}']
    procedure CloseNonModal(AInspector: TcxCustomRTTIInspector);
    procedure PropertyChanged(AInspector: TcxCustomRTTIInspector);
  end;

  TcxFilterPropertyEvent = procedure(Sender: TObject; const PropertyName: string; var Accept: Boolean) of object;

  TcxCustomRTTIInspector = class(TcxUnboundVerticalGrid)
  private
    FBoldFont: TFont;
    FCurrentRow: TcxPropertyRow;
    FListeners: TList;
    FLockRefresh: Boolean;
    FInspectedLevel: Integer;
    FInspectedObject: TPersistent;
    FParentRow: TcxCustomRow;
    FSettingValue: Boolean;

    FOnFilterProperty: TcxFilterPropertyEvent;
    FOnFilterPropertyEx: TcxFilterPropertyEvent;
    FOnPropertyChanged: TNotifyEvent;

    function CanInvokePropertyEditorDlg: Boolean;
    procedure CNPropertyChanged(var AMessage: TMsg); message DXM_VG_PROPERTYCHANGED;
    procedure CreatePropertyRows(AOldInspectedObject: TPersistent);
    procedure CreateRows(APropertyEditor: TcxPropertyEditor);
    procedure GetComponentsProperties(const AInstances: array of TPersistent);
    function GetController: TcxRTTIInspectorController;
    function GetOptionsView: TcxRTTIInspectorOptionsView;
    function GetPropertyEditor: TcxPropertyEditor;
    procedure GetStrProc(const S: string);
    procedure ReleaseComponentProperties;
    procedure RowButtonClick(Sender: TObject; AbsoluteIndex: Integer);
    procedure SetInspectedObject(Value: TPersistent);
    procedure SetOptionsView(const Value: TcxRTTIInspectorOptionsView);
    procedure TryInvokePropertyEditorDlg;
    function TrySetValue(AEdit: TcxCustomEdit; AUseText: Boolean): Boolean;
  protected
    //override VCL
    procedure FontChanged; override;
    function InternalMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    function IsMouseWheelHandleNeeded(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure DataChanged; override;
    procedure DoPropertyChanged;
    procedure EditChange(Sender: TObject);
    procedure EditValueChanged(Sender: TObject);
    function GetControllerClass: TcxCustomControlControllerClass; override;
    function GetControlStylesClass: TcxCustomControlStylesClass; override;
    function GetEditingControllerClass: TcxEditingControllerClass; override;
    function GetOptionsBehaviorClass: TcxControlOptionsBehaviorClass; override;
    function GetOptionsViewClass: TcxControlOptionsViewClass; override;
    procedure LookAndFeelChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues); override;

    procedure BeginLevel;
    procedure EndLevel;
    function FilterProperty(const APropertyName: string): Boolean; virtual;
    function FilterPropertyEx(const AFullPropertyName: string): Boolean; virtual;
    function FindRowByPropertyName(const APropertyName: string): TcxPropertyRow;
    function FindRowByPropertyPath(APropertyPath: string; AExactMatch: Boolean): TcxPropertyRow;
    procedure FocusRowByPropertyName(const APropertyName: string);
    procedure FocusRowByPropertyPath(APropertyPath: string);
    function GetEditPropertiesClass(APropertyEditor: TcxPropertyEditor): TcxCustomEditPropertiesClass; virtual;
    function GetEditPropertiesClassCore(APropertyEditor: TcxPropertyEditor): TcxCustomEditPropertiesClass; virtual;
    function GetRowPropertyPath(ARow: TcxCustomRow): string;
    procedure PostChangedNotification;
    procedure PrepareEditProperties(AProperties: TcxCustomEditProperties; APropertyEditor: TcxPropertyEditor); virtual;
    function Sorted: Boolean;

    property Controller: TcxRTTIInspectorController read GetController;
    property InspectedLevel: Integer read FInspectedLevel;
    property LockRefresh: Boolean read FLockRefresh;
    property PropertyEditor: TcxPropertyEditor read GetPropertyEditor;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddListener(AListener: TPersistent);
    procedure CloseNonModalEditors;
    procedure RefreshInspectedProperties;
    procedure RemoveListener(AListener: TPersistent);

    property OptionsView: TcxRTTIInspectorOptionsView read GetOptionsView write SetOptionsView;
    property InspectedObject: TPersistent read FInspectedObject write SetInspectedObject;
    property OnFilterProperty: TcxFilterPropertyEvent read FOnFilterProperty write FOnFilterProperty;
    property OnFilterPropertyEx: TcxFilterPropertyEvent read FOnFilterPropertyEx write FOnFilterPropertyEx;
    property OnPropertyChanged: TNotifyEvent read FOnPropertyChanged write FOnPropertyChanged;
  end;

  { TcxRTTIInspector }

  TcxRTTIInspector = class(TcxCustomRTTIInspector)
  published
    property Align;
    property Anchors;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property Images;
    property InspectedObject;
    property LayoutStyle;
    property LookAndFeel;
    property OptionsView; //before OptionsBehavior
    property OptionsBehavior;
    property OptionsData;
    property ParentFont;
    property PopupMenu;
    property Styles;
    property TabOrder;
    property TabStop;
    property Visible;

    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawBackground;
    property OnDrawRowHeader;
    property OnDrawValue;
    property OnEdited;
    property OnEditing;
    property OnEditValueChanged;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnFilterProperty;
    property OnFilterPropertyEx;
    property OnItemChanged;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnLayoutChanged;
    property OnLeftVisibleBandIndexChanged;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnPropertyChanged;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnTopRowIndexChanged;
  end;

procedure cxRegisterPropertyEditor(APropertyType: PTypeInfo; AComponentClass: TClass;
  const APropertyName: string; AEditorClass: TcxPropertyEditorClass);

type
  TcxPropertyMapperFunc = function(Obj: TPersistent; PropInfo: PPropInfo): TcxPropertyEditorClass;

procedure cxRegisterPropertyMapper(AMapper: TcxPropertyMapperFunc);
procedure cxGetComponentProperties(AOwner: TComponent;
  AInspector: TcxCustomRTTIInspector; AComponents: TcxComponentList;
  AFilter: TTypeKinds; AProc: TcxGetPropEditProc);

function cxGetPropertiesClassByEditor(APropertyEditor: TcxPropertyEditor): TcxCustomEditPropertiesClass;
procedure cxRegisterEditPropertiesClass(AEditorClass: TcxPropertyEditorClass; AEditPropertiesClass: TcxCustomEditPropertiesClass);
function HasProperty(AClass: TClass; const APropertyName: string): Boolean;
function IsUniquePropertyRelativeTo(AClass, ARelativeClass: TClass; const APropertyName: string): Boolean;
function IsUniquePropertyRelativeParent(AClass: TClass; const APropertyName: string): Boolean;

function IsValidInspectedObject(AObject: TPersistent; AInspector: TcxCustomRTTIInspector): Boolean;

procedure cxDotNetInspectObject(AObject: TPersistent; AInspector: TcxRTTIInspector);

resourcestring
  SInvalidInteger = '''%s'' is not a valid integer value.';

implementation

uses
  Menus, Clipbrd, Dialogs, Consts, Registry, MPlayer, Variants,
  cxFormats, cxContainer, cxCustomData, cxGeometry, cxLookAndFeelPainters, cxDateUtils,
  cxTextEdit, cxDropDownEdit, cxCalendar, cxSpinEdit, cxTimeEdit, cxButtonEdit,
  cxColorComboBox, cxOIStringsEd, cxOIPictureEd, cxOICollectionEd, cxVGridConsts, dxGDIPlusClasses;

const
  MaxNestedComponentLevel = 3;
  cxSString = 'String';
  cxSNull = '(Null)';
  cxSUnassigned = '(Unassigned)';

const
  dxModalResults: array[mrNone..mrYesToAll] of string = (
    'mrNone',
    'mrOk',
    'mrCancel',
    'mrAbort',
    'mrRetry',
    'mrIgnore',
    'mrYes',
    'mrNo',
  {$IFDEF DELPHI16}
    'mrClose',
    'mrHelp',
    'mrTryAgain',
    'mrContinue',
  {$ENDIF}
    'mrAll',
    'mrNoToAll',
    'mrYesToAll');

type
  TcxCustomEditAccess = class(TcxCustomEdit);
  TcxCustomRowAccess = class(TcxCustomRow);
  TcxCustomEditPropertiesAccess = class(TcxCustomEditProperties);
  TcxEditCellViewInfoAccess = class(TcxEditCellViewInfo);
  TcxColorComboBoxPropertiesAccess = class(TcxColorComboBoxProperties);

  TcxIntegerSet = set of 0..SizeOf(Integer) * 8 - 1;

  TcxPropertyClassRec = class
  public
    Group: Integer;
    PropertyType: PTypeInfo;
    PropertyName: string;
    ComponentClass: TClass;
    EditorClass: TcxPropertyEditorClass;
  end;

  TcxPropertyMapperRec = class
  public
    Group: Integer;
    Mapper: TcxPropertyMapperFunc;
  end;

  TcxEditPropertiesMapperRec = class
  public
    EditorClass: TcxPropertyEditorClass;
    EditPropertiesClass: TcxCustomEditPropertiesClass;
  end;

  TcxInspectedObjectPropertyEditor = class(TcxComponentProperty)
  protected
    function IsValidComponent(AComponent: TComponent): Boolean; override;
  end;

const
  cxPropClassMap: array[TTypeKind] of TcxPropertyEditorClass = (
    nil,                // tkUnknown
    TcxIntegerProperty, // tkInteger
    TcxCharProperty,    // tkChar
    TcxEnumProperty,    // tkEnumeration
    TcxFloatProperty,   // tkFloat
    TcxStringProperty,  // tkString
    TcxSetProperty,     // tkSet
    TcxClassProperty,   // tkClass
    nil,                // tkMethod
    TcxPropertyEditor,  // tkWChar
    TcxStringProperty,  // tkLString
    TcxStringProperty,  // tkWString
    TcxPropertyEditor,  // tkVariant
    nil,                // tkArray
    nil,                // tkRecord
    nil,                // tkInterface
    TcxInt64Property,   // tkInt64
    nil                 // tkDynArray
  , TcxStringProperty   // tkUString
  , nil,                // tkClassRef
    nil,                // tkPointer
    nil                 // tkProcedure
{$IFDEF DELPHI103}
  , nil                 // tkMRecord
{$ENDIF}
  );

var
  FPropertyClasses: TList = nil;
  FPropertyMappers: TList = nil;
  FEditPropertiesClasses: TList = nil;

type
  { TcxPropertyComboBoxLookupData }

  TcxPropertyComboBoxLookupData = class(TcxComboBoxLookupData)
  public
    procedure Initialize(AVisualControlsParent: TWinControl); override;
  end;

  { TcxPropertyComboBoxProperties }

  TcxPropertyComboBoxProperties = class(TcxComboBoxProperties)
  public
    class function GetContainerClass: TcxContainerClass; override;
    class function GetLookupDataClass: TcxInterfacedPersistentClass; override;
  end;

  { TcxPropertyComboBox }

  TcxPropertyComboBox = class(TcxComboBox)
  protected
    function SupportsSpelling: Boolean; override;
  public
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
  end;

{ TcxPropertyComboBoxLookupData }

procedure TcxPropertyComboBoxLookupData.Initialize(AVisualControlsParent: TWinControl);
begin
  inherited Initialize(AVisualControlsParent);
  ListContainer.Styles[csNormal].Font.Style := [];
end;

{ TcxPropertyComboBoxProperties }

class function TcxPropertyComboBoxProperties.GetContainerClass: TcxContainerClass;
begin
  Result := TcxPropertyComboBox;
end;

class function TcxPropertyComboBoxProperties.GetLookupDataClass: TcxInterfacedPersistentClass;
begin
  Result := TcxPropertyComboBoxLookupData;
end;

{ TcxPropertyComboBox }

class function TcxPropertyComboBox.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxPropertyComboBoxProperties;
end;

function TcxPropertyComboBox.SupportsSpelling: Boolean;
begin
  Result := False;
end;

{ RTTI routines }

function HasProperty(AClass: TClass; const APropertyName: string): Boolean;
var
  TypeKinds: TTypeKinds;
  PropCount: Integer;
  PropList: PPropList;
  I: Integer;
begin
  TypeKinds := tkProperties;
  PropCount := GetPropList(AClass.ClassInfo, TypeKinds, nil);
  if PropCount > 0 then
  begin
    PropList := AllocMem(PropCount * SizeOf(PPropInfo));
    try
      PropCount := GetPropList(AClass.ClassInfo, TypeKinds, PropList);
      I := 0;
      while (I < PropCount) and (CompareText(dxShortStringToString(PropList^[I].Name), APropertyName) <> 0) do
        Inc(I);
      Result := I < PropCount;
    finally
      FreeMem(PropList, PropCount * SizeOf(PPropInfo));
    end;
  end
  else
    Result := False;
end;

function IsUniquePropertyRelativeTo(AClass, ARelativeClass: TClass; const APropertyName: string): Boolean;
begin
  Result := HasProperty(AClass, APropertyName) and
    ((ARelativeClass = nil) or not HasProperty(ARelativeClass, APropertyName));
end;

function IsUniquePropertyRelativeParent(AClass: TClass; const APropertyName: string): Boolean;
begin
  Result := IsUniquePropertyRelativeTo(AClass, AClass.ClassParent, APropertyName);
end;

function IsValidInspectedObject(AObject: TPersistent; AInspector: TcxCustomRTTIInspector): Boolean;
begin
  Result := AObject <> AInspector;
  if Result and (AObject is TcxCustomRTTIInspector) then
    Result := IsValidInspectedObject(TcxCustomRTTIInspector(AObject).InspectedObject, AInspector);
end;

{ TComponentList }

constructor TcxComponentList.Create;
begin
  inherited Create;
  FList := TList.Create;
end;

destructor TcxComponentList.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

function TcxComponentList.GetItem(Index: Integer): TPersistent;
begin
  Result := FList[Index];
end;

function TcxComponentList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TcxComponentList.Add(Item: TPersistent): Integer;
begin
  Result := FList.Add(Item);
end;

function TcxComponentList.Equals(List: TcxComponentList): Boolean;
var
  I: Integer;
begin
  Result := False;
  if List.Count <> FList.Count then Exit;
  for I := 0 to List.Count - 1 do
    if List[I] <> FList[I] then Exit;
  Result := True;
end;

function cxGetPropertiesClassByEditor(APropertyEditor: TcxPropertyEditor): TcxCustomEditPropertiesClass;
var
  I: Integer;
  Item: TcxEditPropertiesMapperRec;
begin
  Result := nil;
  if FEditPropertiesClasses <> nil then
    for I := 0 to FEditPropertiesClasses.Count - 1 do
    begin
      Item := TcxEditPropertiesMapperRec(FEditPropertiesClasses[I]);
      if Item.EditorClass.InheritsFrom(APropertyEditor.ClassType) then
        Result := Item.EditPropertiesClass;
      if Item.EditorClass = APropertyEditor.ClassType then
        Exit;
    end;
end;

procedure cxRegisterEditPropertiesClass(
  AEditorClass: TcxPropertyEditorClass; AEditPropertiesClass: TcxCustomEditPropertiesClass);
var
  Item: TcxEditPropertiesMapperRec;
begin
  if FEditPropertiesClasses = nil then
    FEditPropertiesClasses := TList.Create;
  Item := TcxEditPropertiesMapperRec.Create;
  Item.EditorClass := AEditorClass;
  Item.EditPropertiesClass := AEditPropertiesClass;
  FEditPropertiesClasses.Insert(0, Item);
end;

procedure ListFreeAndNil(var List: TList);
var
  I: Integer;
begin
  if List <> nil then
  begin
    for I := 0 to List.Count - 1 do
      TObject(List[I]).Free;
    List.Free;
    List := nil;
  end;
end;

{ TcxPropertyEditor }

constructor TcxPropertyEditor.Create(AOwner: TComponent;
  AInspector: TcxCustomRTTIInspector; APropCount: Integer);
begin
  GetMem(FPropList, APropCount * SizeOf(TcxInstProp));
  FInspector := AInspector;
  FPropCount := APropCount;
  FOwner := AOwner;
end;

destructor TcxPropertyEditor.Destroy;
begin
  if FPropList <> nil then
    FreeMem(FPropList, FPropCount * SizeOf(TcxInstProp));
  inherited Destroy;
end;

function TcxPropertyEditor.AllEqual: Boolean;
begin
  Result := FPropCount = 1;
end;

function TcxPropertyEditor.AutoFill: Boolean;
begin
  Result := Assigned(GetPropInfo^.SetProc);
end;

procedure TcxPropertyEditor.Edit;
type
  TcxGetStrFunc = function(const Value: string): Integer of object;
var
  I: Integer;
  Values: TStringList;
  AddValue: TcxGetStrFunc;
begin
  if not AutoFill then Exit;
  Values := TStringList.Create;
  Values.Sorted := ipaSortList in GetAttributes;
  try
    AddValue := Values.Add;
    GetValues(TGetStrProc(AddValue));
    if Values.Count > 0 then
    begin
      I := Values.IndexOf(Value) + 1;
      if I = Values.Count then I := 0;
      Value := Values[I];
    end;
  finally
    Values.Free;
  end;
end;

function TcxPropertyEditor.GetAttributes: TcxPropertyAttributes;
begin
  Result := [ipaMultiSelect, ipaRevertable];
  if FPropList^[0].PropInfo.SetProc = nil then
    Include(Result, ipaReadOnly);
end;

function TcxPropertyEditor.GetComponent(Index: Integer): TPersistent;
begin
  Result := FPropList^[Index].Instance;
end;

function TcxPropertyEditor.GetFloatValue: Extended;
begin
  Result := GetFloatValueAt(0);
end;

function TcxPropertyEditor.GetFloatValueAt(Index: Integer): Extended;
begin
  with FPropList^[Index] do
    Result := GetFloatProp(Instance, PropInfo);
end;

function TcxPropertyEditor.GetInt64Value: Int64;
begin
  Result := GetInt64ValueAt(0);
end;

function TcxPropertyEditor.GetInt64ValueAt(Index: Integer): Int64;
begin
  with FPropList^[Index] do
    Result := GetInt64Prop(Instance, PropInfo);
end;

function TcxPropertyEditor.GetEditLimit: Integer;
begin
  Result := 2047;
end;

function TcxPropertyEditor.GetName: string;
begin
  Result := dxShortStringToString(FPropList^[0].PropInfo^.Name);
end;

function TcxPropertyEditor.GetOrdValue: TdxNativeInt;
begin
  Result := GetOrdValueAt(0);
end;

function TcxPropertyEditor.GetOrdValueAt(Index: Integer): TdxNativeInt;
begin
  with FPropList^[Index] do
    Result := GetOrdProp(Instance, PropInfo);
end;

procedure TcxPropertyEditor.GetProperties(AOwner: TComponent; Proc: TcxGetPropEditProc);
begin
end;

procedure TcxPropertyEditor.AdjustInnerEditProperties(
  AProperties: TcxCustomEditProperties);
begin
  AProperties.ReadOnly := ipaReadOnly in GetAttributes;
end;

function TcxPropertyEditor.GetPropInfo: PPropInfo;
begin
  Result := FPropList^[0].PropInfo;
end;

function TcxPropertyEditor.GetPropType: PTypeInfo;
begin
  Result := FPropList^[0].PropInfo^.PropType^;
end;

function TcxPropertyEditor.GetStrValue: string;
begin
  Result := GetStrValueAt(0);
end;

function TcxPropertyEditor.GetStrValueAt(Index: Integer): string;
begin
  with FPropList^[Index] do
    Result := GetStrProp(Instance, PropInfo);
end;

function TcxPropertyEditor.GetVarValue: Variant;
begin
  Result := GetVarValueAt(0);
end;

function TcxPropertyEditor.GetVarValueAt(Index: Integer): Variant;
begin
  with FPropList^[Index] do
    Result := GetVariantProp(Instance, PropInfo);
end;

function TcxPropertyEditor.GetValue: string;
begin
  Result := srUnknown;
end;

procedure TcxPropertyEditor.GetValues(Proc: TGetStrProc);
begin
end;

function TcxPropertyEditor.FindRoot: TComponent;
begin
  Result := FOwner;
end;

procedure TcxPropertyEditor.PostChangedNotification;
begin
  Inspector.PostChangedNotification;
end;

procedure TcxPropertyEditor.SetFloatValue(Value: Extended);
var
  I: Integer;
begin
  for I := 0 to FPropCount - 1 do
    with FPropList^[I] do
      SetFloatProp(Instance, PropInfo, Value);
end;

procedure TcxPropertyEditor.SetInt64Value(Value: Int64);
var
  I: Integer;
begin
  for I := 0 to FPropCount - 1 do
    with FPropList^[I] do
      SetInt64Prop(Instance, PropInfo, Value);
end;

procedure TcxPropertyEditor.SetOrdValue(const Value: TdxNativeInt);
var
  I: Integer;
begin
  for I := 0 to FPropCount - 1 do
    with FPropList^[I] do
      SetOrdProp(Instance, PropInfo, Value);
end;

procedure TcxPropertyEditor.SetStrValue(const Value: string);
var
  I: Integer;
begin
  for I := 0 to FPropCount - 1 do
    with FPropList^[I] do
      SetStrProp(Instance, PropInfo, Value);
end;

procedure TcxPropertyEditor.SetVarValue(const Value: Variant);
var
  I: Integer;
begin
  for I := 0 to FPropCount - 1 do
    with FPropList^[I] do
      SetVariantProp(Instance, PropInfo, Value);
end;

procedure TcxPropertyEditor.SetValue(const Value: string);
begin
end;

function TcxPropertyEditor.IsDefaultValue: Boolean;
  function CheckProperties(AnObject: TObject): Boolean;
  var
    PropList: PPropList;
    PropInfo: PPropInfo;
    I, Count: Integer;
  begin
    Result := True;
    // Go through each of the properties on the object
    Count := GetTypeData(AnObject.ClassInfo)^.PropCount;
    if Count > 0 then
    begin
      GetMem(PropList, Count * SizeOf(Pointer));
      try
        GetPropInfos(AnObject.ClassInfo, PropList);
        for I := 0 to Count - 1 do
        begin
          PropInfo := PropList^[I];
          if PropInfo = nil then
            Break;
          if not IsDefaultPropertyValue(AnObject, PropInfo, GetLookupInfo) then
          begin
            Result := False;
            Break;
          end;
        end;
      finally
        FreeMem(PropList, Count * SizeOf(Pointer));
      end;
    end;
  end;

  function FindChildComponent(Component: TComponent; const Name: string): TComponent;
  var
    I: Integer;
  begin
    Result := Component.FindComponent(Name);
    if Result = nil then
      for I := 0 to Component.ComponentCount - 1 do
      begin
        Result := FindChildComponent(Component.Components[I], Name);
        if Result <> nil then
          Exit;
      end;
  end;

var
  FirstInstance: TObject;
  FirstPropInfo: PPropInfo;
  ChildName: string;

  SubObject: TObject;
  OldAncestor: TPersistent;

begin
  Result := True;
  if PropCount > 0 then
  begin
    // if they are not all equal, then they aren't all the default (at least one..)
    if not AllEqual then
    begin
      Result := False;
      Exit;
    end;

    FirstInstance := FPropList^[0].Instance;
    FirstPropInfo := FPropList^[0].PropInfo;
    if IsStoredProp(FirstInstance, FirstPropInfo) then
    begin
      // TWriter.WriteDescendent simulation
      FRootAncestor := nil;
      FAncestor := nil;
      FRoot := FindRoot;
      if FirstInstance is TComponent then
      begin
        FLookingFor := TComponent(FirstInstance);
        // Only lookup the component if it was introduced in an ancestor form/frame
        if csAncestor in FLookingFor.ComponentState then
        begin
          FDoneLooking := False;
          if csSubComponent in FLookingFor.ComponentStyle then
          begin
            ChildName := FLookingFor.Name;
            repeat
              FLookingFor := FLookingFor.Owner;
            until not (csSubComponent in FLookingFor.ComponentStyle);
            WriteComponentSimulation(FRoot);
            FAncestor := FindChildComponent(TComponent(FAncestor), ChildName);
          end
          else
            WriteComponentSimulation(FRoot);
        end
        else
        begin
          FRootAncestor := nil;
          FAncestor := nil;
        end;
      end
      else
      begin
        // In this case, we will not look up the ancestor (there really
        // isn't one - take columns on TreeListView as an example)
        FRootAncestor := nil;
        FAncestor := nil;
      end;

      Result := IsDefaultPropertyValue(FirstInstance, FirstPropInfo, GetLookupInfo);
      if not Result then
      begin
        if FirstPropInfo^.PropType^.Kind = tkClass then
        begin
          // If it was a class/object then we need to recursively check that
          // object to see if it has all default properties.
          SubObject := GetObjectProp(FirstInstance, FirstPropInfo);

          OldAncestor := FAncestor;
          try
            if AncestorIsValid(FAncestor, FRoot, FRootAncestor) then
              FAncestor := TPersistent(GetOrdProp(FAncestor, FirstPropInfo));
            Result := CheckProperties(SubObject);
          finally
            FAncestor := OldAncestor;
          end;

          if SubObject is TCollection then
          begin
            if not AncestorIsValid(FAncestor, FRoot, FRootAncestor) or
              not CollectionsEqual(TCollection(SubObject),
                TCollection(GetOrdProp(FAncestor, FirstPropInfo)), FRoot, FRootAncestor) then
                  Result := False;
          end;
        end;
      end;
    end;
  end;
end;

function TcxPropertyEditor.ValueAvailable: Boolean;
var
  I: Integer;
  S: string;
begin
  Result := True;
  for I := 0 to FPropCount - 1 do
    if (FPropList^[I].Instance is TComponent) and
      (csCheckPropAvail in TComponent(FPropList^[I].Instance).ComponentStyle) then
    begin
      try
        S := GetValue;
        AllEqual;
      except
        Result := False;
      end;
      Exit;
    end;
end;

procedure TcxPropertyEditor.AddAncestor(Component: TComponent);
begin
  FAncestorList.Add(Component);
end;

procedure TcxPropertyEditor.GetLookupInfo(var Ancestor: TPersistent;
  var Root, LookupRoot, RootAncestor: TComponent);
begin
  Ancestor := FAncestor;
  Root := FRoot;
  LookupRoot := FRoot; // Same in this case
  RootAncestor := FRootAncestor;
end;

procedure TcxPropertyEditor.SetPropEntry(Index: Integer;
  AInstance: TPersistent; APropInfo: PPropInfo);
begin
  with FPropList^[Index] do
  begin
    Instance := AInstance;
    PropInfo := APropInfo;
  end;
end;

type
  TComponentHack = class(TComponent);

procedure TcxPropertyEditor.WriteComponentSimulation(Component: TComponent);
  function FindAncestor(const Name: string): TComponent;
  var
    I: Integer;
  begin
    for I := 0 to FAncestorList.Count - 1 do
    begin
      Result := FAncestorList[I];
      if SameText(Result.Name, Name) then Exit;
    end;
    Result := nil;
  end;
var
  OldAncestor: TPersistent;
  OldRoot, OldRootAncestor: TComponent;
  OldAncestorList: TList;
  TempAncestor: TPersistent;
begin
  if FDoneLooking then
    Exit;

  OldAncestor := FAncestor;
  OldRootAncestor := FRootAncestor;
  try
    if Assigned(FAncestorList) then
      FAncestor := FindAncestor(Component.Name);

    // If we are at the component we were looking for, then we
    // can stop at this point
    if FLookingFor = Component then
      FDoneLooking := True
    else if SameText(FLookingFor.Name, Component.Name) then
      FDoneLooking := True
    else
    begin
      if (FAncestor = nil) and (Component <> FRoot) then
      begin
        TempAncestor := FRoot;
        if TempAncestor <> nil then
        begin
          FAncestor := TempAncestor;
          FRootAncestor := TComponent(FAncestor);
        end;
      end;
      // Component.WriteState(Self); // This is simulated below, inline
      OldAncestorList := FAncestorList;
      OldRoot := FRoot;
      OldRootAncestor := FRootAncestor;
      try
        FAncestorList := nil;
        try
          if (FAncestor <> nil) and (FAncestor is TComponent) then
          begin
            if csInline in TComponent(FAncestor).ComponentState then
              FRootAncestor := TComponent(FAncestor);
            FAncestorList := TList.Create;
            TComponentHack(FAncestor).GetChildren(AddAncestor, FRootAncestor);
          end;
          if csInline in Component.ComponentState then
            FRoot := Component;
          TComponentHack(Component).GetChildren(WriteComponentSimulation, FRoot);
        finally
          FAncestorList.Free;
        end;
      finally
        FAncestorList := OldAncestorList;
        if not FDoneLooking then
        begin
          FRoot := OldRoot;
          FRootAncestor := OldRootAncestor;
        end;
      end;
    end;
  finally
    if not FDoneLooking then
    begin
      // Only restore the ancestor if we were not done looking.
      // This way, we can continue up the chain looking for the
      // component
      FAncestor := OldAncestor;
      FRootAncestor := OldRootAncestor;
    end
  end;
end;

{ TcxOrdinalProperty }

function TcxOrdinalProperty.AllEqual: Boolean;
var
  I: Integer;
  V: TdxNativeInt;
begin
  Result := False;
  if PropCount > 1 then
  begin
    V := GetOrdValue;
    for I := 1 to PropCount - 1 do
      if GetOrdValueAt(I) <> V then Exit;
  end;
  Result := True;
end;

function TcxOrdinalProperty.GetEditLimit: Integer;
begin
  Result := 63;
end;

{ TcxIntegerProperty }

function TcxIntegerProperty.GetValue: string;
begin
  with GetTypeData(GetPropType)^ do
    if OrdType = otULong then
      Result := IntToStr(Cardinal(GetOrdValue))
    else
      Result := IntToStr(GetOrdValue);
end;

procedure TcxIntegerProperty.SetValue(const Value: string);

  procedure Error(const Args: array of const);
  begin
    raise EcxPropertyError.CreateFmt(SOutOfRange, Args);
  end;

var
  L: Int64;
begin
  if TryStrToInt64(Value, L) then
  begin
    with GetTypeData(GetPropType)^ do
      if OrdType = otULong then
      begin
        if (L < Cardinal(MinValue)) or (L > Cardinal(MaxValue)) then
          Error([Int64(Cardinal(MinValue)), Int64(Cardinal(MaxValue))]);
      end
      else
        if (L < MinValue) or (L > MaxValue) then
          Error([MinValue, MaxValue]);
    SetOrdValue(L);
  end
  else
    raise EcxPropertyError.CreateResFmt(@SInvalidInteger, [Value]);
end;

{ TCharProperty }

function TcxCharProperty.GetValue: string;
var
  Ch: Char;
begin
  Ch := Chr(GetOrdValue);
  if dxCharInSet(Ch, [#33..#127]) then
    Result := Ch
  else
    FmtStr(Result, '#%d', [Ord(Ch)]);
end;

procedure TcxCharProperty.SetValue(const Value: string);
var
  L: Longint;
begin
  if Length(Value) = 0 then
    L := 0
  else
    if Length(Value) = 1 then
      L := Ord(Value[1])
    else
      if Value[1] = '#' then
        L := StrToInt(Copy(Value, 2, MaxInt))
      else
        raise EcxPropertyError.Create(SInvalidPropertyValue);

  with GetTypeData(GetPropType)^ do
    if (L < MinValue) or (L > MaxValue) then
      raise EcxPropertyError.CreateFmt(SOutOfRange, [MinValue, MaxValue]);
  SetOrdValue(L);
end;

{ TcxEnumProperty }

function TcxEnumProperty.GetAttributes: TcxPropertyAttributes;
begin
  Result := [ipaMultiSelect, ipaValueList, ipaSortList, ipaRevertable];
  if FPropList[0].PropInfo.SetProc = nil then
    Result := Result + [ipaReadOnly] - [ipaRevertable, ipaValueList];
end;

function TcxEnumProperty.GetValue: string;
var
  L: TdxNativeInt;
begin
  L := GetOrdValue;
  with GetTypeData(GetPropType)^ do
  begin
    if (L < MinValue) or (L > MaxValue) then
      L := MaxValue;
  end;
  Result := GetEnumName(GetPropType, L);
end;

procedure TcxEnumProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
  EnumType: PTypeInfo;
begin
  EnumType := GetPropType;
  with GetTypeData(EnumType)^ do
  begin
    if MinValue < 0 then
    begin
      Proc(GetEnumName(EnumType, 0));
      Proc(GetEnumName(EnumType, 1));
    end
    else
      for I := MinValue to MaxValue do
        Proc(GetEnumName(EnumType, I));
  end;
end;

procedure TcxEnumProperty.SetValue(const Value: string);
var
  I: Integer;
begin
  I := GetEnumValue(GetPropType, Value);
  with GetTypeData(GetPropType)^ do
    if (I < MinValue) or (I > MaxValue) then
      raise EcxPropertyError.Create(SInvalidPropertyValue);
  SetOrdValue(I);
end;

{ TInt64Property }

function TcxInt64Property.AllEqual: Boolean;
var
  I: Integer;
  V: Int64;
begin
  Result := False;
  if PropCount > 1 then
  begin
    V := GetInt64Value;
    for I := 1 to PropCount - 1 do
      if GetInt64ValueAt(I) <> V then Exit;
  end;
  Result := True;
end;

function TcxInt64Property.GetEditLimit: Integer;
begin
  Result := 63;
end;

function TcxInt64Property.GetValue: string;
begin
  Result := IntToStr(GetInt64Value);
end;

procedure TcxInt64Property.SetValue(const Value: string);
begin
  SetInt64Value(StrToInt64(Value));
end;

{ TcxFloatProperty }

function TcxFloatProperty.AllEqual: Boolean;
var
  I: Integer;
  V: Extended;
begin
  Result := False;
  if PropCount > 1 then
  begin
    V := GetFloatValue;
    for I := 1 to PropCount - 1 do
      if GetFloatValueAt(I) <> V then Exit;
  end;
  Result := True;
end;

function TcxFloatProperty.GetValue: string;
const
  Precisions: array[TFloatType] of Integer = (7, 15, 18, 18, 18);
begin
  Result := FloatToStrF(GetFloatValue, ffGeneral,
    Precisions[GetTypeData(GetPropType)^.FloatType], 0);
end;

procedure TcxFloatProperty.SetValue(const Value: string);
begin
  SetFloatValue(StrToFloat(Value));
end;

{ TcxStringProperty }

function TcxStringProperty.AllEqual: Boolean;
var
  I: Integer;
  V: string;
begin
  Result := False;
  if PropCount > 1 then
  begin
    V := GetStrValue;
    for I := 1 to PropCount - 1 do
      if GetStrValueAt(I) <> V then Exit;
  end;
  Result := True;
end;

function TcxStringProperty.GetEditLimit: Integer;
begin
  if GetPropType^.Kind = tkString then
    Result := GetTypeData(GetPropType)^.MaxLength
  else
    Result := 255;
end;

function TcxStringProperty.GetValue: string;
begin
  Result := GetStrValue;
end;

procedure TcxStringProperty.SetValue(const Value: string);
begin
  SetStrValue(Value);
end;

{ TcxComponentNameProperty }

function TcxComponentNameProperty.GetAttributes: TcxPropertyAttributes;
begin
  Result := [];
end;

function TcxComponentNameProperty.GetEditLimit: Integer;
begin
  Result := 63;
end;

{ TcxSetElementProperty }

constructor TcxSetElementProperty.Create(APropList: PcxInstPropList;
  APropCount: Integer; AElement: Integer);
begin
  FPropList := APropList;
  FPropCount := APropCount;
  FElement := AElement;
end;

destructor TcxSetElementProperty.Destroy;
begin
end;

function TcxSetElementProperty.AllEqual: Boolean;
var
  I: Integer;
  S: TcxIntegerSet;
  V: Boolean;
begin
  Result := False;
  if PropCount > 1 then
  begin
    Integer(S) := GetOrdValue;
    V := FElement in S;
    for I := 1 to PropCount - 1 do
    begin
      Integer(S) := GetOrdValueAt(I);
      if (FElement in S) <> V then Exit;
    end;
  end;
  Result := True;
end;

function TcxSetElementProperty.GetAttributes: TcxPropertyAttributes;
begin
  Result := [ipaMultiSelect, ipaValueList, ipaSortList, ipaRevertable];
  if FPropList^[0].PropInfo.SetProc = nil then
    Include(Result, ipaReadOnly);
end;

function TcxSetElementProperty.GetName: string;
begin
  Result := GetEnumName(GetTypeData(GetPropType)^.CompType^, FElement);
end;

function TcxSetElementProperty.GetValue: string;
var
  S: TcxIntegerSet;
begin
  Integer(S) := GetOrdValue;
  if FElement in S then
    Result := 'True'
  else
    Result := 'False';
end;

procedure TcxSetElementProperty.GetValues(Proc: TGetStrProc);
begin
  Proc('False');
  Proc('True');
end;

procedure TcxSetElementProperty.SetValue(const Value: string);
var
  S: TcxIntegerSet;
begin
  Integer(S) := GetOrdValue;
  if CompareText(Value, 'True') = 0 then
    Include(S, FElement)
  else
    Exclude(S, FElement);
  SetOrdValue(Integer(S));
end;

function TcxSetElementProperty.IsDefaultValue: Boolean;
var
  S1, S2: TcxIntegerSet;
  HasStoredProc: Integer;
  ProcAsInt: Integer;
begin
  Result := inherited IsDefaultValue;
  if not Result then
  begin
    ProcAsInt := Integer(PPropInfo(GetPropInfo)^.StoredProc);
    HasStoredProc := ProcAsInt and $FFFFFF00;
    if HasStoredProc = 0 then
    begin
      Integer(S1) := PPropInfo(GetPropInfo)^.Default;
      Integer(S2) := GetOrdValue;
      Result := not ((FElement in S1) xor (FElement in S2));
    end;
  end;
end;

{ TcxSetProperty }

function TcxSetProperty.GetAttributes: TcxPropertyAttributes;
begin
  Result := [ipaMultiSelect, ipaSubProperties, ipaReadOnly, ipaRevertable];
  if FPropList^[0].PropInfo.SetProc = nil then
    Include(Result, ipaReadOnly);
end;

procedure TcxSetProperty.GetProperties(AOwner: TComponent; Proc: TcxGetPropEditProc);
var
  I: Integer;
begin
  with GetTypeData(GetTypeData(GetPropType)^.CompType^)^ do
    for I := MinValue to MaxValue do
      Proc(TcxSetElementProperty.Create(FPropList, FPropCount, I));
end;

function TcxSetProperty.GetValue: string;
var
  S: TcxIntegerSet;
  TypeInfo: PTypeInfo;
  I: Integer;
begin
  Integer(S) := GetOrdValue;
  TypeInfo := GetTypeData(GetPropType)^.CompType^;
  Result := '[';
  for I := 0 to SizeOf(Integer) * 8 - 1 do
    if I in S then
    begin
      if Length(Result) <> 1 then Result := Result + ',';
      Result := Result + GetEnumName(TypeInfo, I);
    end;
  Result := Result + ']';
end;

{ TcxClassProperty }

function TcxClassProperty.GetAttributes: TcxPropertyAttributes;
begin
  Result := [ipaMultiSelect, ipaSubProperties, ipaReadOnly];
end;

procedure TcxClassProperty.GetProperties(AOwner: TComponent; Proc: TcxGetPropEditProc);
var
  Components: TcxComponentList;
  I: Integer;
begin
  Components := TcxComponentList.Create;
  try
    for I := 0 to PropCount - 1 do
      if TComponent(GetOrdValueAt(I)) <> nil then
        Components.Add(TComponent(GetOrdValueAt(I)));
    cxGetComponentProperties(AOwner, FInspector, Components, tkProperties, Proc);
  finally
    Components.Free;
  end;
end;

function TcxClassProperty.GetValue: string;
begin
  FmtStr(Result, '(%s)', [GetPropType^.Name]);
end;

{ TcxComponentProperty }

function TcxComponentProperty.GetAttributes: TcxPropertyAttributes;
begin
  Result := [ipaMultiSelect];
  if Assigned(GetPropInfo^.SetProc) then
    Result := Result + [ipaValueList, ipaSortList, ipaRevertable]
  else
    Include(Result, ipaReadOnly);
  if (GetComponentReference <> nil) and AllEqual then
    Result := Result + [ipaSubProperties];
end;

function TcxComponentProperty.GetEditLimit: Integer;
begin
  Result := 127;
end;

function TcxComponentProperty.GetValue: string;
begin
  if GetComponentReference <> nil then
    Result := GetFullName(TComponent(GetOrdValue))
  else
    Result := '';
end;

procedure TcxComponentProperty.GetValues(Proc: TGetStrProc);

  procedure AddProc(AComponent: TComponent);
  var
    i: Integer;
  begin
    for i := 0 to AComponent.ComponentCount - 1 do
    begin
      if IsValidComponent(AComponent.Components[i]) then
        Proc(GetFullName(AComponent.Components[i]));
      AddProc(AComponent.Components[i]);
    end;
  end;

var
  AOwner: TComponent;
begin
  if FOwner <> nil then
  begin
    AOwner := FOwner;
    while AOwner.Owner <> nil do
      AOwner := AOwner.Owner;
    AddProc(AOwner);
  end
  else
    AddProc(Application);
end;

procedure TcxComponentProperty.SetValue(const Value: string);
var
  Component: TComponent;

  function GetComponentByName(const AName: string): TComponent;

     procedure CheckOwner(AOwner: TComponent);
     var
       I: Integer;
       AComponent: TComponent;
     begin
       if Result <> nil then Exit;
       for I := 0 to AOwner.ComponentCount - 1 do
       begin
         AComponent := AOwner.Components[I];
         if SameText(GetFullName(AComponent), AName) then
         begin
           Result := AComponent;
           break;
         end
         else
           CheckOwner(AComponent);
       end;
     end;

  var
    AOwner: TComponent;
  begin
    Result := nil;
    AOwner := FOwner;
    while AOwner.Owner <> nil do
      AOwner := AOwner.Owner;
    CheckOwner(AOwner);
  end;

begin
  if Value = '' then
    Component := nil
  else
  begin
    Component := GetComponentByName(Value);
    if not (Component is GetTypeData(GetPropType)^.ClassType) then
      raise EcxPropertyError.Create(SInvalidPropertyValue);
  end;
  SetOrdValue(TdxNativeInt(Component));
end;

function TcxComponentProperty.GetComponentReference: TComponent;
begin
  Result := TComponent(GetOrdValue);
end;

function TcxComponentProperty.IsValidComponent(AComponent: TComponent): Boolean;
begin
  Result := (AComponent.Name <> '') and (AComponent is GetTypeData(GetPropType)^.ClassType);
end;

function TcxComponentProperty.GetFullName(AComponent: TComponent): string;
begin
  Result := AComponent.Name;
  while (AComponent.Name <> '') and (AComponent.Owner <> nil) and (AComponent.Owner <> Inspector.Owner) do
  begin
    AComponent := AComponent.Owner;
    if AComponent.Name <> '' then
      Result := AComponent.Name + '.' + Result;
  end;
end;

procedure TcxComponentProperty.GetProperties(AOwner: TComponent;
  Proc: TcxGetPropEditProc);
var
  Components: TcxComponentList;
  I: Integer;
begin
  Components := TcxComponentList.Create;
  try
    for I := 0 to PropCount - 1 do
      if TComponent(GetOrdValueAt(I)) <> nil then
        Components.Add(TComponent(GetOrdValueAt(I)));
    if Inspector.InspectedLevel < MaxNestedComponentLevel then
      cxGetComponentProperties(AOwner, FInspector, Components, tkAny, Proc);
  finally
    Components.Free;
  end;
end;

{ TcxFontNameProperty }

function TcxFontNameProperty.GetAttributes: TcxPropertyAttributes;
begin
  Result := [ipaMultiSelect, ipaValueList, ipaSortList, ipaRevertable];
end;

procedure TcxFontNameProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  for I := 0 to Screen.Fonts.Count - 1 do
    Proc(Screen.Fonts[I]);
end;

{ TcxFontCharsetProperty }

function TcxFontCharsetProperty.GetAttributes: TcxPropertyAttributes;
begin
  Result := [ipaMultiSelect, ipaSortList, ipaValueList];
end;

function TcxFontCharsetProperty.GetValue: string;
begin
  if not CharsetToIdent(TFontCharset(GetOrdValue), Result) then
    FmtStr(Result, '%d', [GetOrdValue]);
end;

procedure TcxFontCharsetProperty.GetValues(Proc: TGetStrProc);
begin
  GetCharsetValues(Proc);
end;

procedure TcxFontCharsetProperty.SetValue(const Value: string);
var
  NewValue: Longint;
begin
  if IdentToCharset(Value, NewValue) then
    SetOrdValue(NewValue)
  else
    inherited SetValue(Value);
end;

{ TcxImeNameProperty }

function TcxImeNameProperty.GetAttributes: TcxPropertyAttributes;
begin
  Result := [ipaValueList, ipaSortList, ipaMultiSelect];
end;

procedure TcxImeNameProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  for I := 0 to Screen.Imes.Count - 1 do
    Proc(Screen.Imes[I]);
end;

{ TcxMPFilenameProperty }

procedure TcxMPFilenameProperty.Edit;
var
  OpenDialog: TOpenDialog;
begin
  OpenDialog := TOpenDialog.Create(Application);
  with OpenDialog do
  try
    Filename := GetValue;
    Filter := SMPOpenFilter;
    Options := Options + [ofPathMustExist, ofFileMustExist];
    if Execute then
    begin
      SetValue(FileName);
      PostChangedNotification;
    end;
  finally
    Free;
  end;
end;

function TcxMPFilenameProperty.GetAttributes: TcxPropertyAttributes;
begin
  Result := [ipaDialog, ipaRevertable];
end;

{ TcxColorProperty }

procedure TcxColorProperty.Edit;
var
  ColorDialog: TColorDialog;
  IniFile: TRegIniFile;

  procedure GetCustomColors;
  begin
    IniFile := TRegIniFile.Create('\Software\Borland\Delphi\7.0');
    try
      IniFile.ReadSectionValues(SCustomColors, ColorDialog.CustomColors);
    except
      { Ignore errors reading values }
    end;
  end;

  procedure SaveCustomColors;
  var
    I, P: Integer;
    S: string;
  begin
    if IniFile <> nil then
      with ColorDialog do
        for I := 0 to CustomColors.Count - 1 do
        begin
          S := CustomColors.Strings[I];
          P := Pos('=', S);
          if P <> 0 then
          begin
            S := Copy(S, 1, P - 1);
            IniFile.WriteString(SCustomColors, S, CustomColors.Values[S]);
          end;
        end;
  end;

begin
  IniFile := nil;
  ColorDialog := TColorDialog.Create(Application);
  with ColorDialog do
  try
    GetCustomColors;
    Color := GetOrdValue;
    if Execute then
    begin
      SetOrdValue(Color);
      PostChangedNotification;
    end;
    SaveCustomColors;
  finally
    if IniFile <> nil then IniFile.Free;
    Free;
  end;
end;

function TcxColorProperty.GetAttributes: TcxPropertyAttributes;
begin
  Result := [ipaMultiSelect, ipaDialog];
end;

function TcxColorProperty.GetValue: string;
begin
  Result := IntToStr(GetOrdValue);
end;

procedure TcxColorProperty.GetValues(Proc: TGetStrProc);
begin
end;

{ TcxCursorProperty }

function TcxCursorProperty.GetAttributes: TcxPropertyAttributes;
begin
  Result := [ipaMultiSelect, ipaValueList, ipaSortList, ipaRevertable];
end;

function TcxCursorProperty.GetValue: string;
begin
  Result := CursorToString(TCursor(GetOrdValue));
end;

procedure TcxCursorProperty.GetValues(Proc: TGetStrProc);
begin
  GetCursorValues(Proc);
end;

procedure TcxCursorProperty.SetValue(const Value: string);
var
  NewValue: Longint;
begin
  if IdentToCursor(Value, NewValue) then
    SetOrdValue(NewValue)
  else
    inherited SetValue(Value);
end;

{ TcxFontProperty }

procedure TcxFontProperty.Edit;
var
  FontDialog: TFontDialog;
begin
  FontDialog := TFontDialog.Create(Application);
  try
    FontDialog.Font := TFont(GetOrdValue);
    FontDialog.Options := FontDialog.Options + [fdForceFontExist];
    if FontDialog.Execute then
    begin
      SetOrdValue(TdxNativeInt(FontDialog.Font));
      PostChangedNotification;
    end;
  finally
    FontDialog.Free;
  end;
end;

function TcxFontProperty.GetAttributes: TcxPropertyAttributes;
begin
  Result := [ipaMultiSelect, ipaSubProperties, ipaDialog, ipaReadOnly];
end;

{TcxStringsProperty}

procedure TcxStringsProperty.Edit;
var
  Data: TcxStringsEditorDlgData;
begin
  if Inspector <> nil then
    Data.LookAndFeel := TcxCustomRTTIInspector(Inspector).LookAndFeel
  else
    Data.LookAndFeel := nil;
  Data.Caption := GetComponent(0).GetNamePath + '.' + GetName;
  Data.Text := TStrings(GetOrdValue).Text;
  if cxShowStringsEditor(@Data) then
  begin
    TStrings(GetOrdValue).Text := Data.Text;
    PostChangedNotification;
  end;
end;

function TcxStringsProperty.GetAttributes: TcxPropertyAttributes;
begin
  Result := [ipaDialog, ipaReadOnly];
end;

{ TcxGraphicProperty }

procedure TcxGraphicProperty.Edit;
var
  Data: TcxPictureEditorDlgData;
  P: TPicture;
begin
  P := TPicture.Create;
  try
    P.Assign(GetGraphic);
    with Data do
    begin
      if Inspector <> nil then
        LookAndFeel := TcxCustomRTTIInspector(Inspector).LookAndFeel
      else
        LookAndFeel := nil;
      Caption := GetComponent(0).GetNamePath + '.' + GetName;
      ClipboardFormat := GetClipboardFormat;
      GraphicFilter := GetGraphicFilter;
      Picture := P;
    end;
    if cxShowPictureEditor(@Data) then
    begin
      SetGraphic(P.Graphic);
      PostChangedNotification;
    end;
  finally
    P.Free;
  end;
end;

function TcxGraphicProperty.GetAttributes: TcxPropertyAttributes;
begin
  Result := [ipaDialog];
end;

function TcxGraphicProperty.HasGraphic: Boolean;
begin
  Result := (GetGraphic <> nil) and not GetGraphic.Empty;
end;

function TcxGraphicProperty.GraphicClass: TGraphicClass;
begin
  if GetGraphic = nil then
    Result := nil
  else
    Result := TGraphicClass(GetGraphic.ClassType);
end;

function TcxGraphicProperty.GetGraphicFilter: string;
begin
  Result := GraphicFilter(TGraphic)
end;

function TcxGraphicProperty.GetClipboardFormat: Word;
begin
  Result := CF_PICTURE;
  if GraphicClass <> nil then
    if GraphicClass.InheritsFrom(TBitmap) then
      Result := CF_BITMAP
    else
      if GraphicClass.InheritsFrom(TMetafile) then
        Result := CF_METAFILEPICT;
end;

function TcxGraphicProperty.GetGraphic: TGraphic;
begin
  Result := TGraphic(GetOrdValue);
end;

procedure TcxGraphicProperty.SetGraphic(Value: TGraphic);
begin
  GetGraphic.Assign(Value);
end;

function TcxGraphicProperty.GetValue: string;
begin
  if HasGraphic then
    Result := '(' + GetGraphic.ClassName + ')'
  else
    Result := '(None)';
end;

procedure TcxGraphicProperty.SetValue(const Value: string);
begin
  if Value = '' then SetGraphic(nil);
end;

{ TdxSmartGlyphProperty }

function TdxSmartGlyphProperty.GetAttributes: TcxPropertyAttributes;
begin
  Result := inherited + [ipaSubProperties];
end;

{ TcxPictureProperty }

function TcxPictureProperty.GetGraphic: TGraphic;
begin
  if GetOrdValue = 0 then
    Result := nil
  else
    Result := TPicture(GetOrdValue).Graphic;
end;

procedure TcxPictureProperty.SetGraphic(Value: TGraphic);
begin
  TPicture(GetOrdValue).Assign(Value);
end;

{ TcxModalResultProperty }

function TcxModalResultProperty.GetAttributes: TcxPropertyAttributes;
begin
  Result := [ipaMultiSelect, ipaValueList, ipaRevertable];
end;

function TcxModalResultProperty.GetValue: string;
var
  CurValue: TdxNativeInt;
begin
  CurValue := GetOrdValue;
  case CurValue of
    Low(dxModalResults)..High(dxModalResults):
      Result := dxModalResults[CurValue];
  else
    Result := IntToStr(CurValue);
  end;
end;

procedure TcxModalResultProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  for I := Low(dxModalResults) to High(dxModalResults) do
    Proc(dxModalResults[I]);
end;

procedure TcxModalResultProperty.SetValue(const Value: string);
var
  I: Integer;
begin
  if Value = '' then
  begin
    SetOrdValue(0);
    Exit;
  end;
  for I := Low(dxModalResults) to High(dxModalResults) do
    if CompareText(dxModalResults[I], Value) = 0 then
    begin
      SetOrdValue(I);
      Exit;
    end;
  inherited SetValue(Value);
end;

{ TcxShortCutProperty }

const
  cxShortCuts: array[0..82] of TShortCut = (
    scNone,
    Byte('A') or scCtrl,
    Byte('B') or scCtrl,
    Byte('C') or scCtrl,
    Byte('D') or scCtrl,
    Byte('E') or scCtrl,
    Byte('F') or scCtrl,
    Byte('G') or scCtrl,
    Byte('H') or scCtrl,
    Byte('I') or scCtrl,
    Byte('J') or scCtrl,
    Byte('K') or scCtrl,
    Byte('L') or scCtrl,
    Byte('M') or scCtrl,
    Byte('N') or scCtrl,
    Byte('O') or scCtrl,
    Byte('P') or scCtrl,
    Byte('Q') or scCtrl,
    Byte('R') or scCtrl,
    Byte('S') or scCtrl,
    Byte('T') or scCtrl,
    Byte('U') or scCtrl,
    Byte('V') or scCtrl,
    Byte('W') or scCtrl,
    Byte('X') or scCtrl,
    Byte('Y') or scCtrl,
    Byte('Z') or scCtrl,
    VK_F1,
    VK_F2,
    VK_F3,
    VK_F4,
    VK_F5,
    VK_F6,
    VK_F7,
    VK_F8,
    VK_F9,
    VK_F10,
    VK_F11,
    VK_F12,
    VK_F1 or scCtrl,
    VK_F2 or scCtrl,
    VK_F3 or scCtrl,
    VK_F4 or scCtrl,
    VK_F5 or scCtrl,
    VK_F6 or scCtrl,
    VK_F7 or scCtrl,
    VK_F8 or scCtrl,
    VK_F9 or scCtrl,
    VK_F10 or scCtrl,
    VK_F11 or scCtrl,
    VK_F12 or scCtrl,
    VK_F1 or scShift,
    VK_F2 or scShift,
    VK_F3 or scShift,
    VK_F4 or scShift,
    VK_F5 or scShift,
    VK_F6 or scShift,
    VK_F7 or scShift,
    VK_F8 or scShift,
    VK_F9 or scShift,
    VK_F10 or scShift,
    VK_F11 or scShift,
    VK_F12 or scShift,
    VK_F1 or scShift or scCtrl,
    VK_F2 or scShift or scCtrl,
    VK_F3 or scShift or scCtrl,
    VK_F4 or scShift or scCtrl,
    VK_F5 or scShift or scCtrl,
    VK_F6 or scShift or scCtrl,
    VK_F7 or scShift or scCtrl,
    VK_F8 or scShift or scCtrl,
    VK_F9 or scShift or scCtrl,
    VK_F10 or scShift or scCtrl,
    VK_F11 or scShift or scCtrl,
    VK_F12 or scShift or scCtrl,
    VK_INSERT,
    VK_INSERT or scShift,
    VK_INSERT or scCtrl,
    VK_DELETE,
    VK_DELETE or scShift,
    VK_DELETE or scCtrl,
    VK_BACK or scAlt,
    VK_BACK or scShift or scAlt);

function TcxShortCutProperty.GetAttributes: TcxPropertyAttributes;
begin
  Result := [ipaMultiSelect, ipaValueList, ipaRevertable];
end;

function TcxShortCutProperty.GetValue: string;
var
  CurValue: TShortCut;
begin
  CurValue := GetOrdValue;
  if CurValue = scNone then
    Result := srNone
  else
    Result := ShortCutToText(CurValue);
end;

procedure TcxShortCutProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  Proc(srNone);
  for I := 1 to High(cxShortCuts) do
    Proc(ShortCutToText(cxShortCuts[I]));
end;

procedure TcxShortCutProperty.SetValue(const Value: string);
var
  NewValue: TShortCut;
begin
  NewValue := 0;
  if (Value <> '') and (AnsiCompareText(Value, srNone) <> 0) then
  begin
    NewValue := TextToShortCut(Value);
    if NewValue = 0 then
      raise EcxPropertyError.Create(SInvalidPropertyValue);
  end;
  SetOrdValue(NewValue);
end;

{ TcxTabOrderProperty }

function TcxTabOrderProperty.GetAttributes: TcxPropertyAttributes;
begin
  Result := [];
end;

{ TcxCaptionProperty }

function TcxCaptionProperty.GetAttributes: TcxPropertyAttributes;
begin
  Result := [ipaMultiSelect, ipaAutoUpdate, ipaRevertable];
end;

{ TcxDateProperty }

function TcxDateProperty.GetAttributes: TcxPropertyAttributes;
begin
  Result := [ipaMultiSelect, ipaRevertable];
end;

function TcxDateProperty.GetValue: string;
var
  DT: TDateTime;
begin
  DT := GetFloatValue;
  if DT = 0.0 then
    Result := ''
  else
    Result := DateToStr(DT);
end;

procedure TcxDateProperty.SetValue(const Value: string);
var
  DT: TDateTime;
begin
  if Value = '' then
    DT := 0.0
  else
    DT := StrToDate(Value);
  SetFloatValue(DT);
end;

{ TcxTimeProperty }

function TcxTimeProperty.GetAttributes: TcxPropertyAttributes;
begin
  Result := [ipaMultiSelect, ipaRevertable];
end;

function TcxTimeProperty.GetValue: string;
var
  DT: TDateTime;
begin
  DT := GetFloatValue;
  Result := TimeToStr(dxTimeOf(DT));
end;

procedure TcxTimeProperty.SetValue(const Value: string);
var
  DT: TDateTime;
begin
  if Value = '' then
    DT := 0.0
  else
    DT := StrToTime(Value);
  SetFloatValue(DT);
end;

{ TcxDateTimeProperty }

function TcxDateTimeProperty.GetAttributes: TcxPropertyAttributes;
begin
  Result := [ipaMultiSelect, ipaRevertable];
end;

function TcxDateTimeProperty.GetValue: string;
var
  DT: TDateTime;
begin
  DT := GetFloatValue;
  if DT = 0.0 then
    Result := ''
  else
  {$IFDEF DELPHI16}
    if DT = NullDate then
      Result := StringReplace(DateTimeToStr(MinDateTime), '1', '0', [rfReplaceAll])
    else
  {$ENDIF}
      Result := DateTimeToStr(DT);
end;

procedure TcxDateTimeProperty.SetValue(const Value: string);
var
  DT: TDateTime;
begin
  if Value = '' then
    DT := 0.0
  else
    DT := StrToDateTime(Value);
  SetFloatValue(DT);
end;

{ TVariantTypeProperty }

var
  VarTypeNames: array[varEmpty..varInt64] of string = (
    'Unassigned', // varEmpty
    'Null',       // varNull
    'Smallint',   // varSmallint
    'Integer',    // varInteger
    'Single',     // varSingle
    'Double',     // varDouble
    'Currency',   // varCurrency
    'Date',       // varDate
    'OleStr',     // varOleStr
    '',           // varDispatch
    '',           // varError
    'Boolean',    // varBoolean
    '',           // varVariant
    '',           // varUnknown
    '',           // [varDecimal]
    '',           // [undefined]
    'Shortint',   // varShortInt
    'Byte',       // varByte
    'Word',       // varWord
    'LongWord',   // varLongWord
    'Int64');     // varInt64

type
  TcxVariantTypeProperty = class(TcxPropertyEditor)
  public
    constructor Create(APropList: PcxInstPropList; APropCount: Integer);
    destructor Destroy; override;
    function AllEqual: Boolean; override;
    function GetAttributes: TcxPropertyAttributes; override;
    function GetName: string; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
   end;

constructor TcxVariantTypeProperty.Create(APropList: PcxInstPropList;
  APropCount: Integer);
begin
  FPropList := APropList;
  FPropCount := APropCount;
end;

destructor TcxVariantTypeProperty.Destroy;
begin
end;

function TcxVariantTypeProperty.AllEqual: Boolean;
var
  i: Integer;
  V1, V2: Variant;
begin
  Result := False;
  if PropCount > 1 then
  begin
    V1 := GetVarValue;
    for i := 1 to PropCount - 1 do
    begin
      V2 := GetVarValueAt(i);
      if VarType(V1) <> VarType(V2) then Exit;
    end;
  end;
  Result := True;
end;

function TcxVariantTypeProperty.GetAttributes: TcxPropertyAttributes;
begin
  Result := [ipaMultiSelect, ipaValueList, ipaSortList];
end;

function TcxVariantTypeProperty.GetName: string;
begin
  Result := 'Type';
end;

function TcxVariantTypeProperty.GetValue: string;
begin
  if VarIsStr(GetVarValue) then
    Result := cxSString
  else
    case VarType(GetVarValue) and varTypeMask of
      Low(VarTypeNames)..High(VarTypeNames):
        Result := VarTypeNames[VarType(GetVarValue)];
    else
      Result := cxGetResourceString(@cxSvgUnknown);
    end;
end;

procedure TcxVariantTypeProperty.GetValues(Proc: TGetStrProc);
var
  i: Integer;
begin
  for i := 0 to High(VarTypeNames) do
    if VarTypeNames[i] <> '' then
      Proc(VarTypeNames[i]);
  Proc(cxSString);
end;

procedure TcxVariantTypeProperty.SetValue(const Value: string);

  function GetSelectedType: Integer;
  var
    i: Integer;
  begin
    Result := -1;
    for i := 0 to High(VarTypeNames) do
      if VarTypeNames[i] = Value then
      begin
        Result := i;
        break;
      end;
    if (Result = -1) and (Value = cxSString) then
      Result := varString;
  end;

var
  NewType: Integer;
  V: Variant;
begin
  V := GetVarValue;
  NewType := GetSelectedType;
  case NewType of
    varEmpty: VarClear(V);
    varNull: V := NULL;
    -1: raise EdxException.Create('UnknownType');  //todo resource
  else
    try
      VarCast(V, V, NewType);
    except
      // If it cannot cast, clear it and then cast again.
      VarClear(V);
      VarCast(V, V, NewType);
    end;
  end;
  SetVarValue(V);
end;

{ TcxVariantProperty }

function TcxVariantProperty.GetAttributes: TcxPropertyAttributes;
begin
  Result := [ipaMultiSelect, ipaSubProperties];
end;

procedure TcxVariantProperty.GetProperties(AOwner: TComponent;
  Proc: TcxGetPropEditProc);
begin
  Proc(TcxVariantTypeProperty.Create(FPropList, FPropCount));
end;

function TcxVariantProperty.GetValue: string;

  function GetVariantStr(const Value: Variant): string;
  begin
    case VarType(Value) of
      varBoolean:
        Result := BooleanIdents[Value = True];
      varCurrency:
        Result := CurrToStr(Value);
    else
      Result := VarToStrDef(Value, cxSNull);
    end;
  end;

var
  Value: Variant;
begin
  Value := GetVarValue;
  if VarType(Value) <> varDispatch then
    Result := GetVariantStr(Value)
  else
    Result := 'ERROR';
end;

procedure TcxVariantProperty.SetValue(const Value: string);

  function Cast(var Value: Variant; NewType: Integer): Boolean;
  var
    V2: Variant;
  begin
    Result := True;
    if NewType = varCurrency then
      Result := AnsiPos(dxFormatSettings.CurrencyString, Value) > 0;
    if Result then
    try
      VarCast(V2, Value, NewType);
      Result := (NewType = varDate) or (VarToStr(V2) = VarToStr(Value));
      if Result then Value := V2;
    except
      Result := False;
    end;
  end;

var
  V: Variant;
  OldType: Integer;
begin
  OldType := VarType(GetVarValue);
  V := Value;
  if Value = '' then
    VarClear(V) else
  if (CompareText(Value, cxSNull) = 0) then
    V := NULL else
  if not Cast(V, OldType) then
    V := Value;
  SetVarValue(V);
end;

{ TcxEditPropertiesProperty }

function TcxEditPropertiesProperty.HasSubProperties: Boolean;
var
  I: Integer;
  AIntf: IcxEditorPropertiesContainer;
begin
  for I := 0 to PropCount - 1 do
  begin
    Result := Supports(GetComponent(I), IcxEditorPropertiesContainer, AIntf) and
      (AIntf.GetProperties <> nil);
    if not Result then Exit;
  end;
  Result := True;
end;

function TcxEditPropertiesProperty.GetAttributes: TcxPropertyAttributes;
begin
  Result := inherited GetAttributes;
  if not HasSubProperties then
    Exclude(Result, ipaSubProperties);
  Result := Result - [ipaReadOnly] + [ipaValueList, ipaSortList, ipaRevertable];
end;

function TcxEditPropertiesProperty.GetValue: string;
begin
  if HasSubProperties then
    Result := GetRegisteredEditProperties.GetDescriptionByClass(TcxCustomEditProperties(GetOrdValue).ClassType)
  else
    Result := '';
end;

procedure TcxEditPropertiesProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
  ADesc: string;
begin
  for I := 0 to GetRegisteredEditProperties.Count - 1 do
  begin
    ADesc := GetRegisteredEditProperties.Descriptions[I];
    if ADesc <> '' then
      Proc(ADesc);
  end;
end;

procedure TcxEditPropertiesProperty.SetValue(const Value: string);
var
  APropertiesClass: TcxCustomEditPropertiesClass;
  I: Integer;
  AIntf: IcxEditorPropertiesContainer;
begin
  APropertiesClass := TcxCustomEditPropertiesClass(
    GetRegisteredEditProperties.FindByClassName(Value));
  if APropertiesClass = nil then
    APropertiesClass := TcxCustomEditPropertiesClass(
      GetRegisteredEditProperties.FindByDescription(Value));
  for I := 0 to PropCount - 1 do
    if Supports(GetComponent(I), IcxEditorPropertiesContainer, AIntf) then
      AIntf.SetPropertiesClass(APropertiesClass);
  inherited;
end;

{ TcxSkinNameProperty }

function TcxSkinNameProperty.GetAttributes: TcxPropertyAttributes;
begin
  Result := inherited GetAttributes - [ipaReadOnly] + [ipaValueList];
end;

procedure TcxSkinNameProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  for I := 0 to cxLookAndFeelPaintersManager.Count - 1 do
  begin
    if cxLookAndFeelPaintersManager[I].LookAndFeelStyle = lfsSkin then
      Proc(cxLookAndFeelPaintersManager[I].LookAndFeelName);
  end;
end;

{ TcxPropInfoList }

type
  TcxPropInfoList = class
  private
    FList: PPropList;
    FCount: Integer;
    FSize: Integer;
    function Get(Index: Integer): PPropInfo;
  public
    constructor Create(AInstance: TPersistent; AFilter: TTypeKinds; ASorted: Boolean);
    destructor Destroy; override;

    function Contains(P: PPropInfo): Boolean;
    procedure Delete(Index: Integer);
    procedure Intersect(List: TcxPropInfoList);

    property Count: Integer read FCount;
    property Items[Index: Integer]: PPropInfo read Get; default;
  end;

constructor TcxPropInfoList.Create(AInstance: TPersistent; AFilter: TTypeKinds; ASorted: Boolean);
begin
  if AInstance.ClassInfo <> nil then
  begin
    FCount := GetPropList(AInstance.ClassInfo, AFilter, nil);
    FSize := FCount * SizeOf(Pointer);
    GetMem(FList, FSize);
    GetPropList(AInstance.ClassInfo, AFilter, FList, ASorted);
  end;
end;

destructor TcxPropInfoList.Destroy;
begin
  if FList <> nil then FreeMem(FList, FSize);
  inherited Destroy;
end;

function TcxPropInfoList.Contains(P: PPropInfo): Boolean;
var
  I: Integer;
begin
  for I := 0 to FCount - 1 do
    with FList^[I]^ do
      if (PropType^ = P^.PropType^) and
        (CompareText(dxShortStringToString(Name), dxShortStringToString(P^.Name)) = 0) then
      begin
        Result := True;
        Exit;
      end;
  Result := False;
end;

procedure TcxPropInfoList.Delete(Index: Integer);
begin
  Dec(FCount);
  if Index < FCount then
    Move(FList^[Index + 1], FList^[Index], (FCount - Index) * SizeOf(Pointer));
end;

function TcxPropInfoList.Get(Index: Integer): PPropInfo;
begin
  Result := FList^[Index];
end;

procedure TcxPropInfoList.Intersect(List: TcxPropInfoList);
var
  I: Integer;
begin
  for I := FCount - 1 downto 0 do
    if not List.Contains(FList^[I]) then Delete(I);
end;

{ GetComponentProperties }

procedure cxRegisterPropertyEditor(APropertyType: PTypeInfo; AComponentClass: TClass;
  const APropertyName: string; AEditorClass: TcxPropertyEditorClass);
var
  Item: TcxPropertyClassRec;
begin
  if FPropertyClasses = nil then FPropertyClasses := TList.Create;
  Item := TcxPropertyClassRec.Create;
  with Item do
  begin
    Group := CurrentGroup;
    PropertyType := APropertyType;
    ComponentClass := AComponentClass;
    PropertyName := '';
    if ComponentClass <> nil then PropertyName := APropertyName;
    EditorClass := AEditorClass;
  end;
  FPropertyClasses.Insert(0, Item);
end;

procedure cxRegisterPropertyMapper(AMapper: TcxPropertyMapperFunc);
var
  Item: TcxPropertyMapperRec;
begin
  if FPropertyMappers = nil then
    FPropertyMappers := TList.Create;
  Item := TcxPropertyMapperRec.Create;
  Item.Group := CurrentGroup;
  Item.Mapper := AMapper;
  FPropertyMappers.Insert(0, Item);
end;

function cxGetEditorClass(APropInfo: PPropInfo; Obj: TPersistent): TcxPropertyEditorClass;
var
  PropType: PTypeInfo;
  P, C: TcxPropertyClassRec;
  I: Integer;
begin
  if FPropertyMappers <> nil then
    for I := 0 to FPropertyMappers.Count - 1 do
      with TcxPropertyMapperRec(FPropertyMappers[I]) do
      begin
        Result := Mapper(Obj, APropInfo);
        if Result <> nil then Exit;
      end;

  PropType := APropInfo^.PropType^;
  I := 0;
  C := nil;
  if FPropertyClasses <> nil then
    while I < FPropertyClasses.Count do
    begin
      P := FPropertyClasses[I];
      if (P.PropertyType = PropType) or
       // compatible class type
       ( (PropType^.Kind = tkClass) and
         (P.PropertyType^.Kind = tkClass) and
         GetTypeData(PropType)^.ClassType.InheritsFrom(GetTypeData(P.PropertyType)^.ClassType)
       )
      then
        if ((P.ComponentClass = nil) or (Obj.InheritsFrom(P.ComponentClass))) and
           ((P.PropertyName = '') or SameText(GetPropName(APropInfo), P.PropertyName) ) then
          if (C = nil) or   // see if P is better match than C
             ((C.ComponentClass = nil) and (P.ComponentClass <> nil)) or
             ((C.PropertyName = '') and (P.PropertyName <> ''))
             or  // P's proptype match is exact, but C's isn't
             ((C.PropertyType <> PropType) and (P.PropertyType = PropType))
             or  // P's proptype is more specific than C's proptype
             ( (P.PropertyType <> C.PropertyType) and
               ( ( // P has a more specific class type than C.
                   (P.PropertyType^.Kind = tkClass) and
                   (C.PropertyType^.Kind = tkClass) and
                   GetTypeData(P.PropertyType)^.ClassType.InheritsFrom(
                   GetTypeData(C.PropertyType)^.ClassType)
                 )
               )
             ) or // P's component class is more specific than C's component class
             ( (P.ComponentClass <> nil) and (C.ComponentClass <> nil) and
               (P.ComponentClass <> C.ComponentClass) and
               (P.ComponentClass.InheritsFrom(C.ComponentClass))
             ) then
          C := P;
      Inc(I);
    end;

  if C <> nil then
    Result := C.EditorClass
  else
    Result := cxPropClassMap[PropType^.Kind];
end;

procedure cxGetComponentProperties(AOwner: TComponent;
  AInspector: TcxCustomRTTIInspector; AComponents: TcxComponentList;
  AFilter: TTypeKinds; AProc: TcxGetPropEditProc);
var
  I, J, CompCount: Integer;
  CompType: TClass;
  Candidates: TcxPropInfoList;
  PropLists: TList;
  Editor: TcxPropertyEditor;
  EdClass: TcxPropertyEditorClass;
  PropInfo: PPropInfo;
  AddEditor: Boolean;
  Obj: TPersistent;
begin
  if (AComponents = nil) or (AComponents.Count = 0) then Exit;
  AInspector.BeginLevel;
  CompCount := AComponents.Count;
  Obj := AComponents[0];
  CompType := AComponents[0].ClassType;
  Candidates := TcxPropInfoList.Create(AComponents[0], AFilter, AInspector.Sorted);
  try
    for I := Candidates.Count - 1 downto 0 do
    begin
      PropInfo := Candidates[I];
      EdClass := cxGetEditorClass(PropInfo, Obj);
      if EdClass = nil then
        Candidates.Delete(I)
      else
      begin
        Editor := EdClass.Create(AOwner, AInspector, 1);
        try
          Editor.SetPropEntry(0, Obj, PropInfo);
          with PropInfo^ do
            if (GetProc = nil) or
               (not AInspector.OptionsView.ShowReadOnlyProperties and
                ((PropType^.Kind <> tkClass) and (SetProc = nil))) or
               ((CompCount > 1) and
               not (ipaMultiSelect in Editor.GetAttributes)) or
               not Editor.ValueAvailable then
              Candidates.Delete(I);
        finally
          Editor.Free;
        end;
      end;
    end;
    PropLists := TList.Create;
    try
      PropLists.Capacity := CompCount;
      for I := 0 to CompCount - 1 do
        PropLists.Add(TcxPropInfoList.Create(AComponents[I], AFilter, AInspector.Sorted));
      for I := 0 to CompCount - 1 do
        Candidates.Intersect(TcxPropInfoList(PropLists[I]));
      for I := 0 to CompCount - 1 do
        TcxPropInfoList(PropLists[I]).Intersect(Candidates);
      for I := 0 to Candidates.Count - 1 do
      begin
        EdClass := cxGetEditorClass(Candidates[I], Obj);
        if EdClass = nil then Continue;
        Editor := EdClass.Create(AOwner, AInspector, CompCount);
        try
          AddEditor := True;
          for J := 0 to CompCount - 1 do
          begin
            if (AComponents[J].ClassType <> CompType) and
              (cxGetEditorClass(TcxPropInfoList(PropLists[J])[I],
                AComponents[J]) <> EdClass) then
            begin
              AddEditor := False;
              Break;
            end;
            Editor.SetPropEntry(J, AComponents[J], TcxPropInfoList(PropLists[J])[I]);
          end;
        except
          Editor.Free;
          raise;
        end;
        if AddEditor and Editor.ValueAvailable and Assigned(AProc) then
          AProc(Editor)
        else
          Editor.Free;
      end;
    finally
      for I := 0 to PropLists.Count - 1 do TcxPropInfoList(PropLists[I]).Free;
      PropLists.Free;
    end;
  finally
    AInspector.EndLevel;
    Candidates.Free;
  end;
end;

{ TcxRTTIInspectorEditingController }

procedure TcxRTTIInspectorEditingController.DoHideEdit(Accept: Boolean);
var
  AEditViewInfo: TcxEditCellViewInfo;
  AItem: TcxCustomInplaceEditContainer;
  ALink: TcxObjectLink;
begin
  if Accept then
  begin
    ALink := cxAddObjectLink(EditingItem);
    try
      Edit.Deactivate;
      Controller.DataController.PostEditingData;
    finally
      if ALink.Ref = nil then
      begin
        Controller.PostValidateFocusedItem;
        EditingItem := nil;
      end;
      cxRemoveObjectLink(ALink);
    end;
    if EditingItem = nil then Exit;
    AEditViewInfo := Controller.GetFocusedCellViewInfo(EditingItem);
    if AEditViewInfo <> nil then
      Edit.InternalProperties.Update(TcxEditCellViewInfoAccess(AEditViewInfo).Properties);
  end;
  AItem := EditingItem;
  if not Inspector.IsDestroying then
    Inspector.DoEdited(AItem);
  EditingItem := nil;
  Controller.RefreshFocusedCellViewInfo(AItem);
  if Edit <> nil then
  begin
    UninitEdit;
    Edit.EditModified := False;
    Controller.AllowCheckEdit := False;
    try
      Controller.SetFocus;
    finally
      Controller.AllowCheckEdit := True;
    end;
  end;
end;

function TcxRTTIInspectorEditingController.GetController: TcxRTTIInspectorController;
begin
  Result := TcxRTTIInspectorController(inherited Controller);
end;

function TcxRTTIInspectorEditingController.GetInspector: TcxCustomRTTIInspector;
begin
  Result := TcxCustomRTTIInspector(EditingControl);
end;

{ TcxRTTIInspectorController }

procedure TcxRTTIInspectorController.BeforeEditKeyDown(var Key: Word;
  var Shift: TShiftState);
begin
  if (Key = VK_RETURN) and (ssCtrl in Shift) then
  begin
    Key := 0;
    Inspector.TryInvokePropertyEditorDlg;
  end;
end;

procedure TcxRTTIInspectorController.DoEditDblClick(Sender: TObject);
begin
  with Inspector do
    if (PropertyEditor <> nil) and not (ipaRevertable in PropertyEditor.GetAttributes) then
      TryInvokePropertyEditorDlg;
end;

function TcxRTTIInspectorController.IsKeyForController(AKey: Word;
  AShift: TShiftState): Boolean;
begin
  Result := inherited IsKeyForController(AKey, AShift) or
    ((AKey = VK_RETURN) and (ssCtrl in AShift));
end;

procedure TcxRTTIInspectorController.DoUpdateRowAndCell(ANewRow: TcxCustomRow;
  ANewCellIndex: Integer);
begin
  if Inspector.LockRefresh then Exit;
  inherited DoUpdateRowAndCell(ANewRow, ANewCellIndex);
end;

procedure TcxRTTIInspectorController.SetFocusedRecordItem(
  ARecordIndex: TdxNativeInt; AItem: TcxCustomInplaceEditContainer);
begin
  ValidateFocusingItem(AItem);
  AllowCheckEdit := False;
  try
    DisableCellsRefresh := (FocusedRecordIndex = ARecordIndex);
    FocusedRecordIndex := ARecordIndex;
    DisableCellsRefresh := DisableCellsRefresh and (FocusedItem = AItem);
    FocusedItem := AItem as TcxCellEdit;
  finally
    AllowCheckEdit := True;
    CheckEdit;
    DisableCellsRefresh := False;
  end;
end;

procedure TcxRTTIInspectorController.FocusChanged;
begin
  if FFocusChanging or Inspector.LockRefresh then Exit;
  FFocusChanging := True;
  try
    inherited FocusChanged;
  finally
    FFocusChanging := False;
  end;
end;

procedure TcxRTTIInspectorController.PostValidateFocusedItem;
begin
  FNeedValidate := True;
end;

procedure TcxRTTIInspectorController.SetFocusedItem(
  Value: TcxCustomInplaceEditContainer);
begin
  ValidateFocusingItem(Value);
  inherited SetFocusedItem(Value);
end;

procedure TcxRTTIInspectorController.SetFocusedRowAndCell(Value: TcxCustomRow;
  ACellIndex: Integer);
var
  AEditContainer: TcxCustomInplaceEditContainer;
  ALink: TcxObjectLink;
  AIndex: Integer;
begin
  if VerticalGrid.IsDesigning or Assigned(Value) and not TcxCustomRowAccess(Value).CanFocus then
    Exit;
  if FocusedRow = Value then Exit;
  AIndex := GetRowAbsoluteIndex(Value);
  Inc(FLockUpdate);
  try
    if Value <> nil then
      AEditContainer := TcxPropertyRow(Value).GetEditContainer(ACellIndex)
    else
      AEditContainer := nil;
    if FocusedItem <> AEditContainer then
    begin
      ALink := cxAddObjectLink(Value);
      try
        FocusedItem := AEditContainer as TcxCellEdit;
        if (ALink <> nil) and (ALink.Ref = nil) then
        begin
          if (AIndex >= 0) and (AIndex < Inspector.Rows.Count) then
            Value := Inspector.Rows[AIndex]
          else
            Value := nil;
        end;
      finally
        cxRemoveObjectLink(ALink);
      end;
    end;
  finally
    Dec(FLockUpdate);
    AllowCheckEdit := True;
  end;
  DoUpdateRowAndCell(Value, ACellIndex);
end;

procedure TcxRTTIInspectorController.ValidateFocusingItem(
  var AItem: TcxCustomInplaceEditContainer);
var
  AIndex: Integer;
  APropertyPath: string;
  ARow: TcxPropertyRow;
begin
  AIndex := GetRowAbsoluteIndexFromCellEdit(AItem);
  APropertyPath := GetRowPropertyPathFromCellEdit(AItem);
  if IsEditing then
    EditingController.HideEdit(True);
  if FNeedValidate then
  begin
    FNeedValidate := False;
    AItem := nil;
    if APropertyPath <> '' then
    begin
      ARow := Inspector.FindRowByPropertyPath(APropertyPath, True);
      if ARow <> nil then
        AItem := ARow.EditContainer;
    end;
    if (AItem = nil) and (AIndex >= 0) and (Inspector.Rows.Count > 0) then
    begin
      if AIndex >= Inspector.Rows.Count then
        AIndex := Inspector.Rows.Count - 1;
      if Inspector.Rows[AIndex] is TcxPropertyRow then
        AItem := TcxPropertyRow(Inspector.Rows[AIndex]).EditContainer;
    end;
  end;
end;

function TcxRTTIInspectorController.GetEditingController: TcxRTTIInspectorEditingController;
begin
  Result := TcxRTTIInspectorEditingController(inherited EditingController);
end;

function TcxRTTIInspectorController.GetRowAbsoluteIndex(ARow: TcxCustomRow): Integer;
begin
  if ARow <> nil then
    Result := ARow.AbsoluteIndex
  else
    Result := -1;
end;

function TcxRTTIInspectorController.GetRowAbsoluteIndexFromCellEdit(
  Value: TcxCustomInplaceEditContainer): Integer;
begin
  if Value <> nil then
    Result := TcxCellEdit(Value).Row.AbsoluteIndex
  else
    Result := -1;
end;

function TcxRTTIInspectorController.GetRowPropertyPathFromCellEdit(
  Value: TcxCustomInplaceEditContainer): string;
begin
  if Value <> nil then
    Result := Inspector.GetRowPropertyPath(TcxCellEdit(Value).Row)
  else
    Result := '';
end;

function TcxRTTIInspectorController.GetInspector: TcxCustomRTTIInspector;
begin
  Result := TcxCustomRTTIInspector(EditingControl);
end;

{ TcxRTTIInspectorOptionsView }

constructor TcxRTTIInspectorOptionsView.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FSorted := True;
end;

procedure TcxRTTIInspectorOptionsView.Assign(Source: TPersistent);
begin
  if Source is TcxRTTIInspectorOptionsView then
    FSorted := TcxRTTIInspectorOptionsView(Source).Sorted;
  inherited Assign(Source);
end;

function TcxRTTIInspectorOptionsView.GetDefaultPaintStyle: TcxvgPaintStyle;
begin
  Result := psDelphi;
end;

function TcxRTTIInspectorOptionsView.GetDefaultShowEditButtons: TcxEditingControlEditShowButtons;
begin
  Result := ecsbFocused;
end;

function TcxRTTIInspectorOptionsView.GetInspector: TcxCustomRTTIInspector;
begin
  Result := TcxCustomRTTIInspector(Owner);
end;

procedure TcxRTTIInspectorOptionsView.SetShowReadOnlyProperties(
  const Value: Boolean);
begin
  if FShowReadOnlyProperties <> Value then
  begin
    FShowReadOnlyProperties := Value;
    Inspector.RefreshInspectedProperties;
  end;
end;

procedure TcxRTTIInspectorOptionsView.SetSorted(const Value: Boolean);
begin
  if FSorted <> Value then
  begin
    FSorted := Value;
    Inspector.RefreshInspectedProperties;
  end;
end;

{ TcxRTTIInspectorOptionsBehavior }

constructor TcxRTTIInspectorOptionsBehavior.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  AlwaysShowEditor := True;
end;

{ TcxRTTIInspectorStyles }

procedure TcxRTTIInspectorStyles.GetDefaultViewParams(Index: Integer;
  AData: TObject; out AParams: TcxViewParams);
var
  ARow: TcxPropertyRow;
  AContentInfo: PcxvgContentParamsData absolute AData;

  function IsRootParentComponent: Boolean;
  var
    AParent: TcxCustomRow;
  begin
    Result := True;
    AParent := ARow.Parent;
    while AParent <> nil do
    begin
      if (AParent is TcxPropertyRow) and
        (TcxPropertyRow(AParent).PropertyEditor is TcxComponentProperty) then
      begin
        Result := False;
        Break;
      end;
      AParent := AParent.Parent;
    end;
  end;

begin
  if (Index = vgs_Content) and (AContentInfo <> nil) and AContentInfo.EditorRowProperties.EditProperties.ReadOnly then
    AContentInfo.Focused := False;
  inherited GetDefaultViewParams(Index, AData, AParams);
  with AParams do
    case Index of
      vgs_Content, vgs_ContentEven, vgs_ContentOdd:
        if (AContentInfo <> nil) and (AContentInfo.EditorRowProperties <> nil) then
        begin
          ARow := TcxPropertyRow(AContentInfo.EditorRowProperties.Row);
          if not ARow.IsDefaultValue then
            Font := TcxCustomRTTIInspector(Control).FBoldFont;
        end;
      vgs_Header:
        if AData is TcxPropertyRow then
        begin
          ARow := TcxPropertyRow(AData);
          if ARow.PropertyEditor is TcxComponentProperty then
            TextColor := cxComponentPropertyRowTextColor
          else
            if IsRootParentComponent then
              TextColor := CalcHelper.GetHeaderTextColor
            else
              TextColor := cxSubComponentPropertyRowTextColor;
        end;
    end;
end;

{ TcxCustomRTTIInspector }

constructor TcxCustomRTTIInspector.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OptionsBehavior.CellHints := True;
  OptionsBehavior.AlwaysShowEditor := True;
  FBoldFont := TFont.Create;
  FBoldFont.Assign(Font);
  FBoldFont.Style := FBoldFont.Style + [fsBold];
end;

destructor TcxCustomRTTIInspector.Destroy;
begin
  if FListeners <> nil then
  begin
    CloseNonModalEditors;
    FListeners.Free;
  end;
  ReleaseComponentProperties;
  FBoldFont.Free;
  inherited Destroy;
end;

procedure TcxCustomRTTIInspector.AddListener(AListener: TPersistent);
begin
  if FListeners = nil then FListeners := TList.Create;
  if (AListener <> nil) and (FListeners.IndexOf(AListener) < 0) then
    FListeners.Add(AListener)
end;

procedure TcxCustomRTTIInspector.CloseNonModalEditors;
var
  I: Integer;
  AIntf: IcxRTTIInspectorHelper;
begin
  if FListeners <> nil then
    for I := 0 to FListeners.Count - 1 do
      if Supports(TObject(FListeners[I]), IcxRTTIInspectorHelper, AIntf) then
      begin
        AIntf.CloseNonModal(Self);
        AIntf := nil;
      end;
end;

procedure TcxCustomRTTIInspector.RefreshInspectedProperties;

  function GetExpandedRows: TStringList;
  var
    I: Integer;
    ARow: TcxCustomRow;
  begin
    Result := TStringList.Create;
    for I := 0 to Rows.Count - 1 do
    begin
      ARow := Rows[I];
      if ARow.Expanded then
        Result.Add(GetRowPropertyPath(ARow));
    end;
  end;

  procedure SetExpandedRows(AList: TStringList);
  var
    I, J: Integer;
    ARow: TcxCustomRow;
    AFullName: string;
  begin
    if AList.Count = 0 then
      Exit;
    for I := 0 to Rows.Count - 1 do
    begin
      ARow := Rows[I];
      AFullName := GetRowPropertyPath(ARow);
      for J := 0 to AList.Count - 1 do
        if AList[J] = AFullName then
          ARow.Expanded := True;
    end;
  end;

var
  AExpandState: TStringList;
  APropertyName: string;
  ASaveTopRowIndex: Integer;
  AFocused: Boolean;
begin
  if ComponentState * [csDesigning, csLoading] <> [] then
    Exit;
  FLockRefresh := True;
  AFocused := IsFocused;
  APropertyName := GetRowPropertyPath(FocusedRow);
  ASaveTopRowIndex := TopVisibleRowIndex;
  BeginUpdate;
  try
    AExpandState := GetExpandedRows;
    try
      CreatePropertyRows(InspectedObject);
      SetExpandedRows(AExpandState);
    finally
      AExpandState.Free;
    end;
    Controller.Scroller.RecalcBandsInfo;
    if AFocused and CanFocusEx then
      SetFocus;
    TopVisibleRowIndex := ASaveTopRowIndex;
  finally
    FLockRefresh := False;
    FocusRowByPropertyPath(APropertyName);
    EndUpdate;
  end;
end;

procedure TcxCustomRTTIInspector.RemoveListener(AListener: TPersistent);
begin
  if (FListeners <> nil) and (FListeners.IndexOf(AListener) >= 0) then
    FListeners.Remove(AListener);
end;

procedure TcxCustomRTTIInspector.Notification(AComponent: TComponent; AOperation: TOperation);
begin
  inherited Notification(AComponent, AOperation);
  if (AOperation = opRemove) and (AComponent = InspectedObject) then
    InspectedObject := nil;
end;

procedure TcxCustomRTTIInspector.DataChanged;
begin
end;

procedure TcxCustomRTTIInspector.FontChanged;
begin
  FBoldFont.Assign(Font);
  FBoldFont.Style := FBoldFont.Style + [fsBold];
  inherited FontChanged;
end;

function TcxCustomRTTIInspector.InternalMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
var
  ARow: TcxCustomRow;
begin
  if (Shift = [ssShift]) and not IsScrollingContent then
  begin
    if WheelDelta > 0 then
      ARow := PrevVisibleRow(FocusedRow)
    else
      ARow := NextVisibleRow(FocusedRow);
    if ARow <> nil then FocusedRow := ARow;
    Result := ARow <> nil;
  end
  else
    Result := inherited InternalMouseWheel(Shift, WheelDelta, MousePos);
end;

function TcxCustomRTTIInspector.IsMouseWheelHandleNeeded(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
begin
  Result := inherited IsMouseWheelHandleNeeded(Shift, WheelDelta, MousePos) or
    ((Shift = [ssShift]) and not IsScrollingContent);
end;

procedure TcxCustomRTTIInspector.Loaded;
begin
  inherited Loaded;
  if not IsDesigning and Assigned(InspectedObject) then
  begin
    HandleNeeded;
    CreatePropertyRows(nil);
  end;
end;

procedure TcxCustomRTTIInspector.BeginLevel;
begin
  Inc(FInspectedLevel);
end;

procedure TcxCustomRTTIInspector.EndLevel;
begin
  Dec(FInspectedLevel);
end;

function TcxCustomRTTIInspector.FilterProperty(const APropertyName: string): Boolean;
begin
  Result := True;
  if Assigned(FOnFilterProperty) then FOnFilterProperty(Self, APropertyName, Result);
end;

function TcxCustomRTTIInspector.FilterPropertyEx(const AFullPropertyName: string): Boolean;
begin
  Result := True;
  if Assigned(FOnFilterPropertyEx) then FOnFilterPropertyEx(Self, AFullPropertyName, Result);
end;

function TcxCustomRTTIInspector.FindRowByPropertyName(const APropertyName: string): TcxPropertyRow;
var
  I: Integer;
begin
  if APropertyName <> '' then
    for I := 0 to Rows.Count - 1 do
      if Rows[I] is TcxPropertyRow then
      begin
        Result := TcxPropertyRow(Rows[I]);
        if CompareText(APropertyName, Result.PropertyEditor.GetName) = 0 then
          Exit;
      end;
  Result := nil;
end;

function TcxCustomRTTIInspector.FindRowByPropertyPath(APropertyPath: string; AExactMatch: Boolean): TcxPropertyRow;
var
  I, L: Integer;
  ARow: TcxCustomRow;
begin
  Result := nil;
  if APropertyPath = '' then
    Exit;
  for I := 0 to Rows.Count - 1 do
  begin
    ARow := Rows[I];
    if (APropertyPath = GetRowPropertyPath(ARow)) and (ARow is TcxPropertyRow) then
    begin
      Result := TcxPropertyRow(ARow);
      Exit;
    end;
  end;
  if AExactMatch then
    Exit;

  repeat
    for I := 0 to Rows.Count - 1 do
    begin
      ARow := Rows[I];
      if (AnsiPos(APropertyPath, GetRowPropertyPath(ARow)) = 1) and (ARow is TcxPropertyRow) then
      begin
        Result := TcxPropertyRow(ARow);
        Exit;
      end;
    end;
    L := LastDelimiter('.', APropertyPath);
    if L > 0 then
      APropertyPath := Copy(APropertyPath, 1, L - 1)
    else
      Exit;
  until APropertyPath = '';
end;

procedure TcxCustomRTTIInspector.FocusRowByPropertyName(const APropertyName: string);
var
  ARow: TcxCustomRow;
begin
  ARow := FindRowByPropertyName(APropertyName);
  if ARow = nil then ARow := FirstVisibleRow;
  FocusedRow := ARow;
end;

procedure TcxCustomRTTIInspector.FocusRowByPropertyPath(APropertyPath: string);
var
  ARow: TcxCustomRow;
begin
  ARow := FindRowByPropertyPath(APropertyPath, False);
  if ARow <> nil then
    FocusRow(ARow);
end;

function TcxCustomRTTIInspector.GetEditPropertiesClass(APropertyEditor: TcxPropertyEditor): TcxCustomEditPropertiesClass;
var
  AFullPropertyName: string;
  ARow: TcxPropertyRow;
begin
  AFullPropertyName := APropertyEditor.GetName;
  ARow := TcxPropertyRow(FParentRow);
  while ARow <> nil do
  begin
    if ARow.PropertyEditor <> nil then
    begin
      if AFullPropertyName <> '' then
        AFullPropertyName := '.' + AFullPropertyName;
      AFullPropertyName := ARow.PropertyEditor.GetName + AFullPropertyName;
    end;
    ARow := TcxPropertyRow(ARow.Parent);
  end;
  if FilterProperty(APropertyEditor.GetName) and FilterPropertyEx(AFullPropertyName) then
    Result := GetEditPropertiesClassCore(APropertyEditor)
  else
    Result := nil;
end;

function TcxCustomRTTIInspector.GetEditPropertiesClassCore(APropertyEditor: TcxPropertyEditor): TcxCustomEditPropertiesClass;
begin
  Result := cxGetPropertiesClassByEditor(APropertyEditor);
  if (Result <> nil) and Result.InheritsFrom(TcxColorComboBoxProperties) then
    Exit;
  if ipaDialog in APropertyEditor.GetAttributes then
    Result := TcxButtonEditProperties;
  if ipaValueList in APropertyEditor.GetAttributes then
    Result := TcxPropertyComboBoxProperties;
  if Result = nil then
    Result := TcxTextEditProperties
end;

function TcxCustomRTTIInspector.GetRowPropertyPath(ARow: TcxCustomRow): string;
begin
  Result := '';
  while ARow <> nil do
  begin
    if ARow is TcxPropertyRow then
      if Result = '' then
        Result := TcxPropertyRow(ARow).Properties.Caption
      else
        Result := TcxPropertyRow(ARow).Properties.Caption + '.' + Result;
    ARow := ARow.Parent;
  end;
  if Result <> '' then
    Result := AnsiUpperCase(Result);
end;

procedure TcxCustomRTTIInspector.PostChangedNotification;
begin
  PostMessage(Handle, DXM_VG_PROPERTYCHANGED, 0, 0);
end;

procedure TcxCustomRTTIInspector.PrepareEditProperties(
  AProperties: TcxCustomEditProperties; APropertyEditor: TcxPropertyEditor);
begin
  //todo: need cxEditors fix
  TcxCustomEditPropertiesAccess(AProperties).ClickKey := 0;
  //remove ipaSubProperties for TcxVariantProperty
  TcxCustomEditPropertiesAccess(AProperties).ReadOnly := ([ipaReadOnly] * APropertyEditor.GetAttributes <> []);
  TcxCustomEditPropertiesAccess(AProperties).UseMouseWheel := False;
  TcxCustomEditPropertiesAccess(AProperties).ValidateOnEnter := True;
  TcxCustomEditPropertiesAccess(AProperties).OnChange := EditChange;
  TcxCustomEditPropertiesAccess(AProperties).OnEditValueChanged := EditValueChanged;

  if AProperties is TcxCustomTextEditProperties then
    TcxCustomTextEditProperties(AProperties).MaxLength := APropertyEditor.GetEditLimit;

  if AProperties is TcxComboBoxProperties then
  begin
    APropertyEditor.GetValues(GetStrProc);
    TcxComboBoxProperties(AProperties).DropDownAutoWidth := True;
    TcxComboBoxProperties(AProperties).ImmediateDropDown := False;
    TcxComboBoxProperties(AProperties).Revertable := ipaRevertable in APropertyEditor.GetAttributes;
  end;

  if AProperties is TcxColorComboBoxProperties then
  begin
    TcxColorComboBoxPropertiesAccess(AProperties).ColorDialogShowFull := True;
    TcxColorComboBoxPropertiesAccess(AProperties).ColorDialogType := cxcdtAdvanced;
    TcxColorComboBoxPropertiesAccess(AProperties).ColorValueFormat := cxcvHexadecimal;
    TcxColorComboBoxPropertiesAccess(AProperties).DropDownListStyle := lsEditList;
    TcxColorComboBoxPropertiesAccess(AProperties).MaxMRUColors := 0;
    TcxColorComboBoxPropertiesAccess(AProperties).ColorBoxWidth := ScaleFactor.Apply(16);
    TcxColorComboBoxPropertiesAccess(AProperties).PrepareDelphiColorList(False, False);
  end;

  if AProperties is TcxButtonEditProperties then
    TcxButtonEditProperties(AProperties).OnButtonClick := RowButtonClick;

  if AProperties is TcxSpinEditProperties then
    TcxSpinEditProperties(AProperties).UseCtrlIncrement := True;

  APropertyEditor.AdjustInnerEditProperties(AProperties);
end;

function TcxCustomRTTIInspector.Sorted: Boolean;
begin
  Result := OptionsView.Sorted;
end;

procedure TcxCustomRTTIInspector.DoPropertyChanged;
var
  I: Integer;
  AIntf: IcxRTTIInspectorHelper;
begin
  if FListeners <> nil then
    for I := 0 to FListeners.Count - 1 do
      if Supports(TObject(FListeners[I]), IcxRTTIInspectorHelper, AIntf) then
      begin
        AIntf.PropertyChanged(Self);
        AIntf := nil;
      end;
  if Assigned(FOnPropertyChanged) then FOnPropertyChanged(Self);
end;

procedure TcxCustomRTTIInspector.EditChange(Sender: TObject);
begin
  if (PropertyEditor <> nil) and (ipaAutoUpdate in PropertyEditor.GetAttributes) then
  begin
    if Sender is TcxCustomTextEdit then
    begin
      try
        PropertyEditor.Value := TcxCustomTextEdit(Sender).Text;
        DoPropertyChanged;
      except
        TcxCustomTextEdit(Sender).Text := PropertyEditor.GetValue;
        raise
      end;
    end
    else
      TrySetValue(TcxCustomEdit(Sender), True);
  end;
end;

procedure TcxCustomRTTIInspector.EditValueChanged(Sender: TObject);
begin
  if not ((PropertyEditor <> nil) and (ipaAutoUpdate in PropertyEditor.GetAttributes)) then
    TrySetValue(TcxCustomEdit(Sender), False);
  Controller.CheckPostData;
end;

function TcxCustomRTTIInspector.GetControllerClass: TcxCustomControlControllerClass;
begin
  Result := TcxRTTIInspectorController;
end;

function TcxCustomRTTIInspector.GetControlStylesClass: TcxCustomControlStylesClass;
begin
  Result := TcxRTTIInspectorStyles;
end;

function TcxCustomRTTIInspector.GetEditingControllerClass: TcxEditingControllerClass;
begin
  Result := TcxRTTIInspectorEditingController;
end;

function TcxCustomRTTIInspector.GetOptionsBehaviorClass: TcxControlOptionsBehaviorClass;
begin
  Result := TcxRTTIInspectorOptionsBehavior;
end;

function TcxCustomRTTIInspector.GetOptionsView: TcxRTTIInspectorOptionsView;
begin
  Result := TcxRTTIInspectorOptionsView(inherited OptionsView);
end;

function TcxCustomRTTIInspector.GetOptionsViewClass: TcxControlOptionsViewClass;
begin
  Result := TcxRTTIInspectorOptionsView;
end;

procedure TcxCustomRTTIInspector.LookAndFeelChanged(Sender: TcxLookAndFeel;
  AChangedValues: TcxLookAndFeelValues);
begin
  inherited;
  if AChangedValues = [lfvKind..lfvSkinName] then
    RefreshInspectedProperties;
end;

function TcxCustomRTTIInspector.CanInvokePropertyEditorDlg: Boolean;
begin
  Result := (PropertyEditor <> nil) and
    ((ipaDialog in PropertyEditor.GetAttributes) or (ipaValueList in PropertyEditor.GetAttributes));
end;

procedure TcxCustomRTTIInspector.CNPropertyChanged(var AMessage: TMsg);
begin
  DoPropertyChanged;
end;

procedure TcxCustomRTTIInspector.CreatePropertyRows(AOldInspectedObject: TPersistent);
begin
  BeginUpdate;
  try
    if AOldInspectedObject <> nil then
      ReleaseComponentProperties;
    if FInspectedObject <> nil then
    begin
      FInspectedLevel := 0;
      GetComponentsProperties([FInspectedObject]);
    end;
  finally
    EndUpdate;
  end;
end;

procedure TcxCustomRTTIInspector.CreateRows(APropertyEditor: TcxPropertyEditor);
var
  AEditPropertiesClass: TcxCustomEditPropertiesClass;
begin
  AEditPropertiesClass := GetEditPropertiesClass(APropertyEditor);
  if AEditPropertiesClass = nil then
  begin
    APropertyEditor.Free;
    Exit;
  end;
  FCurrentRow := TcxPropertyRow(AddChild(FParentRow, TcxPropertyRow));
  with FCurrentRow.Properties do
  begin
    Caption := APropertyEditor.GetName;
    FCurrentRow.FIsDefaultValue := APropertyEditor.IsDefaultValue;
    EditPropertiesClass := AEditPropertiesClass;
    PrepareEditProperties(EditProperties, APropertyEditor);
  end;
  FCurrentRow.FPropertyEditor := APropertyEditor;
  FCurrentRow.Properties.Value := APropertyEditor.Value;
  if ipaSubProperties in APropertyEditor.GetAttributes then
  begin
    FParentRow := FCurrentRow;
    APropertyEditor.GetProperties(APropertyEditor.FOwner, CreateRows);
    FParentRow := FParentRow.Parent; //check for nil
  end;
end;

procedure TcxCustomRTTIInspector.GetComponentsProperties(
  const AInstances: array of TPersistent);

  function FindRootOwner(APersistent: TPersistent): TComponent;
  begin
    if (APersistent is TComponent) then
      Result := TComponent(APersistent).Owner
    else Result := nil;
    if Result <> nil then
      while (Result.Owner <> nil) and not (Result is TDataModule) and
        not (Result is TCustomForm) and not (Result is TCustomFrame) do
        Result := Result.Owner;
  end;

var
  ComponentList: TcxComponentList;
  I: Integer;
  AOwner: TComponent;
begin
  ComponentList := TcxComponentList.Create;
  try
    AOwner := FindRootOwner(AInstances[Low(AInstances)]);
    for I := Low(AInstances) to High(AInstances) do
    begin
      ComponentList.Add(TPersistent(AInstances[I]));
      if (AOwner <> nil) then
      begin
        if FindRootOwner(AInstances[I]) <> AOwner then
          AOwner := nil;
      end else AOwner := nil;
    end;
    if AOwner = nil then
      AOwner := self;
    cxGetComponentProperties(AOwner, Self, ComponentList, tkProperties, CreateRows);
    FullCollapse;
    FParentRow := nil;
    FCurrentRow := nil;
  finally
    ComponentList.Free;
  end;
end;

function TcxCustomRTTIInspector.GetController: TcxRTTIInspectorController;
begin
  Result := TcxRTTIInspectorController(FController);
end;

function TcxCustomRTTIInspector.GetPropertyEditor: TcxPropertyEditor;
begin
  if FocusedRow <> nil then
    Result := TcxPropertyRow(FocusedRow).PropertyEditor
  else
    Result := nil;
end;

procedure TcxCustomRTTIInspector.GetStrProc(const S: string);
begin
  TcxComboBoxProperties(TcxPropertyRow(FCurrentRow).Properties.EditProperties).Items.Add(S);
end;

procedure TcxCustomRTTIInspector.ReleaseComponentProperties;
var
  I: Integer;
  ARow: TcxCustomRow;
begin
  for I := 0 to Rows.Count - 1 do
  begin
    ARow := Rows[I];
    if ARow is TcxPropertyRow then
      with TcxPropertyRow(ARow) do
      begin
        if PropertyEditor is TcxPropertyEditor then
        begin
          PropertyEditor.Free;
          FPropertyEditor := nil;
        end;
      end;
  end;
  if not (csDestroying in ComponentState) then
    ClearRows;
end;

procedure TcxCustomRTTIInspector.RowButtonClick(Sender: TObject; AbsoluteIndex: Integer);
begin
  TryInvokePropertyEditorDlg;
end;

procedure TcxCustomRTTIInspector.SetInspectedObject(Value: TPersistent);
var
  APrevInspectedObject: TPersistent;
begin
  if (FInspectedObject <> Value) and IsValidInspectedObject(Value, Self) then
  begin
    if not (csDestroying in ComponentState) then
      APrevInspectedObject := FInspectedObject
    else
      APrevInspectedObject := nil;
    if (APrevInspectedObject <> nil) and not OptionsData.CancelOnExit and
       Controller.IsEditing then
      HideEdit;
    FInspectedObject := Value;
    if (FInspectedObject <> nil) and (FInspectedObject is TComponent) then
      TComponent(Value).FreeNotification(Self);
    if ([csDesigning, csLoading, csDestroying] * ComponentState) = [] then
      CreatePropertyRows(APrevInspectedObject);
  end;
end;

procedure TcxCustomRTTIInspector.SetOptionsView(
  const Value: TcxRTTIInspectorOptionsView);
begin
  inherited OptionsView := Value;
end;

procedure TcxCustomRTTIInspector.TryInvokePropertyEditorDlg;
begin
  if CanInvokePropertyEditorDlg then
  begin
    PropertyEditor.Edit;
    RefreshInspectedProperties;
  end;
end;

function TcxCustomRTTIInspector.TrySetValue(
  AEdit: TcxCustomEdit; AUseText: Boolean): Boolean;
var
  V: Variant;
begin
  Result := False;
  if FSettingValue or (PropertyEditor = nil) then Exit;
  FSettingValue := True;
  try
    V := PropertyEditor.Value;
    try
      if AUseText and (AEdit is TcxCustomTextEdit) then
        PropertyEditor.Value := TcxCustomTextEdit(AEdit).Text
      else
        PropertyEditor.Value := AEdit.EditValue;
      Result := True;
    except
      PropertyEditor.Value := V;
      AEdit.EditValue := V;
      raise;
    end;
    DoPropertyChanged;
    RefreshInspectedProperties;
  finally
    FSettingValue := False;
  end;
end;

procedure cxDotNetInspectObject(AObject: TPersistent; AInspector: TcxRTTIInspector);
var
  I: Integer;
  C: TcxCategoryRow;
begin
  if AObject <> nil then
    with AInspector do
    begin
      InspectedObject := nil;
      BeginUpdate;
      try
        InspectedObject := AObject;
        OptionsBehavior.AlwaysShowEditor := False;
        OptionsView.GridLineColor := clBtnFace;
        OptionsView.PaintStyle := psdotNet;
        C := TcxCategoryRow(Add(TcxCategoryRow));
        with C do
        begin
          if (AObject is TComponent) and (TComponent(AObject).Name <> '') then
            Properties.Caption := TComponent(AObject).Name + ': ' + AObject.ClassName
          else
            Properties.Caption := AObject.ClassName;
          Properties.HeaderAlignmentVert := vaCenter;
          Index := 0;
        end;
        for I := 1 to Rows.Count - 1 do
          if Rows[I].Level = 0 then
            Rows[I].Parent := C;
      finally
        FocusedRow := FirstVisibleRow;
        EndUpdate;
      end;
    end;
end;

{ TcxInspectedObjectPropertyEditor }

function TcxInspectedObjectPropertyEditor.IsValidComponent(AComponent: TComponent): Boolean;
begin
  Result := inherited IsValidComponent(AComponent) and
    IsValidInspectedObject(AComponent, Inspector);
end;

initialization
  GetRegisteredEditProperties.Register(TcxPropertyComboBoxProperties, '');
  cxRegisterPropertyEditor(TypeInfo(TColor), nil, '', TcxColorProperty);
  cxRegisterPropertyEditor(TypeInfo(TFont), nil, '', TcxFontProperty);
  cxRegisterPropertyEditor(TypeInfo(TFontCharset), nil, '', TcxFontCharsetProperty);
  cxRegisterPropertyEditor(TypeInfo(TFontName), nil, '', TcxFontNameProperty);
  cxRegisterPropertyEditor(TypeInfo(TCursor), nil, '', TcxCursorProperty);
  cxRegisterPropertyEditor(TypeInfo(string), TMediaPlayer, 'FileName', TcxMPFilenameProperty);
  cxRegisterPropertyEditor(TypeInfo(TCaption), nil, '', TcxCaptionProperty);
  cxRegisterPropertyEditor(TypeInfo(TComponent), nil, '', TcxComponentProperty);
  cxRegisterPropertyEditor(TypeInfo(TComponentName), nil, '', TcxComponentNameProperty);
  cxRegisterPropertyEditor(TypeInfo(TImeName), nil, '', TcxImeNameProperty);
  cxRegisterPropertyEditor(TypeInfo(TModalResult), nil, '', TcxModalResultProperty);
  cxRegisterPropertyEditor(TypeInfo(TShortCut), nil, '', TcxShortCutProperty);
  cxRegisterPropertyEditor(TypeInfo(TTabOrder), nil, '', TcxTabOrderProperty);
  cxRegisterPropertyEditor(TypeInfo(TDate), nil, '', TcxDateProperty);
  cxRegisterPropertyEditor(TypeInfo(TTime), nil, '', TcxTimeProperty);
  cxRegisterPropertyEditor(TypeInfo(TDateTime), nil, '', TcxDateTimeProperty);
  cxRegisterPropertyEditor(TypeInfo(TStrings), nil, '', TcxStringsProperty);
  cxRegisterPropertyEditor(TypeInfo(TPicture), nil, '', TcxPictureProperty);
  cxRegisterPropertyEditor(TypeInfo(TBitmap), nil, '', TcxGraphicProperty);
  cxRegisterPropertyEditor(TypeInfo(Variant), nil, '', TcxVariantProperty);
  cxRegisterPropertyEditor(TypeInfo(TdxSmartGlyph), nil, '', TdxSmartGlyphProperty);

  cxRegisterPropertyEditor(TypeInfo(TPersistent),
    TcxCustomRTTIInspector, 'InspectedObject', TcxInspectedObjectPropertyEditor);

  cxRegisterEditPropertiesClass(TcxCaptionProperty, TcxTextEditProperties);
  cxRegisterEditPropertiesClass(TcxColorProperty, TcxColorComboBoxProperties);
  cxRegisterEditPropertiesClass(TcxComponentNameProperty, TcxTextEditProperties);
  cxRegisterEditPropertiesClass(TcxComponentProperty, TcxPropertyComboBoxProperties);
  cxRegisterEditPropertiesClass(TcxCursorProperty, TcxPropertyComboBoxProperties);
  cxRegisterEditPropertiesClass(TcxDateProperty, TcxDateEditProperties);
  cxRegisterEditPropertiesClass(TcxDateTimeProperty, TcxTextEditProperties);
  cxRegisterEditPropertiesClass(TcxFontCharsetProperty, TcxPropertyComboBoxProperties);
  cxRegisterEditPropertiesClass(TcxFontNameProperty, TcxPropertyComboBoxProperties);
  cxRegisterEditPropertiesClass(TcxFontProperty, TcxTextEditProperties);
  cxRegisterEditPropertiesClass(TcxIntegerProperty, TcxSpinEditProperties);
  cxRegisterEditPropertiesClass(TcxImeNameProperty, TcxTextEditProperties);
  cxRegisterEditPropertiesClass(TcxModalResultProperty, TcxPropertyComboBoxProperties);
  cxRegisterEditPropertiesClass(TcxMPFilenameProperty, TcxTextEditProperties);
  cxRegisterEditPropertiesClass(TcxPictureProperty, TcxButtonEditProperties);
  cxRegisterEditPropertiesClass(TcxShortCutProperty, TcxPropertyComboBoxProperties);
  cxRegisterEditPropertiesClass(TcxStringsProperty, TcxButtonEditProperties);
  cxRegisterEditPropertiesClass(TcxTabOrderProperty, TcxSpinEditProperties);
  cxRegisterEditPropertiesClass(TcxTimeProperty, TcxTimeEditProperties);
  //
  cxRegisterPropertyEditor(TypeInfo(string), TcxCustomEditorRowProperties, 'EditPropertiesClassName', nil);
  cxRegisterPropertyEditor(TypeInfo(TcxCustomEditProperties), TcxCustomEditorRowProperties, 'EditProperties', TcxEditPropertiesProperty);

  cxRegisterPropertyEditor(TypeInfo(TdxSkinName), nil, 'SkinName', TcxSkinNameProperty);

finalization
  ListFreeAndNil(FEditPropertiesClasses);
  ListFreeAndNil(FPropertyClasses);
  ListFreeAndNil(FPropertyMappers);
  GetRegisteredEditProperties.Unregister(TcxPropertyComboBoxProperties);

end.
