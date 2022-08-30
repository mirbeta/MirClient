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

unit cxDBEdit;

{$I cxVer.inc}

interface

uses
  Variants, Windows, Messages, Classes, Controls, DB, DBCtrls, SysUtils,
  dxCore, dxCoreClasses, cxClasses, cxContainer,
  cxCustomData, cxDataUtils, cxDB,
  cxEdit, cxBlobEdit, cxButtonEdit, cxCalc, cxCalendar, cxCheckBox, cxCurrencyEdit,
  cxDropDownEdit, cxHyperLinkEdit, cxImage, cxImageComboBox, cxListBox, cxMaskEdit,
  cxMemo, cxMRUEdit, cxRadioGroup, cxSpinEdit, cxTextEdit, cxTimeEdit;

type
  TcxDBTextEdit = class;

  { TcxCustomDBEditDefaultValuesProvider }

  TcxCustomDBEditDefaultValuesProvider = class(TcxCustomEditDefaultValuesProvider)
  private
    FDataSource: TDataSource;
    FField: TField;
    FFreeNotifier: TcxFreeNotificator;
    procedure FieldFreeNotification(Sender: TComponent);
    procedure SetDataSource(Value: TDataSource);
    procedure SetField(Value: TField);
    function IsDefaultDataAvailable: Boolean;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    function CanSetEditMode: Boolean; override;
    function DefaultAlignment: TAlignment; override;
    function DefaultBlobKind: TcxBlobKind; override;
    function DefaultCanModify: Boolean; override;
    function DefaultDisplayFormat: string; override;
    function DefaultEditFormat: string; override;
    function DefaultEditMask: string; override;
    function DefaultIsFloatValue: Boolean; override;
    function DefaultMaxLength: Integer; override;
    function DefaultMaxValue: Double; override;
    function DefaultMinValue: Double; override;
    function DefaultPrecision: Integer; override;
    function DefaultReadOnly: Boolean; override;
    function DefaultRequired: Boolean; override;
    function IsBlob: Boolean; override;
    function IsCurrency: Boolean; override;
    function IsDataAvailable: Boolean; override;
    function IsDataStorage: Boolean; override;
    function IsDisplayFormatDefined(AIsCurrencyValueAccepted: Boolean): Boolean; override;
    function IsOnGetTextAssigned: Boolean; override;
    function IsOnSetTextAssigned: Boolean; override;
    function IsValidChar(AChar: Char): Boolean; override;
    property DataSource: TDataSource read FDataSource write SetDataSource;
    property Field: TField read FField write SetField;
  end;

  { TcxDBFieldDataLink }

  TcxDBFieldDataLink = class(TcxCustomFieldDataLink)
  protected
    procedure FocusControl(Field: TFieldRef); override;
    procedure VisualControlChanged; override;
    procedure UpdateRightToLeft; override;
  end;

  { TcxEditFieldDataLink }

  TcxDBEditDataBinding = class;

  TcxEditFieldDataLink = class(TDataLink)
  private
    FActiveChanging: Boolean;
    FControl: TComponent;
    FDataBinding: TcxDBEditDataBinding;
    FEditing: Boolean;
    FFreeNotifier: TcxFreeNotificator;
    FModified: Boolean;

    FDataField: TField;
    FFieldName: string;
    FMasterField: TField;
    FPropertiesField: TField;

    procedure FieldFreeNotification(Sender: TComponent);
    function GetCanModify: Boolean;
    procedure InternalSetField(ADataField, APropertiesField: TField);
    procedure SetEditing(Value: Boolean);
    procedure SetField(ADataField, APropertiesField: TField);
    procedure SetFieldName(const Value: string);
    procedure UpdateField;
    procedure UpdateMasterField;
    procedure UpdateRightToLeft;
  protected
    procedure DataEvent(Event: TDataEvent; Info: TdxNativeInt); override;

    procedure FocusControl(Field: TFieldRef); override;
    procedure UpdateData; override;
    procedure ActiveChanged; override;
    procedure EditingChanged; override;
    procedure LayoutChanged; override;
    procedure RecordChanged(AField: TField); override;

    procedure ResetModified;

    property DataField: TField read FDataField;
    property PropertiesField: TField read FPropertiesField;
  public
    constructor Create(ADataBinding: TcxDBEditDataBinding);
    destructor Destroy; override;

    function Edit: Boolean;
    procedure Modified;
    procedure Reset;

    property CanModify: Boolean read GetCanModify;
    property Control: TComponent read FControl write FControl;
    property Editing: Boolean read FEditing;

    property Field: TField read FDataField;
    property FieldName: string read FFieldName write SetFieldName;
  end;

   TdxDataEvents = set of TDataEvent;

  { TcxDBEditDataBinding }

  TcxDBEditDataBinding = class(TcxEditDataBinding)
  private
    FDataLink: TcxEditFieldDataLink;
    FRefreshCount: Integer;
    FReceivedEvents: TdxDataEvents;

    function GetDataFieldName: string;
    function GetDataSource: TDataSource;
    function GetDefaultValuesProvider: TcxCustomDBEditDefaultValuesProvider;
    function GetDataField: TField;
    function GetPropertiesField: TField;
    procedure SetDataFieldName(const Value: string);
    procedure SetDataSource(Value: TDataSource);
  protected
    procedure DefaultValuesChanged; override;
    function GetEditing: Boolean; override;
    function GetIsDataAvailable: Boolean; override;
    function GetModified: Boolean; override;
    function GetStoredValue: TcxEditValue; override;
    function IsNull: Boolean; override;
    function IsInnerControlHaveToBeReadOnly: Boolean; override;
    procedure Reset; override;
    procedure SetDisplayValue(const Value: TcxEditValue); override;
    function SetEditMode: Boolean; override;
    procedure SetStoredValue(const Value: TcxEditValue); override;

    procedure DataEvent(AEvent: TDataEvent; AInfo: TdxNativeInt);
    procedure DisableRefresh;
    procedure EnableRefresh;
    function IsRefreshDisabled: Boolean;
    procedure DataChanged; virtual;
    procedure DataSetChange; virtual;
    procedure EditingChanged; virtual;
    function IsLookupControl: Boolean; virtual;
    procedure UpdateData; virtual;

    property DefaultValuesProvider: TcxCustomDBEditDefaultValuesProvider read GetDefaultValuesProvider;
    property PropertiesField: TField read GetPropertiesField;
  public
    constructor Create(AEdit: TcxCustomEdit); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
    function CanCheckEditorValue: Boolean; override;
    function CanModify: Boolean; override;
    function CanPostEditorValue: Boolean; override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    class function GetDefaultValuesProviderClass: TcxCustomEditDefaultValuesProviderClass; override;
    procedure SetModified; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    procedure UpdateDisplayValue; override;
    procedure UpdateNotConnectedDBEditDisplayValue; override;

    property DataLink: TcxEditFieldDataLink read FDataLink;
    property Field: TField read GetDataField;
  published
    property DataField: string read GetDataFieldName write SetDataFieldName;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
  end;

  { TcxDBTextEditDataBinding }

  TcxDBTextEditDataBinding = class(TcxDBEditDataBinding)
  protected
    procedure SetDisplayValue(const Value: TcxEditValue); override;
  public
    procedure UpdateNotConnectedDBEditDisplayValue; override;
  end;

  { TcxDBTextEdit }


  TcxDBTextEdit = class(TcxCustomTextEdit)
  private

    function GetActiveProperties: TcxTextEditProperties;
    function GetDataBinding: TcxDBTextEditDataBinding;
    function GetProperties: TcxTextEditProperties;
    procedure SetDataBinding(Value: TcxDBTextEditDataBinding);
    procedure SetProperties(Value: TcxTextEditProperties);
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
  protected
    class function GetDataBindingClass: TcxEditDataBindingClass; override;
    function SupportsSpelling: Boolean; override;
  public
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    property ActiveProperties: TcxTextEditProperties read GetActiveProperties;

  published
    property Anchors;
    property AutoSize;
    property BeepOnEnter;
    property BiDiMode;
    property Constraints;
    property DataBinding: TcxDBTextEditDataBinding read GetDataBinding write SetDataBinding;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Properties: TcxTextEditProperties read GetProperties write SetProperties;
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
    property OnStartDrag;
    property OnEndDock;
    property OnStartDock;
  end;

  { TcxDBMemo }

  TcxDBMemo = class(TcxCustomMemo)
  private
    function GetActiveProperties: TcxMemoProperties;
    function GetDataBinding: TcxDBTextEditDataBinding;
    function GetProperties: TcxMemoProperties;
    procedure SetDataBinding(Value: TcxDBTextEditDataBinding);
    procedure SetProperties(Value: TcxMemoProperties);
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
  protected
    class function GetDataBindingClass: TcxEditDataBindingClass; override;
  public
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    property ActiveProperties: TcxMemoProperties read GetActiveProperties;
  published
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DataBinding: TcxDBTextEditDataBinding read GetDataBinding write SetDataBinding;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Properties: TcxMemoProperties read GetProperties write SetProperties;
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

  { TcxDBMaskEdit }

  TcxDBMaskEdit = class(TcxCustomMaskEdit)
  private
    function GetActiveProperties: TcxMaskEditProperties;
    function GetDataBinding: TcxDBTextEditDataBinding;
    function GetProperties: TcxMaskEditProperties;
    procedure SetDataBinding(Value: TcxDBTextEditDataBinding);
    procedure SetProperties(Value: TcxMaskEditProperties);
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
  protected
    class function GetDataBindingClass: TcxEditDataBindingClass; override;
    function SupportsSpelling: Boolean; override;
  public
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    property ActiveProperties: TcxMaskEditProperties read GetActiveProperties;
  published
    property Anchors;
    property AutoSize;
    property BeepOnEnter;
    property Constraints;
    property DataBinding: TcxDBTextEditDataBinding read GetDataBinding write SetDataBinding;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Properties: TcxMaskEditProperties read GetProperties write SetProperties;
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
    property OnStartDrag;
    property OnEndDock;
    property OnStartDock;
  end;

  { TcxDBButtonEdit }

  TcxDBButtonEdit = class(TcxCustomButtonEdit)
  private
    function GetActiveProperties: TcxButtonEditProperties;
    function GetDataBinding: TcxDBTextEditDataBinding;
    function GetProperties: TcxButtonEditProperties;
    procedure SetDataBinding(Value: TcxDBTextEditDataBinding);
    procedure SetProperties(Value: TcxButtonEditProperties);
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
  protected
    class function GetDataBindingClass: TcxEditDataBindingClass; override;
  public
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    property ActiveProperties: TcxButtonEditProperties read GetActiveProperties;
  published
    property Anchors;
    property AutoSize;
    property BeepOnEnter;
    property BiDiMode;
    property Constraints;
    property DataBinding: TcxDBTextEditDataBinding read GetDataBinding write SetDataBinding;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Properties: TcxButtonEditProperties read GetProperties write SetProperties;
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
    property OnStartDrag;
    property OnEndDock;
    property OnStartDock;
  end;

  { TcxDBCheckBox }

  TcxDBCheckBox = class(TcxCustomCheckBox)
  private
    function GetActiveProperties: TcxCheckBoxProperties;
    function GetDataBinding: TcxDBEditDataBinding;
    function GetProperties: TcxCheckBoxProperties;
    procedure SetDataBinding(Value: TcxDBEditDataBinding);
    procedure SetProperties(Value: TcxCheckBoxProperties);
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
  protected
    class function GetDataBindingClass: TcxEditDataBindingClass; override;
  public
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    property ActiveProperties: TcxCheckBoxProperties read GetActiveProperties;
    property Checked;
  published
    property Action;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property Caption;
    property Constraints;
    property DataBinding: TcxDBEditDataBinding read GetDataBinding write SetDataBinding;
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
    property Properties: TcxCheckBoxProperties read GetProperties write SetProperties;
    property ShowHint;
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
    property OnStartDrag;
    property OnEndDock;
    property OnStartDock;
  end;

  { TcxDBComboBox }

  TcxDBComboBox = class(TcxCustomComboBox)
  private
    function GetActiveProperties: TcxComboBoxProperties;
    function GetDataBinding: TcxDBTextEditDataBinding;
    function GetProperties: TcxComboBoxProperties;
    procedure SetDataBinding(Value: TcxDBTextEditDataBinding);
    procedure SetProperties(Value: TcxComboBoxProperties);
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
  protected
    class function GetDataBindingClass: TcxEditDataBindingClass; override;
    function SupportsSpelling: Boolean; override;
  public
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    property ActiveProperties: TcxComboBoxProperties read GetActiveProperties;
    property ItemIndex;
  published
    property Anchors;
    property AutoSize;
    property BeepOnEnter;
    property BiDiMode;
    property Constraints;
    property DataBinding: TcxDBTextEditDataBinding read GetDataBinding write SetDataBinding;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Properties: TcxComboBoxProperties read GetProperties write SetProperties;
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
    property OnStartDrag;
    property OnEndDock;
    property OnStartDock;
  end;

  { TcxDBPopupEdit }

  TcxDBPopupEdit = class(TcxCustomPopupEdit)
  private
    function GetActiveProperties: TcxPopupEditProperties;
    function GetDataBinding: TcxDBTextEditDataBinding;
    function GetProperties: TcxPopupEditProperties;
    procedure SetDataBinding(Value: TcxDBTextEditDataBinding);
    procedure SetProperties(Value: TcxPopupEditProperties);
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
  protected
    class function GetDataBindingClass: TcxEditDataBindingClass; override;
    function SupportsSpelling: Boolean; override;
  public
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    property ActiveProperties: TcxPopupEditProperties read GetActiveProperties;
  published
    property Anchors;
    property AutoSize;
    property BeepOnEnter;
    property Constraints;
    property DataBinding: TcxDBTextEditDataBinding read GetDataBinding write SetDataBinding;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Properties: TcxPopupEditProperties read GetProperties write SetProperties;
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
    property OnStartDrag;
    property OnEndDock;
    property OnStartDock;
  end;

  { TcxDBSpinEdit }

  TcxDBSpinEdit = class(TcxCustomSpinEdit)
  private
    function GetActiveProperties: TcxSpinEditProperties;
    function GetDataBinding: TcxDBTextEditDataBinding;
    function GetProperties: TcxSpinEditProperties;
    procedure SetDataBinding(Value: TcxDBTextEditDataBinding);
    procedure SetProperties(Value: TcxSpinEditProperties);
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
  protected
    class function GetDataBindingClass: TcxEditDataBindingClass; override;
  public
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    property ActiveProperties: TcxSpinEditProperties read GetActiveProperties;
    property Value;
  published
    property Anchors;
    property AutoSize;
    property BeepOnEnter;
    property BiDiMode;
    property Constraints;
    property DataBinding: TcxDBTextEditDataBinding read GetDataBinding write SetDataBinding;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Properties: TcxSpinEditProperties read GetProperties write SetProperties;
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

  { TcxDBTimeEdit }

  TcxDBTimeEdit = class(TcxCustomTimeEdit)
  private
    function GetActiveProperties: TcxTimeEditProperties;
    function GetDataBinding: TcxDBTextEditDataBinding;
    function GetProperties: TcxTimeEditProperties;
    procedure SetDataBinding(Value: TcxDBTextEditDataBinding);
    procedure SetProperties(Value: TcxTimeEditProperties);
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
  protected
    class function GetDataBindingClass: TcxEditDataBindingClass; override;
  public
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    property ActiveProperties: TcxTimeEditProperties read GetActiveProperties;
  published
    property Anchors;
    property AutoSize;
    property BeepOnEnter;
    property Constraints;
    property DataBinding: TcxDBTextEditDataBinding read GetDataBinding write SetDataBinding;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Properties: TcxTimeEditProperties read GetProperties write SetProperties;
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

  { TcxDBBlobEdit }

  TcxDBBlobEdit = class(TcxCustomBlobEdit)
  private
    function GetActiveProperties: TcxBlobEditProperties;
    function GetDataBinding: TcxDBEditDataBinding;
    function GetProperties: TcxBlobEditProperties;
    procedure SetDataBinding(Value: TcxDBEditDataBinding);
    procedure SetProperties(Value: TcxBlobEditProperties);
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
  protected
    class function GetDataBindingClass: TcxEditDataBindingClass; override;
  public
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    property ActiveProperties: TcxBlobEditProperties read GetActiveProperties;
  published
    property Anchors;
    property AutoSize;
    property BeepOnEnter;
    property BiDiMode;
    property Constraints;
    property DataBinding: TcxDBEditDataBinding read GetDataBinding write SetDataBinding;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Properties: TcxBlobEditProperties read GetProperties write SetProperties;
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
    property OnGetGraphicClass;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

  { TcxDBCalcEdit }

  TcxDBCalcEdit = class(TcxCustomCalcEdit)
  private
    function GetActiveProperties: TcxCalcEditProperties;
    function GetDataBinding: TcxDBTextEditDataBinding;
    function GetProperties: TcxCalcEditProperties;
    procedure SetDataBinding(Value: TcxDBTextEditDataBinding);
    procedure SetProperties(Value: TcxCalcEditProperties);
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
  protected
    class function GetDataBindingClass: TcxEditDataBindingClass; override;
  public
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    property ActiveProperties: TcxCalcEditProperties read GetActiveProperties;
    property Value;
  published
    property Anchors;
    property AutoSize;
    property BeepOnEnter;
    property Constraints;
    property DataBinding: TcxDBTextEditDataBinding read GetDataBinding write SetDataBinding;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Properties: TcxCalcEditProperties read GetProperties write SetProperties;
    property ShowHint;
    property Style;
    property StyleDisabled;
    property StyleFocused;
    property StyleHot;
    property TabOrder;
    property TabStop default True;
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
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

  { TcxDBDateEdit }

  TcxDBDateEdit = class(TcxCustomDateEdit)
  private
    function GetActiveProperties: TcxDateEditProperties;
    function GetDataBinding: TcxDBTextEditDataBinding;
    function GetProperties: TcxDateEditProperties;
    procedure SetDataBinding(Value: TcxDBTextEditDataBinding);
    procedure SetProperties(Value: TcxDateEditProperties);
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
  protected
    class function GetDataBindingClass: TcxEditDataBindingClass; override;
  public
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    property ActiveProperties: TcxDateEditProperties read GetActiveProperties;
  published
    property Anchors;
    property AutoSize;
    property BeepOnEnter;
    property Constraints;
    property DataBinding: TcxDBTextEditDataBinding read GetDataBinding write SetDataBinding;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Properties: TcxDateEditProperties read GetProperties write SetProperties;
    property ShowHint;
    property Style;
    property StyleDisabled;
    property StyleFocused;
    property StyleHot;
    property TabOrder;
    property TabStop default True;
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
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

  { TcxDBCurrencyEdit }

  TcxDBCurrencyEdit = class(TcxCustomCurrencyEdit)
  private
    function GetActiveProperties: TcxCurrencyEditProperties;
    function GetDataBinding: TcxDBTextEditDataBinding;
    function GetProperties: TcxCurrencyEditProperties;
    procedure SetDataBinding(Value: TcxDBTextEditDataBinding);
    procedure SetProperties(Value: TcxCurrencyEditProperties);
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
  protected
    class function GetDataBindingClass: TcxEditDataBindingClass; override;
  public
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    property ActiveProperties: TcxCurrencyEditProperties read GetActiveProperties;
  published
    property Anchors;
    property AutoSize;
    property BeepOnEnter;
    property Constraints;
    property DataBinding: TcxDBTextEditDataBinding read GetDataBinding write SetDataBinding;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Properties: TcxCurrencyEditProperties read GetProperties write SetProperties;
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
    property OnEditing;
    property OnEndDock;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
  end;

  { TcxDBHyperLinkEdit }

  TcxDBHyperLinkEdit = class(TcxCustomHyperLinkEdit)
  private
    function GetActiveProperties: TcxHyperLinkEditProperties;
    function GetDataBinding: TcxDBTextEditDataBinding;
    function GetProperties: TcxHyperLinkEditProperties;
    procedure SetDataBinding(Value: TcxDBTextEditDataBinding);
    procedure SetProperties(Value: TcxHyperLinkEditProperties);
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
  protected
    class function GetDataBindingClass: TcxEditDataBindingClass; override;
  public
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    property ActiveProperties: TcxHyperLinkEditProperties read GetActiveProperties;
  published
    property Anchors;
    property AutoSize;
    property BeepOnEnter;
    property Constraints;
    property DataBinding: TcxDBTextEditDataBinding read GetDataBinding write SetDataBinding;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Properties: TcxHyperLinkEditProperties read GetProperties write SetProperties;
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
    property OnEditing;
    property OnEndDock;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
  end;

  { TcxDBImage }

  TcxDBImage = class(TcxCustomImage)
  private
    function GetActiveProperties: TcxImageProperties;
    function GetDataBinding: TcxDBEditDataBinding;
    function GetProperties: TcxImageProperties;
    procedure SetDataBinding(Value: TcxDBEditDataBinding);
    procedure SetProperties(Value: TcxImageProperties);
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
  protected
    class function GetDataBindingClass: TcxEditDataBindingClass; override;
  public
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    property ActiveProperties: TcxImageProperties read GetActiveProperties;
  published
    property Anchors;
    property AnimationOptions;
    property AutoSize;
    property BiDiMode;
    property Constraints;
    property DataBinding: TcxDBEditDataBinding read GetDataBinding write SetDataBinding;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentBiDiMode;
    property ParentColor;
    property PopupMenu;
    property Properties: TcxImageProperties read GetProperties write SetProperties;
    property ShowHint;
    property Style;
    property StyleDisabled;
    property StyleFocused;
    property StyleHot;
    property TabOrder;
    property TabStop;
    property Transparent;
    property Visible;
    property ZoomingOptions;
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
    property OnGetGraphicClass;
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

  { TcxDBImageComboBox }

  TcxDBImageComboBox = class(TcxCustomImageComboBox)
  private
    function GetActiveProperties: TcxImageComboBoxProperties;
    function GetDataBinding: TcxDBTextEditDataBinding;
    function GetProperties: TcxImageComboBoxProperties;
    procedure SetDataBinding(Value: TcxDBTextEditDataBinding);
    procedure SetProperties(Value: TcxImageComboBoxProperties);
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
  protected
    class function GetDataBindingClass: TcxEditDataBindingClass; override;
  public
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    property ActiveProperties: TcxImageComboBoxProperties read GetActiveProperties;
    property ItemIndex;
  published
    property Anchors;
    property AutoSize;
    property BeepOnEnter;
    property BiDiMode;
    property Constraints;
    property DataBinding: TcxDBTextEditDataBinding read GetDataBinding write SetDataBinding;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Properties: TcxImageComboBoxProperties read GetProperties write SetProperties;
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
    property OnEditing;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

  { TcxDBMRUEdit }

  TcxDBMRUEdit = class(TcxCustomMRUEdit)
  private
    function GetActiveProperties: TcxMRUEditProperties;
    function GetDataBinding: TcxDBTextEditDataBinding;
    function GetProperties: TcxMRUEditProperties;
    procedure SetDataBinding(Value: TcxDBTextEditDataBinding);
    procedure SetProperties(Value: TcxMRUEditProperties);
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
  public
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    property ActiveProperties: TcxMRUEditProperties read GetActiveProperties;
  protected
    class function GetDataBindingClass: TcxEditDataBindingClass; override;
  published
    property Anchors;
    property AutoSize;
    property BeepOnEnter;
    property Constraints;
    property DataBinding: TcxDBTextEditDataBinding read GetDataBinding write SetDataBinding;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Properties: TcxMRUEditProperties read GetProperties write SetProperties;
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
    property OnEditing;
    property OnEndDock;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
  end;

  { TcxDBRadioGroupButton }

  TcxDBRadioGroupButton = class(TcxCustomRadioGroupButton)
  private
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TcxDBRadioGroup }

  TcxDBRadioGroup = class(TcxCustomRadioGroup)
  private
    function GetActiveProperties: TcxRadioGroupProperties;
    function GetDataBinding: TcxDBEditDataBinding;
    function GetProperties: TcxRadioGroupProperties;
    procedure SetDataBinding(Value: TcxDBEditDataBinding);
    procedure SetProperties(Value: TcxRadioGroupProperties);
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
  public
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    property ActiveProperties: TcxRadioGroupProperties read GetActiveProperties;
  protected
    function GetButtonInstance: TWinControl; override;
    class function GetDataBindingClass: TcxEditDataBindingClass; override;
  published
    property Alignment;
    property Anchors;
    property BiDiMode;
    property Caption;
    property Constraints;
    property Ctl3D;
    property DataBinding: TcxDBEditDataBinding read GetDataBinding write SetDataBinding;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentBackground;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Properties: TcxRadioGroupProperties read GetProperties write SetProperties;
    property ShowHint;
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
    property OnStartDock;
    property OnStartDrag;
  end;

  { TcxDBListBox }

  TcxDBListBox = class(TcxListBox)
  private
    function GetDataBinding: TcxDBDataBinding;
    procedure SetDataBinding(Value: TcxDBDataBinding);
  protected
    function GetDataBindingClass: TcxCustomDataBindingClass; override;
  published
    property DataBinding: TcxDBDataBinding read GetDataBinding write SetDataBinding;
  end;

function GetcxDBEditDataLink(AEdit: TcxCustomEdit): TDataLink;

implementation

uses
  cxVariants, StdCtrls;

type
  TcxCustomEditAccess = class(TcxCustomEdit);
  TcxDataSetAccess = class(TDataSet);

function GetcxDBEditDataLink(AEdit: TcxCustomEdit): TDataLink;
begin
  if TcxCustomEditAccess(AEdit).DataBinding is TcxDBEditDataBinding then
    Result := TcxDBEditDataBinding(TcxCustomEditAccess(AEdit).DataBinding).DataLink
  else
    Result := nil;
end;

{ TcxCustomDBEditDefaultValuesProvider }

constructor TcxCustomDBEditDefaultValuesProvider.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FFreeNotifier := TcxFreeNotificator.Create(nil);
  FFreeNotifier.OnFreeNotification := FieldFreeNotification;
end;

destructor TcxCustomDBEditDefaultValuesProvider.Destroy;
begin
  FreeAndNil(FFreeNotifier);
  inherited Destroy;
end;

function TcxCustomDBEditDefaultValuesProvider.CanSetEditMode: Boolean;
begin
  Result := (DataSource <> nil) and (DataSource.AutoEdit or
    (DataSource.State in [dsEdit, dsInsert]));
end;

function TcxCustomDBEditDefaultValuesProvider.DefaultAlignment: TAlignment;
begin
  if IsDefaultDataAvailable then
    Result := Field.Alignment
  else
    Result := inherited DefaultAlignment;
end;

function TcxCustomDBEditDefaultValuesProvider.DefaultBlobKind: TcxBlobKind;
begin
  if IsDefaultDataAvailable then
    case Field.DataType of
      ftWideMemo, ftMemo, ftFmtMemo, ftOraClob:
        Result := bkMemo;
      ftGraphic:
        Result := bkGraphic;
      ftParadoxOle, ftDBaseOle:
        Result := bkOle;
      else
        Result := bkNone;
    end
  else
    Result := bkNone;
end;

function TcxCustomDBEditDefaultValuesProvider.DefaultCanModify: Boolean;
begin
  Result := not DefaultReadOnly and IsDataAvailable;
  Result := Result and (Field.CanModify or (Field.Lookup and CanModifyLookupField(Field)));
end;

function TcxCustomDBEditDefaultValuesProvider.DefaultDisplayFormat: string;
begin
  if IsDefaultDataAvailable and (Field is TNumericField) then
    Result := TNumericField(Field).DisplayFormat
  else
    Result := inherited DefaultDisplayFormat;
end;

function TcxCustomDBEditDefaultValuesProvider.DefaultEditFormat: string;
begin
  if IsDefaultDataAvailable and (Field is TNumericField) then
    Result := TNumericField(Field).EditFormat
  else
    Result := inherited DefaultEditFormat;
end;

function TcxCustomDBEditDefaultValuesProvider.DefaultEditMask: string;
begin
  if IsDefaultDataAvailable then
    Result := Field.EditMask
  else
    Result := inherited DefaultEditMask;
end;

function TcxCustomDBEditDefaultValuesProvider.DefaultIsFloatValue: Boolean;
begin
  if IsDefaultDataAvailable then
    Result := Field.DataType in [ftFloat, ftCurrency, ftBCD]
  else
    Result := inherited DefaultIsFloatValue;
end;

function TcxCustomDBEditDefaultValuesProvider.DefaultMaxLength: Integer;
begin
  if IsDefaultDataAvailable and (Field.DataType in [ftString, ftWideString]) then
    Result := Field.Size
  else
    Result := inherited DefaultMaxLength;
end;

function TcxCustomDBEditDefaultValuesProvider.DefaultMaxValue: Double;
begin
  Result := inherited DefaultMaxValue;
  if IsDefaultDataAvailable then
  begin
    if Field is TIntegerField then
      Result := TIntegerField(Field).MaxValue
    else
      if Field is TFloatField then
      begin
        Result := StrToFloat(FloatToStrF(TFloatField(Field).MaxValue, ffGeneral,
          TFloatField(Field).Precision, TFloatField(Field).Precision));
      end
    else
      if Field is TBCDField then
        Result := TBCDField(Field).MaxValue;
  end;
end;

function TcxCustomDBEditDefaultValuesProvider.DefaultMinValue: Double;
begin
  Result := inherited DefaultMinValue;
  if IsDefaultDataAvailable then
  begin
    if Field is TIntegerField then
      Result := TIntegerField(Field).MinValue
    else
      if Field is TFloatField then
      begin
        Result := StrToFloat(FloatToStrF(TFloatField(Field).MinValue, ffGeneral,
          TFloatField(Field).Precision, TFloatField(Field).Precision));
      end
    else
      if Field is TBCDField then
        Result := TBCDField(Field).MinValue;
  end;
end;

function TcxCustomDBEditDefaultValuesProvider.DefaultPrecision: Integer;
begin
  if Field is TFloatField then
    Result := TFloatField(Field).Precision
  else if Field is TBCDField then
    Result := TBCDField(Field).Precision
  else if Field is TAggregateField then
    Result := TAggregateField(Field).Precision
  else if Field is TFMTBCDField then
    Result := TFMTBCDField(Field).Precision
  else
    Result := inherited DefaultPrecision;
end;

function TcxCustomDBEditDefaultValuesProvider.DefaultReadOnly: Boolean;
begin
  if IsDefaultDataAvailable then
    Result := Field.ReadOnly
  else
    Result := inherited DefaultReadOnly;
end;

function TcxCustomDBEditDefaultValuesProvider.DefaultRequired: Boolean;
begin
  if IsDefaultDataAvailable then
    Result := Field.Required
  else
    Result := inherited DefaultRequired;
end;

function TcxCustomDBEditDefaultValuesProvider.IsBlob: Boolean;
begin
  Result := IsDefaultDataAvailable and Field.IsBlob;
end;

function TcxCustomDBEditDefaultValuesProvider.IsCurrency: Boolean;
begin
  Result := inherited IsCurrency;
  if IsDefaultDataAvailable then
    if Field is TFloatField then
      Result := TFloatField(Field).Currency
    else if Field is TBCDField then
      Result := TBCDField(Field).Currency
    else if Field is TAggregateField then
      Result := TAggregateField(Field).Currency
    else if Field is TFMTBCDField then
      Result := TFMTBCDField(Field).Currency
end;

function TcxCustomDBEditDefaultValuesProvider.IsDataAvailable: Boolean;
begin
  Result := cxDB.IsDataAvailable(FField);
end;

function TcxCustomDBEditDefaultValuesProvider.IsDataStorage: Boolean;
begin
  Result := True;
end;

function TcxCustomDBEditDefaultValuesProvider.IsDisplayFormatDefined(AIsCurrencyValueAccepted: Boolean): Boolean;
begin
  Result := IsDefaultDataAvailable;
  if Result and not Assigned(Field.OnGetText) then
  begin
    Result := False;
    if Field is TFloatField then
      Result := Result or TFloatField(Field).Currency and AIsCurrencyValueAccepted;
    if Field is TBCDField then
      Result := Result or TBCDField(Field).Currency and AIsCurrencyValueAccepted;
    if Field is TDateTimeField then
      Result := Result or (TDateTimeField(Field).DisplayFormat <> '');
    if Field is TAggregateField then
      with TAggregateField(Field) do
        Result := Result or (DisplayFormat <> '') or Currency and AIsCurrencyValueAccepted;
    if Field is TFMTBCDField then
      Result := Result or TFMTBCDField(Field).Currency and AIsCurrencyValueAccepted;
    if Field is TSQLTimeStampField then
      Result := Result or (TSQLTimeStampField(Field).DisplayFormat <> '');
    if Field is TNumericField then
      Result := Result or (TNumericField(Field).DisplayFormat <> '');
  end;
end;

function TcxCustomDBEditDefaultValuesProvider.IsOnGetTextAssigned: Boolean;
begin
  Result := IsDisplayFormatDefined(False) and (DefaultDisplayFormat = '');
end;

function TcxCustomDBEditDefaultValuesProvider.IsOnSetTextAssigned: Boolean;
begin
  Result := IsDefaultDataAvailable and Assigned(Field.OnSetText);
end;

function TcxCustomDBEditDefaultValuesProvider.IsValidChar(AChar: Char): Boolean;
begin
  if IsDefaultDataAvailable then
    Result := Field.IsValidChar(AChar)
  else
    Result := inherited IsValidChar(AChar);
end;

procedure TcxCustomDBEditDefaultValuesProvider.FieldFreeNotification(Sender: TComponent);
begin
  if Sender = DataSource then
    DataSource := nil
  else
    Field := nil;
end;

procedure TcxCustomDBEditDefaultValuesProvider.SetDataSource(Value: TDataSource);
begin
  if Value <> FDataSource then
  begin
    if FDataSource <> nil then
      FFreeNotifier.RemoveSender(FDataSource);
    FDataSource := Value;
    if FDataSource <> nil then
      FFreeNotifier.AddSender(FDataSource);
  end;
end;

procedure TcxCustomDBEditDefaultValuesProvider.SetField(Value: TField);
begin
  if Value <> FField then
  begin
    if FField <> nil then
      FFreeNotifier.RemoveSender(FField);
    FField := Value;
    if FField <> nil then
      FFreeNotifier.AddSender(FField);
  end;
end;

function TcxCustomDBEditDefaultValuesProvider.IsDefaultDataAvailable: Boolean;
begin
  Result := FField <> nil;
end;

{ TcxDBFieldDataLink }

procedure TcxDBFieldDataLink.FocusControl(Field: TFieldRef);
var
  AVisualControl: TWinControl;
begin
  if not(DataBinding.VisualControl is TWinControl) then
    Exit;
  AVisualControl := TWinControl(DataBinding.VisualControl);
  if (Field^ <> nil) and (Field^ = Self.Field) and AVisualControl.CanFocus then
  begin
    Field^ := nil;
    AVisualControl.SetFocus;
  end;
end;

procedure TcxDBFieldDataLink.VisualControlChanged;
begin
  VisualControl := DataBinding.VisualControl is TWinControl;
end;

procedure TcxDBFieldDataLink.UpdateRightToLeft;
var
  AIsRightAligned: Boolean;
  AUseRightToLeftAlignment: Boolean;
  AVisualControl: TWinControl;
begin
  if DataBinding.VisualControl is TWinControl then
  begin
    AVisualControl := TWinControl(DataBinding.VisualControl);
    if AVisualControl.IsRightToLeft then
    begin
      AIsRightAligned :=
        (GetWindowLong(AVisualControl.Handle, GWL_EXSTYLE) and WS_EX_RIGHT) = WS_EX_RIGHT;
      AUseRightToLeftAlignment := DBUseRightToLeftAlignment(AVisualControl, Field);
      if AIsRightAligned and not AUseRightToLeftAlignment or
        not AIsRightAligned and AUseRightToLeftAlignment then
          AVisualControl.Perform(CM_RECREATEWND, 0, 0);
    end;
  end;
end;

{ TcxEditFieldDataLink }

constructor TcxEditFieldDataLink.Create(ADataBinding: TcxDBEditDataBinding);
begin
  inherited Create;
  VisualControl := True;
  FDataBinding := ADataBinding;
  FFreeNotifier := TcxFreeNotificator.Create(nil);
  FFreeNotifier.OnFreeNotification := FieldFreeNotification;
end;

destructor TcxEditFieldDataLink.Destroy;
begin
  InternalSetField(nil, nil);
  FreeAndNil(FFreeNotifier);
  inherited Destroy;
end;

function TcxEditFieldDataLink.Edit: Boolean;
begin
  if CanModify then
    inherited Edit;
  Result := FEditing;
end;

procedure TcxEditFieldDataLink.Modified;
begin
  FModified := True;
  FDataBinding.Edit.ModifiedAfterEnter := True;
end;

procedure TcxEditFieldDataLink.Reset;
begin
  RecordChanged(nil);
end;

procedure TcxEditFieldDataLink.DataEvent(Event: TDataEvent; Info: TdxNativeInt);
begin
  FDataBinding.DataEvent(Event, Info);
  inherited DataEvent(Event, Info);

  case Event of
    deDataSetChange:
    begin
      FDataBinding.DisableRefresh;
      try
        FDataBinding.DataSetChange;
      finally
        FDataBinding.EnableRefresh;
        ResetModified;
      end;
    end;
//    deDisabledStateChange:
//      UpdateField;
  end;
end;

procedure TcxEditFieldDataLink.ActiveChanged;
begin
  FActiveChanging := True;
  FDataBinding.DefaultValuesProvider.DataSource := DataSource;
  FDataBinding.DisableRefresh;
  try
    UpdateField;
  finally
    FActiveChanging := False;
    FDataBinding.EnableRefresh;
    ResetModified;
    FDataBinding.DataChanged;
  end;
end;

procedure TcxEditFieldDataLink.FocusControl(Field: TFieldRef);
begin
  if (Field^ <> nil) and (Field^ = DataField) and (FControl is TWinControl) then
    if TWinControl(FControl).CanFocus then
    begin
      Field^ := nil;
      TWinControl(FControl).SetFocus;
    end;
end;

procedure TcxEditFieldDataLink.UpdateData;
begin
  if FModified then
  begin
    if DataField <> nil then
      FDataBinding.UpdateData;
    if not FDataBinding.IsRefreshDisabled then
      ResetModified;
  end;
end;

procedure TcxEditFieldDataLink.EditingChanged;
begin
  SetEditing(inherited Editing and CanModify);
  FDataBinding.EditingChanged;
end;

procedure TcxEditFieldDataLink.LayoutChanged;
begin
  UpdateField;
end;

procedure TcxEditFieldDataLink.RecordChanged(AField: TField);
begin
  if (AField = nil) or (AField = FMasterField) then
  begin
    FDataBinding.DataChanged;
    if not FDataBinding.IsRefreshDisabled then
      ResetModified;
  end;
end;

procedure TcxEditFieldDataLink.ResetModified;
begin
  FModified := False;
  FDataBinding.Edit.ModifiedAfterEnter := False;
end;

procedure TcxEditFieldDataLink.FieldFreeNotification(Sender: TComponent);
begin
  InternalSetField(nil, nil);
end;

function TcxEditFieldDataLink.GetCanModify: Boolean;
begin
  Result := not ReadOnly and (DataField <> nil) and
    (DataField.CanModify or (DataField.Lookup and CanModifyLookupField(DataField)));
end;

procedure TcxEditFieldDataLink.InternalSetField(ADataField, APropertiesField: TField);
begin
  if (FPropertiesField <> APropertiesField) or (FDataField <> ADataField) then
  begin
    FFreeNotifier.RemoveSender(FPropertiesField);
    FFreeNotifier.RemoveSender(FDataField);
    FPropertiesField := APropertiesField;
    FDataField := ADataField;
    FFreeNotifier.AddSender(FPropertiesField);
    FFreeNotifier.AddSender(FDataField);
    UpdateMasterField;
  end;
end;

procedure TcxEditFieldDataLink.SetEditing(Value: Boolean);
begin
  if FEditing <> Value then
  begin
    FEditing := Value;
    if not FDataBinding.IsRefreshDisabled then
      ResetModified;
  end;
end;

procedure TcxEditFieldDataLink.SetField(ADataField, APropertiesField: TField);
begin
  if FDataField <> ADataField then
  begin
    InternalSetField(ADataField, APropertiesField);
    if FActiveChanging then
      FDataBinding.DataSetChange
    else
      FDataBinding.DefaultValuesChanged;
    EditingChanged;
    RecordChanged(nil);
    UpdateRightToLeft;
  end
  else
    if FActiveChanging then
      FDataBinding.DataSetChange;
end;

procedure TcxEditFieldDataLink.SetFieldName(const Value: string);
begin
  if FFieldName <> Value then
  begin
    FFieldName :=  Value;
    UpdateField;
  end;
end;

procedure TcxEditFieldDataLink.UpdateField;

  function GetField(AConsiderActive: Boolean): TField;
  begin
    if (Active or not AConsiderActive) and
      (DataSource <> nil) and (DataSource.DataSet <> nil) and not (csDestroying in DataSource.DataSet.ComponentState) and
      (FFieldName <> '') then
        Result := DataSource.DataSet.FindField(FFieldName)
    else
      Result := nil;
  end;

begin
  InternalSetField(nil, GetField(False));
  SetField(GetField(True), GetField(False));
end;

procedure TcxEditFieldDataLink.UpdateMasterField;
begin
  if DataField = nil then
    FMasterField := nil
  else
    if not FDataBinding.IsLookupControl or (DataField.FieldKind <> fkLookup) or
      (Pos(';', DataField.KeyFields) > 0) then
        FMasterField := DataField
    else
      if Assigned(FControl) then
        FMasterField := GetFieldProperty(DataSource.DataSet, FControl, DataField.KeyFields)
      else
        FMasterField := DataSource.DataSet.FieldByName(DataField.KeyFields);
end;

procedure TcxEditFieldDataLink.UpdateRightToLeft;
var
  AIsRightAligned: Boolean;
  AUseRightToLeftAlignment: Boolean;
begin
  if Assigned(FControl) and (FControl is TWinControl) and
    TWinControl(FControl).IsRightToLeft then
  begin
    AIsRightAligned :=
      (GetWindowLong(TWinControl(FControl).Handle, GWL_EXSTYLE) and WS_EX_RIGHT) = WS_EX_RIGHT;
    AUseRightToLeftAlignment :=
      DBUseRightToLeftAlignment(TControl(FControl), PropertiesField);
    if (AIsRightAligned and (not AUseRightToLeftAlignment)) or
       ((not AIsRightAligned) and AUseRightToLeftAlignment) then
      TWinControl(FControl).Perform(CM_RECREATEWND, 0, 0);
  end;
end;

{ TcxDBEditDataBinding }

constructor TcxDBEditDataBinding.Create(AEdit: TcxCustomEdit);
begin
  inherited Create(AEdit);
  FDataLink := TcxEditFieldDataLink.Create(Self);
  if AEdit.InnerControl <> nil then
    FDataLink.Control := AEdit.InnerControl
  else
    FDataLink.Control := AEdit;
end;

destructor TcxDBEditDataBinding.Destroy;
begin
  FreeAndNil(FDataLink);
  inherited Destroy;
end;

procedure TcxDBEditDataBinding.Assign(Source: TPersistent);
begin
  if Source is TcxDBTextEditDataBinding then
  begin
    DataField := TcxDBTextEditDataBinding(Source).DataField;
    DataSource := TcxDBTextEditDataBinding(Source).DataSource;
    DataChanged;
  end;
  inherited Assign(Source);
end;

function TcxDBEditDataBinding.CanCheckEditorValue: Boolean;
begin
  Result := False;
end;

function TcxDBEditDataBinding.CanModify: Boolean;
begin
  Result := inherited CanModify and DataLink.CanModify;
end;

function TcxDBEditDataBinding.CanPostEditorValue: Boolean;
begin
  Result := Editing and DataLink.FModified;
end;

function TcxDBEditDataBinding.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := FDataLink.ExecuteAction(Action);
end;

class function TcxDBEditDataBinding.GetDefaultValuesProviderClass: TcxCustomEditDefaultValuesProviderClass;
begin
  Result := TcxCustomDBEditDefaultValuesProvider;
end;

procedure TcxDBEditDataBinding.SetModified;
begin
  if Editing then
    DataLink.Modified;
end;

function TcxDBEditDataBinding.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := FDataLink.UpdateAction(Action);
end;

procedure TcxDBEditDataBinding.UpdateDisplayValue;
begin
  Edit.LockClick(True);
  try
    if IsDataAvailable then
      FDataLink.Reset
    else
      inherited UpdateDisplayValue;
  finally
    Edit.LockClick(False);
  end;
end;

procedure TcxDBEditDataBinding.DefaultValuesChanged;
begin
  DefaultValuesProvider.Field := PropertiesField;
  inherited DefaultValuesChanged;
end;

procedure TcxDBEditDataBinding.DataEvent(AEvent: TDataEvent; AInfo: TdxNativeInt);
begin

  Include(FReceivedEvents, AEvent);
  case AEvent of
    deDataSetChange:
       begin
        if not (deUpdateRecord in FReceivedEvents) then
          TcxCustomEditAccess(Edit).RollbackErrorState;
        FReceivedEvents := [];
      end;
    deFieldChange:
      if not Modified and (AInfo = TdxNativeInt(Field)) then
        TcxCustomEditAccess(Edit).ResetErrorState;
    deDataSetScroll:
      begin
        TcxCustomEditAccess(Edit).ResetErrorState;
        FReceivedEvents := [];
      end;
    deRecordChange:
      if (FReceivedEvents = [deCheckBrowseMode, deUpdateState, deRecordChange]) then
      begin
        if not Modified then
          TcxCustomEditAccess(Edit).PrevEditValue := Edit.EditValue
        else
          TcxCustomEditAccess(Edit).ResetErrorState;
      end;
    deUpdateState:
      if FReceivedEvents = [deUpdateState] then
        TcxCustomEditAccess(Edit).ResetErrorState;
  end;
end;

procedure TcxDBEditDataBinding.DisableRefresh;
begin
  Inc(FRefreshCount);
end;

procedure TcxDBEditDataBinding.EnableRefresh;
begin
  if FRefreshCount > 0 then
    Dec(FRefreshCount);
end;

function TcxDBEditDataBinding.GetEditing: Boolean;
begin
  Result := IsDataAvailable and FDataLink.Editing;
end;

function TcxDBEditDataBinding.GetIsDataAvailable: Boolean;
begin
  Result := inherited GetIsDataAvailable and cxDB.IsDataAvailable(Field) and cxDB.IsDataAvailable(PropertiesField);
end;

function TcxDBEditDataBinding.GetModified: Boolean;
begin
  Result := GetEditing and FDataLink.FModified;
end;

function TcxDBEditDataBinding.GetStoredValue: TcxEditValue;

  function GetNullValue(AEditValueSource: TcxDataEditValueSource): Variant;
  var
    AFields: TcxDBFieldList;
    I: Integer;
  begin
    if not IsDataAvailable or (AEditValueSource <> evsKey) or
        (Pos(';', Field.KeyFields) = 0) then
      Result := Null
    else
    begin
      AFields := TcxDBFieldList.Create;
      try
        Field.DataSet.GetFieldList(AFields, Field.KeyFields);
        Result := VarArrayCreate([0, AFields.Count - 1], varVariant);
        for I := 0 to AFields.Count - 1 do
          Result[I] := Null;
      finally
        AFields.Free;
      end;
    end;
  end;

var
  AEditValueSource: TcxDataEditValueSource;
begin
  AEditValueSource := Edit.ActiveProperties.GetEditValueSource(Edit.InternalFocused);
  if not IsDataAvailable or (IsNull and
    ((AEditValueSource <> evsText) or not Assigned(Field.OnGetText))) then
    Result := GetNullValue(AEditValueSource)
  else
  begin
    case AEditValueSource of
      evsKey:
        if Field.KeyFields <> '' then
          Result := Field.DataSet.FieldValues[Field.KeyFields]
        else
          Result := Field.Value;
      evsText:
        if Edit.Focused and FDataLink.CanModify then
          Result := Field.Text
        else
          if Field.IsBlob then
            Result := Field.AsString
          else
            if not Edit.ActiveProperties.IsValueFormattedByProperties then
              Result := Field.DisplayText
            else
              Result := Field.Value;
      evsValue:
        Result := Field.Value;
    else
      Result := Null;
    end;
  end;
end;

function TcxDBEditDataBinding.IsNull: Boolean;
begin
  if Field is TAggregateField then
    Result := VarIsNull(TcxDataSetAccess(DataLink.DataSet).GetAggregateValue(Field))
  else
    Result := Field.IsNull;
end;

function TcxDBEditDataBinding.IsInnerControlHaveToBeReadOnly: Boolean;
begin
  Result := not Editing;
end;

function TcxDBEditDataBinding.IsRefreshDisabled: Boolean;
begin
  Result := FRefreshCount > 0;
end;

procedure TcxDBEditDataBinding.Reset;
var
  APrevValue: TcxEditValue;
begin
  if IsDataAvailable then
  begin
    APrevValue := TcxCustomEditAccess(Edit).DisplayText;
    FDataLink.Reset;
    if not cxEditVarEquals(APrevValue, Edit.EditValue) then
    begin
      TcxCustomEditAccess(Edit).ResetErrorState;
      Edit.SelectAll;
    end;
  end;
end;

procedure TcxDBEditDataBinding.SetDisplayValue(const Value: TcxEditValue);
begin
  if IsDataAvailable then
    SetInternalDisplayValue(Value)
  else
    SetInternalDisplayValue('');
end;

function TcxDBEditDataBinding.SetEditMode: Boolean;
begin
  Result := IDefaultValuesProvider.DefaultCanModify;
  if not Result then
    Exit;

  DisableRefresh;
  try
    FDatalink.Edit;
    Result := FDatalink.Editing;
    if Result then
      FDatalink.Modified;
  finally
    EnableRefresh;
  end;
end;

procedure TcxDBEditDataBinding.SetStoredValue(const Value: TcxEditValue);

  procedure SetFieldValue(AField: TField; const AValue: TcxEditValue);
  begin
    if VarIsStr(AValue) and (AValue = '') and
      not (Field.DataType in [ftString, ftWideString]) then
        AField.Value := Null
    else
    {$IFNDEF DELPHIXE2}
      if (Field is TFMTBCDField) and VarIsType(AValue, varDouble) then
        AField.Value := FloatToStrF(AValue, ffFixed, 15, 18) // bug in VarToBcd when the type AValue is double: VarToStr(0.000012) returns 1.2E-05 which is incorrect for StrToBcd
      else
    {$ENDIF}
        AField.Value := AValue;
  end;

var
  AEditValueSource: TcxDataEditValueSource;
  AFieldList: TcxDBFieldList;
  I: Integer;
begin
  if IsDataAvailable then
  begin
    DisableRefresh;
    try
      if FDataLink.Edit then
      begin
        AEditValueSource := Edit.ActiveProperties.GetEditValueSource(True(*Edit.InternalFocused*));
        if (AEditValueSource = evsText) and not VarIsNull(Value) then
          Field.Text := VarToStr(Value)
        else
          if (AEditValueSource = evsKey) and (Field.KeyFields <> '') then
            if Pos(';', Field.KeyFields) = 0 then
              SetFieldValue(Field.DataSet.FieldByName(Field.KeyFields), Value)
            else
            begin
              AFieldList := TcxDBFieldList.Create;
              try
                Field.DataSet.GetFieldList(AFieldList, Field.KeyFields);
                for I := 0 to AFieldList.Count - 1 do
                  SetFieldValue(TField(AFieldList[I]), Value[I]);
              finally
                AFieldList.Free;
              end;
              Field.DataSet.FieldValues[Field.KeyFields] := Value;
            end
          else
            SetFieldValue(Field, Value);
        FDataLink.ResetModified;
      end;
    finally
      EnableRefresh;
    end;
  end;
end;

procedure TcxDBEditDataBinding.UpdateNotConnectedDBEditDisplayValue;
begin
  if not IsDataAvailable then
    Edit.EditValue := Null;
end;

procedure TcxDBEditDataBinding.DataChanged;
begin
  if IsRefreshDisabled or Edit.IsDestroying then
    Exit;
  if Edit.IsDesigning and not IsDataAvailable then
    UpdateNotConnectedDBEditDisplayValue
  else
  begin
    if not TcxCustomEditAccess(Edit).Focused and
      Edit.ActiveProperties.IsValueFormattedByProvider then
    begin
      if not IsDataAvailable or IsNull then
        TcxCustomEditAccess(Edit).InternalStoreEditValue(Null)
      else
        TcxCustomEditAccess(Edit).InternalStoreEditValue(Field.Value);
      Edit.LockClick(True);
      try
        SetInternalDisplayValue(StoredValue);
      finally
        Edit.LockClick(False);
      end;
    end
    else
      Edit.EditValue := StoredValue;
  end;
end;

procedure TcxDBEditDataBinding.DataSetChange;
begin
  DefaultValuesChanged;
end;

procedure TcxDBEditDataBinding.EditingChanged;
begin
  TcxCustomEditAccess(Edit).EditingChanged;
end;

function TcxDBEditDataBinding.IsLookupControl: Boolean;
begin
  Result := False;
end;

procedure TcxDBEditDataBinding.UpdateData;
begin
  if IsDataAvailable then
  begin
    TcxCustomEditAccess(Edit).InternalValidateEdit;
    StoredValue := Edit.EditValue;
  end;
end;

function TcxDBEditDataBinding.GetDataFieldName: string;
begin
  Result := FDataLink.FieldName;
end;

function TcxDBEditDataBinding.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

function TcxDBEditDataBinding.GetDefaultValuesProvider: TcxCustomDBEditDefaultValuesProvider;
begin
  Result := TcxCustomDBEditDefaultValuesProvider(IDefaultValuesProvider.GetInstance);
end;

function TcxDBEditDataBinding.GetDataField: TField;
begin
  Result := FDataLink.DataField;
end;

function TcxDBEditDataBinding.GetPropertiesField: TField;
begin
  Result := FDataLink.PropertiesField;
end;

procedure TcxDBEditDataBinding.SetDataFieldName(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

procedure TcxDBEditDataBinding.SetDataSource(Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and Edit.IsLoading) then
  begin
    FDataLink.DataSource := Value;
    DefaultValuesProvider.DataSource := Value;
  end;
end;

{ TcxDBTextEditDataBinding }

procedure TcxDBTextEditDataBinding.UpdateNotConnectedDBEditDisplayValue;
begin
  if not IsDataAvailable then
  begin
    Edit.LockClick(True);
    try
      DisplayValue := '';
    finally
      Edit.LockClick(False);
    end;
  end;
end;

procedure TcxDBTextEditDataBinding.SetDisplayValue(const Value: TcxEditValue);
begin
  if IsDataAvailable then
    SetInternalDisplayValue(Value)
  else
    if Edit.IsDesigning then
      SetInternalDisplayValue(Edit.Name)
    else
      SetInternalDisplayValue('');
end;

{ TcxDBTextEdit }

class function TcxDBTextEdit.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxTextEditProperties;
end;

class function TcxDBTextEdit.GetDataBindingClass: TcxEditDataBindingClass;
begin
  Result := TcxDBTextEditDataBinding;
end;

function TcxDBTextEdit.SupportsSpelling: Boolean;
begin
  Result := IsTextInputMode;
end;

function TcxDBTextEdit.GetActiveProperties: TcxTextEditProperties;
begin
  Result := TcxTextEditProperties(InternalGetActiveProperties);
end;

function TcxDBTextEdit.GetDataBinding: TcxDBTextEditDataBinding;
begin
  Result := FDataBinding as TcxDBTextEditDataBinding;
end;

function TcxDBTextEdit.GetProperties: TcxTextEditProperties;
begin
  Result := TcxTextEditProperties(inherited Properties);
end;

procedure TcxDBTextEdit.SetDataBinding(Value: TcxDBTextEditDataBinding);
begin
  FDataBinding.Assign(Value);
end;

procedure TcxDBTextEdit.SetProperties(Value: TcxTextEditProperties);
begin
  Properties.Assign(Value);
end;

procedure TcxDBTextEdit.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := LRESULT(GetcxDBEditDataLink(Self));
end;

{ TcxDBMemo }

class function TcxDBMemo.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxMemoProperties;
end;

class function TcxDBMemo.GetDataBindingClass: TcxEditDataBindingClass;
begin
  Result := TcxDBTextEditDataBinding;
end;

function TcxDBMemo.GetActiveProperties: TcxMemoProperties;
begin
  Result := TcxMemoProperties(InternalGetActiveProperties);
end;

function TcxDBMemo.GetDataBinding: TcxDBTextEditDataBinding;
begin
  Result := TcxDBTextEditDataBinding(FDataBinding);
end;

function TcxDBMemo.GetProperties: TcxMemoProperties;
begin
  Result := TcxMemoProperties(inherited Properties);
end;

procedure TcxDBMemo.SetDataBinding(Value: TcxDBTextEditDataBinding);
begin
  FDataBinding.Assign(Value);
end;

procedure TcxDBMemo.SetProperties(Value: TcxMemoProperties);
begin
  Properties.Assign(Value);
end;

procedure TcxDBMemo.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := LRESULT(GetcxDBEditDataLink(Self));
end;

{ TcxDBMaskEdit }

class function TcxDBMaskEdit.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxMaskEditProperties;
end;

class function TcxDBMaskEdit.GetDataBindingClass: TcxEditDataBindingClass;
begin
  Result := TcxDBTextEditDataBinding;
end;

function TcxDBMaskEdit.SupportsSpelling: Boolean;
begin
  Result := IsTextInputMode;
end;

function TcxDBMaskEdit.GetActiveProperties: TcxMaskEditProperties;
begin
  Result := TcxMaskEditProperties(InternalGetActiveProperties);
end;

function TcxDBMaskEdit.GetDataBinding: TcxDBTextEditDataBinding;
begin
  Result := FDataBinding as TcxDBTextEditDataBinding;
end;

function TcxDBMaskEdit.GetProperties: TcxMaskEditProperties;
begin
  Result := TcxMaskEditProperties(inherited Properties);
end;

procedure TcxDBMaskEdit.SetDataBinding(Value: TcxDBTextEditDataBinding);
begin
  FDataBinding.Assign(Value);
end;

procedure TcxDBMaskEdit.SetProperties(Value: TcxMaskEditProperties);
begin
  Properties.Assign(Value);
end;

procedure TcxDBMaskEdit.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := LRESULT(GetcxDBEditDataLink(Self));
end;

{ TcxDBButtonEdit }

class function TcxDBButtonEdit.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxButtonEditProperties;
end;

class function TcxDBButtonEdit.GetDataBindingClass: TcxEditDataBindingClass;
begin
  Result := TcxDBTextEditDataBinding;
end;

function TcxDBButtonEdit.GetActiveProperties: TcxButtonEditProperties;
begin
  Result := TcxButtonEditProperties(InternalGetActiveProperties);
end;

function TcxDBButtonEdit.GetDataBinding: TcxDBTextEditDataBinding;
begin
  Result := FDataBinding as TcxDBTextEditDataBinding;
end;

function TcxDBButtonEdit.GetProperties: TcxButtonEditProperties;
begin
  Result := TcxButtonEditProperties(inherited Properties);
end;

procedure TcxDBButtonEdit.SetDataBinding(Value: TcxDBTextEditDataBinding);
begin
  FDataBinding.Assign(Value);
end;

procedure TcxDBButtonEdit.SetProperties(Value: TcxButtonEditProperties);
begin
  Properties.Assign(Value);
end;

procedure TcxDBButtonEdit.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := LRESULT(GetcxDBEditDataLink(Self));
end;

{ TcxDBCheckBox }

class function TcxDBCheckBox.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxCheckBoxProperties;
end;

class function TcxDBCheckBox.GetDataBindingClass: TcxEditDataBindingClass;
begin
  Result := TcxDBEditDataBinding;
end;

function TcxDBCheckBox.GetActiveProperties: TcxCheckBoxProperties;
begin
  Result := TcxCheckBoxProperties(InternalGetActiveProperties);
end;

function TcxDBCheckBox.GetDataBinding: TcxDBEditDataBinding;
begin
  Result := TcxDBEditDataBinding(FDataBinding);
end;

function TcxDBCheckBox.GetProperties: TcxCheckBoxProperties;
begin
  Result := TcxCheckBoxProperties(inherited Properties);
end;

procedure TcxDBCheckBox.SetDataBinding(Value: TcxDBEditDataBinding);
begin
  FDataBinding.Assign(Value);
end;

procedure TcxDBCheckBox.SetProperties(Value: TcxCheckBoxProperties);
begin
  Properties.Assign(Value);
end;

procedure TcxDBCheckBox.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := LRESULT(GetcxDBEditDataLink(Self));
end;

{ TcxDBComboBox }

class function TcxDBComboBox.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxComboBoxProperties;
end;

class function TcxDBComboBox.GetDataBindingClass: TcxEditDataBindingClass;
begin
  Result := TcxDBTextEditDataBinding;
end;

function TcxDBComboBox.SupportsSpelling: Boolean;
begin
  Result := IsTextInputMode;
end;

function TcxDBComboBox.GetActiveProperties: TcxComboBoxProperties;
begin
  Result := TcxComboBoxProperties(InternalGetActiveProperties);
end;

function TcxDBComboBox.GetDataBinding: TcxDBTextEditDataBinding;
begin
  Result := TcxDBTextEditDataBinding(FDataBinding);
end;

function TcxDBComboBox.GetProperties: TcxComboBoxProperties;
begin
  Result := TcxComboBoxProperties(inherited Properties);
end;

procedure TcxDBComboBox.SetDataBinding(Value: TcxDBTextEditDataBinding);
begin
  FDataBinding.Assign(Value);
end;

procedure TcxDBComboBox.SetProperties(Value: TcxComboBoxProperties);
begin
  Properties.Assign(Value);
end;

procedure TcxDBComboBox.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := LRESULT(GetcxDBEditDataLink(Self));
end;

{ TcxDBPopupEdit }

class function TcxDBPopupEdit.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxPopupEditProperties;
end;

class function TcxDBPopupEdit.GetDataBindingClass: TcxEditDataBindingClass;
begin
  Result := TcxDBTextEditDataBinding;
end;

function TcxDBPopupEdit.SupportsSpelling: Boolean;
begin
  Result := IsTextInputMode;
end;

function TcxDBPopupEdit.GetActiveProperties: TcxPopupEditProperties;
begin
  Result := TcxPopupEditProperties(InternalGetActiveProperties);
end;

function TcxDBPopupEdit.GetDataBinding: TcxDBTextEditDataBinding;
begin
  Result := TcxDBTextEditDataBinding(FDataBinding);
end;

function TcxDBPopupEdit.GetProperties: TcxPopupEditProperties;
begin
  Result := TcxPopupEditProperties(inherited Properties);
end;

procedure TcxDBPopupEdit.SetDataBinding(Value: TcxDBTextEditDataBinding);
begin
  FDataBinding.Assign(Value);
end;

procedure TcxDBPopupEdit.SetProperties(Value: TcxPopupEditProperties);
begin
  Properties.Assign(Value);
end;

procedure TcxDBPopupEdit.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := LRESULT(GetcxDBEditDataLink(Self));
end;

{ TcxDBSpinEdit }

class function TcxDBSpinEdit.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxSpinEditProperties;
end;

class function TcxDBSpinEdit.GetDataBindingClass: TcxEditDataBindingClass;
begin
  Result := TcxDBTextEditDataBinding;
end;

function TcxDBSpinEdit.GetActiveProperties: TcxSpinEditProperties;
begin
  Result := TcxSpinEditProperties(InternalGetActiveProperties);
end;

function TcxDBSpinEdit.GetDataBinding: TcxDBTextEditDataBinding;
begin
  Result := TcxDBTextEditDataBinding(FDataBinding);
end;

function TcxDBSpinEdit.GetProperties: TcxSpinEditProperties;
begin
  Result := TcxSpinEditProperties(inherited Properties);
end;

procedure TcxDBSpinEdit.SetDataBinding(Value: TcxDBTextEditDataBinding);
begin
  FDataBinding.Assign(Value);
end;

procedure TcxDBSpinEdit.SetProperties(Value: TcxSpinEditProperties);
begin
  Properties.Assign(Value);
end;

procedure TcxDBSpinEdit.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := LRESULT(GetcxDBEditDataLink(Self));
end;

{ TcxDBTimeEdit }

class function TcxDBTimeEdit.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxTimeEditProperties;
end;

class function TcxDBTimeEdit.GetDataBindingClass: TcxEditDataBindingClass;
begin
  Result := TcxDBTextEditDataBinding;
end;

function TcxDBTimeEdit.GetActiveProperties: TcxTimeEditProperties;
begin
  Result := TcxTimeEditProperties(InternalGetActiveProperties);
end;

function TcxDBTimeEdit.GetDataBinding: TcxDBTextEditDataBinding;
begin
  Result := TcxDBTextEditDataBinding(FDataBinding);
end;

function TcxDBTimeEdit.GetProperties: TcxTimeEditProperties;
begin
  Result := TcxTimeEditProperties(inherited Properties);
end;

procedure TcxDBTimeEdit.SetDataBinding(Value: TcxDBTextEditDataBinding);
begin
  FDataBinding.Assign(Value);
end;

procedure TcxDBTimeEdit.SetProperties(Value: TcxTimeEditProperties);
begin
  Properties.Assign(Value);
end;

procedure TcxDBTimeEdit.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := LRESULT(GetcxDBEditDataLink(Self));
end;

{ TcxDBBlobEdit }

class function TcxDBBlobEdit.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxBlobEditProperties;
end;

class function TcxDBBlobEdit.GetDataBindingClass: TcxEditDataBindingClass;
begin
  Result := TcxDBEditDataBinding;
end;

function TcxDBBlobEdit.GetActiveProperties: TcxBlobEditProperties;
begin
  Result := TcxBlobEditProperties(InternalGetActiveProperties);
end;

function TcxDBBlobEdit.GetDataBinding: TcxDBEditDataBinding;
begin
  Result := TcxDBEditDataBinding(FDataBinding);
end;

function TcxDBBlobEdit.GetProperties: TcxBlobEditProperties;
begin
  Result := TcxBlobEditProperties(inherited Properties);
end;

procedure TcxDBBlobEdit.SetDataBinding(Value: TcxDBEditDataBinding);
begin
  FDataBinding.Assign(Value);
end;

procedure TcxDBBlobEdit.SetProperties(Value: TcxBlobEditProperties);
begin
  Properties.Assign(Value);
end;

procedure TcxDBBlobEdit.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := LRESULT(GetcxDBEditDataLink(Self));
end;

{ TcxDBCalcEdit }

class function TcxDBCalcEdit.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxCalcEditProperties;
end;

class function TcxDBCalcEdit.GetDataBindingClass: TcxEditDataBindingClass;
begin
  Result := TcxDBTextEditDataBinding;
end;

function TcxDBCalcEdit.GetActiveProperties: TcxCalcEditProperties;
begin
  Result := TcxCalcEditProperties(InternalGetActiveProperties);
end;

function TcxDBCalcEdit.GetDataBinding: TcxDBTextEditDataBinding;
begin
  Result := TcxDBTextEditDataBinding(FDataBinding);
end;

function TcxDBCalcEdit.GetProperties: TcxCalcEditProperties;
begin
  Result := TcxCalcEditProperties(inherited Properties);
end;

procedure TcxDBCalcEdit.SetDataBinding(Value: TcxDBTextEditDataBinding);
begin
  FDataBinding.Assign(Value);
end;

procedure TcxDBCalcEdit.SetProperties(Value: TcxCalcEditProperties);
begin
  Properties.Assign(Value);
end;

procedure TcxDBCalcEdit.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := LRESULT(GetcxDBEditDataLink(Self));
end;

{ TcxDBDateEdit }

class function TcxDBDateEdit.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxDateEditProperties;
end;

class function TcxDBDateEdit.GetDataBindingClass: TcxEditDataBindingClass;
begin
  Result := TcxDBTextEditDataBinding;
end;

function TcxDBDateEdit.GetActiveProperties: TcxDateEditProperties;
begin
  Result := TcxDateEditProperties(InternalGetActiveProperties);
end;

function TcxDBDateEdit.GetDataBinding: TcxDBTextEditDataBinding;
begin
  Result := TcxDBTextEditDataBinding(FDataBinding);
end;

function TcxDBDateEdit.GetProperties: TcxDateEditProperties;
begin
  Result := TcxDateEditProperties(inherited Properties);
end;

procedure TcxDBDateEdit.SetDataBinding(Value: TcxDBTextEditDataBinding);
begin
  FDataBinding.Assign(Value);
end;

procedure TcxDBDateEdit.SetProperties(Value: TcxDateEditProperties);
begin
  Properties.Assign(Value);
end;

procedure TcxDBDateEdit.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := LRESULT(GetcxDBEditDataLink(Self));
end;

{ TcxDBCurrencyEdit }

class function TcxDBCurrencyEdit.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxCurrencyEditProperties;
end;

class function TcxDBCurrencyEdit.GetDataBindingClass: TcxEditDataBindingClass;
begin
  Result := TcxDBTextEditDataBinding;
end;

function TcxDBCurrencyEdit.GetActiveProperties: TcxCurrencyEditProperties;
begin
  Result := TcxCurrencyEditProperties(InternalGetActiveProperties);
end;

function TcxDBCurrencyEdit.GetDataBinding: TcxDBTextEditDataBinding;
begin
  Result := TcxDBTextEditDataBinding(FDataBinding);
end;

function TcxDBCurrencyEdit.GetProperties: TcxCurrencyEditProperties;
begin
  Result := TcxCurrencyEditProperties(inherited Properties);
end;

procedure TcxDBCurrencyEdit.SetDataBinding(Value: TcxDBTextEditDataBinding);
begin
  FDataBinding.Assign(Value);
end;

procedure TcxDBCurrencyEdit.SetProperties(Value: TcxCurrencyEditProperties);
begin
  Properties.Assign(Value);
end;

procedure TcxDBCurrencyEdit.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := LRESULT(GetcxDBEditDataLink(Self));
end;

{ TcxDBHyperLinkEdit }

class function TcxDBHyperLinkEdit.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxHyperLinkEditProperties;
end;

class function TcxDBHyperLinkEdit.GetDataBindingClass: TcxEditDataBindingClass;
begin
  Result := TcxDBTextEditDataBinding;
end;

function TcxDBHyperLinkEdit.GetActiveProperties: TcxHyperLinkEditProperties;
begin
  Result := TcxHyperLinkEditProperties(InternalGetActiveProperties);
end;

function TcxDBHyperLinkEdit.GetDataBinding: TcxDBTextEditDataBinding;
begin
  Result := TcxDBTextEditDataBinding(FDataBinding);
end;

function TcxDBHyperLinkEdit.GetProperties: TcxHyperLinkEditProperties;
begin
  Result := TcxHyperLinkEditProperties(inherited Properties);
end;

procedure TcxDBHyperLinkEdit.SetDataBinding(Value: TcxDBTextEditDataBinding);
begin
  FDataBinding.Assign(Value);
end;

procedure TcxDBHyperLinkEdit.SetProperties(Value: TcxHyperLinkEditProperties);
begin
  Properties.Assign(Value);
end;

procedure TcxDBHyperLinkEdit.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := LRESULT(GetcxDBEditDataLink(Self));
end;

{ TcxDBImage }

class function TcxDBImage.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxImageProperties;
end;

class function TcxDBImage.GetDataBindingClass: TcxEditDataBindingClass;
begin
  Result := TcxDBEditDataBinding;
end;

function TcxDBImage.GetActiveProperties: TcxImageProperties;
begin
  Result := TcxImageProperties(InternalGetActiveProperties);
end;

function TcxDBImage.GetDataBinding: TcxDBEditDataBinding;
begin
  Result := TcxDBEditDataBinding(FDataBinding);
end;

function TcxDBImage.GetProperties: TcxImageProperties;
begin
  Result := TcxImageProperties(inherited Properties);
end;

procedure TcxDBImage.SetDataBinding(Value: TcxDBEditDataBinding);
begin
  FDataBinding.Assign(Value);
end;

procedure TcxDBImage.SetProperties(Value: TcxImageProperties);
begin
  Properties.Assign(Value);
end;

procedure TcxDBImage.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := LRESULT(GetcxDBEditDataLink(Self));
end;

{ TcxDBImageComboBox }

class function TcxDBImageComboBox.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxImageComboBoxProperties;
end;

class function TcxDBImageComboBox.GetDataBindingClass: TcxEditDataBindingClass;
begin
  Result := TcxDBTextEditDataBinding;
end;

function TcxDBImageComboBox.GetActiveProperties: TcxImageComboBoxProperties;
begin
  Result := TcxImageComboBoxProperties(InternalGetActiveProperties);
end;

function TcxDBImageComboBox.GetDataBinding: TcxDBTextEditDataBinding;
begin
  Result := TcxDBTextEditDataBinding(FDataBinding);
end;

function TcxDBImageComboBox.GetProperties: TcxImageComboBoxProperties;
begin
  Result := TcxImageComboBoxProperties(inherited Properties);
end;

procedure TcxDBImageComboBox.SetDataBinding(Value: TcxDBTextEditDataBinding);
begin
  FDataBinding.Assign(Value);
end;

procedure TcxDBImageComboBox.SetProperties(Value: TcxImageComboBoxProperties);
begin
  Properties.Assign(Value);
end;

procedure TcxDBImageComboBox.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := LRESULT(GetcxDBEditDataLink(Self));
end;

{ TcxDBMRUEdit }

class function TcxDBMRUEdit.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxMRUEditProperties;
end;

class function TcxDBMRUEdit.GetDataBindingClass: TcxEditDataBindingClass;
begin
  Result := TcxDBTextEditDataBinding;
end;

function TcxDBMRUEdit.GetActiveProperties: TcxMRUEditProperties;
begin
  Result := TcxMRUEditProperties(InternalGetActiveProperties);
end;

function TcxDBMRUEdit.GetDataBinding: TcxDBTextEditDataBinding;
begin
  Result := TcxDBTextEditDataBinding(FDataBinding);
end;

function TcxDBMRUEdit.GetProperties: TcxMRUEditProperties;
begin
  Result := TcxMRUEditProperties(inherited Properties);
end;

procedure TcxDBMRUEdit.SetDataBinding(Value: TcxDBTextEditDataBinding);
begin
  FDataBinding.Assign(Value);
end;

procedure TcxDBMRUEdit.SetProperties(Value: TcxMRUEditProperties);
begin
  Properties.Assign(Value);
end;

procedure TcxDBMRUEdit.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := LRESULT(GetcxDBEditDataLink(Self));
end;

{ TcxDBRadioGroupButton }

constructor TcxDBRadioGroupButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable];
end;

procedure TcxDBRadioGroupButton.CMGetDataLink(var Message: TMessage);
begin
  RadioGroup.Perform(Message.Msg, Message.WParam, Message.LParam);
end;

{ TcxDBRadioGroup }

class function TcxDBRadioGroup.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxRadioGroupProperties;
end;

function TcxDBRadioGroup.GetButtonInstance: TWinControl;
begin
  Result := TcxDBRadioGroupButton.Create(Self);
end;

class function TcxDBRadioGroup.GetDataBindingClass: TcxEditDataBindingClass;
begin
  Result := TcxDBEditDataBinding;
end;

function TcxDBRadioGroup.GetActiveProperties: TcxRadioGroupProperties;
begin
  Result := TcxRadioGroupProperties(InternalGetActiveProperties);
end;

function TcxDBRadioGroup.GetDataBinding: TcxDBEditDataBinding;
begin
  Result := TcxDBEditDataBinding(FDataBinding);
end;

function TcxDBRadioGroup.GetProperties: TcxRadioGroupProperties;
begin
  Result := TcxRadioGroupProperties(inherited Properties);
end;

procedure TcxDBRadioGroup.SetDataBinding(Value: TcxDBEditDataBinding);
begin
  FDataBinding.Assign(Value);
end;

procedure TcxDBRadioGroup.SetProperties(Value: TcxRadioGroupProperties);
begin
  Properties.Assign(Value);
end;

procedure TcxDBRadioGroup.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := LRESULT(GetcxDBEditDataLink(Self));
end;

{ TcxDBListBox }

function TcxDBListBox.GetDataBindingClass: TcxCustomDataBindingClass;
begin
  Result := TcxDBDataBinding;
end;

function TcxDBListBox.GetDataBinding: TcxDBDataBinding;
begin
  Result := TcxDBDataBinding(FDataBinding);
end;

procedure TcxDBListBox.SetDataBinding(Value: TcxDBDataBinding);
begin
  FDataBinding.Assign(Value);
end;

end.
