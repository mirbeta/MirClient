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

unit cxTextEdit;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Windows, Messages, Variants, Classes, Clipbrd, Controls, Forms, Graphics,
  Menus, StdCtrls, SysUtils, Generics.Defaults, Generics.Collections,
  dxCore, dxMessages, dxCoreClasses, cxClasses, cxContainer, cxControls, cxDataUtils,
  dxSpellCheckerCore, cxFormats, cxGraphics, cxVariants, cxFilterControlUtils, cxLookAndFeels, cxEdit, cxDrawTextUtils,
  dxUxTheme, cxLookAndFeelPainters, dxTouch, cxGeometry;

const
  cxEditDefaultDropDownPageRowCount = 8;
  ekValueOutOfBounds = 1;
{$IFDEF USETOPVERTICALALIGNMENTASDEFAULT}
  cxTextEditDefaultVertAlignment: TcxEditVertAlignment = taTopJustify;
{$ELSE}
  cxTextEditDefaultVertAlignment: TcxEditVertAlignment = taVCenter;
{$ENDIF}

type
  TcxEditEchoMode = (eemNormal, eemPassword);
  TcxEditScrollCause = (escKeyboard, escMouseWheel);
  TcxEditValueBound = (evbMin, evbMax);
  TcxTextEditViewStyle = (vsNormal, vsHideCursor, vsButtonsOnly, vsButtonsAutoWidth);
  TcxTextEditCustomDrawHandler = procedure(ACanvas: TcxCanvas; ARect: TRect) of object;

  TcxCustomTextEdit = class;
  TcxEditListBoxContainer = class;

  IcxInnerTextEdit = interface(IcxCustomInnerEdit)
  ['{263EBB8D-1EA9-4CAC-8367-ADD74D2A9651}']
    procedure ClearSelection;
    procedure CopyToClipboard;
    function GetAlignment: TAlignment;
    function GetAutoSelect: Boolean;
    function GetCharCase: TEditCharCase;
    function GetEchoMode: TcxEditEchoMode;
    function GetFirstVisibleCharIndex: Integer;
    function GetHideSelection: Boolean;
    function GetImeLastChar: Char;
    function GetImeMode: TImeMode;
    function GetImeName: TImeName;
    function GetInternalUpdating: Boolean;
    function GetMaxLength: Integer;
    function GetMultiLine: Boolean;
    function GetOEMConvert: Boolean;
    function GetOnSelChange: TNotifyEvent;
    function GetPasswordChar: TCaptionChar;
    function GetSelLength: Integer;
    function GetSelStart: Integer;
    function GetSelText: string;
    function GetUseLeftAlignmentOnEditing: Boolean;
    procedure SelectAll;
    procedure SetAlignment(Value: TAlignment);
    procedure SetAutoSelect(Value: Boolean);
    procedure SetCharCase(Value: TEditCharCase);
    procedure SetEchoMode(Value: TcxEditEchoMode);
    procedure SetHideSelection(Value: Boolean);
    procedure SetInternalUpdating(Value: Boolean);
    procedure SetImeMode(Value: TImeMode);
    procedure SetImeName(const Value: TImeName);
    procedure SetMaxLength(Value: Integer);
    procedure SetOEMConvert(Value: Boolean);
    procedure SetOnSelChange(Value: TNotifyEvent);
    procedure SetPasswordChar(Value: TCaptionChar);
    procedure SetSelLength(Value: Integer);
    procedure SetSelStart(Value: Integer);
    procedure SetSelText(Value: string);
    procedure SetUseLeftAlignmentOnEditing(Value: Boolean);
    function GetTextHint: string;
    procedure SetTextHint(Value: string);
    property Alignment: TAlignment read GetAlignment write SetAlignment;
    property AutoSelect: Boolean read GetAutoSelect write SetAutoSelect;
    property CharCase: TEditCharCase read GetCharCase write SetCharCase;
    property EchoMode: TcxEditEchoMode read GetEchoMode write SetEchoMode;
    property HideSelection: Boolean read GetHideSelection write SetHideSelection;
    property ImeLastChar: Char read GetImeLastChar;
    property ImeMode: TImeMode read GetImeMode write SetImeMode;
    property ImeName: TImeName read GetImeName write SetImeName;
    property InternalUpdating: Boolean read GetInternalUpdating write SetInternalUpdating;
    property MaxLength: Integer read GetMaxLength write SetMaxLength;
    property MultiLine: Boolean read GetMultiLine;
    property OEMConvert: Boolean read GetOEMConvert write SetOEMConvert;
    property PasswordChar: TCaptionChar read GetPasswordChar write SetPasswordChar;
    property SelLength: Integer read GetSelLength write SetSelLength;
    property SelStart: Integer read GetSelStart write SetSelStart;
    property SelText: string read GetSelText write SetSelText;
    property UseLeftAlignmentOnEditing: Boolean read GetUseLeftAlignmentOnEditing write SetUseLeftAlignmentOnEditing;
    property TextHint: string read GetTextHint write SetTextHint;
    property OnSelChange: TNotifyEvent read GetOnSelChange write SetOnSelChange;
  end;

  { TcxCustomInnerTextEditHelper }

  TcxCustomInnerTextEdit = class;

  TcxCustomInnerTextEditHelper = class(TcxInterfacedPersistent,
    IcxContainerInnerControl, IcxCustomInnerEdit, IcxInnerTextEdit)
  private
    FEdit: TcxCustomInnerTextEdit;
    FSelLength: Integer;
    FSelStart: Integer;
  protected
    property Edit: TcxCustomInnerTextEdit read FEdit;
  public
    constructor Create(AEdit: TcxCustomInnerTextEdit); reintroduce; virtual;

    // IcxContainerInnerControl
    function GetControlContainer: TcxContainer;
    function GetControl: TWinControl;

    // IcxCustomInnerEdit
    function CallDefWndProc(AMsg: UINT; WParam: WPARAM;
      LParam: LPARAM): LRESULT;
    function CanProcessClipboardMessages: Boolean;
    function GetEditValue: TcxEditValue;
    function GetOnChange: TNotifyEvent;
    procedure LockBounds(ALock: Boolean);
    procedure SafelySetFocus;
    procedure SetEditValue(const Value: TcxEditValue);
    procedure SetParent(Value: TWinControl);
    procedure SetOnChange(Value: TNotifyEvent);

    // IcxInnerTextEdit
    procedure ClearSelection;
    procedure CopyToClipboard;
    function GetAlignment: TAlignment;
    function GetAutoSelect: Boolean;
    function GetCharCase: TEditCharCase;
    function GetEchoMode: TcxEditEchoMode;
    function GetFirstVisibleCharIndex: Integer;
    function GetHideSelection: Boolean;
    function GetImeLastChar: Char;
    function GetImeMode: TImeMode;
    function GetImeName: TImeName;
    function GetInternalUpdating: Boolean;
    function GetIsInplace: Boolean;
    function GetMaxLength: Integer;
    function GetMultiLine: Boolean;
    function GetOEMConvert: Boolean;
    function GetOnSelChange: TNotifyEvent;
    function GetPasswordChar: TCaptionChar;
    function GetReadOnly: Boolean;
    function GetSelLength: Integer;
    function GetSelStart: Integer;
    function GetSelText: string;
    function GetUseLeftAlignmentOnEditing: Boolean;
    procedure SelectAll;
    procedure SetAlignment(Value: TAlignment);
    procedure SetAutoSelect(Value: Boolean);
    procedure SetCharCase(Value: TEditCharCase);
    procedure SetEchoMode(Value: TcxEditEchoMode);
    procedure SetHideSelection(Value: Boolean);
    procedure SetInternalUpdating(Value: Boolean);
    procedure SetImeMode(Value: TImeMode);
    procedure SetImeName(const Value: TImeName);
    procedure SetMaxLength(Value: Integer);
    procedure SetOEMConvert(Value: Boolean);
    procedure SetOnSelChange(Value: TNotifyEvent);
    procedure SetPasswordChar(Value: TCaptionChar);
    procedure SetReadOnly(Value: Boolean);
    procedure SetSelLength(Value: Integer);
    procedure SetSelStart(Value: Integer);
    procedure SetSelText(Value: string);
    procedure SetUseLeftAlignmentOnEditing(Value: Boolean);
    function GetTextHint: string;
    procedure SetTextHint(Value: string);
  end;

  { TcxCustomInnerTextEdit }

  TcxCustomInnerTextEditPrevState = record
    IsPrevTextSaved: Boolean;
    PrevText: string;
    PrevSelLength, PrevSelStart: Integer;
  end;

  TcxCustomInnerTextEdit = class(TCustomEdit, IUnknown,
    IcxContainerInnerControl, IcxInnerEditHelper)
  private
    FAlignment: TAlignment;
    FEchoMode: TcxEditEchoMode;
    FHelper: TcxCustomInnerTextEditHelper;
    FImeCharCount: Integer;
    FImeLastChar: Char;
    FInternalUpdating: Boolean;
    FIsCreating: Boolean;
    FLockBoundsCount: Integer;
    FPasswordChar: TCaptionChar;
  {$IFNDEF VCLGLASSPAINT}
    FRepaintOnGlass: Boolean;
  {$ENDIF}
    FUseLeftAlignmentOnEditing: Boolean;
    FOnSelChange: TNotifyEvent;
    function GetAlignment: TAlignment;
    function GetContainer: TcxCustomTextEdit;
    function GetCursorPos: Integer;
    function GetIsDestroying: Boolean;
    function GetRealAlignment: TAlignment;
    function IsDesigning: Boolean;
    procedure InternalPaint(ADC: THandle);
    procedure SetAlignment(Value: TAlignment);
    procedure SetEchoMode(const Value: TcxEditEchoMode);
    procedure SetUseLeftAlignmentOnEditing(Value: Boolean);
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure EMReplaceSel(var Message: TMessage); message EM_REPLACESEL;
    procedure EMSetSel(var Message: TMessage); message EM_SETSEL;
    procedure WMChar(var Message: TWMChar); message WM_CHAR;
    procedure WMClear(var Message: TMessage); message WM_CLEAR;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMIMEChar(var Message: TMessage); message WM_IME_CHAR;
    procedure WMIMEComposition(var Message: TMessage); message WM_IME_COMPOSITION;
    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMNCPaint(var Message: TWMNCPaint); message WM_NCPAINT;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMPrintClient(var Message: TWMPrintClient); message WM_PRINTCLIENT;
    procedure WMSetCursor(var Message: TWMSetCursor); message WM_SETCURSOR;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMSetFont(var Message: TWMSetFont); message WM_SETFONT;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMUndo(var Message: TWMSize); message WM_UNDO;
  protected
    // IcxContainerInnerControl
    function GetControl: TWinControl;
    function GetControlContainer: TcxContainer;
    // IcxInnerEditHelper
    function GetHelper: IcxCustomInnerEdit;

    procedure Change; override; // for Delphi .NET
    procedure Click; override;
    procedure CreateHandle; override;
    function CreateHelper: TcxCustomInnerTextEditHelper; virtual;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWindowHandle(const Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DblClick; override;
    procedure DestroyWnd; override;

    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean); override;
    function GetBasedAlignment: TAlignment;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure WndProc(var Message: TMessage); override;
    procedure AdjustMargins; virtual;
    procedure MouseEnter(AControl: TControl); dynamic;
    procedure MouseLeave(AControl: TControl); dynamic;
    procedure RecreateWnd; // for Delphi .NET

    property Alignment: TAlignment read FAlignment write SetAlignment;
    property Container: TcxCustomTextEdit read GetContainer;
    property CursorPos: Integer read GetCursorPos;
    property Helper: TcxCustomInnerTextEditHelper read FHelper;
    property IsDestroying: Boolean read GetIsDestroying;
    property UseLeftAlignmentOnEditing: Boolean read FUseLeftAlignmentOnEditing write SetUseLeftAlignmentOnEditing;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DragDrop(Source: TObject; X, Y: Integer); override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    function CanFocus: Boolean; override;
    procedure DefaultHandler(var Message); override;
    function GetControlsAlignment: TAlignment; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    property AutoSelect;
    property CharCase;
    property EchoMode: TcxEditEchoMode read FEchoMode write SetEchoMode;
    property HideSelection;
    property ImeMode;
    property ImeName;
    property MaxLength;
    property OEMConvert;
    property ReadOnly;
    property OnChange;
    property OnSelChange: TNotifyEvent read FOnSelChange write FOnSelChange;
  end;

  { TcxTextEditPropertiesValues }

  TcxTextEditPropertiesValues = class(TcxCustomEditPropertiesValues)
  private
    FDisplayFormat: Boolean;
    FEditFormat: Boolean;
    FMaxLength: Boolean;
    function IsDisplayFormatStored: Boolean;
    function IsEditFormatStored: Boolean;
    procedure SetDisplayFormat(Value: Boolean);
    procedure SetEditFormat(Value: Boolean);
    procedure SetMaxLength(Value: Boolean);
  public
    procedure Assign(Source: TPersistent); override;
    procedure RestoreDefaults; override;
  published
    property DisplayFormat: Boolean read FDisplayFormat write SetDisplayFormat
      stored IsDisplayFormatStored;
    property EditFormat: Boolean read FEditFormat write SetEditFormat
      stored IsEditFormatStored;
    property MaxLength: Boolean read FMaxLength write SetMaxLength stored False;
    property MaxValue;
    property MinValue;
  end;

  { TcxCustomEditListBox }

  TcxCustomEditListBox = class(TcxCustomInnerListBox, IdxGestureClient, IdxGestureOwner)
  strict private
    FGestureAccumulatedDelta: TPoint;
    FGestureHelper: TdxGestureHelper;
    FHotTrack: Boolean;
    FMouseWheelAccumulator: Integer;
    FPrevMousePos: TPoint;
    FOnSelectItem: TNotifyEvent;

    function GetEdit: TcxCustomTextEdit;
    function GetScaleFactor: TdxScaleFactor;
    procedure CMShowingChanged(var Message: TMessage); message CM_SHOWINGCHANGED;
  private
    function GetContainer: TcxEditListBoxContainer;
  protected
    // IdxGestureClient
    function AllowGesture(AGestureId: Integer): Boolean;
    function AllowPan(AScrollKind: TScrollBarKind): Boolean;
    procedure BeginGestureScroll(APos: TPoint);
    procedure EndGestureScroll;
    procedure GestureScroll(ADeltaX, ADeltaY: Integer);
    function GetPanOptions: Integer;
    function IsPanArea(const APoint: TPoint): Boolean;
    function NeedPanningFeedback(AScrollKind: TScrollBarKind): Boolean;
    // IdxGestureOwner
    function GetGestureClient(const APoint: TPoint): IdxGestureClient;
    function IdxGestureOwner.GetHandle = GetGestureClientHandle;
    function GetGestureClientHandle: THandle;
    function IsGestureTarget(AWnd: THandle): Boolean;
    // touch
    procedure DoGesture(const EventInfo: TGestureEventInfo; var Handled: Boolean); override;
    procedure DoGetGestureOptions(var Gestures: TInteractiveGestures; var Options: TInteractiveGestureOptions); override;
    //
    procedure Click; override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
    procedure DrawItemText(const AText: string; const ARect: TRect);
    function GetDrawTextFlags: Cardinal; virtual;
    function GetItemData(Index: Integer): {$IFDEF DELPHI16}TListBoxItemData{$ELSE}Longint{$ENDIF}; override;
    function IsHighlightSearchText: Boolean; virtual;
    function NeedDrawFocusRect: Boolean; override;
    procedure MeasureItem(Index: Integer; var Height: Integer); override;
    procedure MouseLeave(AControl: TControl); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure SetItemData(Index: Integer;
      AData: {$IFDEF DELPHI16}TListBoxItemData{$ELSE}Longint{$ENDIF}); override;
    function DoDrawItem(AIndex: Integer; const ARect: TRect; AState: TOwnerDrawState): Boolean; virtual;
    procedure DoSelectItem;
    function GetDefaultItemHeight: Integer;
    function GetItem(Index: Integer): string; virtual;
    procedure InternalRecreateWindow;
    procedure RecreateWindow; virtual;
    procedure SetItemCount(Value: Integer);
    procedure SetItemIndex(const Value: Integer); override;
    procedure WndProc(var Message: TMessage); override;

    property Container: TcxEditListBoxContainer read GetContainer;
    property Edit: TcxCustomTextEdit read GetEdit;
    property HotTrack: Boolean read FHotTrack write FHotTrack;
    property ScaleFactor: TdxScaleFactor read GetScaleFactor;
    property OnSelectItem: TNotifyEvent read FOnSelectItem write FOnSelectItem;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CanFocus: Boolean; override;
    function GetHeight(ARowCount: Integer; AMaxHeight: Integer): Integer; virtual;
    function GetItemHeight(AIndex: Integer = -1): Integer; virtual;
    function GetItemWidth(AIndex: Integer): Integer; virtual;
    function IsVisible: Boolean;
    procedure SetScrollWidth(Value: Integer);
  end;

  TcxCustomEditListBoxClass = class of TcxCustomEditListBox;

  TcxEditListBoxContainer = class(TcxCustomEditContainer)
  private
    procedure WMWindowPosChanged(var Message: TWMWindowPosChanged); message WM_WINDOWPOSCHANGED;
  protected
    function AllowTouchScrollUIMode: Boolean; override;
    procedure DoScrollUIModeChanged; override;
    function GetBorderExtent: TRect; override;
    function IsMouseWheelHandleNeeded(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    function NeedsScrollBars: Boolean; override;
    procedure Scroll(AScrollBarKind: TScrollBarKind; AScrollCode: TScrollCode; var AScrollPos: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    function CanFocus: Boolean; override;
  end;

  { IcxTextEditLookupData }

  TcxEditLookupDataGoDirection = (egdBegin, egdEnd, egdNext, egdPrev, egdPageUp, egdPageDown);

  IcxTextEditLookupData = interface
  ['{F49C5F08-7758-4362-A360-1DF02354E708}']
    function CanResizeVisualArea(var NewSize: TSize;
      AMaxHeight: Integer = 0; AMaxWidth: Integer = 0): Boolean;
    procedure CloseUp;
    procedure Deinitialize;
    procedure DropDown;
    procedure DroppedDown(const AFindStr: string);
    function Find(const AText: string): Boolean;
    function GetActiveControl: TControl;
    function GetCurrentKey: TcxEditValue;
    function GetDisplayText(const AKey: TcxEditValue): string; overload;
    function GetOnCurrentKeyChanged: TNotifyEvent;
    function GetOnSelectItem: TNotifyEvent;
    function GetSelectedItem: Integer;
    function GetVisualAreaPreferredSize(AMaxHeight: Integer; AWidth: Integer = 0): TSize;
    procedure Go(ADirection: TcxEditLookupDataGoDirection; ACircular: Boolean);
    procedure Initialize(AVisualControlsParent: TWinControl);
    function IsEmpty: Boolean;
    function IsMouseOverList(const P: TPoint): Boolean;
    function Locate(var AText, ATail: string; ANext: Boolean): Boolean;
    procedure PositionVisualArea(const AClientRect: TRect);
    procedure PropertiesChanged;
    procedure SelectItem;
    procedure SetCurrentKey(const AKey: TcxEditValue);
    procedure SetOnCurrentKeyChanged(Value: TNotifyEvent);
    procedure SetOnSelectItem(Value: TNotifyEvent);
    procedure SetSelectedItem(Value: Integer);
    procedure TextChanged;
    property ActiveControl: TControl read GetActiveControl;
    property CurrentKey: TcxEditValue read GetCurrentKey write SetCurrentKey;
    property SelectedItem: Integer read GetSelectedItem write SetSelectedItem;
    property OnCurrentKeyChanged: TNotifyEvent read GetOnCurrentKeyChanged write SetOnCurrentKeyChanged;
    property OnSelectItem: TNotifyEvent read GetOnSelectItem write SetOnSelectItem;
  end;

  IcxTextEditLookupDataIncrementalFilter = interface
    ['{80037B39-866D-45C5-AB85-176FE576CF2A}']
    procedure ResetIncrementalFilter;
  end;

  { TcxCustomTextEditLookupData }

  TcxCustomTextEditProperties = class;

  TcxCustomTextEditLookupData = class(TcxInterfacedPersistent, IcxTextEditLookupData, IcxTextEditLookupDataIncrementalFilter)
  private
    FSearchText: string;

    FCurrentKey: Integer;
    FFilteredLookupItems: TList<Integer>;
    FIsInitializing: Boolean;
    FItemIndex: Integer;
    FList: TcxCustomEditListBox;
    FListContainer: TcxEditListBoxContainer;
    FOwner: TPersistent;
    FOnCurrentKeyChanged: TNotifyEvent;
    FOnSelectItem: TNotifyEvent;

    function FindItemByText(const AText: string; AStartIndex: Integer; AFindFullText: Boolean): Integer;
    function GetEdit: TcxCustomTextEdit;
    function GetItems: TStrings;
    function GetActiveProperties: TcxCustomTextEditProperties;
    function GetScaleFactor: TdxScaleFactor;
    function IndexOf(const AText: string): Integer;
    procedure SetItemIndex(Value: Integer);
    procedure SetItems(Value: TStrings);
  protected
    function GetOwner: TPersistent; override;
    procedure DoCurrentKeyChanged;
    procedure DoInitialize(AVisualControlsParent: TWinControl); virtual;
    procedure DoPositionVisualArea(const ARect: TRect); virtual;
    procedure DoSelectItem;
    function GetFilteredItem(Index: Integer): string;
    function GetLookupItemIndexFromFilteredItemIndex(Index: Integer): Integer;
    function GetFilteredItemIndex(Index: Integer): Integer;
    function GetFilteredItemCount: Integer;
    function GetItem(Index: Integer): string; virtual;
    function GetItemCount: Integer; virtual;
    function GetListBoxClass: TcxCustomEditListBoxClass; virtual;
    function GetSelectedItem: Integer; virtual;
    procedure HandleSelectItem(Sender: TObject); virtual; // TODO test for CLX
    function InternalLocate(var AText, ATail: string; ANext, ASynchronizeWithText: Boolean): Boolean; virtual;
    procedure InternalSetItemIndex(Value: Integer);
    function IsFilterActive: Boolean; virtual;
    function IsIncrementalFiltering: Boolean;
    function IsLikeTypeFiltering: Boolean;
    procedure ListChanged; virtual;
    function NeedLocateItemWithFullString: Boolean; virtual;
    procedure ResetIncrementalFilter;
    procedure SetSelectedItem(Value: Integer); virtual;
    procedure UpdateFilteredLookupItems(const ASearchText: string); virtual;
    function UseSearchControl: Boolean; virtual;

    property ActiveProperties: TcxCustomTextEditProperties read GetActiveProperties;
    property Edit: TcxCustomTextEdit read GetEdit;
    property IsInitializing: Boolean read FIsInitializing;
    property ItemIndex: Integer read FItemIndex write SetItemIndex stored False;
    property Items: TStrings read GetItems write SetItems;
    property List: TcxCustomEditListBox read FList;
    property ListContainer: TcxEditListBoxContainer read FListContainer;
    property ScaleFactor: TdxScaleFactor read GetScaleFactor;
    property SearchText: string read FSearchText;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    function CanResizeVisualArea(var NewSize: TSize;
      AMaxHeight: Integer = 0; AMaxWidth: Integer = 0): Boolean; virtual;
    procedure CloseUp; virtual;
    procedure Deinitialize;
    procedure DropDown; virtual;
    procedure DroppedDown(const AFindStr: string); virtual;
    function Find(const AText: string): Boolean; virtual;
    function GetActiveControl: TControl; virtual;
    function GetCurrentKey: TcxEditValue;
    function GetDisplayText(const AKey: TcxEditValue): string; overload;
    function GetOnCurrentKeyChanged: TNotifyEvent;
    function GetOnSelectItem: TNotifyEvent;
    function GetVisualAreaPreferredSize(AMaxHeight: Integer; AWidth: Integer = 0): TSize; virtual;
    procedure Go(ADirection: TcxEditLookupDataGoDirection; ACircular: Boolean);
    procedure Initialize(AVisualControlsParent: TWinControl); virtual;
    procedure InternalSetCurrentKey(Value: Integer);
    function IsEmpty: Boolean;
    function IsMouseOverList(const P: TPoint): Boolean;
    function Locate(var AText, ATail: string; ANext: Boolean): Boolean;
    procedure PositionVisualArea(const AClientRect: TRect); virtual;
    procedure PropertiesChanged; virtual;
    procedure SelectItem;
    procedure SetCurrentKey(const AKey: TcxEditValue);
    procedure SetOnCurrentKeyChanged(Value: TNotifyEvent);
    procedure SetOnSelectItem(Value: TNotifyEvent);
    procedure TextChanged; virtual;
    property ActiveControl: TControl read GetActiveControl;
    property CurrentKey: TcxEditValue read GetCurrentKey write SetCurrentKey;
    property OnCurrentKeyChanged: TNotifyEvent read GetOnCurrentKeyChanged write SetOnCurrentKeyChanged;
    property OnSelectItem: TNotifyEvent read GetOnSelectItem write SetOnSelectItem;
  end;

  { TcxCustomTextEditViewData }

  TcxCustomTextEditViewData = class(TcxCustomEditViewData)
  private
    FIsValueFormatted: Boolean;
    function GetProperties: TcxCustomTextEditProperties;
  protected
    procedure CalculateButtonNativePartInfo(ATheme: TdxTheme; AButtonViewInfo: TcxEditButtonViewInfo); override;
    procedure InitCacheData; override;
    function InternalEditValueToDisplayText(AEditValue: TcxEditValue): string; override;
    function InternalGetEditContentSize(ACanvas: TcxCanvas; const AEditValue: TcxEditValue;
      const AEditSizeProperties: TcxEditSizeProperties): TSize; override;

    function GetIsEditClass: Boolean; virtual;
    function GetMaxLineCount: Integer; virtual;
    function IsComboBoxStyle: Boolean; virtual;
    procedure PrepareDrawTextFlags(ACanvas: TcxCanvas; AViewInfo: TcxCustomEditViewInfo); virtual;
  public
    procedure Calculate(ACanvas: TcxCanvas; const ABounds: TRect; const P: TPoint;
      Button: TcxMouseButton; Shift: TShiftState; ViewInfo: TcxCustomEditViewInfo;
      AIsMouseEvent: Boolean); override;
    function GetClientExtent(ACanvas: TcxCanvas;
      AViewInfo: TcxCustomEditViewInfo): TRect; override;

    procedure DisplayValueToDrawValue(const ADisplayValue: TcxEditValue;
      AViewInfo: TcxCustomEditViewInfo); virtual;
    procedure EditValueToDrawValue(const AEditValue: TcxEditValue;
      AViewInfo: TcxCustomEditViewInfo); override;

    function GetDrawTextFlags: DWORD; virtual;
    function GetDrawTextOffset: TRect; virtual;
    procedure PrepareSelection(AViewInfo: TcxCustomEditViewInfo);

    property Properties: TcxCustomTextEditProperties read GetProperties;
  end;

  { TcxCustomTextEditViewInfo }

  TcxTextOutData = record
    ForceEndEllipsis: Boolean;
    Initialized: Boolean;
    RowCount: Integer;
    SelStart, SelLength: Integer;
    SelBackgroundColor, SelTextColor: TColor;
    TextParams: TcxTextParams;
    TextRect: TRect;
    TextRows: TcxTextRows;
  end;

  TcxCustomTextEditViewInfo = class(TcxCustomEditViewInfo)
  protected
    function GetBackgroundPaintingStyle: TcxEditBackgroundPaintingStyle; override;
    procedure InternalPaint(ACanvas: TcxCanvas); override;
    function NeedDrawPasswordBullets: Boolean;
  public
    ComboBoxStyle: Boolean;
    CustomDrawHandler: TcxTextEditCustomDrawHandler;
    DrawSelectionBar: Boolean;
    DrawTextFlags: DWORD;
    EditingStyle: TcxEditEditingStyle;
    HasPopupWindow: Boolean;
    IsEditClass: Boolean;
    IsOwnerDrawing: Boolean;
    MaxLineCount: Integer;
    SelStart, SelLength: Integer;
    SelTextColor, SelBackgroundColor: TColor;
    Text: string;
    TextOutData: TcxTextOutData;
    TextRect: TRect;
    destructor Destroy; override;
    function NeedShowHint(ACanvas: TcxCanvas; const P: TPoint;
      const AVisibleBounds: TRect; out AText: TCaption;
      out AIsMultiLine: Boolean; out ATextRect: TRect; AMaxLineCount: Integer = 0): Boolean; override;
    procedure Offset(DX, DY: Integer); override;
    procedure DrawText(ACanvas: TcxCanvas); virtual;
    function GetTextBaseLine: Integer; virtual;
    procedure PrepareCanvasFont(ACanvas: TCanvas); override;
  end;

{ TcxCustomTextEditProperties }

  TcxNewLookupDisplayTextEvent = procedure(Sender: TObject; const AText: TCaption) of object;
  TcxTextEditChars = set of AnsiChar;

  TcxTextEditIncrementalFilteringOption = (ifoHighlightSearchText, ifoUseContainsOperator);
  TcxTextEditIncrementalFilteringOptions = set of TcxTextEditIncrementalFilteringOption;

  TcxCustomTextEditProperties = class(TcxCustomEditProperties, IcxFormatControllerListener)
  private
    FCharCase: TEditCharCase;
    FDisplayFormat: string;
    FEchoMode: TcxEditEchoMode;
    FEditFormat: string;
    FFixedListSelection: Boolean;
    FFormatChanging: Boolean;
    FHideCursor: Boolean;
    FHideSelection: Boolean;
    FImeMode: TImeMode;
    FImeName: TImeName;
    FImmediateUpdateText: Boolean;
    FIncrementalFiltering: Boolean;
    FIncrementalFilteringOptions: TcxTextEditIncrementalFilteringOptions;
    FIncrementalSearch: Boolean;
    FLookupItems: TStringList;
    FMaxLength: Integer;
    FMRUMode: Boolean;
    FNullstring: string;
    FOEMConvert: Boolean;
    FPasswordChar: TCaptionChar;
    FUseDisplayFormatWhenEditing: Boolean;
    FUseNullString: Boolean;
    FValidChars: TcxTextEditChars;

    FOnNewLookupDisplayText: TcxNewLookupDisplayTextEvent;

    function GetAssignedValues: TcxTextEditPropertiesValues;
    function GetDisplayFormat: string;
    function GetEditFormat: string;
    function GetLookupItems: TStrings;
    function GetLookupItemsSorted: Boolean;
    function GetMaxLength: Integer;
    function GetViewStyle: TcxTextEditViewStyle;
    function IsDisplayFormatStored: Boolean;
    function IsEditFormatStored: Boolean;
    function IsMaxLengthStored: Boolean;
    procedure LookupItemsChanged(Sender: TObject);
    procedure ReadIsDisplayFormatAssigned(Reader: TReader); // obsolete
    procedure SetAssignedValues(Value: TcxTextEditPropertiesValues);
    procedure SetDisplayFormat(const Value: string);
    procedure SetEchoMode(Value: TcxEditEchoMode);
    procedure SetEditFormat(const Value: string);
    procedure SetFixedListSelection(Value: Boolean);
    procedure SetHideCursor(Value: Boolean);
    procedure SetHideSelection(Value: Boolean);
    procedure SetImeMode(Value: TImeMode);
    procedure SetImeName(const Value: TImeName);
    procedure SetIncrementalSearch(Value: Boolean);
    procedure SetLookupItems(Value: TStrings);
    procedure SetLookupItemsSorted(Value: Boolean);
    procedure SetMaxLength(Value: Integer);
    procedure SetMRUMode(Value: Boolean);
    procedure SetNullstring(const Value: string);
    procedure SetOEMConvert(Value: Boolean);
    procedure SetPasswordChar(Value: TCaptionChar);
    procedure SetUseDisplayFormatWhenEditing(Value: Boolean);
    procedure SetUseNullString(const Value: Boolean);
    procedure SetViewStyle(Value: TcxTextEditViewStyle);
  protected
    procedure AlignmentChangedHandler(Sender: TObject); override;
    procedure BaseSetAlignment(Value: TcxEditAlignment); override;
    function CanValidate: Boolean; override;
    procedure DefineProperties(Filer: TFiler); override; // obsolete
    procedure DoAssign(AProperties: TcxCustomEditProperties); override;
    class function GetAssignedValuesClass: TcxCustomEditPropertiesValuesClass; override;
    function GetDisplayFormatOptions: TcxEditDisplayFormatOptions; override;
    function GetValidateErrorText(AErrorKind: TcxEditErrorKind): string; override;
    class function GetViewDataClass: TcxCustomEditViewDataClass; override;
    function HasDisplayValue: Boolean; override;

    // IcxFormatControllerListener
    procedure FormatChanged; virtual;

    function CanIncrementalSearch: Boolean;
    procedure CheckEditValueBounds(const AEditValue: TcxEditValue;
      var AErrorText: TCaption; var AError: Boolean; AEdit: TcxCustomEdit); virtual;
    procedure CheckDisplayValueBounds(const ADisplayValue: TcxEditValue;
      var AErrorText: TCaption; var AError: Boolean; AEdit: TcxCustomEdit); virtual;
    function DefaultFocusedDisplayValue: TcxEditValue; virtual;
    procedure DoDisplayValueToDisplayText(ANativeStyle: Boolean; var ADisplayValue: string);
    procedure DoPrepareDisplayValue(const AEditValue: TcxEditValue; var ADisplayValue: TcxEditValue; AEditFocused: Boolean); virtual;
    function FindLookupText(const AText: string): Boolean; virtual;
    function GetDefaultDisplayFormat: string; virtual;
    function GetDefaultDisplayValue(const AEditValue: TcxEditValue; AEditFocused: Boolean): TcxEditValue;
    function GetDefaultIncrementalFilteringOptions: TcxTextEditIncrementalFilteringOptions; virtual;
    function GetDefaultMaxLength: Integer; virtual;
    function GetDefaultVertAlignment: TcxEditVertAlignment; override;
    function GetDropDownPageRowCount: Integer; virtual;
    function GetEditingStyle: TcxEditEditingStyle; virtual;
    class function GetLookupDataClass: TcxInterfacedPersistentClass; virtual;
    function HasDigitGrouping(AIsDisplayValueSynchronizing: Boolean): Boolean; virtual;
    function InternalGetEditFormat(out AIsCurrency, AIsOnGetTextAssigned: Boolean;
      AEdit: TcxCustomTextEdit = nil): string; virtual;
    procedure LookupDataChanged(Sender: TObject); virtual;
    procedure MaxLengthChanged; virtual;
    function NeedUseNullString(AEditValue: TcxEditValue): Boolean;
    procedure SetCharCase(Value: TEditCharCase); virtual;
    procedure SetIncrementalFilteringOptions(Value: TcxTextEditIncrementalFilteringOptions); virtual;
    function UseLookupData: Boolean; virtual;
    function UseSearchControl: Boolean; virtual;

    function IsEditValueEmpty(const AEditValue: TcxEditValue): Boolean; virtual;
    function IsEditValueNumeric: Boolean; virtual;
    function IsLookupDataVisual: Boolean; virtual;
    function IsLookupEdit: Boolean; virtual;
    function IsMultiLine: Boolean; virtual;
    function IsPopupKey(Key: Word; Shift: TShiftState): Boolean; virtual;
    function IsValueBoundDefined(ABound: TcxEditValueBound): Boolean; virtual;
    function IsValueBoundsDefined: Boolean; virtual;

    property AssignedValues: TcxTextEditPropertiesValues read GetAssignedValues
      write SetAssignedValues;
    property EditingStyle: TcxEditEditingStyle read GetEditingStyle;
    property FixedListSelection: Boolean read FFixedListSelection
      write SetFixedListSelection default True;
    property FormatChanging: Boolean read FFormatChanging;
    property HideCursor: Boolean read FHideCursor write SetHideCursor
      stored False;
    property MRUMode: Boolean read FMRUMode write SetMRUMode default False;
    property Nullstring: string read FNullstring write SetNullstring;
    property UseNullString: Boolean read FUseNullString write SetUseNullString default False;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    function CanCompareEditValue: Boolean; override;
    function CompareDisplayValues(
      const AEditValue1, AEditValue2: TcxEditValue): Boolean; override;
    class function GetContainerClass: TcxContainerClass; override;
    function GetDisplayText(const AEditValue: TcxEditValue;
      AFullText: Boolean = False; AIsInplace: Boolean = True): string; override;
    function GetEditValueSource(AEditFocused: Boolean): TcxDataEditValueSource; override;
    function GetSupportedOperations: TcxEditSupportedOperations; override;
    class function GetViewInfoClass: TcxContainerViewInfoClass; override;
    function IsEditValueValid(var EditValue: TcxEditValue; AEditFocused: Boolean): Boolean; override;
    function IsResetEditClass: Boolean; override;

    procedure PrepareDisplayValue(const AEditValue: TcxEditValue; var ADisplayValue: TcxEditValue; AEditFocused: Boolean); override;

    procedure ValidateDisplayValue(var DisplayValue: TcxEditValue; var ErrorText: TCaption;
      var Error: Boolean; AEdit: TcxCustomEdit); override;
    procedure DisplayValueToDisplayText(var ADisplayValue: string); virtual;
    function IsDisplayValueValid(var ADisplayValue: TcxEditValue; AEditFocused: Boolean): Boolean; virtual;
    procedure SetMinMaxValues(AMinValue, AMaxValue: Double);
    property ValidChars: TcxTextEditChars read FValidChars write FValidChars;
    // !!!
    property CharCase: TEditCharCase read FCharCase write SetCharCase default ecNormal;
    property DisplayFormat: string read GetDisplayFormat write SetDisplayFormat
       stored IsDisplayFormatStored;
    property EchoMode: TcxEditEchoMode read FEchoMode write SetEchoMode default eemNormal;
    property EditFormat: string read GetEditFormat write SetEditFormat
       stored IsEditFormatStored;
    property HideSelection: Boolean read FHideSelection write SetHideSelection default True;
    property ImeMode: TImeMode read FImeMode write SetImeMode default imDontCare;
    property ImeName: TImeName read FImeName write SetImeName;
    property ImmediateUpdateText: Boolean read FImmediateUpdateText write FImmediateUpdateText default False;
    property IncrementalFiltering: Boolean read FIncrementalFiltering write FIncrementalFiltering default False;
    property IncrementalFilteringOptions: TcxTextEditIncrementalFilteringOptions read FIncrementalFilteringOptions
      write SetIncrementalFilteringOptions default [ifoHighlightSearchText, ifoUseContainsOperator];
    property IncrementalSearch: Boolean read FIncrementalSearch
      write SetIncrementalSearch default True;
    property LookupItems: TStrings read GetLookupItems write SetLookupItems;
    property LookupItemsSorted: Boolean read GetLookupItemsSorted write
      SetLookupItemsSorted default False;
    property MaxLength: Integer read GetMaxLength write SetMaxLength stored IsMaxLengthStored;
    property MaxValue;
    property MinValue;
    property OEMConvert: Boolean read FOEMConvert write SetOEMConvert default False;
    property PasswordChar: TCaptionChar read FPasswordChar write SetPasswordChar
      default #0;
    property UseDisplayFormatWhenEditing: Boolean
      read FUseDisplayFormatWhenEditing write SetUseDisplayFormatWhenEditing
      default False;
    property ViewStyle: TcxTextEditViewStyle read GetViewStyle write SetViewStyle default vsNormal;
    property OnNewLookupDisplayText: TcxNewLookupDisplayTextEvent read
      FOnNewLookupDisplayText write FOnNewLookupDisplayText;
  end;

  { TcxTextEditProperties }

  TcxTextEditProperties = class(TcxCustomTextEditProperties)
  published
    property Alignment;
    property AssignedValues;
    property AutoSelect;
    property BeepOnError;
    property CharCase;
    property ClearKey;
    property EchoMode;
    property HideSelection;
    property ImeMode;
    property ImeName;
    property IncrementalSearch;
    property LookupItems;
    property LookupItemsSorted;
    property MaxLength;
    property Nullstring;
    property OEMConvert;
    property PasswordChar;
    property ReadOnly;
    property UseLeftAlignmentOnEditing;
    property UseNullString;
    property ValidateOnEnter;
    property ValidationErrorIconAlignment;
    property ValidationOptions;
    property OnChange;
    property OnEditValueChanged;
    property OnNewLookupDisplayText;
    property OnValidate;
  end;

  { TcxCustomTextEdit }

  TcxCustomTextEdit = class(TcxCustomEdit, IcxFormatControllerListener)
  private
    FBeepOnEnter: Boolean;
    FDisableRefresh: Boolean;
    FFindSelection: Boolean;
    FInternalTextSetting: Boolean;
    FIsDisplayValueSynchronizing: Boolean;
    FLastFirstVisibleCharIndex: Integer;
    FLastSelLength: Integer;
    FLastSelPosition: Integer;
    FLookupItemsScrolling: Boolean;
    FText: TCaption;
    function DoIsMouseWheelHandleNeeded(Shift: TShiftState; MousePos: TPoint): Boolean;
    function GetCursorPos: Integer;
    function GetEditingText: TCaption;
    function GetInnerTextEdit: IcxInnerTextEdit;
    function GetLookupData: TcxCustomTextEditLookupData;
    function GetProperties: TcxCustomTextEditProperties;
    function GetActiveProperties: TcxCustomTextEditProperties;
    function GetSelLength: Integer;
    function GetSelStart: Integer;
    function GetSelText: TCaption;
    function GetViewInfo: TcxCustomTextEditViewInfo;
    procedure SetFindSelection(Value: Boolean);
    procedure SetItemObject(Value: TObject);
    procedure SetProperties(Value: TcxCustomTextEditProperties);
    procedure SetSelLength(Value: Integer);
    procedure SetSelStart(Value: Integer);
    function GetTextHint: string;
    procedure SetTextHint(Value: string);
    procedure WMClear(var Message: TMessage); message WM_CLEAR;
    procedure WMCommand(var Message: TWMCommand); message WM_COMMAND;
    procedure WMGetText(var Message: TWMGetText); message WM_GETTEXT;
    procedure WMGetTextLength(var Message: TWMGetTextLength); message WM_GETTEXTLENGTH;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMSetText(var Message: TWMSetText); message WM_SETTEXT;
  protected
    FInnerEditPositionAdjusting: Boolean;
    FIsPopupWindowJustClosed: Boolean;
    FLookupData: TcxInterfacedPersistent;
    FLookupDataTextChangedLocked: Boolean;
    FIsChangeBySpellChecker: Boolean;

    // TWinControl
    procedure DoExit; override;
    procedure KeyPress(var Key: Char); override;
    procedure Loaded; override;
    procedure WndProc(var Message: TMessage); override;

    procedure AdjustInnerEditPosition; override;
    function CanKeyDownModifyEdit(Key: Word; Shift: TShiftState): Boolean; override;
    function CanKeyPressModifyEdit(Key: Char): Boolean; override;
    procedure ChangeHandler(Sender: TObject); override;
    procedure ContainerStyleChanged(Sender: TObject); override;
    procedure DoEditKeyDown(var Key: Word; Shift: TShiftState); override;
    procedure DoEditKeyPress(var Key: Char); override;
    procedure DoEditValueChanged; override;
    procedure DoFocusChanged; override;
    function DoRefreshContainer(const P: TPoint; Button: TcxMouseButton; Shift: TShiftState;
      AIsMouseEvent: Boolean): Boolean; override;
    procedure DoSetFocusWhenActivate; override;
    procedure DoValidateDisplayValue(var ADisplayValue: TcxEditValue; var AErrorText: TCaption;
      var AError: Boolean); override;
    function PopulateFromList(var AFindText: string): Boolean; virtual;
    procedure PopulateSizeProperties(var AEditSizeProperties: TcxEditSizeProperties); override;
    function GetDisplayText: string; override;
    function GetInnerControlBounds(const AInnerControlsRegion: TRect;
      AInnerControl: TControl): TcxContainerInnerControlBounds; override;
    function GetInnerEditClass: TControlClass; override;
    procedure Initialize; override;
    function InternalDoEditing: Boolean; override;
    function InternalGetEditingValue: TcxEditValue; override;
    procedure InternalSetDisplayValue(const Value: TcxEditValue); override;
    procedure InternalValidateDisplayValue(const ADisplayValue: TcxEditValue); override;
    function IsLookupNavigationKey(AKey: Word; AShift: TShiftState): Boolean; virtual;
    function IsTextInputMode: Boolean; virtual;
    function IsValidChar(AChar: Char): Boolean; override;
    procedure PropertiesChanged(Sender: TObject); override;
    function SetDisplayText(const Value: string): Boolean; override;
    procedure SetInternalDisplayValue(Value: TcxEditValue); override;
    function WantNavigationKeys: Boolean; override;
    procedure LockedInnerEditWindowProc(var Message: TMessage); override;
    procedure UnlockInnerEditRepainting; override;

    // IcxFormatControllerListener
    procedure FormatChanged; virtual;

    // SpellChecking
    function CanSpellCheckerPostEditValue: Boolean; virtual;
    procedure DoDrawMisspellings; virtual;
    procedure DoLayoutChanged; virtual;
    procedure DoSelectionChanged; virtual;
    procedure DoSpellCheckerPostEditValue; virtual;
    procedure DoTextChanged; virtual;
    procedure InternalCheckSelection;
    procedure InternalSpellCheckerHandler(var Message: TMessage); virtual;
    procedure RedrawMisspelledWords;
    procedure SpellCheckerPostEditValue;
    procedure SpellCheckerSetSelText(const AValue: string; APost: Boolean = False); override;

    // Lookup
    function GetILookupData: IcxTextEditLookupData; virtual;
    function GetScrollLookupDataList(AScrollCause: TcxEditScrollCause): Boolean; virtual;
    procedure DoOnNewLookupDisplayText(const AText: string); virtual;
    function ItemIndexToLookupKey(AItemIndex: Integer): TcxEditValue; virtual;
    procedure LockLookupDataTextChanged;
    function LookupKeyToEditValue(const AKey: TcxEditValue): TcxEditValue; virtual;
    function LookupKeyToItemIndex(const AKey: TcxEditValue): Integer; virtual;
    procedure UnlockLookupDataTextChanged;

    procedure AdjustInnerEdit; virtual;
    function CanChangeSelText(const Value: string; out ANewText: string;
      out ANewSelStart: Integer): Boolean; virtual;
    function CanSelectItem(AFindSelection: Boolean): Boolean; virtual;
    procedure CheckEditValue; virtual;
    procedure CheckEditorValueBounds; virtual;
    function GetInnerEditHeight: Integer; virtual;
    function GetItemIndex: Integer; virtual;
    function GetItemObject: TObject; virtual;
    procedure HandleSelectItem(Sender: TObject); virtual;
    function InternalGetText: string; virtual;
    function InternalMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; override;
    function InternalSetText(const Value: string): Boolean; virtual;
    function IsMouseWheelHandleNeeded(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; override;
    function NeedsInvokeAfterKeyDown(AKey: Word; AShift: TShiftState): Boolean; override;
    function NeedResetInvalidTextWhenPropertiesChanged: Boolean; virtual;
    procedure ResetOnNewDisplayValue; virtual;
    procedure SelChange(Sender: TObject); virtual;
    procedure SetEditingText(const Value: TCaption); virtual;
    procedure SetItemIndex(Value: Integer); virtual;
    procedure SetSelText(const Value: TCaption); virtual;
    procedure SynchronizeDisplayValue; override;
    procedure SynchronizeEditValue; override;
    procedure UndoPerformed; virtual;
    procedure UpdateDrawValue; override;
    procedure UpdateDisplayValue; virtual;

    property BeepOnEnter: Boolean read FBeepOnEnter write FBeepOnEnter default True;
    property InnerTextEdit: IcxInnerTextEdit read GetInnerTextEdit;
    property ItemIndex: Integer read GetItemIndex write SetItemIndex stored False;
    property ItemObject: TObject read GetItemObject write SetItemObject;
    property LookupData: TcxCustomTextEditLookupData read GetLookupData;
    property LookupItemsScrolling: Boolean read FLookupItemsScrolling write FLookupItemsScrolling;
    property ParentColor default False;
    property ViewInfo: TcxCustomTextEditViewInfo read GetViewInfo;
    property TextHint: string read GetTextHint write SetTextHint;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DefaultHandler(var Message); override;
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    procedure CopyToClipboard; override;
    procedure CutToClipboard; override;
    function IsEditClass: Boolean; override;
    procedure PasteFromClipboard; override;
    procedure PrepareEditValue(const ADisplayValue: TcxEditValue;
      out EditValue: TcxEditValue; AEditFocused: Boolean); override;
    procedure SelectAll; override;
    function GetTextBaseLine: Integer; override;
    function HasTextBaseLine: Boolean; override;
    procedure ClearSelection; virtual;
    function DoInnerControlDefaultHandler(var Message: TMessage): Boolean; override;
    function IsChildWindow(AWnd: THandle): Boolean; override;
    procedure SetScrollBarsParameters(AIsScrolling: Boolean = False); override;
    procedure SetSelection(ASelStart: Integer; ASelLength: Integer);
    procedure Undo; virtual;
    property ActiveProperties: TcxCustomTextEditProperties read GetActiveProperties;
    property CursorPos: Integer read GetCursorPos;
    property EditingText: TCaption read GetEditingText write SetEditingText;
    property FindSelection: Boolean read FFindSelection write SetFindSelection;
    property ILookupData: IcxTextEditLookupData read GetILookupData;
    property Properties: TcxCustomTextEditProperties read GetProperties write SetProperties;
    property SelLength: Integer read GetSelLength write SetSelLength;
    property SelStart: Integer read GetSelStart write SetSelStart;
    property SelText: TCaption read GetSelText write SetSelText;
    property Text;
  published
    property ImeMode stored False; // deprecated hidden
    property ImeName stored False; // deprecated hidden
  end;

  { TcxTextEdit }

  TcxTextEdit = class(TcxCustomTextEdit)
  private
    function GetActiveProperties: TcxTextEditProperties;
    function GetProperties: TcxTextEditProperties;
    procedure SetProperties(Value: TcxTextEditProperties);
  protected
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
    property Text;
    property TextHint;
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

  { TcxFilterTextEditHelper }

  TcxFilterTextEditHelper = class(TcxCustomFilterEditHelper)
  public
    class function GetFilterEditClass: TcxCustomEditClass; override;
    class procedure InitializeProperties(AProperties,
      AEditProperties: TcxCustomEditProperties; AHasButtons: Boolean); override;
    class procedure SetFilterValue(AEdit: TcxCustomEdit; AEditProperties: TcxCustomEditProperties;
      AValue: Variant); override;
  end;

procedure CheckCharsRegister(var AText: string; ACharCase: TEditCharCase);
function CheckTextEditState(ATextEdit: IcxInnerTextEdit; const APrevState:
  TcxCustomInnerTextEditPrevState): Boolean;
procedure DrawEditText(ACanvas: TcxCanvas; AViewInfo: TcxCustomTextEditViewInfo; ADrawPasswordCharAsBullet: Boolean = False);
procedure DrawTextEdit(ACanvas: TcxCanvas; AViewInfo: TcxCustomTextEditViewInfo);
function GetTextEditContentSize(ACanvas: TcxCanvas; AViewData: TcxCustomEditViewData;
  const AText: string;  ADrawTextFlags: DWORD; const AEditSizeProperties: TcxEditSizeProperties;
  ALineCount: Integer = 0; ACorrectWidth: Boolean = True): TSize;
function GetTextEditDrawTextOffset(AViewData: TcxCustomEditViewData): TRect; overload;
function GetTextEditDrawTextOffset(AAlignment: TAlignment; AIsInplace: Boolean): TRect; overload; // deprecated
procedure InternalTextOut(ACanvas: TCanvas; AViewInfo: TcxCustomTextEditViewInfo;
  AText: PcxCaptionChar; var R: TRect; AFormat: TcxTextOutFormat; ASelStart,
  ASelLength: Integer; ASelBackgroundColor, ASelTextColor: TColor;
  AMaxLineCount: Integer = 0; ALeftIndent: Integer = 0; ARightIndent: Integer = 0);
procedure InsertThousandSeparator(var S: string);
function RemoveExponentialPart(var S: string): string;
procedure RemoveThousandSeparator(var S: string);
procedure SaveTextEditState(ATextEdit: IcxInnerTextEdit;
  ASaveText: Boolean;
  var APrevState: TcxCustomInnerTextEditPrevState);
procedure SeparateDigitGroups(AEdit: TcxCustomTextEdit);
function StrToFloatEx(S: string; var AValue: Double): Boolean;

implementation

uses
  Themes, FMTBcd, Types, Math, dxCharacters,
  cxEditConsts, cxEditUtils, dxThemeConsts, dxThemeManager, cxDWMApi, dxIncrementalFiltering;

const
  LookupNavigationKeys = dxIncrementalFilteringNavigationKeys;

type
  TCanvasAccess = class(TCanvas);
  TControlAccess = class(TControl);
  //TcxEditDataBindingAccess = class(TcxEditDataBinding)

procedure CalculateTextEditViewInfo(ACanvas: TcxCanvas; AViewData: TcxCustomTextEditViewData;
  AViewInfo: TcxCustomTextEditViewInfo; AIsMouseEvent: Boolean);
var
  AMeasureTextHeight: Integer;
begin
  with AViewInfo do
  begin
    TextRect := cxRectContent(ClientRect, AViewData.GetDrawTextOffset);
    AMeasureTextHeight := cxTextHeight(ACanvas.Handle);
    if not IsInplace and (cxRectHeight(TextRect) < AMeasureTextHeight) then
      TextRect.Bottom := Min(ClientRect.Bottom, AMeasureTextHeight + TextRect.Top);
  end;
end;

procedure CheckCharsRegister(var AText: string; ACharCase: TEditCharCase);
begin
  if ACharCase = ecUpperCase then
    AText := AnsiUpperCase(AText)
  else
    if ACharCase = ecLowerCase then
      AText := AnsiLowerCase(AText);
end;

function CheckTextEditState(ATextEdit: IcxInnerTextEdit; const APrevState:
  TcxCustomInnerTextEditPrevState): Boolean;
begin
  if not ATextEdit.Control.HandleAllocated then
    Result := True
  else
    with APrevState do
      Result := (PrevSelStart <> ATextEdit.SelStart) or
        (PrevSelLength <> ATextEdit.SelLength) or
        IsPrevTextSaved and not InternalCompareString(PrevText, ATextEdit.EditValue, False);
end;

procedure DrawEditText(ACanvas: TcxCanvas; AViewInfo: TcxCustomTextEditViewInfo; ADrawPasswordCharAsBullet: Boolean = False);
var
  ATextColor: TColor;
  R: TRect;
begin
  with AViewInfo do
  begin
    if Length(Text) = 0 then
      Exit;

    R := TextRect;
    if DrawSelectionBar then
      ATextColor := clHighlightText
    else
      ATextColor := TextColor;
    ACanvas.Font := Font;
    ACanvas.Font.Color := ATextColor;
    PrepareCanvasFont(ACanvas.Canvas);
    if ADrawPasswordCharAsBullet then
      InflateRect(R, 0, 1);
    InternalTextOut(ACanvas.Canvas, AViewInfo, PcxCaptionChar(Text), R, DrawTextFlags,
      SelStart, SelLength, SelBackgroundColor, SelTextColor, MaxLineCount);
  end;
end;

procedure DrawTextEdit(ACanvas: TcxCanvas; AViewInfo: TcxCustomTextEditViewInfo);

  procedure InternalDrawFocusRect(R: TRect);
  begin
    if not AViewInfo.IsEditClass and AViewInfo.Focused and not AViewInfo.IsInplace and not AViewInfo.HasPopupWindow then
    begin
      if AViewInfo.DrawSelectionBar then
      begin
        ACanvas.Font.Color := clHighlightText;
        ACanvas.Brush.Color := clHighlight;
      end else
      begin
        ACanvas.Font.Color := clBtnText;
        ACanvas.Brush.Color := AViewInfo.BackgroundColor;
      end;
      TCanvasAccess(ACanvas.Canvas).RequiredState([csFontValid]);
      ACanvas.Canvas.DrawFocusRect(R);
    end;
  end;

var
  ARealTransparent: Boolean;
  R: TRect;
begin
  ARealTransparent := AViewInfo.Transparent or AViewInfo.DrawSelectionBar;
  AViewInfo.DrawCustomEdit(ACanvas, not ARealTransparent, True);
  R := AViewInfo.ClientRect;
  if not AViewInfo.IsInplace then
  begin
    if ((AViewInfo.NativeState = TC_NONE) or AViewInfo.DrawSelectionBar) and not AViewInfo.Transparent and
      (AViewInfo.EditingStyle <> esNoEdit) then
        ACanvas.FrameRect(R, AViewInfo.BackgroundColor);
    InflateRect(R, -1, -1);
  end;
  if AViewInfo.IsOwnerDrawing then
    AViewInfo.CustomDrawHandler(ACanvas, R)
  else
  begin
    if AViewInfo.DrawSelectionBar then
    begin
      ACanvas.Brush.Color := clHighlight;
      ACanvas.FillRect(R);
    end;
    if AViewInfo.IsDBEditPaintCopyDrawing or not AViewInfo.HasInnerEdit or not AViewInfo.IsEditClass then
      AViewInfo.DrawText(ACanvas);
  end;
  InternalDrawFocusRect(R);
end;

function GetTextEditContentSize(ACanvas: TcxCanvas; AViewData: TcxCustomEditViewData;
  const AText: string; ADrawTextFlags: DWORD; const AEditSizeProperties: TcxEditSizeProperties;
  ALineCount: Integer = 0; ACorrectWidth: Boolean = True): TSize;

  function GetAutoHeightSize: TSize;
  var
    AAlignment: TcxEditAlignment;
    AFlags: DWORD;
    ARowCount: Integer;
    ATextParams: TcxTextParams;
    ATextRows: TcxTextRows;
    ASizeCorrection: TSize;
    AWidth: Integer;
    R: TRect;
  begin
    AAlignment := nil;
    if not AViewData.IsInplace and (AViewData is TcxCustomTextEditViewData) then
      with TcxCustomTextEditProperties(AViewData.Properties) do
        if (EditingStyle in [esFixedList, esNoEdit]) and
          not (not AViewData.IsInplace and TcxCustomTextEditViewData(AViewData).IsComboBoxStyle and
            AreVisualStylesMustBeUsed(AViewData.Style.LookAndFeel.NativeStyle, totEdit)) then
              AAlignment := Alignment;
    ACanvas.Font := AViewData.Style.GetVisibleFont;
    AFlags := ADrawTextFlags;
    ASizeCorrection := AViewData.GetEditContentSizeCorrection;
    AWidth := AEditSizeProperties.Width;

    if ALineCount > 0 then
    begin
      Result.cy := cxTextHeight(ACanvas.Handle) * ALineCount;
      Result.cx := AEditSizeProperties.Width;
    end
    else
    begin
      if AViewData.IsInplace then
        Dec(AWidth, 2);
      if AAlignment <> nil then
        Dec(AWidth);
      if AWidth <= 0 then
      begin
        Result.cx := 0;
        if (epoAllowZeroHeight in AViewData.PaintOptions) and (Length(AText) = 0) then
          Result.cy := 0
        else
          Result.cy := cxTextHeight(ACanvas.Handle);
      end
      else
      begin
        Result.cx := AEditSizeProperties.Width;
        if Length(AText) = 0 then
          if epoAllowZeroHeight in AViewData.PaintOptions then
            Result.cy := 0
          else
            Result.cy := cxTextHeight(ACanvas.Handle)
        else
        begin
          AFlags := AFlags or CXTO_CALCROWCOUNT;
          if AFlags and CXTO_SINGLELINE <> 0 then
            AFlags := AFlags and not CXTO_SINGLELINE or CXTO_WORDBREAK or
              CXTO_EDITCONTROL;
          AFlags := AFlags and not(CXTO_CENTER_VERTICALLY or CXTO_BOTTOM) or CXTO_TOP;
          R := Rect(0, 0, AWidth, cxMaxRectSize);
          ATextParams := cxCalcTextParams(ACanvas.Canvas, AFlags);
          cxMakeTextRows(ACanvas.Canvas, PChar(AText), Length(AText), R, ATextParams,
            ATextRows, ARowCount, AEditSizeProperties.MaxLineCount);
          Result.cy := ARowCount * cxTextHeight(ACanvas.Handle);
          cxResetTextRows(ATextRows);
        end;
      end;
    end;
    if Result.cy > 0 then
      Result.cy := Result.cy + ASizeCorrection.cy;
  end;

  function GetBestFitSize: TSize;
  var
    AAlignment: TcxEditAlignment;
    AFlags: DWORD;
    ARowCount: Integer;
    ATextParams: TcxTextParams;
    ATextRows: TcxTextRows;
    ATextFlags: Integer;
    ASizeCorrection: TSize;
    R: TRect;
  begin
    AAlignment := nil;
    if not AViewData.IsInplace and (AViewData is TcxCustomTextEditViewData) then
      with TcxCustomTextEditProperties(AViewData.Properties) do
        if (EditingStyle in [esFixedList, esNoEdit]) and
          not (not AViewData.IsInplace and TcxCustomTextEditViewData(AViewData).IsComboBoxStyle and
            AreVisualStylesMustBeUsed(AViewData.Style.LookAndFeel.NativeStyle, totEdit)) then
              AAlignment := Alignment;
    ACanvas.Font := AViewData.Style.GetVisibleFont;
    AFlags := ADrawTextFlags;
    ASizeCorrection := AViewData.GetEditContentSizeCorrection;

    if (AFlags and CXTO_SINGLELINE <> 0) and not ((epoAutoHeight in AViewData.PaintOptions) and
      (esoAutoHeight in AViewData.Properties.GetSupportedOperations)) then
    begin
      ATextFlags := cxTextOutFlagsToDrawTextFlags(AFlags);
      Result.cy := cxTextHeight(ACanvas.Handle);
      R := Rect(0, 0, cxMaxRectSize, 0);
      if Length(AText) = 0 then
        Result.cx := 0
      else
      begin
        ACanvas.TextExtent(AText, R, ATextFlags);
        Result.cx := R.Right - R.Left;
      end;
    end
    else
    begin
      if AFlags and CXTO_SINGLELINE <> 0 then
        AFlags := AFlags and not CXTO_SINGLELINE or
          CXTO_WORDBREAK or CXTO_EDITCONTROL;
      AFlags := AFlags or CXTO_CALCRECT;
      R := Rect(0, 0, cxMaxRectSize, cxMaxRectSize);

      ARowCount := ALineCount;
      if ARowCount = 0 then
        ARowCount := AEditSizeProperties.MaxLineCount;
      ATextParams := cxCalcTextParams(ACanvas.Canvas, AFlags);
      cxMakeTextRows(ACanvas.Canvas, PChar(AText), Length(AText), R, ATextParams, ATextRows, ARowCount, ARowCount);
      Result.cx := cxGetLongestTextRowWidth(ATextRows, ARowCount);
      cxResetTextRows(ATextRows);
      Result.cy := cxTextHeight(ACanvas.Handle);
      if ALineCount > 0 then
        Result.cy := Result.cy * ALineCount;
    end;
    if AAlignment <> nil then
      Result.cx := Result.cx + 1;
    if ACorrectWidth then
      Result.cx := Result.cx + ASizeCorrection.cx;
    if Result.cy > 0 then
      Result.cy := Result.cy + ASizeCorrection.cy;
  end;

begin
  if AEditSizeProperties.Width >= 0 then
    Result := GetAutoHeightSize
  else
    Result := GetBestFitSize;
end;

function GetTextEditDrawTextOffset(AViewData: TcxCustomEditViewData): TRect;
begin
  Result := AViewData.EditContentParams.Offsets;
  if not AViewData.IsInplace and (AViewData is TcxCustomTextEditViewData) then
    if TcxCustomTextEditProperties(AViewData.Properties).EditingStyle in [esFixedList, esNoEdit] then
    begin
      Inc(Result.Top);
      Dec(Result.Bottom);
      if AViewData.HorzAlignment = taRightJustify then
        Inc(Result.Right)
      else
        Inc(Result.Left);
    end;
end;

function GetTextEditDrawTextOffset(AAlignment: TAlignment; AIsInplace: Boolean): TRect; // deprecated
begin
  Result := EditContentDefaultOffsets[AIsInplace];
end;

procedure PrepareTextRows(ACanvas: TCanvas; var TextOutData: TcxTextOutData;
  AText: PcxCaptionChar; var R: TRect; AFormat: TcxTextOutFormat; ASelStart,
  ASelLength: Integer; ASelBackgroundColor, ASelTextColor: TColor;
  AMaxLineCount: Integer = 0; ALeftIndent: Integer = 0; ARightIndent: Integer = 0);

  procedure InternalPrepareTextRows;
  var
    ATextLength: Integer;
  begin
    TextOutData.RowCount := 0;
    cxResetTextRows(TextOutData.TextRows);
    ATextLength := StrLen(AText);
    if ATextLength = 0 then
      Exit;
    TextOutData.TextParams := cxCalcTextParams(ACanvas.Handle, AFormat);
    TextOutData.TextRect := cxPrepareRect(R, TextOutData.TextParams, ALeftIndent,
      ARightIndent);

    if not IsRectEmpty(TextOutData.TextRect) then
    begin
      TextOutData.ForceEndEllipsis := not cxMakeTextRows(ACanvas.Handle, AText,
        ATextLength, TextOutData.TextRect, TextOutData.TextParams,
        TextOutData.TextRows, TextOutData.RowCount, AMaxLineCount);
      if TextOutData.RowCount <> 0 then
      begin
        cxPlaceTextRows(ACanvas.Handle, TextOutData.TextRect, TextOutData.TextParams, TextOutData.TextRows, TextOutData.RowCount);
        if (ASelStart < 0) or (ASelStart >= ATextLength) then
          ASelLength := 0
        else
          if (ASelLength + ASelStart) > ATextLength then
            ASelLength := ATextLength - ASelStart;

        TextOutData.SelStart := ASelStart;
        TextOutData.SelLength := ASelLength;
        TextOutData.SelBackgroundColor := ASelBackgroundColor;
        TextOutData.SelTextColor := ASelTextColor;
      end;
    end;
  end;

begin
  TextOutData.Initialized := True;
  InternalPrepareTextRows;
end;

procedure InternalTextOut(ACanvas: TCanvas; AViewInfo: TcxCustomTextEditViewInfo;
  AText: PcxCaptionChar; var R: TRect; AFormat: TcxTextOutFormat; ASelStart,
  ASelLength: Integer; ASelBackgroundColor, ASelTextColor: TColor;
  AMaxLineCount: Integer = 0; ALeftIndent: Integer = 0; ARightIndent: Integer = 0);
begin
  if not AViewInfo.TextOutData.Initialized then
    PrepareTextRows(ACanvas, AViewInfo.TextOutData, AText, R, AFormat, ASelStart,
    ASelLength, ASelBackgroundColor, ASelTextColor, AMaxLineCount, ALeftIndent,
    ARightIndent);
  TCanvasAccess(ACanvas).RequiredState([csFontValid]);
  with AViewInfo.TextOutData do
    cxTextRowsOutHighlight(ACanvas.Handle, TextRect, TextParams, TextRows,
    RowCount, SelStart, SelLength, SelBackgroundColor, SelTextColor, ForceEndEllipsis);
end;

procedure PrepareTextEditDrawTextFlags(ACanvas: TcxCanvas; AViewData: TcxCustomTextEditViewData;
  AViewInfo: TcxCustomTextEditViewInfo);
var
  ADrawTextOffset: TRect;
  AFlags: DWORD;
  R: TRect;
  ATextHeight: Integer;
  ATextParams: TcxTextParams;
  ATextRows: TcxTextRows;
  ARowCount, AMaxLineCount: Integer;
begin
  if AViewData.Style.GetVisibleFont = nil then
    Exit;
  ACanvas.Font := AViewData.Style.GetVisibleFont;
  with AViewInfo do
  begin
    DrawTextFlags := AViewData.GetDrawTextFlags;
    AFlags := DrawTextFlags and not CXTO_SINGLELINE or
      CXTO_WORDBREAK or CXTO_EDITCONTROL or CXTO_CALCROWCOUNT;
    R := Rect(0, 0, TextRect.Right - TextRect.Left, cxMaxRectSize);
    ADrawTextOffset := AViewData.GetDrawTextOffset;
    Inc(R.Right, ADrawTextOffset.Left + ADrawTextOffset.Right);
    Dec(R.Right, AViewData.GetEditContentSizeCorrection.cx);
    Dec(R.Right, AViewData.ContentOffset.Left + AViewData.ContentOffset.Right);
    if TcxCustomTextEditProperties(AViewData.Properties).Alignment.Horz = taRightJustify then
      Inc(R.Right);
    ATextParams := cxCalcTextParams(ACanvas.Canvas, AFlags);
    AMaxLineCount := (TextRect.Bottom - TextRect.Top + ATextParams.RowHeight - 1) div ATextParams.RowHeight + 1;
    cxMakeTextRows(ACanvas.Canvas, PChar(Text), Length(Text), R, ATextParams, ATextRows, ARowCount, AMaxLineCount);
    ATextHeight := ARowCount * ATextParams.RowHeight;
    if ARowCount > 1 then
    begin
      DrawTextFlags := DrawTextFlags and not CXTO_SINGLELINE or
        CXTO_WORDBREAK or CXTO_EDITCONTROL;
      if ATextHeight > TextRect.Bottom - TextRect.Top then
      begin
        DrawTextFlags := DrawTextFlags and not CXTO_BOTTOM or CXTO_TOP;
        if (AViewData.MaxLineCount = 0) or (ARowCount <= AViewData.MaxLineCount) then
          DrawTextFlags := DrawTextFlags and not CXTO_CENTER_VERTICALLY;
      end;
    end;
    cxResetTextRows(ATextRows);
  end;
end;

procedure InsertThousandSeparator(var S: string);
var
  I, J: Integer;
  ACaption: string;
  APrefix, ASuffix: string;
begin
  APrefix := TrimRight(S);
  ASuffix := Copy(S, Length(APrefix) + 1, Length(S) - Length(APrefix));
  APrefix := TrimLeft(S);
  APrefix := Copy(S, 1, Length(S) - Length(APrefix));
  S := Trim(S);
  ACaption := RemoveExponentialPart(S);
  RemoveThousandSeparator(S);
  I := Pos(dxFormatSettings.DecimalSeparator, S);
  if I = 0 then
    I := Length(S)
  else
    Dec(I);
  J := 0;
  while (I > 1) and cxIsDigitChar(S[I - 1]) do
  begin
    Inc(J);
    if J = 3 then
    begin
      Insert(dxFormatSettings.ThousandSeparator, S, I);
      J := 0;
    end;
    Dec(I);
  end;
  S := APrefix + S + ACaption + ASuffix;
end;

function RemoveExponentialPart(var S: string): string;
var
  APos: Integer;
begin
  APos := Pos('E', UpperCase(S));
  if APos > 0 then
  begin
    Result := Copy(S, APos, Length(S) - APos + 1);
    Delete(S, APos, Length(S) - APos + 1);
  end
  else
    Result := '';
end;

procedure RemoveThousandSeparator(var S: string);
var
  APos: Integer;
begin
  repeat
    APos := Pos(dxFormatSettings.ThousandSeparator, S);
    if APos <> 0 then
      Delete(S, APos, 1);
  until APos = 0;
end;

procedure SaveTextEditState(ATextEdit: IcxInnerTextEdit;
  ASaveText: Boolean;
  var APrevState: TcxCustomInnerTextEditPrevState);
begin
  with APrevState do
  begin
    IsPrevTextSaved := ASaveText;
    if ASaveText then
      PrevText := ATextEdit.EditValue;
    PrevSelStart := ATextEdit.SelStart;
    PrevSelLength := ATextEdit.SelLength;
  end;
end;

procedure SeparateDigitGroups(AEdit: TcxCustomTextEdit);

  function IsValidNumber(S: string): Boolean;
  var
    AValue: Extended;
  begin
    RemoveThousandSeparator(S);
    Result := (S <> '') and TextToFloat(PChar(S), AValue, fvExtended);
  end;

  function GetRealCaretPos: Integer;
  var
    I: Integer;
  begin
    Result := 0;
    for I := 1 to AEdit.SelStart do
      if AEdit.Text[I] <> dxFormatSettings.ThousandSeparator then
        Inc(Result);
  end;

  procedure SetRealCaretPos(APrevCaretPos: Integer);
  var
    I: Integer;
    S: string;
  begin
    S := AEdit.Text;
    for I := 1 to Length(S) do
    begin
      if S[I] <> dxFormatSettings.ThousandSeparator then
        Dec(APrevCaretPos);
      if APrevCaretPos = 0 then
      begin
        AEdit.SelStart := I;
        Break;
      end;
    end;
  end;

var
  ACaretPos: Integer;
  S: string;
begin
  S := AEdit.Text;
  InsertThousandSeparator(S);
  if IsValidNumber(S) then
  begin
    ACaretPos := GetRealCaretPos;
    AEdit.SetInternalDisplayValue(S);
    SetRealCaretPos(ACaretPos);
  end;
end;

function StrToFloatEx(S: string; var AValue: Double): Boolean;

  function InternalTextToFloat(const S: string; var AResultValue: Extended): Boolean;
  begin
  {$IFDEF DELPHI103RIO}
    {$IFDEF WIN64}
      if (S = dxFormatSettings.DecimalSeparator) or (S = '-' + dxFormatSettings.DecimalSeparator) then
      begin
        AResultValue := 0;
        Result := True;
      end
      else
        Result := TextToFloat(S, AResultValue);
    {$ELSE}
      Result := TextToFloat(S, AResultValue);
    {$ENDIF}
  {$ELSE}
    Result := TextToFloat(PChar(S), AResultValue, fvExtended);
  {$ENDIF}
  end;

var
  E: Extended;
  I: Integer;
begin
  // Ignore Thousand Separators
  for I := Length(S) downto 1 do
    if S[I] = dxFormatSettings.ThousandSeparator then
      Delete(S, I, 1);
  if not InternalTextToFloat(S, E) or
    ((E <> 0) and ((Abs(E) < MinDouble) or (Abs(E) > MaxDouble))) then
  begin
    AValue := 0;
    Result := S = '';
  end
  else
    begin
      AValue := E;
      Result := True;
    end;
end;

function ShowPasswordCharAsBullet(ANativeStyle: Boolean): Boolean; inline;
begin
  Result := ANativeStyle and IsWinSevenOrLater and IsThemeActive;
end;

function GetWindowsPasswordChar(ANativeStyle: Boolean; APasswordChar: TCaptionChar): TCaptionChar;
begin
  if APasswordChar = #0 then
  begin
    if ShowPasswordCharAsBullet(ANativeStyle) then
      Result := TdxCharacters.PasswordBullet
    else
      Result := '*'
  end
  else
    Result := APasswordChar;
end;

{ TcxCustomInnerTextEditHelper }

constructor TcxCustomInnerTextEditHelper.Create(AEdit: TcxCustomInnerTextEdit);
begin
  inherited Create(nil);
  FEdit := AEdit;
end;

// IcxContainerInnerControl
function TcxCustomInnerTextEditHelper.GetControlContainer: TcxContainer;
begin
  Result := Edit.Container;
end;

function TcxCustomInnerTextEditHelper.GetControl: TWinControl;
begin
  Result := Edit;
end;

// IcxCustomInnerEdit
function TcxCustomInnerTextEditHelper.CallDefWndProc(AMsg: UINT; WParam: WPARAM;
  LParam: LPARAM): LRESULT;
begin
  Result := CallWindowProc(Edit.DefWndProc, Edit.Handle, AMsg, WParam, LParam);
end;

function TcxCustomInnerTextEditHelper.CanProcessClipboardMessages: Boolean;
begin
  Result := False;
end;

function TcxCustomInnerTextEditHelper.GetEditValue: TcxEditValue;
begin
  Result := Edit.Text;
end;

function TcxCustomInnerTextEditHelper.GetOnChange: TNotifyEvent;
begin
  Result := Edit.OnChange;
end;

procedure TcxCustomInnerTextEditHelper.LockBounds(ALock: Boolean);
begin
  if ALock then
    Inc(Edit.FLockBoundsCount)
  else
  begin
    dxTestCheck(Edit.FLockBoundsCount > 0, 'TcxCustomInnerTextEditHelper.LockBounds fails');
    Dec(Edit.FLockBoundsCount);
  end;
end;

procedure TcxCustomInnerTextEditHelper.SafelySetFocus;
var
  APrevAutoSelect: Boolean;
begin
  APrevAutoSelect := Edit.AutoSelect;
  Edit.AutoSelect := False;
  Edit.SetFocus;
  Edit.AutoSelect := APrevAutoSelect;
end;

procedure TcxCustomInnerTextEditHelper.SetEditValue(const Value: TcxEditValue);

  procedure InternalSetValue(const Value: TcxEditValue; ANeedStoreSelection: Boolean);
  var
    AStoreSelStart: Integer;
    AStoreSelLength: Integer;
    S: string;
  begin
    AStoreSelStart := 0;
    AStoreSelLength := 0;
    if ANeedStoreSelection then
    begin
      AStoreSelStart := Edit.SelStart;
      AStoreSelLength := Edit.SelLength;
    end;
    S := VarToStr(Value);
    CheckCharsRegister(S, Edit.CharCase);
    Edit.Text := S;
    if ANeedStoreSelection then
    begin
      Edit.SelStart := AStoreSelStart;
      Edit.SelLength := AStoreSelLength;
    end;
  end;

var
  ATextChanged: Boolean;
  ANeedStoreSelection: Boolean;
begin
  with Edit do
  begin
    ATextChanged := IsDesigning and not InternalCompareString(Text, VarToStr(Value), True);
    ANeedStoreSelection := not IsDesigning and HandleAllocated and Container.IsEditValidating and
      Container.ActiveProperties.UseLookupData and Container.FindSelection and
      InternalCompareString(Text, VarToStr(Value), False) and not InternalCompareString(Text, VarToStr(Value), True);
    InternalSetValue(Value, ANeedStoreSelection);
    if ATextChanged then
      Change;
  end;
end;

procedure TcxCustomInnerTextEditHelper.SetParent(Value: TWinControl);
begin
  Edit.Parent := Value;
end;

procedure TcxCustomInnerTextEditHelper.SetOnChange(Value: TNotifyEvent);
begin
  Edit.OnChange := Value;
end;

// IcxInnerTextEdit
procedure TcxCustomInnerTextEditHelper.ClearSelection;
begin
  Edit.ClearSelection;
end;

procedure TcxCustomInnerTextEditHelper.CopyToClipboard;
begin
  Edit.CopyToClipboard;
end;

function TcxCustomInnerTextEditHelper.GetAlignment: TAlignment;
begin
  Result := Edit.FAlignment;
end;

function TcxCustomInnerTextEditHelper.GetAutoSelect: Boolean;
begin
  Result := Edit.AutoSelect;
end;

function TcxCustomInnerTextEditHelper.GetCharCase: TEditCharCase;
begin
  Result := Edit.CharCase;
end;

function TcxCustomInnerTextEditHelper.GetEchoMode: TcxEditEchoMode;
begin
  Result := Edit.EchoMode;
end;

function TcxCustomInnerTextEditHelper.GetFirstVisibleCharIndex: Integer;
begin
  Result := LoWord(SendMessage(Edit.Handle, EM_CHARFROMPOS, 0, 0));
end;

function TcxCustomInnerTextEditHelper.GetHideSelection: Boolean;
begin
  Result := Edit.HideSelection;
end;

function TcxCustomInnerTextEditHelper.GetImeLastChar: Char;
begin
  if Edit.FImeCharCount > 1 then
    Result := Edit.FImeLastChar
  else
    Result := #0;
end;

function TcxCustomInnerTextEditHelper.GetImeMode: TImeMode;
begin
  Result := Edit.ImeMode;
end;

function TcxCustomInnerTextEditHelper.GetImeName: TImeName;
begin
  Result := Edit.ImeName;
end;

function TcxCustomInnerTextEditHelper.GetInternalUpdating: Boolean;
begin
  Result := Edit.FInternalUpdating;
end;

function TcxCustomInnerTextEditHelper.GetIsInplace: Boolean;
begin
  Result := Edit.Container.IsInplace;
end;

function TcxCustomInnerTextEditHelper.GetMaxLength: Integer;
begin
  Result := Edit.MaxLength;
end;

function TcxCustomInnerTextEditHelper.GetMultiLine: Boolean;
begin
  Result := False;
end;

function TcxCustomInnerTextEditHelper.GetPasswordChar: TCaptionChar;
begin
  Result := Edit.FPasswordChar;
end;

function TcxCustomInnerTextEditHelper.GetOEMConvert: Boolean;
begin
  Result := Edit.OEMConvert;
end;

function TcxCustomInnerTextEditHelper.GetOnSelChange: TNotifyEvent;
begin
  Result := Edit.OnSelChange;
end;

function TcxCustomInnerTextEditHelper.GetReadOnly: Boolean;
begin
  Result := Edit.ReadOnly;
end;

function TcxCustomInnerTextEditHelper.GetSelLength: Integer;
begin
  if Edit.FImeCharCount > 0 then
    Result := FSelLength
  else
    Result := Edit.SelLength;
end;

function TcxCustomInnerTextEditHelper.GetSelStart: Integer;
begin
  if Edit.FImeCharCount > 0 then
    Result := FSelStart
  else
    Result := Edit.SelStart;
end;

function TcxCustomInnerTextEditHelper.GetSelText: string;
begin
  Result := Edit.SelText;
end;

function TcxCustomInnerTextEditHelper.GetUseLeftAlignmentOnEditing: Boolean;
begin
  Result := FEdit.UseLeftAlignmentOnEditing;
end;

procedure TcxCustomInnerTextEditHelper.SelectAll;
begin
  if Edit.HandleAllocated then
    Edit.SelectAll;
end;

procedure TcxCustomInnerTextEditHelper.SetAlignment(Value: TAlignment);
begin
  Edit.Alignment := Value;
end;

procedure TcxCustomInnerTextEditHelper.SetAutoSelect(Value: Boolean);
begin
  Edit.AutoSelect := Value;
end;

procedure TcxCustomInnerTextEditHelper.SetCharCase(Value: TEditCharCase);
begin
  Edit.CharCase := Value;
end;

procedure TcxCustomInnerTextEditHelper.SetEchoMode(Value: TcxEditEchoMode);
begin
  Edit.EchoMode := Value;
end;

procedure TcxCustomInnerTextEditHelper.SetHideSelection(Value: Boolean);
begin
  Edit.HideSelection := Value;
end;

procedure TcxCustomInnerTextEditHelper.SetInternalUpdating(Value: Boolean);
begin
  Edit.FInternalUpdating := Value;
end;

procedure TcxCustomInnerTextEditHelper.SetImeMode(Value: TImeMode);
begin
  Edit.ImeMode := Value;
end;

procedure TcxCustomInnerTextEditHelper.SetImeName(const Value: TImeName);
begin
  Edit.ImeName := Value;
end;

procedure TcxCustomInnerTextEditHelper.SetMaxLength(Value: Integer);
begin
  Edit.MaxLength := Value;
end;

procedure TcxCustomInnerTextEditHelper.SetOEMConvert(Value: Boolean);
begin
  Edit.OEMConvert := Value;
end;

procedure TcxCustomInnerTextEditHelper.SetOnSelChange(Value: TNotifyEvent);
begin
  Edit.OnSelChange := Value;
end;

procedure TcxCustomInnerTextEditHelper.SetPasswordChar(Value: TCaptionChar);
begin
  if Edit.FPasswordChar <> Value then
  begin
    Edit.FPasswordChar := Value;
    if Edit.EchoMode = eemPassword then
      cxRecreateControlWnd(Edit);
  end;
end;

procedure TcxCustomInnerTextEditHelper.SetReadOnly(Value: Boolean);
begin
  Edit.ReadOnly := Value;
end;

procedure TcxCustomInnerTextEditHelper.SetSelLength(Value: Integer);
begin
  if Edit.HandleAllocated then
    if Edit.FImeCharCount > 0 then
      FSelLength := Value
    else
      Edit.SelLength := Value;
end;

procedure TcxCustomInnerTextEditHelper.SetSelStart(Value: Integer);
begin
  if Edit.HandleAllocated then
    if Edit.FImeCharCount > 0 then
      FSelStart := Value
    else
      Edit.SelStart := Value;
end;

procedure TcxCustomInnerTextEditHelper.SetSelText(Value: string);
begin
  Edit.SelText := Value;
end;

procedure TcxCustomInnerTextEditHelper.SetUseLeftAlignmentOnEditing(Value: Boolean);
begin
  Edit.UseLeftAlignmentOnEditing := Value;
end;

function TcxCustomInnerTextEditHelper.GetTextHint: string;
begin
  Result := Edit.TextHint;
end;

procedure TcxCustomInnerTextEditHelper.SetTextHint(Value: string);
begin
  Edit.TextHint := Value;
end;

{ TcxCustomInnerTextEdit }

constructor TcxCustomInnerTextEdit.Create(AOwner: TComponent);
begin
  FIsCreating := True;
  inherited Create(AOwner);
  FHelper := CreateHelper;
  ControlStyle := ControlStyle + [csDoubleClicks];
  ParentColor := True;
  ParentFont := False;
  FInternalUpdating := False;
  FEchoMode := eemNormal;
  FIsCreating := False;
  FUseLeftAlignmentOnEditing := True;
end;

destructor TcxCustomInnerTextEdit.Destroy;
begin
  FreeAndNil(FHelper);
  inherited Destroy;
end;

procedure TcxCustomInnerTextEdit.DragDrop(Source: TObject; X, Y: Integer);
begin
  Container.DragDrop(Source, Left + X, Top + Y);
end;

function TcxCustomInnerTextEdit.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or
    Container.DataBinding.ExecuteAction(Action);
end;

function TcxCustomInnerTextEdit.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or
    Container.DataBinding.UpdateAction(Action);
end;

function TcxCustomInnerTextEdit.CanFocus: Boolean;
begin
  Result := Container.CanFocus;
end;

procedure TcxCustomInnerTextEdit.DefaultHandler(var Message);
begin
  if not Container.InnerControlDefaultHandler(TMessage(Message)) then
    inherited DefaultHandler(Message);
end;

function TcxCustomInnerTextEdit.GetControlsAlignment: TAlignment;
begin
  Result := GetAlignment;
end;

procedure TcxCustomInnerTextEdit.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  if not FIsCreating and (FLockBoundsCount = 0) then
  begin
    Container.LockAlignControls(True);
    try
      inherited SetBounds(ALeft, ATop, AWidth, AHeight);
    finally
      Container.LockAlignControls(False);
    end;
  end;
end;

// IcxContainerInnerControl
function TcxCustomInnerTextEdit.GetControl: TWinControl;
begin
  Result := Self;
end;

function TcxCustomInnerTextEdit.GetControlContainer: TcxContainer;
begin
  Result := Container;
end;

// IcxInnerEditHelper
function TcxCustomInnerTextEdit.GetHelper: IcxCustomInnerEdit;
begin
  Result := Helper;
end;

// for Delphi .NET
procedure TcxCustomInnerTextEdit.Change;
begin
  inherited Change;
end;

procedure TcxCustomInnerTextEdit.Click;
begin
  inherited Click;
  Container.Click;
end;

procedure TcxCustomInnerTextEdit.CreateHandle;
var
  APasswordChar: Char;
begin
  if UseRightToLeftAlignment and (Width = 0) then
    Width := Container.Width;
  Container.ClearSavedChildControlRegions;
  inherited CreateHandle;
  if (Container.ActiveProperties.EchoMode = eemPassword) then
  begin
    APasswordChar := Container.ActiveProperties.PasswordChar;
    if (not (cxIsVCLThemesEnabled and Container.IsNativeStyle) or
        ((GetBasedAlignment <> taLeftJustify) and Container.ActiveProperties.UseLeftAlignmentOnEditing)) and
        (APasswordChar = #0) then
      APasswordChar := '*';
    if APasswordChar <> #0 then
      SendMessage(Handle, EM_SETPASSWORDCHAR, Ord(APasswordChar), 0);
  end;
end;

function TcxCustomInnerTextEdit.CreateHelper: TcxCustomInnerTextEditHelper;
begin
  Result := TcxCustomInnerTextEditHelper.Create(Self);
end;

procedure TcxCustomInnerTextEdit.CreateParams(var Params: TCreateParams);
const
  AAlignmentMap: array[TAlignment] of DWORD = (ES_LEFT, ES_RIGHT, ES_CENTER);
  PasswordMap: array[Boolean] of DWORD = (0, ES_PASSWORD);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style and not (ES_RIGHT or ES_CENTER);
    Style := Style or AAlignmentMap[GetRealAlignment];
    Style := Style and (not WS_BORDER);
    Style := Style and (not WS_DLGFRAME);
    Style := Style and (not WS_SIZEBOX);
    Style := Style and (not WS_THICKFRAME);
    Style := Style or ES_AUTOHSCROLL;
    Style := Style or PasswordMap[Container.ActiveProperties.EchoMode = eemPassword];
    ExStyle := ExStyle and (not WS_EX_CLIENTEDGE);
  end;
end;

procedure TcxCustomInnerTextEdit.CreateWindowHandle(const Params: TCreateParams);
var
  AParams: TCreateParams;
begin
  AParams := Params;
  AParams.Caption := '';
  inherited CreateWindowHandle(AParams);
  if HandleAllocated then
    CallWindowProc(DefWndProc, Handle, WM_SETTEXT, 0, LPARAM(WindowText));
end;

procedure TcxCustomInnerTextEdit.CreateWnd;
begin
  inherited CreateWnd;
  AdjustMargins;
end;

procedure TcxCustomInnerTextEdit.DblClick;
begin
  inherited DblClick;
  Container.DblClick;
end;

procedure TcxCustomInnerTextEdit.DestroyWnd;
begin
{$IFDEF VCLGLASSPAINT}
  dxForceProcessBufferedPaintMessages(Self);
{$ELSE}
  FRepaintOnGlass := False;
{$ENDIF}
  inherited DestroyWnd;
end;

function TcxCustomInnerTextEdit.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean;
begin
  Result := False;
end;

procedure TcxCustomInnerTextEdit.DragOver(Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  Container.DragOver(Source, Left + X, Top + Y, State, Accept);
end;

function TcxCustomInnerTextEdit.GetBasedAlignment: TAlignment;
begin
  Result := Container.ActiveProperties.Alignment.Horz;
end;

procedure TcxCustomInnerTextEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  FInternalUpdating := False;
  try
    Container.KeyDown(Key, Shift);
  finally
    if Key = 0 then
      FInternalUpdating := True;
  end;
  if Key <> 0 then
    inherited KeyDown(Key, Shift);
end;

procedure TcxCustomInnerTextEdit.KeyPress(var Key: Char);
var
  AKey: Word;
begin
  FInternalUpdating := False;
  try
    AKey := Word(Key);
    if (AKey = VK_TAB) then
      Key := #0;
    Container.KeyPress(Key);

    AKey := Word(Key);
    if (Container.IsInplace or Container.FIsPopupWindowJustClosed) and (AKey in [VK_RETURN, VK_ESCAPE]) or
       (not Container.BeepOnEnter and (AKey in [VK_RETURN, 10{Ctrl+Enter}])) then
    begin
      Key := #0;
      Container.FIsPopupWindowJustClosed := False;
    end;
  finally
    if Key = #0 then
      FInternalUpdating := True
  end;
  if Key <> #0 then
    inherited KeyPress(Key);
end;

procedure TcxCustomInnerTextEdit.KeyUp(var Key: Word; Shift: TShiftState);
begin
  FInternalUpdating := False;
  try
    if (Key = VK_TAB) then
      Key := 0;
    Container.KeyUp(Key, Shift);
  finally
    if Key = 0 then
      FInternalUpdating := True;
  end;
  if Key <> 0 then
    inherited KeyUp(Key, Shift);
end;

procedure TcxCustomInnerTextEdit.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  Container.InnerControlMouseDown := True;
  try
    Container.MouseDown(Button, Shift, X + Left, Y + Top);
  finally
    Container.InnerControlMouseDown := False;
  end;
end;

procedure TcxCustomInnerTextEdit.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  Container.MouseMove(Shift, X + Left, Y + Top);
end;

procedure TcxCustomInnerTextEdit.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  Container.MouseUp(Button, Shift, X + Left, Y + Top);
end;

procedure TcxCustomInnerTextEdit.WndProc(var Message: TMessage);
begin
  if Container.InnerControlMenuHandler(Message) then
    Exit;
  case Message.Msg of
    WM_LBUTTONDOWN:
      if (Container.DragMode = dmAutomatic) and not Container.IsDesigning then
        Container.BeginAutoDrag
      else
        inherited WndProc(Message);
    WM_LBUTTONDBLCLK:
      if (Container.DragMode = dmAutomatic) and not Container.IsDesigning then
        Container.BeginAutoDrag
      else
        inherited WndProc(Message);
  {$IFNDEF VCLGLASSPAINT}
    WM_PAINT:
      begin
        if Container.OnGlass and IsCompositionEnabled then
        begin
          dxPaintWindowOnGlass(Handle);
          Message.Result := 0;
        end
        else
          inherited WndProc(Message);
      end;
    CN_CTLCOLOREDIT, CN_CTLCOLORSTATIC:
      begin
        inherited WndProc(Message);
        if not FRepaintOnGlass and Container.OnGlass and IsCompositionEnabled then
        begin
          FRepaintOnGlass := True;
          PostMessage(Handle, DXM_BUFFEREDPAINTONGLASS, 0, 0);
        end;
      end;
    DXM_BUFFEREDPAINTONGLASS:
      if FRepaintOnGlass then
      begin
        dxDrawWindowOnGlass(Handle);
        FRepaintOnGlass := False;
      end;
  {$ENDIF}
  else
    inherited WndProc(Message);
  end;
end;

procedure TcxCustomInnerTextEdit.AdjustMargins;
begin
  SendMessage(Handle, EM_SETMARGINS, EC_LEFTMARGIN + EC_RIGHTMARGIN, 1 shl 16);
end;

procedure TcxCustomInnerTextEdit.MouseEnter(AControl: TControl);
begin
end;

procedure TcxCustomInnerTextEdit.MouseLeave(AControl: TControl);
begin
  Container.ShortRefreshContainer(True);
end;

// for Delphi .NET
procedure TcxCustomInnerTextEdit.RecreateWnd;
begin
  inherited RecreateWnd;
end;

function TcxCustomInnerTextEdit.GetAlignment: TAlignment;
begin
  if UseLeftAlignmentOnEditing and not IsDesigning then
    Result := taLeftJustify
  else
    Result := GetBasedAlignment;
end;

function TcxCustomInnerTextEdit.GetContainer: TcxCustomTextEdit;
begin
  Result := TcxCustomTextEdit(Owner);
end;

function TcxCustomInnerTextEdit.GetCursorPos: Integer;
var
  X: Integer;
  P: TPoint;
  I, I0, I1: Smallint;
  ATextLength: Integer;
begin
  ATextLength := Length(Text);
  GetCaretPos(P);
  if Smallint(SendMessage(Handle, EM_POSFROMCHAR, ATextLength - 1, 0) and $FFFF) < P.X then
    Result := ATextLength
  else
  begin
    I0 := 0;
    I1 := ATextLength - 1;
    repeat
      I := (I0 + I1) div 2;
      X := Smallint(SendMessage(Handle, EM_POSFROMCHAR, I, 0) and $FFFF);
      if X < P.X then
        I0 := I
      else
        I1 := I;
    until I1 - I0 < 2;
    if SendMessage(Handle, EM_POSFROMCHAR, I0, 0) and $FFFF = P.X then
      Result := I0
    else
      Result := I1;
  end;
end;

function TcxCustomInnerTextEdit.GetIsDestroying: Boolean;
begin
  Result := csDestroying in ComponentState;
end;

function TcxCustomInnerTextEdit.GetRealAlignment: TAlignment;
begin
  Result := GetAlignment;
  if UseRightToLeftAlignment then
    ChangeBiDiModeAlignment(Result);
end;

function TcxCustomInnerTextEdit.IsDesigning: Boolean;
begin
  Result := csDesigning in ComponentState;
end;

procedure TcxCustomInnerTextEdit.InternalPaint(ADC: THandle);
var
  R: TRect;
  S: string;
  ALeftMargin, ARightMargin: Word;
  AMargings: DWORD;
  APasswordChar: Char;
  ACanvas: TCanvas;
  ARealAlignment: TAlignment;
begin
  AMargings := Perform(EM_GETMARGINS, 0, 0);
  ARightMargin := HiWord(AMargings);
  ALeftMargin := LoWord(AMargings);

  if Container.ActiveProperties.EchoMode = eemPassword then
    APasswordChar := GetWindowsPasswordChar(Container.LookAndFeel.NativeStyle, FPasswordChar)
  else
    APasswordChar := #0;

  if APasswordChar <> #0 then
    S := StringOfChar(APasswordChar, Length(Text))
  else
    S := Text;

  R := ClientRect;
  ARealAlignment := GetBasedAlignment;
  if UseRightToLeftAlignment then
    ChangeBiDiModeAlignment(ARealAlignment);

  case ARealAlignment of
    taCenter:
      R.Left := (ClientWidth - ARightMargin - ALeftMargin - cxTextSize(Font, S).cx) div 2 + ALeftMargin;
    taRightJustify:
      R.Left := ClientWidth - cxTextSize(Font, S).cx - ARightMargin + ALeftMargin;
    taLeftJustify:
      R.Left := ALeftMargin;
  end;

  ACanvas := TCanvas.Create;
  try
    ACanvas.Handle := ADC;
    if UseRightToLeftReading then
      ACanvas.TextFlags := ACanvas.TextFlags or ETO_RTLREADING;
    try
      ACanvas.Font := Font;
      if (APasswordChar <> #0) and ShowPasswordCharAsBullet(Container.LookAndFeel.NativeStyle) then
        ACanvas.Font.Name := 'Segoe UI';
      ACanvas.Brush.Color := Color;
      ACanvas.FillRect(ClientRect);
      cxDrawText(ACanvas, S, R, 0);
    finally
      ACanvas.Handle := 0;
    end;
  finally
    ACanvas.Free;
  end;
end;

procedure TcxCustomInnerTextEdit.SetAlignment(Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    if not UseLeftAlignmentOnEditing then
      RecreateWnd;
  end;
end;

procedure TcxCustomInnerTextEdit.SetEchoMode(const Value: TcxEditEchoMode);
begin
  if FEchoMode <> Value then
  begin
    FEchoMode := Value;
    if HandleAllocated then
      RecreateWnd;
  end;
end;

procedure TcxCustomInnerTextEdit.SetUseLeftAlignmentOnEditing(Value: Boolean);
begin
  if FUseLeftAlignmentOnEditing <> Value then
  begin
    FUseLeftAlignmentOnEditing := Value;
    if Alignment <> taLeftJustify then
      RecreateWnd;
  end;
end;

procedure TcxCustomInnerTextEdit.WMChar(var Message: TWMChar);
var
  APrevState: TcxCustomInnerTextEditPrevState;
  ANeedSaveModified: Boolean;
begin
  ANeedSaveModified := Message.CharCode = 1;
  if ANeedSaveModified then
    Container.SaveModified;
  try
    SaveTextEditState(Helper, True, APrevState);
    FInternalUpdating := False;
    inherited;
    if FImeCharCount > 0 then
    begin
      Dec(FImeCharCount);
      if (FImeCharCount = 0) and Container.FindSelection then
      begin
        SelStart := Helper.FSelStart;
        SelLength := Helper.FSelLength;
      end;
    end;
    Container.UnlockLookupDataTextChanged;
  finally
    if ANeedSaveModified then
      Container.RestoreModified;
  end;
  if FInternalUpdating then
    Exit;
  if CheckTextEditState(Helper, APrevState) then
    Container.FindSelection := False;
end;

procedure TcxCustomInnerTextEdit.WMClear(var Message: TMessage);
begin
  if not ReadOnly then
  begin
    Container.BeginUserAction;
    try
      Container.ClearSelection;
    finally
      Container.EndUserAction;
    end;
  end;
end;

procedure TcxCustomInnerTextEdit.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.Result := 1;
end;

procedure TcxCustomInnerTextEdit.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  Message.Result := Message.Result or DLGC_WANTCHARS;
  if Container.TabsNeeded and (GetKeyState(VK_CONTROL) >= 0) then
    Message.Result := Message.Result or DLGC_WANTTAB;
  if Container.IsInplace or Container.HasPopupWindow or
    (Container.ModifiedAfterEnter and (GetKeyState(VK_ESCAPE) < 0)) then
    Message.Result := Message.Result or DLGC_WANTALLKEYS;
end;

procedure TcxCustomInnerTextEdit.WMIMEChar(var Message: TMessage);
begin
  if not IsWindowUnicode(Handle) then
  begin
    if FImeCharCount = 0 then
    begin
      Helper.FSelStart := SelStart;
      Helper.FSelLength := SelLength;
    end;
    if (Message.WParam and $FF00) shr 8 <> 0 then
    begin
      Inc(FImeCharCount, 2);
      FImeLastChar := Char(Message.WParam and $FF);
    end
    else
      Inc(FImeCharCount, 1);
  end;
  inherited;
end;

procedure TcxCustomInnerTextEdit.WMIMEComposition(var Message: TMessage);
begin
  if Container.DoEditing then
    inherited;
end;

procedure TcxCustomInnerTextEdit.WMKeyDown(var Message: TWMKeyDown);
var
  AKey: Word;
  APrevState: TcxCustomInnerTextEditPrevState;
  AShiftState: TShiftState;
begin
  AShiftState := KeyDataToShiftState(Message.KeyData);
  if Container.HasPopupWindow and Container.ActiveProperties.IsPopupKey(Message.CharCode, AShiftState) then
    with Container.ILookupData do
      if ActiveControl is TWinControl then
      begin
        SendMessage(TWinControl(ActiveControl).Handle, WM_KEYDOWN, TMessage(Message).WParam, TMessage(Message).LParam);
        if Message.Result = 0 then
          Exit;
      end;

  SaveTextEditState(Helper, True, APrevState);
  FInternalUpdating := False;
  inherited;
  if (Message.CharCode = 0) or FInternalUpdating then
    Exit;
  if not CheckTextEditState(Helper, APrevState) then
  begin
    AKey := Message.CharCode;
    AShiftState := KeyDataToShiftState(Message.KeyData);
    Container.DoAfterKeyDown(AKey, AShiftState);
    Message.CharCode := AKey;
  end;
end;

procedure TcxCustomInnerTextEdit.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;
  if not IsDestroying then
    Container.FocusChanged;
end;

procedure TcxCustomInnerTextEdit.WMNCPaint(var Message: TWMNCPaint);
begin
  Message.Result := 0;
end;

procedure TcxCustomInnerTextEdit.WMPaint(var Message: TWMPaint);
var
  DC: HDC;
  PS: TPaintStruct;
begin
  if UseLeftAlignmentOnEditing and (GetBasedAlignment <> taLeftJustify) and not Focused then
  begin
    DC := Message.DC;
    if DC = 0 then
      DC := BeginPaint(Handle, PS);
    InternalPaint(DC);
    if Message.DC = 0 then
      EndPaint(Handle, PS);
  end
  else
    inherited;
end;

procedure TcxCustomInnerTextEdit.WMPrintClient(var Message: TWMPrintClient);
begin
  if UseLeftAlignmentOnEditing and (GetBasedAlignment <> taLeftJustify) and not Focused then
    InternalPaint(Message.DC)
  else
    inherited;
end;

procedure TcxCustomInnerTextEdit.WMSetCursor(var Message: TWMSetCursor);
var
  P: TPoint;
  ACursor: TCursor;
begin
  P := Container.ScreenToClient(GetMouseCursorPos);
  ACursor := Container.GetCurrentCursor(P.X, P.Y);
  if ACursor <> crDefault then
    SetCursor(Screen.Cursors[ACursor])
  else
    inherited;
end;

procedure TcxCustomInnerTextEdit.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;
  if not IsDestroying and (Message.FocusedWnd <> Container.Handle) then
    Container.FocusChanged;
end;

procedure TcxCustomInnerTextEdit.WMSetFont(var Message: TWMSetFont);
begin
  inherited;
  AdjustMargins;
end;

procedure TcxCustomInnerTextEdit.WMSize(var Message: TWMSize);
begin
  inherited;
  AdjustMargins;
end;

procedure TcxCustomInnerTextEdit.WMUndo(var Message: TWMSize);
begin
  inherited;
  Container.UndoPerformed;
end;

procedure TcxCustomInnerTextEdit.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if Message.lParam = 0 then
    MouseEnter(Self)
  else
    MouseEnter(TControl(Message.lParam));
end;

procedure TcxCustomInnerTextEdit.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if Message.lParam = 0 then
    MouseLeave(Self)
  else
    MouseLeave(TControl(Message.lParam));
end;

procedure TcxCustomInnerTextEdit.EMReplaceSel(var Message: TMessage);
begin
  if (Container = nil) or Container.DoEditing then
    inherited;
end;

procedure TcxCustomInnerTextEdit.EMSetSel(var Message: TMessage);
begin
  inherited;
  if Assigned(OnSelChange) then
    OnSelChange(Self);
end;

{ TcxTextEditPropertiesValues }

procedure TcxTextEditPropertiesValues.Assign(Source: TPersistent);
begin
  if Source is TcxTextEditPropertiesValues then
  begin
    BeginUpdate;
    try
      inherited Assign(Source);
      with Source as TcxTextEditPropertiesValues do
      begin
        Self.DisplayFormat := DisplayFormat;
        Self.EditFormat := EditFormat;
        Self.MaxLength := MaxLength;
      end;
    finally
      EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;

procedure TcxTextEditPropertiesValues.RestoreDefaults;
begin
  BeginUpdate;
  try
    inherited RestoreDefaults;
    DisplayFormat := False;
    EditFormat := False;
    MaxLength := False;
  finally
    EndUpdate;
  end;
end;

function TcxTextEditPropertiesValues.IsDisplayFormatStored: Boolean;
begin
  Result := DisplayFormat and
    (TcxCustomTextEditProperties(Properties).FDisplayFormat = '') and
    IsPropertiesPropertyVisible('DisplayFormat');
end;

function TcxTextEditPropertiesValues.IsEditFormatStored: Boolean;
begin
  Result := EditFormat and
    (TcxCustomTextEditProperties(Properties).FEditFormat = '') and
    IsPropertiesPropertyVisible('EditFormat');
end;

procedure TcxTextEditPropertiesValues.SetDisplayFormat(Value: Boolean);
begin
  if Value <> FDisplayFormat then
  begin
    FDisplayFormat := Value;
    Changed;
  end;
end;

procedure TcxTextEditPropertiesValues.SetEditFormat(Value: Boolean);
begin
  if Value <> FEditFormat then
  begin
    FEditFormat := Value;
    Changed;
  end;
end;

procedure TcxTextEditPropertiesValues.SetMaxLength(Value: Boolean);
begin
  if Value <> FMaxLength then
  begin
    FMaxLength := Value;
    TcxCustomTextEditProperties(Properties).MaxLengthChanged;
  end;
end;

{ TcxCustomEditListBox }

constructor TcxCustomEditListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHotTrack := True;
  BorderStyle := bsNone;
  FPrevMousePos := cxInvalidPoint;
  FGestureHelper := TdxGestureHelper.Create(Self);
end;

destructor TcxCustomEditListBox.Destroy;
begin
  FreeAndNil(FGestureHelper);
  inherited;
end;

function TcxCustomEditListBox.CanFocus: Boolean;
begin
  Result := not Edit.ActiveProperties.UseSearchControl and
    inherited CanFocus;
end;

function TcxCustomEditListBox.GetHeight(ARowCount: Integer; AMaxHeight: Integer): Integer;
begin
  Result := ARowCount * GetItemHeight;
end;

function TcxCustomEditListBox.GetItemHeight(AIndex: Integer = -1): Integer;
begin
  Result := GetDefaultItemHeight;
end;

function TcxCustomEditListBox.GetItemWidth(AIndex: Integer): Integer;
begin
  Canvas.Font.Assign(Font);
  Result := Canvas.TextWidth(GetItem(AIndex));
end;

function TcxCustomEditListBox.IsVisible: Boolean;
begin
  Result := HandleAllocated and IsWindowVisible(Handle);
end;

procedure TcxCustomEditListBox.SetScrollWidth(Value: Integer);
begin
  ScrollWidth := 0;
  ScrollWidth := Value;
end;

procedure TcxCustomEditListBox.WndProc(var Message: TMessage);
begin
  if (FGestureHelper = nil) or not FGestureHelper.HandleMessage(Message) then
    inherited;
end;

procedure TcxCustomEditListBox.Click;
begin
  inherited Click;
  DoSelectItem;
end;

function TcxCustomEditListBox.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;

  procedure InternalMouseWheel(Shift: TShiftState; WheelDelta: Integer;
    MousePos: TPoint);
  const
    AScrollDirectionMap: array [Boolean] of Integer = (SB_LINEDOWN, SB_LINEUP);
  var
    I: Integer;
  begin
    for I := 0 to Mouse.WheelScrollLines - 1 do
      Result := (SendMessage(Handle, WM_VScroll, AScrollDirectionMap[WheelDelta > 0], 0) = 0);
  end;

begin
  Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
  if not Result then
  begin
    WheelDelta := EnsureRange(WheelDelta, -WHEEL_DELTA, WHEEL_DELTA);
    if FMouseWheelAccumulator * WheelDelta > 0 then
      Inc(FMouseWheelAccumulator, WheelDelta)
    else
      FMouseWheelAccumulator := WheelDelta;

    if Abs(FMouseWheelAccumulator) >= WHEEL_DELTA then
    begin
      InternalMouseWheel(Shift, FMouseWheelAccumulator, MousePos);
      FMouseWheelAccumulator := 0;
    end;
  end;
end;

procedure TcxCustomEditListBox.DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  AText: string;
begin
  if DoDrawItem(Index, Rect, State) then
    Exit;
  Canvas.FillRect(Rect);
  if (Index >= 0) and (Index < Items.Count) then
  begin
    if not UseRightToLeftAlignment then
      Inc(Rect.Left, 2)
    else
      Dec(Rect.Right, 2);
    AText := GetItem(Index);
    if IsHighlightSearchText then
      DrawItemText(AText, Rect)
    else
      DrawText(Canvas.Handle, PChar(AText), Length(AText), Rect, GetDrawTextFlags);
  end;
end;

procedure TcxCustomEditListBox.DrawItemText(const AText: string; const ARect: TRect);
var
  ASelStart: Integer;
  R: TRect;
begin
  R := ARect;
  ASelStart := Pos(AnsiUpperCase(Edit.LookupData.SearchText), AnsiUpperCase(AText)) - 1;
  cxTextOut(Canvas.Canvas, AText, R, GetDrawTextFlags, ASelStart, Length(Edit.LookupData.SearchText), Canvas.Font,
    clHighlight, clHighlightText, 0, 0, 0, Canvas.Font.Color);
end;

function TcxCustomEditListBox.GetDrawTextFlags: Cardinal;
const
  AAlignment: array [Boolean] of Cardinal = (CXTO_LEFT, CXTO_RIGHT);
begin
  if IsHighlightSearchText then
    Result := AAlignment[UseRightToLeftAlignment] or CXTO_CENTER_VERTICALLY or CXTO_SINGLELINE or CXTO_END_ELLIPSIS
  else
    Result := DrawTextBiDiModeFlags(DT_SINGLELINE or DT_VCENTER or DT_NOPREFIX);
end;

function TcxCustomEditListBox.GetItemData(Index: Integer):
  {$IFDEF DELPHI16}TListBoxItemData{$ELSE}Longint{$ENDIF};
begin
  Result := 0;
end;

function TcxCustomEditListBox.IsHighlightSearchText: Boolean;
begin
  Result := Edit.ActiveProperties.IncrementalFiltering and
    (ifoHighlightSearchText in Edit.ActiveProperties.IncrementalFilteringOptions);
end;

function TcxCustomEditListBox.NeedDrawFocusRect: Boolean;
begin
  Result := (Edit.ActiveProperties.EditingStyle in [esFixedList, esNoEdit]) and
    not (Edit.ActiveProperties.IncrementalFiltering);
end;

procedure TcxCustomEditListBox.MeasureItem(Index: Integer; var Height: Integer);
begin
  Height := GetItemHeight;
end;

procedure TcxCustomEditListBox.MouseLeave(AControl: TControl);
begin
  inherited;
  FPrevMousePos := cxInvalidPoint;
end;

procedure TcxCustomEditListBox.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  AItemIndex: Integer;
begin
  inherited MouseMove(Shift, X, Y);
  if (GetCaptureControl <> Self) and HotTrack and not cxPointIsEqual(FPrevMousePos, Point(X, Y)) then
  begin
    AItemIndex := ItemAtPos(Point(X, Y), False);
    if (AItemIndex <> -1) and (ItemIndex <> AItemIndex) then
      ItemIndex := AItemIndex;
  end;
  FPrevMousePos := Point(X, Y);
end;

procedure TcxCustomEditListBox.SetItemData(Index: Integer;
  AData: {$IFDEF DELPHI16}TListBoxItemData{$ELSE}Longint{$ENDIF});
begin
end;

function TcxCustomEditListBox.DoDrawItem(AIndex: Integer; const ARect: TRect;
  AState: TOwnerDrawState): Boolean;
begin
  Result := False;
end;

procedure TcxCustomEditListBox.DoSelectItem;
begin
  SetExternalScrollBarsParameters;
  if Assigned(FOnSelectItem) then
    FOnSelectItem(Self);
end;

function TcxCustomEditListBox.GetDefaultItemHeight: Integer;
begin
  Canvas.Font := Font;
  Result := cxTextHeight(Canvas.Handle);
  dxAdjustToTouchableSize(Result, ScaleFactor);
end;

function TcxCustomEditListBox.GetItem(Index: Integer): string;
begin
  Result := Items[Index];
end;

procedure TcxCustomEditListBox.InternalRecreateWindow;
begin
  RecreateWnd;
end;

procedure TcxCustomEditListBox.RecreateWindow;
begin
end;

procedure TcxCustomEditListBox.SetItemCount(Value: Integer);
var
  I: Integer;
begin
  if Value = Items.Count then
    Exit;

  Items.BeginUpdate;
  try
    if Value > Items.Count then
      for I := Items.Count to Value - 1 do
        Items.Add(GetItem(I))
    else
      for I := 1 to Items.Count - Value do
        Items.Delete(Items.Count - 1);
  finally
    Items.EndUpdate;
  end;
end;

procedure TcxCustomEditListBox.SetItemIndex(const Value: Integer);
begin
  inherited SetItemIndex(Value);
  DoSelectItem;
end;

// IdxGestureClient
function TcxCustomEditListBox.AllowGesture(AGestureId: Integer): Boolean;
begin
  Result := (GetInteractiveGestureByGestureID(AGestureId) in Touch.InteractiveGestures);
end;

function TcxCustomEditListBox.AllowPan(AScrollKind: TScrollBarKind): Boolean;
begin
  Result := (AScrollKind = sbVertical) and Container.IsScrollBarActive(sbVertical) and Container.VScrollBar.Enabled or
    (AScrollKind = sbHorizontal) and Container.IsScrollBarActive(sbHorizontal) and Container.HScrollBar.Enabled;
end;

procedure TcxCustomEditListBox.BeginGestureScroll(APos: TPoint);
begin
  FGestureAccumulatedDelta := cxNullPoint;
  Container.ShowTouchScrollUI(Container);
end;

procedure TcxCustomEditListBox.EndGestureScroll;
begin
  Container.HideTouchScrollUI(Container);
end;

procedure TcxCustomEditListBox.GestureScroll(ADeltaX, ADeltaY: Integer);
var
  ANewPosition, AScrollBarPosition: Integer;
  ADelta: Integer;
begin
  if Container.IsScrollBarActive(sbVertical) and Container.VScrollBar.Enabled then
  begin
    Inc(FGestureAccumulatedDelta.Y, ADeltaY);
    ADelta := FGestureAccumulatedDelta.Y div GetItemHeight;
    if ADelta <> 0 then
    begin
      ANewPosition := Container.VScrollBar.Position - ADelta;
      FGestureAccumulatedDelta.Y := 0;
      DoScroll(sbVertical, scPosition, TopIndex - ADelta);
      FGestureHelper.CheckOverpan(Container.VScrollBar.Kind, ANewPosition,
        0, Container.VScrollBar.Max - Container.VScrollBar.PageSize + 1, ADeltaX, ADeltaY);
    end;
  end;
  if Container.IsScrollBarActive(sbHorizontal) and Container.HScrollBar.Enabled then
  begin
    ANewPosition := Container.HScrollBar.Position - ADeltaX;
    AScrollBarPosition := EnsureRange(ANewPosition, 0, Container.HScrollBar.Max - Container.HScrollBar.PageSize + 1);
    DoScroll(sbHorizontal, scPosition, AScrollBarPosition);
    SetExternalScrollBarsParameters;
    FGestureHelper.CheckOverpan(Container.HScrollBar.Kind, ANewPosition,
      0, Container.HScrollBar.Max - Container.HScrollBar.PageSize + 1, ADeltaX, ADeltaY);
  end;
end;

function TcxCustomEditListBox.GetPanOptions: Integer;
begin
   Result := GetPanOptionsByInteractiveGestureOptions(Touch.InteractiveGestureOptions);
end;

function TcxCustomEditListBox.GetScaleFactor: TdxScaleFactor;
begin
  Result := Edit.ScaleFactor;
end;

function TcxCustomEditListBox.IsPanArea(const APoint: TPoint): Boolean;
begin
  Result := PtInRect(ClientRect, APoint);
end;

function TcxCustomEditListBox.NeedPanningFeedback(AScrollKind: TScrollBarKind): Boolean;
begin
  Result := True;
end;

function TcxCustomEditListBox.GetGestureClient(const APoint: TPoint): IdxGestureClient;
begin
  Result := Self;
end;

function TcxCustomEditListBox.GetGestureClientHandle: THandle;
begin
  Result := Handle;
end;

function TcxCustomEditListBox.IsGestureTarget(AWnd: THandle): Boolean;
begin
  Result := AWnd = Handle;
end;

function TcxCustomEditListBox.GetContainer: TcxEditListBoxContainer;
begin
  Result := FContainer as TcxEditListBoxContainer;
end;

procedure TcxCustomEditListBox.DoGesture(const EventInfo: TGestureEventInfo; var Handled: Boolean);
begin
  FGestureHelper.DoGesture(EventInfo, Handled);
end;

procedure TcxCustomEditListBox.DoGetGestureOptions(var Gestures: TInteractiveGestures;
  var Options: TInteractiveGestureOptions);
begin
  inherited;
  FGestureHelper.CheckGestureOptions(Gestures, Options);
end;

procedure TcxCustomEditListBox.CMShowingChanged(var Message: TMessage);
begin
  inherited;
  CallWindowProc(DefWndProc, Handle, WM_SETFOCUS, 0, 0);
end;

function TcxCustomEditListBox.GetEdit: TcxCustomTextEdit;
begin
  Result := (Container.Owner as TcxCustomPopupWindow).OwnerControl as TcxCustomTextEdit;
end;

{ TcxEditListBoxContainer }

constructor TcxEditListBoxContainer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 121;
  Height := 97;
end;

function TcxEditListBoxContainer.AllowTouchScrollUIMode: Boolean;
begin
  Result := not IsDesigning;
end;

function TcxEditListBoxContainer.CanFocus: Boolean;
begin
  Result := InnerControl.CanFocus and
    inherited CanFocus;
end;

procedure TcxEditListBoxContainer.DoScrollUIModeChanged;
begin
  if InnerControl <> nil then
    (InnerControl as TcxCustomEditListBox).RecreateWnd;
end;

function TcxEditListBoxContainer.GetBorderExtent: TRect;
begin
  Result := cxEmptyRect
end;

function TcxEditListBoxContainer.IsMouseWheelHandleNeeded(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean;
begin
  Result := False;
end;

function TcxEditListBoxContainer.NeedsScrollBars: Boolean;
begin
  Result := True;
end;

procedure TcxEditListBoxContainer.Scroll(AScrollBarKind: TScrollBarKind; AScrollCode: TScrollCode; var AScrollPos: Integer);
begin
  inherited Scroll(AScrollBarKind, AScrollCode, AScrollPos);
  if not Enabled then
    Exit;
  (InnerControl as TcxCustomEditListBox).DoScroll(AScrollBarKind, AScrollCode, AScrollPos);
end;

procedure TcxEditListBoxContainer.WMWindowPosChanged(var Message: TWMWindowPosChanged);
begin
  inherited;
  if not IsDestroying and not FInternalWindowRegionSetting then
    SetScrollBarsParameters;
end;

{ TcxCustomTextEditLookupData }

constructor TcxCustomTextEditLookupData.Create(AOwner: TPersistent);
begin
  inherited Create(nil);
  FOwner:= AOwner;
  FCurrentKey := -1;
  FItemIndex := -1;
  FFilteredLookupItems := TList<Integer>.Create;
end;

destructor TcxCustomTextEditLookupData.Destroy;
begin
  FreeAndNil(FFilteredLookupItems);
  inherited;
end;

function TcxCustomTextEditLookupData.CanResizeVisualArea(var NewSize: TSize;
  AMaxHeight: Integer = 0; AMaxWidth: Integer = 0): Boolean;
begin
  Result := True;
end;

procedure TcxCustomTextEditLookupData.CloseUp;
begin
  if not Edit.EditModeSetting then
    InternalSetItemIndex(FCurrentKey);
  ResetIncrementalFilter;
  if FListContainer <> nil then
    FListContainer.HideTouchScrollUI(FListContainer, True);
end;

procedure TcxCustomTextEditLookupData.Deinitialize;
begin
end;

procedure TcxCustomTextEditLookupData.DropDown;
begin
end;

procedure TcxCustomTextEditLookupData.DroppedDown(const AFindStr: string);
begin
end;

function TcxCustomTextEditLookupData.Find(const AText: string): Boolean;
begin
  Result := IndexOf(AText) <> -1;
end;

function TcxCustomTextEditLookupData.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TcxCustomTextEditLookupData.DoCurrentKeyChanged;
begin
  if Assigned(FOnCurrentKeyChanged) then
    FOnCurrentKeyChanged(Self);
end;

procedure TcxCustomTextEditLookupData.DoInitialize(AVisualControlsParent: TWinControl);
var
  AFirstInitialize: Boolean;
begin
  AFirstInitialize := FList = nil;
  if AFirstInitialize then
  begin
    FListContainer := TcxEditListBoxContainer.Create(AVisualControlsParent);
    FList := GetListBoxClass.Create(FListContainer);
    FList.FContainer := FListContainer;
    FListContainer.InnerControl := FList;
    FList.Parent := FListContainer;
  end;
  FListContainer.Parent := AVisualControlsParent;
  FListContainer.Style := Edit.Style;
  FListContainer.Style.AssignedValues := FListContainer.Style.AssignedValues + [csvFont];
  FList.Canvas.Font := FList.Font;
  FList.OnSelectItem := nil;
  if AFirstInitialize then
    FList.RecreateWindow;
  FList.SetItemCount(GetFilteredItemCount);
  FList.OnSelectItem := HandleSelectItem;
  FList.ItemIndex := GetFilteredItemIndex(FItemIndex);
  if not AFirstInitialize then
    FList.RecreateWindow;
  FList.Invalidate;
  TextChanged;
end;

procedure TcxCustomTextEditLookupData.DoPositionVisualArea(const ARect: TRect);
begin
  FListContainer.BoundsRect := ARect;
  if FList.HandleAllocated then
    FList.SetScrollWidth(FList.ScrollWidth);
end;

procedure TcxCustomTextEditLookupData.DoSelectItem;
begin
  if Assigned(FOnSelectItem) then
    FOnSelectItem(Self);
end;

function TcxCustomTextEditLookupData.GetItem(Index: Integer): string;
begin
  Result := ActiveProperties.FLookupItems[Index];
end;

function TcxCustomTextEditLookupData.GetItemCount: Integer;
begin
  Result := ActiveProperties.FLookupItems.Count;
end;

function TcxCustomTextEditLookupData.GetDisplayText(const AKey: TcxEditValue): string;
begin
  if (AKey < 0) or (AKey >= GetItemCount) then
    Result := ''
  else
    Result := GetItem(AKey);
end;

function TcxCustomTextEditLookupData.GetListBoxClass: TcxCustomEditListBoxClass;
begin
  Result := nil;
end;

function TcxCustomTextEditLookupData.GetSelectedItem: Integer;
begin
  if FList = nil then
    Result := -1
  else
    Result := GetLookupItemIndexFromFilteredItemIndex(FList.ItemIndex);
end;

procedure TcxCustomTextEditLookupData.HandleSelectItem(Sender: TObject);
begin
  if FList.ItemIndex <> -1 then
    FItemIndex := GetLookupItemIndexFromFilteredItemIndex(FList.ItemIndex)
  else
    FItemIndex := -1;
end;

function TcxCustomTextEditLookupData.InternalLocate(var AText, ATail: string;
  ANext, ASynchronizeWithText: Boolean): Boolean;

  procedure CheckCurrentKey;
  var
    ACorrectCurrentKey: Boolean;
  begin
    case ActiveProperties.EditingStyle of
      esEditList:
        ACorrectCurrentKey := ASynchronizeWithText or (AText = '');
      esFixedList:
        ACorrectCurrentKey := ASynchronizeWithText;
      else
        ACorrectCurrentKey := True;
    end;
    if ACorrectCurrentKey then
      InternalSetCurrentKey(-1);
  end;

var
  AItem: string;
  AIStart, AItemIndex, L: Integer;
begin
  Result := False;
  if GetItemCount = 0 then
  begin
    CheckCurrentKey;
    Exit;
  end;

  if ASynchronizeWithText and ActiveProperties.MRUMode then
  begin
    AItemIndex := IndexOf(AText);
    if AItemIndex = -1 then
      InternalSetCurrentKey(-1)
    else
    begin
      Result := True;
      ATail := '';
      InternalSetCurrentKey(AItemIndex);
    end;
    Exit;
  end;
  if ANext then
    AIStart := ItemIndex
  else
    AIStart := -1;

  AItemIndex := -1;
  if NeedLocateItemWithFullString then
    AItemIndex := FindItemByText(AText, AIStart + 1, True);

  if AItemIndex = -1 then
    AItemIndex := FindItemByText(AText, AIStart + 1, False);

  Result := AItemIndex <> -1;
  if Result then
  begin
    if not (IsLikeTypeFiltering and IsFilterActive) then
    begin
      L := Length(AText);
      AItem := GetItem(AItemIndex);
      if ActiveProperties.EditingStyle <> esEdit then
        AText := Copy(AItem, 1, L);
      ATail := Copy(AItem, L + 1, Length(AItem) - L);
    end;
    InternalSetCurrentKey(AItemIndex);
  end
  else
    CheckCurrentKey;
end;

function TcxCustomTextEditLookupData.GetVisualAreaPreferredSize(AMaxHeight: Integer;
  AWidth: Integer = 0): TSize;
begin
  Result := cxNullSize;
end;

procedure TcxCustomTextEditLookupData.Go(ADirection: TcxEditLookupDataGoDirection;
  ACircular: Boolean);
var
  ANewCurrentKey: Integer;
  AItemIndex: Integer;
  AIndex, ANewIndex: Integer;
begin
  if not IsFilterActive then
    ResetIncrementalFilter;
  if GetFilteredItemCount = 0 then
    Exit;
  ANewCurrentKey := 0;
  if not UseSearchControl and (ItemIndex = -1) then
    AItemIndex := CurrentKey
  else
    AItemIndex := ItemIndex;
  case ADirection of
    egdBegin:
      ANewCurrentKey := GetLookupItemIndexFromFilteredItemIndex(0);
    egdEnd:
      ANewCurrentKey := GetLookupItemIndexFromFilteredItemIndex(GetFilteredItemCount - 1);
    egdPrev:
      begin
        AIndex := GetFilteredItemIndex(AItemIndex);
        ANewIndex := AIndex - 1;
        if ANewIndex < 0 then
          if ACircular then
            ANewIndex := GetFilteredItemCount - 1
          else
            if UseSearchControl and IsFilterActive then
            begin
              if ANewIndex < -1 then
                ANewIndex := GetFilteredItemCount - 1;
            end
            else
              ANewIndex := AIndex;
        ANewCurrentKey := GetLookupItemIndexFromFilteredItemIndex(ANewIndex);
      end;
    egdNext:
      begin
        AIndex := GetFilteredItemIndex(AItemIndex);
        ANewIndex := AIndex + 1;
        if ANewIndex = GetFilteredItemCount then
          if ACircular then
            ANewIndex := 0
          else
            if UseSearchControl and IsFilterActive then
              ANewIndex := -1
            else
              ANewIndex := AIndex;
        ANewCurrentKey := GetLookupItemIndexFromFilteredItemIndex(ANewIndex);
      end;
    egdPageUp:
      if AItemIndex = -1 then
        if ACircular then
          ANewCurrentKey := GetLookupItemIndexFromFilteredItemIndex(GetFilteredItemCount - 1)
        else
          ANewCurrentKey := AItemIndex
      else
      begin
        AIndex := GetFilteredItemIndex(AItemIndex);
        ANewIndex := AIndex - ActiveProperties.GetDropDownPageRowCount + 1;
        if (ANewIndex < 0) then
          if ACircular then
            if AIndex = 0 then
              ANewIndex := GetFilteredItemCount - 1
            else
              ANewIndex := 0
          else
            ANewIndex := 0;
        ANewCurrentKey := GetLookupItemIndexFromFilteredItemIndex(ANewIndex);
      end;
    egdPageDown:
      begin
        AIndex := GetFilteredItemIndex(AItemIndex);
        if AIndex = -1 then
          ANewIndex := AIndex + ActiveProperties.GetDropDownPageRowCount
        else
          ANewIndex := AIndex + ActiveProperties.GetDropDownPageRowCount - 1;
        if (ANewIndex >= GetFilteredItemCount) then
          if ACircular then
            if AItemIndex = GetFilteredItemCount - 1 then
              ANewIndex := 0
            else
              ANewIndex := GetFilteredItemCount - 1
          else
            ANewIndex := GetFilteredItemCount - 1;
        ANewCurrentKey := GetLookupItemIndexFromFilteredItemIndex(ANewIndex);
      end;
  end;
  if (FList = nil) or not FList.IsVisible or
    ActiveProperties.ImmediateUpdateText and Edit.DoEditing then
      CurrentKey := ANewCurrentKey
  else
  begin
    InternalSetItemIndex(ANewCurrentKey);
    ListContainer.ShowTouchScrollUI(ListContainer, True);
  end;
end;

procedure TcxCustomTextEditLookupData.Initialize(AVisualControlsParent: TWinControl);
begin
  FIsInitializing := True;
  try
    DoInitialize(AVisualControlsParent);
  finally
    FIsInitializing := False;
  end;
end;

procedure TcxCustomTextEditLookupData.InternalSetCurrentKey(Value: Integer);
begin
  if (Value >= -1) and (Value < GetItemCount) then
  begin
    FCurrentKey := Value;
    InternalSetItemIndex(Value);
  end;
end;

function TcxCustomTextEditLookupData.IsEmpty: Boolean;
begin
  Result := GetFilteredItemCount = 0;
end;

function TcxCustomTextEditLookupData.IsMouseOverList(const P: TPoint): Boolean;
begin
  Result := PtInRect(FList.BoundsRect, FList.ScreenToClient(P));
end;

procedure TcxCustomTextEditLookupData.ListChanged;
begin
  if (FList <> nil) and FList.HandleAllocated then
  begin
    FList.Items.BeginUpdate;
    try
      FList.SetItemCount(GetFilteredItemCount);
    finally
      FList.Items.EndUpdate;
    end;
  end;
end;

function TcxCustomTextEditLookupData.NeedLocateItemWithFullString: Boolean;
begin
  Result := False;
end;

procedure TcxCustomTextEditLookupData.ResetIncrementalFilter;
var
  I: Integer;
begin
  FSearchText := '';
  FFilteredLookupItems.Clear;
  for I := 0 to GetItemCount - 1 do
    FFilteredLookupItems.Add(I);
end;

procedure TcxCustomTextEditLookupData.SetSelectedItem(Value: Integer);
begin
  if (FList <> nil) and FList.HandleAllocated then
    FList.ItemIndex := GetFilteredItemIndex(Value);
end;

procedure TcxCustomTextEditLookupData.UpdateFilteredLookupItems(const ASearchText: string);

  function IsSuitable(const ASearchText, AItemText: string): Boolean;
  begin
    Result := (ASearchText = '') or
      IsLikeTypeFiltering and (Pos(AnsiUpperCase(ASearchText), AnsiUpperCase(AItemText)) <> 0) or
      not IsLikeTypeFiltering and (Pos(AnsiUpperCase(ASearchText), AnsiUpperCase(AItemText)) = 1);
  end;

var
  I: Integer;
  AItem: string;
begin
  FSearchText := ASearchText;
  FFilteredLookupItems.Clear;
  for I := 0 to GetItemCount - 1 do
  begin
    AItem := GetItem(I);
    if IsSuitable(ASearchText, AItem) then
      FFilteredLookupItems.Add(I);
  end;
end;

function TcxCustomTextEditLookupData.UseSearchControl: Boolean;
begin
  Result := ActiveProperties.UseSearchControl;
end;

function TcxCustomTextEditLookupData.Locate(var AText, ATail: string;
  ANext: Boolean): Boolean;
begin
  Result := InternalLocate(AText, ATail, ANext, False);
  if IsIncrementalFiltering and IsFilterActive and
    (Result or (AText = '') or ((ActiveProperties.EditingStyle = esEdit) and ActiveProperties.IncrementalSearch)) then
    UpdateFilteredLookupItems(AText);
end;

procedure TcxCustomTextEditLookupData.PositionVisualArea(const AClientRect: TRect);
begin
  DoPositionVisualArea(AClientRect);
end;

procedure TcxCustomTextEditLookupData.PropertiesChanged;
begin
  ListChanged;
end;

procedure TcxCustomTextEditLookupData.SelectItem;
var
  APrevCurrentKey: TcxEditValue;
begin
  if (FItemIndex = -1) or (CurrentKey <> FItemIndex) and not Edit.DoEditing then
    Exit;
  APrevCurrentKey := CurrentKey;
  CurrentKey := FItemIndex;
  if VarEqualsExact(APrevCurrentKey, CurrentKey) then
    DoSelectItem;
end;

function TcxCustomTextEditLookupData.GetActiveControl: TControl;
begin
  Result := FList;
end;

function TcxCustomTextEditLookupData.GetCurrentKey: TcxEditValue;
begin
  Result := FCurrentKey;
end;

function TcxCustomTextEditLookupData.GetOnCurrentKeyChanged: TNotifyEvent;
begin
  Result := FOnCurrentKeyChanged;
end;

function TcxCustomTextEditLookupData.GetOnSelectItem: TNotifyEvent;
begin
  Result := FOnSelectItem;
end;

function TcxCustomTextEditLookupData.FindItemByText(const AText: string; AStartIndex: Integer; AFindFullText: Boolean): Integer;
var
  I, L: Integer;
  S: string;
  AFinded: Boolean;
begin
  Result := -1;
  L := Length(AText);
  if L > 0 then
    for I := AStartIndex to GetItemCount - 1 do
    begin
      if AFindFullText or IsLikeTypeFiltering and IsFilterActive then
        S := GetItem(I)
      else
        S := Copy(GetItem(I), 1, L);

      if IsLikeTypeFiltering and IsFilterActive then
        AFinded := Pos(AnsiUpperCase(AText), AnsiUpperCase(S)) <> 0
      else
        AFinded := InternalCompareString(AText, S, False);

      if AFinded then
      begin
        Result := I;
        Break;
      end;
    end;
end;

function TcxCustomTextEditLookupData.GetEdit: TcxCustomTextEdit;
begin
  Result := TcxCustomTextEdit(FOwner);
end;

function TcxCustomTextEditLookupData.GetLookupItemIndexFromFilteredItemIndex(Index: Integer): Integer;
begin
  if IsIncrementalFiltering and (Index <> -1) then
    Result := FFilteredLookupItems[Index]
  else
    Result := Index;
end;

function TcxCustomTextEditLookupData.GetFilteredItemIndex(Index: Integer): Integer;
begin
  if IsIncrementalFiltering then
    Result :=  FFilteredLookupItems.IndexOf(Index)
  else
    Result := Index;
end;

function TcxCustomTextEditLookupData.GetFilteredItem(Index: Integer): string;
var
  ALookupItemIndex: Integer;
begin
  if IsIncrementalFiltering then
    ALookupItemIndex := GetLookupItemIndexFromFilteredItemIndex(Index)
  else
    ALookupItemIndex := Index;
  Result := GetItem(ALookupItemIndex);
end;

function TcxCustomTextEditLookupData.GetFilteredItemCount: Integer;
begin
  if IsIncrementalFiltering then
    Result := FFilteredLookupItems.Count
  else
    Result := GetItemCount;
end;

function TcxCustomTextEditLookupData.GetItems: TStrings;
begin
  Result := ActiveProperties.FLookupItems;
end;

function TcxCustomTextEditLookupData.GetActiveProperties: TcxCustomTextEditProperties;
begin
  Result := Edit.ActiveProperties;
end;

function TcxCustomTextEditLookupData.GetScaleFactor: TdxScaleFactor;
begin
  Result := Edit.ScaleFactor;
end;

function TcxCustomTextEditLookupData.IndexOf(const AText: string): Integer;
var
  ACount, I: Integer;
begin
  Result := -1;
  ACount := GetItemCount;
  for I := 0 to ACount - 1 do
    if InternalCompareString(AText, GetItem(I), False) then
    begin
      Result := I;
      Break;
    end;
end;

procedure TcxCustomTextEditLookupData.InternalSetItemIndex(Value: Integer);
begin
  if (Value < -1) or (Value >= GetItemCount) or (Value = FItemIndex) and (Value <> -1) then
    Exit;
  if (FList <> nil) and (GetItemCount > 0) then
  begin
    if Value = -1 then
      SetSelectedItem(0);
    SetSelectedItem(Value);
  end;
  FItemIndex := Value;
end;

function TcxCustomTextEditLookupData.IsFilterActive: Boolean;
begin
  Result := Edit.HasPopupWindow;
end;

function TcxCustomTextEditLookupData.IsIncrementalFiltering: Boolean;
begin
  Result := ActiveProperties.IncrementalFiltering;
end;

function TcxCustomTextEditLookupData.IsLikeTypeFiltering: Boolean;
begin
  Result := IsIncrementalFiltering and (ifoUseContainsOperator in ActiveProperties.IncrementalFilteringOptions);
end;

procedure TcxCustomTextEditLookupData.SetCurrentKey(const AKey: TcxEditValue);
var
  APrevCurrentKey: TcxEditValue;
begin
  APrevCurrentKey := FCurrentKey;
  InternalSetCurrentKey(AKey);
  if Edit <> nil then
    Edit.LockLookupDataTextChanged;
  try
    if not VarEqualsExact(APrevCurrentKey, FCurrentKey) or ((AKey >= 0) and (AKey < GetItemCount) and not InternalCompareString(Edit.Text, GetItem(AKey), True)) then
      DoSelectItem;
  finally
    if Edit <> nil then
      Edit.UnlockLookupDataTextChanged;
  end;
end;

procedure TcxCustomTextEditLookupData.SetItemIndex(Value: Integer);
begin
  if (FList = nil) or not FList.IsVisible or ActiveProperties.ImmediateUpdateText then
    CurrentKey := Value
  else
    InternalSetItemIndex(Value);
end;

procedure TcxCustomTextEditLookupData.SetItems(Value: TStrings);
begin
  ActiveProperties.FLookupItems.Assign(Value);
end;

procedure TcxCustomTextEditLookupData.SetOnCurrentKeyChanged(Value: TNotifyEvent);
begin
  FOnCurrentKeyChanged := Value;
end;

procedure TcxCustomTextEditLookupData.SetOnSelectItem(Value: TNotifyEvent);
begin
  FOnSelectItem := Value;
end;

procedure TcxCustomTextEditLookupData.TextChanged;
var
  AText, ATail: string;
begin
  if Edit.EditModeSetting or IsInitializing then
    Exit;
  AText := Edit.Text;
  if (ItemIndex >= GetItemCount) or
    not InternalCompareString(GetDisplayText(ItemIndex), AText, False) then
      InternalLocate(AText, ATail, False, True);
end;

{ TcxCustomTextEditViewData }

procedure TcxCustomTextEditViewData.Calculate(ACanvas: TcxCanvas; const ABounds: TRect;
  const P: TPoint; Button: TcxMouseButton; Shift: TShiftState; ViewInfo: TcxCustomEditViewInfo;
  AIsMouseEvent: Boolean);
var
  AViewInfo: TcxCustomTextEditViewInfo;
begin
  AViewInfo := TcxCustomTextEditViewInfo(ViewInfo);
  inherited Calculate(ACanvas, ABounds, P, Button, Shift, AViewInfo, AIsMouseEvent);
  AViewInfo.DrawSelectionBar := False;
  AViewInfo.EditingStyle := Properties.EditingStyle;
  AViewInfo.IsEditClass := GetIsEditClass;
  ACanvas.Font := AViewInfo.Font;
  AViewInfo.HasPopupWindow := (AViewInfo.Edit <> nil) and AViewInfo.Edit.HasPopupWindow;
  CalculateTextEditViewInfo(ACanvas, Self, AViewInfo, AIsMouseEvent);
  AViewInfo.MaxLineCount := Self.GetMaxLineCount;
  AViewInfo.TextOutData.Initialized := False;
  PrepareDrawTextFlags(ACanvas, AViewInfo);
end;

procedure TcxCustomTextEditViewData.DisplayValueToDrawValue(const ADisplayValue: TcxEditValue; AViewInfo: TcxCustomEditViewInfo);
begin
  with TcxCustomTextEditViewInfo(AViewInfo) do
  begin
    Text := ADisplayValue;
    Properties.DoDisplayValueToDisplayText(NativeStyle, Text);
  end;
end;

procedure TcxCustomTextEditViewData.EditValueToDrawValue(
  const AEditValue: TcxEditValue; AViewInfo: TcxCustomEditViewInfo);
begin
  if PreviewMode then
    TcxCustomTextEditViewInfo(AViewInfo).Text := ''
  else
    TcxCustomTextEditViewInfo(AViewInfo).Text := EditValueToDisplayText(AEditValue);
  PrepareSelection(AViewInfo);
end;

function TcxCustomTextEditViewData.GetClientExtent(ACanvas: TcxCanvas;
  AViewInfo: TcxCustomEditViewInfo): TRect;
begin
  Result := inherited GetClientExtent(ACanvas, AViewInfo);
  if not IsInplace and IsComboBoxStyle and
      AreVisualStylesMustBeUsed(Style.LookAndFeel.NativeStyle, totEdit) then
    Inc(Result.Right);
end;

function TcxCustomTextEditViewData.GetDrawTextFlags: DWORD;
const
  AHorzAlignmentFlags: array [TcxEditHorzAlignment] of DWORD = (
    CXTO_LEFT, CXTO_RIGHT, CXTO_CENTER_HORIZONTALLY
  );
  AVertAlignmentFlags: array [TcxEditVertAlignment] of DWORD = (
    CXTO_TOP, CXTO_BOTTOM, CXTO_CENTER_VERTICALLY
  );

var
  AAlignment: TAlignment;
begin
  AAlignment := HorzAlignment;
  if UseRightToLeftAlignment then
   ChangeBiDiModeAlignment(AAlignment);
  Result := AHorzAlignmentFlags[AAlignment];
  Result := Result or AVertAlignmentFlags[VertAlignment];
  Result := Result or CXTO_SINGLELINE;
  Result := Result or CXTO_PREVENT_LEFT_EXCEED or CXTO_PREVENT_TOP_EXCEED;
  if UseRightToLeftReading then
    Result := Result or CXTO_RTLREADING;
end;

function TcxCustomTextEditViewData.GetDrawTextOffset: TRect;
begin
  Result := GetTextEditDrawTextOffset(Self);
end;

procedure TcxCustomTextEditViewData.PrepareSelection(AViewInfo: TcxCustomEditViewInfo);
var
  ACustomTextEditViewInfo: TcxCustomTextEditViewInfo;
begin
  ACustomTextEditViewInfo := TcxCustomTextEditViewInfo(AViewInfo);
  ACustomTextEditViewInfo.SelStart := SelStart;
  ACustomTextEditViewInfo.SelLength := SelLength;
  if SelLength = 0 then
    Exit;
  if SelBackgroundColor = clDefault then
  begin
    if SelTextColor = clDefault then
    begin
      if Style.Color = clHighlight then
      begin
        ACustomTextEditViewInfo.SelBackgroundColor := clBlack;
        ACustomTextEditViewInfo.SelTextColor := clWhite;
      end
      else
      begin
        ACustomTextEditViewInfo.SelBackgroundColor := clHighlight;
        ACustomTextEditViewInfo.SelTextColor := clHighlightText;
      end;
    end
    else
    begin
      ACustomTextEditViewInfo.SelTextColor := SelTextColor;
      ACustomTextEditViewInfo.SelBackgroundColor := dxInvertColor(SelTextColor);
    end;
  end
  else
  begin
    ACustomTextEditViewInfo.SelBackgroundColor := SelBackgroundColor;
    if SelTextColor = clDefault then
      ACustomTextEditViewInfo.SelTextColor := dxInvertColor(SelBackgroundColor)
    else
      ACustomTextEditViewInfo.SelTextColor := SelTextColor;
  end;
end;

procedure TcxCustomTextEditViewData.CalculateButtonNativePartInfo(
  ATheme: TdxTheme; AButtonViewInfo: TcxEditButtonViewInfo);
begin
  inherited CalculateButtonNativePartInfo(ATheme, AButtonViewInfo);
  if NativeStyle and IsWinVistaOrLater and not IsInplace and
    (Properties.EditingStyle = esFixedList) and AButtonViewInfo.Data.ComboBoxStyle and
    (AButtonViewInfo.Data.NativeState <> CBXS_DISABLED)
  then
    AButtonViewInfo.Data.NativeState := CBXS_NORMAL;
end;

function TcxCustomTextEditViewData.GetIsEditClass: Boolean;
begin
  Result := (Edit <> nil) and Edit.IsEditClass;
end;

function TcxCustomTextEditViewData.GetMaxLineCount: Integer;
begin
  Result := MaxLineCount;
end;

procedure TcxCustomTextEditViewData.InitCacheData;
begin
  inherited InitCacheData;
  FIsValueFormatted := Properties.IsValueFormattedByProvider;
end;

function TcxCustomTextEditViewData.InternalEditValueToDisplayText(AEditValue: TcxEditValue): string;
begin
  Result := '';
  try
    if FIsValueFormatted then
      Result := VarToStr(AEditValue)
    else
      Result := Properties.GetDefaultDisplayValue(AEditValue, InternalFocused);
  finally
    Properties.DoDisplayValueToDisplayText(NativeStyle, Result);
  end;
end;

function TcxCustomTextEditViewData.InternalGetEditContentSize(ACanvas: TcxCanvas;
  const AEditValue: TcxEditValue; const AEditSizeProperties: TcxEditSizeProperties): TSize;
var
  AContentSize: TSize;
begin
  AContentSize := inherited InternalGetEditContentSize(ACanvas, AEditValue, AEditSizeProperties);
  Result := GetTextEditContentSize(ACanvas, Self, EditValueToDisplayText(AEditValue), GetDrawTextFlags, AEditSizeProperties);
  Result := cxSizeMax(Result, AContentSize);
end;

function TcxCustomTextEditViewData.IsComboBoxStyle: Boolean;
begin
  Result := False;
end;

procedure TcxCustomTextEditViewData.PrepareDrawTextFlags(ACanvas: TcxCanvas;
  AViewInfo: TcxCustomEditViewInfo);
begin
  with TcxCustomTextEditViewInfo(AViewInfo) do
  begin
    if not Properties.IsMultiLine and (esoAutoHeight in Properties.GetSupportedOperations) and
        (epoAutoHeight in Self.PaintOptions) then
      PrepareTextEditDrawTextFlags(ACanvas, Self, TcxCustomTextEditViewInfo(AViewInfo))
    else
      DrawTextFlags := GetDrawTextFlags;
    if epoShowEndEllipsis in PaintOptions then
      DrawTextFlags := DrawTextFlags or CXTO_END_ELLIPSIS;
    ComboBoxStyle := IsComboBoxStyle;
  end;
end;

function TcxCustomTextEditViewData.GetProperties: TcxCustomTextEditProperties;
begin
  Result := TcxCustomTextEditProperties(FProperties);
end;

{ TcxCustomTextEditViewInfo }

destructor TcxCustomTextEditViewInfo.Destroy;
begin
  cxResetTextRows(TextOutData.TextRows);
  inherited Destroy;
end;

function TcxCustomTextEditViewInfo.NeedShowHint(ACanvas: TcxCanvas;
  const P: TPoint; const AVisibleBounds: TRect; out AText: TCaption;
  out AIsMultiLine: Boolean; out ATextRect: TRect; AMaxLineCount: Integer = 0): Boolean;

  function GetRealVisibleBounds: TRect;
  begin
    Result := AVisibleBounds;
    if EqualRect(Result, cxEmptyRect) then
      Result := TextRect
    else
    begin
      OffsetRect(Result, -Left, -Top);
      IntersectRect(Result, Result, TextRect);
    end;
  end;

  function IsMultiLine(ATextFlags: DWORD): Boolean;
  begin
    Result := (ATextFlags and CXTO_SINGLELINE = 0) and (ATextFlags and CXTO_WORDBREAK <> 0);
  end;

  function GetCalcTextFlags: Integer;
  begin
    Result := DrawTextFlags and not (CXTO_CENTER_VERTICALLY or CXTO_BOTTOM or CXTO_SINGLELINE) or
      CXTO_CALCRECT;
  end;

  function GetDrawBounds(const AVisibleBounds: TRect): TRect;
  var
    ALinesHeight: Integer;
  begin
    Result := AVisibleBounds;
    if AMaxLineCount > 0 then
    begin
      ALinesHeight := cxTextHeight(ACanvas.Handle) * AMaxLineCount;
      Result.Bottom := Min(Result.Bottom, Result.Top + ALinesHeight);
    end;
  end;

  function IsTextNotFullyVisible(const ARealVisibleBounds: TRect): Boolean;
  var
    R, ADrawBounds: TRect;
  begin
    if Length(Text) > 0 then
    begin
      ACanvas.Font := Font;
      R := ARealVisibleBounds;
      ADrawBounds := GetDrawBounds(ARealVisibleBounds);
      cxTextOut(ACanvas.Canvas, Text, R, GetCalcTextFlags);
      Result := (R.Bottom > ADrawBounds.Bottom) or (R.Right > ADrawBounds.Right);
    end
    else
      Result := False;
  end;

  procedure SetTextAreaHintParameters;
  begin
    AIsMultiLine := IsMultiLine(DrawTextFlags);
    AText := Text;
    ATextRect := TextRect;
    OffsetRect(ATextRect, Left, Top);
  end;

var
  ARealVisibleBounds: TRect;
begin
  ARealVisibleBounds := GetRealVisibleBounds;
  Result := PtInRect(ARealVisibleBounds, Point(P.X - Left, P.Y - Top)) and
    IsTextNotFullyVisible(ARealVisibleBounds);
  if Result then
    SetTextAreaHintParameters
  else
  begin
    Result := inherited NeedShowHint(ACanvas, P, AVisibleBounds, AText, AIsMultiLine, ATextRect);
    if not Result then
      SetTextAreaHintParameters;
  end;
end;

procedure TcxCustomTextEditViewInfo.Offset(DX, DY: Integer);
var
  I: Integer;
begin
  inherited Offset(DX, DY);
  OffsetRect(TextRect, DX, DY);
  with TextOutData do
    if Initialized then
    begin
      OffsetRect(TextRect, DX, DY);
      for I := 0 to cxGetTextRowCount(TextRows) - 1 do
        with cxGetTextRow(TextRows, I)^ do
        begin
          TextOriginX := TextOriginX + DX;
          TextOriginY := TextOriginY + DY;
        end;
    end;
end;

procedure TcxCustomTextEditViewInfo.PrepareCanvasFont(ACanvas: TCanvas);
begin
  inherited PrepareCanvasFont(ACanvas);
  if NeedDrawPasswordBullets then
     ACanvas.Font.Name := 'Segoe UI';
end;

procedure TcxCustomTextEditViewInfo.DrawText(ACanvas: TcxCanvas);
begin
  if CanDrawEditValue then
    DrawEditText(ACanvas, Self, NeedDrawPasswordBullets);
end;

function TcxCustomTextEditViewInfo.GetBackgroundPaintingStyle: TcxEditBackgroundPaintingStyle;
begin
  if not ComboBoxStyle then
    Result := bpsSolid
  else
    if EditingStyle in [esFixedList, esNoEdit] then
      Result := bpsComboListEdit
    else
      Result := bpsComboEdit;
end;

function TcxCustomTextEditViewInfo.GetTextBaseLine: Integer;
var
  ACanvas: TcxScreenCanvas;
  ATextMetric: TTextMetric;
begin
  ACanvas := TcxScreenCanvas.Create;
  try
    ACanvas.Font := Font;
    GetTextMetrics(ACanvas.Handle, ATextMetric);
    case EditProperties.Alignment.Vert of
      taTopJustify:
        Result := TextRect.Top + ATextMetric.tmAscent + 1;
      taBottomJustify:
        Result := TextRect.Bottom - ATextMetric.tmDescent + 1;
    else
      Result := TextRect.Top + (TextRect.Bottom - TextRect.Top - ATextMetric.tmHeight) div 2 + ATextMetric.tmAscent + 1;
    end;
  finally
    ACanvas.Free;
  end;
end;

procedure TcxCustomTextEditViewInfo.InternalPaint(ACanvas: TcxCanvas);
begin
  DrawTextEdit(ACanvas, Self);
end;

function TcxCustomTextEditViewInfo.NeedDrawPasswordBullets: Boolean;
var
  AProperties: TcxCustomTextEditProperties;
begin
  AProperties := Safe<TcxCustomTextEditProperties>.Cast(EditProperties);
  Result := (AProperties <> nil) and (AProperties.EchoMode = TcxEditEchoMode.eemPassword) and
    ShowPasswordCharAsBullet(NativeStyle);
end;

{ TcxCustomTextEditProperties }

constructor TcxCustomTextEditProperties.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FCharCase := ecNormal;
  FEchoMode := eemNormal;
  FFixedListSelection := True;
  FHideSelection := True;
  FImeMode := imDontCare;
  FIncrementalSearch := True;
  FIncrementalFilteringOptions := GetDefaultIncrementalFilteringOptions;
  FLookupItems := TStringList.Create;
  FLookupItems.Duplicates := dupAccept;
  FLookupItems.OnChange := LookupItemsChanged;
  cxFormatController.AddListener(Self);
end;

destructor TcxCustomTextEditProperties.Destroy;
begin
  cxFormatController.RemoveListener(Self);
  FreeAndNil(FLookupItems);
  inherited Destroy;
end;

function TcxCustomTextEditProperties.CanCompareEditValue: Boolean;
begin
  Result := True;
end;

function TcxCustomTextEditProperties.CompareDisplayValues(
  const AEditValue1, AEditValue2: TcxEditValue): Boolean;
var
  AText1, AText2: string;
begin
  AText1 := GetDisplayText(AEditValue1, True);
  AText2 := GetDisplayText(AEditValue2, True);
  Result := InternalCompareString(AText1, AText2, True);
end;

class function TcxCustomTextEditProperties.GetContainerClass: TcxContainerClass;
begin
  Result := TcxTextEdit;
end;

function TcxCustomTextEditProperties.GetDisplayText(const AEditValue: TcxEditValue;
  AFullText: Boolean = False; AIsInplace: Boolean = True): string;
var
  AText: string;
begin
  AText := '';
  try
    if IsValueFormattedByProvider then
      AText := VarToStr(AEditValue)
    else
      AText := GetDefaultDisplayValue(AEditValue,
        not AIsInplace and not IsEditValueConversionDependOnFocused);
  finally
    DisplayValueToDisplayText(AText);
    Result := AText;
  end;
end;

function TcxCustomTextEditProperties.GetSupportedOperations: TcxEditSupportedOperations;
begin
  Result := inherited GetSupportedOperations + [esoAutoHeight, esoEditing,
    esoFiltering, esoHorzAlignment, esoIncSearch, esoSorting];
  if (Buttons.Count = 0) and (EchoMode = eemNormal) then
    Include(Result, esoEditingAutoHeight);
end;

class function TcxCustomTextEditProperties.GetViewInfoClass: TcxContainerViewInfoClass;
begin
  Result := TcxCustomTextEditViewInfo;
end;

function TcxCustomTextEditProperties.IsEditValueValid(var EditValue: TcxEditValue; AEditFocused: Boolean): Boolean;
begin
  Result := VarIsNull(EditValue) or not VarIsStr(EditValue) or
    IsDisplayValueValid(EditValue, AEditFocused);
end;

procedure TcxCustomTextEditProperties.DoPrepareDisplayValue(const AEditValue: TcxEditValue;
  var ADisplayValue: TcxEditValue; AEditFocused: Boolean);

  function InternalPrepareDisplayValue(AEditValue: Variant): TcxEditValue;
  var
    AEditFormat: string;
    AIsCurrency, AIsOnGetTextAssigned: Boolean;
    S: string;
    APrecision: Integer;
    V: TBcd;
  begin
    if AEditFocused then
    begin
      AEditFormat := InternalGetEditFormat(AIsCurrency, AIsOnGetTextAssigned);
      if IDefaultValuesProvider = nil then
        APrecision := cxEditDefaultPrecision
      else
        APrecision := IDefaultValuesProvider.DefaultPrecision;
      if AIsCurrency then
      begin
        Result := VarToStr(AEditValue);
        if TryStrToBcd(Result, V) then
          ADisplayValue := BcdToStrF(V, ffFixed, APrecision, dxFormatSettings.CurrencyDecimals);
      end
      else
        if AEditFormat = '' then
        begin
          S := VarToStr(AEditValue);
          if TryStrToBcd(S, V) then
            S := BcdToStrF(V, ffGeneral, APrecision, 0);
          if HasDigitGrouping(False) then
            InsertThousandSeparator(S);
          Result := S;
        end
        else
          Result := FormatFloat(AEditFormat, AEditValue);
    end
    else
      if DisplayFormat <> '' then
        Result := FormatFloat(DisplayFormat, AEditValue)
      else
        Result := VarToStr(AEditValue);
  end;

begin
  if IsEditValueNumeric then
  begin
    if VarIsSoftNull(AEditValue) then
      ADisplayValue := ''
    else
      if not VarIsNumericEx(AEditValue) and not VarIsStr(AEditValue) then
        raise EConvertError.CreateFmt(cxGetResourceString(@cxSEditNumericValueConvertError), [])
      else
        ADisplayValue := InternalPrepareDisplayValue(AEditValue);
  end
  else
    if VarIsArray(AEditValue) and not (VarArrayDimCount(AEditValue) = 1) then
      ADisplayValue := ''
    else
      ADisplayValue := dxVariantToString(AEditValue);
end;

procedure TcxCustomTextEditProperties.PrepareDisplayValue(const AEditValue: TcxEditValue;
  var ADisplayValue: TcxEditValue; AEditFocused: Boolean);
begin
  if IsEditValueEmpty(AEditValue) and UseNullString and not AEditFocused and not IsDesigning then
    ADisplayValue := Nullstring
  else
    DoPrepareDisplayValue(AEditValue, ADisplayValue, AEditFocused);
end;

procedure TcxCustomTextEditProperties.ValidateDisplayValue(var DisplayValue: TcxEditValue;
  var ErrorText: TCaption; var Error: Boolean; AEdit: TcxCustomEdit);
begin
  if IsEditValueNumeric and IsValueBoundsDefined then
    CheckDisplayValueBounds(DisplayValue, ErrorText, Error, AEdit);
  inherited ValidateDisplayValue(DisplayValue, ErrorText, Error, AEdit);
end;

procedure TcxCustomTextEditProperties.DisplayValueToDisplayText(var ADisplayValue: string);
begin
  if (EchoMode = eemNormal) and (CharCase <> ecNormal) then
    CheckCharsRegister(ADisplayValue, CharCase);
end;

function TcxCustomTextEditProperties.IsDisplayValueValid(var ADisplayValue: TcxEditValue; AEditFocused: Boolean): Boolean;
var
  AText: string;
begin
  AText := VarToStr(ADisplayValue);
  Result := not((AText <> '') and (EditingStyle in [esEditList, esFixedList]) and  UseLookupData and not FindLookupText(AText));
  if Result then
  begin
    CheckCharsRegister(AText, CharCase);
    ADisplayValue := AText;
  end;
end;

procedure TcxCustomTextEditProperties.SetMinMaxValues(AMinValue, AMaxValue: Double);
begin
  FillMinMaxValues(AMinValue, AMaxValue);
end;

procedure TcxCustomTextEditProperties.AlignmentChangedHandler(Sender: TObject);
begin
  BeginUpdate;
  try
    inherited AlignmentChangedHandler(Sender);
  finally
    EndUpdate;
  end;
end;

procedure TcxCustomTextEditProperties.BaseSetAlignment(Value: TcxEditAlignment);
begin
  BeginUpdate;
  try
    inherited BaseSetAlignment(Value);
  finally
    EndUpdate;
  end;
end;

function TcxCustomTextEditProperties.CanValidate: Boolean;
begin
  Result := True;
end;

// obsolete
procedure TcxCustomTextEditProperties.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('IsDisplayFormatAssigned', ReadIsDisplayFormatAssigned,
    nil, False);
end;

procedure TcxCustomTextEditProperties.DoAssign(AProperties: TcxCustomEditProperties);
begin
  inherited;
  if AProperties is TcxCustomTextEditProperties then
    with TcxCustomTextEditProperties(AProperties) do
    begin
      Self.CharCase := CharCase;
      Self.EchoMode := EchoMode;
      Self.FixedListSelection := FixedListSelection;
      Self.HideCursor := HideCursor;
      Self.HideSelection := HideSelection;
      Self.ImmediateUpdateText := ImmediateUpdateText;
      Self.IncrementalSearch := IncrementalSearch;
      Self.IncrementalFiltering := IncrementalFiltering;
      Self.IncrementalFilteringOptions := IncrementalFilteringOptions;
      Self.MRUMode := MRUMode;
      Self.UseNullString := UseNullString;
      Self.Nullstring := Nullstring;
      Self.ValidChars := ValidChars;

      Self.LookupItemsSorted := False;
      Self.LookupItems.Assign(LookupItems);
      Self.LookupItemsSorted := LookupItemsSorted;

      Self.AssignedValues.DisplayFormat := False;
      if AssignedValues.DisplayFormat then
        Self.DisplayFormat := DisplayFormat;

      Self.AssignedValues.EditFormat := False;
      if AssignedValues.EditFormat then
        Self.EditFormat := EditFormat;

      Self.AssignedValues.MaxLength := False;
      if AssignedValues.MaxLength then
        Self.MaxLength := MaxLength;

      Self.OEMConvert := OEMConvert;
      Self.ImeMode := ImeMode;
      Self.ImeName := ImeName;
      Self.PasswordChar := PasswordChar;
      Self.UseDisplayFormatWhenEditing := UseDisplayFormatWhenEditing;
      Self.OnNewLookupDisplayText := OnNewLookupDisplayText;
    end;
end;

class function TcxCustomTextEditProperties.GetAssignedValuesClass: TcxCustomEditPropertiesValuesClass;
begin
  Result := TcxTextEditPropertiesValues;
end;

function TcxCustomTextEditProperties.GetDisplayFormatOptions: TcxEditDisplayFormatOptions;
begin
  Result := [dfoSupports];
end;

function TcxCustomTextEditProperties.GetValidateErrorText(AErrorKind: TcxEditErrorKind): string;
begin
  if AErrorKind = ekValueOutOfBounds then
    Result := cxGetResourceString(@cxSEditValueOutOfBounds)
  else
    Result := inherited GetValidateErrorText(AErrorKind);
end;

class function TcxCustomTextEditProperties.GetViewDataClass: TcxCustomEditViewDataClass;
begin
  Result := TcxCustomTextEditViewData;
end;

function TcxCustomTextEditProperties.GetEditingStyle: TcxEditEditingStyle;
begin
  if FHideCursor then
    Result := esNoEdit
  else
    Result := esEdit;
end;

class function TcxCustomTextEditProperties.GetLookupDataClass: TcxInterfacedPersistentClass;
begin
  Result := TcxCustomTextEditLookupData;
end;

function TcxCustomTextEditProperties.HasDigitGrouping(
  AIsDisplayValueSynchronizing: Boolean): Boolean;
begin
  Result := False;
end;

function TcxCustomTextEditProperties.GetEditValueSource(AEditFocused: Boolean): TcxDataEditValueSource;
begin
  if not AEditFocused and UseNullstring then
    Result := inherited GetEditValueSource(AEditFocused)
  else
    Result := evsText;
end;

function TcxCustomTextEditProperties.HasDisplayValue: Boolean;
begin
  Result := True;
end;

procedure TcxCustomTextEditProperties.FormatChanged;
begin
  FFormatChanging := True;
  try
    Changed;
  finally
    FFormatChanging := False;
  end;
end;

function TcxCustomTextEditProperties.IsResetEditClass: Boolean;
begin
  Result := EditingStyle <> esNoEdit;
end;

function TcxCustomTextEditProperties.CanIncrementalSearch: Boolean;
begin
  Result := (EditingStyle = esEdit) and IncrementalSearch or
    (EditingStyle in [esEditList, esFixedList])
end;

procedure TcxCustomTextEditProperties.CheckEditValueBounds(const AEditValue: TcxEditValue;
  var AErrorText: TCaption; var AError: Boolean; AEdit: TcxCustomEdit);
var
  AEditValueAsDouble: Double;
begin
  if (VarIsNumericEx(AEditValue) or VarIsDate(AEditValue)) then
  begin
    AEditValueAsDouble := AEditValue;
    if (IsValueBoundDefined(evbMin) and (AEditValueAsDouble < MinValue) or IsValueBoundDefined(evbMax) and (AEditValueAsDouble > MaxValue)) then
    begin
      AError := True;
      AErrorText := GetValidateErrorText(ekValueOutOfBounds);
    end;
  end;
end;

procedure TcxCustomTextEditProperties.CheckDisplayValueBounds(const ADisplayValue: TcxEditValue;
  var AErrorText: TCaption; var AError: Boolean; AEdit: TcxCustomEdit);
var
  AEditValue: TcxEditValue;
begin
  AEdit.PrepareEditValue(ADisplayValue, AEditValue, AEdit.InternalFocused);
  CheckEditValueBounds(AEditValue, AErrorText, AError, AEdit);
end;

function TcxCustomTextEditProperties.DefaultFocusedDisplayValue: TcxEditValue;
begin
  Result := '';
end;

procedure TcxCustomTextEditProperties.DoDisplayValueToDisplayText(ANativeStyle: Boolean; var ADisplayValue: string);
begin
  if EchoMode = TcxEditEchoMode.eemPassword then
    ADisplayValue := StringOfChar(GetWindowsPasswordChar(ANativeStyle, PasswordChar), Length(ADisplayValue));
  DisplayValueToDisplayText(ADisplayValue);
end;

function TcxCustomTextEditProperties.FindLookupText(const AText: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to FLookupItems.Count - 1 do
    if InternalCompareString(AText, FLookupItems[I], False) then
    begin
      Result := True;
      Break;
    end;
end;

function TcxCustomTextEditProperties.GetDefaultDisplayFormat: string;
begin
  Result := '';
end;

function TcxCustomTextEditProperties.GetDefaultDisplayValue(const AEditValue: TcxEditValue;
  AEditFocused: Boolean): TcxEditValue;
var
  AValue: TcxEditValue;
begin
  AValue := AEditValue;
  if IsEditValueValid(AValue, AEditFocused) then
    PrepareDisplayValue(AValue, Result, AEditFocused)
  else
    if VarIsDate(AEditValue) then
      Result := DateTimeToStr(AEditValue)
    else
      Result := VarToStr(AEditValue)
end;

function TcxCustomTextEditProperties.GetDefaultMaxLength: Integer;
begin
  if IDefaultValuesProvider = nil then
    Result := 0
  else
    Result := IDefaultValuesProvider.DefaultMaxLength;
end;

function TcxCustomTextEditProperties.GetDefaultVertAlignment: TcxEditVertAlignment;
begin
  Result := cxTextEditDefaultVertAlignment;
end;

function TcxCustomTextEditProperties.GetDropDownPageRowCount: Integer;
begin
  Result := cxEditDefaultDropDownPageRowCount;
end;

function TcxCustomTextEditProperties.InternalGetEditFormat(
  out AIsCurrency, AIsOnGetTextAssigned: Boolean;
  AEdit: TcxCustomTextEdit = nil): string;
begin
  AIsCurrency := False;
  AIsOnGetTextAssigned := False;
  Result := '';
  if AssignedValues.EditFormat then
    Result := FEditFormat
  else
    begin
      if not ((AEdit <> nil) and AEdit.IsInplace) and (IDefaultValuesProvider <> nil) and
        IDefaultValuesProvider.IsOnGetTextAssigned then
      begin
        AIsOnGetTextAssigned := True;
// SC# 49build B153519      Exit;
      end;
      if (IDefaultValuesProvider <> nil) and
          (IDefaultValuesProvider.DefaultEditFormat <> '') then
        Result := IDefaultValuesProvider.DefaultEditFormat
      else
      begin
        if AssignedValues.DisplayFormat then
          Result := DisplayFormat
        else
          if (IDefaultValuesProvider <> nil) and (IDefaultValuesProvider.DefaultDisplayFormat <> '') then
            Result := IDefaultValuesProvider.DefaultDisplayFormat
          else
            if GetDefaultDisplayFormat <> '' then
              Result := GetDefaultDisplayFormat
            else
              if IDefaultValuesProvider <> nil then
                AIsCurrency := not(dfoNoCurrencyValue in DisplayFormatOptions) and
                  IDefaultValuesProvider.IsCurrency;
        if not UseDisplayFormatWhenEditing then
          Result := '';
      end;
    end;
end;

function TcxCustomTextEditProperties.IsEditValueEmpty(const AEditValue: TcxEditValue): Boolean;
begin
  Result := VarIsNull(AEditValue);
end;

function TcxCustomTextEditProperties.IsEditValueNumeric: Boolean;
begin
  Result := False;
end;

function TcxCustomTextEditProperties.IsLookupDataVisual: Boolean;
begin
  Result := False;
end;

function TcxCustomTextEditProperties.IsLookupEdit: Boolean;
begin
  Result := False;
end;

function TcxCustomTextEditProperties.IsMultiLine: Boolean;
begin
  Result := False;
end;

function TcxCustomTextEditProperties.IsPopupKey(Key: Word; Shift: TShiftState): Boolean;
begin
  Result := False;
end;

function TcxCustomTextEditProperties.IsValueBoundDefined(
  ABound: TcxEditValueBound): Boolean;
begin
  if AssignedValues.MinValue xor AssignedValues.MaxValue then
    Result := (ABound = evbMin) and AssignedValues.MinValue or
      (ABound = evbMax) and AssignedValues.MaxValue
  else
    Result := MinValue < MaxValue;
end;

function TcxCustomTextEditProperties.IsValueBoundsDefined: Boolean;
begin
  Result := IsValueBoundDefined(evbMin) or IsValueBoundDefined(evbMax);
end;

procedure TcxCustomTextEditProperties.LookupDataChanged(Sender: TObject);
begin
  Changed;
end;

procedure TcxCustomTextEditProperties.MaxLengthChanged;
begin
  Changed;
end;

function TcxCustomTextEditProperties.NeedUseNullString(AEditValue: TcxEditValue): Boolean;
begin
  Result := UseNullString and VarIsNull(AEditValue);
end;

procedure TcxCustomTextEditProperties.SetCharCase(Value: TEditCharCase);
begin
  if Value <> FCharCase then
  begin
    FCharCase := Value;
    Changed;
  end;
end;

function TcxCustomTextEditProperties.UseLookupData: Boolean;
begin
  Result := GetLookupDataClass <> nil;
end;

function TcxCustomTextEditProperties.UseSearchControl: Boolean;
begin
  Result := not IsLookupEdit and IncrementalFiltering and (EditingStyle in [esFixedList, esEditList]);
end;

function TcxCustomTextEditProperties.GetAssignedValues: TcxTextEditPropertiesValues;
begin
  Result := TcxTextEditPropertiesValues(FAssignedValues);
end;

function TcxCustomTextEditProperties.GetDefaultIncrementalFilteringOptions: TcxTextEditIncrementalFilteringOptions;
begin
  Result := [ifoHighlightSearchText, ifoUseContainsOperator];
end;

function TcxCustomTextEditProperties.GetDisplayFormat: string;
begin
  if AssignedValues.DisplayFormat then
    Result := FDisplayFormat
  else
    if (IDefaultValuesProvider = nil) or
        (IDefaultValuesProvider.DefaultDisplayFormat = '') then
      Result := GetDefaultDisplayFormat
    else
      Result := IDefaultValuesProvider.DefaultDisplayFormat;
end;

function TcxCustomTextEditProperties.GetEditFormat: string;
var
  A: Boolean;
begin
  Result := InternalGetEditFormat(A, A);
end;

function TcxCustomTextEditProperties.GetLookupItems: TStrings;
begin
  Result := FLookupItems;
end;

function TcxCustomTextEditProperties.GetLookupItemsSorted: Boolean;
begin
  Result := FLookupItems.Sorted;
end;

function TcxCustomTextEditProperties.GetMaxLength: Integer;
begin
  if AssignedValues.MaxLength then
    Result := FMaxLength
  else
    Result := GetDefaultMaxLength;
end;

function TcxCustomTextEditProperties.GetViewStyle: TcxTextEditViewStyle;
const
  AViewStyleMap: array[TcxEditButtonsViewStyle] of TcxTextEditViewStyle =
    (vsNormal, vsButtonsOnly, vsButtonsAutoWidth);
begin
  if ButtonsViewStyle <> bvsNormal then
    Result := AViewStyleMap[ButtonsViewStyle]
  else
    if HideCursor then
      Result := vsHideCursor
    else
      Result := vsNormal;
end;

function TcxCustomTextEditProperties.IsDisplayFormatStored: Boolean;
begin
  Result := AssignedValues.DisplayFormat;
end;

function TcxCustomTextEditProperties.IsEditFormatStored: Boolean;
begin
  Result := AssignedValues.EditFormat;
end;

function TcxCustomTextEditProperties.IsMaxLengthStored: Boolean;
begin
  Result := AssignedValues.MaxLength;
end;

procedure TcxCustomTextEditProperties.LookupItemsChanged(Sender: TObject);
begin
  Changed;
end;

procedure TcxCustomTextEditProperties.ReadIsDisplayFormatAssigned(Reader: TReader);
begin
  AssignedValues.DisplayFormat := Reader.ReadBoolean;
end;

procedure TcxCustomTextEditProperties.SetDisplayFormat(const Value: string);
begin
  if AssignedValues.DisplayFormat and (Value = FDisplayFormat) then
    Exit;

  AssignedValues.FDisplayFormat := True;
  FDisplayFormat := Value;
  Changed;
end;

// obsolete
procedure TcxCustomTextEditProperties.SetAssignedValues(
  Value: TcxTextEditPropertiesValues);
begin
  FAssignedValues.Assign(Value);
end;

procedure TcxCustomTextEditProperties.SetEchoMode(Value: TcxEditEchoMode);
begin
  if Value <> FEchoMode then
  begin
    FEchoMode := Value;
    Changed;
  end;
end;

procedure TcxCustomTextEditProperties.SetEditFormat(const Value: string);
begin
  if AssignedValues.EditFormat and (Value = FEditFormat) then
    Exit;

  AssignedValues.FEditFormat := True;
  FEditFormat := Value;
  Changed;
end;

procedure TcxCustomTextEditProperties.SetFixedListSelection(Value: Boolean);
begin
  if Value <> FFixedListSelection then
  begin
    FFixedListSelection := Value;
    if EditingStyle = esFixedList then
      Changed;
  end;
end;

procedure TcxCustomTextEditProperties.SetHideCursor(Value: Boolean);
begin
  if Value <> FHideCursor then
  begin
    FHideCursor := Value;
    Changed;
  end;
end;

procedure TcxCustomTextEditProperties.SetHideSelection(Value: Boolean);
begin
  if Value <> FHideSelection then
  begin
    FHideSelection := Value;
    Changed;
  end;
end;

procedure TcxCustomTextEditProperties.SetImeMode(Value: TImeMode);
begin
  if FImeMode <> Value then
  begin
    FImeMode := Value;
    Changed;
  end;
end;

procedure TcxCustomTextEditProperties.SetImeName(const Value: TImeName);
begin
  if FImeName <> Value then
  begin
    FImeName := Value;
    Changed;
  end;
end;

procedure TcxCustomTextEditProperties.SetIncrementalFilteringOptions(Value: TcxTextEditIncrementalFilteringOptions);
begin
  FIncrementalFilteringOptions := Value;
end;

procedure TcxCustomTextEditProperties.SetIncrementalSearch(Value: Boolean);
begin
  if Value <> FIncrementalSearch then
  begin
    FIncrementalSearch := Value;
    Changed;
  end;
end;

procedure TcxCustomTextEditProperties.SetLookupItems(Value: TStrings);
begin
  FLookupItems.Assign(Value);
end;

procedure TcxCustomTextEditProperties.SetLookupItemsSorted(Value: Boolean);
begin
  FLookupItems.Sorted := Value;
end;

procedure TcxCustomTextEditProperties.SetMaxLength(Value: Integer);
begin
  if Value < 0 then
    Value := 0;
  if AssignedValues.MaxLength and (Value = FMaxLength) then
    Exit;

  AssignedValues.FMaxLength := True;
  FMaxLength := Value;
  MaxLengthChanged;
end;

procedure TcxCustomTextEditProperties.SetMRUMode(Value: Boolean);
begin
  if Value <> FMRUMode then
  begin
    FMRUMode := Value;
    Changed;
  end;
end;

procedure TcxCustomTextEditProperties.SetUseNullString(const Value: Boolean);
begin
  if FUseNullString <> Value then
  begin
    FUseNullString := Value;
    Changed;
  end;
end;

procedure TcxCustomTextEditProperties.SetNullstring(
  const Value: string);
begin
  if FNullstring <> Value then
  begin
    FNullstring := Value;
    Changed;
  end;
end;

procedure TcxCustomTextEditProperties.SetOEMConvert(Value: Boolean);
begin
  if Value <> FOEMConvert then
  begin
    FOEMConvert := Value;
    Changed;
  end;
end;

procedure TcxCustomTextEditProperties.SetPasswordChar(Value: TCaptionChar);
begin
  if Value <> FPasswordChar then
  begin
    FPasswordChar := Value;
    Changed;
  end;
end;

procedure TcxCustomTextEditProperties.SetUseDisplayFormatWhenEditing(
  Value: Boolean);
begin
  if Value <> FUseDisplayFormatWhenEditing then
  begin
    FUseDisplayFormatWhenEditing := Value;
    Changed;
  end;
end;

procedure TcxCustomTextEditProperties.SetViewStyle(Value: TcxTextEditViewStyle);
const
  AButtonsViewStyleMap: array[TcxTextEditViewStyle] of TcxEditButtonsViewStyle =
    (bvsNormal, bvsNormal, bvsButtonsOnly, bvsButtonsAutoWidth);
begin
  if Value <> ViewStyle then
  begin
    BeginUpdate;
    try
      ButtonsViewStyle := AButtonsViewStyleMap[Value];
      HideCursor := Value <> vsNormal;
    finally
      EndUpdate;
    end;
  end;
end;

{ TcxCustomTextEdit }

constructor TcxCustomTextEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TcxCustomTextEdit.Destroy;
begin
  FText := Text;
  cxFormatController.RemoveListener(Self);
  FreeAndNil(FLookupData);
  inherited Destroy;
end;

procedure TcxCustomTextEdit.DefaultHandler(var Message);
var
  AMessage: TMessage;
begin
  AMessage := TMessage(Message);
  if AMessage.Msg <> WM_CONTEXTMENU then
    inherited;
end;

class function TcxCustomTextEdit.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxCustomTextEditProperties;
end;

procedure TcxCustomTextEdit.CopyToClipboard;
begin
  if ActiveProperties.EditingStyle in [esFixedList, esNoEdit] then
    SelectAll;
  if SelLength > 0 then
    InnerTextEdit.CopyToClipboard;
end;

procedure TcxCustomTextEdit.CutToClipboard;
var
  ANewSelStart: Integer;
  ANewText, S: string;
begin
  if SelLength = 0 then
    Exit;
  if Focused then
  begin
    BeginUserAction;
    try
      S := '';
      if CanChangeSelText(S, ANewText, ANewSelStart) then
        InnerEdit.CallDefWndProc(WM_CUT, 0, 0);
    finally
      EndUserAction;
    end;
  end
  else
  begin
    InnerTextEdit.CopyToClipboard;
    SelText := '';
  end;
end;

function TcxCustomTextEdit.IsEditClass: Boolean;
begin
  Result := (ActiveProperties.EditingStyle in [esEdit, esEditList])
    and not PropertiesChangeLocked;
end;

procedure TcxCustomTextEdit.PasteFromClipboard;
var
  ANewSelStart: Integer;
  ANewText, S: string;
begin
  if Clipboard.HasFormat(CF_TEXT) then
    if Focused then
    begin
      BeginUserAction;
      try
        S := Clipboard.AsText;
        if CanChangeSelText(S, ANewText, ANewSelStart) then
          InnerEdit.CallDefWndProc(WM_PASTE, 0, 0);
      finally
        EndUserAction;
      end;
    end
    else
      SelText := Clipboard.AsText;
end;

procedure TcxCustomTextEdit.PrepareEditValue(const ADisplayValue: TcxEditValue;
  out EditValue: TcxEditValue; AEditFocused: Boolean);
begin
  EditValue := ADisplayValue;
end;

procedure TcxCustomTextEdit.SelectAll;
begin
  InnerTextEdit.SelectAll;
end;

function TcxCustomTextEdit.GetTextBaseLine: Integer;
begin
  Result := ViewInfo.GetTextBaseLine;
end;

function TcxCustomTextEdit.HasTextBaseLine: Boolean;
begin
  Result := True;
end;

procedure TcxCustomTextEdit.ClearSelection;
var
  APrevSelStart: Integer;
  AText: string;
begin
  if SelLength = 0 then
    Exit;
  AText := DisplayText;
  Delete(AText, SelStart + 1, SelLength);
  APrevSelStart := SelStart;
  if SetDisplayText(AText) then
    SelStart := APrevSelStart;
end;

function TcxCustomTextEdit.DoInnerControlDefaultHandler(var Message: TMessage): Boolean;
begin
  InternalSpellCheckerHandler(Message);
  Result := inherited DoInnerControlDefaultHandler(Message);
end;

function TcxCustomTextEdit.IsChildWindow(AWnd: THandle): Boolean;
begin
  Result := inherited IsChildWindow(AWnd) or
    (Assigned(TdxSpellCheckerInstance.ISpellChecker) and TdxSpellCheckerInstance.ISpellChecker.IsSpellCheckerDialogControl(AWnd));
end;

procedure TcxCustomTextEdit.SetScrollBarsParameters(AIsScrolling: Boolean = False);
begin
  inherited SetScrollBarsParameters(AIsScrolling);
  InternalCheckSelection;
end;

procedure TcxCustomTextEdit.SetSelection(ASelStart: Integer; ASelLength: Integer);
begin
  SelStart := ASelStart;
  SelLength := ASelLength;
end;

procedure TcxCustomTextEdit.Undo;
begin
  Reset;
end;

procedure TcxCustomTextEdit.AdjustInnerEditPosition;
var
  AInnerEditBounds: TRect;
  AInnerEditHeight, AInnerEditTop: Integer;
  R: TRect;
begin
  if (InnerTextEdit = nil) or FInnerEditPositionAdjusting then
    Exit;
  FInnerEditPositionAdjusting := True;
  try
    AInnerEditHeight := GetInnerEditHeight;
    AInnerEditTop := 0;
    R := ViewInfo.ClientRect;
    case TcxCustomTextEditProperties(ActiveProperties).Alignment.Vert of
      taTopJustify:
        AInnerEditTop := R.Top + ContentParams.Offsets.Top;
      taBottomJustify:
        AInnerEditTop := R.Bottom - AInnerEditHeight - ContentParams.Offsets.Bottom;
      taVCenter:
        AInnerEditTop := R.Top + ContentParams.Offsets.Top + (R.Bottom - R.Top - AInnerEditHeight - ContentParams.Offsets.Top - ContentParams.Offsets.Bottom) div 2;
    end;
    if AInnerEditTop < R.Top + ContentParams.Offsets.Top then
      AInnerEditTop := R.Top + ContentParams.Offsets.Top;
    with ContentParams.Offsets do
      AInnerEditBounds := Rect(R.Left + Left, AInnerEditTop,
        R.Right - R.Left + 1 - (Left + Right), AInnerEditHeight);
    with AInnerEditBounds do
      if not EqualRect(InnerEdit.Control.BoundsRect, Rect(Left, Top, Left + Right, Top + Bottom)) then
        InnerEdit.Control.SetBounds(Left, Top, Right, Bottom);
    if IsInplace then
    begin
      Inc(R.Top);
      Dec(R.Bottom);
    end;
    if not IsInplace and (ViewInfo.NativeState <> TC_NONE) and
      ViewInfo.ComboBoxStyle and (ActiveProperties.EditingStyle in [esEdit, esEditList]) then
    begin
      Dec(R.Right);
      Dec(R.Bottom);
    end;
    AlignControls(InnerEdit.Control, R);
  finally
    FInnerEditPositionAdjusting := False;
  end;
end;

function TcxCustomTextEdit.CanKeyDownModifyEdit(Key: Word; Shift: TShiftState): Boolean;
begin
  Result := inherited CanKeyDownModifyEdit(Key, Shift);
  Result := Result or (Key = VK_DELETE);
  if ActiveProperties.UseLookupData and not ILookupData.IsEmpty and
    GetScrollLookupDataList(escKeyboard) then
      case Key of
        VK_DOWN, VK_UP:
          Result := not(ssAlt in Shift) and not HasPopupWindow;
        VK_PRIOR, VK_NEXT:
          Result := not HasPopupWindow;
      end;
end;

function TcxCustomTextEdit.CanKeyPressModifyEdit(Key: Char): Boolean;
begin
  Result := (Ord(Key) >= 32) or (Key = #1) or (Key = #8) or (Key = #22) or (Key = #24);
end;

procedure TcxCustomTextEdit.ChangeHandler(Sender: TObject);
var
  ANeedSaveModified: Boolean;
begin
  LockChangeEvents(True);
  try
    ANeedSaveModified := FIsDisplayValueSynchronizing;
    if ANeedSaveModified then
      SaveModified;
    try
      inherited ChangeHandler(Sender);
      if not ViewInfo.IsEditClass then
        UpdateDrawValue;
      if Focused and ActiveProperties.IsEditValueNumeric and
        ActiveProperties.HasDigitGrouping(FIsDisplayValueSynchronizing) then
          SeparateDigitGroups(Self);
    finally
      if ANeedSaveModified then
        RestoreModified;
    end;
  finally
    LockChangeEvents(False);
  end;
end;

procedure TcxCustomTextEdit.ContainerStyleChanged(Sender: TObject);

  function NeedInnerEditRecreateWnd: Boolean;
  begin
    Result := not IsDestroying and not FIsCreating and (ActiveProperties.PasswordChar = #0) and
      (ActiveProperties.EchoMode = eemPassword);
  end;

begin
  if HasInnerEdit and NeedInnerEditRecreateWnd then
    cxRecreateControlWnd(InnerControl);
  inherited;
end;

procedure TcxCustomTextEdit.DoEditKeyDown(var Key: Word; Shift: TShiftState);

  procedure ProcessLookupNavigationKey(var Key: Word; Shift: TShiftState);
  var
    APrevCurrentKey: TcxEditValue;
  begin
    if ((Key <> VK_HOME) and (Key <> VK_END) or (ActiveProperties.EditingStyle = esFixedList)) and
      ActiveProperties.UseLookupData and GetScrollLookupDataList(escKeyboard) and
      not ILookupData.IsEmpty then
    begin
      LockChangeEvents(True);
      LookupItemsScrolling := True;
      try
        APrevCurrentKey := ILookupData.CurrentKey;
        LockClick(True);
        try
          case Key of
            VK_PRIOR:
              if ssCtrl in Shift then
                ILookupData.Go(egdBegin, False)
              else
                ILookupData.Go(egdPageUp, False);
            VK_NEXT:
              if ssCtrl in Shift then
                ILookupData.Go(egdEnd, False)
              else
                ILookupData.Go(egdPageDown, False);
            VK_UP:
              ILookupData.Go(egdPrev, False);
            VK_DOWN:
              ILookupData.Go(egdNext, False);
            VK_HOME:
              ILookupData.Go(egdBegin, False);
            VK_END:
              ILookupData.Go(egdEnd, False);
          end;
        finally
          LockClick(False);
        end;
        if not VarEqualsExact(APrevCurrentKey, ILookupData.CurrentKey) then
        begin
          DoClick;
          if CanPostEditValue and ActiveProperties.ImmediatePost and InternalValidateEdit then
            InternalPostEditValue;
        end;
        Key := 0;
      finally
        LookupItemsScrolling := False;
        LockChangeEvents(False);
      end;
    end;
  end;

var
  AEditingStyle: TcxEditEditingStyle;
  AFindSelection: Boolean;
  APrevKey: Word;
begin
  AEditingStyle := ActiveProperties.EditingStyle;
  if (AEditingStyle = esFixedList) and not IsSpecialKey(Key, Shift) then
    case Key of
      VK_LEFT, VK_RIGHT, VK_DELETE:
        begin
          FindSelection := False;
          DoAfterKeyDown(Key, Shift);
          Key := 0;
        end;
    end;

  InnerTextEdit.InternalUpdating := True;
  APrevKey := Key;
  AFindSelection := FindSelection;
  inherited DoEditKeyDown(Key, Shift);
  if (TranslateKey(APrevKey) = VK_RETURN) and CanSelectItem(AFindSelection) then
  begin
    ILookupData.SelectItem;
    SelectAll;
  end;
  if Key = 0 then
    Exit;

  case Key of
    VK_LEFT, VK_RIGHT:
      FindSelection := False;
    VK_DELETE:
      begin
        if AEditingStyle = esEditList then
        begin
          DoAfterKeyDown(Key, Shift);
          Key := 0;
        end
        else
          FindSelection := False;
      end
  else
    if IsLookupNavigationKey(Key, Shift) then
    begin
      ProcessLookupNavigationKey(Key, Shift);
      if (Key <> VK_HOME) and (Key <> VK_END) and not InnerTextEdit.MultiLine then
      begin
        if Key <> 0 then
          DoAfterKeyDown(Key, Shift);
        Key := 0;
      end;
    end;
  end;
  if (Key = VK_END) and (SelLength = 0) and (AEditingStyle <> esFixedList) then
    FindSelection := False;

  if Key <> 0 then
    InnerTextEdit.InternalUpdating := False;
end;

procedure TcxCustomTextEdit.DoEditKeyPress(var Key: Char);

  function CanContinueIncrementalSearch: Boolean;
  begin
    Result := ActiveProperties.EditingStyle in [esEditList, esFixedList];
    if not Result then
      Result := (SelLength = 0) and (SelStart = Length(DisplayText)) or
        FindSelection or (SelLength > 0);
  end;

var
  AEditingStyle: TcxEditEditingStyle;
  AFindText: string;
  AFound: Boolean;
  APrevCurrentKey: TcxEditValue;
  APrevFindSelection: Boolean;
begin
  InnerTextEdit.InternalUpdating := True;
  inherited DoEditKeyPress(Key);
  if Key = #0 then
    Exit;

  UnlockLookupDataTextChanged;
  BeginUserAction;
  AEditingStyle := ActiveProperties.EditingStyle;
  if AEditingStyle = esFixedList then
    case Key of
      #8:
        if not ActiveProperties.FixedListSelection then
        begin
          Key := #0;
          FindSelection := False;
        end;
    end;

  APrevCurrentKey := ILookupData.CurrentKey;
  APrevFindSelection := FindSelection;
  AFound := False;
  LockClick(True);
  try
    if Key = #8 then
    begin
      if ActiveProperties.UseLookupData and ActiveProperties.CanIncrementalSearch then
      begin
        if (AEditingStyle = esEditList) and (Length(DisplayText) > 0) and not FindSelection then
        begin
          SelLength := Length(DisplayText) - SelStart;
          FindSelection := True;
        end;
        if FindSelection then
        begin
          if ActiveProperties.IsLookupEdit and (ActiveProperties.IncrementalFiltering) and
             (ActiveProperties.EditingStyle = esFixedList) and
             (ifoUseContainsOperator in ActiveProperties.IncrementalFilteringOptions) then
            AFindText := Copy(DisplayText, SelStart + 1, SelLength)
          else
            AFindText := Copy(DisplayText, 1, Length(DisplayText) - SelLength);
          SetLength(AFindText, Length(AFindText) - Length(AnsiLastChar(AFindText)));
          LockLookupDataTextChanged;
          AFound := PopulateFromList(AFindText);
        end;
        if AEditingStyle = esFixedList then
          Key := #0;
      end;
    end
    else
      if IsTextChar(Key) then
      begin
        if ActiveProperties.UseLookupData then
        begin
          if ActiveProperties.CanIncrementalSearch and CanContinueIncrementalSearch then
          begin
            LockLookupDataTextChanged;
            AFound := False;
            AFindText := DisplayText;
            if SelLength > 0 then
              if ActiveProperties.IsLookupEdit and (ActiveProperties.IncrementalFiltering) and
                 (ActiveProperties.EditingStyle = esFixedList) and
                 (ifoUseContainsOperator in ActiveProperties.IncrementalFilteringOptions) then
                AFindText := Copy(AFindText, SelStart + 1, SelLength) + Key
              else
                AFindText := Copy(AFindText, 1, SelStart) + Key
            else
              if AEditingStyle = esFixedList then
                if FindSelection then
                begin
                  AFindText := AFindText + Key;
                  AFound := PopulateFromList(AFindText);
                  if not AFound then
                    AFindText := Key;
                end
                else
                  AFindText := Key
              else
                Insert(Key, AFindText, SelStart + 1);
            if not AFound then
              AFound := PopulateFromList(AFindText);
            if (AEditingStyle = esFixedList) and not ActiveProperties.FixedListSelection and not AFound then
            begin
              AFindText := Key;
              AFound := PopulateFromList(AFindText);
            end;
          end;
          if (AEditingStyle in [esEditList, esFixedList]) and not AFound then
          begin
            Key := #0;
            if (AEditingStyle = esEditList) and (DisplayText <> '') or
                (AEditingStyle = esFixedList) and ActiveProperties.FixedListSelection and APrevFindSelection then
              FindSelection := True;
          end;
        end;
      end;
  finally
    LockClick(False);
    EndUserAction;
    if ActiveProperties.UseLookupData and not VarEqualsExact(APrevCurrentKey,
      ILookupData.CurrentKey) then
        DoClick;
  end;
  if AFound then
    Key := #0;
  if Key <> #0 then
    InnerTextEdit.InternalUpdating := False;
end;

procedure TcxCustomTextEdit.DoEditValueChanged;
begin
  inherited DoEditValueChanged;
  DoTextChanged;
end;

procedure TcxCustomTextEdit.DoExit;
begin
  inherited DoExit;
  FindSelection := False;
end;

procedure TcxCustomTextEdit.DoFocusChanged;
begin
  inherited;
  if not ActiveProperties.IsMultiLine then
    InnerControl.Invalidate;
end;

function TcxCustomTextEdit.DoRefreshContainer(const P: TPoint; Button: TcxMouseButton;
  Shift: TShiftState; AIsMouseEvent: Boolean): Boolean;
begin
  Result := inherited DoRefreshContainer(P, Button, Shift, AIsMouseEvent);
  if Result then
    AdjustInnerEdit;
end;

procedure TcxCustomTextEdit.DoSetFocusWhenActivate;
begin
  SelStart := 0;
  inherited;
end;

procedure TcxCustomTextEdit.DoValidateDisplayValue(var ADisplayValue: TcxEditValue; var AErrorText: TCaption;
  var AError: Boolean);
var
  ACurrentKeyText: string;
begin
  if ActiveProperties.CanValidate and (ActiveProperties.EditingStyle = esEdit) and
    ActiveProperties.UseLookupData and FindSelection then
  begin
    ACurrentKeyText := ILookupData.GetDisplayText(ILookupData.CurrentKey);
    if InternalCompareString(VarToStr(ADisplayValue), ACurrentKeyText, False) then
      ADisplayValue := ILookupData.GetDisplayText(ILookupData.CurrentKey);
  end;
  inherited;
end;

function TcxCustomTextEdit.PopulateFromList(var AFindText: string): Boolean;
var
  ATail: string;
  L: Integer;
  S, AFindSubstr: string;
begin
  S := AFindText;
  if InnerTextEdit.ImeLastChar <> #0 then
    S := S + InnerTextEdit.ImeLastChar;
  AFindSubstr := S;
  Result := ILookupData.Locate(S, ATail, False);
  if Result then
  begin
    AFindText := S;
    if InnerTextEdit.ImeLastChar <> #0 then
    begin
      L := Length(AFindText);
      Insert(Copy(AFindText, L, 1), ATail, 1);
      Delete(AFindText, L, 1);
    end;
  end;
  FFindSelection := Result;
  if AFindText = '' then
  begin
    if (ActiveProperties.EditingStyle <> esFixedList) or
      (ActiveProperties.IncrementalFiltering) and (ifoUseContainsOperator in ActiveProperties.IncrementalFilteringOptions) and
        not ActiveProperties.IsLookupEdit then
      InternalSetDisplayValue('');
    FFindSelection := False;
  end;
  if Result then
  begin
    DataBinding.DisplayValue := AFindText + ATail;
    if ActiveProperties.IsLookupEdit and (ActiveProperties.IncrementalFiltering) and
       (ActiveProperties.EditingStyle = esFixedList) and
       (ifoUseContainsOperator in ActiveProperties.IncrementalFilteringOptions) then
    begin
      SelStart := Pos(AFindSubstr, AFindText) - 1;
      SelLength := Length(AFindSubstr);
    end
    else
    begin
      SelStart := Length(AFindText);
      SelLength := Length(ATail);
    end;
  end;
  UpdateDrawValue;
end;

procedure TcxCustomTextEdit.PopulateSizeProperties(var AEditSizeProperties: TcxEditSizeProperties);
begin
  if ViewInfo = nil then
    Exit;
  AEditSizeProperties := cxDefaultEditSizeProperties;
  AEditSizeProperties.MaxLineCount := 1;
end;

function TcxCustomTextEdit.GetInnerControlBounds(const AInnerControlsRegion: TRect;
  AInnerControl: TControl): TcxContainerInnerControlBounds;
begin
  if IsEditClass then
    Result := inherited GetInnerControlBounds(AInnerControlsRegion, AInnerControl)
  else
  begin
    Result.IsEmpty := False;
    Result.Rect := cxEmptyRect;
  end;
end;

function TcxCustomTextEdit.GetDisplayText: string;
begin
  if InnerEdit = nil then
    Result := ''
  else
    Result := InnerEdit.EditValue;
end;

function TcxCustomTextEdit.GetInnerEditClass: TControlClass;
begin
  Result := TcxCustomInnerTextEdit;
end;

procedure TcxCustomTextEdit.Initialize;
var
  ALookupDataClass: TcxInterfacedPersistentClass;
begin
  inherited Initialize;
  FBeepOnEnter := True;
  FFindSelection := False;

  if InnerTextEdit <> nil then
  begin
    TControlAccess(InnerTextEdit.Control).Color := clWindow;
    InnerTextEdit.OnSelChange := SelChange;
  end;
  Width := 121;
  Height := 21;

  ALookupDataClass := Properties.GetLookupDataClass;
  if ALookupDataClass <> nil then
  begin
    FLookupData := ALookupDataClass.Create(Self);
    ILookupData.OnSelectItem := HandleSelectItem;
  end;

  cxFormatController.AddListener(Self);
end;

function TcxCustomTextEdit.InternalDoEditing: Boolean;
begin
  Result := ActiveProperties.EditingStyle <> esNoEdit;
end;

function TcxCustomTextEdit.InternalGetEditingValue: TcxEditValue;
begin
  Result := Text;
end;

procedure TcxCustomTextEdit.InternalSetDisplayValue(const Value: TcxEditValue);
begin
  DataBinding.DisplayValue := Value;
  if not IsUserAction then
  begin
    ResetOnNewDisplayValue;
    SynchronizeEditValue;
    EditModified := False;
  end;
end;

procedure TcxCustomTextEdit.InternalValidateDisplayValue(const ADisplayValue: TcxEditValue);
begin
  if (ActiveProperties.EditingStyle = esEdit) and ActiveProperties.UseLookupData and not ILookupData.Find(ADisplayValue) then
    DoOnNewLookupDisplayText(ADisplayValue);
  inherited InternalValidateDisplayValue(ADisplayValue);
end;

function TcxCustomTextEdit.IsLookupNavigationKey(AKey: Word; AShift: TShiftState): Boolean;
begin
  Result := AKey in LookupNavigationKeys;
end;

function TcxCustomTextEdit.IsTextInputMode: Boolean;
begin
  Result := (ActiveProperties.EchoMode = eemNormal) and ActiveProperties.CanModify;
end;

function TcxCustomTextEdit.IsValidChar(AChar: Char): Boolean;
begin
  Result := inherited IsValidChar(AChar) or (AnsiChar(AChar) in ActiveProperties.ValidChars);
end;

procedure TcxCustomTextEdit.KeyPress(var Key: Char);
begin
  if (Word(Key) = VK_ESCAPE) and IsKeyPressHandled and
      FIsPopupWindowJustClosed and not HasPopupWindow then
    FIsPopupWindowJustClosed := False;
  inherited KeyPress(Key);
end;

procedure TcxCustomTextEdit.Loaded;
begin
  inherited Loaded;
  ShortRefreshContainer(False);
end;

procedure TcxCustomTextEdit.PropertiesChanged(Sender: TObject);
var
  AIncrementalFilterIntf: IcxTextEditLookupDataIncrementalFilter;
begin
  if Supports(FLookupData, IcxTextEditLookupDataIncrementalFilter, AIncrementalFilterIntf) then  // #Ch do not use ILookupData here! T672303
    AIncrementalFilterIntf.ResetIncrementalFilter;
  if ActiveProperties.UseLookupData then
    ILookupData.PropertiesChanged;
  if ActiveProperties.UseLookupData and not FLookupDataTextChangedLocked then
    ILookupData.TextChanged;
  if not PropertiesChangeLocked and not IsEditClass then
    UpdateDrawValue;

  if InnerTextEdit = nil then
    Exit;

  ImeMode := ActiveProperties.ImeMode;
  ImeName := ActiveProperties.ImeName;

  InnerTextEdit.Alignment := ActiveProperties.Alignment.Horz;
  InnerTextEdit.UseLeftAlignmentOnEditing := ActiveProperties.UseLeftAlignmentOnEditing;
  InnerTextEdit.AutoSelect := ActiveProperties.AutoSelect and not IsInplace;
  InnerTextEdit.CharCase := ActiveProperties.FCharCase;
  InnerTextEdit.EchoMode := ActiveProperties.EchoMode;
  InnerTextEdit.HideSelection := ActiveProperties.FHideSelection;
  InnerTextEdit.ImeMode := ActiveProperties.ImeMode;
  InnerTextEdit.ImeName := ActiveProperties.ImeName;
  InnerTextEdit.MaxLength := ActiveProperties.GetMaxLength;
  InnerTextEdit.OEMConvert := ActiveProperties.OEMConvert;
  InnerTextEdit.PasswordChar := ActiveProperties.PasswordChar;

  if not IsInplaceInitializing then
  begin
    CheckEditValue;
    if not IsPosting then
      UpdateDisplayValue;
    UpdateDrawValue;
  end;

  inherited PropertiesChanged(Sender);

  if InnerTextEdit <> nil then
    InnerTextEdit.Control.Invalidate;
end;

function TcxCustomTextEdit.SetDisplayText(const Value: string): Boolean;
var
  ADisplayValue: TcxEditValue;
begin
  ADisplayValue := Value;
  Result := ActiveProperties.IsDisplayValueValid(ADisplayValue, InternalFocused);
  if Result then
    Result := not(IsUserAction and not DoEditing);
  if Result then
    InternalSetDisplayValue(ADisplayValue);
end;

procedure TcxCustomTextEdit.SetInternalDisplayValue(Value: TcxEditValue);
begin
  if InnerEdit <> nil then
    InnerEdit.EditValue := Value;
end;

function TcxCustomTextEdit.WantNavigationKeys: Boolean;
begin
  Result := True;
end;

procedure TcxCustomTextEdit.LockedInnerEditWindowProc(var Message: TMessage);
begin
  if Message.Msg = WM_SETFOCUS then
  begin
    if InnerTextEdit.AutoSelect then
      SelectAll;
  end
  else
    inherited LockedInnerEditWindowProc(Message);
end;

procedure TcxCustomTextEdit.UnlockInnerEditRepainting;
var
  APrevAutoSelect: Boolean;
begin
  inherited UnlockInnerEditRepainting;
  APrevAutoSelect := InnerTextEdit.AutoSelect;
  InnerTextEdit.AutoSelect := False;
  SendMessage(InnerEdit.Control.Handle, WM_SETFOCUS, 0, 0);
  InnerTextEdit.AutoSelect := APrevAutoSelect;
end;

procedure TcxCustomTextEdit.WndProc(var Message: TMessage);
var
  ASaveHideSelection: Boolean;
begin
  if (Message.Msg = DXM_SPELL_AUTOCORRECT) and (InnerControl <> nil) and
    InnerControl.HandleAllocated then
  begin
    FIsChangeBySpellChecker := True;
    try
      with PdxSpellCheckerAutoCorrectWordRange(Message.LParam)^ do
      begin
        ASaveHideSelection := ActiveProperties.HideSelection;
        ActiveProperties.HideSelection := True;
        try
          Self.SelStart := SelStart;
          Self.SelLength := SelLength;
          SendMessageW(InnerControl.Handle, EM_REPLACESEL, 1, LPARAM(PWideChar(Replacement)));
          Self.SelStart := NewSelStart;
        finally
          ActiveProperties.HideSelection := ASaveHideSelection;
        end;
      end;
    finally
      FIsChangeBySpellChecker := False;
    end;
  end;
  inherited WndProc(Message);
end;

procedure TcxCustomTextEdit.FormatChanged;
begin
  ActiveProperties.Changed;
  SynchronizeDisplayValue;
end;

procedure TcxCustomTextEdit.AdjustInnerEdit;
var
  AFont: TFont;
begin
  if (InnerTextEdit = nil) or FIsCreating then
    Exit;
  InnerEdit.LockBounds(True);
  try
    with TControlAccess(InnerTextEdit.Control) do
    begin
      Color := ViewInfo.BackgroundColor;
      AFont := TFont.Create;
      try
        AFont.Assign(VisibleFont);
        AFont.Color := ViewInfo.TextColor;
        Font := AFont;
      finally
        AFont.Free;
      end;
    end;
  finally
    InnerEdit.LockBounds(False);
  end;
end;

function TcxCustomTextEdit.CanChangeSelText(const Value: string;
  out ANewText: string; out ANewSelStart: Integer): Boolean;
var
  ADisplayValue: TcxEditValue;
  AEditingStyle: TcxEditEditingStyle;
  AValue: string;
begin
  AValue := string(PChar(Value));
  Result := False;
  AEditingStyle := ActiveProperties.EditingStyle;
  if IsUserAction and (AEditingStyle = esNoEdit) then
    Exit;
  if AEditingStyle in [esFixedList, esNoEdit] then
    SelectAll;
  if IsUserAction and (AEditingStyle in [esEdit, esNoEdit]) and (ActiveProperties.MaxLength > 0) then
  begin
    ANewText := Copy(Text, 1, SelStart) + AValue;
    ANewSelStart := Length(WideString(ANewText));
    if ANewSelStart > ActiveProperties.MaxLength then
      ANewSelStart := ActiveProperties.MaxLength;
    ANewSelStart := Length(string(Copy(WideString(ANewText), 1, ANewSelStart)));
    if ANewSelStart < SelStart then
      Exit;

    ANewText := ANewText + Copy(Text, SelStart + SelLength + 1, Length(Text) - SelStart - SelLength);
    if Length(WideString(ANewText)) > ActiveProperties.MaxLength then
      ANewText := Copy(WideString(ANewText), 1, ActiveProperties.MaxLength);
  end else
  begin
    if ActiveProperties.EditingStyle <> esFixedList then
    begin
      ANewText := Text;
      ANewText := Copy(ANewText, 1, SelStart) + AValue +
        Copy(ANewText, SelStart + SelLength + 1, Length(ANewText) - SelLength - SelStart);
    end
    else
      ANewText := AValue;
    ANewSelStart := SelStart + Length(AValue);
  end;
  ADisplayValue := ANewText;
  Result := ActiveProperties.IsDisplayValueValid(ADisplayValue, InternalFocused) and
    DoEditing;
  if Result then
    ANewText := VarToStr(ADisplayValue);
end;

function TcxCustomTextEdit.CanSelectItem(AFindSelection: Boolean): Boolean;
begin
  Result := AFindSelection;
end;

procedure TcxCustomTextEdit.CheckEditValue;
begin
  if DataBinding.CanCheckEditorValue and ActiveProperties.IsEditValueNumeric and
    not PropertiesChangeLocked and ActiveProperties.IsValueBoundsDefined then
      CheckEditorValueBounds;
end;

procedure TcxCustomTextEdit.CheckEditorValueBounds;
begin
end;

procedure TcxCustomTextEdit.DoOnNewLookupDisplayText(const AText: string);
begin
  with Properties do
    if Assigned(OnNewLookupDisplayText) then
      OnNewLookupDisplayText(Self, AText);
  if RepositoryItem <> nil then
    with ActiveProperties do
      if Assigned(OnNewLookupDisplayText) then
        OnNewLookupDisplayText(Self, AText);
end;

function TcxCustomTextEdit.CanSpellCheckerPostEditValue: Boolean;
begin
  Result := not IsInplace and CanPostEditValue and not Focused;
end;

procedure TcxCustomTextEdit.DoDrawMisspellings;
begin
  if Assigned(TdxSpellCheckerInstance.ISpellChecker) then
    TdxSpellCheckerInstance.ISpellChecker.DrawMisspellings(InnerControl);
end;

procedure TcxCustomTextEdit.DoSelectionChanged;
begin
  if Assigned(TdxSpellCheckerInstance.ISpellChecker) then
    TdxSpellCheckerInstance.ISpellChecker.SelectionChanged(InnerControl);
end;

procedure TcxCustomTextEdit.DoSpellCheckerPostEditValue;
begin
  SpellCheckerSetValue(DisplayText);
end;

procedure TcxCustomTextEdit.DoLayoutChanged;
begin
  FLastFirstVisibleCharIndex := InnerTextEdit.GetFirstVisibleCharIndex;
  if Assigned(TdxSpellCheckerInstance.ISpellChecker) then
    TdxSpellCheckerInstance.ISpellChecker.LayoutChanged(InnerControl);
end;

procedure TcxCustomTextEdit.DoTextChanged;
begin
  if Assigned(TdxSpellCheckerInstance.ISpellChecker) then
    TdxSpellCheckerInstance.ISpellChecker.TextChanged(InnerControl);
end;

procedure TcxCustomTextEdit.InternalCheckSelection;
begin
  if InnerControl.HandleAllocated then
  begin
    if FLastFirstVisibleCharIndex <> InnerTextEdit.GetFirstVisibleCharIndex then
      DoLayoutChanged
    else
      if (InnerTextEdit.SelStart <> FLastSelPosition) or
        (InnerTextEdit.SelLength <> FLastSelLength) then
      begin
        FLastSelPosition := InnerTextEdit.SelStart;
        FLastSelLength := InnerTextEdit.SelLength;
        DoSelectionChanged;
      end;
  end;
end;

procedure TcxCustomTextEdit.InternalSpellCheckerHandler(var Message: TMessage);
begin
  case Message.Msg of
    WM_SETFOCUS:
      if Assigned(TdxSpellCheckerInstance.ISpellChecker) then
        TdxSpellCheckerInstance.ISpellChecker.CheckStart(InnerControl);
    WM_KILLFOCUS:
      if Assigned(TdxSpellCheckerInstance.ISpellChecker) then
        TdxSpellCheckerInstance.ISpellChecker.CheckFinish;
    WM_SETFONT, WM_SIZE:
      DoLayoutChanged;
    WM_LBUTTONUP, WM_MBUTTONUP, WM_RBUTTONUP, WM_KEYUP:
      InternalCheckSelection;
    WM_CTLCOLOREDIT, CN_CTLCOLOREDIT, WM_PAINT:
      RedrawMisspelledWords;
    WM_UNDO, EM_UNDO:
      if Assigned(TdxSpellCheckerInstance.ISpellChecker) then
        TdxSpellCheckerInstance.ISpellChecker.Undo;
    else
      if Message.Msg = DXM_SPELL_REDRAWMISSPELLINGS then
      begin
        dxMessagesController.KillMessages(InnerControl.Handle, DXM_SPELL_REDRAWMISSPELLINGS);
        DoDrawMisspellings;
      end;
  end;
end;

procedure TcxCustomTextEdit.RedrawMisspelledWords;
begin
  if InnerControl.HandleAllocated then
    PostMessage(InnerControl.Handle, DXM_SPELL_REDRAWMISSPELLINGS, 0, 0);
end;

procedure TcxCustomTextEdit.SpellCheckerPostEditValue;
begin
  if CanSpellCheckerPostEditValue then
  begin
    LockChangeEvents(True);
    try
      DoSpellCheckerPostEditValue;
      InternalPostEditValue;
    finally
      LockChangeEvents(False);
    end;
  end;
end;

procedure TcxCustomTextEdit.SpellCheckerSetSelText(
  const AValue: string; APost: Boolean = False);
begin
  FIsChangeBySpellChecker := True;
  try
    SelText := AValue;
    if APost then
      SpellCheckerPostEditValue;
    ModifiedAfterEnter := True;
  finally
    FIsChangeBySpellChecker := False;
  end;
end;

function TcxCustomTextEdit.GetInnerEditHeight: Integer;
begin
  Result := cxTextHeight(TControlAccess(InnerTextEdit.Control).Font);
end;

function TcxCustomTextEdit.GetItemIndex: Integer;
begin
  Result := LookupKeyToItemIndex(ILookupData.CurrentKey);
end;

function TcxCustomTextEdit.GetItemObject: TObject;
begin
  if ItemIndex <> -1 then
    Result := ActiveProperties.LookupItems.Objects[ItemIndex]
  else
    Result := nil;
end;

function TcxCustomTextEdit.GetScrollLookupDataList(AScrollCause: TcxEditScrollCause): Boolean;
begin
  Result := False;
end;

procedure TcxCustomTextEdit.HandleSelectItem(Sender: TObject);
var
  ANewEditValue: TcxEditValue;
  AEditValueChanged: Boolean;
begin
  ANewEditValue := LookupKeyToEditValue(ILookupData.CurrentKey);
  AEditValueChanged := not VarEqualsExact(EditValue, ANewEditValue);
  if AEditValueChanged and not DoEditing then
    Exit;
  SaveModified;
  LockLookupDataTextChanged;
  try
    InternalEditValue := ANewEditValue;
  finally
    UnlockLookupDataTextChanged;
    RestoreModified;
  end;
  if AEditValueChanged then
    ModifiedAfterEnter := True;
  SelectAll;
  ShortRefreshContainer(False);
end;

function TcxCustomTextEdit.InternalGetText: string;
begin
  Result := DisplayText;
end;

function TcxCustomTextEdit.InternalMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
const
  AGoDirectionMap: array [Boolean] of TcxEditLookupDataGoDirection = (egdNext, egdPrev);
var
  APrevCurrentKey: TcxEditValue;
begin
  Result := inherited InternalMouseWheel(Shift, WheelDelta, MousePos);
  if not Result then
  begin
    Result := DoIsMouseWheelHandleNeeded(Shift, MousePos);
    if Result and DoEditing then
    begin
      APrevCurrentKey := ILookupData.CurrentKey;
      LockChangeEvents(True);
      LookupItemsScrolling := True;
      try
        LockClick(True);
        try
          ILookupData.Go(AGoDirectionMap[WheelDelta > 0], False);
        finally
          LockClick(False);
          if not VarEqualsExact(APrevCurrentKey, ILookupData.CurrentKey) then
          begin
            DoClick;
            if CanPostEditValue and ActiveProperties.ImmediatePost and InternalValidateEdit then
              InternalPostEditValue;
          end;
        end;
      finally
        LookupItemsScrolling := False;
        LockChangeEvents(False);
      end;
    end;
  end;
end;

function TcxCustomTextEdit.InternalSetText(const Value: string): Boolean;
begin
  Result := SetDisplayText(Value);
end;

function TcxCustomTextEdit.IsMouseWheelHandleNeeded(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
begin
  Result := inherited InternalMouseWheel(Shift, WheelDelta, MousePos) or
    DoIsMouseWheelHandleNeeded(Shift, MousePos);
end;

function TcxCustomTextEdit.ItemIndexToLookupKey(AItemIndex: Integer): TcxEditValue;
begin
  Result := AItemIndex;
end;

procedure TcxCustomTextEdit.LockLookupDataTextChanged;
begin
  FLookupDataTextChangedLocked := True;
end;

function TcxCustomTextEdit.LookupKeyToEditValue(const AKey: TcxEditValue): TcxEditValue;
var
  AText: string;
begin
  AText := ILookupData.GetDisplayText(AKey);
  PrepareEditValue(AText, Result, False);
end;

function TcxCustomTextEdit.LookupKeyToItemIndex(const AKey: TcxEditValue): Integer;
begin
  Result := AKey;
end;

function TcxCustomTextEdit.NeedsInvokeAfterKeyDown(AKey: Word; AShift: TShiftState): Boolean;
begin
  Result := False;
end;

function TcxCustomTextEdit.NeedResetInvalidTextWhenPropertiesChanged: Boolean;
begin
  Result := not IsInplace and not DataBinding.IDefaultValuesProvider.IsDataStorage and
    not ActiveProperties.NeedUseNullString(EditValue);
end;

procedure TcxCustomTextEdit.ResetOnNewDisplayValue;
begin
  if ActiveProperties.UseLookupData then
    FindSelection := False;
end;

procedure TcxCustomTextEdit.SelChange(Sender: TObject);
begin
end;

procedure TcxCustomTextEdit.SetEditingText(const Value: TCaption);
begin
  if DoEditing then
  begin
    Text := Value;
    ModifiedAfterEnter := True;
  end;
end;

procedure TcxCustomTextEdit.SetItemIndex(Value: Integer);
var
  ANewEditValue: TcxEditValue;
  APrevItemIndex: Integer;
begin
  APrevItemIndex := ItemIndex;
  LockClick(True);
  try
    ILookupData.CurrentKey := ItemIndexToLookupKey(Value);
    ANewEditValue := LookupKeyToEditValue(ILookupData.CurrentKey);
    if not VarEqualsExact(EditValue, ANewEditValue) then
    begin
      LockLookupDataTextChanged;
      try
        EditValue := ANewEditValue;
      finally
        UnlockLookupDataTextChanged;
      end;
    end;
  finally
    LockClick(False);
  end;
  if ItemIndex <> APrevItemIndex then
  begin
    EditModified := False;
    Click;
  end;
end;

procedure TcxCustomTextEdit.SynchronizeDisplayValue;
var
  ADisplayValue, AEditValue: TcxEditValue;
  AIsEditValueValid: Boolean;
begin
  if ActiveProperties.CanValidate then
  begin
    AEditValue := EditValue;
    AIsEditValueValid := ActiveProperties.IsEditValueValid(AEditValue, InternalFocused);
    if not AIsEditValueValid and not Focused then
      try
        if VarIsDate(EditValue) then
          ADisplayValue := DateTimeToStr(EditValue)
        else
          ADisplayValue := VarToStr(EditValue)
      except
        on EVariantError do
          ADisplayValue := '';
      end
    else
      if AIsEditValueValid then
        PrepareDisplayValue(AEditValue, ADisplayValue, InternalFocused)
      else
        ADisplayValue := ActiveProperties.DefaultFocusedDisplayValue;
  end
  else
    PrepareDisplayValue(EditValue, ADisplayValue, InternalFocused);
  SaveModified;
  FIsDisplayValueSynchronizing := True;
  try
    DataBinding.DisplayValue := ADisplayValue;
  finally
    FIsDisplayValueSynchronizing := False;
    RestoreModified;
    ResetOnNewDisplayValue;
    UpdateDrawValue;
  end;
end;

procedure TcxCustomTextEdit.SynchronizeEditValue;
var
  APrevEditValue, ANewEditValue: TcxEditValue;
  ACompareEditValue, AEditValueChanged: Boolean;
begin
  ACompareEditValue := ActiveProperties.CanCompareEditValue;
  if ACompareEditValue then
    APrevEditValue := EditValue
  else
    APrevEditValue := Null;
  PrepareEditValue(DisplayText, ANewEditValue, InternalFocused);
  InternalStoreEditValue(ANewEditValue);

  if ACompareEditValue then
    AEditValueChanged := not InternalVarEqualsExact(APrevEditValue, ANewEditValue)
  else
    AEditValueChanged := False;
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

procedure TcxCustomTextEdit.UndoPerformed;
begin
end;

procedure TcxCustomTextEdit.UnlockLookupDataTextChanged;
begin
  FLookupDataTextChangedLocked := False;
end;

procedure TcxCustomTextEdit.UpdateDrawValue;

  procedure SetTextSelection;
  var
    AEditingStyle: TcxEditEditingStyle;
  begin
    AEditingStyle := ActiveProperties.EditingStyle;
    if AEditingStyle in [esFixedList, esNoEdit] then
      with ViewInfo do
        if (AEditingStyle = esNoEdit) or not FindSelection or not ActiveProperties.FixedListSelection then
          SelLength := 0
        else
        begin
          if DrawSelectionBar then
          begin
            SelStart := 0;
            SelLength := Length(Text) - Self.SelLength;
            SelBackgroundColor := clHighlightText;
            SelTextColor := clHighlight;
          end else
          begin
            SelStart := Self.SelStart;
            SelLength := Self.SelLength;
            SelBackgroundColor := clHighlight;
            SelTextColor := clHighlightText;
          end;
        end;
  end;

var
  AViewData: TcxCustomTextEditViewData;
begin
  AViewData := CreateViewData as TcxCustomTextEditViewData;
  try
    AViewData.DisplayValueToDrawValue(DisplayText, ViewInfo);
  finally
    AViewData.Free;
  end;
  if HandleAllocated then
  begin
    CalculateViewInfo(False);
    SetTextSelection;
    InvalidateRect(Rect(0, 0, Width, Height), False);
  end;
end;

procedure TcxCustomTextEdit.UpdateDisplayValue;
var
  ADisplayValue: TcxEditValue;
begin
  if PropertiesChangeLocked then
    Exit;

  if ActiveProperties.EditingStyle in [esEditList, esFixedList] then
  begin
    if ModifiedAfterEnter and not IsEditValidated or NeedResetInvalidTextWhenPropertiesChanged then
    begin
      ADisplayValue := DisplayText;
      if not ActiveProperties.IsDisplayValueValid(ADisplayValue, True) then
      begin
        SaveModified;
        DataBinding.DisplayValue := ActiveProperties.DefaultFocusedDisplayValue;
        RestoreModified;
        if not ModifiedAfterEnter then
          SynchronizeEditValue;
      end;
    end
    else
      SynchronizeDisplayValue;
  end
  else
    if not ModifiedAfterEnter then
      DataBinding.UpdateDisplayValue;
end;

function TcxCustomTextEdit.DoIsMouseWheelHandleNeeded(Shift: TShiftState;
  MousePos: TPoint): Boolean;
begin
  Result := GetScrollLookupDataList(escMouseWheel) and not HasPopupWindow and
    ActiveProperties.UseLookupData and not ILookupData.IsEmpty and HandleMouseWheel(Shift);
end;

function TcxCustomTextEdit.GetCursorPos: Integer;
begin
  if InnerTextEdit.Control is TcxCustomInnerTextEdit then
    Result := TcxCustomInnerTextEdit(InnerTextEdit.Control).CursorPos
  else
    Result := 0;
end;

function TcxCustomTextEdit.GetEditingText: TCaption;
begin
  Result := Text;
end;

function TcxCustomTextEdit.GetInnerTextEdit: IcxInnerTextEdit;
begin
  Result := InnerEdit as IcxInnerTextEdit;
end;

function TcxCustomTextEdit.GetILookupData: IcxTextEditLookupData;
begin
  Result := FLookupData as IcxTextEditLookupData;
end;

function TcxCustomTextEdit.GetLookupData: TcxCustomTextEditLookupData;
begin
  Result := TcxCustomTextEditLookupData(FLookupData);
end;

function TcxCustomTextEdit.GetProperties: TcxCustomTextEditProperties;
begin
  Result := TcxCustomTextEditProperties(inherited Properties);
end;

function TcxCustomTextEdit.GetActiveProperties: TcxCustomTextEditProperties;
begin
  Result := TcxCustomTextEditProperties(InternalGetActiveProperties);
end;

function TcxCustomTextEdit.GetSelLength: Integer;
var
  AEditingStyle: TcxEditEditingStyle;
begin
  AEditingStyle := ActiveProperties.EditingStyle;
  if (AEditingStyle = esFixedList) and not FindSelection or (AEditingStyle = esNoEdit) then
    Result := 0
  else
    Result := InnerTextEdit.SelLength;
end;

function TcxCustomTextEdit.GetSelStart: Integer;
var
  AEditingStyle: TcxEditEditingStyle;
begin
  AEditingStyle := ActiveProperties.EditingStyle;
  if (AEditingStyle = esFixedList) and not FindSelection or (AEditingStyle = esNoEdit) then
    Result := 0
  else
    Result := InnerTextEdit.SelStart;
end;

function TcxCustomTextEdit.GetSelText: TCaption;
var
  AEditingStyle: TcxEditEditingStyle;
begin
  AEditingStyle := ActiveProperties.EditingStyle;
  if (AEditingStyle = esFixedList) and not FindSelection or (AEditingStyle = esNoEdit) then
    Result := ''
  else
    Result := InnerTextEdit.SelText;
end;

function TcxCustomTextEdit.GetViewInfo: TcxCustomTextEditViewInfo;
begin
  Result := TcxCustomTextEditViewInfo(FViewInfo);
end;

procedure TcxCustomTextEdit.SetFindSelection(Value: Boolean);
begin
  if not HandleAllocated or (Value = FindSelection) or FDisableRefresh then
    Exit;
  FFindSelection := Value;
  CalculateViewInfo(False);
  UpdateDrawValue;
end;

procedure TcxCustomTextEdit.SetItemObject(Value: TObject);
begin
  ItemIndex := ActiveProperties.LookupItems.IndexOfObject(Value);
end;

procedure TcxCustomTextEdit.SetProperties(Value: TcxCustomTextEditProperties);
begin
  Properties.Assign(Value);
end;

procedure TcxCustomTextEdit.SetSelLength(Value: Integer);
begin
  if ActiveProperties.EditingStyle <> esNoEdit then
    InnerTextEdit.SelLength := Value;
end;

procedure TcxCustomTextEdit.SetSelStart(Value: Integer);
begin
  if ActiveProperties.EditingStyle <> esNoEdit then
    InnerTextEdit.SelStart := Value;
end;

function TcxCustomTextEdit.GetTextHint: string;
begin
  Result := InnerTextEdit.TextHint;
end;

procedure TcxCustomTextEdit.SetTextHint(Value: string);
begin
  InnerTextEdit.TextHint := Value;
end;


procedure TcxCustomTextEdit.SetSelText(const Value: TCaption);
var
  ANewSelStart: Integer;
  ANewText: string;
begin
  if CanChangeSelText(Value, ANewText, ANewSelStart) then
  begin
    InternalSetDisplayValue(ANewText);
    SelStart := ANewSelStart;
  end;
end;

procedure TcxCustomTextEdit.WMClear(var Message: TMessage);
begin
  BeginUserAction;
  try
    if (not ActiveProperties.ReadOnly) and DataBinding.IsDataAvailable then
      ClearSelection;
  finally
    EndUserAction;
  end;
end;

procedure TcxCustomTextEdit.WMCommand(var Message: TWMCommand);
begin
  inherited;
  case Message.NotifyCode of
    EN_CHANGE:
      DoTextChanged;
    EN_VSCROLL, EN_HSCROLL:
      DoLayoutChanged;
  end;
end;

procedure TcxCustomTextEdit.WMGetText(var Message: TWMGetText);
var
  S: string;
begin
  if Message.TextMax > 0 then
  begin
    if Properties = nil then
      S := FText
    else
      S := InternalGetText;

    if WPARAM(Length(S)) > Message.TextMax - 1 then
      SetLength(S, Message.TextMax - 1);
    StrLCopy(Message.Text, PChar(S), Message.TextMax - 1);
    Message.Result := Length(S);
  end
  else
    Message.Result := 0;
end;

procedure TcxCustomTextEdit.WMGetTextLength(var Message: TWMGetTextLength);
begin
  if Properties = nil then
    Message.Result := Length(FText)
  else
    Message.Result := Length(InternalGetText);
end;

procedure TcxCustomTextEdit.WMSetFocus(var Message: TWMSetFocus);
begin
  if HasInnerEdit and not IsInplace and not (csDestroyingHandle in ControlState) then
    InnerControl.HandleNeeded;
  inherited;
end;

procedure TcxCustomTextEdit.WMSetText(var Message: TWMSetText);
begin
  if FInternalTextSetting then
    inherited
  else
  begin
    Message.Result := 0;
    FInternalTextSetting := True;
    try
      if InternalSetText(string(Message.Text)) then
        Message.Result := 1;
    finally
      FInternalTextSetting := False;
    end;
  end;
end;

{ TcxTextEdit }

class function TcxTextEdit.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxTextEditProperties;
end;

function TcxTextEdit.GetActiveProperties: TcxTextEditProperties;
begin
  Result := TcxTextEditProperties(InternalGetActiveProperties);
end;

function TcxTextEdit.GetProperties: TcxTextEditProperties;
begin
  Result := TcxTextEditProperties(inherited Properties);
end;

function TcxTextEdit.SupportsSpelling: Boolean;
begin
  Result := IsTextInputMode;
end;

procedure TcxTextEdit.SetProperties(Value: TcxTextEditProperties);
begin
  Properties.Assign(Value);
end;

{ TcxFilterTextEditHelper }

class function TcxFilterTextEditHelper.GetFilterEditClass: TcxCustomEditClass;
begin
  Result := TcxTextEdit;
end;

class procedure TcxFilterTextEditHelper.InitializeProperties(AProperties,
  AEditProperties: TcxCustomEditProperties; AHasButtons: Boolean);
begin
  inherited InitializeProperties(AProperties, AEditProperties, AHasButtons);
  with TcxCustomTextEditProperties(AProperties) do
  begin
    AutoSelect := True;
    HideSelection := True;
    ViewStyle := vsNormal;
  end;
end;

class procedure TcxFilterTextEditHelper.SetFilterValue(AEdit: TcxCustomEdit;
  AEditProperties: TcxCustomEditProperties; AValue: Variant);
begin
  AEdit.EditValue := AValue;
end;

initialization
  GetRegisteredEditProperties.Register(TcxTextEditProperties, scxSEditRepositoryTextItem);
  FilterEditsController.Register(TcxTextEditProperties, TcxFilterTextEditHelper);

finalization
  FilterEditsController.Unregister(TcxTextEditProperties, TcxFilterTextEditHelper);

end.
