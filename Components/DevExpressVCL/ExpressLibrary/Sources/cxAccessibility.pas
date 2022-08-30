{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           Express Cross Platform Library graphics classes          }
{                                                                    }
{           Copyright (c) 2000-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSCROSSPLATFORMLIBRARY AND ALL   }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM       }
{   ONLY.                                                            }
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

unit cxAccessibility;

interface

{$I cxVer.inc}

uses
  Types, Windows, ActiveX, Classes, OleServer, Messages, dxCore, cxClasses;

(*$HPPEMIT '#include <OleIdl.h>*)

const
  cxAccessibleObjectSelfID = 0;

  SID_IcxAccessible = '{618736E0-3C3D-11CF-810C-00AA00389B71}';
  IID_IcxAccessible: TGUID = SID_IcxAccessible;

type
  TcxAccessibilityHelper = class;

  TcxAccessibleSimpleChildElementID = 0..MaxInt;

  TcxAccessibleObjectProperty = (aopDefaultAction, aopDescription, aopFocus,
    aopLocation, aopShortcut, aopValue);
  TcxAccessibleObjectProperties = set of TcxAccessibleObjectProperty;

  TcxAccessibleObjectHitTest = (aohtNone, aohtSelf, aohtChild);

  TcxAccessibilityNavigationDirection = (andLeft, andUp, andRight, andDown,
    andPrev, andNext);

  { IcxAccessible }

  IcxAccessible = interface(IDispatch)
  [SID_IcxAccessible]
    function Get_accParent(out ppdispParent: IDispatch): HResult; stdcall;
    function Get_accChildCount(out pcountChildren: Integer): HResult; stdcall;
    function Get_accChild(varChild: OleVariant; out ppdispChild: IDispatch): HResult; stdcall;
    function Get_accName(varChild: OleVariant; out pszName: WideString): HResult; stdcall;
    function Get_accValue(varChild: OleVariant; out pszValue: WideString): HResult; stdcall;
    function Get_accDescription(varChild: OleVariant; out pszDescription: WideString): HResult; stdcall;
    function Get_accRole(varChild: OleVariant; out pvarRole: OleVariant): HResult; stdcall;
    function Get_accState(varChild: OleVariant; out pvarState: OleVariant): HResult; stdcall;
    function Get_accHelp(varChild: OleVariant; out pszHelp: WideString): HResult; stdcall;
    function Get_accHelpTopic(out pszHelpFile: WideString; varChild: OleVariant;
      out pidTopic: Integer): HResult; stdcall;
    function Get_accKeyboardShortcut(varChild: OleVariant; out pszKeyboardShortcut: WideString): HResult; stdcall;
    function Get_accFocus(out pvarChild: OleVariant): HResult; stdcall;
    function Get_accSelection(out pvarChildren: OleVariant): HResult; stdcall;
    function Get_accDefaultAction(varChild: OleVariant; out pszDefaultAction: WideString): HResult; stdcall;
    function accSelect(flagsSelect: Integer; varChild: OleVariant): HResult; stdcall;
    function accLocation(out pxLeft: Integer; out pyTop: Integer; out pcxWidth: Integer;
      out pcyHeight: Integer; varChild: OleVariant): HResult; stdcall;
    function accNavigate(navDir: Integer; varStart: OleVariant; out pvarEndUpAt: OleVariant): HResult; stdcall;
    function accHitTest(xLeft: Integer; yTop: Integer; out pvarChild: OleVariant): HResult; stdcall;
    function accDoDefaultAction(varChild: OleVariant): HResult; stdcall;
    function Set_accName(varChild: OleVariant; const pszName: WideString): HResult; stdcall;
    function Set_accValue(varChild: OleVariant; const pszValue: WideString): HResult; stdcall;
  end;

  IcxAccessibilityHelper = interface
  ['{D4890860-09B2-4648-BD9E-DFFBD140E5F1}']
    function GetHelper: TcxAccessibilityHelper;
    procedure OwnerObjectDestroyed;
  end;

  TcxAccessibilityHelper = class(TInterfacedObject, IDispatch, IOleWindow,
    IcxAccessible, IcxAccessibilityHelper)
  private
    FIsOwnerObjectLive: Boolean;

    // IDispatch
    function GetIDsOfNames(const IID: TGUID; Names: Pointer;
      NameCount, LocaleID: Integer; DispIDs: Pointer): HResult; stdcall;
    function GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HResult; stdcall;
    function GetTypeInfoCount(out Count: Integer): HResult; stdcall;
    function Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer;
      Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult; stdcall;
    // IOleWindow
    function ContextSensitiveHelp(fEnterMode: BOOL): HResult; stdcall;
    function GetWindow(out wnd: HWnd): HResult; stdcall;
    // IcxAccessible
    function accDoDefaultAction(varChild: OleVariant): HResult; stdcall;
    function accHitTest(xLeft: Integer; yTop: Integer; out pvarChild: OleVariant): HResult; stdcall;
    function accLocation(out pxLeft: Integer; out pyTop: Integer; out pcxWidth: Integer;
      out pcyHeight: Integer; varChild: OleVariant): HResult; stdcall;
    function accNavigate(navDir: Integer; varStart: OleVariant; out pvarEndUpAt: OleVariant): HResult; stdcall;
    function accSelect(flagsSelect: Integer; varChild: OleVariant): HResult; stdcall;
    function Get_accChild(varChild: OleVariant; out ppdispChild: IDispatch): HResult; stdcall;
    function Get_accChildCount(out pcountChildren: Integer): HResult; stdcall;
    function Get_accDefaultAction(varChild: OleVariant; out pszDefaultAction: WideString): HResult; stdcall;
    function Get_accDescription(varChild: OleVariant; out pszDescription: WideString): HResult; stdcall;
    function Get_accFocus(out pvarChild: OleVariant): HResult; stdcall;
    function Get_accHelp(varChild: OleVariant; out pszHelp: WideString): HResult; stdcall;
    function Get_accHelpTopic(out pszHelpFile: WideString; varChild: OleVariant;
      out pidTopic: Integer): HResult; stdcall;
    function Get_accKeyboardShortcut(varChild: OleVariant; out pszKeyboardShortcut: WideString): HResult; stdcall;
    function Get_accName(varChild: OleVariant; out pszName: WideString): HResult; stdcall;
    function Get_accParent(out ppdispParent: IDispatch): HResult; stdcall;
    function Get_accRole(varChild: OleVariant; out pvarRole: OleVariant): HResult; stdcall;
    function Get_accSelection(out pvarChildren: OleVariant): HResult; stdcall;
    function Get_accState(varChild: OleVariant; out pvarState: OleVariant): HResult; stdcall;
    function Get_accValue(varChild: OleVariant; out pszValue: WideString): HResult; stdcall;
    function Set_accName(varChild: OleVariant; const pszName: WideString): HResult; stdcall;
    function Set_accValue(varChild: OleVariant; const pszValue: WideString): HResult; stdcall;

    function CheckIsOwnerObjectLive(out AErrorCode: HResult): Boolean;
    procedure CheckSimpleChildElementToBeReturned(var AVarChild: OleVariant);
    procedure CheckStringToBeReturned(const AStr: WideString; out AResult: HResult);
    function GetSimpleChildElementID(AChildID: OleVariant;
      out ASimpleChildElementID: TcxAccessibleSimpleChildElementID;
      out AErrorCode: HResult): Boolean;
    function GetVisible: Boolean;
  protected
    FOwnerObject: TObject;

    // IcxAccessibilityHelper
    function GetHelper: TcxAccessibilityHelper;
    procedure OwnerObjectDestroyed; virtual;

    function ChildIsSimpleElement(AIndex: Integer): Boolean; virtual;
    procedure DoDefaultAction(AChildID: TcxAccessibleSimpleChildElementID); virtual;
    function Focused(out AIsChildFocused: Boolean;
      out AFocusedChildIndex: Integer): Boolean; virtual;
    function GetChild(AIndex: Integer): TcxAccessibilityHelper; virtual;
    function GetChildCount: Integer; virtual;
    function GetChildIndex(AChild: TcxAccessibilityHelper): Integer; virtual;
    function GetDefaultActionDescription(AChildID: TcxAccessibleSimpleChildElementID): string; virtual;
    function GetDescription(AChildID: TcxAccessibleSimpleChildElementID): string; virtual;
    function GetHitTest(AScreenX, AScreenY: Integer; out AChildIndex: Integer): TcxAccessibleObjectHitTest; virtual;
    procedure GetKeyboardAccessParameters(AChildID: TcxAccessibleSimpleChildElementID;
      out AShortCut: TShortCut; out ACaptionWithAccelChars: string); virtual;
    function GetName(AChildID: TcxAccessibleSimpleChildElementID): string; virtual;
    function GetOwnerObjectWindow: HWND; virtual;
    function GetParent: TcxAccessibilityHelper; virtual;
    function GetRole(AChildID: TcxAccessibleSimpleChildElementID): Integer; virtual;
    function GetSelectable: Boolean; virtual;
    function GetState(AChildID: TcxAccessibleSimpleChildElementID): Integer; virtual;
    function GetSupportedProperties(AChildID: TcxAccessibleSimpleChildElementID): TcxAccessibleObjectProperties; virtual;
    function GetValue(AChildID: TcxAccessibleSimpleChildElementID): string; virtual;
    function NavigateToChild(ACurrentChildIndex: Integer; ADirection: TcxAccessibilityNavigationDirection): Integer; virtual; // andPrev, andNext must prevent looping
    procedure SetValue(AChildID: TcxAccessibleSimpleChildElementID; const Value: string); virtual;

    function GetRootHelper: TcxAccessibilityHelper;
  public {for friend classes}
    function GetNextSelectableChildIndex(AStartIndex: Integer; AGoForward: Boolean): Integer;
    function GetScreenBounds(AChildID: TcxAccessibleSimpleChildElementID): TRect; virtual;
  public
    constructor Create(AOwnerObject: TObject); virtual;

    property Childs[AIndex: Integer]: TcxAccessibilityHelper read GetChild;
    property ChildCount: Integer read GetChildCount;
    property IsOwnerObjectLive: Boolean read FIsOwnerObjectLive;
    property OwnerObject: TObject read FOwnerObject;
    property OwnerObjectWindow: HWND read GetOwnerObjectWindow;
    property Parent: TcxAccessibilityHelper read GetParent;
    property Selectable: Boolean read GetSelectable;
    property States[AChildID: TcxAccessibleSimpleChildElementID]: Integer read GetState;
    property Visible: Boolean read GetVisible;
  end;

  TcxAccessibilityHelperClass = class of TcxAccessibilityHelper;

const
  cxROLE_SYSTEM_TITLEBAR = $1;
  cxROLE_SYSTEM_MENUBAR = $2;
  cxROLE_SYSTEM_SCROLLBAR = $3;
  cxROLE_SYSTEM_GRIP = $4;
  cxROLE_SYSTEM_SOUND = $5;
  cxROLE_SYSTEM_CURSOR = $6;
  cxROLE_SYSTEM_CARET = $7;
  cxROLE_SYSTEM_ALERT = $8;
  cxROLE_SYSTEM_WINDOW = $9;
  cxROLE_SYSTEM_CLIENT = $a;
  cxROLE_SYSTEM_MENUPOPUP = $b;
  cxROLE_SYSTEM_MENUITEM = $c;
  cxROLE_SYSTEM_TOOLTIP = $d;
  cxROLE_SYSTEM_APPLICATION = $e;
  cxROLE_SYSTEM_DOCUMENT = $f;
  cxROLE_SYSTEM_PANE = $10;
  cxROLE_SYSTEM_CHART = $11;
  cxROLE_SYSTEM_DIALOG = $12;
  cxROLE_SYSTEM_BORDER = $13;
  cxROLE_SYSTEM_GROUPING = $14;
  cxROLE_SYSTEM_SEPARATOR = $15;
  cxROLE_SYSTEM_TOOLBAR = $16;
  cxROLE_SYSTEM_STATUSBAR = $17;
  cxROLE_SYSTEM_TABLE = $18;
  cxROLE_SYSTEM_COLUMNHEADER = $19;
  cxROLE_SYSTEM_ROWHEADER = $1a;
  cxROLE_SYSTEM_COLUMN = $1b;
  cxROLE_SYSTEM_ROW = $1c;
  cxROLE_SYSTEM_CELL = $1d;
  cxROLE_SYSTEM_LINK = $1e;
  cxROLE_SYSTEM_HELPBALLOON = $1f;
  cxROLE_SYSTEM_CHARACTER = $20;
  cxROLE_SYSTEM_LIST = $21;
  cxROLE_SYSTEM_LISTITEM = $22;
  cxROLE_SYSTEM_OUTLINE = $23;
  cxROLE_SYSTEM_OUTLINEITEM = $24;
  cxROLE_SYSTEM_PAGETAB = $25;
  cxROLE_SYSTEM_PROPERTYPAGE = $26;
  cxROLE_SYSTEM_INDICATOR = $27;
  cxROLE_SYSTEM_GRAPHIC = $28;
  cxROLE_SYSTEM_STATICTEXT = $29;
  cxROLE_SYSTEM_TEXT = $2a;
  cxROLE_SYSTEM_PUSHBUTTON = $2b;
  cxROLE_SYSTEM_CHECKBUTTON = $2c;
  cxROLE_SYSTEM_RADIOBUTTON = $2d;
  cxROLE_SYSTEM_COMBOBOX = $2e;
  cxROLE_SYSTEM_DROPLIST = $2f;
  cxROLE_SYSTEM_PROGRESSBAR = $30;
  cxROLE_SYSTEM_DIAL = $31;
  cxROLE_SYSTEM_HOTKEYFIELD = $32;
  cxROLE_SYSTEM_SLIDER = $33;
  cxROLE_SYSTEM_SPINBUTTON = $34;
  cxROLE_SYSTEM_DIAGRAM = $35;
  cxROLE_SYSTEM_ANIMATION = $36;
  cxROLE_SYSTEM_EQUATION = $37;
  cxROLE_SYSTEM_BUTTONDROPDOWN = $38;
  cxROLE_SYSTEM_BUTTONMENU = $39;
  cxROLE_SYSTEM_BUTTONDROPDOWNGRID = $3a;
  cxROLE_SYSTEM_WHITESPACE = $3b;
  cxROLE_SYSTEM_PAGETABLIST = $3c;
  cxROLE_SYSTEM_CLOCK = $3d;
  cxROLE_SYSTEM_SPLITBUTTON = $3e;
  cxROLE_SYSTEM_IPADDRESS = $3f;
  cxROLE_SYSTEM_OUTLINEBUTTON = $40;

  cxSTATE_SYSTEM_NORMAL = $0;
  cxSTATE_SYSTEM_UNAVAILABLE = $1;
  cxSTATE_SYSTEM_SELECTED = $2;
  cxSTATE_SYSTEM_FOCUSED = $4;
  cxSTATE_SYSTEM_PRESSED = $8;
  cxSTATE_SYSTEM_CHECKED = $10;
  cxSTATE_SYSTEM_MIXED = $20;
  cxSTATE_SYSTEM_INDETERMINATE = cxSTATE_SYSTEM_MIXED;
  cxSTATE_SYSTEM_READONLY = $40;
  cxSTATE_SYSTEM_HOTTRACKED = $80;
  cxSTATE_SYSTEM_DEFAULT = $100;
  cxSTATE_SYSTEM_EXPANDED = $200;
  cxSTATE_SYSTEM_COLLAPSED = $400;
  cxSTATE_SYSTEM_BUSY = $800;
  cxSTATE_SYSTEM_FLOATING = $1000;
  cxSTATE_SYSTEM_MARQUEED = $2000;
  cxSTATE_SYSTEM_ANIMATED = $4000;
  cxSTATE_SYSTEM_INVISIBLE = $8000;
  cxSTATE_SYSTEM_OFFSCREEN = $10000;
  cxSTATE_SYSTEM_SIZEABLE = $20000;
  cxSTATE_SYSTEM_MOVEABLE = $40000;
  cxSTATE_SYSTEM_SELFVOICING = $80000;
  cxSTATE_SYSTEM_FOCUSABLE = $100000;
  cxSTATE_SYSTEM_SELECTABLE = $200000;
  cxSTATE_SYSTEM_LINKED = $400000;
  cxSTATE_SYSTEM_TRAVERSED = $800000;
  cxSTATE_SYSTEM_MULTISELECTABLE = $1000000;
  cxSTATE_SYSTEM_EXTSELECTABLE = $2000000;
  cxSTATE_SYSTEM_ALERT_LOW = $4000000;
  cxSTATE_SYSTEM_ALERT_MEDIUM = $8000000;
  cxSTATE_SYSTEM_ALERT_HIGH = $10000000;
  cxSTATE_SYSTEM_PROTECTED = $20000000;
  cxSTATE_SYSTEM_VALID = $7fffffff;
  cxSTATE_SYSTEM_HASPOPUP = $40000000;

function WMGetObjectResultFromIAccessibilityHelper(
  const AWMGetObjectMessage: TMessage; AIHelper: IcxAccessibilityHelper): LRESULT;

function CanReturnAccessibleObject(const AWMGetObjectMessage: TMessage): Boolean;
function IsAccessibilitySupported: Boolean;

implementation

uses
  Menus, SysUtils, cxControls;

const
  NAVDIR_UP = $1 ;
  NAVDIR_DOWN = $2 ;
  NAVDIR_LEFT = $3 ;
  NAVDIR_RIGHT = $4 ;
  NAVDIR_NEXT = $5 ;
  NAVDIR_PREVIOUS = $6 ;
  NAVDIR_FIRSTCHILD = $7 ;
  NAVDIR_LASTCHILD = $8 ;

  CO_E_OBJECTNOTCONNECTED = 0; // TODO

  OleaccLibraryName = 'oleacc.dll';

var
  FcxAccessibleObjectFromWindow: function(hwnd: THandle; dwId: DWORD;
    const riid: TGUID; out ppvObject): HRESULT; stdcall = nil;
  FcxLResultFromObject: function(const riid: TGUID;
    wParam: WPARAM; punk: IUnknown): LRESULT; stdcall = nil;
  FOleaccLibrary: HMODULE;

function WMGetObjectResultFromIAccessibilityHelper(
  const AWMGetObjectMessage: TMessage; AIHelper: IcxAccessibilityHelper): LRESULT;
begin
  Result := FcxLResultFromObject(IID_IcxAccessible, AWMGetObjectMessage.WParam,
    AIHelper as IcxAccessible);
end;

function CanReturnAccessibleObject(const AWMGetObjectMessage: TMessage): Boolean;
begin
  Result := IsAccessibilitySupported and
    (Cardinal(AWMGetObjectMessage.LParam) = OBJID_CLIENT);
end;

function IsAccessibilitySupported: Boolean;
begin
  Result := Assigned(FcxLResultFromObject);
end;

function cxGetAccessibleObjectFromWindow(hwnd: THandle; dwId: DWORD;
  const riid: TGUID; out ppvObject): HRESULT;
begin
  Result := FcxAccessibleObjectFromWindow(hwnd, dwId, riid, ppvObject);
end;

{ TcxAccessibilityHelper }

constructor TcxAccessibilityHelper.Create(AOwnerObject: TObject);
begin
  inherited Create;
  FIsOwnerObjectLive := True;
  FOwnerObject := AOwnerObject;
end;

function TcxAccessibilityHelper.GetNextSelectableChildIndex(AStartIndex: Integer;
  AGoForward: Boolean): Integer;
var
  ACount, AStep, I: Integer;
begin
  Result := -1;

  ACount := ChildCount;
  if AStartIndex < 0 then
    if AGoForward then
      AStartIndex := -1
    else
      AStartIndex := ACount
  else
    if AStartIndex >= ACount then
      raise EdxException.Create('');
  I := AStartIndex;

  if AGoForward then
    AStep := 1
  else
    AStep := -1;

  repeat
    Inc(I, AStep);
    if (I < 0) or (I = ACount) then
      Break;
    if Childs[I].Selectable then
      Result := I;
  until Result <> -1;
end;

// IcxAccessibilityHelper
function TcxAccessibilityHelper.GetHelper: TcxAccessibilityHelper;
begin
  Result := Self;
end;

procedure TcxAccessibilityHelper.OwnerObjectDestroyed;
begin
  FIsOwnerObjectLive := False;
end;

function TcxAccessibilityHelper.ChildIsSimpleElement(AIndex: Integer): Boolean;
begin
  raise EdxException.Create('');
end;

procedure TcxAccessibilityHelper.DoDefaultAction(
  AChildID: TcxAccessibleSimpleChildElementID);
begin
end;

function TcxAccessibilityHelper.Focused(out AIsChildFocused: Boolean;
  out AFocusedChildIndex: Integer): Boolean;
begin
  Result := False;
end;

function TcxAccessibilityHelper.GetChild(
  AIndex: Integer): TcxAccessibilityHelper;
begin
  Result := nil;
end;

function TcxAccessibilityHelper.GetChildCount: Integer;
begin
  Result := 0;
end;

function TcxAccessibilityHelper.GetChildIndex(
  AChild: TcxAccessibilityHelper): Integer;
begin
  raise EdxException.Create('');
end;

function TcxAccessibilityHelper.GetDefaultActionDescription(
  AChildID: TcxAccessibleSimpleChildElementID): string;
begin
  Result := '';
end;

function TcxAccessibilityHelper.GetDescription(
  AChildID: TcxAccessibleSimpleChildElementID): string;
begin
  Result := '';
end;

function TcxAccessibilityHelper.GetHitTest(AScreenX, AScreenY: Integer;
  out AChildIndex: Integer): TcxAccessibleObjectHitTest;
begin
  Result := aohtNone;
end;

procedure TcxAccessibilityHelper.GetKeyboardAccessParameters(
  AChildID: TcxAccessibleSimpleChildElementID; out AShortCut: TShortCut;
  out ACaptionWithAccelChars: string);
begin
  AShortCut := 0;
  ACaptionWithAccelChars := '';
end;

function TcxAccessibilityHelper.GetName(
  AChildID: TcxAccessibleSimpleChildElementID): string;
begin
  Result := '';
end;

function TcxAccessibilityHelper.GetOwnerObjectWindow: HWND;
begin
  Result := 0;
end;

function TcxAccessibilityHelper.GetParent: TcxAccessibilityHelper;
begin
  Result := nil;
end;

function TcxAccessibilityHelper.GetRole(
  AChildID: TcxAccessibleSimpleChildElementID): Integer;
begin
  Result := 0;
end;

function TcxAccessibilityHelper.GetScreenBounds(
  AChildID: TcxAccessibleSimpleChildElementID): TRect;
begin
  Result := Rect(0, 0, 0, 0);
end;

function TcxAccessibilityHelper.GetSelectable: Boolean;
begin
  Result := False;
end;

function TcxAccessibilityHelper.GetState(
  AChildID: TcxAccessibleSimpleChildElementID): Integer;
begin
  Result := cxSTATE_SYSTEM_NORMAL;
end;

function TcxAccessibilityHelper.GetSupportedProperties(
  AChildID: TcxAccessibleSimpleChildElementID): TcxAccessibleObjectProperties;
begin
  Result := [];
end;

function TcxAccessibilityHelper.GetValue(
  AChildID: TcxAccessibleSimpleChildElementID): string;
begin
  raise EdxException.Create('');
end;

function TcxAccessibilityHelper.NavigateToChild(
  ACurrentChildIndex: Integer; ADirection: TcxAccessibilityNavigationDirection): Integer;
begin
  raise EdxException.Create('');
end;

procedure TcxAccessibilityHelper.SetValue(
  AChildID: TcxAccessibleSimpleChildElementID; const Value: string);
begin
end;

function TcxAccessibilityHelper.GetRootHelper: TcxAccessibilityHelper;
var
  AParent: TcxAccessibilityHelper;
begin
  Result := Self;
  repeat
    AParent := Result.Parent;
    if AParent <> nil then
      Result := AParent;
  until (AParent = nil);
end;

// IDispatch
function TcxAccessibilityHelper.GetIDsOfNames(const IID: TGUID; Names: Pointer;
  NameCount, LocaleID: Integer; DispIDs: Pointer): HResult;
begin
  Result := E_NOTIMPL;
end;

function TcxAccessibilityHelper.GetTypeInfo(Index, LocaleID: Integer;
  out TypeInfo): HResult;
begin
  Result := E_NOTIMPL;
end;

function TcxAccessibilityHelper.GetTypeInfoCount(out Count: Integer): HResult;
begin
  Result := E_NOTIMPL;
end;

function TcxAccessibilityHelper.Invoke(DispID: Integer; const IID: TGUID;
  LocaleID: Integer; Flags: Word; var Params;
  VarResult, ExcepInfo, ArgErr: Pointer): HResult;
begin
  Result := E_NOTIMPL;
end;

// IOleWindow
function TcxAccessibilityHelper.ContextSensitiveHelp(fEnterMode: BOOL): HResult;
begin
  Result := S_OK;
end;

function TcxAccessibilityHelper.GetWindow(out wnd: HWnd): HResult;
begin
  if CheckIsOwnerObjectLive(Result) then
  begin
    wnd := GetOwnerObjectWindow;
    Result := S_OK; // TODO
  end;
end;

// IcxAccessible
function TcxAccessibilityHelper.accDoDefaultAction(
  varChild: OleVariant): HResult;
var
  ASimpleChildElementID: TcxAccessibleSimpleChildElementID;
begin
  if GetSimpleChildElementID(varChild, ASimpleChildElementID, Result) then
    if aopDefaultAction in GetSupportedProperties(ASimpleChildElementID) then
    begin
      DoDefaultAction(ASimpleChildElementID);
      Result := S_OK;
    end
    else
      Result := DISP_E_MEMBERNOTFOUND;
end;

function TcxAccessibilityHelper.accHitTest(xLeft: Integer; yTop: Integer;
  out pvarChild: OleVariant): HResult;
var
  AChildIndex: Integer;
begin
  VariantInit(pvarChild);
  if CheckIsOwnerObjectLive(Result) then
    if aopLocation in GetSupportedProperties(cxAccessibleObjectSelfID) then
    begin
      if PtInRect(GetScreenBounds(cxAccessibleObjectSelfID), Point(xLeft, yTop)) then
      begin
        case GetHitTest(xLeft, yTop, AChildIndex) of
          aohtSelf:
            begin
              TVarData(pvarChild).VType := VT_I4;
              pvarChild := CHILDID_SELF;
            end;
          aohtChild:
            begin
              TVarData(pvarChild).VType := VT_I4;
              pvarChild := AChildIndex + 1;
              CheckSimpleChildElementToBeReturned(pvarChild);
            end;
        end;
      end;
      if TVarData(pvarChild).VType <> VT_EMPTY then
        Result := S_OK
      else
        Result := S_FALSE;
    end
    else
      Result := DISP_E_MEMBERNOTFOUND;
end;

function TcxAccessibilityHelper.accLocation(out pxLeft: Integer;
  out pyTop: Integer; out pcxWidth: Integer; out pcyHeight: Integer;
  varChild: OleVariant): HResult;
var
  ASimpleChildElementID: TcxAccessibleSimpleChildElementID;
begin
  if GetSimpleChildElementID(varChild, ASimpleChildElementID, Result) then
    if aopLocation in GetSupportedProperties(cxAccessibleObjectSelfID) then
      with GetScreenBounds(ASimpleChildElementID) do
      begin
        pxLeft := Left;
        pyTop := Top;
        pcxWidth := Right - Left;
        pcyHeight := Bottom - Top;
        Result := S_OK;
      end
    else
      Result := DISP_E_MEMBERNOTFOUND;
end;

function TcxAccessibilityHelper.accNavigate(navDir: Integer;
  varStart: OleVariant; out pvarEndUpAt: OleVariant): HResult;

  procedure NavigateToFirstOrLastChild;
  var
    AChildCount: Integer;
  begin
    if varStart <> CHILDID_SELF then
      Result := E_INVALIDARG
    else
      if not (aopLocation in GetSupportedProperties(cxAccessibleObjectSelfID)) then
        Result := DISP_E_MEMBERNOTFOUND
      else
      begin
        AChildCount := GetChildCount;
        if AChildCount = 0 then
          Result := S_FALSE
        else
        begin
          TVarData(pvarEndUpAt).VType := VT_I4;
          if navDir = NAVDIR_FIRSTCHILD then
            pvarEndUpAt := 1
          else
            pvarEndUpAt := AChildCount;
          Result := S_OK;
        end;
      end;
  end;

  procedure NavigateToNeighboringChildViaParent;
  var
    AParent: TcxAccessibilityHelper;
    AStartChildID: OleVariant;
  begin
    Result := S_FALSE;
    AParent := GetParent; // TODO get_accParent
    if (AParent <> nil) and AParent.CheckIsOwnerObjectLive(Result) and
      (aopLocation in AParent.GetSupportedProperties(cxAccessibleObjectSelfID)) then
    begin
      TVarData(AStartChildID).VType := VT_I4;
      AStartChildID := AParent.GetChildIndex(Self) + 1;
      if (AParent as IcxAccessible).accNavigate(navDir, AStartChildID, pvarEndUpAt) = S_OK then
        Result := S_OK;
    end;
  end;

  procedure NavigateToNeighboringChild;

    function GetNavigationDirection: TcxAccessibilityNavigationDirection;
    begin
      case navDir of
        NAVDIR_DOWN: Result := andDown;
        NAVDIR_LEFT: Result := andLeft;
        NAVDIR_NEXT: Result := andNext;
        NAVDIR_PREVIOUS: Result := andPrev;
        NAVDIR_RIGHT: Result := andRight;
      else
        Result := andUp;
      end;
    end;

  var
    AChildIndex: Integer;
  begin
    if varStart > GetChildCount then
      Result := E_INVALIDARG
    else
      if not (aopLocation in GetSupportedProperties(cxAccessibleObjectSelfID)) then
        Result := DISP_E_MEMBERNOTFOUND
      else
      begin
        AChildIndex := NavigateToChild(varStart - 1, GetNavigationDirection);
        if AChildIndex <> varStart - 1 then
        begin
          TVarData(pvarEndUpAt).VType := VT_I4;
          pvarEndUpAt := AChildIndex + 1;
          Result := S_OK;
        end
        else
          Result := S_FALSE;
      end;
  end;

begin
  VariantInit(pvarEndUpAt);
  if not CheckIsOwnerObjectLive(Result) then
    Exit;
  if (navDir = NAVDIR_FIRSTCHILD) or (navDir = NAVDIR_LASTCHILD) then
    NavigateToFirstOrLastChild
  else
    if varStart = CHILDID_SELF then
      NavigateToNeighboringChildViaParent
    else
      NavigateToNeighboringChild;

  if Result = S_OK then
    CheckSimpleChildElementToBeReturned(pvarEndUpAt);
end;

function TcxAccessibilityHelper.accSelect(flagsSelect: Integer;
  varChild: OleVariant): HResult;
begin
  Result := DISP_E_MEMBERNOTFOUND; // TODO
end;

function TcxAccessibilityHelper.Get_accChild(varChild: OleVariant;
  out ppdispChild: IDispatch): HResult;
begin
  if not CheckIsOwnerObjectLive(Result) then
    Exit;
  if (TVarData(varChild).VType = VT_EMPTY) or (varChild > GetChildCount) then
    Result := E_INVALIDARG
  else
    if ChildIsSimpleElement(varChild - 1) then
      Result := S_FALSE
    else
    begin
      ppdispChild := GetChild(varChild - 1);
      Result := S_OK;
    end;
end;

function TcxAccessibilityHelper.Get_accChildCount(
  out pcountChildren: Integer): HResult;
begin
  if CheckIsOwnerObjectLive(Result) then
  begin
    pcountChildren := GetChildCount;
    Result := S_OK;
  end;
end;

function TcxAccessibilityHelper.Get_accDefaultAction(varChild: OleVariant;
  out pszDefaultAction: WideString): HResult;
var
  ASimpleChildElementID: TcxAccessibleSimpleChildElementID;
begin
  if GetSimpleChildElementID(varChild, ASimpleChildElementID, Result) then
    if aopDefaultAction in GetSupportedProperties(ASimpleChildElementID) then
    begin
      pszDefaultAction := GetDefaultActionDescription(ASimpleChildElementID);
//      CheckStringToBeReturned(pszDefaultAction, Result);
      Result := S_OK;
    end
    else
      Result := DISP_E_MEMBERNOTFOUND;
end;

function TcxAccessibilityHelper.Get_accDescription(varChild: OleVariant;
  out pszDescription: WideString): HResult;
var
  ASimpleChildElementID: TcxAccessibleSimpleChildElementID;
begin
  if GetSimpleChildElementID(varChild, ASimpleChildElementID, Result) then
    if aopDescription in GetSupportedProperties(ASimpleChildElementID) then
    begin
      pszDescription := GetDescription(ASimpleChildElementID);
      CheckStringToBeReturned(pszDescription, Result);
    end
    else
      Result := DISP_E_MEMBERNOTFOUND;
end;

function TcxAccessibilityHelper.Get_accFocus(
  out pvarChild: OleVariant): HResult;
var
  AFocusedChildIndex: Integer;
  AIsChildFocused: Boolean;
begin
  VariantInit(pvarChild);
  if CheckIsOwnerObjectLive(Result) then
    if aopFocus in GetSupportedProperties(cxAccessibleObjectSelfID) then
    begin
      if not Focused(AIsChildFocused, AFocusedChildIndex) then
        Result := S_FALSE
      else
      begin
        TVarData(pvarChild).VType := VT_I4;
        if not AIsChildFocused then
          pvarChild := CHILDID_SELF
        else
          pvarChild := AFocusedChildIndex + 1;
        CheckSimpleChildElementToBeReturned(pvarChild);
        Result := S_OK;
      end;
    end
    else
      Result := DISP_E_MEMBERNOTFOUND;
end;

function TcxAccessibilityHelper.Get_accHelp(varChild: OleVariant;
  out pszHelp: WideString): HResult;
begin
  Result := DISP_E_MEMBERNOTFOUND;
end;

function TcxAccessibilityHelper.Get_accHelpTopic(out pszHelpFile: WideString;
  varChild: OleVariant; out pidTopic: Integer): HResult;
begin
  Result := DISP_E_MEMBERNOTFOUND;
end;

function TcxAccessibilityHelper.Get_accKeyboardShortcut(varChild: OleVariant;
  out pszKeyboardShortcut: WideString): HResult;
var
  ACaption: string;
  AShortCut: TShortCut;
  ASimpleChildElementID: TcxAccessibleSimpleChildElementID;
begin
  if GetSimpleChildElementID(varChild, ASimpleChildElementID, Result) then
    if aopShortcut in GetSupportedProperties(ASimpleChildElementID) then
    begin
      GetKeyboardAccessParameters(ASimpleChildElementID, AShortCut, ACaption);
      if GetHotKey(ACaption) <> '' then
        pszKeyboardShortcut := UpperCase(GetHotKey(ACaption))
      else
        pszKeyboardShortcut := ShortCutToText(AShortCut);
      CheckStringToBeReturned(pszKeyboardShortcut, Result);
    end
    else
      Result := DISP_E_MEMBERNOTFOUND;
end;

function TcxAccessibilityHelper.Get_accName(varChild: OleVariant;
  out pszName: WideString): HResult;
var
  ASimpleChildElementID: TcxAccessibleSimpleChildElementID;
begin
  if GetSimpleChildElementID(varChild, ASimpleChildElementID, Result) then
  begin
    pszName := GetName(ASimpleChildElementID);
    CheckStringToBeReturned(pszName, Result);
  end;
end;

function TcxAccessibilityHelper.Get_accParent(
  out ppdispParent: IDispatch): HResult;
var
  AParentWnd: HWND;
begin
  if CheckIsOwnerObjectLive(Result) then
  begin
    ppdispParent := GetParent;
    if (ppdispParent = nil) and (GetOwnerObjectWindow <> 0) then
    begin
      if IsChildClassWindow(GetOwnerObjectWindow) then
        AParentWnd := Windows.GetParent(GetOwnerObjectWindow)
      else
        AParentWnd := GetDesktopWindow;
      if cxGetAccessibleObjectFromWindow(AParentWnd, OBJID_WINDOW,
        IID_IcxAccessible, ppdispParent) <> S_OK then
          ppdispParent := nil;
    end;
    if ppdispParent <> nil then
      Result := S_OK
    else
      Result := S_FALSE;
  end;
end;

function TcxAccessibilityHelper.Get_accRole(varChild: OleVariant;
  out pvarRole: OleVariant): HResult;
var
  ASimpleChildElementID: TcxAccessibleSimpleChildElementID;
begin
  if GetSimpleChildElementID(varChild, ASimpleChildElementID, Result) then
  begin
    TVarData(pvarRole).VType := VT_I4;
    pvarRole := GetRole(ASimpleChildElementID);
    Result := S_OK;
  end;
end;

function TcxAccessibilityHelper.Get_accSelection(
  out pvarChildren: OleVariant): HResult;
begin
  Result := DISP_E_MEMBERNOTFOUND; // TODO
end;

function TcxAccessibilityHelper.Get_accState(varChild: OleVariant;
  out pvarState: OleVariant): HResult;
var
  ASimpleChildElementID: TcxAccessibleSimpleChildElementID;
begin
  if GetSimpleChildElementID(varChild, ASimpleChildElementID, Result) then
  begin
    TVarData(pvarState).VType := VT_I4;
    pvarState := GetState(ASimpleChildElementID);
    Result := S_OK;
  end;
end;

function TcxAccessibilityHelper.Get_accValue(varChild: OleVariant;
  out pszValue: WideString): HResult;
var
  ASimpleChildElementID: TcxAccessibleSimpleChildElementID;
begin
  if GetSimpleChildElementID(varChild, ASimpleChildElementID, Result) then
    if aopValue in GetSupportedProperties(ASimpleChildElementID) then
    begin
      pszValue := GetValue(ASimpleChildElementID);
      Result := S_OK;
    end
    else
      Result := DISP_E_MEMBERNOTFOUND;
end;

function TcxAccessibilityHelper.Set_accName(varChild: OleVariant;
  const pszName: WideString): HResult;
begin
  Result := DISP_E_MEMBERNOTFOUND;
end;

function TcxAccessibilityHelper.Set_accValue(varChild: OleVariant;
  const pszValue: WideString): HResult;
var
  ASimpleChildElementID: TcxAccessibleSimpleChildElementID;
begin
  if GetSimpleChildElementID(varChild, ASimpleChildElementID, Result) then
    if aopValue in GetSupportedProperties(ASimpleChildElementID) then
    begin
      SetValue(ASimpleChildElementID, pszValue);
      Result := S_OK;
    end
    else
      Result := DISP_E_MEMBERNOTFOUND;
end;

function TcxAccessibilityHelper.CheckIsOwnerObjectLive(
  out AErrorCode: HResult): Boolean;
begin
  Result := FIsOwnerObjectLive;
  if not Result then
    AErrorCode := CO_E_OBJECTNOTCONNECTED;
end;

procedure TcxAccessibilityHelper.CheckSimpleChildElementToBeReturned(
  var AVarChild: OleVariant);
var
  AChild: TcxAccessibilityHelper;
begin
  if (AVarChild <> CHILDID_SELF) and
    not ChildIsSimpleElement(AVarChild - 1) then
  begin
    AChild := GetChild(AVarChild - 1);
    VariantInit(AVarChild);
    AVarChild := AChild as IDispatch;
    TVarData(AVarChild).VType := VT_DISPATCH;
  end;
end;

procedure TcxAccessibilityHelper.CheckStringToBeReturned(const AStr: WideString;
  out AResult: HResult);
begin
  if Length(AStr) <> 0 then
    AResult := S_OK
  else
    AResult := S_FALSE;
end;

function TcxAccessibilityHelper.GetSimpleChildElementID(AChildID: OleVariant;
  out ASimpleChildElementID: TcxAccessibleSimpleChildElementID;
  out AErrorCode: HResult): Boolean;
begin
  Result := CheckIsOwnerObjectLive(AErrorCode);
  if not Result then
    Exit;
  if AChildID = CHILDID_SELF then
    ASimpleChildElementID := cxAccessibleObjectSelfID
  else
    if (AChildID > GetChildCount) or not ChildIsSimpleElement(AChildID - 1) then
    begin
      AErrorCode := E_INVALIDARG;
      Result := False;
    end
    else
      ASimpleChildElementID := AChildID - 1;
end;

function TcxAccessibilityHelper.GetVisible: Boolean;
begin
  Result := States[cxAccessibleObjectSelfID] and cxSTATE_SYSTEM_INVISIBLE = 0;
end;

initialization
  FOleaccLibrary := LoadLibrary(OleaccLibraryName);
  if FOleaccLibrary <> 0 then
  begin
    FcxAccessibleObjectFromWindow := GetProcAddress(FOleaccLibrary, 'AccessibleObjectFromWindow');
    FcxLResultFromObject := GetProcAddress(FOleaccLibrary, 'LresultFromObject');
  end;

finalization
  if FOleaccLibrary <> 0 then
    FreeLibrary(FOleaccLibrary);

end.
