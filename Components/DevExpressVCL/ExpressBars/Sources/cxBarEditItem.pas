{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressBars components                                   }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSBARS AND ALL ACCOMPANYING VCL  }
{   CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY.                  }
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

unit cxBarEditItem;

{$I cxVer.inc}

interface

uses
  Types, Windows, Classes, Controls, Graphics, Messages, cxContainer, cxControls,
  cxDropDownEdit, cxEdit, cxEditConsts, cxGraphics, cxLookAndFeels, dxBar, cxGeometry, dxActions;

type
  TdxBarEditItemActionLink = class;

  { TcxItemsEditorEx }

  TcxItemsEditorEx = class(TdxItemsEditorEx)
  protected
    class procedure InitSubItem(AItemLinks: TdxBarItemLinks); override;
    class function GetAddedItemClass(const AAddedItemName: string): TdxBarItemClass; override;
    class function GetPopupItemCaption: string; override;
    class procedure InitializeAddedItem(AItemLink: TdxBarItemLink; AAddedItemName: string); override;
  end;

  { TcxCustomBarEditItem }

  TcxCustomBarEditItem = class(TdxCustomBarEdit, IcxEditRepositoryItemListener)
  private
    FBarStyleDropDownButton: Boolean;
    FEditData: TcxCustomEditData;
    FEditValue: TcxEditValue;
    FHeight: Integer;
    FPrevIsBlobEditValue: Boolean;
    FPrevOnEditValueChanged: TNotifyEvent;
    FProperties: TcxCustomEditProperties;
    FPropertiesEvents: TNotifyEvent;
    FPropertiesValue: TcxCustomEditProperties;
    FRepositoryItem: TcxEditRepositoryItem;
    FRepositoryItemValue: TcxEditRepositoryItem;

    // IcxEditRepositoryItemListener
    procedure IcxEditRepositoryItemListener.ItemRemoved = RepositoryItemItemRemoved;
    procedure IcxEditRepositoryItemListener.PropertiesChanged = RepositoryItemPropertiesChanged;
    procedure RepositoryItemItemRemoved(Sender: TcxEditRepositoryItem);
    procedure RepositoryItemPropertiesChanged(Sender: TcxEditRepositoryItem);

    procedure CustomizingDoDrawEditButtonBackground(Sender: TcxEditButtonViewInfo;
      ACanvas: TcxCanvas; const ARect: TRect; var AHandled: Boolean);
    procedure CustomizingDoGetEditDefaultButtonWidth(Sender: TcxCustomEditViewData;
      AIndex: Integer; var ADefaultWidth: Integer);

    procedure CheckIsBlobEditValue;
    procedure CreateProperties(APropertiesClass: TcxCustomEditPropertiesClass);
    procedure DestroyProperties;
    function GetActionLink: TdxBarEditItemActionLink;
    function GetCurEditValue: TcxEditValue;
    function GetPropertiesClass: TcxCustomEditPropertiesClass;
    function GetPropertiesClassName: string;
    function GetPropertiesValue: TcxCustomEditProperties;
    function GetRepositoryItemValue: TcxEditRepositoryItem;
    function IsBarCompatibleEdit(AEditProperties: TcxCustomEditProperties = nil): Boolean;
    function IsBlobEditValue: Boolean;
    function IsEditClickable: Boolean;
    function IsEditHasContent: Boolean;
    function IsEditValueStored(AFiler: TFiler): Boolean;
    procedure PropertiesChangedHandler(Sender: TObject);
    procedure PropertiesValueChanged;
    procedure ReadEditValue(AReader: TReader); overload;
  {$HINTS OFF}
    procedure ReadEditValue(AStream: TStream); overload;
  {$HINTS ON}
    procedure SetEditValue(const AValue: TcxEditValue);
    procedure SetHeight(Value: Integer);
    procedure SetProperties(Value: TcxCustomEditProperties);
    procedure SetPropertiesClass(Value: TcxCustomEditPropertiesClass);
    procedure SetPropertiesClassName(const Value: string);
    procedure SetRepositoryItem(Value: TcxEditRepositoryItem);
    procedure SetRepositoryItemValue(Value: TcxEditRepositoryItem);
    procedure SetBarStyleDropDownButton(Value: Boolean);
    procedure UpdateRepositoryItemValue;
    procedure UpdateEditProperties(AEdit: TcxCustomEdit);
    function UseBarPaintingStyle: Boolean;
    procedure WriteEditValue(AWriter: TWriter); overload;
  {$HINTS OFF}
    procedure WriteEditValue(AStream: TStream); overload;
  {$HINTS ON}
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure DrawCustomizingImage(ACanvas: TCanvas; const ARect: TRect; AState: TOwnerDrawState); override;
    procedure DrawCustomizingImageContent(ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean); override;
    function CanEdit: Boolean;
    function CaptionIsEditValue: Boolean;
    function GetActionLinkClass: TdxBarItemActionLinkClass; override;
    function GetControlClass(AIsVertical: Boolean): TdxBarItemControlClass; override;
    function HasAccel(AItemLink: TdxBarItemLink): Boolean; override;
    procedure DoEditValueChanged(Sender: TObject);
    procedure InitProperties(AProperties: TcxCustomEditProperties);
    procedure PropertiesChanged; virtual;
    procedure UpdatePropertiesValue;

    property ActionLink: TdxBarEditItemActionLink read GetActionLink;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
    procedure DoClick; override;
    function GetProperties: TcxCustomEditProperties;

    property CurEditValue: TcxEditValue read GetCurEditValue;
    property EditValue: TcxEditValue read FEditValue write SetEditValue stored False;
    property Height: Integer read FHeight write SetHeight default 0;
    property Properties: TcxCustomEditProperties read FProperties write SetProperties;
    property PropertiesClass: TcxCustomEditPropertiesClass read GetPropertiesClass write SetPropertiesClass;
    property RepositoryItem: TcxEditRepositoryItem read FRepositoryItem write SetRepositoryItem;
  published
    property PropertiesClassName: string read GetPropertiesClassName write SetPropertiesClassName;
    property PropertiesEvents: TNotifyEvent read FPropertiesEvents write FPropertiesEvents;
    property BarStyleDropDownButton: Boolean read FBarStyleDropDownButton write SetBarStyleDropDownButton default True;
  end;

  { TdxBarEditItemActionLink }

  TdxBarEditItemActionLink = class(TdxBarItemActionLink,
    IUnknown,
    IdxActionValueClient)
  strict private
    FUpdateValueLockCount: Integer;

    // IdxActionValueClient
    procedure ActionValueChanged(const AValue: Variant);

    // IUnknown
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  protected
    procedure UpdateValue(const AValue: Variant);
  end;

  { TcxBarEditItem }

  TcxBarEditItem = class(TcxCustomBarEditItem)
  published
    property CanSelect;
    property EditValue;
    property Height;
    property LargeGlyph;
    property Properties;
    property RepositoryItem;
    property StyleEdit;
  end;

  TcxBarEditItemControlEditEvents = record
    OnAfterKeyDown: TKeyEvent;
    OnChange: TNotifyEvent;
    OnClosePopup: TcxEditClosePopupEvent;
    OnFocusChanged: TNotifyEvent;
    OnInitPopup: TNotifyEvent;
    OnKeyDown: TKeyEvent;
    OnKeyPress: TKeyPressEvent;
    OnKeyUp: TKeyEvent;
    OnMouseMove: TMouseMoveEvent;
    OnPostEditValue: TNotifyEvent;
    OnValidate: TcxEditValidateEvent;
  end;

  { TcxBarEditItemControl }

  TcxBarEditItemControl = class(TdxBarCustomEditControl, IcxEditOwner)
  private
    FEdit: TcxCustomEdit;
    FEditValueBeforeHiding: Variant;
    FEditViewInfo: TcxCustomEditViewInfo;
    FInternalProperties: TcxCustomEditProperties;
    FIsEditValueAssigned: Boolean;
    FLastEditPart: Integer;
    FSavedEditEvents: TcxBarEditItemControlEditEvents;

    procedure ClearEditEvents;
    procedure InternalShowEdit;
    procedure SaveEditEvents;

    procedure DoAfterKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure DoCanStartButtonFading(Sender: TcxCustomEditViewInfo; var ACanStart: Boolean);
    procedure DoCustomDrawPopupBorder(AViewInfo: TcxContainerViewInfo;
      ACanvas: TcxCanvas; const R: TRect; var AHandled: Boolean; out ABorderWidth: Integer);
    procedure DoDrawEditBackground(Sender: TcxCustomEditViewInfo;
      ACanvas: TcxCanvas; var AHandled: Boolean);
    procedure DoDrawEditButton(Sender: TcxEditButtonViewInfo; ACanvas: TcxCanvas; var AHandled: Boolean);
    procedure DoDrawEditButtonBackground(Sender: TcxEditButtonViewInfo;
      ACanvas: TcxCanvas; const ARect: TRect; var AHandled: Boolean);
    procedure DoDrawEditButtonBorder(Sender: TcxEditButtonViewInfo;
      ACanvas: TcxCanvas; var ABackgroundRect, AContentRect: TRect; var AHandled: Boolean);
    procedure DoEditPaint(Sender: TcxCustomEditViewInfo; ACanvas: TcxCanvas);
    procedure DoEditClosePopup(Sender: TcxControl; AReason: TcxEditCloseUpReason);
    procedure DoEditPropertiesChange(Sender: TObject);
    procedure DoGetEditButtonContentColor(Sender: TcxEditButtonViewInfo; var AColor: TColor);
    procedure DoGetEditButtonState(Sender: TcxEditButtonViewInfo; var AState: TcxEditButtonState);
    procedure DoGetEditDefaultButtonWidth(Sender: TcxCustomEditViewData;
      AIndex: Integer; var ADefaultWidth: Integer);
    procedure DoFocusChanged(Sender: TObject);
    procedure DoInitPopup(Sender: TObject);
    procedure DoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure DoKeyPress(Sender: TObject; var Key: Char);
    procedure DoKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure DoPostEditValue(Sender: TObject);
    procedure DoPrepareButtonFadingImageEvent(ASender: TcxEditButtonViewInfo;
      AState: TcxEditButtonState; out AImage: TcxBitmap32; var AHandled: Boolean);
    procedure DoValidate(Sender: TObject; var DisplayValue: TcxEditValue;
      var ErrorText: TCaption; var Error: Boolean);
    procedure EditClickHandler(Sender: TObject);

    procedure AssignViewInfoEvents(AViewInfo: TcxCustomEditViewInfo);
    procedure CheckHint(const APoint: TPoint);
    procedure ClearViewInfoEvents(AViewInfo: TcxCustomEditViewInfo);
    procedure DrawEditBackground(ACanvas: TcxCanvas; ARect: TRect; AColor: TColor);
    function GetBoundsRect: TRect;
    function GetCurEditValue: TcxEditValue;
    function GetDefaultEditButtonWidth(AIndex: Integer): Integer;
    function GetDropDownEdit: TcxCustomDropDownEdit;
    function GetPopupWindow: TcxCustomEditPopupWindow;
    function GetEditButtonState(AButtonViewInfo: TcxEditButtonViewInfo): Integer;
    function GetEditSize(AIsMinSize: Boolean = False): TSize;
    function GetEditStyle(AFull: Boolean = True): TcxEditStyle;
    function GetEditViewInfo: TcxCustomEditViewInfo;
    function GetItem: TcxCustomBarEditItem;
    function GetProperties: TcxCustomEditProperties;
    procedure InitEditContentParams(var AParams: TcxEditContentParams);
    procedure InternalDrawComboBoxDropDownButton(ACanvas: TcxCanvas; R: TRect);
    function IsDropDownEdit: Boolean;
    function IsPopupSideward: Boolean;
    function NeedEditShowCaption: Boolean;
    procedure PrepareEditForClose;
    procedure UpdateNormalStateImageForActiveFadingHelpers;

    procedure LockChangeEvents(ALock: Boolean; AInvokeChanged: Boolean = True);

    property DropDownEdit: TcxCustomDropDownEdit read GetDropDownEdit;
    property PopupWindow: TcxCustomEditPopupWindow read GetPopupWindow;
  protected
    procedure AcceleratorClick; override;
    function AllowSelectWithoutFocusing: Boolean; override;
    procedure CalcDrawParams(AFull: Boolean = True); override;
    procedure CalcParts; override;
    function CanHide: Boolean; override;
    function CanSelect: Boolean; override;
    procedure CheckHotTrack(APoint: TPoint); override;
    procedure ControlInactivate(Immediately: Boolean); override;
    procedure DoPaint(ARect: TRect; PaintType: TdxBarPaintType); override;
    procedure DoMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure DrawTextField; override;
    function GetControl: TControl; override;
    function GetCurrentCursor(const AMousePos: TPoint): TCursor; override;
    function GetHandle: HWND; override;
    procedure GetHintParams(out AHintText: string; out AShortCut: string); override;
    function GetDefaultHeight: Integer; override;
    function GetMinEditorWidth: Integer; override;
    function GetPartCount: Integer; override;
    function GetShowCaption: Boolean; override;
    function GetCaptionAreaWidth: Integer; override;
    function GetControlAreaWidth: Integer; override;
    function GetPossibleViewLevels: TdxBarItemViewLevels; override;
    procedure HotPartChanged; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure ParentVisibleChange(AIsShowing: Boolean); override;
    procedure RestoreDisplayValue; override;
    procedure StoreDisplayValue; override;

    procedure ActivateEdit(AByMouse: Boolean; AKey: Char = #0); override;
    procedure InitEdit; override;
    procedure Show; override;
    procedure Hide(AStoreDisplayValue: Boolean); override;

    function DrawSelected: Boolean; override;
    function IsChildWindow(AWnd: HWND): Boolean; override;
    function IsEditMultiRow: Boolean;
    function IsEditTransparent: Boolean; override;
    function IsTransparentOnGlass: Boolean; override;
    function NeedFocusOnClick: Boolean; override;
    function WantsKey(Key: Word): Boolean; override;

    procedure CalculateEditViewInfo(const ABounds: TRect; P: TPoint; AIsMouseEvent: Boolean); overload;
    procedure CalculateEditViewInfo(const ABounds: TRect; P: TPoint; AIsMouseEvent: Boolean;
      AEditViewData: TcxCustomEditViewData; AEditViewInfo: TcxCustomEditViewInfo); overload;
    function CreateEditViewData(AFull: Boolean = True): TcxCustomEditViewData;
    function CreateEditViewInfo: TcxCustomEditViewInfo;

    // IcxEditOwner
    function GetViewData(out AIsViewDataCreated: Boolean): TcxCustomEditViewData;
    procedure Invalidate(const R: TRect; AEraseBackground: Boolean = True);

    property EditViewInfo: TcxCustomEditViewInfo read GetEditViewInfo;
    property Item: TcxCustomBarEditItem read GetItem;
    property Properties: TcxCustomEditProperties read GetProperties;
  public
    destructor Destroy; override;
    function IsDroppedDown: Boolean; override;
    //
    property CurEditValue: TcxEditValue read GetCurEditValue;
    property Edit: TcxCustomEdit read FEdit;
  end;

implementation

uses
  Variants, Forms, StdCtrls, SysUtils, cxBarEditItemValueEditor, cxClasses, cxEditPaintUtils,
  cxEditRepositoryItems, cxEditUtils, cxLookAndFeelPainters, cxTextEdit, dxSpellCheckerCore, dxForms,
  cxVariants, dxBarCustomCustomizationForm, dxOffice11, dxBarStrs, dxBarSkinConsts, dxCore, dxFading, Math,
  dxDPIAwareUtils;//, cxDWMApi;

const
  MinContentWidth = 9;

type
  TControlAccess = class(TControl);
  TCustomdxBarControlAccess = class(TCustomdxBarControl);
  TcxCustomEditAccess = class(TcxCustomEdit);
  TcxCustomEditPropertiesAccess = class(TcxCustomEditProperties);
  TcxCustomEditViewInfoAccess = class(TcxCustomEditViewInfo);
  TdxBarManagerAccess = class(TdxBarManager);
  TdxBarSubMenuControlAccess = class(TdxBarSubMenuControl);
  TcxCustomDropDownEditAccess = class(TcxCustomDropDownEdit);
  TcxCustomPopupWindowAccess = class(TcxCustomPopupWindow);
  TButtonControlAccess = class(TButtonControl);

  { TFakeWinControl }

  TFakeWinControl = class(TWinControl)
  protected
    procedure CreateWnd; override;
    procedure DestroyWindowHandle; override;
    procedure DestroyWnd; override;
  public
    destructor Destroy; override;
  end;

  { TEditorParentForm }

  TEditorParentForm = class(TdxCustomForm)
  private
    FPrevActiveForm: TCustomForm;
    FPrevActiveControl: TWinControl;

    procedure CMActionExecute(var Message: TMessage); message CM_ACTIONEXECUTE;
    procedure CMActionUpdate(var Message: TMessage); message CM_ACTIONUPDATE;
    procedure CMAppSysCommand(var Message: TMessage); message CM_APPSYSCOMMAND;
    procedure WMSysCommand(var Message: TWMSysCommand); message WM_SYSCOMMAND;

    procedure Initialize;
  protected
    procedure InitializeNewForm; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SafePassFocus;
  public
    destructor Destroy; override;
    procedure GetTabOrderList(List: TList); override;
    procedure ScaleForPPI(ATargetDPI: Integer; AWindowRect: PRect = nil); override;
    function SetFocusedControl(Control: TWinControl): Boolean; override;
  end;

  { TcxBarEditItemVerticalButtonControl }

  TcxBarEditItemVerticalButtonControl = class(TdxBarButtonControl)
  strict private
    function CanEdit: Boolean;
  protected
    function CanClicked: Boolean; override;
    function CanSelect: Boolean; override;
    function GetPaintStyle: TdxBarPaintStyle; override;
  end;

var
  FDefaultRepositoryItem: TcxEditRepositoryItem;
  FEditList: TcxInplaceEditList;
  FEditorParentForm: TEditorParentForm;
  FEditStyle: TcxEditStyle;
  FFakeWinControl: TFakeWinControl;

function DefaultRepositoryItem: TcxEditRepositoryItem;

  procedure CreateDefaultRepositoryItem;
  begin
    FDefaultRepositoryItem := TcxEditRepositoryTextItem.Create(nil);
  end;

begin
  if FDefaultRepositoryItem = nil then
    CreateDefaultRepositoryItem;
  Result := FDefaultRepositoryItem;
end;

function EditList: TcxInplaceEditList;
begin
  if FEditList = nil then
    FEditList := TcxInplaceEditList.Create(nil);
  Result := FEditList;
end;

function EditorParentForm: TEditorParentForm;
begin
  if FEditorParentForm = nil then
  begin
    FEditorParentForm := TEditorParentForm.CreateNew(nil);
    FEditorParentForm.Scaled := False;
    FEditorParentForm.Position := poDesigned;
    FEditorParentForm.Name := 'EditorParentForm';
    FEditorParentForm.BorderStyle := bsNone;
  end;
  Result := FEditorParentForm;
end;

function FakeWinControl: TFakeWinControl;
begin
  if FFakeWinControl = nil then
  begin
    FFakeWinControl := TFakeWinControl.Create(nil);
    FFakeWinControl.Name := 'FakeWinControl';
  end;
  Result := FFakeWinControl;
end;

function InternalGetEditStyle(AEditProperties: TcxCustomEditProperties; ABarManager: TdxBarManager;
  APainter: TdxBarPainter; AFont: TFont; AColor, ATextColor: TColor; ADrawSelected: Boolean): TcxEditStyle;

  procedure InitEditStyle;
  begin
    FEditStyle.Font := AFont;
    if AColor <> clDefault then
      FEditStyle.Color := AColor;
    if AColor <> clDefault then
      FEditStyle.TextColor := ATextColor;

    if APainter <> nil then
      APainter.EditGetRealLookAndFeel(ABarManager, FEditStyle.LookAndFeel)
    else
      TdxBarManagerAccess(ABarManager).GetRealLookAndFeel(FEditStyle.LookAndFeel);

    if (FEditStyle.LookAndFeel.ActiveStyle = lfsFlat) and (AEditProperties.Buttons.VisibleCount <> 0) and not ADrawSelected then
      FEditStyle.LookAndFeel.SetStyle(lfsUltraFlat);

    FEditStyle.GradientButtons := True;
    FEditStyle.ButtonTransparency := ebtNone;
  end;

begin
  if FEditStyle = nil then
    FEditStyle := TcxEditStyle.Create(nil, True);
  InitEditStyle;
  Result := FEditStyle;
end;

{ TFakeWinControl }

destructor TFakeWinControl.Destroy;
begin
  inherited;
  FFakeWinControl := nil;
end;

procedure TFakeWinControl.CreateWnd;
begin
// do nothing
end;

procedure TFakeWinControl.DestroyWindowHandle;
begin
  WindowHandle := 0;
end;

procedure TFakeWinControl.DestroyWnd;
begin
  WindowHandle := 0; // because WindowHandle := Edit.Handle
end;

{ TEditorParentForm }

destructor TEditorParentForm.Destroy;
begin
  PopupChildren.Clear; // for test framework
  inherited;
end;

procedure TEditorParentForm.GetTabOrderList(List: TList);
begin
  //do nothing
end;

procedure TEditorParentForm.ScaleForPPI(ATargetDPI: Integer; AWindowRect: PRect = nil);
var
  ARect: TRect;
begin
  ARect := BoundsRect;
  inherited ScaleForPPI(ATargetDPI, @ARect);
end;

function TEditorParentForm.SetFocusedControl(Control: TWinControl): Boolean;
var
  ALink: TcxObjectLink;
  APopupWindow: TcxCustomPopupWindow;
begin
  ALink := nil;
  APopupWindow := dxBarGetParentPopupWindow(Self, True);
  if APopupWindow <> nil then
  begin
    APopupWindow.LockDeactivate(True);
    ALink := cxAddObjectLink(APopupWindow);
  end;
  try
    LockCMActivateMessages(True);
    Result := inherited SetFocusedControl(Control);
    LockCMActivateMessages(False);
  finally
    if APopupWindow <> nil then
    begin
      if ALink.Ref <> nil then
        APopupWindow.LockDeactivate(False);
      cxRemoveObjectLink(ALink);
    end;
  end;
end;

procedure TEditorParentForm.InitializeNewForm;
begin
  inherited;
  ParentBiDiMode := False;
end;

procedure TEditorParentForm.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) then
  begin
    if AComponent = FPrevActiveForm then
      FPrevActiveForm := nil;
    if AComponent = FPrevActiveControl then
      FPrevActiveControl := nil;
  end;
end;

procedure TEditorParentForm.SafePassFocus;
var
  APrevClicksDisabled: Boolean;
begin
  if FPrevActiveForm <> nil then
    if (FPrevActiveControl <> nil) and FPrevActiveControl.HandleAllocated and IsWindowVisible(FPrevActiveControl.Handle) then
    begin
      if FPrevActiveControl is TButtonControl then
      begin
        APrevClicksDisabled := TButtonControlAccess(FPrevActiveControl).ClicksDisabled;
        try
          TButtonControlAccess(FPrevActiveControl).ClicksDisabled := True;
          FPrevActiveForm.SetFocusedControl(FPrevActiveControl);
        finally
          TButtonControlAccess(FPrevActiveControl).ClicksDisabled := APrevClicksDisabled;
        end;
      end
      else
        FPrevActiveForm.SetFocusedControl(FPrevActiveControl)
    end
    else
      FPrevActiveForm.SetFocusedControl(FPrevActiveForm);
end;

procedure TEditorParentForm.Initialize;
begin
  FPrevActiveForm := dxGetScreenActiveForm;
  cxAddFreeNotification(Self, FPrevActiveForm);
  FPrevActiveControl := Screen.ActiveControl;
  cxAddFreeNotification(Self, FPrevActiveControl);
end;

procedure TEditorParentForm.WMSysCommand(var Message: TWMSysCommand);
begin
//  inherited; //# T282322
end;

procedure TEditorParentForm.CMActionExecute(var Message: TMessage);
begin
  if FPrevActiveForm <> nil then
    Message.Result := FPrevActiveForm.Perform(Message.Msg, Message.WParam, Message.LParam)
  else
    inherited;
end;

procedure TEditorParentForm.CMActionUpdate(var Message: TMessage);
begin
  if FPrevActiveForm <> nil then
    Message.Result := FPrevActiveForm.Perform(Message.Msg, Message.WParam, Message.LParam)
  else
    inherited;
end;

procedure TEditorParentForm.CMAppSysCommand(var Message: TMessage);
begin
//  inherited; //# T282322
end;

{ TcxBarEditItemVerticalButtonControl }

function TcxBarEditItemVerticalButtonControl.CanClicked: Boolean;
begin
  Result := CanEdit and inherited CanClicked;
end;

function TcxBarEditItemVerticalButtonControl.CanSelect: Boolean;
begin
  Result := CanEdit and inherited CanSelect;
end;

function TcxBarEditItemVerticalButtonControl.GetPaintStyle: TdxBarPaintStyle;
begin
  if CanEdit then
    Result := inherited GetPaintStyle
  else
    if TcxCustomBarEditItem(Item).Glyph.Empty then
      Result := psCaption
    else
      Result := psCaptionGlyph;
end;

function TcxBarEditItemVerticalButtonControl.CanEdit: Boolean;
begin
  Result := TcxCustomBarEditItem(Item).CanEdit;
end;

{ TcxItemsEditorEx }

class procedure TcxItemsEditorEx.InitSubItem(AItemLinks: TdxBarItemLinks);
var
  I: Integer;
begin
  for I := 0 to GetRegisteredEditProperties.Count - 1 do
    if GetRegisteredEditProperties.Descriptions[I] <> '' then
      BarDesignController.AddInternalItem(AItemLinks, TdxBarButton, GetRegisteredEditProperties.Descriptions[I], OnButtonClick);
end;

class function TcxItemsEditorEx.GetAddedItemClass(const AAddedItemName: string): TdxBarItemClass;
begin
  Result := TcxBarEditItem;
end;

class function TcxItemsEditorEx.GetPopupItemCaption: string;
begin
  Result := dxSBAR_CP_ADDCXITEM;
end;

class procedure TcxItemsEditorEx.InitializeAddedItem(AItemLink: TdxBarItemLink; AAddedItemName: string);
var
  APropertiesClass: TcxCustomEditPropertiesClass;
begin
  APropertiesClass := TcxCustomEditPropertiesClass(GetRegisteredEditProperties.FindByDescription(AAddedItemName));
  TcxBarEditItem(AItemLink.Item).PropertiesClass := APropertiesClass;

  if TcxBarEditItem(AItemLink.Item).IsEditClickable then
    TcxBarEditItem(AItemLink.Item).Properties.ImmediatePost := True;

  if not TcxBarEditItem(AItemLink.Item).IsEditHasContent then
  begin
    TcxBarEditItem(AItemLink.Item).ShowCaption := True;
    TcxBarEditItem(AItemLink.Item).Width := 0;
    AItemLink.ViewLayout := ivlGlyphControlCaption;
  end;
end;

{ TcxCustomBarEditItem }

constructor TcxCustomBarEditItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEditValue := Null;
  UpdatePropertiesValue;
  FPrevIsBlobEditValue := IsBlobEditValue;
  FBarStyleDropDownButton := True;
end;

destructor TcxCustomBarEditItem.Destroy;
begin
  RepositoryItem := nil;
  PropertiesClass := nil;
  SetRepositoryItemValue(nil);
  FreeAndNil(FEditData);
  inherited Destroy;
end;

procedure TcxCustomBarEditItem.Assign(Source: TPersistent);
begin
  inherited;
  if Self is TcxCustomBarEditItem then
    Properties := TcxCustomBarEditItem(Source).Properties;
end;

procedure TcxCustomBarEditItem.DoClick;
begin
  inherited DoClick;
  if not (Assigned(OnClick) or GetProperties.ReadOnly) then
    ShowValueEditor(ClickItemLink);
end;

function TcxCustomBarEditItem.GetProperties: TcxCustomEditProperties;
begin
  Result := FPropertiesValue;
  InitProperties(Result);
end;

procedure TcxCustomBarEditItem.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  if IsBlobEditValue then
    Filer.DefineBinaryProperty('InternalEditValue', ReadEditValue, WriteEditValue, IsEditValueStored(Filer))
  else
    Filer.DefineProperty('InternalEditValue', ReadEditValue, WriteEditValue, IsEditValueStored(Filer));
end;

procedure TcxCustomBarEditItem.DrawCustomizingImage(ACanvas: TCanvas; const ARect: TRect; AState: TOwnerDrawState);
begin
  if CaptionIsEditValue then
    dxBarCustomizingForm.PainterClass.DrawButtonOrSubItem(ACanvas, ARect, Self, GetTextOf(Caption), odSelected in AState)
  else
    dxBarCustomizingForm.PainterClass.DrawEdit(ACanvas, ARect, Self, odSelected in AState, UseBarPaintingStyle);
end;

procedure TcxCustomBarEditItem.DrawCustomizingImageContent(ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
var
  AEditProperties: TcxCustomEditProperties;
  AEditStyle: TcxEditStyle;
  AEditViewData: TcxCustomEditViewData;
  AEditViewInfo: TcxCustomEditViewInfo;
  ATempCanvas: TcxCanvas;
begin
  AEditProperties := nil;
  AEditViewData := nil;
  AEditViewInfo := nil;
  ATempCanvas := nil;
  try
    AEditProperties := GetProperties.CreatePreviewProperties;
    AEditStyle := InternalGetEditStyle(AEditProperties, BarManager, nil, ACanvas.Font, clWindow, ACanvas.Font.Color, ASelected);
    if UseBarPaintingStyle then
      AEditStyle.ButtonTransparency := ebtHideInactive;
    AEditViewData := AEditProperties.CreateViewData(AEditStyle, True, True);
    AEditViewData.OnGetDefaultButtonWidth := CustomizingDoGetEditDefaultButtonWidth;
    AEditViewData.EditContentParams.ExternalBorderBounds := ARect;
    AEditViewInfo := TcxCustomEditViewInfo(AEditProperties.GetViewInfoClass.Create);
    AEditViewInfo.Data := Integer(ASelected);
    AEditViewInfo.OnDrawButtonBackground := CustomizingDoDrawEditButtonBackground;
    ATempCanvas := TcxCanvas.Create(ACanvas);
    AEditViewData.UseRightToLeftAlignment := SysLocale.MiddleEast and (BarManager.BiDiMode = bdRightToLeft);
    AEditViewData.UseRightToLeftReading := (ACanvas.TextFlags and ETO_RTLREADING) <> 0;
    AEditViewData.UseRightToLeftScrollBar := SysLocale.MiddleEast and (BarManager.BiDiMode in [bdRightToLeft, bdRightToLeftNoAlign]);
    AEditViewData.EditValueToDrawValue(Null, AEditViewInfo);
    AEditViewData.Calculate(ATempCanvas, ARect, Point(-1, -1), cxmbNone, [], AEditViewInfo, False);
    AEditViewInfo.Paint(ATempCanvas);
  finally
    FreeAndNil(AEditProperties);
    FreeAndNil(AEditViewData);
    FreeAndNil(AEditViewInfo);
    FreeAndNil(ATempCanvas);
  end;
end;

function TcxCustomBarEditItem.GetActionLinkClass: TdxBarItemActionLinkClass;
begin
  Result := TdxBarEditItemActionLink;
end;

function TcxCustomBarEditItem.GetControlClass(AIsVertical: Boolean): TdxBarItemControlClass;
begin
  if AIsVertical then
    Result := TcxBarEditItemVerticalButtonControl
  else
    Result := TcxBarEditItemControl;
end;

function TcxCustomBarEditItem.HasAccel(AItemLink: TdxBarItemLink): Boolean;
begin
  Result := inherited HasAccel(AItemLink) and CanEdit;
end;

function TcxCustomBarEditItem.CanEdit: Boolean;
begin
  Result := esoEditing in GetProperties.GetSupportedOperations;
end;

function TcxCustomBarEditItem.CaptionIsEditValue: Boolean;
begin
  Result := not CanEdit and (esoShowingCaption in GetProperties.GetSupportedOperations);
end;

procedure TcxCustomBarEditItem.DoEditValueChanged(Sender: TObject);
begin
  UpdateEditProperties(Sender as TcxCustomEdit);

  EditValue := TcxCustomEdit(Sender).EditValue;
  if Assigned(FPrevOnEditValueChanged) then
    FPrevOnEditValueChanged(Sender);
end;

procedure TcxCustomBarEditItem.InitProperties(AProperties: TcxCustomEditProperties);
begin
  if AProperties <> nil then
  begin
    AProperties.LockUpdate(True);
    AProperties.IDefaultValuesProvider := nil;
    AProperties.LockUpdate(False);
  end;
end;

procedure TcxCustomBarEditItem.PropertiesChanged;
begin
  if FEditData <> nil then
    FEditData.Clear;
  CheckIsBlobEditValue;
  UpdateEx;
end;

procedure TcxCustomBarEditItem.UpdatePropertiesValue;
begin
  FPropertiesValue := GetPropertiesValue;
end;

procedure TcxCustomBarEditItem.RepositoryItemItemRemoved(
  Sender: TcxEditRepositoryItem);
begin
  RepositoryItem := nil;
end;

procedure TcxCustomBarEditItem.RepositoryItemPropertiesChanged(
  Sender: TcxEditRepositoryItem);
begin
  PropertiesChanged;
end;

procedure TcxCustomBarEditItem.CustomizingDoDrawEditButtonBackground(
  Sender: TcxEditButtonViewInfo; ACanvas: TcxCanvas; const ARect: TRect; var AHandled: Boolean);
begin
  AHandled := not Sender.EditViewInfo.NativeStyle;
  if (ACanvas <> nil) and AHandled then
    FillRectByColor(ACanvas.Handle, ARect,
      dxBarCustomizingForm.PainterClass.GetButtonColor(Self, False));
end;

procedure TcxCustomBarEditItem.CustomizingDoGetEditDefaultButtonWidth(
  Sender: TcxCustomEditViewData; AIndex: Integer; var ADefaultWidth: Integer);
begin
  if IsBarCompatibleEdit(Sender.Properties) then
    ADefaultWidth := dxBarCustomizingForm.PainterClass.GetComboBoxButtonWidth;
end;

procedure TcxCustomBarEditItem.CheckIsBlobEditValue;
begin
  if FPrevIsBlobEditValue <> IsBlobEditValue then
  begin
    FPrevIsBlobEditValue := IsBlobEditValue;
    EditValue := Null;
  end;
end;

procedure TcxCustomBarEditItem.CreateProperties(
  APropertiesClass: TcxCustomEditPropertiesClass);
begin
  if APropertiesClass <> nil then
    FProperties := APropertiesClass.Create(Self);
end;

procedure TcxCustomBarEditItem.DestroyProperties;
begin
  FreeAndNil(FProperties);
end;

function TcxCustomBarEditItem.GetActionLink: TdxBarEditItemActionLink;
begin
  Result := TdxBarEditItemActionLink(inherited ActionLink);
end;

function TcxCustomBarEditItem.GetCurEditValue: TcxEditValue;
begin
  if (CurItemLink = nil) or (CurItemLink.Control = nil) then
    Result := EditValue
  else
    Result := TcxBarEditItemControl(CurItemLink.Control).CurEditValue;
end;

function TcxCustomBarEditItem.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  if FProperties = nil then
    Result := nil
  else
    Result := TcxCustomEditPropertiesClass(FProperties.ClassType);
end;

function TcxCustomBarEditItem.GetPropertiesClassName: string;
begin
  if FProperties = nil then
    Result := ''
  else
    Result := FProperties.ClassName;
end;

function TcxCustomBarEditItem.GetPropertiesValue: TcxCustomEditProperties;
begin
  UpdateRepositoryItemValue;
  if FRepositoryItemValue <> nil then
  begin
    Result := FRepositoryItemValue.Properties;
    if FProperties <> nil then
      FProperties.OnPropertiesChanged := nil;
  end
  else
  begin
    Result := FProperties;
    FProperties.OnPropertiesChanged := PropertiesChangedHandler;
  end;
end;

function TcxCustomBarEditItem.GetRepositoryItemValue: TcxEditRepositoryItem;
begin
  if FRepositoryItem <> nil then
    Result := FRepositoryItem
  else
    if FProperties = nil then
      Result := DefaultRepositoryItem
    else
      Result := nil;
end;

function TcxCustomBarEditItem.IsBarCompatibleEdit(
  AEditProperties: TcxCustomEditProperties = nil): Boolean;
var
  AButton: TcxEditButton;
  AProperties: TcxCustomEditProperties;
  I: Integer;
begin
  Result := False;
  AProperties := AEditProperties;
  if AProperties = nil then
    AProperties := GetProperties;
  if AProperties.Buttons.VisibleCount = 1 then
    for I := 0 to AProperties.Buttons.Count - 1 do
    begin
      AButton := AProperties.Buttons[I];
      if AButton.Visible then
      begin
        Result := (AButton.Kind = bkDown) and not AButton.LeftAlignment;
        Break;
      end;
    end;
end;

function TcxCustomBarEditItem.IsBlobEditValue: Boolean;
begin
  Result := esfBlobEditValue in GetProperties.GetSpecialFeatures;
end;

function TcxCustomBarEditItem.IsEditClickable: Boolean;
begin
  Result := esfClickable in GetProperties.GetSpecialFeatures;
end;

function TcxCustomBarEditItem.IsEditHasContent: Boolean;
begin
  Result := not (esfNoContentPart in GetProperties.GetSpecialFeatures);
end;

function TcxCustomBarEditItem.IsEditValueStored(AFiler: TFiler): Boolean;

  function Equals(const V1, V2: TcxEditValue): Boolean;
  begin
    Result := (VarType(V1) = VarType(V2)) and VarEqualsExact(V1, V2);
  end;

begin
  if AFiler.Ancestor <> nil then
    Result := not (AFiler.Ancestor is TcxCustomBarEditItem) or
      not Equals(EditValue, TcxCustomBarEditItem(AFiler.Ancestor).EditValue)
  else
    Result := not VarIsNull(EditValue);
end;

procedure TcxCustomBarEditItem.PropertiesChangedHandler(Sender: TObject);
begin
  PropertiesChanged;
end;

procedure TcxCustomBarEditItem.PropertiesValueChanged;
begin
  UpdatePropertiesValue;
  if not (csDestroying in ComponentState) then
  begin
    CheckIsBlobEditValue;
    UpdateEx;
    Changed;
    dxBarDesignerModified(BarManager);
  end;
end;

procedure TcxCustomBarEditItem.ReadEditValue(AReader: TReader);
begin
  EditValue := AReader.ReadVariant;
end;

procedure TcxCustomBarEditItem.ReadEditValue(AStream: TStream);
var
  ASize: DWORD;
  S: AnsiString;
begin
  AStream.ReadBuffer(ASize, SizeOf(ASize));
  SetLength(S, ASize);
  AStream.ReadBuffer(S[1], ASize);
  EditValue := S;
end;

procedure TcxCustomBarEditItem.SetEditValue(const AValue: TcxEditValue);
begin
  if not (GetProperties.CanCompareEditValue and (VarType(AValue) = VarType(FEditValue)) and VarEqualsExact(AValue, FEditValue)) then
  begin
    FEditValue := AValue;
    if ActionLink <> nil then
      ActionLink.UpdateValue(EditValue);
    Change;
    Update;
  end;
end;

procedure TcxCustomBarEditItem.SetHeight(Value: Integer);
begin
  if Value <> FHeight then
  begin
    FHeight := Value;
    if not IsLoading then // TODO
      UpdateEx;           // TODO
  end;
end;

procedure TcxCustomBarEditItem.SetProperties(Value: TcxCustomEditProperties);
begin
  if Value <> nil then
    FProperties.Assign(Value);
end;

procedure TcxCustomBarEditItem.SetPropertiesClass(Value: TcxCustomEditPropertiesClass);
begin
  if Value <> PropertiesClass then
  begin
    if FProperties <> nil then
      Properties.LockUpdate(True);
    DestroyProperties;
    CreateProperties(Value);
    if FProperties <> nil then
      Properties.LockUpdate(False);
    PropertiesValueChanged;
  end;
end;

procedure TcxCustomBarEditItem.SetPropertiesClassName(const Value: string);
begin
  PropertiesClass := TcxCustomEditPropertiesClass(
    GetRegisteredEditProperties.FindByClassName(Value));
end;

procedure TcxCustomBarEditItem.SetRepositoryItem(Value: TcxEditRepositoryItem);
begin
  if FRepositoryItem <> Value then
  begin
    FRepositoryItem := Value;
    PropertiesValueChanged;
  end;
end;

procedure TcxCustomBarEditItem.SetRepositoryItemValue(Value: TcxEditRepositoryItem);
begin
  if Value <> FRepositoryItemValue then
  begin
    if FRepositoryItemValue <> nil then
      FRepositoryItemValue.RemoveListener(Self);
    FRepositoryItemValue := Value;
    if FRepositoryItemValue <> nil then
      FRepositoryItemValue.AddListener(Self);
  end;
end;

procedure TcxCustomBarEditItem.SetBarStyleDropDownButton(Value: Boolean);
begin
  if FBarStyleDropDownButton <> Value then
  begin
    FBarStyleDropDownButton := Value;
    UpdateEx;
  end;
end;

procedure TcxCustomBarEditItem.UpdateRepositoryItemValue;
begin
  SetRepositoryItemValue(GetRepositoryItemValue);
end;

procedure TcxCustomBarEditItem.UpdateEditProperties(AEdit: TcxCustomEdit);
var
  AProperties: TcxCustomEditProperties;
begin
  if AEdit <> nil then
  begin
    AProperties := GetProperties;
    if AEdit.ActiveProperties <> AProperties then
    begin
      AProperties.BeginUpdate;
      try
        AEdit.ActiveProperties.Update(AProperties);
      finally
        AProperties.EndUpdate(False);
      end;
    end;
  end;
end;

function TcxCustomBarEditItem.UseBarPaintingStyle: Boolean;
begin
  Result := BarStyleDropDownButton and IsBarCompatibleEdit;
end;

procedure TcxCustomBarEditItem.WriteEditValue(AWriter: TWriter);
begin
  AWriter.WriteVariant(EditValue);
end;

procedure TcxCustomBarEditItem.WriteEditValue(AStream: TStream);
var
  ASize: DWORD;
  S: AnsiString;
begin
  S := dxVariantToAnsiString(EditValue);
  ASize := Length(S);
  AStream.WriteBuffer(ASize, SizeOf(ASize));
  AStream.WriteBuffer(S[1], ASize);
end;

{ TdxBarEditItemActionLink }

procedure TdxBarEditItemActionLink.UpdateValue(const AValue: Variant);
var
  ABasicAction: TdxBasicAction;
  AEnabled: Boolean;
  AIntf: IdxActionValue;
begin
  if FUpdateValueLockCount = 0 then
  begin
    Inc(FUpdateValueLockCount);
    try
      if Supports(Action, IdxActionValue, AIntf) then
      begin
        ABasicAction := TdxBasicAction(Action);
        ABasicAction.BeginUpdate;
        try
          AEnabled := ABasicAction.Enabled;
          try
            ABasicAction.Enabled := True;
            AIntf.Value := AValue;
          finally
            ABasicAction.Enabled := AEnabled;
          end;
        finally
          ABasicAction.EndUpdate;
        end;
      end;
    finally
      Dec(FUpdateValueLockCount);
    end;
  end;
end;

procedure TdxBarEditItemActionLink.ActionValueChanged(const AValue: Variant);
begin
  if FUpdateValueLockCount = 0 then
  begin
    Inc(FUpdateValueLockCount);
    try
      TcxCustomBarEditItem(FClient).EditValue := AValue;
    finally
      Dec(FUpdateValueLockCount);
    end;
  end;
end;

function TdxBarEditItemActionLink.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

function TdxBarEditItemActionLink._AddRef: Integer;
begin
  Result := -1;
end;

function TdxBarEditItemActionLink._Release: Integer;
begin
  Result := -1;
end;

{ TcxBarEditItemControl }

destructor TcxBarEditItemControl.Destroy;
begin
  Focused := False;
  FreeAndNil(FEditViewInfo);
  FreeAndNil(FInternalProperties);
  inherited Destroy;
end;

function TcxBarEditItemControl.IsDroppedDown: Boolean;
begin
  Result := (Edit <> nil) and Edit.HasPopupWindow;
end;

procedure TcxBarEditItemControl.AcceleratorClick;
begin
  ControlClick(False);
  TcxCustomEditAccess(Edit).AcceleratorClick;
end;

function TcxBarEditItemControl.AllowSelectWithoutFocusing: Boolean;
begin
  Result := Item.IsEditClickable or inherited AllowSelectWithoutFocusing;
end;

procedure TcxBarEditItemControl.ActivateEdit(AByMouse: Boolean; AKey: Char = #0);
var
  P: TPoint;
  AActiveWinControl: TWinControl;
begin
  AActiveWinControl := FindControl(GetActiveWindow);
  if (AActiveWinControl is TCustomForm) and NeedFocusOnClick then
  begin
    FakeWinControl.Parent := AActiveWinControl;
    FakeWinControl.WindowHandle := Edit.Handle;

    LockCMActivateMessages(True);
    FakeWinControl.Action := Item.Action;
    TCustomForm(AActiveWinControl).SetFocusedControl(FakeWinControl);
    FakeWinControl.Action := nil;
    LockCMActivateMessages(False);
  end;
{$IFNDEF VCLGLASSPAINT}
  Edit.OnGlass := Parent.IsOnGlass;
{$ELSE}
  if Parent.IsOnGlass then
    Edit.ActiveControl.ControlState := Edit.ActiveControl.ControlState + [csGlassPaint];
{$ENDIF}
  if not AByMouse then
    if AKey = #0 then
      Edit.Activate(Item.FEditData, NeedFocusOnClick)
    else
      Edit.ActivateByKey(AKey, Item.FEditData)
  else
  begin
    P := Edit.Parent.ScreenToClient(GetMouseCursorPos);
    Edit.ActivateByMouse(KeyboardStateToShiftState, P.X, P.Y, Item.FEditData);
  end;
  if Edit <> nil then
    Edit.InplaceParams.MultiRowParent := False;
end;

procedure TcxBarEditItemControl.CalcDrawParams(AFull: Boolean = True);
begin
  inherited;
  if AFull then
    FDrawParams.DroppedDown := Focused and Edit.HasPopupWindow;
end;

procedure TcxBarEditItemControl.CalcParts;
begin
  inherited;
  if Item.UseBarPaintingStyle then
    Painter.CalculateComboParts(DrawParams, FParts, FAreaParts);
end;

function TcxBarEditItemControl.CanHide: Boolean;
begin
  Result := inherited CanHide and (not IsDropDownEdit or DropDownEdit.CanHide);
end;

function TcxBarEditItemControl.CanSelect: Boolean;
begin
  Result := inherited CanSelect and (Item.CanEdit or Parent.IsCustomizing);
end;

procedure TcxBarEditItemControl.CheckHotTrack(APoint: TPoint);
var
  ATempViewInfo: TcxCustomEditViewInfo;
begin
  inherited;
  ATempViewInfo := CreateEditViewInfo;
  try
    ATempViewInfo.Assign(EditViewInfo);
    CalculateEditViewInfo(GetBoundsRect, APoint, True);
    CheckHint(APoint);
    EditViewInfo.Repaint(Parent, ATempViewInfo);
  finally
    ATempViewInfo.Free;
  end;
end;

// TODO
procedure TcxBarEditItemControl.ControlInactivate(Immediately: Boolean);
begin
//  Focused := False;
  DisableAppWindows(not IsApplicationActive);
  try
    inherited ControlInactivate(Immediately);
  finally
    EnableAppWindows;
  end;
end;

procedure TcxBarEditItemControl.DoPaint(ARect: TRect; PaintType: TdxBarPaintType);
begin
  if Edit <> nil then
    Edit.InvalidateWithChildren;
  inherited;
end;

procedure TcxBarEditItemControl.DoMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  P: TPoint;
begin
  if Assigned(FSavedEditEvents.OnMouseMove) then
    FSavedEditEvents.OnMouseMove(Sender, Shift, X, Y);
  P := ClientToParent(Point(X, Y));
  MouseMove(Shift, P.X, P.Y);
end;

procedure TcxBarEditItemControl.DrawTextField;

  function HasEditButtonCompositeFrame: Boolean;
  begin
    Result := Painter.EditButtonAllowCompositeFrame and Item.UseBarPaintingStyle;
  end;

begin
  if not Focused or HasEditButtonCompositeFrame then
  begin
    CalculateEditViewInfo(GetBoundsRect, Parent.ScreenToClient(GetMouseCursorPos), False);
    EditViewInfo.Data := Integer(FDrawParams.PaintType);
    Canvas.SaveState;
    try
      EditViewInfo.BackgroundColor := cxGetActualColor(Painter.EditControlGetBackgroundColor(DrawParams), EditViewInfo.BackgroundColor);
      EditViewInfo.TextColor := cxGetActualColor(Painter.EditControlGetTextColor(DrawParams), EditViewInfo.TextColor);
      EditViewInfo.Paint(Canvas);
    finally
      Canvas.RestoreState;
    end;
  end;
end;

function TcxBarEditItemControl.DrawSelected: Boolean;
begin
  Result := inherited DrawSelected and (NeedFocusOnClick or IsSelected and not FSelectedByMouse);
end;

function TcxBarEditItemControl.GetControl: TControl;
begin
  Result := Edit;
end;

function TcxBarEditItemControl.GetCurrentCursor(const AMousePos: TPoint): TCursor;
begin
  if Enabled then
    Result := TcxCustomEditViewInfoAccess(EditViewInfo).GetCurrentCursor(AMousePos)
  else
    Result := inherited GetCurrentCursor(AMousePos);
end;

function TcxBarEditItemControl.GetHandle: HWND;
begin
  if (Edit <> nil) and Edit.HandleAllocated then
    Result := Edit.Handle
  else
    Result := 0;
end;

procedure TcxBarEditItemControl.GetHintParams(out AHintText: string; out AShortCut: string);
begin
  AHintText := GetShortHint(TcxCustomEditViewInfoAccess(EditViewInfo).GetHintText(
    TcxCustomEditViewInfoAccess(EditViewInfo).GetPart(Parent.ScreenToClient(GetMouseCursorPos))));
  AShortCut := '';
  if AHintText = '' then
    inherited GetHintParams(AHintText, AShortCut);
end;

function TcxBarEditItemControl.GetDefaultHeight: Integer;
begin
  Result := inherited GetDefaultHeight;
  Result := Max(Result, Item.Height);
  Result := Max(Result, GetEditSize.cy);
end;

function TcxBarEditItemControl.GetMinEditorWidth: Integer;
begin
  Result := GetEditSize(True).cx;
end;

function TcxBarEditItemControl.GetPartCount: Integer;
begin
  Result := inherited GetPartCount;
  if Item.UseBarPaintingStyle then
    Inc(Result);
end;

function TcxBarEditItemControl.GetShowCaption: Boolean;
begin
  if Item.CaptionIsEditValue then
    Result := False
  else
    Result := inherited GetShowCaption;
end;

function TcxBarEditItemControl.GetViewData(out AIsViewDataCreated: Boolean): TcxCustomEditViewData;
begin
  Result := CreateEditViewData;
  AIsViewDataCreated := True;
end;

function TcxBarEditItemControl.GetCaptionAreaWidth: Integer;
begin
  if NeedEditShowCaption then
    if (GetPaintType = ptMenu) and Painter.SubMenuControlHasBand and not (cpIcon in GetViewStructure) then
      Result := TdxBarSubMenuControlAccess(SubMenuParent).BandSize
    else
      Result := 0
  else
    Result := inherited GetCaptionAreaWidth;
end;

function TcxBarEditItemControl.GetControlAreaWidth: Integer;
begin
  if NeedEditShowCaption and (esoAutoWidth in GetProperties.GetSupportedOperations) then
    Result := GetMinEditorWidth
  else
    Result := inherited GetControlAreaWidth;
end;

function TcxBarEditItemControl.GetPossibleViewLevels: TdxBarItemViewLevels;
begin
  Result := inherited GetPossibleViewLevels;
  if Item.CaptionIsEditValue then
    Result := Result - [ivlSmallIconWithText];
  if IsEditMultiRow then
    Result := Result + [ivlLargeIconWithText, ivlLargeControlOnly];
end;

procedure TcxBarEditItemControl.Hide(AStoreDisplayValue: Boolean);
begin
  if Edit <> nil then
    if not (csDestroying in BarManager.ComponentState) then
    begin
      if not IsWindowEnabled {or not CanHide} then
      begin
        Item.FPrevOnEditValueChanged := Edit.InternalProperties.OnEditValueChanged;
        Edit.InternalProperties.OnEditValueChanged := Item.DoEditValueChanged;
      end;
      FEditValueBeforeHiding := Edit.EditingValue;
      FIsEditValueAssigned := False;

      LockCMActivateMessages(True);
      FakeWinControl.Parent := nil;
      LockCMActivateMessages(False);

      EditorParentForm.SetBounds(0, 0, 0, 0);
      DisableAppWindows(not IsApplicationActive);
      try
        if EditorParentForm.HandleAllocated then
          EditorParentForm.FocusControl(nil); // must be before Parent:=nil because DoEnter in WinControl Activate
        Edit.Parent := nil; // must be before DefocusControl
        if IsDropDownEdit then
        begin
          PopupWindow.ActiveControl := nil;
          TcxCustomPopupWindowAccess(PopupWindow).FPrevActiveForm := EditorParentForm.FPrevActiveForm;
          TcxCustomPopupWindowAccess(PopupWindow).FPrevActiveControl := EditorParentForm.FPrevActiveControl;
        end;

        LockCMActivateMessages(True);
        EditorParentForm.SafePassFocus;
        LockCMActivateMessages(false);
      finally
        EnableAppWindows;
      end;
      EditorParentForm.DefocusControl(Edit, True); // must be before ClearEditEvents;
      ClearEditEvents;
      ClearViewInfoEvents(Edit.ViewInfo);
      EditorParentForm.Visible := False;
      EditorParentForm.ParentWindow := 0;
      Item.DoExit;
      if AStoreDisplayValue then
        StoreDisplayValue;
      FEdit := nil;
    end
    else
    begin
      ClearEditEvents;
      ClearViewInfoEvents(Edit.ViewInfo);
    end;
end;

procedure TcxBarEditItemControl.HotPartChanged;
begin
  if HotPartIndex = icpNone then
    UpdateNormalStateImageForActiveFadingHelpers;
  inherited HotPartChanged;
end;

procedure TcxBarEditItemControl.InitEdit;

  procedure SetEditButtonsWidth;
  var
    AButton: TcxEditButton;
    I: Integer;
  begin
    for I := 0 to Edit.InternalProperties.Buttons.Count - 1 do
    begin
      AButton := Edit.InternalProperties.Buttons[I];
      if AButton.Visible and (AButton.Width = 0) then
        AButton.Width := GetDefaultEditButtonWidth(I);
    end;
  end;

  procedure InitEditProperties(AEditProperties: TcxCustomEditProperties);
  begin
    LockChangeEvents(True);
    try
      Edit.InternalProperties.Assign(AEditProperties);
      if not (esoNeedHandle in Properties.GetSupportedOperations) then
        RestoreDisplayValue;
      SetEditButtonsWidth;
    finally
      LockChangeEvents(False);
    end;
  end;

  procedure AssignEditEvents(AEditProperties: TcxCustomEditProperties);
  begin
    SaveEditEvents;
    Edit.InternalProperties.OnChange := DoEditPropertiesChange;
    Edit.InternalProperties.OnClosePopup := DoEditClosePopup;
    if Assigned(Edit.InternalProperties.OnValidate) then
      Edit.InternalProperties.OnValidate := DoValidate;
    Edit.OnFocusChanged := DoFocusChanged;
    Edit.OnKeyDown := DoKeyDown;
    Edit.OnKeyPress := DoKeyPress;
    Edit.OnKeyUp := DoKeyUp;
    Edit.OnMouseMove := DoMouseMove;
    Edit.OnPostEditValue := DoPostEditValue;
    if not Assigned(Edit.InternalProperties.OnEditValueChanged) then
    begin
      if Item.Properties <> nil then
        Edit.InternalProperties.OnEditValueChanged := Item.Properties.OnEditValueChanged;
      if Assigned(AEditProperties.OnEditValueChanged) then
        Edit.InternalProperties.OnEditValueChanged := Properties.OnEditValueChanged;
    end;

    if IsDropDownEdit then
    begin
      DropDownEdit.OnAfterKeyDown := DoAfterKeyDown;
      DropDownEdit.Properties.OnInitPopup := DoInitPopup;
      PopupWindow.ViewInfo.OnCustomDrawBorder := DoCustomDrawPopupBorder;
    end;

//    if IsEditClickable then
      Edit.OnClick := EditClickHandler;
  end;

var
  ISpellCheckerEdit: IdxSpellCheckerControl;
  AProperties: TcxCustomEditProperties;
begin
  AProperties := Properties;
  FEdit := EditList.GetEdit(TcxCustomEditPropertiesClass(AProperties.ClassType));
  TcxControlHelper.ChangeScaleFactor(FEdit, ScaleFactor);
  if Supports(TObject(FEdit), IdxSpellCheckerControl, ISpellCheckerEdit) then
     ISpellCheckerEdit.SetIsBarControl(True);
  InitEditProperties(AProperties);
  Edit.Style := GetEditStyle;
  InitEditContentParams(Edit.ContentParams);
  AssignViewInfoEvents(Edit.ViewInfo);
  AssignEditEvents(AProperties);

  TcxCustomEditAccess(Edit).NeedFocusOnClick := NeedFocusOnClick;
  TcxCustomEditAccess(Edit).PrepareEditForInplaceActivation;
end;

function TcxBarEditItemControl.IsChildWindow(AWnd: HWND): Boolean;
begin
  Result := inherited IsChildWindow(AWnd) or (Edit <> nil) and (Edit.IsChildWindow(AWnd));
end;

function TcxBarEditItemControl.IsEditMultiRow: Boolean;
begin
  Result := esfMultiRow in Properties.GetSpecialFeatures;
end;

function TcxBarEditItemControl.IsEditTransparent: Boolean;
begin
  Result := esoTransparency in Properties.GetSupportedOperations;
end;

function TcxBarEditItemControl.IsTransparentOnGlass: Boolean;
begin
  Result := IsEditTransparent;
end;

function TcxBarEditItemControl.NeedFocusOnClick: Boolean;
begin
  Result := not Item.IsEditClickable or TCustomdxBarControlAccess(Parent).IsPopup or not Properties.ImmediatePost;
end;

procedure TcxBarEditItemControl.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if not Focused then
  begin
    Click(False, Char(Key));
    Key := 0;
  end
  else
    inherited KeyDown(Key, Shift);
end;

procedure TcxBarEditItemControl.ParentVisibleChange(AIsShowing: Boolean);
const
  AShowParams: array[Boolean] of Integer = (SW_HIDE, SW_SHOWNOACTIVATE);
begin
  if IsDropDownEdit and DropDownEdit.HasPopupWindow then
    ShowWindow(PopupWindow.Handle, AShowParams[AIsShowing]);
end;

procedure TcxBarEditItemControl.RestoreDisplayValue;
begin
  if not FIsEditValueAssigned then
  begin
    LockChangeEvents(True);
    try
      if CanSelect then
      begin
        Edit.EditValue := Item.EditValue;
        FIsEditValueAssigned := True;
        if esoShowingCaption in Properties.GetSupportedOperations then
          if NeedEditShowCaption then
            TControlAccess(Edit).Caption := Caption
          else
            TControlAccess(Edit).Caption := '';
      end;
    finally
      LockChangeEvents(False, False);
    end;
  end;
end;

procedure TcxBarEditItemControl.Show;
begin
  inherited Show;
  InternalShowEdit;
  RestoreDisplayValue;
  Item.DoEnter;
end;

procedure TcxBarEditItemControl.StoreDisplayValue;
begin
  PrepareEditForClose;
  if (Edit <> nil) and FIsEditValueAssigned then
    Item.EditValue := Edit.EditValue
  else
    Item.EditValue := FEditValueBeforeHiding;
end;

procedure TcxBarEditItemControl.UpdateNormalStateImageForActiveFadingHelpers;
var
  I: Integer;
begin
  for I := 0 to Length(EditViewInfo.ButtonsInfo) - 1 do
  begin
    if EditViewInfo.ButtonsInfo[I].FadingHelper.Active then
      EditViewInfo.ButtonsInfo[I].FadingHelper.CheckStartFading(cxbsHot, cxbsNormal);
  end;
end;

function TcxBarEditItemControl.WantsKey(Key: Word): Boolean;
begin
  Result := inherited WantsKey(Key) or Properties.IsActivationKey(Char(Key));
end;

procedure TcxBarEditItemControl.CalculateEditViewInfo(const ABounds: TRect; P: TPoint; AIsMouseEvent: Boolean);
var
  AViewData: TcxCustomEditViewData;
begin
  AViewData := CreateEditViewData(True);
  try
    CalculateEditViewInfo(ABounds, P, AIsMouseEvent, AViewData, EditViewInfo);
    InternalCalculateParts;
  finally
    AViewData.Free;
  end;
end;

procedure TcxBarEditItemControl.CalculateEditViewInfo(const ABounds: TRect; P: TPoint;
  AIsMouseEvent: Boolean; AEditViewData: TcxCustomEditViewData; AEditViewInfo: TcxCustomEditViewInfo);

  function CanHotTrack: Boolean;
  begin
    Result := not (Item.IsDesigning or Parent.IsCustomizing) and IsWindowEnabled and Painter.EditButtonAllowHotTrack(FDrawParams);
  end;

var
  ACaptionIsEditValue: Boolean;
begin
  ACaptionIsEditValue := Item.CaptionIsEditValue;

  if not (cxPointIsInvalid(P) or CanHotTrack) then
    P := cxInvalidPoint;

  AssignViewInfoEvents(AEditViewInfo);
  if ACaptionIsEditValue then
    AEditViewData.EditValueToDrawValue(Caption, AEditViewInfo)
  else
    AEditViewData.EditValueToDrawValue(Item.EditValue, AEditViewInfo);

  if not ACaptionIsEditValue and NeedEditShowCaption then
    TcxCustomTextEditViewInfo(AEditViewInfo).Text := Caption;

  Canvas.SaveState;
  try
    AEditViewData.Calculate(Canvas, ABounds, P, cxmbNone, [], AEditViewInfo, AIsMouseEvent);
  finally
    Canvas.RestoreState;
  end;
end;

function TcxBarEditItemControl.CreateEditViewData(AFull: Boolean = True): TcxCustomEditViewData;
begin
  Result := Properties.CreateViewData(GetEditStyle(AFull), True);
  Result.Enabled := Enabled;
  if AFull then
  begin
    InitEditContentParams(Result.EditContentParams);
    Result.UseRightToLeftAlignment := UseRightToLeftAlignment;
    Result.UseRightToLeftReading := Parent.UseRightToLeftReading;
    Result.UseRightToLeftScrollBar := Parent.UseRightToLeftScrollBar;
  end;
  Result.OnGetDefaultButtonWidth := DoGetEditDefaultButtonWidth;
end;

function TcxBarEditItemControl.CreateEditViewInfo: TcxCustomEditViewInfo;
begin
  Result := TcxCustomEditViewInfo(Properties.GetViewInfoClass.Create);
  Result.Owner := Self;
end;

procedure TcxBarEditItemControl.ClearEditEvents;
begin
  Edit.InternalProperties.OnChange := nil;
  Edit.InternalProperties.OnClosePopup := nil;
  Edit.InternalProperties.OnValidate := nil;
  Edit.OnFocusChanged := nil;
  Edit.OnKeyDown := nil;
  Edit.OnKeyPress := nil;
  Edit.OnKeyUp := nil;
  Edit.OnMouseMove := nil;
  Edit.OnPostEditValue := nil;

  if IsDropDownEdit then
  begin
    DropDownEdit.OnAfterKeyDown := nil;
    DropDownEdit.Properties.OnInitPopup := nil;
    PopupWindow.ViewInfo.OnCustomDrawBorder := nil;
  end;

//  if IsEditClickable then
   Edit.OnClick := nil;
end;

procedure TcxBarEditItemControl.InternalDrawComboBoxDropDownButton(ACanvas: TcxCanvas; R: TRect);
var
  APrevWindowOrg: TPoint;
  AOriginalCanvas: TcxCanvas;
begin
  BeforeDrawBackground(Edit, Parent, ACanvas.Handle, R, APrevWindowOrg);
  try
    AOriginalCanvas := FDrawParams.Canvas;
    try
      FDrawParams.Canvas := ACanvas;
      Painter.ComboControlDrawArrowButton(DrawParams, R, True);
    finally
      FDrawParams.Canvas := AOriginalCanvas
    end;
  finally
    AfterDrawBackground(ACanvas.Handle, APrevWindowOrg);
  end;
end;

procedure TcxBarEditItemControl.InternalShowEdit;
begin
  EditorParentForm.Initialize;
  EditorParentForm.ParentWindow := Parent.Handle;
  EditorParentForm.BoundsRect := GetBoundsRect;
  EditorParentForm.ScaleForPPI(ScaleFactor.Apply(dxDefaultDPI));
  Edit.Align := alClient;
  Edit.Parent := EditorParentForm;
  FEdit.BiDiMode := Parent.BiDiMode;
  Edit.Visible := True;
  TControl(EditorParentForm).Visible := True;
end;

procedure TcxBarEditItemControl.Invalidate(const R: TRect; AEraseBackground: Boolean);
begin
  Update(R);
end;

procedure TcxBarEditItemControl.SaveEditEvents;
begin
  FSavedEditEvents.OnChange := Edit.InternalProperties.OnChange;
  FSavedEditEvents.OnClosePopup := Edit.InternalProperties.OnClosePopup;
  FSavedEditEvents.OnValidate := Edit.InternalProperties.OnValidate;
  FSavedEditEvents.OnFocusChanged := Edit.OnFocusChanged;
  FSavedEditEvents.OnKeyDown := Edit.OnKeyDown;
  FSavedEditEvents.OnKeyPress := Edit.OnKeyPress;
  FSavedEditEvents.OnKeyUp := Edit.OnKeyUp;
  FSavedEditEvents.OnMouseMove := Edit.OnMouseMove;
  FSavedEditEvents.OnPostEditValue := Edit.OnPostEditValue;

  if IsDropDownEdit then
  begin
    FSavedEditEvents.OnAfterKeyDown := DropDownEdit.OnAfterKeyDown;
    FSavedEditEvents.OnInitPopup := DropDownEdit.Properties.OnInitPopup;
  end;
end;

procedure TcxBarEditItemControl.DoAfterKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Assigned(FSavedEditEvents.OnAfterKeyDown) then
    FSavedEditEvents.OnAfterKeyDown(Sender, Key, Shift);
  if Key = VK_TAB then
    KeyDown(Key, Shift);
end;

procedure TcxBarEditItemControl.DoCanStartButtonFading(Sender: TcxCustomEditViewInfo; var ACanStart: Boolean);
begin
  ACanStart := ACanStart and not (csDestroying in Parent.ComponentState) and Painter.IsFadingAvailable;
end;

procedure TcxBarEditItemControl.DoCustomDrawPopupBorder(AViewInfo: TcxContainerViewInfo;
  ACanvas: TcxCanvas; const R: TRect; var AHandled: Boolean; out ABorderWidth: Integer);
begin
  AHandled := True;
  ABorderWidth := Painter.GetPopupWindowBorderWidth;
  if (ACanvas <> nil) then
    Painter.DropDownListBoxDrawBorder(ACanvas.Handle, AViewInfo.BackgroundColor, R);
end;

procedure TcxBarEditItemControl.DoDrawEditBackground(
  Sender: TcxCustomEditViewInfo; ACanvas: TcxCanvas; var AHandled: Boolean);
var
  APrevWindowOrg: TPoint;
  ABounds: TRect;
begin
  AHandled := IsEditTransparent;
  if AHandled and (ACanvas <> nil) then
  begin
    ABounds := Sender.Bounds;
    BeforeDrawBackground(Sender.Edit, Edit, ACanvas.Handle, ABounds, APrevWindowOrg);
    try
      DrawEditBackground(ACanvas, ABounds, Sender.BackgroundColor);
    finally
      AfterDrawBackground(ACanvas.Handle, APrevWindowOrg);
    end;
  end;
end;

procedure TcxBarEditItemControl.DoDrawEditButton(
  Sender: TcxEditButtonViewInfo; ACanvas: TcxCanvas; var AHandled: Boolean);
var
  AIsParentCanvas: Boolean;
  ARect: TRect;
begin
  AHandled := Item.UseBarPaintingStyle;
  if AHandled and (ACanvas <> nil) then
  begin
    AIsParentCanvas := ACanvas.Handle = Canvas.Handle;
    ARect := FParts[ccpDropButton];
    if (Edit <> nil) and Edit.HandleAllocated and not AIsParentCanvas then
      ARect := dxMapWindowRect(Parent.Handle, Edit.Handle, ARect);
    if not Sender.FadingHelper.DrawImage(ACanvas.Handle, ARect) then
      InternalDrawComboBoxDropDownButton(ACanvas, ARect);
    if Painter.EditButtonAllowCompositeFrame and not AIsParentCanvas then
      Painter.ComboControlDrawArrowButton(DrawParams, FParts[ccpDropButton], False);
  end;
end;

procedure TcxBarEditItemControl.DoDrawEditButtonBackground(
  Sender: TcxEditButtonViewInfo; ACanvas: TcxCanvas; const ARect: TRect; var AHandled: Boolean);
var
  APrevWindowOrg: TPoint;
  AOriginalCanvas: TcxCanvas;
  AButtonState: Integer;
  ABackgroundRect: TRect;
begin
  AButtonState := GetEditButtonState(Sender);
  AHandled := Painter.EditButtonIsCustomBackground(AButtonState);

  if AHandled and (ACanvas <> nil) then
  begin
    ABackgroundRect := ARect;
    BeforeDrawBackground(Edit, Parent, ACanvas.Handle, ABackgroundRect, APrevWindowOrg);
    try
      AOriginalCanvas := FDrawParams.Canvas;
      try
        FDrawParams.Canvas := ACanvas;
        Painter.EditButtonDrawBackground(DrawParams, AButtonState,
          ABackgroundRect, GetSolidBrush(Sender.EditViewInfo.BackgroundColor));
      finally
        FDrawParams.Canvas := AOriginalCanvas
      end;
    finally
      AfterDrawBackground(ACanvas.Handle, APrevWindowOrg);
    end;
  end;
end;

procedure TcxBarEditItemControl.DoDrawEditButtonBorder(
  Sender: TcxEditButtonViewInfo; ACanvas: TcxCanvas;
  var ABackgroundRect, AContentRect: TRect; var AHandled: Boolean);
var
  APrevWindowOrg: TPoint;
  AOriginalCanvas: TcxCanvas;
  AOffset: TPoint;
begin
  AHandled := Painter.EditButtonIsCustomBorder;
  if (ACanvas <> nil) and AHandled then
  begin
    ABackgroundRect := Sender.Bounds;
    AOffset := BeforeDrawBackground(Edit, Parent, ACanvas.Handle, ABackgroundRect, APrevWindowOrg);
    try
      AOriginalCanvas := FDrawParams.Canvas;
      try
        FDrawParams.Canvas := ACanvas;
        Painter.EditButtonDrawBorder(FDrawParams, GetEditButtonState(Sender), ABackgroundRect, AContentRect);
      finally
        FDrawParams.Canvas := AOriginalCanvas
      end;
    finally
      AfterDrawBackground(ACanvas.Handle, APrevWindowOrg);
    end;
    ABackgroundRect := cxRectOffset(ABackgroundRect, cxPointInvert(AOffset));
    AContentRect := cxRectOffset(AContentRect, cxPointInvert(AOffset));
  end;
end;

procedure TcxBarEditItemControl.DoEditPaint(Sender: TcxCustomEditViewInfo; ACanvas: TcxCanvas);
begin
  CalcDrawParams;
end;

procedure TcxBarEditItemControl.DoEditClosePopup(Sender: TcxControl;
  AReason: TcxEditCloseUpReason);
begin
  if Assigned(FSavedEditEvents.OnClosePopup) then
    FSavedEditEvents.OnClosePopup(Sender, AReason);

  case AReason of
    crCancel: DoEscape;
    crEnter: DoEnter;
    crTab: DoNavigation([]);
  end;
end;

procedure TcxBarEditItemControl.DoEditPropertiesChange(Sender: TObject);
var
  AItem: TcxCustomBarEditItem;
begin
  AItem := Item; // if ItemControl destroyed in OnChange
  dxCallNotify(FSavedEditEvents.OnChange, Sender);
  AItem.CurChange;
end;

procedure TcxBarEditItemControl.DoGetEditButtonContentColor(Sender: TcxEditButtonViewInfo; var AColor: TColor);
begin
  if IsHighContrast then
    case Sender.Data.State of
      ebsDisabled:
        AColor := clGrayText;
      ebsNormal:
        if DrawSelected or (HotPartIndex = ecpEdit) then
          AColor := clHighlightText
        else
          AColor := clBtnText;
      ebsPressed, ebsSelected:
        AColor := clHighlightText;
    end;
end;

procedure TcxBarEditItemControl.DoGetEditButtonState(
  Sender: TcxEditButtonViewInfo; var AState: TcxEditButtonState);
begin
  if Item.UseBarPaintingStyle and (AState = ebsNormal) then
  begin
    if Focused or DrawSelected or (HotPartIndex = ccpDropButton) and CanSelect then
      AState := ebsSelected;
  end;
end;

procedure TcxBarEditItemControl.DoGetEditDefaultButtonWidth(
  Sender: TcxCustomEditViewData; AIndex: Integer; var ADefaultWidth: Integer);
begin
  ADefaultWidth := GetDefaultEditButtonWidth(AIndex);
end;

procedure TcxBarEditItemControl.DoFocusChanged(Sender: TObject);
begin
  dxCallNotify(FSavedEditEvents.OnFocusChanged, Sender);
  if Focused and not Edit.Focused and not Edit.IsChildWindow(GetFocus) then
    Parent.HideAll;
end;

procedure TcxBarEditItemControl.DoInitPopup(Sender: TObject);
begin
  dxCallNotify(FSavedEditEvents.OnInitPopup, Sender);
  if IsPopupSideward then
  begin
    DropDownEdit.Properties.PopupDirection := pdHorizontal;
    DropDownEdit.Properties.PopupVertAlignment := pavTop;
    DropDownEdit.Properties.PopupHorzAlignment := pahRight;
  end;
end;

procedure TcxBarEditItemControl.DoKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Assigned(FSavedEditEvents.OnKeyDown) then
    FSavedEditEvents.OnKeyDown(Sender, Key, Shift);
  KeyDown(Key, Shift);
end;

procedure TcxBarEditItemControl.DoKeyPress(Sender: TObject; var Key: Char);
begin
  if Assigned(FSavedEditEvents.OnKeyPress) then
    FSavedEditEvents.OnKeyPress(Sender, Key);
  KeyPress(Key);
end;

procedure TcxBarEditItemControl.DoKeyUp(
  Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Assigned(FSavedEditEvents.OnKeyUp) then
    FSavedEditEvents.OnKeyUp(Sender, Key, Shift);
  KeyUp(Key, Shift);
end;

procedure TcxBarEditItemControl.DoPostEditValue(Sender: TObject);
begin
  Item.EditValue := Edit.EditValue;
end;

procedure TcxBarEditItemControl.DoPrepareButtonFadingImageEvent(
  ASender: TcxEditButtonViewInfo; AState: TcxEditButtonState;
  out AImage: TcxBitmap32; var AHandled: Boolean);

  procedure PrepareHotPartIndex(var AHotPartIndex: Integer);
  begin
    if AState = ebsSelected then
      AHotPartIndex := ccpDropButton
    else
      if AHotPartIndex = ccpDropButton then
        AHotPartIndex := icpControl;
  end;

var
  APrevDrawSelected: Boolean;
  APrevHotPartIndex: Integer;
  APrevFocused: Boolean;
  ARect: TRect;
begin
  AHandled := Item.UseBarPaintingStyle;
  if AHandled then
  begin
    CalcDrawParams;
    ARect := FParts[ccpDropButton];
    if (Edit <> nil) and Edit.HandleAllocated then
      ARect := dxMapWindowRect(Parent.Handle, Edit.Handle, ARect);

    APrevFocused := DrawParams.Focused;
    APrevDrawSelected := DrawParams.DrawSelected;
    APrevHotPartIndex := DrawParams.HotPartIndex;
    try
      DrawParams.Focused := False;
      DrawParams.IsPressed := False;
      DrawParams.DrawSelected := AState = ebsSelected;
      PrepareHotPartIndex(DrawParams.HotPartIndex);

      AImage := TcxBitmap32.CreateSize(ARect, True);
      AImage.cxCanvas.WindowOrg := ARect.TopLeft;
      AImage.cxCanvas.UseRightToLeftAlignment := UseRightToLeftAlignment;
      InternalDrawComboBoxDropDownButton(AImage.cxCanvas, ARect);
      AImage.cxCanvas.WindowOrg := cxNullPoint;
    finally
      DrawParams.HotPartIndex := APrevHotPartIndex;
      DrawParams.DrawSelected := APrevDrawSelected;
      DrawParams.Focused := APrevFocused;
    end;
  end;
end;

procedure TcxBarEditItemControl.DoValidate(Sender: TObject;
  var DisplayValue: TcxEditValue; var ErrorText: TCaption; var Error: Boolean);
begin
  ErrorText := cxGetResourceString(@scxMaskEditInvalidEditValue);
  if Assigned(FSavedEditEvents.OnValidate) then
    FSavedEditEvents.OnValidate(Sender, DisplayValue, ErrorText, Error);
end;

procedure TcxBarEditItemControl.EditClickHandler(Sender: TObject);
begin
  if not Edit.Focused then
    Parent.PostponedHideAll(bcrEnter);
end;

function TcxBarEditItemControl.GetEditButtonState(AButtonViewInfo: TcxEditButtonViewInfo): Integer;
begin
  case AButtonViewInfo.Data.State of
    ebsDisabled:
      Result := DXBAR_DISABLED;
    ebsPressed:
      Result := DXBAR_DROPPEDDOWN;
    ebsSelected:
      Result := DXBAR_HOT
    else
      if FHotPartIndex = ecpEdit then
        Result := DXBAR_ACTIVE
      else
        Result := DXBAR_NORMAL;
  end;
end;

procedure TcxBarEditItemControl.AssignViewInfoEvents(AViewInfo: TcxCustomEditViewInfo);
begin
  AViewInfo.OnDrawBackground := DoDrawEditBackground;
  AViewInfo.OnDrawButton := DoDrawEditButton;
  AViewInfo.OnDrawButtonBackground := DoDrawEditButtonBackground;
  AViewInfo.OnDrawButtonBorder := DoDrawEditButtonBorder;
  AViewInfo.OnGetButtonContentColor := DoGetEditButtonContentColor;
  AViewInfo.OnGetButtonState := DoGetEditButtonState;
  AViewInfo.OnPrepareButtonFadingImage := DoPrepareButtonFadingImageEvent;
  AViewInfo.OnCanStartButtonFading := DoCanStartButtonFading;
  AViewInfo.OnPaint := DoEditPaint;
end;

procedure TcxBarEditItemControl.CheckHint(const APoint: TPoint);
var
  AEditPart: Integer;
begin
  if FHotPartIndex = ecpEdit then
  begin
    AEditPart := TcxCustomEditViewInfoAccess(EditViewInfo).GetPart(APoint);
    if AEditPart <> FLastEditPart then
    begin
      if TcxCustomEditViewInfoAccess(EditViewInfo).GetHintText(FLastEditPart) <>
        TcxCustomEditViewInfoAccess(EditViewInfo).GetHintText(AEditPart) then
        BarManager.ActivateHint(True, '', Self);
      FLastEditPart := TcxCustomEditViewInfoAccess(EditViewInfo).GetPart(APoint);
    end;
  end
  else
    FLastEditPart := ecpNone;
end;

procedure TcxBarEditItemControl.ClearViewInfoEvents(AViewInfo: TcxCustomEditViewInfo);
begin
  AViewInfo.OnDrawBackground := nil;
  AViewInfo.OnDrawButton := nil;
  AViewInfo.OnDrawButtonBackground := nil;
  AViewInfo.OnDrawButtonBorder := nil;
  AViewInfo.OnGetButtonState := nil;
  AViewInfo.OnPaint := nil;
  AViewInfo.OnPrepareButtonFadingImage := nil;
end;

procedure TcxBarEditItemControl.DrawEditBackground(ACanvas: TcxCanvas;
  ARect: TRect; AColor: TColor);
var
  APrevWindowOrg: TPoint;
begin
  BeforeDrawBackground(Edit, Parent, ACanvas.Handle, ARect, APrevWindowOrg);
  try
    Painter.DrawItemBackground(Self, ACanvas, ARect, GetSolidBrush(AColor));
  finally
    AfterDrawBackground(ACanvas.Handle, APrevWindowOrg);
  end;
end;

function TcxBarEditItemControl.GetBoundsRect: TRect;
begin
  Result := Painter.EditControlGetContentRect(GetPaintType, GetEditRect);
end;

function TcxBarEditItemControl.GetCurEditValue: TcxEditValue;
begin
  if Edit <> nil then
    Result := Edit.EditingValue
  else
    Result := Item.EditValue;
end;

function TcxBarEditItemControl.GetDefaultEditButtonWidth(AIndex: Integer): Integer;
begin
  if Item.UseBarPaintingStyle then
  begin
    Result := Parent.ComboBoxArrowWidth;
    Painter.EditButtonCorrectDefaultWidth(Result);
  end
  else
    Result := 0;
end;

function TcxBarEditItemControl.GetDropDownEdit: TcxCustomDropDownEdit;
begin
  Result := TcxCustomDropDownEdit(Edit);
end;

function TcxBarEditItemControl.GetPopupWindow: TcxCustomEditPopupWindow;
begin
  Result := TcxCustomDropDownEditAccess(DropDownEdit).PopupWindow;
end;

function TcxBarEditItemControl.GetEditSize(AIsMinSize: Boolean): TSize;
var
  ABorderOffsets: TRect;
  AConstantPartSize: TSize;
  AEditViewData: TcxCustomEditViewData;
  AEditViewInfo: TcxCustomEditViewInfo;
begin
  AEditViewInfo := CreateEditViewInfo;
  try
    AEditViewData := CreateEditViewData(not AIsMinSize or Item.CaptionIsEditValue);
    try
      CalculateEditViewInfo(cxRect(0, 0, cxMaxRectSize, cxMaxRectSize), cxInvalidPoint, False, AEditViewData, AEditViewInfo);
      AConstantPartSize := AEditViewData.GetEditConstantPartSize(Canvas, cxDefaultEditSizeProperties, Result, AEditViewInfo);

      if Item.CaptionIsEditValue then
        Result := cxSizeMax(Result, AEditViewData.GetEditContentSize(Canvas, Caption, cxDefaultEditSizeProperties));
      if Item.IsEditHasContent then
        Result.cx := Max(Result.cx, MinContentWidth);

      ABorderOffsets := Painter.EditControlBorderOffsets(GetPaintType);
      Inc(Result.cx, AConstantPartSize.cx + cxMarginsWidth(ABorderOffsets));
      Inc(Result.cy, AConstantPartSize.cy + cxMarginsHeight(ABorderOffsets));
    finally
      AEditViewData.Free;
    end;
  finally
    AEditViewInfo.Free;
  end;
end;

function TcxBarEditItemControl.GetEditStyle(AFull: Boolean = True): TcxEditStyle;
var
  ABackgroundColor: TColor;
  APainter: TdxBarPainter;
  ATextColor: TColor;
begin
  APainter := Painter;
  ATextColor := clDefault;
  ABackgroundColor := clDefault;

  if AFull then
  begin
    CalcDrawParams(AFull);

    if IsEditTransparent then
      ABackgroundColor := cxGetBrushData(Parent.BkBrush).lbColor
    else
      if Focused then
      begin
        ABackgroundColor := APainter.EditControlGetBackgroundColor(DrawParams);
        ATextColor := APainter.EditControlGetTextColor(DrawParams);
      end;

    if ABackgroundColor = clDefault then
      ABackgroundColor := APainter.EditGetBkColor(DrawParams);
    if ATextColor = clDefault then
      ATextColor := APainter.EditGetTextColor(Self);
  end;

  Result := InternalGetEditStyle(Properties, BarManager, APainter, EditFont, ABackgroundColor, ATextColor, DrawSelected);
end;

function TcxBarEditItemControl.GetEditViewInfo: TcxCustomEditViewInfo;
begin
  if (FEditViewInfo <> nil) and (FEditViewInfo.ClassType <> Properties.GetViewInfoClass) then
    FreeAndNil(FEditViewInfo);
  if FEditViewInfo = nil then
    FEditViewInfo := CreateEditViewInfo;
  Result := FEditViewInfo;
end;

function TcxBarEditItemControl.GetItem: TcxCustomBarEditItem;
begin
  Result := TcxCustomBarEditItem(ItemLink.Item);
end;

function TcxBarEditItemControl.GetProperties: TcxCustomEditProperties;
begin
  Result := cxGetScaledEditProperties(FInternalProperties, Item.GetProperties, ScaleFactor);
end;

procedure TcxBarEditItemControl.InitEditContentParams(var AParams: TcxEditContentParams);

  procedure InitializeExternalBorderBounds(var AExternalBorderBounds: TRect);
  var
    ABorderOffsets: TRect;
  begin
    AExternalBorderBounds := GetEditRect;
    OffsetRect(AExternalBorderBounds, -AExternalBorderBounds.Left, -AExternalBorderBounds.Top);
    if IsPopupSideward then
      if UseRightToLeftAlignment then
        Inc(AExternalBorderBounds.Right, GetEditOffset)
      else
        Dec(AExternalBorderBounds.Left, GetEditOffset);
    ABorderOffsets := Painter.EditControlBorderOffsets(GetPaintType);
    OffsetRect(AExternalBorderBounds, -ABorderOffsets.Left, -ABorderOffsets.Top);
  end;

begin
  if NeedEditShowCaption then
  begin
    AParams.Offsets := Painter.GetCaptionOffsets;
    AParams.SizeCorrection.cy := 0;
  end
  else
  begin
    InitializeExternalBorderBounds(AParams.ExternalBorderBounds);
    Painter.GetEditTextParams(AParams.Offsets, AParams.SizeCorrection.cy);
  end;
  AParams.SizeCorrection.cx := 0;

  AParams.Options := [];
  if Painter.EditButtonAllowOffsetContent then
    Include(AParams.Options, ecoOffsetButtonContent);
//  if Focused then
//    AParams.Options := [ecoShowFocusRectWhenInplace];
end;

function TcxBarEditItemControl.IsDropDownEdit: Boolean;
begin
  Result := Edit is TcxCustomDropDownEdit;
end;

function TcxBarEditItemControl.IsPopupSideward: Boolean;
begin
  Result := (Parent.Kind = bkSubMenu) or Parent.IsRealVertical
end;

function TcxBarEditItemControl.NeedEditShowCaption: Boolean;
begin
  Result := Item.CaptionIsEditValue or inherited GetShowCaption and not GetShowCaption;
end;

procedure TcxBarEditItemControl.PrepareEditForClose;
begin
  if Edit.Parent <> nil then
    Edit.Deactivate;
  Item.UpdateEditProperties(Edit);
end;

procedure TcxBarEditItemControl.LockChangeEvents(ALock: Boolean;
  AInvokeChanged: Boolean = True);
begin
  if ALock then
  begin
    Edit.LockChangeEvents(True);
    Edit.InternalProperties.BeginUpdate;
  end
  else
  begin
    Edit.InternalProperties.EndUpdate(AInvokeChanged);
    Edit.LockChangeEvents(False, False);
  end;
end;

initialization
  dxBarRegisterItem(TcxBarEditItem, TcxBarEditItemControl, True);
  BarDesignController.RegisterBarControlEditor(TcxItemsEditorEx);

finalization
  FreeAndNil(FFakeWinControl);

  BarDesignController.UnregisterBarControlEditor(TcxItemsEditorEx);
  dxBarUnregisterItem(TcxBarEditItem);

  FreeAndNil(FDefaultRepositoryItem);
  FreeAndNil(FEditList);
  FreeAndNil(FEditorParentForm);
  FreeAndNil(FEditStyle);

end.
