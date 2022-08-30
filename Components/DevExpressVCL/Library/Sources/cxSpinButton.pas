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

unit cxSpinButton;

{$I cxVer.inc}

interface

uses
  Variants, Windows, Classes, Controls, Forms, Graphics, Messages, StdCtrls, SysUtils,
  dxCore, cxClasses, cxContainer, cxControls, cxEdit, cxExtEditConsts,
  cxExtEditUtils, cxGraphics, cxMaskEdit, cxSpinEdit, cxTextEdit, cxVariants;

type
  { TcxSpinButtonViewData }

  TcxCustomSpinButtonProperties = class;

  TcxSpinButtonViewData = class(TcxSpinEditViewData)
  private
    function GetProperties: TcxCustomSpinButtonProperties;
  protected
    function CanPressButton(AViewInfo: TcxCustomEditViewInfo; AButtonVisibleIndex: Integer):
      Boolean; override;
    function IsButtonPressed(AViewInfo: TcxCustomEditViewInfo; AButtonVisibleIndex:
      Integer): Boolean; override;
  public
    PressedState: TcxSpinEditPressedState;
    procedure Calculate(ACanvas: TcxCanvas; const ABounds: TRect; const P: TPoint;
      Button: TcxMouseButton; Shift: TShiftState; AViewInfo: TcxCustomEditViewInfo;
      AIsMouseEvent: Boolean); override;
    property Properties: TcxCustomSpinButtonProperties read GetProperties;
  end;

  { TcxCustomSpinButtonProperties }

  TcxCustomSpinButton = class;

  TcxCustomSpinButtonProperties = class(TcxCustomSpinEditProperties)
  protected
    class function GetViewDataClass: TcxCustomEditViewDataClass; override;
    function IsDisplayValueNumeric: Boolean; override;
    function IsEditValueNumeric: Boolean; override;
    function PreserveSelection: Boolean; override;
  public
    constructor Create(AOwner: TPersistent); override;
    class function GetContainerClass: TcxContainerClass; override;
  end;

  { TcxSpinButtonProperties }

  TcxSpinButtonProperties = class(TcxCustomSpinButtonProperties)
  published
    property Alignment;
    property AssignedValues;
    property AutoSelect;
    property BeepOnError;
    property HideSelection;
    property ImmediatePost;
    property Increment;
    property LargeIncrement;
    property MaxValue;
    property MinValue;
    property ReadOnly;
    property SpinButtons;
    property UseCtrlIncrement;
    property UseLeftAlignmentOnEditing;
    property ValueType default vtInt;
    property OnChange;
    property OnEditValueChanged;
  end;

  { TcxCustomSpinButton }

  TcxCustomSpinButton = class(TcxCustomSpinEdit)
  private
    FAssociate: TWinControl;
    FSubclassControl: TWinControl;
    FAutoWidth: Boolean;
    FPrevAssociateControlWndProcObject: TcxWindowProcLinkedObject;
    function GetActiveProperties: TcxCustomSpinButtonProperties;
    function GetProperties: TcxCustomSpinButtonProperties;
    procedure SubclassAssociateControl;
    procedure SetAssociate(AValue: TWinControl);
    procedure SetAutoWidth(Value: Boolean);
    procedure SetProperties(Value: TcxCustomSpinButtonProperties);
    procedure SetSubclassControl(const Value: TWinControl);
    procedure UnsubclassAssociateControl;

    function IsAssociatedWithEdit: Boolean;
  protected
    procedure InternalSetEditValue(const Value: TcxEditValue;
      AValidateEditValue: Boolean); override;
    procedure InternalSetDisplayValue(const Value: TcxEditValue); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function CanFocusOnClick: Boolean; override;
    procedure DoSetSize; override;
    procedure Initialize; override;
    function InternalDoEditing: Boolean; override;

    procedure AssociateControlWndProc(var AMessage: TMessage); virtual;
    function GetValue: Variant; override;
    procedure JoinToAssociate; virtual;
    procedure SetAssociateValue(const AValue: Variant); virtual;
    procedure SynchronizeEditValueFromAssociate; virtual;

    property Associate: TWinControl read FAssociate write SetAssociate;
    property AutoWidth: Boolean read FAutoWidth write SetAutoWidth default True;
    property SubclassControl: TWinControl read FSubclassControl write SetSubclassControl;
    property TabStop default False;
  public
    destructor Destroy; override;
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    function Increment(AButton: TcxSpinEditButton): Boolean; override;
    property ActiveProperties: TcxCustomSpinButtonProperties read GetActiveProperties;
    property Properties: TcxCustomSpinButtonProperties read GetProperties
      write SetProperties;
  end;

  { TcxSpinButton }

  TcxSpinButton = class(TcxCustomSpinButton)
  private
    function GetActiveProperties: TcxSpinButtonProperties;
    function GetProperties: TcxSpinButtonProperties;
    procedure SetProperties(Value: TcxSpinButtonProperties);
  public
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    property ActiveProperties: TcxSpinButtonProperties read GetActiveProperties;
  published
    property Anchors;
    property Associate;
    property AutoSize;
    property AutoWidth;
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
    property Properties: TcxSpinButtonProperties read GetProperties
      write SetProperties;
    property ShowHint;
    property Style;
    property StyleDisabled;
    property StyleFocused;
    property StyleHot;
    property TabOrder;
    property TabStop;
    property Value;
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

implementation

uses
  cxEditConsts, cxEditUtils, dxThemeConsts, dxThemeManager;

type
  TWinControlAccess = class(TWinControl);

{ TcxSpinButtonViewData }

procedure TcxSpinButtonViewData.Calculate(ACanvas: TcxCanvas; const ABounds: TRect;
  const P: TPoint; Button: TcxMouseButton; Shift: TShiftState; AViewInfo: TcxCustomEditViewInfo;
  AIsMouseEvent: Boolean);
begin
  inherited Calculate(ACanvas, ABounds, P, Button, Shift, AViewInfo, AIsMouseEvent);
  TcxSpinEditViewInfo(AViewInfo).Text := '';
end;

function TcxSpinButtonViewData.CanPressButton(AViewInfo: TcxCustomEditViewInfo;
  AButtonVisibleIndex: Integer): Boolean;
begin
  Result := inherited CanPressButton(AViewInfo, AButtonVisibleIndex);
end;

function TcxSpinButtonViewData.IsButtonPressed(AViewInfo: TcxCustomEditViewInfo;
  AButtonVisibleIndex: Integer): Boolean;
begin
  Result := inherited IsButtonPressed(AViewInfo, AButtonVisibleIndex);
end;

function TcxSpinButtonViewData.GetProperties: TcxCustomSpinButtonProperties;
begin
  Result := TcxCustomSpinButtonProperties(FProperties);
end;

{ TcxCustomSpinButtonProperties }

constructor TcxCustomSpinButtonProperties.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  HideCursor := True;
end;

class function TcxCustomSpinButtonProperties.GetContainerClass: TcxContainerClass;
begin
  Result := TcxSpinButton;
end;

class function TcxCustomSpinButtonProperties.GetViewDataClass: TcxCustomEditViewDataClass;
begin
  Result := TcxSpinButtonViewData;
end;

function TcxCustomSpinButtonProperties.IsDisplayValueNumeric: Boolean;
begin
  Result := True;
end;

function TcxCustomSpinButtonProperties.IsEditValueNumeric: Boolean;
begin
  Result := True;
end;

function TcxCustomSpinButtonProperties.PreserveSelection: Boolean;
begin
  Result := False;
end;

{ TcxCustomSpinButton }

destructor TcxCustomSpinButton.Destroy;
begin
  SubclassControl := nil;
  inherited Destroy;
end;

class function TcxCustomSpinButton.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxCustomSpinButtonProperties;
end;

function TcxCustomSpinButton.GetValue: Variant;
const
  AVarTypeMap: array [TcxSpinEditValueType] of TVarType = (varInteger, varDouble);
begin
  PrepareEditValue(Text, Result, InternalFocused);
  if not VarIsNumeric(Result) then
    Result := VarAsType(ActiveProperties.MinValue, AVarTypeMap[ActiveProperties.ValueType]);
end;

procedure TcxCustomSpinButton.InternalSetDisplayValue(const Value: TcxEditValue);
begin
  if IsDestroying then
    Exit;
  inherited InternalSetDisplayValue(Value);
  SetAssociateValue(Value);
end;

procedure TcxCustomSpinButton.AssociateControlWndProc(var AMessage: TMessage);
var
  AKey: Longint;
begin
  if Assigned(FSubclassControl) then
  begin
    if (AMessage.Msg = WM_KEYDOWN) or (AMessage.Msg = WM_KEYUP) then
    begin
      AKey := AMessage.WParam;
      if AKey in [VK_UP, VK_DOWN, VK_NEXT, VK_PRIOR] then
        PostMessage(Handle, AMessage.Msg, AKey, AMessage.LParam);
    end;
    FPrevAssociateControlWndProcObject.DefaultProc(AMessage);
    if AMessage.Msg = WM_DESTROY then
      UnsubclassAssociateControl;
  end;
end;

procedure TcxCustomSpinButton.InternalSetEditValue(const Value: TcxEditValue; AValidateEditValue: Boolean);
begin
  if IsDestroying then
    Exit;
  inherited InternalSetEditValue(Value, AValidateEditValue);
  SetAssociateValue(Value);
end;

procedure TcxCustomSpinButton.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FAssociate) then
    Associate := nil;
end;

function TcxCustomSpinButton.CanFocusOnClick: Boolean;
begin
  Result := False;
end;

function TcxCustomSpinButton.Increment(AButton: TcxSpinEditButton): Boolean;
begin
  if Assigned(Associate) then
    SynchronizeEditValueFromAssociate;
  Result := inherited Increment(AButton);
  if not Result then
    Exit;
  BeginUserAction;
  try
    SynchronizeEditValue;
    SetAssociateValue(EditValue);
  finally
    EndUserAction;
  end;
end;

procedure TcxCustomSpinButton.DoSetSize;
var
  AWidth: Integer;
begin
  inherited;
  if not AutoWidth then
    Exit;
  with ViewInfo do
  begin
    AWidth := (BorderRect.Right - BorderRect.Left) - (TextRect.Right - TextRect.Left);
    if UseSkins then
      Dec(AWidth)
    else
      if Length(ButtonsInfo) > 0 then
        if not ButtonsInfo[0].Data.NativeStyle then
        begin
          AWidth := Succ(AWidth);
          if Shadow then
            AWidth := AWidth + cxContainerShadowWidth;
        end;
  end;
  Width := AWidth;
end;

procedure TcxCustomSpinButton.Initialize;
begin
  inherited Initialize;
  FAutoWidth := True;
  AutoSize := False;
  TabStop := False;
end;

function TcxCustomSpinButton.InternalDoEditing: Boolean;
begin
  Result := True;
end;

function TcxCustomSpinButton.GetActiveProperties: TcxCustomSpinButtonProperties;
begin
  Result := TcxCustomSpinButtonProperties(InternalGetActiveProperties);
end;

function TcxCustomSpinButton.GetProperties: TcxCustomSpinButtonProperties;
begin
  Result := TcxCustomSpinButtonProperties(inherited Properties);
end;

procedure TcxCustomSpinButton.SubclassAssociateControl;
begin
  if Assigned(FSubclassControl) and
    (FSubclassControl.ComponentState * [csDesigning, csDestroying] = []) then
     FPrevAssociateControlWndProcObject := cxWindowProcController.Add(FSubclassControl, AssociateControlWndProc);
end;

procedure TcxCustomSpinButton.SetAssociate(AValue: TWinControl);
var
  I: Integer;
  ACompoundControl: IcxCompoundControl;

  function IsClass(ClassType: TClass; const Name: string): Boolean;
  begin
    Result := True;
    while ClassType <> nil do
    begin
      if ClassType.ClassNameIs(Name) then Exit;
      ClassType := ClassType.ClassParent;
    end;
    Result := False;
  end;

begin
  if AValue <> nil then
    for I := 0 to Parent.ControlCount - 1 do {Is control already associated ?}
      if (Parent.Controls[I] is TcxCustomSpinButton) and (Parent.Controls[I] <> Self) then
        if TcxCustomSpinButton(Parent.Controls[I]).Associate = AValue then
          raise EcxEditError.Create(AValue.Name +
            cxGetResourceString(@scxUDAssociated) + Parent.Controls[I].Name);

  if FAssociate <> nil then
  begin
    SubclassControl := nil;
    FAssociate := nil;
  end;

  if (AValue <> nil) and (AValue.Parent = Self.Parent) and
    not (AValue is TcxCustomSpinButton) and
    not IsClass(AValue.ClassType, 'TcxDBTextEdit') and
    not IsClass(AValue.ClassType, 'TcxDBMemo') and
    not IsClass(AValue.ClassType, 'TDBTextEdit') and
    not IsClass(AValue.ClassType, 'TDBMemo') then
  begin
    FAssociate := AValue;
    if Supports(FAssociate, IcxCompoundControl, ACompoundControl) then
      SubclassControl := ACompoundControl.ActiveControl
    else
      SubclassControl := FAssociate;
    JoinToAssociate;
    SetAssociateValue(Value);
  end;
end;

procedure TcxCustomSpinButton.SetAutoWidth(Value: Boolean);
begin
  if FAutoWidth <> Value then
  begin
    FAutoWidth := Value;
    if Value then
    begin
      CheckHandle;
      ActiveProperties.ViewStyle := vsNormal;
    end
    else
      ActiveProperties.ViewStyle := vsButtonsOnly;
    if Value then
    begin
      SetSize;
      ShortRefreshContainer(False);
    end;
  end;
end;

procedure TcxCustomSpinButton.SetProperties(Value: TcxCustomSpinButtonProperties);
begin
  Properties.Assign(Value);
end;

procedure TcxCustomSpinButton.SetSubclassControl(const Value: TWinControl);
begin
  if Value <> FSubclassControl then
  begin
    if FSubclassControl <> nil then
      UnsubclassAssociateControl;
    FSubclassControl := Value;
    if FSubclassControl <> nil then
      SubclassAssociateControl;
  end;
end;

procedure TcxCustomSpinButton.UnsubclassAssociateControl;
begin
  if Assigned(FSubclassControl) then
    cxWindowProcController.Remove(FPrevAssociateControlWndProcObject);
end;

function TcxCustomSpinButton.IsAssociatedWithEdit: Boolean;
begin
  Result := (FAssociate is TCustomEdit) or (FAssociate is TcxCustomTextEdit);
end;

procedure TcxCustomSpinButton.JoinToAssociate;
begin
  if FAssociate is TcxCustomEdit then
    Font := TcxCustomEdit(FAssociate).Style.Font
  else
    Font := TWinControlAccess(FAssociate).Font;
  Top := FAssociate.Top;
  if UseRightToLeftAlignment then
    Left := FAssociate.Left - Width
  else
    Left := FAssociate.Left + FAssociate.Width;
  Height := FAssociate.Height;
end;

procedure TcxCustomSpinButton.SetAssociateValue(const AValue: Variant);
begin
  if not IsAssociatedWithEdit then
    Exit;
  if FAssociate is TcxCustomTextEdit then
    TcxCustomTextEdit(FAssociate).EditValue := AValue
  else
    TCustomEdit(FAssociate).Text := VarToStr(AValue);
end;

procedure TcxCustomSpinButton.SynchronizeEditValueFromAssociate;
var
  AAssociateValue: TcxEditValue;
begin
  if not IsAssociatedWithEdit then
    Exit;

  if FAssociate is TcxCustomTextEdit then
    if TcxCustomTextEdit(FAssociate).Focused then
      AAssociateValue := TcxCustomTextEdit(FAssociate).EditingValue
    else
      AAssociateValue := TcxCustomTextEdit(FAssociate).EditValue
  else
    AAssociateValue := TCustomEdit(FAssociate).Text;

  if ActiveProperties.ValueType = vtInt then
    AAssociateValue := cxStrToInt(AAssociateValue, True)
  else
    AAssociateValue := cxStrToFloat(AAssociateValue, True);

  if not VarEquals(AAssociateValue, Value) then
    Value := AAssociateValue;
end;

{ TcxSpinButton }

class function TcxSpinButton.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxSpinButtonProperties;
end;

function TcxSpinButton.GetActiveProperties: TcxSpinButtonProperties;
begin
  Result := TcxSpinButtonProperties(InternalGetActiveProperties);
end;

function TcxSpinButton.GetProperties: TcxSpinButtonProperties;
begin
  Result := TcxSpinButtonProperties(inherited Properties);
end;

procedure TcxSpinButton.SetProperties(Value: TcxSpinButtonProperties);
begin
  Properties.Assign(Value);
end;

end.
