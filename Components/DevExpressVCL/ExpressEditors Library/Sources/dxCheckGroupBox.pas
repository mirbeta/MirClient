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

unit dxCheckGroupBox;

interface

uses
  Windows, Messages, Types, SysUtils, Classes, Controls, Graphics, Forms,
  cxGroupBox, cxEdit, cxCheckBox, cxLookAndFeelPainters;

type
  TdxCheckGroupBoxCheckBoxAction = (cbaNone, cbaToggleChildrenEnabledState);

  { TdxCheckGroupBoxProperties }

  TdxCheckGroupBoxProperties = class(TcxCustomGroupBoxProperties)
  protected
    function CanValidate: Boolean; override;
  public
    function IsResetEditClass: Boolean; override;
  published
    property ImmediatePost;
    property ValidationErrorIconAlignment;
    property ValidationOptions;
    property OnChange;
    property OnEditValueChanged;
    property OnValidate;
  end;

  { TdxCheckGroupBoxCheckBox }

  TdxCustomCheckGroupBox = class;

  TdxCustomCheckGroupBoxCheckBoxClass = class of TdxCustomCheckGroupBoxCheckBox;

  TdxCustomCheckGroupBoxCheckBox = class(TPersistent)
  private
    FCheckAction: TdxCheckGroupBoxCheckBoxAction;
    FCheckGroupBox: TdxCustomCheckGroupBox;
    FVisible: Boolean;
    function GetChecked: Boolean;
    procedure SetChecked(const AValue: Boolean);
    procedure SetVisible(const AValue: Boolean);
  public
    constructor Create(AOwner: TdxCustomCheckGroupBox); overload; virtual;
    procedure Assign(Source: TPersistent); override;

    property CheckAction: TdxCheckGroupBoxCheckBoxAction read FCheckAction
      write FCheckAction default cbaToggleChildrenEnabledState;
    property Checked: Boolean read GetChecked write SetChecked default True;
    property Visible: Boolean read FVisible write SetVisible default True;
  end;

  { TcxCustomCheckGroupBox }

  TdxCustomCheckGroupBox = class(TcxCustomGroupBox)
  private
    FCheckBox: TdxCustomCheckGroupBoxCheckBox;
    FFocusable: Boolean;
    function GetActiveProperties: TdxCheckGroupBoxProperties;
    function GetProperties: TdxCheckGroupBoxProperties;
    procedure SetCheckBox(AValue: TdxCustomCheckGroupBoxCheckBox);
    procedure SetProperties(Value: TdxCheckGroupBoxProperties);
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMLButtonUp(var Message: TWMLButtonUp); message WM_LBUTTONUP;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
  protected
    function CanFocusOnClick(X, Y: Integer): Boolean; override;
    procedure DoCheckBoxCheckedChanged;
    procedure EnableChildControls(AControl: TControl; AEnabled: Boolean);
    class function GetCheckBoxClass: TdxCustomCheckGroupBoxCheckBoxClass; virtual;
    function GetCheckBoxVisible: Boolean; override;
    procedure Initialize; override;
    function InternalCanFocusOnClick: Boolean; override;
    procedure InternalSetEditValue(const Value: TcxEditValue; AValidateEditValue: Boolean); override;
    procedure Loaded; override;
    property CheckBox: TdxCustomCheckGroupBoxCheckBox read FCheckBox write SetCheckBox;
    property Focusable: Boolean read FFocusable write FFocusable default True;
    property TabStop default True;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CanFocus: Boolean; override;
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    procedure PrepareEditValue(const ADisplayValue: TcxEditValue;
      out EditValue: TcxEditValue; AEditFocused: Boolean); override;
    property ActiveProperties: TdxCheckGroupBoxProperties read GetActiveProperties;
    property Properties: TdxCheckGroupBoxProperties read GetProperties
      write SetProperties;
  end;

  { TdxCheckGroupBoxCheckBox }

  TdxCheckGroupBoxCheckBoxClass = class of TdxCheckGroupBoxCheckBox;

  TdxCheckGroupBoxCheckBox = class(TdxCustomCheckGroupBoxCheckBox)
  published
    property CheckAction;
    property Checked;
    property Visible;
  end;

  { TdxCheckGroupBox }

  TdxCheckGroupBox = class(TdxCustomCheckGroupBox)
  protected
    class function GetCheckBoxClass: TdxCustomCheckGroupBoxCheckBoxClass; override;
  published
    property Alignment;
    property Anchors;
    property BiDiMode;
    property Caption;
    property CheckBox;
    property Constraints;
    property Ctl3D;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Focusable;
    property PanelStyle;
    property ParentBackground;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Properties;
    property RedrawOnResize;
    property ShowHint;
    property Style;
    property StyleDisabled;
    property TabOrder;
    property TabStop;
    property Transparent;
    property UseDockManager;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnCustomDraw;
    property OnCustomDrawCaption;
    property OnCustomDrawContentBackground;
    property OnDblClick;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnMeasureCaptionHeight;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;


implementation

{ TdxCheckGroupBoxProperties }

function TdxCheckGroupBoxProperties.IsResetEditClass: Boolean;
begin
  Result := True;
end;

function TdxCheckGroupBoxProperties.CanValidate: Boolean;
begin
  Result := True;
end;

{ TdxCustomCheckGroupBoxCheckBox }

constructor TdxCustomCheckGroupBoxCheckBox.Create(AOwner: TdxCustomCheckGroupBox);
begin
  inherited Create;
  FCheckGroupBox := AOwner;
  FCheckAction := cbaToggleChildrenEnabledState;
  FVisible := True;
end;

procedure TdxCustomCheckGroupBoxCheckBox.Assign(Source: TPersistent);
begin
  if Source is TdxCustomCheckGroupBoxCheckBox then
  begin
    CheckAction := TdxCustomCheckGroupBoxCheckBox(Source).CheckAction;
    Checked := TdxCustomCheckGroupBoxCheckBox(Source).Checked;
    Visible := TdxCustomCheckGroupBoxCheckBox(Source).Visible;
  end
  else
    inherited Assign(Source);
end;

function TdxCustomCheckGroupBoxCheckBox.GetChecked: Boolean;
begin
  Result := FCheckGroupBox.CheckBoxCheckState = cbsChecked;
end;

procedure TdxCustomCheckGroupBoxCheckBox.SetChecked(const AValue: Boolean);
begin
  if AValue then
    FCheckGroupBox.CheckBoxCheckState := cbsChecked
  else
    FCheckGroupBox.CheckBoxCheckState := cbsUnchecked;
end;

procedure TdxCustomCheckGroupBoxCheckBox.SetVisible(const AValue: Boolean);
begin
  if FVisible <> AValue then
  begin
    FVisible := AValue;
    FCheckGroupBox.ShortRefreshContainer(False);
    FCheckGroupBox.ViewInfo.InvalidateCaption;
  end;
end;

{ TdxCustomCheckGroupBox }

constructor TdxCustomCheckGroupBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCheckBox := GetCheckBoxClass.Create(Self);
  FFocusable := True;
  TabStop := True;
  InternalSetEditValue(True, True);
end;

destructor TdxCustomCheckGroupBox.Destroy;
begin
  FreeAndNil(FCheckBox);
  inherited Destroy;
end;

function TdxCustomCheckGroupBox.CanFocus: Boolean;
begin
  Result := inherited CanFocus and FFocusable;
end;

class function TdxCustomCheckGroupBox.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TdxCheckGroupBoxProperties;
end;

procedure TdxCustomCheckGroupBox.PrepareEditValue(
  const ADisplayValue: TcxEditValue; out EditValue: TcxEditValue;
  AEditFocused: Boolean);
begin
  EditValue := TcxCheckBoxState(ADisplayValue) = cbsChecked;
end;

function TdxCustomCheckGroupBox.CanFocusOnClick(X, Y: Integer): Boolean;
begin
  Result := inherited CanFocusOnClick(X, Y) and PtInRect(ViewInfo.MouseFocusingRect, Point(X, Y));
end;

procedure TdxCustomCheckGroupBox.DoCheckBoxCheckedChanged;
begin
  if not IsDesigning and (CheckBox.CheckAction = cbaToggleChildrenEnabledState) then
    EnableChildControls(Self, CheckBox.Checked);
end;

procedure TdxCustomCheckGroupBox.EnableChildControls(AControl: TControl; AEnabled: Boolean);
var
  I: Integer;
  AChildControl: TControl;
begin
  if AControl is TWinControl then
    for I := 0 to TWinControl(AControl).ControlCount - 1 do
    begin
      AChildControl := TWinControl(AControl).Controls[I];
      AChildControl.Enabled := AEnabled;
      if not (AChildControl is TdxCustomCheckGroupBox) or not AEnabled or
        TdxCustomCheckGroupBox(AChildControl).CheckBox.Checked then
          EnableChildControls(AChildControl, AEnabled);
    end;
end;

class function TdxCustomCheckGroupBox.GetCheckBoxClass;
begin
  Result := TdxCustomCheckGroupBoxCheckBox;
end;

function TdxCustomCheckGroupBox.GetCheckBoxVisible: Boolean;
begin
  Result := CheckBox.Visible and not PanelStyle.Active;
end;

procedure TdxCustomCheckGroupBox.Initialize;
begin
  inherited Initialize;
  PrepareEditValue(cbsUnchecked, FEditValue, False);
end;

function TdxCustomCheckGroupBox.InternalCanFocusOnClick: Boolean;
begin
  Result := True;
end;

procedure TdxCustomCheckGroupBox.InternalSetEditValue(const Value: TcxEditValue;
  AValidateEditValue: Boolean);
var
  APrevState: TcxCheckBoxState;
begin
  APrevState := CheckBoxCheckState;
  inherited InternalSetEditValue(Value, AValidateEditValue);
  if APrevState <> CheckBoxCheckState then
  begin
    ViewInfo.CheckBoxCheckState := CheckBoxCheckState;
    Click;
    DoChange;
    DoCheckBoxCheckedChanged;
  end;
  ShortRefreshContainer(False);
end;

procedure TdxCustomCheckGroupBox.Loaded;
begin
  inherited Loaded;
  DoCheckBoxCheckedChanged;
end;

function TdxCustomCheckGroupBox.GetActiveProperties: TdxCheckGroupBoxProperties;
begin
  Result := TdxCheckGroupBoxProperties(InternalGetActiveProperties);
end;

function TdxCustomCheckGroupBox.GetProperties: TdxCheckGroupBoxProperties;
begin
  Result := TdxCheckGroupBoxProperties(inherited Properties);
end;

procedure TdxCustomCheckGroupBox.SetCheckBox(AValue: TdxCustomCheckGroupBoxCheckBox);
begin
  FCheckBox.Assign(AValue);
end;

procedure TdxCustomCheckGroupBox.SetProperties(Value: TdxCheckGroupBoxProperties);
begin
  Properties.Assign(Value);
end;

procedure TdxCustomCheckGroupBox.CMDialogChar(var Message: TCMDialogChar);
begin
  with Message do
    if IsAccel(CharCode, Caption) and CanFocus then
    begin
      SetFocus;
      if Focused then
        Toggle;
      Result := 1;
    end
    else
      inherited;
end;

procedure TdxCustomCheckGroupBox.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;
  RedrawNonClientArea;
end;

procedure TdxCustomCheckGroupBox.WMLButtonUp(var Message: TWMLButtonUp);
var
  APoint: TPoint;
begin
  if csClicked in ControlState then
  begin
    APoint := SmallPointToPoint(Message.Pos);
    if not PointInCheckBox(APoint) then
      Click;
    ControlState := ControlState - [csClicked];
  end;
  inherited;
end;

procedure TdxCustomCheckGroupBox.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;
  RedrawNonClientArea;
end;

{ TdxCheckGroupBox }

class function TdxCheckGroupBox.GetCheckBoxClass: TdxCustomCheckGroupBoxCheckBoxClass;
begin
  Result := TdxCheckGroupBoxCheckBox;
end;

end.
