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

unit cxMRUEdit;

{$I cxVer.inc}

interface

uses
  Messages, Types, SysUtils, Classes, Controls, Graphics, cxContainer, cxEdit,
  cxDropDownEdit, cxFilterControlUtils;

type
  TcxCustomMRUEditProperties = class;

  TcxMRUEditDeleteLookupItemEvent = procedure(AProperties: TcxCustomMRUEditProperties; AItemIndex: Integer) of object;

  { TcxCustomMRUEditProperties }

  TcxCustomMRUEditProperties = class(TcxCustomComboBoxProperties)
  strict private
    FMaxItemCount: Integer;
    FShowEllipsis: Boolean;

    FOnButtonClick: TNotifyEvent;
    FOnDeleteLookupItem: TcxMRUEditDeleteLookupItemEvent;

    procedure CheckItemCount;
    procedure SetMaxItemCount(Value: Integer);
    procedure SetShowEllipsis(Value: Boolean);
  protected
    procedure DoAssign(AProperties: TcxCustomEditProperties); override;

    procedure DoDeleteLookupItem(AItemIndex: Integer);
    procedure DoOnNewLookupDisplayText(const AText: string); virtual;
    procedure InternalAddItem(const AItem: string; ARaiseOnNewLookupDisplayTextEvent: Boolean); virtual;
  public
    constructor Create(AOwner: TPersistent); override;
    class function GetContainerClass: TcxContainerClass; override;
    procedure DoUpdate(AProperties: TcxCustomEditProperties); override;
    procedure AddItem(const AItem: string);
    // !!!
    property MaxItemCount: Integer read FMaxItemCount write SetMaxItemCount default 0;
    property ShowEllipsis: Boolean read FShowEllipsis write SetShowEllipsis default True;
    property OnButtonClick: TNotifyEvent read FOnButtonClick write FOnButtonClick;
    property OnDeleteLookupItem: TcxMRUEditDeleteLookupItemEvent read FOnDeleteLookupItem write FOnDeleteLookupItem;
  end;

  { TcxMRUEditProperties }

  TcxMRUEditProperties = class(TcxCustomMRUEditProperties)
  published
    property Alignment;
    property AssignedValues;
    property AutoSelect;
    property BeepOnError;
    property ButtonGlyph;
    property CaseInsensitive;
    property CharCase;
    property ClearKey;
    property ClickKey;
    property DropDownAutoWidth;
    property DropDownListStyle;
    property DropDownRows;
    property DropDownSizeable;
    property DropDownWidth;
    property HideSelection;
    property IgnoreMaskBlank;
    property ImeMode;
    property ImeName;
    property ImmediateDropDownWhenActivated;
    property ImmediateDropDownWhenKeyPressed;
    property ImmediatePost;
    property ImmediateUpdateText;
    property ItemHeight;
    property LookupItems;
    property MaskKind;
    property EditMask;
    property MaxItemCount;
    property MaxLength;
    property Nullstring;
    property OEMConvert;
    property PopupAlignment;
    property PostPopupValueOnTab;
    property ReadOnly;
    property Revertable;
    property ShowEllipsis;
    property UseLeftAlignmentOnEditing;
    property UseNullString;
    property ValidateOnEnter;
    property ValidationErrorIconAlignment;
    property ValidationOptions;
    property OnButtonClick;
    property OnChange;
    property OnCloseUp;
    property OnDeleteLookupItem;
    property OnDrawItem;
    property OnEditValueChanged;
    property OnInitPopup;
    property OnMeasureItem;
    property OnNewLookupDisplayText;
    property OnPopup;
    property OnValidate;
  end;

  { TcxMRUEditProperties }

  TcxCustomMRUEdit = class(TcxCustomComboBox)
  private
    function GetProperties: TcxCustomMRUEditProperties;
    function GetActiveProperties: TcxCustomMRUEditProperties;
    procedure SetProperties(Value: TcxCustomMRUEditProperties);
  protected
    procedure DoButtonClick(AButtonVisibleIndex: Integer); override;
    procedure DoEditKeyDown(var Key: Word; Shift: TShiftState); override;
    procedure HandleSelectItem(Sender: TObject); override;
    procedure InternalValidateDisplayValue(const ADisplayValue: TcxEditValue); override;
    function SupportsSpelling: Boolean; override;
  public
    procedure AddItem(const Value: string); virtual;
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    property ActiveProperties: TcxCustomMRUEditProperties read GetActiveProperties;
    property Properties: TcxCustomMRUEditProperties read GetProperties
      write SetProperties;
  end;

  { TcxMRUEdit }

  TcxMRUEdit = class(TcxCustomMRUEdit)
  private
    function GetActiveProperties: TcxMRUEditProperties;
    function GetProperties: TcxMRUEditProperties;
    procedure SetProperties(Value: TcxMRUEditProperties);
  public
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    property ActiveProperties: TcxMRUEditProperties read GetActiveProperties;
    property ItemIndex;
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
    property Properties: TcxMRUEditProperties read GetProperties write SetProperties;
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

implementation

uses
  Windows, cxControls, cxEditConsts, cxTextEdit;

{ TcxCustomMRUEditProperties }

constructor TcxCustomMRUEditProperties.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FMaxItemCount := 0;
  FShowEllipsis := True;
  Buttons.Add;
  GlyphButtonIndex := 1;
  Buttons[1].Kind := bkEllipsis;
  Buttons[1].Default := True;
  DropDownListStyle := lsEditList;
  MRUMode := True;
end;

procedure TcxCustomMRUEditProperties.CheckItemCount;
begin
  if FMaxItemCount > 0 then
    while Items.Count > FMaxItemCount do
    begin
      DoDeleteLookupItem(Items.Count - 1);
      Items.Delete(Items.Count - 1);
    end;
end;

procedure TcxCustomMRUEditProperties.SetMaxItemCount(Value: Integer);
begin
  if Value < 0 then Value := 0;
  if FMaxItemCount <> Value then
  begin
    FMaxItemCount := Value;
    CheckItemCount;
    Changed;
  end;
end;

procedure TcxCustomMRUEditProperties.SetShowEllipsis(Value: Boolean);
begin
  if FShowEllipsis <> Value then
  begin
    BeginUpdate;
    try
      FShowEllipsis := Value;
      Buttons[1].Visible := Value;
      GlyphButtonIndex := Ord(Value);
    finally
      EndUpdate;
    end;
  end;
end;

class function TcxCustomMRUEditProperties.GetContainerClass: TcxContainerClass;
begin
  Result := TcxMRUEdit;
end;

procedure TcxCustomMRUEditProperties.DoUpdate(AProperties: TcxCustomEditProperties);
begin
  (AProperties as TcxCustomMRUEditProperties).LookupItems.Assign(LookupItems);
end;

procedure TcxCustomMRUEditProperties.AddItem(const AItem: string);
begin
  InternalAddItem(AItem, True);
end;

procedure TcxCustomMRUEditProperties.DoAssign(AProperties: TcxCustomEditProperties);
begin
  inherited;
  if AProperties is TcxCustomMRUEditProperties then
    with TcxCustomMRUEditProperties(AProperties) do
    begin
      Self.MaxItemCount := MaxItemCount;
      Self.ShowEllipsis := ShowEllipsis;
      Self.OnButtonClick := OnButtonClick;
      Self.OnDeleteLookupItem := OnDeleteLookupItem;
    end;
end;

procedure TcxCustomMRUEditProperties.DoDeleteLookupItem(AItemIndex: Integer);
begin
  if Assigned(FOnDeleteLookupItem) then
    FOnDeleteLookupItem(Self, AItemIndex);
end;

procedure TcxCustomMRUEditProperties.DoOnNewLookupDisplayText(const AText: string);
begin
  if Assigned(OnNewLookupDisplayText) then
    OnNewLookupDisplayText(Self, AText);
end;

procedure TcxCustomMRUEditProperties.InternalAddItem(const AItem: string; ARaiseOnNewLookupDisplayTextEvent: Boolean);
var
  I: Integer;
begin
  if AItem = '' then
    Exit;
  I := LookupItems.IndexOf(AItem);
  if I <> 0 then
    if I = -1 then
    begin
      if ARaiseOnNewLookupDisplayTextEvent then
        DoOnNewLookupDisplayText(AItem);
      LookupItems.Insert(0, AItem);
      CheckItemCount;
    end
    else
      LookupItems.Move(I, 0);
end;

{ TcxCustomMRUEdit }

function TcxCustomMRUEdit.GetProperties: TcxCustomMRUEditProperties;
begin
  Result := TcxCustomMRUEditProperties(inherited Properties);
end;

function TcxCustomMRUEdit.GetActiveProperties: TcxCustomMRUEditProperties;
begin
  Result := TcxCustomMRUEditProperties(InternalGetActiveProperties);
end;

procedure TcxCustomMRUEdit.SetProperties(Value: TcxCustomMRUEditProperties);
begin
  Properties.Assign(Value);
end;

procedure TcxCustomMRUEdit.DoButtonClick(AButtonVisibleIndex: Integer);
begin
  inherited DoButtonClick(AButtonVisibleIndex);
  if AButtonVisibleIndex = 1 then
  begin
    if Assigned(Properties.OnButtonClick) then
      Properties.OnButtonClick(Self);
    if (RepositoryItem <> nil) and Assigned(ActiveProperties.OnButtonClick) then
      ActiveProperties.OnButtonClick(Self);
  end;
end;

procedure TcxCustomMRUEdit.DoEditKeyDown(var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_RETURN) and not(ssAlt in Shift) and (not HasPopupWindow or (ILookupData.SelectedItem = -1)) then
    AddItem(Text);
  inherited DoEditKeyDown(Key, Shift);
end;

procedure TcxCustomMRUEdit.HandleSelectItem(Sender: TObject);
begin
  inherited HandleSelectItem(Sender);
  if not LookupItemsScrolling and DoEditing then
    AddItem(Text);
end;

procedure TcxCustomMRUEdit.InternalValidateDisplayValue(const ADisplayValue: TcxEditValue);
begin
  AddItem(Text);
  inherited InternalValidateDisplayValue(ADisplayValue);
end;

function TcxCustomMRUEdit.SupportsSpelling: Boolean;
begin
  Result := IsTextInputMode;
end;

procedure TcxCustomMRUEdit.AddItem(const Value: string);
begin
  if Value <> '' then
  begin
    if ActiveProperties.LookupItems.IndexOf(Value) = -1 then
      DoOnNewLookupDisplayText(Value);
    ActiveProperties.InternalAddItem(Value, False);
  end;
end;

class function TcxCustomMRUEdit.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxCustomMRUEditProperties;
end;

{ TcxMRUEdit }

class function TcxMRUEdit.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxMRUEditProperties;
end;

function TcxMRUEdit.GetActiveProperties: TcxMRUEditProperties;
begin
  Result := TcxMRUEditProperties(InternalGetActiveProperties);
end;

function TcxMRUEdit.GetProperties: TcxMRUEditProperties;
begin
  Result := TcxMRUEditProperties(inherited Properties);
end;

procedure TcxMRUEdit.SetProperties(Value: TcxMRUEditProperties);
begin
  Properties.Assign(Value);
end;

initialization
  GetRegisteredEditProperties.Register(TcxMRUEditProperties, scxSEditRepositoryMRUItem);
  FilterEditsController.Register(TcxMRUEditProperties, TcxFilterTextEditHelper);

finalization
  FilterEditsController.Unregister(TcxMRUEditProperties, TcxFilterTextEditHelper);

end.
