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

unit dxColorEdit;

{$I cxVer.inc}

interface

uses
  SysUtils, Windows, Classes, Graphics, Types,
  dxCore, dxCoreClasses, cxGraphics, cxGeometry, cxContainer, cxControls,
  cxDataStorage, cxVariants,
  cxEdit, cxTextEdit, cxDropDownEdit, cxFilterControlUtils,
  dxGallery, dxColorGallery, dxGalleryControl;

type
  TdxCustomColorEdit = class;
  TdxCustomColorEditProperties = class;

  { TdxCustomColorEditViewInfo }

  TdxCustomColorEditViewInfo = class(TcxCustomTextEditViewInfo)
  protected
    FColor: TColor;
  public
    procedure Paint(ACanvas: TcxCanvas); override;
  end;

  { TdxCustomColorEditViewData }

  TdxCustomColorEditViewData = class(TcxCustomDropDownEditViewData)
  strict private
    function GetProperties: TdxCustomColorEditProperties;
  protected
    function IsComboBoxStyle: Boolean; override;
  public
    procedure DisplayValueToDrawValue(const ADisplayValue: TcxEditValue; AViewInfo: TcxCustomEditViewInfo); override;
    procedure EditValueToDrawValue(const AEditValue: TcxEditValue; AViewInfo: TcxCustomEditViewInfo); override;

    property Properties: TdxCustomColorEditProperties read GetProperties;
  end;

  { TdxCustomColorEditProperties }

  TdxCustomColorEditProperties = class(TcxCustomPopupEditProperties)
  strict private
    FColorSet: TdxColorSet;
    FColorPalette: TdxColorPalette;
    FDefaultColor: TColor;
    FItemShowHint: Boolean;
    FItemSize: Integer;
    FShowItemBorders: Boolean;
    FOnGetCustomColorSet: TdxColorGetCustomColorSetEvent;

    procedure SetDefaultColor(Value: TColor);
    procedure SetShowItemBorders(AValue: Boolean);
  protected
    procedure ChangeScale(M, D: Integer); override;
    function EditValueToColorValue(AEditValue: TcxEditValue): TColor;
    procedure DoAssign(AProperties: TcxCustomEditProperties); override;
    function GetAlwaysPostEditValue: Boolean; override;
    function DropDownOnClick: Boolean; override;
    function GetEditingStyle: TcxEditEditingStyle; override;
    class function GetLookupDataClass: TcxInterfacedPersistentClass; override;
    class function GetViewDataClass: TcxCustomEditViewDataClass; override;
  public
    constructor Create(AOwner: TPersistent); override;

    class function GetContainerClass: TcxContainerClass; override;
    class function GetViewInfoClass: TcxContainerViewInfoClass; override;

    property ColorPalette: TdxColorPalette read FColorPalette write FColorPalette;
    property ColorSet: TdxColorSet read FColorSet write FColorSet;
    property DefaultColor: TColor read FDefaultColor write SetDefaultColor;
    property ItemShowHint: Boolean read FItemShowHint write FItemShowHint;
    property ItemSize: Integer read FItemSize write FItemSize;
    property ShowItemBorders: Boolean read FShowItemBorders write SetShowItemBorders;

    property OnGetCustomColorSet: TdxColorGetCustomColorSetEvent read FOnGetCustomColorSet write FOnGetCustomColorSet;
  end;

  { TdxColorEditProperties }

  TdxColorEditProperties = class(TdxCustomColorEditProperties)
  published
    property AssignedValues;
    property ButtonGlyph;
    property ClearKey;
    property ValidationErrorIconAlignment;
    property ValidationOptions;

    property ImmediateDropDownWhenActivated;
    property ImmediateDropDownWhenKeyPressed;
    property ImmediatePost;

    property ReadOnly;

    property ColorPalette default cpOffice;
    property ColorSet default csDefault;
    property DefaultColor default clWindow;
    property ItemShowHint default False;
    property ItemSize default 0;
    property ShowItemBorders default True;

    property OnChange;
    property OnCloseUp;
    property OnEditValueChanged;
    property OnInitPopup;
    property OnPopup;
    property OnValidate;

    property OnGetCustomColorSet;
  end;

  { TdxPopupColorGallery }

  TdxPopupColorGallery = class(TdxCustomColorGallery)
  strict private
    FOnHidePopup: TcxEditClosePopupEvent;

    function GetEdit: TdxCustomColorEdit;
  protected
    procedure HidePopup(Sender: TcxControl; AReason: TcxEditCloseUpReason);
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;

    property Edit: TdxCustomColorEdit read GetEdit;
  public
    constructor Create(AOwner: TComponent); override;

    property OnHidePopup: TcxEditClosePopupEvent read FOnHidePopup write FOnHidePopup;
  end;

  { TdxCustomColorEdit }

  TdxCustomColorEdit = class(TcxCustomPopupEdit)
  strict private
    function IsColorValueStored: Boolean;

    function GetColorValue: TColor;
    procedure SetColorValue(AValue: TColor);

    function GetProperties: TdxCustomColorEditProperties;
    function GetActiveProperties: TdxCustomColorEditProperties;
    procedure SetProperties(Value: TdxCustomColorEditProperties);
  protected
    FColorGallery: TdxPopupColorGallery;

    procedure CreatePopupWindow; override;
    procedure InitializeInnerEdit; override;
    procedure InitializePopupWindow; override; // TcxCustomDropDownEdit

    procedure ItemClickHandler(Sender: TObject; AItem: TdxGalleryControlItem);

    procedure DoEditKeyPress(var Key: Char); override;
    procedure DropDown; override;
  public
    destructor Destroy; override;
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    procedure PrepareEditValue(const ADisplayValue: TcxEditValue; out EditValue: TcxEditValue; AEditFocused: Boolean); override;

    property ActiveProperties: TdxCustomColorEditProperties read GetActiveProperties;
    property Properties: TdxCustomColorEditProperties read GetProperties write SetProperties;
    property ColorValue: TColor read GetColorValue write SetColorValue stored IsColorValueStored;
  end;

  { TdxColorEdit }

  TdxColorEdit = class(TdxCustomColorEdit)
  strict private
    function GetProperties: TdxColorEditProperties;
    procedure SetProperties(Value: TdxColorEditProperties);
  public
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
  published
    property Anchors;
    property BeepOnEnter;
    property BiDiMode;
    property ColorValue;
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
    property Properties: TdxColorEditProperties read GetProperties write SetProperties;
    property ShowHint;
    property Style;
    property StyleDisabled;
    property StyleFocused;
    property StyleHot;
    property TabOrder;
    property TabStop;
  end;

  { TcxFilterColorEditHelper }

  TcxFilterColorEditHelper = class(TcxFilterDropDownEditHelper)
  public
    class function GetFilterEditClass: TcxCustomEditClass; override;
    class function GetSupportedFilterOperators(AProperties: TcxCustomEditProperties;
      AValueTypeClass: TcxValueTypeClass; AExtendedSet: Boolean = False): TcxFilterControlOperators; override;
  end;

implementation

uses
  Variants, Controls, cxExtEditConsts, cxColorComboBox, Math;

type
  TdxCustomColorGalleryAccess = class(TdxCustomColorGallery);

{ TdxCustomColorEditViewInfo }

procedure TdxCustomColorEditViewInfo.Paint(ACanvas: TcxCanvas);
begin
  inherited Paint(ACanvas);
  TcxColorComboBoxHelper.DrawColorBox(ACanvas, ClientRect, clBtnShadow, FColor, BackgroundColor, cxdcColor, ScaleFactor);
  if not IsInplace and Focused and not HasPopupWindow then
    ACanvas.DrawFocusRect(ClientRect);
end;

{ TdxCustomColorEditViewData }

procedure TdxCustomColorEditViewData.DisplayValueToDrawValue(
  const ADisplayValue: TcxEditValue; AViewInfo: TcxCustomEditViewInfo);
begin
  if Edit <> nil then
    EditValueToDrawValue(Edit.EditValue, AViewInfo);
end;

procedure TdxCustomColorEditViewData.EditValueToDrawValue(
  const AEditValue: TcxEditValue; AViewInfo: TcxCustomEditViewInfo);
begin
  (AViewInfo as TdxCustomColorEditViewInfo).FColor := Properties.EditValueToColorValue(AEditValue);
end;

function TdxCustomColorEditViewData.GetProperties: TdxCustomColorEditProperties;
begin
  Result := FProperties as TdxCustomColorEditProperties;
end;

function TdxCustomColorEditViewData.IsComboBoxStyle: Boolean;
begin
  Result := IsWinVistaOrLater;
end;

{ TdxCustomColorEditProperties }

constructor TdxCustomColorEditProperties.Create(AOwner: TPersistent);
begin
  inherited;
  FDefaultColor := clWindow;
  FItemShowHint := False;
  FShowItemBorders := True;
  PopupMinHeight := 30;
end;

class function TdxCustomColorEditProperties.GetContainerClass: TcxContainerClass;
begin
  Result := TdxColorEdit;
end;

class function TdxCustomColorEditProperties.GetViewInfoClass: TcxContainerViewInfoClass;
begin
  Result := TdxCustomColorEditViewInfo;
end;

procedure TdxCustomColorEditProperties.DoAssign(AProperties: TcxCustomEditProperties);
begin
  inherited;
  if AProperties is TdxCustomColorEditProperties then
    with TdxCustomColorEditProperties(AProperties) do
    begin
      Self.ColorSet := ColorSet;
      Self.ColorPalette := ColorPalette;
      Self.DefaultColor := DefaultColor;
      Self.ItemShowHint := ItemShowHint;
      Self.ItemSize := Self.ScaleFactor.Apply(ItemSize, ScaleFactor);
      Self.ShowItemBorders := ShowItemBorders;
      Self.OnGetCustomColorSet := OnGetCustomColorSet;
    end;
end;

function TdxCustomColorEditProperties.GetAlwaysPostEditValue: Boolean;
begin
  Result := True;
end;

function TdxCustomColorEditProperties.DropDownOnClick: Boolean;
begin
  Result := True;
end;

function TdxCustomColorEditProperties.GetEditingStyle: TcxEditEditingStyle;
begin
  Result := esFixedList;
end;

class function TdxCustomColorEditProperties.GetLookupDataClass: TcxInterfacedPersistentClass;
begin
//  Result := TcxCustomTextEditLookupData;
  Result := nil;
end;

class function TdxCustomColorEditProperties.GetViewDataClass: TcxCustomEditViewDataClass;
begin
  Result := TdxCustomColorEditViewData;
end;

procedure TdxCustomColorEditProperties.SetDefaultColor(Value: TColor);
begin
  if FDefaultColor <> Value then
  begin
    FDefaultColor := Value;
    Changed;
  end;
end;

procedure TdxCustomColorEditProperties.SetShowItemBorders(AValue: Boolean);
begin
  if FShowItemBorders <> AValue then
  begin
    FShowItemBorders := AValue;
    Changed;
  end;
end;

procedure TdxCustomColorEditProperties.ChangeScale(M, D: Integer);
begin
  inherited;
  if ItemSize <> 0 then
    ItemSize := Max(MulDiv(ItemSize, M, D), 1);
end;

function TdxCustomColorEditProperties.EditValueToColorValue(AEditValue: TcxEditValue): TColor;
begin
  if VarIsSoftNull(AEditValue) then
    Result := DefaultColor
  else
    Result := AEditValue;
end;

{ TdxPopupColorGallery }

constructor TdxPopupColorGallery.Create(AOwner: TComponent);
begin
  inherited;
  Keys := [kArrows, kTab];
end;

procedure TdxPopupColorGallery.HidePopup(Sender: TcxControl; AReason: TcxEditCloseUpReason);
begin
  if Assigned(FOnHidePopup) then
    FOnHidePopup(Self, AReason);
end;

procedure TdxPopupColorGallery.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  case Key of
    VK_ESCAPE:
      HidePopup(Self, crCancel);
    VK_F4:
      if not (ssAlt in Shift) then
        HidePopup(Self, crClose);
    VK_UP, VK_DOWN:
      if Shift = [ssAlt] then
        HidePopup(Self, crClose);
    VK_TAB:
      Edit.DoEditKeyDown(Key, Shift);
    VK_RETURN:
      HidePopup(Self, crEnter);
  end;
end;

function TdxPopupColorGallery.GetEdit: TdxCustomColorEdit;
begin
  Result := Owner as TdxCustomColorEdit;
end;

{ TdxCustomColorEdit }

destructor TdxCustomColorEdit.Destroy;
begin
  FreeAndNil(FColorGallery);
  inherited;
end;

class function TdxCustomColorEdit.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TdxCustomColorEditProperties;
end;

procedure TdxCustomColorEdit.PrepareEditValue(
  const ADisplayValue: TcxEditValue; out EditValue: TcxEditValue; AEditFocused: Boolean);
var
  V: Integer;
begin
  if VarIsStr(ADisplayValue) then
  begin
    if TryStrToInt(ADisplayValue, V) then
      EditValue := V
    else
      EditValue := Null;
  end
  else
    inherited;
end;

procedure TdxCustomColorEdit.DoEditKeyPress(var Key: Char);
begin
  Key := #0;
end;

procedure TdxCustomColorEdit.DropDown;
begin
  if FColorGallery = nil then
  begin
    FColorGallery := TdxPopupColorGallery.Create(Self);
    FColorGallery.Parent := PopupWindow;
    FColorGallery.OnHidePopup := HidePopup;
    FColorGallery.LookAndFeel.MasterLookAndFeel := PopupControlsLookAndFeel;
    FColorGallery.OnItemClick := ItemClickHandler;
  end;
  ActiveProperties.PopupControl := FColorGallery;
  inherited;
end;

procedure TdxCustomColorEdit.CreatePopupWindow;
begin
  inherited;
  PopupWindow.ModalMode := False;
end;

procedure TdxCustomColorEdit.InitializeInnerEdit;
begin
  inherited;
  InnerEdit.Control.Visible := False;
end;

procedure TdxCustomColorEdit.InitializePopupWindow;
begin
  inherited;
  FColorGallery.HandleNeeded;
  FColorGallery.Font := Font;
  FColorGallery.OnGetCustomColorSet := ActiveProperties.OnGetCustomColorSet;
  TdxCustomColorGalleryAccess(FColorGallery).SetColorParams(ActiveProperties.ColorPalette, ActiveProperties.ColorSet,
    ScaleFactor.Apply(ActiveProperties.ItemSize, ActiveProperties.ScaleFactor));
  FColorGallery.ItemShowHint := ActiveProperties.ItemShowHint;
  FColorGallery.ShowItemBorders := ActiveProperties.ShowItemBorders;
  FColorGallery.ColorValue := ColorValue;
end;

procedure TdxCustomColorEdit.ItemClickHandler(Sender: TObject; AItem: TdxGalleryControlItem);
var
  AColorValue: TColor;
begin
  AColorValue := FColorGallery.ColorValue;
  HidePopup(Self, crEnter);

  if (ColorValue <> AColorValue) and DoEditing then
  begin
    LockChangeEvents(True);
    try
      InternalEditValue := AColorValue;
      ModifiedAfterEnter := True;
    finally
      LockChangeEvents(False);
    end;
  end;
end;

function TdxCustomColorEdit.IsColorValueStored: Boolean;
begin
  Result := ColorValue <> ActiveProperties.DefaultColor;
end;

function TdxCustomColorEdit.GetColorValue: TColor;
begin
  Result := ActiveProperties.EditValueToColorValue(EditValue);
end;

procedure TdxCustomColorEdit.SetColorValue(AValue: TColor);
begin
  EditValue := AValue;
end;

function TdxCustomColorEdit.GetProperties: TdxCustomColorEditProperties;
begin
  Result := inherited Properties as TdxCustomColorEditProperties;
end;

function TdxCustomColorEdit.GetActiveProperties: TdxCustomColorEditProperties;
begin
  Result := inherited ActiveProperties as TdxCustomColorEditProperties;
end;

procedure TdxCustomColorEdit.SetProperties(Value: TdxCustomColorEditProperties);
begin
  Properties.Assign(Value);
end;

{ TdxColorEdit }

class function TdxColorEdit.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TdxColorEditProperties;
end;

function TdxColorEdit.GetProperties: TdxColorEditProperties;
begin
  Result := TdxColorEditProperties(inherited Properties);
end;

procedure TdxColorEdit.SetProperties(Value: TdxColorEditProperties);
begin
  Properties.Assign(Value);
end;

{ TcxFilterColorEditHelper }

class function TcxFilterColorEditHelper.GetFilterEditClass: TcxCustomEditClass;
begin
  Result := TdxColorEdit;
end;

class function TcxFilterColorEditHelper.GetSupportedFilterOperators(AProperties: TcxCustomEditProperties;
  AValueTypeClass: TcxValueTypeClass; AExtendedSet: Boolean = False): TcxFilterControlOperators;
begin
  Result := [fcoEqual, fcoNotEqual, fcoBlanks, fcoNonBlanks];
  if AExtendedSet then
    Result := Result + [fcoInList, fcoNotInList];
end;

initialization
  GetRegisteredEditProperties.Register(TdxColorEditProperties, scxSEditRepositoryColorEditItem);
  FilterEditsController.Register(TdxColorEditProperties, TcxFilterColorEditHelper);

finalization
  FilterEditsController.Unregister(TdxColorEditProperties, TcxFilterColorEditHelper);
  GetRegisteredEditProperties.Unregister(TdxColorEditProperties);

end.
