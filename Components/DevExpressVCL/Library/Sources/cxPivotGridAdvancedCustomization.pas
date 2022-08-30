{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressPivotGrid                                         }
{                                                                    }
{           Copyright (c) 2005-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSPIVOTGRID AND ALL ACCOMPANYING }
{   VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY.              }
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

unit cxPivotGridAdvancedCustomization;

{$I cxVer.inc}

interface

uses
  Windows, Messages, Graphics, Classes, Controls, ExtCtrls, ActnList, Menus, StdCtrls, ImgList,
  cxCustomPivotGrid, cxPivotGridCustomization, cxGraphics, cxControls, cxLookAndFeels,
  cxLookAndFeelPainters, cxContainer, cxEdit, cxGroupBox, cxButtons, cxCheckBox, cxLabel,
  cxSplitter, dxGDIPlusClasses, cxImageList, cxImage, dxMessages;

type
  TcxPivotGridAdvancedCustomizationForm = class;

  { TcxPivotGridCustomizationFormLayoutChooser }

  TcxPivotGridCustomizationFormLayoutChooserClass = class of TcxPivotGridCustomizationFormLayoutChooser;
  TcxPivotGridCustomizationFormLayoutChooser = class
  strict private
    FFieldChooser: TcxPivotGridAdvancedCustomizationForm;
    FPopupMenu: TComponent;
  protected
    procedure CreateItem(Action: TAction); virtual; abstract;
    procedure CreateItems;
    function GetPopupMenuClass: TComponentClass; virtual; abstract;
    procedure InitializePopupMenu; virtual;
  public
    constructor Create(AFieldChooser: TcxPivotGridAdvancedCustomizationForm); virtual;
    destructor Destroy; override;

    procedure Popup(X, Y: Integer);

    property FieldChooser: TcxPivotGridAdvancedCustomizationForm read FFieldChooser;
    property PopupMenu: TComponent read FPopupMenu;
  end;

  { TcxPivotGridCustomizationFormStandardLayoutChooser }

  TcxPivotGridCustomizationFormStandardLayoutChooser = class(TcxPivotGridCustomizationFormLayoutChooser)
  private
    procedure DrawItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect; State: TOwnerDrawState);
    function GetPopupMenu: TPopupMenu;
  protected
    procedure CreateItem(Action: TAction); override;
    function GetPopupMenuClass: TComponentClass; override;
    procedure InitializePopupMenu; override;
  public
    property PopupMenu: TPopupMenu read GetPopupMenu;
  end;

  { TcxPivotGridAdvancedCustomizationForm }

  TcxPivotGridCustomizationFormLayout = (cflBottomPanelOnly1by4, cflBottomPanelOnly2by2,
    cflStackedDefault, cflStackedSideBySide, cflTopPanelOnly);
  TcxPivotGridCustomizationFormLayouts = set of TcxPivotGridCustomizationFormLayout;

  TcxPivotGridAdvancedCustomizationForm = class(TcxPivotGridCustomCustomizationForm)
    acBottomPanelOnly1by4: TAction;
    acBottomPanelOnly2by2: TAction;
    acStackedDefault: TAction;
    acStackedSideBySide: TAction;
    acTopPanelOnly: TAction;
    alLayoutChooser: TActionList;
    btnLayoutMode: TcxButton;
    btnUpdate: TcxButton;
    cbDeferLayoutUpdate: TcxCheckBox;
    gbColumnArea: TcxGroupBox;
    gbDataArea: TcxGroupBox;
    gbFields: TcxGroupBox;
    gbFilterArea: TcxGroupBox;
    gbMain: TcxGroupBox;
    gbMainBottom: TcxGroupBox;
    gbMainCenter: TcxGroupBox;
    gbMainVisibleFields: TcxGroupBox;
    gbRowArea: TcxGroupBox;
    gbTop: TcxGroupBox;
    ilLayoutChooser: TcxImageList;
    imgColumnArea: TcxImage;
    imgDataAdea: TcxImage;
    imgFilterArea: TcxImage;
    imgRowArea: TcxImage;
    lbColumnArea: TLabel;
    lbDataArea: TLabel;
    lbEmpty: TLabel;
    lbFilterArea: TLabel;
    lbMain: TLabel;
    lbRowArea: TLabel;
    lbTopMain: TLabel;
    splAvailableFieldList: TcxSplitter;
    splMainVisibleFields: TcxSplitter;

    procedure btnLayoutModeClick(Sender: TObject);
    procedure btnUpdateClick(Sender: TObject);
    procedure cbDeferLayoutUpdateClick(Sender: TObject);
    procedure gbMainResize(Sender: TObject);
    procedure LayoutChooserExecute(Sender: TObject);
  private
    FMainCaption: string;
    FAvailableFieldList: TcxFieldListListBox;
    FColumnAreaFieldList: TcxFieldListListBox;
    FDataAreaFieldList: TcxFieldListListBox;
    FFilterAreaFieldList: TcxFieldListListBox;
    FAllowedLayouts: TcxPivotGridCustomizationFormLayouts;
    FLayout: TcxPivotGridCustomizationFormLayout;
    FRowAreaFieldList: TcxFieldListListBox;
    FLayoutChooser: TcxPivotGridCustomizationFormLayoutChooser;

    FMainFieldsHeight: Integer;
    FMainFieldsWidth: Integer;

    procedure AlignChildren(AGroup: TcxGroupBox; AColumnCount, ARowCount: Integer);
    procedure CalculateTabOrders;
    function CreateFieldListBox(AParent: TWinControl; AFieldsType: TcxPivotGridCustomizationFormFieldListType): TcxFieldListListBox;
    procedure ListBoxDblClick(Sender: TObject);
    procedure SetAllowedLayouts(const AValue: TcxPivotGridCustomizationFormLayouts);
    procedure SetLayout(AValue: TcxPivotGridCustomizationFormLayout);
    //
    procedure DXMScaleChanged(var Message: TMessage); message DXM_SCALECHANGED;
  protected
    function CanChangeFieldSortOrder: Boolean; override;
    function CanChangeFieldFilter: Boolean; override;
    procedure DoCreateControls; override;
    procedure DoUpdateSelection; override;
    function GetFieldListByType(AListType: TcxPivotGridCustomizationFormFieldListType): TObject; override;
    function GetImmediateUpdate: Boolean; override;
    function HasDeferredLayoutChanges: Boolean;
    procedure Init; override;
    procedure Localize; override;
    procedure LookAndFeelChanged; override;
    procedure SynchronizeFields; virtual;
    procedure UpdateButtonState; override;

    function GetHorzContentIndents: Integer;
    function GetVertContentIndents: Integer;
    procedure BottomPanelOnly1by4FormCalculator;
    procedure BottomPanelOnly2by2FormCalculator;
    procedure SetIsLayoutChanged(AValue: Boolean); override;
    procedure StackedDefaultFormCalculator;
    procedure StackedSideBySideFormCalculator;
    procedure TopPanelOnlyFormCalculator;

    procedure InternalDisableAlign(AControl: TWinControl);
    procedure InternalEnableAlign(AControl: TWinControl);

    property LayoutChooser: TcxPivotGridCustomizationFormLayoutChooser read FLayoutChooser;
    procedure PopulateFieldLists; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure CalculateFormLayout; override;
    procedure RefreshList; override;

    property AllowedLayouts: TcxPivotGridCustomizationFormLayouts read FAllowedLayouts write SetAllowedLayouts;
    property AvailableFieldList: TcxFieldListListBox read FAvailableFieldList;
    property ColumnAreaFieldList: TcxFieldListListBox read FColumnAreaFieldList;
    property DataAreaFieldList: TcxFieldListListBox read FDataAreaFieldList;
    property FilterAreaFieldList: TcxFieldListListBox read FFilterAreaFieldList;
    property Layout: TcxPivotGridCustomizationFormLayout read FLayout write SetLayout;
    property RowAreaFieldList: TcxFieldListListBox read FRowAreaFieldList;
  end;

var
  cxPivotGridCustomizationFormLayoutChooserClass: TcxPivotGridCustomizationFormLayoutChooserClass;

implementation

{$R *.DFM}

uses
  Types, Contnrs, cxGeometry, Math, SysUtils, Forms, cxListBox, cxClasses, cxPivotGridStrs, dxCore;

const
  cxControlsIndent = 6;
  cxSplitterSize = 8;
  cxDefaultFormAllowedLayouts = [cflBottomPanelOnly1by4, cflBottomPanelOnly2by2, cflStackedDefault, cflStackedSideBySide, cflTopPanelOnly];

{ TcxPivotGridCustomizationFormLayoutChooser }

constructor TcxPivotGridCustomizationFormLayoutChooser.Create(
  AFieldChooser: TcxPivotGridAdvancedCustomizationForm);
begin
  inherited Create;
  FFieldChooser := AFieldChooser;
  FPopupMenu := GetPopupMenuClass.Create(nil);
  InitializePopupMenu;
end;

destructor TcxPivotGridCustomizationFormLayoutChooser.Destroy;
begin
  FreeAndNil(FPopupMenu);
  inherited;
end;

procedure TcxPivotGridCustomizationFormLayoutChooser.Popup(X, Y: Integer);
begin
  if PopupMenu is TMenu then
    TMenu(PopupMenu).BiDiMode := FieldChooser.BiDiMode;
  ShowPopupMenu(nil, PopupMenu, X, Y);
end;

procedure TcxPivotGridCustomizationFormLayoutChooser.CreateItems;
begin
  CreateItem(FieldChooser.acStackedDefault);
  CreateItem(FieldChooser.acStackedSideBySide);
  CreateItem(FieldChooser.acTopPanelOnly);
  CreateItem(FieldChooser.acBottomPanelOnly1by4);
  CreateItem(FieldChooser.acBottomPanelOnly2by2);
end;

procedure TcxPivotGridCustomizationFormLayoutChooser.InitializePopupMenu;
begin
  CreateItems;
end;

{ TcxPivotGridCustomizationFormStandardLayoutChooser }

procedure TcxPivotGridCustomizationFormStandardLayoutChooser.CreateItem(Action: TAction);
var
  AItem: TMenuItem;
begin
  AItem := TMenuItem.Create(PopupMenu);
  AItem.Action := Action;
  AItem.OnAdvancedDrawItem := DrawItem;
  PopupMenu.Items.Add(AItem);
end;

function TcxPivotGridCustomizationFormStandardLayoutChooser.GetPopupMenuClass: TComponentClass;
begin
  Result := TPopupMenu;
end;

procedure TcxPivotGridCustomizationFormStandardLayoutChooser.InitializePopupMenu;
begin
  inherited;
  PopupMenu.Images := FieldChooser.ilLayoutChooser;
end;

procedure TcxPivotGridCustomizationFormStandardLayoutChooser.DrawItem(
  Sender: TObject; ACanvas: TCanvas; ARect: TRect; State: TOwnerDrawState);
begin
  cxAdvancedDrawPopupMenuItem(TMenuItem(Sender), ACanvas, ARect, State, PopupMenu.Images);
end;

function TcxPivotGridCustomizationFormStandardLayoutChooser.GetPopupMenu: TPopupMenu;
begin
  Result := TPopupMenu(inherited PopupMenu);
end;

{ TcxPivotGridAdvancedCustomizationForm }

constructor TcxPivotGridAdvancedCustomizationForm.Create(AOwner: TComponent);
begin
  inherited;
  DoubleBuffered := True;
  FLayoutChooser := cxPivotGridCustomizationFormLayoutChooserClass.Create(Self);
  FLayout := cflStackedDefault;
  FAllowedLayouts := cxDefaultFormAllowedLayouts;
  lbEmpty.Width := cxControlsIndent;
end;

destructor TcxPivotGridAdvancedCustomizationForm.Destroy;
begin
  FreeAndNil(FLayoutChooser);
  inherited;
end;

procedure TcxPivotGridAdvancedCustomizationForm.CalculateFormLayout;
begin
  if PivotGrid = nil then
    Exit;
  if Visible then
    InternalDisableAlign(Self);
  try
    case Layout of
      cflBottomPanelOnly1by4:
        BottomPanelOnly1by4FormCalculator;
      cflBottomPanelOnly2by2:
        BottomPanelOnly2by2FormCalculator;
      cflStackedDefault:
        StackedDefaultFormCalculator;
      cflStackedSideBySide:
        StackedSideBySideFormCalculator;
      cflTopPanelOnly:
        TopPanelOnlyFormCalculator;
    end;
    CalculateTabOrders;
    lbEmpty.Visible := Layout = cflStackedSideBySide;
    FMainFieldsHeight := -1;
    FMainFieldsWidth := -1;
  finally
    if Visible then
      InternalEnableAlign(Self);
  end;
end;

procedure TcxPivotGridAdvancedCustomizationForm.RefreshList;
begin
  inherited;
  PopulateFieldLists;
  UpdateSelection;
  IsLayoutChanged := False;
end;

function TcxPivotGridAdvancedCustomizationForm.CanChangeFieldSortOrder: Boolean;
begin
  Result := not HasDeferredLayoutChanges;
end;

function TcxPivotGridAdvancedCustomizationForm.CanChangeFieldFilter: Boolean;
begin
  Result := not HasDeferredLayoutChanges;
end;

procedure TcxPivotGridAdvancedCustomizationForm.DoCreateControls;
begin
  inherited;
  FAvailableFieldList := CreateFieldListBox(gbFields, fltAvailable);
  FAvailableFieldList.TabOrder := 0;
  splAvailableFieldList.Control := FAvailableFieldList;
  FColumnAreaFieldList := CreateFieldListBox(gbColumnArea, fltColumn);
  FDataAreaFieldList := CreateFieldListBox(gbDataArea, fltData);
  FFilterAreaFieldList := CreateFieldListBox(gbFilterArea, fltFilter);
  FRowAreaFieldList := CreateFieldListBox(gbRowArea, fltRow);
end;

procedure TcxPivotGridAdvancedCustomizationForm.DoUpdateSelection;
var
  I: TcxPivotGridCustomizationFormFieldListType;
begin
  for I := Low(TcxPivotGridCustomizationFormFieldListType) to High(TcxPivotGridCustomizationFormFieldListType) do
    if GetFieldListByType(I) <> nil then
      (GetFieldListByType(I) as TcxFieldListListBox).UpdateSelection;
end;

procedure TcxPivotGridAdvancedCustomizationForm.DXMScaleChanged(var Message: TMessage);
begin
  inherited;
  CalculateFormLayout;
end;

function TcxPivotGridAdvancedCustomizationForm.GetFieldListByType(AListType: TcxPivotGridCustomizationFormFieldListType): TObject;
begin
  Result := AvailableFieldList;
  case AListType of
    fltColumn:
      Result := ColumnAreaFieldList;
    fltRow:
      Result := RowAreaFieldList;
    fltFilter:
      Result := FilterAreaFieldList;
    fltData:
      Result := DataAreaFieldList;
   end;
end;

function TcxPivotGridAdvancedCustomizationForm.GetImmediateUpdate: Boolean;
begin
  Result := not cbDeferLayoutUpdate.Checked;
end;

function TcxPivotGridAdvancedCustomizationForm.HasDeferredLayoutChanges: Boolean;
begin
  Result := cbDeferLayoutUpdate.Checked and IsLayoutChanged;
end;

procedure TcxPivotGridAdvancedCustomizationForm.Init;
begin
  inherited Init;
  gbMainBottom.Constraints.MinHeight := gbTop.Constraints.MinHeight;
  gbMainBottom.Constraints.MaxHeight := gbMainBottom.Constraints.MinHeight;
  gbMainBottom.Height := gbMainBottom.Constraints.MinHeight;
  cbDeferLayoutUpdate.Top := cxControlsIndent;
  btnUpdate.Top := cxControlsIndent;
  splMainVisibleFields.Height := cxSplitterSize;
  splAvailableFieldList.Width := cxSplitterSize;
  imgColumnArea.Enabled := False;
  imgDataAdea.Enabled := False;
  imgFilterArea.Enabled := False;
  imgRowArea.Enabled := False;
end;

procedure TcxPivotGridAdvancedCustomizationForm.Localize;
begin
  inherited;
  btnUpdate.Caption := cxGetResourceString(@scxUpdate);
  btnUpdate.Hint := StripHotKey(btnUpdate.Caption);

  cbDeferLayoutUpdate.Caption := cxGetResourceString(@scxDeferLayoutUpdate);
  cbDeferLayoutUpdate.Hint := StripHotKey(cbDeferLayoutUpdate.Caption);

  FMainCaption := cxGetResourceString(@scxAdvancedCustomizationFormMainCaption);
  lbTopMain.Caption := cxGetResourceString(@scxAdvancedCustomizationFormFieldsCaption);
  lbFilterArea.Caption := cxGetResourceString(@scxAdvancedCustomizationFormFilterAreaCaption);
  lbColumnArea.Caption := cxGetResourceString(@scxAdvancedCustomizationFormColumnAreaCaption);
  lbRowArea.Caption := cxGetResourceString(@scxAdvancedCustomizationFormRowAreaCaption);
  lbDataArea.Caption := cxGetResourceString(@scxAdvancedCustomizationFormDataAreaCaption);

  acBottomPanelOnly1by4.Caption := cxGetResourceString(@scxAdvancedCustomizationFormBottomPanelOnly1by4);
  acBottomPanelOnly2by2.Caption := cxGetResourceString(@scxAdvancedCustomizationFormBottomPanelOnly2by2);
  acStackedDefault.Caption := cxGetResourceString(@scxAdvancedCustomizationFormStackedDefault);
  acStackedSideBySide.Caption := cxGetResourceString(@scxAdvancedCustomizationFormStackedSideBySide);
  acTopPanelOnly.Caption := cxGetResourceString(@scxAdvancedCustomizationFormTopPanelOnly);
end;

procedure TcxPivotGridAdvancedCustomizationForm.LookAndFeelChanged;
begin
  inherited;
  Color := Painter.DefaultGroupColor;
end;

procedure TcxPivotGridAdvancedCustomizationForm.SynchronizeFields;
var
  I: TcxPivotGridCustomizationFormFieldListType;
begin
  PivotGrid.BeginUpdate;
  try
    for I := Low(TcxPivotGridCustomizationFormFieldListType) to High(TcxPivotGridCustomizationFormFieldListType) do
      if GetFieldListByType(I) <> nil then
        (GetFieldListByType(I) as TcxFieldListListBox).SynchronizeFields;
  finally
    PivotGrid.EndUpdate;
  end;
end;

function TcxPivotGridAdvancedCustomizationForm.GetHorzContentIndents: Integer;
begin
  Result := 2 * BorderWidth + 2 * ScaleFactor.Apply(cxControlsIndent) + Width - ClientWidth;
end;

function TcxPivotGridAdvancedCustomizationForm.GetVertContentIndents: Integer;
begin
  Result := 2 * BorderWidth + 2 * ScaleFactor.Apply(cxControlsIndent) + Height - ClientHeight;
end;

procedure TcxPivotGridAdvancedCustomizationForm.BottomPanelOnly1by4FormCalculator;
begin
  gbMainVisibleFields.Constraints.MinHeight := gbFilterArea.Constraints.MinHeight +
    gbColumnArea.Constraints.MinHeight + gbRowArea.Constraints.MinHeight +
    gbDataArea.Constraints.MinHeight + gbMainBottom.Constraints.MinHeight;
  gbMainVisibleFields.Constraints.MinWidth := gbMainBottom.Constraints.MinWidth;

  Constraints.MinHeight := gbMainVisibleFields.Constraints.MinHeight + gbTop.Constraints.MinHeight + GetVertContentIndents;
  Constraints.MinWidth := gbTop.Constraints.MinWidth + GetHorzContentIndents;

  lbTopMain.Visible := False;
  gbMainResize(gbMainVisibleFields);
  gbMainVisibleFields.Visible := True;
  gbMainVisibleFields.Align := alClient;

  AvailableFieldList.Visible := False;
  splAvailableFieldList.Visible := False;
  splMainVisibleFields.Visible := False;

  lbMain.Caption := lbTopMain.Caption;
end;

procedure TcxPivotGridAdvancedCustomizationForm.BottomPanelOnly2by2FormCalculator;
begin
  gbMainVisibleFields.Constraints.MinHeight := gbFilterArea.Constraints.MinHeight +
    gbColumnArea.Constraints.MinHeight + gbMainBottom.Constraints.MinHeight;
  gbMainVisibleFields.Constraints.MinWidth := gbMainBottom.Constraints.MinWidth;
  Constraints.MinHeight := gbMainVisibleFields.Constraints.MinHeight + gbTop.Constraints.MinHeight + GetVertContentIndents;
  Constraints.MinWidth := gbTop.Constraints.MinWidth + GetHorzContentIndents;

  lbTopMain.Visible := False;
  gbMainResize(gbMainVisibleFields);
  gbMainVisibleFields.Visible := True;
  gbMainVisibleFields.Align := alClient;

  AvailableFieldList.Visible := False;
  splAvailableFieldList.Visible := False;
  splMainVisibleFields.Visible := False;

  lbMain.Caption := lbTopMain.Caption;
end;

procedure TcxPivotGridAdvancedCustomizationForm.StackedDefaultFormCalculator;
begin
  gbMainVisibleFields.Constraints.MinHeight := gbFilterArea.Constraints.MinHeight +
    gbColumnArea.Constraints.MinHeight + gbMainBottom.Height + gbTop.Height;
  gbMainVisibleFields.Constraints.MinWidth := gbMainBottom.Constraints.MinWidth;

  Constraints.MinWidth := gbMainVisibleFields.Constraints.MinWidth + GetHorzContentIndents;
  Constraints.MinHeight := gbMainVisibleFields.Constraints.MinHeight + ScaleFactor.Apply(cxSplitterSize) +
    AvailableFieldList.Constraints.MinHeight + gbTop.Constraints.MinHeight + GetVertContentIndents;

  lbTopMain.Visible := True;
  gbMainVisibleFields.Visible := True;
  gbMainVisibleFields.Align := alBottom;
  gbMainVisibleFields.Height := MulDiv(gbMainVisibleFields.Parent.ClientHeight, 3, 4);

  AvailableFieldList.Align := alClient;
  AvailableFieldList.Visible := True;

  splAvailableFieldList.Visible := False;
  splMainVisibleFields.Visible := True;
  splMainVisibleFields.Top := 0;

  lbMain.Caption := FMainCaption;
end;

procedure TcxPivotGridAdvancedCustomizationForm.StackedSideBySideFormCalculator;
begin
  gbMainVisibleFields.Constraints.MinHeight := gbFilterArea.Constraints.MinHeight +
    gbColumnArea.Constraints.MinHeight + gbRowArea.Constraints.MinHeight + gbDataArea.Constraints.MinHeight +
    gbMainBottom.Constraints.MinHeight + gbTop.Height;
  gbMainVisibleFields.Constraints.MinWidth := gbMainBottom.Constraints.MinWidth + ScaleFactor.Apply(cxControlsIndent);

  Constraints.MinHeight := gbMainVisibleFields.Constraints.MinHeight + gbTop.Constraints.MinHeight + GetVertContentIndents;
  Constraints.MinWidth := gbMainVisibleFields.Constraints.MinWidth +
    ScaleFactor.Apply(cxSplitterSize) + AvailableFieldList.Constraints.MinWidth + GetHorzContentIndents;

  lbTopMain.Visible := True;
  gbMainVisibleFields.Visible := True;
  gbMainVisibleFields.Align := alRight;
  gbMainVisibleFields.Width := gbMainVisibleFields.Parent.ClientWidth div 2;

  AvailableFieldList.Align := alClient;
  AvailableFieldList.Visible := True;

  splMainVisibleFields.Visible := False;
  splAvailableFieldList.Visible := True;
  splAvailableFieldList.Left := 0;

  lbMain.Caption := FMainCaption;
end;

procedure TcxPivotGridAdvancedCustomizationForm.TopPanelOnlyFormCalculator;
begin
  gbMainVisibleFields.Visible := False;
  splMainVisibleFields.Visible := False;
  splAvailableFieldList.Visible := False;
  AvailableFieldList.Visible := True;
  AvailableFieldList.Align := alClient;

  Constraints.MinHeight := AvailableFieldList.Constraints.MinHeight + gbTop.Constraints.MinHeight + GetVertContentIndents;
  Constraints.MinWidth := gbTop.Constraints.MinWidth + GetHorzContentIndents;
  lbMain.Caption := FMainCaption;
end;

procedure TcxPivotGridAdvancedCustomizationForm.SetIsLayoutChanged(AValue: Boolean);
begin
  inherited SetIsLayoutChanged(AValue);
  UpdateButtonState;
end;

procedure TcxPivotGridAdvancedCustomizationForm.UpdateButtonState;
begin
  inherited;
  btnUpdate.Enabled := HasDeferredLayoutChanges;
  btnLayoutMode.Visible := AllowedLayouts <> [];
  acBottomPanelOnly1by4.Visible := cflBottomPanelOnly1by4 in AllowedLayouts;
  acBottomPanelOnly2by2.Visible := cflBottomPanelOnly2by2 in AllowedLayouts;
  acStackedDefault.Visible := cflStackedDefault in AllowedLayouts;
  acStackedSideBySide.Visible := cflStackedSideBySide in AllowedLayouts;
  acTopPanelOnly.Visible := cflTopPanelOnly in AllowedLayouts;
end;

procedure TcxPivotGridAdvancedCustomizationForm.InternalDisableAlign(
  AControl: TWinControl);
var
  I: Integer;
begin
  AControl.DisableAlign;
  for I := 0 to AControl.ControlCount - 1 do
    if AControl.Controls[I] is TWinControl then
      InternalDisableAlign(TWinControl(AControl.Controls[I]));
end;

procedure TcxPivotGridAdvancedCustomizationForm.InternalEnableAlign(
  AControl: TWinControl);
var
  I: Integer;
begin
  AControl.EnableAlign;
  for I := 0 to AControl.ControlCount - 1 do
    if AControl.Controls[I] is TWinControl then
      InternalEnableAlign(TWinControl(AControl.Controls[I]));
end;

procedure TcxPivotGridAdvancedCustomizationForm.PopulateFieldLists;
begin
  cxPopulateListBox(Self, AvailableFieldList, fltAvailable);
  cxPopulateListBox(Self, ColumnAreaFieldList, fltColumn);
  cxPopulateListBox(Self, FilterAreaFieldList, fltFilter);
  cxPopulateListBox(Self, DataAreaFieldList, fltData);
  cxPopulateListBox(Self, RowAreaFieldList, fltRow);
end;

procedure TcxPivotGridAdvancedCustomizationForm.AlignChildren(AGroup: TcxGroupBox;
  AColumnCount, ARowCount: Integer);

  procedure AlignImagesAndCaption(AImage: TcxImage; ALabel: TLabel; AParent: TcxGroupBox);
  begin
    if UseRightToLeftAlignment then
    begin
      AImage.Left := AParent.Width - AImage.Width;
      ALabel.Left := 0;
      ALabel.Width := AImage.Left - ScaleFactor.Apply(4);
    end
    else
    begin
      AImage.Left := 0;
      ALabel.Left := AImage.Width + ScaleFactor.Apply(4);
    end;

  end;

const
  AButtonAlign: array[Boolean] of TAlign = (alRight, alLeft);
var
  I: Integer;
  R, ABounds: TRect;
  X, Y: Integer;
  AList: TList;
begin
  R := AGroup.ClientRect;
  R.Right := (cxRectWidth(R) - cxControlsIndent * (AColumnCount - 1)) div AColumnCount;
  R.Bottom := (cxRectHeight(R) - IfThen(lbTopMain.Visible, lbTopMain.Height, 0) - gbMainBottom.Height) div ARowCount;
  X := cxRectWidth(R) + cxControlsIndent;
  Y := cxRectHeight(R);
  if lbTopMain.Visible then
    R := cxRectOffset(R, 0, lbTopMain.Height);
  AList := TList.Create;
  try
    AList.Add(gbFilterArea);
    AList.Add(gbRowArea);
    AList.Add(gbColumnArea);
    AList.Add(gbDataArea);
    for I := 0 to AList.Count - 1 do
    begin
      ABounds := cxRectOffset(R, X * (I div ARowCount), Y * (I mod ARowCount));
      if UseRightToLeftAlignment then
        ABounds := TdxRightToLeftLayoutConverter.ConvertRect(ABounds, AGroup.ClientRect);
      TWinControl(AList[I]).BoundsRect := ABounds;
    end;
  finally
    AList.Free;
  end;
  btnUpdate.Align := AButtonAlign[UseRightToLeftAlignment];
  btnLayoutMode.Align := AButtonAlign[UseRightToLeftAlignment];
  AlignImagesAndCaption(imgColumnArea, lbColumnArea, gbColumnArea);
  AlignImagesAndCaption(imgDataAdea, lbDataArea, gbDataArea);
  AlignImagesAndCaption(imgFilterArea, lbFilterArea, gbFilterArea);
  AlignImagesAndCaption(imgRowArea, lbRowArea, gbRowArea);
end;

procedure TcxPivotGridAdvancedCustomizationForm.CalculateTabOrders;
begin
  gbFilterArea.TabOrder := 0;
  if Layout in [cflBottomPanelOnly1by4, cflStackedSideBySide] then
  begin
    gbRowArea.TabOrder := 1;
    gbColumnArea.TabOrder := 2;
  end
  else
  begin
    gbColumnArea.TabOrder := 1;
    gbRowArea.TabOrder := 2;
  end;
  gbDataArea.TabOrder := 3;
end;

function TcxPivotGridAdvancedCustomizationForm.CreateFieldListBox(AParent: TWinControl;
  AFieldsType: TcxPivotGridCustomizationFormFieldListType): TcxFieldListListBox;
var
  R: TRect;
begin
  Result := TcxFieldListListBox.CreateEx(Self, AFieldsType);
  Result.Parent := AParent;
  if Result.Parent <> gbFields then
  begin
    R := AParent.ClientRect;
    R.Top := AParent.Controls[0].BoundsRect.Bottom + ScaleFactor.Apply(cxControlsIndent);
    Result.BoundsRect := R;
  end
  else
    Result.Visible := False;

  Result.Anchors := AnchorAlign[alClient];
  Result.LookAndFeel.MasterLookAndFeel := PivotGrid.LookAndFeel;
  Result.ItemHeight := Painter.ScaledHeaderHeight(cxTextHeight(Font), ScaleFactor);
  Result.OnDblClick := ListBoxDblClick;
  Result.Constraints.MinHeight := ScaleFactor.Apply(30);
  Result.Constraints.MinWidth := ScaleFactor.Apply(30);
end;

procedure TcxPivotGridAdvancedCustomizationForm.ListBoxDblClick(Sender: TObject);
var
  AListBox: TcxFieldListListBox;
begin
  if CanChangeFieldFilter then
  begin
    AListBox := TcxFieldListListBox(Sender);
    UpdateHitTest;
    if (AListBox.SelectedField <> nil) and not PivotGrid.HitTest.HitAtFilter then
      ChangeFieldSorting(AListBox.SelectedField);
  end;
end;

procedure TcxPivotGridAdvancedCustomizationForm.SetAllowedLayouts(const AValue: TcxPivotGridCustomizationFormLayouts);
begin
  if FAllowedLayouts <> AValue then
  begin
    FAllowedLayouts := AValue;
    UpdateButtonState;
  end;
end;

procedure TcxPivotGridAdvancedCustomizationForm.SetLayout(AValue: TcxPivotGridCustomizationFormLayout);
begin
  if FLayout <> AValue then
  begin
    FLayout := AValue;
    CalculateFormLayout;
  end;
end;

procedure TcxPivotGridAdvancedCustomizationForm.gbMainResize(Sender: TObject);
const
  RowCountMap: array[TcxPivotGridCustomizationFormLayout] of Integer = (4, 2, 2, 4, 1);
  ColumnCountMap: array[TcxPivotGridCustomizationFormLayout] of Integer = (1, 2, 2, 1, 1);
begin
  AlignChildren(TcxGroupBox(Sender), ColumnCountMap[Layout], RowCountMap[Layout]);
end;

procedure TcxPivotGridAdvancedCustomizationForm.LayoutChooserExecute(
  Sender: TObject);
begin
  Layout := TcxPivotGridCustomizationFormLayout(TComponent(Sender).Tag);
end;

procedure TcxPivotGridAdvancedCustomizationForm.btnLayoutModeClick(
  Sender: TObject);

  procedure CheckStates;
  var
    I: Integer;
  begin
    for I := 0 to alLayoutChooser.ActionCount - 1 do
      with TAction(alLayoutChooser.Actions[I]) do
        Checked := Tag = Ord(Layout);
  end;

var
  P: TPoint;
begin
  CheckStates;
  P.X := 0;
  if UseRightToLeftAlignment then
    P.X := btnLayoutMode.Width;
  P.Y := btnLayoutMode.Height;
  P := btnLayoutMode.ClientToScreen(P);
  LayoutChooser.Popup(P.X, P.Y);
end;

procedure TcxPivotGridAdvancedCustomizationForm.cbDeferLayoutUpdateClick(
  Sender: TObject);
begin
  if IsLayoutChanged then
    RefreshList;
  UpdateButtonState;
end;

procedure TcxPivotGridAdvancedCustomizationForm.btnUpdateClick(
  Sender: TObject);
begin
  if not IsLayoutChanged then Exit;
  SynchronizeFields;
  IsLayoutChanged := False;
end;

initialization
  cxPivotGridCustomizationFormLayoutChooserClass := TcxPivotGridCustomizationFormStandardLayoutChooser;

end.
