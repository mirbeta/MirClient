{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressLayoutControl registering unit                    }
{                                                                    }
{           Copyright (c) 2001-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSLAYOUTCONTROL AND ALL          }
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

unit dxLayoutControlReg;

{$I cxVer.inc}

interface

uses
  dxLayoutControl;

procedure Register;

implementation

uses
  Windows, SysUtils, Classes, Graphics, Controls, Contnrs, Forms, StdCtrls, ExtCtrls,
  Messages, DesignIntf, DesignEditors, DesignMenus, VCLEditors, DesignWindows,
  ComponentDesigner, TypInfo, ImgList, cxEditReg,
  dxCore, cxClasses, cxControls, cxContainer, cxLookAndFeels,
  dxCoreReg, cxLibraryReg, cxDesignWindows, dxRegEd, cxPC, DB,
  dxLayoutCommon, dxLayoutLookAndFeels, cxPropEditors, dxLayoutEditForm,
  dxLayoutLookAndFeelListDesignForm, dxLayoutSelection, dxLayoutImport, dxLayoutContainer;

const
  dxLayoutControlProductName = 'ExpressLayoutControl';

type
  TControlAccess = class(TControl);
  TLabelAccess = class(TCustomLabel);
  TdxLayoutGroupAcsess = class(TdxLayoutGroup);
  TdxLayoutContainerAccess = class(TdxLayoutContainer);
  TdxLayoutControlAccess = class(TdxLayoutControl);
  TdxCustomLayoutItemAccess = class(TdxCustomLayoutItem);
  TdxLayoutItemAccess = class(TdxLayoutItem);
  TStaticTextAccess = class(TCustomStaticText);
  TdxCustomLayoutItemCaptionOptionsAccess = class(TdxCustomLayoutItemCaptionOptions);

  { TdxLayoutCustomControlEditor }

  TdxLayoutCustomControlEditor = class(TdxComponentEditor)
  protected
    function GetProductName: string; override;
  end;

  { TdxLayoutControlEditor }

  TdxLayoutControlEditor = class(TdxLayoutCustomControlEditor)
  protected
    procedure DoLinkTo(AObject: TObject); override;
    function GetLinkToItemCaption: string; override;
    function GetLinkToTypeClass: TClass; override;
    function IsLinkable: Boolean; override;

    procedure InternalExecuteVerb(AIndex: Integer); override;
    function InternalGetVerb(AIndex: Integer): string; override;
    function InternalGetVerbCount: Integer; override;

    procedure DoImport;
    function GetControl: TdxLayoutControl; virtual;

    property Control: TdxLayoutControl read GetControl;
  public
    procedure PrepareItem(Index: Integer; const AItem: IMenuItem); override;
  end;

  { TdxLayoutControlItemsEditor }

  TdxLayoutControlItemsEditor = class(TdxLayoutCustomControlEditor);

  { TdxLayoutDesignTimeSelectionHelper }

  TdxLayoutDesignTimeSelectionHelper = class(TdxLayoutRunTimeSelectionHelper, IcxDesignSelectionChanged)
  private
    FIsActive: Boolean;
    FDesignHelper: TcxDesignHelper;
    FLayoutControl: TdxCustomLayoutControl;
    FOldSelection: TcxComponentList;
  protected
    //IcxDesignSelectionChanged
    procedure DesignSelectionChanged(ASelection: TList);
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;

    function IsActive: Boolean; override;
    procedure AddSelectionChangedListener(AListener: TPersistent); override;
    procedure RemoveSelectionChangedListener(AListener: TPersistent); override;
    function CanModify: Boolean; override;
    function CanDeleteComponent(AComponent: TComponent): Boolean; override;
    procedure ClearSelection; override;
    procedure DeleteComponents(AList: TComponentList); override;
    procedure GetSelection(AList: TList); override;
    function IsComponentSelected(AComponent: TPersistent): Boolean; override;
    procedure SelectComponent(AComponent: TPersistent; AShift: TShiftState = []); override;
    procedure SetSelection(AList: TList); override;
    function UniqueName(const BaseName: string): string; override;
  end;

  TdxLayoutColorProperty = class(TColorProperty)
  public
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
    procedure ListDrawValue(const Value: string; ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
    procedure PropDrawValue(ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
  end;

  TdxLayoutRegistryPathProperty = class(TStringProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

  TdxLayoutLookAndFeelListEditor = class(TdxLayoutCustomControlEditor)
  private
    function GetLookAndFeelList: TdxLayoutLookAndFeelList;
  protected
    function InternalGetVerb(AIndex: Integer): string; override;
    function InternalGetVerbCount: Integer; override;
    procedure InternalExecuteVerb(AIndex: Integer); override;

    property LookAndFeelList: TdxLayoutLookAndFeelList read GetLookAndFeelList;
  end;

  { TdxLayoutLookAndFeelProperty }

  TdxLayoutLookAndFeelProperty = class(TComponentProperty)
  private
    FLookAndFeelLists: TComponentList;
    function GetLookAndFeelLists: TComponentList;
    procedure GetLookAndFeelListNameProc(const S: string);
  public
    function AutoFill: Boolean; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

  { TdxLayoutSelectionEditor }

  TdxLayoutSelectionEditor = class(TSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

  { TdxLayoutItemSelectionEditor }

  TdxLayoutItemSelectionEditor = class(TSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

  { TdxLayoutItemImageIndexProperty }

  TdxLayoutItemImageIndexProperty = class(TImageIndexProperty)
  public
    function GetImages: TCustomImageList; override;
  end;

const
  sCreateNewLookAndFeelInListBegin = '<Create a new LookAndFeel in ';
  sCreateNewLookAndFeelInListEnd = '>';
  sCreateNewLookAndFeelInNewList = '<Create a new LookAndFeel in the new list>';

{ TdxLayoutCustomControlEditor }

function TdxLayoutCustomControlEditor.GetProductName: string;
begin
  Result := dxLayoutControlProductName;
end;

{ TdxLayoutControlEditor }

procedure TdxLayoutControlEditor.PrepareItem(Index: Integer; const AItem: IMenuItem);
begin
  inherited;
  case Index of
    1, 3:
      AItem.Enabled := not IsInInlined;
    2:
      AItem.Enabled := TdxLayoutControlAccess(Control).CanAutoWidth or TdxLayoutControlAccess(Control).CanAutoHeight;
  end;
end;

procedure CreateItemsFromDataSet(ALayoutControl: TdxCustomLayoutControl; ADataSet: TDataSet);
var
  I: Integer;
  AControl: TControl;
  ADesigner: IDesigner;
  AForm: TCustomForm;
begin
  Screen.Cursor := crHourGlass;
  try
    ALayoutControl.BeginUpdate;
    try
      ADesigner := GetObjectDesigner(ALayoutControl);
      ADesigner.Activate;
      AForm := GetParentForm(ALayoutControl);
      for I := 0 to ADataSet.FieldList.Count - 1 do
      begin
        AControl := cxCreateFieldControl(ADesigner, ADataSet.FieldList[I], AForm, 0, 0);
        ALayoutControl.CreateItemForControl(AControl, ALayoutControl.Items);
        TdxLayoutControlAccess(ALayoutControl).DoCreateFieldControl(AControl, ADataSet.FieldList[I]);
      end;
    finally
      ALayoutControl.EndUpdate;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TdxLayoutControlEditor.DoLinkTo(AObject: TObject);
var
  ADataSet: TDataSet;
begin
  ADataSet := (AObject as TDataSource).DataSet;
  if ADataSet = nil then
    Exit;
  CreateItemsFromDataSet(Control, ADataSet);
end;

function TdxLayoutControlEditor.GetLinkToItemCaption: string;
begin
  Result := 'Create Items From';
end;

function TdxLayoutControlEditor.GetLinkToTypeClass: TClass;
begin
  Result := TDataSource;
end;

function TdxLayoutControlEditor.IsLinkable: Boolean;
begin
  Result := True;
end;

procedure TdxLayoutControlEditor.InternalExecuteVerb(AIndex: Integer);

  procedure DoCustomize;
  begin
    if Control.Customization then
      Control.CustomizeForm.BringToFront
    else
      Control.Customization := True;
  end;

  procedure DoReset;
  var
    I: Integer;
    AIntf: IcxLookAndFeelContainer;
  begin
    for I := 0 to Control.ControlCount - 1 do
      if Supports(Control.Controls[I], IcxLookAndFeelContainer, AIntf) then
        AIntf.GetLookAndFeel.AssignedValues := [];
    TdxLayoutControlAccess(Control).Modified;
  end;

begin
  case AIndex of
    0: DoCustomize;
    1: DoImport;
    2: Control.ApplyBestFit;
    3: DoReset;
  end;
end;

function TdxLayoutControlEditor.InternalGetVerb(AIndex: Integer): string;
begin
  case AIndex of
    0: Result := 'Designer...';
    1: Result := 'Import...';
    2: Result := 'Apply Best Fit';
    3: Result := 'Reset Look and Feel Settings';
  end;
end;

function TdxLayoutControlEditor.InternalGetVerbCount: Integer;
begin
  Result := 4;
end;

procedure TdxLayoutControlEditor.DoImport;
begin
  ImportLayout(Control);
end;

function TdxLayoutControlEditor.GetControl: TdxLayoutControl;
begin
  Result := Component as TdxLayoutControl;
end;

{ TdxLayoutDesignTimeSelectionHelper }

constructor TdxLayoutDesignTimeSelectionHelper.Create(AOwner: TPersistent);
var
  AContainer: TdxLayoutControlContainer;
begin
  inherited Create(AOwner);
  AContainer := AOwner as TdxLayoutControlContainer;
  FLayoutControl := AContainer.Control;
  FOldSelection := TcxComponentList.Create(False);
  FDesignHelper := TcxDesignHelper.Create(Component);
end;

destructor TdxLayoutDesignTimeSelectionHelper.Destroy;
begin
  FDesignHelper := nil;
  FreeAndNil(FOldSelection);
  inherited Destroy;
end;

Function TdxLayoutDesignTimeSelectionHelper.IsActive: Boolean;
begin
  Result := FIsActive;
end;

procedure TdxLayoutDesignTimeSelectionHelper.AddSelectionChangedListener(AListener: TPersistent);
begin
  inherited;
  if Listeners.Count > 0 then
    FDesignHelper.AddSelectionChangedListener(Self);
end;

procedure TdxLayoutDesignTimeSelectionHelper.RemoveSelectionChangedListener(AListener: TPersistent);
begin
  inherited;
  if Listeners.Count = 0 then
    FDesignHelper.RemoveSelectionChangedListener(Self);
end;

function TdxLayoutDesignTimeSelectionHelper.CanModify: Boolean;
begin
  Result := not FDesignHelper.Designer.IsSourceReadOnly;
end;

function TdxLayoutDesignTimeSelectionHelper.CanDeleteComponent(AComponent: TComponent): Boolean;
begin
  Result := FDesignHelper.CanDeleteComponent(Component, AComponent);
end;

procedure TdxLayoutDesignTimeSelectionHelper.ClearSelection;
begin
  FDesignHelper.SelectObject(Component, Component);
end;

procedure TdxLayoutDesignTimeSelectionHelper.DeleteComponents(AList: TComponentList);
begin
  inherited;
  TdxLayoutControlAccess(FLayoutControl).Modified;
end;

procedure TdxLayoutDesignTimeSelectionHelper.GetSelection(AList: TList);
begin
  FDesignHelper.GetSelection(AList);
end;

function TdxLayoutDesignTimeSelectionHelper.IsComponentSelected(AComponent: TPersistent): Boolean;
begin
  Result := FDesignHelper.IsObjectSelected(AComponent);
end;

procedure TdxLayoutDesignTimeSelectionHelper.SelectComponent(AComponent: TPersistent; AShift: TShiftState = []);
begin
  if (ssShift in AShift) and IsComponentSelected(AComponent) then
    FDesignHelper.UnselectObject(AComponent)
  else
    FDesignHelper.SelectObject(AComponent, not (ssShift in AShift));
end;

procedure TdxLayoutDesignTimeSelectionHelper.SetSelection(AList: TList);
begin
  FDesignHelper.SetSelection(AList);
end;

function TdxLayoutDesignTimeSelectionHelper.UniqueName(const BaseName: string): string;
begin
  Result := FDesignHelper.UniqueName(BaseName)
end;

//IcxDesignSelectionChanged
procedure TdxLayoutDesignTimeSelectionHelper.DesignSelectionChanged(ASelection: TList);

  procedure GetSelectionDifference(AList: TList);
  begin
    AList.Assign(FOldSelection, laXor, ASelection);
  end;

  procedure UpdateItemControls(AList: TcxObjectList);
  var
    I: Integer;
  begin
    for I := 0 to AList.Count - 1 do
      if (AList[I] is TControl) and ((AList[I] as TControl).Parent = FLayoutControl) then
        (AList[I] as TControl).Invalidate;
  end;

  function IsChild(AObject: TObject): Boolean;
  var
    AIntf: IdxLayoutSelectableItem;
  begin
    Result := Supports(AObject, IdxLayoutSelectableItem, AIntf) and AIntf.IsOwner(FLayoutControl.Container);
  end;

var
  AList: TcxObjectList;
begin
  AList := TcxObjectList.Create(False);
  try
    GetSelectionDifference(AList);
    FIsActive := ((AList.Count > 0) and IsChild(AList.Last)) or
      ((AList.Count = 0) and (ASelection.Count > 0) and IsChild(ASelection.Last));
    UpdateItemControls(AList);
    NotifyListeners(ASelection, saChanged);
  finally
    AList.Free;
    FOldSelection.Assign(ASelection);
  end;
end;

{ TdxLayoutLookAndFeelListEditor }

function TdxLayoutLookAndFeelListEditor.GetLookAndFeelList: TdxLayoutLookAndFeelList;
begin
  Result := Component as TdxLayoutLookAndFeelList;
end;

function TdxLayoutLookAndFeelListEditor.InternalGetVerb(AIndex: Integer): string;
begin
  Result := 'Designer...'
end;

function TdxLayoutLookAndFeelListEditor.InternalGetVerbCount: Integer;
begin
  Result := 1;
end;

procedure TdxLayoutLookAndFeelListEditor.InternalExecuteVerb(AIndex: Integer);
begin
  if dxLayoutLookAndFeelsDesigner = nil then
    ShowFormEditorClass(Designer, LookAndFeelList, TdxLayoutLookAndFeelListDesignForm)
  else
    dxLayoutLookAndFeelsDesigner.Edit(LookAndFeelList);
end;

{ TdxLayoutColorProperty }

const
  DefaultColorText = 'clDefault';

function TdxLayoutColorProperty.GetValue: string;
begin
  if GetOrdValue = clDefault then
    Result := DefaultColorText
  else
    Result := inherited GetValue;
end;

procedure TdxLayoutColorProperty.GetValues(Proc: TGetStrProc);
begin
  Proc(DefaultColorText);
  inherited;
end;

procedure TdxLayoutColorProperty.SetValue(const Value: string);
begin
  if SameText(Value, DefaultColorText) then
    SetOrdValue(clDefault)
  else
    inherited;
end;

procedure TdxLayoutColorProperty.ListDrawValue(const Value: string;
  ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
begin
  if Value = DefaultColorText then
    with ARect do
      ACanvas.TextRect(ARect, Left + (Bottom - Top) + 1, Top + 1, Value)
  else
    inherited;
end;

procedure TdxLayoutColorProperty.PropDrawValue(ACanvas: TCanvas;
  const ARect: TRect; ASelected: Boolean);
begin
  if GetVisualValue = DefaultColorText then
    with ARect do
      ACanvas.TextRect(ARect, Left + 1, Top + 1, GetVisualValue)
  else
    inherited;
end;

{ TdxLayoutRegistryPathProperty }

procedure TdxLayoutRegistryPathProperty.Edit;
var
  AOptions: TdxStoringOptions;
  S: string;
begin
  AOptions := TdxStoringOptions(GetComponent(0));
  S := AOptions.RegistryPath;
  if dxGetRegistryPath(S) then
  begin
    AOptions.RegistryPath := S;
    Designer.Modified;
  end;
end;

function TdxLayoutRegistryPathProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog];
end;

{ TdxLayoutLookAndFeelProperty }

function TdxLayoutLookAndFeelProperty.AutoFill: Boolean;
begin
  Result := False;
end;

function TdxLayoutLookAndFeelProperty.GetLookAndFeelLists: TComponentList;
begin
  Result := TComponentList.Create(False);
  FLookAndFeelLists := Result;
  Designer.GetComponentNames(GetTypeData(TdxLayoutLookAndFeelList.ClassInfo),
    GetLookAndFeelListNameProc);
end;

procedure TdxLayoutLookAndFeelProperty.GetLookAndFeelListNameProc(const S: string);
begin
  FLookAndFeelLists.Add(Designer.GetComponent(S));
end;

procedure TdxLayoutLookAndFeelProperty.GetValues(Proc: TGetStrProc);
var
  ALookAndFeelLists: TList;
  I: Integer;
begin
  if not Designer.IsSourceReadOnly then
  begin
    ALookAndFeelLists := GetLookAndFeelLists;
    try
      for I := 0 to ALookAndFeelLists.Count - 1 do
      begin
        Proc(sCreateNewLookAndFeelInListBegin +
          Designer.GetComponentName(ALookAndFeelLists.Items[I]) +
          sCreateNewLookAndFeelInListEnd);
      end;
    finally
      ALookAndFeelLists.Free;
    end;
    Proc(sCreateNewLookAndFeelInNewList);
  end;
  inherited GetValues(Proc);
end;

procedure TdxLayoutLookAndFeelProperty.SetValue(const Value: string);

  procedure CreateAndAssignNewLookAndFeel(ALookAndFeelList: TdxLayoutLookAndFeelList);
  var
    ALookAndFeel: TdxCustomLayoutLookAndFeel;
    ALookAndFeelClass: TdxCustomLayoutLookAndFeelClass;
  begin
    if dxLayoutControlSelectLookAndFeel(ALookAndFeelClass) then
    begin
      ALookAndFeel := ALookAndFeelList.CreateItem(ALookAndFeelClass);
      FindRootDesigner(ALookAndFeel).Modified;
      SetOrdValue(Integer(ALookAndFeel));
    end;
  end;

var
  ALookAndFeelList: TdxLayoutLookAndFeelList;
  AName: string;
begin
  if Value = sCreateNewLookAndFeelInNewList then
  begin
    ALookAndFeelList := TdxLayoutLookAndFeelList.Create(Designer.Root);
    ALookAndFeelList.Name := Designer.UniqueName(
      Copy(ALookAndFeelList.ClassName, 2, Length(ALookAndFeelList.ClassName)));
    CreateAndAssignNewLookAndFeel(ALookAndFeelList);
  end
  else
    if Copy(Value, 1, Length(sCreateNewLookAndFeelInListBegin)) = sCreateNewLookAndFeelInListBegin then
    begin
      AName := Copy(Value, Length(sCreateNewLookAndFeelInListBegin) + 1,
        Length(Value) - Length(sCreateNewLookAndFeelInListBegin) -
        Length(sCreateNewLookAndFeelInListEnd));
      CreateAndAssignNewLookAndFeel(Designer.GetComponent(AName) as TdxLayoutLookAndFeelList);
    end
    else
      inherited SetValue(Value);
end;

{ TdxLayoutSelectionEditor }

procedure TdxLayoutSelectionEditor.RequiresUnits(Proc: TGetStrProc);
begin
  dxSkinsRequiresAdditionalUnits(TcxPageControl, Proc);
end;

{ TdxLayoutItemSelectionEditor }

procedure TdxLayoutItemSelectionEditor.RequiresUnits(Proc: TGetStrProc);
var
  I: Integer;
  AComponent: TComponent;
  AItem: TdxLayoutItemAccess;
begin
  for I := 0 to Designer.Root.ComponentCount - 1 do
  begin
    AComponent := Designer.Root.Components[I];
    if AComponent is TdxLayoutItem then
    begin
      AItem := TdxLayoutItemAccess(AComponent);
      if AItem.ControlAdapter <> nil then
        Proc(cxGetUnitName(AItem.ControlAdapter.ClassType));
    end;
  end;
end;

{ TdxLayoutItemImageIndexProperty }

function TdxLayoutItemImageIndexProperty.GetImages: TCustomImageList;
var
  AOptions: TdxCustomLayoutItemCaptionOptionsAccess;
begin
  if GetComponent(0) is TdxCustomLayoutItemCaptionOptions then
  begin
    AOptions := TdxCustomLayoutItemCaptionOptionsAccess(GetComponent(0));
    Result := TdxLayoutContainerAccess(AOptions.Item.Container).GetImages;
  end
  else
    Result := nil;
end;

procedure Register;
begin
  ForceDemandLoadState(dlDisable);

  RegisterComponentEditor(TdxLayoutControl, TdxLayoutControlEditor);
  RegisterComponentEditor(TdxCustomLayoutItem, TdxLayoutControlItemsEditor);
  RegisterComponentEditor(TdxLayoutLookAndFeelList, TdxLayoutLookAndFeelListEditor);

  RegisterPropertyEditor(TypeInfo(TColor), TdxCustomLayoutLookAndFeelOptions, '', TdxLayoutColorProperty);
  RegisterPropertyEditor(TypeInfo(TColor), TdxLayoutLookAndFeelCaptionOptions, '', TdxLayoutColorProperty);
  RegisterPropertyEditor(TypeInfo(TdxCustomLayoutLookAndFeel), nil, '', TdxLayoutLookAndFeelProperty);
  RegisterPropertyEditor(TypeInfo(string), TdxStoringOptions, 'RegistryPath', TdxLayoutRegistryPathProperty);
  RegisterPropertyEditor(TypeInfo(TBitmap), TdxCustomLayoutItemCaptionOptions, 'Glyph', TcxBitmapProperty);
  RegisterPropertyEditor(TypeInfo(TBitmap), TdxLayoutGroupButton, 'Glyph', TcxBitmapProperty);
  RegisterPropertyEditor(TypeInfo(Integer), TdxCustomLayoutItemCaptionOptions, 'ImageIndex', TdxLayoutItemImageIndexProperty);

  RegisterSelectionEditor(TdxLayoutItem, TdxLayoutItemSelectionEditor);
  RegisterSelectionEditor(TdxCustomLayoutControl, TdxLayoutSelectionEditor);

  HideClassProperties(TdxLayoutControl, ['AutoContentSizes', 'AutoControlAlignment', 'AutoControlTabOrders',
    'LookAndFeel', 'DragDropMode', 'CustomizeFormTabbedView', 'HighlightRoot', 'ShowDesignSelectors',
    'IniFileName', 'RegistryPath', 'StoreInIniFile', 'StoreInRegistry', 'TabStop']);
  HideClassProperties(TdxCustomLayoutItem, ['AutoAligns', 'LookAndFeel', 'Caption', 'ShowCaption', 'Parent']);
  HideClassProperties(TdxLayoutGroup, ['LookAndFeelException', 'AllowWrapItems']);
  HideClassProperties(TdxLayoutItemControlOptions, ['FixedSize', 'AutoAlignment']);

  RegisterNoIcon([
    TdxLayoutItem, TdxLayoutEmptySpaceItem, TdxLayoutLabeledItem, TdxLayoutImageItem, TdxLayoutSeparatorItem, TdxLayoutSplitterItem,
    TdxLayoutGroup, TdxLayoutAutoCreatedGroup, TdxLayoutAlignmentConstraint,
    TdxLayoutStandardLookAndFeel, TdxLayoutOfficeLookAndFeel, TdxLayoutWebLookAndFeel]);
  RegisterComponents(dxLibraryProductPage, [TdxLayoutControl, TdxLayoutLookAndFeelList]);
end;

type

  { TdxLayoutDesignTimeHelper }

  TdxLayoutDesignTimeHelper = class(TInterfacedObject, IdxLayoutDesignTimeHelper)
  protected
    //IdxLayoutDesignTimeHelper
    function IsToolSelected: Boolean;
    function GetFieldDisplayName(AField: TObject): string;
  end;

{ TdxLayoutDesignTimeHelper }

function TdxLayoutDesignTimeHelper.IsToolSelected: Boolean;
begin
  Result := (ActiveDesigner <> nil) and ActiveDesigner.Environment.GetToolSelected;
end;

function TdxLayoutDesignTimeHelper.GetFieldDisplayName(AField: TObject): string;
begin
  if AField is TField then
    Result := TField(AField).DisplayName
  else
    Result := '';
end;

initialization
  dxLayoutDesignTimeSelectionHelperClass := TdxLayoutDesignTimeSelectionHelper;
  dxLayoutDesignTimeHelper := TdxLayoutDesignTimeHelper.Create;

finalization
  dxLayoutDesignTimeHelper := nil;

end.
