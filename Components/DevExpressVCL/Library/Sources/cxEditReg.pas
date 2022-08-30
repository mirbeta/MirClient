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

unit cxEditReg;

{$I cxVer.inc}

interface

uses
  Classes, Controls, DB, DesignIntf, DSDesign, ColnEdit, cxEdit;

type
  { TcxFieldsTarget }

  TcxFieldsTarget = class(TDragTarget)
  protected
    procedure CreateControls(AFieldList: TList; AEditor: TFieldsEditor; ATarget: TObject; X, Y: Integer);
    procedure DoCreateFieldControl(AParent, AControl: TControl; AField: TField); virtual;
    function GetAllowTarget(ATarget: TControl): TControl;
    function GetControlClass(AEditor: TFieldsEditor; AField: TField): string; virtual;
    function NeedCreateCaption(ATarget: TControl): Boolean;
  public
    procedure DragDrop(Target, Source: TObject; X, Y: Integer); override;
    function DragOver(Target, Source: TObject; X, Y: Integer; State: TDragState): Boolean; override;
  end;

  TcxGetControlClassNameProc = procedure (AField: TField; out AClassName: string);

  TcxFieldsTargetClass = class of TcxFieldsTarget;

var
  cxGetControlClassNameProc: TcxGetControlClassNameProc;

procedure cxRegisterDragTarget(ATargetClass: TcxFieldsTargetClass);
procedure cxUnregisterDragTarget(ATargetClass: TcxFieldsTargetClass);

function cxCreateFieldControl(ADesigner: IDesigner; AField: TField; AParent: TWinControl; X, Y: Integer): TControl;

procedure Register;

implementation

uses
  DesignEditors, VCLEditors, Windows, Graphics, ImgList, TypInfo,
  DsnDBCst, DrpCtrls, SysUtils, Dialogs, Forms,
  dxCore, cxControls, cxBlobEdit, cxButtonEdit, cxButtons, cxCalc, cxCalendar, cxCheckBox,
  cxContainer, cxCurrencyEdit, cxDB, cxDBEdit, cxDBEditRepository, cxDBLookupComboBox,
  cxDBNavigator, cxDropDownEdit, cxEditConsts, cxEditPropEditors, cxEditRepositoryEditor,
  cxEditRepositoryItems, cxGraphics, cxGroupBox, cxHyperLinkEdit, cxImage,
  cxImageComboBox, cxLibraryReg, cxListBox, cxLookAndFeels, cxLookupDBGrid,
  cxLookupGrid, cxMaskEdit, cxMemo, cxMRUEdit, cxNavigator, cxPropEditors,
  cxRadioGroup, cxSpinEdit, cxTextEdit, cxTimeEdit, cxScrollBox, dxBevel,
  dxBreadcrumbEdit, dxDBBreadcrumbEdit, dxAlertWindow, dxGalleryControl, dxCameraControl,
  dxColorGallery, dxDBColorGallery, dxImageSlider, dxCheckGroupBox, dxDBCheckGroupBox,
  cxCustomData, cxDataStorage, dxColorPicker, dxCoreGraphics, dxColorDialog, dxToggleSwitch, dxDBToggleSwitch,
  dxActivityIndicator, dxWheelPicker, dxDateTimeWheelPicker, dxDBDateTimeWheelPicker, dxNumericWheelPicker,
  dxDBNumericWheelPicker, dxSparkline, dxDBSparkline, dxColorEdit, dxDBColorEdit,
  dxBarCodeUtils, dxBarCode, dxDBBarCode, dxTokenEdit, dxDBTokenEdit, dxGDIPlusClasses;

type
  TcxCustomEditAccess = class(TcxCustomEdit);

  { TcxLookupComboBoxPropertiesFieldNameProperty }

  TcxLookupComboBoxPropertiesFieldNameProperty = class(TFieldNameProperty)
    function GetDataSourcePropName: string; override;
  end;

  { TcxLookupDBGridColumnFieldNameProperty }

  TcxLookupDBGridColumnFieldNameProperty = class(TFieldNameProperty)
  public
    function GetDataSource: TDataSource; override;
  end;

  { TdxLookupSparkLineFieldNameProperty }

  TdxLookupSparkLineFieldNameProperty = class(TFieldNameProperty)
  public
    function GetDataSource: TDataSource; override;
  end;

  { TdxLookupSparklineItemFieldNameProperty }

  TdxLookupSparklineItemFieldNameProperty = class(TFieldNameProperty)
  public
    function GetDataSource: TDataSource; override;
  end;

  { TdxSparklineValueTypeProperty }

  TdxSparklineValueTypeProperty = class(TcxValueTypeProperty)
  protected
    function IsValueTypeClassValid(AValueTypeClass: TcxValueTypeClass): Boolean; override;
  end;

  { TdxSparklineCollectionProperty }

  TdxSparklineCollectionProperty = class(TCollectionProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
  end;

  { TdxColorEditSelectionEditor }

  TdxColorEditSelectionEditor = class(TSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

  { TcxDateEditSelectionEditor }

  TcxDateEditSelectionEditor = class(TSelectionEditor)
  protected
    ComponentsList: TStringList;
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

  TcxLookupDBGridColumnAccess = class(TcxLookupDBGridColumn);

  { TcxDragFields }

  TcxDragFields = class(TDragControlObject)
  private
    FEditor: TFieldsEditor;
  public
    constructor Create(AControl: TControl; AEditor: TFieldsEditor); reintroduce;
    property Editor: TFieldsEditor read FEditor;
  end;

  { TdxAlertWindowManagerSelectionEditor }

  TdxAlertWindowManagerSelectionEditor = class(TSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

function cxGetControlClassName(AField: TField): string;
begin
  Result := '';
  if Assigned(cxGetControlClassNameProc) then
    cxGetControlClassNameProc(AField, Result);
end;

var
  FRegisteredDragTargets: TList;

procedure InternalRegisterDragTarget(ATargetClass: TDragTargetClass);
begin
  RegisterDragTarget('TDragFields', ATargetClass);
end;

procedure cxRegisterDragTarget(ATargetClass: TcxFieldsTargetClass);
begin
  InternalRegisterDragTarget(ATargetClass);
  FRegisteredDragTargets.Add(ATargetClass);
end;

procedure cxUnregisterDragTarget(ATargetClass: TcxFieldsTargetClass);
begin
  FRegisteredDragTargets.Remove(ATargetClass);
  if FRegisteredDragTargets.Count > 0 then
    InternalRegisterDragTarget(FRegisteredDragTargets.Last);
end;

function InternalCreateFieldControl(ADesigner: IDesigner; AField: TField; AControlClassName: string;
  AParent: TControl; X, Y: Integer; ANeedCreateCaption: Boolean): TControl;
var
  ADataBinding: TcxDBEditDataBinding;
begin
  Result := CreateFieldControl(ADesigner, AField, AControlClassName, AParent, X, Y, ANeedCreateCaption);
  if Result is TcxCustomEdit then
  begin
    ADataBinding := TcxDBEditDataBinding(TcxCustomEditAccess(Result).DataBinding);
    ADataBinding.DataSource := GetDataSource(ADesigner, AField.DataSet);
    ADataBinding.DataField := AField.FieldName;
  end;
end;

function cxCreateFieldControl(ADesigner: IDesigner; AField: TField; AParent: TWinControl; X, Y: Integer): TControl;
begin
  Result := InternalCreateFieldControl(ADesigner, AField, cxGetControlClassName(AField), AParent, X, Y, False);
end;

{ TcxDragFields }

constructor TcxDragFields.Create(AControl: TControl; AEditor: TFieldsEditor);
begin
  inherited Create(AControl);
  FEditor := AEditor;
end;

{ TdxAlertWindowManagerSelectionEditor }

procedure TdxAlertWindowManagerSelectionEditor.RequiresUnits(Proc: TGetStrProc);
begin
  Proc('cxLookAndFeelPainters');
  Proc('cxGraphics');
  dxSkinsRequiresAdditionalUnits(nil, Proc);
end;

{ TcxLookupComboBoxPropertiesFieldNameProperty }

function TcxLookupComboBoxPropertiesFieldNameProperty.GetDataSourcePropName: string;
begin
  Result := 'ListSource';
end;

{ TcxLookupDBGridColumnFieldNameProperty }

function TcxLookupDBGridColumnFieldNameProperty.GetDataSource: TDataSource;
begin
  Result := TcxLookupDBGridColumnAccess(GetComponent(0) as TcxLookupDBGridColumn).DataController.DataSource;
end;

{ TdxLookupSparkLineFieldNameProperty }

function TdxLookupSparkLineFieldNameProperty.GetDataSource: TDataSource;
begin
  Result := TdxLookupSparklineProperties(GetComponent(0)).LookupDataSource;
end;

{ TdxLookupSparklineItemFieldNameProperty }

function TdxLookupSparklineItemFieldNameProperty.GetDataSource: TDataSource;
begin
  Result := TdxLookupSparklineItemDataBinding(GetComponent(0)).DataSource;
end;

{ TdxSparklineValueTypeProperty }

function TdxSparklineValueTypeProperty.IsValueTypeClassValid(AValueTypeClass: TcxValueTypeClass): Boolean;
var
  AValueType: TVarType;
const
  ValidIntegerValueTypes = [varSmallint, varInteger, varByte, varShortInt,
    varWord, varLongWord, varInt64];
  ValidFloatValueTypes = [varSingle, varDouble, varCurrency];
begin
  if GetComponent(0) is TdxSparklineSeries then
  begin
    AValueType := AValueTypeClass.GetVarType;
    Result := (AValueType in ValidIntegerValueTypes) and (AValueType in ValidFloatValueTypes);
  end
  else
    Result := inherited IsValueTypeClassValid(AValueTypeClass);
end;

{ TdxSparklineCollectionProperty }

function TdxSparklineCollectionProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paSubProperties];
end;

{ TdxColorEditSelectionEditor }

procedure TdxColorEditSelectionEditor.RequiresUnits(Proc: TGetStrProc);
begin
  Proc('dxCore');
end;

{ TcxDateEditSelectionEditor }

procedure TcxDateEditSelectionEditor.RequiresUnits(Proc: TGetStrProc);
begin
{$IFDEF DELPHI16}
  Proc('Vcl.ComCtrls');
{$ELSE}
  Proc('ComCtrls');
{$ENDIF}
  Proc('dxCore');
  Proc('cxDateUtils');
end;

{ TcxFieldsTarget }

procedure TcxFieldsTarget.DragDrop(Target, Source: TObject; X, Y: Integer);
var
  ASourceRoot: TComponent;
  I: Integer;
  AEditor: TFieldsEditor;
  AFieldList: TList;
begin
  ASourceRoot := TcxDragFields(Source).Editor.Designer.GetRoot;
  if not Designer.IsComponentLinkable(ASourceRoot) then
    if MessageDlg(Format(SDSLinkForms, [Designer.GetRoot.Name,
      ASourceRoot.Name]), mtConfirmation, mbYesNoCancel, 0) <> idYes then
        Exit
    else
      Designer.MakeComponentLinkable(ASourceRoot);
  AFieldList := TList.Create;
  try
    AEditor := TcxDragFields(Source).Editor;
    with AEditor do
    begin
      for I := 0 to FieldListBox.Items.Count - 1 do
        if FieldListBox.Selected[I] then
          AFieldList.Add(FieldListBox.Items.Objects[I]);
    end;
    if AFieldList.Count > 0 then
    begin
      Screen.Cursor := crHourGlass;
      try
        CreateControls(AFieldList, AEditor, Target, X, Y);
      finally
        Screen.Cursor := crDefault;
      end;
    end;
  finally
    AFieldList.Free;
  end;
end;

procedure TcxFieldsTarget.CreateControls(AFieldList: TList; AEditor: TFieldsEditor; ATarget: TObject; X, Y: Integer);
var
  I: Integer;
  ADataSource: TDataSource;
  AField: TField;
  AControl: TControl;
  AParent: TControl;
  AIntf: IcxEditorFieldLink;
  ANeedCreateCaption: Boolean;
begin
  AField := TField(AFieldList[0]);
  ADataSource := GetDataSource(Designer, AField.DataSet);
  AParent := GetAllowTarget(ATarget as TControl);
  if not Supports(AParent, IcxEditorFieldLink, AIntf) or not AIntf.CreateFieldControls(X, Y, ADataSource, AFieldList) then
  begin
    ANeedCreateCaption := NeedCreateCaption(AParent);
    for I := 0 to AFieldList.Count - 1 do
    begin
      AField := TField(AFieldList[I]);
      AControl := InternalCreateFieldControl(Designer, AField, GetControlClass(AEditor, AField),
        AParent, X, Y, ANeedCreateCaption);
      DoCreateFieldControl(AParent, AControl, AField);
      Y := AControl.Top + AControl.Height + 5;
    end;
  end;
end;

procedure TcxFieldsTarget.DoCreateFieldControl(AParent, AControl: TControl; AField: TField);
var
  AIntf: IcxEditorFieldLink2;
begin
  if Supports(AParent, IcxEditorFieldLink2, AIntf) then
    AIntf.DoCreateFieldControl(AControl, AField);
end;

function TcxFieldsTarget.GetAllowTarget(ATarget: TControl): TControl;

  function IsAllow(ACandidate: TControl): Boolean;
  begin
    Result := (csAcceptsControls in ACandidate.ControlStyle) or
      Supports(ACandidate, IcxEditorFieldLink);
  end;

begin
  Result := ATarget;
  while (Result <> nil) and not IsAllow(Result) do
    Result := Result.Parent;
end;

function TcxFieldsTarget.GetControlClass(AEditor: TFieldsEditor; AField: TField): string;
begin
  Result := cxGetControlClassName(AField);
  if Result = '' then
    Result := AEditor.DSDesigner.GetControlClass(AField);
end;

function TcxFieldsTarget.NeedCreateCaption(ATarget: TControl): Boolean;
var
  AIntf: IcxEditorFieldLink2;
begin
  Result := not Supports(ATarget, IcxEditorFieldLink2, AIntf) or AIntf.NeedCreateCaption;
end;

function TcxFieldsTarget.DragOver(Target, Source: TObject; X, Y: Integer;
  State: TDragState): Boolean;
begin
  Result := True;
end;

procedure RegisterEditRepositoryItems;
begin
  RegisterEditRepositoryItem(TcxEditRepositoryTextItem, scxSEditRepositoryTextItem);
  RegisterEditRepositoryItem(TcxEditRepositoryButtonItem, scxSEditRepositoryButtonItem);
  RegisterEditRepositoryItem(TcxEditRepositoryImageItem, scxSEditRepositoryImageItem);
  RegisterEditRepositoryItem(TcxEditRepositoryComboBoxItem, scxSEditRepositoryComboBoxItem);
  RegisterEditRepositoryItem(TcxEditRepositoryMaskItem, scxSEditRepositoryMaskItem);
  RegisterEditRepositoryItem(TcxEditRepositoryPopupItem, scxSEditRepositoryPopupItem);
  RegisterEditRepositoryItem(TcxEditRepositoryCalcItem, scxSEditRepositoryCalcItem);
  RegisterEditRepositoryItem(TcxEditRepositoryDateItem, scxSEditRepositoryDateItem);
  RegisterEditRepositoryItem(TcxEditRepositoryCurrencyItem, scxSEditRepositoryCurrencyItem);
  RegisterEditRepositoryItem(TcxEditRepositorySpinItem, scxSEditRepositorySpinItem);
  RegisterEditRepositoryItem(TcxEditRepositoryMemoItem, scxSEditRepositoryMemoItem);
  RegisterEditRepositoryItem(TcxEditRepositoryImageComboBoxItem, scxSEditRepositoryImageComboBoxItem);
  RegisterEditRepositoryItem(TcxEditRepositoryBlobItem, scxSEditRepositoryBlobItem);
  RegisterEditRepositoryItem(TcxEditRepositoryCheckBoxItem, scxSEditRepositoryCheckBoxItem);
  RegisterEditRepositoryItem(TcxEditRepositoryTimeItem, scxSEditRepositoryTimeItem);
  RegisterEditRepositoryItem(TcxEditRepositoryMRUItem, scxSEditRepositoryMRUItem);
  RegisterEditRepositoryItem(TcxEditRepositoryHyperLinkItem, scxSEditRepositoryHyperLinkItem);
  RegisterEditRepositoryItem(TcxEditRepositoryLookupComboBoxItem, scxSEditRepositoryLookupComboBoxItem);
  RegisterEditRepositoryItem(TcxEditRepositoryRadioGroupItem, scxSEditRepositoryRadioGroupItem);
  RegisterEditRepositoryItem(TcxEditRepositoryToggleSwitchItem, scxSEditRepositoryToggleSwitchItem);
  RegisterEditRepositoryItem(TcxEditRepositoryDateTimeWheelPickerItem, scxSEditRepositoryDateTimeWheelPickerItem);
  RegisterEditRepositoryItem(TcxEditRepositoryNumericWheelPickerItem, scxSEditRepositoryNumericWheelPickerItem);
  RegisterEditRepositoryItem(TcxEditRepositorySparklineItem, scxSEditRepositorySparklineItem);
  RegisterEditRepositoryItem(TcxEditRepositoryLookupSparklineItem, scxSEditRepositoryLookupSparklineItem);
  RegisterEditRepositoryItem(TcxEditRepositoryBarCodeItem, scxSEditRepositoryBarCodeItem);
  RegisterEditRepositoryItem(TcxEditRepositoryTokenItem, scxSEditRepositoryTokenItem);
end;

procedure Register;
begin
  ForceDemandLoadState(dlDisable);

  RegisterComponents(cxEditorsLibraryProductPage, [TcxTextEdit, TcxMaskEdit, TcxMemo,
    TcxDateEdit, TcxButtonEdit, TcxCheckBox, TcxComboBox, TcxImageComboBox,
    TcxSpinEdit, TcxCalcEdit, TcxHyperLinkEdit, TcxTimeEdit, TcxCurrencyEdit,
    TcxImage, TcxBlobEdit, TcxMRUEdit, TcxPopupEdit, TcxLookupComboBox,
    TcxRadioButton, TcxRadioGroup, TcxListBox, TcxNavigator, TdxBreadcrumbEdit,
    TdxColorGallery, TdxCheckGroupBox, TdxColorPicker, TdxToggleSwitch, TdxWheelPicker, TdxDateTimeWheelPicker,
    TdxSparklineEdit, TdxLookupSparklineEdit, TdxNumericWheelPicker, TdxBarCode, TdxTokenEdit]);

  RegisterComponents(cxEditorsDBLibraryProductPage, [TcxDBTextEdit, TcxDBMaskEdit, TcxDBMemo,
    TcxDBDateEdit, TcxDBButtonEdit, TcxDBCheckBox, TcxDBComboBox, TcxDBImageComboBox,
    TcxDBSpinEdit, TcxDBCalcEdit, TcxDBHyperLinkEdit, TcxDBTimeEdit, TcxDBCurrencyEdit,
    TcxDBImage, TcxDBBlobEdit, TcxDBMRUEdit, TcxDBPopupEdit, TcxDBLookupComboBox,
    TcxDBRadioGroup, TcxDBListBox, TcxDBNavigator, TdxDBBreadcrumbEdit,
    TdxDBColorGallery, TdxDBCheckGroupBox, TdxDBToggleSwitch, TdxDBDateTimeWheelPicker,
    TdxDBSparklineEdit, TdxDBLookupSparklineEdit, TdxDBNumericWheelPicker, TdxDBBarCode, TdxDBTokenEdit]);

  RegisterComponents(cxEditorsUtilitiesProductPage, [TcxButton, TdxBevel, TcxGroupBox,
    TcxEditStyleController, TcxDefaultEditStyleController, TcxEditRepository, TdxColorDialog,
    TcxClock, TcxScrollBox, TdxAlertWindowManager, TdxGalleryControl, TdxCameraControl, TdxImageSlider]);

  RegisterClasses([TdxGalleryControlItem, TdxGalleryControlGroup]);
  RegisterNoIcon([TdxGalleryControlItem, TdxGalleryControlGroup]);

  RegisterComponentEditor(TdxActivityIndicator, TcxCustomEditorsLibraryComponentEditor);
  RegisterComponents(cxEditorsLibraryProductPage, [TdxActivityIndicator]);
  RegisterPropertyEditor(TypeInfo(TdxActivityIndicatorProperties), TdxActivityIndicator, 'Properties', TdxActivityIndicatorPropertiesEditor);
  HideClassProperties(TdxActivityIndicator, ['PropertiesClassName']);
  RegisterComponentEditor(TcxEditRepository, TcxEditRepositoryComponentEditor);
  RegisterPropertyEditor(TypeInfo(TNotifyEvent), TcxEditRepositoryItem, 'PropertiesEvents', TcxEditRepositoryItemPropertiesEventEditor);
  RegisterClasses([TcxEditRepositoryItem, TcxEditRepositoryTextItem, TcxEditRepositoryButtonItem]);
  RegisterNoIcon([TcxEditRepositoryItem, TcxEditRepositoryTextItem, TcxEditRepositoryButtonItem]);
  RegisterEditRepositoryItems;

  RegisterComponentEditor(TcxCustomEdit, TcxEditComponentEditor);
  RegisterComponentEditor(TcxEditStyleController, TcxEditStyleControllerEditor);
  RegisterComponentEditor(TcxDefaultEditStyleController, TcxDefaultEditStyleControllerEditor);
  RegisterComponentEditor(TcxCustomButton, TcxCustomButtonComponentEditor);
  RegisterComponentEditor(TdxCustomBevel, TdxBevelComponentEditor);
  RegisterComponentEditor(TcxCustomScrollBox, TcxEditorsLibraryCXControlComponentEditor);
  RegisterComponentEditor(TcxClock, TcxEditorsLibraryCXControlComponentEditor);
  RegisterComponentEditor(TdxImageSlider, TdxSliderImageComponentEditor);
  RegisterComponentEditor(TdxAlertWindowManager, TdxAlertWindowComponentEditor);
  RegisterComponentEditor(TdxCustomBreadcrumbEdit, TcxEditorsLibraryCXControlComponentEditor);
  RegisterComponentEditor(TdxBreadcrumbEdit, TcxBreadcrumbEditComponentEditor);
  RegisterComponentEditor(TdxGalleryControl, TdxGalleryControlComponentEditor);
  RegisterComponentEditor(TdxCameraControl, TcxEditorsLibraryCXControlComponentEditor);
  RegisterComponentEditor(TdxCustomColorGallery, TcxCustomEditorsLibraryComponentEditor);
  RegisterComponentEditor(TdxColorPicker, TcxEditorsLibraryCXControlComponentEditor);
  RegisterComponentEditor(TdxColorDialog, TdxColorDialogComponentEditor);
  RegisterComponentEditor(TdxWheelPicker, TdxGalleryControlComponentEditor);
  RegisterComponentEditor(TcxRadioButton, TcxRadioButtonComponentEditor);
  RegisterComponentEditor(TcxCustomNavigator, TcxCustomNavigatorComponentEditor);
  RegisterComponentEditor(TcxContainer, TcxContainerComponentEditor);
  RegisterComponentEditor(TdxSparklineEdit, TdxSparklineEditComponentEditor);
  RegisterComponentEditor(TdxDBLookupSparklineEdit, TdxSparklineEditComponentEditor);
  RegisterComponentEditor(TdxDBSparklineEdit, TdxSparklineEditComponentEditor);
  RegisterComponentEditor(TdxDBTokenEdit, TdxTokenEditComponentEditor);
  RegisterComponentEditor(TdxLookupSparklineEdit, TdxSparklineEditComponentEditor);
  RegisterComponentEditor(TdxTokenEdit, TdxTokenEditComponentEditor);

  RegisterSelectionEditor(TcxControl, TcxControlSelectionEditor);
  RegisterSelectionEditor(TcxCustomEdit, TcxCustomEditSelectionEditor);
  RegisterSelectionEditor(TcxCustomButton, TcxButtonSelectionEditor);
  RegisterSelectionEditor(TdxAlertWindowManager, TdxAlertWindowManagerSelectionEditor);
  RegisterSelectionEditor(TcxDateEdit, TcxDateEditSelectionEditor);
  RegisterSelectionEditor(TdxColorEdit, TdxColorEditSelectionEditor);
  RegisterSelectionEditor(TdxColorGallery, TdxColorEditSelectionEditor);
  RegisterSelectionEditor(TdxDBColorEdit, TdxColorEditSelectionEditor);
  RegisterSelectionEditor(TdxDBColorGallery, TdxColorEditSelectionEditor);

  RegisterComponentGuidelines(TcxCustomEdit, TcxEditGuidelines);

{$IFDEF DELPHI16}
  RegisterPropertyEditor(TypeInfo(TDateTime), TcxCustomDateEdit, 'Date', TcxDateProperty);
  RegisterPropertyEditor(TypeInfo(TDateTime), TdxCustomDateTimeWheelPicker, 'DateTime', TcxDateProperty);
{$ENDIF}
  RegisterPropertyEditor(TypeInfo(TNotifyEvent), TcxCustomEdit, 'PropertiesEvents', TcxEditPropertiesEventEditor);
  RegisterPropertyEditor(TypeInfo(TcxEditRepositoryItem), TcxCustomEdit, 'RepositoryItem', TcxEditRepositoryItemProperty);
  RegisterPropertyEditor(TypeInfo(TcxCustomEditPropertiesValues), TcxCustomEditProperties, 'AssignedValues', TcxEditPropertiesAssignedValuesProperty);
  RegisterPropertyEditor(TypeInfo(TShortCut), TcxCustomEditProperties, 'ClickKey', TShortCutProperty);
  RegisterPropertyEditor(TypeInfo(TShortCut), TcxCustomEditProperties, 'ClearKey', TShortCutProperty);

  RegisterPropertyEditor(TypeInfo(string), TcxEditDataBinding, 'DataField', TcxDataFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TcxCustomDBDataBinding, 'DataField', TcxDataFieldProperty);

  RegisterPropertyEditor(TypeInfo(string), TdxDBBreadcrumbEditDataBinding, 'ImageIndexField', TcxDataFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TdxDBBreadcrumbEditDataBinding, 'KeyField', TcxDataFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TdxDBBreadcrumbEditDataBinding, 'NameField', TcxDataFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TdxDBBreadcrumbEditDataBinding, 'ParentKeyField', TcxDataFieldProperty);

  RegisterPropertyEditor(TypeInfo(TShortCut), TcxCustomHyperLinkEditProperties, 'StartKey', TShortCutProperty);
  RegisterPropertyEditor(TypeInfo(TBiDiMode), TcxHyperLinkEdit, 'BiDiMode', nil);

  RegisterPropertyEditor(TypeInfo(TcxImageIndex), TcxImageComboBoxProperties, 'DefaultImageIndex', TcxGetPropertiesImageIndexProperty);
  RegisterPropertyEditor(TypeInfo(TcxImageIndex), TcxImageComboBoxItem, 'ImageIndex', TcxGetItemImageIndexProperty);

  RegisterPropertyEditor(TypeInfo(TcxImageIndex), TcxNavigatorButton, 'ImageIndex', TcxNavigatorButtonImageIndexProperty);
  RegisterPropertyEditor(TypeInfo(TcxImageIndex), TcxNavigatorCustomButton, 'ImageIndex', TcxNavigatorCustomButtonImageIndexProperty);

  RegisterPropertyEditor(TypeInfo(TcxImageIndex), TcxEditButton, 'ImageIndex', TcxEditButtonImageIndexProperty);
  RegisterPropertyEditor(TypeInfo(TcxImageIndex), TcxButtonImageOptions, 'ImageIndex', TcxButtonImageIndexProperty);

  RegisterPropertyEditor(TypeInfo(TcxImageIndex), TdxAlertWindowButton, 'ImageIndex', TdxAlertWindowButtonImageIndexProperty);
  RegisterPropertyEditor(TypeInfo(TComponent), TdxAlertWindowOptionsCaptionButtons, 'PopupMenu', TcxControlPopupMenuProperty);

  RegisterPropertyEditor(TypeInfo(TcxImageIndex), TdxBreadcrumbEditButton, 'ImageIndex', TdxBreadcrumbEditButtonImageIndexProperty);
  RegisterPropertyEditor(TypeInfo(TcxImageIndex), TdxBreadcrumbEditRecentPath, 'ImageIndex', TdxBreadcrumbEditRecentPathImageIndexProperty);

  RegisterPropertyEditor(TypeInfo(TNotifyEvent), TcxCustomImage, 'PropertiesEvents', TcxCustomImagePropertiesProperty);
  RegisterPropertyEditor(TypeInfo(string), TcxImageProperties, 'GraphicClassName', TGraphicClassNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TcxImageProperties, 'Caption', TCaptionProperty);

  RegisterPropertyEditor(TypeInfo(TNotifyEvent), TcxCustomBlobEdit, 'PropertiesEvents', TcxCustomImagePropertiesProperty);
  RegisterPropertyEditor(TypeInfo(TCaption), TcxBlobEdit, 'Text', TCaptionProperty);
  RegisterPropertyEditor(TypeInfo(string), TcxBlobEditProperties, 'PictureGraphicClassName', TGraphicClassNameProperty);

  RegisterPropertyEditor(TypeInfo(string), TdxLookupSparklineProperties, 'LookupKeyFieldName', TdxLookupSparkLineFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TdxLookupSparklineItemDataBinding, 'FieldName', TdxLookupSparklineItemFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TdxLookupSparklineItemDataBinding, 'ValueType', nil);
  RegisterPropertyEditor(TypeInfo(string), TdxSparklineItemDataBinding, 'ValueType', TdxSparklineValueTypeProperty);
  RegisterPropertyEditor(TypeInfo(TCollection), TdxSparklineProperties, 'Series', TdxSparklineCollectionProperty);

  RegisterPropertyEditor(GetPropInfo(TdxGalleryControl, 'Gallery').PropType^, TdxGalleryControl, 'Gallery', TdxGalleryPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TcxImageIndex), TdxGalleryControlItem, 'ImageIndex', TdxGalleryControlItemImageIndexProperty);

  RegisterPropertyEditor(TypeInfo(TcxEditMask), TcxCustomMaskEditProperties, 'EditMask', TcxEditMaskProperty);
  RegisterPropertyEditor(TypeInfo(TCaption), TcxCustomMaskEdit, 'Text', TcxTextProperty);
  RegisterPropertyEditor(TypeInfo(TdxAlphaColor), nil, '', TdxAlphaColorPropertyEditor);

  RegisterPropertyEditor(TypeInfo(string), TcxLookupComboBoxProperties, 'KeyFieldNames', TcxLookupComboBoxPropertiesFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TcxLookupComboBoxProperties, 'ListFieldNames', TcxLookupComboBoxPropertiesFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TdxBreadcrumbEdit, 'SelectedPath', TdxBreadcrumbEditSelectedPathPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), TcxLookupDBGridColumn, 'FieldName', TcxLookupDBGridColumnFieldNameProperty);

  RegisterPropertyEditor(TypeInfo(TComponent), TcxCustomNavigatorControl, 'Control', TcxNavigatorControlProperty);
  RegisterPropertyEditor(TypeInfo(TNotifyEvent), TcxCustomNavigator, 'ButtonsEvents', TcxNavigatorButtonsEventEditor);
  RegisterPropertyEditor(TypeInfo(TNotifyEvent), TcxCustomNavigator, 'InfoPanelEvents', TcxNavigatorInfoPanelEventEditor);

  RegisterPropertyEditor(TypeInfo(TComponent), TcxCustomButton, 'PopupMenu', TcxControlPopupMenuProperty);
  RegisterPropertyEditor(TypeInfo(TComponent), TcxButton, 'DropDownMenu', TcxControlPopupMenuProperty);
  RegisterPropertyEditor(TypeInfo(TComponent), TcxRadioButton, 'PopupMenu', TcxControlPopupMenuProperty);
  RegisterPropertyEditor(TypeInfo(TBitmap), TcxButtonImageOptions, 'Glyph', TcxBitmapProperty);
  RegisterPropertyEditor(TypeInfo(TdxSmartGlyph), TcxCheckBoxProperties, 'Glyph', TdxMultiPartGlyphGraphicProperty);

  RegisterPropertyEditor(GetPropInfo(TdxWheelPickerProperties, 'Wheels').PropType^, TdxWheelPickerProperties, 'Wheels', TdxWheelPickerWheelsPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TcxImageIndex), TdxWheelPickerItem, 'ImageIndex', TdxWheelPickerItemImageIndexProperty);
  RegisterPropertyEditor(TypeInfo(TcxImageIndex), TdxTokenEditToken, 'ImageIndex', TdxTokenEditTokenImageIndexProperty);
  RegisterPropertyEditor(TypeInfo(TOwnedCollection), TdxWheelPickerWheel, 'Items', TdxWheelPickerItemCollectionProperty);
  RegisterPropertyEditor(TypeInfo(string), TdxWheelPicker, 'ItemIndex', TdxWheelPickerItemIndexesEditor);

  RegisterPropertyEditor(TypeInfo(TdxDefaultBoolean), TcxDataControllerMultiThreadedOptions, 'Sorting', TdxDataControllerMultithreadedSortingPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TdxDefaultBoolean), TcxDataControllerMultiThreadedOptions, 'Filtering', TdxDataControllerMultithreadedFilteringPropertyEditor);

  RegisterPropertyEditor(TypeInfo(TcxEditStyle), TcxDefaultEditStyleController, '', TcxDefaultEditStyleControllerStyleProperty);

  RegisterPropertyEditor(TypeInfo(TdxCustomBarCodeSymbology), TdxCustomBarCodeProperties, 'Symbology', TdxBarCodeSymbologyProperty);
  RegisterPropertyEditor(TypeInfo(string), TdxCustomBarCodeProperties, 'BarCodeSymbologyClassName', nil);

  HideClassProperties(TcxCustomTextEdit, ['ImeMode', 'ImeName']);
  HideClassProperties(TcxGroupBox, ['RepositoryItem']);
  HideClassProperties(TcxCustomGroupBox, ['CaptionBkColor', 'Color', 'Font', 'LookAndFeel', 'StyleFocused', 'StyleHot']);
  HideClassProperties(TcxButton, ['AllowAllUp', 'CanBeFocused', 'Down', 'Glyph',
    'Layout', 'Margin', 'NumGlyphs', 'Spacing', 'GroupIndex', 'CommandLinkHint', 'UseSystemPaint']);
  HideClassProperties(TcxCheckBoxProperties, ['Caption']);
  HideClassProperties(TcxCustomNavigator, ['Hint']);
  HideClassProperties(TcxHyperLinkStyle, ['TextColor']);
  HideClassProperties(TdxGalleryControl, ['ItemShowHint', 'ItemCheckMode',
    'ColumnCount', 'ItemImageSize', 'ItemShowImageFrame', 'ItemTextPosition',
    'ContentOffset', 'ContentOffsetGroups', 'ContentOffsetItems']);
  HideClassProperties(TdxWheelPicker, ['EditValue']);
  HideClassProperties(TcxCalcEdit, ['EditValue']);

  RegisterPropertyEditor(TypeInfo(Boolean), TcxCustomEditProperties, 'Transparent', nil);
  HideClassProperties(TcxButtonEditProperties, ['HideCursor']);
  HideClassProperties(TcxImageProperties, ['Proportional', 'Stretch']);
  HideClassProperties(TcxCustomDropDownEditProperties, ['ImmediateDropDown', 'ImmediatePopup']);
  HideClassProperties(TcxHyperLinkEditProperties, ['AutoComplete']);
end;

initialization
  FRegisteredDragTargets := TList.Create;
  cxRegisterDragTarget(TcxFieldsTarget);

finalization
  cxUnregisterDragTarget(TcxFieldsTarget);
  FreeAndNil(FRegisteredDragTargets);

end.
