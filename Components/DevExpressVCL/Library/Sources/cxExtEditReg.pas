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

unit cxExtEditReg;

{$I cxVer.inc}

interface

uses
  Classes;

procedure Register;

implementation

uses
  DesignIntf, DesignEditors, Controls, Forms, Graphics, ImgList, StdCtrls,
  SysUtils, TypInfo, ColnEdit, VCLEditors, cxDesignWindows,
  dxCore, cxCheckBox, cxCheckComboBox, cxCheckGroup, cxComponentCollectionEditor,
  cxCheckGroupStatesEditor, cxCheckListBox, cxClasses, cxColorComboBox,
  cxContainer, cxDBCheckComboBox, cxDBCheckGroup, cxDBCheckListBox,
  cxDBColorComboBox, cxDBFontNameComboBox, cxDBLabel, cxDBProgressBar,
  cxDBRichEdit, cxDBTrackBar, cxEdit, cxEditPropEditors, cxEditRepositoryEditor,
  cxExtEditConsts, cxExtEditRepositoryItems, cxFontNameComboBox, cxHeader, cxHint,
  cxHintEditor, cxLabel, cxListView, cxLookAndFeels, cxMCListBox, cxProgressBar,
  cxPropEditors, cxRichEdit, cxScrollBar, cxSpinButton, cxSplitter, cxSplitterEditor,
  cxTrackBar, dxZoomTrackBar, dxDBZoomTrackBar, cxTreeView, dxScreenTip, dxCustomHint, dxTaskbarProgress,
  dxColorEdit, dxDBColorEdit, cxGraphics, cxLibraryReg, dxRatingControl, dxDBRatingControl, dxGDIPlusClasses,
  dxRangeTrackBar, dxDBRangeTrackBar, dxRangeControl, dxUIAdorners, dxUIAdornerTargetElementPathEditor;

const
  cxEditorsRestoreStyleCaption = 'Restore style';
  cxEditorsRestoreLookAndFeelCaption = 'Restore LookAndFeel';
  cxEditComponentEditorVerbS: array[0..0] of string = (cxEditorsRestoreStyleCaption);
  cxEditComponentEditorVerbL: array[0..0] of string = (cxEditorsRestoreLookAndFeelCaption);
  cxSplitterStyleControllerVerb = 'Splitter Editor...';
  cxHintStyleControllerVerb = 'Hints Editor...';

type
  TcxCustomEditAccess = class(TcxCustomEdit);
  TdxAdornersAccess = class(TdxCustomAdorners);

type
  { TdxUIAdornerManagerComponentEditor }

  TdxUIAdornerManagerComponentEditor = class(TcxEditorsLibraryComponentEditorEx)
  private
    function GetManager: TdxUIAdornerManager;
  protected
    procedure InternalExecuteVerb(AIndex: Integer); override;
    function InternalGetVerb(AIndex: Integer): string; override;
    function InternalGetVerbCount: Integer; override;
    procedure ShowAdornersEditor(AAdorners: TdxCustomAdorners);

    property Manager: TdxUIAdornerManager read GetManager;
  end;

    { TdxUIAdornerManagerDesignHelper }

  TdxUIAdornerManagerDesignHelper = class(TdxUIAdornerManagerCustomDesignHelper,
    IcxDesignSelectionChanged)
  private
    FDesignHelper: TcxDesignHelper;
  protected
    function IsObjectSelected(AObject: TPersistent): Boolean; override;
    procedure SelectObject(AObject: TPersistent; AClearSelection: Boolean); override;

    property DesignHelper: TcxDesignHelper read FDesignHelper;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
  end;

  { TdxAdornerTargetElementProperty }

  TdxAdornerTargetElementProperty = class(TClassProperty)
  protected
    function HasSubProperties: Boolean;
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

  { TdxAdornerTargetElementPathProperty }

  TdxAdornerTargetElementPathProperty = class(TStringProperty)
  private
    function GetTargetElement: TdxAdornerTargetElementPath;
  protected
    property TargetElement: TdxAdornerTargetElementPath read GetTargetElement;
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

  { TdxAdornersProperty }

  TdxAdornersProperty = class(TClassProperty)
  private
    function GetAdorners: TdxCustomAdorners;
  protected
    property Adorners: TdxCustomAdorners read GetAdorners;
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

  { TcxHotZoneStyleProperty }

  TcxHotZoneStyleProperty = class(TClassProperty)
  protected
    function HasSubProperties: Boolean;
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

  { TcxSplitterComponentEditor }

  TcxSplitterComponentEditor = class(TcxEditorsLibraryComponentEditorEx)
  protected
    function GetEditItemCaption: string; override;
    procedure ExecuteEditAction; override;
  end;

  { TcxHintStyleComponentEditor }

  TcxHintStyleComponentEditor = class(TcxEditorsLibraryComponentEditorEx)
  private
    function GetController: TcxHintStyleController;
  protected
    function GetEditItemCaption: string; override;
    procedure ExecuteEditAction; override;
  end;

  { TcxHotZoneStyleEventsProperty }

  TcxHotZoneStyleEventsProperty = class(TcxNestedEventProperty)
  protected
    function GetInstance: TPersistent; override;
  end;

  { TcxHeaderSectionImageIndexProperty }

  TcxHeaderSectionImageIndexProperty = class(TImageIndexProperty)
  public
    function GetImages: TCustomImageList; override;
  end;

  { TcxChecksControlStatesItemsProperty }

  TcxChecksControlStatesItemsProperty = class(TPropertyEditor)
  protected
    procedure InitializeDlg(ADialog: TcxCheckGroupStatesEditorDlg); virtual;
    procedure SynchronizeControlCheckStates(ADialog: TcxCheckGroupStatesEditorDlg); virtual;
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetName: string; override;
    function GetValue: string; override;
  end;

  { TcxCheckGroupStatesItemsProperty }

  TcxCheckGroupStatesItemsProperty = class(TcxChecksControlStatesItemsProperty)
  protected
    procedure InitializeDlg(ADialog: TcxCheckGroupStatesEditorDlg); override;
    procedure SynchronizeControlCheckStates(ADialog: TcxCheckGroupStatesEditorDlg); override;
  end;

  { TcxCheckComboBoxComponentEditor }

  TcxCheckComboBoxComponentEditor = class(TcxEditComponentEditor)
  protected
    function HasEditor: Boolean; override;
    procedure RunEditor; override;
  end;

  { TcxCheckComboBoxStatesItemsProperty }

  TcxCheckComboBoxStatesItemsProperty = class(TcxChecksControlStatesItemsProperty)
  protected
    procedure InitializeDlg(ADialog: TcxCheckGroupStatesEditorDlg); override;
    procedure SynchronizeControlCheckStates(ADialog: TcxCheckGroupStatesEditorDlg); override;
  end;

  { TcxHintStyleProperty }

  TcxHintStyleProperty = class(TClassProperty)
  protected
    function HasSubProperties: Boolean;
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

  { TdxRangeControlClientPropertiesProperty }

  TdxRangeControlClientPropertiesProperty = class(TClassProperty)
  protected
    function HasSubProperties: Boolean;
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

  TdxRangeControlPrimaryScaleProperty = class(TPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

  { TdxRangeControlNumericClientRangeValueTypeProperty }

  TdxRangeControlNumericClientRangeValueTypeProperty = class(TPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

  { TdxRangeControlRangeValueProperty }

  TdxRangeControlRangeValueProperty = class(TVariantProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
  end;

  { TdxRangeControlClientProperty }

  TdxRangeControlClientProperty = class(TComponentProperty)
  private
    FProc: TGetStrProc;
    procedure CheckComponent(const Value: string);
  public
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  { TdxRangeControlClientEventsProperty }

  TdxRangeControlClientEventsProperty = class(TcxNestedEventProperty)
  protected
    function GetInstance: TPersistent; override;
  end;

  { TdxTaskbarProgressLinkedComponentProperty }

  TdxTaskbarProgressLinkedComponentProperty = class(TComponentProperty)
  private
    FProc: TGetStrProc;
    procedure CheckComponent(const Value: string);
  public
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  { TcxCheckGroupComponentEditor }

  TcxCheckGroupComponentEditor = class(TcxEditComponentEditor)
  protected
    function HasEditor: Boolean; override;
    procedure RunEditor; override;
  public
    procedure Edit; override;
  end;

  { TcxCheckBoxBasedControlSelectionEditor }

  TcxCheckBoxBasedControlSelectionEditor = class(TSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

  { TcxCommonControlsSelectionEditor }

  TcxCommonControlsSelectionEditor = class(TSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

  { TcxHintStyleControllerSelectionEditor }

  TcxHintStyleControllerSelectionEditor = class(TSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

  TdxRangeControlSelectionEditor = class(TSelectionEditor)
  protected
    ComponentsList: TStringList;
  public
    procedure AddComponent(const Name: string);
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

  { TcxCheckListBoxItemImageIndexProperty }

  TcxCheckListBoxItemImageIndexProperty = class(TImageIndexProperty)
  public
    function GetImages: TCustomImageList; override;
  end;

  { TdxBadgesImageIndexProperty }

  TdxBadgesImageIndexProperty = class(TImageIndexProperty)
  public
    function GetImages: TCustomImageList; override;
  end;

{ TdxUIAdornerManagerComponentEditor }

procedure TdxUIAdornerManagerComponentEditor.InternalExecuteVerb(AIndex: Integer);
begin
  case AIndex of
    0: ShowAdornersEditor(Manager.Badges);
    1: ShowAdornersEditor(Manager.Guides);
  end;
end;

function TdxUIAdornerManagerComponentEditor.InternalGetVerb(AIndex: Integer): string;
begin
  case AIndex of
    0: Result := 'Badges...';
    1: Result := 'Guides...';
  end;
end;

function TdxUIAdornerManagerComponentEditor.InternalGetVerbCount: Integer;
begin
  Result := 2;
end;

procedure TdxUIAdornerManagerComponentEditor.ShowAdornersEditor(AAdorners: TdxCustomAdorners);
begin
  ShowFormEditorClass(Designer, Manager, AAdorners, TdxAdornersAccess(AAdorners).GetName, TfrmComponentCollectionEditor);
end;

function TdxUIAdornerManagerComponentEditor.GetManager: TdxUIAdornerManager;
begin
  Result := TdxUIAdornerManager(Component);
end;

{ TdxUIAdornerManagerDesignHelper }

constructor TdxUIAdornerManagerDesignHelper.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FDesignHelper := TcxDesignHelper.Create(Manager);
  DesignHelper.AddSelectionChangedListener(Self);
end;

destructor TdxUIAdornerManagerDesignHelper.Destroy;
begin
  DesignHelper.RemoveSelectionChangedListener(Self);
  FreeAndNil(FDesignHelper);
  inherited Destroy;
end;

function TdxUIAdornerManagerDesignHelper.IsObjectSelected(AObject: TPersistent): Boolean;
begin
  Result := DesignHelper.IsObjectSelected(AObject);
end;

procedure TdxUIAdornerManagerDesignHelper.SelectObject(AObject: TPersistent; AClearSelection: Boolean);
begin
  DesignHelper.SelectObject(AObject, AClearSelection);
end;

{ TdxAdornerTargetElementProperty }

function TdxAdornerTargetElementProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes;
  if not HasSubProperties then
    Exclude(Result, paSubProperties);
  Result := Result + [paValueEditable, paValueList, paSortList, paRevertable, paVolatileSubProperties];
end;

function TdxAdornerTargetElementProperty.GetValue: string;
begin
  if HasSubProperties then
    Result := dxRegisteredAdornerTargetElementTypes.GetDescriptionByClass(
      TdxAdornerCustomTargetElement(GetOrdValue).ClassType)
  else
    Result := '';
end;

procedure TdxAdornerTargetElementProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  for I := 0 to dxRegisteredAdornerTargetElementTypes.Count - 1 do
    Proc(dxRegisteredAdornerTargetElementTypes.Descriptions[I]);
end;

function TdxAdornerTargetElementProperty.HasSubProperties: Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to PropCount - 1 do
    if not (GetComponent(I) is TdxCustomAdorner) or
      (TdxCustomAdorner(GetComponent(I)).TargetElement = nil) then
      Exit;
  Result := True;
end;

procedure TdxAdornerTargetElementProperty.SetValue(const Value: string);
var
  I: Integer;
  ATargetElementClass: TdxAdornerCustomTargetElementClass;
begin
  ATargetElementClass := TdxAdornerCustomTargetElementClass(dxRegisteredAdornerTargetElementTypes.FindByClassName(Value));
  if ATargetElementClass = nil then
    ATargetElementClass := TdxAdornerCustomTargetElementClass(dxRegisteredAdornerTargetElementTypes.FindByDescription(Value));
  for I := 0 to PropCount - 1 do
    if GetComponent(I) is TdxCustomAdorner then
      TdxCustomAdorner(GetComponent(I)).TargetElementClass := ATargetElementClass;
  Modified;
end;

{ TdxAdornerTargetElementPathProperty }

procedure TdxAdornerTargetElementPathProperty.Edit;
begin
  if ShowAdornerTargetElementPathEditor(TargetElement) then
    Modified;
end;

function TdxAdornerTargetElementPathProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes;
  if PropCount = 1 then
    Result := [paDialog];
end;

function TdxAdornerTargetElementPathProperty.GetTargetElement: TdxAdornerTargetElementPath;
begin
  Result := TdxAdornerTargetElementPath(GetComponent(0));
end;

{ TdxAdornersProperty }

procedure TdxAdornersProperty.Edit;
begin
  ShowFormEditorClass(Designer, Adorners.Manager, Adorners, TdxAdornersAccess(Adorners).GetName, TfrmComponentCollectionEditor);
end;

function TdxAdornersProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes;
  Result := Result + [paSubProperties];
  if PropCount = 1 then
    Result := Result + [paDialog];
end;

function TdxAdornersProperty.GetAdorners: TdxCustomAdorners;
begin
  Result := TdxCustomAdorners(GetOrdValue);
end;

{ TcxHotZoneStyleProperty }

function TcxHotZoneStyleProperty.HasSubProperties: Boolean;
var
  I: Integer;
begin
  for I := 0 to PropCount - 1 do
  begin
    Result := TcxCustomSplitter(GetComponent(I)).HotZone <> nil;
    if not Result then Exit;
  end;
  Result := True;
end;

function TcxHotZoneStyleProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes;
  if not HasSubProperties then
    Exclude(Result, paSubProperties);
  Result := Result - [paReadOnly] +
    [paValueList, paSortList, paRevertable, paVolatileSubProperties];
end;

function TcxHotZoneStyleProperty.GetValue: string;
begin
  if HasSubProperties then
    Result := GetRegisteredHotZoneStyles.GetDescriptionByClass(
      TcxCustomSplitter(GetComponent(0)).HotZone.ClassType)
  else
    Result := '';
end;

procedure TcxHotZoneStyleProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  for I := 0 to GetRegisteredHotZoneStyles.Count - 1 do
    Proc(GetRegisteredHotZoneStyles.Descriptions[I]);
end;

procedure TcxHotZoneStyleProperty.SetValue(const Value: string);
var
  FHotZoneStyleClass: TcxHotZoneStyleClass;
  I: Integer;
begin
  FHotZoneStyleClass := TcxHotZoneStyleClass(GetRegisteredHotZoneStyles.FindByClassName(Value));
  if FHotZoneStyleClass = nil then
    FHotZoneStyleClass := TcxHotZoneStyleClass(GetRegisteredHotZoneStyles.FindByDescription(Value));

  for I := 0 to PropCount - 1 do
    TcxCustomSplitter(GetComponent(I)).HotZoneStyleClass := FHotZoneStyleClass;
  Modified;
end;

{ TcxHotZoneStyleEventsProperty }

function TcxHotZoneStyleEventsProperty.GetInstance: TPersistent;
begin
  Result := TcxCustomSplitter(GetComponent(0)).HotZone;
end;

{ TcxHeaderSectionImageIndexProperty }

function TcxHeaderSectionImageIndexProperty.GetImages: TCustomImageList;
begin
  Result := nil;
  if GetComponent(0) is TcxHeaderSection then
    Result := TCustomImageList(TcxHeaderSection(GetComponent(0)).HeaderControl.Images);
end;

{ TcxSplitterComponentEditor }

function TcxSplitterComponentEditor.GetEditItemCaption: string;
begin
  Result := cxSplitterStyleControllerVerb;
end;

procedure TcxSplitterComponentEditor.ExecuteEditAction;
begin
  ShowSplitterEditor(Component as TcxSplitter);
end;

{ TcxHintStyleComponentEditor }

function TcxHintStyleComponentEditor.GetEditItemCaption: string;
begin
  Result := cxHintStyleControllerVerb;
end;

procedure TcxHintStyleComponentEditor.ExecuteEditAction;
begin
  if GetController.HintStyle is TcxHintStyle then
    ShowHintStyleEditor(GetController)
  else
    ShowCollectionEditor(Designer, Component,
      (GetController.HintStyle as TdxScreenTipStyle).ScreenTipLinks, 'ScreenTipLinks');
end;

function TcxHintStyleComponentEditor.GetController: TcxHintStyleController;
begin
  Result := Component as TcxHintStyleController;
end;

{ TcxChecksControlStatesItemsProperty }

procedure TcxChecksControlStatesItemsProperty.Edit;
var
  ADialog: TcxCheckGroupStatesEditorDlg;
begin
  ADialog := TcxCheckGroupStatesEditorDlg.Create(Application);
  try
    InitializeDlg(ADialog);
    if ADialog.ShowModal = mrOK then
    begin
      SynchronizeControlCheckStates(ADialog);
      Modified;
    end;
  finally
    ADialog.Free;
  end;
end;

function TcxChecksControlStatesItemsProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

function TcxChecksControlStatesItemsProperty.GetName: string;
begin
  Result := 'States';
end;

function TcxChecksControlStatesItemsProperty.GetValue: string;
begin
  Result := '';
end;

procedure TcxChecksControlStatesItemsProperty.InitializeDlg(ADialog: TcxCheckGroupStatesEditorDlg);
begin
end;

procedure TcxChecksControlStatesItemsProperty.SynchronizeControlCheckStates(ADialog: TcxCheckGroupStatesEditorDlg);
begin
end;

{ TcxCheckGroupStatesItemsProperty }

procedure TcxCheckGroupStatesItemsProperty.InitializeDlg(
  ADialog: TcxCheckGroupStatesEditorDlg);
var
  ACheckGroup: TcxCustomCheckGroup;
  AItem: TcxCheckListBoxItem;
  I: Integer;
begin
  ADialog.Caption := cxGetResourceString(@
    cxSCheckGroupStatesItemsPropertyDlgCaption);
  ADialog.clbStates.Items.Clear;
  ACheckGroup := TcxCustomCheckGroup(GetComponent(0));
  ADialog.clbStates.AllowGrayed := ACheckGroup.ActiveProperties.AllowGrayed and
    (ACheckGroup.ActiveProperties.EditValueFormat <> cvfInteger);
  for I := 0 to ACheckGroup.ActiveProperties.Items.Count - 1 do
  begin
    AItem := TcxCheckListBoxItem(ADialog.clbStates.Items.Add);
    AItem.Text := ACheckGroup.ActiveProperties.Items[I].Caption;
    AItem.State := ACheckGroup.States[I];
  end;
end;

procedure TcxCheckGroupStatesItemsProperty.SynchronizeControlCheckStates(ADialog: TcxCheckGroupStatesEditorDlg);
var
  I: Integer;
begin
  with TcxCustomCheckGroup(GetComponent(0)) do
    for I := 0 to ADialog.clbStates.Items.Count - 1 do
      States[I] := ADialog.clbStates.Items[I].State;
end;

{ TcxCheckComboBoxComponentEditor }

function TcxCheckComboBoxComponentEditor.HasEditor: Boolean;
begin
  Result := True;
end;

procedure TcxCheckComboBoxComponentEditor.RunEditor;
begin
  ShowCollectionEditor(Designer, Component,
    TcxCheckComboBoxProperties(TcxCheckComboBox(Component).Properties).Items, 'Items');
end;

{ TcxCheckComboBoxStatesItemsProperty }

procedure TcxCheckComboBoxStatesItemsProperty.InitializeDlg(ADialog: TcxCheckGroupStatesEditorDlg);
var
  ACheckComboBox: TcxCustomCheckComboBox;
  AItem: TcxCheckListBoxItem;
  I: Integer;
begin
  ADialog.Caption := cxGetResourceString(@
    cxSCheckComboBoxStatesItemsPropertyDlgCaption);
  ADialog.clbStates.Items.Clear;
  ACheckComboBox := TcxCustomCheckComboBox(GetComponent(0));
  ADialog.clbStates.AllowGrayed := False;
  for I := 0 to ACheckComboBox.ActiveProperties.Items.Count - 1 do
  begin
    AItem := TcxCheckListBoxItem(ADialog.clbStates.Items.Add);
    AItem.Text := ACheckComboBox.ActiveProperties.Items[I].Description;
    AItem.State := ACheckComboBox.States[I];
  end;
end;

procedure TcxCheckComboBoxStatesItemsProperty.SynchronizeControlCheckStates(ADialog: TcxCheckGroupStatesEditorDlg);
var
  I: Integer;
begin
  with TcxCustomCheckComboBox(GetComponent(0)) do
    for I := 0 to ADialog.clbStates.Items.Count - 1 do
      States[I] := ADialog.clbStates.Items[I].State;
end;

{ TcxHintStyleProperty }

type
  TcxHintStyleControllerAccess = class(TcxCustomHintStyleController);

function TcxHintStyleProperty.HasSubProperties: Boolean;
var
  I: Integer;
begin
  for I := 0 to PropCount - 1 do
  begin
    Result := TcxHintStyleControllerAccess(GetComponent(I)).HintStyle <> nil;  // to do
    if not Result then Exit;
  end;
  Result := True;
end;

function TcxHintStyleProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes;
  if not HasSubProperties then
    Exclude(Result, paSubProperties);
  Result := Result - [paReadOnly] +
    [paValueList, paSortList, paRevertable, paVolatileSubProperties];
end;

function TcxHintStyleProperty.GetValue: string;
begin
  if HasSubProperties then
    Result := cxRegisteredHintStyles.GetDescriptionByClass(TcxCustomHintStyle(GetOrdValue).ClassType)
  else
    Result := '';
end;

procedure TcxHintStyleProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  for I := 0 to cxRegisteredHintStyles.Count - 1 do
    Proc(cxRegisteredHintStyles.Descriptions[I]);
end;

procedure UpdateObjectInspector(ADesigner: IDesigner);
var
  AComponents: IDesignerSelections;
begin
  if ADesigner <> nil then
  begin
    AComponents := CreateSelectionList;
    ADesigner.GetSelections(AComponents);
    ADesigner.ClearSelection;
    ADesigner.SetSelections(AComponents);
  end;
end;

procedure TcxHintStyleProperty.SetValue(const Value: string);
var
  FHintStyleClass: TcxHintStyleClass;
  I: Integer;
begin
  FHintStyleClass := TcxHintStyleClass(cxRegisteredHintStyles.FindByClassName(Value));
  if FHintStyleClass = nil then
    FHintStyleClass := TcxHintStyleClass(cxRegisteredHintStyles.FindByDescription(Value));

  ObjectInspectorCollapseProperty;
  for I := 0 to PropCount - 1 do
    TcxHintStyleControllerAccess(GetComponent(I)).HintStyleClass := FHintStyleClass;
{$IFNDEF DELPHI15}
  UpdateObjectInspector(Designer);
{$ENDIF}
  Modified;
end;

{ TdxRangeControlClientPropertiesProperty }

function TdxRangeControlClientPropertiesProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes;
  if not HasSubProperties then
    Exclude(Result, paSubProperties);
  Result := Result - [paReadOnly] +
    [paValueList, paSortList, paRevertable, paVolatileSubProperties];
end;

function TdxRangeControlClientPropertiesProperty.GetValue: string;
begin
  if HasSubProperties then
    Result := dxRangeControlClients.GetDescriptionByClass(TdxRangeControlCustomClientProperties(GetOrdValue).ClassType)
  else
    Result := '';
end;

procedure TdxRangeControlClientPropertiesProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  for I := 0 to dxRangeControlClients.Count - 1 do
    Proc(dxRangeControlClients.Descriptions[I]);
end;

function TdxRangeControlClientPropertiesProperty.HasSubProperties: Boolean;
var
  I: Integer;
begin
  for I := 0 to PropCount - 1 do
  begin
    Result := TdxRangeControl(GetComponent(I)).ClientProperties <> nil;
    if not Result then Exit;
  end;
  Result := True;
end;

procedure TdxRangeControlClientPropertiesProperty.SetValue(const Value: string);
var
  AClientPropertiesClass: TdxRangeControlClientPropertiesClass;
  I: Integer;
begin
  AClientPropertiesClass := TdxRangeControlClientPropertiesClass(dxRangeControlClients.FindByClassName(Value));
  if AClientPropertiesClass = nil then
    AClientPropertiesClass := TdxRangeControlClientPropertiesClass(dxRangeControlClients.FindByDescription(Value));

  ObjectInspectorCollapseProperty;
  for I := 0 to PropCount - 1 do
    TdxRangeControl(GetComponent(I)).ClientPropertiesClass := AClientPropertiesClass;
{$IFNDEF DELPHI15}
  UpdateObjectInspector(Designer);
{$ENDIF}
  Modified;
end;

const
  AScaleNames: array [rcduMinute..rcduYear] of string = ('Minute', 'Hour', 'Day', 'Week', 'Month', 'Quarter', 'Year');

{ TdxRangeControlPrimaryScaleProperty }

function TdxRangeControlPrimaryScaleProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paMultiSelect, paRevertable, paValueEditable, paReadOnly];
end;

function TdxRangeControlPrimaryScaleProperty.GetValue: string;
begin
  Result := AScaleNames[TdxRangeControlDateTimeScale(GetOrdValue).ScaleUnit];
end;

procedure TdxRangeControlPrimaryScaleProperty.GetValues(Proc: TGetStrProc);
var
  AScaleUnit: TdxRangeControlDateTimeScaleUnit;
begin
  for AScaleUnit := rcduMinute to High(TdxRangeControlDateTimeScaleUnit) do
    Proc(AScaleNames[AScaleUnit]);
end;

procedure TdxRangeControlPrimaryScaleProperty.SetValue(const Value: string);
var
  I: Integer;
  AProperties: TdxRangeControlCustomDateTimeHeaderClientProperties;
  ACurrentScaleUnit, AScaleUnit: TdxRangeControlDateTimeScaleUnit;
begin
  AScaleUnit := rcduDay;
  for ACurrentScaleUnit := rcduMinute to High(TdxRangeControlDateTimeScaleUnit) do
    if SameText(AScaleNames[ACurrentScaleUnit], Value) then
    begin
      AScaleUnit := ACurrentScaleUnit;
      Break;
    end;
  for I := 0 to PropCount - 1 do
  begin
    AProperties := TdxRangeControlCustomDateTimeHeaderClientProperties(GetComponent(I));
    AProperties.Scales.GetScale(AScaleUnit).Active := True;
  end;
  Modified;
end;

{ TdxRangeControlNumericClientRangeValueTypeProperty }

var
  VarTypeNames: array[varEmpty..varInt64] of string = (
    'Unassigned', // varEmpty
    'Null',       // varNull
    'Smallint',   // varSmallint
    'Integer',    // varInteger
    'Single',     // varSingle
    'Double',     // varDouble
    'Currency',   // varCurrency
    'Date',       // varDate
    'OleStr',     // varOleStr
    '',           // varDispatch
    '',           // varError
    'Boolean',    // varBoolean
    '',           // varVariant
    '',           // varUnknown
    '',           // [varDecimal]
    '',           // [undefined]
    'Shortint',   // varShortInt
    'Byte',       // varByte
    'Word',       // varWord
    'LongWord',   // varLongWord
    'Int64');     // varInt64

function TdxRangeControlNumericClientRangeValueTypeProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paValueList, paSortList];
end;

function TdxRangeControlNumericClientRangeValueTypeProperty.GetValue: string;
begin
  Result := VarTypeNames[GetOrdValue];
end;

procedure TdxRangeControlNumericClientRangeValueTypeProperty.GetValues(
  Proc: TGetStrProc);
var
  I: Word;
begin
  for I := varSmallint to varInt64 do
    if TdxRangeControlNumericClientProperties(GetComponent(0)).IsRangeValueTypeSupported(I) then
      Proc(VarTypeNames[I]);
end;

procedure TdxRangeControlNumericClientRangeValueTypeProperty.SetValue(const Value: string);

  function GetSelectedType: Integer;
  var
    I: Integer;
  begin
    Result := varInteger;
    for I := 0 to High(VarTypeNames) do
      if VarTypeNames[I] = Value then
      begin
        Result := I;
        Break;
      end;
  end;

begin
  SetOrdValue(GetSelectedType);
end;

{ TdxRangeControlRangeValueProperty }

function TdxRangeControlRangeValueProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect];
end;

{ TdxRangeControlClientProperty }

procedure TdxRangeControlClientProperty.CheckComponent(const Value: string);
var
  AComponent: TComponent;
begin
  AComponent := Designer.GetComponent(Value);
  if Supports(AComponent, IdxRangeControlClient) then
    FProc(Value);
end;

procedure TdxRangeControlClientProperty.GetValues(Proc: TGetStrProc);
begin
  FProc := Proc;
  inherited GetValues(CheckComponent);
end;

{ TdxRangeControlClientEventsProperty }

function TdxRangeControlClientEventsProperty.GetInstance: TPersistent;
begin
  if GetComponent(0) is TdxCustomRangeControl then
    Result := TdxCustomRangeControl(GetComponent(0)).ClientProperties
  else
    Result := nil;
end;

{ TdxTaskbarProgressLinkedComponentProperty }

procedure TdxTaskbarProgressLinkedComponentProperty.GetValues(Proc: TGetStrProc);
begin
  FProc := Proc;
  inherited GetValues(CheckComponent);
end;

procedure TdxTaskbarProgressLinkedComponentProperty.CheckComponent(const Value: string);
var
  AComponent: TComponent;
begin
  AComponent := Designer.GetComponent(Value);
  if (AComponent <> nil) and (AComponent is TcxCustomProgressBar) then
    FProc(Value);
end;

{ TcxCheckGroupComponentEditor }

procedure TcxCheckGroupComponentEditor.Edit;
const
  AMethodParams: array[0..0] of TMethodParam =
    ((Flags: [pfAddress]; Name: 'Sender'; TypeName: 'TObject'));
begin
  ShowEventMethod(Designer, TcxCustomEditAccess(Component).Properties,
    'OnChange', Component.Name + 'PropertiesChange', AMethodParams);
end;

function TcxCheckGroupComponentEditor.HasEditor: Boolean;
begin
  Result := True;
end;

procedure TcxCheckGroupComponentEditor.RunEditor;
begin
  ShowCollectionEditor(Designer, Component,
    TcxCheckGroupProperties(TcxCheckGroup(GetComponent).Properties).Items, 'Items');
end;

{ TcxCheckBoxBasedControlSelectionEditor }

procedure TcxCheckBoxBasedControlSelectionEditor.RequiresUnits(Proc: TGetStrProc);
begin
  Proc('cxCheckBox');
end;

{ TcxCommonControlsSelectionEditor }

procedure TcxCommonControlsSelectionEditor.RequiresUnits(Proc: TGetStrProc);
begin
{$IFDEF DELPHI16}
  Proc('Vcl.ComCtrls');
{$ELSE}
  Proc('ComCtrls');
{$ENDIF}
end;

{ TcxHintStyleControllerSelectionEditor }

procedure TcxHintStyleControllerSelectionEditor.RequiresUnits(
  Proc: TGetStrProc);
begin
  inherited;
  Proc('dxScreenTip');
end;

{ TdxRangeControlSelectionEditor }

procedure TdxRangeControlSelectionEditor.AddComponent(const Name: string);
begin
  ComponentsList.Add(Name);
end;

procedure TdxRangeControlSelectionEditor.RequiresUnits(Proc: TGetStrProc);

  procedure AddUnitName(AObject: TObject);
  begin
    if AObject <> nil then
      Proc(cxGetUnitName(AObject.ClassType));
  end;

var
  AComponent: TComponent;
  I: Integer;
begin
  inherited;
  ComponentsList := TStringList.Create;
  try
    Designer.GetComponentNames(GetTypeData(PTypeInfo(TdxRangeControl.ClassInfo)), AddComponent);
    for I := 0 to ComponentsList.Count - 1 do
    begin
      AComponent := Designer.GetComponent(ComponentsList[I]);
      if AComponent <> nil then
        AddUnitName((AComponent as TdxRangeControl).ClientProperties);
    end;
  finally
    ComponentsList.Free;
  end;
end;

{ TcxCheckListBoxItemImageIndexProperty }

function TcxCheckListBoxItemImageIndexProperty.GetImages: TCustomImageList;
begin
  Result := TcxCustomInnerCheckListBox(GetPersistentOwner(
    TcxCheckListBoxItem(GetComponent(0)).Collection)).Container.Images;
end;

{ TdxBadgesImageIndexProperty }

function TdxBadgesImageIndexProperty.GetImages: TCustomImageList;
begin
  Result := TdxBadgeBackground(GetComponent(0)).Adorner.Manager.Images;
end;

procedure RegisterEditRepositoryItems;
begin
  RegisterEditRepositoryItem(TcxEditRepositoryLabel, scxSEditRepositoryLabelItem);
  RegisterEditRepositoryItem(TcxEditRepositoryFontNameComboBox, scxSEditRepositoryFontNameComboBoxItem);
  RegisterEditRepositoryItem(TcxEditRepositoryColorComboBox, scxSEditRepositoryColorComboBoxItem);
  RegisterEditRepositoryItem(TcxEditRepositoryColorEdit, scxSEditRepositoryColorEditItem);
  RegisterEditRepositoryItem(TcxEditRepositoryProgressBar, scxSEditRepositoryProgressBarItem);
  RegisterEditRepositoryItem(TcxEditRepositoryTrackBar, scxSEditRepositoryTrackBarItem);
  RegisterEditRepositoryItem(TcxEditRepositoryRangeTrackBar, scxSEditRepositoryRangeTrackBarItem);
  RegisterEditRepositoryItem(TcxEditRepositoryCheckComboBox, scxSEditRepositoryCheckComboBox);
  RegisterEditRepositoryItem(TcxEditRepositoryCheckGroupItem, scxSEditRepositoryCheckGroupItem);
  RegisterEditRepositoryItem(TcxEditRepositoryRichItem, scxSEditRepositoryRichEditItem);
  RegisterEditRepositoryItem(TcxEditRepositoryRatingControl, scxSEditRepositoryRatingControlItem);
end;

procedure UnregisterEditRepositoryItems;
begin
  UnregisterEditRepositoryItem(TcxEditRepositoryLabel);
  UnregisterEditRepositoryItem(TcxEditRepositoryFontNameComboBox);
  UnregisterEditRepositoryItem(TcxEditRepositoryColorComboBox);
  UnregisterEditRepositoryItem(TcxEditRepositoryProgressBar);
  UnregisterEditRepositoryItem(TcxEditRepositoryRangeTrackBar);
  UnregisterEditRepositoryItem(TcxEditRepositoryTrackBar);
  UnregisterEditRepositoryItem(TcxEditRepositoryCheckComboBox);
  UnregisterEditRepositoryItem(TcxEditRepositoryCheckGroupItem);
  UnregisterEditRepositoryItem(TcxEditRepositoryRichItem);
  UnregisterEditRepositoryItem(TcxEditRepositoryRatingControl);
end;

procedure Register;
begin
  ForceDemandLoadState(dlDisable);

  RegisterComponents(cxEditorsLibraryProductPage, [TcxLabel, TcxProgressBar, TcxTrackBar, TdxZoomTrackBar,
    TcxCheckListBox, TcxColorComboBox, TcxFontNameComboBox, TcxCheckComboBox,
    TcxCheckGroup, TcxRichEdit, TdxColorEdit, TdxRatingControl, TdxRangeTrackBar, TdxRangeControl]);
  RegisterComponents(cxEditorsDBLibraryProductPage, [TcxDBLabel, TcxDBProgressBar, TcxDBTrackBar, TdxDBZoomTrackBar,
    TcxDBCheckListBox, TcxDBColorComboBox, TcxDBFontNameComboBox, TcxDBCheckComboBox,
    TcxDBCheckGroup, TcxDBRichEdit, TdxDBColorEdit, TdxDBRatingControl, TdxDBRangeTrackBar]);
  RegisterComponents(cxEditorsUtilitiesProductPage, [TcxHintStyleController, TcxSpinButton,
    TcxMCListBox, TcxListView, TcxTreeView, TcxHeader, TcxSplitter, TdxTaskbarProgress, TdxUIAdornerManager]);
  RegisterClasses([TdxBadge, TdxGuide]);
  RegisterNoIcon([TdxBadge, TdxGuide]);

  RegisterPropertyEditor(TypeInfo(string), TdxAdornerTargetElementPath, 'Path', TdxAdornerTargetElementPathProperty);
  RegisterPropertyEditor(TypeInfo(string), TdxCustomAdorner, 'TargetElementClassName', nil);
  RegisterPropertyEditor(TypeInfo(TdxAdornerCustomTargetElement), TdxCustomAdorner, 'TargetElement', TdxAdornerTargetElementProperty);
  RegisterPropertyEditor(TypeInfo(TShortCut), TdxGuides, 'DeactivateKey', TShortCutProperty);
  RegisterPropertyEditor(TypeInfo(TdxCustomAdorners), TdxUIAdornerManager, '', TdxAdornersProperty);
  RegisterPropertyEditor(TypeInfo(string), TcxCustomSplitter, 'HotZoneClassName', nil);
  RegisterPropertyEditor(TypeInfo(Boolean), TcxCustomSplitter, 'ResizeIgnoreSnap', nil);
  RegisterPropertyEditor(TypeInfo(TcxHotZoneStyle), TcxCustomSplitter, 'HotZone', TcxHotZoneStyleProperty);
  RegisterPropertyEditor(TypeInfo(TNotifyEvent), TcxCustomSplitter, 'HotZoneEvents', TcxHotZoneStyleEventsProperty);
  RegisterPropertyEditor(TypeInfo(TcxEditValue), TcxCustomCheckGroup, 'EditValue', nil);
  RegisterPropertyEditor(TypeInfo(Boolean), TcxCustomCheckGroup, 'StatesItems', TcxCheckGroupStatesItemsProperty);
  RegisterPropertyEditor(TypeInfo(TdxSmartGlyph), TcxCustomCheckGroupProperties, 'Glyph', TdxMultiPartGlyphGraphicProperty);
  RegisterPropertyEditor(TypeInfo(Boolean), TcxCustomCheckComboBox, 'StatesItems', TcxCheckComboBoxStatesItemsProperty);
  RegisterPropertyEditor(TypeInfo(TcxEditValue), TcxCustomCheckListBox, 'EditValue', nil);
  RegisterPropertyEditor(TypeInfo(TMeasureItemEvent), TcxCustomCheckListBox, 'OnMeasureItem', nil);
  RegisterPropertyEditor(TypeInfo(TdxSmartGlyph), TcxCustomCheckListBox, 'Glyph', TdxMultiPartGlyphGraphicProperty);
  RegisterPropertyEditor(TypeInfo(TcxEditValue), TcxCustomCheckComboBox, 'EditValue', nil);
  RegisterPropertyEditor(TypeInfo(Variant), TcxCustomCheckComboBox, 'Value', nil);
  RegisterPropertyEditor(TypeInfo(Boolean), TcxCustomSpinButton, 'AutoSize', nil);
  RegisterPropertyEditor(TypeInfo(TTabOrder), TcxCustomLabel, 'TabOrder', nil);
  RegisterPropertyEditor(TypeInfo(Boolean), TcxCustomLabel, 'TabStop', nil);
  RegisterPropertyEditor(TypeInfo(Boolean), TcxCustomCheckComboBoxProperties, 'AllowGrayed', nil);
  RegisterPropertyEditor(TypeInfo(TdxSmartGlyph), TcxCustomCheckComboBoxProperties, 'Glyph', TdxMultiPartGlyphGraphicProperty);
  RegisterPropertyEditor(TypeInfo(TcxProgressBarPropertiesValues), TcxCustomProgressBarProperties, 'AssignedValues', nil);
  RegisterPropertyEditor(TypeInfo(TComponent), TdxTaskbarCustomProgress, 'LinkedComponent',
    TdxTaskbarProgressLinkedComponentProperty);
  RegisterPropertyEditor(TypeInfo(Integer), TcxCustomListView, 'ItemIndex', nil);
  RegisterPropertyEditor(TypeInfo(TcxCustomHintStyle), TcxCustomHintStyleController,
    'HintStyle', TcxHintStyleProperty);
  RegisterPropertyEditor(TypeInfo(string), TcxCustomHintStyleController,
    'HintStyleClassName', nil);
  RegisterPropertyEditor(TypeInfo(TdxRangeControlCustomClientProperties), TdxRangeControl,
    'ClientProperties', TdxRangeControlClientPropertiesProperty);
  RegisterPropertyEditor(TypeInfo(TdxRangeControlDateTimeScale), TdxRangeControlDateTimeClientProperties,
    'Scale', TdxRangeControlPrimaryScaleProperty);
  RegisterPropertyEditor(TypeInfo(TdxRangeControlDateTimeScale), TdxRangeControlCustomDateTimeHeaderClientProperties,
    'PrimaryScale', TdxRangeControlPrimaryScaleProperty);
  RegisterPropertyEditor(TypeInfo(Word), TdxRangeControlNumericClientProperties,
    'RangeValueType', TdxRangeControlNumericClientRangeValueTypeProperty);
  RegisterPropertyEditor(TypeInfo(TdxRangeEditValue), TdxCustomRangeControl, '', TdxRangeControlRangeValueProperty);
  RegisterPropertyEditor(TypeInfo(TdxRangeEditValue), TdxRangeControlCustomClientProperties, '', TdxRangeControlRangeValueProperty);
  RegisterPropertyEditor(TypeInfo(TComponent), TdxRangeControl, 'Client', TdxRangeControlClientProperty);
  HideClassProperties(TdxRangeControl, ['ClientPropertiesClassName']);

  RegisterSelectionEditor(TcxCustomListView, TcxCommonControlsSelectionEditor);
  RegisterSelectionEditor(TcxCustomTreeView, TcxCommonControlsSelectionEditor);
  RegisterSelectionEditor(TcxHintStyleController, TcxHintStyleControllerSelectionEditor);
  RegisterSelectionEditor(TcxCheckComboBox, TcxCheckBoxBasedControlSelectionEditor);
  RegisterSelectionEditor(TcxCheckListBox, TcxCheckBoxBasedControlSelectionEditor);
  RegisterSelectionEditor(TcxEditRepositoryCheckComboBox, TcxCheckBoxBasedControlSelectionEditor);
  RegisterSelectionEditor(TdxRangeControl, TdxRangeControlSelectionEditor);

  RegisterPropertyEditor(TypeInfo(TcxImageIndex), TcxCheckListBoxItem, 'ImageIndex', TcxCheckListBoxItemImageIndexProperty);
  RegisterPropertyEditor(TypeInfo(TcxImageIndex), TcxHeaderSection, 'ImageIndex', TcxHeaderSectionImageIndexProperty);
  RegisterPropertyEditor(TypeInfo(TcxImageIndex), TdxBadgeBackground, 'ImageIndex', TdxBadgesImageIndexProperty);

  RegisterComponentEditor(TcxCheckComboBox, TcxCheckComboBoxComponentEditor);
  RegisterComponentEditor(TcxCheckListBox, TcxContainerComponentEditor);
  RegisterComponentEditor(TcxCustomCheckGroup, TcxCheckGroupComponentEditor);
  RegisterComponentEditor(TcxCustomHeader, TcxEditorsLibraryCXControlComponentEditor);
  RegisterComponentEditor(TcxDBCheckListBox, TcxContainerComponentEditor);
  RegisterComponentEditor(TcxHintStyleController, TcxHintStyleComponentEditor);
  RegisterComponentEditor(TcxListView, TcxContainerComponentEditor);
  RegisterComponentEditor(TcxMCListBox, TcxContainerComponentEditor);
  RegisterComponentEditor(TcxSplitter, TcxSplitterComponentEditor);
  RegisterComponentEditor(TcxTreeView, TcxContainerComponentEditor);
  RegisterComponentEditor(TdxTaskbarProgress, TcxCustomEditorsLibraryComponentEditor);
  RegisterComponentEditor(TdxRangeControl, TcxCustomEditorsLibraryComponentEditor);
  RegisterComponentEditor(TdxUIAdornerManager, TdxUIAdornerManagerComponentEditor);

  RegisterPropertyEditor(TypeInfo(TNotifyEvent), TdxRangeControl,
    'ClientPropertiesEvents', TdxRangeControlClientEventsProperty);
end;

initialization
  RegisterEditRepositoryItems;
  dxUIAdornerManagerDesignHelperClass := TdxUIAdornerManagerDesignHelper;

finalization
  UnregisterEditRepositoryItems;

end.
