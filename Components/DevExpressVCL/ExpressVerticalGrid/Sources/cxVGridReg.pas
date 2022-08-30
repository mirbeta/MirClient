{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressVerticalGrid                                      }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSVERTICALGRID AND ALL           }
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
unit cxVGridReg;

{$I cxVer.inc}

interface

procedure Register;

implementation

uses
  DesignEditors, DesignIntf, VCLEditors,
  Classes, Controls, ImgList, cxScrollBar, cxVGrid, cxEdit, cxStyles,
  cxEditRepositoryItems, cxEditPropEditors, cxVGridEditor, cxClasses, cxGraphics,
  cxInplaceContainerReg, cxPropEditors, DB, cxDBVGrid, cxOI, SysUtils, Forms,
  cxVGridPredefinedStyles, cxVGridLayoutEditor, TypInfo,
  cxVGridConverter, cxImportDialog, dxCore, dxCoreReg, cxLibraryReg, cxExportVGLink,
  cxVGridStyleSheetPreview,
  cxDataControllerConditionalFormatting;

const
  cxVGridProductName = 'ExpressVerticalGrid';

type
  TcxCustomRowAccess = class(TcxCustomRow);

  { TcxVerticalGridStylesEventsProperty }

  TcxVerticalGridStylesEventsProperty = class(TcxNestedEventProperty)
  protected
    function GetInstance: TPersistent; override;
  end;

  { TcxEditorRowPropertiesEventsProperty }

  TcxEditorRowPropertiesEventsProperty = class(TcxNestedEventProperty)
  protected
    function GetInstance: TPersistent; override;
  end;

  { TcxEditPropertiesEventsProperty }

  TcxEditPropertiesEventsProperty = class(TcxNestedEventProperty)
  protected
    function GetInstance: TPersistent; override;
  end;

  { TcxCollectionItemEditPropertiesEventsProperty }

  TcxCollectionItemEditPropertiesEventsProperty = class(TcxNestedEventProperty)
  protected
    function GetInstance: TPersistent; override;
  end;

  { TcxVirtualVerticalGridNavigatorEventsProperty }

  TcxVirtualVerticalGridNavigatorEventsProperty = class(TcxNestedEventProperty)
  protected
    function GetInstance: TPersistent; override;
  end;

  { TcxCustomRowImageIndexProperty }

  TcxCustomRowPropertiesImageIndexProperty = class(TImageIndexProperty)
  public
    function GetImages: TCustomImageList; override;
  end;

  { TcxCustomVerticalGridComponentEditor }

  TcxCustomVerticalGridComponentEditor = class(TdxComponentEditor)
  protected
    function GetProductName: string; override;
  end;

  { TcxVerticalGridComponentEditor }

  TcxVerticalGridComponentEditor = class(TcxCustomVerticalGridComponentEditor)
  private
    function GetVerticalGrid: TcxCustomVerticalGrid;
  protected
    function InternalGetVerb(AIndex: Integer): string; override;
    function InternalGetVerbCount: Integer; override;
    procedure InternalExecuteVerb(AIndex: Integer); override;
  public
    property VerticalGrid: TcxCustomVerticalGrid read GetVerticalGrid;
  end;

  { TcxDBVerticalGridComponentEditor }

  TcxDBVerticalGridComponentEditor = class(TcxVerticalGridComponentEditor)
  protected
    procedure DoLinkTo(AObject: TObject); override;
    function GetLinkToItemCaption: string; override;
    function GetLinkToTypeClass: TClass; override;
    function IsLinkable: Boolean; override;

    procedure PrepareLinkableSubItem(const AItem: TDesignMenuItem); override;
  end;

  { TcxRTTIInspectorComponentEditor }

  TcxRTTIInspectorComponentEditor = class(TcxCustomVerticalGridComponentEditor)
  private
    function GetInspector: TcxCustomRTTIInspector;
  protected
    function InternalGetVerb(AIndex: Integer): string; override;
    function InternalGetVerbCount: Integer; override;
    procedure InternalExecuteVerb(AIndex: Integer); override;
  public
    property Inspector: TcxCustomRTTIInspector read GetInspector;
  end;

  { TcxDBVerticalGridItemDataBindingFieldNameProperty }

  TcxDBVerticalGridItemDataBindingFieldNameProperty = class(TFieldNameProperty)
  public
    function GetDataSource: TDataSource; override;
  end;

  { TcxOIPropertyEditor }

  TcxOIPropertyEditor = class(TPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

{ TcxVerticalGridStylesEventsProperty }

function TcxVerticalGridStylesEventsProperty.GetInstance: TPersistent;
begin
  if (GetComponent(0) is TcxCustomVerticalGrid) then
    Result := TcxCustomVerticalGrid(GetComponent(0)).Styles
  else
    Result := nil;
end;

{ TcxEditorRowPropertiesEventsProperty }

function TcxEditorRowPropertiesEventsProperty.GetInstance: TPersistent;
begin
  if (GetComponent(0) is TcxCustomEditorRow) then
    Result := TcxCustomRowAccess(GetComponent(0)).FProperties
  else
    Result := nil;
end;

{ TcxEditPropertiesEventsProperty }

function TcxEditPropertiesEventsProperty.GetInstance: TPersistent;
begin
  if (GetComponent(0) is TcxCustomEditorRow) then
    Result := TcxCustomEditorRowProperties(TcxCustomRowAccess(GetComponent(0)).FProperties).EditProperties
  else
    Result := nil;
end;

{ TcxCollectionItemEditPropertiesEventsProperty }

function TcxCollectionItemEditPropertiesEventsProperty.GetInstance: TPersistent;
begin
  if (GetComponent(0) is TcxCollectionItemEditorRowProperties) then
    Result := TcxCollectionItemEditorRowProperties(GetComponent(0)).EditProperties
  else
    Result := nil;
end;

{ TcxVirtualVerticalGridNavigatorEventsProperty }

function TcxVirtualVerticalGridNavigatorEventsProperty.GetInstance: TPersistent;
begin
  Result := TcxVirtualVerticalGrid(GetComponent(0)).Navigator;
end;

{ TcxCustomRowImageIndexProperty }

function TcxCustomRowPropertiesImageIndexProperty.GetImages: TCustomImageList;
begin
  if GetComponent(0) is TcxCustomRowProperties then
    Result := TcxCustomRowProperties(GetComponent(0)).Row.VerticalGrid.Images
  else
    Result := nil;
end;

{ TcxCustomVerticalGridComponentEditor }

function TcxCustomVerticalGridComponentEditor.GetProductName: string;
begin
  Result := cxVGridProductName;
end;

{ TcxVerticalGridComponentEditor }

function TcxVerticalGridComponentEditor.InternalGetVerb(AIndex: Integer): string;
begin
  case AIndex of
    0: Result := 'Edit...';
    1: Result := 'Layout editor...';
    2: Result := 'Import...';
    3: Result := 'Restore default values';
    4:
      begin
        if VerticalGrid.ConditionalFormatting.CanShowRulesManagerDialog then
          Result := 'Conditional Formatting...'
        else
          Result := '';
      end;
  end;
end;

function TcxVerticalGridComponentEditor.InternalGetVerbCount: Integer;
begin
  Result := 4;
  if VerticalGrid.ConditionalFormatting.CanShowRulesManagerDialog then
    Inc(Result);
end;

procedure TcxVerticalGridComponentEditor.InternalExecuteVerb(AIndex: Integer);
begin
  case AIndex of
    0: ShowVerticalGridEditor(Designer, VerticalGrid);
    1: ShowVerticalGridLayoutEditor(VerticalGrid,
      Format('Layout editor - %s', [VerticalGrid.Name]));
    2: if Component is TcxDBVerticalGrid then
         ShowImportDialog(Designer, Component, cxDBVGGroupConverterName, False)
       else if Component is TcxUnboundVerticalGrid then
         ShowImportDialog(Designer, Component, cxVGGroupConverterName, False);
    3: VerticalGrid.RestoreDefaults;
    4: VerticalGrid.ConditionalFormatting.ShowRulesManagerDialog;
  end;
end;

function TcxVerticalGridComponentEditor.GetVerticalGrid: TcxCustomVerticalGrid;
begin
  Result := Component as TcxCustomVerticalGrid;
end;

{ TcxDBVerticalGridComponentEditor }

procedure TcxDBVerticalGridComponentEditor.DoLinkTo(AObject: TObject);
var
  ADataController: TcxDBVerticalGridDataController;
  AVerticalGrid: TcxDBVerticalGrid;
begin
  AVerticalGrid := (Component as TcxDBVerticalGrid);
  ADataController := AVerticalGrid.DataController;
  if ADataController.DataSource <> AObject then
  begin
    AVerticalGrid.ClearRows;
    ADataController.DataSource := (AObject as TDataSource);
    ADataController.CreateAllItems;
    Designer.Modified;
  end;
end;

function TcxDBVerticalGridComponentEditor.GetLinkToItemCaption: string;
begin
  Result := 'Link to DataSource';
end;

function TcxDBVerticalGridComponentEditor.GetLinkToTypeClass: TClass;
begin
  Result := TDataSource;
end;

function TcxDBVerticalGridComponentEditor.IsLinkable: Boolean;
begin
  Result := True;
end;

procedure TcxDBVerticalGridComponentEditor.PrepareLinkableSubItem(const AItem: TDesignMenuItem);
begin
  AItem.Checked := (Component as TcxDBVerticalGrid).DataController.DataSource = Designer.GetComponent(AItem.Caption);
end;

{ TcxRTTIInspectorComponentEditor }

function TcxRTTIInspectorComponentEditor.InternalGetVerb(AIndex: Integer): string;
begin
  case AIndex of
    0: Result := 'Import...';
    1: Result := 'Restore default values';
  end;
end;

function TcxRTTIInspectorComponentEditor.InternalGetVerbCount: Integer;
begin
  Result := 2;
end;

procedure TcxRTTIInspectorComponentEditor.InternalExecuteVerb(AIndex: Integer);
begin
  case AIndex of
    0: ShowImportDialog(Designer, Inspector, cxRTTIVGGroupConverterName, False);
    1: Inspector.RestoreDefaults;
  end;
end;

function TcxRTTIInspectorComponentEditor.GetInspector: TcxCustomRTTIInspector;
begin
  Result := Component as TcxCustomRTTIInspector;
end;

{ TcxDBVerticalGridItemDataBindingFieldNameProperty }

function TcxDBVerticalGridItemDataBindingFieldNameProperty.GetDataSource: TDataSource;
begin
  Result := TcxDBVerticalGridItemDataBinding(GetComponent(0)).DataController.DataSource;
end;

{ TcxOIPropertyEditor }

function TcxOIPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList];
end;

procedure TcxOIPropertyEditor.GetValues(Proc: TGetStrProc);

  function IsValidComponent(AComponent: TComponent): Boolean;
  begin
    Result := (AComponent.Name <> '') and
      IsValidInspectedObject(AComponent, GetComponent(0) as TcxCustomRTTIInspector);
  end;

  procedure PopulateList(AComponent: TComponent);
  var
    I: Integer;
  begin
     if IsValidComponent(AComponent) then
       Proc(AComponent.Name);
     for i := 0 to AComponent.ComponentCount - 1 do
       PopulateList(AComponent.Components[I]);
  end;

begin
  PopulateList(Designer.GetRoot);
end;

function TcxOIPropertyEditor.GetValue: string;
var
  OI: TcxCustomRTTIInspector;
begin
  Result := '';
  OI := TcxCustomRTTIInspector(GetComponent(0));
  if OI.InspectedObject <> nil then
  begin
    if OI.InspectedObject is TComponent then
      Result := OI.InspectedObject.GetNamePath;
  end;
end;

procedure TcxOIPropertyEditor.SetValue(const Value: string);

   function GetValueComponent: TComponent;
   begin
     if CompareText(Value, Designer.GetRoot.Name) = 0 then
       Result := Designer.GetRoot
     else Result := Designer.GetRoot.FindComponent(Value);
     if Result = nil then
       Result := Application.FindComponent(Value);
     if Result = GetComponent(0) then
       Result := nil;
   end;

var
  OI: TcxCustomRTTIInspector;
begin
  OI := TcxCustomRTTIInspector(GetComponent(0));
  if Value = '' then
    OI.InspectedObject := nil
  else
  begin
    if GetValueComponent <> nil then
      OI.InspectedObject := GetValueComponent;
  end;
  Modified;
end;

type
  TcxVerticalGridSelectionEditor = class(TSelectionEditor)
  protected
    ComponentsList: TStringList;
  public
    procedure AddComponent(const Name: string);
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

procedure TcxVerticalGridSelectionEditor.RequiresUnits(Proc: TGetStrProc);

  procedure AddPropertiesUnitName(AProperties: TcxCustomEditProperties);
  begin
    if AProperties <> nil then
      Proc(cxGetUnitName(AProperties.ClassType));
  end;

  procedure CheckRow(ARow: TcxCustomRow);
  var
    AProperties: TcxCustomRowProperties;
    I: Integer;
  begin
    AProperties := TcxCustomRowAccess(ARow).FProperties;
    if AProperties is TcxCustomEditorRowProperties then
      with TcxCustomEditorRowProperties(AProperties) do
        AddPropertiesUnitName(EditProperties)
    else
      if AProperties is TcxDBMultiEditorRowProperties then
        with TcxDBMultiEditorRowProperties(AProperties) do
          for I := 0 to Editors.Count - 1 do
            AddPropertiesUnitName(Editors[I].EditProperties)
      else
        if AProperties is TcxMultiEditorRowProperties then
          with TcxMultiEditorRowProperties(AProperties) do
            for I := 0 to Editors.Count - 1 do
              AddPropertiesUnitName(Editors[I].EditProperties)
  end;

var
  AComponent: TComponent;
  I, J: Integer;
begin
  Proc('cxStyles');
  Proc('cxGraphics');
  Proc('cxEdit');
  ComponentsList := TStringList.Create;
  try
    Designer.GetComponentNames(GetTypeData(PTypeInfo(TcxCustomVerticalGrid.ClassInfo)), AddComponent);
    for I := 0 to ComponentsList.Count - 1 do
    begin
      AComponent := Designer.GetComponent(ComponentsList[I]);
      if AComponent is TcxCustomVerticalGrid then
        with TcxCustomVerticalGrid(AComponent) do
          for J := 0 to Rows.Count - 1 do CheckRow(Rows[J]);
    end;
  finally
    ComponentsList.Free;
  end;
  TcxDataControllerConditionalFormattingRulesManagerDialogProvider.RequiresRulesManagerDialogUnits(Designer.Root, Proc);
end;

procedure TcxVerticalGridSelectionEditor.AddComponent(const Name: string);
begin
  ComponentsList.Add(Name);
end;

procedure Register;
begin
  ForceDemandLoadState(dlDisable);

  RegisterComponents(dxCoreLibraryProductPage, [TcxVerticalGrid,
    TcxVirtualVerticalGrid, TcxDBVerticalGrid, TcxRTTIInspector]);
  //rows
  RegisterClasses([TcxCustomVerticalGrid, TcxCategoryRow, TcxEditorRow,
    TcxMultiEditorRow, TcxVerticalGridStyleSheet]);
  RegisterNoIcon([TcxCategoryRow, TcxEditorRow, TcxMultiEditorRow,
    TcxVerticalGridStyleSheet]);
  //row properties
  RegisterClasses([TcxCaptionRowProperties, TcxEditorRowProperties,
    TcxMultiEditorRowProperties]);
  RegisterPropertyEditor(TypeInfo(string), TcxCustomEditorRowProperties, 'EditPropertiesClassName', nil);
  RegisterPropertyEditor(TypeInfo(TcxCustomEditProperties), TcxCustomEditorRowProperties, 'EditProperties', TcxInplaceEditContainerPropertiesProperty);
  RegisterPropertyEditor(TypeInfo(TcxImageIndex), TcxCustomRowProperties, 'ImageIndex', TcxCustomRowPropertiesImageIndexProperty);
  RegisterPropertyEditor(TypeInfo(TNotifyEvent), TcxCustomVerticalGrid,
    'StylesEvents', TcxVerticalGridStylesEventsProperty);
  RegisterPropertyEditor(TypeInfo(TNotifyEvent), TcxCustomEditorRow,
    'PropertiesEvents', TcxEditorRowPropertiesEventsProperty);
  RegisterPropertyEditor(TypeInfo(TNotifyEvent), TcxCustomEditorRow,
    'EditPropertiesEvents', TcxEditPropertiesEventsProperty);
  RegisterPropertyEditor(TypeInfo(TNotifyEvent), TcxCollectionItemEditorRowProperties,
    'EditPropertiesEvents', TcxCollectionItemEditPropertiesEventsProperty);
  RegisterPropertyEditor(TypeInfo(TNotifyEvent), TcxVirtualVerticalGrid,
    'NavigatorEvents', TcxVirtualVerticalGridNavigatorEventsProperty);

  RegisterComponentEditor(TcxVerticalGrid, TcxVerticalGridComponentEditor);
  RegisterComponentEditor(TcxVirtualVerticalGrid, TcxVerticalGridComponentEditor);
  RegisterPropertyEditor(TypeInfo(string), TcxDBVerticalGridItemDataBinding, 'FieldName', TcxDBVerticalGridItemDataBindingFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TcxDBVerticalGridItemDataBinding, 'FieldName', TcxDBVerticalGridItemDataBindingFieldNameProperty);
  //DB
  RegisterClasses([TcxDBEditorRow, TcxDBMultiEditorRow]);
  RegisterNoIcon([TcxDBEditorRow, TcxDBMultiEditorRow]);
  RegisterClasses([TcxDBEditorRowProperties, TcxDBMultiEditorRowProperties]);
  RegisterComponentEditor(TcxDBVerticalGrid, TcxDBVerticalGridComponentEditor);
  RegisterPropertyEditor(TypeInfo(string), TcxDBVerticalGridItemDataBinding, 'FieldName', TcxDBVerticalGridItemDataBindingFieldNameProperty);
  //RTTI
  RegisterComponentEditor(TcxCustomRTTIInspector, TcxRTTIInspectorComponentEditor);
  RegisterPropertyEditor(TypeInfo(TPersistent), TcxCustomRTTIInspector, 'InspectedObject',
    TcxOIPropertyEditor);
  RegisterSelectionEditor(TcxCustomVerticalGrid, TcxVerticalGridSelectionEditor);

  RegisterPropertyEditor(TypeInfo(TdxDefaultBoolean), TcxvgMultiRecordsOptionsData, 'MultiThreadedFiltering', TdxDataControllerMultithreadedFilteringPropertyEditor);
end;

initialization
  RegisterStyleSheetClass(TcxVerticalGridStyleSheet);

finalization
  UnRegisterStyleSheetClass(TcxVerticalGridStyleSheet);

end.
