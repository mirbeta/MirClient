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

unit cxEditPropEditors;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
{$IFNDEF CBUILDER10}
  DBReg,
{$ENDIF}
  DesignEditors, DesignIntf, VCLEditors,
  Types,
  WideStrings,
  Windows, Forms, Classes, Controls, DB, Graphics, ImgList, TypInfo, cxContainer, ColnEdit,
  cxDateUtils, cxDataStorage, cxEdit, cxEditRepositoryItems, cxLookAndFeels, cxPropEditors,
  cxDesignWindows, dxCoreReg, cxLibraryReg, ActnList, dxAlertWindow, dxBreadcrumbEdit, dxCoreGraphics,
  dxBarCode, dxBarCodeUtils;

const
  cxEditorsLibraryProductName = 'ExpressEditors Library';
  cxEditorsLibraryProductPage = 'Express Editors';
  cxEditorsDBLibraryProductPage = 'Express DBEditors';
  cxEditorsUtilitiesProductPage = cxLibraryUtilitiesProductPage;

type
{$IFDEF DELPHI15}
  TdxValues = TStrings;
  TdxValueList = TStringList;
{$ELSE}
  TdxValues = TWideStrings;
  TdxValueList = TWideStringList;
{$ENDIF}

  { TDBStringProperty }

  TDBStringProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValueList(AList: TStrings); virtual;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

{$IFDEF CBUILDER10}
  { TcxDataFieldProperty }

  TcxDataFieldProperty = class(TDBStringProperty)
  public
    function GetDataSourcePropName: string; virtual;
    procedure GetValueList(AList: TStrings); override;
  end;
{$ELSE}
  TcxDataFieldProperty = class(TDataFieldProperty)
  public
    procedure GetValueList(List: TdxValues); override;
  end;
{$ENDIF}

  { TcxDateProperty }

  TcxDateProperty = class(DesignEditors.TDateProperty)
  public
    function GetValue: string; override;
  end;

  { TcxValueTypeProperty }

  TcxValueTypeProperty = class(TStringProperty)
  protected
    function IsValueTypeClassValid(AValueTypeClass: TcxValueTypeClass): Boolean; virtual;
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  { TFieldNameProperty }

  TFieldNameProperty = class(TDBStringProperty)
  public
    function GetDataSource: TDataSource; virtual;
    function GetDataSourcePropName: string; virtual;
    procedure GetValueList(AList: TStrings); override;
  end;

  { TcxCustomEditorsLibraryComponentEditor }

  TcxCustomEditorsLibraryComponentEditor = class(TdxComponentEditor)
  protected
    function GetProductName: string; override;
  end;

  { TcxEditComponentEditor }

  TcxEditComponentEditor = class(TcxCustomEditorsLibraryComponentEditor)
  private
    function GetEdit: TcxCustomEdit;
    function GetIndex(AIndex: Integer): Integer;
  protected
    function InternalGetVerb(Index: Integer): string; override;
    function InternalGetVerbCount: Integer; override;
    procedure InternalExecuteVerb(AIndex: Integer); override;

    function GetEditVerb: string; virtual;
    function HasEditor: Boolean; virtual;
    procedure RunEditor; virtual;
  public
    procedure Edit; override;
  end;

  { TcxEditRepositoryItemProperty }

  TcxEditRepositoryItemProperty = class(TComponentProperty)
  private
    FStrProc: TGetStrProc;
    procedure StrProc(const S: string);
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  { TcxLookupEditListSourceProperty }

  TcxLookupEditListSourceProperty = class(TcxDataFieldProperty)
  public
    function GetDataSourcePropName: string; override;
  end;

  { TcxEditPropertiesEventEditor }

  TcxEditPropertiesEventEditor = class(TcxNestedEventProperty)
  protected
    function GetInstance: TPersistent; override;
  public
    function GetName: string; override;
  end;

  { TcxEditRepositoryItemPropertiesEventEditor }

  TcxEditRepositoryItemPropertiesEventEditor = class(TcxNestedEventProperty)
  protected
    function GetInstance: TPersistent; override;
  public
    function GetName: string; override;
  end;

  { TcxNavigatorButtonsEventEditor }

  TcxNavigatorButtonsEventEditor = class(TcxNestedEventProperty)
  protected
    function GetInstance: TPersistent; override;
  public
    function GetName: string; override;
  end;

  { TcxNavigatorInfoPanelEventEditor }

  TcxNavigatorInfoPanelEventEditor = class(TcxNestedEventProperty)
  protected
    function GetInstance: TPersistent; override;
  public
    function GetName: string; override;
  end;

  { TcxGetPropertiesImageIndexProperty }

  TcxGetPropertiesImageIndexProperty = class(TImageIndexProperty)
  public
    function GetImages: TCustomImageList; override;
  end;

  { TcxGetItemImageIndexProperty }

  TcxGetItemImageIndexProperty = class(TImageIndexProperty)
  public
    function GetImages: TCustomImageList; override;
  end;

  { TdxGalleryControlItemImageIndexProperty }

  TdxGalleryControlItemImageIndexProperty = class(TImageIndexProperty)
  public
    function GetImages: TCustomImageList; override;
  end;

  { TcxEditorsLibraryComponentEditorEx }

  TcxEditorsLibraryComponentEditorEx = class(TcxCustomEditorsLibraryComponentEditor)
  protected
    function GetEditItemCaption: string; virtual;
    procedure ExecuteEditAction; virtual; abstract;

    function InternalGetVerb(AIndex: Integer): string; override;
    function InternalGetVerbCount: Integer; override;
    procedure InternalExecuteVerb(AIndex: Integer); override;
  end;

  { TcxEditRepositoryComponentEditor }

  TcxEditRepositoryComponentEditor = class(TcxEditorsLibraryComponentEditorEx)
  private
    function GetEditRepository: TcxEditRepository;
  protected
    procedure ExecuteEditAction; override;
  end;

  { TcxEditMaskProperty }

  TcxEditMaskProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  { TcxTextProperty }

  TcxTextProperty = class(TStringProperty)
  private
    function CanShowDialog: Boolean;
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  { TGraphicClassNameProperty }

  TGraphicClassNameProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TcxControlSelectionEditor = class(TSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

  TcxCustomEditSelectionEditor = class(TSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

  TcxButtonSelectionEditor = class(TSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

  { TcxFilterControlComponentEditor }

  TcxFilterControlComponentEditor = class(TcxDefaultEditor)
  protected
    function GetProductName: string; override;
  end;

  { TcxNavigatorControlProperty }

  TcxNavigatorControlProperty = class(TComponentProperty)
  private
    FStrProc: TGetStrProc;
    procedure StrProc(const S: string);
  public
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  { TcxEditorsLibraryComponentEditor }

  TcxEditorsLibraryComponentEditor = class(TcxEditorsLibraryComponentEditorEx)
  public
    procedure Edit; override;
  end;

  { TcxEditorsLibraryComponentWithStylesEditor }

  TcxEditorsLibraryComponentWithStylesEditor = class(TcxEditorsLibraryComponentEditor)
  protected
    function GetEditItemCaption: string; override;
    procedure ExecuteEditAction; override;
    procedure RestoreStyles; virtual; abstract;
  end;

  { TcxEditorsLibraryComponentWithoutStylesEditor }

  TcxEditorsLibraryComponentWithoutStylesEditor = class(TcxEditorsLibraryComponentEditor)
  protected
    function GetEditItemCaption: string; override;
    procedure ExecuteEditAction; override;
    function GetLookAndFeel: TcxLookAndFeel; virtual; abstract;
  end;

  { TcxEditorsLibraryCXControlComponentEditor }

  TcxEditorsLibraryCXControlComponentEditor = class(TcxEditorsLibraryComponentWithoutStylesEditor)
  protected
    function GetLookAndFeel: TcxLookAndFeel; override;
  end;

  { TcxBreadcrumbEditComponentEditor }

  TcxBreadcrumbEditComponentEditor = class(TcxEditorsLibraryCXControlComponentEditor)
  protected
    function InternalGetVerb(AIndex: Integer): string; override;
    function InternalGetVerbCount: Integer; override;
    procedure InternalExecuteVerb(AIndex: Integer); override;
  public
    procedure Edit; override;
  end;

  { TdxBreadcrumbEditSelectedPathPropertyEditor }

  TdxBreadcrumbEditSelectedPathPropertyEditor = class(TStringProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

  { TdxBevelComponentEditor }

  TdxBevelComponentEditor = class(TcxEditorsLibraryComponentWithoutStylesEditor)
  protected
    function GetLookAndFeel: TcxLookAndFeel; override;
  end;

  { TcxEditStyleControllerEditor }

  TcxEditStyleControllerEditor = class(TcxEditorsLibraryComponentWithStylesEditor)
  protected
    procedure RestoreStyles; override;
  public
    procedure Edit; override;
  end;

  { TcxDefaultEditStyleControllerEditor }

  TcxDefaultEditStyleControllerEditor = class(TcxEditorsLibraryComponentWithStylesEditor)
  protected
    procedure RestoreStyles; override;
  public
    procedure Edit; override;
  end;

  { TcxCustomButtonComponentEditor }

  TcxCustomButtonComponentEditor = class(TcxEditorsLibraryComponentWithoutStylesEditor)
  protected
    function GetLookAndFeel: TcxLookAndFeel; override;
  end;

  { TdxAlertWindowComponentEditor }

  TdxAlertWindowComponentEditor = class(TcxEditorsLibraryComponentWithoutStylesEditor)
  protected
    function GetLookAndFeel: TcxLookAndFeel; override;
    function InternalGetVerb(AIndex: Integer): string; override;
    function InternalGetVerbCount: Integer; override;
    procedure InternalExecuteVerb(AIndex: Integer); override;
  public
    procedure Edit; override;
  end;

  { TcxRadioButtonComponentEditor }

  TcxRadioButtonComponentEditor = class(TcxEditorsLibraryComponentWithoutStylesEditor)
  protected
    function GetLookAndFeel: TcxLookAndFeel; override;
  end;

  { TcxContainerComponentEditor }

  TcxContainerComponentEditor = class(TcxEditorsLibraryComponentWithStylesEditor)
  protected
    procedure RestoreStyles; override;
  end;

  { TcxCustomNavigatorComponentEditor }

  TcxCustomNavigatorComponentEditor = class(TcxEditorsLibraryCXControlComponentEditor)
  protected
    function InternalGetVerb(AIndex: Integer): string; override;
    function InternalGetVerbCount: Integer; override;
    procedure InternalExecuteVerb(AIndex: Integer); override;
  public
    procedure Edit; override;
  end;

  { TdxGalleryControlComponentEditor }

  TdxGalleryControlComponentEditor = class(TcxEditorsLibraryComponentEditorEx)
  protected
    procedure ExecuteEditAction; override;
  end;

  { TdxSliderImageComponentEditor }

  TdxSliderImageComponentEditor = class(TcxEditorsLibraryCXControlComponentEditor)
  public
    procedure Edit; override;
  end;

  { TcxCustomImagePropertiesProperty }

  TcxCustomImagePropertiesProperty = class(TcxEditPropertiesEventEditor)
  private
    FProc: TGetPropProc;
    procedure GetPropProc(const Prop: IProperty);
  public
    procedure GetProperties(Proc: TGetPropProc); override;
  end;

  { TcxEditPropertiesAssignedValuesProperty }

  TcxEditPropertiesAssignedValuesProperty = class(TClassProperty)
  private
    FProc: TGetPropProc;
    procedure GetPropProc(const Prop: IProperty);
    function IsVisibleProperty(const APropertyName: string): Boolean;
  public
    procedure GetProperties(Proc: TGetPropProc); override;
    function GetValue: string; override;
  end;

  { TcxDefaultEditStyleControllerStyleProperty }

  TcxDefaultEditStyleControllerStyleProperty = class(TcxStyleControllerStyleProperty)
  protected
    function GetStyle: TcxContainerStyle; override;
    function IsPropertyVisible(const APropertyName: string): Boolean; override;
  end;

  { TcxNavigatorButtonImageIndexProperty }

  TcxNavigatorButtonImageIndexProperty = class(TImageIndexProperty)
  public
    function GetImages: TCustomImageList; override;
  end;

  { TcxNavigatorCustomButtonImageIndexProperty }

  TcxNavigatorCustomButtonImageIndexProperty = class(TImageIndexProperty)
  public
    function GetImages: TCustomImageList; override;
  end;

  { TcxEditButtonImageIndexProperty }

  TcxEditButtonImageIndexProperty = class(TImageIndexProperty)
  public
    function GetImages: TCustomImageList; override;
  end;

  { TcxButtonImageIndexProperty }

  TcxButtonImageIndexProperty = class(TImageIndexProperty)
  public
    function GetImages: TCustomImageList; override;
  end;

  { TdxAlertWindowButtonImageIndexProperty }

  TdxAlertWindowButtonImageIndexProperty = class(TImageIndexProperty)
  public
    function GetImages: TCustomImageList; override;
  end;

  { TdxBreadcrumbEditButtonImageIndexProperty }

  TdxBreadcrumbEditButtonImageIndexProperty = class(TImageIndexProperty)
  public
    function GetImages: TCustomImageList; override;
  end;

  { TdxBreadcrumbEditRecentPathImageIndexProperty }

  TdxBreadcrumbEditRecentPathImageIndexProperty = class(TImageIndexProperty)
  public
    function GetImages: TCustomImageList; override;
    function GetPathEditorProperties: TdxBreadcrumbEditPathEditorProperties;
  end;

  TcxEditGuidelines = class(TWinControlGuidelines)
  private
    function GetEdit: TcxCustomEdit;
  protected
    function GetCount: Integer; override;
    function GetDesignerGuideOffset(Index: Integer): Integer; override;
    function GetDesignerGuideType(Index: Integer): TDesignerGuideType; override;
    property Edit: TcxCustomEdit read GetEdit;
  end;

  { TdxDataControllerMultithreadedSortingPropertyEditor }

  TdxDataControllerMultithreadedSortingPropertyEditor = class(TdxDefaultBooleanPropertyEditor)
  protected
    function GetDefaultValue: Boolean; override;
  end;

  { TdxDataControllerMultithreadedFilteringPropertyEditor }

  TdxDataControllerMultithreadedFilteringPropertyEditor = class(TdxDefaultBooleanPropertyEditor)
  protected
    function GetDefaultValue: Boolean; override;
  end;

  { TdxAlphaColorPropertyEditor }

  TdxAlphaColorPropertyEditor = class(TOrdinalProperty,
    ICustomPropertyDrawing80,
    ICustomPropertyDrawing,
    ICustomPropertyListDrawing)
  private
    FSavedProc: TGetStrProc;
    procedure CalculatePreviewRect(var R: TRect; out APreviewRect: TRect);
    procedure DrawPreview(ACanvas: TCanvas; R: TRect; AColor: TdxAlphaColor; ASelected: Boolean);
    procedure FilteredGetStrProc(const S: string);
    function ValueToAlphaColor(const AValue: string): TdxAlphaColor;
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
    // ICustomPropertyDrawing
    procedure PropDrawName(ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
    procedure PropDrawValue(ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
    // ICustomPropertyListDrawing
    procedure ListDrawValue(const Value: string; ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
    procedure ListMeasureHeight(const Value: string; ACanvas: TCanvas; var AHeight: Integer);
    procedure ListMeasureWidth(const Value: string; ACanvas: TCanvas; var AWidth: Integer);
    // ICustomPropertyDrawing80
    function PropDrawNameRect(const ARect: TRect): TRect;
    function PropDrawValueRect(const ARect: TRect): TRect;
  end;

  { TdxColorDialogComponentEditor }

  TdxColorDialogComponentEditor = class(TcxEditorsLibraryComponentWithoutStylesEditor)
  protected
    function GetLookAndFeel: TcxLookAndFeel; override;
    procedure InternalExecuteVerb(AIndex: Integer); override;
    function InternalGetVerb(AIndex: Integer): string; override;
    function InternalGetVerbCount: Integer; override;
  public
    procedure Edit; override;
  end;

  { TdxActivityIndicatorPropertiesEditor }

  TdxActivityIndicatorPropertiesEditor = class(TClassProperty)
  strict private
    function HasSubProperties: Boolean;
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

  { TdxWheelPickerComponentEditor }

  TdxWheelPickerComponentEditor = class(TcxEditComponentEditor)
  protected
    function GetEditVerb: string; override;
    function HasEditor: Boolean; override;
    procedure RunEditor; override;
  end;

{ TdxWheelPickerWheelsPropertyEditor }

  TdxWheelPickerWheelsPropertyEditor = class(TPropertyEditor)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
  end;

  { TdxWheelPickerItemsPropertiesEditor }

  TdxWheelPickerItemIndexesEditor = class(TPropertyEditor)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetName: string; override;
    function GetValue: string; override;
  end;

  { TdxWheelPickerItemImageIndexProperty }

  TdxWheelPickerItemImageIndexProperty = class(TImageIndexProperty)
  public
    function GetImages: TCustomImageList; override;
  end;

  { TdxWheelPickerItemCollectionProperty }

  TdxWheelPickerItemCollectionProperty = class(TCollectionProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  { TdxSparklineEditComponentEditor }

  TdxSparklineEditComponentEditor = class(TcxEditComponentEditor)
  protected
    function GetEditVerb: string; override;
    function HasEditor: Boolean; override;
    procedure RunEditor; override;
  end;

  { TdxBarCodeSymbologyProperty }

  TdxBarCodeSymbologyProperty = class(TClassProperty)
  protected
    function HasSubProperties: Boolean;
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

  { TdxTokenEditTokenImageIndexProperty }

  TdxTokenEditTokenImageIndexProperty = class(TImageIndexProperty)
  public
    function GetImages: TCustomImageList; override;
  end;

  { TdxTokenEditComponentEditor }

  TdxTokenEditComponentEditor = class(TcxEditComponentEditor)
  protected
    function GetEditVerb: string; override;
    function HasEditor: Boolean; override;
    procedure RunEditor; override;
  end;

implementation

uses
  SysUtils, cxButtons, cxClasses, cxControls, cxEditMaskEditor,
  cxEditRepositoryEditor, cxImage, cxImageComboBox, cxListBox, cxMaskEdit,
  cxMaskEditTextEditor, cxNavigator, cxRadioGroup, dxCore, cxScrollBox,
  cxCalendar, dxBreadcrumbEditEditor, dxGalleryDesigner, dxGalleryControl, dxBevel,
  cxCustomData, cxGraphics, cxGeometry, dxGDIPlusClasses, Math, dxColorDialog, dxAlertWindowDesignTester,
  dxActivityIndicator, dxWheelPicker, dxWheelPickerItemIndexesEditor, dxSparkline, dxTokenEdit;

const
  cxEditRepositoryEditorVerb = 'Edit...';

  cxEditComponentEditorVerbA: array[0..3] of string = (
    cxEditRepositoryEditorVerb,
    '-',
    'Restore properties',
    'Restore styles');

  cxCustomEditControlEditorVerbA: array[0..1] of string = (
    'Restore LookAndFeel',
    'Restore Styles'
  );

  cxCustomNavigatorEditorVerb = 'Restore Buttons';

  ADefaultMethodParams: array[0..0] of TMethodParam =
    ((Flags: [pfAddress]; Name: 'Sender'; TypeName: 'TObject'));

  cxNavigatorButtonsOnButtonClickEventParams: array[0..2] of TMethodParam = (
    (Flags: [pfAddress]; Name: 'Sender'; TypeName: 'TObject'),
    (Flags: [pfAddress]; Name: 'AButtonIndex'; TypeName: 'Integer'),
    (Flags: [pfVar]; Name: 'ADone'; TypeName: 'Boolean')
  );

type
  TcxButtonImageOptionsAccess = class(TcxButtonImageOptions);
  TcxControlAccess = class(TcxControl);
  TcxCustomEditAccess = class(TcxCustomEdit);
  TcxCustomEditPropertiesAccess = class(TcxCustomEditProperties);
  TcxCustomMaskEditPropertiesAccess = class(TcxCustomMaskEditProperties);
  TcxCustomNavigatorAccess = class(TcxCustomNavigator);
  TdxBreadcrumbEditPathEditorPropertiesAccess = class(TdxBreadcrumbEditPathEditorProperties);
  TdxCustomActivityIndicatorAccess = class(TdxCustomActivityIndicator);
  TdxCustomBreadcrumbEditPropertiesAccess = class(TdxCustomBreadcrumbEditProperties);
  TdxCustomGalleryControlAccess = class(TdxCustomGalleryControl);
  TdxGalleryControlItemAccess = class(TdxGalleryControlItem);
  TdxWheelPickerItemAccess = class(TdxWheelPickerItem);
  TdxWheelPickerItemsAccess = class(TdxWheelPickerItems);
  TdxWheelPickerWheelAccess = class(TdxWheelPickerWheel);
  TdxWheelPickerWheelsAccess = class(TdxWheelPickerWheels);

{ TDBStringProperty }

function TDBStringProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paMultiSelect];
end;

procedure TDBStringProperty.GetValueList(AList: TStrings);
begin
end;

procedure TDBStringProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
  Values: TStringList;
begin
  Values := TStringList.Create;
  try
    GetValueList(Values);
    for I := 0 to Values.Count - 1 do Proc(Values[I]);
  finally
    Values.Free;
  end;
end;

{$IFDEF CBUILDER10}

{ TcxDataFieldProperty }

function TcxDataFieldProperty.GetDataSourcePropName: string;
begin
  Result := 'DataSource';
end;

procedure TcxDataFieldProperty.GetValueList(AList: TStrings);
var
  DataSource: TDataSource;
  AAggList: TStringList;
begin
  DataSource := GetObjectProp(GetComponent(0), GetDataSourcePropName) as TDataSource;
  if (DataSource <> nil) and (DataSource.DataSet <> nil) then
  begin
  {$WARN SYMBOL_DEPRECATED OFF}
    DataSource.DataSet.GetFieldNames(AList);
  {$WARN SYMBOL_DEPRECATED ON}
    if DataSource.DataSet.AggFields.Count > 0 then
    begin
      AAggList := TStringList.Create;
      try
        DataSource.DataSet.AggFields.GetFieldNames(AAggList);
        AList.AddStrings(AAggList);
      finally
        AAggList.Free;
      end;
    end;
  end;
end;

{$ELSE}
{ TcxDataFieldProperty }

procedure TcxDataFieldProperty.GetValueList(List: TdxValues);
var
  DataSource: TDataSource;
  AAggList: TdxValues;
begin
  DataSource := GetObjectProp(GetComponent(0), GetDataSourcePropName) as TDataSource;
  if (DataSource <> nil) and (DataSource.DataSet <> nil) then
  begin
    inherited GetValueList(List);
    if DataSource.DataSet.AggFields.Count > 0 then
    begin
      AAggList := TdxValueList.Create;
      try
        DataSource.DataSet.AggFields.GetFieldNames(AAggList);
        List.AddStrings(AAggList);
      finally
        AAggList.Free;
      end;
    end;
  end;
end;

{$ENDIF}

{ TcxDateProperty }

function TcxDateProperty.GetValue: string;
var
  DT: TDateTime;
begin
  DT := GetFloatValue;
  if DT <> NullDate then
    Result := inherited GetValue
  else
    Result := StringReplace(DateTimeToStr(MinDateTime), '1', '0', [rfReplaceAll])
end;

{ TcxValueTypeProperty }

function TcxValueTypeProperty.IsValueTypeClassValid(AValueTypeClass: TcxValueTypeClass): Boolean;
begin
  Result := True;
end;

function TcxValueTypeProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paMultiSelect, paSortList, paRevertable];
end;

procedure TcxValueTypeProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  for I := 0 to cxValueTypeClassList.Count - 1 do
    if IsValueTypeClassValid(cxValueTypeClassList[I]) then
      Proc(cxValueTypeClassList[I].Caption);
end;

function TFieldNameProperty.GetDataSource: TDataSource;
begin
  Result := GetObjectProp(GetComponent(0), GetDataSourcePropName) as TDataSource;
end;

function TFieldNameProperty.GetDataSourcePropName: string;
begin
  Result := 'DataSource';
end;

procedure TFieldNameProperty.GetValueList(AList: TStrings);
var
  ADataSource: TDataSource;
begin
  ADataSource := GetDataSource;
  if (ADataSource <> nil) and (ADataSource.DataSet <> nil) then
{$WARN SYMBOL_DEPRECATED OFF}
    ADataSource.DataSet.GetFieldNames(AList);
{$WARN SYMBOL_DEPRECATED ON}
end;

{ TcxCustomEditorsLibraryComponentEditor }

function TcxCustomEditorsLibraryComponentEditor.GetProductName: string;
begin
  Result := cxEditorsLibraryProductName;
end;

{ TcxEditComponentEditor }

procedure TcxEditComponentEditor.Edit;
var
  AEdit: TcxCustomEdit;
  AEventName: string;
  AInstance: TObject;
  AMethodName: string;
  AProperties: TcxCustomEditProperties;
begin
  AEdit := TcxCustomEdit(Component);
  AMethodName := Component.Name;
  if not(AEdit.InnerControl <> nil) and
    (GetPropInfo(PTypeInfo(AEdit.ClassInfo), 'OnClick') <> nil) then
  begin
    AMethodName := AMethodName + 'Click';
    AInstance := AEdit;
    AEventName := 'OnClick';
  end
  else
  begin
    AProperties := TcxCustomEditAccess(AEdit).Properties;
    if GetPropInfo(PTypeInfo(AProperties.ClassInfo), 'OnChange') <> nil then
    begin
      AMethodName := AMethodName + 'PropertiesChange';
      AInstance := AProperties;
      AEventName := 'OnChange';
    end
    else
      Exit;
  end;

  ShowEventMethod(Designer, AInstance, AEventName, AMethodName, ADefaultMethodParams);
end;

function TcxEditComponentEditor.InternalGetVerb(Index: Integer): string;
var
  AIndex: Integer;
begin
  AIndex := GetIndex(Index);
  if AIndex = 0 then
    Result := GetEditVerb
  else
    Result := cxEditComponentEditorVerbA[AIndex];
end;

function TcxEditComponentEditor.InternalGetVerbCount: Integer;
begin
  Result := Length(cxEditComponentEditorVerbA);
  if not HasEditor then
    Dec(Result, 2);
end;

procedure TcxEditComponentEditor.InternalExecuteVerb(AIndex: Integer);
begin
  AIndex := GetIndex(AIndex);
  if AIndex <> 1 then
  begin
    case AIndex of
      0:
        RunEditor;
      2:
        TcxCustomEditAccess(GetEdit).Properties.RestoreDefaults;
      3:
        GetEdit.RestoreStyles;
    end;
    Designer.Modified;
  end;
end;

function TcxEditComponentEditor.GetEditVerb: string;
begin
  Result := cxEditComponentEditorVerbA[0];
end;

function TcxEditComponentEditor.HasEditor: Boolean;
begin
  Result := False;
end;

procedure TcxEditComponentEditor.RunEditor;
begin
// do nothing;
end;

function TcxEditComponentEditor.GetEdit: TcxCustomEdit;
begin
  Result := Component as TcxCustomEdit;
end;

function TcxEditComponentEditor.GetIndex(AIndex: Integer): Integer;
begin
  Result := AIndex;
  if not HasEditor then
    Inc(Result, 2);
end;

{ TcxEditRepositoryItemProperty }

function TcxEditRepositoryItemProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes;
  if TcxCustomEdit(GetComponent(0)).RepositoryItem <> nil then
    Include(Result, paNotNestable);
end;

procedure TcxEditRepositoryItemProperty.GetValues(Proc: TGetStrProc);
begin
  FStrProc := Proc;
  Designer.GetComponentNames(GetTypeData(GetPropType), StrProc);
end;

procedure TcxEditRepositoryItemProperty.StrProc(const S: string);
var
  I: Integer;
  ARepositoryItemAcceptable: Boolean;
begin
  ARepositoryItemAcceptable := True;
  for I := 0 to PropCount - 1 do
  if not TcxCustomEdit(GetComponent(I)).IsRepositoryItemAcceptable(
    TcxEditRepositoryItem(Designer.GetComponent(S))) then
  begin
    ARepositoryItemAcceptable := False;
    Break;
  end;
  if ARepositoryItemAcceptable then
    FStrProc(S);
end;

{ TcxLookupEditListSourceProperty }

function TcxLookupEditListSourceProperty.GetDataSourcePropName: string;
begin
  Result := 'ListSource';
end;

{ TcxEditPropertiesEventEditor }

function TcxEditPropertiesEventEditor.GetName: string;
begin
  Result := 'Properties';
end;

function TcxEditPropertiesEventEditor.GetInstance: TPersistent;
begin
  Result := TcxCustomEditAccess(GetComponent(0)).Properties;
end;

{ TcxEditRepositoryItemPropertiesEventEditor }

function TcxEditRepositoryItemPropertiesEventEditor.GetName: string;
begin
  Result := 'Properties';
end;

function TcxEditRepositoryItemPropertiesEventEditor.GetInstance: TPersistent;
begin
  Result := TcxEditRepositoryItem(GetComponent(0)).Properties;
end;

{ TcxNavigatorButtonsEventEditor }

function TcxNavigatorButtonsEventEditor.GetName: string;
begin
  Result := 'Buttons';
end;

function TcxNavigatorButtonsEventEditor.GetInstance: TPersistent;
begin
  Result := TcxCustomNavigatorAccess(GetComponent(0)).CustomButtons;
end;

{ TcxNavigatorInfoPanelEventEditor }

function TcxNavigatorInfoPanelEventEditor.GetName: string;
begin
  Result := 'InfoPanel';
end;

function TcxNavigatorInfoPanelEventEditor.GetInstance: TPersistent;
begin
  Result := TcxCustomNavigatorAccess(GetComponent(0)).CustomInfoPanel;
end;

{ TcxGetPropertiesImageIndexProperty }

function TcxGetPropertiesImageIndexProperty.GetImages: TCustomImageList;
begin
  Result := nil;
  if GetComponent(0) is TcxImageComboBoxProperties then
    Result := TcxImageComboBoxProperties(GetComponent(0)).Images;
end;

{ TcxGetItemImageIndexProperty }

function TcxGetItemImageIndexProperty.GetImages: TCustomImageList;
begin
  Result := nil;
  if GetComponent(0) is TcxImageComboBoxItem then
  begin
    Result := TcxImageComboBoxProperties(TcxImageComboBoxItems(
      TcxImageComboBoxItem(GetComponent(0)).Collection).Owner).LargeImages;
    if Result = nil then
      Result := TcxImageComboBoxProperties(TcxImageComboBoxItems(
        TcxImageComboBoxItem(GetComponent(0)).Collection).Owner).Images;
  end;
end;

{ TdxGalleryControlItemImageIndexProperty }

function TdxGalleryControlItemImageIndexProperty.GetImages: TCustomImageList;
begin
  Result := nil;
  if GetComponent(0) is TdxGalleryControlItem then
    Result := TdxCustomGalleryControlAccess(TdxGalleryControlItemAccess(GetComponent(0)).GalleryControl).Images;
end;

{ TcxEditorsLibraryComponentEditorEx }

function TcxEditorsLibraryComponentEditorEx.GetEditItemcaption: string;
begin
  Result := cxEditRepositoryEditorVerb;
end;

function TcxEditorsLibraryComponentEditorEx.InternalGetVerb(AIndex: Integer): string;
begin
  Result := GetEditItemCaption;
end;

function TcxEditorsLibraryComponentEditorEx.InternalGetVerbCount: Integer;
begin
  Result := 1;
end;

procedure TcxEditorsLibraryComponentEditorEx.InternalExecuteVerb(AIndex: Integer);
begin
  ExecuteEditAction;
end;

{ TcxEditRepositoryComponentEditor }

procedure TcxEditRepositoryComponentEditor.ExecuteEditAction;
begin
  ShowEditRepositoryEditor(Designer, GetEditRepository);
end;

function TcxEditRepositoryComponentEditor.GetEditRepository: TcxEditRepository;
begin
  Result := Component as TcxEditRepository
end;

{ TcxEditMaskProperty }

function TcxEditMaskProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paMultiSelect];
end;

procedure TcxEditMaskProperty.Edit;
var
  ADialog: TcxEditMaskEditorDlg;
  AMaskEditProperties: TcxCustomMaskEditPropertiesAccess;
  APrevEditMask: string;
  APrevMaskKind: TcxEditMaskKind;
  I: Integer;
begin
  AMaskEditProperties := TcxCustomMaskEditPropertiesAccess(GetComponent(0));
  APrevEditMask := AMaskEditProperties.EditMask;
  APrevMaskKind := AMaskEditProperties.MaskKind;
  ADialog := TcxEditMaskEditorDlg.Create(Application);
  try
    ADialog.MaskEditProperties := AMaskEditProperties;
    if ADialog.ShowModal = mrOk then
      for I := 1 to PropCount - 1 do
        with TcxCustomMaskEditPropertiesAccess(GetComponent(I)) do
        begin
          MaskKind := AMaskEditProperties.MaskKind;
          EditMask := AMaskEditProperties.EditMask;
        end;
    if (APrevMaskKind <> AMaskEditProperties.MaskKind) or
      (APrevEditMask <> AMaskEditProperties.EditMask) then
        Designer.Modified;
  finally
    ADialog.Free;
  end;
end;

{ TcxTextProperty }

function TcxTextProperty.GetAttributes: TPropertyAttributes;
begin
  if CanShowDialog then
    Result := [paDialog]
  else
    Result := [paMultiSelect];
end;

procedure TcxTextProperty.Edit;
var
  ADialog: TcxMaskEditTextEditorDlg;
begin
  ADialog := TcxMaskEditTextEditorDlg.Create(Application);
  try
    ADialog.MaskEdit := TcxCustomMaskEdit(GetComponent(0));
    ADialog.ShowModal;
  finally
    ADialog.Free;
  end;
end;

function TcxTextProperty.CanShowDialog: Boolean;
begin
  Result := (PropCount = 1) and
    TcxCustomMaskEdit(GetComponent(0)).ActiveProperties.IsMasked;
end;

{ TGraphicClassNameProperty }

function TGraphicClassNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paRevertable];
end;

procedure TGraphicClassNameProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  for i := 0 to GetRegisteredGraphicClasses.Count - 1 do
    Proc(TClass(GetRegisteredGraphicClasses[I]).ClassName);
end;

{ TcxControlSelectionEditor }

procedure TcxControlSelectionEditor.RequiresUnits(Proc: TGetStrProc);
begin
  Proc('cxGraphics');
  Proc('cxControls');
  Proc('cxLookAndFeels');
  Proc('cxLookAndFeelPainters');
end;

{ TcxCustomEditSelectionEditor }

procedure TcxCustomEditSelectionEditor.RequiresUnits(Proc: TGetStrProc);
begin
  Proc('cxContainer');
  Proc('cxEdit');
end;

{ TcxButtonSelectionEditor }

procedure TcxButtonSelectionEditor.RequiresUnits(Proc: TGetStrProc);
begin
  Proc('cxGraphics');
  Proc('cxLookAndFeels');
  Proc('cxLookAndFeelPainters');
{$IFDEF DELPHI16}
  Proc('Vcl.Menus');
{$ELSE}
  Proc('Menus');
{$ENDIF}
end;

{ TcxFilterControlComponentEditor }

function TcxFilterControlComponentEditor.GetProductName: string;
begin
  Result := cxEditorsLibraryProductName;
end;

{ TcxNavigatorControlProperty }

procedure TcxNavigatorControlProperty.GetValues(Proc: TGetStrProc);
begin
  FStrProc := Proc;
  Designer.GetComponentNames(GetTypeData(GetPropType), StrProc);
end;

procedure TcxNavigatorControlProperty.StrProc(const S: string);
var
  AComponent: TComponent;
begin
  AComponent := Designer.GetComponent(S);
  if (AComponent <> nil) and Supports(AComponent, IcxNavigator) then
    FStrProc(S);
end;

{ TcxEditorsLibraryComponentEditor }

procedure TcxEditorsLibraryComponentEditor.Edit;
begin
  ShowEventMethod(Designer, Component, 'OnClick', Component.Name + 'Click',
    ADefaultMethodParams);
end;

{ TcxEditorsLibraryComponentWithStylesEditor }

function TcxEditorsLibraryComponentWithStylesEditor.GetEditItemCaption: string;
begin
  Result := cxCustomEditControlEditorVerbA[1];
end;

procedure TcxEditorsLibraryComponentWithStylesEditor.ExecuteEditAction;
begin
  RestoreStyles;
  Designer.Modified;
end;

{ TcxEditorsLibraryComponentWithoutStylesEditor }

function TcxEditorsLibraryComponentWithoutStylesEditor.GetEditItemCaption: string;
begin
  Result := cxCustomEditControlEditorVerbA[0];
end;

procedure TcxEditorsLibraryComponentWithoutStylesEditor.ExecuteEditAction;
begin
  if GetLookAndFeel.AssignedValues <> [] then
  begin
    GetLookAndFeel.Reset;
    Designer.Modified;
  end;
end;

{ TcxEditorsLibraryCXControlComponentEditor }

function TcxEditorsLibraryCXControlComponentEditor.GetLookAndFeel: TcxLookAndFeel;
begin
  Result := TcxControlAccess(Component).LookAndFeel;
end;

{ TcxBreadcrumbEditComponentEditor }

function TcxBreadcrumbEditComponentEditor.InternalGetVerb(AIndex: Integer): string;
begin
  if AIndex = 0 then
    Result := 'Edit...'
  else
    Result := inherited InternalGetVerb(AIndex - 1);
end;

function TcxBreadcrumbEditComponentEditor.InternalGetVerbCount: Integer;
begin
  Result := inherited InternalGetVerbCount + 1;
end;

procedure TcxBreadcrumbEditComponentEditor.InternalExecuteVerb(AIndex: Integer);
begin
  if AIndex = 0 then
    Edit
  else
    inherited InternalExecuteVerb(AIndex - 1);
end;

procedure TcxBreadcrumbEditComponentEditor.Edit;
begin
  dxBreadcrumbEditShowEditor(Component as TdxBreadcrumbEdit);
end;

{ TdxBreadcrumbEditSelectedPathPropertyEditor }

procedure TdxBreadcrumbEditSelectedPathPropertyEditor.Edit;
begin
  dxBreadcrumbEditShowEditor(GetComponent(0) as TdxBreadcrumbEdit);
end;

function TdxBreadcrumbEditSelectedPathPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes;
  if PropCount = 1 then
    Include(Result, paDialog);
end;

{ TdxBevelComponentEditor }

function TdxBevelComponentEditor.GetLookAndFeel: TcxLookAndFeel;
begin
  Result := TdxCustomBevel(Component).LookAndFeel;
end;

{ TcxEditStyleControllerEditor }

procedure TcxEditStyleControllerEditor.Edit;
begin
  ShowEventMethod(Designer, Component, 'OnStyleChanged',
    Component.Name + 'StyleChanged', ADefaultMethodParams);
end;

procedure TcxEditStyleControllerEditor.RestoreStyles;
begin
  TcxEditStyleController(Component).RestoreStyles;
end;

{ TcxDefaultEditStyleControllerEditor }

procedure TcxDefaultEditStyleControllerEditor.Edit;
begin
  ShowEventMethod(Designer, Component, 'OnStyleChanged',
    Component.Name + 'StyleChanged', ADefaultMethodParams);
end;

procedure TcxDefaultEditStyleControllerEditor.RestoreStyles;
begin
  DefaultEditStyleController.RestoreStyles;
end;

{ TcxCustomButtonComponentEditor }

function TcxCustomButtonComponentEditor.GetLookAndFeel: TcxLookAndFeel;
begin
  Result := TcxCustomButton(Component).LookAndFeel;
end;

{ TdxAlertWindowComponentEditor }

procedure TdxAlertWindowComponentEditor.Edit;
begin
  dxViewAlertWindow(TdxAlertWindowManager(Component));
end;

function TdxAlertWindowComponentEditor.GetLookAndFeel: TcxLookAndFeel;
begin
  Result := TdxAlertWindowManager(Component).LookAndFeel;
end;

procedure TdxAlertWindowComponentEditor.InternalExecuteVerb(AIndex: Integer);
begin
  if AIndex = 0 then
    Edit
  else
    inherited InternalExecuteVerb(AIndex - 1);
end;

function TdxAlertWindowComponentEditor.InternalGetVerb(AIndex: Integer): string;
begin
  if AIndex = 0 then
    Result := 'Test'
  else
    Result := inherited InternalGetVerb(AIndex - 1);
end;

function TdxAlertWindowComponentEditor.InternalGetVerbCount: Integer;
begin
  Result := inherited InternalGetVerbCount + 1;
end;

{ TcxRadioButtonComponentEditor }

function TcxRadioButtonComponentEditor.GetLookAndFeel: TcxLookAndFeel;
begin
  Result := TcxRadioButton(Component).LookAndFeel;
end;

{ TcxContainerComponentEditor }

procedure TcxContainerComponentEditor.RestoreStyles;
begin
  TcxContainer(Component).RestoreStyles;
end;

{ TcxCustomNavigatorComponentEditor }

procedure TcxCustomNavigatorComponentEditor.Edit;
var
  AButtons: TcxCustomNavigatorButtons;
begin
  AButtons := TcxCustomNavigatorAccess(Component).CustomButtons;
  ShowEventMethod(Designer, AButtons, 'OnButtonClick',
    Component.Name + 'ButtonsButtonClick', cxNavigatorButtonsOnButtonClickEventParams);
end;

function TcxCustomNavigatorComponentEditor.InternalGetVerb(AIndex: Integer): string;
begin
  if AIndex = 1 then
    Result := cxCustomNavigatorEditorVerb
  else
    Result := inherited InternalGetVerb(AIndex);
end;

function TcxCustomNavigatorComponentEditor.InternalGetVerbCount: Integer;
begin
  Result := 1 + inherited InternalGetVerbCount;
end;

procedure TcxCustomNavigatorComponentEditor.InternalExecuteVerb(AIndex: Integer);
begin
  if AIndex = 1 then
  begin
    TcxCustomNavigator(Component).RestoreButtons;
    Designer.Modified;
  end
  else
    inherited InternalExecuteVerb(AIndex);
end;

{ TdxGalleryControlComponentEditor }

procedure TdxGalleryControlComponentEditor.ExecuteEditAction;
begin
  EditGallery(Component);
end;

{ TdxSliderImageComponentEditor }

procedure TdxSliderImageComponentEditor.Edit;
begin
  ShowEventMethod(Designer, Component, 'OnChange',
    Component.Name + 'Change', ADefaultMethodParams);
end;

{ TcxCustomImagePropertiesProperty }

procedure TcxCustomImagePropertiesProperty.GetProperties(Proc: TGetPropProc);
begin
  FProc := Proc;
  inherited GetProperties(GetPropProc);
end;

procedure TcxCustomImagePropertiesProperty.GetPropProc(const Prop: IProperty);
var
  I: Integer;
begin
  if InternalCompareString(Prop.GetName, 'OnGetGraphicClass', False) then
    for I := 0 to PropCount - 1 do
      if TcxCustomImage(GetComponent(I)).RepositoryItem = nil then
        Exit;
  FProc(Prop);
end;

{ TcxEditPropertiesAssignedValuesProperty }

procedure TcxEditPropertiesAssignedValuesProperty.GetProperties(Proc: TGetPropProc);
begin
  FProc := Proc;
  inherited GetProperties(GetPropProc);
end;

function TcxEditPropertiesAssignedValuesProperty.GetValue: string;
var
  AAssignedValues: TcxCustomEditPropertiesValues;
  APPropList: PPropList;
  APropertyCount: Integer;
  I: Integer;
begin
  Result := '';
  AAssignedValues := TcxCustomEditPropertiesAccess(GetComponent(0)).AssignedValues;
  APropertyCount := GetPropList(GetPropType, [tkUnknown..tkDynArray], nil);
  if APropertyCount > 0 then
  begin
    GetMem(APPropList, APropertyCount * SizeOf(Pointer));
    try
      GetPropList(GetPropType, [tkUnknown..tkDynArray], APPropList);
      for I := 0 to APropertyCount - 1 do
        if Boolean(GetOrdProp(AAssignedValues, APPropList[I])) and
          IsVisibleProperty(dxShortStringToString(APPropList[I].Name)) then
        begin
          if Result <> '' then
            Result := Result + ',';
          Result := Result + dxShortStringToString(APPropList[I].Name);
        end;
    finally
      FreeMem(APPropList);
    end;
  end;
  Result := '[' + Result + ']';
end;

procedure TcxEditPropertiesAssignedValuesProperty.GetPropProc(const Prop: IProperty);
begin
  if IsVisibleProperty(Prop.GetName) then
    FProc(Prop);
end;

function TcxEditPropertiesAssignedValuesProperty.IsVisibleProperty(
  const APropertyName: string): Boolean;
begin
  Result := TypInfo.GetPropInfo(TcxCustomEditProperties(GetComponent(0)),
    APropertyName) <> nil;
end;

{ TcxDefaultEditStyleControllerStyleProperty }

function TcxDefaultEditStyleControllerStyleProperty.GetStyle: TcxContainerStyle;
begin
  Result := DefaultEditStyleController.Style;
end;

function TcxDefaultEditStyleControllerStyleProperty.IsPropertyVisible(
  const APropertyName: string): Boolean;
begin
  Result := (APropertyName <> 'StyleController') and
    inherited IsPropertyVisible(APropertyName);
end;

{ TcxNavigatorButtonImageIndexProperty }

function TcxNavigatorButtonImageIndexProperty.GetImages: TCustomImageList;
begin
  Result := TcxNavigatorButton(GetComponent(0)).Buttons.Images;
end;

{ TcxNavigatorCustomButtonImageIndexProperty }

function TcxNavigatorCustomButtonImageIndexProperty.GetImages: TCustomImageList;
begin
  Result := (TcxNavigatorCustomButton(GetComponent(0)).Collection.Owner as TcxCustomNavigatorButtons).Images;
end;

{ TcxEditButtonImageIndexProperty }

function TcxEditButtonImageIndexProperty.GetImages: TCustomImageList;
begin
  Result := TcxCustomEditProperties(TcxEditButtons(TcxEditButton(GetComponent(0)).Collection).Owner).Images;
end;

{ TcxButtonImageIndexProperty }

function TcxButtonImageIndexProperty.GetImages: TCustomImageList;
var
  AOptionsImage: TcxButtonImageOptionsAccess;
  AButton: TcxButton;
begin
  AOptionsImage := TcxButtonImageOptionsAccess(GetComponent(0));
  if AOptionsImage.Images <> nil then
    Result := AOptionsImage.Images
  else
  begin
    AButton := (AOptionsImage.GetOwner as TcxButton);
    if (AButton.Action <> nil) and (AButton.Action is TCustomAction) and
      (TCustomAction(AButton.Action).ActionList <> nil)
    then
      Result := TCustomAction(AButton.Action).ActionList.Images
    else
      Result := nil;
  end;
end;

{ TdxBreadcrumbEditRecentPathImageIndexProperty }

function TdxBreadcrumbEditRecentPathImageIndexProperty.GetImages: TCustomImageList;
begin
  Result := TdxCustomBreadcrumbEditPropertiesAccess(
    TdxBreadcrumbEditPathEditorPropertiesAccess(GetPathEditorProperties).Owner).Images;
end;

function TdxBreadcrumbEditRecentPathImageIndexProperty.GetPathEditorProperties: TdxBreadcrumbEditPathEditorProperties;
begin
  Result := TdxBreadcrumbEditPathEditorProperties(
    TdxBreadcrumbEditRecentPath(GetComponent(0)).Collection.Owner);
end;

{ TdxBreadcrumbEditButtonImageIndexProperty }

function TdxBreadcrumbEditButtonImageIndexProperty.GetImages: TCustomImageList;
begin
  Result := TdxCustomBreadcrumbEditPropertiesAccess(
    TdxBreadcrumbEditButton(GetComponent(0)).Collection.Owner).ButtonImages;
end;

{ TdxAlertWindowButtonImageIndexProperty }

function TdxAlertWindowButtonImageIndexProperty.GetImages: TCustomImageList;
begin
  Result := TdxAlertWindowOptionsButtons(TdxAlertWindowButtons(
    TdxAlertWindowButton(GetComponent(0)).Collection).Owner).Images;
end;

{ TcxEditGuidelines }

function TcxEditGuidelines.GetCount: Integer;
begin
  Result := inherited GetCount;
  if Edit.HasTextBaseLine then
    Inc(Result);
end;

function TcxEditGuidelines.GetDesignerGuideOffset(Index: Integer): Integer;
begin
  if Edit.HasTextBaseLine and (Index = GetCount - 1) then
    Result := Edit.GetTextBaseLine
  else
    Result := inherited GetDesignerGuideOffset(Index);
end;

function TcxEditGuidelines.GetDesignerGuideType(Index: Integer): TDesignerGuideType;
begin
  if Edit.HasTextBaseLine and (Index = GetCount - 1) then
    Result := gtBaseline
  else
    Result := inherited GetDesignerGuideType(Index);
end;

function TcxEditGuidelines.GetEdit: TcxCustomEdit;
begin
  Result := TcxCustomEdit(Component);
end;


{ TdxDataControllerMultithreadedSortingPropertyEditor }

function TdxDataControllerMultithreadedSortingPropertyEditor.GetDefaultValue: Boolean;
begin
  Result := dxDefaultMultiThreadedSorting;
end;

{ TdxDataControllerMultithreadedFilteringPropertyEditor }

function TdxDataControllerMultithreadedFilteringPropertyEditor.GetDefaultValue: Boolean;
begin
  Result := dxDefaultMultiThreadedFiltering;
end;

{ TdxAlphaColorPropertyEditor }

procedure TdxAlphaColorPropertyEditor.Edit;
var
  AColorDialog: TdxColorDialog;
begin
  AColorDialog := TdxColorDialog.Create(nil);
  try
    AColorDialog.Color := GetOrdValue;
    AColorDialog.Options.ColorPicker.DefaultVisible := True;
    if AColorDialog.Execute then
      SetOrdValue(AColorDialog.Color);
  finally
    AColorDialog.Free;
  end;
end;

function TdxAlphaColorPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paDialog, paValueList, paRevertable];
end;

function TdxAlphaColorPropertyEditor.GetValue: string;
var
  AAlpha: Byte;
  AColor: TColor;
begin
  AColor := dxAlphaColorToColor(GetOrdValue, AAlpha);
  if not (ColorToIdent(LongInt(AColor), Result) and ((AAlpha = MaxByte) or not cxColorIsValid(AColor))) then
    Result := '$' + IntToHex(GetOrdValue, 8);
end;

procedure TdxAlphaColorPropertyEditor.GetValues(Proc: TGetStrProc);
begin
  FSavedProc := Proc;
  GetColorValues(FilteredGetStrProc);
end;

procedure TdxAlphaColorPropertyEditor.SetValue(const Value: string);
begin
  SetOrdValue(ValueToAlphaColor(Value));
end;

procedure TdxAlphaColorPropertyEditor.PropDrawName(ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
begin
  DefaultPropertyDrawName(Self, ACanvas, ARect);
end;

procedure TdxAlphaColorPropertyEditor.PropDrawValue(ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
begin
  if GetVisualValue <> '' then
    DrawPreview(ACanvas, cxRectCenterHorizontally(ARect, cxRectHeight(ARect)), GetOrdValue, ASelected)
  else
    DefaultPropertyDrawValue(Self, ACanvas, ARect);
end;

procedure TdxAlphaColorPropertyEditor.ListDrawValue(
  const Value: string; ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
var
  R, APreviewRect: TRect;
begin
  R := ARect;
  CalculatePreviewRect(R, APreviewRect);
  DrawPreview(ACanvas, APreviewRect, ValueToAlphaColor(Value), ASelected);
  DefaultPropertyListDrawValue(Value, ACanvas, R, ASelected);
end;

procedure TdxAlphaColorPropertyEditor.ListMeasureHeight(const Value: string; ACanvas: TCanvas; var AHeight: Integer);
begin
  // do nothing
end;

procedure TdxAlphaColorPropertyEditor.ListMeasureWidth(const Value: string; ACanvas: TCanvas; var AWidth: Integer);
begin
  Inc(AWidth, ACanvas.TextHeight('M'));
end;

function TdxAlphaColorPropertyEditor.PropDrawNameRect(const ARect: TRect): TRect;
begin
  Result := ARect;
end;

function TdxAlphaColorPropertyEditor.PropDrawValueRect(const ARect: TRect): TRect;
begin
  Result := cxRectSetWidth(ARect, cxRectHeight(ARect));
end;

procedure TdxAlphaColorPropertyEditor.CalculatePreviewRect(var R: TRect; out APreviewRect: TRect);
begin
  APreviewRect := cxRectSetWidth(R, cxRectHeight(R));
  R.Left := APreviewRect.Right;
end;

procedure TdxAlphaColorPropertyEditor.DrawPreview(ACanvas: TCanvas; R: TRect; AColor: TdxAlphaColor; ASelected: Boolean);

  function GetBorderColor: TColor;
  var
    Q: TRGBQuad;
  begin
    Q := dxColorToRGBQuad(dxAlphaColorToColor(AColor));
    if Max(Max(Q.rgbBlue, Q.rgbRed), Q.rgbGreen) > 192 then
      Result := clBlack
    else
      if ASelected then
        Result := clWhite
      else
        Result := clGray;
  end;

var
  APrevBrushColor: TColor;
  APrevPenColor: TColor;
begin
  APrevPenColor := ACanvas.Pen.Color;
  APrevBrushColor := ACanvas.Brush.Color;
  try
    ACanvas.Pen.Color := ACanvas.Brush.Color;
    ACanvas.Rectangle(R);
    InflateRect(R, -1, -1);

    ACanvas.Pen.Color := GetBorderColor;
    ACanvas.Rectangle(R);
    InflateRect(R, -1, -1);

    cxDrawTransparencyCheckerboard(ACanvas.Handle, R, 2);
    dxGPPaintCanvas.BeginPaint(ACanvas.Handle, R);
    try
      dxGPPaintCanvas.FillRectangle(R, AColor);
    finally
      dxGPPaintCanvas.EndPaint;
    end;
  finally
    ACanvas.Brush.Color := APrevBrushColor;
    ACanvas.Pen.Color := APrevPenColor;
  end;
end;

procedure TdxAlphaColorPropertyEditor.FilteredGetStrProc(const S: string);
var
  AColor: LongInt;
begin
  if IdentToColor(S, AColor) and (ColorToRGB(AColor) = AColor) then
    FSavedProc(S);
end;

function TdxAlphaColorPropertyEditor.ValueToAlphaColor(const AValue: string): TdxAlphaColor;
var
  AColor: Integer;
begin
  if IdentToColor(AValue, AColor) then
    Result := dxMakeAlphaColor(AColor)
  else
    Result := StrToIntDef(AValue, 0);
end;

{ TdxColorDialogComponentEditor }

procedure TdxColorDialogComponentEditor.Edit;
begin
  if TdxColorDialog(Component).Execute then
    Designer.Modified;
end;

function TdxColorDialogComponentEditor.GetLookAndFeel: TcxLookAndFeel;
begin
  Result := TdxColorDialog(Component).LookAndFeel;
end;

procedure TdxColorDialogComponentEditor.InternalExecuteVerb(AIndex: Integer);
begin
  if AIndex = 0 then
    Edit
  else
    inherited InternalExecuteVerb(AIndex - 1);
end;

function TdxColorDialogComponentEditor.InternalGetVerb(AIndex: Integer): string;
begin
  if AIndex = 0 then
    Result := '&Test Dialog'
  else
    Result := inherited InternalGetVerb(AIndex - 1);
end;

function TdxColorDialogComponentEditor.InternalGetVerbCount: Integer;
begin
  Result := inherited InternalGetVerbCount + 1;
end;

{ TdxActivityIndicatorPropertiesEditor }

function TdxActivityIndicatorPropertiesEditor.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes;
  if not HasSubProperties then
    Exclude(Result, paSubProperties);
  Result := Result - [paReadOnly] + [paValueList, paSortList, paRevertable, paVolatileSubProperties];
end;

function TdxActivityIndicatorPropertiesEditor.GetValue: string;
begin
  if HasSubProperties then
    Result := GetRegisteredActivityIndicatorProperties.GetDescriptionByClass(
      TdxCustomActivityIndicatorAccess(GetComponent(0)).PropertiesClass)
  else
    Result := '';
end;

procedure TdxActivityIndicatorPropertiesEditor.GetValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  for I := 0 to GetRegisteredActivityIndicatorProperties.Count - 1 do
    Proc(GetRegisteredActivityIndicatorProperties.Descriptions[I]);
end;

procedure TdxActivityIndicatorPropertiesEditor.SetValue(const Value: string);
var
  APropertiesClass: TdxActivityIndicatorPropertiesClass;
  I: Integer;
begin
  APropertiesClass := TdxActivityIndicatorPropertiesClass(GetRegisteredActivityIndicatorProperties.FindByClassName(Value));
  if APropertiesClass = nil then
    APropertiesClass := TdxActivityIndicatorPropertiesClass(GetRegisteredActivityIndicatorProperties.FindByDescription(Value));
  for I := 0 to PropCount - 1 do
    TdxCustomActivityIndicatorAccess(GetComponent(I)).PropertiesClass := APropertiesClass;
  Modified;
end;

function TdxActivityIndicatorPropertiesEditor.HasSubProperties: Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 0 to PropCount - 1 do
  begin
    if TdxCustomActivityIndicatorAccess(GetComponent(I)).Properties = nil then
      Exit(False);
  end;
end;

{ TdxWheelPickerComponentEditor }

function TdxWheelPickerComponentEditor.GetEditVerb: string;
begin
  Result := 'Wheels...'
end;

function TdxWheelPickerComponentEditor.HasEditor: Boolean;
begin
  Result := True;
end;

procedure TdxWheelPickerComponentEditor.RunEditor;
begin
  EditGallery(Component);
end;

{ TdxWheelPickerWheelsPropertyEditor }

procedure TdxWheelPickerWheelsPropertyEditor.Edit;
begin
  EditGallery((GetComponent(0) as TdxWheelPickerProperties).Owner as TdxWheelPicker);
end;

function TdxWheelPickerWheelsPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paReadOnly, paDialog];
end;

function TdxWheelPickerWheelsPropertyEditor.GetValue: string;
begin
  Result := Format('(%s)', [TdxWheelPicker.Classname]);
end;

{ TdxWheelPickerItemIndexesEditor }

procedure TdxWheelPickerItemIndexesEditor.Edit;
begin
  dxShowWheelPickerItemIndexesEditor(TdxWheelPicker(GetComponent(0)));
end;

function TdxWheelPickerItemIndexesEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

function TdxWheelPickerItemIndexesEditor.GetName: string;
begin
  Result := 'ItemIndexes';
end;

function TdxWheelPickerItemIndexesEditor.GetValue: string;
begin
  Result := '';
end;

{ TdxWheelPickerItemImageIndexProperty }

function TdxWheelPickerItemImageIndexProperty.GetImages: TCustomImageList;
begin
  Result := TcxCustomEditProperties(TdxWheelPickerWheelsAccess(TdxWheelPickerWheelAccess(TdxWheelPickerItemsAccess(
    TdxWheelPickerItemAccess(GetComponent(0)).Collection).Owner).Collection).Owner).Images;
end;

{ TdxWheelPickerItemCollectionProperty }

function TdxWheelPickerItemCollectionProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paSubProperties] - [paDialog];
end;

procedure TdxWheelPickerItemCollectionProperty.Edit;
begin
// do nothing
end;

{ TdxSparklineEditComponentEditor }

function TdxSparklineEditComponentEditor.GetEditVerb: string;
begin
  Result := 'Series...'
end;

function TdxSparklineEditComponentEditor.HasEditor: Boolean;
begin
  Result := True;
end;

procedure TdxSparklineEditComponentEditor.RunEditor;
begin
  ShowCollectionEditor(Designer, Component,
    TdxSparklineProperties((GetEdit as TdxCustomSparklineEdit).Properties).Series, 'Series');
end;

{ TdxBarCodeSymbologyProperty }

function TdxBarCodeSymbologyProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes;
  if not HasSubProperties then
    Exclude(Result, paSubProperties);
  Result := Result - [paReadOnly] +
    [paValueList, paSortList, paRevertable, paVolatileSubProperties];
end;

function TdxBarCodeSymbologyProperty.GetValue: string;
begin
  if HasSubProperties then
    Result := dxGetRegisteredBarCodeSymbologies.GetDescriptionByClass(
      TdxCustomBarCodeProperties(GetComponent(0)).Symbology.ClassType)
  else
    Result := '';
end;

procedure TdxBarCodeSymbologyProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
  ADesc: string;
begin
  for I := 0 to dxGetRegisteredBarCodeSymbologies.Count - 1 do
  begin
    ADesc := dxGetRegisteredBarCodeSymbologies.Descriptions[I];
    if ADesc <> '' then
      Proc(ADesc);
  end;
end;

procedure TdxBarCodeSymbologyProperty.SetValue(const Value: string);
var
  ABarCodeSymbologyClass: TdxCustomBarCodeSymbologyClass;
  I: Integer;
begin
  ABarCodeSymbologyClass := TdxCustomBarCodeSymbologyClass(dxGetRegisteredBarCodeSymbologies.FindByClassName(Value));
  if ABarCodeSymbologyClass = nil then
    ABarCodeSymbologyClass := TdxCustomBarCodeSymbologyClass(dxGetRegisteredBarCodeSymbologies.FindByDescription(Value));

  ObjectInspectorCollapseProperty;
  for I := 0 to PropCount - 1 do
    TdxCustomBarCodeProperties(GetComponent(I)).BarCodeSymbologyClass := ABarCodeSymbologyClass;

  Modified;
end;

function TdxBarCodeSymbologyProperty.HasSubProperties: Boolean;
var
  I: Integer;
begin
  for I := 0 to PropCount - 1 do
  begin
    Result := TdxCustomBarCodeProperties(GetComponent(I)).Symbology <> nil;
    if not Result then
      Exit;
  end;
  Result := True;
end;

{ TdxTokenEditTokenImageIndexProperty }

function TdxTokenEditTokenImageIndexProperty.GetImages: TCustomImageList;
begin
  Result := TdxTokenEditProperties(TdxTokenEditToken(GetComponent(0)).Collection.Owner).Images;
end;

{ TdxTokenEditComponentEditor }

function TdxTokenEditComponentEditor.GetEditVerb: string;
begin
  Result := 'Tokens...';
end;

function TdxTokenEditComponentEditor.HasEditor: Boolean;
begin
  Result := True;
end;

procedure TdxTokenEditComponentEditor.RunEditor;
begin
  ShowCollectionEditor(Designer, Component, (GetEdit as TdxCustomTokenEdit).Properties.Tokens, 'Tokens');
end;

end.

