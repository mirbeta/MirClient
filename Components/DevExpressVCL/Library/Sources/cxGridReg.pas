{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressQuantumGrid                                       }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSQUANTUMGRID AND ALL            }
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

unit cxGridReg;

{$I cxVer.inc}

interface

uses
  DesignEditors, DesignIntf, DesignMenus, VCLEditors,
  Classes, Controls, ExtCtrls, Graphics, Menus, SysUtils, TypInfo, ImgList,
  cxGridCustomView, dxCoreReg, cxClasses, cxGrid;

const
  cxGridProductName  = 'ExpressQuantumGrid Suite';

type
  TcxExecuteGridWizardProc = procedure(AGrid: TcxCustomGrid; ASelectedGridView: TcxCustomGridView = nil);

  { TcxCustomGridEditor }

  TcxCustomGridEditor = class(TdxComponentEditor)
  protected
    function GetProductName: string; override;
  end;

  { TcxCustomGridTableItemProperty }

  TcxCustomGridTableItemProperty = class(TComponentProperty)
  protected
    function GetGridView: TcxCustomGridView;
    procedure GetGridViewItemNames(AGridView: TcxCustomGridView; Proc: TGetStrProc); virtual;
    function InternalGetGridView(APersistent: TPersistent): TcxCustomGridView; virtual; abstract;
  public
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  { TcxGridTableSummaryItemColumnProperty }

  TcxGridTableSummaryItemColumnProperty = class(TcxCustomGridTableItemProperty)
  protected
    function InternalGetGridView(APersistent: TPersistent): TcxCustomGridView; override;
  end;

procedure Register;

implementation

uses
  Windows, Types,
{$IFNDEF NONDB}
  DB, cxDBData, cxDBExtLookupComboBox, cxGridDBBandedTableView,
  cxGridDBCardView, cxGridDBDataDefinitions, cxGridDBTableView, cxGridDBChartView,
{$ENDIF}
  cxGridImportDialog, cxControls,
  cxCustomData, cxDataStorage, cxEdit, cxEditPropEditors, dxCore, cxPC,
  cxEditRepositoryEditor, cxEditRepositoryItems, cxGridBandedTableView,
  cxGridCardView, cxGridCommon, cxGridCustomTableView, cxGridChartView,
  cxGridEditor, cxGridLevel, cxGridStrs, cxGridStructureNavigator, cxGraphics,
  cxGridTableView, cxImageComboBox, cxImageComboBoxItemsEditor, cxLibraryReg,
  cxPropEditors, cxStyles, cxViewEditor, cxBandedTableViewEditor, cxCardViewEditor,
  cxChartViewEditor, cxGridPredefinedStyles, cxGridStyleSheetsPreview, cxGridExportLink,
  ColnEdit, cxGridInplaceEditForm, cxFilter, TreeIntf, cxGridViewLayoutContainer,
  cxDataControllerConditionalFormatting, cxDesignWindows;

{ TGridEditor }

const
  GridEditorVerbs: array [0..3] of string = ('Editor...', 'Import...', 'Structure Navigator', 'Wizard...');

type
  TViewMenuItemAction = (vmiaExecute, vmiaGetCaption, vmiaPrepare);

  { TGridEditor }

  TGridEditor = class(TcxCustomGridEditor)
  private
    FAddOnInsertPosition: Integer;
    FViewMenuProvider: TcxCustomGridViewMenuProvider;
    function GetGrid: TcxCustomGrid;
    function GetGridView: TcxCustomGridView;
  protected
    function InternalGetVerb(AIndex: Integer): string; override;
    function InternalGetVerbCount: Integer; override;
    procedure InternalExecuteVerb(AIndex: Integer); override;

    function GetViewMenuItemCount: Integer;
    function ProcessViewMenuItem(var AIndex: Integer; AAction: TViewMenuItemAction;
      const ADesignMenuItem: TDesignMenuItem = nil): Boolean;

    property Grid: TcxCustomGrid read GetGrid;
    property GridView: TcxCustomGridView read GetGridView;
    property ViewMenuProvider: TcxCustomGridViewMenuProvider read FViewMenuProvider;
  public
    destructor Destroy; override;
    procedure PrepareItem(Index: Integer; const AItem: TDesignMenuItem); override;
    function GetVerbCount: Integer; override;
  end;

{ TcxCustomGridEditor }

function TcxCustomGridEditor.GetProductName: string;
begin
  Result := cxGridProductName;
end;

{ TGridEditor }

destructor TGridEditor.Destroy;
begin
  FreeAndNil(FViewMenuProvider);
  inherited Destroy;
end;

function TGridEditor.GetGrid: TcxCustomGrid;
begin
  Result := TcxCustomGrid(Component);
end;

function TGridEditor.GetGridView: TcxCustomGridView;
begin
  Result := Grid.ActiveView;
  if (Result <> nil) and Result.IsMaster then
    Result := nil;
end;

function TGridEditor.InternalGetVerb(AIndex: Integer): string;
begin
  if ProcessViewMenuItem(AIndex, vmiaGetCaption) then
    Result := ''
  else
    if (AIndex >= 0) and (AIndex <= High(GridEditorVerbs)) then
      Result := GridEditorVerbs[AIndex]
    else
      Result := '';
end;

function TGridEditor.InternalGetVerbCount: Integer;
begin
  Result := FAddOnInsertPosition + GetViewMenuItemCount;
end;

procedure TGridEditor.InternalExecuteVerb(AIndex: Integer);
begin
  inherited InternalExecuteVerb(AIndex);
  if not ProcessViewMenuItem(AIndex, vmiaExecute) then
    case AIndex of
      0: ShowGridEditor(Designer, Grid);
      1: ShowGridImportDialog(Designer, Grid);
      2: Grid.StructureNavigator.Visible := not Grid.StructureNavigator.Visible;
      3: if FGridWizard <> nil then
           FGridWizard.Execute(Grid);
    end;
end;

function TGridEditor.GetVerbCount: Integer;
begin
  FreeAndNil(FViewMenuProvider);
  FAddOnInsertPosition := Length(GridEditorVerbs) - Ord(FGridWizard = nil);
  if GridView <> nil then
    FViewMenuProvider := CreateViewMenuProvider(GridView);
  Result := inherited GetVerbCount;
end;

function TGridEditor.GetViewMenuItemCount: Integer;
begin
  if ViewMenuProvider = nil then
    Result := 0
  else
    Result := 1 + ViewMenuProvider.Items.Count;  // 1 - for separator
end;

function TGridEditor.ProcessViewMenuItem(var AIndex: Integer;
  AAction: TViewMenuItemAction; const ADesignMenuItem: TDesignMenuItem = nil): Boolean;
var
  AMenuProviderItem: TcxGridViewMenuItem;
begin
  Result := False;
  if (AIndex >= FAddOnInsertPosition) and (ViewMenuProvider <> nil) then
    if AIndex - FAddOnInsertPosition < GetViewMenuItemCount then
    begin
      if AIndex = FAddOnInsertPosition then  // separator
        AMenuProviderItem := nil
      else
        AMenuProviderItem := ViewMenuProvider.Items[AIndex - FAddOnInsertPosition - 1];

      case AAction of
        vmiaExecute:
          if AMenuProviderItem <> nil then
            AMenuProviderItem.DoAction;
        vmiaGetCaption:
          ;
        vmiaPrepare:
          if AMenuProviderItem = nil then
            ADesignMenuItem.Caption := cxGridViewMenuSeparatorCaption
          else
            AMenuProviderItem.Prepare(ADesignMenuItem);
      end;
      Result := True;
    end
    else
      Dec(AIndex, GetViewMenuItemCount);
end;

procedure TGridEditor.PrepareItem(Index: Integer; const AItem: TDesignMenuItem);
begin
  if Index = 2 then // Structure Navigator
    AItem.Checked := Grid.StructureNavigator.Visible;
  if Index = 3 then // Wizard...
    AItem.Enabled := CanChangeComponentList(Grid);
  inherited PrepareItem(Index, AItem);
  ProcessViewMenuItem(Index, vmiaPrepare, AItem);
end;

{ TGridViewRepositoryEditor }

type
  TGridViewRepositoryEditor = class(TcxCustomGridEditor)
  private
    function GetGridViewRepository: TcxGridViewRepository;
  protected
    function InternalGetVerb(AIndex: Integer): string; override;
    function InternalGetVerbCount: Integer; override;
    procedure InternalExecuteVerb(AIndex: Integer); override;

    property GridViewRepository: TcxGridViewRepository read GetGridViewRepository;
  end;

function TGridViewRepositoryEditor.GetGridViewRepository: TcxGridViewRepository;
begin
  Result := TcxGridViewRepository(Component);
end;

procedure TGridViewRepositoryEditor.InternalExecuteVerb(AIndex: Integer);
begin
  case AIndex of
    0: ShowViewRepositoryEditor(Designer, GridViewRepository);
  end;
end;

function TGridViewRepositoryEditor.InternalGetVerb(AIndex: Integer): string;
begin
  case AIndex of
    0: Result := 'Editor...';
  end;
end;

function TGridViewRepositoryEditor.InternalGetVerbCount: Integer;
begin
  Result := 1;
end;

{ TcxGridRootLevelStylesEventsProperty }

type
  TcxGridRootLevelStylesEventsProperty = class(TcxNestedEventProperty)
  protected
    function GetInstance: TPersistent; override;
  end;

function TcxGridRootLevelStylesEventsProperty.GetInstance: TPersistent;
begin
  Result := TcxCustomGrid(GetComponent(0)).RootLevelStyles;
end;

{ TcxGridTableItemPropertiesEventsProperty }

type
  TcxGridTableItemPropertiesEventsProperty = class(TcxNestedEventProperty)
  protected
    function GetInstance: TPersistent; override;
  end;

function TcxGridTableItemPropertiesEventsProperty.GetInstance: TPersistent;
begin
  Result := TcxCustomGridTableItem(GetComponent(0)).Properties;
end;

{ TcxGridTableItemStylesEventsProperty }

type
  TcxGridTableItemStylesEventsProperty = class(TcxNestedEventProperty)
  protected
    function GetInstance: TPersistent; override;
  end;

function TcxGridTableItemStylesEventsProperty.GetInstance: TPersistent;
begin
  Result := TcxCustomGridTableItem(GetComponent(0)).Styles;
end;

{ TcxGridDataControllerEventsProperty }

type
  TcxGridDataControllerEventsProperty = class(TcxNestedEventProperty)
  protected
    function GetInstance: TPersistent; override;
  end;

function TcxGridDataControllerEventsProperty.GetInstance: TPersistent;
begin
  Result := TcxCustomGridView(GetComponent(0)).DataController;
end;

{ TcxGridViewStylesEventsProperty }

type
  TcxGridViewStylesEventsProperty = class(TcxNestedEventProperty)
  protected
    function GetInstance: TPersistent; override;
  end;

function TcxGridViewStylesEventsProperty.GetInstance: TPersistent;
begin
  Result := TcxCustomGridViewAccess.GetStyles(TcxCustomGridView(GetComponent(0)));
end;

{ TcxGridLevelStylesEventsProperty }

type
  TcxGridLevelStylesEventsProperty = class(TcxNestedEventProperty)
  protected
    function GetInstance: TPersistent; override;
  end;

function TcxGridLevelStylesEventsProperty.GetInstance: TPersistent;
begin
  Result := TcxGridLevel(GetComponent(0)).Styles;
end;

{ TcxGridBandStylesEventsProperty }

type
  TcxGridBandStylesEventsProperty = class(TcxNestedEventProperty)
  protected
    function GetInstance: TPersistent; override;
  end;

function TcxGridBandStylesEventsProperty.GetInstance: TPersistent;
begin
  Result := TcxGridBand(GetComponent(0)).Styles;
end;

{ TcxGridChartSeriesStylesEventsProperty }

type
  TcxGridChartSeriesStylesEventsProperty = class(TcxNestedEventProperty)
  protected
    function GetInstance: TPersistent; override;
  end;

function TcxGridChartSeriesStylesEventsProperty.GetInstance: TPersistent;
begin
  Result := TcxGridChartSeries(GetComponent(0)).Styles;
end;

{ TcxGridChartHistogramValuesEventsProperty }

type
  TcxGridChartHistogramValuesEventsProperty = class(TcxNestedEventProperty)
  protected
    function GetInstance: TPersistent; override;
  end;

function TcxGridChartHistogramValuesEventsProperty.GetInstance: TPersistent;
begin
  Result := TcxGridChartHistogram(GetComponent(0)).Values;
end;

{ TcxGridChartCategoriesEventsProperty }

type
  TcxGridChartCategoriesEventsProperty = class(TcxNestedEventProperty)
  protected
    function GetInstance: TPersistent; override;
  end;

function TcxGridChartCategoriesEventsProperty.GetInstance: TPersistent;
begin
  Result := TcxGridChartView(GetComponent(0)).Categories;
end;

{ TcxGridChartAreaDiagramEventsProperty }

type
  TcxGridChartAreaDiagramEventsProperty = class(TcxNestedEventProperty)
  protected
    function GetInstance: TPersistent; override;
  end;

function TcxGridChartAreaDiagramEventsProperty.GetInstance: TPersistent;
begin
  Result := TcxGridChartView(GetComponent(0)).DiagramArea;
end;

{ TcxGridChartBarDiagramEventsProperty }

type
  TcxGridChartBarDiagramEventsProperty = class(TcxNestedEventProperty)
  protected
    function GetInstance: TPersistent; override;
  end;

function TcxGridChartBarDiagramEventsProperty.GetInstance: TPersistent;
begin
  Result := TcxGridChartView(GetComponent(0)).DiagramBar;
end;

{ TcxGridChartBarDiagramEventsProperty }

type
  TcxGridChartStackedBarDiagramEventsProperty = class(TcxNestedEventProperty)
  protected
    function GetInstance: TPersistent; override;
  end;

function TcxGridChartStackedBarDiagramEventsProperty.GetInstance: TPersistent;
begin
  Result := TcxGridChartView(GetComponent(0)).DiagramStackedBar;
end;

{ TcxGridChartStackedColumnDiagramEventsProperty }

type
  TcxGridChartStackedColumnDiagramEventsProperty = class(TcxNestedEventProperty)
  protected
    function GetInstance: TPersistent; override;
  end;

function TcxGridChartStackedColumnDiagramEventsProperty.GetInstance: TPersistent;
begin
  Result := TcxGridChartView(GetComponent(0)).DiagramStackedColumn;
end;

{ TcxGridChartStackedAreaDiagramEventsProperty }

type
  TcxGridChartStackedAreaDiagramEventsProperty = class(TcxNestedEventProperty)
  protected
    function GetInstance: TPersistent; override;
  end;

function TcxGridChartStackedAreaDiagramEventsProperty.GetInstance: TPersistent;
begin
  Result := TcxGridChartView(GetComponent(0)).DiagramStackedArea;
end;

{ TcxGridChartColumnDiagramEventsProperty }

type
  TcxGridChartColumnDiagramEventsProperty = class(TcxNestedEventProperty)
  protected
    function GetInstance: TPersistent; override;
  end;

function TcxGridChartColumnDiagramEventsProperty.GetInstance: TPersistent;
begin
  Result := TcxGridChartView(GetComponent(0)).DiagramColumn;
end;

{ TcxGridChartLineDiagramEventsProperty }

type
  TcxGridChartLineDiagramEventsProperty = class(TcxNestedEventProperty)
  protected
    function GetInstance: TPersistent; override;
  end;

function TcxGridChartLineDiagramEventsProperty.GetInstance: TPersistent;
begin
  Result := TcxGridChartView(GetComponent(0)).DiagramLine;
end;

{ TcxGridChartPieDiagramEventsProperty }

type
  TcxGridChartPieDiagramEventsProperty = class(TcxNestedEventProperty)
  protected
    function GetInstance: TPersistent; override;
  end;

function TcxGridChartPieDiagramEventsProperty.GetInstance: TPersistent;
begin
  Result := TcxGridChartView(GetComponent(0)).DiagramPie;
end;

{ TcxGridLevelGridViewProperty }

type
  TcxGridLevelGridViewProperty = class(TComponentProperty)
  private
    function GetLevel: TcxGridLevel;
  protected
    property Level: TcxGridLevel read GetLevel;
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

function TcxGridLevelGridViewProperty.GetLevel: TcxGridLevel;
begin
  Result := TcxGridLevel(GetComponent(0));
end;

function TcxGridLevelGridViewProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes - [paMultiSelect];
end;

procedure TcxGridLevelGridViewProperty.GetValues(Proc: TGetStrProc);
var
  AViewList: TcxGridLevelViewList;
  I: Integer;
begin
  AViewList := TcxGridLevelViewList.Create(Level);
  try
    for I := 0 to AViewList.ViewNames.Count - 1 do
      Proc(AViewList.ViewNames[I]);
  finally
    AViewList.Free;
  end;
end;

{ TcxGridLevelImageIndexProperty }

type
  TcxGridLevelImageIndexProperty = class(TImageIndexProperty)
  public
    function GetImages: TCustomImageList; override;
  end;

function TcxGridLevelImageIndexProperty.GetImages: TCustomImageList;
begin
  Result := TcxCustomGrid(TcxGridLevel(GetComponent(0)).Control).LevelTabs.Images;
end;

{ TcxCustomGridTableItemPropertiesProperty }

type
  TcxCustomGridTableItemPropertiesProperty = class(TClassProperty)
  protected
    function HasSubProperties: Boolean;
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

function TcxCustomGridTableItemPropertiesProperty.HasSubProperties: Boolean;
var
  I: Integer;
begin
  for I := 0 to PropCount - 1 do
  begin
    Result := TcxCustomGridTableItem(GetComponent(I)).Properties <> nil;
    if not Result then Exit;
  end;
  Result := True;
end;

function TcxCustomGridTableItemPropertiesProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes;
  if not HasSubProperties then
    Exclude(Result, paSubProperties);
  Result := Result - [paReadOnly] +
    [paValueList, paSortList, paRevertable, paVolatileSubProperties];
end;

function TcxCustomGridTableItemPropertiesProperty.GetValue: string;
begin
  if HasSubProperties then
    Result := GetRegisteredEditProperties.GetDescriptionByClass(
      TcxCustomGridTableItem(GetComponent(0)).Properties.ClassType)
  else
    Result := '';
end;

procedure TcxCustomGridTableItemPropertiesProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
  ADesc: string;
begin
  for I := 0 to GetRegisteredEditProperties.Count - 1 do
  begin
    ADesc := GetRegisteredEditProperties.Descriptions[I];
    if ADesc <> '' then
      Proc(ADesc);
  end;
end;

procedure TcxCustomGridTableItemPropertiesProperty.SetValue(const Value: string);
const
  scxDoesNotMatchPropertyType = 'The selected property type doesn' +
    '''t match the property type of the component introduced in an ancestor form';
var
  APropertiesClass: TcxCustomEditPropertiesClass;
  I: Integer;
  AItem: TcxCustomGridTableItem;
begin
  APropertiesClass := TcxCustomEditPropertiesClass(GetRegisteredEditProperties.FindByClassName(Value));
  if APropertiesClass = nil then
    APropertiesClass := TcxCustomEditPropertiesClass(GetRegisteredEditProperties.FindByDescription(Value));
  for I := 0 to PropCount - 1 do
  begin
    AItem := TcxCustomGridTableItem(GetComponent(I));
    if (AItem.PropertiesClass <> APropertiesClass) and
      (csAncestor in AItem.ComponentState) then
      raise Exception.Create(scxDoesNotMatchPropertyType);
    AItem.PropertiesClass := APropertiesClass;
  end;
  Modified;
end;

{$IFNDEF NONDB}

type
  TcxDBDataSummaryItemFieldNameProperty = class(TFieldNameProperty)
  public
    function GetDataSource: TDataSource; override;
  end;

function TcxDBDataSummaryItemFieldNameProperty.GetDataSource: TDataSource;
begin
  Result := (GetComponent(0) as TcxDBDataSummaryItem).DataController.DataSource;
end;

type
  TcxGridItemDBDataBindingFieldNameProperty = class(TFieldNameProperty)
  public
    function GetDataSource: TDataSource; override;
  end;

function TcxGridItemDBDataBindingFieldNameProperty.GetDataSource: TDataSource;
begin
  Result := TcxGridItemDBDataBinding(GetComponent(0)).DataController.DataSource;
end;

type
  TMasterKeyFieldNamesProperty = class(TFieldNameProperty)
    function GetDataSource: TDataSource; override;
  end;

function TMasterKeyFieldNamesProperty.GetDataSource: TDataSource;
var
  AIDataController: IcxCustomGridDataController;
  AParentLevel: TcxGridLevel;
begin
  Result := nil;
  if Supports(GetComponent(0), IcxCustomGridDataController, AIDataController) then
  begin
    AParentLevel := AIDataController.GridView.Level as TcxGridLevel;
    if AParentLevel <> nil then
      AParentLevel := AParentLevel.Parent;
    if (AParentLevel <> nil) and (AParentLevel.GridView <> nil) and
      (AParentLevel.GridView.DataController is TcxDBDataController) then
      Result := TcxDBDataController(AParentLevel.GridView.DataController).DataSource;
  end;
end;

{ TcxGridDBChartItemDataBindingFieldNameProperty }

type
  TcxGridDBChartItemDataBindingFieldNameProperty = class(TFieldNameProperty)
  public
    function GetDataSource: TDataSource; override;
    procedure GetValueList(AList: TStrings); override;
  end;

function TcxGridDBChartItemDataBindingFieldNameProperty.GetDataSource: TDataSource;
begin
  Result := TcxGridDBChartItemDataBinding(GetComponent(0)).DataController.DataSource;
end;

procedure TcxGridDBChartItemDataBindingFieldNameProperty.GetValueList(AList: TStrings);
var
  I, J: Integer;
  AField: TField;
  AValueTypeClass: TcxValueTypeClass;
begin
  inherited;
  for I := AList.Count - 1 downto 0 do
  begin
    AField := GetDataSource.DataSet.FindField(AList[I]);
    if AField = nil then
      AList.Delete(I)
    else
    begin
      AValueTypeClass := GetValueTypeClassByField(AField);
      for J := 0 to PropCount - 1 do
        if not (GetComponent(J) as TcxGridDBChartItemDataBinding).IsValueTypeClassValid(AValueTypeClass) then
        begin
          AList.Delete(I);
          Break;
        end;
    end;
  end;
end;

{$ENDIF}

{ TcxCustomGridColumnOptionsProperty }

type
  TcxCustomGridColumnOptionsProperty = class(TClassProperty)
  private
    FProc: TGetPropProc;

    function GetColumn: TcxCustomGridColumn;
    procedure GetPropProc(const Prop: IProperty);
    function IsVisibleProperty(const APropertyName: string): Boolean;
  public
    procedure GetProperties(Proc: TGetPropProc); override;

    property Column: TcxCustomGridColumn read GetColumn;
  end;

{ TcxCustomGridColumnOptionsProperty }

procedure TcxCustomGridColumnOptionsProperty.GetProperties(Proc: TGetPropProc);
begin
  FProc := Proc;
  inherited GetProperties(GetPropProc);
end;

function TcxCustomGridColumnOptionsProperty.GetColumn: TcxCustomGridColumn;
var
  APersistent: TPersistent;
begin
  APersistent := GetComponent(0);
  if APersistent is TcxCustomGridColumn then
    Result := TcxCustomGridColumn(APersistent)
  else
    Result := nil;
end;

procedure TcxCustomGridColumnOptionsProperty.GetPropProc(const Prop: IProperty);
begin
  if not IsVisibleProperty(Prop.GetName) then
    Exit;
  FProc(Prop);
end;

function TcxCustomGridColumnOptionsProperty.IsVisibleProperty(const APropertyName: string): Boolean;
begin
  Result := (APropertyName <> 'FilterRowOperator') or ((Column <> nil) and
    Column.GridView.FilterRow.OperatorCustomization);
end;

{ TcxCustomGridBandIndexProperty }

const
  sNoBand = '<none>';
  sIndexCaptionSeparator = ' - ';

type
  TcxCustomGridBandIndexProperty = class(TIntegerProperty)
  private
    function GetGridView: TcxGridBandedTableView;
  protected
    function GetGridViewForComponent(AIndex: Integer): TcxGridBandedTableView; virtual; abstract;
    function GetValueForBandIndex(AIndex: Integer): string;
    function IsSameGridView: Boolean;
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
    property GridView: TcxGridBandedTableView read GetGridView;
  end;

function TcxCustomGridBandIndexProperty.GetGridView: TcxGridBandedTableView;
begin
  Result := GetGridViewForComponent(0);
end;

function TcxCustomGridBandIndexProperty.GetValueForBandIndex(AIndex: Integer): string;
begin
  if AIndex = -1 then
    Result := sNoBand
  else
  begin
    Result := IntToStr(AIndex);
    if GridView.Bands[AIndex].Caption <> '' then
      Result := Result + sIndexCaptionSeparator + GridView.Bands[AIndex].Caption;
  end;
end;

function TcxCustomGridBandIndexProperty.IsSameGridView: Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 1 to PropCount - 1 do
  begin
    Result := GetGridViewForComponent(I) = GridView;
    if not Result then Break;
  end;
end;

function TcxCustomGridBandIndexProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes;
  if IsSameGridView then Include(Result, paValueList);
end;

function TcxCustomGridBandIndexProperty.GetValue: string;
begin
  if IsSameGridView or (GetOrdValue = -1) then
    Result := GetValueForBandIndex(GetOrdValue)
  else
    Result := inherited GetValue;
end;

procedure TcxCustomGridBandIndexProperty.GetValues(Proc: TGetStrProc);
begin
  Proc(GetValueForBandIndex(-1));
end;

procedure TcxCustomGridBandIndexProperty.SetValue(const Value: string);
var
  P: Integer;
begin
  if Value = sNoBand then
    SetOrdValue(-1)
  else
  begin
    P := Pos(sIndexCaptionSeparator, Value);
    if P = 0 then
      inherited SetValue(Value)
    else
      inherited SetValue(Copy(Value, 1, P - 1));
  end;
end;

{ TcxGridBandedColumnBandIndexProperty }

type
  TcxGridBandedColumnBandIndexProperty = class(TcxCustomGridBandIndexProperty)
  protected
    function GetGridViewForComponent(AIndex: Integer): TcxGridBandedTableView; override;
  public
    procedure GetValues(Proc: TGetStrProc); override;
  end;

function TcxGridBandedColumnBandIndexProperty.GetGridViewForComponent(AIndex: Integer): TcxGridBandedTableView;
begin
  Result := TcxGridBandedColumnPosition(GetComponent(AIndex)).GridView;
end;

procedure TcxGridBandedColumnBandIndexProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  inherited;
  for I := 0 to GridView.Bands.BottomItemCount - 1 do
    Proc(GetValueForBandIndex(GridView.Bands.BottomItems[I].Index));
end;

{ TcxGridBandBandIndexProperty }

type
  TcxGridBandBandIndexProperty = class(TcxCustomGridBandIndexProperty)
  protected
    function GetGridViewForComponent(AIndex: Integer): TcxGridBandedTableView; override;
  public
    procedure GetValues(Proc: TGetStrProc); override;
  end;

function TcxGridBandBandIndexProperty.GetGridViewForComponent(AIndex: Integer): TcxGridBandedTableView;
begin
  Result := TcxGridBandPosition(GetComponent(AIndex)).GridView;
end;

procedure TcxGridBandBandIndexProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;

  function IsBandSelected(ABandIndex: Integer): Boolean;
  var
    ABand: TcxGridBand;
    I: Integer;
  begin
    ABand := GridView.Bands[ABandIndex];
    for I := 0 to PropCount - 1 do
    begin
      Result := TcxGridBandPosition(GetComponent(I)).Band = ABand;
      if Result then Exit;
    end;
    Result := False;
  end;

begin
  inherited;
  for I := 0 to GridView.Bands.Count - 1 do
    if not IsBandSelected(I) then
      Proc(GetValueForBandIndex(I));
end;

{ TcxCustomGridTableItemProperty }

procedure TcxCustomGridTableItemProperty.GetValues(Proc: TGetStrProc);
var
  AGridView: TcxCustomGridView;
begin
  AGridView := GetGridView;
  if AGridView <> nil then
    GetGridViewItemNames(AGridView, Proc);
end;

function TcxCustomGridTableItemProperty.GetGridView: TcxCustomGridView;
var
  I: Integer;
begin
  Result := InternalGetGridView(GetComponent(0));
  for I := 1 to PropCount - 1 do
    if InternalGetGridView(GetComponent(I)) <> Result then
    begin
      Result := nil;
      Break;
    end;
end;

procedure TcxCustomGridTableItemProperty.GetGridViewItemNames(AGridView: TcxCustomGridView;
  Proc: TGetStrProc);
var
  I: Integer;
begin
  if AGridView is TcxCustomGridTableView then
    with AGridView as TcxCustomGridTableView do
      for I := 0 to ItemCount - 1 do
        Proc(Designer.GetComponentName(Items[I]));
end;

{ TcxGridIncSearchItemProperty }

type
  TcxGridIncSearchItemProperty = class(TcxCustomGridTableItemProperty)
  protected
    function InternalGetGridView(APersistent: TPersistent): TcxCustomGridView; override;
  end;

function TcxGridIncSearchItemProperty.InternalGetGridView(APersistent: TPersistent):
  TcxCustomGridView;
begin
  Result := TcxCustomGridTableOptionsBehavior(APersistent).GridView;
end;

{ TcxGridPreviewColumnProperty }

type
  TcxGridPreviewColumnProperty = class(TcxCustomGridTableItemProperty)
  protected
    function InternalGetGridView(APersistent: TPersistent): TcxCustomGridView; override;
  end;

function TcxGridPreviewColumnProperty.InternalGetGridView(APersistent: TPersistent):
  TcxCustomGridView;
begin
  Result := TcxGridPreview(APersistent).GridView;
end;

{ TcxGridTableSummaryItemColumnProperty }

function TcxGridTableSummaryItemColumnProperty.InternalGetGridView(APersistent: TPersistent):
  TcxCustomGridView;
begin
  Result := (TcxCustomDataSummaryItem(APersistent).DataController as
    IcxCustomGridDataController).GridView;
end;

{ TcxGridChartItemValueTypeProperty }

type
  TcxGridChartItemValueTypeProperty = class(TcxValueTypeProperty)
  protected
    function IsValueTypeClassValid(AValueTypeClass: TcxValueTypeClass): Boolean; override;
  end;

function TcxGridChartItemValueTypeProperty.IsValueTypeClassValid(AValueTypeClass: TcxValueTypeClass): Boolean;
var
  I: Integer;
begin
  Result := inherited IsValueTypeClassValid(AValueTypeClass);
  if Result then
    for I := 0 to PropCount - 1 do
    begin
      Result := (GetComponent(I) as TcxGridChartItemDataBinding).IsValueTypeClassValid(AValueTypeClass);
      if not Result then Break;
    end;
end;

{ TcxGridChartActiveDiagramProperty }

type
  TcxGridChartActiveDiagramProperty = class(TPropertyEditor)
  private
    function GetGridView: TcxGridChartView;
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
    property GridView: TcxGridChartView read GetGridView;
  end;

function TcxGridChartActiveDiagramProperty.GetGridView: TcxGridChartView;
begin
  Result := GetComponent(0) as TcxGridChartView;
end;

function TcxGridChartActiveDiagramProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList];
end;

function TcxGridChartActiveDiagramProperty.GetValue: string;
begin
  if GridView.ActiveDiagram = nil then
    Result := 'All diagrams are disabled'
  else
    Result := GridView.ActiveDiagram.DisplayText;
end;

procedure TcxGridChartActiveDiagramProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  for I := 0 to GridView.AvailableDiagramCount - 1 do
    Proc(GridView.AvailableDiagrams[I].DisplayText);
end;

procedure TcxGridChartActiveDiagramProperty.SetValue(const Value: string);
var
  I: Integer;
begin
  for I := 0 to GridView.AvailableDiagramCount - 1 do
    if GridView.AvailableDiagrams[I].DisplayText = Value then
    begin
      GridView.AvailableDiagrams[I].Active := True;
      Modified;
      Break;
    end;
end;

{$IFNDEF NONDB}

{ Ext Lookup }

type
  TcxExtLookupComboBoxPropertiesFieldNameProperty = class(TFieldNameProperty)
  public
    function GetDataSource: TDataSource; override;
  end;

  TcxExtLookupComboBoxPropertiesItemColumnProperty = class(TcxCustomGridTableItemProperty)
  protected
    function InternalGetGridView(APersistent: TPersistent): TcxCustomGridView; override;
  end;

  TcxExtLookupComboBoxPropertiesViewProperty = class(TComponentProperty)
  private
    FProc: TGetStrProc;
    procedure CheckComponent(const Value: string);
  public
    procedure GetValues(Proc: TGetStrProc); override;
  end;

function TcxExtLookupComboBoxPropertiesFieldNameProperty.GetDataSource: TDataSource;
var
  AProperties: TcxExtLookupComboBoxProperties;
begin
  AProperties := GetComponent(0) as TcxExtLookupComboBoxProperties;
  if AProperties.DataController <> nil then
    Result := AProperties.DataController.DataSource
  else
    Result := nil;
end;

function TcxExtLookupComboBoxPropertiesItemColumnProperty.InternalGetGridView(APersistent: TPersistent): TcxCustomGridView;
var
  AProperties: TcxExtLookupComboBoxProperties;
begin
  AProperties := APersistent as TcxExtLookupComboBoxProperties;
  Result := AProperties.View;
end;

procedure TcxExtLookupComboBoxPropertiesViewProperty.GetValues(Proc: TGetStrProc);
begin
  FProc := Proc;
  inherited GetValues(CheckComponent);
end;

procedure TcxExtLookupComboBoxPropertiesViewProperty.CheckComponent(const Value: string);
var
  AView: TcxCustomGridTableView;
begin
  AView := TcxCustomGridTableView(Designer.GetComponent(Value));
  if (AView <> nil) and TcxExtLookupComboBoxProperties.IsViewSupported(AView) then
    FProc(Value);
end;

{$ENDIF}

type
  TcxGridSelectionEditor = class(TSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

  TcxGridLevelSelectionEditor = class(TSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

  TcxCustomTableViewSelectionEditor = class(TSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

  TcxCustomDBTableViewSelectionEditor = class(TcxCustomTableViewSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

  TcxCustomGridTableItemSelectionEditor = class(TSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

procedure TcxGridSelectionEditor.RequiresUnits(Proc: TGetStrProc);
begin
  Proc('cxStyles');
  dxSkinsRequiresAdditionalUnits(TcxPageControl, Proc);
end;

procedure TcxGridLevelSelectionEditor.RequiresUnits(Proc: TGetStrProc);
begin
  Proc('cxStyles');
end;

procedure TcxCustomTableViewSelectionEditor.RequiresUnits(Proc: TGetStrProc);
begin
  Proc('cxStyles');
  Proc('cxCustomData');
  Proc('cxGraphics');
  Proc('cxFilter');
  Proc('cxData');
  Proc('cxDataStorage');
  Proc('cxEdit');
  Proc('cxNavigator');
  Proc('dxDateRanges');
  TcxDataControllerConditionalFormattingRulesManagerDialogProvider.RequiresRulesManagerDialogUnits(Designer.Root, Proc);
end;

procedure TcxCustomDBTableViewSelectionEditor.RequiresUnits(Proc: TGetStrProc);
begin
  inherited RequiresUnits(Proc);
{$IFDEF DELPHI16}
  Proc('Data.DB');
{$ELSE}
  Proc('DB');
{$ENDIF}
  Proc('cxDBData');
end;

procedure TcxCustomGridTableItemSelectionEditor.RequiresUnits(Proc: TGetStrProc);
var
  I: Integer;
  AComponent: TComponent;
  AItem: TcxCustomGridTableItem;
begin
  for I := 0 to Designer.Root.ComponentCount - 1 do
  begin
    AComponent := Designer.Root.Components[I];
    if AComponent is TcxCustomGridTableItem then
    begin
      AItem := TcxCustomGridTableItem(AComponent);
      if AItem.Properties <> nil then
        Proc(cxGetUnitName(AItem.Properties.ClassType));
    end;
  end;
end;

{ TcxGridChartViewSelectionEditor }

type
  TcxGridChartViewSelectionEditor = class(TSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

procedure TcxGridChartViewSelectionEditor.RequiresUnits(Proc: TGetStrProc);
begin
  inherited;
  Proc('cxStyles');
  Proc('cxCustomData');
  Proc('cxGraphics');
end;

{$IFNDEF NONDB}

{ TcxGridDBChartViewSelectionEditor }

type
  TcxGridDBChartViewSelectionEditor = class(TcxGridChartViewSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

procedure TcxGridDBChartViewSelectionEditor.RequiresUnits(Proc: TGetStrProc);
begin
  inherited;
{$IFDEF DELPHI16}
  Proc('Data.DB');
{$ELSE}
  Proc('DB');
{$ENDIF}
  Proc('cxDBData');
end;

{$ENDIF}

type

{ TcxImageComboBoxItemsProperty }

  TcxImageComboBoxItemsProperty = class(TPropertyEditor)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
  end;

procedure TcxImageComboBoxItemsProperty.Edit;
var
  S: string;
  AProperties: TcxImageComboBoxProperties;
begin
  S := 'Properties' + '.' + GetName;
  AProperties := TcxImageComboBoxProperties(GetComponent(0));
  if (AProperties.Owner <> nil) and (AProperties.Owner is TComponent) then
    S := TComponent(AProperties.Owner).Name + '.' + S;
  with TfmImageComboBoxItemsEditor.Create(
    AProperties.Items, AProperties.Images, AProperties.LargeImages) do
  try
    Caption := S;
    if ShowModal = mrOk then
      Self.Designer.Modified;
  finally
    Free;
  end;
end;

function TcxImageComboBoxItemsProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

function TcxImageComboBoxItemsProperty.GetValue: string;
begin
  FmtStr(Result, '(%s)', [GetPropType^.Name]);
end;

///////////////////////////// GRID 7 ///////////////////////////////////////

{ TcxGridViewNavigatorEventsProperty }

type
  TcxGridViewNavigatorEventsProperty = class(TcxNestedEventProperty)
  protected
    function GetInstance: TPersistent; override;
  end;

function TcxGridViewNavigatorEventsProperty.GetInstance: TPersistent;
begin
  Result := TcxCustomGridTableView(GetComponent(0)).Navigator;
end;

{ TcxGridViewNavigatorButtonsEventsProperty }
{
type
  TcxGridViewNavigatorButtonsEventsProperty = class(TcxNestedEventProperty)
  protected
    function GetInstance: TPersistent; override;
  end;

function TcxGridViewNavigatorButtonsEventsProperty.GetInstance: TPersistent;
begin
  Result := TcxCustomGridTableView(GetComponent(0)).Navigator.Buttons;
end;
}

type
  TcxDataGroupSummaryItemsPropertyEditor = class(TCollectionProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
  end;

{ TcxDataGroupSummaryItemsPropertyEditor }

function TcxDataGroupSummaryItemsPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paSubProperties];
end;

////////////////////////////////////////////////////////////////////////////////

type

  { TcxGridColumnLayoutItemPropertyFilter }

  TcxGridColumnLayoutItemPropertyFilter = class(TSelectionEditor, ISelectionPropertyFilter)
  private
    procedure FilterProperties(const ASelection: IDesignerSelections; const ASelectionProperties: IInterfaceList);
  end;

  { TGridColumnLayoutItemProperty }

  TGridColumnLayoutItemProperty = class(TClassProperty)
  private
    function GetColumn: TcxGridColumn;
    function IsLayoutItemAvailable: Boolean;
    function GetLayoutItem: TcxGridInplaceEditFormLayoutItem;
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;

    property Column: TcxGridColumn read GetColumn;
    property LayoutItem: TcxGridInplaceEditFormLayoutItem read GetLayoutItem;
  end;

  { TGridColumnLayoutItemProperty }

  TGridColumnOptionsFilterRowOperatorProperty = class(TEnumProperty)
  private
    function GetColumn: TcxCustomGridColumn;
  public
    procedure GetValues(Proc: TGetStrProc); override;

    property Column: TcxCustomGridColumn read GetColumn;
  end;

  { TGridFindPanelOptionsProperty }

  TGridFindPanelOptionsProperty = class(TClassProperty)
  private
    function IsFindPanelAvailable: Boolean;
    function GetFindPanelOptions: TcxGridFindPanelOptions;
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;

    property FindPanelOptions: TcxGridFindPanelOptions read GetFindPanelOptions;
  end;

  { TGridColumnVisibleForEditFormPropertyEditor }

  TGridColumnVisibleForEditFormPropertyEditor = class(TdxDefaultBooleanPropertyEditor)
  protected
    function GetDefaultValue: Boolean; override;
  end;

{ TcxGridColumnLayoutItemPropertyFilter }

procedure TcxGridColumnLayoutItemPropertyFilter.FilterProperties(const ASelection:
  IDesignerSelections; const ASelectionProperties: IInterfaceList);
var
  AProperty: IProperty;
  I: Integer;
  AUseDefaultLayout: Boolean;
begin
  if ASelection.Count <> 1 then
    Exit;

  if (ASelection[0] is TcxGridColumn) then // well it should be we are only registered for TControl
  begin
    AUseDefaultLayout := TcxGridColumn(ASelection[0]).GridView.EditForm.UseDefaultLayout;
    if AUseDefaultLayout then
    begin
      for I := 0 to ASelectionProperties.Count - 1 do
      begin
        AProperty := ASelectionProperties[I] as IProperty;
        if AProperty <> nil then
        begin
          if AProperty.GetName = 'LayoutItem' then
          begin
            ASelectionProperties.Delete(I);
            Break;
          end;
        end;
      end;
    end
  end;
end;

{ TGridColumnLayoutItemProperty }

function TGridColumnLayoutItemProperty.GetAttributes: TPropertyAttributes;
begin
  if IsLayoutItemAvailable then
    Result := [paReadOnly, paSubProperties, paVolatileSubProperties]
  else
    Result := [paReadOnly];
end;

function TGridColumnLayoutItemProperty.GetColumn: TcxGridColumn;
begin
  if TComponent(GetOrdValue) is TcxGridColumn then
    Result := TcxGridColumn(GetOrdValue)
  else
    Result := nil;
end;

function TGridColumnLayoutItemProperty.GetLayoutItem: TcxGridInplaceEditFormLayoutItem;
begin
  if TComponent(GetOrdValue) is TcxGridInplaceEditFormLayoutItem then
    Result := TcxGridInplaceEditFormLayoutItem(GetOrdValue)
  else
    Result := nil;
end;

function TGridColumnLayoutItemProperty.GetValue: string;
begin
  if IsLayoutItemAvailable then
    Result := inherited GetValue
  else
    Result := '(not available)';
end;

function TGridColumnLayoutItemProperty.IsLayoutItemAvailable: Boolean;
begin
  Result := (LayoutItem <> nil) and
    not TcxGridColumn(LayoutItem.GridViewItem).GridView.EditForm.UseDefaultLayout;
end;

{ TGridColumnOptionsFilterRowOperatorProperty }

function TGridColumnOptionsFilterRowOperatorProperty.GetColumn: TcxCustomGridColumn;
var
  APersistent: TPersistent;
begin
  APersistent := GetComponent(0);
  if APersistent is TcxCustomGridColumnOptions then
    Result := TcxCustomGridColumnOptions(APersistent).Item
  else
    Result := nil;
end;

procedure TGridColumnOptionsFilterRowOperatorProperty.GetValues(Proc: TGetStrProc);
var
  AOperator: TcxFilterOperatorKind;
begin
  for AOperator := Low(TcxFilterOperatorKind) to High(TcxFilterOperatorKind) do
    if (Column <> nil) and Column.GridView.Controller.IsFilterRowOperatorSupported(Column.Index, AOperator) then
      Proc(GetEnumName(GetPropType, Ord(AOperator)));
end;

{ TGridFindPanelOptionsProperty }

function TGridFindPanelOptionsProperty.GetAttributes: TPropertyAttributes;
begin
  if IsFindPanelAvailable then
    Result := [paReadOnly, paSubProperties, paVolatileSubProperties]
  else
    Result := [paReadOnly];
end;

function TGridFindPanelOptionsProperty.GetFindPanelOptions: TcxGridFindPanelOptions;
begin
  if TPersistent(GetOrdValue) is TcxGridFindPanelOptions then
    Result := TcxGridFindPanelOptions(GetOrdValue)
  else
    Result := nil;
end;

function TGridFindPanelOptionsProperty.GetValue: string;
begin
  if IsFindPanelAvailable then
    Result := inherited GetValue
  else
    Result := '(not available)';
end;

function TGridFindPanelOptionsProperty.IsFindPanelAvailable: Boolean;
begin
  Result := FindPanelOptions <> nil;
end;

{ TGridColumnVisibleForEditFormPropertyEditor }

function TGridColumnVisibleForEditFormPropertyEditor.GetDefaultValue: Boolean;
begin
  if GetComponent(0) is TcxGridColumn then
    Result := TcxGridColumn(GetComponent(0)).Visible
  else
    Result := True;
end;

procedure Register;
begin
  ForceDemandLoadState(dlDisable);

  RegisterComponentEditor(TcxGrid, TGridEditor);
  RegisterComponentEditor(TcxGridViewRepository, TGridViewRepositoryEditor);

  RegisterPropertyEditor(TypeInfo(TNotifyEvent), TcxCustomGrid, 'RootLevelStylesEvents',
    TcxGridRootLevelStylesEventsProperty);
  RegisterPropertyEditor(TypeInfo(TNotifyEvent), TcxCustomGridTableItem, 'PropertiesEvents',
    TcxGridTableItemPropertiesEventsProperty);
  RegisterPropertyEditor(TypeInfo(TNotifyEvent), TcxCustomGridTableItem, 'StylesEvents',
    TcxGridTableItemStylesEventsProperty);
  RegisterPropertyEditor(TypeInfo(TNotifyEvent), TcxCustomGridView, 'DataControllerEvents',
    TcxGridDataControllerEventsProperty);
  RegisterPropertyEditor(TypeInfo(TNotifyEvent), TcxCustomGridView, 'StylesEvents',
    TcxGridViewStylesEventsProperty);
  RegisterPropertyEditor(TypeInfo(TNotifyEvent), TcxGridLevel, 'StylesEvents',
    TcxGridLevelStylesEventsProperty);
  RegisterPropertyEditor(TypeInfo(TNotifyEvent), TcxGridBand, 'StylesEvents',
    TcxGridBandStylesEventsProperty);
  RegisterPropertyEditor(TypeInfo(TNotifyEvent), TcxGridChartSeries, 'StylesEvents',
    TcxGridChartSeriesStylesEventsProperty);
  RegisterPropertyEditor(TypeInfo(TNotifyEvent), TcxGridChartHistogram, 'ValuesEvents',
    TcxGridChartHistogramValuesEventsProperty);
  RegisterPropertyEditor(TypeInfo(TNotifyEvent), TcxGridChartView, 'CategoriesEvents',
    TcxGridChartCategoriesEventsProperty);
  RegisterPropertyEditor(TypeInfo(TNotifyEvent), TcxGridChartView, 'DiagramAreaEvents',
    TcxGridChartAreaDiagramEventsProperty);
  RegisterPropertyEditor(TypeInfo(TNotifyEvent), TcxGridChartView, 'DiagramBarEvents',
    TcxGridChartBarDiagramEventsProperty);
  RegisterPropertyEditor(TypeInfo(TNotifyEvent), TcxGridChartView, 'DiagramStackedBarEvents',
    TcxGridChartStackedBarDiagramEventsProperty);
  RegisterPropertyEditor(TypeInfo(TNotifyEvent), TcxGridChartView, 'DiagramStackedColumnEvents',
    TcxGridChartStackedColumnDiagramEventsProperty);
  RegisterPropertyEditor(TypeInfo(TNotifyEvent), TcxGridChartView, 'DiagramStackedAreaEvents',
    TcxGridChartStackedAreaDiagramEventsProperty);
  RegisterPropertyEditor(TypeInfo(TNotifyEvent), TcxGridChartView, 'DiagramColumnEvents',
    TcxGridChartColumnDiagramEventsProperty);
  RegisterPropertyEditor(TypeInfo(TNotifyEvent), TcxGridChartView, 'DiagramLineEvents',
    TcxGridChartLineDiagramEventsProperty);
  RegisterPropertyEditor(TypeInfo(TNotifyEvent), TcxGridChartView, 'DiagramPieEvents',
    TcxGridChartPieDiagramEventsProperty);

  RegisterPropertyEditor(TypeInfo(TcxCustomGridView), TcxGridLevel, 'GridView',
    TcxGridLevelGridViewProperty);
  RegisterPropertyEditor(TypeInfo(TcxImageIndex), TcxGridLevel, 'ImageIndex',
    TcxGridLevelImageIndexProperty);
  RegisterPropertyEditor(TypeInfo(string), TcxCustomGridTableItem, 'PropertiesClassName', nil);
  RegisterPropertyEditor(TypeInfo(TcxCustomEditProperties), TcxCustomGridTableItem,
    'Properties', TcxCustomGridTableItemPropertiesProperty);
  RegisterPropertyEditor(TypeInfo(string), TcxGridItemDataBinding, 'ValueType',
    TcxValueTypeProperty);
  RegisterPropertyEditor(TypeInfo(TComponent), TcxCustomGridView, 'PopupMenu',
    TcxControlPopupMenuProperty);

  RegisterPropertyEditor(TypeInfo(Boolean), TcxCustomGridTableOptionsSelection, 'HideFocusRect', nil);
  RegisterPropertyEditor(TypeInfo(TAlignment), TcxGridBand, 'Alignment', nil);

  RegisterPropertyEditor(TypeInfo(Integer), TcxGridBandedColumnPosition, 'BandIndex',
    TcxGridBandedColumnBandIndexProperty);
  RegisterPropertyEditor(TypeInfo(Integer), TcxGridBandPosition, 'BandIndex',
    TcxGridBandBandIndexProperty);
  RegisterPropertyEditor(TypeInfo(string), TcxGridChartItemDataBinding, 'ValueType',
    TcxGridChartItemValueTypeProperty);
  RegisterPropertyEditor(TypeInfo(TcxGridChartDiagram), TcxGridChartView, 'ActiveDiagram',
    TcxGridChartActiveDiagramProperty);

  RegisterPropertyEditor(TypeInfo(TcxCustomGridTableItem), TcxCustomGridTableOptionsBehavior,
    'IncSearchItem', TcxGridIncSearchItemProperty);
  RegisterPropertyEditor(TypeInfo(TcxCustomGridTableItem), TcxGridPreview,
    'Column', TcxGridPreviewColumnProperty);
  RegisterPropertyEditor(TypeInfo(TcxCustomGridTableItem), TcxGridTableSummaryItem,
    'Column', TcxGridTableSummaryItemColumnProperty);
{$IFNDEF NONDB}
  RegisterPropertyEditor(TypeInfo(TcxCustomGridTableItem), TcxGridDBTableSummaryItem,
    'Column', TcxGridTableSummaryItemColumnProperty);
{$ENDIF}
  RegisterPropertyEditor(TypeInfo(TcxCustomGridTableItem), TcxGridTableSummaryGroupItemLink,
    'Column', TcxGridTableSummaryItemColumnProperty);

{$IFNDEF NONDB}
  RegisterPropertyEditor(TypeInfo(string), TcxDBDataController, 'DetailKeyFieldNames', TFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TcxDBDataController, 'KeyFieldNames', TFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TcxDBDataController, 'MasterKeyFieldNames', TMasterKeyFieldNamesProperty);
  RegisterPropertyEditor(TypeInfo(string), TcxDBDataSummaryItem, 'FieldName', TcxDBDataSummaryItemFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TcxGridItemDBDataBinding, 'FieldName', TcxGridItemDBDataBindingFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TcxGridDBChartItemDataBinding, 'FieldName', TcxGridDBChartItemDataBindingFieldNameProperty);
{$ENDIF}

  RegisterNoIcon([TcxGridLevel,
    TcxGridTableView, {$IFNDEF NONDB}TcxGridDBTableView, {$ENDIF}
    TcxGridBandedTableView, {$IFNDEF NONDB}TcxGridDBBandedTableView,{$ENDIF}
    TcxGridChartView, {$IFNDEF NONDB}TcxGridDBChartView,{$ENDIF}
    TcxGridCardView{$IFNDEF NONDB}, TcxGridDBCardView{$ENDIF}]);
  RegisterNoIcon([
    TcxGridColumn, {$IFNDEF NONDB}TcxGridDBColumn,{$ENDIF}
    TcxGridBandedColumn, {$IFNDEF NONDB}TcxGridDBBandedColumn,{$ENDIF}
    TcxGridCardViewRow, {$IFNDEF NONDB}TcxGridDBCardViewRow,{$ENDIF}
    TcxGridChartDataGroup, {$IFNDEF NONDB}TcxGridDBChartDataGroup,{$ENDIF}
    TcxGridChartSeries{$IFNDEF NONDB}, TcxGridDBChartSeries{$ENDIF}]);
  RegisterNoIcon([TcxGridTableViewStyleSheet, TcxGridBandedTableViewStyleSheet, TcxGridCardViewStyleSheet]);

  RegisterComponents(dxCoreLibraryProductPage, [TcxGrid, TcxGridViewRepository]);
{$IFNDEF NONDB}
  // Ext Lookup
  RegisterComponents(cxEditorsLibraryProductPage, [TcxExtLookupComboBox]);
  RegisterComponents(cxEditorsDBLibraryProductPage, [TcxDBExtLookupComboBox]);
  RegisterEditRepositoryItem(TcxEditRepositoryExtLookupComboBoxItem, cxSEditRepositoryExtLookupComboBoxItem);
  RegisterPropertyEditor(TypeInfo(string), TcxExtLookupComboBoxProperties, 'KeyFieldNames', TcxExtLookupComboBoxPropertiesFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(TcxCustomGridTableItem), TcxExtLookupComboBoxProperties, 'ListFieldItem', TcxExtLookupComboBoxPropertiesItemColumnProperty);
  RegisterPropertyEditor(TypeInfo(TcxCustomGridTableView), TcxExtLookupComboBoxProperties, 'View', TcxExtLookupComboBoxPropertiesViewProperty);
  RegisterPropertyEditor(TypeInfo(TShortCut), TcxExtLookupComboBoxProperties, 'ClearKey', TShortCutProperty);
{$ENDIF}

  RegisterSelectionEditor(TcxCustomGrid, TcxGridSelectionEditor);
  RegisterSelectionEditor(TcxGridLevel, TcxGridLevelSelectionEditor);
  RegisterSelectionEditor(TcxCustomGridTableView, TcxCustomTableViewSelectionEditor);
  RegisterSelectionEditor(TcxGridChartView, TcxGridChartViewSelectionEditor);
{$IFNDEF NONDB}
  RegisterSelectionEditor(TcxGridDBTableView, TcxCustomDBTableViewSelectionEditor);
  RegisterSelectionEditor(TcxGridDBBandedTableView, TcxCustomDBTableViewSelectionEditor);
  RegisterSelectionEditor(TcxGridDBCardView, TcxCustomDBTableViewSelectionEditor);
  RegisterSelectionEditor(TcxGridDBChartView, TcxGridDBChartViewSelectionEditor);
{$ENDIF}

  RegisterSelectionEditor(TcxCustomGridTableItem, TcxCustomGridTableItemSelectionEditor);

  RegisterPropertyEditor(TypeInfo(TcxImageComboBoxItems),
    TcxImageComboBoxProperties, 'Items', TcxImageComboBoxItemsProperty);

/////////////////////////////////////////// GRID 7 /////////////////////////////////////////////////
  RegisterPropertyEditor(TypeInfo(TPersistent), TcxCustomGridTableView, 'NavigatorButtons', nil);
  RegisterPropertyEditor(TypeInfo(TPersistent), TcxCustomGridTableOptionsView, 'Navigator', nil);
  RegisterPropertyEditor(TypeInfo(TNotifyEvent), TcxCustomGridTableView, 'NavigatorEvents',
    TcxGridViewNavigatorEventsProperty);
  //RegisterPropertyEditor(TypeInfo(TNotifyEvent), TcxCustomGridTableView, 'NavigatorButtonsEvents',
  //  TcxGridViewNavigatorButtonsEventsProperty);

  RegisterPropertyEditor(TypeInfo(TcxDataGroupSummaryItems), TcxDataSummary, '',
    TcxDataGroupSummaryItemsPropertyEditor);
////////////////////////////////////////// 14.1 ////////////////////////////////////////////////////
  DesignIntf.RegisterSelectionEditor(TcxGridColumn, TcxGridColumnLayoutItemPropertyFilter);
  RegisterNoIcon([TcxGridInplaceEditFormLayoutItem, TcxGridInplaceEditFormGroup]);
  HideClassProperties(TcxGridInplaceEditFormLayoutItem, ['LayoutLookAndFeel', 'Name']);
  dxHideClassesFromStructureView([TcxGridCustomLayoutItem, TcxGridInplaceEditFormGroup]);

  RegisterPropertyEditor(TypeInfo(TcxGridFindPanelOptions), TcxCustomGridTableView, 'FindPanel', TGridFindPanelOptionsProperty);
  RegisterPropertyEditor(TypeInfo(TcxGridInplaceEditFormLayoutItem), TcxGridColumn, 'LayoutItem', TGridColumnLayoutItemProperty);
  RegisterPropertyEditor(TypeInfo(TdxDefaultBoolean), TcxGridColumn, 'VisibleForEditForm', TGridColumnVisibleForEditFormPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TcxCustomGridColumnOptions), TcxGridColumn, 'Options', TcxCustomGridColumnOptionsProperty);
  RegisterPropertyEditor(TypeInfo(TcxFilterOperatorKind), TcxCustomGridColumnOptions, 'FilterRowOperator', TGridColumnOptionsFilterRowOperatorProperty);
end;

initialization
  StartClassGroup(TControl);
  GroupDescendentsWith(TcxGrid, TControl);
  GroupDescendentsWith(TcxGridViewRepository, TControl);
  GroupDescendentsWith(TcxGridLevel, TControl);
  GroupDescendentsWith(TcxCustomGridView, TControl);
  GroupDescendentsWith(TcxCustomGridTableItem, TControl);
  GroupDescendentsWith(TcxGridItemDataBinding, TControl);
  GroupDescendentsWith(TcxGridChartItem, TControl);

  RegisterStyleSheetClass(TcxGridTableViewStyleSheet);
  RegisterStyleSheetClass(TcxGridBandedTableViewStyleSheet);
  RegisterStyleSheetClass(TcxGridCardViewStyleSheet);

finalization
  UnregisterStyleSheetClass(TcxGridCardViewStyleSheet);
  UnregisterStyleSheetClass(TcxGridBandedTableViewStyleSheet);
  UnregisterStyleSheetClass(TcxGridTableViewStyleSheet);

end.
