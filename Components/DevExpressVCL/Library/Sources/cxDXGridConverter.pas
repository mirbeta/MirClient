{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressQuantumGrid                                       }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSQUANTUMGRID AND ALL            }
{   ACCOMPANYING VCL AND CLX CONTROLS AS PART OF AN EXECUTABLE       }
{   PROGRAM ONLY.                                                    }
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
unit cxDXGridConverter;

interface

{$I cxVer.inc}

uses
  Forms, Classes, SysUtils, cxConverterFactory, cxGridConverter, cxGridCustomView,
  cxGridDBTableView, cxGridDBBandedTableView, cxGridBandedTableView, DB, Controls,
  cxCustomData, cxGraphics, cxGridCustomTableView, cxFilter, cxGrid, cxLookAndFeels,
  cxEdit, cxBlobEdit, cxButtonEdit, cxCalc, cxCalendar, cxCheckBox, cxCurrencyEdit,
  cxDropDownEdit, cxHyperLinkEdit, cxImage, cxImageComboBox, cxDBLookupComboBox,
  cxMaskEdit, cxMemo, cxMRUEdit, cxSpinEdit, cxTimeEdit, cxTextEdit, StdCtrls, Graphics,
  cxGridTableView, cxDBData, cxStyles, cxConverterUtils, cxCustomConverter,
  cxPropertiesConverters;

type
  { TcxDXGridConverter }
  TcxDXGridConverter = class(TcxCustomGridConverter)
  protected
    procedure DoRealImport; override;
    procedure DoImportStyles; override;
    function GetGridViewClass: TcxCustomGridViewClass; override;
  public
    class function GetSourceClassName: string; override;
  end;

  { TcxDXGridLayoutConverter }
  TcxDXGridLayoutConverter = class(TcxDXGridConverter)
  private
    FdxDBGrid: TComponent;
    FComponent: TComponent;
  protected
    procedure DoRealImport; override;
    function GetGridViewClass: TcxCustomGridViewClass; override;
    procedure PostImport; override;
    procedure PreImport; override;
    function TestIntermediary: Boolean; override;
  public
    class function GetIntermediaryClassName: string; override;
    class function GetSourceClassName: string; override;
  end;

implementation

uses
  dxCore;

type
  { TcxRealConverterToTableView }
  TcxRealConverterToTableView = class
  private
    FColor: Integer;
    FFont: TFont;
    FHeaderColor: Integer;
    FHeaderFont: TFont;
    FcxSummaryGroups: TStringList;
    function DefaultPreviewFont(AFont: TFont): Boolean;
    function GetColumn(const AName: string): TComponent;
    procedure GetColumns(AColumns: TList);
    function GetCXColumnByFieldName(const AFieldName: string): TcxGridDBColumn;
    function GetcxGrid: TcxCustomGrid;
    function GetcxGridView: TcxGridTableView;
  protected
    Converter: TcxDXGridConverter;
    procedure AssignColumn(AColumn: TcxGridColumn; ADXColumn: TObject; AIndex: Integer); virtual;
    procedure AssignDXOptionsView(AList: TStringList); virtual;
    procedure AssignGrid; virtual;
    procedure AssignSummaryItem(AColumn: TcxGridColumn; ADXColumn: TObject); virtual;
    function CreateColumn: TcxGridColumn; virtual;
    function ConvertColumnShowEditButtons(const AStyle: string): TcxGridItemShowEditButtons;
    function GetSummaryKind(const AType: string): TcxSummaryKind;
    procedure ImportAutoDataSetFilter(const AValue: Boolean); virtual;
    procedure ImportColumn(AdxColumn: TObject; AcxColumn: TcxGridColumn);
    procedure ImportColumns;
    procedure ImportColumnsStyles;
    procedure ImportColumnSummaryGroups; virtual;
    procedure ImportDXFilter;
    procedure ImportDXOptionsBehavior;
    procedure ImportDXOptionsCustomize; virtual;
    procedure ImportDXOptionsDB;
    procedure ImportDXOptionsView;
    procedure ImportGrid;
    procedure ImportGridStyles;
    procedure ImportPreview;
    procedure ImportPropertiesBlobEdit(AdxColumn: TObject; AcxColumn: TcxGridColumn);
    procedure ImportPropertiesButtonEdit(AdxColumn: TObject; AcxColumn: TcxGridColumn);
    procedure ImportPropertiesCalcEdit(AdxColumn: TObject; AcxColumn: TcxGridColumn);
    procedure ImportPropertiesCheckBox(AdxColumn: TObject; AcxColumn: TcxGridColumn);
    procedure ImportPropertiesComboBox(AdxColumn: TObject; AcxColumn: TcxGridColumn);
    procedure ImportPropertiesCurrencyEdit(AdxColumn: TObject; AcxColumn: TcxGridColumn);
    procedure ImportPropertiesDateEdit(AdxColumn: TObject; AcxColumn: TcxGridColumn);
    procedure ImportPropertiesHyperLinkEdit(AdxColumn: TObject; AcxColumn: TcxGridColumn);
    procedure ImportPropertiesImage(AdxColumn: TObject; AcxColumn: TcxGridColumn);
    procedure ImportPropertiesImageComboBox(AdxColumn: TObject; AcxColumn: TcxGridColumn);
    procedure ImportPropertiesLookupComboBox(AdxColumn: TObject; AcxColumn: TcxGridColumn);
    procedure ImportPropertiesExLookupComboBox(AdxColumn: TObject; AcxColumn: TcxGridColumn);
    procedure ImportPropertiesMaskEdit(AdxColumn: TObject; AcxColumn: TcxGridColumn);
    procedure ImportPropertiesMemo(AdxColumn: TObject; AcxColumn: TcxGridColumn);
    procedure ImportPropertiesMRUEdit(AdxColumn: TObject; AcxColumn: TcxGridColumn);
    procedure ImportPropertiesPopupEdit(AdxColumn: TObject; AcxColumn: TcxGridColumn);
    procedure ImportPropertiesSpinEdit(AdxColumn: TObject; AcxColumn: TcxGridColumn);
    procedure ImportPropertiesTextEdit(AdxColumn: TObject; AcxColumn: TcxGridColumn);
    procedure ImportPropertiesTimeEdit(AdxColumn: TObject; AcxColumn: TcxGridColumn);
    procedure ImportSummaryGroups; virtual;
    procedure ImportSyncMode(const AValue: Boolean); virtual;
    function TestPreviewFieldName(AIndex: Integer; const APreview: string): Boolean; virtual;
    property cxGrid: TcxCustomGrid read GetcxGrid;
    property cxGridView: TcxGridTableView read GetcxGridView;
  public
    constructor Create(AConverter: TcxDXGridConverter);
    destructor Destroy; override;
    procedure DoImport; virtual;
    procedure DoImportStyles; virtual;
  end;

  { TcxRealConverterToBandedTableView }
  TcxRealConverterToBandedTableView = class(TcxRealConverterToTableView)
  private
    FBandHeaderWidth: Boolean;
    function GetCXColumnByFieldName(const AFieldName: string): TcxGridDBBandedColumn;
    function GetcxGridView: TcxGridBandedTableView;
  protected
    procedure AssignColumn(AColumn: TcxGridColumn; ADXColumn: TObject; AIndex: Integer); override;
    procedure ImportAutoDataSetFilter(const AValue: Boolean); override;
    procedure ImportColumnSummaryGroups; override;
    procedure ImportDXOptionsCustomize; override;
    procedure AssignDXOptionsView(AList: TStringList); override;
    procedure AssignGrid; override;
    procedure AssignSummaryItem(AColumn: TcxGridColumn; ADXColumn: TObject); override;
    function CreateColumn: TcxGridColumn; override;
    procedure ImportColumnsStyles;
    procedure ImportBands;
    procedure ImportBandsStyles;
    procedure ImportSummaryGroups; override;
    procedure ImportSyncMode(const AValue: Boolean); override;
    function TestPreviewFieldName(AIndex: Integer; const APreview: string): Boolean; override;
    property cxGridView: TcxGridBandedTableView read GetcxGridView;
  public
    procedure DoImport; override;
    procedure DoImportStyles; override;
  end;

  { TcxDXGridLayoutDataReader }
  TcxDXGridLayoutDataReader = class(TReader)
  protected
    procedure SetName(Component: TComponent; var Name: string); override;
  end;

  { TcxDXGridLayoutDummy }
  TcxDXGridLayoutDummy = class(TComponent)
  private
    FComponent: TComponent;
    FdxDBGrid: TComponent;
    FIntermediary: TComponent;
    procedure ReadData(Stream: TStream);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  public
    constructor Create(AOwner: TComponent); override;
    property Component: TComponent read FComponent write FComponent;
    property dxDBGrid: TComponent read FdxDBGrid write FdxDbGrid;
    property Intermediary: TComponent read FIntermediary write FIntermediary;
  end;

{ TcxRealConverterToTableView }

constructor TcxRealConverterToTableView.Create(AConverter: TcxDXGridConverter);
begin
  FcxSummaryGroups := TStringList.Create;
  Converter := AConverter;
end;

destructor TcxRealConverterToTableView.Destroy;
begin
  FcxSummaryGroups.Free;
  inherited Destroy;
end;

procedure TcxRealConverterToTableView.DoImport;
begin
  TcxGridDBTableView(cxGridView).DataController.DataSource :=
    Converter.GetClassProperty(nil, 'DataSource') as TDataSource;
  ImportDXOptionsView;
  ImportDXFilter;
  ImportColumns;
  TcxGridDBTableView(cxGridView).DataController.KeyFieldNames :=
    Converter.GetStringProperty(nil, 'KeyField', '');
  ImportGrid;
end;

procedure TcxRealConverterToTableView.DoImportStyles;
begin
  ImportGridStyles;
  ImportColumnsStyles;
end;

procedure TcxRealConverterToTableView.AssignColumn(AColumn: TcxGridColumn; ADXColumn: TObject;
  AIndex: Integer);
var
  ASortOrder: string;
begin
  with TcxGridDBColumn(AColumn) do
  begin
    DataBinding.FieldName := Converter.GetStringProperty(ADXColumn, 'FieldName', 'Field' + IntToStr(AIndex));
    Name := Converter.UniqueColumnName(AColumn, DataBinding.FieldName);
    Caption := Converter.GetStringProperty(ADXColumn, 'Caption', Caption);
    Visible := Converter.GetBooleanProperty(ADXColumn, 'Visible', Visible);
    GroupIndex := Converter.GetIntegerProperty(ADXColumn, 'GroupIndex', GroupIndex);
    Width := Converter.GetIntegerProperty(ADXColumn, 'Width', Width);
    ASortOrder := Converter.GetEnumProperty(ADXColumn, 'Sorted');
    if ASortOrder = 'csDown' then
      SortOrder := soDescending
    else if ASortOrder = 'csNone' then
      SortOrder := soNone
    else if ASortOrder = 'csUp' then
      SortOrder := soAscending;
    Options.Moving := not Converter.GetBooleanProperty(ADXColumn, 'DisableDragging', Options.Moving);
    Options.Editing := not Converter.GetBooleanProperty(ADXColumn, 'DisableEditor', Options.Editing);
    Options.Grouping := not Converter.GetBooleanProperty(ADXColumn, 'DisableGrouping', Options.Grouping);
    Options.Focusing := True;
    Options.Filtering := cxGridView.DataController.Filter.Active;
    Options.HorzSizing := Converter.GetBooleanProperty(ADXColumn, 'Sizing', Options.HorzSizing);
    HeaderAlignmentHorz := ConvertAlignment(Converter.GetEnumProperty(ADXColumn, 'HeaderAlignment'));
    MinWidth := Converter.GetIntegerProperty(ADXColumn, 'MinWidth', MinWidth);
    Tag := Converter.GetIntegerProperty(ADXColumn, 'Tag', Tag);
    AssignSummaryItem(AColumn, ADXColumn);
    ImportColumn(ADXColumn, AColumn);
  end;
end;

procedure TcxRealConverterToTableView.AssignDXOptionsView(AList: TStringList);
var
  I: Integer;
begin
  with cxGridView do
  begin
    OptionsView.Indicator := AList.Find('edgoIndicator', I);
    OptionsView.CellAutoHeight := AList.Find('edgoRowAutoHeight', I);
    OptionsView.HeaderAutoHeight := AList.Find('edgoAutoHeaderPanelHeight', I);
    OptionsView.ColumnAutoWidth := AList.Find('edgoAutoWidth', I);
    OptionsSelection.HideFocusRect := AList.Find('edgoHideFocusRect', I);
    OptionsSelection.CellSelect := not AList.Find('edgoRowSelect', I);
    OptionsSelection.InvertSelect := AList.Find('edgoInvertSelect', I);
    OptionsView.GroupFooters := gfVisibleWhenExpanded;
    Preview.Visible := AList.Find('edgoPreview', I);
    if AList.Find('edgoShowButtonAlways', I) then
      OptionsView.ShowEditButtons := gsebForFocusedRecord
    else
      OptionsView.ShowEditButtons := gsebNever;
  end;
end;

procedure TcxRealConverterToTableView.AssignGrid;
var
  ALookAndFeel: string;
begin
  with cxGridView do
  begin
    if Converter.GetBooleanProperty(nil, 'ShowGrid', False) then
      OptionsView.GridLines := glBoth
    else
      OptionsView.GridLines := glNone;
    OptionsView.Header := Converter.GetBooleanProperty(nil, 'ShowHeader', OptionsView.Header);
    OptionsView.NewItemRow := Converter.GetBooleanProperty(nil, 'ShowNewItemRow', OptionsView.NewItemRow);
    OptionsView.Footer := Converter.GetBooleanProperty(nil, 'ShowSummaryFooter', OptionsView.Footer);
    OptionsView.IndicatorWidth := Converter.GetIntegerProperty(nil, 'GrIndicatorWidth', OptionsView.IndicatorWidth);
    OptionsSelection.HideSelection := Converter.GetBooleanProperty(nil, 'HideSelection', OptionsSelection.HideSelection);
    Preview.LeftIndent := Converter.GetIntegerProperty(nil, 'IndentDesc', Preview.LeftIndent);
    ALookAndFeel := Converter.GetEnumProperty(nil, 'LookAndFeel');
    if ALookAndFeel = 'lfFlat' then
      cxGrid.LookAndFeel.Kind := lfFlat
    else if ALookAndFeel = 'lfStandard' then
      cxGrid.LookAndFeel.Kind := lfStandard
    else if ALookAndFeel = 'lfUltraFlat' then
      cxGrid.LookAndFeel.Kind := lfUltraFlat;
    OptionsView.GroupByBox := Converter.GetBooleanProperty(nil, 'ShowGroupPanel', OptionsView.GroupByBox);
  end;
end;

procedure TcxRealConverterToTableView.AssignSummaryItem(AColumn: TcxGridColumn; ADXColumn: TObject);
var
  ASummaryFooterType: string;
  AcxSummaryFooterItem: TcxGridDBTableSummaryItem;
begin
  ASummaryFooterType := Converter.GetEnumProperty(ADXColumn, 'SummaryFooterType', 'cstNone');
  if ASummaryFooterType <> 'cstNone' then
  begin
    AcxSummaryFooterItem := TcxGridDBTableSummaryItem(
      TcxGridDBTableView(cxGridView).DataController.Summary.FooterSummaryItems.Add);
    AcxSummaryFooterItem.FieldName := Converter.GetStringProperty(ADXColumn, 'SummaryFooterField', '');
    AcxSummaryFooterItem.Format := Converter.GetStringProperty(ADXColumn, 'SummaryFooterFormat', AcxSummaryFooterItem.Format);
    AcxSummaryFooterItem.Kind := GetSummaryKind(ASummaryFooterType);
    AcxSummaryFooterItem.Column := TcxGridDBColumn(AColumn);
  end;
end;

function TcxRealConverterToTableView.CreateColumn: TcxGridColumn;
begin
  Result := TcxGridDBTableView(cxGridView).CreateColumn;
end;

procedure TcxRealConverterToTableView.ImportAutoDataSetFilter(const AValue: Boolean);
begin
  TcxGridDBTableView(cxGridView).DataController.Filter.AutoDataSetFilter := AValue;
end;

procedure TcxRealConverterToTableView.ImportColumn(AdxColumn: TObject; AcxColumn: TcxGridColumn);
begin
  if AdxColumn.ClassName = 'TdxDBGridColumn' then
    ImportPropertiesTextEdit(AdxColumn, AcxColumn)
  else if AdxColumn.ClassName = 'TdxDBGridMaskColumn' then
    ImportPropertiesMaskEdit(AdxColumn, AcxColumn)
  else if AdxColumn.ClassName = 'TdxDBGridButtonColumn' then
    ImportPropertiesButtonEdit(AdxColumn, AcxColumn)
  else if AdxColumn.ClassName = 'TdxDBGridDateColumn' then
    ImportPropertiesDateEdit(AdxColumn, AcxColumn)
  else if AdxColumn.ClassName = 'TdxDBGridCheckColumn' then
    ImportPropertiesCheckBox(AdxColumn, AcxColumn)
  else if AdxColumn.ClassName = 'TdxDBGridImageColumn' then
    ImportPropertiesImageComboBox(AdxColumn, AcxColumn)
  else if AdxColumn.ClassName = 'TdxDBGridSpinColumn' then
    ImportPropertiesSpinEdit(AdxColumn, AcxColumn)
  else if AdxColumn.ClassName = 'TdxDBGridLookupColumn' then
    ImportPropertiesLookupComboBox(AdxColumn, AcxColumn)
  else if AdxColumn.ClassName = 'TdxDBGridPickColumn' then
    ImportPropertiesComboBox(AdxColumn, AcxColumn)
  else if AdxColumn.ClassName = 'TdxDBGridCalcColumn' then
    ImportPropertiesCalcEdit(AdxColumn, AcxColumn)
  else if AdxColumn.ClassName = 'TdxDBGridBlobColumn' then
    ImportPropertiesBlobEdit(AdxColumn, AcxColumn)
  else if AdxColumn.ClassName = 'TdxDBGridHyperLinkColumn' then
    ImportPropertiesHyperLinkEdit(AdxColumn, AcxColumn)
  else if AdxColumn.ClassName = 'TdxDBGridTimeColumn' then
    ImportPropertiesTimeEdit(AdxColumn, AcxColumn)
  else if AdxColumn.ClassName = 'TdxDBGridCurrencyColumn' then
    ImportPropertiesCurrencyEdit(AdxColumn, AcxColumn)
  else if AdxColumn.ClassName = 'TdxDBGridMemoColumn' then
    ImportPropertiesMemo(AdxColumn, AcxColumn)
  else if AdxColumn.ClassName = 'TdxDBGridGraphicColumn' then
    ImportPropertiesImage(AdxColumn, AcxColumn)
  else if AdxColumn.ClassName = 'TdxDBGridMRUColumn' then
    ImportPropertiesMRUEdit(AdxColumn, AcxColumn)
  else if AdxColumn.ClassName = 'TdxDBGridPopupColumn' then
    ImportPropertiesPopupEdit(AdxColumn, AcxColumn)
  else if AdxColumn.ClassName = 'TdxDBGridExtLookupColumn' then
    ImportPropertiesExLookupComboBox(AdxColumn, AcxColumn);
end;

procedure TcxRealConverterToTableView.ImportColumns;
var
  AColumns: TList;
  I: Integer;
begin
  AColumns := TList.Create;
  try
    GetColumns(AColumns);
    for I := 0 to AColumns.Count - 1 do
      AssignColumn(CreateColumn, AColumns[I], I);
  finally
    AColumns.Free;
  end;
end;

procedure TcxRealConverterToTableView.ImportColumnsStyles;
var
  AColumns: TList;
  AColumn: TcxGridDBColumn;
  I: Integer;
  AColor: Integer;
  AFont: TFont;
  AStyle: TcxCustomStyle;
begin
  AColumns := TList.Create;
  try
    GetColumns(AColumns);
    for I := 0 to AColumns.Count - 1 do
    begin
      AColumn := GetCXColumnByFieldName(Converter.GetStringProperty(AColumns[I], 'FieldName', ''));
      AColor := Converter.GetIntegerProperty(TComponent(AColumns[I]), 'Color');
      AFont := Converter.GetClassProperty(TComponent(AColumns[I]), 'Font') as TFont;
      if ((AColor <> clWindow) and (AColor <> FColor)) or
        (not DefaultFont(AFont) and not CompareFonts(AFont, FFont)) then
      begin
        AStyle := Converter.CreateStyleItem;
        (AStyle as TcxStyle).Color := AColor;
        (AStyle as TcxStyle).Font.Assign(AFont);
        (AStyle as TcxStyle).TextColor := (AStyle as TcxStyle).Font.Color;
        AColumn.Styles.Content := AStyle as TcxStyle;
      end;
    end;
  finally
    AColumns.Free;
  end;
end;

procedure TcxRealConverterToTableView.ImportColumnSummaryGroups;
var
  AColumns: TList;
  AColumn: TcxGridDBColumn;
  ASummaryField: string;
  ASummaryFormat: string;
  ASummaryGroupName: string;
  ASummaryType: string;
  ASummaryGroup: TcxDataSummaryGroup;
  ASummaryItem: TcxGridDBTableSummaryItem;
  ALink: TcxGridTableSummaryGroupItemLink;
  AIndex: Integer;
  I: Integer;
begin
  AColumns := TList.Create;
  try
    GetColumns(AColumns);
    for I := 0 to AColumns.Count - 1 do
    begin
      AColumn := GetCXColumnByFieldName(Converter.GetStringProperty(AColumns[I], 'FieldName', ''));
      if AColumn = nil then
        Continue;
      ASummaryField := Converter.GetStringProperty(TComponent(AColumns[I]), 'SummaryField', '');
      ASummaryFormat := Converter.GetStringProperty(TComponent(AColumns[I]), 'SummaryFormat', '');
      ASummaryType := Converter.GetEnumProperty(TComponent(AColumns[I]), 'SummaryType');
      ASummaryGroupName := Converter.GetStringProperty(TComponent(AColumns[I]), 'SummaryGroupName', '');
      if ASummaryGroupName <> '' then
      begin
        if FcxSummaryGroups.Find(ASummaryGroupName, AIndex) then
        begin
          ASummaryGroup := TcxDataSummaryGroup(FcxSummaryGroups.Objects[AIndex]);
          ALink := TcxGridTableSummaryGroupItemLink(ASummaryGroup.Links.Add);
          ALink.Column := AColumn;
        end;
      end;
      if (ASummaryField <> '') and (ASummaryType <> 'cstNone') then
      begin
        ASummaryGroup := cxGridView.DataController.Summary.SummaryGroups.Add;
        ASummaryItem := TcxGridDBTableSummaryItem(ASummaryGroup.SummaryItems.Add);
        ASummaryItem.FieldName := ASummaryField;
        ASummaryItem.Format := ASummaryFormat;
        ASummaryItem.Kind := GetSummaryKind(ASummaryType);
        ALink := TcxGridTableSummaryGroupItemLink(ASummaryGroup.Links.Add);
        ALink.Column := AColumn;
      end;
    end;
  finally
    AColumns.Free;
  end;
end;

procedure TcxRealConverterToTableView.ImportDXFilter;
var
  AFilter: TObject;
  AStatus: string;
  AFilterOptions: TcxFilterCriteriaOptions;
begin
  AFilter := Converter.GetClassProperty(nil, 'Filter');
  if AFilter <> nil then
  begin
    cxGridView.Filtering.DropDownWidth :=
      Converter.GetIntegerProperty(AFilter, 'DropDownWidth', cxGridView.Filtering.DropDownWidth);
    cxGridView.DataController.Filter.MaxValueListCount :=
      Converter.GetIntegerProperty(AFilter, 'MaxDropDownCount', cxGridView.DataController.Filter.MaxValueListCount);
    cxGridView.Filtering.MaxDropDownCount := Converter.GetIntegerProperty(AFilter, 'DropDownCount',
      cxGridView.Filtering.MaxDropDownCount);
    AStatus := Converter.GetEnumProperty(AFilter, 'FilterStatus');
    if AStatus = 'fsAlways' then
      cxGridView.Filtering.Visible := fvAlways
    else if AStatus = 'fsAuto' then
      cxGridView.Filtering.Visible := fvNonEmpty
    else if AStatus = 'fsNone' then
      cxGridView.Filtering.Visible := fvNever;
    cxGridView.DataController.Filter.Active := Converter.GetBooleanProperty(AFilter, 'Active', cxGridView.DataController.Filter.Active);
    ImportAutoDatasetFilter(Converter.GetBooleanProperty(AFilter, 'AutoDataSetFilter', False));
    AFilterOptions := cxGridView.DataController.Filter.Options;
    if Converter.GetBooleanProperty(AFilter, 'CaseInsensitive', False) then
      Include(AFilterOptions, fcoCaseInsensitive);
    cxGridView.DataController.Filter.Options := AFilterOptions;
  end;
end;

procedure TcxRealConverterToTableView.ImportDXOptionsBehavior;
var
  AList: TStringList;
  I: Integer;
  AOptions: TcxDataControllerOptions;
begin
  AList := TStringList.Create;
  try
    Converter.EnablePropertyException;
    try
      Converter.GetSetProperty(nil, 'OptionsBehavior', AList);
      AList.Sort;
      with cxGridView do
      begin
        OptionsData.Editing := AList.Find('edgoEditing', I);
        OptionsBehavior.ImmediateEditor := AList.Find('edgoImmediateEditor', I);
        OptionsSelection.MultiSelect := AList.Find('edgoMultiSelect', I);
        OptionsBehavior.GoToNextCellOnEnter := AList.Find('edgoEnterThrough', I);
        OptionsBehavior.FocusCellOnTab := AList.Find('edgoTabs', I);
        OptionsBehavior.FocusCellOnCycle := not Alist.Find('edgoTabThrough', I);
        AOptions := DataController.Options;
        if AList.Find('edgoAnsiSort', I) then
          Include(AOptions, dcoAnsiSort);
        if AList.Find('edgoCaseInsensitive', I) then
          Include(AOptions, dcoCaseInsensitive);
        DataController.Options := AOptions;
        OptionsBehavior.IncSearch := AList.Find('edgoAutoSearch', I);
      end;
    except
      on EcxUnknownProperty do;
    end;
  finally
    AList.Free;
    Converter.DisablePropertyException;
  end;
end;

procedure TcxRealConverterToTableView.ImportDXOptionsCustomize;
var
  AList: TStringList;
  I: Integer;
begin
  AList := TStringList.Create;
  try
    Converter.EnablePropertyException;
    try
      Converter.GetSetProperty(nil, 'OptionsCustomize', AList);
      AList.Sort;
      with cxGridView do
      begin
        OptionsCustomize.ColumnMoving := AList.Find('edgoColumnMoving', I);
        OptionsCustomize.ColumnHorzSizing := AList.Find('edgoColumnSizing', I);
        OptionsCustomize.ColumnHiding := AList.Find('edgoExtCustomizing', I);
      end;
    except
      on EcxUnknownProperty do;
    end;
  finally
    AList.Free;
    Converter.DisablePropertyException;
  end;
end;

procedure TcxRealConverterToTableView.ImportDXOptionsDB;
var
  AList: TStringList;
  I: Integer;
begin
  AList := TStringList.Create;
  try
    Converter.EnablePropertyException;
    try
      Converter.GetSetProperty(nil, 'OptionsDB', AList);
      AList.Sort;
      with cxGridView do
      begin
        OptionsData.Appending := AList.Find('edgoCanAppend', I);
        OptionsData.CancelOnExit := AList.Find('edgoCancelOnExit', I);
        OptionsData.Deleting := AList.Find('edgoCanDelete', I);
        OptionsData.Inserting := AList.Find('edgoCanInsert', I);
        OptionsData.DeletingConfirmation := AList.Find('edgoConfirmDelete', I);
        OptionsBehavior.FocusFirstCellOnNewRecord := AList.Find('edgoResetColumnFocus', I);
        ImportSyncMode(AList.Find('edgoCanNavigation', I));
      end;
    except
      on EcxUnknownProperty do;
    end;
  finally
    AList.Free;
    Converter.DisablePropertyException;
  end;
end;

procedure TcxRealConverterToTableView.ImportDXOptionsView;
var
  AList: TStringList;
begin
  AList := TStringList.Create;
  try
    Converter.EnablePropertyException;
    try
      Converter.GetSetProperty(nil, 'OptionsView', AList);
      AList.Sort;
      AssignDXOptionsView(AList);
    except
      on EcxUnknownProperty do;
    end;
  finally
    AList.Free;
    Converter.DisablePropertyException;
  end;
end;

procedure TcxRealConverterToTableView.ImportGrid;
begin
  AssignGrid;
  ImportPreview;
  ImportDXOptionsDB;
  ImportDXOptionsCustomize;
  ImportDXOptionsBehavior;
  ImportSummaryGroups;
  ImportColumnSummaryGroups;
end;

procedure TcxRealConverterToTableView.ImportGridStyles;
var
  AStyle: TcxCustomStyle;
  AGroupNodeColor: Integer;
  AGroupNodeTextColor: Integer;
  AGroupPanelColor: Integer;
  AGroupPanelFontColor: Integer;
  AAutoSearchColor: Integer;
  AAutoSearchTextColor: Integer;
  APreviewFont: TFont;
  AHighlightColor: Integer;
  AHighlightTextColor: Integer;
  AHideSelectionColor: Integer;
  AHideSelectionTextColor: Integer;
  AGridLineColor: Integer;
begin
  FColor := Converter.GetIntegerProperty(nil, 'Color');
  FFont := Converter.GetClassProperty(nil, 'Font') as TFont;
  if (FColor <> clWindow) or not DefaultFont(FFont) then
  begin
    AStyle := Converter.CreateStyleItem;
    (AStyle as TcxStyle).Color := FColor;
    (AStyle as TcxStyle).Font.Assign(FFont);
    (AStyle as TcxStyle).TextColor := (AStyle as TcxStyle).Font.Color;
    cxGridView.Styles.Content := AStyle as TcxStyle;
  end;
  AGroupNodeColor := Converter.GetIntegerProperty(nil, 'GroupNodeColor');
  AGroupNodeTextColor := Converter.GetIntegerProperty(nil, 'GroupNodeTextColor');
  if (AGroupNodeColor <> clBtnFace) or (AGroupNodeTextColor <> clNone) then
  begin
    if (AGroupNodeColor = FColor) and ((AGroupNodeTextColor = FFont.Color) or
      (AGroupNodeTextColor = clNone)) then
      cxGridView.Styles.Group :=  cxGridView.Styles.Content
    else
    begin
      AStyle := Converter.CreateStyleItem;
      (AStyle as TcxStyle).Color := AGroupNodeColor;
      (AStyle as TcxStyle).Font.Assign(FFont);
      if AGroupNodeTextColor = clNone then
        (AStyle as TcxStyle).TextColor := (AStyle as TcxStyle).Font.Color
      else
        (AStyle as TcxStyle).TextColor := AGroupNodeTextColor;
      cxGridView.Styles.Group := AStyle as TcxStyle;
    end;
  end;
  AGroupPanelColor := Converter.GetIntegerProperty(nil, 'GroupPanelColor');
  AGroupPanelFontColor := Converter.GetIntegerProperty(nil, 'GroupPanelFontColor');
  if (AGroupPanelColor <> clBtnShadow) or (AGroupPanelFontColor <> clBtnFace) then
  begin
    AStyle := Converter.CreateStyleItem;
    (AStyle as TcxStyle).Color := AGroupPanelColor;
    (AStyle as TcxStyle).TextColor := AGroupPanelFontColor;
    cxGridView.Styles.GroupByBox := AStyle as TcxStyle;
  end;
  FHeaderColor := Converter.GetIntegerProperty(nil, 'HeaderColor');
  FHeaderFont := Converter.GetClassProperty(nil, 'HeaderFont') as TFont;
  if (FHeaderColor <> clBtnFace) or not DefaultFont(FHeaderFont) then
  begin
    AStyle := Converter.CreateStyleItem;
    (AStyle as TcxStyle).Color := FHeaderColor;
    (AStyle as TcxStyle).Font.Assign(FHeaderFont);
    (AStyle as TcxStyle).TextColor := (AStyle as TcxStyle).Font.Color;
    cxGridView.Styles.Header := AStyle as TcxStyle;
    cxGridView.Styles.Indicator := AStyle as TcxStyle;
  end;
  if (FHeaderColor <> clBtnface) or not DefaultFont(FFont) then
  begin
    AStyle := Converter.CreateStyleItem;
    (AStyle as TcxStyle).Color := FHeaderColor;
    (AStyle as TcxStyle).Font.Assign(FFont);
    (AStyle as TcxStyle).TextColor := (AStyle as TcxStyle).Font.Color;
    cxGridView.Styles.Footer := AStyle as TcxStyle;
  end;
  AAutoSearchColor := Converter.GetIntegerProperty(nil, 'AutoSearchColor');
  AAutoSearchTextColor := Converter.GetIntegerProperty(nil, 'AutoSearchTextColor');
  if (AAutoSearchColor <> clNone) or (AAutoSearchTextColor <> clNone) then
  begin
    AStyle := Converter.CreateStyleItem;
    (AStyle as TcxStyle).Color := AAutoSearchColor;
    (AStyle as TcxStyle).TextColor := AAutoSearchTextColor;
    cxGridView.Styles.IncSearch := AStyle as TcxStyle;
  end;
  APreviewFont := Converter.GetClassProperty(nil, 'PreviewFont') as TFont;
  if not DefaultPreviewFont(APreviewFont) then
  begin
    AStyle := Converter.CreateStyleItem;
    (AStyle as TcxStyle).Color := FColor;
    (AStyle as TcxStyle).Font.Assign(APreviewFont);
    (AStyle as TcxStyle).TextColor := (AStyle as TcxStyle).Font.Color;
    cxGridView.Styles.Preview := AStyle as TcxStyle;
  end;
  AGridLineColor := Converter.GetIntegerProperty(nil, 'GridLineColor');
  if AGridLineColor <> clNone then
    cxGridView.OptionsView.GridLineColor := Converter.GetIntegerProperty(nil, 'GridLineColor');
  AHighlightColor := Converter.GetIntegerProperty(nil, 'HighlightColor');
  AHighlightTextColor := Converter.GetIntegerProperty(nil, 'HighlightTextColor');
  if (AHighlightColor <> clHighlight) or (AHighlightTextColor <> clHighlightText) then
  begin
    AStyle := Converter.CreateStyleItem;
    (AStyle as TcxStyle).Color := AHighlightColor;
    (AStyle as TcxStyle).TextColor := AHighlightTextColor;
    cxGridView.Styles.Selection := AStyle as TcxStyle;
  end;
  AHideSelectionColor := Converter.GetIntegerProperty(nil, 'HideSelectionColor');
  AHideSelectionTextColor := Converter.GetIntegerProperty(nil, 'HideSelectionTextColor');
  if (AHideSelectionColor <> clHighlight) or (AHideSelectionTextColor <> clHighlightText) then
  begin
    AStyle := Converter.CreateStyleItem;
    (AStyle as TcxStyle).Color := AHideSelectionColor;
    (AStyle as TcxStyle).TextColor := AHideSelectionTextColor;
    cxGridView.Styles.Inactive := AStyle as TcxStyle;
  end;
end;

procedure TcxRealConverterToTableView.ImportPreview;
var
  APreview: string;
  I: Integer;
begin
  APreview := Converter.GetStringProperty(nil, 'PreviewFieldName', '');
  if APreview <> '' then
  begin
    for I := 0 to cxGridView.ColumnCount - 1 do
      if TestPreviewFieldName(I, APreview) then
      begin
        cxGridView.Preview.Column := cxGridView.Columns[I];
        Break;
      end;
  end;
  cxGridView.Preview.Place := ppBottom;
  cxGridView.Preview.AutoHeight := False;
  cxGridView.Preview.MaxLineCount := Converter.GetIntegerProperty(nil, 'PreviewLines',
    cxGridView.Preview.MaxLineCount);
end;

procedure TcxRealConverterToTableView.ImportPropertiesBlobEdit(
  AdxColumn: TObject; AcxColumn: TcxGridColumn);
var
  AConverter: TcxCustomPropertiesConverter;
begin
  AcxColumn.PropertiesClass := TcxBlobEditProperties;
  AConverter := TcxBlobEditPropertiesConverter.Create(AcxColumn.Properties);
  try
    AConverter.ImportFrom(AdxColumn);
    AcxColumn.Options.ShowEditButtons := ConvertColumnShowEditButtons(
      Converter.GetEnumProperty(AdxColumn, 'ShowButtonStyle'));
  finally
    AConverter.Free;
  end;
end;

procedure TcxRealConverterToTableView.ImportPropertiesButtonEdit(
  AdxColumn: TObject; AcxColumn: TcxGridColumn);
var
  AConverter: TcxCustomPropertiesConverter;
begin
  AcxColumn.PropertiesClass := TcxButtonEditProperties;
  AConverter := TcxButtonEditPropertiesConverter.Create(AcxColumn.Properties);
  try
    AConverter.ImportFrom(AdxColumn);
    AcxColumn.Options.ShowEditButtons := ConvertColumnShowEditButtons(
      Converter.GetEnumProperty(AdxColumn, 'ShowButtonStyle'));
  finally
    AConverter.Free;
  end;
end;

procedure TcxRealConverterToTableView.ImportPropertiesCalcEdit(
  AdxColumn: TObject; AcxColumn: TcxGridColumn);
var
  AConverter: TcxCustomPropertiesConverter;
begin
  AcxColumn.PropertiesClass := TcxCalcEditProperties;
  AConverter := TcxCalcEditPropertiesConverter.Create(AcxColumn.Properties);
  try
    AConverter.ImportFrom(AdxColumn);
    AcxColumn.Options.ShowEditButtons := ConvertColumnShowEditButtons(
      Converter.GetEnumProperty(AdxColumn, 'ShowButtonStyle'));
  finally
    AConverter.Free;
  end;
end;

procedure TcxRealConverterToTableView.ImportPropertiesCheckBox(
  AdxColumn: TObject; AcxColumn: TcxGridColumn);
var
  AConverter: TcxCustomPropertiesConverter;
begin
  AcxColumn.PropertiesClass := TcxCheckBoxProperties;
  AConverter := TcxCheckBoxPropertiesConverter.Create(AcxColumn.Properties);
  try
    AConverter.ImportFrom(AdxColumn);
  finally
    AConverter.Free;
  end;
end;

procedure TcxRealConverterToTableView.ImportPropertiesComboBox(
  AdxColumn: TObject; AcxColumn: TcxGridColumn);
var
  AConverter: TcxCustomPropertiesConverter;
begin
  AcxColumn.PropertiesClass := TcxComboBoxProperties;
  AConverter := TcxComboBoxPropertiesConverter.Create(AcxColumn.Properties);
  try
    AConverter.ImportFrom(AdxColumn);
    AcxColumn.Options.ShowEditButtons := ConvertColumnShowEditButtons(
      Converter.GetEnumProperty(AdxColumn, 'ShowButtonStyle'));
  finally
    AConverter.Free;
  end;
end;

procedure TcxRealConverterToTableView.ImportPropertiesCurrencyEdit(
  AdxColumn: TObject; AcxColumn: TcxGridColumn);
var
  AConverter: TcxCustomPropertiesConverter;
begin
  AcxColumn.PropertiesClass := TcxCurrencyEditProperties;
  AConverter := TcxCurrencyEditPropertiesConverter.Create(AcxColumn.Properties);
  try
    AConverter.ImportFrom(AdxColumn);
  finally
    AConverter.Free;
  end;
end;

procedure TcxRealConverterToTableView.ImportPropertiesDateEdit(
  AdxColumn: TObject; AcxColumn: TcxGridColumn);
var
  AConverter: TcxCustomPropertiesConverter;
begin
  AcxColumn.PropertiesClass := TcxDateEditProperties;
  AConverter := TcxDateEditPropertiesConverter.Create(AcxColumn.Properties);
  try
    AConverter.ImportFrom(AdxColumn);
    AcxColumn.Options.ShowEditButtons := ConvertColumnShowEditButtons(
      Converter.GetEnumProperty(AdxColumn, 'ShowButtonStyle'));
  finally
    AConverter.Free;
  end;
end;

procedure TcxRealConverterToTableView.ImportPropertiesExLookupComboBox(
  AdxColumn: TObject; AcxColumn: TcxGridColumn);
begin

end;

procedure TcxRealConverterToTableView.ImportPropertiesHyperLinkEdit(
  AdxColumn: TObject; AcxColumn: TcxGridColumn);
var
  AConverter: TcxCustomPropertiesConverter;
begin
  AcxColumn.PropertiesClass := TcxHyperLinkEditProperties;
  AConverter := TcxHyperLinkEditPropertiesConverter.Create(AcxColumn.Properties);
  try
    AConverter.ImportFrom(AdxColumn);
  finally
    AConverter.Free;
  end;
end;

procedure TcxRealConverterToTableView.ImportPropertiesImage(
  AdxColumn: TObject; AcxColumn: TcxGridColumn);
var
  AConverter: TcxCustomPropertiesConverter;
begin
  AcxColumn.PropertiesClass := TcxImageProperties;
  AConverter := TcxImagePropertiesConverter.Create(AcxColumn.Properties);
  try
    AConverter.ImportFrom(AdxColumn);
  finally
    AConverter.Free;
  end;
end;

procedure TcxRealConverterToTableView.ImportPropertiesImageComboBox(
  AdxColumn: TObject; AcxColumn: TcxGridColumn);
var
  AConverter: TcxCustomPropertiesConverter;
begin
  AcxColumn.PropertiesClass := TcxImageComboBoxProperties;
  AConverter := TcxImageComboBoxPropertiesConverter.Create(AcxColumn.Properties);
  try
    AConverter.ImportFrom(AdxColumn);
    AcxColumn.Options.ShowEditButtons := ConvertColumnShowEditButtons(
      Converter.GetEnumProperty(AdxColumn, 'ShowButtonStyle'));
  finally
    AConverter.Free;
  end;
end;

procedure TcxRealConverterToTableView.ImportPropertiesLookupComboBox(
  AdxColumn: TObject; AcxColumn: TcxGridColumn);
var
  AConverter: TcxCustomPropertiesConverter;
begin
  AcxColumn.PropertiesClass := TcxLookupComboBoxProperties;
  AConverter := TcxLookupComboBoxPropertiesConverter.Create(AcxColumn.Properties);
  try
    AConverter.ImportFrom(AdxColumn);
    AcxColumn.Options.ShowEditButtons := ConvertColumnShowEditButtons(
      Converter.GetEnumProperty(AdxColumn, 'ShowButtonStyle'));
  finally
    AConverter.Free;
  end;
end;

procedure TcxRealConverterToTableView.ImportPropertiesMaskEdit(
  AdxColumn: TObject; AcxColumn: TcxGridColumn);
var
  AConverter: TcxCustomPropertiesConverter;
begin
  AcxColumn.PropertiesClass := TcxMaskEditProperties;
  AConverter := TcxMaskEditPropertiesConverter.Create(AcxColumn.Properties);
  try
    AConverter.ImportFrom(AdxColumn);
  finally
    AConverter.Free;
  end;
end;

procedure TcxRealConverterToTableView.ImportPropertiesMemo(
  AdxColumn: TObject; AcxColumn: TcxGridColumn);
var
  AConverter: TcxCustomPropertiesConverter;
begin
  AcxColumn.PropertiesClass := TcxMemoProperties;
  AConverter := TcxMemoPropertiesConverter.Create(AcxColumn.Properties);
  try
    AConverter.ImportFrom(AdxColumn);
  finally
    AConverter.Free;
  end;
end;

procedure TcxRealConverterToTableView.ImportPropertiesMRUEdit(
  AdxColumn: TObject; AcxColumn: TcxGridColumn);
var
  AConverter: TcxCustomPropertiesConverter;
begin
  AcxColumn.PropertiesClass := TcxMRUEditProperties;
  AConverter := TcxMRUEditPropertiesConverter.Create(AcxColumn.Properties);
  try
    AConverter.ImportFrom(AdxColumn);
    AcxColumn.Options.ShowEditButtons := ConvertColumnShowEditButtons(
      Converter.GetEnumProperty(AdxColumn, 'ShowButtonStyle'));
  finally
    AConverter.Free;
  end;
end;

procedure TcxRealConverterToTableView.ImportPropertiesPopupEdit(
  AdxColumn: TObject; AcxColumn: TcxGridColumn);
var
  AConverter: TcxCustomPropertiesConverter;
begin
  AcxColumn.PropertiesClass := TcxPopupEditProperties;
  AConverter := TcxPopupEditPropertiesConverter.Create(AcxColumn.Properties);
  try
    AConverter.ImportFrom(AdxColumn);
  finally
    AConverter.Free;
  end;
end;

procedure TcxRealConverterToTableView.ImportPropertiesSpinEdit(
  AdxColumn: TObject; AcxColumn: TcxGridColumn);
var
  AConverter: TcxCustomPropertiesConverter;
begin
  AcxColumn.PropertiesClass := TcxSpinEditProperties;
  AConverter := TcxSpinEditPropertiesConverter.Create(AcxColumn.Properties);
  try
    AConverter.ImportFrom(AdxColumn);
    AcxColumn.Options.ShowEditButtons := ConvertColumnShowEditButtons(
      Converter.GetEnumProperty(AdxColumn, 'ShowButtonStyle'));
  finally
    AConverter.Free;
  end;
end;

procedure TcxRealConverterToTableView.ImportPropertiesTextEdit(
  AdxColumn: TObject; AcxColumn: TcxGridColumn);
var
  AConverter: TcxCustomPropertiesConverter;
begin
  AcxColumn.PropertiesClass := TcxTextEditProperties;
  AConverter := TcxTextEditPropertiesConverter.Create(AcxColumn.Properties);
  try
    AConverter.ImportFrom(AdxColumn);
  finally
    AConverter.Free;
  end;
end;

procedure TcxRealConverterToTableView.ImportPropertiesTimeEdit(
  AdxColumn: TObject; AcxColumn: TcxGridColumn);
var
  AConverter: TcxCustomPropertiesConverter;
begin
  AcxColumn.PropertiesClass := TcxTimeEditProperties;
  AConverter := TcxTimeEditPropertiesConverter.Create(AcxColumn.Properties);
  try
    AConverter.ImportFrom(AdxColumn);
  finally
    AConverter.Free;
  end;
end;

procedure TcxRealConverterToTableView.ImportSummaryGroups;
var
  ASummaryGroups: TObject;
  ASummaryItems: TObject;
  AcxSummaryGroup: TcxDataSummaryGroup;
  AcxSummaryItem: TcxGridDBTableSummaryItem;
  I, J, K: Integer;
  ASeparator: string;
  AFieldName: string;
  AIsDefaultGroup: Boolean;
  AdxColumn: TComponent;
  ABeginText, AEndText: string;
begin
  ASeparator := Converter.GetStringProperty(nil, 'SummarySeparator', '');
  ASummaryGroups := Converter.GetClassProperty(nil, 'SummaryGroups');
  if ASummaryGroups <> nil then
  begin
    if ASummaryGroups is TCollection then
    with TCollection(ASummaryGroups) do
    begin
      for I := 0 to Count - 1 do
      begin
        ABeginText := Converter.GetStringProperty(Items[I], 'BeginSummaryText', '');
        AEndText := Converter.GetStringProperty(Items[I], 'EndSummaryText', '');
        AIsDefaultGroup := Converter.GetBooleanProperty(Items[I], 'DefaultGroup', True);
        if AIsDefaultGroup then
        begin
          cxGridView.DataController.Summary.DefaultGroupSummaryItems.BeginText := ABeginText;
          cxGridView.DataController.Summary.DefaultGroupSummaryItems.EndText := AEndText;
          cxGridView.DataController.Summary.DefaultGroupSummaryItems.Separator := ASeparator;
          AcxSummaryGroup := nil;
        end
        else
        begin
          AcxSummaryGroup := cxGridView.DataController.Summary.SummaryGroups.Add;
          AcxSummaryGroup.SummaryItems.BeginText := ABeginText;
          AcxSummaryGroup.SummaryItems.EndText := AEndText;
          AcxSummaryGroup.SummaryItems.Separator := ASeparator;
        end;
        if AcxSummaryGroup <> nil then
          FcxSummaryGroups.AddObject(Converter.GetStringProperty(Items[I], 'Name', ''), AcxSummaryGroup);
        ASummaryItems := Converter.GetClassProperty(Items[I], 'SummaryItems');
        if ASummaryItems <> nil then
        begin
          if ASummaryItems is TCollection then
          with TCollection(ASummaryItems) do
          begin
            for J := 0 to Count - 1 do
            begin
              if AIsDefaultGroup then
                AcxSummaryItem := TcxGridDBTableSummaryItem(cxGridView.DataController.Summary.DefaultGroupSummaryItems.Add)
              else
                AcxSummaryItem := TcxGridDBTableSummaryItem(AcxSummaryGroup.SummaryItems.Add);
              AcxSummaryItem.Column := nil;
              AdxColumn := GetColumn(Converter.GetStringProperty(Items[J], 'ColumnName', ''));
              if AdxColumn <> nil then
              begin
                AFieldName := Converter.GetStringProperty(AdxColumn, 'FieldName', '');
                for K := 0 to cxGridView.ColumnCount - 1 do
                  if TcxGridDbTableView(cxGridView).Columns[K].DataBinding.FieldName = AFieldName then
                  begin
                    AcxSummaryItem.Column := TcxGridDBTableView(cxGridView).Columns[K];
                    AcxSummaryItem.Position := spFooter;
                    Break;
                  end;
              end;
              AcxSummaryItem.FieldName := Converter.GetStringProperty(Items[J], 'SummaryField', '');
              AcxSummaryItem.Format := Converter.GetStringProperty(Items[J], 'SummaryFormat', AcxSummaryItem.Format);
              AcxSummaryItem.Kind := GetSummaryKind(Converter.GetEnumProperty(Items[J], 'SummaryType'));
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TcxRealConverterToTableView.ImportSyncMode(const AValue: Boolean);
begin
  TcxGridDBTableView(cxGridView).DataController.DataModeController.SyncMode := AValue;
end;

function TcxRealConverterToTableView.TestPreviewFieldName(AIndex: Integer;
  const APreview: string): Boolean;
begin
  Result := TcxGridDBTableView(cxGridView).Columns[AIndex].DataBinding.FieldName = APreview;
end;

function TcxRealConverterToTableView.DefaultPreviewFont(AFont: TFont): Boolean;
begin
  if AFont = nil then
  begin
    Result := True;
    Exit;
  end;
  with AFont do
    Result :=
      (Pitch = DefFontData.Pitch) and
      (Style = DefFontData.Style) and
      (Charset = DefFontData.Charset) and
      (Name = dxShortStringToString(DefFontData.Name)) and
      (Color = clBlue) and
      (Size = 8);
end;

function TcxRealConverterToTableView.GetColumn(const AName: string): TComponent;
var
  AColumns: TList;
  I: Integer;
begin
  Result := nil;
  AColumns := TList.Create;
  try
    GetColumns(AColumns);
    for I := 0 to AColumns.Count - 1 do
      if Converter.GetStringProperty(TComponent(AColumns[I]), 'Name', '') = AName then
      begin
        Result := TComponent(AColumns[I]);
        Break;
      end;
  finally
    AColumns.Free;
  end;
end;

procedure TcxRealConverterToTableView.GetColumns(AColumns: TList);
var
  AForm: TComponent;
  I: Integer;
begin
  AForm := Converter.Source.Owner;
  for I := 0 to AForm.ComponentCount - 1 do
  begin
    if AForm.Components[I].GetParentComponent = Converter.Source then
      AColumns.Add(AForm.Components[I]);
  end;
  if AColumns.Count = 0 then
  begin
    for I := 0 to Converter.Source.ComponentCount - 1 do
    begin
      if Converter.Source.Components[I].GetParentComponent = Converter.Source then
        AColumns.Add(Converter.Source.Components[I]);
    end;
  end;
end;

function TcxRealConverterToTableView.GetCXColumnByFieldName(const AFieldName: string): TcxGridDBColumn;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to cxGridView.ColumnCount - 1 do
  begin
    if TcxGridDBTableView(cxGridView).Columns[I].DataBinding.FieldName = AFieldName then
    begin
      Result := TcxGridDBTableView(cxGridView).Columns[I];
      Break;
    end;
  end;
end;

function TcxRealConverterToTableView.GetcxGrid: TcxCustomGrid;
begin
  Result := Converter.Destination;
end;

function TcxRealConverterToTableView.GetcxGridView: TcxGridTableView;
begin
  Result := Converter.cxGridView as TcxGridTableView;
end;

function TcxRealConverterToTableView.ConvertColumnShowEditButtons(
  const AStyle: string): TcxGridItemShowEditButtons;
begin
  if AStyle = 'sbDefault' then
    Result := isebDefault
  else if AStyle = 'sbAlways' then
    Result := isebAlways
  else if AStyle = 'sbNone' then
    Result := isebNever
  else
    Result := isebDefault;
end;

function TcxRealConverterToTableView.GetSummaryKind(
  const AType: string): TcxSummaryKind;
begin
  if AType = 'cstNone' then
    Result := skNone
  else if AType = 'cstAvg' then
    Result := skAverage
  else if AType = 'cstCount' then
    Result := skCount
  else if AType = 'cstMax' then
    Result := skMax
  else if AType = 'cstMin' then
    Result := skMin
  else if AType = 'cstSum' then
    Result := skSum
  else
    Result := skNone;
end;

{ TcxRealConverterToBandedTableView }

procedure TcxRealConverterToBandedTableView.DoImport;
begin
  TcxGridDBBandedTableView(cxGridView).DataController.DataSource :=
    Converter.GetClassProperty(nil, 'DataSource') as TDataSource;
  ImportDXOptionsView;
  ImportDXFilter;
  ImportBands;
  ImportColumns;
  TcxGridDBBandedTableView(cxGridView).DataController.KeyFieldNames :=
    Converter.GetStringProperty(nil, 'KeyField', '');
  ImportGrid;
end;

procedure TcxRealConverterToBandedTableView.DoImportStyles;
begin
  ImportGridStyles;
  ImportBandsStyles;
  ImportColumnsStyles;
end;

procedure TcxRealConverterToBandedTableView.AssignColumn(AColumn: TcxGridColumn;
  ADXColumn: TObject; AIndex: Integer);
begin
  with TcxGridDBBandedColumn(AColumn) do
  begin
    Position.BandIndex := Converter.GetIntegerProperty(ADXColumn, 'BandIndex', Position.BandIndex);
    Position.ColIndex := Converter.GetIntegerProperty(ADXColumn, 'ColIndex', Position.ColIndex);
    Options.VertSizing := Converter.GetBooleanProperty(ADXColumn, 'Sizing', Options.VertSizing);
    Position.RowIndex := Converter.GetIntegerProperty(ADXColumn, 'RowIndex', Position.RowIndex);
  end;
  inherited AssignColumn(AColumn, ADXColumn, AIndex);
end;

procedure TcxRealConverterToBandedTableView.ImportAutoDataSetFilter(const AValue: Boolean);
begin
  TcxGridDBBandedTableView(cxGridView).DataController.Filter.AutoDataSetFilter := AValue;
end;

procedure TcxRealConverterToBandedTableView.ImportColumnSummaryGroups;
  function GetCXColumnByFieldName(const AFieldName: string): TcxGridDBBandedColumn;
  var
    I: Integer;
  begin
    Result := nil;
    for I := 0 to cxGridView.ColumnCount - 1 do
    begin
      if TcxGridDbBandedTableView(cxGridView).Columns[I].DataBinding.FieldName = AFieldName then
      begin
        Result := TcxGridDbBandedTableView(cxGridView).Columns[I];
        Break;
      end;
    end;
  end;
var
  AColumns: TList;
  AColumn: TcxGridDBBandedColumn;
  ASummaryField: string;
  ASummaryFormat: string;
  ASummaryGroupName: string;
  ASummaryType: string;
  ASummaryGroup: TcxDataSummaryGroup;
  ASummaryItem: TcxGridDBTableSummaryItem;
  ALink: TcxGridTableSummaryGroupItemLink;
  AIndex: Integer;
  I: Integer;
begin
  AColumns := TList.Create;
  try
    GetColumns(AColumns);
    for I := 0 to AColumns.Count - 1 do
    begin
      AColumn := GetCXColumnByFieldName(Converter.GetStringProperty(AColumns[I], 'FieldName', ''));
      if AColumn = nil then
        Continue;
      ASummaryField := Converter.GetStringProperty(TComponent(AColumns[I]), 'SummaryField', '');
      ASummaryFormat := Converter.GetStringProperty(TComponent(AColumns[I]), 'SummaryFormat', '');
      ASummaryType := Converter.GetEnumProperty(TComponent(AColumns[I]), 'SummaryType');
      ASummaryGroupName := Converter.GetStringProperty(TComponent(AColumns[I]), 'SummaryGroupName', '');
      if ASummaryGroupName <> '' then
      begin
        if FcxSummaryGroups.Find(ASummaryGroupName, AIndex) then
        begin
          ASummaryGroup := TcxDataSummaryGroup(FcxSummaryGroups.Objects[AIndex]);
          ALink := TcxGridTableSummaryGroupItemLink(ASummaryGroup.Links.Add);
          ALink.Column := AColumn;
        end;
      end;
      if (ASummaryField <> '') and (ASummaryType <> 'cstNone') then
      begin
        ASummaryGroup := cxGridView.DataController.Summary.SummaryGroups.Add;
        ASummaryItem := TcxGridDBTableSummaryItem(ASummaryGroup.SummaryItems.Add);
        ASummaryItem.FieldName := ASummaryField;
        ASummaryItem.Format := ASummaryFormat;
        ASummaryItem.Kind := GetSummaryKind(ASummaryType);
        ALink := TcxGridTableSummaryGroupItemLink(ASummaryGroup.Links.Add);
        ALink.Column := AColumn;
      end;
    end;
  finally
    AColumns.Free;
  end;
end;

procedure TcxRealConverterToBandedTableView.AssignDXOptionsView(AList: TStringList);
var
  I: Integer;
begin
  with cxGridView do
    FBandHeaderWidth := AList.Find('edgoBandHeaderWidth', I);
  inherited AssignDXOptionsView(AList);
end;

procedure TcxRealConverterToBandedTableView.AssignGrid;
begin
  with cxGridView do
  begin
    OptionsView.BandHeaders := Converter.GetBooleanProperty(nil, 'ShowBands', OptionsView.BandHeaders);
    OptionsView.FixedBandSeparatorWidth := Converter.GetIntegerProperty(nil, 'FixedBandLineWidth',
      OptionsView.FixedBandSeparatorWidth);
  end;
  inherited AssignGrid;
end;

procedure TcxRealConverterToBandedTableView.AssignSummaryItem(AColumn: TcxGridColumn; ADXColumn: TObject);
var
  ASummaryFooterType: string;
  AcxSummaryFooterItem: TcxGridDBTableSummaryItem;
begin
  ASummaryFooterType := Converter.GetEnumProperty(ADXColumn, 'SummaryFooterType');
  if ASummaryFooterType <> 'cstNone' then
  begin
    AcxSummaryFooterItem := TcxGridDBTableSummaryItem(
      TcxGridDBBandedTableView(cxGridView).DataController.Summary.FooterSummaryItems.Add);
    AcxSummaryFooterItem.FieldName := Converter.GetStringProperty(ADXColumn, 'SummaryFooterField', '');
    AcxSummaryFooterItem.Format := Converter.GetStringProperty(ADXColumn, 'SummaryFooterFormat',
      AcxSummaryFooterItem.Format);
    AcxSummaryFooterItem.Kind := GetSummaryKind(ASummaryFooterType);
    AcxSummaryFooterItem.Column := AColumn;
  end;
end;

function TcxRealConverterToBandedTableView.CreateColumn: TcxGridColumn;
begin
  Result := TcxGridDBBandedTableView(cxGridView).CreateColumn;
end;

procedure TcxRealConverterToBandedTableView.ImportColumnsStyles;
var
  AColumns: TList;
  AColumn: TcxGridDBBandedColumn;
  I: Integer;
  AColor: Integer;
  AFont: TFont;
  AStyle: TcxCustomStyle;
begin
  AColumns := TList.Create;
  try
    GetColumns(AColumns);
    for I := 0 to AColumns.Count - 1 do
    begin
      AColumn := GetCXColumnByFieldName(Converter.GetStringProperty(AColumns[I], 'FieldName', ''));
      AColor := Converter.GetIntegerProperty(TComponent(AColumns[I]), 'Color');
      AFont := Converter.GetClassProperty(TComponent(AColumns[I]), 'Font') as TFont;
      if ((AColor <> clWindow) and (AColor <> FColor)) or
        (not DefaultFont(AFont) and not CompareFonts(AFont, FFont)) then
      begin
        AStyle := Converter.CreateStyleItem;
        (AStyle as TcxStyle).Color := AColor;
        (AStyle as TcxStyle).Font.Assign(AFont);
        (AStyle as TcxStyle).TextColor := (AStyle as TcxStyle).Font.Color;
        AColumn.Styles.Content := AStyle as TcxStyle;
      end;
    end;
  finally
    AColumns.Free;
  end;
end;

procedure TcxRealConverterToBandedTableView.ImportBands;
var
  ABands: TObject;
  AcxGridBand: TcxGridBand;
  I: Integer;
  AFixed: string;
begin
  cxGridView.Bands.Clear;
  ABands := Converter.GetClassProperty(nil, 'Bands');
  if ABands <> nil then
  begin
    if ABands is TCollection then
    with TCollection(ABands) do
    begin
      for I := 0 to Count - 1 do
      begin
        AcxGridBand := cxGridView.Bands.Add;
        if FBandHeaderWidth then
          AcxGridBand.Width := 0
        else
          AcxGridBand.Width := Converter.GetIntegerProperty(Items[I], 'Width', AcxGridBand.Width);
        AcxGridBand.Caption := Converter.GetStringProperty(Items[I], 'Caption', AcxGridBand.Caption);
        AcxGridBand.Alignment := ConvertAlignment(Converter.GetEnumProperty(Items[I], 'Alignment'));
        AcxGridBand.Visible := Converter.GetBooleanProperty(Items[I], 'Visible', AcxGridBand.Visible);
        AcxGridBand.Options.Sizing := Converter.GetBooleanProperty(Items[I], 'Sizing', AcxGridBand.Options.Sizing);
        AcxGridBand.Options.Moving := not Converter.GetBooleanProperty(Items[I], 'DisableDragging', AcxGridBand.Options.Moving);
        AFixed := Converter.GetEnumProperty(Items[I], 'Fixed');
        if AFixed = 'bfLeft' then
          AcxGridBand.FixedKind := fkLeft
        else if AFixed = 'bfNone' then
          AcxGridBand.FixedKind := fkNone
        else if AFixed = 'bfRight' then
          AcxGridBand.FixedKind := fkRight;
      end;
    end;
  end;
  cxGridView.OptionsView.BandHeaders := Converter.GetBooleanProperty(nil, 'ShowBands', cxGridView.OptionsView.BandHeaders);
end;

procedure TcxRealConverterToBandedTableView.ImportBandsStyles;
var
  ABandColor: Integer;
  ABandFont: TFont;
  AStyle: TcxCustomStyle;
begin
  ABandColor := Converter.GetIntegerProperty(nil, 'BandColor');
  ABandFont := Converter.GetClassProperty(nil, 'BandFont') as TFont;
  if (ABandColor <> clBtnFace) or not DefaultFont(ABandFont) then
  begin
    AStyle := Converter.CreateStyleItem;
    (AStyle as TcxStyle).Color := ABandColor;
    (AStyle as TcxStyle).Font.Assign(ABandFont);
    (AStyle as TcxStyle).TextColor := (AStyle as TcxStyle).Font.Color;
    cxGridView.Styles.BandHeader := AStyle as TcxStyle;
  end;
end;

procedure TcxRealConverterToBandedTableView.ImportSummaryGroups;
var
  ASummaryGroups: TObject;
  ASummaryItems: TObject;
  AcxSummaryGroup: TcxDataSummaryGroup;
  AcxSummaryItem: TcxGridDBTableSummaryItem;
  I, J, K: Integer;
  ASeparator: string;
  AFieldName: string;
  AIsDefaultGroup: Boolean;
  AdxColumn: TComponent;
  ABeginText, AEndText: string;
begin
  ASeparator := Converter.GetStringProperty(nil, 'SummarySeparator', '');
  ASummaryGroups := Converter.GetClassProperty(nil, 'SummaryGroups');
  if ASummaryGroups <> nil then
  begin
    if ASummaryGroups is TCollection then
    with TCollection(ASummaryGroups) do
    begin
      for I := 0 to Count - 1 do
      begin
        ABeginText := Converter.GetStringProperty(Items[I], 'BeginSummaryText', '');
        AEndText := Converter.GetStringProperty(Items[I], 'EndSummaryText', '');
        AIsDefaultGroup := Converter.GetBooleanProperty(Items[I], 'DefaultGroup', false);
        if AIsDefaultGroup then
        begin
          cxGridView.DataController.Summary.DefaultGroupSummaryItems.BeginText := ABeginText;
          cxGridView.DataController.Summary.DefaultGroupSummaryItems.EndText := AEndText;
          cxGridView.DataController.Summary.DefaultGroupSummaryItems.Separator := ASeparator;
          AcxSummaryGroup := nil;
        end
        else
        begin
          AcxSummaryGroup := cxGridView.DataController.Summary.SummaryGroups.Add;
          AcxSummaryGroup.SummaryItems.BeginText := ABeginText;
          AcxSummaryGroup.SummaryItems.EndText := AEndText;
          AcxSummaryGroup.SummaryItems.Separator := ASeparator;
        end;
        if AcxSummaryGroup <> nil then
          FcxSummaryGroups.AddObject(Converter.GetStringProperty(Items[I], 'Name', ''), AcxSummaryGroup);
        ASummaryItems := Converter.GetClassProperty(Items[I], 'SummaryItems');
        if ASummaryItems <> nil then
        begin
          if ASummaryItems is TCollection then
          with TCollection(ASummaryItems) do
          begin
            for J := 0 to Count - 1 do
            begin
              if AIsDefaultGroup then
                AcxSummaryItem := TcxGridDBTableSummaryItem(cxGridView.DataController.Summary.DefaultGroupSummaryItems.Add)
              else
                AcxSummaryItem := TcxGridDBTableSummaryItem(AcxSummaryGroup.SummaryItems.Add);
              AcxSummaryItem.Column := nil;
              AdxColumn := GetColumn(Converter.GetStringProperty(Items[J], 'ColumnName', ''));
              if AdxColumn <> nil then
              begin
                AFieldName := Converter.GetStringProperty(AdxColumn, 'FieldName', '');
                for K := 0 to cxGridView.ColumnCount - 1 do
                  if TcxGridDBBandedTableView(cxGridView).Columns[K].DataBinding.FieldName = AFieldName then
                  begin
                    AcxSummaryItem.Column := TcxGridDBBandedTableView(cxGridView).Columns[K];
                    AcxSummaryItem.Position := spFooter;
                    Break;
                  end;
              end;
              AcxSummaryItem.FieldName := Converter.GetStringProperty(Items[J], 'SummaryField', '');
              AcxSummaryItem.Format := Converter.GetStringProperty(Items[J], 'SummaryFormat', AcxSummaryItem.Format);
              AcxSummaryItem.Kind := GetSummaryKind(Converter.GetEnumProperty(Items[J], 'SummaryType'));
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TcxRealConverterToBandedTableView.ImportSyncMode(const AValue: Boolean);
begin
  TcxGridDBBandedTableView(cxGridView).DataController.DataModeController.SyncMode := AValue;
end;

function TcxRealConverterToBandedTableView.TestPreviewFieldName(
  AIndex: Integer; const APreview: string): Boolean;
begin
  Result := TcxGridDBBandedTableView(cxGridView).Columns[AIndex].DataBinding.FieldName = APreview;
end;

function TcxRealConverterToBandedTableView.GetCXColumnByFieldName(
  const AFieldName: string): TcxGridDBBandedColumn;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to cxGridView.ColumnCount - 1 do
  begin
    if TcxGridDBBandedTableView(cxGridView).Columns[I].DataBinding.FieldName = AFieldName then
    begin
      Result := TcxGridDBBandedTableView(cxGridView).Columns[I];
      Break;
    end;
  end;
end;

function TcxRealConverterToBandedTableView.GetcxGridView: TcxGridBandedTableView;
begin
  Result := Converter.cxGridView as TcxGridBandedTableView;
end;

procedure TcxRealConverterToBandedTableView.ImportDXOptionsCustomize;
var
  AList: TStringList;
  I: Integer;
begin
  AList := TStringList.Create;
  try
    Converter.EnablePropertyException;
    try
      Converter.GetSetProperty(nil, 'OptionsCustomize', AList);
      AList.Sort;
      with cxGridView do
      begin
        OptionsCustomize.BandMoving := AList.Find('edgoBandMoving', I);
        OptionsCustomize.BandSizing := AList.Find('edgoBandSizing', I);
        OptionsCustomize.ColumnVertSizing := AList.Find('edgoColumnSizing', I);
        OptionsCustomize.BandHiding := AList.Find('edgoExtCustomizing', I);
      end;
    except
      on EcxUnknownProperty do;
    end;
  finally
    AList.Free;
    Converter.DisablePropertyException;
  end;
  inherited ImportDXOptionsCustomize;
end;

{ TcxDXGridConverter }

class function TcxDXGridConverter.GetSourceClassName: string;
begin
  Result := 'TdxDBGrid';
end;

procedure TcxDXGridConverter.DoRealImport;
var
  ARealConverter: TcxRealConverterToTableView;
begin
  if GetGridViewClass = TcxGridDBTableView then
    ARealConverter := TcxRealConverterToTableView.Create(Self)
  else
    ARealConverter := TcxRealConverterToBandedTableView.Create(Self);
  try
    ARealConverter.DoImport;
  finally
    ARealConverter.Free;
  end;
end;

procedure TcxDXGridConverter.DoImportStyles;
var
  ARealConverter: TcxRealConverterToTableView;
begin
  if GetGridViewClass = TcxGridDBTableView then
    ARealConverter := TcxRealConverterToTableView.Create(Self)
  else
    ARealConverter := TcxRealConverterToBandedTableView.Create(Self);
  try
    ARealConverter.DoImportStyles;
  finally
    ARealConverter.Free;
  end;
end;

function TcxDXGridConverter.GetGridViewClass: TcxCustomGridViewClass;
var
  ADXBands: TCollection;
begin
  Result := TcxGridDBBandedTableView;
  EnablePropertyException;
  try
    if not GetBooleanProperty(nil, 'ShowBands', False) then
    begin
      ADXBands := GetClassProperty(nil, 'Bands') as TCollection;
      if ADXBands <> nil then
        if ADXBands.Count = 1 then
        begin
          if GetStringProperty(ADXBands.Items[0], 'Caption', '') = '' then
            Result := TcxGridDBTableView;
        end;
    end;
  except
    on EcxUnknownProperty do
      Result := TcxGridDBTableView;
  end;
  DisablePropertyException;
end;

{ TcxDXGridLayoutConverter }

procedure TcxDXGridLayoutConverter.DoRealImport;
begin
  if FdxDBGrid <> nil then
    inherited DoRealImport;
end;

function TcxDXGridLayoutConverter.GetGridViewClass: TcxCustomGridViewClass;
begin
  if FdxDBGrid <> nil then
    Result := inherited GetGridViewClass
  else
    Result := TcxGridDBTableView;
end;

class function TcxDXGridLayoutConverter.GetSourceClassName: string;
begin
  Result := 'TdxDBGridLayout';
end;

procedure TcxDXGridLayoutConverter.PostImport;
begin
  FdxDBGrid.Free;
  FdxDBGrid := nil;
  if FComponent <> nil then
    Source := FComponent;
end;

procedure TcxDXGridLayoutConverter.PreImport;
var
  AReader: TcxDXGridLayoutDataReader;
  AWriter: TWriter;
  AStream: TStringStream;
  ADummy: TcxDXGridLayoutDummy;
begin
  FdxDBGrid := nil;
  FComponent := nil;
  if Intermediary = nil then
    Exit;

  ADummy := TcxDXGridLayoutDummy.Create(nil);
  ADummy.Component := Source;
  ADummy.Intermediary := Intermediary;
  AStream := TStringStream.Create('');
  try
    AWriter := TWriter.Create(AStream, 65536);
    try
      AWriter.Root := Source.Owner;
      AWriter.WriteComponent(Source);
    finally
      AWriter.Free;
    end;
    AReader := TcxDXGridLayoutDataReader.Create(AStream, 65536);
    AStream.Position := 0;
    try
      AReader.BeginReferences;
      AReader.ReadComponent(ADummy);
      FdxDBGrid := ADummy.dxDBGrid;
      FComponent := Source;
      Source := FdxDBGrid;
    finally
      AReader.EndReferences;
      AReader.Free;
    end;
  finally
    AStream.Free;
    ADummy.Free;
  end;
end;

function TcxDXGridLayoutConverter.TestIntermediary: Boolean;
begin
  Result := Intermediary <> nil;
end;

class function TcxDXGridLayoutConverter.GetIntermediaryClassName: string;
begin
  Result := 'TdxDBGrid';
end;

{ TcxDXGridLayoutDataReader }

procedure TcxDXGridLayoutDataReader.SetName(Component: TComponent; var Name: string);
begin
  Name := '';
end;

{ TcxDXGridLayoutDummy }

constructor TcxDXGridLayoutDummy.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FComponent := nil;
  FdxDBGrid := nil;
  FIntermediary := nil;
end;

procedure TcxDXGridLayoutDummy.DefineProperties(Filer: TFiler);
begin
  Filer.DefineBinaryProperty('Data', ReadData, nil, False);
end;

procedure TcxDXGridLayoutDummy.ReadData(Stream: TStream);
var
  AClass: TFormClass;
  ASize: Integer;
begin
  if FIntermediary = nil then
    Exit;
  try
    AClass := TFormClass(FIntermediary.ClassType);
    Stream.ReadBuffer(ASize, SizeOf(ASize));
    FdxDBGrid := AClass.Create(Component.Owner);
    FdxDBGrid := Stream.ReadComponent(FdxDBGrid);
    (FdxDBGrid as TControl).Parent := Component.Owner as TWinControl;
  except
    FdxDBGrid.Free;
    FdxDBGrid := nil;
    raise;
  end;
end;

initialization
  ConverterFactory(cxGridGroupConverterName).RegisterConverter('DX Grid Converter', TcxDXGridConverter);
  ConverterFactory(cxGridGroupConverterName).RegisterConverter('DX Grid Layout Converter', TcxDXGridLayoutConverter);

end.
