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

unit cxDBPivotGrid;

{$I cxVer.inc}

interface

uses
  Classes, SysUtils, DB, cxClasses, cxControls, cxGraphics, cxCustomData, Variants, FMTBcd, SqlTimSt,
  cxDataStorage, cxDBData, cxDataUtils, cxCustomPivotGrid, cxEdit;

type
  TcxCustomDBPivotGrid = class;
  TcxDBPivotGridField = class;
  TcxDBPivotGridDataController = class;

  { TcxDBPivotGridFieldDataBinding }

  TcxDBPivotGridFieldDataBinding = class(TcxPivotGridFieldDataBinding)
  private
    function GetDataController: TcxDBPivotGridDataController;
    function GetDBField: TField;
    function GetField: TcxDBPivotGridField;
    function GetFieldCaption: string;
    function GetFieldName: string;
    procedure SetFieldName(AValue: string);
  protected
    function GetDefaultValuesProviderClass: TcxCustomEditDefaultValuesProviderClass; override;
    function GetFilterFieldName: string; override;
    procedure Init; override;

    property DataController: TcxDBPivotGridDataController read GetDataController;
  public
    procedure Assign(Source: TPersistent); override;

    property DBField: TField read GetDBField;
    property Field: TcxDBPivotGridField read GetField;
    property FieldCaption: string read GetFieldCaption;
  published
    property ValueType;
    property FieldName: string read GetFieldName write SetFieldName;
  end;

  { TcxDBPivotGridField }

  TcxDBPivotGridField = class(TcxPivotGridField)
  private
    function GetDataBinding: TcxDBPivotGridFieldDataBinding;
    function GetPivotGrid: TcxCustomDBPivotGrid;
    procedure SetDataBinding(AValue: TcxDBPivotGridFieldDataBinding);
    procedure SetPivotGrid(AValue: TcxCustomDBPivotGrid);
  protected
    function CreateDataBinding: TcxPivotGridFieldDataBinding; override;
    function DefaultRepositoryItem: TcxEditRepositoryItem; override;
    function GetActualDisplayFormat: string; override;
    function GetCaption: string; override;
    function IsCurrency(AType: TcxValueTypeClass): Boolean; override;
    //
  public
    property PivotGrid: TcxCustomDBPivotGrid read GetPivotGrid write SetPivotGrid;
  published
    property Area;
    property AreaIndex;
    property Caption;
    property DataBinding: TcxDBPivotGridFieldDataBinding read GetDataBinding write SetDataBinding;
    property SortOrder;
    property Visible;
  end;

  { TcxDBPivotGridDataController }

  TcxDBPivotGridDataController = class(TcxDBDataController)
  private
    function GetPivotGrid: TcxCustomDBPivotGrid;
  protected
    procedure DoValueTypeClassChanged(AItemIndex: Integer); override;
    procedure FilterChanged; override;
    procedure UpdateControl(AInfo: TcxUpdateControlInfo); override;
    property PivotGrid: TcxCustomDBPivotGrid read GetPivotGrid;
  public
    function GetFilterItemFieldCaption(AItem: TObject): string; override;
    function GetItem(Index: Integer): TObject; override;
    function GetItemValueSource(AItemIndex: Integer): TcxDataEditValueSource; override;
    function GetItemID(AItem: TObject): Integer; override;
    procedure UpdateData; override;
    procedure UpdateItemIndexes; override;
    procedure UpdateFields; override;
  end;

  { TcxCustomDBPivotGrid }

  TcxCustomDBPivotGrid = class(TcxCustomPivotGrid)
  private
    function GetDataController: TcxDBPivotGridDataController;
    function GetDataSource: TDataSource;
    function GetDataSet: TDataSet;
    procedure SetDataSource(AValue: TDataSource);
  protected
    function CanRetrieveFields(var AIsActive: Boolean): Boolean; override;
    function CreateDataController: TcxCustomDataController; override;
    procedure DoCreateAllFields; override;
    function GetFieldClass: TcxPivotGridFieldClass; override;

    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DataSet: TDataSet read GetDataSet;
    property DataController: TcxDBPivotGridDataController read GetDataController;
  public
    procedure CreateAllFields;
  end;

  { TcxDBPivotGrid }

  TcxDBPivotGrid = class(TcxCustomDBPivotGrid)
  public
    property DataController;
  published
    property Align;
    property Anchors;
    property BevelEdges;
    property BevelInner;
    property BevelOuter;
    property BevelKind;
    property BevelWidth;
    property BiDiMode;
    property BorderWidth;
    property BorderStyle;
    property Constraints;
    property DataSource;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FieldHeaderImages;
    property Font;
    property GroupHeaderImages;
    property Groups;
    property LookAndFeel;
    property OptionsBehavior;
    property OptionsCustomize;
    property OptionsData;
    property OptionsDataField;
    property OptionsLockedStateImage;
    property OptionsPrefilter;
    property OptionsSelection;
    property OptionsView;
    property ParentBiDiMode;
    property ParentFont;
    property PopupMenu;
    property PopupMenus;
    property ShowHint;
    property Styles;
    property TabOrder;
    property TabStop;
    property Visible;
    //events
    // PivotGrid
    property OnCompare;
    property OnCustomDrawFieldHeader;
    property OnCustomDrawColumnHeader;
    property OnCustomDrawRowHeader;
    property OnCustomDrawCell;
    property OnCustomDrawPart;
    property OnCustomization;
    property OnFieldPosChanged;
    property OnFieldSizeChanged;
    property OnFilterChanged;
    property OnGetCellHint;
    property OnPrefilterDialogShow;
    property OnPrepareLockedStateImage;
    property OnLayoutChanged;
    property OnSelectionChanged;
    // standard
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
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
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
  end;

implementation

uses
  cxDBEdit, cxEditDBRegisteredRepositoryItems;

type
  TcxPivotGridCustomizationAccess = class(TcxPivotGridCustomization);

{ TcxDBPivotGridDataController }

function TcxDBPivotGridDataController.GetFilterItemFieldCaption(AItem: TObject): string;
begin
  if AItem is TcxPivotGridField then
    Result := TcxPivotGridField(AItem).Caption
  else
    Result := inherited GetFilterItemFieldCaption(AItem);
end;

function TcxDBPivotGridDataController.GetItem(Index: Integer): TObject;
begin
  Result := PivotGrid.GetItem(Index);
end;

function TcxDBPivotGridDataController.GetItemValueSource(
  AItemIndex: Integer): TcxDataEditValueSource;
begin
  Result := PivotGrid.GetItemValueSource(AItemIndex);
end;

function TcxDBPivotGridDataController.GetItemID(AItem: TObject): Integer;
begin
  Result := PivotGrid.GetItemID(AItem);
end;

procedure TcxDBPivotGridDataController.UpdateData;
begin
  PivotGrid.UpdateData;
end;

procedure TcxDBPivotGridDataController.UpdateFields;
begin
  inherited UpdateFields;
  TcxPivotGridCustomizationAccess(PivotGrid.Customization).Refresh;
end;

procedure TcxDBPivotGridDataController.UpdateItemIndexes;
begin
  PivotGrid.UpdateItemIndexes;
  inherited UpdateItemIndexes;
end;

procedure TcxDBPivotGridDataController.DoValueTypeClassChanged(AItemIndex: Integer);
var
  AItem: TObject;
begin
  inherited;
  AItem := PivotGrid.GetItem(AItemIndex);
  if AItem is TcxDBPivotGridField then
    TcxDBPivotGridField(AItem).PropertiesValueChanged;
end;

procedure TcxDBPivotGridDataController.FilterChanged;
begin
  inherited;
  PivotGrid.DoPrefilterChanged;
end;

procedure TcxDBPivotGridDataController.UpdateControl(AInfo: TcxUpdateControlInfo);
begin
  if (AInfo is TcxFocusedRecordChangedInfo) or
    (AInfo is TcxFocusedRowChangedInfo) then Exit;
  PivotGrid.UpdateControl(AInfo);
end;

function TcxDBPivotGridDataController.GetPivotGrid: TcxCustomDBPivotGrid;
begin
  Result := TcxCustomDBPivotGrid(GetOwner);
end;

{ TcxCustomDBPivotGrid }

procedure TcxCustomDBPivotGrid.CreateAllFields;
var
  I: Integer;
  AName: string;
  AField: TcxDBPivotGridField;
begin
  if (DataSet = nil) or (DataSet.FieldCount = 0) then Exit;
  try
    BeginUpdate;
    try
      for I := 0 to DataSet.FieldCount - 1 do
      begin
        AField := TcxDBPivotGridField(CreateField);
        AName := DataSet.Fields[I].FieldName;
        AField.DataBinding.FieldName := AName;
        AField.Name := CreateUniqueName(Owner, Self, AField, '', AName);
        AField.Area := faFilter;
        AField.Visible := DataSet.Fields[I].Visible;
      end;
    finally
      EndUpdate;
    end;
  finally
    Modified;
  end;
end;

function TcxCustomDBPivotGrid.CanRetrieveFields(var AIsActive: Boolean): Boolean;
begin
  Result := True;
  AIsActive := (DataSet <> nil) and DataSet.Active;
end;

function TcxCustomDBPivotGrid.CreateDataController: TcxCustomDataController;
begin
  Result := TcxDBPivotGridDataController.Create(Self);
end;

procedure TcxCustomDBPivotGrid.DoCreateAllFields;
begin
  CreateAllFields;
end;

function TcxCustomDBPivotGrid.GetFieldClass: TcxPivotGridFieldClass;
begin
  Result := TcxDBPivotGridField;
end;

function TcxCustomDBPivotGrid.GetDataController: TcxDBPivotGridDataController;
begin
  Result := TcxDBPivotGridDataController(inherited DataController);
end;

function TcxCustomDBPivotGrid.GetDataSource: TDataSource;
begin
  Result := DataController.DataSource;
end;

procedure TcxCustomDBPivotGrid.SetDataSource(AValue: TDataSource);
begin
  if DataSource <> AValue then
    DataController.DataSource := AValue;
end;

function TcxCustomDBPivotGrid.GetDataSet: TDataSet;
begin
  if DataSource <> nil then
    Result := DataSource.DataSet
  else
    Result := nil;
end;

{ TcxDBPivotGridFieldDataBinding }

procedure TcxDBPivotGridFieldDataBinding.Assign(Source: TPersistent);
begin
  if Source is TcxDBPivotGridFieldDataBinding then
    FieldName := TcxDBPivotGridFieldDataBinding(Source).FieldName;
  inherited Assign(Source);
end;

function TcxDBPivotGridFieldDataBinding.GetDefaultValuesProviderClass: TcxCustomEditDefaultValuesProviderClass;
begin
  Result := TcxCustomDBEditDefaultValuesProvider;
end;

function TcxDBPivotGridFieldDataBinding.GetFilterFieldName: string;
begin
  if DBField = nil then
    Result := inherited GetFilterFieldName
  else
    Result := DBField.FieldName;
end;

procedure TcxDBPivotGridFieldDataBinding.Init;
begin
  inherited;
  TcxCustomDBEditDefaultValuesProvider(FDefaultValuesProvider).DataSource := DataController.DataSource;
  TcxCustomDBEditDefaultValuesProvider(FDefaultValuesProvider).Field := DBField;
end;

function TcxDBPivotGridFieldDataBinding.GetDataController: TcxDBPivotGridDataController;
begin
  Result := TcxCustomDBPivotGrid(PivotGrid).DataController;
end;

function TcxDBPivotGridFieldDataBinding.GetDBField: TField;
begin
  Result := DataController.GetItemField(Field.Index);
end;

function TcxDBPivotGridFieldDataBinding.GetField: TcxDBPivotGridField;
begin
  Result := TcxDBPivotGridField(inherited Field);
end;

function TcxDBPivotGridFieldDataBinding.GetFieldCaption: string;
begin
  Result := FieldName;
  if DBField <> nil then
    Result := DBField.DisplayName;
end;

function TcxDBPivotGridFieldDataBinding.GetFieldName: string;
begin
  Result := DataController.GetItemFieldName(Field.Index)
end;

procedure TcxDBPivotGridFieldDataBinding.SetFieldName(AValue: string);
begin
  DataController.ChangeFieldName(Field.Index, AValue);
end;

{ TcxDBPivotGridField }

function TcxDBPivotGridField.CreateDataBinding: TcxPivotGridFieldDataBinding;
begin
  Result := TcxDBPivotGridFieldDataBinding.Create(Self);
end;

function TcxDBPivotGridField.DefaultRepositoryItem: TcxEditRepositoryItem;
begin
  Result := GetDefaultEditDBRepositoryItems.GetItemByField(DataBinding.DBField);
end;

function TcxDBPivotGridField.GetActualDisplayFormat: string;
var
  AField: TField;
begin
  Result := inherited GetActualDisplayFormat;
  AField := DataBinding.DataController.GetItemField(Index);
  if (Result = '') and (AField is TNumericField) then
     Result := TNumericField(AField).DisplayFormat;
end;

function TcxDBPivotGridField.GetCaption: string;
begin
  Result := inherited GetCaption;
  if not IsCaptionAssigned and (DataBinding.FieldName <> '') then
    Result := DataBinding.FieldCaption;
end;

function TcxDBPivotGridField.IsCurrency(AType: TcxValueTypeClass): Boolean;
var
  AField: TField;
begin
  Result := inherited IsCurrency(AType);
  AField := DataBinding.DBField;
  if AField is TFloatField then
    Result := TFloatField(AField).Currency
  else
    if AField is TBCDField then
      Result := TBCDField(AField).Currency
    else
      if AField is TAggregateField then
        Result := TAggregateField(AField).Currency
    else
      if AField is TFMTBCDField then
      Result := TFMTBCDField(AField).Currency
end;

function TcxDBPivotGridField.GetDataBinding: TcxDBPivotGridFieldDataBinding;
begin
  Result := TcxDBPivotGridFieldDataBinding(inherited DataBinding);
end;

function TcxDBPivotGridField.GetPivotGrid: TcxCustomDBPivotGrid;
begin
  Result := TcxCustomDBPivotGrid(inherited PivotGrid);
end;

procedure TcxDBPivotGridField.SetDataBinding(
  AValue: TcxDBPivotGridFieldDataBinding);
begin
  DataBinding.Assign(AValue);
end;

procedure TcxDBPivotGridField.SetPivotGrid(
  AValue: TcxCustomDBPivotGrid);
begin
  inherited PivotGrid := AValue;
end;

end.
