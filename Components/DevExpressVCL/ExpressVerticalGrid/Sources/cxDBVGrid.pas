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
unit cxDBVGrid;

{$I cxVer.inc}

interface

uses
  cxVGrid, cxInplaceContainer, Classes, DB, cxCustomData, cxDBData,
  cxDataStorage, cxEdit, cxDBEdit, cxEditRepositoryItems, cxDataUtils;

type
  TcxDBVerticalGrid = class;
  TcxDBCellEdit = class;

  { TcxDBVerticalGridDefaultValuesProvider }

  TcxDBVerticalGridDefaultValuesProvider = class(TcxCustomDBEditDefaultValuesProvider)
  public
    function IsDisplayFormatDefined(AIsCurrencyValueAccepted: Boolean): Boolean; override;
  end;

  { TcxDBVerticalGridDataController }

  TcxDBVerticalGridDataController = class(TcxDBDataController,
    IcxVerticalGridDBDataContoller)
  private
    FPrevBufferCount: Integer;
    FPrevScrollBarPos: Integer;
    function GetVerticalGrid: TcxDBVerticalGrid;
    function GetGridMode: Boolean;
    function GetGridModeBufferCount: Integer;
    function GetScroller: TcxvgScroller;
    procedure SetGridMode(Value: Boolean);
    procedure SetGridModeBufferCount(Value: Integer);
  protected
    // IcxVerticalGridDBDataContoller
    procedure CheckGridModeBufferCount;
    function DoScroll(AForward: Boolean): Boolean;
    function GetDataSetRecordCount: Integer;
    function GetScrollBarPos: Integer;
    function GetScrollBarRecordCount: Integer;
    function IsRecordPixelScrollingSupported: Boolean;
    function SetScrollBarPos(APos: Integer): Boolean;

    function GetDefaultGridModeBufferCount: Integer; override;
    function GetItemID(AItem: TObject): Integer; override;
    function SupportsScrollBarParams: Boolean; virtual;
    procedure UpdateControl(AInfo: TcxUpdateControlInfo); override;
    procedure UpdateScrollBars; override;
    property Scroller: TcxvgScroller read GetScroller;
    property VerticalGrid: TcxDBVerticalGrid read GetVerticalGrid;
  public
    procedure CreateAllItems;
    procedure FocusControl(AItemIndex: Integer; var Done: Boolean); override;
    function GetFilterItemFieldCaption(AItem: TObject): string; override;
    function GetItem(Index: Integer): TObject; override;
    function GetItemValueSource(AItemIndex: Integer): TcxDataEditValueSource; override;
    procedure UpdateData; override;
    procedure UpdateItemIndexes; override;
  published
    property DataSource;
    property GridMode: Boolean read GetGridMode write SetGridMode default False;
    property GridModeBufferCount: Integer read GetGridModeBufferCount write SetGridModeBufferCount default 0;
  end;

  { TcxDBVerticalGridItemDataBinding }

  TcxDBVerticalGridItemDataBinding = class(TcxCustomItemDataBinding)
  private
    function GetDataController: TcxDBVerticalGridDataController;
    function GetField: TField;
    function GetFieldName: string;
    procedure SetFieldName(const Value: string);
  protected
    function DefaultRepositoryItem: TcxEditRepositoryItem; override;
    function GetDefaultValuesProviderClass: TcxCustomEditDefaultValuesProviderClass; override;
    function GetFilterFieldName: string; override;
    function GetValueTypeClass: TcxValueTypeClass; override;
    procedure Init; override;
    procedure ValueTypeClassChanged; override;
  public
    procedure Assign(Source: TPersistent); override;
    function DefaultCaption: string; virtual;
    property DataController: TcxDBVerticalGridDataController read GetDataController;
    property Field: TField read GetField;
  published
    property FieldName: string read GetFieldName write SetFieldName;
  end;

  { TcxDBCellEdit }

  TcxDBCellEdit = class(TcxCellEdit)
  protected
    function GetDataBindingClass: TcxItemDataBindingClass; override;
  end;

  { TcxDBEditorRowItemProperties }

  TcxDBEditorRowItemProperties = class(TcxCollectionItemEditorRowProperties)
  private
    function GetDataBinding: TcxDBVerticalGridItemDataBinding;
    procedure SetDataBinding(Value: TcxDBVerticalGridItemDataBinding);
  protected
    function DefaultCaption: string; override;
    function GetDisplayName: string; override;
    function GetInplaceEditContainerClass: TcxCellEditClass; override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property DataBinding: TcxDBVerticalGridItemDataBinding read GetDataBinding write SetDataBinding;
    property Options;
    property OnGetDisplayText;
    property OnGetEditProperties;
    property OnGetEditingProperties;
    property OnValidateDrawValue;
  end;

  { TcxDBEditorPropertiesCollection }

  TcxDBEditorPropertiesCollection = class(TcxEditorPropertiesCollection)
  private
    function GetItem(Index: Integer): TcxDBEditorRowItemProperties;
  protected
    function GetCollectionItemClass: TCollectionItemClass; override;
  public
    function Add: TcxDBEditorRowItemProperties;
    property Items[Index: Integer]: TcxDBEditorRowItemProperties read GetItem; default;
  end;

  { TcxDBEditorRowProperties }

  TcxDBEditorRowProperties = class(TcxCustomEditorRowProperties)
  private
    function GetDataBinding: TcxDBVerticalGridItemDataBinding;
    procedure SetDataBinding(Value: TcxDBVerticalGridItemDataBinding);
  protected
    function DefaultCaption: string; override;
    function GetInplaceEditContainerClass: TcxCellEditClass; override;
  public
    property Value;
  published
    property DataBinding: TcxDBVerticalGridItemDataBinding read GetDataBinding write SetDataBinding;
    property Options;
    property OnGetDisplayText;
    property OnGetEditProperties;
    property OnGetEditingProperties;
    property OnValidateDrawValue;
  end;

  { TcxDBEditorRow }

  TcxDBEditorRow = class(TcxCustomEditorRow)
  private
    function GetProperties: TcxDBEditorRowProperties;
    procedure SetProperties(Value: TcxDBEditorRowProperties);
  protected
    function GetPropertiesClass: TcxRowPropertiesClass; override;
  published
    property Expanded;
    property Height;
    property Options;
    property Properties: TcxDBEditorRowProperties read GetProperties write SetProperties;
    property Styles;
    property Visible;
  end;

  { TcxDBMultiEditorRowProperties }

  TcxDBMultiEditorRowProperties = class(TcxMultiEditorRowProperties)
  private
    function GetEditors: TcxDBEditorPropertiesCollection;
    procedure SetEditors(Value: TcxDBEditorPropertiesCollection);
  protected
    function GetCollectionClass: TcxEditorPropertiesCollectionClass; override;
  published
    property Editors: TcxDBEditorPropertiesCollection read GetEditors write SetEditors;
  end;

  { TcxDBMultiEditorRow }

  TcxDBMultiEditorRow = class(TcxCustomMultiEditorRow)
  private
    function GetProperties: TcxDBMultiEditorRowProperties;
    procedure SetProperties(Value: TcxDBMultiEditorRowProperties);
  protected
    function GetPropertiesClass: TcxRowPropertiesClass; override;
  published
    property Expanded;
    property Height;
    property Options;
    property Properties: TcxDBMultiEditorRowProperties read GetProperties write SetProperties;
    property Styles;
    property Visible;
  end;

  { TcxDBVerticalGrid }

  TcxDBVerticalGrid = class(TcxVirtualVerticalGrid, IcxVGridDesignerRows)
  private
    function GetDataController: TcxDBVerticalGridDataController;
    procedure SetDataController(Value: TcxDBVerticalGridDataController);
  protected
    function GetDataControllerClass: TcxCustomDataControllerClass; override;
    function GetEditCellDataBindingClass: TcxItemDataBindingClass; override;
    function GetEditorRowClass: TcxCustomRowClass; override;
    function GetMultiEditorRowClass: TcxCustomRowClass; override;
    procedure InitDataController; override;
  public
    destructor Destroy; override;
  published
    property Align;
    property Anchors;
    property Constraints;
    property DataController: TcxDBVerticalGridDataController read GetDataController write SetDataController;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property Images;
    property LayoutStyle;
    property LookAndFeel;
    property OptionsView; //before OptionsBehavior
    property OptionsBehavior;
    property OptionsData;
    property ParentFont;
    property PopupMenu;
    property Styles;
    property TabOrder;
    property TabStop;
    property Visible;

    property OnClick;
    property OnContextPopup;
    property OnCustomizationFormVisibleChanged;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawBackground;
    property OnDrawRowHeader;
    property OnDrawValue;
    property OnEdited;
    property OnEditing;
    property OnEditValueChanged;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnFilterControlDialogShow;
    property OnFilterRecord;
    property OnFocusedRecordChanged;
    property OnInitEdit;
    property OnItemChanged;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnLayoutChanged;
    property OnLeftVisibleBandIndexChanged;
    property OnLeftVisibleRecordIndexChanged;
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
    property OnTopRowIndexChanged;
  end;

implementation

uses
  SysUtils, cxGraphics, cxControls, cxLookAndFeelPainters, cxEditDBRegisteredRepositoryItems, Types,
  cxGeometry, cxVGridViewInfo, cxClasses;

type
  TcxCellEditAccess = class(TcxCellEdit);

{ TcxDBVerticalGridDefaultValuesProvider }

function TcxDBVerticalGridDefaultValuesProvider.IsDisplayFormatDefined(AIsCurrencyValueAccepted: Boolean): Boolean;
begin
  Result := TcxDBVerticalGridItemDataBinding(Owner).IsDisplayFormatDefined(AIsCurrencyValueAccepted);
end;

{ TcxDBVerticalGridDataController }

procedure TcxDBVerticalGridDataController.CreateAllItems;
var
  I: Integer;
  AItem: TcxDBEditorRow;
begin
  if DataSet = nil then Exit;
  ShowHourglassCursor;
  try
    VerticalGrid.BeginUpdate;
    try
      with DataSet do
        for I := 0 to FieldCount - 1 do
        begin
          AItem := TcxDBEditorRow(VerticalGrid.Add(VerticalGrid.GetEditorRowClass));
          AItem.Name := CreateUniqueName(VerticalGrid.Owner, VerticalGrid, AItem, 'Tcx', Fields[I].FieldName);
          with AItem.Properties do
          begin
            DataBinding.FieldName := Fields[I].FieldName;
            Caption := DataBinding.DefaultCaption;
          end;
          AItem.Visible := Fields[I].Visible;
        end;
    finally
      VerticalGrid.EndUpdate;
    end;
  finally
    HideHourglassCursor;
  end;
end;

procedure TcxDBVerticalGridDataController.FocusControl(AItemIndex: Integer;
  var Done: Boolean);
begin
  TcxDBCellEdit(GetItem(AItemIndex)).Editing := True;
  Done := TcxDBCellEdit(GetItem(AItemIndex)).Editing;
end;

function TcxDBVerticalGridDataController.GetFilterItemFieldCaption(AItem: TObject): string;
var
  ACellEdit: TcxCellEditAccess absolute AItem;
begin
  Result := ACellEdit.FilterCaption;
end;

function TcxDBVerticalGridDataController.GetItem(Index: Integer): TObject;
begin
  Result := VerticalGrid.ContainerList.List[Index];
end;

function TcxDBVerticalGridDataController.GetItemValueSource(
  AItemIndex: Integer): TcxDataEditValueSource;
begin
  Result := TcxDBCellEdit(GetItem(AItemIndex)).PropertiesValue.GetEditValueSource(True);
end;

procedure TcxDBVerticalGridDataController.UpdateData;
begin
  VerticalGrid.UpdateData;
end;

procedure TcxDBVerticalGridDataController.UpdateItemIndexes;
begin
  VerticalGrid.UpdateIndexes;
  inherited UpdateItemIndexes;
end;

procedure TcxDBVerticalGridDataController.CheckGridModeBufferCount;
var
  ACount: Integer;
begin
  ACount := GetDefaultGridModeBufferCount;
  if FPrevBufferCount <> ACount then
    UpdateGridModeBufferCount;
  FPrevBufferCount := ACount;
end;

function TcxDBVerticalGridDataController.DoScroll(AForward: Boolean): Boolean;
begin
  Result := SupportsScrollBarParams;
  if Result then
    with Scroller do
      if AForward then GoToNext else GoToPrev;
end;

function TcxDBVerticalGridDataController.GetDataSetRecordCount: Integer;
begin
  if IsGridMode then
    Result := DataSetRecordCount
  else
    Result := RowCount;
end;

function TcxDBVerticalGridDataController.GetScrollBarPos: Integer;
begin
  if SupportsScrollBarParams then
    if dceInsert in EditState then
      Result := FPrevScrollBarPos
    else
      Result := RecNo - 1
  else
    Result := -1;
  FPrevScrollBarPos := Result;
end;

function TcxDBVerticalGridDataController.GetScrollBarRecordCount: Integer;
begin
  if SupportsScrollBarParams then
    Result := DataSetRecordCount + Scroller.VisibleValueCount - 1
  else
    Result := -1;
end;

function TcxDBVerticalGridDataController.IsRecordPixelScrollingSupported: Boolean;
begin
  Result := not SupportsScrollBarParams;
end;

function TcxDBVerticalGridDataController.SetScrollBarPos(APos: Integer): Boolean;
begin
  Result := SupportsScrollBarParams;
  if Result then
    RecNo := APos + 1;
end;

function TcxDBVerticalGridDataController.GetDefaultGridModeBufferCount: Integer;
begin
  Result := VerticalGrid.ViewInfo.GetDefaultGridModeBufferCount;
  if Result = 0 then
    Result := inherited GetDefaultGridModeBufferCount;
end;

function TcxDBVerticalGridDataController.GetItemID(AItem: TObject): Integer;
begin
  if AItem is TcxDBCellEdit then
    Result := TcxDBCellEdit(AItem).ItemIndex
  else
    Result := -1;
end;

function TcxDBVerticalGridDataController.SupportsScrollBarParams: Boolean;
begin
  Result := IsGridMode and IsSequenced;
end;

procedure TcxDBVerticalGridDataController.UpdateControl(
  AInfo: TcxUpdateControlInfo);
begin
  VerticalGrid.ControlUpdateData(AInfo);
end;

procedure TcxDBVerticalGridDataController.UpdateScrollBars;
begin
  // refresh for GridMode
  VerticalGrid.LayoutChanged;
end;

function TcxDBVerticalGridDataController.GetGridMode: Boolean;
begin
  Result := DataModeController.GridMode;
end;

function TcxDBVerticalGridDataController.GetGridModeBufferCount: Integer;
begin
  Result := DataModeController.GridModeBufferCount;
end;

function TcxDBVerticalGridDataController.GetScroller: TcxvgScroller;
begin
  Result := VerticalGrid.Controller.Scroller;
end;

procedure TcxDBVerticalGridDataController.SetGridMode(Value: Boolean);
begin
  DataModeController.GridMode := Value;
end;

procedure TcxDBVerticalGridDataController.SetGridModeBufferCount(Value: Integer);
begin
  DataModeController.GridModeBufferCount := Value;
end;

function TcxDBVerticalGridDataController.GetVerticalGrid: TcxDBVerticalGrid;
begin
  Result := TcxDBVerticalGrid(GetOwner);
end;

{ TcxDBCellEdit }

function TcxDBCellEdit.GetDataBindingClass: TcxItemDataBindingClass;
begin
  Result := TcxDBVerticalGridItemDataBinding;
end;

{ TcxDBVerticalGridItemDataBinding }

procedure TcxDBVerticalGridItemDataBinding.Assign(Source: TPersistent);
begin
  if Source is TcxDBVerticalGridItemDataBinding then
    FieldName := TcxDBVerticalGridItemDataBinding(Source).FieldName;
end;

function TcxDBVerticalGridItemDataBinding.DefaultCaption: string;
var
  AField: TField;
begin
  AField := Field;
  if AField = nil then
    Result := FieldName
  else
    Result := AField.DisplayName;
end;

function TcxDBVerticalGridItemDataBinding.DefaultRepositoryItem: TcxEditRepositoryItem;
begin
  Result := GetDefaultEditDBRepositoryItems.GetItemByDataBinding(Field, Self);
end;

function TcxDBVerticalGridItemDataBinding.GetDefaultValuesProviderClass: TcxCustomEditDefaultValuesProviderClass;
begin
  Result := TcxDBVerticalGridDefaultValuesProvider;
end;

function TcxDBVerticalGridItemDataBinding.GetFilterFieldName: string;
begin
  if Field = nil then
    Result := ''
  else
    Result := Field.FieldName;
end;

function TcxDBVerticalGridItemDataBinding.GetValueTypeClass: TcxValueTypeClass;
begin
  Result := GetValueTypeClassByField(Field);
end;

procedure TcxDBVerticalGridItemDataBinding.Init;
begin
  inherited Init;
  TcxDBVerticalGridDefaultValuesProvider(DefaultValuesProvider).DataSource := DataController.DataSource;
  TcxDBVerticalGridDefaultValuesProvider(DefaultValuesProvider).Field := Field;
end;

procedure TcxDBVerticalGridItemDataBinding.ValueTypeClassChanged;
begin
  TcxDBCellEdit(EditContainer).InternalPropertiesChanged;
end;

function TcxDBVerticalGridItemDataBinding.GetDataController: TcxDBVerticalGridDataController;
begin
  Result := TcxDBVerticalGridDataController(inherited DataController);
end;

function TcxDBVerticalGridItemDataBinding.GetField: TField;
begin
  Result := DataController.GetItemField(EditContainer.ItemIndex);
end;

function TcxDBVerticalGridItemDataBinding.GetFieldName: string;
begin
  Result := DataController.GetItemFieldName(EditContainer.ItemIndex);
end;

procedure TcxDBVerticalGridItemDataBinding.SetFieldName(const Value: string);
begin
  DataController.ChangeFieldName(EditContainer.ItemIndex, Value);
  Init;
  ValueTypeClassChanged;
end;

{ TcxDBEditorRowItemProperties }

procedure TcxDBEditorRowItemProperties.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TcxDBEditorRowItemProperties then
  begin
    Options := TcxDBEditorRowItemProperties(Source).Options;
    DataBinding := TcxDBEditorRowItemProperties(Source).DataBinding;
  end;
end;

function TcxDBEditorRowItemProperties.DefaultCaption: string;
begin
  Result := DataBinding.DefaultCaption;
end;

function TcxDBEditorRowItemProperties.GetDisplayName: string;
begin
  Result := Caption;
  if DataBinding.FieldName <> '' then
    Result := Format('%s [%s]', [Result, DataBinding.FieldName]);
end;

function TcxDBEditorRowItemProperties.GetInplaceEditContainerClass: TcxCellEditClass;
begin
  Result := TcxDBCellEdit;
end;

function TcxDBEditorRowItemProperties.GetDataBinding: TcxDBVerticalGridItemDataBinding;
begin
  Result := TcxDBVerticalGridItemDataBinding(inherited DataBinding);
end;

procedure TcxDBEditorRowItemProperties.SetDataBinding(
  Value: TcxDBVerticalGridItemDataBinding);
begin
  inherited DataBinding.Assign(Value);
end;

{ TcxDBEditorPropertiesCollection }

function TcxDBEditorPropertiesCollection.Add: TcxDBEditorRowItemProperties;
begin
  Result := TcxDBEditorRowItemProperties(inherited Add);
end;

function TcxDBEditorPropertiesCollection.GetCollectionItemClass: TCollectionItemClass;
begin
  Result := TcxDBEditorRowItemProperties;
end;

function TcxDBEditorPropertiesCollection.GetItem(
  Index: Integer): TcxDBEditorRowItemProperties;
begin
  Result := TcxDBEditorRowItemProperties(inherited Items[Index]);
end;

{ TcxDBEditorRowProperties }

function TcxDBEditorRowProperties.DefaultCaption: string;
begin
  Result := DataBinding.DefaultCaption;
end;

function TcxDBEditorRowProperties.GetInplaceEditContainerClass: TcxCellEditClass;
begin
  Result := TcxDBCellEdit;
end;

function TcxDBEditorRowProperties.GetDataBinding: TcxDBVerticalGridItemDataBinding;
begin
  Result := TcxDBVerticalGridItemDataBinding(inherited DataBinding);
end;

procedure TcxDBEditorRowProperties.SetDataBinding(
  Value: TcxDBVerticalGridItemDataBinding);
begin
  inherited DataBinding.Assign(Value);
end;

{ TcxDBEditorRow }

function TcxDBEditorRow.GetPropertiesClass: TcxRowPropertiesClass;
begin
  Result := TcxDBEditorRowProperties;
end;

function TcxDBEditorRow.GetProperties: TcxDBEditorRowProperties;
begin
  Result := TcxDBEditorRowProperties(FProperties)
end;

procedure TcxDBEditorRow.SetProperties(Value: TcxDBEditorRowProperties);
begin
  FProperties.Assign(Value);
end;

{ TcxDBMultiEditorRowProperties }

function TcxDBMultiEditorRowProperties.GetCollectionClass: TcxEditorPropertiesCollectionClass;
begin
  Result := TcxDBEditorPropertiesCollection;
end;

function TcxDBMultiEditorRowProperties.GetEditors: TcxDBEditorPropertiesCollection;
begin
  Result := TcxDBEditorPropertiesCollection(inherited Editors);
end;

procedure TcxDBMultiEditorRowProperties.SetEditors(
  Value: TcxDBEditorPropertiesCollection);
begin
  inherited Editors.Assign(Value);
end;

{ TcxDBMultiEditorRow }

function TcxDBMultiEditorRow.GetPropertiesClass: TcxRowPropertiesClass;
begin
  Result := TcxDBMultiEditorRowProperties;
end;

function TcxDBMultiEditorRow.GetProperties: TcxDBMultiEditorRowProperties;
begin
  Result := TcxDBMultiEditorRowProperties(FProperties);
end;

procedure TcxDBMultiEditorRow.SetProperties(
  Value: TcxDBMultiEditorRowProperties);
begin
  FProperties.Assign(Value);
end;

{ TcxDBVerticalGrid }

destructor TcxDBVerticalGrid.Destroy;
begin
  BeginUpdate;
  DataController.DataSource := nil;
  inherited Destroy;
end;

function TcxDBVerticalGrid.GetDataControllerClass: TcxCustomDataControllerClass;
begin
  Result := TcxDBVerticalGridDataController;
end;

function TcxDBVerticalGrid.GetEditCellDataBindingClass: TcxItemDataBindingClass;
begin
  Result := TcxDBVerticalGridItemDataBinding;
end;

function TcxDBVerticalGrid.GetEditorRowClass: TcxCustomRowClass;
begin
  Result := TcxDBEditorRow;
end;

function TcxDBVerticalGrid.GetMultiEditorRowClass: TcxCustomRowClass;
begin
  Result := TcxDBMultiEditorRow;
end;

procedure TcxDBVerticalGrid.InitDataController;
begin
end;

function TcxDBVerticalGrid.GetDataController: TcxDBVerticalGridDataController;
begin
  Result := TcxDBVerticalGridDataController(FDataController);
end;

procedure TcxDBVerticalGrid.SetDataController(
  Value: TcxDBVerticalGridDataController);
begin
  FDataController.Assign(Value);
end;

initialization
  RegisterClasses([TcxDBVerticalGridItemDataBinding]);
  RegisterClasses([TcxDBEditorRow, TcxDBMultiEditorRow]);
  RegisterClasses([TcxDBEditorRowProperties, TcxDBMultiEditorRowProperties]);

end.

