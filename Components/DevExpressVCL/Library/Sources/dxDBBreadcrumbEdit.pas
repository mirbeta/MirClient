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

unit dxDBBreadcrumbEdit;

interface

{$I cxVer.inc}

uses
  Types, Windows, SysUtils, Classes, Variants, Graphics, Controls, DB,
  dxBreadcrumbEdit, dxCustomTree, cxVariants, dxCore, dxCoreClasses, cxDBData;

type
  TdxCustomDBBreadCrumbEdit = class;
  TdxDBBreadcrumbEditDataBinding = class;

  EdxDBBreadcrumbEditValidationError = class(EdxBreadcrumbEditValidationError);

  { IdxDBBreadCrumbEdit }

  IdxDBBreadCrumbEdit = interface(IdxBreadCrumbEdit)
  ['{B5813847-E4A4-4789-8673-488107711B74}']
    function GetDataBinding: TdxDBBreadcrumbEditDataBinding;
    function GetRootValue: Variant;
    procedure RecordChanged(AField: TField);
    procedure StructureChanged;
    procedure SynchronizeSelection;
  end;

  { TdxDBBreadcrumbEditDataLink }

  TdxDBBreadcrumbEditDataLink = class(TcxDBCustomDataLink)
  private
    FDataBusy: Boolean;
    FDataLocate: Boolean;
    FOwner: TdxDBBreadcrumbEditDataBinding;
  protected
    procedure ActiveChanged; override;
    procedure DataSetChanged; override;
    procedure DataSetScrolled(Distance: Integer); override;
    function GetIsDataSetBusyState: Boolean; override;
    procedure LayoutChanged; override;
    procedure RecordChanged(Field: TField); override;
    procedure SelectionChanged; virtual;
    procedure StructureChanged; virtual;
    procedure UpdateData; override;
  public
    constructor Create(AOwner: TdxDBBreadcrumbEditDataBinding); virtual;

    property DataBusy: Boolean read FDataBusy write FDataBusy;
    property DataLocate: Boolean read FDataLocate write FDataLocate;
    property Owner: TdxDBBreadcrumbEditDataBinding read FOwner;
  end;


  { TdxDBBreadcrumbEditDataBinding }

  TdxDBBreadcrumbEditDataBinding = class(TcxOwnedPersistent)
  private
    FControl: IdxDBBreadCrumbEdit;
    FDataLink: TdxDBBreadcrumbEditDataLink;
    FImageIndexField: TField;
    FImageIndexFieldName: string;
    FKeyField: TField;
    FKeyFieldName: string;
    FNameField: TField;
    FNameFieldName: string;
    FParentKeyField: TField;
    FParentKeyFieldName: string;
    function GetDataSet: TDataSet;
    function GetDataSource: TDataSource;
    function GetIsRequeredFieldsAssigned: Boolean;
    procedure DoSetFieldValue(const AValue: string; var AFieldValue: string);
    procedure SetDataSource(AValue: TDataSource);
    procedure SetImageIndexFieldName(const AValue: string);
    procedure SetKeyFieldName(const AValue: string);
    procedure SetNameFieldName(const AValue: string);
    procedure SetParentKeyFieldName(const AValue: string);
  protected
    function CreateDataLink: TdxDBBreadcrumbEditDataLink; virtual;
    function GetFieldByName(const AFieldName: string): TField;
    procedure BindFields; virtual;
    procedure RecordChanged(AField: TField); virtual;
    procedure SelectionChanged; virtual;
    procedure StructureChanged; virtual;
    //
    property Control: IdxDBBreadCrumbEdit read FControl;
    property DataLink: TdxDBBreadcrumbEditDataLink read FDataLink;
    property DataSet: TDataSet read GetDataSet;
    property FieldImageIndex: TField read FImageIndexField;
    property FieldKey: TField read FKeyField;
    property FieldName: TField read FNameField;
    property FieldParentKey: TField read FParentKeyField;
    property IsRequeredFieldsAssigned: Boolean read GetIsRequeredFieldsAssigned;
  public
    constructor Create(AOwner: TPersistent; AControl: IdxDBBreadCrumbEdit); reintroduce; virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property ImageIndexField: string read FImageIndexFieldName write SetImageIndexFieldName;
    property KeyField: string read FKeyFieldName write SetKeyFieldName;
    property NameField: string read FNameFieldName write SetNameFieldName;
    property ParentKeyField: string read FParentKeyFieldName write SetParentKeyFieldName;
  end;

  { TdxDBBreadcrumbEditProperties }

  TdxDBBreadcrumbEditProperties = class(TdxCustomBreadcrumbEditProperties)
  published
    property Borders;
    property ButtonImages;
    property Buttons;
    property DropDownIndent;
    property DropDownRows;
    property Images;
    property PathEditor;
    property ProgressBar;
  end;

  { TdxDBBreadcrumbEditNode }

  TdxDBBreadcrumbEditNode = class(TdxBreadcrumbEditNode)
  private
    FKeyValue: Variant;
    FParentKeyValue: Variant;
    function GetItem(AIndex: Integer): TdxDBBreadcrumbEditNode;
    function GetParent: TdxDBBreadcrumbEditNode;
  public
    function AddChild: TdxDBBreadcrumbEditNode; overload;
    procedure Clear; override;
    //
    property Items[AIndex: Integer]: TdxDBBreadcrumbEditNode read GetItem; default;
    property KeyValue: Variant read FKeyValue write FKeyValue;
    property Parent: TdxDBBreadcrumbEditNode read GetParent;
    property ParentKeyValue: Variant read FParentKeyValue write FParentKeyValue;
  end;

  { TdxDBBreadcrumbEditDataLoader }

  TdxDBBreadcrumbEditDataLoader = class(TcxIUnknownObject)
  private
    FControl: IdxDBBreadCrumbEdit;
    function GetDataBinding: TdxDBBreadcrumbEditDataBinding;
    function GetDataSet: TDataSet;
    function GetRootNode: TdxDBBreadcrumbEditNode;
  protected
    procedure BuildStructure(AList: TList); virtual;
    function FindLoadedNode(AList: TList; const AParentKeyValue: Variant; out ANode: TdxDBBreadcrumbEditNode): Boolean;
    function GetImageIndexAsInteger(AField: TField): Integer;
    function GetNameAsString(AField: TField): string;
    procedure LoadAllRecords(AList: TList); virtual;
  public
    constructor Create(AControl: IdxDBBreadCrumbEdit); virtual;
    procedure LoadNodeData(ANode: TdxDBBreadcrumbEditNode); virtual;
    procedure RebuildStructure; virtual;
    //
    property Control: IdxDBBreadCrumbEdit read FControl;
    property DataBinding: TdxDBBreadcrumbEditDataBinding read GetDataBinding;
    property DataSet: TDataSet read GetDataSet;
    property RootNode: TdxDBBreadcrumbEditNode read GetRootNode;
  end;

  { TdxDBBreadcrumbEditController }

  TdxDBBreadcrumbEditControllerState = (dxdbbecsSynchronizingSelection, dxdbbecsUpdatingRecord, dxdbbecsLocating);
  TdxDBBreadcrumbEditControllerStates = set of TdxDBBreadcrumbEditControllerState;

  TdxDBBreadcrumbEditController = class(TdxBreadcrumbEditController)
  private
    function GetControl: IdxDBBreadCrumbEdit;
    function GetDataBinding: TdxDBBreadcrumbEditDataBinding;
    function GetDataSet: TDataSet;
    function GetRoot: TdxDBBreadcrumbEditNode;
  protected
    FState: TdxDBBreadcrumbEditControllerStates;
    function CanSelectNode(ANode: TdxBreadcrumbEditNode): Boolean; override;
    procedure SelectionChanged; override;
    //
    property Root: TdxDBBreadcrumbEditNode read GetRoot;
  public
    function FindNodeByKeyValue(const AKeyValue: Variant): TdxDBBreadcrumbEditNode; overload;
    function FindNodeByKeyValue(const AKeyValue: Variant; out ANode: TdxDBBreadcrumbEditNode): Boolean; overload; virtual;
    function ProcessParentKeyValueChanges(const ANewParentKeyValue: Variant): Boolean; virtual;
    procedure Locate(ANode: TdxDBBreadcrumbEditNode); virtual;
    procedure SynchronizeSelection; virtual;
    //
    property Control: IdxDBBreadCrumbEdit read GetControl;
    property DataBinding: TdxDBBreadcrumbEditDataBinding read GetDataBinding;
    property DataSet: TDataSet read GetDataSet;
    property State: TdxDBBreadcrumbEditControllerStates read FState;
  end;

  { TdxCustomDBBreadcrumbEdit }

  TdxCustomDBBreadcrumbEdit = class(TdxCustomBreadcrumbEdit, IdxDBBreadCrumbEdit)
  private
    FDataBinding: TdxDBBreadcrumbEditDataBinding;
    FDataLoader: TdxDBBreadcrumbEditDataLoader;
    FRootValue: Variant;
    function GetDBController: TdxDBBreadcrumbEditController;
    function GetDBProperties: TdxDBBreadcrumbEditProperties;
    function GetDBRoot: TdxDBBreadcrumbEditNode;
    function GetSelected: TdxDBBreadcrumbEditNode;
    procedure SetDataBinding(AValue: TdxDBBreadcrumbEditDataBinding);
    procedure SetDBProperties(AValue: TdxDBBreadcrumbEditProperties);
    procedure SetRootValue(const AValue: Variant);
    procedure SetSelected(AValue: TdxDBBreadcrumbEditNode);
  protected
    function CreateController: TdxBreadcrumbEditController; override;
    function CreateDataBinding: TdxDBBreadcrumbEditDataBinding; virtual;
    function CreateDataLoader: TdxDBBreadcrumbEditDataLoader; virtual;
    function CreateProperties: TdxCustomBreadcrumbEditProperties; override;
    function CreateRoot: TdxBreadcrumbEditNode; override;
    // IdxDBBreadCrumbEdit
    function GetDataBinding: TdxDBBreadcrumbEditDataBinding;
    function GetRootValue: Variant;
    procedure RecordChanged(AField: TField);
    procedure StructureChanged;
    procedure SynchronizeSelection;
    function IdxDBBreadCrumbEdit.GetLookAndFeel = GetLookAndFeelValue;
    //
    property Controller: TdxDBBreadcrumbEditController read GetDBController;
    property DataLoader: TdxDBBreadcrumbEditDataLoader read FDataLoader;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    //
    property DataBinding: TdxDBBreadcrumbEditDataBinding read GetDataBinding write SetDataBinding;
    property Properties: TdxDBBreadcrumbEditProperties read GetDBProperties write SetDBProperties;
    property Root: TdxDBBreadcrumbEditNode read GetDBRoot;
    property RootValue: Variant read GetRootValue write SetRootValue;
    property Selected: TdxDBBreadcrumbEditNode read GetSelected write SetSelected;
  end;

  { TdxDBBreadcrumbEdit }

  TdxDBBreadcrumbEdit = class(TdxCustomDBBreadcrumbEdit)
  published
    property Align;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property Color;
    property DataBinding;
    property Enabled;
    property Font;
    property LookAndFeel;
    property ParentBiDiMode;
    property ParentShowHint;
    property Properties;
    property RootValue;
    property ShowHint;
    property TabOrder;
    property Transparent;
    property Visible;

    property OnButtonClick;
    property OnPathEntered;
    property OnPathSelected;
    property OnPathValidate;
    property OnPopulateAutoCompleteSuggestions;
  end;

implementation

uses
  cxEdit, cxClasses, cxEditConsts;

function cxCompareByParentKeyValue(ANode1, ANode2: TdxDBBreadcrumbEditNode): Integer;
begin
  Result := VarCompare(ANode1.ParentKeyValue, ANode2.ParentKeyValue);
end;

{ TdxDBBreadcrumbEditDataLink }

constructor TdxDBBreadcrumbEditDataLink.Create(
   AOwner: TdxDBBreadcrumbEditDataBinding);
begin
  inherited Create;
  FOwner := AOwner;
end;

procedure TdxDBBreadcrumbEditDataLink.ActiveChanged;
begin
  StructureChanged;
end;

procedure TdxDBBreadcrumbEditDataLink.DataSetChanged;
begin
  if IsDataSetBusy then
  begin
    DataSetScrolled(0);
    Exit;
  end;
  if DataLocate then
  begin
    DataLocate := False;
    Exit;
  end;
  if DataBusy or (DataSet.State in dsEditModes) then
    Exit;
  StructureChanged;
end;

procedure TdxDBBreadcrumbEditDataLink.DataSetScrolled(Distance: Integer);
begin
  SelectionChanged;
end;

function TdxDBBreadcrumbEditDataLink.GetIsDataSetBusyState: Boolean;
begin
  Result := DataBusy;
end;

procedure TdxDBBreadcrumbEditDataLink.LayoutChanged;
begin
  if DataBusy then
    StructureChanged
  else
    DataSetChanged;
end;

procedure TdxDBBreadcrumbEditDataLink.RecordChanged(Field: TField);
begin
  Owner.RecordChanged(Field);
end;

procedure TdxDBBreadcrumbEditDataLink.SelectionChanged;
begin
  Owner.SelectionChanged;
end;

procedure TdxDBBreadcrumbEditDataLink.StructureChanged;
begin
  DataBusy := True;
  try
    Owner.StructureChanged;
  finally
    DataBusy  := False;
  end;
end;

procedure TdxDBBreadcrumbEditDataLink.UpdateData;
begin
  RecordChanged(nil);
end;

{ TdxDBBreadcrumbEditDataBinding }

constructor TdxDBBreadcrumbEditDataBinding.Create(AOwner: TPersistent; AControl: IdxDBBreadCrumbEdit);
begin
  inherited Create(AOwner);
  FControl := AControl;
  FDataLink := CreateDataLink;
end;

destructor TdxDBBreadcrumbEditDataBinding.Destroy;
begin
  FreeAndNil(FDataLink);
  inherited Destroy;
end;

procedure TdxDBBreadcrumbEditDataBinding.Assign(Source: TPersistent);
begin
  if Source is TdxDBBreadcrumbEditDataBinding then
  begin
    ImageIndexField := TdxDBBreadcrumbEditDataBinding(Source).ImageIndexField;
    KeyField := TdxDBBreadcrumbEditDataBinding(Source).KeyField;
    NameField := TdxDBBreadcrumbEditDataBinding(Source).NameField;
    ParentKeyField := TdxDBBreadcrumbEditDataBinding(Source).ParentKeyField;
  end;
end;

procedure TdxDBBreadcrumbEditDataBinding.BindFields;
begin
  FKeyField := GetFieldByName(KeyField);
  FNameField := GetFieldByName(NameField);
  FParentKeyField := GetFieldByName(ParentKeyField);
  FImageIndexField := GetFieldByName(ImageIndexField);
end;

procedure TdxDBBreadcrumbEditDataBinding.RecordChanged(AField: TField);
begin
  if Control <> nil then
    Control.RecordChanged(AField);
end;

procedure TdxDBBreadcrumbEditDataBinding.SelectionChanged;
begin
  if Control <> nil then
    Control.SynchronizeSelection;
end;

procedure TdxDBBreadcrumbEditDataBinding.StructureChanged;
begin
  DataLink.DataBusy := True;
  try
    if Control <> nil then
      Control.StructureChanged;
  finally
    DataLink.DataBusy := False;
  end;
end;

function TdxDBBreadcrumbEditDataBinding.CreateDataLink: TdxDBBreadcrumbEditDataLink;
begin
  Result := TdxDBBreadcrumbEditDataLink.Create(Self);
end;

procedure TdxDBBreadcrumbEditDataBinding.DoSetFieldValue(const AValue: string; var AFieldValue: string);
begin
  if AFieldValue <> AValue then
  begin
    AFieldValue := AValue;
    StructureChanged;
  end;
end;

function TdxDBBreadcrumbEditDataBinding.GetDataSet: TDataSet;
begin
  Result := DataLink.DataSet;
end;

function TdxDBBreadcrumbEditDataBinding.GetDataSource: TDataSource;
begin
  Result := DataLink.DataSource;
end;

function TdxDBBreadcrumbEditDataBinding.GetIsRequeredFieldsAssigned: Boolean;
begin
  Result := (FieldKey <> nil) and (FieldParentKey <> nil) and (FieldName <> nil);
end;

function TdxDBBreadcrumbEditDataBinding.GetFieldByName(const AFieldName: string): TField;
begin
  if DataSet <> nil then
    Result := DataSet.FindField(AFieldName)
  else
    Result := nil;
end;

procedure TdxDBBreadcrumbEditDataBinding.SetDataSource(AValue: TDataSource);
begin
  DataLink.DataSource := AValue;
end;

procedure TdxDBBreadcrumbEditDataBinding.SetImageIndexFieldName(const AValue: string);
begin
  DoSetFieldValue(AValue, FImageIndexFieldName);
end;

procedure TdxDBBreadcrumbEditDataBinding.SetKeyFieldName(const AValue: string);
begin
  DoSetFieldValue(AValue, FKeyFieldName);
end;

procedure TdxDBBreadcrumbEditDataBinding.SetNameFieldName(const AValue: string);
begin
  DoSetFieldValue(AValue, FNameFieldName);
end;

procedure TdxDBBreadcrumbEditDataBinding.SetParentKeyFieldName(const AValue: string);
begin
  DoSetFieldValue(AValue, FParentKeyFieldName);
end;

{ TdxDBBreadcrumbEditDataLoader }

constructor TdxDBBreadcrumbEditDataLoader.Create(AControl: IdxDBBreadCrumbEdit);
begin
  inherited Create;
  FControl := AControl;
end;

procedure TdxDBBreadcrumbEditDataLoader.BuildStructure(AList: TList);

  procedure BuildLevel(ARootNode: TdxDBBreadcrumbEditNode);
  var
    ANode: TdxDBBreadcrumbEditNode;
  begin
    while FindLoadedNode(AList, ARootNode.KeyValue, ANode) do
    begin
      AList.Remove(ANode);
      ANode.MoveTo(ARootNode, namAddChild);
      BuildLevel(ANode);
    end;
  end;

var
  ANode: TdxDBBreadcrumbEditNode;
begin
  AList.Sort(@cxCompareByParentKeyValue);
  if FindLoadedNode(AList, Control.GetRootValue, ANode) then
  begin
    RootNode.KeyValue := ANode.KeyValue;
    RootNode.ParentKeyValue := ANode.ParentKeyValue;
    RootNode.ImageIndex := ANode.ImageIndex;
    RootNode.Name := ANode.Name;
    BuildLevel(RootNode);
  end;
end;

function TdxDBBreadcrumbEditDataLoader.FindLoadedNode(
  AList: TList; const AParentKeyValue: Variant; out ANode: TdxDBBreadcrumbEditNode): Boolean;
var
  L, H, I, C: Integer;
begin
  L := 0;
  H := AList.Count - 1;
  Result := False;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := VarCompare(TdxDBBreadcrumbEditNode(AList[I]).ParentKeyValue, AParentKeyValue);
    if C < 0 then
      L := I + 1
    else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        L := I;
      end;
    end;
  end;

  if Result then
    ANode := TdxDBBreadcrumbEditNode(AList[L])
  else
    ANode := nil;
end;

procedure TdxDBBreadcrumbEditDataLoader.LoadAllRecords(AList: TList);
var
  ANode: TdxDBBreadcrumbEditNode;
begin
  DataSet.First;
  while not DataSet.Eof do
  begin
    ANode := RootNode.AddChild;
    LoadNodeData(ANode);
    AList.Add(ANode);
    DataSet.Next;
  end;
end;

procedure TdxDBBreadcrumbEditDataLoader.LoadNodeData(ANode: TdxDBBreadcrumbEditNode);
begin
  ANode.KeyValue := DataBinding.FieldKey.Value;
  ANode.ParentKeyValue := DataBinding.FieldParentKey.Value;
  ANode.ImageIndex := GetImageIndexAsInteger(DataBinding.FieldImageIndex);
  ANode.Name := GetNameAsString(DataBinding.FieldName);
end;

procedure TdxDBBreadcrumbEditDataLoader.RebuildStructure;
var
  ABookmark: TcxDataSetBookmark;
  AList: TcxObjectList;
begin
  RootNode.BeginUpdate;
  try
    RootNode.Clear;
    if (DataSet <> nil) and DataSet.Active and DataBinding.IsRequeredFieldsAssigned then
    begin
      DataSet.DisableControls;
      try
        ABookmark := cxDataSetCreateBookmark(DataSet);
        try
          AList := TcxObjectList.Create;
          try
            LoadAllRecords(AList);
            BuildStructure(AList);
          finally
            AList.Free;
          end;
        finally
          cxDataSetRestoreBookmark(DataSet, ABookmark);
        end;
      finally
        DataSet.EnableControls;
      end;
    end;
  finally
    RootNode.EndUpdate;
  end;
end;

function TdxDBBreadcrumbEditDataLoader.GetDataBinding: TdxDBBreadcrumbEditDataBinding;
begin
  Result := Control.GetDataBinding;
end;

function TdxDBBreadcrumbEditDataLoader.GetDataSet: TDataSet;
begin
  Result := DataBinding.DataSet;
end;

function TdxDBBreadcrumbEditDataLoader.GetImageIndexAsInteger(AField: TField): Integer;
begin
  Result := -1;
  if AField <> nil then
  try
    Result := AField.Value
  except
    on EVariantError do
      Result := -1;
  end;
end;

function TdxDBBreadcrumbEditDataLoader.GetNameAsString(AField: TField): string;
begin
  Result := '';
  if AField <> nil then
  try
    Result := AField.Value
  except
    on EVariantError do
      Result := '';
  end;
end;

function TdxDBBreadcrumbEditDataLoader.GetRootNode: TdxDBBreadcrumbEditNode;
begin
  Result := Control.GetRoot as TdxDBBreadcrumbEditNode;
end;

{ TdxDBBreadcrumbEditController }

function TdxDBBreadcrumbEditController.CanSelectNode(ANode: TdxBreadcrumbEditNode): Boolean;
begin
  Result := (ANode = nil) or inherited CanSelectNode(ANode);
end;

function TdxDBBreadcrumbEditController.FindNodeByKeyValue(
  const AKeyValue: Variant; out ANode: TdxDBBreadcrumbEditNode): Boolean;

  function InternalFindNode(AProcessingNode: TdxDBBreadcrumbEditNode): Boolean;
  var
    I: Integer;
  begin
    Result := VarEquals(AProcessingNode.KeyValue, AKeyValue);
    if Result then
    begin
      ANode := AProcessingNode;
      Exit;
    end;

    AProcessingNode.LoadChildren;
    for I := 0 to AProcessingNode.Count - 1 do
    begin
      Result := InternalFindNode(AProcessingNode[I]);
      if Result then Break;
    end;
  end;

begin
  Result := InternalFindNode(Root);
end;

function TdxDBBreadcrumbEditController.FindNodeByKeyValue(const AKeyValue: Variant): TdxDBBreadcrumbEditNode;
begin
  if not FindNodeByKeyValue(AKeyValue, Result) then
    Result := nil;
end;

procedure TdxDBBreadcrumbEditController.Locate(ANode: TdxDBBreadcrumbEditNode);
begin
  if (ANode <> nil) and (DataSet <> nil) and DataSet.Active then
  begin
    Include(FState, dxdbbecsLocating);
    DataBinding.DataLink.DataLocate := True;
    try
      DataSet.Locate(DataBinding.KeyField, ANode.KeyValue, []);
    finally
      Exclude(FState, dxdbbecsLocating);
      DataBinding.DataLink.DataLocate := False;
    end;
  end;
end;

function TdxDBBreadcrumbEditController.ProcessParentKeyValueChanges(const ANewParentKeyValue: Variant): Boolean;
var
  ANode: TdxDBBreadcrumbEditNode;
begin
  if FindNodeByKeyValue(ANewParentKeyValue, ANode) then
  begin
    Result := Selected <> nil;
    if Result then
    begin
      Selected.BeginUpdate;
      try
        Selected.MoveTo(ANode, namAddChild);
        Selected.Sort;
      finally
        Selected.EndUpdate;
      end;
    end;
  end
  else
    Result := Selected = nil;
end;

procedure TdxDBBreadcrumbEditController.SelectionChanged;
begin
  inherited SelectionChanged;
  if not (dxdbbecsSynchronizingSelection in FState) then
    Locate(Selected as TdxDBBreadcrumbEditNode);
end;

procedure TdxDBBreadcrumbEditController.SynchronizeSelection;
begin
  if not (dxdbbecsSynchronizingSelection in FState) then
  begin
    Include(FState, dxdbbecsSynchronizingSelection);
    try
      if DataBinding.IsRequeredFieldsAssigned then
        Selected := FindNodeByKeyValue(DataBinding.FieldKey.Value)
      else
        Selected := nil;
    finally
      Exclude(FState, dxdbbecsSynchronizingSelection);
    end;
  end;
end;

function TdxDBBreadcrumbEditController.GetControl: IdxDBBreadCrumbEdit;
begin
  Result := inherited Control as IdxDBBreadCrumbEdit;
end;

function TdxDBBreadcrumbEditController.GetDataBinding: TdxDBBreadcrumbEditDataBinding;
begin
  Result := Control.GetDataBinding;
end;

function TdxDBBreadcrumbEditController.GetDataSet: TDataSet;
begin
  Result := DataBinding.DataSet;
end;

function TdxDBBreadcrumbEditController.GetRoot: TdxDBBreadcrumbEditNode;
begin
  Result := TdxDBBreadcrumbEditNode(inherited Root);
end;

{ TdxDBBreadcrumbEditNode }

function TdxDBBreadcrumbEditNode.AddChild: TdxDBBreadcrumbEditNode;
begin
  Result := TdxDBBreadcrumbEditNode(inherited AddChild);
end;

procedure TdxDBBreadcrumbEditNode.Clear;
begin
  inherited Clear;
  KeyValue := Null;
  ParentKeyValue := Null;
  ImageIndex := -1;
  Name := '';
end;

function TdxDBBreadcrumbEditNode.GetItem(AIndex: Integer): TdxDBBreadcrumbEditNode;
begin
  Result := TdxDBBreadcrumbEditNode(inherited Items[AIndex]);
end;

function TdxDBBreadcrumbEditNode.GetParent: TdxDBBreadcrumbEditNode;
begin
  Result := TdxDBBreadcrumbEditNode(inherited Parent);
end;

{ TdxCustomDBBreadcrumbEdit }

constructor TdxCustomDBBreadcrumbEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLoader := CreateDataLoader;
  FDataBinding := CreateDataBinding;
  FRootValue := Integer(-1);
end;

destructor TdxCustomDBBreadcrumbEdit.Destroy;
begin
  FreeAndNil(FDataBinding);
  FreeAndNil(FDataLoader);
  inherited Destroy;
end;

function TdxCustomDBBreadcrumbEdit.CreateController: TdxBreadcrumbEditController;
begin
  Result := TdxDBBreadcrumbEditController.Create(ViewInfo);
end;

function TdxCustomDBBreadcrumbEdit.CreateDataBinding: TdxDBBreadcrumbEditDataBinding;
begin
  Result := TdxDBBreadcrumbEditDataBinding.Create(Self, Self);
end;

function TdxCustomDBBreadcrumbEdit.CreateDataLoader: TdxDBBreadcrumbEditDataLoader;
begin
  Result := TdxDBBreadcrumbEditDataLoader.Create(Self);
end;

function TdxCustomDBBreadcrumbEdit.CreateProperties: TdxCustomBreadcrumbEditProperties;
begin
  Result := TdxDBBreadcrumbEditProperties.Create(Self);
end;

function TdxCustomDBBreadcrumbEdit.CreateRoot: TdxBreadcrumbEditNode;
begin
  Result := TdxDBBreadcrumbEditNode.Create(Self);
end;

procedure TdxCustomDBBreadcrumbEdit.RecordChanged(AField: TField);
begin
  if (AField <> nil) and not (dxdbbecsUpdatingRecord in Controller.FState) then
  begin
    BeginUpdate;
    try
      Include(Controller.FState, dxdbbecsUpdatingRecord);
      try
        if Selected <> nil then
          DataLoader.LoadNodeData(Selected);
        if AField = DataBinding.FieldParentKey then
        begin
          if not Controller.ProcessParentKeyValueChanges(AField.Value) then
            StructureChanged;
        end;
      finally
        Exclude(Controller.FState, dxdbbecsUpdatingRecord);
      end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TdxCustomDBBreadcrumbEdit.SynchronizeSelection;
begin
  Controller.SynchronizeSelection;
end;

procedure TdxCustomDBBreadcrumbEdit.StructureChanged;
begin
  if not (dxdbbecsLocating in Controller.State) then
  begin
    BeginUpdate;
    try
      DataBinding.BindFields;
      DataLoader.RebuildStructure;
      SynchronizeSelection;
    finally
      EndUpdate;
    end;
  end;
end;

function TdxCustomDBBreadcrumbEdit.GetDBController: TdxDBBreadcrumbEditController;
begin
  Result := TdxDBBreadcrumbEditController(inherited Controller);
end;

function TdxCustomDBBreadcrumbEdit.GetDataBinding: TdxDBBreadcrumbEditDataBinding;
begin
  Result := FDataBinding;
end;

function TdxCustomDBBreadcrumbEdit.GetRootValue: Variant;
begin
  Result := FRootValue;
end;

function TdxCustomDBBreadcrumbEdit.GetDBProperties: TdxDBBreadcrumbEditProperties;
begin
  Result := TdxDBBreadcrumbEditProperties(inherited Properties);
end;

function TdxCustomDBBreadcrumbEdit.GetDBRoot: TdxDBBreadcrumbEditNode;
begin
  Result := TdxDBBreadcrumbEditNode(inherited Root);
end;

function TdxCustomDBBreadcrumbEdit.GetSelected: TdxDBBreadcrumbEditNode;
begin
  Result := TdxDBBreadcrumbEditNode(inherited Selected);
end;

procedure TdxCustomDBBreadcrumbEdit.SetDataBinding(AValue: TdxDBBreadcrumbEditDataBinding);
begin
  FDataBinding.Assign(AValue);
end;

procedure TdxCustomDBBreadcrumbEdit.SetDBProperties(AValue: TdxDBBreadcrumbEditProperties);
begin
  inherited Properties := AValue;
end;

procedure TdxCustomDBBreadcrumbEdit.SetRootValue(const AValue: Variant);
begin
  if not cxEditVarEquals(AValue, FRootValue) then
  begin
    FRootValue := AValue;
    StructureChanged;
  end;
end;

procedure TdxCustomDBBreadcrumbEdit.SetSelected(AValue: TdxDBBreadcrumbEditNode);
begin
  inherited Selected := AValue;
end;

end.
