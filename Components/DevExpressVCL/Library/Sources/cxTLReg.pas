{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressQuantumTreeList                                   }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSQUANTUMTREELIST AND ALL        }
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

unit cxTLReg;

{$I cxVer.inc}

interface
uses
  Classes, SysUtils, TypInfo, Types, DesignIntf, DesignEditors, VCLEditors, dxCore,
  cxInplaceContainer, Forms, DB, cxDesignWindows, cxPropEditors, cxClasses, cxControls,
  cxEdit, cxStyles, cxTL, cxTLData, cxDBTL, cxTLStrs, cxDataControllerConditionalFormatting,
  cxTLEditor, cxTLItemsEditor, cxTLPredefinedStyles, cxTLStyleSheetPreview;

const
  cxTLProductName = 'ExpressQuantumTreeList';

procedure Register;

implementation

uses
  dxCoreReg, cxLibraryReg, cxEditPropEditors, dxCoreClasses;

type
  { TcxTreeListComponentEditor }

  TcxTreeListComponentEditor = class(TdxComponentEditor)
  protected
    FItems: TStringList;
    function GetTreeList: TcxCustomTreeList;
    procedure ItemsNeeded; virtual;
  protected
    function GetDataSet: TDataSet;
    function GetDataSource: TDataSource; virtual;
    function GetProductName: string; override;
    function InternalGetVerb(AIndex: Integer): string; override;
    function InternalGetVerbCount: Integer; override;
    procedure InternalExecuteVerb(AIndex: Integer); override;
  public
    destructor Destroy; override;

    property TreeList: TcxCustomTreeList read GetTreeList;
  end;

  { TcxDBTreeListComponentEditor }

  TcxDBTreeListComponentEditor = class(TcxTreeListComponentEditor)
  protected
    procedure DoLinkTo(AObject: TObject); override;
    function GetDataSource: TDataSource; override;
    function GetLinkToItemCaption: string; override;
    function GetLinkToTypeClass: TClass; override;
    function IsLinkable: Boolean; override;
    procedure PrepareItem(Index: Integer; const AItem: TDesignMenuItem); override;

    procedure PrepareLinkableSubItem(const AItem: TDesignMenuItem); override;
  end;

  { TcxstStylesEventsProperty }

  TcxTreeListStylesEventsProperty = class(TcxNestedEventProperty)
  protected
    function GetInstance: TPersistent; override;
  end;

  { TcxTreeListPopupMenusEventsProperty }

  TcxTreeListPopupMenusEventsProperty = class(TcxNestedEventProperty)
  protected
    function GetInstance: TPersistent; override;
  end;

  TcxTreeListNavigatorEventsProperty = class(TcxNestedEventProperty)
  protected
    function GetInstance: TPersistent; override;
  end;

  { TcxDBTreeListDataBindingFieldNameProperty }

  TcxDBTreeListDataBindingFieldNameProperty = class(TFieldNameProperty)
  public
    function GetDataSource: TDataSource; override;
  end;

  { TcxDBTreeListDataControllerFieldNameProperty }

  TcxDBTreeListDataControllerFieldNameProperty = class(TFieldNameProperty)
  public
    function GetDataSource: TDataSource; override;
  end;

  { TcxTreeListColumnProperty }

  TcxTreeListColumnProperty = class(TComponentProperty)
  public
    procedure GetValues(Proc: TGetStrProc); override;
  end;

procedure ShowItemsDesigner(AEditor: TcxTreeListComponentEditor; APageIndex: Integer);
var
  ADesigner: TcxTreeListBandColumnDesigner;
begin
  ADesigner := TcxTreeListBandColumnDesigner(ShowFormEditorClass(AEditor.Designer,
    AEditor.Component, TcxTreeListBandColumnDesigner));
  ADesigner.SetVisiblePageIndex(APageIndex);
end;

{ TcxTreeListComponentEditor }

destructor TcxTreeListComponentEditor.Destroy;
begin
  FreeAndNil(FItems);
  inherited Destroy;
end;

function TcxTreeListComponentEditor.GetTreeList: TcxCustomTreeList;
begin
  Result := Component as TcxCustomTreeList;
end;

{procedure ShowM(Msg: string);
var
  F: TForm;
begin
  F := TForm.CreateNew(nil);
  F.Caption := Msg;
  F.Show;
end;
}

procedure TcxTreeListComponentEditor.ItemsNeeded;
var
  AOperations: IcxTreeListDesignTimeOperations;
begin
  if FItems = nil then
    FItems := TStringList.Create;
  FItems.Clear;

  if Supports(TreeList, IcxTreeListDesignTimeOperations, AOperations) then
  begin
    if AOperations.SupportBandColumnEditor then
    begin
      FItems.AddObject(scxStr(@scxColumns), TObject(0));
      FItems.AddObject(scxStr(@scxBands), TObject(1));
    end;
    if AOperations.SupportItemsEditor then
      FItems.AddObject(scxStr(@scxItems), TObject(2))
    else
      if AOperations.SupportCreateAllItems and (not IsLinkable or (GetDataSet <> nil)) then
      begin
        FItems.AddObject('-', TObject(-1));
        FItems.AddObject(scxStr(@scxCreateAllItems), TObject(3));
        if (GetDataSet <> nil) and not AOperations.HasAllItems then
          FItems.AddObject(scxStr(@scxCreateMissingItems), TObject(6));
      end;

    if TreeList.ColumnCount > 0 then
    begin
      if not AOperations.SupportCreateAllItems then
        FItems.AddObject('-', TObject(-1));
      FItems.AddObject(scxStr(@scxDeleteAllItems), TObject(4));
    end;

    if (TreeList.ColumnCount > 0) and TreeList.ConditionalFormatting.CanShowRulesManagerDialog then
    begin
      FItems.AddObject('-', TObject(-1));
      FItems.AddObject(scxStr(@scxConditionalFormatting), TObject(5));
    end;
  end;

  if IsLinkable then
    FItems.AddObject('-', TObject(-1));
end;

function TcxTreeListComponentEditor.GetDataSet: TDataSet;
begin
  if GetDataSource <> nil then
    Result := GetDataSource.DataSet
  else
    Result := nil;
end;

function TcxTreeListComponentEditor.GetDataSource: TDataSource;
begin
  Result := nil;
end;

function TcxTreeListComponentEditor.GetProductName: string;
begin
  Result := cxTLProductName;
end;

function TcxTreeListComponentEditor.InternalGetVerb(AIndex: Integer): string;
begin
  Result := FItems[AIndex]
end;

function TcxTreeListComponentEditor.InternalGetVerbCount: Integer;
begin
  ItemsNeeded;
  Result := FItems.Count;
end;

procedure TcxTreeListComponentEditor.InternalExecuteVerb(AIndex: Integer);
const
  Invert: array[Boolean] of Byte = (1, 0);
var
  ADesignerModifiedNeeded: Boolean;
  AOperations: IcxTreeListDesignTimeOperations;
begin
  if Supports(TreeList, IcxTreeListDesignTimeOperations, AOperations) then
  begin
    ADesignerModifiedNeeded := False;
    case Integer(FItems.Objects[AIndex]) of
      0, 1:
        begin
          if AOperations.SupportBandColumnEditor then
            ShowItemsDesigner(Self, Invert[AIndex = 1]);
        end;
      2:
        ADesignerModifiedNeeded := cxShowTreeListItemsEditor(TreeList);
      3, 6:
        begin
          AOperations.CreateAllItems(Integer(FItems.Objects[AIndex]) = 6);
          ADesignerModifiedNeeded := True;
        end;
      4:
        begin
          TreeList.DeleteAllColumns;
          ADesignerModifiedNeeded := True;
        end;
      5:
        TreeList.ConditionalFormatting.ShowRulesManagerDialog;
    end;
    if ADesignerModifiedNeeded then
      Designer.Modified;
  end;
end;

{ TcxDBTreeListComponentEditor }

procedure TcxDBTreeListComponentEditor.DoLinkTo(AObject: TObject);
var
  ADataController: TcxDBTreeListDataController;
  ATreeList: TcxDBTreeList;
begin
  ATreeList := (Component as TcxDBTreeList);
  ADataController := ATreeList.DataController;
  if ADataController.DataSource <> AObject then
  begin
    ATreeList.DeleteAllColumns;
    ADataController.DataSource := (AObject as TDataSource);
    ADataController.CreateAllItems;
    Designer.Modified;
  end;
end;

function TcxDBTreeListComponentEditor.GetDataSource: TDataSource;
begin
  Result := TcxDBTreeList(TreeList).DataController.DataSource;
end;

function TcxDBTreeListComponentEditor.GetLinkToItemCaption: string;
begin
  Result := 'Link to DataSource';
end;

function TcxDBTreeListComponentEditor.GetLinkToTypeClass: TClass;
begin
  Result := TDataSource;
end;

function TcxDBTreeListComponentEditor.IsLinkable: Boolean;
begin
  Result := True;
end;

procedure TcxDBTreeListComponentEditor.PrepareItem(Index: Integer; const AItem: TDesignMenuItem);
var
  AOperations: IcxTreeListDesignTimeOperations;
begin
  if Supports(TreeList, IcxTreeListDesignTimeOperations, AOperations) and (AItem.Caption = scxStr(@scxCreateAllItems)) then
  begin
    AItem.Enabled := GetDataSet <> nil;
    if not AItem.Enabled then
      AItem.Caption := '(no data linked)';
  end;
  inherited PrepareItem(Index, AItem);
end;

procedure TcxDBTreeListComponentEditor.PrepareLinkableSubItem(const AItem: TDesignMenuItem);
begin
  AItem.Checked := (Component as TcxDBTreeList).DataController.DataSource = Designer.GetComponent(AItem.Caption);
end;

{ TcxTreeListStylesEventsProperty }

function TcxTreeListStylesEventsProperty.GetInstance: TPersistent;
begin
  Result := TcxCustomTreeList(GetComponent(0)).Styles;
end;

{ TcxTreeListPopupMenusEventsProperty }

function TcxTreeListPopupMenusEventsProperty.GetInstance: TPersistent;
begin
  Result := TcxCustomTreeList(GetComponent(0)).PopupMenus;
end;

{ TcxTreeListNavigatorEventsProperty }

function TcxTreeListNavigatorEventsProperty.GetInstance: TPersistent;
begin
  Result := TcxCustomTreeList(GetComponent(0)).Navigator;
end;

{ TcxDBTreeListDataBindingFieldNameProperty }

function TcxDBTreeListDataBindingFieldNameProperty.GetDataSource: TDataSource;
begin
  Result := TcxDBItemDataBinding(GetComponent(0)).DataController.DataSource
end;

{ TcxDBTreeListDataControllerFieldNameProperty }

function TcxDBTreeListDataControllerFieldNameProperty.GetDataSource: TDataSource;
begin
  Result := TcxDBTreeListDataController(GetComponent(0)).DataSource;
end;

{ TcxTreeListColumnProperty }

type
  TPersistentAccess = class(TPersistent);

procedure TcxTreeListColumnProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
  ATreeList: TcxCustomTreeList;
begin
  ATreeList := TPersistentAccess(GetComponent(0)).GetOwner as TcxCustomTreeList;
  for I := 0 to ATreeList.ColumnCount - 1 do
    Proc(ATreeList.Columns[I].Name)
end;

type
  TcxTreeListSelectionEditor = class(TSelectionEditor)
  protected
    ComponentsList: TStringList;
  public
    procedure AddComponent(const Name: string);
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

procedure TcxTreeListSelectionEditor.AddComponent(const Name: string);
begin
  ComponentsList.Add(Name);
end;

procedure TcxTreeListSelectionEditor.RequiresUnits(Proc: TGetStrProc);

  procedure AddColumnUnitName(AProperties: TcxCustomEditProperties);
  begin
    if AProperties <> nil then
      Proc(cxGetUnitName(AProperties.ClassType));
  end;

var
  AComponent: TComponent;
  I: Integer;
begin
  inherited RequiresUnits(Proc);
  Proc('cxGraphics');
  Proc('cxCustomData');
  Proc('cxStyles');
  Proc('cxTL');
  ComponentsList := TStringList.Create;
  try
    Designer.GetComponentNames(GetTypeData(PTypeInfo(TcxTreeListColumn.ClassInfo)), AddComponent);
    for I := 0 to ComponentsList.Count - 1 do
    begin
      AComponent := Designer.GetComponent(ComponentsList[I]);
      if AComponent is TcxTreeListColumn then
      begin
        AddColumnUnitName(TcxTreeListColumn(AComponent).Properties);
        AddColumnUnitName(TcxTreeListColumn(AComponent).PropertiesValue);
      end;
    end;
  finally
    ComponentsList.Free;
  end;
  Proc(cxGetUnitName(cxTreeListBuiltInMenuClass));
  TcxDataControllerConditionalFormattingRulesManagerDialogProvider.RequiresRulesManagerDialogUnits(Designer.Root, Proc);
end;

type
  TcxDesignSelectionListener = class(TcxIUnknownObject, IDesignNotification)
  protected
    Listeners: TList;
    // IDesignNotification
    procedure ItemDeleted(const ADesigner: IDesigner; AItem: TPersistent);
    procedure ItemInserted(const ADesigner: IDesigner; AItem: TPersistent);
    procedure ItemsModified(const ADesigner: IDesigner);
    procedure SelectionChanged(const ADesigner: IDesigner; const ASelection: IDesignerSelections);
    procedure DesignerOpened(const ADesigner: IDesigner; AResurrecting: Boolean);
    procedure DesignerClosed(const ADesigner: IDesigner; AGoingDormant: Boolean);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure AddListener(AListener: TObject; AddListener: Boolean);
  end;

constructor TcxDesignSelectionListener.Create;
begin
  Listeners := TList.Create;
  RegisterDesignNotification(Self);
  DesignerNavigatorProc := AddListener;
end;

destructor TcxDesignSelectionListener.Destroy;
begin
  DesignerNavigatorProc := nil;
  Listeners.Clear;
  UnRegisterDesignNotification(Self);
  FreeAndNil(Listeners);
  inherited Destroy;
end;

procedure TcxDesignSelectionListener.AddListener(
  AListener: TObject; AddListener: Boolean);
begin
  Listeners.Remove(AListener);
  if AddListener then
    Listeners.Add(AListener);
end;

procedure TcxDesignSelectionListener.ItemDeleted(
  const ADesigner: IDesigner; AItem: TPersistent);
begin
end;

procedure TcxDesignSelectionListener.ItemInserted(
  const ADesigner: IDesigner; AItem: TPersistent);
begin
end;

procedure TcxDesignSelectionListener.ItemsModified(
  const ADesigner: IDesigner);
begin
end;

procedure TcxDesignSelectionListener.SelectionChanged(
  const ADesigner: IDesigner; const ASelection: IDesignerSelections);
var
  I: Integer;
begin
  for I := 0 to Listeners.Count - 1 do
    TcxCustomTreeList(Listeners[I]).Invalidate;
end;

procedure TcxDesignSelectionListener.DesignerOpened(
  const ADesigner: IDesigner; AResurrecting: Boolean);
begin
end;

procedure TcxDesignSelectionListener.DesignerClosed(
  const ADesigner: IDesigner; AGoingDormant: Boolean);
begin
end;

procedure Register;
begin
  ForceDemandLoadState(dlDisable);

  RegisterComponents(dxCoreLibraryProductPage, [TcxTreeList, TcxDBTreeList, TcxVirtualTreeList]);
  RegisterClasses([TcxTreeListColumn, TcxDBTreeListColumn, TcxTreeListBands, TcxTreeListBand, TcxTreeListStyleSheet]);
  RegisterNoIcon([TcxTreeListColumn, TcxDBTreeListColumn, TcxTreeListStyleSheet]);
  RegisterComponentEditor(TcxTreeList, TcxTreeListComponentEditor);
  RegisterComponentEditor(TcxVirtualTreeList, TcxTreeListComponentEditor);
  RegisterComponentEditor(TcxDBTreeList, TcxDBTreeListComponentEditor);
  RegisterPropertyEditor(TypeInfo(TcxTreeListColumn), TcxTreeListPreview,
    'Column', TcxTreeListColumnProperty);
  RegisterPropertyEditor(TypeInfo(TcxTreeListColumn), TcxTreeListOptionsView,
    'CategorizedColumn', TcxTreeListColumnProperty);
  RegisterPropertyEditor(TypeInfo(TNotifyEvent), TcxCustomTreeList,
    'StylesEvents', TcxTreeListStylesEventsProperty);
  RegisterPropertyEditor(TypeInfo(TNotifyEvent), TcxCustomTreeList,
    'PopupMenusEvents', TcxTreeListPopupMenusEventsProperty);
  RegisterPropertyEditor(TypeInfo(TComponent),
    TcxTreeListPopupMenu, 'PopupMenu', TcxControlPopupMenuProperty);
  RegisterPropertyEditor(TypeInfo(TNotifyEvent), TcxCustomTreeList,
    'NavigatorEvents', TcxTreeListNavigatorEventsProperty);

  RegisterPropertyEditor(TypeInfo(string), TcxDBItemDataBinding,
    'FieldName', TcxDBTreeListDataBindingFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TcxDBTreeListDataController,
    'KeyField', TcxDBTreeListDataControllerFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TcxDBTreeListDataController,
    'ParentField', TcxDBTreeListDataControllerFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TcxDBTreeListDataController,
    'ImageIndexField', TcxDBTreeListDataControllerFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TcxDBTreeListDataController,
    'StateIndexField', TcxDBTreeListDataControllerFieldNameProperty);
  RegisterSelectionEditor(TcxCustomTreeList, TcxTreeListSelectionEditor);
  HideClassProperties(TcxTreeListColumn, ['SummaryFooter']);
  RegisterPropertyEditor(TypeInfo(TdxDefaultBoolean), TcxTreeListOptionsData, 'MultiThreadedSorting', TdxDataControllerMultithreadedSortingPropertyEditor);
end;

var
  DesignSelectionListener: TcxDesignSelectionListener;

initialization
  DesignSelectionListener := TcxDesignSelectionListener.Create;
  RegisterStyleSheetClass(TcxTreeListStyleSheet);

finalization
  UnRegisterStyleSheetClass(TcxTreeListStyleSheet);
  DesignSelectionListener.Free;

end.
