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

unit cxViewEditor;

{$I cxVer.inc}

interface

uses
  DesignIntf, DesignMenus, Variants,
{$IFNDEF NONDB}
  DB, dxServerModeData,
{$ENDIF}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Menus,
  Dialogs, ExtCtrls, dxCoreReg, cxLibraryReg, cxClasses, cxStorage,
  cxDesignWindows, cxCustomData, cxGridCommon, cxGridCustomView, dxCoreClasses, cxGrid, cxGraphics, cxControls,
  cxLookAndFeels, cxLookAndFeelPainters, dxLayoutContainer, dxLayoutControl, dxLayoutLookAndFeels, dxForms;

const
  cxGridViewMenuSeparatorCaption = '-';

type

  { IcxGridWizard }

  IcxGridWizard = interface
  ['{8165A199-CF83-44FA-9CC0-57DC62EBE3BA}']
    procedure Execute(AGrid: TcxCustomGrid; AGridView: TcxCustomGridView = nil);
    function IsGridViewSupported(AGridViewClass: TClass): Boolean;
  end;

  { TcxViewEditor }

  TcxViewEditor = class(TdxForm)
    PViewEditor: TPanel;
    lcMainGroup_Root: TdxLayoutGroup;
    lcMain: TdxLayoutControl;
    dxLayoutLookAndFeelList: TdxLayoutLookAndFeelList;
    dxLayoutCxLookAndFeel: TdxLayoutCxLookAndFeel;
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  private
    FView: TcxCustomGridView;
    function GetDataController: TcxCustomDataController;
    function GetOwnerForm: TComponent;
  protected
    procedure BeginUpdate;
    function CanAddComponent: Boolean;
    function CanDeleteComponent(AComponent: TComponent): Boolean;
    procedure EndUpdate;
    procedure GetSelectionList(AList: TList);
    procedure SelectComponent(AComponent: TPersistent);
    function UniqueName(AComponent: TComponent;
      const ATruncateClassName: string = ScxGridPrefixName): string; virtual;
    procedure UpdateDesigner; virtual;
    procedure UpdateEditor; virtual;
    procedure UpdateSelection; virtual;
    property DataController: TcxCustomDataController read GetDataController;
  public
    FormEditor: TcxDesignFormEditor;
    class function GetViewByObject(APersistent: TPersistent): TcxCustomGridView; virtual;
    procedure SetView(Value: TcxCustomGridView; ARefreshNeeded: Boolean);
    property OwnerForm: TComponent read GetOwnerForm;
    property View: TcxCustomGridView read FView;
  end;

  TcxViewEditorClass = class of TcxViewEditor;

  { TcxCustomGridViewStorage }

  TcxCustomGridViewStorage = class(TcxInterfacedPersistent, IcxStoredObject)
  private
    FView: TcxCustomGridView;
  protected
    // IcxStoredObject
    function GetObjectName: string;
    function GetProperties(AProperties: TStrings): Boolean;
    procedure GetPropertyValue(const AName: string; var AValue: Variant);
    procedure SetPropertyValue(const AName: string; const AValue: Variant);
    property View: TcxCustomGridView read FView;
  public
    constructor Create(AView: TcxCustomGridView); reintroduce; virtual;
    class function GetViewClass: TcxCustomGridViewClass; virtual;
  end;

  TcxCustomGridViewStorageClass = class of TcxCustomGridViewStorage;

  { View Menu Provider }

  TcxGridViewMenuItem = class;

  TcxGridViewMenuItemActionEvent = procedure(Sender: TcxGridViewMenuItem) of object;

  TcxGridViewMenuItems = class
  private
    FItems: TList;
    function GetCount: Integer;
    function GetItem(Index: Integer): TcxGridViewMenuItem;
  protected
    procedure ClearItems;
  public
    constructor Create;
    destructor Destroy; override;
    function AddItem(const ACaption: string; AOnClick: TcxGridViewMenuItemActionEvent = nil;
      AEnabled: Boolean = True; AChecked: Boolean = False; AData: TObject = nil): TcxGridViewMenuItem;
    function AddSeparator: TcxGridViewMenuItem;
    procedure Delete(AIndex: Integer);
    procedure Prepare(AMenuItem: TMenuItem); overload; virtual;
    procedure Prepare(const AMenuItem: TDesignMenuItem); overload; virtual;
    procedure RemoveExcessSeparators;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TcxGridViewMenuItem read GetItem; default;
  end;

  TcxGridViewMenuItem = class(TcxGridViewMenuItems)
  private
    FDesignItem: TDesignMenuItem;
    procedure MenuItemClick(Sender: TObject);
  protected
    OnAction: TcxGridViewMenuItemActionEvent;
  public
    Caption: string;
    Checked: Boolean;
    Data: TObject;
    Enabled: Boolean;
    constructor Create(const ACaption: string; AEnabled: Boolean;
      AChecked: Boolean; AData: TObject; AOnAction: TcxGridViewMenuItemActionEvent);
    destructor Destroy; override;
    procedure DoAction;
    function IsSeparator: Boolean;
    procedure Prepare(AMenuItem: TMenuItem); overload; override;
    procedure Prepare(const AMenuItem: TDesignMenuItem); overload; override;
  end;

  TcxCustomGridViewMenuProviderClass = class of TcxCustomGridViewMenuProvider;

  TcxCustomGridViewMenuProvider = class
  private
    FDesigner: IDesigner;
    FDesignHelper: TcxDesignHelper;
    FGridView: TcxCustomGridView;
    FItems: TcxGridViewMenuItems;
  {$IFNDEF NONDB}
    FDBDataSourceNames: TStringList;
    FServerModeDataSourceNames: TStringList;
    procedure GetDBDataSourceName(const S: string);
    function GetDBViewDataSource: TDataSource;
    function GetDBGridView: TcxCustomGridView;
    procedure SetDBViewDataSource(Value: TDataSource);
    procedure GetServerModeDataSourceName(const S: string);
    function GetServerModeViewDataSource: TdxServerModeCustomDataSource;
    function GetServerModeGridView: TcxCustomGridView;
    procedure SetServerModeViewDataSource(Value: TdxServerModeCustomDataSource);
  {$ENDIF}
  protected
    procedure DesignerModified;
    procedure ObjectCreated(AObject: TPersistent);
    procedure SelectObject(AObject: TPersistent);

    procedure InitAdditionalItems; virtual;
    procedure InitCopySettingsFromViewItem; virtual;
  {$IFNDEF NONDB}
    procedure InitDataBindingItems; virtual;
    procedure InitDBDataBindingItems; virtual;
    procedure InitServerModeDataBindingItems; virtual;
  {$ENDIF}
    procedure InitItems; virtual;
    procedure InitLayoutItems; virtual;
    procedure InitStructureItems; virtual;

    procedure CopySettings(Sender: TcxGridViewMenuItem);
    procedure DeleteView(Sender: TcxGridViewMenuItem);
    procedure EditConditionalFormatting(Sender: TcxGridViewMenuItem);
    procedure EditLayoutAndData(Sender: TcxGridViewMenuItem);
    procedure EditByWizard(Sender: TcxGridViewMenuItem);
    procedure SetAsDefault(Sender: TcxGridViewMenuItem);
  {$IFNDEF NONDB}
    function GetDBDataSourceNames: TStringList;
    procedure LinkToDBDataSource(Sender: TcxGridViewMenuItem);
    function GetServerModeDataSourceNames: TStringList;
    procedure LinkToServerModeDataSource(Sender: TcxGridViewMenuItem);
  {$ENDIF}

  {$IFNDEF NONDB}
    property DBViewDataSource: TDataSource read GetDBViewDataSource write SetDBViewDataSource;
    property DBGridView: TcxCustomGridView read GetDBGridView;
    property ServerModeViewDataSource: TdxServerModeCustomDataSource read GetServerModeViewDataSource write SetServerModeViewDataSource;
    property ServerModeGridView: TcxCustomGridView read GetServerModeGridView;
  {$ENDIF}
    property Designer: IDesigner read FDesigner;
    property DesignHelper: TcxDesignHelper read FDesignHelper;
  public
    constructor Create(AGridView: TcxCustomGridView); virtual;
    destructor Destroy; override;
    property GridView: TcxCustomGridView read FGridView;
    property Items: TcxGridViewMenuItems read FItems;
  end;

  { TcxGridViewList }

  TcxGridViewList = class
  private
    FDesigner: IDesigner;
    FViewNames: TStringList;
    function GetView(Index: Integer): TcxCustomGridView;
    procedure AddViewName(const S: string);
  protected
    procedure GetViewNames; virtual;
  public
    constructor Create(ADesigner: IDesigner);
    destructor Destroy; override;
    function GetViewIndex(AView: TcxCustomGridView): Integer;
    property Designer: IDesigner read FDesigner;
    property ViewNames: TStringList read FViewNames;
    property Views[Index: Integer]: TcxCustomGridView read GetView;
  end;

var
  FGridWizard: IcxGridWizard = nil;

function GetViewEditorClass(AViewClass: TcxCustomGridViewClass): TcxViewEditorClass;
procedure RegisterViewEditorClass(AViewClass: TcxCustomGridViewClass; AViewEditorClass: TcxViewEditorClass);
procedure UnregisterViewEditorClass(AViewClass: TcxCustomGridViewClass; AViewEditorClass: TcxViewEditorClass);
function GetLinkedView(APersistent: TPersistent): TcxCustomGridView;
function IsViewLinkedToComponent(AView: TcxCustomGridView; AComponent: TComponent): Boolean;

function GetViewTemplateRegKey: string;
procedure RegisterDefaultViewStorage(AStorageClass: TcxCustomGridViewStorageClass);
procedure UnregisterDefaultViewStorage(AStorageClass: TcxCustomGridViewStorageClass);
function GetDefaultViewStorageByViewClass(AGridView: TcxCustomGridView): TcxCustomGridViewStorage;

procedure RestoreViewFromTemplate(const ARegPath: string; AView: TcxCustomGridView);
procedure SaveViewToTemplate(const ARegPath: string; AView: TcxCustomGridView);

function CreateViewMenuProvider(AView: TcxCustomGridView): TcxCustomGridViewMenuProvider;
function GetViewMenuProviderClass(AView: TcxCustomGridView): TcxCustomGridViewMenuProviderClass;
procedure RegisterViewMenuProviderClass(AViewClass: TcxCustomGridViewClass;
  AViewMenuProviderClass: TcxCustomGridViewMenuProviderClass);
procedure UnregisterViewMenuProviderClass(AViewClass: TcxCustomGridViewClass;
  AViewMenuProviderClass: TcxCustomGridViewMenuProviderClass);
implementation

{$R *.dfm}

uses
{$IFNDEF NONDB}
  TypInfo, cxDBData,
{$ENDIF}
  cxDataControllerConditionalFormatting,
  cxGridLevel,
  cxGridViewLayoutEditor;

type
  TcxDesignWindowAccess = class(TcxDesignWindow);

var
  ViewEditorClassList: TcxRegisteredClassList;
  DefaultViewStorages: TList;
  ViewMenuProviderClassList: TcxRegisteredClassList;

function GetViewEditorClass(AViewClass: TcxCustomGridViewClass): TcxViewEditorClass;
begin
  Result := TcxViewEditorClass(ViewEditorClassList.FindClass(AViewClass));
end;

procedure RegisterViewEditorClass(AViewClass: TcxCustomGridViewClass; AViewEditorClass: TcxViewEditorClass);
begin
  ViewEditorClassList.Register(AViewClass, AViewEditorClass);
end;

procedure UnregisterViewEditorClass(AViewClass: TcxCustomGridViewClass; AViewEditorClass: TcxViewEditorClass);
begin
  ViewEditorClassList.Unregister(AViewClass, AViewEditorClass);
end;

function GetLinkedView(APersistent: TPersistent): TcxCustomGridView;
var
  I: Integer;
  AViewEditorClass: TcxViewEditorClass;
begin
  Result := nil;
  for I := 0 to ViewEditorClassList.Count - 1 do
  begin
    AViewEditorClass := TcxViewEditorClass(ViewEditorClassList[I].RegisteredClass);
    Result := AViewEditorClass.GetViewByObject(APersistent);
    if Result <> nil then
      Break;
  end;
end;

function IsViewLinkedToComponent(AView: TcxCustomGridView; AComponent: TComponent): Boolean;

  function CheckLevels(ALevel: TcxGridLevel): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    for I := 0 to ALevel.Count - 1 do
      if (ALevel[I].GridView = AView) or CheckLevels(ALevel[I]) then
      begin
        Result := True;
        Break;
      end;
  end;

begin
  Result := AView.GetParentComponent = AComponent;
  if not Result and (AComponent is TcxCustomGrid) then // check Levels (from Repository)
    Result := CheckLevels(TcxCustomGrid(AComponent).Levels);
end;

function GetViewTemplateRegKey: string;
begin
  Result := TcxDesignWindow.GetBaseRegKey + '\TcxGridViewTemplate';
end;

procedure RegisterDefaultViewStorage(AStorageClass: TcxCustomGridViewStorageClass);
begin
  if DefaultViewStorages = nil then
    DefaultViewStorages := TList.Create;
  if DefaultViewStorages.IndexOf((AStorageClass)) < 0 then
    DefaultViewStorages.Add((AStorageClass));
end;

procedure UnregisterDefaultViewStorage(AStorageClass: TcxCustomGridViewStorageClass);
begin
  if DefaultViewStorages <> nil then
    DefaultViewStorages.Remove((AStorageClass));
end;

function GetDefaultViewStorageByViewClass(AGridView: TcxCustomGridView): TcxCustomGridViewStorage;
var
  I: Integer;
  ACurrent, AStorageClass: TcxCustomGridViewStorageClass;
begin
  Result := nil;
  AStorageClass := nil;
  if (DefaultViewStorages <> nil) and (AGridView <> nil) then
  begin
    for I := 0 to DefaultViewStorages.Count - 1 do
    begin
      ACurrent := TcxCustomGridViewStorageClass(DefaultViewStorages[I]);

      if AGridView.InheritsFrom(ACurrent.GetViewClass) then
        if (AStorageClass = nil) or
          not AStorageClass.GetViewClass.InheritsFrom(ACurrent.GetViewClass) then
          AStorageClass := ACurrent;

      if (AStorageClass <> nil) and
        (AStorageClass.GetViewClass = AGridView.ClassType) then
        Break;
    end;
  end;
  if AStorageClass <> nil then
    Result := AStorageClass.Create(AGridView);
end;

procedure RestoreViewFromTemplate(const ARegPath: string; AView: TcxCustomGridView);
var
  AStorage: TcxStorage;
  AViewStorage: TcxCustomGridViewStorage;
begin
  AViewStorage := GetDefaultViewStorageByViewClass(AView);
  if AViewStorage = nil then Exit;
  try
    AStorage := TcxStorage.Create(ARegPath{ + '\' + AViewStorage.GetObjectName});
    try
      AStorage.Modes := AStorage.Modes + [smSavePublishedClassProperties];
      AStorage.RestoreFromRegistry(AViewStorage);
    finally
      AStorage.Free;
    end;
  finally
    AViewStorage.Free;
  end;
end;

procedure SaveViewToTemplate(const ARegPath: string; AView: TcxCustomGridView);
var
  AStorage: TcxStorage;
  AViewStorage: TcxCustomGridViewStorage;
begin
  AViewStorage := GetDefaultViewStorageByViewClass(AView);
  if AViewStorage = nil then Exit;
  try
    AStorage := TcxStorage.Create(ARegPath{ + '\' + AViewStorage.GetObjectName});
    try
      AStorage.Modes := AStorage.Modes + [smSavePublishedClassProperties];
      AStorage.Recreate := False;
      AStorage.StoreToRegistry(AViewStorage);
    finally
      AStorage.Free;
    end;
  finally
    AViewStorage.Free;
  end;
end;

function CreateViewMenuProvider(AView: TcxCustomGridView): TcxCustomGridViewMenuProvider;
var
  AClass: TcxCustomGridViewMenuProviderClass;
begin
  AClass := GetViewMenuProviderClass(AView);
  if AClass = nil then
    Result := nil
  else
    Result := AClass.Create(AView);
end;

function GetViewMenuProviderClass(AView: TcxCustomGridView): TcxCustomGridViewMenuProviderClass;
begin
  Result := TcxCustomGridViewMenuProviderClass(ViewMenuProviderClassList.FindClass(AView.ClassType));
end;

procedure RegisterViewMenuProviderClass(AViewClass: TcxCustomGridViewClass;
  AViewMenuProviderClass: TcxCustomGridViewMenuProviderClass);
begin
  ViewMenuProviderClassList.Register(AViewClass, AViewMenuProviderClass);
end;

procedure UnregisterViewMenuProviderClass(AViewClass: TcxCustomGridViewClass;
  AViewMenuProviderClass: TcxCustomGridViewMenuProviderClass);
begin
  ViewMenuProviderClassList.Unregister(AViewClass, AViewMenuProviderClass);
end;

{ TcxCustomGridViewStorage }

constructor TcxCustomGridViewStorage.Create(AView: TcxCustomGridView);
begin
  inherited Create(AView);
  FView := AView;
end;

class function TcxCustomGridViewStorage.GetViewClass: TcxCustomGridViewClass;
begin
  Result := nil;
end;

function TcxCustomGridViewStorage.GetObjectName: string;
begin
  Result := GetViewClass.ClassName;
end;

function TcxCustomGridViewStorage.GetProperties(AProperties: TStrings): Boolean;
begin
  Result := False;
end;

procedure TcxCustomGridViewStorage.GetPropertyValue(const AName: string; var AValue: Variant);
begin
end;

procedure TcxCustomGridViewStorage.SetPropertyValue(const AName: string; const AValue: Variant);
begin
end;

{ TcxViewEditor }

class function TcxViewEditor.GetViewByObject(APersistent: TPersistent): TcxCustomGridView;
begin
  Result := nil;
end;

procedure TcxViewEditor.SetView(Value: TcxCustomGridView; ARefreshNeeded: Boolean);
begin
  FView := Value;
  if ARefreshNeeded then
    UpdateEditor;
  UpdateSelection;
end;

procedure TcxViewEditor.BeginUpdate;
begin
  FormEditor.BeginUpdate;
end;

function TcxViewEditor.CanAddComponent: Boolean;
begin
  Result := FormEditor.CanAddComponent;
end;

function TcxViewEditor.CanDeleteComponent(AComponent: TComponent): Boolean;
begin
  Result := FormEditor.CanDeleteComponent(AComponent);
end;

procedure TcxViewEditor.EndUpdate;
begin
  FormEditor.EndUpdate;
end;

procedure TcxViewEditor.GetSelectionList(AList: TList);
begin
  FormEditor.GetSelectionList(AList);
end;

procedure TcxViewEditor.SelectComponent(AComponent: TPersistent);
begin
  FormEditor.SelectComponent(AComponent);
end;

function TcxViewEditor.UniqueName(AComponent: TComponent;
  const ATruncateClassName: string = ScxGridPrefixName): string;
begin
  Result := CreateUniqueName(OwnerForm, View, AComponent, ATruncateClassName, '');
end;

procedure TcxViewEditor.UpdateDesigner;
begin
  FormEditor.Designer.Modified;
end;

procedure TcxViewEditor.UpdateEditor;
begin
end;

procedure TcxViewEditor.UpdateSelection;
begin
end;

function TcxViewEditor.GetDataController: TcxCustomDataController;
begin
  Result := FView.DataController;
end;

function TcxViewEditor.GetOwnerForm: TComponent;
begin
  Result := FormEditor.Component.Owner;
end;

procedure TcxViewEditor.FormKeyPress(Sender: TObject; var Key: Char);
begin
  case Key of
    #13, #33..#126:
      begin
        if Key = #13 then Key := #0;
        TcxDesignWindowAccess(FormEditor).ActivateInspector(Key);
        Key := #0;
      end;
  end;
end;

{ TcxGridViewMenuItems }

constructor TcxGridViewMenuItems.Create;
begin
  inherited;
  FItems := TList.Create;
end;

destructor TcxGridViewMenuItems.Destroy;
begin
  ClearItems;
  FItems.Free;
  inherited;
end;

function TcxGridViewMenuItems.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TcxGridViewMenuItems.GetItem(Index: Integer): TcxGridViewMenuItem;
begin
  Result := TcxGridViewMenuItem(FItems[Index]);
end;

procedure TcxGridViewMenuItems.ClearItems;
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    Delete(I);
end;

function TcxGridViewMenuItems.AddItem(const ACaption: string;
  AOnClick: TcxGridViewMenuItemActionEvent = nil; AEnabled: Boolean = True;
  AChecked: Boolean = False; AData: TObject = nil): TcxGridViewMenuItem;
begin
  Result := TcxGridViewMenuItem.Create(ACaption, AEnabled, AChecked, AData, AOnClick);
  FItems.Add(Result);
end;

function TcxGridViewMenuItems.AddSeparator: TcxGridViewMenuItem;
begin
  Result := AddItem(cxGridViewMenuSeparatorCaption);
end;

procedure TcxGridViewMenuItems.Delete(AIndex: Integer);
begin
  Items[AIndex].Free;
  FItems.Delete(AIndex);
end;

procedure TcxGridViewMenuItems.Prepare(AMenuItem: TMenuItem);
var
  I: Integer;
  AChildMenuItem: TMenuItem;
  AItem: TcxGridViewMenuItem;
begin
  for I := 0 to Count - 1 do
  begin
    AChildMenuItem := CreateMenuItem(AMenuItem.Owner, '');
    AItem := Items[I];
    AItem.Prepare(AChildMenuItem);
    AMenuItem.Add(AChildMenuItem);
  end;
end;

procedure TcxGridViewMenuItems.Prepare(const AMenuItem: TDesignMenuItem);
var
  I: Integer;
  AChildMenuItem: TDesignMenuItem;
  AItem: TcxGridViewMenuItem;
begin
  for I := 0 to Count - 1 do
  begin
    AItem := Items[I];
    AChildMenuItem := AMenuItem.AddItem('', 0, False, True, AItem.MenuItemClick);
    AItem.Prepare(AChildMenuItem);
  end;
end;

procedure TcxGridViewMenuItems.RemoveExcessSeparators;
var
  APrevItemIsSeparator: Boolean;
  I: Integer;
begin
  APrevItemIsSeparator := True;
  for I := Count - 1 downto 0 do
    if Items[I].IsSeparator then
      if APrevItemIsSeparator then
        Delete(I)
      else
        APrevItemIsSeparator := True
    else
    begin
      APrevItemIsSeparator := False;
      Items[I].RemoveExcessSeparators;
    end;
end;

{ TcxGridViewMenuItem }

constructor TcxGridViewMenuItem.Create(const ACaption: string;
  AEnabled, AChecked: Boolean; AData: TObject; AOnAction: TcxGridViewMenuItemActionEvent);
begin
  inherited Create;
  Caption := ACaption;
  Enabled := AEnabled;
  Checked := AChecked;
  Data := AData;
  OnAction := AOnAction;
end;

destructor TcxGridViewMenuItem.Destroy;
begin
  FDesignItem := nil;
  inherited Destroy;
end;

procedure TcxGridViewMenuItem.MenuItemClick(Sender: TObject);
begin
  DoAction;
end;

procedure TcxGridViewMenuItem.DoAction;
begin
  if Assigned(OnAction) then OnAction(Self);
end;

function TcxGridViewMenuItem.IsSeparator: Boolean;
begin
  Result := Caption = cxGridViewMenuSeparatorCaption;
end;

procedure TcxGridViewMenuItem.Prepare(AMenuItem: TMenuItem);
begin
  AMenuItem.Caption := Caption;
  AMenuItem.Enabled := Enabled;
  AMenuItem.Checked := Checked;
  if not Assigned(AMenuItem.OnClick) then
    AMenuItem.OnClick := MenuItemClick;
  inherited;
end;

procedure TcxGridViewMenuItem.Prepare(const AMenuItem: TDesignMenuItem);
begin
  FDesignItem := AMenuItem;
  AMenuItem.Caption := Caption;
  AMenuItem.Enabled := Enabled;
  AMenuItem.Checked := Checked;
  inherited Prepare(AMenuItem);
end;

{ TcxCustomGridViewMenuProvider }

constructor TcxCustomGridViewMenuProvider.Create(AGridView: TcxCustomGridView);
begin
  inherited Create;
  FGridView := AGridView;
  FDesigner := GetObjectDesigner(GridView);
  FDesignHelper := TcxDesignHelper.Create(GridView.Control);
  FItems := TcxGridViewMenuItems.Create;
  InitItems;
  Items.RemoveExcessSeparators;
end;

destructor TcxCustomGridViewMenuProvider.Destroy;
begin
  FItems.Free;
  FDesignHelper.Free;
  inherited;
end;

{$IFNDEF NONDB}

procedure TcxCustomGridViewMenuProvider.GetDBDataSourceName(const S: string);
begin
  FDBDataSourceNames.Add(S);
end;

function TcxCustomGridViewMenuProvider.GetDBGridView: TcxCustomGridView;
begin
  Result := GridView;
  if not (Result.DataController is TcxDBDataController) then
    Result := nil;
end;

procedure TcxCustomGridViewMenuProvider.GetServerModeDataSourceName(const S: string);
begin
  FServerModeDataSourceNames.Add(S);
end;

function TcxCustomGridViewMenuProvider.GetServerModeViewDataSource: TdxServerModeCustomDataSource;
begin
  Result := TdxServerModeDataController(ServerModeGridView.DataController).DataSource;
end;

function TcxCustomGridViewMenuProvider.GetServerModeGridView: TcxCustomGridView;
begin
  Result := GridView;
  if not (Result.DataController is TdxServerModeDataController) then
    Result := nil;
end;

function TcxCustomGridViewMenuProvider.GetDBViewDataSource: TDataSource;
begin
  Result := TcxDBDataController(DBGridView.DataController).DataSource;
end;

procedure TcxCustomGridViewMenuProvider.SetDBViewDataSource(Value: TDataSource);
begin
  TcxDBDataController(DBGridView.DataController).DataSource := Value;
end;

procedure TcxCustomGridViewMenuProvider.SetServerModeViewDataSource(
  Value: TdxServerModeCustomDataSource);
begin
  TdxServerModeDataController(ServerModeGridView.DataController).DataSource := Value;
end;

{$ENDIF}

procedure TcxCustomGridViewMenuProvider.DesignerModified;
begin
  Designer.Modified;
end;

procedure TcxCustomGridViewMenuProvider.ObjectCreated(AObject: TPersistent);
begin
  SelectObject(AObject);
  DesignerModified;
end;

procedure TcxCustomGridViewMenuProvider.SelectObject(AObject: TPersistent);
begin
  DesignHelper.SelectObject(AObject, True, False);
end;

procedure TcxCustomGridViewMenuProvider.InitAdditionalItems;
var
  AIntf: IcxGridViewLayoutEditorSupport;
  AConditionalFormattingProviderIntf: IcxDataControllerConditionalFormattingProviderOwner;
begin
  if Supports(GridView, IcxDataControllerConditionalFormattingProviderOwner, AConditionalFormattingProviderIntf) and
      (AConditionalFormattingProviderIntf.GetConditionalFormattingProvider <> nil) and
      AConditionalFormattingProviderIntf.GetConditionalFormattingProvider.ConditionalFormatting.CanShowRulesManagerDialog then
    Items.AddItem('Conditional Formatting...', EditConditionalFormatting, True);
  Items.AddItem('Edit Layout and Data...', EditLayoutAndData,
    Supports(GridView, IcxGridViewLayoutEditorSupport, AIntf) and
    AIntf.CanEditViewLayoutAndData);
  if (FGridWizard <> nil) and FGridWizard.IsGridViewSupported(GridView.ClassType) then
    Items.AddItem('Edit via Wizard...', EditByWizard, CanChangeComponentList(GridView.Owner));
  InitCopySettingsFromViewItem;
  Items.AddItem('Set As Default', SetAsDefault);
end;

procedure TcxCustomGridViewMenuProvider.InitCopySettingsFromViewItem;
var
  AViewList: TcxGridViewList;
  AItem: TcxGridViewMenuItem;
  I: Integer;
begin
  AViewList := TcxGridViewList.Create(Designer);
  try
    if AViewList.ViewNames.Count = 1 then Exit;
    AItem := Items.AddItem('Copy Settings from View');
    for I := 0 to AViewList.ViewNames.Count - 1 do
      if AViewList.Views[I] <> GridView then
        AItem.AddItem(AViewList.ViewNames[I], CopySettings, True, False, AViewList.Views[I]);
  finally
    AViewList.Free;
  end;
end;

{$IFNDEF NONDB}

procedure TcxCustomGridViewMenuProvider.InitDataBindingItems;
begin
  if DBGridView <> nil then
  begin
    InitDBDataBindingItems;
    Items.AddSeparator;
  end;
  if ServerModeGridView <> nil then
  begin
    InitServerModeDataBindingItems;
    Items.AddSeparator;
  end;
end;

procedure TcxCustomGridViewMenuProvider.InitDBDataBindingItems;
var
  I: Integer;
  AItem: TcxGridViewMenuItem;
  ADataSourceNames: TStringList;

  function IsDataSourceLinked(AIndex: Integer): Boolean;
  begin
    Result := Designer.GetComponent(ADataSourceNames[AIndex]) = DBViewDataSource;
  end;

begin
  ADataSourceNames := GetDBDataSourceNames;
  try
    if ADataSourceNames.Count = 0 then Exit;
    if ADataSourceNames.Count = 1 then
      Items.AddItem('Link to ' + ADataSourceNames[0], LinkToDBDataSource, True,
        IsDataSourceLinked(0), TObject(0))
    else
    begin
      AItem := Items.AddItem('Link to DataSource', nil, True, False);
      for I := 0 to ADataSourceNames.Count - 1 do
        AItem.AddItem(ADataSourceNames[I], LinkToDBDataSource, True,
          IsDataSourceLinked(I), TObject(I));
    end;
  finally
    ADataSourceNames.Free;
  end;
end;

procedure TcxCustomGridViewMenuProvider.InitServerModeDataBindingItems;
var
  I: Integer;
  AItem: TcxGridViewMenuItem;
  ADataSourceNames: TStringList;

  function IsDataSourceLinked(AIndex: Integer): Boolean;
  begin
    Result := Designer.GetComponent(ADataSourceNames[AIndex]) = ServerModeViewDataSource;
  end;

begin
  ADataSourceNames := GetServerModeDataSourceNames;
  try
    if ADataSourceNames.Count = 0 then Exit;
    if ADataSourceNames.Count = 1 then
      Items.AddItem('Link to ' + ADataSourceNames[0], LinkToServerModeDataSource, True,
        IsDataSourceLinked(0), TObject(0))
    else
    begin
      AItem := Items.AddItem('Link to DataSource', nil, True, False);
      for I := 0 to ADataSourceNames.Count - 1 do
        AItem.AddItem(ADataSourceNames[I], LinkToServerModeDataSource, True,
          IsDataSourceLinked(I), TObject(I));
    end;
  finally
    ADataSourceNames.Free;
  end;
end;

{$ENDIF}

procedure TcxCustomGridViewMenuProvider.InitItems;
begin
{$IFNDEF NONDB}
  InitDataBindingItems;
{$ENDIF}
  InitStructureItems;
  Items.AddSeparator;
  InitLayoutItems;
  Items.AddSeparator;
  InitAdditionalItems;
end;

procedure TcxCustomGridViewMenuProvider.InitLayoutItems;
begin
end;

procedure TcxCustomGridViewMenuProvider.InitStructureItems;
begin
  Items.AddItem('Delete View', DeleteView, CanDeleteComponent(GridView.Owner, GridView));
end;

procedure TcxCustomGridViewMenuProvider.CopySettings(Sender: TcxGridViewMenuItem);
begin
  GridView.AssignSettings(Sender.Data as TcxCustomGridView);
  DesignerModified;
end;

procedure TcxCustomGridViewMenuProvider.DeleteView(Sender: TcxGridViewMenuItem);
begin
  SelectObject(GridView.Level);
  GridView.Free;
  DesignerModified;
end;

procedure TcxCustomGridViewMenuProvider.EditConditionalFormatting(Sender: TcxGridViewMenuItem);
var
  AIntf: IcxDataControllerConditionalFormattingProviderOwner;
begin
  if Supports(GridView, IcxDataControllerConditionalFormattingProviderOwner, AIntf) and
      (AIntf.GetConditionalFormattingProvider <> nil) then
    AIntf.GetConditionalFormattingProvider.ConditionalFormatting.ShowRulesManagerDialog;
end;

procedure TcxCustomGridViewMenuProvider.EditLayoutAndData(Sender: TcxGridViewMenuItem);
begin
  if ShowGridViewEditor(GridView) then
    DesignerModified;
end;

procedure TcxCustomGridViewMenuProvider.EditByWizard(Sender: TcxGridViewMenuItem);
begin
  FGridWizard.Execute(GridView.Control as TcxCustomGrid, GridView);
end;

procedure TcxCustomGridViewMenuProvider.SetAsDefault(Sender: TcxGridViewMenuItem);
begin
  SaveViewToTemplate(GetViewTemplateRegKey, GridView);
end;

{$IFNDEF NONDB}

function TcxCustomGridViewMenuProvider.GetDBDataSourceNames: TStringList;
begin
  Result := TStringList.Create;
  FDBDataSourceNames := Result;
  Designer.GetComponentNames(GetTypeData(TypeInfo(TDataSource)), GetDBDataSourceName);
  FDBDataSourceNames := nil;
  Result.Sort;
end;

function TcxCustomGridViewMenuProvider.GetServerModeDataSourceNames: TStringList;
begin
  Result := TStringList.Create;
  FServerModeDataSourceNames := Result;
  Designer.GetComponentNames(GetTypeData(TypeInfo(TdxServerModeCustomDataSource)), GetServerModeDataSourceName);
  FServerModeDataSourceNames := nil;
  Result.Sort;
end;

procedure TcxCustomGridViewMenuProvider.LinkToDBDataSource(Sender: TcxGridViewMenuItem);
var
  ADataSourceNames: TStringList;
  ADataSource: TDataSource;
begin
  ADataSourceNames := GetDBDataSourceNames;
  try
    ADataSource := TDataSource(Designer.GetComponent(ADataSourceNames[Integer(Sender.Data)]));
    if DBViewDataSource <> ADataSource then
    begin
      (DBGridView.DataController as IcxCustomGridDataController).DeleteAllItems;
      DBViewDataSource := ADataSource;
      (DBGridView.DataController as IcxCustomGridDataController).CreateAllItems(False);
      DesignerModified;
    end;
  finally
    ADataSourceNames.Free;
  end;
end;

procedure TcxCustomGridViewMenuProvider.LinkToServerModeDataSource(Sender: TcxGridViewMenuItem);
var
  ADataSourceNames: TStringList;
  ADataSource: TdxServerModeCustomDataSource;
begin
  ADataSourceNames := GetServerModeDataSourceNames;
  try
    ADataSource := TdxServerModeCustomDataSource(Designer.GetComponent(ADataSourceNames[Integer(Sender.Data)]));
    if ServerModeViewDataSource <> ADataSource then
    begin
      (ServerModeGridView.DataController as IcxCustomGridDataController).DeleteAllItems;
      ServerModeViewDataSource := ADataSource;
      (ServerModeGridView.DataController as IcxCustomGridDataController).CreateAllItems(False);
      DesignerModified;
    end;
  finally
    ADataSourceNames.Free;
  end;
end;

{$ENDIF}

{ TcxGridViewList }

constructor TcxGridViewList.Create(ADesigner: IDesigner);
begin
  inherited Create;
  FDesigner := ADesigner;
  FViewNames := TStringList.Create;
  GetViewNames;
  FViewNames.Sorted := True;
end;

destructor TcxGridViewList.Destroy;
begin
  FViewNames.Free;
  inherited;
end;

function TcxGridViewList.GetView(Index: Integer): TcxCustomGridView;
begin
  if (0 <= Index) and (Index < ViewNames.Count) then
    Result := Designer.GetComponent(ViewNames[Index]) as TcxCustomGridView
  else
    Result := nil;
end;

procedure TcxGridViewList.AddViewName(const S: string);
begin
  FViewNames.Add(S);
end;

procedure TcxGridViewList.GetViewNames;
begin
  ViewNames.Clear;
  Designer.GetComponentNames(GetTypeData(PTypeInfo(TcxCustomGridView.ClassInfo)), AddViewName);
end;

function TcxGridViewList.GetViewIndex(AView: TcxCustomGridView): Integer;
begin
  Result := ViewNames.IndexOf(Designer.GetComponentName(AView));
end;

initialization
  ViewEditorClassList := TcxRegisteredClassList.Create;
  ViewMenuProviderClassList := TcxRegisteredClassList.Create;
  RegisterViewMenuProviderClass(TcxCustomGridView, TcxCustomGridViewMenuProvider);

finalization
  UnregisterViewMenuProviderClass(TcxCustomGridView, TcxCustomGridViewMenuProvider);
  FreeAndNil(ViewMenuProviderClassList);
  FreeAndNil(ViewEditorClassList);
  FreeAndNil(DefaultViewStorages);
  FGridWizard := nil;
end.

