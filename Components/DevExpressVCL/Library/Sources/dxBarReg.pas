{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressBars registering unit                             }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSBARS AND ALL ACCOMPANYING VCL  }
{   CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY.                  }
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

unit dxBarReg;

{$I cxVer.inc}

interface

uses
  ImgList,
  DesignEditors, DesignIntf, DesignMenus, VCLEditors,
  Windows, Classes, Controls, Graphics, Contnrs,
  dxCoreReg, cxLibraryReg, dxCore, cxPropEditors, cxDesignWindows, cxControls, dxBar,
  dxScreenTipRepositoryEditor;

const
  dxBarProductName = 'ExpressBars';
  dxBarProductPage = 'ExpressBars';

type
  TdxBarComponentEditor = class(TdxComponentEditor)
  protected
    function GetProductName: string; override;
  end;

  TdxBarItemImageIndexProperty = class(TImageIndexProperty)
  private
    function GetBarManager: TdxBarManager;
  protected
    property BarManager: TdxBarManager read GetBarManager;
  public
    function GetImages: TCustomImageList; override;
  end;

  TdxBarItemLargeImageIndexProperty = class(TdxBarItemImageIndexProperty)
  public
    function GetImages: TCustomImageList; override;
  end;

  TdxBarItemHotImageIndexProperty = class(TdxBarItemImageIndexProperty)
  public
    function GetImages: TCustomImageList; override;
  end;

  TdxBarItemLinkImageIndexProperty = class(TImageIndexProperty)
  private
    function GetItemLink: TdxBarItemLink;
  protected
    property ItemLink: TdxBarItemLink read GetItemLink;
  public
    function GetImages: TCustomImageList; override;
  end;

  TdxBarItemLinksPropertyEditor = class(TPropertyEditor)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
  end;

procedure Register;

implementation

uses
  Messages, SysUtils, Forms, Dialogs, TypInfo, TreeIntf, Menus,
  dxRegEd, cxClasses, cxComponentCollectionEditor,
  dxBarStrs, dxBarCustomCustomizationForm, dxBarCustForm, dxBarPopupMenuEd, dxBarConverter, dxBarUIGenerator;

type
  TdxBarManagerAccess = class(TdxBarManager);
  TdxBarItemLinksAccess = class(TdxBarItemLinks);

  { TdxBarDesignHelper }

  TdxBarDesignHelper = class(TcxDesignHelper, IdxBarDesigner)
  public
    // IdxBarDesigner
    function CanDeleteComponent(AComponent: TComponent): Boolean;
    function GetSelectionStatus(AComponent: TPersistent): TdxBarSelectionStatus;
    function IdxBarDesigner.IsComponentSelected = IsObjectSelected;
    procedure SelectComponent(AComponent: TPersistent; ASelectionOperation: TdxBarSelectionOperation = soExclusive);
    procedure ShowDefaultEventHandler(AItem: TdxBarItem);
  end;

  { TdxBarDesignWindow }

  TdxBarDesignWindow = class(TcxGlobalDesignWindow)
  private
    FActiveDesigner: IDesigner;
//    FCurrentSelectionList: TComponentList;
    FCurrentSelectionList: TObjectList;
    FOnSelectionChanged: TcxNotifyProcedure;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function IsComponentSelected(AComponent: TPersistent): Boolean;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    procedure ItemDeleted(const ADesigner: IDesigner; Item: TPersistent); override;
    procedure SelectionsChanged(const ASelection: TDesignerSelectionList); override;

    property ActiveDesigner: IDesigner read FActiveDesigner;
    property OnSelectionChanged: TcxNotifyProcedure read FOnSelectionChanged write FOnSelectionChanged;
  end;

  { TdxBarManagerEditor }

  TdxBarManagerEditor = class(TdxBarComponentEditor)
  protected
    procedure DoLinkTo(AObject: TObject); override;
    function GetLinkToItemCaption: string; override;
    function GetLinkToTypeClass: TClass; override;
    function IsLinkable: Boolean; override;

    procedure InternalExecuteVerb(AIndex: Integer); override;
    function InternalGetVerb(AIndex: Integer): string; override;
    function InternalGetVerbCount: Integer; override;
  public
    procedure PrepareItem(Index: Integer; const AItem: TDesignMenuItem); override;
    // routines
    function BarManager: TdxBarManager;
  end;

  { TdxBarManagerSelection }

  TdxBarManagerSelection = class(TSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

  { TdxBarPopupMenuEditor }

  TdxBarPopupMenuEditor = class(TdxBarComponentEditor)
  protected
    procedure DoLinkTo(AObject: TObject); override;
    function GetLinkToItemCaption: string; override;
    function GetLinkToTypeClass: TClass; override;
    function IsLinkable: Boolean; override;

    function InternalGetVerb(AIndex: Integer): string; override;
    function InternalGetVerbCount: Integer; override;
    procedure InternalExecuteVerb(AIndex: Integer); override;
  end;

var
  FdxBarDesignWindow: TdxBarDesignWindow;

{ TdxBarDesignHelper }

function TdxBarDesignHelper.CanDeleteComponent(AComponent: TComponent): Boolean;
begin
  Result := cxDesignWindows.CanDeleteComponent(Component, AComponent, Designer);
end;

function TdxBarDesignHelper.GetSelectionStatus(AComponent: TPersistent): TdxBarSelectionStatus;
begin
  if FdxBarDesignWindow.IsComponentSelected(AComponent) then
    Result := ssActiveSelected
  else
    if IsObjectSelected(AComponent) then
      Result := ssInactiveSelected
    else
      Result := ssUnselected;
end;

procedure TdxBarDesignHelper.SelectComponent(AComponent: TPersistent; ASelectionOperation: TdxBarSelectionOperation = soExclusive);
begin
  case ASelectionOperation of
    soAdd: SelectObject(AComponent, False);
    soExclude: UnselectObject(AComponent);
    soExclusive: SelectObject(AComponent, True, False);
  end;
end;

procedure TdxBarDesignHelper.ShowDefaultEventHandler(AItem: TdxBarItem);
begin
  ShowComponentDefaultEventHandler(AItem);
end;

{ TdxBarDesignWindow }

constructor TdxBarDesignWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
//  FCurrentSelectionList := TComponentList.Create(False);
  FCurrentSelectionList := TObjectList.Create(False);
end;

destructor TdxBarDesignWindow.Destroy;
begin
  FreeAndNil(FCurrentSelectionList);
  inherited;
end;

function TdxBarDesignWindow.IsComponentSelected(AComponent: TPersistent): Boolean;
begin
  Result := FCurrentSelectionList.IndexOf(TComponent(AComponent)) <> -1;
end;

procedure TdxBarDesignWindow.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
    FCurrentSelectionList.Remove(AComponent);
end;

procedure TdxBarDesignWindow.ItemDeleted(const ADesigner: IDesigner; Item: TPersistent);
begin
  FCurrentSelectionList.Remove(Item);
end;

procedure TdxBarDesignWindow.SelectionsChanged(const ASelection: TDesignerSelectionList);
var
  I: Integer;
  ANewSelection, AOldSelection, ASelectionChanges: TdxObjectList;
  ASelectableItem: IdxBarSelectableItem;
begin
  inherited;

  ASelectionChanges := TdxObjectList.Create(False);
  AOldSelection := TdxObjectList.Create(False);
  ANewSelection := TdxObjectList.Create(False);
  try
    AOldSelection.CopyFrom(FCurrentSelectionList);
    ConvertSelectionToList(ASelection, ANewSelection);
    CleanSelectableItems(ANewSelection);
    ANewSelection.CopyTo(FCurrentSelectionList);

    ASelectionChanges.XorList(FCurrentSelectionList, AOldSelection);

    // add to invalidation single selected object
    if ASelectionChanges.Count > 0 then
    begin
      if AOldSelection.Count = 1 then
        ASelectionChanges.Add(AOldSelection[0]);
      if ANewSelection.Count = 1 then
        ASelectionChanges.Add(ANewSelection[0]);
    end;

    for I := ASelectionChanges.Count - 1 downto 0 do
    begin
      if IsSelectableItem(ASelectionChanges[I], ASelectableItem) then
        ASelectableItem.SelectionChanged;
    end;

    for I := 0 to AOldSelection.Count - 1 do
      if AOldSelection[I] is TComponent then
        TComponent(AOldSelection[I]).RemoveFreeNotification(Self);
    for I := 0 to FCurrentSelectionList.Count - 1 do
      if FCurrentSelectionList[I] is TComponent then
        TComponent(FCurrentSelectionList[I]).FreeNotification(Self);

  finally
    ANewSelection.Free;
    AOldSelection.Free;
    ASelectionChanges.Free;
  end;

  if Assigned(OnSelectionChanged) then
    OnSelectionChanged(Self);
end;

{ TdxBarComponentEditor }

function TdxBarComponentEditor.GetProductName: string;
begin
  Result := dxBarProductName;
end;

{ TdxBarManagerEditor }

procedure TdxBarManagerEditor.PrepareItem(Index: Integer; const AItem: TDesignMenuItem);
begin
  inherited PrepareItem(Index, AItem);
  if Index in [1, 2] then
    AItem.Enabled := TdxBarManagerAccess(BarManager).CanAddComponents;
  if Index = 2 then
    AItem.Enabled := AItem.Enabled and (BarManager.MainMenuBar = nil);
end;

function TdxBarManagerEditor.BarManager: TdxBarManager;
begin
  Result := TdxBarManager(Component);
end;

procedure TdxBarManagerEditor.DoLinkTo(AObject: TObject);
begin
  dxBarConvertMainMenu(AObject as TMainMenu, BarManager);
  AObject.Free;
end;

function TdxBarManagerEditor.GetLinkToItemCaption: string;
begin
  Result := 'Convert Main Menu';
end;

function TdxBarManagerEditor.GetLinkToTypeClass: TClass;
begin
  Result := TMainMenu;
end;

function TdxBarManagerEditor.IsLinkable: Boolean;
begin
  Result := True;
end;

procedure TdxBarManagerEditor.InternalExecuteVerb(AIndex: Integer);
begin
  case AIndex of
    0: BarManager.Customizing(True);
    1: BarManager.AddToolBar;
    2: BarManager.AddToolBar(True)
  end;
end;

function TdxBarManagerEditor.InternalGetVerb(AIndex: Integer): string;
begin
  case AIndex of
    0: Result := cxGetResourceString(@dxSBAR_CUSTOMIZINGFORM);
    1: Result := 'Add Toolbar';
    2: Result := 'Add MainMenu';
  else
    Result := inherited InternalGetVerb(AIndex);
  end;
end;

function TdxBarManagerEditor.InternalGetVerbCount: Integer;
begin
  Result := 3;
end;

{ TdxBarManagerSelection }

procedure TdxBarManagerSelection.RequiresUnits(Proc: TGetStrProc);
begin
  inherited RequiresUnits(Proc);
  dxSkinsRequiresAdditionalUnits(TdxBarManager, Proc);
end;

{ TdxBarPopupMenuEditor }

procedure TdxBarPopupMenuEditor.DoLinkTo(AObject: TObject);
begin
  dxBarConvertPopupMenu(AObject as TPopupMenu, TdxBarCustomPopupMenu(Component));
  AObject.Free;
end;

function TdxBarPopupMenuEditor.GetLinkToItemCaption: string;
begin
  Result := 'Convert Popup Menu';
end;

function TdxBarPopupMenuEditor.GetLinkToTypeClass: TClass;
begin
  Result := TPopupMenu;
end;

function TdxBarPopupMenuEditor.IsLinkable: Boolean;
begin
  Result := True;
end;

function TdxBarPopupMenuEditor.InternalGetVerb(AIndex: Integer): string;
begin
  Result := cxGetResourceString(@dxSBAR_POPUPMENUEDITOR);
end;

function TdxBarPopupMenuEditor.InternalGetVerbCount: Integer;
begin
  Result := 1;
end;

procedure TdxBarPopupMenuEditor.InternalExecuteVerb(AIndex: Integer);
begin
  ShowdxBarSubMenuEditor(TdxBarCustomPopupMenu(Component).ItemLinks);
end;

{ TdxBarsPropertyEditor }

type
  TdxBarsPropertyEditor = class(TPropertyEditor)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
  end;

procedure TdxBarsPropertyEditor.Edit;
var
  BarManager: TdxBarManager;
begin
  BarManager := TdxBarManager(GetComponent(0));
  BarManager.Customizing(True);
  if BarManager.IsCustomizing then
    dxBarCustomizingForm.SelectPage(0);
end;

function TdxBarsPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

function TdxBarsPropertyEditor.GetValue: string;
begin
  Result := Format('(%s)', [TdxBars.ClassName]);
end;

{ TdxCategoriesPropertyEditor }

type
  TdxCategoriesPropertyEditor = class(TPropertyEditor)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
  end;

procedure TdxCategoriesPropertyEditor.Edit;
var
  BarManager: TdxBarManager;
begin
  BarManager := TdxBarManager(GetComponent(0));
  BarManager.Customizing(True);
  if BarManager.IsCustomizing then
    dxBarCustomizingForm.SelectPage(1);
end;

function TdxCategoriesPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

function TdxCategoriesPropertyEditor.GetValue: string;
begin
  Result := Format('(%s)', [TStrings.ClassName]);
end;

{ TdxRegistryPathProperty }

type
  TdxRegistryPathProperty = class(TStringProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

procedure TdxRegistryPathProperty.Edit;
var
  BarManager: TdxBarManager;
  S: string;
begin
  BarManager := TdxBarManager(GetComponent(0));
  S := BarManager.RegistryPath;
  if dxGetRegistryPath(S) then
  begin
    BarManager.RegistryPath := S;
    Designer.Modified;
  end;
end;

function TdxRegistryPathProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog];
end;

{ TDetachingBarPropertyEditor }

const
  NoneBarCaption = '<none>';

type
  TDetachingBarPropertyEditor = class(TPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

function TDetachingBarPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList];
end;

procedure TDetachingBarPropertyEditor.GetValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  Proc(NoneBarCaption);
  with TdxBarItem(GetComponent(0)).BarManager do
    for I := 0 to Bars.Count - 1 do
      Proc(Bars[I].Caption);
end;

function TDetachingBarPropertyEditor.GetValue: string;
begin
  with TCustomdxBarSubItem(GetComponent(0)) do
    if GetDetachingBar = nil then
      Result := NoneBarCaption
    else
      Result := GetDetachingBar.Caption;
end;

procedure TDetachingBarPropertyEditor.SetValue(const Value: string);
begin
  with TCustomdxBarSubItem(GetComponent(0)) do
    if (Value = NoneBarCaption) or (BarManager.BarByCaption(Value) = nil) then
      DetachingBar := -1
    else
      DetachingBar := BarManager.BarByCaption(Value).Index;
  Modified;
end;

{ TdxBarItemImageIndexProperty  }

function TdxBarItemImageIndexProperty.GetImages: TCustomImageList;
begin
  Result := BarManager.Images;
end;

function TdxBarItemImageIndexProperty.GetBarManager: TdxBarManager;
begin
  Result := (GetComponent(0) as TdxBarItem).BarManager;
end;

{ TdxBarItemLargeImageIndexProperty }

function TdxBarItemLargeImageIndexProperty.GetImages: TCustomImageList;
begin
  Result := BarManager.LargeImages;
end;

{ TdxBarItemHotImageIndexProperty }

function TdxBarItemHotImageIndexProperty.GetImages: TCustomImageList;
begin
  Result := BarManager.HotImages;
end;

{ TdxBarItemLinkImageIndexProperty  }

function TdxBarItemLinkImageIndexProperty.GetImages: TCustomImageList;

  function GetCurrentImages(ALinks: TdxBarItemLinks): TCustomImageList;
  begin
    Result := TdxBarItemLinksAccess(ALinks).LinksOwner.GetImages;
    if (Result = nil) and (ALinks.Owner is TCustomdxBarSubItem) and (TCustomdxBarSubItem(ALinks.Owner).LinkCount > 0) then
      Result := GetCurrentImages(TCustomdxBarSubItem(ALinks.Owner).Links[0].Owner);
  end;

var
  ABarItemLink: TdxBarItemLink;
begin
  ABarItemLink := ItemLink;
  Result := TdxBarItemLinksAccess(ABarItemLink.Owner).GetCurrentImages;
  if Result = nil then
    Result := GetCurrentImages(ABarItemLink.Owner);
  if Result = nil then
    Result := ItemLink.BarManager.Images;
end;

function TdxBarItemLinkImageIndexProperty.GetItemLink: TdxBarItemLink;
begin
  Result := GetComponent(0) as TdxBarItemLink;
end;

{ TdxBarItemLinksPropertyEditor }

procedure TdxBarItemLinksPropertyEditor.Edit;
begin
  if not (GetComponent(0) is TdxBar) then
    ShowdxBarSubMenuEditor(TdxBarItemLinks(GetOrdValue));
end;

function TdxBarItemLinksPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paReadOnly];
  if not (GetComponent(0) is TdxBar) then Include(Result, paDialog);
end;

function TdxBarItemLinksPropertyEditor.GetValue: string;
begin
  Result := Format('(%s)', [TdxBarItemLinks.ClassName]);
end;

{ TdxBarScreenTipRepositoryEditor }

type
  TdxBarScreenTipRepositoryEditor = class(TdxBarComponentEditor)
  private
    function GetRepository: TdxBarScreenTipRepository;
  protected
    function InternalGetVerb(AIndex: Integer): string; override;
    function InternalGetVerbCount: Integer; override;
    procedure InternalExecuteVerb(AIndex: Integer); override;
  public
    property Repository: TdxBarScreenTipRepository read GetRepository;
  end;

function TdxBarScreenTipRepositoryEditor.InternalGetVerb(AIndex: Integer): string;
begin
  Result := 'Edit...';
end;

function TdxBarScreenTipRepositoryEditor.InternalGetVerbCount: Integer;
begin
  Result := 1
end;

procedure TdxBarScreenTipRepositoryEditor.InternalExecuteVerb(AIndex: Integer);
begin
  ShowFormEditorClass(Designer, Component, Repository.Items,
    'Items', TfrmScreenTipRepositoryEditor);
end;

function TdxBarScreenTipRepositoryEditor.GetRepository: TdxBarScreenTipRepository;
begin
  Result := Component as TdxBarScreenTipRepository;
end;

{ TdxBarItemLinksSprig }

type
  TdxBarItemLinksSprig = class(TCollectionSprig)
  public
    function AddTypeCount: Integer; override;
  end;

function TdxBarItemLinksSprig.AddTypeCount: Integer;
begin
  Result := 0;
end;

{ register }

procedure Register;
begin
  ForceDemandLoadState(dlDisable);

  RegisterComponents(dxBarProductPage, [TdxBarManager, TdxBarPopupMenu, TdxBarDockControl, TdxBarScreenTipRepository]);
  RegisterNoIcon([
    TdxBarGroup, TdxBarButton, TdxBarLargeButton,  TdxBarEdit, TCustomdxBarCombo, TdxBarCombo, TdxBarSeparator,
    TdxBarSubItem, TdxBarListItem, TdxBarContainerItem, TdxBar, TdxBarScreenTip]);

  RegisterSelectionEditor(TdxBarManager, TdxBarManagerSelection);
  RegisterComponentEditor(TdxBarManager, TdxBarManagerEditor);
  RegisterComponentEditor(TdxBarCustomPopupMenu, TdxBarPopupMenuEditor);
  RegisterComponentEditor(TdxBarScreenTipRepository, TdxBarScreenTipRepositoryEditor);
  RegisterComponentEditor(TdxBarDockControl, TdxBarComponentEditor);

  RegisterPropertyEditor(TypeInfo(TShortCut), TdxBarCustomButton, 'ShortCut', TShortCutProperty);
  RegisterPropertyEditor(TypeInfo(TdxBars), TdxBarManager, 'Bars',
    TdxBarsPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStrings), TdxBarManager, 'Categories',
    TdxCategoriesPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), TdxBarManager, 'RegistryPath',
    TdxRegistryPathProperty);
  RegisterPropertyEditor(TypeInfo(TdxBarItemLinks), TdxBar, 'ItemLinks',
    TdxBarItemLinksPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TdxBarItemLinks), TdxBarCustomPopupComponent, 'ItemLinks',
    TdxBarItemLinksPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TdxBarItemLinks), TCustomdxBarSubItem, 'ItemLinks',
    TdxBarItemLinksPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Integer), TCustomdxBarSubItem, 'DetachingBar',
    TDetachingBarPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Integer), TdxBarItem, 'ImageIndex',
    TdxBarItemImageIndexProperty);
  RegisterPropertyEditor(TypeInfo(Integer), TdxBarItem, 'LargeImageIndex',
    TdxBarItemLargeImageIndexProperty);
  RegisterPropertyEditor(TypeInfo(Integer), TdxBarLargeButton, 'HotImageIndex',
    TdxBarItemHotImageIndexProperty);
  RegisterPropertyEditor(TypeInfo(Integer), TdxBarItemLink, 'ImageIndex',
    TdxBarItemLinkImageIndexProperty);

  RegisterPropertyEditor(TypeInfo(TBitmap), TdxBarItem, 'Glyph', TcxBitmapProperty);
  RegisterPropertyEditor(TypeInfo(TBitmap), TdxBarItem, 'LargeGlyph', TcxBitmapProperty);
  RegisterPropertyEditor(TypeInfo(TBitmap), TdxBarItemLink, 'UserGlyph', TcxBitmapProperty);
  RegisterPropertyEditor(TypeInfo(TBitmap), TdxBarLargeButton, 'HotGlyph', TcxBitmapProperty);
//  RegisterPropertyEditor(TypeInfo(Integer), TdxBarLargeButton, 'ImageIndex', nil);
//  RegisterPropertyEditor(TypeInfo(TBitmap), TdxBarLargeButton, 'Glyph', nil);

  RegisterSprigType(TdxBarItemLinks, TdxBarItemLinksSprig);

  // ImageOptions
  HideClassProperties(TdxBarManager, ['DisabledImages', 'DisabledLargeImages',
    'HotImages', 'Images', 'LargeImages', 'ImageListBkColor', 'LargeIcons',
    'MakeDisabledImagesFaded', 'StretchGlyphs', 'UseLargeImagesForLargeIcons']);

  HideClassProperties(TdxBarSeparator, ['Action', 'Align', 'Category',
    'Description', 'Enabled', 'HelpContext', 'Hint', 'MergeKind', 'MergeOrder',
    'ScreenTip', 'OnDestroy']);

  HideClassProperties(TdxBarItemLink, ['ViewLevels']);
end;

procedure DesignSelectionChanged(ASender: TObject);
begin
  if dxBarCustomizingForm <> nil then
    dxBarCustomizingForm.DesignSelectionChanged(ASender);
end;

procedure RegisterBarManager(ASender: TObject);
begin
  if dxBarManagerList.Count = 1 then
  begin
    FdxBarDesignWindow := TdxBarDesignWindow.Create(nil);
    FdxBarDesignWindow.OnSelectionChanged := DesignSelectionChanged;
  end;
  TdxBarManagerAccess(ASender).FdxBarDesignHelper := TdxBarDesignHelper.Create(TComponent(ASender));
end;

procedure UnregisterBarManager(ASender: TObject);
begin
  if (FdxBarDesignWindow <> nil) and ((dxBarManagerList = nil) or (dxBarManagerList.Count = 0)) then
    cxReleaseForm(FdxBarDesignWindow);
  TdxBarManagerAccess(ASender).FdxBarDesignHelper := nil;
end;

initialization
  FOnRegisterBarManager := RegisterBarManager;
  FOnUnregisterBarManager := UnregisterBarManager;

end.
