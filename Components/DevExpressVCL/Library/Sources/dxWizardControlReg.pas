{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressWizardControl                                     }
{                                                                    }
{           Copyright (c) 2012-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSWIZARDCONTROL AND ALL          }
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

unit dxWizardControlReg;

{$I cxVer.inc}

interface

uses
  Classes, DesignIntf, DesignEditors, DesignMenus, ImgList,
  SysUtils, dxCoreReg, dxCustomWizardControl, cxDesignWindows, dxWizardControl,
  dxWizardControlPageCollectionEditor, cxPropEditors, cxGraphics, cxLibraryReg;

const
  sdxWizardControlProductName = 'ExpressWizardControl Suite';

type

  { TdxWizardControlCustomComponentEditor }

  TdxWizardControlCustomComponentEditor = class(TdxComponentEditor)
  protected
    procedure InternalExecuteVerb(AIndex: Integer); override;
    function InternalGetVerb(AIndex: Integer): string; override;
    function InternalGetVerbCount: Integer; override;
    function GetProductName: string; override;
    function GetWizardControl: TdxCustomWizardControl; virtual; abstract;
  public
    procedure AddPage;
    procedure DeletePage;
    procedure Edit; override;
    procedure NextPage;
    procedure PrevPage;
    procedure PrepareItem(Index: Integer; const AItem: IMenuItem); override;

    property WizardControl: TdxCustomWizardControl read GetWizardControl;
  end;

  { TdxWizardControlComponentEditor }

  TdxWizardControlComponentEditor = class(TdxWizardControlCustomComponentEditor)
  protected
    function GetWizardControl: TdxCustomWizardControl; override;
  end;

  { TdxWizardControlPageComponentEditor }

  TdxWizardControlPageComponentEditor = class(TdxWizardControlCustomComponentEditor)
  protected
    function GetWizardControl: TdxCustomWizardControl; override;
  end;

  { TdxWizardControlPageDesignWindow }

  TdxWizardControlPageDesignWindow = class(TcxGlobalDesignWindow)
  private
    FCurrentSelection: TList;
  protected
    procedure DoAddFreeNotification(AList: TList);
    procedure DoRemoveFreeNotification(AList: TList);
    procedure DoSelectionChanged(AList: TList);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ItemDeleted(const ADesigner: IDesigner; Item: TPersistent); override;
    procedure SelectionsChanged(const ASelection: IDesignerSelections); override;
  end;

  { TdxWizardControlButtonImageIndexProperty }

  TdxWizardControlButtonImageIndexProperty = class(TImageIndexProperty)
  public
    function GetImages: TCustomImageList; override;
  end;

  { TdxWizardControlCustomButtonCollectionItemImageIndexProperty }

  TdxWizardControlCustomButtonCollectionItemImageIndexProperty = class(TImageIndexProperty)
  public
    function GetImages: TCustomImageList; override;
  end;

procedure Register;

implementation

const
  sdxDeletePage = 'Delete Pa&ge';
  sdxNewPage = 'Ne&w Page';
  sdxNextPage = 'Next P&age';
  sdxPageCollectionEditor = 'Page Collection Editor...';
  sdxPrevPage = 'Previo&us Page';

type
  TdxWizardControlCustomButtonCollectionAccess = class(TdxWizardControlCustomButtonCollection);
  TdxWizardControlCustomButtonsAccess = class(TdxWizardControlCustomButtons);
  TdxWizardControlPageAccess = class(TdxWizardControlCustomPage);

var
  FDesignWindow: TdxWizardControlPageDesignWindow;
  FRegisteredWizardControlPageCount: Integer;

{ TdxWizardControlCustomComponentEditor }

procedure TdxWizardControlCustomComponentEditor.InternalExecuteVerb(AIndex: Integer);
begin
  case AIndex of
    0: AddPage;
    1: NextPage;
    2: PrevPage;
    3: DeletePage;
    5: Edit;
    else
      inherited InternalExecuteVerb(AIndex - 1);
  end;
end;

function TdxWizardControlCustomComponentEditor.InternalGetVerb(AIndex: Integer): String;
begin
  case AIndex of
    0: Result := sdxNewPage;
    1: Result := sdxNextPage;
    2: Result := sdxPrevPage;
    3: Result := sdxDeletePage;
    4: Result := '-';
    5: Result := sdxPageCollectionEditor;
    else
      Result := inherited InternalGetVerb(AIndex - 1);
  end;
end;

function TdxWizardControlCustomComponentEditor.InternalGetVerbCount: Integer;
begin
  Result := inherited InternalGetVerbCount + 6;
end;

procedure TdxWizardControlCustomComponentEditor.AddPage;
var
  APage: TdxWizardControlCustomPage;
begin
  APage := WizardControl.AddPage(TdxWizardControlPage);
  APage.Name := Designer.UniqueName(TdxWizardControlPage.ClassName);
  Designer.SelectComponent(APage);
end;

procedure TdxWizardControlCustomComponentEditor.DeletePage;
begin
  ObjectInspectorCollapseProperty;
  WizardControl.DeletePage(WizardControl.ActivePage);
end;

procedure TdxWizardControlCustomComponentEditor.Edit;
begin
  ExecutePageCollectionEditor(WizardControl, Designer);
end;

procedure TdxWizardControlCustomComponentEditor.NextPage;
begin
  WizardControl.GoToNextPage;
  Designer.SelectComponent(WizardControl.ActivePage);
end;

procedure TdxWizardControlCustomComponentEditor.PrevPage;
begin
  WizardControl.GoToPrevPage;
  Designer.SelectComponent(WizardControl.ActivePage);
end;

procedure TdxWizardControlCustomComponentEditor.PrepareItem(Index: Integer; const AItem: IMenuItem);
begin
  inherited PrepareItem(Index, AItem);
  case Index of
    1: AItem.Enabled := WizardControl.CanGoToNextPage;
    2: AItem.Enabled := WizardControl.CanGoToPrevPage;
    3: AItem.Enabled := (WizardControl.ActivePage <> nil) and
        CanDeleteComponent(WizardControl, WizardControl.ActivePage, Designer);
  end;
end;

function TdxWizardControlCustomComponentEditor.GetProductName: string;
begin
  Result := sdxWizardControlProductName;
end;

{ TdxWizardControlComponentEditor }

function TdxWizardControlComponentEditor.GetWizardControl: TdxCustomWizardControl;
begin
  Result := Component as TdxWizardControl;
end;

{ TdxWizardControlPageComponentEditor }

function TdxWizardControlPageComponentEditor.GetWizardControl: TdxCustomWizardControl;
begin
  Result := (Component as TdxWizardControlPage).WizardControl;
end;

{ TdxWizardControlPageDesignWindow }

constructor TdxWizardControlPageDesignWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCurrentSelection := TList.Create;
end;

destructor TdxWizardControlPageDesignWindow.Destroy;
begin
  DoRemoveFreeNotification(FCurrentSelection);
  FreeAndNil(FCurrentSelection);
  inherited Destroy;
end;

procedure TdxWizardControlPageDesignWindow.DoAddFreeNotification(AList: TList);
var
  I: Integer;
begin
  for I := 0 to AList.Count - 1 do
  begin
    if TObject(AList[I]) is TComponent then
      TComponent(AList[I]).FreeNotification(Self);
  end;
end;

procedure TdxWizardControlPageDesignWindow.DoRemoveFreeNotification(AList: TList);
var
  I: Integer;
begin
  for I := 0 to AList.Count - 1 do
  begin
    if TObject(AList[I]) is TComponent then
      TComponent(AList[I]).RemoveFreeNotification(Self);
  end;
end;

procedure TdxWizardControlPageDesignWindow.DoSelectionChanged(AList: TList);
var
  ASelectableItem: IdxWizardControlSelectableItem;
  I: Integer;
begin
  for I := 0 to AList.Count - 1 do
  begin
    if Supports(TObject(AList[I]), IdxWizardControlSelectableItem, ASelectableItem) then
      ASelectableItem.SelectionChanged;
  end;
end;

procedure TdxWizardControlPageDesignWindow.Notification(
  AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
    FCurrentSelection.Remove(AComponent)
end;

procedure TdxWizardControlPageDesignWindow.ItemDeleted(const ADesigner: IDesigner; Item: TPersistent);
begin
  FCurrentSelection.Remove(Item);
end;

procedure TdxWizardControlPageDesignWindow.SelectionsChanged(const ASelection: IDesignerSelections);
var
  ANewSelection, AChanges: TList;
begin
  inherited SelectionsChanged(ASelection);
  AChanges := TList.Create;
  ANewSelection := TList.Create;
  try
    ConvertSelectionToList(ASelection, ANewSelection);
    AChanges.Assign(ANewSelection, laXor, FCurrentSelection);
    DoSelectionChanged(AChanges);
    DoRemoveFreeNotification(FCurrentSelection);
    FCurrentSelection.Assign(ANewSelection);
    DoAddFreeNotification(FCurrentSelection);
  finally
    ANewSelection.Free;
    AChanges.Free;
  end;
end;

{ TdxWizardControlButtonImageIndexProperty }

function TdxWizardControlButtonImageIndexProperty.GetImages: TCustomImageList;
begin
  Result := TdxWizardControlButtons(TdxWizardControlCustomButton(GetComponent(0)).Buttons).Images;
end;

{ TdxWizardControlCustomButtonCollectionItemImageIndexProperty }

function TdxWizardControlCustomButtonCollectionItemImageIndexProperty.GetImages: TCustomImageList;
begin
  Result := TdxWizardControlCustomButtonsAccess(TdxWizardControlCustomButtonCollectionAccess(
    TdxWizardControlCustomButtonCollectionItem(GetComponent(0)).Collection).CustomButtons).OwnerButtons.Images;
end;

// ---

procedure Register;
begin
  RegisterNoIcon([TdxWizardControlPage]);
  RegisterComponents(dxCoreLibraryProductPage, [TdxWizardControl]);
  RegisterComponentEditor(TdxWizardControl, TdxWizardControlComponentEditor);
  RegisterComponentEditor(TdxWizardControlPage, TdxWizardControlPageComponentEditor);
  RegisterPropertyEditor(TypeInfo(TcxImageIndex), TdxWizardControlCustomButton,
    'ImageIndex', TdxWizardControlButtonImageIndexProperty);
  RegisterPropertyEditor(TypeInfo(TcxImageIndex), TdxWizardControlCustomButtonCollectionItem,
    'ImageIndex', TdxWizardControlCustomButtonCollectionItemImageIndexProperty);
  HideClassProperties(TdxWizardControl, ['ActivePage']);
end;

procedure RegisterWizardControlPage(Sender: TObject);
begin
  if FRegisteredWizardControlPageCount = 0 then
    FDesignWindow := TdxWizardControlPageDesignWindow.Create(nil);
  TdxWizardControlPageAccess(Sender).FDesignHelper := TcxDesignHelper.Create(TComponent(Sender));
  Inc(FRegisteredWizardControlPageCount);
end;

procedure UnregisterWizardControlPage(Sender: TObject);
begin
  if FRegisteredWizardControlPageCount = 1 then
    FreeAndNil(FDesignWindow);
  TdxWizardControlPageAccess(Sender).FDesignHelper := nil;
  Dec(FRegisteredWizardControlPageCount);
end;

initialization
  FOnRegisterWizardControlPage := RegisterWizardControlPage;
  FOnUnregisterWizardControlPage := UnregisterWizardControlPage;
end.
