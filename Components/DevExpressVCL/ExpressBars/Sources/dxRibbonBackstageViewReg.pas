{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressBars components                                   }
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

unit dxRibbonBackstageViewReg;

interface

{$I cxVer.inc}

uses
  ColnEdit,
  DesignEditors, DesignIntf,
  TypInfo, Windows, Classes, SysUtils, Messages, Controls, Graphics, Math, Forms, StdCtrls, ImgList,
  dxCore, cxGraphics, cxGeometry, cxControls, cxClasses, dxBar, dxRibbon, dxRibbonSkins, dxRibbonBackstageView,
  dxCoreReg, dxBarReg, cxDesignWindows, cxComponentCollectionEditor, dxRibbonBackstageViewGalleryControl,
  dxGalleryDesigner, cxLibraryReg, cxPropEditors;

procedure Register;

implementation

uses
  dxBarStrs;

type

  { TdxRibbonBackstageViewComponentEditor }

  TdxRibbonBackstageViewComponentEditor = class(TdxBarComponentEditor)
  private
    function GetBackstageView: TdxRibbonCustomBackstageView;
  protected
    function InternalGetVerb(AIndex: Integer): string; override;
    function InternalGetVerbCount: Integer; override;
    procedure InternalExecuteVerb(AIndex: Integer); override;
  public
    procedure DeleteActiveTab;
    procedure Edit; override;
    procedure NewTab;
    procedure PrepareItem(Index: Integer; const AItem: TDesignMenuItem); override;
    //
    property BackstageView: TdxRibbonCustomBackstageView read GetBackstageView;
  end;

  { TdxRibbonBackstageViewGalleryControlComponentEditor }

  TdxRibbonBackstageViewGalleryControlComponentEditor = class(TdxBarComponentEditor)
  protected
    function InternalGetVerb(AIndex: Integer): string; override;
    function InternalGetVerbCount: Integer; override;
    procedure InternalExecuteVerb(AIndex: Integer); override;
  public
    procedure Edit; override;
  end;

  { TdxRibbonBackstageViewTabSheetImageIndexProperty }

  TdxRibbonBackstageViewTabSheetImageIndexProperty = class(TImageIndexProperty)
  public
    function GetImages: TCustomImageList; override;
  end;

  { TdxRibbonBackstageViewDesignWindow }

  TdxRibbonBackstageViewDesignWindow = class(TcxGlobalDesignWindow)
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

  TdxRibbonCustomBackstageViewAccess = class(TdxRibbonCustomBackstageView);

var
  FDesignWindow: TdxRibbonBackstageViewDesignWindow;
  FRegisteredBackstageViewCount: Integer;

procedure Register;
begin
  RegisterComponents(dxBarProductPage, [TdxRibbonBackstageView, TdxRibbonBackstageViewGalleryControl]);
  RegisterNoIcon([TdxRibbonBackstageViewGalleryGroup, TdxRibbonBackstageViewGalleryItem]);

  RegisterComponentEditor(TdxRibbonBackstageView, TdxRibbonBackstageViewComponentEditor);
  RegisterComponentEditor(TdxRibbonBackstageViewTabSheet, TdxRibbonBackstageViewComponentEditor);
  RegisterComponentEditor(TdxRibbonBackstageViewGalleryControl, TdxRibbonBackstageViewGalleryControlComponentEditor);

  RegisterPropertyEditor(TypeInfo(TcxImageIndex), TdxRibbonBackstageViewTabSheet,
    'ImageIndex', TdxRibbonBackstageViewTabSheetImageIndexProperty);
  RegisterPropertyEditor(GetPropInfo(TdxRibbonBackstageViewGalleryControl, 'Gallery').PropType^,
    TdxRibbonBackstageViewGalleryControl, 'Gallery', TdxGalleryPropertyEditor);
end;

procedure RegisterBackstageView(Sender: TObject);
begin
  if FRegisteredBackstageViewCount = 0 then
    FDesignWindow := TdxRibbonBackstageViewDesignWindow.Create(nil);
  TdxRibbonCustomBackstageViewAccess(Sender).FDesignHelper := TcxDesignHelper.Create(TComponent(Sender));
  Inc(FRegisteredBackstageViewCount);
end;

procedure UnregisterBackstageView(Sender: TObject);
begin
  if FRegisteredBackstageViewCount = 1 then
    FreeAndNil(FDesignWindow);
  TdxRibbonCustomBackstageViewAccess(Sender).FDesignHelper := nil;
  Dec(FRegisteredBackstageViewCount);
end;

{ TdxRibbonBackstageViewComponentEditor }

procedure TdxRibbonBackstageViewComponentEditor.DeleteActiveTab;
begin
  BackstageView.ActiveTab.Free;
end;

function TdxRibbonBackstageViewComponentEditor.InternalGetVerb(AIndex: Integer): string;
begin
  case AIndex of
    0: Result := cxGetResourceString(@dxSBAR_RIBBONADDTAB);
    1: Result := cxGetResourceString(@dxSBAR_RIBBONDELETETAB);
    else
      Result := '';
  end;
end;

function TdxRibbonBackstageViewComponentEditor.InternalGetVerbCount: Integer;
begin
  Result := 2;
end;

procedure TdxRibbonBackstageViewComponentEditor.InternalExecuteVerb(AIndex: Integer);
begin
  case AIndex of
    0: NewTab;
    1: DeleteActiveTab;
  end;
end;

procedure TdxRibbonBackstageViewComponentEditor.Edit;
begin
end;

procedure TdxRibbonBackstageViewComponentEditor.NewTab;
var
  ATab: TdxRibbonBackstageViewTabSheet;
begin
  ATab := BackstageView.AddTab;
  ATab.Name := Designer.UniqueName(TdxRibbonBackstageViewTabSheet.ClassName);
  ATab.Active := True;
  Designer.SelectComponent(ATab);
end;

procedure TdxRibbonBackstageViewComponentEditor.PrepareItem(Index: Integer; const AItem: TDesignMenuItem);
begin
  if Index = 1 then
    AItem.Enabled := Assigned(BackstageView.ActiveTab);
end;

function TdxRibbonBackstageViewComponentEditor.GetBackstageView: TdxRibbonCustomBackstageView;
begin
  if Component is TdxRibbonBackstageViewTabSheet then
    Result := TdxRibbonBackstageViewTabSheet(Component).BackstageView
  else
    Result := TdxRibbonCustomBackstageView(Component);
end;

{ TdxRibbonBackstageViewGalleryControlComponentEditor }

procedure TdxRibbonBackstageViewGalleryControlComponentEditor.Edit;
begin
  EditGallery(Component);
end;

procedure TdxRibbonBackstageViewGalleryControlComponentEditor.InternalExecuteVerb(AIndex: Integer);
begin
  Edit;
end;

function TdxRibbonBackstageViewGalleryControlComponentEditor.InternalGetVerb(AIndex: Integer): string;
begin
  Result := 'Edit...';
end;

function TdxRibbonBackstageViewGalleryControlComponentEditor.InternalGetVerbCount: Integer;
begin
  Result := 1;
end;

{ TdxRibbonBackstageViewTabSheetImageIndexProperty }

function TdxRibbonBackstageViewTabSheetImageIndexProperty.GetImages: TCustomImageList;
begin
  Result := TdxRibbonBackstageViewTabSheet(GetComponent(0)).Images;
end;

{ TdxRibbonBackstageViewDesignWindow }

constructor TdxRibbonBackstageViewDesignWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCurrentSelection := TList.Create;
end;

destructor TdxRibbonBackstageViewDesignWindow.Destroy;
begin
  DoRemoveFreeNotification(FCurrentSelection);
  FreeAndNil(FCurrentSelection);
  inherited Destroy;
end;

procedure TdxRibbonBackstageViewDesignWindow.DoAddFreeNotification(AList: TList);
var
  I: Integer;
begin
  for I := 0 to AList.Count - 1 do
  begin
    if TObject(AList[I]) is TComponent then
      TComponent(AList[I]).FreeNotification(Self);
  end;
end;

procedure TdxRibbonBackstageViewDesignWindow.DoRemoveFreeNotification(AList: TList);
var
  I: Integer;
begin
  for I := 0 to AList.Count - 1 do
  begin
    if TObject(AList[I]) is TComponent then
      TComponent(AList[I]).RemoveFreeNotification(Self);
  end;
end;

procedure TdxRibbonBackstageViewDesignWindow.DoSelectionChanged(AList: TList);
var
  ASelectableItem: IdxRibbonBackstageViewSelectableItem;
  I: Integer;
begin
  for I := 0 to AList.Count - 1 do
  begin
    if Supports(TObject(AList[I]), IdxRibbonBackstageViewSelectableItem, ASelectableItem) then
      ASelectableItem.SelectionChanged;
  end;
end;

procedure TdxRibbonBackstageViewDesignWindow.Notification(
  AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
    FCurrentSelection.Remove(AComponent)
end;

procedure TdxRibbonBackstageViewDesignWindow.ItemDeleted(const ADesigner: IDesigner; Item: TPersistent);
begin
  FCurrentSelection.Remove(Item);
end;

procedure TdxRibbonBackstageViewDesignWindow.SelectionsChanged(const ASelection: IDesignerSelections);
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

initialization
  FOnRegisterBackstageView := RegisterBackstageView;
  FOnUnregisterBackstageView := UnregisterBackstageView;

end.
