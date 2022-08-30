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

unit cxGridLayoutViewCustomizationForm;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, StdCtrls, ComCtrls, Forms, ImgList,
  ActnList, Dialogs, Menus,
  dxCore, cxGraphics, cxControls, cxLookAndFeels, cxCheckBox, cxButtons, cxTreeView,
  cxLookAndFeelPainters, cxContainer, cxEdit, dxLayoutPainters, dxLayoutCommon,
  dxLayoutCustomizeForm, dxLayoutControlAdapters, dxLayoutContainer, dxLayoutcxEditAdapters,
  dxLayoutControl, cxGridLayoutView, cxGridCustomView, dxLayoutLookAndFeels,
  cxStyles, cxClasses, cxGridLevel, cxGrid, cxGridViewLayoutCustomizationForm, cxGridViewLayoutContainer;

type
  TcxGridLayoutViewCustomizationForm = class;
  TcxGridLayoutViewCustomizationFormLayoutLookAndFeel = class;

  { TcxGridLayoutViewCustomizationFormGroupPainter }

  TcxGridLayoutViewCustomizationFormGroupPainter = class(TcxGridViewLayoutCustomizationFormGroupPainter);

  { TcxGridLayoutViewCustomizationFormLayoutLookAndFeel }

  TcxGridLayoutViewCustomizationFormLayoutLookAndFeel = class(TcxGridViewLayoutCustomizationFormLayoutLookAndFeel)
  public
    function GetGroupPainterClass: TClass; override;
  end;

  { TcxGridViewLayoutContainer }

  TcxGridViewLayoutContainer = class(TcxGridViewLayoutCustomizationFormContainer)
  protected
    function GetCloneItemClass: TcxGridCustomLayoutItemClass; override;
  end;

  { TcxGridViewLayoutControl }

  TcxGridViewLayoutControl = class(TcxGridViewLayoutCustomizationFormLayoutControl)
  private
    function GetContainer: TcxGridViewLayoutContainer;
    function GetLayoutLookAndFeel: TcxGridLayoutLookAndFeel;
    procedure SetLayoutLookAndFeel(Value: TcxGridLayoutLookAndFeel);
  protected
    function GetContainerClass: TdxLayoutControlContainerClass; override;
  public
    property Container: TcxGridViewLayoutContainer read GetContainer;
  published
    property LayoutLookAndFeel: TcxGridLayoutLookAndFeel read GetLayoutLookAndFeel write SetLayoutLookAndFeel;
  end;

  { TcxGridLayoutViewCustomizationForm }

  TcxGridLayoutViewCustomizationForm = class(TcxGridViewLayoutCustomizationForm)
    lcMainTemplateCardGroup: TdxLayoutGroup;
    lcMainTabbedGroup: TdxLayoutGroup;
    lcMainViewLayoutGroup: TdxLayoutGroup;
    gMain: TcxGrid;
    lcMainItem16: TdxLayoutItem;
    cbSaveData: TcxCheckBox;
    liSaveData: TdxLayoutItem;
    acUseHorizontalScrolling: TAction;
    acUseVerticalScrolling: TAction;
    miGroupScrolling: TMenuItem;
    miHorizontalScrolling: TMenuItem;
    miVerticalScrolling: TMenuItem;
    btnConditionalFormatting: TcxButton;
    liConditionalFormatting: TdxLayoutItem;
    procedure lcMainTabbedGroupTabChanged(Sender: TObject);
    procedure acUseScrollingExecute(Sender: TObject);
    procedure btnConditionalFormattingClick(Sender: TObject);
  private
    FPreviewView: TcxGridLayoutView;

    function GetController: TcxGridLayoutViewController;
    function GetDataControllerSupport: IcxCustomGridDataController;
    function GetGridView: TcxGridLayoutView;
    function GetGridViewLayoutControl: TcxGridViewLayoutControl;
    function GetGroupScrollMode(AUseScrolling: Boolean): TdxLayoutGroupScrollMode;
    function GetUseScrolling(AScrollMode: TdxLayoutGroupScrollMode): Boolean;
  protected
    procedure Load; override;
    procedure Save; override;

    procedure CalculateTreeViewPopupActionVisibilities; override;
    function CheckControlOKVisible: Boolean; override;
    procedure CheckControlVisible; override;
    procedure CreatePreviewView; override;
    procedure DestroyPreviewView; override;
    procedure DoInitializeControl; override;
    function GetGridViewContainerInstance: TdxLayoutContainer; override;
    function GetGridViewLayoutControlClass: TcxGridViewLayoutCustomizationFormLayoutControlClass; override;
    function GetGridViewLayoutLookAndFeel: TdxLayoutCxLookAndFeel; override;
    function GetLayoutLookAndFeelClass: TcxGridViewLayoutCustomizationFormLayoutLookAndFeelClass; override;
    function IsDataChangeable: Boolean;
    function IsLayoutChangeable: Boolean;
    procedure Localize; override;
    procedure RefreshEnableds; override;
    procedure SynchronizeTreeViewPopupActionStates; override;

    property PreviewView: TcxGridLayoutView read FPreviewView;
  public
    procedure ApplyChanges; override;

    property Controller: TcxGridLayoutViewController read GetController;
    property DataControllerSupport: IcxCustomGridDataController read GetDataControllerSupport;
    property GridView: TcxGridLayoutView read GetGridView;
    property GridViewLayoutControl: TcxGridViewLayoutControl read GetGridViewLayoutControl;
  end;

implementation

{$R *.dfm}

uses
  cxGeometry, cxGridStrs, dxComCtrlsUtils, cxDataControllerConditionalFormatting;

type
  TcxGridLayoutViewAccess = class(TcxGridLayoutView);
  TdxLayoutGroupAccess = class(TdxCustomLayoutGroup);

{ TcxGridLayoutViewCustomizationFormLayoutLookAndFeel }

function TcxGridLayoutViewCustomizationFormLayoutLookAndFeel.GetGroupPainterClass: TClass;
begin
  Result := TcxGridLayoutViewCustomizationFormGroupPainter;
end;

{ TcxGridViewLayoutContainer }

function TcxGridViewLayoutContainer.GetCloneItemClass: TcxGridCustomLayoutItemClass;
begin
  Result := TcxGridBaseLayoutItem;
end;

{ TcxGridViewLayoutControl }

function TcxGridViewLayoutControl.GetContainerClass: TdxLayoutControlContainerClass;
begin
  Result := TcxGridViewLayoutContainer;
end;

function TcxGridViewLayoutControl.GetContainer: TcxGridViewLayoutContainer;
begin
  Result := TcxGridViewLayoutContainer(inherited Container);
end;

function TcxGridViewLayoutControl.GetLayoutLookAndFeel: TcxGridLayoutLookAndFeel;
begin
  Result := TcxGridLayoutLookAndFeel(inherited LayoutLookAndFeel);
end;

procedure TcxGridViewLayoutControl.SetLayoutLookAndFeel(Value: TcxGridLayoutLookAndFeel);
begin
  inherited LayoutLookAndFeel := Value;
end;

{ TcxGridLayoutViewCustomizationForm }

procedure TcxGridLayoutViewCustomizationForm.acUseScrollingExecute(Sender: TObject);
var
  I: Integer;
  AUseScrolling, AIsVerticalScroll: Boolean;
  ASender: TAction;
  AGroup: TdxLayoutGroupAccess;
  AList: TList;
begin
  if tvVisibleItems.IsEditing then
    Exit;
  SaveToUndo;
  ASender := TAction(Sender);
  ASender.Checked := not ASender.Checked;
  AUseScrolling := ASender.Checked;
  AIsVerticalScroll := ASender.Tag = 0;
  AList := TList.Create;
  try
    cxTreeViewGetSelection(tvVisibleItems.InnerTreeView, AList);
    for I := 0 to AList.Count - 1 do
    begin
      AGroup := TdxLayoutGroupAccess(AList[I]);
      if AIsVerticalScroll then
        AGroup.ScrollOptions.Vertical := GetGroupScrollMode(AUseScrolling)
      else
        AGroup.ScrollOptions.Horizontal := GetGroupScrollMode(AUseScrolling);
    end;
    Container.Modified;
  finally
    AList.Free;
  end;
  tvVisibleItems.Invalidate;
end;

procedure TcxGridLayoutViewCustomizationForm.ApplyChanges;
begin
  inherited ApplyChanges;
  if DataControllerSupport.IsDataChangeable and cbSaveData.Checked then
    DataControllerSupport.AssignData(PreviewView.DataController);
end;

procedure TcxGridLayoutViewCustomizationForm.btnConditionalFormattingClick(
  Sender: TObject);
var
  AIntf: IcxDataControllerConditionalFormattingProviderOwner;
begin
  AIntf := PreviewView as IcxDataControllerConditionalFormattingProviderOwner;
  if (AIntf <> nil) and (AIntf.GetConditionalFormattingProvider <> nil) then
    AIntf.GetConditionalFormattingProvider.ConditionalFormatting.ShowRulesManagerDialog;
end;

procedure TcxGridLayoutViewCustomizationForm.Load;
begin
  GridView.BeginUpdate(lsimNever);
  try
    PreviewView.BeginUpdate;
    try
      PreviewView.Assign(GridView);
      (PreviewView.DataController as IcxCustomGridDataController).AssignData(GridView.DataController);
    finally
      PreviewView.CancelUpdate;
    end;
    ViewSupport.BeforeEditLayout(PreviewView);
  finally
    GridView.EndUpdate;
  end;
  GridViewLayoutControl.OptionsImage.Images := PreviewView.Images;
  GridViewLayoutControl.Container.CustomizationHelper.CopyStructure(PreviewView.Container);
end;

procedure TcxGridLayoutViewCustomizationForm.Save;
begin
  PreviewView.BeginUpdate;
  try
    TcxGridLayoutViewAccess(PreviewView).CopyLayoutStructure(Container);
    if IsLayoutChangeable then
      TcxGridLayoutViewAccess(GridView).AssignLayout(PreviewView);
  finally
    PreviewView.CancelUpdate;
  end;
end;

procedure TcxGridLayoutViewCustomizationForm.CalculateTreeViewPopupActionVisibilities;
begin
  inherited CalculateTreeViewPopupActionVisibilities;
  miGroupScrolling.Visible := GridView.IsDesigning and HasGroupsOnly and not HasRootInSelection;
end;

function TcxGridLayoutViewCustomizationForm.CheckControlOKVisible: Boolean;
begin
  Result := inherited CheckControlOKVisible or DataControllerSupport.IsDataChangeable;
end;

procedure TcxGridLayoutViewCustomizationForm.CheckControlVisible;
var
  AIntf: IcxDataControllerConditionalFormattingProviderOwner;
begin
  inherited CheckControlVisible;
  liSaveData.Visible := GridView.IsDesigning and IsDataChangeable;
  liConditionalFormatting.Visible := liSaveLayout.Visible and Supports(GridView, IcxDataControllerConditionalFormattingProviderOwner, AIntf) and
    (AIntf.GetConditionalFormattingProvider <> nil) and
    AIntf.GetConditionalFormattingProvider.ConditionalFormatting.CanShowRulesManagerDialog;
end;

procedure TcxGridLayoutViewCustomizationForm.CreatePreviewView;
begin
  FPreviewView := gMain.CreateView(TcxCustomGridViewClass(GridView.ClassType)) as TcxGridLayoutView;
  FPreviewView.Name := 'Preview';
  gMain.Levels.Add.GridView := FPreviewView;
end;

procedure TcxGridLayoutViewCustomizationForm.DestroyPreviewView;
begin
  FreeAndNil(FPreviewView);
end;

procedure TcxGridLayoutViewCustomizationForm.DoInitializeControl;
begin
  inherited DoInitializeControl;
  if not GridView.IsDesigning then
  begin
    PreviewView.BeginUpdate;
    try
      PreviewView.Navigator.Visible := False;
      PreviewView.OptionsData.Editing := False;
      PreviewView.OptionsData.Inserting := False;
      PreviewView.OptionsData.Appending := False;
      PreviewView.OptionsData.Deleting := False;
    finally
      PreviewView.CancelUpdate;
    end;
  end
  else
    Caption := 'Layout and Data Editor - ' + GridView.Name;
end;

function TcxGridLayoutViewCustomizationForm.GetGridViewContainerInstance: TdxLayoutContainer;
begin
  Result := GridView.Container;
end;

function TcxGridLayoutViewCustomizationForm.GetGridViewLayoutControlClass: TcxGridViewLayoutCustomizationFormLayoutControlClass;
begin
  Result := TcxGridViewLayoutControl;
end;

function TcxGridLayoutViewCustomizationForm.GetGridViewLayoutLookAndFeel: TdxLayoutCxLookAndFeel;
begin
  Result := TcxGridLayoutViewAccess(GridView).LayoutLookAndFeel;
end;

function TcxGridLayoutViewCustomizationForm.GetLayoutLookAndFeelClass: TcxGridViewLayoutCustomizationFormLayoutLookAndFeelClass;
begin
  Result := TcxGridLayoutViewCustomizationFormLayoutLookAndFeel;
end;

function TcxGridLayoutViewCustomizationForm.IsDataChangeable: Boolean;
begin
  Result := GridView.IsDesigning and DataControllerSupport.IsDataChangeable;
end;

function TcxGridLayoutViewCustomizationForm.IsLayoutChangeable: Boolean;
begin
  Result := ViewSupport.IsLayoutChangeable and
    (not GridView.IsDesigning or cbSaveLayout.Checked);
end;

procedure TcxGridLayoutViewCustomizationForm.Localize;
begin
  inherited Localize;
  lcMainTemplateCardGroup.Caption := cxGetResourceString(@scxGridLayoutViewCustomizeFormTemplateCard);
  lcMainViewLayoutGroup.Caption := cxGetResourceString(@scxGridLayoutViewCustomizeFormViewLayout);
end;

procedure TcxGridLayoutViewCustomizationForm.RefreshEnableds;
begin
  inherited RefreshEnableds;
  miGroupScrolling.Enabled := CanModify;
  acUseHorizontalScrolling.Enabled := CanModify;
  acUseVerticalScrolling.Enabled := CanModify;
end;

procedure TcxGridLayoutViewCustomizationForm.SynchronizeTreeViewPopupActionStates;
var
  AGroup: TdxCustomLayoutGroup;
begin
  inherited SynchronizeTreeViewPopupActionStates;
  if (tvVisibleItems.Selected <> nil) and (TObject(tvVisibleItems.Selected.Data) is TdxCustomLayoutGroup) then
  begin
    AGroup := TdxCustomLayoutGroup(tvVisibleItems.Selected.Data);
    acUseHorizontalScrolling.Checked := GetUseScrolling(TdxLayoutGroupAccess(AGroup).ScrollOptions.Horizontal);
    acUseVerticalScrolling.Checked := GetUseScrolling(TdxLayoutGroupAccess(AGroup).ScrollOptions.Vertical);
  end;
end;

function TcxGridLayoutViewCustomizationForm.GetController: TcxGridLayoutViewController;
begin
  Result := TcxGridLayoutViewController(inherited Controller);
end;

function TcxGridLayoutViewCustomizationForm.GetDataControllerSupport: IcxCustomGridDataController;
begin
  Result := GridView.DataController as IcxCustomGridDataController;
end;

function TcxGridLayoutViewCustomizationForm.GetGridView: TcxGridLayoutView;
begin
  Result := TcxGridLayoutView(inherited GridView);
end;

function TcxGridLayoutViewCustomizationForm.GetGridViewLayoutControl: TcxGridViewLayoutControl;
begin
  Result := TcxGridViewLayoutControl(inherited GridViewLayoutControl);
end;

function TcxGridLayoutViewCustomizationForm.GetGroupScrollMode(AUseScrolling: Boolean): TdxLayoutGroupScrollMode;
begin
  if AUseScrolling then
    Result := smAuto
  else
    Result := smNone;
end;

function TcxGridLayoutViewCustomizationForm.GetUseScrolling(AScrollMode: TdxLayoutGroupScrollMode): Boolean;
begin
  Result := AScrollMode <> smNone;
end;

procedure TcxGridLayoutViewCustomizationForm.lcMainTabbedGroupTabChanged(
  Sender: TObject);
begin
  inherited;
  if (lcMainTabbedGroup.ItemIndex = 1) and HasChanges then
    TcxGridLayoutViewAccess(PreviewView).CopyLayoutStructure(Container);
end;

end.
