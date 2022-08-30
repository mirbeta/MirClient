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

unit cxGridViewLayoutCustomizationForm;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, StdCtrls, ComCtrls, Forms, ImgList,
  ActnList, Dialogs, Menus,
  dxCore, cxGraphics, cxControls, cxLookAndFeels, cxCheckBox, cxButtons, cxTreeView,
  cxLookAndFeelPainters, cxContainer, cxEdit, dxLayoutPainters, dxLayoutCommon,
  dxLayoutCustomizeForm, dxLayoutControlAdapters, dxLayoutContainer, dxLayoutcxEditAdapters,
  dxLayoutControl, cxGridCustomView, dxLayoutLookAndFeels,
  cxStyles, cxClasses, cxGridLevel, cxGrid, cxGridViewLayoutContainer, cxGridCustomTableView;

type
  TcxGridViewLayoutCustomizationFormLayoutLookAndFeel = class;

  { TcxGridViewLayoutCustomizationFormRootViewInfo }

  TcxGridViewLayoutCustomizationFormRootViewInfo = class(TdxLayoutRootViewInfo)
  private
    function GetGridLayoutViewLayoutLookAndFeel: TcxGridViewLayoutCustomizationFormLayoutLookAndFeel;
    function GetRootOffset: Integer;
  protected
    function CalculateOffset(ASide: TdxLayoutSide): Integer; override;
    function CalculatePadding: TRect; override;
    function GetSelectionBoundsOffset: Integer; override;
    function HasCaption: Boolean; override;

    property LayoutLookAndFeel: TcxGridViewLayoutCustomizationFormLayoutLookAndFeel read GetGridLayoutViewLayoutLookAndFeel;
  end;

  { TcxGridLayoutViewCustomizationFormGroupPainter }

  TcxGridViewLayoutCustomizationFormGroupPainter = class(TdxLayoutGroupCxLookAndFeelPainter)
  protected
    function CanDrawBorders: Boolean; override;
    function CanDrawCaption: Boolean; override;
  end;

  { TcxGridLayoutViewCustomizationFormLayoutLookAndFeel }

  TcxGridViewLayoutCustomizationFormLayoutLookAndFeel = class(TcxGridCustomLayoutLookAndFeel)
  public
    function GetGroupPainterClass: TClass; override;
  end;

  TcxGridViewLayoutCustomizationFormLayoutLookAndFeelClass = class of TcxGridViewLayoutCustomizationFormLayoutLookAndFeel;

  TcxGridViewLayoutCustomizationFormContainerViewInfo = class(TdxLayoutControlContainerViewInfo)
  protected
    function GetRootViewInfoClass: TdxLayoutRootViewInfoClass; override;
  end;

  { TcxGridViewLayoutContainer }

  TcxGridViewLayoutCustomizationFormContainer = class(TdxLayoutControlContainer)
  private
    FIsModified: Boolean;
  private
    function GetCustomizationHelper: TcxGridLayoutContainerCustomizationHelper;
  protected
    function CalculateCustomizeFormBounds(const AFormBounds: TRect): TRect; override;
    procedure CreateCustomizeForm; override;
    function GetViewInfoClass: TdxLayoutContainerViewInfoClass; override;
    function GetCloneItemClass: TcxGridCustomLayoutItemClass; virtual;
    function GetCustomizeForm: TdxLayoutControlCustomCustomizeForm; override;
    function GetCustomizationHelperClass: TdxLayoutContainerCustomizationHelperClass; override;
    procedure DestroyCustomizeForm; override;
    procedure ShowCustomizeForm; override;

    property CustomizationHelper: TcxGridLayoutContainerCustomizationHelper read GetCustomizationHelper;
    property IsModified: Boolean read FIsModified;
  public
    function CloneItem(AItem: TdxCustomLayoutItem; AParent: TdxCustomLayoutGroup = nil): TdxCustomLayoutItem; override;
    procedure Modified; override;
  end;

  { TcxGridViewLayoutControl }

  TcxGridViewLayoutCustomizationFormLayoutControl = class(TdxLayoutControl)
  private
    function GetContainer: TcxGridViewLayoutCustomizationFormContainer;
  protected
    function GetContainerClass: TdxLayoutControlContainerClass; override;
  public
    constructor Create(AOwner: TComponent); override;

    property Container: TcxGridViewLayoutCustomizationFormContainer read GetContainer;
  end;

  TcxGridViewLayoutCustomizationFormLayoutControlClass = class of TcxGridViewLayoutCustomizationFormLayoutControl;

  { TcxGridLayoutViewCustomizationForm }

  TcxGridViewLayoutCustomizationForm = class(TdxLayoutControlCustomizeForm,
    IcxGridCustomizationForm,
    IcxDialogMetricsInfoData
  )
    liGridViewContainer: TdxLayoutItem;
    lcMainGroup5: TdxLayoutGroup;
    dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList;
    llfMain: TdxLayoutCxLookAndFeel;
    acCancel: TAction;
    acOk: TAction;
    btnOk: TcxButton;
    liOk: TdxLayoutItem;
    btnCancel: TcxButton;
    liCancel: TdxLayoutItem;
    lcMainGroup4: TdxLayoutGroup;
    cbSaveLayout: TcxCheckBox;
    liSaveLayout: TdxLayoutItem;
    procedure acOkExecute(Sender: TObject);
    procedure acCancelExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormShortCut(var Msg: TWMKey; var Handled: Boolean);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FController: TcxCustomGridController;
    FGridViewLayoutControl: TcxGridViewLayoutCustomizationFormLayoutControl;
    FLayoutLookAndFeel: TcxGridViewLayoutCustomizationFormLayoutLookAndFeel;
    FTreeViewWidth: Integer;

    function GetContainer: TcxGridViewLayoutCustomizationFormContainer;
    function GetGridView: TcxCustomGridView;
    function GetGridViewContainer: TdxLayoutContainer;
    function GetViewSupport: IcxGridViewLayoutEditorSupport;
    procedure SetContainer(const Value: TcxGridViewLayoutCustomizationFormContainer);
  protected
    //IcxDialogMetricsInfoData
    function GetInfoData: Pointer; virtual;
    function GetInfoDataSize: Integer; virtual;
    procedure SetInfoData(AData: Pointer); virtual;

    // IcxGridCustomizationForm
    function GetController: TcxCustomGridController;
    procedure GridViewChanged;
    procedure Initialize(AController: TcxCustomGridController); reintroduce;
    procedure RefreshData;

    procedure Load; virtual;
    procedure Save; virtual;

    procedure CalculateTreeViewPopupActionVisibilities; override;
    function CheckControlOKVisible: Boolean; virtual;
    procedure CheckControlVisible; virtual;
    procedure CreateControls; virtual;
    procedure CreateLayoutLookAndFeel; virtual;
    procedure CreatePreviewView; virtual;
    procedure DestroyPreviewView; virtual;
    procedure DestroyLayoutLookAndFeel; virtual;
    procedure DoInitializeControl; override;
    function GetGridViewContainerInstance: TdxLayoutContainer; virtual; abstract;
    function GetGridViewLayoutControlClass: TcxGridViewLayoutCustomizationFormLayoutControlClass; virtual;
    function GetGridViewLayoutLookAndFeel: TdxLayoutCxLookAndFeel; virtual; abstract;
    function GetLayoutLookAndFeelClass: TcxGridViewLayoutCustomizationFormLayoutLookAndFeelClass; virtual;
    function GetWndParent: THandle; override;
    function HasChanges: Boolean;
    procedure InitializeGridViewLayout; virtual;
    function IsLayoutChangeable: Boolean;
    procedure Loaded; override;
    procedure Localize; override;
    procedure RefreshLayoutLookAndFeel; override;
    procedure RefreshStoring; override;
    procedure RepaintGridView; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ApplyChanges; virtual;
    procedure CancelChanges; virtual;
    function GetHitTest(const P: TPoint): TdxCustomLayoutHitTest; override;

    property Container: TcxGridViewLayoutCustomizationFormContainer read GetContainer write SetContainer;
    property Controller: TcxCustomGridController read FController;
    property GridView: TcxCustomGridView read GetGridView;
    property GridViewContainer: TdxLayoutContainer read GetGridViewContainer;
    property GridViewLayoutControl: TcxGridViewLayoutCustomizationFormLayoutControl read FGridViewLayoutControl;
    property ViewSupport: IcxGridViewLayoutEditorSupport read GetViewSupport;
  end;

implementation

{$R *.dfm}

uses
  cxGeometry, cxGridStrs;

type
  TdxLayoutContainerAccess = class(TdxLayoutContainer);
  TcxCustomGridControllerAccess = class(TcxCustomGridController);
  TdxCustomLayoutItemViewInfoAccess = class(TdxCustomLayoutItemViewInfo);

{ TcxGridViewLayoutCustomizationFormRootViewInfo }

function TcxGridViewLayoutCustomizationFormRootViewInfo.CalculateOffset(ASide: TdxLayoutSide): Integer;
begin
  Result := inherited CalculateOffset(ASide) + GetRootOffset;
end;

function TcxGridViewLayoutCustomizationFormRootViewInfo.CalculatePadding: TRect;
begin
  Result := inherited CalculatePadding;
  Inc(Result.Top, CaptionViewInfo.Height);
end;

function TcxGridViewLayoutCustomizationFormRootViewInfo.GetGridLayoutViewLayoutLookAndFeel: TcxGridViewLayoutCustomizationFormLayoutLookAndFeel;
begin
  Result := TcxGridViewLayoutCustomizationFormLayoutLookAndFeel(inherited LayoutLookAndFeel);
end;

function TcxGridViewLayoutCustomizationFormRootViewInfo.GetRootOffset: Integer;
begin
  Result := LayoutLookAndFeel.DLUToPixels(LayoutLookAndFeel.GetGroupCaptionFont(Group.Container), 7);
end;

function TcxGridViewLayoutCustomizationFormRootViewInfo.GetSelectionBoundsOffset: Integer;
begin
  Result := - inherited GetSelectionBoundsOffset;
end;

function TcxGridViewLayoutCustomizationFormRootViewInfo.HasCaption: Boolean;
begin
  Result := True;
end;

{ TcxGridViewLayoutCustomizationFormGroupPainter }

function TcxGridViewLayoutCustomizationFormGroupPainter.CanDrawBorders: Boolean;
begin
  Result := inherited CanDrawBorders or TdxCustomLayoutItemViewInfoAccess(ViewInfo).Item.IsRoot;
end;

function TcxGridViewLayoutCustomizationFormGroupPainter.CanDrawCaption: Boolean;
begin
  Result := inherited CanDrawCaption or TdxCustomLayoutItemViewInfoAccess(ViewInfo).Item.IsRoot;
end;

function TcxGridViewLayoutCustomizationFormContainerViewInfo.GetRootViewInfoClass: TdxLayoutRootViewInfoClass;
begin
  Result := TcxGridViewLayoutCustomizationFormRootViewInfo;
end;

{ TcxGridViewLayoutCustomizationFormLayoutLookAndFeel }

function TcxGridViewLayoutCustomizationFormLayoutLookAndFeel.GetGroupPainterClass: TClass;
begin
  Result := TcxGridViewLayoutCustomizationFormGroupPainter;
end;

{ TcxGridViewCustomLayoutContainer }

function TcxGridViewLayoutCustomizationFormContainer.CloneItem(
  AItem: TdxCustomLayoutItem; AParent: TdxCustomLayoutGroup = nil): TdxCustomLayoutItem;
begin
  if AItem is TcxGridCustomLayoutItem then
  begin
    Result := CreateItem(GetCloneItemClass, AParent);
    Result.Assign(AItem);
  end
  else
    Result := inherited CloneItem(AItem, AParent);
end;

procedure TcxGridViewLayoutCustomizationFormContainer.Modified;
begin
  inherited Modified;
  FIsModified := True;
end;

function TcxGridViewLayoutCustomizationFormContainer.CalculateCustomizeFormBounds(
  const AFormBounds: TRect): TRect;
begin
  Result := AFormBounds;
end;

procedure TcxGridViewLayoutCustomizationFormContainer.CreateCustomizeForm;
begin
// do nothing
end;

function TcxGridViewLayoutCustomizationFormContainer.GetViewInfoClass: TdxLayoutContainerViewInfoClass;
begin
  Result := TcxGridViewLayoutCustomizationFormContainerViewInfo;
end;

function TcxGridViewLayoutCustomizationFormContainer.GetCloneItemClass: TcxGridCustomLayoutItemClass;
begin
  Result := TcxGridCustomLayoutItem;
end;

function TcxGridViewLayoutCustomizationFormContainer.GetCustomizeForm: TdxLayoutControlCustomCustomizeForm;
var
  AParent: TWinControl;
begin
  Result := nil;
  AParent := ItemsParentControl;
  while AParent <> nil do
  begin
    if AParent is TdxLayoutControlCustomCustomizeForm then
    begin
      Result := TdxLayoutControlCustomCustomizeForm(AParent);
      Break;
    end;
    AParent := AParent.Parent;
  end;
end;

function TcxGridViewLayoutCustomizationFormContainer.GetCustomizationHelperClass: TdxLayoutContainerCustomizationHelperClass;
begin
  Result := TcxGridLayoutContainerCustomizationHelper;
end;

function TcxGridViewLayoutCustomizationFormContainer.GetCustomizationHelper: TcxGridLayoutContainerCustomizationHelper;
begin
  Result := inherited CustomizationHelper as TcxGridLayoutContainerCustomizationHelper;
end;

procedure TcxGridViewLayoutCustomizationFormContainer.DestroyCustomizeForm;
begin
// do nothing
end;

procedure TcxGridViewLayoutCustomizationFormContainer.ShowCustomizeForm;
begin
// do nothing
end;

{ TcxGridViewCustomLayoutControl }

constructor TcxGridViewLayoutCustomizationFormLayoutControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CustomizeFormTabbedView := True;
  BevelKind := bkTile;
end;

function TcxGridViewLayoutCustomizationFormLayoutControl.GetContainerClass: TdxLayoutControlContainerClass;
begin
  Result := TcxGridViewLayoutCustomizationFormContainer;
end;

function TcxGridViewLayoutCustomizationFormLayoutControl.GetContainer: TcxGridViewLayoutCustomizationFormContainer;
begin
  Result := TcxGridViewLayoutCustomizationFormContainer(inherited Container)
end;

{ TcxGridLayoutCustomizationForm }

constructor TcxGridViewLayoutCustomizationForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CreateLayoutLookAndFeel;
end;

destructor TcxGridViewLayoutCustomizationForm.Destroy;
begin
  Container := nil;
  DestroyPreviewView;
  FreeAndNil(FGridViewLayoutControl);
  DestroyLayoutLookAndFeel;
  inherited Destroy;
end;

procedure TcxGridViewLayoutCustomizationForm.ApplyChanges;
begin
  if HasChanges then
  begin
    GridView.BeginUpdate;
    try
      Save;
      Container.UndoRedoManager.Clear;
      RefreshEnableds;
      RepaintGridView;
    finally
      GridView.EndUpdate;
    end;
  end
end;

procedure TcxGridViewLayoutCustomizationForm.CancelChanges;
begin
//do nothing
end;

function TcxGridViewLayoutCustomizationForm.GetHitTest(const P: TPoint): TdxCustomLayoutHitTest;
begin
  if PtInRect(FGridViewLayoutControl.BoundsRect, P) then
    Result := nil
  else
    Result := inherited GetHitTest(P);
end;

function TcxGridViewLayoutCustomizationForm.GetInfoData: Pointer;
begin
  FTreeViewWidth := lcMainGroup1.Width;
  Result := @FTreeViewWidth;
end;

function TcxGridViewLayoutCustomizationForm.GetInfoDataSize: Integer;
begin
  Result := SizeOf(FTreeViewWidth);
end;

procedure TcxGridViewLayoutCustomizationForm.SetContainer(const Value: TcxGridViewLayoutCustomizationFormContainer);
begin
  inherited Container := Value;
end;

procedure TcxGridViewLayoutCustomizationForm.SetInfoData(AData: Pointer);
begin
  FTreeViewWidth := PInteger(AData)^;
  lcMainGroup1.Width := FTreeViewWidth;
end;

function TcxGridViewLayoutCustomizationForm.GetController: TcxCustomGridController;
begin
  Result := FController;
end;

procedure TcxGridViewLayoutCustomizationForm.GridViewChanged;
begin
end;

procedure TcxGridViewLayoutCustomizationForm.Initialize(AController: TcxCustomGridController);
begin
  FController := AController;
  TcxCustomGridControllerAccess(AController).InitializeCustomizationForm(Self);
  CreateControls;
  inherited Initialize;
end;

procedure TcxGridViewLayoutCustomizationForm.RefreshData;
begin
end;

procedure TcxGridViewLayoutCustomizationForm.Load;
begin
//do nothing
end;

procedure TcxGridViewLayoutCustomizationForm.Save;
begin
//do nothing
end;

procedure TcxGridViewLayoutCustomizationForm.CalculateTreeViewPopupActionVisibilities;
var
  AHasRootInSelection: Boolean;
begin
  inherited;
  AHasRootInSelection := HasRootInSelection;
  miAlignHorz.Visible := miAlignHorz.Visible and not AHasRootInSelection;
  miAlignVert.Visible := miAlignVert.Visible and not AHasRootInSelection;
end;

function TcxGridViewLayoutCustomizationForm.CheckControlOKVisible: Boolean;
begin
  Result := ViewSupport.IsLayoutChangeable;
end;

procedure TcxGridViewLayoutCustomizationForm.CheckControlVisible;
begin
  acAddSplitter.Visible := False;
  liOK.Visible := CheckControlOKVisible;
  liSaveLayout.Visible := GridView.IsDesigning and IsLayoutChangeable;
  if not liOK.Visible then
  begin
    acCancel.Caption := 'Close';
    acCancel.Hint := StripHotKey(acCancel.Caption);
    btnCancel.Default := True;
  end
  else
    btnOk.Default := True;
end;

procedure TcxGridViewLayoutCustomizationForm.CreateControls;
begin
  InitializeGridViewLayout;
end;

procedure TcxGridViewLayoutCustomizationForm.CreateLayoutLookAndFeel;
begin
  FLayoutLookAndFeel := TcxGridViewLayoutCustomizationFormLayoutLookAndFeel(
    dxLayoutLookAndFeelList1.CreateItem(GetLayoutLookAndFeelClass));
end;

procedure TcxGridViewLayoutCustomizationForm.CreatePreviewView;
begin
//do nothing
end;

procedure TcxGridViewLayoutCustomizationForm.DestroyPreviewView;
begin
//do nothing
end;

procedure TcxGridViewLayoutCustomizationForm.DestroyLayoutLookAndFeel;
begin
  FreeAndNil(FLayoutLookAndFeel);
end;

procedure TcxGridViewLayoutCustomizationForm.DoInitializeControl;
begin
  inherited DoInitializeControl;
  CheckControlVisible;
  if Container <> nil then
    Container.Root.Caption := cxGetResourceString(@scxGridLayoutViewCustomizeFormTemplateCard);
end;

function TcxGridViewLayoutCustomizationForm.GetGridViewContainer: TdxLayoutContainer;
begin
  Result := GetGridViewContainerInstance;
end;

function TcxGridViewLayoutCustomizationForm.GetGridViewLayoutControlClass: TcxGridViewLayoutCustomizationFormLayoutControlClass;
begin
  Result := TcxGridViewLayoutCustomizationFormLayoutControl;
end;

function TcxGridViewLayoutCustomizationForm.GetLayoutLookAndFeelClass: TcxGridViewLayoutCustomizationFormLayoutLookAndFeelClass;
begin
  Result := TcxGridViewLayoutCustomizationFormLayoutLookAndFeel;
end;

function TcxGridViewLayoutCustomizationForm.GetWndParent: THandle;
begin
  if Parent <> nil then
    Result := Parent.Handle
  else
    if (Controller = nil) or (GridView.Control = nil) then
      Result := 0
    else
      Result := GridView.Control.Handle;
end;

function TcxGridViewLayoutCustomizationForm.HasChanges: Boolean;
begin
  Result := Container.IsModified;
end;

procedure TcxGridViewLayoutCustomizationForm.InitializeGridViewLayout;
begin
  FGridViewLayoutControl := GetGridViewLayoutControlClass.Create(Self);
  FGridViewLayoutControl.Name := 'LayoutControl';
  FGridViewLayoutControl.OnKeyDown := FormKeyDown;
  FGridViewLayoutControl.MenuItems := TdxLayoutContainerAccess(GridViewContainer).MenuItems;

  FGridViewLayoutControl.OptionsItem.SizableHorz := TdxLayoutContainerAccess(GridViewContainer).IsSizableHorz;
  FGridViewLayoutControl.OptionsItem.SizableVert := TdxLayoutContainerAccess(GridViewContainer).IsSizableVert;

  CreatePreviewView;

  liGridViewContainer.Control := FGridViewLayoutControl;
  liGridViewContainer.SizeOptions.SizableHorz := True;
  liGridViewContainer.SizeOptions.SizableVert := True;

  Load;

  Container := FGridViewLayoutControl.Container;
  Container.Customization := True;
end;

function TcxGridViewLayoutCustomizationForm.IsLayoutChangeable: Boolean;
begin
  Result := ViewSupport.IsLayoutChangeable and
    (not GridView.IsDesigning or cbSaveLayout.Checked);
end;

procedure TcxGridViewLayoutCustomizationForm.Loaded;
begin
  inherited;
  Constraints.MinWidth := Width div 2;
end;

procedure TcxGridViewLayoutCustomizationForm.Localize;
begin
  inherited Localize;
  acCancel.Caption := cxGetResourceString(@scxGridLayoutViewCustomizeFormCancel);
  acCancel.Hint := StripHotKey(acCancel.Caption);
  acOk.Caption := cxGetResourceString(@scxGridLayoutViewCustomizeFormOk);
  acOk.Hint := StripHotKey(acOk.Caption);
end;

procedure TcxGridViewLayoutCustomizationForm.RefreshLayoutLookAndFeel;
begin
  FLayoutLookAndFeel.Assign(GetGridViewLayoutLookAndFeel);
  GridViewLayoutControl.LayoutLookAndFeel := FLayoutLookAndFeel;
  llfMain.LookAndFeel.MasterLookAndFeel := FLayoutLookAndFeel.LookAndFeel;
end;

procedure TcxGridViewLayoutCustomizationForm.RefreshStoring;
begin
  acStore.Visible := False;
  liStore.Visible := False;
  acRestore.Visible := False;
  liRestore.Visible := False;
end;

procedure TcxGridViewLayoutCustomizationForm.RepaintGridView;
begin
  GridView.Invalidate(True);
end;

function TcxGridViewLayoutCustomizationForm.GetContainer: TcxGridViewLayoutCustomizationFormContainer;
begin
  Result := TcxGridViewLayoutCustomizationFormContainer(inherited Container);
end;

function TcxGridViewLayoutCustomizationForm.GetGridView: TcxCustomGridView;
begin
  Result := Controller.GridView;
end;

function TcxGridViewLayoutCustomizationForm.GetViewSupport: IcxGridViewLayoutEditorSupport;
begin
  Result := GridView as IcxGridViewLayoutEditorSupport;
end;

procedure TcxGridViewLayoutCustomizationForm.acOkExecute(Sender: TObject);
begin
// for enabled action
end;

procedure TcxGridViewLayoutCustomizationForm.acCancelExecute(
  Sender: TObject);
begin
  Close;
end;

procedure TcxGridViewLayoutCustomizationForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
var
  AChoice: Integer;
begin
  if (ModalResult = mrCancel) and FGridViewLayoutControl.UndoRedoManager.CanUndo then
  begin
    AChoice := MessageBox(Handle,
      PChar(cxGetResourceString(@scxGridLayoutViewCustomizeWarningDialogMessage)),
      PChar(cxGetResourceString(@scxGridLayoutViewCustomizeWarningDialogCaption)),
      MB_ICONWARNING or MB_YESNOCANCEL);
    CanClose := AChoice <> IDCANCEL;
    if AChoice = IDYES then
      ModalResult := mrOk;
  end;
end;

procedure TcxGridViewLayoutCustomizationForm.FormShortCut(var Msg: TWMKey;
  var Handled: Boolean);
begin
  inherited;
  if not Handled then
    case Msg.CharCode of
      VK_RETURN:
        begin
          Handled := tvVisibleItems.IsEditing or tvAvailableItems.IsEditing;
          if Handled then
            if tvVisibleItems.Selected <> nil then
              tvVisibleItems.Selected.EndEdit(False)
            else
              if tvAvailableItems.Selected <> nil then
                tvAvailableItems.Selected.EndEdit(False);
        end;
    end;
end;

procedure TcxGridViewLayoutCustomizationForm.FormKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  inherited;
  if Key = VK_DELETE then
      acTreeViewItemsDelete.Execute;
end;

end.
