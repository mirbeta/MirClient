{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressGaugeControl                                      }
{                                                                    }
{           Copyright (c) 2013-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSGAUGECONTROL AND ALL           }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM       }
{   ONLY.                                                            }
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

unit dxGaugeControlScalesEditor;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, ImgList, Menus, ActnList, Dialogs, ComCtrls,
  Generics.Defaults, Generics.Collections,
  cxGraphics, cxControls, cxDesignWindows, cxContainer, cxTreeView, dxGaugeControl, dxGaugeCustomScale, cxLookAndFeels,
  cxClasses, cxLookAndFeelPainters, cxEdit;

type
  { TfmGaugeControlScalesEditor }

  TfmGaugeControlScalesEditor = class(TcxDesignFormEditor)
    alScalesMain: TActionList;
    acDeleteScale: TAction;
    acAddCircularScale: TAction;
    acAddDigitalScale: TAction;
    acRestoreStyleParameters: TAction;
    pmScalesMain: TPopupMenu;
    miDeleteScale: TMenuItem;
    miSeparator: TMenuItem;
    miRestoreStyleParameters: TMenuItem;
    ilScalesTreeView: TcxImageList;
    acAddLinearScale: TAction;
    acAddCircularHalfScale: TAction;
    acAddCircularQuarterLeftScale: TAction;
    acAddCircularQuarterRightScale: TAction;
    tvScales: TcxTreeView;
    acBringForward: TAction;
    acSendBackward: TAction;
    acBringToFront: TAction;
    acSendToBack: TAction;
    N1: TMenuItem;
    BringForward1: TMenuItem;
    acBringToBack1: TMenuItem;
    BringToFront1: TMenuItem;
    N2: TMenuItem;
    miDeleteScaleSeparator: TMenuItem;
    acAddContainer: TAction;
    AddContainer1: TMenuItem;
    miAddScale: TMenuItem;
    acAddCaption: TAction;
    acAddRange: TAction;
    AddCaption1: TMenuItem;
    AddRange1: TMenuItem;

    procedure acAddCircularScaleExecute(Sender: TObject);
    procedure acAddCircularHalfScaleExecute(Sender: TObject);
    procedure acAddCircularQuarterLeftScaleExecute(Sender: TObject);
    procedure acAddCircularQuarterRightScaleExecute(Sender: TObject);
    procedure acAddDigitalScaleExecute(Sender: TObject);
    procedure acAddLinearScaleExecute(Sender: TObject);
    procedure acDeleteScaleExecute(Sender: TObject);

    procedure acRestoreStyleParametersExecute(Sender: TObject);

    procedure pmScalesMainPopup(Sender: TObject);
    procedure tvScalesChange(Sender: TObject; Node: TTreeNode);
    procedure tvScalesEdited(Sender: TObject; Node: TTreeNode; var AName: string);
    procedure tvScalesDragDrop(ASender, ASource: TObject; X, Y: Integer);
    procedure tvScalesDragOver(ASender, ASource: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure tvScalesKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ListView1GetImageIndex(Sender: TObject; Item: TListItem);
    // ZOrder
    procedure acSendBackwardExecute(Sender: TObject);
    procedure acBringForwardExecute(Sender: TObject);
    procedure acBringToFrontExecute(Sender: TObject);
    procedure acSendToBackExecute(Sender: TObject);
    procedure acDeleteScalesExecute(Sender: TObject);
    procedure acAddContainerExecute(Sender: TObject);
    procedure acAddCaptionExecute(Sender: TObject);
    procedure acAddRangeExecute(Sender: TObject);
  private
    FNodeImageIndexes: TDictionary<TClass, Integer>;

    // Gauge Control and Controller
    procedure AddScale(AScaleClass: TdxGaugeCustomScaleClass);
    procedure SetSelections;

    // Add Scale Menu Items
    procedure AddScaleClickHandler(ASender: TObject);
    procedure CreateRegisteredScaleMenuItems;

    // Scales Tree View
    function FindScaleNode(AScale: TdxGaugeCustomScale; var ANode: TTreeNode): Boolean;
    function GetController: TdxCustomGaugeControlController;
    function GetGaugeControl: TdxCustomGaugeControl;
    function GetImageIndex(AScale: Pointer): Integer; overload;
    function GetImageIndex(ANode: TTreeNode): Integer; overload;
    function GetImageIndex(AItem: TListItem): Integer; overload;
    function GetScaleIndex(AScale: TdxGaugeCustomScale): Integer;
    function IsContainerScale(ANode: TTreeNode): Boolean;
    function IsDragCopyMode: Boolean;
    function IsGaugeControl(ANode: TTreeNode): Boolean;
    procedure AddNode(AComponent: TComponent);
    procedure CreateNodeImageIndexes;
    procedure Drop(ATargetScale: TdxGaugeCustomScale);
    procedure PopulateTreeView;
    procedure RefreshTreeView(AForceRefresh: Boolean = False);
    procedure RestoreCollapsedNodes(AList: TList);
    procedure RestoreSelectedNodes(AList: TList);
    procedure StoreCollapsedNodes(AList: TList);
    procedure StoreSelectedNodes(AList: TList);
    procedure SyncSelection(const ASelection: TDesignerSelectionList);

    procedure Changed(ANeedRefreshTreeView: Boolean = True);
  protected
    procedure InitFormEditor; override;
    procedure UpdateCaption; override;

    property Controller: TdxCustomGaugeControlController read GetController;
    property GaugeControl: TdxCustomGaugeControl read GetGaugeControl;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure DoItemsModified; override;
    procedure SelectionsChanged(const ASelection: TDesignerSelectionList); override;
  end;

implementation

{$R *.dfm}

uses
  dxCore, cxGeometry, dxComCtrlsUtils, dxGaugeQuantitativeScale, dxGaugeCircularScale, dxGaugeDigitalScale, dxGaugeLinearScale;

type
  TdxCustomGaugeControlAccess = class(TdxCustomGaugeControl);
  TdxCustomGaugeControlControllerAccess = class(TdxCustomGaugeControlController);
  TdxGaugeCustomScaleAccess = class(TdxGaugeCustomScale);
  TdxGaugeCustomScaleAccessClass = class of TdxGaugeCustomScaleAccess;
  TdxGaugeQuantitativeScaleAccess = class(TdxGaugeQuantitativeScale);
  TdxGaugeScaleCollectionAccess = class(TdxGaugeScaleCollection);

function cxTreeViewNodeObjectInheritsFrom(ANode: TTreeNode; AClass: TClass): Boolean;
begin
  Result := (ANode <> nil) and (TComponent(ANode.Data) is AClass);
end;

function IsScale(ANode: TTreeNode): Boolean;
begin
  Result := cxTreeViewNodeObjectInheritsFrom(ANode, TdxGaugeCustomScale);
end;

function GetScaleFromNode(ANode: TTreeNode): TdxGaugeCustomScale;
begin
  if IsScale(ANode) then
    Result := TdxGaugeCustomScale(ANode.Data)
  else
    Result := nil;
end;

function SortNodes(ANode1, ANode2: TTreeNode; AData: TcxTag): Integer; stdcall;
var
  AScale1, AScale2: TdxGaugeCustomScale;
begin
  AScale1 := GetScaleFromNode(ANode1);
  AScale2 := GetScaleFromNode(ANode2);
  if (AScale1 <> nil) and (AScale2 <> nil) then
    Result := dxCompareValues(TdxGaugeCustomScale(GetScaleFromNode(ANode1)).Index,
      TdxGaugeCustomScale(GetScaleFromNode(ANode2)).Index)
  else
    Result := 0;
end;

{ TfmGaugeControlScalesEditor }

constructor TfmGaugeControlScalesEditor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CreateNodeImageIndexes;
  CreateRegisteredScaleMenuItems;
end;

destructor TfmGaugeControlScalesEditor.Destroy;
begin
  FreeAndNil(FNodeImageIndexes);
  inherited Destroy;
end;

procedure TfmGaugeControlScalesEditor.DoItemsModified;
begin
  inherited DoItemsModified;
  if GaugeControl <> nil then
    RefreshTreeView(True);
end;

procedure TfmGaugeControlScalesEditor.SelectionsChanged(const ASelection: TDesignerSelectionList);
begin
  if not Closing then
    SyncSelection(ASelection);
end;

procedure TfmGaugeControlScalesEditor.InitFormEditor;
begin
  inherited InitFormEditor;
  RefreshTreeView;
end;

procedure TfmGaugeControlScalesEditor.UpdateCaption;
begin
  Caption := cxGetFullComponentName(GaugeControl) + ' - Editor';
end;

procedure TfmGaugeControlScalesEditor.AddScale(AScaleClass: TdxGaugeCustomScaleClass);
var
  ANode: TTreeNode;
  AAnchorScale, AScale: TdxGaugeCustomScale;
begin
  BeginUpdate;
  AAnchorScale := GetScaleFromNode(tvScales.Selected);
  GaugeControl.BeginUpdate;
  AScale := GaugeControl.AddScale(AScaleClass);
  TdxGaugeCustomScaleAccess(AScale).AnchorScale := AAnchorScale;
  GaugeControl.EndUpdate;
  EndUpdate(False);
  Changed;
  if FindScaleNode(AScale, ANode) then
  begin
    ANode.Selected := True;
    SetSelections;
  end;
end;

procedure TfmGaugeControlScalesEditor.SetSelections;
var
  ASelection: TList;
begin
  ASelection := TList.Create;
  try
    if tvScales.SelectionCount > 0 then
      cxTreeViewGetSelection(tvScales.InnerTreeView, ASelection)
    else
      if IsGaugeControl(tvScales.Selected) then
        ASelection.Add(GaugeControl);
    if (ASelection.Count > 0) and (LockCount = 0) or IsGaugeControl(tvScales.Selected) then
      SetSelectionList(ASelection);
  finally
    ASelection.Free;
  end;
end;

procedure TfmGaugeControlScalesEditor.AddScaleClickHandler(ASender: TObject);

  function GetScaleClass(AMenuItemIndex: Integer): TdxGaugeCustomScaleClass;
  var
    AScaleClasses: TList;
  begin
    AScaleClasses := TList.Create;
    try
      dxGaugeGetRegisteredScaleClasses(AScaleClasses);
      Result := TdxGaugeCustomScaleClass(AScaleClasses[AMenuItemIndex]);
    finally
      AScaleClasses.Free;
    end;
  end;

begin
  AddScale(GetScaleClass((ASender as TMenuItem).Tag - 2001));
end;

procedure TfmGaugeControlScalesEditor.CreateRegisteredScaleMenuItems;

  function CreateMenuItem(const ACaption: string; AMenuIndex: Integer; AOnClick: TNotifyEvent): TMenuItem;
  begin
    Result := TMenuItem.Create(nil);
    Result.Caption := ACaption;
    Result.Tag := AMenuIndex;
    Result.OnClick := AOnClick;
  end;

var
  I: Integer;
  ANeedAddSeparator: Boolean;
  AScaleName: string;
  AScaleClasses: TList;
begin
  ANeedAddSeparator := True;
  AScaleClasses := TList.Create;
  try
    dxGaugeGetRegisteredScaleClasses(AScaleClasses);
    for I := 0 to AScaleClasses.Count - 1 do
      if TdxGaugeCustomScaleAccessClass(AScaleClasses[I]).GetScaleType <> stContainerScale then
      begin
        AScaleName := TdxGaugeCustomScaleAccessClass(AScaleClasses[I]).GetScaleName;
        if ANeedAddSeparator and (Pos('DB ', AScaleName) = 1) then
        begin
          miAddScale.Add(CreateMenuItem(cLineCaption, -1, nil));
          ANeedAddSeparator := False;
        end;
        miAddScale.Add(CreateMenuItem(AScaleName, 2001 + I, AddScaleClickHandler));
      end;
  finally
    AScaleClasses.Free;
  end;
end;

function TfmGaugeControlScalesEditor.FindScaleNode(AScale: TdxGaugeCustomScale; var ANode: TTreeNode): Boolean;
begin
  Result := cxTreeViewFindNodeByData(tvScales.InnerTreeView, AScale, ANode);
  if not Result then
    ANode := tvScales.Items[0];
end;

function TfmGaugeControlScalesEditor.GetController: TdxCustomGaugeControlController;
begin
  Result := TdxCustomGaugeControlAccess(GaugeControl).Controller;
end;

function TfmGaugeControlScalesEditor.GetGaugeControl: TdxCustomGaugeControl;
begin
  Result := Component as TdxCustomGaugeControl;
end;

function TfmGaugeControlScalesEditor.GetImageIndex(AScale: Pointer): Integer;
begin
  if not FNodeImageIndexes.TryGetValue(TComponent(AScale).ClassType, Result) then
    if not FNodeImageIndexes.TryGetValue(TComponent(AScale).ClassParent, Result) then
      Result := -1;
end;

function TfmGaugeControlScalesEditor.GetImageIndex(ANode: TTreeNode): Integer;
begin
  Result := GetImageIndex(ANode.Data);
end;

function TfmGaugeControlScalesEditor.GetImageIndex(AItem: TListItem): Integer;
begin
  Result := GetImageIndex(GaugeControl.Scales[AItem.Index]);
end;

function TfmGaugeControlScalesEditor.GetScaleIndex(AScale: TdxGaugeCustomScale): Integer;
begin
  if AScale = nil then
    Result := -1
  else
    Result := AScale.Index;
end;

function TfmGaugeControlScalesEditor.IsContainerScale(ANode: TTreeNode): Boolean;
begin
  Result := cxTreeViewNodeObjectInheritsFrom(ANode, TdxGaugeContainerScale);
end;

function TfmGaugeControlScalesEditor.IsDragCopyMode: Boolean;
begin
  Result := IsCtrlPressed;
end;

function TfmGaugeControlScalesEditor.IsGaugeControl(ANode: TTreeNode): Boolean;
begin
  Result := cxTreeViewNodeObjectInheritsFrom(ANode, TdxCustomGaugeControl);
end;

procedure TfmGaugeControlScalesEditor.ListView1GetImageIndex(Sender: TObject; Item: TListItem);
begin
  inherited;
  Item.ImageIndex := GetImageIndex(Item);
end;

procedure TfmGaugeControlScalesEditor.AddNode(AComponent: TComponent);

  function GetParentNode(AComponent: TComponent): TTreeNode;
  begin
    if AComponent is TdxCustomGaugeControl then
      Result := nil
    else
      FindScaleNode(TdxGaugeCustomScaleAccess(AComponent).AnchorScale, Result);
  end;

  procedure AddCaptions(ANode: TTreeNode);
  var
    I: Integer;
    AChildNode: TTreeNode;
  begin
    if AComponent is TdxGaugeCustomScale and (TdxGaugeCustomScaleAccess(AComponent).Captions.Count > 0) then
      for I := 0 to TdxGaugeCustomScaleAccess(AComponent).Captions.Count - 1 do
      begin
        AChildNode := tvScales.Items.AddChild(ANode, TdxGaugeCustomScaleAccess(AComponent).Captions[I].Name);
        AChildNode.Data := TdxGaugeCustomScaleAccess(AComponent).Captions[I];
        AChildNode.Selected := False;
        AChildNode.Focused := False;
        AChildNode.ImageIndex := GetImageIndex(AChildNode);
        AChildNode.SelectedIndex := AChildNode.ImageIndex;
      end;
  end;

  procedure AddRanges(ANode: TTreeNode);
  var
    I: Integer;
    AChildNode: TTreeNode;
  begin
    if AComponent is TdxGaugeQuantitativeScale and (TdxGaugeQuantitativeScaleAccess(AComponent).Ranges.Count > 0) then
      for I := 0 to TdxGaugeQuantitativeScaleAccess(AComponent).Ranges.Count - 1 do
      begin
        AChildNode := tvScales.Items.AddChild(ANode, TdxGaugeQuantitativeScaleAccess(AComponent).Ranges[I].Name);
        AChildNode.Data := TdxGaugeQuantitativeScaleAccess(AComponent).Ranges[I];
        AChildNode.Selected := False;
        AChildNode.Focused := False;
        AChildNode.ImageIndex := GetImageIndex(AChildNode);
        AChildNode.SelectedIndex := AChildNode.ImageIndex;
      end;
  end;

var
  ANode: TTreeNode;
begin
  ANode := tvScales.Items.AddChild(GetParentNode(AComponent), AComponent.Name);
  ANode.Data := AComponent;
  ANode.Selected := False;
  ANode.Focused := False;
  ANode.ImageIndex := GetImageIndex(ANode);
  ANode.SelectedIndex := ANode.ImageIndex;
  if not IsGaugeControl(ANode) then
  begin
    AddCaptions(ANode);
    AddRanges(ANode);
  end;
end;

procedure TfmGaugeControlScalesEditor.CreateNodeImageIndexes;
begin
  FNodeImageIndexes := TDictionary<TClass, Integer>.Create;
  FNodeImageIndexes.Add(TdxCustomGaugeControl, 0);
  FNodeImageIndexes.Add(TdxGaugeCustomCircularScale, 1);
  FNodeImageIndexes.Add(TdxGaugeCustomDigitalScale, 2);
  FNodeImageIndexes.Add(TdxGaugeCustomLinearScale, 3);
  FNodeImageIndexes.Add(TdxGaugeCustomCircularHalfScale, 4);
  FNodeImageIndexes.Add(TdxGaugeCustomCircularQuarterLeftScale, 5);
  FNodeImageIndexes.Add(TdxGaugeCustomCircularQuarterRightScale, 6);
  FNodeImageIndexes.Add(TdxGaugeCustomCircularThreeFourthScale, 7);
  FNodeImageIndexes.Add(TdxGaugeCustomCircularWideScale, 8);
  FNodeImageIndexes.Add(TdxGaugeCustomContainerScale, 9);
  FNodeImageIndexes.Add(TdxGaugeCustomCaption, 10);
  FNodeImageIndexes.Add(TdxGaugeLinearScaleRange, 11);
  FNodeImageIndexes.Add(TdxGaugeCircularScaleRange, 12);
  FNodeImageIndexes.Add(TdxGaugeCircularWideScaleRange, 12);
end;

procedure TfmGaugeControlScalesEditor.Drop(ATargetScale: TdxGaugeCustomScale);

  procedure CreateScaleCopy;
  var
    AScale, ABaseScale: TdxGaugeCustomScale;
  begin
    ABaseScale := GetScaleFromNode(tvScales.Selected);
    AScale := GaugeControl.AddScale(TdxGaugeCustomScaleClass(ABaseScale.ClassType));
    AScale.Assign(ABaseScale);
    TdxGaugeCustomScaleAccess(AScale).AnchorScaleIndex := GetScaleIndex(ATargetScale);
  end;

  procedure ReAnchoringSelectedScales;
  var
    I: Integer;
    ASelectedScale: TdxGaugeCustomScale;
  begin
    for I := 0 to tvScales.SelectionCount - 1 do
    begin
      ASelectedScale := GetScaleFromNode(TTreeNode(tvScales.Selections[I]));
      TdxGaugeCustomScaleAccess(ASelectedScale).AnchorScaleIndex := GetScaleIndex(ATargetScale);
    end;
  end;

begin
  GaugeControl.BeginUpdate;
  if IsDragCopyMode then
    CreateScaleCopy
  else
    ReAnchoringSelectedScales;
  GaugeControl.EndUpdate;
  Designer.Modified;
end;

procedure TfmGaugeControlScalesEditor.PopulateTreeView;
var
  I: Integer;
  AScales: TList;
begin
  tvScales.Items.BeginUpdate;
  tvScales.Items.Clear;
  AddNode(GaugeControl);
  AScales := TList.Create;
  try
    TdxCustomGaugeControlControllerAccess(Controller).GetScalesByDependences(AScales);
    for I := 0 to AScales.Count -1  do
      AddNode(TdxGaugeCustomScale(AScales[I]));
    tvScales.Items[0].CustomSort(@SortNodes, 0, True);
  finally
    tvScales.Items.EndUpdate;
    AScales.Free;
  end;
end;

procedure TfmGaugeControlScalesEditor.RefreshTreeView(AForceRefresh: Boolean = False);

  function NeedRefresh: Boolean;

    procedure GetGaugeControlObjects(AList: TList);
    var
      I, J: Integer;
      AScale: TdxGaugeCustomScale;
    begin
      for I := 0 to GaugeControl.Scales.Count - 1 do
      begin
        AScale := GaugeControl.Scales[I];
        AList.Add(AScale);
        for J := 0 to TdxGaugeCustomScaleAccess(AScale).Captions.Count - 1 do
          AList.Add(TdxGaugeCustomScaleAccess(AScale).Captions[J]);
        if AScale is TdxGaugeQuantitativeScale then
          for J := 0 to TdxGaugeQuantitativeScaleAccess(AScale).Ranges.Count - 1 do
            AList.Add(TdxGaugeQuantitativeScaleAccess(AScale).Ranges[J]);
      end;
      AList.Add(GaugeControl);
    end;

  var
    ADifference, ATreeViewObjects, AGaugeControlObjects: TList;
  begin
    Result := (GaugeControl.Scales.Count = 0) and (tvScales.Items.Count = 1);
    if not Result then
    begin
      ADifference := TList.Create;
      ATreeViewObjects := TList.Create;
      AGaugeControlObjects := TList.Create;
      try
        GetGaugeControlObjects(AGaugeControlObjects);
        cxTreeViewGetData(tvScales.InnerTreeView, ATreeViewObjects);
        ADifference.Assign(ATreeViewObjects, laXor, AGaugeControlObjects);
        Result := ADifference.Count <> 0;
      finally
        AGaugeControlObjects.Free;
        ATreeViewObjects.Free;
        ADifference.Free;
      end;
    end;
  end;

var
  ASelectedNodes: TList;
  ACollapsedItems: TList;
begin
  if AForceRefresh or NeedRefresh then
  begin
    ACollapsedItems := TList.Create;
    ASelectedNodes := TList.Create;
    tvScales.Items.BeginUpdate;
    try
      StoreSelectedNodes(ASelectedNodes);
      StoreCollapsedNodes(ACollapsedItems);
      PopulateTreeView;
      RestoreCollapsedNodes(ACollapsedItems);
      RestoreSelectedNodes(ASelectedNodes);
    finally
      tvScales.Items.EndUpdate;
      ASelectedNodes.Free;
      ACollapsedItems.Free;
    end;
  end;
end;

procedure TfmGaugeControlScalesEditor.RestoreCollapsedNodes(AList: TList);
var
  I: Integer;
  ANode: TTreeNode;
begin
  tvScales.FullExpand;
  for I := 0 to AList.Count - 1 do
    if FindScaleNode(AList[I], ANode) then
      ANode.Expanded := False;
end;

procedure TfmGaugeControlScalesEditor.RestoreSelectedNodes(AList: TList);
begin
  cxTreeViewSetSelection(tvScales.InnerTreeView, AList);
end;

procedure TfmGaugeControlScalesEditor.StoreCollapsedNodes(AList: TList);
begin
  cxTreeViewGetCollapsed(tvScales.InnerTreeView, AList);
end;

procedure TfmGaugeControlScalesEditor.StoreSelectedNodes(AList: TList);
begin
  cxTreeViewGetSelection(tvScales.InnerTreeView, AList);
end;

procedure TfmGaugeControlScalesEditor.SyncSelection(const ASelection: TDesignerSelectionList);
var
  AList: TList;
begin
  BeginUpdate;
  AList := TList.Create;
  try
    RefreshTreeView;
    ConvertSelectionToList(ASelection, AList);
    cxTreeViewSetSelection(tvScales.InnerTreeView, AList);
  finally
    EndUpdate(False);
    AList.Free;
  end;
end;

procedure TfmGaugeControlScalesEditor.Changed(ANeedRefreshTreeView: Boolean = True);
begin
  BeginUpdate;
  try
    if ANeedRefreshTreeView then
      RefreshTreeView;
  finally
    EndUpdate(False);
  end;
  Designer.Modified;
end;

procedure TfmGaugeControlScalesEditor.acAddCaptionExecute(Sender: TObject);
begin
  TdxCustomGaugeControlControllerAccess(Controller).AddCaption(GetScaleFromNode(tvScales.Selected));
end;

procedure TfmGaugeControlScalesEditor.acAddCircularHalfScaleExecute(Sender: TObject);
begin
  AddScale(TdxGaugeCircularHalfScale);
end;

procedure TfmGaugeControlScalesEditor.acAddCircularQuarterLeftScaleExecute(Sender: TObject);
begin
  AddScale(TdxGaugeCircularQuarterLeftScale);
end;

procedure TfmGaugeControlScalesEditor.acAddCircularQuarterRightScaleExecute(Sender: TObject);
begin
  AddScale(TdxGaugeCircularQuarterRightScale);
end;

procedure TfmGaugeControlScalesEditor.acAddCircularScaleExecute(Sender: TObject);
begin
  AddScale(TdxGaugeCircularScale);
end;

procedure TfmGaugeControlScalesEditor.acAddContainerExecute(Sender: TObject);
begin
  AddScale(TdxGaugeContainerScale);
end;

procedure TfmGaugeControlScalesEditor.acAddDigitalScaleExecute(Sender: TObject);
begin
  AddScale(TdxGaugeDigitalScale);
end;

procedure TfmGaugeControlScalesEditor.acAddLinearScaleExecute(Sender: TObject);
begin
  AddScale(TdxGaugeLinearScale);
end;

procedure TfmGaugeControlScalesEditor.acAddRangeExecute(Sender: TObject);
begin
  TdxCustomGaugeControlControllerAccess(Controller).AddRange(GetScaleFromNode(tvScales.Selected));
end;

procedure TfmGaugeControlScalesEditor.acSendBackwardExecute(Sender: TObject);
begin
  TdxCustomGaugeControlControllerAccess(Controller).ChangeSelectionsZOrders(octSendBackward);
end;

procedure TfmGaugeControlScalesEditor.acBringForwardExecute(Sender: TObject);
begin
  TdxCustomGaugeControlControllerAccess(Controller).ChangeSelectionsZOrders(octBringForward);
end;

procedure TfmGaugeControlScalesEditor.acBringToFrontExecute(Sender: TObject);
begin
  TdxCustomGaugeControlControllerAccess(Controller).ChangeSelectionsZOrders(octBringToFront);
end;

procedure TfmGaugeControlScalesEditor.acSendToBackExecute(Sender: TObject);
begin
  TdxCustomGaugeControlControllerAccess(Controller).ChangeSelectionsZOrders(octSendToBack);
end;

procedure TfmGaugeControlScalesEditor.acDeleteScaleExecute(Sender: TObject);
begin
  if not ((tvScales.SelectionCount = 1) and IsGaugeControl(tvScales.Selections[0])) then
  begin
    BeginUpdate;
    TdxCustomGaugeControlControllerAccess(Controller).DeleteSelection;
    EndUpdate(False);
    cxTreeViewSetSelection(tvScales.InnerTreeView, TdxCustomGaugeControlControllerAccess(Controller).Selections);
    SetSelections;
  end;
end;

procedure TfmGaugeControlScalesEditor.acDeleteScalesExecute(Sender: TObject);
begin
  BeginUpdate;
  try
    GaugeControl.Clear;
  finally
    EndUpdate(False);
    Changed(False);
  end;
end;

procedure TfmGaugeControlScalesEditor.acRestoreStyleParametersExecute(Sender: TObject);
var
  I: Integer;
  AScale: TdxGaugeCustomScale;
begin
  GaugeControl.BeginUpdate;
  for I := 0 to tvScales.SelectionCount - 1 do
  begin
    AScale := GetScaleFromNode(tvScales.Selections[I]);
    if AScale <> nil then
      AScale.RestoreStyleParameters;
  end;
  GaugeControl.EndUpdate;
  Changed(False);
end;

procedure TfmGaugeControlScalesEditor.pmScalesMainPopup(Sender: TObject);

  function CanAddRange: Boolean;
  begin
    Result := (GetScaleFromNode(tvScales.Selected) is TdxGaugeQuantitativeScale) and (tvScales.SelectionCount = 1);
  end;

var
  AIsNotGaugeControlNodeSelected: Boolean;
  AIsScaleElement: Boolean;
begin
  AIsScaleElement := TdxCustomGaugeControlControllerAccess(Controller).IsScaleElement;
  AIsNotGaugeControlNodeSelected := not IsGaugeControl(tvScales.Selected);
  acDeleteScale.Visible := AIsNotGaugeControlNodeSelected;
  acRestoreStyleParameters.Visible := AIsNotGaugeControlNodeSelected and not AIsScaleElement and
    not IsContainerScale(tvScales.Selected);
  acBringForward.Visible := AIsNotGaugeControlNodeSelected and not AIsScaleElement;
  acSendBackward.Visible := AIsNotGaugeControlNodeSelected and not AIsScaleElement;
  acBringToFront.Visible := AIsNotGaugeControlNodeSelected and not AIsScaleElement;
  acSendToBack.Visible := AIsNotGaugeControlNodeSelected and not AIsScaleElement;
  miAddScale.Visible := not AIsScaleElement;
  acAddContainer.Visible := not AIsScaleElement;
  acAddCaption.Visible := AIsNotGaugeControlNodeSelected and not IsContainerScale(tvScales.Selected) and not AIsScaleElement;
  acAddRange.Visible := CanAddRange and not AIsScaleElement;
end;

procedure TfmGaugeControlScalesEditor.tvScalesChange(Sender: TObject; Node: TTreeNode);
begin
  if tvScales.IsFocused and
    not (IsGaugeControl(Node) or TdxCustomGaugeControlAccess(GaugeControl).Locked) then
    SetSelections;
end;

procedure TfmGaugeControlScalesEditor.tvScalesEdited(Sender: TObject; Node: TTreeNode; var AName: string);
begin
  if Length(AName) > 0 then
    TComponent(Node.Data).Name := AName
  else
    AName := TComponent(Node.Data).Name;
end;

procedure TfmGaugeControlScalesEditor.tvScalesDragDrop(ASender, ASource: TObject; X, Y: Integer);
begin
  if ASender = tvScales then
  begin
    Drop(GetScaleFromNode(tvScales.GetNodeAt(X, Y)));
    SetSelections;
  end;
end;

procedure TfmGaugeControlScalesEditor.tvScalesDragOver(ASender, ASource: TObject; X, Y: Integer; State: TDragState;
  var Accept: Boolean);

  function IsGaugeControlNodeDragging: Boolean;
  var
    I: Integer;
  begin
    Result := IsGaugeControl(tvScales.Selected);
    if not Result then
      for I := 0 to tvScales.SelectionCount - 1 do
      begin
        Result := IsGaugeControl(tvScales.Selections[I]);
        if Result then
          Break;
      end;
  end;

var
  ANode: TTreeNode;
  AScaleIndex, AAnchorScaleIndex: Integer;
begin
  Accept := not IsGaugeControlNodeDragging and (ASender = cxExtractDragObjectSource(ASource));
  if Accept then
    if not IsCtrlPressed then
    begin
      ANode := tvScales.GetNodeAt(X, Y);
      AScaleIndex := GetScaleIndex(GetScaleFromNode(tvScales.Selected));
      AAnchorScaleIndex := GetScaleIndex(GetScaleFromNode(ANode));
      Accept := TdxCustomGaugeControlControllerAccess(Controller).CanAnchorScale(AScaleIndex, AAnchorScaleIndex);
    end;
end;

procedure TfmGaugeControlScalesEditor.tvScalesKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_DELETE:
      if tvScales.Focused and not tvScales.IsEditing then
        acDeleteScale.Execute;
    VK_ESCAPE:
      CloseEditor;
  end;
end;

end.
