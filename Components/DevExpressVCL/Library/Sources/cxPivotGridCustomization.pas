{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressPivotGrid                                         }
{                                                                    }
{           Copyright (c) 2005-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSPIVOTGRID AND ALL ACCOMPANYING }
{   VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY.              }
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
unit cxPivotGridCustomization;

{$I cxVer.inc}

interface

uses
  Windows, Messages, Classes, SysUtils, Forms, Controls, StdCtrls, Graphics, Math,
  dxCore, cxClasses,
  cxControls, cxGraphics, cxDropDownEdit, cxButtons, cxLookAndFeels, cxListBox, cxLabel,
  cxStyles, cxLookAndFeelPainters, cxGeometry, cxCustomPivotGrid, cxPivotGridStrs,
  cxCustomData, cxTreeView, ComCtrls, cxGroupBox;

type
  TcxPivotGridCustomizationForm = class;

  { TcxFieldListListBox }

  TcxFieldListListBox = class(TdxCustomListBox)
  strict private
    FFieldsType: TcxPivotGridCustomizationFormFieldListType;

    procedure CheckItemIndex(const P: TPoint);
    procedure FilterInitPopupHandler(Sender: TObject);
    function GetFieldChooser: TcxPivotGridCustomCustomizationForm;
    function GetPainter: TcxCustomLookAndFeelPainter;
    function GetPivotGrid: TcxCustomPivotGrid;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
  protected
    function AllowTouchScrollUIMode: Boolean; override;
    function CanChangeFieldFilter: Boolean;
    procedure Click; override;
    procedure DrawItem(const R: TRect; AItem: TdxCustomListBoxItem; AState: TcxButtonState); override;
    function GetAreaIndexOffset(AField: TcxPivotGridField; ACheckGroup: Boolean): Integer;
    function GetDragAndDropObjectClass: TcxDragAndDropObjectClass; override;
    function GetSelectedField: TcxPivotGridField;
    procedure Initialize; virtual;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure LookAndFeelChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    function StartDragAndDrop(const P: TPoint): Boolean; override;
    procedure SynchronizeField(AField: TcxPivotGridField; AAreaIndex: Integer);
    procedure UpdateBackgroundColor;
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateEx(AOwner: TComponent; AFieldsType: TcxPivotGridCustomizationFormFieldListType); virtual;
    function CalculateHitTest(AHitTest: TcxPivotGridHitTest): Boolean; virtual;
    function FieldByIndex(AIndex: Integer): TcxPivotGridField;
    procedure SynchronizeFields;
    procedure UpdateItemHeight;
    procedure UpdateSelection;

    property FieldChooser: TcxPivotGridCustomCustomizationForm read GetFieldChooser;
    property FieldsType: TcxPivotGridCustomizationFormFieldListType read FFieldsType;
    property Painter: TcxCustomLookAndFeelPainter read GetPainter;
    property PivotGrid: TcxCustomPivotGrid read GetPivotGrid;
    property SelectedField: TcxPivotGridField read GetSelectedField;
  end;

  { TcxPivotGridListListBoxDragAndDropObject }

  TcxPivotGridListListBoxDragAndDropObject = class(TcxPivotGridDragAndDropObject)
  private
    function GetField: TcxPivotGridField;
    function GetFieldHeaderHeight: Integer;
  protected
    procedure BeginDragAndDrop; override;
    function CheckArea(const P: TPoint; var AInfo: TcxPivotGridDragDropAreaInfo): Boolean; override;
    procedure CreateDragImage; override;
    procedure DragAndDrop(const P: TPoint; var Accepted: Boolean); override;
    procedure EndDragAndDrop(Accepted: Boolean); override;
    function GetPivotGrid: TcxCustomPivotGrid; override;
    function IsSameDropPlace: Boolean; override;

    property Field: TcxPivotGridField read GetField;
    property FieldHeaderHeight: Integer read GetFieldHeaderHeight;
  end;

  { TcxPivotGridCustomizationForm }

  TcxPivotGridCustomizationForm = class(TcxPivotGridCustomCustomizationForm)
  private
    procedure CMBiDiModeChanged(var Message: TMessage); message CM_BIDIMODECHANGED;
  protected
    AddButton: TcxButton;
    AreaList: TcxComboBox;
    BottomPanel: TcxGroupBox;
    FieldList: TcxFieldListListBox;
    FieldListCaption: TcxLabel;

    procedure AddToClickHandler(Sender: TObject);
    procedure AreaListClickHandler(Sender: TObject);
    procedure DoCreateControls; override;
    procedure DoUpdateSelection; override;
    function GetButtonAlign: TAlign;
    function GetCustomizationFormListBackgroundColor: TColor; override;
    function GetFieldListByType(AListType: TcxPivotGridCustomizationFormFieldListType): TObject; override;
    procedure Init; override;
    procedure Resize; override;
    //
    procedure OnItemDblClick(Sender: TObject);
    procedure FieldListClickHandler(Sender: TObject);
    //
    function CreateAreaList: TcxComboBox;
    function CreateBottomPanel: TcxGroupBox;
    function CreateButton: TcxButton;
    function CreateCaption: TcxLabel;
    function CreateFieldList: TcxFieldListListBox;
    procedure SyncArea;
    procedure SyncVisibility;
  public
    constructor Create(AOwner: TComponent); override;
    procedure RefreshList; override;
  end;

  // Customization form for OLAP mode

  TcxPivotGridOLAPCustomizationForm = class;
  TcxPivotGridOLAPStructureInnerTreeView = class;

  { TcxPivotGridStructureTreeDragAndDropObject }

  TcxPivotGridOLAPTreeDragAndDropObject = class(TcxPivotGridDragAndDropObject)
  private
    FTreeView: TcxPivotGridOLAPStructureInnerTreeView;
    function GetField: TcxPivotGridField;
    function GetFieldChooser: TcxPivotGridOLAPCustomizationForm;
    function GetFieldHeaderHeight: Integer;
  protected
    procedure BeginDragAndDrop; override;
    function CheckArea(const P: TPoint; var AInfo: TcxPivotGridDragDropAreaInfo): Boolean; override;
    procedure CreateDragImage; override;
    procedure DragAndDrop(const P: TPoint; var Accepted: Boolean); override;
    procedure EndDragAndDrop(Accepted: Boolean); override;
    function GetClientCursorPos: TPoint; override;
    function GetImmediateStart: Boolean; override;

    property Field: TcxPivotGridField read GetField;
    property FieldChooser: TcxPivotGridOLAPCustomizationForm read GetFieldChooser;
    property FieldHeaderHeight: Integer read GetFieldHeaderHeight;
    property TreeView: TcxPivotGridOLAPStructureInnerTreeView read FTreeView;
  end;

  { TcxPivotGridOLAPStructureInnerTreeView }

  TcxPivotGridOLAPStructureInnerTreeView = class(TcxCustomInnerTreeView)
  private
    FDragAndDropObject: TcxDragAndDropObject;
    FDragAndDropPrevCursor: TCursor;
    FDragAndDropState: TcxDragAndDropState;
    FFinishingDragAndDrop: Boolean;
    FMouseCaptureObject: TObject;
    FMouseDownPos: TPoint;
    function GetIsDestroying: Boolean;
    function GetDragAndDropObject: TcxDragAndDropObject;
    function GetFieldChooser: TcxPivotGridCustomCustomizationForm;
    function GetForm: TcxPivotGridOLAPCustomizationForm;
    function GetPivotGrid: TcxCustomPivotGrid;
    procedure SetDragAndDropState(AValue: TcxDragAndDropState);
    procedure SetMouseCaptureObject(AValue: TObject);
    procedure CNKeyDown(var Message: TWMKeyDown); message CN_KEYDOWN;
    procedure WMCancelMode(var Message: TWMCancelMode); message WM_CANCELMODE;
  protected
    function CanEdit(Node: TTreeNode): Boolean; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

    procedure BeginDragAndDrop;
    procedure DoCancelMode;
    procedure DragAndDrop(const P: TPoint; var Accepted: Boolean);
    procedure EndDragAndDrop(Accepted: Boolean);
    procedure FinishDragAndDrop(Accepted: Boolean);
    function StartDragAndDrop(const P: TPoint): Boolean; dynamic;

    property DragAndDropObject: TcxDragAndDropObject read GetDragAndDropObject;
    property DragAndDropState: TcxDragAndDropState read FDragAndDropState write SetDragAndDropState;
    property Form: TcxPivotGridOLAPCustomizationForm read GetForm;
    property IsDestroying: Boolean read GetIsDestroying;
    property MouseCaptureObject: TObject read FMouseCaptureObject write SetMouseCaptureObject;
    property PivotGrid: TcxCustomPivotGrid read GetPivotGrid;
  public
    function CalculateHitTest(AHitTest: TcxPivotGridHitTest): Boolean; virtual;

    property FieldChooser: TcxPivotGridCustomCustomizationForm read GetFieldChooser;
  end;

  { TcxPivotGridOLAPCustomizationForm }

  TcxPivotGridOLAPTreeView = class(TcxTreeView)
  protected
    class function GetTreeViewClass: TcxCustomInnerTreeViewClass; override;
  end;

  TcxPivotGridOLAPCustomizationForm = class(TcxPivotGridCustomCustomizationForm)
  private
    function GetStructure: TcxPivotGridOLAPStructureNode;
  protected
    CurrentStructure: TcxPivotGridOLAPStructureNode;
    FieldListCaption: TcxLabel;
    FieldList: TcxPivotGridOLAPTreeView;
    ExpandingInfo: TList;
    StructureIsPopulated: Boolean;
    procedure DoCreateControls; override;
    procedure DoUpdateSelection; override;
    procedure DrawStructureItem(Sender: TCustomTreeView; Node: TTreeNode;
      State: TCustomDrawState; var DefaultDraw: Boolean);
    function GetCustomizationFormListBackgroundColor: TColor; override;
    function GetStructureDisplayText(AStructure: TcxPivotGridOLAPStructureNode): string;
    procedure Init; override;
    procedure Localize; override;
    procedure OnGetTreeViewNodeImageIndex(Sender: TObject; Node: TTreeNode);
    procedure OnItemDblClick(Sender: TObject);
    procedure PopulateStructure; virtual;
    procedure SaveExpanding;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure RefreshList; override;

    property Structure: TcxPivotGridOLAPStructureNode read GetStructure;
  end;

procedure cxPopulateListBox(AFieldChooser: TcxPivotGridCustomCustomizationForm;
  AListBox: TcxFieldListListBox; AType: TcxPivotGridCustomizationFormFieldListType);

implementation

uses
  Types, cxContainer, Contnrs, cxEdit;

const
  ListType2FieldArea: array[TcxPivotGridCustomizationFormFieldListType] of TcxPivotGridFieldArea =
    (faFilter, faColumn, faRow, faFilter, faData);
  FieldArea2ListType: array[TcxPivotGridFieldArea] of TcxPivotGridCustomizationFormFieldListType =
    (fltColumn, fltRow, fltFilter, fltData);

type
  TcxPivotAccess = class(TcxCustomPivotGrid);

  { TcxFieldListListBoxFieldViewInfo }

  TcxFieldListListBoxFieldViewInfo = class(TcxPivotGridFieldHeaderCellViewInfo)
  strict private
    FTempFont: TFont;
  public
    constructor CreateEx(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure Initialize(ACanvas: TcxCanvas; AScaleFactor: TdxScaleFactor; ASource: TcxPivotGridFieldHeaderCellViewInfo);
  end;

procedure cxPopulateListBox(AFieldChooser: TcxPivotGridCustomCustomizationForm;
  AListBox: TcxFieldListListBox; AType: TcxPivotGridCustomizationFormFieldListType);
var
  AList: TObjectList;
  I: Integer;
  AField: TcxPivotGridField;
begin
  AList := TObjectList.Create(False);
  try
    TcxPivotGridCustomizationForm(AFieldChooser).PopulateFieldList(AList, AType);
    AListBox.BeginUpdate;
    try
      AListBox.Items.Clear;
      for I := 0 to AList.Count - 1 do
      begin
        AField := TcxPivotGridField(AList[I]);
        AListBox.AddItem(AField.Caption, AField);
      end;
      AListBox.Sorted := (AType = fltAvailable) and AFieldChooser.PivotGrid.Customization.AvailableFieldsSorted;
    finally
      AListBox.Initialize;
      AListBox.EndUpdate;
    end;
  finally
    AList.Free;
  end;
end;

{ TcxFieldListListBox }

constructor TcxFieldListListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  UpdateBackgroundColor;
end;

constructor TcxFieldListListBox.CreateEx(
  AOwner: TComponent; AFieldsType: TcxPivotGridCustomizationFormFieldListType);
begin
  Create(AOwner);
  FFieldsType := AFieldsType;
end;

function TcxFieldListListBox.AllowTouchScrollUIMode: Boolean;
begin
  Result := not IsDesigning;
end;

function TcxFieldListListBox.CanChangeFieldFilter: Boolean;
begin
  Result := TcxPivotGridCustomizationForm(FieldChooser).CanChangeFieldFilter;
end;

procedure TcxFieldListListBox.Click;
begin
  inherited Click;
  TcxPivotGridCustomizationForm(FieldChooser).SelectedObject := SelectedField;
end;

procedure TcxFieldListListBox.CMFontChanged(var Message: TMessage);
begin
  inherited;
  UpdateItemHeight;
end;

procedure TcxFieldListListBox.DrawItem(const R: TRect; AItem: TdxCustomListBoxItem; AState: TcxButtonState);
const
  HeaderStateMap: array[Boolean] of TcxButtonState = (cxbsNormal, cxbsPressed);
  FilterStateMap: array[Boolean] of TcxButtonState = (cxbsNormal, cxbsDisabled);

  function Clone(AFieldViewInfo: TcxPivotGridFieldHeaderCellViewInfo): TcxFieldListListBoxFieldViewInfo;
  begin
    Result := TcxFieldListListBoxFieldViewInfo.CreateEx(AFieldViewInfo.Field);
    Result.Initialize(Canvas, ScaleFactor, AFieldViewInfo);
    Result.FIsRightToLeftConverted := PivotGrid.UseRightToLeftAlignment;
  end;

  function GetSortOrder(AFieldViewInfo: TcxPivotGridFieldHeaderCellViewInfo): TcxDataSortOrder;
  begin
    Result := AFieldViewInfo.SortOrder;
    if AFieldViewInfo.Group <> nil then
      Result := soNone;
  end;

var
  AField: IcxPivotGridField;
  AFieldViewInfo: TcxPivotGridFieldHeaderCellViewInfo;
begin
  if not IsLocked or FieldChooser.IsLocked then
  begin
    AField := FieldByIndex(AItem.Index);
    AFieldViewInfo := Clone(AField.GetViewInfo);
    try
      AFieldViewInfo.PaintTo(Canvas, R, AState,
        FilterStateMap[AFieldViewInfo.FilterState <> cxbsDisabled], GetSortOrder(AFieldViewInfo), False,
        TcxPivotGridCustomizationForm(FieldChooser).DoCustomDrawFieldHeader);
    finally
      AFieldViewInfo.Free;
    end;
  end;
end;

function TcxFieldListListBox.CalculateHitTest(AHitTest: TcxPivotGridHitTest): Boolean;

  function GetItemRect(AIndex: Integer): TRect;
  begin
    if Count = 0 then
      Result := ClientRect
    else
    begin
      Result := ItemRect(AIndex);
      if AIndex >= Count then
        Result.Bottom := ClientRect.Bottom;
    end;
  end;

var
  R: TRect;
  P, Origin: TPoint;
  AIndex: Integer;
  AInfo: TcxPivotGridDragDropAreaInfo;
begin
  Result := Visible;
  if not Result then Exit;
  P := dxMapWindowPoint(PivotGrid.Handle, Handle, AHitTest.HitPoint);
  Origin := ClientToScreen(cxNullPoint);
  Result := cxRectPtIn(ClientRect, P);
  if not Result then Exit;
  AInfo := TcxPivotGridCustomizationForm(FieldChooser).GetDragDropInfo;
  AInfo.Area := ListType2FieldArea[FieldsType];
  AInfo.Visible := FieldsType <> fltAvailable;
  AHitTest.FieldListType := FieldsType;
  AInfo.AreaIndex := 0;
  AInfo.DisplayBounds := cxRectOffset(ClientRect, Origin);
  AInfo.Field := nil;
  if FieldsType <> fltAvailable then
  begin
    for AIndex := 0 to TopIndex - 1 do
      Inc(AInfo.AreaIndex, GetAreaIndexOffset(FieldByIndex(AIndex),
        TcxPivotGridCustomizationForm(FieldChooser).GetImmediateUpdate));
    for AIndex := TopIndex to Count do
    begin
      R := GetItemRect(AIndex);
      AInfo.DisplayBounds := cxRectOffset(R, Origin);
      AInfo.Field := FieldByIndex(AIndex);
      if cxRectPtIn(R, P) or (R.Bottom > ClientBounds.Bottom) then
        Break
      else
        Inc(AInfo.AreaIndex, GetAreaIndexOffset(FieldByIndex(AIndex),
          TcxPivotGridCustomizationForm(FieldChooser).GetImmediateUpdate));
    end;
  end;
  AIndex := ItemAtPos(P, True);
  if AIndex <> -1 then
  begin
    TcxPivotGridCustomizationForm(FieldChooser).CalculateFieldHitTest(AHitTest,
      FieldByIndex(AIndex), dxMapWindowRect(Handle, PivotGrid.Handle, ItemRect(AIndex)));
  end
end;

function TcxFieldListListBox.FieldByIndex(AIndex: Integer): TcxPivotGridField;
begin
  if (AIndex >= 0) and (AIndex < Count) then
    Result := Items[AIndex].Data as TcxPivotGridField
  else
    Result := nil;
end;

procedure TcxFieldListListBox.SynchronizeFields;
var
  AAreaIndex, I: Integer;
begin
  PivotGrid.BeginUpdate;
  try
    AAreaIndex := 0;
    for I := 0 to Count - 1 do
    begin
      SynchronizeField(FieldByIndex(I), AAreaIndex);
      Inc(AAreaIndex, GetAreaIndexOffset(FieldByIndex(I), True));
    end;
  finally
    PivotGrid.EndUpdate;
  end;
end;

procedure TcxFieldListListBox.UpdateItemHeight;
begin
  ItemHeight := LookAndFeelPainter.ScaledHeaderHeight(cxTextHeight(Font), ScaleFactor);
end;

procedure TcxFieldListListBox.UpdateSelection;
var
  AParentForm: TCustomForm;
begin
  ItemIndex := Items.IndexOfObject(TcxPivotGridCustomizationForm(FieldChooser).SelectedObject);
  if (ItemIndex <> -1) and CanFocus then
  begin
    AParentForm := GetParentForm(Self);
    AParentForm.ActiveControl := Self;
  end;
end;

function TcxFieldListListBox.GetSelectedField: TcxPivotGridField;
begin
  if (ItemIndex < 0) or (ItemIndex >= Count) then
    Result := nil
  else
    Result := FieldByIndex(ItemIndex);
end;

procedure TcxFieldListListBox.Initialize;
begin
end;

procedure TcxFieldListListBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_ESCAPE) and (DragAndDropState <> ddsNone) then
    FinishDragAndDrop(False);
  inherited KeyDown(Key, Shift);
end;

procedure TcxFieldListListBox.LookAndFeelChanged(
  Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues);
begin
  inherited LookAndFeelChanged(Sender, AChangedValues);
  UpdateBackgroundColor;
  UpdateItemHeight;
end;

procedure TcxFieldListListBox.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  CheckItemIndex(Point(X, Y));
  TcxPivotGridCustomizationForm(FieldChooser).SelectedObject := SelectedField;
  if CanChangeFieldFilter then
  begin
    FieldChooser.UpdateHitTest;
    if PivotGrid.HitTest.HitAtFilter then
      FieldChooser.FieldFilterPopup(TcxPivotGridField(PivotGrid.HitTest.Field), FilterInitPopupHandler);
  end;
end;

function TcxFieldListListBox.StartDragAndDrop(const P: TPoint): Boolean;
var
  AItemIndex: Integer;
begin
  AItemIndex := ItemAtPos(P, True);
  if AItemIndex <> -1 then
  begin
    ItemIndex := AItemIndex;
    ItemIndex := AItemIndex;
  end;
  Result := (AItemIndex <> -1) and (SelectedField <> nil) and CanDrag(P.X, P.Y);
  if Result then
    TcxPivotGridCustomizationForm(FieldChooser).SetDragFieldToController(SelectedField);
end;

procedure TcxFieldListListBox.SynchronizeField(
  AField: TcxPivotGridField; AAreaIndex: Integer);
var
  I: Integer;
begin
  if FieldsType = fltAvailable then
    AField.Visible := False
  else
  begin
    (AField as IcxPivotGridField).AssignAreaIndex(ListType2FieldArea[FieldsType], AAreaIndex);
    if AField.Group <> nil then
    begin
      for I := 1 to AField.Group.FieldCount - 1 do
        (AField.Group.Fields[I] as IcxPivotGridField).AssignAreaIndex(ListType2FieldArea[FieldsType], AAreaIndex + I);
    end;
    AField.Visible := True;
  end;
end;

procedure TcxFieldListListBox.UpdateBackgroundColor;
begin
  Color := TcxPivotGridCustomizationForm(FieldChooser).GetCustomizationFormListBackgroundColor;
end;

function TcxFieldListListBox.GetAreaIndexOffset(
  AField: TcxPivotGridField; ACheckGroup: Boolean): Integer;
begin
  Result := 0;
  if AField <> nil then
  begin
    Inc(Result);
    if ACheckGroup and (AField.Group <> nil) then
      Inc(Result, AField.Group.FieldCount - 1);
  end;
end;

function TcxFieldListListBox.GetDragAndDropObjectClass: TcxDragAndDropObjectClass;
begin
  Result := TcxPivotGridListListBoxDragAndDropObject;
end;

procedure TcxFieldListListBox.CheckItemIndex(const P: TPoint);
var
  AItemIndex: Integer;
begin
  if Focused and (ItemIndex < 0) and (Count > 0) then
  begin
    if cxPointIsEqual(P, cxInvalidPoint) then
      AItemIndex := Items.IndexOfObject(TcxPivotGridCustomizationForm(FieldChooser).SelectedObject)
    else
      AItemIndex := ItemAtPos(P, False);
    if AItemIndex < 0 then
      AItemIndex := 0
    else
      if AItemIndex >= Count then
        AItemIndex := Count - 1;
    ItemIndex := AItemIndex;
    TcxPivotGridCustomizationForm(FieldChooser).SelectedObject := SelectedField;
  end;
end;

procedure TcxFieldListListBox.FilterInitPopupHandler(Sender: TObject);
var
  R: TRect;
  P: TPoint;
  AIndex: Integer;
  AField: TcxPivotGridField;
  AFilterPopup: TcxPivotGridFilterPopup;
begin
  AFilterPopup := TcxPivotGridFilterPopup(Sender);
  AField := AFilterPopup.Field;
  AIndex := Items.IndexOfObject(AField);
  if AIndex > -1 then
  begin
    R := ItemRect(AIndex);
    P := dxMapWindowPoint(Handle, PivotGrid.Handle, R.BottomRight);
    if not PivotGrid.UseRightToLeftAlignment then
    begin
      P := cxPointOffset(P, AFilterPopup.BoundsRect.BottomRight, False);
      AFilterPopup.BoundsRect := cxRectOffset(AFilterPopup.BoundsRect, P);
    end
    else
    begin
      P := cxPointOffset(P, cxRectLeftBottom(AFilterPopup.BoundsRect), False);
      AFilterPopup.BoundsRect := cxRectOffset(cxRectOffset(AFilterPopup.BoundsRect, P), -cxRectWidth(R), 0);
    end;
  end;
end;

function TcxFieldListListBox.GetFieldChooser: TcxPivotGridCustomCustomizationForm;
begin
  Result := Owner as TcxPivotGridCustomCustomizationForm;
end;

function TcxFieldListListBox.GetPainter: TcxCustomLookAndFeelPainter;
begin
  Result := FieldChooser.Painter;
end;

function TcxFieldListListBox.GetPivotGrid: TcxCustomPivotGrid;
begin
  Result := FieldChooser.PivotGrid;
end;

{ TcxFieldListListBoxFieldViewInfo }

constructor TcxFieldListListBoxFieldViewInfo.CreateEx(AOwner: TPersistent);
begin
  inherited CreateEx(AOwner);
  FTempFont := TFont.Create;
end;

destructor TcxFieldListListBoxFieldViewInfo.Destroy;
begin
  FreeAndNil(FTempFont);
  inherited Destroy;
end;

procedure TcxFieldListListBoxFieldViewInfo.Initialize(
  ACanvas: TcxCanvas; AScaleFactor: TdxScaleFactor; ASource: TcxPivotGridFieldHeaderCellViewInfo);
begin
  SetBounds(ASource.Bounds, ASource.ClipRect);
  inherited Initialize(ACanvas, AScaleFactor, ASource.Painter, ASource.ViewParams);
  FTempFont.Assign(FViewParams.Font);
  FTempFont.Height := AScaleFactor.Apply(FTempFont.Height, ASource.ScaleFactor);
  FViewParams.Font := FTempFont;
end;

{ TcxPivotGridListListBoxDragAndDropObject }

procedure TcxPivotGridListListBoxDragAndDropObject.BeginDragAndDrop;
begin
  inherited BeginDragAndDrop;
  HotSpot := cxPointInvert(CurMousePos);
  HotSpot.Y := HotSpot.Y mod FieldHeaderHeight;
end;

function TcxPivotGridListListBoxDragAndDropObject.CheckArea(
  const P: TPoint; var AInfo: TcxPivotGridDragDropAreaInfo): Boolean;
begin
  Result := inherited CheckArea(P, AInfo);
  if not ImmediateUpdate then
    Result := Result and HitTest.HitAtFieldList;
end;

procedure TcxPivotGridListListBoxDragAndDropObject.CreateDragImage;
var
  R: TRect;
begin
  DragImage := TcxDragImage.Create;
  R := Rect(0, 0, Control.ClientWidth, FieldHeaderHeight);
  DragImage.BoundsRect := R;
  PaintDragHeader(R);
end;

procedure TcxPivotGridListListBoxDragAndDropObject.DragAndDrop(
  const P: TPoint; var Accepted: Boolean);
var
  APoint: TPoint;
begin
  APoint := dxMapWindowPoint(Control.Handle, PivotGrid.Handle, P);
  CurMousePos := dxMapWindowPoint(Control.Handle, PivotGrid.Handle, CurMousePos);
  inherited DragAndDrop(APoint, Accepted);
end;

procedure TcxPivotGridListListBoxDragAndDropObject.EndDragAndDrop(Accepted: Boolean);
var
  AIndex: Integer;
  ASource, ADest: TcxFieldListListBox;
  AListType: TcxPivotGridCustomizationFormFieldListType;
begin
  if not Accepted then
    DirtyChanged
  else
    if ImmediateUpdate then
      inherited EndDragAndDrop(Accepted)
    else
    begin
      if Accepted then
      begin
        DragDropInfo.Visible := DragDropInfo.Visible and AAccepted;
        AListType := fltAvailable;
        if DragDropInfo.Visible then
          AListType := FieldArea2ListType[DragDropInfo.Area];
        ADest := TcxFieldListListBox(GetFieldListByType(AListType));
        ASource := Control as TcxFieldListListBox;
        AIndex := ASource.Items.IndexOfObject(Field);
        if (ASource = ADest) and DragDropInfo.Visible then
        begin
          if AIndex < DragDropInfo.AreaIndex then
            Dec(DragDropInfo.AreaIndex);
          ASource.Items.Delete(ASource.Items.IndexOfObject(Field));
        end;
        if not DragDropInfo.Visible then
        begin
          if ASource <> ADest then
            ADest.AddItem(Field.Caption, Field)
        end
        else
          ADest.Items.Insert(DragDropInfo.AreaIndex, Field.Caption, -1, Field);
        if ASource <> ADest then
          ASource.Items.FreeAndDelete(AIndex);
        with TcxPivotGridCustomizationForm((Control as TcxFieldListListBox).FieldChooser) do
        begin
          SelectedObject := Field;
          IsLayoutChanged := True;
        end;
        ADest.SetFocus;
      end;
    end;
end;

function TcxPivotGridListListBoxDragAndDropObject.GetPivotGrid: TcxCustomPivotGrid;
begin
  Result := (Control as TcxFieldListListBox).PivotGrid;
end;

function TcxPivotGridListListBoxDragAndDropObject.IsSameDropPlace: Boolean;
begin
  Result := inherited IsSameDropPlace;
{  if Result and HitTest.HitAtFieldList then
    Result := Field.AreaIndex = DragDropInfo.AreaIndex;}
  if ImmediateUpdate or not DragDropInfo.Visible then Exit;
  Result := (ListType2FieldArea[TcxFieldListListBox(Control).FieldsType] = DragDropInfo.Area) and
    (Field = DragDropInfo.Field);
end;

function TcxPivotGridListListBoxDragAndDropObject.GetField: TcxPivotGridField;
begin
  Result := FieldViewInfo.Field as TcxPivotGridField;
end;

function TcxPivotGridListListBoxDragAndDropObject.GetFieldHeaderHeight: Integer;
begin
  Result := (Control as TcxFieldListListBox).ItemHeight;
end;

{ TcxPivotGridCustomizationForm }

constructor TcxPivotGridCustomizationForm.Create(AOwner: TComponent);
begin
  inherited CreateNew(nil);
  Constraints.MinWidth := ScaleFactor.Apply(cxPivotGridCustomizationMinWidth);
  Constraints.MinHeight := ScaleFactor.Apply(cxPivotGridCustomizationMinHeight);
end;

procedure TcxPivotGridCustomizationForm.AddToClickHandler(Sender: TObject);
var
  AField: TcxPivotGridField;
begin
  PivotGrid.BeginUpdate;
  try
    AField := FieldList.SelectedField;
    AField.Area := TcxPivotGridFieldArea(AreaList.ItemObject);
    AField.AreaIndex := MaxInt;
    AField.Visible := True;
  finally
    PivotGrid.EndUpdate;
  end;
end;

procedure TcxPivotGridCustomizationForm.AreaListClickHandler(Sender: TObject);
begin
  SyncVisibility;
end;

procedure TcxPivotGridCustomizationForm.CMBiDiModeChanged(var Message: TMessage);
begin
  inherited;
  if AddButton <> nil then
    AddButton.Align := GetButtonAlign;
end;

procedure TcxPivotGridCustomizationForm.DoCreateControls;
begin
  inherited;
  BorderWidth := ScaleFactor.Apply(cxPivotGridControlsIndent);
  // caption
  FieldListCaption := CreateCaption;
  // bottom pane
  BottomPanel := CreateBottomPanel;
  AddButton := CreateButton;
  AreaList := CreateAreaList;
  // list box
  FieldList := CreateFieldList;
  FieldList.ItemHeight := Max(Painter.ScaledHeaderHeight(cxTextHeight(Font), ScaleFactor), FieldItemHeight);
  FieldList.OnClick := FieldListClickHandler;
end;

procedure TcxPivotGridCustomizationForm.DoUpdateSelection;
begin
  FieldList.UpdateSelection;
  SyncArea;
end;

function TcxPivotGridCustomizationForm.GetButtonAlign: TAlign;
const
  AAlign: array[Boolean] of TAlign = (alLeft, alRight);
begin
  Result := AAlign[UseRightToLeftAlignment];
end;

function TcxPivotGridCustomizationForm.GetCustomizationFormListBackgroundColor: TColor;
begin
  Result := Painter.GetCustomizationFormListBackgroundColor;
end;

function TcxPivotGridCustomizationForm.GetFieldListByType(
  AListType: TcxPivotGridCustomizationFormFieldListType): TObject;
begin
  Result := FieldList;
end;

procedure TcxPivotGridCustomizationForm.Init;
begin
  inherited Init;
  SetBounds(Left, Top,
    ScaleFactor.Apply(cxPivotGridCustomizationDefaultHeight),
    ScaleFactor.Apply(cxPivotGridCustomizationDefaultWidth));
end;

function TcxPivotGridCustomizationForm.CreateAreaList: TcxComboBox;
begin
  Result := TcxComboBox.Create(Self);
  Result.Parent := BottomPanel;
  Result.Align := alClient;
  Result.AlignWithMargins := True;
  with Result.Properties.Items do
  begin
    AddObject(cxGetResourceString(@scxRowArea), TObject(faRow));
    AddObject(cxGetResourceString(@scxColumnArea), TObject(faColumn));
    AddObject(cxGetResourceString(@scxFilterArea), TObject(faFilter));
    AddObject(cxGetResourceString(@scxDataArea), TObject(faData));
  end;
  Result.Properties.DropDownListStyle := lsFixedList;
  Result.OnClick := AreaListClickHandler;
end;

function TcxPivotGridCustomizationForm.CreateBottomPanel: TcxGroupBox;
begin
  Result := TcxGroupBox.Create(Self);
  Result.Parent := Self;
  Result.PanelStyle.Active := True;
  Result.Style.BorderStyle := ebsNone;
  Result.Style.LookAndFeel.NativeStyle := True;
  Result.Transparent := True;
  Result.Align := alBottom;
end;

function TcxPivotGridCustomizationForm.CreateButton: TcxButton;
begin
  Result := TcxButton.Create(Self);
  Result.Parent := BottomPanel;
  BottomPanel.Height := Result.Height + Result.Margins.Bottom + Result.Margins.Top;
  Result.Caption := cxGetResourceString(@scxAddTo);
  Result.Align := GetButtonAlign;
  Result.AlignWithMargins := True;
  Result.OnClick := AddToClickHandler;
end;

function TcxPivotGridCustomizationForm.CreateCaption: TcxLabel;
begin
  Result := TcxLabel.Create(Self);
  Result.Parent := Self;
  Result.Transparent := True;
  Result.Align := alTop;
  Result.AlignWithMargins := True;
  Result.Style.TransparentBorder := False;
  Result.Caption := cxGetResourceString(@scxDragItems);
end;

function TcxPivotGridCustomizationForm.CreateFieldList: TcxFieldListListBox;
begin
  Result := TcxFieldListListBox.Create(Self);
  Result.Parent := Self;
  Result.Align := alClient;
  Result.AlignWithMargins := True;
  Result.OnDblClick := OnItemDblClick;
end;

procedure TcxPivotGridCustomizationForm.OnItemDblClick(Sender: TObject);
begin
  UpdateHitTest;
  if not PivotGrid.HitTest.HitAtFilter and AddButton.Enabled then
    AddButton.Click;
end;

procedure TcxPivotGridCustomizationForm.Resize;
begin
  inherited Resize;
{  if (FieldList <> nil) and FieldList.HandleAllocated then
    FieldList.Invalidate;}
end;

procedure TcxPivotGridCustomizationForm.FieldListClickHandler(
  Sender: TObject);
begin
  SyncArea;
end;

procedure TcxPivotGridCustomizationForm.SyncArea;
begin
  if FieldList.SelectedField <> nil then
    AreaList.ItemObject := TObject(FieldList.SelectedField.Area)
  else
    AreaList.ItemObject := nil;
  SyncVisibility;
end;

procedure TcxPivotGridCustomizationForm.SyncVisibility;
begin
  AddButton.Enabled := (FieldList.ItemIndex >= 0) and
    (TcxPivotGridFieldArea(AreaList.ItemObject) in FieldList.SelectedField.AllowedAreas);
  AreaList.Enabled := FieldList.ItemIndex >= 0;
end;

procedure TcxPivotGridCustomizationForm.RefreshList;
begin
  inherited;
  cxPopulateListBox(Self, FieldList, fltAvailable);
  UpdateSelection;
end;

{ TcxPivotGridDragAndDropObject }

function TcxPivotGridOLAPTreeDragAndDropObject.GetField: TcxPivotGridField;
begin
  Result := FieldViewInfo.Field as TcxPivotGridField;
end;

function TcxPivotGridOLAPTreeDragAndDropObject.GetFieldChooser: TcxPivotGridOLAPCustomizationForm;
begin
  Result := TreeView.Form;
end;

function TcxPivotGridOLAPTreeDragAndDropObject.GetFieldHeaderHeight: Integer;
begin
  Result := PivotGrid.LookAndFeelPainter.ScaledHeaderHeight(cxTextHeight(FieldViewInfo.Font), FieldChooser.ScaleFactor);
end;

procedure TcxPivotGridOLAPTreeDragAndDropObject.BeginDragAndDrop;
begin
  inherited BeginDragAndDrop;
  HotSpot := cxPointInvert(CurMousePos);
  HotSpot.Y := HotSpot.Y mod FieldHeaderHeight;
  IndentOffset := TreeView.Selected.DisplayRect(True).Left;
end;

function TcxPivotGridOLAPTreeDragAndDropObject.CheckArea(
  const P: TPoint; var AInfo: TcxPivotGridDragDropAreaInfo): Boolean;
begin
  Result := inherited CheckArea(P, AInfo);
  if not ImmediateUpdate then
    Result := Result and HitTest.HitAtFieldList;
end;

procedure TcxPivotGridOLAPTreeDragAndDropObject.CreateDragImage;
var
  R: TRect;
begin
  DragImage := TcxDragImage.Create;
  R := Rect(0, 0, cxRectWidth(TreeView.Selected.DisplayRect(True)) +
    cxPivotGridHierarchyImages.Width + cxTextOffset * 2, FieldHeaderHeight);
  DragImage.BoundsRect := R;
  PaintDragHeader(R);
end;

procedure TcxPivotGridOLAPTreeDragAndDropObject.DragAndDrop(
  const P: TPoint; var Accepted: Boolean);
var
  APoint: TPoint;
begin
  APoint := dxMapWindowPoint(TreeView.Handle, PivotGrid.Handle, P);
  CurMousePos := dxMapWindowPoint(TreeView.Handle, PivotGrid.Handle, CurMousePos);
  inherited DragAndDrop(APoint, Accepted);
end;

procedure TcxPivotGridOLAPTreeDragAndDropObject.EndDragAndDrop(Accepted: Boolean);
var
  AIndex: Integer;
  ASource, ADest: TcxFieldListListBox;
  AListType: TcxPivotGridCustomizationFormFieldListType;
begin
  if not Accepted then
    DirtyChanged
  else
    if (Field <> nil) and (DragDropInfo = nil) then
    begin
      if DragImage <> nil then
        DragImage.Hide;
      if Field.Visible then
        Field.Visible := False;
    end
    else
      if ImmediateUpdate then
        inherited EndDragAndDrop(Accepted)
      else
      begin
        if Accepted then
        begin
          DragDropInfo.Visible := DragDropInfo.Visible and AAccepted;
          AListType := fltAvailable;
          if DragDropInfo.Visible then
            AListType := FieldArea2ListType[DragDropInfo.Area];
          ADest := TcxFieldListListBox(GetFieldListByType(AListType));
          ASource := Control as TcxFieldListListBox;
          AIndex := ASource.Items.IndexOfObject(Field);
          if (ASource = ADest) and DragDropInfo.Visible then
          begin
            if AIndex < DragDropInfo.AreaIndex then
              Dec(DragDropInfo.AreaIndex);
            ASource.Items.Delete(ASource.Items.IndexOfObject(Field));
          end;
          if not DragDropInfo.Visible then
          begin
            if ASource <> ADest then
              ADest.AddItem(Field.Caption, Field)
          end
          else
            ADest.Items.Insert(DragDropInfo.AreaIndex, Field.Caption, -1, Field);
          if ASource <> ADest then
            ASource.Items.Delete(AIndex);
          with TcxPivotGridCustomizationForm((Control as TcxFieldListListBox).FieldChooser) do
          begin
            SelectedObject := Field;
            IsLayoutChanged := True;
          end;
          ADest.SetFocus;
        end;
      end;
end;

function TcxPivotGridOLAPTreeDragAndDropObject.GetClientCursorPos: TPoint;
begin
  Result := TreeView.ScreenToClient(GetMouseCursorPos);
end;

function TcxPivotGridOLAPTreeDragAndDropObject.GetImmediateStart: Boolean;
begin
  Result := True;
end;

{ TcxPivotGridOLAPStructureInnerTreeView }

function TcxPivotGridOLAPStructureInnerTreeView.CalculateHitTest(AHitTest: TcxPivotGridHitTest): Boolean;
var
  P, Origin: TPoint;
  AInfo: TcxPivotGridDragDropAreaInfo;
begin
  Result := Visible;
  if not Result then Exit;
  P := dxMapWindowPoint(PivotGrid.Handle, Handle, AHitTest.HitPoint);
  Origin := ClientToScreen(cxNullPoint);
  Result := cxRectPtIn(ClientRect, P);
  if not Result then Exit;

  AInfo := TcxPivotGridCustomizationForm(FieldChooser).GetDragDropInfo;
  AInfo.AreaIndex := 0;
  AInfo.DisplayBounds := cxRectOffset(ClientRect, Origin);
  AInfo.Field := nil;
end;

procedure TcxPivotGridOLAPStructureInnerTreeView.BeginDragAndDrop;
begin
  DragAndDropObject.DoBeginDragAndDrop;
  MouseCapture := True;
  FDragAndDropPrevCursor := Screen.Cursor;
  DragAndDropState := ddsInProcess;
end;

procedure TcxPivotGridOLAPStructureInnerTreeView.DoCancelMode;
begin
  MouseCaptureObject := nil;
end;

procedure TcxPivotGridOLAPStructureInnerTreeView.DragAndDrop(
  const P: TPoint; var Accepted: Boolean);
begin
  DragAndDropObject.DoDragAndDrop(P, Accepted);
end;

procedure TcxPivotGridOLAPStructureInnerTreeView.EndDragAndDrop(Accepted: Boolean);
begin
  DragAndDropState := ddsNone;
  Screen.Cursor := FDragAndDropPrevCursor;
  MouseCapture := False;
  MouseCaptureObject := nil;
  DragAndDropObject.DoEndDragAndDrop(Accepted);
end;

procedure TcxPivotGridOLAPStructureInnerTreeView.FinishDragAndDrop(Accepted: Boolean);
begin
  if FFinishingDragAndDrop then Exit;
  FFinishingDragAndDrop := True;
  try
    if DragAndDropState = ddsInProcess then
      EndDragAndDrop(Accepted)
    else
      DragAndDropState := ddsNone;
    FreeAndNil(FDragAndDropObject);
  finally
    FFinishingDragAndDrop := False;
  end;
end;

function TcxPivotGridOLAPStructureInnerTreeView.CanEdit(Node: TTreeNode): Boolean;
begin
  Result := False;
end;

procedure TcxPivotGridOLAPStructureInnerTreeView.MouseDown(
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

  procedure ProcessDragAndDrop;
  begin
    if (Button = mbLeft) and not (ssDouble in Shift) and StartDragAndDrop(Point(X, Y)) then
{      if DragAndDropObject.ImmediateStart then
        BeginDragAndDrop
      else}
        DragAndDropState := ddsStarting
    else
      FinishDragAndDrop(False);
  end;

var
  ALink: TcxObjectLink;
  AOriginalBounds: TRect;
begin
  FMouseDownPos := Point(X, Y);
  ALink := cxAddObjectLink(Self);
  try
    if not (ssDouble in Shift) then  // to allow form showing on dbl click
    begin
      AOriginalBounds := BoundsRect;
      SetFocus;
      if ALink.Ref = nil then Exit;
      // to workaround the bug in VCL with parented forms
      if (GetParentForm(Self) <> nil) and (GetParentForm(Self).ActiveControl = Self) and
        not Focused then
        Windows.SetFocus(Handle);
      if not Focused then
      begin
        MouseCapture := False;
        Exit;
      end;
    end;
    ProcessDragAndDrop;
    if ALink.Ref = nil then Exit;
    inherited;
  finally
    cxRemoveObjectLink(ALink);
  end;
end;

procedure TcxPivotGridOLAPStructureInnerTreeView.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  AAccepted: Boolean;
begin
  inherited;
  if (DragAndDropState = ddsStarting) and not IsPointInDragDetectArea(FMouseDownPos, X, Y) then
    BeginDragAndDrop;
  if DragAndDropState = ddsInProcess then
  begin
    AAccepted := False;
    DragAndDrop(Point(X, Y), AAccepted);
  end;
end;

procedure TcxPivotGridOLAPStructureInnerTreeView.MouseUp(
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FinishDragAndDrop(True);
  MouseCaptureObject := nil;
  inherited;
end;

function TcxPivotGridOLAPStructureInnerTreeView.StartDragAndDrop(const P: TPoint): Boolean;
var
  ANode: TTreeNode;
  AField: TcxPivotGridField;
  AStructure: TcxPivotGridOLAPStructureNode;
begin
  Result := DragAndDropState = ddsNone;
  if Result then
  begin
    Result := False;
    ANode := GetNodeAt(P.X, P.Y);
    if (ANode = nil) or (ANode.Level <= 1) or not (htOnItem in GetHitTestInfoAt(P.X, P.Y)) then Exit;
    AStructure := TcxPivotGridOLAPStructureNode(ANode.Data);
    AField := AStructure.GetLinkedField(PivotGrid);
    Result := ((AStructure.NodeType in [ntField, ntSet]) and not (ntGroup in AStructure.AggregateType)) or
      (AStructure.NodeType = ntGroup) and (AField <> nil);
    if Result then
      Form.SetDragFieldToController(AField);
  end;
end;

function TcxPivotGridOLAPStructureInnerTreeView.GetDragAndDropObject: TcxDragAndDropObject;
begin
  if FDragAndDropObject = nil then
  begin
    FDragAndDropObject := TcxPivotGridOLAPTreeDragAndDropObject.Create(PivotGrid);
    TcxPivotGridOLAPTreeDragAndDropObject(FDragAndDropObject).FTreeView := Self;
  end;
  Result := FDragAndDropObject;
end;

function TcxPivotGridOLAPStructureInnerTreeView.GetFieldChooser: TcxPivotGridCustomCustomizationForm;
begin
  Result := Owner.Owner as TcxPivotGridCustomCustomizationForm;
end;

function TcxPivotGridOLAPStructureInnerTreeView.GetForm: TcxPivotGridOLAPCustomizationForm;
var
  AParent: TWinControl;
begin
  AParent := Self.Parent;
  while (AParent <> nil) and not (AParent is TcxPivotGridOLAPCustomizationForm) do
    AParent := AParent.Parent;
  Result := AParent as TcxPivotGridOLAPCustomizationForm;
end;

function TcxPivotGridOLAPStructureInnerTreeView.GetPivotGrid: TcxCustomPivotGrid;
begin
  Result := Form.PivotGrid;
end;

function TcxPivotGridOLAPStructureInnerTreeView.GetIsDestroying: Boolean;
begin
  Result := csDestroying in ComponentState;
end;

procedure TcxPivotGridOLAPStructureInnerTreeView.SetDragAndDropState(
  AValue: TcxDragAndDropState);
begin
  if FDragAndDropState <> AValue then
  begin
    FDragAndDropState := AValue;
    if (AValue = ddsNone) and not FFinishingDragAndDrop then DoCancelMode;
  end;
end;

procedure TcxPivotGridOLAPStructureInnerTreeView.SetMouseCaptureObject(AValue: TObject);
var
  AIMouseCaptureObject: IcxMouseCaptureObject;
  AMouseWasCaught: Boolean;
begin
  if FMouseCaptureObject <> AValue then
  begin
    if (FMouseCaptureObject <> nil) and
      Supports(FMouseCaptureObject, IcxMouseCaptureObject, AIMouseCaptureObject) then
      AIMouseCaptureObject.DoCancelMode;
    FMouseCaptureObject := AValue;
    AMouseWasCaught := MouseCapture;
    MouseCapture := FMouseCaptureObject <> nil;
    if AMouseWasCaught and not MouseCapture and (DragAndDropState = ddsStarting) then
      Perform(WM_CANCELMODE, 0, 0);
  end;
end;

procedure TcxPivotGridOLAPStructureInnerTreeView.CNKeyDown(var Message: TWMKeyDown);
begin
  if DragAndDropState <> ddsNone then
  begin
    FinishDragAndDrop(False);
    Message.Result := 1;
    Exit;
  end;
  inherited;
end;

procedure TcxPivotGridOLAPStructureInnerTreeView.WMCancelMode(var Message: TWMCancelMode);
begin
  inherited;
  FinishDragAndDrop(False);
  DoCancelMode;
end;

{ TcxPivotGridOLAPTreeView }

class function TcxPivotGridOLAPTreeView.GetTreeViewClass: TcxCustomInnerTreeViewClass;
begin
  Result := TcxPivotGridOLAPStructureInnerTreeView;
end;

{ TcxPivotGridOLAPCustomizationForm }

constructor TcxPivotGridOLAPCustomizationForm.Create(AOwner: TComponent);
begin
  inherited CreateNew(nil);
  Constraints.MinWidth := Round(ScaleFactor.Apply(cxPivotGridCustomizationMinWidth) * 1.3);
  Constraints.MinHeight := Round(ScaleFactor.Apply(cxPivotGridCustomizationMinHeight) * 2.5);
  ExpandingInfo := TList.Create;
end;

destructor TcxPivotGridOLAPCustomizationForm.Destroy;
begin
  FreeAndNil(ExpandingInfo);
  inherited Destroy;
end;

procedure TcxPivotGridOLAPCustomizationForm.RefreshList;
begin
  SaveExpanding;
  inherited;
  PopulateStructure;
  UpdateSelection;
end;

procedure TcxPivotGridOLAPCustomizationForm.DoCreateControls;
begin
  inherited;
  BorderWidth := ScaleFactor.Apply(cxPivotGridControlsIndent);
  // caption
  FieldListCaption := TcxLabel.Create(Self);
  FieldListCaption.Parent := Self;
  FieldListCaption.Transparent := True;
  FieldListCaption.Align := alTop;
  FieldListCaption.AlignWithMargins := True;
  FieldListCaption.Caption := cxGetResourceString(@scxDragItems);
  // bottom pane
  FieldList := TcxPivotGridOLAPTreeView.Create(Self);
  FieldList.Parent := Self;
  FieldList.ToolTips := False;
  FieldList.Align := alClient;
  FieldList.AlignWithMargins := True;
  FieldList.Images := cxPivotGridHierarchyImages;
  FieldList.OnGetImageIndex := OnGetTreeViewNodeImageIndex;
  FieldList.OnGetSelectedIndex := OnGetTreeViewNodeImageIndex;
  FieldList.OnDblClick := OnItemDblClick;
  FieldList.OnCustomDrawItem := DrawStructureItem;
end;

procedure TcxPivotGridOLAPCustomizationForm.DoUpdateSelection;
begin
end;

procedure TcxPivotGridOLAPCustomizationForm.DrawStructureItem(
  Sender: TCustomTreeView; Node: TTreeNode;
  State: TCustomDrawState; var DefaultDraw: Boolean);
var
  AField: TcxPivotGridField;
  AInfo: TcxPivotGridOLAPStructureNode;
begin
  DefaultDraw := True;
  if (Node = nil) or (Node.Data = nil) then Exit;
  AInfo := TcxPivotGridOLAPStructureNode(Node.Data);
  Sender.Canvas.Font.Color := clWindowText;
  if AInfo.NodeType = ntCube then
    Sender.Canvas.Font.Style := [fsBold]
  else
    if (AInfo.NodeType = ntField) or (AInfo.NodeType = ntSet) then
    begin
      AField := AInfo.GetLinkedField(PivotGrid);
      if (AField <> nil) and (((AField.Group <> nil) and AField.Group.Visible) or ((AField.Group = nil) and AField.Visible)) then
      begin
        Sender.Canvas.Font.Color := clBtnShadow;
        Sender.Canvas.Font.Style := [fsBold, fsItalic]
      end
      else
        Sender.Canvas.Font.Style := [];
    end;
end;

function TcxPivotGridOLAPCustomizationForm.GetCustomizationFormListBackgroundColor: TColor;
begin
  Result := Painter.GetCustomizationFormListBackgroundColor;
end;

function TcxPivotGridOLAPCustomizationForm.GetStructureDisplayText(AStructure: TcxPivotGridOLAPStructureNode): string;
var
  I: Integer;
  AField: TcxPivotGridOLAPField;
begin
  for I := 0 to PivotGrid.FieldCount - 1 do
  begin
    AField := TcxPivotGridOLAPField(PivotGrid.Fields[I]);
    if AField.Structure = AStructure then
    begin
      Result := AField.Caption;
      Exit;
    end;
  end;
  Result := AStructure.DisplayText;
end;

procedure TcxPivotGridOLAPCustomizationForm.OnGetTreeViewNodeImageIndex(Sender: TObject; Node: TTreeNode);
var
  S: TcxPivotGridOLAPStructureNode;
begin
  S := TcxPivotGridOLAPStructureNode(Node.Data);
  if S.NodeType <> ntFolder then Exit;
  Node.ImageIndex := S.ImageIndex + Byte(Node.Expanded);
  Node.SelectedIndex := Node.ImageIndex;
end;

procedure TcxPivotGridOLAPCustomizationForm.OnItemDblClick(Sender: TObject);
begin
end;

procedure TcxPivotGridOLAPCustomizationForm.Init;
begin
  inherited Init;
  Width := Round(ScaleFactor.Apply(cxPivotGridCustomizationMinWidth) * 1.3);
  Height := Round(ScaleFactor.Apply(cxPivotGridCustomizationMinHeight) * 2.8);
end;

procedure TcxPivotGridOLAPCustomizationForm.Localize;
begin
  Caption := cxGetResourceString(@scxMeasureGroups);
end;

procedure TcxPivotGridOLAPCustomizationForm.PopulateStructure;
var
  ATopIndex: Integer;
  ATopNode, ANode: TTreeNode;
  S: TcxPivotGridOLAPStructureNode;

  procedure SyncNodes(ASource: TcxPivotGridOLAPStructureNode; ADest: TTreeNode);
  var
    ADestChild: TTreeNode;
    AExpanded: Boolean;
  begin
    ADest.ImageIndex := ASource.ImageIndex;
    ADest.SelectedIndex := ASource.ImageIndex;
    if ADest.AbsoluteIndex = ATopIndex then
      ATopNode := ADest;
    AExpanded := ExpandingInfo.IndexOf(ASource) >= 0;
    ASource := TcxPivotGridOLAPStructureNode(ASource.First);
    while ASource <> nil do
    begin
      ADestChild := FieldList.Items.AddChildObject(ADest, GetStructureDisplayText(ASource), ASource);
      SyncNodes(ASource,  ADestChild);
      ASource := TcxPivotGridOLAPStructureNode(ASource.Next);
    end;
    if ADest.Count > 0 then
      ADest.Expanded := AExpanded;
  end;

begin
  ATopIndex := -1;
  ATopNode := nil;
  if FieldList.TopItem <> nil then
    ATopIndex := FieldList.TopItem.AbsoluteIndex;
  FieldList.Items.BeginUpdate;
  try
    FieldList.Items.Clear;
    S := Structure;
    ANode := FieldList.Items.AddObject(nil, S.DisplayText, S);
    SyncNodes(S, ANode);
    ANode.Expanded := True;
    if ATopNode <> nil then
      FieldList.TopItem := ATopNode;
  finally
    FieldList.Items.EndUpdate;
  end;
  StructureIsPopulated := True;
  CurrentStructure := Structure;
end;

procedure TcxPivotGridOLAPCustomizationForm.SaveExpanding;

  procedure CheckTreeNode(ANode: TTreeNode);
  var
    I: Integer;
  begin
    if ANode.Expanded then
      ExpandingInfo.Add(ANode.Data);
    for I := 0 to ANode.Count - 1 do
      CheckTreeNode(ANode.Item[I]);
  end;

begin
  ExpandingInfo.Clear;
  if FieldList.Items.Count = 0 then Exit;
  CheckTreeNode(FieldList.Items[0]);
end;

function TcxPivotGridOLAPCustomizationForm.GetStructure: TcxPivotGridOLAPStructureNode;
begin
  Result := PivotGrid.OLAPDataSource.Structure;
end;

initialization
  cxPivotGridCustomizationFormClass := TcxPivotGridCustomizationForm;
end.

