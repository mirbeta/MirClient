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

unit cxDBExtLookupComboBox;

{$I cxVer.inc}

interface

uses
  Types, Variants, Windows, Classes, Controls, DB, Graphics, Messages, SysUtils, Forms,
  cxClasses, cxContainer, cxControls, cxCustomData, cxDB, cxDBData, cxDBEdit,
  cxDBLookupEdit, cxEditConsts, cxGrid, cxGridCustomTableView, cxEdit, cxTextEdit,
  cxGridCustomView, cxGridStrs, cxGridTableView, cxLookAndFeels, cxLookupEdit;

type
  { TcxExtLookupGrid }

  TcxExtLookupGrid = class(TcxGrid)
  private
    FEditable: Boolean;
    FMousePos: TPoint;
    FPopupMouseMoveLocked: Boolean;
    FPrevOnKeyDown: TKeyEvent;
    FPrevOnMouseDown: TMouseEvent;
    FPrevOnMouseMove: TMouseMoveEvent;
    FPrevOnMouseUp: TMouseEvent;
    FRowPressed: Boolean;
    FOnCloseUp: TcxLookupGridCloseUpEvent;
    function GetView: TcxCustomGridTableView;
    procedure ViewKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ViewMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ViewMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ViewMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  protected
    procedure DoCancelMode; override;
    procedure DoCloseUp(AAccept: Boolean); virtual;
    function IsDataRow(AHitTest: TcxCustomGridHitTest): Boolean;
    property Editable: Boolean read FEditable write FEditable;
    property PopupMouseMoveLocked: Boolean read FPopupMouseMoveLocked write FPopupMouseMoveLocked;
    property OnCloseUp: TcxLookupGridCloseUpEvent read FOnCloseUp write FOnCloseUp;
  public
    property View: TcxCustomGridTableView read GetView;
  end;

  { TcxCustomExtLookupComboBoxProperties }

  TcxCustomExtLookupComboBoxProperties = class(TcxCustomDBLookupEditProperties);

  { TcxExtLookupComboBoxProperties }

  TcxExtLookupComboBoxProperties = class(TcxCustomExtLookupComboBoxProperties)
  private
    FAutoSearchOnPopup: Boolean;
    FDestroying: Boolean;
    FFocusPopup: Boolean;
    FGrid: TcxExtLookupGrid;
    FInCheckListFieldItem: Boolean;
    FListFieldItem: TcxCustomGridTableItem;
    FPrevColumnFiltering: Boolean;
    FPrevColumnsQuickCustomization: Boolean;
    FPrevPullFocusing: Boolean;
    FPrevImmediateEditor: Boolean;
    FPrevIncSearch: Boolean;
    FPrevMultiSelect: Boolean;
    FView: TcxCustomGridTableView;
    function GetGrid: TcxExtLookupGrid;
    function GetGridMode: Boolean;
    function GetListFieldIndex: Integer;
    function GetListFieldItem: TcxCustomGridTableItem;
    procedure SetGridMode(Value: Boolean);
    procedure SetListFieldItem(Value: TcxCustomGridTableItem);
    procedure SetView(Value: TcxCustomGridTableView);
  protected
    procedure CheckListFieldItem;
    procedure DeinitializeDataController; override;
    procedure FreeNotification(Sender: TComponent); override;
    function GetIncrementalFiltering: Boolean; override;
    function GetListIndex: Integer; override;
    procedure InitializeDataController; override;
    procedure LinkView(AView: TcxCustomGridTableView);
    function PopupWindowCapturesFocus: Boolean; override;
    procedure UnlinkView(AView: TcxCustomGridTableView);
    // LookupGrid methods
    function GetLookupGridActiveControl: TWinControl; override;
    function GetLookupGridCanResize: Boolean; override;
    function GetLookupGridColumnCount: Integer; override;
    function GetLookupGridControl: TWinControl; override;
    function GetLookupGridDataController: TcxCustomDataController; override;
    function GetLookupGridVisualAreaPreferredWidth: Integer; override;
    function GetLookupGridNearestPopupHeight(AHeight: Integer): Integer; override;
    function GetLookupGridPopupHeight(ADropDownRowCount: Integer): Integer; override;
    function IsLookupGridMouseOverList(const P: TPoint): Boolean; override;
    procedure LookupGridDeinitialize; override;
    procedure LookupGridDroppedDown(const AFindStr: string); override;
    procedure LookupGridInitEvents(AOnClick, AOnFocusedRowChanged: TNotifyEvent;
      AOnCloseUp: TcxLookupGridCloseUpEvent); override;
    procedure LookupGridInitialize; override;
    procedure LookupGridInitLookAndFeel(ALookAndFeel: TcxLookAndFeel;
      AColor: TColor; AFont: TFont); override;
    procedure LookupGridLockMouseMove; override;
    procedure LookupGridMakeFocusedRowVisible; override;
    procedure LookupGridUnlockMouseMove; override;
    // DBLookupGrid methods
    procedure DBLookupGridBeginUpdate; override;
    procedure DBLookupGridCheckColumnByFieldName(const AFieldName: string); override;
    procedure DBLookupGridCreateColumnsByFieldNames(const AFieldNames: string); override;
    procedure DBLookupGridEndUpdate; override;
    function GetDBLookupGridColumnField(AIndex: Integer): TField; override;
    function GetDBLookupGridColumnFieldName(AIndex: Integer): string; override;
    function GetDBLookupGridColumnIndexByFieldName(const AFieldName: string): Integer; override;
    function GetDBLookupGridDataController: TcxDBDataController; override;
    procedure SetIncrementalFilteringOptions(Value: TcxTextEditIncrementalFilteringOptions); override;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    class function GetContainerClass: TcxContainerClass; override;
    class function IsViewSupported(Value: TcxCustomGridTableView): Boolean;
    property Grid: TcxExtLookupGrid read GetGrid;
    property ListFieldIndex: Integer read GetListFieldIndex;
  published
    property Alignment;
    property AssignedValues;
    property AutoSearchOnPopup: Boolean read FAutoSearchOnPopup write FAutoSearchOnPopup default True;
    property AutoSelect;
    property ButtonGlyph;
    property CaseSensitiveSearch;
    property CharCase;
    property ClearKey;
    property DropDownAutoSize;
    property DropDownHeight;
    property DropDownListStyle;
    property DropDownRows;
    property DropDownSizeable;
    property DropDownWidth;
    property FocusPopup: Boolean read FFocusPopup write FFocusPopup default False;
    property GridMode: Boolean read GetGridMode write SetGridMode default False;
    property HideSelection;
    property ImeMode;
    property ImeName;
    property ImmediateDropDown;
    property ImmediateDropDownWhenActivated;
    property ImmediateDropDownWhenKeyPressed;
    property ImmediatePost;
    property IncrementalFiltering;
    property IncrementalFilteringOptions default [];
    property View: TcxCustomGridTableView read FView write SetView; // before
    property KeyFieldNames;
    property ListFieldItem: TcxCustomGridTableItem read GetListFieldItem write SetListFieldItem;
    property MaxLength;
    property OEMConvert;
    property PopupAlignment;
    property PostPopupValueOnTab;
    property ReadOnly;
    property Revertable;
    property UseLeftAlignmentOnEditing;
    property ValidateOnEnter;
    property ValidationErrorIconAlignment;
    property ValidationOptions;
    property OnChange;
    property OnCloseUp;
    property OnEditValueChanged;
    property OnInitPopup;
    property OnNewLookupDisplayText;
    property OnPopup;
    property OnValidate;
  end;

  { TcxCustomExtLookupComboBox }

  TcxCustomExtLookupComboBox = class(TcxCustomDBLookupEdit)
  private
    function GetActiveProperties: TcxExtLookupComboBoxProperties;
    function GetProperties: TcxExtLookupComboBoxProperties;
    procedure SetProperties(Value: TcxExtLookupComboBoxProperties);
  protected
    function CanDropDown: Boolean; override;
  public
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    property ActiveProperties: TcxExtLookupComboBoxProperties
      read GetActiveProperties;
    property EditValue;
    property Properties: TcxExtLookupComboBoxProperties read GetProperties
      write SetProperties;
    property Text;
  end;

  { TcxExtLookupComboBox }

  TcxExtLookupComboBox = class(TcxCustomExtLookupComboBox)
  published
    property Anchors;
    property AutoSize;
    property BeepOnEnter;
    property BiDiMode;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ImeMode;
    property ImeName;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Properties;
    property EditValue;
    property ShowHint;
    property Style;
    property StyleDisabled;
    property StyleFocused;
    property StyleHot;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnEditing;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

  { TcxDBExtLookupComboBox }

  TcxDBExtLookupComboBox = class(TcxCustomExtLookupComboBox)
  private
    function GetDataBinding: TcxDBTextEditDataBinding;
    procedure SetDataBinding(Value: TcxDBTextEditDataBinding);
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
  protected
    class function GetDataBindingClass: TcxEditDataBindingClass; override;
  published
    property Anchors;
    property AutoSize;
    property BeepOnEnter;
    property BiDiMode;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DataBinding: TcxDBTextEditDataBinding read GetDataBinding
      write SetDataBinding;
    property DragMode;
    property Enabled;
    property ImeMode;
    property ImeName;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Properties;
    property ShowHint;
    property Style;
    property StyleDisabled;
    property StyleFocused;
    property StyleHot;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnEditing;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

  { TcxEditRepositoryExtLookupComboBoxItem }

  TcxEditRepositoryExtLookupComboBoxItem = class(TcxEditRepositoryItem)
  private
    function GetProperties: TcxExtLookupComboBoxProperties;
    procedure SetProperties(Value: TcxExtLookupComboBoxProperties);
  public
    class function GetEditPropertiesClass: TcxCustomEditPropertiesClass; override;
  published
    property Properties: TcxExtLookupComboBoxProperties read GetProperties write SetProperties;
  end;

implementation

uses
   cxGeometry, cxGridFilterHelpers;

type
  TcxCustomGridTableOptionsBehaviorAccess = class(TcxCustomGridTableOptionsBehavior);
  TcxCustomGridTableViewAccess = class(TcxCustomGridTableView);

{ TcxExtLookupGrid }

procedure TcxExtLookupGrid.DoCancelMode;
begin
  FRowPressed := False;
  inherited;
end;

procedure TcxExtLookupGrid.DoCloseUp(AAccept: Boolean);
begin
  if AAccept then
    View.DataController.SyncSelected(True);
  if Assigned(FOnCloseUp) then FOnCloseUp(Self, AAccept);
end;

function TcxExtLookupGrid.IsDataRow(AHitTest: TcxCustomGridHitTest): Boolean;
begin
  Result := (AHitTest is TcxGridRecordHitTest) and
    TcxGridRecordHitTest(AHitTest).GridRecord.IsData;
end;

function TcxExtLookupGrid.GetView: TcxCustomGridTableView;
begin
  Result := Levels[0].GridView as TcxCustomGridTableView;
end;

procedure TcxExtLookupGrid.ViewKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Assigned(FPrevOnKeyDown) then
    FPrevOnKeyDown(Self, Key, Shift);
  if (View = nil) then Exit;
  case Key of
    VK_RETURN:
      if not Editable or not View.OptionsData.Editing or (ssCtrl in Shift) then
      begin
        if View.DataController.IsEditing then
          View.DataController.Post;
        DoCloseUp(View.DataController.FocusedRowIndex <> -1);
      end;
    VK_ESCAPE:
      if Editable and not View.DataController.IsEditing then
        DoCloseUp(False);
  end;
end;

procedure TcxExtLookupGrid.ViewMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  AHitTest: TcxCustomGridHitTest;
begin
  if Assigned(FPrevOnMouseDown) then
    FPrevOnMouseDown(Sender, Button, Shift, X, Y);
  AHitTest := View.ViewInfo.GetHitTest(X, Y);
  if (Button = mbLeft) and IsDataRow(AHitTest) then
  begin
    if Editable then
    begin
      if ssDouble in Shift then
        DoCloseUp(True);
    end
    else
      FRowPressed := True;
  end;
end;

procedure TcxExtLookupGrid.ViewMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  AHitTest: TcxCustomGridHitTest;
begin
  if Assigned(FPrevOnMouseMove) then
    FPrevOnMouseMove(Sender, Shift, X, Y);
  if not MouseCapture and PopupMouseMoveLocked then
  begin
    PopupMouseMoveLocked := False;
    Exit;
  end;
  // Hot Track
  if (View = nil) or Editable then Exit;
  AHitTest := View.ViewInfo.GetHitTest(X, Y);
  if IsDataRow(AHitTest) and ((FMousePos.X <> X) or (FMousePos.Y <> Y)) then
  begin
    FMousePos.X := X;
    FMousePos.Y := Y;
    TcxGridRecordHitTest(AHitTest).GridRecord.Focused := True;
  end;
end;

procedure TcxExtLookupGrid.ViewMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  AHitTest: TcxCustomGridHitTest;
begin
  if Assigned(FPrevOnMouseUp) then
    FPrevOnMouseUp(Sender, Button, Shift, X, Y);
  AHitTest := View.ViewInfo.GetHitTest(X, Y);
  if (Button = mbLeft) and FRowPressed then
    DoCloseUp(IsDataRow(AHitTest));
  FRowPressed := False;
end;

{ TcxExtLookupComboBoxProperties }

constructor TcxExtLookupComboBoxProperties.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FAutoSearchOnPopup := True;
end;

destructor TcxExtLookupComboBoxProperties.Destroy;
begin
  FDestroying := True;
  ListFieldItem := nil;
  View := nil;
  FreeAndNil(FGrid);
  inherited Destroy;
end;

procedure TcxExtLookupComboBoxProperties.Assign(Source: TPersistent);
begin
  if Source is TcxExtLookupComboBoxProperties then
  begin
    BeginUpdate;
    try
      AutoSearchOnPopup := TcxExtLookupComboBoxProperties(Source).AutoSearchOnPopup;
      FocusPopup := TcxExtLookupComboBoxProperties(Source).FocusPopup;
      View := nil; //?
      GridMode := TcxExtLookupComboBoxProperties(Source).GridMode;
      View := TcxExtLookupComboBoxProperties(Source).View;
      inherited Assign(Source);
      ListFieldItem := TcxExtLookupComboBoxProperties(Source).ListFieldItem;
    finally
      EndUpdate;
    end
  end
  else
    inherited Assign(Source);
end;

class function TcxExtLookupComboBoxProperties.GetContainerClass: TcxContainerClass;
begin
  Result := TcxExtLookupComboBox;
end;

class function TcxExtLookupComboBoxProperties.IsViewSupported(Value: TcxCustomGridTableView): Boolean;
begin
  Result := Value.CanBeLookupList and
    (TcxCustomGridView(Value).DataController is TcxDBDataController);
end;

procedure TcxExtLookupComboBoxProperties.CheckListFieldItem;
begin
  FInCheckListFieldItem := True;
  try
    if (View <> nil) and (ListFieldItem <> nil) and
      (View.IndexOfItem(ListFieldItem) = -1) then
      ListFieldItem := nil;
  finally
    FInCheckListFieldItem := False;
  end;
end;

procedure TcxExtLookupComboBoxProperties.DeinitializeDataController;
begin
  inherited DeinitializeDataController;
  if DataController <> nil then
    DataController.RemoveDataChangeRefCount;
end;

procedure TcxExtLookupComboBoxProperties.FreeNotification(Sender: TComponent);
begin
  inherited FreeNotification(Sender);
  if Sender = ListFieldItem then
    ListFieldItem := nil;
  if Sender = View then
    View := nil;
end;

function TcxExtLookupComboBoxProperties.GetIncrementalFiltering: Boolean;
begin
  if FocusPopup then
    Result := False
  else
    Result := inherited GetIncrementalFiltering;
end;

function TcxExtLookupComboBoxProperties.GetListIndex: Integer;
begin
  Result := Self.ListFieldIndex;
end;

procedure TcxExtLookupComboBoxProperties.InitializeDataController;
begin
  inherited InitializeDataController;
  if DataController <> nil then
    DataController.AddDataChangeRefCount;
end;

procedure TcxExtLookupComboBoxProperties.LinkView(AView: TcxCustomGridTableView);
begin
  TcxCustomGridTableViewAccess(AView).HighlightIncrementalFilteringText := ifoHighlightSearchText in IncrementalFilteringOptions;
  CheckListFieldItem;
  FreeNotificator.AddSender(AView);
  InitializeDataController;
end;

function TcxExtLookupComboBoxProperties.PopupWindowCapturesFocus: Boolean;
begin
  Result := FocusPopup;
end;

procedure TcxExtLookupComboBoxProperties.UnlinkView(AView: TcxCustomGridTableView);
begin
  TcxCustomGridTableViewAccess(AView).HighlightIncrementalFilteringText := False;
  DeinitializeDataController;
  FreeNotificator.RemoveSender(AView);
end;

// LookupGrid methods

function TcxExtLookupComboBoxProperties.GetLookupGridActiveControl: TWinControl;
begin
  if View <> nil then
    Result := View.Site
  else
    Result := inherited GetLookupGridActiveControl;
end;

function TcxExtLookupComboBoxProperties.GetLookupGridCanResize: Boolean;
begin
  Result := (View <> nil) and View.IsEqualHeightRecords;
end;

function TcxExtLookupComboBoxProperties.GetLookupGridColumnCount: Integer;
begin
  if View <> nil then
    Result := View.ItemCount
  else
    Result := 0;
end;

function TcxExtLookupComboBoxProperties.GetLookupGridControl: TWinControl;
begin
  Result := Grid;
end;

function TcxExtLookupComboBoxProperties.GetLookupGridDataController: TcxCustomDataController;
begin
  if View <> nil then
    Result := View.DataController
  else
    Result := nil;
end;

function TcxExtLookupComboBoxProperties.GetLookupGridVisualAreaPreferredWidth: Integer;
var
  ASavedWidth: Integer;
begin
  Result := 0;
  if View <> nil then
  begin
    ASavedWidth := Grid.Width;
    View.ViewInfo.GetWidth(Point(cxMaxRectSize, cxMaxRectSize), Result);
    Grid.Width := Result;
    View.ViewInfo.GetWidth(Point(cxMaxRectSize, cxMaxRectSize), Result);
    Grid.Width := ASavedWidth;
  end;
end;

function TcxExtLookupComboBoxProperties.GetLookupGridNearestPopupHeight(AHeight: Integer): Integer;
begin
  if View <> nil then
    Result := View.ViewInfo.GetNearestPopupHeight(AHeight, FocusPopup)
  else
    Result := AHeight;
end;

function TcxExtLookupComboBoxProperties.GetLookupGridPopupHeight(ADropDownRowCount: Integer): Integer;
begin
  if View <> nil then
  begin
    if FocusPopup and (ADropDownRowCount < 2) then // TODO: Check New Item Row
      ADropDownRowCount := 2;
    Result := View.ViewInfo.GetPopupHeight(ADropDownRowCount);
  end
  else
    Result := 0;
end;

function TcxExtLookupComboBoxProperties.IsLookupGridMouseOverList(const P: TPoint): Boolean;
var
  AHitTest: TcxCustomGridHitTest;
begin
  Result := False;
  if View <> nil then
  begin
    AHitTest := View.ViewInfo.GetHitTest(P);
    Result := AHitTest is TcxGridRecordHitTest;
  end;
end;

procedure TcxExtLookupComboBoxProperties.LookupGridDeinitialize;
begin
  Grid.Levels[0].GridView := nil;
  // restore options
  if (View <> nil) and not (csDestroying in View.ComponentState) then
  begin
    TcxCustomGridTableOptionsBehaviorAccess(View.OptionsBehavior).PullFocusing := FPrevPullFocusing;
    View.OptionsSelection.MultiSelect := FPrevMultiSelect;
    View.OptionsBehavior.ImmediateEditor := FPrevImmediateEditor;
    if View is TcxGridTableView then
    begin
      TcxGridTableView(View).OptionsCustomize.ColumnFiltering := FPrevColumnFiltering;
      TcxGridTableView(View).OptionsCustomize.ColumnsQuickCustomization := FPrevColumnsQuickCustomization;
    end;
    View.OptionsBehavior.IncSearch := FPrevIncSearch;
    View.OnKeyDown := Grid.FPrevOnKeyDown;
    View.OnMouseDown := Grid.FPrevOnMouseDown;
    View.OnMouseMove := Grid.FPrevOnMouseMove;
    View.OnMouseUp := Grid.FPrevOnMouseUp;
  end;
end;

procedure TcxExtLookupComboBoxProperties.LookupGridDroppedDown(const AFindStr: string);
begin
  // Init Inc Search
  // TODO: !!!
  if FocusPopup and AutoSearchOnPopup and (ListFieldItem <> nil) then
  begin
    ListFieldItem.Focused := True;
    View.DataController.Search.Locate(ListFieldItem.Index, AFindStr);
  end;
end;

procedure TcxExtLookupComboBoxProperties.LookupGridInitEvents(AOnClick, AOnFocusedRowChanged: TNotifyEvent;
  AOnCloseUp: cxLookupEdit.TcxLookupGridCloseUpEvent);
begin
  Grid.OnClick := AOnClick; // not impl
  if View <> nil then
  begin
//    View.OnFocusedRecordChanged := AOnFocusedRowChanged;
    Grid.OnCloseUp := AOnCloseUp;
  end;
end;

procedure TcxExtLookupComboBoxProperties.LookupGridInitialize;
begin
  if View = nil then
    Exit;
  // save options
  FPrevPullFocusing := TcxCustomGridTableOptionsBehaviorAccess(View.OptionsBehavior).PullFocusing;
  TcxCustomGridTableOptionsBehaviorAccess(View.OptionsBehavior).PullFocusing := True;
  FPrevMultiSelect := View.OptionsSelection.MultiSelect;
  View.OptionsSelection.MultiSelect := False;
  FPrevImmediateEditor := View.OptionsBehavior.ImmediateEditor;
  View.OptionsBehavior.ImmediateEditor := False;
  if View is TcxGridTableView then
  begin
    FPrevColumnFiltering := TcxGridTableView(View).OptionsCustomize.ColumnFiltering;
    FPrevColumnsQuickCustomization := TcxGridTableView(View).OptionsCustomize.ColumnsQuickCustomization;
    if not FocusPopup then
    begin
      TcxGridTableView(View).OptionsCustomize.ColumnFiltering := False;
      TcxGridTableView(View).OptionsCustomize.ColumnsQuickCustomization := False;
    end;
  end;

  Grid.FPrevOnKeyDown := View.OnKeyDown;
  View.OnKeyDown := Grid.ViewKeyDown;

  Grid.FPrevOnMouseDown := View.OnMouseDown;
  View.OnMouseDown := Grid.ViewMouseDown;

  Grid.FMousePos := Point(-1, -1);
  Grid.FPrevOnMouseMove := View.OnMouseMove;
  View.OnMouseMove := Grid.ViewMouseMove;

  Grid.FPrevOnMouseUp := View.OnMouseUp;
  View.OnMouseUp := Grid.ViewMouseUp;

  Grid.Editable := FocusPopup;
  Grid.Levels[0].GridView := View;

  FPrevIncSearch := View.OptionsBehavior.IncSearch;
  if FocusPopup and AutoSearchOnPopup then
    View.OptionsBehavior.IncSearch := True;
  View.DataController.Search.Cancel;
end;

procedure TcxExtLookupComboBoxProperties.LookupGridInitLookAndFeel(ALookAndFeel: TcxLookAndFeel;
  AColor: TColor; AFont: TFont);
begin
  Grid.LookAndFeel.MasterLookAndFeel := ALookAndFeel;
  Grid.Color := AColor;
  Grid.Font := AFont;
end;

procedure TcxExtLookupComboBoxProperties.LookupGridLockMouseMove;
begin
  Grid.PopupMouseMoveLocked := True;
end;

procedure TcxExtLookupComboBoxProperties.LookupGridMakeFocusedRowVisible;
begin
  if View <> nil then
    View.Controller.MakeFocusedRecordVisible;
end;

procedure TcxExtLookupComboBoxProperties.LookupGridUnlockMouseMove;
begin
  Grid.MouseCapture := False;
  Grid.PopupMouseMoveLocked := False;
end;

// DBLookupGrid methods

procedure TcxExtLookupComboBoxProperties.DBLookupGridBeginUpdate;
begin
  if View <> nil then View.BeginUpdate;
end;

procedure TcxExtLookupComboBoxProperties.DBLookupGridCheckColumnByFieldName(const AFieldName: string);
begin
  if (View <> nil) and (DataController <> nil) then
  begin
    if (AFieldName <> '') and (DataController.GetItemByFieldName(AFieldName) = nil) then
      with View.CreateItem do
      begin
        Index := 0;
        DataController.ChangeFieldName(Index, AFieldName);
      end;
  end;
end;

procedure TcxExtLookupComboBoxProperties.DBLookupGridCreateColumnsByFieldNames(const AFieldNames: string);
var
  I: Integer;
  AFieldNamesList: TStrings;
begin
  if View <> nil then
  begin
    View.ClearItems;
    AFieldNamesList := TStringList.Create;
    try
      GetFieldNames(AFieldNames, AFieldNamesList);
      View.BeginUpdate;
      try
        for I := 0 to AFieldNamesList.Count - 1 do
          DataController.ChangeFieldName(View.CreateItem.Index, AFieldNamesList[I]);
      finally
        View.EndUpdate;
      end;
    finally
      AFieldNamesList.Free;
    end;
  end;
end;

procedure TcxExtLookupComboBoxProperties.DBLookupGridEndUpdate;
begin
  if View <> nil then View.EndUpdate;
end;

function TcxExtLookupComboBoxProperties.GetDBLookupGridColumnField(AIndex: Integer): TField;
begin
  if DataController <> nil then
    Result := DataController.GetItemField(AIndex)
  else
    Result := nil;
end;

function TcxExtLookupComboBoxProperties.GetDBLookupGridColumnFieldName(AIndex: Integer): string;
begin
  if DataController <> nil then
    Result := DataController.GetItemFieldName(AIndex)
  else
    Result := '';
end;

function TcxExtLookupComboBoxProperties.GetDBLookupGridColumnIndexByFieldName(const AFieldName: string): Integer;
var
  AItem: TcxCustomGridTableItem;
begin
  Result := -1;
  if DataController <> nil then
  begin
    AItem := TcxCustomGridTableItem(DataController.GetItemByFieldName(AFieldName));
    if AItem <> nil then
      Result := AItem.Index;
  end;
end;

function TcxExtLookupComboBoxProperties.GetDBLookupGridDataController: TcxDBDataController;
begin
  Result := TcxDBDataController(GetLookupGridDataController);
end;

function TcxExtLookupComboBoxProperties.GetGrid: TcxExtLookupGrid;

  procedure CreateGrid;
  begin
    FGrid := TcxExtLookupGrid.Create(nil);
    FGrid.IsPopupControl := True;
    FGrid.BorderStyle := cxcbsNone;
    FGrid.Levels.Add;
  end;

begin
  if (FGrid = nil) and not FDestroying then
    CreateGrid;
  Result := FGrid;
end;

function TcxExtLookupComboBoxProperties.GetGridMode: Boolean;
begin
  Result := inherited IsUseLookupList;
end;

function TcxExtLookupComboBoxProperties.GetListFieldIndex: Integer;
var
  AItem: TcxCustomGridTableItem;
begin
  if IsDefinedByLookup then
    Result := GetDisplayColumnIndex
  else
  begin
    AItem := ListFieldItem;
    if AItem <> nil then
      Result := AItem.Index
    else
      Result := -1;
  end;
end;

function TcxExtLookupComboBoxProperties.GetListFieldItem: TcxCustomGridTableItem;
begin
  if IsDefinedByLookup then
    Result := nil
  else
    Result := FListFieldItem;
end;

procedure TcxExtLookupComboBoxProperties.SetGridMode(Value: Boolean);
begin
  inherited IsUseLookupList := Value;
end;

procedure TcxExtLookupComboBoxProperties.SetIncrementalFilteringOptions(Value: TcxTextEditIncrementalFilteringOptions);
begin
  inherited SetIncrementalFilteringOptions(Value);
  if View <> nil then
    TcxCustomGridTableViewAccess(View).HighlightIncrementalFilteringText := ifoHighlightSearchText in Value;
end;

procedure TcxExtLookupComboBoxProperties.SetListFieldItem(Value: TcxCustomGridTableItem);
begin
  if (View <> nil) and (View.IndexOfItem(Value) = -1) then
    Value := nil;
  if FListFieldItem <> Value then
  begin
    if FListFieldItem <> nil then
      FreeNotificator.RemoveSender(FListFieldItem);
    FListFieldItem := Value;
    if FListFieldItem <> nil then
      FreeNotificator.AddSender(FListFieldItem);
    if not FInCheckListFieldItem then
      Changed;
  end;
end;

procedure TcxExtLookupComboBoxProperties.SetView(Value: TcxCustomGridTableView);
begin
  if (Value <> nil) and not IsViewSupported(Value) then Exit;
  if FView <> Value then
  begin
    if FView <> nil then
      UnlinkView(FView);
    FView := Value;
    if FView <> nil then
      LinkView(FView);
    Changed;
  end;
end;

{ TcxCustomExtLookupComboBox }

class function TcxCustomExtLookupComboBox.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxExtLookupComboBoxProperties;
end;

function TcxCustomExtLookupComboBox.CanDropDown: Boolean;
begin
  if ActiveProperties.FocusPopup then
    Result := True
  else
   Result := inherited CanDropDown;
end;

function TcxCustomExtLookupComboBox.GetActiveProperties: TcxExtLookupComboBoxProperties;
begin
  Result := TcxExtLookupComboBoxProperties(InternalGetActiveProperties);
end;

function TcxCustomExtLookupComboBox.GetProperties: TcxExtLookupComboBoxProperties;
begin
  Result := TcxExtLookupComboBoxProperties(inherited Properties);
end;

procedure TcxCustomExtLookupComboBox.SetProperties(Value: TcxExtLookupComboBoxProperties);
begin
  Properties.Assign(Value);
end;

{ TcxDBExtLookupComboBox }

class function TcxDBExtLookupComboBox.GetDataBindingClass: TcxEditDataBindingClass;
begin
  Result := TcxDBLookupEditDataBinding;
end;

function TcxDBExtLookupComboBox.GetDataBinding: TcxDBTextEditDataBinding;
begin
  Result := TcxDBTextEditDataBinding(FDataBinding);
end;

procedure TcxDBExtLookupComboBox.SetDataBinding(Value: TcxDBTextEditDataBinding);
begin
  FDataBinding.Assign(Value);
end;

procedure TcxDBExtLookupComboBox.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := LRESULT(GetcxDBEditDataLink(Self));
end;

{ TcxEditRepositoryExtLookupComboBoxItem }

class function TcxEditRepositoryExtLookupComboBoxItem.GetEditPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxExtLookupComboBoxProperties;
end;

function TcxEditRepositoryExtLookupComboBoxItem.GetProperties: TcxExtLookupComboBoxProperties;
begin
  Result := inherited Properties as TcxExtLookupComboBoxProperties;
end;

procedure TcxEditRepositoryExtLookupComboBoxItem.SetProperties(Value: TcxExtLookupComboBoxProperties);
begin
  inherited Properties := Value;
end;

initialization
  RegisterClasses([TcxEditRepositoryExtLookupComboBoxItem]);
  GetRegisteredEditProperties.Register(TcxExtLookupComboBoxProperties,
    cxSEditRepositoryExtLookupComboBoxItem);

finalization
  GetRegisteredEditProperties.Unregister(TcxExtLookupComboBoxProperties);

end.
