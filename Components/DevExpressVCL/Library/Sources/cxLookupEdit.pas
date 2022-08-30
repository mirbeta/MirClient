{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressEditors                                           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSEDITORS AND ALL                }
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

unit cxLookupEdit;

{$I cxVer.inc}

interface

uses
  Variants, Windows, Messages, SysUtils, Classes, Controls, Graphics, Forms,
  dxCoreClasses, cxClasses, cxGraphics, cxControls, cxContainer, cxLookAndFeels,
  cxEdit, cxTextEdit, cxDropDownEdit, cxDataUtils, cxCustomData;

type
  TcxCustomLookupEdit = class;
  TcxCustomLookupEditProperties = class;

  { TcxCustomLookupEditLookupData }

  TcxLookupGridCloseUpEvent = procedure (Sender: TObject; AAccept: Boolean) of object;

  TcxCustomLookupEditLookupData = class(TcxInterfacedPersistent,
    IUnknown, IcxTextEditLookupData)
  private
    FOwner: TPersistent;
    FVisible: Boolean;
    FOnCurrentKeyChanged: TNotifyEvent;
    FOnSelectItem: TNotifyEvent;
    function GetDataController: TcxCustomDataController;
    function GetEdit: TcxCustomLookupEdit;
    function GetFocusedRecordIndex: Integer;
    function GetListIndex: Integer;
    function GetProperties: TcxCustomLookupEditProperties;
  protected
    FCurrentKey: Variant;
    function GetOwner: TPersistent; override;
    // IcxTextEditLookupData
    function CanResizeVisualArea(var NewSize: TSize;
      AMaxHeight: Integer = 0; AMaxWidth: Integer = 0): Boolean; virtual;
    procedure CloseUp; virtual;
    procedure Deinitialize; virtual;
    procedure DropDown; virtual;
    procedure DroppedDown(const AFindStr: string); virtual;
    function Find(const AText: string): Boolean; virtual;
    function GetActiveControl: TControl;
    function GetCurrentKey: TcxEditValue; virtual;
    function GetDisplayText(const AKey: TcxEditValue): string; virtual;
    function GetOnCurrentKeyChanged: TNotifyEvent;
    function GetOnSelectItem: TNotifyEvent;
    function GetSelectedItem: Integer; virtual;
    function GetVisualAreaPreferredSize(AMaxHeight: Integer; AWidth: Integer = 0): TSize;
    procedure Go(ADirection: TcxEditLookupDataGoDirection; ACircular: Boolean); virtual;
    procedure Initialize(AVisualControlsParent: TWinControl);
    function IsEmpty: Boolean; virtual;
    function IsMouseOverList(const P: TPoint): Boolean;
    function Locate(var AText, ATail: string; ANext: Boolean): Boolean; virtual;
    procedure PositionVisualArea(const AClientRect: TRect);
    procedure PropertiesChanged; virtual;
    procedure SelectItem; virtual;
    procedure SetCurrentKey(const AKey: TcxEditValue); virtual;
    procedure SetOnCurrentKeyChanged(Value: TNotifyEvent);
    procedure SetOnSelectItem(Value: TNotifyEvent);
    procedure SetSelectedItem(Value: Integer); virtual;
    procedure TextChanged; virtual;

    procedure DoCurrentKeyChanged; virtual;
    procedure DoSelectItem; virtual;
    procedure DoSetCurrentKey(ARecordIndex: Integer); virtual;
    procedure DoSetKeySelection(AKeySelected: Boolean);
    procedure DoSyncGrid; virtual;
    procedure GridClick(Sender: TObject); virtual;
    procedure GridCloseUp(Sender: TObject; AAccept: Boolean); virtual;
    procedure GridFocusedRowChanged(Sender: TObject); virtual;
    function IsIncrementalFiltering: Boolean;
    function IsKeySelected: Boolean; virtual;
    function IsLikeTypeFiltering: Boolean;
    function LocateText(const AText: string): Boolean; virtual;
    procedure ResetIncrementalFilter; virtual;
    procedure SyncGrid; virtual;
    procedure SyncSelected; virtual;
    procedure UpdateDropDownCount; virtual;
    property DataController: TcxCustomDataController read GetDataController;
    property Edit: TcxCustomLookupEdit read GetEdit;
    property FocusedRecordIndex: Integer read GetFocusedRecordIndex;
    property Properties: TcxCustomLookupEditProperties read GetProperties;
  public
    constructor Create(AOwner: TPersistent); override;
    procedure DisableChanging;
    procedure EnableChanging;
  end;

  { TcxCustomLookupEditProperties }

  TcxCustomLookupEditProperties = class(TcxCustomComboBoxProperties)
  private
    FDisplayColumnIndex: Integer;
    FLockCount: Integer;
    FImmediateUpdateText: Boolean;
    FInGridDataChanged: Boolean;
    function GetDataController: TcxCustomDataController;
    function GetDropDownAutoSize: Boolean;
    function GetDropDownHeight: Integer;
    function GetDropDownSizeable: Boolean;
    function GetDropDownWidth: Integer;
    procedure SetDropDownAutoSize(Value: Boolean);
    procedure SetDropDownHeight(Value: Integer);
    procedure SetDropDownSizeable(Value: Boolean);
    procedure SetDropDownWidth(Value: Integer);
  protected
    procedure DoAssign(AProperties: TcxCustomEditProperties); override;
    // LookupGrid methods
    function GetLookupGridActiveControl: TWinControl; virtual;
    function GetLookupGridCanResize: Boolean; virtual;
    function GetLookupGridColumnCount: Integer; virtual;
    function GetLookupGridColumnProperties(AIndex: Integer): TcxCustomEditProperties; virtual;
    function GetLookupGridControl: TWinControl; virtual;
    function GetLookupGridDataController: TcxCustomDataController; virtual;
    function GetLookupGridVisualAreaPreferredWidth: Integer; virtual;
    function GetLookupGridNearestPopupHeight(AHeight: Integer): Integer; virtual;
    function GetLookupGridPopupHeight(ADropDownRowCount: Integer): Integer; virtual;
    function IsLookupGridMouseOverList(const P: TPoint): Boolean; virtual;
    procedure LookupGridDeinitialize; virtual; // IsPopup := False
    procedure LookupGridDroppedDown(const AFindStr: string); virtual;
    procedure LookupGridInitEvents(AOnClick, AOnFocusedRowChanged: TNotifyEvent; AOnCloseUp: TcxLookupGridCloseUpEvent); virtual;
    procedure LookupGridInitialize; virtual; // IsPopup := True
    procedure LookupGridInitLookAndFeel(ALookAndFeel: TcxLookAndFeel; AColor: TColor; AFont: TFont); virtual;
    procedure LookupGridLockMouseMove; virtual;
    procedure LookupGridMakeFocusedRowVisible; virtual;
    procedure LookupGridUnlockMouseMove; virtual;

    procedure CheckDisplayColumnIndex;
    procedure DeinitializeDataController; virtual;
    function FindByText(AItemIndex: Integer; const AText: string; APartialCompare: Boolean): Integer; virtual;
    function FindLookupText(const AText: string): Boolean; override;
    function GetDefaultIncrementalFilteringOptions: TcxTextEditIncrementalFilteringOptions; override;
    function GetDisplayColumnIndex: Integer; virtual;
    function GetDisplayFormatOptions: TcxEditDisplayFormatOptions; override;
    function GetDisplayLookupText(const AKey: TcxEditValue): string; virtual;
    function GetIncrementalFiltering: Boolean; virtual;
    function GetListIndex: Integer; virtual;
    class function GetLookupDataClass: TcxInterfacedPersistentClass; override;
    function GetNullKey: Variant; virtual;
    procedure GridDataChanged(Sender: TObject); virtual;
    procedure InitializeDataController; virtual;
    function IsChangeLocked: Boolean; virtual;
    function IsDataChangedPostponed: Boolean; virtual;
    function IsEditValueConversionDependOnFocused: Boolean; override;
    function IsLookupEdit: Boolean; override;
    function IsPickMode: Boolean; virtual;
    function IsPopupKey(Key: Word; Shift: TShiftState): Boolean; override;
    procedure LockDataChanged; virtual;
    procedure SetDisplayColumnIndex(Value: Integer); virtual;
    procedure UnlockDataChanged; virtual;
    procedure UnlinkLookupGridControlParent; virtual;
    property DataController: TcxCustomDataController read GetDataController;
    property DisplayColumnIndex: Integer read GetDisplayColumnIndex write SetDisplayColumnIndex default 0;
    property ImmediateUpdateText: Boolean read FImmediateUpdateText write FImmediateUpdateText default False;
  public
    constructor Create(AOwner: TPersistent); override;
    class function GetContainerClass: TcxContainerClass; override;
    function GetEditValueSource(AEditFocused: Boolean): TcxDataEditValueSource; override;
    function GetSupportedOperations: TcxEditSupportedOperations; override;
    function IsDefinedByLookup: Boolean; virtual;
    function IsEditValueValid(var EditValue: TcxEditValue; AEditFocused: Boolean): Boolean; override;
    function IsLookupField: Boolean; virtual;
    procedure PrepareDisplayValue(const AEditValue: TcxEditValue; var DisplayValue: TcxEditValue;
      AEditFocused: Boolean); override;
    property DropDownAutoSize: Boolean read GetDropDownAutoSize write SetDropDownAutoSize default False;
    property DropDownHeight: Integer read GetDropDownHeight write SetDropDownHeight default 0;
    property DropDownListStyle default lsEditFixedList;
    property DropDownSizeable: Boolean read GetDropDownSizeable write SetDropDownSizeable default False;
    property DropDownWidth: Integer read GetDropDownWidth write SetDropDownWidth default 0;
    property IncrementalFiltering default True;
  end;

  { TcxCustomLookupEdit }

  TcxCustomLookupEdit = class(TcxCustomComboBox)
  private
    FIsTextChanging: Boolean;
    function GetLookupData: TcxCustomLookupEditLookupData;
    function GetProperties: TcxCustomLookupEditProperties;
    function GetActiveProperties: TcxCustomLookupEditProperties;
    procedure SetLookupData(Value: TcxCustomLookupEditLookupData);
    procedure SetProperties(Value: TcxCustomLookupEditProperties);
  protected
    procedure AfterPosting; override;
    procedure BeforePosting; override;
    procedure ChangeHandler(Sender: TObject); override;
    procedure DoShowEdit; override;
    procedure DropDown; override;
    procedure Initialize; override;
    function InternalGetEditingValue: TcxEditValue; override;
    function NeedResetInvalidTextWhenPropertiesChanged: Boolean; override;
    procedure PrepareEditForInplaceActivation; override;
    procedure RepositoryItemAssigning; override;
    function SupportsSpelling: Boolean; override;
    procedure SynchronizeDisplayValue; override;

    property LookupData: TcxCustomLookupEditLookupData read GetLookupData write SetLookupData;
  public
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    procedure PrepareEditValue(const ADisplayValue: TcxEditValue; out EditValue: TcxEditValue; AEditFocused: Boolean); override;
    property ActiveProperties: TcxCustomLookupEditProperties read GetActiveProperties;
    property Properties: TcxCustomLookupEditProperties read GetProperties
      write SetProperties;
  end;

implementation

uses
  StdCtrls, cxVariants;

// TODO: Field.DisplayText <> Field.AsString <- DataSet.Locate

{ TcxCustomLookupEditLookupData }

constructor TcxCustomLookupEditLookupData.Create(AOwner: TPersistent);
begin
  inherited Create(nil);
  FOwner := AOwner;
  FCurrentKey := Null;
end;

procedure TcxCustomLookupEditLookupData.DisableChanging;
begin
  Inc(Properties.FLockCount);
end;

procedure TcxCustomLookupEditLookupData.EnableChanging;
begin
  Dec(Properties.FLockCount);
end;

function TcxCustomLookupEditLookupData.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

// IcxTextEditLookupData

function TcxCustomLookupEditLookupData.CanResizeVisualArea(var NewSize: TSize;
  AMaxHeight: Integer = 0; AMaxWidth: Integer = 0): Boolean;
begin
  if (AMaxHeight > 0) and (NewSize.cy > AMaxHeight) then
    NewSize.cy := AMaxHeight;
  if Properties.GetLookupGridCanResize then
    NewSize.cy := Properties.GetLookupGridNearestPopupHeight(NewSize.cy);
  Result := True;
end;

procedure TcxCustomLookupEditLookupData.CloseUp;
begin
  DisableChanging;
  try
//    if Properties.GetLookupGridControl <> nil then
//      Properties.GetLookupGridControl.Parent := nil;
    ResetIncrementalFilter;
    FVisible := False;
    Properties.LookupGridInitEvents(nil, nil, nil);
    Properties.LookupGridUnlockMouseMove;
    Properties.LookupGridDeinitialize;
    Properties.UnlockDataChanged;
  finally
    EnableChanging;
  end;
end;

procedure TcxCustomLookupEditLookupData.Deinitialize;
begin
//  if Properties.GetLookupGridControl <> nil then
//    Properties.GetLookupGridControl.Parent := nil;
  Properties.UnlinkLookupGridControlParent
end;

procedure TcxCustomLookupEditLookupData.DropDown;
begin
  DisableChanging;
  try
    Properties.LockDataChanged;
    Properties.LookupGridInitialize;
    Properties.LookupGridInitEvents(GridClick, GridFocusedRowChanged, GridCloseUp);
    Properties.LookupGridInitLookAndFeel(Edit.PopupControlsLookAndFeel,
      Edit.GetBackgroundColor, Edit.VisibleFont);
    Properties.LookupGridLockMouseMove;
    FVisible := True;
    SyncGrid;
    SyncSelected;
  finally
    EnableChanging;
  end;
end;

procedure TcxCustomLookupEditLookupData.DroppedDown(const AFindStr: string);
begin
  Properties.LookupGridDroppedDown(AFindStr);
end;

function TcxCustomLookupEditLookupData.Find(const AText: string): Boolean;
begin
  Result := Properties.FindLookupText(AText);
end;

function TcxCustomLookupEditLookupData.GetActiveControl: TControl;
begin
  Result := Properties.GetLookupGridActiveControl;
end;

function TcxCustomLookupEditLookupData.GetCurrentKey: TcxEditValue;
begin
  Result := FCurrentKey;
end;

function TcxCustomLookupEditLookupData.GetDisplayText(const AKey: TcxEditValue): string;
begin
  Result := Properties.GetDisplayLookupText(AKey);
end;

function TcxCustomLookupEditLookupData.GetOnCurrentKeyChanged: TNotifyEvent;
begin
  Result := FOnCurrentKeyChanged;
end;

function TcxCustomLookupEditLookupData.GetOnSelectItem: TNotifyEvent;
begin
  Result := FOnSelectItem;
end;

function TcxCustomLookupEditLookupData.GetSelectedItem: Integer;
begin
  Result := FocusedRecordIndex;
end;

function TcxCustomLookupEditLookupData.GetVisualAreaPreferredSize(AMaxHeight: Integer;
  AWidth: Integer = 0): TSize;
var
  ARowCount: Integer;
begin
  Result.cx := Properties.GetLookupGridVisualAreaPreferredWidth;
  // auto correct
  ARowCount := Properties.DropDownRows;
  if (DataController <> nil) and (DataController.GetRowCount < ARowCount) then
    ARowCount := DataController.GetRowCount;
  if ARowCount < 1 then
    ARowCount := 1;
  Result.cy := Properties.GetLookupGridPopupHeight(ARowCount);
end;

procedure TcxCustomLookupEditLookupData.Go(ADirection: TcxEditLookupDataGoDirection;
  ACircular: Boolean);
begin
  if (DataController = nil) or (DataController.RecordCount = 0) then Exit;
  DisableChanging;
  try
    ResetIncrementalFilter;
    UpdateDropDownCount;
    Properties.LockDataChanged;
    try
      if ADirection = egdBegin then
        DataController.GotoFirst
      else
        if ADirection = egdEnd then
          DataController.GotoLast
        else
        begin
          SyncGrid;
          if ADirection in [egdNext, egdPageDown] then
          begin
            if ACircular and DataController.IsEOF then
              DataController.GotoFirst
            else
              if ADirection = egdNext then
                DataController.GotoNext
              else
                DataController.MoveBy(Properties.DropDownRows);
          end
          else
          begin
            if ACircular and DataController.IsBOF then
              DataController.GotoLast
            else
              if ADirection = egdPrev then
                DataController.GotoPrev
              else
                DataController.MoveBy(-Properties.DropDownRows);
          end;
        end;
    finally
      Properties.UnlockDataChanged;
    end;
    DoSetCurrentKey(DataController.GetFocusedRecordIndex);
    DoSetKeySelection(DataController.GetFocusedRecordIndex <> -1);
    DoSelectItem;
  finally
    EnableChanging;
  end;
end;

procedure TcxCustomLookupEditLookupData.Initialize(AVisualControlsParent: TWinControl);
begin
  if Properties.GetLookupGridControl <> nil then
    Properties.GetLookupGridControl.Parent := AVisualControlsParent;
end;

function TcxCustomLookupEditLookupData.IsEmpty: Boolean;
begin
  Result := (GetListIndex = -1) or (DataController = nil) or
    (DataController.RecordCount = 0);
end;

function TcxCustomLookupEditLookupData.IsMouseOverList(const P: TPoint): Boolean;
var
  APoint: TPoint;
begin
  if Properties.GetLookupGridControl <> nil then
  begin
    APoint := Properties.GetLookupGridControl.ScreenToClient(P);
    Result := Properties.IsLookupGridMouseOverList(APoint);
  end
  else
    Result := False;
end;

function TcxCustomLookupEditLookupData.Locate(var AText, ATail: string; ANext: Boolean): Boolean;

  function SetGridFilter(AItemIndex: Integer; const AText: string): Integer;
  var
    APrevIncrementalFilterText: string;
  begin
    if AText = '' then
    begin
      ResetIncrementalFilter;
      Result := Properties.FindByText(AItemIndex, AText, True);
    end
    else
    begin
      APrevIncrementalFilterText := DataController.GetIncrementalFilterText;
      Result := DataController.SetIncrementalFilter(AItemIndex, AText, IsLikeTypeFiltering);
      if DataController.FilteredRecordCount = 0 then
      begin
        if Properties.DropDownListStyle <> lsEditList then
          DataController.SetIncrementalFilter(AItemIndex, APrevIncrementalFilterText, IsLikeTypeFiltering);
        Result := -1;
      end;
    end;
    UpdateDropDownCount;
  end;

var
  AItemIndex, ARecordIndex: Integer;
  S: string;
begin
  Result := False;
  DisableChanging;
  try
    AItemIndex := GetListIndex;
    if (AItemIndex <> -1) and (DataController <> nil) then
    begin
      // TODO: Next
      if FVisible and Properties.GetIncrementalFiltering {and (Properties.DropDownListStyle <> lsFixedList)} then
        ARecordIndex := SetGridFilter(AItemIndex, AText)
      else
        ARecordIndex := Properties.FindByText(AItemIndex, AText, True);
      if ARecordIndex <> -1 then
      begin
        DataController.ChangeFocusedRecordIndex(ARecordIndex);
        DoSetCurrentKey(ARecordIndex);
        Result := True;
        S := DataController.DisplayTexts[ARecordIndex, AItemIndex];
        if IsLikeTypeFiltering then
        begin
          if Properties.EditingStyle = esFixedList then
            AText := S;
          ATail := '';
        end
        else
        begin
          AText := Copy(S, 1, Length(AText));
          ATail := Copy(S, Length(AText) + 1, Length(S));
        end;
        DoSetKeySelection(True);
      end
      else
        DoSetKeySelection(False);
    end;
  finally
    EnableChanging;
  end;
end;

procedure TcxCustomLookupEditLookupData.PositionVisualArea(const AClientRect: TRect);
begin
  if Properties.GetLookupGridControl <> nil then
  begin
    with AClientRect do
      Properties.GetLookupGridControl.SetBounds(Left, Top, Right - Left, Bottom - Top);
    Properties.GetLookupGridControl.HandleNeeded;
    Properties.LookupGridMakeFocusedRowVisible;
  end;
end;

procedure TcxCustomLookupEditLookupData.PropertiesChanged;
begin
end;

procedure TcxCustomLookupEditLookupData.SelectItem;
begin
  if IsKeySelected then
  begin
    DoSetCurrentKey(FocusedRecordIndex);
    DoSelectItem;
  end;
end;

procedure TcxCustomLookupEditLookupData.SetCurrentKey(const AKey: TcxEditValue);
begin
  if not VarEquals(FCurrentKey, AKey) then
  begin
    FCurrentKey := AKey;
    SyncGrid;
  end;
end;

procedure TcxCustomLookupEditLookupData.SetOnCurrentKeyChanged(Value: TNotifyEvent);
begin
  FOnCurrentKeyChanged := Value;
end;

procedure TcxCustomLookupEditLookupData.SetOnSelectItem(Value: TNotifyEvent);
begin
  FOnSelectItem := Value;
end;

procedure TcxCustomLookupEditLookupData.SetSelectedItem(Value: Integer);
begin
  if DataController <> nil then
    DataController.FocusedRecordIndex := Value;
end;

procedure TcxCustomLookupEditLookupData.TextChanged;
begin
  if Edit.CanSynchronizeLookupData then
  begin
    DisableChanging;
    try
      SyncSelected;
    finally
      EnableChanging;
    end;
  end;
end;

// end IcxTextEditLookupData

procedure TcxCustomLookupEditLookupData.DoCurrentKeyChanged;
begin
  if Assigned(FOnCurrentKeyChanged) then
    FOnCurrentKeyChanged(Self);
end;

procedure TcxCustomLookupEditLookupData.DoSelectItem;
begin
  if Assigned(FOnSelectItem) then
    FOnSelectItem(Self);
end;

procedure TcxCustomLookupEditLookupData.DoSetCurrentKey(ARecordIndex: Integer);
begin
  FCurrentKey := ARecordIndex;
end;

procedure TcxCustomLookupEditLookupData.DoSetKeySelection(AKeySelected: Boolean);
begin
  if DataController <> nil then
    DataController.SyncSelected(AKeySelected);
end;

procedure TcxCustomLookupEditLookupData.DoSyncGrid;
begin
  if DataController <> nil then
    DataController.ChangeFocusedRecordIndex(Integer(GetCurrentKey));
end;

procedure TcxCustomLookupEditLookupData.GridClick(Sender: TObject);
begin
  SelectItem;
end;

procedure TcxCustomLookupEditLookupData.GridCloseUp(Sender: TObject; AAccept: Boolean);
begin
  if AAccept then
  begin
    Edit.BeginUserAction;
    try
      Edit.CloseUp(crEnter)
    finally
      Edit.EndUserAction;
    end;
  end
  else
    Edit.CloseUp(crCancel)
end;

procedure TcxCustomLookupEditLookupData.GridFocusedRowChanged(Sender: TObject);
begin
  // TODO: ImmediateUpdateText
end;

function TcxCustomLookupEditLookupData.IsKeySelected: Boolean;
begin
  Result := (DataController <> nil) and (DataController.GetSelectedCount = 1) and
    (DataController.GetFocusedRowIndex = DataController.GetSelectedRowIndex(0));
end;

function TcxCustomLookupEditLookupData.LocateText(const AText: string): Boolean;
var
  AItemIndex, ARecordIndex: Integer;
begin
  Result := False;
  AItemIndex := GetListIndex;
  if (AItemIndex <> -1) and (DataController <> nil) then
  begin
//  Result := InternalCompareString(AText, GetDisplayText(FCurrentKey), False);
//  if Result then
//    Exit;
    // TODO: Search
    ARecordIndex := Properties.FindByText(AItemIndex, AText, False);
    if ARecordIndex <> -1 then
    begin
      DataController.ChangeFocusedRecordIndex(ARecordIndex);
      DoSetCurrentKey(ARecordIndex);
      Result := True;
      DoSetKeySelection(True);
    end
    else
      DoSetKeySelection(False);
  end;
end;

procedure TcxCustomLookupEditLookupData.ResetIncrementalFilter;
begin
  if DataController <> nil then
    DataController.ResetIncrementalFilter;
end;

procedure TcxCustomLookupEditLookupData.SyncGrid;
begin
  DoSyncGrid;
  Properties.LookupGridMakeFocusedRowVisible;
end;

procedure TcxCustomLookupEditLookupData.SyncSelected;
var
  ASelected: Boolean;
  AItemIndex: Integer;
  ARecordIndex: Integer;
  S: string;
begin
  if Properties.FInGridDataChanged then Exit;
  ASelected := False;
  AItemIndex := GetListIndex;
  if (DataController <> nil) and (AItemIndex <> -1) then
  begin
    if FVisible and Properties.GetIncrementalFiltering then
      if DataController.GetIncrementalFilterField = nil then
        DataController.SetIncrementalFilter(AItemIndex, '', IsLikeTypeFiltering)
      else
        if Edit.FIsTextChanging and DataController.ChangeIncrementalFilterText(Edit.Text) then
          UpdateDropDownCount;
    ARecordIndex := FocusedRecordIndex;
    if ARecordIndex <> -1 then
    begin
      S := DataController.DisplayTexts[ARecordIndex, AItemIndex];
      ASelected := DataCompareText(S, Edit.Text, False);
    end;
    if not ASelected then // Properties.EditingStyle in [esEditList, esFixedList]
    begin
      ARecordIndex := Properties.FindByText(AItemIndex, Edit.Text, False);
      if ARecordIndex <> -1 then
      begin
        ASelected := True;
        DataController.ChangeFocusedRecordIndex(ARecordIndex);
        DoSetCurrentKey(ARecordIndex);
      end;
    end;
  end;
  DoSetKeySelection(ASelected);
end;

procedure TcxCustomLookupEditLookupData.UpdateDropDownCount;
var
  AGridRowCount, ARowCount, AHeight: Integer;
begin
  if not FVisible then Exit;
  if Edit.PopupWindow.IsVisible and (DataController <> nil) then
  begin
    AGridRowCount := DataController.GetRowCount;
    ARowCount := Properties.DropDownRows;
    if AGridRowCount > ARowCount then
      AGridRowCount := ARowCount;
    if AGridRowCount < 1 then
      AGridRowCount := 1;
    AHeight := Properties.GetLookupGridPopupHeight(AGridRowCount);
    if (Properties.GetLookupGridControl <> nil) and
      (Properties.GetLookupGridControl.ClientHeight <> AHeight) then
    begin
      if not Edit.PopupSizeChanged then
        Edit.ResetPopupHeight;
      Edit.SetupPopupWindow;
    end;
  end;
end;

function TcxCustomLookupEditLookupData.GetDataController: TcxCustomDataController;
begin
  Result := Properties.DataController;
end;

function TcxCustomLookupEditLookupData.GetEdit: TcxCustomLookupEdit;
begin
  Result := TcxCustomLookupEdit(FOwner);
end;

function TcxCustomLookupEditLookupData.GetFocusedRecordIndex: Integer;
begin
  if DataController <> nil then
    Result := DataController.GetFocusedRecordIndex
  else
    Result := -1;
end;

function TcxCustomLookupEditLookupData.GetListIndex: Integer;
begin
  Result := Properties.GetListIndex;
end;

function TcxCustomLookupEditLookupData.GetProperties: TcxCustomLookupEditProperties;
begin
  Result := Edit.ActiveProperties;
end;

function TcxCustomLookupEditLookupData.IsIncrementalFiltering: Boolean;
begin
  Result := Properties.IncrementalFiltering;
end;

function TcxCustomLookupEditLookupData.IsLikeTypeFiltering: Boolean;
begin
  Result := IsIncrementalFiltering and (ifoUseContainsOperator in Properties.IncrementalFilteringOptions);
end;

{ TcxCustomLookupEditProperties }

constructor TcxCustomLookupEditProperties.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  IncrementalFiltering := True;
  DropDownHeight := 0;
  DropDownWidth := 0;
  DropDownListStyle := lsEditFixedList;
  DropDownAutoSize := False;
  DropDownSizeable := False;
end;

class function TcxCustomLookupEditProperties.GetContainerClass: TcxContainerClass;
begin
  Result := TcxCustomLookupEdit;
end;

function TcxCustomLookupEditProperties.GetEditValueSource(AEditFocused: Boolean): TcxDataEditValueSource;
begin
  if IsPickMode then
    Result := evsText
  else
    Result := evsValue;
end;

function TcxCustomLookupEditProperties.GetSupportedOperations: TcxEditSupportedOperations;
begin
  Result := inherited GetSupportedOperations;
  if GetEditValueSource(False) = evsValue then
    Include(Result, esoSortingByDisplayText);
end;

function TcxCustomLookupEditProperties.IsDefinedByLookup: Boolean;
begin
  Result := IsLookupField;
end;

function TcxCustomLookupEditProperties.IsEditValueValid(var EditValue: TcxEditValue;
  AEditFocused: Boolean): Boolean;
begin
  Result := True;
end;

function TcxCustomLookupEditProperties.IsLookupField: Boolean;
begin
  Result := False;
end;

procedure TcxCustomLookupEditProperties.PrepareDisplayValue(const AEditValue: TcxEditValue;
  var DisplayValue: TcxEditValue; AEditFocused: Boolean);
begin
  if IsPickMode or (IsLookupField and not AEditFocused) then
    inherited PrepareDisplayValue(AEditValue, DisplayValue, AEditFocused)
  else
    DisplayValue := GetDisplayLookupText(AEditValue);
end;

procedure TcxCustomLookupEditProperties.DoAssign(AProperties: TcxCustomEditProperties);
begin
  inherited;
  if AProperties is TcxCustomLookupEditProperties then
    with TcxCustomLookupEditProperties(AProperties) do
    begin
      if not Self.IsDefinedByLookup then
        Self.DisplayColumnIndex := DisplayColumnIndex;
      Self.ImmediateUpdateText := ImmediateUpdateText;
    end;
end;

function TcxCustomLookupEditProperties.GetLookupGridActiveControl: TWinControl;
begin
  Result := GetLookupGridControl;
end;

function TcxCustomLookupEditProperties.GetLookupGridCanResize: Boolean;
begin
  Result := True;
end;

function TcxCustomLookupEditProperties.GetLookupGridColumnCount: Integer;
begin
  Result := 0;
end;

function TcxCustomLookupEditProperties.GetLookupGridColumnProperties(AIndex: Integer): TcxCustomEditProperties;
begin
  Result := nil;
end;

function TcxCustomLookupEditProperties.GetLookupGridControl: TWinControl;
begin
  Result := nil;
end;

function TcxCustomLookupEditProperties.GetLookupGridDataController: TcxCustomDataController;
begin
  Result := nil;
end;

function TcxCustomLookupEditProperties.GetLookupGridVisualAreaPreferredWidth: Integer;
begin
  Result := 0;
end;

function TcxCustomLookupEditProperties.GetLookupGridNearestPopupHeight(
  AHeight: Integer): Integer;
begin
  Result := AHeight;
end;

function TcxCustomLookupEditProperties.GetLookupGridPopupHeight(ADropDownRowCount: Integer): Integer;
begin
  Result := 0;
end;

function TcxCustomLookupEditProperties.IsLookupGridMouseOverList(const P: TPoint): Boolean;
begin
  Result := False;
end;

procedure TcxCustomLookupEditProperties.LookupGridDeinitialize;
begin
end;

procedure TcxCustomLookupEditProperties.LookupGridDroppedDown(const AFindStr: string);
begin
end;

procedure TcxCustomLookupEditProperties.LookupGridInitEvents(AOnClick, AOnFocusedRowChanged: TNotifyEvent;
  AOnCloseUp: TcxLookupGridCloseUpEvent);
begin
end;

procedure TcxCustomLookupEditProperties.LookupGridInitialize;
begin
end;

procedure TcxCustomLookupEditProperties.LookupGridInitLookAndFeel(ALookAndFeel: TcxLookAndFeel;
  AColor: TColor; AFont: TFont);
begin
end;

procedure TcxCustomLookupEditProperties.LookupGridLockMouseMove;
begin
end;

procedure TcxCustomLookupEditProperties.LookupGridMakeFocusedRowVisible;
begin
end;

procedure TcxCustomLookupEditProperties.LookupGridUnlockMouseMove;
begin
end;

procedure TcxCustomLookupEditProperties.CheckDisplayColumnIndex;
begin
  if FDisplayColumnIndex >= GetLookupGridColumnCount then
    DisplayColumnIndex := GetLookupGridColumnCount - 1;
end;

procedure TcxCustomLookupEditProperties.DeinitializeDataController;
begin
  if DataController <> nil then
    DataController.RemoveDataChangedListener(Self, GridDataChanged);
end;

function TcxCustomLookupEditProperties.FindByText(AItemIndex: Integer;
  const AText: string; APartialCompare: Boolean): Integer;
begin
  Result := DataController.FindRecordIndexByText(0, AItemIndex, AText, APartialCompare, False, True);
end;

function TcxCustomLookupEditProperties.FindLookupText(const AText: string): Boolean;
var
  AItemIndex: Integer;
begin
  AItemIndex := GetListIndex;
  Result := (AItemIndex <> -1) and (DataController <> nil) and
    (FindByText(AItemIndex, AText, False) <> -1);
end;

function TcxCustomLookupEditProperties.GetDisplayColumnIndex: Integer;
begin
  Result := FDisplayColumnIndex;
end;

function TcxCustomLookupEditProperties.GetDisplayFormatOptions: TcxEditDisplayFormatOptions;
begin
  Result := [];
end;

function TcxCustomLookupEditProperties.GetDisplayLookupText(const AKey: TcxEditValue): string;
begin
  Result := '';
end;

function TcxCustomLookupEditProperties.GetIncrementalFiltering: Boolean;
begin
  Result := IncrementalFiltering;
end;

function TcxCustomLookupEditProperties.GetListIndex: Integer;
begin
  Result := DisplayColumnIndex;
  if Result >= GetLookupGridColumnCount then
    Result := GetLookupGridColumnCount - 1;
end;

class function TcxCustomLookupEditProperties.GetLookupDataClass: TcxInterfacedPersistentClass;
begin
  Result := TcxCustomLookupEditLookupData;
end;

function TcxCustomLookupEditProperties.GetNullKey: Variant;
begin
  Result := Null;
end;

procedure TcxCustomLookupEditProperties.GridDataChanged(Sender: TObject);
begin
  if FInGridDataChanged or IsChangeLocked then Exit;
  if IsDataChangedPostponed then Exit;
  FInGridDataChanged := True;
  try
    Changed;
  finally
    FInGridDataChanged := False;
  end;
end;

procedure TcxCustomLookupEditProperties.InitializeDataController;
begin
  if DataController <> nil then
    DataController.AddDataChangedListener(Self, GridDataChanged);
end;

function TcxCustomLookupEditProperties.IsChangeLocked: Boolean;
begin
  Result := FLockCount <> 0;
end;

function TcxCustomLookupEditProperties.IsDataChangedPostponed: Boolean;
begin
  Result := (DataController <> nil) and DataController.DataChangedNotifyLocked;
end;

function TcxCustomLookupEditProperties.IsEditValueConversionDependOnFocused: Boolean;
begin
  Result := False;
end;

function TcxCustomLookupEditProperties.IsLookupEdit: Boolean;
begin
  Result := True;
end;

function TcxCustomLookupEditProperties.IsPickMode: Boolean;
begin
  Result := False;
end;

function TcxCustomLookupEditProperties.IsPopupKey(Key: Word; Shift: TShiftState): Boolean;
begin
  case Key of
    VK_UP, VK_DOWN, VK_PRIOR, VK_NEXT:
      Result := not (ssAlt in Shift);
    VK_HOME, VK_END:
      Result := ssCtrl in Shift;
  else
    Result := False;
  end;
end;

procedure TcxCustomLookupEditProperties.LockDataChanged;
begin
  if DataController <> nil then
    DataController.LockDataChangedNotify;
end;

procedure TcxCustomLookupEditProperties.SetDisplayColumnIndex(Value: Integer);
begin
  if Value >= GetLookupGridColumnCount then Value := GetLookupGridColumnCount - 1;
  if Value < 0 then Value := 0;
  if FDisplayColumnIndex <> Value then
  begin
    FDisplayColumnIndex := Value;
    Changed;
  end;
end;

procedure TcxCustomLookupEditProperties.UnlockDataChanged;
begin
  if DataController <> nil then
    DataController.UnlockDataChangedNotify;
end;

procedure TcxCustomLookupEditProperties.UnlinkLookupGridControlParent;
begin
  if GetLookupGridControl <> nil then
    GetLookupGridControl.Parent := nil;
end;

function TcxCustomLookupEditProperties.GetDataController: TcxCustomDataController;
begin
  Result := GetLookupGridDataController;
end;

function TcxCustomLookupEditProperties.GetDefaultIncrementalFilteringOptions: TcxTextEditIncrementalFilteringOptions;
begin
  Result := [];
end;

function TcxCustomLookupEditProperties.GetDropDownAutoSize: Boolean;
begin
  Result := inherited PopupAutoSize;
end;

function TcxCustomLookupEditProperties.GetDropDownHeight: Integer;
begin
  Result := inherited PopupHeight;
end;

function TcxCustomLookupEditProperties.GetDropDownSizeable: Boolean;
begin
  Result := inherited PopupSizeable;
end;

function TcxCustomLookupEditProperties.GetDropDownWidth: Integer;
begin
  Result := inherited PopupWidth;
end;

procedure TcxCustomLookupEditProperties.SetDropDownAutoSize(Value: Boolean);
begin
  inherited PopupAutoSize := Value;
end;

procedure TcxCustomLookupEditProperties.SetDropDownHeight(Value: Integer);
begin
  inherited PopupHeight := Value;
end;

procedure TcxCustomLookupEditProperties.SetDropDownSizeable(Value: Boolean);
begin
  inherited PopupSizeable := Value;
end;

procedure TcxCustomLookupEditProperties.SetDropDownWidth(Value: Integer);
begin
  inherited PopupWidth := Value;
end;

{ TcxCustomLookupEdit }

class function TcxCustomLookupEdit.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxCustomLookupEditProperties;
end;

procedure TcxCustomLookupEdit.PrepareEditValue(const ADisplayValue: TcxEditValue;
  out EditValue: TcxEditValue; AEditFocused: Boolean);
begin
  if ActiveProperties.IsPickMode then
    EditValue := ADisplayValue
  else
    if InternalCompareString(ILookupData.GetDisplayText(ILookupData.CurrentKey),
        ADisplayValue, ActiveProperties.CharCase = ecNormal) then
      EditValue := ILookupData.CurrentKey
    else
      if LookupData.LocateText(ADisplayValue) then
        EditValue := ILookupData.CurrentKey
      else
        EditValue := ActiveProperties.GetNullKey;
end;

procedure TcxCustomLookupEdit.AfterPosting;
begin
  inherited AfterPosting;
  LookupData.EnableChanging;
end;

procedure TcxCustomLookupEdit.BeforePosting;
begin
  LookupData.DisableChanging;
  inherited BeforePosting;
end;

procedure TcxCustomLookupEdit.ChangeHandler(Sender: TObject);
begin
  FIsTextChanging := True;
  inherited ChangeHandler(Sender);
  FIsTextChanging := False;
end;

procedure TcxCustomLookupEdit.DoShowEdit;
begin
  inherited DoShowEdit;
  ILookupData.TextChanged;
end;

procedure TcxCustomLookupEdit.DropDown;
begin
  inherited DropDown;
  ILookupData.CurrentKey := EditValue;
end;

procedure TcxCustomLookupEdit.Initialize;
begin
  inherited Initialize;
  Width := 145;
end;

function TcxCustomLookupEdit.InternalGetEditingValue: TcxEditValue;
begin
  PrepareEditValue(Text, Result, InternalFocused);
end;

function TcxCustomLookupEdit.NeedResetInvalidTextWhenPropertiesChanged: Boolean;
begin
  Result := False;
end;

procedure TcxCustomLookupEdit.PrepareEditForInplaceActivation;
begin
  ILookupData.CurrentKey := EditValue;
end;

procedure TcxCustomLookupEdit.RepositoryItemAssigning;
begin
  TcxCustomLookupEditProperties(ActiveProperties).UnlinkLookupGridControlParent;
end;

procedure TcxCustomLookupEdit.SynchronizeDisplayValue;
begin
  inherited SynchronizeDisplayValue;
  if not EditModeSetting then
    ILookupData.CurrentKey := EditValue;
end;

function TcxCustomLookupEdit.GetLookupData: TcxCustomLookupEditLookupData;
begin
  Result := TcxCustomLookupEditLookupData(FLookupData);
end;

function TcxCustomLookupEdit.GetProperties: TcxCustomLookupEditProperties;
begin
  Result := TcxCustomLookupEditProperties(inherited Properties);
end;

function TcxCustomLookupEdit.GetActiveProperties: TcxCustomLookupEditProperties;
begin
  Result := TcxCustomLookupEditProperties(InternalGetActiveProperties);
end;

procedure TcxCustomLookupEdit.SetLookupData(Value: TcxCustomLookupEditLookupData);
begin
  FLookupData.Assign(Value);
end;

procedure TcxCustomLookupEdit.SetProperties(Value: TcxCustomLookupEditProperties);
begin
  Properties.Assign(Value);
end;

function TcxCustomLookupEdit.SupportsSpelling: Boolean;
begin
  Result := IsTextInputMode;
end;

end.
