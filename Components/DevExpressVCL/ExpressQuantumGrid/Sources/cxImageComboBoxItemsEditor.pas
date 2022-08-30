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

unit cxImageComboBoxItemsEditor;

{$I cxVer.inc}

interface

uses
  Variants, Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, cxStyles, cxCustomData, cxFilter,
  cxGraphics, cxData, cxDataStorage, cxEdit, cxImageComboBox, cxLookAndFeels, cxGridCustomTableView, cxGridTableView,
  cxControls, cxGridCustomView, cxClasses, cxGridLevel, cxGrid, Menus, StdCtrls, ExtCtrls, cxTextEdit, cxMaskEdit,
  cxDropDownEdit, cxContainer, cxGroupBox, cxLookAndFeelPainters, cxButtons, cxCalendar, ImgList, cxNavigator,
  dxLayoutContainer, dxLayoutControl, dxLayoutControlAdapters, dxLayoutLookAndFeels, dxForms;

type
  TfmImageComboBoxItemsEditor = class(TdxForm)
    btnAdd: TcxButton;
    btnCancel: TcxButton;
    btnDelete: TcxButton;
    btnInsert: TcxButton;
    btnOk: TcxButton;
    btnSelectAll: TcxButton;
    btnValueType: TcxButton;
    clnDescription: TcxGridColumn;
    clnImage: TcxGridColumn;
    clnTag: TcxGridColumn;
    clnValue: TcxGridColumn;
    clnValueType: TcxGridColumn;
    cxgImageComboBoxItems: TcxGrid;
    dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel;
    dxLayoutGroup1: TdxLayoutGroup;
    dxLayoutItem1: TdxLayoutItem;
    dxLayoutItem2: TdxLayoutItem;
    dxLayoutItem3: TdxLayoutItem;
    dxLayoutItem4: TdxLayoutItem;
    dxLayoutItem5: TdxLayoutItem;
    dxLayoutItem6: TdxLayoutItem;
    dxLayoutItem7: TdxLayoutItem;
    dxLayoutItem8: TdxLayoutItem;
    dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList;
    lcMain: TdxLayoutControl;
    lcMainGroup_Root: TdxLayoutGroup;
    lvImageComboBoxItems: TcxGridLevel;
    miAdd: TMenuItem;
    miDelete: TMenuItem;
    miHelp: TMenuItem;
    miInsert: TMenuItem;
    mnuValueTypes: TPopupMenu;
    N1: TMenuItem;
    tvImageComboBoxItems: TcxGridTableView;

    procedure FormCreate(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnInsertClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure btnSelectAllClick(Sender: TObject);
    procedure clnValueTypePropertiesEditValueChanged(Sender: TObject);
    procedure miValueTypeClick(Sender: TObject);
    procedure tvImageComboBoxItemsKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure tvImageComboBoxItemsDataControllerRecordChanged(
      ADataController: TcxCustomDataController; ARecordIndex, AItemIndex: Integer);
    procedure tvImageComboBoxItemsEditKeyDown(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem; AEdit: TcxCustomEdit; var Key: Word; Shift: TShiftState);
  private
    FImageComboBoxItems: TcxImageComboBoxItems;
    FImages, FLargeImages: TCustomImageList;

    procedure FillGridViewRecords(AImageComboBoxItems: TcxImageComboBoxItems);
    procedure FocusAndSelectRecord(ARecordIndex: Integer);
    function GetVarTypeName(AVarType: Word): string;
    function GetVarTypeByName(AVarTypeName: string): Word;
    procedure InitNewRecord(ARecordIndex: Integer);
    procedure PopulateImages(AImages, ALargeImages: TCustomImageList);
    procedure PopulateValueTypeNames;
    procedure SetItemValueType(ARecordIndex: Integer; AValueType: Word);
    procedure SetSelectionValueType(AValueType: Word);
    function ValueToValueType(var AValue: Variant; AValueType: Word): Boolean;
  public
    constructor Create(AItems: TcxImageComboBoxItems; AImages, ALargeImages: TCustomImageList); reintroduce; overload;
  end;

implementation

uses
  cxVariants, TypInfo;

{$R *.dfm}

const
  cxImageComboErrorVarTypeName = 'ERROR';
  cxImageComboNullValue = '(Null)';
  cxImageComboOleStrVarTypeName = 'OleStr';
  cxImageComboStringVarTypeName = 'String';
  cxImageComboUnknownVarTypeName = '(Unknown)';

type
  TcxAccessImageComboBoxProperties = class(TcxImageComboBoxProperties);

var
  cxImageComboVarTypeNames: array[varEmpty..varInt64]
    of string = (
      'Unassigned', // varEmpty
      'Null',       // varNull
      'Smallint',   // varSmallint
      'Integer',    // varInteger
      'Single',     // varSingle
      'Double',     // varDouble
      'Currency',   // varCurrency
      'Date',       // varDate
      'OleStr',     // varOleStr
      '',           // varDispatch
      '',           // varError
      'Boolean',    // varBoolean
      '',           // varVariant
      '',           // varUnknown
      '',           // [varDecimal]
      '',           // [undefined]
      'Shortint',   // varShortInt
      'Byte',       // varByte
      'Word',       // varWord
      'LongWord',   // varLongWord
      'Int64'       // varInt64
    );

constructor TfmImageComboBoxItemsEditor.Create(AItems: TcxImageComboBoxItems;
  AImages, ALargeImages: TCustomImageList);
begin
  FImageComboBoxItems := AItems;
  FImages := AImages;
  FLargeImages :=  ALargeImages;
  inherited Create(Application);
end;

procedure TfmImageComboBoxItemsEditor.FormCreate(Sender: TObject);
begin
  TcxAccessImageComboBoxProperties(clnImage.Properties).ShowImageIndexInsteadDescription := True;
  PopulateValueTypeNames;
  PopulateImages(FImages, FLargeImages);
  FillGridViewRecords(FImageComboBoxItems);
  tvImageComboBoxItems.Controller.GoToLast(False);
end;

procedure TfmImageComboBoxItemsEditor.btnAddClick(Sender: TObject);
begin
  tvImageComboBoxItems.DataController.Post;
  tvImageComboBoxItems.BeginUpdate;
  try
    tvImageComboBoxItems.DataController.AppendRecord;
    InitNewRecord(tvImageComboBoxItems.DataController.RecordCount - 1);
  finally
    tvImageComboBoxItems.EndUpdate;
  end;
  FocusAndSelectRecord(tvImageComboBoxItems.DataController.RecordCount - 1);
end;

procedure TfmImageComboBoxItemsEditor.btnDeleteClick(Sender: TObject);
var
  AIndex: Integer;
begin
  if tvImageComboBoxItems.DataController.RecordCount = 0 then
    Exit;
  AIndex := tvImageComboBoxItems.Controller.FocusedRecordIndex;
  tvImageComboBoxItems.DataController.DeleteSelection;
  if tvImageComboBoxItems.DataController.RecordCount > AIndex then
  begin
    tvImageComboBoxItems.Controller.FocusedRecordIndex := AIndex;
    tvImageComboBoxItems.Controller.FocusedRecord.Selected := True;
  end
  else
    tvImageComboBoxItems.Controller.GoToLast(False);
end;

procedure TfmImageComboBoxItemsEditor.btnOkClick(Sender: TObject);

  procedure InitImageComboBoxItem(AItem: TcxImageComboBoxItem;
    ARecord: TcxCustomGridRecord);

      function VarToInteger(AValue: Variant): Integer;
      begin
        if VarIsNull(AValue) then
          Result := 0
        else
          Result := AValue;
      end;

  var
    AValue: Variant;
    AVarType: Word;
  begin
    AItem.Description := VarToStr(ARecord.Values[clnDescription.Index]);
    AItem.Tag := VarToInteger(ARecord.Values[clnTag.Index]);
    AItem.ImageIndex := VarToInteger(ARecord.Values[clnImage.Index]);
    AValue := ARecord.Values[clnValue.Index];
    AVarType := GetVarTypeByName(ARecord.Values[clnValueType.Index]);
    case AVarType of
      varNull:
        AValue := Null;
      varEmpty:
        VarClear(AValue);
      varError:
      begin
        AValue := Null;
        raise EVariantError.Create('Invalid value type');
      end;
      else
        VarCast(AValue, AValue, AVarType);
    end;
    AItem.Value := AValue;
  end;

var
  I: Integer;
begin
  FImageComboBoxItems.Clear;
  for I := 0 to tvImageComboBoxItems.ViewData.RecordCount - 1 do
    InitImageComboBoxItem(
      FImageComboBoxItems.Add as TcxImageComboBoxItem,
      tvImageComboBoxItems.ViewData.Records[I]);
end;

procedure TfmImageComboBoxItemsEditor.btnInsertClick(Sender: TObject);

  function InsertRecordBeforeFocused: Integer;
  begin
    Result := tvImageComboBoxItems.DataController.InsertRecord(
      tvImageComboBoxItems.Controller.FocusedRecord.Index);
  end;

var
  ARecordIndex: Integer;
begin
  tvImageComboBoxItems.DataController.Post;
  if tvImageComboBoxItems.Controller.FocusedRecord = nil then
    btnAddClick(nil)
  else
  begin
    tvImageComboBoxItems.BeginUpdate;
    try
      ARecordIndex := InsertRecordBeforeFocused;
      InitNewRecord(ARecordIndex);
    finally
      tvImageComboBoxItems.EndUpdate;
    end;
    FocusAndSelectRecord(ARecordIndex);
  end;
end;

procedure TfmImageComboBoxItemsEditor.btnSelectAllClick(Sender: TObject);
begin
  tvImageComboBoxItems.DataController.SelectAll;
end;

procedure TfmImageComboBoxItemsEditor.clnValueTypePropertiesEditValueChanged(
  Sender: TObject);
begin
  SetItemValueType(tvImageComboBoxItems.DataController.FocusedRecordIndex,
    GetVarTypeByName(TcxComboBox(Sender).Text));
end;

procedure TfmImageComboBoxItemsEditor.miValueTypeClick(Sender: TObject);
begin
  SetSelectionValueType(TMenuItem(Sender).Tag);
end;

procedure TfmImageComboBoxItemsEditor.FillGridViewRecords(
  AImageComboBoxItems: TcxImageComboBoxItems);

  function ValueToStr(AValue: Variant): string;
  begin
    if VarIsNull(AValue) then
      Result := cxImageComboNullValue
    else
      case VarType(AValue) of
        varBoolean:
          Result := BooleanIdents[AValue = True];
        varCurrency:
          Result := CurrToStr(AValue);
        varDispatch:
          Result := cxImageComboErrorVarTypeName;
      else
        Result := VarToStr(AValue);
      end;
  end;

  procedure InitRecord(ARecordIndex: Integer;
    AImageComboBoxItem: TcxImageComboBoxItem);
  var
    ARec: TcxCustomGridRecord;
  begin
    ARec := tvImageComboBoxItems.ViewData.Records[ARecordIndex];
    tvImageComboBoxItems.BeginUpdate;
    try
      ARec.Values[clnImage.Index] := AImageComboBoxItem.ImageIndex;
      ARec.Values[clnValue.Index] := ValueToStr(AImageComboBoxItem.Value);
      ARec.Values[clnValueType.Index] :=
        GetVarTypeName(VarType(AImageComboBoxItem.Value));
      ARec.Values[clnDescription.Index] := AImageComboBoxItem.Description;
      ARec.Values[clnTag.Index] := AImageComboBoxItem.Tag;
    finally
      tvImageComboBoxItems.EndUpdate;
    end;
  end;

var
  I: Integer;
begin
  tvImageComboBoxItems.DataController.RecordCount :=
    AImageComboBoxItems.Count;
  if tvImageComboBoxItems.DataController.RecordCount > 0 then
  begin
    tvImageComboBoxItems.BeginUpdate;
    try
      for I := 0 to tvImageComboBoxItems.DataController.RecordCount - 1 do
        InitRecord(I, AImageComboBoxItems[I])
    finally
      tvImageComboBoxItems.EndUpdate;
    end;
  end
  else
  begin
    tvImageComboBoxItems.DataController.RecordCount := 1;
    InitNewRecord(0);
  end;
end;

procedure TfmImageComboBoxItemsEditor.FocusAndSelectRecord(ARecordIndex: Integer);
begin
  tvImageComboBoxItems.DataController.ClearSelection;
  tvImageComboBoxItems.DataController.FocusedRecordIndex :=
    ARecordIndex;
  tvImageComboBoxItems.Controller.FocusedRecord.Selected := True;
end;

function TfmImageComboBoxItemsEditor.GetVarTypeName(AVarType: Word): string;
begin
  AVarType := AVarType and varTypeMask;
  case AVarType of
    Low(cxImageComboVarTypeNames)..High(cxImageComboVarTypeNames):
      Result := cxImageComboVarTypeNames[AVarType];
    varString:
      Result := cxImageComboStringVarTypeName;
  else
    Result := cxImageComboUnknownVarTypeName;
  end;
end;

function TfmImageComboBoxItemsEditor.GetVarTypeByName(AVarTypeName: string): Word;
var
  I: Integer;
begin
  Result := varError;
  if AVarTypeName = cxImageComboStringVarTypeName then
    Result := varString
  else
    for I := Low(cxImageComboVarTypeNames) to High(cxImageComboVarTypeNames) do
      if cxImageComboVarTypeNames[I] = AVarTypeName then
      begin
        Result := I;
        Break;
      end;
end;

procedure TfmImageComboBoxItemsEditor.InitNewRecord(ARecordIndex: Integer);
begin
  tvImageComboBoxItems.BeginUpdate;
  try
    SetItemValueType(ARecordIndex, varNull);
    tvImageComboBoxItems.DataController.Values[ARecordIndex, clnTag.Index] := 0;
    if ARecordIndex <
      TcxImageComboBoxProperties(clnImage.Properties).Items.Count then
        tvImageComboBoxItems.DataController.Values[ARecordIndex, clnImage.Index] := ARecordIndex
      else
        tvImageComboBoxItems.DataController.Values[ARecordIndex, clnImage.Index] := -1;
  finally
    tvImageComboBoxItems.EndUpdate;
  end;
end;

procedure TfmImageComboBoxItemsEditor.PopulateImages(AImages, ALargeImages: TCustomImageList);
var
  I: Integer;
  ACurrentImages: TCustomImageList;
  AImageComboBoxProperties: TcxImageComboBoxProperties;
begin
  if (AImages = nil) and (ALargeImages = nil) then
    Exit;
  AImageComboBoxProperties := TcxImageComboBoxProperties(clnImage.Properties);
  AImageComboBoxProperties.Images := AImages;
  AImageComboBoxProperties.LargeImages := ALargeImages;
  if ALargeImages <> nil then
    ACurrentImages := ALargeImages
  else
    ACurrentImages := AImages;
  AImageComboBoxProperties.Items.Clear;
  with TcxImageComboBoxItem(AImageComboBoxProperties.Items.Add) do
  begin
    ImageIndex := -1;
    Value := -1;
  end;
  for I := 0 to ACurrentImages.Count - 1 do
    with TcxImageComboBoxItem(AImageComboBoxProperties.Items.Add) do
    begin
      ImageIndex := I;
      Value := I;
    end;
end;

procedure TfmImageComboBoxItemsEditor.PopulateValueTypeNames;

  procedure AddValueTypeToDropDownMenu(const AValueTypeName: string; AValue: Integer);
  var
    AMenuItem: TMenuItem;
  begin
    AMenuItem := TMenuItem.Create(mnuValueTypes);
    AMenuItem.Caption := AValueTypeName;
    AMenuItem.OnClick := miValueTypeClick;
    AMenuItem.Tag := AValue;
    mnuValueTypes.Items.Add(AMenuItem);
  end;

  procedure AddValueTypeToValueTypeColumn(const AValueTypeName: string; AValue: Integer);
  var
    AComboBoxProperties: TcxComboBoxProperties;
  begin
    AComboBoxProperties :=
      TcxComboBoxProperties(clnValueType.Properties);
    AComboBoxProperties.Items.AddObject(AValueTypeName, TObject(AValue));
  end;

var
  I: Integer;
begin
  TcxComboBoxProperties(clnValueType.Properties).Items.Clear;
  mnuValueTypes.Items.Clear;
  for I := Low(cxImageComboVarTypeNames) to High(cxImageComboVarTypeNames) do
    if cxImageComboVarTypeNames[I] <> '' then
    begin
      AddValueTypeToDropDownMenu(cxImageComboVarTypeNames[I], I);
      AddValueTypeToValueTypeColumn(cxImageComboVarTypeNames[I], I);
    end;
  AddValueTypeToDropDownMenu(cxImageComboStringVarTypeName, varString);
  AddValueTypeToValueTypeColumn(cxImageComboStringVarTypeName, varString);
end;

procedure TfmImageComboBoxItemsEditor.SetItemValueType(ARecordIndex: Integer; AValueType: Word);
var
  AValue: Variant;
begin
  tvImageComboBoxItems.BeginUpdate;
  try
    tvImageComboBoxItems.DataController.Values[ARecordIndex, clnValueType.Index] :=
      GetVarTypeName(AValueType);
    AValue := tvImageComboBoxItems.DataController.Values[ARecordIndex, clnValue.Index];
    ValueToValueType(AValue, AValueType);
    tvImageComboBoxItems.DataController.Values[ARecordIndex, clnValue.Index] := AValue;
  finally
    tvImageComboBoxItems.EndUpdate;
  end;
end;

procedure TfmImageComboBoxItemsEditor.SetSelectionValueType(AValueType: Word);
var
  I: Integer;
begin
  tvImageComboBoxItems.BeginUpdate;
  try
    for I := 0 to tvImageComboBoxItems.Controller.SelectedRecordCount -1 do
      SetItemValueType(
        tvImageComboBoxItems.Controller.SelectedRecords[I].RecordIndex, AValueType);
  finally
    tvImageComboBoxItems.EndUpdate;
  end;
end;

function TfmImageComboBoxItemsEditor.ValueToValueType(var AValue: Variant; AValueType: Word): Boolean;
var
  AStringValue: string;
begin
  Result := True;
  case AValueType of
    varEmpty:
      if not VarIsSoftNull(AValue) then
      begin
        Result := False;
        VarClear(AValue);
      end;
    varNull:
      if not VarEquals(AValue, cxImageComboNullValue) then
      begin
        AValue := cxImageComboNullValue;
        Result := False;
      end;
    varString, varOleStr:
      if VarEquals(AValue, cxImageComboNullValue) or VarIsNull(AValue) then
         AValue := '';
  else
    if Result then
      try
        AStringValue := VarToStr(AValue);
        VarCast(AValue, AValue, AValueType);
        Result := (AValueType in [varDate, varCurrency, varDouble, varBoolean]) or
          (VarToStr(AValue) = AStringValue);
      except
        Result := False;
        VarClear(AValue);
        VarCast(AValue, AValue, AValueType);
      end;
  end;
end;

procedure TfmImageComboBoxItemsEditor.tvImageComboBoxItemsKeyDown(
  Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_INSERT) and (Shift = []) then
  begin
    btnInsertClick(Self);
    Key := 0;
  end;
end;

procedure TfmImageComboBoxItemsEditor.tvImageComboBoxItemsDataControllerRecordChanged(
  ADataController: TcxCustomDataController; ARecordIndex,
  AItemIndex: Integer);

  function GetFocusedItemValueType: Word;
  begin
    Result := GetVarTypeByName(ADataController.GetValue(ARecordIndex, clnValueType.Index));
  end;

var
  AValue: Variant;
begin
  if AItemIndex = clnValue.Index then
  begin
    ADataController.OnRecordChanged := nil;
    try
      AValue := clnValue.EditValue;
      if VarIsSoftNull(AValue) then
        ADataController.SetValue(ARecordIndex, clnValueType.Index, GetVarTypeName(varEmpty))
      else
        if VarEquals(AValue, cxImageComboNullValue) then
          ADataController.SetValue(ARecordIndex, clnValueType.Index, GetVarTypeName(varNull))
        else
          if not ValueToValueType(AValue, GetFocusedItemValueType) then
            ADataController.SetValue(ARecordIndex, clnValueType.Index, cxImageComboStringVarTypeName)
          else
            ADataController.SetValue(ARecordIndex, clnValue.Index, VarToStr(AValue));
    finally
      ADataController.OnRecordChanged := tvImageComboBoxItemsDataControllerRecordChanged;
    end;
  end;
end;

procedure TfmImageComboBoxItemsEditor.tvImageComboBoxItemsEditKeyDown(
  Sender: TcxCustomGridTableView; AItem: TcxCustomGridTableItem;
  AEdit: TcxCustomEdit; var Key: Word; Shift: TShiftState);
begin
  tvImageComboBoxItemsKeyDown(Sender, Key, Shift);
end;

end.
