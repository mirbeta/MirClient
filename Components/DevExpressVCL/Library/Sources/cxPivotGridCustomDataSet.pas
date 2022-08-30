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

unit cxPivotGridCustomDataSet;

{$I cxVer.inc}

interface

uses
  Classes, DB, cxClasses, dxmdaset, cxCustomPivotGrid, cxDBPivotGrid, cxDataUtils;

type
  TcxPivotGridCustomDataSet = class;

  TcxPivotGridDataSetFieldEvent = procedure(Sender: TcxPivotGridCustomDataSet;
    APivotGridField: TcxPivotGridField; ADataSetField: TField) of object;

  TcxPivotGridCustomDataSet = class(TdxCustomMemData, IcxPivotGridListener)
  private
    FPivotGrid: TcxCustomPivotGrid;
    FSynchronizeData: Boolean;
    FOnCreateField: TcxPivotGridDataSetFieldEvent;
    FOnDataChanged: TNotifyEvent;

    function GetDataBuilder: TcxPivotGridDataBuilder;
    function GetPivotGrid: TcxCustomPivotGrid;
    procedure SetPivotGrid(Value: TcxCustomPivotGrid);
    procedure SetSynchronizeData(Value: Boolean);
  protected
    // DB
    function IsDBFieldAssigned(APivotGridField: TcxPivotGridField): Boolean;
    function GetDBField(APivotGridField: TcxPivotGridField): TField;

    { IcxPivotGridListener }
    procedure DataChanged(Sender: TcxCustomPivotGrid); virtual;
    procedure LayoutChanged(Sender: TcxCustomPivotGrid); virtual;
    procedure PivotRemoved(Sender: TcxCustomPivotGrid); virtual;
    procedure SelectionChanged(Sender: TcxCustomPivotGrid); virtual;

    function GetDefaultFieldType(APivotGridField: TcxPivotGridField): TFieldType;
    function GetFieldType(APivotGridField: TcxPivotGridField): TFieldType; virtual;

    procedure DoAssignFieldProperties(AField: TField; APivotGridField: TcxPivotGridField); virtual;
    procedure DoCreateData; virtual;
    function DoCreateField(APivotGridField: TcxPivotGridField): TField; virtual;
    procedure DoCreateFields; virtual;
    procedure DoPopulate; virtual;

    procedure ClearAll;

    property DataBuilder: TcxPivotGridDataBuilder read GetDataBuilder;
    property PivotGrid: TcxCustomPivotGrid read GetPivotGrid write SetPivotGrid;
    property SynchronizeData: Boolean read FSynchronizeData write SetSynchronizeData default False;
    property OnCreateField: TcxPivotGridDataSetFieldEvent read FOnCreateField write FOnCreateField;
    property OnDataChanged: TNotifyEvent read FOnDataChanged write FOnDataChanged;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure CreateData;
  end;

implementation

type
  TcxPivotGridFieldAccess = class(TcxPivotGridField);
  TcxCustomPivotGridAccess = class(TcxCustomPivotGrid);
  TcxPivotGridDataBuilderAccess = class(TcxPivotGridDataBuilder);

{ TcxCustomPivotGridDataSet }

constructor TcxPivotGridCustomDataSet.Create(AOwner: TComponent);
begin
  inherited;
  ReadOnly := True;
end;

destructor TcxPivotGridCustomDataSet.Destroy;
begin
  PivotGrid := nil;
  inherited Destroy;
end;

procedure TcxPivotGridCustomDataSet.CreateData;
begin
  if not (csDestroying in ComponentState) then
  begin
    DisableControls;
    try
      ClearAll;

      if (PivotGrid <> nil) and TcxPivotGridDataBuilderAccess(DataBuilder).CanGroup then
      begin
        ReadOnly := False;
        try
          DoCreateData;
        finally
          ReadOnly := True;
        end;
      end;
    finally
      EnableControls;
    end;
    CallNotify(OnDataChanged, Self);
  end;
end;

function TcxPivotGridCustomDataSet.IsDBFieldAssigned(APivotGridField: TcxPivotGridField): Boolean;
begin
  Result := (APivotGridField is TcxDBPivotGridField) and (TcxDBPivotGridField(APivotGridField).DataBinding.DBField <> nil);
end;

function TcxPivotGridCustomDataSet.GetDBField(APivotGridField: TcxPivotGridField): TField;
begin
  if IsDBFieldAssigned(APivotGridField) then
    Result := (APivotGridField as TcxDBPivotGridField).DataBinding.DBField
  else
    Result := nil;
end;

{ IcxPivotGridListener }

procedure TcxPivotGridCustomDataSet.DataChanged(
  Sender: TcxCustomPivotGrid);
begin
  // do nothing
end;

procedure TcxPivotGridCustomDataSet.LayoutChanged(
  Sender: TcxCustomPivotGrid);
begin
  // do nothing
end;

procedure TcxPivotGridCustomDataSet.PivotRemoved(
  Sender: TcxCustomPivotGrid);
begin
  PivotGrid := nil;
end;

procedure TcxPivotGridCustomDataSet.SelectionChanged(
  Sender: TcxCustomPivotGrid);
begin
  // do nothing
end;

procedure TcxPivotGridCustomDataSet.DoCreateFields;
begin

end;

function TcxPivotGridCustomDataSet.GetFieldType(APivotGridField: TcxPivotGridField): TFieldType;
begin
  if ((APivotGridField.Area <> faData) and (APivotGridField.Properties <> nil) and
    (APivotGridField.Properties.GetEditValueSource(False) = evsText)) or APivotGridField.TopValueShowOthers then
    Result := ftString
  else
    case APivotGridField.GroupInterval of
      giDate: Result := ftDate;
      giDateDay..giDayAge, giNumeric: Result := ftInteger;
      giAlphabetical: Result := ftString;
    else
      Result := GetDefaultFieldType(APivotGridField);
    end;
end;

procedure TcxPivotGridCustomDataSet.DoAssignFieldProperties(AField: TField; APivotGridField: TcxPivotGridField);
begin
// do nothing
end;

procedure TcxPivotGridCustomDataSet.DoCreateData;
begin
  DoCreateFields;
  DoPopulate;
end;

function TcxPivotGridCustomDataSet.DoCreateField(APivotGridField: TcxPivotGridField): TField;

  function GetFieldName: string;

    function CheckFieldName(AFieldName: string): Boolean;
    begin
      Result := (AFieldName <> '') and (FindField(AFieldName) = nil);
    end;

  begin
    Result := APivotGridField.Caption;
    if not CheckFieldName(Result) then
      Result := APivotGridField.Name;
    if not CheckFieldName(Result) then
      Result := CreateUniqueName(Owner, Self, Self, '', 'Field');
  end;

var
  AFieldType: TFieldType;
begin
  AFieldType := GetFieldType(APivotGridField);

  if SupportedFieldType(AFieldType) then
    Result := DefaultFieldClasses[AFieldType].Create(Self)
  else
  begin
    Result := TStringField.Create(Self);
    Result.Visible := False;
  end;

  Result.FieldName := GetFieldName;
  DoAssignFieldProperties(Result, APivotGridField);
  Result.DataSet := Self;

  if Assigned(OnCreateField) then
    OnCreateField(Self, APivotGridField, Result);
end;

procedure TcxPivotGridCustomDataSet.DoPopulate;
begin
  Open;
end;

procedure TcxPivotGridCustomDataSet.ClearAll;
var
  I: Integer;
begin
  Close;
  for I := Fields.Count - 1 downto 1 do
    Fields[I].Free;
end;

function TcxPivotGridCustomDataSet.GetDefaultFieldType(APivotGridField: TcxPivotGridField): TFieldType;
begin
  if IsDBFieldAssigned(APivotGridField) then
  begin
    Result := GetDBField(APivotGridField).DataType;
    if Result = ftAutoInc then
      Result := ftInteger;
  end
  else
    Result := VarTypeToDataType(TcxPivotGridFieldAccess(APivotGridField).GetDataType);
  if (Result in [ftDate, ftDateTime]) and (APivotGridField.Area = faData) then
    if APivotGridField.SummaryType = stCount then
      Result := ftInteger
    else
      if not (APivotGridField.SummaryType in [stMin, stMax]) then
        Result := ftFloat;
end;

function TcxPivotGridCustomDataSet.GetDataBuilder: TcxPivotGridDataBuilder;
begin
  Result := TcxCustomPivotGridAccess(PivotGrid).DataBuilder;
end;

function TcxPivotGridCustomDataSet.GetPivotGrid: TcxCustomPivotGrid;
begin
  Result := FPivotGrid;
end;

procedure TcxPivotGridCustomDataSet.SetPivotGrid(
  Value: TcxCustomPivotGrid);
begin
  if PivotGrid <> Value then
  begin
    if (FPivotGrid <> nil) and not FPivotGrid.IsDestroying then
      FPivotGrid.RemoveListener(Self);
    FPivotGrid := Value;
    if (FPivotGrid <> nil) and not FPivotGrid.IsDestroying then
      FPivotGrid.AddListener(Self);
    if SynchronizeData then
      CreateData;
  end;
end;

procedure TcxPivotGridCustomDataSet.SetSynchronizeData(Value: Boolean);
begin
  FSynchronizeData := Value;
  if FSynchronizeData then
    CreateData;
end;

end.

