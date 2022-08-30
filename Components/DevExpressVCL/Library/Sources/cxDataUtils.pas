{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressDataController                                    }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSDATACONTROLLER AND ALL         }
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

unit cxDataUtils;

{$I cxVer.inc}

interface

uses
  Windows, Variants, Classes, SysUtils;

type
  TcxDataEditValueSource = (evsValue, evsText, evsKey);

  { TcxCustomDataBinding }

  TcxDataBindingNotifyEvent = procedure of object;

  TcxCustomDataBinding = class(TPersistent)
  private
    FDataComponent: TComponent;
    FOwner: TComponent;
    FReadOnly: Boolean;
    FVisualControl: TComponent;
    FOnDataChange: TcxDataBindingNotifyEvent;
    FOnDataSetChange: TcxDataBindingNotifyEvent;
    FOnUpdateData: TcxDataBindingNotifyEvent;
    procedure SetVisualControl(Value: TComponent);
  protected
    function GetOwner: TPersistent; override;
    procedure DataChange; virtual;
    procedure DataSetChange; virtual;
    function GetModified: Boolean; virtual;
    function GetReadOnly: Boolean; virtual;
    procedure SetReadOnly(Value: Boolean); virtual;
    procedure UpdateData; virtual;
    procedure VisualControlChanged; virtual;
    property DataComponent: TComponent read FDataComponent;
  public
    constructor Create(AOwner, ADataComponent: TComponent); virtual;
    function CanModify: Boolean; virtual;
    function ExecuteAction(Action: TBasicAction): Boolean; virtual;
    function GetStoredValue(AValueSource: TcxDataEditValueSource; AFocused: Boolean): Variant; virtual;
    function IsControlReadOnly: Boolean; virtual;
    function IsDataSourceLive: Boolean; virtual;
    function IsDataStorage: Boolean; virtual;
    procedure Reset; virtual;
    function SetEditMode: Boolean; virtual;
    procedure SetStoredValue(AValueSource: TcxDataEditValueSource; const Value: Variant); virtual;
    function UpdateAction(Action: TBasicAction): Boolean; virtual;
    procedure UpdateDataSource; virtual;
    property Modified: Boolean read GetModified;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property VisualControl: TComponent read FVisualControl write SetVisualControl;
    property OnDataChange: TcxDataBindingNotifyEvent read FOnDataChange write FOnDataChange;
    property OnDataSetChange: TcxDataBindingNotifyEvent read FOnDataSetChange write FOnDataSetChange;
    property OnUpdateData: TcxDataBindingNotifyEvent read FOnUpdateData write FOnUpdateData;
  end;

  TcxCustomDataBindingClass = class of TcxCustomDataBinding;

function DataCompareText(const S1, S2: string; APartialCompare: Boolean; AIsAnywhere: Boolean = False): Boolean;

implementation

function DataCompareText(const S1, S2: string; APartialCompare: Boolean; AIsAnywhere: Boolean = False): Boolean;
var
  AText1, AText2: string;
  L2: Integer;
begin
  AText1 := AnsiUpperCase(S1);
  AText2 := AnsiUpperCase(S2);
  L2 := Length(AText2);
  if L2 = 0 then
    Result := Length(AText1) = 0
  else
    if not APartialCompare then
      Result := AText1 = AText2
    else
      if AIsAnywhere then
        Result := AnsiPos(AText2, AText1) <> 0
      else
        Result := (Length(AText1) >= L2) and (Copy(AText1, 1, L2) = AText2);
end;

{ TcxCustomDataBinding }

constructor TcxCustomDataBinding.Create(AOwner, ADataComponent: TComponent);
begin
  inherited Create;
  FDataComponent := ADataComponent;
  FOwner := AOwner;
end;

function TcxCustomDataBinding.CanModify: Boolean;
begin
  Result := not ReadOnly;
end;

function TcxCustomDataBinding.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := False;
end;

function TcxCustomDataBinding.GetStoredValue(AValueSource: TcxDataEditValueSource;
  AFocused: Boolean): Variant;
begin
  Result := Null;
end;

function TcxCustomDataBinding.IsControlReadOnly: Boolean;
begin
  Result := ReadOnly;
end;

function TcxCustomDataBinding.IsDataSourceLive: Boolean;
begin
  Result := True;
end;

function TcxCustomDataBinding.IsDataStorage: Boolean;
begin
  Result := False;
end;

procedure TcxCustomDataBinding.Reset;
begin
end;

function TcxCustomDataBinding.SetEditMode: Boolean;
begin
  Result := CanModify;
end;

procedure TcxCustomDataBinding.SetStoredValue(AValueSource: TcxDataEditValueSource;
  const Value: Variant);
begin
end;

function TcxCustomDataBinding.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := False;
end;

procedure TcxCustomDataBinding.UpdateDataSource;
begin
end;

function TcxCustomDataBinding.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TcxCustomDataBinding.DataChange;
begin
  if Assigned(FOnDataChange) then
    FOnDataChange;
end;

procedure TcxCustomDataBinding.DataSetChange;
begin
  if Assigned(FOnDataSetChange) then
    FOnDataSetChange;
end;

function TcxCustomDataBinding.GetModified: Boolean;
begin
  Result := False;
end;

function TcxCustomDataBinding.GetReadOnly: Boolean;
begin
  Result := FReadOnly;
end;

procedure TcxCustomDataBinding.SetReadOnly(Value: Boolean);
begin
  if Value <> FReadOnly then
  begin
    FReadOnly := Value;
    DataSetChange;
  end;
end;

procedure TcxCustomDataBinding.UpdateData;
begin
  if Assigned(FOnUpdateData) then
    FOnUpdateData;
end;

procedure TcxCustomDataBinding.VisualControlChanged;
begin
end;

procedure TcxCustomDataBinding.SetVisualControl(Value: TComponent);
begin
  if Value <> FVisualControl then
  begin
    FVisualControl := Value;
    VisualControlChanged;
  end;
end;

end.
