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

unit cxIBXAdapters;

{$I cxVer.inc}

interface

uses
  Variants, DB, IBSQL, cxDBData, cxFilter, cxDBFilter;

type
  { TcxIBXProviderDetailFilterAdapter }

  TcxIBXProviderDetailFilterAdapter = class(TcxDBProviderDetailFilterAdapter)
  protected
    function GetParamValues(ASQLParams: TIBXSQLDA): Variant;
    procedure SetParamValues(ASQLParams: TIBXSQLDA; const AValues: Variant);
  public
    function IsCurrentQuery(ADataSet: TDataSet; const AParamNames: string; const AParamValues: Variant): Boolean; override;
    procedure ReopenSQL(ADataSet: TDataSet; const AParamNames: string; const AParamValues: Variant; var AReopened: Boolean); override;
  end;

  { TcxIBXFilterOperatorAdapter }

  TcxIBXFilterOperatorAdapter = class(TcxDBFilterOperatorAdapter)
  public
    procedure PrepareOperatorClass(ASender: TObject; ADataSet: TDataSet;
      var AOperatorClass: TcxFilterOperatorClass); override;
  end;

implementation

uses
  TypInfo, IBCustomDataSet, cxVariants;

{ TcxIBXProviderDetailFilterAdapter }

function TcxIBXProviderDetailFilterAdapter.IsCurrentQuery(ADataSet: TDataSet;
  const AParamNames: string; const AParamValues: Variant): Boolean;
var
  ASQLParams: TIBXSQLDA;
begin
  Result := False;
  if ADataSet is TIBDataSet then
  begin
    ASQLParams := TIBDataSet(ADataSet).Params;
    if VarEquals(GetParamValues(ASQLParams), AParamValues) then
      Result := True;
  end;
end;

procedure TcxIBXProviderDetailFilterAdapter.ReopenSQL(ADataSet: TDataSet;
  const AParamNames: string; const AParamValues: Variant; var AReopened: Boolean);
var
  ASQLParams: TIBXSQLDA;
begin
  if ADataSet is TIBDataSet then
  begin
    ASQLParams := TIBDataSet(ADataSet).Params;
    if VarEquals(GetParamValues(ASQLParams), AParamValues) then
      ADataSet.First
    else
    begin
      ADataSet.DisableControls;
      try
        ADataSet.Active := False;
        SetParamValues(ASQLParams, AParamValues);
        ADataSet.Active := True;
      finally
        ADataSet.EnableControls;
      end;
      AReopened := True; // set Flag if Query reopened
    end;
  end;
end;

function TcxIBXProviderDetailFilterAdapter.GetParamValues(ASQLParams: TIBXSQLDA): Variant;
var
  I: Integer;
begin
  if ASQLParams.Count = 1 then
    Result := Variant(ASQLParams[0].Value)
  else
  begin
    Result := VarArrayCreate([0, ASQLParams.Count - 1], varVariant);
    for I := 0 to ASQLParams.Count - 1 do
      Result[I] := Variant(ASQLParams[I].Value);
  end;
end;

procedure TcxIBXProviderDetailFilterAdapter.SetParamValues(ASQLParams: TIBXSQLDA;
  const AValues: Variant);
var
  I: Integer;
begin
  if ASQLParams.Count = 1 then
    ASQLParams[0].Value := AValues
  else
    for I := 0 to ASQLParams.Count - 1 do
      ASQLParams[I].Value := AValues[I];
end;

{ TcxIBXFilterOperatorAdapter }

procedure TcxIBXFilterOperatorAdapter.PrepareOperatorClass(ASender: TObject;
  ADataSet: TDataSet; var AOperatorClass: TcxFilterOperatorClass);
begin
  if AOperatorClass.InheritsFrom(TcxFilterNullOperator) or
    AOperatorClass.InheritsFrom(TcxFilterNotNullOperator) then
  begin
    if ADataSet is TIBDataSet then
    begin
      if AOperatorClass.InheritsFrom(TcxFilterNotNullOperator) then
        AOperatorClass := TcxFilterSQLNotNullOperator
      else
        AOperatorClass := TcxFilterSQLNullOperator;
    end;
  end;
end;

initialization
  cxDetailFilterControllers.RegisterAdapter(TIBDataSet, TcxIBXProviderDetailFilterAdapter);
  cxFilterOperatorAdapters.RegisterAdapter(TIBDataSet, TcxIBXFilterOperatorAdapter);

finalization
  cxDetailFilterControllers.UnregisterAdapter(TIBDataSet, TcxIBXProviderDetailFilterAdapter);
  cxFilterOperatorAdapters.UnregisterAdapter(TIBDataSet, TcxIBXFilterOperatorAdapter);

end.
