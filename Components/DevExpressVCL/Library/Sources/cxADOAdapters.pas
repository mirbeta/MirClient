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

unit cxADOAdapters;

{$I cxVer.inc}

interface

uses
  DB, cxDBData, cxFilter, cxDBFilter;

type
  { TcxADOProviderDetailFilterAdapter }

  TcxADOProviderDetailFilterAdapter = class(TcxDBProviderDetailFilterAdapter)
  public
    function IsCurrentQuery(ADataSet: TDataSet; const AParamNames: string; const AParamValues: Variant): Boolean; override;
    procedure ReopenSQL(ADataSet: TDataSet; const AParamNames: string; const AParamValues: Variant; var AReopened: Boolean); override;
  end;

  { TcxADOFilterOperatorAdapter }

  TcxADOFilterOperatorAdapter = class(TcxDBFilterOperatorAdapter)
  public
    procedure PrepareOperatorClass(ASender: TObject; ADataSet: TDataSet;
      var AOperatorClass: TcxFilterOperatorClass); override;
  end;

implementation

uses
  TypInfo, ADODB, cxVariants;

{ TcxADOProviderDetailFilterAdapter }

function TcxADOProviderDetailFilterAdapter.IsCurrentQuery(ADataSet: TDataSet;
  const AParamNames: string; const AParamValues: Variant): Boolean;
var
  AParameters: TParameters;
begin
  Result := False;
  if IsPublishedProp(ADataSet, 'Parameters') then
  begin
    AParameters := GetObjectProp(ADataSet, 'Parameters') as TParameters;
    if AParameters <> nil then
    begin
      if VarEquals(AParameters.ParamValues[AParamNames], AParamValues) then
        Result := True;
    end;
  end;
end;

procedure TcxADOProviderDetailFilterAdapter.ReopenSQL(ADataSet: TDataSet;
  const AParamNames: string; const AParamValues: Variant; var AReopened: Boolean);
var
  AParameters: TParameters;
begin
  if IsPublishedProp(ADataSet, 'Parameters') then
  begin
    AParameters := GetObjectProp(ADataSet, 'Parameters') as TParameters;
    if AParameters <> nil then
    begin
      if VarEquals(AParameters.ParamValues[AParamNames], AParamValues) then
        ADataSet.First
      else
      begin
        ADataSet.DisableControls;
        try
          ADataSet.Active := False;
          AParameters.ParamValues[AParamNames] := AParamValues;
          ADataSet.Active := True;
        finally
          ADataSet.EnableControls;
        end;
        AReopened := True; // set Flag if Query reopened
      end;
    end;
  end;
end;

{ TcxADOFilterOperatorAdapter }

procedure TcxADOFilterOperatorAdapter.PrepareOperatorClass(ASender: TObject;
  ADataSet: TDataSet; var AOperatorClass: TcxFilterOperatorClass);
begin
  {if AOperatorClass.InheritsFrom(TcxFilterNullOperator) or
    AOperatorClass.InheritsFrom(TcxFilterNotNullOperator) then
  begin
    if (ADataSet is TADOQuery) or
      ((ADataSet is TADODataSet) and (TADODataSet(ADataSet).CommandType = cmdText)) then
    begin
      if AOperatorClass.InheritsFrom(TcxFilterNotNullOperator) then
        AOperatorClass := TcxFilterSQLNotNullOperator
      else
        AOperatorClass := TcxFilterSQLNullOperator;
    end;
  end;}
end;

initialization
  cxDetailFilterControllers.RegisterAdapter(TCustomADODataSet, TcxADOProviderDetailFilterAdapter);
  cxFilterOperatorAdapters.RegisterAdapter(TCustomADODataSet, TcxADOFilterOperatorAdapter);

finalization
  cxDetailFilterControllers.UnregisterAdapter(TCustomADODataSet, TcxADOProviderDetailFilterAdapter);
  cxFilterOperatorAdapters.UnregisterAdapter(TCustomADODataSet, TcxADOFilterOperatorAdapter);

end.
