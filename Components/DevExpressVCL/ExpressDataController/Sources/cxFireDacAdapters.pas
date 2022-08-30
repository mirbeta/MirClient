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

unit cxFireDacAdapters;

{$I cxVer.inc}

interface

uses
  Classes, Variants, DB,
{$IFDEF DELPHI19}
  FireDAC.Comp.Client, FireDAC.Stan.Param,
{$ELSE}
  uADCompClient, uADStanParam,
{$ENDIF}
  cxDBData, cxFilter, cxDBFilter;

type
  { TcxFireDacProviderDetailFilterAdapter }

  TcxFireDacProviderDetailFilterAdapter = class(TcxDBProviderDetailFilterAdapter)
  public
    function IsCurrentQuery(ADataSet: TDataSet; const AParamNames: string; const AParamValues: Variant): Boolean; override;
    procedure ReopenSQL(ADataSet: TDataSet; const AParamNames: string; const AParamValues: Variant; var AReopened: Boolean); override;
  end;

  { TcxFireDacFilterOperatorAdapter }

  TcxFireDacFilterOperatorAdapter = class(TcxDBFilterOperatorAdapter)
  public
    procedure PrepareOperatorClass(ASender: TObject; ADataSet: TDataSet;
      var AOperatorClass: TcxFilterOperatorClass); override;
  end;

implementation

uses
  TypInfo, cxVariants;

{ TcxFireDacProviderDetailFilterAdapter }

function TcxFireDacProviderDetailFilterAdapter.IsCurrentQuery(ADataSet: TDataSet;
  const AParamNames: string; const AParamValues: Variant): Boolean;
var
  ASQLParams: {$IFDEF DELPHI19}TFDParams{$ELSE}TADParams{$ENDIF};
begin
  Result := False;
  if ADataSet is {$IFDEF DELPHI19}TFDQuery{$ELSE}TADQuery{$ENDIF} then
  begin
    ASQLParams := {$IFDEF DELPHI19}TFDQuery{$ELSE}TADQuery{$ENDIF}(ADataSet).Params;
    if VarEquals(ASQLParams.ParamValues[AParamNames], AParamValues) then
      Result := True;
  end;
end;

procedure TcxFireDacProviderDetailFilterAdapter.ReopenSQL(ADataSet: TDataSet;
  const AParamNames: string; const AParamValues: Variant; var AReopened: Boolean);
var
  ASQLParams: {$IFDEF DELPHI19}TFDParams{$ELSE}TADParams{$ENDIF};
begin
  if ADataSet is {$IFDEF DELPHI19}TFDQuery{$ELSE}TADQuery{$ENDIF} then
  begin
    ASQLParams := {$IFDEF DELPHI19}TFDQuery{$ELSE}TADQuery{$ENDIF}(ADataSet).Params;
    if VarEquals(ASQLParams.ParamValues[AParamNames], AParamValues) then
      ADataSet.First
    else
    begin
      ADataSet.DisableControls;
      try
        ADataSet.Active := False;
        ASQLParams.ParamValues[AParamNames] := AParamValues;
        ADataSet.Active := True;
      finally
        ADataSet.EnableControls;
      end;
      AReopened := True; // set Flag if Query reopened
    end;
  end;
end;

{ TcxFireDacFilterOperatorAdapter }

procedure TcxFireDacFilterOperatorAdapter.PrepareOperatorClass(ASender: TObject;
  ADataSet: TDataSet; var AOperatorClass: TcxFilterOperatorClass);
begin
  if AOperatorClass.InheritsFrom(TcxFilterNullOperator) or
    AOperatorClass.InheritsFrom(TcxFilterNotNullOperator) then
  begin
    if ADataSet is {$IFDEF DELPHI19}TFDQuery{$ELSE}TADQuery{$ENDIF} then
    begin
      if AOperatorClass.InheritsFrom(TcxFilterNotNullOperator) then
        AOperatorClass := TcxFilterSQLNotNullOperator
      else
        AOperatorClass := TcxFilterSQLNullOperator;
    end;
  end;
end;

initialization
  cxDetailFilterControllers.RegisterAdapter({$IFDEF DELPHI19}TFDQuery{$ELSE}TADQuery{$ENDIF}, TcxFireDacProviderDetailFilterAdapter);
  cxFilterOperatorAdapters.RegisterAdapter({$IFDEF DELPHI19}TFDQuery{$ELSE}TADQuery{$ENDIF}, TcxFireDacFilterOperatorAdapter);

finalization
  cxDetailFilterControllers.UnregisterAdapter({$IFDEF DELPHI19}TFDQuery{$ELSE}TADQuery{$ENDIF}, TcxFireDacProviderDetailFilterAdapter);
  cxFilterOperatorAdapters.UnregisterAdapter({$IFDEF DELPHI19}TFDQuery{$ELSE}TADQuery{$ENDIF}, TcxFireDacFilterOperatorAdapter);

end.
