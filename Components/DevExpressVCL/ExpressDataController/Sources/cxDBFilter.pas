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

unit cxDBFilter;

{$I cxVer.inc}

interface

uses
  DB, cxFilter, cxDB;

type
  { TcxFilterSQLNullOperator }

  TcxFilterSQLNullOperator = class(TcxFilterNullOperator)
  public
    function FilterText: string; override;
  end;

  { TcxFilterSQLNotNullOperator }

  TcxFilterSQLNotNullOperator = class(TcxFilterNotNullOperator)
  public
    function FilterText: string; override;
  end;

  { TcxDBFilterOperatorAdapter }

  TcxDBFilterOperatorAdapter = class(TcxDBAdapterItem)
  public
    procedure PrepareOperatorClass(ASender: TObject; ADataSet: TDataSet;
      var AOperatorClass: TcxFilterOperatorClass); virtual;
  end;

function cxGetFilterOperatorAdapter(ADataSet: TDataSet): TcxDBFilterOperatorAdapter;

var
  cxFilterOperatorAdapters: TcxDBAdapterList;

implementation

uses
  SysUtils;

function cxGetFilterOperatorAdapter(ADataSet: TDataSet): TcxDBFilterOperatorAdapter;
var
  AIndex: Integer;
begin
  if Assigned(ADataSet) and cxFilterOperatorAdapters.FindAdapter(TDataSetClass(ADataSet.ClassType), AIndex) then
    Result := cxFilterOperatorAdapters[AIndex] as TcxDBFilterOperatorAdapter
  else
    Result := nil;
end;

{ TcxFilterSQLNullOperator }

function TcxFilterSQLNullOperator.FilterText: string;
begin
  Result := 'IS';
end;

{ TcxFilterSQLNotNullOperator }

function TcxFilterSQLNotNullOperator.FilterText: string;
begin
  Result := 'IS NOT';
end;

{ TcxDBFilterOperatorAdapter }

procedure TcxDBFilterOperatorAdapter.PrepareOperatorClass(ASender: TObject;
  ADataSet: TDataSet; var AOperatorClass: TcxFilterOperatorClass);
begin
  if AOperatorClass.InheritsFrom(TcxFilterNullOperator) or
    AOperatorClass.InheritsFrom(TcxFilterNotNullOperator) then
  begin
    if Pos(AnsiUpperCase('Query'), AnsiUpperCase(ADataSet.ClassName)) <> 0 then
      if AOperatorClass.InheritsFrom(TcxFilterNotNullOperator) then
        AOperatorClass := TcxFilterSQLNotNullOperator
      else
        AOperatorClass := TcxFilterSQLNullOperator;
  end;
end;

initialization
  cxFilterOperatorAdapters := TcxDBAdapterList.Create;
  cxFilterOperatorAdapters.RegisterAdapter(TDataSet, TcxDBFilterOperatorAdapter);

finalization
  cxFilterOperatorAdapters.Free;
  cxFilterOperatorAdapters := nil;

end.
