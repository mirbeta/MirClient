{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressQuantumGrid Utils                                 }
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

unit cxGridUIBandedTableHelper;

{$I cxVer.inc}

interface

uses
  Classes, SysUtils, cxGrid, cxGridCustomView, cxGridUIHelper,
  cxGridCustomTableView, cxGridUICustomTableHelper, cxGridUITableHelper;

type
  TcxGridBandedTableViewOperationHelper = class(TcxGridTableViewOperationHelper)
  protected
    procedure RegisterOperations; override;
    procedure DoShowBands(const AParameter: TcxCustomGridOperationHelperParameters);
  public
    class function GetViewClass: TcxCustomGridViewClass; override;
  end;

implementation

uses
  cxGridBandedTableView;

{ TcxGridBandedTableViewOperationHelper }

procedure TcxGridBandedTableViewOperationHelper.RegisterOperations;
begin
  inherited RegisterOperations;
  RegisterOperation(GROP_SHOWBANDS,DoShowBands);
end;

procedure TcxGridBandedTableViewOperationHelper.DoShowBands(
  const AParameter: TcxCustomGridOperationHelperParameters);
begin
  If AParameter.IsPerform then
    (AParameter.View as TcxGridBandedTableView).OptionsView.BandHeaders := GetShowProperty(AParameter)
  else SetShowProperty(AParameter, (AParameter.View as TcxGridBandedTableView).OptionsView.BandHeaders);
end;

class function TcxGridBandedTableViewOperationHelper.GetViewClass: TcxCustomGridViewClass;
begin
  Result := TcxGridBandedTableView;
end;

initialization
  RegisterGridViewOperationHelper(TcxGridBandedTableViewOperationHelper);

end.
