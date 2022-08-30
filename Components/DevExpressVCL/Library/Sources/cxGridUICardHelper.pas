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

unit cxGridUICardHelper;

{$I cxVer.inc}

interface

uses
  Classes, SysUtils, cxGrid, cxGridCustomView, cxGridUIHelper,
  cxGridCustomTableView, cxGridUICustomTableHelper, cxGridCardView;

type
  TcxGridCardViewOperationHelper = class(TcxGridCustomTableViewOperationHelper)
  protected
    function GetCardViewFromParameter(const AParameter: TcxCustomGridOperationHelperParameters): TcxGridCardView;
    procedure RegisterOperations; override;
    procedure DoLayoutDirection(const AParameter: TcxCustomGridOperationHelperParameters);
    procedure DoShowColumnsCustomizing(const AParameter: TcxCustomGridOperationHelperParameters);
  public
    class function GetViewClass: TcxCustomGridViewClass; override;
  end;

implementation

function TcxGridCardViewOperationHelper.GetCardViewFromParameter(const AParameter: TcxCustomGridOperationHelperParameters): TcxGridCardView;
begin
  Result := AParameter.View as TcxGridCardView;
end;

procedure TcxGridCardViewOperationHelper.RegisterOperations;
begin
  inherited RegisterOperations;
  RegisterOperation(GROP_LAYOUTDIRECTION, DoLayoutDirection);
  RegisterOperation(GROP_SHOWCOLUMNCUSTOMIZING, DoShowColumnsCustomizing);
end;

procedure TcxGridCardViewOperationHelper.DoLayoutDirection(
  const AParameter: TcxCustomGridOperationHelperParameters);
const
  BoolToLayoutDirection: Array[Boolean] of TcxGridCardViewLayoutDirection = (ldHorizontal, ldVertical);
  LayoutDirectionToBool: Array[TcxGridCardViewLayoutDirection] of Boolean = (False,True);
begin
  If AParameter.IsPerform then
    GetCardViewFromParameter(AParameter).LayoutDirection :=
        BoolToLayoutDirection[GetShowProperty(AParameter)]
  else
    SetShowProperty(AParameter,LayoutDirectionToBool[GetCardViewFromParameter(AParameter).LayoutDirection]);
end;

procedure TcxGridCardViewOperationHelper.DoShowColumnsCustomizing(const AParameter: TcxCustomGridOperationHelperParameters);
begin
  if AParameter.IsPerform then
    GetCardViewFromParameter(AParameter).Controller.Customization := GetShowProperty(AParameter)
  else SetShowProperty(AParameter, GetCardViewFromParameter(AParameter).Controller.Customization);
end;

class function TcxGridCardViewOperationHelper.GetViewClass: TcxCustomGridViewClass;
begin
  Result := TcxGridCardView;
end;

initialization
  RegisterGridViewOperationHelper(TcxGridCardViewOperationHelper);
end.
