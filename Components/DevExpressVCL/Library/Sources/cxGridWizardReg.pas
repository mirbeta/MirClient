{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressQuantumGrid                                       }
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

unit cxGridWizardReg;

{$I cxVer.inc}

interface

procedure Register;

implementation

uses
  Classes, cxGrid, cxGridCustomView, cxGridWizard, cxGridReg, cxViewEditor, DesignIntf, cxGridWizardCustomHelper;

type

  { TcxGridWizardHelper }

  TcxGridWizardHelper = class(TInterfacedObject, IcxGridWizard)
  public
    // IcxGridWizard
    procedure Execute(AGrid: TcxCustomGrid; AGridView: TcxCustomGridView = nil);
    function IsGridViewSupported(AGridViewClass: TClass): Boolean;
  end;

procedure Register;
begin
  ForceDemandLoadState(dlDisable);
  FGridWizard := TcxGridWizardHelper.Create;
end;

{ TcxGridWizardHelper }

procedure TcxGridWizardHelper.Execute(AGrid: TcxCustomGrid; AGridView: TcxCustomGridView);
begin
  ExecuteGridWizard(AGrid, AGridView);
end;

function TcxGridWizardHelper.IsGridViewSupported(AGridViewClass: TClass): Boolean;
begin
  Result := cxGridWizardHelperInfoList.GetItemIndexBy(AGridViewClass) >= 0;
end;

initialization

finalization
  FGridWizard := nil;

end.
