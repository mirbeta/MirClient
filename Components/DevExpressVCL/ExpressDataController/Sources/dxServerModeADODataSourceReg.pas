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

unit dxServerModeADODataSourceReg;

{$I cxVer.inc}

interface

uses
  Windows, Types, Classes, DesignIntf,
  dxCoreReg, dxServerModeReg, dxServerModeADODataSource;

procedure Register;

implementation

uses
  TreeIntf, SysUtils;

const
  cConnectionSprigPrefix = '<ImpliedConnection>'; { do not localize }

type
  TdxServerModeADODataSourceSprig = class(TdxServerModeCustomDataSourceSprig)
  protected
    function GetImpliedConnectionString: string; override;
  end;

  TdxServerModeADOQueryDataSourceSprig = class(TdxServerModeCustomDataSourceSprig)
  protected
    function GetImpliedConnectionString: string; override;
  end;

{ TdxServerModeADODataSourceSprig }

function TdxServerModeADODataSourceSprig.GetImpliedConnectionString: string;
begin
  Result := Format('%s.%s', [cConnectionSprigPrefix, '']);
end;

{ TdxServerModeADOQueryDataSourceSprig }

function TdxServerModeADOQueryDataSourceSprig.GetImpliedConnectionString: string;
begin
  Result := Format('%s.%s', [cConnectionSprigPrefix, '']);
end;

procedure Register;
begin
  ForceDemandLoadState(dlDisable);
{$IFNDEF NONDB}
  RegisterComponents(dxCoreLibraryProductPage, [TdxServerModeADODataSource, TdxServerModeADOQueryDataSource]);
  RegisterSprigType(TdxServerModeADODataSource, TdxServerModeADODataSourceSprig);
  RegisterSprigType(TdxServerModeADOQueryDataSource, TdxServerModeADOQueryDataSourceSprig);
{$ENDIF}
end;

end.
