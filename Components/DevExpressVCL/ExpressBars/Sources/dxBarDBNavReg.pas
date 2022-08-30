{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressBars DB Navigator registering unit                }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSBARS AND ALL ACCOMPANYING VCL  }
{   CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY.                  }
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

unit dxBarDBNavReg;

{$I cxVer.inc}

interface

procedure Register;

implementation

uses
  Classes, SysUtils, DesignIntf, DesignEditors, dxBarDBNav, dxBarDBCheckLinksEd,
  dxBarReg;

{ TdxBarDBCheckLinksPropertyEditor }

type
  TdxBarDBCheckLinksPropertyEditor = class(TPropertyEditor)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
  end;

procedure TdxBarDBCheckLinksPropertyEditor.Edit;
begin
  if dxBarDBCheckLinksEditor(TdxBarDBNavigator(GetComponent(0))) then Modified;
end;

function TdxBarDBCheckLinksPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

function TdxBarDBCheckLinksPropertyEditor.GetValue: string;
begin
  Result := Format('(%s)', [TdxBarDBCheckLinks.ClassName]);
end;

procedure Register;
begin
  ForceDemandLoadState(dlDisable);
  RegisterComponents(dxBarProductPage, [TdxBarDBNavigator]);
  RegisterComponentEditor(TdxBarDBNavigator, TdxBarComponentEditor);
  RegisterNoIcon([TdxBarDBNavButton]);
  RegisterPropertyEditor(TypeInfo(TdxBarDBNavigator), TdxBarDBNavButton, 'BarDBNavigator', nil);
  RegisterPropertyEditor(TypeInfo(TdxBarDBCheckLinks), TdxBarDBNavigator, 'DBCheckLinks', TdxBarDBCheckLinksPropertyEditor);
end;

end.
