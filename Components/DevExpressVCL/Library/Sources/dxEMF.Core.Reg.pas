{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressEntityMapping Framework                           }
{                                                                    }
{           Copyright (c) 2016-2019 Developer Express Inc.           }
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
{   (DCU, OBJ, DLL, DPU, SO, ETC.) ARE CONFIDENTIAL AND PROPRIETARY  }
{   TRADE SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER }
{   IS LICENSED TO DISTRIBUTE THE EXPRESSENTITYMAPPING FRAMEWORK     }
{   AS PART OF AN EXECUTABLE PROGRAM ONLY.                           }
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

unit dxEMF.Core.Reg;

interface

uses
  SysUtils, VCLEditors, DesignIntf, DesignEditors, DesignMenus, Classes,
  dxCore, dxCoreReg;

type
  { TdxEMFCustomComponentEditor }

  TdxEMFCustomComponentEditorClass = class of TdxEMFCustomComponentEditor;
  TdxEMFCustomComponentEditor = class(TdxComponentEditor)
  protected
    function GetProductName: string; override;
  end;

procedure RegisterEMFComponent(AComponentClass: TComponentClass); overload;
procedure RegisterEMFComponent(AComponentClass: TComponentClass; AEditorClass: TdxEMFCustomComponentEditorClass); overload;

implementation

const
  dxEMFProductName = 'ExpressEntityMapping Framework';
  dxEMFProductPage = 'ExpressEntityMapping Framework';

procedure RegisterEMFComponent(AComponentClass: TComponentClass);
begin
  RegisterEMFComponent(AComponentClass, TdxEMFCustomComponentEditor);
end;

procedure RegisterEMFComponent(AComponentClass: TComponentClass; AEditorClass: TdxEMFCustomComponentEditorClass);
begin
  ForceDemandLoadState(dlDisable);
  RegisterComponents(dxEMFProductPage, [AComponentClass]);
  RegisterComponentEditor(AComponentClass, AEditorClass);
end;

{ TdxEMFCustomComponentEditor }

function TdxEMFCustomComponentEditor.GetProductName: string;
begin
  Result := dxEMFProductName;
end;

end.
