{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressBars components                                   }
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

unit dxOfficeSearchBoxReg;

{$I cxVer.inc}

interface

procedure Register;

implementation

uses
  Classes, DesignIntf, DesignEditors, dxOfficeSearchBox, cxEditPropEditors;

type
  { TdxOfficeSearchBoxSearchSourceProperty }

  TdxOfficeSearchBoxSearchSourceProperty = class(TComponentProperty)
  strict private
    FProc: TGetStrProc;
    //
    procedure CheckComponent(const Value: string);
  public
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TdxOfficeSearchBoxPropertiesAccess = class(TdxOfficeSearchBoxProperties);


{ TdxOfficeSearchBoxSearchSourceProperty }

procedure TdxOfficeSearchBoxSearchSourceProperty.GetValues(Proc: TGetStrProc);
begin
  FProc := Proc;
  inherited GetValues(CheckComponent);
end;

procedure TdxOfficeSearchBoxSearchSourceProperty.CheckComponent(const Value: string);
var
  AComponent: TComponent;
begin
  AComponent := Designer.GetComponent(Value);
  if TdxOfficeSearchBoxPropertiesAccess.IsValidSearchSource(AComponent) then
    FProc(Value);
end;

{ Register }

procedure Register;
begin
  ForceDemandLoadState(dlDisable);
  RegisterComponents(cxEditorsLibraryProductPage, [TdxOfficeSearchBox]);
  RegisterPropertyEditor(TypeInfo(TComponent), TdxOfficeSearchBoxProperties, 'SearchSource', TdxOfficeSearchBoxSearchSourceProperty);
end;

end.
