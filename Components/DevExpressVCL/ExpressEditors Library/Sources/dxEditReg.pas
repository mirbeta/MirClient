{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressEditors                                           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSEDITORS AND ALL                }
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

unit dxEditReg;

{$I cxVer.inc}

interface

uses
  SysUtils, Classes, Controls, DB, DesignIntf, DSDesign;

procedure Register;

implementation

uses
  DesignEditors, VCLEditors, Windows, dximctrl, cxEditReg, cxEditPropEditors,
  dxImagePropEditor, dxSpinImagePropEditor;

type
  TdxImageControlItemProperties = class(TPropertyEditor)
  public
    function GetValue: string; override;
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  TdxSpinImageItemsProperties = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

{ TdxImageControlItemProperties }

function TdxImageControlItemProperties.GetValue: string;
begin
  Result := Format('(%s)', [TStrings.ClassName]);
end;

function TdxImageControlItemProperties.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paDialog];
end;

procedure TdxImageControlItemProperties.Edit;
begin
  if(ExpressImageItemsPropEditor(GetComponent(0) as TWinControl)) then
    Modified;
end;

{ TdxSpinImageItemsProperties }

function TdxSpinImageItemsProperties.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

procedure TdxSpinImageItemsProperties.Edit;
begin
  if(ExpressSpinImageItemsPropEditor(GetComponent(0) as TdxCustomSpinImage)) then
    Modified;
end;

procedure Register;
begin
  ForceDemandLoadState(dlDisable);

  RegisterComponents(cxEditorsUtilitiesProductPage, [TdxImageListBox, TdxSpinImage, TdxImageComboBox]);

  RegisterComponentEditor(TdxImageListBox, TcxEditComponentEditor);
  RegisterComponentEditor(TdxSpinImage, TcxEditComponentEditor);
  RegisterComponentEditor(TdxImageComboBox, TcxEditComponentEditor);

  RegisterPropertyEditor(TypeInfo(TStrings), TdxCustomImageListBox, 'Items', TdxImageControlItemProperties);
  RegisterPropertyEditor(TypeInfo(TStrings), TdxImageComboBox, 'Items', TdxImageControlItemProperties);
  RegisterPropertyEditor(TypeInfo(TdxSpinImageItems), TdxCustomSpinImage, 'Items', TdxSpinImageItemsProperties);
end;

end.
