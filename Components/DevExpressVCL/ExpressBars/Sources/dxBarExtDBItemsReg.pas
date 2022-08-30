{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressBars extended DB items registering unit           }
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

unit dxBarExtDBItemsReg;

{$I cxVer.inc}

interface

procedure Register;

implementation

uses
  DesignIntf, DesignEditors, WideStrings, Classes, dxBarExtDBItems;

type
  { TdxBarStringList }

{$IFDEF DELPHI17}
  TdxBarStringList = TStringList;
{$ELSE}
  TdxBarStringList = TWideStringList;
{$ENDIF}

  { TdxBarLookupComboFieldProperty }

  TdxBarLookupComboFieldProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

function TdxBarLookupComboFieldProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paMultiSelect];
end;

procedure TdxBarLookupComboFieldProperty.GetValues(Proc: TGetStrProc);
var
  AComponent: TdxBarLookupCombo;
  AValues: TdxBarStringList;
  I: Integer;
begin
  AValues := TdxBarStringList.Create;
  try
    AComponent := TdxBarLookupCombo(GetComponent(0));
    if (AComponent.ListSource <> nil) and (AComponent.ListSource.DataSet <> nil) then
    begin
      AComponent.ListSource.DataSet.GetFieldNames(AValues);
      for I := 0 to AValues.Count - 1 do
        Proc(AValues[I]);
    end;
  finally
    AValues.Free;
  end;
end;

procedure Register;
begin
  ForceDemandLoadState(dlDisable);

  RegisterNoIcon([TdxBarLookupCombo]);
  RegisterPropertyEditor(TypeInfo(string), TdxBarLookupCombo, 'KeyField', TdxBarLookupComboFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TdxBarLookupCombo, 'ListField', TdxBarLookupComboFieldProperty);
end;

end.
