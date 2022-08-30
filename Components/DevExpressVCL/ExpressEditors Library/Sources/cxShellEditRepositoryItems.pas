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

unit cxShellEditRepositoryItems;

interface

uses
  cxEdit, cxShellComboBox;

type
  { TcxEditRepositoryShellComboBoxItem }

  TcxEditRepositoryShellComboBoxItem = class(TcxEditRepositoryItem)
  private
    function GetProperties: TcxShellComboBoxProperties;
    procedure SetProperties(Value: TcxShellComboBoxProperties);
  public
    class function GetEditPropertiesClass: TcxCustomEditPropertiesClass; override;
  published
    property Properties: TcxShellComboBoxProperties read GetProperties write SetProperties;
  end;

implementation

uses
  Classes;

{ TcxEditRepositoryShellComboBoxItem }

class function TcxEditRepositoryShellComboBoxItem.GetEditPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxShellComboBoxProperties;
end;

function TcxEditRepositoryShellComboBoxItem.GetProperties: TcxShellComboBoxProperties;
begin
  Result := TcxShellComboBoxProperties(inherited Properties);
end;

procedure TcxEditRepositoryShellComboBoxItem.SetProperties(Value: TcxShellComboBoxProperties);
begin
  inherited Properties := Value;
end;

initialization
  RegisterClasses([TcxEditRepositoryShellComboBoxItem]);

finalization
  UnregisterClasses([TcxEditRepositoryShellComboBoxItem]);

end.
