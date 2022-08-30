{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressPrinting System                                   }
{                                                                    }
{           Copyright (C) 1998-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSPRINTING SYSTEM AND            }
{   ALL ACCOMPANYING VCL CONTROLS AS PART OF AN                      }
{   EXECUTABLE PROGRAM ONLY.                                         }
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

unit dxPSDBCtrlProducers;

interface

{$I cxVer.inc}

uses
  Controls, Graphics, DB, DBCtrls, dxPSCore, dxPSContainerLnk;

type
  TdxPSDBEditProducer = class(TdxPSCustomEditProducer)
  private
    function Field: TField;
  protected
    procedure InitializeItem(AnItem: TdxReportVisualItem); override;
  public
    function Control: TDBEdit; reintroduce; overload;
    class function ControlClass: TControlClass; override;
  end;

  TdxPSDBLookupComboBoxProducer = class(TdxPSContainerWinControlProducer)
  protected
    procedure InitializeItem(AnItem: TdxReportVisualItem); override;
  public
    function Control: TDBLookupComboBox; reintroduce; overload;
    class function ControlClass: TControlClass; override;
  end;

implementation

uses
  dxPSDBCtrlLnks;

{ TdxPSDBEditProducer }

function TdxPSDBEditProducer.Control: TDBEdit;
begin
  Result := inherited Control as TDBEdit;
end;

class function TdxPSDBEditProducer.ControlClass: TControlClass;
begin
  Result := TDBEdit;
end;

procedure TdxPSDBEditProducer.InitializeItem(AnItem: TdxReportVisualItem);
begin
  inherited;
  if Field <> nil then
    TdxReportCellString(AnItem).TextAlignX := dxPSCore.dxTextAlignX[Field.Alignment];
end;

function TdxPSDBEditProducer.Field: TField;
begin
  Result := Control.Field;
end;

{ TdxPSDBLookupComboBoxProducer }

function TdxPSDBLookupComboBoxProducer.Control: TDBLookupComboBox;
begin
  Result := inherited Control as TDBLookupComboBox;
end;

class function TdxPSDBLookupComboBoxProducer.ControlClass: TControlClass;
begin
  Result := TDBLookupComboBox;
end;

procedure TdxPSDBLookupComboBoxProducer.InitializeItem(AnItem: TdxReportVisualItem);
begin
  inherited;
  TdxReportCellString(AnItem).Text := Control.Text;
end;

procedure RegisterAssistants;
begin
  TdxPSDBEditProducer.Register;
  TdxPSDBLookupComboBoxProducer.Register;
end;

procedure UnregisterAssistants;
begin
  TdxPSDBLookupComboBoxProducer.Unregister;
  TdxPSDBEditProducer.Unregister;
end;

initialization
  RegisterAssistants;

finalization
  UnregisterAssistants;

end.
