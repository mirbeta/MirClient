{********************************************************************}
{       Developer Express Visual Component Library                   }
{           Developer Express Visual Component Library               }
{                                                                    }
{       Copyright (c) 2003-2019 Developer Express Inc.               }
{           ALL RIGHTS RESERVED                                      }
{           ALL RIGHTS RESERVED                                      }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSSCHEDULER AND ALL ACCOMPANYING }
{   VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY.              }
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

unit cxSchedulercxGridConnectionReg;

{$I cxVer.inc}

interface

uses
  Classes,
  Types, DesignIntf, DesignEditors,
  cxSchedulercxGridConnection, dxCoreReg, cxSchedulerReg, cxPropEditors;

type
  { TcxSchedulerGridConnectionGridPopupMenuEventsProperty }

  TcxSchedulerGridConnectionGridPopupMenuEventsProperty = class(TcxNestedEventProperty)
  protected
    function GetInstance: TPersistent; override;
  end;

procedure Register;

implementation

{ TcxSchedulerGridConnectionGridPopupMenuEventsProperty }

function TcxSchedulerGridConnectionGridPopupMenuEventsProperty.GetInstance: TPersistent;
begin
  if (GetComponent(0) is TcxSchedulerGridConnection) then
    Result := TcxSchedulerGridConnection(GetComponent(0)).GridPopupMenu
  else
    Result := nil;
end;

procedure Register;
begin
  ForceDemandLoadState(dlDisable);
  RegisterComponents(dxCoreLibraryProductPage, [TcxSchedulerGridConnection]);
  RegisterClasses([TcxSchedulerGridConnection]);
  RegisterComponentEditor(TcxSchedulerGridConnection, cxSchedulerComponentEditor);

  RegisterPropertyEditor(TypeInfo(TNotifyEvent), TcxSchedulerGridConnection,
    'GridPopupMenuEvents', TcxSchedulerGridConnectionGridPopupMenuEventsProperty);
end;

end.
