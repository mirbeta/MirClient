{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressQuantumGrid Utils                                 }
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

unit cxGridGroupByBoxPopupMenuItems;

{$I cxVer.inc}

interface

uses
  Classes, dxCore, cxGridMenuOperations, cxGridCustomTableView, cxCustomData;

type
  { TcxGridGroupByBoxPopupMenuOperation }

  TcxGridGroupByBoxPopupMenuOperation = class(TcxGridTablePopupMenuOperation)
  private
    function GetGroups: TcxDataControllerGroups;
  protected
    function GetEnabled: Boolean; override;

    property Groups: TcxDataControllerGroups read GetGroups;
  end;

  { TcxGridGroupByBoxExpand }

  TcxGridGroupByBoxExpand = class(TcxGridGroupByBoxPopupMenuOperation)
  protected
    procedure Execute(Sender: TObject); override;
    function GetImageResourceName: string; override;
  public
    constructor Create; override;
  end;

  { TcxGridGroupByBoxCollapse }

  TcxGridGroupByBoxCollapse = class(TcxGridGroupByBoxPopupMenuOperation)
  protected
    procedure Execute(Sender: TObject); override;
    function GetImageResourceName: string; override;
  public
    constructor Create; override;
  end;

  { TcxGridGroupByBoxClearGrouping }

  TcxGridGroupByBoxClearGrouping = class(TcxGridGroupByBoxPopupMenuOperation)
  protected
    procedure Execute(Sender: TObject); override;
    function GetImageResourceName: string; override;
  public
    constructor Create; override;
  end;

  { TcxGridGroupByBoxHide }

  TcxGridGroupByBoxHide = class(TcxGridGroupByBoxPopupMenuOperation)
  protected
    procedure Execute(Sender: TObject); override;
    function GetEnabled: Boolean; override;
    function GetImageResourceName: string; override;
  public
    constructor Create; override;
  end;

  { TcxGridGroupByBoxPopupMenuOperations }

  TcxGridGroupByBoxPopupMenuOperations = class(TcxGridPopupMenuOperations)
  protected
    procedure AddItems; override;
  end;

implementation

uses
  Variants, cxEdit, cxGridTableView, cxGridPopupMenuConsts;

const
  AImageName = 'GBBIMG';

{ TcxGridGroupByBoxPopupMenuOperation }

function TcxGridGroupByBoxPopupMenuOperation.GetEnabled: Boolean;
begin
  Result := Groups.GroupingItemCount > 0;
end;

function TcxGridGroupByBoxPopupMenuOperation.GetGroups: TcxDataControllerGroups;
begin
  Result := HitGridView.DataController.Groups;
end;

{ TcxGridGroupByBoxToggleMenuOperation }

constructor TcxGridGroupByBoxExpand.Create;
begin
  inherited Create;
  ResCaption := @cxSGridFullExpand;
end;

procedure TcxGridGroupByBoxExpand.Execute(Sender: TObject);
begin
  Groups.FullExpand;
end;

function TcxGridGroupByBoxExpand.GetImageResourceName: string;
begin
  Result := AImageName + '1';
end;

{ TcxGridGroupByBoxCollapse }

constructor TcxGridGroupByBoxCollapse.Create;
begin
  inherited Create;
  ResCaption := @cxSGridFullCollapse;
end;

procedure TcxGridGroupByBoxCollapse.Execute(Sender: TObject);
begin
  Groups.FullCollapse;
end;

function TcxGridGroupByBoxCollapse.GetImageResourceName: string;
begin
  Result := AImageName + '2';
end;

{ TcxGridGroupByBoxClearGrouping }

constructor TcxGridGroupByBoxClearGrouping.Create;
begin
  inherited Create;
  ResCaption := @cxSGridClearGrouping;
end;

procedure TcxGridGroupByBoxClearGrouping.Execute(Sender: TObject);
begin
  HitGridView.Controller.ClearGrouping;
end;

function TcxGridGroupByBoxClearGrouping.GetImageResourceName: string;
begin
  Result := AImageName + '3';
end;

{ TcxGridGroupByBoxHide }

constructor TcxGridGroupByBoxHide.Create;
begin
  inherited Create;
  ResCaption := @cxSGridHideGroupByBox;
end;

procedure TcxGridGroupByBoxHide.Execute(Sender: TObject);
begin
  GridOperationHelper.DoShowGroupingPanel(False);
end;

function TcxGridGroupByBoxHide.GetEnabled: Boolean;
begin
  Result := True;
end;

function TcxGridGroupByBoxHide.GetImageResourceName: string;
begin
  Result := 'HDRIMG7';
end;

{ TcxGridFindPanelPopupMenuOperations }

procedure TcxGridGroupByBoxPopupMenuOperations.AddItems;
begin
  AddItem(TcxGridGroupByBoxExpand);
  AddItem(TcxGridGroupByBoxCollapse);
  AddItem(TcxGridGroupByBoxClearGrouping).BeginGroup := True;
  AddItem(TcxGridGroupByBoxHide);
end;

end.
