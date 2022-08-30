{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressQuantumGrid                                       }
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

unit cxGridDataRowPopupMenuItems;

{$I cxVer.inc}

interface

uses
  dxCore, cxCustomData, cxGridCustomView, cxGridTableView, cxGridMenuOperations, Classes;

type
  TcxGridDataRowPopupMenuOperation = class(TcxGridTablePopupMenuOperation)
  private
    function GetHitRow: TcxGridDataRow;
  public
    property HitRow: TcxGridDataRow read GetHitRow;
  end;

  TcxGridDataRowCustomFixOperation = class(TcxGridDataRowPopupMenuOperation)
  protected
    procedure Execute(Sender: TObject); override;
    function GetDown: Boolean; override;
    function GetOperation: TcxDataControllerRowFixedState; virtual; abstract;
    function GetResCaption: Pointer; virtual; abstract;
    function GetVisible: Boolean; override;
  public
    constructor Create; override;
  end;

  TcxGridDataRowFixToTopOperation = class(TcxGridDataRowCustomFixOperation)
  protected
    function GetImageResourceName: string; override;
    function GetOperation: TcxDataControllerRowFixedState; override;
    function GetResCaption: Pointer; override;
  end;

  TcxGridDataRowFixToBottomOperation = class(TcxGridDataRowCustomFixOperation)
  protected
    function GetImageResourceName: string; override;
    function GetOperation: TcxDataControllerRowFixedState; override;
    function GetResCaption: Pointer; override;
  end;

  TcxGridDataRowUnfixOperation = class(TcxGridDataRowCustomFixOperation)
  protected
    function GetImageResourceName: string; override;
    function GetOperation: TcxDataControllerRowFixedState; override;
    function GetResCaption: Pointer; override;
  end;

  TcxGridDataRowPopupMenuOperations = class(TcxGridPopupMenuOperations)
  protected
    procedure AddItems; override;
  public
    function CanProcess(AHitTest: TcxCustomGridHitTest): Boolean; override;
  end;

implementation

uses
  SysUtils, cxClasses, cxGridCustomTableView, cxGridPopupMenuConsts, cxGridStrs;

const
  AImageName = 'DTRIMG';

{ TcxGridDataRowPopupMenuOperation }

function TcxGridDataRowPopupMenuOperation.GetHitRow: TcxGridDataRow;
begin
  Result := TcxGridDataRow(TcxGridRecordHitTest(GridPopupMenu.HitTest).GridRecord);
end;

{ TcxGridDataRowCustomFixedOperation }

constructor TcxGridDataRowCustomFixOperation.Create;
begin
  inherited Create;
  ResCaption := GetResCaption;
end;

procedure TcxGridDataRowCustomFixOperation.Execute(Sender: TObject);
begin
  HitRow.FixedState := GetOperation;
end;

function TcxGridDataRowCustomFixOperation.GetDown: Boolean;
begin
  Result := HitRow.FixedState = GetOperation;
end;

function TcxGridDataRowCustomFixOperation.GetVisible: Boolean;
begin
  Result := HitGridView.OptionsCustomize.DataRowFixing and not HitRow.IsNewItemRow and not HitRow.IsFilterRow and
    (HitRow.FixedState <> GetOperation);
end;

{ TcxGridDataRowFixToTopOperation }

function TcxGridDataRowFixToTopOperation.GetImageResourceName: string;
begin
  Result := AImageName + '1';
end;

function TcxGridDataRowFixToTopOperation.GetOperation: TcxDataControllerRowFixedState;
begin
  Result := rfsFixedToTop;
end;

function TcxGridDataRowFixToTopOperation.GetResCaption: Pointer;
begin
  Result := @scxGridDataRowFixingPopupCommandFixToTop;
end;

{ TcxGridDataRowFixToBottomOperation }

function TcxGridDataRowFixToBottomOperation.GetImageResourceName: string;
begin
  Result := AImageName + '3';
end;

function TcxGridDataRowFixToBottomOperation.GetOperation: TcxDataControllerRowFixedState;
begin
  Result := rfsFixedToBottom;
end;

function TcxGridDataRowFixToBottomOperation.GetResCaption: Pointer;
begin
  Result := @scxGridDataRowFixingPopupCommandFixToBottom;
end;

{ TcxGridDataRowUnfixOperation }

function TcxGridDataRowUnfixOperation.GetImageResourceName: string;
begin
  Result := AImageName + '2';
end;

function TcxGridDataRowUnfixOperation.GetOperation: TcxDataControllerRowFixedState;
begin
  Result := rfsNotFixed;
end;

function TcxGridDataRowUnfixOperation.GetResCaption: Pointer;
begin
  Result := @scxGridDataRowFixingPopupCommandUnfix;
end;

{ TcxGridDataRowPopupMenuOperations }

function TcxGridDataRowPopupMenuOperations.CanProcess(AHitTest: TcxCustomGridHitTest): Boolean;
begin
  Result := inherited CanProcess(AHitTest) and (AHitTest is TcxGridRecordHitTest) and
    TcxGridRecordHitTest(AHitTest).GridRecord.IsData;
end;

procedure TcxGridDataRowPopupMenuOperations.AddItems;
begin
  AddItem(TcxGridDataRowFixToTopOperation);
  AddItem(TcxGridDataRowUnfixOperation);
  AddItem(TcxGridDataRowFixToBottomOperation);
end;

end.
