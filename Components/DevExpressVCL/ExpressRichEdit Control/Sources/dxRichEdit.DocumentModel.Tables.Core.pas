{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressRichEditControl                                   }
{                                                                    }
{           Copyright (c) 2000-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSRICHEDITCONTROL AND ALL        }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM       }
{   ONLY.                                                            }
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

unit dxRichEdit.DocumentModel.Tables.Core;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  dxCore, dxCoreClasses;

type
  TdxCustomTableCell = class;

  { TdxCustomTasble }

  TdxCustomTable = class abstract
  public
    function GetCellCore(ARowIndex, AColumnIndex: Integer): TdxCustomTableCell; virtual;
    function GetIndexCore: Integer; virtual;
    function GetParentCellCore: TdxCustomTableCell; virtual; abstract;
    function IsContainsFrame: Boolean; virtual;
    function UseFloatingPosition: Boolean; virtual;
  end;

  { TdxCustomTableRow }

  TdxCustomTableRow = class
  public
    function GetIndexCore: Integer; virtual;
  end;

  { TdxCustomTableCell }

  TdxCustomTableCell = class abstract(TcxIUnknownObject)
  public
    function IsContinueVerticalMerging: Boolean; virtual;
    function GetEndParagraphIndexCore: Integer; virtual;
    function GetIndexCore: Integer; virtual;
    function GetRowCore: TdxCustomTableRow; virtual;
    function GetTableCore: TdxCustomTable; virtual;
  end;

implementation

{ TdxCustomTable }

function TdxCustomTable.GetCellCore(ARowIndex,
  AColumnIndex: Integer): TdxCustomTableCell;
begin
  Result := nil;
end;

function TdxCustomTable.GetIndexCore: Integer;
begin
  Result := -1;
end;

function TdxCustomTable.IsContainsFrame: Boolean;
begin
  Result := False;
end;

function TdxCustomTable.UseFloatingPosition: Boolean;
begin
  Result := False;
end;

{ TdxCustomTableRow }

function TdxCustomTableRow.GetIndexCore: Integer;
begin
  Result := -1;
end;

{ TdxCustomTableCell }

function TdxCustomTableCell.IsContinueVerticalMerging: Boolean;
begin
  Result := False;
end;

function TdxCustomTableCell.GetEndParagraphIndexCore: Integer;
begin
  Result := -1;
end;

function TdxCustomTableCell.GetTableCore: TdxCustomTable;
begin
  Result := nil;
end;

function TdxCustomTableCell.GetIndexCore: Integer;
begin
  Result := -1;
end;

function TdxCustomTableCell.GetRowCore: TdxCustomTableRow;
begin
  Result := nil;
end;

end.
