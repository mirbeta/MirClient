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

unit dxRichEdit.DocumentModel.VisibleTextFilter.Simple;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  dxRichEdit.Utils.Types,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.DocumentModel.VisibleTextFilter.Core;

type
  { TdxSimpleVisibleTextFilter }

  TdxSimpleVisibleTextFilter = class abstract(TdxVisibleTextFilterBase)
  strict private
    function GetPieceTable: TdxSimplePieceTable;
  public
    property PieceTable: TdxSimplePieceTable read GetPieceTable;
  end;

  { TdxEmptyTextFilter }

  TdxEmptyTextFilter = class(TdxSimpleVisibleTextFilter)
  public
    function Clone(APieceTable: TdxCustomPieceTable): IdxVisibleTextFilter; override;
  end;

  { TdxVisibleTextFilter }

  TdxVisibleTextFilter = class(TdxSimpleVisibleTextFilter)
  public
    function Clone(APieceTable: TdxCustomPieceTable): IdxVisibleTextFilter; override;
    function IsRunVisible(const ARunIndex: TdxRunIndex): Boolean; override;
    function IsRunVisibleCore(const ARunIndex: TdxRunIndex): TdxRunVisibility; override;
  end;

  { TdxVisibleOnlyTextFilter }

  TdxVisibleOnlyTextFilter = class(TdxSimpleVisibleTextFilter)
  public
    function Clone(APieceTable: TdxCustomPieceTable): IdxVisibleTextFilter; override;
    function IsRunVisible(const ARunIndex: TdxRunIndex): Boolean; override;
    function IsRunVisibleCore(const ARunIndex: TdxRunIndex): TdxRunVisibility; override;
  end;

implementation

{ TdxSimpleVisibleTextFilter }

function TdxSimpleVisibleTextFilter.GetPieceTable: TdxSimplePieceTable;
begin
  Result := TdxSimplePieceTable(inherited PieceTable);
end;

{ TdxEmptyTextFilter }

function TdxEmptyTextFilter.Clone(APieceTable: TdxCustomPieceTable): IdxVisibleTextFilter;
begin
  if Self = nil then
    Exit(nil);
  Result := TdxEmptyTextFilter.Create(APieceTable);
end;

{ TdxVisibleTextFilter }

function TdxVisibleTextFilter.Clone(APieceTable: TdxCustomPieceTable): IdxVisibleTextFilter;
begin
  if Self = nil then
    Exit(nil);
  Result := TdxVisibleTextFilter.Create(APieceTable);
end;

function TdxVisibleTextFilter.IsRunVisible(const ARunIndex: TdxRunIndex): Boolean;
begin
  Result := (IsRunVisibleCore(ARunIndex) <> TdxRunVisibility.Hidden) or (ARunIndex = PieceTable.Runs.Count - 1);
end;

function TdxVisibleTextFilter.IsRunVisibleCore(const ARunIndex: TdxRunIndex): TdxRunVisibility;
var
  AShouldDisplayHiddenText: Boolean;
begin
  Result := inherited IsRunVisibleCore(ARunIndex);
  if Result <> TdxRunVisibility.Visible then
    Exit;
  AShouldDisplayHiddenText := PieceTable.DocumentModel.FormattingMarkVisibilityOptions.HiddenText = TdxRichEditFormattingMarkVisibility.Visible;
  if (not PieceTable.Runs[ARunIndex].Hidden) or AShouldDisplayHiddenText then
    Result := TdxRunVisibility.Visible
  else
    Result := TdxRunVisibility.Hidden;
end;

{ TdxVisibleOnlyTextFilter }

function TdxVisibleOnlyTextFilter.Clone(APieceTable: TdxCustomPieceTable): IdxVisibleTextFilter;
begin
  if Self = nil then
    Exit(nil);
  Result := TdxVisibleOnlyTextFilter.Create(APieceTable);
end;

function TdxVisibleOnlyTextFilter.IsRunVisible(const ARunIndex: TdxRunIndex): Boolean;
begin
  Result := (IsRunVisibleCore(ARunIndex) <> TdxRunVisibility.Hidden) or
    (ARunIndex = PieceTable.Runs.Count - 1);
end;

function TdxVisibleOnlyTextFilter.IsRunVisibleCore(const ARunIndex: TdxRunIndex): TdxRunVisibility;
begin
  Result := inherited IsRunVisibleCore(ARunIndex);
  if Result <> TdxRunVisibility.Visible then
    Exit;
  if not PieceTable.Runs[ARunIndex].Hidden then
    Result := TdxRunVisibility.Visible
  else
    Result := TdxRunVisibility.Hidden;
end;

end.
