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

unit dxRichEdit.Dialogs.SplitTableCellsFormController;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, Generics.Defaults, Generics.Collections,
  dxRichEdit.Dialogs.Core,
  dxRichEdit.View.Core,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.Utils.Properties,
  dxRichEdit.Utils.Types,
  dxGenerics;

type

  { TdxSplitTableCellsFormControllerParameters }

  TdxSplitTableCellsFormControllerParameters = class(TdxFormControllerParameters)
  strict private
    FParameters: TdxSplitTableCellsParameters;
  public
    constructor Create(const AControl: IdxRichEditControl; const AParameters: TdxSplitTableCellsParameters);
    property Parameters: TdxSplitTableCellsParameters read FParameters;
  end;

  { TdxSplitTableCellsFormController }

  TdxSplitTableCellsFormController = class(TdxFormController)
  strict private
    FMaxRowsCount: Integer;
    FMaxColumnsCount: Integer;
    FSourceParameters: TdxSplitTableCellsParameters;
    FColumnsCount: Integer;
    FRowsCount: Integer;
    FMergeCellsBeforeSplit: Boolean;
    FAllowedRowsCount: TdxIntegerList;
    function GetRowsCountAfterMerge: Integer;
    function GetSourceRowsCount: Integer;
  protected
    procedure InitializeController;
    function CalculateAllowedRowsCount: TdxIntegerList;
  public
    constructor Create(AControllerParameters: TdxSplitTableCellsFormControllerParameters);
    destructor Destroy; override;
    procedure ApplyChanges; override;

    property SourceParameters: TdxSplitTableCellsParameters read FSourceParameters;
    property ColumnsCount: Integer read FColumnsCount write FColumnsCount;
    property RowsCount: Integer read FRowsCount write FRowsCount;
    property MergeCellsBeforeSplit: Boolean read FMergeCellsBeforeSplit write FMergeCellsBeforeSplit;
    property RowsCountAfterMerge: Integer read GetRowsCountAfterMerge;
    property SourceRowsCount: Integer read GetSourceRowsCount;
    property AllowedRowsCount: TdxIntegerList read FAllowedRowsCount;
    property MaxRowsCount: Integer read FMaxRowsCount;
    property MaxColumnsCount: Integer read FMaxColumnsCount;
  end;

implementation

{ TdxSplitTableCellsFormControllerParameters }

constructor TdxSplitTableCellsFormControllerParameters.Create(const AControl: IdxRichEditControl;
  const AParameters: TdxSplitTableCellsParameters);
begin
  inherited Create(AControl);
  FParameters := AParameters;
end;

{ TdxSplitTableCellsFormController }

constructor TdxSplitTableCellsFormController.Create(AControllerParameters: TdxSplitTableCellsFormControllerParameters);
begin
  inherited Create;
  FMaxRowsCount := 15;
  FMaxColumnsCount := 63;
  FSourceParameters := AControllerParameters.Parameters;
  InitializeController;
end;

destructor TdxSplitTableCellsFormController.Destroy;
begin
  FreeAndNil(FAllowedRowsCount);
  inherited Destroy;
end;

function TdxSplitTableCellsFormController.GetRowsCountAfterMerge: Integer;
begin
  Result := SourceParameters.RowCountAfterMerge;
end;

function TdxSplitTableCellsFormController.GetSourceRowsCount: Integer;
begin
  Result := SourceParameters.RowsCount;
end;

procedure TdxSplitTableCellsFormController.InitializeController;
begin
  ColumnsCount := FSourceParameters.ColumnsCount;
  RowsCount := FSourceParameters.RowsCount;
  MergeCellsBeforeSplit := FSourceParameters.MergeCellsBeforeSplit;

  FAllowedRowsCount := CalculateAllowedRowsCount;
end;

function TdxSplitTableCellsFormController.CalculateAllowedRowsCount: TdxIntegerList;
var
  I: Integer;
begin
  if RowsCountAfterMerge <= 1 then
    Exit(nil);

  Result := TdxIntegerList.Create;
  Result.Add(1);
  for I := 2 to RowsCountAfterMerge do
  begin
    if RowsCountAfterMerge mod I = 0 then
      Result.Add(I);
  end;
end;

procedure TdxSplitTableCellsFormController.ApplyChanges;
begin
  FSourceParameters.ColumnsCount := ColumnsCount;
  FSourceParameters.RowsCount := RowsCount;
  FSourceParameters.MergeCellsBeforeSplit := MergeCellsBeforeSplit;
end;

end.
