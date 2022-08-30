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

unit dxRichEdit.Dialogs.InsertDeleteTableCellsFormController;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface
uses
  SysUtils, Generics.Defaults, Generics.Collections,
  dxRichEdit.Dialogs.Core,
  dxRichEdit.View.Core,
  dxRichEdit.Utils.Properties,
  dxRichEdit.Utils.Types,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.PieceTable;

type
  { TdxInsertDeleteTableCellsFormControllerParameters }

  TdxInsertDeleteTableCellsFormControllerParameters = class abstract(TdxFormControllerParameters)
  strict private
    FCellsParameters: TdxTableCellsParameters;
  public
    constructor Create(const AControl: IdxRichEditControl; const ACellsParameters: TdxTableCellsParameters);
    property CellsParameters: TdxTableCellsParameters read FCellsParameters;
  end;

  { TdxInsertTableCellsFormControllerParameters }

  TdxInsertTableCellsFormControllerParameters = class(TdxInsertDeleteTableCellsFormControllerParameters);

  { TdxDeleteTableCellsFormControllerParameters }

  TdxDeleteTableCellsFormControllerParameters = class(TdxInsertDeleteTableCellsFormControllerParameters);

  { TdxInsertDeleteTableCellsFormController }

  TdxInsertDeleteTableCellsFormController = class abstract(TdxFormController)
  strict private
    FSourceParameters: TdxTableCellsParameters;
    FCellOperations: TdxTableCellOperation;
  protected
    procedure InitializeController; virtual;
  public
    constructor Create(AControllerParameters: TdxInsertDeleteTableCellsFormControllerParameters);
    procedure ApplyChanges; override;

    property SourceParameters: TdxTableCellsParameters read FSourceParameters;
    property CellOperation: TdxTableCellOperation read FCellOperations write FCellOperations;
  end;

  { TdxInsertTableCellsFormController }

  TdxInsertTableCellsFormController = class(TdxInsertDeleteTableCellsFormController);

  { TdxDeleteTableCellsFormController }

  TdxDeleteTableCellsFormController = class(TdxInsertDeleteTableCellsFormController);

implementation

{ TdxInsertDeleteTableCellsFormControllerParameters }

constructor TdxInsertDeleteTableCellsFormControllerParameters.Create(const AControl: IdxRichEditControl;
  const ACellsParameters: TdxTableCellsParameters);
begin
  inherited Create(AControl);
  FCellsParameters := ACellsParameters;
end;

{ TdxInsertDeleteTableCellsFormController }

constructor TdxInsertDeleteTableCellsFormController.Create(AControllerParameters: TdxInsertDeleteTableCellsFormControllerParameters);
begin
  inherited Create;
  FSourceParameters := AControllerParameters.CellsParameters;
  InitializeController;
end;

procedure TdxInsertDeleteTableCellsFormController.InitializeController;
begin
  CellOperation := FSourceParameters.CellOperation;
end;

procedure TdxInsertDeleteTableCellsFormController.ApplyChanges;
begin
  FSourceParameters.CellOperation := CellOperation;
end;

end.
