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
unit dxRichEdit.Export.Doc.DocLineNumberingActions;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, Classes, Generics.Defaults, Generics.Collections, dxCore, dxCoreClasses,
  dxCoreGraphics, dxGenerics,
  dxRichEdit.Utils.Types,
  dxRichEdit.DocumentModel.SectionFormatting,
  dxRichEdit.DocumentModel.PieceTable;

type

  { TdxDocLineNumberingActions }

  TdxDocLineNumberingActions = class
  strict private
    FWriter: TBinaryWriter;
    FLineNumbering: TdxSectionLineNumbering;
  public
    constructor Create(AOutput: TdxMemoryStream; ALineNumbering: TdxSectionLineNumbering);
    destructor Destroy; override;
    procedure DistanceAction;
    procedure NumberingRestartTypeAction;
    procedure StartingLineNumberAction;
    procedure StepAction;
  end;

implementation

uses
  Math, Contnrs,
  dxRichEdit.Import.Doc.DocCommand;


{ TdxDocLineNumberingActions }

constructor TdxDocLineNumberingActions.Create(AOutput: TdxMemoryStream; ALineNumbering: TdxSectionLineNumbering);
begin
  FWriter := TBinaryWriter.Create(AOutput);
  FLineNumbering := ALineNumbering;
end;

destructor TdxDocLineNumberingActions.Destroy;
begin
  FWriter.Free;
  inherited Destroy;
end;

procedure TdxDocLineNumberingActions.DistanceAction;
var
  ACommand: TdxDocCommandLineNumberingDistance;
begin
  ACommand := TdxDocCommandLineNumberingDistance.Create;
  try
    ACommand.Value := FLineNumbering.Distance;
    ACommand.Write(FWriter);
  finally
    ACommand.Free;
  end;
end;

procedure TdxDocLineNumberingActions.NumberingRestartTypeAction;
var
  ACommand: TdxDocCommandNumberingRestartType;
begin
  ACommand := TdxDocCommandNumberingRestartType.Create;
  try
    ACommand.NumberingRestartType := FLineNumbering.NumberingRestartType;
    ACommand.Write(FWriter);
  finally
    ACommand.Free;
  end;
end;

procedure TdxDocLineNumberingActions.StartingLineNumberAction;
var
  ACommand: TdxDocCommandStartLineNumber;
begin
  ACommand := TdxDocCommandStartLineNumber.Create;
  try
    ACommand.Value := FLineNumbering.StartingLineNumber;
    ACommand.Write(FWriter);
  finally
    ACommand.Free;
  end;
end;

procedure TdxDocLineNumberingActions.StepAction;
var
  ACommand: TdxDocCommandStep;
begin
  ACommand := TdxDocCommandStep.Create;
  try
    ACommand.Value := FLineNumbering.Step;
    ACommand.Write(FWriter);
  finally
    ACommand.Free;
  end;
end;

end.
