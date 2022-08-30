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
unit dxRichEdit.Export.Doc.DocFootNoteActions;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, Classes, Generics.Defaults, Generics.Collections, dxCore, dxCoreClasses,
  dxCoreGraphics, dxGenerics,
  dxRichEdit.Utils.Types,
  dxRichEdit.DocumentModel.SectionFormatting,
  dxRichEdit.DocumentModel.Section,
  dxRichEdit.DocumentModel.PieceTable;

type

  { TdxDocFootNoteActions }

  TdxDocFootNoteActions = class
  strict private
    FWriter: TBinaryWriter;
    FFootNote: TdxSectionFootNote;
    FIsFootNote: Boolean;
  protected
    property IsFootNote: Boolean read FIsFootNote write FIsFootNote;
    property FootNote: TdxSectionFootNote read FFootNote write FFootNote;
  public
    constructor Create(AOutput: TdxMemoryStream; AFootNote: TdxSectionFootNote; AIsFootNote: Boolean);
    destructor Destroy; override;
    procedure NumberingFormatAction;
    procedure NumberingRestartTypeAction;
    procedure PositionAction;
    procedure StartingNumberAction;
  end;

implementation

uses
  Math, Contnrs,
  dxRichEdit.Import.Doc.DocCommand;

{ TdxDocFootNoteActions }

constructor TdxDocFootNoteActions.Create(AOutput: TdxMemoryStream; AFootNote: TdxSectionFootNote; AIsFootNote: Boolean);
begin
  FWriter := TBinaryWriter.Create(AOutput);
  FFootNote := AFootNote;
  FIsFootNote := AIsFootNote;
end;

destructor TdxDocFootNoteActions.Destroy;
begin
  FWriter.Free;
  inherited Destroy;
end;

procedure TdxDocFootNoteActions.NumberingFormatAction;
var
  ACommand: TdxDocCommandFootNoteNumberingFormatBase;
begin
  if FIsFootNote then
    ACommand := TdxDocCommandFootNoteNumberingFormat.Create
  else
    ACommand := TdxDocCommandEndNoteNumberingFormat.Create;
  try
    ACommand.NumberingFormat := FFootNote.NumberingFormat;
    ACommand.Write(FWriter);
  finally
    ACommand.Free;
  end;
end;

procedure TdxDocFootNoteActions.NumberingRestartTypeAction;
var
  ACommand: TdxDocCommandFootNoteNumberingRestartTypeBase;
begin
  if FIsFootNote then
    ACommand := TdxDocCommandFootNoteNumberingRestartType.Create
  else
    ACommand := TdxDocCommandEndNoteNumberingRestartType.Create;
  try
    ACommand.NumberingRestartType := FFootNote.NumberingRestartType;
    ACommand.Write(FWriter);
  finally
    ACommand.Free;
  end;
end;

procedure TdxDocFootNoteActions.PositionAction;
begin
end;

procedure TdxDocFootNoteActions.StartingNumberAction;
var
  ACommand: TdxDocCommandShortPropertyValueBase;
begin
  if FIsFootNote then
    ACommand := TdxDocCommandFootNoteStartingNumber.Create
  else
    ACommand := TdxDocCommandEndNoteStartingNumber.Create;
  try
    ACommand.Value := FFootNote.StartingNumber;
    ACommand.Write(FWriter);
  finally
    ACommand.Free;
  end;
end;

end.
