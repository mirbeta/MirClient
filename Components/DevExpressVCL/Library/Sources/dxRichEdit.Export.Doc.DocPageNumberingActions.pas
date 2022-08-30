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
unit dxRichEdit.Export.Doc.DocPageNumberingActions;

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

  { TdxDocPageNumberingActions }

  TdxDocPageNumberingActions = class
  public const
    DefaultPageNumber = Integer(-1);
  strict private
    FWriter: TBinaryWriter;
    FPageNumbering: TdxSectionPageNumbering;
  public
    constructor Create(AOutput: TdxMemoryStream; APageNumbering: TdxSectionPageNumbering);
    destructor Destroy; override;
    procedure ChapterHeaderStyleAction;
    procedure ChapterSeparatorAction;
    procedure NumberingFormatAction;
    procedure FirstPageNumberAction;
  end;

implementation

uses
  Math, Contnrs,
  dxRichEdit.Import.Doc.DocCommand;

{ TdxDocPageNumberingActions }

constructor TdxDocPageNumberingActions.Create(AOutput: TdxMemoryStream; APageNumbering: TdxSectionPageNumbering);
begin
  FWriter := TBinaryWriter.Create(AOutput);
  FPageNumbering := APageNumbering;
end;

destructor TdxDocPageNumberingActions.Destroy;
begin
  FWriter.Free;
  inherited Destroy;
end;

procedure TdxDocPageNumberingActions.ChapterHeaderStyleAction;
var
  ACommand: TdxDocCommandChapterHeaderStyle;
begin
  ACommand := TdxDocCommandChapterHeaderStyle.Create;
  try
    ACommand.ChapterHeaderStyle := FPageNumbering.ChapterHeaderStyle;
    ACommand.Write(FWriter);
  finally
    ACommand.Free;
  end;
end;

procedure TdxDocPageNumberingActions.ChapterSeparatorAction;
var
  ACommand: TdxDocCommandChapterSeparator;
begin
  ACommand := TdxDocCommandChapterSeparator.Create;
  try
    ACommand.ChapterSeparator := FPageNumbering.ChapterSeparator;
    ACommand.Write(FWriter);
  finally
    ACommand.Free;
  end;
end;

procedure TdxDocPageNumberingActions.NumberingFormatAction;
var
  ACommand: TdxDocCommandNumberingFormat;
begin
  ACommand := TdxDocCommandNumberingFormat.Create;
  try
    ACommand.NumberingFormat := FPageNumbering.NumberingFormat;
    ACommand.Write(FWriter);
  finally
    ACommand.Free;
  end;
end;

procedure TdxDocPageNumberingActions.FirstPageNumberAction;
var
  AUsePageNumberingCommand: TdxDocCommandUseStartingPageNumber;
  AStartingPageNumberCommand: TdxDocCommandStartingPageNumber;
begin
  if FPageNumbering.ContinueNumbering then
    Exit;
  AUsePageNumberingCommand := TdxDocCommandUseStartingPageNumber.Create;
  try
    AUsePageNumberingCommand.Value := True;
    AUsePageNumberingCommand.Write(FWriter);
  finally
    AUsePageNumberingCommand.Free;
  end;
  AStartingPageNumberCommand := TdxDocCommandStartingPageNumber.Create;
  try
    AStartingPageNumberCommand.Value := FPageNumbering.FirstPageNumber;
    AStartingPageNumberCommand.Write(FWriter);
  finally
    AStartingPageNumberCommand.Free;
  end;
end;

end.
