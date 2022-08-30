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
unit dxRichEdit.Export.Doc.DocPageActions;

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

  { TdxDocPageActions }

  TdxDocPageActions = class
  strict private
    FWriter: TBinaryWriter;
    FPage: TdxSectionPage;
  public
    constructor Create(AOutput: TdxMemoryStream; APage: TdxSectionPage);
    destructor Destroy; override;
    procedure HeightAction;
    procedure LandscapeAction;
    procedure PaperKindAction;
    procedure WidthAction;
  end;

implementation

uses
  Math, Contnrs,
  dxRichEdit.Import.Doc.DocCommand;

{ TdxDocPageActions }

constructor TdxDocPageActions.Create(AOutput: TdxMemoryStream; APage: TdxSectionPage);
begin
  FWriter := TBinaryWriter.Create(AOutput);
  FPage := APage;
end;

destructor TdxDocPageActions.Destroy;
begin
  FWriter.Free;
  inherited Destroy;
end;

procedure TdxDocPageActions.HeightAction;
var
  ACommand: TdxDocCommandPageHeight;
begin
  ACommand := TdxDocCommandPageHeight.Create;
  try
    ACommand.Value := FPage.Height;
    ACommand.Write(FWriter);
  finally
    ACommand.Free;
  end;
end;

procedure TdxDocPageActions.LandscapeAction;
var
  ACommand: TdxDocCommandPageOrientation;
begin
  ACommand := TdxDocCommandPageOrientation.Create;
  try
    ACommand.Landscape := FPage.Landscape;
    ACommand.Write(FWriter);
  finally
    ACommand.Free;
  end;
end;

procedure TdxDocPageActions.PaperKindAction;
begin
end;

procedure TdxDocPageActions.WidthAction;
var
  ACommand: TdxDocCommandPageWidth;
begin
  ACommand := TdxDocCommandPageWidth.Create;
  try
    ACommand.Value := FPage.Width;
    ACommand.Write(FWriter);
  finally
    ACommand.Free;
  end;
end;

end.
