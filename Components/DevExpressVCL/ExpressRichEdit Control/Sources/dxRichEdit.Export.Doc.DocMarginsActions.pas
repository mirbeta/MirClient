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
unit dxRichEdit.Export.Doc.DocMarginsActions;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, Classes, Generics.Defaults, Generics.Collections, dxCore, dxCoreClasses,
  dxCoreGraphics, dxGenerics,
  dxRichEdit.Utils.Types,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.SectionFormatting,
  dxRichEdit.DocumentModel.Section;

type

  { TdxDocMarginsActions }

  TdxDocMarginsActions = class
  strict private
    FWriter: TBinaryWriter;
    FMargins: TdxSectionMargins;
  public
    constructor Create(AOutput: TdxMemoryStream; AMargins: TdxSectionMargins);
    destructor Destroy; override;
    procedure LeftAction;
    procedure RightAction;
    procedure TopAction;
    procedure BottomAction;
    procedure GutterAction;
    procedure GutterAlignmentAction;
    procedure HeaderOffsetAction;
    procedure FooterOffsetAction;
  end;

implementation

uses
  Math, Contnrs,
  dxRichEdit.Import.Doc.DocCommand;

{ TdxDocMarginsActions }

constructor TdxDocMarginsActions.Create(AOutput: TdxMemoryStream; AMargins: TdxSectionMargins);
begin
  FWriter := TBinaryWriter.Create(AOutput);
  FMargins := AMargins;
end;

destructor TdxDocMarginsActions.Destroy;
begin
  FWriter.Free;
  inherited Destroy;
end;

procedure TdxDocMarginsActions.LeftAction;
var
  ACommand: TdxDocCommandLeftMargin;
begin
  ACommand := TdxDocCommandLeftMargin.Create;
  try
    ACommand.Value := FMargins.Left;
    ACommand.Write(FWriter);
  finally
    ACommand.Free;
  end;
end;

procedure TdxDocMarginsActions.RightAction;
var
  ACommand: TdxDocCommandRightMargin;
begin
  ACommand := TdxDocCommandRightMargin.Create;
  try
    ACommand.Value := FMargins.Right;
    ACommand.Write(FWriter);
  finally
    ACommand.Free;
  end;
end;

procedure TdxDocMarginsActions.TopAction;
var
  ACommand: TdxDocCommandTopMargin;
begin
  ACommand := TdxDocCommandTopMargin.Create;
  try
    ACommand.Value := FMargins.Top;
    ACommand.Write(FWriter);
  finally
    ACommand.Free;
  end;
end;

procedure TdxDocMarginsActions.BottomAction;
var
  ACommand: TdxDocCommandBottomMargin;
begin
  ACommand := TdxDocCommandBottomMargin.Create;
  try
    ACommand.Value := FMargins.Bottom;
    ACommand.Write(FWriter);
  finally
    ACommand.Free;
  end;
end;

procedure TdxDocMarginsActions.GutterAction;
var
  ACommand: TdxDocCommandGutter;
begin
  ACommand := TdxDocCommandGutter.Create;
  try
    ACommand.Value := FMargins.DocumentModel.UnitConverter.ModelUnitsToTwips(FMargins.Gutter);
    ACommand.Write(FWriter);
  finally
    ACommand.Free;
  end;
end;

procedure TdxDocMarginsActions.GutterAlignmentAction;
var
  ACommand: TdxDocCommandRTLGutter;
begin
  if FMargins.GutterAlignment in [TdxSectionGutterAlignment.Top, TdxSectionGutterAlignment.Bottom] then
    Exit;
  ACommand := TdxDocCommandRTLGutter.Create;
  try
    ACommand.GutterAlignment := FMargins.GutterAlignment;
    ACommand.Write(FWriter);
  finally
    ACommand.Free;
  end;
end;

procedure TdxDocMarginsActions.HeaderOffsetAction;
var
  ACommand: TdxDocCommandHeaderOffset;
begin
  ACommand := TdxDocCommandHeaderOffset.Create;
  try
    ACommand.Value := FMargins.HeaderOffset;
    ACommand.Write(FWriter);
  finally
    ACommand.Free;
  end;
end;

procedure TdxDocMarginsActions.FooterOffsetAction;
var
  ACommand: TdxDocCommandFooterOffset;
begin
  ACommand := TdxDocCommandFooterOffset.Create;
  try
    ACommand.Value := FMargins.FooterOffset;
    ACommand.Write(FWriter);
  finally
    ACommand.Free;
  end;
end;

end.
