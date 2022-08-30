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
unit dxRichEdit.Export.Doc.DocColumnsActions;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, Classes, Generics.Defaults, Generics.Collections, dxCore, dxCoreClasses,
  dxCoreGraphics, dxGenerics,
  dxRichEdit.Utils.Types,
  dxRichEdit.DocumentModel.SectionFormatting,
  dxRichEdit.DocumentModel.Section;

type

  { TdxDocColumnsActions }

  TdxDocColumnsActions = class
  strict private
    FWriter: TBinaryWriter;
    FColumns: TdxSectionColumns;
  public
    constructor Create(AOutput: TdxMemoryStream; AColumns: TdxSectionColumns);
    destructor Destroy; override;
    procedure ColumnCountAction;
    procedure ColumnsAction;
    procedure DrawVerticalSeparatorAction;
    procedure EqualWidthColumnsAction;
    procedure SpaceAction;
  end;

implementation

uses
  Math, Contnrs,
  dxRichEdit.Import.Doc.DocCommand;

{ TdxDocColumnsActions }

constructor TdxDocColumnsActions.Create(AOutput: TdxMemoryStream; AColumns: TdxSectionColumns);
begin
  FWriter := TBinaryWriter.Create(AOutput);
  FColumns := AColumns;
end;

destructor TdxDocColumnsActions.Destroy;
begin
  FWriter.Free;
  inherited Destroy;
end;

procedure TdxDocColumnsActions.ColumnCountAction;
var
  ACommand: TdxDocCommandColumnCount;
begin
  ACommand := TdxDocCommandColumnCount.Create;
  try
    ACommand.Value := FColumns.ColumnCount;
    ACommand.Write(FWriter);
  finally
    ACommand.Free;
  end;
end;

procedure TdxDocColumnsActions.ColumnsAction;
var
  ACount, I: Integer;
  AInfo: TdxColumnInfo;
  ACommandWidth: TdxDocCommandColumnWidth;
  ACommandSpace: TdxDocCommandNotEvenlyColumnsSpace;
begin
  if FColumns.EqualWidthColumns then
    Exit;
  ACount := FColumns.Info.Columns.Count;
  for I := 0 to ACount - 1 do
  begin
    AInfo := FColumns.Info.Columns[I];
    ACommandWidth := TdxDocCommandColumnWidth.Create;
    try
      ACommandWidth.ColumnIndex := I;
      ACommandWidth.ColumnWidth := AInfo.Width;
      ACommandWidth.Write(FWriter);
    finally
      ACommandWidth.Free;
    end;
    ACommandSpace := TdxDocCommandNotEvenlyColumnsSpace.Create;
    try
      ACommandSpace.ColumnIndex := I;
      ACommandSpace.ColumnSpace := AInfo.Space;
      ACommandSpace.Write(FWriter);
    finally
      ACommandSpace.Free;
    end;
  end;
end;

procedure TdxDocColumnsActions.DrawVerticalSeparatorAction;
var
  ACommand: TdxDocCommandDrawVerticalSeparator;
begin
  ACommand := TdxDocCommandDrawVerticalSeparator.Create;
  try
    ACommand.Value := FColumns.DrawVerticalSeparator;
    ACommand.Write(FWriter);
  finally
    ACommand.Free;
  end;
end;

procedure TdxDocColumnsActions.EqualWidthColumnsAction;
var
  ACommand: TdxDocCommandEqualWidthColumns;
begin
  ACommand := TdxDocCommandEqualWidthColumns.Create;
  try
    ACommand.Value := FColumns.EqualWidthColumns;
    ACommand.Write(FWriter);
  finally
    ACommand.Free;
  end;
end;

procedure TdxDocColumnsActions.SpaceAction;
var
  ACommand: TdxDocCommandColumnSpace;
begin
  ACommand := TdxDocCommandColumnSpace.Create;
  try
    ACommand.Value := FColumns.Space;
    ACommand.Write(FWriter);
  finally
    ACommand.Free;
  end;
end;

end.
