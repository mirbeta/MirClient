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
unit dxRichEdit.Export.Doc.FKPWriter;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, Classes, Generics.Defaults, Generics.Collections, dxCore, dxCoreClasses,
  dxCoreGraphics, dxGenerics,
  dxRichEdit.Utils.Types,
  dxRichEdit.Import.Doc.FormattedDiskPage,
  dxRichEdit.Import.Doc.BinTable;

type

  { TdxFKPWriter }

  TdxFKPWriter = class
  strict private
    FPropertiesWriter: TBinaryWriter;
    FDataStreamWriter: TBinaryWriter;
    FCurrentCharacterFKP: TdxCHPXFormattedDiskPage;
    FCurrentParagraphFKP: TdxPAPXFormattedDiskPage;
    FCharactersBinTable: TdxBinTable;
    FParagraphsBinTable: TdxBinTable;
    FTextStartPosition: Integer;
  protected
    function GetFilePositionByCharacterPosition(ACharacterPosition: Integer): Integer;
    procedure UpdateBinTables(AOffset: Integer);

    property PropertiesWriter: TBinaryWriter read FPropertiesWriter;
    property DataStreamWriter: TBinaryWriter read FDataStreamWriter;
  public
    constructor Create(ATextStartPosition: Integer; APropertiesStream: TdxMemoryStream; ADataStreamWriter: TBinaryWriter);
    destructor Destroy; override;
    procedure WriteTextRun(ACharacterPosition: Integer; const APropertyModifiers: TBytes); overload;
    procedure WriteTextRun(ACharacterPosition: Integer); overload;
    procedure WriteTextRunBase(ACharacterPosition: Integer; const AGrpprl: TBytes);
    procedure WriteParagraph(ACharacterPosition: Integer; AParagraphStyleIndex: Integer); overload;
    procedure WriteParagraph(ACharacterPosition: Integer; AParagraphStyleIndex: Integer; const APropertyModifiers: TBytes); overload;
    procedure WriteParagraphBase(ACharacterPosition: Integer; AParagraphStyleIndex: Integer; const AGrpprl: TBytes);
    procedure WriteParagarphGrpprlAndPosition(AFilePosition: Integer; AParagraphStyleIndex: Integer; const AGrpprl: TBytes);
    procedure CreateHugeParagraphFKP(AFilePosition: Integer; AStyleIndex: Integer; const AGrpprl: TBytes);
    procedure WriteTableProperties(ACharacterPosition: Integer; AStyleIndex: Integer; const AGrpprl: TBytes);
    procedure WriteCurrentCharacterFKP(ALastPosition: Integer);
    procedure WriteCurrentParagraphFKP(ALastFKPPosition: Integer);
    procedure Finish(ALastCharacterPosition: Integer);

    property TextStartPosition: Integer read FTextStartPosition;
    property CharactersBinTable: TdxBinTable read FCharactersBinTable;
    property ParagraphsBinTable: TdxBinTable read FParagraphsBinTable;
  end;

implementation

uses
  Math, Contnrs,
  dxRichEdit.Utils.Exceptions,
  dxRichEdit.Doc.Utils,
  dxRichEdit.Import.Doc.DocCommand,
  dxRichEdit.Import.Doc.DocCommandHelper,
  dxRichEdit.Import.Doc.DocContentBuilder;

{ TdxFKPWriter }

constructor TdxFKPWriter.Create(ATextStartPosition: Integer; APropertiesStream: TdxMemoryStream; ADataStreamWriter: TBinaryWriter);
begin
  Assert(APropertiesStream <> nil, 'propertiesStream');
  Assert(ADataStreamWriter <> nil, 'dataStreamWriter');
  FTextStartPosition := ATextStartPosition;
  FDataStreamWriter := ADataStreamWriter;

  FPropertiesWriter := TBinaryWriter.Create(APropertiesStream);
  FCharactersBinTable := TdxBinTable.Create;
  FParagraphsBinTable := TdxBinTable.Create;
  FCurrentCharacterFKP := TdxCHPXFormattedDiskPage.Create;
  FCurrentParagraphFKP := TdxPAPXFormattedDiskPage.Create;
end;

destructor TdxFKPWriter.Destroy;
begin
  FCharactersBinTable.Free;
  FParagraphsBinTable.Free;
  FCurrentParagraphFKP.Free;
  FCurrentCharacterFKP.Free;
  FPropertiesWriter.Free;
  inherited Destroy;
end;

procedure TdxFKPWriter.WriteTextRun(ACharacterPosition: Integer; const APropertyModifiers: TBytes);
begin
  WriteTextRunBase(ACharacterPosition, APropertyModifiers);
end;

procedure TdxFKPWriter.WriteTextRun(ACharacterPosition: Integer);
begin
  WriteTextRunBase(ACharacterPosition, TBytes.Create());
end;

procedure TdxFKPWriter.WriteTextRunBase(ACharacterPosition: Integer; const AGrpprl: TBytes);
var
  AFilePosition: Integer;
begin
  AFilePosition := GetFilePositionByCharacterPosition(ACharacterPosition);
  if FCurrentCharacterFKP.TryToAddGrpprlAndPosition(AFilePosition, AGrpprl) then
    Exit;
  WriteCurrentCharacterFKP(AFilePosition);

  FCurrentCharacterFKP.Free;
  FCurrentCharacterFKP := TdxCHPXFormattedDiskPage.Create;
  if FCurrentCharacterFKP.TryToAddGrpprlAndPosition(AFilePosition, AGrpprl) then
    Exit;
  TdxRichEditExceptions.ThrowInternalException;
end;

procedure TdxFKPWriter.WriteParagraph(ACharacterPosition: Integer; AParagraphStyleIndex: Integer);
begin
  WriteParagraphBase(ACharacterPosition, AParagraphStyleIndex, TBytes.Create());
end;

procedure TdxFKPWriter.WriteParagraph(ACharacterPosition: Integer; AParagraphStyleIndex: Integer; const APropertyModifiers: TBytes);
begin
  WriteParagraphBase(ACharacterPosition, AParagraphStyleIndex, APropertyModifiers);
end;

procedure TdxFKPWriter.WriteParagraphBase(ACharacterPosition: Integer; AParagraphStyleIndex: Integer; const AGrpprl: TBytes);
var
  AFilePosition: Integer;
begin
  AFilePosition := GetFilePositionByCharacterPosition(ACharacterPosition);
  WriteParagarphGrpprlAndPosition(AFilePosition, AParagraphStyleIndex, AGrpprl);
end;

procedure TdxFKPWriter.WriteParagarphGrpprlAndPosition(AFilePosition: Integer; AParagraphStyleIndex: Integer; const AGrpprl: TBytes);
begin
  if FCurrentParagraphFKP.TryToAddGrpprlAndPosition(AFilePosition, AParagraphStyleIndex, AGrpprl) then
    Exit;
  WriteCurrentParagraphFKP(AFilePosition);

  FCurrentParagraphFKP.Free;
  FCurrentParagraphFKP := TdxPAPXFormattedDiskPage.Create;
  if FCurrentParagraphFKP.TryToAddGrpprlAndPosition(AFilePosition, AParagraphStyleIndex, AGrpprl) then
    Exit;
  CreateHugeParagraphFKP(AFilePosition, AParagraphStyleIndex, AGrpprl);
end;

procedure TdxFKPWriter.CreateHugeParagraphFKP(AFilePosition: Integer; AStyleIndex: Integer; const AGrpprl: TBytes);
var
  AOpcode: SmallInt;
  AOperand, AExtendedPropertyModifiersData: TBytes;
begin
  AOpcode := TdxDocCommandFactory.GetOpcodeByType(TdxDocCommandReadExtendedPropertyModifiers);
  AOperand := TdxByteArrayHelper.From<SmallInt>(SmallInt(DataStreamWriter.BaseStream.Position));
  AExtendedPropertyModifiersData := TdxDocCommandHelper.CreateSinglePropertyModifier(AOpcode, AOperand);
  if not FCurrentParagraphFKP.TryToAddGrpprlAndPosition(AFilePosition, AStyleIndex, AExtendedPropertyModifiersData) then
    TdxRichEditExceptions.ThrowInternalException;
  DataStreamWriter.Write(SmallInt(Length(AGrpprl)));
  DataStreamWriter.Write(AGrpprl);
end;

procedure TdxFKPWriter.WriteTableProperties(ACharacterPosition: Integer; AStyleIndex: Integer; const AGrpprl: TBytes);
var
  AFilePosition: Integer;
  AOpcode: SmallInt;
  AOperand, ATablePropertyModifiers: TBytes;
begin
  AFilePosition := GetFilePositionByCharacterPosition(ACharacterPosition);
  AOpcode := TdxDocCommandFactory.GetOpcodeByType(TdxDocCommandReadTableProperties);
  AOperand := TdxByteArrayHelper.From<Integer>(Integer(DataStreamWriter.BaseStream.Position));
  ATablePropertyModifiers := TdxDocCommandHelper.CreateSinglePropertyModifier(AOpcode, AOperand);
  DataStreamWriter.Write(SmallInt(Length(AGrpprl)));
  DataStreamWriter.Write(AGrpprl);
  WriteParagarphGrpprlAndPosition(AFilePosition, AStyleIndex, ATablePropertyModifiers);
end;

procedure TdxFKPWriter.WriteCurrentCharacterFKP(ALastPosition: Integer);
var
  AFkpOffset, AFcFirst: Integer;
begin
  AFkpOffset := Integer(PropertiesWriter.BaseStream.Position);
  AFcFirst := FCurrentCharacterFKP.GetFirstOffset;
  FCurrentCharacterFKP.AddLastPosition(ALastPosition);
  FCurrentCharacterFKP.Write(PropertiesWriter);
  CharactersBinTable.AddEntry(AFcFirst, AFkpOffset);
end;

procedure TdxFKPWriter.WriteCurrentParagraphFKP(ALastFKPPosition: Integer);
var
  AFkpOffset, AFcFirst: Integer;
begin
  AFkpOffset := Integer(PropertiesWriter.BaseStream.Position);
  AFcFirst := FCurrentParagraphFKP.GetFirstOffset;
  FCurrentParagraphFKP.AddLastPosition(ALastFKPPosition);
  FCurrentParagraphFKP.Write(PropertiesWriter);
  ParagraphsBinTable.AddEntry(AFcFirst, AFkpOffset);
end;

procedure TdxFKPWriter.Finish(ALastCharacterPosition: Integer);
var
  ALastFilePosition, AOffset: Integer;
begin
  ALastFilePosition := GetFilePositionByCharacterPosition(ALastCharacterPosition);
  WriteCurrentCharacterFKP(ALastFilePosition);
  WriteCurrentParagraphFKP(ALastFilePosition);
  CharactersBinTable.AddLastPosition(ALastFilePosition);
  ParagraphsBinTable.AddLastPosition(ALastFilePosition);
  if (ALastFilePosition mod TdxDocContentBuilder.SectorSize = 0) then
    AOffset := ALastFilePosition div TdxDocContentBuilder.SectorSize
  else
    AOffset := ALastFilePosition div TdxDocContentBuilder.SectorSize + 1;
  UpdateBinTables(AOffset);
end;

function TdxFKPWriter.GetFilePositionByCharacterPosition(ACharacterPosition: Integer): Integer;
begin
  Result := FTextStartPosition + ACharacterPosition * 2;
end;

procedure TdxFKPWriter.UpdateBinTables(AOffset: Integer);
begin
  CharactersBinTable.UpdateSectorsOffsets(AOffset);
  ParagraphsBinTable.UpdateSectorsOffsets(AOffset);
end;

end.
