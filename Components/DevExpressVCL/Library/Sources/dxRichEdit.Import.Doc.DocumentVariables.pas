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
unit dxRichEdit.Import.Doc.DocumentVariables;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, Classes, Generics.Defaults, Generics.Collections, dxCoreClasses, dxGenerics,
  dxRichEdit.DocumentModel.Fields,
  dxRichEdit.DocumentModel.DocumentProperties;

type

  { TdxDocumentVariables }

  TdxDocumentVariables = class
  public const
    ExtraDataSize = 4;
  strict private
    FNames: TdxStringList;
    FValues: TdxStringList;
  protected
    procedure Read(AReader: TBinaryReader; AOffset: Integer; ASize: Integer);
    procedure GetVariables(AVariables: TdxDocumentVariableCollection; ADocumentProperties: TdxDocumentProperties);
  public
    constructor Create;
    destructor Destroy; override;
    class function FromStream(AReader: TBinaryReader; AOffset: Integer; ASize: Integer): TdxDocumentVariables; static;
    class function FromVariablesCollection(AVariables: TdxDocumentVariableCollection; ADocumentProperties: TdxDocumentProperties): TdxDocumentVariables; static;
    procedure Write(AWriter: TBinaryWriter);
    procedure SetVariables(AVariables: TdxDocumentVariableCollection; ADocumentProperties: TdxDocumentProperties);

    property Names: TdxStringList read FNames;
    property Values: TdxStringList read FValues;
  end;

implementation

uses
  RTTI, dxRichEdit.Import.Doc.DocStringTable, dxEncoding;

{ TdxDocumentVariables }

constructor TdxDocumentVariables.Create;
begin
  FValues := TdxStringList.Create;
end;

destructor TdxDocumentVariables.Destroy;
begin
  FNames.Free;
  FValues.Free;
  inherited Destroy;
end;

class function TdxDocumentVariables.FromStream(AReader: TBinaryReader; AOffset: Integer; ASize: Integer): TdxDocumentVariables;
begin
  Result := TdxDocumentVariables.Create;
  Result.Read(AReader, AOffset, ASize);
end;

class function TdxDocumentVariables.FromVariablesCollection(AVariables: TdxDocumentVariableCollection;
  ADocumentProperties: TdxDocumentProperties): TdxDocumentVariables;
begin
  Result := TdxDocumentVariables.Create;
  Result.GetVariables(AVariables, ADocumentProperties);
end;

procedure TdxDocumentVariables.Read(AReader: TBinaryReader; AOffset: Integer; ASize: Integer);
var
  AVariablesNames: TdxDocStringTable;
  ACount, I, ALength: Integer;
  ABuffer: TBytes;
  AValue: string;
begin
  AVariablesNames := TdxDocStringTable.FromStream(AReader, AOffset, ASize);
  try
    FNames := AVariablesNames.ExtractData;
  finally
    AVariablesNames.Free;
  end;

  ACount := FNames.Count;
  for I := 0 to ACount - 1 do
  begin
    ALength := AReader.ReadUInt16 * 2;
    ABuffer := AReader.ReadBytes(ALength);
    AValue := TdxEncoding.Unicode.GetString(ABuffer, 0, Length(ABuffer));
    Values.Add(AValue);
  end;
end;

procedure TdxDocumentVariables.Write(AWriter: TBinaryWriter);
var
  AVariableNames: TdxDocStringTable;
  ACount, I: Integer;
begin
  AVariableNames := TdxDocStringTable.Create(Names);
  try
    AVariableNames.ExtraDataSize := ExtraDataSize;
    AVariableNames.Write(AWriter);
  finally
    AVariableNames.Free;
  end;
  ACount := FValues.Count;
  for I := 0 to ACount - 1 do
  begin
    AWriter.Write(Word(Length(Values[I])));
    AWriter.Write(TdxEncoding.Unicode.GetBytes(Values[I]));
  end;
end;

procedure TdxDocumentVariables.GetVariables(AVariables: TdxDocumentVariableCollection; ADocumentProperties: TdxDocumentProperties);
var
  AName: string;
  AValue: TValue;
  AUpdateFieldsBeforePrint: TdxUpdateDocVariablesBeforePrint;
begin
  FNames.Free;
  FNames := TdxStringList.Create;
  for AName in AVariables.GetVariableNames do
  begin
    FNames.Add(AName);
    AValue := AVariables[AName];
    FValues.Add(AValue.ToString);
  end;
  AUpdateFieldsBeforePrint := ADocumentProperties.UpdateDocVariablesBeforePrint;
  if AUpdateFieldsBeforePrint <> TdxUpdateDocVariablesBeforePrint.Auto then
  begin
    FNames.Add(TdxDocumentProperties.UpdateDocVariableFieldsBeforePrintDocVarName);
    FValues.Add(ADocumentProperties.GetUpdateFieldsBeforePrintDocVarValue);
  end;
end;

procedure TdxDocumentVariables.SetVariables(AVariables: TdxDocumentVariableCollection; ADocumentProperties: TdxDocumentProperties);
var
  ACount, I: Integer;
  AName: string;
begin
  ACount := FNames.Count;
  for I := 0 to ACount - 1 do
  begin
    AName := FNames[I];
    if AName <> TdxDocumentProperties.UpdateDocVariableFieldsBeforePrintDocVarName then
      AVariables.Add(FNames[I], FValues[I])
    else
      ADocumentProperties.SetUpdateFieldsBeforePrintFromDocVar(FValues[I]);
  end;
end;

end.
