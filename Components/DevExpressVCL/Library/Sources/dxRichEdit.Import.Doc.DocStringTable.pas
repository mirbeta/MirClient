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
unit dxRichEdit.Import.Doc.DocStringTable;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, Classes, Generics.Defaults, Generics.Collections, dxGenerics,
  dxEncoding;

type

  { TdxDocStringTableBase }

  TdxDocStringTableBase = class abstract
  public const
    ExtendedTypeCode = Word($ffff);
  strict private
    FIsExtended: Boolean;
    FEncoding: TEncoding;
    FCount: Integer;
    FExtraDataSize: Integer;
  protected
    procedure Read(AReader: TBinaryReader; AOffset: Integer; ASize: Integer); virtual;
    procedure ReadCore(AReader: TBinaryReader); virtual;
    function CalcIsExtended(AReader: TBinaryReader): Boolean; virtual;
    function GetEncoding: TEncoding; virtual;
    function CalcRecordsCount(AReader: TBinaryReader): Integer; virtual;
    function CalcExtraDataSize(AReader: TBinaryReader): Integer; virtual;
    procedure WriteCore(AWriter: TBinaryWriter); virtual;
    procedure WriteIsExtended(AWriter: TBinaryWriter); virtual;
    procedure WriteCount(AWriter: TBinaryWriter); virtual;
    procedure WriteExtraDataSize(AWriter: TBinaryWriter); virtual;
    procedure ReadString(AReader: TBinaryReader); virtual;
    procedure ReadExtraData(AReader: TBinaryReader); virtual;
    procedure WriteString(AWriter: TBinaryWriter; AIndex: Integer); virtual;
    procedure WriteExtraData(AWriter: TBinaryWriter; AIndex: Integer); virtual;
  public
    procedure Write(AWriter: TBinaryWriter); virtual;

    property IsExtended: Boolean read FIsExtended write FIsExtended;
    property Encoding: TEncoding read FEncoding write FEncoding;
    property Count: Integer read FCount write FCount;
    property ExtraDataSize: Integer read FExtraDataSize write FExtraDataSize;
  end;

  { TdxDocStringTable }

  TdxDocStringTable = class(TdxDocStringTableBase)
  strict private
    FShouldWriteEmptyTable: Boolean;
    FData: TdxStringList;
    FOwnsData: Boolean;
  private
    procedure SetData(const Value: TdxStringList);
  protected
    function CalcIsExtended(AReader: TBinaryReader): Boolean; override;
    function GetEncoding: TEncoding; override;
    procedure ReadString(AReader: TBinaryReader); override;
    procedure ReadExtraData(AReader: TBinaryReader); override;
    procedure WriteString(AWriter: TBinaryWriter; AIndex: Integer); override;
  public
    constructor Create(AData: TdxStringList = nil);
    destructor Destroy; override;
    function ExtractData: TdxStringList;
    class function FromStream(AReader: TBinaryReader; AOffset: Integer; ASize: Integer): TdxDocStringTable; static;
    procedure Write(AWriter: TBinaryWriter); override;

    property ShouldWriteEmptyTable: Boolean read FShouldWriteEmptyTable write FShouldWriteEmptyTable;
    property Data: TdxStringList read FData write SetData;
  end;

  { TdxRmdThreading }

  TdxRmdThreading = class
  strict private
    FSttbMessage: TdxDocStringTable;
    FSttbStyle: TdxDocStringTable;
    FSttbAuthorAttrib: TdxDocStringTable;
    FSttbAuthorValue: TdxDocStringTable;
    FSttbMessageAttrib: TdxDocStringTable;
    FSttbMessageValue: TdxDocStringTable;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Write(AWriter: TBinaryWriter); //virtual;
  end;

implementation

uses
  dxStringHelper;

{ TdxDocStringTableBase }

procedure TdxDocStringTableBase.Read(AReader: TBinaryReader; AOffset: Integer; ASize: Integer);
begin
  Assert(AReader <> nil, 'reader');
  if (ASize = 0) or (AReader.BaseStream.Size < Abs(AOffset)) then
    Exit;
  AReader.BaseStream.Seek(AOffset, TSeekOrigin.soBeginning);
  FIsExtended := CalcIsExtended(AReader);
  FEncoding := GetEncoding;
  Count := CalcRecordsCount(AReader);
  ExtraDataSize := CalcExtraDataSize(AReader);
  ReadCore(AReader);
end;

procedure TdxDocStringTableBase.ReadCore(AReader: TBinaryReader);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    ReadString(AReader);
    ReadExtraData(AReader);
  end;
end;

function TdxDocStringTableBase.CalcIsExtended(AReader: TBinaryReader): Boolean;
begin
  Result := AReader.ReadUInt16 = ExtendedTypeCode;
end;

function TdxDocStringTableBase.GetEncoding: TEncoding;
begin
  Result := TdxEncoding.Unicode;
end;

function TdxDocStringTableBase.CalcRecordsCount(AReader: TBinaryReader): Integer;
begin
  Result := AReader.ReadSmallInt;
end;

function TdxDocStringTableBase.CalcExtraDataSize(AReader: TBinaryReader): Integer;
begin
  Result := AReader.ReadSmallInt;
end;

procedure TdxDocStringTableBase.Write(AWriter: TBinaryWriter);
begin
  Assert(AWriter <> nil, 'writer');
  WriteIsExtended(AWriter);
  WriteCount(AWriter);
  WriteExtraDataSize(AWriter);
  WriteCore(AWriter);
end;

procedure TdxDocStringTableBase.WriteCore(AWriter: TBinaryWriter);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    WriteString(AWriter, I);
    WriteExtraData(AWriter, I);
  end;
end;

procedure TdxDocStringTableBase.WriteIsExtended(AWriter: TBinaryWriter);
begin
  AWriter.Write(ExtendedTypeCode);
end;

procedure TdxDocStringTableBase.WriteCount(AWriter: TBinaryWriter);
begin
  AWriter.Write(SmallInt(Count));
end;

procedure TdxDocStringTableBase.WriteExtraDataSize(AWriter: TBinaryWriter);
begin
  AWriter.Write(SmallInt(ExtraDataSize));
end;

procedure TdxDocStringTableBase.ReadString(AReader: TBinaryReader);
begin
end;

procedure TdxDocStringTableBase.ReadExtraData(AReader: TBinaryReader);
begin
end;

procedure TdxDocStringTableBase.WriteString(AWriter: TBinaryWriter; AIndex: Integer);
begin
end;

procedure TdxDocStringTableBase.WriteExtraData(AWriter: TBinaryWriter; AIndex: Integer);
var
  AExtraData: TBytes;
begin
  SetLength(AExtraData, ExtraDataSize);
  AWriter.Write(AExtraData);
end;

{ TdxDocStringTable }

constructor TdxDocStringTable.Create(AData: TdxStringList = nil);
begin
  if AData = nil then
  begin
    FData := TdxStringList.Create;
    FOwnsData := True;
  end
  else
    FData := AData;
end;

destructor TdxDocStringTable.Destroy;
begin
  if FOwnsData then
    FData.Free;
  inherited Destroy;
end;

function TdxDocStringTable.ExtractData: TdxStringList;
begin
  Result := FData;
  FOwnsData := False;
end;

class function TdxDocStringTable.FromStream(AReader: TBinaryReader; AOffset: Integer; ASize: Integer): TdxDocStringTable;
begin
  Result := TdxDocStringTable.Create;
  Result.Read(AReader, AOffset, ASize);
end;

procedure TdxDocStringTable.Write(AWriter: TBinaryWriter);
begin
  Count := Data.Count;
  if (Count > 0) or ShouldWriteEmptyTable then
    inherited Write(AWriter);
end;

function TdxDocStringTable.CalcIsExtended(AReader: TBinaryReader): Boolean;
begin
  Result := AReader.ReadUInt16 = ExtendedTypeCode;
  if not Result then
    AReader.BaseStream.Seek(-2, TSeekOrigin.soCurrent);
end;

function TdxDocStringTable.GetEncoding: TEncoding;
begin
  if IsExtended then
    Result := TdxEncoding.Unicode
  else
    Result := TdxEncoding.ASCII;
end;

procedure TdxDocStringTable.ReadString(AReader: TBinaryReader);
var
  ALength: Integer;
  ABuffer: TBytes;
  AResult: string;
begin
  if IsExtended then
    ALength := AReader.ReadSmallInt * 2
  else
    ALength := AReader.ReadByte;

  ABuffer := AReader.ReadBytes(ALength);
  AResult := Encoding.GetString(ABuffer, 0, Length(ABuffer));
  Data.Add(TdxStringHelper.RemoveSpecialSymbols(AResult));
end;

procedure TdxDocStringTable.SetData(const Value: TdxStringList);
begin
  assert(false);
  FData.Free;
  FData := Value;
end;

procedure TdxDocStringTable.ReadExtraData(AReader: TBinaryReader);
begin
  AReader.BaseStream.Seek(ExtraDataSize, TSeekOrigin.soCurrent);
end;

procedure TdxDocStringTable.WriteString(AWriter: TBinaryWriter; AIndex: Integer);
var
  S: string;
begin
  S := Data[AIndex];
  AWriter.Write(SmallInt(Length(S)));
  AWriter.Write(TdxEncoding.Unicode.GetBytes(S));
end;

{ TdxRmdThreading }

constructor TdxRmdThreading.Create;
begin
  FSttbMessage := TdxDocStringTable.Create;
  FSttbMessage.ShouldWriteEmptyTable := True;

  FSttbStyle := TdxDocStringTable.Create;
  FSttbStyle.ShouldWriteEmptyTable := True;

  FSttbAuthorAttrib := TdxDocStringTable.Create;
  FSttbAuthorAttrib.ShouldWriteEmptyTable := True;

  FSttbAuthorValue := TdxDocStringTable.Create;
  FSttbAuthorValue.ShouldWriteEmptyTable := True;

  FSttbMessageAttrib := TdxDocStringTable.Create;
  FSttbMessageAttrib.ShouldWriteEmptyTable := True;

  FSttbMessageValue := TdxDocStringTable.Create;
  FSttbMessageValue.ShouldWriteEmptyTable := True;
end;

destructor TdxRmdThreading.Destroy;
begin
  FSttbMessage.Free;
  FSttbStyle.Free;
  FSttbAuthorAttrib.Free;
  FSttbAuthorValue.Free;
  FSttbMessageAttrib.Free;
  FSttbMessageValue.Free;
  inherited Destroy;
end;

procedure TdxRmdThreading.Write(AWriter: TBinaryWriter);
begin
  FSttbMessage.ExtraDataSize := 8;
  FSttbMessage.Write(AWriter);
  FSttbStyle.Write(AWriter);
  FSttbAuthorAttrib.ExtraDataSize := 2;
  FSttbAuthorAttrib.Write(AWriter);
  FSttbAuthorValue.Write(AWriter);
  FSttbMessageAttrib.ExtraDataSize := 2;
  FSttbMessageAttrib.Write(AWriter);
  FSttbAuthorValue.Write(AWriter);
end;

end.
