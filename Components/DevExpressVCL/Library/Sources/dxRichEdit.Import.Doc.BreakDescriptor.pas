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
unit dxRichEdit.Import.Doc.BreakDescriptor;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, Classes, Generics.Defaults, Generics.Collections, dxCore, dxCoreGraphics, dxGenerics,
  dxRichEdit.Utils.Types,
  dxRichEdit.DocumentModel.UnitConverter,
  dxRichEdit.DocumentModel.Borders,
  dxRichEdit.Doc.Utils;

type

  { TdxBreakDescriptor }

  TdxBreakDescriptor = class
  public const
    ReservedDataSize = Integer(4);
    LastIndex        = Integer($ffff);
    Size             = Integer(6);
  strict private
    FIndex: SmallInt;
  protected
    procedure Read(AReader: TBinaryReader);
  public
    class function FromStream(AReader: TBinaryReader): TdxBreakDescriptor; static;
    class function CreateLastDescriptor: TdxBreakDescriptor; static;
    procedure Write(AWriter: TBinaryWriter);

    property Index: SmallInt read FIndex write FIndex;
  end;

  { TdxBreakDescriptorTable }

  TdxBreakDescriptorTable = class
  strict private
    FCharacterPositions: TdxIntegerList;
    FBreakDescriptors: TObjectList<TdxBreakDescriptor>;
    FCurrentIndex: SmallInt;
  protected
    procedure Read(AReader: TBinaryReader; AOffset: Integer; ASize: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    class function FromStream(AReader: TBinaryReader; AOffset: Integer; ASize: Integer): TdxBreakDescriptorTable; static;
    procedure Write(AWriter: TBinaryWriter);
    procedure AddEntry(ACharacterPosition: Integer);
    procedure Finish(ACharacterPosition: Integer);

    property CharacterPositions: TdxIntegerList read FCharacterPositions;
    property BreakDescriptors: TObjectList<TdxBreakDescriptor> read FBreakDescriptors;
  end;


implementation

{ TdxBreakDescriptor }

class function TdxBreakDescriptor.FromStream(AReader: TBinaryReader): TdxBreakDescriptor;
begin
  Result := TdxBreakDescriptor.Create;
  Result.Read(AReader);
end;

class function TdxBreakDescriptor.CreateLastDescriptor: TdxBreakDescriptor;
begin
  Result := TdxBreakDescriptor.Create;
  Result.Index := SmallInt(LastIndex);
end;

procedure TdxBreakDescriptor.Read(AReader: TBinaryReader);
begin
  Index := AReader.ReadSmallInt;
  AReader.BaseStream.Seek(ReservedDataSize, TSeekOrigin.soCurrent);
end;

procedure TdxBreakDescriptor.Write(AWriter: TBinaryWriter);
begin
  AWriter.Write(Index);
  AWriter.Seek(ReservedDataSize, TSeekOrigin.soCurrent);
end;

{ TdxBreakDescriptorTable }

constructor TdxBreakDescriptorTable.Create;
begin
  FCharacterPositions := TdxIntegerList.Create;
  FBreakDescriptors := TObjectList<TdxBreakDescriptor>.Create;
end;

destructor TdxBreakDescriptorTable.Destroy;
begin
  FCharacterPositions.Free;
  FBreakDescriptors.Free;
  inherited Destroy;
end;

class function TdxBreakDescriptorTable.FromStream(AReader: TBinaryReader; AOffset: Integer; ASize: Integer): TdxBreakDescriptorTable;
begin
  Result := TdxBreakDescriptorTable.Create;
  Result.Read(AReader, AOffset, ASize);
end;

procedure TdxBreakDescriptorTable.Read(AReader: TBinaryReader; AOffset: Integer; ASize: Integer);
var
  ACount, I: Integer;
begin
  Assert(AReader <> nil, 'reader');
  if (ASize = 0) or (AOffset + ASize > AReader.BaseStream.Size) then
    Exit;

  AReader.BaseStream.Seek(AOffset, TSeekOrigin.soBeginning);
  ACount := (ASize - TdxDocConstants.CharacterPositionSize) div (TdxDocConstants.CharacterPositionSize + TdxBreakDescriptor.Size);
  for I := 0 to ACount do
    FCharacterPositions.Add(AReader.ReadInt32);
  for I := 0 to ACount - 1 do
    FBreakDescriptors.Add(TdxBreakDescriptor.FromStream(AReader));
end;

procedure TdxBreakDescriptorTable.Write(AWriter: TBinaryWriter);
var
  ACount, I: Integer;
begin
  ACount := FCharacterPositions.Count;
  for I := 0 to ACount - 1 do
    AWriter.Write(FCharacterPositions[I]);
  ACount := FBreakDescriptors.Count;
  for I := 0 to ACount - 1 do
    FBreakDescriptors[I].Write(AWriter);
end;

procedure TdxBreakDescriptorTable.AddEntry(ACharacterPosition: Integer);
var
  ABreakDescriptor: TdxBreakDescriptor;
begin
  FCharacterPositions.Add(ACharacterPosition);
  ABreakDescriptor := TdxBreakDescriptor.Create;
  ABreakDescriptor.Index := FCurrentIndex;
  FBreakDescriptors.Add(ABreakDescriptor);
  Inc(FCurrentIndex);
end;

procedure TdxBreakDescriptorTable.Finish(ACharacterPosition: Integer);
begin
  FCharacterPositions.Add(ACharacterPosition);
  FBreakDescriptors.Add(TdxBreakDescriptor.CreateLastDescriptor);
end;

end.
