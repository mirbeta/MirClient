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
unit dxRichEdit.Import.Doc.ListFormatOverride;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, Classes, Generics.Defaults, Generics.Collections, dxCore, dxCoreClasses,
  dxCoreGraphics, dxGenerics,

  dxRichEdit.DocumentModel.NumberingFormatting,
  dxRichEdit.Doc.Utils,
  dxRichEdit.Import.Doc.ListFormatInformation;

type
  { TdxDocListOverrideFormat }

  TdxDocListOverrideFormat = class
  strict private
    FListIdentifier: Integer;
    FLevelsCount: Byte;
    FAutoNumberedFieldType: Byte;
    FHtmlCompatibilities: Byte;
  protected
    procedure Read(AReader: TBinaryReader);
  public
    class function FromStream(AReader: TBinaryReader): TdxDocListOverrideFormat; static;
    procedure Write(AWriter: TBinaryWriter);

    property ListIdentifier: Integer read FListIdentifier write FListIdentifier;
    property LevelsCount: Byte read FLevelsCount write FLevelsCount;
  end;

  { TdxDocListOverrideLevelFormat }

  TdxDocListOverrideLevelFormat = class
  strict private
    FStartAt: Integer;
    FOverriddenLevel: Integer;
    FOverrideStart: Boolean;
    FOverrideFormatting: Boolean;
    FOverrideLevelFormatting: TdxDocListLevel;
  protected
    procedure Read(AReader: TBinaryReader);
  public
    destructor Destroy; override;
    class function FromStream(AReader: TBinaryReader): TdxDocListOverrideLevelFormat; static;
    procedure Write(AWriter: TBinaryWriter);
    procedure BeforeWrite;

    property StartAt: Integer read FStartAt write FStartAt;
    property OverriddenLevel: Integer read FOverriddenLevel write FOverriddenLevel;
    property OverrideStart: Boolean read FOverrideStart write FOverrideStart;
    property OverrideFormatting: Boolean read FOverrideFormatting write FOverrideFormatting;
    property OverrideLevelFormatting: TdxDocListLevel read FOverrideLevelFormatting write FOverrideLevelFormatting;
  end;

  { TdxDocListOverrideLevelInformation }

  TdxDocListOverrideLevelInformation = class
  strict private
    FCharacterPosition: Integer;
    FLevelFormatOverrideData: TdxObjectList<TdxDocListOverrideLevelFormat>;
  protected
    procedure Read(AReader: TBinaryReader; ALevelsCount: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    class function FromStream(AReader: TBinaryReader; ALevelsCount: Integer): TdxDocListOverrideLevelInformation; static;
    procedure Write(AWriter: TBinaryWriter; ALevelsCount: Integer);

    property CharacterPosition: Integer read FCharacterPosition write FCharacterPosition;
    property LevelFormatOverrideData: TdxObjectList<TdxDocListOverrideLevelFormat> read FLevelFormatOverrideData;
  end;

  { TdxDocListOverrideFormatInformation }

  TdxDocListOverrideFormatInformation = class
  strict private
    FListFormatOverride: TdxObjectList<TdxDocListOverrideFormat>;
    FListFormatOverrideData: TdxObjectList<TdxDocListOverrideLevelInformation>;
  protected
    procedure Read(AReader: TBinaryReader; AOffset: Integer; ASize: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    class function FromStream(AReader: TBinaryReader; AOffset: Integer; ASize: Integer): TdxDocListOverrideFormatInformation; static;
    procedure Write(AWriter: TBinaryWriter; AAllowNonLinkedListDefinitions: Boolean);
    procedure BeforeWrite(AAllowNonLinkedListDefinitions: Boolean);

    property FormatOverride: TdxObjectList<TdxDocListOverrideFormat> read FListFormatOverride;
    property FormatOverrideData: TdxObjectList<TdxDocListOverrideLevelInformation> read FListFormatOverrideData;
  end;


implementation

uses
  Math, Contnrs, dxCharacters,
  dxRichEdit.Import.Rtf,
  dxStringHelper;

{ TdxDocListOverrideFormat }

class function TdxDocListOverrideFormat.FromStream(AReader: TBinaryReader): TdxDocListOverrideFormat;
begin
  Result := TdxDocListOverrideFormat.Create;
  Result.Read(AReader);
end;

procedure TdxDocListOverrideFormat.Read(AReader: TBinaryReader);
begin
  Assert(AReader <> nil, 'reader');
  FListIdentifier := AReader.ReadInt32;
  AReader.BaseStream.Seek(8, TSeekOrigin.soCurrent);
  FLevelsCount := AReader.ReadByte;
  FAutoNumberedFieldType := AReader.ReadByte;
  FHtmlCompatibilities := AReader.ReadByte;
  AReader.ReadByte;
end;

procedure TdxDocListOverrideFormat.Write(AWriter: TBinaryWriter);
begin
  Assert(AWriter <> nil, 'writer');
  AWriter.Write(FListIdentifier);
  AWriter.BaseStream.Seek(8, TSeekOrigin.soCurrent);
  AWriter.Write(FLevelsCount);
  AWriter.BaseStream.Seek(3, TSeekOrigin.soCurrent);
end;

{ TdxDocListOverrideLevelFormat }

destructor TdxDocListOverrideLevelFormat.Destroy;
begin
  FOverrideLevelFormatting.Free;
  inherited Destroy;
end;

class function TdxDocListOverrideLevelFormat.FromStream(AReader: TBinaryReader): TdxDocListOverrideLevelFormat;
begin
  Result := TdxDocListOverrideLevelFormat.Create;
  Result.Read(AReader);
end;

procedure TdxDocListOverrideLevelFormat.Read(AReader: TBinaryReader);
var
  AListLevelOptionsBitField: Integer;
begin
  Assert(AReader <> nil, 'reader');
  FStartAt := AReader.ReadInt32;
  AListLevelOptionsBitField := AReader.ReadInt32;
  FOverriddenLevel := AListLevelOptionsBitField and $0f;
  FOverrideStart := (AListLevelOptionsBitField and $10) = $10;
  FOverrideFormatting := (AListLevelOptionsBitField and $20) = $20;
  if FOverrideFormatting then
    FOverrideLevelFormatting := TdxDocListLevel.FromStream(AReader);
end;

procedure TdxDocListOverrideLevelFormat.Write(AWriter: TBinaryWriter);
var
  AListLevelOptionsBitField: Integer;
begin
  Assert(AWriter <> nil, 'writer');
  BeforeWrite;
  AWriter.Write(FStartAt);
  AListLevelOptionsBitField := FOverriddenLevel;
  if FOverrideStart then
    AListLevelOptionsBitField := AListLevelOptionsBitField or $10;
  if FOverrideFormatting then
    AListLevelOptionsBitField := AListLevelOptionsBitField or $20;
  AWriter.Write(AListLevelOptionsBitField);
  if FOverrideFormatting then
    FOverrideLevelFormatting.Write(AWriter);
end;

procedure TdxDocListOverrideLevelFormat.BeforeWrite;
begin
  if not OverrideStart or (OverrideStart and OverrideFormatting) then
    FStartAt := 0;
end;

{ TdxDocListOverrideLevelInformation }

constructor TdxDocListOverrideLevelInformation.Create;
begin
  FLevelFormatOverrideData := TdxObjectList<TdxDocListOverrideLevelFormat>.Create;
end;

destructor TdxDocListOverrideLevelInformation.Destroy;
begin
  FLevelFormatOverrideData.Free;
  inherited Destroy;
end;

class function TdxDocListOverrideLevelInformation.FromStream(AReader: TBinaryReader; ALevelsCount: Integer): TdxDocListOverrideLevelInformation;
begin
  Result := TdxDocListOverrideLevelInformation.Create;
  Result.Read(AReader, ALevelsCount);
end;

procedure TdxDocListOverrideLevelInformation.Read(AReader: TBinaryReader; ALevelsCount: Integer);
var
  I: Integer;
begin
  Assert(AReader <> nil, 'reader');
  FCharacterPosition := Integer(AReader.ReadUInt32);
  for I := 0 to ALevelsCount - 1 do
    FLevelFormatOverrideData.Add(TdxDocListOverrideLevelFormat.FromStream(AReader));
end;

procedure TdxDocListOverrideLevelInformation.Write(AWriter: TBinaryWriter; ALevelsCount: Integer);
var
  I: Integer;
begin
  Assert(AWriter <> nil, 'writer');
  AWriter.Write(FCharacterPosition);
  for I := 0 to ALevelsCount - 1 do
    FLevelFormatOverrideData[I].Write(AWriter);
end;

{ TdxDocListOverrideFormatInformation }

constructor TdxDocListOverrideFormatInformation.Create;
begin
  FListFormatOverride := TdxObjectList<TdxDocListOverrideFormat>.Create;
  FListFormatOverrideData := TdxObjectList<TdxDocListOverrideLevelInformation>.Create;
end;

destructor TdxDocListOverrideFormatInformation.Destroy;
begin
  FListFormatOverride.Free;
  FListFormatOverrideData.Free;
  inherited Destroy;
end;

class function TdxDocListOverrideFormatInformation.FromStream(AReader: TBinaryReader; AOffset: Integer; ASize: Integer): TdxDocListOverrideFormatInformation;
begin
  Result := TdxDocListOverrideFormatInformation.Create;
  Result.Read(AReader, AOffset, ASize);
end;

procedure TdxDocListOverrideFormatInformation.Read(AReader: TBinaryReader; AOffset: Integer; ASize: Integer);
var
  AListCount, I: Integer;
begin
  Assert(AReader <> nil, 'reader');
  if ASize = 0 then
    Exit;
  AReader.BaseStream.Seek(AOffset, TSeekOrigin.soBeginning);
  AListCount := AReader.ReadInt32;
  for I := 0 to AListCount - 1 do
    FListFormatOverride.Add(TdxDocListOverrideFormat.FromStream(AReader));
  for I := 0 to AListCount - 1 do
    FListFormatOverrideData.Add(TdxDocListOverrideLevelInformation.FromStream(AReader, FListFormatOverride[I].LevelsCount));
end;

procedure TdxDocListOverrideFormatInformation.Write(AWriter: TBinaryWriter; AAllowNonLinkedListDefinitions: Boolean);
var
  ACount, I: Integer;
begin
  Assert(AWriter <> nil, 'writer');
  BeforeWrite(AAllowNonLinkedListDefinitions);
  ACount := FListFormatOverride.Count;
  if ACount = 0 then
    Exit;
  AWriter.Write(ACount);
  for I := 0 to ACount - 1 do
    FListFormatOverride[I].Write(AWriter);
  for I := 0 to ACount - 1 do
    FListFormatOverrideData[I].Write(AWriter, FListFormatOverride[I].LevelsCount);
end;

procedure TdxDocListOverrideFormatInformation.BeforeWrite(AAllowNonLinkedListDefinitions: Boolean);
var
  ACount, I: Integer;
begin
  if AAllowNonLinkedListDefinitions then
    Exit;
  ACount := FListFormatOverride.Count;
  for I := 0 to ACount - 1 do
  begin
    if FListFormatOverrideData[I].CharacterPosition = Integer($ffffffff) then
    begin
      FListFormatOverride[I].LevelsCount := 0;
      FListFormatOverrideData[I].LevelFormatOverrideData.Clear;
    end;
  end;
end;

end.
