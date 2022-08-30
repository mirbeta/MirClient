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
unit dxRichEdit.Import.Doc.FileShapeAddress;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, Classes, Generics.Defaults, Generics.Collections, dxCore, dxCoreClasses, dxCoreGraphics,
  dxGenerics,
  dxRichEdit.Doc.Utils,
  dxRichEdit.DocumentModel.FloatingObjectFormatting;

type

  { TdxFileShapeAddress }

  TdxFileShapeAddress = class
  public const
    Size = 26;
  strict private
    FUseIsBehindDoc: Boolean;
    FShapeIdentifier: Integer;
    FLeft: Integer;
    FTop: Integer;
    FRight: Integer;
    FBottom: Integer;
    FHorizontalPositionType: TdxFloatingObjectHorizontalPositionType;
    FVerticalPositionType: TdxFloatingObjectVerticalPositionType;
    FTextWrapType: TdxFloatingObjectTextWrapType;
    FTextWrapSide: TdxFloatingObjectTextWrapSide;
    FIsBehindDoc: Boolean;
    FAnchorLock: Boolean;
    FTextBoxesCount: Integer;
    function GetWidhtInTwips: Integer;
    function GetHeightInTwips: Integer;
  protected
    procedure Read(AReader: TBinaryReader);
  public
    class function FromStream(AReader: TBinaryReader): TdxFileShapeAddress; static;
    procedure Write(AWriter: TBinaryWriter);

    property ShapeIdentifier: Integer read FShapeIdentifier write FShapeIdentifier;
    property WidhtInTwips: Integer read GetWidhtInTwips;
    property HeightInTwips: Integer read GetHeightInTwips;
    property Left: Integer read FLeft write FLeft;
    property Top: Integer read FTop write FTop;
    property Right: Integer read FRight write FRight;
    property Bottom: Integer read FBottom write FBottom;
    property HorisontalPositionType: TdxFloatingObjectHorizontalPositionType read FHorizontalPositionType write FHorizontalPositionType;
    property VericalPositionType: TdxFloatingObjectVerticalPositionType read FVerticalPositionType write FVerticalPositionType;
    property TextWrapType: TdxFloatingObjectTextWrapType read FTextWrapType write FTextWrapType;
    property TextWrapSide: TdxFloatingObjectTextWrapSide read FTextWrapSide write FTextWrapSide;
    property UseIsBehindDoc: Boolean read FUseIsBehindDoc write FUseIsBehindDoc;
    property IsBehindDoc: Boolean read FIsBehindDoc write FIsBehindDoc;
    property Locked: Boolean read FAnchorLock write FAnchorLock;
  end;

  { TdxFileShapeAddressTable }

  TdxFileShapeAddressTable = class
  strict private
    FCharacterPositions: TdxIntegerList;
    FShapeAddresses: TObjectList<TdxFileShapeAddress>;
    FTranslationTable: TDictionary<Integer, TdxFileShapeAddress>;
    function GetAddressesCount: Integer;
  protected
    procedure Read(AReader: TBinaryReader; AOffset: Integer; ASize: Integer);

    property CharacterPositions: TdxIntegerList read FCharacterPositions;
    property ShapeAddresses: TObjectList<TdxFileShapeAddress> read FShapeAddresses;
  public
    constructor Create;
    destructor Destroy; override;
    class function FromStream(AReader: TBinaryReader; AOffset: Integer; ASize: Integer): TdxFileShapeAddressTable; static;
    procedure AddEntry(ACharacterPosition: Integer; AAddress: TdxFileShapeAddress);
    procedure Finish(ACharacterPosition: Integer);
    procedure Write(AWriter: TBinaryWriter);

    property AddressesCount: Integer read GetAddressesCount;
    property TranslationTable: TDictionary<Integer, TdxFileShapeAddress> read FTranslationTable;
  end;

implementation

{ TdxFileShapeAddress }

class function TdxFileShapeAddress.FromStream(AReader: TBinaryReader): TdxFileShapeAddress;
begin
  Result := TdxFileShapeAddress.Create;
  Result.Read(AReader);
end;

function TdxFileShapeAddress.GetWidhtInTwips: Integer;
begin
  Result := FRight - FLeft;
end;

function TdxFileShapeAddress.GetHeightInTwips: Integer;
begin
  Result := FBottom - FTop;
end;

procedure TdxFileShapeAddress.Read(AReader: TBinaryReader);
var
  AFlags: SmallInt;
  ABelowText: Boolean;
begin
  Assert(AReader <> nil, 'reader');
  FShapeIdentifier := AReader.ReadInt32;
  FLeft := AReader.ReadInt32;
  FTop := AReader.ReadInt32;
  FRight := AReader.ReadInt32;
  FBottom := AReader.ReadInt32;
  AFlags := AReader.ReadSmallInt;
  FHorizontalPositionType := TdxDocFloatingObjectHorizontalPositionTypeCalculator.CalcHorizontalPositionType97((AFlags and $06) shr 1);
  FVerticalPositionType := TdxDocFloatingObjectVerticalPositionTypeCalculator.CalcVerticalPositionType97((AFlags and $18) shr 3);
  FTextWrapType := TdxDocFloatingObjectTextWrapTypeCalculator.CalcTextWrapType((AFlags and $01e0) shr 5);
  FTextWrapSide := TdxDocFloatingObjectTextWrapSideCalculator.CalcTextWrapSide((AFlags and $1e00) shr 9);
  ABelowText := AFlags and $4000 <> 0;

  if TextWrapType = TdxDocFloatingObjectTextWrapTypeCalculator.WrapTypeBehindText then
  begin
    TextWrapType := TdxFloatingObjectTextWrapType.None;
    UseIsBehindDoc := True;
    IsBehindDoc := ABelowText;
  end;
  FAnchorLock := AFlags and $8000 <> 0;
  FTextBoxesCount := AReader.ReadInt32;
end;

procedure TdxFileShapeAddress.Write(AWriter: TBinaryWriter);
var
  AFlags: Word;
begin
  Assert(AWriter <> nil, 'writer');
  AWriter.Write(ShapeIdentifier);
  AWriter.Write(Left);
  AWriter.Write(Top);
  AWriter.Write(Right);
  AWriter.Write(Bottom);
  AFlags := 0;
  AFlags := AFlags or Word(TdxDocFloatingObjectHorizontalPositionTypeCalculator.CalcHorizontalPositionTypeCode97(HorisontalPositionType) shl 1);
  AFlags := AFlags or Word(TdxDocFloatingObjectVerticalPositionTypeCalculator.CalcVerticalPositionTypeCode97(VericalPositionType) shl 3);
  AFlags := AFlags or Word(TdxDocFloatingObjectTextWrapTypeCalculator.CalcTextWrapTypeCode(TextWrapType) shl 5);
  AFlags := AFlags or Word(TdxDocFloatingObjectTextWrapSideCalculator.CalcTextWrapSideTypeCode(TextWrapSide) shl 9);

  if (TextWrapType = TdxDocFloatingObjectTextWrapTypeCalculator.WrapTypeBehindText) and IsBehindDoc then
    AFlags := AFlags or $4000;
  if Locked then
    AFlags := AFlags or $8000;
  AWriter.Write(AFlags);
  AWriter.Write(FTextBoxesCount);
end;

{ TdxFileShapeAddressTable }

constructor TdxFileShapeAddressTable.Create;
begin
  FCharacterPositions := TdxIntegerList.Create;
  FShapeAddresses := TObjectList<TdxFileShapeAddress>.Create;
  FTranslationTable := TDictionary<Integer, TdxFileShapeAddress>.Create;
end;

destructor TdxFileShapeAddressTable.Destroy;
begin
  FCharacterPositions.Free;
  FShapeAddresses.Free;
  FTranslationTable.Free;
  inherited Destroy;
end;

class function TdxFileShapeAddressTable.FromStream(AReader: TBinaryReader; AOffset: Integer; ASize: Integer): TdxFileShapeAddressTable;
begin
  Result := TdxFileShapeAddressTable.Create;
  Result.Read(AReader, AOffset, ASize);
end;

function TdxFileShapeAddressTable.GetAddressesCount: Integer;
begin
  Result := FShapeAddresses.Count;
end;

procedure TdxFileShapeAddressTable.Read(AReader: TBinaryReader; AOffset: Integer; ASize: Integer);
var
  ACount, I: Integer;
begin
  Assert(AReader <> nil, 'reader');
  if ASize = 0 then
    Exit;
  AReader.BaseStream.Seek(AOffset, TSeekOrigin.soBeginning);
  ACount := (ASize - TdxDocConstants.CharacterPositionSize) div (TdxDocConstants.CharacterPositionSize + TdxFileShapeAddress.Size);
  for I := 0 to ACount + 1 - 1 do
    CharacterPositions.Add(AReader.ReadInt32);
  for I := 0 to ACount - 1 do
    ShapeAddresses.Add(TdxFileShapeAddress.FromStream(AReader));
  for I := 0 to ACount - 1 do
    TranslationTable.Add(CharacterPositions[I], ShapeAddresses[I]);
end;

procedure TdxFileShapeAddressTable.AddEntry(ACharacterPosition: Integer; AAddress: TdxFileShapeAddress);
begin
  CharacterPositions.Add(ACharacterPosition);
  ShapeAddresses.Add(AAddress);
end;

procedure TdxFileShapeAddressTable.Finish(ACharacterPosition: Integer);
begin
  CharacterPositions.Add(ACharacterPosition);
end;

procedure TdxFileShapeAddressTable.Write(AWriter: TBinaryWriter);
var
  ACount, I: Integer;
begin
  Assert(AWriter <> nil, 'writer');
  ACount := CharacterPositions.Count;
  if ACount = 0 then
    Exit;
  for I := 0 to ACount - 1 do
    AWriter.Write(CharacterPositions[I]);
  for I := 0 to ACount - 1 - 1 do
    ShapeAddresses[I].Write(AWriter);
end;

end.
