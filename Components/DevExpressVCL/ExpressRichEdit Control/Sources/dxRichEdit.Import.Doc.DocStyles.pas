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
unit dxRichEdit.Import.Doc.DocStyles;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, Classes, Generics.Defaults, Generics.Collections, dxCore, dxCoreClasses, dxCoreGraphics,
  dxGenerics,

  dxRichEdit.Utils.Types,
  dxRichEdit.DocumentModel.IndexBasedObject,
  dxRichEdit.DocumentModel.CharacterFormatting,
  dxRichEdit.DocumentModel.ParagraphFormatting,
  dxRichEdit.DocumentModel.TabFormatting,
  dxRichEdit.DocumentModel.Styles.Core,
  dxRichEdit.DocumentModel.Styles,
  dxRichEdit.DocumentModel.TableStyles,
  dxRichEdit.DocumentModel.Numbering,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.Doc.Utils,
  dxRichEdit.Import.Doc.DocStyleSheet;

type
  { TdxStyleDescriptionTopologicalComparer }

  TdxStyleDescriptionTopologicalComparer = class(TComparer<TdxStyleDescriptionBase>)
  public
    function Compare(const Left, Right: TdxStyleDescriptionBase): Integer; override;
  end;

  { TdxCharacterStyleDescription }

  TdxCharacterStyleDescription = class(TdxStyleDescriptionBase)
  strict private
    FCharacterUPX: TBytes;
  protected
    procedure ReadUPX(AReader: TBinaryReader); override;
    procedure WriteUPX(AWriter: TBinaryWriter); override;
  public
    constructor Create; overload;
    constructor Create(ACharacterStyle: TdxCharacterStyle; ACharacterStyleIndex: Integer; AFontNameIndex: Integer); overload;
    class function FromStream(AReader: TBinaryReader; ABaseSize: SmallInt; AIsExtended: Boolean): TdxCharacterStyleDescription; static;

    property CharacterUPX: TBytes read FCharacterUPX;
  end;

  { TdxDocParagraphStyleInfo }

  TdxDocParagraphStyleInfo = class
  strict private
    FCharacterFormattingInfo: TdxCharacterFormattingInfo;
    FCharacterFormattingOptions: TdxCharacterFormattingOptions;
    FParagraphFormattingInfo: TdxParagraphFormattingInfo;
    FParagraphFormattingOptions: TdxParagraphFormattingOptions;
    FTabFormattingInfo: TdxTabFormattingInfo;
    FNumberingListIndex: TdxNumberingListIndex;
    FListLevelIndex: Integer;
    FFontNameIndex: Integer;
    FDocumentModel: TdxDocumentModel;
  public
    constructor Create(ACharacterFormattingInfo: TdxCharacterFormattingInfo;
      ACharacterFormattingOptions: TdxCharacterFormattingOptions; AParagraphFormattingInfo: TdxParagraphFormattingInfo;
      AParagraphFormattingOptions: TdxParagraphFormattingOptions; ATabFormattingInfo: TdxTabFormattingInfo;
      ANumberingListIndex, AListLevelIndex, AFontNameIndex: Integer; ADocumentModel: TdxCustomDocumentModel); overload;
    constructor Create(AStyle: TdxParagraphStyle; AFontNameIndex: Integer); overload;
    destructor Destroy; override;

    property CharacterFormattingInfo: TdxCharacterFormattingInfo read FCharacterFormattingInfo;
    property CharacterFormattingOptions: TdxCharacterFormattingOptions read FCharacterFormattingOptions;
    property ParagraphFormattingInfo: TdxParagraphFormattingInfo read FParagraphFormattingInfo;
    property ParagraphFormattingOptions: TdxParagraphFormattingOptions read FParagraphFormattingOptions;
    property TabFormattingInfo: TdxTabFormattingInfo read FTabFormattingInfo;
    property NumberingListIndex: TdxNumberingListIndex read FNumberingListIndex;
    property ListLevelIndex: Integer read FListLevelIndex;
    property FontNameIndex: Integer read FFontNameIndex;
    property DocumentModel: TdxDocumentModel read FDocumentModel;
  end;

  { TdxParagraphStyleDescription }

  TdxParagraphStyleDescription = class(TdxStyleDescriptionBase)
  strict private
    FCharacterUPX: TBytes;
    FParagraphUPX: TBytes;
  protected
    procedure ReadUPX(AReader: TBinaryReader); override;
    procedure WriteUPX(AWriter: TBinaryWriter); override;
  public
    constructor Create; overload;
    constructor Create(AParagraphStyleInfo: TdxDocParagraphStyleInfo); overload;
    class function FromStream(AReader: TBinaryReader; ABaseSize: SmallInt; AIsExtended: Boolean): TdxParagraphStyleDescription; static;

    property CharacterUPX: TBytes read FCharacterUPX;
    property ParagraphUPX: TBytes read FParagraphUPX;
  end;

  { TdxTableStyleDescription }

  TdxTableStyleDescription = class(TdxStyleDescriptionBase)
  strict private
    FTableUPX: TBytes;
    FParagraphUPX: TBytes;
    FCharacterUPX: TBytes;
  protected
    procedure ReadUPX(AReader: TBinaryReader); override;
    procedure WriteUPX(AWriter: TBinaryWriter); override;
  public
    constructor Create; overload;
    constructor Create(ATableStyle: TdxTableStyle; AFontNameIndex: Integer); overload;
    class function FromStream(AReader: TBinaryReader; ABaseSize: SmallInt; AIsExtended: Boolean): TdxTableStyleDescription; static;

    property TableUPX: TBytes read FTableUPX;
    property ParagraphUPX: TBytes read FParagraphUPX;
    property CharacterUPX: TBytes read FCharacterUPX;
  end;

  { TdxListStyleDescription }

  TdxListStyleDescription = class(TdxStyleDescriptionBase)
  strict private
    FParagraphUPX: TBytes;
    FStyleIndex: Word;
  protected
    procedure ReadUPX(AReader: TBinaryReader); override;
    procedure WriteUPX(AWriter: TBinaryWriter); override;
  public
    constructor Create; overload;
    constructor Create(AListStyle: TdxNumberingListStyle); overload;
    class function FromStream(AReader: TBinaryReader; ABaseSize: SmallInt; AIsExtended: Boolean): TdxListStyleDescription; static;

    property ParagraphUPX: TBytes read FParagraphUPX;
  end;

implementation

uses
  Math, Contnrs,
  dxEncoding,
  dxStringHelper,
  dxRichEdit.Import.Doc.DocCommandHelper,
  dxRichEdit.Export.Doc.DocCharacterPropertiesActions,
  dxRichEdit.Export.Doc.DocParagraphPropertiesActions,
  dxRichEdit.Export.Doc.DocTableActions;


{ TdxStyleDescriptionTopologicalComparer }

function TdxStyleDescriptionTopologicalComparer.Compare(const Left, Right: TdxStyleDescriptionBase): Integer;
begin
  if (Left = nil) or (Right = nil) then
    Exit(0);
  if Left.BaseStyleIndex = Right.StyleIndex then
    Exit(1);
  if Right.BaseStyleIndex = Left.StyleIndex then
    Exit(-1);
  Result := 0;
end;

{ TdxCharacterStyleDescription }

constructor TdxCharacterStyleDescription.Create(ACharacterStyle: TdxCharacterStyle; ACharacterStyleIndex: Integer; AFontNameIndex: Integer);
var
  ACharacterMemoryStream: TdxMemoryStream;
  AActions: TdxDocCharacterPropertiesActions;
begin
  inherited Create(TdxStyleType.CharacterStyle, 1);
  ACharacterMemoryStream := TdxMemoryStream.Create;
  try
    AActions := TdxDocCharacterPropertiesActions.Create(ACharacterMemoryStream, ACharacterStyle.CharacterProperties, AFontNameIndex);
    try
      AActions.CreateCharacterPropertiesModifiers(ACharacterStyleIndex, TdxDocStyleIndexes.DefaultParagraphStyleIndex, False);
      FCharacterUPX := ACharacterMemoryStream.ToArray;
    finally
      AActions.Free;
    end;
  finally
    ACharacterMemoryStream.Free;
  end;
end;

constructor TdxCharacterStyleDescription.Create;
begin
  inherited Create(TdxStyleType.CharacterStyle, 1);
end;

class function TdxCharacterStyleDescription.FromStream(AReader: TBinaryReader; ABaseSize: SmallInt; AIsExtended: Boolean): TdxCharacterStyleDescription;
begin
  Result := TdxCharacterStyleDescription.Create;
  Result.Read(AReader, ABaseSize, False);
end;

procedure TdxCharacterStyleDescription.ReadUPX(AReader: TBinaryReader);
var
  ACharacterUPXLength: Word;
begin
  ACharacterUPXLength := AReader.ReadUInt16;
  FCharacterUPX := AReader.ReadBytes(ACharacterUPXLength);
end;

procedure TdxCharacterStyleDescription.WriteUPX(AWriter: TBinaryWriter);
begin
  AWriter.Write(Word(Length(CharacterUPX)));
  AWriter.Write(CharacterUPX);
  AlignOffset(AWriter);
end;

{ TdxDocParagraphStyleInfo }

constructor TdxDocParagraphStyleInfo.Create(AStyle: TdxParagraphStyle; AFontNameIndex: Integer);
begin
  Create(
    AStyle.CharacterProperties.Info.Info,
    AStyle.CharacterProperties.Info.Options,
    AStyle.ParagraphProperties.Info.Info,
    AStyle.ParagraphProperties.Info.Options,
    AStyle.Tabs.Info,
    AStyle.GetNumberingListIndex,
    AStyle.GetListLevelIndex,
    AFontNameIndex,
    AStyle.DocumentModel);
end;

constructor TdxDocParagraphStyleInfo.Create(
  ACharacterFormattingInfo: TdxCharacterFormattingInfo; ACharacterFormattingOptions: TdxCharacterFormattingOptions;
  AParagraphFormattingInfo: TdxParagraphFormattingInfo; AParagraphFormattingOptions: TdxParagraphFormattingOptions;
  ATabFormattingInfo: TdxTabFormattingInfo;
  ANumberingListIndex, AListLevelIndex, AFontNameIndex: Integer; ADocumentModel: TdxCustomDocumentModel);
begin
  FCharacterFormattingInfo := ACharacterFormattingInfo.Clone;
  FCharacterFormattingOptions := ACharacterFormattingOptions;
  FParagraphFormattingInfo := AParagraphFormattingInfo.Clone;
  FParagraphFormattingOptions := AParagraphFormattingOptions;
  FTabFormattingInfo := ATabFormattingInfo.Clone;
  FNumberingListIndex := ANumberingListIndex;
  FListLevelIndex := AListLevelIndex;
  FFontNameIndex := AFontNameIndex;
  FDocumentModel := TdxDocumentModel(ADocumentModel);
end;

destructor TdxDocParagraphStyleInfo.Destroy;
begin
  FCharacterFormattingInfo.Free;
  FParagraphFormattingInfo.Free;
  FTabFormattingInfo.Free;
  inherited Destroy;
end;

{ TdxParagraphStyleDescription }

constructor TdxParagraphStyleDescription.Create(AParagraphStyleInfo: TdxDocParagraphStyleInfo);
var
  AParagraphMemoryStream, ACharacterMemoryStream: TdxMemoryStream;
  AParagraphPropertiesActions: TdxDocParagraphPropertiesActions;
  ACharacterPropertiesActions: TdxDocCharacterPropertiesActions;
begin
  inherited Create(TdxStyleType.ParagraphStyle, 2);
  AParagraphMemoryStream := TdxMemoryStream.Create;
  try
    AParagraphPropertiesActions := TdxDocParagraphPropertiesActions.Create(AParagraphMemoryStream, AParagraphStyleInfo);
    try
      AParagraphPropertiesActions.CreateParagarphPropertyModifiers;
      FParagraphUPX := AParagraphMemoryStream.ToArray;
    finally
      AParagraphPropertiesActions.Free;
    end;
  finally
    AParagraphMemoryStream.Free;
  end;
  ACharacterMemoryStream := TdxMemoryStream.Create;
  try
    ACharacterPropertiesActions := TdxDocCharacterPropertiesActions.Create(ACharacterMemoryStream, AParagraphStyleInfo);
    try
      ACharacterPropertiesActions.CreateCharacterPropertiesModifiers;
      FCharacterUPX := ACharacterMemoryStream.ToArray;
    finally
      ACharacterPropertiesActions.Free;
    end;
  finally
    ACharacterMemoryStream.Free;
  end;
end;

constructor TdxParagraphStyleDescription.Create;
begin
  Create(TdxStyleType.ParagraphStyle, 2);
end;

class function TdxParagraphStyleDescription.FromStream(AReader: TBinaryReader; ABaseSize: SmallInt; AIsExtended: Boolean): TdxParagraphStyleDescription;
begin
  Result := TdxParagraphStyleDescription.Create;
  Result.Read(AReader, ABaseSize, AIsExtended);
end;

procedure TdxParagraphStyleDescription.ReadUPX(AReader: TBinaryReader);
var
  AParagraphUPXLength, ACharacterUPXLength: Word;
begin
  AParagraphUPXLength := AReader.ReadUInt16;
  StyleIndex := AReader.ReadUInt16;
  FParagraphUPX := ReadParagraphUPX(AReader, AParagraphUPXLength);
  AlignOffset(AReader);
  ACharacterUPXLength := AReader.ReadUInt16;
  FCharacterUPX := AReader.ReadBytes(ACharacterUPXLength);
end;

procedure TdxParagraphStyleDescription.WriteUPX(AWriter: TBinaryWriter);
begin
  AWriter.Write(Word((Length(ParagraphUPX) + 2)));
  AWriter.Write(Word(StyleIndex));
  AWriter.Write(ParagraphUPX);
  AlignOffset(AWriter);
  AWriter.Write(Word(Length(CharacterUPX)));
  AWriter.Write(CharacterUPX);
  AlignOffset(AWriter);
end;

{ TdxTableStyleDescription }

constructor TdxTableStyleDescription.Create(ATableStyle: TdxTableStyle; AFontNameIndex: Integer);
var
  ACharacterMemoryStream, AParagraphMemoryStream, ATableMemoryStream: TdxMemoryStream;
  ACharacterActions: TdxDocCharacterPropertiesActions;
  AParagraphActions: TdxDocParagraphPropertiesActions;
  ATableActions: TdxDocTableActions;
begin
  inherited Create(TdxStyleType.TableStyle, 3);
  ACharacterMemoryStream := TdxMemoryStream.Create;
  try
    ACharacterActions := TdxDocCharacterPropertiesActions.Create(ACharacterMemoryStream, ATableStyle.CharacterProperties, AFontNameIndex);
    try
      ACharacterActions.CreateCharacterPropertiesModifiers;
      FCharacterUPX := ACharacterMemoryStream.ToArray;
    finally
      ACharacterActions.Free;
    end;
  finally
    ACharacterMemoryStream.Free;
  end;
  AParagraphMemoryStream := TdxMemoryStream.Create;
  try
    AParagraphActions := TdxDocParagraphPropertiesActions.Create(AParagraphMemoryStream, ATableStyle.ParagraphProperties, ATableStyle.Tabs.Info);
    try
      AParagraphActions.CreateParagarphPropertyModifiers;
      FParagraphUPX := AParagraphMemoryStream.ToArray;
    finally
      AParagraphActions.Free;
    end;
  finally
    AParagraphMemoryStream.Free;
  end;
  ATableMemoryStream := TdxMemoryStream.Create;
  try
    ATableActions := TdxDocTableActions.Create(ATableMemoryStream, ATableStyle);
    try
      ATableActions.CreateTablePropertyModifiers;
      FTableUPX := ATableMemoryStream.ToArray;
    finally
      ATableActions.Free;
    end;
  finally
    ATableMemoryStream.Free;
  end;
end;

constructor TdxTableStyleDescription.Create;
begin
  inherited Create(TdxStyleType.TableStyle, 3);
end;

class function TdxTableStyleDescription.FromStream(AReader: TBinaryReader; ABaseSize: SmallInt; AIsExtended: Boolean): TdxTableStyleDescription;
begin
  Result := TdxTableStyleDescription.Create;
  Result.Read(AReader, ABaseSize, AIsExtended);
end;

procedure TdxTableStyleDescription.ReadUPX(AReader: TBinaryReader);
var
  ATableUPXLength, AParagraphUPXLength, ACharacterUPXLength: Word;
begin
  ATableUPXLength := AReader.ReadUInt16;
  FTableUPX := AReader.ReadBytes(ATableUPXLength);
  AlignOffset(AReader);
  AParagraphUPXLength := AReader.ReadUInt16;
  StyleIndex := AReader.ReadUInt16;
  FParagraphUPX := ReadParagraphUPX(AReader, AParagraphUPXLength);
  AlignOffset(AReader);
  ACharacterUPXLength := AReader.ReadUInt16;
  FCharacterUPX := AReader.ReadBytes(ACharacterUPXLength);
end;

procedure TdxTableStyleDescription.WriteUPX(AWriter: TBinaryWriter);
begin
  AWriter.Write(Word(Length(TableUPX)));
  AWriter.Write(TableUPX);
  AlignOffset(AWriter);
  AWriter.Write(Word((Length(ParagraphUPX) + 2)));
  AWriter.Write(Word(StyleIndex));
  AWriter.Write(ParagraphUPX);
  AlignOffset(AWriter);
  AWriter.Write(Word(Length(CharacterUPX)));
  AWriter.Write(CharacterUPX);
  AlignOffset(AWriter);
end;

{ TdxListStyleDescription }

constructor TdxListStyleDescription.Create(AListStyle: TdxNumberingListStyle);
var
  AValue: SmallInt;
begin
  inherited Create(TdxStyleType.NumberingListStyle, 1);
  AValue := AListStyle.NumberingListIndex + 1;
  FParagraphUPX := TdxDocCommandHelper.CreateSinglePropertyModifier($460b, TdxByteArrayHelper.From<SmallInt>(AValue));
end;

constructor TdxListStyleDescription.Create;
begin
  inherited Create(TdxStyleType.NumberingListStyle, 1);
end;

class function TdxListStyleDescription.FromStream(AReader: TBinaryReader; ABaseSize: SmallInt; AIsExtended: Boolean): TdxListStyleDescription;
begin
  Result := TdxListStyleDescription.Create;
  Result.Read(AReader, ABaseSize, AIsExtended);
end;

procedure TdxListStyleDescription.ReadUPX(AReader: TBinaryReader);
var
  AParagraphUPXLength: Word;
begin
  AParagraphUPXLength := AReader.ReadUInt16;
  FStyleIndex := AReader.ReadUInt16;
  FParagraphUPX := ReadParagraphUPX(AReader, AParagraphUPXLength);
end;

procedure TdxListStyleDescription.WriteUPX(AWriter: TBinaryWriter);
begin
  AWriter.Write(Word(Length(ParagraphUPX) + 2));
  AWriter.Write(Word(StyleIndex));
  AWriter.Write(ParagraphUPX);
  AlignOffset(AWriter);
end;

end.
