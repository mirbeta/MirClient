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
unit dxRichEdit.Doc.Utils;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, Classes, Generics.Defaults, Generics.Collections, dxCore, dxCoreClasses,
  dxCoreGraphics, dxGenerics,
  dxRichEdit.Utils.Types,
  dxRichEdit.Utils.OfficeImage,
  dxRichEdit.Platform.Font,
  dxRichEdit.DocumentModel.IndexBasedObject,
  dxRichEdit.DocumentModel.CharacterFormatting,
  dxRichEdit.DocumentModel.ParagraphFormatting,
  dxRichEdit.DocumentModel.TableFormatting,
  dxRichEdit.DocumentModel.FloatingObjectFormatting,
  dxRichEdit.DocumentModel.NumberingFormatting,
  dxRichEdit.DocumentModel.Numbering,
  dxRichEdit.DocumentModel.SectionFormatting,
  dxRichEdit.DocumentModel.Section,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.ProtectionFormatting,
  dxRichEdit.NativeApi,
  dxRichEdit.Import.Doc.DocCharacterFormattingInfo;

type
  { IdxDocOfficeImageCreator }

  IdxDocOfficeImageCreator = interface
  ['{23E68E8E-38E4-48A0-BE8A-D3FD08587535}']
    function CreateImage(const AStream: TStream): TdxOfficeImageReference; overload;
    function CreateImage(AImage: TdxOfficeImage): TdxOfficeImageReference; overload;
    function CreateMetafile(AStream: TMemoryStream; AMapMode: TdxMapMode; APictureWidth, APictureHeight: Integer): TdxOfficeImageReference;
  end;

  { TdxTextCodes }

  TdxTextCodes = class
  public const
    InlinePicture                 = #1;
    AutoNumberedFootNoteReference = #2;
    FootNoteSeparatorCharacter    = #3;
    FootNoteContinuationCharacter = #4;
    AnnotationReference           = #5;
    TableUnitMark                 = #7;
    FloatingObjectAnchor          = #8;
    SectionMark                   = #12;
    ParagraphMark                 = #13;
    ColumnBreak                   = #14;
    FieldBegin                    = #19;
    FieldSeparator                = #20;
    FieldEnd                      = #21;
    SpecialSymbol                 = '(';
  end;

  { TdxTextRunStartReason }

  TdxTextRunStartReason = (
    TextRunMark,
    ParagraphMark,
    SectionMark,
    ColumnBreak,
    TableUnitMark
  );
  TdxTextRunStartReasons = set of TdxTextRunStartReason;

  { IdxDocShadingDescriptor }

  IdxDocShadingDescriptor = interface
  ['{7FBE22AC-908A-4485-ACCC-B4645BEA81D2}']
    function GetForeColor: TdxAlphaColor;
    procedure SetForeColor(const Value: TdxAlphaColor);
    function GetBackgroundColor: TdxAlphaColor;
    procedure SetBackgroundColor(const Value: TdxAlphaColor);
    function GetShadingPattern: SmallInt;
    procedure SetShadingPattern(const Value: SmallInt);

    property ForeColor: TdxAlphaColor read GetForeColor write SetForeColor;
    property BackgroundColor: TdxAlphaColor read GetBackgroundColor write SetBackgroundColor;
    property ShadingPattern: SmallInt read GetShadingPattern write SetShadingPattern;
  end;

  { TdxDocConstants }

  TdxDocConstants = class
  public const
    ContentBuilderLastSupportedVersion = 105;
    ContentBuilderSectorSize = 512;

    CharacterPositionSize = 4;
    LastByteOffsetInSector = 511;
    MaxXASValue = 31680;
    DefaultMSWordColor: array[0..16] of TdxAlphaColor = (
      TdxAlphaColors.Empty,
      TdxAlphaColors.Black,
      TdxAlphaColors.Blue,
      TdxAlphaColors.Cyan,
      TdxAlphaColors.Lime,
      TdxAlphaColors.Magenta,
      TdxAlphaColors.Red,
      TdxAlphaColors.Yellow,
      TdxAlphaColors.White,
      TdxAlphaColors.Navy,
      TdxAlphaColors.Teal,
      TdxAlphaColors.Green,
      TdxAlphaColors.Purple,
      TdxAlphaColors.Maroon,
      TdxAlphaColors.Olive,
      TdxAlphaColors.Gray,
      TdxAlphaColors.Silver
    );
  public
    class function GetColorIndex(AColor: TdxAlphaColor): Integer; static;
  end;

  { TdxDocStyleIndexes }

  TdxDocStyleIndexes = class
  public const
    DefaultParagraphStyleIndex = Integer($0000);
    DefaultCharacterStyleIndex = Integer($000a);
    DefaultTableStyleIndex     = Integer($000b);
    DefaultListStyleIndex      = Integer($000c);
  end;

  { TdxDocColorReference }

  TdxDocColorReference = class
  public const
    ColorReferenceSize = 4;
  strict private
    FColor: TdxAlphaColor;
  protected
    procedure Read(const AData: TBytes; AStartIndex: Integer);
  public
    constructor Create;
    class function FromByteArray(const AData: TBytes; AStartIndex: Integer): TdxDocColorReference; static;
    function GetBytes: TBytes;

    property Color: TdxAlphaColor read FColor write FColor;
  end;

  { TdxTextRunBorder }

  TdxTextRunBorder = class
  strict private
    FOffset: Integer;
    FReason: TdxTextRunStartReasons;
  public
    constructor Create(AOffset: Integer; const AReason: TdxTextRunStartReasons);

    property Offset: Integer read FOffset;
    property Reason: TdxTextRunStartReasons read FReason write FReason;
  end;

  { TdxTextRunBorderComparable }

  TdxTextRunBorderComparable = class(TInterfacedObject, IdxComparable<TdxTextRunBorder>)
  strict private
    FOffset: Integer;
  public
    constructor Create(AOffset: Integer);
    function CompareTo(const ATextRunBorder: TdxTextRunBorder): Integer;

    property Offset: Integer read FOffset;
  end;

  { TdxDocCharacterFormattingHelper }

  TdxDocCharacterFormattingHelper = class
  public
    class function GetMergedCharacterFormattingInfo(AInfo: TdxDocCharacterFormattingInfo;
      AParentInfo: TdxCharacterFormattingInfo; ADocumentModel: TdxDocumentModel): TdxCharacterFormattingInfo; static;
    class function ConvertFromDocBoolWrapper(AValue: Boolean; ABoolWrapper: TdxDocBoolWrapper): Boolean; static;
    class function CalcStriceoutType(AStrike: TdxDocBoolWrapper; ADoubleStrike: TdxDocBoolWrapper;
      ABaseStrikeout: TdxStrikeoutType): TdxStrikeoutType; static;
  end;

  { TdxAlignmentCalculator }

  TdxAlignmentCalculator = class
  public
    class function CalcParagraphAlignment(AAlignmentCode: Byte): TdxParagraphAlignment; static;
    class function CalcParagraphAlignmentCode(AAlignment: TdxParagraphAlignment): Byte; static;
    class function CalcListNumberAlignment(AAlignmentCode: Byte): TdxListNumberAlignment; static;
    class function CalcListNumberAlignmentCode(AAlignment: TdxListNumberAlignment): Byte; static;
    class function CalcVerticalAlignment(AAlignmentTypeCode: Byte): TdxVerticalAlignment; static;
    class function CalcVerticalAlignmentTypeCode(AAlignment: TdxVerticalAlignment): Byte; static;
  end;

  { TdxNumberingFormatInfo }

  TdxNumberingFormatInfo = class
  strict private
    FFormatCode: Byte;
    FNumberingFormat: TdxNumberingFormat;
  public
    constructor Create(AFormatCode: Byte; ANumberingFormat: TdxNumberingFormat);

    property FormatCode: Byte read FFormatCode;
    property NumberingFormat: TdxNumberingFormat read FNumberingFormat;
  end;

  { TdxNumberingFormatCalculator }

  TdxNumberingFormatCalculator = class
  strict private class var
    FInfos: TObjectList<TdxNumberingFormatInfo>;
    FNumberingFormats: TDictionary<Byte, TdxNumberingFormat>;
    FNumberingFormatCodes: TDictionary<TdxNumberingFormat, Byte>;
    class constructor Initialize;
    class destructor Finalize;
  public
    class function CalcNumberingFormat(ANumberingFormatCode: Integer): TdxNumberingFormat; static;
    class function CalcNumberingFormatCode(ANumberingFormat: TdxNumberingFormat): Byte; static;
  end;

  { TdxWidthUnitCalculator }

  TdxWidthUnitCalculator = class
  public
    class function CalcWidthUnitType(ATypeCode: Byte): TdxWidthUnitType; static;
    class function CalcWidthUnitTypeCode(AUnitType: TdxWidthUnitType): Byte; static;
  end;

  { TdxMergingStateCalculator }

  TdxMergingStateCalculator = class
  public
    class function CalcHorizontalMergingState(ATypeCode: Byte): TdxMergingState; static;
    class function CalcHorizontalMergingTypeCode(AMergingState: TdxMergingState): Byte; static;
    class function CalcVerticalMergingState(ATypeCode: Byte): TdxMergingState; static;
    class function CalcVerticalMergingTypeCode(AMergingState: TdxMergingState): Byte; static;
  end;

  { TdxTextDirectionCalculator }

  TdxTextDirectionCalculator = class
  public
    class function CalcTextDirection(ATypeCode: Byte): TdxTextDirection; static;
  end;

  { TdxFootNotePositionCalculator }

  TdxFootNotePositionCalculator = class
  public
    class function CalcFootNotePosition(ATypeCode: Integer): TdxFootNotePosition; static;
    class function CalcFootNotePositionTypeCode(APosition: TdxFootNotePosition): Byte; static;
    class function CalcFootNotePositionTypeCodeForDocumentProperties(APosition: TdxFootNotePosition): Byte; static;
  end;

  { TdxFootNoteNumberingRestartCalculator }

  TdxFootNoteNumberingRestartCalculator = class
  public
    class function CalcFootNoteNumberingRestart(ATypeCode: Integer): TdxLineNumberingRestart; static;
    class function CalcFootNoteNumberingRestartTypeCode(ANumberingRestart: TdxLineNumberingRestart): Byte; static;
  end;

  { TdxDocFloatingObjectHorizontalPositionTypeCalculator }

  TdxDocFloatingObjectHorizontalPositionTypeCalculator = class
  public
    class function CalcHorizontalPositionType97(ATypeCode: Integer): TdxFloatingObjectHorizontalPositionType; static;
    class function CalcHorizontalPositionTypeCode97(AType: TdxFloatingObjectHorizontalPositionType): Byte; static;
  end;

  { TdxDocFloatingObjectVerticalPositionTypeCalculator }

  TdxDocFloatingObjectVerticalPositionTypeCalculator = class
  public
    class function CalcVerticalPositionType97(ATypeCode: Integer): TdxFloatingObjectVerticalPositionType; static;
    class function CalcVerticalPositionTypeCode97(AType: TdxFloatingObjectVerticalPositionType): Byte; static;
  end;

  { TdxDocFloatingObjectTextWrapTypeCalculator }

  TdxDocFloatingObjectTextWrapTypeCalculator = class
  public const
    WrapTypeBehindText = TdxFloatingObjectTextWrapType(-1);
  public
    class function CalcTextWrapType(ATypeCode: Integer): TdxFloatingObjectTextWrapType; static;
    class function CalcTextWrapTypeCode(AType: TdxFloatingObjectTextWrapType): Byte; static;
  end;

  { TdxDocFloatingObjectTextWrapSideCalculator }

  TdxDocFloatingObjectTextWrapSideCalculator = class
  public
    class function CalcTextWrapSide(ATypeCode: Integer): TdxFloatingObjectTextWrapSide; static;
    class function CalcTextWrapSideTypeCode(AWrapSide: TdxFloatingObjectTextWrapSide): Byte; static;
  end;

  { TdxDocTableBorderColorReference }

  TdxDocTableBorderColorReference = class
  public const
    ColorReferenceSize = 4;
  strict private
    FColor: TdxAlphaColor;
  protected
    procedure Read(const AData: TBytes; AStartIndex: Integer);
  public
    class function FromByteArray(const AData: TBytes; AStartIndex: Integer): TdxDocTableBorderColorReference; static;
    function GetBytes: TBytes;

    property Color: TdxAlphaColor read FColor write FColor;
  end;
  TdxDocTableBorderColorReferenceList = class(TdxObjectList<TdxDocTableBorderColorReference>);

  { TdxDocShadingDescriptor }

  TdxDocShadingDescriptor = class(TcxIUnknownObject, IdxDocShadingDescriptor)
  public const
    Size = Byte($0A);
  strict private
    FForeColor: TdxAlphaColor;
    FBackgroundColor: TdxAlphaColor;
    FShadingPattern: SmallInt;
  protected
    function GetBackgroundColor: TdxAlphaColor;
    function GetForeColor: TdxAlphaColor;
    function GetShadingPattern: SmallInt;
    procedure SetBackgroundColor(const Value: TdxAlphaColor);
    procedure SetForeColor(const Value: TdxAlphaColor);
    procedure SetShadingPattern(const Value: SmallInt);
    procedure Read(const AData: TBytes; AStartIndex: Integer);
  public
    constructor Create;
    class function FromByteArray(const AData: TBytes; AStartIndex: Integer): TdxDocShadingDescriptor; static;
    procedure Write(AWriter: TBinaryWriter);

    property ForeColor: TdxAlphaColor read GetForeColor write SetForeColor;
    property BackgroundColor: TdxAlphaColor read GetBackgroundColor write SetBackgroundColor;
    property ShadingPattern: SmallInt read GetShadingPattern write SetShadingPattern;
  end;

  { TdxDocShadingDescriptor80 }

  TdxDocShadingDescriptor80 = class(TcxIUnknownObject, IdxDocShadingDescriptor)
  strict private
    FForeColor: TdxAlphaColor;
    FBackgroundColor: TdxAlphaColor;
    FShadingPattern: SmallInt;
  protected
    function GetBackgroundColor: TdxAlphaColor;
    function GetForeColor: TdxAlphaColor;
    function GetShadingPattern: SmallInt;
    procedure SetBackgroundColor(const Value: TdxAlphaColor);
    procedure SetForeColor(const Value: TdxAlphaColor);
    procedure SetShadingPattern(const Value: SmallInt);

    procedure Read(const AData: TBytes; AStartIndex: Integer);
  public
    constructor Create;
    class function FromByteArray(const AData: TBytes): TdxDocShadingDescriptor80; overload; static;
    class function FromByteArray(const AData: TBytes; AStartIndex: Integer): TdxDocShadingDescriptor80; overload; static;
    procedure Write(AWriter: TBinaryWriter);

    property ForeColor: TdxAlphaColor read GetForeColor write SetForeColor;
    property BackgroundColor: TdxAlphaColor read GetBackgroundColor write SetBackgroundColor;
    property ShadingPattern: SmallInt read GetShadingPattern write SetShadingPattern;
  end;

  { TdxDocumentProtectionTypeCalculator }

  TdxDocumentProtectionTypeCalculator = class
  public
    class function CalcDocumentProtectionType(AProtectionTypeCode: SmallInt): TdxDocumentProtectionType; static;
    class function CalcDocumentProtectionTypeCode(AType: TdxDocumentProtectionType): SmallInt; static;
    class function CalcRangePermissionProtectionType(AProtectionTypeCode: SmallInt): TdxDocumentProtectionType; static;
    class function CalcRangePermissionProtectionTypeCode(AType: TdxDocumentProtectionType): SmallInt; static;
  end;

  { TdxSectorHelper }

  TdxSectorHelper = class
  public
    class function GetBorders(AReader: TBinaryReader; AOffset: Integer): TArray<Integer>; static;
  end;

  TdxByteArrayHelper = class
  public
    class function From<T>(const AValue: T): TBytes; static; inline;
  end;

implementation

{ TdxDocConstants }

class function TdxDocConstants.GetColorIndex(AColor: TdxAlphaColor): Integer;
var
  I: Integer;
begin
  for I := Low(DefaultMSWordColor) to High(DefaultMSWordColor) do
    if DefaultMSWordColor[I] = AColor then
      Exit(I);
  Result := -1;
end;

{ TdxDocColorReference }

constructor TdxDocColorReference.Create;
begin
  FColor := TdxAlphaColors.Empty;
end;

class function TdxDocColorReference.FromByteArray(const AData: TBytes; AStartIndex: Integer): TdxDocColorReference;
begin
  Result := TdxDocColorReference.Create;
  Result.Read(AData, AStartIndex);
end;

procedure TdxDocColorReference.Read(const AData: TBytes; AStartIndex: Integer);
var
  ARed, AGreen, ABlue, AAuto: Integer;
begin
  ARed := AData[AStartIndex];
  AGreen := AData[AStartIndex + 1];
  ABlue := AData[AStartIndex + 2];
  AAuto := AData[AStartIndex + 3];
  if AAuto <> $FF then
    FColor := TdxAlphaColors.FromArgb(ARed, AGreen, ABlue);
end;

function TdxDocColorReference.GetBytes: TBytes;
var
  AAuto: Byte;
begin
  if TdxAlphaColors.IsTransparentOrEmpty(Color) then
    AAuto := $FF
  else
    AAuto := $00;
  Result := TBytes.Create(TdxAlphaColors.R(Color), TdxAlphaColors.G(Color), TdxAlphaColors.B(Color), AAuto);
end;

{ TdxTextRunBorder }

constructor TdxTextRunBorder.Create(AOffset: Integer; const AReason: TdxTextRunStartReasons);
begin
  FOffset := AOffset;
  FReason := AReason;
end;

{ TdxTextRunBorderComparable }

constructor TdxTextRunBorderComparable.Create(AOffset: Integer);
begin
  FOffset := AOffset;
end;

function TdxTextRunBorderComparable.CompareTo(const ATextRunBorder: TdxTextRunBorder): Integer;
begin
  Result := ATextRunBorder.Offset - Offset;
end;

{ TdxDocCharacterFormattingHelper }

class function TdxDocCharacterFormattingHelper.GetMergedCharacterFormattingInfo(AInfo: TdxDocCharacterFormattingInfo;
  AParentInfo: TdxCharacterFormattingInfo; ADocumentModel: TdxDocumentModel): TdxCharacterFormattingInfo;
begin
  Result := AParentInfo.Clone;
  Result.AllCaps := ConvertFromDocBoolWrapper(Result.AllCaps, AInfo.AllCaps);
  Result.FontBold := ConvertFromDocBoolWrapper(Result.FontBold, AInfo.FontBold);
  Result.FontItalic := ConvertFromDocBoolWrapper(Result.FontItalic, AInfo.FontItalic);
  Result.FontName := AInfo.FontName;
  Result.DoubleFontSize := AInfo.DoubleFontSize;
  Result.FontStrikeoutType := CalcStriceoutType(AInfo.Strike, AInfo.DoubleStrike, Result.FontStrikeoutType);
  Result.FontUnderlineType := AInfo.FontUnderlineType;

  Result.BackColor := AInfo.BackColor;
  Result.ForeColor := AInfo.ForeColor;
  Result.UnderlineColor := AInfo.UnderlineColor;
  Result.StrikeoutColor := AInfo.StrikeoutColor;

  Result.Hidden := ConvertFromDocBoolWrapper(Result.Hidden, AInfo.Hidden);
  Result.Script := AInfo.Script;
  Result.StrikeoutWordsOnly := AInfo.StrikeoutWordsOnly;
  Result.UnderlineWordsOnly := AInfo.UnderlineWordsOnly;
end;

class function TdxDocCharacterFormattingHelper.ConvertFromDocBoolWrapper(AValue: Boolean; ABoolWrapper: TdxDocBoolWrapper): Boolean;
begin
  case ABoolWrapper of
    TdxDocBoolWrapper.False:
      Result := False;
    TdxDocBoolWrapper.True:
      Result := True;
    TdxDocBoolWrapper.Leave:
      Result := AValue;
    TdxDocBoolWrapper.Inverse:
      Result := not AValue;
    else
      Result := False;
  end;
end;

class function TdxDocCharacterFormattingHelper.CalcStriceoutType(AStrike: TdxDocBoolWrapper;
  ADoubleStrike: TdxDocBoolWrapper; ABaseStrikeout: TdxStrikeoutType): TdxStrikeoutType;
begin
  if ADoubleStrike = TdxDocBoolWrapper.True then
    Exit(TdxStrikeoutType.Double);
  if AStrike = TdxDocBoolWrapper.True then
    Exit(TdxStrikeoutType.Single);
  if (AStrike = TdxDocBoolWrapper.Leave) or (ADoubleStrike = TdxDocBoolWrapper.Leave) then
    Exit(ABaseStrikeout);
  if (ADoubleStrike = TdxDocBoolWrapper.Inverse) and (ABaseStrikeout = TdxStrikeoutType.None) then
    Exit(TdxStrikeoutType.Double);
  if (AStrike = TdxDocBoolWrapper.Inverse) and (ABaseStrikeout = TdxStrikeoutType.None) then
    Exit(TdxStrikeoutType.Single);
  Result := TdxStrikeoutType.None;
end;

{ TdxAlignmentCalculator }

class function TdxAlignmentCalculator.CalcParagraphAlignment(AAlignmentCode: Byte): TdxParagraphAlignment;
begin
  case AAlignmentCode of
    0: Result := TdxParagraphAlignment.Left;
    1: Result := TdxParagraphAlignment.Center;
    2: Result := TdxParagraphAlignment.Right;
  else
    Result := TdxParagraphAlignment.Justify;
  end;
end;

class function TdxAlignmentCalculator.CalcParagraphAlignmentCode(AAlignment: TdxParagraphAlignment): Byte;
begin
  case AAlignment of
    TdxParagraphAlignment.Left:    Result := 0;
    TdxParagraphAlignment.Center:  Result := 1;
    TdxParagraphAlignment.Right:   Result := 2;
    TdxParagraphAlignment.Justify: Result := 3;
  else
    Result := 0;
  end;
end;

class function TdxAlignmentCalculator.CalcListNumberAlignment(AAlignmentCode: Byte): TdxListNumberAlignment;
begin
  case AAlignmentCode of
    0: Result := TdxListNumberAlignment.Left;
    1: Result := TdxListNumberAlignment.Center;
    2: Result := TdxListNumberAlignment.Right;
  else
    Result := TdxListNumberAlignment.Left;
  end;
end;

class function TdxAlignmentCalculator.CalcListNumberAlignmentCode(AAlignment: TdxListNumberAlignment): Byte;
begin
  case AAlignment of
    TdxListNumberAlignment.Left:   Result := 0;
    TdxListNumberAlignment.Center: Result := 1;
    TdxListNumberAlignment.Right:  Result := 2;
  else
    Result := 0;
  end;
end;

class function TdxAlignmentCalculator.CalcVerticalAlignment(AAlignmentTypeCode: Byte): TdxVerticalAlignment;
begin
  case AAlignmentTypeCode of
    0: Result := TdxVerticalAlignment.Top;
    1: Result := TdxVerticalAlignment.Center;
    2: Result := TdxVerticalAlignment.Bottom;
  else
    Result := TdxVerticalAlignment.Both;
  end;
end;

class function TdxAlignmentCalculator.CalcVerticalAlignmentTypeCode(AAlignment: TdxVerticalAlignment): Byte;
begin
  case AAlignment of
    TdxVerticalAlignment.Top:    Result := 0;
    TdxVerticalAlignment.Center: Result := 1;
    TdxVerticalAlignment.Bottom: Result := 2;
  else
    Result := 0;
  end;
end;

{ TdxNumberingFormatInfo }

constructor TdxNumberingFormatInfo.Create(AFormatCode: Byte; ANumberingFormat: TdxNumberingFormat);
begin
  FFormatCode := AFormatCode;
  FNumberingFormat := ANumberingFormat;
end;

{ TdxNumberingFormatCalculator }

class constructor TdxNumberingFormatCalculator.Initialize;
var
  ACount, I: Integer;
begin
  FInfos := TObjectList<TdxNumberingFormatInfo>.Create;
  FNumberingFormatCodes := TDictionary<TdxNumberingFormat, Byte>.Create;
  FNumberingFormats := TDictionary<Byte, TdxNumberingFormat>.Create;

  FInfos.Add(TdxNumberingFormatInfo.Create($00, TdxNumberingFormat.Decimal));
  FInfos.Add(TdxNumberingFormatInfo.Create($01, TdxNumberingFormat.UpperRoman));
  FInfos.Add(TdxNumberingFormatInfo.Create($02, TdxNumberingFormat.LowerRoman));
  FInfos.Add(TdxNumberingFormatInfo.Create($03, TdxNumberingFormat.UpperLetter));
  FInfos.Add(TdxNumberingFormatInfo.Create($04, TdxNumberingFormat.LowerLetter));
  FInfos.Add(TdxNumberingFormatInfo.Create($05, TdxNumberingFormat.Ordinal));
  FInfos.Add(TdxNumberingFormatInfo.Create($06, TdxNumberingFormat.CardinalText));
  FInfos.Add(TdxNumberingFormatInfo.Create($07, TdxNumberingFormat.OrdinalText));
  FInfos.Add(TdxNumberingFormatInfo.Create($08, TdxNumberingFormat.Hex));
  FInfos.Add(TdxNumberingFormatInfo.Create($09, TdxNumberingFormat.Chicago));
  FInfos.Add(TdxNumberingFormatInfo.Create($0a, TdxNumberingFormat.IdeographDigital));
  FInfos.Add(TdxNumberingFormatInfo.Create($0b, TdxNumberingFormat.JapaneseCounting));
  FInfos.Add(TdxNumberingFormatInfo.Create($0c, TdxNumberingFormat.AIUEOHiragana));
  FInfos.Add(TdxNumberingFormatInfo.Create($0d, TdxNumberingFormat.Iroha));
  FInfos.Add(TdxNumberingFormatInfo.Create($0e, TdxNumberingFormat.DecimalFullWidth));
  FInfos.Add(TdxNumberingFormatInfo.Create($0f, TdxNumberingFormat.DecimalHalfWidth));
  FInfos.Add(TdxNumberingFormatInfo.Create($10, TdxNumberingFormat.JapaneseLegal));
  FInfos.Add(TdxNumberingFormatInfo.Create($11, TdxNumberingFormat.JapaneseDigitalTenThousand));
  FInfos.Add(TdxNumberingFormatInfo.Create($12, TdxNumberingFormat.DecimalEnclosedCircle));
  FInfos.Add(TdxNumberingFormatInfo.Create($13, TdxNumberingFormat.DecimalFullWidth2));
  FInfos.Add(TdxNumberingFormatInfo.Create($14, TdxNumberingFormat.AIUEOFullWidthHiragana));
  FInfos.Add(TdxNumberingFormatInfo.Create($15, TdxNumberingFormat.IrohaFullWidth));
  FInfos.Add(TdxNumberingFormatInfo.Create($16, TdxNumberingFormat.DecimalZero));
  FInfos.Add(TdxNumberingFormatInfo.Create($17, TdxNumberingFormat.Bullet));
  FInfos.Add(TdxNumberingFormatInfo.Create($18, TdxNumberingFormat.Ganada));
  FInfos.Add(TdxNumberingFormatInfo.Create($19, TdxNumberingFormat.Chosung));
  FInfos.Add(TdxNumberingFormatInfo.Create($1a, TdxNumberingFormat.DecimalEnclosedFullstop));
  FInfos.Add(TdxNumberingFormatInfo.Create($1b, TdxNumberingFormat.DecimalEnclosedParentheses));
  FInfos.Add(TdxNumberingFormatInfo.Create($1c, TdxNumberingFormat.DecimalEnclosedCircleChinese));
  FInfos.Add(TdxNumberingFormatInfo.Create($1d, TdxNumberingFormat.IdeographEnclosedCircle));
  FInfos.Add(TdxNumberingFormatInfo.Create($1e, TdxNumberingFormat.IdeographTraditional));
  FInfos.Add(TdxNumberingFormatInfo.Create($1f, TdxNumberingFormat.IdeographZodiac));
  FInfos.Add(TdxNumberingFormatInfo.Create($20, TdxNumberingFormat.IdeographZodiacTraditional));
  FInfos.Add(TdxNumberingFormatInfo.Create($21, TdxNumberingFormat.TaiwaneseCounting));
  FInfos.Add(TdxNumberingFormatInfo.Create($22, TdxNumberingFormat.IdeographLegalTraditional));
  FInfos.Add(TdxNumberingFormatInfo.Create($23, TdxNumberingFormat.TaiwaneseCountingThousand));
  FInfos.Add(TdxNumberingFormatInfo.Create($24, TdxNumberingFormat.TaiwaneseDigital));
  FInfos.Add(TdxNumberingFormatInfo.Create($25, TdxNumberingFormat.ChineseCounting));
  FInfos.Add(TdxNumberingFormatInfo.Create($26, TdxNumberingFormat.ChineseLegalSimplified));
  FInfos.Add(TdxNumberingFormatInfo.Create($27, TdxNumberingFormat.ChineseCountingThousand));
  FInfos.Add(TdxNumberingFormatInfo.Create($28, TdxNumberingFormat.Decimal));
  FInfos.Add(TdxNumberingFormatInfo.Create($29, TdxNumberingFormat.KoreanDigital));
  FInfos.Add(TdxNumberingFormatInfo.Create($2a, TdxNumberingFormat.KoreanCounting));
  FInfos.Add(TdxNumberingFormatInfo.Create($2b, TdxNumberingFormat.KoreanLegal));
  FInfos.Add(TdxNumberingFormatInfo.Create($2c, TdxNumberingFormat.KoreanDigital2));
  FInfos.Add(TdxNumberingFormatInfo.Create($2d, TdxNumberingFormat.Hebrew1));
  FInfos.Add(TdxNumberingFormatInfo.Create($2e, TdxNumberingFormat.ArabicAlpha));
  FInfos.Add(TdxNumberingFormatInfo.Create($2f, TdxNumberingFormat.Hebrew2));
  FInfos.Add(TdxNumberingFormatInfo.Create($30, TdxNumberingFormat.ArabicAbjad));
  FInfos.Add(TdxNumberingFormatInfo.Create($31, TdxNumberingFormat.HindiVowels));
  FInfos.Add(TdxNumberingFormatInfo.Create($32, TdxNumberingFormat.HindiConsonants));
  FInfos.Add(TdxNumberingFormatInfo.Create($33, TdxNumberingFormat.HindiNumbers));
  FInfos.Add(TdxNumberingFormatInfo.Create($34, TdxNumberingFormat.HindiDescriptive));
  FInfos.Add(TdxNumberingFormatInfo.Create($35, TdxNumberingFormat.ThaiLetters));
  FInfos.Add(TdxNumberingFormatInfo.Create($36, TdxNumberingFormat.ThaiNumbers));
  FInfos.Add(TdxNumberingFormatInfo.Create($37, TdxNumberingFormat.ThaiDescriptive));
  FInfos.Add(TdxNumberingFormatInfo.Create($38, TdxNumberingFormat.VietnameseDescriptive));
  FInfos.Add(TdxNumberingFormatInfo.Create($39, TdxNumberingFormat.NumberInDash));
  FInfos.Add(TdxNumberingFormatInfo.Create($3a, TdxNumberingFormat.RussianLower));
  FInfos.Add(TdxNumberingFormatInfo.Create($3b, TdxNumberingFormat.RussianUpper));
  FInfos.Add(TdxNumberingFormatInfo.Create($ff, TdxNumberingFormat.None));
  ACount := FInfos.Count;
  for I := 0 to ACount - 1 do
  begin
    FNumberingFormats.Add(FInfos[I].FormatCode, FInfos[I].NumberingFormat);
    if not FNumberingFormatCodes.ContainsKey(FInfos[I].NumberingFormat) then
      FNumberingFormatCodes.Add(FInfos[I].NumberingFormat, FInfos[I].FormatCode);
  end;
end;

class destructor TdxNumberingFormatCalculator.Finalize;
begin
  FInfos.Free;
  FNumberingFormatCodes.Free;
  FNumberingFormats.Free;
end;

class function TdxNumberingFormatCalculator.CalcNumberingFormat(ANumberingFormatCode: Integer): TdxNumberingFormat;
begin
  if not FNumberingFormats.TryGetValue(Byte(ANumberingFormatCode), Result) then
    Result := TdxNumberingFormat.Decimal;
end;

class function TdxNumberingFormatCalculator.CalcNumberingFormatCode(ANumberingFormat: TdxNumberingFormat): Byte;
begin
  Result := FNumberingFormatCodes[ANumberingFormat];
end;

{ TdxWidthUnitCalculator }

class function TdxWidthUnitCalculator.CalcWidthUnitType(ATypeCode: Byte): TdxWidthUnitType;
begin
  case ATypeCode of
    $00: Result := TdxWidthUnitType.&Nil;
    $01: Result := TdxWidthUnitType.Auto;
    $02: Result := TdxWidthUnitType.FiftiethsOfPercent;
    $03: Result := TdxWidthUnitType.ModelUnits;
    $13: Result := TdxWidthUnitType.ModelUnits;
  else
    Result := TdxWidthUnitType.&Nil;
  end;
end;

class function TdxWidthUnitCalculator.CalcWidthUnitTypeCode(AUnitType: TdxWidthUnitType): Byte;
begin
  case AUnitType of
    TdxWidthUnitType.Auto:               Result := 1;
    TdxWidthUnitType.FiftiethsOfPercent: Result := 2;
    TdxWidthUnitType.ModelUnits:         Result := 3;
  else
    Result := 0;
  end;
end;

{ TdxMergingStateCalculator }

class function TdxMergingStateCalculator.CalcHorizontalMergingState(ATypeCode: Byte): TdxMergingState;
begin
  case ATypeCode of
    1:    Result := TdxMergingState.Restart;
    2, 3: Result := TdxMergingState.Continue;
  else
    Result := TdxMergingState.None;
  end;
end;

class function TdxMergingStateCalculator.CalcHorizontalMergingTypeCode(AMergingState: TdxMergingState): Byte;
begin
  case AMergingState of
    TdxMergingState.None:     Result := 0;
    TdxMergingState.Continue: Result := 3;
    TdxMergingState.Restart:  Result := 1;
  else
    Result := 0;
  end;
end;

class function TdxMergingStateCalculator.CalcVerticalMergingState(ATypeCode: Byte): TdxMergingState;
begin
  case ATypeCode of
    1:    Result := TdxMergingState.Continue;
    2, 3: Result := TdxMergingState.Restart;
  else
    Result := TdxMergingState.None;
  end;
end;

class function TdxMergingStateCalculator.CalcVerticalMergingTypeCode(AMergingState: TdxMergingState): Byte;
begin
  case AMergingState of
    TdxMergingState.None:     Result := 0;
    TdxMergingState.Continue: Result := 1;
    TdxMergingState.Restart:  Result := 3;
  else
    Result := 0;
  end;
end;

{ TdxTextDirectionCalculator }

class function TdxTextDirectionCalculator.CalcTextDirection(ATypeCode: Byte): TdxTextDirection;
begin
  case ATypeCode of
    1: Result := TdxTextDirection.TopToBottomRightToLeftRotated;
    3..5: Result := TdxTextDirection.LeftToRightTopToBottomRotated;
  else
    Result := TdxTextDirection.LeftToRightTopToBottom;
  end;
end;

{ TdxFootNotePositionCalculator }

class function TdxFootNotePositionCalculator.CalcFootNotePosition(ATypeCode: Integer): TdxFootNotePosition;
begin
  case ATypeCode of
    0: Result := TdxFootNotePosition.EndOfSection;
    1: Result := TdxFootNotePosition.BottomOfPage;
    2: Result := TdxFootNotePosition.BelowText;
    3: Result := TdxFootNotePosition.EndOfDocument;
  else
    Result := TdxFootNotePosition.BottomOfPage;
  end;
end;

class function TdxFootNotePositionCalculator.CalcFootNotePositionTypeCode(APosition: TdxFootNotePosition): Byte;
begin
  case APosition of
    TdxFootNotePosition.EndOfSection:  Result := 0;
    TdxFootNotePosition.BottomOfPage:  Result := 1;
    TdxFootNotePosition.BelowText:     Result := 2;
    TdxFootNotePosition.EndOfDocument: Result := 3;
  else
    Result := 0;
  end;
end;

class function TdxFootNotePositionCalculator.CalcFootNotePositionTypeCodeForDocumentProperties(APosition: TdxFootNotePosition): Byte;
begin
  case APosition of
    TdxFootNotePosition.EndOfSection: Result := 0;
    TdxFootNotePosition.BottomOfPage: Result := 1;
    TdxFootNotePosition.BelowText:    Result := 2;
  else
    Result := 0;
  end;
end;

{ TdxFootNoteNumberingRestartCalculator }

class function TdxFootNoteNumberingRestartCalculator.CalcFootNoteNumberingRestart(ATypeCode: Integer): TdxLineNumberingRestart;
begin
  case ATypeCode of
    0: Result := TdxLineNumberingRestart.Continuous;
    1: Result := TdxLineNumberingRestart.NewSection;
    2: Result := TdxLineNumberingRestart.NewPage;
  else
    Result := TdxLineNumberingRestart.Continuous;
  end;
end;

class function TdxFootNoteNumberingRestartCalculator.CalcFootNoteNumberingRestartTypeCode(ANumberingRestart: TdxLineNumberingRestart): Byte;
begin
  case ANumberingRestart of
    TdxLineNumberingRestart.Continuous: Result := 0;
    TdxLineNumberingRestart.NewSection: Result := 1;
    TdxLineNumberingRestart.NewPage:    Result := 2;
  else
    Result := 0;
  end;
end;

{ TdxDocFloatingObjectHorizontalPositionTypeCalculator }

class function TdxDocFloatingObjectHorizontalPositionTypeCalculator.CalcHorizontalPositionType97(ATypeCode: Integer): TdxFloatingObjectHorizontalPositionType;
begin
  case ATypeCode of
    1: Result := TdxFloatingObjectHorizontalPositionType.Page;
    2: Result := TdxFloatingObjectHorizontalPositionType.Column;
  else
    Result := TdxFloatingObjectHorizontalPositionType.Margin;
  end;
end;

class function TdxDocFloatingObjectHorizontalPositionTypeCalculator.CalcHorizontalPositionTypeCode97(AType: TdxFloatingObjectHorizontalPositionType): Byte;
begin
  case AType of
    TdxFloatingObjectHorizontalPositionType.Page:   Result := 1;
    TdxFloatingObjectHorizontalPositionType.Column: Result := 2;
  else
    Result := 0;
  end;
end;

{ TdxDocFloatingObjectVerticalPositionTypeCalculator }

class function TdxDocFloatingObjectVerticalPositionTypeCalculator.CalcVerticalPositionType97(ATypeCode: Integer): TdxFloatingObjectVerticalPositionType;
begin
  case ATypeCode of
    1: Result := TdxFloatingObjectVerticalPositionType.Page;
    2: Result := TdxFloatingObjectVerticalPositionType.Paragraph;
  else
    Result := TdxFloatingObjectVerticalPositionType.Margin;
  end;
end;

class function TdxDocFloatingObjectVerticalPositionTypeCalculator.CalcVerticalPositionTypeCode97(AType: TdxFloatingObjectVerticalPositionType): Byte;
begin
  case AType of
    TdxFloatingObjectVerticalPositionType.Page:      Result := 1;
    TdxFloatingObjectVerticalPositionType.Paragraph: Result := 2;
  else
    Result := 0;
  end;
end;

{ TdxDocFloatingObjectTextWrapTypeCalculator }

class function TdxDocFloatingObjectTextWrapTypeCalculator.CalcTextWrapType(ATypeCode: Integer): TdxFloatingObjectTextWrapType;
begin
  case ATypeCode of
    1: Result := TdxFloatingObjectTextWrapType.TopAndBottom;
    2: Result := TdxFloatingObjectTextWrapType.Square;
    3: Result := WrapTypeBehindText;
    4: Result := TdxFloatingObjectTextWrapType.Tight;
    5: Result := TdxFloatingObjectTextWrapType.Through;
  else
    Result := TdxFloatingObjectTextWrapType.None;
  end;
end;

class function TdxDocFloatingObjectTextWrapTypeCalculator.CalcTextWrapTypeCode(AType: TdxFloatingObjectTextWrapType): Byte;
begin
  case AType of
    TdxFloatingObjectTextWrapType.TopAndBottom: Result := 1;
    TdxFloatingObjectTextWrapType.Square:       Result := 2;
    TdxFloatingObjectTextWrapType.Tight:        Result := 4;
    TdxFloatingObjectTextWrapType.Through:      Result := 5;
  else
    if AType = WrapTypeBehindText then
      Result := 3
    else
      Result := 0;
  end;
end;

{ TdxDocFloatingObjectTextWrapSideCalculator }

class function TdxDocFloatingObjectTextWrapSideCalculator.CalcTextWrapSide(ATypeCode: Integer): TdxFloatingObjectTextWrapSide;
begin
  case ATypeCode of
    1: Result := TdxFloatingObjectTextWrapSide.Left;
    2: Result := TdxFloatingObjectTextWrapSide.Right;
    3: Result := TdxFloatingObjectTextWrapSide.Largest;
  else
    Result := TdxFloatingObjectTextWrapSide.Both;
  end;
end;

class function TdxDocFloatingObjectTextWrapSideCalculator.CalcTextWrapSideTypeCode(AWrapSide: TdxFloatingObjectTextWrapSide): Byte;
begin
  case AWrapSide of
    TdxFloatingObjectTextWrapSide.Both:    Result := 0;
    TdxFloatingObjectTextWrapSide.Left:    Result := 1;
    TdxFloatingObjectTextWrapSide.Right:   Result := 2;
    TdxFloatingObjectTextWrapSide.Largest: Result := 3;
  else
    Result := 0;
  end;
end;

{ TdxDocTableBorderColorReference }

class function TdxDocTableBorderColorReference.FromByteArray(const AData: TBytes; AStartIndex: Integer): TdxDocTableBorderColorReference;
begin
  Result := TdxDocTableBorderColorReference.Create;
  Result.Read(AData, AStartIndex);
end;

procedure TdxDocTableBorderColorReference.Read(const AData: TBytes; AStartIndex: Integer);
var
  ARed, AGreen, ABlue, AAuto: Integer;
begin
  ARed := AData[AStartIndex];
  AGreen := AData[AStartIndex + 1];
  ABlue := AData[AStartIndex + 2];
  AAuto := AData[AStartIndex + 3];
  if (AAuto = $FF) and (ARed = $FF) and (AGreen = $FF) and (ABlue = $FF) then
    FColor := TdxAlphaColors.Empty
  else
    FColor := TdxAlphaColors.FromArgb(ARed, AGreen, ABlue);
end;

function TdxDocTableBorderColorReference.GetBytes: TBytes;
var
  AAuto: Byte;
begin
  if TdxAlphaColors.IsTransparentOrEmpty(Color) then
    AAuto := $FF
  else
    AAuto := $00;
  Result := TBytes.Create(TdxAlphaColors.R(Color), TdxAlphaColors.G(Color), TdxAlphaColors.B(Color), AAuto);
end;

{ TdxDocShadingDescriptor }

constructor TdxDocShadingDescriptor.Create;
begin
  FForeColor := TdxAlphaColors.Empty;
  FBackgroundColor := TdxAlphaColors.Empty;
end;

class function TdxDocShadingDescriptor.FromByteArray(const AData: TBytes; AStartIndex: Integer): TdxDocShadingDescriptor;
begin
  Result := TdxDocShadingDescriptor.Create;
  Result.Read(AData, AStartIndex);
end;

function TdxDocShadingDescriptor.GetBackgroundColor: TdxAlphaColor;
begin
  Result := FBackgroundColor;
end;

function TdxDocShadingDescriptor.GetForeColor: TdxAlphaColor;
begin
  Result := FForeColor;
end;

function TdxDocShadingDescriptor.GetShadingPattern: SmallInt;
begin
  Result := FShadingPattern;
end;

procedure TdxDocShadingDescriptor.Read(const AData: TBytes; AStartIndex: Integer);
var
  ARed, AGreen, ABlue, AAuto: Integer;
begin
  ARed := AData[AStartIndex];
  AGreen := AData[AStartIndex + 1];
  ABlue := AData[AStartIndex + 2];
  AAuto := AData[AStartIndex + 3];
  if (AAuto <> $ff) then
    ForeColor := TdxAlphaColors.FromArgb(ARed, AGreen, ABlue)
  else
    ForeColor := TdxAlphaColors.Empty;
  ARed := AData[AStartIndex + 4];
  AGreen := AData[AStartIndex + 5];
  ABlue := AData[AStartIndex + 6];
  AAuto := AData[AStartIndex + 7];
  if (AAuto <> $ff) then
    BackgroundColor := TdxAlphaColors.FromArgb(ARed, AGreen, ABlue)
  else
    BackgroundColor := TdxAlphaColors.Empty;
  ShadingPattern := PSmallInt(@AData[AStartIndex + 8])^;
end;

procedure TdxDocShadingDescriptor.SetBackgroundColor(const Value: TdxAlphaColor);
begin
  FBackgroundColor := Value;
end;

procedure TdxDocShadingDescriptor.SetForeColor(const Value: TdxAlphaColor);
begin
  FForeColor := Value;
end;

procedure TdxDocShadingDescriptor.SetShadingPattern(const Value: SmallInt);
begin
  FShadingPattern := Value;
end;

procedure TdxDocShadingDescriptor.Write(AWriter: TBinaryWriter);
var
  AAuto: Byte;
begin
  Assert(AWriter <> nil, 'writer');

  if TdxAlphaColors.IsTransparentOrEmpty(ForeColor) then
    AAuto := $FF
  else
    AAuto := $00;
  AWriter.Write(TBytes.Create(TdxAlphaColors.R(ForeColor), TdxAlphaColors.G(ForeColor), TdxAlphaColors.B(ForeColor), AAuto));

  if TdxAlphaColors.IsTransparentOrEmpty(BackgroundColor) then
    AAuto := $FF
  else
    AAuto := $00;
  AWriter.Write(TBytes.Create(TdxAlphaColors.R(BackgroundColor), TdxAlphaColors.G(BackgroundColor), TdxAlphaColors.B(BackgroundColor), AAuto));
  AWriter.Write(ShadingPattern);
end;

{ TdxDocShadingDescriptor80 }

constructor TdxDocShadingDescriptor80.Create;
begin
  FForeColor := TdxAlphaColors.Empty;
  FBackgroundColor := TdxAlphaColors.Empty;
end;

class function TdxDocShadingDescriptor80.FromByteArray(const AData: TBytes): TdxDocShadingDescriptor80;
begin
  Result := FromByteArray(AData, 0);
end;

class function TdxDocShadingDescriptor80.FromByteArray(const AData: TBytes; AStartIndex: Integer): TdxDocShadingDescriptor80;
begin
  Result := TdxDocShadingDescriptor80.Create;
  Result.Read(AData, AStartIndex);
end;

function TdxDocShadingDescriptor80.GetBackgroundColor: TdxAlphaColor;
begin
  Result := FBackgroundColor;
end;

function TdxDocShadingDescriptor80.GetForeColor: TdxAlphaColor;
begin
  Result := FForeColor;
end;

function TdxDocShadingDescriptor80.GetShadingPattern: SmallInt;
begin
  Result := FShadingPattern;
end;

procedure TdxDocShadingDescriptor80.SetBackgroundColor(const Value: TdxAlphaColor);
begin
  FBackgroundColor := Value;
end;

procedure TdxDocShadingDescriptor80.SetForeColor(const Value: TdxAlphaColor);
begin
  FForeColor := Value;
end;

procedure TdxDocShadingDescriptor80.SetShadingPattern(const Value: SmallInt);
begin
  FShadingPattern := Value;
end;

procedure TdxDocShadingDescriptor80.Read(const AData: TBytes; AStartIndex: Integer);
var
  AInfo: SmallInt;
  AColorIndex: Byte;
begin
  AInfo := PSmallInt(@AData[AStartIndex])^;
  AColorIndex := AInfo and $1F;

  if AColorIndex < Length(TdxDocConstants.DefaultMSWordColor) then
    if (AColorIndex <> 0) then
      ForeColor := TdxDocConstants.DefaultMSWordColor[AColorIndex]
    else
      ForeColor := TdxAlphaColors.Empty;
  AColorIndex := (AInfo and $03E0) shr 5;

  if AColorIndex < Length(TdxDocConstants.DefaultMSWordColor) then
    if (AColorIndex <> 0) then
      BackgroundColor := TdxDocConstants.DefaultMSWordColor[AColorIndex]
    else
      BackgroundColor := TdxAlphaColors.Empty;
  ShadingPattern := (AInfo and $FC00) shr 10;
end;

procedure TdxDocShadingDescriptor80.Write(AWriter: TBinaryWriter);
var
  AInfo: SmallInt;
  AColorIndex: Integer;
begin
  Assert(AWriter <> nil, 'writer');

  AColorIndex := TdxDocConstants.GetColorIndex(ForeColor);
  if AColorIndex < 0 then
    AColorIndex := 0;
  AInfo := AColorIndex;

  AColorIndex := TdxDocConstants.GetColorIndex(BackgroundColor);
  if AColorIndex < 0 then
    AColorIndex := 0;

  AInfo := SmallInt(Word(AInfo) or (AColorIndex shl 5));
  AInfo := SmallInt(Word(AInfo) or Word(ShadingPattern));
  AWriter.Write(AInfo);
end;

{ TdxDocumentProtectionTypeCalculator }

class function TdxDocumentProtectionTypeCalculator.CalcDocumentProtectionType(AProtectionTypeCode: SmallInt): TdxDocumentProtectionType;
begin
  case AProtectionTypeCode of
    $3: Result := TdxDocumentProtectionType.ReadOnly;
    else
      Result := TdxDocumentProtectionType.None;
  end;
end;

class function TdxDocumentProtectionTypeCalculator.CalcDocumentProtectionTypeCode(AType: TdxDocumentProtectionType): SmallInt;
begin
  case AType of
    TdxDocumentProtectionType.None:          Result := $7;
    else
      Result := $3;
  end;
end;

class function TdxDocumentProtectionTypeCalculator.CalcRangePermissionProtectionType(AProtectionTypeCode: SmallInt): TdxDocumentProtectionType;
begin
  case AProtectionTypeCode of
    $0004: Result := TdxDocumentProtectionType.ReadOnly;
    else
      Result := TdxDocumentProtectionType.None;
  end;
end;

class function TdxDocumentProtectionTypeCalculator.CalcRangePermissionProtectionTypeCode(AType: TdxDocumentProtectionType): SmallInt;
begin
  case AType of
    TdxDocumentProtectionType.None:          Result := $0001;
    TdxDocumentProtectionType.ReadOnly:      Result := $0004;
    else
      Result := $0000;
  end;
end;

{ TdxSectorHelper }

class function TdxSectorHelper.GetBorders(AReader: TBinaryReader; AOffset: Integer): TArray<Integer>;
var
  I: Integer;
begin
  if AOffset + TdxDocConstants.LastByteOffsetInSector >= AReader.BaseStream.Size then
    Exit(TArray<Integer>.Create(0));
  AReader.BaseStream.Seek(AOffset + TdxDocConstants.LastByteOffsetInSector, TSeekOrigin.soBeginning);
  SetLength(Result, AReader.ReadByte + 1);
  AReader.BaseStream.Seek(AOffset, TSeekOrigin.soBeginning);
  for I := 0 to Length(Result) - 1 do
    Result[I] := AReader.ReadInt32;
end;

{ TdxByteArrayHelper }

class function TdxByteArrayHelper.From<T>(const AValue: T): TBytes;
begin
  SetLength(Result, SizeOf(T));
  Move(AValue, Result[0], SizeOf(T));
end;

end.
