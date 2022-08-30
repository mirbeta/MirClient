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

unit dxRichEdit.DocumentModel.NumberingFormatting;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, Classes, SysUtils, Graphics, Generics.Defaults, Generics.Collections,
  dxCoreClasses, dxCoreGraphics, dxCultureInfo,
  dxRichEdit.NativeApi,
  dxRichEdit.Utils.Types,
  dxGenerics,
  dxRichEdit.Utils.ModelUnitConverter,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.ParagraphFormatting,
  dxRichEdit.DocumentModel.CharacterFormatting,
  dxRichEdit.DocumentModel.IndexBasedObject,
  dxRichEdit.DocumentModel.History.IndexChangedHistoryItem,
  dxRichEdit.DocumentModel.UnitConverter;

type
  TdxNumberingFormat = TdxRichEditNumberingFormat;
  TdxNumberingType = TdxRichEditNumberingType;

  TdxListNumberAlignment = (
    Left,
    Center,
    Right);


  TdxListLevelChangeType = (
    None,
    Start,
    Format,
    Alignment,
    ConvertPreviousLevelNumberingToDecimal,
    Separator,
    SuppressRestart,
    SuppressBulletResize,
    DisplayFormatString,
    RelativeRestartLevel,
    TemplateCode,
    Legacy,
    LegacySpace,
    LegacyIndent,
    BatchUpdate
  );

  IdxReadOnlyListLevelProperties = interface
    function GetStart: Integer;
    function GetFormat: TdxNumberingFormat;
    function GetAlignment: TdxListNumberAlignment;
    function GetConvertPreviousLevelNumberingToDecimal: Boolean;
    function GetSeparator: Char;
    function GetSuppressRestart: Boolean;
    function GetSuppressBulletResize: Boolean;
    function GetDisplayFormatString: string;
    function GetRelativeRestartLevel: Integer;
    function GetTemplateCode: Integer;
    function GetOriginalLeftIndent: Integer;
    function GetLegacy: Boolean;
    function GetLegacyIndent: Integer;
    function GetLegacySpace: Integer;

    property Start: Integer read GetStart;
    property Format: TdxNumberingFormat read GetFormat;
    property Alignment: TdxListNumberAlignment read GetAlignment;
    property ConvertPreviousLevelNumberingToDecimal: Boolean read GetConvertPreviousLevelNumberingToDecimal;
    property Separator: Char read GetSeparator;
    property SuppressRestart: Boolean read GetSuppressRestart;
    property SuppressBulletResize: Boolean read GetSuppressBulletResize;
    property DisplayFormatString: string read GetDisplayFormatString;
    property RelativeRestartLevel: Integer read GetRelativeRestartLevel;
    property TemplateCode: Integer read GetTemplateCode;
    property OriginalLeftIndent: Integer read GetOriginalLeftIndent;
    property Legacy: Boolean read GetLegacy;
    property LegacyIndent: Integer read GetLegacyIndent;
    property LegacySpace: Integer read GetLegacySpace;
  end;

  IdxListLevelProperties = interface(IdxReadOnlyListLevelProperties)
    procedure SetStart(const Value: Integer);
    procedure SetFormat(const Value: TdxNumberingFormat);
    procedure SetAlignment(const Value: TdxListNumberAlignment);
    procedure SetConvertPreviousLevelNumberingToDecimal(const Value: Boolean);
    procedure SetSeparator(const Value: Char);
    procedure SetSuppressRestart(const Value: Boolean);
    procedure SetSuppressBulletResize(const Value: Boolean);
    procedure SetDisplayFormatString(const Value: string);
    procedure SetRelativeRestartLevel(const Value: Integer);
    procedure SetTemplateCode(const Value: Integer);
    procedure SetLegacy(const Value: Boolean);
    procedure SetLegacyIndent(const Value: Integer);
    procedure SetLegacySpace(const Value: Integer);

    property Start: Integer read GetStart write SetStart;
    property Format: TdxNumberingFormat read GetFormat write SetFormat;
    property Alignment: TdxListNumberAlignment read GetAlignment write SetAlignment;
    property ConvertPreviousLevelNumberingToDecimal: Boolean read GetConvertPreviousLevelNumberingToDecimal write SetConvertPreviousLevelNumberingToDecimal;
    property Separator: Char read GetSeparator write SetSeparator;
    property SuppressRestart: Boolean read GetSuppressRestart write SetSuppressRestart;
    property SuppressBulletResize: Boolean read GetSuppressBulletResize write SetSuppressBulletResize;
    property DisplayFormatString: string read GetDisplayFormatString write SetDisplayFormatString;
    property RelativeRestartLevel: Integer read GetRelativeRestartLevel write SetRelativeRestartLevel;
    property TemplateCode: Integer read GetTemplateCode write SetTemplateCode;
    property Legacy: Boolean read GetLegacy write SetLegacy;
    property LegacyIndent: Integer read GetLegacyIndent write SetLegacyIndent;
    property LegacySpace: Integer read GetLegacySpace write SetLegacySpace;
  end;

  { TdxListLevelInfo }

  TdxListLevelInfo = class(TdxCloneableIUnknownObject,
    IdxListLevelProperties,
    IdxReadOnlyListLevelProperties)
  private
    FStart: Integer;
    FFormat: TdxNumberingFormat;
    FAlignment: TdxListNumberAlignment;
    FConvertPreviousLevelNumberingToDecimal: Boolean;
    FSeparator: Char;
    FSuppressRestart: Boolean;
    FSuppressBulletResize: Boolean;
    FDisplayFormatString: string;
    FRelativeRestartLevel: Integer;
    FTemplateCode: Integer;
    FOriginalLeftIndent: Integer;
    FLegacy: Boolean;
    FLegacySpace: Integer;
    FLegacyIndent: Integer;
    function GetHasTemplateCode: Boolean;
    function GetStart: Integer;
    function GetFormat: TdxNumberingFormat;
    function GetAlignment: TdxListNumberAlignment;
    function GetConvertPreviousLevelNumberingToDecimal: Boolean;
    function GetSeparator: Char;
    function GetSuppressRestart: Boolean;
    function GetSuppressBulletResize: Boolean;
    function GetDisplayFormatString: string;
    function GetRelativeRestartLevel: Integer;
    function GetTemplateCode: Integer;
    function GetOriginalLeftIndent: Integer;
    function GetLegacy: Boolean;
    function GetLegacyIndent: Integer;
    function GetLegacySpace: Integer;
    procedure SetStart(const Value: Integer);
    procedure SetFormat(const Value: TdxNumberingFormat);
    procedure SetAlignment(const Value: TdxListNumberAlignment);
    procedure SetConvertPreviousLevelNumberingToDecimal(const Value: Boolean);
    procedure SetSeparator(const Value: Char);
    procedure SetSuppressRestart(const Value: Boolean);
    procedure SetSuppressBulletResize(const Value: Boolean);
    procedure SetDisplayFormatString(const Value: string);
    procedure SetRelativeRestartLevel(const Value: Integer);
    procedure SetTemplateCode(const Value: Integer);
    procedure SetLegacy(const Value: Boolean);
    procedure SetLegacyIndent(const Value: Integer);
    procedure SetLegacySpace(const Value: Integer);
  public
    function Clone: TdxListLevelInfo; reintroduce; inline;
    procedure CopyFrom(Source: TdxCloneable); override;
    procedure InitializeAsDefault;
    function Equals(Obj: TObject): Boolean; override;

    property Start: Integer read FStart write FStart;
    property Format: TdxNumberingFormat read FFormat write FFormat;
    property Alignment: TdxListNumberAlignment read FAlignment write FAlignment;
    property ConvertPreviousLevelNumberingToDecimal: Boolean read FConvertPreviousLevelNumberingToDecimal
      write FConvertPreviousLevelNumberingToDecimal;
    property Separator: Char read FSeparator write FSeparator;
    property SuppressRestart: Boolean read FSuppressRestart write FSuppressRestart;
    property SuppressBulletResize: Boolean read FSuppressBulletResize write FSuppressBulletResize;
    property DisplayFormatString: string read FDisplayFormatString write FDisplayFormatString;
    property RelativeRestartLevel: Integer read FRelativeRestartLevel write FRelativeRestartLevel;
    property TemplateCode: Integer read FTemplateCode write FTemplateCode;
    property OriginalLeftIndent: Integer read FOriginalLeftIndent write FOriginalLeftIndent;
    property HasTemplateCode: Boolean read GetHasTemplateCode;
    property Legacy: Boolean read FLegacy write FLegacy;
    property LegacySpace: Integer read FLegacySpace write FLegacySpace;
    property LegacyIndent: Integer read FLegacyIndent write FLegacyIndent;
  end;

  { TdxListLevelInfoCache }

  TdxListLevelInfoCache = class(TdxUniqueItemsCache<TdxListLevelInfo>)
  public
    function CreateDefaultItem(const AUnitConverter: IdxDocumentModelUnitConverter): TdxListLevelInfo; override;
  end;

  { TdxListLevelChangeActionsCalculator }

  TdxListLevelChangeActionsCalculator = class abstract
  public
    class function CalculateChangeActions(AChange: TdxListLevelChangeType): TdxDocumentModelChangeActions; static;
  end;

  { TdxListLevelProperties }

  TdxListLevelProperties = class(TdxRichEditIndexBasedObject<TdxListLevelInfo>,
      IdxListLevelProperties,
      IdxReadOnlyListLevelProperties
    )
  private
    function GetAlignment: TdxListNumberAlignment;
    function GetConvertPreviousLevelNumberingToDecimal: Boolean;
    function GetDisplayFormatString: string;
    function GetFormat: TdxNumberingFormat;
    function GetLegacy: Boolean;
    function GetLegacyIndent: Integer;
    function GetLegacySpace: Integer;
    function GetOriginalLeftIndent: Integer;
    function GetRelativeRestartLevel: Integer;
    function GetSeparator: Char;
    function GetStart: Integer;
    function GetSuppressBulletResize: Boolean;
    function GetSuppressRestart: Boolean;
    function GetTemplateCode: Integer;
    procedure SetAlignment(const Value: TdxListNumberAlignment);
    procedure SetConvertPreviousLevelNumberingToDecimal(const Value: Boolean);
    procedure SetDisplayFormatString(const Value: string);
    procedure SetFormat(const Value: TdxNumberingFormat);
    procedure SetLegacy(const Value: Boolean);
    procedure SetLegacyIndent(const Value: Integer);
    procedure SetLegacySpace(const Value: Integer);
    procedure InternalSetOriginalLeftIndent(const Value: Integer);
    procedure SetRelativeRestartLevel(const Value: Integer);
    procedure SetSeparator(const Value: Char);
    procedure SetStart(const Value: Integer);
    procedure SetSuppressBulletResize(const Value: Boolean);
    procedure SetSuppressRestart(const Value: Boolean);
    procedure IdxListLevelProperties.SetTemplateCode = SetTemplateCodeIntf;
    procedure SetTemplateCodeIntf(const Value: Integer); overload;
  strict protected
    function SetStartCore(const AListLevel: TdxListLevelInfo;
      const AValue: Integer): TdxDocumentModelChangeActions; virtual;
    function SetFormatCore(const AListLevel: TdxListLevelInfo;
      const AValue: TdxNumberingFormat): TdxDocumentModelChangeActions; virtual;
    function SetAlignmentCore(const AListLevel: TdxListLevelInfo;
      const AValue: TdxListNumberAlignment): TdxDocumentModelChangeActions; virtual;
    function SetConvertPreviousLevelNumberingToDecimalCore(const AListLevel: TdxListLevelInfo;
      const AValue: Boolean): TdxDocumentModelChangeActions; virtual;
    function SetSeparatorCore(const AListLevel: TdxListLevelInfo;
      const AValue: Char): TdxDocumentModelChangeActions; virtual;
    function SetSuppressRestartCore(const AListLevel: TdxListLevelInfo;
      const AValue: Boolean): TdxDocumentModelChangeActions; virtual;
    function SetSuppressBulletResizeCore(const AListLevel: TdxListLevelInfo;
      const AValue: Boolean): TdxDocumentModelChangeActions; virtual;
    function SetDisplayFormatStringCore(const AListLevel: TdxListLevelInfo;
      const AValue: string): TdxDocumentModelChangeActions; virtual;
    function SetRestartLevelCore(const AListLevel: TdxListLevelInfo;
      const AValue: Integer): TdxDocumentModelChangeActions; virtual;
    function SetTemplateCode(const AListLevel: TdxListLevelInfo;
      const AValue: Integer): TdxDocumentModelChangeActions; overload; virtual;
    function SetOriginalLeftIndent(const AListLevel: TdxListLevelInfo;
      const AValue: Integer): TdxDocumentModelChangeActions; overload; virtual;
    function SetLegacyCore(const AListLevel: TdxListLevelInfo;
      const AValue: Boolean): TdxDocumentModelChangeActions; virtual;
    function SetLegacySpaceCore(const AListLevel: TdxListLevelInfo;
      const AValue: Integer): TdxDocumentModelChangeActions; virtual;
    function SetLegacyIndentCore(const AListLevel: TdxListLevelInfo;
      const AValue: Integer): TdxDocumentModelChangeActions; virtual;
    function GetCache(const ADocumentModel: TdxCustomDocumentModel): TdxUniqueItemsCache<TdxListLevelInfo>; override;
    function GetBatchUpdateChangeActions: TdxDocumentModelChangeActions; override;
  public
    constructor Create(ADocumentModel: TdxCustomDocumentModel); reintroduce;
    function Clone: TdxListLevelProperties;

    property Start: Integer read GetStart write SetStart;
    property Format: TdxNumberingFormat read GetFormat write SetFormat;
    property Alignment: TdxListNumberAlignment read GetAlignment write SetAlignment;
    property ConvertPreviousLevelNumberingToDecimal: Boolean
      read GetConvertPreviousLevelNumberingToDecimal write SetConvertPreviousLevelNumberingToDecimal;
    property Separator: Char read GetSeparator write SetSeparator;
    property SuppressRestart: Boolean read GetSuppressRestart write SetSuppressRestart;
    property SuppressBulletResize: Boolean read GetSuppressBulletResize write SetSuppressBulletResize;
    property DisplayFormatString: string read GetDisplayFormatString write SetDisplayFormatString;
    property OriginalLeftIndent: Integer read GetOriginalLeftIndent write InternalSetOriginalLeftIndent;
    property RelativeRestartLevel: Integer read GetRelativeRestartLevel write SetRelativeRestartLevel;
    property TemplateCode: Integer read GetTemplateCode write SetTemplateCodeIntf;
    property Legacy: Boolean read GetLegacy write SetLegacy;
    property LegacySpace: Integer read GetLegacySpace write SetLegacySpace;
    property LegacyIndent: Integer read GetLegacyIndent write SetLegacyIndent;
  end;

  { TdxOrdinalBasedNumberConverter }

  TdxOrdinalBasedNumberConverter = class abstract
  strict protected
    function GetType: TdxNumberingFormat; virtual; abstract;
    function GetMinValue: Int64; virtual;
    function GetMaxValue: Int64; virtual;
    function ConvertNumberCore(AValue: Int64): string; virtual; abstract;

    property &Type: TdxNumberingFormat read GetType;
  public
    function ConvertNumber(AValue: Int64): string;
    class function GetOrdinalNumberConverterByLanguage(ALanguageId: Integer): TdxOrdinalBasedNumberConverter;
    class function GetDescriptiveCardinalNumberConverterByLanguage(ALanguageId: Integer): TdxOrdinalBasedNumberConverter;
    class function GetDescriptiveOrdinalNumberConverterByLanguage(ALanguageId: Integer): TdxOrdinalBasedNumberConverter;
    class function CreateConverter(AFormat: TdxNumberingFormat; ALanguageId: Integer): TdxOrdinalBasedNumberConverter;
    class function GetSupportNumberingFormat: TdxOrdinalList<TdxNumberingFormat>;

    property MinValue: Int64 read GetMinValue;
    property MaxValue: Int64 read GetMaxValue;
  end;

  { TdxDecimalNumberConverter }

  TdxDecimalNumberConverter = class(TdxOrdinalBasedNumberConverter)
  strict protected
    function ConvertNumberCore(AValue: Int64): string; override;
    function GetType: TdxNumberingFormat; override;
  end;

  { TdxAlphabetBasedNumberConverter }

  TdxAlphabetBasedNumberConverter = class abstract(TdxOrdinalBasedNumberConverter)
  strict protected
    function ConvertNumberCore(AValue: Int64): string; override;
    function GetAlphabet: TArray<Char>; virtual; abstract;
    function GetAlphabetSize: Integer; virtual; abstract;
    function GetMinValue: Int64; override;
    function GetMaxValue: Int64; override;

    property Alphabet: TArray<Char> read GetAlphabet;
    property AlphabetSize: Integer read GetAlphabetSize;
  end;

  { TdxUpperLatinLetterNumberConverter }

  TdxUpperLatinLetterNumberConverter = class(TdxAlphabetBasedNumberConverter)
  strict private
    class var FUpperLetter: TArray<Char>;
    class constructor Initialize;
  strict protected
    function GetAlphabet: TArray<Char>; override;
    function GetAlphabetSize: Integer; override;
    function GetType: TdxNumberingFormat; override;
  end;

  { TdxLowerLatinLetterNumberConverter }

  TdxLowerLatinLetterNumberConverter = class(TdxAlphabetBasedNumberConverter)
  strict private
    class var FLowerLetter: TArray<Char>;
    class constructor Initialize;
  strict protected
    function GetAlphabet: TArray<Char>; override;
    function GetAlphabetSize: Integer; override;
    function GetType: TdxNumberingFormat; override;
  end;

  { TdxRomanNumberConverter }

  TdxRomanNumberConverter = class abstract (TdxOrdinalBasedNumberConverter)
  strict protected
    function GetArabic: TArray<Integer>; virtual; abstract;
    function GetRoman: TArray<string>; virtual; abstract;
    function GetMinValue: Int64; override;
    function ConvertNumberCore(AValue: Int64): string; override;

    property Arabic: TArray<Integer> read GetArabic;
    property Roman: TArray<string> read GetRoman;
  end;

  { TdxUpperRomanNumberConverterClassic }

  TdxUpperRomanNumberConverterClassic = class(TdxRomanNumberConverter)
  strict private
    class var FArabic: TArray<Integer>;
    class var FRoman: TArray<string>;
    class constructor Initialize;
  strict protected
    function GetArabic: TArray<Integer>; override;
    function GetRoman: TArray<string>; override;
    function GetType: TdxNumberingFormat; override;
  end;

  { TdxLowerRomanNumberConverter }

  TdxLowerRomanNumberConverterClassic = class(TdxRomanNumberConverter)
  strict private
    class var FArabic: TArray<Integer>;
    class var FRoman: TArray<string>;
    class constructor Initialize;
  strict protected
    function GetArabic: TArray<Integer>; override;
    function GetRoman: TArray<string>; override;
    function GetType: TdxNumberingFormat; override;
  end;

  { TdxNumberInDashNumberConverter }

  TdxNumberInDashNumberConverter = class(TdxOrdinalBasedNumberConverter)
  strict protected
    function ConvertNumberCore(AValue: Int64): string; override;
    function GetType: TdxNumberingFormat; override;
  end;

  { TdxDecimalZeroNumberConverter }

  TdxDecimalZeroNumberConverter = class(TdxOrdinalBasedNumberConverter)
  strict protected
    function ConvertNumberCore(AValue: Int64): string; override;
    function GetType: TdxNumberingFormat; override;
  end;

  { TdxBulletNumberConverter }

  TdxBulletNumberConverter = class(TdxOrdinalBasedNumberConverter)
  strict protected
    function ConvertNumberCore(AValue: Int64): string; override;
    function GetType: TdxNumberingFormat; override;
  end;

  { TdxRussianUpperNumberConverter }

  TdxRussianUpperNumberConverter = class(TdxAlphabetBasedNumberConverter)
  strict private
    class var FRussianUpper: TArray<Char>;
    class constructor Initialize;
  strict protected
    function GetAlphabet: TArray<Char>; override;
    function GetAlphabetSize: Integer; override;
    function GetType: TdxNumberingFormat; override;
  end;

  { TdxRussianLowerNumberConverter }

  TdxRussianLowerNumberConverter = class(TdxAlphabetBasedNumberConverter)
  strict private
    class var FRussianLower: TArray<Char>;
    class constructor Initialize;
  strict protected
    function GetAlphabet: TArray<Char>; override;
    function GetAlphabetSize: Integer; override;
    function GetType: TdxNumberingFormat; override;
  end;

  { TdxDecimalEnclosedParenthesesNumberConverter }

  TdxDecimalEnclosedParenthesesNumberConverter = class(TdxOrdinalBasedNumberConverter)
  strict protected
    function ConvertNumberCore(AValue: Int64): string; override;
    function GetType: TdxNumberingFormat; override;
  end;

implementation

uses
  RTLConsts, StrUtils, Math, dxCore,

  dxRichEdit.Utils.BatchUpdateHelper,
  dxRichEdit.DocumentModel.Cache,
  dxRichEdit.DocumentModel.Cache.Simple,
  dxRichEdit.Utils.Exceptions,
  dxCharacters;

{ TdxListLevelInfo }

function TdxListLevelInfo.Clone: TdxListLevelInfo;
begin
  Result := TdxListLevelInfo(inherited Clone);
end;

procedure TdxListLevelInfo.CopyFrom(Source: TdxCloneable);
var
  AInfo: TdxListLevelInfo absolute Source;
begin
  Start := AInfo.Start;
  Format := AInfo.Format;
  Alignment := AInfo.Alignment;
  ConvertPreviousLevelNumberingToDecimal := AInfo.ConvertPreviousLevelNumberingToDecimal;
  Separator := AInfo.Separator;
  SuppressRestart := AInfo.SuppressRestart;
  SuppressBulletResize := AInfo.SuppressBulletResize;
  DisplayFormatString := AInfo.DisplayFormatString;
  RelativeRestartLevel := AInfo.RelativeRestartLevel;
  FTemplateCode := AInfo.TemplateCode;
  OriginalLeftIndent := AInfo.OriginalLeftIndent;
  Legacy := AInfo.Legacy;
  LegacySpace := AInfo.LegacySpace;
  LegacyIndent := AInfo.LegacyIndent;
end;

procedure TdxListLevelInfo.InitializeAsDefault;
begin
  Start := 1;
  Separator := TdxCharacters.TabMark;
  DisplayFormatString := '%0:s.';
  RelativeRestartLevel := 0;
end;

procedure TdxListLevelInfo.SetAlignment(const Value: TdxListNumberAlignment);
begin
  FAlignment := Value;
end;

procedure TdxListLevelInfo.SetConvertPreviousLevelNumberingToDecimal(const Value: Boolean);
begin
  FConvertPreviousLevelNumberingToDecimal := Value;
end;

procedure TdxListLevelInfo.SetDisplayFormatString(const Value: string);
begin
  FDisplayFormatString := Value;
end;

procedure TdxListLevelInfo.SetFormat(const Value: TdxNumberingFormat);
begin
  FFormat := Value;
end;

procedure TdxListLevelInfo.SetLegacy(const Value: Boolean);
begin
  FLegacy := Value;
end;

procedure TdxListLevelInfo.SetLegacyIndent(const Value: Integer);
begin
  FLegacyIndent := Value;
end;

procedure TdxListLevelInfo.SetLegacySpace(const Value: Integer);
begin
  FLegacySpace := Value;
end;

procedure TdxListLevelInfo.SetRelativeRestartLevel(const Value: Integer);
begin
  FRelativeRestartLevel := Value;
end;

procedure TdxListLevelInfo.SetSeparator(const Value: Char);
begin
  FSeparator := Value;
end;

procedure TdxListLevelInfo.SetStart(const Value: Integer);
begin
  FStart := Value;
end;

procedure TdxListLevelInfo.SetSuppressBulletResize(const Value: Boolean);
begin
   FSuppressBulletResize := Value;
end;

procedure TdxListLevelInfo.SetSuppressRestart(const Value: Boolean);
begin
  FSuppressRestart := Value;
end;

procedure TdxListLevelInfo.SetTemplateCode(const Value: Integer);
begin
  FTemplateCode := Value;
end;

function TdxListLevelInfo.Equals(Obj: TObject): Boolean;
var
  AInfo: TdxListLevelInfo;
begin
  AInfo := TdxListLevelInfo(Obj);
  Result := (Start = AInfo.Start) and
    (Format = AInfo.Format) and
    (Alignment = AInfo.Alignment) and
    (ConvertPreviousLevelNumberingToDecimal = AInfo.ConvertPreviousLevelNumberingToDecimal) and
    (Separator = AInfo.Separator) and
    (SuppressRestart = AInfo.SuppressRestart) and
    (SuppressBulletResize = AInfo.SuppressBulletResize) and
    (DisplayFormatString = AInfo.DisplayFormatString) and
    (RelativeRestartLevel = AInfo.RelativeRestartLevel) and
    (HasTemplateCode = AInfo.HasTemplateCode) and
    (OriginalLeftIndent = AInfo.OriginalLeftIndent) and
    (Legacy = AInfo.Legacy) and
    (LegacyIndent = AInfo.LegacyIndent) and
    (LegacySpace = AInfo.LegacySpace) and
    (TemplateCode = AInfo.TemplateCode);
end;

function TdxListLevelInfo.GetAlignment: TdxListNumberAlignment;
begin
  Result := FAlignment;
end;

function TdxListLevelInfo.GetConvertPreviousLevelNumberingToDecimal: Boolean;
begin
  Result := FConvertPreviousLevelNumberingToDecimal;
end;

function TdxListLevelInfo.GetDisplayFormatString: string;
begin
  Result := FDisplayFormatString;
end;

function TdxListLevelInfo.GetFormat: TdxNumberingFormat;
begin
  Result := FFormat;
end;

function TdxListLevelInfo.GetHasTemplateCode: Boolean;
begin
  Result := TemplateCode <> 0;
end;

function TdxListLevelInfo.GetLegacy: Boolean;
begin
  Result := FLegacy;
end;

function TdxListLevelInfo.GetLegacyIndent: Integer;
begin
  Result := FLegacyIndent;
end;

function TdxListLevelInfo.GetLegacySpace: Integer;
begin
  Result := FLegacySpace;
end;

function TdxListLevelInfo.GetOriginalLeftIndent: Integer;
begin
  Result := FOriginalLeftIndent;
end;

function TdxListLevelInfo.GetRelativeRestartLevel: Integer;
begin
  Result := FRelativeRestartLevel;
end;

function TdxListLevelInfo.GetSeparator: Char;
begin
  Result := FSeparator;
end;

function TdxListLevelInfo.GetStart: Integer;
begin
  Result := FStart;
end;

function TdxListLevelInfo.GetSuppressBulletResize: Boolean;
begin
  Result := FSuppressBulletResize;
end;

function TdxListLevelInfo.GetSuppressRestart: Boolean;
begin
  Result := FSuppressRestart;
end;

function TdxListLevelInfo.GetTemplateCode: Integer;
begin
  Result := FTemplateCode;
end;

{ TdxListLevelInfoCache }

function TdxListLevelInfoCache.CreateDefaultItem(const AUnitConverter: IdxDocumentModelUnitConverter): TdxListLevelInfo;
var
  AListLevelInfo: TdxListLevelInfo;
begin
  AListLevelInfo := TdxListLevelInfo.Create;
  AListLevelInfo.InitializeAsDefault;
  Result := AListLevelInfo;
end;

{ TdxListLevelChangeActionsCalculator }

class function TdxListLevelChangeActionsCalculator.CalculateChangeActions(
  AChange: TdxListLevelChangeType): TdxDocumentModelChangeActions;
const
  ListLevelChangeActionsMap: array[TdxListLevelChangeType] of TdxDocumentModelChangeActions = (
    [],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout],
    [],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout]
  );
begin
  Result := ListLevelChangeActionsMap[AChange];
end;

{ TdxListLevelProperties }

constructor TdxListLevelProperties.Create(ADocumentModel: TdxCustomDocumentModel);

  function GetMainPieceTable(ADocumentModel: TdxCustomDocumentModel): TdxCustomPieceTable;
  begin
    Assert(ADocumentModel <> nil);
    Result := ADocumentModel.MainPart;
  end;

begin
  inherited Create(GetMainPieceTable(ADocumentModel));
end;

function TdxListLevelProperties.Clone: TdxListLevelProperties;
begin
  if Self = nil then
    Exit(nil);
  Result := TdxListLevelProperties.Create(DocumentModel);
  Result.CopyFrom(Self);
end;

function TdxListLevelProperties.GetAlignment: TdxListNumberAlignment;
begin
  Result := Info.Alignment;
end;

function TdxListLevelProperties.GetBatchUpdateChangeActions: TdxDocumentModelChangeActions;
begin
  Result := TdxListLevelChangeActionsCalculator.CalculateChangeActions(TdxListLevelChangeType.BatchUpdate);
end;

function TdxListLevelProperties.GetCache(
  const ADocumentModel: TdxCustomDocumentModel): TdxUniqueItemsCache<TdxListLevelInfo>;
begin
  Result := TdxDocumentCache(ADocumentModel.Cache).ListLevelInfoCache;
end;

function TdxListLevelProperties.GetConvertPreviousLevelNumberingToDecimal: Boolean;
begin
  Result := Info.ConvertPreviousLevelNumberingToDecimal;
end;

function TdxListLevelProperties.GetDisplayFormatString: string;
begin
  Result := Info.DisplayFormatString;
end;

function TdxListLevelProperties.GetFormat: TdxNumberingFormat;
begin
  Result := Info.Format;
end;

function TdxListLevelProperties.GetLegacy: Boolean;
begin
  Result := Info.Legacy;
end;

function TdxListLevelProperties.GetLegacyIndent: Integer;
begin
  Result := Info.LegacyIndent;
end;

function TdxListLevelProperties.GetLegacySpace: Integer;
begin
  Result := Info.LegacySpace;
end;

function TdxListLevelProperties.GetOriginalLeftIndent: Integer;
begin
  Result := Info.OriginalLeftIndent;
end;

function TdxListLevelProperties.GetRelativeRestartLevel: Integer;
begin
  Result := Info.RelativeRestartLevel;
end;

function TdxListLevelProperties.GetSeparator: Char;
begin
  Result := Info.Separator;
end;

function TdxListLevelProperties.GetStart: Integer;
begin
  Result := Info.Start;
end;

function TdxListLevelProperties.GetSuppressBulletResize: Boolean;
begin
  Result := Info.SuppressBulletResize;
end;

function TdxListLevelProperties.GetSuppressRestart: Boolean;
begin
  Result := Info.SuppressRestart;
end;

function TdxListLevelProperties.GetTemplateCode: Integer;
begin
  Result := Info.TemplateCode;
end;

procedure TdxListLevelProperties.InternalSetOriginalLeftIndent(const Value: Integer);
begin
  if OriginalLeftIndent = Value then
    Exit;
  SetPropertyValue<Integer>(SetOriginalLeftIndent, Value);
end;

procedure TdxListLevelProperties.SetTemplateCodeIntf(const Value: Integer);
begin
  if TemplateCode = Value then
    Exit;
  SetPropertyValue<Integer>(SetTemplateCode, Value);
end;

procedure TdxListLevelProperties.SetAlignment(const Value: TdxListNumberAlignment);
begin
  if Alignment = Value then
    Exit;
  SetPropertyValue<TdxListNumberAlignment>(SetAlignmentCore, Value);
end;

function TdxListLevelProperties.SetAlignmentCore(const AListLevel: TdxListLevelInfo;
  const AValue: TdxListNumberAlignment): TdxDocumentModelChangeActions;
begin
  AListLevel.Alignment := AValue;
  Result := TdxListLevelChangeActionsCalculator.CalculateChangeActions(TdxListLevelChangeType.Alignment);
end;

procedure TdxListLevelProperties.SetConvertPreviousLevelNumberingToDecimal(const Value: Boolean);
begin
  if ConvertPreviousLevelNumberingToDecimal = Value then
    Exit;
  SetPropertyValue<Boolean>(SetConvertPreviousLevelNumberingToDecimalCore, Value);
end;

function TdxListLevelProperties.SetConvertPreviousLevelNumberingToDecimalCore(const AListLevel: TdxListLevelInfo;
  const AValue: Boolean): TdxDocumentModelChangeActions;
begin
  AListLevel.ConvertPreviousLevelNumberingToDecimal := AValue;
  Result := TdxListLevelChangeActionsCalculator.CalculateChangeActions(TdxListLevelChangeType.RelativeRestartLevel);
end;

procedure TdxListLevelProperties.SetDisplayFormatString(const Value: string);
begin
  if DisplayFormatString = Value then
    Exit;
  SetPropertyValue<string>(SetDisplayFormatStringCore, Value);
end;

function TdxListLevelProperties.SetDisplayFormatStringCore(const AListLevel: TdxListLevelInfo;
  const AValue: string): TdxDocumentModelChangeActions;
begin
  AListLevel.DisplayFormatString := AValue;
  Result := TdxListLevelChangeActionsCalculator.CalculateChangeActions(TdxListLevelChangeType.DisplayFormatString);
end;

procedure TdxListLevelProperties.SetFormat(const Value: TdxNumberingFormat);
begin
  if Format = Value then
    Exit;
  SetPropertyValue<TdxNumberingFormat>(SetFormatCore, Value);
end;

function TdxListLevelProperties.SetFormatCore(const AListLevel: TdxListLevelInfo;
  const AValue: TdxNumberingFormat): TdxDocumentModelChangeActions;
begin
  AListLevel.Format := AValue;
  Result := TdxListLevelChangeActionsCalculator.CalculateChangeActions(TdxListLevelChangeType.Format);
end;

procedure TdxListLevelProperties.SetLegacy(const Value: Boolean);
begin
  if Legacy = Value then
    Exit;
  SetPropertyValue<Boolean>(SetLegacyCore, Value);
end;

function TdxListLevelProperties.SetLegacyCore(const AListLevel: TdxListLevelInfo;
  const AValue: Boolean): TdxDocumentModelChangeActions;
begin
  AListLevel.Legacy := AValue;
  Result := TdxListLevelChangeActionsCalculator.CalculateChangeActions(TdxListLevelChangeType.Legacy);
end;

procedure TdxListLevelProperties.SetLegacyIndent(const Value: Integer);
begin
  if LegacyIndent = Value then
    Exit;
  SetPropertyValue<Integer>(SetLegacyIndentCore, Value);
end;

function TdxListLevelProperties.SetLegacyIndentCore(const AListLevel: TdxListLevelInfo;
  const AValue: Integer): TdxDocumentModelChangeActions;
begin
  AListLevel.LegacyIndent := AValue;
  Result := TdxListLevelChangeActionsCalculator.CalculateChangeActions(TdxListLevelChangeType.LegacyIndent);
end;

procedure TdxListLevelProperties.SetLegacySpace(const Value: Integer);
begin
  if LegacySpace = Value then
    Exit;
  SetPropertyValue<Integer>(SetLegacySpaceCore, Value);
end;

function TdxListLevelProperties.SetLegacySpaceCore(const AListLevel: TdxListLevelInfo;
  const AValue: Integer): TdxDocumentModelChangeActions;
begin
  AListLevel.LegacySpace := AValue;
  Result := TdxListLevelChangeActionsCalculator.CalculateChangeActions(TdxListLevelChangeType.LegacySpace);
end;

function TdxListLevelProperties.SetOriginalLeftIndent(const AListLevel: TdxListLevelInfo;
  const AValue: Integer): TdxDocumentModelChangeActions;
begin
  AListLevel.OriginalLeftIndent := AValue;
  Result := [];
end;

procedure TdxListLevelProperties.SetRelativeRestartLevel(const Value: Integer);
begin
  if Value < 0 then
    TdxRichEditExceptions.ThrowArgumentException('RestartLevel', Value);
  if RelativeRestartLevel = Value then
    Exit;
  SetPropertyValue<Integer>(SetRestartLevelCore, Value);
end;

function TdxListLevelProperties.SetRestartLevelCore(const AListLevel: TdxListLevelInfo;
  const AValue: Integer): TdxDocumentModelChangeActions;
begin
  AListLevel.RelativeRestartLevel := AValue;
  Result := TdxListLevelChangeActionsCalculator.CalculateChangeActions(TdxListLevelChangeType.RelativeRestartLevel);
end;

procedure TdxListLevelProperties.SetSeparator(const Value: Char);
begin
  if Separator = Value then
    Exit;
  SetPropertyValue<Char>(SetSeparatorCore, Value);
end;

function TdxListLevelProperties.SetSeparatorCore(const AListLevel: TdxListLevelInfo;
  const AValue: Char): TdxDocumentModelChangeActions;
begin
  AListLevel.Separator := AValue;
  Result := TdxListLevelChangeActionsCalculator.CalculateChangeActions(TdxListLevelChangeType.Separator);
end;

procedure TdxListLevelProperties.SetStart(const Value: Integer);
begin
  if Value < 0 then
    TdxRichEditExceptions.ThrowArgumentException('Start', Value);
  if Start = Value then
    Exit;
  SetPropertyValue<Integer>(SetStartCore, Value);
end;

function TdxListLevelProperties.SetStartCore(const AListLevel: TdxListLevelInfo;
  const AValue: Integer): TdxDocumentModelChangeActions;
begin
  AListLevel.Start := AValue;
  Result := TdxListLevelChangeActionsCalculator.CalculateChangeActions(TdxListLevelChangeType.Start);
end;

procedure TdxListLevelProperties.SetSuppressBulletResize(const Value: Boolean);
begin
  if SuppressBulletResize = Value then
    Exit;
  SetPropertyValue<Boolean>(SetSuppressBulletResizeCore, value);
end;

function TdxListLevelProperties.SetSuppressBulletResizeCore(const AListLevel: TdxListLevelInfo;
  const AValue: Boolean): TdxDocumentModelChangeActions;
begin
  AListLevel.SuppressBulletResize := AValue;
  Result := TdxListLevelChangeActionsCalculator.CalculateChangeActions(TdxListLevelChangeType.SuppressBulletResize);
end;

procedure TdxListLevelProperties.SetSuppressRestart(const Value: Boolean);
begin
  if SuppressRestart = Value then
    Exit;
  SetPropertyValue<Boolean>(SetSuppressRestartCore, Value);
end;

function TdxListLevelProperties.SetSuppressRestartCore(const AListLevel: TdxListLevelInfo;
  const AValue: Boolean): TdxDocumentModelChangeActions;
begin
  AListLevel.SuppressRestart := AValue;
  Result := TdxListLevelChangeActionsCalculator.CalculateChangeActions(TdxListLevelChangeType.SuppressRestart);
end;

function TdxListLevelProperties.SetTemplateCode(const AListLevel: TdxListLevelInfo;
  const AValue: Integer): TdxDocumentModelChangeActions;
begin
  AListLevel.TemplateCode := AValue;
  Result := TdxListLevelChangeActionsCalculator.CalculateChangeActions(TdxListLevelChangeType.TemplateCode);
end;

{ TdxOrdinalBasedNumberConverter }

class function TdxOrdinalBasedNumberConverter.CreateConverter(AFormat: TdxNumberingFormat;
  ALanguageId: Integer): TdxOrdinalBasedNumberConverter;
begin
  case AFormat of
    TdxNumberingFormat.UpperRoman:
      Result := TdxUpperRomanNumberConverterClassic.Create;
    TdxNumberingFormat.LowerRoman:
      Result := TdxLowerRomanNumberConverterClassic.Create;
    TdxNumberingFormat.UpperLetter:
      Result := TdxUpperLatinLetterNumberConverter.Create;
    TdxNumberingFormat.LowerLetter:
      Result := TdxLowerLatinLetterNumberConverter.Create;
    TdxNumberingFormat.NumberInDash:
      Result := TdxNumberInDashNumberConverter.Create;
    TdxNumberingFormat.DecimalZero:
      Result := TdxDecimalZeroNumberConverter.Create;
    TdxNumberingFormat.Bullet:
      Result := TdxBulletNumberConverter.Create;
    TdxNumberingFormat.Ordinal:
      Result := GetOrdinalNumberConverterByLanguage(ALanguageId);
    TdxNumberingFormat.RussianUpper:
      Result := TdxRussianUpperNumberConverter.Create;
    TdxNumberingFormat.RussianLower:
      Result := TdxRussianLowerNumberConverter.Create;
    TdxNumberingFormat.DecimalEnclosedParentheses:
      Result := TdxDecimalEnclosedParenthesesNumberConverter.Create;
    TdxNumberingFormat.CardinalText:
      Result := GetDescriptiveCardinalNumberConverterByLanguage(ALanguageId);
    TdxNumberingFormat.OrdinalText:
      Result := GetDescriptiveOrdinalNumberConverterByLanguage(ALanguageId);
    TdxNumberingFormat.Decimal:
      Result := TdxDecimalNumberConverter.Create;
  else
    Result := TdxDecimalNumberConverter.Create;
  end;
end;

class function TdxOrdinalBasedNumberConverter.GetDescriptiveCardinalNumberConverterByLanguage(
  ALanguageId: Integer): TdxOrdinalBasedNumberConverter;
begin
  Result := TdxDecimalNumberConverter.Create;
end;

function TdxOrdinalBasedNumberConverter.ConvertNumber(AValue: Int64): string;
begin
  if (AValue < MinValue) or (AValue > MaxValue) then
    Result := ''
  else
    Result := ConvertNumberCore(AValue);
end;

class function TdxOrdinalBasedNumberConverter.GetOrdinalNumberConverterByLanguage(
  ALanguageId: Integer): TdxOrdinalBasedNumberConverter;
begin
  Result := TdxDecimalNumberConverter.Create;
end;

class function TdxOrdinalBasedNumberConverter.GetSupportNumberingFormat: TdxOrdinalList<TdxNumberingFormat>;
begin
  Result := TdxOrdinalList<TdxNumberingFormat>.Create;
  Result.Add(TdxNumberingFormat.Decimal);
  Result.Add(TdxNumberingFormat.UpperRoman);
  Result.Add(TdxNumberingFormat.LowerRoman);
  Result.Add(TdxNumberingFormat.UpperLetter);
  Result.Add(TdxNumberingFormat.LowerLetter);
  Result.Add(TdxNumberingFormat.Ordinal);
  Result.Add(TdxNumberingFormat.CardinalText);
  Result.Add(TdxNumberingFormat.OrdinalText);
  Result.Add(TdxNumberingFormat.DecimalZero);
  Result.Add(TdxNumberingFormat.RussianUpper);
  Result.Add(TdxNumberingFormat.RussianLower);
  Result.Add(TdxNumberingFormat.DecimalEnclosedParentheses);
  Result.Add(TdxNumberingFormat.NumberInDash);
  Result.Add(TdxNumberingFormat.Chicago);
end;

class function TdxOrdinalBasedNumberConverter.GetDescriptiveOrdinalNumberConverterByLanguage(
  ALanguageId: Integer): TdxOrdinalBasedNumberConverter;
begin
  Result := TdxDecimalNumberConverter.Create;
end;

function TdxOrdinalBasedNumberConverter.GetMaxValue: Int64;
begin
  Result := MaxInt64;
end;

function TdxOrdinalBasedNumberConverter.GetMinValue: Int64;
begin
  Result := MinInt64;
end;

{ TdxDecimalNumberConverter }

function TdxDecimalNumberConverter.ConvertNumberCore(AValue: Int64): string;
begin
  Result := IntToStr(AValue);
end;

function TdxDecimalNumberConverter.GetType: TdxNumberingFormat;
begin
  Result := TdxNumberingFormat.Decimal;
end;

{ TdxAlphabetBasedNumberConverter }

function TdxAlphabetBasedNumberConverter.ConvertNumberCore(AValue: Int64): string;
var
  ACount: Integer;
  ASymbol: Char;
begin
  if AValue = 0 then
    Exit('');
  Dec(AValue);
  ACount := AValue div AlphabetSize + 1;
  ASymbol := Alphabet[AValue mod AlphabetSize];
  Result := DupeString(ASymbol, ACount);
end;

function TdxAlphabetBasedNumberConverter.GetMaxValue: Int64;
begin
  Result := 780;
end;

function TdxAlphabetBasedNumberConverter.GetMinValue: Int64;
begin
  Result := 1;
end;

{ TdxUpperLatinLetterNumberConverter }

class constructor TdxUpperLatinLetterNumberConverter.Initialize;
begin
  FUpperLetter := TArray<Char>.Create('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
    'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z');
end;

function TdxUpperLatinLetterNumberConverter.GetAlphabetSize: Integer;
begin
  Result := Length(FUpperLetter);
end;

function TdxUpperLatinLetterNumberConverter.GetAlphabet: TArray<Char>;
begin
  Result := FUpperLetter;
end;

function TdxUpperLatinLetterNumberConverter.GetType: TdxNumberingFormat;
begin
  Result := TdxNumberingFormat.UpperLetter;
end;

{ TdxLowerLatinLetterNumberConverter }

class constructor TdxLowerLatinLetterNumberConverter.Initialize;
begin
  FLowerLetter := TArray<Char>.Create('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
    'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z');
end;

function TdxLowerLatinLetterNumberConverter.GetAlphabetSize: Integer;
begin
  Result := Length(FLowerLetter);
end;

function TdxLowerLatinLetterNumberConverter.GetAlphabet: TArray<Char>;
begin
  Result := FLowerLetter;
end;

function TdxLowerLatinLetterNumberConverter.GetType: TdxNumberingFormat;
begin
  Result := TdxNumberingFormat.LowerLetter;
end;

{ TdxRomanNumberConverter }

function TdxRomanNumberConverter.ConvertNumberCore(AValue: Int64): string;
var
  I: Integer;
begin
  Assert(Length(Roman) = Length(Arabic));
  Result := '';
  for I := Length(Roman) - 1 downto 0 do
    while AValue >= Arabic[I] do
    begin
      Dec(AValue, Arabic[I]);
      Result := Result + Roman[I];
    end;
end;

function TdxRomanNumberConverter.GetMinValue: Int64;
begin
  Result := 1;
end;

{ TdxUpperRomanNumberConverterClassic }

class constructor TdxUpperRomanNumberConverterClassic.Initialize;
begin
  FRoman := TArray<string>.Create('I', 'IV', 'V', 'IX', 'X', 'XL', 'L', 'XC', 'C', 'CD', 'D', 'CM', 'M');
  FArabic := TArray<Integer>.Create(1, 4, 5, 9, 10, 40, 50, 90, 100, 400, 500, 900, 1000);
end;

function TdxUpperRomanNumberConverterClassic.GetArabic: TArray<Integer>;
begin
  Result := FArabic;
end;

function TdxUpperRomanNumberConverterClassic.GetRoman: TArray<string>;
begin
  Result := FRoman;
end;

function TdxUpperRomanNumberConverterClassic.GetType: TdxNumberingFormat;
begin
  Result := TdxNumberingFormat.UpperRoman;
end;

{ TdxLowerRomanNumberConverter }

class constructor TdxLowerRomanNumberConverterClassic.Initialize;
begin
  FRoman := TArray<string>.Create('i', 'iv', 'v', 'ix', 'x', 'xl', 'l', 'xc', 'c', 'cd', 'd', 'cm', 'm');
  FArabic := TArray<Integer>.Create(1, 4, 5, 9, 10, 40, 50, 90, 100, 400, 500, 900, 1000);
end;

function TdxLowerRomanNumberConverterClassic.GetArabic: TArray<Integer>;
begin
  Result := FArabic;
end;

function TdxLowerRomanNumberConverterClassic.GetRoman: TArray<string>;
begin
  Result := FRoman;
end;

function TdxLowerRomanNumberConverterClassic.GetType: TdxNumberingFormat;
begin
  Result := TdxNumberingFormat.LowerRoman;
end;

{ TdxNumberInDashNumberConverter }

function TdxNumberInDashNumberConverter.ConvertNumberCore(AValue: Int64): string;
begin
  Result := Format('- %d -', [AValue]);
end;

function TdxNumberInDashNumberConverter.GetType: TdxNumberingFormat;
begin
  Result := TdxNumberingFormat.NumberInDash;
end;

{ TdxDecimalZeroNumberConverter }

function TdxDecimalZeroNumberConverter.ConvertNumberCore(AValue: Int64): string;
begin
  if AValue < 10 then
    Result := Format('0%d', [AValue])
  else
    Result := IntToStr(AValue);
end;

function TdxDecimalZeroNumberConverter.GetType: TdxNumberingFormat;
begin
  Result := TdxNumberingFormat.DecimalZero;
end;

{ TdxBulletNumberConverter }

function TdxBulletNumberConverter.ConvertNumberCore(AValue: Int64): string;
begin
  Result := TdxCharacters.Bullet;
end;

function TdxBulletNumberConverter.GetType: TdxNumberingFormat;
begin
  Result := TdxNumberingFormat.Bullet;
end;

{ TdxRussianUpperNumberConverter }

class constructor TdxRussianUpperNumberConverter.Initialize;
begin
  FRussianUpper := TArray<Char>.Create(
    #1040, #1041, #1042, #1043, #1044, #1045, #1046, #1047, #1048, #1050, #1051, #1052, #1053, #1054,
    #1055, #1056, #1057, #1058, #1059, #1060, #1061, #1062, #1063, #1064, #1065, #1067, #1069, #1070, #1071);
end;

function TdxRussianUpperNumberConverter.GetAlphabet: TArray<Char>;
begin
  Result := FRussianUpper;
end;

function TdxRussianUpperNumberConverter.GetAlphabetSize: Integer;
begin
  Result := Length(FRussianUpper);
end;

function TdxRussianUpperNumberConverter.GetType: TdxNumberingFormat;
begin
  Result := TdxNumberingFormat.RussianUpper;
end;

{ TdxRussianLowerNumberConverter }

function TdxRussianLowerNumberConverter.GetAlphabet: TArray<Char>;
begin
  Result := FRussianLower;
end;

function TdxRussianLowerNumberConverter.GetAlphabetSize: Integer;
begin
  Result := Length(FRussianLower);
end;

function TdxRussianLowerNumberConverter.GetType: TdxNumberingFormat;
begin
  Result := TdxNumberingFormat.RussianLower;
end;

class constructor TdxRussianLowerNumberConverter.Initialize;
begin
  FRussianLower := TArray<Char>.Create(
    #1072, #1073, #1074, #1075, #1076, #1077, #1078, #1079, #1080, #1082, #1083, #1084, #1085, #1086,
    #1087, #1088, #1089, #1090, #1091, #1092, #1093, #1094, #1095, #1096, #1097, #1099, #1101, #1102, #1103);
end;

{ TdxDecimalEnclosedParenthesesNumberConverter }

function TdxDecimalEnclosedParenthesesNumberConverter.ConvertNumberCore(AValue: Int64): string;
begin
  Result := Format('(%d)', [AValue]);
end;

function TdxDecimalEnclosedParenthesesNumberConverter.GetType: TdxNumberingFormat;
begin
  Result := TdxNumberingFormat.DecimalEnclosedParentheses;
end;

end.
