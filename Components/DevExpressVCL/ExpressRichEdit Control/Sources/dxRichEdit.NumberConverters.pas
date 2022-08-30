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

unit dxRichEdit.NumberConverters;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Generics.Defaults, Generics.Collections,
  Classes, Contnrs,
  dxCoreClasses,
  dxGenerics,
  dxRichEdit.DocumentModel.NumberingFormatting;

type
  TdxDescriptiveNumberConverterBase = class;

  { IdxNumericsProvider }

  IdxNumericsProvider = interface
    function GetBillion: TArray<string>;
    function GetHundreds: TArray<string>;
    function GetMillion: TArray<string>;
    function GetQuadrillion: TArray<string>;
    function GetQuintillion: TArray<string>;
    function GetSeparator: TArray<string>;
    function GetSingles: TArray<string>;
    function GetSinglesNumeral: TArray<string>;
    function GetTeens: TArray<string>;
    function GetTenths: TArray<string>;
    function GetThousands: TArray<string>;
    function GetTrillion: TArray<string>;

    property Billion: TArray<string> read GetBillion;
    property Hundreds: TArray<string> read GetHundreds;
    property Million: TArray<string> read GetMillion;
    property Quadrillion: TArray<string> read GetQuadrillion;
    property Quintillion: TArray<string> read GetQuintillion;
    property Separator: TArray<string> read GetSeparator;
    property Singles: TArray<string> read GetSingles;
    property SinglesNumeral: TArray<string> read GetSinglesNumeral;
    property Teens: TArray<string> read GetTeens;
    property Tenths: TArray<string> read GetTenths;
    property Thousands: TArray<string> read GetThousands;
    property Trillion: TArray<string> read GetTrillion;
  end;

  TdxDigitType = (
    Zero,
    SingleNumeral,
    Single,
    Teen,
    Tenth,
    Hundred,
    Thousand,
    Million,
    Billion,
    Trillion,
    Quadrillion,
    Quintillion,
    Separator);

  { TdxDigitInfo }

  TdxDigitInfo = class abstract
  strict private
    FProvider: IdxNumericsProvider;
    FNumber: Int64;
  protected
    function GetType: TdxDigitType; virtual; abstract;
    function GetNumerics: TArray<string>; virtual; abstract;
  public
    constructor Create(const AProvider: IdxNumericsProvider; ANumber: Int64); virtual;
    function ConvertToString: string;

    property &Type: TdxDigitType read GetType;
    property Provider: IdxNumericsProvider read FProvider write FProvider;
    property Number: Int64 read FNumber;
  end;

  TdxDigitInfoCollection = TdxObjectList<TdxDigitInfo>;

  { TdxSingleNumeralDigitInfo }

  TdxSingleNumeralDigitInfo = class(TdxDigitInfo)
  protected
    function GetType: TdxDigitType; override;
    function GetNumerics: TArray<string>; override;
  public
    constructor Create(const AProvider: IdxNumericsProvider; ANumber: Int64); override;
  end;

  { TdxZeroDigitInfo }

  TdxZeroDigitInfo = class(TdxDigitInfo)
  protected
    function GetType: TdxDigitType; override;
    function GetNumerics: TArray<string>; override;
  public
    constructor Create(const AProvider: IdxNumericsProvider); reintroduce;
  end;

  { TdxSingleDigitInfo }

  TdxSingleDigitInfo = class(TdxDigitInfo)
  protected
    function GetType: TdxDigitType; override;
    function GetNumerics: TArray<string>; override;
  public
    constructor Create(const AProvider: IdxNumericsProvider; ANumber: Int64); override;
  end;

  { TdxTeensDigitInfo }

  TdxTeensDigitInfo = class(TdxDigitInfo)
  protected
    function GetType: TdxDigitType; override;
    function GetNumerics: TArray<string>; override;
  end;

  { TdxTenthsDigitInfo }

  TdxTenthsDigitInfo = class(TdxDigitInfo)
  protected
    function GetType: TdxDigitType; override;
    function GetNumerics: TArray<string>; override;
  public
    constructor Create(const AProvider: IdxNumericsProvider; ANumber: Int64); override;
  end;

  { TdxHundredsDigitInfo }

  TdxHundredsDigitInfo = class(TdxDigitInfo)
  protected
    function GetType: TdxDigitType; override;
    function GetNumerics: TArray<string>; override;
  public
    constructor Create(const AProvider: IdxNumericsProvider; ANumber: Int64); override;
  end;

  { TdxThousandsDigitInfo }

  TdxThousandsDigitInfo = class(TdxDigitInfo)
  protected
    function GetType: TdxDigitType; override;
    function GetNumerics: TArray<string>; override;
  end;

  { TdxMillionDigitInfo }

  TdxMillionDigitInfo = class(TdxDigitInfo)
  protected
    function GetType: TdxDigitType; override;
    function GetNumerics: TArray<string>; override;
  end;

  { TdxBillionDigitInfo }

  TdxBillionDigitInfo = class(TdxDigitInfo)
  protected
    function GetType: TdxDigitType; override;
    function GetNumerics: TArray<string>; override;
  end;

  { TdxTrillionDigitInfo }

  TdxTrillionDigitInfo = class(TdxDigitInfo)
  protected
    function GetType: TdxDigitType; override;
    function GetNumerics: TArray<string>; override;
  end;

  { TdxQuadrillionDigitInfo }

  TdxQuadrillionDigitInfo = class(TdxDigitInfo)
  protected
    function GetType: TdxDigitType; override;
    function GetNumerics: TArray<string>; override;
  end;

  { TdxQuintillionDigitInfo }

  TdxQuintillionDigitInfo = class(TdxDigitInfo)
  protected
    function GetType: TdxDigitType; override;
    function GetNumerics: TArray<string>; override;
  end;

  { TdxSeparatorDigitInfo }

  TdxSeparatorDigitInfo = class(TdxDigitInfo)
  protected
    function GetType: TdxDigitType; override;
    function GetNumerics: TArray<string>; override;
  end;

  { TdxNumberToSingleCharConverter }

  TdxNumberToSingleCharConverter = class abstract
  strict private
    FAlphanumericChars: TArray<Char>;
  protected
    function CreateAlphanumericCharsTable: TArray<Char>; virtual; abstract;
  public
    constructor Create;
    function TryConvert(ANumber: Integer; out AResult: string): Boolean; virtual;
  end;

  { TdxNumberToDollarTextConverter }

  TdxNumberToDollarTextConverter = class abstract
  protected
    function ConvertCore(AIntegerPart: Integer; AFractionPart: Integer): string; virtual;
    function GetCardinalText(AIntegerPart: Integer): string;
    function GetContactString: string; virtual; abstract;
    function GetNumberConverter: TdxDescriptiveNumberConverterBase; virtual; abstract;
  public
    function Convert(AValue: Double): string; virtual;
  end;

  { TdxDescriptiveNumberConverterBase }

  TdxDescriptiveNumberConverterBase = class abstract(TdxOrdinalBasedNumberConverter)
  strict private
    FFlagThousand: Boolean;
    FFlagMillion: Boolean;
    FFlagBillion: Boolean;
    FFlagTrillion: Boolean;
    FFlagQuadrillion: Boolean;
    FFlagQuintillion: Boolean;
    function GetIsValueGreaterThousand: Boolean;
    function GetIsValueGreaterHundred: Boolean;
  protected
    procedure GenerateQuintillionSeparator(ADigits: TdxDigitInfoCollection; const AProvider: IdxNumericsProvider; AValue: Int64); virtual;
    function GenerateQuintillionProvider: IdxNumericsProvider; virtual;
    procedure GenerateQuintillionDigits(ADigits: TdxDigitInfoCollection; AValue: Int64); virtual;
    procedure GenerateQuadrillionSeparator(ADigits: TdxDigitInfoCollection; const AProvider: IdxNumericsProvider; AValue: Int64); virtual;
    function GenerateQuadrillionProvider: IdxNumericsProvider; virtual;
    procedure GenerateQuadrillionDigits(ADigits: TdxDigitInfoCollection; AValue: Int64); virtual;
    procedure GenerateTrillionSeparator(ADigits: TdxDigitInfoCollection; const AProvider: IdxNumericsProvider; AValue: Int64); virtual;
    function GenerateTrillionProvider: IdxNumericsProvider; virtual;
    procedure GenerateTrillionDigits(ADigits: TdxDigitInfoCollection; AValue: Int64); virtual;
    procedure GenerateBillionSeparator(ADigits: TdxDigitInfoCollection; const AProvider: IdxNumericsProvider; AValue: Int64); virtual;
    function GenerateBillionProvider: IdxNumericsProvider; virtual;
    procedure GenerateBillionDigits(ADigits: TdxDigitInfoCollection; AValue: Int64); virtual;
    procedure GenerateMillionSeparator(ADigits: TdxDigitInfoCollection; const AProvider: IdxNumericsProvider; AValue: Int64); virtual;
    function GenerateMillionProvider: IdxNumericsProvider; virtual;
    procedure GenerateMillionDigits(ADigits: TdxDigitInfoCollection; AValue: Int64); virtual;
    procedure GenerateThousandSeparator(ADigits: TdxDigitInfoCollection; const AProvider: IdxNumericsProvider; AValue: Int64); virtual;
    function GenerateThousandProvider: IdxNumericsProvider; virtual;
    procedure GenerateThousandDigits(ADigits: TdxDigitInfoCollection; AValue: Int64); virtual;
    procedure GenerateHundredSeparator(ADigits: TdxDigitInfoCollection; const AProvider: IdxNumericsProvider); virtual;
    function GenerateHundredProvider: IdxNumericsProvider; virtual;
    procedure GenerateHundredDigits(ADigits: TdxDigitInfoCollection; AValue: Int64); virtual;
    procedure GenerateTenthsSeparator(ADigits: TdxDigitInfoCollection; const AProvider: IdxNumericsProvider); virtual;
    function GenerateTenthsProvider: IdxNumericsProvider; virtual;
    procedure GenerateTenthsDigits(ADigits: TdxDigitInfoCollection; AValue: Int64); virtual;
    procedure GenerateTeensSeparator(ADigits: TdxDigitInfoCollection; const AProvider: IdxNumericsProvider; AValue: Int64); virtual;
    function GenerateTeensProvider: IdxNumericsProvider; virtual;
    procedure GenerateTeensDigits(ADigits: TdxDigitInfoCollection; AValue: Int64); virtual;
    procedure GenerateSinglesSeparator(ADigits: TdxDigitInfoCollection; const AProvider: IdxNumericsProvider; AValue: Int64); virtual;
    function GenerateSinglesProvider: IdxNumericsProvider; virtual;
    procedure GenerateSinglesDigits(ADigits: TdxDigitInfoCollection; AValue: Int64); virtual;
    procedure GenerateDigitsInfo(ADigits: TdxDigitInfoCollection; AValue: Int64); virtual;
    procedure GenerateDigits(ADigits: TdxDigitInfoCollection; AValue: Int64); virtual;
    procedure AddZero(ADigits: TdxDigitInfoCollection); virtual;
    function ConvertDigitsToString(ADigits: TdxDigitInfoCollection): string; virtual;
    function ConvertNumberCore(AValue: Int64): string; override;
    function IsDigitInfoGreaterHundred(AInfo: TdxDigitInfo): Boolean;
    function IsDigitInfoGreaterThousand(AInfo: TdxDigitInfo): Boolean;
    function GetMinValue: Int64; override;

    property FlagThousand: Boolean read FFlagThousand write FFlagThousand;
    property FlagMillion: Boolean read FFlagMillion write FFlagMillion;
    property FlagBillion: Boolean read FFlagBillion write FFlagBillion;
    property FlagTrillion: Boolean read FFlagTrillion write FFlagTrillion;
    property FlagQuadrillion: Boolean read FFlagQuadrillion write FFlagQuadrillion;
    property FlagQuintillion: Boolean read FFlagQuintillion write FFlagQuintillion;
    property IsValueGreaterThousand: Boolean read GetIsValueGreaterThousand;
    property IsValueGreaterHundred: Boolean read GetIsValueGreaterHundred;
  end;

implementation

uses
  SysUtils, Character, Math,
  dxRichEdit.NumberConverters.EnglishUS,
  dxRichEdit.Utils.Exceptions;

{ TdxDigitInfo }

constructor TdxDigitInfo.Create(const AProvider: IdxNumericsProvider; ANumber: Int64);
begin
  inherited Create;
  FProvider := AProvider;
  FNumber := ANumber;
end;

function TdxDigitInfo.ConvertToString: string;
var
  ANumerics: TArray<string>;
begin
  ANumerics := GetNumerics;
  Result := ANumerics[FNumber];
end;

{ TdxSingleNumeralDigitInfo }

constructor TdxSingleNumeralDigitInfo.Create(const AProvider: IdxNumericsProvider; ANumber: Int64);
begin
  inherited Create(AProvider, ANumber - 1);
end;

function TdxSingleNumeralDigitInfo.GetType: TdxDigitType;
begin
  Result := TdxDigitType.SingleNumeral;
end;

function TdxSingleNumeralDigitInfo.GetNumerics: TArray<string>;
begin
  Result := Provider.SinglesNumeral;
end;

{ TdxZeroDigitInfo }

constructor TdxZeroDigitInfo.Create(const AProvider: IdxNumericsProvider);
begin
  inherited Create(AProvider, 9);
end;

function TdxZeroDigitInfo.GetType: TdxDigitType;
begin
  Result := TdxDigitType.Zero;
end;

function TdxZeroDigitInfo.GetNumerics: TArray<string>;
begin
  Result := Provider.Singles;
end;

{ TdxSingleDigitInfo }

constructor TdxSingleDigitInfo.Create(const AProvider: IdxNumericsProvider; ANumber: Int64);
begin
  inherited Create(AProvider, ANumber - 1);
end;

function TdxSingleDigitInfo.GetType: TdxDigitType;
begin
  Result := TdxDigitType.Single;
end;

function TdxSingleDigitInfo.GetNumerics: TArray<string>;
begin
  Result := Provider.Singles;
end;

{ TdxTeensDigitInfo }

function TdxTeensDigitInfo.GetType: TdxDigitType;
begin
  Result := TdxDigitType.Teen;
end;

function TdxTeensDigitInfo.GetNumerics: TArray<string>;
begin
  Result := Provider.Teens;
end;

{ TdxTenthsDigitInfo }

constructor TdxTenthsDigitInfo.Create(const AProvider: IdxNumericsProvider; ANumber: Int64);
begin
  inherited Create(AProvider, ANumber - 2);
end;

function TdxTenthsDigitInfo.GetType: TdxDigitType;
begin
  Result := TdxDigitType.Tenth;
end;

function TdxTenthsDigitInfo.GetNumerics: TArray<string>;
begin
  Result := Provider.Tenths;
end;

{ TdxHundredsDigitInfo }

constructor TdxHundredsDigitInfo.Create(const AProvider: IdxNumericsProvider; ANumber: Int64);
begin
  inherited Create(AProvider, ANumber - 1);
end;

function TdxHundredsDigitInfo.GetType: TdxDigitType;
begin
  Result := TdxDigitType.Hundred;
end;

function TdxHundredsDigitInfo.GetNumerics: TArray<string>;
begin
  Result := Provider.Hundreds;
end;

{ TdxThousandsDigitInfo }

function TdxThousandsDigitInfo.GetType: TdxDigitType;
begin
  Result := TdxDigitType.Thousand;
end;

function TdxThousandsDigitInfo.GetNumerics: TArray<string>;
begin
  Result := Provider.Thousands;
end;

{ TdxMillionDigitInfo }

function TdxMillionDigitInfo.GetType: TdxDigitType;
begin
  Result := TdxDigitType.Million;
end;

function TdxMillionDigitInfo.GetNumerics: TArray<string>;
begin
  Result := Provider.Million;
end;

{ TdxBillionDigitInfo }

function TdxBillionDigitInfo.GetType: TdxDigitType;
begin
  Result := TdxDigitType.Billion;
end;

function TdxBillionDigitInfo.GetNumerics: TArray<string>;
begin
  Result := Provider.Billion;
end;

{ TdxTrillionDigitInfo }

function TdxTrillionDigitInfo.GetType: TdxDigitType;
begin
  Result := TdxDigitType.Trillion;
end;

function TdxTrillionDigitInfo.GetNumerics: TArray<string>;
begin
  Result := Provider.Trillion;
end;

{ TdxQuadrillionDigitInfo }

function TdxQuadrillionDigitInfo.GetType: TdxDigitType;
begin
  Result := TdxDigitType.Quadrillion;
end;

function TdxQuadrillionDigitInfo.GetNumerics: TArray<string>;
begin
  Result := Provider.Quadrillion;
end;

{ TdxQuintillionDigitInfo }

function TdxQuintillionDigitInfo.GetType: TdxDigitType;
begin
  Result := TdxDigitType.Quintillion;
end;

function TdxQuintillionDigitInfo.GetNumerics: TArray<string>;
begin
  Result := Provider.Quintillion;
end;

{ TdxSeparatorDigitInfo }

function TdxSeparatorDigitInfo.GetType: TdxDigitType;
begin
  Result := TdxDigitType.Separator;
end;

function TdxSeparatorDigitInfo.GetNumerics: TArray<string>;
begin
  Result := Provider.Separator;
end;

{ TdxDescriptiveNumberConverterBase }

function TdxDescriptiveNumberConverterBase.GetIsValueGreaterThousand: Boolean;
begin
  Result := (FlagMillion) or (FlagBillion) or (FlagTrillion) or (FlagQuadrillion) or (FlagQuintillion);
end;

function TdxDescriptiveNumberConverterBase.GetIsValueGreaterHundred: Boolean;
begin
  Result := (FlagThousand) or (IsValueGreaterThousand);
end;

procedure TdxDescriptiveNumberConverterBase.GenerateQuintillionSeparator(ADigits: TdxDigitInfoCollection; const AProvider: IdxNumericsProvider; AValue: Int64);
begin
  if ADigits.Count <> 0 then
    ADigits.Add(TdxSeparatorDigitInfo.Create(AProvider, 0));
end;

function TdxDescriptiveNumberConverterBase.GenerateQuintillionProvider: IdxNumericsProvider;
begin
  Result := TdxCardinalEnglishNumericsProvider.Create;
end;

procedure TdxDescriptiveNumberConverterBase.GenerateQuintillionDigits(ADigits: TdxDigitInfoCollection; AValue: Int64);
begin
  FlagQuintillion := True;
  GenerateDigitsInfo(ADigits, AValue);
  GenerateQuintillionSeparator(ADigits, GenerateQuintillionProvider, AValue);
  ADigits.Add(TdxQuintillionDigitInfo.Create(GenerateQuintillionProvider, 0));
  FlagQuintillion := False;
end;

procedure TdxDescriptiveNumberConverterBase.GenerateQuadrillionSeparator(ADigits: TdxDigitInfoCollection; const AProvider: IdxNumericsProvider; AValue: Int64);
begin
  if ADigits.Count <> 0 then
    ADigits.Add(TdxSeparatorDigitInfo.Create(AProvider, 0));
end;

function TdxDescriptiveNumberConverterBase.GenerateQuadrillionProvider: IdxNumericsProvider;
begin
  Result := TdxCardinalEnglishNumericsProvider.Create;
end;

procedure TdxDescriptiveNumberConverterBase.GenerateQuadrillionDigits(ADigits: TdxDigitInfoCollection; AValue: Int64);
begin
  FlagQuadrillion := True;
  GenerateDigitsInfo(ADigits, AValue);
  GenerateQuadrillionSeparator(ADigits, GenerateQuadrillionProvider, AValue);
  ADigits.Add(TdxQuadrillionDigitInfo.Create(GenerateQuadrillionProvider, 0));
  FlagQuadrillion := False;
end;

procedure TdxDescriptiveNumberConverterBase.GenerateTrillionSeparator(ADigits: TdxDigitInfoCollection; const AProvider: IdxNumericsProvider; AValue: Int64);
begin
  if ADigits.Count <> 0 then
    ADigits.Add(TdxSeparatorDigitInfo.Create(AProvider, 0));
end;

function TdxDescriptiveNumberConverterBase.GenerateTrillionProvider: IdxNumericsProvider;
begin
  Result := TdxCardinalEnglishNumericsProvider.Create;
end;

procedure TdxDescriptiveNumberConverterBase.GenerateTrillionDigits(ADigits: TdxDigitInfoCollection; AValue: Int64);
begin
  FlagTrillion := True;
  GenerateDigitsInfo(ADigits, AValue);
  GenerateTrillionSeparator(ADigits, GenerateTrillionProvider, AValue);
  ADigits.Add(TdxTrillionDigitInfo.Create(GenerateTrillionProvider, 0));
  FlagTrillion := False;
end;

procedure TdxDescriptiveNumberConverterBase.GenerateBillionSeparator(ADigits: TdxDigitInfoCollection; const AProvider: IdxNumericsProvider; AValue: Int64);
begin
  if ADigits.Count <> 0 then
    ADigits.Add(TdxSeparatorDigitInfo.Create(AProvider, 0));
end;

function TdxDescriptiveNumberConverterBase.GenerateBillionProvider: IdxNumericsProvider;
begin
  Result := TdxCardinalEnglishNumericsProvider.Create;
end;

procedure TdxDescriptiveNumberConverterBase.GenerateBillionDigits(ADigits: TdxDigitInfoCollection; AValue: Int64);
begin
  FlagBillion := True;
  GenerateDigitsInfo(ADigits, AValue);
  GenerateBillionSeparator(ADigits, GenerateBillionProvider, AValue);
  ADigits.Add(TdxBillionDigitInfo.Create(GenerateBillionProvider, 0));
  FlagBillion := False;
end;

procedure TdxDescriptiveNumberConverterBase.GenerateMillionSeparator(ADigits: TdxDigitInfoCollection; const AProvider: IdxNumericsProvider; AValue: Int64);
begin
  if ADigits.Count <> 0 then
    ADigits.Add(TdxSeparatorDigitInfo.Create(AProvider, 0));
end;

function TdxDescriptiveNumberConverterBase.GenerateMillionProvider: IdxNumericsProvider;
begin
  Result := TdxCardinalEnglishNumericsProvider.Create;
end;

procedure TdxDescriptiveNumberConverterBase.GenerateMillionDigits(ADigits: TdxDigitInfoCollection; AValue: Int64);
begin
  FlagMillion := True;
  GenerateDigitsInfo(ADigits, AValue);
  GenerateMillionSeparator(ADigits, GenerateMillionProvider, AValue);
  ADigits.Add(TdxMillionDigitInfo.Create(GenerateMillionProvider, 0));
  FlagMillion := False;
end;

procedure TdxDescriptiveNumberConverterBase.GenerateThousandSeparator(ADigits: TdxDigitInfoCollection; const AProvider: IdxNumericsProvider; AValue: Int64);
begin
  if ADigits.Count <> 0 then
    ADigits.Add(TdxSeparatorDigitInfo.Create(AProvider, 0));
end;

function TdxDescriptiveNumberConverterBase.GenerateThousandProvider: IdxNumericsProvider;
begin
  Result := TdxCardinalEnglishNumericsProvider.Create;
end;

procedure TdxDescriptiveNumberConverterBase.GenerateThousandDigits(ADigits: TdxDigitInfoCollection; AValue: Int64);
begin
  FlagThousand := True;
  GenerateDigitsInfo(ADigits, AValue);
  GenerateThousandSeparator(ADigits, GenerateThousandProvider, AValue);
  ADigits.Add(TdxThousandsDigitInfo.Create(GenerateThousandProvider, 0));
  FlagThousand := False;
end;

procedure TdxDescriptiveNumberConverterBase.GenerateHundredSeparator(ADigits: TdxDigitInfoCollection; const AProvider: IdxNumericsProvider);
begin
  if ADigits.Count <> 0 then
    ADigits.Add(TdxSeparatorDigitInfo.Create(AProvider, 0));
end;

function TdxDescriptiveNumberConverterBase.GenerateHundredProvider: IdxNumericsProvider;
begin
  Result := TdxCardinalEnglishNumericsProvider.Create;
end;

procedure TdxDescriptiveNumberConverterBase.GenerateHundredDigits(ADigits: TdxDigitInfoCollection; AValue: Int64);
begin
  GenerateHundredSeparator(ADigits, GenerateHundredProvider);
  ADigits.Add(TdxHundredsDigitInfo.Create(GenerateHundredProvider, AValue));
end;

procedure TdxDescriptiveNumberConverterBase.GenerateTenthsSeparator(ADigits: TdxDigitInfoCollection; const AProvider: IdxNumericsProvider);
begin
  if ADigits.Count <> 0 then
    ADigits.Add(TdxSeparatorDigitInfo.Create(AProvider, 0));
end;

function TdxDescriptiveNumberConverterBase.GenerateTenthsProvider: IdxNumericsProvider;
begin
  Result := TdxCardinalEnglishNumericsProvider.Create;
end;

procedure TdxDescriptiveNumberConverterBase.GenerateTenthsDigits(ADigits: TdxDigitInfoCollection; AValue: Int64);
begin
  GenerateTenthsSeparator(ADigits, GenerateTenthsProvider);
  ADigits.Add(TdxTenthsDigitInfo.Create(GenerateTenthsProvider, AValue div 10));
  GenerateSinglesDigits(ADigits, AValue mod 10);
end;

procedure TdxDescriptiveNumberConverterBase.GenerateTeensSeparator(ADigits: TdxDigitInfoCollection; const AProvider: IdxNumericsProvider; AValue: Int64);
begin
  if ADigits.Count <> 0 then
    ADigits.Add(TdxSeparatorDigitInfo.Create(AProvider, 0));
end;

function TdxDescriptiveNumberConverterBase.GenerateTeensProvider: IdxNumericsProvider;
begin
  Result := TdxCardinalEnglishNumericsProvider.Create;
end;

procedure TdxDescriptiveNumberConverterBase.GenerateTeensDigits(ADigits: TdxDigitInfoCollection; AValue: Int64);
begin
  GenerateTeensSeparator(ADigits, GenerateTeensProvider, AValue);
  ADigits.Add(TdxTeensDigitInfo.Create(GenerateTeensProvider, AValue));
end;

procedure TdxDescriptiveNumberConverterBase.GenerateSinglesSeparator(ADigits: TdxDigitInfoCollection; const AProvider: IdxNumericsProvider; AValue: Int64);
begin
  if ADigits.Count <> 0 then
  begin
    if ADigits.Last.&Type = TdxDigitType.Tenth then
      ADigits.Add(TdxSeparatorDigitInfo.Create(AProvider, 1))
    else
      ADigits.Add(TdxSeparatorDigitInfo.Create(AProvider, 0));
  end;
end;

function TdxDescriptiveNumberConverterBase.GenerateSinglesProvider: IdxNumericsProvider;
begin
  Result := TdxCardinalEnglishNumericsProvider.Create;
end;

procedure TdxDescriptiveNumberConverterBase.GenerateSinglesDigits(ADigits: TdxDigitInfoCollection; AValue: Int64);
begin
  if AValue = 0 then
    Exit;
  GenerateSinglesSeparator(ADigits, GenerateSinglesProvider, AValue);
  if FlagThousand then
    ADigits.Add(TdxSingleNumeralDigitInfo.Create(GenerateSinglesProvider, AValue))
  else
    ADigits.Add(TdxSingleDigitInfo.Create(GenerateSinglesProvider, AValue));
  FlagThousand := False;
end;

procedure TdxDescriptiveNumberConverterBase.GenerateDigitsInfo(ADigits: TdxDigitInfoCollection; AValue: Int64);
begin
  if AValue >= 1000000000000000000 then
    GenerateQuintillionDigits(ADigits, AValue div 1000000000000000000);
  AValue := AValue mod 1000000000000000000;

  if AValue >= 1000000000000000 then
    GenerateQuadrillionDigits(ADigits, AValue div 1000000000000000);
  AValue := AValue mod 1000000000000000;

  if AValue >= 1000000000000 then
    GenerateTrillionDigits(ADigits, AValue div 1000000000000);
  AValue := AValue mod 1000000000000;

  if AValue >= 1000000000 then
    GenerateBillionDigits(ADigits, AValue div 1000000000);
  AValue := AValue mod 1000000000;

  if AValue >= 1000000 then
    GenerateMillionDigits(ADigits, AValue div 1000000);
  AValue := AValue mod 1000000;

  if AValue >= 1000 then
    GenerateThousandDigits(ADigits, AValue div 1000);
  AValue := AValue mod 1000;

  if AValue >= 100 then
    GenerateHundredDigits(ADigits, AValue div 100);
  AValue := AValue mod 100;
  if AValue = 0 then
    Exit;

  if AValue >= 20 then
    GenerateTenthsDigits(ADigits, AValue)
  else
  begin
    if AValue >= 10 then
      GenerateTeensDigits(ADigits, AValue mod 10)
    else
      GenerateSinglesDigits(ADigits, AValue);
  end;
end;

procedure TdxDescriptiveNumberConverterBase.GenerateDigits(ADigits: TdxDigitInfoCollection; AValue: Int64);
begin
  GenerateDigitsInfo(ADigits, AValue);
  if ADigits.Count = 0 then
    AddZero(ADigits);
end;

procedure TdxDescriptiveNumberConverterBase.AddZero(ADigits: TdxDigitInfoCollection);
begin
  ADigits.Add(TdxZeroDigitInfo.Create(GenerateSinglesProvider));
end;

function TdxDescriptiveNumberConverterBase.ConvertNumberCore(AValue: Int64): string;
var
  ADigits: TdxDigitInfoCollection;
begin
  ADigits := TdxDigitInfoCollection.Create;
  try
    GenerateDigits(ADigits, AValue);
    Result := ConvertDigitsToString(ADigits);
  finally
    ADigits.Free;
  end;
end;

function TdxDescriptiveNumberConverterBase.ConvertDigitsToString(ADigits: TdxDigitInfoCollection): string;
var
  ABuilder: TStringBuilder;
  ACount, I: Integer;
begin
  ABuilder := TStringBuilder.Create;
  try
    ACount := ADigits.Count;
    for I := 0 to ACount - 1 do
      ABuilder.Append(ADigits[I].ConvertToString);
    if ABuilder.Length > 0 then
    {$IFDEF DELPHIXE4}
      ABuilder[0] := ABuilder[0].ToUpper;
    {$ELSE}
      ABuilder[0] := TCharacter.ToUpper(ABuilder[0]);
    {$ENDIF}
    Result := ABuilder.ToString;
  finally
    ABuilder.Free;
  end;
end;

function TdxDescriptiveNumberConverterBase.IsDigitInfoGreaterHundred(AInfo: TdxDigitInfo): Boolean;
begin
  Result := (AInfo.&Type = TdxDigitType.Thousand) or (IsDigitInfoGreaterThousand(AInfo));
end;

function TdxDescriptiveNumberConverterBase.IsDigitInfoGreaterThousand(AInfo: TdxDigitInfo): Boolean;
var
  AType: TdxDigitType;
begin
  AType := AInfo.&Type;
  Result := (AType = TdxDigitType.Million) or (AType = TdxDigitType.Billion) or
    (AType = TdxDigitType.Trillion) or (AType = TdxDigitType.Quadrillion) or (AType = TdxDigitType.Quintillion);
end;

function TdxDescriptiveNumberConverterBase.GetMinValue: Int64;
begin
  Result := 0;
end;

{ TdxNumberToSingleCharConverter }

constructor TdxNumberToSingleCharConverter.Create;
begin
  inherited Create;
  FAlphanumericChars := CreateAlphanumericCharsTable;
end;

function TdxNumberToSingleCharConverter.TryConvert(ANumber: Integer;
  out AResult: string): Boolean;
begin
  AResult := '';
  if (ANumber <= 0) or (Length(FAlphanumericChars) < ANumber) then
    Exit(False);
  AResult := FAlphanumericChars[ANumber - 1];
  Result := True;
end;

{ TdxNumberToDollarTextConverter }

function TdxNumberToDollarTextConverter.Convert(AValue: Double): string;
var
  AIntegerPart, AFractionPart: Integer;
  ARemainder: Double;
begin
  AIntegerPart := Trunc(AValue);
  ARemainder := RoundTo((AValue - AIntegerPart), -2);
  AFractionPart := Round(ARemainder * 100);
  Result := ConvertCore(AIntegerPart, AFractionPart);
end;

function TdxNumberToDollarTextConverter.ConvertCore(AIntegerPart: Integer; AFractionPart: Integer): string;
var
  ACardinalText, AContactString: string;
begin
  ACardinalText := GetCardinalText(AIntegerPart);
  AContactString := GetContactString;
  if (0 < AFractionPart) and (AFractionPart < 100) then
    Result := Format('%s %s %d/100', [ACardinalText, AContactString, AFractionPart])
  else
    Result := Format('%s %s 00/100', [ACardinalText, AContactString]);
end;

function TdxNumberToDollarTextConverter.GetCardinalText(AIntegerPart: Integer): string;
var
  AConverter: TdxDescriptiveNumberConverterBase;
begin
  AConverter := GetNumberConverter;
  try
    Result := AConverter.ConvertNumber(AIntegerPart);
  finally
    AConverter.Free;
  end;
end;

end.

