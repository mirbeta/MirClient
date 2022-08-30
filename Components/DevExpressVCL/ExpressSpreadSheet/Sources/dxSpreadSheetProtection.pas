{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressSpreadSheet                                       }
{                                                                    }
{           Copyright (c) 2001-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSSPREADSHEET CONTROL AND ALL    }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY. }
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

unit dxSpreadSheetProtection;

{$I cxVer.Inc}

interface

uses
  Windows, Classes, dxCore, cxClasses, dxCoreClasses, SysUtils, dxHash, dxProtectionUtils;

type

  { IdxSpreadSheetProtectionInfo }

  IdxSpreadSheetProtectionInfo = interface
  ['{F6851380-98C4-49A6-A437-B24535F1189C}']
    function CheckPassword(const APassword: string): Boolean;
  end;

  { TdxSpreadSheetCustomProtectionInfo }

  TdxSpreadSheetCustomProtectionInfoClass = class of TdxSpreadSheetCustomProtectionInfo;
  TdxSpreadSheetCustomProtectionInfo = class abstract(TInterfacedObject,
    IdxSpreadSheetProtectionInfo)
  strict private
    FPassword: string;
  protected
    function CheckPasswordCore(const APassword: string): Boolean; virtual; abstract;
    procedure Initialize(const APassword: string); virtual; abstract;
  public
    constructor Create(const APassword: string); overload;
    // IdxSpreadSheetProtectionInfo
    function CheckPassword(const APassword: string): Boolean;
    //
    property Password: string read FPassword;
  end;

  { TdxSpreadSheetStandardProtectionInfo }

  TdxSpreadSheetStandardProtectionInfo = class(TdxSpreadSheetCustomProtectionInfo)
  strict private
    FKeyWord: Word;

    function CalculateKeyWord(const APassword: string): Word;
    function GetKeyWordAsString: string;
    procedure SetKeyWordAsString(const Value: string);
  protected
    function CheckPasswordCore(const APassword: string): Boolean; override;
    procedure Initialize(const APassword: string); override;
  public
    property KeyWord: Word read FKeyWord write FKeyWord;
    property KeyWordAsString: string read GetKeyWordAsString write SetKeyWordAsString;
  end;

  { TdxSpreadSheetStrongProtectionInfo }

  TdxSpreadSheetStrongProtectionInfo = class(TdxSpreadSheetCustomProtectionInfo)
  strict private
    FHashAlgorithm: TdxHashAlgorithmType;
    FHashValue: TBytes;
    FSaltValue: TBytes;
    FSpinCount: Integer;

    function CalculateHash(const APassword: string): TBytes;
    function GetHashValueAsString: string;
    function GetSaltValueAsString: string;
    procedure SetHashValueAsString(const Value: string);
    procedure SetSaltValueAsString(const Value: string);
  protected
    function CheckPasswordCore(const APassword: string): Boolean; override;
    procedure Initialize(const APassword: string); override;
  public
    property HashAlgorithm: TdxHashAlgorithmType read FHashAlgorithm write FHashAlgorithm;
    property HashValue: TBytes read FHashValue write FHashValue;
    property HashValueAsString: string read GetHashValueAsString write SetHashValueAsString;
    property SaltValue: TBytes read FSaltValue write FSaltValue;
    property SaltValueAsString: string read GetSaltValueAsString write SetSaltValueAsString;
    property SpinCount: Integer read FSpinCount write FSpinCount;
  end;

  { TdxSpreadSheetCustomProtectionOptions }

  TdxSpreadSheetCustomProtectionOptions = class(TPersistent)
  strict private
    FProtected: Boolean;
    FProtectionInfo: IdxSpreadSheetProtectionInfo;

    FOnChange: TNotifyEvent;

    procedure SetProtected(const Value: Boolean);
  protected
    procedure Changed; virtual;
  public
    constructor Create; overload; virtual;
    constructor Create(AChangeEvent: TNotifyEvent); overload;
    procedure Assign(Source: TPersistent); override;
    //
    property ProtectionInfo: IdxSpreadSheetProtectionInfo read FProtectionInfo write FProtectionInfo;
  published
    property Protected: Boolean read FProtected write SetProtected default False;
  end;

  { TdxSpreadSheetSheetProtectionOptions }

  TdxSpreadSheetSheetProtectionOptions = class(TdxSpreadSheetCustomProtectionOptions)
  strict private
    FPermissions: array[0..11] of Boolean;

    function GetActualPermissionState(const Index: Integer): Boolean;
    function GetPermissionState(const Index: Integer): Boolean;
    procedure SetAllowSelectLockedCells(const Index: Integer; const Value: Boolean);
    procedure SetAllowSelectUnlockedCells(const Index: Integer; const Value: Boolean);
    procedure SetPermissionState(const Index: Integer; const Value: Boolean);
  public
    constructor Create; override;
    procedure Assign(Source: TPersistent); override;

    property ActualAllowDeleteColumns: Boolean index 0 read GetActualPermissionState;
    property ActualAllowDeleteRows: Boolean index 1 read GetActualPermissionState;
    property ActualAllowEditContainers: Boolean index 2 read GetActualPermissionState;
    property ActualAllowEditHyperlinks: Boolean index 3 read GetActualPermissionState;
    property ActualAllowFormatCells: Boolean index 4 read GetActualPermissionState;
    property ActualAllowInsertColumns: Boolean index 5 read GetActualPermissionState;
    property ActualAllowInsertRows: Boolean index 6 read GetActualPermissionState;
    property ActualAllowResizeColumns: Boolean index 7 read GetActualPermissionState;
    property ActualAllowResizeRows: Boolean index 8 read GetActualPermissionState;
    property ActualAllowSelectLockedCells: Boolean index 9 read GetActualPermissionState;
    property ActualAllowSelectUnlockedCells: Boolean index 10 read GetActualPermissionState;
    property ActualAllowSort: Boolean index 11 read GetActualPermissionState;
  published
    property AllowDeleteColumns: Boolean index 0 read GetPermissionState write SetPermissionState default False;
    property AllowDeleteRows: Boolean index 1 read GetPermissionState write SetPermissionState default False;
    property AllowEditContainers: Boolean index 2 read GetPermissionState write SetPermissionState default True;
    property AllowEditHyperlinks: Boolean index 3 read GetPermissionState write SetPermissionState default False;
    property AllowFormatCells: Boolean index 4 read GetPermissionState write SetPermissionState default False;
    property AllowInsertColumns: Boolean index 5 read GetPermissionState write SetPermissionState default False;
    property AllowInsertRows: Boolean index 6 read GetPermissionState write SetPermissionState default False;
    property AllowResizeColumns: Boolean index 7 read GetPermissionState write SetPermissionState default False;
    property AllowResizeRows: Boolean index 8 read GetPermissionState write SetPermissionState default False;
    property AllowSelectLockedCells: Boolean index 9 read GetPermissionState write SetAllowSelectLockedCells default True;
    property AllowSelectUnlockedCells: Boolean index 10 read GetPermissionState write SetAllowSelectUnlockedCells default True;
    property AllowSort: Boolean index 11 read GetPermissionState write SetPermissionState default False;
  end;

  { TdxSpreadSheetWorkbookProtectionOptions }

  TdxSpreadSheetWorkbookProtectionOptions = class(TdxSpreadSheetCustomProtectionOptions)
  strict private
    FAllowChangeStructure: Boolean;

    function GetActualAllowChangeStructure: Boolean;
    procedure SetAllowChangeStructure(const Value: Boolean);
  public
    constructor Create; override;
    procedure Assign(Source: TPersistent); override;

    property ActualAllowChangeStructure: Boolean read GetActualAllowChangeStructure;
  published
    property AllowChangeStructure: Boolean read FAllowChangeStructure write SetAllowChangeStructure default True;
  end;

var
  dxSpreadSheetDefaultProtectionProvider: TdxSpreadSheetCustomProtectionInfoClass = TdxSpreadSheetStandardProtectionInfo;

implementation

uses
  dxBase64, dxCrypto, dxCryptoAPI, StrUtils;

{ TdxSpreadSheetCustomProtectionInfo }

constructor TdxSpreadSheetCustomProtectionInfo.Create(const APassword: string);
begin
  Create;
  FPassword := APassword;
  Initialize(APassword);
end;

function TdxSpreadSheetCustomProtectionInfo.CheckPassword(const APassword: string): Boolean;
begin
  Result := CheckPasswordCore(APassword);
  if Result then
    FPassword := APassword;
end;

{ TdxSpreadSheetStandardProtectionInfo }

function TdxSpreadSheetStandardProtectionInfo.CheckPasswordCore(const APassword: string): Boolean;
begin
  Result := KeyWord = CalculateKeyWord(APassword);
end;

procedure TdxSpreadSheetStandardProtectionInfo.Initialize(const APassword: string);
begin
  FKeyWord := CalculateKeyWord(APassword);
end;

function TdxSpreadSheetStandardProtectionInfo.CalculateKeyWord(const APassword: string): Word;
begin
  Result := TdxPasswordHashCodeCalculator.CalculateKeyLowWord(
    TdxPasswordHashCodeCalculator.CalculatePasswordBytes(APassword));
end;

function TdxSpreadSheetStandardProtectionInfo.GetKeyWordAsString: string;
begin
  Result := IntToHex(KeyWord, 4);
end;

procedure TdxSpreadSheetStandardProtectionInfo.SetKeyWordAsString(const Value: string);
begin
  HexToBin(PChar(DupeString('0', 4 - Length(Value)) + Value), @FKeyWord, SizeOf(FKeyWord));
  FKeyWord := MakeWord(HiByte(FKeyWord), LoByte(FKeyWord));
end;

{ TdxSpreadSheetStrongProtectionInfo }

function TdxSpreadSheetStrongProtectionInfo.CheckPasswordCore(const APassword: string): Boolean;
begin
  Result := TdxByteArray.Compare(HashValue, CalculateHash(APassword));
end;

procedure TdxSpreadSheetStrongProtectionInfo.Initialize(const APassword: string);
begin
  FSpinCount := 10000;
  FHashAlgorithm := TdxHashAlgorithmType.SHA256;
  FSaltValue := dxGenerateSalt(64);
  FHashValue := CalculateHash(APassword);
end;

function TdxSpreadSheetStrongProtectionInfo.CalculateHash(const APassword: string): TBytes;
begin
  Result := TdxPasswordHashCodeCalculator.CalculatePasswordHash(APassword, SaltValue, SpinCount, HashAlgorithm);
end;

function TdxSpreadSheetStrongProtectionInfo.GetHashValueAsString: string;
begin
  Result := TdxBase64.ToBase64String(HashValue);
end;

function TdxSpreadSheetStrongProtectionInfo.GetSaltValueAsString: string;
begin
  Result := TdxBase64.ToBase64String(SaltValue);
end;

procedure TdxSpreadSheetStrongProtectionInfo.SetHashValueAsString(const Value: string);
begin
  HashValue := TdxBase64.FromBase64String(Value);
end;

procedure TdxSpreadSheetStrongProtectionInfo.SetSaltValueAsString(const Value: string);
begin
  SaltValue := TdxBase64.FromBase64String(Value);
end;

{ TdxSpreadSheetCustomProtectionOptions }

constructor TdxSpreadSheetCustomProtectionOptions.Create;
begin
  inherited Create;
end;

constructor TdxSpreadSheetCustomProtectionOptions.Create(AChangeEvent: TNotifyEvent);
begin
  Create;
  FOnChange := AChangeEvent;
end;

procedure TdxSpreadSheetCustomProtectionOptions.SetProtected(const Value: Boolean);
begin
  if FProtected <> Value then
  begin
    FProtected := Value;
    Changed;
  end;
end;

procedure TdxSpreadSheetCustomProtectionOptions.Assign(Source: TPersistent);
begin
  if Source is TdxSpreadSheetCustomProtectionOptions then
  begin
    ProtectionInfo := TdxSpreadSheetCustomProtectionOptions(Source).ProtectionInfo;
    Protected := TdxSpreadSheetCustomProtectionOptions(Source).Protected;
  end
  else
    inherited Assign(Source);
end;

procedure TdxSpreadSheetCustomProtectionOptions.Changed;
begin
  dxCallNotify(FOnChange, Self);
end;

{ TdxSpreadSheetSheetProtectionOptions }

constructor TdxSpreadSheetSheetProtectionOptions.Create;
begin
  inherited Create;
  AllowEditContainers := True;
  AllowSelectLockedCells := True;
  AllowSelectUnlockedCells := True;
end;

procedure TdxSpreadSheetSheetProtectionOptions.Assign(Source: TPersistent);
var
  I: Integer;
begin
  inherited Assign(Source);
  if Source is TdxSpreadSheetSheetProtectionOptions then
  begin
    for I := 0 to Length(FPermissions) - 1 do
      FPermissions[I] := TdxSpreadSheetSheetProtectionOptions(Source).FPermissions[I];
    Changed;
  end;
end;

function TdxSpreadSheetSheetProtectionOptions.GetActualPermissionState(const Index: Integer): Boolean;
begin
  Result := not Protected or GetPermissionState(Index);
end;

function TdxSpreadSheetSheetProtectionOptions.GetPermissionState(const Index: Integer): Boolean;
begin
  Result := FPermissions[Index];
end;

procedure TdxSpreadSheetSheetProtectionOptions.SetAllowSelectLockedCells(const Index: Integer; const Value: Boolean);
begin
  if Value then
    AllowSelectUnlockedCells := True;
  SetPermissionState(Index, Value);
end;

procedure TdxSpreadSheetSheetProtectionOptions.SetAllowSelectUnlockedCells(const Index: Integer; const Value: Boolean);
begin
  if not Value then
    AllowSelectLockedCells := False;
  SetPermissionState(Index, Value);
end;

procedure TdxSpreadSheetSheetProtectionOptions.SetPermissionState(const Index: Integer; const Value: Boolean);
begin
  if FPermissions[Index] <> Value then
  begin
    FPermissions[Index] := Value;
    Changed;
  end;
end;

{ TdxSpreadSheetWorkbookProtectionOptions }

constructor TdxSpreadSheetWorkbookProtectionOptions.Create;
begin
  inherited Create;
  AllowChangeStructure := True;
end;

procedure TdxSpreadSheetWorkbookProtectionOptions.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TdxSpreadSheetWorkbookProtectionOptions then
    AllowChangeStructure := TdxSpreadSheetWorkbookProtectionOptions(Source).AllowChangeStructure;
end;

function TdxSpreadSheetWorkbookProtectionOptions.GetActualAllowChangeStructure: Boolean;
begin
  Result := not Protected or AllowChangeStructure;
end;

procedure TdxSpreadSheetWorkbookProtectionOptions.SetAllowChangeStructure(const Value: Boolean);
begin
  if FAllowChangeStructure <> Value then
  begin
    FAllowChangeStructure := Value;
    Changed;
  end;
end;

end.
