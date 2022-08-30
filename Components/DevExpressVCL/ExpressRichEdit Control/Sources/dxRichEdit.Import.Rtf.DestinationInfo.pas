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

unit dxRichEdit.Import.Rtf.DestinationInfo;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, Classes, SysUtils, dxCoreClasses,
  dxProtectionUtils,
  dxRichEdit.Utils.Types,
  dxRichEdit.Import.Rtf,
  dxRichEdit.DocumentModel.PieceTable;

type
  { TdxInfoDestination }

  TdxInfoDestination = class(TdxRichEditRtfDestinationBase)
  strict private
    class var FKeywordHT: TdxKeywordTranslatorTable;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateKeywordHT: TdxKeywordTranslatorTable; static;
  private
    class procedure LegacyPasswordHashHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure PasswordHashHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
  protected
    function CreateClone: TdxRichEditRtfDestinationBase; override;
    class function GetKeywordHT: TdxKeywordTranslatorTable; override;
  end;

  { TdxHexContentDestination }

  TdxHexContentDestination = class abstract(TdxRichEditRtfDestinationBase)
  private
    FFirstPosition: Boolean;
    FValue: Integer;
  protected
    procedure ProcessCharCore(AChar: Char); override;
  public
    constructor Create(AImporter: TdxRtfImporter); override;
  end;

  { TdxHexByteArrayDestination }

  TdxHexByteArrayDestination = class abstract(TdxHexContentDestination)
  private
    FValue: TBytes;
  protected
    procedure ProcessBinCharCore(AChar: Char); override;
    property Value: TBytes read FValue;
  public
    destructor Destroy; override;
  end;

  { TdxHexStreamDestination }

  TdxHexStreamDestination = class abstract(TdxHexContentDestination)
  private
    FValue: TdxMemoryStream;
  protected
    procedure ProcessBinCharCore(AChar: Char); override;
    function CreateEmptyClone: TdxHexStreamDestination; virtual; abstract;
    property Value: TdxMemoryStream read FValue;
  public
    constructor Create(AImporter: TdxRtfImporter); override;
    destructor Destroy; override;

    function CreateClone: TdxRichEditRtfDestinationBase; override;
  end;

  { TdxLegacyPasswordHashDestination }

  TdxLegacyPasswordHashDestination = class(TdxHexStreamDestination)
  private
    procedure ReadPasswordHash;
  protected
    function CreateEmptyClone: TdxHexStreamDestination; override;
  public
    procedure AfterPopRtfState; override;
  end;

  { TdxPasswordHashDestination }

  TdxPasswordHashDestination = class(TdxHexStreamDestination)
  private
    procedure ReadPasswordHash;
  protected
    function CreateEmptyClone: TdxHexStreamDestination; override;
  public
    procedure AfterPopRtfState; override;
  end;

implementation

uses
  dxHash,
  dxRichEdit.DocumentModel.ProtectionFormatting;

{ TdxInfoDestination }

class constructor TdxInfoDestination.Initialize;
begin
  FKeywordHT := CreateKeywordHT;
end;

class destructor TdxInfoDestination.Finalize;
begin
  FreeAndNil(FKeywordHT);
end;

function TdxInfoDestination.CreateClone: TdxRichEditRtfDestinationBase;
begin
  Result := TdxInfoDestination.Create(Importer);
end;

class function TdxInfoDestination.GetKeywordHT: TdxKeywordTranslatorTable;
begin
  Result := FKeywordHT;
end;

class function TdxInfoDestination.CreateKeywordHT: TdxKeywordTranslatorTable;
begin
  Result := TdxKeywordTranslatorTable.Create;
  Result.Add('password', LegacyPasswordHashHandler);
  Result.Add('passwordhash', PasswordHashHandler);
end;

class procedure TdxInfoDestination.LegacyPasswordHashHandler(
  AImporter: TdxRtfImporter; AParameterValue: Integer;
  AHasParameter: Boolean);
begin
  AImporter.Destination := TdxLegacyPasswordHashDestination.Create(AImporter);
end;

class procedure TdxInfoDestination.PasswordHashHandler(
  AImporter: TdxRtfImporter; AParameterValue: Integer;
  AHasParameter: Boolean);
begin
  AImporter.Destination := TdxPasswordHashDestination.Create(AImporter);
end;

{ TdxHexContentDestination }

constructor TdxHexContentDestination.Create(AImporter: TdxRtfImporter);
begin
  inherited Create(AImporter);
  FFirstPosition := True;
end;

procedure TdxHexContentDestination.ProcessCharCore(AChar: Char);
var
  AHex: Integer;
begin
  if AChar <> ' ' then
  begin
    AHex := dxHexToInt(AChar);
    if FFirstPosition then
      FValue := AHex shl 4
    else
    begin
      FValue := FValue + AHex;
      ProcessBinChar(Char(FValue));
      FValue := 0
    end;
    FFirstPosition := not FFirstPosition;
  end;
end;

{ TdxHexByteArrayDestination }

destructor TdxHexByteArrayDestination.Destroy;
begin
  SetLength(FValue, 0);
  inherited Destroy;
end;

procedure TdxHexByteArrayDestination.ProcessBinCharCore(AChar: Char);
var
  ACount: Integer;
begin
  ACount := Length(FValue);
  SetLength(FValue, ACount + 1);
  FValue[ACount] := Ord(AChar);
end;

{ TdxHexStreamDestination }

constructor TdxHexStreamDestination.Create(AImporter: TdxRtfImporter);
begin
  inherited Create(AImporter);
  FValue := TdxMemoryStream.Create;
end;

destructor TdxHexStreamDestination.Destroy;
begin
  FreeAndNil(FValue);
  inherited Destroy;
end;

function TdxHexStreamDestination.CreateClone: TdxRichEditRtfDestinationBase;
var
  AClone: TdxHexStreamDestination;
begin
  AClone := CreateEmptyClone;
  AClone.Value.CopyFrom(Value, 0);
  Result := AClone;
end;

procedure TdxHexStreamDestination.ProcessBinCharCore(AChar: Char);
begin
  FValue.WriteByte(Ord(AChar) and $FF);
end;

{ TdxLegacyPasswordHashDestination }

procedure TdxLegacyPasswordHashDestination.AfterPopRtfState;
begin
  Value.Position := 0;
  ReadPasswordHash;
end;

function TdxLegacyPasswordHashDestination.CreateEmptyClone: TdxHexStreamDestination;
begin
  Result := TdxLegacyPasswordHashDestination.Create(Importer);
end;

procedure TdxLegacyPasswordHashDestination.ReadPasswordHash;
var
  ABytes: TArray<Byte>;
  I: Integer;
  AProperties: TdxDocumentProtectionProperties;
begin
  AProperties := Importer.DocumentModel.ProtectionProperties;
  AProperties.BeginInit;
  try
    SetLength(ABytes, 4);
    for I := 3 downto 0 do
      ABytes[I] := Value.ReadByte;
    AProperties.Word2003PasswordHash := ABytes;
  finally
    AProperties.EndInit;
  end;
end;

{ TdxPasswordHashDestination }

procedure TdxPasswordHashDestination.AfterPopRtfState;
begin
  Value.Position := 0;
  ReadPasswordHash;
end;

function TdxPasswordHashDestination.CreateEmptyClone: TdxHexStreamDestination;
begin
  Result := TdxPasswordHashDestination.Create(Importer);
end;

procedure TdxPasswordHashDestination.ReadPasswordHash;
var
  AProperties: TdxDocumentProtectionProperties;
  AValue, APasswordHashLength, APasswordPrefixLength: Integer;
  ABytes: TArray<Byte>;
begin
  AProperties := Importer.DocumentModel.ProtectionProperties;
  AProperties.BeginInit;
  try
    Value.Seek(12, TSeekOrigin.soCurrent);
    AValue := Value.ReadInteger;
    AProperties.HashAlgorithmType := TdxHashAlgorithmType(AValue - $8000);

    AProperties.HashIterationCount := Value.ReadInteger;

    APasswordHashLength := Value.ReadInteger;
    if (APasswordHashLength < 0) or (APasswordHashLength > 128) then
      raise EArgumentOutOfRangeException.Create('PasswordHashLength');

    APasswordPrefixLength := Value.ReadInteger;
    if (APasswordPrefixLength < 0) or (APasswordPrefixLength > 128) then
      raise EArgumentOutOfRangeException.Create('PasswordPrefixLength');
    Value.Seek(12, TSeekOrigin.soCurrent);

    if APasswordHashLength > 0 then
    begin
      SetLength(ABytes, APasswordHashLength);
      Value.ReadBuffer(PByte(ABytes)^, APasswordHashLength);
      AProperties.PasswordHash := ABytes;
    end;
    if APasswordPrefixLength > 0 then
    begin
      SetLength(ABytes, APasswordPrefixLength);
      Value.ReadBuffer(PByte(ABytes)^, APasswordPrefixLength);
      AProperties.PasswordPrefix := ABytes;
    end;
  finally
    AProperties.EndInit;
  end;
end;

end.


