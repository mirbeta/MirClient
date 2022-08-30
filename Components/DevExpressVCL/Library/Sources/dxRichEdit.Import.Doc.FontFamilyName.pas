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
unit dxRichEdit.Import.Doc.FontFamilyName;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, Classes;

type

  { TdxDocFontFamilyName }

  TdxDocFontFamilyName = class
  public const
    IgnoredDataSize     = Integer(34);
    DefaultFontFamilyId = Byte($04);
    BaseLength          = Integer(39);
    DelimiterSize       = Integer(2);
    DefaultWeight       = SmallInt(400);
    DefaultCharset      = Byte(1);
  strict private
    FAlternateFontName: string;
    FAlternateFontNameStartIndex: Byte;
    FBaseWeight: SmallInt;
    FCharsetIdentifier: Byte;
    FFontFamilyId: Byte;
    FFontName: string;
    FTotalLength: Byte;
    procedure SetFontName(const AValue: string);
  protected
    procedure Read(AReader: TBinaryReader);
  public
    constructor Create;
    class function FromStream(AReader: TBinaryReader): TdxDocFontFamilyName; static;
    procedure Write(AWriter: TBinaryWriter);
    function CalcTotalLength: Byte;

    property AlternateFontName: string read FAlternateFontName;
    property Charset: Byte read FCharsetIdentifier write FCharsetIdentifier;
    property FontName: string read FFontName write SetFontName;
  end;

implementation

uses
  dxStringHelper, dxEncoding;

{ TdxDocFontFamilyName }

constructor TdxDocFontFamilyName.Create;
begin
  FFontFamilyId := DefaultFontFamilyId;
  FBaseWeight := DefaultWeight;
  FCharsetIdentifier := DefaultCharset;
  FAlternateFontNameStartIndex := 0;
end;

class function TdxDocFontFamilyName.FromStream(AReader: TBinaryReader): TdxDocFontFamilyName;
begin
  Result := TdxDocFontFamilyName.Create;
  Result.Read(AReader);
end;

procedure TdxDocFontFamilyName.SetFontName(const AValue: string);
begin
  FFontName := TdxStringHelper.PrepareFontNameForDoc(AValue);
end;

procedure TdxDocFontFamilyName.Read(AReader: TBinaryReader);
var
  ABuffer: TBytes;
  AFontName, AAlternateFontName: string;
begin
  Assert(AReader <> nil, 'reader');
  FTotalLength := AReader.ReadByte;
  FFontFamilyId := AReader.ReadByte;
  FBaseWeight := AReader.ReadSmallInt;
  FCharsetIdentifier := AReader.ReadByte;
  FAlternateFontNameStartIndex := AReader.ReadByte;
  AReader.BaseStream.Seek(IgnoredDataSize, TSeekOrigin.soCurrent);
  if FAlternateFontNameStartIndex = 0 then
  begin
    ABuffer := AReader.ReadBytes(FTotalLength - BaseLength - DelimiterSize);
    AFontName := TdxEncoding.Unicode.GetString(ABuffer, 0, Length(ABuffer));
    AAlternateFontName := '';
  end
  else
  begin
    ABuffer := AReader.ReadBytes((FAlternateFontNameStartIndex - 1) * 2);
    AFontName := TdxEncoding.Unicode.GetString(ABuffer, 0, Length(ABuffer));
    AReader.ReadSmallInt;
    ABuffer := AReader.ReadBytes(FTotalLength - BaseLength - (FAlternateFontNameStartIndex * 2) - DelimiterSize);
    AAlternateFontName := TdxEncoding.Unicode.GetString(ABuffer, 0, Length(ABuffer));
  end;
  FFontName := TdxStringHelper.PrepareFontNameForDoc(AFontName);
  FAlternateFontName := TdxStringHelper.PrepareFontNameForDoc(AAlternateFontName);
  AReader.ReadSmallInt;
end;

procedure TdxDocFontFamilyName.Write(AWriter: TBinaryWriter);
begin
  Assert(AWriter <> nil, 'writer');
  AWriter.Write(CalcTotalLength);
  AWriter.Write(FFontFamilyId);
  AWriter.Write(FBaseWeight);
  AWriter.Write(FCharsetIdentifier);
  AWriter.Write(FAlternateFontNameStartIndex);
  AWriter.BaseStream.Seek(IgnoredDataSize, TSeekOrigin.soCurrent);
  AWriter.Write(TdxEncoding.Unicode.GetBytes(FFontName));
  AWriter.Write(SmallInt(0));
end;

function TdxDocFontFamilyName.CalcTotalLength: Byte;
begin
  Result := Byte(BaseLength + Length(FFontName) * 2 + 2);
end;

end.
