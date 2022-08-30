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
unit dxRichEdit.Import.Doc.ComplexFileInformation;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, Classes, Generics.Defaults, Generics.Collections, dxCore, dxCoreGraphics,
  dxRichEdit.Utils.Types,
  dxRichEdit.DocumentModel.UnitConverter,
  dxRichEdit.DocumentModel.Borders,
  dxRichEdit.Doc.Utils;

type

  { TdxComplexFileInformation }

  TdxComplexFileInformation = class
  public const
    PieceTableByteCode   = Byte(2);
    ComplexTypeSize      = Integer(1);
    SizeofGrpprlSize     = Integer(2);
    SizeofPieceTableSize = Integer(4);
  strict private
    FPieceTable: TBytes;
  protected
    procedure Read(AReader: TBinaryReader; AOffset: Integer; ASize: Integer);
  public
    class function FromStream(AReader: TBinaryReader; AOffset: Integer; ASize: Integer): TdxComplexFileInformation; static;
    procedure Write(AWriter: TBinaryWriter);
    function ExtractPieceTable(const AComplexFileInformation: TBytes): TBytes;

    property PieceTable: TBytes read FPieceTable write FPieceTable;
  end;

implementation

{ TdxComplexFileInformation }

class function TdxComplexFileInformation.FromStream(AReader: TBinaryReader; AOffset: Integer; ASize: Integer): TdxComplexFileInformation;
begin
  Result := TdxComplexFileInformation.Create;
  if ASize <> 0 then
    Result.Read(AReader, AOffset, ASize);
end;

procedure TdxComplexFileInformation.Read(AReader: TBinaryReader; AOffset: Integer; ASize: Integer);
var
  AComplexFileInformation: TBytes;
begin
  Assert(AReader <> nil, 'reader');
  AReader.BaseStream.Seek(AOffset, TSeekOrigin.soBeginning);
  AComplexFileInformation := AReader.ReadBytes(ASize);
  FPieceTable := ExtractPieceTable(AComplexFileInformation);
end;

procedure TdxComplexFileInformation.Write(AWriter: TBinaryWriter);
begin
  Assert(AWriter <> nil, 'writer');
  AWriter.Write(PieceTableByteCode);
  AWriter.Write(Cardinal(Length(PieceTable)));
  AWriter.Write(PieceTable);
end;

function TdxComplexFileInformation.ExtractPieceTable(const AComplexFileInformation: TBytes): TBytes;
var
  AIsPieceTableRetrieved: Boolean;
  ACurrentPosition, APieceTableSize: Integer;
  ACurrentEntryType: Byte;
  AGrpplSize: SmallInt;
begin
  AIsPieceTableRetrieved := False;
  ACurrentPosition := 0;
  ACurrentEntryType := AComplexFileInformation[0];
  Result := nil;
  while not AIsPieceTableRetrieved and (ACurrentPosition < Length(AComplexFileInformation)) do
  begin
    if ACurrentEntryType = PieceTableByteCode then
    begin
      APieceTableSize := PInteger(@AComplexFileInformation[ACurrentPosition + 1])^;
      SetLength(Result, APieceTableSize);
      Move(AComplexFileInformation[ACurrentPosition + ComplexTypeSize + SizeofPieceTableSize], Result[0], APieceTableSize);
      AIsPieceTableRetrieved := True;
    end
    else
    begin
      AGrpplSize := PSmallInt(@AComplexFileInformation[ACurrentPosition + 1])^;
      ACurrentPosition := ACurrentPosition + ComplexTypeSize + SizeofGrpprlSize + AGrpplSize;
      ACurrentEntryType := AComplexFileInformation[ACurrentPosition];
    end;
  end;
end;

end.
