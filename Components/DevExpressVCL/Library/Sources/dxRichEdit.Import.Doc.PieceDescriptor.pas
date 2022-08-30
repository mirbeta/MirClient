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
unit dxRichEdit.Import.Doc.PieceDescriptor;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, Classes, Generics.Defaults, Generics.Collections;

type

  { TdxPieceDescriptor }

  TdxPieceDescriptor = class
  public const
    PieceDescriptorSize = 8;
  protected
    FDescriptorStart: SmallInt;
    FFc: Integer;
    FDescriptorEnd: SmallInt;
    procedure Read(const APieceDescriptor: TBytes);

    property DescriptorStart: SmallInt read FDescriptorStart;
    property DescriptorEnd: SmallInt read FDescriptorEnd;
  public
    class function FromByteArray(const APieceDescriptor: TBytes): TdxPieceDescriptor; static;
    class function FromFileOffset(AFc: Integer): TdxPieceDescriptor; static;
    function ToByteArray: TBytes;
    function GetEncoding: TEncoding;
    function GetOffset: Integer;

    property FC: Integer read FFc;
  end;

implementation

uses
  dxEncoding;

{ TdxPieceDescriptor }

class function TdxPieceDescriptor.FromByteArray(const APieceDescriptor: TBytes): TdxPieceDescriptor;
begin
  Result := TdxPieceDescriptor.Create;
  Result.Read(APieceDescriptor);
end;

class function TdxPieceDescriptor.FromFileOffset(AFc: Integer): TdxPieceDescriptor;
begin
  Result := TdxPieceDescriptor.Create;
  Result.FFc := AFc;
end;

procedure TdxPieceDescriptor.Read(const APieceDescriptor: TBytes);
begin
  FDescriptorStart := PSmallInt(@APieceDescriptor[0])^;
  FFc := PInteger(@APieceDescriptor[2])^;
  FDescriptorEnd := PSmallInt(@APieceDescriptor[6])^;
end;

function TdxPieceDescriptor.ToByteArray: TBytes;
begin
  SetLength(Result, 8);
  Move(FDescriptorStart, Result[0], 2);
  Move(FFC, Result[2], 4);
  Move(FDescriptorEnd, Result[6], 2);
end;

function TdxPieceDescriptor.GetEncoding: TEncoding;
begin
  if (FC and $40000000) <> 0 then
    Result := TdxEncoding.GetEncoding(1252)
  else
    Result := TdxEncoding.Unicode;
end;

function TdxPieceDescriptor.GetOffset: Integer;
begin
  if (FC and $40000000) <> 0 then
    Result := (FC and not $40000000) div 2
  else
    Result := FC;
end;

end.
