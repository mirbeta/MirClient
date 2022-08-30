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

unit dxRichEdit.Import.PackageUtils;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, Classes, SysUtils, Generics.Defaults, Generics.Collections, dxCoreClasses,
  dxGenerics;

type

  { TdxPackageFile }

  TdxPackageFile = class
  strict private
    FFileName: string;
    FStream: TStream;
    FStreamLength: Integer;
    FMemoryStream: TMemoryStream;
  public
    constructor Create(const AFileName: string; AStream: TStream; AStreamLength: Integer);
    destructor Destroy; override;
    function GetMemoryStream(AOwnerStreamList: TdxFastObjectList): TMemoryStream;

    property FileName: string read FFileName;
    property Stream: TStream read FStream;
    property StreamLength: Integer read FStreamLength write FStreamLength;
  end;

  TdxPackageFileCollection = TdxObjectList<TdxPackageFile>;

  TdxPackageFileStreams = TdxNamedOrdinalDictionary<TStream>;

implementation

{ TdxPackageFile }

constructor TdxPackageFile.Create(const AFileName: string; AStream: TStream; AStreamLength: Integer);
begin
  Assert(AFileName <> '');
  Assert(AStream <> nil);
  FFileName := AFileName;
  FStream := AStream;
  FStreamLength := AStreamLength;
end;

destructor TdxPackageFile.Destroy;
begin
  FStream.Free;
  inherited Destroy;
end;

function TdxPackageFile.GetMemoryStream(AOwnerStreamList: TdxFastObjectList): TMemoryStream;
begin
  if FMemoryStream = nil then
  begin
    FMemoryStream := TMemoryStream.Create;
    FMemoryStream.SetSize(StreamLength);
    FMemoryStream.CopyFrom(Stream, StreamLength);
    FMemoryStream.Position := 0;
    AOwnerStreamList.Add(FMemoryStream);
  end;
  Result := FMemoryStream;
end;

end.
